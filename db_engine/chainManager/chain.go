package chainManager

import (
	"ShardDB/proto"
	"ShardDB/util"
	"encoding/json"
	"errors"
	"github.com/boltdb/bolt"
	"sync"
	"sync/atomic"
	"time"
)

const TPCCNewOrderChainId = 0

var NumberOfProcessedHops = int32(0)
var NumberOfHops = int32(0)
var NumberOfDB = int32(0)
var TimeOfProcessedHops = int32(0)
var TimeOfHops = int32(0)
var TimeOfDB = int32(0)

type NextReqFunc func(in *proto.TrxRes, params map[string]string, shards int) (*proto.TrxReq, map[string]string, int)
type SortFunc func(params map[string]string) map[string]string
type GetShardsFunc func(params map[string]string) []int32

type Chain struct {
	db   *bolt.DB
	hops []*Hop

	NextReq   NextReqFunc
	Sort      SortFunc
	GetShards GetShardsFunc

	EndHop int32
}

func (_chain *Chain) Handle(in *proto.TrxReq, history *sync.Map) (
	*proto.TrxRes, error) {
	begin := time.Now()
	// Waiting block
	// Scheduler: register and wait for msg
	hop := _chain.hops[in.Info.Hopid-1]
	res := &proto.TrxRes{RWSets: nil}
	var err error

	switch util.ConcurrentMode {
	case util.IC3Mode:
		if hop.isWaitTx {
			hop.WaitTx(in.Dependency, history)
		}
	case util.DistributedIC3Mode:
		if hop.isWaitTx {
			hop.WaitByHop(in.Info.Chainid, in.Dependency, history)
		}
	}

	// Process block
	operation := func(tx *bolt.Tx) error {
		begin1 := time.Now()

		res, err = hop.process(tx, in)
		res.Dependency = in.Dependency

		var success bool
		if hop.is2PC {
			success = _chain.Lock(hop, res.RWSets, in.Info.Trxid)
		} else {
			// IC3 normal hops, just check if the resource is available
			success = _chain.TryLock(hop, res.RWSets, in.Info.Trxid)
		}

		if success {
			switch util.ConcurrentMode {
			case util.IC3Mode:
				if hop.isRecordDep {
					hop.Access(res.RWSets, in.Info.Trxid, res.Dependency, hop.hopType > 0)
				}
			case util.DistributedIC3Mode:
				if hop.isRecordDep {
					hop.AccessD(res.RWSets, in.Info.Trxid, _chain.GetShards(in.Params), res.Dependency, hop.hopType > 0)
				}
			}
			atomic.AddInt32(&NumberOfProcessedHops, 1)
			atomic.AddInt32(&TimeOfProcessedHops, int32(time.Since(begin1).Milliseconds()))
			return nil
		} else {
			atomic.AddInt32(&NumberOfProcessedHops, 1)
			atomic.AddInt32(&TimeOfProcessedHops, int32(time.Since(begin1).Milliseconds()))
			return errors.New("Lock failed")
		}
	}

	for {
		// Try to lock before processing
		if !_chain.TryLock(hop, res.RWSets, in.Info.Trxid) {
			time.Sleep(5 * time.Millisecond)
			continue
		}

		begin2 := time.Now()
		if hop.isReadOnly {
			err = _chain.db.View(operation)
		} else {
			err = _chain.db.Update(operation)
		}
		atomic.AddInt32(&TimeOfDB, int32(time.Since(begin2).Milliseconds()))
		atomic.AddInt32(&NumberOfDB, 1)

		if err == nil {
			break
		}
		time.Sleep(5 * time.Millisecond)
	}
	atomic.AddInt32(&TimeOfHops, int32(time.Since(begin).Milliseconds()))
	atomic.AddInt32(&NumberOfHops, 1)

	return res, err
}

func (_chain *Chain) TryLock(hop *Hop, rwSet []*proto.RWSet, txId uint64) bool {
	if hop.isReadOnly {
		return TryRLock(rwSet, txId)
	} else {
		return TryLock(rwSet, txId)
	}
}

func (_chain *Chain) Lock(hop *Hop, rwSet []*proto.RWSet, txId uint64) bool {
	if hop.isReadOnly {
		return RLock(rwSet, txId)
	} else {
		return Lock(rwSet, txId)
	}
}

func (_chain *Chain) HopType(hopId int32) int {
	return _chain.hops[hopId-1].hopType
}

func (_chain *Chain) IsWait(hopId int32) bool {
	return _chain.hops[hopId-1].isWaitTx
}

func (_chain *Chain) GetLaterHopsWait(hopId int32) map[int32]map[int32]int32 {
	waitFor := make(map[int32]map[int32]int32)
	for _, h := range _chain.hops {
		if h.id > hopId && (h.hopType == util.NormalHop || h.hopType == util.MergedHopBegin) && h.isWaitTx {
			waitFor[h.id] = h.conflicts
		}
	}
	return waitFor
}

func (_chain *Chain) Aggregate(hopId int32, results [][]string, params map[string]string) map[string]string {
	return _chain.hops[hopId-1].aggregate(results, params)
}

type KeyedMutex struct {
	mutexes sync.Map // Zero value is empty and ready for use
}

func putData(tx *bolt.Bucket, key []byte, value []byte) {
	tx.Put(key, value)
}

var LockSchedulers map[string]*Scheduler
var AccessSchedulers map[string]*Scheduler
var AccessDSchedulers map[string]*Scheduler

func GetSchedulers() {
	LockSchedulers = make(map[string]*Scheduler)
	AccessSchedulers = make(map[string]*Scheduler)
	AccessDSchedulers = make(map[string]*Scheduler)
}

func GetChains(db *bolt.DB) []*Chain {
	GetSchedulers()
	Scheds()
	var chains []*Chain

	if util.IsPreVerification {
		chains = append(chains, ChainsP(db)...)
	} else {
		chains = append(chains, Chains(db)...)
	}

	for _, chain := range chains {
		chainAnalysis(chain)
	}

	return chains
}

func chainAnalysis(chain *Chain) {
	hopCount := len(chain.hops)
	last := 0
	first := true

	for i, hop := range chain.hops {
		if len(hop.conflicts) != 0 {
			if hop.hopType == util.NormalHop || hop.hopType == util.MergedHopBegin || hop.hopType == util.GlobalHop {
				hop.isWaitTx = true
				last = i
			} else {
				hop.isWaitTx = false
			}

			hop.isRecordDep = true

		} else {
			hop.isRecordDep = false
			hop.isWaitTx = false
		}

		// First conflict don't need to wait anything
		if first && hop.isWaitTx {
			first = false
			hop.isWaitTx = false
		}

		hop.is2PC = util.ConcurrentMode == util.LockMode || hop.hopType != util.NormalHop
	}

	// Last conflicting hops don't need to record dependency
	for i := last; i < hopCount; i++ {
		chain.hops[i].isRecordDep = false
	}

	for _, hop := range chain.hops {
		hop.laterWaitHop = chain.GetLaterHopsWait(hop.id)
	}

	chain.EndHop = int32(hopCount)
	if chain.hops[hopCount-1].hopType != util.NormalHop {
		chain.EndHop = util.MaxHop
	}
}

func AddRWSet(rwSet []*proto.RWSet, tableName string, key []byte) []*proto.RWSet {
	seen := make(map[string]bool)

	for _, val := range rwSet {
		dep := val.String()
		if _, ok := seen[dep]; !ok {
			seen[dep] = true
		} else {
			return rwSet
		}
	}

	return append(rwSet, &proto.RWSet{TableName: tableName, Key: string(key)})
}

func defaultSort(params map[string]string) map[string]string {
	return params
}

func ListConcatenate[T any](src [][]string, dest []T) string {
	var tmp []T
	for _, v := range src {
		if len(v) == 0 {
			continue
		}
		json.Unmarshal([]byte(v[0]), &tmp)

		dest = append(dest, tmp...)
	}

	result, _ := json.Marshal(dest)
	return string(result)
}
