package chainManager

import (
	"ShardDB/proto"
	"ShardDB/util"
	"github.com/boltdb/bolt"
	"sync"
	"time"
)

type ProcessFunc func(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error)
type PullFunc func(chainId int32, serverId int32, msg *proto.IdList)

type AggregateFunc func(results [][]string, params map[string]string) map[string]string

var Pull PullFunc

type Hop struct {
	id         int32
	isReadOnly bool
	hopType    int
	conflicts  map[int32]int32
	process    ProcessFunc
	aggregate  AggregateFunc

	isWaitTx     bool
	is2PC        bool
	isRecordDep  bool
	laterWaitHop map[int32]map[int32]int32
}

func (_h *Hop) WaitTx(dependency map[int32]*proto.IdList, txHistory *sync.Map) {
	if !_h.isWaitTx {
		return
	}

	for _, d := range dependency[0].List {
		chainId := util.GetChainId(d.TxId)

		hopId, ok := _h.conflicts[chainId]

		// If no conflict, wait for hop finish
		if !ok {
			hopId = util.MaxHop
		}

		for {
			v, loaded := txHistory.Load(d.TxId)

			if loaded && v.(int32) >= hopId {
				break
			}

			time.Sleep(2 * time.Millisecond)
		}
	}
}

func (_h *Hop) WaitByHop(chainId int32, dependency map[int32]*proto.IdList, txHistory *sync.Map) {
	if !_h.isWaitTx {
		return
	}

	if dependency[_h.id] == nil {
		return
	}

	needWait := map[int32][]*proto.DependencyTuple{}
	for _, d := range dependency[_h.id].List {
		// Try to load the current history
		v, loaded := txHistory.Load(d.TxId)

		// If not done yet, pull from remote
		if !loaded || v.(int32) < d.HopId {
			needWait[d.ServerId] = append(needWait[d.ServerId], d)
		}
	}

	wg := new(sync.WaitGroup)
	wg.Add(len(needWait))
	for serverId, v := range needWait {
		go func(id int32, msg []*proto.DependencyTuple) {
			Pull(chainId, id, &proto.IdList{List: msg})
			wg.Done()
		}(serverId, v)
	}
	wg.Wait()
}

func (_h *Hop) addDependencyIC3(id int32, list []uint64, dep map[int32]*proto.IdList, txId uint64, woundWait bool) {
	if dep[id] == nil {
		dep[id] = &proto.IdList{}
	}

	for _, d := range list {
		if d == txId {
			// Do not include itself
			continue
		}

		if woundWait && util.IsWoundWaitLock && d > txId {
			// If wound Wait, never add younger tx Id
			continue
		}

		// If no conflicts, skip
		chainId := util.GetChainId(d)
		_, ok := _h.conflicts[chainId]
		if !ok {
			continue
		}

		dep[id].List = append(dep[id].List, &proto.DependencyTuple{TxId: d})
	}

	dep[id].List = DeduplicateDpendency(dep[id].List)
}

func (_h *Hop) addDependencyDIC3(id int32, list []*proto.DependencyTuple, dep map[int32]*proto.IdList, txId uint64, woundWait bool) {
	if dep[id] == nil {
		dep[id] = &proto.IdList{}
	}

	for _, d := range list {
		if d.TxId == txId {
			// Do not include itself
			continue
		}

		if woundWait && util.IsWoundWaitLock && d.TxId > txId {
			// If wound Wait, never add younger tx Id
			continue
		}

		dep[id].List = append(dep[id].List, d)
	}

	dep[id].List = DeduplicateDpendency(dep[id].List)
}

func DeduplicateDpendency(slice []*proto.DependencyTuple) []*proto.DependencyTuple {
	seen := make(map[string]bool)
	result := []*proto.DependencyTuple{}

	for _, val := range slice {
		dep := val.String()
		if _, ok := seen[dep]; !ok {
			seen[dep] = true
			result = append(result, val)
		}
	}
	return result
}

func TryLock(accessList []*proto.RWSet, txId uint64) bool {
	for _, v := range accessList {
		if !LockSchedulers[v.TableName].TryLock(v.Key, txId) {
			return false
		}
	}

	return true
}

func TryRLock(accessList []*proto.RWSet, txId uint64) bool {
	for _, v := range accessList {
		if !LockSchedulers[v.TableName].TryRLock(v.Key, txId) {
			return false
		}
	}

	return true
}

func Lock(accessList []*proto.RWSet, txId uint64) bool {
	success := true
	top := 0
	for i, v := range accessList {
		locked, woundedList := LockSchedulers[v.TableName].Lock(v.Key, txId)

		if !locked {
			success = false
			top = i
			break
		}

		if util.ConcurrentMode == util.IC3Mode && util.IsWoundWaitLock {
			for _, w := range woundedList {
				AccessSchedulers[v.TableName].RemoveAccess(v.Key, w)
			}
		} else if util.ConcurrentMode == util.DistributedIC3Mode && util.IsWoundWaitLock {
			for _, w := range woundedList {
				AccessDSchedulers[v.TableName].RemoveAccessD(v.Key, w)
			}
		}
	}

	if !success {
		for i, v := range accessList {
			if i == top {
				break
			}
			LockSchedulers[v.TableName].UnLock(v.Key, txId)
		}
	}

	return success
}

func RLock(accessList []*proto.RWSet, txId uint64) bool {
	success := true
	top := 0
	for i, v := range accessList {
		locked, woundedList := LockSchedulers[v.TableName].RLock(v.Key, txId)

		if !locked {
			success = false
			top = i
			break
		}

		if util.ConcurrentMode == util.IC3Mode {
			for _, w := range woundedList {
				AccessSchedulers[v.TableName].RemoveAccess(v.Key, w)
			}
		}
	}

	if !success {
		for i, v := range accessList {
			if i == top {
				break
			}
			LockSchedulers[v.TableName].UnLock(v.Key, txId)
		}
	}

	return success
}

func (_h *Hop) Access(accessList []*proto.RWSet, txId uint64, dep map[int32]*proto.IdList, woundWait bool) {
	var list []uint64
	for _, v := range accessList {
		list = append(list, AccessSchedulers[v.TableName].AddAccess(v.Key, txId)...)
	}
	_h.addDependencyIC3(0, list, dep, txId, woundWait)
}

func (_h *Hop) AccessD(accessList []*proto.RWSet, txId uint64, shards []int32, dep map[int32]*proto.IdList, woundWait bool) {
	depList := make(map[int32][]*proto.DependencyTuple)

	list := make([]util.AccessTuple, 0)
	// Get all conflicting transactions
	for _, v := range accessList {
		list = append(list, AccessDSchedulers[v.TableName].AddAccessD(v.Key, txId, _h.id, shards)...)
	}

	// Find later conflicts for each dependency
	for _, a := range list {
		chainId := util.GetChainId(a.TxId)

		_, ok := _h.conflicts[chainId]
		if !ok {
			continue
		}

		hasConflict := false
		// For each later hop
		for myHopId, w := range _h.laterWaitHop {
			// If the tx has later conflict
			if hop, ok := w[chainId]; ok {
				if hop <= a.HopId {
					// the hop that I need to wait for is earlier than the current conflict
					// no need to wait
					continue
				}
				serverId := a.Shards[hop]
				depList[myHopId] = append(depList[myHopId], &proto.DependencyTuple{TxId: a.TxId, ServerId: serverId, HopId: hop})
				hasConflict = true
			}
		}

		// If no conflict found, wait for tx finish
		if !hasConflict {
			depList[0] = append(depList[0], &proto.DependencyTuple{TxId: a.TxId, ServerId: a.Shards[0], HopId: util.MaxHop})
		}
	}

	for k, v := range depList {
		_h.addDependencyDIC3(k, v, dep, txId, woundWait)
	}
}
