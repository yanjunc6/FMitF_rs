package server

import (
	ChainMgr "ShardDB/chainManager"
	"ShardDB/proto"
	"ShardDB/server/config"
	"ShardDB/util"
	"context"
	"flag"
	"fmt"
	"github.com/boltdb/bolt"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"log"
	"math"
	"net"
	"os"
	"strconv"
	"sync"
	"sync/atomic"
	"time"
)

type HopNode struct {
	nodeId    int
	chains    []*ChainMgr.Chain
	txHistory *sync.Map
	server    *grpc.Server

	db    *bolt.DB
	ready bool

	Shards      []proto.HopNodeClient
	Addrs       []string
	ShardNumber int

	numAbort   []int32
	numMessage []int32

	mu sync.Mutex
	proto.UnimplementedHopNodeServer
}

func runNode(conf config.Options, nodeId int, verbose bool) {
	// Create Database
	dbname := "server/db/District" + strconv.FormatInt(int64(nodeId), 10) + ".db"
	db, err := bolt.Open(dbname, 0600, nil)
	if err != nil {
		log.Fatal(err)
	}

	var addrs []string
	var shards []proto.HopNodeClient
	for _, shard := range conf.Shards {
		addrs = append(addrs, shard.Addr)
		shards = append(shards, nil)
	}

	node := &HopNode{nodeId: nodeId, chains: ChainMgr.GetChains(db), txHistory: &sync.Map{},
		db: db, Shards: shards, Addrs: addrs, ShardNumber: len(addrs), ready: false}
	node.numAbort = make([]int32, len(node.chains))
	node.numMessage = make([]int32, len(node.chains))
	ChainMgr.Pull = node.PullImpl

	util.NumShards = node.ShardNumber
	util.Verbose = verbose
	server := grpc.NewServer()
	node.server = server
	proto.RegisterHopNodeServer(server, node)
	ln, err := net.Listen("tcp", conf.Shards[nodeId].Addr)
	if err != nil {
		log.Panic(err)
	}

	go func() {
		var wg sync.WaitGroup
		wg.Add(len(addrs))
		for i := range addrs {
			go node.connectToPeer(i, &wg)
		}
		wg.Wait()

		node.ready = true
	}()

	defer server.Stop()
	if err := server.Serve(ln); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}

var ongoing = sync.Map{}

func StartNode() {
	configFile := flag.String("config", "config.toml", "The path to the config file")
	nodeId := flag.Int("id", 0, "The id of node")
	mode := flag.Int("mode", 0, "Running mode")
	verbose := flag.Bool("v", false, "print debug log or not")
	preVerify := flag.Bool("p", false, "Without verification")
	flag.Parse()

	util.IsPreVerification = *preVerify
	util.SetConcurrentMode(*mode)
	var conf config.Options
	err := config.ReadConfig(&conf, *configFile) //default config file "config.toml"
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read config: %v\n", err)
		os.Exit(1)
	}
	runNode(conf, *nodeId, *verbose)
}

func (_node *HopNode) ProcessRequest(ctx context.Context, in *proto.TrxReq) (*proto.TrxRes, error) {
	switch util.ConcurrentMode {
	case util.LockMode:
		return _node.ProcessRequest2PL(ctx, in)
	case util.IC3Mode, util.DistributedIC3Mode:
		ongoing.Store(in.Info.Trxid, in)
		res, err := _node.ProcessRequestConstrained(ctx, in)
		ongoing.Delete(in.Info.Trxid)
		return res, err
	}
	return &proto.TrxRes{}, nil
}

func (_node *HopNode) ProcessRequestGlobal(ctx context.Context, rpcRequest *proto.TrxReq) (*proto.TrxRes, map[string]string, map[int][]*proto.RWSet) {
	accessMap := make(map[int][]*proto.RWSet)
	resultList := make([][]string, len(_node.Shards))

	chainId := rpcRequest.Info.Chainid

	wg := new(sync.WaitGroup)
	wg.Add(len(_node.Shards))
	mutex := sync.Mutex{}
	atomic.AddInt32(&_node.numMessage[chainId], int32(len(_node.Shards)))
	for i, shard := range _node.Shards {
		go func(id int, server proto.HopNodeClient) {
			rpcResponse, err := server.ProcessHop(ctx, rpcRequest)
			if err != nil {
				if util.Verbose {
					log.Println("!!!! Error request in global: ", err, "\n", rpcRequest)
				}
				return
			}

			mutex.Lock()
			accessMap[id] = rpcResponse.RWSets
			resultList[id] = rpcResponse.Results
			mutex.Unlock()

			wg.Done()
		}(i, shard)
	}
	wg.Wait()

	status := proto.Status_Success
	if util.ConcurrentMode != util.LockMode {
		msgs := make(map[int]*proto.TxRWSets)
		for k, v := range accessMap {
			msg := &proto.TxRWSets{
				RW:   v,
				TxId: rpcRequest.Info.Trxid,
			}
			msgs[k] = msg
		}

		wg.Add(len(msgs))
		atomic.AddInt32(&_node.numMessage[chainId], int32(len(msgs)))
		for k, msg := range msgs {
			go func(shardId int, endMsg *proto.TxRWSets) {
				_, err := _node.Shards[shardId].Prepare(ctx, endMsg)
				if err != nil {
					status = proto.Status_Abort
				}
				wg.Done()
			}(k, msg)
		}

		wg.Wait()

		wg.Add(len(msgs))
		atomic.AddInt32(&_node.numMessage[chainId], int32(len(msgs)))
		for k, msg := range msgs {
			go func(shardId int, endMsg *proto.TxRWSets) {
				_node.Shards[shardId].Unlock(ctx, endMsg)
				wg.Done()
			}(k, msg)
		}
		wg.Wait()

		if status == proto.Status_Abort {
			atomic.AddInt32(&_node.numAbort[chainId], 1)
			return &proto.TrxRes{Status: status}, rpcRequest.Params, accessMap
		}
	}

	return &proto.TrxRes{Status: status, Info: rpcRequest.Info}, _node.chains[chainId].Aggregate(rpcRequest.Info.Hopid, resultList, rpcRequest.Params), accessMap
}

func (_node *HopNode) ProcessRequest2PL(ctx context.Context, rpcRequest *proto.TrxReq) (*proto.TrxRes, error) {
	rpcResponse := &proto.TrxRes{Info: rpcRequest.Info, Results: nil}
	accessMap := make(map[int][]*proto.RWSet)
	var shardId int
	var err error

	chainId := rpcRequest.Info.Chainid
	params := rpcRequest.Params

	rpcRequest, params, shardId = _node.chains[chainId].NextReq(rpcResponse, params, _node.ShardNumber)
	rpcRequest.Dependency = make(map[int32]*proto.IdList)
	rpcRequest.Dependency[0] = &proto.IdList{}

	// While not the last hop
	for rpcRequest != nil {
		hopType := _node.chains[rpcRequest.Info.Chainid].HopType(rpcRequest.Info.Hopid)

		if hopType == util.GlobalHop {
			accessMapCopy := make(map[int][]*proto.RWSet)
			rpcResponse, params, accessMapCopy = _node.ProcessRequestGlobal(ctx, rpcRequest)

			for k, v := range accessMapCopy {
				_, ok := accessMap[k]
				if ok {
					accessMap[k] = append(accessMap[k], v...)
				} else {
					accessMap[k] = v
				}
			}
		} else {
			if shardId == _node.nodeId {
				rpcResponse, err = _node.ProcessHop(ctx, rpcRequest)
			} else {
				atomic.AddInt32(&_node.numMessage[chainId], 1)
				rpcResponse, err = _node.Shards[shardId].ProcessHop(ctx, rpcRequest)
			}

			if err != nil {
				if util.Verbose {
					log.Println("!!!! Error request: ", err, "\n", rpcRequest)
				}
				return &proto.TrxRes{Status: proto.Status_Abort}, err
			}

			_, ok := accessMap[shardId]
			if ok {
				accessMap[shardId] = append(accessMap[shardId], rpcResponse.RWSets...)
			} else {
				accessMap[shardId] = rpcResponse.RWSets
			}
		}

		rpcRequest, params, shardId = _node.chains[chainId].NextReq(rpcResponse, params, _node.ShardNumber)
	}

	status := proto.Status_Success

	msgs := make(map[int]*proto.TxRWSets)
	for k, v := range accessMap {
		msg := &proto.TxRWSets{
			RW:   v,
			TxId: rpcResponse.Info.Trxid,
		}
		msgs[k] = msg
	}

	wg := new(sync.WaitGroup)
	wg.Add(len(msgs))
	atomic.AddInt32(&_node.numMessage[chainId], int32(len(msgs)))
	for k, msg := range msgs {
		go func(shardId int, endMsg *proto.TxRWSets) {
			_, err = _node.Shards[shardId].Prepare(ctx, endMsg)
			if err != nil {
				//log.Println("Fail prepare ", endMsg, "at ", _node.nodeId, err)
				status = proto.Status_Abort
			}
			wg.Done()
		}(k, msg)
	}

	wg.Wait()

	wg.Add(len(msgs))
	atomic.AddInt32(&_node.numMessage[chainId], int32(len(msgs)))
	for k, msg := range msgs {
		go func(shardId int, endMsg *proto.TxRWSets) {
			_, err = _node.Shards[shardId].Unlock(ctx, endMsg)
			wg.Done()
		}(k, msg)
	}
	wg.Wait()

	if status == proto.Status_Abort {
		atomic.AddInt32(&_node.numAbort[chainId], 1)
	}

	return &proto.TrxRes{Status: status}, nil
}

func (_node *HopNode) ProcessRequest2PLPartial(rpcRequest *proto.TrxReq, shardId int) (*proto.TrxRes, map[string]string, map[int][]*proto.RWSet) {
	rpcResponse := &proto.TrxRes{Info: rpcRequest.Info, Results: nil}
	accessMap := make(map[int][]*proto.RWSet)
	var err error

	chainId := rpcRequest.Info.Chainid
	params := rpcRequest.Params

	// While not the last hop
	for {
		hopType := _node.chains[chainId].HopType(rpcRequest.Info.Hopid)
		if shardId == _node.nodeId {
			rpcResponse, err = _node.ProcessHop(context.Background(), rpcRequest)
		} else {
			atomic.AddInt32(&_node.numMessage[chainId], 1)
			rpcResponse, err = _node.Shards[shardId].ProcessHop(context.Background(), rpcRequest)
		}

		if err != nil {
			if util.Verbose {
				log.Println("!!!! Error request: ", err, "\n", rpcRequest)
			}
			return &proto.TrxRes{Status: proto.Status_Abort}, nil, nil
		}

		_, ok := accessMap[shardId]
		if ok {
			accessMap[shardId] = append(accessMap[shardId], rpcResponse.RWSets...)
		} else {
			accessMap[shardId] = rpcResponse.RWSets
		}

		if hopType == util.MergedHopEnd {
			break
		}

		rpcRequest, params, shardId = _node.chains[chainId].NextReq(rpcResponse, params, _node.ShardNumber)
	}

	status := proto.Status_Success
	msgs := make(map[int]*proto.TxRWSets)
	for k, v := range accessMap {
		msg := &proto.TxRWSets{
			RW:   v,
			TxId: rpcResponse.Info.Trxid,
		}
		msgs[k] = msg
	}

	wg := new(sync.WaitGroup)
	wg.Add(len(msgs))
	atomic.AddInt32(&_node.numMessage[chainId], int32(len(msgs)))
	for k, msg := range msgs {
		go func(shardId int, endMsg *proto.TxRWSets) {
			_, err = _node.Shards[shardId].Prepare(context.Background(), endMsg)
			if err != nil {
				log.Println("Fail prepare ", endMsg, "at ", _node.nodeId, err)
				status = proto.Status_Abort
			}
			wg.Done()
		}(k, msg)
	}

	wg.Wait()
	aborted := status == proto.Status_Abort

	wg.Add(len(msgs))
	atomic.AddInt32(&_node.numMessage[chainId], int32(len(msgs)))
	for k, msg := range msgs {
		go func(shardId int, endMsg *proto.TxRWSets) {
			endMsg.Aborted = aborted
			_, err = _node.Shards[shardId].Unlock(context.Background(), endMsg)
			wg.Done()
		}(k, msg)
	}
	wg.Wait()

	rpcResponse.Status = status

	if status == proto.Status_Abort {
		atomic.AddInt32(&_node.numAbort[chainId], 1)
	}

	return rpcResponse, params, accessMap
}

func (_node *HopNode) ProcessRequest2PLOrdered(rpcRequest *proto.TrxReq, shardId int) (*proto.TrxRes, map[string]string, map[int][]*proto.RWSet) {
	rpcResponse := &proto.TrxRes{Info: rpcRequest.Info, Results: nil}
	accessMap := make(map[int][]*proto.RWSet)
	var err error

	chainId := rpcRequest.Info.Chainid
	params := rpcRequest.Params

	// While not the last hop
	for {
		hopType := _node.chains[chainId].HopType(rpcRequest.Info.Hopid)
		if shardId == _node.nodeId {
			rpcResponse, err = _node.ProcessHop(context.Background(), rpcRequest)
		} else {
			atomic.AddInt32(&_node.numMessage[chainId], 1)
			rpcResponse, err = _node.Shards[shardId].ProcessHop(context.Background(), rpcRequest)
		}

		if err != nil {
			if util.Verbose {
				log.Println("!!!! Error request: ", err, "\n", rpcRequest)
			}
			return &proto.TrxRes{Status: proto.Status_Abort}, nil, nil
		}

		_, ok := accessMap[shardId]
		if ok {
			accessMap[shardId] = append(accessMap[shardId], rpcResponse.RWSets...)
		} else {
			accessMap[shardId] = rpcResponse.RWSets
		}

		if hopType == util.MergedHopEnd {
			break
		}

		_node.UpdateHistory(rpcRequest.Info.Trxid, rpcRequest.Info.Hopid)

		rpcRequest, params, shardId = _node.chains[chainId].NextReq(rpcResponse, params, _node.ShardNumber)
	}

	wg := new(sync.WaitGroup)
	wg.Add(len(accessMap))
	atomic.AddInt32(&_node.numMessage[chainId], int32(len(accessMap)))
	for k, v := range accessMap {
		msg := &proto.TxRWSets{
			RW:   v,
			TxId: rpcResponse.Info.Trxid,
		}

		go func(shardId int, endMsg *proto.TxRWSets) {
			_, err = _node.Shards[shardId].Unlock(context.Background(), endMsg)
			wg.Done()
		}(k, msg)
	}
	wg.Wait()

	rpcResponse.Status = proto.Status_Success

	return rpcResponse, params, accessMap
}

func (_node *HopNode) ProcessRequestConstrained(ctx context.Context, in *proto.TrxReq) (*proto.TrxRes, error) {
	rpcResponse := &proto.TrxRes{Info: in.Info, Results: nil}
	accessMap := make(map[int][]*proto.RWSet)
	var shardId int
	var err error
	var rpcRequest *proto.TrxReq

	chainId := in.Info.Chainid
	params := in.Params
	if !util.IsWoundWaitLock && _node.chains[chainId].Sort != nil {
		params = _node.chains[chainId].Sort(in.Params)
	}

	rpcRequest, params, shardId = _node.chains[chainId].NextReq(rpcResponse, params, _node.ShardNumber)
	rpcRequest.Dependency = make(map[int32]*proto.IdList)
	rpcRequest.Dependency[0] = &proto.IdList{}

	// While not the last hop
	for rpcRequest != nil {
		hopType := _node.chains[rpcRequest.Info.Chainid].HopType(rpcRequest.Info.Hopid)

		if hopType == util.MergedHopBegin {
			for {
				rpcInfoCopy := &proto.TrxInfo{Trxid: rpcRequest.Info.Trxid, Hopid: rpcRequest.Info.Hopid, Chainid: rpcRequest.Info.Chainid}
				depCopy := make(map[int32]*proto.IdList, len(rpcRequest.Dependency)) // Pre-allocate capacity for efficiency
				for k, v := range rpcRequest.Dependency {
					depCopy[k] = v
				}
				rpcRequestCopy := &proto.TrxReq{Info: rpcInfoCopy, Params: params, Dependency: depCopy}

				var rpcResponseCopy *proto.TrxRes
				var paramCopy map[string]string
				var accessMapCopy map[int][]*proto.RWSet

				if util.IsWoundWaitLock {
					rpcResponseCopy, paramCopy, accessMapCopy = _node.ProcessRequest2PLPartial(rpcRequestCopy, shardId)
				} else {
					rpcResponseCopy, paramCopy, accessMapCopy = _node.ProcessRequest2PLOrdered(rpcRequestCopy, shardId)
				}

				if rpcResponseCopy.Status == proto.Status_Success {
					params = paramCopy
					rpcResponse = rpcResponseCopy

					for k, v := range accessMapCopy {
						_, ok := accessMap[k]
						if ok {
							accessMap[k] = append(accessMap[k], v...)
						} else {
							accessMap[k] = v
						}
					}

					break
				}
			}

			if util.ConcurrentMode == util.IC3Mode {
				atomic.AddInt32(&_node.numMessage[chainId], int32(len(_node.Shards)))
				for _, shard := range _node.Shards {
					go shard.EndHop(context.Background(), &proto.TrxInfo{Trxid: rpcResponse.Info.Trxid, Hopid: rpcResponse.Info.Hopid})
				}
			}
		} else if hopType == util.GlobalHop {
			accessMapCopy := make(map[int][]*proto.RWSet)
			rpcResponse, params, accessMapCopy = _node.ProcessRequestGlobal(ctx, rpcRequest)

			for k, v := range accessMapCopy {
				_, ok := accessMap[k]
				if ok {
					accessMap[k] = append(accessMap[k], v...)
				} else {
					accessMap[k] = v
				}
			}

			if util.ConcurrentMode == util.IC3Mode {
				atomic.AddInt32(&_node.numMessage[chainId], int32(len(_node.Shards)))
				for _, shard := range _node.Shards {
					go shard.EndHop(context.Background(), &proto.TrxInfo{Trxid: rpcResponse.Info.Trxid, Hopid: rpcResponse.Info.Hopid})
				}
			}
		} else {
			if shardId == _node.nodeId {
				rpcResponse, err = _node.ProcessHop(ctx, rpcRequest)
			} else {
				atomic.AddInt32(&_node.numMessage[chainId], 1)
				rpcResponse, err = _node.Shards[shardId].ProcessHop(context.Background(), rpcRequest)
			}

			if err != nil {
				if util.Verbose {
					log.Println("!!!! Error request: ", err, "\n", rpcRequest)
				}
				return &proto.TrxRes{Status: proto.Status_Abort}, nil
			}

			_, ok := accessMap[shardId]
			if ok {
				accessMap[shardId] = append(accessMap[shardId], rpcResponse.RWSets...)
			} else {
				accessMap[shardId] = rpcResponse.RWSets
			}
		}

		if util.ConcurrentMode == util.DistributedIC3Mode {
			_node.UpdateHistory(rpcRequest.Info.Trxid, rpcRequest.Info.Hopid)
		}
		rpcRequest, params, shardId = _node.chains[chainId].NextReq(rpcResponse, params, _node.ShardNumber)
	}

	status := proto.Status_Success

	if util.ConcurrentMode == util.DistributedIC3Mode {
		// Remove all access
		atomic.AddInt32(&_node.numMessage[chainId], int32(len(accessMap)))
		for id, v := range accessMap {
			msg := &proto.TxRWSets{
				RW:   v,
				TxId: in.Info.Trxid,
			}
			go func(shardId int, msg *proto.TxRWSets) {
				_, err = _node.Shards[shardId].EndTx(context.Background(), msg)
				if err != nil {
					log.Println("Fail endTX ", msg, "at ", _node.nodeId, err)
				}
			}(id, msg)
		}
	} else {
		// Broadcast completion
		atomic.AddInt32(&_node.numMessage[chainId], int32(len(_node.Shards)))
		for i, _ := range _node.Shards {
			go func(index int) {
				_, err = _node.Shards[index].EndTx(context.Background(), &proto.TxRWSets{RW: accessMap[index], TxId: in.Info.Trxid})
				if err != nil {
					log.Println("Fail endTX at ", _node.nodeId, err)
				}
			}(i)
		}
	}

	_node.TxFinish(in.Info.Trxid)
	return &proto.TrxRes{Status: status}, nil
}

// ProcessHop
// RPC function for receiving ProcessHop event
func (_node *HopNode) ProcessHop(ctx context.Context, in *proto.TrxReq) (*proto.TrxRes, error) {
	// Register scheduler and process corresponding hop
	res, _ := _node.chains[in.Info.Chainid].Handle(in, _node.txHistory)

	switch util.ConcurrentMode {
	case util.IC3Mode:
		hopType := _node.chains[in.Info.Chainid].HopType(in.Info.Hopid)
		isWait := _node.chains[in.Info.Chainid].IsWait(in.Info.Hopid)
		if hopType == util.NormalHop && isWait {
			// Send hop finished event to all nodes
			atomic.AddInt32(&_node.numMessage[in.Info.Chainid], int32(len(_node.Shards)))
			hopId := in.Info.Hopid

			// Early broadcast transaction commitment if possible
			if _node.chains[in.Info.Chainid].EndHop == hopId {
				hopId = int32(math.MaxInt32)
			}

			for _, shard := range _node.Shards {
				go shard.EndHop(context.Background(), &proto.TrxInfo{Trxid: in.Info.Trxid, Hopid: hopId})
			}
		}
	case util.DistributedIC3Mode:
		_node.UpdateHistory(in.Info.Trxid, in.Info.Hopid)
	}

	return res, nil
}

func (_node *HopNode) EndTx(ctx context.Context, msg *proto.TxRWSets) (*proto.Empty, error) {
	switch util.ConcurrentMode {
	case util.IC3Mode:
		for _, rw := range msg.RW {
			ChainMgr.AccessSchedulers[rw.TableName].RemoveAccess(rw.Key, msg.TxId)
		}
		_node.TxFinish(msg.TxId)
	case util.DistributedIC3Mode:
		for _, rw := range msg.RW {
			ChainMgr.AccessDSchedulers[rw.TableName].RemoveAccessD(rw.Key, msg.TxId)
		}
		_node.TxFinish(msg.TxId)
	}

	return &proto.Empty{}, nil
}

func (_node *HopNode) Unlock(ctx context.Context, msg *proto.TxRWSets) (*proto.Empty, error) {
	for _, rw := range msg.RW {
		if util.ConcurrentMode == util.IC3Mode && msg.Aborted {
			ChainMgr.AccessSchedulers[rw.TableName].RemoveAccess(rw.Key, msg.TxId)
		}

		ChainMgr.LockSchedulers[rw.TableName].UnLock(rw.Key, msg.TxId)
	}

	return &proto.Empty{}, nil
}

func (_node *HopNode) Prepare(ctx context.Context, msg *proto.TxRWSets) (*proto.Empty, error) {
	for _, rw := range msg.RW {
		if !ChainMgr.LockSchedulers[rw.TableName].Prepare(rw.Key, msg.TxId) {
			return &proto.Empty{}, fmt.Errorf("prepare failed")
		}
	}
	return &proto.Empty{}, nil
}

func (_node *HopNode) EndHop(ctx context.Context, in *proto.TrxInfo) (*proto.Empty, error) {
	if util.Verbose {
		log.Println("update hop history", in)
	}

	_node.UpdateHistory(in.Trxid, in.Hopid)

	return &proto.Empty{}, nil
}

func (_node *HopNode) Pull(ctx context.Context, in *proto.IdList) (*proto.History, error) {
	history := &proto.History{His: make(map[uint64]int32)}
	for _, d := range in.List {
		for {
			currentHop, loaded := _node.txHistory.Load(d.TxId)
			if !loaded || currentHop.(int32) < d.HopId {
				time.Sleep(2 * time.Millisecond)
				continue
			}
			history.His[d.TxId] = currentHop.(int32)
			break
		}
	}
	return history, nil
}

func (_node *HopNode) PullImpl(chainId int32, serverId int32, msg *proto.IdList) {
	atomic.AddInt32(&_node.numMessage[chainId], 1)
	history, _ := _node.Shards[serverId].Pull(context.Background(), msg)

	for k, v := range history.His {
		_node.UpdateHistory(k, v)
	}
}

func (_node *HopNode) UpdateHistory(txId uint64, hopId int32) {
	actual, loaded := _node.txHistory.LoadOrStore(txId, hopId) // Try to load, if not present, store hopId

	if loaded { // Key was already present
		if hopId > actual.(int32) {
			_node.txHistory.Store(txId, hopId) // Update only if hopId is greater
		}
	}
}

func (_node *HopNode) TxFinish(trxId uint64) {
	_node.txHistory.Store(trxId, int32(math.MaxInt32)) // Update only if hopId is greater
}

func (_node *HopNode) Tok(ctx context.Context, in *proto.Empty) (*proto.Empty, error) {
	return &proto.Empty{}, nil
}

func (_node *HopNode) Tick(ctx context.Context, in *proto.Empty) (*proto.Empty, error) {
	for {
		if _node.ready {
			return &proto.Empty{}, nil
		} else {
			time.Sleep(100 * time.Millisecond)
		}
	}
}

func (_node *HopNode) Shutdown(ctx context.Context, in *proto.Empty) (*proto.Stat, error) {
	verbose := util.Verbose
	util.Verbose = false
	if verbose {
		if false && util.ConcurrentMode != util.LockMode {
			log.Println("History: ")
			_node.txHistory.Range(func(key, value interface{}) bool {
				fmt.Printf("Key: %v, Value: %v\n", key, value)
				return true // Continue iterating
			})

			if _node.nodeId%10 == 0 {
				min := uint64(math.MaxUint64)
				ongoing.Range(func(key, value any) bool {
					current := key
					if current.(uint64) < min {
						min = current.(uint64)
					}
					return true
				})

				if min != uint64(math.MaxUint64) {
					value, _ := ongoing.Load(min)
					log.Println("Ongoing: ", min, value)
					h, _ := _node.txHistory.Load(min)
					log.Println("History: ", min, h)
					for _, dep := range value.(*proto.TrxReq).Dependency {
						for _, d := range dep.List {
							h, _ = _node.txHistory.Load(d.TxId)
							log.Println("History: ", d, h)
						}
					}
				}
			}
		}
	}

	go func() {
		time.Sleep(100 * time.Millisecond)
		_node.server.GracefulStop()
		os.Exit(0)
	}()

	log.Println("Average hop total time: ", float32(ChainMgr.TimeOfHops)/float32(ChainMgr.NumberOfHops), "ms")
	log.Println("Average DB time: ", float32(ChainMgr.TimeOfDB)/float32(ChainMgr.NumberOfDB), "ms")
	log.Println("Average hop processing time: ", float32(ChainMgr.TimeOfProcessedHops)/float32(ChainMgr.NumberOfProcessedHops), "ms")
	log.Println("Server ", _node.nodeId, " shutdown")
	return &proto.Stat{NumAbort: _node.numAbort, NumMessage: _node.numMessage}, nil
}

func (_node *HopNode) connectToPeer(id int, wg *sync.WaitGroup) {
	const maxRetry = 300
	retires := 0

	for {
		conn, err := grpc.Dial(_node.Addrs[id], grpc.WithTransportCredentials(insecure.NewCredentials()))

		if err != nil {
			retires += 1
			if retires >= maxRetry {
				log.Fatalf("Fail to connect to peer %d: %v", id, err)
			}

			time.Sleep(50 * time.Millisecond)
			continue
		}

		if util.Verbose {
			log.Println("Server ", _node.nodeId, " connected to peer ", id)
		}
		_node.Shards[id] = proto.NewHopNodeClient(conn)

		for {
			_, err = _node.Shards[id].Tok(context.Background(), &proto.Empty{})
			if err != nil {
				time.Sleep(50 * time.Millisecond)
				continue
			}
			break
		}

		wg.Done()
		return
	}
}
