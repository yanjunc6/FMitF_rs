package chainManager

import (
	"ShardDB/util"
	"sync"
)

type Scheduler struct {
	Name     string
	schedKey []string
	endKey   []string
	endHopId int32
	maps     *sync.Map
}

func NewScheduler() *Scheduler {
	return &Scheduler{maps: &sync.Map{}}
}

//-------------------- Schedueler -----------------
func (_sched *Scheduler) Register(params map[string]string) {
	key := getKey(params, _sched.schedKey)

	val, _ := _sched.maps.LoadOrStore(key, util.NewAtomicQueue())

	// If the queue is empty, non-blocking
	// Otherwise, wait
	val.(*util.AtomicQueue).Enqueue(1)
}

func (_sched *Scheduler) UnRegister(params map[string]string) {
	key := getKey(params, _sched.schedKey)

	val, _ := _sched.maps.LoadOrStore(key, util.NewAtomicQueue())

	// If the queue is empty, non-blocking
	// Otherwise, wait
	val.(*util.AtomicQueue).Dequeue()
}

//-------------------- 2PL -----------------
func (_sched *Scheduler) TryRLock(key string, trxId uint64) bool {
	if _sched == nil {
		return true
	}
	val, _ := _sched.maps.LoadOrStore(key, util.NewWoundWaitRWLock(key))
	// If the queue is empty, non-blocking
	// Otherwise, wait
	return val.(*util.WoundWaitRWLock).TryRLock(trxId)
}

func (_sched *Scheduler) RLock(key string, trxId uint64) (bool, []uint64) {
	if _sched == nil {
		return true, nil
	}
	val, _ := _sched.maps.LoadOrStore(key, util.NewWoundWaitRWLock(key))
	// If the queue is empty, non-blocking
	// Otherwise, wait
	return val.(*util.WoundWaitRWLock).RLock(trxId)
}

func (_sched *Scheduler) TryLock(key string, trxId uint64) bool {
	if _sched == nil {
		return true
	}
	val, _ := _sched.maps.LoadOrStore(key, util.NewWoundWaitRWLock(key))
	// If the queue is empty, non-blocking
	// Otherwise, wait
	return val.(*util.WoundWaitRWLock).TryLock(trxId)
}

func (_sched *Scheduler) Lock(key string, trxId uint64) (bool, []uint64) {
	if _sched == nil {
		return true, nil
	}
	val, _ := _sched.maps.LoadOrStore(key, util.NewWoundWaitRWLock(key))
	// If the queue is empty, non-blocking
	// Otherwise, wait
	return val.(*util.WoundWaitRWLock).Lock(trxId)
}

func (_sched *Scheduler) UnLock(key string, trxId uint64) {
	if _sched == nil {
		return
	}
	val, _ := _sched.maps.LoadOrStore(key, util.NewWoundWaitRWLock(key))

	// If the queue is empty, non-blocking
	// Otherwise, wait
	val.(*util.WoundWaitRWLock).Unlock(trxId)
}

func (_sched *Scheduler) Prepare(key string, trxId uint64) bool {
	if _sched == nil {
		return true
	}
	val, _ := _sched.maps.Load(key)

	return val.(*util.WoundWaitRWLock).Prepare(trxId)
}

//-------------------- IC3 -----------------
func (_sched *Scheduler) AddAccess(key string, txId uint64) []uint64 {
	if _sched == nil {
		return nil
	}

	val, _ := _sched.maps.LoadOrStore(key, util.NewAccessList())

	return val.(*util.AccessList).Add(txId)
}

func (_sched *Scheduler) RemoveAccess(key string, txId uint64) {
	if _sched == nil {
		return
	}

	val, _ := _sched.maps.LoadOrStore(key, util.NewAccessList())

	val.(*util.AccessList).Remove(txId)
}

//-------------------- DIC3 -----------------
func (_sched *Scheduler) AddAccessD(key string, txId uint64, hopId int32, shards []int32) []util.AccessTuple {
	val, _ := _sched.maps.LoadOrStore(key, util.NewAccessDList())

	return val.(*util.AccessDList).Add(txId, hopId, shards)
}

func (_sched *Scheduler) RemoveAccessD(key string, txId uint64) {
	val, _ := _sched.maps.Load(key)

	val.(*util.AccessDList).Remove(txId)
}

type EndScheduler struct {
	schedName string
	hopID     int32
}

func getKey(params map[string]string, schedKey []string) string {
	var key string
	for _, v := range schedKey {
		key += params[v] + "|"
	}
	return key
}
