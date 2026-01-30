package util

import (
	"sync"
)

type AccessList struct {
	lock sync.Mutex
	list []uint64
}

func NewAccessList() *AccessList {
	return &AccessList{
		lock: sync.Mutex{},
		list: nil,
	}
}

func (list *AccessList) GetList() []uint64 {
	list.lock.Lock()
	defer list.lock.Unlock()

	rList := list.list
	return rList
}

func (list *AccessList) Remove(txId uint64) {
	list.lock.Lock()
	defer list.lock.Unlock()

	id := Contains(list.list, txId)
	list.list = RemoveElement(list.list, id)
}

func (list *AccessList) Add(txId uint64) []uint64 {
	list.lock.Lock()
	defer list.lock.Unlock()

	oldList := make([]uint64, len(list.list))
	copy(oldList, list.list)
	list.list = append(list.list, txId)
	return oldList
}

type AccessTuple struct {
	TxId   uint64
	HopId  int32
	Shards []int32
}
type AccessDList struct {
	lock sync.Mutex
	list []AccessTuple
}

func NewAccessDList() *AccessDList {
	return &AccessDList{
		lock: sync.Mutex{},
		list: nil,
	}
}

func (list *AccessDList) Remove(txId uint64) {
	list.lock.Lock()
	defer list.lock.Unlock()

	id := -1
	for i, v := range list.list {
		if v.TxId == txId {
			id = i
			break
		}
	}

	if id != -1 {
		list.list = append(list.list[:id], list.list[id+1:]...)
	}
}

func (list *AccessDList) Add(txId uint64, hopId int32, shards []int32) []AccessTuple {
	list.lock.Lock()
	defer list.lock.Unlock()

	oldList := make([]AccessTuple, len(list.list))
	copy(oldList, list.list)
	list.list = append(list.list, AccessTuple{TxId: txId, HopId: hopId, Shards: shards})
	return oldList
}
