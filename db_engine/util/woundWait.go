package util

import (
	"sync"
)

type WoundWaitLock struct {
	mu       sync.Mutex
	locked   bool
	holder   uint64
	prepared bool
	waitList map[uint64]chan struct{}
}

func NewWoundWaitLock() *WoundWaitLock {
	return &WoundWaitLock{
		waitList: make(map[uint64]chan struct{}),
	}
}

// Return if the lock is wounded
// If true, also return the id being wounded
func (l *WoundWaitLock) Lock(id uint64) (bool, uint64) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if !l.locked {
		l.locked = true
		l.holder = id
		l.prepared = false
		return false, 0
	}

	if id == l.holder {
		return false, 0
	}

	if id < l.holder && !l.prepared {
		// Wound the current holder
		wounded := l.holder
		l.holder = id
		return true, wounded
	}

	// Wait for the lock
	ch := make(chan struct{})
	l.waitList[id] = ch
	l.mu.Unlock()
	<-ch
	l.mu.Lock()

	l.locked = true
	l.holder = id
	l.prepared = false
	return false, 0
}

func (l *WoundWaitLock) Unlock(id uint64) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if !l.locked || l.holder != id {
		return
	}

	l.locked = false
	l.holder = 0
	l.prepared = false

	// Notify waiters
	for waiterID, ch := range l.waitList {
		close(ch)
		delete(l.waitList, waiterID)
		break // Only notify one waiter
	}
}

func (l *WoundWaitLock) Prepare(id uint64) bool {
	l.mu.Lock()
	defer l.mu.Unlock()

	if !l.locked || l.holder != id {
		return false
	}

	l.prepared = true
	return true
}
