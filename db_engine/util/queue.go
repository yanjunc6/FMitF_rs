package util

import "sync/atomic"

type AtomicQueue struct {
	// Items to be added to channel
	items chan any
	// counter used to incremented and decremented atomically.
	counter uint64
}

func NewAtomicQueue() *AtomicQueue {
	return &AtomicQueue{
		items:   make(chan any, 1),
		counter: 0,
	}
}

func (q *AtomicQueue) Enqueue(item any) {
	// counter variable atomically incremented
	atomic.AddUint64(&q.counter, 1)
	// put item to channel
	q.items <- item
}

func (q *AtomicQueue) Dequeue() any {
	// read item from channel
	item := <-q.items
	// counter variable decremented atomically.
	atomic.AddUint64(&q.counter, ^uint64(0))
	return item
}

func (q *AtomicQueue) IsEmpty() bool {
	return q.counter == 0
}
