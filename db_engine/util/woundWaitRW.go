package util

import (
	"log"
	"sync"
)

type WoundWaitRWLock struct {
	mu       sync.Mutex
	locked   bool
	writer   uint64
	prepared bool
	readers  []uint64
	name     string
}

func NewWoundWaitRWLock(name string) *WoundWaitRWLock {
	return &WoundWaitRWLock{name: name}
}

func (l *WoundWaitRWLock) TryRLock(id uint64) bool {
	l.mu.Lock()
	defer l.mu.Unlock()

	if !l.locked {
		return true
	}

	if id == l.writer {
		return true
	}

	if IsWoundWaitLock && id < l.writer && !l.prepared {
		return true
	}

	return false
}

func (l *WoundWaitRWLock) RLock(id uint64) (bool, []uint64) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if Verbose {
		log.Println("RLock ", l)
	}

	if !l.locked {
		l.readers = append(l.readers, id)
		return true, nil
	}

	if id == l.writer {
		return true, nil
	}

	if IsWoundWaitLock && id < l.writer && !l.prepared {
		// Wound the current holder
		wounded := l.writer
		l.locked = false
		l.readers = append(l.readers, id)
		return true, []uint64{wounded}
	}

	return false, nil
}

func (l *WoundWaitRWLock) TryLock(id uint64) bool {
	l.mu.Lock()
	defer l.mu.Unlock()

	// Not locked, no reader
	if !l.locked && len(l.readers) == 0 {
		return true
	}

	// Locked by itself
	if l.locked && id == l.writer {
		return true
	}

	// Not locked, has reader
	if IsWoundWaitLock && len(l.readers) > 0 && !l.prepared {
		for _, reader := range l.readers {
			if reader < id {
				return false
			}
		}

		return true
	}

	// Locked by younger
	if IsWoundWaitLock && id < l.writer && !l.prepared {
		return true
	}

	return false
}

func (l *WoundWaitRWLock) Lock(id uint64) (bool, []uint64) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if Verbose {
		log.Println("Lock ", l)
	}

	// Not locked, no reader
	if !l.locked && len(l.readers) == 0 {
		l.locked = true
		l.writer = id
		l.prepared = false
		return true, nil
	}

	// Locked by itself
	if l.locked && id == l.writer {
		return true, nil
	}

	// Not locked, has reader
	if IsWoundWaitLock && len(l.readers) > 0 && !l.prepared {
		for _, reader := range l.readers {
			if reader < id {
				return false, nil
			}
		}

		readers := make([]uint64, len(l.readers))
		copy(readers, l.readers)
		l.readers = []uint64{}
		l.locked = true
		l.writer = id
		l.prepared = false
		return true, readers
	}

	// Locked by younger
	if IsWoundWaitLock && id < l.writer && !l.prepared {
		// Wound the current holder
		wounded := l.writer
		l.writer = id
		return true, []uint64{wounded}
	}

	return false, nil
}

func (l *WoundWaitRWLock) Unlock(id uint64) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if Verbose {
		log.Println("Unlock ", l)
	}

	// If is a reader, remove itself
	var newReaders []uint64
	for _, reader := range l.readers {
		if reader != id {
			newReaders = append(newReaders, reader)
		}
	}
	l.readers = newReaders

	// Not the writer, do nothing
	if !l.locked || l.writer != id {
		return
	}

	l.locked = false
	l.writer = 0
	l.prepared = false
}

func (l *WoundWaitRWLock) Prepare(id uint64) bool {
	l.mu.Lock()
	defer l.mu.Unlock()

	// If is a reader,
	for _, reader := range l.readers {
		if reader == id {
			l.prepared = true
			return true
		}
	}

	// If is a writer
	if !l.locked || l.writer != id {
		return false
	}

	l.prepared = true
	return true
}
