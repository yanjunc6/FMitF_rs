package chainManager

import (
	"encoding/json"
	"sync"

	bolt "go.etcd.io/bbolt"
)

// Global variable for the single-row write-back cache with a RWMutex.
var orderCache struct {
	lock    sync.RWMutex
	row     *Order
	isDirty bool
}

// flushOrderCacheUnlocked contains the core logic for flushing.
// It is unexported and "unsafe" because it assumes the caller has already acquired the necessary lock.
func flushOrderCacheUnlocked(tx *bolt.Tx) {
	if orderCache.row != nil && orderCache.isDirty {
		bucket := tx.Bucket([]byte("tOrder"))
		if bucket == nil {
			panic("missing bucket tOrder")
		}
		var key OrderKey = orderCache.row.Key
		var row Order = *orderCache.row
		putData(bucket, mustJSON(key), mustJSON(row))
	}
	// Clear the cache after flushing.
	orderCache.row = nil
	orderCache.isDirty = false
}

// flushOrderCache is the public, thread-safe API for flushing the cache.
// It acquires the lock before calling the core logic.
func flushOrderCache(tx *bolt.Tx) {
	orderCache.lock.Lock()
	defer orderCache.lock.Unlock()
	flushOrderCacheUnlocked(tx)
}

// getOrder fetches data, utilizing the cache in a highly concurrent and safe manner.
func getOrder(tx *bolt.Tx, key OrderKey) ([]byte, Order) {
	// 1. Fast path (Read-only): Use a read lock for concurrent cache checks.
	orderCache.lock.RLock()
	if orderCache.row != nil && orderCache.row.Key == key {
		row := *orderCache.row
		orderCache.lock.RUnlock()
		return mustJSON(key), row // Cache Hit
	}
	orderCache.lock.RUnlock()

	// 2. Slow path (Write path): Acquire an exclusive lock.
	orderCache.lock.Lock()
	defer orderCache.lock.Unlock()

	// 3. Double-check in case another goroutine populated the cache while we waited for the lock.
	if orderCache.row != nil && orderCache.row.Key == key {
		return mustJSON(key), *orderCache.row // Cache Hit
	}

	// 4. Cache Miss: Call the "unlocked" helper since we already hold the lock.
	flushOrderCacheUnlocked(tx)

	// 5. Proceed with DB lookup.
	bucket := tx.Bucket([]byte("tOrder"))
	if bucket == nil {
		panic("missing bucket tOrder")
	}
	keyBytes := mustJSON(key)
	var row Order
	if data := bucket.Get(keyBytes); data != nil {
		if err := json.Unmarshal(data, &row); err != nil {
			panic(err)
		}
	}
	
	// 6. Update cache with the newly fetched, clean row.
	orderCache.row = &row
	orderCache.isDirty = false

	return keyBytes, row
}

// putOrder updates the cache and marks it as dirty (write-back).
func putOrder(tx *bolt.Tx, key OrderKey, row Order) {
	orderCache.lock.Lock()
	defer orderCache.lock.Unlock()

	// If the cache holds a different row, call the "unlocked" helper to flush it.
	if orderCache.row != nil && orderCache.row.Key != key {
		flushOrderCacheUnlocked(tx)
	}

	// Place the new row in the cache and mark it as dirty.
	row.Key = key
	orderCache.row = &row
	orderCache.isDirty = true
}