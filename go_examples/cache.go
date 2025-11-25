package chainManager

import (
	"encoding/json"
	"github.com/boltdb/bolt"
)

func mustJSON(value any) []byte {
	bytes, err := json.Marshal(value)
	if err != nil {
		panic(err)
	}
	return bytes
}


type Users struct {
	Key           UsersKey
	Firstname     string
	Lastname      string
	Nickname      string
	Password      string
	Email         string
	Rating        uint64
	Balance       float32
	Creation_date uint64
	Region        uint64
}

type UsersKey struct {
	ID uint64
}

// Global variable for the single-row write-back cache.
var userCache struct {
	user    *Users
	isDirty bool
}


// flushUsersCache writes the cached data to the database if it's dirty.
func flushUsersCache(tx *bolt.Tx) {
	if userCache.user != nil && userCache.isDirty {
		// Only write to DB if the cache is marked as dirty.
		bucket := tx.Bucket([]byte("tUsers"))
		if bucket == nil {
			panic("missing bucket tUsers")
		}
		var key UsersKey = userCache.user.Key
		var row Users = *userCache.user
		putData(bucket, mustJSON(key), mustJSON(row))
	}
	// Clear the cache after flushing.
	userCache.user = nil
	userCache.isDirty = false
}

// getUsers fetches user data, utilizing the cache.
func getUsers(tx *bolt.Tx, key UsersKey) Users {
	// 1. Check cache first.
	if userCache.user != nil && userCache.user.Key == key {
		return *userCache.user // Cache Hit
	}

	// 2. Cache Miss: Flush any old dirty data before loading new data.
	flushUsersCache(tx)

	// 3. Proceed with DB lookup.
	bucket := tx.Bucket([]byte("tUsers"))
	if bucket == nil {
		panic("missing bucket tUsers")
	}
	keyBytes := mustJSON(key)
	var row Users
	if data := bucket.Get(keyBytes); data != nil {
		if err := json.Unmarshal(data, &row); err != nil {
			panic(err)
		}
	}
	// 4. Update cache with the newly fetched, clean row.
	userCache.user = &row
	userCache.isDirty = false

	return row
}

// putUsers updates the cache and marks it as dirty (write-back).
func putUsers(tx *bolt.Tx, key UsersKey, row Users) {
	// If the cache holds a different user, flush it first.
	if userCache.user != nil && userCache.user.Key != key {
		flushUsersCache(tx)
	}

	// Place the new row in the cache and mark it as dirty.
	row.Key = key
	userCache.user = &row
	userCache.isDirty = true
}