import (
	"sync"
)

var usersCache struct {
	row    *Users
	isDirty bool
	lock   sync.Mutex
}

func flushUsersCache(tx *bolt.Tx) {
	usersCache.lock.Lock()
	defer usersCache.lock.Unlock()
	if usersCache.row != nil && usersCache.isDirty {
		// Only write to DB if the cache is marked as dirty.
		bucket := tx.Bucket([]byte("tUsers"))
		if bucket == nil {
			panic("missing bucket tUsers")
		}
		var key UsersKey = usersCache.row.Key
		var row Users = *usersCache.row
		putData(bucket, mustJSON(key), mustJSON(row))
	}
	// Clear the cache after flushing.
	usersCache.row = nil
	usersCache.isDirty = false
}

func getUsersCache(key UsersKey) *Users {
	usersCache.lock.Lock()
	defer usersCache.lock.Unlock()
	if usersCache.row != nil && usersCache.row.Key == key {
		return usersCache.row // Cache Hit
	}
	return nil
}

func getUsers(tx *bolt.Tx, key UsersKey) ([]byte, Users) {
	// 1. Check cache first.
	if row := getUsersCache(key); row != nil {
		return mustJSON(key), *row
	}

	// 2. Cache Miss: Flush any old dirty data before loading new data.
	flushUsersCache(tx)

	// 3. Proceed with DB lookup.
	bucket := tx.Bucket([]byte("tUsers"))
	if bucket == nil {
		panic("missing bucket tUsers")
	}
	keyBytes := mustJSON(key)
	row := Users{}
	if data := bucket.Get(keyBytes); data != nil {
		if err := json.Unmarshal(data, &row); err != nil {
			panic(err)
		}
	}
	// 4. Update cache with the newly fetched, clean row.
	usersCache.row = &row
	usersCache.isDirty = false

	return keyBytes, row
}