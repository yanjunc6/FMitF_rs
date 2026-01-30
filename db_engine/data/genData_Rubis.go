package main

import (
	chainMgr "ShardDB/chainManager"
	"encoding/json"
	"flag"
	"fmt"
	"github.com/boltdb/bolt"
	"hash/fnv"
	"log"
	"math/rand"
	"os"
	"strconv"
	"time"
)

func main() {
	numShards := flag.Int("shards", 4, "number of shards")
	numItems := flag.Int("items", 10, "number of items of per server")
	rand.Seed(time.Now().UnixNano())

	flag.Parse()

	rand.Seed(time.Now().UnixNano())
	names := make(map[int][]chainMgr.Usernames)
	numUsers := 10

	numServers := *numShards * 10
	for w := 0; w < *numShards; w++ {
		for d := 0; d < 10; d++ {
			log.Println("Creating District", w*10+d)
			dbname := "District" + strconv.FormatInt(int64(w*10+d), 10) + ".db"
			os.Remove(dbname)
			db, err := bolt.Open(dbname, 0600, nil)
			if err != nil {
				log.Fatal(err)
			}

			err = db.Update(func(tx *bolt.Tx) error {
				items, err := tx.CreateBucketIfNotExists([]byte("tItems"))
				if err != nil {
					return fmt.Errorf("could not create AItem bucket: %v", err)
				}
				users, err := tx.CreateBucketIfNotExists([]byte("tUsers"))
				if err != nil {
					return fmt.Errorf("could not create AItem bucket: %v", err)
				}
				tx.CreateBucketIfNotExists([]byte("tComments"))
				tx.CreateBucketIfNotExists([]byte("tBids"))
				tx.CreateBucketIfNotExists([]byte("tBuyNow"))

				iid := uint64(10*w + d)
				for i := 0; i < *numItems; i += 1 {
					itemKey := chainMgr.ItemsKey{
						Id: chainMgr.UUID(iid),
					}

					item := chainMgr.Items{
						Key:           itemKey,
						Name:          fmt.Sprintf("Item %d", iid),
						Description:   fmt.Sprintf("Description for item %d", iid),
						Initial_price: float32(rand.Intn(1000)),
						Max_bid:       0,
						Quantity:      uint64(rand.Intn(100)),
						Reserve_price: float32(rand.Intn(500)),
						Buy_now:       float32(rand.Intn(3000)),
						Nb_of_bids:    0,
						Start_date:    uint64(rand.Int63n(1000000)),
						Seller:        chainMgr.UUID(rand.Int63n(int64(*numShards * numUsers))),
						Category:      uint64(rand.Int63n(10)),
						Region:        uint64(rand.Intn(10)),
					}

					keybytes, err := json.Marshal(itemKey)
					itemBytes, err := json.Marshal(item)
					err = items.Put(keybytes, itemBytes)
					if err != nil {
						log.Fatal(err)
					}

					iid += uint64(numServers)
				}

				uid := uint64(10*w + d)
				for i := 0; i < numUsers; i += 1 {
					userKey := chainMgr.UsersKey{
						Id: chainMgr.UUID(uid),
					}

					user := chainMgr.Users{
						Key:           userKey,
						Firstname:     fmt.Sprintf("User %d", uid),
						Lastname:      fmt.Sprintf("Lastname %d", uid),
						Nickname:      fmt.Sprintf("Nickname %d", uid),
						Password:      fmt.Sprintf("PASSWORD %d", uid),
						Email:         fmt.Sprintf("Email %d", uid),
						Rating:        uint64(rand.Intn(5)),
						Balance:       float32(rand.Intn(1000)),
						Creation_date: uint64(rand.Int63n(1000000)),
						Region:        uint64(rand.Intn(10)),
					}

					h := fnv.New64a()
					h.Write([]byte(user.Nickname))
					v := int(h.Sum64()) % numServers
					names[v] = append(names[v], chainMgr.Usernames{Key: chainMgr.UsernamesKey{Nickname: user.Nickname}, Id: userKey.Id, Password: user.Password, Counter: 0})

					keybytes, err := json.Marshal(userKey)
					userBytes, err := json.Marshal(user)
					err = users.Put(keybytes, userBytes)
					if err != nil {
						log.Fatal(err)
					}

					uid += uint64(numServers)
				}

				return nil
			})
			db.Close()
		}
	}

	for w := 0; w < *numShards; w++ {
		for d := 0; d < 10; d++ {
			server := w*10 + d
			dbname := "District" + strconv.FormatInt(int64(w*10+d), 10) + ".db"
			db, err := bolt.Open(dbname, 0600, nil)
			if err != nil {
				log.Fatal(err)
			}

			err = db.Update(func(tx *bolt.Tx) error {
				usernames, err := tx.CreateBucketIfNotExists([]byte("tUsernames"))
				if err != nil {
					return fmt.Errorf("could not create AItem bucket: %v", err)
				}

				for _, v := range names[server] {
					keybytes, err := json.Marshal(v.Key)
					usernameBytes, err := json.Marshal(v)
					err = usernames.Put(keybytes, usernameBytes)
					if err != nil {
						log.Fatal(err)
					}
				}
				return nil
			})
		}
	}
}
