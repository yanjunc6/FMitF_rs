package main

import (
	"flag"
	"github.com/boltdb/bolt"
	"log"
	"math/rand"
	"os"
	"strconv"
	"time"
)

func main() {
	numShards := flag.Int("shards", 4, "number of shards")

	flag.Parse()

	rand.Seed(time.Now().UnixNano())

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
				tx.CreateBucketIfNotExists([]byte("tGraph"))
				tx.CreateBucketIfNotExists([]byte("tStatus"))
				tx.CreateBucketIfNotExists([]byte("tWall"))
				tx.CreateBucketIfNotExists([]byte("tActivity"))
				return nil
			})
			db.Close()
		}
	}
}
