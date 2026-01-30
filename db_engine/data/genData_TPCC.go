package main

import (
	chainMgr "ShardDB/chainManager"
	"encoding/json"
	"flag"
	"fmt"
	"github.com/boltdb/bolt"
	"log"
	"math/rand"
	"os"
	"strconv"
	"time"
)

var vtables map[string]*bolt.Bucket

func main() {
	numWhse := flag.Int("w", 4, "number of warehouses")
	numDperW := 10
	rand.Seed(time.Now().UnixNano())

	flag.Parse()

	// Each wareHouse
	for w := 0; w < *numWhse; w++ {
		for d := 0; d < numDperW; d++ {
			dbname := "District" + strconv.FormatInt(int64(w*numDperW+d), 10) + ".db"
			os.Remove(dbname)
			db, err := bolt.Open(dbname, 0600, nil)
			if err != nil {
				log.Fatal(err)
			}

			err = db.Update(func(tx *bolt.Tx) error {
				if d == 0 {
					vLoadWareHouse(tx, w)
				}
				vCreateTables(tx)
				vLoadDistrict(w, d)
				vLoadCustomer(w, d)
				vLoadStockItem(w, d)

				return nil
			})

			if w == 0 && d == 0 {
				db.View(func(tx *bolt.Tx) error {
					vBucketSize(tx, "tCustomer")
					vBucketSize(tx, "tItem")
					vBucketSize(tx, "tStock")
					return nil
				})
			}
		}
	}
}

func vLoadWareHouse(tx *bolt.Tx, w int) {
	warehouses, _ := tx.CreateBucketIfNotExists([]byte("tWarehouse"))

	warehousesKey := chainMgr.WarehouseKey{W_ID: uint64(w)}

	warehouse := chainMgr.Warehouse{
		Key:   warehousesKey,
		W_TAX: rand.Float32() / 10,
		W_YTD: 0,
	}

	key, _ := json.Marshal(warehousesKey)
	wBytes, _ := json.Marshal(warehouse)

	warehouses.Put(key, wBytes)
}

func vLoadDistrict(w int, d int) {
	dKey := chainMgr.DistrictKey{
		D_ID:   uint64(d),
		D_W_ID: uint64(w),
	}
	district := chainMgr.District{
		Key:         dKey,
		D_TAX:       rand.Float32() / 10,
		D_YTD:       0,
		D_NEXT_O_ID: 0,
	}

	dKeyBytes, _ := json.Marshal(dKey)
	districtBytes, _ := json.Marshal(district)

	vtables["district"].Put(dKeyBytes, districtBytes)
}

func vLoadCustomer(w int, d int) {
	cKey := chainMgr.CustomerKey{
		C_ID:   0,
		C_D_ID: uint64(d),
		C_W_ID: uint64(w),
	}
	customer := chainMgr.Customer{
		Key: cKey,
	}

	for c := 0; c < 3000; c++ {
		cKey.C_ID = uint64(c)
		customer.Key.C_ID = uint64(c)
		customer.C_SINCE = uint64(time.Now().UnixNano())
		customer.C_CREDIT_LIMIT = float32(rand.Intn(40000) + 10000)
		customer.C_DISCOUNT = rand.Float32() * 0.5
		customer.C_BALANCE = rand.Float32()*2000 - 1000
		customer.C_YTD_PAYMENT = rand.Float32() * 1000
		customer.C_PAYMENT_CNT = float32(rand.Intn(100))
		customer.C_DELIVERY_CNT = float32(rand.Intn(100))

		cKeyBytes, _ := json.Marshal(cKey)
		customerBytes, _ := json.Marshal(customer)

		vtables["customer"].Put(cKeyBytes, customerBytes)
	}
}

func vLoadStockItem(w int, d int) {
	itemKey := chainMgr.ItemKey{
		W_ID: uint64(w),
		I_ID: 0,
	}
	item := chainMgr.Item{
		Key:     itemKey,
		I_PRICE: rand.Float32() * 100,
	}
	stockKey := chainMgr.StockKey{
		S_W_ID: uint64(w),
		S_I_ID: 0,
	}
	stock := chainMgr.Stock{
		Key:        stockKey,
		S_QUANTITY: uint64(rand.Intn(90) + 10),
	}
	numIPerD := 10000

	for i := 0; i < numIPerD; i++ {
		itemKey.I_ID = uint64(i*10 + d)
		item.I_PRICE = rand.Float32() * 100
		item.I_IM_ID = uint64(rand.Intn(10000))
		item.Key = itemKey

		stockKey.S_I_ID = itemKey.I_ID
		stock.S_YTD = float32(rand.Intn(10000))
		stock.S_QUANTITY = uint64(rand.Intn(90) + 10)
		stock.S_ORDER_CNT = 0
		stock.S_REMOTE_CNT = 0
		stock.Key = stockKey

		sKeyBytes, _ := json.Marshal(stockKey)
		iKeyBytes, _ := json.Marshal(itemKey)
		itemBytes, _ := json.Marshal(item)
		stockBytes, _ := json.Marshal(stock)

		vtables["item"].Put(iKeyBytes, itemBytes)
		vtables["stock"].Put(sKeyBytes, stockBytes)
	}
}

func vCreateTables(tx *bolt.Tx) {
	vtables = make(map[string]*bolt.Bucket)

	vtables["district"], _ = tx.CreateBucketIfNotExists([]byte("tDistrict"))
	vtables["customer"], _ = tx.CreateBucketIfNotExists([]byte("tCustomer"))
	vtables["stock"], _ = tx.CreateBucketIfNotExists([]byte("tStock"))
	vtables["item"], _ = tx.CreateBucketIfNotExists([]byte("tItem"))
	vtables["newOrder"], _ = tx.CreateBucketIfNotExists([]byte("tNewOrder"))
	vtables["order"], _ = tx.CreateBucketIfNotExists([]byte("tOrder"))
	vtables["orderLine"], _ = tx.CreateBucketIfNotExists([]byte("tOrderLine"))
	vtables["history"], _ = tx.CreateBucketIfNotExists([]byte("tHistory"))
}

func vBucketSize(tx *bolt.Tx, tableName string) {

	var totalSize int
	tx.Bucket([]byte(tableName)).ForEach(func(k, v []byte) error {
		totalSize += len(k) + len(v)
		return nil
	})

	fmt.Printf("Approximate size of bucket %q: %d bytes\n", tableName, totalSize)
}
