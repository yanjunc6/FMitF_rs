// For each application, create a global.go for table definition and shared functions

// --- Fixed block for packaging
package chainManager

import (
	"encoding/json"
	"github.com/boltdb/bolt"
)
// --- 


// Define the table
type Warehouse struct {
	// Key: W_ID
	W_ID  uint64
	W_TAX float32
	W_YTD float32
}

// For compsite key --- keys with multiple columns
// Create a struct to represent the key
type DistrictKey struct {
	D_ID   uint64
	D_W_ID uint64
}

type District struct {
	D_KEY       DistrictKey   // Use the defined key as columns directly
	D_TAX       float32
	D_YTD       float32
	D_NEXT_O_ID uint64
}

type CustomerKey struct {
	C_ID   uint64
	C_D_ID uint64
	C_W_ID uint64
}

type Customer struct {
	C_KEY        CustomerKey
	C_CREDIT     string
	C_CREDIT_LIM float32
	C_DISCOUNT   float32
	C_BALANCE    float32
}

type NewOrder struct {
	NO_O_ID uint64
	NO_D_ID uint64
	NO_W_ID uint64
}

type Order struct {
	O_KEY       NewOrder
	O_C_ID      uint64
	O_OL_CNT    uint64
	O_ALL_LOCAL bool
}

type OrderLineKey struct {
	OL_O_ID   uint64
	OL_D_ID   uint64
	OL_W_ID   uint64
	OL_NUMBER uint64
}

type OrderLine struct {
	OL_KEY         OrderLineKey
	OL_I_ID        uint64
	OL_SUPPLY_W_ID uint64
	OL_AMOUNT      uint64
}

type ItemKey struct {
	W_ID uint64
	I_ID uint64
}

type Item struct {
	I_KEY   ItemKey
	I_PRICE float32
}

type Stock struct {
	S_KEY        ItemKey
	S_QUANTITY   uint64
	S_DIST       string
	S_YTD        uint64
	S_ORDER_CNT  uint64
	S_REMOTE_CNT uint64
}

func TPCCScheds() {
	// Create one lock scheduler for each table
	LockSchedulers["Warehouse"] = NewScheduler()
	LockSchedulers["Item"] = NewScheduler()
	LockSchedulers["Stock"] = NewScheduler()
	LockSchedulers["District"] = NewScheduler()
	LockSchedulers["NewOrder"] = NewScheduler()
	LockSchedulers["Order"] = NewScheduler()
	LockSchedulers["OrderLine"] = NewScheduler()
	LockSchedulers["Customer"] = NewScheduler()

	// Create one access schedueler for each table in hops that has C-edges
	AccessSchedulers["Item"] = NewScheduler()
	AccessSchedulers["Stock"] = NewScheduler()
	// I ignored district/newOrder ... here since they are in the last hop of a transaction
	// No dependency is needed to be recorded.
}

func TPCCChains(db *bolt.DB) []*Chain {
	// Remove schedulers in this function
	return []*Chain{TPCCNewOrderChainImpl(db)}
}

// Function to read a column from the database
// tx: the pointer to the database
// params: a map of all input parameters --- everything in string
// returns:
//     []byte: a byte array representing the serialized key of the column
//     Warehouse: the column
func getWareHouse(tx *bolt.Tx, params map[string]string) ([]byte, Warehouse) {
	// Get the table from database
	// note the "tWarehouse" is the name of table when creating the data
	// Convention for this name: "t" + tablename
	warehouses := tx.Bucket([]byte("tWarehouse"))

	// convert the key to byte array --- requested by the database package bolt
	// For single column key, just convert it to []byte
	// See next function for composite key
	widBytes := []byte(params["wid"])

	// read the serialized warehouse object
	warehousesBytes := warehouses.Get(widBytes)

	// deserialize []byte to object
	var warehouse Warehouse
	json.Unmarshal(warehousesBytes, &warehouse)

	return widBytes, warehouse
}

func getDistrict(tx *bolt.Tx, params map[string]string) ([]byte, District) {
	districts := tx.Bucket([]byte("tDistrict"))

	// To get the composite key
	// First cast the string parameters back to the original type
	wid, _ := strconv.ParseUint(params["wid"], 10, 64)
	did, _ := strconv.ParseUint(params["did"], 10, 64)
	// Then construct the composite key struct
	dKey := DistrictKey{
		D_ID:   did,
		D_W_ID: wid,
	}
	// Finally, convert the struct to []btye 
	dKeyBytes, _ := json.Marshal(dKey)


	var d District
	json.Unmarshal(districts.Get(dKeyBytes), &d)
	return dKeyBytes, d
}

func getItem(tx *bolt.Tx, params map[string]string) ([]byte, Item) {
	items := tx.Bucket([]byte("tItem"))

	wid, _ := strconv.ParseUint(in.Params["iwid"], 10, 64)
	iid, _ := strconv.ParseUint(in.Params["iid"], 10, 64)
	itemKey := ItemKey{
		I_ID: iid,
		W_ID: wid,
	}

	itemKeyBytes, _ := json.Marshal(itemKey)
	var item Item
	json.Unmarshal(items.Get(itemKeyBytes), &item)

	return itemKeyBytes, item
}

func getStock(tx *bolt.Tx, params map[string]string) ([]byte, Stock) {
	stocks := tx.Bucket([]byte("tStock"))

	wid, _ := strconv.ParseUint(in.Params["iwid"], 10, 64)
	iid, _ := strconv.ParseUint(in.Params["iid"], 10, 64)
	stockKey := ItemKey{
		I_ID: iid,
		W_ID: wid,
	}

	stockKeyBytes, _ := json.Marshal(stockKey)
	var stock Stock
	json.Unmarshal(stocks.Get(stockKeyBytes), &stock)

	return stockKeyBytes, stock
}