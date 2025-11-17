package chainManager
// Showcase global hop

import (
	"ShardDB/proto"
	"ShardDB/util"
	"encoding/json"
	"github.com/boltdb/bolt"
	"strconv"
)

type ItemsKey struct {
	Iid uint64
}

type Items struct {
	Key      ItemsKey
	Price    float32
	Category int
}

func RubisScheds() {
	LockSchedulers["Items"] = NewScheduler()

	AccessSchedulers["Items"] = NewScheduler()
}

func RubisChains(db *bolt.DB) []*Chain {
	return []*Chain{RubisGetItemByCategoryImpl(db)}
}

func RubisGetItemByCategoryImpl(db *bolt.DB) *Chain {
	// A single hop that get all items with a given category
	hops := make([]*Hop, 1)

	hops[0] = &Hop{
		id:         0,
		hopType:    util.GlobalHop, // global hop type
		isReadOnly: true,           // read only hop
		process:    RubisGetItemByCategoryHop,   // use boltdb scan
		aggregate:  RubisGetItemByCategoryHopAggregate, // Provide a aggregation function
		conflicts:  map[int32]int32{},
	}

	return &Chain{db: db, hops: hops, NextReq: RubisGetItemByCategoryNextReq}
}

func RubisGetItemByCategoryHop(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error) {
	var rwSet []*proto.RWSet
	category, _ := strconv.ParseUint(in.Params["Category"], 10, 64)

	// Example of using bolt iterator to scan table
	b := tx.Bucket([]byte("Items"))
	c := b.Cursor()

	item := Items{}
	itemList := make([]Items, 0)
	for k, v := c.First(); k != nil; k, v = c.Next() {
		json.Unmarshal(v, &item)

		// Matched category
		if item.Category == int(category) {
			itemList = append(itemList, item)
			rwSet = AddRWSet(rwSet, "Items", k)
		}
	}

	// Convert the list of items to string and add to results
	itemString, _ := json.Marshal(itemList)
	var results []string
	results = append(results, string(itemString))

	return &proto.TrxRes{Status: proto.Status_Success, Info: in.Info, Results: results, RWSets: rwSet}, nil
}

// The aggregation function, currently only have concatenate
func RubisGetItemByCategoryHopAggregate(results [][]string, params map[string]string) map[string]string {
	// Define the type of the list
	var items []Items

	// Concatenate the results from all servers into a list
	params["itemList"] = ListConcatenate(results, items)

	return params
}

// The list concatenate function is included here just for reference
// Don't add this to the generated code
func ListConcatenate[T any](src [][]string, dest []T) string {
	var tmp []T
	for _, v := range src {
		if len(v) == 0 {
			continue
		}

		json.Unmarshal([]byte(v[0]), &tmp)
		dest = append(dest, tmp...)
	}

	result, _ := json.Marshal(dest)

	return string(result)
}

func RubisGetItemByCategoryNextReq(in *proto.TrxRes, params map[string]string,
	shards int) (*proto.TrxReq, map[string]string, int) {
	// Fixed steps to set up the transaction request for the next hop
	hopId := in.Info.Hopid
	req := &proto.TrxReq{
		Info: in.Info,
	}
	nextShard := 0

	// Get the hopId for next hop
	// Set up the transaction parameters accordingly from the result of the last hop
	switch hopId {
	case 0:
		// nothing to do, the coordinator will send to all servers
		break
	default:
		return nil, params, 0
	}

	// Fixed steps
	req.Params = params
	req.Info.Hopid += 1
	req.Dependency = in.Dependency
	return req, params, nextShard
}
