// --- Fixed block
package chainManager

import (
	"ShardDB/proto"
	"ShardDB/util"
	"encoding/json"
	"github.com/boltdb/bolt"
	"strconv"
	"sync"
)
// ---

const numOrderLine = 10

func TPCCNewOrderChainImpl(db *bolt.DB) *Chain {
	// Create the list of hops
	hops := make([]*Hop, 12)

	hops[0] = &Hop{
		id:         0,
		hopType:    util.NormalHop,
		isReadOnly: true,
		isRecordDep:false,	// Don't have c-edge, false
		process:    TPCCNewOrderWareHouseHop,
		waitHop:    map[int32]int32{},
	}

	for i := 1; i < 11; i++ {
		// hop := strconv.FormatUint(uint64(i), 10)
		hops[i] = &Hop{
			id:         int32(i),
			hopType:    util.MergedHop,
			isReadOnly: false,
			isRecordDep:true, // Has c-edge and not the last hop, true
			process:    TPCCNewOrderStockHop,
			waitHop:    map[int32]int32{0: 1}, // key: chainId, value: hopId that it is waiting for
		}
	}
	hops[1].hopType = util.MergedHopBegin
	hops[10].hopType = util.MergedHopEnd

	hops[11] = &Hop{
		id:         11,
		hopType:    util.NormalHop,
		isReadOnly: false,
		isRecordDep:false, // Last hop of a transaction, false
		process:    TPCCNewOrderDistrictHop,
		waitHop:    map[int32]int32{0: 11},
	}

	// add field names
	return &Chain{db: db, hops: hops, NextReq: TPCCNewOrderNextReq}
}


// Define the function for each hop
// tx: the database pointer
// in: the request, mostly use "in.Params" to get parameter, or "hopId := in.Info.Hopid" to get current hop id
func TPCCNewOrderWareHouseHop(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error) {
	// Fixed step to setup a list of objects being accessed
	var rwSet []*proto.RWSet
	// Get the warehouse using function defined in global file
	// If don't need to write back the object, ignore the first return value
	key, warehouse := getWareHouse(tx, in.Params)
	// For each object/row accessed, add it to rwSet
	// second param is the schdeuler name in TPCCScheds(), basically the table name
	// Third param is the seriliazed key of the row, []byte
	rwSet = AddRWSet(rwSet, "Warehouse", key)

	// To return a value
	// Convert the return value to a string and then append to result
	// If returning a struct, use json.Marshal to serialize it to string
	var results []string
	results = append(results, strconv.FormatFloat(float64(warehouse.W_TAX), 'f', 2, 32))

	// Last line is basically the same
	// If no return results, just remove "Results: results"
	// Also add the rwSet to the response
	return &proto.TrxRes{Status: proto.Status_Success, Info: in.Info, Results: results, RWSets: rwSet}, nil
}

func TPCCNewOrderStockHop(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error) {
	var rwSet []*proto.RWSet
	// Get item and stock
	// We will write stock so get the first return as the key to write
	itemKeyBytes, item := getItem(tx, in.Params)
	rwSet = AddRWSet(rwSet, "Item", itemKeyBytes)
	stockKeyBytes, stock := getStock(tx, in.Params)
	rwSet = AddRWSet(rwSet, "Stock", stockKeyBytes)

	// Get the qty and convert it to string
	qty, _ := strconv.ParseUint(in.Params["qty"], 10, 64)

	// The hops operation to do anything
	if stock.S_QUANTITY - qty >= 10 {
		stock.S_QUANTITY = stock.S_QUANTITY - qty;
	} else {
		stock.S_QUANTITY = stock.S_QUANTITY + 91 - qty;
	}

	if in.Params["iwid"] != in.Params["wid"] {
		stock.S_REMOTE_CNT += 1
	}

	// Finally write the stock back
	// First convert stock to string
	// Then use a putData function to write
	//	 first parameter: get the table with table name, similar to getStock function
	//   second parameter: the []byte of object key
	//   last parameter:   the []byte of object
	sBytes, _ := json.Marshal(stock)
	putData(tx.Bucket([]byte("tStock")), stockKeyBytes, sBytes)

	var results []string
	results = append(results, strconv.FormatFloat(float64(item.I_PRICE), 'f', 2, 32))

	return &proto.TrxRes{Status: proto.Status_Success, Info: in.Info, Results: results, RWSets: rwSet}, nil
}

func TPCCNewOrderDistrictHop(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error) {
	var rwSet []*proto.RWSet

	wid, _ := strconv.ParseUint(in.Params["wid"], 10, 64)
	did, _ := strconv.ParseUint(in.Params["did"], 10, 64)
	cid, _ := strconv.ParseUint(in.Params["cid"], 10, 64)

	dKey, d := getDistrict(tx, in.Params)
	rwSet = AddRWSet(rwSet, "District", dKey)

	oid := d.D_NEXT_O_ID
	d.D_NEXT_O_ID += 1

	dBytes, _ := json.Marshal(d)
	putData(tx.Bucket([]byte("tDistrict")), dKey, dBytes)

	newOrder := NewOrder{
		NO_O_ID: oid,
		NO_D_ID: did,
		NO_W_ID: wid,
	}
	newOrderBytes, _ := json.Marshal(newOrder)
	putData(tx.Bucket([]byte("tNewOrder")), newOrderBytes, newOrderBytes)
	rwSet = AddRWSet(rwSet, "NewOrder", newOrderBytes)

	order := Order{
		O_KEY:       newOrder,
		O_C_ID:      cid,
		O_OL_CNT:    numOrderLine,
		O_ALL_LOCAL: false,
	}
	orderBytes, _ := json.Marshal(order)
	putData(tx.Bucket([]byte("tOrder")), newOrderBytes, orderBytes)
	rwSet = AddRWSet(rwSet, "Order", newOrderBytes)


	for i := 0; i < numOrderLine; i++ {
		hop := strconv.FormatUint(uint64(i+1), 10)

		iid, _ := strconv.ParseUint(in.Params["iid" + hop], 10, 64)
		iwid, _ := strconv.ParseUint(in.Params["iwid" + hop], 10, 64)
		qty, _ := strconv.ParseUint(in.Params["qty" + hop], 10, 64)
		price, _ := strconv.ParseFloat(in.Params["iprice" + hop], 32)
		
		orderLineKey := OrderLineKey {
			OL_O_ID:  oid,
			OL_D_ID:  did,
			OL_W_ID:  wid,
			OL_NUMBER: uint64(i),
		}

		orderLine := OrderLine {
			OL_KEY:  orderLineKey,
			OL_I_ID: iid,
			OL_SUPPLY_W_ID: iwid,
			OL_AMOUNT: uint64(float32(qty) * price),
		}

		orderLineKeyBytes, _ := json.Marshal(orderLineKey)
		orderLineBytes, _ := json.Marshal(orderLine)
		putData(tx.Bucket([]byte("tOrderLine")), orderLineKeyBytes, orderLineBytes)
		rwSet = AddRWSet(rwSet, "OrderLine", orderLineKeyBytes)
	}

	return &proto.TrxRes{Status: proto.Status_Success, Info: in.Info, RWSets: rwSet}, nil
}


// Define how to construct parameters for next hop
// in: the return response from last hop
// params: 
// history: used for concurrency contorl, will be set with fixed steps so you don't need to care
// shards: the number of total partitions, can be used for modular operation, like id%shards to get the corresponding nodeId
func TPCCNewOrderNextReq(in *proto.TrxRes, params map[string]string, history []uint32,
	shards int) (*proto.TrxReq, map[string]string, []uint32, int) {
	// --- Fixed steps to set up the transaction request for the next hop
	hopId := in.Info.Hopid
	req := &proto.TrxReq{
		Info: in.Info,
	}
	nextShard := 0
	// ---

	// Set up the transaction parameters accordingly from the result of the last hop
	switch hopId {
	case 0:
		// First hop, find hopId of the warehouse

		// Get wid from parameters and convert to uint64
		wid, _ := strconv.ParseUint(params["wid"], 10, 64)

		// Get next hop id according to the partition function
		nextShard = int(wid * 10)
	case 1:
		fallthrough
	case 2:
		fallthrough
	case 3:
		fallthrough
	case 4:
		fallthrough
	case 5:
		fallthrough
	case 6:
		fallthrough
	case 7:
		fallthrough
	case 8:
		fallthrough
	case 9:
		fallthrough
	case 10:
		if hopId > 1 {
			// set the item price from the returned result
			// Skip hopId = 1 since the result is from hop 0
			lastHop := strconv.FormatUint(uint64(hopId-1), 10)
			params["iprice"+lastHop] = in.Results[0]
		}

		// Set the parameter of the repeated hop
		hop := strconv.FormatUint(uint64(hopId), 10)
		params["iwid"] = params["iwid"+hop]
		params["iid"] = params["iid"+hop]
		params["qty"] = params["qty"+hop]

		// Find the hopId by warehouse id and item id
		w, _ := strconv.ParseUint(params["iwid"], 10, 64)
		i, _ := strconv.ParseUint(params["iid"], 10, 64)
		nextShard = int(w*10 + i%10)
	case 11:
		params["iprice10"] = in.Results[0]

		// Find the hopId by warehouseid and district id
		w, _ := strconv.ParseUint(params["wid"], 10, 64)
		d, _ := strconv.ParseUint(params["did"], 10, 64)
		nextShard = int(w*10 + d)
	default:
		return nil, params, history, 0
	}

	// Fixed steps
	history = append(history, uint32(nextShard))
	req.Params = params
	req.History = history
	req.Info.Hopid += 1
	req.Dependency = in.Dependency
	return req, params, history, nextShard
}
