package main

import (
	"ShardDB/chainManager"
	"ShardDB/server"
	"encoding/json"
	"flag"
	"math/rand"
	"strconv"
)

var numItems = 100000

func main() {
	items := flag.Int("item", 100000, "Number of items")

	rand.Seed(2068)
	//rand.Seed(time.Now().UnixNano())

	client := server.SetupClient(len(chainManager.GetChains(nil)))
	client.ExpTime = 15
	client.WarmUpTime = 5
	client.PrintChain = make([]string, len(chainManager.GetChains(nil)))
	client.PrintChain[0] = "NewOrder"
	client.PrintChain[1] = "Delivery"
	client.PrintChain[2] = "Payment"
	numWarehouses := len(client.Shards) / 10
	numItems = *items

	client.GenRequest = func(threadId int, count int, isReady bool) (int, map[string]string, int) {
		if !isReady {
			// Warmup using NewOrder transaction
			return genvNewOrder(numWarehouses)
		}

		txnType := rand.Intn(100)

		if txnType < 48 {
			return genvNewOrder(numWarehouses)
		} else if txnType < 96 {
			return genvPayment(numWarehouses)
		} else {
			return genvDelivery(numWarehouses)
		}
	}

	client.StartExperiment()
}

func genvNewOrder(warehouses int) (int, map[string]string, int) {
	params := map[string]string{
		"w_id": strconv.FormatInt(int64(rand.Intn(warehouses)), 10),
		"d_id": strconv.FormatInt(int64(rand.Intn(10)), 10),
		"c_id": strconv.FormatInt(int64(rand.Intn(3000)), 10),
		"date": strconv.FormatInt(int64(rand.Intn(10000)), 10)}

	allLocal := 1
	itemIds := make([]uint64, 10)
	supplierWarehouseIDs := make([]uint64, 10)
	orderQuantities := make([]uint64, 10)
	for i := 1; i < 11; i++ {
		wid := params["w_id"]
		if warehouses > 1 && rand.Intn(100) < 1 {
			for wid == params["w_id"] {
				wid = strconv.FormatInt(int64(rand.Intn(warehouses)), 10)
			}
			allLocal = 0
		}

		itemIds[i-1] = uint64(rand.Intn(numItems))
		v, _ := strconv.ParseUint(wid, 10, 64)
		supplierWarehouseIDs[i-1] = v
		orderQuantities[i-1] = uint64(rand.Intn(10) + 1)
	}

	params["o_all_local"] = strconv.FormatUint(uint64(allLocal), 10)
	items, _ := json.Marshal(itemIds)
	params["itemIDs"] = string(items)
	suppliers, _ := json.Marshal(supplierWarehouseIDs)
	params["supplierWarehouseIDs"] = string(suppliers)
	quantities, _ := json.Marshal(orderQuantities)
	params["orderQuantities"] = string(quantities)

	shardId, _ := strconv.ParseUint(params["w_id"], 10, 64)
	shardId = shardId * 10

	return 0, params, int(shardId)
}

func genvPayment(warehouses int) (int, map[string]string, int) {
	wid := int64(rand.Intn(warehouses))
	cwid := wid
	if warehouses > 1 && rand.Intn(100) < 15 {
		for wid == cwid {
			cwid = int64(rand.Intn(warehouses))
		}
	}

	params := map[string]string{
		"w_id":   strconv.FormatInt(wid, 10),
		"d_id":   strconv.FormatInt(int64(rand.Intn(10)), 10),
		"c_w_id": strconv.FormatInt(cwid, 10),
		"c_d_id": strconv.FormatInt(int64(rand.Intn(10)), 10),
		"c_id":   strconv.FormatInt(int64(rand.Intn(3000)), 10),
		"amount": strconv.FormatInt(int64(rand.Intn(100)), 10)}

	shardId, _ := strconv.ParseUint(params["w_id"], 10, 64)
	shardId = shardId * 10
	return 2, params, int(shardId)
}

func genvDelivery(warehouses int) (int, map[string]string, int) {
	params := map[string]string{
		"w_id":         strconv.FormatInt(int64(rand.Intn(warehouses)), 10),
		"o_carrier_id": strconv.FormatInt(int64(10), 10),
		"date":         strconv.FormatInt(int64(10), 10)}

	shardId, _ := strconv.ParseUint(params["w_id"], 10, 64)
	shardId = shardId * 10
	return 1, params, int(shardId)
}
