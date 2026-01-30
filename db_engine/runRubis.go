package main

import (
	"ShardDB/chainManager"
	"ShardDB/server"
	"hash/fnv"
	"math/rand"
	"strconv"
	"sync"
	"sync/atomic"
	"time"
)

const numShardsR = 40
const numItemPerServerR = 10

const numUserRStatic = numItemPerServerR * numShardsR

var numUsersR int64 = numUserRStatic

const numItemRStatic = numItemPerServerR * numShardsR

var numItemR int64 = numItemRStatic
var uniqueIdR int64 = 1000

type genRequestFuncR func() (int, map[string]string, int)

func main() {
	//rand.Seed(2068)
	rand.Seed(time.Now().UnixNano())

	client := server.SetupClient(len(chainManager.GetChains(nil)))
	client.ExpTime = 10
	client.WarmUpTime = 0
	client.PrintChain = make([]string, len(chainManager.GetChains(nil)))
	client.PrintChain[0] = "UserAuth"
	client.PrintChain[1] = "RegisterUser"
	client.PrintChain[2] = "ViewUser"
	client.PrintChain[3] = "MakeComment"
	client.PrintChain[4] = "GetCommentsToUid"
	client.PrintChain[5] = "GetCommentsIid"
	client.PrintChain[6] = "GetItemsByRegion"
	client.PrintChain[7] = "GetItemsByCategory"
	client.PrintChain[8] = "RegisterItem"
	client.PrintChain[9] = "PutBid"
	client.PrintChain[10] = "GetBidsByItem"
	client.PrintChain[11] = "GetBidsByUser"
	client.PrintChain[12] = "BuyNow"
	client.PrintChain[13] = "CancelBuy"

	readFuncs := make([]genRequestFuncR, 8)
	readFuncs[0] = genUserauth
	readFuncs[1] = genViewuser
	readFuncs[2] = genGetcommentsbytouid
	readFuncs[3] = genGetcommentsbyiid
	readFuncs[4] = genGetitemsbyregion
	readFuncs[5] = genGetitemsbycategory
	readFuncs[6] = genGetbidsbyuser
	readFuncs[7] = genGetbidsbyitem

	frequentWriteFunc := make([]genRequestFuncR, 3)
	frequentWriteFunc[0] = genBuynow
	frequentWriteFunc[1] = genCancelBuy
	frequentWriteFunc[2] = genPutbid

	lessWriteFunc := make([]genRequestFuncR, 3)
	lessWriteFunc[0] = genRegisteruser
	lessWriteFunc[1] = genRegisteritem
	lessWriteFunc[2] = genMakecomment

	client.GenRequest = func(threadId int, count int, isReady bool) (int, map[string]string, int) {
		txnType := rand.Intn(100)
		if txnType < 80 {
			return frequentWriteFunc[rand.Intn(len(frequentWriteFunc))]()
		} else if txnType < 95 {
			return lessWriteFunc[rand.Intn(len(lessWriteFunc))]()
		} else {
			return readFuncs[rand.Intn(len(readFuncs))]()
		}
	}

	client.StartExperiment()
}

func genUserauth() (int, map[string]string, int) {
	uid := strconv.FormatInt(rand.Int63n(numItemRStatic), 10)
	params := map[string]string{
		"nickname": "Nickname " + uid,
		"password": "PASSWORD " + uid,
	}

	h := fnv.New64a()
	h.Write([]byte(params["nickname"]))
	shardId := h.Sum64() % numShardsR
	return 0, params, int(shardId)
}

func genRegisteruser() (int, map[string]string, int) {
	nuid := atomic.AddInt64(&numUsersR, 1)
	uid := strconv.FormatInt(nuid, 10)
	params := map[string]string{
		"uid":       uid,
		"nickname":  "Nickname " + uid,
		"password":  "PASSWORD " + uid,
		"firstname": "Firstname " + uid,
		"lastname":  "Lastname " + uid,
		"email":     "Email " + uid,
	}
	h := fnv.New64a()
	h.Write([]byte(params["nickname"]))
	shardId := h.Sum64() % numShardsR
	return 1, params, int(shardId)
}
func genViewuser() (int, map[string]string, int) {
	nuid := rand.Int63n(numUserRStatic)
	uid := strconv.FormatInt(nuid, 10)
	params := map[string]string{
		"uid": uid,
	}

	shardId := nuid % numShardsR
	return 2, params, int(shardId)
}

func genMakecomment() (int, map[string]string, int) {
	nuidt := rand.Int63n(numUserRStatic)
	nuidf := rand.Int63n(numUserRStatic)
	niid := rand.Int63n(numItemRStatic)
	nid := atomic.AddInt64(&uniqueIdR, 1)
	id := strconv.FormatInt(nid, 10)
	uidt := strconv.FormatInt(nuidt, 10)
	uidf := strconv.FormatInt(nuidf, 10)
	iid := strconv.FormatInt(niid, 10)

	params := map[string]string{
		"id":       id,
		"from_uid": uidf,
		"to_uid":   uidt,
		"iid":      iid,
		"comment":  "Comment " + uidf,
		"rating":   strconv.Itoa(rand.Intn(5)),
	}
	shardId := nuidt % numShardsR
	return 3, params, int(shardId)
}

func genGetcommentsbytouid() (int, map[string]string, int) {
	nuidt := rand.Int63n(numUserRStatic)
	uidt := strconv.FormatInt(nuidt, 10)
	params := map[string]string{
		"to_id": uidt,
	}

	shardId := rand.Intn(numShardsR)
	return 4, params, shardId
}
func genGetcommentsbyiid() (int, map[string]string, int) {
	niid := rand.Int63n(numItemRStatic)
	iid := strconv.FormatInt(niid, 10)
	params := map[string]string{
		"iid": iid,
	}

	shardId := rand.Intn(numShardsR)
	return 5, params, shardId
}
func genGetitemsbyregion() (int, map[string]string, int) {
	nrid := rand.Int63n(10)
	rid := strconv.FormatInt(nrid, 10)
	params := map[string]string{
		"region_id": rid,
	}
	shardId := rand.Intn(numShardsR)
	return 6, params, shardId
}
func genGetitemsbycategory() (int, map[string]string, int) {
	nrid := rand.Int63n(10)
	rid := strconv.FormatInt(nrid, 10)
	params := map[string]string{
		"category_id": rid,
	}
	shardId := rand.Intn(numShardsR)
	return 7, params, shardId
}

func genRegisteritem() (int, map[string]string, int) {
	nuid := rand.Int63n(numUserRStatic)
	uid := strconv.FormatInt(nuid, 10)
	niid := atomic.AddInt64(&numItemR, 1)
	iid := strconv.FormatInt(niid, 10)

	params := map[string]string{
		"iid":           iid,
		"name":          "Item " + iid,
		"description":   "Description for item " + iid,
		"initial_price": strconv.FormatFloat(float64(rand.Intn(1000)), 'f', 2, 64),
		"quantity":      strconv.FormatInt(int64(rand.Intn(100)), 10),
		"reserve_price": strconv.FormatFloat(float64(rand.Intn(500)), 'f', 2, 64),
		"buy_now":       strconv.FormatFloat(float64(rand.Intn(3000)), 'f', 2, 64),
		"date":          strconv.FormatInt(rand.Int63n(1000000), 10),
		"seller":        uid,
		"category":      strconv.FormatInt(rand.Int63n(10), 10),
		"region":        strconv.FormatInt(rand.Int63n(10), 10),
	}
	shardId := niid % numShardsR
	return 8, params, int(shardId)
}

func genPutbid() (int, map[string]string, int) {
	niid := rand.Int63n(numItemRStatic)
	iid := strconv.FormatInt(niid, 10)
	nuid := rand.Int63n(numUserRStatic)
	uid := strconv.FormatInt(nuid, 10)
	nid := atomic.AddInt64(&uniqueIdR, 1)
	id := strconv.FormatInt(nid, 10)

	params := map[string]string{
		"bid_id":      id,
		"iid":         iid,
		"max_bid_val": strconv.FormatFloat(float64(rand.Intn(1000)), 'f', 2, 64),
		"bid_val":     strconv.FormatFloat(float64(rand.Intn(500)), 'f', 2, 64),
		"uid":         uid,
		"quantity":    strconv.FormatInt(int64(rand.Intn(10)), 10),
		"date":        strconv.FormatInt(rand.Int63n(1000000), 10),
	}
	shardId := niid % numShardsR
	return 9, params, int(shardId)

}
func genGetbidsbyitem() (int, map[string]string, int) {
	niid := rand.Int63n(numItemRStatic)
	iid := strconv.FormatInt(niid, 10)
	params := map[string]string{
		"iid": iid,
	}
	shardId := rand.Intn(numShardsR)
	return 10, params, shardId
}
func genGetbidsbyuser() (int, map[string]string, int) {
	nuidt := rand.Int63n(numUserRStatic)
	uidt := strconv.FormatInt(nuidt, 10)
	params := map[string]string{
		"uid": uidt,
	}
	shardId := rand.Intn(numShardsR)
	return 11, params, shardId
}

var buyList []chainManager.BuyNowKey
var buyNowLock sync.Mutex

func genBuynow() (int, map[string]string, int) {
	niid := rand.Int63n(numItemRStatic)
	iid := strconv.FormatInt(niid, 10)
	nid := atomic.AddInt64(&uniqueIdR, 1)
	id := strconv.FormatInt(nid, 10)

	params := map[string]string{
		"bnid":     id,
		"iid":      iid,
		"quantity": strconv.FormatInt(int64(rand.Intn(10)), 10),
		"date":     strconv.FormatInt(rand.Int63n(1000000), 10),
	}

	buyNowLock.Lock()
	buyNow := chainManager.BuyNowKey{chainManager.UUID(nid), chainManager.UUID(niid)}
	buyList = append(buyList, buyNow)
	buyNowLock.Unlock()

	shardId := niid % numShardsR
	return 12, params, int(shardId)
}

func genCancelBuy() (int, map[string]string, int) {
	buyNowLock.Lock()
	if len(buyList) == 0 {
		buyNowLock.Unlock()
		return genBuynow()
	}

	idx := rand.Intn(len(buyList))
	buyNow := buyList[idx]
	buyList = append(buyList[:idx], buyList[idx+1:]...)

	id := strconv.FormatInt(int64(buyNow.Id), 10)
	nid := int64(buyNow.Id)
	iid := strconv.FormatInt(int64(buyNow.Item_id), 10)
	buyNowLock.Unlock()

	params := map[string]string{
		"bnid":     id,
		"iid":      iid,
		"quantity": strconv.FormatInt(int64(rand.Intn(10)), 10),
	}
	shardId := nid % numShardsR
	return 13, params, int(shardId)
}
