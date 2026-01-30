package main

import (
	"ShardDB/chainManager"
	"ShardDB/server"
	"math/rand"
	"strconv"
	"sync/atomic"
)

const numShardsS = 40
const numUserSStatic = 10 * numShardsS

var uniqueIdS int64 = 0

type genRequestFuncS func() (int, map[string]string, int)

func main() {
	//rand.Seed(time.Now().UnixNano())
	rand.Seed(2068)

	client := server.SetupClient(len(chainManager.GetChains(nil)))
	client.ExpTime = 10
	client.WarmUpTime = 0
	client.PrintChain = make([]string, len(chainManager.GetChains(nil)))
	client.PrintChain[0] = "Befriend"
	client.PrintChain[1] = "Unfriend"
	client.PrintChain[2] = "ReadFriends"
	client.PrintChain[3] = "ReadActivities"
	client.PrintChain[4] = "PostStatus"
	client.PrintChain[5] = "PostWall"

	readFuncs := make([]genRequestFuncS, 2)
	readFuncs[0] = genReadFriends
	readFuncs[1] = genReadActivities

	client.GenRequest = func(threadId int, count int, isReady bool) (int, map[string]string, int) {
		txnType := rand.Intn(100)

		if txnType < 25 {
			return genBeFriend()
		} else if txnType < 50 {
			return genUnFriend()
		} else if txnType < 74 {
			return genPostStatus()
		} else if txnType < 98 {
			return genPostWall()
		} else {
			return readFuncs[rand.Intn(len(readFuncs))]()
		}
	}

	client.StartExperiment()
}

func genBeFriend() (int, map[string]string, int) {
	uid1 := rand.Int63n(numUserSStatic)
	uid2 := rand.Int63n(numUserSStatic)
	for uid1 == uid2 {
		uid2 = rand.Int63n(numUserSStatic)
	}

	aid1 := atomic.AddInt64(&uniqueIdS, 1)
	aid2 := atomic.AddInt64(&uniqueIdS, 1)
	params := map[string]string{
		"uid1": strconv.FormatInt(uid1, 10),
		"uid2": strconv.FormatInt(uid2, 10),
		"aid1": strconv.FormatInt(aid1, 10),
		"aid2": strconv.FormatInt(aid2, 10),
		"time": strconv.FormatInt(rand.Int63n(1000000), 10),
	}

	shardId := uid1 % numShardsS
	return 0, params, int(shardId)
}

func genUnFriend() (int, map[string]string, int) {
	uid1 := rand.Int63n(numUserSStatic)
	uid2 := rand.Int63n(numUserSStatic)
	for uid1 == uid2 {
		uid2 = rand.Int63n(numUserSStatic)
	}

	aid1 := atomic.AddInt64(&uniqueIdS, 1)
	aid2 := atomic.AddInt64(&uniqueIdS, 1)
	params := map[string]string{
		"uid1": strconv.FormatInt(uid1, 10),
		"uid2": strconv.FormatInt(uid2, 10),
		"aid1": strconv.FormatInt(aid1, 10),
		"aid2": strconv.FormatInt(aid2, 10),
		"time": strconv.FormatInt(rand.Int63n(1000000), 10),
	}

	shardId := uid1 % numShardsS
	return 1, params, int(shardId)
}

func genReadFriends() (int, map[string]string, int) {
	uid := rand.Int63n(numUserSStatic)

	params := map[string]string{
		"uid": strconv.FormatInt(uid, 10),
	}

	shardId := uid % numShardsS
	return 2, params, int(shardId)
}

func genReadActivities() (int, map[string]string, int) {
	uid := rand.Int63n(numUserSStatic)

	params := map[string]string{
		"uid": strconv.FormatInt(uid, 10),
	}

	shardId := uid % numShardsS
	return 3, params, int(shardId)
}

func genPostStatus() (int, map[string]string, int) {
	uid := rand.Int63n(numUserSStatic)

	sid := atomic.AddInt64(&uniqueIdS, 1)
	aid := atomic.AddInt64(&uniqueIdS, 1)

	params := map[string]string{
		"uid":   strconv.FormatInt(uid, 10),
		"sid":   strconv.FormatInt(sid, 10),
		"aid":   strconv.FormatInt(aid, 10),
		"staus": "New Status" + strconv.FormatInt(sid, 10),
		"time":  strconv.FormatInt(rand.Int63n(1000000), 10),
	}

	shardId := uid % numShardsS
	return 4, params, int(shardId)
}

func genPostWall() (int, map[string]string, int) {
	uid := rand.Int63n(numUserSStatic)

	wid := atomic.AddInt64(&uniqueIdS, 1)
	aid := atomic.AddInt64(&uniqueIdS, 1)

	params := map[string]string{
		"uid":     strconv.FormatInt(uid, 10),
		"wid":     strconv.FormatInt(wid, 10),
		"aid":     strconv.FormatInt(aid, 10),
		"message": "New Message" + strconv.FormatInt(wid, 10),
		"time":    strconv.FormatInt(rand.Int63n(1000000), 10),
	}

	shardId := uid % numShardsS
	return 5, params, int(shardId)
}
