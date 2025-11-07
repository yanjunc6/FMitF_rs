package chainManager
import (
	"ShardDB/proto"
	"ShardDB/util"
	"github.com/boltdb/bolt"
	"encoding/json"
	"sync"
)

const TPCCNewOrderChainId = 0

type NextReqFunc func(in *proto.TrxRes, params map[string]string, shards int) (*proto.TrxReq, map[string]string, int)
type Chain struct {
	db   *bolt.DB
	hops []*Hop

	NextReq NextReqFunc
}


var LockSchedulers map[string]*Scheduler
var AccessSchedulers map[string]*Scheduler
var AccessDSchedulers map[string]*Scheduler


type ChainManager struct {
	chains map[string]*Chain
}

type ProcessFunc func(tx *bolt.Tx, in *proto.TrxReq) (*proto.TrxRes, error)

type Hop struct {
	id          int32
	isReadOnly  bool
	isRecordDep bool
	hopType     int

	waitHop map[int32]int32

	process ProcessFunc
	accessD []*Scheduler
}

type Scheduler struct {
	Name     string
	schedKey []string
	endKey   []string
	endHopId int32
	maps     *sync.Map
}
func NewScheduler() *Scheduler {
	return &Scheduler{maps: &sync.Map{}}
}


func AddRWSet(rwSet []*proto.RWSet, tableName string, key []byte) []*proto.RWSet {
	return append(rwSet, &proto.RWSet{TableName: tableName, Key: string(key)})
}

func putData(tx *bolt.Bucket, key []byte, value any) {
	valueBytes, _ := json.Marshal(value)
	tx.Put(key, valueBytes)
}

const NormalHop = 0
const MergedHop = 1
const MergedHopBegin = 2
const MergedHopEnd = 3