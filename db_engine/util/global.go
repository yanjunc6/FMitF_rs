package util

const SchedulerMode = 0
const LockMode = 1
const IC3Mode = 2
const DistributedIC3Mode = 3

var IsPreVerification = false

const NormalHop = 0
const MergedHop = 1
const MergedHopBegin = 2
const MergedHopEnd = 3
const GlobalHop = 4

const MaxHop = int32(1024)

var ConcurrentMode = LockMode
var IsWoundWaitLock = true

func SetConcurrentMode(mode int) {
	switch mode {
	case DistributedIC3Mode:
		ConcurrentMode = DistributedIC3Mode
		IsWoundWaitLock = false
	case 4:
		// IC3 + Ordered2PL
		ConcurrentMode = IC3Mode
		IsWoundWaitLock = false
	case 5:
		// IC3 + MessagePulling, meaning DIC3 - Ordered2PL
		ConcurrentMode = DistributedIC3Mode
		IsWoundWaitLock = true
	default:
		ConcurrentMode = mode
	}
}

var Verbose = false
var NumShards = 0

func TxId(id uint64, chainId int) uint64 {
	highValue := uint64(chainId) << 56
	return highValue | id
}

func GetChainId(tx uint64) int32 {
	return int32(tx >> 56)
}
