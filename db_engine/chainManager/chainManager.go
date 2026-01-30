package chainManager

import (
	"hash/fnv"
)

type ChainManager struct {
	chains map[string]*Chain
}

func hashStr(str string) uint64 {
	h := fnv.New64a()
	h.Write([]byte(str))
	return h.Sum64()
}
