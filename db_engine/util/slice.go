package util

type numbers interface {
	int32 | uint64
}

func Contains[T numbers](slice []T, s T) int {
	for i, v := range slice {
		if v == s {
			return i
		}
	}
	return -1
}

func RemoveElement[T comparable](slice []T, s int) []T {
	if s == -1 {
		return slice
	}
	return append(slice[:s], slice[s+1:]...)
}

func DeduplicateSlice[T numbers](slice []T) []T {
	seen := make(map[T]bool)
	result := []T{}

	for _, val := range slice {
		if _, ok := seen[val]; !ok {
			seen[val] = true
			result = append(result, val)
		}
	}
	return result
}
