package main

type T interface{}

func Contains(s []T, x T) bool {
	for _, e := range s {
		if e.Equal(x) {
			return true
		}
	}
	return false
}
