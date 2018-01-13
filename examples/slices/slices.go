package slices

type T interface {
	Equal(T) bool
}

type U interface{}

func Index(s []T, v T) int {
	for i, e := range s {
		if e.Equal(v) {
			return i
		}
	}
	return -1
}

func Map(s []T, f func(T) U) []U {
	us := make([]U, len(s))
	for i, e := range s {
		us[i] = f(e)
	}
	return us
}
