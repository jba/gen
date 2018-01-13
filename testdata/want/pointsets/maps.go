package pointsets

import (
	"github.com/jba/gen/examples/geo"
)

type (
	K = geo.Point

	V = bool
)

func Clone(m map[K]V) map[K]V {
	if m == nil {
		return nil
	}
	r := map[K]V{}
	for k, v := range m {
		r[k] = v
	}
	return r
}

func Keys(m map[K]V) []K {
	if m == nil {
		return nil
	}
	ks := make([]K, 0, len(m))
	for k := range m {
		ks = append(ks, k)
	}
	return ks
}

func Union(dest, src map[K]V) map[K]V {
	for k, v := range src {
		dest[k] = v
	}
	return dest
}

func Difference(dest, src map[K]V) map[K]V {
	for k := range src {
		delete(dest, k)
	}
	return dest
}

func Intersection(dest, src map[K]V) map[K]V {
	for k := range dest {
		if _, ok := src[k]; !ok {
			delete(dest, k)
		}
	}
	return dest
}
