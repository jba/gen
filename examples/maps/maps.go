package maps

import "github.com/jba/gen/generic"

type (
	K interface {
		generic.Comparable
	}
	V interface{}
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

func Clear(m map[K]V) {
	for k := range m {
		delete(m, k)
	}
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

// Union adds all the entries from src to dest and returns dest. If a key is in dest,
// its value is overwritten with the value from src.
func Union(dest, src map[K]V) map[K]V {
	for k, v := range src {
		dest[k] = v
	}
	return dest
}

// Difference removes all of src's keys from dest and returns dest.
func Difference(dest, src map[K]V) map[K]V {
	for k := range src {
		delete(dest, k)
	}
	return dest
}

// Intersection keeps the keys in dest that are also in src, removing the others.
// It returns dest.
func Intersection(dest, src map[K]V) map[K]V {
	for k := range dest {
		if _, ok := src[k]; !ok {
			delete(dest, k)
		}
	}
	return dest
}
