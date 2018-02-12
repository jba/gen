package maps

type (
	K []int
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
