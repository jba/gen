package hashtable

type (
	K interface {
		Hash() uint64
		Equal(K) bool
	}

	V interface{}
)

func (t *Hashtable) Get(key K) V {
	b, i := t.lookup(key)
	if i >= 0 {
		return t.buckets[b][i].Value
	}
	return nil
}

func (t *Hashtable) lookup(k K) (int, int) {
	return 0, 0
}

type Item struct {
	Key   K
	Value V
}

type Hashtable struct {
	buckets [][]*Item
}
