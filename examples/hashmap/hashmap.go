package hashmap

type (
	K interface {
		Hash() uint64
		Equal(K) bool
	}

	V = interface{}
)

type Item struct {
	Key   K
	Value V
}

type Map struct {
	buckets [][]*Item
}

func New() *Map {
	return &Map{buckets: make([][]*Item, 10)}
}

func (m *Map) Set(key K, val V) {
	b := m.hash(key)
	i := lookup(key, m.buckets[b])
	if i >= 0 {
		m.buckets[b][i].Value = val
	} else {
		m.buckets[b] = append(m.buckets[b], &Item{Key: key, Value: val})
	}
}

func (m *Map) Get(key K) (V, bool) {
	b := m.hash(key)
	i := lookup(key, m.buckets[b])
	if i >= 0 {
		return m.buckets[b][i].Value, true
	}
	var v V
	return v, false
}

func (m *Map) Delete(key K) bool {
	b := m.hash(key)
	items := m.buckets[b]
	i := lookup(key, items)
	if i < 0 {
		return false
	} else if len(items) == 1 {
		m.buckets[b] = nil
	} else {
		last := len(items) - 1
		items[i] = items[last]
		items[last] = nil
		m.buckets[b] = items[:last]
	}
	return true
}

func (m *Map) hash(key K) uint64 {
	return key.Hash() % uint64(len(m.buckets))
}

func lookup(key K, items []*Item) int {
	for i, item := range items {
		if item.Key.Equal(key) {
			return i
		}
	}
	return -1
}
