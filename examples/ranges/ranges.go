package ranges

type T interface{}

type ordnum interface {
	Less(T) bool
	Plus(T) T
}

type Range struct {
	low, high, delta T
}

func (r *Range) Do(f func(T) bool) {
	for i := r.low; less(i, r.high); i = plus(i, r.delta) {
		if !f(i) {
			break
		}
	}
}

func plus(a, b T) T {
	switch a := a.(type) {
	case int:
		return a + b.(int)
	case float64:
		return a + b.(float64)
	case ordnum:
		return a.Plus(b)
	default:
		panic("not an int, float64 or does not implement Plus")
	}
}

func less(a, b T) bool {
	switch a := a.(type) {
	case int:
		return a < b.(int)
	case float64:
		return a < b.(float64)
	case ordnum:
		return a.Less(b)
	default:
		panic("bad T")
	}
}
