package ex4

type T interface{}

func plus(a, b T) T {
	switch a := a.(type) {
	case int:
		return a + b.(int)
	case float64:
		return a + b.(float64)
	case interface{ Plus(T) T }:
		return a.Plus(b)
	default:
		panic("not an int, float64 or does not implement Plus")
	}
}
