package ex5

func add1(x interface{}) interface{} {
	switch x := x.(type) {
	case int:
		return x + 1
	case float64:
		return x + 1
	default:
		panic("bad arg")
	}
}
