package floatranges

type T = float64

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
	{
		a := a

		return a + b
	}

}

func less(a, b T) bool {
	{
		a := a

		return a < b
	}

}
