package intss

type T = int

type Set []T

func equal(a, b T) bool {
	return !(a < b) && !(b < a)
}

func MakePtr() *T {
	return new(T)
}

func (s Set) Contains(x T) bool {
	for _, e := range s {
		if !(e < x) {
			return !(x < e)
		}
	}
	return false
}

func foo(arg T) bool {
	return invoke(func(a, b T) bool {
		return a < b
	}, arg)
}

func bar(arg T) bool {
	f := func(a T) func(T) bool {
		return func(b T) bool {
			return a < b
		}
	}(arg)
	return f(arg)
}

func invoke(f func(T, T) bool, arg T) bool {
	return f(arg, arg)
}
