package set

type T interface {
	Less(T) bool
}

type Set []T

func equal(a, b T) bool {
	return !a.Less(b) && !b.Less(a)
}

func MakePtr() *T {
	return new(T)
}

func (s Set) Contains(x T) bool {
	for _, e := range s {
		if !e.Less(x) {
			return !x.Less(e)
		}
	}
	return false
}

func foo(arg T) bool {
	return invoke(T.Less, arg)
}

func bar(arg T) bool {
	f := arg.Less
	return f(arg)
}

func invoke(f func(T, T) bool, arg T) bool {
	return f(arg, arg)
}
