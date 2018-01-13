package stack

type T interface{}

type Stack struct {
	items []T
}

func (s *Stack) Push(x T) {
	s.items = append(s.items, x)
}

func (s *Stack) Pop() T {
	last := len(s.items) - 1
	x := s.items[last]
	s.items = s.items[:last]
	return x
}

func (s *Stack) Len() int {
	return len(s.items)
}

func (s *Stack) Empty() bool {
	return len(s.items) == 0
}
