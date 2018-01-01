package set

import (
	"testing"
)

type Int int

func (a Int) Less(b T) bool {
	return a < b.(Int)
}

func TestContains(t *testing.T) {
	s := Set([]T{Int(3), Int(11)})
	if !s.Contains(Int(3)) {
		t.Fail()
	}
	if s.Contains(Int(5)) {
		t.Fail()
	}
}
