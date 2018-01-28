package ranges

import (
	"math/big"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestDo(t *testing.T) {
	r := &Range{1, 6, 2}
	got := collect(r)
	want := []T{1, 3, 5}
	if !cmp.Equal(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}

	r = &Range{2.0, 4.0, .5}
	got = collect(r)
	want = []T{2.0, 2.5, 3.0, 3.5}
	if !cmp.Equal(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}

	r = &Range{newInt(10), newInt(12), newInt(1)}
	got = collect(r)
	want = []T{newInt(10), newInt(11)}
	if !cmp.Equal(got, want) {
		t.Errorf("got %v, want %v", got, want)
	}
}

type Int struct {
	i *big.Int
}

func newInt(i int) Int {
	return Int{big.NewInt(int64(i))}
}

func (a Int) Less(b T) bool {
	return a.i.Cmp(b.(Int).i) < 0
}

func (a Int) Equal(b Int) bool {
	return a.i.Cmp(b.i) == 0
}

func (a Int) Plus(b T) T {
	var c big.Int
	return Int{c.Add(a.i, b.(Int).i)}
}

func collect(r *Range) []T {
	var got []T
	r.Do(func(x T) bool {
		got = append(got, x)
		return true
	})
	return got
}
