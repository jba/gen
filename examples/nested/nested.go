package nested

import (
	"fmt"
	"math"

	"github.com/jba/gen/tmp.gitignore/boolmaps"

	// gen:import boolmaps "github.com/jba/gen/examples/maps" K:T V:bool

	"golang.org/x/net/context"
)

type T interface{}

type Set map[T]bool

func (s Set) Clone() Set {
	return Set(boolmaps.Clone(map[T]bool(s)))
}

func f() {
	fmt.Println()
	math.Sqrt(3)
	c := context.Context()
}
