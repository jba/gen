package nested

import (
	"fmt"
	"math"

	/*
	 gen:import boolmaps "github.com/jba/gen/examples/maps" K:T V:bool
	*/
	"github.com/jba/gen/examples/maps"
	"golang.org/x/net/context"
)

type T interface{}

type Set map[T]bool

func (s Set) Clone() Set {
	return Set(maps.Clone(s))
}

func f() {
	fmt.Println()
	math.Sqrt(3)
	c := context.Context()
}

// func (s1 Set) Union(s2 Set) Set {
// 	return Set(maps.Union(maps.Clone(s1), s2))
// }
