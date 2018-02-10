package gen

import (
	"go/token"
	"go/types"
	"reflect"
	"testing"
)

func TestCheckBinding(t *testing.T) {
	for _, test := range []struct {
		wantVal   interface{}
		ptypeDecl string
		atypeExpr string
	}{
		{nil, "type T interface{}", "int"},
		{nil, "type T interface{}", "time.Time"},
		{nil, "type T interface { Equal(T) bool }", "int"}, // by augmentation
		{nil, "type T interface { generic.Comparable }", "int"},
		{missingMethodError(""), "type T interface { M() }", "int"},
		// TODO: test nillable
	} {
		code := `package p; import "github.com/jba/gen/generic";` + test.ptypeDecl
		pkg := packageFromSource(code)
		ptype := topLevelType("T", pkg)
		atype, err := buildType(test.atypeExpr, lookupBuiltinName)
		if err != nil {
			t.Fatal(err)
		}
		err = checkBinding(ptype, atype)
		if test.wantVal == nil && err != nil {
			t.Errorf(`%s, %s: wanted nil, got "%v" (%T)`, test.ptypeDecl, test.atypeExpr, err, err)
		} else if got, want := reflect.TypeOf(err), reflect.TypeOf(test.wantVal); got != want {
			t.Errorf("%s, %s: got error type %s, want %s", test.ptypeDecl, test.atypeExpr, got, want)
		}
	}
}

func TestBuildType(t *testing.T) {
	intType := types.Typ[types.Int]
	boolType := types.Typ[types.Bool]
	lookup := func(path, name string) types.Type {
		typ, err := lookupNamedType(path, name)
		if err != nil {
			t.Fatal(err)
		}
		return typ
	}

	for _, test := range []struct {
		in   string
		want types.Type // nil means error
	}{
		{"", nil},
		{"bork", nil},
		{"map[int", nil},
		{"[slice]", nil},
		{"int", intType},
		{"[]int", types.NewSlice(intType)},
		{"[5]int", types.NewArray(intType, 5)},
		{"map[int]bool", types.NewMap(intType, boolType)},
		{"time.Time", lookup("time", "Time")},
		{"map[time.Time][]int", types.NewMap(lookup("time", "Time"), types.NewSlice(intType))},
		{`"github.com/jba/gen/examples/geo".Point`, lookup("github.com/jba/gen/examples/geo", "Point")},
		{"map[[]int]bool", nil}, // error on non-comparable key
		{"struct{x int}", types.NewStruct([]*types.Var{types.NewField(token.NoPos, nil, "x", intType, false)}, nil)},
		{"map[int]struct{}", types.NewMap(intType, types.NewStruct(nil, nil))},
	} {
		got, err := buildType(test.in, lookupBuiltinName)
		if err != nil && test.want != nil {
			t.Errorf(`%s: got "%v", want %s`, test.in, err, test.want)
		} else if err == nil && test.want == nil {
			t.Errorf("%s: got no error, wanted one", test.in)
		} else if err == nil && !types.Identical(got, test.want) {
			t.Errorf("%s: got %s, want %s", test.in, got, test.want)
		}
	}
}
