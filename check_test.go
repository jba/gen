package gen

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"os"
	"reflect"
	"strings"
	"testing"
)

func TestDirToImportPath(t *testing.T) {
	got := dirToImportPath(os.Getenv("HOME") + "/go/src/github.com/jba")
	want := "github.com/jba"
	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}

func TestComparableMod(t *testing.T) {
	empty := types.NewInterface(nil, nil).Complete()
	if !empty.Empty() {
		t.Fatal("not empty")
	}
	T := newNamedType("T", empty)
	for _, test := range []struct {
		in   types.Type
		want bool
	}{
		{T, false},
		{types.NewPointer(T), true},
		// {newNamedType("U", T), false}, fails because types doesn't remember "type U T"
		{types.NewArray(T, 1), false},
		{types.NewSlice(T), false},
	} {
		got := comparableMod(test.in, T)
		if got != test.want {
			t.Errorf("%v: got %t, want %t", test.in, got, test.want)
		}
	}
}

func TestCheckParamErrors(t *testing.T) {
	// Test the additional checks we perform on generic parameter types. The go/types checker
	// handles the usual interface implementation checks.
	for _, test := range []struct {
		wantVal interface{}
		code    string
	}{
		{notInterfaceError(""), "type T int"},
		{needsComparableError(""), "var m map[T]bool"},
		{needsComparableError(""), "var m map[[1]T]bool"},
		{needsComparableError(""), "type S struct { x T }; var m map[S]bool"},
		{needsComparableError(""), "func m() bool { var t1, t2 T;return t1 == t2 }"},
		{needsComparableError(""), `
			func m() bool {
				var t1, t2 T
				return (t1 == t2) != (1 == 2)
			}`},
		{needsComparableError(""), `
			func m() {
                var m map[interface{}]bool
				var t T
				_ = m[t]
			}`},
		{needsComparableError(""), "func m(t, u T) { switch t {case u:}}"},
		{needsNillableError(""), "func m(T) { m(nil) }"},
		{needsNillableError(""), "func m() T { return nil }"},
		{needsNillableError(""), "func m(t T) bool { return t == nil  }"},
		{needsNillableError(""), "func m()  { var t T; t = nil; _ = t }"},
		{needsNillableError(""), "func m()  { var t T = nil; _ = t }"},
		{needsNillableError(""), "func m(t T)  { x := (nil == t); _ = x }"},
		{needsNillableError(""), "func m(t1, t2 T) bool { return (1 == 2) != (t1 == nil) }"},
		// TODO: handle "type U T"
		//{needsNillableError(""), "type U T; func m()  { var u U = nil; _ = u }"},
		{needsNillableError(""), "func m(t, u T) { switch t {case nil:}}"},
		{needsNillableError(""), "func m() { c := make(chan T); c <- nil }"},
		{needsNillableError(""), "func m() { func(t T) {}(nil) }"},
	} {
		code := test.code
		if !strings.HasPrefix(code, "type T") {
			code = "type T interface{}; " + code
		}
		code = "package p; " + code
		pkg := packageFromSource(code)
		gtn, err := pkg.topLevelTypeName("T")
		if err != nil {
			t.Fatal(err)
		}
		if err := checkParam(gtn, pkg); err == nil {
			t.Errorf("%s: wanted error, got nil", test.code)
		} else if got, want := reflect.TypeOf(err), reflect.TypeOf(test.wantVal); got != want {
			t.Errorf("%s: got error type %s, want %s", test.code, got, want)
		}
	}
}

func TestParamTypeFromAST(t *testing.T) {
	src := `
package p

type T1 interface{}
type T2 interface { Less(T2) bool }
`
	apkg := astPackageFromSource(src)
	got, err := paramTypeFromAST("T1", apkg)
	if err != nil {
		t.Fatal(err)
	}
	empty := types.NewInterface(nil, nil).Complete()
	if !empty.Empty() {
		t.Fatal("not empty")
	}
	want := newNamedType("T1", empty)
	if !types.Identical(got.Underlying(), want.Underlying()) {
		t.Errorf("got %v, want %v", got.Underlying(), want.Underlying())
	}

	tn := types.NewTypeName(token.NoPos, nil, "T2", nil)
	nt := types.NewNamed(tn, nil, nil)
	sig := types.NewSignature(
		nil,
		types.NewTuple(types.NewParam(token.NoPos, nil, "", nt)),
		types.NewTuple(types.NewParam(token.NoPos, nil, "", types.Typ[types.Bool])),
		false)
	ms := []*types.Func{types.NewFunc(token.NoPos, nil, "Less", sig)}
	want = types.NewInterface(ms, nil).Complete()
	got, err = paramTypeFromAST("T2", apkg)
	if err != nil {
		t.Fatal(err)
	}
	gots := fmt.Sprint(got.Underlying())
	wants := fmt.Sprint(want)
	if gots != wants {
		t.Errorf("got %v, want %v", gots, wants)
	}
}

// Creates the same type you get from "type <name> <t>"
func newNamedType(name string, t types.Type) types.Type {
	tn := types.NewTypeName(token.NoPos, nil, name, nil)
	// NewNamed will set tn's type.
	nt := types.NewNamed(tn, t.Underlying(), nil)
	if tn.Type() != nt {
		panic("misconstructed name type")
	}
	return nt
}

func packageFromSource(src string) *Package {
	apkg := astPackageFromSource(src)
	pkg, err := makePackage("<path>", apkg, theImporter)
	if err != nil {
		panic(fmt.Sprintf("%s: %v", src, err))
	}
	return pkg
}

func astPackageFromSource(src interface{}) *astPackage {
	const filename = "<src>"
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, filename, src, parser.ParseComments)
	if err != nil {
		panic(err)
	}
	return &astPackage{
		pkg: &ast.Package{
			Name:  file.Name.Name,
			Files: map[string]*ast.File{filename: file},
		},
		fset: fset,
	}
}
