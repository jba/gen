// TODO:
/*
checkBinding: make sure we handle embedded iface:
type Equaler interface { Equal(T) bool }

type T interface{
	Equaler
	Less(T) bool
}
*/

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

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
		{"map[[]int]bool", types.NewMap(types.NewSlice(intType), boolType)}, // no error on non-comparable key
		{"struct{x int}", types.NewStruct([]*types.Var{types.NewField(token.NoPos, nil, "x", intType, false)}, nil)},
		{"map[int]struct{}", types.NewMap(intType, types.NewStruct(nil, nil))},
	} {
		got, err := buildType(test.in)
		if err != nil && test.want != nil {
			t.Errorf(`%s: got "%v", want %s`, test.in, err, test.want)
		} else if err == nil && test.want == nil {
			t.Errorf("%s: got no error, wanted one", test.in)
		} else if err == nil && !types.Identical(got, test.want) {
			t.Errorf("%s: got %s, want %s", test.in, got, test.want)
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

func TestCheckBinding(t *testing.T) {
	for _, test := range []struct {
		wantVal   interface{}
		ptypeDecl string
		atypeExpr string
	}{
		{nil, "type T interface{}", "int"},
		{nil, "type T interface{}", "time.Time"},
		{nil, "type T interface { Equal(T) bool }", "int"}, // by augmentation
		{nil, "type T interface { gen.Comparable }", "int"},
		{missingMethodError(""), "type T interface { M() }", "int"},
	} {
		code := `package p; import "github.com/jba/gen";` + test.ptypeDecl
		pkg := packageFromSource(code)
		ptype := topLevelType("T", pkg)
		atype, err := buildType(test.atypeExpr)
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

func topLevelType(name string, pkg *Package) types.Type {
	tn, err := pkg.topLevelTypeName(name)
	if err != nil {
		log.Fatal(err)
	}
	return tn.Type()
}

func TestReplaceCode(t *testing.T) {
	code := `
	package p
	type T interface {
		Equal(T) bool
		Less(T) bool
		Greater(T) bool
		GreaterEqual(T) bool
		LessEqual(T) bool
	}
	func f(x, y T) bool {
		if x.Equal(y) {
			return x.Greater(y)
		}
		for x.GreaterEqual(y) {
			switch {
			case x.LessEqual(y): return true
			default: return x.Less(y)
			}
		}
		return false
	}
	func g(x T) {
		m1 := x.Less
		m2 := T.Greater
		_, _ = m1, m2
	}
`

	wantf := `func f(x, y T) bool {
	if x == y {
		return x > y
	}
	for x >= y {
		switch {
		case x <= y:
			return true
		default:
			return x < y
		}
	}
	return false
}`

	wantg := `func g(x T) {
	m1 := func(a T) func(T) bool {
		return func(b T) bool {
			return a < b
		}
	}(x)
	m2 := func(a, b T) bool {
		return a > b
	}
	_, _ = m1, m2
}`
	pkg := packageFromSource(code)
	var file *ast.File
	for _, f := range pkg.apkg.Files {
		file = f
		break
	}
	ptype := topLevelType("T", pkg)
	var rewrites []rewrite
	for _, am := range append([]augmentMethod{equalMethod}, orderedMethods...) {
		rewrites = append(rewrites, rewrite{ptype, am.name, am.tok})
	}
	if err := replaceCode(file, rewrites, pkg.info); err != nil {
		t.Fatal(err)
	}

	compare := func(funcName, want string) {
		fdecl := file.Scope.Lookup(funcName).Decl
		var buf bytes.Buffer
		if err := printer.Fprint(&buf, pkg.fset, fdecl); err != nil {
			t.Fatalf("%s: %v", funcName, err)
		}
		got := buf.String()
		if got != want {
			t.Errorf("%s:\n-- got --\n%s\n-- want --\n%s", funcName, got, want)
		}
	}

	compare("f", wantf)
	compare("g", wantg)
}

func TestExamples(t *testing.T) {
	log.SetOutput(ioutil.Discard)
	for _, test := range []struct {
		dir    string
		outPkg string
		specs  []string
	}{
		{"stack", "intstack", []string{"T:int"}},
		{"slices", "strslices", []string{"T:string"}},
		{"slices", "timeslices", []string{"T:time.Time"}},
		{"maps", "pointsets", []string{"K:github.com/jba/gen/examples/geo.Point", "V:bool"}},
	} {
		t.Run(test.outPkg, func(t *testing.T) {
			err := run(filepath.Join("github.com/jba/gen/examples", test.dir), "/tmp", test.outPkg, test.specs)
			if err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command("diff", "-u",
				filepath.Join("../testdata/want", test.outPkg), filepath.Join("/tmp", test.outPkg))
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Log(string(out))
				t.Fatal(err)
			}
			if len(out) > 0 {
				t.Error(string(out))
			}
		})
	}
}

func Test_TypesInfo(t *testing.T) {
	t.SkipNow()
	// Learn about how the go/types package works.
	const src = `
		package p
		type T interface{}
	    type U T
        type V U
`
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "<src>", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	info := &types.Info{
		Defs: make(map[*ast.Ident]types.Object),
	}
	_, err = typecheck("path", fset, []*ast.File{file}, info)
	for id, obj := range info.Defs {
		fmt.Printf("%s => %v:\n", id.Name, obj)
		if obj != nil {
			dumpType(obj.Type(), 0)
		}
	}
}

func typecheckSource(src string) error {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "<src>", src, 0)
	if err != nil {
		panic(fmt.Sprintf("could not parse (%v)\nsource code:\n%s", err, src))
	}
	_, err = typecheck("path", fset, []*ast.File{file}, nil)
	return err
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
	fset := token.NewFileSet()
	apkg, err := astPackage(fset, "<src>", src)
	if err != nil {
		panic(err)
	}
	pkg, err := makePackage(fset, "<path>", apkg)
	if err != nil {
		panic(err)
	}
	return pkg
}

// func loadPackageFromFile(ipath, filename string) (*Package, error) {
// 	fset := token.NewFileSet()
// 	apkg, err := astPackage(fset, filename, nil)
// 	if err != nil {
// 		return nil, err
// 	}
// 	return makePackage(fset, ipath, apkg)
// }

func astPackage(fset *token.FileSet, filename string, src interface{}) (*ast.Package, error) {
	file, err := parser.ParseFile(fset, filename, src, 0)
	if err != nil {
		return nil, err
	}
	return &ast.Package{
		Name:  file.Name.Name,
		Files: map[string]*ast.File{filename: file},
	}, nil
}
