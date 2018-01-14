package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"
)

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

func TestCheckParamErrors(t *testing.T) {
	// Test the additional checks we perform on generic parameter types. The go/types checker
	// handles the usual interface implementation checks.
	for _, test := range []struct {
		wantVal interface{}
		code    string
	}{
		{notInterfaceError(""), "type T int"},
		{needsComparableError(""), "type T interface{}; var m map[T]bool"},
		{needsComparableError(""), "type T interface{}; var m map[[1]T]bool"},
		{needsComparableError(""), "type T interface{}; type S struct { x T }; var m map[S]bool"},
		{needsComparableError(""), `
			type T interface{};
			func m() bool {
				var t1, t2 T
				return t1 == t2
			}`},
		{needsComparableError(""), `
			type T interface{};
			func m() bool {
				var t1, t2 T
				return (t1 == t2) != (1 == 2)
			}`},
		{needsComparableError(""), `
			import "fmt"
			type T interface{}
			func m() {	var m map[interface{}]bool
				var t T
				fmt.Println(m[t])
			}`},
		{needsNillableError(""), "type T interface{}; func m(T) { m(nil) }"},
		{needsNillableError(""), "type T interface{}; func m() T { return nil }"},
		{needsNillableError(""), "type T interface{}; func m(t T) bool { return t == nil  }"},
		{needsNillableError(""), "type T interface{}; func m()  { var t T = nil; _ = t }"},
		{needsNillableError(""), "type T interface{}; func m(t T)  { x := (nil == t); _ = x }"},
		{needsNillableError(""), "type T interface{}; type U T; func m()  { var u U = nil; _ = u }"},
	} {
		pkg := packageFromSource("package p; " + test.code)
		gtn, err := pkg.topLevelTypeName("T")
		if err != nil {
			t.Fatal(err)
		}
		if err := checkParam(gtn, pkg); err == nil {
			t.Errorf("%s: wanted error, got nil", test.code)
		} else if got, want := reflect.TypeOf(err), reflect.TypeOf(test.wantVal); got != want {
			t.Errorf("got error type %s, want %s", got, want)
		}
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
