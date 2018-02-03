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
	"go/format"
	"go/parser"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
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

func TestTypeToExpr(t *testing.T) {
	intType := types.Typ[types.Int]
	boolType := types.Typ[types.Bool]
	tpkg, err := theImporter.Import("github.com/jba/gen")
	if err != nil {
		t.Fatal(err)
	}
	lookup := func(path, name string) types.Type {
		typ, err := lookupNamedType(path, name)
		if err != nil {
			t.Fatal(err)
		}
		return typ
	}

	for _, test := range []struct {
		in   types.Type
		want string
	}{
		{intType, "int"},
		{types.NewSlice(intType), "[]int"},
		{types.NewArray(intType, 5), "[5]int"},
		{types.NewMap(intType, boolType), "map[int]bool"},
		{lookup("time", "Time"), "time.Time"},
		{types.NewMap(lookup("time", "Time"), types.NewSlice(intType)), "map[time.Time][]int"},
		{lookup("github.com/jba/gen/examples/geo", "Point"), "geo.Point"},
		{types.NewStruct([]*types.Var{types.NewField(token.NoPos, nil, "x", intType, false)}, nil), "struct{x int}"},
		{types.NewMap(intType, types.NewStruct(nil, nil)), "map[int]struct{}"},
	} {
		got := types.ExprString(typeToExpr(test.in, tpkg))
		if got != test.want {
			t.Errorf("%s: got %s, want %s", test.in, got, test.want)
		}
	}
}

func TestZeroExpr(t *testing.T) {
	fset := token.NewFileSet()
	intType := types.Typ[types.Int]
	boolType := types.Typ[types.Bool]
	timeType, err := lookupNamedType("time", "Time")
	if err != nil {
		t.Fatal(err)
	}
	tpkg, err := theImporter.Import("github.com/jba/gen")
	if err != nil {
		t.Fatal(err)
	}
	for _, test := range []struct {
		in   types.Type
		want string
	}{
		{intType, "0"},
		{boolType, "false"},
		{types.Typ[types.Float32], "0"},
		{types.Typ[types.String], `""`},
		{types.Typ[types.Rune], "0"},
		{timeType, "time.Time{}"},
		{types.NewSlice(intType), "nil"},
		{types.NewArray(intType, 5), "[5]int{}"},
		{types.NewMap(intType, boolType), "nil"},
		{types.NewStruct(nil, nil), "struct{}{}"},
		{types.NewStruct([]*types.Var{types.NewField(token.NoPos, nil, "x", intType, false)}, nil),
			"struct{ x int }{}"},
	} {
		expr := zeroExpr(test.in, tpkg)
		// types.ExprString doesn't handle array and struct literals well.
		got := nodeString(expr, fset)
		got = strings.Replace(got, "\n", "", -1)
		got = strings.Replace(got, "\t", " ", -1)
		if got != test.want {
			t.Errorf("%s: got %q, want %q", test.in, got, test.want)
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
		// TODO: test nillable
	} {
		code := `package p; import "github.com/jba/gen";` + test.ptypeDecl
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

func topLevelType(name string, pkg *Package) types.Type {
	tn, err := pkg.topLevelTypeName(name)
	if err != nil {
		log.Fatal(err)
	}
	return tn.Type()
}

func TestDirToImportPath(t *testing.T) {
	got := dirToImportPath(os.Getenv("HOME") + "/go/src/github.com/jba")
	want := "github.com/jba"
	if got != want {
		t.Errorf("got %s, want %s", got, want)
	}
}

func TestReplaceCode(t *testing.T) {
	for _, test := range []struct {
		in, want string
	}{
		{
			in: `func f(x, y T) bool {
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
			}`,
			want: `func f(x, y T) bool {
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
			}`,
		},
		{
			in: `func f(x T) bool {
					return func() T { if (x.Equal(x)) { return x }; return x }().Greater(x)
		        }`,
			want: `func f(x T) bool {
					return func() T {
							if x == x {
								return x
							 }
							 return x
						}() > x
				  }`,
		},
		{
			in: `func f(x I) bool {
					switch x.(type) {
					case int: return true
					case bool: return false
					default: return false
					}
				}`,
			want: `func f(x I) bool {
				{
					return true
				}
			}`,
		},
		{
			// TODO: fix this; z is unused in the resulting code.
			in: `func f(x I) bool {
					switch z := false; x.(type) {
					case int: return true
					case bool: return z
					default: return false
					}
				}`,
			want: `func f(x I) bool {
				{
					z := false
					return true
				}
			}`,
		},
		{
			in: `func f(x I) bool {
					switch x.(type) {
					case bool: return true
					default: return false
					}
				}`,
			want: `func f(x I) bool {
				{
					return false
				}
			}`,
		},
		{
			in: `func f(x I) bool {
					switch x.(type) {
					case bool: return true
					}
			        return false
				}`,
			want: `func f(x I) bool {
				return false
			}`,
		},
		{
			in: `func f(x I) bool {
					switch y := x.(type) {
					case bool: return y
					}
			        return false
				}`,
			want: `func f(x I) bool {
				return false
			}`,
		},
		{
			in: `func f(x I) bool {
					switch y := x.(type) {
					case bool: return true
					case  int: return y == 3
					}
			        return false
				}`,
			want: `func f(x I) bool {
				{
					y := x
				    return y == 3
			    }
			    return false
			}`,
		},
		{
			in: `func f(x T) {
						m1 := x.Less
						if (T.Greater)(x, x) {
							_ = m1
						}
					}`,
			want: `func f(x T) {
						m1 := func(a T) func(T) bool {
							return func(b T) bool {
								return a < b
							}
						}(x)
						if (func(a, b T) bool {
							return a > b
						})(x, x) {
							_ = m1
						}
					}`,
		},
		{
			in:   `func f(x I) int { return x.(int) + x.(int) }`,
			want: `func f(x I) int { return x + x }`,
		},
		{
			in:   `func f(x I) bool { return x.(bool) }`,
			want: "", // error
		},
		{
			in:   `func f(x I) { var y, ok = x.(int); _, _ = y, ok }`,
			want: `func f(x I) { var y, ok = x, true; _, _ = y, ok }`,
		},
		{
			in:   `func f(x I) { var y, ok = x.(string); _, _ = y, ok }`,
			want: `func f(x I) { var y, ok = "", false; _, _ = y, ok }`,
		},
		{
			in:   `func f(x I) { y, ok := x.(int); _, _ = y, ok }`,
			want: `func f(x I) { y, ok := x, true; _, _ = y, ok }`,
		},
		{
			in:   `func f(x I) { y, ok := x.(bool); _, _ = y, ok }`,
			want: `func f(x I) { y, ok := false, false; _, _ = y, ok }`,
		},
		{
			in:   `func f(x I) { y, ok := x.([]int); _, _ = y, ok }`,
			want: `func f(x I) { y, ok := nil, false; _, _ = y, ok }`,
		},
		{
			in:   `func f(x I) { y, ok := x.(struct{}); _, _ = y, ok }`,
			want: "func f(x I) {\ny, ok := struct {\n}{}, false\n_, _ = y, ok\n}",
		},
		{
			in:   `func f(x I) { type S struct{ i int }; y, ok := x.(S); _, _ = y, ok }`,
			want: `func f(x I) { type S struct{ i int }; y, ok := S{}, false; _, _ = y, ok }`,
		},
	} {
		code := `
			package p
			type T interface {
				Equal(T) bool
				Less(T) bool
				Greater(T) bool
				GreaterEqual(T) bool
				LessEqual(T) bool
			}
			type I interface{}
	    ` + test.in
		pkg := packageFromSource(code)
		file := singleFile(pkg.apkg)
		ptype := topLevelType("T", pkg)
		var rewrites []rewrite
		for _, am := range append([]augmentMethod{equalMethod}, orderedMethods...) {
			rewrites = append(rewrites, rewrite{ptype, am.name, am.tok})
		}
		tn, err := pkg.topLevelTypeName("I")
		if err != nil {
			t.Fatal(err)
		}
		bindings := []*Binding{
			{tn, types.Typ[types.Int], false},
		}
		err = replaceCode(file, bindings, rewrites, pkg)
		if err != nil {
			if test.want == "" {
				continue
			}
			t.Fatal(err)
		}
		got := nodeString(file.Scope.Lookup("f").Decl, pkg.fset)
		if trim(got) != trim(test.want) {
			t.Errorf("-- got --\n%s\n-- want --\n%s", got, test.want)
			t.Logf("trim(got):  %q\n", trim(got))
			t.Logf("trim(want): %q\n", trim(test.want))
		}
	}
}

func singleFile(apkg *ast.Package) *ast.File {
	for _, f := range apkg.Files {
		return f
	}
	return nil
}

func nodeString(n interface{}, fset *token.FileSet) string {
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, n); err != nil {
		log.Fatal(err)
	}
	return buf.String()
}

// Remove blank lines and trim each line.
func trim(s string) string {
	var lines []string
	for _, line := range strings.Split(s, "\n") {
		line = strings.TrimSpace(line)
		if line != "" {
			lines = append(lines, strings.Replace(line, "\t", " ", -1))
		}
	}
	return strings.Join(lines, "\n")
}

func TestExamples(t *testing.T) {
	log.SetOutput(ioutil.Discard)
	outputDir := fmt.Sprintf("%s/src/github.com/jba/gen/tmp.gitignore", os.Getenv("GOPATH"))
	if err := os.MkdirAll(outputDir, os.ModePerm); err != nil {
		t.Fatal(err)
	}
	for _, test := range []struct {
		dir    string
		outPkg string
		specs  []string
	}{
		// {"stack", "intstack", []string{"T:int"}},
		// {"stack", "stringmapstack", []string{"T:map[string]bool"}},
		// {"slices", "strslices", []string{"T:string"}},
		// {"slices", "timeslices", []string{"T:time.Time"}},
		// {"maps", "pointsets", []string{"K:github.com/jba/gen/examples/geo.Point", "V:bool"}},
		// {"ranges", "intranges", []string{"T:int"}},
		// {"ranges", "floatranges", []string{"T:float64"}},
		// TODO: ranges for the Int type in ranges_test.go (put it in another package)
		{"nested", "intnested", []string{"T:int"}},
	} {
		t.Run(test.outPkg, func(t *testing.T) {
			err := run(filepath.Join("github.com/jba/gen/examples", test.dir), outputDir, test.outPkg, test.specs)
			if err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command("diff", "-u",
				filepath.Join("../testdata/want", test.outPkg), filepath.Join(outputDir, test.outPkg))
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

func TestReloadAST(t *testing.T) {
	fset := token.NewFileSet()
	apkg, err := astPackageFromPath(fset, "github.com/jba/gen/examples/nested")
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range apkg.Files {
		if pos := file.Pos(); pos.IsValid() {
			fmt.Println(fset.File(pos).Name())
		}
	}

	fset2, apkg2, err := reloadAST(fset, apkg)
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range apkg2.Files {
		if pos := file.Pos(); pos.IsValid() {
			fmt.Println(fset2.File(pos).Name())
		}
	}
}

func TestParseDirectives(t *testing.T) {
	for i, test := range []struct {
		src     string
		want    *importDirective // nil if none
		wantErr bool
	}{
		{
			src: `
package p

// gen:import x:1
import "fmt"
`,
			wantErr: true,
		},
		{
			src: `
package p
import (
	"fmt"

    // gen:import x
)
`,
		},
		{
			src: `
package p
import (
	"fmt"
	"math"

	/*
	   gen:import x
       gen:import y
	*/
	"github.com/jba/gen/examples/maps"
	"golang.org/x/net/context"
)
`,
			wantErr: true,
		},
		{
			src: `
package p

import foo "bar" // gen:import X:Y
`,
			want: &importDirective{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		},
		{
			src: `
package p

// gen:import X:Y
import foo "bar"
`,
			want: &importDirective{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		},
		{
			src: `
package p


import (
	"fmt"
	foo "bar" // gen:import X:Y
)
`,
			want: &importDirective{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		},
		{
			src: `
package p


import (
	"fmt"

	// gen:import X:Y
	foo "bar"
)
`,
			want: &importDirective{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		}} {
		fset := token.NewFileSet()
		apkg, err := astPackage(fset, "<src>", test.src)
		if err != nil {
			t.Fatal(err)
		}
		ids, err := parseDirectives(fset, apkg)
		if err != nil {
			if !test.wantErr {
				t.Fatalf("%d: %v", i, err)
			}
			continue
		}
		if test.wantErr {
			t.Errorf("%d: got no error, wanted one", i)
		}
		if len(ids) == 0 {
			if test.want != nil {
				t.Errorf("%d: got none, wanted one", i)
			}
			continue
		}
		if len(ids) > 1 {
			t.Errorf("%d: got > 1 directive", i)
			continue
		}
		got := ids[0]
		got.file = nil
		if !cmp.Equal(got, *test.want, cmp.AllowUnexported(importDirective{})) {
			t.Errorf("%d: got %v, want %v", i, got, test.want)
		}
	}
}

func TestNested(t *testing.T) {
	fset := token.NewFileSet()
	apkg, err := astPackageFromDir(fset, "../examples/nested")
	if err != nil {
		t.Fatal(err)
	}
	_ = apkg
	ast.Print(fset, apkg)
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
		panic(fmt.Sprintf("%s: %v", src, err))
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
	file, err := parser.ParseFile(fset, filename, src, parser.ParseComments)
	if err != nil {
		return nil, err
	}
	return &ast.Package{
		Name:  file.Name.Name,
		Files: map[string]*ast.File{filename: file},
	}, nil
}
