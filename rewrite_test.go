package gen

import (
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"strings"
	"testing"
)

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
		file := singleFile(pkg.Apkg)
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
		got := nodeString(file.Scope.Lookup("f").Decl, pkg.Fset)
		if trim(got) != trim(test.want) {
			t.Errorf("-- got --\n%s\n-- want --\n%s", got, test.want)
			t.Logf("trim(got):  %q\n", trim(got))
			t.Logf("trim(want): %q\n", trim(test.want))
		}
	}
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

func singleFile(apkg *ast.Package) *ast.File {
	for _, f := range apkg.Files {
		return f
	}
	return nil
}

func topLevelType(name string, pkg *Package) types.Type {
	tn, err := pkg.topLevelTypeName(name)
	if err != nil {
		log.Fatal(err)
	}
	return tn.Type()
}
