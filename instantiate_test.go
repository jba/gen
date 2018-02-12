package gen

import (
	"fmt"
	"go/types"
	"testing"
)

func TestInstantiate(t *testing.T) {
	apkg := astPackageFromSource(stackSrc)
	pkg, err := checkPackage(apkg, "PATH", "DIR", []string{"T"})
	if err != nil {
		t.Fatal(err)
	}
	err = Instantiate(pkg, "instack", map[string]types.Type{"T": types.Typ[types.Int]})
	if err != nil {
		t.Fatal(err)
	}
	file := singleFile(pkg.Apkg.pkg)
	got := nodeString(file, pkg.Apkg.fset)
	want := `package instack

type T = int

type Stack struct{ items []T }

func (s *Stack) Push(x T) { s.items = append(s.items, x) }
`
	if got != want {
		t.Errorf("got:\n%s\nwant:\n%s", got, want)
	}
}

// Don't prefix methods
// Don't prefix init()

func TestInstantiateInto(t *testing.T) {
	//func InstantiateInto(gpkg *Package, gimp genericImport, bindings map[string]types.Type, dest *astPackage) error
	src := `
package p
import intstack "github.com/jba/gen/examples/stack" // gen:import T:int

func push(s *intstack.Stack, i int) { s.Push(i) }
`
	stackPkg, err := astPackageFromDir("examples/stack")
	if err != nil {
		t.Fatal(err)
	}
	gpkg, err := checkPackage(stackPkg, "github.com/jba/gen/examples/stack", "", []string{"T"})
	if err != nil {
		t.Fatal(err)
	}
	apkg := astPackageFromSource(src)
	if got := len(apkg.genericImports); got != 1 {
		t.Fatalf("want one generic import, got %d", got)
	}
	err = instantiateInto(gpkg, apkg.genericImports[0], map[string]types.Type{"T": types.Typ[types.Int]}, apkg)
	if err != nil {
		t.Fatal(err)
	}
	for filename, file := range apkg.pkg.Files {
		fmt.Printf("=== %s ===\n", filename)
		fmt.Println(nodeString(file, apkg.fset))
	}
}
