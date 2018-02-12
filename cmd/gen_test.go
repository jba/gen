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
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

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
		// {"sortedset", "intss", []string{"T:int"}},
		// // TODO: ranges for the Int type in ranges_test.go (put it in another package)
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

// func TestNested(t *testing.T) {
// 	fset := token.NewFileSet()
// 	apkg, err := astPackageFromDir(fset, "../examples/nested")
// 	if err != nil {
// 		t.Fatal(err)
// 	}
// 	_ = apkg
// 	ast.Print(fset, apkg)
// }
