package gen

import (
	"bytes"
	"go/format"
	"go/token"
	"log"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestReload(t *testing.T) {
	apkg, err := astPackageFromPath("github.com/jba/gen/examples/nested", "")
	if err != nil {
		t.Fatal(err)
	}
	if err := apkg.reload(); err != nil {
		t.Fatal(err)
	}
}

func TestParseComments(t *testing.T) {
	for i, test := range []struct {
		src     string
		want    *genericImport // nil if none
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
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: map[string]bool{"X:Y": true}},
		},
		{
			src: `
package p

// gen:import X:Y
import foo "bar"
`,
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: map[string]bool{"X:Y": true}},
		},
		{
			src: `
package p


import (
	"fmt"
	foo "bar" // gen:import X:Y
)
`,
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: map[string]bool{"X:Y": true}},
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
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: map[string]bool{"X:Y": true}},
		}} {
		apkg := astPkgFromSrc(test.src, false)
		ids, err := parseComments(apkg.fset, apkg.pkg)
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
		got.spec = nil
		got.file = nil
		if !cmp.Equal(got, *test.want, cmp.AllowUnexported(genericImport{})) {
			t.Errorf("%d: got %v, want %v", i, got, test.want)
		}
	}
}

func TestPrefixTopLevelSymbols(t *testing.T) {
	src := `
package p
const foo = 1
type T int
var v T
func f(foo T) T { var v = foo; return foo }

func g(bar T) { type T bool; v = bar }
`
	apkg := astPackageFromSource(src)
	file := singleFile(apkg.pkg)
	prefixTopLevelSymbols(file, "pre_")
	got := nodeString(file, apkg.fset)
	want := `package p

const pre_foo = 1

type pre_T int

var pre_v pre_T

func pre_f(foo pre_T) pre_T { var v = foo; return foo }

func pre_g(bar pre_T) { type T bool; pre_v = bar }
`
	if got != want {
		t.Errorf("got\n%s\nwant\n%s", got, want)
	}
}

func TestReplaceImportWithPrefix(t *testing.T) {
	src := `
package p
import foo "bar"
const c = foo.c
type t = foo.t
var v = foo.v
func f(x foo.t) { var y = foo.d; var foo struct{i int}; foo.i = 1 }

`
	apkg := astPackageFromSource(src)
	file := singleFile(apkg.pkg)
	replaceImportWithPrefix(file, "foo", "foo_")
	got := nodeString(file, apkg.fset)
	want := `package p

import foo "bar"

const c = foo_c

type t = foo_t

var v = foo_v

func f(x foo_t,) { var y = foo_d; var foo struct{ i int }; foo.i = 1 }
`
	// Note: extra comma in f's arglist is what astutil.Apply followed by format.Node does.
	if got != want {
		t.Errorf("got\n%s\nwant\n%s", got, want)
	}
}

func nodeString(n interface{}, fset *token.FileSet) string {
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, n); err != nil {
		log.Fatal(err)
	}
	return buf.String()
}
