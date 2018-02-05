package gen

import (
	"go/token"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestReloadAST(t *testing.T) {
	fset := token.NewFileSet()
	apkg, err := astPackageFromPath(fset, "github.com/jba/gen/examples/nested", "")
	if err != nil {
		t.Fatal(err)
	}
	_, _, err = reloadAST(fset, apkg)
	if err != nil {
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
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		},
		{
			src: `
package p

// gen:import X:Y
import foo "bar"
`,
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		},
		{
			src: `
package p


import (
	"fmt"
	foo "bar" // gen:import X:Y
)
`,
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
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
			want: &genericImport{name: "foo", path: "bar", bindingSpecs: []string{"X:Y"}},
		}} {
		fset := token.NewFileSet()
		apkg, err := astPackage(fset, "<src>", test.src)
		if err != nil {
			t.Fatal(err)
		}
		ids, err := parseComments(fset, apkg)
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
		if !cmp.Equal(got, *test.want, cmp.AllowUnexported(genericImport{})) {
			t.Errorf("%d: got %v, want %v", i, got, test.want)
		}
	}
}
