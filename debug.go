package gen

import (
	"fmt"
	"go/ast"
	"go/types"
)

func dumpTypeSpec(ts *ast.TypeSpec, info *types.Info) {
	fmt.Printf("TypeSpec %q:\n", ts.Name.Name)
	typ := info.Defs[ts.Name].Type()
	ut := typ.Underlying()
	switch ut := ut.(type) {
	case *types.Interface:
		for i := 0; i < ut.NumMethods(); i++ {
			fmt.Printf("%d: %v\n", i, ut.Method(i))
		}
	default:
		fmt.Printf("unknown underlying type: %T\n", ut)
	}
}

func dumpInterface(name string, iface *types.Interface) {
	fmt.Printf("interface %s:\n", name)
	fmt.Println("explicit")
	for i := 0; i < iface.NumExplicitMethods(); i++ {
		fmt.Printf("  %s\n", iface.ExplicitMethod(i))
	}
	fmt.Println("embedded")
	for i := 0; i < iface.NumEmbeddeds(); i++ {
		dumpInterface(iface.Embedded(i).String(), iface.Embedded(i).Underlying().(*types.Interface))
	}
}

func dumpType(t types.Type, level int) {
	if level > 10 {
		fmt.Println("error: TOO DEEP")
		return
	}
	printf := func(format string, args ...interface{}) {
		for i := 0; i < level; i++ {
			fmt.Print("    ")
		}
		fmt.Printf(format, args...)
	}

	printf("type %[1]s (%[1]T):\n", t)
	printf("- underlying: %[1]v (%[1]T)\n", t.Underlying())
	switch t := t.(type) {
	case *types.Named:
		tn := t.Obj()
		printf("- typename: %v\n", tn)
		printf("- # methods: %d\n", t.NumMethods())
		if tn.Type() == t {
			printf("t's TypeName's type == t\n")
		} else {
			dumpType(tn.Type(), level+1)
		}
	case *types.Signature:
		for i := 0; i < t.Params().Len(); i++ {
			v := t.Params().At(i)
			printf("param #%d:\n", i)
			dumpType(v.Type(), level+1)
		}
		for i := 0; i < t.Results().Len(); i++ {
			v := t.Results().At(i)
			printf("result #%d:\n", i)
			dumpType(v.Type(), level+1)
		}
	}
}

func dumpCommentMap(m ast.CommentMap) {
	for n, cgs := range m {
		fmt.Printf("%#v: ", n)
		for _, cg := range cgs {
			dumpCommentGroup(cg)
		}
		fmt.Println()
	}
}

func dumpCommentGroup(cg *ast.CommentGroup) {
	if cg == nil {
		return
	}
	for _, c := range cg.List {
		fmt.Printf("%q\n", c.Text)
	}
}
