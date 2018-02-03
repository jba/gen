package main

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
	fmt.Printf("%*stype %s (%T):\n", level*4, "", t, t)
	fmt.Printf("%*s- underlying: %v (%T)\n", level*4, "", t.Underlying(), t.Underlying())
	if named, ok := t.(*types.Named); ok {
		tn := named.Obj()
		fmt.Printf("%*s- typename: %v\n", level*4, "", tn)
		if tn.Type() == t {
			fmt.Printf("t's TypeName's type == t\n")
		} else {
			dumpType(tn.Type(), level+1)
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
