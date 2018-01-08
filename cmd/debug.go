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
