package gen

import (
	"go/ast"
	"go/token"
)

func lit(kind token.Token, s string) *ast.BasicLit {
	return &ast.BasicLit{Kind: kind, Value: s}
}

func id(name string) *ast.Ident {
	return &ast.Ident{Name: name}
}
