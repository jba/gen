package gen

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"strconv"

	"golang.org/x/tools/go/ast/astutil"
)

// rewrite a method to a binary operator
type rewrite struct {
	argType    types.Type // both arg and receiver type
	methodName string
	op         token.Token
}

func (r *rewrite) match(e ast.Expr, info *types.Info) (ast.Expr, bool) {
	s, ok := e.(*ast.SelectorExpr)
	if !ok {
		return nil, false
	}
	if s.Sel.Name != r.methodName {
		return nil, false
	}
	tv, ok := info.Types[s.X]
	if !ok {
		log.Fatalf("no type info for expression %s", s.X)
	}
	if !types.Identical(tv.Type, r.argType) {
		return nil, false
	}
	return s.X, true
}

// e.g. func(a, b T) bool { return a < b }
func twoArgOpFuncLit(paramName string, tok token.Token) *ast.FuncLit {
	return &ast.FuncLit{
		Type: funcType(
			&ast.Field{Names: []*ast.Ident{id("a"), id("b")}, Type: id(paramName)},
			&ast.Field{Type: id("bool")},
		),
		Body: returnStmtBlock(&ast.BinaryExpr{X: id("a"), Op: tok, Y: id("b")}),
	}
}

// e.g.
// func(a T) func (b T) bool {
//   return func(b T) bool { return a < b }
// }(x)
func oneArgOpFuncCall(e ast.Expr, paramName string, tok token.Token) *ast.CallExpr {
	return &ast.CallExpr{
		Fun: &ast.FuncLit{
			Type: funcType(
				&ast.Field{Names: []*ast.Ident{id("a")}, Type: id(paramName)},
				&ast.Field{Type: funcType(&ast.Field{Type: id(paramName)}, &ast.Field{Type: id("bool")})},
			),
			Body: returnStmtBlock(&ast.FuncLit{
				Type: funcType(
					&ast.Field{Names: []*ast.Ident{id("b")}, Type: id(paramName)},
					&ast.Field{Type: id("bool")}),
				Body: returnStmtBlock(&ast.BinaryExpr{X: id("a"), Op: tok, Y: id("b")}),
			}),
		},
		Args: []ast.Expr{e},
	}
}

func funcType(fields ...*ast.Field) *ast.FuncType {
	// Assume last field is return value.
	return &ast.FuncType{
		Params: &ast.FieldList{
			List: fields[:len(fields)-1],
		},
		Results: &ast.FieldList{
			List: fields[len(fields)-1:],
		},
	}
}

func returnStmtBlock(e ast.Expr) *ast.BlockStmt {
	return &ast.BlockStmt{
		List: []ast.Stmt{
			&ast.ReturnStmt{Results: []ast.Expr{e}},
		},
	}
}

func replaceCode(file *ast.File, bindings []*Binding, rewrites []rewrite, pkg *Package) error {
	// Note: Apply does not walk replacement nodes, but it does continue to walk the original node's
	// children.
	var err error
	var replace func(n ast.Node)

	pre := func(c *astutil.Cursor) bool {
		if err != nil {
			return false
		}
		switch n := c.Node().(type) {
		case *ast.CallExpr:
			for _, r := range rewrites {
				if x, ok := r.match(n.Fun, pkg.info); ok {
					c.Replace(&ast.BinaryExpr{X: x, Op: r.op, Y: n.Args[0]})
					break
				}
			}

		case *ast.SelectorExpr:
			// Not a call, because we would have caught that above. A method expression or value,
			// like t.Less or T.Less.
			for _, r := range rewrites {
				if x, ok := r.match(n, pkg.info); ok {
					name := r.argType.(*types.Named).Obj().Name()
					// The function denoted by n has one or two arguments.
					if pkg.info.Types[n].Type.(*types.Signature).Params().Len() == 2 {
						c.Replace(twoArgOpFuncLit(name, r.op))
					} else {
						c.Replace(oneArgOpFuncCall(x, name, r.op))
					}
					break
				}
			}

		case *ast.AssignStmt:
			if len(n.Lhs) == 2 && len(n.Rhs) == 1 {
				if e, ok := n.Rhs[0].(*ast.TypeAssertExpr); ok {
					n.Rhs = replaceTwoValueAssert(e, bindings, pkg)
				}
			}

		case *ast.ValueSpec:
			if len(n.Names) == 2 && len(n.Values) == 1 {
				if e, ok := n.Values[0].(*ast.TypeAssertExpr); ok {
					n.Values = replaceTwoValueAssert(e, bindings, pkg)
				}
			}

		case *ast.TypeAssertExpr:
			if n.Type == nil {
				// part of type switch; handled below
				break
			}
			for _, b := range bindings {
				if !types.Identical(pkg.info.Types[n.X].Type, b.param.Type()) {
					continue
				}
				if !types.Identical(pkg.info.Types[n.Type].Type, b.arg) {
					err = fmt.Errorf("%s: failed type assertion", pkg.position(n.Pos()))
					return false
				}
				c.Replace(n.X)
				break
			}

		case *ast.TypeSwitchStmt:
			var e ast.Expr
			var assign *ast.AssignStmt
			switch s := n.Assign.(type) {
			case *ast.ExprStmt:
				e = s.X.(*ast.TypeAssertExpr).X
			case *ast.AssignStmt:
				if len(s.Rhs) != 1 {
					panic("bad assign stmt in type switch")
				}
				e = s.Rhs[0].(*ast.TypeAssertExpr).X
				assign = s
			default:
				panic("unknown statement in type switch")
			}
			var binding *Binding
			for _, b := range bindings {
				if types.Identical(pkg.info.Types[e].Type, b.param.Type()) {
					binding = b
					break
				}
			}
			if binding == nil {
				break
			}
			var stmts []ast.Stmt
			if n.Init != nil {
				stmts = []ast.Stmt{n.Init}
			}
			cc := matchingCase(n.Body.List, binding.arg, pkg)
			if cc != nil {
				// TODO: if cc.Body is panic call, return error here.
				if assign != nil {
					stmts = append(stmts, &ast.AssignStmt{
						Lhs: assign.Lhs,
						Tok: token.DEFINE,
						Rhs: []ast.Expr{assign.Rhs[0].(*ast.TypeAssertExpr).X},
					})
				}
				stmts = append(stmts, cc.Body...)
			}
			if len(stmts) == 0 {
				c.Delete()
			} else {
				block := &ast.BlockStmt{List: stmts}
				replace(block)
				c.Replace(block)
			}
			return false
		}
		return true
	}

	replace = func(n ast.Node) {
		astutil.Apply(n, pre, nil)
	}

	replace(file)
	return err
}

// Given a list of case clauses from a type switch, return the case clause that
// matches type t, or nil if none.
func matchingCase(cases []ast.Stmt, t types.Type, pkg *Package) *ast.CaseClause {
	var defaultCase *ast.CaseClause
	for _, s := range cases {
		cc := s.(*ast.CaseClause)
		if cc.List == nil {
			defaultCase = cc
		} else {
			for _, te := range cc.List {
				if types.Identical(pkg.info.Types[te].Type, t) {
					return cc
				}
			}
		}
	}
	return defaultCase
}

// Make an ast.Node that corresponds to the type.
func typeToExpr(typ types.Type, tpkg *types.Package) ast.Expr {
	switch typ := typ.(type) {
	case *types.Named:
		p := typ.Obj().Pkg()
		n := typ.Obj().Name()
		if p == tpkg {
			return id(n)
		}
		return &ast.SelectorExpr{
			X:   id(p.Name()),
			Sel: id(n),
		}
	case *types.Basic:
		return id(typ.Name())
	case *types.Slice:
		return &ast.ArrayType{
			Elt: typeToExpr(typ.Elem(), tpkg),
		}
	case *types.Array:
		return &ast.ArrayType{
			Elt: typeToExpr(typ.Elem(), tpkg),
			Len: &ast.BasicLit{Value: strconv.FormatInt(typ.Len(), 10)},
		}
	case *types.Map:
		return &ast.MapType{
			Key:   typeToExpr(typ.Key(), tpkg),
			Value: typeToExpr(typ.Elem(), tpkg),
		}
	case *types.Struct:
		var fields []*ast.Field
		for i := 0; i < typ.NumFields(); i++ {
			f := typ.Field(i)
			fields = append(fields, &ast.Field{
				Names: []*ast.Ident{id(f.Name())},
				Type:  typeToExpr(f.Type(), tpkg),
			})
		}
		return &ast.StructType{Fields: &ast.FieldList{
			List:    fields,
			Opening: 1,
			Closing: 2,
		}}
	default:
		panic(fmt.Sprintf("unknown type %T", typ))
	}
}

func replaceTwoValueAssert(e *ast.TypeAssertExpr, bindings []*Binding, pkg *Package) []ast.Expr {
	for _, b := range bindings {
		if !types.Identical(pkg.info.Types[e.X].Type, b.param.Type()) {
			continue
		}
		etype := pkg.info.Types[e.Type].Type
		if types.Identical(etype, b.arg) {
			return []ast.Expr{e.X, id("true")}
		} else {
			return []ast.Expr{zeroExpr(etype, pkg.Tpkg), id("false")}
		}
	}
	return []ast.Expr{e}
}

// Constructs an expression for the zero value of type t.
func zeroExpr(t types.Type, tpkg *types.Package) ast.Expr {
	if nt, ok := t.(*types.Named); ok {
		if _, ok := t.Underlying().(*types.Struct); ok {
			return &ast.CompositeLit{Type: typeToExpr(nt, tpkg)}
		}
	}
	switch t := t.Underlying().(type) {
	case *types.Basic:
		switch {
		case t.Info()&types.IsBoolean != 0:
			return id("false")
		case t.Info()&types.IsNumeric != 0:
			return lit(token.INT, "0")
		case t.Info()&types.IsString != 0:
			return lit(token.STRING, `""`)
		default:
			panic("bad basic type")
		}
	case *types.Pointer, *types.Interface, *types.Chan, *types.Slice, *types.Signature, *types.Map:
		return id("nil")
	case *types.Struct:
		return &ast.CompositeLit{Type: typeToExpr(t, tpkg)}
	case *types.Array:
		return &ast.CompositeLit{
			Type: &ast.ArrayType{
				Len: lit(token.INT, strconv.Itoa(int(t.Len()))),
				Elt: typeToExpr(t.Elem(), tpkg),
			},
		}
	default:
		panic("unknown type")
	}
}
