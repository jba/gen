package gen

import (
	"errors"
	"fmt"
	"go/ast"
	"go/importer"
	"go/token"
	"go/types"
	"log"
	"os"
	"strconv"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

// CheckPath checks the generic package at path (with respect to dir) for errors. It returns
// the first error it finds.
func CheckPath(path, dir string, params []string) (*Package, error) {
	// Parse the generic package.
	fset := token.NewFileSet()
	apkg, err := astPackageFromPath(fset, path, dir)
	if err != nil {
		return nil, err
	}
	// Handle import directives by recursively instantiating the types.
	importDirectives, err := parseDirectives(fset, apkg)
	if err != nil {
		return nil, err
	}
	if len(importDirectives) > 0 {
		bindingMap := map[string]types.Type{}
		for _, p := range params {
			typ, err := paramTypeFromAST(p, apkg)
			if err != nil {
				return nil, err
			}
			bindingMap[p] = typ
		}
		outputDir := "/tmp"
		err := processImportDirectives(outputDir, importDirectives, fset, bindingMap)
		if err != nil {
			return nil, err
		}
		// We changed the AST, so the fset will be wrong. Reload.
		fset, apkg, err = reloadAST(fset, apkg)
		if err != nil {
			return nil, err
		}
	}
	// Typecheck the package.
	pkg, err := makePackage(path, fset, apkg)
	if err != nil {
		return nil, err
	}
	pkg.Params = params
	// Check each generic parameter.
	for _, paramName := range params {
		gtn, err := pkg.topLevelTypeName(paramName)
		if err != nil {
			return nil, err
		}
		if err := checkParam(gtn, pkg); err != nil {
			return nil, fmt.Errorf("%s: %v", pkg.Fset.Position(gtn.Pos()), err)
		}
	}
	return pkg, nil
}

// An import directive is of the form "gen:import bindingSpec...".
func processImportDirectives(outputDir string, ids []importDirective, fset *token.FileSet, bindings map[string]types.Type) error {
	panic("unimp")
}

// 	ipath := dirToImportPath(outputDir)
// 	if ipath == "" {
// 		return fmt.Errorf("cannot map directory %s to an import path", outputDir)
// 	}
// 	seen := map[string]bool{}
// 	for _, id := range ids {
// 		key := id.name + ";" + id.path
// 		if !seen[key] {
// 			seen[key] = true
// 			bindings2, err := makeBindingMap(id.bindingSpecs, func(name string) types.Type {
// 				if t := bindings[name]; t != nil {
// 					return t
// 				}
// 				return lookupBuiltinName(name)
// 			})
// 			if err != nil {
// 				return err
// 			}
// 			if err := runWithBindings(id.path, outputDir, id.name, bindings2); err != nil {
// 				return err
// 			}
// 		}
// 		if err := instantiateImportPaths(fset, id.file, id.name, id.path, ipath+"/"+id.name); err != nil {
// 			return err
// 		}
// 	}
// 	return nil
// }

func typecheckPackage(path string, fset *token.FileSet, apkg *ast.Package) (*types.Package, *types.Info, error) {
	var files []*ast.File
	for _, file := range apkg.Files {
		files = append(files, file)
	}
	info := &types.Info{
		Defs:  make(map[*ast.Ident]types.Object),
		Types: make(map[ast.Expr]types.TypeAndValue),
	}
	tpkg, err := typecheck(path, fset, files, info)
	if err != nil {
		return nil, nil, fmt.Errorf("typechecker on %s: %v", path, err)
	}
	return tpkg, info, nil
}

func makePackage(path string, fset *token.FileSet, apkg *ast.Package) (*Package, error) {
	tpkg, info, err := typecheckPackage(path, fset, apkg)
	if err != nil {
		return nil, err
	}
	return &Package{
		Path: path,
		Fset: fset,
		Apkg: apkg,
		Tpkg: tpkg,
		info: info,
	}, nil
}

type Package struct {
	Path   string
	Fset   *token.FileSet
	Apkg   *ast.Package
	Tpkg   *types.Package
	Params []string
	info   *types.Info
}

func (p *Package) String() string {
	return fmt.Sprintf("%s (%s)", p.Tpkg.Name(), p.Tpkg.Path())
}

func (p *Package) topLevelTypeName(name string) (*types.TypeName, error) {
	gobj := p.Tpkg.Scope().Lookup(name)
	if gobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", name, p)
	}
	gtn, ok := gobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", name, p)
	}
	return gtn, nil
}

type compiledThenSourceImporter struct {
	defaultImporter types.Importer
	sourceImporter  types.Importer
}

func (c compiledThenSourceImporter) Import(path string) (*types.Package, error) {
	p, err := c.defaultImporter.Import(path)
	if err != nil {
		p, err = c.sourceImporter.Import(path)
		if err != nil {
			return nil, fmt.Errorf("importer: %v", err)
		}
	}
	return p, nil
}

var theImporter = compiledThenSourceImporter{
	importer.Default(),
	importer.For("source", nil),
}

var typecheck = (&types.Config{Importer: theImporter, DisableUnusedImportCheck: true}).Check

func paramTypeFromAST(paramName string, apkg *ast.Package) (types.Type, error) {
	obj := apkg.Scope.Objects[paramName]
	if obj == nil {
		return nil, fmt.Errorf("no top-level name %q in package %s", paramName, apkg.Name)
	}
	if obj.Kind != ast.Typ {
		return nil, fmt.Errorf("%q is not a type in package %s", paramName, apkg.Name)
	}
	expr, ok := obj.Type.(ast.Expr)
	if !ok {
		return nil, fmt.Errorf("obj.Type is %T, not expr", obj.Type)
	}
	return exprToType(expr, lookupBuiltinName)
}

func exprToType(expr ast.Expr, lookupName func(string) types.Type) (types.Type, error) {
	switch e := expr.(type) {
	case *ast.Ident:
		if t := lookupName(e.Name); t != nil {
			return t, nil
		}
		return nil, fmt.Errorf("unknown type name %s", e.Name)

	case *ast.SelectorExpr:
		var importPath string
		switch x := e.X.(type) {
		case *ast.Ident:
			importPath = x.Name
		case *ast.BasicLit:
			var err error
			importPath, err = strconv.Unquote(x.Value)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("bad selector expression type %T", x)
		}
		return lookupNamedType(importPath, e.Sel.Name)

	case *ast.MapType:
		k, err := exprToType(e.Key, lookupName)
		if err != nil {
			return nil, err
		}
		if !types.Comparable(k) {
			return nil, fmt.Errorf("map key type %s is not comparable", k)
		}
		v, err := exprToType(e.Value, lookupName)
		if err != nil {
			return nil, err
		}
		return types.NewMap(k, v), nil

	case *ast.ArrayType:
		elType, err := exprToType(e.Elt, lookupName)
		if err != nil {
			return nil, err
		}
		if e.Len == nil {
			return types.NewSlice(elType), nil
		} else {
			bl, ok := e.Len.(*ast.BasicLit)
			if !ok {
				return nil, errors.New("array length is not a literal integer")
			}
			length, err := strconv.ParseInt(bl.Value, 10, 64)
			if err != nil {
				return nil, fmt.Errorf("array length: %v", err)
			}
			return types.NewArray(elType, length), nil
		}

	case *ast.StructType:
		var fields []*types.Var
		for _, f := range e.Fields.List {
			ft, err := exprToType(f.Type, lookupName)
			if err != nil {
				return nil, err
			}
			for _, n := range f.Names {
				fields = append(fields, types.NewField(token.NoPos, nil, n.Name, ft, false))
			}
		}
		return types.NewStruct(fields, nil), nil

	default:
		return nil, fmt.Errorf("unknown type expr %T", e)
	}
}

func lookupNamedType(importPath, typeName string) (types.Type, error) {
	tpkg, err := theImporter.Import(importPath)
	if err != nil {
		return nil, err
	}
	sobj := tpkg.Scope().Lookup(typeName)
	if sobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", typeName, tpkg.Name())
	}
	stn, ok := sobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", typeName, tpkg.Path())
	}
	return stn.Type(), nil
}

func lookupBuiltinName(name string) types.Type {
	for _, b := range types.Typ {
		if b.Name() == name && b.Kind() != types.UnsafePointer && b.Info()&types.IsUntyped == 0 {
			return b
		}
	}
	return nil
}

type needsComparableError string

func (e needsComparableError) Error() string { return string(e) }

type needsNillableError string

func (e needsNillableError) Error() string { return string(e) }

type notInterfaceError string

func (e notInterfaceError) Error() string { return string(e) }

type missingMethodError string

func (e missingMethodError) Error() string { return string(e) }

func checkParam(gtn *types.TypeName, pkg *Package) error {
	putype, ok := gtn.Type().Underlying().(*types.Interface)
	if !ok {
		return notInterfaceError(fmt.Sprintf("type must be interface, not %T\n", gtn.Type()))
	}

	if msg := checkSpecialInterface("Comparable", gtn, putype, pkg, comparableNode); msg != "" {
		return needsComparableError(msg)
	}
	if msg := checkSpecialInterface("Nillable", gtn, putype, pkg, nillableNode); msg != "" {
		return needsNillableError(msg)
	}
	return nil
}

func checkSpecialInterface(name string, gtn *types.TypeName, iface *types.Interface, pkg *Package, finder func(types.Type, *Package) ast.Node) string {
	node := finder(gtn.Type(), pkg)
	has := implementsSpecialInterface(iface, name)
	if node != nil && !has {
		return fmt.Sprintf("param %s does not implement gen.%s, but it is required at %s",
			gtn.Name(), name, pkg.Fset.Position(node.Pos()))
	}
	if node == nil && has {
		log.Printf("param %s includes gen.%s, but does not need it", gtn.Name(), name)
	}
	return ""
}

func dirToImportPath(dir string) string {
	// TODO(jba): make more robust?
	for _, gp := range strings.Split(os.Getenv("GOPATH"), ":") {
		p := gp + "/src/"
		if strings.HasPrefix(dir, p) {
			return dir[len(p):]
		}
	}
	return ""
}

// If t needs to implement Comparable in pkg, return a node that proves it.
// Else return nil.
func comparableNode(t types.Type, pkg *Package) ast.Node {
	typeOf := func(e ast.Expr) types.Type {
		return pkg.info.Types[e].Type
	}
	var result ast.Node
	for _, file := range pkg.Apkg.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			if needsComparable(n, t, typeOf) {
				result = n
				return false
			}
			return true
		})
		if result != nil {
			return result
		}
	}
	return nil
}

// needsComparable reports whether n implies that t must be comparable.
func needsComparable(n ast.Node, t types.Type, typeOf func(ast.Expr) types.Type) bool {
	switch n := n.(type) {
	case *ast.BinaryExpr:
		if !(n.Op == token.EQL || n.Op == token.NEQ) {
			return false
		}

		tx := typeOf(n.X)
		ty := typeOf(n.Y)
		// Consider only == and != where neither operand is nil.
		return !isNil(tx) && !isNil(ty) && (!comparableMod(tx, t) || !comparableMod(ty, t))

	case *ast.IndexExpr:
		_, ok := typeOf(n.X).(*types.Map)
		return ok && !comparableMod(typeOf(n.Index), t)

	case *ast.MapType:
		return !comparableMod(typeOf(n.Key), t)

	case *ast.SwitchStmt:
		// Comparable required only if there is some non-nil case expression.
		if someCaseExpr(n, func(e ast.Expr) bool { return !isNil(typeOf(e)) }) {
			return !comparableMod(typeOf(n.Tag), t)
		}
		return false

	default:
		return false
	}
}

// Reports whether there is a case expression for which f returns true.
func someCaseExpr(n *ast.SwitchStmt, f func(ast.Expr) bool) bool {
	for _, s := range n.Body.List {
		if c, ok := s.(*ast.CaseClause); ok {
			for _, e := range c.List {
				if f(e) {
					return true
				}
			}
		}
	}
	return false
}

func isNil(t types.Type) bool {
	b, ok := t.(*types.Basic)
	return ok && b.Kind() == types.UntypedNil
}

// needsNillable reports whether node n implies that a generic parameter needs to be declared nillable.
// match reports whether its argument type is the generic parameter type (or an equivalent type).
// typeOf returns the type of an expression.
// sig is the signature of the enclosing function, if any.
func needsNillable(n ast.Node, match func(types.Type) bool, typeOf func(ast.Expr) types.Type, sig *types.Signature) bool {
	switch n := n.(type) {
	case *ast.BinaryExpr:
		if !(n.Op == token.EQL || n.Op == token.NEQ) {
			return false
		}
		tx := typeOf(n.X)
		ty := typeOf(n.Y)
		return (isNil(tx) && match(ty)) || (isNil(ty) && match(tx))

	case *ast.AssignStmt:
		for i := 0; i < len(n.Lhs); i++ {
			if match(typeOf(n.Lhs[i])) && isNil(typeOf(n.Rhs[i])) {
				return true
			}
		}

	case *ast.ValueSpec:
		if match(typeOf(n.Type)) {
			for _, v := range n.Values {
				if isNil(typeOf(v)) {
					return true
				}
			}
		}

	case *ast.CallExpr:
		sig, ok := typeOf(n.Fun).(*types.Signature)
		if ok {
			for i, arg := range n.Args {
				if isNil(typeOf(arg)) && match(sig.Params().At(i).Type()) {
					return true
				}
			}
		}

	case *ast.ReturnStmt:
		for i, r := range n.Results {
			if isNil(typeOf(r)) && match(sig.Results().At(i).Type()) {
				return true
			}
		}

	case *ast.SendStmt:
		return isNil(typeOf(n.Value)) && match(typeOf(n.Chan).(*types.Chan).Elem())

	case *ast.SwitchStmt:
		return match(typeOf(n.Tag)) && someCaseExpr(n, func(e ast.Expr) bool { return isNil(typeOf(e)) })
	}
	return false
}

// If t needs to implement Nillable in pkg, return a node that proves it.
// Else return nil.
func nillableNode(t types.Type, pkg *Package) ast.Node {
	match := func(x types.Type) bool { return types.Identical(x, t) }
	typeOf := func(e ast.Expr) types.Type {
		return pkg.info.Types[e].Type
	}
	var result ast.Node

	var withSig func(sig *types.Signature) visitorFunc
	withSig = func(sig *types.Signature) visitorFunc {
		var vf visitorFunc
		vf = func(n ast.Node) ast.Visitor {
			switch n := n.(type) {
			case *ast.FuncDecl:
				return withSig(pkg.info.Defs[n.Name].Type().(*types.Signature))

			case *ast.FuncLit:
				return withSig(typeOf(n.Type).(*types.Signature))

			default:
				if needsNillable(n, match, typeOf, sig) {
					result = n
					return nil
				}
			}
			return vf
		}
		return vf
	}

	for _, file := range pkg.Apkg.Files {
		ast.Walk(withSig(nil), file)
		if result != nil {
			return result
		}
	}
	return nil
}

type visitorFunc func(ast.Node) ast.Visitor

func (v visitorFunc) Visit(n ast.Node) ast.Visitor { return v(n) }

// Reports whether t is comparable, assuming the assumeNot arg is not comparable.
func comparableMod(t types.Type, assumeNot types.Type) bool {
	if types.Identical(t, assumeNot) {
		return false
	}
	// Code from https://golang.org/src/go/types/predicates.go.
	switch t := t.Underlying().(type) {
	case *types.Basic:
		// assume invalid types to be comparable
		// to avoid follow-up errors
		return t.Kind() != types.UntypedNil
	case *types.Pointer, *types.Interface, *types.Chan:
		return true
	case *types.Struct:
		for i := 0; i < t.NumFields(); i++ {
			if !comparableMod(t.Field(i).Type(), assumeNot) {
				return false
			}
		}
		return true
	case *types.Array:
		return comparableMod(t.Elem(), assumeNot)
	}
	return false
}

func instantiateImportPaths(fset *token.FileSet, file *ast.File, name, gpath, ipath string) error {
	astutil.DeleteImport(fset, file, gpath)
	astutil.AddNamedImport(fset, file, name, ipath)
	return nil
}
