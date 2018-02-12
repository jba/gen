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
)

// TODO: with nesting, the package returned from Check isn't suitable for further use
// because it has the rewritten files of the nested packages in it, and its own references
// to symbols in those packages have been rewritten. Checking a nested package and instiating it
// have to follow different code paths.

// Check checks the generic package at path (with respect to dir) for errors. It returns
// the first error it finds.
func Check(path, dir string, params []string) (*Package, error) {
	// Parse the generic package.
	apkg, err := astPackageFromPath(path, dir)
	if err != nil {
		return nil, err
	}
	return checkPackage(apkg, path, dir, params)
}

func checkPackage(apkg *astPackage, path, dir string, params []string) (*Package, error) {
	if len(apkg.genericImports) > 0 {
		env := map[string]types.Type{}
		for _, p := range params {
			typ, err := paramTypeFromAST(p, apkg)
			if err != nil {
				return nil, err
			}
			env[p] = typ
		}
		// For each generic import, instantiate it with the types given by its
		// binding specs. This may include the generic type params of this package,
		// so we instantiate into apkg.
		err := processGenericImports(apkg, dir, env)
		if err != nil {
			return nil, err
		}
	}
	// Typecheck the package.
	pkg, err := makePackage(path, apkg, theImporter)
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
			return nil, fmt.Errorf("%s: %v", pkg.position(gtn.Pos()), err)
		}
	}
	return pkg, nil
}

// A generic import is the import of a generic package with a set of bindings to the generic params.
// We instantiate the generic import with the given bindings in memory, with a dummy import path.
// We change the import statements to have the new path.
// When we then typecheck the original package, we use an importer that recognizes these new paths.
func processGenericImports(apkg *astPackage, dir string, env map[string]types.Type) error {
	// Check that all equivalent generic imports use the same name. We depend on that fact when rewriting.
	for i, gi1 := range apkg.genericImports {
		for _, gi2 := range apkg.genericImports[i+1:] {
			if gi1.samePathAndBindings(gi2) && gi1.name != gi2.name {
				return fmt.Errorf("different names for same generic import: %q and %q", gi1.name, gi2.name)
			}
		}
	}
	seen := map[string]bool{}
	for _, gi := range apkg.genericImports {
		if seen[gi.name] {
			continue
		}
		seen[gi.name] = true
		var params []string
		bspecs := map[string]string{}
		for bs := range gi.bindingSpecs {
			param, arg, err := ParseBindingSpec(bs)
			if err != nil {
				return err
			}
			bspecs[param] = arg
			params = append(params, param)
		}
		gpkg, err := Check(gi.path, dir, params)
		if err != nil {
			return err
		}
		bindings, err := newBindingMap(bspecs, func(name string) types.Type {
			if t := env[name]; t != nil {
				return t
			}
			return lookupBuiltinName(name)
		})
		if err != nil {
			return err
		}
		if err := instantiateInto(gpkg, gi, bindings, apkg); err != nil {
			return err
		}
	}
	return nil
}

func makePackage(path string, apkg *astPackage, importer types.Importer) (*Package, error) {
	tpkg, info, err := typecheckPackage(path, apkg, importer)
	if err != nil {
		return nil, err
	}
	return &Package{
		Path: path,
		Apkg: apkg,
		Tpkg: tpkg,
		info: info,
	}, nil
}

func typecheckPackage(path string, apkg *astPackage, importer types.Importer) (*types.Package, *types.Info, error) {
	var files []*ast.File
	for _, file := range apkg.pkg.Files {
		files = append(files, file)
	}
	info := &types.Info{
		Defs:  make(map[*ast.Ident]types.Object),
		Types: make(map[ast.Expr]types.TypeAndValue),
	}
	config := &types.Config{
		Importer:                 importer,
		DisableUnusedImportCheck: true,
	}
	tpkg, err := config.Check(path, apkg.fset, files, info)
	if err != nil {
		return nil, nil, fmt.Errorf("typechecker on %s: %v", path, err)
	}
	return tpkg, info, nil
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

type mapImporter struct {
	m   map[string]*types.Package
	imp types.Importer
}

func (mi mapImporter) Import(path string) (*types.Package, error) {
	if p := mi.m[path]; p != nil {
		return p, nil
	}
	return mi.imp.Import(path)
}

func paramTypeFromAST(paramName string, apkg *astPackage) (types.Type, error) {
	obj := apkg.lookupTopLevel(paramName)
	if obj == nil {
		return nil, fmt.Errorf("no top-level name %q in package %s", paramName, apkg.pkg.Name)
	}
	if obj.Kind != ast.Typ {
		return nil, fmt.Errorf("%q is not a type in package %s", paramName, apkg.pkg.Name)
	}
	spec, ok := obj.Decl.(*ast.TypeSpec)
	if !ok {
		return nil, fmt.Errorf("obj.Decl is %T, not TypeSpec", obj.Decl)
	}
	// TODO: support other types in the package.
	return typeSpecToType(spec, lookupBuiltinName)
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

// Converts an AST interface to a types interface.
func typeSpecToType(spec *ast.TypeSpec, lookupName func(string) types.Type) (types.Type, error) {
	tn := types.NewTypeName(token.NoPos, nil, spec.Name.Name, nil)
	n := types.NewNamed(tn, nil, nil)
	iface, ok := spec.Type.(*ast.InterfaceType)
	if !ok {
		return nil, fmt.Errorf("type spec %s: type is not an interface", spec.Name.Name)
	}
	var methods []*types.Func
	var embeddeds []*types.Named // TODO: support embedded types
	for _, f := range iface.Methods.List {
		switch len(f.Names) {
		case 0: //embedded type
			// TODO: Use the file's imports to convert the selector expr of f.Type (if present) to
			// an import path.
			et, err := exprToType(f.Type, lookupBuiltinName)
			if err != nil {
				return nil, err
			}
			fmt.Printf("embedded type %s (%T)\n", et, et)
		case 1:
			methodName := f.Names[0].Name
			sig, err := funcTypeToSignature(f.Type.(*ast.FuncType), map[string]types.Type{spec.Name.Name: n})
			if err != nil {
				return nil, err
			}
			methods = append(methods, types.NewFunc(token.NoPos, nil, methodName, sig))
		default:
			return nil, fmt.Errorf("field %#v: names != 1", f)
		}
	}
	u := types.NewInterface(methods, embeddeds)
	u.Complete()
	n.SetUnderlying(u)
	return n, nil
}

func funcTypeToSignature(ftype *ast.FuncType, env map[string]types.Type) (*types.Signature, error) {
	lookup := func(name string) types.Type {
		if t := env[name]; t != nil {
			return t
		}
		return lookupBuiltinName(name)
	}
	tuple := func(fl *ast.FieldList) (*types.Tuple, error) {
		var vars []*types.Var
		for _, f := range fl.List {
			typ, err := exprToType(f.Type, lookup)
			if err != nil {
				return nil, err
			}
			if len(f.Names) == 0 {
				vars = append(vars, types.NewParam(token.NoPos, nil, "", typ))
			} else {
				for _, id := range f.Names {
					vars = append(vars, types.NewParam(token.NoPos, nil, id.Name, typ))
				}
			}
		}
		return types.NewTuple(vars...), nil
	}

	params, err := tuple(ftype.Params)
	if err != nil {
		return nil, err
	}
	results, err := tuple(ftype.Results)
	if err != nil {
		return nil, err
	}
	// TODO: handle variadic
	return types.NewSignature(nil, params, results, false), nil
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
			gtn.Name(), name, pkg.position(node.Pos()))
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
	for _, file := range pkg.Apkg.pkg.Files {
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

	for _, file := range pkg.Apkg.pkg.Files {
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
