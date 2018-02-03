// TODO
// - rewrite type switches
// - typecheck the output to make sure we didn't miss anything (like "var x T = 7").
// - a generic package importing other generic packages (ones with interface definitions)
//
// - When a generic param has the Comparable constraint, allow instantiations of interface types.
//   That mirrors the compiler, which allows interface types in == and map indexing and defers the comparable
//   check to runtime.

// - Given
//     type T interface{}
//     type U T
//   the types package loses the information that U was defined in terms of T. So we can't do a comparability
//   check correctly: map[U]int will succeed even though we are assuming T is not comparable. I guess I'll
//   have to use the ast to recognize this case.

// The init statement in a type switch can define variables which are not used in the selected
// case. The resulting code doesn't compile because of the unused identifier. E.g.
//		switch z := 1; x.(type) {
//		case int: fmt.Println()
//		case bool: fmt.Println(z)
//		}
// If the arg type is int, the resulting block is { z := 1; fmt.Println() }, which is invalid.

// examples (for readme):
// - container/ring
// - jba/btree
// - pubsub/pullstream
// - bundler
// - lru.Cache?
// - sync.Map?

package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/build"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os"
	"path"
	"path/filepath"
	"strconv"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

var (
	genPath    = flag.String("g", "", "import path of generic package")
	outputDir  = flag.String("o", "", "output directory (resulting package will be in a subdir of this)")
	outputName = flag.String("p", "", "output package name (also subdir name)")
)

func main() {
	flag.Parse()
	requireFlag("g", *genPath)
	requireFlag("o", *outputDir)
	requireFlag("p", *outputName)

	err := run(*genPath, *outputDir, *outputName, flag.Args())
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}
}

func requireFlag(flag, val string) {
	if val == "" {
		log.Fatalf("need -%s", flag)
	}
}

func run(genPath, outputDir, outputName string, args []string) error {
	// Parse the bindings, and build the arg types.
	if len(args) == 0 {
		return errors.New("need bindings")
	}
	argTypeMap, err := makeBindingMap(args, lookupBuiltinName)
	if err != nil {
		return err
	}
	return runWithBindings(genPath, outputDir, outputName, argTypeMap)
}

func makeBindingMap(args []string, lookup func(string) types.Type) (map[string]types.Type, error) {
	m := map[string]types.Type{}
	for _, arg := range args {
		param, arg, err := parseBindingSpec(arg)
		if err != nil {
			return nil, err
		}
		atype, err := buildType(arg, lookup)
		if err != nil {
			return nil, err
		}
		m[param] = atype
	}
	return m, nil
}

func runWithBindings(genPath, outputDir, outputName string, bindingMap map[string]types.Type) error {
	fmt.Printf("runWithBindings genPath=%s, outputDir=%s, outputName=%s\n",
		genPath, outputDir, outputName)
	// Parse the generic package.
	fset := token.NewFileSet()
	apkg, err := astPackageFromPath(fset, genPath)
	if err != nil {
		return err
	}
	// Handle import directives by recursively instantiating the types.
	importDirectives, err := parseDirectives(fset, apkg)
	if err != nil {
		return err
	}
	if len(importDirectives) > 0 {
		err := processImportDirectives(outputDir, importDirectives, fset, bindingMap)
		if err != nil {
			return err
		}
		// We changed the AST, so the fset will be wrong. Reload.
		fset, apkg, err = reloadAST(fset, apkg)
		if err != nil {
			return err
		}
	}
	// Typecheck the package.
	pkg, err := makePackage(fset, genPath, apkg)
	if err != nil {
		return err
	}
	var bindings []*Binding
	for param, argType := range bindingMap {
		b, err := newBinding(param, argType, pkg)
		if err != nil {
			return err
		}
		bindings = append(bindings, b)
	}
	if err := substitutePackage(pkg, bindings, outputName); err != nil {
		return err
	}
	for _, b := range bindings {
		if !b.found {
			return fmt.Errorf("no type parameter %s in %s", b.param, genPath)
		}
	}
	if err := os.MkdirAll(outputDir, os.ModePerm); err != nil {
		return err
	}
	tempOutDir, err := ioutil.TempDir(outputDir, "gen-")
	if err != nil {
		return err
	}
	if err := printPackage(pkg, tempOutDir); err != nil {
		if err := os.Remove(tempOutDir); err != nil {
			log.Printf("removing %s: %v", tempOutDir, err)
		}
		return err
	}
	destDir := filepath.Join(outputDir, pkg.apkg.Name)
	if err := os.RemoveAll(destDir); err != nil {
		return err
	}
	return os.Rename(tempOutDir, destDir)
}

func printPackage(pkg *Package, dir string) error {
	for filename, file := range pkg.apkg.Files {
		if strings.HasSuffix(filename, "_test.go") {
			continue
		}
		outfile := filepath.Join(dir, filepath.Base(filename))
		f, err := os.Create(outfile)
		if err != nil {
			return err
		}
		if err := format.Node(f, pkg.fset, file); err != nil {
			f.Close()
			return err
		}
		if err := f.Close(); err != nil {
			return err
		}
	}
	return nil
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

// An arg looks like
//    param:type
func parseBindingSpec(s string) (param, arg string, err error) {
	parts := strings.Split(s, ":")
	if len(parts) != 2 {
		return "", "", fmt.Errorf("bad spec %q", s)
	}
	param = parts[0]
	sub := parts[1]
	switch i := strings.LastIndex(sub, "."); {
	case i < 0:
		arg = sub
	case i == 0:
		return "", "", fmt.Errorf("empty import path in spec %q", s)
	default:
		path, name := sub[:i], sub[i+1:]
		if path[0] != '"' {
			path = strconv.Quote(path)
		}
		arg = path + "." + name
	}
	return param, arg, nil
}

type Binding struct {
	param *types.TypeName
	arg   types.Type
	found bool
}

func newBinding(paramName string, argType types.Type, pkg *Package) (*Binding, error) {
	gtn, err := pkg.topLevelTypeName(paramName)
	if err != nil {
		return nil, err
	}
	if err := checkParam(gtn, pkg); err != nil {
		return nil, fmt.Errorf("%s: %v", pkg.fset.Position(gtn.Pos()), err)
	}
	if err := checkBinding(gtn.Type(), argType); err != nil {
		return nil, fmt.Errorf("%s: %v", pkg.fset.Position(gtn.Pos()), err)
	}
	return &Binding{
		param: gtn,
		arg:   argType,
	}, nil
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
			gtn.Name(), name, pkg.fset.Position(node.Pos()))
	}
	if node == nil && has {
		log.Printf("param %s includes gen.%s, but does not need it", gtn.Name(), name)
	}
	return ""
}

// Check that a binding is valid: that the arg can be substituted for the param.
func checkBinding(ptype, atype types.Type) error {
	putype := ptype.Underlying().(*types.Interface)
	iface := newSubInterface(putype, ptype, atype)
	augType := augmentedType(atype)
	method, wrongType := types.MissingMethod(augType, iface, true)
	if method != nil {
		var msg string
		if wrongType {
			msg = "has wrong type"
		} else {
			msg = "is missing"
		}
		return missingMethodError(fmt.Sprintf("%s does not implement %s: method %s %s", atype, ptype,
			method.Name(), msg))
	}
	if implementsSpecialInterface(putype, "Comparable") && !types.Comparable(atype) {
		return needsComparableError(fmt.Sprintf("%s is not comparable but %s requires it", atype, ptype))
	}
	if implementsSpecialInterface(putype, "Nillable") && !hasNil(atype) {
		return needsNillableError(fmt.Sprintf("%s is not nillable but %s requires it", atype, ptype))
	}
	return nil
}

// hasNil reports whether a type includes the nil value.
// Code from https://golang.org/src/go/types/predicates.go.
func hasNil(typ types.Type) bool {
	switch t := typ.Underlying().(type) {
	case *types.Basic:
		return t.Kind() == types.UnsafePointer
	case *types.Slice, *types.Pointer, *types.Signature, *types.Interface, *types.Map, *types.Chan:
		return true
	}
	return false
}

// augmentedType creates a new type that may add methods to t.
// Comparable types behave like they implement Equal(T) bool (if they don't already).
// Ordered types (which are only basic types) behave like they implement Less(T)  bool,
// Greater(T) bool, LessEqual(T) bool and GreaterEqual(T) bool.
func augmentedType(t types.Type) types.Type {
	parm := func(t types.Type) *types.Var {
		return types.NewParam(token.NoPos, nil, "", t)
	}
	sig := types.NewSignature(parm(t), types.NewTuple(parm(t)), types.NewTuple(parm(types.Typ[types.Bool])), false)
	var methods []*types.Func
	for _, am := range augmentedMethods(t) {
		methods = append(methods, types.NewFunc(token.NoPos, nil, am.name, sig))
	}
	tn := types.NewTypeName(token.NoPos, nil, "AUGMENTED", nil)
	u := t.Underlying()
	if _, ok := t.(*types.Named); ok {
		// Use embedding to avoid losing the methods from the original type.
		u = types.NewStruct([]*types.Var{types.NewField(token.NoPos, nil, "", t, true)}, nil)
	}
	return types.NewNamed(tn, u, methods)
}

type augmentMethod struct {
	name string      // e.g. Equal
	tok  token.Token // e.g. ==
}

var (
	equalMethod    = augmentMethod{"Equal", token.EQL}
	orderedMethods = []augmentMethod{
		{"Less", token.LSS},
		{"Greater", token.GTR},
		{"LessEqual", token.LEQ},
		{"GreaterEqual", token.GEQ},
	}
)

func augmentedMethods(t types.Type) []augmentMethod {
	var ams []augmentMethod
	mset := types.NewMethodSet(t)
	if types.Comparable(t) {
		if mset.Lookup(nil, "Equal") != nil {
			log.Printf("not augmenting comparable type %s with Equal because it already has an equal method", t)
		} else {
			ams = append(ams, equalMethod)
		}
	}
	if bt, ok := t.(*types.Basic); ok && (bt.Info()&types.IsOrdered != 0) {
		ams = append(ams, orderedMethods...)
	}
	return ams
}

type Package struct {
	fset *token.FileSet
	apkg *ast.Package
	tpkg *types.Package
	info *types.Info
}

func (p *Package) String() string {
	return fmt.Sprintf("%s (%s)", p.tpkg.Name(), p.tpkg.Path())
}

func (p *Package) topLevelTypeName(name string) (*types.TypeName, error) {
	gobj := p.tpkg.Scope().Lookup(name)
	if gobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", name, p)
	}
	gtn, ok := gobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", name, p)
	}
	return gtn, nil
}

func astPackageFromPath(fset *token.FileSet, ipath string) (*ast.Package, error) {
	pwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	bpkg, err := build.Import(ipath, pwd, build.FindOnly)
	if err != nil {
		return nil, err
	}
	return astPackageFromDir(fset, bpkg.Dir)
}

func astPackageFromDir(fset *token.FileSet, dir string) (*ast.Package, error) {
	pkgs, err := parser.ParseDir(fset, dir, nil, parser.ParseComments)
	if err != nil {
		return nil, err
	}
	if len(pkgs) == 1 {
		for _, p := range pkgs {
			return p, nil
		}
		panic("impossible")
	} else {
		pkgName := filepath.Base(dir)
		apkg := pkgs[pkgName]
		if apkg == nil {
			return nil, fmt.Errorf("can't find package %q in directory %q", pkgName, dir)
		}
		return apkg, nil
	}
}

func makePackage(fset *token.FileSet, ipath string, apkg *ast.Package) (*Package, error) {
	var files []*ast.File
	for _, file := range apkg.Files {
		files = append(files, file)
	}
	info := &types.Info{
		Defs:  make(map[*ast.Ident]types.Object),
		Types: make(map[ast.Expr]types.TypeAndValue),
	}
	tpkg, err := typecheck(ipath, fset, files, info)
	if err != nil {
		fmt.Printf("typechecker on %s: %v", ipath, err)
		return nil, err
	}
	return &Package{
		fset: fset,
		apkg: apkg,
		tpkg: tpkg,
		info: info,
	}, nil
}

type importDirective struct {
	name         string
	path         string
	bindingSpecs []string
	file         *ast.File
}

// An import directive is of the form "gen:import bindingSpec...".
// An import directive must be on the same line as the import spec.
func parseDirectives(fset *token.FileSet, p *ast.Package) ([]importDirective, error) {
	var ids []importDirective
	var err error
	for _, file := range p.Files {
		cmap := ast.NewCommentMap(fset, file, file.Comments)
		ast.Inspect(file, func(n ast.Node) bool {
			var ispec *ast.ImportSpec
			switch n := n.(type) {
			case *ast.ImportSpec:
				ispec = n
			case *ast.GenDecl:
				// In the case of a single import statement, the comment is attached to
				// the GenDecl node.
				if len(n.Specs) == 1 && cmap[n] != nil {
					if is, ok := n.Specs[0].(*ast.ImportSpec); ok {
						ispec = is
					}
				}
			}
			if ispec == nil {
				return true
			}
			cgs := cmap[n]
			if cgs == nil && ispec.Comment != nil {
				cgs = []*ast.CommentGroup{ispec.Comment}
			}
			if cgs == nil {
				return true
			}
			bspecs, err2 := extractBindingSpecs(cgs)
			if err2 != nil {
				err = fmt.Errorf("%s: %v", fset.Position(ispec.Pos()), err)
				return false
			}
			if bspecs != nil {
				if ispec.Name == nil {
					err = fmt.Errorf("%s: %s: generic import must have a name", fset.Position(ispec.Pos()),
						ispec.Path.Value)
					return false
				}
				ids = append(ids, importDirective{
					name:         ispec.Name.Name,
					path:         importPath(ispec),
					bindingSpecs: bspecs,
					file:         file,
				})
			}
			return true
		})
	}
	return ids, err
}

func extractBindingSpecs(cgroups []*ast.CommentGroup) ([]string, error) {
	const directiveName = "gen:import"
	var result []string
	for _, g := range cgroups {
		for _, c := range g.List {
			txt := strings.Trim(c.Text, "/* \t")
			fields := strings.Fields(txt)
			if len(fields) == 0 {
				continue
			}
			if fields[0] != directiveName {
				continue
			}
			if result != nil {
				return nil, fmt.Errorf("more than one associated set of bindings: %v and %v",
					result, fields[1:])
			}
			result = fields[1:]
		}
	}
	return result, nil
}

// Modifies the asts in pkg. pkgName is the new package name.
func substitutePackage(pkg *Package, bindings []*Binding, pkgName string) error {
	var rws []rewrite
	for _, b := range bindings {
		for _, am := range augmentedMethods(b.arg) {
			rws = append(rws, rewrite{
				argType:    b.param.Type(),
				methodName: am.name,
				op:         am.tok,
			})
		}
	}
	pkg.apkg.Name = pkgName
	for filename, file := range pkg.apkg.Files {
		// Skip test files. They probably have concrete implementations of the
		// type parameters, so we can't specialize them.
		if strings.HasSuffix(filename, "_test.go") {
			continue
		}
		if err := substituteFile(filename, bindings, rws, pkg, file, pkgName); err != nil {
			return err
		}
	}
	return nil
}

func substituteFile(filename string, bindings []*Binding, rewrites []rewrite, pkg *Package, file *ast.File, name string) error {
	for _, b := range bindings {
		paramPath, _ := astutil.PathEnclosingInterval(file, b.param.Pos(), b.param.Pos())
		if len(paramPath) < 2 {
			// Type decl for param is not in this file.
			continue
		}
		b.found = true
		typeSpec := paramPath[1].(*ast.TypeSpec)
		// Add an import for the argument type if necessary.
		// TODO: the named type from another package might be embedded in the type, like map[int]geo.Point.
		if named, ok := b.arg.(*types.Named); ok {
			tn := named.Obj()
			name := tn.Pkg().Name()
			ipath := tn.Pkg().Path()
			if name == path.Base(ipath) {
				name = ""
			}
			astutil.AddNamedImport(pkg.fset, file, name, ipath)
		}
		// Turn the type spec into an alias if it isn't already.
		if !typeSpec.Assign.IsValid() {
			typeSpec.Assign = typeSpec.Type.Pos()
		}
		typeSpec.Type = typeToExpr(b.arg, pkg.tpkg)
	}
	file.Name.Name = name
	if err := replaceCode(file, bindings, rewrites, pkg); err != nil {
		return err
	}
	for _, impgrp := range astutil.Imports(pkg.fset, file) {
		for _, impspec := range impgrp {
			path := importPath(impspec)
			if !astutil.UsesImport(file, path) {
				name := ""
				if impspec.Name != nil {
					name = impspec.Name.Name
				}
				astutil.DeleteNamedImport(pkg.fset, file, name, path)
			}
		}
	}
	return nil
}

// importPath returns the unquoted import path of s,
// or "" if the path is not properly quoted.
// Copied from astutil.
func importPath(s *ast.ImportSpec) string {
	t, err := strconv.Unquote(s.Path.Value)
	if err == nil {
		return t
	}
	return ""
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

type GenericParam struct {
	typeSpec *ast.TypeSpec
	typ      types.Type
}

// Substitute atype for occurrences of the param type in iface, returning a new interface type.
func newSubInterface(iface *types.Interface, param, arg types.Type) *types.Interface {
	var nms []*types.Func
	for i := 0; i < iface.NumMethods(); i++ {
		m := iface.Method(i)
		sig := m.Type().(*types.Signature)
		params := subTuple(sig.Params(), param, arg)
		results := subTuple(sig.Results(), param, arg)
		nsig := types.NewSignature(sig.Recv(), params, results, sig.Variadic())
		nm := types.NewFunc(m.Pos(), m.Pkg(), m.Name(), nsig)
		nms = append(nms, nm)
	}
	ni := types.NewInterface(nms, nil)
	return ni.Complete()
}

func subTuple(t *types.Tuple, old, new types.Type) *types.Tuple {
	var vs []*types.Var
	for i := 0; i < t.Len(); i++ {
		v := t.At(i)
		if types.Identical(v.Type(), old) {
			v = types.NewVar(v.Pos(), v.Pkg(), v.Name(), new)
		}
		vs = append(vs, v)
	}
	return types.NewTuple(vs...)
}

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
					err = fmt.Errorf("%s: failed type assertion", pkg.fset.Position(n.Pos()))
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

// Given a list of case clauses from a type swtich, return the case clause that
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

func replaceTwoValueAssert(e *ast.TypeAssertExpr, bindings []*Binding, pkg *Package) []ast.Expr {
	for _, b := range bindings {
		if !types.Identical(pkg.info.Types[e.X].Type, b.param.Type()) {
			continue
		}
		etype := pkg.info.Types[e.Type].Type
		if types.Identical(etype, b.arg) {
			return []ast.Expr{e.X, id("true")}
		} else {
			return []ast.Expr{zeroExpr(etype, pkg.tpkg), id("false")}
		}
	}
	return []ast.Expr{e}
}

// If t needs to implement Comparable in pkg, return a node that proves it.
// Else return nil.
func comparableNode(t types.Type, pkg *Package) ast.Node {
	typeOf := func(e ast.Expr) types.Type {
		return pkg.info.Types[e].Type
	}
	var result ast.Node
	for _, file := range pkg.apkg.Files {
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

	for _, file := range pkg.apkg.Files {
		ast.Walk(withSig(nil), file)
		if result != nil {
			return result
		}
	}
	return nil
}

type visitorFunc func(ast.Node) ast.Visitor

func (v visitorFunc) Visit(n ast.Node) ast.Visitor { return v(n) }

const genImportPath = "github.com/jba/gen"

func implementsSpecialInterface(iface *types.Interface, name string) bool {
	for i := 0; i < iface.NumEmbeddeds(); i++ {
		tn := iface.Embedded(i).Obj()
		if tn.Name() == name && tn.Pkg().Path() == genImportPath {
			return true
		}
		if implementsSpecialInterface(iface.Embedded(i).Underlying().(*types.Interface), name) {
			return true
		}
	}
	return false
}

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

func processImportDirectives(outputDir string, ids []importDirective, fset *token.FileSet, bindings map[string]types.Type) error {
	ipath := dirToImportPath(outputDir)
	if ipath == "" {
		return fmt.Errorf("cannot map directory %s to an import path", outputDir)
	}
	seen := map[string]bool{}
	for _, id := range ids {
		key := id.name + ";" + id.path
		if !seen[key] {
			seen[key] = true
			bindings2, err := makeBindingMap(id.bindingSpecs, func(name string) types.Type {
				if t := bindings[name]; t != nil {
					return t
				}
				return lookupBuiltinName(name)
			})
			if err != nil {
				return err
			}
			if err := runWithBindings(id.path, outputDir, id.name, bindings2); err != nil {
				return err
			}
		}
		if err := instantiateImportPaths(fset, id.file, id.name, id.path, ipath+"/"+id.name); err != nil {
			return err
		}
	}
	return nil
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

func instantiateImportPaths(fset *token.FileSet, file *ast.File, name, gpath, ipath string) error {
	astutil.DeleteImport(fset, file, gpath)
	astutil.AddNamedImport(fset, file, name, ipath)
	return nil
}

func reloadAST(fset *token.FileSet, apkg *ast.Package) (*token.FileSet, *ast.Package, error) {
	fset2 := token.NewFileSet()
	apkg2 := &ast.Package{
		Name:  apkg.Name,
		Files: make(map[string]*ast.File),
	}
	for filename, file := range apkg.Files {
		var buf bytes.Buffer
		if err := format.Node(&buf, fset, file); err != nil {
			return nil, nil, err
		}
		file2, err := parser.ParseFile(fset2, filename, &buf, parser.ParseComments)
		if err != nil {
			return nil, nil, err
		}
		apkg2.Files[filename] = file2
	}
	return fset2, apkg2, nil
}

// buildType constructs a types.Type from a string expression that should
// denote a type. In the string, import paths with slashes must be quoted,
// and array lengths must be literal integers.
// lookupName returns the type for an unqualified name.
func buildType(s string, lookupName func(string) types.Type) (types.Type, error) {
	expr, err := parser.ParseExpr(s)
	if err != nil {
		return nil, err
	}
	return exprToType(expr, lookupName)
}

func lookupBuiltinName(name string) types.Type {
	for _, b := range types.Typ {
		if b.Name() == name && b.Kind() != types.UnsafePointer && b.Info()&types.IsUntyped == 0 {
			return b
		}
	}
	return nil
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

func lit(kind token.Token, s string) *ast.BasicLit {
	return &ast.BasicLit{Kind: kind, Value: s}
}

func id(name string) *ast.Ident {
	return &ast.Ident{Name: name}
}
