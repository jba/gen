// TODO
// - Support unnamed types for instantiation, like []geo.Point or map[string]bool
//   When we do this, see if we have to check ourselves that K is comparable in map[K]V, or if go/types
//   will report an error for us.
// - Fix bug with augmentedType, where by creating a new type with an Equals method, we
//   lose all the methods of the original type. Maybe embed?
//    test: go run gen.go -g github.com/jba/gen/examples/slices -o $HOME/go/src/github.com/jba/gen/output \
//                 -p timeslices T:time.Time
// - gen.Nillable interface for nil
// - type assertions
// - a generic package importing other generic packages (ones with interface definitions)
// - When a generic param has the Comparable constraint, allow instantiations of interface types.
//   That mirrors the compiler, which allows interface types in == and map indexing and defers the comparable
//   check to runtime.
// - Given
//     type T interface{}
//     type U T
//   the types package loses the information that U was defined in terms of T. So we can't do a comparability
//   check correctly: map[U]int will succeed even though we are assuming T is not comparable. I guess I'll
//   have to use the ast to recognize this case.

// examples (for readme):
// - container/ring
// - google/btree
// - pubsub/pullstream
// - lru.Cache?
// - sync.Map?

package main

import (
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
	gpkg, err := loadPackage(genPath)
	if err != nil {
		return err
	}
	var bindings []*Binding
	for _, arg := range args {
		bs, err := parseBindingSpec(arg)
		if err != nil {
			return err
		}
		b, err := specToBinding(bs, gpkg)
		if err != nil {
			return err
		}
		bindings = append(bindings, b)
	}
	if len(bindings) == 0 {
		return errors.New("need bindings")
	}
	if err := substitutePackage(gpkg, bindings, outputName); err != nil {
		return err
	}
	if err := os.MkdirAll(outputDir, os.ModePerm); err != nil {
		return err
	}
	tempOutDir, err := ioutil.TempDir(outputDir, "gen-")
	if err != nil {
		return err
	}
	if err := printPackage(gpkg, tempOutDir); err != nil {
		if err := os.Remove(tempOutDir); err != nil {
			log.Printf("removing %s: %v", tempOutDir, err)
		}
		return err
	}
	destDir := filepath.Join(outputDir, gpkg.apkg.Name)
	if err := os.RemoveAll(destDir); err != nil {
		return err
	}
	if err := os.Rename(tempOutDir, destDir); err != nil {
		return err
	}
	return nil
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

var typecheck = (&types.Config{Importer: theImporter}).Check

type BindingSpec struct {
	param         string
	argImportPath string
	argTypeName   string
}

// An arg looks like
//    param:type
func parseBindingSpec(s string) (*BindingSpec, error) {
	parts := strings.Split(s, ":")
	if len(parts) != 2 {
		return nil, fmt.Errorf("bad spec %q", s)
	}
	sa := &BindingSpec{param: parts[0]}
	sub := parts[1]
	i := strings.LastIndex(sub, ".")
	if i < 0 {
		sa.argImportPath = ""
		sa.argTypeName = sub
	} else {
		sa.argImportPath, sa.argTypeName = sub[:i], sub[i+1:]
	}
	return sa, nil
}

type Binding struct {
	param    *types.TypeName
	arg      types.Type
	rewrites []*rewrite
}

func specToBinding(spec *BindingSpec, gpkg *Package) (*Binding, error) {
	gtn, err := gpkg.topLevelTypeName(spec.param)
	if err != nil {
		return nil, err
	}
	if err := checkParam(gtn, gpkg); err != nil {
		return nil, fmt.Errorf("%s: %v", gpkg.fset.Position(gtn.Pos()), err)
	}
	var atype types.Type
	if spec.argImportPath == "" {
		for _, bt := range types.Typ {
			if spec.argTypeName == bt.Name() {
				atype = bt
				break
			}
		}
		if atype == nil {
			return nil, fmt.Errorf("no basic type %q", spec.argTypeName)
		}
	} else {
		tpkg, err := theImporter.Import(spec.argImportPath)
		if err != nil {
			return nil, err
		}
		sobj := tpkg.Scope().Lookup(spec.argTypeName)
		if sobj == nil {
			return nil, fmt.Errorf("cannot find %s in package %s", spec.argTypeName, tpkg.Name())
		}
		stn, ok := sobj.(*types.TypeName)
		if !ok {
			return nil, fmt.Errorf("%s is not a named type in package %s", spec.argTypeName, tpkg.Path())
		}
		atype = stn.Type()
	}
	if err := checkBinding(gtn.Type(), atype); err != nil {
		return nil, fmt.Errorf("%s: %v", gpkg.fset.Position(gtn.Pos()), err)
	}

	return &Binding{
		param: gtn,
		arg:   atype,
	}, nil
}

type needsComparableError string

func (e needsComparableError) Error() string { return string(e) }

type needsNillableError string

func (e needsNillableError) Error() string { return string(e) }

type notInterfaceError string

func (e notInterfaceError) Error() string { return string(e) }

func checkParam(gtn *types.TypeName, gpkg *Package) error {
	putype, ok := gtn.Type().Underlying().(*types.Interface)
	if !ok {
		return notInterfaceError(fmt.Sprintf("type must be interface, not %T\n", gtn.Type()))
	}

	if msg := checkSpecialInterface("Comparable", gtn, putype, gpkg, comparableNode); msg != "" {
		return needsComparableError(msg)
	}
	if msg := checkSpecialInterface("Nillable", gtn, putype, gpkg, nillableNode); msg != "" {
		return needsNillableError(msg)
	}
	return nil
}

func checkSpecialInterface(name string, gtn *types.TypeName, iface *types.Interface, gpkg *Package, finder func(types.Type, *Package) ast.Node) string {
	node := finder(gtn.Type(), gpkg)
	has := implementsSpecialInterface(iface, name)
	if node != nil && !has {
		return fmt.Sprintf("param %s does not implement gen.%s, but it is required at %s",
			gtn.Name(), name, gpkg.fset.Position(node.Pos()))
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
		return fmt.Errorf("%s does not implement %s: method %s %s", atype, ptype,
			method.Name(), msg)
	}
	if implementsSpecialInterface(putype, "Comparable") && !types.Comparable(atype) {
		return fmt.Errorf("%s is not comparable but %s requires it", atype, ptype)
	}
	if implementsSpecialInterface(putype, "Nillable") &&
	// TODO: check Nillable
	return nil
}

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

func augmentedMethods(t types.Type) []augmentMethod {
	var ams []augmentMethod
	mset := types.NewMethodSet(t)
	if types.Comparable(t) {
		if mset.Lookup(nil, "Equal") != nil {
			log.Printf("not augmenting comparable type %s with Equal because it already has an equal method", t)
		} else {
			ams = append(ams, augmentMethod{"Equal", token.EQL})
		}
	}
	if bt, ok := t.(*types.Basic); ok && (bt.Info()&types.IsOrdered != 0) {
		ams = append(ams,
			augmentMethod{"Less", token.LSS},
			augmentMethod{"Greater", token.GTR},
			augmentMethod{"LessEqual", token.LEQ},
			augmentMethod{"GreaterEqual", token.GEQ})
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

func loadPackage(ipath string) (*Package, error) {
	pwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}
	bpkg, err := build.Import(ipath, pwd, build.FindOnly)
	if err != nil {
		return nil, err
	}
	dir := bpkg.Dir
	fset := token.NewFileSet()
	apkg, err := astPackageFromDir(fset, dir)
	if err != nil {
		return nil, err
	}
	return makePackage(fset, ipath, apkg)
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
		return nil, err
	}
	return &Package{
		fset: fset,
		apkg: apkg,
		tpkg: tpkg,
		info: info,
	}, nil
}

func astPackageFromDir(fset *token.FileSet, dir string) (*ast.Package, error) {
	pkgs, err := parser.ParseDir(fset, dir, nil, 0)
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

// Modifies the asts in gpkg. pkgName is the new package name.
func substitutePackage(gpkg *Package, bindings []*Binding, pkgName string) error {
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
	gpkg.apkg.Name = pkgName
	for filename, file := range gpkg.apkg.Files {
		// Skip test files. They probably have concrete implementations of the
		// type parameters, so we can't specialize them.
		if strings.HasSuffix(filename, "_test.go") {
			continue
		}
		if err := substituteFile(filename, bindings, rws, gpkg.info, gpkg.fset, file, pkgName); err != nil {
			return err
		}
	}
	return nil
}

func substituteFile(filename string, bindings []*Binding, rewrites []rewrite, info *types.Info, fset *token.FileSet, file *ast.File, name string) error {
	file.Name.Name = name
	for _, b := range bindings {
		paramPath, _ := astutil.PathEnclosingInterval(file, b.param.Pos(), b.param.Pos())
		if len(paramPath) < 2 {
			continue
		}
		typeSpec := paramPath[1].(*ast.TypeSpec)
		if named, ok := b.arg.(*types.Named); ok {
			tn := named.Obj()
			name := tn.Pkg().Name()
			ipath := tn.Pkg().Path()
			if name == path.Base(ipath) {
				name = ""
			}
			astutil.AddNamedImport(fset, file, name, ipath)
		}
		// Turn the type spec into an alias if it isn't already.
		if !typeSpec.Assign.IsValid() {
			typeSpec.Assign = typeSpec.Type.Pos()
		}
		typeSpec.Type = exprForType(b.arg)
	}
	if err := replaceCode(file, rewrites, info); err != nil {
		return err
	}
	for _, impgrp := range astutil.Imports(fset, file) {
		for _, impspec := range impgrp {
			path := importPath(impspec)
			if !astutil.UsesImport(file, path) {
				name := ""
				if impspec.Name != nil {
					name = impspec.Name.Name
				}
				astutil.DeleteNamedImport(fset, file, name, path)
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
func exprForType(typ types.Type) ast.Expr {
	switch typ := typ.(type) {
	case *types.Named:
		return &ast.SelectorExpr{
			X:   &ast.Ident{Name: typ.Obj().Pkg().Name()},
			Sel: &ast.Ident{Name: typ.Obj().Name()},
		}
	case *types.Basic:
		return &ast.Ident{Name: typ.Name()}
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

func id(name string) *ast.Ident {
	return &ast.Ident{Name: name}
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

func replaceCode(file *ast.File, rewrites []rewrite, info *types.Info) error {
	replaceExpr(file, func(ep *ast.Expr) bool {
		switch e := (*ep).(type) {
		case *ast.CallExpr:
			for _, r := range rewrites {
				if x, ok := r.match(e.Fun, info); ok {
					*ep = &ast.BinaryExpr{
						X:  x,
						Op: r.op,
						Y:  e.Args[0],
					}
					break
				}
			}

		case *ast.SelectorExpr:
			// Not a call, because we would have caught that above. A method expression or value,
			// like t.Less or T.Less.
			for _, r := range rewrites {
				if x, ok := r.match(e, info); ok {
					name := r.argType.(*types.Named).Obj().Name()
					// The function denoted by e has one or two arguments.
					if info.Types[e].Type.(*types.Signature).Params().Len() == 2 {
						*ep = twoArgOpFuncLit(name, r.op)
					} else {
						*ep = oneArgOpFuncCall(x, name, r.op)
					}
					break
				}
			}
		}
		return true
	})
	return nil
}

func replaceExpr(n ast.Node, f func(*ast.Expr) bool) {
	rep := func(pe *ast.Expr) {
		if f(pe) {
			replaceExpr(*pe, f)
		}
	}

	elist := func(es []ast.Expr) {
		for i := range es {
			rep(&es[i])
		}
	}

	slist := func(ss []ast.Stmt) {
		for _, s := range ss {
			replaceExpr(s, f)
		}
	}

	dlist := func(ds []ast.Decl) {
		for _, d := range ds {
			replaceExpr(d, f)
		}
	}

	switch n := n.(type) {
	case *ast.AssignStmt:
		elist(n.Lhs)
		elist(n.Rhs)

	case *ast.BinaryExpr:
		rep(&n.X)
		rep(&n.Y)

	case *ast.BlockStmt:
		slist(n.List)

	case *ast.CallExpr:
		rep(&n.Fun)
		elist(n.Args)

	case *ast.CaseClause:
		elist(n.List)
		slist(n.Body)

	case *ast.CommClause:
		replaceExpr(n.Comm, f)
		slist(n.Body)

	case *ast.CompositeLit:
		rep(&n.Type)
		elist(n.Elts)

	case *ast.DeclStmt:
		replaceExpr(n.Decl, f)

	case *ast.DeferStmt:
		//TODO: what if we need to replace the call itself?
		// e.g. defer t.Less(u)
		replaceExpr(n.Call, f)

	case *ast.ExprStmt:
		rep(&n.X)

	case *ast.File:
		dlist(n.Decls)

	case *ast.ForStmt:
		replaceExpr(n.Init, f)
		rep(&n.Cond)
		replaceExpr(n.Post, f)
		replaceExpr(n.Body, f)

	case *ast.FuncDecl:
		replaceExpr(n.Body, f)

	case *ast.FuncLit:
		replaceExpr(n.Body, f)

	case *ast.GenDecl:
		for _, s := range n.Specs {
			replaceExpr(s, f)
		}

	case *ast.GoStmt:
		//TODO: what if we need to replace the call itself?
		// e.g. go t.Less(u)
		replaceExpr(n.Call, f)

	case *ast.IfStmt:
		replaceExpr(n.Init, f)
		rep(&n.Cond)
		replaceExpr(n.Body, f)
		replaceExpr(n.Else, f)

	case *ast.IncDecStmt:
		rep(&n.X)

	case *ast.IndexExpr:
		rep(&n.X)
		rep(&n.Index)

	case *ast.KeyValueExpr:
		rep(&n.Key)
		rep(&n.Value)

	case *ast.LabeledStmt:
		replaceExpr(n.Stmt, f)

	case *ast.ParenExpr:
		rep(&n.X)

	case *ast.RangeStmt:
		rep(&n.Key)
		rep(&n.Value)
		rep(&n.X)
		replaceExpr(n.Body, f)

	case *ast.ReturnStmt:
		elist(n.Results)

	case *ast.SelectStmt:
		replaceExpr(n.Body, f)

	case *ast.SelectorExpr:
		rep(&n.X)

	case *ast.SendStmt:
		rep(&n.Chan)
		rep(&n.Value)

	case *ast.SliceExpr:
		rep(&n.X)
		rep(&n.Low)
		rep(&n.High)
		rep(&n.Max)

	case *ast.StarExpr:
		rep(&n.X)

	case *ast.SwitchStmt:
		replaceExpr(n.Init, f)
		rep(&n.Tag)
		replaceExpr(n.Body, f)

	case *ast.TypeAssertExpr:
		rep(&n.X)
		// Don't do n.Type because it's a type.

	case *ast.TypeSwitchStmt:
		replaceExpr(n.Init, f)
		replaceExpr(n.Assign, f)
		replaceExpr(n.Body, f)

	case *ast.UnaryExpr:
		rep(&n.X)

	case *ast.ValueSpec:
		elist(n.Values)

		// Ignore all other node types.
	}
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
		sig := typeOf(n.Fun).(*types.Signature)
		for i, arg := range n.Args {
			if isNil(typeOf(arg)) && match(sig.Params().At(i).Type()) {
				return true
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

// buildType constructs a types.Type from a string expression that should
// denote a type. Array lengths must be literal integers.
func buildType(s string) (types.Type, error) {
	expr, err := parser.ParseExpr(s)
	if err != nil {
		return nil, err
	}
	return exprToType(expr)
}

func exprToType(expr ast.Expr) (types.Type, error) {
	switch e := expr.(type) {
	case *ast.Ident:
		for _, b := range types.Typ {
			if b.Name() == e.Name && b.Kind() != types.UnsafePointer && b.Info()&types.IsUntyped == 0 {
				return b, nil
			}
		}
		return nil, errors.New("can only handle basic types for now")
	case *ast.MapType:
		k, err := exprToType(e.Key)
		if err != nil {
			return nil, err
		}
		v, err := exprToType(e.Value)
		if err != nil {
			return nil, err
		}
		return types.NewMap(k, v), nil
	case *ast.ArrayType:
		elType, err := exprToType(e.Elt)
		if err != nil {
			return nil, err
		}
		if e.Len == nil {
			return types.NewSlice(elType), nil
		} else {
			bl, ok := e.Elt.(*ast.BasicLit)
			if !ok {
				return nil, errors.New("array len is not a literal integer")
			}
			length, err := strconv.ParseInt(bl.Value, 10, 64)
			if err != nil {
				return nil, fmt.Errorf("array length: %v", err)
			}
			return types.NewArray(elType, length), nil
		}
	default:
		return nil, fmt.Errorf("unknown type expr %T", e)
	}
}
