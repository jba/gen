// TODO
// - Support unnamed types, like []geo.Point or map[string]bool
// - Fix bug with augmentedType, where by creating a new type with an Equals method, we
//   lose all the methods of the original type. Maybe embed?
//    test: go run gen.go -g github.com/jba/gen/examples/slices -o $HOME/go/src/github.com/jba/gen/output \
//                 -p timeslices T:time.Time
// - Eq interface for ==.
//   Now, if we gen example/maps with a non-comparable type like regexp.Regexp, gen succeeds even
//   though the resulting instantiation doesn't compile. Require Eq for map keys (as well as explicit uses of ==).
// - CanNil interface for nil
// - type assertions
// - a generic package importing other generic packages

package main

import (
	"bytes"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/printer"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
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

	gpkg, err := loadPackage(*genPath)
	if err != nil {
		log.Fatal(err)
	}
	argImporter := compiledThenSourceImporter{
		importer.Default(),
		importer.For("source", nil),
	}
	var bindings []*Binding
	for _, arg := range flag.Args() {
		bs, err := parseBindingSpec(arg)
		if err != nil {
			log.Fatal(err)
		}
		b, err := specToBinding(bs, gpkg, argImporter)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			os.Exit(1)
		}
		bindings = append(bindings, b)
	}
	if len(bindings) == 0 {
		log.Fatal("need bindings")
	}
	if err := substitutePackage(gpkg, bindings, *outputName); err != nil {
		log.Fatal(err)
	}
	if err := os.MkdirAll(*outputDir, os.ModePerm); err != nil {
		log.Fatal(err)
	}
	tempOutDir, err := ioutil.TempDir(*outputDir, "gen-")
	if err != nil {
		log.Fatal(err)
	}
	if err := printPackage(gpkg, tempOutDir); err != nil {
		if err := os.Remove(tempOutDir); err != nil {
			log.Printf("removing %s: %v", tempOutDir, err)
		}
		log.Fatal(err)
	}
	destDir := filepath.Join(*outputDir, gpkg.apkg.Name)
	if err := os.RemoveAll(destDir); err != nil {
		log.Fatal(err)
	}
	if err := os.Rename(tempOutDir, destDir); err != nil {
		log.Fatal(err)
	}
	fmt.Printf("wrote to %s\n", destDir)
}

func requireFlag(flag, val string) {
	if val == "" {
		log.Fatalf("need -%s", flag)
	}
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

func specToBinding(spec *BindingSpec, gpkg *Package, imp types.Importer) (*Binding, error) {
	gobj := gpkg.tpkg.Scope().Lookup(spec.param)
	if gobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", spec.param, gpkg.tpkg.Name())
	}
	gtn, ok := gobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", spec.param, gpkg)
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
		tpkg, err := imp.Import(spec.argImportPath)
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
	if err := checkBinding(gtn, atype); err != nil {
		return nil, err
	}

	exprs := eqExprs(gtn, gpkg)
	has := hasEq(gtn.Type().Underlying().(*types.Interface))
	if len(exprs) > 0 && !has {
		var buf bytes.Buffer
		fmt.Fprintf(&buf, "param %s does not include gen.Eq, but == is used or implied at:\n", gtn.Name())
		for _, e := range exprs {
			fmt.Fprintf(&buf, "  %s: ", gpkg.fset.Position(e.Pos()))
			printer.Fprint(&buf, gpkg.fset, e)
			fmt.Fprintln(&buf)
		}

		return nil, errors.New(buf.String())
	}
	if len(exprs) == 0 && has {
		log.Printf("param %s includes gen.Eq, but does not need it", gtn.Name())
	}

	return &Binding{
		param: gtn,
		arg:   atype,
	}, nil
}

// Check that a binding is valid: that the arg can be substituted for the param.
func checkBinding(param *types.TypeName, atype types.Type) error {
	ptype := param.Type()
	switch putype := ptype.Underlying().(type) {
	case *types.Interface:
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
		if hasEq(ptype.Underlying().(*types.Interface)) && !types.Comparable(atype) {
			return fmt.Errorf("%s is not comparable but %s requires it", atype, ptype)
		}
		return nil
	default:
		return fmt.Errorf("type of param %s must be interface, not %T\n", param.Name(), ptype)
	}

	return nil
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

// Comparable types behave like they implement Equal(T) bool (if they don't already).
// Ordered types (which are only basic types) behave like they implement Less(T) bool, Greater(T) bool, LessEqual(T) bool and GreaterEqual(T) bool.
func augmentedType(t types.Type) types.Type {
	parm := func(t types.Type) *types.Var {
		return types.NewParam(token.NoPos, nil, "", t)
	}
	sig := types.NewSignature(parm(t), types.NewTuple(parm(t)), types.NewTuple(parm(types.Typ[types.Bool])), false)
	var methods []*types.Func
	for _, am := range augmentedMethods(t) {
		methods = append(methods, types.NewFunc(token.NoPos, nil, am.name, sig))
	}
	// TODO: I don't think this works for non-basic comparable types. By defining a new type,
	// we lose the method set of the original type.
	return types.NewNamed(types.NewTypeName(token.NoPos, nil, "TYPENAME", t), t.Underlying(), methods)
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

func loadPackage(path string) (*Package, error) {
	dir := filepath.Join(os.Getenv("HOME"), "go/src", path)
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, dir, nil, parser.ParseComments)
	if err != nil {
		return nil, err
	}
	var apkg *ast.Package
	if len(pkgs) == 1 {
		for _, p := range pkgs {
			apkg = p
			break
		}
	} else {
		pkgName := filepath.Base(dir)
		apkg = pkgs[pkgName]
		if apkg == nil {
			return nil, fmt.Errorf("can't find package %q in directory %q", pkgName, dir)
		}
	}
	var files []*ast.File
	for _, file := range apkg.Files {
		files = append(files, file)
	}
	info := &types.Info{
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
		Types: make(map[ast.Expr]types.TypeAndValue),
	}
	conf := types.Config{Importer: compiledThenSourceImporter{
		importer.Default(),
		importer.For("source", nil),
	}}
	tpkg, err := conf.Check(path, fset, files, info)
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
			addImport(file, tn.Pkg().Name(), tn.Pkg().Path())
		}
		// Turn the type spec into an alias if it isn't already.
		if !typeSpec.Assign.IsValid() {
			typeSpec.Assign = typeSpec.Type.Pos()
		}
		typeSpec.Type = exprForType(b.arg)
	}
	return replaceCode(file, rewrites, info)
}

func addImport(file *ast.File, name, path string) {
	if len(file.Decls) == 0 {
		file.Decls = []ast.Decl{&ast.GenDecl{Tok: token.IMPORT}}
	} else if d, ok := file.Decls[0].(*ast.GenDecl); !ok || d.Tok != token.IMPORT {
		ds := make([]ast.Decl, len(file.Decls)+1)
		copy(ds[1:], file.Decls)
		ds[0] = &ast.GenDecl{Tok: token.IMPORT}
		file.Decls = ds
	}
	d := file.Decls[0].(*ast.GenDecl)
	var nameID *ast.Ident
	if name != filepath.Base(path) {
		nameID = &ast.Ident{Name: name}
	}
	d.Specs = append(d.Specs, &ast.ImportSpec{
		Name: nameID,
		Path: &ast.BasicLit{Value: `"` + path + `"`, Kind: token.STRING},
	})
	if len(d.Specs) > 1 && !d.Lparen.IsValid() {
		d.Lparen = file.Package
	}
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

// Find every expression that assumes param has ==.
func eqExprs(param *types.TypeName, pkg *Package) []ast.Expr {
	var exprs []ast.Expr
	ptype := param.Type()
	typeOf := func(e ast.Expr) types.Type {
		return pkg.info.Types[e].Type
	}

	// TODO: making a map[T]X, even if it's never indexed?
	for _, file := range pkg.apkg.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.BinaryExpr:
				if (n.Op == token.EQL || n.Op == token.NEQ) &&
					(types.Identical(typeOf(n.X), ptype) || types.Identical(typeOf(n.Y), ptype)) {
					exprs = append(exprs, n)
					return false
				}
				return true // children of this binary expression might still match

			case *ast.IndexExpr:
				if _, ok := typeOf(n.X).(*types.Map); ok && types.Identical(typeOf(n.Index), ptype) {
					exprs = append(exprs, n)
					return false
				}
				return true
			}
			return true
		})
	}
	return exprs
}

const eqImportPath = "github.com/jba/gen"

func hasEq(iface *types.Interface) bool {
	for i := 0; i < iface.NumEmbeddeds(); i++ {
		tn := iface.Embedded(i).Obj()
		if tn.Name() == "Eq" && tn.Pkg().Path() == eqImportPath {
			return true
		}
		if hasEq(iface.Embedded(i).Underlying().(*types.Interface)) {
			return true
		}
	}
	return false
}
