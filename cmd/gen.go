// TODO
// - Support types without packages, like string
// - Support unnamed types, like []geo.Point or map[string]bool

package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
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
	// gparams := genericParams(gpkg)
	// if len(gparams) != 1 {
	// 	log.Fatal("need exactly one generic param")
	// }
	var bindings []*Binding
	for _, arg := range flag.Args() {
		sa, err := parseSubstitutionArg(arg)
		if err != nil {
			log.Fatal(err)
		}
		b, err := argToBinding(sa, gpkg)
		if err != nil {
			log.Fatal(err)
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
		log.Fatal(err)
	}

	// if len(args) > 1 {
	// 	log.Fatal("only one for now")
	// }

	// if err := checkGenericBinding(gparams[0], typ); err != nil {
	// 	log.Fatalf("%s cannot be bound to %s: %v",
	// 		st, gparams[0].typeSpec.Name.Name, err)
	// }

	// ts := gparams[0].typeSpec
	// if !ts.Assign.IsValid() {
	// 	ts.Assign = ts.Type.Pos()
	// }
	// ts.Type = &ast.Ident{Name: stype}
	// for filename, file := range gpkg.apkg.Files {
	// 	fmt.Printf("==== %s ====\n", filename)
	// 	if err := printFile(os.Stdout, gpkg.fset, file); err != nil {
	// 		log.Fatalf("%s: %v", filename, err)
	// 	}
	// }
}

func requireFlag(flag, val string) {
	if val == "" {
		log.Fatalf("need -%s", flag)
	}
}

type SubstitutionArg struct {
	param       string
	subPath     string
	subTypename string
}

// An arg looks like
//    param:type
func parseSubstitutionArg(s string) (*SubstitutionArg, error) {
	parts := strings.Split(s, ":")
	if len(parts) != 2 {
		return nil, fmt.Errorf("bad spec %q", s)
	}
	sa := &SubstitutionArg{param: parts[0]}
	sub := parts[1]
	i := strings.LastIndex(sub, ".")
	if i < 0 {
		return nil, fmt.Errorf("%q must be path.type", sub)
	}
	sa.subPath, sa.subTypename = sub[:i], sub[i+1:]
	return sa, nil
}

type Binding struct {
	param *types.TypeName
	arg   *types.TypeName
}

func argToBinding(arg *SubstitutionArg, gpkg *Package) (*Binding, error) {
	gobj := gpkg.tpkg.Scope().Lookup(arg.param)
	if gobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", arg.param, gpkg.tpkg.Name())
	}
	gtn, ok := gobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", arg.param, gpkg)
	}
	spkg, err := loadPackage(arg.subPath)
	if err != nil {
		return nil, err
	}
	sobj := spkg.tpkg.Scope().Lookup(arg.subTypename)
	if sobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", arg.subTypename, spkg.tpkg.Name())
	}
	stn, ok := sobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", arg.subTypename, spkg)
	}
	if err := checkBinding(gtn, stn); err != nil {
		return nil, err
	}
	return &Binding{param: gtn, arg: stn}, nil
}

// Check that a binding is valid: that the arg can be substituted for the param.
func checkBinding(param, arg *types.TypeName) error {
	ptype := param.Type()
	atype := arg.Type()
	switch putype := ptype.Underlying().(type) {
	case *types.Interface:
		iface := newSubInterface(putype, ptype, atype)
		if !types.Implements(atype, iface) {
			return fmt.Errorf("%s.%s does not implement %s", arg.Pkg().Path(), arg.Name(), ptype)
		}
		return nil
	default:
		return fmt.Errorf("unknown underlying type: %T\n", ptype)
	}
	return nil
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
	conf := types.Config{Importer: importer.Default()}
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
	gpkg.apkg.Name = pkgName
	for filename, file := range gpkg.apkg.Files {
		if err := substituteFile(filename, bindings, gpkg.fset, file, pkgName); err != nil {
			return err
		}
	}
	return nil
}

func substituteFile(filename string, bindings []*Binding, fset *token.FileSet, file *ast.File, name string) error {
	file.Name.Name = name
	for _, b := range bindings {
		paramPath, _ := astutil.PathEnclosingInterval(file, b.param.Pos(), b.param.Pos())
		if len(paramPath) < 2 {
			continue
		}
		typeSpec := paramPath[1].(*ast.TypeSpec)
		addImport(file, b.arg.Pkg().Name(), b.arg.Pkg().Path())
		// Turn the type spec into an alias if it isn't already.
		if !typeSpec.Assign.IsValid() {
			typeSpec.Assign = typeSpec.Type.Pos()
		}
		typeSpec.Type = exprForType(b.arg.Type())
	}
	return nil
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
	d.Specs = append(d.Specs, &ast.ImportSpec{
		Name: &ast.Ident{Name: name},
		Path: &ast.BasicLit{Value: `"` + path + `"`, Kind: token.STRING},
	})
	if len(d.Specs) > 1 && !d.Lparen.IsValid() {
		d.Lparen = d.Specs[0].Pos()
	}
	//ast.Print(nil, file.Decls[0])
	//fmt.Printf("%#v\n", file.Decls[0].(*ast.GenDecl).Specs[0])
}

// Make an ast.Node that corresponds to the type.
func exprForType(typ types.Type) ast.Expr {
	switch typ := typ.(type) {
	case *types.Named:
		return &ast.SelectorExpr{
			X:   &ast.Ident{Name: typ.Obj().Pkg().Name()},
			Sel: &ast.Ident{Name: typ.Obj().Name()},
		}
	default:
		return nil
	}
}

type GenericParam struct {
	typeSpec *ast.TypeSpec
	typ      types.Type
}

// type Substitution struct {
// 	expr ast.Expr
// 	typ  types.Type
// }

// func genericParams(pkg *Package) []*GenericParam {
// 	var gps []*GenericParam
// 	for _, file := range pkg.apkg.Files {
// 		tspecs := genericTypeSpecs(file)
// 		for _, ts := range tspecs {
// 			gps = append(gps, &GenericParam{
// 				typeSpec: ts,
// 				typ:      pkg.info.Defs[ts.Name].Type(),
// 			})
// 		}
// 	}
// 	return gps
// }

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

// Collect all top-level type specs that have the right comment.
func genericTypeSpecs(file *ast.File) []*ast.TypeSpec {
	const want = "go:gen"
	var specs []*ast.TypeSpec
	ast.Inspect(file, func(n ast.Node) bool {
		if _, ok := n.(*ast.File); ok {
			return true
		}
		gd, ok := n.(*ast.GenDecl)
		if !ok || gd.Tok != token.TYPE {
			return false
		}
		if strings.TrimSpace(gd.Doc.Text()) == want {
			if len(gd.Specs) != 1 {
				log.Fatal("comment on type group with > 1 spec")
			}
			specs = append(specs, gd.Specs[0].(*ast.TypeSpec))
			return false
		}
		for _, spec := range gd.Specs {
			ts := spec.(*ast.TypeSpec)
			if strings.TrimSpace(ts.Doc.Text()) == want {
				specs = append(specs, ts)
			}
		}
		return false
	})
	// Skip alias check (for now) because "type T = interface{...}" fails with
	// recursive alias if the interface methods mention T.
	// for _, s := range specs {
	// 	if !s.Assign.IsValid() {
	// 		log.Fatalf("%s: not a type alias decl", s)
	// 	}
	// }
	return specs
}

func printPackage(pkg *Package, dir string) error {
	outdir := filepath.Join(dir, pkg.apkg.Name)
	if err := os.Mkdir(outdir, os.ModePerm); err != nil {
		return err
	}
	for filename, file := range pkg.apkg.Files {
		outfile := filepath.Join(outdir, filepath.Base(filename))
		fmt.Printf("writing to file %s\n", outfile)
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

// func printFile(w io.Writer, fset *token.FileSet, file *ast.File) error {
// 	conf := &printer.Config{
// 		Mode:     printer.UseSpaces,
// 		Tabwidth: 4,
// 	}
// 	return conf.Fprint(w, fset, file)
// }

// func cgtext(cgs []*ast.CommentGroup) string {
// 	var ss []string
// 	for _, cg := range cgs {
// 		ss = append(ss, cg.Text())
// 	}
// 	return strings.Join(ss, ", ")
// }

// ast.Inspect(file, func(n ast.Node) bool {
// 	if n == nil {
// 		level--
// 	} else {
// 		fmt.Printf("%snode %v (%T), comments %s\n", strings.Repeat("  ", level), n, n, cgtext(cm[n]))
// 		level++
// 	}
// 	return true
// })

// func processDir(dir string) error {
// 	fset := token.NewFileSet()
// 	pkgs, err := parser.ParseDir(fset, dir, nil, parser.ParseComments)
// 	if err != nil {
// 		return err
// 	}
// 	for _, pkg := range pkgs {
// 		var files []*ast.File
// 		for _, file := range pkg.Files {
// 			files = append(files, file)
// 		}
// 		info := &types.Info{
// 			Defs:  make(map[*ast.Ident]types.Object),
// 			Uses:  make(map[*ast.Ident]types.Object),
// 			Types: make(map[ast.Expr]types.TypeAndValue),
// 		}
// 		conf := types.Config{Importer: importer.Default()}
// 		_, err := conf.Check("PKG", fset, files, info)
// 		if err != nil {
// 			return err
// 		}
// 		for filename, file := range pkg.Files {
// 			err := processFile(filename, fset, file, info)
// 			if err != nil {
// 				return err
// 			}
// 			// if err := printFile(os.Stdout, fset, file); err != nil {
// 			// 	return err
// 			// }
// 		}
// 	}
// 	return nil
// }

// func processFile(filename string, fset *token.FileSet, file *ast.File, info *types.Info) error {
// 	fmt.Printf("== file %s ==\n", filename)
// 	tspecs := genericTypeSpecs(file)
// 	for _, ts := range tspecs {
// 		gparam := newGenericParam(ts, info)
// 		sub := &Substitution{
// 			expr: &ast.Ident{
// 				//NamePos: ts.Type.Pos(),
// 				Name: "int",
// 			},
// 			typ: types.Typ[types.Int],
// 		}
// 		if err := checkGenericBinding(gparam, sub); err != nil {
// 			return err
// 		}
// 		if !ts.Assign.IsValid() {
// 			ts.Assign = ts.Type.Pos()
// 		}
// 		ts.Type = sub.expr
// 	}
// 	return nil
// }
