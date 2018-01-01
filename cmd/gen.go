package main

import (
	"flag"
	"fmt"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/printer"
	"go/token"
	"go/types"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"
)

var (
	genPath = flag.String("g", "", "import path of generic package")
	subType = flag.String("s", "", "import/path.type of type to sub")
)

func main() {
	flag.Parse()
	if *genPath == "" || *subType == "" {
		log.Fatal("need -g and -s")
	}
	gpkg, err := loadPackage(*genPath)
	if err != nil {
		log.Fatal(err)
	}
	gparams := genericParams(gpkg)
	if len(gparams) != 1 {
		log.Fatal("need exactly one generic param")
	}
	i := strings.LastIndex(*subType, ".")
	if i < 0 {
		log.Fatal("-s must be path.type")
	}
	spath, stype := (*subType)[:i], (*subType)[i+1:]
	spkg, err := loadPackage(spath)
	if err != nil {
		log.Fatal(err)
	}

	tobj := spkg.tpkg.Scope().Lookup(stype)
	typ := tobj.Type()
	if err := checkGenericBinding(gparams[0], typ); err != nil {
		log.Fatalf("%s cannot be bound to %s: %v",
			*subType, gparams[0].typeSpec.Name.Name, err)
	}

	ts := gparams[0].typeSpec
	if !ts.Assign.IsValid() {
		ts.Assign = ts.Type.Pos()
	}
	ts.Type = &ast.Ident{Name: stype}
	for filename, file := range gpkg.apkg.Files {
		fmt.Printf("==== %s ====\n", filename)
		if err := printFile(os.Stdout, gpkg.fset, file); err != nil {
			log.Fatalf("%s: %v", filename, err)
		}
	}
}

type Package struct {
	fset *token.FileSet
	apkg *ast.Package
	tpkg *types.Package
	info *types.Info
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

type GenericParam struct {
	typeSpec *ast.TypeSpec
	typ      types.Type
}

type Substitution struct {
	expr ast.Expr
	typ  types.Type
}

func genericParams(pkg *Package) []*GenericParam {
	var gps []*GenericParam
	for _, file := range pkg.apkg.Files {
		tspecs := genericTypeSpecs(file)
		for _, ts := range tspecs {
			gps = append(gps, &GenericParam{
				typeSpec: ts,
				typ:      pkg.info.Defs[ts.Name].Type(),
			})
		}
	}
	return gps
}

func checkGenericBinding(param *GenericParam, typ types.Type) error {
	switch pt := param.typ.Underlying().(type) {
	case *types.Interface:
		iface := newSubInterface(pt, param, typ)
		if types.Implements(typ, iface) {
			return nil
		} else {
			return fmt.Errorf("%s does not implement %s", typ, pt)
		}
	default:
		return fmt.Errorf("unknown underlying type: %T\n", pt)
	}
}

// Substitute stype for occurrences of the param type in iface, returning a new interface type.
func newSubInterface(iface *types.Interface, param *GenericParam, stype types.Type) *types.Interface {
	var nms []*types.Func
	for i := 0; i < iface.NumMethods(); i++ {
		m := iface.Method(i)
		sig := m.Type().(*types.Signature)
		params := subTuple(sig.Params(), param.typ, stype)
		results := subTuple(sig.Results(), param.typ, stype)
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

func printFile(w io.Writer, fset *token.FileSet, file *ast.File) error {
	conf := &printer.Config{
		Mode:     printer.UseSpaces,
		Tabwidth: 4,
	}
	return conf.Fprint(w, fset, file)
}

func cgtext(cgs []*ast.CommentGroup) string {
	var ss []string
	for _, cg := range cgs {
		ss = append(ss, cg.Text())
	}
	return strings.Join(ss, ", ")
}

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
