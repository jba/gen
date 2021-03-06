package gen

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/build"
	"go/format"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

type astPackage struct {
	dir            string
	pkg            *ast.Package
	fset           *token.FileSet
	genericImports []genericImport
}

func astPackageFromPath(ipath, dir string) (*astPackage, error) {
	bpkg, err := build.Import(ipath, dir, build.FindOnly)
	if err != nil {
		return nil, err
	}
	return astPackageFromDir(bpkg.Dir)
}

func astPackageFromDir(dir string) (*astPackage, error) {
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, dir, nil, parser.ParseComments|parser.AllErrors)
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
	return newASTPackage(dir, fset, apkg, true)
}

func newASTPackage(dir string, fset *token.FileSet, apkg *ast.Package, parseComms bool) (*astPackage, error) {
	for filename := range apkg.Files {
		if strings.HasSuffix(filename, "_test.go") {
			delete(apkg.Files, filename)
		}
	}
	var gis []genericImport
	if parseComms {
		var err error
		gis, err = parseComments(fset, apkg)
		if err != nil {
			return nil, err
		}
	}
	return &astPackage{
		dir:            dir,
		pkg:            apkg,
		fset:           fset,
		genericImports: gis,
	}, nil
}

func (a *astPackage) lookupTopLevel(name string) *ast.Object {
	// a.pkg.Scope is nil (bug in ast package?)
	for _, f := range a.pkg.Files {
		if obj := f.Scope.Objects[name]; obj != nil {
			return obj
		}
	}
	return nil
}

func (a *astPackage) reload() error {
	fset2 := token.NewFileSet()
	apkg2 := &ast.Package{
		Name:  a.pkg.Name,
		Files: make(map[string]*ast.File),
	}
	for filename, file := range a.pkg.Files {
		var buf bytes.Buffer
		if err := format.Node(&buf, a.fset, file); err != nil {
			return err
		}
		file2, err := parser.ParseFile(fset2, filename, &buf, parser.ParseComments)
		if err != nil {
			return err
		}
		apkg2.Files[filename] = file2
	}
	gis, err := parseComments(fset2, apkg2)
	if err != nil {
		return err
	}
	a.fset = fset2
	a.pkg = apkg2
	a.genericImports = gis
	return nil
}

func (a *astPackage) write(dir string) error {
	for filename, file := range a.pkg.Files {
		outfile := filepath.Join(dir, filepath.Base(filename))
		f, err := os.Create(outfile)
		if err != nil {
			return err
		}
		if err := format.Node(f, a.fset, file); err != nil {
			_ = f.Close()
			return err
		}
		if err := f.Close(); err != nil {
			return err
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

type genericImport struct {
	name         string
	path         string
	bindingSpecs map[string]bool
	file         *ast.File
	spec         *ast.ImportSpec
}

func (g1 genericImport) samePathAndBindings(g2 genericImport) bool {
	return g1.path == g2.path && reflect.DeepEqual(g1.bindingSpecs, g2.bindingSpecs)
}

func parseComments(fset *token.FileSet, p *ast.Package) ([]genericImport, error) {
	var ids []genericImport
	var err error
	for _, file := range p.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			var ispec *ast.ImportSpec
			var cgs []*ast.CommentGroup
			switch n := n.(type) {
			case *ast.ImportSpec:
				ispec = n
				cgs = []*ast.CommentGroup{n.Doc, n.Comment}

			case *ast.GenDecl:
				if n.Doc != nil && len(n.Specs) == 1 {
					if is, ok := n.Specs[0].(*ast.ImportSpec); ok {
						ispec = is
						cgs = []*ast.CommentGroup{n.Doc}
					}
				}
			}
			if ispec == nil {
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
				ids = append(ids, genericImport{
					name:         ispec.Name.Name,
					path:         importPath(ispec),
					bindingSpecs: bspecs,
					file:         file,
					spec:         ispec,
				})
				return false
			}
			return true
		})
	}
	return ids, err
}

func extractBindingSpecs(cgroups []*ast.CommentGroup) (map[string]bool, error) {
	const directiveName = "gen:import"
	var list []string
	for _, g := range cgroups {
		if g == nil {
			continue
		}
		for _, c := range g.List {
			txt := strings.Trim(c.Text, "/* \t")
			fields := strings.Fields(txt)
			if len(fields) == 0 {
				continue
			}
			if fields[0] != directiveName {
				continue
			}
			if list != nil {
				return nil, fmt.Errorf("more than one associated set of bindings: %v and %v",
					list, fields[1:])
			}
			list = fields[1:]
		}
	}
	if len(list) == 0 {
		return nil, nil
	}
	result := map[string]bool{}
	for _, bs := range list {
		result[bs] = true
	}
	return result, nil
}

// trimImports removes unused imports from file.
func trimImports(fset *token.FileSet, file *ast.File) {
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
}

func prefixTopLevelSymbols(file *ast.File, prefix string) {
	pref := func(id *ast.Ident) {
		id.Name = prefix + id.Name
	}

	// Create a set of the top-level decls.
	topLevelDecls := map[interface{}]bool{}
	for _, decl := range file.Decls {
		switch decl := decl.(type) {
		case *ast.FuncDecl:
			if decl.Name.Name != "init" { // don't prefix init functions
				topLevelDecls[decl] = true
			}
		case *ast.GenDecl:
			switch decl.Tok {
			case token.IMPORT:
				continue
			case token.TYPE:
				for _, spec := range decl.Specs {
					topLevelDecls[spec] = true
				}
			case token.CONST, token.VAR:
				for _, spec := range decl.Specs {
					topLevelDecls[spec] = true
				}
			default:
				panic("bad token")
			}
		default:
			panic("bad decl")
		}
	}
	// Prefix any identifier that refers to a top-level decl.
	ast.Inspect(file, func(n ast.Node) bool {
		if id, ok := n.(*ast.Ident); ok && id.Obj != nil && topLevelDecls[id.Obj.Decl] {
			pref(id)
		}
		return true
	})

}

func nodeString(n interface{}, fset *token.FileSet) string {
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, n); err != nil {
		log.Fatal(err)
	}
	return buf.String()
}

func replaceImportWithPrefix(file *ast.File, importID, prefix string) {
	pre := func(c *astutil.Cursor) bool {
		if sel, ok := c.Node().(*ast.SelectorExpr); ok {
			// If the X expression is an identifier without an Obj, assume it refers
			// to an import.
			// TODO: verify the above.
			if id, ok := sel.X.(*ast.Ident); ok && id.Obj == nil && id.Name == importID {
				c.Replace(&ast.Ident{Name: prefix + sel.Sel.Name})
			}
		}
		return true
	}
	astutil.Apply(file, pre, nil)
}
