package gen

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/build"
	"go/format"
	"go/parser"
	"go/token"
	"path/filepath"
	"strconv"
	"strings"
)

func astPackageFromPath(fset *token.FileSet, ipath, dir string) (*ast.Package, error) {
	bpkg, err := build.Import(ipath, dir, build.FindOnly)
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
	for filename := range apkg.Files {
		if strings.HasSuffix(filename, "_test.go") {
			delete(apkg.Files, filename)
		}
	}
	return apkg, nil
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
	bindingSpecs []string
	spec         *ast.ImportSpec
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
					spec:         ispec,
				})
				return false
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
			if result != nil {
				return nil, fmt.Errorf("more than one associated set of bindings: %v and %v",
					result, fields[1:])
			}
			result = fields[1:]
		}
	}
	return result, nil
}
