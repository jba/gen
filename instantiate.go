package gen

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/types"
	"path"
	"path/filepath"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

// Instantiate instantiates the generic package, giving the resulting package the given name.
// The bindings are a map from generic parameter name to the instantiated type of the parameter.
// After Instantiate returns, the AST of pkg has been altered, the FileSet has been updated to
// match and the new package has been typechecked.
// Note: the AST may be altered even if Instantiate returns an error.
func Instantiate(pkg *Package, pkgName string, bindings map[string]types.Type) error {
	bindingList, err := newBindingList(bindings, pkg)
	if err != nil {
		return err
	}
	if err := substitutePackage(pkg, bindingList, pkgName); err != nil {
		return err
	}
	for _, b := range bindingList {
		if !b.found {
			return fmt.Errorf("no type parameter %s in %s", b.param, pkg.Path)
		}
	}
	if err := pkg.Apkg.reload(); err != nil {
		return err
	}
	tpkg, info, err := typecheckPackage("dummy_import_path/"+pkgName, pkg.Apkg, theImporter)
	if err != nil {
		return err
	}
	pkg.Tpkg = tpkg
	pkg.info = info
	return nil
}

func newBindingList(bindings map[string]types.Type, pkg *Package) ([]*Binding, error) {
	var bindingList []*Binding
	for param, argType := range bindings {
		b, err := newBinding(param, argType, pkg)
		if err != nil {
			return nil, err
		}
		bindingList = append(bindingList, b)
	}
	return bindingList, nil
}

// Modifies the asts in pkg. pkgName is the new package name.
func substitutePackage(pkg *Package, bindings []*Binding, pkgName string) error {
	rws := makeRewriteRules(bindings)
	pkg.Apkg.pkg.Name = pkgName
	for _, file := range pkg.Apkg.pkg.Files {
		if err := substituteFile(file, bindings, rws, pkg, pkgName, true); err != nil {
			return err
		}
	}
	return nil
}

func makeRewriteRules(bindings []*Binding) []rewrite {
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
	return rws
}

// substituteFile changes the AST of file to reflect the bindings and rewrites.
func substituteFile(file *ast.File, bindings []*Binding, rewrites []rewrite, pkg *Package, pkgName string, addImports bool) error {
	for _, b := range bindings {
		typeSpec := findTypeDecl(b.param, file)
		if typeSpec == nil {
			continue
		}
		b.found = true
		if addImports {
			// Add an import for the argument type if necessary.
			// TODO: the named type from another package might be embedded in the type, like map[int]geo.Point.
			if named, ok := b.arg.(*types.Named); ok {
				tn := named.Obj()
				name := tn.Pkg().Name()
				ipath := tn.Pkg().Path()
				if name == path.Base(ipath) {
					name = ""
				}
				astutil.AddNamedImport(pkg.Apkg.fset, file, name, ipath)
			}
		}
		// Turn the type spec into an alias if it isn't already.
		if !typeSpec.Assign.IsValid() {
			typeSpec.Assign = typeSpec.Type.Pos()
		}
		typeSpec.Type = typeToExpr(b.arg, pkg.Tpkg)
	}
	file.Name.Name = pkgName
	if err := replaceCode(file, bindings, rewrites, pkg); err != nil {
		return err
	}
	trimImports(pkg.Apkg.fset, file)
	return nil
}

// Return the declaration of param in file, or nil if not present.
func findTypeDecl(param *types.TypeName, file *ast.File) *ast.TypeSpec {
	paramPath, _ := astutil.PathEnclosingInterval(file, param.Pos(), param.Pos())
	if len(paramPath) < 2 {
		// Type decl for param is not in this file.
		return nil
	}
	return paramPath[1].(*ast.TypeSpec)
}

// instantiateInto instantiates generic package gpkg into the package dest.
// The filenames and symbols of pkg are prefixed with the generic import's name.
// gpkg's AST is messed up afterwards.
// TODO: name collisions between the new names we create and existing names in dest (including filenames).
func instantiateInto(gpkg *Package, gimp genericImport, bindings map[string]types.Type, dest *astPackage) error {
	bindingList, err := newBindingList(bindings, gpkg)
	if err != nil {
		return err
	}
	if err := substitutePackageInto(gpkg, bindingList, gimp, dest); err != nil {
		return err
	}
	for _, b := range bindingList {
		if !b.found {
			return fmt.Errorf("no type parameter %s in %s", b.param, gpkg.Path)
		}
	}
	return dest.reload()
}

func substitutePackageInto(src *Package, bindings []*Binding, gimp genericImport, dest *astPackage) error {
	prefix := gimp.name + "_"
	// Remove filenames with prefix from dest, because they are from an earlier instantiation.
	for filename := range dest.pkg.Files {
		if strings.HasPrefix(filename, prefix) {
			delete(dest.pkg.Files, filename)
		}
	}
	// For each file in dest that imports src, replace references to the import identifier with
	// prefixed symbols.
	// We already checked that all generic imports matching gimp use the same name.
	for _, gi := range dest.genericImports {
		if gi.name == gimp.name {
			replaceImportWithPrefix(gi.file, gimp.name, prefix)
			cmap := ast.NewCommentMap(dest.fset, gi.file, gi.file.Comments)
			// TODO: Instead of deleting the import, comment it out. That will
			// preserve line numbers in error messages.
			astutil.DeleteNamedImport(dest.fset, gi.file, gi.name, gi.path)
			gi.file.Comments = cmap.Filter(gi.file).Comments()
		}
	}

	// Perform normal substitution on the files of src, and also prefix all top-level symbols.
	// Add the modified files of src to dest.
	rws := makeRewriteRules(bindings)
	for filename, file := range src.Apkg.pkg.Files {
		if err := substituteFileInto(filename, file, bindings, rws, src, dest, prefix); err != nil {
			return err
		}
	}
	return nil
}

func substituteFileInto(filename string, file *ast.File, bindings []*Binding, rewrites []rewrite, src *Package, dest *astPackage, prefix string) error {
	if err := substituteFile(file, bindings, rewrites, src, dest.pkg.Name, false); err != nil {
		return err
	}
	prefixTopLevelSymbols(file, prefix)
	// Format file with the src FileSet, then parse it with the dest FileSet. If
	// we don't do this before we add file to dest, then its positions are
	// interpreted relative to the wrong FileSet, and that can actually result in
	// syntactically invalid code.
	var buf bytes.Buffer
	if err := format.Node(&buf, src.Apkg.fset, file); err != nil {
		return err
	}
	newFilename := filepath.Join(dest.dir, prefix+filepath.Base(filename))
	newFile, err := parser.ParseFile(dest.fset, newFilename, &buf, parser.ParseComments)
	if err != nil {
		return err
	}
	dest.pkg.Files[newFilename] = newFile
	return nil
}
