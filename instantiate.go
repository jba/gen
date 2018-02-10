package gen

import (
	"fmt"
	"go/ast"
	"go/types"
	"path"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

// TODO: make sure that for in-package instantiation, all import path aliases are identical.>

// // InstantiatePath instantiates the generic package at the given import path (relative to pdir).
// // The bindings are a map from generic parameter name to the instantiated type of the parameter.
// func InstantiatePath(path, pdir, outputName string, bindings map[string]types.Type) error {
// 	var params []string
// 	for p := range bindingMap {
// 		params = append(params, p)
// 	}
// 	pkg, err := CheckPath(path, pdir, params)
// 	if err != nil {
// 		return err
// 	}
// 	return Instantiate(pkg, outputName, bindingMap)
// }

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
	pkg.Fset, pkg.Apkg, err = reloadAST(pkg.Fset, pkg.Apkg)
	if err != nil {
		return err
	}
	tpkg, info, err := typecheckPackage("dummy_import_path/"+pkgName, pkg.Fset, pkg.Apkg, theImporter)
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
	pkg.Apkg.Name = pkgName
	for _, file := range pkg.Apkg.Files {
		if err := substituteFile(file, bindings, rws, pkg, pkgName); err != nil {
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
func substituteFile(file *ast.File, bindings []*Binding, rewrites []rewrite, pkg *Package, pkgName string) error {
	for _, b := range bindings {
		typeSpec := findTypeDecl(b.param, file)
		if typeSpec == nil {
			continue
		}
		b.found = true
		// Add an import for the argument type if necessary.
		// TODO: the named type from another package might be embedded in the type, like map[int]geo.Point.
		if named, ok := b.arg.(*types.Named); ok {
			tn := named.Obj()
			name := tn.Pkg().Name()
			ipath := tn.Pkg().Path()
			if name == path.Base(ipath) {
				name = ""
			}
			astutil.AddNamedImport(pkg.Fset, file, name, ipath)
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
	trimImports(pkg.Fset, file)
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

// InstantiateInto instantiates generic package pkg into the package dest.
// The filenames and symbols of pkg are given prefix.
// **************** pkg is trashed afterwards.
func InstantiateInto(pkg *Package, prefix string, bindings map[string]types.Type, dest *ast.Package) error {
	// TODO: name collisions between the new names we create and existing names in dest (including filenames).
	bindingList, err := newBindingList(bindings, pkg)
	if err != nil {
		return err
	}
	if err := substitutePackageInto(pkg, bindingList, prefix+"_", dest); err != nil {
		return err
	}
	for _, b := range bindingList {
		if !b.found {
			return fmt.Errorf("no type parameter %s in %s", b.param, pkg.Path)
		}
	}
	// var err error
	// pkg.Fset, pkg.Apkg, err = reloadAST(pkg.Fset, pkg.Apkg)
	// if err != nil {
	// 	return err
	// }
	// tpkg, info, err := typecheckPackage("dummy_import_path/"+name, pkg.Fset, pkg.Apkg, theImporter)
	// if err != nil {
	// 	return err
	// }
	// pkg.Tpkg = tpkg
	// pkg.info = info
	return nil
}

func substitutePackageInto(src *Package, bindings []*Binding, prefix string, dest *ast.Package) error {
	// Remove filenames with prefix from dest, because they are from an earlier instantiation.
	for filename := range dest.Files {
		if strings.HasPrefix(filename, prefix) {
			delete(dest.Files, filename)
		}
	}

	// For each file in dest that imports src, replace references to the import identifier with
	// prefixed symbols.

	// Perform normal substitution on the files of src, and also prefix all top-level symbols.
	// Add the modified files of src to dest.
	rws := makeRewriteRules(bindings)
	for filename, file := range src.Apkg.Files {
		if err := substituteFile(file, bindings, rws, src, dest.Name); err != nil {
			return err
		}
		prefixTopLevelSymbols(file, prefix)
		dest.Files[prefix+filename] = file
	}
	return nil
}
