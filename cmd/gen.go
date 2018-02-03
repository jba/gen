package main

import (
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os"
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
	pwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}

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

	///// snip
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
	///// snip

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

func instantiateImportPaths(fset *token.FileSet, file *ast.File, name, gpath, ipath string) error {
	astutil.DeleteImport(fset, file, gpath)
	astutil.AddNamedImport(fset, file, name, ipath)
	return nil
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
