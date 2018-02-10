package main

import (
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"

	"github.com/jba/gen"
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
	if len(args) == 0 {
		return errors.New("need bindings")
	}
	var params []string
	bspecs := map[string]string{}
	for _, arg := range args {
		param, arg, err := gen.ParseBindingSpec(arg)
		if err != nil {
			return err
		}
		bspecs[param] = arg
		params = append(params, param)
	}
	cwd, err := os.Getwd()
	if err != nil {
		return err
	}
	pkg, err := gen.Check(genPath, cwd, params)
	if err != nil {
		return fmt.Errorf("check failed: %v", err)
	}
	bindings, err := gen.NewBindingMap(bspecs)
	if err != nil {
		return err
	}
	if err := gen.Instantiate(pkg, outputName, bindings); err != nil {
		return err
	}
	return writePackage(pkg, outputDir, outputName)
}

func writePackage(pkg *gen.Package, outputDir, outputName string) error {
	if err := os.MkdirAll(outputDir, os.ModePerm); err != nil {
		return err
	}
	tempOutDir, err := ioutil.TempDir(outputDir, "gen-")
	if err != nil {
		return err
	}

	if err := pkg.Write(tempOutDir); err != nil {
		if err := os.Remove(tempOutDir); err != nil {
			log.Printf("removing %s: %v", tempOutDir, err)
		}
		return err
	}
	destDir := filepath.Join(outputDir, pkg.Name())
	if err := os.RemoveAll(destDir); err != nil {
		return err
	}
	return os.Rename(tempOutDir, destDir)
}
