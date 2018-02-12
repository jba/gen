package gen

import (
	"fmt"
	"go/token"
	"go/types"
)

type Package struct {
	Path string

	Apkg   *astPackage
	Tpkg   *types.Package
	Params []string
	info   *types.Info
}

func (p *Package) String() string {
	return fmt.Sprintf("%s (%s)", p.Tpkg.Name(), p.Tpkg.Path())
}

func (p *Package) Name() string {
	return p.Apkg.pkg.Name
}

func (p *Package) position(pos token.Pos) token.Position {
	return p.Apkg.fset.Position(pos)
}

func (p *Package) topLevelTypeName(name string) (*types.TypeName, error) {
	gobj := p.Tpkg.Scope().Lookup(name)
	if gobj == nil {
		return nil, fmt.Errorf("cannot find %s in package %s", name, p)
	}
	gtn, ok := gobj.(*types.TypeName)
	if !ok {
		return nil, fmt.Errorf("%s is not a named type in package %s", name, p)
	}
	return gtn, nil
}

// Write writes the files of p to the directory dir. The directory must already exist.
func (p *Package) Write(dir string) error {
	return p.Apkg.write(dir)
}
