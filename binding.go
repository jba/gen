package gen

import (
	"fmt"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"strconv"
	"strings"
)

type Binding struct {
	param *types.TypeName
	arg   types.Type
	found bool
}

var verbose = false

// Parse a string of the form "param:type", where param is an identifier
// and type is a possibly-qualified type name, like "fmt.Stringer"
// or "golang.org/x/net/context.Context".
func ParseBindingSpec(s string) (param, arg string, err error) {
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

func NewBindingMap(specs map[string]string) (map[string]types.Type, error) {
	return newBindingMap(specs, lookupBuiltinName)
}

func newBindingMap(specs map[string]string, lookup func(string) types.Type) (map[string]types.Type, error) {
	m := map[string]types.Type{}
	for param, arg := range specs {
		atype, err := buildType(arg, lookup)
		if err != nil {
			return nil, err
		}
		m[param] = atype
	}
	return m, nil
}

func newBinding(paramName string, argType types.Type, pkg *Package) (*Binding, error) {
	gtn, err := pkg.topLevelTypeName(paramName)
	if err != nil {
		return nil, err
	}
	if err := checkBinding(gtn.Type(), argType); err != nil {
		return nil, fmt.Errorf("%s: %v", pkg.Fset.Position(gtn.Pos()), err)
	}
	return &Binding{
		param: gtn,
		arg:   argType,
		found: false,
	}, nil
}

// Check that a binding is valid: that the arg can be substituted for the param.
func checkBinding(ptype, atype types.Type) error {
	putype := ptype.Underlying().(*types.Interface)
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
		return missingMethodError(fmt.Sprintf("%s does not implement %s: method %s %s", atype, ptype,
			method.Name(), msg))
	}
	if implementsSpecialInterface(putype, "Comparable") && !types.Comparable(atype) {
		return needsComparableError(fmt.Sprintf("%s is not comparable but %s requires it", atype, ptype))
	}
	if implementsSpecialInterface(putype, "Nillable") && !hasNil(atype) {
		return needsNillableError(fmt.Sprintf("%s is not nillable but %s requires it", atype, ptype))
	}
	return nil
}

// hasNil reports whether a type includes the nil value.
// Code from https://golang.org/src/go/types/predicates.go.
func hasNil(typ types.Type) bool {
	switch t := typ.Underlying().(type) {
	case *types.Basic:
		return t.Kind() == types.UnsafePointer
	case *types.Slice, *types.Pointer, *types.Signature, *types.Interface, *types.Map, *types.Chan:
		return true
	}
	return false
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

// augmentedType creates a new type that may add methods to t.
// Comparable types behave like they implement Equal(T) bool (if they don't already).
// Ordered types (which are only basic types) behave like they implement Less(T)  bool,
// Greater(T) bool, LessEqual(T) bool and GreaterEqual(T) bool.
func augmentedType(t types.Type) types.Type {
	parm := func(t types.Type) *types.Var {
		return types.NewParam(token.NoPos, nil, "", t)
	}
	sig := types.NewSignature(parm(t), types.NewTuple(parm(t)), types.NewTuple(parm(types.Typ[types.Bool])), false)
	var methods []*types.Func
	for _, am := range augmentedMethods(t) {
		methods = append(methods, types.NewFunc(token.NoPos, nil, am.name, sig))
	}
	tn := types.NewTypeName(token.NoPos, nil, "AUGMENTED", nil)
	u := t.Underlying()
	if _, ok := t.(*types.Named); ok {
		// Use embedding to avoid losing the methods from the original type.
		u = types.NewStruct([]*types.Var{types.NewField(token.NoPos, nil, "", t, true)}, nil)
	}
	return types.NewNamed(tn, u, methods)
}

func augmentedMethods(t types.Type) []augmentMethod {
	var ams []augmentMethod
	mset := types.NewMethodSet(t)
	if types.Comparable(t) {
		if mset.Lookup(nil, "Equal") != nil {
			if verbose {
				log.Printf("not augmenting comparable type %s with Equal because it already has an equal method", t)
			}
		} else {
			ams = append(ams, equalMethod)
		}
	}
	if bt, ok := t.(*types.Basic); ok && (bt.Info()&types.IsOrdered != 0) {
		ams = append(ams, orderedMethods...)
	}
	return ams
}

const genImportPath = "github.com/jba/gen/generic"

func implementsSpecialInterface(iface *types.Interface, name string) bool {
	for i := 0; i < iface.NumEmbeddeds(); i++ {
		tn := iface.Embedded(i).Obj()
		if tn.Name() == name && tn.Pkg().Path() == genImportPath {
			return true
		}
		if implementsSpecialInterface(iface.Embedded(i).Underlying().(*types.Interface), name) {
			return true
		}
	}
	return false
}

type augmentMethod struct {
	name string      // e.g. Equal
	tok  token.Token // e.g. ==
}

var (
	equalMethod    = augmentMethod{"Equal", token.EQL}
	orderedMethods = []augmentMethod{
		{"Less", token.LSS},
		{"Greater", token.GTR},
		{"LessEqual", token.LEQ},
		{"GreaterEqual", token.GEQ},
	}
)

// buildType constructs a types.Type from a string expression that should
// denote a type. In the string, import paths with slashes must be quoted,
// and array lengths must be literal integers.
// lookupName returns the type for an unqualified name.
func buildType(s string, lookupName func(string) types.Type) (types.Type, error) {
	expr, err := parser.ParseExpr(s)
	if err != nil {
		return nil, err
	}
	return exprToType(expr, lookupName)
}
