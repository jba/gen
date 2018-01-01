package gen

// go:gen
type T = interface{}

// Eq means supports ==
type Eq interface{}

type Equal interface {
	Equal(T) bool
}

type Cmp interface {
	Equal
	Less(T) bool
	Greater(T) bool
	LessEqual(T) bool
	GreaterEqual(T) bool
}
