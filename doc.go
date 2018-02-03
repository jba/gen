// TODO
// - rewrite type switches
// - typecheck the output to make sure we didn't miss anything (like "var x T = 7").
// - a generic package importing other generic packages (ones with interface definitions)
//
// - When a generic param has the Comparable constraint, allow instantiations of interface types.
//   That mirrors the compiler, which allows interface types in == and map indexing and defers the comparable
//   check to runtime.

// - Given
//     type T interface{}
//     type U T
//   the types package loses the information that U was defined in terms of T. So we can't do a comparability
//   check correctly: map[U]int will succeed even though we are assuming T is not comparable. I guess I'll
//   have to use the ast to recognize this case.

// The init statement in a type switch can define variables which are not used in the selected
// case. The resulting code doesn't compile because of the unused identifier. E.g.
//		switch z := 1; x.(type) {
//		case int: fmt.Println()
//		case bool: fmt.Println(z)
//		}
// If the arg type is int, the resulting block is { z := 1; fmt.Println() }, which is invalid.

// examples (for readme):
// - container/ring
// - jba/btree
// - pubsub/pullstream
// - bundler
// - lru.Cache?
// - sync.Map?

package gen
