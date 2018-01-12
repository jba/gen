package main

import "testing"

func TestComparable(t *testing.T) {
	// Tests that we detect when a generic param must implement Comparable.
	for _, test := range []struct {
		// Assume T is the param
		mapIndexType string
		want         bool
	}{
		{"T", true},
		{"*T", false},
		{"U", true}, // where type U T
		{"struct{x T}", true},
		{"[1]T", true},
	} {
		// TODO
	}
}
