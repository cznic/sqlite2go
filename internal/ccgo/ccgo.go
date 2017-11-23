// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package ccgo translates c99 ASTs to Go source code. (Work In Progress)
//
// This package is a modification of [1] supporting only SQLite.
//
// [1] https://github.com/cznic/ccgo
package ccgo

import (
	"io"

	"github.com/cznic/sqlite2go/internal/c99"
)

// Command outputs a Go program generated from inputs to w.
//
// No package or import clause is generated.
func Command(w io.Writer, inputs []*c99.TranslationUnit) error {
	return generate(w, true, inputs)
}

// Package outputs a Go package generated from inputs to w.
//
// No package or import clause is generated.
func Package(w io.Writer, inputs []*c99.TranslationUnit) error {
	return generate(w, false, inputs)
}

func generate(w io.Writer, cmd bool, inputs []*c99.TranslationUnit) error {
	panic("TODO")
}
