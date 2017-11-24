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
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"path/filepath"
	"runtime/debug"
	"sort"
	"strings"

	"github.com/cznic/ccir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
)

var (
	dict = xc.Dict

	idStart = dict.SID("_start")
)

// Command outputs a Go program generated from in to w.
//
// No package or import clause is generated.
func Command(w io.Writer, in []*c99.TranslationUnit) error {
	return newGen(w, in).gen(true)
}

// Package outputs a Go package generated from in to w.
//
// No package or import clause is generated.
func Package(w io.Writer, in []*c99.TranslationUnit) error {
	return newGen(w, in).gen(false)
}

type extern struct {
	d  *c99.Declarator
	fs *token.FileSet
}

func (e *extern) position(n c99.Node) token.Position { return e.fs.PositionFor(n.Pos(), true) }

type gen struct {
	errs              scanner.ErrorList
	externals         map[int]extern
	in                []*c99.TranslationUnit
	internals         []map[int]*c99.Declarator
	out               io.Writer
	producedExternals map[int]struct{}
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		externals:         map[int]extern{},
		in:                in,
		internals:         make([]map[int]*c99.Declarator, len(in)),
		out:               out,
		producedExternals: map[int]struct{}{},
	}
}

func (g *gen) gen(cmd bool) (err error) {
	defer func() {
		switch e := recover(); e.(type) {
		case nil:
			// nop
		default:
			err = fmt.Errorf("PANIC: %v\n%s", e, debugStack())
		}
	}()

	if err := g.collectSymbols(); err != nil {
		return err
	}

	switch {
	case cmd:
		todo("")
	default:
		var a []string
		for nm := range g.externals {
			a = append(a, string(dict.S(nm)))
		}
		sort.Strings(a)
		for _, nm := range a {
			g.produceExternal(dict.SID(nm))
		}
		todo("")
	}
	if err := g.errs.Err(); err != nil {
		return fmt.Errorf("%s", errString(err))
	}

	return nil
}

func errString(err error) string {
	var b bytes.Buffer
	printError(&b, "", err)
	return b.String()
}

func printError(w io.Writer, pref string, err error) {
	switch x := err.(type) {
	case scanner.ErrorList:
		x.RemoveMultiples()
		for i, v := range x {
			fmt.Fprintf(w, "%s%v\n", pref, v)
			if i == 50 {
				fmt.Fprintln(w, "too many errors")
				break
			}
		}
	default:
		fmt.Fprintf(w, "%s%v\n", pref, err)
	}
}

func (g *gen) produceExternal(nm int) {
	if _, ok := g.producedExternals[nm]; ok {
		return
	}

	defer func(nm int) { g.producedExternals[nm] = struct{}{} }(nm)

	extern, ok := g.externals[nm]
	if !ok {
		todo("")
	}
	d := extern.d
	switch d.Type.Kind() {
	case c99.Function:
		g.functionDefinition(extern, nm)
	default:
		//TODO
	}
}

func (g *gen) functionDefinition(e extern, nm int) {
	d := e.d
	if strings.HasSuffix(filepath.Dir(e.position(d).Filename), ccir.LibcIncludePath) {
		return
	}

	fd := d.FunctionDefinition
	if fd == nil {
		return
	}

	todo("%v: %s %v", e.position(d), dict.S(nm), d.Type)
}

func todo(msg string, args ...interface{}) { panic(fmt.Errorf(msg, args...)) }

func debugStack() []byte {
	b := debug.Stack()
	b = b[bytes.Index(b, bPanic)+1:]
	b = b[bytes.Index(b, bPanic):]
	b = b[bytes.Index(b, bNL)+1:]
	return b
}

var (
	bNL    = []byte{'\n'}
	bPanic = []byte("panic")
)

func (g *gen) collectSymbols() error {
	for it, t := range g.in {
		internal := map[int]*c99.Declarator{}
		g.internals[it] = internal
		for nm, n := range t.FileScope.Idents {
			switch x := n.(type) {
			case *c99.Declarator:
				switch x.Linkage {
				case c99.LinkageExternal:
					if _, ok := g.externals[nm]; ok {
						todo("")
					}
					g.externals[nm] = extern{x, t.FileSet}
				case c99.LinkageInternal:
					if _, ok := internal[nm]; ok {
						todo("")
					}
					internal[nm] = x
				case c99.LinkageNone:
					if x.DeclarationSpecifier.IsTypedef() {
						// nop ATM
						break
					}

					todo("")
				default:
					todo("")
				}
			case *c99.EnumerationConstant:
				// nop
			default:
				todo("%T", x)
			}
		}
	}
	return nil
}
