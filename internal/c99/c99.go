// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate golex -o trigraphs.go trigraphs.l
//go:generate golex -o scanner.go scanner.l
//go:generate yy -kind Case -o parser.y -astImport "\"github.com/cznic/xc\";\"go/token\";\"fmt\"" -prettyString PrettyString parser.yy

//go:generate goyacc -o /dev/null -xegen xegen parser.y
//go:generate goyacc -o parser.go -fs -xe xegen -dlvalf "%v" -dlval "PrettyString(lval.Token)" parser.y
//go:generate rm -f xegen
//go:generate stringer -output enum_string.go -type=TypeKind,cond enum.go type.go
//go:generate sh -c "go test -run ^Example |fe"
//go:generate gofmt -l -s -w .

// Package c99 is a C99 compiler front end. (Work In Progress)
//
// This package is a modification of[1] supporting only SQLite.
//
//  [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
//  [1]: https://github.com/cznic/cc
package c99

import (
	"bufio"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"os"
)

type tweaks struct {
	cppExpandTest   bool // Fake includes
	enableTrigraphs bool
	injectFinalNL   bool
}

// Translation unit context.
type context struct {
	declarationSpecifiers *DeclarationSpecifiers
	errors                scanner.ErrorList
	exampleAST            interface{}
	exampleRule           int
	fset                  *token.FileSet
	includePaths          []string
	model                 Model
	scope                 *scope
	scopes                []*scope
	sysIncludePaths       []string
	tweaks                *tweaks
}

func newContext(fset *token.FileSet, t *tweaks) (*context, error) {
	return &context{
		fset:   fset,
		scope:  newScope(nil),
		tweaks: t,
	}, nil
}

func (c context) position(n Node) token.Position { return c.fset.PositionFor(n.Pos(), true) }

func (c *context) err(n Node, msg string, args ...interface{}) { c.errPos(n.Pos(), msg, args...) }

func (c *context) errPos(pos token.Pos, msg string, args ...interface{}) {
	c.errors.Add(c.fset.PositionFor(pos, true), fmt.Sprintf(msg, args...))
}

func (c *context) error() error {
	if len(c.errors) == 0 {
		return nil
	}

	c.errors.Sort()
	err := append(scanner.ErrorList(nil), c.errors...)
	return err
}

func (c *context) toC(ch rune, val int) rune {
	if ch != IDENTIFIER {
		return ch
	}

	if x, ok := keywords[val]; ok {
		return x
	}

	return ch
}

// Source represents a preprocessing file.
type Source interface {
	Cache([]uint32)
	Cached() []uint32
	Name() string
	ReadCloser() (io.ReadCloser, error)
	Size() (int64, error)
}

type fileSource struct {
	*bufio.Reader
	f    *os.File
	path string
}

func newFileSource(nm string) *fileSource { return &fileSource{path: nm} }

func (s *fileSource) Cache([]uint32)   {}
func (s *fileSource) Cached() []uint32 { return nil }
func (s *fileSource) Close() error     { return s.f.Close() }
func (s *fileSource) Name() string     { return s.path }

func (s *fileSource) ReadCloser() (io.ReadCloser, error) {
	f, err := os.Open(s.path)
	if err != nil {
		return nil, err
	}

	s.f = f
	s.Reader = bufio.NewReader(f)
	return s, nil
}

func (s *fileSource) Size() (int64, error) {
	fi, err := os.Stat(s.path)
	if err != nil {
		return 0, err
	}

	return fi.Size(), nil
}

type scope struct {
	parent *scope
	m      map[int]*Declarator // name: *Declarator
}

func newScope(parent *scope) *scope { return &scope{parent: parent} }

func (s *scope) insert(c *context, d *Declarator) {
	if s.m == nil {
		s.m = map[int]*Declarator{}
	}
	nm := d.nm
	if ex := s.m[nm]; ex != nil {
		panic(fmt.Errorf("%q ex %v, now %v", dict.S(nm), c.position(ex), c.position(d)))
	}

	s.m[nm] = d
}

func (s *scope) lookup(nm int) *Declarator {
	for s != nil {
		if d := s.m[nm]; d != nil {
			return d
		}

		s = s.parent
	}
	return nil
}
