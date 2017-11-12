// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate rm -f scanner.go trigraphs.go
//go:generate golex -o trigraphs.go trigraphs.l
//go:generate golex -o scanner.go scanner.l

//go:generate rm -f ast.go
//go:generate yy -kind Case -o parser.y -astImport "\"github.com/cznic/xc\";\"go/token\";\"fmt\"" -prettyString PrettyString parser.yy

//go:generate rm -f parser.go
//go:generate goyacc -o /dev/null -xegen xegen parser.y
//go:generate goyacc -o parser.go -fs -xe xegen -dlvalf "%v" -dlval "PrettyString(lval.Token)" parser.y
//go:generate rm -f xegen

//go:generate rm -f enum_string.go
//go:generate stringer -output enum_string.go -type=TypeKind,cond enum.go type.go
//go:generate sh -c "go test -run ^Example |fe"
//go:generate gofmt -l -s -w .

// Package c99 is a C99 compiler front end. (Work In Progress)
//
// This package is a modification of [1] supporting only SQLite.
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
	"strings"
	"sync"
)

type tweaks struct {
	cppExpandTest      bool // Fake includes
	enableEmptyStructs bool // struct{}
	enableTrigraphs    bool
	injectFinalNL      bool
}

// Translation unit context.
type context struct {
	errors       scanner.ErrorList
	exampleAST   interface{}
	exampleRule  int
	fset         *token.FileSet
	includePaths []string
	model        Model
	scope        *scope
	sync.Mutex
	sysIncludePaths []string
	tweaks          *tweaks
}

func newContext(fset *token.FileSet, t *tweaks) (*context, error) {
	return &context{
		fset:   fset,
		scope:  newScope(nil),
		tweaks: t,
	}, nil
}

func (c *context) err(n Node, msg string, args ...interface{}) { c.errPos(n.Pos(), msg, args...) }
func (c *context) newScope()                                   { c.scope = newScope(c.scope) }
func (c *context) position(n Node) token.Position              { return c.fset.PositionFor(n.Pos(), true) }

func (c *context) errPos(pos token.Pos, msg string, args ...interface{}) {
	c.Lock()
	c.errors.Add(c.fset.PositionFor(pos, true), fmt.Sprintf(msg, args...))
	c.Unlock()
}

func (c *context) error() error {
	c.Lock()

	defer c.Unlock()

	if len(c.errors) == 0 {
		return nil
	}

	c.errors.Sort()
	err := append(scanner.ErrorList(nil), c.errors...)
	return err
}

func (c *context) popScope() (old, new *scope) {
	old = c.scope
	c.scope = c.scope.parent
	return old, c.scope
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

type stringSource struct {
	*strings.Reader
	name string
	src  string
}

func newStringSource(name, src string) *stringSource { return &stringSource{name: name, src: src} }

func (s *stringSource) Cache([]uint32)       {}
func (s *stringSource) Cached() []uint32     { return nil }
func (s *stringSource) Close() error         { return nil }
func (s *stringSource) Name() string         { return s.name }
func (s *stringSource) Size() (int64, error) { return int64(len(s.src)), nil }

func (s *stringSource) ReadCloser() (io.ReadCloser, error) {
	s.Reader = strings.NewReader(s.src)
	return s, nil
}

type scope struct {
	m       map[int]*Declarator // name: *Declarator
	parent  *scope
	typedef bool
}

func newScope(parent *scope) *scope { return &scope{parent: parent} }

func (s *scope) predeclareTypedef(c *context, d *Declarator) {
	if s.m == nil {
		s.m = map[int]*Declarator{}
	}
	s.m[d.nm()] = d
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

func parse(fset *token.FileSet, in []Source, includePaths, sysIncludePaths []string, tweaks *tweaks) (*TranslationUnit, error) {
	model, err := newModel()
	if err != nil {
		return nil, err
	}

	ctx, err := newContext(fset, tweaks)
	if err != nil {
		return nil, err
	}

	ctx.model = model
	cpp := newCPP(ctx)
	cpp.includePaths = includePaths
	cpp.sysIncludePaths = sysIncludePaths
	r, err := cpp.parse(in...)
	if err != nil {
		return nil, err
	}

	lx, err := newLexer(ctx, "", 0, nil)
	if err != nil {
		return nil, err
	}

	p := newTokenPipe(1024)
	lx.tc = p

	go func() {
		defer p.close()

		if err := cpp.eval(r, p); err != nil {
			ctx.err(nopos, "%v", err)
		}
	}()

	if !lx.parse(TRANSLATION_UNIT) { // drain
		go func() {
			for range p.ch {
			}
		}()
	}
	if err := ctx.error(); err != nil {
		return nil, err
	}

	return lx.ast.(*TranslationUnit), nil
}
