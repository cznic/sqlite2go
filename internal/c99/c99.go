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

//go:generate stringer -output stringer.go -type=cond,Linkage,StorageDuration enum.go
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
	"sort"
	"strings"
	"sync"

	"github.com/cznic/ir"
)

type tweaks struct {
	cppExpandTest               bool // Fake includes
	enableAnonymousStructFields bool // struct{int;}
	enableEmptyStructs          bool // struct{}
	enableTrigraphs             bool
	injectFinalNL               bool
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

func (c *context) parse(in []Source) (*TranslationUnit, error) {
	cpp := newCPP(c)
	r, err := cpp.parse(in...)
	if err != nil {
		return nil, err
	}

	lx, err := newLexer(c, "", 0, nil)
	if err != nil {
		return nil, err
	}

	p := newTokenPipe(1024)
	lx.tc = p

	go func() {
		defer p.close()

		if err := cpp.eval(r, p); err != nil {
			c.err(nopos, "%v", err)
		}
	}()

	ok := lx.parse(TRANSLATION_UNIT)
	if err := c.error(); err != nil || !ok {
		go func() { // drain
			for range p.ch {
			}
		}()
		return nil, err
	}

	if c.scope.parent != nil {
		panic("internal error 7")
	}

	return lx.ast.(*TranslationUnit), nil
}

func (c *context) popScope() (old, new *scope) {
	old = c.scope
	c.scope = c.scope.parent
	return old, c.scope
}

func (c *context) ptrDiff() Type {
	d, ok := c.scope.lookupIdent(idPtrdiffT).(*Declarator)
	if !ok {
		panic("TODO")
	}

	if !d.DeclarationSpecifier.isTypedef() {
		panic(d.Type)
	}

	return d.Type
}

func (c *context) sizeof(t Type) Operand {
	sz := c.model.Sizeof(t)
	d, ok := c.scope.lookupIdent(idSizeT).(*Declarator)
	if !ok {
		return newIntConst(c, nopos, uint64(sz), UInt, ULong, ULongLong)
	}

	if !d.DeclarationSpecifier.isTypedef() {
		panic(d.Type)
	}

	return Operand{Type: d.Type, Value: &ir.Int64Value{Value: sz}}
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
	enumTags   map[int]Type // name: Type
	idents     map[int]Node // name: Node in {*Declarator, EnumerationConstant}
	labels     map[int]*LabeledStmt
	parent     *scope
	structTags map[int]*StructOrUnionSpecifier // name: *StructOrUnionSpecifier
	typedefs   map[int]struct{}

	typedef bool
}

func newScope(parent *scope) *scope { return &scope{parent: parent} }

func (s *scope) insertLabel(ctx *context, st *LabeledStmt) {
	for s.parent != nil && s.parent.parent != nil {
		s = s.parent
	}
	if s.labels == nil {
		s.labels = map[int]*LabeledStmt{}
	}
	if ex := s.labels[st.Token.Val]; ex != nil {
		panic("TODO")
	}

	s.labels[st.Token.Val] = st
}

func (s *scope) insertEnumTag(ctx *context, nm int, t Type) {
	for s.parent != nil {
		s = s.parent
	}
	if s.enumTags == nil {
		s.enumTags = map[int]Type{}
	}
	if ex := s.enumTags[nm]; ex != nil {
		if ex == t {
			return
		}

		panic("TODO")
	}

	s.enumTags[nm] = t
}

func (s *scope) insertDeclarator(ctx *context, d *Declarator) {
	if s.idents == nil {
		s.idents = map[int]Node{}
	}
	nm := d.nm()
	if ex := s.idents[nm]; ex != nil {
		panic("internal error 8")
	}

	s.idents[nm] = d
}

func (s *scope) insertEnumerationConstant(ctx *context, c *EnumerationConstant) {
	if s.idents == nil {
		s.idents = map[int]Node{}
	}
	nm := c.Token.Val
	if ex := s.idents[nm]; ex != nil {
		if ex == c {
			return
		}

		panic(ctx.position(c))
	}

	s.idents[nm] = c
}

func (s *scope) insertStructTag(ctx *context, ss *StructOrUnionSpecifier) {
	for s.parent != nil {
		s = s.parent
	}
	if s.structTags == nil {
		s.structTags = map[int]*StructOrUnionSpecifier{}
	}
	nm := ss.IdentifierOpt.Token.Val
	if ex := s.structTags[nm]; ex != nil && !ex.typ.Equal(ss.typ) {
		//dbg("", ex.typ)
		//dbg("", ss.typ)
		panic(ctx.position(ss))
	}

	s.structTags[nm] = ss
}

func (s *scope) insertTypedef(ctx *context, d *Declarator) {
	if s.typedefs == nil {
		s.typedefs = map[int]struct{}{}
	}
	s.typedefs[d.nm()] = struct{}{}
}

func (s *scope) isTypedef(nm int) bool {
	for s != nil {
		if _, ok := s.typedefs[nm]; ok {
			return true
		}

		s = s.parent
	}
	return false
}

func (s *scope) lookupIdent(nm int) Node {
	for s != nil {
		if n := s.idents[nm]; n != nil {
			return n
		}

		s = s.parent
	}
	return nil
}

func (s *scope) lookupLabel(nm int) Node {
	for s != nil {
		if n := s.labels[nm]; n != nil {
			if s.parent == nil && s.parent.parent != nil {
				panic("internal error")
			}

			return n
		}

		s = s.parent
	}
	return nil
}

func (s *scope) lookupStructTag(nm int) *StructOrUnionSpecifier {
	for s != nil {
		if n := s.structTags[nm]; n != nil {
			return n
		}

		s = s.parent
	}
	return nil
}

func (s *scope) String() string {
	var a []string
	for _, v := range s.idents {
		switch x := v.(type) {
		case *Declarator:
			a = append(a, string(dict.S(x.nm())))
		default:
			panic(fmt.Errorf("%T", x))
		}
	}
	sort.Strings(a)
	return "{" + strings.Join(a, ", ") + "}"
}
