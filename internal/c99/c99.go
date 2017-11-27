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

var (
	_ Source = (*FileSource)(nil)
	_ Source = (*StringSource)(nil)
)

// TranslationUnit represents a translation unit, see [0]6.9.
type TranslationUnit struct {
	ExternalDeclarationList *ExternalDeclarationList
	FileScope               *Scope
	FileSet                 *token.FileSet
	Model                   Model
}

// Tweaks amend the behavior of the parser.
type Tweaks struct {
	cppExpandTest               bool // Fake includes
	EnableAnonymousStructFields bool // struct{int;}
	EnableEmptyStructs          bool // struct{}
	EnableTrigraphs             bool
	InjectFinalNL               bool // Specs want the source to always end in a newline.
}

// Translate preprocesses, parses and type checks a translation unit using fset
// to record node and error positions, includePaths and sysIncludePaths for
// looking for "foo.h" and <foo.h> files. A special path "@" is interpretted as
// 'the same directory as where the file with the #include is'. The input
// consists of sources which must include any predefined/builtin stuff.
//
// The returned scope is the file scope of the Translation unit.
func Translate(fset *token.FileSet, tweaks *Tweaks, includePaths, sysIncludePaths []string, sources ...Source) (*TranslationUnit, error) {
	model, err := newModel()
	if err != nil {
		return nil, err
	}

	ctx, err := newContext(fset, tweaks)
	if err != nil {
		return nil, err
	}

	ctx.model = model
	ctx.includePaths = append([]string(nil), includePaths...)
	ctx.sysIncludePaths = append([]string(nil), sysIncludePaths...)
	t, err := ctx.parse(sources)
	if err != nil {
		return nil, err
	}

	if err := t.ExternalDeclarationList.check(ctx); err != nil {
		return nil, err
	}

	if err := ctx.error(); err != nil {
		return nil, err
	}

	return t, nil
}

// Translation unit context.
type context struct {
	errors       scanner.ErrorList
	exampleAST   interface{}
	exampleRule  int
	fset         *token.FileSet
	includePaths []string
	model        Model
	scope        *Scope
	sync.Mutex
	sysIncludePaths []string
	tweaks          *Tweaks
}

func newContext(fset *token.FileSet, t *Tweaks) (*context, error) {
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

	if c.scope.Parent != nil {
		panic("internal error 7")
	}

	return lx.ast.(*TranslationUnit), nil
}

func (c *context) popScope() (old, new *Scope) {
	old = c.scope
	c.scope = c.scope.Parent
	return old, c.scope
}

func (c *context) ptrDiff() Type {
	d, ok := c.scope.LookupIdent(idPtrdiffT).(*Declarator)
	if !ok {
		panic("TODO")
	}

	if !d.DeclarationSpecifier.IsTypedef() {
		panic(d.Type)
	}

	return d.Type
}

func (c *context) sizeof(t Type) Operand {
	sz := c.model.Sizeof(t)
	d, ok := c.scope.LookupIdent(idSizeT).(*Declarator)
	if !ok {
		return newIntConst(c, nopos, uint64(sz), UInt, ULong, ULongLong)
	}

	if !d.DeclarationSpecifier.IsTypedef() {
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

// Source represents parser's input.
type Source interface {
	Cache([]uint32)                     // Optionally cache the encoded source. Can be a no-operation.
	Cached() []uint32                   // Return nil or the optionally encoded source cached by earlier call to Cache.
	Name() string                       // Result will be used in reporting source code positions.
	ReadCloser() (io.ReadCloser, error) // Where to read the source from
	Size() (int64, error)               // Report the size of the source in bytes.
}

// FileSource is a Source reading from a named file.
type FileSource struct {
	*bufio.Reader
	f    *os.File
	path string
}

// NewFileSource returns a newly created *FileSource reading from name.
func NewFileSource(name string) *FileSource { return &FileSource{path: name} }

// Cache implements Source.
func (s *FileSource) Cache([]uint32) {}

// Cached implements Source.
func (s *FileSource) Cached() []uint32 { return nil }

// Close implements io.ReadCloser.
func (s *FileSource) Close() error { return s.f.Close() }

// Name implements Source.
func (s *FileSource) Name() string { return s.path }

// ReadCloser implements Source.
func (s *FileSource) ReadCloser() (io.ReadCloser, error) {
	f, err := os.Open(s.path)
	if err != nil {
		return nil, err
	}

	s.f = f
	s.Reader = bufio.NewReader(f)
	return s, nil
}

// Size implements Source.
func (s *FileSource) Size() (int64, error) {
	fi, err := os.Stat(s.path)
	if err != nil {
		return 0, err
	}

	return fi.Size(), nil
}

// StringSource is a Source reading from a string.
type StringSource struct {
	*strings.Reader
	name string
	src  string
}

// NewStringSource returns a newly created *StringSource reading from src and
// having the presumed name.
func NewStringSource(name, src string) *StringSource { return &StringSource{name: name, src: src} }

// Cache implements Source.
func (s *StringSource) Cache([]uint32) {}

// Cached implements Source.
func (s *StringSource) Cached() []uint32 { return nil }

// Close implements io.ReadCloser.
func (s *StringSource) Close() error { return nil }

// Name implements Source.
func (s *StringSource) Name() string { return s.name }

// Size implements Source.
func (s *StringSource) Size() (int64, error) { return int64(len(s.src)), nil }

// ReadCloser implements Source.
func (s *StringSource) ReadCloser() (io.ReadCloser, error) {
	s.Reader = strings.NewReader(s.src)
	return s, nil
}

// Scope binds names to declarations.
type Scope struct {
	EnumTags   map[int]Type         // name ID: Type
	Idents     map[int]Node         // name ID: Node in {*Declarator, EnumerationConstant}
	Labels     map[int]*LabeledStmt // name ID: label
	Parent     *Scope
	StructTags map[int]*StructOrUnionSpecifier // name ID: *StructOrUnionSpecifier

	// parser support
	typedefs map[int]struct{} // name: nothing
	typedef  bool
}

func newScope(parent *Scope) *Scope { return &Scope{Parent: parent} }

func (s *Scope) insertLabel(ctx *context, st *LabeledStmt) {
	for s.Parent != nil && s.Parent.Parent != nil {
		s = s.Parent
	}
	if s.Labels == nil {
		s.Labels = map[int]*LabeledStmt{}
	}
	if ex := s.Labels[st.Token.Val]; ex != nil {
		panic("TODO")
	}

	s.Labels[st.Token.Val] = st
}

func (s *Scope) insertEnumTag(ctx *context, nm int, t Type) {
	for s.Parent != nil {
		s = s.Parent
	}
	if s.EnumTags == nil {
		s.EnumTags = map[int]Type{}
	}
	if ex := s.EnumTags[nm]; ex != nil {
		if ex == t {
			return
		}

		panic("TODO")
	}

	s.EnumTags[nm] = t
}

func (s *Scope) insertDeclarator(ctx *context, d *Declarator) {
	if s.Idents == nil {
		s.Idents = map[int]Node{}
	}
	nm := d.Name()
	if ex := s.Idents[nm]; ex != nil {
		panic("internal error 8")
	}

	s.Idents[nm] = d
}

func (s *Scope) insertEnumerationConstant(ctx *context, c *EnumerationConstant) {
	if s.Idents == nil {
		s.Idents = map[int]Node{}
	}
	nm := c.Token.Val
	if ex := s.Idents[nm]; ex != nil {
		if ex == c {
			return
		}

		panic(ctx.position(c))
	}

	s.Idents[nm] = c
}

func (s *Scope) insertStructTag(ctx *context, ss *StructOrUnionSpecifier) {
	for s.Parent != nil {
		s = s.Parent
	}
	if s.StructTags == nil {
		s.StructTags = map[int]*StructOrUnionSpecifier{}
	}
	nm := ss.IdentifierOpt.Token.Val
	if ex := s.StructTags[nm]; ex != nil && !ex.typ.Equal(ss.typ) {
		//dbg("", ex.typ)
		//dbg("", ss.typ)
		panic(ctx.position(ss))
	}

	s.StructTags[nm] = ss
}

func (s *Scope) insertTypedef(ctx *context, d *Declarator) {
	if s.typedefs == nil {
		s.typedefs = map[int]struct{}{}
	}
	s.typedefs[d.Name()] = struct{}{}
}

func (s *Scope) isTypedef(nm int) bool {
	for s != nil {
		if _, ok := s.typedefs[nm]; ok {
			return true
		}

		s = s.Parent
	}
	return false
}

// LookupIdent will return the Node associated with name ID nm.
func (s *Scope) LookupIdent(nm int) Node {
	for s != nil {
		if n := s.Idents[nm]; n != nil {
			return n
		}

		s = s.Parent
	}
	return nil
}

// LookupLabel will return the Node associated with label ID nm.
func (s *Scope) LookupLabel(nm int) Node {
	for s != nil {
		if n := s.Labels[nm]; n != nil {
			if s.Parent == nil && s.Parent.Parent != nil {
				panic("internal error")
			}

			return n
		}

		s = s.Parent
	}
	return nil
}

func (s *Scope) lookupStructTag(nm int) *StructOrUnionSpecifier {
	for s != nil {
		if n := s.StructTags[nm]; n != nil {
			return n
		}

		s = s.Parent
	}
	return nil
}

func (s *Scope) String() string {
	var a []string
	for _, v := range s.Idents {
		switch x := v.(type) {
		case *Declarator:
			a = append(a, string(dict.S(x.Name())))
		default:
			panic(fmt.Errorf("%T", x))
		}
	}
	sort.Strings(a)
	return "{" + strings.Join(a, ", ") + "}"
}
