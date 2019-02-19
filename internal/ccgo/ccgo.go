// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Need go1.1O+ b/c of https://github.com/golang/go/issues/23812

// +build go1.10

// Package ccgo translates c99 ASTs to Go source code. (Work In Progress)
//
// This package is a modification of [1] supporting only SQLite.
//
// [1] https://github.com/cznic/ccgo
package ccgo

import (
	"bytes"
	"container/list"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"os"
	"runtime"
	"sort"
	"strings"

	"github.com/cznic/sqlite2go/internal/c99"
	"go/ast"
)

var (
	isTesting bool // Test hook
)

const (
	mainSrc = `
func main() {
	psz := unsafe.Sizeof(uintptr(0))
	argv := crt.MustCalloc((len(os.Args) + 1) * int(psz))
	p := argv
	for _, v := range os.Args {
		*(*uintptr)(unsafe.Pointer(p)) = %[1]sCString(v)
		p += psz
	}
	a := os.Environ()
	env := crt.MustCalloc((len(a) + 1) * int(psz))
	p = env
	for _, v := range a {
		*(*uintptr)(unsafe.Pointer(p)) = %[1]sCString(v)
		p += psz
	}
	*(*uintptr)(unsafe.Pointer(%[2]s)) = env
	X_start(%[1]sNewTLS(), int32(len(os.Args)), argv)
}
`
	compactStack = 30
)

// Command outputs a Go program generated from in to w.
//
// No package or import clause is generated.
func Command(w io.Writer, in []*c99.TranslationUnit) (err error) { return command(w, "", &in) }

func command(w io.Writer, fn string, in *[]*c99.TranslationUnit, more ...func(*[]byte) error) (err error) {
	returned := false

	defer func() {
		if e := recover(); !returned && err == nil {
			err = fmt.Errorf("PANIC: %v\n%s", e, compact(string(debugStack()), compactStack))
		}
	}()

	err = newGen(w, in).gen(true, fn, more...)
	returned = true
	return err
}

// Package outputs a Go package generated from in to w.
//
// No package or import clause is generated.
func Package(w io.Writer, in []*c99.TranslationUnit) error {
	return newGen(w, &in).gen(false, "")
}

type gen struct {
	bss                    int64
	ds                     []byte
	enqueued               map[interface{}]struct{}
	errs                   scanner.ErrorList
	externs                map[int]*c99.Declarator
	filenames              map[string]struct{}
	fixArgs                map[*c99.Declarator]int
	fset                   *token.FileSet
	helpers                map[string]int
	in                     []*c99.TranslationUnit
	incompleteExternArrays map[int]*c99.Declarator
	initializedExterns     map[int]struct{}
	model                  c99.Model
	needBool2int           int
	nextLabel              int
	num                    int
	nums                   map[*c99.Declarator]int
	opaqueStructTags       map[int]struct{}
	out                    io.Writer
	out0                   bytes.Buffer
	producedDeclarators    map[*c99.Declarator]struct{}
	producedEnumTags       map[int]struct{}
	producedExterns        map[int]struct{}
	producedNamedTypes     map[int]struct{}
	producedStructTags     map[int]struct{}
	queue                  list.List
	staticDeclarators      map[int]*c99.Declarator
	strings                map[int]int64
	tCache                 map[tCacheKey]string
	text                   []int
	ts                     int64
	units                  map[*c99.Declarator]int

	mainFn     bool
	needAlloca bool
	needNZ32   bool //TODO -> crt
	needNZ64   bool //TODO -> crt
	needPreInc bool
}

func newGen(out io.Writer, in *[]*c99.TranslationUnit) *gen {
	defer func() { *in = nil }()

	return &gen{
		enqueued:  map[interface{}]struct{}{},
		externs:   map[int]*c99.Declarator{},
		filenames: map[string]struct{}{},
		fixArgs:   map[*c99.Declarator]int{},
		helpers:   map[string]int{},
		in:        *in,
		incompleteExternArrays: map[int]*c99.Declarator{},
		initializedExterns:     map[int]struct{}{},
		nums:                   map[*c99.Declarator]int{},
		opaqueStructTags:       map[int]struct{}{},
		out:                    out,
		producedDeclarators:    map[*c99.Declarator]struct{}{},
		producedEnumTags:       map[int]struct{}{},
		producedExterns:        map[int]struct{}{},
		producedNamedTypes:     map[int]struct{}{},
		producedStructTags:     map[int]struct{}{},
		staticDeclarators:      map[int]*c99.Declarator{},
		strings:                map[int]int64{},
		tCache:                 map[tCacheKey]string{},
		units:                  map[*c99.Declarator]int{},
	}
}

func (g *gen) enqueue(n interface{}) {
	if _, ok := g.enqueued[n]; ok {
		return
	}

	g.enqueued[n] = struct{}{}
	switch x := n.(type) {
	case *c99.Declarator:
		if x.Linkage == c99.LinkageNone {
			return
		}

		if x.DeclarationSpecifier.IsStatic() {
			g.enqueueNumbered(x)
			return
		}

		if x.DeclarationSpecifier.IsExtern() && c99.UnderlyingType(x.Type).Kind() != c99.Function {
			return
		}
	}

	g.queue.PushBack(n)
}

func (g *gen) enqueueNumbered(n *c99.Declarator) {
	if _, ok := g.nums[n]; ok {
		return
	}

	g.num++
	g.nums[n] = g.num
	g.queue.PushBack(n)
}

func env(key, val string) string {
	if s := os.Getenv(key); s != "" {
		return s
	}

	return val
}

func (g *gen) gen(cmd bool, fn string, more ...func(*[]byte) error) (err error) {
	if len(g.in) == 0 {
		return fmt.Errorf("no translation unit passed")
	}

	g.model = g.in[0].Model
	g.fset = g.in[0].FileSet
	for _, v := range g.in[1:] {
		if !g.model.Equal(v.Model) {
			return fmt.Errorf("translation units use different memory models")
		}
	}

	if err := g.collectSymbols(); err != nil {
		return err
	}

	g.wx(`
var _ unsafe.Pointer

const %s = uintptr(0)
`, null)
	switch {
	case cmd:
		sym, ok := g.externs[idStart]
		if !ok {
			todo("")
			break
		}

		// get some annoying windows mangling to work
		environName := "Xenviron"
		if env("GOOS", runtime.GOOS) == "windows" {
			environName = "X_environ"
		}
		g.wx(mainSrc, crt, environName)
		g.wdecl(g.define(sym)...)
	default:
		var a []string
		for nm := range g.externs {
			a = append(a, string(dict.S(nm)))
		}
		sort.Strings(a)
		var decls []ast.Decl
		for _, nm := range a {
			decls = append(decls, g.define(g.externs[dict.SID(nm)])...)
		}
		g.wdecl(decls...)
		todo("")
	}
	if err := g.errs.Err(); err != nil {
		return fmt.Errorf("%s", errString(err))
	}

	if g.needNZ64 {
		g.wx("\n\nfunc init() { nz64 = -nz64 }")
	}
	if g.needNZ32 {
		g.wx("\n\nfunc init() { nz32 = -nz32 }")
	}

	var a []string
	for k := range g.opaqueStructTags {
		a = append(a, string(dict.S(k)))
	}
	sort.Strings(a)
	for _, k := range a {
		tag := dict.SID(k)
		if _, ok := g.producedStructTags[tag]; !ok {
			g.wx("\ntype S%s struct{ uintptr }\n", k)
		}
	}

	if g.needPreInc {
		g.wx("\n\nfunc preinc(p *uintptr, n uintptr) uintptr { *p += n; return *p }")
	}
	if g.needAlloca {
		g.wx("\n\nfunc alloca(p *[]uintptr, n int) uintptr { r := %sMustMalloc(n); *p = append(*p, r); return r }", crt)
	}

	g.genHelpers()

	g.wx("\n\nvar (\n")
	if g.bss != 0 {
		g.wx("bss = %sBSS(&bssInit[0])\n", crt)
		g.wx("bssInit [%d]byte\n", g.bss)
	}
	if n := len(g.ds); n != 0 {
		if n < 16 {
			g.ds = append(g.ds, make([]byte, 16-n)...)
		}
		g.wx("ds = %sDS(dsInit)\n", crt)
		g.wx("dsInit = []byte{")
		if isTesting {
			g.wx("\n")
		}
		for i, v := range g.ds {
			g.wx("%#02x, ", v)
			if isTesting && i&15 == 15 {
				g.wx("// %#x\n", i&^15)
			}
		}
		g.wx("}\n")
	}
	if g.needNZ64 {
		g.wx("nz64 float64\n")
	}
	if g.needNZ32 {
		g.wx("nz32 float32\n")
	}
	g.wx("ts = %sTS(\"", crt)
	for _, v := range g.text {
		s := fmt.Sprintf("%q", dict.S(v))
		g.wx("%s\\x00", s[1:len(s)-1])
	}
	g.wx("\")\n)\n")
	g.in = nil
	cur := g.out0.Bytes()
	if err := newOpt().do(g.out, &g.out0, fn, g.needBool2int, more...); err != nil {
		g.out.Write(cur)
		return err
	}
	return nil
}

// dbg only
func (g *gen) position0(n c99.Node) token.Position { return g.in[0].FileSet.PositionFor(n.Pos(), true) }

func (g *gen) position(n *c99.Declarator) token.Position {
	return g.in[g.units[n]].FileSet.PositionFor(n.Pos(), true)
}

func (g *gen) getW() io.Writer {
	if !traceWrites {
		return &g.out0
	}
	return io.MultiWriter(&g.out0, os.Stderr)
}
func (g *gen) wx(s string, args ...interface{}) {
	if _, err := fmt.Fprintf(g.getW(), s, args...); err != nil {
		panic(err)
	}
}

func (g *gen) wdecl(decl ...ast.Decl) {
	if err := printDecls(g.getW(), decl); err != nil {
		panic(err)
	}
}

func (g *gen) collectSymbols() error {
	for unit, t := range g.in {
		for nm, n := range t.FileScope.Idents {
			switch x := n.(type) {
			case *c99.Declarator:
				g.units[x] = unit
				if d := x.Definition; d != nil {
					x = d
				}
				if x.Type.Kind() == c99.Function && x.FunctionDefinition == nil {
					continue
				}

				switch x.Linkage {
				case c99.LinkageExternal:
					if nm == idMain {
						x.Type = &c99.FunctionType{
							Params: []c99.Type{
								c99.Int,
								&c99.PointerType{Item: &c99.PointerType{Item: c99.Char}},
							},
							Result: c99.Int,
						}
					}
					ex := g.externs[nm]
					if ex == nil {
						g.externs[nm] = x
						break
					}

					if g.position(ex) == g.position(x) {
						break // ok
					}

					if ex.Type.Kind() == c99.Function {
						todo("")
					}

					if !ex.Type.IsCompatible(x.Type) {
						//typeDiff(ex.Type, x.Type)
						todo("", g.position(ex), ex.Type, g.position(x), x.Type)
					}

					if ex.Initializer != nil && x.Initializer != nil {
						todo("")
					}

					if prefer(x) && !prefer(ex) || !x.DeclarationSpecifier.IsExtern() && ex.DeclarationSpecifier.IsExtern() {
						g.externs[nm] = x
					}
				case c99.LinkageInternal:
					// ok
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

func (g gen) escaped(n *c99.Declarator) bool {
	if isVaList(n.Type) {
		return false
	}

	if n.AddressTaken {
		return true
	}

	switch c99.UnderlyingType(n.Type).(type) {
	case *c99.ArrayType:
		return !n.IsFunctionParameter
	case
		*c99.StructType,
		*c99.TaggedStructType,
		*c99.TaggedUnionType,
		*c99.UnionType:

		return n.IsTLD() || n.DeclarationSpecifier.IsStatic()
	default:
		return false
	}
}

func (g *gen) allocString(s int) int64 {
	if n, ok := g.strings[s]; ok {
		return n
	}

	r := g.ts
	g.strings[s] = r
	g.ts += int64(len(dict.S(s))) + 1
	g.text = append(g.text, s)
	return r
}

func (g *gen) tsString(s int, comm string) ast.Expr {
	// FIXME: comment
	return add(ident("ts"), intLit(g.allocString(s)))
}

func (g *gen) shiftMod(t c99.Type) int {
	if g.model.Sizeof(t) > 4 {
		return 64
	}

	return 32
}

func (g *gen) registerHelper(a ...interface{}) *ast.Ident {
	b := make([]string, len(a))
	for i, v := range a {
		b[i] = fmt.Sprint(v)
	}
	k := strings.Join(b, "$")
	if id := g.helpers[k]; id != 0 {
		return ident(fmt.Sprintf(b[0], id))
	}

	id := len(g.helpers) + 1
	g.helpers[k] = id
	return ident(fmt.Sprintf(b[0], id))
}

func (g *gen) genHelpers() {
	a := make([]string, 0, len(g.helpers))
	for k := range g.helpers {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "$")
		g.wx("\n\nfunc "+a[0], g.helpers[k])
		switch a[0] {
		case "add%d", "and%d", "div%d", "mod%d", "mul%d", "or%d", "sub%d", "xor%d":
			// eg.: [0: "add%d" 1: op "+" 2: lhs type "uint16" 3: rhs type "uint8" 4: promotion type "int32"]
			g.wx("(p *%[2]s, v %[3]s) (r %[2]s) { r = %[2]s(%[4]s(*p) %[1]s %[4]s(v)); *p = r; return r }", a[1], a[2], a[3], a[4])
		case "and%db", "or%db", "xor%db":
			// eg.: [0: "or%db" 1: op "|" 2: lhs type "uint16" 3: rhs type "uint8" 4: promotion type "int32" 5: packed type "uint32" 6: bitoff 7: promotion type bits 8: bits 9: lhs type bits]
			g.wx(`(p *%[5]s, v %[3]s) (r %[2]s) {
r = %[2]s((%[4]s(%[2]s(*p>>%[6]s))<<(%[7]s-%[8]s)>>(%[7]s-%[8]s)) %[1]s %[4]s(v))
*p = (*p &^ ((1<<%[8]s - 1) << %[6]s)) | (%[5]s(r) << %[6]s & ((1<<%[8]s - 1) << %[6]s))
return r<<(%[9]s-%[8]s)>>(%[9]s-%[8]s)
}`, a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9])
		case "set%d": // eg.: [0: "set%d" 1: op "" 2: operand type "uint32"]
			g.wx("(p *%[2]s, v %[2]s) %[2]s { *p = v; return v }", a[1], a[2])
		case "set%db":
			// eg.: [0: "set%db" 1: packed type "uint32" 2: lhs type "int16" 3: rhs type "char" 4: bitoff 5: bits 6: lhs type bits]
			g.wx(`(p *%[1]s, v %[3]s) (r %[2]s) { 
r = %[2]s(v)
*p = (*p &^ ((1<<%[5]s - 1) << %[4]s)) | (%[1]s(r) << %[4]s & ((1<<%[5]s - 1) << %[4]s))
return r<<(%[6]s-%[5]s)>>(%[6]s-%[5]s)
}`, a[1], a[2], a[3], a[4], a[5], a[6])
		case "rsh%d":
			// eg.: [0: "rsh%d" 1: op ">>" 2: lhs type "uint32" 3: promotion type]
			g.wx("(p *%[2]s, v uint) (r %[2]s) { r = %[2]s(%[3]s(*p) >> v); *p = r; return r }", a[1], a[2], a[3])
		case "fn%d":
			// eg.: [0: "fn%d" 1: type "unc()"]
			g.wx("(p uintptr) %[1]s { return *(*%[1]s)(unsafe.Pointer(&p)) }", a[1])
		case "fp%d":
			g.wx("(f %[1]s) uintptr { return *(*uintptr)(unsafe.Pointer(&f)) }", a[1])
		case "postinc%d":
			// eg.: [0: "postinc%d" 1: operand type "int32" 2: delta "1"]
			g.wx("(p *%[1]s) %[1]s { r := *p; *p += %[2]s; return r }", a[1], a[2])
		case "preinc%d":
			// eg.: [0: "preinc%d" 1: operand type "int32" 2: delta "1"]
			g.wx("(p *%[1]s) %[1]s { *p += %[2]s; return *p }", a[1], a[2])
		case "postinc%db":
			// eg.: [0: "postinc%db" 1: delta "1" 2: lhs type "int32" 3: pack type "uint8" 4: lhs type bits "32" 5: bits "3" 6: bitoff "2"]
			g.wx(`(p *%[3]s) %[2]s {
r := %[2]s(*p>>%[6]s)<<(%[4]s-%[5]s)>>(%[4]s-%[5]s)
*p = (*p &^ ((1<<%[5]s - 1) << %[6]s)) | (%[3]s(r+%[1]s) << %[6]s & ((1<<%[5]s - 1) << %[6]s))
return r
}`, a[1], a[2], a[3], a[4], a[5], a[6])
		case "preinc%db":
			// eg.: [0: "preinc%db" 1: delta "1" 2: lhs type "int32" 3: pack type "uint8" 4: lhs type bits "32" 5: bits "3" 6: bitoff "2"]
			g.wx(`(p *%[3]s) %[2]s {
r := (%[2]s(*p>>%[6]s+%[1]s)<<(%[4]s-%[5]s)>>(%[4]s-%[5]s))
*p = (*p &^ ((1<<%[5]s - 1) << %[6]s)) | (%[3]s(r) << %[6]s & ((1<<%[5]s - 1) << %[6]s))
return r
}`, a[1], a[2], a[3], a[4], a[5], a[6])
		case "float2int%d":
			// eg.: [0: "float2int%d" 1: type "uint64" 2: max "18446744073709551615"]
			g.wx("(f float32) %[1]s { if f > %[2]s { return 0 }; return %[1]s(f) }", a[1], a[2])
		default:
			todo("%q", a)
		}
	}
}
