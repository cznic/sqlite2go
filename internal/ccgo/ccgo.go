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
	"sort"
	"strings"

	"github.com/cznic/sqlite2go/internal/c99"
)

//TODO do not pass pointers to helpers, use uintptrs everywhere.

var (
	isTesting bool // Test hook
)

// Command outputs a Go program generated from in to w.
//
// No package or import clause is generated.
func Command(w io.Writer, in []*c99.TranslationUnit) (err error) {
	return newGen(w, in).gen(true)
}

// Package outputs a Go package generated from in to w.
//
// No package or import clause is generated.
func Package(w io.Writer, in []*c99.TranslationUnit) error {
	return newGen(w, in).gen(false)
}

type gen struct {
	bss                 int64
	ds                  []byte
	errs                scanner.ErrorList
	externs             map[int]*c99.Declarator
	fnTypes             map[string]int
	fpTypes             map[string]int
	fset                *token.FileSet
	helpers             map[string]int
	in                  []*c99.TranslationUnit
	internalNames       map[int]struct{}          //TODO-?
	internals           []map[int]*c99.Declarator //TODO-?
	model               c99.Model
	needBool2int        int
	nextLabel           int
	num                 int
	nums                map[*c99.Declarator]int
	opaqueStructTags    map[int]struct{}
	orTypes             map[string]int
	out                 io.Writer
	out0                bytes.Buffer
	postDecIBitsTypes   map[string]int
	postDecTypes        map[string]int
	postDecUBitsTypes   map[string]int
	postIncTypes        map[string]int
	postIncUBitsTypes   map[string]int
	preDecTypes         map[string]int
	preDecUBitsTypes    map[string]int
	preIncTypes         map[string]int
	preIncUBitsTypes    map[string]int
	producedDeclarators map[*c99.Declarator]struct{}
	producedEnumTags    map[int]struct{}
	producedNamedTypes  map[int]struct{}
	producedStructTags  map[int]struct{}
	queue               list.List
	rshTypes            map[string]int
	strings             map[int]int64
	subTypes            map[string]int
	tCache              map[tCacheKey]string
	text                []int
	ts                  int64
	units               map[*c99.Declarator]int
	xorTypes            map[string]int

	needAlloca  bool
	needNZ32    bool //TODO -> crt
	needNZ64    bool //TODO -> crt
	needPostDec bool
	needPostInc bool
	needPreDec  bool
	needPreInc  bool
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		externs:             map[int]*c99.Declarator{},
		fnTypes:             map[string]int{},
		fpTypes:             map[string]int{},
		helpers:             map[string]int{},
		in:                  in,
		internalNames:       map[int]struct{}{},
		internals:           make([]map[int]*c99.Declarator, len(in)),
		nums:                map[*c99.Declarator]int{},
		opaqueStructTags:    map[int]struct{}{},
		orTypes:             map[string]int{},
		out:                 out,
		postDecIBitsTypes:   map[string]int{},
		postDecTypes:        map[string]int{},
		postDecUBitsTypes:   map[string]int{},
		postIncTypes:        map[string]int{},
		postIncUBitsTypes:   map[string]int{},
		preDecTypes:         map[string]int{},
		preDecUBitsTypes:    map[string]int{},
		preIncTypes:         map[string]int{},
		preIncUBitsTypes:    map[string]int{},
		producedDeclarators: map[*c99.Declarator]struct{}{},
		producedEnumTags:    map[int]struct{}{},
		producedNamedTypes:  map[int]struct{}{},
		producedStructTags:  map[int]struct{}{},
		rshTypes:            map[string]int{},
		strings:             map[int]int64{},
		subTypes:            map[string]int{},
		tCache:              map[tCacheKey]string{},
		units:               map[*c99.Declarator]int{},
		xorTypes:            map[string]int{},
	}
}

func (g *gen) enqueue(n interface{}) {
	switch x := n.(type) {
	case *c99.Declarator:
		if x.Linkage == c99.LinkageNone {
			return
		}

		if x.DeclarationSpecifier.IsStatic() {
			g.enqueueNumbered(x)
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

func (g *gen) gen(cmd bool) (err error) {
	defer func() {
		switch e := recover(); e.(type) {
		case nil:
			// nop
		default:
			err = fmt.Errorf("%v", compact(fmt.Sprintf("PANIC: %v\n%s", e, debugStack()), 15))
		}
	}()

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

	g.w(`
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

		g.w(`

func main() {
	argv := crt.MustCalloc((len(os.Args)+1) * int(unsafe.Sizeof(uintptr(0))))
	p := argv
	for _, v := range os.Args {
		*(*uintptr)(unsafe.Pointer(p)) = %[1]sCString(v)
		p += unsafe.Sizeof(uintptr(0))
	}
	X_start(%[1]sNewTLS(), int32(len(os.Args)), argv)
}
`, crt)
		g.define(sym)
	default:
		var a []string
		for nm := range g.externs {
			a = append(a, string(dict.S(nm)))
		}
		sort.Strings(a)
		for _, nm := range a {
			g.define(g.externs[dict.SID(nm)])
		}
		todo("")
	}
	if err := g.errs.Err(); err != nil {
		return fmt.Errorf("%s", errString(err))
	}

	if g.needNZ64 {
		g.w("\n\nfunc init() { nz64 = -nz64 }")
	}
	if g.needNZ32 {
		g.w("\n\nfunc init() { nz32 = -nz32 }")
	}

	var a []string
	for k := range g.opaqueStructTags {
		a = append(a, string(dict.S(k)))
	}
	sort.Strings(a)
	for _, k := range a {
		tag := dict.SID(k)
		if _, ok := g.producedStructTags[tag]; !ok {
			g.w("\ntype S%s struct{ uintptr }\n", k)
		}
	}

	if g.needPostInc {
		g.w("\n\nfunc postinc(p *uintptr, n uintptr) uintptr { r := *p; *p += n; return r }")
	}
	if g.needPostDec {
		g.w("\n\nfunc postdec(p *uintptr, n uintptr) uintptr { r := *p; *p -= n; return r }")
	}
	if g.needPreInc {
		g.w("\n\nfunc preinc(p *uintptr, n uintptr) uintptr { *p += n; return *p }")
	}
	if g.needPreDec {
		g.w("\n\nfunc predec(p *uintptr, n uintptr) uintptr { *p -= n; return *p }")
	}
	if g.needAlloca {
		g.w("\n\nfunc alloca(p *[]uintptr, n int) uintptr { r := %sMustMalloc(n); *p = append(*p, r); return r }", crt)
	}

	a = a[:0]
	for k := range g.postIncTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc postinc%d(n *%[2]s) %[2]s { r := *n; *n++; return r }", g.postIncTypes[k], k)
	}
	a = a[:0]
	for k := range g.preDecTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc predec%d(n *%[2]s) %[2]s { *n--; return *n }", g.preDecTypes[k], k)
	}
	a = a[:0]
	for k := range g.postDecTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc postdec%d(n *%[2]s) %[2]s { r := *n; *n--; return r }", g.postDecTypes[k], k)
	}
	a = a[:0]
	for k := range g.postDecUBitsTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "|")
		g.w("\n\nfunc postdecu%d(n uintptr, m %[2]s, off uint) %[2]s {", g.postDecUBitsTypes[k], a[0])
		g.w(`
pf := *(*%[2]s)(unsafe.Pointer(n))
r := %[1]s(pf)&m>>off
*(*%[2]s)(unsafe.Pointer(n)) = pf&^%[2]s(m)|%[2]s((r-1)<<off&m)
return r
}`, a[0], a[1])
	}
	a = a[:0]
	for k := range g.postDecIBitsTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "|")
		g.w("\n\nfunc postdeci%d(n uintptr, s, m %[2]s, off uint) %[2]s {", g.postDecIBitsTypes[k], a[0])
		g.w(`
pf := *(*%[2]s)(unsafe.Pointer(n))
r := %[1]s(pf)&m
if r&s != 0 {
	r |= ^m
}
r >>= off
*(*%[2]s)(unsafe.Pointer(n)) = pf&^%[2]s(m)|%[2]s((r-1)<<off&m)
return r
}`, a[0], a[1])
	}
	a = a[:0]
	for k := range g.postIncUBitsTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "|")
		g.w("\n\nfunc postincu%d(n uintptr, m %[2]s, off uint) %[2]s {", g.postIncUBitsTypes[k], a[0])
		g.w(`
pf := *(*%[2]s)(unsafe.Pointer(n))
r := %[1]s(pf)&m>>off
*(*%[2]s)(unsafe.Pointer(n)) = pf&^%[2]s(m)|%[2]s((r+1)<<off&m)
return r
}`, a[0], a[1])
	}
	a = a[:0]
	for k := range g.preDecUBitsTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "|")
		g.w("\n\nfunc predecu%d(n uintptr, m %[2]s, off uint) %[2]s {", g.preDecUBitsTypes[k], a[0])
		g.w(`
pf := *(*%[2]s)(unsafe.Pointer(n))
r := %[1]s(pf)&m>>off-1
*(*%[2]s)(unsafe.Pointer(n)) = pf&^%[2]s(m)|%[2]s(r<<off&m)
return r
}`, a[0], a[1])
	}
	a = a[:0]
	for k := range g.preIncUBitsTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "|")
		g.w("\n\nfunc preincu%d(n uintptr, m %[2]s, off uint) %[2]s {", g.preIncUBitsTypes[k], a[0])
		g.w(`
pf := *(*%[2]s)(unsafe.Pointer(n))
r := %[1]s(pf)&m>>off+1
*(*%[2]s)(unsafe.Pointer(n)) = pf&^%[2]s(m)|%[2]s(r<<off&m)
return r
}`, a[0], a[1])
	}
	a = a[:0]
	for k := range g.preIncTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc preinc%d(n *%[2]s) %[2]s { *n++; return *n }", g.preIncTypes[k], k)
	}
	a = a[:0]
	for k := range g.orTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc or%d(n *%[2]s, m %[2]s) %[2]s { *n |= m; return *n }", g.orTypes[k], k)
	}
	a = a[:0]
	for k := range g.xorTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc xor%d(n *%[2]s, m %[2]s) %[2]s { *n ^= m; return *n }", g.xorTypes[k], k)
	}
	a = a[:0]
	for k := range g.rshTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc rsh%d(n *%[2]s, m int) %[2]s { *n >>= uint(m); return *n }", g.rshTypes[k], k)
	}
	a = a[:0]
	for k := range g.subTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc sub%d(n *%[2]s, m %[2]s) %[2]s { *n -= m; return *n }", g.subTypes[k], k)
	}
	a = a[:0]
	for k := range g.fpTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc fp%d(f %[2]s) uintptr { return *(*uintptr)(unsafe.Pointer(&f)) }", g.fpTypes[k], k)
	}
	a = a[:0]
	for k := range g.fnTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc fn%d(f uintptr) %[2]s { return *(*%[2]s)(unsafe.Pointer(&f)) }", g.fnTypes[k], k)
	}

	g.genHelpers()
	g.w("\n\nvar (\n")
	if g.bss != 0 {
		g.w("bss = %sBSS(&bssInit[0])\n", crt)
		g.w("bssInit [%d]byte\n", g.bss)
	}
	if n := len(g.ds); n != 0 {
		if n < 16 {
			g.ds = append(g.ds, make([]byte, 16-n)...)
		}
		g.w("ds = %sDS(dsInit)\n", crt)
		g.w("dsInit = []byte{")
		if isTesting {
			g.w("\n")
		}
		for i, v := range g.ds {
			g.w("%#02x, ", v)
			if isTesting && i&15 == 15 {
				g.w("// %#x\n", i&^15)
			}
		}
		g.w("}\n")
	}
	if g.needNZ64 {
		g.w("nz64 float64\n")
	}
	if g.needNZ32 {
		g.w("nz32 float32\n")
	}
	g.w("ts = %sTS(\"", crt)
	for _, v := range g.text {
		s := fmt.Sprintf("%q", dict.S(v))
		g.w("%s\\x00", s[1:len(s)-1])
	}
	g.w("\")\n)\n")
	return newOpt().do(g.out, &g.out0, testFn, g.needBool2int)
}

// dbg only
func (g *gen) position0(n c99.Node) token.Position { return g.in[0].FileSet.PositionFor(n.Pos(), true) }

func (g *gen) position(n *c99.Declarator) token.Position {
	return g.in[g.units[n]].FileSet.PositionFor(n.Pos(), true)
}

func (g *gen) w(s string, args ...interface{}) {
	if _, err := fmt.Fprintf(&g.out0, s, args...); err != nil {
		panic(err)
	}
	if traceWrites {
		fmt.Fprintf(os.Stderr, s, args...)
	}
}

func (g *gen) collectSymbols() error {
	for unit, t := range g.in {
		internal := map[int]*c99.Declarator{}
		g.internals[unit] = internal
		for nm, n := range t.FileScope.Idents {
			switch x := n.(type) {
			case *c99.Declarator:
				g.units[x] = unit
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
					if ex, ok := g.externs[nm]; ok {
						if g.position(ex) == g.position(x) {
							break // ok
						}

						if ex.Type.Kind() == c99.Function {
							todo("")
						}

						if !ex.Type.IsCompatible(x.Type) {
							todo("", g.position(ex), ex.Type, g.position(x), x.Type)
						}

						if ex.Initializer != nil && x.Initializer != nil {
							todo("")
						}

						if prefer(ex.Type) || !prefer(x.Type) {
							break // ok
						}
					}

					g.externs[nm] = x
				case c99.LinkageInternal:
					if _, ok := internal[nm]; ok {
						todo("")
					}

					internal[nm] = x
					if _, ok := g.internalNames[nm]; ok {
						g.num++
						g.nums[x] = g.num
					}
					g.internalNames[nm] = struct{}{}
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

		return n.IsTLD() || n.DeclarationSpecifier.IsStatic() || g.hasBitFields(n.Type)
	default:
		return false
	}
}

func (g *gen) hasBitFields(t c99.Type) bool {
	t = c99.UnderlyingType(t)
	if k := t.Kind(); k != c99.Struct && k != c99.Union {
		return false
	}

	switch x := t.(type) {
	case *c99.StructType:
		for _, v := range x.Fields {
			if v.Bits != 0 {
				return true
			}
		}
	case *c99.UnionType:
		for _, v := range x.Fields {
			if v.Bits != 0 {
				return true
			}
		}
	}
	return false
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

func (g *gen) registerType(m map[string]int, t c99.Type, more ...string) int { //TODO-
	s := g.typ(t)
	if len(more) != 0 {
		s += "|" + strings.Join(more, "|")
	}
	if id := m[s]; id != 0 {
		return id
	}

	m[s] = len(m) + 1
	return len(m)
}

func (g *gen) shiftMod(t c99.Type) int {
	if g.model.Sizeof(t) > 4 {
		return 64
	}

	return 32
}

func (g *gen) registerHelper(a ...interface{}) int {
	b := make([]string, len(a))
	for i, v := range a {
		b[i] = fmt.Sprint(v)
	}
	k := strings.Join(b, "$")
	if id := g.helpers[k]; id != 0 {
		return id
	}

	id := len(g.helpers) + 1
	g.helpers[k] = id
	return id
}

func (g *gen) genHelpers() {
	a := make([]string, 0, len(g.helpers))
	for k := range g.helpers {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "$")
		switch a[0] {
		case "add%d", "and%d", "div%d", "mod%d", "mul%d":
			g.asop(g.helpers[k], a)
		case "and%db", "or%db", "xor%db":
			g.bitAsop(g.helpers[k], a)
		case "set%d":
			g.set(g.helpers[k], a)
		case "set%db":
			g.bitSet(g.helpers[k], a)
		default:
			todo("%q", a)
		}
	}
}

// eg.: [0: "add%d" 1: op "" 2: operand type "uint32"]
func (g *gen) asop(k int, a []string) {
	g.w(`

func `+a[0]+`(p uintptr, v %[3]s) %[3]s {
	*(*%[3]s)(unsafe.Pointer(p)) %[2]s= v
	return *(*%[3]s)(unsafe.Pointer(p))
}`, k, a[1], a[2])
}

// eg.: [0: "or%db" 1: op "|" 2: operand type "uint32" 3: pack type "uint32" 4: pack size "32" 5: bits "3" 6: bitoff "14"]
func (g *gen) bitAsop(k int, a []string) {
	g.w(`

func `+a[0]+`(p uintptr, v %[3]s) %[3]s {
	r := %[3]s(*(*%[4]s)(unsafe.Pointer(p))<<(%[5]s-%[6]s-%[7]s)>>(%[5]s-%[7]s)) %[2]s v
	*(*%[4]s)(unsafe.Pointer(p)) = ((*(*%[4]s)(unsafe.Pointer(p))) &^ ((1<<%[6]s - 1) << %[7]s)) | (%[4]s(r) << %[7]s & ((1<<%[6]s - 1) << %[7]s))
	return r
}
`, k, a[1], a[2], a[3], a[4], a[5], a[6])

}

// eg.: [0: "set%db" 1: op "=" 2: operand type "uint32" 3: pack type "uint32" 4: pack size "32" 5: bits "3" 6: bitoff "14"]
func (g *gen) bitSet(k int, a []string) {
	g.w(`

func `+a[0]+`(p uintptr, v %[3]s) %[3]s {
	*(*%[4]s)(unsafe.Pointer(p)) = ((*(*%[4]s)(unsafe.Pointer(p))) &^ ((1<<%[6]s - 1) << %[7]s)) | (%[4]s(v) << %[7]s & ((1<<%[6]s - 1) << %[7]s))
	return v
}
`, k, a[1], a[2], a[3], a[4], a[5], a[6])
}

// eg.: [0: "set%d" 1: op "" 2: operand type "uint32"]
func (g *gen) set(k int, a []string) {
	g.w("\n\nfunc "+a[0]+"(p uintptr, v %[3]s) %[3]s { *(*%[3]s)(unsafe.Pointer(p)) = v; return v }", k, a[1], a[2])
}
