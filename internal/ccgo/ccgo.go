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
	addTypes            map[string]int
	assignTypes         map[string]int
	bss                 int64
	divTypes            map[string]int
	ds                  []byte
	errs                scanner.ErrorList
	externs             map[int]*c99.Declarator
	fnTypes             map[string]int
	fpTypes             map[string]int
	fset                *token.FileSet
	in                  []*c99.TranslationUnit
	internalNames       map[int]struct{}          //TODO-?
	internals           []map[int]*c99.Declarator //TODO-?
	modTypes            map[string]int
	model               c99.Model
	mulTypes            map[string]int
	needBool2int        int
	nextLabel           int
	num                 int
	nums                map[*c99.Declarator]int
	opaqueStructTags    map[int]struct{}
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
	setBitsTypes        map[string]int
	shlTypes            map[string]int
	shrTypes            map[string]int
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
		addTypes:            map[string]int{},
		assignTypes:         map[string]int{},
		divTypes:            map[string]int{},
		externs:             map[int]*c99.Declarator{},
		fnTypes:             map[string]int{},
		fpTypes:             map[string]int{},
		in:                  in,
		internalNames:       map[int]struct{}{},
		internals:           make([]map[int]*c99.Declarator, len(in)),
		modTypes:            map[string]int{},
		mulTypes:            map[string]int{},
		nums:                map[*c99.Declarator]int{},
		opaqueStructTags:    map[int]struct{}{},
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
		setBitsTypes:        map[string]int{},
		shlTypes:            map[string]int{},
		shrTypes:            map[string]int{},
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
	for k := range g.assignTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc set%d(l *%[2]s, r %[2]s) %[2]s { *l = r; return r }", g.assignTypes[k], k)
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
	for k := range g.setBitsTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		a := strings.Split(k, "|")
		g.w("\n\nfunc setb%d(n uintptr,m %[3]s, off uint, v %[2]s) {", g.setBitsTypes[k], a[0], a[1])
		g.w("*(*%[1]s)(unsafe.Pointer(n)) = (*(*%[1]s)(unsafe.Pointer(n)))&^m|%[1]s(v)<<off&m }", a[1])
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
	for k := range g.xorTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc xor%d(n *%[2]s, m %[2]s) %[2]s { *n ^= m; return *n }", g.xorTypes[k], k)
	}
	a = a[:0]
	for k := range g.shrTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		b := strings.Split(k, "|")
		g.w(`

func shr%d(n %[2]s, m int) %[2]s {
	if m < 0 {
		m = -m
	}
	if m >= %[3]s {
		return n
	}

	return n >> uint(m)
}
`, g.shrTypes[k], b[1], b[0])
	}
	a = a[:0]
	for k := range g.shlTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		b := strings.Split(k, "|")
		g.w(`

func shl%d(n %[2]s, m int) %[2]s {
	if m < 0 {
		m = -m
	}
	if m >= %[3]s {
		return n
	}

	return n << uint(m)
}
`, g.shlTypes[k], b[1], b[0])
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
	for k := range g.mulTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc mul%d(n *%[2]s, m %[2]s) %[2]s { *n *= m; return *n }", g.mulTypes[k], k)
	}
	a = a[:0]
	for k := range g.modTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc mod%d(n *%[2]s, m %[2]s) %[2]s { *n %%= m; return *n }", g.modTypes[k], k)
	}
	a = a[:0]
	for k := range g.divTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc div%d(n *%[2]s, m %[2]s) %[2]s { *n /= m; return *n }", g.divTypes[k], k)
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
	for k := range g.addTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\n\nfunc add%d(n *%[2]s, m %[2]s) %[2]s { *n += m; return *n }", g.addTypes[k], k)
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
		for _, v := range g.ds {
			g.w("%d, ", v)
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
		return true
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
	switch x := c99.UnderlyingType(t).(type) {
	case *c99.StructType:
		for _, v := range x.Fields {
			if v.Bits != 0 {
				return true
			}
		}

		return false
	case *c99.UnionType:
		for _, v := range x.Fields {
			if v.Bits != 0 {
				return true
			}
		}

		return false
	default:
		todo("%T", x)
	}
	panic("unreachable")
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

func (g *gen) registerType(m map[string]int, t c99.Type) int {
	s := g.typ(t)
	if id := m[s]; id != 0 {
		return id
	}

	m[s] = len(m) + 1
	return len(m)
}

func (g *gen) registerBitType(m map[string]int, field, packed c99.Type) int {
	s := g.typ(field) + "|" + g.typ(packed)
	if id := m[s]; id != 0 {
		return id
	}

	m[s] = len(m) + 1
	return len(m)
}

func (g *gen) registerShiftType(m map[string]int, t c99.Type) int {
	b := 32
	if g.model.Sizeof(t) > 4 {
		b = 64
	}
	s := fmt.Sprintf("%d|%s", b, g.typ(t))
	if id := m[s]; id != 0 {
		return id
	}

	m[s] = len(m) + 1
	return len(m)
}
