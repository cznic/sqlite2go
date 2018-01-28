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

	"github.com/cznic/sqlite2go/internal/c99"
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

type gen struct {
	assignTypes         map[string]int
	bss                 int64
	ds                  []byte
	dsBits              []byte
	errs                scanner.ErrorList
	externs             map[int]*c99.Declarator
	fset                *token.FileSet
	in                  []*c99.TranslationUnit
	internalNames       map[int]struct{}          //TODO-?
	internals           []map[int]*c99.Declarator //TODO-?
	model               c99.Model
	needBool2int        int
	nextLabel           int
	num                 int
	nums                map[*c99.Declarator]int
	out                 io.Writer
	out0                bytes.Buffer
	postIncTypes        map[string]int
	preIncTypes         map[string]int
	producedDeclarators map[*c99.Declarator]struct{}
	producedEnumTags    map[int]struct{}
	producedNamedTypes  map[int]struct{}
	producedStructTags  map[int]struct{}
	queue               list.List
	strings             map[int]int64
	text                []int
	ts                  int64
	tsBits              []byte
	units               map[*c99.Declarator]int

	needNZ64    bool //TODO -> crt
	needNZ32    bool //TODO -> crt
	needPostInc bool
	needPreInc  bool
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		externs:             map[int]*c99.Declarator{},
		in:                  in,
		internalNames:       map[int]struct{}{},
		internals:           make([]map[int]*c99.Declarator, len(in)),
		nums:                map[*c99.Declarator]int{},
		assignTypes:         map[string]int{},
		out:                 out,
		postIncTypes:        map[string]int{},
		preIncTypes:         map[string]int{},
		producedDeclarators: map[*c99.Declarator]struct{}{},
		producedEnumTags:    map[int]struct{}{},
		producedNamedTypes:  map[int]struct{}{},
		producedStructTags:  map[int]struct{}{},
		strings:             map[int]int64{},
		units:               map[*c99.Declarator]int{},
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

	switch {
	case cmd:
		sym, ok := g.externs[idStart]
		if !ok {
			todo("")
			break
		}

		g.w("\nvar _ unsafe.Pointer\n")
		g.w("\nfunc main() { X_start(%sNewTLS(), 0, 0) } //TODO real args\n", crt)
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

	g.w("\n\nvar (\n")
	if g.bss != 0 {
		g.w("bss = %sBSS(&bssInit[0])\n", crt)
		g.w("bssInit [%d]byte\n", g.bss)
	}
	var tb []byte
	if len(g.ds) != 0 {
		g.w("ds = %sDS(dsInit)\n", crt)
		g.w("dsInit = []byte{")
		for _, v := range g.ds {
			g.w("%d, ", v)
		}
		g.w("}\n")
		b := g.tsBits
		n := len(b) - 1
		for n >= 0 && b[n] == 0 {
			n--
		}
		b = b[:n+1]
		tb = make([]byte, (len(b)+7)/8)
		for i, v := range b {
			if v != 0 {
				tb[i/8] |= 1 << uint(i&7)
			}
		}
		if len(tb) != 0 {
			g.w("tsBits = []byte{")
			for _, v := range tb {
				g.w("%d, ", v)
			}
			g.w("}\n")
		}
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
	if len(tb) != 0 {
		g.w(`
func init() {
	for i, v := range tsBits {
		for j := 0; v != 0 && j < 8; j++ {
			if v&1 != 0 {
				*(*uintptr)(unsafe.Pointer(&dsInit[8*i+j])) += ts
			}
			v >>= 1
		}
	}
}`)
	}
	if g.needNZ64 {
		g.w("\nfunc init() { nz64 = -nz64 }")
	}
	if g.needNZ32 {
		g.w("\nfunc init() { nz32 = -nz32 }")
	}
	if g.needPostInc {
		g.w("\nfunc postinc(p *uintptr, n uintptr) uintptr { r := *p; *p += n; return r }")
	}
	if g.needPreInc {
		g.w("\nfunc preinc(p *uintptr, n uintptr) uintptr { *p += n; return *p }")
	}
	var a []string
	for k := range g.assignTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\nfunc set%d(l *%[2]s, r %[2]s) %[2]s { *l = r; return r }", g.assignTypes[k], k)
	}
	a = a[:0]
	for k := range g.postIncTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\nfunc postinc%d(n *%[2]s) %[2]s { r := *n; *n++; return r }", g.postIncTypes[k], k)
	}
	a = a[:0]
	for k := range g.preIncTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\nfunc preinc%d(n *%[2]s) %[2]s { *n++; return *n }", g.preIncTypes[k], k)
	}
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
					g.units[x] = unit
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
					g.units[x] = unit
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

	switch underlyingType(n.Type).(type) {
	case *c99.ArrayType:
		return true
	case
		*c99.StructType,
		*c99.TaggedStructType,
		*c99.TaggedUnionType,
		*c99.UnionType:

		return n.IsTLD()
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

func (g *gen) registerType(m map[string]int, t c99.Type) int {
	s := g.typ(t)
	if id := m[s]; id != 0 {
		return id
	}

	m[s] = len(m) + 1
	return len(m)
}
