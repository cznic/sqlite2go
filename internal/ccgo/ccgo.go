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
	"math"
	"os"
	"runtime"
	"runtime/debug"
	"sort"
	"strconv"
	"strings"
	"unsafe"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
)

func todo(msg string, args ...interface{}) {
	_, f, l, _ := runtime.Caller(1)
	if msg == "" {
		msg = strings.Repeat("%v ", len(args))
	}
	panic(fmt.Errorf("\n\n%v:%d: TODO\n\n%s", f, l, fmt.Sprintf(msg, args...))) //TODOOK
}

const (
	ap         = "ap"
	crt        = "crt."
	vaListType = "pointer to struct{_ struct{}}" // *__builtin_va_list
)

var (
	dict        = xc.Dict
	testFn      string
	traceOpt    bool
	traceWrites bool

	idFuncName = dict.SID("__func__")
	idMain     = dict.SID("main")
	idStart    = dict.SID("_start")
	idVaList   = dict.SID("va_list")

	voidPtrType = &c99.PointerType{Item: c99.Void}
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
	errs                scanner.ErrorList
	escapes             map[*c99.Declarator]struct{}
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
	producedExterns     map[*c99.Declarator]struct{}
	producedNamedTypes  map[int]struct{}
	producedStructTags  map[int]struct{}
	queue               list.List
	strings             map[int]int64
	text                []int
	ts                  int64
	units               map[*c99.Declarator]int

	needNZ64    bool //TODO -> crt
	needPostInc bool
	needPreInc  bool
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		escapes:             map[*c99.Declarator]struct{}{},
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
		producedExterns:     map[*c99.Declarator]struct{}{},
		producedNamedTypes:  map[int]struct{}{},
		producedStructTags:  map[int]struct{}{},
		strings:             map[int]int64{},
		units:               map[*c99.Declarator]int{},
	}
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
		g.produceDeclarator(sym)
	default:
		var a []string
		for nm := range g.externs {
			a = append(a, string(dict.S(nm)))
		}
		sort.Strings(a)
		for _, nm := range a {
			g.produceDeclarator(g.externs[dict.SID(nm)])
		}
		todo("")
	}
	if err := g.errs.Err(); err != nil {
		return fmt.Errorf("%s", errString(err))
	}

	g.w("\nvar (\n")
	if g.bss != 0 {
		g.w("bss = %sBSS(&bssInit[0])\n", crt)
		g.w("bssInit [%d]byte\n", g.bss)
	}
	if len(g.ds) != 0 {
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
	g.w("ts = %sTS(\"", crt)
	for _, v := range g.text {
		s := fmt.Sprintf("%q", dict.S(v))
		g.w("%s\\x00", s[1:len(s)-1])
	}
	g.w("\")\n)\n")
	if g.needNZ64 {
		g.w("\nfunc init() { nz64 = -nz64 }")
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
		g.w("\nfunc preinc%d(n *%[2]s) %[2]s { *r += n; return *r }", g.preIncTypes[k], k)
	}
	return newOpt().do(g.out, &g.out0, testFn, g.needBool2int)
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

func (g *gen) produceDeclarator(n *c99.Declarator) {
outer:
	for {
		func() {
			if _, ok := g.producedDeclarators[n]; ok {
				return
			}

			g.producedDeclarators[n] = struct{}{}
			switch n.Type.Kind() {
			case c99.Function:
				g.functionDefinition(n)
			default:
				g.tld(n)
			}
		}()

		for {
			x := g.queue.Front()
			if x == nil {
				return
			}

			g.queue.Remove(x)
			switch y := x.Value.(type) {
			case *c99.Declarator:
				n = y
				continue outer
			case *c99.NamedType:
				g.produceNamedType(y)
			case *c99.TaggedEnumType:
				g.produceTaggedEnumType(y)
			case *c99.TaggedStructType:
				g.produceTaggedStructType(y)
			default:
				todo("%T", y)
			}
		}
	}
}

func (g *gen) produceNamedType(t *c99.NamedType) {
	if _, ok := g.producedNamedTypes[t.Name]; ok {
		return
	}

	g.producedNamedTypes[t.Name] = struct{}{}
	g.w("\ntype T%s = %s\n", dict.S(t.Name), g.typ(t.Type))
}

func (g *gen) produceTaggedEnumType(t *c99.TaggedEnumType) {
	if _, ok := g.producedEnumTags[t.Tag]; ok {
		return
	}

	g.producedEnumTags[t.Tag] = struct{}{}
	et := t.Type.(*c99.EnumType)
	tag := dict.S(t.Tag)
	g.w("\ntype E%s %s\n", tag, g.typ(et.Enums[0].Operand.Type))
	g.w("\nconst (")
	var iota int64
	for i, v := range et.Enums {
		val := v.Operand.Value.(*ir.Int64Value).Value
		if i == 0 {
			g.w("\nC%s E%s = iota", dict.S(v.Token.Val), tag)
			if val != 0 {
				g.w(" %+d", val)
			}
			iota = val + 1
			continue
		}

		g.w("\nC%s", dict.S(v.Token.Val))
		if val == iota {
			iota++
			continue
		}

		g.w(" = %d", val)
		iota = val + 1
	}
	g.w("\n)\n")

}

func (g *gen) produceTaggedStructType(t *c99.TaggedStructType) {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return
	}

	g.producedStructTags[t.Tag] = struct{}{}
	g.w("\ntype S%s %s\n", dict.S(t.Tag), g.typ(t.Type))
}

func (g *gen) tld(n *c99.Declarator) {
	if n.Linkage == c99.LinkageExternal {
		m := g.externs[n.Name()]
		if _, ok := g.producedExterns[m]; ok {
			return
		}

		g.producedExterns[m] = struct{}{}
	}
	if n.Initializer == nil || n.Initializer.Expr != nil && n.Initializer.Expr.Operand.IsZero() {
		switch x := n.Type.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil {
				todo("")
			}
			g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
		case
			*c99.PointerType,
			*c99.StructType:

			g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Int:

				if n.AddressTaken {
					g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
					break
				}

				g.w("\nvar %s %s\n", g.mangleDeclarator(n), g.typ(n.Type))
			default:
				todo("%v: %v", g.position(n), x)
			}
		default:
			todo("%v: %T", g.position(n), x)
		}
		return
	}

	switch n.Initializer.Case {
	case c99.InitializerExpr: // Expr
		switch x := n.Type.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil {
				todo("", g.position0(n))
			}
			g.w("\nvar %s = ds + %d\n", g.mangleDeclarator(n), g.allocDS(n.Type, n.Initializer))
		case *c99.PointerType:
			if n.AddressTaken {
				todo("")
			}

			a := n.Initializer.Expr.Operand.Address
			if a != nil {
				switch d := a.Declarator; x := a.Declarator.Type.(type) {
				case
					*c99.ArrayType,
					*c99.FunctionType:

					g.w("\nvar %s = %s + %d\n", g.mangleDeclarator(n), g.mangleDeclarator(d), a.Offset)
					g.produceDeclarator(d)
				default:
					todo("%v: %T", g.position(n), x)
				}
				break
			}

			todo("", g.position(n))
		case c99.TypeKind:
			switch x {
			case c99.Int:
				if n.AddressTaken {
					g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
					g.w("\n\nfunc init() { *(*%s)(unsafe.Pointer(%s)) = ", g.typ(n.Type), g.mangleDeclarator(n))
					g.rvalue(n.Initializer.Expr, true)
					g.w("}\n")
					break
				}

				g.w("\nvar %s = ", g.mangleDeclarator(n))
				g.rvalue(n.Initializer.Expr, true)
				g.w("\n")
			default:
				todo("%v: %v", g.position(n), x)
			}
		default:
			todo("%v: %T", g.position(n), x)
		}
	default:
		todo("", g.position0(n), n.Initializer.Case)
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

func (g *gen) allocBSS(t c99.Type) int64 {
	g.bss = roundup(g.bss, int64(g.model.Alignof(t)))
	r := g.bss
	g.bss += g.model.Sizeof(t)
	return r
}

func roundup(n, to int64) int64 {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

func (g *gen) allocDS(t c99.Type, n *c99.Initializer) int64 {
	up := roundup(int64(len(g.ds)), int64(g.model.Alignof(t)))
	if n := up - int64(len(g.ds)); n != 0 {
		g.ds = append(g.ds, make([]byte, n)...)
	}
	r := len(g.ds)
	g.ds = append(g.ds, g.renderInitializer(t, n)...)
	return int64(r)
}

func (g *gen) renderInitializer(t c99.Type, n *c99.Initializer) []byte {
	switch n.Case {
	case c99.InitializerExpr:
		return g.renderInitializerExpr(t, n.Expr)
	default:
		todo("", g.position0(n), n.Case)
	}
	panic("unreachable")
}

func (g *gen) renderInitializerExpr(t c99.Type, n *c99.Expr) []byte {
	switch x := t.(type) {
	case *c99.ArrayType:
		switch y := x.Item.(type) {
		case c99.TypeKind:
			switch y {
			case c99.Char:
				switch z := n.Operand.Value.(type) {
				case *ir.StringValue:
					b := append([]byte(nil), dict.S(int(z.StringID))...)
					return append(b, 0)
				default:
					todo("%v: %T", g.position0(n), z)
				}
			default:
				todo("", g.position0(n), y)
			}
		default:
			todo("%v: %T", g.position0(n), y)
		}
	default:
		todo("%v: %T", g.position0(n), x)
	}
	panic("unreachable")
}

func (g *gen) functionDefinition(n *c99.Declarator) {
	g.nextLabel = 1
	g.w("\n\nfunc %s(tls *%sTLS", g.mangleDeclarator(n), crt)
	names := n.ParameterNames()
	t := n.Type.(*c99.FunctionType)
	if len(names) != len(t.Params) {
		if len(names) != 0 {
			if !(len(names) == 1 && names[0] == 0) {
				todo("", g.position(n), names, t.Params)
			}
		}

		names = make([]int, len(t.Params))
	}
	params := n.Parameters
	var escParams []*c99.Declarator
	switch {
	case len(t.Params) == 1 && t.Params[0].Kind() == c99.Void:
		// nop
	default:
		for i, v := range t.Params {
			var param *c99.Declarator
			if i < len(params) {
				param = params[i]
			}
			nm := names[i]
			g.w(", ")
			switch {
			case param != nil && param.AddressTaken:
				g.w("a%s %s", dict.S(nm), g.typ(v))
				escParams = append(escParams, param)
			default:
				t := v
				for done := false; !done; {
					switch x := t.(type) {
					case *c99.NamedType:
						t = x.Type
					case *c99.PointerType:
						if x.Item.Kind() == c99.Function {
							v = x.Item
						}
						done = true
					case *c99.TaggedStructType:
						done = true
					case c99.TypeKind:
						switch x {
						case
							c99.Char,
							c99.Double,
							c99.Float,
							c99.Int,
							c99.Long,
							c99.LongLong,
							c99.SChar,
							c99.UChar,
							c99.UInt,
							c99.ULong,
							c99.UShort:

							done = true
						default:
							todo("%v: %v", g.position(n), x)
						}
					default:
						todo("%v: %T", g.position(n), x)
					}
				}
				g.w("%s %s", mangleIdent(nm, false), g.typ(v))
				if isVaList(v) {
					continue
				}

				if v.Kind() == c99.Ptr {
					g.w(" /* %s */", g.ptyp(v, false))
				}
			}
		}
		if t.Variadic {
			g.w(", %s...interface{}", ap)
		}
	}
	g.w(")")
	void := t.Result.Kind() == c99.Void
	if !void {
		g.w("(r %s)", g.typ(t.Result))
	}
	g.functionBody(n.FunctionDefinition.FunctionBody, n.FunctionDefinition.LocalVariables(), void, escParams, t.Result)
}

func isVaList(t c99.Type) bool {
	x, ok := t.(*c99.NamedType)
	return ok && x.Name == idVaList
}

func mangleIdent(nm int, exported bool) string {
	switch {
	case exported:
		return fmt.Sprintf("X%s", dict.S(nm))
	default:
		return fmt.Sprintf("_%s", dict.S(nm))
	}
}

func (g *gen) mangleDeclarator(n *c99.Declarator) string {
	nm := n.Name()
	if num, ok := g.nums[n]; ok {
		return fmt.Sprintf("_%d%s", num, dict.S(nm))
	}

	switch n.Linkage {
	case c99.LinkageInternal, c99.LinkageNone:
		return mangleIdent(nm, false)
	case c99.LinkageExternal:
		switch {
		case g.externs[n.Name()] == nil:
			return crt + mangleIdent(nm, true)
		default:
			return mangleIdent(nm, true)
		}
	default:
		todo("%s %v", dict.S(nm), n.Linkage)
	}
	panic("unreachable")
}

func (g *gen) functionBody(n *c99.FunctionBody, vars []*c99.Declarator, void bool, escParams []*c99.Declarator, rt c99.Type) {
	if vars == nil {
		vars = []*c99.Declarator{}
	}
	g.compoundStmt(n.CompoundStmt, vars, nil, !void, nil, nil, escParams, rt)
}

func (g *gen) compoundStmt(n *c99.CompoundStmt, vars []*c99.Declarator, cases map[*c99.LabeledStmt]int, sentinel bool, brk, cont *int, escParams []*c99.Declarator, rt c99.Type) {
	if vars != nil {
		g.w(" {")
	}
	w := 0
	for _, v := range vars {
		if v.Referenced == 0 && v.Initializer != nil && v.Linkage == c99.LinkageNone && v.DeclarationSpecifier.IsStatic() && v.Name() == idFuncName {
			continue
		}

		if v.Referenced == 0 && v.Initializer == nil && !v.AddressTaken {
			continue
		}

		if v.DeclarationSpecifier.IsStatic() {
			g.enqueueNumbered(v)
			continue
		}

		vars[w] = v
		w++
	}
	vars = vars[:w]
	var free []*c99.Declarator
	if len(vars)+len(escParams) != 0 {
		localNames := map[int]struct{}{}
		num := 0
		for _, v := range vars {
			nm := v.Name()
			if _, ok := localNames[nm]; ok {
				num++
				g.nums[v] = num
			}
			localNames[nm] = struct{}{}
		}
		switch {
		case len(vars)+len(escParams) == 1:
			g.w("\nvar ")
		default:
			g.w("\nvar (\n")
		}
		for _, v := range escParams {
			free = append(free, v)
			g.w("\n\t%s = %sMustMalloc(%d) // *%s", g.mangleDeclarator(v), crt, g.model.Sizeof(v.Type), g.ptyp(v.Type, false))
		}
		for _, v := range vars {
			initializer := v.Initializer
			malloc := "MustMalloc"
			if initializer != nil && initializer.Case == c99.InitializerExpr {
				o := initializer.Expr.Operand
				if o.Type != nil && (o.IsZero() || o.Address == c99.Null) {
					initializer = nil
					malloc = "MustCalloc"
				}
			}
			switch {
			case g.escaped(v):
				free = append(free, v)
				g.w("\n\t%s = %s%s(%d) // *%s", g.mangleDeclarator(v), crt, malloc, g.model.Sizeof(v.Type), g.ptyp(v.Type, false))
			default:
				switch {
				case v.Type.Kind() == c99.Ptr:
					g.w("\n\t%s %s\t// %s", g.mangleDeclarator(v), g.ptyp(v.Type, true), g.ptyp(v.Type, false))
				default:
					g.w("\n\t%s %s", g.mangleDeclarator(v), g.typ(v.Type))
				}
				if v.Referenced == 0 || v.Referenced == 1 && v.AssignedTo == 1 {
					g.w("\n_ = %s", g.mangleDeclarator(v))
				}
			}
		}
		if len(vars)+len(escParams) != 1 {
			g.w("\n)")
		}
		for _, v := range escParams {
			g.w("\n*(*%s)(unsafe.Pointer(%s)) = a%s", g.ptyp(v.Type, false), g.mangleDeclarator(v), dict.S(v.Name()))
		}
	}
	if len(free) != 0 {
		g.w("\ndefer func() {")
		for _, v := range free {
			g.w("\n%sFree(%s)", crt, g.mangleDeclarator(v))
		}
		g.w("\n}()")
	}
	g.blockItemListOpt(n.BlockItemListOpt, cases, brk, cont, rt)
	if vars != nil {
		if sentinel {
			g.w(";return r")
		}
		g.w("\n}")
	}
}

func (g gen) escaped(n *c99.Declarator) bool {
	if isVaList(n.Type) {
		return false
	}

	if n.AddressTaken {
		g.escapes[n] = struct{}{}
		return true
	}

	t := n.Type
	for {
		switch x := t.(type) {
		case *c99.ArrayType:
			g.escapes[n] = struct{}{}
			return true
		case *c99.NamedType:
			t = x.Type
		case
			*c99.FunctionType,
			*c99.PointerType,
			*c99.TaggedEnumType:

			return false
		case *c99.TaggedStructType:
			if n.IsTLD() {
				g.escapes[n] = struct{}{}
				return true
			}

			return false
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Double,
				c99.Float,
				c99.Int,
				c99.LongLong,
				c99.UInt,
				c99.ULong:

				return false
			default:
				todo("%v: %v", g.position(n), x)
			}
		default:
			todo("%v: %T", g.position(n), x)
		}
	}
}

func (g *gen) blockItemListOpt(n *c99.BlockItemListOpt, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	if n == nil {
		return
	}

	g.blockItemList(n.BlockItemList, cases, brk, cont, rt)
}

func (g *gen) blockItemList(n *c99.BlockItemList, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	for ; n != nil; n = n.BlockItemList {
		g.blockItem(n.BlockItem, cases, brk, cont, rt)
	}
}

func (g *gen) blockItem(n *c99.BlockItem, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	switch n.Case {
	case c99.BlockItemDecl: // Declaration
		g.declaration(n.Declaration)
	case c99.BlockItemStmt: // Stmt
		g.stmt(n.Stmt, cases, brk, cont, rt)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) declaration(n *c99.Declaration) {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	g.initDeclaratorListOpt(n.InitDeclaratorListOpt)
}

func (g *gen) initDeclaratorListOpt(n *c99.InitDeclaratorListOpt) {
	if n == nil {
		return
	}

	g.initDeclaratorList(n.InitDeclaratorList)
}

func (g *gen) initDeclaratorList(n *c99.InitDeclaratorList) {
	for ; n != nil; n = n.InitDeclaratorList {
		g.initDeclarator(n.InitDeclarator)
	}
}

func (g *gen) initDeclarator(n *c99.InitDeclarator) {
	d := n.Declarator
	if d.DeclarationSpecifier.IsStatic() {
		return
	}

	if d.Referenced == 0 && d.Initializer == nil {
		return
	}

	switch n.Case {
	case c99.InitDeclaratorBase: // Declarator
		// nop
	case c99.InitDeclaratorInit: // Declarator '=' Initializer
		g.initializer(d)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) initializer(d *c99.Declarator) {
	n := d.Initializer
	t := d.Type
	switch n.Case {
	case c99.InitializerExpr: // Expr
		switch {
		case d.AddressTaken:
			switch a := n.Expr.Operand.Address; {
			case a != nil && g.escaped(a.Declarator):
				g.w("\n*(*uintptr)(unsafe.Pointer(%s)) = ", g.mangleDeclarator(d))
			default:
				g.w("\n*(*%s)(unsafe.Pointer(%s)) = ", g.ptyp(d.Type, false), g.mangleDeclarator(d))
			}
		default:
			g.w("\n%s = ", g.mangleDeclarator(d))
		}
		g.rvalue2(n.Expr, t)
	case c99.InitializerCompLit: // '{' InitializerList CommaOpt '}'
		op := n.InitializerList.Operand
		if !op.Type.Equal(t) {
			todo("", g.position0(n))
		}
		switch v := op.Value.(type) {
		case *ir.CompositeValue:
			switch x := t.(type) {
			case *c99.ArrayType:
				b := make([]byte, g.model.Sizeof(x))
				g.arrayCompositeValue(b, v, x.Item)
				g.w("\n%sCopy(%s, ts+%d, %d)", crt, g.mangleDeclarator(d), g.allocString(dict.ID(b)), len(b))
			default:
				todo("%v: %T", g.position0(n), x)
			}
		default:
			todo("%v: %T", g.position0(n), v)
		}
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) arrayCompositeValue(b []byte, v *ir.CompositeValue, item c99.Type) {
	itemSz := g.model.Sizeof(item)
	var index int64
	for _, v := range v.Values {
		switch x := v.(type) {
		case c99.Operand:
			off := index * itemSz
			g.arrayCompositeValueItem(b[off:off+itemSz], x, itemSz)
			index++
		default:
			todo("%T", x)
		}
	}
}

func (g *gen) arrayCompositeValueItem(n []byte, op c99.Operand, itemSz int64) {
	switch x := op.Type.(type) {
	case c99.TypeKind:
		switch x {
		case c99.Int:
			switch itemSz {
			case 4:
				*(*int32)(unsafe.Pointer(&n[0])) = int32(op.Value.(*ir.Int64Value).Value)
			default:
				todo("", itemSz)
			}
		default:
			todo("", x)
		}
	default:
		todo("%T", x)
	}
}

func (g *gen) stmt(n *c99.Stmt, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	switch n.Case {
	case c99.StmtBlock: // CompoundStmt
		g.compoundStmt(n.CompoundStmt, nil, cases, false, brk, cont, nil, rt)
	case c99.StmtExpr: // ExprStmt
		g.exprStmt(n.ExprStmt)
	case c99.StmtIter: // IterationStmt
		g.iterationStmt(n.IterationStmt, cases, brk, cont, rt)
	case c99.StmtJump: // JumpStmt
		g.jumpStmt(n.JumpStmt, brk, cont, rt)
	case c99.StmtLabeled: // LabeledStmt
		g.labeledStmt(n.LabeledStmt, cases, brk, cont, rt)
	case c99.StmtSelect: // SelectionStmt
		g.selectionStmt(n.SelectionStmt, cases, brk, cont, rt)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) labeledStmt(n *c99.LabeledStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	switch n.Case {
	case
		c99.LabeledStmtSwitchCase, // "case" ConstExpr ':' Stmt
		c99.LabeledStmtDefault:    // "default" ':' Stmt

		l, ok := cases[n]
		if !ok {
			todo("", g.position0(n))
		}
		g.w("\n_%d:", l)
		g.stmt(n.Stmt, cases, brk, cont, rt)
	case c99.LabeledStmtLabel: // IDENTIFIER ':' Stmt
		g.w("\n%s:\n", mangleIdent(n.Token.Val, false))
		g.stmt(n.Stmt, cases, brk, cont, rt)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) jumpStmt(n *c99.JumpStmt, brk, cont *int, rt c99.Type) {
	switch n.Case {
	case c99.JumpStmtBreak: // "break" ';'
		if *brk < 0 {
			*brk = -*brk // Signal used.
		}
		g.w("\ngoto _%d\n", *brk)
	case c99.JumpStmtReturn: // "return" ExprListOpt ';'
		g.w("\nreturn ")
		if o := n.ExprListOpt; o != nil {
			g.exprList2(o.ExprList, rt)
		}
		g.w("\n")
	case c99.JumpStmtGoto: // "goto" IDENTIFIER ';'
		g.w("\ngoto %s\n", mangleIdent(n.Token2.Val, false))
	case c99.JumpStmtContinue: // "continue" ';'
		if *cont < 0 {
			*cont = -*cont // Signal used.
		}
		g.w("\ngoto _%d\n", *cont)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) iterationStmt(n *c99.IterationStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	switch n.Case {
	case c99.IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
		if op := n.ExprList.Operand; op.Value != nil {
			switch {
			case op.IsZero():
				// stmt
				// goto A
				// A:
				a := -g.label()
				g.stmt(n.Stmt, cases, &a, cont, rt)
				if a > 0 {
					g.w("\ngoto _%d\n\n_%d:", a, a)
				}
				return
			case op.IsNonzero():
				todo("", g.position0(n))
			default:
				todo("", g.position0(n))
			}
		}
		// A:
		// stmt
		// C:
		// if exprList != 0 { goto A }
		// goto B
		// B:
		a := g.label()
		c := -g.label()
		b := -g.label()
		g.w("\n_%d:", a)
		g.stmt(n.Stmt, cases, &b, &c, rt)
		if c > 0 {
			g.w("\n_%d:", c)
		}
		g.w("\nif ")
		g.exprList(n.ExprList, false)
		g.w(" != 0 { goto _%d }\n", a)
		if b > 0 {
			g.w("\ngoto _%d\n\n_%d:", b, b)
		}
	case c99.IterationStmtFor: // "for" '(' ExprListOpt ';' ExprListOpt ';' ExprListOpt ')' Stmt
		if n.ExprListOpt2 != nil && n.ExprListOpt2.ExprList.Operand.Value != nil {
			todo("", g.position0(n))
		}

		// ExprListOpt
		// A:
		// if ExprListOpt2 == 0 { goto B }
		// Stmt
		// ExprListOpt3
		// goto A
		// B:
		g.w("\n")
		g.exprListOpt(n.ExprListOpt, true)
		a := g.label()
		b := -g.label()
		g.w("\n_%d:", a)
		if n.ExprListOpt2 != nil {
			g.w("if ")
			g.exprListOpt(n.ExprListOpt2, false)
			b = -b
			g.w(" == 0 { goto _%d }\n", b)
		}
		g.stmt(n.Stmt, cases, &b, &a, rt)
		if n.ExprListOpt3 != nil {
			g.w("\n")
		}
		g.exprListOpt(n.ExprListOpt3, true)
		g.w("\ngoto _%d\n", a)
		if b > 0 {
			g.w("\n_%d:", b)
		}
	case c99.IterationStmtWhile: // "while" '(' ExprList ')' Stmt
		if n.ExprList.Operand.Value != nil {
			todo("", g.position0(n))
		}
		// A:
		// if exprList == 0 { goto B }
		// stmt
		// goto A
		// B:
		a := g.label()
		b := g.label()
		g.w("\n_%d:\nif ", a)
		g.exprList(n.ExprList, false)
		g.w(" == 0 { goto _%d }\n", b)
		g.stmt(n.Stmt, cases, &b, &a, rt)
		g.w("\ngoto _%d\n\n_%d:", a, b)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) label() int {
	r := g.nextLabel
	g.nextLabel++
	return r
}

func (g *gen) selectionStmt(n *c99.SelectionStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, rt c99.Type) {
	switch n.Case {
	case c99.SelectionStmtIf: // "if" '(' ExprList ')' Stmt
		if n.ExprList.Operand.Value != nil {
			todo("")
		}
		// if exprList == 0 { goto A }
		// stmt
		// A:
		a := g.label()
		g.w("\nif ")
		g.exprList(n.ExprList, false)
		g.w(" == 0 { goto _%d }\n", a)
		g.stmt(n.Stmt, cases, brk, cont, rt)
		g.w("\n_%d:", a)
	case c99.SelectionStmtIfElse: // "if" '(' ExprList ')' Stmt "else" Stmt
		if n.ExprList.Operand.Value != nil {
			todo("")
		}
		// if exprList == 0 { goto A }
		// stmt
		// goto B
		// A:
		// stmt2
		// B:
		a := g.label()
		b := g.label()
		g.w("\nif ")
		g.exprList(n.ExprList, false)
		g.w(" == 0 { goto _%d }\n", a)
		g.stmt(n.Stmt, cases, brk, cont, rt)
		g.w("\ngoto _%d\n", b)
		g.w("\n_%d:", a)
		g.stmt(n.Stmt2, cases, brk, cont, rt)
		g.w("\n_%d:", b)
	case c99.SelectionStmtSwitch: // "switch" '(' ExprList ')' Stmt
		if n.ExprList.Operand.Value != nil {
			todo("")
		}
		g.w("\nswitch ")
		switch el := n.ExprList; {
		case isSingleExpression(el):
			g.rvalue2(n.ExprList.Expr, n.SwitchOp.Type)
		default:
			todo("", g.position0(n))
		}
		g.w("{")
		after := -g.label()
		cases := map[*c99.LabeledStmt]int{}
		var deflt *c99.LabeledStmt
		for _, v := range n.Cases {
			l := g.label()
			cases[v] = l
			switch ce := v.ConstExpr; {
			case ce != nil:
				g.w("\ncase ")
				g.rvalue(ce.Expr, true)
				g.w(": goto _%d", l)
			default:
				deflt = v
				g.w("\ndefault: goto _%d\n", l)
			}
		}
		g.w("}")
		if deflt == nil {
			after = -after
			g.w("\ngoto _%d\n", after)
		}
		g.stmt(n.Stmt, cases, &after, cont, rt)
		if after > 0 {
			g.w("\n_%d:", after)
		}
	default:
		todo("", g.position0(n), n.Case)
	}
}

func isSingleExpression(n *c99.ExprList) bool { return n.ExprList == nil }

func (g *gen) exprStmt(n *c99.ExprStmt) {
	if n.ExprListOpt != nil {
		g.w("\n")
		g.exprListOpt(n.ExprListOpt, true)
	}
}

func (g *gen) exprListOpt(n *c99.ExprListOpt, void bool) {
	if n == nil {
		return
	}

	g.exprList(n.ExprList, void)
}

func (g *gen) exprList(n *c99.ExprList, void bool) {
	switch {
	case void:
		for ; n != nil; n = n.ExprList {
			g.void(n.Expr)
		}
	default:
		if isSingleExpression(n) {
			g.rvalue(n.Expr, true)
			break
		}

		todo("", g.position0(n))
	}
}

func (g *gen) void(n *c99.Expr) {
	if g.voidCanIgnore(n) {
		return
	}

	switch n.Case {
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.rvalue(n, false)
	case c99.ExprAssign: // Expr '=' Expr
		l := n.Expr.Operand
		la := l.Address
		r := n.Expr2.Operand
		ra := r.Address
		if la != nil && la.Offset == 0 && la.Declarator.Type.Kind() == c99.Ptr && !g.escaped(la.Declarator) {
			switch n.Expr2.Case {
			case c99.ExprPSelect: // Expr "->" IDENTIFIER
				g.w("%s = ", g.mangleDeclarator(la.Declarator))
				g.w("*(*uintptr)(unsafe.Pointer(")
				g.uintptr(n.Expr2.Expr, false)
				g.w("))")
				return
			}
		}

		switch n.Expr.Case {
		case c99.ExprDeref: // '*' Expr
			if n.Expr.Expr.Operand.Type.(*c99.PointerType).Item.Kind() == c99.Ptr &&
				r.Type.Kind() == c99.Ptr && ra != nil && !g.escaped(ra.Declarator) {

				g.w("*(*uintptr)(unsafe.Pointer(")
				g.uintptr(n.Expr.Expr, false)
				g.w("))")
				g.w(" = ")
				g.rvalue(n.Expr2, false)
				return
			}
		case c99.ExprPSelect: // Expr "->" IDENTIFIER
			if l.Type.Kind() == c99.Ptr && r.Type.Kind() == c99.Ptr &&
				(ra != nil && g.escaped(ra.Declarator) || n.Expr2.Case == c99.ExprString || n.Expr2.Case == c99.ExprLString) {

				g.w("*(*uintptr)(unsafe.Pointer(")
				g.uintptr(n.Expr.Expr, false)
				g.w("))")
				g.w(" = ")
				g.rvalue(n.Expr2, false)
				return
			}
		case c99.ExprSelect: // Expr '.' IDENTIFIER
			if l.Type.Kind() == c99.Ptr && la != nil && g.escaped(la.Declarator) {
				g.w("*(*uintptr)(unsafe.Pointer(%s+%d))", g.mangleDeclarator(la.Declarator), la.Offset)
				g.w(" = ")
				g.rvalue(n.Expr2, false)
				return
			}
		}

		g.w("*")
		g.lvalue(n.Expr)
		g.w(" = ")
		if isVaList(l.Type) && n.Expr2.Case == c99.ExprInt {
			g.w("nil")
			return
		}

		g.rvalue2(n.Expr2, n.Expr.Operand.Type)
	case c99.ExprPostInc: // Expr "++"
		switch x := n.Operand.Type.(type) {
		case *c99.PointerType:
			switch sz := g.model.Sizeof(n.Expr.Operand.Type.(*c99.PointerType).Item); {
			case sz == 1:
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")++")
			default:
				todo("", g.position0(n))
			}
		case c99.TypeKind:
			switch x {
			case c99.Int:
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")++")
			default:
				todo("%v: %v", g.position0(n), x)
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPostDec: // Expr "--"
		switch x := n.Operand.Type.(type) {
		case c99.TypeKind:
			switch x {
			case c99.Int:
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")--")
			default:
				todo("%v: %v", g.position0(n), x)
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprAddAssign: // Expr "+=" Expr
		switch {
		case n.Expr.Operand.Type.Kind() == c99.Ptr:
			g.w(" *(")
			g.lvalue(n.Expr)
			g.w(") += %d*uintptr(", g.model.Sizeof(n.Expr.Operand.Type.(*c99.PointerType).Item))
			g.rvalue(n.Expr2, false)
			g.w(")")
		default:
			g.voidArithmeticAsop(n)
		}
	case c99.ExprSubAssign: // Expr "-=" Expr
		switch {
		case n.Expr.Operand.Type.Kind() == c99.Ptr:
			todo("", g.position0(n))
			// g.w(" *(")
			// g.lvalue(n.Expr)
			// g.w(") -= %d*uintptr(", g.model.Sizeof(n.Expr.Operand.Type.(*c99.PointerType).Item))
			// g.rvalue(n.Expr2, false)
			// g.w(")")
		default:
			g.voidArithmeticAsop(n)
		}
	case
		c99.ExprAndAssign, // Expr "&=" Expr
		c99.ExprDivAssign, // Expr "/=" Expr
		c99.ExprMulAssign, // Expr "*=" Expr
		c99.ExprOrAssign:  // Expr "|=" Expr

		g.voidArithmeticAsop(n)
	case c99.ExprPExprList: // '(' ExprList ')'
		for l := n.ExprList; l != nil; l = l.ExprList {
			g.void(l.Expr)
		}
	case c99.ExprCast: // '(' TypeName ')' Expr
		g.void(n.Expr)
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		switch {
		case n.Expr.Operand.IsZero():
			todo("", g.position0(n))
		case n.Expr.Operand.IsNonzero():
			todo("", g.position0(n))
		default:
			ok := true
			for l := n.ExprList; l != nil; l = l.ExprList {
				if !g.voidCanIgnore(l.Expr) {
					ok = false
					break
				}
			}
			switch {
			case ok:
				if g.voidCanIgnore(n.Expr2) {
					return
				}

				// if expr == 0 {
				//	expr2
				// }
				g.w("if ")
				g.rvalue(n.Expr, false)
				g.w(" == 0 {")
				g.void(n.Expr2)
				g.w("}")
			case g.voidCanIgnore(n.Expr2):
				todo("", g.position0(n))
			default:
				todo("", g.position0(n))
			}
		}
	default:
		todo("", g.position0(n), n.Case, n.Operand)
	}
}

func (g *gen) voidCanIgnore(n *c99.Expr) bool {
	switch n.Case {
	case
		c99.ExprIdent, // IDENTIFIER
		c99.ExprInt:   // INTCONST

		return true
	case c99.ExprPExprList: // '(' ExprList ')'
		if isSingleExpression(n.ExprList) {
			return g.voidCanIgnore(n.ExprList.Expr)
		}

		for l := n.ExprList; l != nil; l = l.ExprList {
			if !g.voidCanIgnore(l.Expr) {
				return false
			}
		}

		return true
	case
		c99.ExprAddAssign, // Expr "+=" Expr
		c99.ExprAndAssign, // Expr "&=" Expr
		c99.ExprAssign,    // Expr '=' Expr
		c99.ExprCall,      // Expr '(' ArgumentExprListOpt ')'
		c99.ExprDeref,     // '*' Expr
		c99.ExprDivAssign, // Expr "/=" Expr
		c99.ExprMulAssign, // Expr "*=" Expr
		c99.ExprOrAssign,  // Expr "|=" Expr
		c99.ExprPostDec,   // Expr "--"
		c99.ExprPostInc,   // Expr "++"
		c99.ExprPreDec,    // "--" Expr
		c99.ExprPreInc,    // "++" Expr
		c99.ExprSubAssign, // Expr "-=" Expr
		c99.ExprXorAssign: // Expr "^=" Expr

		return false
	case c99.ExprCast: // '(' TypeName ')' Expr
		return g.voidCanIgnore(n.Expr)
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		switch {
		case n.Expr.Operand.IsNonzero():
			if isSingleExpression(n.ExprList) {
				return g.voidCanIgnore(n.ExprList.Expr)
			}

			for l := n.ExprList; l != nil; l = l.ExprList {
				if !g.voidCanIgnore(l.Expr) {
					return false
				}
			}

			return true
		case n.Expr.Operand.IsZero():
			return g.voidCanIgnore(n.Expr2)
		}
		return false
	case
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprLe:  // Expr "<=" Expr

		return g.voidCanIgnore(n.Expr) && g.voidCanIgnore(n.Expr2)
	case
		c99.ExprLAnd, // Expr "&&" Expr
		c99.ExprLOr:  // Expr "||" Expr

		return n.Operand.Value != nil
	default:
		todo("", g.position0(n), n.Case, n.Operand)
	}
	panic("unreachable")
}

func (g *gen) voidArithmeticAsop(n *c99.Expr) { //TODO may evaluate lhs twice, fix and add test
	op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
	g.w(" *(")
	g.lvalue(n.Expr)
	g.w(") = %s(", g.typ(n.Expr.Operand.Type))
	g.rvalue2(n.Expr, op.Type)
	switch n.Token.Rune {
	case c99.ANDASSIGN:
		g.w(" & ")
	case c99.ADDASSIGN:
		g.w(" + ")
	case c99.SUBASSIGN:
		g.w(" - ")
	case c99.MULASSIGN:
		g.w(" * ")
	case c99.DIVASSIGN:
		g.w(" / ")
	case c99.ORASSIGN:
		g.w(" | ")
	default:
		todo("", g.position0(n), c99.TokSrc(n.Token))
	}
	g.rvalue2(n.Expr2, op.Type)
	g.w(")")
}

func (g *gen) normalizeAddress(a *c99.Address) (_ *c99.Address, undefined bool) {
	if a == nil || a.Declarator == nil || a.Declarator.Linkage != c99.LinkageExternal {
		return a, false
	}

	d := g.externs[a.Declarator.Name()]
	if d == nil {
		return a, true
	}

	a.Declarator = d
	return a, false
}

func (g *gen) lvalue(n *c99.Expr) {
	if n.Operand.Value != nil {
		todo("", g.position0(n), n.Case, n.Operand)
	}

	g.w("&")
	g.rvalue(n, false)
}

func (g *gen) rvalue(n *c99.Expr, typedIntLiterals bool) {
	if n.Operand.Value != nil {
		switch n.Case {
		case c99.ExprPExprList: // '(' ExprList ')'
			if isSingleExpression(n.ExprList) {
				g.rvalue2(n.ExprList.Expr, n.Operand.Type)
				return
			}
		default:
			g.constant(n, typedIntLiterals)
			return
		}
	}

	g.w("(")

	defer g.w(")")
	switch n.Case {
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.rvalue(n.Expr, false)
		g.w("(tls")
		if o := n.ArgumentExprListOpt; o != nil {
			i := 0
			for l := o.ArgumentExprList; l != nil; l = l.ArgumentExprList {
				g.w(", ")
				switch {
				case n.CallArgs[i].Type.Kind() == c99.Ptr:
					g.uintptr(l.Expr, false)
				default:
					g.rvalue2(l.Expr, n.CallArgs[i].Type)
				}
				i++
			}
		}
		g.w(")")
	case c99.ExprIdent: // IDENTIFIER
		a, undef := g.normalizeAddress(n.Operand.Address)
		d := a.Declarator
		if !undef {
			g.enqueue(d)
		}
		switch {
		case g.escaped(d):
			g.w(" *(*%s)(unsafe.Pointer(%s))", g.ptyp(d.Type, d.Type.Kind() == c99.Ptr), g.mangleDeclarator(d))
		default:
			g.w(" %s", g.mangleDeclarator(d))
		}
	case c99.ExprPExprList: // '(' ExprList ')'
		if isSingleExpression(n.ExprList) {
			g.rvalue(n.ExprList.Expr, typedIntLiterals)
			return
		}

		todo("", g.position0(n))
	case c99.ExprCast: // '(' TypeName ')' Expr
		if n.Operand.Address == c99.Null {
			g.w(" 0")
			return
		}

		if isVaList(n.Expr.Operand.Type) {
			t := n.TypeName.Type
			for {
				switch x := t.(type) {
				case c99.TypeKind:
					switch x {
					case c99.Int:
						g.w(" %sVA%s(&", crt, g.typ(t))
						g.rvalue(n.Expr, false)
						g.w(")")
						return
					default:
						//dbg("%v: %v", g.position0(n), x) //TODO-
						todo("%v: %v", g.position0(n), x)
					}
				default:
					//dbg("%v: %T", g.position0(n), x) //TODO-
					todo("%v: %T", g.position0(n), x)
				}
			}
		}

		g.rvalue2(n.Expr, n.TypeName.Type)
	case c99.ExprUnaryMinus: // '-' Expr
		g.w(" -")
		g.rvalue2(n.Expr, n.Operand.Type)
	case
		c99.ExprIndex,   // Expr '[' ExprList ']'
		c99.ExprPSelect, // Expr "->" IDENTIFIER
		c99.ExprSelect:  // Expr '.' IDENTIFIER

		g.w("*(*%s)(unsafe.Pointer(", g.ptyp(n.Operand.Type, false))
		g.uintptr(n, true)
		g.w("))")
	case c99.ExprAddrof: // '&' Expr
		a := n.Operand.Address
		g.w("(%s+%d)", g.mangleDeclarator(a.Declarator), a.Offset)
		g.enqueue(a.Declarator)
	case c99.ExprDeref: // '*' Expr
		t := n.Expr.Operand.Type
		for done := false; !done; {
			switch x := t.(type) {
			case *c99.PointerType:
				if x.Item.Kind() == c99.Function {
					g.rvalue(n.Expr, false)
					return
				}

				done = true
			default:
				todo("%v: %T", g.position0(n), x)
			}
		}
		n0 := n
		i := 1
		for n.Expr.Case == c99.ExprDeref {
			i++
			n = n.Expr
		}
		if a := n.Expr.Operand.Address; a != nil && g.escaped(a.Declarator) {
			i++
			g.w(" %[1]s(%[1]s%s)(unsafe.Pointer(%s+%d))", strings.Repeat("*", i), g.ptyp(n0.Operand.Type, false), g.mangleDeclarator(a.Declarator), a.Offset)
			return
		}

		g.w(" %[1]s(%[1]s%s)(unsafe.Pointer(", strings.Repeat("*", i), g.ptyp(n0.Operand.Type, false))
		g.rvalue(n.Expr, false)
		g.w("))")
	case c99.ExprAssign: // Expr '=' Expr
		g.assignmentValue(n)
	case c99.ExprLOr: // Expr "||" Expr
		g.needBool2int++
		g.w(" bool2int((")
		g.rvalue(n.Expr, false)
		g.w(" != 0) || (")
		g.rvalue(n.Expr2, false)
		g.w(" != 0))")
	case c99.ExprLAnd: // Expr "&&" Expr
		g.needBool2int++
		g.w(" bool2int((")
		g.rvalue(n.Expr, false)
		g.w(" != 0) && (")
		g.rvalue(n.Expr2, false)
		g.w(" != 0))")
	case c99.ExprPostInc: // Expr "++"
		switch x := n.Operand.Type.(type) {
		case *c99.PointerType:
			g.needPostInc = true
			g.w(" postinc(")
			g.uintptr(n.Expr, true)
			g.w(", %d)", g.model.Sizeof(x.Item))
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Int:

				g.w(" postinc%d(", g.registerType(g.postIncTypes, x))
				g.lvalue(n.Expr)
				g.w(")")
			default:
				todo("%v: %v", g.position0(n), x)
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPreInc: // "++" Expr
		switch x := n.Operand.Type.(type) {
		case c99.TypeKind:
			switch x {
			case c99.Int:
				g.w(" preinc%d(", g.registerType(g.preIncTypes, x))
				g.lvalue(n.Expr)
				g.w(")")
			default:
				todo("%v: %v", g.position0(n), x)
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		t := n.Operand.Type
		t0 := t
		switch x := t.(type) {
		case *c99.PointerType:
			if x.Item.Kind() == c99.Function {
				t = x.Item
			}
		}
		g.w(" func() %s { if ", g.typ(t))
		g.rvalue(n.Expr, false)
		g.w(" != 0 { return ")
		g.exprList2(n.ExprList, t0)
		g.w(" }\n\nreturn ")
		g.rvalue2(n.Expr2, t0)
		g.w(" }()")
	case
		c99.ExprAdd, // Expr '+' Expr
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprDiv, // Expr '/' Expr
		c99.ExprMod, // Expr '%' Expr
		c99.ExprMul, // Expr '*' Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprSub, // Expr '-' Expr
		c99.ExprXor: // Expr '^' Expr

		g.binop(n)
	case
		c99.ExprEq, // Expr "==" Expr
		c99.ExprGe, // Expr ">=" Expr
		c99.ExprGt, // Expr ">" Expr
		c99.ExprLe, // Expr "<=" Expr
		c99.ExprLt, // Expr '<' Expr
		c99.ExprNe: // Expr "!=" Expr

		g.relop(n)
	case c99.ExprLsh:
		g.rvalue2(n.Expr, n.Operand.Type)
		g.w(" << uint(")
		g.rvalue(n.Expr2, false)
	case c99.ExprNot: // '!' Expr
		g.needBool2int++
		g.w(" bool2int(")
		g.rvalue(n.Expr, true)
		switch a := n.Expr.Operand.Address; {
		case n.Expr.Operand.Type.Kind() == c99.Ptr && a != nil && g.escaped(a.Declarator):
			g.w(" == nil)")
		default:
			g.w(" == 0)")
		}
	default:
		todo("", g.position0(n), n.Case, n.Operand)
	}
}

func (g *gen) constant(n *c99.Expr, typedIntLiterals bool) {
	switch x := n.Operand.Value.(type) {
	case *ir.Int64Value:
		if n.Case == c99.ExprChar {
			g.w(" %s", strconv.QuoteRuneToASCII(rune(x.Value)))
			return
		}

		f := " %d"
		m := n
		s := ""
		for done := false; !done; {
			switch m.Case {
			case c99.ExprInt: // INTCONST
				s = string(m.Token.S())
				done = true
			case
				c99.ExprCast,       // '(' TypeName ')' Expr
				c99.ExprUnaryMinus: // '-' Expr

				m = m.Expr
			default:
				done = true
			}
		}
		s = strings.ToLower(s)
		switch {
		case strings.HasPrefix(s, "0x"):
			f = "%#x"
		}

		if n.Operand.Type.Kind() == c99.Ptr {
			switch {
			case n.Operand.Type.String() == vaListType && x.Value == 1:
				g.w(" %s", ap)
			default:
				g.w(f, uint64(x.Value))
			}
			return
		}

		v := interface{}(x.Value)
		if n.Operand.Type.IsUnsigned() {
			v = uint64(x.Value)
		}
		switch {
		case typedIntLiterals:
			g.w(" %s("+f+")", g.typ(n.Operand.Type), v)
		default:
			g.w(f, v)
		}
	case *ir.Float32Value:
		t := n.Operand.Type
		for {
			switch u := t.(type) {
			case c99.TypeKind:
				switch u {
				case c99.Float:
					g.w(" float32(%v)", x.Value)
					return
				default:
					todo("", g.position0(n), u)
				}
			default:
				todo("%v: %T", g.position0(n), u)
			}
		}
	case *ir.Float64Value:
		t := n.Operand.Type
		for {
			switch u := t.(type) {
			case c99.TypeKind:
				switch u {
				case c99.Double:
					switch {
					case x.Value == 0 && math.Copysign(1, x.Value) == -1:
						g.w(" nz64")
						g.needNZ64 = true
					default:
						g.w(" %v", x.Value)
					}
					return
				default:
					todo("", g.position0(n), u)
				}
			default:
				todo("%v: %T", g.position0(n), u)
			}
		}
	case *ir.StringValue:
		g.w(" ts+%d %s", g.allocString(int(x.StringID)), strComment(x))
	default:
		todo("%v: %T", g.position0(n), x)
	}
}

func (g *gen) uintptr(n *c99.Expr, ptr bool) {
	switch n.Case {
	case c99.ExprSelect: // Expr '.' IDENTIFIER
		t := n.Expr.Operand.Type
		for {
			switch x := t.(type) {
			case *c99.NamedType:
				t = x.Type
			case *c99.StructType:
				layout := g.model.Layout(x)
				for _, v := range layout {
					if v.Bits != 0 {
						todo("", g.position0(n))
					}
				}
				switch a := n.Operand.Address; {
				case a != nil:
					d := a.Declarator
					g.enqueue(d)
					switch {
					case g.escaped(d):
						g.w(" (%s+%d)", g.mangleDeclarator(a.Declarator), a.Offset)
					default:
						switch {
						case ptr && a.Offset == 0:
							g.w(" &%s", g.mangleDeclarator(a.Declarator))
						case a.Offset != 0:
							g.w("uintptr(unsafe.Pointer(&%s))+%d", g.mangleDeclarator(a.Declarator), a.Offset)
						default:
							todo("", g.position0(n), ptr, n.Operand)
						}
					}
				default:
					t := n.Expr.Operand.Type
					for {
						switch x := t.(type) {
						case *c99.StructType:
							g.uintptr(n.Expr, ptr)
							d := x.Field(n.Token2.Val)
							if d == nil {
								todo("")
							}
							g.w(" + %d", layout[d.Field].Offset)
							return
						case *c99.TaggedStructType:
							t = x.Type
						default:
							todo("%v: %v", g.position0(n), x)
						}
					}
				}
				return
			case *c99.TaggedStructType:
				t = x.Type
			default:
				todo("%v: %v", g.position0(n), x)
			}
		}
	case c99.ExprIdent: // IDENTIFIER
		a := n.Operand.Address
		g.enqueue(a.Declarator)
		t := n.Operand.Type
		for {
			switch x := t.(type) {
			case *c99.PointerType:
				if ptr {
					g.w(" &")
				}
				g.w("%s", g.mangleDeclarator(a.Declarator))
				return
			case *c99.NamedType:
				if x.Name == idVaList {
					g.w(" %s", g.mangleDeclarator(a.Declarator))
					return
				}

				todo("%v: %q", g.position0(n), dict.S(x.Name))
			case *c99.TaggedStructType:
				switch {
				case ptr:
					g.w("&%s", g.mangleDeclarator(a.Declarator))
				default:
					g.w("uintptr(unsafe.Pointer(&%s))", g.mangleDeclarator(a.Declarator))
				}
				return
			default:
				todo("%v: %T", g.position0(n), x)
			}
		}
	case c99.ExprIndex: // Expr '[' ExprList ']'
		t := n.Expr.Operand.Type
		for {
			switch x := t.(type) {
			case *c99.PointerType:
				switch a := n.Operand.Address; {
				case a != nil:
					g.w(" %s+%d", g.mangleDeclarator(a.Declarator), a.Offset)
					g.enqueue(a.Declarator)
				default:
					g.uintptr(n.Expr, false)
					g.w(" +%d*uintptr(", g.model.Sizeof(x.Item))
					g.exprList(n.ExprList, false)
					g.w(")")
				}
				return
			default:
				todo("%v: %T", g.position0(n), x)
			}
		}
	case c99.ExprPSelect: // Expr "->" IDENTIFIER
		t := n.Expr.Operand.Type
		for {
			switch x := t.(type) {
			case *c99.NamedType:
				t = x.Type
			case *c99.PointerType:
				switch a := n.Operand.Address; {
				case a != nil:
					switch {
					case ptr:
						g.w(" (*uintptr)(unsafe.Pointer(%s+%d))", g.mangleDeclarator(a.Declarator), a.Offset)
					default:
						g.w(" (%s+%d)", g.mangleDeclarator(a.Declarator), a.Offset)
					}
					g.enqueue(a.Declarator)
				default:
					todo("", g.position0(n))
				}
				return
			default:
				todo("%v: %T", g.position0(n), x)
			}
		}
	case c99.ExprString: // STRINGLITERAL
		x := n.Operand.Value.(*ir.StringValue)
		g.w(" ts+%d %s", g.allocString(int(x.StringID)), strComment(x))
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		t := n.Operand.Type
		for {
			switch x := t.(type) {
			case *c99.PointerType:
				g.rvalue(n, false)
				return
			default:
				todo("%v: %T", g.position0(n), x)
			}
		}
	case c99.ExprAddrof: // '&' Expr
		a := n.Operand.Address
		g.w("(%s+%d)", g.mangleDeclarator(a.Declarator), a.Offset)
		g.enqueue(a.Declarator)
	case c99.ExprInt: // INTCONST
		g.w(" %d", uintptr(n.Operand.Value.(*ir.Int64Value).Value))
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		g.w(" func() uintptr { if ")
		g.rvalue(n.Expr, false)
		g.w(" != 0 { return ")
		switch {
		case isSingleExpression(n.ExprList):
			g.uintptr(n.ExprList.Expr, false)
		default:
			todo("", g.position0(n))
		}
		g.w(" }\n\nreturn ")
		g.uintptr(n.Expr2, false)
		g.w(" }()")
	case c99.ExprPExprList: // '(' ExprList ')'
		if isSingleExpression(n.ExprList) {
			g.uintptr(n.ExprList.Expr, ptr)
			return
		}

		todo("", g.position0(n))
	case c99.ExprDeref: // '*' Expr
		g.w(" uintptr(unsafe.Pointer(")
		g.rvalue(n, false)
		g.w("))")
	default:
		todo("", g.position0(n), n.Case, n.Operand)
	}
}

func (g *gen) uintptrValue(n *c99.Expr) {
	t := n.Operand.Type
	switch n.Case {
	case c99.ExprIdent: // IDENTIFIER
		if t.Kind() != c99.Ptr {
			panic(fmt.Errorf("%v: internal error: %s", g.position0(n), n.Operand))
		}

		d := n.Operand.Address.Declarator
		g.enqueue(d)
		switch {
		case g.escaped(d):
			g.w(" *(*uintptr)(unsafe.Pointer(%s))", g.mangleDeclarator(d))
		default:
			g.w(" %s", g.mangleDeclarator(d))
		}
	case c99.ExprPExprList: // '(' ExprList ')'
		if isSingleExpression(n.ExprList) {
			g.uintptrValue(n.ExprList.Expr)
			return
		}

		todo("", g.position0(n))
	case c99.ExprCast: // '(' TypeName ')' Expr
		g.uintptrValue(n.Expr)
	case c99.ExprInt: // INTCONST
		g.w(" %d", uintptr(n.Operand.Value.(*ir.Int64Value).Value))
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		if t.Kind() != c99.Ptr {
			panic(fmt.Errorf("%v: internal error: %s", g.position0(n), n.Operand))
		}

		g.rvalue(n, false)
	case c99.ExprAddrof: // '&' Expr
		a := n.Operand.Address
		g.w("(%s+%d)", g.mangleDeclarator(a.Declarator), a.Offset)
		g.enqueue(a.Declarator)
	case c99.ExprIndex: // Expr '[' ExprList ']'
		g.uintptr(n, false)
	case c99.ExprPreDec: // "--" Expr
		switch x := t.(type) {
		case *c99.PointerType:
			g.needPreInc = true
			g.w(" preinc(")
			g.lvalue(n.Expr)
			g.w(", %d)", g.model.Sizeof(x.Item))
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprSub: // Expr '-' Expr
		g.w("(")
		defer g.w(")")
		switch x := t.(type) {
		case *c99.PointerType:
			u := n.Expr2.Operand.Type
			switch y := u.(type) {
			case c99.TypeKind:
				switch y {
				case c99.Int:
					g.uintptrValue(n.Expr)
					g.w(" - %d*uintptr(", g.model.Sizeof(x.Item))
					g.rvalue(n.Expr2, false)
					g.w(")")
				default:
					todo("%v: %v", g.position0(n), y)
				}
			default:
				todo("%v: %T", g.position0(n), y)
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprAdd: // Expr '+' Expr
		g.w("(")
		defer g.w(")")
		switch x := t.(type) {
		case *c99.PointerType:
			u := n.Expr2.Operand.Type
			switch y := u.(type) {
			case c99.TypeKind:
				switch y {
				case c99.Int:
					g.uintptrValue(n.Expr)
					g.w(" + %d*uintptr(", g.model.Sizeof(x.Item))
					g.rvalue(n.Expr2, false)
					g.w(")")
				default:
					todo("%v: %v", g.position0(n), y)
				}
			default:
				todo("%v: %T", g.position0(n), y)
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPSelect: // Expr "->" IDENTIFIER
		if t.Kind() != c99.Ptr {
			panic(fmt.Errorf("%v: internal error: %s", g.position0(n), n.Operand))
		}

		switch a := n.Operand.Address; {
		case a != nil:
			g.w(" *(*uintptr)(unsafe.Pointer(%s+%d))", g.mangleDeclarator(a.Declarator), a.Offset)
			g.enqueue(a.Declarator)
		default:
			todo("", g.position0(n))
		}
	case c99.ExprSelect: // Expr '.' IDENTIFIER
		a := n.Operand.Address
		switch {
		case g.escaped(a.Declarator):
			g.w(" (%s+%d)", g.mangleDeclarator(a.Declarator), a.Offset)
		default:
			todo("", g.position0(n))
		}
	case c99.ExprAssign: // Expr '=' Expr
		if t.Kind() != c99.Ptr {
			panic(fmt.Errorf("%v: internal error: %s", g.position0(n), n.Operand))
		}

		g.rvalue(n, false)
	default:
		//dbg("", g.position0(n), n.Case, n.Operand)
		todo("", g.position0(n), n.Case, n.Operand)
	}
}

func (g *gen) exprList2(n *c99.ExprList, t c99.Type) {
	if isSingleExpression(n) {
		g.rvalue2(n.Expr, t)
		return
	}

	todo("", g.position0(n))
}

func strComment(sv *ir.StringValue) string {
	s := dict.S(int(sv.StringID))
	if len(s) > 32 {
		s = append(append([]byte(nil), s[:32]...), []byte("...")...)
		if bytes.Contains(s, []byte("*/")) {
			todo("")
		}
	}
	return fmt.Sprintf("/* %q+%d */", s, sv.Offset)
}

func (g *gen) assignmentValue(n *c99.Expr) {
	if n.Case != c99.ExprAssign { // Expr '=' Expr
		panic("internal error")
	}

	switch {
	case n.Expr.Operand.Type.Kind() == c99.Ptr:
		g.w(" set%d(", g.registerType(g.assignTypes, voidPtrType))
		switch a := n.Expr.Operand.Address; {
		case a != nil && g.escaped(a.Declarator):
			g.w("(*uintptr)(unsafe.Pointer(%s+%d))", g.mangleDeclarator(a.Declarator), a.Offset)
		default:
			g.uintptr(n.Expr, true)
		}
		g.w(", ")
		g.rvalue2(n.Expr2, n.Operand.Type)
		g.w(")")
	default:
		g.w(" set%d(", g.registerType(g.assignTypes, n.Operand.Type))
		g.lvalue(n.Expr)
		g.w(", ")
		g.rvalue2(n.Expr2, n.Operand.Type)
		g.w(")")
	}
}

func (g *gen) registerType(m map[string]int, t c99.Type) int {
	s := g.typ(t)
	if id := m[s]; id != 0 {
		return id
	}

	m[s] = len(m) + 1
	return len(m)
}

func (g *gen) relop(n *c99.Expr) {
	g.needBool2int++
	g.w(" bool2int(")
	l, r := n.Expr.Operand.Type, n.Expr2.Operand.Type
	if n.Expr.Operand.Type.IsArithmeticType() && n.Expr2.Operand.Type.IsArithmeticType() {
		op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
		l, r = op.Type, op.Type
	}
	switch {
	case n.Expr.Operand.Type.Kind() == c99.Ptr:
		g.uintptrValue(n.Expr)
		g.w(" %s ", c99.TokSrc(n.Token))
		g.uintptrValue(n.Expr2)
		g.w(")")
	default:
		g.rvalue2(n.Expr, l)
		g.w(" %s ", c99.TokSrc(n.Token))
		g.rvalue2(n.Expr2, r)
		g.w(")")
	}
}

func (g *gen) binop(n *c99.Expr) {
	l, r := n.Expr.Operand.Type, n.Expr2.Operand.Type
	if n.Expr.Operand.Type.IsArithmeticType() && n.Expr2.Operand.Type.IsArithmeticType() {
		op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
		l, r = op.Type, op.Type
	}
	switch {
	case
		l.Kind() == c99.Ptr && n.Operand.Type.IsArithmeticType(),
		n.Operand.Type.Kind() == c99.Ptr && l.IsArithmeticType():

		g.rvalue2(n.Expr, n.Operand.Type)
	default:
		g.rvalue2(n.Expr, l)
	}
	g.w(" %s ", c99.TokSrc(n.Token))
	switch {
	case
		r.Kind() == c99.Ptr && n.Operand.Type.IsArithmeticType(),
		n.Operand.Type.Kind() == c99.Ptr && r.IsArithmeticType():

		g.rvalue2(n.Expr2, n.Operand.Type)
	default:
		g.rvalue2(n.Expr2, r)
	}
}

func (g *gen) rvalue2(n *c99.Expr, t c99.Type) {
	if n.Operand.Type.Equal(t) {
		g.rvalue(n, true)
		return
	}

	g.convert(n, t)
}

func (g *gen) convert(n *c99.Expr, t c99.Type) {
	op := n.Operand
	switch to := t.(type) {
	case *c99.PointerType:
		if op.Address == c99.Null {
			g.w(" 0")
			return
		}

		if to.Item.Kind() == c99.Function {
			switch from := op.Type.(type) {
			case *c99.PointerType:
				if from.Item.Kind() != c99.Function {
					todo("", g.position0(n))
				}

				if n.Operand.Type.Equal(t) {
					todo("", g.position0(n))
				}

				if op.Address != nil {
					op.Address, _ = g.normalizeAddress(op.Address)
					if op.Address.Declarator.Type.Equal(to.Item) && op.Address.Offset == 0 {
						g.rvalue(n, false)
						return
					}
				}

				// (*(*to)(unsafe.Pointer(&struct{ f from }{expr})))
				g.w("(*(*%s)(unsafe.Pointer(&struct{ f %s }{", g.typ(to.Item), g.typ(from.Item))
				g.rvalue(n, false)
				g.w("})))")
			default:
				todo("", g.position0(n))
			}
			return
		}

		t := op.Type
		for done := false; !done; {
			switch from := t.(type) {
			case *c99.NamedType:
				t = from.Type
			case *c99.PointerType:
				g.rvalue(n, false)
				return
			case c99.TypeKind:
				switch from {
				case c99.Int:
					switch {
					case n.Operand.Value != nil:
						g.rvalue(n, false)
					default:
						g.w(" uintptr(")
						g.rvalue(n, false)
						g.w(")")
					}
				default:
					todo("%v: %v %T -> %T", g.position0(n), op, from, to)
				}
				return
			default:
				todo("%v: %v %T -> %T", g.position0(n), op, from, to)
			}
		}
	case *c99.TaggedEnumType:
		if op.Value != nil {
			g.w(" %d", op.Value.(*ir.Int64Value).Value)
			return
		}
	}

	if op.Type.IsIntegerType() && op.Value != nil {
		n.Operand.Type = t
		g.rvalue(n, true)
		return
	}

	g.w(" %s(", g.typ(t))
	g.rvalue(n, false)
	g.w(")")
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

// dbg only
func (g *gen) position0(n c99.Node) token.Position { return g.in[0].FileSet.PositionFor(n.Pos(), true) }

func (g *gen) position(n *c99.Declarator) token.Position {
	return g.in[g.units[n]].FileSet.PositionFor(n.Pos(), true)
}

func (g *gen) enqueueNumbered(n *c99.Declarator) {
	if _, ok := g.nums[n]; ok {
		return
	}

	g.num++
	g.nums[n] = g.num
	g.queue.PushBack(n)
}

func (g *gen) typ(t c99.Type) string { return g.ptyp(t, true) }

func (g *gen) ptyp(t c99.Type, ptr2uintptr bool) string {
	var buf bytes.Buffer
	switch x := t.(type) {
	case
		*c99.ArrayType,
		*c99.FunctionType:

		g.typ0(&buf, x)
		return buf.String()
	case *c99.NamedType:
		if x.Name == idVaList {
			if ptr2uintptr {
				return "[]interface{}"
			}

			return fmt.Sprintf("%s", dict.S(x.Name))
		}

		g.enqueue(x)
		return fmt.Sprintf("T%s", dict.S(x.Name))
	case *c99.PointerType:
		if ptr2uintptr {
			return "uintptr"
		}

		g.typ0(&buf, t)
		return buf.String()
	case *c99.StructType:
		buf.WriteString(" struct{")
		for _, v := range x.Fields {
			fmt.Fprintf(&buf, "%s ", mangleIdent(v.Name, true))
			g.typ0(&buf, v.Type)
			buf.WriteByte(';')
		}
		buf.WriteByte('}')
		return buf.String()
	case *c99.TaggedEnumType:
		g.enqueue(x)
		return fmt.Sprintf("E%s", dict.S(x.Tag))
	case *c99.TaggedStructType:
		g.enqueue(x)
		return fmt.Sprintf("S%s", dict.S(x.Tag))
	case c99.TypeKind:
		switch x {
		case
			c99.Char,
			c99.Int,
			c99.Long,
			c99.LongLong,
			c99.SChar:

			return fmt.Sprintf("int%d", g.model[x].Size*8)
		case
			c99.UChar,
			c99.UShort,
			c99.UInt,
			c99.ULong:

			return fmt.Sprintf("uint%d", g.model[x].Size*8)
		case c99.Float:
			return fmt.Sprintf("float32")
		case c99.Double:
			return fmt.Sprintf("float64")
		default:
			todo("", x)
		}
	default:
		todo("%T %v", x, x)
	}
	panic("unreachable")
}

func (g *gen) typ0(buf *bytes.Buffer, t c99.Type) {
	for {
		switch x := t.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil {
				todo("")
			}
			fmt.Fprintf(buf, "[%d]", x.Size.Value.(*ir.Int64Value).Value)
			t = x.Item
		case *c99.FunctionType:
			fmt.Fprintf(buf, " func(*%sTLS", crt)
			switch {
			case len(x.Params) == 1 && x.Params[0].Kind() == c99.Void:
				// nop
			default:
				for _, v := range x.Params {
					buf.WriteString(", ")
					buf.WriteString(g.typ(v))
				}
			}
			if x.Variadic {
				fmt.Fprintf(buf, ", ...interface{}")
			}
			buf.WriteString(")")
			if x.Result != nil && x.Result.Kind() != c99.Void {
				buf.WriteString(" " + g.typ(x.Result))
			}
			return
		case *c99.NamedType:
			if x.Name == idVaList {
				buf.WriteString("[]interface{}")
				return
			}

			g.enqueue(x)
			fmt.Fprintf(buf, "T%s", dict.S(x.Name))
			return
		case *c99.PointerType:
			t = x.Item
			if t.Kind() == c99.Void {
				buf.WriteString(" uintptr")
				return
			}

			buf.WriteByte('*')
		case *c99.TaggedStructType:
			g.enqueue(x)
			fmt.Fprintf(buf, "S%s", dict.S(x.Tag))
			return
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Int,
				c99.SChar,
				c99.UShort:

				buf.WriteString(g.ptyp(x, false))
				return
			default:
				todo("", x)
			}
		default:
			todo("%T(%v)", x, x)
		}
	}
}

func (g *gen) w(s string, args ...interface{}) {
	if _, err := fmt.Fprintf(&g.out0, s, args...); err != nil {
		panic(err)
	}
	if traceWrites {
		fmt.Fprintf(os.Stderr, s, args...)
	}
}

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

						break // ok
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

func compact(s string, maxLines int) string {
	a := strings.Split(s, "\n")
	w := 0
	for _, v := range a {
		v = strings.TrimSpace(v)
		if v != "" {
			a[w] = v
			w++
		}
	}
	a = a[:w]
	if len(a) > maxLines {
		a = a[:maxLines]
	}
	return strings.Join(a, "\n")
}
