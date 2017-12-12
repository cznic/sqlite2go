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
	"runtime"
	"runtime/debug"
	"sort"
	"strings"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
)

func todo(msg string, args ...interface{}) {
	_, f, l, _ := runtime.Caller(1)
	if msg == "" {
		msg = strings.Repeat("%v ", len(args))
	}
	panic(fmt.Errorf("\n\n%v:%d: TODO\n\n%s\n", f, l, fmt.Sprintf(msg, args...)))
}

const (
	crt    = "crt"
	crtDot = crt + "."
)

var (
	dict        = xc.Dict
	testFn      string
	traceOpt    bool
	traceWrites bool

	idMain  = dict.SID("main")
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

type gen struct {
	bss                   int64
	ds                    int64
	errs                  scanner.ErrorList
	escapes               map[*c99.Declarator]struct{}
	externals             map[int]*c99.Declarator
	fset                  *token.FileSet
	in                    []*c99.TranslationUnit
	internalNames         map[int]struct{}
	internals             []map[int]*c99.Declarator
	model                 c99.Model
	nextLabel             int
	num                   int
	nums                  map[*c99.Declarator]int
	out                   io.Writer
	out0                  bytes.Buffer
	produced              map[*c99.Declarator]struct{}
	producedEnumTags      map[int]struct{}
	producedStructTags    map[int]struct{}
	queue                 list.List
	registeredAssignTypes map[string]int
	strings               map[int]int64
	text                  []int
	ts                    int64
	units                 map[*c99.Declarator]int
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		escapes:               map[*c99.Declarator]struct{}{},
		externals:             map[int]*c99.Declarator{},
		in:                    in,
		internalNames:         map[int]struct{}{},
		internals:             make([]map[int]*c99.Declarator, len(in)),
		nums:                  map[*c99.Declarator]int{},
		out:                   out,
		produced:              map[*c99.Declarator]struct{}{},
		producedEnumTags:      map[int]struct{}{},
		producedStructTags:    map[int]struct{}{},
		registeredAssignTypes: map[string]int{},
		strings:               map[int]int64{},
		units:                 map[*c99.Declarator]int{},
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
		sym, ok := g.externals[idStart]
		if !ok {
			todo("")
			break
		}

		g.w("\nvar _ unsafe.Pointer\n")
		g.w("\nfunc main() { X_start(%s.NewTLS(), 0, 0) } //TODO real args\n", crt)
		g.produceDeclarator(sym)
	default:
		var a []string
		for nm := range g.externals {
			a = append(a, string(dict.S(nm)))
		}
		sort.Strings(a)
		for _, nm := range a {
			g.produceDeclarator(g.externals[dict.SID(nm)])
		}
		todo("")
	}
	if err := g.errs.Err(); err != nil {
		return fmt.Errorf("%s", errString(err))
	}

	g.w("\nvar (\n")
	if g.bss != 0 {
		g.w("bss = %s.BSS(&bssInit[0])\n", crt)
		g.w("bssInit [%d]byte\n", g.bss)
	}
	if g.ds != 0 {
		g.w("ds = %s.DS(dsInit)\n", crt)
		g.w("dsInit = []byte{")
		todo("")
		g.w("}\n")
	}
	g.w("ts = %s.TS(\"", crt)
	for _, v := range g.text {
		s := fmt.Sprintf("%q", dict.S(v))
		g.w("%s\\x00", s[1:len(s)-1])
	}
	g.w("\")\n)\n")
	g.w(`
func bool2int(b bool) int32 {
	if b {
		return 1
	}

	return 0
}
`)
	var a []string
	for k := range g.registeredAssignTypes {
		a = append(a, k)
	}
	sort.Strings(a)
	for _, k := range a {
		g.w("\nfunc set%d(l *%[2]s, r %[2]s) %[2]s { *l = r; return r }", g.registeredAssignTypes[k], k)
	}
	return newOpt().do(g.out, &g.out0, testFn)
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
	for {
		func() {
			if _, ok := g.produced[n]; ok {
				return
			}

			g.produced[n] = struct{}{}
			switch n.Type.Kind() {
			case c99.Function:
				g.functionDefinition(n)
			default:
				g.tld(n)
			}
		}()

		if x := g.queue.Front(); x != nil {
			g.queue.Remove(x)
			switch y := x.Value.(type) {
			case *c99.Declarator:
				n = y
				continue
			case *c99.TaggedEnumType:
				g.produceTaggedEnumType(y)
			case *c99.TaggedStructType:
				g.produceTaggedStructType(y)
			default:
				todo("%T", y)
			}
		}
		return
	}
}

func (g *gen) produceTaggedEnumType(t *c99.TaggedEnumType) {
	if _, ok := g.producedEnumTags[t.Tag]; ok {
		return
	}

	g.producedEnumTags[t.Tag] = struct{}{}
	g.w("\ntype E%s %s\n", dict.S(t.Tag), g.typ(t.Type))
	todo("", t, t.Type)
}

func (g *gen) produceTaggedStructType(t *c99.TaggedStructType) {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return
	}

	g.producedStructTags[t.Tag] = struct{}{}
	g.w("\ntype S%s %s\n", dict.S(t.Tag), g.typ(t.Type))
}

func (g *gen) tld(n *c99.Declarator) {
	if n.Initializer == nil {
		switch x := n.Type.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil {
				todo("")
			}
			g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
		case *c99.StructType:
			g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
		default:
			todo("%v: %T", g.position(n), x)
		}
		return
	}

	switch n.Initializer.Case {
	case c99.InitializerExpr: // Expr
		switch x := n.Type.(type) {
		case *c99.PointerType:
			if n.AddressTaken {
				todo("")
			}

			a := n.Initializer.Expr.Operand.Addr
			if d := g.findAddrValue(a); d != nil {
				switch x := d.Type.(type) {
				case *c99.ArrayType:
					g.w("\nvar %s = %s + %d\n", g.mangleDeclarator(n), g.mangleDeclarator(d), a.Offset)
					g.produceDeclarator(d)
				default:
					todo("%v: %T", g.position(n), x)
				}
				break
			}

			todo("", g.position(n))
		default:
			todo("%v: %T", g.position(n), x)
		}
	default:
		todo("", g.position0(n), n.Initializer.Case)
	}
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

func (g *gen) findAddrValue(a *ir.AddressValue) *c99.Declarator {
	if a == nil {
		return nil
	}

	switch a.Linkage {
	case ir.ExternalLinkage:
		return g.externals[int(a.NameID)]
	default:
		panic(a.Linkage)
	}
}

func (g *gen) functionDefinition(n *c99.Declarator) {
	g.nextLabel = 0
	g.w("\nfunc %s(tls *%s.TLS", g.mangleDeclarator(n), crt)
	names := n.ParameterNames()
	t := n.Type.(*c99.FunctionType)
	if len(names) != len(t.Params) {
		if len(names) != 0 {
			todo("")
		}

		names = make([]int, len(t.Params))
	}
	for i, v := range t.Params {
		nm := names[i]
		g.w(", ")
		g.w("%s %s", mangleIdent(nm, false), g.typ(v))
		if v.Kind() == c99.Ptr {
			g.w(" /* %s */", g.ptyp(v, false))
		}
	}
	if t.Variadic {
		todo("")
	}
	g.w(")")
	void := t.Result.Kind() == c99.Void
	if !void {
		g.w(" %s", g.typ(t.Result))
	}
	g.functionBody(n.FunctionDefinition.FunctionBody, n.FunctionDefinition.LocalVariables(), void)
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
		return mangleIdent(nm, true)
	default:
		todo("%s %v", dict.S(nm), n.Linkage)
	}
	panic("unreachable")
}

func (g *gen) functionBody(n *c99.FunctionBody, vars []*c99.Declarator, void bool) {
	if vars == nil {
		vars = []*c99.Declarator{}
	}
	g.compoundStmt(n.CompoundStmt, vars, nil, !void)
}

func (g *gen) compoundStmt(n *c99.CompoundStmt, vars []*c99.Declarator, cases map[*c99.LabeledStmt]int, sentinel bool) {
	if vars != nil {
		g.w(" {")
	}
	w := 0
	for _, v := range vars {
		if !v.IsReferenced {
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
	if len(vars) != 0 {
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
		g.w("var (\n")
		for _, v := range vars {
			initializer := v.Initializer
			malloc := "MustMalloc"
			if initializer != nil && initializer.Case == c99.InitializerExpr {
				o := initializer.Expr.Operand
				if o.Type != nil && (o.IsZero() || o.Addr == c99.Null) {
					initializer = nil
					malloc = "Calloc"
				}
			}
			if v.ScopeNum != 0 || initializer == nil {
				switch {
				case g.escaped(v):
					free = append(free, v)
					g.w("\t%s uintptr = %s.%s(%d) // *%s", g.mangleDeclarator(v), crt, malloc, g.model.Sizeof(v.Type), g.ptyp(v.Type, false))
				default:
					g.w("\t%s %s", g.mangleDeclarator(v), g.typ(v.Type))
					if v.Type.Kind() == c99.Ptr {
						g.w("\t// %s", g.ptyp(v.Type, false))
					}
				}
				g.w("\n")
				continue
			}

			if g.escaped(v) {
				todo("", g.position(v))
			}
			switch n := initializer; n.Case {
			case c99.InitializerExpr: // Expr
				o := n.Expr.Operand
				if o.Value != nil {
					switch v.Type.Kind() {
					case c99.Int:
						if o.IsZero() {
							g.w("\t%s %s\n", g.mangleDeclarator(v), g.typ(v.Type))
							break
						}

						g.w("\t%s %s = %d\n", g.mangleDeclarator(v), g.typ(v.Type), o.Value.(*ir.Int64Value).Value)
					default:
						todo("", g.position(v), v.Type.Kind())
					}
					break
				}

				if o.Addr != nil {
					if o.Addr == c99.Null {
						g.w("\t%s %s\t// %s\n", g.mangleDeclarator(v), g.typ(v.Type), g.ptyp(v.Type, false))
						break
					}

					switch {
					case o.Addr.Linkage == ir.ExternalLinkage:
						d, ok := g.externals[int(o.Addr.NameID)]
						if !ok {
							todo("")
						}
						g.w("\t%s = %v + %d\t// %s\n", g.mangleDeclarator(v), g.mangleDeclarator(d), o.Addr.Offset, g.ptyp(v.Type, false))
					default:
						todo("", o.Addr)
					}
					break
				}

				todo("", g.position0(n), o)
			default:
				todo("", g.position0(n), n.Case)
			}
		}
		g.w(")")
	}
	if len(free) != 0 {
		g.w("\ndefer func() {\n")
		for _, v := range free {
			g.w("%s.Free(%s)\n", crt, g.mangleDeclarator(v))
		}
		g.w("}()\n")
	}
	g.blockItemListOpt(n.BlockItemListOpt, cases)
	if vars != nil {
		if sentinel {
			g.w("\npanic(\"unreachable\")")
		}
		g.w("}\n")
	}
}

func (g gen) escaped(n *c99.Declarator) bool {
	if n.AddressTaken {
		g.escapes[n] = struct{}{}
		return true
	}

	t := n.Type
	switch x := t.(type) {
	case *c99.ArrayType:
		g.escapes[n] = struct{}{}
		return true
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
		case c99.Int:
			return false
		default:
			todo("%v: %v", g.position(n), x)
		}
	default:
		todo("%v: %T", g.position(n), x)
	}
	panic("unreachable")
}

func (g *gen) blockItemListOpt(n *c99.BlockItemListOpt, cases map[*c99.LabeledStmt]int) {
	if n == nil {
		return
	}

	g.blockItemList(n.BlockItemList, cases)
}

func (g *gen) blockItemList(n *c99.BlockItemList, cases map[*c99.LabeledStmt]int) {
	for ; n != nil; n = n.BlockItemList {
		g.blockItem(n.BlockItem, cases)
	}
}

func (g *gen) blockItem(n *c99.BlockItem, cases map[*c99.LabeledStmt]int) {
	switch n.Case {
	case c99.BlockItemDecl: // Declaration
		g.declaration(n.Declaration)
	case c99.BlockItemStmt: // Stmt
		g.stmt(n.Stmt, cases)
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
	if d.ScopeNum == 0 || !d.IsReferenced {
		return
	}

	switch n.Case {
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) stmt(n *c99.Stmt, cases map[*c99.LabeledStmt]int) {
	switch n.Case {
	case c99.StmtBlock: // CompoundStmt
		g.compoundStmt(n.CompoundStmt, nil, cases, false)
	case c99.StmtExpr: // ExprStmt
		g.exprStmt(n.ExprStmt)
	case c99.StmtIter: // IterationStmt
		g.iterationStmt(n.IterationStmt, cases)
	case c99.StmtJump: // JumpStmt
		g.jumpStmt(n.JumpStmt, cases)
	case c99.StmtLabeled: // LabeledStmt
		g.labeledStmt(n.LabeledStmt, cases)
	case c99.StmtSelect: // SelectionStmt
		g.selectionStmt(n.SelectionStmt, cases)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) labeledStmt(n *c99.LabeledStmt, cases map[*c99.LabeledStmt]int) {
	switch n.Case {
	case
		c99.LabeledStmtSwitchCase, // "case" ConstExpr ':' Stmt
		c99.LabeledStmtDefault:    // "default" ':' Stmt

		g.w("\n_%d:", cases[n])
		g.stmt(n.Stmt, nil)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) jumpStmt(n *c99.JumpStmt, cases map[*c99.LabeledStmt]int) {
	switch n.Case {
	case c99.JumpStmtBreak: // "break" ';'
		l, ok := cases[nil]
		if !ok {
			todo("")
		}
		g.w("\ngoto _%d\n", l)
	case c99.JumpStmtReturn: // "return" ExprListOpt ';'
		g.w("\nreturn ")
		g.exprListOpt(n.ExprListOpt, false)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) iterationStmt(n *c99.IterationStmt, cases map[*c99.LabeledStmt]int) {
	switch n.Case {
	case c99.IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
		// A:
		// stmt
		// if exprList != 0 { goto A }
		a := g.label()
		g.w("\n_%d:", a)
		g.stmt(n.Stmt, cases)
		g.w("\nif ")
		g.exprList(n.ExprList, false)
		g.w(" != 0 { goto _%d }\n", a)
	case c99.IterationStmtFor: // "for" '(' ExprListOpt ';' ExprListOpt ';' ExprListOpt ')' Stmt
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
		b := g.label()
		g.w("\n_%d:", a)
		if n.ExprListOpt2 != nil {
			g.w("if ")
			g.exprListOpt(n.ExprListOpt2, false)
			g.w(" == 0 { goto _%d }\n", b)
		}
		g.stmt(n.Stmt, cases)
		if n.ExprListOpt3 != nil {
			g.w("\n")
		}
		g.exprListOpt(n.ExprListOpt3, true)
		g.w("\ngoto _%d\n\n_%d:", a, b)
	case c99.IterationStmtWhile: // "while" '(' ExprList ')' Stmt
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
		g.stmt(n.Stmt, cases)
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

func (g *gen) selectionStmt(n *c99.SelectionStmt, cases map[*c99.LabeledStmt]int) {
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
		g.w(" == 0 { goto _%d }", a)
		g.stmt(n.Stmt, cases)
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
		g.stmt(n.Stmt, cases)
		g.w("\ngoto _%d\n", b)
		g.w("\n_%d:", a)
		g.stmt(n.Stmt2, cases)
		g.w("\n_%d:", b)
	case c99.SelectionStmtSwitch: // "switch" '(' ExprList ')' Stmt
		g.w("\nswitch ")
		g.exprList(n.ExprList, false)
		g.w("{")
		after := g.label()
		m := map[*c99.LabeledStmt]int{nil: after}
		var deflt *c99.LabeledStmt
		for _, v := range n.Cases {
			l := g.label()
			m[v] = l
			switch ce := v.ConstExpr; {
			case ce != nil:
				g.w("\ncase ")
				g.rvalue2(ce.Expr, n.ExprList.Operand.Type)
				g.w(": goto _%d", l)
			default:
				deflt = v
				g.w("\ndefault: goto _%d\n", l)
			}
		}
		if deflt == nil {
			todo("")
		}
		g.w("}")
		g.stmt(n.Stmt, m)
		g.w("\n_%d:", after)
	default:
		todo("", g.position0(n), n.Case)
	}
}

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
		if n.ExprList == nil { // single expression list
			g.rvalue(n.Expr)
			break
		}

		todo("", g.position0(n))
	}
}

func (g *gen) void(n *c99.Expr) {
	if n.Operand.Value != nil {
		todo("", g.position0(n))
	}

	if n.Operand.Addr != nil {
		switch n.Case {
		case c99.ExprAssign: // Expr '=' Expr
			g.w(" *")
			g.lvalue(n.Expr)
			g.w(" = ")
			g.rvalue2(n.Expr2, n.Expr.Operand.Type)
			return
		case c99.ExprPostInc: // Expr "++"
			switch x := n.Operand.Type.(type) {
			case c99.TypeKind:
				switch x {
				case c99.Int:
					g.w(" *(")
					g.lvalue(n.Expr)
					g.w(")++")
					return
				default:
					todo("%v: %v", g.position0(n), x)
				}
			default:
				todo("%v: %T", g.position0(n), x)
			}
		default:
			todo("", g.position0(n), n.Case, n.Operand)
		}
	}

	switch n.Case {
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.rvalue(n)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) lvalue(n *c99.Expr) {
	g.w("&")
	g.rvalue(n)
}

func (g *gen) rvalue(n *c99.Expr) {
	switch x := n.Operand.Value.(type) {
	case nil:
		// ok
	case *ir.Int64Value:
		g.w(" %s(%d)", g.typ(n.Operand.Type), x.Value)
		return
	case *ir.StringValue:
		id := int(x.StringID)
		g.w(" ts+%d", g.allocString(id))
		s := dict.S(id)
		if len(s) > 32 {
			s = append(append([]byte(nil), s[:32]...), []byte("...")...)
			if bytes.Contains(s, []byte("*/")) {
				todo("")
			}
		}
		g.w(" /* %q */", s)
		return
	default:
		todo("%v: %T", g.position0(n), x)
	}

	switch n.Case {
	case c99.ExprPExprList:
		n.Operand.Addr = nil
	case c99.ExprAssign:
		g.assignValue(n)
		return
	}
	if a := n.Operand.Addr; a != nil {
		nm := int(a.NameID)
		ad := n.Scope.LookupIdent(nm)
		switch x := ad.(type) {
		case *c99.Declarator:
			if n.Case == c99.ExprAddrof {
				g.w(" %s+%d", g.mangleDeclarator(x), a.Offset)
				return
			}

			d := x
			switch a.Linkage {
			case ir.ExternalLinkage:
				d = g.externals[nm]
				if d != nil {
					g.enqueue(d)
				}
			case 0: // linkage none
				d = x
			default:
				todo("", g.position0(n), n.Case, n.Operand, a.Linkage)
			}

			dt := x.Type
			if d != nil {
				dt = d.Type
			}
			var sz int64
			switch t := dt.(type) {
			case *c99.ArrayType:
				sz = g.model.Sizeof(t.Item)
				dt = &c99.PointerType{Item: t.Item}
			case *c99.FunctionType:
				dt = &c99.PointerType{Item: dt}
			case
				*c99.PointerType,
				*c99.StructType,
				*c99.TaggedEnumType,
				*c99.TaggedStructType:

				// ok
			case c99.TypeKind:
				switch t {
				case c99.Int:
					// ok
				default:
					todo("", g.position0(n), t)
				}
			default:
				todo("%v: %T", g.position0(n), t)
			}

			if dt.Equal(n.Operand.Type) {
				if a.Offset != 0 {
					todo("", g.position0(n), n.Case, dt, n.Operand)
				}

				if n.Case == c99.ExprIndex {
					todo("", g.position0(n), n.Case, dt, n.Operand)
				}

				if g.escaped(x) {
					g.w("*(*%s)(unsafe.Pointer(%s+%d))", g.ptyp(n.Operand.Type, false), g.mangleDeclarator(x), a.Offset)
					return
				}

				if d == nil {
					g.w(" %s.%s", crt, g.mangleDeclarator(x))
					return
				}

				g.w(" %s", g.mangleDeclarator(d))
				return
			}

			var pkg string
			if d == nil {
				pkg = crtDot
			}
			if g.escaped(x) || dt.Kind() == c99.Ptr {
				if n.Case == c99.ExprIndex && n.ExprList.Operand.Value == nil {
					g.w(" *(*%s)(unsafe.Pointer(%s%s+%d+%d*uintptr(", g.ptyp(n.Operand.Type, false), pkg, g.mangleDeclarator(x), a.Offset, sz)
					g.exprList(n.ExprList, false)
					g.w(")))")
					return
				}

				g.w(" *(*%s)(unsafe.Pointer(%s%s+%d))", g.ptyp(n.Operand.Type, false), pkg, g.mangleDeclarator(x), a.Offset)
				return
			}

			g.w(" *(*%s)(unsafe.Pointer(uintptr(unsafe.Pointer(&%s%s))+%d))", g.ptyp(n.Operand.Type, false), pkg, g.mangleDeclarator(x), a.Offset)
			return
		default:
			todo("%v: %T", g.position0(n), x)
		}
	}

	g.w("(")

	defer g.w(")")

	switch n.Case {
	case
		c99.ExprMul, // Expr '*' Expr
		c99.ExprSub, // Expr '-' Expr
		c99.ExprAdd, // Expr '+' Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprXor, // Expr '^' Expr
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprDiv: // Expr '/' Expr

		g.binop(n)
	case
		c99.ExprLe, // Expr "<=" Expr
		c99.ExprLt, // Expr '<' Expr
		c99.ExprEq, // Expr "==" Expr
		c99.ExprNe: // Expr "!=" Expr

		g.relop(n)
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.rvalue(n.Expr)
		g.w("(tls")
		if o := n.ArgumentExprListOpt; o != nil {
			for n := o.ArgumentExprList; n != nil; n = n.ArgumentExprList {
				g.w(", ")
				g.rvalue(n.Expr)
			}
		}
		g.w(")")
	case c99.ExprDeref: // '*' Expr
		g.w(" *(*%s)(unsafe.Pointer(", g.ptyp(n.Operand.Type, false))
		g.rvalue(n.Expr)
		g.w("))")
	case c99.ExprLOr: // Expr "||" Expr
		g.w(" bool2int((")
		g.rvalue(n.Expr)
		g.w(" != 0) || (")
		g.rvalue(n.Expr2)
		g.w(" != 0))")
	case c99.ExprLAnd: // Expr "&&" Expr
		g.w(" bool2int((")
		g.rvalue(n.Expr)
		g.w(" != 0) && (")
		g.rvalue(n.Expr2)
		g.w(" != 0))")
	case c99.ExprPExprList:
		if n.ExprList.ExprList == nil { // single expression list
			g.rvalue(n.ExprList.Expr)
			return
		}

		todo("", g.position0(n))
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) assignValue(n *c99.Expr) {
	if n.Case != c99.ExprAssign {
		panic("internal error")
	}

	g.w(" set%d(", g.regAssignType(n.Operand.Type))
	g.lvalue(n.Expr)
	g.w(", ")
	g.rvalue2(n.Expr2, n.Operand.Type)
	g.w(")")
}

func (g *gen) regAssignType(t c99.Type) int {
	s := g.typ(t)
	if id := g.registeredAssignTypes[s]; id != 0 {
		return id
	}

	g.registeredAssignTypes[s] = len(g.registeredAssignTypes) + 1
	return len(g.registeredAssignTypes)
}

func (g *gen) relop(n *c99.Expr) {
	g.w(" bool2int(")
	g.binop(n)
	g.w(")")
}

func (g *gen) binop(n *c99.Expr) {
	op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
	g.rvalue2(n.Expr, op.Type)
	g.w(" %s ", c99.TokSrc(n.Token))
	g.rvalue2(n.Expr2, op.Type)
}

func (g *gen) rvalue2(n *c99.Expr, t c99.Type) {
	if n.Operand.Type.Equal(t) {
		g.rvalue(n)
		return
	}

	g.convert(n, t)
}

func (g *gen) convert(n *c99.Expr, t c99.Type) {
	op := n.Operand
	switch to := t.(type) {
	case *c99.TaggedEnumType:
		switch from := n.Operand.Type.(type) {
		case c99.TypeKind:
			switch from {
			case
				c99.Char,
				c99.Int:

				if op.Value != nil {
					g.w(" %d", op.Value.(*ir.Int64Value).Value)
					return
				}

				todo("", g.position0(n))
			default:
				todo("%v: %v %v -> %T", g.position0(n), op, from, to)
			}
		default:
			todo("%v: %v %T -> %T", g.position0(n), op, from, to)
		}
	default:
		todo("%v: %v -> %T", g.position0(n), n.Operand, to)
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

func (g *gen) enqueue(n interface{}) { g.queue.PushBack(n) }

// dbg only
func (g *gen) position0(n c99.Node) token.Position { return g.in[0].FileSet.PositionFor(n.Pos(), true) }

func (g *gen) position(n *c99.Declarator) token.Position {
	return g.in[g.units[n]].FileSet.PositionFor(n.Pos(), true)
}

func (g *gen) enqueueNumbered(n *c99.Declarator) {
	g.num++
	g.nums[n] = g.num
	g.queue.PushBack(n)
}

func (g *gen) typ(t c99.Type) string { return g.ptyp(t, true) }

func (g *gen) ptyp(t c99.Type, ptr2uintptr bool) string {
	var buf bytes.Buffer
	switch x := t.(type) {
	case *c99.ArrayType:
		g.typ0(&buf, x)
		return buf.String()
	case *c99.NamedType:
		todo("")
		//TODO produce the definition
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
			c99.Int:

			return fmt.Sprintf("int%d", g.model[x].Size*8)
		default:
			todo("", x)
		}
	default:
		todo("%T", x)
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
		case *c99.NamedType:
			todo("")
			//TODO produce the definition
			fmt.Fprintf(buf, "T%s", dict.S(x.Name))
			return
		case *c99.PointerType:
			t = x.Item
			buf.WriteByte('*')
		case *c99.TaggedStructType:
			g.enqueue(x)
			fmt.Fprintf(buf, "S%s", dict.S(x.Tag))
			return
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Int:

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
					if ex, ok := g.externals[nm]; ok {
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

					g.externals[nm] = x
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
