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
	"runtime/debug"
	"sort"
	"strings"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
)

const (
	crt = "crt"
)

var (
	dict        = xc.Dict
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
	bss           int64
	ds            int64
	errs          scanner.ErrorList
	externals     map[int]*c99.Declarator
	fset          *token.FileSet
	in            []*c99.TranslationUnit
	internalNames map[int]struct{}
	internals     []map[int]*c99.Declarator
	model         c99.Model
	num           int
	nums          map[*c99.Declarator]int
	out           io.Writer
	produced      map[*c99.Declarator]struct{}
	queue         list.List
	strings       map[int]int64
	text          []int
	ts            int64
	units         map[*c99.Declarator]int
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		externals:     map[int]*c99.Declarator{},
		in:            in,
		internalNames: map[int]struct{}{},
		internals:     make([]map[int]*c99.Declarator, len(in)),
		nums:          map[*c99.Declarator]int{},
		out:           out,
		produced:      map[*c99.Declarator]struct{}{},
		strings:       map[int]int64{},
		units:         map[*c99.Declarator]int{},
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

		g.w("\nfunc main() { X_start(%s.NewTLS(), 0, 0) } //TODO real args\n", crt)
		g.produce(sym)
	default:
		var a []string
		for nm := range g.externals {
			a = append(a, string(dict.S(nm)))
		}
		sort.Strings(a)
		for _, nm := range a {
			g.produce(g.externals[dict.SID(nm)])
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
	return nil
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

func (g *gen) produce(n *c99.Declarator) {
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
			n = x.Value.(*c99.Declarator)
			g.queue.Remove(x)
			continue
		}

		return
	}
}

func (g *gen) tld(n *c99.Declarator) {
	if n.Initializer == nil {
		switch x := n.Type.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil {
				todo("")
			}
			g.w("\nvar %s = bss + %d\n", g.mangleDeclarator(n), g.allocBSS(n.Type))
		default:
			todo("%v: %T", g.position(n), x)
		}
		return
	}

	switch n.Initializer.Case {
	//TODO case c99.InitializerCompLit: // '{' InitializerList CommaOpt '}'
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
					g.produce(d)
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
	if t.Result.Kind() != c99.Void {
		g.w(" %s", g.typ(t.Result))
	}
	g.functionBody(n.FunctionDefinition.FunctionBody, n.FunctionDefinition.LocalVariables())
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

func (g *gen) functionBody(n *c99.FunctionBody, vars []*c99.Declarator) {
	g.compoundStmt(n.CompoundStmt, vars)
}

func (g *gen) compoundStmt(n *c99.CompoundStmt, vars []*c99.Declarator) {
	g.w(" {\n")
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
			malloc := "Malloc"
			if initializer != nil && initializer.Case == c99.InitializerExpr {
				o := initializer.Expr.Operand
				if o.Type != nil && (o.IsZero() || o.Addr == c99.Null) {
					initializer = nil
					malloc = "Calloc"
				}
			}
			if v.ScopeNum != 0 || initializer == nil {
				switch {
				case v.AddressTaken:
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

			if v.AddressTaken {
				todo("", g.position(v))
			}
			switch n := initializer; n.Case {
			//TODO case c99.InitializerCompLit: // '{' InitializerList CommaOpt '}'
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

					todo("", g.position0(n), o)
				}

				todo("", g.position0(n), o)
			default:
				todo("", g.position0(n), n.Case)
			}
		}
		g.w(")\n")
	}
	if len(free) != 0 {
		g.w("defer func() {\n")
		for _, v := range free {
			g.w("%s.Free(%s)\n", crt, g.mangleDeclarator(v))
		}
		g.w("}()\n")
	}
	g.blockItemListOpt(n.BlockItemListOpt)
	g.w("}\n")
}

func (g *gen) blockItemListOpt(n *c99.BlockItemListOpt) {
	if n == nil {
		return
	}

	g.blockItemList(n.BlockItemList)
}

func (g *gen) blockItemList(n *c99.BlockItemList) {
	for ; n != nil; n = n.BlockItemList {
		g.blockItem(n.BlockItem)
	}
}

func (g *gen) blockItem(n *c99.BlockItem) {
	switch n.Case {
	case c99.BlockItemDecl: // Declaration
		g.declaration(n.Declaration)
	case c99.BlockItemStmt: // Stmt
		g.stmt(n.Stmt)
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
	//TODO case c99.InitDeclaratorBase: // Declarator
	//TODO case c99.InitDeclaratorInit: // Declarator '=' Initializer
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) stmt(n *c99.Stmt) {
	switch n.Case {
	case c99.StmtBlock: // CompoundStmt
		g.compoundStmt(n.CompoundStmt, nil)
	case c99.StmtExpr: // ExprStmt
		g.exprStmt(n.ExprStmt)
	case c99.StmtIter: // IterationStmt
		g.iterationStmt(n.IterationStmt)
	case c99.StmtJump: // JumpStmt
		g.jumpStmt(n.JumpStmt)
	//TODO case c99.StmtLabeled: // LabeledStmt
	case c99.StmtSelect: // SelectionStmt
		g.selectionStmt(n.SelectionStmt)
	default:
		todo("", g.position0(n), n.Case)
	}
	g.w("\n")
}

func (g *gen) jumpStmt(n *c99.JumpStmt) {
	switch n.Case {
	//TODO case c99.JumpStmtBreak : // "break" ';'
	//TODO case c99.JumpStmtContinue : // "continue" ';'
	//TODO case c99.JumpStmtGoto : // "goto" IDENTIFIER ';'
	case c99.JumpStmtReturn: // "return" ExprListOpt ';'
		g.w("return ")
		g.exprListOpt(n.ExprListOpt, false)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) iterationStmt(n *c99.IterationStmt) {
	switch n.Case {
	//TODO case c99.IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
	//TODO case c99.IterationStmtForDecl: // "for" '(' Declaration ExprListOpt ';' ExprListOpt ')' Stmt
	//TODO case c99.IterationStmtFor: // "for" '(' ExprListOpt ';' ExprListOpt ';' ExprListOpt ')' Stmt
	//TODO case c99.IterationStmtWhile: // "while" '(' ExprList ')' Stmt
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) selectionStmt(n *c99.SelectionStmt) {
	switch n.Case {
	//TODO case c99.SelectionStmtIfElse: // "if" '(' ExprList ')' Stmt "else" Stmt
	case c99.SelectionStmtIf: // "if" '(' ExprList ')' Stmt
		if n.ExprList.Operand.Value != nil {
			todo("")
		}
		g.w("if ")
		g.exprList(n.ExprList, false)
		g.w(" != 0")
		switch n.Stmt.Case {
		case c99.StmtBlock:
			g.stmt(n.Stmt)
		default:
			g.w("{")
			g.stmt(n.Stmt)
			g.w("}")
		}
	//TODO case c99.SelectionStmtSwitch: // "switch" '(' ExprList ')' Stmt
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) exprStmt(n *c99.ExprStmt) {
	g.exprListOpt(n.ExprListOpt, true)
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
			g.voidExpr(n.Expr)
		}
	default:
		if n.ExprList == nil { // single expression list
			g.expr(n.Expr)
			break
		}

		todo("", g.position0(n))
	}
}

func (g *gen) voidExpr(n *c99.Expr) {
	if n.Operand.Value != nil {
		todo("%v: %v", g.position0(n), n.Operand)
	}

	if n.Operand.Addr != nil {
		switch n.Case {
		case c99.ExprAssign:
			// ok
		default:
			todo("%v: %v: %v", g.position0(n), n.Case, n.Operand)
		}
	}

	switch n.Case {
	//TODO case c99.ExprPreInc: // "++" Expr
	//TODO case c99.ExprPreDec: // "--" Expr
	//TODO case c99.ExprSizeofType: // "sizeof" '(' TypeName ')'
	//TODO case c99.ExprSizeofExpr: // "sizeof" Expr
	//TODO case c99.ExprNot: // '!' Expr
	//TODO case c99.ExprAddrof: // '&' Expr
	case c99.ExprPExprList: // '(' ExprList ')'
		if n.ExprList.ExprList == nil { // single expression list
			g.voidExpr(n.ExprList.Expr)
			break
		}

		todo("")
	//TODO case c99.ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	case c99.ExprCast: // '(' TypeName ')' Expr
		o := n.Expr.Operand
		if o.Value != nil {
			break // nop
		}

		if o.Addr != nil {
			todo("", n.TypeName.Type, o)
		}
		todo("", n.TypeName.Type, o)
	//TODO case c99.ExprDeref: // '*' Expr
	//TODO case c99.ExprUnaryPlus: // '+' Expr
	//TODO case c99.ExprUnaryMinus: // '-' Expr
	//TODO case c99.ExprCpl: // '~' Expr
	//TODO case c99.ExprChar: // CHARCONST
	//TODO case c99.ExprNe: // Expr "!=" Expr
	//TODO case c99.ExprModAssign: // Expr "%=" Expr
	//TODO case c99.ExprLAnd: // Expr "&&" Expr
	//TODO case c99.ExprAndAssign: // Expr "&=" Expr
	//TODO case c99.ExprMulAssign: // Expr "*=" Expr
	//TODO case c99.ExprPostInc: // Expr "++"
	//TODO case c99.ExprAddAssign: // Expr "+=" Expr
	//TODO case c99.ExprPostDec: // Expr "--"
	//TODO case c99.ExprSubAssign: // Expr "-=" Expr
	//TODO case c99.ExprPSelect: // Expr "->" IDENTIFIER
	//TODO case c99.ExprDivAssign: // Expr "/=" Expr
	//TODO case c99.ExprLsh: // Expr "<<" Expr
	//TODO case c99.ExprLshAssign: // Expr "<<=" Expr
	//TODO case c99.ExprLe: // Expr "<=" Expr
	//TODO case c99.ExprEq: // Expr "==" Expr
	//TODO case c99.ExprGe: // Expr ">=" Expr
	//TODO case c99.ExprRsh: // Expr ">>" Expr
	//TODO case c99.ExprRshAssign: // Expr ">>=" Expr
	//TODO case c99.ExprXorAssign: // Expr "^=" Expr
	//TODO case c99.ExprOrAssign: // Expr "|=" Expr
	//TODO case c99.ExprLOr: // Expr "||" Expr
	//TODO case c99.ExprMod: // Expr '%' Expr
	//TODO case c99.ExprAnd: // Expr '&' Expr
	//TODO case c99.ExprMul: // Expr '*' Expr
	//TODO case c99.ExprAdd: // Expr '+' Expr
	//TODO case c99.ExprSub: // Expr '-' Expr
	//TODO case c99.ExprSelect: // Expr '.' IDENTIFIER
	//TODO case c99.ExprDiv: // Expr '/' Expr
	//TODO case c99.ExprLt: // Expr '<' Expr
	case c99.ExprAssign: // Expr '=' Expr
		g.expr(n.Expr)
		g.w(" = ")
		g.expr(n.Expr2)
	//TODO case c99.ExprGt: // Expr '>' Expr
	//TODO case c99.ExprCond: // Expr '?' ExprList ':' Expr
	//TODO case c99.ExprIndex: // Expr '[' ExprList ']'
	//TODO case c99.ExprXor: // Expr '^' Expr
	//TODO case c99.ExprOr: // Expr '|' Expr
	//TODO case c99.ExprFloat: // FLOATCONST
	//TODO case c99.ExprIdent: // IDENTIFIER
	//TODO case c99.ExprInt: // INTCONST
	//TODO case c99.ExprLChar: // LONGCHARCONST
	//TODO case c99.ExprLString: // LONGSTRINGLITERAL
	//TODO case c99.ExprString: // STRINGLITERAL
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.expr(n)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) expr(n *c99.Expr) {
	if a := n.Operand.Addr; a != nil && a.Offset != 0 {
		todo("", g.position0(n))
	}

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
	//TODO case c99.ExprPreInc: // "++" Expr
	//TODO case c99.ExprPreDec: // "--" Expr
	//TODO case c99.ExprSizeofType: // "sizeof" '(' TypeName ')'
	//TODO case c99.ExprSizeofExpr: // "sizeof" Expr
	//TODO case c99.ExprNot: // '!' Expr
	case c99.ExprAddrof: // '&' Expr
		g.expr(n.Expr) // no '&' needed, pointers are hidden from Go runtime
	case c99.ExprPExprList: // '(' ExprList ')'
		if n.ExprList.ExprList == nil { // single expression list
			g.expr(n.ExprList.Expr)
			break
		}

		todo("")
	//TODO case c99.ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	case c99.ExprCast: // '(' TypeName ')' Expr
		g.cast(n)
	//TODO case c99.ExprDeref: // '*' Expr
	//TODO case c99.ExprUnaryPlus: // '+' Expr
	//TODO case c99.ExprUnaryMinus: // '-' Expr
	//TODO case c99.ExprCpl: // '~' Expr
	//TODO case c99.ExprChar: // CHARCONST
	case c99.ExprNe: // Expr "!=" Expr
		g.w(" bool2int(")
		g.expr(n.Expr)
		g.w(" != ")
		g.expr(n.Expr2)
		g.w(")")
	//TODO case c99.ExprModAssign: // Expr "%=" Expr
	//TODO case c99.ExprLAnd: // Expr "&&" Expr
	//TODO case c99.ExprAndAssign: // Expr "&=" Expr
	//TODO case c99.ExprMulAssign: // Expr "*=" Expr
	//TODO case c99.ExprPostInc: // Expr "++"
	//TODO case c99.ExprAddAssign: // Expr "+=" Expr
	//TODO case c99.ExprPostDec: // Expr "--"
	//TODO case c99.ExprSubAssign: // Expr "-=" Expr
	//TODO case c99.ExprPSelect: // Expr "->" IDENTIFIER
	//TODO case c99.ExprDivAssign: // Expr "/=" Expr
	//TODO case c99.ExprLsh: // Expr "<<" Expr
	//TODO case c99.ExprLshAssign: // Expr "<<=" Expr
	//TODO case c99.ExprLe: // Expr "<=" Expr
	//TODO case c99.ExprEq: // Expr "==" Expr
	//TODO case c99.ExprGe: // Expr ">=" Expr
	//TODO case c99.ExprRsh: // Expr ">>" Expr
	//TODO case c99.ExprRshAssign: // Expr ">>=" Expr
	//TODO case c99.ExprXorAssign: // Expr "^=" Expr
	//TODO case c99.ExprOrAssign: // Expr "|=" Expr
	//TODO case c99.ExprLOr: // Expr "||" Expr
	//TODO case c99.ExprMod: // Expr '%' Expr
	//TODO case c99.ExprAnd: // Expr '&' Expr
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.expr(n.Expr)
		g.w("(tls")
		if o := n.ArgumentExprListOpt; o != nil {
			for n := o.ArgumentExprList; n != nil; n = n.ArgumentExprList {
				g.w(", ")
				g.expr(n.Expr)
			}
		}
		g.w(")")
	//TODO case c99.ExprMul: // Expr '*' Expr
	//TODO case c99.ExprAdd: // Expr '+' Expr
	//TODO case c99.ExprSub: // Expr '-' Expr
	//TODO case c99.ExprSelect: // Expr '.' IDENTIFIER
	//TODO case c99.ExprDiv: // Expr '/' Expr
	//TODO case c99.ExprLt: // Expr '<' Expr
	//TODO case c99.ExprAssign: // Expr '=' Expr
	//TODO case c99.ExprGt: // Expr '>' Expr
	//TODO case c99.ExprCond: // Expr '?' ExprList ':' Expr
	case c99.ExprIndex: // Expr '[' ExprList ']'
		var t c99.Type
		switch x := n.Expr.Operand.Type.(type) {
		case *c99.PointerType:
			t = x.Item
		default:
			todo("%v: %T", g.position0(n), x)
		}
		switch x := t.(type) {
		case *c99.PointerType:
			g.w("*(*uintptr)(unsafe.Pointer(")
			g.expr(n.Expr)
			g.w(" + %d*(", g.model.Sizeof(t))
			g.exprList(n.ExprList, false)
			g.w(")))")
		default:
			todo("%v: %T", g.position0(n), x)
		}
	//TODO case c99.ExprXor: // Expr '^' Expr
	//TODO case c99.ExprOr: // Expr '|' Expr
	//TODO case c99.ExprFloat: // FLOATCONST
	case c99.ExprIdent: // IDENTIFIER
		nm := n.Token.Val
		if a := n.Operand.Addr; a != nil && a.Offset != 0 {
			todo("%v: %s", g.position0(n), n.Token.S())
		}
		switch x := n.Scope.LookupIdent(nm).(type) {
		case *c99.Declarator:
			switch x.Linkage {
			case c99.LinkageExternal:
				switch y := g.externals[nm]; {
				case y == nil:
					g.w(" %s.%s", crt, g.mangleDeclarator(x))
				default:
					g.w(" %s", g.mangleDeclarator(y))
					g.enqueue(y)
				}
			case c99.LinkageInternal:
				g.w(" %s", g.mangleDeclarator(x))
				g.enqueue(x)
			case c99.LinkageNone:
				g.w(" %s", g.mangleDeclarator(x))
			default:
				todo("%v: %s %v", g.position(x), n.Token.S(), x.Linkage)
			}
		default:
			todo("%v: %s %T", g.position0(n), n.Token.S(), x)
		}
	//TODO case c99.ExprInt: // INTCONST
	//TODO case c99.ExprLChar: // LONGCHARCONST
	//TODO case c99.ExprLString: // LONGSTRINGLITERAL
	//TODO case c99.ExprString: // STRINGLITERAL
	default:
		todo("", g.position0(n), n.Case)
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

func (g *gen) enqueue(n *c99.Declarator) { g.queue.PushBack(n) }

func (g *gen) cast(n *c99.Expr) {
	// (' TypeName ')' Expr
	to := normalizeType(n.TypeName.Type)
	from := normalizeType(n.Expr.Operand.Type)
	if a := n.Expr.Operand.Addr; a != nil {
		if a.Linkage != ir.ExternalLinkage {
			todo("")
		}

		if a.Offset != 0 {
			todo("")
		}

		d, ok := g.externals[int(a.NameID)]
		if !ok {
			todo("")
		}

		from = normalizeType(d.Type)
	}
	if to.Equal(from) {
		g.expr(n.Expr)
		return
	}

	todo("", from, to, n.Operand)
}

func normalizeType(t c99.Type) c99.Type {
	switch x := t.(type) {
	case *c99.FunctionType:
		return x
	case *c99.PointerType:
		switch y := x.Item.(type) {
		case *c99.FunctionType:
			return y
		default:
			todo("%T", y)
		}
	case c99.TypeKind:
		switch x {
		case
			c99.Int,
			c99.Void:

			return x
		default:
			todo("%v", x)
		}
	default:
		todo("%T", x)
	}
	panic("unreachable")
}

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
		//TODO produce the definition
		return fmt.Sprintf("T%s", dict.S(x.Name))
	case *c99.PointerType:
		if ptr2uintptr {
			return "uintptr"
		}

		g.typ0(&buf, t)
		return buf.String()
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
			//TODO produce the definition
			fmt.Fprintf(buf, "T%s", dict.S(x.Name))
			return
		case *c99.PointerType:
			t = x.Item
			buf.WriteByte('*')
		case c99.TypeKind:
			switch x {
			case c99.Char:
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
	if _, err := fmt.Fprintf(g.out, s, args...); err != nil {
		panic(err)
	}
	if traceWrites {
		fmt.Fprintf(os.Stderr, s, args...)
	}
}

func todo(msg string, args ...interface{}) {
	if msg == "" {
		msg = strings.Repeat("%v ", len(args))
	}
	panic(fmt.Errorf(msg, args...))
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
								&c99.PointerType{&c99.PointerType{c99.Char}},
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
