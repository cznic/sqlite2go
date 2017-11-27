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
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"runtime/debug"
	"sort"
	"strings"

	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
)

var (
	dict = xc.Dict

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

type symbol struct {
	decl  *c99.Declarator
	iunit int
	unit  *c99.TranslationUnit
}

func (e *symbol) position(n c99.Node) token.Position { return e.unit.FileSet.PositionFor(n.Pos(), true) }

type queued struct {
	n   c99.Node
	num int // num > 0: _<num>name
}

type gen struct {
	errs      scanner.ErrorList
	externals map[int]symbol
	fset      *token.FileSet
	in        []*c99.TranslationUnit
	internals []map[int]symbol
	model     c99.Model
	out       io.Writer
	produced  map[symbol]struct{}
	queue     []queued
	num       int
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		externals: map[int]symbol{},
		in:        in,
		internals: make([]map[int]symbol, len(in)),
		out:       out,
		produced:  map[symbol]struct{}{},
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

func (g *gen) produce(sym symbol) {
	if _, ok := g.produced[sym]; ok {
		return
	}

	g.produced[sym] = struct{}{}
	switch sym.decl.Type.Kind() {
	case c99.Function:
		g.functionDefinition(sym)
	default:
		todo("")
	}
}

func (g *gen) functionDefinition(sym symbol) {
	d := sym.decl
	nm := d.Name()
	g.w("func %s(", mangleIdent(nm, sym.decl.Linkage == c99.LinkageExternal))
	names := d.ParameterNames()
	t := sym.decl.Type.(*c99.FunctionType)
	if len(names) != len(t.Params) {
		todo("")
	}
	for i, v := range t.Params {
		nm := names[i]
		if i != 0 {
			g.w(", ")
		}
		g.w("%s %s", mangleIdent(nm, false), g.typ(v))
	}
	if t.Variadic {
		todo("")
	}
	g.w(") ")
	if t.Result.Kind() != c99.Void {
		todo("")
	}
	g.functionBody(d.FunctionDefinition.FunctionBody)
	g.w("\n")
}

func (g *gen) functionBody(n *c99.FunctionBody) {
	g.compoundStmt(n.CompoundStmt)
}

func (g *gen) compoundStmt(n *c99.CompoundStmt) {
	g.w("{\n")
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
		g.declaration(n.Declaration, true)
	case c99.BlockItemStmt: // Stmt
		g.stmt(n.Stmt)
	default:
		todo("", g.position(n), n.Case)
	}
}

func (g *gen) stmt(n *c99.Stmt) {
	switch n.Case {
	//TODO case c99.StmtBlock: // CompoundStmt
	case c99.StmtExpr: // ExprStmt
		g.exprStmt(n.ExprStmt)
	//TODO case c99.StmtIter: // IterationStmt
	//TODO case c99.StmtJump: // JumpStmt
	//TODO case c99.StmtLabeled: // LabeledStmt
	//TODO case c99.StmtSelect: // SelectionStmt
	default:
		todo("", g.position(n), n.Case)
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
		todo("", g.position(n))
	}
}

func (g *gen) voidExpr(n *c99.Expr) {
	switch {
	case n.Operand.Type.Kind() == c99.Void:
		g.expr0(n)
	default:
		todo("", g.position(n))
	}
}

func (g *gen) expr0(n *c99.Expr) {
	switch n.Case {
	//TODO case c99.ExprPreInc: // "++" Expr
	//TODO case c99.ExprPreDec: // "--" Expr
	//TODO case c99.ExprSizeofType: // "sizeof" '(' TypeName ')'
	//TODO case c99.ExprSizeofExpr: // "sizeof" Expr
	//TODO case c99.ExprNot: // '!' Expr
	//TODO case c99.ExprAddrof: // '&' Expr
	//TODO case c99.ExprPExprList: // '(' ExprList ')'
	//TODO case c99.ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	//TODO case c99.ExprCast: // '(' TypeName ')' Expr
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
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		todo("", g.position(n), n.Case, n.Expr.Operand)
	//TODO case c99.ExprMul: // Expr '*' Expr
	//TODO case c99.ExprAdd: // Expr '+' Expr
	//TODO case c99.ExprSub: // Expr '-' Expr
	//TODO case c99.ExprSelect: // Expr '.' IDENTIFIER
	//TODO case c99.ExprDiv: // Expr '/' Expr
	//TODO case c99.ExprLt: // Expr '<' Expr
	//TODO case c99.ExprAssign: // Expr '=' Expr
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
	default:
		todo("", g.position(n), n.Case)
	}
}

// debug only
func (g *gen) position(n c99.Node) token.Position { return g.fset.PositionFor(n.Pos(), true) }

func (g *gen) declaration(n *c99.Declaration, inBlock bool) {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	g.initDeclaratorListOpt(n.InitDeclaratorListOpt, inBlock)
}

func (g *gen) initDeclaratorListOpt(n *c99.InitDeclaratorListOpt, inBlock bool) {
	if n == nil {
		return
	}

	g.initDeclaratorList(n.InitDeclaratorList, inBlock)
}

func (g *gen) initDeclaratorList(n *c99.InitDeclaratorList, inBlock bool) {
	for ; n != nil; n = n.InitDeclaratorList {
		g.initDeclarator(n.InitDeclarator, inBlock)
	}
}

func (g *gen) initDeclarator(n *c99.InitDeclarator, inBlock bool) {
	switch n.Case {
	//TODO case InitDeclaratorBase: // Declarator
	case c99.InitDeclaratorInit: // Declarator '=' Initializer
		g.declarator(n.Declarator, inBlock)
	default:
		todo("", g.position(n), n.Case)
	}
}

func (g *gen) declarator(n *c99.Declarator, inBlock bool) {
	if n.DeclarationSpecifier.IsStatic() && inBlock {
		g.enqueueNumbered(n)
		return
	}

	todo("")
}

func (g *gen) enqueueNumbered(n c99.Node) {
	g.num++
	g.queue = append(g.queue, queued{n, g.num})
}

func (g *gen) enqueue(n c99.Node) { g.queue = append(g.queue, queued{n, 0}) }

func (g *gen) typ(t c99.Type) string {
	var buf bytes.Buffer
	switch x := t.(type) {
	case *c99.PointerType:
		g.typ0(&buf, x)
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
		case *c99.PointerType:
			buf.WriteByte('*')
			t = x.Item
		case c99.TypeKind:
			switch x {
			case c99.Char:
				buf.WriteString(g.typ(x))
				return
			default:
				todo("", x)
			}
		default:
			todo("%T", x)
		}
	}
}

func mangleIdent(nm int, exported bool) string {
	switch {
	case exported:
		return fmt.Sprintf("X%s", dict.S(nm))
	default:
		return fmt.Sprintf("_%s", dict.S(nm))
	}
}

func (g *gen) w(s string, args ...interface{}) {
	if _, err := fmt.Fprintf(g.out, s, args...); err != nil {
		panic(err)
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
	for it, t := range g.in {
		internal := map[int]symbol{}
		g.internals[it] = internal
		for nm, n := range t.FileScope.Idents {
			switch x := n.(type) {
			case *c99.Declarator:
				if x.Type.Kind() != c99.Function || x.FunctionDefinition == nil { //TODO-
					continue
				}

				switch x.Linkage {
				case c99.LinkageExternal:
					extern := symbol{decl: x, unit: t, iunit: it}
					if ex, ok := g.externals[nm]; ok {
						if ex.position(ex.decl) == extern.position(x) {
							break
						}

						todo("%v: %s %v, %v: %v", ex.position(ex.decl), dict.S(nm), ex.decl.Type, extern.position(x), x.Type)
					}
					g.externals[nm] = extern
				case c99.LinkageInternal:
					if _, ok := internal[nm]; ok {
						todo("")
					}
					internal[nm] = symbol{decl: x, unit: t, iunit: it}
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
