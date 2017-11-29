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
	bss       int64
	errs      scanner.ErrorList
	externals map[int]*c99.Declarator
	fset      *token.FileSet
	in        []*c99.TranslationUnit
	internals []map[int]*c99.Declarator
	model     c99.Model
	num       int
	nums      map[*c99.Declarator]int
	out       io.Writer
	produced  map[*c99.Declarator]struct{}
	queue     list.List
	units     map[*c99.Declarator]int
}

func newGen(out io.Writer, in []*c99.TranslationUnit) *gen {
	return &gen{
		externals: map[int]*c99.Declarator{},
		in:        in,
		internals: make([]map[int]*c99.Declarator, len(in)),
		nums:      map[*c99.Declarator]int{},
		out:       out,
		produced:  map[*c99.Declarator]struct{}{},
		units:     map[*c99.Declarator]int{},
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
	if n.Initializer.Type == nil {
		switch x := n.Type.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil {
				todo("")
			}
			g.w("\nvar %s /* %s */ = bss + %d\n", g.mangleDeclarator(n), g.typ(n.Type), g.allocBSS(n.Type))
		default:
			todo("%v: %T", g.position(n), x)
		}
		return
	}

	switch x := n.Type.(type) {
	case *c99.PointerType:
		if n.AddressTaken {
			todo("")
			break
		}

		if d := g.findAddrValue(n.Initializer.Addr); d != nil {
			switch x := d.Type.(type) {
			case *c99.ArrayType:
				g.w("\nvar %s /* %s */ = %s + %d\n", g.mangleDeclarator(n), g.typ(n.Type), g.mangleDeclarator(d), n.Initializer.Addr.Offset)
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
	g.w("\nfunc %s(", g.mangleDeclarator(n))
	names := n.ParameterNames()
	t := n.Type.(*c99.FunctionType)
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
	g.w(")")
	if t.Result.Kind() != c99.Void {
		g.w(" %s", g.typ(t.Result))
	}
	g.functionBody(n.FunctionDefinition.FunctionBody, n.FunctionDefinition.LocalVariables())
	g.w("\n")
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
	switch n.Linkage {
	case c99.LinkageNone:
		if num, ok := g.nums[n]; ok {
			return fmt.Sprintf("_%d%s", num, dict.S(nm))
		}

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
	for _, v := range vars {
		if v.ScopeNum != 0 {
			g.nums[v] = v.ScopeNum
		}
		g.declarator(v, true)
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
		todo("", g.position0(n), n.Case)
	}
	g.w("\n")
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
		todo("", g.position0(n))
	}
}

func (g *gen) voidExpr(n *c99.Expr) {
	switch x := n.Operand.Type.(type) {
	case c99.TypeKind:
		switch x {
		case c99.Void:
			g.expr(n)
		default:
			g.w("_ = ")
			g.expr(n)
		}
	default:
		todo("%v: %T", g.position0(n), x)
	}
}

func (g *gen) expr(n *c99.Expr) {
	switch n.Case {
	//TODO case c99.ExprPreInc: // "++" Expr
	//TODO case c99.ExprPreDec: // "--" Expr
	//TODO case c99.ExprSizeofType: // "sizeof" '(' TypeName ')'
	//TODO case c99.ExprSizeofExpr: // "sizeof" Expr
	//TODO case c99.ExprNot: // '!' Expr
	//TODO case c99.ExprAddrof: // '&' Expr
	case c99.ExprPExprList: // '(' ExprList ')'
		if n.ExprList.ExprList == nil { // sinngle expression list
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
		g.expr(n.Expr)
		g.w("(")
		if o := n.ArgumentExprListOpt; o != nil {
			first := true
			for n := o.ArgumentExprList; n != nil; n = n.ArgumentExprList {
				if !first {
					g.w(", ")
				}
				first = false
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
	//TODO case c99.ExprIndex: // Expr '[' ExprList ']'
	//TODO case c99.ExprXor: // Expr '^' Expr
	//TODO case c99.ExprOr: // Expr '|' Expr
	//TODO case c99.ExprFloat: // FLOATCONST
	case c99.ExprIdent: // IDENTIFIER
		if a := n.Operand.Addr; a != nil {
			if a.Linkage != ir.ExternalLinkage {
				todo("")
			}

			if a.Offset != 0 {
				todo("")
			}

			nm := int(a.NameID)
			d, ok := g.externals[nm]
			if !ok {
				g.w(" %s.", crt)
				g.w("%s", mangleIdent(nm, true))
				break
			}

			g.w("%s", mangleIdent(nm, true))
			g.enqueue(d)
			break
		}

		nm := n.Token.Val
		switch x := n.Scope.LookupIdent(nm).(type) {
		case *c99.Declarator:
			if x.IsFunctionParameter {
				g.w("%s", mangleIdent(nm, false))
				break
			}

			todo("%v: %T", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprInt: // INTCONST
		g.w(" %s(%d)", g.typ(n.Operand.Type), n.Operand.Value.(*ir.Int64Value).Value)
	//TODO case c99.ExprLChar: // LONGCHARCONST
	//TODO case c99.ExprLString: // LONGSTRINGLITERAL
	//TODO case c99.ExprString: // STRINGLITERAL
	default:
		todo("", g.position0(n), n.Case)
	}
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

	todo("")
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
	switch n.Case {
	case
		c99.InitDeclaratorBase, // Declarator
		c99.InitDeclaratorInit: // Declarator '=' Initializer

		g.declarator(n.Declarator, false)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) declarator(n *c99.Declarator, declaringLocalVariables bool) {
	if !n.IsReferenced {
		return
	}

	if n.ScopeNum >= 0 {
		if n.DeclarationSpecifier.IsStatic() {
			g.enqueueNumbered(n)
			return
		}

		if !declaringLocalVariables {
			return
		}

		if n.AddressTaken {
			todo("%v: %s", g.position(n), dict.S(n.Name()))
		}

		g.w("var %s\n", g.mangleDeclarator(n))
		//TODO todo("%v: %s", g.position(n), dict.S(n.Name()))
		return
	}

	todo("", g.position(n))
}

func (g *gen) enqueueNumbered(n *c99.Declarator) {
	g.num++
	g.nums[n] = g.num
	g.queue.PushBack(n)
}

func (g *gen) typ(t c99.Type) string {
	var buf bytes.Buffer
	switch x := t.(type) {
	case *c99.ArrayType:
		g.typ0(&buf, x)
		return buf.String()
	case *c99.PointerType:
		if x.Item.Kind() == c99.Void {
			return "uintptr"
		}
		fmt.Fprintf(&buf, "/* ")
		g.typ0(&buf, x)
		buf.WriteString(" */ uintptr")
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
			//TODO produce the defintion
			fmt.Fprintf(buf, "T%s", dict.S(x.Name))
			return
		case *c99.PointerType:
			t = x.Item
			buf.WriteByte('*')
		case c99.TypeKind:
			switch x {
			case c99.Char:
				buf.WriteString(g.typ(x))
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

						if ex.Initializer.Type != nil && x.Initializer.Type != nil {
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
