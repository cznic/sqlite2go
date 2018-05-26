// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"github.com/cznic/sqlite2go/internal/c99"
	"go/ast"
)

func (g *gen) compoundStmt(n *c99.CompoundStmt, vars []*c99.Declarator, cases map[*c99.LabeledStmt]int, sentinel bool, brk, cont *int, params, escParams []*c99.Declarator, deadcode bool) []ast.Stmt {

	var out []ast.Stmt
	vars = append([]*c99.Declarator(nil), vars...)
	w := 0
	for _, v := range vars {
		if v != allocaDeclarator {
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
		}

		vars[w] = v
		w++
	}
	vars = vars[:w]
	alloca := false
	var malloc int64
	var offp, offv []int64
	for _, v := range escParams {
		malloc = roundup(malloc, 16)
		offp = append(offp, malloc)
		malloc += g.model.Sizeof(v.Type)
	}
	for _, v := range vars {
		if v == allocaDeclarator {
			continue
		}

		if g.escaped(v) {
			malloc = roundup(malloc, 16)
			offv = append(offv, malloc)
			malloc += g.model.Sizeof(v.Type)
		}
	}
	if malloc != 0 {
		out = append(out,
			define(ident("esc"), callFunc(crtP, "MustMalloc", intLit(malloc))),
		)
	}
	if len(vars)+len(escParams) != 0 {
		localNames := map[int]struct{}{}
		num := 0
		for _, v := range append(params, vars...) {
			if v == nil || v == allocaDeclarator {
				continue
			}

			nm := v.Name()
			if _, ok := localNames[nm]; ok {
				num++
				g.nums[v] = num
			}
			localNames[nm] = struct{}{}
		}
		var specs []ast.Spec
		for i, v := range escParams {
			specs = append(specs, commentSpec(
				valSpec(g.mangleDeclarator(v), nil, addInt(ident("esc"), offp[i])),
				"*"+g.ptyp(v.Type, false, 1),
			))
		}

		for _, v := range vars {
			use := func(id *ast.Ident) {
				specs = append(specs, valSpec("_", nil, id))
			}
			switch {
			case v == allocaDeclarator:
				g.needAlloca = true
				alloca = true
				specs = append(specs, valSpec("allocs", sliceTyp("uintptr"), nil))
			case g.escaped(v):
				specs = append(specs, commentSpec(
					valSpec(g.mangleDeclarator(v), nil, addInt(ident("esc"), offv[0])),
					"*"+g.typeComment(v.Type),
				))
				use(g.mangleDeclaratorI(v))
				offv = offv[1:]
			default:
				comm := ""
				switch {
				case v.Type.Kind() == c99.Ptr:
					comm = g.typeComment(v.Type)
				}
				specs = append(specs, commentSpec(
					valSpec(g.mangleDeclarator(v), g.typI(v.Type), nil),
					comm,
				))
				use(g.mangleDeclaratorI(v))
			}
		}
		out = append(out, declStmt(
			defineVars(specs...),
		))
	}
	switch {
	case alloca:
		var defr []ast.Stmt
		if malloc != 0 {
			defr = append(defr, exprStmt(
				callFunc(crtP, "Free", ident("esc")),
			))
		}
		if alloca {
			defr = append(defr, rangeLoop("", "v", ident("allocs"), exprStmt(
				callFunc(crtP, "Free", ident("v")),
			)))
		}
		out = append(out, defers(defr...))
	case malloc != 0:
		out = append(out, defers(exprStmt(
			callFunc(crtP, "Free", ident("esc")),
		)))
	}
	for _, v := range escParams {
		out = append(out, setPtrUnsafe(g.typ(v.Type), g.mangleDeclaratorI(v), ident("a"+string(dict.S(v.Name())))))
	}
	out = append(out, g.blockItemListOpt(n.BlockItemListOpt, cases, brk, cont, &deadcode)...)
	if vars != nil {
		if sentinel && !deadcode {
			out = append(out, returnVal(ident("r")))
		}
	}
	return out
}

func (g *gen) blockItemListOpt(n *c99.BlockItemListOpt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	if n == nil {
		return nil
	}

	return g.blockItemList(n.BlockItemList, cases, brk, cont, deadcode)
}

func (g *gen) blockItemList(n *c99.BlockItemList, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	var stmts []ast.Stmt
	for ; n != nil; n = n.BlockItemList {
		stmts = append(stmts, g.blockItem(n.BlockItem, cases, brk, cont, deadcode)...)
	}
	return stmts
}

func (g *gen) blockItem(n *c99.BlockItem, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	switch n.Case {
	case c99.BlockItemDecl: // Declaration
		return g.declaration(n.Declaration, deadcode)
	case c99.BlockItemStmt: // Stmt
		return g.stmt(n.Stmt, cases, brk, cont, deadcode)
	default:
		todo("", g.position0(n), n.Case)
		return nil
	}
}

func (g *gen) stmt(n *c99.Stmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	switch n.Case {
	case c99.StmtExpr: // ExprStmt
		return g.exprStmt(n.ExprStmt, deadcode)
	case c99.StmtJump: // JumpStmt
		return g.jumpStmt(n.JumpStmt, brk, cont, deadcode)
	case c99.StmtIter: // IterationStmt
		return g.iterationStmt(n.IterationStmt, cases, brk, cont, deadcode)
	case c99.StmtBlock: // CompoundStmt
		return g.compoundStmt(n.CompoundStmt, nil, cases, false, brk, cont, nil, nil, *deadcode)
	case c99.StmtSelect: // SelectionStmt
		return g.selectionStmt(n.SelectionStmt, cases, brk, cont, deadcode)
	case c99.StmtLabeled: // LabeledStmt
		return g.labeledStmt(n.LabeledStmt, cases, brk, cont, deadcode)
	default:
		todo("", g.position0(n), n.Case)
		return nil
	}
}

func (g *gen) labeledStmt(n *c99.LabeledStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	*deadcode = false
	switch n.Case {
	case
		c99.LabeledStmtSwitchCase, // "case" ConstExpr ':' Stmt
		c99.LabeledStmtDefault:    // "default" ':' Stmt

		l, ok := cases[n]
		if !ok {
			todo("", g.position0(n))
		}
		out := g.stmt(n.Stmt, cases, brk, cont, new(bool))
		return labelN(l, out...)
	case c99.LabeledStmtLabel: // IDENTIFIER ':' Stmt
		l := mangleIdent(n.Token.Val, false)
		out := g.stmt(n.Stmt, cases, brk, cont, new(bool))
		return append([]ast.Stmt{gotoStmt(l)}, label(l, out...)...)
	default:
		todo("", g.position0(n), n.Case)
		return nil
	}
}

func (g *gen) selectionStmt(n *c99.SelectionStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	switch n.Case {
	case c99.SelectionStmtSwitch: // "switch" '(' ExprList ')' Stmt
		if n.ExprList.Operand.Value != nil && g.voidCanIgnoreExprList(n.ExprList) {
			//TODO optimize
		}
		var x ast.Expr
		switch el := n.ExprList; {
		case isSingleExpression(el):
			x = g.convert(n.ExprList.Expr, n.SwitchOp.Type)
		default:
			todo("", g.position0(n))
		}
		var (
			out    []ast.Stmt
			scases []*ast.CaseClause
		)

		after := -g.local()
		cases := map[*c99.LabeledStmt]int{}
		var deflt *c99.LabeledStmt
		for _, v := range n.Cases {
			l := g.local()
			cases[v] = l
			switch ce := v.ConstExpr; {
			case ce != nil:
				scases = append(scases, caseStmt(
					g.convert(ce.Expr, n.SwitchOp.Type),
					gotoN(l),
				))
			default:
				deflt = v
				scases = append(scases, caseStmt(
					nil,
					gotoN(l),
				))
			}
		}

		out = append(out, switchStmt(x, scases...))
		if deflt == nil {
			after = -after
			out = append(out, gotoN(after))
		}
		out = append(out, g.stmt(n.Stmt, cases, &after, cont, deadcode)...)
		if after > 0 {
			out = append(out, labelN(after)...)
			*deadcode = false
		}
		return out
	case c99.SelectionStmtIf: // "if" '(' ExprList ')' Stmt
		if n.ExprList.IsZero() {
			a := g.local()

			stmt := g.stmt(n.Stmt, cases, brk, cont, newTrue())
			*deadcode = false

			return flattenStmts(stmtsArr{
				g.exprListVoid(n.ExprList),
				{gotoN(a)},
				stmt,
				labelN(a),
			})
		}

		if n.ExprList.IsNonZero() {
			s := g.stmt(n.Stmt, cases, brk, cont, deadcode)
			*deadcode = false
			return flattenStmts(stmtsArr{
				g.exprListVoid(n.ExprList),
				s,
			})
		}

		// if exprList == 0 { goto A }
		// stmt
		// A:
		a := g.local()
		stmt := g.stmt(n.Stmt, cases, brk, cont, deadcode)
		*deadcode = false
		return flattenStmts(stmtsArr{
			{ifStmt(
				eqZero(g.exprList(n.ExprList)),
				gotoN(a),
			)},
			stmt,
			labelN(a),
		})
	case c99.SelectionStmtIfElse: // "if" '(' ExprList ')' Stmt "else" Stmt
		if n.ExprList.IsZero() {
			a := g.local()
			b := g.local()
			s1 := g.stmt(n.Stmt, cases, brk, cont, newTrue())
			*deadcode = false
			s2 := g.stmt(n.Stmt2, cases, brk, cont, deadcode)
			*deadcode = false
			return flattenStmts(stmtsArr{
				g.exprListVoid(n.ExprList),
				{gotoN(a)},
				s1,
				{gotoN(b)},
				labelN(a, s2...),
				labelN(b),
			})
		}

		if n.ExprList.IsNonZero() {
			a := g.local()
			s1 := g.stmt(n.Stmt, cases, brk, cont, deadcode)
			s2 := g.stmt(n.Stmt2, cases, brk, cont, newTrue())
			*deadcode = false
			return flattenStmts(stmtsArr{
				g.exprListVoid(n.ExprList),
				s1,
				{gotoN(a)},
				s2,
				labelN(a),
			})
		}

		// if exprList == 0 { goto A }
		// stmt
		// goto B
		// A:
		// stmt2
		// B:
		a := g.local()
		b := g.local()
		s1 := g.stmt(n.Stmt, cases, brk, cont, deadcode)
		*deadcode = false
		s2 := g.stmt(n.Stmt2, cases, brk, cont, new(bool))
		*deadcode = false
		return flattenStmts(stmtsArr{
			{ifStmt(
				eqZero(g.exprList(n.ExprList)),
				gotoN(a),
			)},
			s1,
			{gotoN(b)},
			labelN(a, s2...),
			labelN(b),
		})
	default:
		todo("", g.position0(n), n.Case)
		return nil
	}
}

const (
	nTrue    = nBool(1)
	nUnknown = nBool(0)
	nFalse   = nBool(-1)
)

type nBool int8

func (a nBool) Or(b nBool) nBool {
	if a == nTrue || b == nTrue {
		return nTrue
	} else if a == nUnknown || b == nUnknown {
		return nUnknown
	}
	return nFalse
}

func hasGoto(s *c99.Stmt) nBool {
	if s == nil {
		return nFalse
	}
	switch s.Case {
	case c99.StmtJump:
		return nTrue
	case c99.StmtBlock:
		s := s.CompoundStmt
		if s == nil || s.BlockItemListOpt == nil {
			return nFalse
		}
		for l := s.BlockItemListOpt.BlockItemList; l != nil; l = l.BlockItemList {
			if l.BlockItem != nil {
				if has := hasGoto(l.BlockItem.Stmt); has == nTrue || has == nUnknown {
					return has
				}
			}
		}
		return nFalse
	case c99.StmtSelect:
		s := s.SelectionStmt
		if s == nil {
			return nFalse
		}
		return hasGoto(s.Stmt).Or(hasGoto(s.Stmt2))
	case c99.StmtExpr:
		return nFalse
	default:
		return nUnknown
	}
}

func (g *gen) iterationStmt(n *c99.IterationStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) []ast.Stmt {
	switch n.Case {
	case c99.IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
		// A:
		// stmt
		// B: <- continue
		// if exprList != 0 { goto A }
		// goto C
		// C: <- break
		a := g.local()
		b := -g.local()
		c := -g.local()

		*deadcode = false
		stmt := g.stmt(n.Stmt, cases, &c, &b, deadcode)

		var out []ast.Stmt
		out = append(out, labelN(a, stmt...)...)
		if b > 0 {
			out = append(out, labelN(b)...)
			*deadcode = false
		}
		out = append(out, ifStmt(
			neqZero(g.exprList(n.ExprList)),
			gotoN(a),
		))
		if c > 0 {
			out = appendStmts(out, stmtsArr{
				{gotoN(c)},
				labelN(c),
			})
			*deadcode = false
		}
		return out
	case c99.IterationStmtFor: // "for" '(' ExprListOpt ';' ExprListOpt ';' ExprListOpt ')' Stmt
		// ExprListOpt
		// A:
		// if ExprListOpt2 == 0 { goto C }
		// Stmt
		// B: <- continue
		// ExprListOpt3
		// goto A
		// C: <- break

		a := g.local()
		b := -g.local()
		c := -g.local()

		out := flattenStmts(stmtsArr{
			g.exprListOpt(n.ExprListOpt),
			labelN(a),
		})

		*deadcode = false
		if n.ExprListOpt2 != nil {
			c = -c
			out = append(out, ifStmt(
				eqZero(g.exprList(n.ExprListOpt2.ExprList)),
				gotoN(c),
			))
		}
		out = append(out, g.stmt(n.Stmt, cases, &c, &b, deadcode)...)

		if b > 0 {
			out = append(out, labelN(b)...)
			*deadcode = false
		}
		out = appendStmts(out, stmtsArr{
			g.exprListOpt(n.ExprListOpt3),
			{gotoN(a)},
		})
		if c > 0 {
			out = append(out, labelN(c)...)
			*deadcode = false
		}
		return out
	case c99.IterationStmtWhile: // "while" '(' ExprList ')' Stmt
		if n.ExprList.IsNonZero() {
			// A:
			// exprList
			// stmt
			// goto A
			// B:
			a := g.local()
			b := -g.local()
			*deadcode = false
			stmt := g.stmt(n.Stmt, cases, &b, &a, deadcode)

			out := flattenStmts(stmtsArr{
				labelN(a, g.exprListVoid(n.ExprList)...),
				stmt,
				{gotoN(a)},
			})
			if b > 0 {
				out = append(out, labelN(b)...)
				*deadcode = false
			}
			return out
		}

		// A:
		// if exprList == 0 { goto B }
		// stmt
		// goto A
		// B:
		a := g.local()
		b := g.local()
		stmt := g.stmt(n.Stmt, cases, &b, &a, deadcode)
		*deadcode = false
		return flattenStmts(stmtsArr{
			labelN(a,
				ifStmt(
					eqZero(g.exprList(n.ExprList)),
					gotoN(b),
				),
			),
			stmt,
			{gotoN(a)},
			labelN(b),
		})
	default:
		todo("", g.position0(n), n.Case)
		return nil
	}
}

func (g *gen) local() int {
	r := g.nextLabel
	g.nextLabel++
	return r
}

func (g *gen) jumpStmt(n *c99.JumpStmt, brk, cont *int, deadcode *bool) []ast.Stmt {
	if g.mainFn {
		n.ReturnOperand.Type = c99.Int
	}
	switch n.Case {
	case c99.JumpStmtReturn: // "return" ExprListOpt ';'
		defer func() {
			*deadcode = true
		}()
		switch o := n.ExprListOpt; {
		case o != nil:
			switch rt := n.ReturnOperand.Type; {
			case rt == nil:
				switch {
				case isSingleExpression(o.ExprList) && o.ExprList.Expr.Case == c99.ExprCond:
					todo("", g.position0(n))
					return nil
				default:
					return g.exprListVoid(o.ExprList)
				}
			default:
				switch {
				case isSingleExpression(o.ExprList) && o.ExprList.Expr.Case == c99.ExprCond:
					n := o.ExprList.Expr // Expr '?' ExprList ':' Expr
					switch {
					case n.Expr.IsZero() && g.voidCanIgnore(n.Expr):
						todo("", g.position0(n))
						return nil
					case n.Expr.IsNonZero() && g.voidCanIgnore(n.Expr):
						todo("", g.position0(n))
						return nil
					default:
						return []ast.Stmt{
							ifStmt(
								neqZero(g.value(n.Expr, false)),
								returnVal(g.exprListConv(n.ExprList, rt)),
							),
							returnVal(g.convert(n.Expr2, rt)),
						}
					}
				default:
					return []ast.Stmt{returnVal(g.exprListConv(o.ExprList, rt))}
				}
			}
		default:
			return []ast.Stmt{returnVal()}
		}
	case c99.JumpStmtBreak: // "break" ';'
		if *brk < 0 {
			*brk = -*brk // Signal used.
		}
		return []ast.Stmt{gotoN(*brk)}
	case c99.JumpStmtGoto: // "goto" IDENTIFIER ';'
		return []ast.Stmt{gotoStmt(mangleIdent(n.Token2.Val, false))}
	case c99.JumpStmtContinue: // "continue" ';'
		if *cont < 0 {
			*cont = -*cont // Signal used.
		}
		return []ast.Stmt{gotoN(*cont)}
	default:
		todo("", g.position0(n), n.Case)
		return nil
	}
}

func (g *gen) exprStmt(n *c99.ExprStmt, deadcode *bool) []ast.Stmt {
	if *deadcode {
		return nil
	}
	if o := n.ExprListOpt; o != nil {
		return g.exprListVoid(o.ExprList)
	}
	return nil
}
