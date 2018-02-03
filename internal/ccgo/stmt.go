// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"github.com/cznic/sqlite2go/internal/c99"
)

func (g *gen) compoundStmt(n *c99.CompoundStmt, vars []*c99.Declarator, cases map[*c99.LabeledStmt]int, sentinel bool, brk, cont *int, escParams []*c99.Declarator, deadcode *bool) {
	if vars != nil {
		g.w(" {")
	}
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
	var free []*c99.Declarator
	alloca := false
	if len(vars)+len(escParams) != 0 {
		localNames := map[int]struct{}{}
		num := 0
		for _, v := range vars {
			if v == allocaDeclarator {
				continue
			}

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
			switch {
			case v == allocaDeclarator:
				g.w("\nallocs []uintptr")
				g.needAlloca = true
				alloca = true
			case g.escaped(v):
				free = append(free, v)
				g.w("\n\t%s = %sMustMalloc(%d) // *%s", g.mangleDeclarator(v), crt, g.model.Sizeof(v.Type), g.ptyp(v.Type, false))
			default:
				switch {
				case v.Type.Kind() == c99.Ptr:
					g.w("\n\t%s %s\t// %s", g.mangleDeclarator(v), g.ptyp(v.Type, true), g.ptyp(v.Type, false))
				default:
					g.w("\n\t%s %s", g.mangleDeclarator(v), g.typ(v.Type))
				}
				if v.Referenced == 0 || v.Referenced == v.AssignedTo {
					g.w("\n_ = %s", g.mangleDeclarator(v))
				}
			}
		}
		if len(vars)+len(escParams) != 1 {
			g.w("\n)")
		}
		for _, v := range escParams {
			g.w("\n*(*%s)(unsafe.Pointer(%s)) = a%s", g.typ(v.Type), g.mangleDeclarator(v), dict.S(v.Name()))
		}
	}
	if len(free) != 0 || alloca {
		g.w("\ndefer func() {")
		for _, v := range free {
			g.w("\n%sFree(%s)", crt, g.mangleDeclarator(v))
		}
		if alloca {
			g.w(`
for _, v := range allocs {
	%sFree(v)
}`, crt)
		}
		g.w("\n}()")
	}
	g.blockItemListOpt(n.BlockItemListOpt, cases, brk, cont, deadcode)
	if vars != nil {
		if sentinel && !*deadcode {
			g.w(";return r")
		}
		g.w("\n}")
	}
}

func (g *gen) blockItemListOpt(n *c99.BlockItemListOpt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	if n == nil {
		return
	}

	g.blockItemList(n.BlockItemList, cases, brk, cont, deadcode)
}

func (g *gen) blockItemList(n *c99.BlockItemList, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	for ; n != nil; n = n.BlockItemList {
		g.blockItem(n.BlockItem, cases, brk, cont, deadcode)
	}
}

func (g *gen) blockItem(n *c99.BlockItem, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	switch n.Case {
	case c99.BlockItemDecl: // Declaration
		g.declaration(n.Declaration)
	case c99.BlockItemStmt: // Stmt
		g.stmt(n.Stmt, cases, brk, cont, deadcode)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) stmt(n *c99.Stmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	switch n.Case {
	case c99.StmtExpr: // ExprStmt
		if *deadcode {
			break
		}

		g.exprStmt(n.ExprStmt)
	case c99.StmtJump: // JumpStmt
		g.jumpStmt(n.JumpStmt, brk, cont, deadcode)
	case c99.StmtIter: // IterationStmt
		g.iterationStmt(n.IterationStmt, cases, brk, cont, deadcode)
	case c99.StmtBlock: // CompoundStmt
		g.compoundStmt(n.CompoundStmt, nil, cases, false, brk, cont, nil, deadcode)
	case c99.StmtSelect: // SelectionStmt
		g.selectionStmt(n.SelectionStmt, cases, brk, cont, deadcode)
	case c99.StmtLabeled: // LabeledStmt
		g.labeledStmt(n.LabeledStmt, cases, brk, cont, deadcode)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) labeledStmt(n *c99.LabeledStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	f := false
	switch n.Case {
	case
		c99.LabeledStmtSwitchCase, // "case" ConstExpr ':' Stmt
		c99.LabeledStmtDefault:    // "default" ':' Stmt

		l, ok := cases[n]
		if !ok {
			todo("", g.position0(n))
		}
		g.w("\n_%d:", l)
		g.stmt(n.Stmt, cases, brk, cont, &f)
	case c99.LabeledStmtLabel: // IDENTIFIER ':' Stmt
		g.w("\n%s:\n", mangleIdent(n.Token.Val, false))
		g.stmt(n.Stmt, cases, brk, cont, &f)
	default:
		todo("", g.position0(n), n.Case)
	}
	*deadcode = false
}

func (g *gen) selectionStmt(n *c99.SelectionStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	switch n.Case {
	case c99.SelectionStmtSwitch: // "switch" '(' ExprList ')' Stmt
		if n.ExprList.Operand.Value != nil {
			todo("")
		}
		g.w("\nswitch ")
		switch el := n.ExprList; {
		case isSingleExpression(el):
			g.convert(n.ExprList.Expr, n.SwitchOp.Type)
		default:
			todo("", g.position0(n))
		}
		g.w("{")
		after := -g.local()
		cases := map[*c99.LabeledStmt]int{}
		var deflt *c99.LabeledStmt
		for _, v := range n.Cases {
			l := g.local()
			cases[v] = l
			switch ce := v.ConstExpr; {
			case ce != nil:
				g.w("\ncase ")
				g.value(ce.Expr)
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
		g.stmt(n.Stmt, cases, &after, cont, deadcode)
		if after > 0 {
			g.w("\n_%d:", after)
			*deadcode = false
		}
	case c99.SelectionStmtIf: // "if" '(' ExprList ')' Stmt
		switch op := n.ExprList.Operand; {
		case op.IsZero():
			for l := n.ExprList; l != nil; l = l.ExprList {
				g.void(l.Expr)
			}
			a := g.local()
			g.w("\ngoto _%d\n", a)
			t := true
			g.stmt(n.Stmt, cases, brk, cont, &t)
			g.w("\n_%d:", a)
			*deadcode = false
			return
		case op.IsNonzero():
			for l := n.ExprList; l != nil; l = l.ExprList {
				g.void(l.Expr)
			}
			g.stmt(n.Stmt, cases, brk, cont, deadcode)
			return
		}

		// if exprList == 0 { goto A }
		// stmt
		// A:
		a := g.local()
		g.w("\nif ")
		g.exprList(n.ExprList, false)
		g.w(" == 0 { goto _%d }\n", a)
		g.stmt(n.Stmt, cases, brk, cont, deadcode)
		g.w("\n_%d:", a)
		*deadcode = false
	case c99.SelectionStmtIfElse: // "if" '(' ExprList ')' Stmt "else" Stmt
		switch op := n.ExprList.Operand; {
		case op.IsZero():
			// goto A
			// stmt
			// goto B
			// A:
			// stmt2
			// B:
			a := g.local()
			b := g.local()
			g.exprList(n.ExprList, true)
			g.w("\ngoto _%d\n\n", a)
			t := true
			g.stmt(n.Stmt, cases, brk, cont, &t)
			g.w("\ngoto _%d\n", b)
			g.w("\n_%d:", a)
			g.stmt(n.Stmt2, cases, brk, cont, deadcode)
			g.w("\n_%d:", b)
			*deadcode = false
			return
		case op.IsNonzero():
			// exprList
			// stmt
			// goto A
			// stmt2
			// A:
			a := g.local()
			g.exprList(n.ExprList, true)
			g.stmt(n.Stmt, cases, brk, cont, deadcode)
			g.w("\ngoto _%d\n", a)
			t := true
			g.stmt(n.Stmt2, cases, brk, cont, &t)
			g.w("\n_%d:", a)
			*deadcode = false
			return
		}

		// if exprList == 0 { goto A }
		// stmt
		// goto B
		// A:
		// stmt2
		// B:
		a := g.local()
		b := g.local()
		g.w("\nif ")
		g.exprList(n.ExprList, false)
		g.w(" == 0 { goto _%d }\n", a)
		g.stmt(n.Stmt, cases, brk, cont, deadcode)
		g.w("\ngoto _%d\n", b)
		g.w("\n_%d:", a)
		f := false
		g.stmt(n.Stmt2, cases, brk, cont, &f)
		g.w("\n_%d:", b)
		*deadcode = false
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) iterationStmt(n *c99.IterationStmt, cases map[*c99.LabeledStmt]int, brk, cont *int, deadcode *bool) {
	switch n.Case {
	case c99.IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
		if op := n.ExprList.Operand; op.Value != nil {
			switch {
			case op.IsZero():
				// stmt
				// goto A
				// A:
				a := -g.local()
				t := true
				g.stmt(n.Stmt, cases, &a, cont, &t)
				if a > 0 {
					g.w("\ngoto _%d\n\n_%d:", a, a)
				}
				return
			case op.IsNonzero():
				// A:
				// stmt
				// goto A
				// B:
				a := g.local()
				b := -g.local()
				g.w("\n_%d:", a)
				g.stmt(n.Stmt, cases, &b, cont, deadcode)
				g.w("\ngoto _%d\n\n", a)
				if b > 0 {
					g.w("\n_%d:", b)
				}
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
		a := g.local()
		c := -g.local()
		b := -g.local()
		g.w("\n_%d:", a)
		g.stmt(n.Stmt, cases, &b, &c, deadcode)
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
		if n.ExprListOpt2 == nil {
			// ExprListOpt
			// A:
			// Stmt
			// B: <- continue
			// ExprListOpt3
			// goto A
			// C: <- break
			g.w("\n")
			g.exprListOpt(n.ExprListOpt, true)
			a := g.local()
			b := -g.local()
			c := -g.local()
			g.w("\n_%d:", a)
			g.stmt(n.Stmt, cases, &c, &b, deadcode)
			if n.ExprListOpt3 != nil {
				g.w("\n")
			}
			if b > 0 {
				g.w("\n_%d:", b)
			}
			g.exprListOpt(n.ExprListOpt3, true)
			g.w("\ngoto _%d\n", a)
			if c > 0 {
				g.w("\n_%d:", c)
			}
			return
		}

		if op := n.ExprListOpt2.ExprList.Operand; op.Value != nil {
			switch {
			case op.IsZero():
				// ExprListOpt
				// A:
				// ExprListOpt2
				// Stmt
				// B: <- continue
				// ExprListOpt3
				// goto A
				// C: <- break
				g.w("\n")
				g.exprListOpt(n.ExprListOpt, true)
				a := g.local()
				b := -g.local()
				c := -g.local()
				g.w("\n_%d:", a)
				g.w("if ")
				g.exprListOpt(n.ExprListOpt2, true)
				g.stmt(n.Stmt, cases, &c, &b, deadcode)
				if n.ExprListOpt3 != nil {
					g.w("\n")
				}
				if b > 0 {
					g.w("\n_%d:", b)
				}
				g.exprListOpt(n.ExprListOpt3, true)
				g.w("\ngoto _%d\n", a)
				if c > 0 {
					g.w("\n_%d:", c)
				}
				return
			case op.IsNonzero():
				todo("", g.position0(n))
			default:
				todo("", g.position0(n))
			}
		}

		// ExprListOpt
		// A:
		// if ExprListOpt2 == 0 { goto C }
		// Stmt
		// B: <- continue
		// ExprListOpt3
		// goto A
		// C: <- break
		g.w("\n")
		g.exprListOpt(n.ExprListOpt, true)
		a := g.local()
		b := -g.local()
		c := g.local()
		g.w("\n_%d:", a)
		g.w("if ")
		g.exprListOpt(n.ExprListOpt2, false)
		g.w(" == 0 { goto _%d }\n", c)
		g.stmt(n.Stmt, cases, &c, &b, deadcode)
		if n.ExprListOpt3 != nil {
			g.w("\n")
		}
		if b > 0 {
			g.w("\n_%d:", b)
		}
		g.exprListOpt(n.ExprListOpt3, true)
		g.w("\ngoto _%d\n", a)
		g.w("\n_%d:", c)
	case c99.IterationStmtWhile: // "while" '(' ExprList ')' Stmt
		if n.ExprList.Operand.IsZero() {
			todo("", g.position0(n))
		}

		if n.ExprList.Operand.IsNonzero() {
			a := g.local()
			g.w("\n_%d:", a)
			b := -g.local()
			g.exprList(n.ExprList, true)
			g.stmt(n.Stmt, cases, &b, &a, deadcode)
			g.w("\ngoto _%d\n", a)
			if b > 0 {
				g.w("\n_%d:", b)
			}
			return
		}

		// A:
		// if exprList == 0 { goto B }
		// stmt
		// goto A
		// B:
		a := g.local()
		b := g.local()
		g.w("\n_%d:\nif ", a)
		g.exprList(n.ExprList, false)
		g.w(" == 0 { goto _%d }\n", b)
		g.stmt(n.Stmt, cases, &b, &a, deadcode)
		g.w("\ngoto _%d\n\n_%d:", a, b)
	default:
		todo("", g.position0(n), n.Case)
	}
}

func (g *gen) local() int {
	r := g.nextLabel
	g.nextLabel++
	return r
}

func (g *gen) jumpStmt(n *c99.JumpStmt, brk, cont *int, deadcode *bool) {
	switch n.Case {
	case c99.JumpStmtReturn: // "return" ExprListOpt ';'
		g.w("\nreturn ")
		if o := n.ExprListOpt; o != nil {
			switch rt := n.ReturnOperand.Type; {
			case rt == nil:
				g.exprList(o.ExprList, false)
			default:
				g.exprList2(o.ExprList, rt)
			}
		}
		g.w("\n")
		*deadcode = true
	case c99.JumpStmtBreak: // "break" ';'
		if *brk < 0 {
			*brk = -*brk // Signal used.
		}
		g.w("\ngoto _%d\n", *brk)
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

func (g *gen) exprStmt(n *c99.ExprStmt) {
	if n.ExprListOpt != nil {
		g.w("\n")
		g.exprListOpt(n.ExprListOpt, true)
	}
}
