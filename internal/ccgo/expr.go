// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"math"
	"strconv"
	"strings"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
)

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
			g.value(n.Expr)
			break
		}

		todo("", g.position0(n))
	}
}

func (g *gen) exprList2(n *c99.ExprList, t c99.Type) {
	if isSingleExpression(n) {
		g.convert(n.Expr, t)
		return
	}

	todo("", g.position0(n))
}

func (g *gen) void(n *c99.Expr) {
	if g.voidCanIgnore(n) {
		return
	}

	switch n.Case {
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.value(n)
	case c99.ExprAssign: // Expr '=' Expr
		g.w("*")
		g.lvalue(n.Expr)
		g.w(" = ")
		if isVaList(n.Expr.Operand.Type) && n.Expr2.Case == c99.ExprInt && n.Expr2.Operand.IsZero() {
			g.w("nil")
			return
		}

		g.convert(n.Expr2, n.Expr.Operand.Type)
	case c99.ExprPostInc: // Expr "++"
		switch x := underlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			switch sz := g.model.Sizeof(x.Item); {
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
		switch x := underlyingType(n.Operand.Type).(type) {
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
		case underlyingType(n.Expr.Operand.Type).Kind() == c99.Ptr:
			g.w(" *(")
			g.lvalue(n.Expr)
			g.w(") += %d*uintptr(", g.model.Sizeof(n.Expr.Operand.Type.(*c99.PointerType).Item))
			g.value(n.Expr2)
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
				g.value(n.Expr)
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
		todo("", g.position0(n), n.Case, n.Operand) // void
	}
}

func (g *gen) lvalue(n *c99.Expr) {
	g.w("&")
	g.value(n)
}

func (g *gen) value(n *c99.Expr) {
	g.w("(")

	defer g.w(")")

	if n.Operand.Value != nil {
		g.constant(n)
		return
	}

	if a := n.Operand.Address; a != nil {
		if a == c99.Null {
			g.w(" 0")
			return
		}

		d := g.normalizeDeclarator(a.Declarator)
		g.enqueue(d)
		switch n.Case {
		case c99.ExprAddrof: // '&' Expr
			g.w("%s+%d", g.mangleDeclarator(d), a.Offset)
		case c99.ExprIdent: // IDENTIFIER
			switch {
			case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
				g.w(" *(*%s)(unsafe.Pointer(%s+%d))", g.typ(d.Type), g.mangleDeclarator(d), a.Offset)
			default:
				g.w("%s+%d", g.mangleDeclarator(d), a.Offset)
			}
		case c99.ExprSelect: // Expr '.' IDENTIFIER
			var f *c99.Declarator
			switch x := underlyingType(n.Expr.Operand.Type).(type) {
			case *c99.StructType:
				f = x.Field(n.Token2.Val)
			default:
				todo("%v: %T", g.position0(n), x)
			}
			switch {
			case underlyingType(f.Type).Kind() == c99.Array:
				g.w("%s+%d", g.mangleDeclarator(d), a.Offset)
			case g.escaped(d):
				g.w("*(*%s)(unsafe.Pointer(%s+%d))", g.typ(n.Operand.Type), g.mangleDeclarator(d), a.Offset)
			default:
				switch n.Expr.Case {
				case c99.ExprIdent: // IDENTIFIER
					g.w("%s.X%s", g.mangleDeclarator(d), n.Token2.S())
				default:
					g.w("*(*%s)(unsafe.Pointer(%s+%d))", g.typ(n.Operand.Type), g.mangleDeclarator(d), a.Offset)
				}
			}
		case c99.ExprPSelect: // Expr "->" IDENTIFIER
			g.w("*(*%s)(unsafe.Pointer(%s+%d))", g.typ(n.Operand.Type), g.mangleDeclarator(d), a.Offset)
		case c99.ExprIndex: // Expr '[' ExprList ']'
			g.w("*(*%s)(unsafe.Pointer(%s+%d))", g.typ(n.Operand.Type), g.mangleDeclarator(d), a.Offset)
		default:
			todo("", g.position0(n), n.Case, n.Operand) // value
		}
		return
	}

	switch n.Case {
	case
		c99.ExprEq, // Expr "==" Expr
		c99.ExprGe, // Expr ">=" Expr
		c99.ExprGt, // Expr ">" Expr
		c99.ExprLe, // Expr "<=" Expr
		c99.ExprLt, // Expr '<' Expr
		c99.ExprNe: // Expr "!=" Expr

		g.relop(n)
	case
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprDiv, // Expr '/' Expr
		c99.ExprMod, // Expr '%' Expr
		c99.ExprMul, // Expr '*' Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprXor: // Expr '^' Expr

		g.binop(n)
	case c99.ExprAdd: // Expr '+' Expr
		switch t, u := underlyingType(n.Expr.Operand.Type), underlyingType(n.Expr2.Operand.Type); {
		case t.Kind() == c99.Ptr:
			g.value(n.Expr)
			g.w(" + %d*uintptr(", g.model.Sizeof(t.(*c99.PointerType).Item))
			g.value(n.Expr2)
			g.w(")")
		case u.Kind() == c99.Ptr:
			todo("", g.position0(n))
		default:
			g.binop(n)
		}
	case c99.ExprSub: // Expr '-' Expr
		switch t, u := underlyingType(n.Expr.Operand.Type), underlyingType(n.Expr2.Operand.Type); {
		case t.Kind() == c99.Ptr && u.Kind() == c99.Ptr:
			g.w("%s(", g.typ(n.Operand.Type))
			g.value(n.Expr)
			g.w(" - ")
			g.value(n.Expr2)
			g.w("/%d)", g.model.Sizeof(t.(*c99.PointerType).Item))
		case t.Kind() == c99.Ptr:
			g.value(n.Expr)
			g.w(" - %d*uintptr(", g.model.Sizeof(t.(*c99.PointerType).Item))
			g.value(n.Expr2)
			g.w(")")
		default:
			g.binop(n)
		}
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.convert(n.Expr, underlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item)
		g.w("(tls")
		if o := n.ArgumentExprListOpt; o != nil {
			i := 0
			for l := o.ArgumentExprList; l != nil; l = l.ArgumentExprList {
				g.w(", ")
				g.convert(l.Expr, n.CallArgs[i].Type)
				i++
			}
		}
		g.w(")")
	case c99.ExprSelect: // Expr '.' IDENTIFIER
		t := underlyingType(n.Expr.Operand.Type)
		var d *c99.Declarator
		switch x := t.(type) {
		case *c99.StructType:
			d = x.Field(n.Token2.Val)
		default:
			todo("%v: %T", g.position0(n), x)
		}
		g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
		g.uintptr(n.Expr)
		g.w("+%d", g.model.Layout(t)[d.Field].Offset)
		g.w("))")
	case c99.ExprIndex: // Expr '[' ExprList ']'
		g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
		g.uintptr(n.Expr)
		g.w("+%d*uintptr(", g.model.Sizeof(n.Operand.Type))
		g.exprList(n.ExprList, false)
		g.w(")))")
	case c99.ExprDeref: // '*' Expr
		switch t := underlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item; t.Kind() {
		case c99.Function:
			g.value(n.Expr)
		default:
			i := 1
			for n.Expr.Case == c99.ExprDeref {
				i++
				n = n.Expr
			}
			g.w(" %[1]s(%[1]s%s)(unsafe.Pointer(", strings.Repeat("*", i), g.typ(t))
			g.value(n.Expr)
			g.w("))")
		}
	case c99.ExprPExprList: // '(' ExprList ')'
		g.exprList(n.ExprList, false)
	case c99.ExprAssign: // Expr '=' Expr
		g.assignmentValue(n)
	case c99.ExprLOr: // Expr "||" Expr
		g.needBool2int++
		g.w(" bool2int((")
		g.value(n.Expr)
		g.w(" != 0) || (")
		g.value(n.Expr2)
		g.w(" != 0))")
	case c99.ExprLAnd: // Expr "&&" Expr
		g.needBool2int++
		g.w(" bool2int((")
		g.value(n.Expr)
		g.w(" != 0) && (")
		g.value(n.Expr2)
		g.w(" != 0))")
	case c99.ExprPostInc: // Expr "++"
		switch x := underlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPostInc = true
			g.w(" postinc(")
			g.lvalue(n.Expr)
			g.w(", %d)", g.model.Sizeof(x.Item))
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Int,
				c99.ULong:

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
		switch x := underlyingType(n.Operand.Type).(type) {
		case c99.TypeKind:
			switch x {
			case
				c99.Int,
				c99.ULong:

				g.w(" preinc%d(", g.registerType(g.preIncTypes, n.Operand.Type))
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
		switch x := underlyingType(t).(type) {
		case *c99.PointerType:
			if x.Item.Kind() == c99.Function {
				t = x.Item
			}
		}
		g.w(" func() %s { if ", g.typ(t))
		g.convert(n.Expr, t0)
		g.w(" != 0 { return ")
		g.exprList2(n.ExprList, t0)
		g.w(" }\n\nreturn ")
		g.convert(n.Expr2, t0)
		g.w(" }()")
	case c99.ExprCast: // '(' TypeName ')' Expr
		if isVaList(n.Expr.Operand.Type) {
			todo("", g.position0(n))
			// 		t := n.TypeName.Type
			// 		t0 := t
			// 		for {
			// 			switch x := t.(type) {
			// 			case *c99.PointerType:
			// 				g.w(" %sVA%s(&", crt, g.typ(t))
			// 				g.rvalue(n.Expr, false)
			// 				g.w(")")
			// 				return
			// 			case *c99.NamedType:
			// 				t = x.Type
			// 			case c99.TypeKind:
			// 				if t0 != t {
			// 					g.w(" %s(", g.typ(t0))
			// 					defer g.w(")")
			// 				}
			// 				switch x {
			// 				case
			// 					c99.Int,
			// 					c99.LongLong,
			// 					c99.UInt:

			// 					g.w(" %sVA%s(&", crt, g.typ(t))
			// 					g.rvalue(n.Expr, false)
			// 					g.w(")")
			// 					return
			// 				default:
			// 					todo("%v: %v", g.position0(n), x)
			// 				}
			// 			default:
			// 				todo("%v: %T", g.position0(n), x)
			// 			}
			// 		}
		}

		g.convert(n.Expr, n.TypeName.Type)
	case c99.ExprNot: // '!' Expr
		g.needBool2int++
		g.w(" bool2int(")
		g.value(n.Expr)
		switch a := n.Expr.Operand.Address; {
		case underlyingType(n.Expr.Operand.Type).Kind() == c99.Ptr && a != nil && g.escaped(a.Declarator):
			g.w(" == nil)")
		default:
			g.w(" == 0)")
		}
	case c99.ExprPSelect: // Expr "->" IDENTIFIER
		g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
		switch x := underlyingType(underlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item).(type) {
		case *c99.StructType:
			g.uintptr(n.Expr)
			g.w("+%d", g.model.Layout(x)[x.Field(n.Token2.Val).Field].Offset)
		default:
			todo("%v: %T", g.position0(n), x)
		}
		g.w("))")
	default:
		todo("", g.position0(n), n.Case, n.Operand) // value
	}
}

func (g *gen) uintptr(n *c99.Expr) {
	g.w("(")

	defer g.w(")")

	if n.Operand.Value != nil {
		todo("", g.position0(n))
		return
	}

	if a := n.Operand.Address; a != nil {
		if a == c99.Null {
			g.w(" 0")
			return
		}

		d := g.normalizeDeclarator(a.Declarator)
		g.enqueue(d)
		switch n.Case {
		case c99.ExprIdent: // IDENTIFIER
			g.w("%s+%d", g.mangleDeclarator(d), a.Offset)
		case c99.ExprPSelect: // Expr "->" IDENTIFIER
			g.w("*(*uintptr)(unsafe.Pointer(%s+%d))", g.mangleDeclarator(d), a.Offset)
		default:
			todo("", g.position0(n), n.Case, n.Operand) // value
		}
		return
	}

	switch n.Case {
	case c99.ExprIndex: // Expr '[' ExprList ']'
		g.uintptr(n.Expr)
		g.w(" +%d*uintptr(", g.model.Sizeof(underlyingType(underlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item)))
		g.exprList(n.ExprList, false)
		g.w(")")

	default:
		todo("", g.position0(n), n.Case, n.Operand) // value
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
	g.convert(n.Expr, op.Type)
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
	g.convert(n.Expr2, op.Type)
	g.w(")")
}

func (g *gen) assignmentValue(n *c99.Expr) {
	g.w(" set%d(", g.registerType(g.assignTypes, n.Operand.Type))
	g.lvalue(n.Expr)
	g.w(", ")
	g.convert(n.Expr2, n.Operand.Type)
	g.w(")")
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

		g.convert(n.Expr, n.Operand.Type)
	default:
		g.convert(n.Expr, l)
	}
	g.w(" %s ", c99.TokSrc(n.Token))
	switch {
	case
		r.Kind() == c99.Ptr && n.Operand.Type.IsArithmeticType(),
		n.Operand.Type.Kind() == c99.Ptr && r.IsArithmeticType():

		g.convert(n.Expr2, n.Operand.Type)
	default:
		g.convert(n.Expr2, r)
	}
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
		g.value(n.Expr)
		g.w(" %s ", c99.TokSrc(n.Token))
		g.value(n.Expr2)
		g.w(")")
	default:
		g.convert(n.Expr, l)
		g.w(" %s ", c99.TokSrc(n.Token))
		g.convert(n.Expr2, r)
		g.w(")")
	}
}

func (g *gen) convert(n *c99.Expr, t c99.Type) {
	if a := n.Operand.Address; a != nil {
		if a == c99.Null {
			g.w(" 0")
			return
		}

		if d := g.normalizeDeclarator(a.Declarator); d.Type.Equal(t) {
			g.enqueue(d)
			switch n.Case {
			case c99.ExprIdent: // IDENTIFIER
				switch {
				case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
					g.w(" *(*%s)(unsafe.Pointer(%s+%d))", g.typ(d.Type), g.mangleDeclarator(d), a.Offset)
				default:
					g.w("(%s+%d)", g.mangleDeclarator(d), a.Offset)
				}
			case c99.ExprPExprList: // '(' ExprList ')'
				switch {
				case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
					g.w("/*TODO677 */ (func() %s { panic(677) }())", g.typ(t))
				default:
					if isSingleExpression(n.ExprList) {
						g.w("(%s+%d)", g.mangleDeclarator(d), a.Offset)
						break
					}

					g.w("/*TODO679 */ (func() %s { panic(679) }())", g.typ(t))
				}
			case c99.ExprPSelect: // Expr "->" IDENTIFIER
				switch {
				case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
					g.w("/*TODO689 */ (func() %s { panic(689) }())", g.typ(t))
				default:
					g.w("*(*%s)(unsafe.Pointer(%s+%d))", g.typ(n.Operand.Type), g.mangleDeclarator(d), a.Offset)
				}
			case c99.ExprCast: // '(' TypeName ')' Expr
				switch {
				case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
					g.w("/*TODO697 */ (func() %s { panic(697) }())", g.typ(t))
				default:
					g.w("(%s+%d)", g.mangleDeclarator(d), a.Offset)
				}
			case c99.ExprAdd: // Expr '+' Expr
				switch {
				case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
					g.w("/*TODO703 */ (func() %s { panic(703) }())", g.typ(t))
				default:
					g.w("(%s+%d)", g.mangleDeclarator(d), a.Offset)
				}
			case c99.ExprAddrof: // '&' Expr
				switch {
				case g.escaped(d) && underlyingType(d.Type).Kind() != c99.Array:
					g.w("%s+%d", g.mangleDeclarator(d), a.Offset)
				default:
					g.w("/*TODO712 */ (func() %s { panic(712) }())", g.typ(t))
				}
			default:
				g.w("/*TODO675 %v */ (func() %s { panic(%q) }())", n.Case, g.typ(t), n.Case.String())
			}
			return
		}
	}

	if t.Kind() == c99.Ptr {
		g.value(n)
		return
	}

	if n.Operand.Type.Equal(t) {
		switch {
		case n.Operand.Value != nil:
			g.w(" %s(", g.typ(t))
			g.constant(n)
			g.w(")")
		default:
			g.value(n)
		}
		return
	}

	if t.IsArithmeticType() {
		g.w(" %s(", g.typ(t))
		switch {
		case n.Operand.Value != nil:
			n.Operand.Type = t
			g.constant(n)
		default:
			g.value(n)
		}
		g.w(")")
		return
	}

	switch x := underlyingType(n.Operand.Type).(type) {
	case *c99.PointerType:
		if x.Item.Kind() == c99.Function {
			if x.Item.Equal(t) {
				g.value(n)
				return
			}

			todo("%v: %v -> %v", g.position0(n), x.Item, t)
		}
	default:
		todo("%v: %v -> %v", g.position0(n), n.Operand, t)
	}
}

func (g *gen) constant(n *c99.Expr) {
	switch x := n.Operand.Value.(type) {
	case *ir.Float32Value:
		switch u := underlyingType(n.Operand.Type).(type) {
		case c99.TypeKind:
			switch u {
			case c99.Float:
				g.w(" %v", x.Value)
				return
			default:
				todo("", g.position0(n), u)
			}
		default:
			todo("%v: %T", g.position0(n), u)
		}
	case *ir.Float64Value:
		switch u := underlyingType(n.Operand.Type).(type) {
		case c99.TypeKind:
			switch u {
			case
				c99.Char,
				c99.Double,
				c99.Int:

				switch {
				case x.Value == 0 && math.Copysign(1, x.Value) == -1:
					g.w(" nz64")
					g.needNZ64 = true
				default:
					g.w(" %v", x.Value)
				}
				return
			case c99.Float:
				switch {
				case x.Value == 0 && math.Copysign(1, x.Value) == -1:
					g.w(" nz32")
					g.needNZ32 = true
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
	case *ir.Int64Value:
		if n.Case == c99.ExprChar {
			g.w(" %s", strconv.QuoteRuneToASCII(rune(x.Value)))
			return
		}

		f := " %d"
		m := n
		s := ""
		for done := false; !done; { //TODO-
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
		case strings.HasPrefix(s, "0"):
			f = "%#o"
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
		g.w(f, v)
	case *ir.StringValue:
		g.w(" ts+%d+%d %s", g.allocString(int(x.StringID)), x.Offset, strComment(x))
	default:
		todo("%v: %T", g.position0(n), x)
	}
}
