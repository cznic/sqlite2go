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
			g.w(";")
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

	i := 0
	for l := n.ExprList; l != nil; l = l.ExprList {
		if !g.voidCanIgnore(l.Expr) {
			i++
		}
	}
	switch i {
	case 0:
		var e *c99.Expr
		for l := n.ExprList; l != nil; l = l.ExprList {
			e = l.Expr
		}
		g.convert(e, t)
	case 1:
		todo("", g.position0(n))
	default:
		todo("", g.position0(n))
	}
}

func (g *gen) void(n *c99.Expr) {
	if n.Case == c99.ExprPExprList && isSingleExpression(n.ExprList) {
		g.void(n.ExprList.Expr)
		return
	}

	if g.voidCanIgnore(n) {
		return
	}

	switch n.Case {
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		g.value(n)
	case c99.ExprAssign: // Expr '=' Expr
		op := n.Expr.Operand
		if n.Expr.Operand.Bits != 0 {
			g.w("set%d(", g.registerBitType(g.setBitsTypes, op.Type, op.PackedType))
			g.uintptr(n.Expr, true)
			g.w(", %#x, %d, ", (1<<uint(op.Bits)-1)<<uint(op.Bitoff), n.Expr.Operand.Bitoff)
			g.convert(n.Expr2, n.Expr.Operand.Type)
			g.w(")")
			return
		}

		g.w("*")
		g.lvalue(n.Expr)
		g.w(" = ")
		if isVaList(n.Expr.Operand.Type) && n.Expr2.Case == c99.ExprInt && n.Expr2.Operand.IsZero() {
			g.w("nil")
			return
		}

		g.convert(n.Expr2, n.Expr.Operand.Type)
	case
		c99.ExprPostInc, // Expr "++"
		c99.ExprPreInc:  // "++" Expr

		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			switch sz := g.model.Sizeof(x.Item); {
			case sz == 1:
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")++")
			default:
				g.value(n.Expr)
				g.w(" += %d", sz)
			}
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")++")
				return
			}
			todo("%v: %v", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case
		c99.ExprPostDec, // Expr "--"
		c99.ExprPreDec:  // "--" Expr

		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			switch sz := g.model.Sizeof(x.Item); {
			case sz == 1:
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")--")
			default:
				g.value(n.Expr)
				g.w(" -= %d", sz)
			}
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" *(")
				g.lvalue(n.Expr)
				g.w(")--")
				return
			}
			todo("%v: %v", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprAddAssign: // Expr "+=" Expr
		switch {
		case c99.UnderlyingType(n.Expr.Operand.Type).Kind() == c99.Ptr:
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
			g.w(" *(")
			g.lvalue(n.Expr)
			g.w(") -= %d*uintptr(", g.model.Sizeof(n.Expr.Operand.Type.(*c99.PointerType).Item))
			g.value(n.Expr2)
			g.w(")")
		default:
			g.voidArithmeticAsop(n)
		}
	case
		c99.ExprAndAssign, // Expr "&=" Expr
		c99.ExprDivAssign, // Expr "/=" Expr
		c99.ExprLshAssign, // Expr "<<=" Expr
		c99.ExprModAssign, // Expr "%=" Expr
		c99.ExprMulAssign, // Expr "*=" Expr
		c99.ExprOrAssign,  // Expr "|=" Expr
		c99.ExprRshAssign, // Expr ">>=" Expr
		c99.ExprXorAssign: // Expr "^=" Expr

		g.voidArithmeticAsop(n)
	case c99.ExprPExprList: // '(' ExprList ')'
		for l := n.ExprList; l != nil; l = l.ExprList {
			g.void(l.Expr)
			g.w(";")
		}
	case c99.ExprCast: // '(' TypeName ')' Expr
		if isVaList(n.Expr.Operand.Type) {
			g.w("%sVA%s(&", crt, g.typ(c99.UnderlyingType(n.TypeName.Type)))
			g.value(n.Expr)
			g.w(")")
			return
		}

		g.void(n.Expr)
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		switch {
		case n.Expr.Operand.IsZero():
			todo("", g.position0(n))
		case n.Expr.Operand.IsNonzero():
			todo("", g.position0(n))
		default:
			switch {
			case g.voidCanIgnoreExprList(n.ExprList):
				switch {
				case g.voidCanIgnore(n.Expr2):
					todo("", g.position0(n))
				default:
					// if expr == 0 {
					//	expr2
					// }
					g.w("if ")
					g.value(n.Expr)
					g.w(" == 0 {")
					g.void(n.Expr2)
					g.w("}")
				}
			default:
				switch {
				case g.voidCanIgnore(n.Expr2):
					todo("", g.position0(n))
				default:
					// if expr != 0 {
					//	exprList
					// } else {
					//	expr2
					// }
					g.w("if ")
					g.value(n.Expr)
					g.w(" != 0 {")
					g.exprList(n.ExprList, true)
					g.w("} else {")
					g.void(n.Expr2)
					g.w("}")
				}
			}
			//TODO- ok := true
			//TODO- for l := n.ExprList; l != nil; l = l.ExprList {
			//TODO- 	if !g.voidCanIgnore(l.Expr) {
			//TODO- 		ok = false
			//TODO- 		break
			//TODO- 	}
			//TODO- }
			//TODO- switch {
			//TODO- case ok:
			//TODO- 	if g.voidCanIgnore(n.Expr2) {
			//TODO- 		return
			//TODO- 	}

			//TODO- 	// if expr == 0 {
			//TODO- 	//	expr2
			//TODO- 	// }
			//TODO- 	g.w("if ")
			//TODO- 	g.value(n.Expr)
			//TODO- 	g.w(" == 0 {")
			//TODO- 	g.void(n.Expr2)
			//TODO- 	g.w("}")
			//TODO- case g.voidCanIgnore(n.Expr2):
			//TODO- 	todo("", g.position0(n))
			//TODO- default:
			//TODO- 	todo("", g.position0(n))
			//TODO- }
		}
	case c99.ExprDeref: // '*' Expr
		g.w("_ = ")
		g.value(n)
	case
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprLe:  // Expr "<=" Expr

		if g.voidCanIgnore(n.Expr) {
			g.void(n.Expr2)
			return
		}

		if g.voidCanIgnore(n.Expr2) {
			g.void(n.Expr)
			return
		}

		g.w("_ = ")
		g.value(n)
	case c99.ExprLAnd: // Expr "&&" Expr
		if g.voidCanIgnore(n.Expr) {
			if !n.Expr.Operand.IsZero() {
				g.void(n.Expr2)
			}
			return
		}

		if g.voidCanIgnore(n.Expr2) {
			g.void(n.Expr)
			return
		}

		g.w("_ = ")
		g.value(n)
	case c99.ExprLOr: // Expr "||" Expr
		if g.voidCanIgnore(n.Expr) {
			if !n.Expr.Operand.IsNonzero() {
				g.void(n.Expr2)
			}
			return
		}

		if g.voidCanIgnore(n.Expr2) {
			g.void(n.Expr)
			return
		}

		g.w("_ = ")
		g.value(n)
	default:
		todo("", g.position0(n), n.Case, n.Operand) // void
	}
}

func (g *gen) lvalue(n *c99.Expr) {
	g.w("&")
	g.value(n)
}

func (g *gen) value(n *c99.Expr) {
	if n.Case == c99.ExprPExprList && isSingleExpression(n.ExprList) {
		g.value(n.ExprList.Expr)
		return
	}

	g.w("(")

	defer g.w(")")

	if n.Operand.Value != nil && g.voidCanIgnore(n) {
		g.constant(n)
		return
	}

	if n.Operand.Address == c99.Null {
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			if x.Item.Kind() == c99.Function {
				g.w("nil")
				return
			}
		}

		g.w("uintptr(0)")
		return
	}

	switch n.Case {
	case c99.ExprIdent: // IDENTIFIER
		d := g.normalizeDeclarator(n.Operand.Address.Declarator)
		g.enqueue(d)
		switch {
		case g.escaped(d) && c99.UnderlyingType(d.Type).Kind() != c99.Array:
			g.w(" *(*%s)(unsafe.Pointer(%s))", g.typ(d.Type), g.mangleDeclarator(d))
		default:
			g.w("%s", g.mangleDeclarator(d))
		}
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
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		if a := n.Expr.Operand.Address; a != nil && a.Declarator != nil && a.Declarator.Name() == idAlloca {
			g.w("alloca(&allocs, int(")
			if n.ArgumentExprListOpt.ArgumentExprList.ArgumentExprList != nil {
				todo("", g.position0(n))
			}
			g.value(n.ArgumentExprListOpt.ArgumentExprList.Expr)
			g.w("))")
			return
		}

		ft := c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item.(*c99.FunctionType)
		g.convert(n.Expr, ft)
		g.w("(tls")
		if o := n.ArgumentExprListOpt; o != nil {
			i := 0
			for l := o.ArgumentExprList; l != nil; l = l.ArgumentExprList {
				switch {
				case i < len(ft.Params) || ft.Variadic:
					g.w(", ")
					g.convert(l.Expr, n.CallArgs[i].Type)
				default:
					if !g.voidCanIgnore(l.Expr) {
						todo("", g.position0(l.Expr))
					}
				}
				i++
			}
		}
		g.w(")")
	case c99.ExprAddrof: // '&' Expr
		g.uintptr(n.Expr, false)
	case c99.ExprSelect: // Expr '.' IDENTIFIER
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.StructType:
			d := x.Field(n.Token2.Val)
			fp := g.model.Layout(x)[d.Field]
			switch {
			case d.Type.Kind() == c99.Array:
				g.uintptr(n.Expr, false)
				g.w("+%d", fp.Offset)
			default:
				switch {
				case fp.Bits != 0:
					g.w("(*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
					g.uintptr(n.Expr, true)
					g.w("+%d", fp.Offset)
					switch {
					case n.Operand.Type.IsUnsigned():
						g.w("))&%#x)>>%v", fp.Mask(), fp.Bitoff)
					default:
						todo("", g.position0(n))
					}
				default:
					g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
					g.uintptr(n.Expr, false)
					g.w("+%d", fp.Offset)
					g.w("))")
				}
			}
		case *c99.UnionType:
			d := x.Field(n.Token2.Val)
			layout := g.model.Layout(x)
			if bits := layout[d.Field].Bits; bits != 0 {
				todo("", g.position0(n), n.Operand)
			}
			switch {
			case d.Type.Kind() == c99.Array:
				g.uintptr(n.Expr, false)
			default:
				g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
				g.uintptr(n.Expr, false)
				g.w("))")
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPSelect: // Expr "->" IDENTIFIER
		switch x := c99.UnderlyingType(c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item).(type) {
		case *c99.StructType:
			layout := g.model.Layout(x)
			d := x.Field(n.Token2.Val)
			fp := layout[d.Field]
			switch {
			case d.Type.Kind() == c99.Array:
				g.value(n.Expr)
				g.w("+%d", g.model.Layout(x)[d.Field].Offset)
			default:
				switch {
				case fp.Bits != 0:
					g.w("(*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
					g.value(n.Expr)
					g.w("+%d", fp.Offset)
					switch {
					case n.Operand.Type.IsUnsigned():
						g.w("))&%#x)>>%v", fp.Mask(), fp.Bitoff)
					default:
						todo("", g.position0(n))
					}
				default:
					g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
					g.value(n.Expr)
					g.w("+%d))", g.model.Layout(x)[d.Field].Offset)
				}
			}
		case *c99.UnionType:
			layout := g.model.Layout(x)
			d := x.Field(n.Token2.Val)
			fp := layout[d.Field]
			switch {
			case d.Type.Kind() == c99.Array:
				todo("", g.position0(n))
			default:
				switch {
				case fp.Bits != 0:
					todo("", g.position0(n))
				default:
					g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
					g.value(n.Expr)
					g.w("))")
				}
			}
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprIndex: // Expr '[' ExprList ']'
		var it c99.Type
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.ArrayType:
			it = x.Item
		case *c99.PointerType:
			it = x.Item
		default:
			todo("%v: %T", g.position0(n), x)
		}
		switch {
		case it.Kind() == c99.Array:
			g.value(n.Expr)
			g.indexOff(n.ExprList, it)
		default:
			g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type))
			g.value(n.Expr)
			g.indexOff(n.ExprList, it)
			g.w("))")
		}
	case c99.ExprAdd: // Expr '+' Expr
		switch t, u := c99.UnderlyingType(n.Expr.Operand.Type), c99.UnderlyingType(n.Expr2.Operand.Type); {
		case t.Kind() == c99.Ptr:
			g.value(n.Expr)
			g.w(" + %d*uintptr(", g.model.Sizeof(t.(*c99.PointerType).Item))
			g.value(n.Expr2)
			g.w(")")
		case u.Kind() == c99.Ptr:
			g.w("%d*uintptr(", g.model.Sizeof(u.(*c99.PointerType).Item))
			g.value(n.Expr)
			g.w(") + ")
			g.value(n.Expr2)
		default:
			g.binop(n)
		}
	case c99.ExprSub: // Expr '-' Expr
		switch t, u := c99.UnderlyingType(n.Expr.Operand.Type), c99.UnderlyingType(n.Expr2.Operand.Type); {
		case t.Kind() == c99.Ptr && u.Kind() == c99.Ptr:
			g.w("%s((", g.typ(n.Operand.Type))
			g.value(n.Expr)
			g.w(" - ")
			g.value(n.Expr2)
			g.w(")/%d)", g.model.Sizeof(t.(*c99.PointerType).Item))
		case t.Kind() == c99.Ptr:
			g.value(n.Expr)
			g.w(" - %d*uintptr(", g.model.Sizeof(t.(*c99.PointerType).Item))
			g.value(n.Expr2)
			g.w(")")
		default:
			g.binop(n)
		}
	case c99.ExprDeref: // '*' Expr
		it := c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item
		switch it.Kind() {
		case c99.Function:
			g.value(n.Expr)
		case c99.Array:
			g.value(n.Expr)
		default:
			i := 1
			for n.Expr.Case == c99.ExprDeref {
				i++
				n = n.Expr
			}
			g.w("%[1]s(%[1]s%[2]s)(unsafe.Pointer(", strings.Repeat("*", i), g.typ(it))
			g.value(n.Expr)
			g.w("))")
		}
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
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		t := n.Operand.Type
		t0 := t
		switch x := c99.UnderlyingType(t).(type) {
		case *c99.PointerType:
			if x.Item.Kind() == c99.Function {
				t = x.Item
			}
		}
		g.w(" func() %s { if ", g.typ(t))
		g.value(n.Expr)
		g.w(" != 0 { return ")
		g.exprList2(n.ExprList, t0)
		g.w(" }\n\nreturn ")
		g.convert(n.Expr2, t0)
		g.w(" }()")
	case c99.ExprCast: // '(' TypeName ')' Expr
		t := n.TypeName.Type
		op := n.Expr.Operand
		if isVaList(op.Type) {
			g.w("%sVA%s(&", crt, g.typ(c99.UnderlyingType(t)))
			g.value(n.Expr)
			g.w(")")
			return
		}

		switch x := c99.UnderlyingType(t).(type) {
		case *c99.PointerType:
			if x.Item.Kind() == c99.Function && op.Address != nil && op.Address.Declarator != nil && g.normalizeDeclarator(op.Address.Declarator).Type.Equal(x.Item) {
				g.value(n.Expr)
				return
			}
		}
		g.convert(n.Expr, t)
	case c99.ExprPreInc: // "++" Expr
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPreInc = true
			g.w(" preinc(")
			g.lvalue(n.Expr)
			g.w(", %d)", g.model.Sizeof(x.Item))
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" preinc%d(", g.registerType(g.preIncTypes, n.Operand.Type))
				g.lvalue(n.Expr)
				g.w(")")
				return
			}

			todo("%v: %v", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPostInc: // Expr "++"
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPostInc = true
			g.w(" postinc(")
			g.lvalue(n.Expr)
			g.w(", %d)", g.model.Sizeof(x.Item))
		case c99.TypeKind:
			if n.Expr.Operand.Bits != 0 {
				switch {
				case n.Operand.Type.IsUnsigned():
					g.w(" postincu%d(", g.registerBitType(g.postIncUBitsTypes, n.Operand.Type, n.Operand.PackedType))
					g.uintptr(n.Expr, true)
					g.w(", %d, %d)", n.Expr.Operand.Bits, n.Expr.Operand.Bitoff)
				default:
					todo("", g.position0(n))
				}
				return
			}

			if x.IsArithmeticType() {
				g.w(" postinc%d(", g.registerType(g.postIncTypes, x))
				g.lvalue(n.Expr)
				g.w(")")
				return
			}

			todo("%v: %v", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPreDec: // "--" Expr
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPreDec = true
			g.w(" predec(")
			g.lvalue(n.Expr)
			g.w(", %d)", g.model.Sizeof(x.Item))
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" predec%d(", g.registerType(g.preDecTypes, n.Operand.Type))
				g.lvalue(n.Expr)
				g.w(")")
				return
			}
			todo("%v: %v", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPostDec: // Expr "--"
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPostDec = true
			g.w(" postdec(")
			g.lvalue(n.Expr)
			g.w(", %d)", g.model.Sizeof(x.Item))
		case c99.TypeKind:
			if n.Expr.Operand.Bits != 0 {
				switch {
				case n.Operand.Type.IsUnsigned():
					g.w(" postdecu%d(", g.registerBitType(g.postDecUBitsTypes, n.Operand.Type, n.Operand.PackedType))
					g.uintptr(n.Expr, true)
					g.w(", %d, %d)", n.Expr.Operand.Bits, n.Expr.Operand.Bitoff)
				default:
					todo("", g.position0(n))
				}
				return
			}

			if x.IsArithmeticType() {
				g.w(" postdec%d(", g.registerType(g.postDecTypes, x))
				g.lvalue(n.Expr)
				g.w(")")
				return
			}
			todo("%v: %v", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprNot: // '!' Expr
		g.needBool2int++
		g.w(" bool2int(")
		g.value(n.Expr)
		g.w(" == 0)")
	case c99.ExprLsh: // Expr "<<" Expr
		if n.Expr.Operand.Bits != 0 {
			g.w("(")
			g.convert(n.Expr, n.Operand.Type)
			g.w(" << uint(")
			g.value(n.Expr2)
			g.w(")&%#x)", uint64(1)<<uint(n.Expr.Operand.Bits)-1)
			return
		}

		g.convert(n.Expr, n.Operand.Type)
		g.w(" << uint(")
		g.value(n.Expr2)
		g.w(")")
	case c99.ExprRsh: // Expr ">>" Expr
		g.convert(n.Expr, n.Operand.Type)
		g.w(" >> uint(")
		g.value(n.Expr2)
		g.w(")")
	case c99.ExprUnaryMinus: // '-' Expr
		g.w("- ")
		g.convert(n.Expr, n.Operand.Type)
	case c99.ExprCpl: // '~' Expr
		g.w("^ ")
		g.value(n.Expr)
	case c99.ExprAddAssign: // Expr "+=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPreInc = true
			g.w("preinc(")
			g.lvalue(n.Expr)
			g.w(", %d*uintptr(", g.model.Sizeof(x.Item))
			g.value(n.Expr2)
			g.w("))")
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" add%d(", g.registerType(g.addTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.convert(n.Expr2, x)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprSubAssign: // Expr "-=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" sub%d(", g.registerType(g.subTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.convert(n.Expr2, x)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprXorAssign: // Expr "^=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" xor%d(", g.registerType(g.xorTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.convert(n.Expr2, n.Operand.Type)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPExprList: // '(' ExprList ')'
		var e *c99.Expr
		i := 0
		for l := n.ExprList; l != nil; l = l.ExprList {
			if !g.voidCanIgnore(l.Expr) {
				e = l.Expr
				i++
			}
		}
		_ = e //TODO-
		switch i {
		//TODO string-opt-5.c
		//TODO case 0:
		//TODO 	panic("internal error")
		//TODO case 1:
		//TODO 	g.value(e)
		default:
			g.w("func() %v {", g.typ(n.Operand.Type))
			for l := n.ExprList; l != nil; l = l.ExprList {
				switch {
				case l.ExprList == nil:
					g.w("return ")
					g.convert(l.Expr, n.Operand.Type)
				default:
					g.void(l.Expr)
				}
				g.w(";")
			}
			g.w("}()")
		}
	case c99.ExprMulAssign: // Expr "*=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" mul%d(", g.registerType(g.mulTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.convert(n.Expr2, n.Operand.Type)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprDivAssign: // Expr "/=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" div%d(", g.registerType(g.divTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.convert(n.Expr2, n.Operand.Type)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprModAssign: // Expr "%=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" mod%d(", g.registerType(g.modTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.convert(n.Expr2, n.Operand.Type)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprRshAssign: // Expr ">>=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsArithmeticType() {
				g.w(" rsh%d(", g.registerType(g.rshTypes, x))
				g.lvalue(n.Expr)
				g.w(", ")
				g.value(n.Expr2)
				g.w(")")
				return
			}
			todo("", g.position0(n), x)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	default:
		todo("", g.position0(n), n.Case, n.Operand) // value
	}
}

func (g *gen) indexOff(n *c99.ExprList, it c99.Type) {
	switch {
	case n.Operand.Value != nil && g.voidCanIgnoreExprList(n):
		g.w("%+d", g.model.Sizeof(it)*n.Operand.Value.(*ir.Int64Value).Value)
	default:
		g.w(" + %d*uintptr(", g.model.Sizeof(it))
		g.exprList(n, false)
		g.w(")")
	}
}

func (g *gen) uintptr(n *c99.Expr, ignoreBits bool) {
	if n.Case == c99.ExprPExprList && isSingleExpression(n.ExprList) {
		g.uintptr(n.ExprList.Expr, ignoreBits)
		return
	}

	g.w("(")

	defer g.w(")")

	if a := n.Operand.Address; a != nil {
		if a == c99.Null {
			g.w("uintptr(0)")
			return
		}

		d := g.normalizeDeclarator(a.Declarator)
		g.enqueue(d)
		arr := c99.UnderlyingType(d.Type).Kind() == c99.Array
		switch n.Case {
		case c99.ExprIdent: // IDENTIFIER
			switch {
			case arr:
				g.w("%s", g.mangleDeclarator(d))
			case g.escaped(d):
				g.w("%s", g.mangleDeclarator(d))
			default:
				g.w("uintptr(unsafe.Pointer(&%s))", g.mangleDeclarator(d))
			}
			return
		}
	}

	switch n.Case {
	case c99.ExprIndex: // Expr '[' ExprList ']'
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.ArrayType:
			g.uintptr(n.Expr, false)
			g.indexOff(n.ExprList, x.Item)
		case *c99.PointerType:
			g.value(n.Expr)
			g.indexOff(n.ExprList, x.Item)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprSelect: // Expr '.' IDENTIFIER
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.StructType:
			f := x.Field(n.Token2.Val)
			layout := g.model.Layout(x)
			if bits := layout[f.Field].Bits; bits != 0 && !ignoreBits {
				todo("", g.position0(n), n.Operand)
			}
			g.uintptr(n.Expr, ignoreBits)
			g.w("+%d", layout[f.Field].Offset)
		case *c99.UnionType:
			f := x.Field(n.Token2.Val)
			layout := g.model.Layout(x)
			if bits := layout[f.Field].Bits; bits != 0 && !ignoreBits {
				todo("", g.position0(n), n.Operand)
			}
			g.uintptr(n.Expr, ignoreBits)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprPSelect: // Expr "->" IDENTIFIER
		switch x := c99.UnderlyingType(c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item).(type) {
		case *c99.StructType:
			layout := g.model.Layout(x)
			f := x.Field(n.Token2.Val)
			if bits := layout[f.Field].Bits; bits != 0 && !ignoreBits {
				todo("", g.position0(n), n.Operand)
			}
			g.value(n.Expr)
			g.w("+%d", g.model.Layout(x)[f.Field].Offset)
		case *c99.UnionType:
			layout := g.model.Layout(x)
			f := x.Field(n.Token2.Val)
			if bits := layout[f.Field].Bits; bits != 0 && !ignoreBits {
				todo("", g.position0(n), n.Operand)
			}
			g.value(n.Expr)
		default:
			todo("%v: %T", g.position0(n), x)
		}
	case c99.ExprDeref: // '*' Expr
		switch c99.UnderlyingType(c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item).(type) {
		case *c99.ArrayType:
			g.value(n.Expr)
		default:
			g.value(n.Expr)
		}
	default:
		todo("", g.position0(n), n.Case, n.Operand) // uintptr
	}
}

func (g *gen) voidCanIgnore(n *c99.Expr) bool {
	if n.Case == c99.ExprPExprList && isSingleExpression(n.ExprList) {
		return g.voidCanIgnore(n.ExprList.Expr)
	}

	switch n.Case {
	case
		c99.ExprChar,       // CHARCONST
		c99.ExprFloat,      // FLOATCONST
		c99.ExprIdent,      // IDENTIFIER
		c99.ExprInt,        // INTCONST
		c99.ExprSizeofExpr, // "sizeof" Expr
		c99.ExprSizeofType, // "sizeof" '(' TypeName ')'
		c99.ExprString:     // STRINGLITERAL

		return true
	case c99.ExprPExprList: // '(' ExprList ')'
		return g.voidCanIgnoreExprList(n.ExprList)
	case
		c99.ExprAddAssign, // Expr "+=" Expr
		c99.ExprAndAssign, // Expr "&=" Expr
		c99.ExprAssign,    // Expr '=' Expr
		c99.ExprCall,      // Expr '(' ArgumentExprListOpt ')'
		c99.ExprDeref,     // '*' Expr
		c99.ExprDivAssign, // Expr "/=" Expr
		c99.ExprLshAssign, // Expr "<<=" Expr
		c99.ExprModAssign, // Expr "%=" Expr
		c99.ExprMulAssign, // Expr "*=" Expr
		c99.ExprOrAssign,  // Expr "|=" Expr
		c99.ExprPostDec,   // Expr "--"
		c99.ExprPostInc,   // Expr "++"
		c99.ExprPreDec,    // "--" Expr
		c99.ExprPreInc,    // "++" Expr
		c99.ExprRshAssign, // Expr ">>=" Expr
		c99.ExprSubAssign, // Expr "-=" Expr
		c99.ExprXorAssign: // Expr "^=" Expr

		return false
	case c99.ExprCast: // '(' TypeName ')' Expr
		return !isVaList(n.Expr.Operand.Type) && g.voidCanIgnore(n.Expr)
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
		c99.ExprAdd, // Expr '+' Expr
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprDiv, // Expr '/' Expr
		c99.ExprEq,  // Expr "==" Expr
		c99.ExprGe,  // Expr ">=" Expr
		c99.ExprGt,  // Expr ">" Expr
		c99.ExprLe,  // Expr "<=" Expr
		c99.ExprLsh, // Expr "<<" Expr
		c99.ExprLt,  // Expr '<' Expr
		c99.ExprMul, // Expr '*' Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprNe,  // Expr "!=" Expr
		c99.ExprRsh, // Expr ">>" Expr
		c99.ExprSub: // Expr '-' Expr

		return g.voidCanIgnore(n.Expr) && g.voidCanIgnore(n.Expr2)
	case c99.ExprLAnd: // Expr "&&" Expr
		return n.Operand.Value != nil && g.voidCanIgnore(n.Expr)
	case c99.ExprLOr: // Expr "||" Expr
		return n.Operand.Value != nil && g.voidCanIgnore(n.Expr)
	case
		c99.ExprAddrof,     // '&' Expr
		c99.ExprCpl,        // '~' Expr
		c99.ExprNot,        // '!' Expr
		c99.ExprSelect,     // Expr '.' IDENTIFIER
		c99.ExprUnaryMinus, // '-' Expr
		c99.ExprUnaryPlus:  // '+' Expr

		return g.voidCanIgnore(n.Expr)
	case c99.ExprIndex: // Expr '[' ExprList ']'
		return g.voidCanIgnore(n.Expr) && g.voidCanIgnoreExprList(n.ExprList)
	default:
		todo("", g.position0(n), n.Case, n.Operand) // voidCanIgnore
	}
	panic("unreachable")
}

func (g *gen) voidCanIgnoreExprList(n *c99.ExprList) bool {
	if isSingleExpression(n) {
		return g.voidCanIgnore(n.Expr)
	}

	for l := n; l != nil; l = l.ExprList {
		if !g.voidCanIgnore(l.Expr) {
			return false
		}
	}

	return true
}

func (g *gen) constant(n *c99.Expr) {
	if !g.voidCanIgnore(n) {
		todo("", g.position0(n), n.Case)
	}

	switch x := n.Operand.Value.(type) {
	case *ir.Float32Value:
		switch u := c99.UnderlyingType(n.Operand.Type).(type) {
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
		switch u := c99.UnderlyingType(n.Operand.Type).(type) {
		case c99.TypeKind:
			switch u {
			case
				c99.Char,
				c99.Double,
				c99.Int,
				c99.LongDouble:

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
		g.w(" ts+%d %s", g.allocString(int(x.StringID)), strComment(x))
	default:
		todo("%v: %T", g.position0(n), x)
	}
}

func (g *gen) voidArithmeticAsop(n *c99.Expr) {
	op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
	switch {
	case n.Expr.Operand.Address != nil:
		g.w(" *(")
		g.lvalue(n.Expr)
		g.w(") = %s(", g.typ(n.Expr.Operand.Type))
		g.convert(n.Expr, op.Type)
	default:
		g.w("{ p := ")
		g.lvalue(n.Expr)
		g.w("; *p = %s(%s(*p)", g.typ(n.Expr.Operand.Type), g.typ(op.Type))
	}
	switch n.Token.Rune {
	case c99.ANDASSIGN:
		g.w("&")
	case c99.ADDASSIGN:
		g.w("+")
	case c99.SUBASSIGN:
		g.w("-")
	case c99.MULASSIGN:
		g.w("*")
	case c99.DIVASSIGN:
		g.w("/")
	case c99.ORASSIGN:
		g.w("|")
	case c99.RSHASSIGN:
		g.w(">>")
		op.Type = c99.UInt
	case c99.XORASSIGN:
		g.w("^")
	case c99.MODASSIGN:
		g.w(`%`)
	case c99.LSHASSIGN:
		g.w("<<")
	default:
		todo("", g.position0(n), c99.TokSrc(n.Token))
	}
	g.convert(n.Expr2, op.Type)
	switch {
	case n.Expr.Operand.Address != nil:
		g.w(")")
	default:
		g.w(")}")
	}
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
	case n.Expr.Operand.Type.Kind() == c99.Ptr || n.Expr2.Operand.Type.Kind() == c99.Ptr:
		g.fpValue(n.Expr)
		g.w(" %s ", c99.TokSrc(n.Token))
		g.fpValue(n.Expr2)
		g.w(")")
	default:
		g.convert(n.Expr, l)
		g.w(" %s ", c99.TokSrc(n.Token))
		g.convert(n.Expr2, r)
		g.w(")")
	}
}

func (g *gen) fpValue(n *c99.Expr) {
	switch x := c99.UnderlyingType(n.Operand.Type).(type) {
	case *c99.PointerType:
		if x.Item.Kind() == c99.Function {
			// (*(*uintptr)(unsafe.Pointer(&struct{ f from }{expr})))
			g.w("(*(*uintptr)(unsafe.Pointer(&struct{ f %s }{", g.typ(x.Item))
			g.value(n)
			g.w("})))")
			return
		}
	}
	g.value(n)
}

func (g *gen) convert(n *c99.Expr, t c99.Type) {
	if n.Case == c99.ExprPExprList && isSingleExpression(n.ExprList) {
		g.convert(n.ExprList.Expr, t)
		return
	}

	switch x := c99.UnderlyingType(n.Operand.Type).(type) {
	case *c99.PointerType:
		if x.Item.Kind() == c99.Function {
			if x.Item.Equal(t) || n.Operand.Type.Equal(t) {
				g.value(n)
				return
			}

			if n.Operand.IsZero() && g.voidCanIgnore(n.Expr) {
				g.w("nil")
				return
			}

			// (*(*to)(unsafe.Pointer(&struct{ f from }{expr})))
			g.w("(*(*%s)(unsafe.Pointer(&struct{ f %s }{", g.ptyp(t, false), g.typ(x.Item))
			g.value(n)
			g.w("})))")
			return
		}
	}

	if t.Kind() == c99.Ptr {
		switch {
		case n.Operand.Type.IsIntegerType():
			g.w(" uintptr(")
			g.value(n)
			g.w(")")
		default:
			g.value(n)
		}
		return
	}

	if n.Operand.Type.Equal(t) {
		switch {
		case n.Operand.Value != nil && g.voidCanIgnore(n):
			g.w(" %s(", g.typ(t))
			g.constant(n)
			g.w(")")
		default:
			g.value(n)
		}
		return
	}

	if c99.UnderlyingType(t).IsArithmeticType() {
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

	todo("%v: %v -> %v, %T, %v", g.position0(n), n.Operand, t, t, c99.UnderlyingType(t))
}
