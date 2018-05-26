// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"fmt"
	"math"
	"strings"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
	"go/ast"
	"go/token"
)

func (g *gen) exprListOpt(n *c99.ExprListOpt) []ast.Stmt {
	if n == nil {
		return nil
	}
	return g.exprListVoid(n.ExprList)
}

func (g *gen) exprListVoid(n *c99.ExprList) []ast.Stmt {
	if n == nil {
		panic("should not be nil")
	}
	var out []ast.Stmt
	for _, v := range g.pexprList(n) {
		out = append(out, g.void(v)...)
	}
	return out
}

func (g *gen) exprList(n *c99.ExprList) ast.Expr {
	last, stmts := g.pexprListSplit(n)
	return runAndReturn(
		g.value(last, false),
		g.typI(n.Operand.Type),
		stmts...,
	)
}

func (g *gen) exprListConv(n *c99.ExprList, t c99.Type) ast.Expr {
	last, stmts := g.pexprListSplit(n)
	return runAndReturn(
		g.convert(last, t),
		g.typI(t),
		stmts...,
	)
}

func (g *gen) void(n *c99.Expr) (out []ast.Stmt) {
	if n.Case == c99.ExprCast && n.Expr.Case == c99.ExprIdent && !isVaList(n.Expr.Operand.Type) {
		g.enqueue(n.Expr.Declarator)
		return nil
	}

	if g.voidCanIgnore(n) {
		return nil
	}

	switch n.Case {
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		return []ast.Stmt{exprStmt(g.value(n, false))}
	case c99.ExprAssign: // Expr '=' Expr
		if n.Expr.Equals(n.Expr2) {
			return nil
		}

		op := n.Expr.Operand
		if op.Bits() != 0 {
			return []ast.Stmt{exprStmt(g.assignmentValue(n))}
		}

		if isVaList(n.Expr.Operand.Type) {
			switch rhs := n.Expr2.Operand; {
			case isVaList(rhs.Type): // va_copy
				tmp := ident("x")
				return []ast.Stmt{block(
					define(tmp, ptrVal(g.value(n.Expr2, false))),
					setPtr(g.lvalue(n.Expr), takeAddr(tmp)),
				)}
			case n.Expr2.Declarator != nil && n.Expr2.Declarator.Name() == idVaStart:
				tmp := ident("x")
				return []ast.Stmt{block(
					define(tmp, ident("ap")),
					setPtr(g.lvalue(n.Expr), takeAddr(tmp)),
				)}
			case n.Expr2.Declarator != nil && n.Expr2.Declarator.Name() == idVaEnd:
				return []ast.Stmt{
					setPtr(g.lvalue(n.Expr), nil),
				}
			}
			panic(fmt.Errorf("%v: %v = %v", g.position0(n), n.Expr.Operand, n.Expr2.Operand))
		}

		var v ast.Expr
		comm := ""
		if isVaList(n.Expr.Operand.Type) && n.Expr2.Case == c99.ExprCast {
			comm = fmt.Sprintf("/*TODO103 %v = %v */", n.Expr.Operand, n.Expr2.Operand)
			if ec := n.Expr2; g.voidCanIgnore(ec) {
				switch op := ec.Expr; {
				case op.IsNonZero():
					v = takeAddr(ident(ap))
				case op.IsZero():
					v = ident("nil")
				default:
					panic(g.position0(n))
				}
			} else {
				panic(g.position0(n))
			}
		} else {
			v = g.convert(n.Expr2, n.Expr.Operand.Type)
		}
		out = append(out, commentStmt(assign(
			ptrVal(g.lvalue(n.Expr)), v,
		), comm))
		return out
	case
		c99.ExprPostInc, // Expr "++"
		c99.ExprPreInc:  // "++" Expr

		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			switch sz := g.model.Sizeof(x.Item); {
			case sz == 1:
				return []ast.Stmt{
					inc(ptrVal(g.lvalue(n.Expr))),
				}
			default:
				return []ast.Stmt{
					incn(g.value(n.Expr, false), sz),
				}
			}
		case c99.TypeKind:
			if op := n.Expr.Operand; op.Bits() != 0 {
				fp := op.FieldProperties
				return []ast.Stmt{exprStmt(call(
					g.registerHelper("postinc%db", 1, g.typ(op.Type), g.typ(fp.PackedType), g.model.Sizeof(op.Type)*8, fp.Bits, fp.Bitoff),
					takeAddr(g.value(n.Expr, true)),
				))}
			}

			if x.IsArithmeticType() {
				return []ast.Stmt{
					inc(ptrVal(g.lvalue(n.Expr))),
				}
			}
			todo("%v: %v", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case
		c99.ExprPostDec, // Expr "--"
		c99.ExprPreDec:  // "--" Expr

		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			switch sz := g.model.Sizeof(x.Item); {
			case sz == 1:
				return []ast.Stmt{
					dec(ptrVal(g.lvalue(n.Expr))),
				}
			default:
				return []ast.Stmt{
					decn(g.value(n.Expr, false), sz),
				}
			}
		case c99.TypeKind:
			if op := n.Expr.Operand; op.Bits() != 0 {
				fp := op.FieldProperties
				return []ast.Stmt{exprStmt(call(
					g.registerHelper("postinc%db", g.convertInt64(-1, x), g.typ(op.Type), g.typ(fp.PackedType), g.model.Sizeof(op.Type)*8, fp.Bits, fp.Bitoff),
					takeAddr(g.value(n.Expr, true)),
				))}
			}

			if x.IsArithmeticType() {
				return []ast.Stmt{
					dec(ptrVal(g.lvalue(n.Expr))),
				}
			}
			todo("%v: %v", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case
		c99.ExprAddAssign, // Expr "+=" Expr
		c99.ExprSubAssign: // Expr "-=" Expr
		switch {
		case c99.UnderlyingType(n.Expr.Operand.Type).Kind() == c99.Ptr:
			op := token.ADD_ASSIGN
			if n.Case == c99.ExprSubAssign {
				op = token.SUB_ASSIGN
			}
			return []ast.Stmt{assignStmt(
				ptrVal(g.lvalue(n.Expr)),
				op,
				toUintptrMul(
					g.value(n.Expr2, false),
					g.model.Sizeof(n.Expr.Operand.Type.(*c99.PointerType).Item),
				),
			)}
		default:
			return g.voidArithmeticAsop(n)
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

		return g.voidArithmeticAsop(n)
	case c99.ExprPExprList: // '(' ExprList ')'
		for l := n.ExprList; l != nil; l = l.ExprList {
			out = append(out, g.void(l.Expr)...)
		}
		return out
	case c99.ExprCast: // '(' TypeName ')' Expr
		if isVaList(n.Expr.Operand.Type) { //TODO- ?
			return []ast.Stmt{exprStmt(
				g.castVA(n.Expr, n.TypeName.Type),
			)}
		}

		return g.void(n.Expr)
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		switch {
		case n.Expr.IsZero() && g.voidCanIgnore(n.Expr):
			return g.void(n.Expr2)
		case n.Expr.IsNonZero() && g.voidCanIgnore(n.Expr):
			return g.exprListVoid(n.ExprList)
		default:
			// if expr != 0 {
			//	exprList
			// } else {
			//	expr2
			// }
			return []ast.Stmt{ifElseStmt(
				neqZero(g.value(n.Expr, false)),
				g.exprListVoid(n.ExprList),
				g.void(n.Expr2),
			)}
		}
	case c99.ExprLAnd: // Expr "&&" Expr
		if n.Expr.IsZero() && g.voidCanIgnore(n.Expr) {
			return
		}
		return []ast.Stmt{
			ifStmt(
				neqZero(g.value(n.Expr, false)),
				g.void(n.Expr2)...,
			),
		}
	case c99.ExprLOr: // Expr "||" Expr
		if n.Expr.IsNonZero() && g.voidCanIgnore(n.Expr) {
			return
		}
		return []ast.Stmt{
			ifStmt(
				eqZero(g.value(n.Expr, false)),
				g.void(n.Expr2)...,
			),
		}
	case c99.ExprIndex: // Expr '[' ExprList ']'
		out = append(out, g.void(n.Expr)...)
		out = append(out, g.exprListVoid(n.ExprList)...)
		return out
	case // Unary
		c99.ExprAddrof,     // '&' Expr
		c99.ExprCpl,        // '~' Expr
		c99.ExprDeref,      // '*' Expr
		c99.ExprNot,        // '!' Expr
		c99.ExprUnaryMinus, // '-' Expr
		c99.ExprUnaryPlus:  // '+' Expr

		return g.void(n.Expr)
	case // Binary
		c99.ExprAdd, // Expr '+' Expr
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprDiv, // Expr '/' Expr
		c99.ExprEq,  // Expr "==" Expr
		c99.ExprGe,  // Expr ">=" Expr
		c99.ExprGt,  // Expr ">" Expr
		c99.ExprLe,  // Expr "<=" Expr
		c99.ExprLsh, // Expr "<<" Expr
		c99.ExprLt,  // Expr '<' Expr
		c99.ExprMod, // Expr '%' Expr
		c99.ExprMul, // Expr '*' Expr
		c99.ExprNe,  // Expr "!=" Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprRsh, // Expr ">>" Expr
		c99.ExprSub, // Expr '-' Expr
		c99.ExprXor: // Expr '^' Expr

		out = append(out, g.void(n.Expr)...)
		out = append(out, g.void(n.Expr2)...)
		return out
	default:
		todo("", g.position0(n), n.Case, n.Operand) // void
		return nil
	}
}

func (g *gen) lvalue(n *c99.Expr) ast.Expr {
	return takeAddr(g.value(n, false))
}

func (g *gen) value(n *c99.Expr, packedField bool) (ge ast.Expr) {
	defer func() {
		ge = paren(ge)
	}()

	if n.Operand.Value != nil && g.voidCanIgnore(n) {
		return g.constant(n)
	}

	switch n.Case {
	case c99.ExprIdent: // IDENTIFIER
		d := g.normalizeDeclarator(n.Declarator)
		switch {
		case d == nil:
			if n.Operand.Type == nil || n.Operand.Value == nil {
				todo("", g.position0(n), n.Operand)
			}

			// Enum const
			return cast(
				g.constant(n),
				g.typI(n.Operand.Type),
			)
		default:
			g.enqueue(d)
			switch {
			case d.Type.Kind() == c99.Function:
				return call(
					g.registerHelper("fp%d", g.typ(d.Type)),
					g.mangleDeclaratorI(d),
				)
			case g.escaped(d) && c99.UnderlyingType(d.Type).Kind() != c99.Array:
				return unaddrUnsafe(g.typ(d.Type), g.mangleDeclaratorI(d))
			default:
				return g.mangleDeclaratorI(d)
			}
		}
	case c99.ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}
		switch d := n.Declarator; {
		case g.escaped(d):
			todo("", g.position(d))
			return nil
		default:
			return runAndReturn(
				g.mangleDeclaratorI(d),
				g.typI(d.Type),
				assign(g.mangleDeclaratorI(d), g.literal(d.Type, d.Initializer)),
			)
		}
	case
		c99.ExprEq, // Expr "==" Expr
		c99.ExprGe, // Expr ">=" Expr
		c99.ExprGt, // Expr ">" Expr
		c99.ExprLe, // Expr "<=" Expr
		c99.ExprLt, // Expr '<' Expr
		c99.ExprNe: // Expr "!=" Expr

		return g.relop(n)
	case
		c99.ExprAnd, // Expr '&' Expr
		c99.ExprDiv, // Expr '/' Expr
		c99.ExprMod, // Expr '%' Expr
		c99.ExprMul, // Expr '*' Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprXor: // Expr '^' Expr

		return g.binop(n)
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		var d *c99.Declarator
		if d = n.Expr.Declarator; d != nil && d.Name() == idBuiltinAlloca {
			if n.ArgumentExprListOpt.ArgumentExprList.ArgumentExprList != nil {
				todo("", g.position0(n))
			}
			return callFunc("", "alloca",
				takeAddr(ident("allocs")),
				cast(
					g.value(n.ArgumentExprListOpt.ArgumentExprList.Expr, false),
					ident("int"),
				),
			)
		}

		if n.Expr.Case == c99.ExprIdent && n.Expr.Declarator == nil {
			switch x := n.Expr.Scope.LookupIdent(n.Expr.Token.Val).(type) {
			case *c99.Declarator:
				n.Expr.Declarator = x
				d = x
				n.Expr.Operand.Type = &c99.PointerType{Item: x.Type}
			default:
				todo("%v: %T undefined: %q", g.position0(n), x, dict.S(n.Expr.Token.Val))
			}
		}
		if d = n.Expr.Declarator; d != nil {
			d = g.normalizeDeclarator(d)
			n.Expr.Declarator = d
		}
		var ft0 c99.Type
		if !isFnPtr(n.Expr.Operand.Type, &ft0) {
			todo("%v: %v", g.position0(n), n.Expr.Operand.Type)
		}
		ft := c99.UnderlyingType(ft0).(*c99.FunctionType)
		var args []*c99.Expr
		if o := n.ArgumentExprListOpt; o != nil {
			for l := o.ArgumentExprList; l != nil; l = l.ArgumentExprList {
				args = append(args, l.Expr)
			}
		}
		params := ft.Params
		variadic := ft.Variadic
		switch {
		case len(params) == 1 && params[0].Kind() == c99.Void: // void foo(void) can have no args at call site
			params = nil
		case len(ft.Params) == 0: // void foo() can be called with any number of args
			variadic = true
			if len(args) != 0 && d != nil {
				g.fixArgs[d] = len(args)
			}
		}
		switch np := len(params); {
		case len(args) > np && !variadic:
			for _, v := range args[np:] {
				if !g.voidCanIgnore(v) {
					todo("", g.position0(v))
				}
			}
			args = args[:np]
			fallthrough
		case
			len(args) > np && variadic,
			len(args) == np:

			par := []ast.Expr{
				ident("tls"),
			}
			for i, v := range args {
				var pv ast.Expr
				switch t := n.CallArgs[i].Type; {
				case t == nil:
					pv = g.value(v, false)
				default:
					pv = g.convert(v, t)
				}
				par = append(par, pv)
			}
			return call(
				g.convert(n.Expr, ft),
				par...,
			)
		default:
			todo("", g.position0(n), np, len(args), ft.Variadic)
			return nil
		}
	case c99.ExprAddrof: // '&' Expr
		return g.uintptr(n.Expr, false)
	case
		c99.ExprSelect,  // Expr '.' IDENTIFIER
		c99.ExprPSelect: // Expr "->" IDENTIFIER
		fp := n.Operand.FieldProperties

		arr := fp.Declarator.Type.Kind() == c99.Array

		if !arr {
			if fp.Bits != 0 && !packedField {
				return g.bitField(n)
			}
			if n.Case == c99.ExprSelect && n.Expr.Case == c99.ExprCall {
				return sel(
					g.value(n.Expr, false),
					mangleIdent(n.Token2.Val, true),
				)
			}
		}

		var v ast.Expr
		if n.Case == c99.ExprSelect {
			v = g.uintptr(n.Expr, false)
		} else {
			v = g.value(n.Expr, false)
		}

		if arr {
			return addInt(v, fp.Offset)
		}

		t := n.Operand.Type
		if fp.Bits != 0 {
			t = fp.PackedType
		}
		return unaddrUnsafe(g.typ(t), addInt(v, fp.Offset))
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
			return g.indexOff(g.value(n.Expr, false), n.ExprList, it)
		default:
			return unaddrUnsafe(
				g.typ(n.Operand.Type),
				g.indexOff(g.value(n.Expr, false), n.ExprList, it),
			)
		}
	case c99.ExprAdd: // Expr '+' Expr
		switch t, u := c99.UnderlyingType(n.Expr.Operand.Type), c99.UnderlyingType(n.Expr2.Operand.Type); {
		case t.Kind() == c99.Ptr:
			return add(
				g.value(n.Expr, false),
				mul(
					intLit(g.model.Sizeof(t.(*c99.PointerType).Item)),
					toUintptr(g.value(n.Expr2, false)),
				),
			)
		case u.Kind() == c99.Ptr:
			return add(
				mul(
					intLit(g.model.Sizeof(u.(*c99.PointerType).Item)),
					toUintptr(g.value(n.Expr, false)),
				),
				g.value(n.Expr2, false),
			)
		default:
			return g.binop(n)
		}
	case c99.ExprSub: // Expr '-' Expr
		switch t, u := c99.UnderlyingType(n.Expr.Operand.Type), c99.UnderlyingType(n.Expr2.Operand.Type); {
		case t.Kind() == c99.Ptr && u.Kind() == c99.Ptr:
			return cast(
				div(
					paren(sub(
						g.value(n.Expr, false),
						g.value(n.Expr2, false),
					)),
					intLit(g.model.Sizeof(t.(*c99.PointerType).Item)),
				),
				g.typI(n.Operand.Type),
			)
		case t.Kind() == c99.Ptr:
			return sub(
				g.value(n.Expr, false),
				mul(
					intLit(g.model.Sizeof(t.(*c99.PointerType).Item)),
					toUintptr(g.value(n.Expr2, false)),
				),
			)
		default:
			return g.binop(n)
		}
	case c99.ExprDeref: // '*' Expr
		it := c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item
		switch it.Kind() {
		case
			c99.Array,
			c99.Function:

			return g.value(n.Expr, false)
		default:
			i := 1
			for n.Expr.Case == c99.ExprDeref {
				i++
				n = n.Expr
			}
			return unaddrUnsafeN(g.typ(it), i, g.value(n.Expr, false))
		}
	case c99.ExprAssign: // Expr '=' Expr
		return g.assignmentValue(n)
	case
		c99.ExprLAnd, // Expr "&&" Expr
		c99.ExprLOr:  // Expr "||" Expr
		if n.Operand.Value != nil && g.voidCanIgnore(n) {
			return g.constant(n)
		}
		g.needBool2int++

		var op token.Token
		if n.Case == c99.ExprLAnd {
			op = token.LAND
		} else {
			op = token.LOR
			if n.Expr.Equals(n.Expr2) {
				return bool2int(neqZero(g.value(n.Expr, false)))
			}
		}
		x, y := neqZero(g.value(n.Expr, false)), neqZero(g.value(n.Expr2, false))
		return bool2int(binaryExpr(x, op, y))
	case c99.ExprCond: // Expr '?' ExprList ':' Expr
		t := n.Operand.Type
		if g.voidCanIgnore(n.Expr) {
			if n.Expr.IsZero() {
				return g.value(n.Expr2, false)
			} else if n.Expr.IsNonZero() {
				return g.exprList(n.ExprList)
			}
		}
		return eval(
			g.typI(t),
			ifStmt(
				neqZero(g.value(n.Expr, false)),
				returnVal(g.exprListConv(n.ExprList, t)),
			),
			returnVal(g.convert(n.Expr2, t)),
		)
	case c99.ExprCast: // '(' TypeName ')' Expr
		t := n.TypeName.Type

		if op := n.Expr.Operand; isVaList(op.Type) {
			return g.castVA(n.Expr, t)
		}

		switch x := c99.UnderlyingType(t).(type) {
		case *c99.PointerType:
			if d := n.Expr.Declarator; x.Item.Kind() == c99.Function && d != nil && g.normalizeDeclarator(d).Type.Equal(x.Item) {
				return g.value(n.Expr, false)
			}
		}
		return g.convert(n.Expr, t)
	case
		c99.ExprPreInc,  // "++" Expr
		c99.ExprPostInc: // Expr "++"
		name := ""
		switch n.Case {
		case c99.ExprPreInc:
			name = "preinc"
		case c99.ExprPostInc:
			name = "postinc"
		}
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			return call(
				g.registerHelper(name+"%d", g.typ(x), g.model.Sizeof(x.Item)),
				g.lvalue(n.Expr),
			)
		case c99.TypeKind:
			if op := n.Expr.Operand; op.Bits() != 0 {
				fp := op.FieldProperties
				return call(
					g.registerHelper(name+"%db", 1, g.typ(op.Type), g.typ(fp.PackedType), g.model.Sizeof(op.Type)*8, fp.Bits, fp.Bitoff),
					takeAddr(g.value(n.Expr, true)),
				)
			}

			if x.IsArithmeticType() {
				return call(
					g.registerHelper(name+"%d", g.typ(x), 1),
					g.lvalue(n.Expr),
				)
			}

			todo("%v: %v", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case
		c99.ExprPreDec,  // "--" Expr
		c99.ExprPostDec: // Expr "--"
		name := ""
		switch n.Case {
		case c99.ExprPreDec:
			name = "preinc"
		case c99.ExprPostDec:
			name = "postinc"
		}
		switch x := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			return call(
				g.registerHelper(name+"%d", g.typ(x), g.int64ToUintptr(-g.model.Sizeof(x.Item))),
				g.lvalue(n.Expr),
			)
		case c99.TypeKind:
			if op := n.Expr.Operand; op.Bits() != 0 {
				fp := op.FieldProperties
				return call(
					g.registerHelper(name+"%db", g.convertInt64(-1, x), g.typ(op.Type), g.typ(fp.PackedType), g.model.Sizeof(op.Type)*8, fp.Bits, fp.Bitoff),
					takeAddr(g.value(n.Expr, true)),
				)
			}

			if x.IsArithmeticType() {
				return call(
					g.registerHelper(name+"%d", g.typ(x), g.convertInt64(-1, x)),
					g.lvalue(n.Expr),
				)
			}
			todo("%v: %v", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case c99.ExprNot: // '!' Expr
		g.needBool2int++
		return bool2int(eqZero(g.value(n.Expr, false)))
	case
		c99.ExprLsh, // Expr "<<" Expr
		c99.ExprRsh: // Expr ">>" Expr
		x := g.convert(n.Expr, n.Operand.Type)
		y := paren(binaryExpr(
			cast(
				g.value(n.Expr2, false),
				ident("uint"),
			),
			token.REM,
			intLit(int64(g.shiftMod(c99.UnderlyingType(n.Operand.Type)))),
		))
		if n.Case == c99.ExprLsh {
			return shle(x, y)
		}
		return shre(x, y)
	case c99.ExprUnaryMinus: // '-' Expr
		return unaryExpr(token.SUB, g.convert(n.Expr, n.Operand.Type))
	case c99.ExprCpl: // '~' Expr
		return unaryExpr(token.XOR, g.convert(n.Expr, n.Operand.Type))
	case c99.ExprAddAssign: // Expr "+=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.PointerType:
			g.needPreInc = true
			return call(
				ident("preinc"),
				g.lvalue(n.Expr),
				mul(
					intLit(g.model.Sizeof(x.Item)),
					toUintptr(g.value(n.Expr2, false)),
				),
			)
		case c99.TypeKind:
			if x.IsIntegerType() {
				switch op := n.Expr.Operand; {
				case op.Bits() != 0:
					todo("", g.position0(n))
				default:
					pro, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
					return call(
						g.registerHelper("add%d", "+", g.typ(x), g.typ(n.Expr2.Operand.Type), g.typ(pro.Type)),
						g.lvalue(n.Expr),
						g.value(n.Expr2, false),
					)
				}
				return nil
			}
			todo("", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case c99.ExprSubAssign, // Expr "-=" Expr
		c99.ExprMulAssign, // Expr "*=" Expr
		c99.ExprDivAssign, // Expr "/=" Expr
		c99.ExprModAssign: // Expr "%=" Expr
		name, opr := "", ""
		switch n.Case {
		case c99.ExprSubAssign:
			name, opr = "sub", "-"
		case c99.ExprMulAssign:
			name, opr = "mul", "*"
		case c99.ExprDivAssign:
			name, opr = "div", "/"
		case c99.ExprModAssign:
			name, opr = "mod", "%"
		}
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsIntegerType() {
				switch op := n.Expr.Operand; {
				case op.Bits() != 0:
					todo("", g.position0(n))
				default:
					pro, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
					return call(
						g.registerHelper(name+"%d", opr, g.typ(x), g.typ(n.Expr2.Operand.Type), g.typ(pro.Type)),
						g.lvalue(n.Expr),
						g.value(n.Expr2, false),
					)
				}
				return nil
			}
			todo("", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case
		c99.ExprOrAssign,  // Expr "|=" Expr
		c99.ExprAndAssign, // Expr "&=" Expr
		c99.ExprXorAssign: // Expr "^=" Expr
		name, opr := "", ""
		switch n.Case {
		case c99.ExprOrAssign:
			name, opr = "or", "|"
		case c99.ExprAndAssign:
			name, opr = "and", "&"
		case c99.ExprXorAssign:
			name, opr = "xor", "^"
		}
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsIntegerType() {
				switch op := n.Expr.Operand; {
				case op.Bits() != 0:
					fp := op.FieldProperties
					pro, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
					return call(
						g.registerHelper(name+"%db", opr, g.typ(n.Expr.Operand.Type), g.typ(n.Expr2.Operand.Type), g.typ(pro.Type), g.typ(fp.PackedType), fp.Bitoff, g.model.Sizeof(pro.Type)*8, fp.Bits, g.model.Sizeof(op.Type)*8),
						takeAddr(g.value(n.Expr, true)),
						g.value(n.Expr2, false),
					)
				default:
					pro, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
					return call(
						g.registerHelper(name+"%d", opr, g.typ(x), g.typ(n.Expr2.Operand.Type), g.typ(pro.Type)),
						g.lvalue(n.Expr),
						g.value(n.Expr2, false),
					)
				}
				return nil
			}
			todo("", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case c99.ExprPExprList: // '(' ExprList ')'
		return g.exprListConv(n.ExprList, n.Operand.Type)
	case c99.ExprRshAssign: // Expr ">>=" Expr
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case c99.TypeKind:
			if x.IsIntegerType() {
				switch op := n.Expr.Operand; {
				case op.Bits() != 0:
					todo("", g.position0(n))
				default:
					return call(
						g.registerHelper("rsh%d", ">>", g.typ(n.Expr.Operand.Type), g.typ(x)),
						g.lvalue(n.Expr),
						binaryExpr(
							cast(
								g.value(n.Expr2, false),
								ident("uint"),
							),
							token.REM,
							intLit(int64(g.shiftMod(x))),
						),
					)
				}
				return nil
			}
			todo("", g.position0(n), x)
			return nil
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case c99.ExprUnaryPlus: // '+' Expr
		return g.convert(n.Expr, n.Operand.Type)
	case
		c99.ExprInt,        // INTCONST
		c99.ExprSizeofExpr, // "sizeof" Expr
		c99.ExprSizeofType, // "sizeof" '(' TypeName ')'
		c99.ExprString:     // STRINGLITERAL

		return g.constant(n)
	default:
		todo("", g.position0(n), n.Case, n.Operand) // value
		return nil
	}
}

func (g *gen) pexprList(n *c99.ExprList) (r []*c99.Expr) {
	if n == nil {
		panic("should not be nil")
	}
	for l := n; l != nil; l = l.ExprList {
		if e := l.Expr; l.ExprList == nil || !g.voidCanIgnore(e) {
			r = append(r, e)
		}
	}
	return r
}

func (g *gen) pexprListSplit(n *c99.ExprList) (last *c99.Expr, stmts []ast.Stmt) {
	l := g.pexprList(n)
	li := len(l) - 1
	for _, v := range l[:li] {
		stmts = append(stmts, g.void(v)...)
	}
	return l[li], stmts
}

func (g *gen) bitField(n *c99.Expr) ast.Expr {
	op := n.Operand
	fp := op.FieldProperties
	bits := int(g.model.Sizeof(op.Type) * 8)
	return shr(
		shl(
			cast(
				shr(g.value(n, true), fp.Bitoff),
				g.typI(op.Type),
			),
			bits-fp.Bits,
		),
		bits-fp.Bits,
	)
}

func (g *gen) indexOff(x ast.Expr, n *c99.ExprList, it c99.Type) ast.Expr {
	var v ast.Expr
	switch {
	case n.Operand.Value != nil && g.voidCanIgnoreExprList(n):
		v = intLit(g.model.Sizeof(it) * n.Operand.Value.(*ir.Int64Value).Value)
	default:
		v = mul(
			intLit(g.model.Sizeof(it)),
			cast(
				g.exprList(n),
				ident("uintptr"),
			),
		)
	}
	return add(x, v)
}

func (g *gen) uintptr(n *c99.Expr, packedField bool) (ge ast.Expr) {
	defer func() {
		ge = paren(ge)
	}()

	switch n.Case {
	case c99.ExprPExprList: // '(' ExprList ')'
		last, stmts := g.pexprListSplit(n.ExprList)
		return runAndReturn(
			g.uintptr(last, packedField),
			uintPtr(),
			stmts...,
		)
	case c99.ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}
		switch d := n.Declarator; {
		case g.escaped(d):
			return runAndReturn(
				g.mangleDeclaratorI(d),
				ident("uintptr"),
				setPtrUnsafe(g.typ(d.Type), g.mangleDeclaratorI(d), g.literal(d.Type, d.Initializer)),
			)
		default:
			return runAndReturn(
				toUintptr(toUnsafePtr(takeAddr(
					g.mangleDeclaratorI(d),
				))),
				uintPtr(),
				assign(g.mangleDeclaratorI(d), g.literal(d.Type, d.Initializer)),
			)
		}
	case c99.ExprIdent: // IDENTIFIER
		d := g.normalizeDeclarator(n.Declarator)
		g.enqueue(d)
		arr := c99.UnderlyingType(d.Type).Kind() == c99.Array
		switch {
		case d.Type.Kind() == c99.Function:
			return call(
				g.registerHelper("fp%d", g.typ(d.Type)),
				g.mangleDeclaratorI(d),
			)
		case arr:
			return g.mangleDeclaratorI(d)
		case g.escaped(d):
			return g.mangleDeclaratorI(d)
		default:
			return cast(
				toUnsafePtr(
					takeAddr(g.mangleDeclaratorI(d)),
				),
				ident("uintptr"),
			)
		}
	case c99.ExprIndex: // Expr '[' ExprList ']'
		switch x := c99.UnderlyingType(n.Expr.Operand.Type).(type) {
		case *c99.ArrayType:
			return g.indexOff(g.uintptr(n.Expr, false), n.ExprList, x.Item)
		case *c99.PointerType:
			return g.indexOff(g.value(n.Expr, false), n.ExprList, x.Item)
		default:
			todo("%v: %T", g.position0(n), x)
			return nil
		}
	case c99.ExprSelect: // Expr '.' IDENTIFIER
		fp := n.Operand.FieldProperties
		if bits := fp.Bits; bits != 0 && !packedField {
			todo("", g.position0(n), n.Operand)
		}
		return add(
			g.uintptr(n.Expr, packedField),
			intLit(fp.Offset),
		)
	case c99.ExprPSelect: // Expr "->" IDENTIFIER
		fp := n.Operand.FieldProperties
		if bits := fp.Bits; bits != 0 && !packedField {
			todo("", g.position0(n), n.Operand)
		}
		return add(
			g.value(n.Expr, false),
			intLit(fp.Offset),
		)
	case c99.ExprDeref: // '*' Expr
		switch c99.UnderlyingType(c99.UnderlyingType(n.Expr.Operand.Type).(*c99.PointerType).Item).(type) {
		case *c99.ArrayType:
			return g.value(n.Expr, false)
		default:
			return g.value(n.Expr, false)
		}
	case c99.ExprString: // STRINGLITERAL
		return g.constant(n)
	default:
		todo("", g.position0(n), n.Case, n.Operand) // uintptr
		return nil
	}
}

func (g *gen) voidCanIgnore(n *c99.Expr) bool {
	switch n.Case {
	case
		c99.ExprAlignofExpr, // "__alignof__" Expr
		c99.ExprAlignofType, // "__alignof__" '(' TypeName ')'
		c99.ExprChar,        // CHARCONST
		c99.ExprFloat,       // FLOATCONST
		c99.ExprIdent,       // IDENTIFIER
		c99.ExprInt,         // INTCONST
		c99.ExprSizeofExpr,  // "sizeof" Expr
		c99.ExprSizeofType,  // "sizeof" '(' TypeName ')'
		c99.ExprString:      // STRINGLITERAL

		return true
	case c99.ExprPExprList: // '(' ExprList ')'
		return g.voidCanIgnoreExprList(n.ExprList)
	case c99.ExprCall: // Expr '(' ArgumentExprListOpt ')'
		switch n.Expr.Case {
		case c99.ExprIdent:
			switch n.Expr.Token.Val {
			case idBuiltinTypesCompatible:
				return true
			}
		}
		return false
	case
		c99.ExprAddAssign, // Expr "+=" Expr
		c99.ExprAndAssign, // Expr "&=" Expr
		c99.ExprAssign,    // Expr '=' Expr
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
		if !g.voidCanIgnore(n.Expr) {
			return false
		}

		switch {
		case n.Expr.IsNonZero():
			return g.voidCanIgnoreExprList(n.ExprList)
		case n.Expr.IsZero():
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
		c99.ExprMod, // Expr '%' Expr
		c99.ExprMul, // Expr '*' Expr
		c99.ExprNe,  // Expr "!=" Expr
		c99.ExprOr,  // Expr '|' Expr
		c99.ExprRsh, // Expr ">>" Expr
		c99.ExprSub, // Expr '-' Expr
		c99.ExprXor: // Expr '^' Expr

		return g.voidCanIgnore(n.Expr) && g.voidCanIgnore(n.Expr2)
	case c99.ExprLAnd: // Expr "&&" Expr
		return g.voidCanIgnore(n.Expr) && g.voidCanIgnore(n.Expr2)
	case c99.ExprLOr: // Expr "||" Expr
		return g.voidCanIgnore(n.Expr) && g.voidCanIgnore(n.Expr2)
	case
		c99.ExprAddrof,     // '&' Expr
		c99.ExprCpl,        // '~' Expr
		c99.ExprDeref,      // '*' Expr
		c99.ExprNot,        // '!' Expr
		c99.ExprPSelect,    // Expr "->" IDENTIFIER
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
	if n.ExprList == nil {
		return g.voidCanIgnore(n.Expr)
	}

	for l := n; l != nil; l = l.ExprList {
		if !g.voidCanIgnore(l.Expr) {
			return false
		}
	}

	return true
}

func (g *gen) constant(n *c99.Expr) ast.Expr {
	switch x := n.Operand.Value.(type) {
	case *ir.Float32Value:
		switch {
		case math.IsInf(float64(x.Value), 1):
			return callFunc("math", "Inf", intLit(1))
		case math.IsInf(float64(x.Value), -1):
			return callFunc("math", "Inf", intLit(-1))
		}
		switch u := c99.UnderlyingType(n.Operand.Type).(type) {
		case c99.TypeKind:
			switch u {
			case c99.Double:
				switch {
				case x.Value == 0 && math.Copysign(1, float64(x.Value)) == -1:
					g.needNZ64 = true
					return ident("nz64")
				default:
					return floatLit(float64(x.Value))
				}
			case c99.Float:
				switch {
				case x.Value == 0 && math.Copysign(1, float64(x.Value)) == -1:
					g.needNZ32 = true
					return ident("nz32")
				default:
					return floatLit(float64(float32(x.Value)))
				}
			default:
				todo("", g.position0(n), u)
				return nil
			}
		default:
			todo("%v: %T", g.position0(n), u)
			return nil
		}
	case *ir.Float64Value:
		switch {
		case math.IsInf(x.Value, 1):
			return callFunc("math", "Inf", intLit(1))
		case math.IsInf(x.Value, -1):
			return callFunc("math", "Inf", intLit(-1))
		}

		switch u := c99.UnderlyingType(n.Operand.Type).(type) {
		case c99.TypeKind:
			if u.IsIntegerType() {
				return intLit(c99.ConvertFloat64(x.Value, u, g.model))
			}

			switch u {
			case
				c99.Double,
				c99.LongDouble:

				switch {
				case x.Value == 0 && math.Copysign(1, x.Value) == -1:
					g.needNZ64 = true
					return ident("nz64")
				default:
					return floatLit(x.Value)
				}
			case c99.Float:
				switch {
				case x.Value == 0 && math.Copysign(1, x.Value) == -1:
					g.needNZ32 = true
					return ident("nz32")
				default:
					return floatLit(float64(float32(x.Value)))
				}
			default:
				todo("", g.position0(n), u)
				return nil
			}
		default:
			todo("%v: %T", g.position0(n), u)
			return nil
		}
	case *ir.Int64Value:
		if n.Case == c99.ExprChar {
			return charLit(rune(x.Value))
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

		switch y := c99.UnderlyingType(n.Operand.Type).(type) {
		case *c99.PointerType:
			if n.IsZero() {
				return ident(null)
			}

			var v ast.Expr
			switch {
			case y.Item.Kind() == c99.Function:
				v = ptrLit(uintptr(x.Value))
			default:
				v = ptrLitF(f, uintptr(x.Value))
			}
			return toUintptr(v)
		}

		switch {
		case n.Operand.Type.IsUnsigned():
			return uintLitF(f, uint64(c99.ConvertInt64(x.Value, n.Operand.Type, g.model)))
		default:
			return intLitF(f, c99.ConvertInt64(x.Value, n.Operand.Type, g.model))
		}
	case *ir.StringValue:
		return g.tsString(int(x.StringID), strComment(x))
	case *ir.AddressValue:
		if x == c99.Null {
			return ident(null)
		}

		todo("", g.position0(n))
		return nil
	default:
		todo("%v: %v %T(%v)", g.position0(n), n.Operand, x, x)
		return nil
	}
} // constant

func (g *gen) voidArithmeticAsop(n *c99.Expr) []ast.Stmt {
	op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
	opr := ""
	tp2 := op.Type
	switch n.Token.Rune {
	case c99.ANDASSIGN:
		opr = "&"
	case c99.ADDASSIGN:
		opr = "+"
	case c99.SUBASSIGN:
		opr = "-"
	case c99.MULASSIGN:
		opr = "*"
	case c99.DIVASSIGN:
		opr = "/"
	case c99.ORASSIGN:
		opr = "|"
	case c99.RSHASSIGN:
		opr = ">>"
		tp2 = c99.UInt
	case c99.LSHASSIGN:
		opr = "<<"
		tp2 = c99.UInt
	case c99.XORASSIGN:
		opr = "^"
	case c99.MODASSIGN:
		opr = "%"
	default:
		todo("", g.position0(n), c99.TokSrc(n.Token))
	}

	doOp := func(x ast.Expr, par bool) ast.Expr {
		tok := toTokenS(opr)
		y := g.convert(n.Expr2, tp2)
		if par {
			y = paren(y)
		}
		return binaryExpr(x, tok, y)
	}

	var mask uint64
	lhs := n.Expr.Operand
	switch {
	case lhs.Bits() != 0:
		fp := lhs.FieldProperties

		tmp := ident("p")
		bits := int(g.model.Sizeof(fp.Type) * 8)
		mask = (uint64(1)<<uint(fp.Bits) - 1) << uint(fp.Bitoff)

		// (*p &^ %#x)
		xmask := paren(binaryExpr(
			ptrVal(tmp),
			token.AND_NOT,
			uintLitF("%#x", mask),
		))

		//TODO- g.w("; *p = (*p &^ %#x) | (%s((%s(*p>>%d)<<%d>>%[5]d) ", mask, g.typ(fp.PackedType), g.typ(op.Type), fp.Bitoff, opBits-fp.Bits)

		// a3(a4(*p>>a5)<<a6>>a6)
		xcast1 := cast(
			trimBits(
				cast(
					shr(ptrVal(tmp), fp.Bitoff),
					g.typI(fp.Type),
				),
				bits-fp.Bits,
			),
			g.typI(op.Type),
		)
		// a2( (xcast1) op (x) )
		xcast2 := cast(
			doOp(paren(xcast1), true),
			g.typI(fp.PackedType),
		)

		// (xcast2 << a & b)
		xshift2 := paren(binaryExpr(
			shl(
				xcast2,
				lhs.FieldProperties.Bitoff,
			),
			token.AND,
			uintLitF("%#x", mask),
		))

		// *p := &a
		// *p = xmask | xshift2
		return []ast.Stmt{block(
			define(tmp, takeAddr(g.value(n.Expr, true))),
			setPtr(tmp, binaryExpr(
				xmask, token.OR, xshift2,
			)),
		)}
	case n.Expr.Declarator != nil:
		return []ast.Stmt{
			setPtr(g.lvalue(n.Expr), cast(
				doOp(g.convert(n.Expr, op.Type), false),
				g.typI(n.Expr.Operand.Type),
			)),
		}
	default:
		tmp := ident("p")
		return []ast.Stmt{block(
			define(tmp, g.lvalue(n.Expr)),
			setPtr(tmp,
				cast(
					doOp(cast(
						ptrVal(tmp),
						g.typI(op.Type),
					), false),
					g.typI(n.Expr.Operand.Type),
				),
			),
		)}
	}
}

func (g *gen) assignmentValue(n *c99.Expr) ast.Expr {
	switch op := n.Expr.Operand; {
	case op.Bits() != 0:
		fp := op.FieldProperties
		return call(
			g.registerHelper("set%db", g.typ(fp.PackedType), g.typ(op.Type), g.typ(n.Expr2.Operand.Type), fp.Bitoff, fp.Bits, g.model.Sizeof(op.Type)*8),
			takeAddr(g.value(n.Expr, true)),
			g.value(n.Expr2, false),
		)
	default:
		return call(
			g.registerHelper("set%d", "", g.typ(op.Type)),
			g.lvalue(n.Expr),
			g.convert(n.Expr2, n.Operand.Type),
		)
	}
}

var tokenMap = map[string]token.Token{
	"+": token.ADD,
	"-": token.SUB,
	"*": token.MUL,
	"/": token.QUO,
	"%": token.REM,

	"&":  token.AND,
	"|":  token.OR,
	"^":  token.XOR,
	">>": token.SHR,
	"<<": token.SHL,

	"<":  token.LSS,
	">":  token.GTR,
	"<=": token.LEQ,
	">=": token.GEQ,
	"==": token.EQL,
	"!=": token.NEQ,
}

func toTokenS(s string) token.Token {
	op, ok := tokenMap[s]
	if !ok {
		panic(fmt.Errorf("unhandled token: %q", s))
	}
	return op
}

func toToken(tok xc.Token) token.Token {
	return toTokenS(c99.TokSrc(tok))
}

func (g *gen) binTypes(n *c99.Expr) (l, r c99.Type) {
	l, r = n.Expr.Operand.Type, n.Expr2.Operand.Type
	if l.IsArithmeticType() && r.IsArithmeticType() {
		op, _ := c99.UsualArithmeticConversions(g.model, n.Expr.Operand, n.Expr2.Operand)
		l, r = op.Type, op.Type
	}
	return
}
func (g *gen) binop(n *c99.Expr) ast.Expr {
	side := func(e *c99.Expr, t c99.Type) ast.Expr {
		switch {
		case
			t.Kind() == c99.Ptr && n.Operand.Type.IsArithmeticType(),
			n.Operand.Type.Kind() == c99.Ptr && t.IsArithmeticType():

			return g.convert(e, n.Operand.Type)
		default:
			return g.convert(e, t)
		}
	}

	l, r := g.binTypes(n)

	return binaryExpr(side(n.Expr, l), toToken(n.Token), side(n.Expr2, r))
}

func (g *gen) relop(n *c99.Expr) ast.Expr {
	g.needBool2int++

	l, r := g.binTypes(n)
	var x, y ast.Expr
	switch {
	case l.Kind() == c99.Ptr || r.Kind() == c99.Ptr:
		x, y = g.value(n.Expr, false), g.value(n.Expr2, false)
	default:
		x, y = g.convert(n.Expr, l), g.convert(n.Expr2, r)
	}
	return bool2int(binaryExpr(x, toToken(n.Token), y))
}

func (g *gen) convert(n *c99.Expr, t c99.Type) ast.Expr {
	if n.Case == c99.ExprPExprList {
		return g.exprListConv(n.ExprList, t)
	}

	if t.Kind() == c99.Function {
		ft := c99.UnderlyingType(t)
		switch n.Case {
		case c99.ExprIdent: // IDENTIFIER
			d := g.normalizeDeclarator(n.Declarator)
			g.enqueue(d)
			dt := c99.UnderlyingType(d.Type)
			if dt.Equal(ft) {
				return g.mangleDeclaratorI(d)
			}

			if c99.UnderlyingType(n.Operand.Type).Equal(&c99.PointerType{Item: ft}) {
				x := g.mangleDeclaratorI(n.Declarator)
				switch {
				case d.Type.Kind() == c99.Ptr:
					var v ast.Expr
					if g.escaped(d) {
						v = unaddrUnsafe("uintptr", x)
					} else {
						v = x
					}
					return call(
						g.registerHelper("fn%d", g.typ(ft)), v,
					)
				default:
					return x
				}
			}

			todo("", g.position0(n))
			return nil
		case c99.ExprCast: // '(' TypeName ')' Expr
			if d := g.normalizeDeclarator(n.Expr.Declarator); d != nil {
				g.enqueue(d)
				x := g.mangleDeclaratorI(d)
				if d.Type.Equal(t) {
					return x
				}
				return call(
					g.registerHelper("fn%d", g.typ(t)),
					call(
						g.registerHelper("fp%d", g.typ(d.Type)),
						x,
					),
				)
			}
			return call(
				g.registerHelper("fn%d", g.typ(ft)),
				g.value(n, false),
			)
		default:
			return call(
				g.registerHelper("fn%d", g.typ(t)),
				g.value(n, false),
			)
		}
	}

	if isVaList(n.Operand.Type) && !isVaList(t) {
		return g.castVA(n, t)
	}

	if t.Kind() == c99.Ptr {
		switch {
		case n.Operand.Value != nil && isVaList(t):
			return ident(ap)
		case n.Operand.Type.Kind() == c99.Ptr:
			return g.value(n, false)
		case isVaList(t):
			switch x := n.Operand.Value.(type) {
			case *ir.Int64Value:
				if x.Value == 1 {
					return ident(ap)
				}
			default:
				todo("%v, %T, %v %v -> %v", g.position0(n), x, n.Case, n.Operand, t)
			}
			todo("", g.position0(n))
			return nil
		case n.Operand.Type.IsIntegerType():
			if n.Operand.Value != nil && g.voidCanIgnore(n) {
				t0 := n.Operand.Type
				n.Operand.Type = t
				c := g.constant(n)
				n.Operand.Type = t0
				return c
			}
			return toUintptr(
				g.value(n, false),
			)
		default:
			todo("%v: %v -> %v, %T, %v", g.position0(n), n.Operand, t, t, c99.UnderlyingType(t))
			return nil
		}
		return nil
	}

	if n.Operand.Type.Equal(t) {
		switch {
		case n.Operand.Value != nil && g.voidCanIgnore(n):
			return cast(
				g.constant(n),
				g.typI(t),
			)
		default:
			return g.value(n, false)
		}
		return nil
	}

	if c99.UnderlyingType(t).IsArithmeticType() {
		if n.Operand.Value == nil && t.IsIntegerType() {
			switch n.Operand.Type.Kind() {
			case c99.Float:
				switch {
				case t.IsUnsigned():
					switch g.model.Sizeof(t) {
					case 8:
						return call(
							g.registerHelper("float2int%d", g.typ(t), math.Nextafter32(math.MaxUint64, 0)),
							g.value(n, false),
						)
					}
				}
			}
		}

		var ev ast.Expr
		switch {
		case n.Operand.Value != nil && g.voidCanIgnore(n):
			if n.Operand.Type.Kind() == c99.Double && t.IsIntegerType() {
				v := c99.ConvertFloat64(n.Operand.Value.(*ir.Float64Value).Value, t, g.model)
				switch {
				case t.IsUnsigned():
					ev = uintLit(uint64(v))
				default:
					ev = intLit(v)
				}
			} else {
				t0 := n.Operand.Type
				n.Operand.Type = t
				ev = g.constant(n)
				n.Operand.Type = t0
			}
		default:
			ev = g.value(n, false)
		}
		return cast(
			ev, g.typI(t),
		)
	}

	todo("%v: %v -> %v, %T, %v", g.position0(n), n.Operand, t, t, c99.UnderlyingType(t))
	return nil
}

func (g *gen) int64ToUintptr(n int64) uint64 {
	switch g.model[c99.Ptr].Size {
	case 4:
		return uint64(uint32(n))
	case 8:
		return uint64(n)
	}
	panic("unreachable")
}

func (g *gen) convertInt64(n int64, t c99.Type) string {
	v := c99.ConvertInt64(n, t, g.model)
	switch {
	case t.IsUnsigned():
		return fmt.Sprint(uint64(v))
	default:
		return fmt.Sprint(v)
	}
}
