// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

// [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

import (
	"bytes"
	"fmt"
	"go/token"
	"sort"
	"strconv"
	"strings"

	"github.com/cznic/ir"
)

// Node represents an AST node.
type Node interface {
	Pos() token.Pos
}

// DeclarationSpecifier describes declaration specifiers.
type DeclarationSpecifier struct {
	StorageClassSpecifiers []*StorageClassSpecifier
	TypeQualifiers         []*TypeQualifier
	TypeSpecifiers         []*TypeSpecifier
	typeSpecifiers         []TypeSpecifierCase
}

func (d *DeclarationSpecifier) typ() Type {
	if d.typeSpecifiers == nil {
		d.typeSpecifiers = make([]TypeSpecifierCase, len(d.TypeSpecifiers))
		for i, v := range d.TypeSpecifiers {
			d.typeSpecifiers[i] = v.Case
		}
		sort.Slice(d.typeSpecifiers, func(i, j int) bool { return d.typeSpecifiers[i] < d.typeSpecifiers[j] })
	}

	// [0]6.7.2-2
	if len(d.typeSpecifiers) == 1 {
		switch d.typeSpecifiers[0] {
		case TypeSpecifierChar:
			return Char
		case TypeSpecifierDouble:
			return Double
		case TypeSpecifierFloat:
			return Float
		case TypeSpecifierInt:
			return Int
		case TypeSpecifierLong:
			return Long
		case TypeSpecifierName:
			ts := d.TypeSpecifiers[0]
			return &NamedType{Name: ts.Token.Val}
		case TypeSpecifierStruct:
			return d.TypeSpecifiers[0].StructOrUnionSpecifier.typ
		case TypeSpecifierUnsigned:
			return UInt
		case TypeSpecifierVoid:
			return Void
		default:
			panic(d.typeSpecifiers)
		}
	}

	switch {
	case d.is(TypeSpecifierChar, TypeSpecifierSigned):
		return SChar
	case d.is(TypeSpecifierChar, TypeSpecifierUnsigned):
		return UChar
	case d.is(TypeSpecifierInt, TypeSpecifierLong):
		return Long
	case d.is(TypeSpecifierInt, TypeSpecifierLong, TypeSpecifierLong):
		return LongLong
	case d.is(TypeSpecifierInt, TypeSpecifierLong, TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULongLong
	case d.is(TypeSpecifierInt, TypeSpecifierLong, TypeSpecifierSigned):
		return Long
	case d.is(TypeSpecifierInt, TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULong
	case d.is(TypeSpecifierInt, TypeSpecifierShort, TypeSpecifierSigned):
		return Short
	case d.is(TypeSpecifierInt, TypeSpecifierShort, TypeSpecifierUnsigned):
		return UShort
	case d.is(TypeSpecifierInt, TypeSpecifierSigned):
		return Int
	case d.is(TypeSpecifierInt, TypeSpecifierUnsigned):
		return UInt
	case d.is(TypeSpecifierLong, TypeSpecifierLong):
		return LongLong
	case d.is(TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULong
	case d.is(TypeSpecifierLong, TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULongLong
	case d.is(TypeSpecifierShort, TypeSpecifierUnsigned):
		return UShort
	default:
		panic(d.typeSpecifiers)
	}
}

func (d *DeclarationSpecifier) is(a ...TypeSpecifierCase) bool {
	if len(d.typeSpecifiers) != len(a) {
		return false
	}

	for i, v := range a {
		if v != d.typeSpecifiers[i] {
			return false
		}
	}
	return true
}

func (d *DeclarationSpecifier) isTypedef() bool {
	for _, v := range d.StorageClassSpecifiers {
		if v.Case == StorageClassSpecifierTypedef {
			return true
		}
	}
	return false
}

func (d *DeclarationSpecifier) isStatic() bool {
	for _, v := range d.StorageClassSpecifiers {
		if v.Case == StorageClassSpecifierStatic {
			return true
		}
	}
	return false
}

func (d *DeclarationSpecifier) isExtern() bool {
	for _, v := range d.StorageClassSpecifiers {
		if v.Case == StorageClassSpecifierExtern {
			return true
		}
	}
	return false
}

func (n *ConstExpr) eval(ctx *context) *Operand {
	if n.Operand == nil {
		n.Operand = n.Expr.eval(ctx)
		if n.Operand.Value == nil { // not a constant expression
			panic("TODO")
		}
	}
	return n.Operand
}

func (n *Expr) eval(ctx *context) *Operand {
	if n.Operand != nil {
		return n.Operand
	}

	switch n.Case {
	//TODO case ExprPreInc: // "++" Expr
	//TODO case ExprPreDec: // "--" Expr
	//TODO case ExprSizeOfType: // "sizeof" '(' TypeName ')'
	case ExprSizeofExpr: // "sizeof" Expr
		// [0]6.5.3.4
		switch t := n.Expr.evalSizeofOperand(ctx).Type.(type) {
		case *ArrayType:
			n.Operand = t.Size.mul(ctx, ctx.sizeof(t.Item))
		case *PointerType:
			n.Operand = ctx.sizeof(t)
		default:
			panic(t)
		}
		if n.Operand.Value == nil {
			panic("TODO")
		}
	case ExprNot: // '!' Expr
		n.Operand = &Operand{Type: Int}
		a := n.Expr.eval(ctx)
		if a.isZero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
			break
		}

		if a.isNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
		}
	case ExprAddrof: // '&' Expr
		// [0]6.5.3.2
		op := n.Expr.eval(ctx)
		if op.Addr != nil {
			n.Operand = op.copy()
			n.Operand.Type = &PointerType{op.Type}
			break
		}

		panic(ctx.position(n))
	case ExprPExprList: // '(' ExprList ')'
		n.Operand = n.ExprList.eval(ctx)
	//TODO case ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	case ExprCast: // '(' TypeName ')' Expr
		// [0]6.5.4
		t := n.TypeName.check(ctx)
		op := n.Expr.eval(ctx)
		if t == Void {
			n.Operand = newOperand(Void, nil, nil)
			break
		}

		if !t.IsScalarType() {
			panic(ctx.position(n))
		}

		if !op.isScalarType() /*TODO- && op.Type.Kind() != Array*/ {
			panic(ctx.position(n))
		}

		switch x := t.(type) {
		case *PointerType:
			n.Operand = op.convertTo(ctx, t)
		case TypeKind:
			switch x {
			case UChar:
				n.Operand = op.convertTo(ctx, t)
			default:
				panic(x)
			}
		default:
			//dbg("", ctx.position(n))
			panic(x)
		}
	case ExprDeref: // '*' Expr
		// [0]6.5.3
		op := n.Expr.eval(ctx)
		switch op.Type.Kind() {
		case Ptr:
			switch item := op.Type.(*PointerType).Item; item.Kind() {
			case
				Char,
				Int:

				n.Operand = &Operand{Type: item}
			default:
				panic(item)
			}
		default:
			panic(op)
		}
	//TODO case ExprUnaryPlus: // '+' Expr
	case ExprUnaryMinus: // '-' Expr
		n.Operand = n.Expr.eval(ctx).unaryMinus(ctx)
	//TODO case ExprCpl: // '~' Expr
	case ExprChar: // CHARCONST
		s := dict.S(n.Token.Val)
		if bytes.Contains(s, []byte{'\\'}) && bytes.Contains(s, []byte{'"'}) {
			panic("TODO") // If present, must replace any `\"` with `"`.
		}
		r, _, tail, err := strconv.UnquoteChar(string(s[1:len(s)-1]), '\'')
		if err != nil {
			panic(err)
		}

		if tail != "" {
			panic("TODO")
		}

		n.Operand = newOperand(Int, &ir.Int64Value{Value: int64(r)}, nil)
	case ExprNe: // Expr "!=" Expr
		n.Operand = n.Expr.eval(ctx).ne(ctx, n.Expr2.eval(ctx))
	//TODO case ExprModAssign: // Expr "%=" Expr
	case ExprLAnd: // Expr "&&" Expr
		n.Operand = &Operand{Type: Int}
		a := n.Expr.eval(ctx)
		if a.isZero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
			break
		}

		b := n.Expr2.eval(ctx)
		if b.isZero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
			break
		}

		if a.isNonzero() && b.isNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
		}
	//TODO case ExprAndAssign: // Expr "&=" Expr
	//TODO case ExprMulAssign: // Expr "*=" Expr
	case ExprPostInc: // Expr "++"
		n.Operand = n.Expr.eval(ctx)
	//TODO case ExprAddAssign: // Expr "+=" Expr
	//TODO case ExprPostDec: // Expr "--"
	//TODO case ExprSubAssign: // Expr "-=" Expr
	//TODO case ExprPSelect: // Expr "->" IDENTIFIER
	//TODO case ExprDivAssign: // Expr "/=" Expr
	//TODO case ExprLsh: // Expr "<<" Expr
	//TODO case ExprLshAssign: // Expr "<<=" Expr
	case ExprLe: // Expr "<=" Expr
		n.Operand = n.Expr.eval(ctx).le(ctx, n.Expr2.eval(ctx))
	case ExprEq: // Expr "==" Expr
		n.Operand = n.Expr.eval(ctx).eq(ctx, n.Expr2.eval(ctx))
	case ExprGe: // Expr ">=" Expr
		n.Operand = n.Expr.eval(ctx).ge(ctx, n.Expr2.eval(ctx))
	//TODO case ExprRsh: // Expr ">>" Expr
	//TODO case ExprRshAssign: // Expr ">>=" Expr
	//TODO case ExprXorAssign: // Expr "^=" Expr
	//TODO case ExprOrAssign: // Expr "|=" Expr
	case ExprLOr: // Expr "||" Expr
		n.Operand = &Operand{Type: Int}
		a := n.Expr.eval(ctx)
		if a.isNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
			break
		}

		b := n.Expr2.eval(ctx)
		if b.isNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
			break
		}

		if a.isZero() && b.isZero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
		}
	//TODO case ExprMod: // Expr '%' Expr
	case ExprAnd: // Expr '&' Expr
		n.Operand = n.Expr.eval(ctx).and(ctx, n.Expr2.eval(ctx))
	//TODO case ExprCall: // Expr '(' ArgumentExprListOpt ')'
	//TODO case ExprMul: // Expr '*' Expr
	case ExprAdd: // Expr '+' Expr
		n.Operand = n.Expr.eval(ctx).add(ctx, n.Expr2.eval(ctx))
	//TODO case ExprSub: // Expr '-' Expr
	//TODO case ExprSelect: // Expr '.' IDENTIFIER
	case ExprDiv: // Expr '/' Expr
		n.Operand = n.Expr.eval(ctx).div(ctx, n.Expr2.eval(ctx)) // [0]6.5.5
	case ExprLt: // Expr '<' Expr
		n.Operand = n.Expr.eval(ctx).lt(ctx, n.Expr2.eval(ctx))
	case ExprAssign: // Expr '=' Expr
		lhs := n.Expr.eval(ctx)
		rhs := n.Expr2.eval(ctx)
		// [0]6.5.16.1
		switch {
		// One of the following shall hold:
		case
			// the left operand has qualified or unqualified
			// arithmetic type and the right has arithmetic type;
			lhs.isArithmeticType() && rhs.isArithmeticType():
		case
			// both operands are pointers to qualified or
			// unqualified versions of compatible types, and the
			// type pointed to by the left has all the qualifiers
			// of the type pointed to by the right;
			lhs.Type.Kind() == Ptr && rhs.Type.Kind() == Ptr &&
				lhs.Type.(*PointerType).Item.Compatible(rhs.Type.(*PointerType).Item):
		default:
			//dbg("", lhs)
			//dbg("", rhs)
			panic(ctx.position(n))
		}
		n.Operand = lhs
	case ExprGt: // Expr '>' Expr
		n.Operand = n.Expr.eval(ctx).gt(ctx, n.Expr2.eval(ctx))
	//TODO case ExprCond: // Expr '?' ExprList ':' Expr
	case ExprIndex: // Expr '[' ExprList ']'
		// [0]6.5.2.1
		op := n.Expr.eval(ctx)
		index := n.ExprList.eval(ctx)
		if op.Type.Kind() != Ptr {
			panic("TODO")
		}
		if !index.isIntegerType() {
			panic("TODO")
		}
		pt := op.Type.(*PointerType)
		n.Operand = &Operand{Type: pt.Item}
		if op.Addr != nil {
			if index.Value == nil {
				panic(ctx.position(n))
			}

			av := *op.Addr
			av.Offset += uintptr(index.Value.(*ir.Int64Value).Value * ctx.model.Sizeof(pt.Item))
			n.Operand.Addr = &av
		}
	//TODO case ExprXor: // Expr '^' Expr
	case ExprOr: // Expr '|' Expr
		n.Operand = n.Expr.eval(ctx).or(ctx, n.Expr2.eval(ctx))
	//TODO case ExprFloat: // FLOATCONST
	case ExprIdent: // IDENTIFIER
		// [0]6.5.1
		nm := n.Token.Val
		switch x := n.scope.lookupIdent(nm).(type) {
		case *Declarator:
			switch t := x.Type.(type) {
			case *ArrayType:
				// [0]6.3.2.1-3
				//
				// Except when it is the operand of the sizeof
				// operator or the unary & operator, or is a
				// string literal used to initialize an array,
				// an expression that has type ‘‘array of
				// type’’ is converted to an expression with
				// type ‘‘pointer to type’’ that points to the
				// initial element of the array object and is
				// not an lvalue. If the array object has
				// register storage class, the behavior is
				// undefined.
				n.Operand = newOperand(&PointerType{t.Item}, nil, nil)
			case *PointerType:
				n.Operand = newOperand(t, nil, nil)
			case TypeKind:
				switch t {
				case Int:
					n.Operand = newOperand(t, nil, nil)
				default:
					panic(t)
				}
			default:
				panic(t)
			}
			if x.StorageDuration == StorageDurationStatic {
				n.Operand.Addr = &ir.AddressValue{Index: -1, Linkage: ir.Linkage(x.Linkage), NameID: ir.NameID(nm)}
			}
		case *EnumerationConstant:
			n.Operand = x.Operand
		default:
			panic(fmt.Errorf("%v: %T", ctx.position(n), x))
		}
	case ExprInt: // INTCONST
		s0 := string(dict.S(n.Token.Val))
		s := s0
	loop:
		for i := len(s) - 1; i > 0; i-- {
			switch s0[i] {
			case 'l', 'L', 'u', 'U':
				s = s[:i]
			default:
				break loop
			}
		}
		decadic := s == "0" || !strings.HasPrefix(s, "0")
		v, err := strconv.ParseUint(s, 0, 64)
		if err != nil {
			panic(fmt.Errorf("%v: %v", ctx.position(n), n.Case))
		}

		// [0]6.4.4.1
		switch suff := strings.ToUpper(s0[len(s):]); {
		case suff == "" && decadic:
			n.Operand = newIntConst(ctx, n, v, Int, Long, LongLong)
		case suff == "":
			n.Operand = newIntConst(ctx, n, v, Int, UInt, Long, ULong, LongLong, ULongLong)
		case suff == "L" && decadic:
			n.Operand = newIntConst(ctx, n, v, Long, LongLong)
		case suff == "UL" && decadic:
			n.Operand = newIntConst(ctx, n, v, ULong, ULongLong)
		default:
			panic(fmt.Errorf("%v: TODO %q %q decadic: %v\n%s", ctx.fset.Position(n.Pos()), s, suff, decadic, PrettyString(n)))
		}
	//TODO case ExprLChar: // LONGCHARCONST
	//TODO case ExprLString: // LONGSTRINGLITERAL
	case ExprString: // STRINGLITERAL
		s := dict.S(n.Token.Val)
		n.Operand = newOperand(&PointerType{Item: Char}, &ir.StringValue{StringID: ir.StringID(dict.ID(s[1 : len(s)-1]))}, nil)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	return n.Operand
}

func (n *Expr) evalSizeofOperand(ctx *context) *Operand {
	if n.Operand != nil {
		return n.Operand
	}

	switch n.Case {
	//TODO case ExprPreInc: // "++" Expr
	//TODO case ExprPreDec: // "--" Expr
	//TODO case ExprSizeOfType: // "sizeof" '(' TypeName ')'
	//TODO case ExprSizeofExpr: // "sizeof" Expr
	//TODO case ExprNot: // '!' Expr
	//TODO case ExprAddrof: // '&' Expr
	case ExprPExprList: // '(' ExprList ')'
		n.Operand = n.ExprList.evalSizeofOperand(ctx)
	//TODO case ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	//TODO case ExprCast: // '(' TypeName ')' Expr
	//TODO case ExprDeref: // '*' Expr
	//TODO case ExprUnaryPlus: // '+' Expr
	//TODO case ExprUnaryMinus: // '-' Expr
	//TODO case ExprCpl: // '~' Expr
	//TODO case ExprChar: // CHARCONST
	//TODO case ExprNe: // Expr "!=" Expr
	//TODO case ExprModAssign: // Expr "%=" Expr
	//TODO case ExprLAnd: // Expr "&&" Expr
	//TODO case ExprAndAssign: // Expr "&=" Expr
	//TODO case ExprMulAssign: // Expr "*=" Expr
	//TODO case ExprPostInt: // Expr "++"
	//TODO case ExprAddAssign: // Expr "+=" Expr
	//TODO case ExprPostDec: // Expr "--"
	//TODO case ExprSubAssign: // Expr "-=" Expr
	//TODO case ExprPSelect: // Expr "->" IDENTIFIER
	//TODO case ExprDivAssign: // Expr "/=" Expr
	//TODO case ExprLsh: // Expr "<<" Expr
	//TODO case ExprLshAssign: // Expr "<<=" Expr
	//TODO case ExprLe: // Expr "<=" Expr
	//TODO case ExprEq: // Expr "==" Expr
	//TODO case ExprGe: // Expr ">=" Expr
	//TODO case ExprRsh: // Expr ">>" Expr
	//TODO case ExprRshAssign: // Expr ">>=" Expr
	//TODO case ExprXorAssign: // Expr "^=" Expr
	//TODO case ExprOrAssign: // Expr "|=" Expr
	//TODO case ExprLOr: // Expr "||" Expr
	//TODO case ExprMod: // Expr '%' Expr
	//TODO case ExprAnd: // Expr '&' Expr
	//TODO case ExprCall: // Expr '(' ArgumentExprListOpt ')'
	//TODO case ExprMul: // Expr '*' Expr
	//TODO case ExprAdd: // Expr '+' Expr
	//TODO case ExprSub: // Expr '-' Expr
	//TODO case ExprSelect: // Expr '.' IDENTIFIER
	//TODO case ExprDiv: // Expr '/' Expr
	//TODO case ExprLt: // Expr '<' Expr
	//TODO case ExprAssign: // Expr '=' Expr
	//TODO case ExprGt: // Expr '>' Expr
	//TODO case ExprCond: // Expr '?' ExprList ':' Expr
	case ExprIndex: // Expr '[' ExprList ']'
		return n.eval(ctx)
	//TODO case ExprXor: // Expr '^' Expr
	//TODO case ExprOr: // Expr '|' Expr
	//TODO case ExprFloat: // FLOATCONST
	case ExprIdent: // IDENTIFIER
		// [0]6.5.1
		nm := n.Token.Val
		switch x := n.scope.lookupIdent(nm).(type) {
		case *Declarator:
			switch t := x.Type.(type) {
			case *ArrayType:
				// [0]6.3.2.1-3
				//
				// Except when it is the operand of the sizeof
				// operator or the unary & operator, or is a
				// string literal used to initialize an array,
				// an expression that has type ‘‘array of
				// type’’ is converted to an expression with
				// type ‘‘pointer to type’’ that points to the
				// initial element of the array object and is
				// not an lvalue. If the array object has
				// register storage class, the behavior is
				// undefined.
				n.Operand = newOperand(t, nil, nil)
			default:
				panic(t)
			}
			if x.StorageDuration == StorageDurationStatic {
				n.Operand.Addr = &ir.AddressValue{Index: -1, Linkage: ir.Linkage(x.Linkage), NameID: ir.NameID(nm)}
			}
		default:
			panic(fmt.Errorf("%v: %T", ctx.position(n), x))
		}
	//TODO case ExprInt: // INTCONST
	//TODO case ExprLChar: // LONGCHARCONST
	//TODO case ExprLString: // LONGSTRINGLITERAL
	//TODO case ExprString: // STRINGLITERAL
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	return n.Operand
}

func (n *ExprList) evalSizeofOperand(ctx *context) *Operand {
	if n.Operand == nil {
		for l := n; l != nil; l = l.ExprList {
			n.Operand = l.Expr.evalSizeofOperand(ctx)
		}
	}
	return n.Operand
}

func (n *TypeName) check(ctx *context) Type {
	// SpecifierQualifierList AbstractDeclaratorOpt
	ds := &DeclarationSpecifier{}
	n.SpecifierQualifierList.check(ctx, ds)
	if n.AbstractDeclaratorOpt == nil {
		n.Type = ds.typ()
		return n.Type
	}

	n.AbstractDeclaratorOpt.check(ctx, ds, ds.typ())
	n.Type = n.AbstractDeclaratorOpt.AbstractDeclarator.Type
	return n.Type
}

func (n *ExprList) eval(ctx *context) *Operand {
	if n.Operand == nil {
		for l := n; l != nil; l = l.ExprList {
			n.Operand = l.Expr.eval(ctx)
		}
	}
	return n.Operand
}

func (n *ExprOpt) eval(ctx *context) *Operand {
	if n == nil {
		return nil
	}

	return n.Expr.eval(ctx)
}

func (n *Declarator) nm() int { return n.DirectDeclarator.nm() }

func (n *DirectDeclarator) nm() int {
	switch n.Case {
	case DirectDeclaratorArray, DirectDeclaratorParamList:
		return n.DirectDeclarator.nm()
	case DirectDeclaratorIdent:
		return n.Token.Val
	case DirectDeclaratorParen:
		return n.Declarator.nm()
	default:
		panic(fmt.Errorf("TODO %v", n.Case))
	}
}

func (n *TranslationUnit) check(ctx *context) (err error) {
	defer func() {
		switch e := recover(); x := e.(type) {
		case nil:
			// nop
		case error:
			err = newPanicError(fmt.Errorf("PANIC: %v\n%s", errString(x), debugStack()))
		default:
			err = newPanicError(fmt.Errorf("PANIC: %v\n%s", e, debugStack()))
		}
	}()

	for ; n != nil; n = n.TranslationUnit {
		n.ExternalDeclaration.check(ctx)
	}
	return nil
}

func (n *ExternalDeclaration) check(ctx *context) {
	switch n.Case {
	case ExternalDeclarationDecl: // Declaration
		n.Declaration.check(ctx)
	case ExternalDeclarationFunc: // FunctionDefinition
		n.FunctionDefinition.check(ctx)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *FunctionDefinition) check(ctx *context) {
	// DeclarationSpecifiers Declarator DeclarationListOpt FunctionBody
	ds := &DeclarationSpecifier{}
	n.DeclarationSpecifiers.check(ctx, ds)
	if len(ds.TypeSpecifiers) == 0 { // [0]6.7.2-2
		panic("TODO")
	}
	n.Declarator.check(ctx, ds, ds.typ(), false, true)
	if n.Declarator.Type.Kind() != Function {
		panic("TODO")
	}
	n.FunctionBody.check(ctx, n.Declarator)
}

func (n *FunctionBody) check(ctx *context, fn *Declarator) {
	// CompoundStmt *CompoundStmt
	n.CompoundStmt.check(ctx, fn, true)
}

func (n *CompoundStmt) check(ctx *context, fn *Declarator, outermost bool) {
	// '{' BlockItemListOpt '}'
	if outermost { // Pull function parameters into the outermost block scope.
		for _, v := range fn.fpScope(ctx).idents {
			d := v.(*Declarator)
			nm := d.nm()
			if ex := n.scope.idents[nm]; ex != nil {
				panic("TODO") // redeclared
			}

			n.scope.insertDeclarator(ctx, d)
		}
	}
	n.BlockItemListOpt.check(ctx, fn)
}

func (n *Declarator) fpScope(ctx *context) *scope { return n.DirectDeclarator.fpScope(ctx) }

func (n *DirectDeclarator) fpScope(ctx *context) *scope {
	switch n.Case {
	//TODO case DirectDeclaratorParen: // '(' Declarator ')'
	//TODO case DirectDeclaratorIdentList: // DirectDeclarator '(' IdentifierListOpt ')'
	case DirectDeclaratorParamList: // DirectDeclarator '(' ParameterTypeList ')'
		return n.paramScope
	//TODO case DirectDeclaratorArraySize: // DirectDeclarator '[' "static" TypeQualifierListOpt Expr ']'
	//TODO case DirectDeclaratorArraySize2: // DirectDeclarator '[' TypeQualifierList "static" Expr ']'
	//TODO case DirectDeclaratorArrayVar: // DirectDeclarator '[' TypeQualifierListOpt '*' ']'
	//TODO case DirectDeclaratorArray: // DirectDeclarator '[' TypeQualifierListOpt ExprOpt ']'
	//TODO case DirectDeclaratorIdent: // IDENTIFIER
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *BlockItemListOpt) check(ctx *context, fn *Declarator) {
	if n == nil {
		return
	}

	n.BlockItemList.check(ctx, fn)
}

func (n *BlockItemList) check(ctx *context, fn *Declarator) {
	for ; n != nil; n = n.BlockItemList {
		n.BlockItem.check(ctx, fn)
	}
}

func (n *BlockItem) check(ctx *context, fn *Declarator) {
	switch n.Case {
	case BlockItemDecl: // Declaration
		n.Declaration.check(ctx)
	case BlockItemStmt: // Stmt
		n.Stmt.check(ctx, fn)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *Stmt) check(ctx *context, fn *Declarator) {
	switch n.Case {
	case StmtBlock: // CompoundStmt
		n.CompoundStmt.check(ctx, fn, false)
	case StmtExpr: // ExprStmt
		n.ExprStmt.check(ctx)
	case StmtIter: // IterationStmt
		n.IterationStmt.check(ctx, fn)
	case StmtJump: // JumpStmt
		n.JumpStmt.check(ctx, fn)
	//TODO case StmtLabeled: // LabeledStmt
	//TODO case StmtSelect: // SelectionStmt
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *IterationStmt) check(ctx *context, fn *Declarator) {
	switch n.Case {
	case IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
		n.Stmt.check(ctx, fn)
		n.ExprList.eval(ctx)
	//TODO case IterationStmtForDecl: // "for" '(' Declaration ExprListOpt ';' ExprListOpt ')' Stmt
	//TODO case IterationStmtFor: // "for" '(' ExprListOpt ';' ExprListOpt ';' ExprListOpt ')' Stmt
	//TODO case IterationStmtWhile: // "while" '(' ExprList ')' Stmt
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *JumpStmt) check(ctx *context, fn *Declarator) {
	switch n.Case {
	//TODO case JumpStmtBreak: // "break" ';'
	//TODO case JumpStmtContinue: // "continue" ';'
	//TODO case JumpStmtGoto: // "goto" IDENTIFIER ';'
	case JumpStmtReturn: // "return" ExprListOpt ';'
		// [0]6.8.6.4
		op := n.ExprListOpt.check(ctx)
		switch t := fn.Type.(*FunctionType).Result; t.Kind() {
		case Void:
			panic(ctx.position(n))
		default:
			n.ReturnOperand = op.convertTo(ctx, t)
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ExprStmt) check(ctx *context) {
	// ExprListOpt ';'
	n.ExprListOpt.check(ctx)
}

func (n *ExprListOpt) check(ctx *context) *Operand {
	if n == nil {
		return nil
	}

	return n.ExprList.check(ctx)
}

func (n *ExprList) check(ctx *context) (r *Operand) {
	for ; n != nil; n = n.ExprList {
		r = n.Expr.eval(ctx)
	}
	return r
}

func (n *Declaration) check(ctx *context) {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	ds := &DeclarationSpecifier{}
	n.DeclarationSpecifiers.check(ctx, ds)
	if len(ds.TypeSpecifiers) == 0 { // [0]6.7.2-2
		panic("TODO")
	}
	n.InitDeclaratorListOpt.check(ctx, ds)
}

func (n *InitDeclaratorListOpt) check(ctx *context, ds *DeclarationSpecifier) {
	if n == nil {
		return
	}

	n.InitDeclaratorList.check(ctx, ds)
}

func (n *InitDeclaratorList) check(ctx *context, ds *DeclarationSpecifier) {
	for ; n != nil; n = n.InitDeclaratorList {
		n.InitDeclarator.check(ctx, ds)
	}
}

func (n *InitDeclarator) check(ctx *context, ds *DeclarationSpecifier) {
	switch n.Case {
	case InitDeclaratorBase: // Declarator
		n.Declarator.check(ctx, ds, ds.typ(), !ds.isTypedef(), false)
	case InitDeclaratorInit: // Declarator '=' Initializer
		if ds.isTypedef() {
			panic(ctx.position(n)) // error
		}
		n.Declarator.check(ctx, ds, ds.typ(), true, false)
		n.Declarator.Initializer = n.Initializer.check(ctx)
		switch t := n.Declarator.Type.(type) {
		case *ArrayType:
			if t.Size == nil {
				switch x := n.Declarator.Initializer.(type) {
				case *ir.CompositeValue: // [0]6.7.8-22
					t.Size = newIntConst(ctx, n, uint64(len(x.Values)), UInt, ULong, ULongLong)
				case *ir.StringValue:
					switch t.Item.Kind() {
					case Char:
						t.Size = newIntConst(ctx, n, uint64(len(dict.S(int(x.StringID)))+1), UInt, ULong, ULongLong)
					default:
						panic(t.Item)
					}
				default:
					panic(ctx.position(n))
				}
			}
		case *PointerType:
			switch n.Declarator.Initializer.(type) {
			case *ir.AddressValue:
				// nop
			default:
				panic(ctx.position(n))
			}
		default:
			panic(fmt.Errorf("%v: %v %v", ctx.position(n), t, n.Declarator.Initializer))
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *Initializer) check(ctx *context) ir.Value { //TODO pass and check declarator type
	switch n.Case {
	case InitializerCompLit: // '{' InitializerList CommaOpt '}'
		return n.InitializerList.check(ctx)
	case InitializerExpr: // Expr
		v := n.Expr.eval(ctx)
		if v.Value == nil {
			if v.Addr == nil {
				panic(ctx.position(n))
			}

			v.Value = v.Addr
		}

		return v.Value
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *InitializerList) check(ctx *context) ir.Value {
	r := &ir.CompositeValue{}
	for ; n != nil; n = n.InitializerList {
		switch {
		case n.Designation != nil:
			panic(ctx.position(n.Initializer))
		default:
			r.Values = append(r.Values, n.Initializer.check(ctx))
		}
	}
	return r
}

func (n *Declarator) check(ctx *context, ds *DeclarationSpecifier, t Type, isObject, isFunction bool) Type { //TODO remove isFunction argument
	// PointerOpt DirectDeclarator
	n.DeclarationSpecifier = ds
	t = n.PointerOpt.check(ctx, t, &n.TypeQualifiers)
	n.Type = n.DirectDeclarator.check(ctx, t)
	switch x := n.Type.(type) {
	case
		*ArrayType,
		*FunctionType,
		*PointerType,
		*StructType,
		*TaggedStruct,
		*UnionType:

		// nop
	case *NamedType:
		d, ok := n.scope.lookupIdent(x.Name).(*Declarator)
		if !ok {
			panic("TODO") // undefined
		}

		if !d.DeclarationSpecifier.isTypedef() {
			panic("internal error")
		}

		x.Type = d.Type
	case TypeKind:
		switch x {
		case
			Double,
			Float,
			Int,
			Long,
			LongLong,
			SChar,
			Short,
			UChar,
			UInt,
			ULong,
			ULongLong,
			UShort,
			Void:

			// nop
		default:
			panic(ctx.position(n))
		}
	default:
		panic(fmt.Errorf("%v: %T", ctx.position(n), x))
	}
	if n.Embedded {
		return n.Type
	}

	// [0]6.2.2
	switch {
	case
		// 3. If the declaration of a file scope identifier for an
		// object or a function contains the storage-class specifier
		// static, the identifier has internal linkage.
		(isObject || isFunction) && n.scope == ctx.scope && ds.isStatic():

		n.Linkage = LinkageInternal
	case
		// 4. For an identifier declared with the storage-class specifier extern in a scope in which a
		// prior declaration of that identifier is visible, if the prior declaration specifies internal or
		// external linkage, the linkage of the identifier at the later declaration is the same as the
		// linkage specified at the prior declaration. If no prior declaration is visible, or if the prior
		// declaration specifies no linkage, then the identifier has external linkage.
		(isObject || isFunction) && ds.isExtern():

		n.Linkage = LinkageExternal
	case
		// 5. If the declaration of an identifier for a function has no
		// storage-class specifier, its linkage is determined exactly
		// as if it were declared with the storage-class specifier
		// extern. If the declaration of an identifier for an object
		// has file scope and no storage-class specifier, its linkage
		// is external.
		(isObject || isFunction) && n.scope == ctx.scope && len(ds.StorageClassSpecifiers) == 0:

		n.Linkage = LinkageExternal
	case
		// 6. The following identifiers have no linkage: an identifier
		// declared to be anything other than an object or a function;
		// an identifier declared to be a function parameter; a block
		// scope identifier for an object declared without the
		// storage-class specifier extern.
		!(isObject || isFunction),
		n.scope != ctx.scope && !ds.isExtern():

		n.Linkage = LinkageNone
	default:
		panic(ctx.position(n))
	}

	// [0]6.2.4
	switch {
	case
		// 3. An object whose identifier is declared with external or
		// internal linkage, or with the storage-class specifier static
		// has static storage duration. Its lifetime is the entire
		// execution of the program and its stored value is initialized
		// only once, prior to program startup.
		n.Linkage == LinkageExternal,
		n.Linkage == LinkageInternal,
		ds.isStatic():

		n.StorageDuration = StorageDurationStatic
	case
		// 4. An object whose identifier is declared with no linkage
		// and without the storage-class specifier static has automatic
		// storage duration.
		n.Linkage == LinkageNone && !ds.isStatic():

		n.StorageDuration = StorageDurationAutomatic
	default:
		panic(ctx.position(n))
	}

	nm := n.nm()
	switch ex := n.scope.idents[n.nm()]; ex := ex.(type) {
	case nil:
		n.scope.insertDeclarator(ctx, n)
	case *Declarator:
		switch ex.Linkage {
		case LinkageNone:
			switch n.Linkage {
			case LinkageNone:
				if ex.DeclarationSpecifier.isTypedef() && n.DeclarationSpecifier.isTypedef() && ex.Type.String() == n.Type.String() {
					break
				}

				panic(ctx.position(n))
			default:
				panic(n.Linkage)
			}
		case LinkageExternal:
			switch n.Linkage {
			case LinkageExternal:
				if !ex.Type.Equal(n.Type) {
					panic(ctx.position(n))
				}

				if isFunction {
					n.scope.idents[nm] = n
				}
			default:
				panic(n.Linkage)
			}
		default:
			panic(ex.Linkage)
		}
	default:
		//dbg("%T", ex)
		panic(ctx.position(n))
	}

	return n.Type
}

func (n *PointerOpt) check(ctx *context, t Type, tq *[]*TypeQualifier) Type {
	if n == nil {
		return t
	}

	return n.Pointer.check(ctx, t, tq)
}

func (n *Pointer) check(ctx *context, t Type, tq *[]*TypeQualifier) Type {
	n.TypeQualifierListOpt.check(ctx, tq)
	switch n.Case {
	case PointerBase: // '*' TypeQualifierListOpt
		return &PointerType{t}
	case PointerPtr: // '*' TypeQualifierListOpt Pointer
		return n.Pointer.check(ctx, &PointerType{t}, tq)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *TypeQualifierListOpt) check(ctx *context, tq *[]*TypeQualifier) {
	if n == nil {
		return
	}

	n.TypeQualifierList.check(ctx, tq)
}

func (n *TypeQualifierList) check(ctx *context, tq *[]*TypeQualifier) {
	s := *tq
	for ; n != nil; n = n.TypeQualifierList {
		s = append(s, n.TypeQualifier)
	}
	*tq = s
}

func (n *DirectDeclarator) check(ctx *context, t Type) Type {
	switch n.Case {
	case DirectDeclaratorParen: // '(' Declarator ')'
		return n.Declarator.check(ctx, nil, t, false, false)
	//TODO case DirectDeclaratorIdentList: // DirectDeclarator '(' IdentifierListOpt ')'
	case DirectDeclaratorParamList: // DirectDeclarator '(' ParameterTypeList ')'
		fp, variadic := n.ParameterTypeList.check(ctx)
		t := &FunctionType{
			Params:   fp,
			Result:   t,
			Variadic: variadic,
		}
		return n.DirectDeclarator.check(ctx, t)
	//TODO case DirectDeclaratorArraySize: // DirectDeclarator '[' "static" TypeQualifierListOpt Expr ']'
	//TODO case DirectDeclaratorArraySize2: // DirectDeclarator '[' TypeQualifierList "static" Expr ']'
	//TODO case DirectDeclaratorArrayVar: // DirectDeclarator '[' TypeQualifierListOpt '*' ']'
	case DirectDeclaratorArray: // DirectDeclarator '[' TypeQualifierListOpt ExprOpt ']'
		var tq []*TypeQualifier
		n.TypeQualifierListOpt.check(ctx, &tq)
		n.ExprOpt.eval(ctx)
		var sz *Operand
		if o := n.ExprOpt; o != nil {
			sz = o.Expr.Operand
		}
		t := &ArrayType{
			Item:           t,
			Size:           sz,
			TypeQualifiers: tq,
		}
		return n.DirectDeclarator.check(ctx, t)
	case DirectDeclaratorIdent: // IDENTIFIER
		return t
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ParameterTypeList) check(ctx *context) ([]Type, bool) {
	switch n.Case {
	case ParameterTypeListBase: // ParameterList
		return n.ParameterList.check(ctx), false
	case ParameterTypeListDots: // ParameterList ',' "..."
		return n.ParameterList.check(ctx), true
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ParameterList) check(ctx *context) (r []Type) {
	for ; n != nil; n = n.ParameterList {
		r = append(r, n.ParameterDeclaration.check(ctx))
	}
	return r
}

func (n *ParameterDeclaration) check(ctx *context) Type {
	switch n.Case {
	case ParameterDeclarationAbstract: // DeclarationSpecifiers AbstractDeclaratorOpt
		ds := &DeclarationSpecifier{}
		n.DeclarationSpecifiers.check(ctx, ds)
		n.AbstractDeclaratorOpt.check(ctx, ds, ds.typ())
		if n.AbstractDeclaratorOpt == nil {
			return ds.typ()
		}

		return n.AbstractDeclaratorOpt.AbstractDeclarator.Type
	case ParameterDeclarationDeclarator: // DeclarationSpecifiers Declarator
		ds := &DeclarationSpecifier{}
		n.DeclarationSpecifiers.check(ctx, ds)
		return n.Declarator.check(ctx, ds, ds.typ(), false, false)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *AbstractDeclaratorOpt) check(ctx *context, ds *DeclarationSpecifier, t Type) {
	if n == nil {
		return
	}

	n.AbstractDeclarator.check(ctx, ds, t)
}

func (n *AbstractDeclarator) check(ctx *context, ds *DeclarationSpecifier, t Type) Type {
	n.DeclarationSpecifier = ds
	switch n.Case {
	case AbstractDeclaratorPointer: // Pointer
		n.Type = n.Pointer.check(ctx, t, &n.TypeQualifiers)
	case AbstractDeclaratorAbstract: // PointerOpt DirectAbstractDeclarator
		t = n.PointerOpt.check(ctx, t, &n.TypeQualifiers)
		n.Type = n.DirectAbstractDeclarator.check(ctx, t)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	return n.Type
}

func (n *DirectAbstractDeclarator) check(ctx *context, t Type) Type {
	switch n.Case {
	case DirectAbstractDeclaratorAbstract: // '(' AbstractDeclarator ')'
		return n.AbstractDeclarator.check(ctx, nil, t)
	//TODO case DirectAbstractDeclaratorParamList: // '(' ParameterTypeListOpt ')'
	case DirectAbstractDeclaratorDFn: // DirectAbstractDeclarator '(' ParameterTypeListOpt ')'
		fp, variadic := n.ParameterTypeListOpt.check(ctx)
		t := &FunctionType{
			Params:   fp,
			Result:   t,
			Variadic: variadic,
		}
		return n.DirectAbstractDeclarator.check(ctx, t)
	//TODO case DirectAbstractDeclaratorDArrSize: // DirectAbstractDeclaratorOpt '[' "static" TypeQualifierListOpt Expr ']'
	//TODO case DirectAbstractDeclaratorDArrVL: // DirectAbstractDeclaratorOpt '[' '*' ']'
	//TODO case DirectAbstractDeclaratorDArr: // DirectAbstractDeclaratorOpt '[' ExprOpt ']'
	//TODO case DirectAbstractDeclaratorDArrSize2: // DirectAbstractDeclaratorOpt '[' TypeQualifierList "static" Expr ']'
	//TODO case DirectAbstractDeclaratorDArr2: // DirectAbstractDeclaratorOpt '[' TypeQualifierList ExprOpt ']'
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ParameterTypeListOpt) check(ctx *context) ([]Type, bool) {
	if n == nil {
		return nil, false
	}

	return n.ParameterTypeList.check(ctx)
}

func (n *DeclarationSpecifiers) check(ctx *context, ds *DeclarationSpecifier) {
	switch n.Case {
	//TODO case DeclarationSpecifiersFunc: // FunctionSpecifier DeclarationSpecifiersOpt
	case DeclarationSpecifiersStorage: // StorageClassSpecifier DeclarationSpecifiersOpt
		n.StorageClassSpecifier.check(ctx, ds)
	case DeclarationSpecifiersQualifier: // TypeQualifier DeclarationSpecifiersOpt
		n.TypeQualifier.check(ctx, ds)
	case DeclarationSpecifiersSpecifier: // TypeSpecifier DeclarationSpecifiersOpt
		n.TypeSpecifier.check(ctx, ds)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	n.DeclarationSpecifiersOpt.check(ctx, ds)
}

func (n *TypeQualifier) check(ctx *context, ds *DeclarationSpecifier) {
	switch n.Case {
	case
		TypeQualifierConst,    // "const"
		TypeQualifierRestrict, // "restrict"
		TypeQualifierVolatile: // "volatile"

		//nop
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	ds.TypeQualifiers = append(ds.TypeQualifiers, n)
}

func (n *TypeSpecifier) check(ctx *context, ds *DeclarationSpecifier) {
	switch n.Case {
	//TODO case TypeSpecifierBool: // "_Bool"
	//TODO case TypeSpecifierComplex: // "_Complex"
	case
		TypeSpecifierChar,     // "char"
		TypeSpecifierDouble,   // "double"
		TypeSpecifierFloat,    // "float"
		TypeSpecifierInt,      // "int"
		TypeSpecifierLong,     // "long"
		TypeSpecifierName,     // TYPEDEF_NAME
		TypeSpecifierShort,    // "short"
		TypeSpecifierSigned,   // "signed"
		TypeSpecifierUnsigned, // "unsigned"
		TypeSpecifierVoid:     // "void"

		// nop
	case TypeSpecifierEnum: // EnumSpecifier
		n.EnumSpecifier.check(ctx)
	case TypeSpecifierStruct: // StructOrUnionSpecifier
		n.StructOrUnionSpecifier.check(ctx)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	ds.TypeSpecifiers = append(ds.TypeSpecifiers, n)
}

func (n *EnumSpecifier) check(ctx *context) { // [0]6.7.2.2
	switch n.Case {
	//TODO case EnumSpecifierTag: // "enum" IDENTIFIER
	case EnumSpecifierDefine: // "enum" IdentifierOpt '{' EnumeratorList CommaOpt '}'
		t := n.EnumeratorList.check(ctx, n.scope)
		var max uint64
		for i, v := range t.Enums {
			w := v.Operand.Value.(*ir.Int64Value).Value
			u := uint64(w)
			if i < 0 {
				u = uint64(-w)
			}
			if u > max {
				max = u
			}
		}
		x := newIntConst(ctx, n, max, Char, Int, UInt, Long, ULong, LongLong, ULongLong)
		for i := range t.Enums {
			t.Enums[i].Operand.Type = x.Type
		}
		if n.IdentifierOpt != nil {
			n.scope.insertEnumTag(ctx, n.IdentifierOpt.Token.Val, t)
		}
		n.typ = t
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *EnumeratorList) check(ctx *context, s *scope) *EnumType {
	r := &EnumType{}
	iota := int64(-1)
	for ; n != nil; n = n.EnumeratorList {
		r.Enums = append(r.Enums, n.Enumerator.check(ctx, s, &iota))
	}
	return r
}

func (n *Enumerator) check(ctx *context, s *scope, iota *int64) *EnumerationConstant {
	c := n.EnumerationConstant
	switch n.Case {
	case EnumeratorBase: // EnumerationConstant
		*iota++
		c.Operand = &Operand{Value: &ir.Int64Value{Value: *iota}}
		s.insertEnumerationConstant(ctx, c)
		return c
	case EnumeratorInit: // EnumerationConstant '=' ConstExpr
		c.Operand = n.ConstExpr.eval(ctx)
		s.insertEnumerationConstant(ctx, c)
		switch x := c.Operand.Value.(type) {
		case *ir.Int64Value:
			*iota = x.Value
		default:
			panic(ctx.position(n))
		}
		return c
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *StructOrUnionSpecifier) check(ctx *context) {
	switch n.Case {
	case StructOrUnionSpecifierTag: // StructOrUnion IDENTIFIER
		switch n.StructOrUnion.Case {
		case StructOrUnionStruct:
			n.typ = &TaggedStruct{Tag: n.Token.Val}
		default:
			panic(ctx.position(n))
		}
	case StructOrUnionSpecifierEmpty: // StructOrUnion IdentifierOpt '{' '}'
		if n.IdentifierOpt != nil {
			panic(ctx.position(n)) // declare tag
		}
		switch n.StructOrUnion.Case {
		case StructOrUnionStruct:
			n.typ = &StructType{}
		default:
			panic(ctx.position(n))
		}
	case StructOrUnionSpecifierDefine: // StructOrUnion IdentifierOpt '{' StructDeclarationList '}'
		switch n.StructOrUnion.Case {
		case StructOrUnionStruct:
			n.typ = &StructType{Fields: n.StructDeclarationList.check(ctx)}
		default:
			n.typ = &UnionType{Fields: n.StructDeclarationList.check(ctx)}
		}
		if n.IdentifierOpt != nil {
			n.scope.parent.insertStructTag(ctx, n)
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *StructDeclarationList) check(ctx *context) (r []Field) {
	for ; n != nil; n = n.StructDeclarationList {
		r = append(r, n.StructDeclaration.check(ctx)...)
	}
	return r
}

func (n *StructDeclaration) check(ctx *context) []Field {
	// SpecifierQualifierList StructDeclaratorList ';'
	ds := &DeclarationSpecifier{}
	n.SpecifierQualifierList.check(ctx, ds)
	return n.StructDeclaratorList.check(ctx, ds)
}

func (n *StructDeclaratorList) check(ctx *context, ds *DeclarationSpecifier) (r []Field) {
	for ; n != nil; n = n.StructDeclaratorList {
		r = append(r, n.StructDeclarator.check(ctx, ds))
	}
	return r
}

func (n *StructDeclarator) check(ctx *context, ds *DeclarationSpecifier) Field {
	switch n.Case {
	case StructDeclaratorBase: // Declarator
		return Field{Type: n.Declarator.check(ctx, ds, ds.typ(), false, false), Name: n.Declarator.nm()}
	//TODO case StructDeclaratorBits: // DeclaratorOpt ':' ConstExpr
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *SpecifierQualifierList) check(ctx *context, ds *DeclarationSpecifier) {
	switch n.Case {
	case SpecifierQualifierListQualifier: // TypeQualifier SpecifierQualifierListOpt
		n.TypeQualifier.check(ctx, ds)
		n.SpecifierQualifierListOpt.check(ctx, ds)
	case SpecifierQualifierListSpecifier: // TypeSpecifier SpecifierQualifierListOpt
		n.TypeSpecifier.check(ctx, ds)
		n.SpecifierQualifierListOpt.check(ctx, ds)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *SpecifierQualifierListOpt) check(ctx *context, ds *DeclarationSpecifier) {
	if n == nil {
		return
	}

	n.SpecifierQualifierList.check(ctx, ds)
}

func (n *StorageClassSpecifier) check(ctx *context, ds *DeclarationSpecifier) {
	if len(ds.StorageClassSpecifiers) != 0 {
		panic("TODO") // [0]6.7.1-2
	}
	ds.StorageClassSpecifiers = []*StorageClassSpecifier{n}
}

func (n *DeclarationSpecifiersOpt) check(ctx *context, ds *DeclarationSpecifier) {
	if n == nil {
		return
	}

	n.DeclarationSpecifiers.check(ctx, ds)
}
