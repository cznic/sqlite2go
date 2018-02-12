// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

// [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

import (
	"fmt"
	"go/token"
	"math/big"
	"sort"
	"strconv"
	"strings"

	"github.com/cznic/ir"
)

// Node represents an AST node.
type Node interface {
	Pos() token.Pos
}

// Pos implements Node.
func (n *TranslationUnit) Pos() token.Pos { return token.Pos(0) }

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
			r := &NamedType{Name: ts.Token.Val}
			switch x := ts.scope.LookupIdent(ts.Token.Val).(type) {
			case *Declarator:
				if !x.DeclarationSpecifier.IsTypedef() {
					panic("internal error 1")
				}

				r.Type = x.Type
				t := r.Type
				for {
					switch x := t.(type) {
					case
						*ArrayType,
						*FunctionType,
						*StructType,
						*UnionType:

						return r
					case *NamedType:
						t = x.Type
					case *PointerType:
						t = x.Item
					case *TaggedStructType:
						x.getType()
						return r
					case *TaggedUnionType:
						x.getType()
						return r
					case TypeKind:
						switch x {
						case
							Char,
							Double,
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

							return r
						default:
							panic(x)
						}
					default:
						panic(fmt.Errorf("%T %v", x, x))
					}
				}
			default:
				panic(fmt.Errorf("%T", x))
			}
			return r
		case TypeSpecifierShort:
			return Short
		case TypeSpecifierStruct:
			t := d.TypeSpecifiers[0].StructOrUnionSpecifier.typ
			switch x := t.(type) {
			case *TaggedStructType:
				x.getType()
			case *TaggedUnionType:
				x.getType()
			}
			return t
		case TypeSpecifierSigned:
			return Int
		case TypeSpecifierUnsigned:
			return UInt
		case TypeSpecifierVoid:
			return Void
		case TypeSpecifierEnum:
			t := d.TypeSpecifiers[0].EnumSpecifier.typ
			if x, ok := t.(*TaggedEnumType); ok {
				x.getType()
			}
			return t
		default:
			panic(d.typeSpecifiers)
		}
	}

	switch {
	case d.is(TypeSpecifierChar, TypeSpecifierSigned):
		return SChar
	case d.is(TypeSpecifierChar, TypeSpecifierUnsigned):
		return UChar
	case d.is(TypeSpecifierDouble, TypeSpecifierLong):
		return LongDouble
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
	case d.is(TypeSpecifierInt, TypeSpecifierShort):
		return Short
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
	if d == nil {
		return false
	}

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

// IsTypedef return true when the storage class specifier "typedef" is present.
func (d *DeclarationSpecifier) IsTypedef() bool {
	if d == nil {
		return false
	}

	for _, v := range d.StorageClassSpecifiers {
		if v.Case == StorageClassSpecifierTypedef {
			return true
		}
	}
	return false
}

// IsStatic return true when the storage class specifier "static" is present.
func (d *DeclarationSpecifier) IsStatic() bool {
	if d == nil {
		return false
	}

	for _, v := range d.StorageClassSpecifiers {
		if v.Case == StorageClassSpecifierStatic {
			return true
		}
	}
	return false
}

func (d *DeclarationSpecifier) isExtern() bool {
	if d == nil {
		return false
	}

	for _, v := range d.StorageClassSpecifiers {
		if v.Case == StorageClassSpecifierExtern {
			return true
		}
	}
	return false
}

func (n *ConstExpr) eval(ctx *context) Operand {
	if n.Operand.Type == nil {
		n.Operand = n.Expr.eval(ctx, true, nil)
		if n.Operand.Value == nil { // not a constant expression
			panic("TODO")
		}
	}
	return n.Operand
}

func (n *Expr) eval(ctx *context, arr2ptr bool, fn *Declarator) Operand {
	if n.Operand.Type != nil {
		return n.Operand
	}

	switch n.Case {
	case ExprPreInc: // "++" Expr
		// [0]6.5.3.1
		//
		// The operand of the prefix increment or decrement operator
		// shall have qualified or unqualified real or pointer type and
		// shall be a modifiable lvalue.
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn)
		if !n.Operand.isScalarType() {
			panic(ctx.position(n))
		}
	case ExprPreDec: // "--" Expr
		// [0]6.5.3.1
		//
		// The operand of the prefix increment or decrement operator
		// shall have qualified or unqualified real or pointer type and
		// shall be a modifiable lvalue.
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn)
		if !n.Operand.isScalarType() {
			panic(ctx.position(n))
		}
	case ExprSizeofType: // "sizeof" '(' TypeName ')'
		t := n.TypeName.check(ctx)
		n.Operand = ctx.sizeof(t)
	case ExprSizeofExpr: // "sizeof" Expr
		// [0]6.5.3.4
		switch t := n.Expr.eval(ctx, false, fn).Type.(type) { // [0]6.3.2.1-3
		case *ArrayType:
			n.Operand = t.Size.mul(ctx, ctx.sizeof(t.Item))
		case
			*PointerType,
			*StructType,
			*TaggedStructType,
			*UnionType:

			n.Operand = ctx.sizeof(t)
		case *NamedType:
			n.Operand = ctx.sizeof(t.Type)
		case TypeKind:
			switch t {
			case
				Char,
				Double,
				Float,
				Int,
				Long,
				LongLong,
				Short,
				UChar,
				UInt,
				ULong,
				ULongLong:

				n.Operand = ctx.sizeof(t)
			default:
				panic(t)
			}
		default:
			//dbg("", ctx.position(n))
			panic(t)
		}
		if n.Operand.Value == nil {
			panic("TODO")
		}
	case ExprNot: // '!' Expr
		n.Operand = Operand{Type: Int}
		a := n.Expr.eval(ctx, arr2ptr, fn)
		if a.IsZero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
			break
		}

		if a.IsNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
		}
	case ExprAddrof: // '&' Expr
		// [0]6.5.3.2
		op := n.Expr.eval(ctx, false, fn) // [0]6.3.2.1-3
		n.Operand = Operand{Type: &PointerType{op.Type}}
		if d := n.Expr.Declarator; d != nil && n.Expr.Case != ExprPSelect {
			d.AddressTaken = true
		}
		n.Operand.Value = op.Value
	case ExprPExprList: // '(' ExprList ')'
		n.Operand = n.ExprList.eval(ctx, arr2ptr, fn)
	//TODO case ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	case ExprCast: // '(' TypeName ')' Expr
		// [0]6.5.4
		t := n.TypeName.check(ctx)
		op := n.Expr.eval(ctx, arr2ptr, fn)
		if t == Void {
			n.Operand = Operand{Type: Void}
			break
		}

		if !t.IsScalarType() {
			panic(ctx.position(n))
		}

		if !op.isScalarType() {
			panic(ctx.position(n))
		}

	more:
		switch x := t.(type) {
		case *PointerType:
			//dbg("", ctx.position(n), t, op)
			n.Operand = op.convertTo(ctx.model, t)
		case *NamedType:
			t = x.Type
			goto more
		case TypeKind:
			switch x {
			case
				Char,
				Double,
				Int,
				Long,
				LongDouble,
				LongLong,
				SChar,
				Short,
				UChar,
				UInt,
				ULong,
				ULongLong,
				UShort:

				//dbg("", ctx.position(n), t, op)
				n.Operand = op.convertTo(ctx.model, t)
			default:
				panic(x)
			}
		default:
			panic(x)
		}
		if n.Expr.Operand.Value != nil {
			op := n.Expr.Operand.convertTo(ctx.model, t)
			n.Operand.Value = op.Value
		}
	case ExprDeref: // '*' Expr
		// [0]6.5.3
		op := n.Expr.eval(ctx, arr2ptr, fn)
		for t, done := op.Type, false; !done; {
			switch x := t.(type) {
			case *NamedType:
				t = x.Type
			case *PointerType:
				n.Operand = Operand{Type: x.Item}
				done = true
			default:
				panic(ctx.position(n))
			}
		}
	case ExprUnaryPlus: // '+' Expr
		// [0]6.5.3.3
		// The operand of the unary + or - operator shall have
		// arithmetic type; of the ~ operator, integer type; of the !
		// operator, scalar type.
		op := n.Expr.eval(ctx, arr2ptr, fn)
		if !op.isArithmeticType() {
			panic(ctx.position(n))
		}
		n.Operand = op.integerPromotion(ctx.model)
	case ExprUnaryMinus: // '-' Expr
		// [0]6.5.3.3
		// The operand of the unary + or - operator shall have
		// arithmetic type; of the ~ operator, integer type; of the !
		// operator, scalar type.
		op := n.Expr.eval(ctx, arr2ptr, fn)
		if !op.isArithmeticType() {
			panic(ctx.position(n))
		}
		n.Operand = op.unaryMinus(ctx)
	case ExprCpl: // '~' Expr
		// [0]6.5.3.3
		// The operand of the unary + or - operator shall have
		// arithmetic type; of the ~ operator, integer type; of the !
		// operator, scalar type.
		op := n.Expr.eval(ctx, arr2ptr, fn).integerPromotion(ctx.model)
		if !op.Type.IsIntegerType() {
			panic(ctx.position(n))
		}
		n.Operand = op.cpl(ctx)
	case ExprChar: // CHARCONST
		n.Operand = charConst(n.Token.S())
	case ExprNe: // Expr "!=" Expr
		lhs := n.Expr.eval(ctx, arr2ptr, fn)
		rhs := n.Expr2.eval(ctx, arr2ptr, fn)
		// [0]6.5.9
		switch {
		// One of the following shall hold:
		case
			// both operands have arithmetic type
			lhs.isArithmeticType() && rhs.isArithmeticType():

			n.Operand = lhs.ne(ctx, rhs)
		case
			// both operands are pointers to qualified or unqualified versions of compatible types
			lhs.isPointerType() && rhs.isPointerType() && lhs.Type.IsCompatible(rhs.Type):

			n.Operand = Operand{Type: Int}
		case
			// one operand is a pointer and the other is a null
			// pointer constant.
			lhs.isPointerType() && rhs.isIntegerType() && rhs.IsZero():

			n.Operand = Operand{Type: Int}
		default:
			panic(fmt.Errorf("%v: %v %v", ctx.position(n), lhs, rhs))
		}
	case ExprModAssign: // Expr "%=" Expr
		// [0]6.5.16.2
		n.Expr.eval(ctx, arr2ptr, fn).mod(ctx, n, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprLAnd: // Expr "&&" Expr
		n.Operand = Operand{Type: Int}
		a := n.Expr.eval(ctx, arr2ptr, fn)
		if a.IsZero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
			break
		}

		b := n.Expr2.eval(ctx, arr2ptr, fn)
		if b.IsZero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
			break
		}

		if a.IsNonzero() && b.IsNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
		}
	case ExprAndAssign: // Expr "&=" Expr
		n.Expr.eval(ctx, arr2ptr, fn).and(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprMulAssign: // Expr "*=" Expr
		// [0]6.5.16.2
		n.Expr.eval(ctx, arr2ptr, fn).mul(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprPostInc: // Expr "++"
		// [0]6.5.2.4
		//
		// The operand of the postfix increment or decrement operator
		// shall have qualified or unqualified real or pointer type and
		// shall be a modifiable lvalue.
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn)
		if !n.Operand.isScalarType() {
			panic(ctx.position(n))
		}
	case ExprAddAssign: // Expr "+=" Expr
		// [0]6.5.16.2
		//
		// 1. For the operators += and -= only, either the left operand
		// shall be a pointer to an object type and the right shall
		// have integer type, or the left operand shall have qualified
		// or unqualified arithmetic type and the right shall have
		// arithmetic type.
		lhs := n.Expr.eval(ctx, arr2ptr, fn)
		rhs := n.Expr2.eval(ctx, arr2ptr, fn)
		switch {
		case
			lhs.isPointerType() && rhs.isIntegerType(),
			lhs.isArithmeticType() && rhs.isArithmeticType():

			// ok
		default:
			panic(ctx.position(n))
		}
		n.Operand = lhs
	case ExprPostDec: // Expr "--"
		// [0]6.5.2.4
		//
		// The operand of the postfix increment or decrement operator
		// shall have qualified or unqualified real or pointer type and
		// shall be a modifiable lvalue.
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn)
		if !n.Operand.isScalarType() {
			panic(ctx.position(n))
		}
	case ExprSubAssign: // Expr "-=" Expr
		// [0]6.5.16.2
		//
		// 1. For the operators += and -= only, either the left operand
		// shall be a pointer to an object type and the right shall
		// have integer type, or the left operand shall have qualified
		// or unqualified arithmetic type and the right shall have
		// arithmetic type.
		lhs := n.Expr.eval(ctx, arr2ptr, fn)
		rhs := n.Expr2.eval(ctx, arr2ptr, fn)
		switch {
		case
			lhs.isPointerType() && rhs.isIntegerType(),
			lhs.isArithmeticType() && rhs.isArithmeticType():

			// ok
		default:
			panic(ctx.position(n))
		}
		n.Operand = lhs
	case ExprPSelect: // Expr "->" IDENTIFIER
		n.Expr.AssignedTo = n.AssignedTo
		op := n.Expr.eval(ctx, arr2ptr, fn)
		if d := n.Expr.Declarator; d != nil && n.AssignedTo {
			d.AssignedTo++
		}
		t := op.Type
		for done := false; !done; {
			switch x := t.(type) {
			case *NamedType:
				t = x.Type
			case *PointerType:
				t = x.Item
				done = true
			default:
				panic(fmt.Errorf("%T", x))
			}
		}
	out:
		for {
			switch x := t.(type) {
			case *NamedType:
				t = x.Type
			case *StructType:
				nm := n.Token2.Val
				d0, ok := x.scope.Idents[nm]
				if !ok {
					panic(ctx.position(n))
				}
				d := d0.(*Declarator)
				fp := ctx.model.Layout(x)[d.Field]
				n.Operand = Operand{Type: x.Fields[d.Field].Type}
				n.Operand.Bits = x.Fields[d.Field].Bits
				n.Operand.PackedType = fp.PackedType
				n.Operand.Bitoff = fp.Bitoff
				if op.Value == Null {
					n.Operand.Value = &ir.Int64Value{Value: fp.Offset}
				}
				break out
			case *TaggedStructType:
				t = x.getType()
				if t == x {
					panic("TODO")
				}
			case *UnionType:
				nm := n.Token2.Val
				d0, ok := x.scope.Idents[nm]
				if !ok {
					panic(ctx.position(n))
				}
				d := d0.(*Declarator)
				fp := ctx.model.Layout(x)[d.Field]
				n.Operand = Operand{Type: x.Fields[d.Field].Type}
				n.Operand.Bits = x.Fields[d.Field].Bits
				n.Operand.PackedType = fp.PackedType
				n.Operand.Bitoff = fp.Bitoff
				if op.Value == Null {
					n.Operand.Value = &ir.Int64Value{}
				}
				break out
			default:
				panic(x)
			}
		}
	case ExprDivAssign: // Expr "/=" Expr
		// [0]6.5.16.2
		n.Expr.eval(ctx, arr2ptr, fn).div(ctx, n, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprLsh: // Expr "<<" Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).lsh(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprLshAssign: // Expr "<<=" Expr
		n.Expr.eval(ctx, arr2ptr, fn).lsh(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprLe: // Expr "<=" Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).le(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprEq: // Expr "==" Expr
		lhs := n.Expr.eval(ctx, arr2ptr, fn)
		rhs := n.Expr2.eval(ctx, arr2ptr, fn)
		// [0]6.5.9
		switch {
		// One of the following shall hold:
		case
			// both operands have arithmetic type
			lhs.isArithmeticType() && rhs.isArithmeticType():

			n.Operand = lhs.eq(ctx, rhs)
		case
			// both operands are pointers to qualified or unqualified versions of compatible types
			lhs.isPointerType() && rhs.isPointerType() && lhs.Type.IsCompatible(rhs.Type):

			n.Operand = Operand{Type: Int}
		case
			// one operand is a pointer and the other is a null
			// pointer constant.
			lhs.isPointerType() && rhs.isIntegerType() && rhs.IsZero():

			n.Operand = Operand{Type: Int}
		case
			// one operand is a pointer and the other is a null
			// pointer constant.
			lhs.isIntegerType() && lhs.IsZero() && rhs.isPointerType():

			n.Operand = Operand{Type: Int}
		default:
			panic(fmt.Errorf("%v: %v %v", ctx.position(n), lhs, rhs))
		}
	case ExprGe: // Expr ">=" Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).ge(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprRsh: // Expr ">>" Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).rsh(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprRshAssign: // Expr ">>=" Expr
		n.Expr.eval(ctx, arr2ptr, fn).rsh(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprXorAssign: // Expr "^=" Expr
		n.Expr.eval(ctx, arr2ptr, fn).xor(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprOrAssign: // Expr "|=" Expr
		n.Expr.eval(ctx, arr2ptr, fn).or(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		n.Operand = n.Expr.Operand
	case ExprLOr: // Expr "||" Expr
		n.Operand = Operand{Type: Int}
		a := n.Expr.eval(ctx, arr2ptr, fn)
		if a.IsNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
			break
		}

		b := n.Expr2.eval(ctx, arr2ptr, fn)
		if b.IsNonzero() {
			n.Operand.Value = &ir.Int64Value{Value: 1}
			break
		}

		if a.IsZero() && b.IsZero() {
			n.Operand.Value = &ir.Int64Value{Value: 0}
		}
	case ExprMod: // Expr '%' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).mod(ctx, n, n.Expr2.eval(ctx, arr2ptr, fn)) // [0]6.5.5
	case ExprAnd: // Expr '&' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).and(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprCall: // Expr '(' ArgumentExprListOpt ')'
		// [0]6.5.2.2
		op := n.Expr.eval(ctx, arr2ptr, fn)
		args := n.ArgumentExprListOpt.eval(ctx, fn)
		ops := make([]Operand, len(args))
		n.CallArgs = ops
		t := checkFn(ctx, op.Type)
		if t == nil {
			if !ctx.tweaks.EnableImplicitDeclarations {
				panic(ctx.position(n))
			}

			if n.Expr.Case == ExprIdent && n.Expr.Token.Val == idAsm {
				panic(ctx.position(n).String())
			}

			n.Operand = Operand{Type: Int}
			break
		}

		if _, ok := t.Result.(*ArrayType); ok {
			panic(ctx.position)
		}

		n.Operand = Operand{Type: t.Result}
		// 2. If the expression that denotes the called function has a
		// type that includes a prototype, the number of arguments
		// shall agree with the number of parameters. Each argument
		// shall have a type such that its value may be assigned to an
		// object with the unqualified version of the type of its
		// corresponding parameter.
	out2:
		switch {
		case t.Variadic:
			switch {
			case len(args) >= len(t.Params):
				// ok
			default:
				panic(ctx.position(n))
			}
			for i, rhs := range args {
				if i >= len(t.Params) {
					ops[i] = ctx.model.defaultArgumentPromotion(rhs)
					continue
				}

				ops[i] = AdjustedParameterType(t.Params[i]).assign(ctx, rhs)
			}
		default:
			switch {
			case len(args) == len(t.Params):
				// ok
			case len(args) == 0 && len(t.Params) == 1 && t.Params[0] == Void:
				// ok
			case len(t.Params) == 0:
				break out2
			default:
				panic(ctx.position(n))
			}

			for i, rhs := range args {
				ops[i] = AdjustedParameterType(t.Params[i]).assign(ctx, rhs)
			}
			break out2
		}
		for i, v := range ops {
			if v.Value != nil {
				ops[i] = v.convertTo(ctx.model, ops[i].Type)
			}
		}
		if o := n.ArgumentExprListOpt; o != nil {
			i := 0
			for l := o.ArgumentExprList; l != nil; l = l.ArgumentExprList {
				if i >= len(ops) {
					break
				}

				if op := ops[i]; op.Value != nil {
					l.Expr.Operand = op
				}
				i++
			}
		}
	case ExprMul: // Expr '*' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).mul(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprAdd: // Expr '+' Expr
		lhs := n.Expr.eval(ctx, arr2ptr, fn)
		rhs := n.Expr2.eval(ctx, arr2ptr, fn)
		// [0]6.5.6
		//
		// For addition, either both operands shall have arithmetic
		// type, or one operand shall be a pointer to an object type
		// and the other shall have integer type. (Incrementing is
		// equivalent to adding 1.)
		switch {
		case lhs.isArithmeticType() && rhs.isArithmeticType():
			n.Operand = n.Expr.eval(ctx, arr2ptr, fn).add(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		case lhs.isPointerType() && rhs.isIntegerType():
			n.Operand = lhs
			n.Operand.Value = nil
		case lhs.isIntegerType() && rhs.isPointerType():
			n.Operand = rhs
			n.Operand.Value = nil
		default:
			panic(ctx.position(n))
		}
	case ExprSub: // Expr '-' Expr
		// [0]6.5.6
		lhs := n.Expr.eval(ctx, arr2ptr, fn)
		rhs := n.Expr2.eval(ctx, arr2ptr, fn)
		switch {
		// 3. For subtraction, one of the following shall hold:
		case
			// both operands have arithmetic type;
			lhs.isArithmeticType() && rhs.isArithmeticType():

			n.Operand = n.Expr.eval(ctx, arr2ptr, fn).sub(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
		case
			// both operands are pointers to qualified or
			// unqualified versions of compatible object types;
			lhs.isPointerType() && rhs.isPointerType() && lhs.Type.IsCompatible(rhs.Type):

			n.Operand = Operand{Type: ctx.ptrDiff()}
		case
			// the left operand is a pointer to an object type and
			// the right operand has integer type.
			lhs.isPointerType() && rhs.isIntegerType():

			n.Operand = lhs
		default:
			//dbg("", lhs, rhs)
			panic(ctx.position(n))
		}
	case ExprSelect: // Expr '.' IDENTIFIER
		n.Expr.AssignedTo = n.AssignedTo
		op := n.Expr.eval(ctx, arr2ptr, fn)
		if d := n.Expr.Declarator; d != nil && n.AssignedTo {
			d.AssignedTo++
		}
		t := op.Type
	out3:
		for {
			switch x := t.(type) {
			case *NamedType:
				t = x.Type
			case *StructType:
				nm := n.Token2.Val
				d0, ok := x.scope.Idents[nm]
				if !ok {
					panic(ctx.position(n))
				}
				d := d0.(*Declarator)
				f := x.Fields[d.Field]
				fp := ctx.model.Layout(x)[d.Field]
				n.Operand = Operand{Type: f.Type}
				n.Operand.Bits = f.Bits
				n.Operand.PackedType = fp.PackedType
				n.Operand.Bitoff = fp.Bitoff
				if d := n.Expr.Declarator; d != nil && (n.Operand.Type.Kind() == Array || fp.Bits != 0) {
					d.AddressTaken = true
				}
				break out3
			case *TaggedStructType:
				y := x.getType()
				if x == y {
					panic("TODO")
				}
				t = y
			case *TaggedUnionType:
				y := x.getType()
				if x == y {
					panic("TODO")
				}
				t = y
			case *UnionType:
				nm := n.Token2.Val
				d0, ok := x.scope.Idents[nm]
				if !ok {
					panic(ctx.position(n))
				}
				d := d0.(*Declarator)
				f := x.Fields[d.Field]
				fp := ctx.model.Layout(x)[d.Field]
				n.Operand = Operand{Type: f.Type}
				n.Operand.Bits = f.Bits
				n.Operand.PackedType = fp.PackedType
				n.Operand.Bitoff = fp.Bitoff
				if d := n.Expr.Declarator; d != nil && (n.Operand.Type.Kind() == Array || fp.Bits != 0) {
					d.AddressTaken = true
				}
				break out3
			default:
				//dbg("%v: %T", ctx.position(n), x)
				panic(x)
			}
		}
		if d := n.Expr.Declarator; d != nil {
			d.Referenced++
		}
	case ExprDiv: // Expr '/' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).div(ctx, n, n.Expr2.eval(ctx, arr2ptr, fn)) // [0]6.5.5
	case ExprLt: // Expr '<' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).lt(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprAssign: // Expr '=' Expr
		n.Expr.AssignedTo = true
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn)
		if d := n.Expr.Declarator; d != nil {
			d.AssignedTo++
			if n.Expr.Case == ExprIdent {
				n.Operand.Type = d.Type
			}
		}
		n.Operand.Type.assign(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprGt: // Expr '>' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).gt(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprCond: // Expr '?' ExprList ':' Expr
		// [0]6.5.15
		cond := n.Expr.eval(ctx, arr2ptr, fn)
		a := n.ExprList.eval(ctx, arr2ptr, fn)
		b := n.Expr2.eval(ctx, arr2ptr, fn)
		// 2. The first operand shall have scalar type.
		if !cond.isScalarType() {
			panic(ctx.position(n))
		}
		done := false
		switch {
		// 3. One of the following shall hold for the second and third
		// operands:
		case
			// both operands have arithmetic type;
			a.isArithmeticType() && b.isArithmeticType():

			// 5. If both the second and third operands have
			// arithmetic type, the result type that would be
			// determined by the usual arithmetic conversions, were
			// they applied to those two operands, is the type of
			// the result.
			x, _ := UsualArithmeticConversions(ctx.model, a, b)
			n.Operand = Operand{Type: x.Type}
		case
			// both operands have the same structure or union type
			a.Type.Kind() == Struct && b.Type.Kind() == Struct && a.Type.Equal(b.Type),
			a.Type.Kind() == Union && b.Type.Kind() == Union && a.Type.Equal(b.Type):

			n.Operand = Operand{Type: a.Type}
		case
			// both operands are pointers to qualified or
			// unqualified versions of compatible types;
			a.isPointerType() && b.isPointerType() && a.Type.IsCompatible(b.Type):

			n.Operand = Operand{Type: a.Type}
		case
			// one operand is a pointer and the other is a null pointer constant
			a.isIntegerType() && a.IsZero() && b.isPointerType():

			n.Operand = Operand{Type: b.Type}
			if cond.IsNonzero() {
				n.Operand.Value = Null
				done = true
			}
		case
			// one operand is a pointer and the other is a null pointer constant
			a.isPointerType() && b.isIntegerType() && b.IsZero():

			n.Operand = Operand{Type: a.Type}
			if cond.IsZero() {
				n.Operand.Value = Null
				done = true
			}
		default:
			//dbg("", a, b)
			panic(ctx.position(n))
		}
		if done {
			break
		}

		switch {
		case cond.IsNonzero():
			n.Operand = a
		case cond.IsZero():
			n.Operand = b
		}
	case ExprIndex: // Expr '[' ExprList ']'
		// [0]6.5.2.1
		op := n.Expr.eval(ctx, arr2ptr, fn)
		index := n.ExprList.eval(ctx, true, fn)
		switch t := op.Type.(type) {
		case *ArrayType:
			if arr2ptr {
				panic("internal error")
			}
			n.Operand = Operand{Type: t.Item}
		case *PointerType:
			n.Operand = Operand{Type: t.Item}
		default:
			panic(ctx.position(n))
		}
		if !index.isIntegerType() {
			panic("TODO")
		}
		if d := n.Expr.Declarator; d != nil {
			d.Referenced++
		}
	case ExprXor: // Expr '^' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).xor(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprOr: // Expr '|' Expr
		n.Operand = n.Expr.eval(ctx, arr2ptr, fn).or(ctx, n.Expr2.eval(ctx, arr2ptr, fn))
	case ExprFloat: // FLOATCONST
		n.floatConst(ctx)
	case ExprIdent: // IDENTIFIER
		// [0]6.5.1
		nm := n.Token.Val
		if n.Scope.LookupIdent(nm) == nil && ctx.tweaks.EnableImplicitBuiltins {
			nm2 := dict.SID("__builtin_" + string(dict.S(nm)))
			if n.Scope.LookupIdent(nm2) != nil {
				nm = nm2
			} else {
				if !ctx.tweaks.EnableImplicitDeclarations {
					panic(fmt.Errorf("%v", ctx.position(n)))
				}
			}
		}
		switch x := n.Scope.LookupIdent(nm).(type) {
		case *Declarator:
			n.Declarator = x
			if arr2ptr {
				x.Referenced++
			}
			t := x.Type
			t0 := t
		more2:
			switch y := t.(type) {
			case
				*ArrayType,
				*PointerType,
				*StructType,
				*TaggedEnumType,
				*TaggedStructType,
				*TaggedUnionType,
				*UnionType:

				n.Operand = Operand{Type: t0}
			case *FunctionType:
				n.Operand = Operand{Type: t0}
				if nm == idAlloca {
					fn.Alloca = true
				}
			case *NamedType:
				t = y.Type
				goto more2
			case TypeKind:
				switch y {
				case
					Char,
					Double,
					Float,
					Int,
					Long,
					LongDouble,
					LongLong,
					SChar,
					Short,
					UChar,
					UInt,
					ULong,
					ULongLong,
					UShort:

					n.Operand = Operand{Type: t0}
				default:
					//dbg("", ctx.position(n))
					panic(y)
				}
			default:
				//dbg("", ctx.position(n))
				panic(y)
			}
		case *EnumerationConstant:
			n.Operand = x.Operand
		case nil:
			if ctx.tweaks.EnableImplicitDeclarations {
				return Operand{}
			}

			panic(fmt.Errorf("%v: undefined: %s", ctx.position(n), dict.S(nm)))
		default:

			//dbg("%s", dict.S(nm))
			panic(fmt.Errorf("%v: %T", ctx.position(n), x))
		}
	case ExprInt: // INTCONST
		defer func() { //TODO-
			if n.Operand.Type == nil || n.Operand.Value == nil {
				fmt.Printf("TODO1114 %v: %q %v\n", ctx.position(n), n.Token.S(), n.Operand)         //TODO-
				panic(fmt.Sprintf("TODO1114 %v: %q %v\n", ctx.position(n), n.Token.S(), n.Operand)) //TODO-
			}
		}() //TODO-
		s0 := string(dict.S(n.Token.Val))
		s := s0
		if strings.Contains(s, "p") {
			n.Case = ExprFloat
			n.floatConst(ctx)
			break
		}

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
			if ctx.tweaks.EnableBinaryLiterals && (strings.HasPrefix(s, "0b") || strings.HasPrefix(s, "0B")) {
				decadic = false
				v, err = strconv.ParseUint(s[2:], 2, 64)
			}
			if err != nil {
				panic(fmt.Errorf("%v: %v %v", ctx.position(n), n.Case, err))
			}
		}

		// [0]6.4.4.1
		switch suff := strings.ToUpper(s0[len(s):]); {
		case suff == "" && decadic:
			n.Operand = newIntConst(ctx, n, v, Int, Long, LongLong)
		case suff == "" && !decadic:
			n.Operand = newIntConst(ctx, n, v, Int, UInt, Long, ULong, LongLong, ULongLong)
		case suff == "L" && decadic:
			n.Operand = newIntConst(ctx, n, v, Long, LongLong)
		case suff == "L" && !decadic:
			n.Operand = newIntConst(ctx, n, v, Int, UInt, Long, ULong, LongLong, ULongLong)
		case suff == "LL" && decadic:
			n.Operand = newIntConst(ctx, n, v, LongLong)
		case suff == "LL" && !decadic:
			n.Operand = newIntConst(ctx, n, v, LongLong, ULongLong)
		case suff == "U":
			n.Operand = newIntConst(ctx, n, v, UInt, ULong, ULongLong)
		case suff == "UL", suff == "LU":
			n.Operand = newIntConst(ctx, n, v, ULong, ULongLong)
		case suff == "ULL", suff == "LLU":
			n.Operand = newIntConst(ctx, n, v, ULongLong)
		default:
			panic(fmt.Errorf("%v: TODO %q %q decadic: %v\n%s", ctx.position(n), s, suff, decadic, PrettyString(n)))
		}
	//TODO case ExprLChar: // LONGCHARCONST
	//TODO case ExprLString: // LONGSTRINGLITERAL
	case ExprString: // STRINGLITERAL
		n.Operand = strConst(n.Token)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	if !arr2ptr {
		return n.Operand
	}

	// [0]6.3.2.1
	//
	// 3. Except when it is the operand of the sizeof operator or
	// the unary & operator, or is a string literal used to
	// initialize an array, an expression that has type ‘‘array of
	// type’’ is converted to an expression with type ‘‘pointer to
	// type’’ that points to the initial element of the array
	// object and is not an lvalue. If the array object has
	// register storage class, the behavior is undefined.
	t := n.Operand.Type
	for {
		switch x := t.(type) {
		case *ArrayType:
			n.Operand.Type = &PointerType{x.Item}
			return n.Operand
		case *FunctionType:
			n.Operand.Type = &PointerType{x}
			return n.Operand
		case *NamedType:
			t = x.Type
		case
			*PointerType,
			*StructType,
			*TaggedEnumType,
			*TaggedStructType,
			*TaggedUnionType,
			*UnionType:

			return n.Operand
		case TypeKind:
			switch x {
			case
				Char,
				Double,
				Float,
				Int,
				Long,
				LongDouble,
				LongLong,
				SChar,
				Short,
				UChar,
				UInt,
				ULong,
				ULongLong,
				UShort,
				Void:

				return n.Operand
			default:
				//dbg("", ctx.position(n))
				panic(x)
			}
		case nil:
			if ctx.tweaks.EnableImplicitDeclarations {
				return n.Operand
			}

			panic(fmt.Errorf("%v: %T", ctx.position(n), x))
		default:
			//dbg("", ctx.position(n))
			panic(fmt.Errorf("%T", x))
		}
	}
}

func (n *Expr) floatConst(ctx *context) {
	s0 := string(dict.S(n.Token.Val))
	s := s0
loop2:
	for i := len(s) - 1; i > 0; i-- {
		switch s0[i] {
		case 'l', 'L', 'f', 'F':
			s = s[:i]
		default:
			break loop2
		}
	}

	var v float64
	var err error
	switch {
	case strings.Contains(s, "p"):
		var bf *big.Float
		bf, _, err = big.ParseFloat(s, 0, 53, big.ToNearestEven)
		if err == nil {
			v, _ = bf.Float64()
		}
	default:
		v, err = strconv.ParseFloat(s, 64)
	}
	if err != nil {
		panic(ctx.position(n))
	}

	// [0]6.4.4.2
	switch suff := strings.ToUpper(s0[len(s):]); suff {
	case "", "l", "L":
		n.Operand = Operand{Type: Double, Value: &ir.Float64Value{Value: v}}
	case "f", "F":
		n.Operand = Operand{Type: Float, Value: &ir.Float32Value{Value: float32(v)}}
	default:
		panic(fmt.Errorf("%v: TODO %q %q %v", ctx.position(n), s, suff, v))
	}
}

func pitem(t Type) Type {
	switch x := t.(type) {
	case *PointerType:
		return x.Item
	default:
		panic(fmt.Errorf("%T", x))
	}
}

func checkFn(ctx *context, t Type) *FunctionType {
	// 1. The expression that denotes the called function 80) shall
	// have type pointer to function returning void or returning an
	// object type other than an array type.
	for {
		switch x := t.(type) {
		case *FunctionType:
			return x
		case *NamedType:
			t = x.Type
		case *PointerType:
			switch x := x.Item.(type) {
			case *FunctionType:
				return x
			default:
				panic(ctx.position)
			}
		case nil:
			return nil
		default:
			panic(fmt.Errorf("%T", x))
		}
	}
}

func (n *ArgumentExprListOpt) eval(ctx *context, fn *Declarator) []Operand {
	if n == nil {
		return nil
	}

	return n.ArgumentExprList.eval(ctx, fn)
}

func (n *ArgumentExprList) eval(ctx *context, fn *Declarator) (r []Operand) {
	for ; n != nil; n = n.ArgumentExprList {
		r = append(r, n.Expr.eval(ctx, true, fn))
	}
	return r
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

func (n *ExprListOpt) eval(ctx *context, arr2ptr bool, fn *Declarator) Operand {
	if n == nil {
		return Operand{}
	}

	return n.ExprList.eval(ctx, arr2ptr, fn)
}

func (n *ExprList) eval(ctx *context, arr2ptr bool, fn *Declarator) Operand {
	if n.Operand.Type == nil {
		for l := n; l != nil; l = l.ExprList {
			n.Operand = l.Expr.eval(ctx, arr2ptr, fn)
		}
	}
	return n.Operand
}

func (n *ExprOpt) eval(ctx *context, arr2ptr bool, fn *Declarator) Operand {
	if n == nil {
		return Operand{}
	}

	return n.Expr.eval(ctx, arr2ptr, fn)
}

// Name returns the ID of the declared name.
func (n *Declarator) Name() int { return n.DirectDeclarator.nm() }

func (n *DirectDeclarator) nm() int {
	switch n.Case {
	case DirectDeclaratorArray, DirectDeclaratorParamList, DirectDeclaratorIdentList:
		return n.DirectDeclarator.nm()
	case DirectDeclaratorIdent:
		return n.Token.Val
	case DirectDeclaratorParen:
		return n.Declarator.Name()
	default:
		panic(fmt.Errorf("TODO %v", n.Case))
	}
}

func (n *ExternalDeclarationList) check(ctx *context) (err error) {
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

	for ; n != nil; n = n.ExternalDeclarationList {
		n.ExternalDeclaration.check(ctx)
	}
	return nil
}

func (n *ExternalDeclaration) check(ctx *context) {
	switch n.Case {
	case ExternalDeclarationDecl: // Declaration
		n.Declaration.check(ctx, nil, nil)
	case ExternalDeclarationFunc: // FunctionDefinition
		n.FunctionDefinition.check(ctx)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

// LocalVariables returns all local variables declared in a function in the
// order of appearance. The result does not include function parameters.
func (n *FunctionDefinition) LocalVariables() []*Declarator { return n.Declarator.vars }

func (n *FunctionDefinition) check(ctx *context) {
	ds := &DeclarationSpecifier{}
	switch n.Case {
	case FunctionDefinitionSpec: // DeclarationSpecifiers Declarator DeclarationListOpt FunctionBody
		n.DeclarationSpecifiers.check(ctx, ds)
		if len(ds.TypeSpecifiers) == 0 { // [0]6.7.2-2
			panic("TODO")
		}
	case FunctionDefinitionInt: // DeclarationSpecifiers Declarator DeclarationListOpt FunctionBody
		ds.typeSpecifiers = []TypeSpecifierCase{TypeSpecifierInt}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	n.Declarator.check(ctx, ds, ds.typ(), false, nil, nil)
	if n.Declarator.Type.Kind() != Function {
		panic("TODO")
	}
	n.FunctionBody.check(ctx, n.Declarator)
	d := n.Declarator
	sc := d.fpScope(ctx)
	for _, v := range d.ParameterNames() {
		p, _ := sc.Idents[v].(*Declarator)
		d.Parameters = append(d.Parameters, p)
	}
}

func (n *FunctionBody) check(ctx *context, fn *Declarator) {
	// CompoundStmt *CompoundStmt
	seq := -1
	n.CompoundStmt.check(ctx, fn, &seq, nil, nil, false)
}

func (n *CompoundStmt) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	// '{' BlockItemListOpt '}'
	*seq++
	sc = append(sc, *seq)
	if *seq == 0 { // Pull function parameters into the outermost block scope.
		for _, v := range fn.fpScope(ctx).Idents {
			d := v.(*Declarator)
			nm := d.Name()
			if ex := n.scope.Idents[nm]; ex != nil {
				panic("TODO") // redeclared
			}

			n.scope.insertDeclarator(ctx, d)
		}
	}
	n.BlockItemListOpt.check(ctx, fn, seq, sc, inSwitch, inLoop)
}

func (n *Declarator) fpScope(ctx *context) *Scope { return n.DirectDeclarator.fpScope(ctx) }

func (n *DirectDeclarator) fpScope(ctx *context) *Scope {
	switch n.Case {
	//TODO case DirectDeclaratorParen: // '(' Declarator ')'
	case DirectDeclaratorIdentList: // DirectDeclarator '(' IdentifierListOpt ')'
		switch n.DirectDeclarator.Case {
		case DirectDeclaratorIdent:
			return n.paramScope
		default:
			panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.DirectDeclarator.Case))
		}
	case DirectDeclaratorParamList: // DirectDeclarator '(' ParameterTypeList ')'
		switch n.DirectDeclarator.Case {
		case DirectDeclaratorParen:
			if n.DirectDeclarator.Declarator.DirectDeclarator.Case == DirectDeclaratorIdent {
				return n.paramScope
			}

			return n.DirectDeclarator.Declarator.DirectDeclarator.fpScope(ctx)
		case DirectDeclaratorIdent:
			return n.paramScope
		default:
			panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.DirectDeclarator.Case))
		}
	//TODO case DirectDeclaratorArraySize: // DirectDeclarator '[' "static" TypeQualifierListOpt Expr ']'
	//TODO case DirectDeclaratorArraySize2: // DirectDeclarator '[' TypeQualifierList "static" Expr ']'
	//TODO case DirectDeclaratorArrayVar: // DirectDeclarator '[' TypeQualifierListOpt '*' ']'
	//TODO case DirectDeclaratorArray: // DirectDeclarator '[' TypeQualifierListOpt ExprOpt ']'
	//TODO case DirectDeclaratorIdent: // IDENTIFIER
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

// ParameterNames returns a list of IDs of names of parameters of n. The
// function panics if n is not function type.
func (n *Declarator) ParameterNames() []int { return n.DirectDeclarator.parameterNames() }

func (n *DirectDeclarator) parameterNames() (r []int) {
	switch n.Case {
	//TODO case DirectDeclaratorParen: // '(' Declarator ')'
	case DirectDeclaratorIdentList: // DirectDeclarator '(' IdentifierListOpt ')'
		switch n.DirectDeclarator.Case {
		case DirectDeclaratorIdent:
			return n.IdentifierListOpt.check()
		default:
			panic(n.DirectDeclarator.Case)
		}
	case DirectDeclaratorParamList: // DirectDeclarator '(' ParameterTypeList ')'
		switch n.DirectDeclarator.Case {
		case DirectDeclaratorIdent:
			for l := n.ParameterTypeList.ParameterList; l != nil; l = l.ParameterList {
				switch n := l.ParameterDeclaration; n.Case {
				case ParameterDeclarationAbstract: // DeclarationSpecifiers AbstractDeclaratorOpt
					r = append(r, 0)
				case ParameterDeclarationDeclarator: // DeclarationSpecifiers Declarator
					r = append(r, n.Declarator.Name())
				default:
					panic(n.Case)
				}
			}
			return r
		case DirectDeclaratorParen:
			if n.DirectDeclarator.Declarator.DirectDeclarator.Case == DirectDeclaratorIdent {
				for l := n.ParameterTypeList.ParameterList; l != nil; l = l.ParameterList {
					switch n := l.ParameterDeclaration; n.Case {
					case ParameterDeclarationAbstract: // DeclarationSpecifiers AbstractDeclaratorOpt
						r = append(r, 0)
					case ParameterDeclarationDeclarator: // DeclarationSpecifiers Declarator
						r = append(r, n.Declarator.Name())
					default:
						panic(n.Case)
					}
				}
				return r
			}

			return n.DirectDeclarator.Declarator.DirectDeclarator.parameterNames()
		default:
			panic(n.DirectDeclarator.Case)
		}
	//TODO case DirectDeclaratorArraySize: // DirectDeclarator '[' "static" TypeQualifierListOpt Expr ']'
	//TODO case DirectDeclaratorArraySize2: // DirectDeclarator '[' TypeQualifierList "static" Expr ']'
	//TODO case DirectDeclaratorArrayVar: // DirectDeclarator '[' TypeQualifierListOpt '*' ']'
	//TODO case DirectDeclaratorArray: // DirectDeclarator '[' TypeQualifierListOpt ExprOpt ']'
	//TODO case DirectDeclaratorIdent: // IDENTIFIER
	default:
		panic(n.Case)
	}
}

func (n *BlockItemListOpt) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	if n == nil {
		return
	}

	n.BlockItemList.check(ctx, fn, seq, sc, inSwitch, inLoop)
}

func (n *BlockItemList) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	for ; n != nil; n = n.BlockItemList {
		n.BlockItem.check(ctx, fn, seq, sc, inSwitch, inLoop)
	}
}

func (n *BlockItem) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	switch n.Case {
	case BlockItemDecl: // Declaration
		n.Declaration.check(ctx, sc, fn)
	case BlockItemStmt: // Stmt
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *Stmt) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	switch n.Case {
	case StmtBlock: // CompoundStmt
		n.CompoundStmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case StmtExpr: // ExprStmt
		n.ExprStmt.check(ctx, fn)
	case StmtIter: // IterationStmt
		n.IterationStmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case StmtJump: // JumpStmt
		n.JumpStmt.check(ctx, fn, inSwitch, inLoop)
	case StmtLabeled: // LabeledStmt
		n.LabeledStmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case StmtSelect: // SelectionStmt
		n.SelectionStmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *LabeledStmt) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	//[0]6.8.1
	switch n.Case {
	case LabeledStmtSwitchCase: // "case" ConstExpr ':' Stmt
		op := n.ConstExpr.eval(ctx).convertTo(ctx.model, inSwitch.SwitchOp.Type)
		n.ConstExpr.Operand = op
		if op.Value == nil {
			panic("TODO")
		}
		if inSwitch == nil {
			panic("TODO")
		}
		inSwitch.Cases = append(inSwitch.Cases, n)
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case LabeledStmtDefault: // "default" ':' Stmt
		if inSwitch == nil {
			panic("TODO")
		}
		inSwitch.Cases = append(inSwitch.Cases, n)
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case LabeledStmtLabel: // IDENTIFIER ':' Stmt
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *SelectionStmt) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	switch n.Case {
	case SelectionStmtIfElse: // "if" '(' ExprList ')' Stmt "else" Stmt
		if !n.ExprList.eval(ctx, true, fn).isScalarType() {
			panic("TODO")
		}
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
		n.Stmt2.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case SelectionStmtIf: // "if" '(' ExprList ')' Stmt
		if !n.ExprList.eval(ctx, true, fn).isScalarType() {
			panic("TODO")
		}
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, inLoop)
	case SelectionStmtSwitch: // "switch" '(' ExprList ')' Stmt
		// [0]6.8.4.2
		if !n.ExprList.eval(ctx, true, fn).isIntegerType() {
			panic("TODO")
		}
		n.SwitchOp = n.ExprList.Operand.integerPromotion(ctx.model)
		n.Stmt.check(ctx, fn, seq, sc, n, inLoop)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *IterationStmt) check(ctx *context, fn *Declarator, seq *int, sc []int, inSwitch *SelectionStmt, inLoop bool) {
	switch n.Case {
	case IterationStmtDo: // "do" Stmt "while" '(' ExprList ')' ';'
		if !n.ExprList.eval(ctx, true, fn).isScalarType() {
			panic(ctx.position)
		}
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, true)
	//TODO case IterationStmtForDecl: // "for" '(' Declaration ExprListOpt ';' ExprListOpt ')' Stmt
	case IterationStmtFor: // "for" '(' ExprListOpt ';' ExprListOpt ';' ExprListOpt ')' Stmt
		// [0]6.8.5.3
		n.ExprListOpt.eval(ctx, true, fn)
		if e := n.ExprListOpt2.eval(ctx, true, fn); e.Type != nil && !e.isScalarType() {
			panic(ctx.position(n))
		}
		n.ExprListOpt3.eval(ctx, true, fn)
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, true)
	case IterationStmtWhile: // "while" '(' ExprList ')' Stmt
		if e := n.ExprList.eval(ctx, true, fn); e.Type != nil && !e.isScalarType() {
			panic(ctx.position(n))
		}
		n.Stmt.check(ctx, fn, seq, sc, inSwitch, true)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *JumpStmt) check(ctx *context, fn *Declarator, inSwitch *SelectionStmt, inLoop bool) {
	switch n.Case {
	case JumpStmtBreak: // "break" ';'
		// [0]6.8.6.3
		//
		// 1. A break statement shall appear only in or as a switch
		// body or loop body.
		if inSwitch == nil && !inLoop {
			panic(ctx.position)
		}
	case JumpStmtContinue: // "continue" ';'
		// [0]6.8.6.2
		//
		// 1. A continue statement shall appear only in or as a loop
		// body.
		if !inLoop {
			panic(ctx.position(n))
		}
	case JumpStmtGoto: // "goto" IDENTIFIER ';'
		if nm := n.Token2.Val; n.scope.LookupLabel(nm) == nil {
			panic(ctx.position(n))
		}
	case JumpStmtReturn: // "return" ExprListOpt ';'
		// [0]6.8.6.4
		op := n.ExprListOpt.eval(ctx, true, fn)
		switch t := fn.Type.(*FunctionType).Result; t.Kind() {
		case Void:
			if op.Type != nil {
				if ctx.tweaks.EnableReturnExprInVoidFunc {
					break
				}

				panic(ctx.position(n))
			}
		default:
			if op.Type == nil {
				panic(ctx.position(n))
			}
			n.ReturnOperand = op.convertTo(ctx.model, t)
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ExprStmt) check(ctx *context, fn *Declarator) {
	// ExprListOpt ';'
	n.ExprListOpt.eval(ctx, true, fn)
}

func (n *Declaration) check(ctx *context, sc []int, fn *Declarator) {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	ds := &DeclarationSpecifier{}
	n.DeclarationSpecifiers.check(ctx, ds)
	if len(ds.TypeSpecifiers) == 0 { // [0]6.7.2-2
		panic("TODO")
	}
	n.InitDeclaratorListOpt.check(ctx, ds, sc, fn)
}

func (n *InitDeclaratorListOpt) check(ctx *context, ds *DeclarationSpecifier, sc []int, fn *Declarator) {
	if n == nil {
		return
	}

	n.InitDeclaratorList.check(ctx, ds, sc, fn)
}

func (n *InitDeclaratorList) check(ctx *context, ds *DeclarationSpecifier, sc []int, fn *Declarator) {
	for ; n != nil; n = n.InitDeclaratorList {
		n.InitDeclarator.check(ctx, ds, sc, fn)
	}
}

func (n *InitDeclarator) check(ctx *context, ds *DeclarationSpecifier, sc []int, fn *Declarator) {
	switch n.Case {
	case InitDeclaratorBase: // Declarator
		n.Declarator.check(ctx, ds, ds.typ(), !ds.IsTypedef(), sc, fn)
	case InitDeclaratorInit: // Declarator '=' Initializer
		if ds.IsTypedef() {
			panic(ctx.position(n)) // error
		}
		n.Declarator.check(ctx, ds, ds.typ(), true, sc, fn)
		n.Initializer.check(ctx, n.Declarator.Type, fn)
		ex := n.Declarator.scope.Idents[n.Declarator.Name()].(*Declarator)
		switch {
		case ex.Initializer == nil:
			ex.Initializer = n.Initializer
		default:
			if n.Initializer != nil {
				panic(ctx.position(n)) // More than one initializer
			}
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *Initializer) check(ctx *context, t Type, fn *Declarator) (r Operand) {
	// [0]6.7.8
	switch n.Case {
	case InitializerCompLit: // '{' InitializerList CommaOpt '}'
		return n.InitializerList.check(ctx, t, fn)
	case InitializerExpr: // Expr
		op := n.Expr.eval(ctx, true, fn)
		if t.IsScalarType() {
			// 11. The initializer for a scalar shall be a single
			// expression, optionally enclosed in braces. The
			// initial value of the object is that of the
			// expression (after conversion); the same type
			// constraints and conversions as for simple assignment
			// apply, taking the type of the scalar to be the
			// unqualified version of its declared type.
			t.assign(ctx, op)
			return n.Expr.Operand
		}

		if t.Kind() == Struct || t.Kind() == Union {
			// 13. The initializer for a structure or union object
			// that has automatic storage duration shall be either
			// an initializer list as described below, or a single
			// expression that has compatible structure or union
			// type. In the latter case, the initial value of the
			// object, including unnamed members, is that of the
			// expression.
			if t.IsCompatible(op.Type) {
				return Operand{}
			}

			panic(ctx.position(n))
		}

		if t.Kind() == Array && t.(*ArrayType).Item == Char && op.isPointerType() && op.Type.(*PointerType).Item == Char {
			// 14. An array of character type may be initialized by
			// a character string literal, optionally enclosed in
			// braces. Successive characters of the character
			// string literal (including the terminating null
			// character if there is room or if the array is of
			// unknown size) initialize the elements of the array.
			x := t.(*ArrayType)
			if x.Size.Type == nil {
				switch y := op.Value.(type) {
				case *ir.StringValue:
					x.Size = newIntConst(ctx, n, uint64(len(dict.S(int(y.StringID)))+1), UInt, ULong, ULongLong)
				default:
					panic(fmt.Errorf("%v: TODO", ctx.position(n)))
				}
			}
			return op
		}

		panic(fmt.Errorf("%v: TODO %v %v", ctx.position(n), t, op))
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *InitializerList) check(ctx *context, t Type, fn *Declarator) Operand {
	// InitializerList:
	//         /* empty */                                  // Case 0
	// |       Initializer                                  // Case 1
	// |       Designation Initializer                      // Case 2
	// |       InitializerList ',' Initializer              // Case 3
	// |       InitializerList ',' Designation Initializer  // Case 4
	r := &ir.CompositeValue{}
	n.Operand = Operand{Type: t, Value: r}
	n0 := n
	for {
		switch x := t.(type) {
		case *ArrayType:
			var index, maxIndex int64 = 0, -1
			for ; n != nil; n = n.InitializerList {
				if n.Designation != nil {
					panic(fmt.Errorf("%v: TODO", ctx.position(n.Initializer)))
				}

				r.Values = append(r.Values, n.Initializer.check(ctx, x.Item, fn))
				if index > maxIndex {
					maxIndex = index
				}
				index++
				n0.Len++
			}
			if x.Size.Type == nil {
				x.Size = newIntConst(ctx, n0, uint64(maxIndex+1), UInt, ULong, ULongLong)
			}
			return Operand{Type: t, Value: r}
		case *NamedType:
			t = x.Type
		case *StructType:
			field := 0
			for ; n != nil; n = n.InitializerList {
				if n.Designation != nil {
					panic(fmt.Errorf("%v: TODO", ctx.position(n.Initializer)))
				}

				switch {
				case field < len(x.Fields):
					r.Values = append(r.Values, n.Initializer.check(ctx, x.Fields[field].Type, fn))
					field++
				default:
					panic(fmt.Errorf("%v: TODO", ctx.position(n.Initializer)))
				}
				n0.Len++
			}
			return Operand{Type: t, Value: r}
		case *TaggedStructType:
			t = x.getType()
			if t == x {
				panic("TODO")
			}
		case *TaggedUnionType:
			t = x.getType()
			if t == x {
				panic("TODO")
			}
		case *UnionType:
			field := 0
			for ; n != nil; n = n.InitializerList {
				if field != 0 {
					panic("TODO")
				}
				if n.Designation != nil {
					panic(fmt.Errorf("%v: TODO", ctx.position(n.Initializer)))
				}

				switch {
				case field < len(x.Fields):
					r.Values = append(r.Values, n.Initializer.check(ctx, x.Fields[field].Type, fn))
					field++
				default:
					panic(fmt.Errorf("%v: TODO", ctx.position(n.Initializer)))
				}
				n0.Len++
			}
			return Operand{Type: t, Value: r}
		default:
			panic(fmt.Errorf("%v: TODO %T", ctx.position(n), x))
		}
	}
}

func (n *Declarator) check(ctx *context, ds *DeclarationSpecifier, t Type, isObject bool, sc []int, fn *Declarator) (r Type) {
	// PointerOpt DirectDeclarator
	if l := len(sc); l != 0 {
		n.ScopeNum = sc[l-1]
	}
	n.DeclarationSpecifier = ds
	if fn != nil && !n.Embedded {
		fn.vars = append(fn.vars, n)
	}
	t = n.PointerOpt.check(ctx, t, &n.TypeQualifiers)
	n.Type = n.DirectDeclarator.check(ctx, t, sc, fn)
	isFunction := n.Type.Kind() == Function && !ds.IsTypedef()
	if n.Embedded {
		return n.Type
	}

	// [0]6.2.2
	switch {
	case
		// 3. If the declaration of a file scope identifier for an
		// object or a function contains the storage-class specifier
		// static, the identifier has internal linkage.
		(isObject || isFunction) && n.scope == ctx.scope && ds.IsStatic():

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
		n.IsFunctionParameter,
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
		ds.IsStatic():

		n.StorageDuration = StorageDurationStatic
	case
		// 4. An object whose identifier is declared with no linkage
		// and without the storage-class specifier static has automatic
		// storage duration.
		n.Linkage == LinkageNone && !ds.IsStatic():

		n.StorageDuration = StorageDurationAutomatic
	default:
		panic(ctx.position(n))
	}

	nm := n.Name()
	switch ex := n.scope.Idents[n.Name()]; ex := ex.(type) {
	case nil:
		n.scope.insertDeclarator(ctx, n)
	case *Declarator:
		switch ex.Linkage {
		case LinkageNone:
			switch n.Linkage {
			case LinkageNone:
				if ex.DeclarationSpecifier.IsTypedef() && n.DeclarationSpecifier.IsTypedef() && ex.Type.String() == n.Type.String() {
					break
				}

				panic(ctx.position(n))
			default:
				panic(n.Linkage)
			}
		case LinkageExternal:
			switch n.Linkage {
			case LinkageExternal:
				if !ex.Type.IsCompatible(n.Type) {
					if !(n.Name() == idMain && n.scope.Parent == nil && n.Type.Kind() == Function) {
						panic(ctx.position(n))
					}
				}
			default:
				panic(n.Linkage)
			}
		case LinkageInternal:
			switch n.Linkage {
			case LinkageInternal:
				if !ex.Type.IsCompatible(n.Type) {
					panic(ctx.position(n))
				}
			default:
				panic(n.Linkage)
			}
		default:
			panic(ex.Linkage)
		}

		if isFunction {
			switch {
			case n.FunctionDefinition != nil:
				if ex.FunctionDefinition != nil {
					panic(ctx.position(n))
				}

				ex.Definition = n
				n.scope.Idents[nm] = n
			case ex.FunctionDefinition != nil:
				n.Definition = ex
			}
			break
		}

		switch {
		case n.Initializer != nil:
			if ex.Initializer != nil {
				panic(ctx.position(n))
			}

			ex.Definition = n
			n.scope.Idents[nm] = n
		case ex.Initializer != nil:
			n.Definition = ex
		}
	default:
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

func (n *DirectDeclarator) check(ctx *context, t Type, sc []int, fn *Declarator) Type {
	switch n.Case {
	case DirectDeclaratorParen: // '(' Declarator ')'
		return n.Declarator.check(ctx, nil, t, false, sc, fn)
	case DirectDeclaratorIdentList: // DirectDeclarator '(' IdentifierListOpt ')'
		var params []Type
		var variadic bool
		names := n.IdentifierListOpt.check()
		if len(names) != 0 {
			panic(fmt.Errorf("%v", ctx.position(n)))
		}
		t := &FunctionType{
			Params:   params,
			Result:   t,
			Variadic: variadic,
		}
		return n.DirectDeclarator.check(ctx, t, sc, fn)
	case DirectDeclaratorParamList: // DirectDeclarator '(' ParameterTypeList ')'
		fp, variadic := n.ParameterTypeList.check(ctx)
		t := &FunctionType{
			Params:   fp,
			Result:   t,
			Variadic: variadic,
		}
		return n.DirectDeclarator.check(ctx, t, sc, fn)
	//TODO case DirectDeclaratorArraySize: // DirectDeclarator '[' "static" TypeQualifierListOpt Expr ']'
	//TODO case DirectDeclaratorArraySize2: // DirectDeclarator '[' TypeQualifierList "static" Expr ']'
	//TODO case DirectDeclaratorArrayVar: // DirectDeclarator '[' TypeQualifierListOpt '*' ']'
	case DirectDeclaratorArray: // DirectDeclarator '[' TypeQualifierListOpt ExprOpt ']'
		var tq []*TypeQualifier
		n.TypeQualifierListOpt.check(ctx, &tq)
		n.ExprOpt.eval(ctx, true, fn)
		var sz Operand
		if o := n.ExprOpt; o != nil {
			sz = o.Expr.Operand
			//o.Expr.dumpValues("· ") //TODO-
		}
		t := &ArrayType{
			Item:           t,
			Size:           sz,
			TypeQualifiers: tq,
		}
		return n.DirectDeclarator.check(ctx, t, sc, fn)
	case DirectDeclaratorIdent: // IDENTIFIER
		return t
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *IdentifierListOpt) check() []int {
	if n == nil {
		return nil
	}

	return n.IdentifierList.check()
}

func (n *IdentifierList) check() (r []int) {
	m := map[int]struct{}{}
	for ; n != nil; n = n.IdentifierList {
		nm := n.Token.Val
		if n.Token.Rune == ',' {
			nm = n.Token2.Val
		}

		if _, ok := m[nm]; ok {
			panic("TODO")
		}

		m[nm] = struct{}{}
		r = append(r, nm)
	}
	return r
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
		n.Declarator.IsFunctionParameter = true
		return n.Declarator.check(ctx, ds, ds.typ(), true, nil, nil)
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
	case EnumSpecifierTag: // "enum" IDENTIFIER
		n.typ = &TaggedEnumType{Tag: n.Token2.Val, scope: n.scope}
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
			n.scope.insertEnumTag(ctx, n.IdentifierOpt.Token.Val, n)
		}
		n.typ = t
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *EnumeratorList) check(ctx *context, s *Scope) *EnumType {
	r := &EnumType{}
	iota := int64(-1)
	for ; n != nil; n = n.EnumeratorList {
		r.Enums = append(r.Enums, n.Enumerator.check(ctx, s, &iota))
	}
	return r
}

func (n *Enumerator) check(ctx *context, s *Scope, iota *int64) *EnumerationConstant {
	c := n.EnumerationConstant
	switch n.Case {
	case EnumeratorBase: // EnumerationConstant
		*iota++
		c.Operand = Operand{Value: &ir.Int64Value{Value: *iota}}
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
			n.typ = &TaggedStructType{Tag: n.Token.Val, scope: n.scope}
		case StructOrUnionUnion:
			n.typ = &TaggedUnionType{Tag: n.Token.Val, scope: n.scope}
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
			n.typ = &StructType{Fields: n.StructDeclarationList.check(ctx), scope: n.scope}
		default:
			n.typ = &UnionType{Fields: n.StructDeclarationList.check(ctx), scope: n.scope}
		}
		if n.IdentifierOpt != nil {
			n.scope.Parent.insertStructTag(ctx, n)
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *StructDeclarationList) check(ctx *context) (r []Field) {
	field := 0
	for ; n != nil; n = n.StructDeclarationList {
		r = append(r, n.StructDeclaration.check(ctx, &field)...)
	}
	return r
}

func (n *StructDeclaration) check(ctx *context, field *int) []Field {
	switch n.Case {
	case StructDeclarationBase: // SpecifierQualifierList StructDeclaratorList ';'
		ds := &DeclarationSpecifier{}
		n.SpecifierQualifierList.check(ctx, ds)
		return n.StructDeclaratorList.check(ctx, ds, field)
	case StructDeclarationAnon: // SpecifierQualifierList ';'
		ds := &DeclarationSpecifier{}
		n.SpecifierQualifierList.check(ctx, ds)
		return []Field{{Type: ds.typ()}}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *StructDeclaratorList) check(ctx *context, ds *DeclarationSpecifier, field *int) (r []Field) {
	for ; n != nil; n = n.StructDeclaratorList {
		r = append(r, n.StructDeclarator.check(ctx, ds, *field))
		*field++
	}
	return r
}

func (n *StructDeclarator) check(ctx *context, ds *DeclarationSpecifier, field int) Field {
	switch n.Case {
	case StructDeclaratorBase: // Declarator
		f := Field{Type: n.Declarator.check(ctx, ds, ds.typ(), false, nil, nil), Name: n.Declarator.Name()}
		n.Declarator.IsField = true
		n.Declarator.Field = field
		return f
	case StructDeclaratorBits: // DeclaratorOpt ':' ConstExpr
		var d *Declarator
		var nm int
		t := ds.typ()
		if n.DeclaratorOpt != nil {
			d = n.DeclaratorOpt.Declarator
			nm = d.Name()
			d.IsField = true
			d.Field = field
			t = d.check(ctx, ds, t, false, nil, nil)
		} else {
			panic(ctx.position(n))
		}
		op := n.ConstExpr.eval(ctx)
		if op.Value == nil {
			panic(ctx.position)
		}
		if !op.isIntegerType() {
			panic(ctx.position)
		}
		bits := op.Value.(*ir.Int64Value).Value
		if bits < 1 || bits > 64 {
			panic(ctx.position)
		}
		n.Bits = int(bits)
		if d != nil {
			d.Bits = n.Bits
		}
		return Field{Type: t, Name: nm, Bits: n.Bits}
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

// IsTLD reports whether n is declared in file scope.
func (n *Declarator) IsTLD() bool { return n.scope.Parent == nil }
