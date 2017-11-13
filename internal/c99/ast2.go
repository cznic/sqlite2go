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
	case d.is(TypeSpecifierInt, TypeSpecifierUnsigned):
		return UInt
	case d.is(TypeSpecifierLong, TypeSpecifierLong):
		return LongLong
	case d.is(TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULong
	case d.is(TypeSpecifierInt, TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULong
	case d.is(TypeSpecifierLong, TypeSpecifierLong, TypeSpecifierUnsigned):
		return ULongLong
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

func (n *ConstExpr) eval(ctx *context) *Operand {
	if n.Operand == nil {
		n.Operand = n.Expr.eval(ctx)
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
	//TODO case ExprSizeofExpr: // "sizeof" Expr
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
	//TODO case ExprAddrof: // '&' Expr
	case ExprPExprList: // '(' ExprList ')'
		n.Operand = n.ExprList.eval(ctx)
	//TODO case ExprCompLit: // '(' TypeName ')' '{' InitializerList CommaOpt '}'
	//TODO case ExprCast: // '(' TypeName ')' Expr
	//TODO case ExprDeref: // '*' Expr
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

		n.Operand = &Operand{Int, &ir.Int64Value{Value: int64(r)}}
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
	//TODO case ExprPostInt: // Expr "++"
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
	//TODO case ExprAnd: // Expr '&' Expr
	//TODO case ExprCall: // Expr '(' ArgumentExprListOpt ')'
	//TODO case ExprMul: // Expr '*' Expr
	case ExprAdd: // Expr '+' Expr
		n.Operand = n.Expr.eval(ctx).add(ctx, n.Expr2.eval(ctx))
	//TODO case ExprSub: // Expr '-' Expr
	//TODO case ExprSelect: // Expr '.' IDENTIFIER
	//TODO case ExprDiv: // Expr '/' Expr
	case ExprLt: // Expr '<' Expr
		n.Operand = n.Expr.eval(ctx).lt(ctx, n.Expr2.eval(ctx))
	//TODO case ExprAssign: // Expr '=' Expr
	case ExprGt: // Expr '>' Expr
		n.Operand = n.Expr.eval(ctx).gt(ctx, n.Expr2.eval(ctx))
	//TODO case ExprCond: // Expr '?' ExprList ':' Expr
	//TODO case ExprIndex: // Expr '[' ExprList ']'
	//TODO case ExprXor: // Expr '^' Expr
	//TODO case ExprOr: // Expr '|' Expr
	//TODO case ExprFloat: // FLOATCONST
	//TODO case ExprIdent: // IDENTIFIER
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
			n.Operand = newIntConstOperand(ctx, n, v, Int, Long, LongLong)
		case suff == "":
			n.Operand = newIntConstOperand(ctx, n, v, Int, UInt, Long, ULong, LongLong, ULongLong)
		case suff == "L" && decadic:
			n.Operand = newIntConstOperand(ctx, n, v, Long, LongLong)
		default:
			panic(fmt.Errorf("%v: TODO %q %q decadic: %v\n%s", ctx.fset.Position(n.Pos()), s, suff, decadic, PrettyString(n)))
		}
	//TODO case ExprLChar: // LONGCHARCONST
	//TODO case ExprLString: // LONGSTRINGLITERAL
	case ExprString: // STRINGLITERAL
		s := dict.S(n.Token.Val)
		n.Operand = &Operand{&PointerType{Item: Char}, &ir.StringValue{StringID: ir.StringID(dict.ID(s[1 : len(s)-1]))}}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	return n.Operand
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
	n.Declarator.check(ctx, ds, ds.typ())
	n.FunctionBody.check(ctx)
}

func (n *FunctionBody) check(ctx *context) {
	// CompoundStmt *CompoundStmt
	n.CompoundStmt.check(ctx)
}

func (n *CompoundStmt) check(ctx *context) {
	// '{' BlockItemListOpt '}'
	n.BlockItemListOpt.check(ctx)
}

func (n *BlockItemListOpt) check(ctx *context) {
	if n == nil {
		return
	}

	n.BlockItemList.check(ctx)
}

func (n *BlockItemList) check(ctx *context) {
	for ; n != nil; n = n.BlockItemList {
		n.BlockItem.check(ctx)
	}
}

func (n *BlockItem) check(ctx *context) {
	switch n.Case {
	//TODO case BlockItemDecl: // Declaration
	case BlockItemStmt: // Stmt
		n.Stmt.check(ctx)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *Stmt) check(ctx *context) {
	switch n.Case {
	//TODO case StmtBlock: // CompoundStmt
	//TODO case StmtExpr: // ExprStmt
	//TODO case StmtIter: // IterationStmt
	//TODO case StmtJump: // JumpStmt
	//TODO case StmtLabeled: // LabeledStmt
	//TODO case StmtSelect: // SelectionStmt
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
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
		n.Declarator.check(ctx, ds, ds.typ())
	case InitDeclaratorInit: // Declarator '=' Initializer
		n.Declarator.check(ctx, ds, ds.typ())
		n.Declarator.Initializer = n.Initializer.check(ctx)
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
			panic("TODO")
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

func (n *Declarator) check(ctx *context, ds *DeclarationSpecifier, t Type) {
	// PointerOpt DirectDeclarator
	n.DeclarationSpecifier = ds
	t = n.PointerOpt.check(ctx, t, &n.TypeQualifiers)
	n.Type = n.DirectDeclarator.check(ctx, t)
	switch x := n.Type.(type) {
	case
		*ArrayType,
		*FunctionType,
		*PointerType,
		*StructType:

		// nop
	case *NamedType:
		d := n.scope.lookup(x.Name)
		if d == nil {
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
			UInt,
			ULong,
			ULongLong:

			// nop
		case EnumTag:
			panic("TODO") // lookup
		case StructTag:
			panic("TODO") // lookup
		default:
			panic(x.String())
		}
	default:
		panic(fmt.Errorf("%v: %T", ctx.position(n), x))
	}
	if !n.Embedded {
		n.scope.insert(ctx, n)
	}
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
	//TODO case DirectDeclaratorParen: // '(' Declarator ')'
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
		var e *Expr
		if o := n.ExprOpt; o != nil {
			e = o.Expr
		}
		t := &ArrayType{
			Item:           t,
			Expr:           e,
			TypeQualifiers: tq,
		}
		return n.DirectDeclarator.check(ctx, t)
	case DirectDeclaratorIdent: // IDENTIFIER
		return t
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ParameterTypeList) check(ctx *context) ([]*ParameterDeclaration, bool) {
	switch n.Case {
	case ParameterTypeListBase: // ParameterList
		return n.ParameterList.check(ctx), false
	case ParameterTypeListDots: // ParameterList ',' "..."
		return n.ParameterList.check(ctx), true
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *ParameterList) check(ctx *context) (r []*ParameterDeclaration) {
	for ; n != nil; n = n.ParameterList {
		r = append(r, n.ParameterDeclaration.check(ctx))
	}
	return r
}

func (n *ParameterDeclaration) check(ctx *context) *ParameterDeclaration {
	switch n.Case {
	case ParameterDeclarationAbstract: // DeclarationSpecifiers AbstractDeclaratorOpt
		ds := &DeclarationSpecifier{}
		n.DeclarationSpecifiers.check(ctx, ds)
		n.AbstractDeclaratorOpt.check(ctx, ds, ds.typ())
	case ParameterDeclarationDeclarator: // DeclarationSpecifiers Declarator
		ds := &DeclarationSpecifier{}
		n.DeclarationSpecifiers.check(ctx, ds)
		n.Declarator.check(ctx, ds, ds.typ())
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	return n
}

func (n *AbstractDeclaratorOpt) check(ctx *context, ds *DeclarationSpecifier, t Type) {
	if n == nil {
		return
	}

	n.AbstractDeclarator.check(ctx, ds, t)
}

func (n *AbstractDeclarator) check(ctx *context, ds *DeclarationSpecifier, t Type) {
	n.DeclarationSpecifier = ds
	switch n.Case {
	case AbstractDeclaratorPointer: // Pointer
		n.Type = n.Pointer.check(ctx, t, &n.TypeQualifiers)
	//TODO case AbstractDeclaratorAbstract : // PointerOpt DirectAbstractDeclarator
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
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
	//TODO case TypeSpecifierShort: // "short"
	//TODO case TypeSpecifierSigned: // "signed"
	case
		TypeSpecifierChar,     // "char"
		TypeSpecifierDouble,   // "double"
		TypeSpecifierFloat,    // "float"
		TypeSpecifierInt,      // "int"
		TypeSpecifierLong,     // "long"
		TypeSpecifierName,     // TYPEDEF_NAME
		TypeSpecifierUnsigned, // "unsigned"
		TypeSpecifierVoid:     // "void"

		// nop
	//TODO case TypeSpecifierEnum: // EnumSpecifier
	case TypeSpecifierStruct: // StructOrUnionSpecifier
		n.StructOrUnionSpecifier.check(ctx)
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	ds.TypeSpecifiers = append(ds.TypeSpecifiers, n)
}

func (n *StructOrUnionSpecifier) check(ctx *context) {
	switch n.Case {
	//TODO case StructOrUnionSpecifierTag: // StructOrUnion IDENTIFIER
	case StructOrUnionSpecifierEmpty: // StructOrUnion IdentifierOpt '{' '}'
		if n.IdentifierOpt != nil {
			panic("TODO") // declare tag
		}
		switch n.StructOrUnion.Case {
		case StructOrUnionStruct:
			n.typ = &StructType{}
		default:
			panic("TODO")
		}
	case StructOrUnionSpecifierDefine: // StructOrUnion IdentifierOpt '{' StructDeclarationList '}'
		if n.IdentifierOpt != nil {
			panic("TODO") // declare tag
		}
		switch n.StructOrUnion.Case {
		case StructOrUnionStruct:
			n.typ = &StructType{Fields: n.StructDeclarationList.check(ctx)}
		default:
			panic("TODO")
		}
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
}

func (n *StructDeclarationList) check(ctx *context) (r []*StructDeclarator) {
	for ; n != nil; n = n.StructDeclarationList {
		r = append(r, n.StructDeclaration.check(ctx)...)
	}
	return r
}

func (n *StructDeclaration) check(ctx *context) []*StructDeclarator {
	// SpecifierQualifierList StructDeclaratorList ';'
	ds := &DeclarationSpecifier{}
	n.SpecifierQualifierList.check(ctx, ds)
	return n.StructDeclaratorList.check(ctx, ds)
}

func (n *StructDeclaratorList) check(ctx *context, ds *DeclarationSpecifier) (r []*StructDeclarator) {
	for ; n != nil; n = n.StructDeclaratorList {
		r = append(r, n.StructDeclarator.check(ctx, ds))
	}
	return r
}

func (n *StructDeclarator) check(ctx *context, ds *DeclarationSpecifier) *StructDeclarator {
	switch n.Case {
	case StructDeclaratorBase: // Declarator
		n.Declarator.check(ctx, ds, ds.typ())
	//TODO case StructDeclaratorBits: // DeclaratorOpt ':' ConstExpr
	default:
		panic(fmt.Errorf("%v: TODO %v", ctx.position(n), n.Case))
	}
	return n
}

func (n *StructDeclarator) string() string {
	switch n.Case {
	case StructDeclaratorBase: // Declarator
		return fmt.Sprintf("%s %s", n.Declarator.Type, dict.S(n.Declarator.nm()))
	//TODO case StructDeclaratorBits: // DeclaratorOpt ':' ConstExpr
	default:
		panic(fmt.Errorf("TODO %v", n.Case))
	}
}

func (n *SpecifierQualifierList) check(ctx *context, ds *DeclarationSpecifier) {
	switch n.Case {
	//TODO case SpecifierQualifierListQualifier: // TypeQualifier SpecifierQualifierListOpt
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
