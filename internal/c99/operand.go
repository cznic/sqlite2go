// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

// [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

import (
	"fmt"
	"math"
	"math/bits"

	"github.com/cznic/interval"
	"github.com/cznic/ir"
	"github.com/cznic/mathutil"
)

var (
	// [0]6.3.1.1-1
	//
	// Every integer type has an integer conversion rank defined as
	// follows:
	intConvRank = [maxTypeKind]int{
		Bool:      1,
		Char:      2,
		SChar:     2,
		UChar:     2,
		Short:     3,
		UShort:    3,
		Int:       4,
		UInt:      4,
		Long:      5,
		ULong:     5,
		LongLong:  6,
		ULongLong: 6,
	}

	isSigned = [maxTypeKind]bool{
		Bool:     true,
		Char:     true,
		SChar:    true,
		Short:    true,
		Int:      true,
		Long:     true,
		LongLong: true,
	}

	isArithmeticType = [maxTypeKind]bool{
		Bool:      true,
		Char:      true,
		Enum:      true,
		Int:       true,
		Long:      true,
		LongLong:  true,
		SChar:     true,
		Short:     true,
		UChar:     true,
		UInt:      true,
		ULong:     true,
		ULongLong: true,
		UShort:    true,

		Float:      true,
		Double:     true,
		LongDouble: true,

		FloatComplex:      true,
		DoubleComplex:     true,
		LongDoubleComplex: true,
	}
)

// Address represents the address of a variable.
type Address struct { //TODO-
	Declarator *Declarator
	Offset     uintptr
}

func (a *Address) String() string {
	return fmt.Sprintf("(%s+%d, %s)", dict.S(a.Declarator.Name()), a.Offset, a.Declarator.Linkage)
}

func newBoolDomain() *interval.Int128 {
	return &interval.Int128{Cls: interval.Closed, B: mathutil.Int128{Lo: 1}}
}

func newFalseDomain() *interval.Int128 {
	return &interval.Int128{Cls: interval.Degenerate, A: mathutil.Int128{Lo: 0}, B: mathutil.Int128{Hi: 0}}
}

func newTrueDomain() *interval.Int128 {
	return &interval.Int128{Cls: interval.Degenerate, A: mathutil.Int128{Lo: 1}, B: mathutil.Int128{Hi: 1}}
}

// UsualArithmeticConversions performs transformations of operands of a binary
// operation. The function panics if either of the operands is not an
// artithmetic type.
//
// [0]6.3.1.8
//
// Many operators that expect operands of arithmetic type cause conversions and
// yield result types in a similar way. The purpose is to determine a common
// real type for the operands and result. For the specified operands, each
// operand is converted, without change of type domain, to a type whose
// corresponding real type is the common real type. Unless explicitly stated
// otherwise, the common real type is also the corresponding real type of the
// result, whose type domain is the type domain of the operands if they are the
// same, and complex otherwise. This pattern is called the usual arithmetic
// conversions:
func UsualArithmeticConversions(m Model, a, b Operand) (Operand, Operand) {
	if !a.isArithmeticType() || !b.isArithmeticType() {
		panic(fmt.Sprint(a, b))
	}

	a = a.normalize(m)
	b = b.normalize(m)
	// First, if the corresponding real type of either operand is long
	// double, the other operand is converted, without change of type
	// domain, to a type whose corresponding real type is long double.
	if a.Type.Kind() == LongDoubleComplex || b.Type.Kind() == LongDoubleComplex {
		return a.convertTo(m, LongDoubleComplex), b.convertTo(m, LongDoubleComplex)
	}

	if a.Type.Kind() == LongDouble || b.Type.Kind() == LongDouble {
		return a.convertTo(m, LongDouble), b.convertTo(m, LongDouble)
	}

	// Otherwise, if the corresponding real type of either operand is
	// double, the other operand is converted, without change of type
	// domain, to a type whose corresponding real type is double.
	if a.Type.Kind() == DoubleComplex || b.Type.Kind() == DoubleComplex {
		return a.convertTo(m, DoubleComplex), b.convertTo(m, DoubleComplex)
	}

	if a.Type.Kind() == Double || b.Type.Kind() == Double {
		return a.convertTo(m, Double), b.convertTo(m, Double)
	}

	// Otherwise, if the corresponding real type of either operand is
	// float, the other operand is converted, without change of type
	// domain, to a type whose corresponding real type is float.)
	if a.Type.Kind() == FloatComplex || b.Type.Kind() == FloatComplex {
		return a.convertTo(m, FloatComplex), b.convertTo(m, FloatComplex)
	}

	if a.Type.Kind() == Float || b.Type.Kind() == Float {
		return a.convertTo(m, Float), b.convertTo(m, Float)
	}

	// Otherwise, the integer promotions are performed on both operands.
	// Then the following rules are applied to the promoted operands:
	if !a.isIntegerType() || !b.isIntegerType() {
		//dbg("", a)
		//dbg("", b)
		panic("TODO")
	}

	a = a.integerPromotion(m)
	b = b.integerPromotion(m)

	// If both operands have the same type, then no further conversion is
	// needed.
	if a.Type.Equal(b.Type) {
		return a, b
	}

	// Otherwise, if both operands have signed integer types or both have
	// unsigned integer types, the operand with the type of lesser integer
	// conversion rank is converted to the type of the operand with greater
	// rank.
	if a.isSigned() == b.isSigned() {
		t := a.Type
		if intConvRank[b.Type.Kind()] > intConvRank[a.Type.Kind()] {
			t = b.Type
		}
		return a.convertTo(m, t), b.convertTo(m, t)
	}

	// Otherwise, if the operand that has unsigned integer type has rank
	// greater or equal to the rank of the type of the other operand, then
	// the operand with signed integer type is converted to the type of the
	// operand with unsigned integer type.
	switch {
	case a.isSigned(): // b is unsigned
		if intConvRank[b.Type.Kind()] >= intConvRank[a.Type.Kind()] {
			return a.convertTo(m, b.Type), b
		}
	case b.isSigned(): // a is unsigned
		if intConvRank[a.Type.Kind()] >= intConvRank[b.Type.Kind()] {
			return a, b.convertTo(m, a.Type)
		}
	default:
		panic(fmt.Errorf("TODO %v %v", a, b))
	}

	// Otherwise, if the type of the operand with signed integer type can
	// represent all of the values of the type of the operand with unsigned
	// integer type, then the operand with unsigned integer type is
	// converted to the type of the operand with signed integer type.
	switch {
	case a.isSigned(): // b is unsigned
		if intConvRank[a.Type.Kind()] > intConvRank[b.Type.Kind()] {
			return a, b.convertTo(m, a.Type)
		}
	case b.isSigned(): // a is unsigned
		if intConvRank[b.Type.Kind()] > intConvRank[a.Type.Kind()] {
			return a.convertTo(m, b.Type), b
		}
	default:
		panic(fmt.Errorf("TODO %v %v", a, b))
	}

	panic(fmt.Errorf("TODO %v %v", a, b))
}

// Operand represents the type and optionally the value of an expression.
type Operand struct {
	Type Type
	ir.Value
	Bits       int  // Non zero: bit field width.
	PackedType Type // Bits != 0: Storage type holding the bit field.
	Bitoff     int  // Bits != 0: bit field offset
	Domain     interval.Interface
}

func newIntConst(ctx *context, n Node, v uint64, t ...TypeKind) (r Operand) {
	b := bits.Len64(v)
	for _, t := range t {
		sign := 1
		if t.IsUnsigned() {
			sign = 0
		}
		if ctx.model[t].Size*8 >= b+sign {
			return Operand{Type: t, Value: &ir.Int64Value{Value: int64(v)}}.normalize(ctx.model)
		}
	}

	last := t[len(t)-1]
	if ctx.model[last].Size*8 == b {
		return Operand{Type: last, Value: &ir.Int64Value{Value: int64(v)}}.normalize(ctx.model)
	}

	ctx.err(n, "invalid integer constant")
	return Operand{Type: Int}.normalize(ctx.model)
}

func (o Operand) isArithmeticType() bool { return o.Type.IsArithmeticType() }
func (o Operand) String() string         { return fmt.Sprintf("(%v, %v)", o.Type, o.Value) }
func (o Operand) isIntegerType() bool    { return o.Type.IsIntegerType() }
func (o Operand) isPointerType() bool    { return o.Type.IsPointerType() }
func (o Operand) isScalarType() bool     { return o.Type.IsScalarType() } // [0]6.2.5-21
func (o Operand) isSigned() bool         { return isSigned[o.Type.Kind()] }

func (o Operand) add(ctx *context, p Operand) (r Operand) {
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value + p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value + p.Value.(*ir.Float64Value).Value}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) and(ctx *context, p Operand) (r Operand) {
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value & p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

// convertTo converts o to type t.
func (o Operand) convertTo(m Model, t Type) (r Operand) {
	if o.Type.Equal(t) {
		return o.normalize(m)
	}

	switch x := t.(type) {
	case *PointerType:
		// ok
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
			UShort:

			// ok
		default:
			panic(x)
		}
	case *NamedType:
		return o.convertTo(m, x.Type)
	default:
		panic(fmt.Errorf("%T", x))
	}

	if o.Value == nil {
		if o.isIntegerType() && t.IsIntegerType() && o.Type.IsUnsigned() != t.IsUnsigned() {
			o.Domain = nil
		}
		o.Type = t
		return o.normalize(m)
	}

	if o.isIntegerType() {
		if t.IsIntegerType() {
			return Operand{Type: t, Value: o.Value}.normalize(m)
		}

		if t.IsPointerType() {
			// [0]6.3.2.3
			if o.IsZero() {
				// 3. An integer constant expression with the
				// value 0, or such an expression cast to type
				// void *, is called a null pointer constant.
				// If a null pointer constant is converted to a
				// pointer type, the resulting pointer, called
				// a null pointer, is guaranteed to compare
				// unequal to a pointer to any object or
				// function.
				return Operand{Type: t, Value: Null}
			}

			return Operand{Type: t, Value: o.Value}
		}

		switch t.Kind() {
		case Double:
			return Operand{Type: t, Value: &ir.Float64Value{Value: float64(o.Value.(*ir.Int64Value).Value)}}
		case Float:
			return Operand{Type: t, Value: &ir.Float32Value{Value: float32(o.Value.(*ir.Int64Value).Value)}}
		default:
			panic(t)
		}
	}

	if o.Type.Kind() == Double {
		if t.IsIntegerType() {
			return Operand{Type: t, Value: &ir.Int64Value{Value: int64(o.Value.(*ir.Float64Value).Value)}}.normalize(m)
		}
		switch x := t.(type) {
		case TypeKind:
			switch x {
			case Float:
				return Operand{Type: t, Value: &ir.Float32Value{Value: float32(o.Value.(*ir.Float64Value).Value)}}
			case LongDouble:
				return Operand{Type: t, Value: o.Value}
			default:
				panic(x)
			}
		default:
			panic(x)
		}
	}

	if o.isPointerType() && t.IsPointerType() {
		o.Type = t
		return o
	}

	if o.isPointerType() && t.IsIntegerType() {
		o.Type = t
		return o
	}

	panic(fmt.Errorf("%T(%v) -> %T(%v)", o.Type, o, t, t))
}

func (o Operand) cpl(ctx *context) Operand {
	if o.isIntegerType() {
		o = o.integerPromotion(ctx.model)
	}

	switch x := o.Value.(type) {
	case nil:
		return o
	case *ir.Int64Value:
		o.Value = &ir.Int64Value{Value: ^o.Value.(*ir.Int64Value).Value}
		return o.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) div(ctx *context, n Node, p Operand) (r Operand) {
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if p.IsZero() {
		ctx.err(n, "division by zero")
		return
	}

	if o.IsZero() { // 0 / x == 0
		return o
	}

	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value / p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value / p.Value.(*ir.Float64Value).Value}}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) eq(ctx *context, p Operand) (r Operand) {
	d := newBoolDomain()
	r = Operand{Type: Int, Domain: d}
	if o.isArithmeticType() && p.isArithmeticType() {
		o, p = UsualArithmeticConversions(ctx.model, o, p)
	}
	if o.Value == nil || p.Value == nil {
		if a, b := o.Domain, p.Domain; a != nil && b != nil && interval.Intersection(a, b).Class() == interval.Empty {
			r.Value = &ir.Int64Value{Value: 0}
		}
		return r.normalize(ctx.model)
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		var val int64
		if x.Value == p.Value.(*ir.Int64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	case *ir.Float64Value:
		var val int64
		if x.Value == p.Value.(*ir.Float64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return r.normalize(ctx.model)
}

func (o Operand) ge(ctx *context, p Operand) (r Operand) {
	d := newBoolDomain()
	r = Operand{Type: Int, Domain: d}
	if o.isArithmeticType() && p.isArithmeticType() {
		o, p = UsualArithmeticConversions(ctx.model, o, p)
	}
	if o.Value == nil || p.Value == nil {
		if a, b := o.Domain, p.Domain; a != nil && b != nil && interval.Intersection(a, b).Class() == interval.Empty {
			switch {
			case a.(*interval.Int128).B.Cmp(b.(*interval.Int128).A) < 0:
				r.Value = &ir.Int64Value{Value: 0}
			case a.(*interval.Int128).A.Cmp(b.(*interval.Int128).B) >= 0:
				r.Value = &ir.Int64Value{Value: 1}
			}
		}
		return r.normalize(ctx.model)
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		var val int64
		switch {
		case o.isSigned():
			if x.Value >= p.Value.(*ir.Int64Value).Value {
				val = 1
			}
		default:
			if uint64(x.Value) >= uint64(p.Value.(*ir.Int64Value).Value) {
				val = 1
			}
		}
		r.Value = &ir.Int64Value{Value: val}
	case *ir.Float64Value:
		var val int64
		if x.Value >= p.Value.(*ir.Float64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return r.normalize(ctx.model)
}

func (o Operand) gt(ctx *context, p Operand) (r Operand) {
	d := newBoolDomain()
	r = Operand{Type: Int, Domain: d}
	if o.isArithmeticType() && p.isArithmeticType() {
		o, p = UsualArithmeticConversions(ctx.model, o, p)
	}
	if o.Value == nil || p.Value == nil {
		if a, b := o.Domain, p.Domain; a != nil && b != nil && interval.Intersection(a, b).Class() == interval.Empty {
			switch {
			case a.(*interval.Int128).B.Cmp(b.(*interval.Int128).A) <= 0:
				r.Value = &ir.Int64Value{Value: 0}
			case a.(*interval.Int128).A.Cmp(b.(*interval.Int128).B) > 0:
				r.Value = &ir.Int64Value{Value: 1}
			}
		}
		return r.normalize(ctx.model)
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		var val int64
		switch {
		case o.isSigned():
			if x.Value > p.Value.(*ir.Int64Value).Value {
				val = 1
			}
		default:
			if uint64(x.Value) > uint64(p.Value.(*ir.Int64Value).Value) {
				val = 1
			}
		}
		r.Value = &ir.Int64Value{Value: val}
	case *ir.Float64Value:
		var val int64
		if x.Value > p.Value.(*ir.Float64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return r.normalize(ctx.model)
}

// integerPromotion computes the integer promotion of o.
//
// [0]6.3.1.1-2
//
// If an int can represent all values of the original type, the value is
// converted to an int; otherwise, it is converted to an unsigned int. These
// are called the integer promotions. All other types are unchanged by the
// integer promotions.
func (o Operand) integerPromotion(m Model) Operand {
	t := o.Type
	for {
		switch x := t.(type) {
		case *NamedType:
			t = x.Type
		case *TaggedEnumType:
			t = x.getType().(*EnumType).Enums[0].Operand.Type
		case TypeKind:
			if x.IsIntegerType() && o.Bits != 0 {
				bits := m[Int].Size * 8
				switch {
				case x.IsUnsigned():
					if o.Bits < bits {
						return o.convertTo(m, Int)
					}
				default:
					if o.Bits < bits-1 {
						return o.convertTo(m, Int)
					}
				}
			}

			switch x {
			case
				Double,
				Int,
				Long,
				LongLong,
				UInt,
				ULong,
				ULongLong:

				return o
			case
				Char,
				SChar,
				Short,
				UChar,
				UShort:

				return o.convertTo(m, Int)
			default:
				panic(x)
			}
		default:
			panic(x)
		}
	}
}

// IsNonzero returns true when the value of o is known to be non-zero.
func (o Operand) IsNonzero() bool {
	switch x := o.Value.(type) {
	case nil:
		return false
	case *ir.Int64Value:
		return x.Value != 0
	case *ir.StringValue:
		return true
	case *ir.AddressValue:
		return x != Null
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

// IsZero returns true when the value of o is known to be zero.
func (o Operand) IsZero() bool {
	switch x := o.Value.(type) {
	case nil:
		return false
	case *ir.Int64Value:
		return x.Value == 0
	case *ir.Float32Value:
		return x.Value == 0
	case *ir.Float64Value:
		return x.Value == 0
	case *ir.StringValue:
		return false
	case *ir.AddressValue:
		return x == Null
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) isNullPtrConst() bool {
	return o.isIntegerType() && o.IsZero() || o.Value == Null
}

func (o Operand) le(ctx *context, p Operand) (r Operand) {
	d := newBoolDomain()
	r = Operand{Type: Int, Domain: d}
	if o.isArithmeticType() && p.isArithmeticType() {
		o, p = UsualArithmeticConversions(ctx.model, o, p)
	}
	if o.Value == nil || p.Value == nil {
		if a, b := o.Domain, p.Domain; a != nil && b != nil && interval.Intersection(a, b).Class() == interval.Empty {
			switch {
			case a.(*interval.Int128).B.Cmp(b.(*interval.Int128).A) <= 0:
				r.Value = &ir.Int64Value{Value: 1}
			case a.(*interval.Int128).A.Cmp(b.(*interval.Int128).B) > 0:
				r.Value = &ir.Int64Value{Value: 0}
			}
		}
		return r.normalize(ctx.model)
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		var val int64
		switch {
		case o.isSigned():
			if x.Value <= p.Value.(*ir.Int64Value).Value {
				val = 1
			}
		default:
			if uint64(x.Value) <= uint64(p.Value.(*ir.Int64Value).Value) {
				val = 1
			}
		}
		r.Value = &ir.Int64Value{Value: val}
	case *ir.Float64Value:
		var val int64
		if x.Value <= p.Value.(*ir.Float64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return r.normalize(ctx.model)
}

func (o Operand) lsh(ctx *context, p Operand) (r Operand) { // [0]6.5.7
	// 2. Each of the operands shall have integer type.
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}

	// 3. The integer promotions are performed on each of the operands. The
	// type of the result is that of the promoted left operand. If the
	// value of the right operand is negative or is greater than or equal
	// to the width of the promoted left operand, the behavior is
	// undefined.
	o = o.integerPromotion(ctx.model)
	p = p.integerPromotion(ctx.model)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value << uint64(p.Value.(*ir.Int64Value).Value)}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) lt(ctx *context, p Operand) (r Operand) {
	d := newBoolDomain()
	r = Operand{Type: Int, Domain: d}
	if o.isArithmeticType() && p.isArithmeticType() {
		o, p = UsualArithmeticConversions(ctx.model, o, p)
	}
	if o.Value == nil || p.Value == nil {
		if a, b := o.Domain, p.Domain; a != nil && b != nil && interval.Intersection(a, b).Class() == interval.Empty {
			switch {
			case a.(*interval.Int128).B.Cmp(b.(*interval.Int128).A) < 0:
				r.Value = &ir.Int64Value{Value: 1}
			case a.(*interval.Int128).A.Cmp(b.(*interval.Int128).B) >= 0:
				r.Value = &ir.Int64Value{Value: 0}
			}
		}
		return r.normalize(ctx.model)
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		var val int64
		switch {
		case o.isSigned():
			if x.Value < p.Value.(*ir.Int64Value).Value {
				val = 1
			}
		default:
			if uint64(x.Value) < uint64(p.Value.(*ir.Int64Value).Value) {
				val = 1
			}
		}
		r.Value = &ir.Int64Value{Value: val}
	case *ir.Float64Value:
		var val int64
		if x.Value < p.Value.(*ir.Float64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return r.normalize(ctx.model)
}

func (o Operand) mod(ctx *context, n Node, p Operand) (r Operand) {
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if p.IsZero() {
		ctx.err(n, "division by zero")
		return
	}

	if o.IsZero() { // 0 % x == 0
		return o
	}

	switch x := p.Value.(type) {
	case *ir.Int64Value:
		if x.Value == 1 || x.Value == -1 {
			return Operand{Type: o.Type, Value: &ir.Int64Value{Value: 0}}.normalize(ctx.model) //  x % {1,-1} == 0
		}
	case nil:
		return Operand{Type: o.Type}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}

	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value % p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) mul(ctx *context, p Operand) (r Operand) {
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if o.IsZero() || p.IsZero() {
		switch x := UnderlyingType(o.Type).(type) {
		case TypeKind:
			if x.IsIntegerType() {
				return Operand{Type: o.Type, Value: &ir.Int64Value{Value: 0}}
			}
		default:
			panic(fmt.Errorf("TODO %T", x))
		}
	}

	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value * p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	case *ir.Float32Value:
		return Operand{Type: o.Type, Value: &ir.Float32Value{Value: x.Value * p.Value.(*ir.Float32Value).Value}}
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value * p.Value.(*ir.Float64Value).Value}}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) ne(ctx *context, p Operand) (r Operand) {
	d := newBoolDomain()
	r = Operand{Type: Int, Domain: d}
	if o.isArithmeticType() && p.isArithmeticType() {
		o, p = UsualArithmeticConversions(ctx.model, o, p)
	}
	if o.Value == nil || p.Value == nil {
		if a, b := o.Domain, p.Domain; a != nil && b != nil && interval.Intersection(a, b).Class() == interval.Empty {
			r.Value = &ir.Int64Value{Value: 1}
		}
		return r.normalize(ctx.model)
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		var val int64
		if x.Value != p.Value.(*ir.Int64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	case *ir.Float64Value:
		var val int64
		if x.Value != p.Value.(*ir.Float64Value).Value {
			val = 1
		}
		r.Value = &ir.Int64Value{Value: val}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return r.normalize(ctx.model)
}

func (o Operand) normalize(m Model) (r Operand) {
	switch x := o.Value.(type) {
	case *ir.Int64Value:
		val := x.Value
		switch sz := m[o.Type.Kind()].Size; sz {
		case 1:
			switch {
			case o.isSigned():
				switch {
				case int8(val) < 0:
					x.Value = val | ^math.MaxUint8
				default:
					x.Value = val & math.MaxUint8
				}
			default:
				x.Value = val & math.MaxUint8
			}
		case 2:
			switch {
			case o.isSigned():
				switch {
				case int16(val) < 0:
					x.Value = val | ^math.MaxUint16
				default:
					x.Value = val & math.MaxUint16
				}
			default:
				x.Value = val & math.MaxUint16
			}
		case 4:
			switch {
			case o.isSigned():
				switch {
				case int32(val) < 0:
					x.Value = val | ^math.MaxUint32
				default:
					x.Value = val & math.MaxUint32
				}
			default:
				x.Value = val & math.MaxUint32
			}
		case 8:
			// nop
		default:
			panic(fmt.Errorf("TODO %v", sz))
		}
		var a mathutil.Int128
		switch {
		case o.isSigned():
			a.SetInt64(x.Value)
		default:
			a.SetUint64(uint64(x.Value))
		}
		o.Domain = &interval.Int128{Cls: interval.Degenerate, A: a, B: a}
	case nil:
		var a, b mathutil.Int128
		if o.Type.IsIntegerType() {
			sz := m[o.Type.Kind()].Size
			switch {
			case o.Type.IsUnsigned():
				switch sz {
				case 1:
					b.SetUint64(math.MaxUint8)
				case 2:
					b.SetUint64(math.MaxUint16)
				case 4:
					b.SetUint64(math.MaxUint32)
				case 8:
					b.SetUint64(math.MaxUint64)
				default:
					panic("internal error")
				}
			default:
				switch sz {
				case 1:
					a.SetInt64(math.MinInt8)
					b.SetInt64(math.MaxInt8)
				case 2:
					a.SetInt64(math.MinInt16)
					b.SetInt64(math.MaxInt16)
				case 4:
					a.SetInt64(math.MinInt32)
					b.SetInt64(math.MaxInt32)
				case 8:
					a.SetInt64(math.MinInt64)
					b.SetInt64(math.MaxInt64)
				default:
					panic("internal error")
				}
			}
			d := interval.Interface(&interval.Int128{Cls: interval.Closed, A: a, B: b})
			if e := o.Domain; e != nil {
				d = interval.Intersection(d, e)
			}
			o.Domain = d
		}
	case
		*ir.AddressValue,
		*ir.Float32Value,
		*ir.Float64Value,
		*ir.StringValue:

		// nop
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
	return o
}

func (o Operand) or(ctx *context, p Operand) (r Operand) {
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value | p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) rsh(ctx *context, p Operand) (r Operand) { // [0]6.5.7
	// 2. Each of the operands shall have integer type.
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}

	// 3. The integer promotions are performed on each of the operands. The
	// type of the result is that of the promoted left operand. If the
	// value of the right operand is negative or is greater than or equal
	// to the width of the promoted left operand, the behavior is
	// undefined.
	o = o.integerPromotion(ctx.model)
	p = p.integerPromotion(ctx.model)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		switch {
		case o.isSigned():
			return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value >> uint64(p.Value.(*ir.Int64Value).Value)}}.normalize(ctx.model)
		default:
			return Operand{Type: o.Type, Value: &ir.Int64Value{Value: int64(uint64(x.Value) >> uint64(p.Value.(*ir.Int64Value).Value))}}.normalize(ctx.model)
		}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) sub(ctx *context, p Operand) (r Operand) {
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value - p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value - p.Value.(*ir.Float64Value).Value}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) unaryMinus(ctx *context) Operand {
	if o.isIntegerType() {
		o = o.integerPromotion(ctx.model)
	}

	switch x := o.Value.(type) {
	case nil:
		return o
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: -x.Value}}.normalize(ctx.model)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: -x.Value}}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) xor(ctx *context, p Operand) (r Operand) {
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = UsualArithmeticConversions(ctx.model, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value ^ p.Value.(*ir.Int64Value).Value}}.normalize(ctx.model)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}
