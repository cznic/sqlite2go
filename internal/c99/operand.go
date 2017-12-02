// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

// [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

import (
	"fmt"
	"math"
	"math/bits"

	"github.com/cznic/ir"
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
func usualArithmeticConversions(ctx *context, a, b Operand) (Operand, Operand) {
	if !a.isArithmeticType() || !b.isArithmeticType() {
		panic("TODO")
	}

	// First, if the corresponding real type of either operand is long
	// double, the other operand is converted, without change of type
	// domain, to a type whose corresponding real type is long double.
	if a.Type.Kind() == LongDoubleComplex || b.Type.Kind() == LongDoubleComplex {
		return a.convertTo(ctx, LongDoubleComplex), b.convertTo(ctx, LongDoubleComplex)
	}

	if a.Type.Kind() == LongDouble || b.Type.Kind() == LongDouble {
		return a.convertTo(ctx, LongDouble), b.convertTo(ctx, LongDouble)
	}

	// Otherwise, if the corresponding real type of either operand is
	// double, the other operand is converted, without change of type
	// domain, to a type whose corresponding real type is double.
	if a.Type.Kind() == DoubleComplex || b.Type.Kind() == DoubleComplex {
		return a.convertTo(ctx, DoubleComplex), b.convertTo(ctx, DoubleComplex)
	}

	if a.Type.Kind() == Double || b.Type.Kind() == Double {
		return a.convertTo(ctx, Double), b.convertTo(ctx, Double)
	}

	// Otherwise, if the corresponding real type of either operand is
	// float, the other operand is converted, without change of type
	// domain, to a type whose corresponding real type is float.)
	if a.Type.Kind() == FloatComplex || b.Type.Kind() == FloatComplex {
		return a.convertTo(ctx, FloatComplex), b.convertTo(ctx, FloatComplex)
	}

	if a.Type.Kind() == Float || b.Type.Kind() == Float {
		return a.convertTo(ctx, Float), b.convertTo(ctx, Float)
	}

	// Otherwise, the integer promotions are performed on both operands.
	// Then the following rules are applied to the promoted operands:
	if !a.isIntegerType() || !b.isIntegerType() {
		//dbg("", a)
		//dbg("", b)
		panic("TODO")
	}

	a = a.integerPromotion(ctx)
	b = b.integerPromotion(ctx)

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
		return a.convertTo(ctx, t), b.convertTo(ctx, t)
	}

	// Otherwise, if the operand that has unsigned integer type has rank
	// greater or equal to the rank of the type of the other operand, then
	// the operand with signed integer type is converted to the type of the
	// operand with unsigned integer type.
	switch {
	case a.isSigned(): // b is unsigned
		if intConvRank[b.Type.Kind()] >= intConvRank[a.Type.Kind()] {
			return a.convertTo(ctx, b.Type), b
		}
	case b.isSigned(): // a is unsigned
		if intConvRank[a.Type.Kind()] >= intConvRank[b.Type.Kind()] {
			return a, b.convertTo(ctx, a.Type)
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
			return a, b.convertTo(ctx, a.Type)
		}
	case b.isSigned(): // a is unsigned
		if intConvRank[b.Type.Kind()] > intConvRank[a.Type.Kind()] {
			return a.convertTo(ctx, b.Type), b
		}
	default:
		panic(fmt.Errorf("TODO %v %v", a, b))
	}

	//dbg("", a.isSigned())
	//dbg("", b.isSigned())
	panic(fmt.Errorf("TODO %v %v", a, b))
}

// Operand represents the type and optionally the value of an expression.
type Operand struct {
	Addr *ir.AddressValue // When address known link-statically.
	Type Type
	ir.Value
	//TODO lvalue bool
}

func newIntConst(c *context, n Node, v uint64, t ...TypeKind) (r Operand) {
	r = Operand{Type: Undefined}
	b := bits.Len64(v)
	for _, t := range t {
		if c.model[t].Size*8 >= b {
			return Operand{Type: t, Value: &ir.Int64Value{Value: int64(v)}}
		}
	}

	c.err(n, "invalid integer constant")
	return Operand{Type: Undefined}
}

func (o Operand) isArithmeticType() bool { return o.Type.IsArithmeticType() }
func (o Operand) String() string         { return fmt.Sprintf("(%v, %v, %v)", o.Type, o.Value, o.Addr) }
func (o Operand) isIntegerType() bool    { return o.Type.IsIntegerType() }
func (o Operand) isPointerType() bool    { return o.Type.IsPointerType() }
func (o Operand) isScalarType() bool     { return o.Type.IsScalarType() } // [0]6.2.5-21
func (o Operand) isSigned() bool         { return isSigned[o.Type.Kind()] }

func (o Operand) add(ctx *context, p Operand) (r Operand) {
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value + p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value + p.Value.(*ir.Float64Value).Value}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) and(ctx *context, p Operand) (r Operand) {
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value & p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) convertTo(ctx *context, t Type) (r Operand) {
	if o.Type.Equal(t) {
		return o
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
		return o.convertTo(ctx, x.Type)
	default:
		panic(fmt.Errorf("%T", x))
	}

	if o.Value == nil {
		o.Type = t
		return o
	}

	if o.isIntegerType() {
		if t.IsIntegerType() {
			return Operand{Type: t, Value: o.Value}.normalize(ctx)
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
				return Operand{Type: t, Addr: Null}
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
		switch x := t.(type) {
		case TypeKind:
			switch x {
			case
				Char,
				Int:

				return Operand{Type: t, Value: &ir.Int64Value{Value: int64(o.Value.(*ir.Float64Value).Value)}}.normalize(ctx)
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

	panic(fmt.Errorf("%T(%v) -> %T(%v)", o.Type, o, t, t))
}

func (o Operand) cpl(ctx *context) Operand {
	if o.isIntegerType() {
		o = o.integerPromotion(ctx)
	}

	switch x := o.Value.(type) {
	case nil:
		return o
	case *ir.Int64Value:
		o.Value = &ir.Int64Value{Value: ^o.Value.(*ir.Int64Value).Value}
		return o.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) div(ctx *context, p Operand) (r Operand) {
	o, p = usualArithmeticConversions(ctx, o, p)
	if p.IsZero() {
		panic("TODO")
	}
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value / p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value / p.Value.(*ir.Float64Value).Value}}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) eq(ctx *context, p Operand) (r Operand) {
	r = Operand{Type: Int}
	if o.Value == nil || p.Value == nil {
		return r
	}

	o, p = usualArithmeticConversions(ctx, o, p)
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
	return r
}

func (o Operand) ge(ctx *context, p Operand) (r Operand) {
	r = Operand{Type: Int}
	if o.Value == nil || p.Value == nil {
		return r
	}

	o, p = usualArithmeticConversions(ctx, o, p)
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
	return r
}

func (o Operand) gt(ctx *context, p Operand) (r Operand) {
	r = Operand{Type: Int}
	if o.Value == nil || p.Value == nil {
		return r
	}

	o, p = usualArithmeticConversions(ctx, o, p)
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
	return r
}

// [0]6.3.1.1-2
//
// If an int can represent all values of the original type, the value is
// converted to an int; otherwise, it is converted to an unsigned int. These
// are called the integer promotions. All other types are unchanged by the
// integer promotions.
func (o Operand) integerPromotion(ctx *context) Operand {
	t := o.Type
	for {
		switch x := t.(type) {
		case *NamedType:
			t = x.Type
		case *TaggedEnumType:
			return o
		case TypeKind:
			switch x {
			case
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

				return o.convertTo(ctx, Int)
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
	case *ir.Float64Value:
		return x.Value == 0
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) le(ctx *context, p Operand) (r Operand) {
	r = Operand{Type: Int}
	if o.Value == nil || p.Value == nil {
		return r
	}

	o, p = usualArithmeticConversions(ctx, o, p)
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
	return r
}

func (o Operand) lsh(ctx *context, p Operand) (r Operand) { // [0]6.5.7
	// 2. Each of the operands shall have integer type.
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value << uint64(p.Value.(*ir.Int64Value).Value)}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) lt(ctx *context, p Operand) (r Operand) {
	r = Operand{Type: Int}
	if o.Value == nil || p.Value == nil {
		return r
	}

	o, p = usualArithmeticConversions(ctx, o, p)
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
	return r
}

func (o Operand) mod(ctx *context, p Operand) (r Operand) {
	o, p = usualArithmeticConversions(ctx, o, p)
	if p.IsZero() {
		panic("TODO")
	}
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) mul(ctx *context, p Operand) (r Operand) {
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value * p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value * p.Value.(*ir.Float64Value).Value}}
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) ne(ctx *context, p Operand) (r Operand) {
	r = Operand{Type: Int}
	if o.Value == nil || p.Value == nil {
		return r
	}

	o, p = usualArithmeticConversions(ctx, o, p)
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
	return r
}

func (o Operand) normalize(ctx *context) Operand {
	switch x := o.Value.(type) {
	case *ir.Int64Value:
		val := x.Value
		switch sz := ctx.model[o.Type.Kind()].Size; sz {
		case 1:
			switch {
			case o.isSigned():
				switch {
				case val < 0:
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
				case val < 0:
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
				case val < 0:
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
	case *ir.Float64Value:
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
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value | p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) rsh(ctx *context, p Operand) (r Operand) { // [0]6.5.7
	// 2. Each of the operands shall have integer type.
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value >> uint64(p.Value.(*ir.Int64Value).Value)}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) sub(ctx *context, p Operand) (r Operand) {
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	case *ir.Int64Value:
		return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value - p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	case *ir.Float64Value:
		return Operand{Type: o.Type, Value: &ir.Float64Value{Value: x.Value - p.Value.(*ir.Float64Value).Value}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) unaryMinus(ctx *context) Operand {
	if o.isIntegerType() {
		o = o.integerPromotion(ctx)
	}

	switch x := o.Value.(type) {
	case nil:
		return o
	case *ir.Int64Value:
		x.Value = -x.Value
		return o.normalize(ctx)
	case *ir.Float64Value:
		x.Value = -x.Value
		return o
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}

func (o Operand) xor(ctx *context, p Operand) (r Operand) {
	if !o.isIntegerType() || !p.isIntegerType() {
		panic("TODO")
	}
	o, p = usualArithmeticConversions(ctx, o, p)
	if o.Value == nil || p.Value == nil {
		return Operand{Type: o.Type}
	}

	switch x := o.Value.(type) {
	//case *ir.Int64Value:
	//	return Operand{Type: o.Type, Value: &ir.Int64Value{Value: x.Value ^ p.Value.(*ir.Int64Value).Value}}.normalize(ctx)
	default:
		panic(fmt.Errorf("TODO %T", x))
	}
}
