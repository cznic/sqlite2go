// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

// [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

import (
	"bytes"
	"fmt"
)

var (
	_ Type = (*ArrayType)(nil)
	_ Type = (*FunctionType)(nil)
	_ Type = (*NamedType)(nil)
	_ Type = (*PointerType)(nil)
	_ Type = (*StructType)(nil)
	_ Type = (*undefinedType)(nil)
	_ Type = TypeKind(0)

	// Undefined represents an instance of undefined type. R/O
	Undefined = &undefinedType{}
)

type undefinedType struct {
	Type
}

// Type represents a C type.
type Type interface {
	Equal(Type) bool
	IsArithmeticType() bool
	IsCompatible(Type) bool // [0]6.2.7
	IsScalarType() bool
	Kind() TypeKind
	String() string
}

// TypeKind represents a particular type kind.
type TypeKind int

// TypeKind values.
const (
	_ TypeKind = iota

	Bool
	Char
	Int
	Long
	LongLong
	SChar
	Short
	UChar
	UInt
	ULong
	ULongLong
	UShort

	Float
	Double
	LongDouble

	FloatComplex
	DoubleComplex
	LongDoubleComplex

	Array
	Enum
	EnumTag
	Function
	Ptr
	Struct
	StructTag
	TypedefName
	Union
	Void

	maxTypeKind
)

// Kind implements Type.
func (t TypeKind) Kind() TypeKind { return t }

// IsScalarType implements Type.
func (t TypeKind) IsScalarType() bool {
	switch t {
	case
		Char,
		Int,
		LongLong,
		UChar,
		UInt,
		ULong,
		UShort:

		return true
	default:
		panic(t)
	}
}

// IsArithmeticType implements Type.
func (t TypeKind) IsArithmeticType() bool { return isArithmeticType[t] }

// IsCompatible implements Type.
func (t TypeKind) IsCompatible(u Type) bool {
	if t.Equal(u) {
		return true
	}

	panic("TODO")
}

// Equal implements Type.
func (t TypeKind) Equal(u Type) bool {
	u = flat(u)
	switch t {
	case
		Char,
		Int,
		LongLong,
		UShort,
		ULong,
		Void:

		return t == u.Kind()
	default:
		panic(t)
	}
}

func (t TypeKind) String() string {
	switch t {
	case Bool:
		return "bool"
	case Char:
		return "char"
	case Int:
		return "int"
	case Long:
		return "long"
	case LongLong:
		return "long long"
	case SChar:
		return "signed char"
	case Short:
		return "short"
	case UChar:
		return "unsigned char"
	case UInt:
		return "unsigned"
	case ULong:
		return "unsigned long"
	case ULongLong:
		return "unsigned  long long"
	case UShort:
		return "unsigned short"
	case Float:
		return "float"
	case Double:
		return "double"
	case LongDouble:
		return "long double"
	case FloatComplex:
		return "float complex"
	case DoubleComplex:
		return "double complex"
	case LongDoubleComplex:
		return "long double complex"
	case Array:
		return "array"
	case Enum:
		return "enum"
	case EnumTag:
		return "enum tag"
	case Function:
		return "function"
	case Ptr:
		return "ptr"
	case Struct:
		return "struct"
	case StructTag:
		return "struct tag"
	case TypedefName:
		return "typedef name"
	case Union:
		return "union"
	case Void:
		return "void"
	default:
		return fmt.Sprintf("TypeKind(%v)", int(t))
	}
}

// ArrayType type represents an array type.
type ArrayType struct {
	Item           Type
	Size           *Operand
	TypeQualifiers []*TypeQualifier // Eg. double a[restrict 3][5], see 6.7.5.3-21.
}

// IsArithmeticType implements Type.
func (t *ArrayType) IsArithmeticType() bool {
	panic("TODO")
}

// IsCompatible implements Type.
func (t *ArrayType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *ArrayType) Equal(u Type) bool {
	u = flat(u)
	panic("TODO")
}

// Kind implements Type.
func (t *ArrayType) Kind() TypeKind { return Array }

// IsScalarType implements Type.
func (t *ArrayType) IsScalarType() bool { return false }

func (t *ArrayType) String() string {
	switch {
	case t.Size != nil && t.Size.Value != nil:
		return fmt.Sprintf("array %v of %v", t.Size.Value, t.Item)
	default:
		return fmt.Sprintf("array of %v", t.Item)
	}
}

// EnumType represents an enum type.
type EnumType struct {
	Enums []*EnumerationConstant
}

// IsArithmeticType implements Type.
func (t *EnumType) IsArithmeticType() bool {
	panic("TODO")
}

// IsCompatible implements Type.
func (t *EnumType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *EnumType) Equal(u Type) bool {
	u = flat(u)
	panic("TODO")
}

// Kind implements Type.
func (t *EnumType) Kind() TypeKind { return Enum }

// IsScalarType implements Type.
func (t *EnumType) IsScalarType() bool { panic("TODO") }

func (t *EnumType) String() string { panic("TODO169b") }

// Field represents a struct/union field.
type Field struct {
	Name int
	Type Type
}

// FunctionType represents a function type.
type FunctionType struct {
	Params    []Type
	Prototype *FunctionType
	Result    Type
	Variadic  bool
}

// IsArithmeticType implements Type.
func (t *FunctionType) IsArithmeticType() bool {
	panic("TODO")
}

// IsCompatible implements Type.
func (t *FunctionType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *FunctionType) Equal(u Type) bool {
	u = flat(u)
	if u.Kind() != Function {
		return false
	}

	v := u.(*FunctionType)
	if len(t.Params) != len(v.Params) || t.Variadic != v.Variadic || !t.Result.Equal(v.Result) {
		return false
	}

	for i, t := range t.Params {
		if !t.Equal(v.Params[i]) {
			return false
		}
	}
	return true
}

// Kind implements Type.
func (t *FunctionType) Kind() TypeKind { return Function }

// IsScalarType implements Type.
func (t *FunctionType) IsScalarType() bool { panic("TODO") }

func (t *FunctionType) String() string {
	var buf bytes.Buffer
	buf.WriteString("function (")
	for i, v := range t.Params {
		if i != 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(v.String())
	}
	fmt.Fprintf(&buf, ") returning %v", t.Result)
	return buf.String()
}

// NamedType represents a type described by a typedef name.
type NamedType struct {
	Name int
	Type Type // The type Name refers to.
}

// IsArithmeticType implements Type.
func (t *NamedType) IsArithmeticType() bool { return t.Type.IsArithmeticType() }

// IsCompatible implements Type.
func (t *NamedType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *NamedType) Equal(u Type) bool {
	return t.Type.Equal(u)
}

// Kind implements Type.
func (t *NamedType) Kind() TypeKind { return TypedefName }

// IsScalarType implements Type.
func (t *NamedType) IsScalarType() bool { return t.Type.IsScalarType() }

func (t *NamedType) String() string { return string(dict.S(t.Name)) }

// PointerType represents a pointer type.
type PointerType struct {
	Item Type
}

// IsArithmeticType implements Type.
func (t *PointerType) IsArithmeticType() bool { return false }

// IsCompatible implements Type.
func (t *PointerType) IsCompatible(u Type) bool {
	if t.Equal(u) {
		return true
	}

	if u = flat(u); u.Kind() != Ptr {
		return false
	}

	v := u.(*PointerType)
	// [0]6.3.2.3
	//
	// 1. A pointer to void may be converted to or from a pointer to any
	// incomplete or object type. A pointer to any incomplete or object
	// type may be converted to a pointer to void and back again; the
	// result shall compare equal to the original pointer.
	if t.Item == Void || v.Item == Void {
		return true
	}

	panic("TODO")
}

// Equal implements Type.
func (t *PointerType) Equal(u Type) bool {
	u = flat(u)
	return u.Kind() == Ptr && t.Item.Equal(u.(*PointerType).Item)
}

// Kind implements Type.
func (t *PointerType) Kind() TypeKind { return Ptr }

// IsScalarType implements Type.
func (t *PointerType) IsScalarType() bool { return true }

func (t *PointerType) String() string { return fmt.Sprintf("pointer to %v", t.Item) }

// StructType represents a struct type.
type StructType struct {
	Fields []Field
}

// IsArithmeticType implements Type.
func (t *StructType) IsArithmeticType() bool {
	panic("TODO")
}

// IsCompatible implements Type.
func (t *StructType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *StructType) Equal(u Type) bool {
	u = flat(u)
	if u.Kind() != Struct {
		return false
	}

	panic("TODO")
}

// Kind implements Type.
func (t *StructType) Kind() TypeKind { return Struct }

// IsScalarType implements Type.
func (t *StructType) IsScalarType() bool { panic("TODO") }

func (t *StructType) String() string {
	var buf bytes.Buffer
	buf.WriteString("struct{")
	for i, v := range t.Fields {
		if i != 0 {
			buf.WriteByte(';')
		}
		buf.WriteString(v.Type.String())
	}
	buf.WriteByte('}')
	return buf.String()
}

// TaggedStructType represents a struct type described by a tag name.
type TaggedStructType struct {
	Tag   int
	Type  Type
	scope *scope
}

// IsArithmeticType implements Type.
func (t *TaggedStructType) IsArithmeticType() bool {
	panic("TODO")
}

// IsCompatible implements Type.
func (t *TaggedStructType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *TaggedStructType) Equal(u Type) bool {
	switch u = flat(u); u.(type) {
	case TypeKind:
		switch u {
		case Void:
			return false
		default:
			panic(u)
		}
	default:
		panic(u)
	}
}

func (t *TaggedStructType) getType() Type {
	if t.Type != nil {
		return t.Type
	}

	t.Type = Undefined
	if s := t.scope.lookupStructTag(t.Tag); s != nil {
		t.Type = s.typ
	}

	return t.Type
}

// Kind implements Type.
func (t *TaggedStructType) Kind() TypeKind { return StructTag }

// IsScalarType implements Type.
func (t *TaggedStructType) IsScalarType() bool { panic("TODO") }

func (t *TaggedStructType) String() string { return fmt.Sprintf("struct %s", dict.S(t.Tag)) }

// UnionType represents a union type.
type UnionType struct {
	Fields []Field
}

// IsArithmeticType implements Type.
func (t *UnionType) IsArithmeticType() bool {
	panic("TODO")
}

// IsCompatible implements Type.
func (t *UnionType) IsCompatible(u Type) bool { panic("TODO") }

// Equal implements Type.
func (t *UnionType) Equal(u Type) bool {
	u = flat(u)
	panic("TODO")
}

// Kind implements Type.
func (t *UnionType) Kind() TypeKind { return Union }

// IsScalarType implements Type.
func (t *UnionType) IsScalarType() bool { panic("TODO") }

func (t *UnionType) String() string {
	var buf bytes.Buffer
	buf.WriteString("union{")
	for i, v := range t.Fields {
		if i != 0 {
			buf.WriteByte(';')
		}
		buf.WriteString(v.Type.String())
	}
	buf.WriteByte('}')
	return buf.String()
}

func flat(t Type) Type {
	switch x := t.(type) {
	case *ArrayType:
		r := *x
		r.Item = flat(r.Item)
		return &r
	case *FunctionType:
		if x == nil {
			return (*FunctionType)(nil)
		}

		r := *x
		for i, v := range r.Params {
			r.Params[i] = flat(v)
		}
		r.Prototype = flat(r.Prototype).(*FunctionType)
		r.Result = flat(r.Result)
		return &r
	case *NamedType:
		return flat(x.Type)
	case *PointerType:
		r := *x
		r.Item = flat(r.Item)
		return &r
	case *StructType:
		r := *x
		for i, v := range r.Fields {
			r.Fields[i].Type = flat(v.Type)
		}
		return &r
	case *TaggedStructType:
		return t
	case TypeKind:
		return t
	default:
		panic(x)
	}
}
