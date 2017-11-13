// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

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

// TypeKind represents a particular type kind.
type TypeKind int

// Kind implements Type.
func (k TypeKind) Kind() TypeKind { return k }

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

func (k TypeKind) String() string {
	switch k {
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
		return fmt.Sprintf("TypeKind(%v)", int(k))
	}
}

// Type represents a C type.
type Type interface {
	Kind() TypeKind
	String() string
}

// ArrayType type represents an array type.
type ArrayType struct {
	Item           Type
	Expr           *Expr
	TypeQualifiers []*TypeQualifier // Eg. double a[restrict 3][5], see 6.7.5.3-21.
}

// Kind implements Type.
func (t *ArrayType) Kind() TypeKind { return Array }

func (t *ArrayType) String() string {
	var buf bytes.Buffer
	buf.WriteString(t.Item.String())
	buf.WriteByte('[')
	for range t.TypeQualifiers {
		panic("TODO")
	}
	if t.Expr != nil && t.Expr.Operand.Value != nil {
		buf.WriteString(t.Expr.Operand.Value.(fmt.Stringer).String())
	}
	buf.WriteByte(']')
	return buf.String()
}

// FunctionType represents a function type.
type FunctionType struct {
	Params   []*ParameterDeclaration
	Result   Type
	Variadic bool
}

// Kind implements Type.
func (t *FunctionType) Kind() TypeKind { return Function }

func (t *FunctionType) String() string {
	var buf bytes.Buffer
	buf.WriteString(t.Result.String())
	buf.WriteByte('(')
	for i, v := range t.Params {
		if i != 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(v.Declarator.Type.String())
	}
	buf.WriteByte(')')
	return buf.String()
}

// NamedType represents a type described by a typedef name.
type NamedType struct {
	Name int
	Type Type
}

// Kind implements Type.
func (t *NamedType) Kind() TypeKind { return TypedefName }

func (t *NamedType) String() string { return string(dict.S(t.Name)) }

// PointerType represents a pointer type.
type PointerType struct {
	Item Type
}

// Kind implements Type.
func (t *PointerType) Kind() TypeKind { return Ptr }

func (t *PointerType) String() string { return t.Item.String() + "*" }

// StructType type represents a struct type.
type StructType struct {
	Fields []*StructDeclarator
}

// Kind implements Type.
func (t *StructType) Kind() TypeKind { return Struct }

func (t *StructType) String() string {
	var buf bytes.Buffer
	buf.WriteString("struct{")
	for i, v := range t.Fields {
		if i != 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(v.string())
	}
	buf.WriteByte('}')
	return buf.String()
}
