// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

var (
	_ Type = TypeKind(0)
)

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

	maxTypeKind
)

type cond int

const (
	condZero cond = iota

	condIfOff
	condIfOn
	condIfSkip

	maxCond
)

var (
	condOn = [maxCond]bool{
		condIfOn: true,
		condZero: true,
	}
)
