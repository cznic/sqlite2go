// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

import (
	"fmt"
	"runtime"

	"github.com/cznic/ir"
	"github.com/cznic/mathutil"
)

// Model describes properties of TypeKinds.
type Model map[TypeKind]ModelItem

// ModelItem describers properties of a particular TypeKind.
type ModelItem struct {
	Size        int
	Align       int
	StructAlign int
}

func newModel() (m Model, err error) {
	switch arch := runtime.GOARCH; arch {
	case "386", "arm", "armbe", "mips", "mipsle", "ppc", "ppc64le", "s390", "s390x", "sparc":
		panic("TODO")
	case "amd64p32", "mips64p32", "mips64p32le":
		panic("TODO")
	case "amd64", "arm64", "arm64be", "mips64", "mips64le", "ppc64", "sparc64":
		return Model{
			Bool:      {1, 1, 1},
			Char:      {1, 1, 1},
			Int:       {4, 4, 4},
			Long:      {8, 8, 8},
			LongLong:  {8, 8, 8},
			SChar:     {1, 1, 1},
			Short:     {2, 2, 2},
			UChar:     {1, 1, 1},
			UInt:      {4, 4, 4},
			ULong:     {8, 8, 8},
			ULongLong: {8, 8, 8},
			UShort:    {2, 2, 2},

			Float:      {4, 4, 4},
			Double:     {8, 8, 4},
			LongDouble: {8, 8, 8},

			FloatComplex:      {8, 8, 4},
			DoubleComplex:     {8, 8, 4},
			LongDoubleComplex: {8, 8, 4},

			Ptr: {8, 8, 8},
		}, nil
	default:
		return nil, fmt.Errorf("unknown/unsupported architecture %s", arch)
	}
}

// Sizeof returns the size in bytes of a variable of type t.
func (m Model) Sizeof(t Type) int64 {
	for {
		switch x := t.(type) {
		case *ArrayType:
			if x.Size == nil {
				panic("TODO")
			}
			return m.Sizeof(x.Item) * x.Size.Value.(*ir.Int64Value).Value
		case *NamedType:
			return m.Sizeof(x.Type)
		case *PointerType:
			return int64(m[Ptr].Size)
		case TypeKind:
			return int64(m[x].Size)
		default:
			panic(x)
		}
	}
}

func (m Model) Layout(t Type) []ir.FieldProperties {
	for {
		switch x := t.(type) {
		case *StructType:
			if len(x.Fields) == 0 {
				return nil
			}

			r := make([]ir.FieldProperties, len(x.Fields))
			var off int64
			for i, v := range x.Fields {
				sz := m.Sizeof(v.Type)
				a := m.StructAlignof(v.Type)
				z := off
				if a != 0 {
					off = roundup(off, int64(a))
				}
				if off != z {
					r[i-1].Padding = int(off - z)
				}
				r[i] = ir.FieldProperties{Offset: off, Size: sz}
				off += sz
			}
			z := off
			off = roundup(off, int64(m.Alignof(t)))
			if off != z {
				r[len(r)-1].Padding = int(off - z)
			}
			return r
		default:
			panic(x)
		}
	}
}

// Alignof computes the memory alignment requirements of t. Zero is returned
// for a struct/union type with no fields.
func (m Model) Alignof(t Type) int {
	for {
		switch x := t.(type) {
		case *ArrayType:
			return mathutil.Max(1, m.Alignof(x.Item))
		case *NamedType:
			return m.Alignof(x.Type)
		case *StructType:
			var r int
			for _, v := range x.Fields {
				if a := m.Alignof(v.Type); a > r {
					r = a
				}
			}
			return mathutil.Max(1, r)
		case TypeKind:
			return m[x].Align
		default:
			panic(x)
		}
	}
}

// StructAlignof computes the memory alignment requirements of t when its
// instance is a struct field. Zero is returned for a struct/union type with no
// fields.
func (m Model) StructAlignof(t Type) int {
	for {
		switch x := t.(type) {
		case *ArrayType:
			return m.StructAlignof(x.Item)
		case *NamedType:
			return m.StructAlignof(x.Type)
		case TypeKind:
			return m[x].StructAlign
		default:
			panic(x)
		}
	}
}

func roundup(n, to int64) int64 {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}
