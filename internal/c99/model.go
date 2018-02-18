// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

import (
	"fmt"
	"runtime"

	"github.com/cznic/ir"
)

// Model describes properties of scalar Types.
type Model map[TypeKind]ModelItem

// ModelItem describers properties of a particular Type.
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

			Void: {1, 1, 1},
			Ptr:  {8, 8, 8},
		}, nil
	default:
		return nil, fmt.Errorf("unknown/unsupported architecture %s", arch)
	}
}

// Equal returns whether m equals n.
func (m Model) Equal(n Model) bool {
	if len(m) != len(n) {
		return false
	}

	for k, v := range m {
		if v != n[k] {
			return false
		}
	}
	return true
}

// Sizeof returns the size in bytes of a variable of type t.
func (m Model) Sizeof(t Type) int64 {
	switch x := UnderlyingType(t).(type) {
	case *ArrayType:
		if x.Size.Type == nil || x.Size.Value == nil {
			panic("TODO")
		}
		return m.Sizeof(x.Item) * x.Size.Value.(*ir.Int64Value).Value
	case *NamedType:
		return m.Sizeof(x.Type)
	case *PointerType:
		return int64(m[Ptr].Size)
	case *StructType:
		layout := m.Layout(x)
		if len(layout) == 0 {
			return 0
		}

		lf := layout[len(layout)-1]
		return roundup(lf.Offset+lf.Size+int64(lf.Padding), int64(m.Alignof(t)))
	case *TaggedStructType:
		u := x.getType()
		if u == x {
			panic("TODO")
		}

		return m.Sizeof(u)
	case TypeKind:
		return int64(m[x].Size)
	case *UnionType:
		var sz int64
		for _, v := range x.Fields {
			if n := m.Sizeof(v.Type); n > sz {
				sz = n
			}
		}
		return roundup(sz, int64(m.Alignof(t)))
	case nil:
		panic("internal error")
	default:
		panic(x)
	}
}

// FieldProperties describe a struct/union field.
type FieldProperties struct {
	Bitoff     int   // Zero based bit number of a bitfield
	Bits       int   // Width of a bit field or zero otherwise.
	Offset     int64 // Byte offset relative to start of the struct/union.
	Size       int64 // Field size for copying.
	Padding    int   // Adjustment to enforce proper alignment.
	PackedType Type  // Bits != 0: Storage type holding the bit field.
}

func (f *FieldProperties) Mask() uint64 {
	if f.Bits == 0 {
		return 1<<64 - 1
	}

	return (1<<uint(f.Bits) - 1) << uint(f.Bitoff)
}

// Layout computes the memory layout of t.
func (m Model) Layout(t Type) []FieldProperties {
	switch x := UnderlyingType(t).(type) {
	case *StructType:
		if len(x.Fields) == 0 {
			return nil
		}

		r := make([]FieldProperties, len(x.Fields))
		var off int64
		bitoff := 0
		for i, v := range x.Fields {
			switch {
			case v.Bits != 0:
				switch {
				case bitoff == 0:
					r[i] = FieldProperties{Offset: off, Bitoff: bitoff, Bits: v.Bits}
					bitoff = v.Bits
				default:
					n := bitoff + v.Bits
					if n > 64 {
						off = m.packBits(bitoff, i-1, off, r)
						r[i] = FieldProperties{Offset: off, Bits: v.Bits}
						bitoff = v.Bits
						break
					}

					r[i] = FieldProperties{Offset: off, Bitoff: bitoff, Bits: v.Bits}
					bitoff = n
				}
			default:
				if bitoff != 0 {
					off = m.packBits(bitoff, i-1, off, r)
					bitoff = 0
				}
				sz := m.Sizeof(v.Type)
				a := m.StructAlignof(v.Type)
				z := off
				if a != 0 {
					off = roundup(off, int64(a))
				}
				if off != z {
					r[i-1].Padding = int(off - z)
				}
				r[i] = FieldProperties{Offset: off, Size: sz}
				off += sz
			}
		}
		i := len(r) - 1
		if bitoff != 0 {
			off = m.packBits(bitoff, i, off, r)
		}
		for i, v := range r {
			if v.Bits != 0 {
				x.Fields[i].PackedType = v.PackedType
			}
		}
		z := off
		off = roundup(off, int64(m.Alignof(t)))
		if off != z {
			r[len(r)-1].Padding = int(off - z)
		}
		return r
	case *UnionType:
		if len(x.Fields) == 0 {
			return nil
		}

		r := make([]FieldProperties, len(x.Fields))
		var off int64
		for i, v := range x.Fields {
			switch {
			case v.Bits != 0:
				panic("internal error")
			default:
				sz := m.Sizeof(v.Type)
				a := m.StructAlignof(v.Type)
				z := off
				if a != 0 {
					off = roundup(off, int64(a))
				}
				if off != z {
					r[i-1].Padding = int(off - z)
				}
				r[i] = FieldProperties{Offset: off, Size: sz, Bits: v.Bits}
			}
		}
		for i, v := range r {
			if v.Bits != 0 {
				if v.PackedType == nil {
					panic("TODO")
				}
				x.Fields[i].PackedType = v.PackedType
			}
		}
		z := off
		off = roundup(off, int64(m.Alignof(t)))
		if off != z {
			r[len(r)-1].Padding = int(off - z)
		}
		return r
	case nil:
		panic("internal error")
	default:
		panic(x)
	}
}

func (m *Model) packBits(bitoff, i int, off int64, r []FieldProperties) int64 {
	var t Type
	switch {
	case bitoff <= 8:
		t = UChar
	case bitoff <= 16:
		t = UShort
	case bitoff <= 32:
		t = UInt
	case bitoff <= 64:
		t = ULongLong
	default:
		panic("internal error")
	}
	sz := m.Sizeof(t)
	a := m.StructAlignof(t)
	z := off
	if a != 0 {
		off = roundup(off, int64(a))
	}
	var first int
	for first = i; first >= 0 && r[first].Bits != 0 && r[first].PackedType == nil; first-- {
	}
	first++
	if off != z {
		r[first-1].Padding = int(off - z)
	}
	for j := first; j <= i; j++ {
		r[j].Offset = off
		r[j].Size = sz
		r[j].PackedType = t
	}
	return off + sz
}

// Alignof computes the memory alignment requirements of t. One is returned
// for a struct/union type with no fields.
func (m Model) Alignof(t Type) int {
	switch x := t.(type) {
	case *ArrayType:
		return m.Alignof(x.Item)
	case *NamedType:
		return m.Alignof(x.Type)
	case *PointerType:
		return m[Ptr].Align
	case *StructType:
		r := 1
		for _, v := range x.Fields {
			t := v.Type
			if v.Bits != 0 {
				t = v.PackedType
			}
			if a := m.Alignof(t); a > r {
				r = a
			}
		}
		return r
	case *TaggedStructType:
		u := x.getType()
		if u == x {
			panic("TODO")
		}
		return m.Alignof(u)
	case *TaggedUnionType:
		u := x.getType()
		if u == x {
			panic("TODO")
		}
		return m.Alignof(u)
	case TypeKind:
		return m[x].Align
	case *UnionType:
		r := 1
		for _, v := range x.Fields {
			t := v.Type
			if v.Bits != 0 {
				t = v.PackedType
			}
			if a := m.Alignof(t); a > r {
				r = a
			}
		}
		return r
	case nil:
		panic("internal error")
	default:
		panic(x)
	}
}

// StructAlignof computes the memory alignment requirements of t when its
// instance is a struct field. One is returned for a struct/union type with no
// fields.
func (m Model) StructAlignof(t Type) int {
	switch x := t.(type) {
	case *ArrayType:
		return m.StructAlignof(x.Item)
	case *NamedType:
		return m.StructAlignof(x.Type)
	case *PointerType:
		return m[Ptr].StructAlign
	case *StructType:
		r := 1
		for _, v := range x.Fields {
			if a := m.StructAlignof(v.Type); a > r {
				r = a
			}
		}
		return r
	case *TaggedStructType:
		u := x.getType()
		if u == x {
			panic("TODO")
		}
		return m.StructAlignof(u)
	case TypeKind:
		return m[x].StructAlign
	case *UnionType:
		r := 1
		for _, v := range x.Fields {
			if a := m.StructAlignof(v.Type); a > r {
				r = a
			}
		}
		return r
	default:
		panic(x)
	}
}

func roundup(n, to int64) int64 {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

func (m Model) defaultArgumentPromotion(op Operand) (r Operand) {
	u := op.Type
	for {
		switch x := u.(type) {
		case *EnumType:
			u = x.Enums[0].Operand.Type
		case *NamedType:
			u = x.Type
		case *PointerType:
			op.Type = x
			return op
		case *TaggedEnumType:
			u = x.getType()
		case TypeKind:
			op.Type = x
			switch x {
			case Float:
				return op.convertTo(m, Double)
			case Double:
				return op
			case
				Char,
				Int,
				Long,
				LongLong,
				SChar,
				Short,
				UChar,
				UInt,
				ULong,
				ULongLong,
				UShort:

				return op.integerPromotion(m)
			default:
				panic(x)
			}
		default:
			panic(x)
		}
	}
}
