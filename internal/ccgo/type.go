// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bytes"
	"fmt"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
)

func isVaList(t c99.Type) bool {
	x, ok := t.(*c99.NamedType)
	return ok && x.Name == idVaList
}

func (g *gen) typ(t c99.Type) string { return g.ptyp(t, true) }

func (g *gen) ptyp(t c99.Type, ptr2uintptr bool) string {
	if ptr2uintptr && c99.UnderlyingType(t).Kind() == c99.Ptr && !isVaList(t) {
		return "uintptr"
	}

	var buf bytes.Buffer
	switch x := t.(type) {
	case
		*c99.ArrayType,
		*c99.FunctionType:

		g.typ0(&buf, x)
		return buf.String()
	case *c99.NamedType:
		if x.Name == idVaList {
			if ptr2uintptr {
				return "[]interface{}"
			}

			return fmt.Sprintf("%s", dict.S(x.Name))
		}

		g.enqueue(x)
		return fmt.Sprintf("T%s", dict.S(x.Name))
	case *c99.PointerType:
		g.typ0(&buf, t)
		return buf.String()
	case *c99.StructType:
		buf.WriteString(" struct{")
		layout := g.model.Layout(x)
		for i, v := range x.Fields {
			if v.Bits != 0 {
				if layout[i].Bitoff == 0 {
					fmt.Fprintf(&buf, "F%d ", layout[i].Offset)
					g.typ0(&buf, layout[i].BitType)
					buf.WriteByte(';')
				}
				continue
			}

			fmt.Fprintf(&buf, "%s ", mangleIdent(v.Name, true))
			g.typ0(&buf, v.Type)
			buf.WriteByte(';')
		}
		buf.WriteByte('}')
		return buf.String()
	case *c99.TaggedEnumType:
		g.enqueue(x)
		return fmt.Sprintf("E%s", dict.S(x.Tag))
	case *c99.TaggedStructType:
		g.enqueue(x)
		return fmt.Sprintf("S%s", dict.S(x.Tag))
	case c99.TypeKind:
		switch x {
		case
			c99.Char,
			c99.Int,
			c99.Long,
			c99.LongLong,
			c99.SChar,
			c99.Short:

			return fmt.Sprintf("int%d", g.model[x].Size*8)
		case
			c99.UChar,
			c99.UShort,
			c99.UInt,
			c99.ULong,
			c99.ULongLong:

			return fmt.Sprintf("uint%d", g.model[x].Size*8)
		case c99.Float:
			return fmt.Sprintf("float32")
		case c99.Double:
			return fmt.Sprintf("float64")
		default:
			todo("", x)
		}
	case *c99.UnionType:
		fmt.Fprintf(&buf, "struct{[%d]byte; [0]struct{", g.model.Sizeof(x))
		for _, v := range x.Fields {
			fmt.Fprintf(&buf, "%s ", mangleIdent(v.Name, true))
			g.typ0(&buf, v.Type)
			buf.WriteByte(';')
		}
		buf.WriteString("}}")
		return buf.String()
	default:
		todo("%T %v", x, x)
	}
	panic("unreachable")
}

func (g *gen) typ0(buf *bytes.Buffer, t c99.Type) {
	for {
		switch x := t.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil || x.Size.IsZero() {
				todo("%s", x)
			}
			fmt.Fprintf(buf, "[%d]", x.Size.Value.(*ir.Int64Value).Value)
			t = x.Item
		case *c99.FunctionType:
			fmt.Fprintf(buf, " func(*%sTLS", crt)
			switch {
			case len(x.Params) == 1 && x.Params[0].Kind() == c99.Void:
				// nop
			default:
				for _, v := range x.Params {
					buf.WriteString(", ")
					buf.WriteString(g.typ(v))
				}
			}
			if x.Variadic {
				fmt.Fprintf(buf, ", ...interface{}")
			}
			buf.WriteString(")")
			if x.Result != nil && x.Result.Kind() != c99.Void {
				buf.WriteString(" " + g.typ(x.Result))
			}
			return
		case *c99.NamedType:
			if x.Name == idVaList {
				buf.WriteString("[]interface{}")
				return
			}

			g.enqueue(x)
			fmt.Fprintf(buf, "T%s", dict.S(x.Name))
			return
		case *c99.PointerType:
			t = x.Item
			if t.Kind() == c99.Void {
				buf.WriteString(" uintptr")
				return
			}

			buf.WriteByte('*')
		case *c99.StructType:
			buf.WriteString(" struct{")
			layout := g.model.Layout(x)
			for i, v := range x.Fields {
				if v.Bits != 0 {
					if layout[i].Bitoff == 0 {
						fmt.Fprintf(buf, "F%d ", layout[i].Offset)
						g.typ0(buf, layout[i].BitType)
						buf.WriteByte(';')
					}
					continue
				}

				fmt.Fprintf(buf, "%s ", mangleIdent(v.Name, true))
				g.typ0(buf, v.Type)
				buf.WriteByte(';')
			}
			buf.WriteByte('}')
			return
		case *c99.TaggedStructType:
			g.enqueue(x)
			fmt.Fprintf(buf, "S%s", dict.S(x.Tag))
			return
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Double,
				c99.Float,
				c99.Int,
				c99.Long,
				c99.LongLong,
				c99.SChar,
				c99.Short,
				c99.UChar,
				c99.UInt,
				c99.ULong,
				c99.ULongLong,
				c99.UShort:

				buf.WriteString(g.ptyp(x, false))
				return
			default:
				todo("", x)
			}
		default:
			todo("%T(%v)", x, x)
		}
	}
}

func prefer(t c99.Type) bool {
	for {
		switch x := t.(type) {
		case *c99.ArrayType:
			return x.Size.Type != nil
		case *c99.PointerType:
			t = x.Item
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Void:

				return true
			default:
				todo("", x)
			}
		default:
			todo("%T", x)
		}
	}
}
