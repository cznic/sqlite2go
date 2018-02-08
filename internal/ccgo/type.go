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

type tCacheKey struct {
	c99.Type
	bool
}

func isVaList(t c99.Type) bool {
	x, ok := t.(*c99.NamedType)
	return ok && x.Name == idVaList
}

func (g *gen) typ(t c99.Type) string { return g.ptyp(t, true, 0) }

func (g *gen) ptyp(t c99.Type, ptr2uintptr bool, lvl int) (r string) {
	k := tCacheKey{t, ptr2uintptr}
	if s, ok := g.tCache[k]; ok {
		return s
	}

	defer func() { g.tCache[k] = r }()

	if ptr2uintptr && t.Kind() == c99.Ptr && !isVaList(t) {
		if _, ok := t.(*c99.NamedType); !ok {
			return "uintptr"
		}
	}

	switch x := t.(type) {
	case *c99.ArrayType:
		return fmt.Sprintf("[%d]%s", x.Size.Value.(*ir.Int64Value).Value, g.ptyp(x.Item, ptr2uintptr, lvl))
	case *c99.FunctionType:
		var buf bytes.Buffer
		fmt.Fprintf(&buf, "func(*%sTLS", crt)
		switch {
		case len(x.Params) == 1 && x.Params[0].Kind() == c99.Void:
			// nop
		default:
			for _, v := range x.Params {
				fmt.Fprintf(&buf, ", %s", g.typ(v))
			}
		}
		if x.Variadic {
			fmt.Fprintf(&buf, ", ...interface{}")
		}
		buf.WriteString(")")
		if x.Result != nil && x.Result.Kind() != c99.Void {
			buf.WriteString(" " + g.typ(x.Result))
		}
		return buf.String()
	case *c99.NamedType:
		if x.Name == idVaList {
			if ptr2uintptr {
				return "[]interface{}"
			}

			return fmt.Sprintf("%s", dict.S(x.Name))
		}

		g.enqueue(x)
		switch y := x.Type.(type) {
		case *c99.TaggedStructType:
			g.enqueue(y)
		}
		return fmt.Sprintf("T%s", dict.S(x.Name))
	case *c99.PointerType:
		if x.Item.Kind() == c99.Void {
			return "uintptr"
		}

		switch {
		case x.Kind() == c99.Function:
			todo("")
		default:
			return fmt.Sprintf("*%s", g.ptyp(x.Item, ptr2uintptr, lvl+1))
		}
	case *c99.StructType:
		var buf bytes.Buffer
		buf.WriteString("struct{")
		layout := g.model.Layout(x)
		for i, v := range x.Fields {
			if v.Bits != 0 {
				if layout[i].Bitoff == 0 {
					fmt.Fprintf(&buf, "F%d %s;", layout[i].Offset, g.typ(layout[i].PackedType))
					if lvl == 0 {
						fmt.Fprintf(&buf, "\n")
					}
				}
				continue
			}

			switch {
			case v.Name == 0:
				fmt.Fprintf(&buf, "_ ")
			default:
				fmt.Fprintf(&buf, "%s ", mangleIdent(v.Name, true))
			}
			fmt.Fprintf(&buf, "%s;", g.ptyp(v.Type, ptr2uintptr, lvl+1))
			if lvl == 0 && ptr2uintptr && v.Type.Kind() == c99.Ptr {
				fmt.Fprintf(&buf, "// %s\n", g.ptyp(v.Type, false, lvl+1))
			}
		}
		buf.WriteByte('}')
		return buf.String()
	case *c99.TaggedEnumType:
		g.enqueue(x)
		return fmt.Sprintf("E%s", dict.S(x.Tag))
	case *c99.TaggedStructType:
		g.enqueue(x)
		return fmt.Sprintf("S%s", dict.S(x.Tag))
	case *c99.TaggedUnionType:
		g.enqueue(x)
		return fmt.Sprintf("U%s", dict.S(x.Tag))
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
		case
			c99.Double,
			c99.LongDouble:

			return fmt.Sprintf("float64")
		default:
			todo("", x)
		}
	case *c99.UnionType:
		al := int64(g.model.Alignof(x))
		sz := g.model.Sizeof(x)
		switch {
		case al == sz:
			return fmt.Sprintf("struct{X int%d}", 8*sz)
		default:
			return fmt.Sprintf("struct{X int%d; _ [%d]byte}", 8*al, sz-al)
		}
	default:
		todo("%v %T %v", t, x, ptr2uintptr)
	}
	panic("unreachable")
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
