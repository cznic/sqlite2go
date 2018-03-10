// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"unsafe"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
)

func (g *gen) isZeroInitializer(n *c99.Initializer) bool {
	if n == nil {
		return true
	}

	if n.Case == c99.InitializerExpr { // Expr
		return n.Expr.IsZero()
	}

	// '{' InitializerList CommaOpt '}'
	for l := n.InitializerList; l != nil; l = l.InitializerList {
		if !g.isZeroInitializer(l.Initializer) {
			return false
		}
	}
	return true
}

func (g *gen) isConstInitializer(n *c99.Initializer) bool {
	if n.Case == c99.InitializerCompLit { // '{' InitializerList CommaOpt '}'
		for l := n.InitializerList; l != nil; l = l.InitializerList {
			if !g.isConstInitializer(l.Initializer) {
				return false
			}
		}
		return true
	}

	// Expr
	if n.Expr.Operand.Value != nil && g.voidCanIgnore(n.Expr) {
		_, ok := n.Expr.Operand.Value.(*ir.StringValue)
		return !ok
	}

	return false
}

func (g *gen) allocBSS(t c99.Type) int64 {
	g.bss = roundup(g.bss, int64(g.model.Alignof(t)))
	r := g.bss
	g.bss += g.model.Sizeof(t)
	return r
}

func (g *gen) allocDS(t c99.Type, n *c99.Initializer) int64 {
	up := roundup(int64(len(g.ds)), int64(g.model.Alignof(t)))
	if n := up - int64(len(g.ds)); n != 0 {
		g.ds = append(g.ds, make([]byte, n)...)
	}
	r := len(g.ds)
	b := make([]byte, g.model.Sizeof(t))
	g.renderInitializer(b, t, n)
	g.ds = append(g.ds, b...)
	return int64(r)
}

func (g *gen) initializer(d *c99.Declarator) { // non TLD
	n := d.Initializer
	if n.Case == c99.InitializerExpr { // Expr
		switch {
		case g.escaped(d):
			g.w("\n*(*%s)(unsafe.Pointer(%s))", g.typ(d.Type), g.mangleDeclarator(d))
		default:
			g.w("\n%s", g.mangleDeclarator(d))
		}
		g.w(" = ")
		g.literal(d.Type, n)
		return
	}

	if g.isConstInitializer(n) {
		b := make([]byte, g.model.Sizeof(d.Type))
		g.renderInitializer(b, d.Type, n)
		switch {
		case g.escaped(d):
			g.w("\n%sCopy(%s, ts+%d, %d)", crt, g.mangleDeclarator(d), g.allocString(dict.ID(b)), len(b))
		default:
			g.w("\n%s = *(*%s)(unsafe.Pointer(ts+%d))", g.mangleDeclarator(d), g.typ(d.Type), g.allocString(dict.ID(b)))
		}
		return
	}

	switch {
	case g.escaped(d):
		g.w("\n*(*%s)(unsafe.Pointer(%s))", g.typ(d.Type), g.mangleDeclarator(d))
	default:
		g.w("\n%s", g.mangleDeclarator(d))
	}
	g.w(" = ")
	g.literal(d.Type, n)
}

func (g *gen) literal(t c99.Type, n *c99.Initializer) {
	switch x := c99.UnderlyingType(t).(type) {
	case *c99.ArrayType:
		if n.Expr != nil {
			switch x.Item.Kind() {
			case c99.Char:
				g.w("*(*%s)(unsafe.Pointer(", g.typ(t))
				switch n.Expr.Case {
				case c99.ExprString:
					b := make([]byte, x.Size.Value.(*ir.Int64Value).Value)
					copy(b, dict.S(int(n.Expr.Operand.Value.(*ir.StringValue).StringID)))
					if len(b) != 0 && b[len(b)-1] == 0 {
						b = b[:len(b)-1]
					}
					g.w("ts+%d", g.allocString(dict.ID(b)))
				default:
					todo("", g.position0(n), n.Expr.Case)
				}
				g.w("))")
			default:
				todo("", g.position0(n), x.Item.Kind())
			}
			return
		}

		g.w("%s{", g.typ(t))
		g.initializerListNL(n.InitializerList)
		if !g.isZeroInitializer(n) {
			index := 0
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(n))
				}
				g.w("%d: ", index)
				g.literal(x.Item, l.Initializer)
				g.w(", ")
				g.initializerListNL(n.InitializerList)
				index++
			}
		}
		g.w("}")
	case *c99.PointerType:
		if n.Expr.IsZero() || n.Expr.Operand.Value == c99.Null {
			g.w("0")
			return
		}

		g.value(n.Expr, false)
	case *c99.StructType:
		if n.Expr != nil {
			g.value(n.Expr, false)
			return
		}

		g.w("%s{", g.typ(t))
		g.initializerListNL(n.InitializerList)
		if !g.isZeroInitializer(n) {
			layout := g.model.Layout(t)
		more:
			fld := 0
			fields := x.Fields
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(n))
				}
				switch {
				case layout[fld].Bits < 0:
					fld++
					goto more
				case layout[fld].Bits > 0:
					todo("bit field %v", g.position0(n))
				}
				d := fields[fld]
				g.w("%s: ", mangleIdent(d.Name, true))
				g.literal(d.Type, l.Initializer)
				g.w(", ")
				g.initializerListNL(n.InitializerList)
				fld++
			}
		}
		g.w("}")
	case c99.TypeKind:
		if x.IsArithmeticType() {
			g.convert(n.Expr, t)
			return
		}
		todo("", g.position0(n), x)
	case *c99.UnionType:
		if n.Expr != nil {
			todo("", g.position0(n), x)
			return
		}

		g.w("%s{", g.typ(t))
		if !g.isZeroInitializer(n) {
			layout := g.model.Layout(t)
		more2:
			fld := 0
			fields := x.Fields
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(n))
				}
				switch {
				case layout[fld].Bits < 0:
					fld++
					goto more2
				case layout[fld].Bits > 0:
					todo("bit field %v", g.position0(n))
				}
				if fld != 0 {
					todo("", g.position0(n))
				}
				d := fields[fld]
				g.w("%s: ", mangleIdent(d.Name, true))
				g.literal(d.Type, l.Initializer)
				g.w(", ")
				g.initializerListNL(n.InitializerList)
				fld++
			}
		}
		g.w("}")
	default:
		todo("%v: %T", g.position0(n), x)
	}
}

func (g *gen) initializerListNL(n *c99.InitializerList) {
	if n.Len > 1 {
		g.w("\n")
	}
}

func (g *gen) renderInitializer(b []byte, t c99.Type, n *c99.Initializer) {
	switch x := c99.UnderlyingType(t).(type) {
	case *c99.ArrayType:
		if n.Expr != nil {
			switch y := n.Expr.Operand.Value.(type) {
			case *ir.StringValue:
				switch z := x.Item.Kind(); z {
				case c99.Char:
					copy(b, dict.S(int(y.StringID)))
				default:
					todo("", g.position0(n), z)
				}
			default:
				todo("%v: %T", g.position0(n), y)
			}
			return
		}

		itemSz := g.model.Sizeof(x.Item)
		var index int64
		for l := n.InitializerList; l != nil; l = l.InitializerList {
			if l.Designation != nil {
				todo("", g.position0(n))
			}
			lo := index * itemSz
			hi := lo + itemSz
			g.renderInitializer(b[lo:hi:hi], x.Item, l.Initializer)
			index++
		}
	case *c99.PointerType:
		switch {
		case n.Expr.IsNonZero():
			todo("", g.position0(n))
		case n.Expr.IsZero():
			// nop
		default:
			todo("", g.position0(n), n.Expr.Operand)
		}
	case *c99.StructType:
		if n.Expr != nil {
			todo("", g.position0(n))
		}

		layout := g.model.Layout(t)
		fld := 0
		fields := x.Fields
		for l := n.InitializerList; l != nil; l = l.InitializerList {
			for fields[fld].Bits < 0 || fields[fld].Name == 0 {
				fld++
			}
			if l.Designation != nil {
				todo("", g.position0(n))
			}
			fp := layout[fld]
			lo := layout[fld].Offset
			hi := lo + layout[fld].Size
			switch {
			case fp.Bits > 0:
				v := uint64(l.Initializer.Expr.Operand.Value.(*ir.Int64Value).Value)
				switch sz := g.model.Sizeof(fp.PackedType); sz {
				case 1:
					m := fp.Mask()
					x := uint64(b[lo])
					x = x&^m | v<<uint(fp.Bitoff)&m
					b[lo] = byte(x)
				case 2:
					m := fp.Mask()
					x := uint64(*(*uint16)(unsafe.Pointer(&b[lo])))
					x = x&^m | v<<uint(fp.Bitoff)&m
					*(*uint16)(unsafe.Pointer(&b[lo])) = uint16(x)
				case 4:
					m := fp.Mask()
					x := uint64(*(*uint32)(unsafe.Pointer(&b[lo])))
					x = x&^m | v<<uint(fp.Bitoff)&m
					*(*uint32)(unsafe.Pointer(&b[lo])) = uint32(x)
				case 8:
					m := fp.Mask()
					x := *(*uint64)(unsafe.Pointer(&b[lo]))
					x = x&^m | v<<uint(fp.Bitoff)&m
					*(*uint64)(unsafe.Pointer(&b[lo])) = x
				default:
					todo("", g.position0(n), sz, v)
				}
			default:
				g.renderInitializer(b[lo:hi:hi], fields[fld].Type, l.Initializer)
			}
			fld++
		}
	case c99.TypeKind:
		if x.IsIntegerType() {
			v := n.Expr.Operand.Value.(*ir.Int64Value).Value
			switch sz := g.model[x].Size; sz {
			case 1:
				*(*int8)(unsafe.Pointer(&b[0])) = int8(v)
			case 2:
				*(*int16)(unsafe.Pointer(&b[0])) = int16(v)
			case 4:
				*(*int32)(unsafe.Pointer(&b[0])) = int32(v)
			case 8:
				*(*int64)(unsafe.Pointer(&b[0])) = v
			default:
				todo("", g.position0(n), sz)
			}
			return
		}

		switch x {
		case c99.Float:
			switch x := n.Expr.Operand.Value.(type) {
			case *ir.Float32Value:
				*(*float32)(unsafe.Pointer(&b[0])) = x.Value
			case *ir.Float64Value:
				*(*float32)(unsafe.Pointer(&b[0])) = float32(x.Value)
			}
		case
			c99.Double,
			c99.LongDouble:

			switch x := n.Expr.Operand.Value.(type) {
			case *ir.Float64Value:
				*(*float64)(unsafe.Pointer(&b[0])) = x.Value
			case *ir.Int64Value:
				*(*float64)(unsafe.Pointer(&b[0])) = float64(x.Value)
			}
		default:
			todo("", g.position0(n), x)
		}
	case *c99.UnionType:
		if n.Expr != nil {
			todo("", g.position0(n))
		}

		layout := g.model.Layout(t)
		fld := 0
		fields := x.Fields
		for l := n.InitializerList; l != nil; l = l.InitializerList {
			for fields[fld].Bits < 0 || fields[fld].Name == 0 {
				fld++
			}
			if l.Designation != nil {
				todo("", g.position0(n))
			}
			if fld != 0 {
				todo("%v", g.position0(n))
			}
			fp := layout[fld]
			lo := layout[fld].Offset
			hi := lo + layout[fld].Size
			switch {
			case layout[fld].Bits > 0:
				v := uint64(l.Initializer.Expr.Operand.Value.(*ir.Int64Value).Value)
				switch sz := g.model.Sizeof(fp.PackedType); sz {
				case 1:
					m := fp.Mask()
					x := uint64(b[lo])
					x = x&^m | v<<uint(fp.Bitoff)&m
					b[lo] = byte(x)
				case 2:
					m := fp.Mask()
					x := uint64(*(*uint16)(unsafe.Pointer(&b[lo])))
					x = x&^m | v<<uint(fp.Bitoff)&m
					*(*uint16)(unsafe.Pointer(&b[lo])) = uint16(x)
				case 4:
					m := fp.Mask()
					x := uint64(*(*uint32)(unsafe.Pointer(&b[lo])))
					x = x&^m | v<<uint(fp.Bitoff)&m
					*(*uint32)(unsafe.Pointer(&b[lo])) = uint32(x)
				case 8:
					m := fp.Mask()
					x := *(*uint64)(unsafe.Pointer(&b[lo]))
					x = x&^m | v<<uint(fp.Bitoff)&m
					*(*uint64)(unsafe.Pointer(&b[lo])) = x
				default:
					todo("", g.position0(n), sz, v)
				}
			default:
				g.renderInitializer(b[lo:hi:hi], fields[fld].Type, l.Initializer)
			}
			fld++
		}
	default:
		todo("%v: %T", g.position0(n), x)
	}
}
