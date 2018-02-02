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
		return n.Expr.Operand.IsZero()
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
		g.convert(n.Expr, d.Type)
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
				g.w("*(*%s)(unsafe.Pointer(")
				g.value(n.Expr)
				g.w("))", g.typ(t), g.allocString(int(n.Expr.Operand.Value.(*ir.StringValue).StringID)))
			default:
				todo("", g.position0(n), x.Item.Kind())
			}
			return
		}

		g.w("%s{", g.typ(t))
		if !g.isZeroInitializer(n) {
			index := 0
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(n))
				}
				g.w("%d: ", index)
				g.literal(x.Item, l.Initializer)
				g.w(", ")
				index++
			}
		}
		g.w("}")
	case *c99.PointerType:
		if n.Expr.Operand.IsZero() || n.Expr.Operand.Address == c99.Null {
			g.w("nil")
			return
		}

		if x.Item.Kind() == c99.Function {
			g.value(n.Expr)
			return
		}

		g.w("(*%s)(unsafe.Pointer(", g.ptyp(x.Item, false))
		g.value(n.Expr)
		g.w("))")
	case *c99.StructType:
		if n.Expr != nil {
			g.value(n.Expr)
			return
		}

		g.w("%s{", g.typ(t))
		if !g.isZeroInitializer(n) {
			layout := g.model.Layout(t)
			fld := 0
			fields := x.Fields
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(n))
				}
				if layout[fld].Bits != 0 {
					todo("%v: bit field", g.position0(n))
				}
				d := fields[fld]
				g.w("%s: ", mangleIdent(d.Name, true))
				g.literal(d.Type, l.Initializer)
				g.w(", ")
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
	default:
		todo("%v: %T", g.position0(n), x)
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
		case n.Expr.Operand.IsNonzero():
			todo("", g.position0(n))
		case n.Expr.Operand.IsZero():
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
			if l.Designation != nil {
				todo("", g.position0(n))
			}
			if layout[fld].Bits != 0 {
				todo("%v: bit field", g.position0(n))
			}
			lo := layout[fld].Offset
			hi := lo + layout[fld].Size
			g.renderInitializer(b[lo:hi:hi], fields[fld].Type, l.Initializer)
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
			*(*float32)(unsafe.Pointer(&b[0])) = float32(n.Expr.Operand.Value.(*ir.Float64Value).Value)
		case c99.Double:
			*(*float64)(unsafe.Pointer(&b[0])) = n.Expr.Operand.Value.(*ir.Float64Value).Value
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
		i := 0
		for l := n.InitializerList; l != nil; l = l.InitializerList {
			if i != 0 {
				todo("%v", g.position0(n))
			}
			if l.Designation != nil {
				todo("", g.position0(n))
			}
			if layout[fld].Bits != 0 {
				todo("%v: bit field", g.position0(n))
			}
			lo := layout[fld].Offset
			hi := lo + layout[fld].Size
			g.renderInitializer(b[lo:hi:hi], fields[fld].Type, l.Initializer)
			fld++
			i++
		}
	default:
		todo("%v: %T", g.position0(n), x)
	}
}
