// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"unsafe"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
)

func (g *gen) initializer(d *c99.Declarator) {
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

	switch x := n.InitializerList.Operand.Value.(type) {
	case *ir.CompositeValue:
		switch y := underlyingType(d.Type).(type) {
		case *c99.ArrayType:
			b := make([]byte, g.model.Sizeof(d.Type))
			g.arrayCompositeValue(b, x, y.Item)
			g.w("\n%sCopy(%s, ts+%d, %d)", crt, g.mangleDeclarator(d), g.allocString(dict.ID(b)), len(b))
		default:
			todo("%v: %T", g.position(d), y)
		}
	default:
		todo("%v: %T", g.position(d), x)
	}
}

func (g *gen) arrayCompositeValue(b []byte, v *ir.CompositeValue, item c99.Type) {
	itemSz := g.model.Sizeof(item)
	var index int64
	for _, v := range v.Values {
		switch x := v.(type) {
		case c99.Operand:
			lo := index * itemSz
			hi := lo + itemSz
			g.arrayCompositeValueItem(b[lo:hi:hi], x, itemSz)
			index++
		default:
			todo("%T", x)
		}
	}
}

func (g *gen) arrayCompositeValueItem(n []byte, op c99.Operand, itemSz int64) {
	switch x := op.Type.(type) {
	case c99.TypeKind:
		switch x {
		case c99.Int:
			switch itemSz {
			case 4:
				*(*int32)(unsafe.Pointer(&n[0])) = int32(op.Value.(*ir.Int64Value).Value)
			default:
				todo("", itemSz)
			}
		default:
			todo("", x)
		}
	default:
		todo("%T", x)
	}
}
