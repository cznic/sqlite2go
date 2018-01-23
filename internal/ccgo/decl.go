// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"fmt"
	"unsafe"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
)

func (g *gen) define(n *c99.Declarator) {
more:
	n = g.normalizeDeclarator(n)
	defined := true
	if n.Linkage == c99.LinkageExternal {
		_, defined = g.externs[n.Name()]
	}
	if _, ok := g.producedDeclarators[n]; defined && !ok {
		g.producedDeclarators[n] = struct{}{}
		g.tld(n)
	}

	for g.queue.Front() != nil {
		x := g.queue.Front()
		g.queue.Remove(x)
		switch y := x.Value.(type) {
		case *c99.Declarator:
			n = y
			goto more
		case *c99.NamedType:
			g.defineNamedType(y)
		case *c99.TaggedEnumType:
			g.defineTaggedEnumType(y)
		case *c99.TaggedStructType:
			g.defineTaggedStructType(y)
		default:
			todo("%T", y)
		}
	}
}

func (g *gen) defineNamedType(t *c99.NamedType) {
	if _, ok := g.producedNamedTypes[t.Name]; ok {
		return
	}

	g.producedNamedTypes[t.Name] = struct{}{}
	g.w("\ntype T%s = %s\n", dict.S(t.Name), g.typ(t.Type))
}

func (g *gen) defineTaggedEnumType(t *c99.TaggedEnumType) {
	if _, ok := g.producedEnumTags[t.Tag]; ok {
		return
	}

	g.producedEnumTags[t.Tag] = struct{}{}
	et := t.Type.(*c99.EnumType)
	tag := dict.S(t.Tag)
	g.w("\ntype E%s %s\n", tag, g.typ(et.Enums[0].Operand.Type))
	g.w("\nconst (")
	var iota int64
	for i, v := range et.Enums {
		val := v.Operand.Value.(*ir.Int64Value).Value
		if i == 0 {
			g.w("\nC%s E%s = iota", dict.S(v.Token.Val), tag)
			if val != 0 {
				g.w(" %+d", val)
			}
			iota = val + 1
			continue
		}

		g.w("\nC%s", dict.S(v.Token.Val))
		if val == iota {
			iota++
			continue
		}

		g.w(" = %d", val)
		iota = val + 1
	}
	g.w("\n)\n")

}

func (g *gen) defineTaggedStructType(t *c99.TaggedStructType) {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return
	}

	g.producedStructTags[t.Tag] = struct{}{}
	g.w("\ntype S%s %s\n", dict.S(t.Tag), g.typ(t.Type))
}

func (g *gen) tld(n *c99.Declarator) {
	t := underlyingType(n.Type)
	switch t.(type) {
	case *c99.StructType, *c99.UnionType:
		for _, v := range g.model.Layout(t) {
			if v.Bits != 0 {
				todo("", g.position(n))
			}
		}
	}

	if t.Kind() == c99.Function {
		g.functionDefinition(n)
		return
	}

	if n.Initializer == nil || n.Initializer.Expr != nil && n.Initializer.Expr.Operand.IsZero() {
		if g.escaped(n) {
			g.w("\n\nvar %s = bss + %d", g.mangleDeclarator(n), g.allocBSS(n.Type))
			return
		}

		switch x := t.(type) {
		case c99.TypeKind:
			switch x {
			case
				c99.Char,
				c99.Int:

				g.w("\nvar %s %s\n", g.mangleDeclarator(n), g.typ(n.Type))
			default:
				todo("%v: %v", g.position(n), x)
			}
		default:
			todo("%v: %s %v %T", g.position(n), dict.S(n.Name()), n.Type, x)
		}
		return
	}

	if g.escaped(n) {
		g.escapedTLD(n)
		return
	}

	if n.Initializer.Case == c99.InitializerExpr {
		g.w("\n\nvar %s = ", g.mangleDeclarator(n))
		g.convert(n.Initializer.Expr, n.Type)
		return
	}

	todo("%v: %s %v", g.position(n), dict.S(n.Name()), n.Type)
}

func (g *gen) escapedTLD(n *c99.Declarator) {
	switch x := underlyingType(n.Type).(type) {
	case *c99.ArrayType:
		g.w("\nvar %s = ds + %d\n", g.mangleDeclarator(n), g.allocDS(n.Type, n.Initializer))
	default:
		todo("%v: %T", g.position(n), x)
	}
}

func (g *gen) allocDS(t c99.Type, n *c99.Initializer) int64 {
	up := roundup(int64(len(g.ds)), int64(g.model.Alignof(t)))
	if n := up - int64(len(g.ds)); n != 0 {
		g.ds = append(g.ds, make([]byte, n)...)
	}
	r := len(g.ds)
	ds, dsBits, tsBits := g.renderInitializer(t, n)
	g.ds = append(g.ds, ds...)
	g.dsBits = append(g.dsBits, dsBits...)
	g.tsBits = append(g.tsBits, tsBits...)
	return int64(r)
}

func (g *gen) renderInitializer(t c99.Type, n *c99.Initializer) (ds, dsBits, tsBits []byte) {
	switch n.Case {
	case c99.InitializerExpr:
		return g.renderInitializerExpr(t, n.Expr)
	case c99.InitializerCompLit:
		switch x := t.(type) {
		case *c99.ArrayType:
			if x.Size.Type == nil || x.Size.IsZero() {
				todo("", g.position0(n), x)
			}
			len := x.Size.Value.(*ir.Int64Value).Value
			sz := g.model.Sizeof(x.Item)
			ds = make([]byte, len*sz)
			dsBits = make([]byte, len*sz)
			tsBits = make([]byte, len*sz)
			var index int64
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(l.Designation))
				}
				d, db, tb := g.renderInitializer(x.Item, l.Initializer)
				off := index * sz
				copy(ds[off:], d)
				copy(dsBits[off:], db)
				copy(tsBits[off:], tb)
				index++
			}
			return ds, dsBits, tsBits
		case *c99.StructType:
			sz := g.model.Sizeof(x)
			ds = make([]byte, sz)
			dsBits = make([]byte, sz)
			tsBits = make([]byte, sz)
			layout := g.model.Layout(x)
			field := 0
			for l := n.InitializerList; l != nil; l = l.InitializerList {
				if l.Designation != nil {
					todo("", g.position0(l.Designation))
				}
				if x.Fields[field].Bits != 0 {
					todo("", g.position0(l.Initializer))
				}
				d, db, tb := g.renderInitializer(x.Fields[field].Type, l.Initializer)
				off := layout[field].Offset
				copy(ds[off:], d)
				copy(dsBits[off:], db)
				copy(tsBits[off:], tb)
				field++
			}
			return ds, dsBits, tsBits
		default:
			todo("%v: %T", g.position0(n), x)
		}
	default:
		todo("", g.position0(n), n.Case)
	}
	panic("unreachable")
}

func (g *gen) renderInitializerExpr(t c99.Type, n *c99.Expr) (ds, dsBits, tsBits []byte) {
	ds = make([]byte, g.model.Sizeof(t))
	dsBits = make([]byte, len(ds))
	tsBits = make([]byte, len(ds))
	switch x := t.(type) {
	case *c99.ArrayType:
		switch y := x.Item.(type) {
		case c99.TypeKind:
			switch y {
			case c99.Char:
				switch z := n.Operand.Value.(type) {
				case *ir.StringValue:
					if z.Offset != 0 {
						todo("", g.position0(n))
					}
					copy(ds, dict.S(int(z.StringID)))
					return ds, dsBits, tsBits
				default:
					todo("%v: %T", g.position0(n), z)
				}
			default:
				todo("", g.position0(n), y)
			}
		default:
			todo("%v: %T", g.position0(n), y)
		}
	case *c99.PointerType:
		switch y := x.Item.(type) {
		case c99.TypeKind:
			switch y {
			case c99.Char:
				switch z := n.Operand.Value.(type) {
				case *ir.StringValue:
					*(*uintptr)(unsafe.Pointer(&ds[0])) = uintptr(g.allocString(int(z.StringID))) + z.Offset
					tsBits[0] = 1
				default:
					todo("%v: %T", g.position0(n), z)
				}
				return ds, dsBits, tsBits
			default:
				todo("", g.position0(n), y)
			}
		default:
			todo("%v: %T", g.position0(n), y)
		}
	case c99.TypeKind:
		switch x {
		case c99.Int:
			dsBits = make([]byte, len(ds))
			tsBits = make([]byte, len(ds))
			switch len(ds) {
			case 4:
				*(*int32)(unsafe.Pointer(&ds[0])) = int32(n.Operand.Value.(*ir.Int64Value).Value)
			default:
				todo("", g.position0(n), len(ds))
			}
			return ds, dsBits, tsBits
		default:
			todo("", g.position0(n), x)
		}
	default:
		todo("%v: %T", g.position0(n), x)
	}
	panic("unreachable")
}

func (g *gen) allocBSS(t c99.Type) int64 {
	g.bss = roundup(g.bss, int64(g.model.Alignof(t)))
	r := g.bss
	g.bss += g.model.Sizeof(t)
	return r
}

func (g *gen) functionDefinition(n *c99.Declarator) {
	g.nextLabel = 1
	g.w("\n\nfunc %s(tls *%sTLS", g.mangleDeclarator(n), crt)
	names := n.ParameterNames()
	t := n.Type.(*c99.FunctionType)
	if len(names) != len(t.Params) {
		if len(names) != 0 {
			if !(len(names) == 1 && names[0] == 0) {
				todo("", g.position(n), names, t.Params)
			}
		}

		names = make([]int, len(t.Params))
	}
	params := n.Parameters
	var escParams []*c99.Declarator
	switch {
	case len(t.Params) == 1 && t.Params[0].Kind() == c99.Void:
		// nop
	default:
		for i, v := range t.Params {
			var param *c99.Declarator
			if i < len(params) {
				param = params[i]
			}
			nm := names[i]
			g.w(", ")
			switch {
			case param != nil && param.AddressTaken:
				g.w("a%s %s", dict.S(nm), g.typ(v))
				escParams = append(escParams, param)
			default:
				if x, ok := underlyingType(v).(*c99.PointerType); ok && x.Item.Kind() == c99.Function {
					v = x.Item
				}
				g.w("%s %s", mangleIdent(nm, false), g.typ(v))
				if isVaList(v) {
					continue
				}

				if v.Kind() == c99.Ptr {
					g.w(" /* %s */", g.ptyp(v, false))
				}
			}
		}
		if t.Variadic {
			g.w(", %s...interface{}", ap)
		}
	}
	g.w(")")
	void := t.Result.Kind() == c99.Void
	if !void {
		g.w("(r %s)", g.typ(t.Result))
	}
	g.functionBody(n.FunctionDefinition.FunctionBody, n.FunctionDefinition.LocalVariables(), void, escParams, t.Result)
}

func (g *gen) functionBody(n *c99.FunctionBody, vars []*c99.Declarator, void bool, escParams []*c99.Declarator, rt c99.Type) {
	if vars == nil {
		vars = []*c99.Declarator{}
	}
	f := false
	g.compoundStmt(n.CompoundStmt, vars, nil, !void, nil, nil, escParams, rt, &f)
}

func (g *gen) mangleDeclarator(n *c99.Declarator) string {
	nm := n.Name()
	if num, ok := g.nums[n]; ok {
		return fmt.Sprintf("_%d%s", num, dict.S(nm))
	}

	if n.IsField {
		return mangleIdent(nm, true)
	}

	if n.Linkage == c99.LinkageExternal {
		switch {
		case g.externs[n.Name()] == nil:
			return crt + mangleIdent(nm, true)
		default:
			return mangleIdent(nm, true)
		}
	}

	return mangleIdent(nm, false)
}

func (g *gen) normalizeDeclarator(n *c99.Declarator) *c99.Declarator {
	if n == nil {
		return nil
	}

	if n.Linkage == c99.LinkageExternal {
		if d, ok := g.externs[n.Name()]; ok {
			return d
		}
	}
	return n
}

func (g *gen) declaration(n *c99.Declaration) {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	g.initDeclaratorListOpt(n.InitDeclaratorListOpt)
}

func (g *gen) initDeclaratorListOpt(n *c99.InitDeclaratorListOpt) {
	if n == nil {
		return
	}

	g.initDeclaratorList(n.InitDeclaratorList)
}

func (g *gen) initDeclaratorList(n *c99.InitDeclaratorList) {
	for ; n != nil; n = n.InitDeclaratorList {
		g.initDeclarator(n.InitDeclarator)
	}
}

func (g *gen) initDeclarator(n *c99.InitDeclarator) {
	d := n.Declarator
	if d.DeclarationSpecifier.IsStatic() {
		return
	}

	if d.Referenced == 0 && d.Initializer == nil {
		return
	}

	if n.Case == c99.InitDeclaratorInit { // Declarator '=' Initializer
		g.initializer(d)
	}
}
