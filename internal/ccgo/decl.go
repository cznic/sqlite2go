// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"fmt"
	"path/filepath"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
	"go/ast"
	"strconv"
)

func (g *gen) define(n *c99.Declarator) []ast.Decl {
	var out []ast.Decl
more:
	n = g.normalizeDeclarator(n)
	defined := true
	if n.Linkage == c99.LinkageExternal {
		_, defined = g.externs[n.Name()]
	}
	if _, ok := g.producedDeclarators[n]; defined && !ok {
		g.producedDeclarators[n] = struct{}{}
		out = append(out, g.tld(n)...)
	}

	for g.queue.Front() != nil {
		x := g.queue.Front()
		g.queue.Remove(x)
		switch y := x.Value.(type) {
		case *c99.Declarator:
			n = y
			goto more
		case *c99.EnumType:
			out = append(out, g.defineEnumType(y)...)
		case *c99.NamedType:
			switch z := underlyingType(y.Type, true).(type) {
			case *c99.StructType:
				switch {
				case z.Tag == 0:
					out = append(out, g.defineNamedType(y)...)
				default:
					g.enqueue(y.Type)
				}
			case *c99.UnionType:
				switch {
				case z.Tag == 0:
					out = append(out, g.defineNamedType(y)...)
				default:
					g.enqueue(y.Type)
				}
			default:
				g.enqueue(y.Type)
			}
		case *c99.TaggedEnumType:
			out = append(out, g.defineTaggedEnumType(y)...)
		case *c99.TaggedStructType:
			out = append(out, g.defineTaggedStructType(y)...)
		case *c99.TaggedUnionType:
			out = append(out, g.defineTaggedUnionType(y)...)
		case
			*c99.ArrayType,
			*c99.FunctionType,
			*c99.PointerType,
			*c99.StructType,
			*c99.UnionType,
			c99.TypeKind:

			// nop
		default:
			todo("%T %v", y, y)
		}
	}
	return out
}

func (g *gen) defineEnumType(t *c99.EnumType) []ast.Decl {
	if t.Tag == 0 {
		return nil
	}
	return g.defineTaggedEnumType(&c99.TaggedEnumType{Tag: t.Tag, Type: t})
}

func (g *gen) defineTaggedEnumType(t *c99.TaggedEnumType) []ast.Decl {
	if _, ok := g.producedEnumTags[t.Tag]; ok {
		return nil
	}

	g.producedEnumTags[t.Tag] = struct{}{}
	et := t.Type.(*c99.EnumType)
	tag := string(dict.S(t.Tag))

	var (
		iota  int64
		specs []ast.Spec
	)
	for i, v := range et.Enums {
		val := v.Operand.Value.(*ir.Int64Value).Value
		if i == 0 {
			var ev ast.Expr = ident("iota")
			if val > 0 {
				ev = add(ev, intLit(val))
			} else if val < 0 {
				ev = sub(ev, intLit(-val))
			}
			specs = append(specs, valSpec(
				"C"+string(dict.S(v.Token.Val)),
				ident("E"+tag),
				ev,
			))
			iota = val + 1
			continue
		}

		var ev ast.Expr
		if val != iota {
			ev = intLit(val)
			iota = val + 1
		} else {
			iota++
		}
		specs = append(specs, valSpec(
			"C"+string(dict.S(v.Token.Val)),
			nil,
			ev,
		))
	}
	return []ast.Decl{
		typeAlias(
			"E"+tag,
			g.typ(et.Enums[0].Operand.Type),
		),
		consts(specs...),
	}
}

func (g *gen) defineNamedType(t *c99.NamedType) []ast.Decl {
	if _, ok := g.producedNamedTypes[t.Name]; ok {
		return nil
	}

	g.producedNamedTypes[t.Name] = struct{}{}
	decl := typeAlias("T"+string(dict.S(t.Name)), g.typ(t.Type))
	return []ast.Decl{decl}
}

func (g *gen) defineTaggedStructType(t *c99.TaggedStructType) []ast.Decl {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return nil
	}

	switch {
	case t.Type == nil:
		g.opaqueStructTags[t.Tag] = struct{}{}
		return nil
	default:
		g.producedStructTags[t.Tag] = struct{}{}
		var out []ast.Decl
		out = append(out, typeDef(
			"S"+string(dict.S(t.Tag)),
			g.typ(t.Type),
		))
		if isTesting {
			var body []ast.Stmt
			st := c99.UnderlyingType(t.Type).(*c99.StructType)
			fields := st.Fields
			for i, v := range g.model.Layout(st) {
				if v.Bits < 0 {
					continue
				}

				if v.Bits != 0 && v.Bitoff != 0 {
					continue
				}

				if v.Bits != 0 && v.Bitoff == 0 {
					body = append(body, structOffsetTests(
						"S"+string(dict.S(t.Tag)),
						"F"+strconv.FormatInt(v.Offset, 10),
						v.Offset, g.model.Sizeof(v.PackedType),
					)...)
					continue
				}

				if fields[i].Name == 0 {
					continue
				}
				body = append(body, structOffsetTests(
					"S"+string(dict.S(t.Tag)),
					mangleIdent(fields[i].Name, true),
					v.Offset, v.Size,
				)...)
			}
			out = append(out, initFunc(body...))
		}
		return out
	}
}

func (g *gen) defineTaggedUnionType(t *c99.TaggedUnionType) []ast.Decl {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return nil
	}

	g.producedStructTags[t.Tag] = struct{}{}

	var out []ast.Decl
	out = append(out, typeDef(
		"U"+string(dict.S(t.Tag)),
		g.typ(t.Type),
	))
	if !isTesting {
		out = append(out, initFunc(
			assertSizeof(
				compositeLit("U"+string(dict.S(t.Tag))),
				g.model.Sizeof(t),
			),
		))
	}
	return out
}

func (g *gen) tld(n *c99.Declarator) (out []ast.Decl) {
	nm := n.Name()
	t := c99.UnderlyingType(n.Type)
	if t.Kind() == c99.Function {
		return g.functionDefinition(n)
	}

	switch x := n.Type.(type) {
	case
		*c99.NamedType,
		*c99.TaggedStructType,
		*c99.TaggedUnionType:

		g.enqueue(x)
	}

	pos := g.position(n)
	pos.Filename, _ = filepath.Abs(pos.Filename)
	if !isTesting {
		pos.Filename = filepath.Base(pos.Filename)
	}
	defer func() {
		if len(out) != 0 {
			out[0] = commentDeclf(out[0], "%s %s, escapes: %v, %v",
				g.mangleDeclarator(n), g.typeComment(n.Type), g.escaped(n), pos,
			)
		}
	}()

	if n.Initializer != nil && n.Linkage == c99.LinkageExternal {
		g.initializedExterns[nm] = struct{}{}
	}
	if g.isZeroInitializer(n.Initializer) {
		if isVaList(n.Type) {
			out = append(out, varDecl(
				g.mangleDeclarator(n),
				ptr(sliceOf(emptyInf())),
				nil,
			))
			return
		}

		if g.escaped(n) {
			out = append(out, g.bssVarFor(n))
			return
		}

		switch x := t.(type) {
		case *c99.StructType:
			out = append(out, g.bssVarFor(n))
		case *c99.PointerType:
			out = append(out, varDecl(
				g.mangleDeclarator(n), ident("uintptr"), nil,
			))
		case
			*c99.EnumType,
			c99.TypeKind:

			if x.IsArithmeticType() {
				out = append(out, varDecl(
					g.mangleDeclarator(n),
					g.typI(n.Type),
					nil,
				))
				break
			}

			todo("%v: %v", g.position(n), x)
		default:
			todo("%v: %s %v %T", g.position(n), dict.S(nm), n.Type, x)
		}
		return
	}

	if g.escaped(n) {
		out = append(out, g.escapedTLD(n)...)
		return out
	}

	switch n.Initializer.Case {
	case c99.InitializerExpr: // Expr
		out = append(out, varDecl(
			g.mangleDeclarator(n),
			nil,
			g.convert(n.Initializer.Expr, n.Type),
		))
	default:
		todo("", g.position0(n), n.Initializer.Case)
	}
	return out
}

func (g *gen) escapedTLD(n *c99.Declarator) []ast.Decl {
	if g.isConstInitializer(n.Type, n.Initializer) {
		return []ast.Decl{varDecl(
			g.mangleDeclarator(n), nil,
			addInt(ident("ds"), g.allocDS(n.Type, n.Initializer)),
		)}
	}

	switch x := c99.UnderlyingType(n.Type).(type) {
	case *c99.ArrayType:
		if x.Item.Kind() == c99.Char && n.Initializer.Expr.Operand.Value != nil {
			return []ast.Decl{varDecl(
				g.mangleDeclarator(n), nil,
				addInt(ident("ds"), g.allocDS(n.Type, n.Initializer)),
			)}
		}
	}

	return []ast.Decl{
		commentDeclf(g.bssVarFor(n), "%v", n.Type),
		initFunc(
			setPtrUnsafe(
				g.typ(n.Type),
				g.mangleDeclaratorI(n),
				g.literal(n.Type, n.Initializer),
			),
		),
	}
}

func (g *gen) functionDefinition(n *c99.Declarator) []ast.Decl {
	if n.FunctionDefinition == nil {
		return nil
	}

	g.mainFn = n.Name() == idMain && n.Linkage == c99.LinkageExternal
	g.nextLabel = 1
	pos := g.position(n)
	pos.Filename, _ = filepath.Abs(pos.Filename)
	if !isTesting {
		pos.Filename = filepath.Base(pos.Filename)
	}

	names := n.ParameterNames()
	t := n.Type.(*c99.FunctionType)
	switch {
	case len(names) < len(t.Params):
		for len(names) < len(t.Params) {
			names = append(names, 0)
		}
	case len(names) != 0 && len(t.Params) == 0:
		t.Params = make([]c99.Type, len(names))
		for i := range t.Params {
			t.Params[i] = c99.Int
		}
	case len(names) != len(t.Params):
		if len(names) != 0 {
			if !(len(names) == 1 && names[0] == 0) {
				todo("K&R C %v %v %v", g.position(n), names, t.Params)
			}
		}

		names = make([]int, len(t.Params))
	}
	var (
		escParams []*c99.Declarator
		par       []*ast.Field
	)
	params := n.Parameters
	_, ok := g.fixArgs[n]
	switch {
	case len(t.Params) == 1 && t.Params[0].Kind() == c99.Void:
		// nop
	case ok:
		par = append(par, field("_", variadic(emptyInf()), nil))
	default:
		for i, v := range t.Params {
			var param *c99.Declarator
			if i < len(params) {
				param = params[i]
			}
			nm := names[i]

			switch {
			case param != nil && g.escaped(param):
				par = append(par, field(
					"a"+string(dict.S(nm)),
					g.typI(v), nil,
				))
				escParams = append(escParams, param)
			default:
				var comment *ast.CommentGroup
				if v.Kind() == c99.Ptr {
					comment = commentf("%s", g.typeComment(v))
				}
				switch c99.UnderlyingType(v).(type) {
				case *c99.ArrayType:
					comment = commentf("%v", g.typ(v))
					par = append(par, field(
						mangleIdent(nm, false),
						ident("uintptr"),
						comment,
					))
				default:
					if isVaList(v) {
						comment = nil
					}
					par = append(par, field(
						mangleIdent(nm, false),
						g.typI(v),
						comment,
					))
				}

			}
		}
		if t.Variadic {
			par = append(par, field(ap, variadic(emptyInf()), nil))
		}
	}

	// return value
	void := t.Result.Kind() == c99.Void
	var ret []*ast.Field
	if !void {
		var comment *ast.CommentGroup
		if t.Result.Kind() == c99.Ptr {
			comment = commentf("%s", g.typeComment(t.Result))
		}
		ret = append(ret, field("r", g.typI(t.Result), comment))
	}

	// local vars
	vars := n.FunctionDefinition.LocalVariables()
	if n.Alloca {
		vars = append(append([]*c99.Declarator(nil), vars...), allocaDeclarator)
	}

	// final declaration
	fdecl := funcDecl(
		g.mangleDeclarator(n),
		par, ret,
		g.functionBody(n.FunctionDefinition.FunctionBody, vars, void, n.Parameters, escParams),
	)
	return []ast.Decl{
		commentDeclf(
			withTLS(fdecl),
			"%s is defined at %v", g.mangleDeclarator(n), pos,
		),
	}
}

func (g *gen) functionBody(n *c99.FunctionBody, vars []*c99.Declarator, void bool, params, escParams []*c99.Declarator) *ast.BlockStmt {
	if vars == nil {
		vars = []*c99.Declarator{}
	}
	return block(g.compoundStmt(n.CompoundStmt, vars, nil, !void, nil, nil, params, escParams, false)...)
}

func (g *gen) mangleDeclaratorI(n *c99.Declarator) *ast.Ident {
	return ident(g.mangleDeclarator(n))
}
func (g *gen) mangleDeclarator(n *c99.Declarator) string {
	nm := n.Name()
	if n.Linkage == c99.LinkageInternal {
		if m := g.staticDeclarators[nm]; m != nil {
			n = m
		}
	}
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

	switch n.Linkage {
	case c99.LinkageExternal:
		if d, ok := g.externs[n.Name()]; ok {
			n = d
		}
	}

	if n.Definition != nil {
		return n.Definition
	}

	return n
}

func (g *gen) declaration(n *c99.Declaration, deadCode *bool) []ast.Stmt {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	return g.initDeclaratorListOpt(n.InitDeclaratorListOpt, deadCode)
}

func (g *gen) initDeclaratorListOpt(n *c99.InitDeclaratorListOpt, deadCode *bool) []ast.Stmt {
	if n == nil {
		return nil
	}

	return g.initDeclaratorList(n.InitDeclaratorList, deadCode)
}

func (g *gen) initDeclaratorList(n *c99.InitDeclaratorList, deadCode *bool) []ast.Stmt {
	var out []ast.Stmt
	for ; n != nil; n = n.InitDeclaratorList {
		out = append(out, g.initDeclarator(n.InitDeclarator, deadCode)...)
	}
	return out
}

func (g *gen) initDeclarator(n *c99.InitDeclarator, deadCode *bool) []ast.Stmt {
	d := n.Declarator
	if d.DeclarationSpecifier.IsStatic() {
		return nil
	}

	if d.Referenced == 0 && d.Initializer == nil {
		return nil
	}

	if n.Case == c99.InitDeclaratorInit { // Declarator '=' Initializer
		return g.initializer(d)
	}
	return nil
}
