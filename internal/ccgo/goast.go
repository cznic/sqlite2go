// Copyright 2018 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/token"
	"io"
	"strconv"
	"strings"
)

func ident(name string) *ast.Ident {
	return &ast.Ident{Name: name}
}

func binaryExpr(a ast.Expr, op token.Token, b ast.Expr) ast.Expr {
	if _, ok := a.(*ast.BinaryExpr); ok {
		a = paren(a)
	}
	if _, ok := b.(*ast.BinaryExpr); ok {
		b = paren(b)
	}
	return &ast.BinaryExpr{
		X: a, Op: op, Y: b,
	}
}

func unaryExpr(op token.Token, a ast.Expr) ast.Expr {
	return &ast.UnaryExpr{
		X: a, Op: op,
	}
}

func eq(a, b ast.Expr) ast.Expr {
	return binaryExpr(a, token.EQL, b)
}

func neq(a, b ast.Expr) ast.Expr {
	return binaryExpr(a, token.NEQ, b)
}

func add(a, b ast.Expr) ast.Expr {
	return binaryExpr(a, token.ADD, b)
}

func sub(a, b ast.Expr) ast.Expr {
	return binaryExpr(a, token.SUB, b)
}

func mul(a, b ast.Expr) ast.Expr {
	return binaryExpr(a, token.MUL, b)
}

func div(a, b ast.Expr) ast.Expr {
	return binaryExpr(a, token.QUO, b)
}

func shle(a, n ast.Expr) ast.Expr {
	return binaryExpr(a, token.SHL, n)
}

func shre(a, n ast.Expr) ast.Expr {
	return binaryExpr(a, token.SHR, n)
}

func shl(a ast.Expr, n int) ast.Expr {
	return shle(a, intLit(int64(n)))
}

func shr(a ast.Expr, n int) ast.Expr {
	return shre(a, intLit(int64(n)))
}

func inc(a ast.Expr) ast.Stmt {
	return &ast.IncDecStmt{
		Tok: token.INC, X: a,
	}
}

func dec(a ast.Expr) ast.Stmt {
	return &ast.IncDecStmt{
		Tok: token.DEC, X: a,
	}
}

func incn(a ast.Expr, n int64) ast.Stmt {
	return &ast.AssignStmt{
		Tok: token.ADD_ASSIGN,
		Lhs: []ast.Expr{a},
		Rhs: []ast.Expr{intLit(n)},
	}
}

func decn(a ast.Expr, n int64) ast.Stmt {
	return &ast.AssignStmt{
		Tok: token.SUB_ASSIGN,
		Lhs: []ast.Expr{a},
		Rhs: []ast.Expr{intLit(n)},
	}
}

func addInt(a ast.Expr, v int64) ast.Expr {
	if v == 0 {
		return a
	}
	return add(a, intLit(v))
}

func eqZero(a ast.Expr) ast.Expr {
	return eq(a, intLit(0))
}

func neqZero(a ast.Expr) ast.Expr {
	return neq(a, intLit(0))
}

func takeAddr(x ast.Expr) ast.Expr {
	return unaryExpr(token.AND, x)
}

func ptrVal(x ast.Expr) ast.Expr {
	return &ast.StarExpr{X: x}
}

func index(x, n ast.Expr) ast.Expr {
	return &ast.IndexExpr{X: x, Index: n}
}

func ptr(x ast.Expr) ast.Expr {
	return &ast.StarExpr{X: x}
}

func paren(x ast.Expr) ast.Expr {
	switch x := x.(type) {
	case *ast.ParenExpr, *ast.CallExpr, *ast.Ident, *ast.BasicLit:
		return x
	}
	return &ast.ParenExpr{X: x}
}

func sliceTyp(name string) ast.Expr {
	return sliceOf(ident(name))
}

func arrayTyp(name string, n int) ast.Expr {
	return &ast.ArrayType{Elt: ident(name), Len: intLit(int64(n))}
}

func emptyInf() ast.Expr {
	return ident("interface{}")
}

func unsafePtr() ast.Expr {
	return sel(ident("unsafe"), "Pointer")
}

func uintPtr() ast.Expr {
	return ident("uintptr")
}

func toUnsafePtr(x ast.Expr) ast.Expr {
	return cast(
		x, unsafePtr(),
	)
}

func toUintptr(x ast.Expr) ast.Expr {
	return cast(
		x, uintPtr(),
	)
}

func toUintptrMul(x ast.Expr, n int64) ast.Expr {
	return mul(intLit(n), toUintptr(x))
}

func sliceOf(x ast.Expr) ast.Expr {
	return &ast.ArrayType{Elt: x}
}

func variadic(x ast.Expr) ast.Expr {
	return &ast.Ellipsis{Elt: x}
}

func typeAlias(name, typ string) ast.Decl {
	// type A = B
	return &ast.GenDecl{
		Tok: token.TYPE,
		Specs: []ast.Spec{&ast.TypeSpec{
			Name:   ident(name),
			Type:   ident(typ),
			Assign: 1,
		}},
	}
}

func typeDef(name, typ string) ast.Decl {
	// type A B
	return &ast.GenDecl{
		Tok: token.TYPE,
		Specs: []ast.Spec{&ast.TypeSpec{
			Name: ident(name),
			Type: ident(typ),
		}},
	}
}

func varDecl(name string, typ, v ast.Expr) ast.Decl {
	return defineVars(valSpec(name, typ, v))
}

func declStmt(d ast.Decl) ast.Stmt {
	return &ast.DeclStmt{Decl: d}
}

func valSpec(name string, typ, v ast.Expr) ast.Spec {
	var vals []ast.Expr
	if v != nil {
		vals = []ast.Expr{v}
	}
	return &ast.ValueSpec{
		Names: []*ast.Ident{
			ident(name),
		},
		Type: typ, Values: vals,
	}
}

func defineVars(specs ...ast.Spec) ast.Decl {
	d := &ast.GenDecl{
		Tok:   token.VAR,
		Specs: specs,
	}
	if len(specs) > 1 {
		d.Lparen = 1
		d.Rparen = 1
	}
	return d
}

func constSpec(name string, v ast.Expr) ast.Spec {
	return valSpec(name, nil, v)
}

func consts(specs ...ast.Spec) ast.Decl {
	return &ast.GenDecl{
		Tok:   token.CONST,
		Specs: specs,
	}
}

func block(stmts ...ast.Stmt) *ast.BlockStmt {
	return &ast.BlockStmt{List: stmts}
}

func ifStmt(cond ast.Expr, stmts ...ast.Stmt) ast.Stmt {
	return &ast.IfStmt{
		Cond: cond,
		Body: block(stmts...),
	}
}

func ifElseStmt(cond ast.Expr, then, els []ast.Stmt) ast.Stmt {
	return &ast.IfStmt{
		Cond: cond,
		Body: block(then...),
		Else: block(els...),
	}
}

func field(name string, typ ast.Expr, comment *ast.CommentGroup) *ast.Field {
	f := &ast.Field{Type: typ, Doc: comment}
	if name != "" {
		f.Names = []*ast.Ident{ident(name)}
	}
	if comment != nil && len(comment.List) != 0 {
		c := comment.List[0]
		if strings.HasPrefix(c.Text, "//") {
			c.Text = "/*" + strings.TrimPrefix(c.Text, "//") + " */"
		}
	}
	return f
}

func initFunc(stmts ...ast.Stmt) ast.Decl {
	// func init() { ..stmts.. }
	return voidFunc("init", stmts...)
}

func voidFunc(name string, stmts ...ast.Stmt) ast.Decl {
	// func name() { ..stmts.. }
	return funcDecl(name, nil, nil, block(stmts...))
}

func funcDecl(name string, par, ret []*ast.Field, body *ast.BlockStmt) *ast.FuncDecl {
	return &ast.FuncDecl{
		Name: ident(name),
		Type: &ast.FuncType{
			Params:  &ast.FieldList{List: par},
			Results: &ast.FieldList{List: ret},
		},
		Body: body,
	}
}

func assignStmt(a ast.Expr, tok token.Token, b ast.Expr) ast.Stmt {
	return &ast.AssignStmt{
		Lhs: []ast.Expr{a},
		Tok: tok,
		Rhs: []ast.Expr{b},
	}
}

func define(a, b ast.Expr) ast.Stmt {
	return assignStmt(a, token.DEFINE, b)
}

func assign(a, b ast.Expr) ast.Stmt {
	if b == nil {
		b = ident("nil")
	}
	return assignStmt(a, token.ASSIGN, b)
}

func assertStmt(val, exp ast.Expr) ast.Stmt {
	// if n := %val; n != %exp { panic(n) }
	v := ident("n")
	return &ast.IfStmt{
		Init: define(v, val),
		Cond: neq(v, exp),
		Body: block(
			callPanic(v),
		),
	}
}

func intLit(v int64) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: strconv.FormatInt(v, 10),
	}
}

func intLitF(f string, v int64) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: fmt.Sprintf(f, v),
	}
}

func uintLit(v uint64) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: strconv.FormatUint(v, 10),
	}
}

func uintLitF(f string, v uint64) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: fmt.Sprintf(f, v),
	}
}

func ptrLit(v uintptr) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: "0x" + strconv.FormatUint(uint64(v), 16),
	}
}

func ptrLitF(f string, v uintptr) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.INT,
		Value: fmt.Sprintf(f, v),
	}
}

func floatLit(v float64) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.FLOAT,
		Value: strconv.FormatFloat(v, 'g', -1, 64),
	}
}

func charLit(r rune) ast.Expr {
	return &ast.BasicLit{
		Kind:  token.CHAR,
		Value: strconv.QuoteRuneToASCII(r),
	}
}

func compositeLit(typ string, elts ...ast.Expr) ast.Expr {
	return &ast.CompositeLit{Type: ident(typ), Elts: elts}
}

func compositeLitT(fields []*ast.Field, elts ...ast.Expr) ast.Expr {
	return &ast.CompositeLit{
		Type: &ast.StructType{Fields: &ast.FieldList{List: fields}},
		Elts: elts,
	}
}

func pair(k, v ast.Expr) ast.Expr {
	return &ast.KeyValueExpr{Key: k, Value: v}
}

func callFunc(pkg, fnc string, args ...ast.Expr) ast.Expr {
	var f ast.Expr = ident(fnc)
	if pkg != "" {
		f = sel(ident(pkg), fnc)
	}
	return call(f, args...)
}

func cast(x, to ast.Expr) ast.Expr {
	if to == nil {
		panic("type should not be nil")
	}
	return call(to, x)
}

func call(fnc ast.Expr, args ...ast.Expr) ast.Expr {
	if fnc == nil {
		panic("func should not be nil")
	}
	for i := range args {
		switch a := args[i].(type) {
		case *ast.ParenExpr:
			args[i] = a.X
		}
	}
	if s, ok := fnc.(*ast.SelectorExpr); ok {
		if pkg, ok := s.X.(*ast.Ident); ok && pkg.Name == "unsafe" {
			switch s.Sel.Name {
			case "Offsetof", "Sizeof":
				// Sizeof( (Struct{}).Fld ) -> Sizeof( Struct{}.Fld )
				if len(args) == 1 {
					if fs, ok := args[0].(*ast.SelectorExpr); ok {
						if p, ok := fs.X.(*ast.ParenExpr); ok {
							fs.X = p.X
						}
					}
				}
			}
		}
	}
	return &ast.CallExpr{
		Fun: fnc, Args: args,
	}
}

func exprStmt(e ast.Expr) ast.Stmt {
	return &ast.ExprStmt{X: e}
}

func sel(root ast.Expr, fld string) ast.Expr {
	return selStmt(root, ident(fld))
}

func selStmt(root ast.Expr, fld *ast.Ident) ast.Expr {
	switch r := root.(type) {
	case *ast.CompositeLit:
		root = &ast.ParenExpr{X: r}
	}
	return &ast.SelectorExpr{
		X: root, Sel: fld,
	}
}

func callPanic(e ast.Expr) ast.Stmt {
	return exprStmt(
		callFunc("", "panic", e),
	)
}

func assertSizeof(v ast.Expr, sizeof int64) ast.Stmt {
	// if n := unsafe.Sizeof(%v); n != %d { panic(n) }
	return assertStmt(callFunc("unsafe", "Sizeof", v), intLit(sizeof))
}

func structOffsetTests(typ, fld string, offset, sizeof int64) []ast.Stmt {
	// if n := unsafe.Offsetof(S%s{}.%s); n != %d { panic(n) }
	// if n := unsafe.Sizeof(S%s{}.%s); n != %d { panic(n) }
	f := sel(compositeLit(typ), fld)
	return []ast.Stmt{
		assertStmt(callFunc("unsafe", "Offsetof", f), intLit(offset)),
		assertSizeof(f, sizeof),
	}
}

func comment(s string) *ast.CommentGroup {
	if strings.Contains(s, "\n") {
		s = "/* " + s + " */"
	} else {
		s = "// " + s
	}
	return &ast.CommentGroup{
		List: []*ast.Comment{
			{Slash: 1, Text: s},
		},
	}
}

func commentf(s string, args ...interface{}) *ast.CommentGroup {
	return comment(fmt.Sprintf(s, args...))
}

func commentDeclf(decl ast.Decl, s string, args ...interface{}) ast.Decl {
	return commentDecl(decl, fmt.Sprintf(s, args...))
}

func commentDecl(decl ast.Decl, s string) ast.Decl {
	if s == "" {
		return decl
	}
	c := comment(s)
	switch d := decl.(type) {
	case *ast.FuncDecl:
		d.Doc = c
	case *ast.GenDecl:
		d.Doc = c
	default:
		// FIXME
	}
	return decl
}

func commentSpecf(decl ast.Spec, s string, args ...interface{}) ast.Spec {
	return commentSpec(decl, fmt.Sprintf(s, args...))
}

func commentSpec(decl ast.Spec, s string) ast.Spec {
	if s == "" {
		return decl
	}
	c := comment(s)
	switch d := decl.(type) {
	case *ast.ValueSpec:
		d.Doc = c
	default:
		// FIXME
	}
	return decl
}

func commentStmtf(decl ast.Stmt, s string, args ...interface{}) ast.Stmt {
	return commentStmt(decl, fmt.Sprintf(s, args...))
}

func commentStmt(decl ast.Stmt, s string) ast.Stmt {
	if s == "" {
		return decl
	}
	return decl // FIXME
}

func printDecls(w io.Writer, decls []ast.Decl) error {
	fs := token.NewFileSet()
	buf := bytes.NewBuffer(nil)
	if err := format.Node(buf, fs, &ast.File{Name: ident("x"), Decls: decls}); err != nil {
		return err
	}
	pref := []byte("package x")
	data := buf.Bytes()
	if !bytes.Contains(data, pref) {
		return fmt.Errorf("prefix: %q", data[:len(pref)+1])
	}
	data = bytes.TrimPrefix(data, pref)
	_, err := w.Write(data)
	return err
}

func setPtr(x, v ast.Expr) ast.Stmt {
	return assign(
		ptrVal(x), v,
	)
}

func unaddrUnsafe(typ string, x ast.Expr) ast.Expr {
	return ptrVal(
		cast(
			toUnsafePtr(x),
			ptr(ident(typ)),
		),
	)
}

func unaddrUnsafeN(typ string, n int, x ast.Expr) ast.Expr {
	var t ast.Expr = ident(typ)
	for i := 0; i < n; i++ {
		t = ptr(t)
	}
	x = cast(toUnsafePtr(x), t)
	for i := 0; i < n; i++ {
		x = ptrVal(x)
	}
	return x
}

func setPtrUnsafe(typ string, x, v ast.Expr) ast.Stmt {
	// *(*%s)(unsafe.Pointer(%s)) = %s
	return assign(unaddrUnsafe(typ, x), v)
}

func withTLS(decl *ast.FuncDecl) *ast.FuncDecl {
	par := decl.Type.Params
	par.List = append([]*ast.Field{
		{
			Names: []*ast.Ident{ident("tls")},
			Type:  sel(ident(crtP), "TLS"),
		},
	}, par.List...)
	return decl
}

func eval(typ ast.Expr, stmts ...ast.Stmt) ast.Expr {
	return &ast.CallExpr{
		Fun: &ast.FuncLit{
			Type: &ast.FuncType{
				Params: &ast.FieldList{},
				Results: &ast.FieldList{List: []*ast.Field{
					field("", typ, nil),
				}},
			},
			Body: block(stmts...),
		},
	}
}

func runAndReturn(ret, typ ast.Expr, stmts ...ast.Stmt) ast.Expr {
	if len(stmts) == 0 {
		return ret
	}
	stmts = append([]ast.Stmt{}, stmts...)
	stmts = append(stmts, returnVal(ret))
	return eval(typ, stmts...)
}

func returnVal(x ...ast.Expr) ast.Stmt {
	return &ast.ReturnStmt{
		Results: x,
	}
}

func bool2int(x ast.Expr) ast.Expr {
	return callFunc("", "bool2int", x)
}

func trimBits(x ast.Expr, n int) ast.Expr {
	return paren(shr(shl(x, n), n))
}

func label(name string, stmts ...ast.Stmt) []ast.Stmt {
	var s ast.Stmt = &ast.EmptyStmt{} // TODO: attach to the following statement
	if len(stmts) != 0 {
		s = stmts[0]
		stmts = stmts[1:]
	}
	return append([]ast.Stmt{
		&ast.LabeledStmt{
			Label: ident(name),
			Stmt:  s,
		},
	}, stmts...)
}

func labelN(n int, stmts ...ast.Stmt) []ast.Stmt {
	return label(fmt.Sprintf("_%d", n), stmts...)
}

func gotoN(n int) ast.Stmt {
	return gotoStmt(fmt.Sprintf("_%d", n))
}

func gotoStmt(name string) ast.Stmt {
	return &ast.BranchStmt{Tok: token.GOTO, Label: ident(name)}
}

func defers(stmts ...ast.Stmt) ast.Stmt {
	if len(stmts) == 1 {
		if es, ok := stmts[0].(*ast.ExprStmt); ok {
			if c, ok := es.X.(*ast.CallExpr); ok {
				return &ast.DeferStmt{Call: c}
			}
		}
	}
	return &ast.DeferStmt{
		Call: &ast.CallExpr{
			Fun: &ast.FuncLit{
				Type: &ast.FuncType{
					Params: &ast.FieldList{},
				},
				Body: block(stmts...),
			},
		},
	}
}

func rangeLoop(i, v string, arr ast.Expr, stmts ...ast.Stmt) ast.Stmt {
	if i == "" {
		i = "_"
	}
	var ve ast.Expr
	if v != "" {
		ve = ident(v)
	}
	return &ast.RangeStmt{
		Tok: token.DEFINE,
		Key: ident(i), Value: ve, X: arr,
		Body: block(stmts...),
	}
}

type stmtsArr [][]ast.Stmt

func flattenStmts(arrs stmtsArr) []ast.Stmt {
	return appendStmts(nil, arrs)
}

func appendStmts(dst []ast.Stmt, arrs stmtsArr) []ast.Stmt {
	for _, arr := range arrs {
		dst = append(dst, arr...)
	}
	return dst
}

func newTrue() *bool {
	t := true
	return &t
}

func switchStmt(x ast.Expr, cases ...*ast.CaseClause) ast.Stmt {
	var stmts []ast.Stmt
	for _, c := range cases {
		stmts = append(stmts, c)
	}
	return &ast.SwitchStmt{
		Tag: x, Body: block(stmts...),
	}
}

func caseStmt(x ast.Expr, stmts ...ast.Stmt) *ast.CaseClause {
	var list []ast.Expr
	if x != nil {
		list = []ast.Expr{x}
	}
	return &ast.CaseClause{
		List: list, Body: stmts,
	}
}
