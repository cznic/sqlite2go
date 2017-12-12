// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"io"
	"os"
	//"github.com/cznic/strutil"
)

var (
	_ io.Writer = (*opt)(nil)
)

type opt struct {
	fset  *token.FileSet
	out   io.Writer
	write bool
}

func newOpt() *opt { return &opt{} }

func (o *opt) Write(b []byte) (int, error) {
	if traceOpt {
		os.Stderr.Write(b)
	}
	if o.write {
		return o.out.Write(b)
	}

	if i := bytes.IndexByte(b, '\n'); i >= 0 {
		o.write = true
		n, err := o.out.Write(b[i+1:])
		return n + i, err
	}

	return len(b), nil
}

func (o *opt) pos(n ast.Node) token.Position {
	if n == nil {
		return token.Position{}
	}

	return o.fset.Position(n.Pos())
}

func (o *opt) do(out io.Writer, in io.Reader, fn string) error {
	o.out = out
	o.fset = token.NewFileSet()
	ast, err := parser.ParseFile(o.fset, "", io.MultiReader(bytes.NewBufferString(fmt.Sprintf("package p // %s\n", fn)), in), parser.ParseComments)
	if err != nil {
		return err
	}

	//dbg("\n%s", strutil.PrettyString(ast, "", "", nil))
	o.file(ast)
	return format.Node(o, o.fset, ast)
}

func (o *opt) file(n *ast.File) {
	for i := range n.Decls {
		o.decl(&n.Decls[i])
	}
}

func (o *opt) decl(n *ast.Decl) {
	switch x := (*n).(type) {
	case *ast.FuncDecl:
		o.blockStmt(x.Body)
	case *ast.GenDecl:
		for i := range x.Specs {
			o.spec(&x.Specs[i])
		}
	default:
		todo("%v: %T", o.pos(x), x)
	}
}

func (o *opt) spec(n *ast.Spec) {
	switch x := (*n).(type) {
	case *ast.TypeSpec:
		// nop
	case *ast.ValueSpec:
		for i := range x.Values {
			o.expr(&x.Values[i])
		}
	default:
		todo("%v: %T", o.pos(x), x)
	}
}

func (o *opt) blockStmt(n *ast.BlockStmt) {
	o.body(n.List)
}

func (o *opt) body(l []ast.Stmt) {
	for i := range l {
		o.stmt(&l[i])
	}
}

func (o *opt) stmt(n *ast.Stmt) {
	switch x := (*n).(type) {
	case nil:
		// nop
	case *ast.AssignStmt:
		for i := range x.Lhs {
			o.expr(&x.Lhs[i])
			switch x2 := x.Lhs[i].(type) {
			case *ast.ParenExpr:
				x.Lhs[i] = x2.X
			}
		}
		for i := range x.Rhs {
			o.expr(&x.Rhs[i])
			switch x2 := x.Rhs[i].(type) {
			case *ast.ParenExpr:
				x.Rhs[i] = x2.X
			}
		}
	case *ast.BlockStmt:
		o.blockStmt(x)
	case *ast.BranchStmt:
		// nop
	case *ast.CaseClause:
		for i := range x.List {
			o.expr(&x.List[i])
		}
		o.body(x.Body)
	case *ast.DeclStmt:
		o.decl(&x.Decl)
	case *ast.DeferStmt:
		o.call(x.Call)
	case *ast.EmptyStmt:
		// nop
	case *ast.ExprStmt:
		o.expr(&x.X)
		switch x2 := x.X.(type) {
		case *ast.ParenExpr:
			x.X = x2.X
		}
	case *ast.IfStmt:
		o.stmt(&x.Init)
		o.expr(&x.Cond)
		o.blockStmt(x.Body)
		o.stmt(&x.Else)
	case *ast.IncDecStmt:
		o.expr(&x.X)
	case *ast.LabeledStmt:
		o.stmt(&x.Stmt)
	case *ast.ReturnStmt:
		for i := range x.Results {
			o.expr(&x.Results[i])
		}
	case *ast.SwitchStmt:
		o.stmt(&x.Init)
		o.expr(&x.Tag)
		o.blockStmt(x.Body)
	default:
		todo("%v: %T", o.pos(x), x)
	}
}

func (o *opt) expr(n *ast.Expr) {
	switch x := (*n).(type) {
	case *ast.BasicLit:
		// nop
	case *ast.BinaryExpr:
		o.expr(&x.X)
		o.expr(&x.Y)
		switch rhs := x.Y.(type) {
		case *ast.BasicLit:
			switch x.Op {
			case token.ADD, token.SUB:
				if rhs.Value == "0" {
					*n = x.X
				}
			case token.EQL:
				if rhs.Value != "0" {
					break
				}

				switch x2 := x.X.(type) {
				case *ast.CallExpr:
					switch x3 := x2.Fun.(type) {
					case *ast.Ident:
						if x3.Name == "bool2int" {
							rel := x2.Args[0].(*ast.BinaryExpr)
							rel.Op = invOP(rel.Op)
							*n = rel
						}
					}
				}
			case token.NEQ:
				if rhs.Value != "0" {
					break
				}

				switch x2 := x.X.(type) {
				case *ast.CallExpr:
					switch x3 := x2.Fun.(type) {
					case *ast.Ident:
						if x3.Name == "bool2int" {
							*n = x2.Args[0].(*ast.BinaryExpr)
						}
					}
				}
			}
		}
	case *ast.CallExpr:
		o.call(x)
	case *ast.FuncLit:
		o.body(x.Body.List)
	case *ast.Ident:
		// nop
	case *ast.IndexExpr:
		o.expr(&x.X)
		o.expr(&x.Index)
	case *ast.ParenExpr:
		o.expr(&x.X)
		switch x2 := x.X.(type) {
		case *ast.CallExpr:
			*n = x2
		case *ast.Ident:
			*n = x2
		case *ast.ParenExpr:
			*n = x2.X
		case *ast.SelectorExpr:
			switch x2.X.(type) {
			case *ast.Ident:
				*n = x2
			}
		case *ast.UnaryExpr:
			switch x2.Op {
			case token.AND:
				switch x2.X.(type) {
				case
					*ast.Ident,
					*ast.SelectorExpr:

					*n = x2
				}
			}
		}
	case *ast.SelectorExpr:
		o.expr(&x.X)
	case *ast.StarExpr:
		o.expr(&x.X)
		switch x2 := x.X.(type) {
		case *ast.UnaryExpr:
			switch x2.Op {
			case token.AND:
				*n = x2.X
			}
		}
	case *ast.StructType:
		// nop
	case *ast.UnaryExpr:
		o.expr(&x.X)
	default:
		todo("%v: %T", o.pos(x), x)
	}
}

func invOP(op token.Token) token.Token {
	switch op {
	case token.LSS:
		return token.GEQ
	case token.LEQ:
		return token.GTR
	default:
		todo("", op)
	}
	panic("unreachable")
}

func (o *opt) call(n *ast.CallExpr) {
	o.expr(&n.Fun)
	for i := range n.Args {
		o.expr(&n.Args[i])
		switch x := n.Args[i].(type) {
		case *ast.ParenExpr:
			n.Args[i] = x.X
		}
	}
}
