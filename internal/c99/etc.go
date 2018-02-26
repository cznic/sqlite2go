// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

import (
	"bytes"
	"fmt"
	"go/scanner"
	"io"
	"runtime/debug"

	"github.com/cznic/strutil"
	"github.com/cznic/xc"
)

var (
	bNL    = []byte{'\n'}
	bPanic = []byte("panic")
)

type panicError struct {
	error
}

func newPanicError(err error) panicError { return panicError{err} }

// PrettyString returns pretty strings for things produced by this package.
func PrettyString(v interface{}) string {
	return strutil.PrettyString(v, "", "", printHooks)
}

func debugStack() []byte {
	b := debug.Stack()
	b = b[bytes.Index(b, bPanic)+1:]
	b = b[bytes.Index(b, bPanic):]
	b = b[bytes.Index(b, bNL)+1:]
	return b
}

func trimSpace(toks []xc.Token) []xc.Token {
	for len(toks) != 0 && toks[0].Rune == ' ' {
		toks = toks[1:]
	}
	for len(toks) != 0 && toks[len(toks)-1].Rune == ' ' {
		toks = toks[:len(toks)-1]
	}
	return toks
}

func trimAllSpace(toks []xc.Token) []xc.Token {
	w := 0
	for _, v := range toks {
		switch v.Rune {
		case ' ', '\n':
			// nop
		default:
			toks[w] = v
			w++
		}
	}
	return toks[:w]
}

func errString(err error) string {
	var b bytes.Buffer
	printError(&b, "", err)
	return b.String()
}

func printError(w io.Writer, pref string, err error) {
	switch x := err.(type) {
	case scanner.ErrorList:
		x.RemoveMultiples()
		for i, v := range x {
			fmt.Fprintf(w, "%s%v\n", pref, v)
			if i == 50 {
				fmt.Fprintln(w, "too many errors")
				break
			}
		}
	default:
		fmt.Fprintf(w, "%s%v\n", pref, err)
	}
}

func (n *ExprList) dumpOperands(s string) {
	for l := n; l != nil; l = l.ExprList {
		l.Expr.dumpOperands(s + "· ")
	}
}

func (n *Expr) dumpOperands(s string) {
	z := ""
	switch {
	case n.IsZero():
		z = ".Z"
	case n.IsNonZero():
		z = ".NZ"
	}
	fmt.Printf("%s%v%v %v\n", s, n.Case, z, n.Operand)
	switch n.Case {
	case
		ExprAddrof,
		ExprCast,
		ExprCpl,
		ExprDeref,
		ExprIndex,
		ExprNot,
		ExprPSelect,
		ExprPostDec,
		ExprPostInc,
		ExprPreDec,
		ExprPreInc,
		ExprSelect,
		ExprSizeofExpr,
		ExprUnaryMinus,
		ExprUnaryPlus:

		n.Expr.dumpOperands(s + "· ")
	case
		ExprAdd,
		ExprAnd,
		ExprAssign,
		ExprDiv,
		ExprEq,
		ExprGe,
		ExprGt,
		ExprLAnd,
		ExprLOr,
		ExprLe,
		ExprLsh,
		ExprLt,
		ExprMod,
		ExprMul,
		ExprNe,
		ExprOr,
		ExprRsh,
		ExprSub,
		ExprXor:

		n.Expr.dumpOperands(s + "· ")
		n.Expr2.dumpOperands(s + "· ")
	case ExprPExprList:
		n.ExprList.dumpOperands(s + "· ")
	case ExprCond:
		n.Expr.dumpOperands(s + "· ")
		n.ExprList.dumpOperands(s + "· ")
		n.Expr2.dumpOperands(s + "· ")
	case
		ExprCall,
		ExprChar,
		ExprFloat,
		ExprIdent,
		ExprInt,
		ExprSizeofType:

		// nop
	default:
		panic(n.Case.String())
	}
}

func isVaList(t Type) bool { //TODO export and use
	x, ok := t.(*NamedType)
	return ok && x.Name == idVaList
}
