// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bytes"
	"fmt"
	"go/scanner"
	"io"
	"os"
	"runtime"
	"runtime/debug"
	"strings"

	"github.com/cznic/ir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/xc"
)

const (
	ap         = "ap"
	crt        = "crt."
	vaListType = "pointer to struct{_ struct{}}" // *__builtin_va_list
)

var (
	allocaDeclarator = &c99.Declarator{}
	bNL              = []byte{'\n'}
	bPanic           = []byte("panic")
	dict             = xc.Dict

	idAlloca   = dict.SID("__builtin_alloca")
	idFuncName = dict.SID("__func__")
	idMain     = dict.SID("main")
	idStart    = dict.SID("_start")
	idVaList   = dict.SID("va_list")

	testFn      string
	traceOpt    bool
	traceTODO   bool
	traceWrites bool
)

func compact(s string, maxLines int) string {
	a := strings.Split(s, "\n")
	w := 0
	for _, v := range a {
		v = strings.TrimSpace(v)
		if v != "" {
			a[w] = v
			w++
		}
	}
	a = a[:w]
	if len(a) > maxLines {
		a = a[:maxLines]
	}
	return strings.Join(a, "\n")
}

func debugStack() []byte {
	b := debug.Stack()
	b = b[bytes.Index(b, bPanic)+1:]
	b = b[bytes.Index(b, bPanic):]
	b = b[bytes.Index(b, bNL)+1:]
	return b
}

func errString(err error) string {
	var b bytes.Buffer
	printError(&b, "", err)
	return b.String()
}

func isSingleExpression(n *c99.ExprList) bool { return n.ExprList == nil }

func mangleIdent(nm int, exported bool) string {
	switch {
	case exported:
		return fmt.Sprintf("X%s", dict.S(nm))
	default:
		return fmt.Sprintf("_%s", dict.S(nm))
	}
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

func roundup(n, to int64) int64 {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

func strComment(sv *ir.StringValue) string {
	s := dict.S(int(sv.StringID))
	if len(s) > 32 {
		s = append(append([]byte(nil), s[:32]...), []byte("...")...)
	}
	s = bytes.Replace(s, []byte("*/"), []byte(`*\x2f`), -1)
	return fmt.Sprintf("/* %q */", s)
}

func todo(msg string, args ...interface{}) {
	_, f, l, _ := runtime.Caller(1)
	if msg == "" {
		msg = strings.Repeat("%v ", len(args))
	}
	if traceTODO {
		fmt.Fprintf(os.Stderr, "\n\n%v:%d: TODO\n\n%s", f, l, fmt.Sprintf(msg, args...)) //TODOOK
	}
	panic(fmt.Errorf("\n\n%v:%d: TODO\n\n%s", f, l, fmt.Sprintf(msg, args...))) //TODOOK
}

func packedType(packed, t c99.Type) c99.Type {
	if t.IsUnsigned() {
		return packed
	}
	switch packed.Kind() {
	case c99.UChar:
		return c99.SChar
	case c99.UShort:
		return c99.Short
	case c99.UInt:
		return c99.Int
	case c99.ULong:
		return c99.Long
	case c99.ULongLong:
		return c99.LongLong
	default:
		todo("", packed.Kind())
	}
	panic("unreachable")
}

func isFnPtr(t c99.Type, out *c99.Type) bool {
	switch x := c99.UnderlyingType(t).(type) {
	case *c99.PointerType:
		if x.Item.Kind() == c99.Function {
			if out != nil {
				*out = x.Item
			}
			return true
		}
	}
	return false
}
