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
	bNL    = []byte{'\n'}
	bPanic = []byte("panic")
	dict   = xc.Dict

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
		if bytes.Contains(s, []byte("*/")) {
			todo("")
		}
	}
	switch {
	case sv.Offset == 0:
		return fmt.Sprintf("/* %q */", s)
	default:
		return fmt.Sprintf("/* %q+%d */", s, sv.Offset)
	}
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