// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

import (
	"bytes"
	"fmt"
	"go/scanner"
	"io"
	"os"
	"path"
	"runtime"
	"runtime/debug"
	"strings"

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

func rtCaller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "# caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
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
