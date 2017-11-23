// Copyright 2017 The SQLite2Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command sqlite2go ports SQLite to Go. (Work In Progress)
//
// Installation
//
// To install or update sqlite2go
//
//     $ go get [-u] github.com/cznic/sqlite2go
//
// Online documentation
//
// GoDoc: https://godoc.org/github.com/cznic/sqlite2go
//
// Usage
//
// To generate a Go SQLite package:
//
//     $ sqlite2go [options]
//
// To generate a standalone Go SQLite shell:
//
//     $ sqlite2go -shell [options]
//
// Options
//
//     -D<name>
//     -D<name>=<value>
//
// Define preprocessor macros. The first form is equivalent of -D<name>=1.
//
//     -h
//
// Print usage to standard error and exit with status 2.
//
//     -o <path>
//
// Write the result to <path>. Defaults to standard output.
//
//     -package <name>
//
// Name the generated package. Ignored when -shell is used.
//
//     -shell
//
// Generate the standalone SQLite shell program in package main.
package main

import (
	"bufio"
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"runtime/debug"
	"strings"

	"github.com/cznic/ccir"
	"github.com/cznic/sqlite2go/internal/c99"
	"github.com/cznic/sqlite2go/internal/ccgo"
	"github.com/cznic/strutil"
)

const (
	usage = `Usage

To generate a Go SQLite package:

    $ %[1]s [options]

To generate a standalone Go SQLite shell:

    $ %[1]s -shell [options]

Options

-D<name>
-D<name>=<value>

    Define preprocessor macros. The first form is equivalent of -D<name>=1.

-h

    Print usage to standard error and exit with status 2.

-o <path>

    Write the result to <path>. Defaults to standard output.

-package <name>

    Name the generated package. Ignored when -shell is used.

-shell

    Generate the standalone SQLite shell program in package main.
`

	inject = `
#define _CCGO 1
#define __arch__ %s
#define __os__ %s
#include <builtin.h>
%s
`
)

func main() {
	defer func() {
		switch e := recover(); x := e.(type) {
		case nil:
			// nop
		case error:
			exit(1, "PANIC: %v\n%s", errString(x), debugStack())
		default:
			exit(1, "PANIC: %v\n%s", e, debugStack())
		}
	}()

	var opts opts
	opts.get(os.Args)
	if opts.o == "" {
		opts.o = os.Stdout.Name()
	}
	if opts.shell {
		opts.pkg = "main"
	}
	f, err := os.Create(opts.o)
	if err != nil {
		exit(1, "%v", err)
	}

	w := bufio.NewWriter(f)

	defer func() {
		if err := w.Flush(); err != nil {
			exit(1, "%v", err)
		}

		if err := f.Close(); err != nil {
			exit(1, "%v", err)
		}
	}()

	fset := token.NewFileSet()
	tweaks := &c99.Tweaks{
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
		InjectFinalNL:               true,
	}
	inc := []string{"@", ccir.LibcIncludePath}
	sysInc := []string{ccir.LibcIncludePath}
	repo := findRepo()
	predefSource := c99.NewStringSource("<predefine>", fmt.Sprintf(inject, runtime.GOARCH, runtime.GOOS, strings.Join(opts.D, "\n")))
	sqliteSource := c99.NewFileSource(filepath.Join(repo, filepath.FromSlash("_sqlite/sqlite-amalgamation-3210000/sqlite3.c")))
	sqlite, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, sqliteSource)
	if err != nil {
		exit(1, "%s", errString(err))
	}

	switch {
	case opts.shell:
		shellSource := c99.NewFileSource(filepath.Join(repo, filepath.FromSlash("_sqlite/sqlite-amalgamation-3210000/shell.c")))
		shell, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, shellSource)
		if err != nil {
			exit(1, "%s", errString(err))
		}

		err = ccgo.Command(w, []*c99.TranslationUnit{shell, sqlite})
	default:
		err = ccgo.Package(w, []*c99.TranslationUnit{sqlite})
	}
	if err != nil {
		exit(1, "%v", err)
	}
	panic("TODO")
}

func findRepo() string {
	ip, err := strutil.ImportPath()
	if err != nil {
		exit(1, "%v", err)
	}

	gp := strutil.Gopath()
	for _, v := range strings.Split(gp, string(os.PathListSeparator)) {
		s := filepath.Join(v, "src", ip)
		fi, err := os.Stat(s)
		if err != nil {
			println(err.Error())
		}
		if err != nil {
			if os.IsNotExist(err) {
				continue
			}

			exit(1, "%v", err)
		}

		if fi.IsDir() {
			return s
		}
	}
	exit(1, "cannot find repository: %s", ip)
	panic("unreachable")
}

type opts struct {
	D     []string
	o     string
	pkg   string
	shell bool
}

func (o *opts) get(args []string) {
	prog := args[0]
	args = args[1:]
	for len(args) != 0 {
		arg := args[0]
		args = args[1:]
		switch {
		case strings.HasPrefix(arg, "-D"):
			arg = arg[2:]
			p := strings.SplitN(arg, "=", 2)
			if len(p) == 1 {
				p = append(p, "1")
			}
			o.D = append(o.D, fmt.Sprintf("#define %s %s", p[0], p[1]))
		case arg == "-h":
			exit(2, usage, prog)
		case arg == "-o":
			o.value(arg, &args, &o.o)
		case arg == "-package":
			o.value(arg, &args, &o.pkg)
		case arg == "-shell":
			o.shell = true
		default:
			exit(2, "invalid argument: %s, use %s -h to show usage", arg, prog)
		}
	}
}

func (o *opts) value(arg string, args *[]string, v *string) {
	a := *args
	if len(a) == 0 {
		exit(1, "missing argument of %s", arg)
	}

	if *v != "" {
		exit(1, "multiple use of %s", arg)
	}

	*v = a[0]
	*args = a[1:]
}

func debugStack() []byte {
	b := debug.Stack()
	b = b[bytes.Index(b, bPanic)+1:]
	b = b[bytes.Index(b, bPanic):]
	b = b[bytes.Index(b, bNL)+1:]
	return b
}

var (
	bNL    = []byte{'\n'}
	bPanic = []byte("panic")
)

func errString(err error) string {
	var b bytes.Buffer
	printError(&b, "", err)
	return b.String()
}

func exit(code int, msg string, arg ...interface{}) {
	msg = strings.TrimSpace(msg)
	if msg != "" {
		fmt.Fprintf(os.Stderr, os.Args[0]+": "+msg+"\n", arg...)
	}
	os.Stderr.Sync()
	os.Exit(code)
}

func printError(w io.Writer, pref string, err error) {
	switch x := err.(type) {
	case scanner.ErrorList:
		for i, v := range x {
			fmt.Fprintf(w, "%s%v\n", pref, v)
			if i == 10 {
				fmt.Fprintln(w, "too many errors")
				break
			}
		}
	default:
		fmt.Fprintf(w, "%s%v\n", pref, err)
	}
}
