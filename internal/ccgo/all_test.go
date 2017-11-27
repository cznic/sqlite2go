// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bytes"
	"fmt"
	"go/token"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/cznic/ccir"
	"github.com/cznic/sqlite2go/internal/c99"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "# caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func dbg(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# dbg %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func TODO(...interface{}) string { //TODOOK
	_, fn, fl, _ := runtime.Caller(1)
	return fmt.Sprintf("# TODO: %s:%d:\n", path.Base(fn), fl) //TODOOK
}

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO) //TODOOK
}

// ============================================================================

const inject = `
#define _CCGO 1
#define __arch__ %s
#define __os__ %s
#include <builtin.h>
`

func Test(t *testing.T) {
	fset := token.NewFileSet()
	tweaks := &c99.Tweaks{
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
		InjectFinalNL:               true,
	}
	inc := []string{"@", ccir.LibcIncludePath}
	sysInc := []string{ccir.LibcIncludePath}
	repo := filepath.FromSlash("../../_sqlite/sqlite-amalgamation-3210000/")
	predefSource := c99.NewStringSource("<predefine>", fmt.Sprintf(inject, runtime.GOARCH, runtime.GOOS))
	sqliteSource := c99.NewFileSource(filepath.Join(repo, "sqlite3.c"))
	sqlite, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, sqliteSource)
	if err != nil {
		t.Fatal(errString(err))
	}

	crt0Source := c99.NewFileSource(filepath.Join(ccir.LibcIncludePath, "crt0.c"))
	crt0, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, crt0Source)
	if err != nil {
		t.Fatal(errString(err))
	}

	shellSource := c99.NewFileSource(filepath.Join(repo, "shell.c"))
	shell, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, shellSource)
	if err != nil {
		t.Fatal(errString(err))
	}

	var buf bytes.Buffer
	if err = Command(&buf, []*c99.TranslationUnit{crt0, shell, sqlite}); err != nil {
		t.Fatal(err)
	}

	t.Logf("\n%s", buf.Bytes())
}
