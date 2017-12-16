// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bufio"
	"bytes"
	"encoding/hex"
	"flag"
	"fmt"
	"go/token"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
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
	flag.BoolVar(&traceOpt, "to", false, "")
	flag.BoolVar(&traceWrites, "tw", false, "")
}

// ============================================================================

const inject = `
#define _CCGO 1
#define __arch__ %s
#define __os__ %s
#include <builtin.h>
`

var (
	oRE = flag.String("re", "", "")
)

func TestOpt(t *testing.T) {
	for _, v := range []struct{ in, out string }{
		{"var _ = (a(b))", "var _ = a(b)"},
		{"var _ = ((a)(b))", "var _ = a(b)"},
		{"var _ = *((*a)(b))", "var _ = *(*a)(b)"},
	} {
		in := bytes.NewBufferString(v.in)
		var out bytes.Buffer
		if err := newOpt().do(&out, in, "TestOp", 0); err != nil {
			t.Fatal(err)
		}

		if g, e := bytes.TrimSpace(out.Bytes()), []byte(v.out); !bytes.Equal(g, e) {
			t.Fatalf("got\n%s\nexp\n%s", g, e)
		}
	}
}

func testTCC(t *testing.T, pth string) {
	testFn = pth
	fset := token.NewFileSet()
	predefSource := c99.NewStringSource("<predefine>", fmt.Sprintf(inject, runtime.GOARCH, runtime.GOOS))
	crt0Source := c99.NewFileSource(filepath.Join(ccir.LibcIncludePath, "crt0.c"))
	mainSource := c99.NewFileSource(pth)
	inc := []string{"@", ccir.LibcIncludePath}
	sysInc := []string{ccir.LibcIncludePath}
	tweaks := &c99.Tweaks{
		EnableBinaryLiterals:       true,
		EnableEmptyStructs:         true,
		EnableImplicitDeclarations: true,
		EnableReturnExprInVoidFunc: true,
	}
	crt0, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, crt0Source)
	if err != nil {
		t.Fatal(errString(err))
	}

	main, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, mainSource)
	if err != nil {
		t.Fatal(errString(err))
	}

	dir, err := ioutil.TempDir("", "test-ccgo-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()
	t.Log(pth)
	build(t, dir, []*c99.TranslationUnit{crt0, main})
	out := run(t, dir)
	fn := pth[:len(pth)-len(filepath.Ext(pth))] + ".expect"
	s, err := ioutil.ReadFile(fn)
	if err != nil {
		if os.IsNotExist(err) {
			return
		}
	}

	out = trim(out)
	s = trim(s)
	if !bytes.Equal(out, s) {
		t.Logf("\ngot\n%s\nexp\n%s", hex.Dump(out), hex.Dump(s))
		t.Fatalf("\ngot\n%s\nexp\n%s", out, s)
	}
}

func trim(b []byte) []byte {
	a := bytes.Split(b, []byte{'\n'})
	for i, v := range a {
		a[i] = bytes.TrimRight(v, " ")
	}
	return bytes.Join(a, []byte{'\n'})
}

func build(t *testing.T, dir string, in []*c99.TranslationUnit) {
	f, err := os.Create(filepath.Join(dir, "main.go"))
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := f.Close(); err != nil {
			t.Fatal(err)
		}
	}()

	w := bufio.NewWriter(f)

	defer func() {
		if err := w.Flush(); err != nil {
			t.Fatal(err)
		}
	}()

	w.WriteString(`package main
	
import (
	"unsafe"

	"github.com/cznic/crt"
)

`)
	if err = Command(w, in); err != nil {
		t.Fatal(err)
	}
}

func run(t *testing.T, dir string) []byte {
	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	var stdout, stderr bytes.Buffer

	cmd := exec.Command("go", "run", "main.go")
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err = cmd.Run()
	if err != nil {
		var log bytes.Buffer
		if b := stdout.Bytes(); len(b) != 0 {
			fmt.Fprintf(&log, "stdout:\n%s\n", b)
		}
		if b := stderr.Bytes(); len(b) != 0 {
			fmt.Fprintf(&log, "stderr:\n%s\n", b)
		}
		t.Fatalf("err %v\n%s", err, log.Bytes())
	}

	return append(stdout.Bytes(), stderr.Bytes()...)
}

func TestTCC(t *testing.T) {
	blacklist := map[string]struct{}{
		"31_args.c":               {},
		"34_array_assignment.c":   {}, // gcc: main.c:16:6: error: incompatible types when assigning to type ‘int[4]’ from type ‘int *’
		"40_stdio.c":              {}, //TODO
		"42_function_pointer.c":   {}, //TODO
		"46_grep.c":               {}, // gcc: 46_grep.c:489:12: error: ‘documentation’ undeclared (first use in this function)
		"47_switch_return.c":      {}, //TODO
		"49_bracket_evaluation.c": {}, //TODO
		"51_static.c":             {}, //TODO
		"54_goto.c":               {}, //TODO
		"55_lshift_type.c":        {}, //TODO
	}
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}
	m, err := filepath.Glob("../c99/testdata/tcc-0.9.26/tests/tests2/*.c")
	if err != nil {
		t.Fatal(err)
	}

	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		testTCC(t, pth)
	}
}

func TestSQLiteShell(t *testing.T) {
	return //TODO-
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
}
