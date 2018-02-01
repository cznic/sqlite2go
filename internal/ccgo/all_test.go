// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bufio"
	"bytes"
	"context"
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
	"time"

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
	flag.BoolVar(&traceTODO, "todo", false, "")
	flag.BoolVar(&traceWrites, "tw", false, "")
}

// ============================================================================

const (
	inject = `
#define _CCGO 1
#define __arch__ %s
#define __os__ %s
#include <builtin.h>
`
	injectGCC = `
#define _CCGO 1
#define __arch__ %s
#define __os__ %s
#include <builtin.h>

#define SIGNAL_SUPPRESS // gcc.c-torture/execute/20101011-1.c
#define llabs(x) __builtin_llabs(x)
`
)

var (
	oRE   = flag.String("re", "", "")
	oEdit = flag.Bool("edit", false, "")
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
	mustBuild(t, dir, []*c99.TranslationUnit{crt0, main})
	out := mustRun(t, dir)
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

func build(t *testing.T, dir string, in []*c99.TranslationUnit) error {
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
	return Command(w, in)
}

func mustBuild(t *testing.T, dir string, in []*c99.TranslationUnit) {
	if err := build(t, dir, in); err != nil {
		t.Fatal(err)
	}
}

func run(t *testing.T, dir string) ([]byte, error) {
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

	if out, err := exec.Command("go", "build", "-o", "main").CombinedOutput(); err != nil {
		return nil, fmt.Errorf("%v: %s", err, out)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)

	defer cancel()

	return exec.CommandContext(ctx, "./main").CombinedOutput()
}

func mustRun(t *testing.T, dir string) []byte {
	out, err := run(t, dir)
	if err != nil {
		t.Fatal(err)
	}

	return out
}

func TestTCC(t *testing.T) {
	blacklist := map[string]struct{}{
		"31_args.c":             {}, // Needs fake argv
		"34_array_assignment.c": {}, // gcc: main.c:16:6: error: incompatible types when assigning to type ‘int[4]’ from type ‘int *’
		"46_grep.c":             {}, // gcc: 46_grep.c:489:12: error: ‘documentation’ undeclared (first use in this function)
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

func TestOther(t *testing.T) {
	testDir(t, "../c99/testdata/bug/*.c", nil)
}

func testDir(t *testing.T, glob string, blacklist map[string]struct{}) {
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}
	m, err := filepath.Glob(filepath.FromSlash(glob))
	if err != nil {
		t.Fatal(err)
	}

	var compiles, builds, runs int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		testFile(t, pth, &compiles, &builds, &runs)
	}
	if runs == 0 || runs < builds {
		t.Errorf("compiles: %d, builds: %v, runs: %v", compiles, builds, runs)
		return
	}

	if testing.Verbose() || *oEdit {
		t.Logf("compiles: %d, builds: %v, runs: %v", compiles, builds, runs)
	}

	if *oEdit {
		fmt.Printf("compiles: %d, builds: %v, runs: %v\n", compiles, builds, runs)
	}
}

func TestGCC(t *testing.T) {
	testDir(t, "../c99/testdata/github.com/gcc-mirror/gcc/gcc/testsuite/gcc.c-torture/execute/*.c", map[string]struct{}{
		"20021127-1.c":      {}, // non standard GCC behavior
		"20070824-1.c":      {}, // alloca
		"920711-1.c":        {}, //TODO
		"built-in-setjmp.c": {}, // alloca
		"pr34456.c":         {}, //TODO
		"pr36321.c":         {}, // alloca + depends on alloca addresses difference
		"pr45034.c":         {}, //TODO
		"pr60003.c":         {}, //TODO __builtin_setjmp
	})
	// compiles: 493, builds: 423, runs: 421
}

func testFile(t *testing.T, pth string, compiles, builds, runs *int) {
	testFn = pth
	fset := token.NewFileSet()
	predefSource := c99.NewStringSource("<predefine>", fmt.Sprintf(injectGCC, runtime.GOARCH, runtime.GOOS))
	crt0Source := c99.NewFileSource(filepath.Join(ccir.LibcIncludePath, "crt0.c"))
	mainSource := c99.NewFileSource(pth)
	inc := []string{"@", ccir.LibcIncludePath}
	sysInc := []string{ccir.LibcIncludePath}
	tweaks := &c99.Tweaks{
		EnableEmptyStructs: true,
	}
	crt0, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, crt0Source)
	if err != nil {
		t.Fatal(errString(err))
	}

	main, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, mainSource)
	if err != nil {
		if testing.Verbose() {
			t.Logf("      cc: %s: %s", pth, compact(err.Error(), 10))
		}
		return
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

	*compiles++
	if err := build(t, dir, []*c99.TranslationUnit{crt0, main}); err != nil {
		if testing.Verbose() {
			t.Logf("compiles: %v: %v", pth, compact(err.Error(), 15))
		}
		return
	}

	*builds++
	if out, err := run(t, dir); err != nil {
		t.Errorf("    FAIL: %s: out: %s err: %s", pth, compact(string(out), 1), compact(err.Error(), 2))
		return
	}

	*runs++
	if testing.Verbose() {
		t.Logf("    PASS: %s", pth)
	}
}

func TestSQLiteShell(t *testing.T) {
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
