// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

//	TCC	cc 51 ccgo 51 build 51 run 51 ok 51
//	Other	cc 8 ccgo 8 build 8 run 8 ok 8
//	GCC	cc 928 ccgo 912 build 895 run 895 ok 895
//	Shell	cc 1 ccgo 1 build 1 run 1 ok 1

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

%s

`

	testTimeout = 10 * time.Second
)

var (
	oBuild  = flag.Bool("build", false, "full build errors")
	oCC     = flag.Bool("cc", false, "full cc errors")
	oCCGO   = flag.Bool("ccgo", false, "full ccgo errors")
	oCSmith = flag.Duration("csmith", time.Minute, "") // Use something like -timeout 25h -csmith 24h for real testing.
	oEdit   = flag.Bool("edit", false, "")
	oI      = flag.String("I", "", "")
	oNoCmp  = flag.Bool("nocmp", false, "")
	oRE     = flag.String("re", "", "")
	re      *regexp.Regexp
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

func trim(b []byte) []byte {
	a := bytes.Split(b, []byte{'\n'})
	for i, v := range a {
		a[i] = bytes.TrimRight(v, " ")
	}
	return bytes.Join(a, []byte{'\n'})
}

func test(t *testing.T, clean bool, cc, ccgo, build, run *int, def, imp, inc2, dir string, pth []string, args ...string) ([]byte, error) {
	testFn = pth[len(pth)-1]
	if clean {
		m, err := filepath.Glob(filepath.Join(dir, "*.*"))
		if err != nil {
			t.Fatal(err)
		}

		for _, v := range m {
			if err := os.Remove(v); err != nil {
				t.Fatal(err)
			}
		}
	}

	fset := token.NewFileSet()
	tweaks := &c99.Tweaks{
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
		EnableImplicitBuiltins:      true,
		EnableOmitFuncDeclSpec:      true,
		EnablePointerCompatibility:  true, // CSmith transparent_crc_bytes
		EnableReturnExprInVoidFunc:  true,
		IgnorePragmas:               true,
	}
	inc := []string{"@", ccir.LibcIncludePath, inc2}
	sysInc := []string{ccir.LibcIncludePath}

	predefSource := c99.NewStringSource("<predefine>", fmt.Sprintf(inject, runtime.GOARCH, runtime.GOOS, def))
	crt0, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, c99.NewFileSource(filepath.Join(ccir.LibcIncludePath, "crt0.c")))
	if err != nil {
		return nil, err
	}

	tus := []*c99.TranslationUnit{crt0}
	for _, v := range pth {
		tu, err := c99.Translate(fset, tweaks, inc, sysInc, predefSource, c99.NewFileSource(v))
		if err != nil {
			if !*oCC {
				err = nil
			}
			return nil, err
		}

		tus = append(tus, tu)
	}

	*cc++
	f, err := os.Create(filepath.Join(dir, "main.go"))
	if err != nil {
		t.Fatal(err)
	}

	w := bufio.NewWriter(f)
	w.WriteString(`package main
	
import (
	"os"
	"unsafe"
	"github.com/cznic/crt"
)
`)
	w.WriteString(imp)
	if err := Command(w, tus); err != nil {
		if !*oCCGO {
			err = nil
		}
		return nil, err
	}

	if err := w.Flush(); err != nil {
		t.Fatal(err)
	}

	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	*ccgo++

	if out, err := exec.Command("go", "build", "-o", filepath.Join(dir, "main"), f.Name()).CombinedOutput(); err != nil {
		if !*oBuild {
			return nil, nil
		}

		return nil, fmt.Errorf("%v: %s", err, out)
	}

	*build++

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

	ctx, cancel := context.WithTimeout(context.Background(), testTimeout)

	defer cancel()

	out, err := exec.CommandContext(ctx, filepath.Join(dir, "main"), args...).CombinedOutput()
	if err == nil {
		*run++
	}
	return out, err
}

func TestTCC(t *testing.T) {
	blacklist := map[string]struct{}{
		"13_integer_literals.c": {}, // 9:12: ExprInt strconv.ParseUint: parsing "0b010101010101": invalid syntax
		"31_args.c":             {},
		"34_array_assignment.c": {}, // gcc: main.c:16:6: error: incompatible types when assigning to type ‘int[4]’ from type ‘int *’
		"46_grep.c":             {}, // incompatible forward declaration type
	}

	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir, err := ioutil.TempDir("", "test-ccgo-tcc-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()

	m, err := filepath.Glob(filepath.FromSlash("../c99/testdata/tcc-0.9.26/tests/tests2/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	var cc, ccgo, build, run, ok int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		run0 := run
		out, err := test(t, false, &cc, &ccgo, &build, &run, "", "", "", dir, []string{pth})
		if err != nil {
			t.Errorf("%v: %v", pth, err)
			continue
		}

		if run == run0 {
			continue
		}

		fn := pth[:len(pth)-len(filepath.Ext(pth))] + ".expect"
		s, err := ioutil.ReadFile(fn)
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		s = trim(s)
		if !bytes.Equal(out, s) {
			t.Errorf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(s), out, s)
			continue
		}

		ok++
	}
	t.Logf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	if *oEdit {
		fmt.Printf("TCC\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestOther(t *testing.T) {
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir, err := ioutil.TempDir("", "test-ccgo-other-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()

	m, err := filepath.Glob(filepath.FromSlash("../c99/testdata/bug/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	var cc, ccgo, build, run, ok int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		run0 := run
		out, err := test(t, false, &cc, &ccgo, &build, &run, "", "", *oI, dir, []string{pth})
		if err != nil {
			t.Errorf("%v: %v", pth, err)
			continue
		}

		if run == run0 {
			continue
		}

		fn := pth[:len(pth)-len(filepath.Ext(pth))] + ".expect"
		s, err := ioutil.ReadFile(fn)
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		s = trim(s)
		if !bytes.Equal(out, s) {
			t.Errorf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(s), out, s)
			continue
		}

		ok++
	}
	t.Logf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	if *oEdit {
		fmt.Printf("Other\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestGCC(t *testing.T) {
	const def = `
#define SIGNAL_SUPPRESS // gcc.c-torture/execute/20101011-1.c
`
	blacklist := map[string]struct{}{
		"20010904-1.c":    {}, // __attribute__((aligned(32)))
		"20010904-2.c":    {}, // __attribute__((aligned(32)))
		"20021127-1.c":    {}, // non standard GCC behavior
		"pr23467.c":       {}, // __attribute__ ((aligned (8)))
		"pushpop_macro.c": {}, // #pragma push_macro("_")

		"bitfld-1.c": {}, //TODO bits, arithmetic precision
		"bitfld-3.c": {}, //TODO bits, arithmetic precision
	}

	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir, err := ioutil.TempDir("", "test-ccgo-gcc-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()

	m, err := filepath.Glob(filepath.FromSlash("../c99/testdata/github.com/gcc-mirror/gcc/gcc/testsuite/gcc.c-torture/execute/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	var cc, ccgo, build, run, ok int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		run0 := run
		out, err := test(t, false, &cc, &ccgo, &build, &run, def, "", "", dir, []string{pth})
		if err != nil {
			t.Errorf("%v: %v", pth, err)
			continue
		}

		if run == run0 {
			continue
		}

		fn := pth[:len(pth)-len(filepath.Ext(pth))] + ".expect"
		s, err := ioutil.ReadFile(fn)
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		s = trim(s)
		if !bytes.Equal(out, s) {
			t.Errorf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(s), out, s)
			continue
		}

		ok++
	}
	t.Logf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	if *oEdit {
		fmt.Printf("GCC\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestSQLiteShell(t *testing.T) {
	dir, err := ioutil.TempDir("", "test-ccgo-shell-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()

	var cc, ccgo, build, run, ok int
	root := filepath.FromSlash("../../_sqlite/sqlite-amalgamation-3210000")
	if out, err := test(t, false, &cc, &ccgo, &build, &run, "",
		`
		import "math"
`,
		"",
		dir,
		[]string{
			filepath.Join(root, "shell.c"),
			filepath.Join(root, "sqlite3.c"),
		},
		"foo", "create table t(i)",
	); err != nil {
		t.Fatalf("%s: %v", out, err)
	}

	if run == 1 {
		ok++
	}
	t.Logf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	if *oEdit {
		fmt.Printf("Shell\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestCSmith(t *testing.T) {
	csmith, err := exec.LookPath("csmith")
	if err != nil {
		t.Logf("%v: skipping test", err)
		return
	}

	gcc, err := exec.LookPath("gcc")
	if err != nil {
		t.Logf("%v: skipping test", err)
		return
	}

	var inc string
	switch runtime.GOOS {
	case "linux":
		inc = "/usr/include"
	default:
		t.Logf("unsupported OS")
		return
	}
	if _, err := os.Stat(filepath.Join(inc, "csmith.h")); err != nil {
		if os.IsNotExist(err) {
			t.Logf("%s not found: skipping test", inc)
			return
		}

		t.Fatal(err)
	}

	dir, err := ioutil.TempDir("", "test-ccgo-csmith-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	const (
		gccBin = "gcc"
		mainC  = "main.c"
	)

	ch := time.After(*oCSmith)
	var cs, cc, ccgo, build, run, ok int
	t0 := time.Now()
out:
	for {
		select {
		case <-ch:
			break out
		default:
		}

		if err := exec.Command(csmith, "-o", mainC,
			"--argc",         // --argc | --no-argc: genereate main function with/without argv and argc being passed (enabled by default).
			"--arrays",       // --arrays | --no-arrays: enable | disable arrays (enabled by default).
			"--no-bitfields", //TODO --bitfields | --no-bitfields: enable | disable full-bitfields structs (disabled by default).
			// --builtin-function-prob <num>: set the probability of choosing a builtin function (default is 20).
			// --builtins | --no-builtins: enable | disable to generate builtin functions (disabled by default).
			// --checksum | --no-checksum: enable | disable checksum calculation (enabled by default).
			"--no-comma-operators",     //TODO --comma-operators | --no-comma-operators: enable | disable comma operators (enabled by default).
			"--no-compound-assignment", //TODO --compound-assignment | --no-compound-assignment: enable | disable compound assignments (enabled by default).
			// --concise: generated programs with minimal comments (disabled by default).
			"--no-const-pointers",   // --const-pointers | --no-const-pointers: enable | disable const pointers (enabled by default).
			"--no-consts",           // --consts | --no-consts: enable | disable const qualifier (enabled by default).
			"--divs",                // --divs | --no-divs: enable | disable divisions (enabled by default).
			"--no-embedded-assigns", //TODO --embedded-assigns | --no-embedded-assigns: enable | disable embedded assignments as sub-expressions (enabled by default).
			// --enable-builtin-kinds k1,k2 | --disable-builtin-kinds k1,k2: enable | disable certain kinds of builtin functions.
			"--no-float", //TODO --float | --no-float: enable | disable float (disabled by default).
			// --help or -h: print this information.
			// --inline-function | --no-inline-function: enable | disable inline attributes on generated functions.
			// --inline-function-prob <num>: set the probability of each function being marked as inline (default is 50).
			"--int8",  // --int8 | --no-int8: enable | disable int8_t (enabled by default).
			"--jumps", // --jumps | --no-jumps: enable | disable jumps (enabled by default).
			// --lang-cpp : generate C++ code (C by default).
			"--longlong", // --longlong| --no-longlong: enable | disable long long (enabled by default).
			// --main | --nomain: enable | disable to generate main function (enabled by default).
			"--no-math64",          //TODO --math64 | --no-math64: enable | disable 64-bit math ops (enabled by default).
			"--max-array-dim", "1", //TODO --max-array-dim <num>: limit array dimensions to <num>. (default 3)
			// --max-array-len-per-dim <num>: limit array length per dimension to <num> (default 10).
			"--max-block-depth", "1", //TODO --max-block-depth <num>: limit depth of nested blocks to <num> (default 5).
			// --max-block-size <size>: limit the number of non-return statements in a block to <size> (default 4).
			"--max-expr-complexity", "2", //TODO --max-expr-complexity <num>: limit expression complexities to <num> (default 10).
			// --max-funcs <num>: limit the number of functions (besides main) to <num>  (default 10).
			"--max-pointer-depth", "1", //TODO --max-pointer-depth <depth>: limit the indirect depth of pointers to <depth> (default 2).
			// --max-struct-fields <num>: limit the number of struct fields to <num> (default 10).
			// --max-union-fields <num>: limit the number of union fields to <num> (default 5).
			"--muls", // --muls | --no-muls: enable | disable multiplications (enabled by default).
			// --output <filename> or -o <filename>: specify the output file name.
			"--no-packed-struct", // --packed-struct | --no-packed-struct: enable | disable packed structs by adding #pragma pack(1) before struct definition (disabled by default).
			// --paranoid | --no-paranoid: enable | disable pointer-related assertions (disabled by default).
			"--pointers",           // --pointers | --no-pointers: enable | disable pointers (enabled by default).
			"--post-decr-operator", // --post-decr-operator | --no-post-decr-operator: enable | disable post -- operator (enabled by default).
			"--post-incr-operator", // --post-incr-operator | --no-post-incr-operator: enable | disable post ++ operator (enabled by default).
			"--pre-decr-operator",  // --pre-decr-operator | --no-pre-decr-operator: enable | disable pre -- operator (enabled by default).
			"--pre-incr-operator",  // --pre-incr-operator | --no-pre-incr-operator: enable | disable pre ++ operator (enabled by default).
			// --quiet: generate programs with less comments (disabled by default).
			"--no-safe-math", // --safe-math | --no-safe-math: Emit safe math wrapper functions (enabled by default).
			// --seed <seed> or -s <seed>: use <seed> instead of a random seed generated by Csmith.
			"--structs",             // --structs | --no-structs: enable | disable to generate structs (enable by default).
			"--uint8",               // --uint8 | --no-uint8: enable | disable uint8_t (enabled by default).
			"--unary-plus-operator", // --unary-plus-operator | --no-unary-plus-operator: enable | disable + operator (enabled by default).
			"--unions",              // --unions | --no-unions: enable | disable to generate unions (enable by default).
			// --version or -v: print the version of Csmith.
			"--no-volatile-pointers", // --volatile-pointers | --no-volatile-pointers: enable | disable volatile pointers (enabled by default).
			"--no-volatiles",         // --volatiles | --no-volatiles: enable | disable volatiles (enabled by default).
			// -hh: describe extra options probably useful only for Csmith developers.
		).Run(); err != nil {
			t.Fatal(err)
		}

		if err := exec.Command(gcc, "-o", gccBin, mainC).Run(); err != nil {
			t.Fatal(err)
		}

		var gccOut []byte
		var gccT0 time.Time
		var gccT time.Duration
		func() {
			ctx, cancel := context.WithTimeout(context.Background(), testTimeout/3)

			defer cancel()

			gccT0 = time.Now()
			gccOut, err = exec.CommandContext(ctx, filepath.Join(dir, gccBin), "1").CombinedOutput()
			gccT = time.Since(gccT0)
		}()
		if err != nil {
			continue
		}

		cs++
		build0 := build
		os.Remove("main.go")
		ccgoOut, err := test(t, false, &cc, &ccgo, &build, &run, "", "", inc, dir, []string{mainC}, "1")
		if err != nil {
			t.Log(err)
			csmithFatal(t, mainC, gccOut, ccgoOut, cc, ccgo, build, run, ok, cs, gccT)
		}

		if build == build0 {
			continue
		}

		if bytes.Equal(gccOut, ccgoOut) {
			ok++
			if *oEdit {
				fmt.Printf("cc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)\n", cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, time.Since(t0))
			}
			continue
		}

		if *oNoCmp {
			continue
		}

		csmithFatal(t, mainC, gccOut, ccgoOut, cc, ccgo, build, run, ok, cs, gccT)
	}
	d := time.Since(t0)
	t.Logf("cc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)", cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, d)
	if *oEdit {
		fmt.Printf("CSmith\tcc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)\n", cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, d)
	}
}

func csmithFatal(t *testing.T, mainC string, gccOut, ccgoOut []byte, cc, ccgo, build, run, ok, cs int, gccT time.Duration) {
	b, err := ioutil.ReadFile(mainC)
	if err != nil {
		t.Fatal(err)
	}

	b2, err := ioutil.ReadFile("main.go")
	if err != nil {
		b2 = nil
	}

	t.Fatalf(`
==== CSmith code ==============================================================
%s
==== Go code (if any ) ========================================================
%s
===============================================================================
 GCC   time: %v
 GCC output: %s
CCGO output: %s
cc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)
`,
		b, b2, gccT, bytes.TrimSpace(gccOut), bytes.TrimSpace(ccgoOut),
		cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, *oCSmith)
}
