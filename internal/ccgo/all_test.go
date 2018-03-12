// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

//	TCC	cc 51 ccgo 51 build 51 run 51 ok 51
//	Other	cc 12 ccgo 12 build 12 run 12 ok 12
//	GCC	cc 1036 ccgo 996 build 983 run 983 ok 983
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
	isTesting = true
}

// ============================================================================

const (
	inject = `
#define _CCGO 1
#define __os__ %s
#define __arch__ %s
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

func test(t *testing.T, clean bool, cc, ccgo, build, run *int, def, imp string, inc2 []string, dir string, pth []string, args ...string) ([]byte, error) {
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
		EnableImplicitDeclarations:  true,
		EnableOmitFuncDeclSpec:      true,
		EnablePointerCompatibility:  true, // CSmith transparent_crc_bytes
		EnableReturnExprInVoidFunc:  true,
		IgnorePragmas:               true,
	}
	inc := append([]string{"@", ccir.LibcIncludePath}, inc2...)
	sysInc := []string{ccir.LibcIncludePath, "@"}

	predefSource := c99.NewStringSource("<predefine>", fmt.Sprintf(inject, runtime.GOOS, runtime.GOARCH, def))
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
		out, err := test(t, false, &cc, &ccgo, &build, &run, "", "", nil, dir, []string{pth})
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
		out, err := test(t, false, &cc, &ccgo, &build, &run, "", "", strings.Split(*oI, ","), dir, []string{pth})
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

		"921016-1.c":                   {}, //TODO bits, arithmetic precision
		"970217-1.c":                   {}, //TODO VLA
		"bitfld-1.c":                   {}, //TODO bits, arithmetic precision
		"bitfld-3.c":                   {}, //TODO bits, arithmetic precision
		"builtin-types-compatible-p.c": {}, //TODO must track type qualifiers
		"pr32244-1.c":                  {}, //TODO bits, arithmetic precision
		"pr34971.c":                    {}, //TODO bits, arithmetic precision
		"pr77767.c":                    {}, //TODO VLA
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
		out, err := test(t, false, &cc, &ccgo, &build, &run, def, "", nil, dir, []string{pth})
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
	if out, err := test(t, false, &cc, &ccgo, &build, &run,
		`
#define HAVE_FDATASYNC 1
#define HAVE_ISNAN 1
#define HAVE_LOCALTIME_R 1
/* #define HAVE_MALLOC_USABLE_SIZE 1 */
#define HAVE_USLEEP 1
#define SQLITE_DEBUG 1
#define SQLITE_MEMDEBUG 1
		`,
		`
import "math"
`,
		nil,
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

		out, err := exec.Command(
			csmith,
			"-o", mainC,
			"--bitfields",            // --bitfields | --no-bitfields: enable | disable full-bitfields structs (disabled by default).
			"--no-const-pointers",    // --const-pointers | --no-const-pointers: enable | disable const pointers (enabled by default).
			"--no-consts",            // --consts | --no-consts: enable | disable const qualifier (enabled by default).
			"--paranoid",             // --paranoid | --no-paranoid: enable | disable pointer-related assertions (disabled by default).
			"--no-volatile-pointers", // --volatile-pointers | --no-volatile-pointers: enable | disable volatile pointers (enabled by default).
			"--no-volatiles",         // --volatiles | --no-volatiles: enable | disable volatiles (enabled by default).
		).Output()
		if err != nil {
			t.Fatalf("%v\n%s", err, out)
		}

		if out, err := exec.Command(gcc, "-w", "-o", gccBin, mainC).CombinedOutput(); err != nil {
			t.Fatalf("%v\n%s", err, out)
		}

		var gccOut []byte
		var gccT0 time.Time
		var gccT time.Duration
		func() {
			ctx, cancel := context.WithTimeout(context.Background(), testTimeout/3)

			defer cancel()

			gccT0 = time.Now()
			gccOut, err = exec.CommandContext(ctx, filepath.Join(dir, gccBin)).CombinedOutput()
			gccT = time.Since(gccT0)
		}()
		if err != nil {
			continue
		}

		cs++
		build0 := build
		os.Remove("main.go")
		ccgoOut, err := test(t, false, &cc, &ccgo, &build, &run, "", "", []string{inc}, dir, []string{mainC})
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

func TestTCL(t *testing.T) {
	dir, err := ioutil.TempDir("", "test-ccgo-tcl-")
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.RemoveAll(dir); err != nil {
			t.Fatal(err)
		}
	}()

	tweaks := &c99.Tweaks{
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
	}
	fset := token.NewFileSet()
	inc := []string{
		ccir.LibcIncludePath,
	}
	sysInc := []string{ccir.LibcIncludePath}
	crt0, err := translate(fset, tweaks, inc, sysInc, "", filepath.FromSlash(filepath.Join(ccir.LibcIncludePath, "crt0.c")))
	if err != nil {
		t.Fatal(err)
	}

	tus := []*c99.TranslationUnit{crt0}

	root := "../.."
	tweaks = &c99.Tweaks{
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
	}
	inc = []string{
		".",
		filepath.FromSlash(filepath.Join(root, "_tcl8.6.8/unix")), //TODO Windows
		filepath.FromSlash(filepath.Join(root, "_tcl8.6.8/generic")),
		filepath.FromSlash(filepath.Join(root, "_tcl8.6.8/libtommath")),
		ccir.LibcIncludePath,
	}
	sysInc = []string{
		"@",
		ccir.LibcIncludePath,
	}
	for _, v := range []string{
		"_tcl8.6.8/generic/tclFileName.c",
		"_tcl8.6.8/generic/tclTimer.c",
		"_tcl8.6.8/generic/tclScan.c",
		"_tcl8.6.8/generic/tclCompCmdsGR.c",
		"_tcl8.6.8/generic/tclProc.c",
		"_tcl8.6.8/generic/tclCompCmds.c",
		"_tcl8.6.8/generic/tclThread.c",
		"_tcl8.6.8/generic/tclAlloc.c",
		"_tcl8.6.8/generic/tclNotify.c",
		"_tcl8.6.8/generic/tclIO.c",
		"_tcl8.6.8/generic/tclStrToD.c",
		"_tcl8.6.8/generic/tclThreadStorage.c",
		"_tcl8.6.8/generic/tclStringObj.c",
		"_tcl8.6.8/generic/tclOO.c",
		"_tcl8.6.8/generic/tclTomMathInterface.c",
		"_tcl8.6.8/generic/tclPkg.c",
		"_tcl8.6.8/generic/tclParse.c",
		"_tcl8.6.8/generic/tclUtil.c",
		"_tcl8.6.8/generic/tclTrace.c",
		"_tcl8.6.8/generic/tclPkgConfig.c",
		"_tcl8.6.8/generic/tclEnv.c",
		"_tcl8.6.8/generic/tclAssembly.c",
		"_tcl8.6.8/generic/tclDisassemble.c",
		"_tcl8.6.8/generic/tclClock.c",
		"_tcl8.6.8/generic/tclIndexObj.c",
		"_tcl8.6.8/generic/tclCmdMZ.c",
		"_tcl8.6.8/generic/tclCmdIL.c",
		"_tcl8.6.8/generic/tclCmdAH.c",
		"_tcl8.6.8/generic/tclDictObj.c",
		"_tcl8.6.8/generic/tclIOCmd.c",
		"_tcl8.6.8/generic/tclBinary.c",
		"_tcl8.6.8/generic/tclInterp.c",
		"_tcl8.6.8/generic/tclEnsemble.c",
		"_tcl8.6.8/generic/tclAsync.c",
		"_tcl8.6.8/generic/tclCompCmdsSZ.c",
		"_tcl8.6.8/generic/tclExecute.c",
		"_tcl8.6.8/generic/tclNamesp.c",
		"_tcl8.6.8/unix/tclUnixThrd.c", //TODO Windows: win/tclWinThrd.c
		"_tcl8.6.8/generic/tclLiteral.c",
		"_tcl8.6.8/generic/tclListObj.c",
		"_tcl8.6.8/generic/tclCompile.c",
		"_tcl8.6.8/generic/tclOptimize.c",
		"_tcl8.6.8/generic/tclPreserve.c",
		"_tcl8.6.8/generic/tclObj.c",
		"_tcl8.6.8/generic/tclCkalloc.c",
		"_tcl8.6.8/generic/tclHash.c",
		"_tcl8.6.8/generic/tclPanic.c",
		"_tcl8.6.8/unix/tclUnixFCmd.c",
		"_tcl8.6.8/generic/tclIOUtil.c",
		"_tcl8.6.8/unix/tclUnixFile.c", //TODO Windows: win/tclWinFile.c
		"_tcl8.6.8/unix/tclUnixInit.c", //TODO Windows: win/tclWinInit.c
		"_tcl8.6.8/generic/tclEvent.c",
		"_tcl8.6.8/generic/tclResult.c",
		"_tcl8.6.8/generic/tclVar.c",
		"_tcl8.6.8/generic/tclBasic.c",
		"_tcl8.6.8/generic/tclEncoding.c",
	} {
		tu, err := translate(fset, tweaks, inc, sysInc, `
#define	CFG_INSTALL_BINDIR "\"/usr/local/bin\""
#define	CFG_INSTALL_DOCDIR "\"/usr/local/man\""
#define	CFG_INSTALL_INCDIR "\"usr/local/include\""
#define	CFG_INSTALL_LIBDIR "\"/usr/local/lib64\"" //TODO hardcoded for linux_amd64
#define	CFG_INSTALL_SCRDIR "\"/usr/local/bin/lib/tcl8.6.8\""
#define	CFG_RUNTIME_BINDIR "\"/usr/local/bin\""
#define	CFG_RUNTIME_DOCDIR "\"/usr/local/man\""
#define	CFG_RUNTIME_INCDIR "\"usr/local/include\""
#define	CFG_RUNTIME_LIBDIR "\"/usr/local/lib64\"" //TODO hardcoded for linux_amd64
#define	CFG_RUNTIME_SCRDIR "\"/usr/local/bin/lib/tcl8.6.8\""
#define HAVE_STRUCT_DIRENT64 1 //TODO 386
#define HAVE_UNISTD_H 1
#define TCL_CFGVAL_ENCODING "iso8859-1"
#define TCL_LIBRARY "\"/usr/local/lib/tcl8.6\""
#define TCL_PACKAGE_PATH "\"/usr/local/lib64 /usr/local/lib \""
	`,
			filepath.FromSlash(filepath.Join(root, v)))
		if err != nil {
			t.Fatal(err)
		}

		tus = append(tus, tu)
	}
	tweaks = &c99.Tweaks{
		EnableEmptyStructs: true,
	}
	inc = []string{
		filepath.FromSlash(filepath.Join(root, "_sqlite/sqlite-amalgamation-3210000")),
		ccir.LibcIncludePath,
	}
	for _, v := range []string{
		"_sqlite/src/tclsqlite.c",
		"_sqlite/sqlite-amalgamation-3210000/sqlite3.c",
	} {
		tu, err := translate(fset, tweaks, inc, sysInc, `
#define HAVE_FDATASYNC 1
#define HAVE_ISNAN 1
#define HAVE_LOCALTIME_R 1
/* #define HAVE_MALLOC_USABLE_SIZE 1 */
#define HAVE_USLEEP 1
#define SQLITE_DEBUG 1
#define SQLITE_MEMDEBUG 1
#define TCLSH 1
	`,
			filepath.FromSlash(filepath.Join(root, v)))
		if err != nil {
			t.Fatal(err)
		}

		tus = append(tus, tu)
	}
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
	if err := Command(w, tus); err != nil {
		t.Fatal(err)
	}
	if err := w.Flush(); err != nil {
		t.Fatal(err)
	}

	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	if out, err := exec.Command("go", "build", "-o", filepath.Join(dir, "main"), f.Name()).CombinedOutput(); err != nil {
		t.Fatalf("%v\n%s", err, out)
	}
}

func translate(fset *token.FileSet, tweaks *c99.Tweaks, includePaths, sysIncludePaths []string, def string, sources ...string) (*c99.TranslationUnit, error) {
	if traceTODO {
		fmt.Println(sources)
	}
	in := []c99.Source{c99.NewStringSource("<predefine>", fmt.Sprintf(inject, runtime.GOOS, runtime.GOARCH, def))}
	for _, v := range sources {
		in = append(in, c99.NewFileSource(v))
	}
	return c99.Translate(fset, tweaks, includePaths, sysIncludePaths, in...)
}
