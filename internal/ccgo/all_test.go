// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

// linux_386
//
//	TCC	cc 51 ccgo 51 build 51 run 51 ok 51
//	Other	cc 16 ccgo 16 build 16 run 16 ok 16
//	GCC	cc 1105 ccgo 1065 build 1058 run 1058 ok 1058
//	Shell	cc 1 ccgo 1 build 1 run 1 ok 1
//	TCL	tclsqlite build ok

// linux_amd64
//
//	TCC	cc 51 ccgo 51 build 51 run 51 ok 51
//	Other	cc 19 ccgo 19 build 19 run 19 ok 19
//	GCC	cc 1096 ccgo 1094 build 1094 run 1094 ok 1094
//	Shell	cc 1 ccgo 1 build 1 run 1 ok 1
//	--- FAIL: TestTCL (188.97s)
//		all_test.go:1334: Tests running in interp:  /home/jnml/src/tmp/tcl/tcl
//			Tests located in:  /home/jnml/src/tmp/tcl
//			Tests running in:  /home/jnml/src/tmp/tcl
//			Temporary files stored in /home/jnml/src/tmp/tcl
//			Test files run in separate interpreters
//			Running tests that match:  *
//			Skipping test files that match:  l.*.test
//			Only running test files that match:  *.test
//			Tests began at Thu May 23 13:49:13 -463544 3918
//			aaa_exit.test
//			Test file error: panic: TODO
//
//			goroutine 1 [running]:
//			github.com/cznic/crt.Xpthread_attr_init(0x7f6efe1001a0, 0x7f6efdffc4a0, 0x7f6efdffc4a0)
//				/home/jnml/src/github.com/cznic/crt/pthread_linux.go:744 +0x39
//			main.XTclpThreadCreate(0x7f6efe1001a0, 0xad3d10, 0x890010, 0x0, 0x100000000, 0xc400000000)
//				/home/jnml/src/tmp/tcl/main.go:131314 +0x80
//			main._218StartNotifierThread(0x7f6efe1001a0, 0x87cfe9)
//				/home/jnml/src/tmp/tcl/main.go:120193 +0x9b
//			main.XTcl_WaitForEvent(0x7f6efe1001a0, 0x7f6efe7f2860, 0x7f6e00000000)
//				/home/jnml/src/tmp/tcl/main.go:29642 +0xeb
//			main.XTcl_DoOneEvent(0x7f6efe1001a0, 0x7f6efffffffd, 0x7f6efe1b72c0)
//				/home/jnml/src/tmp/tcl/main.go:33243 +0x1f9
//			main.XTcl_VwaitObjCmd(0x7f6efe1001a0, 0x0, 0x7f6efcf00020, 0x7f6e00000002, 0x7f6efcd00d80, 0x7f6e00000000)
//				/home/jnml/src/tmp/tcl/main.go:97044 +0x157
//			main._330Dispatch(0x7f6efe1001a0, 0x7f6efcef8c88, 0x7f6efcf00020, 0x7f6e00000000, 0x7f6e00000000)
//				/home/jnml/src/tmp/tcl/main.go:136443 +0x6d
//			main.XTclNRRunCallbacks(0x7f6efe1001a0, 0x7f6efcf00020, 0x0, 0x0, 0x7f6e00200010)
//				/home/jnml/src/tmp/tcl/main.go:116458 +0x78
//			main.XTcl_EvalObjv(0x7f6efe1001a0, 0x7f6efcf00020, 0x7f6e00000005, 0x7f6efcd00300, 0x7f6e00200010, 0xc400000021)
//				/home/jnml/src/tmp/tcl/main.go:23948 +0x9e
//			main.XTclEvalEx(0x7f6efe1001a0, 0x7f6efcf00020, 0x7f6efd11a020, 0x6a2, 0x1, 0x0, 0x7f6efd11a020, 0x0)
//				/home/jnml/src/tmp/tcl/main.go:20567 +0x106c
//			main.XTcl_FSEvalFileEx(0x7f6efe1001a0, 0x7f6efcf00020, 0x7f6efce14bf0, 0x0, 0x7f6e00000000)
//				/home/jnml/src/tmp/tcl/main.go:2575 +0x3cf
//			main.XTcl_MainEx(0x7f6efe1001a0, 0x7f6effffffff, 0x7f6efe500030, 0x88d8c8, 0x7f6efcf00020)
//				/home/jnml/src/tmp/tcl/main.go:209 +0xf13
//			main.Xmain(0x7f6efe1001a0, 0x2, 0x7f6efe500020, 0x7f6efe1001a0)
//				/home/jnml/src/tmp/tcl/main.go:58 +0x61
//			main.X_start(0x7f6efe1001a0, 0x2, 0x7f6efe500020)
//				/home/jnml/src/tmp/tcl/main.go:41 +0x79
//			main.main()
//				/home/jnml/src/tmp/tcl/main.go:35 +0x17f
//			append.test
//
//
//			==== append-1.1 append command FAILED
//			==== Contents of test case:
//
//			    unset -nocomplain x
//
//			...
//
//			Tests ended at Thu May 23 13:51:12 -463544 3918
//			all.tcl:	Total	12635	Passed	928	Skipped	3487	Failed	8220
//			Sourced 148 Test Files.
//			Files with failing tests: append.test apply.test assemble.test autoMkindex.test basic.test case.test compExpr-old.test compExpr.test compile.test concat.test config.test coroutine.test env.test error.test eval.test execute.test expr-old.test expr.test for-old.test for.test foreach.test get.test history.test if-old.test if.test incr-old.test incr.test init.test ioTrans.test join.test lindex.test linsert.test list.test listObj.test llength.test lmap.test load.test lrange.test lrepeat.test lsearch.test lsetComp.test misc.test namespace.test nre.test obj.test ooNext2.test opt.test package.test parse.test parseExpr.test pid.test proc-old.test proc.test pwd.test reg.test regexp.test regexpComp.test rename.test result.test security.test set-old.test source.test split.test stack.test stringComp.test subst.test switch.test tailcall.test tm.test unknown.test unload.test uplevel.test upvar.test utf.test util.test while-old.test while.test
//			Number of tests skipped for each constraint:
//				9	!ieeeFloatingPoint
//				49	dde
//				30	emptyTest
//				3	fullutf
//				2	ieeeFloatingPoint&&testexprdouble
//				20	knownBug
//				17	longIs32bit
//				14	macosxFileAttr
//				54	memory
//				2	nonPortable
//				9	nt
//				1	pcOnly
//				12	pkgaRequired
//				20	pkguaRequired
//				8	procbodytest
//				12	testasync
//				24	testbytestring
//				54	testchannel
//				9	testcmdinfo
//				6	testcmdtoken
//				1	testconcatobj
//				2	testcreatecommand
//				6	testdcall
//				8	testdel
//				3	testdelassocdata
//				126	testdoubledigits
//				1	testdoubleobj
//				40	testdstring
//				217	testevalex
//				11	testevalobjv
//				25	testevent
//				7	testexprdouble
//				5	testexprdoubleobj
//				4	testexprdoubleobj&&ieeeFloatingPoint
//				16	testexprlong
//				17	testexprlongobj
//				215	testexprparser
//				1	testexprparser && !ieeeFloatingPoint
//				1	testexprparser && ieeeFloatingPoint
//				3	testexprstring
//				7	testfindexecutable
//				1	testfindfirst
//				1	testfindlast
//				1	testfork
//				4	testgetassocdata
//				15	testgetint
//				299	testhashsystemhash
//				31	testindexobj
//				10	testinterpresolver
//				28	testlink
//				21	testmathfunctions
//				33	testnrelevels
//				10	testnumutfchars
//				232	testobj
//				7	testparseargs
//				117	testparser
//				5	testparsevar
//				12	testparsevarname
//				1098	testregexp
//				2	testreturn
//				11	testsaveresult
//				4	testsetassocdata
//				4	testseterrorcode
//				5	testsetobjerrorcode
//				5	teststaticpkg
//				5	teststaticpkg_8.x
//				4	teststringobj
//				7	testupvar
//				1	testwinclock
//				54	thread
//				2	unthreaded
//				318	win
//				4	winVista
//				65	zlib
//
//			Test files exiting with errors:
//
//			  aaa_exit.test
//
//			  appendComp.test
//
//			  binary.test
//
//			  chan.test
//
//			  chanio.test
//
//			  clock.test
//
//			  cmdAH.test
//
//			  cmdIL.test
//
//			  cmdMZ.test
//
//			  dict.test
//
//			  encoding.test
//
//			  event.test
//
//			  exec.test
//
//			  fCmd.test
//
//			  fileName.test
//
//			  fileSystem.test
//
//			  format.test
//
//			  http.test
//
//			  http11.test
//
//			  httpold.test
//
//			  info.test
//
//			  interp.test
//
//			  io.test
//
//			  ioCmd.test
//
//			  lreplace.test
//
//			  main.test
//
//			  mathop.test
//
//			  msgcat.test
//
//			  namespace-old.test
//
//			  oo.test
//
//			  parseOld.test
//
//			  pkgMkIndex.test
//
//			  platform.test
//
//			  safe.test
//
//			  scan.test
//
//			  set.test
//
//			  socket.test
//
//			  string.test
//
//			  tcltest.test
//
//			  timer.test
//
//			  trace.test
//
//			  unixFCmd.test
//
//			  unixInit.test
//
//			  var.test
//
//			  winPipe.test
//
//			~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//			tmp test began at Thu May 23 13:51:12 -463544 3918
//		all_test.go:1336: Failed: exit status 1
//			all.tcl:	Total	12635	Passed	928	Skipped	3487	Failed	8220
//			couldn't read file "/home/jnml/src/tmp/tcl/tmp/all.tcl": permission denied
//			    while executing
//			"::source /home/jnml/src/tmp/tcl/tmp/all.tcl"
//			    ("uplevel" body line 1)
//			    invoked from within
//			"uplevel 1 [list ::source [file join $directory all.tcl]]"
//			    (procedure "runAllTests" line 129)
//			    invoked from within
//			"runAllTests"
//			    (file "all.tcl" line 21)
//	cc 1 ccgo 1 build 1 run 1 ok 1 (100.00%) csmith 1 (387.628162ms)
//	cc 2 ccgo 2 build 2 run 2 ok 2 (100.00%) csmith 2 (1.131841229s)
//	cc 3 ccgo 3 build 3 run 3 ok 3 (100.00%) csmith 3 (1.463566375s)
//	cc 4 ccgo 4 build 4 run 4 ok 4 (100.00%) csmith 4 (8.393599152s)
//	cc 5 ccgo 5 build 5 run 5 ok 5 (100.00%) csmith 5 (8.991066109s)
//	cc 6 ccgo 6 build 6 run 6 ok 6 (100.00%) csmith 6 (9.731711691s)
//	cc 7 ccgo 7 build 7 run 7 ok 7 (100.00%) csmith 7 (16.606096138s)
//	cc 8 ccgo 8 build 8 run 8 ok 8 (100.00%) csmith 8 (16.949035567s)
//	cc 9 ccgo 9 build 9 run 9 ok 9 (100.00%) csmith 9 (17.578978177s)
//	cc 10 ccgo 10 build 10 run 10 ok 10 (100.00%) csmith 10 (18.048452155s)
//	cc 11 ccgo 11 build 11 run 11 ok 11 (100.00%) csmith 11 (18.652161015s)
//	cc 12 ccgo 12 build 12 run 12 ok 12 (100.00%) csmith 12 (19.119004339s)
//	cc 13 ccgo 13 build 13 run 13 ok 13 (100.00%) csmith 13 (20.236893378s)
//	cc 14 ccgo 14 build 14 run 14 ok 14 (100.00%) csmith 14 (21.23137532s)
//	cc 15 ccgo 15 build 15 run 15 ok 15 (100.00%) csmith 15 (22.323200031s)
//	cc 16 ccgo 16 build 16 run 16 ok 16 (100.00%) csmith 16 (23.133609954s)
//	cc 17 ccgo 17 build 17 run 17 ok 17 (100.00%) csmith 17 (24.149925574s)
//	cc 18 ccgo 18 build 18 run 18 ok 18 (100.00%) csmith 18 (25.082482481s)
//	cc 19 ccgo 19 build 19 run 19 ok 19 (100.00%) csmith 19 (32.419101506s)
//	cc 20 ccgo 20 build 20 run 20 ok 20 (100.00%) csmith 20 (39.174316192s)
//	cc 21 ccgo 21 build 21 run 21 ok 21 (100.00%) csmith 21 (39.923787351s)
//	cc 22 ccgo 22 build 22 run 22 ok 22 (100.00%) csmith 22 (47.697836037s)
//	cc 23 ccgo 23 build 23 run 23 ok 23 (100.00%) csmith 23 (48.629624992s)
//	cc 24 ccgo 24 build 24 run 24 ok 24 (100.00%) csmith 24 (49.151410885s)
//	cc 25 ccgo 25 build 25 run 25 ok 25 (100.00%) csmith 25 (49.556676275s)
//	cc 26 ccgo 26 build 26 run 26 ok 26 (100.00%) csmith 26 (49.871004057s)
//	cc 27 ccgo 27 build 27 run 27 ok 27 (100.00%) csmith 27 (50.847288459s)
//	cc 28 ccgo 28 build 28 run 28 ok 28 (100.00%) csmith 28 (58.355333234s)
//	cc 29 ccgo 29 build 29 run 29 ok 29 (100.00%) csmith 29 (59.740545879s)
//	cc 30 ccgo 30 build 30 run 30 ok 30 (100.00%) csmith 30 (1m0.334634597s)
//	CSmith	cc 30 ccgo 30 build 30 run 30 ok 30 (100.00%) csmith 30 (1m0.334648139s)
//	FAIL
//	exit status 1
//	FAIL	github.com/cznic/sqlite2go/internal/ccgo	427.016s

import (
	"bufio"
	"bytes"
	"context"
	"encoding/hex"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"strconv"
	"strings"
	"testing"
	"time"

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
	testTimeout = 60 * time.Second
)

var (
	oBuild      = flag.Bool("build", false, "full build errors")
	oCC         = flag.Bool("cc", false, "full cc errors")
	oCCGO       = flag.Bool("ccgo", false, "full ccgo errors")
	oCSmith     = flag.Duration("csmith", time.Minute, "") // Use something like -timeout 25h -csmith 24h for real testing.
	oEdit       = flag.Bool("edit", false, "")
	oI          = flag.String("I", "", "")
	oNoCmp      = flag.Bool("nocmp", false, "")
	oRE         = flag.String("re", "", "")
	oTCLRace    = flag.Bool("tclrace", false, "")
	oTmp        = flag.String("tmp", "", "")
	oTrace      = flag.Bool("trc", false, "")
	re          *regexp.Regexp
	searchPaths []string

	tclPatches = []string{
		// ----
		`
var _ unsafe.Pointer
`,
		`
var (
	cmdMu  sync.Mutex
	cmdMap = map[int]*exec.Cmd{}
)
`,
		// ----
		`
func XTclpCreateProcess(tls crt.TLS, _interp uintptr /* *TTcl_Interp = struct{XresultDon...ntptr);XerrorLineDontUse int32;} */, _argc int32, _argv uintptr /* **int8 */, _inputFile uintptr /* TTclFile = *STclFile_ */, _outputFile uintptr /* TTclFile = *STclFile_ */, _errorFile uintptr /* TTclFile = *STclFile_ */, _pidPtr uintptr /* **STcl_Pid_ */) (r int32) {
	esc := crt.MustMalloc(280)
`,
		`
func XTclpCreateProcess(tls crt.TLS, _interp uintptr /* *TTcl_Interp = struct{XresultDon...ntptr);XerrorLineDontUse int32;} */, _argc int32, _argv uintptr /* **int8 */, _inputFile uintptr /* TTclFile = *STclFile_ */, _outputFile uintptr /* TTclFile = *STclFile_ */, _errorFile uintptr /* TTclFile = *STclFile_ */, _pidPtr uintptr /* **STcl_Pid_ */) (r int32) {
	var argv []string
	var cmd *exec.Cmd
	esc := crt.MustMalloc(280)
`,
		// ----
		`
	_pid = crt.Xvfork(tls)
	if _pid != int32(0) {
		goto _8
	}

	_joinThisError = bool2int((_errorFile != 0) && (_errorFile == _outputFile))
	_fd = int32(*(*uintptr)(unsafe.Pointer(_errPipeOut))) - int32(1)
	if _1074SetupStdFile(tls, _inputFile, int32(2)) != 0 && _1074SetupStdFile(tls, _outputFile, int32(4)) != 0 && (_joinThisError != 0 || _1074SetupStdFile(tls, _errorFile, int32(8)) != 0) && (_joinThisError == 0 || crt.Xdup2(tls, int32(1), int32(2)) != int32(-1) && crt.Xfcntl(tls, int32(2), int32(2), int32(0)) == int32(0)) {
		goto _9
	}

	crt.Xsprintf(tls, _errSpace, ts+58557 /* "%dforked process couldn't set up..." */, *(*int32)(unsafe.Pointer(crt.X__errno_location(tls))))
	_len = crt.Xstrlen(tls, _errSpace)
	if _len == uint64(crt.Xwrite(tls, _fd, _errSpace, _len)) {
		goto _10
	}

	XTcl_Panic(tls, ts+58603 /* "TclpCreateProcess: unable to wri..." */)
_10:
	crt.X_exit(tls, int32(1))
_9:
	_1075RestoreSignals(tls)
	crt.Xexecvp(tls, *(*uintptr)(unsafe.Pointer(_newArgv)), _newArgv)
	crt.Xsprintf(tls, _errSpace, ts+58652 /* "%dcouldn't execute \"%.150s\"" */, *(*int32)(unsafe.Pointer(crt.X__errno_location(tls))), *(*uintptr)(unsafe.Pointer(_argv)))
	_len = crt.Xstrlen(tls, _errSpace)
	if _len == uint64(crt.Xwrite(tls, _fd, _errSpace, _len)) {
		goto _11
	}

	XTcl_Panic(tls, ts+58603 /* "TclpCreateProcess: unable to wri..." */)
_11:
	crt.X_exit(tls, int32(1))
_8:
`,
		`
	_ = _len
	_ = _joinThisError
	_pid = 0
	for p := _newArgv; ; p += unsafe.Sizeof(uintptr(0)) {
		q := *(*uintptr)(unsafe.Pointer(p))
		if q == 0 {
			break
		}

		argv = append(argv, crt.GoString(q))
	}
	cmd = exec.Command(argv[0], argv[1:]...)
	if fd := _inputFile; fd != 0 {
		fd--
		f := os.NewFile(fd, "inputFile")
		if f == nil {
			panic(fd)
		}

		cmd.Stdin = f
	}
	if fd := _outputFile; fd != 0 {
		fd--
		f := os.NewFile(fd, "outputFile")
		if f == nil {
			panic(fd)
		}

		cmd.Stdout = f
	}
	if fd := _errorFile; fd != 0 {
		fd--
		f := os.NewFile(fd, "errorFile")
		if f == nil {
			panic(fd)
		}

		cmd.Stderr = f
	}
	if err := cmd.Start(); err != nil {
		panic(err.Error)
	}

	_pid = int32(cmd.Process.Pid)
	cmdMu.Lock()
	cmdMap[int(_pid)] = cmd
	cmdMu.Unlock()
`,
		// ----
		`
func XTcl_WaitPid(tls crt.TLS, _pid uintptr /* TTcl_Pid = *STcl_Pid_ */, _statPtr uintptr /* *int32 */, _options int32) (r uintptr /* TTcl_Pid = *STcl_Pid_ */) {
	var (
		_result int32
		_real_pid int32
	)
	_real_pid = int32(_pid)
`,
		`
func XTcl_WaitPid(tls crt.TLS, _pid uintptr /* TTcl_Pid = *STcl_Pid_ */, _statPtr uintptr /* *int32 */, _options int32) (r uintptr /* TTcl_Pid = *STcl_Pid_ */) {
	var (
		_result   int32
		_real_pid int32
	)
	_real_pid = int32(_pid)
	cmdMu.Lock()
	cmd := cmdMap[int(_real_pid)]
	cmdMu.Unlock()
	if cmd == nil {
		panic(_real_pid)
	}

	cmd.Wait()
	cmdMu.Lock()
	delete(cmdMap, int(_real_pid))
	cmdMu.Unlock()
	switch st := cmd.ProcessState; {
	case st.Success():
		if p := _statPtr; p != 0 {
			*(*int32)(unsafe.Pointer(p)) = 0
		}
	default:
		if p := _statPtr; p != 0 {
			*(*int32)(unsafe.Pointer(p)) = 1
		}
	}
	return _pid

`,
	}
	tclsqlitePatches = []string{
		"Xopen64:",
		"Xopen:",
		// ----
		`
var _ unsafe.Pointer
`,
		`
var (
	cmdMu  sync.Mutex
	cmdMap = map[int]*exec.Cmd{}
)
`,
		// ----
		`
func XTclpCreateProcess(tls crt.TLS, _interp uintptr /* *TTcl_Interp = struct{XresultDon...ntptr);XerrorLineDontUse int32;} */, _argc int32, _argv uintptr /* **int8 */, _inputFile uintptr /* TTclFile = *STclFile_ */, _outputFile uintptr /* TTclFile = *STclFile_ */, _errorFile uintptr /* TTclFile = *STclFile_ */, _pidPtr uintptr /* **STcl_Pid_ */) (r int32) {
	esc := crt.MustMalloc(280)
`,
		`
func XTclpCreateProcess(tls crt.TLS, _interp uintptr /* *TTcl_Interp = struct{XresultDon...ntptr);XerrorLineDontUse int32;} */, _argc int32, _argv uintptr /* **int8 */, _inputFile uintptr /* TTclFile = *STclFile_ */, _outputFile uintptr /* TTclFile = *STclFile_ */, _errorFile uintptr /* TTclFile = *STclFile_ */, _pidPtr uintptr /* **STcl_Pid_ */) (r int32) {
	var argv []string
	var cmd *exec.Cmd
	esc := crt.MustMalloc(280)
`,
		// ----
		`
	_pid = crt.Xvfork(tls)
	if _pid != int32(0) {
		goto _8
	}

	_joinThisError = bool2int((_errorFile != 0) && (_errorFile == _outputFile))
	_fd = int32(*(*uintptr)(unsafe.Pointer(_errPipeOut))) - int32(1)
	if _1916SetupStdFile(tls, _inputFile, int32(2)) != 0 && _1916SetupStdFile(tls, _outputFile, int32(4)) != 0 && (_joinThisError != 0 || _1916SetupStdFile(tls, _errorFile, int32(8)) != 0) && (_joinThisError == 0 || crt.Xdup2(tls, int32(1), int32(2)) != int32(-1) && crt.Xfcntl(tls, int32(2), int32(2), int32(0)) == int32(0)) {
		goto _9
	}

	crt.Xsprintf(tls, _errSpace, ts+83465 /* "%dforked process couldn't set up..." */, *(*int32)(unsafe.Pointer(crt.X__errno_location(tls))))
	_len = crt.Xstrlen(tls, _errSpace)
	if _len == uint64(crt.Xwrite(tls, _fd, _errSpace, _len)) {
		goto _10
	}

	XTcl_Panic(tls, ts+83511 /* "TclpCreateProcess: unable to wri..." */)
_10:
	crt.X_exit(tls, int32(1))
_9:
	_1917RestoreSignals(tls)
	crt.Xexecvp(tls, *(*uintptr)(unsafe.Pointer(_newArgv)), _newArgv)
	crt.Xsprintf(tls, _errSpace, ts+83560 /* "%dcouldn't execute \"%.150s\"" */, *(*int32)(unsafe.Pointer(crt.X__errno_location(tls))), *(*uintptr)(unsafe.Pointer(_argv)))
	_len = crt.Xstrlen(tls, _errSpace)
	if _len == uint64(crt.Xwrite(tls, _fd, _errSpace, _len)) {
		goto _11
	}

	XTcl_Panic(tls, ts+83511 /* "TclpCreateProcess: unable to wri..." */)
_11:
	crt.X_exit(tls, int32(1))
_8:
`,
		`
	_ = _len
	_ = _joinThisError
	_pid = 0
	for p := _newArgv; ; p += unsafe.Sizeof(uintptr(0)) {
		q := *(*uintptr)(unsafe.Pointer(p))
		if q == 0 {
			break
		}

		argv = append(argv, crt.GoString(q))
	}
	cmd = exec.Command(argv[0], argv[1:]...)
	if fd := _inputFile; fd != 0 {
		fd--
		f := os.NewFile(fd, "inputFile")
		if f == nil {
			panic(fd)
		}

		cmd.Stdin = f
	}
	if fd := _outputFile; fd != 0 {
		fd--
		f := os.NewFile(fd, "outputFile")
		if f == nil {
			panic(fd)
		}

		cmd.Stdout = f
	}
	if fd := _errorFile; fd != 0 {
		fd--
		f := os.NewFile(fd, "errorFile")
		if f == nil {
			panic(fd)
		}

		cmd.Stderr = f
	}
	if err := cmd.Start(); err != nil {
		panic(err.Error)
	}

	_pid = int32(cmd.Process.Pid)
	cmdMu.Lock()
	cmdMap[int(_pid)] = cmd
	cmdMu.Unlock()
`,
		// ----
		`
func XTcl_WaitPid(tls crt.TLS, _pid uintptr /* TTcl_Pid = *STcl_Pid_ */, _statPtr uintptr /* *int32 */, _options int32) (r uintptr /* TTcl_Pid = *STcl_Pid_ */) {
	var (
		_result int32
		_real_pid int32
	)
	_real_pid = int32(_pid)
`,
		`
func XTcl_WaitPid(tls crt.TLS, _pid uintptr /* TTcl_Pid = *STcl_Pid_ */, _statPtr uintptr /* *int32 */, _options int32) (r uintptr /* TTcl_Pid = *STcl_Pid_ */) {
	var (
		_result   int32
		_real_pid int32
	)
	_real_pid = int32(_pid)
	cmdMu.Lock()
	cmd := cmdMap[int(_real_pid)]
	cmdMu.Unlock()
	if cmd == nil {
		panic(_real_pid)
	}

	cmd.Wait()
	cmdMu.Lock()
	delete(cmdMap, int(_real_pid))
	cmdMu.Unlock()
	switch st := cmd.ProcessState; {
	case st.Success():
		if p := _statPtr; p != 0 {
			*(*int32)(unsafe.Pointer(p)) = 0
		}
	default:
		if p := _statPtr; p != 0 {
			*(*int32)(unsafe.Pointer(p)) = 1
		}
	}
	return _pid

`,
	}
)

func init() {
	var err error
	if searchPaths, err = c99.Paths(true); err != nil {
		panic(err)
	}
}

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

func translate(tweaks *c99.Tweaks, includePaths, sysIncludePaths []string, def string, sources ...c99.Source) (*c99.TranslationUnit, error) {
	in := []c99.Source{c99.MustBuiltin()}
	if def != "" {
		in = append(in, c99.NewStringSource("<defines>", def))
	}
	in = append(in, sources...)
	if *oTrace {
		fmt.Fprintln(os.Stderr, in)
	}
	return c99.Translate(tweaks, includePaths, sysIncludePaths, in...)
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

	tweaks := &c99.Tweaks{
		// TrackExpand:                 func(s string) { fmt.Print(s) }, //TODO-
		EnableAnonymousStructFields: true,
		EnableEmptyStructs:          true,
		EnableImplicitBuiltins:      true,
		EnableImplicitDeclarations:  true,
		EnableOmitFuncDeclSpec:      true,
		EnablePointerCompatibility:  true, // CSmith transparent_crc_bytes
		EnableReturnExprInVoidFunc:  true,
		IgnorePragmas:               true,
		InjectFinalNL:               true,
	}
	inc := append([]string{"@"}, inc2...)

	crt0, err := c99.Translate(tweaks, inc, searchPaths, c99.MustBuiltin(), c99.MustCrt0())
	if err != nil {
		return nil, err
	}

	tus := []*c99.TranslationUnit{crt0}
	for _, v := range pth {
		tu, err := translate(tweaks, inc, searchPaths, def, c99.MustFileSource2(v, false))
		if err != nil {
			//dbg("cc: %v", errString(err)) //TODO-
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
		//dbg("ccgo: %v", errString(err)) //TODO-
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
		// dbg("build: %v", errString(err)) //TODO-
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
	switch {
	case err != nil:
		//dbg("run: %v", errString(err)) //TODO-
	default:
		*run++
	}
	return out, err
}

func TestTCC(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

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

	var cc, ccgo, build, run, ok, n int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		run0 := run
		n++
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
	if cc != n || ccgo != n || build != n || run != n || ok != n {
		t.Fatalf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	}

	if *oEdit {
		fmt.Printf("TCC\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestOther(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

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

	var cc, ccgo, build, run, ok, n int
	for _, pth := range m {
		if b := filepath.Base(pth); b == "log.c" && *oRE != "log.c" || re != nil && !re.MatchString(b) {
			continue
		}

		run0 := run
		n++
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
	if cc != n || ccgo != n || build != n || run != n || ok != n {
		t.Fatalf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	}

	if *oEdit {
		fmt.Printf("Other\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestGCC(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

	const def = `
#define SIGNAL_SUPPRESS // gcc.c-torture/execute/20101011-1.c
`
	blacklist := map[string]struct{}{
		"20010904-1.c":    {}, // __attribute__((aligned(32)))
		"20010904-2.c":    {}, // __attribute__((aligned(32)))
		"20021127-1.c":    {}, // non standard GCC behavior
		"pr23467.c":       {}, // __attribute__ ((aligned (8)))
		"pr67037.c":       {}, // void f(); f(); f(42)
		"pushpop_macro.c": {}, // #pragma push_macro("_")
		"pr17377.c":       {}, // undefined: "__builtin_return_address"

		"20000703-1.c":                 {}, //TODO statement expression
		"20040411-1.c":                 {}, //TODO VLA
		"20040423-1.c":                 {}, //TODO VLA
		"20040629-1.c":                 {}, //TODO bits, arithmetic precision
		"20040705-1.c":                 {}, //TODO bits, arithmetic precision
		"20040705-2.c":                 {}, //TODO bits, arithmetic precision
		"20041218-2.c":                 {}, //TODO VLA
		"20101011-1.c":                 {}, //TODO Needs sigfpe on int division by zero
		"921016-1.c":                   {}, //TODO bits, arithmetic precision
		"970217-1.c":                   {}, //TODO VLA
		"alias-4.c":                    {}, //TODO __attribute__ ((alias ("a")))
		"bitfld-1.c":                   {}, //TODO bits, arithmetic precision
		"bitfld-3.c":                   {}, //TODO bits, arithmetic precision
		"built-in-setjmp.c":            {}, //TODO undefined: "__builtin_setjmp"
		"builtin-constant.c":           {}, //TODO undefined: "__builtin_constant_p"
		"builtin-types-compatible-p.c": {}, //TODO must track type qualifiers
		"pr19449.c":                    {}, //TODO undefined: "__builtin_constant_p"
		"pr32244-1.c":                  {}, //TODO bits, arithmetic precision
		"pr34971.c":                    {}, //TODO bits, arithmetic precision
		"pr37780.c":                    {}, //TODO undefined: "__builtin_ctz"
		"pr47237.c":                    {}, //TODO undefined: "__builtin_apply"
		"pr60003.c":                    {}, //TODO undefined: "__builtin_setjmp"
		"pr64006.c":                    {}, //TODO undefined: "__builtin_mul_overflow"
		"pr68381.c":                    {}, //TODO undefined: "__builtin_mul_overflow"
		"pr77767.c":                    {}, //TODO VLA
		"zerolen-2.c":                  {}, //TODO The Go translation makes the last zero items array to have size 1.
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
	if run == 0 || run != build || ok != build {
		t.Fatalf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	}

	if *oEdit {
		fmt.Printf("GCC\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestSQLiteShell(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

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
	if out, err := test(t, false, &cc, &ccgo, &build, &run, `
		#define HAVE_FDATASYNC 1
		#define HAVE_ISNAN 1
		#define HAVE_LOCALTIME_R 1
		#define HAVE_USLEEP 1
		#define SQLITE_DEBUG 1
		#define SQLITE_MEMDEBUG 1
		#define _LARGEFILE64_SOURCE 1
		/* #define HAVE_MALLOC_USABLE_SIZE 1 */
`,
		"",
		nil,
		dir,
		[]string{
			filepath.Join(root, "shell.c"),
			filepath.Join(root, "sqlite3.c"),
		},
		"foo", "create table t(i)",
	); err != nil {
		t.Fatalf("%s: %v", out, errString(err))
	}

	if run == 1 {
		ok++
	}
	if ok != 1 {
		t.Fatalf("cc %v ccgo %v build %v run %v ok %v", cc, ccgo, build, run, ok)
	}

	if *oEdit {
		fmt.Printf("Shell\tcc %v ccgo %v build %v run %v ok %v\n", cc, ccgo, build, run, ok)
	}
}

func TestTCL(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

	const (
		defs = `
			// Output of gcc features.c && ./a.out in github.com/cznic/sqlite2go/internal/c99/headers on linux_amd64.
			#define _DEFAULT_SOURCE 1
			#define _POSIX_C_SOURCE 200809
			#define _POSIX_SOURCE 1

			// TCL
			#define CFG_INSTALL_BINDIR "library"
			#define CFG_INSTALL_DOCDIR "library"
			#define CFG_INSTALL_INCDIR "library"
			#define CFG_INSTALL_LIBDIR "library"
			#define CFG_INSTALL_SCRDIR "library"
			#define CFG_RUNTIME_BINDIR "library"
			#define CFG_RUNTIME_DOCDIR "library"
			#define CFG_RUNTIME_INCDIR "library"
			#define CFG_RUNTIME_LIBDIR "library"
			#define CFG_RUNTIME_SCRDIR "library"
			#define HAVE_SYS_TIME_H 1
			#define HAVE_UNISTD_H 1
			#define TCL_CFGVAL_ENCODING "iso8859-1"
			#define TCL_LIBRARY "library"
			#define TCL_PACKAGE_PATH "library"
			#define TCL_THREADS 1
			#define TIME_WITH_SYS_TIME 1
			#define USE_VFORK 1
`
	)

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-tcl-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	root := "../../_tcl8.6.8/"
	tweaks := &c99.Tweaks{
		// TrackExpand: func(s string) { fmt.Print(s) }, //TODO-
		// TrackIncludes:               func(s string) { fmt.Printf("#include %s\n", s) }, //TODO-
		EnableAnonymousStructFields: true,
	}
	inc := []string{
		filepath.FromSlash(filepath.Join(root, "generic")),
		filepath.FromSlash(filepath.Join(root, "unix")),
		filepath.FromSlash(filepath.Join(root, "libtommath")),
		"@",
	}
	sysInc := append(searchPaths, inc...)
	crt0, err := c99.Translate(tweaks, inc, searchPaths, c99.MustBuiltin(), c99.MustCrt0())
	if err != nil {
		t.Fatal(errString(err))
	}

	tus := []*c99.TranslationUnit{crt0}

	m, err := filepath.Glob(filepath.FromSlash(filepath.Join(root, "libtommath/*.c")))
	if err != nil {
		t.Fatal(err)
	}

	for i, v := range m {
		m[i] = v[len(root):]
	}

	for _, v := range append([]string{
		"generic/regcomp.c",
		"generic/regerror.c",
		"generic/regexec.c",
		"generic/regfree.c",
		"generic/tclAlloc.c",
		"generic/tclAssembly.c",
		"generic/tclAsync.c",
		"generic/tclBasic.c",
		"generic/tclBinary.c",
		"generic/tclCkalloc.c",
		"generic/tclClock.c",
		"generic/tclCmdAH.c",
		"generic/tclCmdIL.c",
		"generic/tclCmdMZ.c",
		"generic/tclCompCmds.c",
		"generic/tclCompCmdsGR.c",
		"generic/tclCompCmdsSZ.c",
		"generic/tclCompExpr.c",
		"generic/tclCompile.c",
		"generic/tclConfig.c",
		"generic/tclDate.c",
		"generic/tclDictObj.c",
		"generic/tclDisassemble.c",
		"generic/tclEncoding.c",
		"generic/tclEnsemble.c",
		"generic/tclEnv.c",
		"generic/tclEvent.c",
		"generic/tclExecute.c",
		"generic/tclFCmd.c",
		"generic/tclFileName.c",
		"generic/tclGet.c",
		"generic/tclHash.c",
		"generic/tclHistory.c",
		"generic/tclIO.c",
		"generic/tclIOCmd.c",
		"generic/tclIOGT.c",
		"generic/tclIORChan.c",
		"generic/tclIORTrans.c",
		"generic/tclIOSock.c",
		"generic/tclIOUtil.c",
		"generic/tclIndexObj.c",
		"generic/tclInterp.c",
		"generic/tclLink.c",
		"generic/tclListObj.c",
		"generic/tclLiteral.c",
		"generic/tclLoad.c",
		"generic/tclLoadNone.c", // TclGuessPackageName
		"generic/tclMain.c",
		"generic/tclNamesp.c",
		"generic/tclNotify.c",
		"generic/tclOO.c",
		"generic/tclOOBasic.c",
		"generic/tclOOCall.c",
		"generic/tclOODefineCmds.c",
		"generic/tclOOInfo.c",
		"generic/tclOOMethod.c",
		"generic/tclOOStubInit.c",
		"generic/tclObj.c",
		"generic/tclOptimize.c",
		"generic/tclPanic.c",
		"generic/tclParse.c",
		"generic/tclPathObj.c",
		"generic/tclPipe.c",
		"generic/tclPkg.c",
		"generic/tclPkgConfig.c",
		"generic/tclPosixStr.c",
		"generic/tclPreserve.c",
		"generic/tclProc.c",
		"generic/tclRegexp.c",
		"generic/tclResolve.c",
		"generic/tclResult.c",
		"generic/tclScan.c",
		"generic/tclStrToD.c",
		"generic/tclStringObj.c",
		"generic/tclStubInit.c",
		"generic/tclThread.c",
		"generic/tclThreadStorage.c",
		"generic/tclTimer.c",
		"generic/tclTomMathInterface.c",
		"generic/tclTrace.c",
		"generic/tclTrace.c",
		"generic/tclUtf.c",
		"generic/tclUtil.c",
		"generic/tclVar.c",
		"generic/tclZlib.c",
		"unix/tclAppInit.c",
		"unix/tclUnixChan.c",
		"unix/tclUnixCompat.c",
		"unix/tclUnixEvent.c",
		"unix/tclUnixFCmd.c",
		"unix/tclUnixFile.c",
		"unix/tclUnixInit.c",
		"unix/tclUnixNotfy.c",
		"unix/tclUnixPipe.c",
		"unix/tclUnixSock.c",
		"unix/tclUnixThrd.c",
		"unix/tclUnixTime.c",
	}, m...) {
		tu, err := translate(tweaks, inc, sysInc, defs, c99.MustFileSource2(filepath.FromSlash(filepath.Join(root, v)), false))
		if err != nil {
			t.Fatal(errString(err))
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
	"os/exec"
	"sync"
	"unsafe"

	"github.com/cznic/crt"
)
`)
	if err := command(w, &tus, patches(tclPatches)...); err != nil {
		t.Fatal(err)
	}

	c99.FlushCache()
	debug.FreeOSMemory()

	if err := w.Flush(); err != nil {
		t.Fatal(err)
	}

	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	if out, err := exec.Command("go", "build", "-o", filepath.Join(dir, "tcl"), f.Name()).CombinedOutput(); err != nil {
		t.Fatalf("%s\n%v", out, err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Error(err)
		}
	}()

	src, err := filepath.Abs(filepath.Join(cwd, filepath.FromSlash("../..")))
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	if err := cpDir(dir, filepath.Join(src, filepath.FromSlash("_tcl8.6.8/tests")), nil); err != nil {
		t.Fatal(err)
	}

	if err := cpDir(filepath.Join(dir, "library"), filepath.Join(src, filepath.FromSlash("_tcl8.6.8/library")), nil); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command("./tcl", "all.tcl")
	errPipe, err := cmd.StderrPipe()
	if err != nil {
		t.Fatal(err)
	}

	pipe, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}

	ch := make(chan int)
	var errLines []string
	go func() {
		sc := bufio.NewScanner(errPipe)
		for sc.Scan() {
			errLines = append(errLines, sc.Text())
		}
		close(ch)
	}()

	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}

	const (
		testsEnded = "Tests ended"
		results    = "all.tcl:	Total	"
	)
	const (
		st0 = iota // Looking for prefix
		st1        // Seen prefix
	)
	sc := bufio.NewScanner(pipe)
	state := st0
	nline := 0
	var lines []string
	var resultLine string
	for sc.Scan() {
		line := sc.Text()
		if *oTrace {
			fmt.Fprintln(os.Stderr, line)
		}
		switch state {
		case st0:
			if !strings.HasPrefix(line, testsEnded) {
				nline++
				switch {
				case nline <= 50:
					lines = append(lines, line)
				case nline == 51:
					lines = append(lines, "\n...\n")
				}
				continue
			}

			state = st1
			fallthrough
		case st1:
			lines = append(lines, line)
			if strings.HasPrefix(line, results) {
				for i := len(line) - 1; ; i-- {
					if c := line[i]; c < '0' || c > '9' {
						n, err := strconv.Atoi(line[i+1:])
						if err != nil {
							resultLine = err.Error()
							break
						}

						if n != 0 {
							resultLine = line
						}
						break
					}
				}
			}
		default:
			panic(fmt.Errorf("internal error: %v", state))
		}
	}
	err = cmd.Wait()
	<-ch
	t.Logf("%s", strings.Join(lines, "\n"))
	if err != nil || !cmd.ProcessState.Success() || resultLine != "" {
		t.Fatalf("Failed: %v\n%s\n%s", err, resultLine, strings.Join(errLines, "\n"))
	}
}

func patch(b *[]byte, old, new []byte) error {
	if !bytes.Contains(*b, old) {
		s := old
		if len(s) > 100 {
			s = append(s[:100], " ..."...)
		}
		dbg("patch failed: %q", s) //TODO-
		return nil                 //TODO-
		return fmt.Errorf("patch failed: %q", s)
	}

	*b = bytes.Replace(*b, old, new, 1)
	return nil
}

func patches(a []string) (r []func(*[]byte) error) {
	if len(a)&1 != 0 {
		panic("internal error")
	}

	for i := 0; i < len(a); i += 2 {
		j := i
		r = append(r, func(b *[]byte) error {
			return patch(b, []byte(a[j]), []byte(a[j+1]))
		})
	}
	return r
}

func TestTCLSQLite(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

	const (
		defs = `
			// Output of gcc features.c && ./a.out in github.com/cznic/sqlite2go/internal/c99/headers on linux_amd64.
			#define _DEFAULT_SOURCE 1
			#define _POSIX_C_SOURCE 200809
			#define _POSIX_SOURCE 1
`
		sqliteDefs = defs + `
			#define SQLITE_CORE 1 // Must be defined for TCL extensions to work.
			#define SQLITE_DEBUG 1
			#define SQLITE_ENABLE_RBU 1
			#define SQLITE_PRIVATE
			#define SQLITE_TEST 1
			#define TCLSH_INIT_PROC sqlite3TestInit
`

		tclDefs = defs + `
			#define CFG_INSTALL_BINDIR "library"
			#define CFG_INSTALL_DOCDIR "library"
			#define CFG_INSTALL_INCDIR "library"
			#define CFG_INSTALL_LIBDIR "library"
			#define CFG_INSTALL_SCRDIR "library"
			#define CFG_RUNTIME_BINDIR "library"
			#define CFG_RUNTIME_DOCDIR "library"
			#define CFG_RUNTIME_INCDIR "library"
			#define CFG_RUNTIME_LIBDIR "library"
			#define CFG_RUNTIME_SCRDIR "library"
			#define HAVE_SYS_TIME_H 1
			#define HAVE_UNISTD_H 1
			#define TCL_CFGVAL_ENCODING "iso8859-1"
			#define TCL_LIBRARY "library"
			#define TCL_PACKAGE_PATH "library"
			#define TCL_THREADS 1
			#define TIME_WITH_SYS_TIME 1
			#define USE_VFORK 1
`
	)

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-tcl-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}
	testdir := filepath.Join(dir, "test")
	if err := mkdir(testdir); err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("../.."))
	if err != nil {
		t.Fatal(err)
	}

	tclRoot := filepath.Join(root, "_tcl8.6.8")
	sqliteRoot := filepath.Join(root, "_sqlite")

	sqliteTweaks := &c99.Tweaks{
		// TrackExpand: func(s string) { fmt.Print(s) }, //TODO-
		// TrackIncludes:               func(s string) { fmt.Printf("#include %s\n", s) }, //TODO-
		EnableAnonymousStructFields: true,
	}

	sqliteInc := []string{
		"@",
		filepath.Join(sqliteRoot, "sqlite-amalgamation-3210000"),
		filepath.Join(tclRoot, "generic"),
	}
	sysInc := append(searchPaths, sqliteInc...)
	crt0, err := c99.Translate(sqliteTweaks, sqliteInc, searchPaths, c99.MustBuiltin(), c99.MustCrt0())
	if err != nil {
		t.Fatal(errString(err))
	}
	tus := []*c99.TranslationUnit{crt0}

	for _, v := range []string{
		"ext/fts5/fts5_tcl.c",
		"ext/misc/amatch.c",
		"ext/misc/carray.c",
		"ext/misc/closure.c",
		"ext/misc/csv.c",
		"ext/misc/eval.c",
		"ext/misc/fileio.c",
		"ext/misc/fuzzer.c",
		"ext/misc/ieee754.c",
		"ext/misc/mmapwarm.c",
		"ext/misc/nextchar.c",
		"ext/misc/percentile.c",
		"ext/misc/regexp.c",
		"ext/misc/remember.c",
		"ext/misc/series.c",
		"ext/misc/spellfix.c",
		"ext/misc/totype.c",
		"ext/misc/unionvtab.c",
		"ext/misc/wholenumber.c",
		"ext/rbu/test_rbu.c",
		"sqlite-amalgamation-3210000/sqlite3.c",
		"src/tclsqlite.c",
		"src/test1.c",
		"src/test2.c",
		"src/test3.c",
		"src/test4.c",
		"src/test5.c",
		"src/test6.c",
		"src/test7.c",
		"src/test8.c",
		"src/test9.c",
		"src/test_async.c",
		"src/test_autoext.c",
		"src/test_backup.c",
		"src/test_bestindex.c",
		"src/test_blob.c",
		"src/test_btree.c",
		"src/test_config.c",
		"src/test_delete.c",
		"src/test_demovfs.c",
		"src/test_devsym.c",
		"src/test_fs.c",
		"src/test_func.c",
		"src/test_hexio.c",
		"src/test_init.c",
		"src/test_intarray.c",
		"src/test_journal.c",
		"src/test_malloc.c",
		"src/test_md5.c",
		"src/test_multiplex.c",
		"src/test_mutex.c",
		"src/test_onefile.c",
		"src/test_osinst.c",
		"src/test_pcache.c",
		"src/test_quota.c",
		"src/test_rtree.c",
		"src/test_schema.c",
		"src/test_superlock.c",
		"src/test_syscall.c",
		"src/test_tclsh.c",
		"src/test_tclvar.c",
		"src/test_thread.c",
		"src/test_vfs.c",
	} {
		tu, err := translate(sqliteTweaks, sqliteInc, sysInc, sqliteDefs, c99.MustFileSource2(filepath.Join(sqliteRoot, v), false))
		if err != nil {
			t.Fatal(errString(err))
		}

		tus = append(tus, tu)
	}

	tclTweaks := &c99.Tweaks{
		// TrackExpand: func(s string) { fmt.Print(s) }, //TODO-
		// TrackIncludes:               func(s string) { fmt.Printf("#include %s\n", s) }, //TODO-
		EnableAnonymousStructFields: true,
	}
	tclInc := []string{
		filepath.Join(tclRoot, "generic"),
		filepath.Join(tclRoot, "unix"),
		filepath.Join(tclRoot, "libtommath"),
		"@",
	}
	sysInc = append(searchPaths, tclInc...)

	m, err := filepath.Glob(filepath.Join(tclRoot, "libtommath", "*.c"))
	if err != nil {
		t.Fatal(err)
	}

	for i, v := range m {
		m[i] = v[len(tclRoot):]
	}

	for _, v := range append([]string{
		"generic/regcomp.c",
		"generic/regerror.c",
		"generic/regexec.c",
		"generic/regfree.c",
		"generic/tclAlloc.c",
		"generic/tclAssembly.c",
		"generic/tclAsync.c",
		"generic/tclBasic.c",
		"generic/tclBinary.c",
		"generic/tclCkalloc.c",
		"generic/tclClock.c",
		"generic/tclCmdAH.c",
		"generic/tclCmdIL.c",
		"generic/tclCmdMZ.c",
		"generic/tclCompCmds.c",
		"generic/tclCompCmdsGR.c",
		"generic/tclCompCmdsSZ.c",
		"generic/tclCompExpr.c",
		"generic/tclCompile.c",
		"generic/tclConfig.c",
		"generic/tclDate.c",
		"generic/tclDictObj.c",
		"generic/tclDisassemble.c",
		"generic/tclEncoding.c",
		"generic/tclEnsemble.c",
		"generic/tclEnv.c",
		"generic/tclEvent.c",
		"generic/tclExecute.c",
		"generic/tclFCmd.c",
		"generic/tclFileName.c",
		"generic/tclGet.c",
		"generic/tclHash.c",
		"generic/tclHistory.c",
		"generic/tclIO.c",
		"generic/tclIOCmd.c",
		"generic/tclIOGT.c",
		"generic/tclIORChan.c",
		"generic/tclIORTrans.c",
		"generic/tclIOSock.c",
		"generic/tclIOUtil.c",
		"generic/tclIndexObj.c",
		"generic/tclInterp.c",
		"generic/tclLink.c",
		"generic/tclListObj.c",
		"generic/tclLiteral.c",
		"generic/tclLoad.c",
		"generic/tclLoadNone.c", // TclGuessPackageName
		"generic/tclMain.c",
		"generic/tclNamesp.c",
		"generic/tclNotify.c",
		"generic/tclOO.c",
		"generic/tclOOBasic.c",
		"generic/tclOOCall.c",
		"generic/tclOODefineCmds.c",
		"generic/tclOOInfo.c",
		"generic/tclOOMethod.c",
		"generic/tclOOStubInit.c",
		"generic/tclObj.c",
		"generic/tclOptimize.c",
		"generic/tclPanic.c",
		"generic/tclParse.c",
		"generic/tclPathObj.c",
		"generic/tclPipe.c",
		"generic/tclPkg.c",
		"generic/tclPkgConfig.c",
		"generic/tclPosixStr.c",
		"generic/tclPreserve.c",
		"generic/tclProc.c",
		"generic/tclRegexp.c",
		"generic/tclResolve.c",
		"generic/tclResult.c",
		"generic/tclScan.c",
		"generic/tclStrToD.c",
		"generic/tclStringObj.c",
		"generic/tclStubInit.c",
		"generic/tclThread.c",
		"generic/tclThreadStorage.c",
		"generic/tclTimer.c",
		"generic/tclTomMathInterface.c",
		"generic/tclTrace.c",
		"generic/tclUtf.c",
		"generic/tclUtil.c",
		"generic/tclVar.c",
		"generic/tclZlib.c",
		"unix/tclUnixChan.c",
		"unix/tclUnixCompat.c",
		"unix/tclUnixEvent.c",
		"unix/tclUnixFCmd.c",
		"unix/tclUnixFile.c",
		"unix/tclUnixInit.c",
		"unix/tclUnixNotfy.c",
		"unix/tclUnixPipe.c",
		"unix/tclUnixSock.c",
		"unix/tclUnixThrd.c",
		"unix/tclUnixTime.c",
	}, m...) {
		tu, err := translate(tclTweaks, tclInc, sysInc, tclDefs, c99.MustFileSource2(filepath.Join(tclRoot, v), false))
		if err != nil {
			t.Fatal(errString(err))
		}

		tus = append(tus, tu)
	}

	f, err := os.Create(filepath.Join(testdir, "main.go"))
	if err != nil {
		t.Fatal(err)
	}

	w := bufio.NewWriter(f)
	w.WriteString(`package main
	
import (
	"math"
	"os"
	"os/exec"
	"sync"
	"unsafe"

	"github.com/cznic/crt"
)
`)
	if err := command(w, &tus, patches(tclsqlitePatches)...); err != nil {
		t.Fatal(err)
	}

	c99.FlushCache()
	debug.FreeOSMemory()

	if err := w.Flush(); err != nil {
		t.Fatal(err)
	}

	if err := f.Close(); err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(testdir); err != nil {
		t.Fatal(err)
	}

	if out, err := exec.Command("go", "build", "-o", "test", f.Name()).CombinedOutput(); err != nil {
		t.Fatalf("%s\n%v", out, err)
	}

	if err := cpDir(filepath.Join(dir, "ext"), filepath.Join(root, filepath.FromSlash("_sqlite/ext")), nil); err != nil {
		t.Fatal(err)
	}

	if err := cpDir(filepath.Join(dir, "library"), filepath.Join(root, filepath.FromSlash("_tcl8.6.8/library")), nil); err != nil {
		t.Fatal(err)
	}

	if err := cpDir(filepath.Join(dir, "test"), filepath.Join(root, filepath.FromSlash("_sqlite/test")), nil); err != nil {
		t.Fatal(err)
	}

	blacklist := []string{
		"autovacuum_ioerr2.test", // crt.Xfstatfs
		"backup5.test",           // crt.Xfstatfs
		"boundary4.test",         // ??? manually passes
		"btree01.test",           // ??? manually passes
		"btree02.test",           // ??? manually passes
		"btreefault.test",        // crt.Xfstatfs
		"busy.test",              // ??? manually passes
		"cache.test",             // ??? manually passes
		"cacheflush.test",        // crt.Xfstatfs
		"cffault.test",           // crt.Xfstatfs
		"collate1.test",          // hangs
		"collate2.test",          // hangs
		"collate3.test",          // hangs
		"collate4.test",          // hangs
		"collate5.test",          // hangs
		"collate6.test",          // hangs
		"collate9.test",          // hangs
		"corruptC.test",          // crt.Xfstatfs
		"crash.test",             // hangs
		"crash4.test",            // hangs
		"crash7.test",            // crt.Xfstatfs
		"date.test",              // crt.Xselect
		"e_reindex.test",         // hangs
		"e_walauto.test",         // hangs
		"fuzz.test",              // memory corruption
		"fuzzerfault.test",       // crt.Xfstatfs
		"incrcorrupt.test",       // crt.Xftruncate
		"journal3.test",          // crt.Xfchmod
		"lock.test",              // hangs
		"lock4.test",             // crt.Xselect
		"lock5.test",             // crt.Xutimes
		"misc1.test",             // hangs
		"misc7.test",             // crt.Xatof
		"mjournal.test",          // crt.Xfopen
		"nan.test",               // hangs
		"pager4.test",            // crt.Xrename
		"pagerfault.test",        // crt.Xfstatfs
		"pagerfault2.test",       // crt.Xfstatfs
		"pagerfault3.test",       // crt.Xfstatfs
		"pragma.test",            // crt.X__assert_fail
		"printf.test",            // hangs
		"quota2.test",            // crt.Xfopen
		"rbu.test",               // crt.Xrename
		"reindex.test",           // hangs
		"select9.test",           // hangs
		"shared9.test",           // hangs
		"sharedA.test",           // crt.Xpthread_attr_init
		"sort2.test",             // crt.Xpthread_create
		"sort3.test",             // hangs
		"sort4.test",             // crt.Xpthread_create
		"sortfault.test",         // memory corruption
		"statfault.test",         // crt.Xfstatfs
		"symlink.test",           // crt.Xsymlink
		"syscall.test",           // crt.Xfstatfs
		"tempfault.test",         // memory corruption
		"thread001.test",         // crt.Xpthread_attr_init
		"thread002.test",         // crt.Xpthread_attr_init
		"thread003.test",         // crt.Xpthread_attr_init
		"thread004.test",         // crt.Xpthread_attr_init
		"thread005.test",         // crt.Xpthread_attr_init
		"thread1.test",           // crt.Xpthread_create
		"thread2.test",           // crt.Xpthread_create
		"tkt-fc62af4523.test",    // memory corruption
		"tkt3997.test",           // hangs
		"trans.test",             // couldn't open "test.tcl": permission denied
		"unionvtabfault.test",    // crt.Xfstatfs
		"vacuum2.test",           // hangs
		"vtabH.test",             // crt.Xreaddir64_r
		"wal.test",               // crt.Xfstatfs
		"wal2.test",              // memory corruption
		"wal3.test",              // crt.Xfstatfs
		"wal4.test",              // crt.Xfstatfs
		"walcrash2.test",         // memory corruption
		"walcrash4.test",         // crt.Xfstatfs
		"walslow.test",           // crt.Xfstatfs
		"walthread.test",         // crt.Xpthread_attr_init
		// "crash2.test", // Error: invalid command name "db"
		// "crash3.test", // Error: invalid command name "db"
		// "crash6.test", // couldn't open "crash.tcl": permission denied
		// "e_createtable.test", // unknown switch: -error
		// "e_delete.test", // unknown switch: -error
		// "e_insert.test", // unknown switch: -error
		// "e_select.test",  // unknown switch: -error
		// "e_update.test", // unknown switch: -error
		// "exists.test", // couldn't open "tf_main.tcl": permission denied
		// "mmap1.test", // couldn't open "tf_main.tcl": permission denied
		// "mmap4.test", // couldn't open "tf_main.tcl": permission denied
		// "multiplex2.test", // couldn't open "tf_main.tcl": permission denied
		// "pager1.test", // couldn't open "tf_main.tcl": permission denied
		// "rowallock.test", // couldn't open "tf_main.tcl": permission denied
		// "savepoint.test", // couldn't open "tf_main.tcl": permission denied
		// "schema3.test", // couldn't open "tf_main.tcl": permission denied
		// "shared2.test", // couldn't open "tf_main.tcl": permission denied
		// "superlock.test", // couldn't open "tf_main.tcl": permission denied
		// "tkt-5d863f876e.test", // couldn't open "tf_main.tcl": permission denied
		// "unixexcl.test", // couldn't open "tf_main.tcl": permission denied
		// "wal5.test",    // couldn't open "tf_main.tcl": permission denied
		// "walro.test",     // couldn't open "tf_main.tcl": permission denied
	}

	for _, v := range blacklist {
		if err := os.Remove(filepath.Join(testdir, v)); err != nil {
			t.Fatal(err)
		}
	}

	cmd := exec.Command("./test", "all.test")
	pipe, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}

	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}

	const maxFails = 10
	var green, red int
	var fail []string
	sc := bufio.NewScanner(pipe)
	var lastStdout, lastOk, lastFinished string
	for sc.Scan() {
		lastStdout = sc.Text()
		if *oTrace {
			fmt.Fprintln(os.Stderr, lastStdout)
		}
		switch {
		case strings.HasPrefix(lastStdout, "!") && strings.Contains(lastStdout, "expected"):
			if len(fail) < maxFails {
				fail = append(fail, lastStdout)
			}
		case
			strings.HasPrefix(lastStdout, "Error:"),
			strings.HasPrefix(lastStdout, "!") && strings.Contains(lastStdout, "got"):

			red++
			if len(fail) < maxFails {
				fail = append(fail, lastStdout)
			}
		case strings.HasPrefix(lastStdout, "Time: "):
			lastFinished = lastStdout
		case strings.HasSuffix(lastStdout, "... Ok") && !strings.Contains(lastStdout, "-closeallfiles") && !strings.Contains(lastStdout, "-sharedcachesetting"):
			green++
			lastOk = lastStdout
		}
	}
	total := green + red
	if red > len(fail) {
		fail = append(fail, "... too many fails")
	}
	t.Logf(`
Test cases: %8d
Pass:       %8d (%3.2f%%)
Fail:       %8d (%3.2f%%)
%s`,
		total, green, 100*float64(green)/float64(total), red, 100*float64(red)/float64(total), strings.Join(fail, "\n"),
	)
	if err := cmd.Wait(); err != nil || len(blacklist) != 0 {
		t.Fatalf(`
Test binary exit error: %v
Last completed test file: %q
Last passed test: %q
Last line written to stdout: %q
Blacklisted test files: %d
%s`, err, lastFinished, lastOk, lastStdout, len(blacklist), strings.Join(blacklist, "\n"))
	}
}

func TestCSmith(t *testing.T) {
	defer func() {
		c99.FlushCache()
		debug.FreeOSMemory()
	}()

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
			"--no-packed-struct",     // --packed-struct | --no-packed-struct: enable | disable packed structs by adding #pragma pack(1) before struct definition (disabled by default).
			"--no-volatile-pointers", // --volatile-pointers | --no-volatile-pointers: enable | disable volatile pointers (enabled by default).
			"--no-volatiles",         // --volatiles | --no-volatiles: enable | disable volatiles (enabled by default).
			"--paranoid",             // --paranoid | --no-paranoid: enable | disable pointer-related assertions (disabled by default).
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
			ctx, cancel := context.WithTimeout(context.Background(), testTimeout/10)

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
