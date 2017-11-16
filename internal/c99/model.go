// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

import (
	"fmt"
	"runtime"
)

// Model describes properties of TypeKinds.
type Model map[TypeKind]ModelItem

// ModelItem describers properties of a particular TypeKind.
type ModelItem struct {
	Size        int
	Align       int
	StructAlign int
}

func newModel() (m Model, err error) {
	switch arch := runtime.GOARCH; arch {
	case "386", "arm", "armbe", "mips", "mipsle", "ppc", "ppc64le", "s390", "s390x", "sparc":
		panic("TODO")
	case "amd64p32", "mips64p32", "mips64p32le":
		panic("TODO")
	case "amd64", "arm64", "arm64be", "mips64", "mips64le", "ppc64", "sparc64":
		return Model{
			Bool:      {1, 1, 1},
			Char:      {1, 1, 1},
			Int:       {4, 4, 4},
			Long:      {8, 8, 8},
			LongLong:  {8, 8, 8},
			SChar:     {1, 1, 1},
			Short:     {2, 2, 2},
			UChar:     {1, 1, 1},
			UInt:      {4, 4, 4},
			ULong:     {8, 8, 8},
			ULongLong: {8, 8, 8},
			UShort:    {2, 2, 2},

			Float:      {4, 4, 4},
			Double:     {8, 8, 4},
			LongDouble: {8, 8, 8},

			FloatComplex:      {8, 8, 4},
			DoubleComplex:     {8, 8, 4},
			LongDoubleComplex: {8, 8, 4},

			Ptr: {8, 8, 8},
		}, nil
	default:
		return nil, fmt.Errorf("unknown/unsupported architecture %s", arch)
	}
}

// Sizeof returns the size in bytes of a variable of type t.
func (m Model) Sizeof(t Type) int64 {
	if x, ok := m[t.Kind()]; ok {
		return int64(x.Size)
	}

	panic(t)
}
