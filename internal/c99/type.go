// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

var (
	_ Type = (*undefinedType)(nil)

	// Undefined represents an instance of undefined type. R/O
	Undefined = &undefinedType{}
)

type undefinedType struct {
	Type
}

// Type represents a C type.
type Type interface {
	Kind() TypeKind
}
