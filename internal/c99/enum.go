// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

type cond int

const (
	condZero cond = iota

	condIfOff
	condIfOn
	condIfSkip

	maxCond
)

var (
	condOn = [maxCond]bool{
		condIfOn: true,
		condZero: true,
	}
)
