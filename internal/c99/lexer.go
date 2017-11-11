// Copyright 2017 The C99 Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package c99

// [0]: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

import (
	"bufio"
	"go/token"
	"io"
	"unicode/utf8"

	"github.com/cznic/golex/lex"
	"github.com/cznic/mathutil"
	"github.com/cznic/xc"
)

const (
	intBits  = mathutil.IntBits
	bitShift = intBits>>6 + 5
	bitMask  = intBits - 1

	scINITIAL = 0 // Start condition (shared value).
)

const (
	// Character class is an 8 bit encoding of an Unicode rune for the
	// golex generated FSM.
	//
	// Every ASCII rune is its own class.  DO NOT change any of the
	// existing values. Adding new classes is OK.
	ccEOF         = iota + 0x80
	_             // ccError
	ccOther       // Any other rune.
	ccUCNDigit    // [0], Annex D, Universal character names for identifiers - digits.
	ccUCNNonDigit // [0], Annex D, Universal character names for identifiers - non digits.
)

type trigraphs struct {
	*lex.Lexer
	pos token.Pos
	r   *bufio.Reader
	sc  int
}

func newTrigraphs(ctx *context, file *token.File, r io.Reader) (*trigraphs, error) {
	sc := scINITIAL
	if ctx.tweaks.enableTrigraphs {
		sc = scTRIGRAPHS
	}
	t := &trigraphs{
		pos: file.Pos(0),
		r:   bufio.NewReader(r),
		sc:  sc,
	}
	lx, err := lex.New(
		file,
		t,
		lex.ErrorFunc(func(pos token.Pos, msg string) { ctx.errPos(pos, msg) }),
		lex.RuneClass(func(r rune) int { return int(r) }),
	)
	if err != nil {
		return nil, err
	}

	t.Lexer = lx
	return t, nil
}

func (t *trigraphs) ReadRune() (rune, int, error) { panic("internal error") }

func (t *trigraphs) ReadChar() (c lex.Char, size int, err error) {
	size = 1
	b, err := t.r.ReadByte()
	if err != nil {
		return lex.NewChar(t.pos, rune(b)), 0, err
	}

	c = lex.NewChar(t.pos, rune(b))
	t.pos++
	return c, 1, nil
}

type ungetBuffer []xc.Token

func (u *ungetBuffer) unget(t xc.Token) { *u = append(*u, t) }

func (u *ungetBuffer) read() (t xc.Token) {
	s := *u
	n := len(s) - 1
	t = s[n]
	*u = s[:n]
	return t
}

func (u *ungetBuffer) ungets(toks ...xc.Token) {
	s := *u
	for i := len(toks) - 1; i >= 0; i-- {
		s = append(s, toks[i])
	}
	*u = s
}

type lexer struct {
	*context
	*lex.Lexer
	ast         Node
	commentPos0 token.Pos
	last        lex.Char
	mode        int // CONSTANT_EXPRESSION, TRANSLATION_UNIT
	prev        lex.Char
	sc          int
	t           *trigraphs
	ungetBuffer
}

func newLexer(ctx *context, nm string, sz int, r io.Reader) (*lexer, error) {
	file := ctx.fset.AddFile(nm, -1, sz)
	t, err := newTrigraphs(ctx, file, r)
	if err != nil {
		return nil, err
	}

	l := &lexer{
		context: ctx,
		t:       t,
	}

	lx, err := lex.New(
		file,
		l,
		lex.ErrorFunc(func(pos token.Pos, msg string) { l.errPos(pos, msg) }),
		lex.RuneClass(rune2class),
	)
	if err != nil {
		return nil, err
	}

	l.Lexer = lx
	return l, nil
}

func (l *lexer) Error(msg string)             { l.err(l.First, "%v", msg) }
func (l *lexer) ReadRune() (rune, int, error) { panic("internal error") }

func (l *lexer) Lex(lval *yySymType) (r int) {
	// defer func() { dbg("", r) }()
	//TODO use follow set to recover from errors.
	if len(l.ungetBuffer) != 0 {
		lval.Token = l.ungetBuffer.read()
		if lval.Token.Rune == PPNUMBER {
			lval.Token.Rune = INTCONST
		out:
			for _, v := range dict.S(lval.Token.Val) {
				switch v {
				case '.', '+', '-', 'e', 'E', 'p', 'P':
					lval.Token.Rune = FLOATCONST
					break out
				}
			}
		}
		return int(lval.Token.Rune)
	}

	if l.lex0(lval) < 0 {
		lval.Token.Val = 0
		return int(lval.Token.Rune)
	}

	lval.Token.Rune = l.toC(lval.Token.Rune, lval.Token.Val)
	return int(lval.Token.Rune)
}

func (l *lexer) ReadChar() (c lex.Char, size int, err error) {
	if c = l.t.Lookahead(); c.Rune == lex.RuneEOF {
		return c, 0, io.EOF
	}

	ch := l.t.scan()
	c = lex.NewChar(l.t.First.Pos(), rune(ch))
	switch {
	case ch <= 0x7f: // 1
		return c, 1, nil
	case ch <= 0xc1: // invalid
		c.Rune = utf8.RuneError
		return c, 1, nil
	case ch <= 0xdf: // 110xxxxx 10xxxxxx
		ch2 := l.t.scan()
		if ch2&0xc0 != 0x80 {
			c.Rune = utf8.RuneError
			return c, 2, nil
		}

		c.Rune = rune(ch&0x1f)<<6 | rune(ch2)&0x3f
		return c, 2, nil
	case ch <= 0xef: // 1110xxxx 10xxxxxx 10xxxxxx
		ch2 := l.t.scan()
		if ch2&0xc0 != 0x80 {
			c.Rune = utf8.RuneError
			return c, 2, nil
		}

		ch3 := l.t.scan()
		if ch3&0xc0 != 0x80 {
			c.Rune = utf8.RuneError
			return c, 3, nil
		}

		c.Rune = rune(ch&0xf)<<12 | rune(ch2)&0x3f<<6 | rune(ch3)&0x3f
		return c, 3, nil
	case ch <= 0xf4: // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
		ch2 := l.t.scan()
		if ch2&0xc0 != 0x80 {
			c.Rune = utf8.RuneError
			return c, 2, nil
		}

		ch3 := l.t.scan()
		if ch3&0xc0 != 0x80 {
			c.Rune = utf8.RuneError
			return c, 3, nil
		}

		ch4 := l.t.scan()
		if ch4&0xc0 != 0x80 {
			c.Rune = utf8.RuneError
			return c, 4, nil
		}

		c.Rune = rune(ch&0x7)<<18 | rune(ch2)&0x3f<<12 | rune(ch3)&0x3f<<6 | rune(ch4)&0x3f
		return c, 4, nil
	default: // invalid
		c.Rune = utf8.RuneError
		return c, 1, nil
	}
}

func (l *lexer) Reduced(rule, state int, lval *yySymType) (stop bool) {
	if rule != l.exampleRule {
		return false
	}

	switch x := lval.node.(type) {
	case interface {
		fragment() interface{}
	}:
		l.exampleAST = x.fragment()
	default:
		l.exampleAST = x
	}
	return true
}

func (l *lexer) comment(general bool)         { /*TODO*/ }
func (l *lexer) parseC() bool                 { return l.parse(TRANSLATION_UNIT) }
func (l *lexer) parseExpr() bool              { return l.parse(CONSTANT_EXPRESSION) }
func (l *lexer) lastPosition() token.Position { return l.fset.PositionFor(l.last.Pos(), true) }

func (l *lexer) cppScan() lex.Char {
again:
	r := l.scan()
	if r == ' ' && l.last.Rune == ' ' {
		goto again
	}

	l.prev = l.last
	l.last = lex.NewChar(l.First.Pos(), rune(r))
	return l.last
}

func (l *lexer) lex0(lval *yySymType) int {
	ch := l.scanChar()
	lval.Token = xc.Token{Char: ch}
out:
	switch ch.Rune {
	case ccEOF:
		lval.Token.Rune = -1
	case PPNUMBER:
		lval.Token.Rune = INTCONST
		s := l.TokenBytes(nil)
		lval.Token.Val = dict.ID(s)
		for _, v := range s {
			switch v {
			case '.', '+', '-', 'e', 'E', 'p', 'P':
				lval.Token.Rune = FLOATCONST
				break out
			}
		}
	default:
		if _, ok := tokHasVal[ch.Rune]; ok {
			lval.Token = xc.Token{Char: ch, Val: dict.ID(l.TokenBytes(nil))}
		}
	}
	return int(lval.Token.Rune)
}

func (l *lexer) parse(mode int) bool {
	var tok xc.Token
	tok.Rune = rune(mode)
	l.ungetBuffer = append(l.ungetBuffer, tok)
	l.mode = mode
	l.last.Rune = '\n'
	return yyParse(l) == 0
}

func (l *lexer) scanChar() (c lex.Char) {
again:
	r := l.scan()
	if r == ' ' {
		goto again
	}

	l.prev = l.last
	l.last = lex.NewChar(l.First.Pos(), rune(r))
	switch r {
	case CONSTANT_EXPRESSION, TRANSLATION_UNIT:
		l.mode = r
	}
	return l.last
}
