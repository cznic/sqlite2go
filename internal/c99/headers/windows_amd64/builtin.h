//  Copyright 2017 The CCIR Authors. All rights reserved.
//  Use of this source code is governed by a BSD-style
//  license that can be found in the LICENSE file.

// +build ignore

#ifndef _BUILTIN_H_
#define _BUILTIN_H_

#include "predefined.h"

typedef struct {
	void *_[2];		// Go *[]interface{}
} *__builtin_va_list;

#define _MSC_VER 1900
#define _MSVCRT_
#define _MSC_EXTENSIONS
#define __int8 char
#define BYTE unsigned char
#define __int16 short
#define __int32 int
#define __int64 long long
// disable SSE and other instrisics we dont support
#define _X86INTRIN_H_INCLUDED 1
#undef __SSE__
#undef __SSE2__
#undef __MMX__
// bypass bug that fails for: #define MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS (_WIN32_WINNT >= 0x0502 || !defined (_WINBASE_))
#define MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS 1
#define __unaligned
#define __inline__ inline
#define __forceinline inline __attribute__((__always_inline__,__gnu_inline__))

// TODO: __builtin_fprintf(stderr, "%s:%i.%s STUB %s called!\n", __FILE__, __LINE__, __func__, #x), __builtin_abort(), 0)
#define abort_stubbed(x) 0

// maybe: `#define __CRT__NO_INLINE` to remove some unnecessary stuff
// Implementing __readgsqword as macro also prevents an anonymous union access 
// (which isn't supported in C99 actually WTF)
#define __readfsdword(x) abort_stubbed("__readfsdword")
#define __readgsqword(x) abort_stubbed("__readgsqword")

// TODO: ? not relevant for SQLite since we have mutexes
#define _mm_mfence()

#define __FUNCTION__ __func__
#define __attribute(x)
#define __attribute__(x)
#define __builtin_choose_expr(a, b, c) (a) ? (b) : (c)
#define __builtin_expect(exp, c) (exp)
#define __builtin_offsetof(st, m) ((__SIZE_TYPE__)(&((st *)0)->m))
#define __builtin_prefetch(addr, ...) (void)(addr)
#define __builtin_signbit(x) (sizeof(x) == sizeof(float) ? __signbitf(x) : sizeof (x) == sizeof(double) ? __signbit(x) : __signbitl(x))
#define __builtin_types_compatible_p(type1, type2) __builtin_types_compatible__((type1){}, (type2){})
#define __complex__ _Complex
#define __const const
#define __extension__
#define __inline inline
#define __restrict restrict
#define __roundup(n, mod) ((n + mod - 1) & ~(mod - 1))
#define __volatile volatile

typedef void *__FILE_TYPE__;
#define _FILE_DEFINED
typedef void *FILE;
//typedef void *__jmp_buf[7];

// prevent annoying _iob stuff (and better to do it directly)
extern FILE stdin;
extern FILE stdout;
extern FILE stderr;
#define _STDSTREAM_DEFINED

__FILE_TYPE__ __builtin_fopen(char *__filename, char *__modes);
__SIZE_TYPE__ __builtin_strlen(char *__s);
__UINT64_TYPE__ __builtin_bswap64(__UINT64_TYPE__ x);
char *__builtin_strchr(char *__s, int __c);
char *__builtin_strcpy(char *__dest, char *__src);
double __builtin_copysign(double x, double y);
int __builtin_abs(int j);
int __builtin_clrsb(int x);
int __builtin_clrsbl(long x);
int __builtin_clrsbll(long long x);
int __builtin_clz(unsigned x);
int __builtin_clzl(unsigned long x);
int __builtin_clzll(unsigned long long x);
int __builtin_ctz(unsigned x);
int __builtin_ctzl(unsigned long x);
int __builtin_ctzll(unsigned long long x);
int __builtin_ffs(int i);
int __builtin_ffsl(long i);
int __builtin_ffsll(long long i);
int __builtin_fprintf(void *__stream, char *__format, ...);
int __builtin_isprint(int);
int __builtin_memcmp(void *__s1, void *__s2, __SIZE_TYPE__ __n);
int __builtin_parity(unsigned x);
int __builtin_parityl(unsigned long x);
int __builtin_parityll(unsigned long long x);
int __builtin_popcount(unsigned x);
int __builtin_popcountl(unsigned long x);
int __builtin_popcountll(unsigned long long x);
int __builtin_printf(char *__format, ...);
int __builtin_setjmp(void *__env);
int __builtin_sprintf(char *__s, char *__format, ...);
int __builtin_strcmp(char *__s1, char *__s2);
int __signbit(double x);
int __signbitf(float x);
void *__builtin_alloca(__SIZE_TYPE__ __size);
void *__builtin_frame_address(unsigned int level);
void *__builtin_malloc(__SIZE_TYPE__ __size);
void *__builtin_memcpy(void *dest, const void *src, __SIZE_TYPE__ n);
void *__builtin_memset(void *s, int c, __SIZE_TYPE__ n);
void *__builtin_return_address(unsigned int level);
void *__va_end;
void *__va_start;
void __builtin_abort(void);
void __builtin_exit(int __status);
void __builtin_longjmp(void *__env, int __val);
void __builtin_trap(void);
void __register_stdfiles(void *, void *, void *, void *);

#define _INC_STDARG
#define __builtin_va_arg(ap, type) (type)ap
#define __builtin_va_copy(dest, src) dest = src
#define __builtin_va_end(ap) ap = __va_end
#define __builtin_va_start(ap, arg) ap = __va_start

#ifdef _WIN32
#ifndef _WIN64
#define InterlockedCompareExchange(d,e,c) _InterlockedCompareExchange(d,e,c)
#endif

long _InterlockedCompareExchange(long volatile *Destination, long Exchange, long Comparand);
#endif

#endif				/* _BUILTIN_H_ */
