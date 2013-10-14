/**********************************************************************
 *                                                                    *
 * Comparison functions for single & double precision floating point  *
 * numbers, which function correctly in the presence of NaNs &        *
 * infinities but don't account for denormalised numbers              *
 *                                                                    *
 *    J.Sloan, 06/2013                                                *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 *********************************************************************/

#ifdef __cplusplus

extern "C" {

#endif

#include <math.h>

#include "cbf_ulp.h"

/**
Cast to 32 bit int (assuming double & long long have same endianness),
convert from sign-magnitude to 2s-complement,
return (magnitude of) difference in ULPs.

The difference between 2 32-bit numbers is a 33-bit number, so only get the
magnitude of the difference. The usual floating point comparisons (<code>a > b</code>, etc)
can be used to find which is bigger.

This currently uses isnan and isinf, if they are not defined they should be
implemented using bitwise comparisons. isnan must not use <code>x != x</code>, or eqivalent.

\param a,b Any 32-bit non-denormalised floating point numbers.

\return The unsigned distance between <code>a</code> and <code>b</code> in 'floating point space'.
 */
unsigned int cbf_ULP32(const float a, const float b)
{
	/*
	Check for NAN or INF, ignore subnormals completely.
	Don't use 'x!=x' to test for NANs, it might be optimised out,
	test for a bit pattern manually if 'isnan' or 'isinf' can't be used.
	*/
	if (isnan32(a) || isinf32(a) || isnan32(b) || isinf32(b)) {
		if (isinf32(a) && isinf32(b) && a==b) return 0;
		return 0xffffffff;
	} else {
		const unsigned int signmask = 0x80000000;

		/* cast numbers so that ia >= ib */
		unsigned int  ia = *(unsigned int *)(a>b ? &a : &b);
		unsigned int  ib = *(unsigned int *)(a>b ? &b : &a);

		/* convert to 2's complement form */
		ia = signmask&ia ? signmask^(~ia+1) : ia;
		ib = signmask&ib ? signmask^(~ib+1) : ib;

		/* subtract, knowing that ia >= ib */
		return ia-ib;
	}
}

#ifndef NO_UINT64_TYPE

/**
Cast to 64 bit int (assuming double & long long have same endianness),
convert from sign-magnitude to 2s-complement,
return (magnitude of) difference in ULPs.

The difference between 2 64-bit numbers is a 65-bit number, so only get the
magnitude of the difference. The usual floating point comparisons (<code>a > b</code>, etc)
can be used to find which is bigger.

This currently uses isnan and isinf, if they are not defined they should be
implemented using bitwise comparisons. isnan must not use <code>x != x</code>, or eqivalent.

If no 64bit unsigned int is provided then it becomes very complicated to implement this function.

\param a,b Any 64-bit non-denormalised floating point numbers.

\return The unsigned distance between <code>a</code> and <code>b</code> in 'floating point space'.
 */
uint64_t cbf_ULP64(const double a, const double b)
{
	/*
	Check for NAN or INF, ignore subnormals completely.
	Don't use 'x!=x' to test for NANs, it might be optimised out,
	test for a bit pattern manually if 'isnan' or 'isinf' can't be used.
	*/
	if (isnan64(a) || isinf64(a) || isnan64(b) || isinf64(b)) {
		if (isinf64(a) && isinf64(b) && a==b) return 0;
		return 0xffffffffffffffffl;
	} else {
		const uint64_t signmask = 0x8000000000000000l;

		/* cast numbers so that ia >= ib */
		uint64_t ia = *(int64_t*)(a>b ? &a : &b);
		uint64_t ib = *(int64_t*)(a>b ? &b : &a);

		/* convert to 2's complement form */
		ia = signmask&ia ? signmask^(~ia+1) : ia;
		ib = signmask&ib ? signmask^(~ib+1) : ib;

		/* subtract, knowing that ia >= ib */
		return ia-ib;
	}
}

int cbf_has_ULP64() {return 1;}

#else

int cbf_has_ULP64() {return 0;}

#endif

#ifdef __cplusplus

}

#endif
