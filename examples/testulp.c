/**********************************************************************
 *                                                                    *
 * Comparison functions for single & double precision floating point  *
 * numbers, which function correctly in the presence of NaNs &        *
 * infinities but don't account for denormalised numbers              *
 *                                                                    *
 * J.Sloan                                                            *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * (the License, or (at your option) any later version.               *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           *
 * 02111-1307  USA                                                    *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 *********************************************************************/

#include "cbf_ulp.h"
#include "unittest.h"

#include <stdio.h>
#include <string.h>
#include <float.h>

/*
32-bit quiet NaN:
0b s111 1111 11xx xxxx xxxx xxxx xxxx xxxx
sign, s, and payload, x, are not important
*/

float _pqnan_32()
{
	const unsigned int i = 0x7fffffff;
	const float d = *(float*)&i;
	return d;
}

float _nqnan_32()
{
	const unsigned int i = 0xffffffff;
	const float d = *(float*)&i;
	return d;
}

/*
32-bit signalling NaN:
0b s111 1111 10xx xxxx xxxx xxxx xxxx xxxx
sign, s, is not important
payload, x, must be non-zero
*/

float _psnan_32()
{
	const unsigned int i = 0x7fbfffff;
	const float d = *(float*)&i;
	return d;
}

float _nsnan_32()
{
	const unsigned int i = 0xffbfffff;
	const float d = *(float*)&i;
	return d;
}

/*
Tests
*/

testResult_t test_isinf32()
{
	testResult_t r = {0,0,0};

	/*
	test +ve infinities
	*/

	unsigned int px = 0x7f800000;
	unsigned int nx = 0xff800000;

	TEST(isinf32(*(float*)&px));
	TEST(isinf32(*(float*)&nx));

#define TEST_flipped_n(n) \
do { \
	const unsigned int px##n = px^(1<<n); \
	const unsigned int nx##n = nx^(1<<n); \
	TEST(!isinf32(*(float*)&px##n)); \
	TEST(!isinf32(*(float*)&nx##n)); \
} while (0)

	TEST_flipped_n(0);
	TEST_flipped_n(1);
	TEST_flipped_n(2);
	TEST_flipped_n(3);
	TEST_flipped_n(4);
	TEST_flipped_n(5);
	TEST_flipped_n(6);
	TEST_flipped_n(7);
	TEST_flipped_n(8);
	TEST_flipped_n(9);
	TEST_flipped_n(10);
	TEST_flipped_n(11);
	TEST_flipped_n(12);
	TEST_flipped_n(13);
	TEST_flipped_n(14);
	TEST_flipped_n(15);
	TEST_flipped_n(16);
	TEST_flipped_n(17);
	TEST_flipped_n(18);
	TEST_flipped_n(19);
	TEST_flipped_n(20);
	TEST_flipped_n(21);
	TEST_flipped_n(22);
	TEST_flipped_n(23);
	TEST_flipped_n(24);
	TEST_flipped_n(25);
	TEST_flipped_n(26);
	TEST_flipped_n(27);
	TEST_flipped_n(28);
	TEST_flipped_n(29);
	TEST_flipped_n(30);

#undef TEST_flipped_n

	return r;
}

testResult_t test_isnan32()
{
	testResult_t r = {0,0,0};

	/*
	test NaNs
	*/

	unsigned int px = 0x7f800000;
	unsigned int nx = 0xff800000;

	TEST(!isnan32(*(float*)&px));
	TEST(!isnan32(*(float*)&nx));

#define TEST_flipped_n(n) \
do { \
	const unsigned int px##n = px^(1<<n); \
	const unsigned int nx##n = nx^(1<<n); \
	TEST(isnan32(*(float*)&px##n)); \
	TEST(isnan32(*(float*)&nx##n)); \
} while (0)

	TEST_flipped_n(0);
	TEST_flipped_n(1);
	TEST_flipped_n(2);
	TEST_flipped_n(3);
	TEST_flipped_n(4);
	TEST_flipped_n(5);
	TEST_flipped_n(6);
	TEST_flipped_n(7);
	TEST_flipped_n(8);
	TEST_flipped_n(9);
	TEST_flipped_n(10);
	TEST_flipped_n(11);
	TEST_flipped_n(12);
	TEST_flipped_n(13);
	TEST_flipped_n(14);
	TEST_flipped_n(15);
	TEST_flipped_n(16);
	TEST_flipped_n(17);
	TEST_flipped_n(18);
	TEST_flipped_n(19);
	TEST_flipped_n(20);
	TEST_flipped_n(21);
	TEST_flipped_n(22);

#undef TEST_flipped_n

#define TEST_flipped_n(n) \
do { \
	const unsigned int px##n = px^(1<<n); \
	const unsigned int nx##n = nx^(1<<n); \
	TEST(!isnan32(*(float*)&px##n)); \
	TEST(!isnan32(*(float*)&nx##n)); \
} while (0)

	TEST_flipped_n(23);
	TEST_flipped_n(24);
	TEST_flipped_n(25);
	TEST_flipped_n(26);
	TEST_flipped_n(27);
	TEST_flipped_n(28);
	TEST_flipped_n(29);
	TEST_flipped_n(30);

#undef TEST_flipped_n

	return r;
}

testResult_t test_ulp32()
{
	testResult_t r = {0,0,0};

	/* define +ve & -ve special values. */
	const float pzero = 0.0f;
	const float nzero = -0.0f;
	const float eps = FLT_EPSILON;
	const float pinf = 1.0f/0.0f;
	const float ninf = -1.0f/0.0f;
	const float pqnan = _pqnan_32(); /* +ve quiet nan */
	const float nqnan = _nqnan_32(); /* -ve quiet nan */
	const float psnan = _psnan_32(); /* +ve signalling nan */
	const float nsnan = _nsnan_32(); /* -ve signalling nan */
	const unsigned int u32max = 0xffffffff;

	/* Zeros */

	TEST(*(unsigned int*)&pzero != *(unsigned int*)&nzero);
	TEST(cbf_ULP32(pzero,pzero) == 0);
	TEST(cbf_ULP32(pzero,nzero) == 0);
	TEST(cbf_ULP32(nzero,pzero) == 0);
	TEST(cbf_ULP32(nzero,nzero) == 0);

	TEST(cbf_ULP32(pzero,1.0) > 0);
	TEST(cbf_ULP32(nzero,1.0) > 0);

	TEST(cbf_ULP32(pzero,-1.0) > 0);
	TEST(cbf_ULP32(nzero,-1.0) > 0);

	/* Normal numbers */

	TEST(cbf_ULP32(1.0,1.0) == 0);
	TEST(cbf_ULP32(-1.0,-1.0) == 0);

	TEST(cbf_ULP32(1.0,1.0+eps) == 1);
	TEST(cbf_ULP32(1.0+eps,1.0) == 1);
	TEST(cbf_ULP32(-1.0,-1.0-eps) == 1);
	TEST(cbf_ULP32(-1.0-eps,-1.0) == 1);

	TEST(cbf_ULP32(1.0,1.0+2*eps) == 2);
	TEST(cbf_ULP32(1.0+2*eps,1.0) == 2);
	TEST(cbf_ULP32(-1.0,-1.0-2*eps) == 2);
	TEST(cbf_ULP32(-1.0-2*eps,-1.0) == 2);

	TEST(cbf_ULP32(1.0,2.0) == cbf_ULP32(-1.0,-2.0));
	TEST(cbf_ULP32(2.0,4.0) == cbf_ULP32(-2.0,-4.0));

	/* INFs */

	TEST(*(unsigned int*)&pinf != *(unsigned int*)&ninf);
	TEST(cbf_ULP32(pinf,pinf)==0);
	TEST(cbf_ULP32(pinf,ninf)==u32max);
	TEST(cbf_ULP32(ninf,pinf)==u32max);
	TEST(cbf_ULP32(ninf,ninf)==0);

	TEST(cbf_ULP32(pinf,pzero)==u32max);
	TEST(cbf_ULP32(pinf,nzero)==u32max);

	TEST(cbf_ULP32(ninf,pzero)==u32max);
	TEST(cbf_ULP32(ninf,nzero)==u32max);

	TEST(cbf_ULP32(pinf,1.0)==u32max);
	TEST(cbf_ULP32(ninf,1.0)==u32max);

	TEST(cbf_ULP32(pinf,-1.0)==u32max);
	TEST(cbf_ULP32(ninf,-1.0)==u32max);

	TEST(pinf == pinf);
	TEST(pinf != ninf);
	TEST(ninf != pinf);
	TEST(ninf == ninf);

	/* NaNs */

	TEST(cbf_ULP32(pqnan,pqnan)==u32max);
	TEST(cbf_ULP32(pqnan,nqnan)==u32max);
	TEST(cbf_ULP32(pqnan,psnan)==u32max);
	TEST(cbf_ULP32(pqnan,nsnan)==u32max);

	TEST(cbf_ULP32(nqnan,pqnan)==u32max);
	TEST(cbf_ULP32(nqnan,nqnan)==u32max);
	TEST(cbf_ULP32(nqnan,psnan)==u32max);
	TEST(cbf_ULP32(nqnan,nsnan)==u32max);

	TEST(cbf_ULP32(psnan,pqnan)==u32max);
	TEST(cbf_ULP32(psnan,nqnan)==u32max);
	TEST(cbf_ULP32(psnan,psnan)==u32max);
	TEST(cbf_ULP32(psnan,nsnan)==u32max);

	TEST(cbf_ULP32(nsnan,pqnan)==u32max);
	TEST(cbf_ULP32(nsnan,nqnan)==u32max);
	TEST(cbf_ULP32(nsnan,psnan)==u32max);
	TEST(cbf_ULP32(nsnan,nsnan)==u32max);

	TEST(cbf_ULP32(pqnan,pzero)==u32max);
	TEST(cbf_ULP32(nqnan,pzero)==u32max);
	TEST(cbf_ULP32(psnan,pzero)==u32max);
	TEST(cbf_ULP32(nsnan,pzero)==u32max);

	TEST(cbf_ULP32(pqnan,nzero)==u32max);
	TEST(cbf_ULP32(nqnan,nzero)==u32max);
	TEST(cbf_ULP32(psnan,nzero)==u32max);
	TEST(cbf_ULP32(nsnan,nzero)==u32max);

	TEST(cbf_ULP32(pqnan,1.0)==u32max);
	TEST(cbf_ULP32(nqnan,1.0)==u32max);
	TEST(cbf_ULP32(psnan,1.0)==u32max);
	TEST(cbf_ULP32(nsnan,1.0)==u32max);

	TEST(cbf_ULP32(pqnan,-1.0)==u32max);
	TEST(cbf_ULP32(nqnan,-1.0)==u32max);
	TEST(cbf_ULP32(psnan,-1.0)==u32max);
	TEST(cbf_ULP32(nsnan,-1.0)==u32max);

	return r;
}


#ifndef NO_UINT64_TYPE

/*
64-bit quiet NaN:
0b s111 1111 1111 1xxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx
sign, s, and payload, x, are not important
*/

double _pqnan_64()
{
	double d;
	const uint64_t i = 0x7fffffffffffffffl;
	memcpy((void*)&d,(void*)&i,8);
	return d;
}

double _nqnan_64()
{
	double d;
	const uint64_t i = 0xffffffffffffffffl;
	memcpy((void*)&d,(void*)&i,8);
	return d;
}

/*
64-bit signalling NaN:
0b s111 1111 1111 0xxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxxx
sign, s, is not important
payload, x, must be non-zero
*/

double _psnan_64()
{
	double d;
	const uint64_t i = 0x7ff8ffffffffffffl;
	memcpy((void*)&d,(void*)&i,8);
	return d;
}

double _nsnan_64()
{
	double d;
	const uint64_t i = 0xfff8ffffffffffffl;
	memcpy((void*)&d,(void*)&i,8);
	return d;
}

testResult_t test_isinf64()
{
	testResult_t r = {0,0,0};

	/*
	test +ve infinities
	*/

	uint64_t px = 0x7ff0000000000000l;
	uint64_t nx = 0xfff0000000000000l;

	TEST(isinf64(*(double*)&px));
	TEST(isinf64(*(double*)&nx));

#define TEST_flipped_n(n) \
do { \
	const uint64_t one = 1; \
	const uint64_t px##n = px^(one<<n); \
	const uint64_t nx##n = nx^(one<<n); \
	TEST(!isinf64(*(double*)&px##n)); \
	TEST(!isinf64(*(double*)&nx##n)); \
} while (0)

	TEST_flipped_n(0);
	TEST_flipped_n(1);
	TEST_flipped_n(2);
	TEST_flipped_n(3);
	TEST_flipped_n(4);
	TEST_flipped_n(5);
	TEST_flipped_n(6);
	TEST_flipped_n(7);
	TEST_flipped_n(8);
	TEST_flipped_n(9);
	TEST_flipped_n(10);
	TEST_flipped_n(11);
	TEST_flipped_n(12);
	TEST_flipped_n(13);
	TEST_flipped_n(14);
	TEST_flipped_n(15);
	TEST_flipped_n(16);
	TEST_flipped_n(17);
	TEST_flipped_n(18);
	TEST_flipped_n(19);
	TEST_flipped_n(20);
	TEST_flipped_n(21);
	TEST_flipped_n(22);
	TEST_flipped_n(23);
	TEST_flipped_n(24);
	TEST_flipped_n(25);
	TEST_flipped_n(26);
	TEST_flipped_n(27);
	TEST_flipped_n(28);
	TEST_flipped_n(29);
	TEST_flipped_n(30);
	TEST_flipped_n(31);
	TEST_flipped_n(32);
	TEST_flipped_n(33);
	TEST_flipped_n(34);
	TEST_flipped_n(35);
	TEST_flipped_n(36);
	TEST_flipped_n(37);
	TEST_flipped_n(38);
	TEST_flipped_n(39);
	TEST_flipped_n(40);
	TEST_flipped_n(41);
	TEST_flipped_n(42);
	TEST_flipped_n(43);
	TEST_flipped_n(44);
	TEST_flipped_n(45);
	TEST_flipped_n(46);
	TEST_flipped_n(47);
	TEST_flipped_n(48);
	TEST_flipped_n(49);
	TEST_flipped_n(50);
	TEST_flipped_n(51);
	TEST_flipped_n(52);
	TEST_flipped_n(53);
	TEST_flipped_n(54);
	TEST_flipped_n(55);
	TEST_flipped_n(56);
	TEST_flipped_n(57);
	TEST_flipped_n(58);
	TEST_flipped_n(59);
	TEST_flipped_n(60);
	TEST_flipped_n(61);
	TEST_flipped_n(62);

#undef TEST_flipped_n

	return r;
}

testResult_t test_isnan64()
{
	testResult_t r = {0,0,0};

	/*
	test NaNs
	*/

	uint64_t px = 0x7ff0000000000000l;
	uint64_t nx = 0xfff0000000000000l;

	TEST(!isnan64(*(double*)&px));
	TEST(!isnan64(*(double*)&nx));

#define TEST_flipped_n(n) \
do { \
	const uint64_t one = 1; \
	const uint64_t px##n = px^(one<<n); \
	const uint64_t nx##n = nx^(one<<n); \
	TEST(isnan64(*(double*)&px##n)); \
	TEST(isnan64(*(double*)&nx##n)); \
} while (0)

	TEST_flipped_n(0);
	TEST_flipped_n(1);
	TEST_flipped_n(2);
	TEST_flipped_n(3);
	TEST_flipped_n(4);
	TEST_flipped_n(5);
	TEST_flipped_n(6);
	TEST_flipped_n(7);
	TEST_flipped_n(8);
	TEST_flipped_n(9);
	TEST_flipped_n(10);
	TEST_flipped_n(11);
	TEST_flipped_n(12);
	TEST_flipped_n(13);
	TEST_flipped_n(14);
	TEST_flipped_n(15);
	TEST_flipped_n(16);
	TEST_flipped_n(17);
	TEST_flipped_n(18);
	TEST_flipped_n(19);
	TEST_flipped_n(20);
	TEST_flipped_n(21);
	TEST_flipped_n(22);
	TEST_flipped_n(23);
	TEST_flipped_n(24);
	TEST_flipped_n(25);
	TEST_flipped_n(26);
	TEST_flipped_n(27);
	TEST_flipped_n(28);
	TEST_flipped_n(29);
	TEST_flipped_n(30);
	TEST_flipped_n(31);
	TEST_flipped_n(32);
	TEST_flipped_n(33);
	TEST_flipped_n(34);
	TEST_flipped_n(35);
	TEST_flipped_n(36);
	TEST_flipped_n(37);
	TEST_flipped_n(38);
	TEST_flipped_n(39);
	TEST_flipped_n(40);
	TEST_flipped_n(41);
	TEST_flipped_n(42);
	TEST_flipped_n(43);
	TEST_flipped_n(44);
	TEST_flipped_n(45);
	TEST_flipped_n(46);
	TEST_flipped_n(47);
	TEST_flipped_n(48);
	TEST_flipped_n(49);
	TEST_flipped_n(50);
	TEST_flipped_n(51);

#undef TEST_flipped_n

#define TEST_flipped_n(n) \
do { \
	const uint64_t px##n = px^(1l<<n); \
	const uint64_t nx##n = nx^(1l<<n); \
	TEST(!isnan64(*(double*)&px##n)); \
	TEST(!isnan64(*(double*)&nx##n)); \
} while (0)

	TEST_flipped_n(52);
	TEST_flipped_n(53);
	TEST_flipped_n(54);
	TEST_flipped_n(55);
	TEST_flipped_n(56);
	TEST_flipped_n(57);
	TEST_flipped_n(58);
	TEST_flipped_n(59);
	TEST_flipped_n(60);
	TEST_flipped_n(61);
	TEST_flipped_n(62);

#undef TEST_flipped_n

	return r;
}

testResult_t test_ulp64()
{
	testResult_t r = {0,0,0};

	/* define +ve & -ve special values. */
	const double pzero = 0.0;
	const double nzero = -0.0;
	const double eps = DBL_EPSILON;
	const double pinf = 1.0/0.0;
	const double ninf = -1.0/0.0;
	const double pqnan = _pqnan_64(); /* +ve quiet nan */
	const double nqnan = _nqnan_64(); /* -ve quiet nan */
	const double psnan = _psnan_64(); /* +ve signalling nan */
	const double nsnan = _nsnan_64(); /* -ve signalling nan */

	/* Zeros */

	TEST(*(int64_t*)&pzero != *(int64_t*)&nzero);
	TEST(cbf_ULP64(pzero,pzero) == 0);
	TEST(cbf_ULP64(pzero,nzero) == 0);
	TEST(cbf_ULP64(nzero,pzero) == 0);
	TEST(cbf_ULP64(nzero,nzero) == 0);

	TEST(cbf_ULP64(pzero,1.0) > 0);
	TEST(cbf_ULP64(nzero,1.0) > 0);

	TEST(cbf_ULP64(pzero,-1.0) > 0);
	TEST(cbf_ULP64(nzero,-1.0) > 0);

	/* Normal numbers */

	TEST(cbf_ULP64(1.0,1.0) == 0);
	TEST(cbf_ULP64(-1.0,-1.0) == 0);

	TEST(cbf_ULP64(1.0,1.0+eps) == 1);
	TEST(cbf_ULP64(1.0+eps,1.0) == 1);
	TEST(cbf_ULP64(-1.0,-1.0-eps) == 1);
	TEST(cbf_ULP64(-1.0-eps,-1.0) == 1);

	TEST(cbf_ULP64(1.0,1.0+2*eps) == 2);
	TEST(cbf_ULP64(1.0+2*eps,1.0) == 2);
	TEST(cbf_ULP64(-1.0,-1.0-2*eps) == 2);
	TEST(cbf_ULP64(-1.0-2*eps,-1.0) == 2);

	TEST(cbf_ULP64(1.0,2.0) == cbf_ULP64(-1.0,-2.0));
	TEST(cbf_ULP64(2.0,4.0) == cbf_ULP64(-2.0,-4.0));

	/* INFs */

	TEST(*(int64_t*)&pinf != *(int64_t*)&ninf);
	TEST(cbf_ULP64(pinf,pinf)==0);
	TEST(cbf_ULP64(pinf,ninf)==UINT64_MAX);
	TEST(cbf_ULP64(ninf,pinf)==UINT64_MAX);
	TEST(cbf_ULP64(ninf,ninf)==0);

	TEST(cbf_ULP64(pinf,pzero)==UINT64_MAX);
	TEST(cbf_ULP64(pinf,nzero)==UINT64_MAX);

	TEST(cbf_ULP64(ninf,pzero)==UINT64_MAX);
	TEST(cbf_ULP64(ninf,nzero)==UINT64_MAX);

	TEST(cbf_ULP64(pinf,1.0)==UINT64_MAX);
	TEST(cbf_ULP64(ninf,1.0)==UINT64_MAX);

	TEST(cbf_ULP64(pinf,-1.0)==UINT64_MAX);
	TEST(cbf_ULP64(ninf,-1.0)==UINT64_MAX);

	TEST(pinf == pinf);
	TEST(pinf != ninf);
	TEST(ninf != pinf);
	TEST(ninf == ninf);

	/* NaNs */

	TEST(cbf_ULP64(pqnan,pqnan)==UINT64_MAX);
	TEST(cbf_ULP64(pqnan,nqnan)==UINT64_MAX);
	TEST(cbf_ULP64(pqnan,psnan)==UINT64_MAX);
	TEST(cbf_ULP64(pqnan,nsnan)==UINT64_MAX);

	TEST(cbf_ULP64(nqnan,pqnan)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,nqnan)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,psnan)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,nsnan)==UINT64_MAX);

	TEST(cbf_ULP64(psnan,pqnan)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,nqnan)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,psnan)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,nsnan)==UINT64_MAX);

	TEST(cbf_ULP64(nsnan,pqnan)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,nqnan)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,psnan)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,nsnan)==UINT64_MAX);

	TEST(cbf_ULP64(pqnan,pzero)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,pzero)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,pzero)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,pzero)==UINT64_MAX);

	TEST(cbf_ULP64(pqnan,nzero)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,nzero)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,nzero)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,nzero)==UINT64_MAX);

	TEST(cbf_ULP64(pqnan,1.0)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,1.0)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,1.0)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,1.0)==UINT64_MAX);

	TEST(cbf_ULP64(pqnan,-1.0)==UINT64_MAX);
	TEST(cbf_ULP64(nqnan,-1.0)==UINT64_MAX);
	TEST(cbf_ULP64(psnan,-1.0)==UINT64_MAX);
	TEST(cbf_ULP64(nsnan,-1.0)==UINT64_MAX);

	return r;
}

#endif


testResult_t test_monotonic()
{
	testResult_t r = {0,0,0};

	/* initialise some variables */
	const unsigned int end = 0x7FFFFFFFu;
	unsigned int i = 0x00000000;
	float fp_pos = 0.0;
	float fp_neg = 0.0;
	int invalid = 0;
	/*
	'k' is the number of blocks to split the tests into,so the progress
	of the tests can be monitored. Should be a power of 2 to ensure
	nothing strange happens around the end of the range of tests.
	'block' is just a counter.
	*/
	unsigned int k = 256, block = 0;

	/* start testing */
	fprintf(stdout,"testing 32-bit floating point monotonicity & sign...\n");
	fflush(stdout);
	while (block++!=k) {
		while (block*(end/k) != ++i) {
			const unsigned int i_pos = i;
			const unsigned int i_neg = i|0x80000000;
			const float f_pos = *(float*)(&i_pos);
			const float f_neg = *(float*)(&i_neg);
			if (!invalid) {
				if (isnan32(f_pos) || isinf32(f_pos)) invalid = 1;
				if (isnan32(f_neg) || isinf32(f_neg)) invalid = 1;
			} else {
				if (!(isnan32(f_pos) || isinf32(f_pos))) {
					fprintf(stderr,"\nFail: finite +ve values have a gap @ %#8.8x\n",i_pos);
					r.fail = 1;
				}
				if (!(isnan32(f_neg) || isinf32(f_neg))) {
					fprintf(stderr,"\nFail: finite -ve values have a gap @ %#8.8x\n",i_neg);
					r.fail = 1;
				}
			}
			if (!invalid) {
				if (!(f_pos>fp_pos)) {
					fprintf(stderr,"\nFail: +ve values not monotonically increasing @ %#8.8x\n",i_pos);
					r.fail = 1;
				}
				if (!(f_neg<fp_neg)) {
					fprintf(stderr,"\nFail: -ve values not monotonically increasing @ %#8.8x\n",i_neg);
					r.fail = 1;
				}
				if (!(f_pos>0.)) {
					fprintf(stderr,"\nFail: unexpected sign for supposedly +ve number @ %#8.8x\n",i_pos);
					r.fail = 1;
				}
				if (!(f_neg<0.)) {
					fprintf(stderr,"\nFail: unexpected sign for supposedly -ve number @ %#8.8x\n",i_neg);
					r.fail = 1;
				}
			}
			if (r.fail) return r;
			fp_pos = f_pos;
			fp_neg = f_neg;
		}
		/* mark each block as 'done' by writing a period character, */
		fprintf(stdout,".");
		/* with a simple way to output multiple lines for usability. */
		if (0==(block&(64-1))) fprintf(stdout,"\n");
		fflush(stdout);
	}
	if (!r.fail) ++r.pass;
	return r;
}

testResult_t test_basic32()
{
	testResult_t r = {0,0,0};

	if (4!=sizeof(float)) {
		++r.fail;
		fprintf(stderr,"Fail: unexpected size of float type (%lu, expected 4)\n",sizeof(float));
	} else {
		++r.pass;
	}
	if (4!=sizeof(unsigned int)) {
		++r.fail;
		fprintf(stderr,"Fail: unexpected size of unsigned int type (%lu, expected 4)\n",sizeof(unsigned int));
	} else {
		++r.pass;
	}
	{
		const unsigned int z_pos = 0x00000000;
		const unsigned int z_neg = 0x80000000;
		const float f_pos = *(float*)(&z_pos);
		const float f_neg = *(float*)(&z_neg);
		if (f_pos != f_neg) {
			++r.fail;
			fprintf(stderr,"Fail: +ve zero (%#8.8x) != -ve zero (%#8.8x)\n",z_pos,z_neg);
		} else {
			++r.pass;
		}
	}
	if (1.f/0.f < FLT_MAX) {
		++r.fail;
		fprintf(stderr,"Fail: infinity (1.f/0.f) < FLT_MAX\n");
	} else {
		++r.pass;
	}
	return r;
}

testResult_t test_basic64()
{
	testResult_t r = {0,0,0};

	if (8!=sizeof(double)) {
		++r.fail;
		fprintf(stderr,"Fail: unexpected size of double type (%lu, expected 8)\n",sizeof(double));
	} else {
		++r.pass;
	}
	if (8!=sizeof(uint64_t)) {
		++r.fail;
		fprintf(stderr,"Fail: unexpected size of uint64_t type (%lu, expected 8)\n",sizeof(uint64_t));
	} else {
		++r.pass;
	}
	{
		const uint64_t z_pos = 0x0000000000000000l;
		const uint64_t z_neg = 0x8000000000000000l;
		const double f_pos = *(double*)(&z_pos);
		const double f_neg = *(double*)(&z_neg);
		if (f_pos != f_neg) {
			++r.fail;
			fprintf(stderr,"Fail: +ve zero (%#16.16lx) != -ve zero (%#16.16lx)\n",z_pos,z_neg);
		} else {
			++r.pass;
		}
	}
	if (1./0. < DBL_MAX) {
		++r.fail;
		fprintf(stderr,"Fail: infinity (1./0.) < DBL_MAX\n");
	} else {
		++r.pass;
	}
	return r;
}

int main()
{
	testResult_t r = {0,0,0};

	TEST_COMPONENT(test_basic32());
	TEST_COMPONENT(test_isinf32());
	TEST_COMPONENT(test_isnan32());
	TEST_COMPONENT(test_ulp32());

#ifndef NO_UINT64_TYPE
	if (cbf_has_ULP64()) {
		TEST_COMPONENT(test_basic64());
		TEST_COMPONENT(test_isinf64());
		TEST_COMPONENT(test_isnan64());
		TEST_COMPONENT(test_ulp64());
	}
	else
#endif
	{
		r.skip += 4;
		printf("64-bit unsigned int not available: skipped 64-bit tests\n");
	}

	TEST_COMPONENT(test_monotonic());

	/* Done, output the results */

	printf("%d passed\n%d failed\n%d components skipped\n", r.pass, r.fail, r.skip);
	return r.fail ? 1 : 0;
}
