/**********************************************************************
 *                                                                    *
 * Minimal unit test framework for CBFlib tests.                      *
 *                                                                    *
 * J. Sloan                                                           *
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
 *********************************************************************/

#ifndef _UNITTEST_H_
#define _UNITTEST_H_

/* Macros to get the current location in a file in form `file:line' */
#define __STR2(n) #n
#define __STR(n) __STR2(n)
#define __WHERE__ __FILE__":"__STR(__LINE__)

typedef struct testResult_t
{
	int pass;
	int fail;
	int skip;
} testResult_t;

/*
Test a component in the form of a test function with signature:
testResult_t testFunc(args...);
by calling it as:
TEST_COMPONENT(testFunc(foo,bar));

Logs the number of passes, failures and skipped sections in the
'testResult_t' variable 'r'. Also reports the number of failed or
skipped tests (if any), and where they failed, to 'stderr'.
*/
#define TEST_COMPONENT(testFunc) \
do { \
	const testResult_t t = (testFunc); \
	r.pass += t.pass; \
	r.fail += t.fail; \
	r.skip += t.skip; \
	if (t.fail) fprintf(stderr,"%s: test(s) failed in '%s'\n",__WHERE__,#testFunc); \
	if (t.skip) fprintf(stderr,"%s: test(s) skipped in '%s'\n",__WHERE__,#testFunc); \
} while (0)

/*
Test the boolean expression 'exp' with:
TEST(exp);
or, to invert it:
TEST(!exp);

The test will pass if the result is true, and be recorded in the
'testResult_t' variable 'r'. Also reports where the test failed and
what it was, if it failed, to 'stderr'.
*/
#define TEST(exp) \
do { \
if (!(exp)) { \
	++r.fail; \
	fprintf(stderr,"%s: test failed: '%s'\n",__WHERE__,#exp); \
} else ++r.pass; \
} while (0)

/*
Test the CBF function 'exp', which should return a CBFlib error type.

Require that the function return the error code 'CBF_SUCCESS', otherwise
report the failure was to the 'testResult_t' variable 'r' and print a
message to say where and what the failure was to 'stderr'.
*/
#define TEST_CBF_PASS(exp) \
do { \
const int err = (exp); \
error |= err; \
if (CBF_SUCCESS != err) { \
	++r.fail; \
	fprintf(stderr,"%s: test failed: '%s'\nerror: %s\n",__WHERE__,#exp,cbf_strerror(err)); \
} else ++r.pass; \
} while (0)

/*
Test the CBF function 'exp', which should return a CBFlib error type.

Require that the function DOESN'T return 'CBF_SUCCESS', otherwise
report the failure was to the 'testResult_t' variable 'r' and print a
message to say where and what the failure was to 'stderr'.
*/
#define TEST_CBF_FAIL(exp) \
do { \
const int err = (exp); \
if (CBF_SUCCESS == err) { \
	++r.fail; \
	fprintf(stderr,"%s: test failed: '%s' didn't fail\n",__WHERE__,#exp); \
} else ++r.pass; \
} while (0)

/*
More CBFlib error codes could be tested, but are not nearly as widely used as
the 'CBF_SUCCESS' vs anything other than 'CBF_SUCCESS' convension. A boolean
test is largely satisfactory for the vast majority of those situations.
*/

#endif
