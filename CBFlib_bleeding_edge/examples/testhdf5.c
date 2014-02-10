/**********************************************************************
 *                                                                    *
 * Unit tests for the HDF5 abstraction layer, to ensure it works as   *
 * documented and protect against regressions.                        *
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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "cbf.h"
#include "cbf_hdf5.h"
#include "unittest.h"

#ifndef UINT64_MAX
#define NO_UINT64_TYPE
#endif

/*
 Call a function unconditionally with the expectation that it should work.
 If it fails, print the location & error message, add the error to the pre-defined
 'error' variable and increment the number of failures.
 */
#define CBF_CALL(func) \
do { \
const int err = (func); \
if (CBF_SUCCESS!=err) { \
error |= err; \
fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(err)); \
} \
} while (0)

/*
 NOTE: this only works with exactly representable 'sentinel' values, do not use with arbitrary numbers
 */
#ifdef CBF_USE_ULP
#define CBFM_cmp_dbl_exact(exp,ext,len,prm) cmp_dbl_exact(exp,ext,len,prm)
int cmp_dbl_exact(const void * expected, const void * existing, size_t length, const void * const params)
#else
#define CBFM_cmp_dbl_exact(exp,ext,len,prm) cmp_dbl_exact(exp,ext,len)
int cmp_dbl_exact(const void * expected, const void * existing, size_t length)
#endif
{
	const double * A = (const double*)expected;
	const double * B = (const double*)existing;
	while (length && *A++ == *B++) --length;
	return length;
}

#ifdef CBF_USE_ULP
#define CBFM_cmp_int_exact(exp,ext,len,prm) cmp_int_exact(exp,ext,len,prm)
int cmp_int_exact(const void * expected, const void * existing, size_t length, const void * const params)
#else
#define CBFM_cmp_int_exact(exp,ext,len,prm) cmp_int_exact(exp,ext,len)
int cmp_int_exact(const void * expected, const void * existing, size_t length)
#endif
{
	const int * A = (const int*)expected;
	const int * B = (const int*)existing;
	while (length && *A++ == *B++) --length;
	return length;
}

#ifdef CBF_USE_ULP
#define CBFM_cmp_hsize_t_exact(exp,ext,len,prm) cmp_hsize_t_exact(exp,ext,len,prm)
int cmp_hsize_t_exact(const void * expected, const void * existing, size_t length, const void * const params)
#else
#define CBFM_cmp_hsize_t_exact(exp,ext,len,prm) cmp_hsize_t_exact(exp,ext,len)
int cmp_hsize_t_exact(const void * expected, const void * existing, size_t length)
#endif
{
	const hsize_t * A = (const hsize_t*)expected;
	const hsize_t * B = (const hsize_t*)existing;
	while (length && *A++ == *B++) --length;
	return length;
}

testResult_t test_H5Acreate
		(const hid_t obj,
		hid_t * const attr,
		const char * const name,
		const hid_t type,
		const hid_t space)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};

	/* check parameters */
	if (!cbf_H5Ivalid(obj)
			 || !attr
			 || !name
			 || H5I_DATATYPE!=H5Iget_type(type)
			 || !cbf_H5Ivalid(space))
		error |= CBF_ARGUMENT;

	/* check preconditions */
	if (CBF_SUCCESS==error && !r.fail) {
		/* no attribute should exist with the given name */
		const htri_t exists = H5Aexists(obj,name);
		if (exists<0) {
			error |= CBF_H5ERROR;
		} else if (exists) {
			error |= CBF_UNDEFINED;
}
	}

	if (CBF_SUCCESS==error && !r.fail) {
		/* verify expected failures */
		if (!r.fail) {
			hid_t attribute = CBF_H5FAIL;
			TEST_CBF_FAIL(cbf_H5Acreate(CBF_H5FAIL, &attribute, name, type, space));
			TEST_CBF_FAIL(cbf_H5Acreate(obj, 0, name, type, space));
			TEST_CBF_FAIL(cbf_H5Acreate(obj, &attribute, 0, type, space));
			TEST_CBF_FAIL(cbf_H5Acreate(obj, &attribute, name, CBF_H5FAIL, space));
			TEST_CBF_FAIL(cbf_H5Acreate(obj, &attribute, name, type, CBF_H5FAIL));
			/* check that attr is unchanged */
			TEST(CBF_H5FAIL==attribute);
		}

		if (!r.fail) {
			/* check I can create an attribute */
			TEST_CBF_PASS(cbf_H5Acreate(obj, attr, name, type, space));
			TEST(cbf_H5Ivalid(*attr));
		}

		if (!r.fail) {
			/* check I can't create the same attribute and that the argument is unchanged */
			hid_t attribute = CBF_H5FAIL;
			TEST_CBF_FAIL(cbf_H5Acreate(obj, &attribute, name, type, space));
			TEST(CBF_H5FAIL==attribute);
		}
	} else {
		fprintf(stderr,"%s: Skipping attribute/create tests\n",__WHERE__);
		++r.skip;
	}

	return r;
}

testResult_t test_H5Afree
		(const hid_t attr)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};

	/* preconditions/check arguments */
	if (!cbf_H5Ivalid(attr)) error |= CBF_ARGUMENT;

	if (CBF_SUCCESS==error && !r.fail) {
		/* verify expected failures */
		TEST_CBF_FAIL(cbf_H5Afree(CBF_H5FAIL));

		/* check the attribute can be free'd */
		TEST_CBF_PASS(cbf_H5Afree(attr));

		/* check the attribute can't be free'd again */
		TEST_CBF_FAIL(cbf_H5Afree(attr));
	} else {
		fprintf(stderr,"%s: Skipping attribute/free tests\n",__WHERE__);
		++r.skip;
	}

	return r;
}

testResult_t test_H5Afind
		(const hid_t obj,
		 const char * const validAttr,
		 const char * const invalidAttr,
		 const hid_t type,
		 const hid_t wrongType,
		 const hid_t space)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};

	/* check parameters */
	if (!cbf_H5Ivalid(obj)
			|| !validAttr
			|| !invalidAttr
			|| H5I_DATATYPE!=H5Iget_type(type)
			|| H5I_DATATYPE!=H5Iget_type(wrongType)
			|| !cbf_H5Ivalid(space))
		error |= CBF_ARGUMENT;

	/* check preconditions */
	if (CBF_SUCCESS==error && !r.fail) {
		const htri_t exists_valid = H5Aexists(obj,validAttr);
		const htri_t exists_invalid = H5Aexists(obj,invalidAttr);
		/* the 'valid' attribute should exist */
		if (exists_valid<0) error |= CBF_H5ERROR;
		else if (!exists_valid) error |= CBF_UNDEFINED;
		/* the 'invalid' attribute shouldn't exist */
		if (exists_invalid<0) error |= CBF_H5ERROR;
		else if (exists_invalid) error |= CBF_UNDEFINED;
	}

	if (CBF_SUCCESS==error && !r.fail) {
		/* verify expected failures */
		if (!r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_CBF_FAIL(cbf_H5Afind(CBF_H5FAIL, &attr, validAttr, type, space));
			TEST_CBF_FAIL(cbf_H5Afind(obj, 0, validAttr, type, space));
			TEST_CBF_FAIL(cbf_H5Afind(obj, &attr, 0, type, space));
			/* check that attr is unchanged */
			TEST(CBF_H5FAIL==attr);
		}

		/* test failure to find a non-existant attribute */
		if (!r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_CBF_NOTFOUND(cbf_H5Afind(obj, &attr, invalidAttr, type, space));
			/* check that attr is unchanged */
			TEST(CBF_H5FAIL==attr);
		}

		/* test failure to find an existing attribute with a different type */
		if (!r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_CBF_FAIL(cbf_H5Afind(obj, &attr, validAttr, wrongType, space));
			/* check that attr is unchanged */
			TEST(CBF_H5FAIL==attr);
		}

		/* TODO: tests for different dataspaces */

		/* test successfully finding an attribute that does exist & has known type */
		if (!r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_CBF_PASS(cbf_H5Afind(obj, &attr, validAttr, type, space));
			/* check that attr is valid */
			TEST(cbf_H5Ivalid(attr));
			/* clean up */
			cbf_H5Afree(attr);
		}

		/* test successfully finding an attribute that does exist but has unknown type */
		if (!r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_CBF_PASS(cbf_H5Afind(obj, &attr, validAttr, CBF_H5FAIL, space));
			/* check that attr is valid */
			TEST(cbf_H5Ivalid(attr));
			/* clean up */
			cbf_H5Afree(attr);
		}

		/* test successfully finding an attribute that does exist but has unknown space */
		if (!r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_CBF_PASS(cbf_H5Afind(obj, &attr, validAttr, type, CBF_H5FAIL));
			/* check that attr is valid */
			TEST(cbf_H5Ivalid(attr));
			/* clean up */
			cbf_H5Afree(attr);
		}
	} else {
		fprintf(stderr,"%s: Skipping attribute/find tests\n",__WHERE__);
		++r.skip;
	}

	return r;
}

testResult_t _test_H5Arequire_cmp
(const hid_t obj,
 const char * const attrName,
 const int rank,
 const hsize_t * const dim,
 const hsize_t * const dim_h,
 const hsize_t * const dim_l,
 const hid_t ftype,
 const hid_t ftype_b,
 const hid_t mtype,
 const void * const attrValue,
 const void * const attrValue_b,
 void * const buf,
 int cmp(const void *, const void *, size_t
#ifdef CBF_USE_ULP
         , const void * const
#endif
 ),const void * const cmp_params)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	if (rank<0) {
		fprintf(stderr,"%s: Error: test function called incorrectly\n",__WHERE__);
		++r.fail;
		return r;
	}
    
    
	/* test creation of the attributes */
	TEST_CBF_PASS(CBFM_H5Arequire_cmp2(obj,attrName,rank,dim,ftype,mtype,attrValue,buf,cmp,cmp_params));
	{ /* check that it actually exists */
		hid_t attr = CBF_H5FAIL;
		TEST(H5Aexists(obj,attrName)>0);
		attr = H5Aopen(obj,attrName,H5P_DEFAULT);
		TEST(cbf_H5Ivalid(attr));
		{ /* verify type */
			const hid_t type = H5Aget_type(attr);
			TEST(H5Tequal(type,ftype)>0);
			H5Tclose(type);
		}
		{ /* verify dataspace */
			const hid_t expected = rank>0 ? H5Screate_simple(rank,dim,dim) : H5Screate(H5S_SCALAR);
			const hid_t existing = H5Aget_space(attr);
			TEST(H5Sextent_equal(expected,existing)>0);
			H5Sclose(existing);
			H5Sclose(expected);
		}
		H5Aclose(attr);
	}
	/* test verification of existing attributes */
	TEST_CBF_PASS(CBFM_H5Arequire_cmp2(obj,attrName,rank,dim,ftype,mtype,attrValue,buf,cmp,cmp_params));
	if (rank>0) { /* test failure to verify differing attribute sizes */
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,rank,dim_h,ftype,mtype,attrValue,buf,cmp,cmp_params));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,rank,dim_l,ftype,mtype,attrValue,buf,cmp,cmp_params));
	}
	/* test failure to verify differing attribute types */
	TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,rank,dim,ftype_b,mtype,attrValue,buf,cmp,cmp_params));
	/* test failure to verify differing rank */
	TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,rank+1,dim,ftype,mtype,attrValue,buf,cmp,cmp_params));
	/* test failure to verify differing attribute values */
	TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,rank,dim,ftype,mtype,attrValue_b,buf,cmp,cmp_params));
	return r;
}

testResult_t test_H5Arequire_cmp(const hid_t obj)
{
	testResult_t r = {0,0,0};
	/*
     Test the attribute API on hdf5 object 'obj'.
     */
    
	{ /* verify expected failures */
		const char attrName[] = "fail";
		int attrValue[] = {0,1,2,3};
		int buf[4];
		const hsize_t dim[] = {4};
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(CBF_H5FAIL,attrName,1,dim,H5T_STD_I32LE,H5T_NATIVE_INT,attrValue,buf,cmp_int_exact,0));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,0,1,dim,H5T_STD_I32LE,H5T_NATIVE_INT,attrValue,buf,cmp_int_exact,0));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,-1,dim,H5T_STD_I32LE,H5T_NATIVE_INT,attrValue,buf,cmp_int_exact,0));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,1,0,H5T_STD_I32LE,H5T_NATIVE_INT,attrValue,buf,cmp_int_exact,0));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,1,dim,CBF_H5FAIL,H5T_NATIVE_INT,attrValue,buf,cmp_int_exact,0));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,1,dim,H5T_STD_I32LE,CBF_H5FAIL,attrValue,buf,cmp_int_exact,0));
		TEST_CBF_FAIL(CBFM_H5Arequire_cmp2(obj,attrName,1,dim,H5T_STD_I32LE,H5T_NATIVE_INT,0,buf,cmp_int_exact,0));
	}
    
	{ /* rank 0 */
		int attrValue[] = {42};
		int attrValue_b[] = {43};
		int buf[1];
		TEST_COMPONENT(_test_H5Arequire_cmp(obj,"i0",0,0,0,0,H5T_STD_I32LE,H5T_STD_U32LE,H5T_NATIVE_INT,attrValue,attrValue_b,0,cmp_int_exact,0));
		TEST_COMPONENT(_test_H5Arequire_cmp(obj,"i0_b",0,0,0,0,H5T_STD_I32LE,H5T_STD_U32LE,H5T_NATIVE_INT,attrValue,attrValue_b,buf,cmp_int_exact,0));
	}
	{ /* rank 1 */
		const hsize_t dim_l[] = {3};
		const hsize_t dim[] = {4};
		const hsize_t dim_h[] = {5};
		int attrValue[] = {0,1,2,3};
		int attrValue_b[] = {0,1,2,4};
		int buf[4];
		TEST_COMPONENT(_test_H5Arequire_cmp(obj,"i1",1,dim,dim_h,dim_l,H5T_STD_I32LE,H5T_STD_U32LE,H5T_NATIVE_INT,attrValue,attrValue_b,0,cmp_int_exact,0));
		TEST_COMPONENT(_test_H5Arequire_cmp(obj,"i1_b",1,dim,dim_h,dim_l,H5T_STD_I32LE,H5T_STD_U32LE,H5T_NATIVE_INT,attrValue,attrValue_b,buf,cmp_int_exact,0));
	}
	{ /* rank 2 */
		const hsize_t dim_l[] = {3,3};
		const hsize_t dim[] = {3,4};
		const hsize_t dim_h[] = {3,5};
		int attrValue[][4] = {
			{0,1,2,3},
			{4,5,6,7},
			{8,9,10,11}
		};
		int attrValue_b[][4] = {
			{0,1,2,3},
			{4,5,6,7},
			{8,9,10,12}
		};
		int buf[12];
		TEST_COMPONENT(_test_H5Arequire_cmp(obj,"i2",2,dim,dim_h,dim_l,H5T_STD_I32LE,H5T_STD_U32LE,H5T_NATIVE_INT,attrValue,attrValue_b,0,cmp_int_exact,0));
		TEST_COMPONENT(_test_H5Arequire_cmp(obj,"i2_b",2,dim,dim_h,dim_l,H5T_STD_I32LE,H5T_STD_U32LE,H5T_NATIVE_INT,attrValue,attrValue_b,buf,cmp_int_exact,0));
	}
	return r;
}

testResult_t test_H5Arequire_string(const hid_t obj)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	/*
     Test the attribute API on hdf5 object 'obj'.
     */
	const char attrName[] = "attr_r0_str";
	const char attrValue[] = "value";
	const char attrValue_b[] = "Value";
    
	/* verify expected failures */
	TEST_CBF_FAIL(cbf_H5Arequire_string(CBF_H5FAIL,attrName,attrValue));
	TEST_CBF_FAIL(cbf_H5Arequire_string(obj,0,attrValue));
	TEST_CBF_FAIL(cbf_H5Arequire_string(obj,attrName,0));
    
	/* test creation of the attributes */
	TEST_CBF_PASS(cbf_H5Arequire_string(obj,attrName,attrValue));
    
	{/* independently verify existance */
		hid_t attr = CBF_H5FAIL;
		TEST(H5Aexists(obj,attrName)>0);
		attr = H5Aopen(obj,attrName,H5P_DEFAULT);
		{ /* verify dataspace */
			const hid_t expected = H5Screate(H5S_SCALAR);
			const hid_t existing = H5Aget_space(attr);
			TEST(H5Sextent_equal(expected,existing)>0);
			H5Sclose(existing);
			H5Sclose(expected);
		}
		{ /* verify type */
			const hid_t type = H5Aget_type(attr);
			TEST(H5T_STRING == H5Tget_class(type));
			H5Tclose(type);
		}
		H5Aclose(attr);
	}
    
	/* test verification of existing attributes */
	TEST_CBF_PASS(cbf_H5Arequire_string(obj,attrName,attrValue));
    
	/* test failure to verify differing attribute values */
	TEST_CBF_FAIL(cbf_H5Arequire_string(obj,attrName,attrValue_b));
	return r;
}

testResult_t test_attribute
		(const hid_t obj)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};

	/* preconditions/check arguments */
	if (!cbf_H5Ivalid(obj)) error |= CBF_ARGUMENT;

	if (CBF_SUCCESS==error && !r.fail) {
		const char validName[] = "attr";
		const char invalidName[] = "attr_invalid";
		const hid_t space = H5Screate(H5S_SCALAR);
		if (space < 0) error |= CBF_H5ERROR;

		/* test create & free functions together */
		if (CBF_SUCCESS==error && !r.fail) {
			hid_t attr = CBF_H5FAIL;
			TEST_COMPONENT(test_H5Acreate(obj, &attr, validName, H5T_STD_I32LE, space));
			TEST_COMPONENT(test_H5Afree(attr));
		}

		/* test some other functions */
		TEST_COMPONENT(test_H5Afind(obj, validName, invalidName, H5T_STD_I32LE, H5T_STD_U64BE, space));
		TEST_COMPONENT(test_H5Arequire_cmp(obj));
		TEST_COMPONENT(test_H5Arequire_string(obj));

		H5Sclose(space);
	} else {
		fprintf(stderr,"%s: Skipping attribute tests\n",__WHERE__);
		++r.skip;
	}

	return r;
}

testResult_t testDatasetFind(const hid_t grp, hsize_t * const buf)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	/*
     Test the dataset find function on hdf5 group 'grp', with or without a buffer being set.
     */
	const int rank = 2;
	const hsize_t dim[] = {0,0};
	const hsize_t max[] = {H5S_UNLIMITED,16};
	const hsize_t chunk[] = {1,16};
	const char name00[] = "dataset_find_nobuf";
	const char name01[] = "dataset_find_buf";
	const char name10[] = "Dataset_find_nobuf";
	const char name11[] = "Dataset_find_buf";
	const char * const name1 = 0==buf ? name00 : name01;
	const char * const name2 = 0==buf ? name10 : name11;
	hid_t validHandle = CBF_H5FAIL;
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	/* ensure a suitable test environment exists */
	if (CBF_SUCCESS==error) {
		if (0!=H5Lexists(grp,name1,H5P_DEFAULT)) error |= CBF_H5ERROR;
		CBF_CALL(cbf_H5Dcreate(grp,&validHandle,name1,rank,dim,max,chunk,H5T_NATIVE_INT));
		if (!cbf_H5Ivalid(validHandle)) error |= CBF_H5ERROR;
	}
    
	/* do the tests? */
	if (CBF_SUCCESS==error && !r.fail) {
		/* verify expected failures */
		hid_t handle = CBF_H5FAIL;
		TEST_CBF_FAIL(cbf_H5Dfind2(0,&handle,name1,rank,max,buf,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dfind2(grp,0,name1,rank,max,buf,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dfind2(grp,&handle,0,rank,max,buf,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dfind2(grp,&handle,name1,-1,max,buf,H5T_NATIVE_INT));
		{ /* check it can find a match */
			hid_t handle = CBF_H5FAIL;
			TEST_CBF_PASS(cbf_H5Dfind2(grp,&handle,name1,rank,max,buf,H5T_NATIVE_INT));
			TEST(cbf_H5Ivalid(handle));
			cbf_H5Dfree(handle);
		}
		{ /* check case where match not found (by name), but dataset can be created */
			hid_t handle = CBF_H5FAIL;
			const hid_t tmpHandle = handle;
			TEST_CBF_NOTFOUND(cbf_H5Dfind2(grp,&handle,name2,rank,max,buf,H5T_NATIVE_INT));
			TEST(!cbf_H5Ivalid(handle));
			TEST(tmpHandle==handle);
			cbf_H5Dfree(handle);
		}
		{ /* test failure when type differs */
			hid_t handle = CBF_H5FAIL;
			const hid_t tmpHandle = handle;
			TEST_CBF_FAIL(cbf_H5Dfind2(grp,&handle,name1,rank,max,buf,H5T_NATIVE_UINT));
			TEST(!cbf_H5Ivalid(handle));
			TEST(tmpHandle==handle);
			cbf_H5Dfree(handle);
		}
		{ /* test no failure when no type given */
			hid_t handle = CBF_H5FAIL;
			TEST_CBF_PASS(cbf_H5Dfind2(grp,&handle,name1,rank,max,buf,CBF_H5FAIL));
			TEST(cbf_H5Ivalid(handle));
			cbf_H5Dfree(handle);
		}
		{ /* test failure when rank differs */
			hid_t handle = CBF_H5FAIL;
			const hid_t tmpHandle = handle;
			/*
             NOTE: max & buf lengths are not (and can't be) tested, just assumed to contain 'rank' elements if used.
             Therefore, don't do this test with a higher rank than the number of elements in either of those arrays.
             */
			TEST_CBF_FAIL(cbf_H5Dfind2(grp,&handle,name1,rank-1,max,buf,H5T_NATIVE_INT));
			TEST(!cbf_H5Ivalid(handle));
			TEST(tmpHandle==handle);
			cbf_H5Dfree(handle);
		}
		{ /* test no failure when current max is bigger */
			hid_t handle = CBF_H5FAIL;
			const hsize_t max2[] = {H5S_UNLIMITED,8};
			TEST_CBF_PASS(cbf_H5Dfind2(grp,&handle,name1,rank,max2,buf,H5T_NATIVE_INT));
			TEST(cbf_H5Ivalid(handle));
			cbf_H5Dfree(handle);
		}
		{ /* test failure when current max is smaller */
			hid_t handle = CBF_H5FAIL;
			const hid_t tmpHandle = handle;
			const hsize_t max2[] = {H5S_UNLIMITED,32};
			TEST_CBF_FAIL(cbf_H5Dfind2(grp,&handle,name1,rank,max2,buf,H5T_NATIVE_INT));
			TEST(!cbf_H5Ivalid(handle));
			TEST(tmpHandle==handle);
			cbf_H5Dfree(handle);
		}
	} else {
		fprintf(stderr,"Failed to set up dataset/find tests\n");
		++r.skip;
	}
	cbf_H5Dfree(validHandle);
    
	return r;
}

testResult_t test_H5Dcreate(const hid_t grp, hid_t * const dset, const char * name)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	const int rank = 2;
	const hsize_t dim[] = {0,0};
	const hsize_t max[] = {H5S_UNLIMITED,16};
	const hsize_t chunk[] = {1,16};
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	/* Ensure it fails at the appropriate times */
	TEST_CBF_FAIL(cbf_H5Dcreate(CBF_H5FAIL,dset,name,rank,dim,max,chunk,H5T_NATIVE_INT));
	TEST_CBF_FAIL(cbf_H5Dcreate(grp,dset,0,rank,dim,max,chunk,H5T_NATIVE_INT));
	TEST_CBF_FAIL(cbf_H5Dcreate(grp,dset,name,-1,dim,max,chunk,H5T_NATIVE_INT));
	TEST_CBF_FAIL(cbf_H5Dcreate(grp,dset,name,rank,0,max,chunk,H5T_NATIVE_INT));
	TEST_CBF_FAIL(cbf_H5Dcreate(grp,dset,name,rank,dim,max,0,H5T_NATIVE_INT));
	TEST_CBF_FAIL(cbf_H5Dcreate(grp,dset,name,rank,dim,max,chunk,CBF_H5FAIL));
    
	{ /* pre-conditions: no dataset */
		TEST(!cbf_H5Ivalid(*dset));
		TEST(0==H5Lexists(grp,name,H5P_DEFAULT));
	}
	{ /* creation should work */
		TEST_CBF_PASS(cbf_H5Dcreate(grp,dset,name,rank,dim,max,chunk,H5T_NATIVE_INT));
	}
	{ /* post-conditions from previous block, pre-conditions for next block */
		hid_t dsetObj = CBF_H5FAIL;
		TEST(cbf_H5Ivalid(*dset));
		TEST(H5I_DATASET==H5Iget_type(*dset));
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		TEST(cbf_H5Ivalid(dsetObj=H5Oopen(grp,name,H5P_DEFAULT)));
		TEST(!cbf_H5Ocmp(*dset,dsetObj));
		H5Oclose(dsetObj);
	}
	{ /* dataset exists: ensure 'create' fails, verify the handle is valid, unmodified & the dataset (link, at least) remains */
		const hid_t tmpHandle = *dset;
		TEST_CBF_FAIL(cbf_H5Dcreate(grp,dset,name,rank,dim,max,chunk,H5T_NATIVE_INT));
		TEST(tmpHandle == *dset);
	}
	{ /* post-conditions from previous block */
		hid_t dsetObj = CBF_H5FAIL;
		TEST(cbf_H5Ivalid(*dset));
		TEST(H5I_DATASET==H5Iget_type(*dset));
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		TEST(cbf_H5Ivalid(dsetObj=H5Oopen(grp,name,H5P_DEFAULT)));
		TEST(!cbf_H5Ocmp(*dset,dsetObj));
		H5Oclose(dsetObj);
	}
    
	return r;
}

testResult_t test_H5Dfree(const hid_t grp, const hid_t dset, const char * name)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	TEST(H5I_GROUP==H5Iget_type(grp));
	TEST(H5I_DATASET==H5Iget_type(dset));
    
	{ /* pre-conditions */
		hid_t dsetObj = CBF_H5FAIL;
		TEST(cbf_H5Ivalid(dset));
		TEST(H5I_DATASET==H5Iget_type(dset));
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		TEST(cbf_H5Ivalid(dsetObj=H5Oopen(grp,name,H5P_DEFAULT)));
		TEST(!cbf_H5Ocmp(dset,dsetObj));
		H5Oclose(dsetObj);
	}
	/* free'ing an invalid handle should fail */
	TEST_CBF_FAIL(cbf_H5Dfree(CBF_H5FAIL));
	/* Freeing an existing handle should work */
	TEST_CBF_PASS(cbf_H5Dfree(dset));
	{ /* post-conditions */
		const hid_t tmpHandle = dset;
		TEST(!cbf_H5Ivalid(dset));
		TEST(tmpHandle == dset);
		/* the data must remain in the tree */
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
	}
	/* Freeing a previously free'd handle should fail */
	TEST_CBF_FAIL(cbf_H5Dfree(dset));
    
	return r;
}

testResult_t test_H5Drequire_flstring(const hid_t grp)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	const char name[] = "dataset_require_string";
	const char value[] = "  value...\n\twith leading/trailing spaces & a newline  ";
	const char value_b[] = "  Value...\n\twith leading/trailing spaces & a newline  ";
	hid_t handle = CBF_H5FAIL;
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	/* verify expected failures */
	TEST_CBF_FAIL(cbf_H5Drequire_flstring(CBF_H5FAIL,&handle,name,value));
	TEST_CBF_FAIL(cbf_H5Drequire_flstring(grp,&handle,0,value));
	TEST_CBF_FAIL(cbf_H5Drequire_flstring(grp,&handle,name,0));
    
	/* test creation of the dataset */
	TEST_CBF_PASS(cbf_H5Drequire_flstring(grp,&handle,name,value));
    
	{/* independently verify existance */
		hid_t dset = CBF_H5FAIL;
        
		/* verify existence of link */
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		/* open it and verify object type & data type */
		TEST(cbf_H5Ivalid(dset = H5Oopen(grp,name,H5P_DEFAULT)));
		/* check object type */
		TEST(H5I_DATASET==H5Iget_type(dset));
        
		{ /* verify dataspace */
			hid_t expected = H5Screate(H5S_SCALAR);
			const hid_t existing = H5Dget_space(dset);
			TEST(H5Sextent_equal(expected,existing)>0);
			H5Sclose(existing);
			H5Sclose(expected);
		}
		{ /* verify type class */
			const hid_t type = H5Dget_type(dset);
			TEST(H5T_STRING == H5Tget_class(type));
			TEST(0==H5Tis_variable_str(type));
			H5Tclose(type);
		}
        
		/* close the object */
		H5Oclose(dset);
	}
	cbf_H5Dfree(handle);
    
	/* test verification of existing datasets */
	TEST_CBF_PASS(cbf_H5Drequire_flstring(grp,&handle,name,value));
	/* test failure to verify a different dataset */
	TEST_CBF_FAIL(cbf_H5Drequire_flstring(grp,&handle,name,value_b));
	cbf_H5Dfree(handle);
    
	return r;
}

testResult_t test_H5Drequire_F64LE(const hid_t grp)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	const char name[] = "dataset_require_F64LE";
	hid_t handle = CBF_H5FAIL;
	/* exactly representable 'sentinel' values, to avoid approximation/rounding errors & allow exact comparisons */
	const double value = 42.;
	const double value_b = 1.;
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	/* verify expected failures */
	TEST_CBF_FAIL(CBFM_H5Drequire_scalar_F64LE2(CBF_H5FAIL,&handle,name,value,cmp_dbl_exact,0));
	TEST_CBF_FAIL(CBFM_H5Drequire_scalar_F64LE2(grp,&handle,0,value,cmp_dbl_exact,0));
    
	/* test creation of the dataset */
	TEST_CBF_PASS(CBFM_H5Drequire_scalar_F64LE2(grp,&handle,name,value,cmp_dbl_exact,0));
    
	{/* independently verify existance */
		hid_t dset = CBF_H5FAIL;
        
		/* verify existence of link */
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		/* open it and verify object type & data type */
		TEST(cbf_H5Ivalid(dset = H5Oopen(grp,name,H5P_DEFAULT)));
		/* check object type */
		TEST(H5I_DATASET==H5Iget_type(dset));
        
		{ /* verify dataspace */
			hid_t expected = H5Screate(H5S_SCALAR);
			const hid_t existing = H5Dget_space(dset);
			TEST(H5Sextent_equal(expected,existing)>0);
			H5Sclose(existing);
			H5Sclose(expected);
		}
		{ /* verify type class */
			const hid_t type = H5Dget_type(dset);
			TEST(H5T_FLOAT == H5Tget_class(type));
			H5Tclose(type);
		}
        
		/* close the object */
		H5Oclose(dset);
	}
	cbf_H5Dfree(handle);
    
	/* test verification of existing datasets */
	TEST_CBF_PASS(CBFM_H5Drequire_scalar_F64LE2(grp,&handle,name,value,cmp_dbl_exact,0));
	/* test failure to verify a different dataset */
	TEST_CBF_FAIL(CBFM_H5Drequire_scalar_F64LE2(grp,&handle,name,value_b,cmp_dbl_exact,0));
	cbf_H5Dfree(handle);
    
	return r;
}

testResult_t test_H5Dset_extent(const hid_t grp)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	const int rank = 2;
	const hsize_t dim[] = {0,0};
	const hsize_t dim2[] = {2,16};
	const hsize_t max[] = {H5S_UNLIMITED,16};
	const hsize_t chunk[] = {1,16};
	const char name[] = "dataset_set_extent";
	hid_t handle = CBF_H5FAIL;
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	/* ensure a suitable test environment exists */
	if (0!=H5Lexists(grp,name,H5P_DEFAULT)) error |= CBF_H5ERROR;
	CBF_CALL(cbf_H5Dcreate(grp,&handle,name, rank,dim,max, chunk, H5T_NATIVE_INT));
	if (!cbf_H5Ivalid(handle)) error |= CBF_H5ERROR;
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* verify expected failures */
		TEST_CBF_FAIL(cbf_H5Dset_extent(CBF_H5FAIL,dim2));
		TEST_CBF_FAIL(cbf_H5Dset_extent(handle,0));
        
		{ /* check current extent */
			const hid_t dataspace = H5Dget_space(handle);
			hsize_t currDims[] = {-1,-1};
			hsize_t currMax[] = {0,0};
			TEST(rank==H5Sget_simple_extent_dims(dataspace,0,0));
			TEST(rank==H5Sget_simple_extent_dims(dataspace,currDims,currMax));
			H5Sclose(dataspace);
			TEST(dim[0]==currDims[0]);
			TEST(dim[1]==currDims[1]);
			TEST(max[0]==currMax[0]);
			TEST(max[1]==currMax[1]);
		}
        
		/* change the extent */
		TEST_CBF_PASS(cbf_H5Dset_extent(handle,dim2));
        
		{ /* verify the new extent */
			const hid_t dataspace = H5Dget_space(handle);
			hsize_t currDims[] = {-1,-1};
			hsize_t currMax[] = {0,0};
			TEST(rank==H5Sget_simple_extent_dims(dataspace,0,0));
			TEST(rank==H5Sget_simple_extent_dims(dataspace,currDims,currMax));
			H5Sclose(dataspace);
			TEST(dim2[0]==currDims[0]);
			TEST(dim2[1]==currDims[1]);
			TEST(max[0]==currMax[0]);
			TEST(max[1]==currMax[1]);
		}
	} else {
		++r.skip;
		fprintf(stderr,"%s: Failed to set up dataset/set_extent tests\n",__WHERE__);
	}
	if (CBF_SUCCESS!=error)
		fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(error));
    
	cbf_H5Dfree(handle);
    
	return r;
}

void fill_array(int * array, size_t length, const int fill)
{
	while (length) {
		--length;
		*array++ = fill;
	}
}

testResult_t testDataset_read_write(const hid_t grp)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	const char name0[] = "dataset_read_write_rank0";
	const char name2[] = "dataset_read_write_rank2";
	hid_t handle0 = CBF_H5FAIL;
	hid_t handle2 = CBF_H5FAIL;
	const int N = 8;
	const int N2 = N*2;
	int dataWrite0[] = {42};
	int dataRead0[] = {0};
	int dataWrite2[N2];
	int dataRead2[N2];
	const int rank = 2;
	const hsize_t offset[] = {0,0};
	const hsize_t offset_1[] = {1,0};
	const hsize_t stride[] = {1,1};
	const hsize_t stride_2[] = {1,2};
	const hsize_t count[] = {1,N2};
	const hsize_t count_2[] = {1,N};
	const hsize_t dim[] = {2,N2};
	const hsize_t max[] = {H5S_UNLIMITED,N2};
	const hsize_t chunk[] = {1,N2};
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	{/* populate the data */
		int i;
		for (i = 0; i < N2; ++i) dataWrite2[i] = i*i;
	}
    
	/* ensure a suitable test environment exists */
	CBF_CALL(cbf_H5Dcreate(grp,&handle0,name0, 0,0,0, 0, H5T_NATIVE_INT));
	if (!cbf_H5Ivalid(handle0)) error |= CBF_H5ERROR;
	CBF_CALL(cbf_H5Dcreate(grp,&handle2,name2, rank,dim,max, chunk, H5T_NATIVE_INT));
	if (!cbf_H5Ivalid(handle2)) error |= CBF_H5ERROR;
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* verify expected failures */
		TEST_CBF_FAIL(cbf_H5Dwrite2(CBF_H5FAIL,offset,stride,count,dataWrite2,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dwrite2(handle2,0,stride,count,dataWrite2,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dwrite2(handle2,offset,stride,0,dataWrite2,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dwrite2(handle2,offset,stride,count,0,H5T_NATIVE_INT));
		TEST_CBF_FAIL(cbf_H5Dwrite2(handle2,offset,stride,count,dataWrite2,CBF_H5FAIL));
        
		{ /* rank 0 */
			/* check read/write with null pointers */
			TEST_CBF_PASS(cbf_H5Dwrite2(handle0,0,0,0,dataWrite0,H5T_NATIVE_INT));
			TEST_CBF_PASS(cbf_H5Dread2(handle0,0,0,0,dataRead0,H5T_NATIVE_INT));
			TEST(*dataWrite0 == *dataRead0);
		}
        
		/* rank 2: need to check independent changes to offset/stride/count, and allow for null stride */
        
		{ /* Using default ({1,1,...,1}) strides, 4 permutations of (read,write)^(NULL,{1,1,...,1}) */
			/* verify writing with {1,1,...,1} stride */
			TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset,stride,count,dataWrite2,H5T_NATIVE_INT));
            
			/* check I can read the data with the default stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,stride,count,dataRead2,H5T_NATIVE_INT));
			TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N2,0));
            
			/* check I can read the data with the (equivalent) null stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,0,count,dataRead2,H5T_NATIVE_INT));
			TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N2,0));
            
			/* verify writing with null stride */
			TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset,0,count,dataWrite2,H5T_NATIVE_INT));
            
			/* check I can read the data with the (equivalent) default stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,stride,count,dataRead2,H5T_NATIVE_INT));
			TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N2,0));
            
			/* check I can read the data with the null stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,0,count,dataRead2,H5T_NATIVE_INT));
			TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N2,0));
		}
        
		{ /* verify writing with an offset */
			TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset_1,stride,count,dataWrite2,H5T_NATIVE_INT));
			/* check I can read the data with the default stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset_1,stride,count,dataRead2,H5T_NATIVE_INT));
			TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N2,0));
			/* check I can read the data with the (equivalent) null stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset_1,0,count,dataRead2,H5T_NATIVE_INT));
			TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N2,0));
		}
        
		{ /* verify writing with a non-trivial stride, which uses but doesn't verify that a different count works */
			{ /* first set a reasonable 'background' value - later tests will ensure it worked */
				int background[16];
				fill_array(background,N2,-1);
				TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset,0,count,background,H5T_NATIVE_INT));
			}
            
			/* change settings & do the real write */
			TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset,stride_2,count_2,dataWrite2,H5T_NATIVE_INT));
			/* check I can read the data with the same stride */
			fill_array(dataRead2,N2,-1);
			TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,stride_2,count_2,dataRead2,H5T_NATIVE_INT));
            TEST(!CBFM_cmp_int_exact(dataWrite2,dataRead2,N,0));
			{ /* check the data was embedded correctly */
				const int result[16] = {
					0,-1,1,-1,4,-1,9,-1,16,-1,25,-1,36,-1,49,-1
				};
				fill_array(dataRead2,N2,-1);
				TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,stride,count,dataRead2,H5T_NATIVE_INT));
				TEST(!CBFM_cmp_int_exact(result,dataRead2,N2,0));
			}
		}
        
		{ /* verify writing with a different count */
			{ /* first set a reasonable 'background' value - later tests will ensure it worked */
				int background[16];
				fill_array(background,N2,-2);
				TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset,0,count,background,H5T_NATIVE_INT));
			}
			/* do the real write */
			TEST_CBF_PASS(cbf_H5Dwrite2(handle2,offset,stride,count_2,dataWrite2,H5T_NATIVE_INT));
			{ /* check the results... */
				const int result[16] = {
					0,1,4,9,16,25,36,49,-2,-2,-2,-2,-2,-2,-2,-2
				};
				/* ...with the default - null - stride */
				fill_array(dataRead2,N2,-1);
				TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,0,count,dataRead2,H5T_NATIVE_INT));
				TEST(!CBFM_cmp_int_exact(result,dataRead2,N2,0));
				/* ...with the equivalent - non-null - stride */
				fill_array(dataRead2,N2,-1);
				TEST_CBF_PASS(cbf_H5Dread2(handle2,offset,stride,count,dataRead2,H5T_NATIVE_INT));
				TEST(!CBFM_cmp_int_exact(result,dataRead2,N2,0));
			}
		}
	} else {
		++r.skip;
		fprintf(stderr,"%s: Failed to set up dataset/read-write tests\n",__WHERE__);
	}
	if (CBF_SUCCESS!=error)
		fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(error));
    
	/* free the handle after using it */
	cbf_H5Dfree(handle0);
	cbf_H5Dfree(handle2);
    
	return r;
}

/* wrapper for common tests, expected failures checked in calling function */
testResult_t test_H5Drequire
(const hid_t grp,
 const char * const name,
 const int rank,
 const hsize_t * const max,
 const hsize_t * const cnk,
 hsize_t * const buf,
 const hid_t type)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	hid_t dset = CBF_H5FAIL;
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	{ /* pre-conditions: no dataset */
		TEST(!cbf_H5Ivalid(dset));
		TEST(0==H5Lexists(grp,name,H5P_DEFAULT));
	}
	/* check it works *without* a previously existing dataset */
	TEST_CBF_PASS(cbf_H5Drequire(grp,&dset,name,rank,max,cnk,buf,type));
	{ /* post-conditions from previous test, pre-conditions for next test */
		hid_t dsetObj = CBF_H5FAIL;
		TEST(cbf_H5Ivalid(dset));
		TEST(H5I_DATASET==H5Iget_type(dset));
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		TEST(cbf_H5Ivalid(dsetObj=H5Oopen(grp,name,H5P_DEFAULT)));
		TEST(!cbf_H5Ocmp(dset,dsetObj));
		H5Oclose(dsetObj);
	}
	/* check it works *with* a previously existing dataset */
	TEST_CBF_PASS(cbf_H5Drequire(grp,&dset,name,rank,max,cnk,buf,H5T_STD_I32LE));
	{ /* post-conditions from previous test */
		hid_t dsetObj = CBF_H5FAIL;
		TEST(cbf_H5Ivalid(dset));
		TEST(H5I_DATASET==H5Iget_type(dset));
		TEST(H5Lexists(grp,name,H5P_DEFAULT)>0);
		TEST(cbf_H5Ivalid(dsetObj=H5Oopen(grp,name,H5P_DEFAULT)));
		TEST(!cbf_H5Ocmp(dset,dsetObj));
		H5Oclose(dsetObj);
	}
    
	cbf_H5Dfree(dset);
    
	return r;
}

/* wrapper for common tests, expected failures checked in calling function */
testResult_t test_H5Dinsert
(const hid_t dset,
 const hsize_t * const off,
 const hsize_t * const std,
 const hsize_t * const cnt,
 hsize_t * const buf, /* length = rank */
 const void * const val,
 void * const valBuf, /* length = nElems */
 const hid_t type, /* type of val & valBuf = H5T_NATIVE_SOMETHING */
 int (*cmp)(const void * const, const void * const, size_t
#ifdef CBF_USE_ULP
 ,const void * const
#endif
 ),
 const size_t length,
 const void * const cmp_params)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	TEST(H5I_DATASET==H5Iget_type(dset));
    
	/* pre-conditions */
	TEST(cbf_H5Ivalid(dset));
	/* call the function */
	TEST_CBF_PASS(cbf_H5Dinsert(dset,off,std,cnt,buf,val,type));
	{/* post-conditions */
		CBF_CALL(cbf_H5Dread2(dset,off,std,cnt,valBuf,type));
		if (CBF_SUCCESS==error) TEST(!cmp(val,valBuf,length
#ifdef CBF_USE_ULP
                                          ,cmp_params
#endif
                                          ));
	}
    
	return r;
}

testResult_t testDatasets(const hid_t grp)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	/*
     Test the dataset API on hdf5 group 'grp'.
     */
    
	TEST(H5I_GROUP==H5Iget_type(grp));
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* create & free functions */
		const char name[] = "dataset_create_free";
		hid_t handle = CBF_H5FAIL;
		TEST_COMPONENT(test_H5Dcreate(grp,&handle,name));
		TEST_COMPONENT(test_H5Dfree(grp,handle,name));
		cbf_H5Dfree(handle);
	} else {
		fprintf(stderr,"%s: Skipping dataset/create&free tests\n",__WHERE__);
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* find function */
		hsize_t buf[] = {0,0};
		TEST_COMPONENT(testDatasetFind(grp,NULL));
		TEST_COMPONENT(testDatasetFind(grp,buf));
	} else {
		fprintf(stderr,"%s: Skipping dataset/find tests\n",__WHERE__);
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		TEST_COMPONENT(test_H5Drequire_flstring(grp));
	} else {
		fprintf(stderr,"%s: Skipping dataset/require_flstring tests\n",__WHERE__);
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		TEST_COMPONENT(test_H5Drequire_F64LE(grp));
	} else {
		fprintf(stderr,"%s: Skipping dataset/require_F64LE tests\n",__WHERE__);
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		TEST_COMPONENT(test_H5Dset_extent(grp));
	} else {
		fprintf(stderr,"Skipping dataset/set_extent tests\n");
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		TEST_COMPONENT(testDataset_read_write(grp));
	} else {
		fprintf(stderr,"Skipping dataset/read&write tests\n");
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		hid_t dset = CBF_H5FAIL;
		const hsize_t max[] = {H5S_UNLIMITED,2};
		const hsize_t cnk[] = {2,2};
		hsize_t buf[] = {0,0};
		/* verify expected failures */
		TEST_CBF_FAIL(cbf_H5Drequire(CBF_H5FAIL,&dset,"name",2,max,cnk,buf,H5T_STD_I32LE));
		TEST_CBF_FAIL(cbf_H5Drequire(grp,&dset,0,2,max,cnk,buf,H5T_STD_I32LE));
		TEST_CBF_FAIL(cbf_H5Drequire(grp,&dset,"name",-1,max,cnk,buf,H5T_STD_I32LE));
		TEST_CBF_FAIL(cbf_H5Drequire(grp,&dset,"name",2,max,0,buf,H5T_STD_I32LE));
		TEST_CBF_FAIL(cbf_H5Drequire(grp,&dset,"name",2,max,cnk,buf,CBF_H5FAIL));
		/* check if things work */
		TEST_CBF_PASS(cbf_H5Drequire(grp,0,"dataset_require_0",2,max,cnk,buf,H5T_STD_I32LE));
		TEST_COMPONENT(test_H5Drequire(grp,"dataset_require_1",2,max,cnk,buf,H5T_STD_I32LE));
		TEST_COMPONENT(test_H5Drequire(grp,"dataset_require_2",2,max,cnk,0,H5T_STD_I32LE));
		TEST_COMPONENT(test_H5Drequire(grp,"dataset_require_3",0,0,cnk,buf,H5T_STD_I32LE));
		TEST_COMPONENT(test_H5Drequire(grp,"dataset_require_4",0,max,0,buf,H5T_STD_I32LE));
		TEST_COMPONENT(test_H5Drequire(grp,"dataset_require_5",0,max,cnk,0,H5T_STD_I32LE));
		cbf_H5Dfree(dset);
	} else {
		fprintf(stderr,"Skipping dataset/require tests\n");
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		hid_t dset = CBF_H5FAIL;
		const hsize_t dim[] = {0,2};
		const hsize_t max[] = {H5S_UNLIMITED,2};
		const hsize_t cnk[] = {2,2};
		const hsize_t off0[] = {0,0};
		const hsize_t off1[] = {1,0};
		const hsize_t off2[] = {2,0};
		const hsize_t std[] = {1,1};
		const hsize_t cnt[] = {1,2};
		hsize_t buf[] = {0,0};
		const int val[2] = {1,2};
		CBF_CALL(cbf_H5Dcreate(grp,&dset,"dataset_insert",2,dim,max,cnk,H5T_STD_I32LE));
		if (CBF_SUCCESS == error) {
			int valBuf[2];
			/* verify expected failures */
			TEST_CBF_FAIL(cbf_H5Dinsert(CBF_H5FAIL,off0,std,cnt,buf,val,H5T_NATIVE_INT));
			TEST_CBF_FAIL(cbf_H5Dinsert(dset,0,std,cnt,buf,val,H5T_NATIVE_INT));
			TEST_CBF_FAIL(cbf_H5Dinsert(dset,off0,std,0,buf,val,H5T_NATIVE_INT));
			TEST_CBF_FAIL(cbf_H5Dinsert(dset,off0,std,cnt,buf,0,H5T_NATIVE_INT));
			TEST_CBF_FAIL(cbf_H5Dinsert(dset,off0,std,cnt,buf,val,CBF_H5FAIL));
			/* check if things work */
			TEST_COMPONENT(test_H5Dinsert(dset,off0,std,cnt,buf,val,valBuf,H5T_NATIVE_INT,cmp_int_exact,2,NULL));
			TEST_COMPONENT(test_H5Dinsert(dset,off1,0,cnt,buf,val,valBuf,H5T_NATIVE_INT,cmp_int_exact,2,NULL));
			TEST_COMPONENT(test_H5Dinsert(dset,off2,std,cnt,0,val,valBuf,H5T_NATIVE_INT,cmp_int_exact,2,NULL));
		}
		cbf_H5Dfree(dset);
	} else {
		fprintf(stderr,"Skipping dataset/insert tests\n");
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* test some of the attribute management API */
		hid_t handle = CBF_H5FAIL;
		CBF_CALL(cbf_H5Dcreate(grp,&handle,"dataset_attributes", 0,0,0, 0, H5T_STD_I32LE));
		/* check for errors */
		if (CBF_SUCCESS != error) {
			++r.fail;
			++r.skip;
			fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(error));
			return r;
		}
		TEST_COMPONENT(test_attribute(handle));
		/* free the handle after using it */
		cbf_H5Dfree(handle);
	} else {
		fprintf(stderr,"Skipping dataset/attribute tests\n");
		++r.skip;
	}
    
	return r;
}

/*
 Test the group API in hdf5 file or group 'obj'.
 */
testResult_t testGroups(const hid_t obj)
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	const char grpname1[] = "group_1";
	const char grpname2[] = "group_2";
    
	if (CBF_SUCCESS==error && !r.fail) {
		/*
         cbf_H5Gcreate & cbf_H5Gfree:
         Verify expected failures,
         create a new group,
         ensure I can't create a duplicate group,
         check that freeing groups works correctly.
         */
		hid_t grp = CBF_H5FAIL;
		TEST_CBF_FAIL(cbf_H5Gcreate(CBF_H5FAIL, &grp, grpname1));
		TEST_CBF_FAIL(cbf_H5Gcreate(obj, 0, grpname1));
		TEST_CBF_FAIL(cbf_H5Gcreate(obj, &grp, 0));
		{ /* preconditions */
			TEST(H5Lexists(obj,grpname1,H5P_DEFAULT)==0);
		}
		/* check I can create groups */
		TEST_CBF_PASS(cbf_H5Gcreate(obj, &grp, grpname1));
		{ /* post-conditions */
			hid_t testObject = CBF_H5FAIL;
			const char * name = grpname1;
			TEST(cbf_H5Ivalid(grp));
			TEST(H5Lexists(obj,name,H5P_DEFAULT)>0);
			TEST(cbf_H5Ivalid(testObject = H5Oopen(obj,name,H5P_DEFAULT)));
			TEST(H5I_GROUP==H5Iget_type(testObject));
			H5Oclose(testObject);
		}
		{ /* ensure another group can't be created with the same name */
			hid_t handle = CBF_H5FAIL;
			TEST_CBF_FAIL(cbf_H5Gcreate(obj, &handle, grpname1));
			TEST(!cbf_H5Ivalid(handle));
		}
		{ /* free some groups */
			const hid_t tmpHandle = grp;
			TEST(cbf_H5Ivalid(grp));
			TEST_CBF_FAIL(cbf_H5Gfree(CBF_H5FAIL));
			TEST_CBF_PASS(cbf_H5Gfree(grp));
			TEST(!cbf_H5Ivalid(grp));
			TEST_CBF_FAIL(cbf_H5Gfree(grp));
			TEST(tmpHandle==grp);
			/* the data must remain in the tree */
			TEST(H5Lexists(obj,grpname1,H5P_DEFAULT)>0);
		}
	} else {
		fprintf(stderr,"Skipping group/create-free tests\n");
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		/*
		cbf_H5Gfind:
		Verify expected failures,
		check I can retrieve an existing group,
		check failure mode for non-existant group.
		*/
		hid_t grp = CBF_H5FAIL;

		{
			/*
			Preconditions:
			A group with name given by grpname1 exists;
			A group with name given by grpname2 doesn't exist.
			*/
			hid_t testObject = CBF_H5FAIL;
			TEST(H5Lexists(obj,grpname1,H5P_DEFAULT)>0);
			TEST(cbf_H5Ivalid(testObject = H5Oopen(obj,grpname1,H5P_DEFAULT)));
			TEST(H5I_GROUP==H5Iget_type(testObject));
			H5Oclose(testObject);
			TEST(H5Lexists(obj,grpname2,H5P_DEFAULT)==0);
		}

		/* verify expected failures */
		TEST_CBF_FAIL(cbf_H5Gfind(CBF_H5FAIL, &grp, grpname1));
		TEST_CBF_FAIL(cbf_H5Gfind(obj, 0, grpname1));
		TEST_CBF_FAIL(cbf_H5Gfind(obj, &grp, 0));

		/* check it can find an existing group */
		TEST_CBF_PASS(cbf_H5Gfind(obj, &grp, grpname1));
		cbf_H5Gfree(grp);

		/* check it doesn't find a non-exstant group correctly */
		TEST_CBF_NOTFOUND(cbf_H5Gfind(obj, &grp, grpname2));
	} else {
		fprintf(stderr,"Skipping group/find tests\n");
		++r.skip;
	}

	if (CBF_SUCCESS==error && !r.fail) {
		/*
         cbf_H5Grequire:
         Verify expected failures,
         check I can retrieve an existing group,
         check I can create a new group.
         */
		hid_t grp1 = CBF_H5FAIL;
		hid_t grp2 = CBF_H5FAIL;
		TEST_CBF_FAIL(cbf_H5Grequire(CBF_H5FAIL, &grp1, grpname1));
		TEST_CBF_FAIL(cbf_H5Grequire(obj, 0, grpname1));
		TEST_CBF_FAIL(cbf_H5Grequire(obj, &grp1, 0));
		{ /* preconditions */
			hid_t testObject = CBF_H5FAIL;
			TEST(H5Lexists(obj,grpname1,H5P_DEFAULT)>0);
			TEST(cbf_H5Ivalid(testObject = H5Oopen(obj,grpname1,H5P_DEFAULT)));
			TEST(H5I_GROUP==H5Iget_type(testObject));
			H5Oclose(testObject);
			TEST(H5Lexists(obj,grpname2,H5P_DEFAULT)==0);
		}
		/* check I can access existing groups or create new groups */
		TEST_CBF_PASS(cbf_H5Grequire(obj, &grp1, grpname1));
		TEST_CBF_PASS(cbf_H5Grequire(obj, &grp2, grpname2));
		{ /* post-conditions */
			hid_t testObject = CBF_H5FAIL;
			TEST(cbf_H5Ivalid(grp1));
			TEST(cbf_H5Ivalid(grp2));
			TEST(H5Lexists(obj,grpname2,H5P_DEFAULT)>0);
			TEST(cbf_H5Ivalid(testObject = H5Oopen(obj,grpname2,H5P_DEFAULT)));
			TEST(H5I_GROUP==H5Iget_type(grp2));
			TEST(!cbf_H5Ocmp(testObject,grp2));
			H5Oclose(testObject);
		}
		cbf_H5Gfree(grp1);
		cbf_H5Gfree(grp2);
	} else {
		fprintf(stderr,"Skipping group/require tests\n");
		++r.skip;
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* test some of the dataset management API */
		hid_t grp = CBF_H5FAIL;
		CBF_CALL(cbf_H5Grequire(obj,&grp,grpname1));
		TEST_COMPONENT(testDatasets(grp));
		cbf_H5Gfree(grp);
	} else {
		++r.skip;
		fprintf(stderr,"%s: Skipping dataset tests\n",__WHERE__);
	}
    
	if (CBF_SUCCESS==error && !r.fail) {
		/* test some of the attribute management API */
		hid_t grp = CBF_H5FAIL;
		CBF_CALL(cbf_H5Grequire(obj,&grp,grpname1));
		TEST_COMPONENT(test_attribute(grp));
		cbf_H5Gfree(grp);
	} else {
		++r.skip;
		fprintf(stderr,"%s: Skipping attribute tests\n",__WHERE__);
	}
    
	return r;
}

testResult_t testDatatypes()
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	/*
     Test the datatype API.
     */
	{ /* test variable length string type creation */
		hid_t type = CBF_H5FAIL;
		TEST_CBF_PASS(cbf_H5Tcreate_string(&type, H5T_VARIABLE));
		TEST(cbf_H5Ivalid(type));
		TEST(H5I_DATATYPE==H5Iget_type(type));
		TEST(H5T_STRING==H5Tget_class(type));
		TEST(H5T_STR_NULLTERM==H5Tget_strpad(type));
		TEST(H5Tis_variable_str(type)>0);
		{ /* test cbf_H5Tfree */
			const hid_t tmpType = type;
			TEST_CBF_FAIL(cbf_H5Tfree(CBF_H5FAIL));
			TEST_CBF_PASS(cbf_H5Tfree(type));
			TEST(!cbf_H5Ivalid(type));
			TEST_CBF_FAIL(cbf_H5Tfree(type));
			TEST(tmpType == type);
		}
	}
	{ /* test fixed length string type creation */
		hid_t type = CBF_H5FAIL;
		const int len = 42;
		TEST_CBF_PASS(cbf_H5Tcreate_string(&type, len));
		TEST(cbf_H5Ivalid(type));
		TEST(H5I_DATATYPE==H5Iget_type(type));
		TEST(H5T_STRING==H5Tget_class(type));
		TEST(H5T_STR_NULLTERM==H5Tget_strpad(type));
		TEST(H5Tis_variable_str(type)==0);
		TEST(H5Tget_size(type)==1+len);
		cbf_H5Tfree(type);
	}
	return r;
}

testResult_t testDataspaces()
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
	/*
     Test the dataspace API.
     */
	{ /* rank -1 */
		hid_t space = CBF_H5FAIL;
		TEST_CBF_FAIL(cbf_H5Screate(&space,-1,0,0));
	}
	{ /* rank 0 */
		hid_t space = CBF_H5FAIL;
		hid_t tmp = CBF_H5FAIL;
		TEST_CBF_PASS(cbf_H5Screate(&space,0,0,0));
		TEST(cbf_H5Ivalid(space));
		TEST(0==H5Sget_simple_extent_dims(space,0,0));
		{ /* test cbf_H5Sfree */
			tmp = space;
			TEST_CBF_FAIL(cbf_H5Sfree(CBF_H5FAIL));
			TEST_CBF_PASS(cbf_H5Sfree(space));
			TEST(!cbf_H5Ivalid(space));
			TEST_CBF_FAIL(cbf_H5Sfree(space));
			TEST(tmp==space);
		}
	}
	{ /* rank 1 */
		hid_t space = CBF_H5FAIL;
		hsize_t dim[] = {2};
		hsize_t max[] = {4};
		{ /* dim & max => pass */
			hsize_t _dim[] = {0};
			hsize_t _max[] = {0};
			TEST_CBF_PASS(cbf_H5Screate(&space,1,dim,max));
			TEST(cbf_H5Ivalid(space));
			TEST(1==H5Sget_simple_extent_dims(space,0,0));
			TEST(1==H5Sget_simple_extent_dims(space,_dim,_max));
			TEST(!CBFM_cmp_hsize_t_exact(dim,_dim,1,0));
			TEST(!CBFM_cmp_hsize_t_exact(max,_max,1,0));
			cbf_H5Sfree(space);
		}
		{ /* dim & !max => pass */
			hsize_t _dim[] = {0};
			hsize_t _max[] = {0};
			TEST_CBF_PASS(cbf_H5Screate(&space,1,dim,0));
			TEST(cbf_H5Ivalid(space));
			TEST(1==H5Sget_simple_extent_dims(space,0,0));
			TEST(1==H5Sget_simple_extent_dims(space,_dim,_max));
			TEST(!CBFM_cmp_hsize_t_exact(dim,_dim,1,0));
			TEST(!CBFM_cmp_hsize_t_exact(dim,_max,1,0));
			cbf_H5Sfree(space);
		}
		{ /* !dim & max => fail */
			const hid_t tmp = space;
			TEST_CBF_FAIL(cbf_H5Screate(&space,1,0,max));
			TEST(!cbf_H5Ivalid(space));
			TEST(tmp==space);
			cbf_H5Sfree(space);
		}
		{ /* !dim & !max => fail */
			const hid_t tmp = space;
			TEST_CBF_FAIL(cbf_H5Screate(&space,1,0,0));
			TEST(!cbf_H5Ivalid(space));
			TEST(tmp==space);
			cbf_H5Sfree(space);
		}
	}
	{ /* rank 2 */
		hid_t space = CBF_H5FAIL;
		hsize_t dim[] = {2,4};
		hsize_t max[] = {8,16};
		{ /* dim & max => pass */
			hsize_t _dim[] = {0,0};
			hsize_t _max[] = {0,0};
			TEST_CBF_PASS(cbf_H5Screate(&space,2,dim,max));
			TEST(cbf_H5Ivalid(space));
			TEST(2==H5Sget_simple_extent_dims(space,0,0));
			TEST(2==H5Sget_simple_extent_dims(space,_dim,_max));
			TEST(!CBFM_cmp_hsize_t_exact(dim,_dim,2,0));
			TEST(!CBFM_cmp_hsize_t_exact(max,_max,2,0));
			cbf_H5Sfree(space);
		}
		{ /* dim & !max => pass */
			hsize_t _dim[] = {0,0};
			hsize_t _max[] = {0,0};
			TEST_CBF_PASS(cbf_H5Screate(&space,2,dim,0));
			TEST(cbf_H5Ivalid(space));
			TEST(2==H5Sget_simple_extent_dims(space,0,0));
			TEST(2==H5Sget_simple_extent_dims(space,_dim,_max));
			TEST(!CBFM_cmp_hsize_t_exact(dim,_dim,2,0));
			TEST(!CBFM_cmp_hsize_t_exact(dim,_max,2,0));
			cbf_H5Sfree(space);
		}
		{ /* !dim & max => fail */
			const hid_t tmp = space;
			TEST_CBF_FAIL(cbf_H5Screate(&space,2,0,max));
			TEST(!cbf_H5Ivalid(space));
			TEST(tmp==space);
			cbf_H5Sfree(space);
		}
		{ /* !dim & !max => fail */
			const hid_t tmp = space;
			TEST_CBF_FAIL(cbf_H5Screate(&space,2,0,0));
			TEST(!cbf_H5Ivalid(space));
			TEST(tmp==space);
			cbf_H5Sfree(space);
		}
	}
	return r;
}

int main()
{
	int error = CBF_SUCCESS;
	testResult_t r = {0,0,0};
    
	/* test the test functions a bit */
	TEST(1);
	TEST_CBF_PASS(CBF_SUCCESS);
	TEST_CBF_FAIL(CBF_FORMAT);
	TEST_CBF_FAIL(CBF_ARGUMENT);
	TEST_CBF_FAIL(CBF_H5ERROR);
	TEST_CBF_FAIL(CBF_H5DIFFERENT);
    
	/* check that cbf_H5Ivalid can fail: */
	TEST(!cbf_H5Ivalid(CBF_H5FAIL));
	TEST(H5I_DATATYPE==H5Iget_type(H5T_STD_I32LE));
    
	{ /* Try opening a file */
		const char filename[] = "testfile.h5";
		hid_t h5file = CBF_H5FAIL;
		TEST_CBF_PASS(cbf_H5Fopen(&h5file, filename));
		TEST(cbf_H5Ivalid(h5file));
		/* test the API */
		TEST_COMPONENT(testGroups(h5file));
		TEST_COMPONENT(testDatatypes());
		TEST_COMPONENT(testDataspaces());
		{ /* close the file */
			const hid_t const_h5file = h5file;
			TEST_CBF_PASS(cbf_H5Fclose(const_h5file));
			TEST(!cbf_H5Ivalid(h5file));
			TEST(CBF_H5FAIL!=h5file);
		}
	}
    
	printf_results(&r);
	return r.fail || r.skip ? 1 : 0;
}

