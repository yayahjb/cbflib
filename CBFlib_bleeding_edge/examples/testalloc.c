#include <stdio.h>
#include <cbf_alloc.h>
#include "cbf.h"
#include "unittest.h"

/*
cbf_realloc should:
behave in a similar way to malloc, if given no or zero 'old_nelem';
behave in a similar way to free, if given 0 for 'elsize' or 'nelem';
retain the contents of any allocated block up to the minimum of
'*old_nelem * elsize' (the old size) and 'nelem * elsize' (the new size).
*/
testResult_t test_cbf_realloc()
{
	testResult_t r = {0,0,0};
	int error = CBF_SUCCESS;
	int * block = NULL;
	size_t nelem = 0;

	/* test expected failures */

	TEST_CBF_FAIL(cbf_realloc(0,&nelem,sizeof(int),4));
	TEST_CBF_FAIL(cbf_realloc((void**)&block,&nelem,0,4));

	/* check a precondition */
	TEST(!block);

	/* allocate */

	TEST_CBF_PASS(cbf_realloc((void**)&block,&nelem,sizeof(int),4));
	TEST(block);

	/* fill with some sample data */
	block[0] = 0;
	block[1] = 1;
	block[2] = 2;
	block[3] = 3;

	/* reallocate */

	TEST_CBF_PASS(cbf_realloc((void**)&block,&nelem,sizeof(int),8));
	TEST(block);

	/* check the sample data */
	TEST(0==block[0]);
	TEST(1==block[1]);
	TEST(2==block[2]);
	TEST(3==block[3]);
	
	/* free */

	TEST_CBF_PASS(cbf_realloc((void**)&block,&nelem,sizeof(int),0));
	TEST(!block);

	return r;
}

int main()
{
	testResult_t r = {0,0,0};

	TEST_COMPONENT(test_cbf_realloc());

	printf("Passed: %u\nFailed: %u\nSkipped: %u\n",r.pass,r.fail,r.skip);
	return r.fail ? 1 : 0;
}

