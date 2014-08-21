/**********************************************************************
 *                                                                    *
 * Unit tests for CBF's memory allocation routines, to ensure         *
 * they work as documented and protect against regressions.           *
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
testResult_t test_cbf_realloc( void )
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
    
	/* Note: this doesn't check that 'block' has been free'd, valgrind is required for that. */
	TEST(!block);
    
	return r;
}

int main(int argc, char ** argv)
{

    CBF_UNUSED(argc);
    CBF_UNUSED(argv);
	testResult_t r = {0,0,0};
    
	TEST_COMPONENT(test_cbf_realloc());
    
	printf_results(&r);
	return r.fail ? 1 : 0;
}

