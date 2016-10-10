/**********************************************************************
 *  cbf_getopt.c                                                      *
 *                                                                    *
 *                                                                    *
 * Created by Herbert J. Bernstein on 6/8/09.                         *
 * (C) Copyright 2009 Herbert J. Bernstein                            *
 *                                                                    *
 **********************************************************************/

 /**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 **********************************************************************/

/*************************** GPL NOTICES ******************************
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
 **********************************************************************/

/************************* LGPL NOTICES *******************************
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
 **********************************************************************/


#ifdef __cplusplus

extern "C" {

#endif

#include <cbf.h>
#include <cbf_getopt.h>
#include <cbf_alloc.h>
#include <cbf_context.h>
#include <string.h>


	/* \brief Consume one option from the option specification string given in 'options'.

	If hasvalue is greater than zero an option value is required, if hasvalue is zero the
	option has no value, if hasvalue is less than zero the value is optional.

	\return The new location within the option specification string.
	*/
    static const char * cbf_getopt_locate_option(const char * options,
                                          char * optchar,
                                          const char * * longopt,
                                          int * hasvalue) {

        char c;

        if (optchar) *optchar = '\0';

        if (longopt) *longopt = NULL;

        if (hasvalue) *hasvalue = 0;

		/* Anything other than '(' or ':' is a short option character. 
           It may have a long option, a value or both. */

        if (*options != '(' && *options != ':' && optchar)*optchar = *options;

        while ((c=*options)) {

            if (c=='(') {

				/* A long option is surrounded by '(' and ')'. A value 
                   specification may follow. */

                ++options;

                if (longopt) *longopt = options;

                while ((c=*options) && (c!=')')) {

                    options++;
                }

                if (!c) {

                    return options;
                }

            } else if (c==':') {

				/* A single ':' characters results in hasvalue being set 
                   to a positive number. Two ':' characters results in 
                   hasvalue being set to a negative number. This always 
                   ends processing for this option. */

                if (hasvalue) *hasvalue = 1;

                options ++;

                if ((c=*options) && (c==':')) {

                    if (hasvalue) *hasvalue = -1;

                    options ++;
                }

                return options;

            } else if (c) {

				/* Consume the single option character. */

                options ++;

                if ((c=*options) && (c=='(' || c==':')) continue;

                return options;

            }

        }

        return options;
    }

	/**
	Allocate memory for a new handle and initialise it.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	*/
	int cbf_make_getopt_handle(
        cbf_getopt_handle * handle /**< A pointer to the handle to be allocated. */
    )
	{
        *handle = NULL;

        cbf_failnez (cbf_alloc ((void **) handle, NULL,
                                sizeof (cbf_getopt_struct), 1))

        (*handle)->optstructs = NULL;

        cbf_onfailnez (cbf_alloc ((void **) &((*handle)->optstructs),
                                  &((*handle)->optstructs_capacity),
                                  sizeof(cbf_getopt_optstruct), 10),
                       cbf_free((void **) handle, NULL))

        (*handle)->optstructs_size = 0;
        (*handle)->optind = 0; /* ordinal of option in options     */
        (*handle)->options = NULL;
		return CBF_SUCCESS;
    }


    /* clear the data in a cbf_getopt handle */

    static int cbf_clear_getopt_handle(cbf_getopt_handle handle) {

        cbf_getopt_optstruct * optstruct;

        size_t index;

        for (index = 0; index < handle->optstructs_size; index++) {

            optstruct = &(handle->optstructs[index]);

            if (optstruct->optstr) {

                cbf_failnez(cbf_free_text(&(optstruct->optstr),NULL))
            }

            if (optstruct->optval) {

                cbf_failnez(cbf_free_text(&(optstruct->optval),NULL))
            }

        }

        handle-> optstructs_size = 0;

        handle->optind = 0;

        if (handle->options) {

            cbf_failnez(cbf_free_text(&(handle->options),NULL))
        }

		return CBF_SUCCESS;
    }

	/**
	Free a handle and all memory it is responsible for.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	*/
	int cbf_free_getopt_handle(
        cbf_getopt_handle handle /**< The handle to be free'd. */
    )
	{
        void *memblock;

        cbf_failnez( cbf_clear_getopt_handle(handle) )

        memblock = (void *) handle;

        if (handle) {

            if (handle->optstructs) {

                cbf_failnez(cbf_free((void**)&(handle->optstructs), NULL));

            }

            return cbf_free(&memblock, NULL);

        }

        return CBF_SUCCESS;

    }


	/**
	Parses command line options according to a option specification string "[+-]?([^\(:]\([^\)]*\):{0,2})+".

	For example, "ab:c::d(long-d)e(long-e):f(long-f)::\1(long-g)\1(long-h):\1(long-i)::" specifies:

	- A short option 'a' without a value.
	- A short option 'b' with a required value.
	- A short option 'c' with an optional value.
	- A long option "long-d" with short version 'd' and no value.
	- A long option "long-e" with short version 'e' and a required value.
	- A long option "long-f" with short version 'f' and an optional value.
	- A long option "long-g" with no short version and no value.
	- A long option "long-h" with no short version and a required value.
	- A long option "long-i" with no short version and an optional value.

	A required option will always cause the following argument to be consumed, an optional option will cause
	the next argument to be consumed iff it doesn't begin with '-'. An argument of "--" may be consumed as
	the value of an option which requires an argument, or will otherwise end option parsing.

	A leading '-' in the option string will result in anything that looks like an option being treated as an
	option with an optional argument. A leading '+' in the option string will cause option processing to end
	before or after the first non-option argument has been processed.

	This function will:

	- Clear the previous contents of the handle, and copy the option specification string.
	- Iterate over each argument to:
		- Check for "--", which ends option parsing.
		- Check for a long option beginning with "--", storing the option in optstr and any value in optval.
		- Check for a short option beginning with '-', storing the option in optopt and any value in optval.
			optstr is set to a single character string equal to optopt.
	- Append remaining options with:
		- optval = argv[i]
		- optopt = 0
		- optord = -1
		- optstr = NULL
	- Permute the options using a stable partitioning algorithm within the handle to
	sort all non-options to the end iff the option specification string is not '-'.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	*/

    int cbf_getopt_parse(
             cbf_getopt_handle handle, /**< The <code>cbf_getopt_handle</code> to populate. */
			 int argc, /**< The number of arguments in the argument vector. */
			 char ** argv, /**< The argument vector - as may have been passed to main. */
			 const char * options /**< The option specification string. */
    )
	{

        int ii, iii, ios;

        void * voptstructs;

        cbf_getopt_optstruct * optstruct;

        const char * opts;

        int hasvalue;

        const char * longopt;

        char optchar;

        size_t optlen;

        int optord;

        int foundopt;

        if (handle == NULL || argv == NULL ) return CBF_ARGUMENT;

        cbf_failnez( cbf_clear_getopt_handle(handle) )

        /* Allocate as many opstruct slots as we have arguments */

        if (handle->optstructs) {

            voptstructs = (void *)(handle->optstructs);

            cbf_failnez(cbf_realloc((void **) &voptstructs,
                                    &(handle->optstructs_capacity),
                                    sizeof(cbf_getopt_optstruct),argc))

        } else {

            cbf_failnez(cbf_alloc((void **) &voptstructs,
                                  &(handle->optstructs_capacity),
                                  sizeof(cbf_getopt_optstruct),argc))

        }

        handle->optstructs = (cbf_getopt_optstruct *)voptstructs;

        if (handle->options != NULL) {

            cbf_failnez(cbf_free_text((const char * *)&(handle->options),NULL))

        }

        /*  If options have been specified, use them.  Otherwise
         default to "-"
         */
         if (options) {

            handle->options = cbf_copy_string(NULL,options,0);

        } else {

            handle->options = cbf_copy_string(NULL,"-",0);

        }

        handle->optind = 0;

        for ( ii=1; ii < argc; ii++) {

            /* Prepare the next slot */

            optstruct = &((handle->optstructs)[(handle->optind)++]);

            (handle->optstructs_size)++;

            optstruct->optopt = 0;

            optstruct->optord = -1;

            optstruct->optstr = NULL;

            optstruct->optval = NULL;


            /* on --, end the options scan */

            if ( !strcmp(argv[ii],"--") ) {

                /* copy all remaining arguments as unflagged values */

                break;

            }
            /* process a --option case */

            if (!strncmp("--",argv[ii],2)) {

                optstruct->optstr = cbf_copy_string(NULL,2+argv[ii],0);

                opts = options;

                if (*opts=='-' || *opts =='+') opts++;

                optlen = strlen(argv[ii]+2);

                optord = -1;

                foundopt = 0;

                do {
                    opts= cbf_getopt_locate_option(opts, &optchar, &longopt, &hasvalue);

                    optord++;

                    if (longopt && !strncmp(longopt,argv[ii]+2,optlen) && longopt[optlen]==')') {

                        optstruct->optopt = optchar;

                        optstruct->optord = optord;

                        optstruct->optval = NULL;

                        if (ii+1 < argc && (hasvalue >0
                                            || (*(argv[ii+1])!='-'&& hasvalue < 0) )) {

                            /* NOTE: an argument of "--" will be matched by the above test! */

                            optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);

                            ii++;
						}

                            foundopt++;

                            break;

                        }

                } while (*opts);

                if (foundopt) continue;

                if (*options == '-') {

                    optstruct->optopt = '\1';

                    optstruct->optord = -1;

                    optstruct->optval = NULL;

                    if (ii+1 < argc && *(argv[ii+1])!='-' ) {

                        optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);

                        ii++;

                        break;

                    }

                    continue;

                }

                /* this is not an expected long option and the option string
                 does not have a leading '-', therefore this is simply
                 a non-option value */

                if (*options=='+') break;

                optstruct->optval = cbf_copy_string(NULL,argv[ii],0);

                optstruct->optopt = 0;

                optstruct->optord = -1;

                cbf_failnez(cbf_free_text(&(optstruct->optstr),NULL))

                optstruct->optstr = NULL;

                continue;

            }

            /* now for the single '-' case marking a lone letter option */

            if (*(argv[ii]) == '-' && strlen(argv[ii]) > 1 ) {

                char xc[2];

                xc[0] = argv[ii][1];

                xc[1] = '\0';

                optstruct->optstr = cbf_copy_string(NULL,xc,0);

                opts = options;

                if (*opts=='-' || *opts =='+') opts++;

                optlen = 1;

                optord = -1;

                foundopt = 0;

                do {
                    opts= cbf_getopt_locate_option(opts, &optchar, &longopt,&hasvalue);

                    optord++;

                    if (xc[0] == optchar) {

                        optstruct->optopt = optchar;

                        optstruct->optord = optord;

                        optstruct->optval = NULL;

                        if ((strlen(argv[ii]+2) > 0) ||
                            (ii+1 < argc && (hasvalue >0
                                             || (*(argv[ii+1])!='-'&& hasvalue < 0) ))) {
                            if (strlen(argv[ii]+2) > 0) {

                                optstruct->optval = cbf_copy_string(NULL,argv[ii]+2,0);

                            } else {

                                optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);

                                ii++;

                            }


                        }

                        foundopt++;

                        break;
                    }

                } while (*opts);

                if (foundopt) continue;

                if (*options == '-')      {

                    optstruct->optopt = '\1';

                    optstruct->optord = -1;

                    optstruct->optval = NULL;

                    if (ii+1 < argc && *(argv[ii+1])!='-' ) {

                        optstruct->optval = cbf_copy_string(NULL,argv[ii+1],0);

                        ii++;

                        break;

                    }

                    continue;

                };


                /* this is not an expected short option and the option string
                 does not have a leading '-', therefore this is simply
                 a non-option value

                 */

                if (*options=='+') break;

                optstruct->optval = cbf_copy_string(NULL,argv[ii],0);

                optstruct->optopt = 0;

                optstruct->optord = -1;

                cbf_failnez(cbf_free_text(&(optstruct->optstr),NULL))

                optstruct->optstr = NULL;

                continue;

            }

            /* All that is left is to treat this as a non-option value */

            optstruct->optval = cbf_copy_string(NULL,argv[ii],0);

            optstruct->optopt = 0;

            optstruct->optord = -1;

            optstruct->optstr = NULL;

            if (*options=='+') break;

        }


        /* ii is the last argument processed, the remaining arguments get added
           at the end */

        ios =  handle->optstructs_size;

        for (iii = ii+1; iii < argc; iii++) {

            optstruct = &((handle->optstructs)[(handle->optind)++]);

            (handle->optstructs_size)++;

            optstruct->optval = cbf_copy_string(NULL,argv[iii],0);

            optstruct->optopt = 0;

            optstruct->optord = -1;

            optstruct->optstr = NULL;

        }

        /* if *options is not '-', then all non-options in obstructs need to
           be sorted to the end of the list */

        iii = ios-1;

        while (iii >= 0) {

            cbf_getopt_optstruct temp;

            optstruct = &((handle->optstructs)[iii]);

            /* Any option with no option ordinal moves up to index ios-1 */

            if (optstruct->optord < 0) {

                if (iii < ios-1) {

                  memmove((void *)(&temp),(void *)optstruct,sizeof(cbf_getopt_optstruct));

                    for (ii = iii; ii < ios-1; ii++) {

                        memmove((void *)(&((handle->optstructs)[ii])),
                                (void *)(&((handle->optstructs)[ii+1])), 
                                sizeof(cbf_getopt_optstruct));

                    }

                  memmove((void *)(&((handle->optstructs)[ios-1])),
                                (void *)(&temp),
                                sizeof(cbf_getopt_optstruct));

                }

                ios --;

            }

            iii--;

        }

        return CBF_SUCCESS;


    }

	/**
	Selects the option at index <code>0</code> within the given handle,
	or fails if no handle is given or no options are present.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	*/
	int cbf_rewind_getopt_option(
        cbf_getopt_handle handle /**< The handle to attempt to use. */
    )
	{
        if ( !handle ) return CBF_ARGUMENT;

        handle->optind = 0 ;

        if ( handle->optind >=  (ssize_t)(handle->optstructs_size)) return CBF_NOTFOUND;

        return CBF_SUCCESS;

    }

	/**
	Selects the option at <code>current_index + 1</code> within the given handle,
	or fails if no handle is given or no further options are present.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	 */
	int cbf_next_getopt_option(
        cbf_getopt_handle handle /**< The handle to attempt to use. */
    )
	{
        if ( !handle ) return CBF_ARGUMENT;

        handle->optind++;

        if ( handle->optind >=  (ssize_t)(handle->optstructs_size)) return CBF_NOTFOUND;

        return CBF_SUCCESS;

    }

	/**
	Selects the option at <code>option</code> within the given handle,
	or fails if no handle is given or no further options are present.
	The value of <code>option</code> should be less than the value
	obtained by a call to <code>cbf_count_getopt_options</code>. If
	<code>option</code> is set to zero the first option will be
	selected, if it exists.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	 */
	int cbf_select_getopt_option(
            cbf_getopt_handle handle, /**< The handle to attempt to use. */
			unsigned int option /**< The index of the option to select. */
    )
	{
        if ( !handle ) return CBF_ARGUMENT;

        if ( option >=  handle->optstructs_size) return CBF_ARGUMENT;

        handle->optind = option;

        return CBF_SUCCESS;

    }

	/**
	The value returned in <code>options</code> will be a number that is one
	larger than the highest index that may be used to select an option with
	a call to <code>cbf_select_getopt_option</code>. All non-negative
	integers smaller than this value index an option.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	 */
	int cbf_count_getopt_options(
            cbf_getopt_handle handle, /**< The handle to attempt to use. */
			unsigned int * options /**< The (optional) location to store the total number of options. */
    )
	{
        if ( !handle ) return CBF_ARGUMENT;

        if ( options ) *options = handle->optstructs_size;

        return CBF_SUCCESS;
    }

	/**
	Retrieves the data stored in the given handle relating the the currently selected option within
	the handle. To select an option see the documentation for the <code>cbf_rewind_getopt_option</code>,
	<code>cbf_next_getopt_option</code>, <code>cbf_count_getopt_options</code> and
	<code>cbf_select_getopt_option</code> functions.

	\sa cbf_make_getopt_handle
	\sa cbf_free_getopt_handle
	\sa cbf_getopt_parse
	\sa cbf_rewind_getopt_option
	\sa cbf_next_getopt_option
	\sa cbf_select_getopt_option
	\sa cbf_count_getopt_options
	\sa cbf_get_getopt_data
	\return An error code.
	*/
	int cbf_get_getopt_data(
             cbf_getopt_handle handle, /**< The handle to read data from. */
			 int * optopt, /**< The (optional) location to return the single 
                              character short option, which defaults to 
                              '<code>\\0</code>'. */
			 int * optord, /**< The (optional) location to return the index of 
                              the option within the option specification string that 
                              was parsed by <code>cbf_getopt_parse</code>. */
			 const char * * optstr, /**< The (optional) location to return the 
                              long form of the option or a single character string 
                              version of the short form. */
			 const char * * optval /**< The (optional) location to return the value 
                              associated with the selected option, if any. */)
	{
        cbf_getopt_optstruct * optstruct;

        if ( !handle ) return CBF_ARGUMENT;

        if ( handle->optind < 0 || handle->optind >= (ssize_t)(handle->optstructs_size)) return CBF_NOTFOUND;

        optstruct = &(handle->optstructs[handle->optind]);

        if (optopt) *optopt = optstruct->optopt;

        if (optord) *optord = optstruct->optord;

        if (optstr) *optstr = optstruct->optstr;

        if (optval) *optval = optstruct->optval;

        return CBF_SUCCESS;

    }

#ifdef __cplusplus

}

#endif
