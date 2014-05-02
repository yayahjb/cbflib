/**********************************************************************
 *  cbf_getopt.h                                                      *
 *                                                                    *
 *                                                                    *
 * Created by Herbert J. Bernstein on 6/8/09.                         *
 * (C) Copyright 2009 Herbert J. Bernstein                            *
 *                                                                    *
 *                                                                    *
 *  This is a functional replacement for gnu getopt                   *
 *  for use with CBFlib to minimize porting problems and              *
 *  to ensure appropriateness of use under the LGPL                   *
 *                                                                    *
 *  The interface is _not_ a drop-in replacment for the               *
 *  gnu getopt interface.                                             *
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

/**
\file cbf_getopt.h
\defgroup getopt
*/

#ifndef CBF_GETOPT_H
#define CBF_GETOPT_H

#ifdef __cplusplus

extern "C" {
    
#endif
    

typedef struct {
    int optopt;         /* character of the option
                          if a "-x" option, this is 'x', whether
                             given in options or not
                          if a "--xxx" option, this is
                             the option letter preceding the
                             (xxx) in the options string
                          if there is no option letter, 0 */
    int optord;         /* ordinal of the option in options, or -1 */
    const char * optstr;      /* the null-terminated character string of the option
                          if a "-x" option is given, this is "x"
                          if a "--xxx" option, this is "xxx"
                          optstr is given whether the option is
                          in options or not.  A NULL is used for
                          a value with no option */
    const char * optval;      /* the null-terminated character string of the option
                          value, or null if none is given
                          if the option is specified in options,
                          the value may begin with '-', but if the
                          option is not specified in options,
                          a value that begins with '-' is treated
                          as a new option */
} cbf_getopt_optstruct;

typedef struct {
    cbf_getopt_optstruct * optstructs;
                       /* array of optstructs */
    size_t optstructs_size;
                       /* count of valid optstructs in optstructs */
    size_t optstructs_capacity;
                       /* capacity of optstructs */
    int optind;        /* next option in optstructs to process */
    const char * options;
                       /* string of options:  if x is a
                          valid option that has a required
                          argument, "x:" should appear in
                          the string;  if x is a valid option
                          that has an optional argument, "x::"
                          should appear in the string.  If what
                          follows "x" is the end of the string
                          or is a character other than a ":",
                          then x does not accept an option. 
                          If '(' appears, then all characters
                          prior to the next matching ')' are the name
                          of a long option 
                        
                          If the options string begins with '-', all
                          options are left in their original location,
                          any options not specified in the options string
                          are reported with optord -1 and if the next
                          argv value is not an option, it is returned
                          as the value.
                        
                          If the options string begins with '+', the
                          first non-option cases all remaining argv
                          elements to be return as values just as if
                          as "--" had been encountered at that point
                        
                          Normally, non-options are sorted to the end
                          of the list.
                        
                          */
} cbf_getopt_struct;

typedef cbf_getopt_struct * cbf_getopt_handle;


	/**
	\brief Create a cbf_getopt handle.
	\ingroup getopt
	*/
int cbf_make_getopt_handle(cbf_getopt_handle * handle);

	/**
	\brief Free a cbf_getopt handle.
	\ingroup getopt
	*/
int cbf_free_getopt_handle(cbf_getopt_handle handle);

	/**
	\brief Parse argc and argv into a cbf_getopt_handle according to the option specification given by <code>options</code>.
	\ingroup getopt
	*/
	int cbf_getopt_parse( cbf_getopt_handle handle, int argc, char ** argv,
			  const char * options);

	/**
	\brief Select the first option within a cbf_getopt handle.
	\ingroup getopt
	*/
int cbf_rewind_getopt_option ( cbf_getopt_handle handle );

	/**
	\brief Select the next option within a cbf_getopt handle.
	\ingroup getopt
	 */
int cbf_next_getopt_option ( cbf_getopt_handle handle );

	/**
	\brief Select an option within the given handle by its index.
	\ingroup getopt
	 */
int cbf_select_getopt_option ( cbf_getopt_handle handle, unsigned int option );

	/**
	\brief Get the number of options within the given handle.
	\ingroup getopt
	 */
int cbf_count_getopt_options ( cbf_getopt_handle handle, unsigned int * options );

	/**
	\brief Get the data for an option.
	\ingroup getopt
	*/
int cbf_get_getopt_data ( cbf_getopt_handle handle, int * optopt,
                         int * optord, const char * * optstr, const char ** optval);
    
#ifdef __cplusplus
    
}

#endif

#endif

