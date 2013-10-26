/**********************************************************************
 *                                                                    *
 * Cross platform 'tail' program to deal with h5dump writing a        *
 * filename to the first line of its output.                          *
 *                                                                    *
 * J.Sloan                                                            *
 *                                                                    *
 **********************************************************************
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
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
#include <stdlib.h>
#include <string.h>

/*
Filter to remove the first few lines from a file without needing to read the whole thing into memory.

This is only present to help get cmake to compare hdf5 files:
h5diff doesn't work properly with certain attribute differences, forcing h5dump to be used. h5dump always
puts the filename on the first line. Cmake has no nice cross-platform 'tail' command, so for two dumped
files to compare equal the first line must be stripped. Cmake's file & string functions would read the
entire file into memory and be incredibly difficult (or impossible) to use, so this seems to be the least
painful way of getting rid of the filename printed by h5dump.
*/
int main(const int argc, const char * const * const argv)
{
	/* variables
	N - number of lines to remove from the top of stdin: default = 0
	help - flag to print help message:
		0 - don't
		1 - print it
		2 - print and exit successfully
	error - error code:
		0 - success
		1 - problem
	arg - the argument currently being examined. Skip the program name if we have any arguments, I don't want to try to parse it.
	*/
	unsigned long N = 0;
	int help = 0;
	int error = 0;
	const char * const * arg = argv + (argc ? 1 : 0);

	/* extract command line arguments */
	for (; arg != argv+argc; ++arg) {
		if (!strcmp(*arg,"-?") || !strcmp(*arg,"--help")) {
			help = 2;
			break;
		} else if (!strcmp(*arg,"-n")) {
			/* try to extract a positive integer from argv[2], set N appropriately */
			if (arg+1 != argv+argc) {
				const char * end;
				++arg;
				N = strtoul(*arg,(char**)(&end),0);
				if (!('\0' != **arg && '\0' == *end)) {
					fprintf(stderr,"Invalid argument for '-n': '%s'\n",*arg);
					error = 1;
				}
			} else {
				fprintf(stderr,"Expected an argument for '-n'\n");
				error = 1;
			}
			break;
		} else {
			/* print error message to stderr */
			fprintf(stderr,"Unrecognised argument: '%s'\n",*arg);
			error = 1;
			break;
		}
	}

	if (help) {
		/* print usage message */
		printf(
			"Usage:\n\n"
			"%s\n"
			"Input to 'stdin' passed straight to 'stdout'.\n\n"
			"%s -?\n"
			"%s --help\n"
			"Print a short usage message, then immediately exit successfully without filtering anything.\n\n"
			"%s -n N\n"
			"Remove the first N lines from stdin, delimited by the '\\n' character, passing the rest to stdout.\n",
			*argv, *argv, *argv, *argv
		);
		/* return successfully - even if there were some errors in the arguments - if one of the 'help' arguments was used */
		if (2 == help) return 0;
	}

	if (!error) {
		/* start filtering */
		while (!feof(stdin)) {
			const int c = fgetc(stdin);
			if (N) {
				if ('\n' == c) --N;
			} else fputc(c,stdout);
		}
	}

	/* no cleanup required */

	/* done */
	return error;
}

