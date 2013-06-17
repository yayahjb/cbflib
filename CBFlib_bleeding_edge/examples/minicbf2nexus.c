
/**********************************************************************
 * minicbf2nexus.c by J. Sloan of Diamond Light Source                *
 *                                                                    *
 * CBFlib Version 0.9.3 30 May 2013                                   *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2013 Herbert J. Bernstein                            *
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

/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term "this software", as used in these Notices, refers to      *
 * those portions of the software package CBFlib that were created by *
 * employees of the Stanford Linear Accelerator Center, Stanford      *
 * University.                                                        *
 *                                                                    *
 * Stanford disclaimer of copyright                                   *
 * --------------------------------                                   *
 * Stanford University, owner of the copyright, hereby disclaims its  *
 * copyright and all other rights in this software.  Hence, anyone    *
 * may freely use it for any purpose without restriction.             *
 *                                                                    *
 * Acknowledgement of sponsorship                                     *
 * ------------------------------                                     *
 * This software was produced by the Stanford Linear Accelerator      *
 * Center, Stanford University, under Contract DE-AC03-76SFO0515 with *
 * the Department of Energy.                                          *
 *                                                                    *
 * Government disclaimer of liability                                 *
 * ----------------------------------                                 *
 * Neither the United States nor the United States Department of      *
 * Energy, nor any of their employees, makes any warranty, express or *
 * implied, or assumes any legal liability or responsibility for the  *
 * accuracy, completeness, or usefulness of any data, apparatus,      *
 * product, or process disclosed, or represents that its use would    *
 * not infringe privately owned rights.                               *
 *                                                                    *
 * Stanford disclaimer of liability                                   *
 * --------------------------------                                   *
 * Stanford University makes no representations or warranties,        *
 * express or implied, nor assumes any liability for the use of this  *
 * software.                                                          *
 *                                                                    *
 * Maintenance of notices                                             *
 * ----------------------                                             *
 * In the interest of clarity regarding the origin and status of this *
 * software, this and all the preceding Stanford University notices   *
 * are to remain affixed to any copy or derivative of this software   *
 * made or distributed by the recipient and are to be affixed to any  *
 * copy of software made or distributed by the recipient that         *
 * contains a copy or derivative of this software.                    *
 *                                                                    *
 * Based on SLAC Software Notices, Set 4                              *
 * OTT.002a, 2004 FEB 03                                              *
 **********************************************************************/



/**********************************************************************
 *                               NOTICE                               *
 * Creative endeavors depend on the lively exchange of ideas. There   *
 * are laws and customs which establish rights and responsibilities   *
 * for authors and the users of what authors create.  This notice     *
 * is not intended to prevent you from using the software and         *
 * documents in this package, but to ensure that there are no         *
 * misunderstandings about terms and conditions of such use.          *
 *                                                                    *
 * Please read the following notice carefully.  If you do not         *
 * understand any portion of this notice, please seek appropriate     *
 * professional legal advice before making use of the software and    *
 * documents included in this software package.  In addition to       *
 * whatever other steps you may be obliged to take to respect the     *
 * intellectual property rights of the various parties involved, if   *
 * you do make use of the software and documents in this package,     *
 * please give credit where credit is due by citing this package,     *
 * its authors and the URL or other source from which you obtained    *
 * it, or equivalent primary references in the literature with the    *
 * same authors.                                                      *
 *                                                                    *
 * Some of the software and documents included within this software   *
 * package are the intellectual property of various parties, and      *
 * placement in this package does not in any way imply that any       *
 * such rights have in any way been waived or diminished.             *
 *                                                                    *
 * With respect to any software or documents for which a copyright    *
 * exists, ALL RIGHTS ARE RESERVED TO THE OWNERS OF SUCH COPYRIGHT.   *
 *                                                                    *
 * Even though the authors of the various documents and software      *
 * found here have made a good faith effort to ensure that the        *
 * documents are correct and that the software performs according     *
 * to its documentation, and we would greatly appreciate hearing of   *
 * any problems you may encounter, the programs and documents any     *
 * files created by the programs are provided **AS IS** without any   *
 * warranty as to correctness, merchantability or fitness for any     *
 * particular or general use.                                         *
 *                                                                    *
 * THE RESPONSIBILITY FOR ANY ADVERSE CONSEQUENCES FROM THE USE OF    *
 * PROGRAMS OR DOCUMENTS OR ANY FILE OR FILES CREATED BY USE OF THE   *
 * PROGRAMS OR DOCUMENTS LIES SOLELY WITH THE USERS OF THE PROGRAMS   *
 * OR DOCUMENTS OR FILE OR FILES AND NOT WITH AUTHORS OF THE          *
 * PROGRAMS OR DOCUMENTS.                                             *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 *                           The IUCr Policy                          *
 *      for the Protection and the Promotion of the STAR File and     *
 *     CIF Standards for Exchanging and Archiving Electronic Data     *
 *                                                                    *
 * Overview                                                           *
 *                                                                    *
 * The Crystallographic Information File (CIF)[1] is a standard for   *
 * information interchange promulgated by the International Union of  *
 * Crystallography (IUCr). CIF (Hall, Allen & Brown, 1991) is the     *
 * recommended method for submitting publications to Acta             *
 * Crystallographica Section C and reports of crystal structure       *
 * determinations to other sections of Acta Crystallographica         *
 * and many other journals. The syntax of a CIF is a subset of the    *
 * more general STAR File[2] format. The CIF and STAR File approaches *
 * are used increasingly in the structural sciences for data exchange *
 * and archiving, and are having a significant influence on these     *
 * activities in other fields.                                        *
 *                                                                    *
 * Statement of intent                                                *
 *                                                                    *
 * The IUCr's interest in the STAR File is as a general data          *
 * interchange standard for science, and its interest in the CIF,     *
 * a conformant derivative of the STAR File, is as a concise data     *
 * exchange and archival standard for crystallography and structural  *
 * science.                                                           *
 *                                                                    *
 * Protection of the standards                                        *
 *                                                                    *
 * To protect the STAR File and the CIF as standards for              *
 * interchanging and archiving electronic data, the IUCr, on behalf   *
 * of the scientific community,                                       *
 *                                                                    *
 * * holds the copyrights on the standards themselves,                *
 *                                                                    *
 * * owns the associated trademarks and service marks, and            *
 *                                                                    *
 * * holds a patent on the STAR File.                                 *
 *                                                                    *
 * These intellectual property rights relate solely to the            *
 * interchange formats, not to the data contained therein, nor to     *
 * the software used in the generation, access or manipulation of     *
 * the data.                                                          *
 *                                                                    *
 * Promotion of the standards                                         *
 *                                                                    *
 * The sole requirement that the IUCr, in its protective role,        *
 * imposes on software purporting to process STAR File or CIF data    *
 * is that the following conditions be met prior to sale or           *
 * distribution.                                                      *
 *                                                                    *
 * * Software claiming to read files written to either the STAR       *
 * File or the CIF standard must be able to extract the pertinent     *
 * data from a file conformant to the STAR File syntax, or the CIF    *
 * syntax, respectively.                                              *
 *                                                                    *
 * * Software claiming to write files in either the STAR File, or     *
 * the CIF, standard must produce files that are conformant to the    *
 * STAR File syntax, or the CIF syntax, respectively.                 *
 *                                                                    *
 * * Software claiming to read definitions from a specific data       *
 * dictionary approved by the IUCr must be able to extract any        *
 * pertinent definition which is conformant to the dictionary         *
 * definition language (DDL)[3] associated with that dictionary.      *
 *                                                                    *
 * The IUCr, through its Committee on CIF Standards, will assist      *
 * any developer to verify that software meets these conformance      *
 * conditions.                                                        *
 *                                                                    *
 * Glossary of terms                                                  *
 *                                                                    *
 * [1] CIF:  is a data file conformant to the file syntax defined     *
 * at http://www.iucr.org/iucr-top/cif/spec/index.html                *
 *                                                                    *
 * [2] STAR File:  is a data file conformant to the file syntax       *
 * defined at http://www.iucr.org/iucr-top/cif/spec/star/index.html   *
 *                                                                    *
 * [3] DDL:  is a language used in a data dictionary to define data   *
 * items in terms of "attributes". Dictionaries currently approved    *
 * by the IUCr, and the DDL versions used to construct these          *
 * dictionaries, are listed at                                        *
 * http://www.iucr.org/iucr-top/cif/spec/ddl/index.html               *
 *                                                                    *
 * Last modified: 30 September 2000                                   *
 *                                                                    *
 * IUCr Policy Copyright (C) 2000 International Union of              *
 * Crystallography                                                    *
 **********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>

#include "cbf.h"
#include "cbf_simple.h"
#include "img.h"
#include "cbf_string.h"
#include "cbf_copy.h"
#include "cbf_hdf5.h"
#include "cbf_getopt.h"

#define C2CBUFSIZ 8192

#ifdef __MINGW32__
#define NOMKSTEMP
#define NOTMPDIR
#endif


int main (int argc, char *argv [])
{
    FILE *file = NULL;
    clock_t a,b;
    cbf_handle cif;
    cbf_getopt_handle opts;
    int c = 0;
    int errflg = 0;
	size_t cifid = 0;
	size_t f = 0;
	int h5_write_flags = 0;
	cbf_h5handle h5out = NULL;
	const char ** const cifin = memset(malloc(argc*sizeof(char*)),0,argc*sizeof(char*));
	const char *hdf5out = NULL;
	const char *config = NULL;
    char *ciftmp=NULL;
#ifndef NOMKSTEMP
    int ciftmpfd;
#endif
    int ciftmpused = 0;
    int nbytes;
    char buf[C2CBUFSIZ];
    cbf_hdf5_configItemVectorhandle vec = cbf_hdf5_createConfigItemVector();

    int digest = 0;

    const char * optarg = NULL;

	cbf_onfailnez(cbf_make_getopt_handle(&opts),free(cifin));

	cbf_onfailnez(cbf_getopt_parse(opts, argc, argv,
                                   "c(config):"
                                   "o(output):"
                                   "z(compression):"),free(cifin));

	if (!cbf_rewind_getopt_option(opts)) {
        for(; !cbf_get_getopt_data(opts,&c,NULL,NULL,&optarg); cbf_next_getopt_option(opts)) {
            switch (c) {
				case 'o': { /* output file */
					if (hdf5out) errflg++;
                    else hdf5out = optarg;
                    break;
				}
				case 'c': { // config file
					if (config) errflg++;
					else config = optarg;
					break;
				}
                case 'z': { // compression
					if (!strcmp("zlib",optarg?optarg:"")) h5_write_flags |= CBF_H5_ZLIB;
					else if (!strcmp("none",optarg?optarg:"")) h5_write_flags &= ~CBF_H5_ZLIB;
					else ++errflg;
					break;
				}
				case 0: { /* input file */
					if (NULL != optarg) cifin[cifid++] = optarg;
					break;
				}
				default: {
                    errflg++;
                    break;
				}
            }
        }
	}
	if (errflg || 0==cifid) {
		fprintf(stderr,"Usage: %s [-z|--compression zlib|none] [-c config_file] [-o output_nexus] input_minicbf ...\n", argv[0]);
        exit(2);
    }
    
	// parse the config file
	{
		int parseError = CBF_SUCCESS;
		FILE * const configFile = fopen(config, "r");
		parseError = cbf_hdf5_parseConfig(configFile, stderr, vec);
		fclose(configFile);
		if (CBF_SUCCESS != parseError) {
			fprintf(stderr, "config parsing error: %s\n", cbf_hdf5_configParseStrerror(parseError));
			cbf_hdf5_destroyConfigItemVector(vec);
			exit(1);
		}
	}
    
	// prepare the output file
	if(cbf_create_h5handle2(&h5out,hdf5out)) printf ("Couldn't open the HDF5 file '%s'.\n", hdf5out);
	h5out->nxid = H5Gcreate_anon(h5out->hfile,H5P_DEFAULT,H5P_DEFAULT);
	cbf_H5Arequire_string(h5out->nxid,"NX_class","NXentry");

	for (f = 0; f != cifid; ++f) {

		/* Read the minicbf */

		if (!(cifin[f]) || strcmp(cifin[f]?cifin[f]:"","-") == 0) {
			ciftmp = (char *)malloc(strlen("/tmp/cif2cbfXXXXXX")+1);
	#ifdef NOTMPDIR
			strcpy(ciftmp, "cif2cbfXXXXXX");
	#else
			strcpy(ciftmp, "/tmp/cif2cbfXXXXXX");
	#endif
	#ifdef NOMKSTEMP
			if ((ciftmp = mktemp(ciftmp)) == NULL ) {
				fprintf(stderr,"%s: Can't create temporary file name %s.\n%s\n", argv[0], ciftmp,strerror(errno));
				exit(1);
			}
			if ( (file = fopen(ciftmp,"wb+")) == NULL) {
				fprintf(stderr,"Can't open temporary file %s.\n%s\n", ciftmp,strerror(errno));
				exit(1);
			}
	#else
			if ((ciftmpfd = mkstemp(ciftmp)) == -1 ) {
				fprintf(stderr,"%s: Can't create temporary file %s.\n%s\n", argv[0], ciftmp,strerror(errno));
				exit(1);
			}
			if ( (file = fdopen(ciftmpfd, "w+")) == NULL) {
				fprintf(stderr,"Can't open temporary file %s.\n%s\n", ciftmp,strerror(errno));
				exit(1);
			}
	#endif
			while ((nbytes = fread(buf, 1, C2CBUFSIZ, stdin))) {
				if(nbytes != fwrite(buf, 1, nbytes, file)) {
					fprintf(stderr,"Failed to write %s.\n", ciftmp);
					exit(1);
				}
			}
			fclose(file);
			cifin[f] = ciftmp;
			ciftmpused = 1;
		}


		// start timing
		a = clock ();
		{ // do the work
			// prepare the file
			FILE * const in = fopen (cifin[f], "rb");
			if (NULL == in) {
				fprintf (stderr,"Couldn't open the input CIF file '%s': %s\n", cifin[f], strerror(errno));
				exit (1);
			}
			// ensure temporary file is removed when the program closes
			if (ciftmpused) {
				if (unlink(ciftmp)) {
					fprintf(stderr,"Can't unlink temporary file '%s': %s\n", ciftmp,strerror(errno));
					exit(1);
				}
			}
			// make the handle
			if ( cbf_make_handle (&cif) ) {
				fprintf(stderr,"Failed to create handle for input_cif\n");
				exit(1);
			}
			// read the file
			cbf_onfailnez(cbf_read_file(cif, in, MSG_DIGEST|(digest&MSG_DIGESTWARN)),free(cifin));
		}
		// stop timing
		b = clock ();
		fprintf(stderr, "Time to read '%s': %.3fs\n", cifin[f], ((float)(b - a))/CLOCKS_PER_SEC);

		// start timing
		a = clock ();
		{ // do the work
			int cbfError = CBF_SUCCESS;
			h5out->slice = f;
				// convert to nexus format
				cbfError = cbf_write_minicbf_h5file(cif, h5out, vec, h5_write_flags);
			cbf_onfailnez(cbfError,{cbf_free_handle(cif);free(cifin);});
		}
        
		cbf_free_handle(cif);
        
		// stop timing
		b = clock ();
		fprintf(stderr, "Time to convert the data: %.3fs\n", ((float)(b - a))/CLOCKS_PER_SEC);

	}
    
	cbf_hdf5_destroyConfigItemVector(vec);
    
	{ // write the file
		// start timing
		a = clock ();
		H5Olink(h5out->nxid,h5out->hfile,"entry",H5P_DEFAULT,H5P_DEFAULT);
		// clean up cbf handles
		cbf_free_h5handle(h5out);
		// stop timing
		b = clock ();
		fprintf(stderr, "Time to write '%s': %.3fs\n", hdf5out, ((float)(b - a))/CLOCKS_PER_SEC);
	}


	// cleanup
	free(cifin);
	cbf_failnez(cbf_free_getopt_handle(opts));
    exit(0);
}
