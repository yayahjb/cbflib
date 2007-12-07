/**********************************************************************
 *          cif2c -- convert a cif to a CBFlib function               *
 *                                                                    *
 * Version 0.7.8 12 June 2007                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006 Herbert J. Bernstein                            *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
 *                                                                    *                                                                    *
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



/**********************************************************************
 *                                SYNOPSIS                            *
 *                                                                    *
 *  cif2c [-i input_cif] [-o output_C_function] \                     *
 *    [-n name_of_function]                 \                         *
 *    [input_cif] [output_C_function]                                 *
 *                                                                    *
 *  the options are:                                                  *
 *                                                                    *
 *  -i input_cif (default: stdin)                                     *
 *    the input  file in CIF or CBF  format.  If input_cif is not     *
 *    specified or is given as "-", it is copied from stdin to a      *
 *    temporary file.                                                 *
 *                                                                    *
 *  -o output_C_function (default: stdout)                            *
 *    the file that will receive the code of a C function that will   *
 *    regenerate the given cif, replacing binary text fields with     *
 *    "."  If no output_C_code is specified or is given as "-",       *
 *    the output is written to stdout                                 *
 *                                                                    *
 *  -n name_of_function (default: cbf_create_template)                *
 *    the name of the function that will be written                   *
 *                                                                    *                                                                    *
 *  The function that is created has a cbf handle as it argument      *
 *  and returns an int value, which will be 0 for normal completion.  *
 *                                                                    *
 *  Binary sections are not recreated by the C function               *
 *                                                                    *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                CREDITS                             *
 *                                                                    *
 *  This program is a Crystallographic Information File (CIF)         *
 *  application.  Please see the IUCR Policy below.   See the IUCR    *
 *  web page (http://www.iucr.org) or its mirrors for background      *
 *  and references on CIF.                                            *
 *                                                                    *
 *  This program is a Crystallographic Binary File (CBF) application. *
 *  Please see the ImgCIF/CBF web page at                             *
 *                                                                    *
 *            http://ndbserver.rutgers.edu/mmcif/cbf                  *
 *                                                                    *
 *  for background and references.  The CBF definition is available   *
 *  on the web page created by Andy Hammersley at                     *
 *                                                                    *
 *     http://www.ersf.fr/computing/Forum/imgCIF/cbf_definition.html  *
 *                                                                    *
 *  This program is a CBFlib application.  See "CBFLIB, An ANSI-C     *
 *  API for Crystallographic Binary Files", Version 0.1, April 1998   *
 *  by Paul J. Ellis, Stanford Synchrotron Radiation Laboratory,      *
 *  ellis@ssrl.slac.stanford.edu                                      *
 *                                                                    *
 *  This program uses routines derived from mpack/munpack version     *
 *  1.5, ftp://ftp.andrew.cmu.edu/pub/mpack by John G. Myers,         *
 *  jgm+@cmu.edu.  "Mpack and munpack are utilties for encoding and   *
 *  decoding ... binary files in MIME ... format."  Please see the    *
 *  copyright notices and disclaimers in the mpack/munpack routines   *
 *                                                                    *
 *  This program uses routines derived from the "RSA Data Security,   *
 *  Inc. MD5 Message-Digest Algorithm."  Please see the copyright     *
 *  notice and disclaimer in md5c.c                                   *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term ‘this software’, as used in these Notices, refers to      *
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
 *                                 NOTICE                             *
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

#include "cbf.h"
#include "img.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>

#define C2CBUFSIZ 8192

#ifdef __MINGW32__
#define NOMKSTEMP
#define NOTMPDIR
#endif


int local_exit (int status);

#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr,"CBFlib fatal error %d\n",err); \
    local_exit (-1); \
  } \
 }

void set_MP_terms(int crterm, int nlterm);

int main (int argc, char *argv [])
{
  FILE *in, *out, *file;
  clock_t a,b;
  cbf_handle cif;
  cbf_handle cbf;
  int c;
  int errflg = 0;
  char *cifin, *codeout, *function_name;
  char ciftmp[19];
#ifdef NOMKSTEMP
  char *xciftmp;
#endif
#ifndef NOMKSTEMP
  int ciftmpfd;
#endif
  int ciftmpused;
  unsigned int nbytes;
  char buf[C2CBUFSIZ];
  char ovalue[C2CBUFSIZ*8];
  unsigned int blocks, categories, blocknum, catnum;
  const char *datablock_name;
  const char *category_name;
  const char *column_name;
  const char *value;
  unsigned int colnum, rownum;
  unsigned int columns;
  unsigned int rows;



     /* Extract options */

/**********************************************************************
 *  cif2c [-i input_cif] [-o output_C_function] \                   *
 *    [-n {name_of_function] \                                        *
 *    [input_cif] [output_cbf]                                        *
 *                                                                    *
 **********************************************************************/


   cifin = NULL;
   codeout = NULL;
   function_name = NULL;
   ciftmpused = 0;

   while ((c = getopt(argc, argv, "i:o:n:")) != EOF) {
     switch (c) {
       case 'i':
         if (cifin) errflg++;
         else cifin = optarg;
         break;
       case 'o':
         if (codeout) errflg++;
         else codeout = optarg;
         break;
       case 'n':
         if (function_name) errflg++;
         else function_name = optarg;
         break;
       default:
         errflg++;
         break;
      }
    }
   for (; optind < argc; optind++) {
     if (!cifin) {
        cifin = argv[optind];
     } else {
       if (!codeout) {
         codeout = argv[optind];
       } else {
         errflg++;
       }
     }
   }
   if (errflg) {
     fprintf(stderr,"cif2c:  Usage: \n");
     fprintf(stderr,
       "  cif2cbf [-i input_cif] [-o output_C_function] \\\n");
     fprintf(stderr,
       "    [-n name_of_function] \\\n");
     fprintf(stderr,
       "    [input_cif] [output_C_function] \n\n");
     exit(2);
   }



    /* Read the cif */

   if (!cifin || strcmp(cifin?cifin:"","-") == 0) {
#ifdef NOTMPDIR
     strcpy(ciftmp, "cif2cXXXXXX");
#else
     strcpy(ciftmp, "/tmp/cif2cXXXXXX");
#endif
#ifdef NOMKSTEMP
     if ((xciftmp=mktemp(ciftmp)) == NULL ) {
       fprintf(stderr,"\n cif2c: Can't create temporary file name %s.\n", ciftmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     if ( (file = fopen(ciftmp,"wb+")) == NULL) {
       fprintf(stderr,"Can't open temporary file %s.\n", ciftmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);     	
     }
#else
     if ((ciftmpfd = mkstemp(ciftmp)) == -1 ) {
       fprintf(stderr,"Can't create temporary file %s.\n", ciftmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     if ( (file = fdopen(ciftmpfd, "w+")) == NULL) {
       fprintf(stderr,"Can't open temporary file %s.\n", ciftmp);
       fprintf(stderr,"%s\n",strerror(errno));
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
     cifin = ciftmp;
     ciftmpused = 1;
   }
   if ( cbf_make_handle (&cif) ) {
     fprintf(stderr,"Failed to create handle for input_cif\n");
     exit(1);
   }
   if ( cbf_make_handle (&cbf) ) {
     fprintf(stderr,"Failed to create handle for output_cbf\n");
     exit(1);
   }

   a = clock ();

   /* Read the file */
   if (!(in = fopen (cifin, "rb"))) {
     fprintf (stderr,"Couldn't open the input CIF file %s\n", cifin);
     exit (1);
   }

   if (ciftmpused) {
     if (unlink(ciftmp) != 0 ) {
       fprintf(stderr,"cif2cif:  Can't unlink temporary file %s.\n", ciftmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
   }

   cbf_failnez (cbf_read_file (cif, in, MSG_DIGEST))

   /* Prepare the output file */

   if ( ! codeout || strcmp(codeout?codeout:"","-") == 0 ) {
     out = stdout;
   } else {
     out = fopen (codeout, "w+b");
   }
   if ( ! out ) {
       printf (" Couldn't open the output code file %s\n", codeout);
       exit (1);
   }

   /* Start the code output */
   fprintf(out,"/* Code generated by cif2c */\n");
   fprintf(out,"#ifdef __cplusplus\n");
   fprintf(out,"extern \"C\" {\n\n");
   fprintf(out,"#endif\n\n");

   fprintf(out,"#include \"cbf.h\"\n");
   fprintf(out,"#include \"cbf_simple.h\"\n");

   if (!function_name) function_name = "cbf_create_template";

   fprintf(out,"int %s(cbf_handle handle) {\n\n",function_name);


   cbf_failnez (cbf_rewind_datablock(cif))

   cbf_failnez (cbf_count_datablocks(cif, &blocks))

   for (blocknum = 0; blocknum < blocks;  blocknum++ )
   { /* start of copy loop */


     cbf_failnez (cbf_select_datablock(cif, blocknum))
     cbf_failnez (cbf_datablock_name(cif, &datablock_name))
     fprintf(out,"  cbf_failnez (cbf_force_new_datablock(handle, \"%s\"))\n", datablock_name);

     if ( !cbf_rewind_category(cif) ) {
     cbf_failnez (cbf_count_categories(cif, &categories))

     for (catnum = 0; catnum < categories;  catnum++) {
       cbf_select_category(cif, catnum);
       cbf_category_name(cif,&category_name);
       fprintf(out,"\n  cbf_failnez (cbf_force_new_category(handle,\"%s\"))\n", category_name);
       cbf_count_rows(cif,&rows);
       cbf_count_columns(cif,&columns);

       /*  Transfer the columns names from cif to cbf */
       if ( ! cbf_rewind_column(cif) ) {
       do {
         cbf_failnez(cbf_column_name(cif, &column_name))
         fprintf(out,"    cbf_failnez (cbf_require_column(handle,\"%s\"))\n", column_name);
       } while ( ! cbf_next_column(cif) );
       cbf_rewind_column(cif);
       cbf_rewind_row(cif);
       }
       /* Transfer the rows from cif to cbf */
       for (rownum = 0; rownum < rows; rownum++ ) {
         cbf_failnez (cbf_select_row(cif, rownum))
         fprintf(out,"    cbf_failnez (cbf_new_row(handle))\n");
         cbf_rewind_column(cif);
         for (colnum = 0; colnum < columns; colnum++ ) {
           const char *typeofvalue;

           cbf_failnez (cbf_select_column(cif, colnum))
           cbf_failnez (cbf_column_name(cif, &column_name))
           if ( ! cbf_get_value(cif, &value) ) {
             int ipos, opos, skip;
             cbf_failnez (cbf_get_typeofvalue(cif, &typeofvalue))
             ipos = 0;
             opos = 0;
             while(value[ipos]) {
               skip = 0;
               switch (value[ipos]) {
                 case '"':
                 case '\\':
                   ovalue[opos++] = '\\'; break;
                 case '\t':
                   ovalue[opos++] = '\\';
                   ovalue[opos++] = 't';
                   skip = 1;
                   break;
                 case '\n':
                   ovalue[opos++] = '\\';
                   ovalue[opos++] = 'n';
                   skip = 1;
                   break;
               }
               if ( !skip && ((unsigned char)(value[ipos]) < 32 || (unsigned char)(value[ipos]) >= 127)) {
                  sprintf(ovalue+opos,"\\0x%03o",(unsigned char)value[ipos]);
                  opos+= 6;
                  skip = 1;
               }
               if ( !skip ) ovalue[opos++] = value[ipos++];
               else ipos++;
             }
             ovalue[opos] = '\0';
             fprintf(out,"    cbf_failnez (cbf_find_column(handle, \"%s\"))\n", column_name);
             fprintf(out,"    cbf_failnez (cbf_set_value(handle, \"%s\"))\n", ovalue);
             fprintf(out,"    cbf_failnez (cbf_set_typeofvalue(handle, \"%s\"))\n", typeofvalue);
            } else {
             fprintf(out,"    cbf_failnez (cbf_find_column(handle, \"%s\"))\n", column_name);
             fprintf(out,"    cbf_failnez (cbf_set_value(handle, \".\"))\n"); 
             fprintf(out,"    cbf_failnez (cbf_set_typeofvalue(handle, \"%s\"))\n", "null");
            }
         }
       }

     }
     }
   }

   fprintf(out,"  return 0;\n\n}\n");
   fprintf(out,"#ifdef __cplusplus\n\n");

   fprintf(out,"}\n");

   fprintf(out,"#endif\n");


   b = clock ();
   fprintf (stderr,
     " Time to read input_cif: %.3fs\n",
       ((b - a) * 1.0) / CLOCKS_PER_SEC);
   a = clock ();



   cbf_failnez (cbf_free_handle (cbf))

   b = clock ();
   fprintf (stderr, " Time to write the code: %.3fs\n",
       ((b - a) * 1.0) / CLOCKS_PER_SEC);

   exit(0);

}

int local_exit (int status)
{
  exit(status);
  return 1; /* avoid warnings */
}
