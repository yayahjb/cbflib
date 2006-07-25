/**********************************************************************
 *          cif2cbf -- convert a cif to a cbf file                    *
 *          Version 0.6 26 December 1998                               *
 *                                                                    *
 *          Herbert J. Bernstein, Bernstein + Sons                    *
 *          P.O. Box 177, Bellport, NY 11713                          *
 *          yaya@bernstein-plus-sons.com                              *
 **********************************************************************/
 
/**********************************************************************
 *                                SYNOPSIS                            *
 *                                                                    *
 *  cif2cbf [-i input_cif] [-o output_cbf] \                          *
 *    [-c {p[acked]|c[annonical]|[n[one]}] \                          *
 *    [-m {h[eaders]|n[oheaders]}] [-d {d[igest]|n[odigest]}] \       *
 *    [-e {b[ase64]|q[uoted-printable]| \                             *
 *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
 *    [-b {f[orward]|b[ackwards]}] \                                  *
 *    [input_cif] [output_cbf]                                        *
 *                                                                    *
 *  the options are:                                                  *
 *                                                                    *
 *  -i input_cif (default: stdin)                                     *
 *    the input  file in CIF or CBF  format.  If input_cif is not     *
 *    specified or is given as "-", it is copied from stdin to a      *
 *    temporary file.                                                 *
 *                                                                    *
 *  -o output_cbf (default: stdout)                                   *
 *    the output cif (if base64 or quoted-printable encoding is used) *
 *    or cbf (if no encoding is used).  if no output_cif is specified *
 *    or is given as "-", the output is written to stdout             *
 *                                                                    *
 *  The remaining options specify the characteristics of the          *
 *  output cbf.  The characteristics of the input cif are derived     *
 *  from context.                                                     *
 *                                                                    *
 *  -c compression_scheme (packed, canonical or none,                 *
 *    default packed)                                                 *
 *                                                                    *
 *  -m [no]headers (default headers for cifs, noheaders for cbfs)     *
 *    selects MIME (N. Freed, N. Borenstein, RFC 2045, November 1996) *
 *    headers within binary data value text fields.                   *
 *                                                                    *
 *  -d [no]digest  (default md5 digest [R. Rivest, RFC 1321, April    *
 *    1992 using"RSA Data Security, Inc. MD5 Message-Digest           *
 *    Algorithm"] when MIME headers are selected)                     *
 *                                                                    *
 *  -e encoding (base64, quoted-printable or none, default base64)    *
 *    specifies one of the standard MIME encodings for an ascii cif   *
 *    or "none" for a binary cbf                                      *
 *                                                                    *
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
 *                             The IUCr Policy                        *
 *                                    on                              *
 *     the Use of the Crystallographic Information File (CIF)         *
 *                                                                    *
 * The Crystallographic Information File (Hall, Allen & Brown,        *
 * 1991) is, as of January 1992, the recommended method for           *
 * submitting publications to Acta Crystallographica Section C. The   *
 * International Union of Crystallography holds the Copyright on      *
 * the CIF, and has applied for Patents on the STAR File syntax       *
 * which is the basis for the CIF format.                             *
 *                                                                    *
 * It is a principal objective of the IUCr to promote the use of      *
 * CIF for the exchange and storage of scientific data. The IUCr's    *
 * sponsorship of the CIF development was motivated by its            *
 * responsibility to its scientific journals, which set the           *
 * standards in crystallographic publishing. The IUCr intends that    *
 * CIFs will be used increasingly for electronic submission of        *
 * manuscripts to these journals in future. The IUCr recognises       *
 * that, if the CIF and the STAR File are to be adopted as a means    *
 * for universal data exchange, the syntax of these files must be     *
 * strictly and uniformly adhered to. Even small deviations from      *
 * the syntax would ultimately cause the demise of the universal      *
 * file concept. Through its Copyrights and Patents the IUCr has      *
 * taken the steps needed to ensure strict conformance with this      *
 * syntax.                                                            *
 *                                                                    *
 * The IUCr policy on the use of the CIF and STAR File processes is   *
 * as follows:                                                        *
 * _________________________________________________________________  *
 *                                                                    *
 *  * 1 CIFs and STAR Files may be generated, stored or transmitted,  *
 *    without permission or charge, provided their purpose is not     *
 *    specifically for profit or commercial gain, and provided that   *
 *    the published syntax is strictly adhered to.                    *
 *  * 2 Computer software may be developed for use with CIFs or STAR  *
 *    files, without permission or charge, provided it is distributed *
 *    in the public domain. This condition also applies to software   *
 *    for which a charge is made, provided that its primary function  *
 *    is for use with files that satisfy condition 1 and that it is   *
 *    distributed as a minor component of a larger package of         *
 *    software.                                                       *
 *  * 3 Permission will be granted for the use of CIFs and STAR Files *
 *    for specific commercial purposes (such as databases or network  *
 *    exchange processes), and for the distribution of commercial     *
 *    CIF/STAR software, on written application to the IUCr Executive *
 *    Secretary, 2 Abbey Square, Chester CH1 2HU, England. The        *
 *    nature, terms and duration of the licences granted will be      *
 *    determined by the IUCr Executive and Finance Committees.        *
 *                                                                    *
 * _________________________________________________________________  *
 *                                                                    *
 * In summary, the IUCr wishes to promote the use of the STAR File    *
 * concepts as a standard universal data file. It will insist on      *
 * strict compliance with the published syntax for all                *
 * applications. To assist with this compliance, the IUCr provides    *
 * public domain software for checking the logical integrity of a     *
 * CIF, and for validating the data name definitions contained        *
 * within a CIF. Detailed information on this software, and the       *
 * associated dictionaries, may be obtained from the IUCr Office at   *
 * 5 Abbey Square, Chester CH1 2HU, England.                          *
 **********************************************************************/

#include "cbf.h"
#include "img.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>

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

char *tmpnam(char *s);

int main (int argc, char *argv [])
{
  FILE *in, *out, *file;
  clock_t a,b;
  cbf_handle cif;
  cbf_handle cbf;
  int id, index;
  size_t nelem_read;
  double pixel_size, gain, wavelength, distance;
  int overload, dimension [2], precedence [2];
  const char *detector;
  char *detector_char;
  char detector_id [64];
  const char *direction [2], *array_id;
  int c;
  int errflg = 0;
  char *cifin, *cbfout, *ciftmp;
  char tmpbuf[L_tmpnam];
  int nbytes;
  char buf[2048];
  unsigned int blocks, categories, blocknum, catnum;
  const char *datablock_name;
  const char *category_name;
  const char *column_name;
  const char *value;
  unsigned int colnum, rownum;
  unsigned int columns;
  unsigned int rows;

  int mime, digest, encoding, compression, bytedir, cbforcif, term;
  
  
     /* Extract options */

/********************************************************************** 
 *  cif2cbf [-i input_cif] [-o output_cbf] \                          *
 *    [-c {p[acked]|c[annonical]|[n[one]}] \                          *
 *    [-m {h[eaders]|n[oheaders]}] [-d {d[igest]|n[odigest]}]  \      *
 *    [-e {b[ase64]|q[uoted-printable]| \                             *
 *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
 *    [-b {f[orward]|b[ackwards]}] \                                  *
 *    [input_cif] [output_cbf]                                        *
 *                                                                    *
 **********************************************************************/ 
  
   mime = 0;
   digest = 0;
   encoding = 0;
   compression = 0;
   bytedir = 0;
     
   cifin = NULL;
   cbfout = NULL;
   ciftmp = NULL;

   while ((c = getopt(argc, argv, "i:o:c:m:d:e:b:")) != EOF) {
     switch (c) {
       case 'i':
         if (cifin) errflg++;
         else cifin = optarg;
         break;
       case 'o':
         if (cbfout) errflg++;
         else cbfout = optarg;
         break;
       case 'c':
         if (compression) errflg++;
         if (optarg[0] == 'p' || optarg[0] == 'P') {
           compression = CBF_PACKED;
         } else {
           if (optarg[0] == 'c' || optarg[0] == 'C') {
             compression = CBF_CANONICAL;
           } else {
             if (optarg[0] == 'n' || optarg[0] == 'N') {
             compression = CBF_NONE;
             } else {
               errflg++;
             }
           }
         }
         break;
       case 'm':
         if (mime) errflg++;
         if (optarg[0] == 'h' || optarg[0] == 'H' ) {
           mime = MIME_HEADERS;
         } else {
           if (optarg[0] == 'n' || optarg[0] == 'N' ) {
           mime = PLAIN_HEADERS;
           } else {
             errflg++;
           }
         }
         break;
       case 'd':
         if (digest) errflg++;
         if (optarg[0] == 'd' || optarg[0] == 'H' ) {
           digest = MSG_DIGEST;
         } else {
           if (optarg[0] == 'n' || optarg[0] == 'N' ) {
           digest = MSG_NODIGEST;
           } else {
             errflg++;
           }
         }
         break;
       case 'b':
        if (bytedir) errflg++;
        if (optarg[0] == 'f' || optarg[0] == 'F') {
          bytedir = ENC_FORWARD;
        } else {
          if (optarg[0] == 'b' || optarg[0] == 'B' ) {
            bytedir = ENC_BACKWARD;
          } else {
            errflg++;
          }
        }
        break;
       case 'e':
         if (encoding) errflg++;
         if (optarg[0] == 'b' || optarg[0] == 'B' ) {
           encoding = ENC_BASE64;
         } else {
           if (optarg[0] == 'q' || optarg[0] == 'Q' ) {
             encoding = ENC_QP;
           } else {
             if (optarg[0] == 'd' || optarg[0] == 'D' ) {
               encoding = ENC_BASE10;
             } else {
               if (optarg[0] == 'h' || optarg[0] == 'H' ) {
                 encoding = ENC_BASE16;
               } else {
                 if (optarg[0] == 'o' || optarg[0] == 'O' ) {
                   encoding = ENC_BASE8;
                 } else {
                   if (optarg[0] == 'n' || optarg[0] == 'N' ) {
                     encoding = ENC_NONE;
                   } else {
                     errflg++;
                   }
                 }
               }
             }
           }
         }
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
       if (!cbfout) {
         cbfout = argv[optind];
       } else {
         errflg++;
       }
     }
   }
   if (errflg) {
     fprintf(stderr,"cif2cbf:  Usage: \n");
     fprintf(stderr,
       "  cif2cbf [-i input_cif] [-o output_cbf] \\\n");
     fprintf(stderr,
       "    [-c {p[acked]|c[annonical]|[n[one]}] \\\n");
     fprintf(stderr,
       "    [-m {h[eaders]|n[oheaders]}] [-d {d[igest]|n[odigest]}] \\\n");
     fprintf(stderr,
       "    [-e {b[ase64]|q[uoted-printable]|\\\n");
     fprintf(stderr,
       "                  d[ecimal]|h[examdecimal|o[ctal]|n[one]}] \\\n");
     fprintf(stderr,
       "    [-w {2|3|4|6|8}] [-b {f[orward]|b[ackwards]}\\\n");
     fprintf(stderr,
       "    [input_cif] [output_cbf] \n\n");
     exit(2);
   }
   
  
     /* Set up for CIF of CBF output */
  
   if (!encoding) { 
     encoding = ENC_BASE64;
   }
   cbforcif = CBF;
   term = ENC_CRTERM | ENC_LFTERM;
   if (encoding == ENC_BASE64 || \
       encoding == ENC_QP || \
       encoding == ENC_BASE10 || \
       encoding == ENC_BASE16 || \
       encoding == ENC_BASE8) {
     cbforcif = CIF;
     term = ENC_LFTERM;
   }
    
     /* Set up for headers */
  
   if (!mime) {
     mime = MIME_HEADERS;
   }
   if (!digest) {
     if (mime == MIME_HEADERS) {
       digest = MSG_DIGEST;
     } else {
       digest = MSG_NODIGEST;
     }
   }

     /* Set up for decimal, hexadecimal or octal output */
  if (!bytedir)
     bytedir = ENC_BACKWARD;

     /* Set up for Compression */
  
   if (!compression) 
     compression = CBF_PACKED;


    /* Read the cif */
  
   if (!cifin || strcmp(cifin?cifin:"","-") == 0) {
     ciftmp = strdup(tmpnam(&tmpbuf[0]));
     if ( (file = fopen(ciftmp, "w+")) == NULL) {
       fprintf(stderr,"Can't open temporary file %s.\n", ciftmp);
       exit(1);
     }
     while (nbytes = fread(buf, 1, 1024, stdin)) {
       if(nbytes != fwrite(buf, 1, nbytes, file)) {
         fprintf(stderr,"Failed to write %s.\n", ciftmp);
         exit(1);
       }
     }
     fclose(file);
     cifin = ciftmp;
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

   cbf_failnez (cbf_read_file (cif, in, MSG_DIGEST))
   cbf_failnez (cbf_rewind_datablock(cif))

   cbf_failnez (cbf_count_datablocks(cif, &blocks))

   for (blocknum = 0; blocknum < blocks;  blocknum++ )
   { /* start of copy loop */
   
 
     cbf_failnez (cbf_select_datablock(cif, blocknum))
     cbf_failnez (cbf_datablock_name(cif, &datablock_name))
     cbf_failnez (cbf_force_new_datablock(cbf, datablock_name))

     if ( !cbf_rewind_category(cif) ) {
     cbf_failnez (cbf_count_categories(cif, &categories))

     for (catnum = 0; catnum < categories;  catnum++) {
       cbf_select_category(cif, catnum);
       cbf_category_name(cif,&category_name);
       cbf_force_new_category(cbf, category_name);
       cbf_count_rows(cif,&rows);
       cbf_count_columns(cif,&columns);

       /*  Transfer the columns names from cif to cbf */
       if ( ! cbf_rewind_column(cif) ) {
       do {
         cbf_failnez(cbf_column_name(cif, &column_name))
         cbf_failnez(cbf_new_column(cbf, column_name))
       } while ( ! cbf_next_column(cif) );
       cbf_rewind_column(cif);
       cbf_rewind_row(cif);
       }
       /* Transfer the rows from cif to cbf */
       for (rownum = 0; rownum < rows; rownum++ ) {
         cbf_failnez (cbf_select_row(cif, rownum))
         cbf_failnez (cbf_new_row(cbf))
         cbf_rewind_column(cif);
         for (colnum = 0; colnum < columns; colnum++ ) {
           cbf_failnez (cbf_select_column(cif, colnum))
           if ( ! cbf_get_value(cif, &value) ) {
             
             cbf_failnez (cbf_select_column(cbf, colnum))
             cbf_failnez (cbf_set_value(cbf, value))

           } else {

             void * array;
             int binary_id, elsigned, elunsigned;
             size_t elements,elements_read, elsize;
             int minelement, maxelement;
             unsigned int cifcompression;

             cbf_failnez(cbf_get_integerarrayparameters(
               cif, &cifcompression,
               &binary_id, &elsize, &elsigned, &elunsigned,
               &elements, &minelement, &maxelement))
	     if (array=malloc(elsize*elements)) {
               cbf_failnez (cbf_select_column(cbf,colnum))
               cbf_failnez (cbf_get_integerarray(
               cif, &binary_id, array, elsize, elsigned,
               elements, &elements_read))
               cbf_failnez(cbf_set_integerarray(
               cbf, compression,
               binary_id, array, elsize, elsigned, elements))
               free(array);
             } else {
               fprintf(stderr,
                 "\nFailed to allocate memory %d bytes",
                 elsize*elements); 
                exit(1);
             }
           }
         }
       }
       
     }
     }
   }

   b = clock ();
   fprintf (stderr, 
     " Time to read input_cif: %.3fs\n", 
       ((b - a) * 1.0) / CLOCKS_PER_SEC);
   a = clock ();

   if ( ! cbfout || strcmp(cbfout?cbfout:"","-") == 0 ) {
      out = stdout;
   } else {
     out = fopen (cbfout, "w+b");
   }
   if ( ! out ) {
     if (encoding == ENC_NONE) {
       printf (" Couldn't open the CBF file %s\n", cbfout);
     } else {
       printf (" Couldn't open the CIF file %s\n", cbfout);
     }
     exit (1);
   }

   cbf_failnez (cbf_write_file (cbf, out, 1, cbforcif, mime | digest,
                                         encoding | bytedir | term))
 
   cbf_failnez (cbf_free_handle (cbf))

   b = clock ();
   if (encoding == ENC_NONE) {
     fprintf (stderr, " Time to write the CBF image: %.3fs\n", 
       ((b - a) * 1.0) / CLOCKS_PER_SEC); 
   } else {
     fprintf (stderr, " Time to write the CIF image: %.3fs\n", 
       ((b - a) * 1.0) / CLOCKS_PER_SEC); 
   }
  
   exit(0);
   
}

int local_exit (int status)
{
  exit(status);
  return 1; /* avoid warnings */
}
