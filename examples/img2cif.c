/**********************************************************************
 *          img2cif -- convert an image file to a cif file            *
 *                                                                    *
 * Version 0.7.6 28 June 2006                                         *
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
 *  img2cif [-i input_image] [-o output_cif] \                        *
 *    [-c {p[acked]|c[annonical]|[n[one]}] \                          *
 *    [-m {h[eaders]|n[oheaders]}] [-d {d[igest]|n[odigest]}] \       *
 *    [-e {b[ase64]|q[uoted-printable]| \                             *
 *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
 *    [-b {f[orward]|b[ackwards]}] \                                  *
 *    [input_image] [output_cif]                                      *
 *                                                                    *
 *  the options are:                                                  *
 *                                                                    *
 *  -i input_image (default: stdin)                                   *
 *    the input_image file in MAR300, MAR345 or ADSC CCD detector     *
 *    format is given.  If no input_image file is specified or is     *
 *    given as "-", an image is copied from stdin to a temporary file.*
 *                                                                    *
 *  -o output_cif (default: stdout)                                   *
 *    the output cif (if base64 or quoted-printable encoding is used) *
 *    or cbf (if no encoding is used).  if no output_cif is specified *
 *    or is given as "-", the output is written to stdout             *
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
 *  -e encoding (base64, quoted-printable, decimal, hexadecimal,      *
 *    octal or none, default: base64) specifies one of the standard   *
 *    MIME encodings (base64 or quoted-printable) or a non-standard   *
 *    decimal, hexamdecimal or octal encoding for an ascii cif        *
 *    or "none" for a binary cbf                                      *
 *                                                                    *
 *  -b direction (forward or backwards, default: natural direction)   *
 *    specifies the direction of mapping of bytes into words          *
 *    for decimal, hexadecimal or octal output, marked by '>' for     *
 *    forward or '<' for backwards as the second character of each    *
 *    line of output, and in '#' comment lines.                       *
 *                                                                    *
 *                                                                    *
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
 * The term 'this software', as used in these Notices, refers to      *
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

#ifdef GNUGETOPT
#include "getopt.h"
#endif

#define I2CBUFSIZ 8192

#ifdef __MINGW32__
#define NOMKSTEMP
#define NOTMPDIR
#endif



#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr,"\nCBFlib fatal error %d \n",err); \
    exit(-1); \
  } \
 }


int main (int argc, char *argv [])
{
  FILE  *out, *file;
  clock_t a,b;
  img_handle img;
  cbf_handle cbf;
  double pixel_size, gain, wavelength, distance;
  int overload, dimension [2], precedence [2];
  const char *detector;
  char *detector_char;
  char detector_id [64];
  const char *direction [2];
  int c;
  int errflg = 0;
  char *imgin, *imgout;
  char *imgtmp=NULL;
#ifndef NOMKSTEMP
  int imgtmpfd;
#endif
  int imgtmpused;
  int nbytes;
  char buf[I2CBUFSIZ];

  int mime, digest, encoding, compression, bytedir, term, cbforcif;
    
  
     /* Extract options */

/********************************************************************** 
 *  img2cif [-i input_image] [-o output_cif] \                        *
 *    [-c {p[acked]|c[annonical]|[n[one]}] \                          *
 *    [-m {h[eaders]|n[oheaders]}] [-d {d[igest]|n[odigest]}]  \      *
 *    [-e {b[ase64]|q[uoted-printable]|\                              *
 *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
 *    [-w {2|3|4|6|8}] [-b {f[orward]|b[ackwards]}\                   *
 *    [input_image] [output_cif]                                      *
 *                                                                    *
 **********************************************************************/ 
  
 mime = 0;
 digest = 0;
 encoding = 0;
 compression = 0;
 bytedir = 0;
 
 imgin = NULL;
 imgout = NULL;
 imgtmpused = 0;
    
 while ((c = getopt(argc, argv, "i:o:c:m:d:e:b:")) != EOF)
 {
   switch (c) {
     case 'i':
       if (imgin) errflg++;
       else imgin = optarg;
       break;
     case 'o':
       if (imgout) errflg++;
       else imgout = optarg;
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
    if (!imgin) {
      imgin = argv[optind];
    } else {
      if (!imgout) {
        imgout = argv[optind];
      } else {
        errflg++;
      }
    }
  }
  if (errflg) {
    fprintf(stderr,"img2cif:  Usage: \n");
    fprintf(stderr,"  img2cif [-i input_image] [-o output_cif] \\\n");
    fprintf(stderr,"    [-c {p[acked]|c[annonical]|[n[one]}] \\\n");
    fprintf(stderr,"    [-m {h[eaders]|n[oheaders]}] [-d {d[igest]|n[odigest]}] \\\n");
    fprintf(stderr,"    [-e {b[ase64]|q[uoted-printable]|\\\n");
    fprintf(stderr,"                  d[ecimal]|h[examdecimal|o[ctal]|n[one]}] \\\n");
    fprintf(stderr,"    [-w {2|3|4|6|8}] [-b {f[orward]|b[ackwards]}\\\n");
    fprintf(stderr,"    [input_image] [output_cif] \n\n");
    exit(2);
  }

  
     /* Set up for CIF of CBF output */
  
  if (!encoding) 
    encoding = ENC_BASE64;
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
  
  if (!mime) 
    mime = MIME_HEADERS;
  if (!digest) {
     if (mime == MIME_HEADERS) {
       digest = MSG_DIGEST;
     } else {
       digest = MSG_NODIGEST;
     }
  }

     /* Set up for Compression */
  
  if (!compression) 
    compression = CBF_PACKED;


    /* Read the image */
  
  if (!imgin || strcmp(imgin?imgin:"","-") == 0) {
     imgtmp=(char *)malloc(strlen("/tmp/img2cifXXXXXX")+1);
#ifdef NOTMPDIR
     strcpy(imgtmp, "img2cifXXXXXX");
#else
     strcpy(imgtmp, "/tmp/img2cifXXXXXX");
#endif
#ifdef NOMKSTEMP
     if ((imgtmp = mktemp(imgtmp)) == NULL ) {
       fprintf(stderr,"\n img2cif: Can't create temporary file name %s.\n", imgtmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     if ( (file = fopen(imgtmp,"wb+")) == NULL) {
       fprintf(stderr,"Can't open temporary file %s.\n", imgtmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);     	
     }
#else
    if ((imgtmpfd = mkstemp(imgtmp)) == -1 ) {
      fprintf(stderr,"Can't create temporary file %s.\n", imgtmp);
      fprintf(stderr,"%s\n",strerror(errno));
      exit(1);
    }
    if ( (file = fdopen(imgtmpfd, "w+")) == NULL) {
      fprintf(stderr,"img2cif:  Can't open temporary file %s.\n", imgtmp);
      fprintf(stderr,"%s\n",strerror(errno));
      exit(1);
    }
#endif
    while ((nbytes = fread(buf, 1, 8192, stdin))) {
      if(nbytes != fwrite(buf, 1, nbytes, file)) {
        fprintf(stderr,"img2cif:  Failed to write %s.\n", imgtmp);
        exit(1);
      }
    }
    fclose(file);
    imgin = imgtmp;
    imgtmpused = 1;
  }

  img = img_make_handle ();

  a = clock ();

  cbf_failnez (img_read (img, imgin))
  if ( imgtmpused ) {
    if (unlink(imgtmp) != 0 ) {
      fprintf(stderr,"img2cif:  Can't unlink temporary file %s.\n", imgtmp);
      fprintf(stderr,"%s\n",strerror(errno));
      exit(1);
    }
  }

  b = clock ();

  fprintf (stderr, "img2cif:  Time to read the image: %.3fs\n", ((b - a) * 1.0) / CLOCKS_PER_SEC);


    /* Get some detector parameters */

    /* Detector identifier */

  detector = img_get_field (img, "DETECTOR");

  if (!detector)

    detector = "unknown";

  strncpy (detector_id, detector, 63);

  detector_id [63] = 0;

  detector_char = detector_id;

  while (*detector_char)

    if (isspace (*detector_char))

      memmove (detector_char, detector_char + 1, strlen (detector_char));

    else
    {
      *detector_char = tolower (*detector_char);

      detector_char++;
    }


    /* Pixel size */
    
  pixel_size = img_get_number (img, "PIXEL SIZE") * 0.001;


    /* Wavelength */

  wavelength = img_get_number (img, "WAVELENGTH");
  

    /* Distance */

  distance = img_get_number (img, "DISTANCE") * 0.001;
  

    /* Image size and orientation & gain and overload */

  if (strcmp (detector_id, "mar180") == 0 ||
      strcmp (detector_id, "mar300") == 0)
  {
    gain = 1.08;

    overload = 120000;

    dimension [0] = img_rows (img);
    dimension [1] = img_columns (img);

    precedence [0] = 1;
    precedence [1] = 2;

    direction [0] = "decreasing";
    direction [1] = "increasing";
  }
  else

    if (strcmp (detector_id, "mar345") == 0)
    {
      gain = 1.55;

      overload = 240000;

      dimension [0] = img_columns (img);
      dimension [1] = img_rows (img);

      precedence [0] = 2;
      precedence [1] = 1;

      direction [0] = "increasing";
      direction [1] = "increasing";
    }
    else

      if (strncmp (detector_id, "adscquantum", 11) == 0)
      {
        gain = 0.20;

        overload = 65000;

        dimension [0] = img_columns (img);
        dimension [1] = img_rows (img);

        precedence [0] = 2;
        precedence [1] = 1;

        direction [0] = "increasing";
        direction [1] = "increasing";
      }
      else
      {
        gain = 0.0;

        overload = 0;

        dimension [0] = img_rows (img);
        dimension [1] = img_columns (img);

        precedence [0] = 1;
        precedence [1] = 2;

        direction [0] = NULL;
        direction [1] = NULL;
      }


    /* Make a cbf version of the image */

  a = clock ();
                                                

    /* Create the cbf */

  cbf_failnez (cbf_make_handle (&cbf))


    /* Make a new data block */

  cbf_failnez (cbf_new_datablock (cbf, "image_1"))


    /* Make the _diffrn category */

  cbf_failnez (cbf_new_category (cbf, "diffrn"))
  cbf_failnez (cbf_new_column   (cbf, "id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))


    /* Make the _diffrn_source category */

  cbf_failnez (cbf_new_category (cbf, "diffrn_source"))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "source"))
  cbf_failnez (cbf_set_value    (cbf, "synchrotron"))
  cbf_failnez (cbf_new_column   (cbf, "type"))
  cbf_failnez (cbf_set_value    (cbf, "ssrl crystallography"))


    /* Make the _diffrn_radiation category */  

  cbf_failnez (cbf_new_category (cbf, "diffrn_radiation"))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "wavelength_id"))
  cbf_failnez (cbf_set_value    (cbf, "L1"))


    /* Make the _diffrn_radiation_wavelength category */

  cbf_failnez (cbf_new_category    (cbf, "diffrn_radiation_wavelength"))
  cbf_failnez (cbf_new_column      (cbf, "id"))
  cbf_failnez (cbf_set_value       (cbf, "L1"))
  cbf_failnez (cbf_new_column      (cbf, "wavelength"))

  if (wavelength)
  
    cbf_failnez (cbf_set_doublevalue (cbf, "%.4f", wavelength))

  cbf_failnez (cbf_new_column      (cbf, "wt"))
  cbf_failnez (cbf_set_value       (cbf, "1.0"))


    /* Make the _diffrn_measurement category */  

  cbf_failnez (cbf_new_category (cbf, "diffrn_measurement"))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "method"))
  cbf_failnez (cbf_set_value    (cbf, "oscillation"))
  cbf_failnez (cbf_new_column   (cbf, "sample_detector_distance"))

  if (distance)

    cbf_failnez (cbf_set_doublevalue (cbf, "%.4f", distance))


    /* Make the _diffrn_detector category */  

  cbf_failnez (cbf_new_category (cbf, "diffrn_detector"))
  cbf_failnez (cbf_new_column   (cbf, "id"))
  cbf_failnez (cbf_set_value    (cbf, detector_id))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "type"))
  cbf_failnez (cbf_set_value    (cbf, detector))


    /* Make the _diffrn_detector_element category */  

  cbf_failnez (cbf_new_category     (cbf, "diffrn_detector_element"))
  cbf_failnez (cbf_new_column       (cbf, "id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "detector_id"))
  cbf_failnez (cbf_set_value        (cbf, detector_id))


    /* Make the _diffrn_frame_data category */  

  cbf_failnez (cbf_new_category     (cbf, "diffrn_frame_data"))
  cbf_failnez (cbf_new_column       (cbf, "id"))
  cbf_failnez (cbf_set_value        (cbf, "frame_1"))
  cbf_failnez (cbf_new_column       (cbf, "detector_element_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "detector_id"))
  cbf_failnez (cbf_set_value        (cbf, detector_id))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "binary_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))


    /* Make the _array_structure_list category */  

  cbf_failnez (cbf_new_category     (cbf, "array_structure_list"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_row          (cbf))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "index"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 2))
  cbf_failnez (cbf_new_column       (cbf, "dimension"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, dimension [0]))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, dimension [1]))
  cbf_failnez (cbf_new_column       (cbf, "precedence"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, precedence [0]))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, precedence [1]))
  cbf_failnez (cbf_new_column       (cbf, "direction"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_value        (cbf, direction [0]))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_value        (cbf, direction [1]))


    /* Make the _array_element_size category */

  cbf_failnez (cbf_new_category     (cbf, "array_element_size"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_row          (cbf))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "index"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 2))
  cbf_failnez (cbf_new_column       (cbf, "size"))

  if (pixel_size > 0)
  {
    cbf_failnez (cbf_rewind_row       (cbf))
    cbf_failnez (cbf_set_doublevalue  (cbf, "%.1fe-6", pixel_size * 1e6))
    cbf_failnez (cbf_next_row         (cbf))
    cbf_failnez (cbf_set_doublevalue  (cbf, "%.1fe-6", pixel_size * 1e6))
  }


    /* Make the _array_intensities category */

  cbf_failnez (cbf_new_category     (cbf, "array_intensities"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "binary_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "linearity"))
  cbf_failnez (cbf_set_value        (cbf, "linear"))
  cbf_failnez (cbf_new_column       (cbf, "gain"))

  if (gain)
  
    cbf_failnez (cbf_set_doublevalue  (cbf, "%.3g", gain))
    
  cbf_failnez (cbf_new_column       (cbf, "overload"))

  if (overload)
    
    cbf_failnez (cbf_set_integervalue (cbf, overload))
    
  cbf_failnez (cbf_new_column       (cbf, "undefined"))
  cbf_failnez (cbf_set_integervalue (cbf, 0))


    /* Make the _array_data category */

  cbf_failnez (cbf_new_category     (cbf, "array_data"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "binary_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "data"))


    /* Save the binary data */

  cbf_failnez (cbf_set_integerarray (cbf, compression, 1,
                                 img_pixelptr (img, 0, 0), sizeof (int), 1,
                                 img_rows (img) * img_columns (img)))
  

    /* Write the new file */

  if (!imgout || strcmp(imgout?imgout:"","-") == 0) {
  out = stdout;
  } else {
  out = fopen (imgout, "w+b");
  }
  if (!out)
  {
    if (encoding == ENC_NONE) {
      fprintf (stderr, "img2cif:  Couldn't open the CBF file %s\n", imgout);
    } else {
      fprintf (stderr, "img2cif:  Couldn't open the CIF file %s\n", imgout);
    }
    exit (1);
  }
  cbf_failnez (cbf_write_file (cbf, out, 1, cbforcif, mime | digest,
                                        encoding | bytedir | term))
 


    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))

  b = clock ();
  if (encoding == ENC_NONE) {
    fprintf (stderr, "img2cif:  Time to write the CBF image: %.3fs\n", ((b - a) * 1.0) / CLOCKS_PER_SEC); 
    } else {
    fprintf (stderr, "img2cif:  Time to write the CIF image: %.3fs\n", ((b - a) * 1.0) / CLOCKS_PER_SEC); 
    }

    /* Success */

  return 0;
}
