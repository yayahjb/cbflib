/**********************************************************************
 *          img2cif -- convert an image file to a cif file            *
 *          Version 0.6   26 December 1998                             *
 *                                                                    *
 *          based on makecbf by Paul Ellis                            *
 *          revisions by H. J. Bernstein                              *
 *          yaya@bernstein-plus-sons.com                              *
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
    fprintf(stderr,"\nCBFlib fatal error %d \n",err); \
    exit(-1); \
  } \
 }


char *tmpnam(char *s);

int main (int argc, char *argv [])
{
  FILE *in, *out, *file;
  clock_t a,b;
  img_handle img, cbf_img;
  cbf_handle cbf;
  int id, index;
  unsigned int column, row;
  size_t nelem_read;
  double pixel_size, gain, wavelength, distance;
  int overload, dimension [2], precedence [2];
  const char *detector;
  char *detector_char;
  char detector_id [64];
  const char *direction [2], *array_id;
  int c;
  int errflg = 0;
  char *imgin, *imgout, *imgtmp;
  char tmpbuf[L_tmpnam];
  int nbytes;
  char buf[2048];

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
 imgtmp = NULL;
     
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
    imgtmp = strdup(tmpnam(&tmpbuf[0]));
    if ( (file = fopen(imgtmp, "w+")) == NULL) {
       fprintf(stderr,"img2cif:  Can't open temporary file %s.\n", imgtmp);
       exit(1);
       }
     while (nbytes = fread(buf, 1, 1024, stdin)) {
      if(nbytes != fwrite(buf, 1, nbytes, file)) {
       fprintf(stderr,"img2cif:  Failed to write %s.\n", imgtmp);
       exit(1);
       }
    }
    fclose(file);
    imgin = imgtmp;
  }

  img = img_make_handle ();

  a = clock ();

  cbf_failnez (img_read (img, imgin))

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
                                 &img_pixel (img, 0, 0), sizeof (int), 1,
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
