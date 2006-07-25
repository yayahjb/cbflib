/**************************************************
 * cbf_mime.c -- create MIME output               *
 *                                                *
 * based on cbf_binary.c by Paul Ellis            *
 * revisions by H. J. Bernstein                   *
 * yaya@bernstein-plus-sons.com                   *
 **************************************************/
 
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


#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_tree.h"
#include "cbf_compress.h"
#include "cbf_context.h"
#include "cbf_binary.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "md5.h"
char *md5digest_sized(FILE *inpfile, long *flen, long size);
int to64_sized(FILE *inpfile, FILE *outfile, long limit, long size);
                    
  /* Write a binary value as a MIME section */
  
int cbf_write_mime (cbf_node *column, unsigned int row, cbf_file *file, int isbuffer)
{
  cbf_file *infile;
  
  char *infiledigest;
  long infilesize = 0;
  long outfilesize;
  long outfilechars;
  long limit;
  long int start;
  int eltype;
  int elsigned;
  int elunsigned;
  size_t nelem;
  int minelem;
  int maxelem;
  unsigned int compression;

  long size;
  char sizestring[30];

  int binary_id, errorcode;
  char binary_idstring[30];

  char binary_paramstring[60];

  char *intext;

  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int)) * CHAR_BIT) >> 2) + 16];
                
  limit = 0;


    /* Check the arguments */

  if (!file)

    return CBF_ARGUMENT;
    

    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&intext, column, row))

  if (!intext)

    return CBF_ASCII;

  
    /* Parse the value */

  if (*intext != CBF_TOKEN_BIN && *intext != CBF_TOKEN_BIN_TMP)

    return CBF_ASCII;

  size = 0;

  sscanf (intext + 1, " %x %p %lx %lx", &binary_id, &infile, &start, &size);

  if (size == 0 || start < 0 || !infile)

    return CBF_FORMAT;

    /* Position the file at the start of the binary section 
       and compute the MD5 digest                            */


  if (CBFdigest == MSG_DIGEST) {
    if (fseek (infile->stream, start, SEEK_SET)) {
      fprintf(stderr,"\nCBFlib fseek failure for message digest\n");
      return CBF_FILEREAD;
    }

    /* Discard any bits in the buffers */
    cbf_failnez (cbf_reset_bits (infile));
  

    /* Read the compression id (64 bits) to position properly */
    cbf_failnez (cbf_get_integer (infile, (int *) &compression, 0, 64))

    /* Process the digest */
    infiledigest = md5digest_sized(infile->stream, &infilesize, size-8);
    if (infilesize != size-8 ) {
      fprintf(stderr,"\nCBFlib digest file size mismatch %lx %lx\n",
        infilesize, size-8);
      return CBF_FILEREAD;
    }

  }

    /* Position the file at the start of the binary section  again*/

  if (fseek (infile->stream, start, SEEK_SET))
    return CBF_FILEREAD;
  cbf_failnez (cbf_decompress_parameters ( &eltype, &elsigned,
    &elunsigned, &nelem, &minelem, &maxelem, &compression, infile))
  if (fseek (infile->stream, start, SEEK_SET))
    return CBF_FILEREAD;
    cbf_failnez (cbf_reset_bits (infile));
    cbf_failnez (cbf_get_integer (infile, (int *) &compression, 0, 64))
    
  
    /* Write the MIME header */

  cbf_failnez (cbf_write_string (file, "\n;\n"))
  if (CBFmime == MIME_HEADERS) {
    cbf_failnez (cbf_write_string (file, "--CIF-BINARY-FORMAT-SECTION--\n"))
    if (CBFcompression == CBF_NONE) {
      cbf_failnez (cbf_write_string (file, 
        "Content-Type: application/octet-stream\n"))
    } else {
      cbf_failnez (cbf_write_string (file, 
        "Content-Type: application/octet-stream;\n"))
      if (CBFcompression == CBF_PACKED) {
        cbf_failnez (cbf_write_string (file, 
          "     conversions=\"x-CBF_PACKED\"\n"))
      } else {
        if (CBFcompression == CBF_CANONICAL) {
          cbf_failnez (cbf_write_string (file, 
            "     conversions=\"x-CBF_CANONICAL\"\n"))
        } else {
          if (CBFcompression == CBF_BYTE_OFFSET) {
            cbf_failnez (cbf_write_string (file, 
              "     conversions=\"x-CBF_BYTE_OFFSET\"\n"))
          } else {
            if (CBFcompression == CBF_PREDICTOR) {
              cbf_failnez (cbf_write_string (file, 
                "     conversions=\"x-CBF_PREDICTOR\"\n"))
	    } else {
              cbf_failnez (cbf_write_string (file, 
                "     conversions=\"x-CBF_UNKNOWN\"\n"))
	    }
          }
        }
              
      }
    }

    if (CBFencoding == ENC_BASE64) {
      cbf_failnez (cbf_write_string (file, 
        "Content-Transfer-Encoding: BASE64\n"))
    } else {
      if (CBFencoding == ENC_QP ) {
        cbf_failnez (cbf_write_string (file, 
        "Content-Transfer-Encoding: QUOTED-PRINTABLE\n"))
      } else {
        if (CBFencoding == ENC_BASE10 ) {
          cbf_failnez (cbf_write_string (file,
          "Content-Transfer-Encoding: X-BASE10\n"))
        } else {
          if (CBFencoding == ENC_BASE16 ) {
            cbf_failnez (cbf_write_string (file,
            "Content-Transfer-Encoding: X-BASE16\n"))
	  } else {
            cbf_failnez (cbf_write_string (file,
            "Content-Transfer-Encoding: X-BASE8\n"))
	  }
        }
      }
    }
    sprintf(sizestring, "X-Binary-Size: %u\n",size-8);
    cbf_failnez (cbf_write_string (file, sizestring))
    sprintf(binary_idstring, "X-Binary-ID: %d\n",binary_id);
    cbf_failnez (cbf_write_string (file, binary_idstring))
  }
  
  if (CBFdigest == MSG_DIGEST) {
    cbf_failnez (cbf_write_string (file, "Content-MD5: "))
    cbf_failnez (cbf_write_string (file, infiledigest))
    cbf_failnez (cbf_write_string (file, "\n\n"))
  }
  
  if (CBFdigest != MSG_DIGEST && CBFmime == MIME_HEADERS) {
    cbf_failnez (cbf_write_string (file, "\n"))  
  }

    /* Copy the binary section to the output file */
  if (CBFencoding == ENC_BASE64) {
    outfilesize = to64_sized(infile->stream, file->stream, limit, size-8);
  } else {
    if (CBFencoding == ENC_QP ) {
      outfilesize = toqp_sized(infile->stream, file->stream, limit, size-8);
    } else {
      if (CBFencoding == ENC_BASE10 ) {
        outfilesize = todec_sized(infile->stream, file->stream, limit, 
          size-8, CBFelsize, CBFbytedir);
      } else {
        if (CBFencoding == ENC_BASE16 ) {
         outfilesize = tohex_sized(infile->stream, file->stream, limit, 
           size-8, CBFelsize, CBFbytedir);
        } else {
         outfilesize = tooct_sized(infile->stream, file->stream, limit, 
           size-8, CBFelsize, CBFbytedir);
        }
      }
    }
  }

    /* Write the MIME footer */

  if (CBFmime == MIME_HEADERS) {
    cbf_failnez (cbf_write_string (file, "\n--CIF-BINARY-FORMAT-SECTION----\n;\n"))
  } else {
    cbf_failnez (cbf_write_string (file, "\n;\n"))
  }



    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif

