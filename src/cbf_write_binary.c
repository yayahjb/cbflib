/**********************************************************************
 * cbf_write_binary -- write binary sections                          *
 *                                                                    *
 * Version 0.6 13 January 1999                                        *
 *                                                                    *
 *            Paul Ellis (ellis@ssrl.slac.stanford.edu) and           *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
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
 *                          The IUCr Policy                           *
 *                                 on                                 *
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
#include "cbf_codes.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "cbf_write_binary.h"

                
  /* Write a binary value */
  
int cbf_write_binary (cbf_node *column, unsigned int row, 
                                        cbf_file *file, 
                                        int isbuffer)
{
  cbf_file *infile;

  char digest [25], text [100];
  
  long start;
  
  size_t size;
  
  unsigned int compression;

  int id, bits, sign, type, checked_digest, elsize;


    /* Check the arguments */

  if (!file)

    return CBF_ARGUMENT;
    
  if (((file->write_encoding & ENC_QP)     > 0) + 
      ((file->write_encoding & ENC_BASE64) > 0) + 
      ((file->write_encoding & ENC_BASE8)  > 0) + 
      ((file->write_encoding & ENC_BASE10) > 0) + 
      ((file->write_encoding & ENC_BASE16) > 0) + 
      ((file->write_encoding & ENC_NONE)   > 0) != 1)

    return CBF_ARGUMENT;
    
  if (!cbf_is_binary (column, row))

    return CBF_ARGUMENT;
    
  if (cbf_is_mimebinary (column, row))

    return CBF_ARGUMENT;
    

    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, &type, &id, &infile,
                                &start, &size, &checked_digest,
                                 digest, &bits, &sign, &compression))


    /* Position the file at the start of the binary section */

  cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))
  

    /* Calculate the digest if necessary */
  
  if (!cbf_is_base64digest (digest) && (file->write_headers & MSG_DIGEST))
  {
      /* Discard any bits in the buffers */

    cbf_failnez (cbf_reset_bits (infile))


      /* Compute the message digest */

    cbf_failnez (cbf_md5digest (infile, size, digest))
      

      /* Go back to the start of the binary data */

    cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))
    
    
      /* Update the entry */
      
    checked_digest = 1;
      
    cbf_failnez (cbf_set_bintext (column, row, type,
                                  id, infile, start, size,
                                  checked_digest, digest, bits, 
                                                          sign,
                                                          compression))
  }


    /* Discard any bits in the buffers */

  cbf_failnez (cbf_reset_bits (infile))

  
    /* Do we need MIME headers? */
    
  if (compression == CBF_NONE && (file->write_headers & MIME_NOHEADERS))
  
    return CBF_ARGUMENT;
    

    /* Write the header */

  cbf_failnez (cbf_write_string (file, "\n;\n"))


    /* MIME header? */

  if (file->write_headers & MIME_HEADERS)
  {
    cbf_failnez (cbf_write_string (file, "--CIF-BINARY-FORMAT-SECTION--\n"))
    
    if (compression == CBF_NONE)
    
      cbf_failnez (cbf_write_string (file, 
                                "Content-Type: application/octet-stream\n"))
      
    else
    {
      cbf_failnez (cbf_write_string (file, 
                                "Content-Type: application/octet-stream;\n"))
 
      switch (compression)
      {
        case CBF_PACKED:
        
          cbf_failnez (cbf_write_string (file, 
                                "     conversions=\"x-CBF_PACKED\"\n"))

          break;
              
        case CBF_CANONICAL:
        
          cbf_failnez (cbf_write_string (file, 
                                "     conversions=\"x-CBF_CANONICAL\"\n"))

          break;
              
        case CBF_BYTE_OFFSET:
        
          cbf_failnez (cbf_write_string (file, 
                                "     conversions=\"x-CBF_BYTE_OFFSET\"\n"))

          break;
              
        case CBF_PREDICTOR:
        
          cbf_failnez (cbf_write_string (file, 
                                "     conversions=\"x-CBF_PREDICTOR\"\n"))

          break;
              
        default:
      
          cbf_failnez (cbf_write_string (file, 
                                "     conversions=\"x-CBF_UNKNOWN\"\n"))
      }
    }

    if (file->write_encoding & ENC_QP)
                                
      cbf_failnez (cbf_write_string (file, 
                      "Content-Transfer-Encoding: QUOTED-PRINTABLE\n"))
                                
    else
                  
      if (file->write_encoding & ENC_BASE64)
                                
        cbf_failnez (cbf_write_string (file, 
                      "Content-Transfer-Encoding: BASE64\n"))
                                
      else
                  
        if (file->write_encoding & ENC_BASE8)
                                
          cbf_failnez (cbf_write_string (file, 
                      "Content-Transfer-Encoding: X-BASE8\n"))
                                
        else
                  
          if (file->write_encoding & ENC_BASE10)
                                
            cbf_failnez (cbf_write_string (file, 
                      "Content-Transfer-Encoding: X-BASE10\n"))
                                
          else
                  
            if (file->write_encoding & ENC_BASE16)
                                
              cbf_failnez (cbf_write_string (file, 
                      "Content-Transfer-Encoding: X-BASE16\n"))
                      
            else
            
              cbf_failnez (cbf_write_string (file, 
                      "Content-Transfer-Encoding: BINARY\n"))

    sprintf (text, "X-Binary-Size: %u\n", size);
    
    cbf_failnez (cbf_write_string (file, text))

    sprintf (text, "X-Binary-ID: %d\n", id);

    cbf_failnez (cbf_write_string (file, text))
    
    if (sign)
    
      sprintf (text, "X-Binary-Element-Type: \"signed %d-bit integer\"\n", 
                                                    bits);
      
    else

      sprintf (text, "X-Binary-Element-Type: \"unsigned %d-bit integer\"\n", 
                                                      bits);
      
    cbf_failnez (cbf_write_string (file, text))
    
    
      /* Save the digest if we have one */

    if (cbf_is_base64digest (digest))
    {
      sprintf (text, "Content-MD5: %24s\n", digest);

      cbf_failnez (cbf_write_string (file, text))
    }

    cbf_failnez (cbf_write_string (file, "\n"))  
  }
  else

      /* Simple header */
    
    cbf_failnez (cbf_write_string (file, "START OF BINARY SECTION\n"))
    

    /* Copy the binary section to the output file */
    
  if (file->write_encoding & ENC_NONE)
  {
      /* Write the separators */  
  
    cbf_failnez (cbf_put_character (file, 12))
    cbf_failnez (cbf_put_character (file, 26))
    cbf_failnez (cbf_put_character (file, 4))
    cbf_failnez (cbf_put_character (file, 213))


      /* Flush any bits in the buffers */

    cbf_failnez (cbf_flush_bits (file))


      /* If no MIME header, write the necessary data here */

    if ( !(file->write_headers & MIME_HEADERS) ) {


        /* Write the binary identifier (64 bits) */

      cbf_failnez (cbf_put_integer (file, id, 1, 64))


        /* Write the size of the binary section (64 bits) */

      cbf_failnez (cbf_put_integer (file, size, 0, 64))


        /* Write the compression type (64 bits) */

      cbf_failnez (cbf_put_integer (file, compression, 0, 64))
    }


      /* Get the current point in the new file */

    cbf_failnez (cbf_get_fileposition (file, &start))


      /* Copy the binary section to the output file */

    cbf_failnez (cbf_copy_file (file, infile, size))
  }
  else
  {
      /* Read the element size with no compression? */
      
    if (compression == CBF_NONE)
    {
      elsize = (bits + 4) / 8;
      
      if (elsize < 1 || elsize == 5)
      
        elsize = 4;
        
      else
        
        if (elsize == 7)
      
          elsize = 6;
          
        else

          if (elsize > 8)
      
            elsize = 8;
    }
    else
    
      elsize = 4;
      
      
      /* Go back to the start of the binary data */

    cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))


      /* Flush any bits in the buffers */

    cbf_failnez (cbf_flush_bits (infile))


    if (file->write_encoding & ENC_QP)
  
      cbf_failnez (cbf_toqp (infile, file, size))

    else
  
      if (file->write_encoding & ENC_BASE64)
  
        cbf_failnez (cbf_tobase64 (infile, file, size))

      else
  
        if (file->write_encoding & ENC_BASE8)
  
          cbf_failnez (cbf_tobasex (infile, file, size, elsize, 8))

        else
  
          if (file->write_encoding & ENC_BASE10)
  
            cbf_failnez (cbf_tobasex (infile, file, size, elsize, 10))

          else
  
            cbf_failnez (cbf_tobasex (infile, file, size, elsize, 16))
  }


    /* Write the MIME footer */

  if (file->write_headers & MIME_HEADERS)

    cbf_failnez (cbf_write_string (file, 
                               "\n--CIF-BINARY-FORMAT-SECTION----\n;\n"))
    
  else
  
    cbf_failnez (cbf_write_string (file, "\nEND OF BINARY SECTION\n;\n"))


    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (file))


    /* Replace a connection to a temporary file? */
    
  if (start  != 0               && 
      isbuffer                  && 
      type == CBF_TOKEN_TMP_BIN && (file->write_encoding & ENC_NONE))

    cbf_failnez (cbf_set_bintext (column, row, CBF_TOKEN_BIN,
                                  id, file, start, size, checked_digest,
                                  digest, bits, sign, compression))


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif
