/**********************************************************************
 * cbf_binary -- handle simple binary values                          *
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
#include "cbf_codes.h"
#include "cbf_compress.h"
#include "cbf_context.h"
#include "cbf_binary.h"
#include "cbf_read_mime.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>


  /* Parse a binary text value */
  
int cbf_get_bintext (cbf_node  *column, unsigned int row,
                     int       *type,
                     int       *id, 
                     cbf_file **file,
                     long      *start,
                     size_t    *size,
                     int       *checked_digest,
                     char      *digest,
                     int       *bits,
                     int       *sign,
            unsigned int       *compression)
{
  cbf_file *file_text;

  long start_text, size_text;

  int id_text, type_text, checked_digest_text, bits_text, sign_text;

  unsigned int compression_text;
  
  char digest_text [25];

  const char *text;


    /* Check that the value is binary */

  if (!cbf_is_binary (column, row))

    return CBF_ASCII;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))
    
    
    /* Parse it */
    
  type_text = *text;

  sscanf (text + 1, " %x %p %lx %lx %d %24s %x %d %u", 
                      &id_text, 
                      &file_text, 
                      &start_text, 
                      &size_text, 
                      &checked_digest_text, 
                       digest_text, 
                      &bits_text, 
                      &sign_text,
                      &compression_text);


    /* Copy the values */
    
  if (type)
  
    *type = type_text;
    
  if (id)
  
    *id = id_text;
    
  if (file)
  
    *file = file_text;
    
  if (start)
  
    *start = start_text;
   
  if (size)
 
    *size = size_text;
   
  if (checked_digest)
  
    *checked_digest = checked_digest_text;
    
  if (digest)
  
    strcpy (digest, digest_text);
    
  if (bits)
  
    *bits = bits_text;
    
  if (sign)
  
    *sign = sign_text;

  if (compression)

    *compression = compression_text;
    
    
    /* Success */
    
  return 0;
}  
   

  /* Set a binary text value */
  
int cbf_set_bintext (cbf_node *column, unsigned int row,
                     int         type,
                     int         id, 
                     cbf_file   *file,
                     long        start,
                     long        size,
                     int         checked_digest,
                     const char *digest,
                     int         bits,
                     int         sign,
            unsigned int         compression)
{
  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int) * 3) * CHAR_BIT) >> 2) + 55];
                
  const char *new_text;

  int errorcode;
  

    /* Check that the digest has the correct format */
    
  if (!cbf_is_base64digest (digest))
  {
    digest = "------------------------";
    
    checked_digest = 0;
  }


    /* Create the new text */                

  sprintf (text, "%x %p %lx %lx %1d %24s %x %d %u", 
                   id, 
                   file, 
                   start, 
                   size, 
                   checked_digest != 0,
                   digest, 
                   bits, 
                   sign,
                   compression);
    
  new_text = cbf_copy_string (NULL, text, type);

  if (!new_text)
    
    return CBF_ALLOC;


    /* Add a new connection to the file */

  cbf_onfailnez (cbf_add_fileconnection (&file, NULL),
                 cbf_free_string (NULL, new_text))


    /* Set the new value */

  errorcode = cbf_set_columnrow (column, row, new_text, 1);

  if (errorcode)
  {
    cbf_free_string (NULL, new_text);
    
    return errorcode | cbf_delete_fileconnection (&file);
  }

  
    /* Success */
    
  return 0;
}


  /* Is this a binary value? */

int cbf_is_binary (cbf_node *column, unsigned int row)
{
  const char *text;
  
  
    /* Get the value */

  if (cbf_get_columnrow (&text, column, row))
  
    return 0;
  
  if (text)

    return (*text == CBF_TOKEN_BIN     || 
            *text == CBF_TOKEN_TMP_BIN || 
            *text == CBF_TOKEN_MIME_BIN);
      
      
    /* Fail */

  return 0;
}


  /* Is this an encoded binary value? */

int cbf_is_mimebinary (cbf_node *column, unsigned int row)
{
  const char *text;
  
  
    /* Get the value */

  if (cbf_get_columnrow (&text, column, row))
  
   return 0;
  
  if (text)

    return (*text == CBF_TOKEN_MIME_BIN);
        
      
    /* Fail */

  return 0;
}


  /* Free a value */

int cbf_free_value (cbf_context *context, cbf_node *column, unsigned int row)
{
  cbf_file *file;

  const char *text;

  int is_binary, type;


    /* Check the argument */

  if (!column)

    return CBF_ARGUMENT;
    
    
    /* Is the value binary? */
    
  is_binary = cbf_is_binary (column, row);
  

    /* Parse the (binary) value */
    
  if (is_binary)
      
    cbf_failnez (cbf_get_bintext (column, row, &type, NULL, &file, NULL, 
                                           NULL, NULL, NULL, NULL, NULL, NULL))

    
    /* Get the ASCII value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))
  

    /* Set the value to null */
      
  cbf_failnez (cbf_set_columnrow (column, row, NULL, 0))
    

    /* And free it */

  cbf_free_string (NULL, text);

  if (is_binary)

    if (type == CBF_TOKEN_TMP_BIN)

      cbf_failnez (cbf_close_temporary (context, &file))
      
    else

      cbf_failnez (cbf_delete_fileconnection (&file))
  
  
    /* Success */
    
  return 0;
}


  /* Set a binary value */

int cbf_set_binary (cbf_node *column, unsigned int row,
                    unsigned int compression,  int binary_id, 
                    void *value, size_t elsize, int elsign,
                    size_t nelem)
{
  cbf_file *tempfile;

  char digest [25];
  
  size_t size;
  
  long start;
  
  int bits;


    /* Remove the old value */

  cbf_failnez (cbf_set_columnrow (column, row, NULL, 1))


    /* Get the temporary file */

  cbf_failnez (cbf_open_temporary (column->context, &tempfile))


    /* Move to the end of the temporary file */

  if (cbf_set_fileposition (tempfile, 0, SEEK_END))

    return CBF_FILESEEK | cbf_delete_fileconnection (&tempfile);


    /* Get the starting location */

  if (cbf_get_fileposition (tempfile, &start))

    return CBF_FILETELL | cbf_delete_fileconnection (&tempfile);
    

    /* Add the binary data to the temporary file */

  cbf_onfailnez (cbf_compress (value, elsize, elsign, nelem,
                               compression, tempfile,
                               &size, &bits, digest),
                 cbf_delete_fileconnection (&tempfile))


    /* Set the value */
    
  cbf_onfailnez (cbf_set_bintext (column, row, CBF_TOKEN_TMP_BIN,
                                  binary_id, tempfile, start, size,
                                  1, digest, bits, elsign != 0, compression),
                 cbf_delete_fileconnection (&tempfile))


    /* Success */

  return 0;
}
    

  /* Check the message digest */
  
int cbf_check_digest (cbf_node *column, unsigned int row)
{
  cbf_file *file;

  long start;
  
  size_t size;

  char old_digest [25], new_digest [25];

  int id, bits, sign, type, checked_digest;

  unsigned int compression;


    /* Parse the value */
    
  cbf_failnez (cbf_get_bintext (column, row, &type, &id, &file, 
                                &start, &size, &checked_digest, 
                                old_digest, &bits, &sign, &compression))


    /* Recalculate and compare the digest? */

  if ((file->read_headers & MSG_DIGEST) && !checked_digest)
  
    if (cbf_is_base64digest (old_digest))
    {
        /* Is it encoded? */
    
      if (cbf_is_mimebinary (column, row))
      {
          /* Convert the value to a normal binary value */
      
        cbf_failnez (cbf_mime_temp (column, row))
    
    
          /* Rerun the function */

        return cbf_check_digest (column, row);
      }


        /* Position the file */

      cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))

        /* Recalculate and check the digest */

      cbf_failnez (cbf_md5digest (file, size, new_digest))

      if (strcmp (old_digest, new_digest) != 0)
                 
        return CBF_FORMAT;
      
      
        /* Change the text to show that the digest has been checked */
      
      cbf_failnez (cbf_set_bintext (column, row, type,
                                    id, file, start, size,
                                    1, new_digest, bits, sign, compression))
    }
  
  
    /* Success */
    
  return 0;
}


  /* Get the parameters of a binary value */
  
int cbf_binary_parameters (cbf_node *column, 
                           unsigned int row, unsigned int *compression,
                           int *id, 
                           int *eltype, size_t *elsize, 
                           int *elsigned, 
                           int *elunsigned,
                           size_t *nelem,
                           int *minelem, int *maxelem)
{
  cbf_file *file;

  long start;

  size_t size, file_elsize, file_nelem;

  int text_bits, errorcode;
  
  
    /* Check the digest (this will also decode it if necessary) */

  cbf_failnez (cbf_check_digest (column, row))
  

    /* Is it an encoded binary section? */
    
  if (cbf_is_mimebinary (column, row))
  {

      /* Convert the value to a normal binary value */
      
    cbf_failnez (cbf_mime_temp (column, row))
    
    
      /* Rerun the function */

    return cbf_binary_parameters (column, row, 
                                  compression,
                                  id, 
                                  eltype, elsize, 
                                  elsigned, elunsigned,
                                  nelem,
                                  minelem, maxelem);
  }


    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, NULL,
                                id, &file, &start, &size, NULL,
                                NULL, &text_bits, NULL, compression))


    /* Position the file at the start of the binary section */
    
  cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))

  
    /* Get the parameters */

  errorcode = cbf_decompress_parameters (eltype, &file_elsize, elsigned, 
                                         elunsigned, 
                                         &file_nelem, minelem, maxelem,
                                         *compression, file);

  if (!errorcode)
  {
    if (elsize)
  
      if (file_elsize > 0)
  
        *elsize = file_elsize;
        
      else

        *elsize = (text_bits + CHAR_BIT - 1) / CHAR_BIT;    
        
    if (nelem)
    
      if (file_nelem > 0)
      
        *nelem = file_nelem;
        
      else
      
        *nelem = (size * 8) / text_bits;
  }

  return errorcode;
}

                   
  /* Get a binary value */
  
int cbf_get_binary (cbf_node *column, unsigned int row, int *id,
                    void *value, size_t elsize, int elsign,
                    size_t nelem, size_t *nelem_read)
{
  cbf_file *file;

  long start;

  int eltype_file, elsigned_file, elunsigned_file, 
                   minelem_file, maxelem_file, bits, sign;

  unsigned int compression;

  size_t nelem_file;


    /* Check the digest (this will also decode it if necessary) */

  cbf_failnez (cbf_check_digest (column, row))
  

    /* Is it an encoded binary section? */
    
  if (cbf_is_mimebinary (column, row))
  {
      /* Convert the value to a normal binary value */
      
    cbf_failnez (cbf_mime_temp (column, row))
    
    
      /* Rerun the function */

    return cbf_get_binary (column, row, 
                           id, value, elsize, elsign,
                           nelem, nelem_read);
  }


    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, NULL,
                                id, &file, &start, NULL, 
                                 NULL, NULL, &bits, &sign, &compression))
  

    /* Position the file at the start of the binary section */

  cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))

  
    
    /* Get the parameters and position the file */

  cbf_failnez (cbf_decompress_parameters (&eltype_file, NULL,
                                          &elsigned_file, &elunsigned_file,
                                          &nelem_file,
                                          &minelem_file, &maxelem_file,
                                          compression, 
                                          file))


    /* Decompress the binary data */

  return cbf_decompress (value, elsize, elsign, nelem, nelem_read,
                         compression, bits, sign, file);
}

                    
#ifdef __cplusplus

}

#endif
