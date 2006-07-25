/**********************************************************************
 * cbf_binary -- handle simple binary values                          *
 *                                                                    *
 * Version 0.4 15 November 1998                                       *
 *                                                                    *
 *             Paul Ellis (ellis@ssrl.slac.stanford.edu)              *
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
#include "cbf_read_mime.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>


  /* Is this a binary value? */

int cbf_is_binary (const char *value)
{
  if (value)

    return *value == CBF_TOKEN_BIN     || 
           *value == CBF_TOKEN_TMP_BIN || 
           *value == CBF_TOKEN_MIME_BIN;

  return 0;
}


  /* Free a value */

int cbf_free_value (cbf_context *context, const char **value)
{
  cbf_file *file;

  int binary_id;

  long start;
  
  long size;

  const char *text;

  char text0;



    /* Check the argument */

  if (!value)

    return CBF_ARGUMENT;

  text = *value;

  if (!text)

    return 0;


    /* Is the value ascii? */

  text0 = *text;

  if (!cbf_is_binary (text))
  {
      /* Free the value */

    cbf_free_string (NULL, *value);

    *value = NULL;

    return 0;
  }


    /* Parse the (binary) value */

  size = 0;

  sscanf (text + 1, " %x %p %lx %lx", &binary_id, &file, &start, &size);

  if (size == 0 || !file)

    return CBF_FORMAT;


    /* Free the value */

  cbf_free_string (NULL, *value);

  *value = NULL;
  

    /* Free the binary section */

  if (text0 == CBF_TOKEN_TMP_BIN)

    return cbf_close_temporary (context, &file);

  return cbf_delete_fileconnection (&file);
}


  /* Set a binary value */

int cbf_set_binary (cbf_node *column, unsigned int row,
                    unsigned int compression, size_t repeat,
                    int binary_id, void *value, size_t elsize, int elsign,
                    size_t nelem)
{
  cbf_file *tempfile;

  const char *newvalue;

  char digest[25];

  int errorcode;
  
  size_t size;
  
  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int)) * CHAR_BIT) >> 2) + 16 + 38];

  long start;


    /* Remove the old value */

  cbf_failnez (cbf_set_columnrow (column, row, NULL))


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
                               compression, repeat, tempfile,
                               &size, digest),
                 cbf_delete_fileconnection (&tempfile))


    /* Set the value */

  sprintf (text, "%x %p %lx %lx %25s %x %x", binary_id, tempfile, start, 
                                  (long) size, digest, elsize, elsign);

  newvalue = cbf_copy_string (NULL, text, CBF_TOKEN_TMP_BIN);

  if (newvalue)

    errorcode = cbf_set_columnrow (column, row, newvalue);

  else

    errorcode = CBF_ALLOC;

  if (errorcode)
  {
    cbf_free_string (NULL, newvalue);

    return errorcode | cbf_delete_fileconnection (&tempfile);
  }


    /* Success */

  return 0;
}

    
  /* Get the parameters of a binary value */
  
int cbf_binary_parameters (cbf_node *column, 
                           unsigned int row, unsigned int *compression,
                           size_t *repeat, int *binary_id, 
                           int *eltype, size_t *elsize, 
                           int *elsigned, 
                           int *elunsigned,
                           size_t *nelem,
                           int *minelem, int *maxelem)
{
  cbf_file *file;

  long start;

  long size;

  const char *text;

  char text_digest [25];

  size_t text_elsize;

  int text_elsign;

  int errorcode;

    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))

  if (!text)

    return CBF_ASCII;

  
    /* Parse the value */
    
  if (*text == CBF_TOKEN_MIME_BIN)
  {
      /* Convert the value to a normal binary value */
      
    cbf_failnez (cbf_mime_temp (column, row))
    
    
      /* Rerun the function */

    return cbf_binary_parameters (column, row, 
                                  compression,
                                  repeat, binary_id, 
                                  eltype, elsize, 
                                  elsigned, elunsigned,
                                  nelem,
                                  minelem, maxelem);
  }

  if (*text != CBF_TOKEN_BIN && *text != CBF_TOKEN_TMP_BIN)

    return CBF_ASCII;

  size = 0;

  sscanf (text + 1, " %x %p %lx %lx %25s %x %x", 
          binary_id, &file, &start, &size, 
          text_digest, &text_elsize, &text_elsign);
  
  if (size == 0 || !file)

    return CBF_FORMAT;


    /* Position the file at the start of the binary section */

  cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))

  
    /* Get the parameters */

  errorcode = cbf_decompress_parameters (eltype, elsize, elsigned, elunsigned, 
                                         nelem, minelem, maxelem,
                                         compression, repeat, &size, file);

  if ( *elsize == 0 ) *elsize = text_elsize;

  return errorcode;

}

                   
  /* Get a binary value */
  
int cbf_get_binary (cbf_node *column, unsigned int row, int *binary_id,
                    void *value, size_t elsize, int elsign,
                    size_t nelem, size_t *nelem_read)
{
  cbf_file *file;

  long start;

  long size;

  const char *text;

  char text_digest [25];

  size_t text_elsize;

  int text_elsign;

  int id_file, eltype_file, elsigned_file, elunsigned_file, 
                            minelem_file, maxelem_file;

  unsigned int compression;

  size_t nelem_file, repeat;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))

  if (!text)

    return CBF_ASCII;

  
    /* Parse the value */

  if (*text == CBF_TOKEN_MIME_BIN)
  {
      /* Convert the value to a normal binary value */
      
    cbf_failnez (cbf_mime_temp (column, row))
    
    
      /* Rerun the functionm */

    return cbf_get_binary (column, row, 
                           binary_id,
                           value, elsize, elsign,
                           nelem, nelem_read);
  }

  if (*text != CBF_TOKEN_BIN && *text != CBF_TOKEN_TMP_BIN)

    return CBF_ASCII;

  size = 0;

  sscanf (text + 1, " %x %p %lx %lx %25s %x %x", 
          &id_file, &file, &start, &size, 
          text_digest, &text_elsize, &text_elsign);

  if (size == 0 || !file)

    return CBF_FORMAT;


    /* Position the file at the start of the binary section */

  cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))

  
    /* Get the parameters and position the file */

  cbf_failnez (cbf_decompress_parameters (&eltype_file, NULL,
                                      &elsigned_file, &elunsigned_file,
                                      &nelem_file,
                                      &minelem_file, &maxelem_file,
                                      &compression, &repeat, 
                                      NULL, file))

  if (binary_id)

    *binary_id = id_file;

  if (elsize == 0) elsize = text_elsize;

    /* Decompress the binary data */

  return cbf_decompress (value, elsize, elsign, nelem, nelem_read,
                         compression, repeat, file);
}

                    
#ifdef __cplusplus

}

#endif

