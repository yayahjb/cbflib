/**********************************************************************
 * cbf_binary -- handle simple binary values                          *
 *                                                                    *
 * Version 0.7.4 12 January 2004                                      *
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
    
  new_text = cbf_copy_string (NULL, text, (char) type);

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

  if (is_binary) {

    if (type == CBF_TOKEN_TMP_BIN) {

      cbf_failnez (cbf_close_temporary (context, &file))
      
    } else {

      cbf_failnez (cbf_delete_fileconnection (&file))
    }

  }


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
    if (elsize) {
  
      if (file_elsize > 0)
  
        *elsize = file_elsize;
        
      else

        *elsize = (text_bits + CHAR_BIT - 1) / CHAR_BIT;    
    }
        
    if (nelem) {
    
      if (file_nelem > 0)
      
        *nelem = file_nelem;
        
      else
      
        *nelem = (size * 8) / text_bits;

    }

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
