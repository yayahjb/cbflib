/**********************************************************************
 * cbf_binary -- handle simple binary values                          *
 *                                                                    *
 * Version 0.8.0 20 July 2008                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006, 2007 Herbert J. Bernstein                      *
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
#include "cbf_string.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

static const char * big_endian = "big_endian";
static const char * little_endian = "little_endian";
static const char * unknown = "unknown";


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
                     int       *realarray,
                     const char **byteorder,
                     size_t    *dimover,
                     size_t    *dimfast,
                     size_t    *dimmid,
                     size_t    *dimslow,
                     size_t    *padding,
            unsigned int       *compression)
{
  void *file_text;

  unsigned long start_text, size_text;

  int id_text, type_text, checked_digest_text, bits_text, sign_text, realarray_text;
  
  unsigned long dimover_text, dimfast_text, dimmid_text, dimslow_text;
  
  unsigned long padding_text;

  unsigned int compression_text;

  char digest_text [25];
  
  char byteorder_text [14];

  const char *text;


    /* Check that the value is binary */

  if (!cbf_is_binary (column, row))

    return CBF_ASCII;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))


    /* Parse it */

  type_text = *text;

  sscanf (text + 1, " %x %p %lx %lx %d %24s %x %d %d %14s %lu %lu %lu %lu %lu %u",
                      (unsigned int *)&id_text,
                      &file_text,
                      (unsigned long *)&start_text,
                      (unsigned long *)&size_text,
                      &checked_digest_text,
                       digest_text,
                      (unsigned int *)&bits_text,
                      &sign_text,
                      &realarray_text,
                      byteorder_text, 
                      (unsigned long *)&dimover_text, 
                      (unsigned long *)&dimfast_text, 
                      (unsigned long *)&dimmid_text, 
                      (unsigned long *)&dimslow_text, 
                      (unsigned long *)&padding_text,
                      &compression_text);


    /* Copy the values */

  if (type)

    *type = type_text;

  if (id)

    *id = id_text;

  if (file)

    *file = (cbf_file *)file_text;

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

  if (realarray)

    *realarray = realarray_text;
    
  if (byteorder)  {
  
    if (byteorder_text[0]=='b'|| byteorder_text[0]=='B') {
    
      *byteorder = big_endian;
    	
    }  else if (byteorder_text[0]=='l'|| byteorder_text[0]=='L') {
    
      *byteorder = little_endian;
    	
    }
    else *byteorder = unknown;
  	
  }
   
  if (dimover)
  
     *dimover = dimover_text;

  if (dimfast)
  
     *dimfast = dimfast_text;
   
  if (dimmid)
  
     *dimmid = dimmid_text;
   
  if (dimslow)
  
     *dimslow = dimslow_text;
   
  if (padding)
  
     *padding = padding_text;

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
                     int         realarray,
                     const char *byteorder,
                     size_t      dimover,
                     size_t      dimfast,
                     size_t      dimmid,
                     size_t      dimslow,
                     size_t      padding,
            unsigned int         compression)
{
  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int) * 3) * CHAR_BIT) >> 2) + 57
                +15+((5*sizeof (size_t)*3*CHAR_BIT)>>2)];

  const char *new_text;

  int errorcode;


    /* Check that the digest has the correct format */

  if (!cbf_is_base64digest (digest))
  {
    digest = "------------------------";

    checked_digest = 0;
  }


    /* Create the new text */

  sprintf (text, "%x %p %lx %lx %1d %24s %x %d %d %14s %ld %ld %ld %ld %ld %u",
                   (unsigned int)id,
                   (void *)file,
                   (unsigned long)start,
                   (unsigned long)size,
                   checked_digest != 0,
                   digest,
                   (unsigned int)bits,
                   sign,
                   realarray,
                   byteorder,
                   (unsigned long)dimover,
                   (unsigned long)dimfast, 
                   (unsigned long)dimmid, 
                   (unsigned long)dimslow, 
                   (unsigned long)padding,
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
                                           NULL, NULL, NULL, NULL, NULL, 
                                           NULL, NULL, NULL, NULL, NULL,
                                           NULL, NULL, NULL))


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
                    size_t nelem, int realarray, const char *byteorder,
                    size_t dimover,
                    size_t dimfast, size_t dimmid, size_t dimslow, size_t padding)
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
                               &size, &bits, digest, realarray,
                               "little_endian", dimfast, dimmid, dimslow, padding),
                 cbf_delete_fileconnection (&tempfile))


    /* Set the value */
    
    /* We do not yet support writing of big_endian binary sections */
    
  if (cbf_cistrncmp(byteorder,"little_endian",14)) {

  	 cbf_delete_fileconnection (&tempfile);

     return CBF_FORMAT;
  }

  cbf_onfailnez (cbf_set_bintext (column, row, CBF_TOKEN_TMP_BIN,
                                  binary_id, tempfile, start, size,
                                  1, digest, bits, elsign != 0, realarray, 
                                  "little_endian", dimover, dimfast, dimmid, dimslow, padding, compression),
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
  
  const char *byteorder;

  int id, bits, sign, type, checked_digest, realarray;
  
  size_t dimover, dimfast, dimmid, dimslow;
  
  size_t padding;

  unsigned int compression;


    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, &type, &id, &file,
                                &start, &size, &checked_digest,
                                old_digest, &bits, &sign, &realarray, 
                                &byteorder, &dimover, &dimfast, &dimmid, &dimslow,
                                &padding, &compression))


    /* Recalculate and compare the digest? */

  if ((file->read_headers & (MSG_DIGEST|MSG_DIGESTNOW|MSG_DIGESTWARN) ) && !checked_digest)

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
                                    1, new_digest, bits, sign, realarray, 
                                    byteorder, dimover, dimfast, dimmid, dimslow, padding,
                                    compression))
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
                           int *minelem, int *maxelem, int *realarray,
                           const char **byteorder, 
                           size_t *dimfast, size_t *dimmid, size_t *dimslow,
                           size_t *padding)
{
  cbf_file *file;

  long start;

  size_t size, file_elsize, file_nelem;

  int text_bits, errorcode;
  
  size_t text_dimover;
  
  int text_sign;


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
                                  minelem, maxelem, realarray,
                                  byteorder, dimfast, dimmid, dimslow,
                                  padding);
  }


    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, NULL,
                                id, &file, &start, &size, NULL,
                                NULL, &text_bits, &text_sign, realarray,
                                byteorder, &text_dimover, dimfast, dimmid, dimslow, padding, 
                                compression))

    /* Position the file at the start of the binary section */

  cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))


    /* Get the parameters */

  errorcode = cbf_decompress_parameters (eltype, &file_elsize, elsigned,
                                         elunsigned,
                                         &file_nelem, minelem, maxelem,
                                         *compression, file);

  if (!errorcode)
  {
    if (text_sign != -1 && elsigned) {
    
      *elsigned = text_sign?1:0;
      
    }
    
    if (text_sign != -1 && elunsigned) 
    {
      
      *elunsigned = text_sign?0:1;
    	
    }
    
    if (elsize) {
    
      if ( text_bits > 0) {
      
         *elsize = (text_bits + CHAR_BIT - 1) / CHAR_BIT;
      	
      } else  {

        if (file_elsize > 0) {
        	
          *elsize = file_elsize;
          
        }

      }

        
    }

    if (nelem) {

      if (file_nelem > 0)

        *nelem = file_nelem;

      else  {
      
        if (text_dimover > 0) *nelem = text_dimover;
        
        else

        *nelem = (size * 8) / text_bits;
     }

    }

  }

  return errorcode;
}


  /* Get a binary value */

int cbf_get_binary (cbf_node *column, unsigned int row, int *id,
                    void *value, size_t elsize, int elsign,
                    size_t nelem, size_t *nelem_read, int *realarray,
                    const char **byteorder, size_t *dimover, size_t *dimfast, size_t *dimmid,
                    size_t *dimslow, size_t *padding)
{
  cbf_file *file;

  long start;

  int eltype_file, elsigned_file, elunsigned_file,
                   minelem_file, maxelem_file, bits, sign;

  unsigned int compression;

  size_t nelem_file;
  
  size_t text_dimover;


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
                           nelem, nelem_read, realarray,
                           byteorder, dimover, dimfast, dimmid, dimslow, padding);
  }


    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, NULL,
                                id, &file, &start, NULL,
                                 NULL, NULL, &bits, &sign, realarray,
                                 byteorder, &text_dimover, dimfast, dimmid, dimslow, padding,
                                 &compression))

  if (dimover) *dimover = text_dimover;

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
                         compression, bits, sign, file, *realarray,
                         *byteorder, text_dimover, *dimfast, *dimmid, *dimslow, *padding);
}


#ifdef __cplusplus

}

#endif
