/**********************************************************************
 * cbf_uncompressed -- uncompressed binary sections                   *
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_uncompressed.h"

#define CBF_SHIFT63 (sizeof (int) * CHAR_BIT > 64 ? 63 : 0)


  /* Copy an array without compression */
  
int cbf_compress_none (void         *source, 
                       size_t        elsize, 
                       int           elsign, 
                       size_t        nelem, 
                       unsigned int  compression, 
                       cbf_file     *file, 
                       size_t       *compressedsize,
                       int          *storedbits)
{
  unsigned int count, element, unsign, sign, limit, bits;

  unsigned char *unsigned_char_data;
  

    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;


    /* Initialise the pointer */

  unsigned_char_data = (unsigned char *) source;


    /* Maximum limit (unsigned) is 64 bits */

  if (elsize * CHAR_BIT > 64)
  {
    sign = 1 << CBF_SHIFT63;

    limit = ~-(sign << 1);
    
    bits = 64;
  }
  else
  {
    sign = 1 << (elsize * CHAR_BIT - 1);

    limit = ~0;

    bits = elsize * CHAR_BIT;
  }

  if (storedbits)
    
    *storedbits = bits;


    /* Offset to make the value unsigned */

  if (elsign)

    unsign = sign;

  else

    unsign = 0;


    /* Initialise the pointer */
    
  unsigned_char_data = (unsigned char *) source;


    /* Write the elements */
    
  for (count = 0; count < nelem; count++)
  {
      /* Get the next element */
      
    if (elsize == sizeof (int))
    
      element = *((unsigned int *) unsigned_char_data);
      
    else
    
      if (elsize == sizeof (short))
      
        element = *((unsigned short *) unsigned_char_data);
        
      else
      
        element = *unsigned_char_data;
        
    unsigned_char_data += elsize;


      /* Make the element unsigned */

    element += unsign;


      /* Limit the value to 64 bits */

    if (element > limit) {

      if (elsign && (int) (element - unsign) < 0)

        element = 0;

      else

        element = limit;
        
    }


      /* Write the element to the file */

    cbf_failnez (cbf_put_integer (file, element - unsign, 0, bits))
  }


    /* Return the number of characters written */
    
  if (compressedsize)
  
    *compressedsize = (nelem * bits + 7) / 8;


    /* Success */

  return 0;
}


  /* Recover an array without decompression */

int cbf_decompress_none (void         *destination, 
                         size_t        elsize, 
                         int           elsign, 
                         size_t        nelem, 
                         size_t       *nelem_read,
                         unsigned int  compression, 
                         int           data_bits, 
                         int           data_sign,
                         cbf_file     *file)
{
  unsigned int element, sign, unsign, limit, count;

  unsigned int data_unsign;

  unsigned char *unsigned_char_data;

  int errorcode, overflow;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;
    
    
    /* Check the stored element size */
    
  if (data_bits < 1 || data_bits > 64)
  
    return CBF_ARGUMENT;


    /* Initialise the pointer */

  unsigned_char_data = (unsigned char *) destination;


    /* Maximum limits */
    
  sign = 1 << (elsize * CHAR_BIT - 1);
    
  if (elsize == sizeof (int))
    
    limit = ~0;
      
  else
    
    limit = ~-(1 << (elsize * CHAR_BIT));


    /* Check the element size */
    
  if (data_bits < 1 || data_bits > 64)
  
    return CBF_FORMAT;
  

    /* Offsets to make the value unsigned */

  if (data_sign)

    data_unsign = sign;

  else

    data_unsign = 0;

  if (elsign)

    unsign = sign;

  else

    unsign = 0;


    /* Read the elements */

  count = 0;

  overflow = 0;

  while (count < nelem)
  {
      /* Get the next element */

    errorcode = cbf_get_integer (file, (int *) &element, 
                                                data_sign, data_bits);

    if (errorcode) {

      if (errorcode == CBF_OVERFLOW)

        overflow = errorcode;

      else
      {
        if (nelem_read)
      
          *nelem_read = count;
        
        return errorcode | overflow;
      }

    }


      /* Make the element unsigned */

    element += data_unsign;


      /* Limit the value to fit the element size */

    if (element > limit)
    {
      if (elsign && (int) (element - unsign) < 0)
      
        element = 0;
        
      else
      
        element = limit;

      overflow = CBF_OVERFLOW;
    }
        
        
      /* Make the element signed? */
        
    element -= unsign;


      /* Save the element */
        
    if (elsize == sizeof (int))
      
      *((unsigned int *) unsigned_char_data) = element;
        
    else
      
      if (elsize == sizeof (short))
        
        *((unsigned short *) unsigned_char_data) = element;
          
      else
        
        *unsigned_char_data = element;
          
    unsigned_char_data += elsize;
    
    count++;
  }


    /* Number read */
    
  if (nelem_read)
  
    *nelem_read = count;


    /* Success */

  return overflow;
}


#ifdef __cplusplus

}

#endif
