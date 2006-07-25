/**********************************************************************
 * cbf_uncompressed -- uncompressed binary sections                   *
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

    if (element > limit)

      if (elsign && (int) (element - unsign) < 0)

        element = 0;

      else

        element = limit;
        

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
  unsigned int element, sign, unsign, limit, count, bit;

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

    if (errorcode)

      if (errorcode == CBF_OVERFLOW)

        overflow = errorcode;

      else
      {
        if (nelem_read)
      
          *nelem_read = count;
        
        return errorcode | overflow;
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
