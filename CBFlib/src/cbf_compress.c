/**********************************************************************
 * cbf_compress -- compression and decompression                      *
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
#include "cbf_file.h"
#include "cbf_compress.h"
#include "cbf_canonical.h"
#include "cbf_packed.h"
#include "cbf_byte_offset.h"
#include "cbf_predictor.h"
#include "cbf_uncompressed.h"


  /* Compress an array */

int cbf_compress (void         *source, 
                  size_t        elsize, 
                  int           elsign, 
                  size_t        nelem,
                  unsigned int  compression, 
                  cbf_file     *file, 
                  size_t       *compressedsize,
                  int          *bits, 
                  char         *digest)
{
  int errorcode;
  
  size_t size;


    /* Discard any bits in the buffers */
    
  cbf_failnez (cbf_reset_bits (file))
  
  if (compressedsize)
  
    *compressedsize = 0;


    /* Start a digest? */
    
  if (digest)
  
    cbf_failnez (cbf_start_digest (file))


  errorcode = 0;
  
  size = 0;

  switch (compression)
  {
    case CBF_CANONICAL:
    
      errorcode = cbf_compress_canonical (source, elsize, elsign, nelem,
                                          compression, file, 
                                          &size, bits);
      break;

    case CBF_PACKED:
    case 0:

      errorcode = cbf_compress_packed (source, elsize, elsign, nelem,
                                       compression, file,
                                       &size, bits);
      break;

    case CBF_BYTE_OFFSET:
    
      errorcode = cbf_compress_byte_offset (source, elsize, elsign, nelem,
                                            compression, file, 
                                            &size, bits);
      break;

    case CBF_PREDICTOR:
    
      errorcode = cbf_compress_predictor (source, elsize, elsign, nelem,
                                          compression, file, 
                                          &size, bits);
      break;

    case CBF_NONE:
    
      errorcode = cbf_compress_none (source, elsize, elsign, nelem,
                                     compression, file, 
                                     &size, bits);
      break;

  default:
  
      errorcode = CBF_ARGUMENT;
  }
  
  
    /* Add the compressed size */
    
  if (compressedsize)
  
    *compressedsize += size;


    /* Flush the buffers */

  errorcode |= cbf_flush_bits (file);
  

    /* Get the digest? */
    
  if (digest)
  
     errorcode |= cbf_end_digest (file, digest);


    /* Done */

  return errorcode;
}


  /* Get the parameters of an array (read up to the start of the table) */
  
int cbf_decompress_parameters (int          *eltype, 
                               size_t       *elsize, 
                               int          *elsigned, 
                               int          *elunsigned,
                               size_t       *nelem, 
                               int          *minelem, 
                               int          *maxelem,
                               unsigned int  compression,
                               cbf_file     *file)
{
  unsigned int nelem_file;

  int errorcode, minelement_file, maxelement_file, 
                   elsigned_file, elunsigned_file;


    /* Discard any bits in the buffers */

  cbf_failnez (cbf_reset_bits (file));
  
   /* Check compression type */

  if (compression != CBF_CANONICAL   &&
      compression != CBF_PACKED      &&
      compression != CBF_BYTE_OFFSET &&
      compression != CBF_PREDICTOR   &&
      compression != CBF_NONE)

    return CBF_FORMAT;

  if (compression == CBF_NONE)
  {
    nelem_file = 0;

    minelement_file = maxelement_file = 0;
  } 
  else 
  { 
      /* Read the number of elements (64 bits) */

    cbf_failnez (cbf_get_integer (file, (int *) &nelem_file, 0, 64))


      /* Read the minimum element (64 bits) */

    errorcode = cbf_get_integer (file, &minelement_file, 1, 64);

    if (errorcode && errorcode != CBF_OVERFLOW)

      return errorcode;


      /* Read the maximum element (64 bits) */

    errorcode = cbf_get_integer (file, &maxelement_file, 1, 64);

    if (errorcode && errorcode != CBF_OVERFLOW)

      return errorcode;
  }


    /* Update the element sign, type, minimum, maximum and number */

  elsigned_file = !(((unsigned) minelement_file) <= 
                    ((unsigned) maxelement_file) &&
                    ((signed)   minelement_file) >
                    ((signed)   maxelement_file));

  elunsigned_file = !(((signed)   minelement_file) <= 
                      ((signed)   maxelement_file) &&
                      ((unsigned) minelement_file) >  
                      ((unsigned) maxelement_file));

  if (elsigned)
  
    *elsigned = elsigned_file;

  if (elunsigned)
  
    *elunsigned = elunsigned_file;

  if (eltype)
  
    *eltype = CBF_INTEGER;

  if (elsize)

      /* Calculate the minimum number of bytes needed to hold the elements */
      
    if (minelement_file == 0 && maxelement_file == 0)
      
      *elsize = 0;
      
    else

      if ((!elsigned_file ||
          ((signed) minelement_file == (signed short) minelement_file &&
           (signed) maxelement_file == (signed short) maxelement_file)) ||
          (!elunsigned_file ||
          ((unsigned) minelement_file == (unsigned short) minelement_file &&
           (unsigned) maxelement_file == (unsigned short) maxelement_file)))
           
        if ((!elsigned_file ||
            ((signed) minelement_file == (signed char) minelement_file &&
             (signed) maxelement_file == (signed char) maxelement_file)) ||
             (!elunsigned_file ||
            ((unsigned) minelement_file == (unsigned char) minelement_file &&
             (unsigned) maxelement_file == (unsigned char) maxelement_file)))
             
          *elsize = sizeof (char);
          
        else

          *elsize = sizeof (short);
          
      else
        
        *elsize = sizeof (int);

  if (minelem)
  
    *minelem = minelement_file;

  if (maxelem)
  
    *maxelem = maxelement_file;

  if (nelem)
  
    *nelem = nelem_file;


    /* Success */

  return 0;
}


  /* Decompress an array (from the start of the table) */

int cbf_decompress (void         *destination, 
                    size_t        elsize, 
                    int           elsign, 
                    size_t        nelem, 
                    size_t       *nelem_read,
                    unsigned int  compression,
                    int           bits, 
                    int           sign,
                    cbf_file     *file)
{
  switch (compression)
  {
    case CBF_CANONICAL:
    
      return cbf_decompress_canonical (destination, elsize, elsign, nelem,
                                       nelem_read, compression, 
                                       file);

    case CBF_PACKED:
    case 0:

      return cbf_decompress_packed (destination, elsize, elsign, nelem,
                                    nelem_read, compression, 
                                    file);

    case CBF_BYTE_OFFSET:
    
      return cbf_decompress_byte_offset (destination, elsize, elsign, nelem,
                                         nelem_read, compression, 
                                         file);

    case CBF_PREDICTOR:
    
      return cbf_decompress_predictor (destination, elsize, elsign, nelem,
                                       nelem_read, compression,
                                       file);

    case CBF_NONE:
    
      return cbf_decompress_none (destination, elsize, elsign, nelem, 
                                  nelem_read, compression, 
                                  bits, sign, file);
  }


    /* Fail */

  return CBF_ARGUMENT;
}


#ifdef __cplusplus

}

#endif
