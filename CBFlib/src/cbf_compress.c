
#ifdef __cplusplus

extern "C" {

#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_canonical.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_packed.h"


  /* Compress an array */

int cbf_compress (void *buf, size_t elsize, int elsign, size_t nelem,
                             unsigned int compression, size_t repeat,
                             cbf_file *file)
{
  if (compression == CBF_CANONICAL)

    return cbf_compress_canonical (buf, elsize, elsign, nelem,
                                   compression, repeat, file);

  if (compression == CBF_PACKED || compression == 0)

    return cbf_compress_packed (buf, elsize, elsign, nelem,
                                compression, repeat, file);


    /* Fail */

  return CBF_ARGUMENT;
}


  /* Get the parameters of an array (read up to the start of the table) */
  
int cbf_decompress_parameters (int *eltype, int *elsigned, int *elunsigned,
                               size_t *nelem, int *minelem, int *maxelem,
                               unsigned int *compression,
                               cbf_file *file)
{
  unsigned int compression_file, nelem_file;

  int errorcode, minelement_file, maxelement_file;


    /* Discard any bits in the buffers */

  cbf_failnez (cbf_reset_bits (file));
  

    /* Read the compression id (64 bits) */

  cbf_failnez (cbf_get_integer (file, (int *) &compression_file, 0, 64))

  if (compression_file != CBF_CANONICAL &&
      compression_file != CBF_PACKED)

    return CBF_FORMAT;


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


    /* Update the element sign, type, minimum, maximum and number */

  if (elsigned)
  
    *elsigned = !(((unsigned) minelement_file) <= ((unsigned) maxelement_file) &&
                  ((signed)   minelement_file) >  ((signed)   maxelement_file));

  if (elunsigned)
  
    *elunsigned = !(((signed)   minelement_file) <= ((signed)   maxelement_file) &&
                    ((unsigned) minelement_file) >  ((unsigned) maxelement_file));

  if (compression)

    *compression = compression_file;

  if (eltype)
  
    *eltype = CBF_INTEGER;
    
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

int cbf_decompress (void *buf, size_t elsize, int elsign, 
                               size_t nelem, size_t *nelem_read,
                               unsigned int compression, cbf_file *file)
{
  if (compression == CBF_CANONICAL)

    return cbf_decompress_canonical (buf, elsize, elsign, nelem, nelem_read,
                                     compression, file);

  if (compression == CBF_PACKED || compression == 0)

    return cbf_decompress_packed (buf, elsize, elsign, nelem, nelem_read,
                                  compression, file);


    /* Fail */

  return CBF_ARGUMENT;
}


#ifdef __cplusplus

}

#endif



