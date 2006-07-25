
#ifndef CBF_UNCOMPRESSED_H
#define CBF_UNCOMPRESSED_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Copy an array without compression */
  
int cbf_compress_none (void *buf, size_t elsize, int elsign, size_t nelem,
                       unsigned int compression, size_t repeat,
                       cbf_file *file, size_t *compressedsize);


  /* Recover an array without decompression */

int cbf_decompress_none (void *buf, size_t elsize, int elsign, 
                         size_t nelem, size_t *nelem_read,
                         unsigned int compression, 
                         size_t elbits, cbf_file *file);

#ifdef __cplusplus

}

#endif

#endif /* CBF_UNCOMPRESSED_H */
