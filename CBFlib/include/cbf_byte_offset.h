
#ifndef CBF_BYTE_OFFSET_H
#define CBF_BYTE_OFFSET_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Compress an array with the byte-offset algorithm */
  
int cbf_compress_byte_offset (void *buf, size_t elsize, int elsign, 
                              size_t nelem, unsigned int compression, 
                              size_t repeat, cbf_file *file, 
                              size_t *compressedsize);


  /* Decompress an array with the byte-offset algorithm */

int cbf_decompress_byte_offset (void *buf, size_t elsize, int elsign, 
                                size_t nelem, size_t *nelem_read,
                                unsigned int compression, 
                                size_t repeat, cbf_file *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_BYTE_OFFSET_H */
