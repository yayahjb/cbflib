
#ifndef CBF_PACKED_H
#define CBF_PACKED_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Compress an array */

int cbf_compress_packed (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file);


  /* Decompress an array */

int cbf_decompress_packed (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file);

  /* Copy an array without compression */
  
int cbf_compress_none (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file);

  /* Recover an array without decompression */

int cbf_decompress_none (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file);

  /* Compress an array with byte offset algorithm */
  
int cbf_compress_byte_off (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file);

  /* Decompress an array with byte offset algorithm */

int cbf_decompress_byte_off (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file);

  /* Compress an array with Predictor-Huffman alogrithm */
  
int cbf_compress_predict (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file);

  /* Decompress an array with Predictor-Huffman algorithm */

int cbf_decompress_predict (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_PACKED_H */
