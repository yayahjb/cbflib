
#ifndef CBF_COMPRESS_H
#define CBF_COMPRESS_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Compress an array */

int cbf_compress (void *buf, size_t elsize, int elsign, size_t nelem,
                             unsigned int compression, size_t repeat,
                             cbf_file *file, size_t *compressedsize,
                             char *digest);


  /* Get the parameters of an array (read up to the start of the table) */
  
int cbf_decompress_parameters (int *eltype, size_t *elsize, 
                               int *elsigned, int *elunsigned,
                               size_t *nelem, 
                               int *minelem, int *maxelem,
                               unsigned int *compression,
                               size_t *repeat,
                               long *size,
                               cbf_file *file);


  /* Decompress an array (from the start of the table) */

int cbf_decompress (void *buf, size_t elsize, int elsign, 
                               size_t nelem, size_t *nelem_read,
                               unsigned int compression, 
                               size_t repeat, cbf_file *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_COMPRESS_H */
