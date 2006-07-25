
#ifndef CBF_PACKED_H
#define CBF_PACKED_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Compress an array */

int cbf_compress_packed (void         *source, 
                         size_t        elsize, 
                         int           elsign, 
                         size_t        nelem, 
                         unsigned int  compression, 
                         cbf_file     *file, 
                         size_t       *compressedsize,
                         int          *storedbits);


  /* Decompress an array */

int cbf_decompress_packed (void         *destination, 
                           size_t        elsize, 
                           int           elsign, 
                           size_t        nelem, 
                           size_t       *nelem_read,
                           unsigned int  compression, 
                           cbf_file     *file);

#ifdef __cplusplus

}

#endif

#endif /* CBF_PACKED_H */
