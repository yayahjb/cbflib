
#ifndef CBF_CANONICAL_H
#define CBF_CANONICAL_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Compress an array */

int cbf_compress_canonical (void         *source, 
                            size_t        elsize, 
                            int           elsign, 
                            size_t        nelem, 
                            unsigned int  compression,
                            cbf_file     *file, 
                            size_t       *compressedsize,
                            int          *storedbits);


  /* Decompress an array (from the start of the table) */

int cbf_decompress_canonical (void         *destination, 
                              size_t        elsize, 
                              int           elsign, 
                              size_t        nelem, 
                              size_t       *nelem_read,
                              unsigned int  compression, 
                              cbf_file     *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_CANONICAL_H */
