
#ifndef CBF_PREDICTOR_H
#define CBF_PREDICTOR_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>

#include "cbf_file.h"


  /* Compress an array with the Predictor-Huffman algorithm */
  
int cbf_compress_predictor (void         *source, 
                            size_t        elsize, 
                            int           elsign, 
                            size_t        nelem, 
                            unsigned int  compression, 
                            cbf_file     *file, 
                            size_t       *compressedsize,
                            int          *storedbits);


  /* Decompress an array with the Predictor-Huffman algorithm */

int cbf_decompress_predictor (void         *destination, 
                              size_t        elsize, 
                              int           elsign, 
                              size_t        nelem, 
                              size_t       *nelem_read,
                              unsigned int  compression, 
                              cbf_file     *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_PREDICTOR_H */
