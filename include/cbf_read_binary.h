
#ifndef CBF_READ_BINARY_H
#define CBF_READ_BINARY_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_file.h"


  /* Parse a binary header looking for the size and id */
     
int cbf_parse_binaryheader (cbf_file *file, size_t *size, long *id);


#ifdef __cplusplus

}

#endif

#endif /* CBF_READ_MIME_H */
