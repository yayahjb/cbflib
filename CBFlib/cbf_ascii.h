
#ifndef CBF_ASCII_H
#define CBF_ASCII_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_file.h"


  /* Write an ascii value */

int cbf_write_ascii (const char *string, cbf_file *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_ASCII_H */

