
#ifndef CBF_STRING_H
#define CBF_STRING_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>


  /* Case-insensitive strcmp */

int cbf_cistrcmp (const char *s1, const char *s2);


  /* Case-insensitive strncmp */

int cbf_cistrncmp (const char *s1, const char *s2, size_t n);


#ifdef __cplusplus

}

#endif

#endif /* CBF_FILE_H */

