
#ifndef CBF_CODES_H
#define CBF_CODES_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_file.h"


  /* Calculate the MD5 digest (25 characters) of a block of data */

int cbf_md5digest (cbf_file *file, size_t size, char *digest);


  /* Convert binary data to quoted-printable text */

int cbf_toqp (cbf_file *infile, cbf_file *outfile, size_t size);


  /* Convert binary data to base-64 text */

int cbf_tobase64 (cbf_file *infile, cbf_file *outfile, size_t size);


  /* Convert binary data to base-8/base-10/base-16 text */

int cbf_tobasex (cbf_file *infile, cbf_file *outfile, size_t size,
                                                      size_t elsize,
                                                      unsigned int base);


  /* Convert quoted-printable text to binary data */

int cbf_fromqp (cbf_file *infile, cbf_file *outfile, size_t size, 
                                                     size_t *readsize,
                                                       char *digest);


  /* Convert base-64 text to binary data */

int cbf_frombase64 (cbf_file *infile, cbf_file *outfile, size_t size,
                                                         size_t *readsize,
                                                           char *digest);


  /* Convert base-8/base-10/base-16 text to binary data */

int cbf_frombasex (cbf_file *infile, cbf_file *outfile, size_t size, 
                                                        size_t *readsize,
                                                          char *digest);


#ifdef __cplusplus

}

#endif

#endif /* CBF_CODES_H */


