
#ifndef CBF_READ_MIME_H
#define CBF_READ_MIME_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_file.h"


  /* Convert a MIME-encoded binary section to a temporary binary section */

int cbf_mime_temp (cbf_node *column, unsigned int row);

  /* Find non-blank length of a line */

int cbf_nblen (const char *line, int *nblen);

  /* Convert a MIME-encoded binary section to a normal binary section */
     
int cbf_read_mime (cbf_file *infile, cbf_file   *outfile,
                                     size_t     *size,
                                     long       *id,
                                     char       *old_digest,
                                     char       *new_digest);
  

  /* Parse the MIME header looking for values of type:
  
     Content-Type:
     Content-Transfer-Encoding:
     X-Binary-Size:
     X-Binary-ID:
     Content-MD5: */
     
int cbf_parse_mimeheader (cbf_file *file, int        *encoding,
                                          size_t     *size,
                                          long       *id,
                                          char       *digest,
                                 unsigned int        *compression,
                                          int        *bits,
                                          int        *sign);


#ifdef __cplusplus

}

#endif

#endif /* CBF_READ_MIME_H */
