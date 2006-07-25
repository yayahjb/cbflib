
#ifndef CBF_READ_MIME_H
#define CBF_READ_MIME_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_file.h"


  /* Convert a MIME-encoded binary section to a temporary binary section */

int cbf_mime_temp (cbf_node *column, unsigned int row);


  /* Convert a MIME-encoded binary section to a normal binary section */
     
int cbf_read_mime (cbf_file *infile, cbf_file   *outfile,
                                     size_t     *size,
                                     long       *id,
                                     char       *digest);
  
  /* Skip whitespace and comments in a MIME header */  
  /* Derived from mpack routine SkipWhitespace     */ 

  /* 
     line is a pointer to a pointer to a null-terminated single line
     curpoint is a pointer to a pointer to the current position in line
     nblen is a pointer to the non-blank length of line
     freshline is a pointer to a logical, 1 if a fresh line is loaded
  */
     

int cbf_skip_whitespace (cbf_file *file, char **line, 
      char **curpoint, unsigned int *nblen, int *freshline );

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
                                 unsigned int        *compression);


#ifdef __cplusplus

}

#endif

#endif /* CBF_READ_MIME_H */
