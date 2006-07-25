
#ifndef CBF_BINARY_H
#define CBF_BINARY_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"


  /* Parse a binary text value */
  
int cbf_get_bintext (cbf_node  *column, unsigned int row,
                     int       *type,
                     int       *id, 
                     cbf_file **file,
                     long      *start,
                     size_t    *size,
                     int       *checked_digest,
                     char      *digest,
                     int       *elsize,
                     int       *elsign,
            unsigned int       *compression);
   

  /* Set a binary text value */
  
int cbf_set_bintext (cbf_node *column, unsigned int row,
                     int         type,
                     int         id, 
                     cbf_file   *file,
                     long        start,
                     long        size,
                     int         checked_digest,
                     const char *digest,
                     int         elsize,
                     int         elsign,
            unsigned int         compression);


  /* Check for a binary value */

int cbf_is_binary (cbf_node *column, unsigned int row);


  /* Is this an encoded binary value? */

int cbf_is_mimebinary (cbf_node *column, unsigned int row);


  /* Free a value */

int cbf_free_value (cbf_context *context, cbf_node *column, unsigned int row);


  /* Set a binary value */
  
int cbf_set_binary (cbf_node *column, unsigned int row,
                    unsigned int compression,
                    int binary_id, void *value, size_t elsize, int elsign,
                    size_t nelem);

                    
  /* Get the parameters of a binary value */
  
int cbf_binary_parameters (cbf_node *column, 
                           unsigned int row, unsigned int *compression,
                           int *binary_id, 
                           int *eltype, size_t *elsize, 
                           int *elsigned, 
                           int *elunsigned,
                           size_t *nelem,
                           int *minelem, int *maxelem);

                    
  /* Get a binary value */
  
int cbf_get_binary (cbf_node *column, unsigned int row, int *binary_id,
                    void *value, size_t elsize, int elsign,
                    size_t nelem, size_t *nelem_read);

                    
#ifdef __cplusplus

}

#endif

#endif /* CBF_BINARY_H */

