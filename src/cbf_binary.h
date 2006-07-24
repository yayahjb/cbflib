
#ifndef CBF_BINARY_H
#define CBF_BINARY_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"


  /* Check for a binary value */

int cbf_is_binary (const char *value);


  /* Free a value */

int cbf_free_value (cbf_context *context, const char **value);


  /* Set a binary value */
  
int cbf_set_binary (cbf_node *column, unsigned int row,
                    unsigned int compression, size_t repeat,
                    int binary_id, void *value, size_t elsize, int elsign,
                    size_t nelem);

                    
  /* Get the parameters of a binary value */
  
int cbf_binary_parameters (cbf_node *column, 
                           unsigned int row, int *binary_id, 
                           int *eltype, int *elsigned, int *elunsigned,
                           size_t *nelem,
                           int *minelem, int *maxelem);

                    
  /* Get a binary value */
  
int cbf_get_binary (cbf_node *column, unsigned int row, int *binary_id,
                    void *value, size_t elsize, int elsign,
                    size_t nelem, size_t *nelem_read);

                    
  /* Write a binary value */
  
int cbf_write_binary (cbf_node *column, unsigned int row, cbf_file *file, int isbuffer);

                    
#ifdef __cplusplus

}

#endif

#endif /* CBF_BINARY_H */

