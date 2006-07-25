
#ifndef CBF_WRITE_BINARY_H
#define CBF_WRITE_BINARY_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"

                    
  /* Write a binary value */
  
int cbf_write_binary (cbf_node *column, unsigned int row, 
                                        cbf_file *file, 
                                        int isbuffer);

                    
#ifdef __cplusplus

}

#endif

#endif /* CBF_WRITE_BINARY_H */

