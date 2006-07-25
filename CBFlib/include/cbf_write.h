
#ifndef CBF_WRITE_H
#define CBF_WRITE_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"


  /* Write a node to a stream */

int cbf_write_node (const cbf_node *node, cbf_file *file, int isbuffer);


#ifdef __cplusplus

}

#endif

#endif /* CBF_WRITE_H */

