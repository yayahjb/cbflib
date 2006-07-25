
#ifndef CBF_WRITE_H
#define CBF_WRITE_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"


  /* Get the value type of an ascii string */

int cbf_get_value_type(const char *value, const char **value_type);


  /* Set the value type of an ascii string */

int cbf_set_value_type(char *value, const char *value_type);


  /* Write a node to a stream */

int cbf_write_node (const cbf_node *node, cbf_file *file, int isbuffer);


#ifdef __cplusplus

}

#endif

#endif /* CBF_WRITE_H */

