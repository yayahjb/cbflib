
#ifndef CBF_CONTEXT_H
#define CBF_CONTEXT_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_file.h"

#include <stdio.h>


  /* Context structure */

typedef struct
{
  cbf_file *temporary;       /* Temporary file */

  unsigned int connections;  /* Number of pointers to this structure */
}
cbf_context;


  /* Create and initialise a context */

int cbf_make_context (cbf_context **context);


  /* Free a context */

int cbf_free_context (cbf_context **context);


  /* Add a context connection */

int cbf_add_contextconnection (cbf_context **context);


  /* Remove a context connection */

int cbf_delete_contextconnection (cbf_context **context);


  /* Open a temporary file connection */

int cbf_open_temporary (cbf_context *context, cbf_file **temporary);


  /* Close a temporary file connection */

int cbf_close_temporary (cbf_context *context, cbf_file **temporary);


  /* Copy a string */

const char *cbf_copy_string (cbf_context *context, const char *string, char type);


  /* Free a string */

void cbf_free_string (cbf_context *context, const char *string);


#ifdef __cplusplus

}

#endif

#endif /* CBF_CONTEXT_H */

