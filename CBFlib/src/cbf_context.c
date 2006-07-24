
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_context.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>


  /* Create and initialise a context */

int cbf_make_context (cbf_context **context)
{
    /* Allocate the memory */

  cbf_failnez (cbf_alloc ((void **) context, NULL, sizeof (cbf_context), 1))


    /* Initialise */
    
  (*context)->temporary = NULL;

  (*context)->connections = 1;


    /* Success */

  return 0;
}


  /* Free a context */

int cbf_free_context (cbf_context **context)
{
  int errorcode;

  errorcode = 0;

  if (context)

    if (*context)
    {
      if ((*context)->temporary)

        errorcode = cbf_free_file (&(*context)->temporary);

      errorcode |= cbf_free ((void **) context, NULL);
    }


    /* Success? */

  return errorcode;
}


  /* Add a context connection */

int cbf_add_contextconnection (cbf_context **context)
{
    /* Does the context pointer exist? */

  if (!context)

    return CBF_ARGUMENT;


    /* Does the context exist? */

  if (*context)
  {
    (*context)->connections++;

    return 0;
  }


    /* Create a new context */

  return cbf_make_context (context);
}


  /* Remove a context connection */

int cbf_delete_contextconnection (cbf_context **context)
{
    /* Does the context pointer exist? */

  if (!context)

    return CBF_ARGUMENT;


    /* Does the context exist? */

  if (!*context)

    return CBF_ARGUMENT;


    /* Remove a connection */

  (*context)->connections--;


    /* Delete the context? */

  if ((*context)->connections == 0)

    return cbf_free_context (context);


    /* Success */

  return 0;
}


  /* Open a temporary file connection */

int cbf_open_temporary (cbf_context *context, cbf_file **temporary)
{
  FILE *stream;

  int errorcode;

  
    /* Check the arguments */

  if (!context || !temporary)

    return CBF_ARGUMENT;


    /* Does a temporary file already exist? */

  if (context->temporary)
  {
    cbf_failnez (cbf_add_fileconnection (&context->temporary, NULL))

    *temporary = context->temporary;

    return 0;
  }


    /* Create the temporary file */

  stream = tmpfile ();

  if (!stream)

    return CBF_FILEOPEN;

  errorcode = cbf_make_file (&context->temporary, stream);
  
  if (errorcode)
  {
    if (fclose (stream))

      errorcode |= CBF_FILECLOSE;

    return errorcode;
  }


    /* Open a connection */
    
  return cbf_open_temporary (context, temporary);
}


  /* Close a temporary file connection */

int cbf_close_temporary (cbf_context *context, cbf_file **temporary)
{
    /* Check the arguments */

  if (!context || !temporary)

    return CBF_ARGUMENT;

  if (!*temporary)

    return CBF_ARGUMENT;


    /* Check that the temporary file matches */

  if (context->temporary != *temporary)

    return CBF_NOTFOUND;
    

    /* Delete the connection */

  cbf_failnez (cbf_delete_fileconnection (&context->temporary))

  *temporary = NULL;


    /* Is there only one connection left? */

  if (context->temporary)

    if (cbf_file_connections (context->temporary) == 1)

      cbf_failnez (cbf_free_file (&context->temporary))


    /* Success */

  return 0;
}


  /* Copy a string */

const char *cbf_copy_string (cbf_context *context, const char *string, char type)
{
  char *new_string;

  if (string)

    if (type)
    {
      if (cbf_alloc ((void **) &new_string, NULL, sizeof (char), strlen (string) + 2) == 0)
      {
        *new_string = type;
        
        strcpy (new_string + 1, string);

        return new_string;
      }
    }
    else

      if (cbf_alloc ((void **) &new_string, NULL, sizeof (char), strlen (string) + 1) == 0)
      {
        strcpy (new_string, string);

        return new_string;
      }

 
    /* Fail */

  return NULL;
}


  /* Free a string */

void cbf_free_string (cbf_context *context, const char *string)
{
  cbf_free ((void **) &string, NULL);
}


#ifdef __cplusplus

}

#endif

