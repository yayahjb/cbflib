
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_alloc.h"

#include <stdlib.h>
#include <string.h>


  /* Reallocate a block of memory (never lose the old block on failure) */

int cbf_realloc (void **old_block, size_t *old_nelem, size_t elsize, size_t nelem)
{
  void *new_block;

  
    /* Are the arguments valid? */
    
  if (!old_block || elsize == 0)

    return CBF_ARGUMENT;


    /* Is the size alread correct? */

  if (old_nelem)

    if (*old_nelem == nelem)

      return 0;


    /* Allocate the memory */

  if (nelem > 0)
  {
    new_block = malloc (nelem * elsize);

    if (!new_block)

      return CBF_ALLOC;
  }
  else

    new_block = NULL;


    /* Copy the old data */

  if (old_nelem)
  
    if (*old_block && *old_nelem > 0 && nelem > 0)
    {
      if (*old_nelem > nelem)
    
        *old_nelem = nelem;

      memcpy (new_block, *old_block, *old_nelem * elsize);
    }


    /* Free the old memory */

  if (*old_block)

    free (*old_block);


    /* Clear the new data */

  if (!old_nelem)

    memset (new_block, 0, nelem * elsize);
  
  else
  
    if (nelem > 0 && nelem > *old_nelem)

      memset (((char *) new_block) + *old_nelem * elsize, 0,
                                     (nelem - *old_nelem) * elsize);


    /* Replace the old data */

  *old_block = new_block;

  if (old_nelem)
  
    *old_nelem = nelem;
  

    /* Success */

  return 0;
}


  /* Allocate a block of memory */

int cbf_alloc (void **new_block, size_t *new_nelem, size_t elsize, size_t nelem)
{
    /* Are the arguments valid? */
    
  if (!new_block)

    return CBF_ARGUMENT;


    /* Initialise */

  *new_block = NULL;

  if (new_nelem)
  
    *new_nelem = 0;


    /* Allocate the memory */

  return cbf_realloc (new_block, new_nelem, elsize, nelem);
}


  /* Free a block of memory */

int cbf_free (void **old_block, size_t *old_nelem)
{
    /* Are the arguments valid? */
    
  if (!old_block)

    return CBF_ARGUMENT;


    /* Free the memory */

  if (*old_block)

    free (*old_block);

  *old_block = NULL;

  if (old_nelem)
  
    *old_nelem = 0;


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif

