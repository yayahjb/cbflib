
#ifndef CBF_ALLOC_H
#define CBF_ALLOC_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdlib.h>


  /* Allocate a block of memory */

int cbf_alloc (void **new_block, size_t *new_nelem, size_t elsize, size_t nelem);


  /* Reallocate a block of memory (never lose the old block on failure) */

int cbf_realloc (void **old_block, size_t *old_nelem, size_t elsize, size_t nelem);


  /* Free a block of memory */

int cbf_free (void **old_block, size_t *old_nelem);


#ifdef __cplusplus

}

#endif

#endif /* CBF_ALLOC_H */

