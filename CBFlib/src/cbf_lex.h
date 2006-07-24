
#ifndef CBF_LEX_H
#define CBF_LEX_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"
#include "cbf_file.h"
#include "cbf_stx.h"

#include <stdio.h>


  /* Get the next token */

int cbf_lex (YYSTYPE *val, cbf_file *file);


#ifdef __cplusplus

}

#endif

#endif /* CBF_LEX_H */

