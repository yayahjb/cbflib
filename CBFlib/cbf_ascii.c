
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_ascii.h"
#include "cbf_tree.h"
#include "cbf_file.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>


  /* Write an ascii value */

int cbf_write_ascii (const char *string, cbf_file *file)
{
  int end;

  unsigned int column;

  const char *c;

  char delim;


    /* Check the arguments */

  if (!string)

    string = "\300?";

  else

    if ((*string < CBF_TOKEN_WORD || *string > CBF_TOKEN_SCSTRING) && 
      (*string != CBF_TOKEN_NULL))

      return CBF_ARGUMENT;


    /* Get the current column */

  cbf_failnez (cbf_get_filecoordinates (file, NULL, &column))
  

    /* Do we need to start a new line? */

  if (column)

    if (*string == CBF_TOKEN_SCSTRING)

      cbf_failnez (cbf_write_character (file, '\n'))

    else
    {
      if ( *string ==  CBF_TOKEN_WORD ||
        *string == CBF_TOKEN_NULL )

        end = column + 3;

      else

        end = column + 1;

      for (c = string + 1; *c && end <= CBF_LINELENGTH; c++)

        if (*c == '\t')

          end = (end & ~0x07) + 8;

        else

          end = end + 1;

      if (end > CBF_LINELENGTH)

        cbf_failnez (cbf_write_character (file, '\n'))
    }


    /* Write the value */

  switch (*string)
  {
      /* Simple word? */
      
    case  CBF_TOKEN_WORD:
    case  CBF_TOKEN_NULL:
    
      cbf_failnez (cbf_write_character (file, ' '))
      
      cbf_failnez (cbf_write_string (file, string + 1))
      
      break;


      /* Single line? */

    case CBF_TOKEN_SQSTRING:
    case CBF_TOKEN_DQSTRING:

      if (*string == CBF_TOKEN_SQSTRING)

        delim = '\'';

      else

        delim = '"';

      cbf_failnez (cbf_write_character (file, ' '))
      
      cbf_failnez (cbf_write_character (file, delim))
      
      cbf_failnez (cbf_write_string (file, string + 1))
      
      cbf_failnez (cbf_write_character (file, delim))
      
      break;


      /* Multiple lines? */

    case CBF_TOKEN_SCSTRING:

      cbf_failnez (cbf_write_character (file, ';'))

      end = 1;

      for (c = string + 1; *c; c++)
      {
        if (*c == ';' && end == 0)

          cbf_failnez (cbf_write_character (file, '\\'))

        cbf_failnez (cbf_write_character (file, *c))

        if (*c == '\n')

          end = 0;

        else

          end = 1;
      }
      
      cbf_failnez (cbf_write_string (file, "\n;\n"))

      end = 0;

      break;
  }


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif

