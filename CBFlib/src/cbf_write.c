
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_ascii.h"
#include "cbf_binary.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_tree.h"
#include "cbf_write.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>


  /* Check the value type */

int cbf_value_type (char *value)
{
  int test [6], C, count;


    /* Is the value missing? */

  if (!value)

    return 0;


    /* Is the value valid? */

  if ((*value & '\200') != '\200')

    return CBF_ARGUMENT;
    

    /* Has the value already been checked? */

  if ((value [0] & '\300') == '\300')

    return 0;
    

    /* Properties */

  memset (test, 0, sizeof (test));

  for (count = 1; value [count]; count++)
  {
    C = toupper (value [count]);

    test [0] |= isspace (C);

    test [1] |= C == '\n';
    test [2] |= C == '\'';
    test [3] |= C == '"';

    if (count <= 5)
    {
      test [4] |= C != " DATA_" [count];
      test [5] |= C != " LOOP_" [count];

      if (count <= 1)

        test [0] |= C == '_' || C == '\'' || C == '"' || C == '#';
    }
  }

  test [0] |= strcmp (value, "?") == 0;


    /* Simple word? */

  if (!test [0] && test [4] && test [5])

    *value = '\300';

  else

      /* Single line? */

    if (!test [1] && (!test [2] || !test [3]))
    {
      if (!test [2])

        *value = '\301';

      else

        *value = '\302';
    }
    else

        /* Multiple lines */    

      *value = '\303';


    /* Success */

  return 0;
}


  /* Write a datablock name to a file */

int cbf_write_datablockname (const cbf_node *datablock, cbf_file *file)
{
    /* Does the node exist? */

  if (!datablock)

    return CBF_ARGUMENT;


    /* Write the name */

  if (datablock->name)
  {
    cbf_failnez (cbf_write_string (file, "\ndata_"))

    cbf_failnez (cbf_write_string (file, datablock->name))

    cbf_failnez (cbf_write_character (file, '\n'))
  }
  else

    if (datablock->children)

      cbf_failnez (cbf_write_string (file, "\ndata_\n"))


    /* Success */

  return 0;
}


  /* Write an item name to a file */

int cbf_write_itemname (const cbf_node *column, cbf_file *file)
{
  cbf_node *category;


    /* Get the category */
      
  cbf_failnez (cbf_find_parent (&category, column, CBF_CATEGORY))


    /* Check that the name is valid */

  if (!category->name && !column->name)

    return CBF_ARGUMENT;


    /* Write the category name */

  cbf_failnez (cbf_write_character (file, '_'))

  if (category->name)
  {
    cbf_failnez (cbf_write_string (file, category->name))

    cbf_failnez (cbf_write_character (file, '.'))
  }


    /* Write the column name */

  if (column->name)

    cbf_failnez (cbf_write_string (file, column->name))


    /* Success */

  return 0;
}


  /* Write a value to a file */

int cbf_write_value (cbf_node *column, unsigned int row, cbf_file *file, int isbuffer)
{
  const char *text;


    /* Check the arguments */

  if (!column)

    return CBF_ARGUMENT;

  if (row >= column->children)

    return CBF_NOTFOUND;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))

  if (!text)

    return cbf_write_ascii (text, file);

  cbf_failnez (cbf_value_type ((char *) text))

  if (*text >= '\300' && *text <= '\303')

    return cbf_write_ascii (text, file);

  if (*text >= '\304' && *text <= '\305')

    return cbf_write_binary (column, row, file, isbuffer);


    /* Fail */

  return CBF_ARGUMENT;
}


  /* Write a category to a file */

int cbf_write_category (const cbf_node *category, cbf_file *file, int isbuffer)
{
  unsigned int count, first, last, column, columns, row;
  
  int loop;


    /* Check the arguments */

  if (!category)

    return CBF_ARGUMENT;

    
    /* Print out columns of the same length in loops */

  for (first = 0, loop = 1; first < category->children; first = last)
  {
    columns = 1;
    
    if (category->child [first])
    {
      for (last = first + 1; last < category->children; last++)

        if (category->child [last])
        {
          if (category->child [last]->children != category->child [first]->children)

            break;
            
          columns++;
        }


        /* Make a loop? */

      if (columns > 1 || category->child [first]->children > 1)
      {
        cbf_failnez (cbf_write_string (file, "\nloop_\n"))

        loop = 1;
      }
      else
      {
        if (loop)

          cbf_failnez (cbf_write_character (file, '\n'))

        loop = 0;
      }


        /* Write the items */

      for (count = first; count < last; count++)
      {
        cbf_failnez (cbf_write_itemname (category->child [count], file))
          
        if (loop)

          cbf_failnez (cbf_write_character (file, '\n'))
      }


        /* Write the values */

      for (row = 0; row < category->child [first]->children; row++)
      {
        for (column = first; column < last; column++)

          cbf_failnez (cbf_write_value (category->child [column], row, file, isbuffer))

        cbf_failnez (cbf_get_filecoordinates (file, NULL, &column))

        if (column)

          cbf_failnez (cbf_write_character (file, '\n'))
      }
    }
  }


    /* Success */

  return 0;
}


  /* Write a node to a file */

int cbf_write_node (const cbf_node *node, cbf_file *file, int isbuffer)
{
  unsigned int count;
  

    /* Follow any links */

  node = cbf_get_link (node);


    /* Does the node exist? */

  if (!node)

    return CBF_ARGUMENT;


    /* Node type */

  switch (node->type)
  {
    case CBF_ROOT:

      cbf_failnez (cbf_write_string (file, "# CBF file written by cbflib v0.1\n"))

      break;

    case CBF_DATABLOCK:

      cbf_failnez (cbf_write_datablockname (node, file))

      break;

    case CBF_CATEGORY:

      cbf_failnez (cbf_write_category (node, file, isbuffer))

      break;

    default:

      return CBF_ARGUMENT;
  }


    /* Write the children */

  if (node->type == CBF_ROOT || node->type == CBF_DATABLOCK)

    for (count = 0; count < node->children; count++)

      cbf_failnez (cbf_write_node (node->child [count], file, isbuffer))


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif



