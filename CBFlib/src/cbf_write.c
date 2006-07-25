/**********************************************************************
 * cbf_write -- write files                                           *
 *                                                                    *
 * Version 0.6 13 January 1999                                        *
 *                                                                    *
 *            Paul Ellis (ellis@ssrl.slac.stanford.edu) and           *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 **********************************************************************/
  
/**********************************************************************
 *                               NOTICE                               *
 * Creative endeavors depend on the lively exchange of ideas. There   *
 * are laws and customs which establish rights and responsibilities   *
 * for authors and the users of what authors create.  This notice     *
 * is not intended to prevent you from using the software and         *
 * documents in this package, but to ensure that there are no         *
 * misunderstandings about terms and conditions of such use.          *
 *                                                                    *
 * Please read the following notice carefully.  If you do not         *
 * understand any portion of this notice, please seek appropriate     *
 * professional legal advice before making use of the software and    *
 * documents included in this software package.  In addition to       *
 * whatever other steps you may be obliged to take to respect the     *
 * intellectual property rights of the various parties involved, if   *
 * you do make use of the software and documents in this package,     *
 * please give credit where credit is due by citing this package,     *
 * its authors and the URL or other source from which you obtained    *
 * it, or equivalent primary references in the literature with the    *
 * same authors.                                                      *
 *                                                                    *
 * Some of the software and documents included within this software   *
 * package are the intellectual property of various parties, and      *
 * placement in this package does not in any way imply that any       *
 * such rights have in any way been waived or diminished.             *
 *                                                                    *
 * With respect to any software or documents for which a copyright    *
 * exists, ALL RIGHTS ARE RESERVED TO THE OWNERS OF SUCH COPYRIGHT.   *
 *                                                                    *
 * Even though the authors of the various documents and software      *
 * found here have made a good faith effort to ensure that the        *
 * documents are correct and that the software performs according     *
 * to its documentation, and we would greatly appreciate hearing of   *
 * any problems you may encounter, the programs and documents any     *
 * files created by the programs are provided **AS IS** without any   *
 * warranty as to correctness, merchantability or fitness for any     *
 * particular or general use.                                         *
 *                                                                    *
 * THE RESPONSIBILITY FOR ANY ADVERSE CONSEQUENCES FROM THE USE OF    *
 * PROGRAMS OR DOCUMENTS OR ANY FILE OR FILES CREATED BY USE OF THE   *
 * PROGRAMS OR DOCUMENTS LIES SOLELY WITH THE USERS OF THE PROGRAMS   *
 * OR DOCUMENTS OR FILE OR FILES AND NOT WITH AUTHORS OF THE          *
 * PROGRAMS OR DOCUMENTS.                                             *
 **********************************************************************/
 
/**********************************************************************
 *                          The IUCr Policy                           *
 *                                 on                                 *
 *     the Use of the Crystallographic Information File (CIF)         *
 *                                                                    *
 * The Crystallographic Information File (Hall, Allen & Brown,        *
 * 1991) is, as of January 1992, the recommended method for           *
 * submitting publications to Acta Crystallographica Section C. The   *
 * International Union of Crystallography holds the Copyright on      *
 * the CIF, and has applied for Patents on the STAR File syntax       *
 * which is the basis for the CIF format.                             *
 *                                                                    *
 * It is a principal objective of the IUCr to promote the use of      *
 * CIF for the exchange and storage of scientific data. The IUCr's    *
 * sponsorship of the CIF development was motivated by its            *
 * responsibility to its scientific journals, which set the           *
 * standards in crystallographic publishing. The IUCr intends that    *
 * CIFs will be used increasingly for electronic submission of        *
 * manuscripts to these journals in future. The IUCr recognises       *
 * that, if the CIF and the STAR File are to be adopted as a means    *
 * for universal data exchange, the syntax of these files must be     *
 * strictly and uniformly adhered to. Even small deviations from      *
 * the syntax would ultimately cause the demise of the universal      *
 * file concept. Through its Copyrights and Patents the IUCr has      *
 * taken the steps needed to ensure strict conformance with this      *
 * syntax.                                                            *
 *                                                                    *
 * The IUCr policy on the use of the CIF and STAR File processes is   *
 * as follows:                                                        *
 * _________________________________________________________________  *
 *                                                                    *
 *  * 1 CIFs and STAR Files may be generated, stored or transmitted,  *
 *    without permission or charge, provided their purpose is not     *
 *    specifically for profit or commercial gain, and provided that   *
 *    the published syntax is strictly adhered to.                    *
 *  * 2 Computer software may be developed for use with CIFs or STAR  *
 *    files, without permission or charge, provided it is distributed *
 *    in the public domain. This condition also applies to software   *
 *    for which a charge is made, provided that its primary function  *
 *    is for use with files that satisfy condition 1 and that it is   *
 *    distributed as a minor component of a larger package of         *
 *    software.                                                       *
 *  * 3 Permission will be granted for the use of CIFs and STAR Files *
 *    for specific commercial purposes (such as databases or network  *
 *    exchange processes), and for the distribution of commercial     *
 *    CIF/STAR software, on written application to the IUCr Executive *
 *    Secretary, 2 Abbey Square, Chester CH1 2HU, England. The        *
 *    nature, terms and duration of the licences granted will be      *
 *    determined by the IUCr Executive and Finance Committees.        *
 *                                                                    *
 * _________________________________________________________________  *
 *                                                                    *
 * In summary, the IUCr wishes to promote the use of the STAR File    *
 * concepts as a standard universal data file. It will insist on      *
 * strict compliance with the published syntax for all                *
 * applications. To assist with this compliance, the IUCr provides    *
 * public domain software for checking the logical integrity of a     *
 * CIF, and for validating the data name definitions contained        *
 * within a CIF. Detailed information on this software, and the       *
 * associated dictionaries, may be obtained from the IUCr Office at   *
 * 5 Abbey Square, Chester CH1 2HU, England.                          *
 **********************************************************************/

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
#include "cbf_write_binary.h"
#include "cbf_read_mime.h"

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

  test [0] |= strcmp (&value [1], "?") == 0;
  test [0] |= strcmp (&value [1], ".") == 0;


    /* Simple word? */

  if (!test [0] && test [4] && test [5])

    *value = CBF_TOKEN_WORD;

  else

      /* Single line? */

    if (!test [1] && (!test [2] || !test [3]))
    {
      if (!test [2])

        *value = CBF_TOKEN_SQSTRING;

      else

        *value = CBF_TOKEN_DQSTRING;
    }
    else

        /* Multiple lines */    

      *value = CBF_TOKEN_SCSTRING;


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

int cbf_write_value (cbf_node *column, unsigned int row, 
                     cbf_file *file, int isbuffer)
{
  const char *text;


    /* Check the arguments */

  if (!column)

    return CBF_ARGUMENT;

  if (row >= column->children)

    return CBF_NOTFOUND;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))


    /* Missing value? */
    
  if (!text)

    return cbf_write_ascii (text, file);


    /* Plain ASCII? */

  cbf_failnez (cbf_value_type ((char *) text))

  if (*text == CBF_TOKEN_WORD     ||
      *text == CBF_TOKEN_SQSTRING ||
      *text == CBF_TOKEN_DQSTRING ||
      *text == CBF_TOKEN_SCSTRING ||
      *text == CBF_TOKEN_NULL)

    return cbf_write_ascii (text, file);


    /* Plain binary? */

  if (*text == CBF_TOKEN_BIN || *text == CBF_TOKEN_TMP_BIN)

    return cbf_write_binary (column, row, file, isbuffer);


    /* Undecoded MIME? */
  
  if (*text == CBF_TOKEN_MIME_BIN)
  {
      /* Convert the value to a normal binary section */
      
    cbf_failnez (cbf_mime_temp (column, row))
  
    return cbf_write_binary (column, row, file, isbuffer);
  }


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
          if (category->child [last]->children != 
            category->child [first]->children)

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

          cbf_failnez (cbf_write_value (category->child [column], row, 
                                                       file, isbuffer))

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

      cbf_failnez (cbf_write_string (file, "###CBF: VERSION 0.6\n"))
    
      if (file->write_encoding & ENC_NONE)

        cbf_failnez (cbf_write_string (file, 
                             "# CBF file written by cbflib v0.6\n"))
      else

        cbf_failnez (cbf_write_string (file, 
                             "# CIF file written by cbflib v0.6\n"))

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


    /* Flush the buffers */

  return cbf_flush_characters (file);
}


#ifdef __cplusplus

}

#endif
