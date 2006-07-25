/**********************************************************************
 * cbf -- cbflib API functions                                        *
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
#include "cbf_alloc.h"
#include "cbf_binary.h"
#include "cbf_write.h"

#include <stdlib.h>
#include <string.h>

int cbf_parse (void *context);


  /* Create a handle */

int cbf_make_handle (cbf_handle *handle)
{
  int errorcode;
  
  cbf_failnez (cbf_alloc ((void **) handle, NULL, sizeof (cbf_handle), 1))

  errorcode = cbf_make_node (&(*handle)->node, CBF_ROOT, NULL, NULL);

  if (errorcode)

    return errorcode | cbf_free ((void **) handle, NULL);
    
  (*handle)->row = 0;

  (*handle)->search_row = 0;

  return 0;
}


  /* Free a handle */

int cbf_free_handle (cbf_handle handle)
{
  int errorcode;
  
  if (handle)
  {
    errorcode = cbf_free_node (handle->node);

    return errorcode | cbf_free ((void **) &handle, NULL);
  }

  return 0;
}


  /* Read a file */

int cbf_read_file (cbf_handle handle, FILE *stream, int headers)
{
  cbf_file *file;

  cbf_node *node;

  void *parse [2];

  int errorcode;

  unsigned int children;

  const char *name;


    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

  if (((headers & (MSG_DIGEST | MSG_DIGESTNOW)) && (headers & MSG_NODIGEST)))

    return CBF_ARGUMENT;

  
    /* Delete the old datablocks */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))

  cbf_failnez (cbf_set_children (node, 0))

  handle->node = node;

  
    /* Create the input file */

  cbf_failnez (cbf_make_file (&file, stream))


    /* Defaults */
    
  if ((headers & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW)) == 0)

    headers |= (HDR_DEFAULT & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW));

  if (headers & MSG_DIGESTNOW)
  
    headers |= MSG_DIGEST;


    /* Copy the flags */
    
  file->read_headers = headers;


    /* Parse the file */
    
  parse [0] = file;
  parse [1] = handle->node;
  
  errorcode = cbf_parse (parse);


    /* Delete the first datablock if it's empty */

  if (!errorcode)
  {
    errorcode = cbf_get_child (&node, node, 0);
    
    if (!errorcode)
    {
      errorcode = cbf_get_name (&name, node);

      if (!errorcode && !name)
      {
        errorcode = cbf_count_children (&children, node);

        if (!errorcode && !children)

          errorcode = cbf_free_node (node);
      }
    }
    else

      if (errorcode == CBF_NOTFOUND)

        errorcode = 0;
  }


    /* Disconnect the file */

  return errorcode | cbf_delete_fileconnection (&file);
}


  /* Write a file */

int cbf_write_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding)
{
  cbf_file *file;

  cbf_node *node;

  int errorcode;

  int little;


    /* CIF or CBF? */
    
  if (ciforcbf == CIF)

    encoding = encoding & ~ENC_NONE;

  else

    encoding = (encoding & ~(ENC_BASE8   |
                             ENC_BASE10  |
                             ENC_BASE16  |
                             ENC_BASE64  |
                             ENC_QP      |
                             ENC_FORWARD |
                             ENC_BACKWARD)) | ENC_NONE | ENC_CRTERM 
                                                       | ENC_LFTERM;


    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;
    
  if (((headers  & MIME_HEADERS)  && (headers  & PLAIN_HEADERS)) ||
      ((headers  & MSG_DIGEST)    && (headers  & MSG_NODIGEST))  ||
      ((headers  & MSG_DIGEST)    && (headers  & PLAIN_HEADERS)) ||
      ((headers  & MSG_DIGESTNOW) && (headers  & MSG_NODIGEST))  ||
      ((headers  & MSG_DIGESTNOW) && (headers  & PLAIN_HEADERS)) ||
      ((encoding & ENC_FORWARD)   && (encoding & ENC_BACKWARD)))

    return CBF_ARGUMENT;
    
  if (((encoding & ENC_NONE)   > 0) +
      ((encoding & ENC_BASE8)  > 0) +
      ((encoding & ENC_BASE10) > 0) +
      ((encoding & ENC_BASE16) > 0) +
      ((encoding & ENC_BASE64) > 0) +
      ((encoding & ENC_QP)     > 0) > 1)

    return CBF_ARGUMENT;
 

    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Create the file */

  cbf_failnez (cbf_make_file (&file, stream))


    /* Defaults */
    
  if (headers & (MSG_DIGEST | MSG_DIGESTNOW))
  
    headers |= MIME_HEADERS;
    
  else
    
    if ((headers & (MIME_HEADERS | PLAIN_HEADERS)) == 0)

      headers |= (HDR_DEFAULT & (MIME_HEADERS | PLAIN_HEADERS));

  if (headers & PLAIN_HEADERS)
  
    headers |= MSG_NODIGEST;
    
  else

    if ((headers & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW)) == 0)

      headers |= (HDR_DEFAULT & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW));

  if (headers & MSG_DIGESTNOW)
  
    headers |= MSG_DIGEST;

  if ((encoding & (ENC_NONE   |
                   ENC_BASE8  |
                   ENC_BASE10 |
                   ENC_BASE16 |
                   ENC_BASE64 |
                   ENC_QP)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_NONE   |
                                ENC_BASE8  |
                                ENC_BASE10 |
                                ENC_BASE16 |
                                ENC_BASE64 |
                                ENC_QP));
                               
  if ((encoding & (ENC_CRTERM | ENC_LFTERM)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_CRTERM | ENC_LFTERM));

  if ((encoding & (ENC_FORWARD | ENC_BACKWARD)) == 0)
  
    encoding |= (ENC_DEFAULT & (ENC_FORWARD | ENC_BACKWARD));


    /* Copy the flags */
    
  file->write_headers  = headers;
  file->write_encoding = encoding;
 

    /* Write the file */

  errorcode = cbf_write_node (node, file, isbuffer);


    /* Free the file structure but don't close the file? */

  if (!isbuffer)

    file->stream = NULL;


    /* Disconnect the file */

  return errorcode | cbf_delete_fileconnection (&file);
}


  /* Add a data block */

int cbf_new_datablock (cbf_handle handle, const char *datablockname)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Copy the name */

  if (datablockname)
  {
    datablockname = cbf_copy_string (NULL, datablockname, 0);

    if (!datablockname)

      return CBF_ALLOC;
  }


    /* Add a datablock */

  errorcode = cbf_make_child (&node, node, CBF_DATABLOCK, datablockname);

  if (errorcode)
  {
    cbf_free_string (NULL, datablockname);

    return errorcode;
  }


    /* Success */

  handle->node = node;

  return 0;
}


  /* Add a data block, allowing for duplicates */

int cbf_force_new_datablock (cbf_handle handle, const char *datablockname)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Copy the name */

  if (datablockname)
  {
    datablockname = cbf_copy_string (NULL, datablockname, 0);

    if (!datablockname)

      return CBF_ALLOC;
  }


    /* Add a datablock */

  errorcode = cbf_make_new_child (&node, node, CBF_DATABLOCK, datablockname);

  if (errorcode)
  {
    cbf_free_string (NULL, datablockname);

    return errorcode;
  }


    /* Success */

  handle->node = node;

  return 0;
}


  /* Add a category to the current data block */

int cbf_new_category (cbf_handle handle, const char *categoryname)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Copy the name */

  if (categoryname)
  {
    categoryname = cbf_copy_string (NULL, categoryname, 0);

    if (!categoryname)

      return CBF_ALLOC;
  }


    /* Add a category */

  errorcode = cbf_make_child (&node, node, CBF_CATEGORY, categoryname);

  if (errorcode)
  {
    cbf_free_string (NULL, categoryname);

    return errorcode;
  }


    /* Success */

  handle->node = node;

  return 0;
}


  /* Add a category to the current data block, allowing for duplicates */

int cbf_force_new_category (cbf_handle handle, const char *categoryname)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Copy the name */

  if (categoryname)
  {
    categoryname = cbf_copy_string (NULL, categoryname, 0);

    if (!categoryname)

      return CBF_ALLOC;
  }


    /* Add a category */

  errorcode = cbf_make_new_child (&node, node, CBF_CATEGORY, categoryname);

  if (errorcode)
  {
    cbf_free_string (NULL, categoryname);

    return errorcode;
  }


    /* Success */

  handle->node = node;

  return 0;
}


  /* Add a column to the current category */
  
int cbf_new_column (cbf_handle handle, const char *columnname)
{
  cbf_node *node;

  int errorcode;

  unsigned int rows;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* How many rows does this category have? */

  cbf_failnez (cbf_count_rows (handle, &rows))
  

    /* Copy the name */

  if (columnname)
  {
    columnname = cbf_copy_string (NULL, columnname, 0);

    if (!columnname)

      return CBF_ALLOC;
  }


    /* Add a column */

  errorcode = cbf_make_child (&node, node, CBF_COLUMN, columnname);

  if (errorcode)
  {
    cbf_free_string (NULL, columnname);

    return errorcode;
  }


    /* Set the number of rows */

  errorcode = cbf_set_children (node, rows);

  if (errorcode)

    return errorcode | cbf_free_node (node);


    /* Success */

  handle->node = node;

  handle->row = 0;

  handle->search_row = 0;

  return 0;
}


  /* Add a row to the current category */

int cbf_new_row (cbf_handle handle)
{
  cbf_node *node, *columnnode;

  int errorcode [2];

  unsigned int rows, columns, column;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* How many rows and columns does this category have? */

  cbf_failnez (cbf_count_rows (handle, &rows))
  
  cbf_failnez (cbf_count_columns (handle, &columns))
  

    /* Add a row to each column */

  for (column = 0; column < columns; column++)
  {
    errorcode [0] = cbf_get_child (&columnnode, node, column);

    if (!errorcode [0])

      errorcode [0] = cbf_add_columnrow (columnnode, NULL);

    if (errorcode [0])
    {
        /* Set the columns back to the original number of rows */
        
      while (column)
      {
        column--;

        errorcode [1] = cbf_get_child (&columnnode, node, column);

        if (!errorcode [1])

          errorcode [1] |= cbf_set_children (columnnode, rows);

        errorcode [0] |= errorcode [1];
      }

      return errorcode [0];
    }
  }

  
    /* Success */

  handle->row = rows;

  handle->search_row = rows;

  return 0;
}


  /* Insert a row in the current category */

int cbf_insert_row (cbf_handle handle, const int rownumber)
{
  cbf_node *node, *columnnode;

  int errorcode [2];

  unsigned int rows, columns, column;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* How many rows and columns does this category have? */

  cbf_failnez (cbf_count_rows (handle, &rows))
  
  cbf_failnez (cbf_count_columns (handle, &columns))
  

    /* Insert a row into each column */

  for (column = 0; column < columns; column++)
  {
    errorcode [0] = cbf_get_child (&columnnode, node, column);

    if (!errorcode [0])

      errorcode [0] = cbf_insert_columnrow (columnnode, rownumber, NULL);

    if (errorcode [0])
    {
        /* Set the columns back to the original number of rows */
        
      while (column)
      {
        column--;

        errorcode [1] = cbf_get_child (&columnnode, node, column);

        if (!errorcode [1])

          errorcode [1] |= cbf_delete_columnrow (columnnode, rownumber);

        errorcode [0] |= errorcode [1];
      }

      return errorcode [0];
    }
  }

  
    /* Success */

  handle->row = rownumber;

  handle->search_row = rownumber;

  return 0;
}


  /* Delete a row from the current category */

int cbf_delete_row (cbf_handle handle, const int rownumber)
{
  cbf_node *node, *columnnode;

  int errorcode [2];

  unsigned int rows, columns, column;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* How many rows and columns does this category have? */

  cbf_failnez (cbf_count_rows (handle, &rows))
  
  cbf_failnez (cbf_count_columns (handle, &columns))
  

    /* Delete a row from each column */

  errorcode [0] = 0;
  
  for (column = 0; column < columns; column++)
  {
    errorcode [1] = cbf_get_child (&columnnode, node, column);

    if (!errorcode [1])

      errorcode [1] = cbf_delete_columnrow (columnnode, rownumber);

    errorcode [0] |= errorcode [1];
  }

  rows--;

  if (handle->row > rownumber)

    handle->row--;

  if (handle->search_row > rownumber)

    handle->search_row--;

  return errorcode [0];
}


  /* Change the name of the current data block */

int cbf_set_datablockname (cbf_handle handle, const char *datablockname)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Copy the name */

  if (datablockname)
  {
    datablockname = cbf_copy_string (NULL, datablockname, 0);

    if (!datablockname)

      return CBF_ALLOC;
  }


    /* Change the name */

  errorcode = cbf_name_node (node, datablockname);

  if (errorcode)
  {
    cbf_free_string (NULL, datablockname);

    return errorcode;
  }


    /* Success */

  handle->node = node;

  return 0;
}


  /* Delete all categories from all the data blocks */

int cbf_reset_datablocks (cbf_handle handle)
{
  cbf_node *node, *datablocknode;

  unsigned int datablocks, datablock;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  errorcode = cbf_find_parent (&datablocknode, handle->node, CBF_DATABLOCK);

  if (errorcode && errorcode != CBF_NOTFOUND)

    return errorcode;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))

  if (errorcode)
  
    handle->node = node;

  else

    handle->node = datablocknode;


    /* Delete all grandchildren */

  cbf_failnez (cbf_count_children (&datablocks, node))

  for (datablock = 0; datablock < datablocks; datablock++)
  {
    cbf_failnez (cbf_get_child (&node, handle->node, datablock))

    cbf_failnez (cbf_set_children (node, 0))
  }


    /* Success */

  return 0;
}


  /* Delete all categories from the current data block */

int cbf_reset_datablock (cbf_handle handle)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  handle->node = node;


    /* Delete the children */

  return cbf_set_children (node, 0);
}


  /* Delete all columns and rows from the current category */

int cbf_reset_category (cbf_handle handle)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))

  handle->node = node;


    /* Delete the children */

  return cbf_set_children (node, 0);
}


  /* Delete the current data block */

int cbf_remove_datablock (cbf_handle handle)
{
  cbf_node *node, *parent;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Find the root node */
    
  cbf_failnez (cbf_find_parent (&parent, node, CBF_ROOT))

  handle->node = parent;


    /* Delete the datablock */

  return cbf_free_node (node);
}


  /* Delete the current category */

int cbf_remove_category (cbf_handle handle)
{
  cbf_node *node, *parent;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Find the data block node */
    
  cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))

  handle->node = parent;


    /* Delete the column */

  return cbf_free_node (node);
}


  /* Delete the current column */

int cbf_remove_column (cbf_handle handle)
{
  cbf_node *node, *parent;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))


    /* Find the category node */
    
  cbf_failnez (cbf_find_parent (&parent, node, CBF_CATEGORY))

  handle->node = parent;


    /* Delete the column */

  return cbf_free_node (node);
}


  /* Delete the current row */

int cbf_remove_row (cbf_handle handle)
{
  if (!handle)

    return CBF_ARGUMENT;

  return cbf_delete_row (handle, handle->row);
}


  /* Make the first data block the current data block */

int cbf_rewind_datablock (cbf_handle handle)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Find the first child */

  cbf_failnez (cbf_get_child (&node, node, 0))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the first category in the current data block the current category */

int cbf_rewind_category (cbf_handle handle)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Find the first child */

  cbf_failnez (cbf_get_child (&node, node, 0))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the first column in the current category the current column */

int cbf_rewind_column (cbf_handle handle)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;

  
    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Find the first child */

  cbf_failnez (cbf_get_child (&node, node, 0))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the first row in the current category the current row */

int cbf_rewind_row (cbf_handle handle)
{
  if (!handle)

    return CBF_ARGUMENT;

  handle->row = 0;

  handle->search_row = 0;


    /* Success */

  return 0;
}


  /* Make the next data block the current data block */

int cbf_next_datablock (cbf_handle handle)
{
  cbf_node *parent, *node;

  unsigned int index;
  
  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_ROOT))


    /* Which child is this? */

  cbf_failnez (cbf_child_index (&index, node))
  

    /* Get the next data block */

  cbf_failnez (cbf_get_child (&node, parent, index + 1))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the next category in the current data block the current category */

int cbf_next_category (cbf_handle handle)
{
  cbf_node *parent, *node;

  unsigned int index;
  
  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))


    /* Which child is this? */

  cbf_failnez (cbf_child_index (&index, node))
  

    /* Get the next category */

  cbf_failnez (cbf_get_child (&node, parent, index + 1))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the next column in the current category the current column */

int cbf_next_column (cbf_handle handle)
{
  cbf_node *parent, *node;

  unsigned int index;
  
  if (!handle)

    return CBF_ARGUMENT;


    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_CATEGORY))


    /* Which child is this? */

  cbf_failnez (cbf_child_index (&index, node))
  

    /* Get the next column */

  cbf_failnez (cbf_get_child (&node, parent, index + 1))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the next row in the current category the current row */

int cbf_next_row (cbf_handle handle)
{
  cbf_node *node;

  unsigned int rows;
  
  if (!handle)

    return CBF_ARGUMENT;


    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))

  cbf_failnez (cbf_count_children (&rows, node))


    /* Is the row valid? */

  if (handle->row >= rows)

    return CBF_NOTFOUND;

  handle->row++;

  handle->search_row = handle->row;


    /* Success */

  return 0;
}


  /* Make the specified data block the current data block */

int cbf_select_datablock (cbf_handle handle, unsigned int datablock)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Select the data block */

  cbf_failnez (cbf_get_child (&node, node, datablock))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the specified category the current category */

int cbf_select_category (cbf_handle handle, unsigned int category)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Select the category */

  cbf_failnez (cbf_get_child (&node, node, category))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the specified column the current column */

int cbf_select_column (cbf_handle handle, unsigned int column)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Select the column */

  cbf_failnez (cbf_get_child (&node, node, column))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the specified row the current row */

int cbf_select_row (cbf_handle handle, unsigned int row)
{
  cbf_node *node;

  unsigned int rows;
  
  if (!handle)

    return CBF_ARGUMENT;


    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))

  cbf_failnez (cbf_count_children (&rows, node))


    /* Is the row valid? */

  if (row >= rows)

    return CBF_NOTFOUND;

  handle->row = row;

  handle->search_row = row;


    /* Success */

  return 0;
}


  /* Make the named data block the current data block */

int cbf_find_datablock (cbf_handle handle, const char *datablockname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Find the data block */

  cbf_failnez (cbf_find_child (&node, node, datablockname))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the named category in the current data block the current category */

int cbf_find_category (cbf_handle handle, const char *categoryname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Find the category */

  cbf_failnez (cbf_find_child (&node, node, categoryname))

  handle->node = node;

  handle->row = 0;

  handle->search_row = 0;


    /* Success */

  return 0;
}


  /* Make the named column in the current category the current column */

int cbf_find_column (cbf_handle handle, const char *columnname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Find the column */

  cbf_failnez (cbf_find_child (&node, node, columnname))

  handle->node = node;


    /* Success */

  return 0;
}


  /* Make the first row with matching value the current row */

int cbf_find_row (cbf_handle handle, const char *value)
{
  cbf_failnez (cbf_rewind_row (handle))

  return cbf_find_nextrow (handle, value);
}


  /* Make the next row with matching value the current row */

int cbf_find_nextrow (cbf_handle handle, const char *value)
{
  cbf_node *node;

  unsigned int row, rows;

  const char *text;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))


    /* Count the rows */

  cbf_failnez (cbf_count_children (&rows, node))

  for (row = handle->search_row; row < rows; row++)
  {
      /* Is the value ascii? */

    if (cbf_is_binary (node, row))

      continue;


      /* Get the value of the current row */

    cbf_failnez (cbf_get_columnrow (&text, node, row))


      /* Compare the values */

    if (text && value)
    {
      if (strcmp (text + 1, value))

        continue;
    }
    else

      if (text != value)

        continue;


      /* Found a match */

    handle->row = row;

    handle->search_row = row + 1;

    return 0;
  }


    /* Fail */

  return CBF_NOTFOUND;
}


  /* Count the data blocks */

int cbf_count_datablocks (cbf_handle handle, unsigned int *datablocks)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Count the data blocks */

  return cbf_count_children (datablocks, node);
}


  /* Count the categories in the current data block */

int cbf_count_categories (cbf_handle handle, unsigned int *categories)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Count the categories */

  return cbf_count_children (categories, node);
}


  /* Count the columns in the current category */

int cbf_count_columns (cbf_handle handle, unsigned int *columns)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Count the columns */

  return cbf_count_children (columns, node);
}


  /* Count the rows in the current category */

int cbf_count_rows (cbf_handle handle, unsigned int *rows)
{
  cbf_node *node, *parent;

  unsigned int columns, column, columnrows, categoryrows;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&parent, handle->node, CBF_CATEGORY))


    /* Count the columns */

  cbf_failnez (cbf_count_children (&columns, parent))


    /* Get the size of each column */

  categoryrows = 0;

  for (column = 0; column < columns; column++)
  {
      /* Get the column */

    cbf_failnez (cbf_get_child (&node, parent, column))


      /* Count the number of rows */

    cbf_failnez (cbf_count_children (&columnrows, node))


      /* Is it the same size as the other columns? */

    if (column == 0)

      categoryrows = columnrows;

    else

      if (categoryrows != columnrows)

        return CBF_FORMAT;
  }

  if (rows)

    *rows = categoryrows;


    /* Success */

  return 0;
}


  /* Get the name of the current data block */
  
int cbf_datablock_name (cbf_handle handle, const char **datablockname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Get the name */

  return cbf_get_name (datablockname, node);
}


  /* Get the name of the current category */
  
int cbf_category_name (cbf_handle handle, const char **categoryname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Get the name */

  return cbf_get_name (categoryname, node);
}


  /* Get the name of the current column */
  
int cbf_column_name (cbf_handle handle, const char **columnname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))


    /* Get the name */

  return cbf_get_name (columnname, node);
}


  /* Get the number of the current row */
  
int cbf_row_number (cbf_handle handle, unsigned int *row)
{
  if (!handle)

    return CBF_ARGUMENT;

  if (row)

    *row = handle->row;


    /* Success */

  return 0;
}


  /* Get the ascii value of the current (row, column) entry */
  
int cbf_get_value (cbf_handle handle, const char **value)
{
  const char *text;

  
    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

    
    /* Is the value binary? */

  if (cbf_is_binary (handle->node, handle->row))

    return CBF_BINARY;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, handle->node, handle->row))

  if (value)
  
    if (text)

      *value = text + 1;
      
    else
    
      *value = NULL;


    /* Success */

  return 0;
}


  /* Set the ascii value of the current (row, column) entry */
  
int cbf_set_value (cbf_handle handle, const char *value)
{
  int errorcode;

  
    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;


    /* Copy the string */

  if (value)
  {
    value = cbf_copy_string (NULL, value, '\200');

    if (!value)

      return CBF_ALLOC;
  }

    
    /* Set the new value */

  errorcode = cbf_set_columnrow (handle->node, handle->row, value, 1);

  if (errorcode)
  {
    cbf_free_string (NULL, value);

    return errorcode;
  }


    /* Success */

  return 0;
}


  /* Get the (int) numeric value of the current (row, column) entry */
  
int cbf_get_integervalue (cbf_handle handle, int *number)
{
  const char *value;


    /* Get the value */

  cbf_failnez (cbf_get_value (handle, &value))


    /* Convert it into an integer */

  if (!value)

    return CBF_NOTFOUND;

  if (number)
  
    *number = atoi (value);


    /* Success */

  return 0;
}


  /* Get the (double) numeric value of the current (row, column) entry */
  
int cbf_get_doublevalue (cbf_handle handle, double *number)
{
  const char *value;


    /* Get the value */

  cbf_failnez (cbf_get_value (handle, &value))


    /* Convert it into a double */

  if (!value)

    return CBF_NOTFOUND;

  if (number)
  
    *number = atof (value);


    /* Success */

  return 0;
}


  /* Set the ascii value of the current (row, column) entry from an int */
  
int cbf_set_integervalue (cbf_handle handle, int number)
{
  char value [64];


    /* Write the value */

  sprintf (value, "%d", number);


    /* Save it */

  return cbf_set_value (handle, value);
}


  /* Set the ascii value of the current (row, column) entry from a double */
  
int cbf_set_doublevalue (cbf_handle handle, const char *format, double number)
{
  char value [64];


    /* Write the value */

  sprintf (value, format, number);


    /* Save it */

  return cbf_set_value (handle, value);
}


  /* Get the parameters of the current (row, column) array entry */
  
int cbf_get_integerarrayparameters (cbf_handle    handle, 
                                    unsigned int *compression,
                                    int          *id, 
                                    size_t       *elsize, 
                                    int          *elsigned, 
                                    int          *elunsigned, 
                                    size_t       *nelem, 
                                    int          *minelem, 
                                    int          *maxelem)
{
    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

    
    /* Is the value binary? */

  if (!cbf_is_binary (handle->node, handle->row))

    return CBF_ASCII;


    /* Get the parameters */

  return cbf_binary_parameters (handle->node, handle->row, 
                                compression, id, NULL, elsize,
                                elsigned, elunsigned, nelem,
                                minelem, maxelem);
}


  /* Get the value of the current (row, column) array entry */
  
int cbf_get_integerarray (cbf_handle  handle,
                          int        *id,
                          void       *value, 
                          size_t      elsize, 
                          int         elsign,
                          size_t      nelem, 
                          size_t     *nelem_read)
{
  if (!handle)

    return CBF_ARGUMENT;

  return cbf_get_binary (handle->node, handle->row, id,
                         value, elsize, elsign, nelem, nelem_read);
}


  /* Set the value of the current (row, column) array entry */
  
int cbf_set_integerarray (cbf_handle    handle,
                          unsigned int  compression, 
                          int           id, 
                          void         *value, 
                          size_t        elsize,
                          int           elsign, 
                          size_t        nelem)
{
  if (!handle)

    return CBF_ARGUMENT;

  return cbf_set_binary (handle->node, handle->row,
                         compression, id, value, elsize, elsign, nelem);
}


  /* Issue a warning message */

void cbf_warning (const char *message)

{
  fprintf (stderr, " CBFlib: warning -- %s\n", message);
}


  /* Issue an error message */

void cbf_error (const char *message)
{
  fprintf (stderr, " CBFlib: error -- %s\n", message);
}


#ifdef __cplusplus

}

#endif

