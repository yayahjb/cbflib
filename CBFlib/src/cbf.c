/**********************************************************************
 * cbf -- cbflib API functions                                        *
 *                                                                    *
 * Version 0.7.5 15 April 2006                                        *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006 Herbert J. Bernstein                            *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 **********************************************************************/

/*************************** GPL NOTICES ******************************
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * (the License, or (at your option) any later version.               *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           *
 * 02111-1307  USA                                                    *
 *                                                                    *
 **********************************************************************/

/************************* LGPL NOTICES *******************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term ‘this software’, as used in these Notices, refers to      *
 * those portions of the software package CBFlib that were created by *
 * employees of the Stanford Linear Accelerator Center, Stanford      *
 * University.                                                        *
 *                                                                    *
 * Stanford disclaimer of copyright                                   *
 * --------------------------------                                   *
 * Stanford University, owner of the copyright, hereby disclaims its  *
 * copyright and all other rights in this software.  Hence, anyone    *
 * may freely use it for any purpose without restriction.             *
 *                                                                    *
 * Acknowledgement of sponsorship                                     *
 * ------------------------------                                     *
 * This software was produced by the Stanford Linear Accelerator      *
 * Center, Stanford University, under Contract DE-AC03-76SFO0515 with *
 * the Department of Energy.                                          *
 *                                                                    *
 * Government disclaimer of liability                                 *
 * ----------------------------------                                 *
 * Neither the United States nor the United States Department of      *
 * Energy, nor any of their employees, makes any warranty, express or *
 * implied, or assumes any legal liability or responsibility for the  *
 * accuracy, completeness, or usefulness of any data, apparatus,      *
 * product, or process disclosed, or represents that its use would    *
 * not infringe privately owned rights.                               *
 *                                                                    *
 * Stanford disclaimer of liability                                   *
 * --------------------------------                                   *
 * Stanford University makes no representations or warranties,        *
 * express or implied, nor assumes any liability for the use of this  *
 * software.                                                          *
 *                                                                    *
 * Maintenance of notices                                             *
 * ----------------------                                             *
 * In the interest of clarity regarding the origin and status of this *
 * software, this and all the preceding Stanford University notices   *
 * are to remain affixed to any copy or derivative of this software   *
 * made or distributed by the recipient and are to be affixed to any  *
 * copy of software made or distributed by the recipient that         *
 * contains a copy or derivative of this software.                    *
 *                                                                    *
 * Based on SLAC Software Notices, Set 4                              *
 * OTT.002a, 2004 FEB 03                                              *
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
 *                                                                    *
 *                           The IUCr Policy                          *
 *      for the Protection and the Promotion of the STAR File and     *
 *     CIF Standards for Exchanging and Archiving Electronic Data     *
 *                                                                    *
 * Overview                                                           *
 *                                                                    *
 * The Crystallographic Information File (CIF)[1] is a standard for   *
 * information interchange promulgated by the International Union of  *
 * Crystallography (IUCr). CIF (Hall, Allen & Brown, 1991) is the     *
 * recommended method for submitting publications to Acta             *
 * Crystallographica Section C and reports of crystal structure       *
 * determinations to other sections of Acta Crystallographica         *
 * and many other journals. The syntax of a CIF is a subset of the    *
 * more general STAR File[2] format. The CIF and STAR File approaches *
 * are used increasingly in the structural sciences for data exchange *
 * and archiving, and are having a significant influence on these     *
 * activities in other fields.                                        *
 *                                                                    *
 * Statement of intent                                                *
 *                                                                    *
 * The IUCr's interest in the STAR File is as a general data          *
 * interchange standard for science, and its interest in the CIF,     *
 * a conformant derivative of the STAR File, is as a concise data     *
 * exchange and archival standard for crystallography and structural  *
 * science.                                                           *
 *                                                                    *
 * Protection of the standards                                        *
 *                                                                    *
 * To protect the STAR File and the CIF as standards for              *
 * interchanging and archiving electronic data, the IUCr, on behalf   *
 * of the scientific community,                                       *
 *                                                                    *
 * * holds the copyrights on the standards themselves,                *
 *                                                                    *
 * * owns the associated trademarks and service marks, and            *
 *                                                                    *
 * * holds a patent on the STAR File.                                 *
 *                                                                    *
 * These intellectual property rights relate solely to the            *
 * interchange formats, not to the data contained therein, nor to     *
 * the software used in the generation, access or manipulation of     *
 * the data.                                                          *
 *                                                                    *
 * Promotion of the standards                                         *
 *                                                                    *
 * The sole requirement that the IUCr, in its protective role,        *
 * imposes on software purporting to process STAR File or CIF data    *
 * is that the following conditions be met prior to sale or           *
 * distribution.                                                      *
 *                                                                    *
 * * Software claiming to read files written to either the STAR       *
 * File or the CIF standard must be able to extract the pertinent     *
 * data from a file conformant to the STAR File syntax, or the CIF    *
 * syntax, respectively.                                              *
 *                                                                    *
 * * Software claiming to write files in either the STAR File, or     *
 * the CIF, standard must produce files that are conformant to the    *
 * STAR File syntax, or the CIF syntax, respectively.                 *
 *                                                                    *
 * * Software claiming to read definitions from a specific data       *
 * dictionary approved by the IUCr must be able to extract any        *
 * pertinent definition which is conformant to the dictionary         *
 * definition language (DDL)[3] associated with that dictionary.      *
 *                                                                    *
 * The IUCr, through its Committee on CIF Standards, will assist      *
 * any developer to verify that software meets these conformance      *
 * conditions.                                                        *
 *                                                                    *
 * Glossary of terms                                                  *
 *                                                                    *
 * [1] CIF:  is a data file conformant to the file syntax defined     *
 * at http://www.iucr.org/iucr-top/cif/spec/index.html                *
 *                                                                    *
 * [2] STAR File:  is a data file conformant to the file syntax       *
 * defined at http://www.iucr.org/iucr-top/cif/spec/star/index.html   *
 *                                                                    *
 * [3] DDL:  is a language used in a data dictionary to define data   *
 * items in terms of "attributes". Dictionaries currently approved    *
 * by the IUCr, and the DDL versions used to construct these          *
 * dictionaries, are listed at                                        *
 * http://www.iucr.org/iucr-top/cif/spec/ddl/index.html               *
 *                                                                    *
 * Last modified: 30 September 2000                                   *
 *                                                                    *
 * IUCr Policy Copyright (C) 2000 International Union of              *
 * Crystallography                                                    *
 **********************************************************************/

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_binary.h"
#include "cbf_write.h"
#include "cbf_string.h"

#include <stdlib.h>
#include <string.h>

int cbf_parse (void *context);


  /* Create a handle */

int cbf_make_handle (cbf_handle *handle)
{
  int errorcode;

  cbf_failnez (cbf_alloc ((void **) handle, NULL,
               sizeof (cbf_handle_struct), 1))

  errorcode = cbf_make_node (&(*handle)->node, CBF_ROOT, NULL, NULL);

  if (errorcode)

    return errorcode | cbf_free ((void **) handle, NULL);

  (*handle)->row = 0;

  (*handle)->search_row = 0;

  (*handle)->refcount = 1;

  (*handle)->dictionary = NULL;

  return 0;
}


  /* Free a handle */

int cbf_free_handle (cbf_handle handle)
{
  int errorcode;

  errorcode = 0;

  if (handle && (--(handle->refcount) <= 0) )
  {
    if (handle->dictionary) {

      errorcode |=
        cbf_free_handle ((cbf_handle) handle->dictionary);

    }
    errorcode |= cbf_free_node (handle->node);

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

  errorcode = cbf_write_node (handle, node, file, isbuffer);


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


  /* Add a save frame */

int cbf_new_saveframe (cbf_handle handle, const char *saveframename)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Copy the name */

  if (saveframename)
  {
    saveframename = cbf_copy_string (NULL, saveframename, 0);

    if (!saveframename)

      return CBF_ALLOC;
  }


    /* Add a save frame */

  errorcode = cbf_make_child (&node, node, CBF_SAVEFRAME, saveframename);

  if (errorcode)
  {
    cbf_free_string (NULL, saveframename);

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


  /* Add a save frame, allowing for duplicates */

int cbf_force_new_saveframe (cbf_handle handle, const char *saveframename)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the DATABLOCK */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Copy the name */

  if (saveframename)
  {
    saveframename = cbf_copy_string (NULL, saveframename, 0);

    if (!saveframename)

      return CBF_ALLOC;
  }


    /* Add a save frame */

  errorcode = cbf_make_new_child (&node, node, CBF_SAVEFRAME, saveframename);

  if (errorcode)
  {
    cbf_free_string (NULL, saveframename);

    return errorcode;
  }


    /* Success */

  handle->node = node;

  return 0;
}


  /* Add a category to the current data block or save frame*/

int cbf_new_category (cbf_handle handle, const char *categoryname)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the save frame or data block node */

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }

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

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }

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

  /* Change the name of the current save frame */

int cbf_set_saveframename (cbf_handle handle, const char *saveframename)
{
  cbf_node *node;

  int errorcode;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the save frame node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME))


    /* Copy the name */

  if (saveframename)
  {
    saveframename = cbf_copy_string (NULL, saveframename, 0);

    if (!saveframename)

      return CBF_ALLOC;
  }


    /* Change the name */

  errorcode = cbf_name_node (node, saveframename);

  if (errorcode)
  {
    cbf_free_string (NULL, saveframename);

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


  /* Delete all categories from the current save frame */

int cbf_reset_saveframe (cbf_handle handle)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME))

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


  /* Delete the current save frame  */

int cbf_remove_saveframe (cbf_handle handle)
{
  cbf_node *node, *parent;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the save frame node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME))


    /* Find the data block */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))

  handle->node = parent;


    /* Delete the save frame */

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


    /* Find the save frame or data block node */

  if (cbf_find_parent (&parent, node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))

  }

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


  /* Make the first save frame in the current data block the current saveframe */

int cbf_rewind_saveframe (cbf_handle handle)
{
  cbf_node *node;

  cbf_node *child_node;

  int i;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the save frame or data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))



    /* Find the first child that is a save frame*/

  for (i = 0; i < node->children; i++) {

    cbf_failnez (cbf_get_child (&child_node, node, i))

    if (child_node && child_node->type == CBF_SAVEFRAME) {

      handle->node = child_node;

      /* Success */

      return 0;

    }
  }

  return CBF_NOTFOUND;

}


  /* Make the first category in the current data block the current category */

int cbf_rewind_category (cbf_handle handle)
{
  cbf_node *node;

  cbf_node *child_node;

  int i;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the save frame or data block node */

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }


    /* Find the first child that is a category*/

  for (i = 0; i < node->children; i++) {

    cbf_failnez (cbf_get_child (&child_node, node, i))

    if (child_node && child_node->type == CBF_CATEGORY) {

      handle->node = child_node;

      /* Success */

      return 0;

    }
  }

  return CBF_NOTFOUND;

}


  /* Make the first save frame or category in the current data block the current save frame or category */

int cbf_rewind_blockitem (cbf_handle handle, CBF_NODETYPE * type)
{
  cbf_node *node;

  cbf_node *child_node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Find the first child */

  cbf_failnez (cbf_get_child (&child_node, node, 0))

  handle->node = child_node;

  *type = child_node->type;

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


  /* Make the next category in the current save frame or data block the current category */

int cbf_next_category (cbf_handle handle)
{
  cbf_node *parent, *node;

  int i;

  unsigned int index;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_CATEGORY))


    /* Find the save frame or data block node */

  if (cbf_find_parent (&parent, node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))

  }


    /* Which child is this? */

  cbf_failnez (cbf_child_index (&index, node))


    /* Get the next category */

  for (i = index+1; i<parent->children; i++) {

    cbf_failnez (cbf_get_child (&node, parent, i))

    if (node->type == CBF_CATEGORY) {

      handle->node = node;

      /* Success */

      return 0;
    }

  }

  return CBF_NOTFOUND;
}


  /* Make the next save frame in the current data block the current save frame */

int cbf_next_saveframe (cbf_handle handle)
{
  cbf_node *parent, *node;

  int i;

  unsigned int index;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME))


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))


    /* Which child is this? */

  cbf_failnez (cbf_child_index (&index, node))


    /* Get the next save frame */

  for (i = index+1; i<parent->children; i++) {

    cbf_failnez (cbf_get_child (&node, parent, i))

    if (node->type == CBF_SAVEFRAME) {

      handle->node = node;

      /* Success */

      return 0;
    }

  }

  return CBF_NOTFOUND;
}


  /* Make the next save frame or category the current data block or category */

int cbf_next_blockitem (cbf_handle handle, CBF_NODETYPE * type)
{
  cbf_node *parent, *node;

  unsigned int index;

  if (!handle)

    return CBF_ARGUMENT;

    /* Discover if we are in a save frame or just in a data block */

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    /* There is no save frame look for a category */

    cbf_failnez(cbf_find_parent (&node, handle->node, CBF_CATEGORY))

  }

     /* Find the root node */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_DATABLOCK))

    /* Which child is this? */

  cbf_failnez (cbf_child_index (&index, node))


    /* Get the next data block */

  cbf_failnez (cbf_get_child (&node, parent, index + 1))

  handle->node = node;

  *type = handle->node->type;

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


  /* Make the specified save frame the current save frame */

int cbf_select_saveframe (cbf_handle handle, unsigned int saveframe)
{
  cbf_node *node;

  cbf_node *child_node;

  unsigned int isf, jsf;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Select the save frame */

  isf = 0;

  jsf = 0;

  while (jsf < saveframe+1 && isf < node->children) {

    cbf_failnez (cbf_get_child (&child_node, node, isf++))

    if (child_node->type == CBF_SAVEFRAME) jsf++;

  }

  if (jsf == saveframe+1) {

    handle->node = child_node;

    /* Success */

    return 0;

  }

  return CBF_NOTFOUND;
}


  /* Make the specified category the current category */

int cbf_select_category (cbf_handle handle, unsigned int category)
{
  cbf_node *node;

  cbf_node *child_node;

  unsigned int icat, jcat;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }


    /* Select the category */

  icat = 0;

  jcat = 0;

  while (jcat < category+1 && icat < node->children) {

    cbf_failnez (cbf_get_child (&child_node, node, icat++))

    if (child_node->type == CBF_CATEGORY) jcat++;

  }

  if (jcat == category+1) {

    handle->node = child_node;

    /* Success */

    return 0;

  }

  return CBF_NOTFOUND;
}


  /* Make the specified category or save frame the current block item */

int cbf_select_blockitem (cbf_handle handle, unsigned int item, CBF_NODETYPE * type)
{
  cbf_node *node;

  cbf_node *child_node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Select the item */

  cbf_failnez (cbf_get_child (&child_node, node, item))

  handle->node = child_node;

  *type = child_node->type;

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


  /* Make the named save frame in the current data block the current save frame */

int cbf_find_saveframe (cbf_handle handle, const char *saveframename)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))



    /* Find the save frame */

  cbf_failnez (cbf_find_typed_child (&node, node, saveframename, CBF_SAVEFRAME))

  handle->node = node;

  handle->row = 0;

  handle->search_row = 0;


    /* Success */

  return 0;
}



  /* Make the named category in the current save frame or data block the current category */

int cbf_find_category (cbf_handle handle, const char *categoryname)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }


    /* Find the category */

  cbf_failnez (cbf_find_typed_child (&node, node, categoryname, CBF_CATEGORY))

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



  /* Make the first row with matching value the current row
     creating it if necessary */

int cbf_require_row (cbf_handle handle, const char *value)
{
  if (cbf_rewind_row (handle)) {

    cbf_failnez(cbf_new_row (handle))
    
    return cbf_set_value (handle, value);
  	
  }

  return cbf_require_nextrow (handle, value);
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

  return CBF_NOTFOUND;

}

  /* Make the next row with matching value the current row,
     creating the row if necessary */

int cbf_require_nextrow (cbf_handle handle, const char *value)
{

  if (cbf_find_nextrow(handle, value)) {
  
    cbf_failnez( cbf_new_row(handle))
    
    return cbf_set_value(handle, value);
  	
  }
  
  return 0;

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


  /* Count the save frames in the current data block */

int cbf_count_saveframes (cbf_handle handle, unsigned int *saveframes)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Count the save frames */

  return cbf_count_typed_children (saveframes, node, CBF_SAVEFRAME);
}

  /* Count the categories in the current save frame or data block */

int cbf_count_categories (cbf_handle handle, unsigned int *categories)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME)) {

    cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }


    /* Count the categories */

  return cbf_count_typed_children (categories, node, CBF_CATEGORY);
}

  /* Count the items in the current data block */

int cbf_count_blockitems (cbf_handle handle, unsigned int *blockitems)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))


    /* Count the categories */

  return cbf_count_children (blockitems, node);
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


  /* Get the name of the current save frame */

int cbf_saveframe_name (cbf_handle handle, const char **saveframename)
{
  cbf_node *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the data block node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME))


    /* Get the name */

  return cbf_get_name (saveframename, node);
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

  if (value) {

    if (text) {

      *value = text + 1;

    } else {

      *value = NULL;
    }

  }


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

  /* Get the ascii value of the current (row, column) entry,
     setting it to a default value if necessary */

int cbf_require_value (cbf_handle handle, const char **value, 
                                          const char *defaultvalue)
{
  if (cbf_get_value (handle, value)) 
  {
  	 cbf_failnez (cbf_set_value(handle, defaultvalue))
  	 
  	 return (cbf_get_value(handle, value));
  }
  
  return 0;
}


  /* Set the ascii type value of the current (row, column) entry */

int cbf_set_typeofvalue (cbf_handle handle, const char *typeofvalue)
{
  char *text;

    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

    /* Is the value binary? */

  if (cbf_is_binary (handle->node, handle->row))

    return CBF_BINARY;

    /* Get the value */

  cbf_failnez (cbf_get_columnrow ((const char **)(&text), handle->node, handle->row))

  cbf_failnez (cbf_set_value_type(text, typeofvalue))

    /* Success */

  return 0;
}

  /* Get the ascii type of value of the current (row, column) entry */

int cbf_get_typeofvalue (cbf_handle handle, const char **typeofvalue)
{
  const char *text;

    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

    /* Is the value binary? */

  if (cbf_is_binary (handle->node, handle->row)) {

    *typeofvalue = "bnry";

    return 0;

  }
    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, handle->node, handle->row))

  if (typeofvalue) {

    if (text) {

      cbf_failnez (cbf_get_value_type(text, typeofvalue))

    } else {

      *typeofvalue = NULL;

    }

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

  int lopos, hipos;


    /* Write the value */

  sprintf (value, format, number);

    /* strip the leading and trailing blanks */

  for (lopos=0; (value[lopos]==' '||value[lopos]=='\t'); lopos++);

  for (hipos=strlen(value+lopos); hipos>0&&(value[lopos+hipos-1]==' '||value[lopos+hipos-1]=='\t'); hipos--);

  *(value+lopos+hipos) = '\0';


    /* Save it */

  return cbf_set_value (handle, value+lopos);
}

  /* Get the integer value of the current (row, column) entry,
     setting it to a default value if necessary */

int cbf_require_integervalue (cbf_handle handle, int *number, 
                                          int defaultvalue)
{
  if (cbf_get_integervalue (handle, number)) 
  {
  	 cbf_failnez (cbf_set_integervalue(handle, defaultvalue))
  	 
  	 return (cbf_get_integervalue(handle, number));
  }
  
  return 0;
}

  /* Get the integer value of the current (row, column) entry,
     setting it to a default value if necessary */

int cbf_require_doublevalue (cbf_handle handle, double *number, 
                                          double defaultvalue)
{
  if (cbf_get_doublevalue (handle, number)) 
  {
  	 cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", defaultvalue))
  	 
  	 return (cbf_get_doublevalue(handle, number));
  }
  
  return 0;
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
  int realarray;

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
                                minelem, maxelem, &realarray);
}


  /* Get the parameters of the current (row, column) array entry */

int cbf_get_realarrayparameters (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    size_t       *nelem)
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
                                NULL, NULL, nelem,
                                NULL, NULL, NULL);
}



  /* Get the integer value of the current (row, column) array entry */

int cbf_get_integerarray (cbf_handle  handle,
                          int        *id,
                          void       *value,
                          size_t      elsize,
                          int         elsign,
                          size_t      nelem,
                          size_t     *nelem_read)
{

  int realarray;

  if (!handle)

    return CBF_ARGUMENT;

  return cbf_get_binary (handle->node, handle->row, id,
                         value, elsize, elsign, nelem, nelem_read, &realarray);
}


  /* Get the real value of the current (row, column) array entry */

int cbf_get_realarray (cbf_handle  handle,
                          int        *id,
                          void       *value,
                          size_t      elsize,
                          size_t      nelem,
                          size_t     *nelem_read)
{
  int realarray;

  if (!handle)

    return CBF_ARGUMENT;

  return cbf_get_binary (handle->node, handle->row, id,
                         value, elsize, 1, nelem, nelem_read, &realarray);
}


  /* Set the integer value of the current (row, column) array entry */

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
                         compression, id, value, elsize, elsign, nelem, 0);
}


  /* Set the real value of the current (row, column) array entry */

int cbf_set_realarray (cbf_handle    handle,
                          unsigned int  compression,
                          int           id,
                          void         *value,
                          size_t        elsize,
                          size_t        nelem)
{
  if (!handle)

    return CBF_ARGUMENT;

  return cbf_set_binary (handle->node, handle->row,
                         compression, id, value, elsize, 1, nelem, 1);
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

  /* Find a datablock, creating it if necessary */

int cbf_require_datablock (cbf_handle  handle,
                             const char *datablockname)
{
  if (cbf_find_datablock(handle, datablockname)) {

    cbf_failnez(cbf_new_datablock(handle, datablockname))

  }
  return 0;
}


  /* Find a category, creating it if necessary */

int cbf_require_category (cbf_handle  handle,
                             const char *categoryname)
{
  if (cbf_find_category(handle, categoryname)) {

    cbf_failnez(cbf_new_category(handle, categoryname))

  }
  return 0;
}

  /* Find a column, creating it if necessary */

int cbf_require_column (cbf_handle  handle,
                             const char *columnname)
{
  if (cbf_find_column(handle, columnname)) {

    cbf_failnez(cbf_new_column(handle, columnname))

  }
  return 0;
}


  /* Find a column value, return a default if necessary */

int cbf_require_column_value (cbf_handle  handle,
                             const char *columnname,
                             const char **value,
                             const char *defaultvalue)
{
  if (!cbf_require_column(handle, columnname) &&
      !cbf_get_value(handle, value)) {

    return 0;

  } else {

    cbf_failnez (cbf_set_value(handle, defaultvalue))

    return cbf_get_value(handle, value);

  }
}

  /* Find a column integer value, return a default if necessary */

int cbf_require_column_integervalue (cbf_handle  handle,
                             const char *columnname,
                             int *number,
                             const int defaultvalue)
{
    if (!cbf_require_column(handle, columnname) &&
      !cbf_get_integervalue(handle, number)) {

      return 0;

    } else {
    
      cbf_failnez (cbf_set_integervalue(handle, defaultvalue))

      return cbf_get_integervalue(handle, number);

    }
}

  /* Find a column double value, return a default if necessary */

int cbf_require_column_doublevalue (cbf_handle  handle,
                             const char *columnname,
                             double *number,
                             const double defaultvalue)
{
    if (!cbf_require_column(handle, columnname) &&
      !cbf_get_doublevalue(handle, number)) {

      return 0;

    } else {

      cbf_failnez (cbf_set_doublevalue(handle, "%.15g", defaultvalue))

      return cbf_get_doublevalue(handle, number);

    }
}


  /* Get the local byte order of the default integer type */

int cbf_get_local_integer_byte_order (char ** byte_order)
{
   static char le[14] = "little_endian";

   static char be[11] = "big_endian";

   int *test;

   int probe = 1;

   test = (int *)&probe;

   if (*(char*)test) *byte_order = le;

   else *byte_order = be;

   return 0;
}

  /* Get the local byte order of the default real type */

int cbf_get_local_real_byte_order (char ** byte_order)
{
   static char le[14] = "little_endian";

   static char be[11] = "big_endian";

   double *test;

   double probe = 1.;

   test = (double *)&probe;

   if (*(char*)test) *byte_order = be;

   else *byte_order = le;

   return 0;
}

  /* Get the local real format */

int cbf_get_local_real_format (char ** real_format )
{
   static char ieee[14] = "ieee 754-1985";

   static char other[6] = "other";

   float *test;

   float value=1.;

   *real_format = other;

   test = &value;

   if (sizeof (float) == sizeof (long) ) {

     if (*(long *)test == 0774000000L ) *real_format = ieee;

   } else {
     if (sizeof (float) == sizeof (int) ) {

       if (*(int *)test == 0774000000 ) *real_format = ieee;

     }
   }

   return 0;
}

  /* Get the dictionary for a cbf */

int cbf_get_dictionary (cbf_handle handle, cbf_handle * dictionary)
{
  if (handle &&
    (*dictionary = (cbf_handle)(handle->dictionary)) ) return 0;

  return CBF_NOTFOUND;

}

  /* Set the dictionary for a cbf */

int cbf_set_dictionary (cbf_handle handle, cbf_handle dictionary)
{
  if (!handle) return CBF_ARGUMENT;

  if (handle->dictionary) {

    cbf_failnez(cbf_free_handle((cbf_handle)(handle->dictionary)))

  }

  * ((cbf_handle *)(&handle->dictionary)) = dictionary;

  (dictionary->refcount)++;

  return 0;

}

  /* Get the dictionary for a cbf, or create one */

int cbf_require_dictionary (cbf_handle handle, cbf_handle * dictionary)
{
  if (!handle) return CBF_ARGUMENT;

  if ( cbf_get_dictionary(handle, dictionary)) return 0;

  cbf_failnez (cbf_make_handle((cbf_handle *)&(handle->dictionary)))

  *dictionary = (cbf_handle)(handle->dictionary);

  return 0;

}

  /* Put the value into the named column, updating the hash table links */

int cbf_set_hashedvalue(cbf_handle handle, const char * value, const char * columnname) {

  char colhash[91];

  char colhashnext[91];

  unsigned int hashcode;

  unsigned int orownum, rownum, nrownum;

  const char * hashcodestring;

  int colnamelen;

  if ( !columnname ) return CBF_ARGUMENT;

  if ( (colnamelen = strlen(columnname)) > 80 ) return CBF_ARGUMENT;

  strcpy (colhash, columnname);

  strcpy (colhashnext, columnname);

  strcpy (colhash+colnamelen, "_hash");

  strcpy (colhashnext+colnamelen, "_hash_next");


  cbf_failnez( cbf_compute_hashcode(value, &hashcode))

  cbf_failnez( cbf_require_column(handle, columnname))

  cbf_failnez( cbf_new_row          (handle))

  cbf_failnez( cbf_set_value        (handle,    value))

  cbf_failnez( cbf_row_number       (handle,   &rownum))

  cbf_failnez( cbf_require_column   (handle,   (const char *) colhashnext))

  cbf_failnez( cbf_set_integervalue (handle,   -1))

  cbf_failnez( cbf_require_column   (handle,   colhash))

  cbf_failnez( cbf_set_integervalue (handle,   hashcode))

  cbf_failnez( cbf_get_value        (handle,   &hashcodestring))

  cbf_failnez( cbf_rewind_row       (handle))

  cbf_failnez( cbf_find_row         (handle,   hashcodestring))

  cbf_failnez( cbf_row_number       (handle,   &orownum))

  cbf_failnez( cbf_find_column      (handle,   colhashnext))

  while (orownum < rownum) {
    cbf_failnez( cbf_get_integervalue   (handle, (int *)(int *)&rownum))

    if (nrownum == -1) {

       cbf_failnez( cbf_set_integervalue(handle,   rownum))

       break;

    }

    orownum = nrownum;

  }

  return 0;


}


  /* Find value in the named column, using the hash table links */

int cbf_find_hashedvalue(cbf_handle handle, const char * value, const char * columnname) {

  char colhash[91];

  char colhashnext[91];

  unsigned int hashcode;

  unsigned int orownum, rownum, nrownum;

  const char * hashcodestring;

  const char * rowvalue;

  int colnamelen;

  if (!columnname) return CBF_ARGUMENT;

  if ( (colnamelen = strlen(columnname)) > 80 ) return CBF_ARGUMENT;

  strcpy (colhash, columnname);

  strcpy (colhashnext, columnname);

  strcpy (colhash+colnamelen, "_hash");

  strcpy (colhashnext+colnamelen, "_hash_next");


  cbf_failnez (cbf_compute_hashcode(value, &hashcode))

  sprintf ((char *)hashcodestring,"%d",hashcode);

  cbf_failnez( cbf_find_column      (handle,   colhash))

  cbf_failnez( cbf_count_rows       (handle,   &rownum))

  cbf_failnez( cbf_rewind_row       (handle))

  cbf_failnez( cbf_find_row         (handle,   hashcodestring))

  cbf_failnez( cbf_row_number       (handle,   &orownum))

  cbf_failnez( cbf_find_column      (handle,   colhashnext))

  do {

    cbf_failnez (cbf_find_column    (handle, columnname))

    if (cbf_get_value      (handle, &rowvalue))
    {

      if( !cbf_cistrcmp(value,rowvalue)) return 0;

    }

    cbf_failnez( cbf_find_column      (handle,   colhashnext))

    cbf_failnez( cbf_get_integervalue (handle, (int *)(int *)&rownum))

    if (nrownum == -1)  break;

    orownum = nrownum;

  }  while (orownum < rownum);

  return CBF_NOTFOUND;


}


int cbf_convert_dictionary_definition(cbf_handle cbfdictionary, cbf_handle dictionary,const char * name)
{
    const char *category_id;

    const char *sub_category_id;

    const char *mandatory_code;

    const char *itemname;

    const char *categoryname;

    const char *type_code;

    const char *units_code;

    const char *default_value;

    const char *parent_name;

    const char *child_name;

    const char *alias_name;

    const char *group;

    const char *key;

    const char *oldkey;

    unsigned int hashcode, rowlink;

    const char *hashcodestring;

    unsigned int rownum, numrows, numrow, orownum, nrownum;

    int haveitemname;

    int haveitemcategory;

    haveitemname = haveitemcategory = 0;

    if (!cbf_find_local_tag(dictionary,"_name") ||
      cbf_find_local_tag(dictionary,"_item.name") )  haveitemname = 1;

    if (!cbf_find_category(dictionary,"item")) haveitemcategory = 1;

    if (haveitemname || haveitemcategory) {

      cbf_failnez( cbf_count_rows (dictionary,&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      for (numrow=0; numrow < numrows; numrow++) {

        cbf_failnez( cbf_require_category  (cbfdictionary, "items"))

        cbf_failnez( cbf_select_row(dictionary, numrow) )

        if (haveitemname) {

          cbf_failnez( cbf_get_value         (dictionary, &itemname))

        } else {

          itemname = name;

        }

        if (cbf_find_hashedvalue(cbfdictionary, itemname, "name")) {

          cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, itemname, "name"))

        }

        if (!cbf_find_column(dictionary,"category_id") || !cbf_find_column(dictionary,"_category")
          || ((numrows==1) && !cbf_find_local_tag(dictionary,"_category")) ){

          if (!cbf_get_value(dictionary, &category_id)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "category_id"));

            cbf_failnez( cbf_set_value(cbfdictionary, category_id))

          }

        }

        if (!cbf_find_column(dictionary,"mandatory_code") ) {

          if (!cbf_get_value(dictionary, &mandatory_code)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "mandatory_code"));

            cbf_failnez( cbf_set_value(cbfdictionary, mandatory_code))

          }

        }

        if (!cbf_find_column(dictionary,"sub_category_id")) {

          if (!cbf_get_value(dictionary, &sub_category_id)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "sub_category_id"));

            cbf_failnez( cbf_set_value(cbfdictionary, sub_category_id))

          }

        }

        if ((numrows==1) && (!cbf_find_local_tag(dictionary,"_type") ||
          !cbf_find_local_tag(dictionary,"_item_type.code") ) ) {

          if (!cbf_get_value(dictionary, &type_code)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "type_code"));

            cbf_failnez( cbf_set_value(cbfdictionary, type_code))

          }

        }

        if ((numrows==1) && (!cbf_find_local_tag(dictionary,"_units") ||
          !cbf_find_local_tag(dictionary,"_item_units.code") ) ) {

          if (!cbf_get_value(dictionary, &units_code)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "units_code"));

            cbf_failnez( cbf_set_value(cbfdictionary, units_code))

          }

        }

        if ((numrows==1) && (!cbf_find_local_tag(dictionary,"_enumeration_default") ||
          !cbf_find_local_tag(dictionary,"_item_default.value") ) ) {

          if (!cbf_get_value(dictionary, &default_value)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "default_value"));

            cbf_failnez( cbf_set_value(cbfdictionary, default_value))

          }

        }

        if ((numrows==1) && !cbf_find_local_tag(dictionary,"_item_aliases.alias_name") )  {

          if (!cbf_get_value(dictionary, &alias_name)) {

            cbf_failnez(cbf_find_category(cbfdictionary, "item_aliases"))

            if (cbf_find_hashedvalue(cbfdictionary, alias_name, "item_alias")) {

            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, alias_name, "item_alias"))

            }

            cbf_failnez( cbf_find_column(cbfdictionary, "alias_root"))

            cbf_failnez( cbf_set_value(cbfdictionary, itemname))

            cbf_failnez (cbf_compute_hashcode(itemname, &hashcode))

            cbf_failnez( cbf_find_column(cbfdictionary, "alias_root_hash"))

            cbf_failnez( cbf_set_integervalue(cbfdictionary, hashcode))

            cbf_failnez( cbf_get_value(cbfdictionary, &hashcodestring))

            cbf_failnez( cbf_row_number       (cbfdictionary,   &rownum))

            cbf_failnez( cbf_require_column   (cbfdictionary,   "alias_root_hash_next"))

            if ( cbf_get_integervalue(cbfdictionary, (int *)&rowlink) || rowlink <= 0) {

              cbf_failnez( cbf_set_integervalue (cbfdictionary,   -1))

              cbf_failnez( cbf_require_column   (cbfdictionary,   "alias_root_hash"))

              cbf_failnez( cbf_rewind_row       (cbfdictionary))

              cbf_failnez( cbf_find_row         (cbfdictionary,   hashcodestring))

              cbf_failnez( cbf_row_number       (cbfdictionary,   &orownum))

              cbf_failnez( cbf_find_column      (cbfdictionary,   "alias_root_hash_next"))

              while (orownum < rownum) {

                cbf_failnez( cbf_get_integervalue   (cbfdictionary,   (int *)&rownum))

                  if (nrownum == -1) {

                   cbf_failnez( cbf_set_integervalue(cbfdictionary,   rownum))

                   break;

                  }

                orownum = nrownum;

              }

            }

          }

        }

      }

    }

    if (!cbf_find_local_tag(dictionary, "_item_linked.parent_name") ||
      !cbf_find_local_tag(dictionary, "_item_link_parent"))  {

      cbf_failnez( cbf_count_rows (dictionary,&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      for (numrow=0; numrow < numrows; numrow++) {

        cbf_failnez( cbf_require_category  (cbfdictionary, "items"))

        cbf_failnez( cbf_select_row(dictionary, numrow) )

        parent_name = NULL;

        if (!cbf_find_column(dictionary,parent_name) ||
            !cbf_find_column(dictionary,"_list_link_parent") ||
            ((numrows==1)&& !cbf_find_local_tag(dictionary,"_list_link_parent")))
            if (cbf_get_value(dictionary,&parent_name)) parent_name = NULL;

        child_name = NULL;

        if (!cbf_find_column(dictionary,child_name) ||
            !cbf_find_column(dictionary,"_list_link_child") ||
            ((numrows==1)&& !cbf_find_local_tag(dictionary,"_list_link_child")))
            if (cbf_get_value(dictionary,&child_name)) child_name = NULL;

        if ((numrows==1) && (child_name == NULL)) {

          child_name = itemname;

        }

        if (parent_name && child_name) {

          if (cbf_find_hashedvalue(cbfdictionary, child_name, "name")) {

            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, child_name, "name"))

          }

          cbf_failnez(cbf_find_column(cbfdictionary, "parent"))

          cbf_failnez(cbf_set_value(cbfdictionary,parent_name))

        }

      }

    }

    if (!cbf_find_local_tag(dictionary,"_category") ||
      cbf_find_local_tag(dictionary,"_category.id") )  {

      cbf_failnez( cbf_count_rows (dictionary,&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      for (numrow=0; numrow < numrows; numrow++) {

        cbf_failnez( cbf_require_category  (cbfdictionary, "categories"))

        cbf_failnez( cbf_select_row(dictionary, numrow) )

        cbf_failnez( cbf_get_value         (dictionary, &categoryname))

        if (cbf_find_hashedvalue(cbfdictionary, categoryname, "id")) {

          cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, categoryname, "id"))

          cbf_failnez( cbf_require_column(cbfdictionary, "key"))

          cbf_failnez( cbf_set_value(cbfdictionary, " "))

        }


        if (!cbf_find_column(dictionary,"mandatory_code") ) {

          if (!cbf_get_value(dictionary, &mandatory_code)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "mandatory_code"));

            cbf_failnez( cbf_set_value(cbfdictionary, mandatory_code))

          }

        }


        if ((numrows==1) && (!cbf_find_local_tag(dictionary,"_list_reference") ||
          !cbf_find_local_tag(dictionary,"_category_key.name") ) ) {

          if (!cbf_get_value(dictionary, &key)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "key"))

            if (!cbf_get_value(cbfdictionary, &oldkey) && !oldkey && strcmp(oldkey," ")) {

                cbf_failnez(cbf_row_number(cbfdictionary, &orownum))

                cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, categoryname, "id"))

                 cbf_failnez(cbf_row_number(cbfdictionary, (unsigned int *)&rownum))

                cbf_failnez( cbf_select_row(cbfdictionary, orownum))

                 if (!cbf_find_column(cbfdictionary,"mandatory_code") ) {

                  if (!cbf_get_value(cbfdictionary,&mandatory_code)) {

                    cbf_failnez( cbf_select_row(cbfdictionary, nrownum))

                    cbf_failnez( cbf_set_value(cbfdictionary,mandatory_code))

                    cbf_failnez( cbf_select_row(cbfdictionary, orownum))

                  }

                }
                  if (!cbf_find_column(cbfdictionary,"group") ) {

                  if (!cbf_get_value(cbfdictionary,&group) ) {

                    cbf_failnez( cbf_select_row(cbfdictionary, nrownum))

                    cbf_failnez( cbf_set_value(cbfdictionary, group))

                    cbf_failnez( cbf_select_row(cbfdictionary, orownum))

                  }

                }

                cbf_failnez( cbf_select_row(cbfdictionary, nrownum))

                cbf_failnez( cbf_find_column(cbfdictionary, "key"))

            }

            cbf_failnez( cbf_set_value(cbfdictionary, key))

          }

        }

        if ((numrows==1) && (!cbf_find_local_tag(dictionary,"_category_group.id") ) ) {

          if (!cbf_get_value(dictionary, &group)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "group"));

            cbf_failnez( cbf_set_value(cbfdictionary, group))

          }

        }


        if ((numrows==1) && !cbf_find_local_tag(dictionary,"_category_aliases.alias_name") )  {

          if (!cbf_get_value(dictionary, &alias_name)) {

            cbf_failnez(cbf_find_category(cbfdictionary, "catgeory_aliases"))

            if (cbf_find_hashedvalue(cbfdictionary, alias_name, "category_alias")) {

            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, alias_name, "category_alias"))

            }

            cbf_failnez( cbf_find_column(cbfdictionary, "category_root"))

            cbf_failnez( cbf_set_value(cbfdictionary, categoryname))

            cbf_failnez (cbf_compute_hashcode(categoryname, &hashcode))

            cbf_failnez( cbf_find_column(cbfdictionary, "category_root_hash"))

            cbf_failnez( cbf_set_integervalue(cbfdictionary, hashcode))

            cbf_failnez( cbf_get_value(cbfdictionary, &hashcodestring))

            cbf_failnez( cbf_row_number       (cbfdictionary,   &rownum))

            cbf_failnez( cbf_require_column   (cbfdictionary,   "category_root_hash_next"))

            if ( cbf_get_integervalue(cbfdictionary, (int *)&rowlink) || rowlink <= 0) {

              cbf_failnez( cbf_set_integervalue (cbfdictionary,   -1))

              cbf_failnez( cbf_require_column   (cbfdictionary,   "category_root_hash"))

              cbf_failnez( cbf_rewind_row       (cbfdictionary))

              cbf_failnez( cbf_find_row         (cbfdictionary,   hashcodestring))

              cbf_failnez( cbf_row_number       (cbfdictionary,   &orownum))

              cbf_failnez( cbf_find_column      (cbfdictionary,   "category_root_hash_next"))

              while (orownum < rownum) {

                cbf_failnez( cbf_get_integervalue   (cbfdictionary,   (int *)&rownum))

                if (nrownum == -1) {

                  cbf_failnez( cbf_set_integervalue(cbfdictionary,   rownum))

                  break;

                }

                orownum = nrownum;

              }

            }

          }

        }

      }

    }


    return 0;
}

  /* Convert a DDL1 or DDL2 dictionary and add it to a CBF dictionary */


int cbf_convert_dictionary (cbf_handle handle, cbf_handle dictionary )
{
    cbf_handle dict;

    unsigned int blocks, frames, blockitems;

    int blocknum, itemnum;

    CBF_NODETYPE itemtype;

    const char *datablock_name;

    const char *saveframe_name;

    if (!handle || !dictionary ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dict))

    cbf_failnez( cbf_require_datablock  (dict, "cbf_dictionary"))

    cbf_failnez( cbf_require_category   (dict, "category_aliases"))

      cbf_failnez( cbf_require_column   (dict, "category_root"))

      cbf_failnez( cbf_require_column   (dict, "category_alias"))

      cbf_failnez( cbf_require_column   (dict, "category_root_hash"))

      cbf_failnez( cbf_require_column   (dict, "category_alias_hash"))

      cbf_failnez( cbf_require_column   (dict, "category_root_hash_next"))

      cbf_failnez( cbf_require_column   (dict, "category_alias_hash_next"))


    cbf_failnez( cbf_require_category   (dict, "item_aliases"))

      cbf_failnez( cbf_require_column   (dict, "item_root"))

      cbf_failnez( cbf_require_column   (dict, "item_alias"))

      cbf_failnez( cbf_require_column   (dict, "item_root_hash"))

      cbf_failnez( cbf_require_column   (dict, "item_alias_hash"))

      cbf_failnez( cbf_require_column   (dict, "item_root_hash_next"))

      cbf_failnez( cbf_require_column   (dict, "item_alias_hash_next"))


    cbf_failnez( cbf_require_category   (dict, "categories"))

      cbf_failnez( cbf_require_column   (dict, "id"))

      cbf_failnez( cbf_require_column   (dict, "id_hash"))

      cbf_failnez( cbf_require_column   (dict, "id_hash_next"))

      cbf_failnez( cbf_require_column   (dict, "group"))

      cbf_failnez( cbf_require_column   (dict, "key"))

      cbf_failnez( cbf_require_column   (dict, "mandatory_code"))


    cbf_failnez( cbf_require_category   (dict, "items"))

      cbf_failnez( cbf_require_column   (dict, "name"))

      cbf_failnez( cbf_require_column   (dict, "name_hash"))

      cbf_failnez( cbf_require_column   (dict, "name_hash_next"))

      cbf_failnez( cbf_require_column   (dict, "type_code"))

      cbf_failnez( cbf_require_column   (dict, "units_code"))

      cbf_failnez( cbf_require_column   (dict, "category_id"))

      cbf_failnez( cbf_require_column   (dict, "sub_category_id"))

      cbf_failnez( cbf_require_column   (dict, "mandatory_code"))

      cbf_failnez( cbf_require_column   (dict, "default_value"))

      cbf_failnez( cbf_require_column   (dict, "parent"))

      cbf_failnez (cbf_rewind_datablock (dictionary))

      cbf_failnez (cbf_count_datablocks (dictionary, &blocks))

    for (blocknum = 0; blocknum < blocks;  blocknum++ )
    {
      cbf_failnez (cbf_select_datablock(dictionary, blocknum))

      cbf_failnez (cbf_datablock_name(dictionary, &datablock_name))

      if ( !cbf_rewind_blockitem(dictionary, &itemtype) ) {

        if (cbf_count_saveframes(dictionary, &frames) || frames == 0)
        {
            cbf_failnez( cbf_convert_dictionary_definition(dict, dictionary, datablock_name))

        } else {

          cbf_failnez (cbf_count_blockitems(dictionary, &blockitems))

          for (itemnum = 0; itemnum < blockitems;  itemnum++) {

            cbf_select_blockitem(dictionary, itemnum, &itemtype);

            if (itemtype == CBF_SAVEFRAME) {

              cbf_failnez( cbf_saveframe_name(dictionary, &saveframe_name))

              cbf_failnez( cbf_convert_dictionary_definition(dict, dictionary, saveframe_name))

            }

          }

        }

      }

    }

     return 0;

}


  /* Find the requested tag anywhere in the cbf, make it the current column */

int cbf_find_tag (cbf_handle handle, const char *tag)
{
  cbf_node *node;

  size_t catlen, collen;

  char categoryname[81];

  char columnname[81];

  char *colstart;

  if (!handle || !tag) return CBF_ARGUMENT;

  if (strlen(tag)>80) return CBF_ARGUMENT;

  if (tag[0] == '_') tag++;

  if (!(colstart = strchr(tag,'.'))) {

    colstart=(char *)tag-1;

    catlen = 0;

  } else {

    catlen = colstart-tag;

  }
  if (catlen) strncpy(categoryname,tag,catlen);

  categoryname[catlen] = '\0';

  collen = (tag+strlen(tag))-colstart;

  if (collen) strncpy(columnname,colstart+1,collen);

  columnname[collen] = '\0';

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))

  cbf_failnez(cbf_srch_tag(handle, node, categoryname, columnname))

  return 0;

}

  /* Find the requested tag in the cbf within the current

     save frame or data block, make it the current column */

int cbf_find_local_tag (cbf_handle handle, const char *tag)
{
  cbf_node *node;

  size_t catlen, collen;

  char categoryname[81];

  char columnname[81];

  char *colstart;

  if (!handle || !tag) return CBF_ARGUMENT;

  if (strlen(tag)>80) return CBF_ARGUMENT;

  if (tag[0] == '_') tag++;

  if (!(colstart = strchr(tag,'.'))) {

    colstart=(char *)tag-1;

    catlen = 0;

  } else {

    catlen = colstart-tag;

  }
  if (catlen) strncpy(categoryname,tag,catlen);

  categoryname[catlen] = '\0';

  collen = (tag+strlen(tag))-colstart;

  if (collen) strncpy(columnname,colstart+1,collen);

  columnname[collen] = '\0';

  if (cbf_find_parent (&node, handle->node, CBF_SAVEFRAME))
  {

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_DATABLOCK))

  }

  cbf_failnez(cbf_srch_tag(handle, node, categoryname, columnname))

  return 0;

}


  /* Find the requested category and column anywhere in the cbf, make it the current column */


int cbf_srch_tag (cbf_handle handle, cbf_node *node,
                                     const char *categoryname,
                                     const char *columnname)
{
  unsigned int children, child;

  if (!node) return CBF_NOTFOUND;

  node = cbf_get_link(node);

  if (node->type == CBF_CATEGORY) {

    if ((!(node->name)&&(categoryname[0]=='\0'))||
        ((node->name)&&!strcasecmp(node->name,categoryname))) {

      cbf_failnez (cbf_find_child(&node,node,columnname))

      handle->node = node;

      return 0;

    } else {

      return CBF_NOTFOUND;

    }
  }

  children = node->children;

  for (child = 0; child < children; child++) {

    if(! cbf_srch_tag(handle, (node->child)[child],
      categoryname, columnname)) return 0;

  }

  return CBF_NOTFOUND;

}

  /* Find the root alias of a given category */

int cbf_find_category_root (cbf_handle handle, const char* categoryname,
                                            const char** categoryroot)
{
    cbf_handle dictionary;

    if (!handle || !categoryname || !categoryroot ) return CBF_ARGUMENT;

    dictionary = (cbf_handle) handle->dictionary;

    if (!dictionary) return CBF_NOTFOUND;

    cbf_failnez( cbf_find_tag(dictionary, "_category_aliases.alias_id"))

    cbf_failnez( cbf_rewind_row(dictionary))

    cbf_failnez( cbf_find_row(dictionary, categoryname))

    cbf_failnez( cbf_find_column(dictionary, "root_id"))

    return cbf_get_value(dictionary,categoryroot);

}

  /* Find the root alias of a given category, defaulting to the current one */

int cbf_require_category_root (cbf_handle handle, const char* categoryname,
                                            const char** categoryroot)
{
    if (!handle || !categoryname || !categoryroot ) return CBF_ARGUMENT;

    if (cbf_find_category_root(handle,categoryname,categoryroot))

      * categoryroot = categoryname;

    return 0;

}

  /* Set the root alias of a given category */

int cbf_set_category_root (cbf_handle handle, const char* categoryname,
                                            const char* categoryroot)
{
    cbf_handle dictionary;

    char * tempcat;

    if (!handle || !categoryname || !categoryroot ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dictionary))

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_category_aliases.alias_id")) {

        cbf_failnez( cbf_require_datablock(dictionary, "dictionary"))

        cbf_failnez( cbf_require_category(dictionary, "category_aliases"))

        cbf_failnez( cbf_require_column(dictionary, "alias_id"))
    }

    cbf_failnez( cbf_require_column(dictionary, "root_id"))

    cbf_failnez( cbf_rewind_row(dictionary))

    while (cbf_find_nextrow(dictionary,categoryroot)) {

        cbf_failnez( cbf_require_column(dictionary, "alias_id"))

        if (cbf_get_value(dictionary,(const char **)&tempcat)) {

          if (tempcat && !strcasecmp(tempcat,categoryname))return 0;

        }

        cbf_failnez( cbf_find_column(dictionary, "root_id"))

    }

    cbf_failnez( cbf_new_row(dictionary))

    cbf_failnez( cbf_set_value(dictionary,categoryroot))

    cbf_failnez( cbf_find_column(dictionary, "alias_id"))

    return cbf_set_value(dictionary,categoryname);

}

  /* Find the root alias of a given tag */

int cbf_find_tag_root (cbf_handle handle, const char* tagname,
                                            const char** tagroot)
{
    cbf_handle dictionary;

    if (!handle || !tagname || !tagroot ) return CBF_ARGUMENT;

    dictionary = (cbf_handle) handle->dictionary;

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_item_aliases.alias_name"))
      {
        return CBF_NOTFOUND;
      }

    cbf_failnez( cbf_rewind_row(dictionary))

    cbf_failnez( cbf_find_row(dictionary, tagname))

    cbf_failnez( cbf_find_column(dictionary, "root_name"))

    return cbf_get_value(dictionary,tagroot);

}

  /* Find the root alias of a given tag, defaulting to the current one */

int cbf_require_tag_root (cbf_handle handle, const char* tagname,
                                            const char** tagroot)
{
    if (!handle || !tagname || !tagroot ) return CBF_ARGUMENT;

    if (cbf_find_tag_root(handle,tagname,tagroot))
      * tagroot = tagname;

    return 0;

}

  /* Set the root alias of a given tag */

int cbf_set_tag_root (cbf_handle handle, const char* tagname,
                                            const char* tagroot)
{
    cbf_handle dictionary;

    char * temptag;

    if (!handle || !tagname || !tagroot ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dictionary))

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_item_aliases.alias_name")) {

        cbf_failnez( cbf_require_datablock(dictionary, "dictionary"))

        cbf_failnez( cbf_require_category(dictionary, "item_aliases"))

        cbf_failnez( cbf_require_column(dictionary, "alias_name"))
    }

    cbf_failnez( cbf_require_column(dictionary, "root_name"))

    cbf_failnez( cbf_rewind_row(dictionary))

    while (cbf_find_nextrow(dictionary,tagroot)) {

        cbf_failnez( cbf_require_column(dictionary, "root_name"))

        if (cbf_get_value(dictionary,(const char **)&temptag)) {

          if (temptag && !strcasecmp(temptag,tagname))return 0;

        }

        cbf_failnez( cbf_find_column(dictionary, "root_name"))

    }

    cbf_failnez( cbf_new_row(dictionary))

    cbf_failnez( cbf_set_value(dictionary,tagroot))

    cbf_failnez( cbf_find_column(dictionary, "alias_name"))

    return cbf_set_value(dictionary,tagname);

}


  /* Find the category of a given tag */

int cbf_find_tag_category (cbf_handle handle, const char* tagname,
                                            const char** categoryname)
{
    cbf_handle dictionary;

    if (!handle || !tagname || !categoryname ) return CBF_ARGUMENT;

    dictionary = (cbf_handle) handle->dictionary;

    if (!dictionary) return CBF_NOTFOUND;

    cbf_failnez( cbf_find_tag(dictionary, "_item.name"))

    cbf_failnez( cbf_rewind_row(dictionary))

    cbf_failnez( cbf_find_row(dictionary, tagname))

    cbf_failnez( cbf_find_column(dictionary, "category_id"))

    return cbf_get_value(dictionary,categoryname);

}


  /* Set category of a given tag */

int cbf_set_tag_category (cbf_handle handle, const char* tagname,
                                            const char* categoryname)
{
    cbf_handle dictionary;

    char * tempcat;

    if (!handle || !tagname || !categoryname ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dictionary))

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_item.name")) {

        cbf_failnez( cbf_require_datablock(dictionary, "dictionary"))

        cbf_failnez( cbf_require_category(dictionary, "item"))

        cbf_failnez( cbf_require_column(dictionary, "name"))
    }

    cbf_failnez( cbf_require_column(dictionary, "category_id"))

    cbf_failnez( cbf_rewind_row(dictionary))

    cbf_failnez( cbf_find_column(handle, tagname))

    while (cbf_find_nextrow(dictionary,tagname)) {

        cbf_failnez( cbf_require_column(dictionary, "catgeory_id"))

        if (cbf_get_value(dictionary,(const char **)&tempcat)) {

          if (tempcat && !strcasecmp(tempcat,categoryname))return 0;

        }

        cbf_failnez( cbf_find_column(dictionary, "name"))

    }

    cbf_failnez( cbf_new_row(dictionary))

    cbf_failnez( cbf_set_value(dictionary,tagname))

    cbf_failnez( cbf_find_column(dictionary, "category_id"))

    return cbf_set_value(dictionary,categoryname);

}





#ifdef __cplusplus

}

#endif

