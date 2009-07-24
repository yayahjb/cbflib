/**********************************************************************
 * cbf -- cbflib API functions                                        *
 *                                                                    *
 * Version 0.8.0 20 July 2008                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006, 2007 Herbert J. Bernstein                      *
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
#include "cbf_ascii.h"

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <regex.h>

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
  
  (*handle)->file = NULL;
  
  (*handle)->logfile = stderr;
  
  (*handle)->warnings = 0;
  
  (*handle)->errors = 0;

  (*handle)->startline = 0;

  (*handle)->startcolumn = 0;

  return 0;
}

int cbf_set_cbf_logfile (cbf_handle handle, FILE * logfile) 
{
	handle ->logfile = logfile;
	
	return 0;
}

  /* Free a handle */

int cbf_free_handle (cbf_handle handle)
{
  int errorcode;
  
  void *memblock;
  
  cbf_node *node;

  errorcode = 0;
  
  memblock = (void *) handle;

  if (handle && (--(handle->refcount) <= 0) )
  {
    if (handle->dictionary) {

      errorcode |=
        cbf_free_handle ((cbf_handle) handle->dictionary);
        
      handle->dictionary = NULL;

    }
    
    errorcode |= cbf_find_parent (&node, handle->node, CBF_ROOT);

    if (!errorcode) errorcode |= cbf_free_node (node);

    return errorcode | cbf_free (&memblock, NULL);
  }

  return 0;
}


  /* Read a file or a wide file */

static int cbf_read_anyfile (cbf_handle handle, FILE *stream, int flags, const char * buffer, size_t buffer_size)
{
  cbf_file *file;

  cbf_node *node, *tnode;

  void *parse [4];

  int errorcode;
  
  unsigned int children;

  const char *name;


    /* Check the arguments */

  if (!handle) {

    if (stream)
      fclose (stream);
    
    return CBF_ARGUMENT;
    	
  }


  if (((flags & (MSG_DIGEST | MSG_DIGESTNOW | MSG_DIGESTWARN)) && (flags & MSG_NODIGEST))) {

    if (stream)
      fclose (stream);
    
    return CBF_ARGUMENT;
    	
  }
  
  if (((flags & PARSE_NOBRACKETS) && (flags & (PARSE_BRACKETS|PARSE_LIBERAL_BRACKETS)))
    ||((flags & PARSE_TRIPLE_QUOTES) && (flags & PARSE_NOTRIPLE_QUOTES)) ) {

    if (stream)
      fclose (stream);
    
    return CBF_ARGUMENT;
    	
  }
  
  if (!stream && (!buffer || !buffer_size))
  
    return CBF_ARGUMENT;


    /* Delete the old datablocks */

  cbf_onfailnez (cbf_find_parent (&node, handle->node, CBF_ROOT), fclose(stream))

  cbf_onfailnez (cbf_set_children (node, 0), if (stream) fclose(stream))

  handle->node = node;
  
  cbf_onfailnez (cbf_reset_refcounts(handle->dictionary), if (stream) fclose(stream))


    /* Create the input file */

  if (flags&PARSE_WIDE) {
  	
    cbf_onfailnez (cbf_make_widefile (&file, stream), if (stream) fclose(stream))
  
  } else {

    cbf_onfailnez (cbf_make_file (&file, stream), if (stream) fclose(stream))
  	
  }
  
  handle->file = file;
  
  if (buffer && buffer_size != 0) {
    
    cbf_onfailnez (cbf_set_io_buffersize(file, buffer_size+1), if (stream) fclose(stream))
    
    memmove((void *)file->characters_base,(const void *)buffer,buffer_size);
    
    file->characters = file->characters_base;
    
    file->characters_used = buffer_size;
    
    if (stream) {
    
      file->characters[file->characters_used++] = '\n';
    	
    }
    
  }


    /* Defaults */

  if ((flags & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW | MSG_DIGESTWARN )) == 0)

    flags |= (HDR_DEFAULT & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW | MSG_DIGESTWARN));

  if (flags & (MSG_DIGESTNOW | MSG_DIGESTWARN) )

    flags |= MSG_DIGEST;


    /* Copy the flags */

  file->read_headers = flags;


    /* Parse the file */

  parse [0] = file;
  parse [1] = handle->node;
  parse [2] = handle;
  parse [3] = 0;

  errorcode = cbf_parse (parse);


    /* Validate the last category, save frame and data block and do
       overall checks */
  
  cbf_failnez(cbf_validate(handle, handle->node, CBF_ROOT, (cbf_node *)NULL) )

    /* Delete the first datablock if it's empty */

  if (!errorcode)
  {
    errorcode = cbf_get_child (&tnode, node, 0);

    if (!errorcode)
    {
      errorcode = cbf_get_name (&name, tnode);

      if (!errorcode && !name)
      {
        errorcode = cbf_count_children (&children, tnode);

        if (!errorcode && !children)

          errorcode = cbf_free_node (tnode);
      }
    }
    else

      if (errorcode == CBF_NOTFOUND)

        errorcode = 0;
  }

  cbf_onfailnez (cbf_find_parent (&node, handle->node, CBF_ROOT), cbf_delete_fileconnection (&file))
  
  errorcode = cbf_count_children (&children, node);

  if (!errorcode && !children) {

    cbf_log(handle, "no data blocks found", CBF_LOGWARNING|CBF_LOGWOLINE);
  	
  }
  
    /* Disconnect the file */
    
  handle->file = NULL;

  return errorcode 
    |(handle->errors?CBF_FORMAT:0)
    | cbf_delete_fileconnection (&file);
}

  /* Read a file */

int cbf_read_file (cbf_handle handle, FILE *stream, int flags) 
{
	return cbf_read_anyfile (handle, stream, flags, NULL, 0);
}

  /* Read a wide file */


int cbf_read_widefile (cbf_handle handle, FILE *stream, int flags) 
{
	return cbf_read_anyfile (handle, stream, flags|PARSE_WIDE, NULL, 0);
}

  /* Read a pre-read buffered file */
  
int cbf_read_buffered_file (cbf_handle handle, FILE *stream, int flags, 
                            const char * buffer, size_t buffer_len)
{
	return cbf_read_anyfile (handle, stream, flags, buffer, buffer_len);	
}


  /* Write a file */

int cbf_write_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int flags,
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
                             ENC_BASE32K |
                             ENC_QP      |
                             ENC_FORWARD |
                             ENC_BACKWARD)) | ENC_NONE | ENC_CRTERM
                                                       | ENC_LFTERM;


    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

  if (((flags  & MIME_HEADERS)  && (flags  & PLAIN_HEADERS)) ||
      ((flags  & MSG_DIGEST)    && (flags  & MSG_NODIGEST))  ||
      ((flags  & MSG_DIGEST)    && (flags  & PLAIN_HEADERS)) ||
      ((flags  & MSG_DIGESTNOW) && (flags  & MSG_NODIGEST))  ||
      ((flags  & MSG_DIGESTNOW) && (flags  & PLAIN_HEADERS)) ||
      ((encoding & ENC_FORWARD)   && (encoding & ENC_BACKWARD)))

    return CBF_ARGUMENT;

  if (((encoding & ENC_NONE)    > 0) +
      ((encoding & ENC_BASE8)   > 0) +
      ((encoding & ENC_BASE10)  > 0) +
      ((encoding & ENC_BASE16)  > 0) +
      ((encoding & ENC_BASE64)  > 0) +
      ((encoding & ENC_BASE32K) > 0) +
      ((encoding & ENC_QP)      > 0) > 1)

    return CBF_ARGUMENT;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Create the file */

  cbf_failnez (cbf_make_file (&file, stream))


    /* Defaults */

  if (flags & (MSG_DIGEST | MSG_DIGESTNOW))

    flags |= MIME_HEADERS;

  else

    if ((flags & (MIME_HEADERS | PLAIN_HEADERS)) == 0)

      flags |= (HDR_DEFAULT & (MIME_HEADERS | PLAIN_HEADERS));

  if (flags & PLAIN_HEADERS)

    flags |= MSG_NODIGEST;

  else

    if ((flags & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW)) == 0)

      flags |= (HDR_DEFAULT & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW));

  if (flags & MSG_DIGESTNOW)

    flags |= MSG_DIGEST;

  if ((encoding & (ENC_NONE    |
                   ENC_BASE8   |
                   ENC_BASE10  |
                   ENC_BASE16  |
                   ENC_BASE64  |
                   ENC_BASE32K |
                   ENC_QP)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_NONE    |
                                ENC_BASE8   |
                                ENC_BASE10  |
                                ENC_BASE16  |
                                ENC_BASE64  |
                                ENC_BASE32K |
                                ENC_QP));

  if ((encoding & (ENC_CRTERM | ENC_LFTERM)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_CRTERM | ENC_LFTERM));

  if ((encoding & (ENC_FORWARD | ENC_BACKWARD)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_FORWARD | ENC_BACKWARD));


    /* Copy the flags */

  file->write_headers  = flags;
  file->write_encoding = encoding;
  
    /* Reset the reference counts */
    
 cbf_failnez( cbf_reset_refcounts(handle->dictionary) )


    /* Write the file */

  errorcode = cbf_write_node (handle, node, file, isbuffer);


    /* Free the file structure but don't close the file? */

  if (!isbuffer)

    file->stream = NULL;


    /* Disconnect the file */

  return errorcode | cbf_delete_fileconnection (&file);
}


  /* Write a file, starting from the local node */

int cbf_write_local_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int flags,
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
                             ENC_BASE32K |
                             ENC_QP      |
                             ENC_FORWARD |
                             ENC_BACKWARD)) | ENC_NONE | ENC_CRTERM
                                                       | ENC_LFTERM;


    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

  if (((flags  & MIME_HEADERS)  && (flags  & PLAIN_HEADERS)) ||
      ((flags  & MSG_DIGEST)    && (flags  & MSG_NODIGEST))  ||
      ((flags  & MSG_DIGEST)    && (flags  & PLAIN_HEADERS)) ||
      ((flags  & MSG_DIGESTNOW) && (flags  & MSG_NODIGEST))  ||
      ((flags  & MSG_DIGESTNOW) && (flags  & PLAIN_HEADERS)) ||
      ((encoding & ENC_FORWARD)   && (encoding & ENC_BACKWARD)))

    return CBF_ARGUMENT;

  if (((encoding & ENC_NONE)    > 0) +
      ((encoding & ENC_BASE8)   > 0) +
      ((encoding & ENC_BASE10)  > 0) +
      ((encoding & ENC_BASE16)  > 0) +
      ((encoding & ENC_BASE64)  > 0) +
      ((encoding & ENC_BASE32K) > 0) +
      ((encoding & ENC_QP)     > 0) > 1)

    return CBF_ARGUMENT;


    /* Create the file */

  cbf_failnez (cbf_make_file (&file, stream))


    /* Defaults */

  if (flags & (MSG_DIGEST | MSG_DIGESTNOW))

    flags |= MIME_HEADERS;

  else

    if ((flags & (MIME_HEADERS | PLAIN_HEADERS)) == 0)

      flags |= (HDR_DEFAULT & (MIME_HEADERS | PLAIN_HEADERS));

  if (flags & PLAIN_HEADERS)

    flags |= MSG_NODIGEST;

  else

    if ((flags & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW)) == 0)

      flags |= (HDR_DEFAULT & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW));

  if (flags & MSG_DIGESTNOW)

    flags |= MSG_DIGEST;

  if ((encoding & (ENC_NONE    |
                   ENC_BASE8   |
                   ENC_BASE10  |
                   ENC_BASE16  |
                   ENC_BASE64  |
                   ENC_BASE32K |
                   ENC_QP)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_NONE    |
                                ENC_BASE8   |
                                ENC_BASE10  |
                                ENC_BASE16  |
                                ENC_BASE64  |
                                ENC_BASE32K |
                                ENC_QP));

  if ((encoding & (ENC_CRTERM | ENC_LFTERM)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_CRTERM | ENC_LFTERM));

  if ((encoding & (ENC_FORWARD | ENC_BACKWARD)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_FORWARD | ENC_BACKWARD));


    /* Copy the flags */

  file->write_headers  = flags;
  file->write_encoding = encoding;

  node = handle->node;

    /* Write the file */

  errorcode = cbf_write_node (handle, node, file, isbuffer);


    /* Free the file structure but don't close the file? */

  if (!isbuffer)

    file->stream = NULL;


    /* Disconnect the file */

  return errorcode | cbf_delete_fileconnection (&file);
}



  /* Write a wide file */

int cbf_write_widefile (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int flags,
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
                             ENC_BASE32K |
                             ENC_QP      |
                             ENC_FORWARD |
                             ENC_BACKWARD)) | ENC_NONE | ENC_CRTERM
                                                       | ENC_LFTERM;


    /* Check the arguments */

  if (!handle)

    return CBF_ARGUMENT;

  if (((flags  & MIME_HEADERS)  && (flags  & PLAIN_HEADERS)) ||
      ((flags  & MSG_DIGEST)    && (flags  & MSG_NODIGEST))  ||
      ((flags  & MSG_DIGEST)    && (flags  & PLAIN_HEADERS)) ||
      ((flags  & MSG_DIGESTNOW) && (flags  & MSG_NODIGEST))  ||
      ((flags  & MSG_DIGESTNOW) && (flags  & PLAIN_HEADERS)) ||
      ((encoding & ENC_FORWARD)   && (encoding & ENC_BACKWARD)))

    return CBF_ARGUMENT;

  if (((encoding & ENC_NONE)    > 0) +
      ((encoding & ENC_BASE8)   > 0) +
      ((encoding & ENC_BASE10)  > 0) +
      ((encoding & ENC_BASE16)  > 0) +
      ((encoding & ENC_BASE64)  > 0) +
      ((encoding & ENC_BASE32K) > 0) +
      ((encoding & ENC_QP)     > 0) > 1)

    return CBF_ARGUMENT;


    /* Find the root node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))


    /* Create the file */

  cbf_failnez (cbf_make_widefile (&file, stream))


    /* Defaults */

  if (flags & (MSG_DIGEST | MSG_DIGESTNOW))

    flags |= MIME_HEADERS;

  else

    if ((flags & (MIME_HEADERS | PLAIN_HEADERS)) == 0)

      flags |= (HDR_DEFAULT & (MIME_HEADERS | PLAIN_HEADERS));

  if (flags & PLAIN_HEADERS)

    flags |= MSG_NODIGEST;

  else

    if ((flags & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW)) == 0)

      flags |= (HDR_DEFAULT & (MSG_DIGEST | MSG_NODIGEST | MSG_DIGESTNOW));

  if (flags & MSG_DIGESTNOW)

    flags |= MSG_DIGEST;

  if ((encoding & (ENC_NONE    |
                   ENC_BASE8   |
                   ENC_BASE10  |
                   ENC_BASE16  |
                   ENC_BASE64  |
                   ENC_BASE32K |
                   ENC_QP)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_NONE    |
                                ENC_BASE8   |
                                ENC_BASE10  |
                                ENC_BASE16  |
                                ENC_BASE64  |
                                ENC_BASE32K |
                                ENC_QP));

  if ((encoding & (ENC_CRTERM | ENC_LFTERM)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_CRTERM | ENC_LFTERM));

  if ((encoding & (ENC_FORWARD | ENC_BACKWARD)) == 0)

    encoding |= (ENC_DEFAULT & (ENC_FORWARD | ENC_BACKWARD));


    /* Copy the flags */

  file->write_headers  = flags;
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

  for (i = 0; (unsigned int)i < node->children; i++) {

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

  for (i = 0; (unsigned int)i < node->children; i++) {

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

  for (i = index+1; (unsigned int)i<parent->children; i++) {

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

  for (i = index+1; (unsigned int)i<parent->children; i++) {

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



  /* Get the number of the current column */

int cbf_column_number (cbf_handle handle, unsigned int *column)
{

  cbf_node *parent, *node;

  if (!handle)

    return CBF_ARGUMENT;


    /* Find the column node */

  cbf_failnez (cbf_find_parent (&node, handle->node, CBF_COLUMN))


    /* Find the category node */

  cbf_failnez (cbf_find_parent (&parent, node, CBF_CATEGORY))


    /* Which child is this? */

  cbf_failnez (cbf_child_index (column, node))


    /* Success */

  return 0;
}


  /* Get the number of the current block item */

int cbf_blockitem_number (cbf_handle handle, unsigned int *blockitem)
{
  
  cbf_node *parent, *node;

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

  cbf_failnez (cbf_child_index (blockitem, node))

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
  if (cbf_get_value (handle, value) || !*value) 
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
  
  char buffer[80];
  
  char *endptr;


    /* Get the value */

  cbf_failnez (cbf_get_value (handle, &value))


    /* Convert it into a double */

  if (!value)

    return CBF_NOTFOUND;

  if (number) {
  
    *number = strtod(value,&endptr);
    
    if (!*endptr) return 0;
    
    strncpy(buffer,value,79);
    
    buffer[79] = '\0';
    
    if (*endptr == '.') *(buffer+(endptr-value)) = ',';
    
    if (!cbf_cistrncmp(buffer,",",80) || !cbf_cistrncmp(buffer,"?",80)) {
    
      *number = 0;
      
      return 0;
    	
    }
    
    *number = strtod(buffer,&endptr);

    if (!*endptr || *endptr==' ') return 0;
    
    return CBF_FORMAT;
  	
  }

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

  int lopos, hipos, ic;


    /* Write the value */

  sprintf (value, format, number);

    /* strip the leading and trailing blanks */

  for (lopos=0; (value[lopos]==' '||value[lopos]=='\t'); lopos++);

  for (hipos=strlen(value+lopos); hipos>0&&(value[lopos+hipos-1]==' '||value[lopos+hipos-1]=='\t'); hipos--);

  *(value+lopos+hipos) = '\0';
  
    /* undo locale conversions of '.' to ',' */
    
  for (ic = 0; ic < strlen(value+lopos); ic++)
    if (value[lopos+ic] == ',') value[lopos+ic] = '.';


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

int cbf_get_arrayparameters (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    int          *elsigned,
                                    int          *elunsigned,
                                    size_t       *nelem,
                                    int          *minelem,
                                    int          *maxelem,
                                    int          *realarray)
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
                                minelem, maxelem, realarray,
                                NULL, NULL, NULL, NULL, NULL);
}



  /* Get the parameters of the current (row, column) array entry */

int cbf_get_arrayparameters_wdims (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    int          *elsigned,
                                    int          *elunsigned,
                                    size_t       *nelem,
                                    int          *minelem,
                                    int          *maxelem,
                                    int          *realarray,
                                    const char  **byteorder,
                                    size_t       *dimfast,
                                    size_t       *dimmid,
                                    size_t       *dimslow,
                                    size_t       *padding)
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
                                minelem, maxelem, realarray,
                                byteorder, dimfast, dimmid, dimslow, padding);
}




  /* Get the parameters of the current (row, column) integer array entry */

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
                                minelem, maxelem, &realarray,
                                NULL, NULL, NULL, NULL, NULL);
}

  /* Get the parameters of the current (row, column) integer array entry */
  

int cbf_get_integerarrayparameters_wdims (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    int          *elsigned,
                                    int          *elunsigned,
                                    size_t       *nelem,
                                    int          *minelem,
                                    int          *maxelem,
                                    const char  **byteorder,
                                    size_t       *dimfast,
                                    size_t       *dimmid,
                                    size_t       *dimslow,
                                    size_t       *padding)
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
                                minelem, maxelem, &realarray,
                                byteorder,dimfast,dimmid,dimslow,padding);
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
                                NULL, NULL, NULL,
                                NULL, NULL, NULL, NULL, NULL);
}


  /* Get the parameters of the current (row, column) array entry */

int cbf_get_realarrayparameters_wdims (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    size_t       *nelem,
                                    const char  **byteorder,
                                    size_t       *dimfast,
                                    size_t       *dimmid,
                                    size_t       *dimslow,
                                    size_t       *padding)
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
                                NULL, NULL, NULL,
                                byteorder,dimfast,dimmid,dimslow,padding);
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
  
  const char *byteorder;
  
  size_t dimover, dimfast, dimmid, dimslow, padding;

  if (!handle)

    return CBF_ARGUMENT;

  return cbf_get_binary (handle->node, handle->row, id,
                         value, elsize, elsign, nelem, nelem_read, &realarray,
                         &byteorder,&dimover, &dimfast, &dimmid, &dimslow, &padding);
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
  
  const char *byteorder;
  
  size_t dimover, dimfast, dimmid, dimslow, padding;

  if (!handle)

    return CBF_ARGUMENT;

  return cbf_get_binary (handle->node, handle->row, id,
                         value, elsize, 1, nelem, nelem_read, &realarray,
                         &byteorder, &dimover, &dimfast, &dimmid, &dimslow, &padding);
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
                         compression, id, value, elsize, elsign, nelem, 0,
                         "little_endian", nelem, 0, 0, 0, 0);
}

  /* Set the integer value of the current (row, column) array entry */

int cbf_set_integerarray_wdims (cbf_handle    handle,
                          unsigned int  compression,
                          int           id,
                          void         *value,
                          size_t        elsize,
                          int           elsign,
                          size_t        nelem,
                          const char   *byteorder,
                          size_t        dimfast,
                          size_t        dimmid,
                          size_t        dimslow,
                          size_t        padding)
{
  if (!handle)

    return CBF_ARGUMENT;

  return cbf_set_binary (handle->node, handle->row,
                         compression, id, value, elsize, elsign, nelem, 0,
                         byteorder, nelem, dimfast, dimmid, dimslow, padding);
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
                         compression, id, value, elsize, 1, nelem, 1,
                         "little_endian", nelem, 0, 0, 0, 0);
}

  /* Set the real value of the current (row, column) array entry
     with dimensions */

int cbf_set_realarray_wdims (cbf_handle    handle,
                          unsigned int  compression,
                          int           id,
                          void         *value,
                          size_t        elsize,
                          size_t        nelem,
                          const char   *byteorder,
                          size_t        dimfast,
                          size_t        dimmid,
                          size_t        dimslow,
                          size_t        padding)
{
  if (!handle)

    return CBF_ARGUMENT;

  return cbf_set_binary (handle->node, handle->row,
                         compression, id, value, elsize, 1, nelem, 1,
                         byteorder, nelem, dimfast, dimmid, dimslow, padding);
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


  /* Issue a log message for a cbf */

void cbf_log (cbf_handle handle, const char *message, 
                                 int logflags)

{
  char * buffer;
  
  void * memblock;
  
  int line=0, column=0;
  
  if (cbf_alloc(&memblock, NULL, 1, strlen(message)+80) ) {
  	  
    fprintf (handle->logfile, "CBFlib: memory allocation error\n");
    
    return;

  }
  
  buffer = (char *)memblock;
  
  if (logflags & CBF_LOGCURRENTLOC) {

    line = (handle->file->line);

    column = (handle->file->column);

    logflags &= (~CBF_LOGWOLINE);

  } else if (logflags & CBF_LOGSTARTLOC) {

  	line = (handle->startline);

  	column = (handle->startcolumn);

    logflags &= (~CBF_LOGWOLINE);

  } else {

  	logflags |= CBF_LOGWOLINE;

  }
  
  if (logflags&CBF_LOGERROR)  handle->errors++;
  
  else if (logflags&CBF_LOGWARNING) handle->warnings++;

  if ( !handle->logfile ) return;

  if (handle->file) {
  
    if (logflags&CBF_LOGWOLINE)

      sprintf (buffer, "CBFlib: %s -- %s\n",
        (logflags&CBF_LOGERROR)?"error":
        ((logflags&CBF_LOGWARNING)?("warning"):""), 
        message);
        
    else if (logflags&CBF_LOGWOCOLUMN || column==0)  

      sprintf (buffer, "CBFlib: %s input line %d -- %s\n",
        (logflags&CBF_LOGERROR)?"error":
        ((logflags&CBF_LOGWARNING)?("warning"):""), 
        line+1, 
        message);  
    
    else

      sprintf (buffer, "CBFlib: %s input line %d (%d) -- %s\n",
        (logflags&CBF_LOGERROR)?"error":
        ((logflags&CBF_LOGWARNING)?("warning"):""), 
        line+1, column,
        message);  

    fprintf (handle->logfile, "%s", buffer);
    
  }
  
  cbf_free(&memblock, NULL );
 
  return;
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
  
    const char * datablockname;
    
    if (cbf_datablock_name(handle, &datablockname)) 
    
      cbf_failnez(cbf_require_datablock(handle,"(null)"))

    cbf_failnez(cbf_new_category(handle, categoryname))

  }
  return 0;
}

  /* Find a column, creating it if necessary */

int cbf_require_column (cbf_handle  handle,
                             const char *columnname)
{
  unsigned int currow, rows;
  
  if (cbf_row_number(handle,&currow)) currow = 0;
  
  if (cbf_count_rows(handle,&rows))   rows = 0;
  

  if (cbf_find_column(handle, columnname)) {

    cbf_failnez(cbf_count_rows(handle, &rows))
    
    cbf_failnez(cbf_new_column(handle, columnname))
    
    if (currow < rows) cbf_failnez(cbf_select_row(handle, currow))

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


   union ftest { float fltest; int itest; long ltest; } test;

   *real_format = other; 

   test.fltest = 1.;

   if (sizeof (float) == sizeof (long) ) {

     if ( test.ltest == 1065353216L ) *real_format = ieee;

   } else {

     if (sizeof (float) == sizeof (int ) ) {

       if (test.itest == 1065353216 ) *real_format = ieee;


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

  if (!cbf_get_dictionary(handle, dictionary)) return 0;

  cbf_failnez (cbf_make_handle((cbf_handle *)&(handle->dictionary)))

  *dictionary = (cbf_handle)(handle->dictionary);

  return 0;

}

  /* Put the value into the named column, updating the hash table links */

int cbf_set_hashedvalue(cbf_handle handle, const char * value, 
                                           const char * columnname, 
                                           int valuerow) {

  char colhashnext[91];
  
  char * category;
  
  const char * ovalue;
  
  int ohashnext;
  
  char categoryhashtable[91];

  unsigned int hashcode, ohashcode;

  int orownum, rownum, nrownum=0, catrownum;

  int colnamelen, catnamelen;

  if ( !columnname ) return CBF_ARGUMENT;

  if ( (colnamelen = strlen(columnname)) > 80 ) return CBF_ARGUMENT;

  cbf_failnez(cbf_category_name (handle, (const char * *)&category));
  
  if ( (catnamelen = strlen(category)) > 80 ) return CBF_ARGUMENT;
  
  
  strcpy (categoryhashtable,category);
  
  strcpy (categoryhashtable + catnamelen, "(hash_table)");

  strcpy (colhashnext, columnname);

  strcpy (colhashnext+colnamelen, "(hash_next)");

  cbf_failnez( cbf_compute_hashcode(value, &hashcode))

  cbf_failnez( cbf_require_column(handle, columnname))

    /* If we are going to hash an exisiting row, we need to
  
     undo any existing hash to the same row */
     
  if (valuerow >= 0) {
  
    cbf_failnez( cbf_select_row       (handle, valuerow))
    
    if (!cbf_get_value(handle,&ovalue) && ovalue
      && !cbf_find_column(handle, colhashnext)
      && !cbf_get_integervalue(handle, &ohashnext)) {
  
      cbf_failnez( cbf_compute_hashcode(value, &ohashcode))
      
      if (hashcode != ohashcode)   {

        cbf_failnez( cbf_require_category (handle,   categoryhashtable))
  
        cbf_failnez( cbf_require_column   (handle,   colhashnext))
      	
        cbf_failnez( cbf_select_row       (handle,   ohashcode))

        if ( ! cbf_get_integervalue (handle, &rownum)) {
        
          if (rownum == valuerow) {
          
            cbf_failnez(cbf_set_integervalue(handle,ohashnext))
          	
          } else  {
          
            cbf_failnez( cbf_find_category    (handle,   category))

            cbf_failnez( cbf_find_column      (handle,   colhashnext))

            while ( rownum >=0 && rownum != valuerow)  {
  
              cbf_failnez( cbf_select_row     (handle,   rownum))
    
              orownum = -1;
    
              if (cbf_get_integervalue (handle,&orownum) || orownum <= rownum) {

                break;
      
              } else {
      	
      	        if (orownum == valuerow) {
      	      
      	          cbf_failnez(cbf_set_integervalue(handle,ohashnext))
      	        
      	          break;
      	      	
      	        }
              }
      	
              rownum = orownum;
  	
            }
          	
          }
        	
        }
  
      }
      	
    } 
  	
  }
  

  if ( valuerow < 0 )  {
  	
    cbf_failnez( cbf_new_row          (handle))
    
  } else {
  	
    cbf_failnez( cbf_select_row       (handle, valuerow))

  }

  cbf_failnez( cbf_set_value        (handle,    value))

  cbf_failnez( cbf_row_number       (handle,   (unsigned int *)&nrownum))

  cbf_failnez( cbf_require_column   (handle,   (const char *) colhashnext))

  cbf_failnez( cbf_set_integervalue (handle,   -1))

  cbf_failnez( cbf_require_category (handle,   categoryhashtable))
  
  cbf_failnez( cbf_require_column   (handle,   colhashnext))
  
  cbf_failnez( cbf_count_rows       (handle,   (unsigned int *)&catrownum))
  
  if (catrownum < hashcode+1) {
  
    for (rownum = catrownum; rownum < hashcode+1; rownum++) {
    
      cbf_failnez(cbf_new_row(handle))
    	
    }	
  	
  }

  
  cbf_failnez( cbf_find_column      (handle, colhashnext))

  cbf_failnez( cbf_select_row       (handle, hashcode))

  if ( cbf_get_integervalue (handle, &rownum) || rownum == -1) {

    cbf_failnez( cbf_set_integervalue   (handle, nrownum))
    
    cbf_failnez( cbf_find_category      (handle, category))
      
    cbf_failnez( cbf_find_column        (handle,  colhashnext))
     
    cbf_failnez( cbf_select_row         (handle, nrownum))
        
    cbf_failnez( cbf_set_integervalue   (handle, -1))

    cbf_failnez( cbf_find_column        (handle, columnname))

    return 0;
  
  }
  
  if (nrownum < rownum) {
  
    cbf_failnez( cbf_set_integervalue(handle,nrownum))
  	
  }
  
  cbf_failnez( cbf_find_category    (handle,   category))

  cbf_failnez( cbf_find_column      (handle,   colhashnext))

  if (rownum >= nrownum) {
  
        cbf_failnez( cbf_select_row         (handle, nrownum))

        if (rownum > nrownum) {
        	
          cbf_failnez( cbf_set_integervalue(handle, rownum))
          
        }

        if (cbf_get_integervalue (handle, &orownum)) {
      	
   
          cbf_failnez(cbf_set_integervalue (handle, -1))
        
        }

        cbf_failnez( cbf_find_column        (handle, columnname))

        return 0;
  	
  }
 
  
  while ( rownum >=0 )  {
  
    cbf_failnez( cbf_select_row     (handle,   rownum))
    
    orownum = -1;
    
    if (cbf_get_integervalue (handle,&orownum) || orownum < 0 || orownum >= nrownum) {
          
      cbf_failnez( cbf_set_integervalue   (handle, nrownum))

      cbf_failnez( cbf_select_row         (handle, nrownum))

      if ( orownum < 0  || orownum > nrownum) {
        
        cbf_failnez( cbf_set_integervalue   (handle, orownum))
        
      }
      
      if (cbf_get_integervalue (handle, &orownum)) {
      	
   
        cbf_failnez(cbf_set_integervalue (handle, -1))
        
      }

      cbf_failnez( cbf_find_column        (handle, columnname))

      return 0;
      
    }      
    
    rownum = orownum;
  	
  }

  return CBF_NOTFOUND;

}


  /* Find value in the named column, using the hash table links, if available*/

int cbf_find_hashedvalue(cbf_handle handle, const char * value, 
                                            const char * columnname,
                                            int caseinsensitive) {

  char colhashnext[91];
  
  char * category;
  
  char categoryhashtable[91];

  char hashcodestring[81];
  
  unsigned int hashcode;

  int rownum, catrownum;

  const char * rowvalue;

  int colnamelen, catnamelen;
  
  if (!columnname) return CBF_ARGUMENT;

  if ( (colnamelen = strlen(columnname)) > 80 ) return CBF_ARGUMENT;
  
  cbf_failnez(cbf_category_name (handle, (const char **)&category));
  
  if ( (catnamelen = strlen(category)) > 80 ) return CBF_ARGUMENT;
  

    /* Compute the hashcode value (0-255) */

  cbf_failnez (cbf_compute_hashcode(value, &hashcode))

  sprintf (hashcodestring,"%d",hashcode);
  
    /* Save the category of the primary search */
  
  strcpy (categoryhashtable,category);
  
    /* Compute the names
    
        <category>(hash_table)
        <column>(hash_next)
        
     */
  
  strcpy (categoryhashtable + catnamelen, "(hash_table)");   

  strcpy (colhashnext, columnname);

  strcpy (colhashnext+colnamelen, "(hash_next)");
  
    /* Switch the the hash table and make sure it has enough rows */
  
  cbf_failnez( cbf_require_category (handle, categoryhashtable))
  
  cbf_failnez( cbf_require_column   (handle, colhashnext))
  
  cbf_failnez( cbf_count_rows       (handle, (unsigned int *)&catrownum))
  
  if (catrownum < hashcode+1) {
  
    for (rownum = catrownum; rownum < hashcode+1; rownum++) {
    
      cbf_failnez( cbf_new_row(handle))
    	
    }	
  	
  }
  
    /* examine the row in the hash table given by the hash code
       to see if it points to a row  */

  if ( ! cbf_select_row(handle, hashcode) 
       && !cbf_get_integervalue(handle, (int *) &rownum)
       &&  rownum >= 0 ) {
  
    /* If we have a start point, trace the chain until we find
       a match to the probe, or fail */

  	cbf_failnez( cbf_find_category    (handle,   category))
  	
    while ( rownum >=0 )  {
  
      cbf_failnez( cbf_find_column    (handle,   columnname))

      cbf_failnez( cbf_select_row     (handle,   rownum))
      
      if (caseinsensitive) {
      	
        if ( !cbf_get_value(handle, &rowvalue)  && !cbf_cistrcmp(rowvalue, value)) {
      
          return 0;
          
        }
      }else {
      
        if ( !cbf_get_value(handle, &rowvalue)  && !strcmp(rowvalue, value)) {
      
          return 0;
          
        }
      	
      }
      
      cbf_failnez( cbf_find_column    (handle, colhashnext))
    
      if (cbf_get_integervalue       (handle,&rownum)) break;
            	
    }
    
  }

  cbf_failnez( cbf_find_category      (handle,   category))
  
  cbf_failnez( cbf_find_column        (handle,   columnname))
  
  return CBF_NOTFOUND;

}


int cbf_convert_dictionary_definition(cbf_handle cbfdictionary, cbf_handle dictionary,
                                                                const char * name)
{
    const char *category_id;

    const char *mandatory_code;

    const char *itemname;

    const char *columnname;
    
    const char *categoryname, *ocategoryname;
           
    int colno;

    const char *type_code;

    const char *default_value;

    const char *parent_name;

    const char *child_name;

    const char *alias_name;

    const char *key, *oldkey;
    
    const char *value, *value2, *value_type;
    
    char buffer[255];

    cbf_node * base_node, * local_node;

    int rownum, numrows, numrow;
    
    int nextkeyrow;

    int haveitemname;

    int haveitemcategory;

    haveitemname = haveitemcategory = 0;
    
      /* Save the base data block or save frame to come back to */
    
    base_node = dictionary->node;
    
    local_node = base_node;
    
      /* Find the name for this defintion */

    if (!cbf_find_local_tag(dictionary,"_name") ||
      !cbf_find_local_tag(dictionary,"_item.name") ||
      !cbf_find_local_tag(dictionary,"_definition.id")) {
    	
       haveitemname = 1; local_node = dictionary->node;
       
       cbf_failnez(cbf_column_name(dictionary, &columnname) )
       
    }

    if (!haveitemname && (!cbf_find_category(dictionary,"item") || !cbf_find_category(dictionary,"name"))) haveitemcategory = 1;

    if (haveitemname || haveitemcategory) {

      cbf_failnez( cbf_count_rows (dictionary,(unsigned int *)&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      for (numrow=0; numrow < numrows; numrow++) {

        cbf_failnez( cbf_require_category  (cbfdictionary, "items"))
        
        if (haveitemname  && !cbf_find_column(dictionary,columnname)) {
 
          cbf_failnez( cbf_select_row(dictionary, numrow) )

          cbf_failnez( cbf_get_value(dictionary, &itemname))

        } else {

          itemname = name;

        }

        if (cbf_find_hashedvalue(cbfdictionary, itemname, "name", 
           CBF_CASE_INSENSITIVE)) {

          cbf_failnez( cbf_set_hashedvalue (cbfdictionary, itemname, "name", -1))

        }
        
        cbf_failnez( cbf_row_number          (cbfdictionary, (unsigned int*)&rownum))

        if (!cbf_find_column(dictionary,"category_id") 
          || !cbf_find_column(dictionary,"_category")){

          cbf_failnez( cbf_select_row(dictionary, (unsigned int)numrow) )

          if (!cbf_get_value(dictionary, &category_id)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "category_id"));
            
            if (cbf_get_value(cbfdictionary, &categoryname) 
              || !categoryname 
              || !strcmp(categoryname," "))
              
            cbf_failnez(cbf_set_hashedvalue(cbfdictionary, category_id, "category_id", rownum))

          }

        } else  {
        
          dictionary->node = base_node;
        	
          if (!cbf_find_local_tag(dictionary,"_category")
              ||!cbf_find_local_tag(dictionary,"_item.category_id") ) {
              
            if (!cbf_get_value(dictionary, &category_id)) {

              cbf_failnez( cbf_find_column(cbfdictionary, "category_id"));

              if (cbf_get_value(cbfdictionary, &categoryname) 
                || !categoryname 
                || !strcmp(categoryname," "))
              
              cbf_failnez(cbf_set_hashedvalue(cbfdictionary, category_id, "category_id", rownum))
              
            }

          }
             
        }

        dictionary->node = local_node;

        if (!cbf_find_column(dictionary,"mandatory_code") ) {

          cbf_failnez( cbf_select_row(dictionary, numrow) )

          if (!cbf_get_value(dictionary, &mandatory_code)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "mandatory_code"));

            cbf_failnez( cbf_set_value(cbfdictionary, mandatory_code))

          }

        }


        dictionary->node = base_node;
        	
        if ( !cbf_find_local_tag(dictionary,"_type") ||
          !cbf_find_local_tag(dictionary,"_item_type.code") ||
          !cbf_find_local_tag(dictionary,"_type.contents") ) {

          if (!cbf_get_value(dictionary, &type_code)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "type_code"));

            cbf_failnez( cbf_set_value(cbfdictionary, type_code))

          }

        }  
        

        dictionary->node = base_node;
        	
        if (!cbf_find_local_tag(dictionary,"_enumeration_default") ||
          !cbf_find_local_tag(dictionary,"_item_default.value") )  {

          if (!cbf_get_value(dictionary, &default_value)) {

            cbf_failnez( cbf_find_column(cbfdictionary, "default_value"));

            cbf_failnez( cbf_set_value(cbfdictionary, default_value))

          }

        }


        dictionary->node = base_node;
        	
        if ( !cbf_find_local_tag(dictionary,"_item_aliases.alias_name") )  {

          if (!cbf_get_value(dictionary, &alias_name)) {

            cbf_failnez(cbf_find_category(cbfdictionary, "item_aliases"))

            if (cbf_find_hashedvalue(cbfdictionary, alias_name, "item_alias", 
              CBF_CASE_INSENSITIVE)) {

            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, alias_name, "item_alias", -1))

            }
            
            cbf_failnez( cbf_row_number          (cbfdictionary, (unsigned int *)&rownum))
            
            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, itemname, "item_root", rownum))

          }

        }

      }

    }
    
    
    /* extract enumerations */
    
    dictionary->node = base_node;
    
    value_type = "value";
        	
    if (!cbf_find_local_tag(dictionary, "_item_enumeration.value") ||
      !cbf_find_local_tag(dictionary, "_enumeration"))  {
      
      cbf_failnez( cbf_column_number(dictionary, (unsigned int *)&colno))
      
      cbf_failnez( cbf_count_rows (dictionary, (unsigned int *)&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      cbf_failnez( cbf_require_category  (cbfdictionary, "items_enumerations"))
        
      for (numrow=0; numrow < numrows; numrow++) {
      
        cbf_failnez( cbf_select_row(dictionary, numrow))
        
        cbf_failnez( cbf_select_column(dictionary, colno))

        cbf_failnez( cbf_get_value (dictionary, &value))

        if (!haveitemname)   {
        
          cbf_failnez (cbf_find_column(dictionary,"name"))

          cbf_failnez (cbf_get_value(dictionary,&itemname))
        	
        }

        cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, itemname, "name", -1))

        cbf_failnez( cbf_require_column      (cbfdictionary, "value"))
        
        cbf_failnez( cbf_set_value           (cbfdictionary, value))
        
        cbf_failnez( cbf_require_column      (cbfdictionary, "value_type") )
        
        cbf_failnez( cbf_set_value           (cbfdictionary, value_type))
        
      }
    
    }

    dictionary->node = base_node;
        	
    if (!cbf_find_local_tag(dictionary, "_item_range.minimum") ||
      !cbf_find_local_tag(dictionary, "_enumeration_range"))  {
      
      cbf_failnez( cbf_column_number(dictionary, (unsigned int *)&colno))
      
      cbf_failnez( cbf_count_rows (dictionary, (unsigned int *)&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      cbf_failnez( cbf_require_category    (cbfdictionary, "items_enumerations"))
        
      for (numrow=0; numrow < numrows; numrow++) {
            
        cbf_failnez( cbf_select_row(dictionary, numrow) )
        
        cbf_failnez( cbf_select_column(dictionary, colno))

        cbf_failnez( cbf_get_value (dictionary, &value))
                
        if (!haveitemname)   {
        
          cbf_failnez (cbf_find_column(dictionary,"name"))

          cbf_failnez (cbf_get_value(dictionary,&itemname))
        	
        }

        value_type = "closed_range";

        if (!cbf_find_column(dictionary, "maximum")) {
        
          cbf_failnez( cbf_get_value (dictionary, &value2))
          
          if (value && value2 && strlen(value)+strlen(value2) < 255) {
          
            if (strcmp(value,value2)) value_type = "open_range";
 
            strcpy(buffer,value);
            
            buffer[strlen(value)]=':';
            
            strcpy(buffer+strlen(value)+1,value2);
            
            value = buffer;
            
          	
          } else {
          
            value = "invalid";
            
            sprintf(buffer,"dictionary:  invalid range of values for %s",itemname);
            
            cbf_warning(buffer);
          	
          }
        	
        }


        cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, itemname, "name", -1))

        cbf_failnez( cbf_require_column      (cbfdictionary, "value"))
        
        cbf_failnez( cbf_set_value           (cbfdictionary, value))
        
        cbf_failnez( cbf_require_column      (cbfdictionary, "value_type") )
        
        cbf_failnez( cbf_set_value           (cbfdictionary, value_type))
        
      }


    }



    dictionary->node = base_node;
        	
    if (!cbf_find_local_tag(dictionary, "_item_linked.parent_name") ||
      !cbf_find_local_tag(dictionary, "_item_link_parent") || 
      !cbf_find_local_tag(dictionary, "_category.parent_id"))  {

      cbf_failnez( cbf_count_rows (dictionary,(unsigned int *)&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))

      for (numrow=0; numrow < numrows; numrow++) {

        cbf_failnez( cbf_require_category  (cbfdictionary, "items"))

        cbf_failnez( cbf_select_row(dictionary, numrow) )

        parent_name = NULL;

        if (!cbf_find_column(dictionary,"parent_name") ||
            !cbf_find_column(dictionary,"_list_link_parent") || 
            !cbf_find_column(dictionary,"parent_id") )
            if (cbf_get_value(dictionary,&parent_name)) parent_name = NULL;

        child_name = NULL;

        if (!cbf_find_column(dictionary,"child_name") ||
            !cbf_find_column(dictionary,"_list_link_child") )
            if (cbf_get_value(dictionary,&child_name)) child_name = NULL;

        if ((numrows==1) && (child_name == NULL)) {

          child_name = itemname;

        }

        if (parent_name && child_name) {

          if (cbf_find_hashedvalue(cbfdictionary, child_name, "name", 
            CBF_CASE_INSENSITIVE)) {

            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, child_name, "name", -1))

          }

          cbf_failnez(cbf_find_column(cbfdictionary, "parent"))

          cbf_failnez(cbf_set_value(cbfdictionary,parent_name))

        }

      }

    }

    dictionary->node = base_node;
        	
    if (!cbf_find_local_tag(dictionary,"_category") ||

      !cbf_find_local_tag(dictionary,"_category.id") )  {

      cbf_failnez( cbf_count_rows (dictionary,(unsigned int *)&numrows))

      cbf_failnez( cbf_rewind_row        (dictionary))
      
      local_node = dictionary->node;
      
      for (numrow=0; numrow < numrows; numrow++) {

        cbf_failnez( cbf_require_category  (cbfdictionary, "categories"))
        
        dictionary->node = local_node;

        cbf_failnez( cbf_select_row(dictionary, numrow) )
        
        cbf_failnez( cbf_get_value         (dictionary, &categoryname))
   
        if (cbf_find_hashedvalue(cbfdictionary, categoryname, "id", 
          CBF_CASE_INSENSITIVE)) {

          cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, categoryname, "id", -1))

        } 
        
        key = NULL;
                
        mandatory_code = "no";
        
        if (!cbf_find_column(dictionary,"mandatory_code") 
          && !cbf_get_value(dictionary,&mandatory_code)) {
          
          cbf_failnez(cbf_require_column(cbfdictionary,"mandatory_code"))
          
          cbf_failnez(cbf_set_value(cbfdictionary,mandatory_code))
       
        }


        dictionary->node = base_node;

        if (!cbf_find_local_tag(dictionary,"_list_reference") ||
          !cbf_find_local_tag(dictionary,"_category_key.name") ) {

          if (!cbf_get_value(dictionary, &key) && key) {

            cbf_failnez( cbf_require_column(cbfdictionary, "key"))
            
            while (cbf_get_value(cbfdictionary, &oldkey) || !oldkey || strcmp(oldkey," "))  {
            
              if (key && oldkey && !strcmp(oldkey,key)) break;
              
              cbf_failnez( cbf_find_column         (cbfdictionary, "id(hash_next)"))
              
              cbf_failnez( cbf_get_integervalue    (cbfdictionary, &nextkeyrow))
              
              cbf_failnez( cbf_find_column(cbfdictionary, "key"))

              if (nextkeyrow < 0) {
            
                cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, categoryname, "id", -1))

                cbf_failnez( cbf_find_column         (cbfdictionary, "key"))
              
                break;
 
              }
              
              cbf_failnez( cbf_select_row            (cbfdictionary, (unsigned int)nextkeyrow))
            	
            }
            
            cbf_failnez(cbf_set_value(cbfdictionary,key))

            cbf_failnez(cbf_require_column(cbfdictionary,"mandatory_code"))
            
            cbf_failnez(cbf_set_value(cbfdictionary,mandatory_code))
            
          }
          
        }
        
        cbf_failnez( cbf_require_category  (cbfdictionary, "items"))
        
        cbf_failnez (cbf_require_column(cbfdictionary,"name"))
        
        if (key) {
        	
            if (cbf_find_hashedvalue(cbfdictionary,key,"name",CBF_CASE_INSENSITIVE)) {
        
              cbf_failnez(cbf_set_hashedvalue(cbfdictionary,key,"name", -1))
          
              cbf_failnez(cbf_require_column(cbfdictionary,"mandatory_code"))
          
              cbf_failnez(cbf_set_value(cbfdictionary,"yes"))
            
            } else {
        
              cbf_failnez(cbf_require_column(cbfdictionary,"mandatory_code"))
          
              if (cbf_get_value(cbfdictionary,&mandatory_code) || !mandatory_code) {
          
                 cbf_failnez(cbf_set_value(cbfdictionary,"yes"))
          	
              }
              
            }
            
            cbf_failnez(cbf_row_number(cbfdictionary, (unsigned int *)&rownum))
            
            cbf_failnez(cbf_require_column(cbfdictionary,"category_id"))
              
              if (cbf_get_value(cbfdictionary, &ocategoryname) 
                || !ocategoryname 
                || !strcmp(ocategoryname," "))
              
                cbf_failnez(cbf_set_hashedvalue(cbfdictionary, categoryname, "category_id", rownum))
          
        }
        
        dictionary->node = base_node;
        	
        if (!cbf_find_local_tag(dictionary,"_category_aliases.alias_name") )  {

          if (!cbf_get_value(dictionary, &alias_name)) {

            cbf_failnez(cbf_find_category(cbfdictionary, "category_aliases"))

            if (cbf_find_hashedvalue(cbfdictionary, alias_name, "category_alias", 
              CBF_CASE_INSENSITIVE)) {

            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, alias_name, "category_alias", -1))

            }
            
            cbf_failnez( cbf_row_number          (cbfdictionary, (unsigned int *)&rownum))
            
            cbf_failnez( cbf_set_hashedvalue     (cbfdictionary, categoryname, "category_root", rownum))

          }

        }

      }

    }

    return 0;
}

  /* Increment a column */

int cbf_increment_column( cbf_handle handle, const char* columnname, int * count ) {

  cbf_failnez(cbf_find_column(handle, columnname))
  
  if (!cbf_get_integervalue(handle, count)) {
  
    (*count)++;
  
    return cbf_set_integervalue(handle, *count);
  	
  }
  
  *count = 1;
  
  return cbf_set_integervalue(handle, 1);  
	
}

  /* Reset a column */

int cbf_reset_column( cbf_handle handle, const char* columnname) {

  if (!cbf_find_column(handle, columnname )) {
  
    cbf_failnez( cbf_remove_column(handle))
  	
  }
  
  return cbf_new_column( handle, columnname);
	
}


  /* Reset reference counts for a dictionary */
  
int cbf_reset_refcounts( cbf_handle dictionary ) {

  if ( dictionary && !cbf_find_tag(dictionary,"_items.name"))  {
  
    cbf_failnez(cbf_reset_column(dictionary, "CBF_wide_refcounts") )
  
    cbf_failnez(cbf_reset_column(dictionary, "DB_wide_refcounts") )
  
    cbf_failnez(cbf_reset_column(dictionary, "DBcat_wide_refcounts") )
  	
    cbf_failnez(cbf_reset_column(dictionary, "SF_wide_refcounts") )

    cbf_failnez(cbf_reset_column(dictionary, "SFcat_wide_refcounts") )
  }
  
  return 0;
	
}

  /* Convert a DDL1 or DDL2 dictionary and add it to a CBF dictionary */


int cbf_convert_dictionary (cbf_handle handle, cbf_handle dictionary )
{
    cbf_handle dict;

    unsigned int blocks, frames, blockitems;

    int blocknum, itemnum;
    
    unsigned int numrows, rownum, parent_row;

    CBF_NODETYPE itemtype;

    const char *datablock_name;

    const char *saveframe_name;
    
    const char *parent_name, *child_name;
    
    const char *type_code, *otype_code;
    
    char buffer[255];

    if (!handle || !dictionary ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dict))

    cbf_failnez( cbf_require_datablock  (dict, "cbf_dictionary"))
    

    cbf_failnez( cbf_require_category   (dict, "category_aliases(hash_table)"))
    
      cbf_failnez( cbf_require_column   (dict, "category_root(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "category_alias(hash_next)"))


    cbf_failnez( cbf_require_category   (dict, "category_aliases"))

      cbf_failnez( cbf_require_column   (dict, "category_root"))

      cbf_failnez( cbf_require_column   (dict, "category_alias"))

      cbf_failnez( cbf_require_column   (dict, "category_root(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "category_alias(hash_next)"))


    cbf_failnez( cbf_require_category   (dict, "item_aliases(hash_table)"))

      cbf_failnez( cbf_require_column   (dict, "item_root(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "item_alias(hash_next)"))


    cbf_failnez( cbf_require_category   (dict, "item_aliases"))

      cbf_failnez( cbf_require_column   (dict, "item_root"))

      cbf_failnez( cbf_require_column   (dict, "item_alias"))

      cbf_failnez( cbf_require_column   (dict, "item_root(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "item_alias(hash_next)"))


    cbf_failnez( cbf_require_category   (dict, "categories(hash_table)"))

      cbf_failnez( cbf_require_column   (dict, "id(hash_next)"))


    cbf_failnez( cbf_require_category   (dict, "categories"))

      cbf_failnez( cbf_require_column   (dict, "id"))

      cbf_failnez( cbf_require_column   (dict, "id(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "key"))


    cbf_failnez( cbf_require_category   (dict, "items(hash_table)"))

      cbf_failnez( cbf_require_column   (dict, "name(hash_next)"))
      
      cbf_failnez( cbf_require_column   (dict, "category_id(hash_next)"))


    cbf_failnez( cbf_require_category   (dict, "items"))

      cbf_failnez( cbf_require_column   (dict, "name"))

      cbf_failnez( cbf_require_column   (dict, "name(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "type_code"))

      cbf_failnez( cbf_require_column   (dict, "category_id"))

      cbf_failnez( cbf_require_column   (dict, "category_id(hash_next)"))

      cbf_failnez( cbf_require_column   (dict, "sub_category_id"))

      cbf_failnez( cbf_require_column   (dict, "mandatory_code"))

      cbf_failnez( cbf_require_column   (dict, "default_value"))

      cbf_failnez( cbf_require_column   (dict, "parent"))
      
 
    cbf_failnez( cbf_require_category   (dict, "items_enumerations(hash_table)"))

      cbf_failnez( cbf_require_column   (dict, "name(hash_next)"))
   

    cbf_failnez( cbf_require_category   (dict, "items_enumerations"))

      cbf_failnez( cbf_require_column   (dict, "name"))

      cbf_failnez( cbf_require_column   (dict, "name(hash_next)"))
      
      cbf_failnez( cbf_require_column   (dict, "value"))

      cbf_failnez( cbf_require_column   (dict, "value_type"))
           

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
    
    /* Update unfilled-in items for children */
    
    if( !cbf_find_tag(dict,"_items.parent")) {
    
      cbf_failnez(cbf_count_rows(dict,&numrows))
      
      for (rownum = 0; rownum < numrows; rownum++)  {
      
        cbf_failnez(cbf_find_column(dict,"parent"))
        
        if (!cbf_select_row(dict,rownum)) {
        
          if (!cbf_get_value(dict,&parent_name) && parent_name) {
          
            if (!cbf_find_hashedvalue(dict, parent_name, "name", CBF_CASE_INSENSITIVE)) {
            
              cbf_failnez(cbf_row_number(dict,&parent_row))
              
              cbf_failnez(cbf_find_column(dict,"type_code"))
              
              if (!cbf_get_value(dict,&type_code) && type_code) {
              
                cbf_failnez(cbf_select_row(dict,rownum))
                
                if (cbf_get_value(dict,&otype_code)) otype_code = NULL;
                
                cbf_failnez(cbf_set_value(dict,type_code))
                
                if (otype_code && !cbf_cistrcmp(otype_code, type_code)) {
                
                  cbf_failnez(cbf_find_column(dict,"name"))
                  
                  if (!cbf_get_value(dict,&child_name)) {
                  	
                    sprintf(buffer," inconsistent data type %s for %s", otype_code, child_name);
                    
                  }
                	
                }
              	
              }
            	
            }
          	
          }
        	
        }
      	
      }
          
    	
    }

    if (getenv("CBFLIB_DEBUG") ) cbf_failnez(cbf_write_file(dict,stderr,0,0,0,0))

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

  columnname[0] = '_';

  if (collen) strncpy(columnname+(catlen?0:1),colstart+1,collen);

  columnname[collen+(catlen?0:1)] = '\0';

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
  
  columnname[0] = '_';

  if (collen) strncpy(columnname+(catlen?0:1),colstart+1,collen);

  columnname[collen+(catlen?0:1)] = '\0';

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

    if (((!(node->name) || (node->name[0] == '_')) &&(categoryname[0]=='\0'))
       || ((node->name)&&!cbf_cistrcmp(node->name,categoryname))) {

      cbf_failnez (cbf_find_child(&node,node,columnname))

      handle->node = node;
      
      handle->row = 0;

      handle->search_row = 0;

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
    
    const char * tempcatname;

    if (!handle || !categoryname || !categoryroot ) return CBF_ARGUMENT;

    dictionary = (cbf_handle) handle->dictionary;

    if (!dictionary) return CBF_NOTFOUND;
    
    if (categoryname[0] == '_') {
    
      if (!cbf_find_tag(dictionary,"_items.name") 
        && !cbf_find_hashedvalue(dictionary,categoryname,"name",CBF_CASE_INSENSITIVE)
        && !cbf_find_column(dictionary,"category_id")
        && !cbf_get_value(dictionary,&tempcatname)
        && tempcatname)  categoryname = tempcatname;
        
        else return CBF_NOTFOUND;
    	
    }

    cbf_failnez( cbf_find_tag(dictionary, "_category_aliases.alias_id"))

    cbf_failnez( cbf_rewind_row(dictionary))

    cbf_failnez( cbf_find_hashedvalue(dictionary, categoryname,"alias_id",CBF_CASE_INSENSITIVE))

    cbf_failnez( cbf_find_column(dictionary, "root_id"))

    return cbf_get_value(dictionary,categoryroot);

}

  /* Find the root alias of a given category, defaulting to the current one */

int cbf_require_category_root (cbf_handle handle, const char* categoryname,
                                            const char** categoryroot)
{
    cbf_handle dictionary;
        
    const char * tempcatname;

    if (!handle || !categoryname || !categoryroot ) return CBF_ARGUMENT;

    dictionary = (cbf_handle) handle->dictionary;

    if (categoryname[0] == '_') {
    
      if (!cbf_find_tag(dictionary,"_items.name") 
        && !cbf_find_hashedvalue(dictionary,categoryname,"name",CBF_CASE_INSENSITIVE)
        && !cbf_find_column(dictionary,"category_id")
        && !cbf_get_value(dictionary,&tempcatname)
        && tempcatname)  categoryname = tempcatname;
        
      else {
        
        * categoryroot = categoryname;
          
        return 0;
        	
      }
    	
    }

    if (cbf_find_category_root(handle,categoryname,categoryroot))

      * categoryroot = categoryname;

    return 0;

}

  /* Set the root alias of a given category */

int cbf_set_category_root (cbf_handle handle, const char* categoryname,
                                            const char* categoryroot)
{
    cbf_handle dictionary;

    unsigned int rownum;

    if (!handle || !categoryname || !categoryroot ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dictionary))

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_category_aliases.alias_id")) {

        cbf_failnez( cbf_require_datablock(dictionary, "dictionary"))

        cbf_failnez( cbf_require_category(dictionary, "category_aliases"))

        cbf_failnez( cbf_require_column(dictionary, "alias_id"))
    }

    if (cbf_find_hashedvalue(dictionary, categoryname, "alias_id", 
              CBF_CASE_INSENSITIVE)) {

        cbf_failnez( cbf_set_hashedvalue(dictionary, categoryname, "alias_id", -1))

    }
    
    cbf_failnez( cbf_row_number(dictionary, &rownum))
            
    cbf_failnez( cbf_set_hashedvalue(dictionary, categoryroot, "root_id", rownum))

    return 0;
}

  /* Find the root alias of a given tag */

int cbf_find_tag_root (cbf_handle handle, const char* tagname,
                                            const char** tagroot)
{
    cbf_handle dictionary;

    if (!handle || !tagname || !tagroot ) return CBF_ARGUMENT;

    dictionary = (cbf_handle) handle->dictionary;

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_item_aliases.alias_name") && cbf_find_tag(dictionary, "_aliases.definition_id"))
      {
        return CBF_NOTFOUND;
      }

    cbf_failnez( cbf_find_hashedvalue(dictionary,tagname,"alias_name",
      CBF_CASE_INSENSITIVE))
      
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

    unsigned int rownum;

    if (!handle || !tagname || !tagroot ) return CBF_ARGUMENT;

    cbf_failnez( cbf_require_dictionary(handle, &dictionary))

    if (!dictionary) return CBF_NOTFOUND;

    if ( cbf_find_tag(dictionary, "_item_aliases.alias_name")) {

        cbf_failnez( cbf_require_datablock(dictionary, "dictionary"))

        cbf_failnez( cbf_require_category(dictionary, "item_aliases"))

        cbf_failnez( cbf_require_column(dictionary, "alias_name"))
    }

    if (cbf_find_hashedvalue(dictionary, tagname, "alias_name", 
              CBF_CASE_INSENSITIVE)) {

        cbf_failnez( cbf_set_hashedvalue(dictionary, tagname, "alias_name", -1))

    }
    
    cbf_failnez( cbf_row_number(dictionary, &rownum))
            
    cbf_failnez( cbf_set_hashedvalue(dictionary, tagroot, "root_name", rownum))

    return 0;

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

    cbf_failnez( cbf_find_column(handle, "name"))

    while (!cbf_find_nextrow(dictionary,tagname)) {

        cbf_failnez( cbf_require_column(dictionary, "category_id"))

        if (!cbf_get_value(dictionary,(const char **)&tempcat)) {

          if (tempcat && !cbf_cistrcmp(tempcat,categoryname))return 0;

        }

	    if (!tempcat) return cbf_set_value(dictionary,categoryname);

        cbf_failnez( cbf_find_column(dictionary, "name"))

    }

    cbf_failnez( cbf_new_row(dictionary))

    cbf_failnez( cbf_set_value(dictionary,tagname))

    cbf_failnez( cbf_find_column(dictionary, "category_id"))

    return cbf_set_value(dictionary,categoryname);

}

  /* check a category for all required tags and for parent tags */

int cbf_check_category_tags(cbf_handle handle, cbf_node* category, cbf_node* parent) {

  int rownum;
  
  long refcount;
  
  char *endptr;
  
  char buffer[512];
  
  const char* refcount_column, *mandatory_code, *item_name, 
    *category_id, *parent_name, *refcountval, *block_name;
  
  if (parent->type == CBF_SAVEFRAME) refcount_column = "SF_wide_refcounts";
  
  else refcount_column = "DB_wide_refcounts";
  
  block_name = parent->name?parent->name:"(null)";

  if (handle->dictionary && category->name && category->name[0]) {
  
    if (getenv("CBFLIB_DEBUG")) cbf_write_file(handle->dictionary, stderr, 0, 0, 0, 0);
  
    if (!cbf_find_tag(handle->dictionary,"_items.name") &&   
      !cbf_find_hashedvalue(handle->dictionary,category->name,"category_id",
        CBF_CASE_INSENSITIVE) ) {
        
      cbf_failnez(cbf_row_number(handle->dictionary,(unsigned int *)&rownum))
      
      do {
      
        cbf_failnez(cbf_select_row(handle->dictionary,rownum))
      
       	cbf_failnez(cbf_find_column(handle->dictionary,"name"))
        	   
        cbf_failnez(cbf_get_value(handle->dictionary,&item_name))
        
        if (!item_name) item_name = "(null)";
        
        if (!cbf_find_column(handle->dictionary,"category_id")
          && !cbf_get_value(handle->dictionary, &category_id)
          && category_id
          && !cbf_cistrcmp(category_id, category->name)) {
          
          refcount = 0;
          
          if(!cbf_find_column(handle->dictionary,refcount_column)
        	&& !cbf_get_value(handle->dictionary, &refcountval)
        	&& refcountval)  {
          	
            refcount = strtol(refcountval,&endptr,10);
         
          }

      
          if (!cbf_find_column(handle->dictionary,"mandatory_code")
            && !cbf_get_value(handle->dictionary, &mandatory_code)
            && mandatory_code
            && !cbf_cistrcmp(mandatory_code,"yes")) {

            if( refcount <= 0) {
        	   
         	  sprintf(buffer, "required tag %s in %s not given", 
        	    item_name, block_name);
        	   
        	  cbf_log(handle,buffer, CBF_LOGWARNING|CBF_LOGWOLINE);
        	  
        	}
        	 	
          }
          
          if (refcount > 0) {
          
            if (!cbf_find_column(handle->dictionary,"parent")
              && !cbf_get_value(handle->dictionary, &parent_name)
              && parent_name
              && !cbf_find_hashedvalue(handle->dictionary,parent_name,"name",
                CBF_CASE_INSENSITIVE)
              && !cbf_find_column(handle->dictionary,refcount_column)
              && (cbf_get_value(handle->dictionary, &refcountval)
        	      || !refcountval || strtol(refcountval,&endptr,10) <=0))  {
                      	   
        	  sprintf(buffer, "required parent tag %s for %s in %s not given", 
        	    parent_name?parent_name:"(null)",
        	    item_name, block_name);
        	   
        	  cbf_log(handle,buffer, CBF_LOGWARNING|CBF_LOGWOLINE);
        	          	  

            }
          	
          }
          
        }
        
        cbf_failnez(cbf_select_row(handle->dictionary, rownum))
        
        cbf_failnez(cbf_find_column(handle->dictionary, "category_id(hash_next)"))
        
        if (cbf_get_integervalue(handle->dictionary, &rownum)) rownum = 1;
      	
      } while (rownum >= 0);
        
    }
  	
  }
  
  return 0;
	
}

  /* Validate portion of CBF */
 
int cbf_validate (cbf_handle handle, cbf_node * node, CBF_NODETYPE type, cbf_node * auxnode) {

  cbf_node * tnode;
  
  cbf_node * colnode;
  
  cbf_node * ttnode;
  
  const char * dictype;
  
  const char * catname, * catroot;
  
  const char * diccat, * diccatroot;
  
  const char * loopname;

  unsigned int children, columns, rows;
  
  cbf_file * file;
  
  char buffer[255];
  
  char itemname[82];
    
  int lcolumn=0, litemname=0;
  
  int count;
  
  int column, minrows, maxrows;
  
  file = handle->file;

  if ( type == CBF_ROOT ) {
  
    /* we come here at the end of the parse
    
       'node' points to the cbf up to the point prior to
       the end of the parse, so that at the beginning, it
       is the ROOT, but after that is at a lower level,
       somewhere within a CBF_DATABLOCK node, if we have
       has any data.
       We need to check any pending category, save frame or 
       data block.  We do this recursively.
       
       If there is a dictionary, we need to scan the
       CBF checking all parent-child relationships.
       
       This is done in the data block scan.
       
       Code is needed to report the cases where the
       relationships are satisfied across data blocks.
       
       Code is needed to insert category names for
       DDL1 tags.
                    
       */
  cbf_failnez(cbf_validate(handle, node, CBF_DATABLOCK, NULL))
      
  	
  } else if ( type == CBF_DATABLOCK ) {
  
    /* we come here at the start of a new datablock
       or at the end of the parse.
       
       'node' points to the cbf up to the point prior to
       the new datablock, so that at the beginning, it
       is the ROOT, but after that is at a lower level,
       somewhere within a CBF_DATABLOCK node
       
       We need to check:
       
         1.  Does the prior data block have any content
         2.  If there is a dictionary, we need to check
              2.1.  If a tag is given within the
                    prior datablock, then the parent
                    of that tag is given in the same
                    data block
              2.2.  For each category in the prior datablock
                    that each mandatory tag for that
                    category is given, and that for
                    each implicit tag that is not
                    explicitly given, that the parent,
                    if any, of that tag is given in the
                    same data block
                    
    */


    /* First validate the last category before the termination */
    
      cbf_failnez(cbf_validate(handle, node, CBF_CATEGORY, NULL))

    /* Now check if the parent data block has any content */

    if (!cbf_find_parent(&tnode, node, CBF_DATABLOCK)) {
    
      cbf_failnez(cbf_count_children(&children, tnode))
    
      if ( children == 0 ) {
      
        if (file != (cbf_file *)NULL) {
        
          if ((tnode->name) != (char *)NULL)  {

            sprintf(buffer,
              "data block %s ends with no content",tnode->name);
              

          } else  {

            sprintf(buffer,
              "data block (null) ends with no content");
              
          }
          
          cbf_log (handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);
              
        } else {
        
          return CBF_FORMAT;
        	
        }
      } else {
      
        /* We have content, now check each category for required tags */
        
        if (handle->dictionary) {
        
          cbf_node *child_node;
          
          unsigned int child;
          
          for (child = 0; child < children; child++) {
          
            cbf_failnez (cbf_get_child (&child_node, tnode, child))
          
            if (child_node->type == CBF_CATEGORY) {
            
              cbf_failnez(cbf_check_category_tags(handle, child_node, tnode))
            	
            } else  if (child_node->type == CBF_SAVEFRAME) {
                      
              cbf_node *sfchild_node;
          
              unsigned int sfchild, sfchildren;
              
              cbf_failnez(cbf_count_children(&sfchildren, child_node))
              
              for (sfchild = 0; sfchild < sfchildren; sfchild++) {
          
                cbf_failnez (cbf_get_child (&sfchild_node, child_node, sfchild))
          
                if (sfchild_node->type == CBF_CATEGORY) {
                
                  cbf_failnez(cbf_check_category_tags(handle, sfchild_node, child_node))
                	
                }

              }
            	
            }
          	
          }
        	
        }
      	
      }
    
    }
    
    if (handle->dictionary) {
              	
      if (!cbf_find_tag(handle->dictionary, "_items.name") || !cbf_find_tag(handle->dictionary, "_definition.id"))  {
        
        cbf_failnez(cbf_reset_column(handle->dictionary, "DB_wide_refcounts") )
  
        cbf_failnez(cbf_reset_column(handle->dictionary, "DBcat_wide_refcounts") )
  	
        cbf_failnez(cbf_reset_column(handle->dictionary, "SF_wide_refcounts") )

        cbf_failnez(cbf_reset_column(handle->dictionary, "SFcat_wide_refcounts") )
        	
      }
    }

  } else if (type == CBF_SAVEFRAME) {

    /* we come here at the end of a save frame
       
       'node' points to the cbf up to the point prior to
       the save frame end.  
       
         1.  Does the save frame have any content
         2.  If there is a dictionary, we need to check
              2.1.  If a tag is given within the
                    save frame, then the parent
                    of that tag is given in the same
                    save frame
              2.2.  For each category in the save frame
                    that each mandatory tag for that
                    category is given, and that for
                    each implicit tag that is not
                    explicitly given, that the parent,
                    if any, of that tag is given in the
                    same data block
         3.  We need to reset the counters for the save
             frame.
                    
    */

    /* Now check if the save frame has any content */

    if (!cbf_find_parent(&tnode, node, CBF_SAVEFRAME)) {
    
      cbf_failnez(cbf_count_children(&children, tnode))
    
      if ( children == 0 ) {
      
        if (file != (cbf_file *)NULL) {
        
          if ((tnode->name) != (char *)NULL)  {

            sprintf(buffer,
              "save frame %s ends with no content",tnode->name);
              

          } else  {

            sprintf(buffer,
              "save frame (null) ends with no content");
              
          }
          
          cbf_log (handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);
              
        } else {
        
          return CBF_FORMAT;
        	
        }
      } else {
      
        /* We have content, now check each category for required tags */
        
        if (handle->dictionary) {
        
          cbf_node *child_node;
          
          unsigned int child;
          
          for (child = 0; child < children; child++) {
          
            cbf_failnez (cbf_get_child (&child_node, tnode, child))
          
            if (child_node->type == CBF_CATEGORY) {
            
              cbf_failnez(cbf_check_category_tags(handle, child_node, tnode))
            	
            }
          	
          }
        	
        }
      	
      }
    
    }

    if (handle->dictionary) {

      if (!cbf_find_tag(handle->dictionary, "_items.name") || !cbf_find_tag(handle->dictionary, "_definition.id"))  {
  	
        cbf_failnez(cbf_reset_column(handle->dictionary, "SF_wide_refcounts") )

        cbf_failnez(cbf_reset_column(handle->dictionary, "SFcat_wide_refcounts") )
        	
      }
    }

  } else if (type == CBF_CATEGORY ) {
  
    /* We come here at the start of a new datablock element
       or save frame element in the form of an assignment, a 
       loop assignment or (if a data block) a save frame.  
       Alternatively, we may come here at the start
       of a new save frame element.
       
       'node' will be pointing to
          the data block
          the save frame
          or to a node somehere within the prior category
          
        In the third case, we need to check
        
           1.  If there are any columns at all within the
               prior category
           2.  If the columns in the prior category are all 
                the same length
           3.  If there is a dictionary ...
                
                
     */

    /* Find the category node */

    if (!cbf_find_parent (&tnode, node, CBF_CATEGORY)) {
    
    catname = tnode->name;

    if (!cbf_count_children (&columns, tnode)) {
    
      if (columns == 0) cbf_log(handle,"no columns in category",CBF_LOGWARNING|CBF_LOGSTARTLOC);
      
      else {
      
        maxrows = minrows = 0;
        
        for (column = 0; column < columns; column++) {
        
          rows = 0;
        
          if ( !cbf_get_child(&ttnode,tnode, column) ) {
          
            if ( !cbf_count_children (&rows, ttnode)) {
            
              if (column == 0) {
              
                maxrows = minrows = rows;
              	
              }
              
              if (rows > maxrows) maxrows = rows;
              
              if (rows < minrows) minrows = rows;
            	
            }
          	
          }
        	
        }
        
        if ( maxrows != minrows ) {
        
          sprintf(buffer, "incomplete row in category %s", (tnode->name)?(tnode->name):"(null)");
        	
          cbf_log(handle,buffer,CBF_LOGWARNING|CBF_LOGSTARTLOC);
        }
      
        if ( maxrows == 0 ) {
        
          sprintf(buffer, "no rows in category %s", (tnode->name)?(tnode->name):"(null)");
        	
          cbf_log(handle,buffer,CBF_LOGWARNING|CBF_LOGSTARTLOC);
        }  
      	
      }
    	
    }
    
    } else {
    
      if (!cbf_find_parent (&tnode, node, CBF_SAVEFRAME)) {
      
        if (!cbf_count_children (&columns, tnode)) {
    
          if (columns == 0) cbf_log(handle,"no categories in save frame",CBF_LOGWARNING|CBF_LOGSTARTLOC);
        }

      }
    }
    
    if (handle->dictionary) {
              	
      if (!cbf_find_tag(handle->dictionary, "_items.name") || !cbf_find_tag(handle->dictionary, "_definition.id"))  {
        
        cbf_failnez(cbf_reset_column(handle->dictionary, "DBcat_wide_refcounts") )
  	
        cbf_failnez(cbf_reset_column(handle->dictionary, "SFcat_wide_refcounts") )
        	
      }
    }

  } else if (type == CBF_COLUMN) {
  
    if (!cbf_find_parent(&tnode, node, CBF_CATEGORY)) {
    
      lcolumn = 0;
    
      if (node->name) lcolumn = strlen(node->name);
    
      if (!tnode->name|| !(tnode->name[0]) ||
        !cbf_cistrcmp("(none)",tnode->name) ||
         (node->name && node->name[0]=='_') ) {
      	
        litemname = lcolumn;

        if (litemname > 75) cbf_log(handle, 
          "item name longer than 75 characters", 
          CBF_LOGWARNING|CBF_LOGSTARTLOC);   	

      } else {
      
        litemname = 1 + strlen(tnode->name) + 1 + lcolumn;
        
        if (litemname > 75) cbf_log(handle, 
          "category name + column name longer than 75 characters", 
          CBF_LOGWARNING|CBF_LOGSTARTLOC);	

         	
      }
      
      if (tnode->name && auxnode->name) {
      
        sprintf(buffer,"item category name %s inconsistent with category %s",
          tnode->name, auxnode->name);

        if (cbf_cistrcmp(tnode->name,auxnode->name))  cbf_log(handle,
          buffer,CBF_LOGWARNING|CBF_LOGSTARTLOC);
      	
      }
      
      if (handle->dictionary)  {
      
        loopname = NULL;

        if ((!cbf_find_tag(handle->dictionary, "_items.name") || !cbf_find_tag(handle->dictionary, "_definition.id"))
        
          && !cbf_compose_itemname(handle, node, itemname, 80)) {
        	
          if (cbf_find_hashedvalue(handle->dictionary, itemname,
            "name",CBF_CASE_INSENSITIVE) ) {
          
            sprintf(buffer,"item name %s not found in the dictionary",itemname);
          
            cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);
        	
          } else {
                      
            if (auxnode->name && auxnode->name[0]) {
            
              loopname = auxnode->name;
            	
            } else  {
            
              cbf_failnez (cbf_get_child (&colnode, auxnode, 0))
              
              loopname = colnode->name;            
              
            }
            
            if (loopname && loopname[0] == '_')  {

              if (!cbf_find_hashedvalue(handle->dictionary, loopname,
                "name",CBF_CASE_INSENSITIVE) 
                && ! cbf_find_column(handle->dictionary, "category_id")) {

                   cbf_get_value(handle->dictionary, &loopname); 

              }
            	
            }
        
            cbf_failnez(cbf_find_hashedvalue(handle->dictionary, itemname,
              "name",CBF_CASE_INSENSITIVE))
                      	
            if (!cbf_find_column(handle->dictionary, "category_id") 
              && ! cbf_get_value(handle->dictionary, &diccat) 
              && diccat  && loopname
              && ! cbf_require_category_root(handle->dictionary, diccat, &diccatroot)
              && ! cbf_require_category_root(handle->dictionary, loopname, &catroot)) {
              
              if (cbf_cistrcmp(diccatroot,catroot))  {
              
                sprintf(buffer,"dictionary item %s, category name %s inconsistent with %s" ,
                  itemname, diccatroot, catroot);

                cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);
        	
              	
              }                
            	
            }
            
            cbf_failnez(cbf_increment_column(handle->dictionary, "CBF_wide_refcounts", &count )) 

            if (!cbf_find_parent(&ttnode,tnode,CBF_SAVEFRAME)) {

	      int count, countcat;

              cbf_failnez(cbf_increment_column(handle->dictionary, "SF_wide_refcounts", &count ))

              cbf_failnez(cbf_increment_column(handle->dictionary, "SFcat_wide_refcounts", &countcat ))

              if (count > 1 && countcat <= 1) {
              	
                sprintf(buffer,"item name %s appears more than once in a save frame, count %d",
                  itemname, count );

                cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);

              }
              if (countcat > 1 ) {
              	
                sprintf(buffer,"item name %s appears more than once in a save frame category, count %d",
                
                  itemname, countcat);

                cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);
 
              }

            } else {

              int count, countcat;

              cbf_failnez(cbf_increment_column(handle->dictionary, "DB_wide_refcounts", &count ))
            
              cbf_failnez(cbf_increment_column(handle->dictionary, "DBcat_wide_refcounts", &countcat ))

              if (count > 1 && countcat <= 1 ) {
              	
                sprintf(buffer,"item name %s appears more than once in a data block, count %d",

                  itemname, count);

                cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);

              }

              if (countcat > 1 ) {
              	
                sprintf(buffer,"item name %s appears more than once in a data block category, count %d",
                
                  itemname, countcat);

                cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);
 
              }
              	
            }

        	
          }
        
        }
      	
      }
      
    }
  	

  } else if (type == CBF_VALUE) {
  
    /*
    
    
    */
  
    int tokentype;
    
    char * valuestring;
    
    char fline[2049];

    char * flptr;
    	     
    int goodmatch;
    
    long ltest;

    double dtest;

    char * endptr;
    
    char loval[255], hival[255];
    	                    
    char * colonpos;
    
    long symop, xlate;
    
    long yyyy, mm, dd, hr, mi, se, sf, tz;

    valuestring = ((char *)node)+1;
    
    tokentype = (((char *)node)[0]);



    if (handle->dictionary && (tnode = cbf_get_link(auxnode)) && (tnode->name) ){
    
        if (!cbf_compose_itemname(handle, tnode, itemname, 80)) {
    
          if (!cbf_find_tag(handle->dictionary, "_items.name") || !cbf_find_tag(handle->dictionary, "_definition.id")) {
   	            	     

    	    if (!cbf_find_hashedvalue(handle->dictionary, itemname, 
    	      "name",CBF_CASE_INSENSITIVE)) {
    	
    	      if (!cbf_find_column(handle->dictionary, "type_code") &&
    	        !cbf_get_value(handle->dictionary, &dictype)) {
     	        
    	        
    	        goodmatch = 0;
    	       	                      
    	               
    	        if (tokentype==CBF_TOKEN_SCSTRING) {
    	        
    	          if (valuestring[0]=='\\') {
    	          
    	            flptr = valuestring+1;
    	            
    	            if (cbf_foldtextline((const char **)&flptr, fline, 2048, 1, 0, ';'))  {
    	            
    	              tokentype = CBF_TOKEN_SQSTRING;
    	              
    	              valuestring = fline;
    	            	
    	            }
    	          	
    	          }
    	        	
    	        }
    	        
    	        if (tokentype==CBF_TOKEN_TSQSTRING
    	          || tokentype==CBF_TOKEN_TDQSTRING
    	          || tokentype==CBF_TOKEN_PRNSTRING
    	          || tokentype==CBF_TOKEN_BRCSTRING
    	          || tokentype==CBF_TOKEN_BKTSTRING
    	          ) {
    	        
    	          if (valuestring[0]=='\\') {
    	          
    	            flptr = valuestring+1;
    	            
    	            if (cbf_foldtextline((const char **)&flptr, fline, 2048, 1, 0, '\0'))  {
    	            
    	              tokentype = CBF_TOKEN_SQSTRING;
    	              
    	              valuestring = fline;
    	            	
    	            }
    	          	
    	          }
    	        	
    	        }
    	        
    	        
                if (tokentype==CBF_TOKEN_SQSTRING || tokentype== CBF_TOKEN_DQSTRING) {
                
                  if (strchr(valuestring,'\n')) tokentype=CBF_TOKEN_SCSTRING;
                  else if(!strchr(valuestring,' ')
                    &&    !strchr(valuestring,'\t')
                    &&    !strchr(valuestring,'\v')
                    &&    !strchr(valuestring,'\f')
                    &&    !strchr(valuestring,'\r')) tokentype = CBF_TOKEN_WORD;
                	
                }

    	        
    	        switch (tokentype) {
    	        
    	        	case 0:

    	        	case CBF_TOKEN_NULL:

    	        	  goodmatch = 1;

    	        	  break;
    	        	  
    	        	case CBF_TOKEN_WORD:
    	        	
    	        	  if ( !cbf_cistrncmp(dictype,"implied",8) ) {
    	        	  
    	        	    goodmatch = 1;
    	        	    
    	        	    break;
    	        	  	
    	        	  }
 
     	        	  if ( !cbf_cistrncmp(dictype,"uchar3",7) )
     	        	  {
     	        	  	if (strlen(valuestring)==3 
     	        	  	  || (strlen(valuestring)==4 && *(valuestring)=='+'))
     	        	  	  
     	        	  	  goodmatch = 1;
     	        	  	
     	        	  	break;
     	        	  	
     	        	  }


     	        	  if ( !cbf_cistrncmp(dictype,"uchar1",7) ) {
     	        	  	if (strlen(valuestring)==1 
     	        	  	  || (strlen(valuestring)==2 && *(valuestring)=='+'))
     	        	  	  
     	        	  	  goodmatch = 1;
     	        	  	
     	        	  	break;
     	        	  	
     	        	  }

     	        	  if ( !cbf_cistrncmp(dictype,"symo",4) ) {
     	        	         	        	    
     	        	    symop = strtol(valuestring, &endptr, 10);
     	        	    
     	                xlate = 0;
     	        	    
     	        	    if ( *endptr=='_') xlate = strtol(endptr+1, &endptr, 10);
     	        	    
     	        	    if ( *endptr=='\0' 
     	        	      && symop >=1 
     	        	      && symop <=192 
     	        	      && xlate >=0 
     	        	      && xlate <1000)
     	        	      
     	        	      goodmatch = 1;
     	        	      
     	        	    break;
     	        	  }
     	        	    
     	        	  if( !cbf_cistrncmp(dictype,"yyyy-",5) ||
     	        	      !cbf_cistrncmp(dictype,"date",4) )  {
     	        	  
     	        	    mm=-1, dd=-1, hr=0, mi =0, se=0, sf=0, tz = 0;
     	        	       	        	    
     	        	    yyyy=strtol(valuestring, &endptr, 10);
     	        	    if (*endptr=='-') {
     	        	      mm=strtol(endptr+1, &endptr, 10);
     	        	      if (*endptr=='-') {
     	        	        dd=strtol(endptr+1, &endptr, 10);
     	        	        if ( *endptr=='T'
     	        	          || *endptr=='t'
     	        	          || *endptr==':') {	        	          
     	        	          hr=strtol(endptr+1, &endptr, 10);
     	        	          if (  *endptr==':') {
     	        	            mi=strtol(endptr+1, &endptr, 10);
     	        	            if ( *endptr==':') {
     	        	              se=strtol(endptr+1, &endptr, 10);
     	        	              if ( *endptr=='.') {
     	        	                sf=strtol(endptr+1, &endptr, 10);
     	        	              }
     	        	            }
     	        	          }
     	        	        }   	        	      
     	        	      }
     	        	    }
     	        	    if (*endptr=='-'||*endptr=='+') tz=strtol(endptr+1, &endptr, 10);
     	        	    if (*endptr=='\0'
     	        	      && yyyy>=0  && yyyy<10000  && mm > 0 && mm < 13
     	        	      && dd > 0 && dd < 32
     	        	      && hr >=0 && hr <25 && mi >=0 && mi <61  && se >=0 && se <61
     	        	      && sf >=0
     	        	      && tz >=0 && tz <25 ) goodmatch = 1;
     	   
     	        	  	break;
     	        	  }
    	        	
    	        	  if ( !cbf_cistrncmp(dictype,"char",4)
    	        	    || !cbf_cistrncmp(dictype,"ucha",4)
    	        	    || !cbf_cistrncmp(dictype,"code",4)
    	        	    || !cbf_cistrncmp(dictype,"name",4)
    	        	    || !cbf_cistrncmp(dictype,"idna",4)
    	        	    || !cbf_cistrncmp(dictype,"alia",4)
    	        	    || !cbf_cistrncmp(dictype,"ucod",4)
    	        	    || !cbf_cistrncmp(dictype,"line",4)
    	        	    || !cbf_cistrncmp(dictype,"ulin",4)
    	        	    || !cbf_cistrncmp(dictype,"any", 3)
    	        	    || !cbf_cistrncmp(dictype,"atco",4)
    	        	    || !cbf_cistrncmp(dictype,"phon",4)
    	        	    || !cbf_cistrncmp(dictype,"emai",4)
    	        	    || !cbf_cistrncmp(dictype,"fax", 3)
    	        	    || !cbf_cistrncmp(dictype,"text",4) 
    	        	    || !cbf_cistrncmp(dictype,"tag",3) 
    	        	    || !cbf_cistrncmp(dictype,"ctag",4) 
    	        	    || !cbf_cistrncmp(dictype,"otag",4) )  {
    	        	  	
    	        	    goodmatch = 1; break;
    	        	  	
    	        	  }
    	        	  
    	        	  if ( cbf_cistrncmp(dictype,"numb",4)
    	        	    || cbf_cistrncmp(dictype,"int",3)
    	        	    || cbf_cistrncmp(dictype,"floa",4) ) {
    	        	        	        	    
    	        	    ltest = strtol(valuestring, &endptr, 10);

    	        	    if (*endptr=='\0') { goodmatch = 1; break; }
    	        	    
    	        	    if (*endptr == '(')  {
    	        	    
    	        	      ltest = strtol(endptr+1, &endptr, 10);
    	        	      
    	        	      if (*endptr==')') { goodmatch = 1; break; }
    	        	    	
    	        	    
    	        	    }
    	        	    
    	        	    if ( !cbf_cistrncmp(dictype,"numb",4)
    	        	      || !cbf_cistrncmp(dictype,"floa",4) ) {
    	        	      
    	        	      dtest = strtod(valuestring, &endptr);
    	        	      
    	        	      if (*endptr=='\0') { goodmatch = 1; break; }
    	        	      
      	        	      if (*endptr == '(')  {
    	        	    
    	        	        ltest = strtol(endptr+1, &endptr, 10);
    	        	      
    	        	        if (*endptr==')') { goodmatch = 1; break; }
    	        	        
      	        	      }
    	        	    	
    	        	    
    	        	    }
  	        	        	        	    
    	        	  }
    	        	  
    	        	  if (!cbf_check_type_contents(dictype,valuestring)) { goodmatch = 1; break; }
    	        	  break;
    	        	
    	        	case CBF_TOKEN_SQSTRING:
    	        	case CBF_TOKEN_DQSTRING:
    	        	
    	        	  if ( !cbf_cistrncmp(dictype,"implied",8) ) {
    	        	  
    	        	    goodmatch = 1;
    	        	    
    	        	    break;
    	        	  	
    	        	  }

    	        	  if(!cbf_cistrncmp(dictype,"text",4) 
    	        	    || !cbf_cistrncmp(dictype,"any",3)
    	        	    || !cbf_cistrncmp(dictype,"line",4)
    	        	    || !cbf_cistrncmp(dictype,"ulin",4)
    	        	    || !cbf_cistrncmp(dictype,"name",4)
    	        	    || !cbf_cistrncmp(dictype,"idna",4)
    	        	    || !cbf_cistrncmp(dictype,"alia",4)
    	        	    || !cbf_cistrncmp(dictype,"atco",4)
    	        	    || !cbf_cistrncmp(dictype,"char",4)
    	        	    || !cbf_cistrncmp(dictype,"ucha",4) ) { goodmatch = 1; break;   }
    	        	    
    	        	  if (!cbf_check_type_contents(dictype,valuestring)) { goodmatch = 1; break; }
    	        	  break;

    	        	
    	        	case CBF_TOKEN_SCSTRING:
    	        	
    	        	  if ( !cbf_cistrncmp(dictype,"implied",8) ) {
    	        	  
    	        	    goodmatch = 1;
    	        	    
    	        	    break;
    	        	  	
    	        	  }

    	        	
    	        	  if(!cbf_cistrncmp(dictype,"text",4) 
    	        	    || !cbf_cistrncmp(dictype,"any",3)
    	        	    || !cbf_cistrncmp(dictype,"char",4)
    	                || !cbf_cistrncmp(dictype,"ucha",4) ) { goodmatch = 1; break;   }
    	        	   
    	        	  if (!cbf_check_type_contents(dictype,valuestring)) { goodmatch = 1; break; }
    	        	  break;

    	        	
    	        }
    	                      
    	        if (!cbf_cistrcmp(dictype,"binary")) {
    	        
    	            if ( (((char *)node)) == NULL  
    	              || (((char *)node)[0]) == CBF_TOKEN_NULL
    	              || (((char *)node)[0]) == CBF_TOKEN_TMP_BIN 
    	              || (((char *)node)[0]) == CBF_TOKEN_BIN 
    	              || (((char *)node)[0]) == CBF_TOKEN_MIME_BIN )  goodmatch = 1;
    	        	
    	        } 
    	                      
    	        if (!goodmatch)   {
    	          
    	          sprintf(buffer," %s type conflicts with dictionary type %s", itemname, dictype );

    	          cbf_log(handle, buffer,CBF_LOGWARNING|CBF_LOGSTARTLOC);
    	          
    	        } else {
    	        
    	          if (tokentype != CBF_TOKEN_NULL
    	            && !cbf_find_tag(handle->dictionary,"_items_enumerations.name")) {
    	          
    	            if (!cbf_find_hashedvalue(handle->dictionary,itemname,"name", CBF_CASE_INSENSITIVE)) {
    	            
    	              int nextrow, valok, numb;
    	              
    	              double doubleval=0.0;
    	              
    	              const char *nextitem, *enumvalue, *enumvaluetype;
    	              
    	              char * endptr;
    	              
    	              cbf_failnez(cbf_row_number(handle->dictionary, (unsigned int *) &nextrow))
    	              
    	              valok = numb = 0;
    	              
    	              if ( cbf_cistrncmp(dictype,"numb",4)
    	        	    || cbf_cistrncmp(dictype,"int",3)
    	        	    || cbf_cistrncmp(dictype,"floa",4) ) {
    	        	    
    	        	    numb = 1;
    	        	    
    	        	    doubleval = strtod(valuestring, &endptr);
    	        	  }

    	              
    	              while ( nextrow >=0 ) {
    	              
    	                cbf_failnez( cbf_find_column (handle->dictionary, "name"))
    	                
    	                cbf_failnez( cbf_select_row (handle->dictionary, nextrow))
    	                
    	                cbf_failnez( cbf_get_value (handle->dictionary, &nextitem))
    	                
    	                cbf_failnez( cbf_find_column (handle->dictionary, "name(hash_next)"))
    	                
    	                cbf_failnez( cbf_get_integervalue(handle->dictionary, &nextrow))

     	                if (nextitem && !cbf_cistrcmp(nextitem, itemname)) {
    	                
    	                  cbf_failnez( cbf_find_column (handle->dictionary, "value_type"))
    	                  
    	                  cbf_failnez( cbf_get_value (handle->dictionary, &enumvaluetype))
    	                  
    	                  cbf_failnez( cbf_find_column (handle->dictionary, "value"))
    	                  
    	                  cbf_failnez( cbf_get_value (handle->dictionary, &enumvalue))
    	                  
    	                  if (!cbf_cistrcmp(enumvaluetype,"value")) {
    	                  
    	                    if (!strcmp(enumvalue,valuestring) 
    	                      || (numb && doubleval == strtod(enumvalue, &endptr))) {
    	                    
    	                      valok = 1;
    	                      
    	                      break;
    	                    	
    	                    }
    	                    
    	                  } else {
     	                    
    	                    colonpos = strchr(enumvalue,':');
    	                    
    	                    if (colonpos)  {
    	                    
    	                      strncpy(loval, enumvalue, (size_t)(colonpos-enumvalue));
    	                      
    	                      loval[colonpos-enumvalue] = '\0';
    	                      
    	                      strcpy(hival, colonpos+1);
    	                      
    	                      if (numb) {
    	                      
    	                        if (loval[0]!= '\0' 
    	                          && !strcmp(loval,",")
    	                          && cbf_match(loval, "^-?(([0-9]+)|([0-9]*[.][0-9]+))([(][0-9]+[)])?([eE][+-]?[0-9]+)?")){

                                  sprintf(buffer,"illegal lower range value %s", loval);

                                  cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);

                                }
                                if (hival[0]!= '\0' 
    	                          && !strcmp(hival,",")
                                  && cbf_match(hival, "^-?(([0-9]+)|([0-9]*[.][0-9]+))([(][0-9]+[)])?([eE][+-]?[0-9]+)?")){

                                   sprintf(buffer,"illegal higher range value %s", hival);

                                   cbf_log(handle, buffer, CBF_LOGWARNING|CBF_LOGSTARTLOC);

                                }

    	                      
    	                        if (loval[0] == '\0' || !strcmp(loval,".")) {
    	                        
    	                          if ((!strcmp(enumvaluetype,"open_range") 
    	                            && doubleval < strtod(hival,&endptr))
    	                            || (!strcmp(enumvaluetype,"closed_range") 
    	                            && doubleval <= strtod(hival,&endptr))) {
    	                            
    	                              valok = 1;
    	                              
    	                              break;
    	                          	
    	                          } else continue;
    	                        	
    	                        } else {
    	                        	
    	                          if (hival[0] == '\0' || !strcmp(hival,".")) {
    	                           	                        
    	                            if ((!strcmp(enumvaluetype,"open_range") 
    	                              && doubleval > strtod(loval,&endptr))
    	                              || (!strcmp(enumvaluetype,"closed_range") 
    	                              && doubleval >= strtod(loval,&endptr))) {
    	                            
    	                                valok = 1;
    	                              
    	                                break;
    	                          	
    	                            } else continue;
    	                          
    	                          } else { 
    	                        
    	                            if ((!strcmp(enumvaluetype,"open_range") 
    	                              && doubleval > strtod(loval,&endptr)
    	                              && doubleval < strtod(hival,&endptr))
    	                              || (!strcmp(enumvaluetype,"closed_range") 
    	                              && doubleval >= strtod(loval,&endptr)
    	                              && doubleval <= strtod(hival,&endptr))) {
    	                            
    	                              valok = 1;
    	                              
    	                              break;
    	                        	
    	                            } 

    	                          }
    	                      	
    	                        }
    	                        
    	                      } else {
    	                        
    	                        if ( (loval[0] == '\0'
    	                          && ( (!strcmp(enumvaluetype,"open_range") 
    	                             && cbf_cistrcmp(valuestring,hival) < 0)
    	                            || cbf_cistrcmp(valuestring,hival) <= 0 ) ) 
    	                          || (hival[0] == '\0'
    	                            && ( (!strcmp(enumvaluetype,"open_range") 
    	                               && cbf_cistrcmp(valuestring,loval) > 0)
    	                            || cbf_cistrcmp(valuestring,loval) >= 0 ) ) 
    	                          || ((!strcmp(enumvaluetype,"open_range") 
    	                               && cbf_cistrcmp(valuestring,hival) < 0
    	                               && cbf_cistrcmp(valuestring,loval) > 0)
    	                             || (cbf_cistrcmp(valuestring,hival) <= 0
    	                               && cbf_cistrcmp(valuestring,loval) >= 0))) {
    	                            
    	                            valok = 1;
    	                              
    	                            break;
    	                          	
    	                        } else continue;
    	                      
    	                      }
    	                        	                  	
    	                    }
    	                    
    	                  }
    	                	
    	                }
    	                    	              	
    	              } /* while ( nextrow >=0 ) */
    	              
    	              if (!valok) {
    	              
    	                 sprintf(buffer," %s value out of dictionary range", itemname);
    	              	
    	                 cbf_log(handle, buffer,CBF_LOGWARNING|CBF_LOGSTARTLOC);

    	              }
    	            	
    	            }
    	          	
    	          }
    	        	
    	        }
    	      
    	        
    	        
    	      }

    	    }
    	  	
    	  }
    		
    	}
    	
    }
  	
  }
  
  return 0;
	
}


/*  WARNING -- THIS VERSION IS FOR TWO'S COMPLEMENT SYSTEMS */

/*  Load mpint accumulator acc[1..acsize] with the contents of source
    containing an element of size elsize.   If elsize if greater than
    sizeof (size of int) it must be a multiple of sizeof (unsigned int)*/
     
  /* Load accumulator */

int cbf_mpint_load_acc(unsigned int * acc, size_t acsize, 
                               void * source, size_t elsize, 
                               int elsign, const char * border) {

  size_t bits;

  unsigned char * unsigned_char_data;

  int iint, numints;

  unsigned int sign;

  unsigned int sextend;
  
  bits = elsize * CHAR_BIT;

  numints =  (bits + CHAR_BIT*sizeof (unsigned int) -1)/(CHAR_BIT*sizeof (unsigned int));

  if (numints > acsize) return CBF_ARGUMENT;

  if (numints > 1 && numints*sizeof(int)*CHAR_BIT != bits) return CBF_ARGUMENT;

  sign = elsign?(1<<(bits-(numints-1)*sizeof(unsigned int)*CHAR_BIT-1)):0;

  sextend = 0;

  if (elsize < sizeof(unsigned int)) {

  	sextend = (-(1 << (elsize * CHAR_BIT)));

  }

  unsigned_char_data = (unsigned char *)source;
  
  switch (elsize) {
  
  	case (sizeof(char)):

  	  acc[0] = *unsigned_char_data; break;

  	case (sizeof(short)):

  	  acc[0] = *((unsigned short *) unsigned_char_data); break;

  	case (sizeof(int)):

  	  acc[0] = *((unsigned int *) unsigned_char_data); break;

  	default:

  	  if (*border == 'b' || *border == 'B') {

        for (iint = numints; iint; iint--) {

          acc[iint-1] = *((unsigned int *)unsigned_char_data);

          unsigned_char_data += sizeof(unsigned int);

        }

  	  } else {

        for (iint = 0; iint < numints; iint++) {

          acc[iint] = *((unsigned int *)unsigned_char_data);

          unsigned_char_data += sizeof(unsigned int);

        } 	

      }

      break;

  }

  if (acc[numints-1] & sign) {

    acc[numints-1] |= sextend;

    if ( numints < acsize ) {

      for (iint = numints; iint < acsize; iint++) acc[iint] = ~0; 	

    }

  } else {	

    if ( numints < acsize ) {

    for (iint = numints; iint < acsize; iint++) acc[iint] = 0; 	

    }

  }

  return 0;
	
}

  /* Store accumulator */

int cbf_mpint_store_acc(unsigned int * acc, size_t acsize, 
                                void * dest, size_t elsize,
                                int elsign, const char *border) {

  size_t bits;

  unsigned char * unsigned_char_data;

  int iint, numints;
  
  bits = elsize * CHAR_BIT;

  numints =  (bits + CHAR_BIT*sizeof (unsigned int) -1)/(CHAR_BIT*sizeof (unsigned int));

  if (numints > acsize) return CBF_FORMAT;

  unsigned_char_data = (unsigned char *)dest;

  
  switch (elsize) {
  
  	case (sizeof(char)):

  	  *unsigned_char_data = acc[0]; break;

  	case (sizeof(short)):

  	  *((unsigned short *) unsigned_char_data) = acc[0]; break;

  	case (sizeof(int)):

  	  *((unsigned int *) unsigned_char_data) = acc[0]; break;

  	default:

  	  if (*border == 'b' || *border == 'B') {

        for (iint = numints; iint; iint--) {

          *((unsigned int *)unsigned_char_data) = acc[iint-1];

  	      unsigned_char_data += sizeof(unsigned int);

        }

  	  } else {

        for (iint = 0; iint < numints; iint++) {

          *((unsigned int *)unsigned_char_data) = acc[iint];

          unsigned_char_data += sizeof(unsigned int);

        }

      }

      break;

  }

  return 0;
	
}

  /* Clear accumulator */

int cbf_mpint_clear_acc(unsigned int * acc, size_t acsize) {

  int iint;

  for (iint=0; iint<acsize; iint++) acc[iint] = 0;

  return 0;
	
}

  /* Increment accumulator */

int cbf_mpint_increment_acc(unsigned int * acc, size_t acsize) {

  /*  In incrementing a multiprecision integer, we need to
  carry from one element to the next if the element has
  the sign bit set and, if after an increment of the
  element, the  sign bit of that element is no longer set */

  int iint;

  int carry, precarry;

  unsigned int sign;

  carry = 1;

  precarry = 0;

  sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);

  for (iint=0; iint<acsize && carry; iint++) {

    if ( (acc[iint]&sign) ) precarry = 1;    

    acc[iint]++;

    if ( (acc[iint]&sign) || !precarry) carry = 0;

    precarry = 0;

  }

  return 0;
	
}

  /* Decrement accumulator */

int cbf_mpint_decrement_acc(unsigned int * acc, size_t acsize) {

  /*  In decrementing a multiprecision integer, we need to
  borrow from the next element if the current element does
  not have the sign bit set and, if after an increment of the
  element, the  sign bit of that element is set */
  
  int iint;

  int borrow, preborrow;

  unsigned int sign;

  borrow = 1;

  preborrow = 0;

  sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);

  for (iint=0; iint<acsize && borrow; iint++) {

    if ( !(acc[0]&sign) ) preborrow = 1;    

    acc[iint]--;

    if ( !(acc[iint]&sign) || !preborrow  ) borrow = 0;

    preborrow = 0;

  }

  return 0;
	
}

  /* Negate accumulator */

int cbf_mpint_negate_acc(unsigned int * acc, size_t acsize) {

  int iint;
  
  for (iint=0; iint< acsize; iint++) {

    acc[iint] = ~acc[iint];  	

  }
  
  return cbf_mpint_increment_acc(acc,acsize);

}

  /* Add to accumulator */

int cbf_mpint_add_acc(unsigned int * acc, size_t acsize, unsigned int * add, size_t addsize) {

  int iint;

  unsigned int carry;

  unsigned int precarry;

  unsigned int sign;
  
  carry = 0; precarry=0;

  sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);

  for (iint = 0; iint < acsize && iint < addsize; iint++) {

    if (carry) {

      cbf_failnez(cbf_mpint_increment_acc(acc+iint,acsize-iint));

    }

    if ( acc[iint] & sign ) precarry++;

    if ( add[iint] & sign ) precarry++;

    acc[iint] += add[iint];

    carry = 0;

    if (precarry == 2 || (precarry == 1 && !(acc[iint]&sign) ) ) carry = 1;

    precarry = 0;

  }
  
  if (addsize > acsize) {

    if (add[acsize-1] &sign ) {

      for (iint = acsize; iint < acsize; iint++)

        if ( add[iint] != -1) return CBF_ARGUMENT;

    } else {

      for (iint = acsize; iint < acsize; iint++)

        if ( add[iint] != 0) return CBF_ARGUMENT;    	

    }  	

  } else if (acsize > addsize){

    for (iint = addsize; iint < acsize; iint++) {

      if (carry) {

        cbf_failnez(cbf_mpint_increment_acc(acc+iint,acsize-iint));

      }

      if ( acc[iint] & sign ) precarry++;

      carry = 0;

      if (precarry == 1 && !(acc[iint] &sign) ) carry = 1;

      precarry = 0;

    } 	

  }

  return 0;

}

  /* Shift accumulator right */

int cbf_mpint_rightshift_acc(unsigned int * acc, size_t acsize, int shift) {

  int iint;

  size_t bigshift;

  unsigned int extrabits, xextrabits, mask;

  unsigned int sign;
  
  sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);
  
  if (shift < 0) return cbf_mpint_leftshift_acc(acc, acsize, -shift);

  bigshift = 0;

  if (shift >= sizeof(unsigned int)*CHAR_BIT) {

    extrabits = 0;

    if (acc[acsize-1]<0) extrabits = ~0;

    bigshift = shift/(sizeof(unsigned int)*CHAR_BIT);

    shift -= bigshift*sizeof(unsigned int)*CHAR_BIT;

  	if (bigshift > acsize*sizeof(unsigned int)*CHAR_BIT) {

  	  return cbf_mpint_clear_acc(acc, acsize);

  	} else {

  	  for (iint = acsize; iint-bigshift > 0; iint--) acc[iint-bigshift-1] = acc[iint-1];

  	  for (iint = acsize; iint > acsize-bigshift+1; iint--) acc[iint-1] = extrabits;

  	}

  }

  
  if (shift == 0) return 0;
  
  extrabits = 0;

  if (acc[acsize]&sign) extrabits = (~0)<<(sizeof(unsigned int)*CHAR_BIT-shift);

  mask = ~((~0)<<(sizeof(unsigned int)*CHAR_BIT-shift));

  for (iint = acsize; iint; iint--) {

    xextrabits = acc[iint-1]<<(sizeof(unsigned int)*CHAR_BIT-shift);

    acc[iint-1] = ((acc[iint-1]>>shift)&mask)|extrabits;

    extrabits = xextrabits;

  }
  
  return 0;

}


  /* Shift accumulator left */

int cbf_mpint_leftshift_acc(unsigned int * acc, size_t acsize, int shift) {

  int iint;

  size_t bigshift;

  unsigned int extrabits, xextrabits, mask;

  unsigned int sign;
  
  sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);
  
  if (shift < 0) return cbf_mpint_rightshift_acc(acc, acsize, -shift);

  bigshift = 0;

  if (shift >= sizeof(unsigned int)*CHAR_BIT) {

    extrabits = 0;

    bigshift = shift/(sizeof(unsigned int)*CHAR_BIT);

    shift -= bigshift*sizeof(unsigned int)*CHAR_BIT;

  	if (bigshift > acsize*sizeof(unsigned int)*CHAR_BIT) {

  	  return cbf_mpint_clear_acc(acc, acsize);

  	} else {

  	  for (iint = 0; iint+bigshift < acsize; iint++) acc[iint+bigshift] = acc[iint];

  	  for (iint = 0; iint < bigshift; iint++) acc[iint] = extrabits;

  	}

  }
  
  if (shift == 0) return 0;
  
  extrabits = 0;

  mask = -(1<<shift);

  for (iint = 0; iint < acsize; iint++) {

    xextrabits = (acc[iint]>>(sizeof(unsigned int)*CHAR_BIT-shift))&(~mask);

    acc[iint] = ((acc[iint]<<shift)&mask)|extrabits;

    extrabits = xextrabits;

  }

  return 0;

}

  /* Check value of type validity */
  
int cbf_check_type_contents (const char *type, const char *value){
	if (!cbf_cistrcmp(type,"Achar")) {
		
		return cbf_match(value,"^[A-Za-z]$");
		
	}else if (!cbf_cistrcmp(type,"ANchar")) {
		
		return cbf_match(value,"^[A-Za-z0-9]$");
	
	}else if (!cbf_cistrcmp(type,"Element")) {
	
		/*Achar +*/
		return cbf_match(value,"^[A-Za-z]+$");
		
	}else if (!cbf_cistrcmp(type,"Tag")) {
	
		/*_ Ctag [._] Otag*/
		return cbf_match(value,"^[_][A-Za-z0-9]+[_][._][A-Za-z0-9]+[_]$");
		
	}else if (!cbf_cistrcmp(type,"Otag")
			  || !cbf_cistrcmp(type,"Ctag")
			  || !cbf_cistrcmp(type,"Filename")) {
		
		/* ANchar [_] +*/
		return cbf_match(value,"^[A-Za-z0-9]+[_]$");
		
	}else if (!cbf_cistrcmp(type,"Savename")) {
		
		/*$ Otag*/
		return cbf_match(value,"[$][A-Za-z0-9]+[_]");
		
	}else if (!cbf_cistrcmp(type,"Date")) {
		
		/*[0-9][0-9][0-9][0-9]-[0-1]?[0-9]-[0-3][0-9]*/
		return cbf_match(value,"^[0-9][0-9][0-9][0-9]-[0-1]?[0-9]-[0-3][0-9]$");
		
	}else if (!cbf_cistrcmp(type,"Version")) {
		
		/*Count [.] Count [.] Count*/
		return cbf_match(value,"^[0-9]+[.][0-9]+[.][0-9]+$");
		
	}else if (!cbf_cistrcmp(type,"Range")) {
		
		/*Integer ? : Integer ?*/
		return cbf_match(value,"[+-]?[0-9]+:[+-]?[0-9]+");
		
	}else if (!cbf_cistrcmp(type,"Digit")) {
		
		/*[0-9]*/
		return cbf_match(value,"^[0-9]$");
		
	}else if (!cbf_cistrcmp(type,"Count")) {
		
		/*[0-9]+*/
		return cbf_match(value,"^[0-9]+$");
		
	}else if (!cbf_cistrcmp(type,"Index")) {
		
		/*[1-9] Digit +*/
		return cbf_match(value,"^[1-9]+[0-9]+");
		
	}else if (!cbf_cistrcmp(type,"Integer")) {
		
		/*[+-]? Count*/
		return cbf_match(value,"^[+-]?[0-9]+$");
		
	}else if (!cbf_cistrcmp(type,"Binary")) {
		
		/*0b[0-1]+*/
		return cbf_match(value,"^0b[0-1]+");
		
	}else if (!cbf_cistrcmp(type,"Hexadecimal")) {
		
		/*0x[0-7a-fA-F]+*/
		return cbf_match(value,"^0x[0-9a-fA-F]+$");
		
	}else if (!cbf_cistrcmp(type,"Octal")) {
		
		/*0o[0-7]+*/
		return cbf_match(value,"^0o[0-7]+$");
		
	}else if (!cbf_cistrcmp(type,"Symop")) {
		
		/*[0-1]?[0-9]?[0-9]_[0-9][0-9][0-9]*/
		return cbf_match(value,"^[0-1]?[0-9]?[0-9]_[0-9][0-9][0-9]$");
		
	}else if (!cbf_cistrcmp(type,"YesorNo")) {
		return cbf_match(value,"^y(es)?$|^n(o)?$"); 
		
	}else if (!cbf_cistrcmp(type,"Pchar")
			  ||!cbf_cistrcmp(type,"Uri")) {
		
		/*[()\[\]_,.;:"&<>/\{}'`~!@#$%?+=*A-Za-z0-9|^-]*/
		return cbf_match(value,"");
		
	}else if (!cbf_cistrcmp(type,"Text")) {
		
		/*[][ \n\t()_,.;:"&<>/\{}'`~!@#$%?+=*A-Za-z0-9|^-]*/
		return cbf_match(value,"");
		
	}else if (!cbf_cistrcmp(type,"Code")) {
		
		/*[()\[\]_&<>{}~!@#$%?+=*A-Za-z0-9|^-]+*/
		return cbf_match(value,"");
		
	}else if (!cbf_cistrcmp(type,"Dimension")) {
		
		/*[[] Count [,]? + []]*/
		return cbf_match(value,"");
		
	}else if (!cbf_cistrcmp(type,"Float")
			  || !cbf_cistrcmp(type,"Real")) {
		
		/*-?(([0-9]+)|([0-9]*[.][0-9]+))([(][0-9]+[)])?([eE][+-]?[0-9]+)?*/
		return cbf_match(value,"^-?(([0-9]+)|([0-9]*[.][0-9]+))([(][0-9]+[)])?([eEdDqQ][+-]?[0-9]+)?");
		
	}else if (!cbf_cistrcmp(type,"Imag")) {
		
		/*Real[jJ]*/
		return cbf_match(value,"^-?((([0-9]+)|([0-9]*[.][0-9]+))([(][0-9]+[)])?([eEdDqQ][+-]?[0-9]+)?)?[iIjJ]");
		
	}else if (!cbf_cistrcmp(type,"Label")) {
		
		/*[()\[\]_&<>{}~!@#$%?+=*A-Za-z0-9|^-]+*/
		return cbf_match(value,"");
		
	}else if (!cbf_cistrcmp(type,"Formula")) {
		
		/*[()\[\]+-=*A-Za-z0-9]+*/
		return cbf_match(value,"");
		
	}

	return 1;
}

  /* Regex Match function */
  
int cbf_match(const char *string, char *pattern) { 
	
	int status; 
	
	regex_t re; 
	
	if(regcomp(&re, pattern, REG_EXTENDED|REG_NOSUB) != 0) { 
		
    		return 1; 
	
	} 
	
	status = regexec(&re, string, (size_t)0, NULL, 0); 
	
	regfree(&re); 
	
	if(status != 0) { 
	
	    return 1;
	} 
	return 0;
}

#ifdef __cplusplus

}

#endif

