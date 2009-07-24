/**********************************************************************
 * cbf_file -- file access (characterwise and bitwise)                *
 *                                                                    *
 * Version 0.7.7 19 February 2006                                     *
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
#include "cbf_codes.h"
#include "cbf_file.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>



  /* Create and initialise a file */

int cbf_make_file (cbf_file **file, FILE *stream)
{
  char ** fc;
  
    /* Allocate the memory */

  cbf_failnez (cbf_alloc ((void **) file, NULL, sizeof (cbf_file), 1))
  
  fc = &((*file)->characters);

  cbf_onfailnez (cbf_alloc ( (void **)fc, 
    NULL, CBF_INIT_WRITE_BUFFER, 1),
    cbf_free((void **)file,NULL)) 

    /* Initialise */

  (*file)->stream          = stream;
  (*file)->connections     = 1;
  (*file)->temporary       = stream?0:1;
  (*file)->bits [0]        = 0;
  (*file)->bits [1]        = 0;
  (*file)->characters_base = (*file)->characters;
  (*file)->characters_size = CBF_INIT_WRITE_BUFFER;
  (*file)->characters_used = 0;
  (*file)->last_read       = 0;
  (*file)->line            = 0;
  (*file)->column          = 0;
  (*file)->columnlimit     = CBF_LINELENGTH_10;
  (*file)->buffer_size     = 0;
  (*file)->buffer_used     = 0;

  (*file)->buffer          = NULL;
  (*file)->digest          = NULL;

  (*file)->read_headers    = 0;
  (*file)->write_headers   = 0;
  (*file)->write_encoding  = 0;


    /* Success */

  return 0;
}

  /* Create and initialise a wide file */

int cbf_make_widefile (cbf_file **file, FILE *stream)
{
    cbf_failnez(cbf_make_file (file, stream))

    (*file)->columnlimit     = CBF_LINELENGTH_11;

    return 0;
}

  /* Free a file */

int cbf_free_file (cbf_file **file)
{
  int errorcode;
  
  void *vbuffer;
  
  void *vdigest;
  
  void *vcharacters;

  errorcode = 0;

  if (file)

    if (*file)
    {
      if ((*file)->stream)

        if (fclose ((*file)->stream))

          errorcode = CBF_FILECLOSE;
          
      vbuffer = (void *)(*file)->buffer;
      
      vdigest = (void *)(*file)->digest;
      
      vcharacters = (void *)(*file)->characters;
      
      if ((*file)->characters_base) vcharacters = (void *)(*file)->characters_base; 

      errorcode |= cbf_free ((void **) &vbuffer,
                                       &(*file)->buffer_size);

      errorcode |= cbf_free ((void **) &vcharacters,
                                       &(*file)->characters_size);

      errorcode |= cbf_free ((void **) &vdigest, NULL);

      errorcode |= cbf_free ((void **) file, NULL);
    }


    /* Success? */

  return errorcode;
}


  /* Add a file connection */

int cbf_add_fileconnection (cbf_file **file, FILE *stream)
{
    /* Does the file pointer exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Does the file exist? */

  if (*file) {

      /* Does the stream match? */

    if (stream && (*file)->stream != stream)

      return CBF_NOTFOUND;

    else
    {
      (*file)->connections++;

      return 0;
    }

  }


    /* Create a new file */

  return cbf_make_file (file, stream);
}


  /* Remove a file connection */

int cbf_delete_fileconnection (cbf_file **file)
{
    /* Does the file pointer exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Does the file exist? */

  if (!*file)

    return CBF_ARGUMENT;


    /* Remove a connection */

  (*file)->connections--;


    /* Delete the file? */

  if ((*file)->connections == 0)

    return cbf_free_file (file);


    /* Success */

  return 0;
}


  /* Count the connections */

int cbf_file_connections (cbf_file *file)
{
  if (!file)

    return 0;

  return file->connections;
}


  /* Set the size of the buffer */

int cbf_set_buffersize (cbf_file *file, size_t size)
{
  void * vbuffer;

    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;

    /* Is the size already close enough? */

  if (size > 0 && file->buffer_size >=  size &&
                  file->buffer_size <   2*size)

    return 0;


    /* Reallocate the buffer */
    
  vbuffer = (void *)file->buffer;

  cbf_failnez(cbf_realloc ((void **) &vbuffer,
                                &file->buffer_size, sizeof (char), size))
                                
  file->buffer = (char *)vbuffer;
  
  return 0;
}


  /* Empty the buffer */

int cbf_reset_buffer (cbf_file *file)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Empty the buffer */

  file->buffer_used = 0;


    /* success */

  return 0;
}


  /* Set input/output buffer size */


int cbf_set_io_buffersize (cbf_file *file, size_t size)
{

    size_t old_data, old_size, target_size;
      
    char ** fc;
        
    /* if insufficient space, increase to at least double the
        old space, but certainly to the requested size */

	if (file->characters_size < CBF_INIT_WRITE_BUFFER
	  || file->characters_size < size ) {
      
      fc = &(file->characters_base);
    
      old_data = file->characters-file->characters_base;
    
      old_size = old_data + file->characters_size;
      
      target_size = old_data + size;
      
      if (target_size  < old_size) target_size = old_size*2;
    
      if (cbf_realloc ((void **)fc, &old_size, 1, target_size)) {
      
        file->temporary = 0;
        
        file->characters = file->characters_base;
        
        file->characters_used = old_data;
        
        file->characters_size = old_size;
        
        if (file->characters_size < size) return CBF_ALLOC;
        
        return 0;
      	
      } else {
      
        file->characters = file->characters_base + old_data;
        
        file->characters_size = old_size - old_data;
        
      }
          	
    }
    
    return 0;

}

  
  
  /* Set output buffer size */
  
int cbf_set_output_buffersize (cbf_file *file, size_t size)
{
    
    /* try to get the needed space by flushing the
       current output buffer */
    
    cbf_failnez (cbf_flush_characters(file))
    
    cbf_failnez (cbf_set_io_buffersize(file, size))
    
    return 0;

}

  /* Add a character to the buffer */

int cbf_save_character (cbf_file *file, int c)
{
  unsigned int new_size;

    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Expand the buffer? */

  if (file->buffer_size < file->buffer_used+3) {

    new_size = (file->buffer_used+3)*2;

    if (new_size >= file->buffer_size)
      cbf_failnez (cbf_set_buffersize (file, new_size))
  }

    /* Add the character */

  file->buffer [file->buffer_used] = (char) c;

  file->buffer_used++;

  file->buffer [file->buffer_used] = '\0';


    /* Success */

  return 0;
}

  /* Add a character to the buffer, trim lines */

int cbf_save_character_trim (cbf_file *file, int c)
{
  unsigned int new_size;

    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Expand the buffer? */

  if (file->buffer_size < file->buffer_used+3) {

    new_size = (file->buffer_used+3)*2;

    if (new_size >= file->buffer_size)
      cbf_failnez (cbf_set_buffersize (file, new_size))
  }

    /* Check for end of line, if so, trim */
    
  if ((char)c == '\n') {

    while (file->buffer_used > 0 
      && file->buffer[file->buffer_used-1] != '\n' 
      && file->buffer[file->buffer_used-1] != '\r' 
      && isspace(file->buffer[file->buffer_used-1])) {
      
      file->buffer_used--;    }
  	
  }
    /* Add the character */

  file->buffer [file->buffer_used] = (char) c;

  file->buffer_used++;

  file->buffer [file->buffer_used] = '\0';


    /* Success */

  return 0;
}


  /* Retrieve the buffer */

int cbf_get_buffer (cbf_file *file, const char **buffer,
                                         size_t *buffer_size)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Copy the buffer */

  if (buffer) {

    if (file->buffer_used <= 0)

      *buffer = NULL;

    else

      *buffer = file->buffer;

  }

  if (buffer_size)

    *buffer_size = file->buffer_used;


    /* Success */

  return 0;
}


  /* Get the file coordinates */

int cbf_get_filecoordinates (cbf_file *file, unsigned int *line,
                                             unsigned int *column)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Read the coordinates */

  if (line)

    *line = file->line;

  if (column)

    *column = file->column;


    /* Success */

  return 0;
}


  /* Set the file coordinates */

int cbf_set_filecoordinates (cbf_file *file, unsigned int line,
                                             unsigned int column)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Set the coordinates */

  file->line = line;

  file->column = column;


    /* Success */

  return 0;
}


  /* Read the next bit */

int cbf_get_bit (cbf_file *file)
{
  int bit;

  if (file->bits [0] == 0) {

    if (file->temporary) {
      
      if (file->characters_used) {
      
        file->bits [1] = *((file->characters)++);
          
        file->bits [1] &= 0xFF;
          
        file->characters_used--;
          
        file->characters_size--;
        
      } else {
        
        file->bits [1] = EOF;
        	
      }
      	
    } else {
    
      if (file->characters_used) {
      
        file->bits [1] = *((file->characters)++);
          
        file->bits [1] &= 0xFF;
          
        file->characters_used--;
          
        file->characters_size--;
        
      } else {
      	
        file->bits [1] = getc (file->stream);

      }

    }

    if (file->bits [1] == EOF)

      return EOF;

    file->bits [0] = 8;
  }

  bit = file->bits [1] & 1;

  file->bits [1] >>= 1;

  file->bits [0]--;


    /* Success */

  return bit;
}


  /* Read the next bits (signed) */

int cbf_get_bits (cbf_file *file, int *bitslist, int bitcount)
{
  int bitcode, count, m, maxbits;


    /* Number of bits in an integer */

  maxbits = sizeof (int) * CHAR_BIT;


    /* Read the bits in int-sized blocks */

  while (bitcount > maxbits)
  {
    cbf_failnez (cbf_get_bits (file, bitslist, maxbits))

    bitslist++;

    bitcount -= maxbits;
  }


    /* Read the bits into an int */

  count = file->bits [0];

  bitcode = file->bits [1] & 0x0ff;

  while (count < bitcount)
  {
    if (file->temporary) {
      
      if (file->characters_used) {
      
        file->bits [1] = *((file->characters)++);
          
        file->bits [1] &= 0xFF;
          
        file->characters_used--;
          
        file->characters_size--;
        
      } else {
        
        file->bits [1] = EOF;
        	
      }
      	
    } else {

      if (file->characters_used) {
      
        file->bits [1] = *((file->characters)++);
          
        file->bits [1] &= 0xFF;
          
        file->characters_used--;
          
        file->characters_size--;
        
      } else {
      	
        file->bits [1] = getc (file->stream);

      }
 
    }

    if (file->bits [1] == EOF)

      return CBF_FILEREAD;

    file->bits [0] = 8;

    bitcode |= (file->bits [1] << count) & -(1 << count);

    count += 8;
  }

  file->bits [1] = (file->bits [1] >> (file->bits [0] - (count - bitcount)));

  file->bits [0] = count - bitcount;


    /* Sign-extend */

  m = 1 << (bitcount - 1);

  if (bitcode & m)

    *bitslist = bitcode | -m;

  else

    *bitslist = bitcode & ~-m;


    /* Success */

  return 0;
}


  /* Write bits */

int cbf_put_bits (cbf_file *file, int *bitslist, int bitcount)
{
  int resultcode, maxbits, bits0, bits1;


    /* Number of bits in an integer */

  maxbits = sizeof (int) * CHAR_BIT;


    /* Write the bits in int-sized blocks */

  while (bitcount > maxbits)
  {
    cbf_failnez (cbf_put_bits (file, bitslist, maxbits))

    bitslist++;

    bitcount -= maxbits;
  }


  bits0 = file->bits [0];
  bits1 = file->bits [1];


    /* Get the first 8 bits */

  bits1 |= (*bitslist & 0x0ff) << bits0;
  bits0 +=  bitcount;


    /* Write 8 bits? */

  if (bits0 >= 8)
  {
      /* Add the character to the character buffer */

    file->characters [file->characters_used] = bits1 & 0xff;

    file->characters_used++;

    if (file->characters_used == file->characters_size)
    {
      resultcode = cbf_flush_characters (file);

      if (resultcode)
      {
        file->bits [0] = bits0;
        file->bits [1] = bits1;

        return resultcode;
      }
    }

    bits0 -= 8;


      /* Get the remaining bits */

    bits1 = *bitslist >> (bitcount - bits0);


      /* Write the remaining bits */

    while (bits0 >= 8)
    {
      file->characters [file->characters_used] = bits1 & 0xff;

      file->characters_used++;

      if (file->characters_used == file->characters_size)
      {
        resultcode = cbf_flush_characters (file);

        if (resultcode)
        {
          file->bits [0] = bits0;
          file->bits [1] = bits1;

          return resultcode;
        }
      }

      bits1 >>= 8;
      bits0 -=  8;
    }
  }

  bits1 &= ~-(1 << bits0);

  file->bits [0] = bits0;
  file->bits [1] = bits1;


    /* Success */

  return 0;
}


  /* Read an integer as a series of bits */

int cbf_get_integer (cbf_file *file, int *val, int valsign,
                                               int bitcount)
{
  int maxbits, signbits, valbits, sign, errorcode;

  int deval;

  int *tval = &deval;


    /* Any bits to read? */

  if (bitcount <= 0)
  {
    if (val) *val = 0;

    return 0;
  }


    /* Number of bits in an integer */

  maxbits = sizeof (int) * CHAR_BIT;


    /* Number of bits in the value and sign parts */

  signbits = bitcount - sizeof (int) * CHAR_BIT;

  if (signbits > 0)

    valbits = bitcount - signbits;

  else

    valbits = bitcount;

    /* Read the value */

  cbf_failnez (cbf_get_bits (file, (int *)tval, valbits))


    /* Fix the sign */

  if (valbits < maxbits && valsign == 0)

    deval &= ~-(1 << valbits);


    /* Read the sign bits */

  errorcode = 0;

  while (signbits > 0)
  {
    if (signbits < maxbits)

      cbf_failnez (cbf_get_bits (file, &sign, signbits))

    else

      cbf_failnez (cbf_get_bits (file, &sign, maxbits))

    signbits -= maxbits;


      /* Overflow? */

    if (sign != -(deval < 0 && valsign))
    {
      errorcode = CBF_OVERFLOW;

      if (valsign)

        deval = -(sign >= 0) ^ (1 << (maxbits - 1));

      else

        deval = -1;
    }
  }

  if (val) {

    *val = deval;

  }

  return errorcode;

}


  /* Write an integer as a series of bits */

int cbf_put_integer (cbf_file *file, int val, int valsign,
                                              int bitcount)
{
  int maxbits, signbits, valbits, sign;


    /* Any bits to write? */

  if (bitcount <= 0)

    return 0;


    /* Number of bits in an integer */

  maxbits = sizeof (int) * CHAR_BIT;


    /* Number of bits in the value and sign parts */

  signbits = bitcount - maxbits;

  if (signbits > 0)

    valbits = bitcount - signbits;

  else

    valbits = bitcount;


    /* Sign value */

  sign = -(val < 0 && valsign);


    /* Write the value */

  cbf_failnez (cbf_put_bits (file, &val, valbits))


    /* Write the sign part */

  while (signbits >= maxbits)
  {
    cbf_failnez (cbf_put_bits (file, &sign, maxbits))

    signbits -= maxbits;
  }

  if (signbits > 0)

    cbf_failnez (cbf_put_bits (file, &sign, signbits))


    /* Success */

  return 0;
}


  /* Initialize a message digest */

int cbf_start_digest (cbf_file *file)
{
  void *vdigest;

  if (!file)

    return CBF_ARGUMENT;


    /* Flush the buffers */

  cbf_failnez (cbf_flush_characters (file))


    /* Allocate the md5 context */

  if (!file->digest) {
  	
  	vdigest = (void *)file->digest;

    cbf_failnez (cbf_alloc ((void **)&vdigest,
                                       NULL, sizeof (MD5_CTX), 1))
                                       
    file->digest = (MD5_CTX *)vdigest;
                                       
  }


    /* Initialize */

  MD5Init (file->digest);


    /* Success */

  return 0;
}


  /* Get the message digest */

int cbf_end_digest (cbf_file *file, char *digest)
{
  unsigned char raw_digest [16];
  
  void *vdigest;

  if (!file || !digest)

    return CBF_ARGUMENT;

  if (!file->digest)

    return CBF_ARGUMENT;


    /* Flush the buffers */

  cbf_failnez (cbf_flush_characters (file))


    /* Get the raw digest */

  MD5Final (raw_digest, file->digest);


    /* Free the md5 context */
    
  vdigest = (void *)file->digest;

  cbf_failnez (cbf_free ((void **) &vdigest, NULL))
  
  file->digest = NULL;


    /* Encode the digest in base-64 */

  cbf_md5digest_to64 (digest, raw_digest);


    /* Success */

  return 0;
}


  /* Flush the bit buffer */

int cbf_flush_bits (cbf_file *file)
{
  if (!file)

    return CBF_ARGUMENT;


    /* Flush any partial bytes into the character buffer */

  cbf_failnez (cbf_put_integer (file, 0, 0, 7))


    /* Reset the bit buffers */

  file->bits [0] = 0;
  file->bits [1] = 0;


    /* Write the characters */

  return cbf_flush_characters (file);
}


  /* Flush the character buffer */

int cbf_flush_characters (cbf_file *file)
{
  int done;

  if (!file)

    return CBF_ARGUMENT;


    /* Write the characters */

  if (file->characters_used == 0)

    return 0;
    
    /* Update the message digest */

  if (file->digest)

    MD5Update (file->digest, file->characters, file->characters_used);

  while (file->temporary) {
  
    file->characters += file->characters_used;
    
    file->characters_size -= file->characters_used;
    
    file->characters_used = 0;
    
    /* Attempt to expand the character buffer if it has
       fallen below the initial write buffer size.
       If it fails, revert to disk I/O                   */
             
    if (file->characters_size < CBF_INIT_WRITE_BUFFER )  {
    
      size_t old_data, old_size;
      
      char ** fc;
      
      fc = &(file->characters_base);
    
      old_data = file->characters-file->characters_base;
    
      old_size = old_data + file->characters_size;
    
      if (cbf_realloc ((void **)fc, &old_size, 1, old_size*2)) {
      
        file->temporary = 0;
        
        file->characters = file->characters_base;
        
        file->characters_used = old_data;
        
        file->characters_size = old_size;
        
        break;
      	
      } else {
      
        file->characters = file->characters_base + old_data;
        
        file->characters_size = old_size - old_data;
        
      }
          	
    }
    
    return 0;
   	
  }

  done = fwrite (file->characters, 1, file->characters_used, file->stream);
  	

    /* Make sure the file is really updated */

  if (done > 0)

    fflush (file->stream);


    /* Remove the characters written */

  if (done < file->characters_used)
  {
    if (done > 0)
    {
      memmove (file->characters, file->characters + done,
        file->characters_size - done);

      file->characters_used = file->characters_size - done;
    }

    return CBF_FILEWRITE;
  }

  file->characters_used = 0;


    /* Success */

  return 0;
}


  /* Discard any bits in the bits buffers */

int cbf_reset_bits (cbf_file *file)
{
  if (!file)

    return CBF_ARGUMENT;

  file->bits [0] = 0;
  file->bits [1] = 0;

  return cbf_reset_characters (file);
}

  /* Discard any bits in the bits buffers for input */

int cbf_reset_in_bits (cbf_file *file)
{
  if (!file)

    return CBF_ARGUMENT;

  file->bits [0] = 0;
  file->bits [1] = 0;

  return 0;
}


  /* Discard any characters in the character buffers */

int cbf_reset_characters (cbf_file *file)
{
  if (!file)

    return CBF_ARGUMENT;
    
  file->characters_used = 0;


    /* Success */

  return 0;
}


  /* Get the next character */

int cbf_get_character (cbf_file *file)
{
 
  if (file->characters_used)  {
    
    file->last_read = *(file->characters++);
      
    file->last_read &= 0xff;
      
    (file->characters_used)--;
      
    (file->characters_size)--;
    
    return file->last_read;
    
  }
 
  if (file->temporary) {
      
    file->last_read = EOF;
    	
    return file->last_read;

  }

  file->last_read = EOF;
  
  if (file->stream) {
  	
  	size_t increment;
  	 
    if (!file->characters_base) {
    	
      cbf_failnez(cbf_set_io_buffersize(file,CBF_INIT_READ_BUFFER))

    }
      
    increment = file->characters - file->characters_base;
        
    file->characters_size += increment;
        
    file->characters = file->characters_base;
      
    if (!file->characters_size) {
      
      cbf_failnez(cbf_set_io_buffersize(file,CBF_INIT_READ_BUFFER))
      	
    }
      
    if (feof(file->stream) || ferror(file->stream))
      
      return EOF;
                       
    file->characters_used = fread(file->characters_base,1,file->characters_size,file->stream);
          
    if (file->characters_used) {

      file->last_read = *(file->characters++);
      
      file->last_read &= 0xff;
      
      (file->characters_used)--;
      
      (file->characters_size)--;
          	
    } else {
      
      if (ferror(file->stream)) 
        
        return CBF_FILEREAD;
          
      else return EOF;
        
    }

  } 
  
  return file->last_read;
}


  /* Read the next character (convert end-of-line and update line and column) */

int cbf_read_character (cbf_file *file)
{
  int last, current;


    /* Does the file exist? */

  if (!file)

    return EOF;


    /* Read the next character */

  last = file->last_read;

  while ( (current = cbf_get_character (file)) == 0);

  if ((current == '\n' && last == '\r') ||
      (current == '\r' && last == '\n'))

  while ( (current = cbf_get_character (file)) == 0);


    /* Convert the end-of-line character and update line and column */

  if (current == '\n' || current == '\r')
  {
    current = '\n';

    file->column = 0;

    file->line++;
  }
  else

    if (current == '\t')

      file->column = (file->column & ~0x07) + 8;

    else

      if (current != EOF)file->column++;

  return current;
}


  /* Put a character */

int cbf_put_character (cbf_file *file, int c)
{
    /* Does the file exist? */

  if (!file)

    return EOF;


    /* Flush the buffer? */

  if (file->characters_used == file->characters_size)

    cbf_failnez (cbf_flush_characters (file))


    /* Add the character */

  file->characters [file->characters_used] = c & 0xff;

  file->characters_used++;


    /* Success */

  return 0;
}


  /* Write a character (convert end-of-line and update line and column) */

int cbf_write_character (cbf_file *file, int c)
{
    /* Does the file exist? */

  if (!file)

    return EOF;


    /* Write the next character */

  if (c == '\n')
  {
      /* Line termination */

    if (file->write_encoding & ENC_CRTERM)

      cbf_failnez (cbf_put_character (file, '\r'))

    if (file->write_encoding & ENC_LFTERM)

      cbf_failnez (cbf_put_character (file, '\n'))


      /* Update line and column */

    if (c == '\n')
    {
      file->column = 0;

      file->line++;
    }
  }
  else
  {
    cbf_failnez (cbf_put_character (file, c))


      /* Update column */

    if (c == '\t')

      file->column = (file->column & ~0x07) + 8;

    else

      file->column++;
  }


    /* Success */

  return 0;
}


  /* Put a string */

int cbf_put_string (cbf_file *file, const char *string)
{
    /* Does the string exist? */

  if (!string)

    return CBF_ARGUMENT;


    /* Write the string one character at a time */

  while (*string)
  {
    cbf_failnez (cbf_put_character (file, *string))

    string++;
  }


    /* Success */

  return 0;
}


  /* Write a string (convert end-of-line and update line and column) */

int cbf_write_string (cbf_file *file, const char *string)
{
    /* Does the string exist? */

  if (!string)

    return CBF_ARGUMENT;


    /* Write the string */

  while (*string)
  {
    cbf_failnez (cbf_write_character (file, *string))

    string++;
  }


    /* Success */

  return 0;
}


  /* Read a (CR/LF)-terminated line into the buffer */

int cbf_read_line (cbf_file *file, const char **line)
{
  int c;
  
  char buffer[80];


    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Empty the buffer */

  file->buffer_used = 0;

  file->column = 0;


    /* Read the characters */

  do
  {
    c = cbf_read_character (file);

    if (c == EOF)

      return CBF_FILEREAD;
      
    if (file->column == file->columnlimit+1) {
      
      sprintf(buffer, "input line %u over size limit",1+file->line);

      cbf_warning(buffer);

    }


    cbf_failnez (cbf_save_character (file, c))

  }
  while (c != '\n');


    /* Copy the pointer */

  if (line)

    *line = file->buffer;


    /* Success */

  return 0;
}


  /* Read nelem characters into the buffer */

int cbf_get_block (cbf_file *file, size_t nelem)
{
  size_t done;


    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Set the buffer size */

  cbf_failnez (cbf_set_buffersize (file, nelem))


    /* Read the characters */

  file->buffer_used = 0;

  while (file->buffer_used < nelem)
  {
  
    if (file->temporary) {
    
      if (file->characters_used >= nelem-file->buffer_used) {
      
        memmove(file->buffer + file->buffer_used,
          file->characters, nelem-file->buffer_used);
          
        done = nelem-file->buffer_used;
      	
      } else if (file->characters_used) {
      
        memmove(file->buffer + file->buffer_used,
          file->characters, file->characters_used);
          
        done = file->characters_used;

      	
      } else done = 0;
      
      file->characters_used -= done;
      
      file->characters_size -= done;
      
      file->characters += done;
    
      file->buffer_used += done;
      
      return 0;
    }
    	
    if (file->stream)  {
      
      done = 0;

      if (file->characters_used >= nelem-file->buffer_used) {
      
        memmove(file->buffer + file->buffer_used,
          file->characters, nelem-file->buffer_used);
          
        done = nelem-file->buffer_used;
 
      } else if (file->characters_used) {
      
        memmove(file->buffer + file->buffer_used,
          file->characters, file->characters_used);
          
        done = file->characters_used;
        	
      } 

      file->characters_used -= done;
      
      file->characters_size -= done;
      
      file->characters += done;
        
      file->buffer_used += done;
        
      done = 0;

      if (nelem > file->buffer_used)
        done = fread (file->buffer + file->buffer_used, 1,
                         nelem - file->buffer_used, file->stream);
                         
      if ( done < nelem - file->buffer_used ) 
        return CBF_FILEREAD;
      
      file->buffer_used += done;
      
      return 0;
      
    } else  {
      	
      done = 0;
        
    }
      
    if (done <= 0)

      return CBF_FILEREAD;

    file->buffer_used += done;
  }


    /* Success */

  return 0;
}


  /* Write nelem characters from the buffer */

int cbf_put_block (cbf_file *file, size_t nelem)
{
  size_t done;


    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Are there enough characters in the buffer? */

  if (nelem > file->buffer_size)

    return CBF_ARGUMENT;


    /* Flush the buffers */

  cbf_failnez (cbf_flush_characters (file))

  if (nelem && file->digest)

    MD5Update (file->digest, file->buffer, nelem);

  while (file->temporary)  {
      
    if (file->characters_used + nelem > file->characters_size) {
    	
      size_t old_data, old_size;
      
      char ** fc;
      
      fc = &(file->characters_base);

      old_data = file->characters-file->characters_base;
    
      old_size = old_data + file->characters_size;
    
      if (cbf_realloc ((void **)fc, &old_size, 1, old_size+nelem)) {
      
        file->temporary = 0;
        
        file->characters = file->characters_base;

        file->characters_used = old_data;
        
        file->characters_size = old_size;
        
        cbf_failnez (cbf_flush_characters (file))
        
        break;
      	
      } else {
      
        file->characters = file->characters_base + old_data;
        
        file->characters_size = old_size-old_data;
        
      }
      
    }
    
    memmove(file->characters+file->characters_used,file->buffer,nelem);
    
    file->characters_used += nelem;
    
    file->characters_size -= nelem;
    
    cbf_failnez(cbf_flush_characters(file))
    
    return 0;
  	
  }

    /* Write the characters */

  if (file->stream && nelem)

    done = fwrite (file->buffer, 1, nelem, file->stream);

  else

    done = 0;


    /* Fail? */

  if (done < nelem)

    return CBF_FILEWRITE;


    /* Success */

  return 0;
}


  /* Copy characters between files */

int cbf_copy_file (cbf_file *destination, cbf_file *source, size_t nelem)
{
  size_t done=0, todo;


    /* Do the files exist? */

  if (!destination || !source)

    return CBF_ARGUMENT;

  if (!destination->stream || !source->stream)

    return CBF_ARGUMENT;


    /* Flush the buffers */

  cbf_failnez (cbf_flush_characters (destination))
  
  if (source->temporary && !(destination->temporary))  {
  
    if (source->characters_used < nelem) {
    	
      if ( source->characters_used ) 
        done = fwrite (source->characters, 1, source->characters_used, destination->stream);
      
      source->characters += source->characters_used;
      
      source->characters_size -= source->characters_used;
      
      source->characters_used = 0;
      
      return CBF_FILEREAD;
      
    }
  
    done = fwrite (source->characters, 1, nelem, destination->stream);
    
    source->characters += nelem;
    
    source->characters_size -= nelem;
    
    source->characters_used -= nelem;

    if (done < nelem)

      return CBF_FILEWRITE;
      
    return 0;

  }


    /* Copy the characters in blocks of up to CBF_TRANSFER_BUFFER */
    

  while (nelem > 0)
  {
    if (nelem >= CBF_TRANSFER_BUFFER)

      todo = CBF_TRANSFER_BUFFER;

    else

      todo = nelem;

    cbf_failnez (cbf_get_block (source, todo))
    
       /* Update the message digest */

    if (todo > 0 && destination->digest)

      MD5Update (destination->digest, source->buffer, todo);

    while (destination->temporary)  {
  
      if (destination->characters_used + todo > destination->characters_size) {
    	
        size_t old_data, old_size;

        char ** fc;
      
        fc = &(destination->characters_base);
    
        old_data = destination->characters-destination->characters_base;
    
        old_size = old_data + destination->characters_size;
    
        if (cbf_realloc ((void **)fc, &old_size, 1, old_size+todo)) {
      
          destination->temporary = 0;
        
          destination->characters = destination->characters_base;
        
          destination->characters_used = old_data;
        
          destination->characters_size = old_size;
        
          cbf_failnez (cbf_flush_characters (destination))
        
          break;
      	
        } else {
      
          destination->characters = destination->characters_base + old_data;
          
          destination->characters_size = old_size-old_data;
        
        }
      
      }
    
      memmove(destination->characters+destination->characters_used,source->buffer,todo);
    
      destination->characters_used += todo;
      
      destination->characters_size -= todo;
      
      done = todo;
    
      break;
  	
    }

    if (!(destination->temporary))
  
      done = fwrite (source->buffer, 1, todo, destination->stream);


      /* Fail? */

    if (done < todo)

      return CBF_FILEWRITE;

    nelem -= done;
  }

  if (destination->temporary) {
  	
    cbf_failnez(cbf_flush_characters(destination))

  }

    /* Success */

  return 0;
}


  /* Get the file position */

int cbf_get_fileposition (cbf_file *file, long int *position)
{
  long int file_position;


    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;

  if (!file->stream)

    return CBF_ARGUMENT;


    /* Get the position */
    
  if (file->temporary) {
  
    file_position = (long int)(file->characters - file->characters_base);
  	
  } else  {

    file_position = ftell (file->stream);

    if (file_position == -1L)

      return CBF_FILETELL;
      
    if (file->characters) {
    	
      file_position -= file->characters_used;
      
      if (file_position < 0 )
      
        return CBF_FILETELL;

    }
      
  }

  if (position)

    *position = file_position;


    /* Success */

  return 0;
}


  /* Set the file position */

int cbf_set_fileposition (cbf_file *file, long int position, int whence)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;

  if (!file->stream)

    return CBF_ARGUMENT;


    /* Set the position */
    
 if (file->temporary)  {
 
   if (whence == SEEK_CUR) position += file->characters-file->characters_base;
   
   if (whence == SEEK_END) position += file->characters-file->characters_base+file->characters_used;
   
   if (position < 0 || position > file->characters-file->characters_base+file->characters_used)
   
     return CBF_FILESEEK;
   
   file->characters_used += file->characters-file->characters_base-position;
   
   file->characters_size += file->characters-file->characters_base-position;
   
   file->characters = file->characters_base + position;
 	
 } else {

   if (file->characters && whence == SEEK_CUR 
     && ((position >= 0 && file->characters_used > position)
       || (position < 0 && file->characters - file->characters_base >  -position)))  {

     file->characters += position;
          
     file->characters_used -= position;
          
     file->characters_size -= position;
     	
   } else {
   
   if (whence == SEEK_CUR) {
   
     position -= file->characters_used;
   	
   }
   	
   if (fseek (file->stream, position, whence) < 0)

     return CBF_FILESEEK;
     
   file->characters_used = 0;
   
   file->characters_size += (file->characters-file->characters_base);
   
   file->characters = file->characters_base;
   
   }
   
 }
 
   file->bits [0] = 0;

   file->bits [1] = 0;



    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif
