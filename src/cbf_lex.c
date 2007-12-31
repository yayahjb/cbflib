/**********************************************************************
 * cbf_lex -- lexical scanner for CBF tokens                          *
 *                                                                    *
 * Version 0.7.9 30 December 2007                                     *
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
#include "cbf_compress.h"
#include "cbf_lex.h"
#include "cbf_codes.h"
#include "cbf_file.h"
#include "cbf_string.h"
#include "cbf_read_binary.h"
#include "cbf_read_mime.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>


  /* Return an error code */

#define cbf_errornez(f,v) { if (((v)->errorcode = (f)) != 0) return ERROR; }


  /* Return a copy of the text */

int cbf_return_text (int code, YYSTYPE *val, const char *text, char type)
{

  val->text = cbf_copy_string (NULL, text, type);

  if (!val->text)
  {
    val->errorcode = CBF_ALLOC;

    return ERROR;
  }

  return code;
}


  /* Get the next token */

int cbf_lex (cbf_handle handle, YYSTYPE *val )
{
  int data, save, loop, item, column, comment, string, ascii,
      c, cprev, cprevprev, reprocess, errorcode, mime, encoding, 
      bits, sign, checked_digest, real;

  long id, position;

  unsigned int file_column, compression;

  size_t size, length=0, code_size, dimover, dimfast, dimmid, dimslow, padding;

  const char *line;
  
  cbf_file *file;

  char out_line [(((sizeof (void *) +
                    sizeof (long int) * 2 +
                    sizeof (int) * 3) * CHAR_BIT) >> 2) + 57
                    +15+((5*sizeof (size_t)*3*CHAR_BIT)>>2)];

  char digest [25], new_digest [25];
  
  const char * byteorder;
  
  file = handle->file;

  cbf_errornez (cbf_reset_buffer (file), val)

  c = file->last_read;

  cprevprev = cprev = ' ';
  
  if (file->column == 0) c = '\n';

  column = c == '.';

  comment = c == '#';

  reprocess = (column || comment);

  data = save = loop = item = string = !reprocess;

  comment = !column;

  do
  {
    cbf_errornez (cbf_get_buffer (file, &line, &length), val)

    if (reprocess) {

      reprocess = 0;

    } else {

      cprevprev = cprev;
      
      cprev = c;

      c = cbf_read_character (file);
      
      if (file->column == file->columnlimit+1) {
      
        cbf_log(handle, "over line size limit", CBF_LOGWARNING|CBF_LOGCURRENTLOC);

      }

      if (isspace (c)) {
      
        if (c== '\013' || c == '\014') 
          cbf_log(handle,"invalid space character",CBF_LOGWARNING|CBF_LOGCURRENTLOC);
      
      }  else {

        if ( c != EOF && ((unsigned char)c > 126 || (unsigned char )c < 32 ) )
          cbf_log(handle,"invalid character",CBF_LOGWARNING|CBF_LOGCURRENTLOC);

      	
      }


    }

      /* Discard spaces ([[:space:]]+)  and mark starting point */


    if (length == 0) {

      if (isspace (c)) 
      {
      
      if (c== '\013' || c == '\014') cbf_log(handle,"invalid space character",CBF_LOGWARNING|CBF_LOGCURRENTLOC);

      continue;
      
      }

      handle->startline = file->line;

      handle->startcolumn = file->column;

    }


       /* DATA ([Dd][Aa][Tt][Aa][_][^[:space:]]*) */

    if (data) {

      if (length < 5) {

         data = toupper (c) == "DATA_" [length];

      } else {
      
        if ( length == 81 ) cbf_log(handle, "data block name exceeds 75 characters",
          CBF_LOGERROR|CBF_LOGSTARTLOC);

        if (isspace (c) || c == EOF)

          return cbf_return_text (DATA, val, &line [5], 0);

      }

    }


       /* SAVE ([Ss][Aa][Vv][Ee][_][^[:space:]]*) */

    if (save) {

      if (length < 5) {

         save = toupper (c) == "SAVE_" [length];

      } else {

        if ( length == 81 ) cbf_log(handle, "save frame name exceeds 75 characters",
          CBF_LOGERROR|CBF_LOGSTARTLOC);
        
        if (isspace (c) || c == EOF) {

          if (length==5) return SAVEEND;

          return cbf_return_text (SAVE, val, &line [5], 0);

        }

      }

    }

       /* LOOP ([Ll][Oo][Oo][Pp][_]) */

    if (loop)
    {
    
      loop = 0;

      if (length < 5) loop = (toupper (c) == "LOOP_" [length]);

      if ((!loop) && (length == 5)) {
      	
        if (isspace(c) || c == EOF) return LOOP;
      
        cbf_log(handle, "\"loop_\" must be followed by white space", 
          CBF_LOGERROR|CBF_LOGSTARTLOC );
          
        if (file->temporary)  {
        
          file->characters--;
          
          file->characters_used++;
          
          file->characters_size++;
        	
        } else  {
        	
          ungetc(c,file->stream);
      
        }
      
        file->column--;
      
        file->last_read='\0';
      
        return LOOP;

      }

    }


       /* ITEM ([_][^[:space:]\.]+) */

    if (item) {

      if (length == 0) {

        item = c == '_';

      } else {

        item = !isspace (c) && c != '.' && c != EOF;

        if (length >= 2 && !item) {

          if (c == '.') {

            if ( length > 74 ) cbf_log(handle, "category name exceeds 73 characters",
              CBF_LOGERROR|CBF_LOGSTARTLOC);
            
            return cbf_return_text (CATEGORY, val, &line [1], 0);
            
          }

          else {
          
            /* if ( length > 75 ) cbf_log(handle, "data item name exceeds 75 characters",
              CBF_LOGERROR|CBF_LOGSTARTLOC); */
           	
            return cbf_return_text (ITEM, val, &line [0], 0);
          }

        }

      }

    }


      /* COLUMN (\.[^[:space:]]+) */

    if (column)

    {

      column = (!isspace(c) && c != EOF);

      if (!column)

        return cbf_return_text (COLUMN, val, &line [1], 0);

    }


      /* STRING ([\'][^'\n]*[\'\n])|(([\"][^"\n]*[\"\n])) */

    if (string) {

      if (length == 0) {

        string = (c == '\'' || c == '"');

      } else {

        /*  The string continues as long as we do not encounter a blank
            after the quote mark in a position after the second position */

        string = ( cprev != line[0] ||
                   length < 2 ||
                   !isspace(c));

        if ( string && ( c == '\n' || c == EOF ) )  {
            
           if (line[0] == '\'') 

              cbf_log(handle,"ended before end of single-quoted string", 
                CBF_LOGWARNING|CBF_LOGSTARTLOC);
            
            else

              cbf_log(handle,"ended before end of double-quoted string", 
                CBF_LOGWARNING|CBF_LOGSTARTLOC);
                
            string = 0;
            
              
        }

        if ( !string )
        {
          if ( cprev == line[0] && file->buffer_used > 0 ) {

            file->buffer_used--;

            file->buffer [file->buffer_used] = '\0';
            
          }

          if (line [0] == '\'')

            return cbf_return_text (STRING, val, &line [1],
                                                  CBF_TOKEN_SQSTRING);

          else

            return cbf_return_text (STRING, val, &line [1],
                                                  CBF_TOKEN_DQSTRING);
        }
        


      }

    }
       /* COMMENT ([#][^\n]*) */

    if (comment) {

      if (length == 0)

        comment = c == '#';

      else

      {
        comment = (c != '\n' && c!= EOF);

        if (! comment)


          return cbf_return_text (COMMENT, val, &line [1], 0);
      }
    }

       /* CBFWORD ([^[:space:]]+) */

    if (!data && !loop && !item && !comment && !string && !column) {

      if (length && (isspace (c) || c == EOF)) {

          /* Missing value? */

        if (length == 1 && (line [0] == '?' || line [0] == '.'))

          return cbf_return_text (CBFWORD, val, &line [0], CBF_TOKEN_NULL);

        else

          return cbf_return_text (CBFWORD, val, &line [0], CBF_TOKEN_WORD);

      }

    }


      /* semicolon-delimited STRING (^;[^\n]*[\n])([^;][^\n]*[\n])*(;) */

    if (length == 0 && c == ';')
    {
      cbf_errornez (cbf_get_filecoordinates (file, NULL, &file_column), val)

      if (file_column == 1)
      {
          /* Save the position */

        cbf_errornez (cbf_get_fileposition (file, &position), val)

        mime = 0;

        do
        {
            /* Save the character */

          cbf_errornez (cbf_save_character (file, c), val)


            /* Check for a Mime boundary */

          if (c == '-')
          {
            cbf_errornez (cbf_get_buffer (file, &line, &length), val)

            cbf_nblen (line, &length);

            if (length > 29)

              mime = cbf_cistrcmp (&line [length - 30],
                                   "\n--CIF-BINARY-FORMAT-SECTION--")
                                    == 0;
          }


            /* Read the next character */

          cprevprev = cprev; cprev = c;

          c = cbf_read_character (file);

          ascii = isgraph (c) || isspace (c);
          
          if (!ascii && (unsigned char)c!=12 && (unsigned char)c!=26 && (unsigned char)c!=4 && c != EOF) 
          {
          	cbf_log(handle,"invalid character in text field", CBF_LOGWARNING|CBF_LOGCURRENTLOC);
          	
          	ascii = 1;
          }
          
          if (ascii) {

            if (file->column == file->columnlimit+1) {
      
            cbf_log(handle, "over line size limit", CBF_LOGWARNING|CBF_LOGCURRENTLOC);


            }
          	
          }

        }
        while ((cprevprev != '\n' 
               || cprev != ';'
               || file->buffer_used < 3 
               || !(isspace(c) || c==EOF)) 
             && !mime && ascii && c != EOF);

        if ( c == EOF && (cprev != ';' || cprevprev != '\n')) {
        
          cbf_log(handle, "text field terminated by EOF", CBF_LOGERROR|CBF_LOGCURRENTLOC);
        	
        }
          /* Plain ASCII string or terminated by EOF */

        if ((!mime && ascii) || c==EOF)
        {
          cbf_errornez (cbf_get_buffer (file, &line, &length), val)

          ((char *) line) [(length>2)?(length - 2):1] = '\0';

          return cbf_return_text (STRING, val, &line [1],
                                                  CBF_TOKEN_SCSTRING);
        }

        encoding = ENC_NONE;

        bits = 0;

        sign = -1;

        real = -1;

        checked_digest = 0;


          /* Mime header */

        if (mime)
        {
            /* Position */

          cbf_errornez (cbf_get_fileposition (file, &position), val)


            /* Read the header */
            
          dimover=dimfast=dimmid=dimslow=padding = 0;
          
          byteorder="little_endian";

          cbf_errornez (cbf_parse_mimeheader (file, &encoding,
                                                    &size,
                                                    &id,
                                                    digest,
                                                    &compression,
                                                    &bits,
                                                    &sign,
                                                    &real,
                                                    &byteorder,
                                                    &dimover,
                                                    &dimfast, &dimmid, &dimslow,
                                                    &padding), val);


            /* Check the digest? */

          if ((file->read_headers & MSG_DIGESTNOW) &&
                                    cbf_is_base64digest (digest))
          {
              /* Recalculate the digest (note that this will decode the
                 binary section but not save the result so this section
                 is not very efficient) */

            code_size = 0;

            switch (encoding)
        {
              case ENC_QP:

                cbf_errornez (cbf_fromqp (file, NULL, size, &code_size,
                                                         new_digest), val)

                break;

              case ENC_BASE64:

                cbf_errornez (cbf_frombase64 (file, NULL, size, &code_size,
                                                         new_digest), val)

                break;

              case ENC_BASE32K:

		        cbf_errornez (cbf_frombase32k (file, NULL, size, &code_size, new_digest), val)
		
                break;


              case ENC_BASE8:
              case ENC_BASE10:
              case ENC_BASE16:

                cbf_errornez (cbf_frombasex (file, NULL, size, &code_size,
                                                         new_digest),val)

                break;

          case ENC_NONE:

                cbf_errornez (cbf_parse_binaryheader (file, NULL, \
                                                            NULL, \
                                                            NULL, \
                                                            mime), val)

                code_size = size;

                cbf_errornez (cbf_get_fileposition (file, &position), val)

                cbf_errornez (cbf_md5digest (file, code_size, new_digest),
                                                              val)

                break;

             default:

               cbf_errornez (CBF_FORMAT, val)
            }


              /* Check the number of characters read */

            if ((size && (size != code_size)) || code_size == 0)

              cbf_errornez (CBF_FORMAT, val)


              /* Compare the old digest to the new one */

            if (strcmp (digest, new_digest) != 0)

              cbf_errornez (CBF_FORMAT | 2, val)

            checked_digest = 1;
          }
          else
          {
              /* Calculate the minimum number of characters in the data */

            if (encoding == ENC_NONE)
            {
              cbf_errornez (cbf_parse_binaryheader (file, NULL, NULL, NULL, \
                                                                  mime), val)

              cbf_errornez (cbf_get_fileposition (file, &position), val)

              code_size = size;
            }
            else

              if (encoding == ENC_QP)

                code_size = size;

              else

                if (encoding == ENC_BASE64)

                  code_size = size * 8 / 6;
                  
                else

                  if (encoding == ENC_BASE32K)


		            code_size = size * 16 / 15;

                  else

                    code_size = size / 4;


              /* Skip to the end of the data */

            cbf_errornez (cbf_set_fileposition (file, code_size+padding, SEEK_CUR),
                                                      val)
          }
        }
        else
        {
            /* Simple binary */

          cbf_errornez (cbf_parse_binaryheader (file, &size, \
                                                      &id,   \
                                                      &compression, mime), val)

          cbf_errornez (cbf_get_fileposition (file, &position), val)

          code_size = size;


            /* Skip to the end of the data */

          cbf_errornez (cbf_set_fileposition (file, code_size, SEEK_CUR), val)
        }


          /* Find the terminating semi-colon */

        cprev = c = 0;

        do
        {
          if (c==EOF) break;
  
          cprevprev = cprev;
  
          cprev = c;

          c = cbf_read_character (file);

          if (c == EOF && (cprevprev != '\n' ||  cprev != ';')) {

            cbf_log(handle, "text field terminated by EOF", CBF_LOGERROR|CBF_LOGCURRENTLOC);

            cbf_errornez (CBF_FILEREAD, val)
          }
        }
        while (cprevprev != '\n' || cprev != ';' || !(isspace(c)||c==EOF));


          /* Check the element size and sign */

        if (bits < 0 || bits > 64)

          cbf_errornez (CBF_FORMAT, val)

        if (bits == 0)

          bits = 32;

        if (sign == -1)

          sign = 1;


          /* Add a connection */

        cbf_errornez (cbf_add_fileconnection (&file, NULL), val)


          /* Code the id, file, position, size and digest */

        if (!cbf_is_base64digest (digest))

          strcpy (digest, "------------------------");

        sprintf (out_line, "%lx %p %lx %lx %d %s %x %d %d %s %ld %ld %ld %ld %ld %u",
                            id, (void *)file, position, (unsigned long) size, checked_digest,
                            digest, bits, sign, real<1?0:1, 
                            byteorder, (unsigned long)dimover, 
                            (unsigned long)dimfast, 
                            (unsigned long)dimmid, 
                            (unsigned long)dimslow, 
                            (unsigned long)padding,
                            compression);

        if (encoding == ENC_NONE)

          errorcode = cbf_return_text (BINARY, val, out_line,
                                                      CBF_TOKEN_BIN);

        else

          errorcode = cbf_return_text (BINARY, val, out_line,
                                                      CBF_TOKEN_MIME_BIN);

        if (errorcode == ERROR)

          val->errorcode |= cbf_delete_fileconnection (&file);

        return errorcode;
      }
    }


      /* Add the character to the text */

    errorcode = cbf_save_character (file, c);

    cbf_errornez (errorcode, val);
  }
  while (c != EOF);

  return 0;
}


#ifdef __cplusplus

}

#endif
