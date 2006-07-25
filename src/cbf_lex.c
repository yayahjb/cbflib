/**********************************************************************
 * cbf_lex -- lexical scanner for CBF tokens                          *
 *                                                                    *
 * Version 0.7.2 22 April 2001                                        *
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

int cbf_lex (YYSTYPE *val, cbf_file *file)
{
  int data, loop, item, column, comment, string, ascii, 
      l, c, count, reprocess, errorcode, mime, encoding, bits, sign,
      checked_digest;

  long id, position;
  
  unsigned int file_column, compression;
  
  size_t size, length, code_size;
  
  const char *line;
    
  char out_line [(((sizeof (void *) +
                    sizeof (long int) * 2 +
                    sizeof (int) * 3) * CHAR_BIT) >> 2) + 55];

  char digest [25], new_digest [25];


  cbf_errornez (cbf_reset_buffer (file), val)
  
  l = c = file->last_read;
  
  column = c == '.';
  
  comment = c == '#';
  
  reprocess = (column || comment);
  
  data = loop = item = string = !reprocess;
  
  comment = !column;
  
  do
  {
    cbf_errornez (cbf_get_buffer (file, &line, &length), val)
    
    if (reprocess)

      reprocess = 0;

    else
    {
      l = c;

      c = cbf_read_character (file);
    }
    

      /* Discard spaces ([[:space:]]+) */

    if (length == 0)

      if (isspace (c))

         continue;
            
        
       /* DATA ([Dd][Aa][Tt][Aa][_][^[:space:]]*) */
    
    if (data)

      if (length < 5)

         data = toupper (c) == "DATA_" [length];

      else

        if (isspace (c) || c == EOF)

          return cbf_return_text (DATA, val, &line [5], 0);
   
   
       /* LOOP ([Ll][Oo][Oo][Pp][_]) */
     
    if (loop)
    {
      loop = toupper (c) == "LOOP_" [length];

      if (loop && length == 4)

        return LOOP;
    }

   
       /* ITEM ([_][^[:space:]\.]+) */
     
    if (item)

      if (length == 0)

        item = c == '_';

      else
      {
        item = !isspace (c) && c != '.' && c != '#' && c != EOF;

        if (length >= 2 && !item)

          if (c == '.')

            return cbf_return_text (CATEGORY, val, &line [1], 0);
            
          else

            return cbf_return_text (ITEM, val, &line [1], 0);
      }

   
      /* COLUMN (\.[^[:space:]]+) */
     
    if (column)

      if (isspace (c) || c == EOF)

        return cbf_return_text (COLUMN, val, &line [1], 0);

  
      /* STRING ([\'][^'\n]*[\'\n])|(([\"][^"\n]*[\"\n])) */
     
    if (string)

      if (length == 0)

        string = c == '\'' || c == '"';

      else

        if (c == line [0] || c == '\n' || c == EOF)

          if (line [0] == '\'')

            return cbf_return_text (STRING, val, &line [1], 
                                                  CBF_TOKEN_SQSTRING);
            
          else

            return cbf_return_text (STRING, val, &line [1],
                                                  CBF_TOKEN_DQSTRING);


       /* COMMENT ([#][^\n]*) */
     
    if (comment)

      if (length == 0)

        comment = c == '#';

      else

        if (c == '\n' || c == EOF)

          return cbf_return_text (COMMENT, val, &line [1], 0);


       /* WORD ([^[:space:]]+) */
     
    if (!data && !loop && !item && !comment && !string && !column)

      if (length && (isspace (c) || c == EOF))

          /* Missing value? */

        if (length == 1 && (line [0] == '?' || line [0] == '.'))
          
          return cbf_return_text (WORD, val, &line [0], CBF_TOKEN_NULL);
          
        else
        
          return cbf_return_text (WORD, val, &line [0], CBF_TOKEN_WORD);


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
            
          l = c;
         
          c = cbf_read_character (file);
          
          ascii = isgraph (c) || isspace (c);
        }
        while ((l != '\n' || c != ';') && !mime && ascii);


          /* Plain ASCII string */
          
        if (!mime && ascii)
        {
          cbf_errornez (cbf_get_buffer (file, &line, &length), val)
        
          ((char *) line) [length - 1] = '\0';


            /* Convert "\n\\;" -> "\n;" */

          for (count = 0; line [count]; count++)

            if (strncmp (&line [count], "\n\\;", 3) == 0)

              memmove ((void *) &line [count + 1], 
                       (void *) &line [count + 2], length - count - 2);

          return cbf_return_text (STRING, val, &line [1], 
                                                  CBF_TOKEN_SCSTRING);
        }
    
        encoding = ENC_NONE;
          
        bits = 0;
        
        sign = -1;
        
        checked_digest = 0;
        

          /* Mime header */
          
        if (mime)
        {
            /* Position */
          
          cbf_errornez (cbf_get_fileposition (file, &position), val)
          
        
            /* Read the header */

          cbf_errornez (cbf_parse_mimeheader (file, &encoding,
                                                    &size,
                                                    &id,
                                                    digest,
                                                    &compression,
                                                    &bits,
                                                    &sign), val)


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
          
                  code_size = size / 4;


              /* Skip to the end of the data */

            cbf_errornez (cbf_set_fileposition (file, code_size, SEEK_CUR), 
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

        c = 0;
          
        do
        {
          l = c;
        
          c = cbf_read_character (file);
          
          if (c == EOF)
          
            cbf_errornez (CBF_FILEREAD, val)
        }
        while (l != '\n' || c != ';');


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
          
        sprintf (out_line, "%x %p %lx %lx %d %s %x %d %u", 
                            id, file, position, size, checked_digest, 
                            digest, bits, sign, compression);
        
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
