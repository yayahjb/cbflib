/**********************************************************************
 * cbf_lex -- lexical scanner for CBF tokens                          *
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
