/**********************************************************************
 * cbf_read_mime -- read MIME-encoded binary sections                 *
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

/**********************************************************************
 * The following notice applies to some portions of this software     *
 * which were derived in part from the routine decode.c in mpack      *
 *                                                                    *
 * (C) Copyright 1993,1994 by Carnegie Mellon University              *
 * All Rights Reserved.                                               *
 *                                                                    *
 * Permission to use, copy, modify, distribute, and sell this         *
 * softwareand its documentation for any purpose is hereby granted    *
 * without fee, provided that the above copyright notice appear in    *
 * all copies and that both that copyright notice and this permission *
 * notice appear in supporting documentation, and that the name of    *
 * Carnegie Mellon University not be used in advertising or publicity *
 * pertaining to distribution of the software without specific,       *
 * written prior permission.  Carnegie Mellon University makes no     *
 * representations about the suitability of this software for any     *
 * purpose.  It is provided "as is" without express or implied        *
 * warranty.                                                          *
 *                                                                    *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO *
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY *
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE       *
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY   *
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,    *
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS     *
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR            *
 * PERFORMANCE OF THIS SOFTWARE.                                      *
 **********************************************************************/

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_file.h"
#include "cbf_context.h"
#include "cbf_binary.h"
#include "cbf_codes.h"
#include "cbf_read_mime.h"
#include "cbf_string.h"

#include <ctype.h>
#include <string.h>


  /* Convert a MIME-encoded binary section to a temporary binary section */

int cbf_mime_temp (cbf_node *column, unsigned int row)
{
  cbf_file *file;
  
  cbf_file *temp_file;
  
  long start, temp_start;
  
  size_t size;
  
  int id, bits, sign, type, checked_digest;

  unsigned int compression;
  
  char old_digest [25], *new_digest, digest [25];

  
    /* Check the value */

  if (!cbf_is_mimebinary (column, row))

    return CBF_ASCII;


    /* Parse it */

  size = 0;
  
  cbf_failnez (cbf_get_bintext (column, row, &type,
                                &id, &file, &start, &size, &checked_digest, 
                                old_digest, &bits, &sign, &compression))
  

    /* Position the file at the start of the mime section */

  cbf_failnez (cbf_set_fileposition (file, start, SEEK_SET))


    /* Get the temporary file */

  cbf_failnez (cbf_open_temporary (column->context, &temp_file))


    /* Move to the end of the temporary file */

  cbf_onfailnez (cbf_set_fileposition (temp_file, 0, SEEK_END),
                 cbf_delete_fileconnection (&temp_file))


    /* Get the starting location */

  cbf_onfailnez (cbf_get_fileposition (temp_file, &temp_start),
                 cbf_delete_fileconnection (&temp_file))
    

    /* Calculate a new digest if necessary */
    
  if (cbf_is_base64digest (old_digest) && (file->read_headers & MSG_DIGEST) 
                                       && !checked_digest)

    new_digest = digest;
    
  else
  
    new_digest = NULL;
    

    /* Decode the binary data to the temporary file */
    
  cbf_onfailnez (cbf_read_mime (file, temp_file, 
                                      NULL, NULL, old_digest, new_digest),
                 cbf_delete_fileconnection (&temp_file))


    /* Check the digest */
    
  if (new_digest)
  
    if (strcmp (old_digest, new_digest) == 0)
    
      checked_digest = 1;
      
    else
                 
      return CBF_FORMAT | cbf_delete_fileconnection (&temp_file);

  
    /* Replace the connection */
    
  cbf_onfailnez (cbf_set_bintext (column, row, CBF_TOKEN_TMP_BIN,
                                  id, temp_file, temp_start, size, 
                                  checked_digest, old_digest, bits,
                                                              sign,
                                                              compression),
                    cbf_delete_fileconnection (&temp_file))
 

    /* Success */
    
  return 0;
}


  /* Convert a MIME-encoded binary section to a normal binary section */
     
int cbf_read_mime (cbf_file *infile, cbf_file   *outfile,
                                     size_t     *size,
                                     long       *id,
                                     char       *old_digest,
                                     char       *new_digest)
{
  int encoding;
  
  size_t file_size;

  unsigned int compression;

  
    /* Read the header */
    
  encoding = 0;
  
  file_size = 0;
    
  cbf_failnez (cbf_parse_mimeheader (infile, &encoding, 
                                             &file_size, id, 
                                             old_digest,
                                             &compression,
                                             NULL, NULL))
                                             
  if (file_size <= 0)

    return CBF_FORMAT;


    /* Discard any bits in the buffers */
    
  cbf_failnez (cbf_reset_bits (outfile))


    /* Decode the binary data */
    
  switch (encoding)
  {
    case ENC_QP:
    
      cbf_failnez (cbf_fromqp (infile, outfile, file_size, NULL, 
                               new_digest))

      break;
      
    case ENC_BASE64:
    
      cbf_failnez (cbf_frombase64 (infile, outfile, file_size, NULL, 
                                   new_digest))

      break;
      
    case ENC_BASE8:
    case ENC_BASE10:
    case ENC_BASE16:
    
      cbf_failnez (cbf_frombasex (infile, outfile, file_size, NULL, 
                                  new_digest))

      break;
      
    default:
    
      return CBF_FORMAT;
  }


    /* Flush the buffers */

  cbf_failnez (cbf_flush_bits (outfile))


    /* Size (excluding the encoding) */

  if (size)
  
    *size = file_size;
    
    
    /* Success */
    
  return 0;
}


  /* Is the line blank? */

int cbf_is_blank (const char *line)
{
  if (line)
  
    for (; *line; line++)
    
      if (!isspace (*line))
      
        return 0;
        
  return 1;
}

/* Find non-blank length of a line */

int cbf_nblen (const char *line, int *nblen)
{
  register char *myline;

  register int mylen;

  *nblen = mylen = 0;

  

  if (!(myline = (char *)line)) return 1;

  for (; *myline; myline++)

    if (!isspace (*myline)) mylen = myline-(char *)line+1;

  *nblen = mylen;

  return 0;
  
}


  /* Skip whitespace and comments */ 

int cbf_skip_whitespace (cbf_file *file, const char **line, 
                                         const char **curpoint, 
                                         int        *freshline)
{
  static const char end = '\0';
  
  const char *c;     

  int comment_level;


    /* Repeating the end of a line? */

  if (*freshline)
  {
    *curpoint = &end;
    
    return 0;
  }

  c = *curpoint;

  comment_level = 0;
  
  while (isspace (*c) || *c == '(' || *c == '\0')

    if (*c == '\0')
    {
      cbf_failnez (cbf_read_line (file, line))

      c = *line;

      if (cbf_is_blank (c) || (*c != ' ' && *c != '\t'))
      {
        *freshline = 1;

        *curpoint = &end;

        return 0;
      }
    } 
    else 
    
      if (*c == '(')
      {
        c++;

        comment_level++;

        while (comment_level)
        {
          switch (*c)
          {
            case '\0':

              cbf_failnez (cbf_read_line (file, line))

              c = *line;

              if (cbf_is_blank (c) || (*c != ' ' && *c != '\t'))
              {
                *freshline = 1;

                *curpoint = &end;

                return 0;
              }

              break;

            case '\\':

              c++;

              break;

            case '(':              

              comment_level++;                

              break;

            case ')':

              comment_level--;

              break;
          }

          c++;
        }
      }
      else  
      
        c++;

  *freshline = 0;

  *curpoint = c;


    /* Success */
    
  return 0;
}
  

  /* Parse the MIME header looking for values of type:
  
     Content-Type:
     Content-Transfer-Encoding:
     X-Binary-Size:
     X-Binary-ID:
     X-Binary-Element-Type:
     Content-MD5: */
     
int cbf_parse_mimeheader (cbf_file *file, int        *encoding,
                                          size_t     *size,
                                          long       *id,
                                          char       *digest,
                                 unsigned int        *compression,
                                          int        *bits,
                                          int        *sign)
{
  static const char *value [] = {
  
    "Content-Type:",                /* State 0 */
    "Content-Transfer-Encoding:",   /* State 1 */
    "X-Binary-Size:",               /* State 2 */
    "X-Binary-ID:",                 /* State 3 */
    "X-Binary-Element-Type:",       /* State 4 */
    "Content-MD5:"                  /* State 5 */
  
    };

  const char *line, *c;
  
  int state, continuation, item, line_count, fresh_line, quote, text_bits, 
      count, failure, nblen;
  
  
    /* Defaults */
    
  if (encoding)
  
    *encoding = 0;
  
  if (size)
  
    *size = 0;
  
  if (id)
  
    *id = 0;
  
  if (digest)
  
    *digest = '\0';

  if (compression)

    *compression = CBF_NONE;
    
  if (bits)
  
    *bits = 0;
    
  if (sign)
  
    *sign = -1;
  
  
    /* Read the file line by line */
    
  state = -1;
  
  line_count = 0;

  fresh_line = 0;

  nblen = 1;

  while (nblen)
  {
    if (!fresh_line)
    
      cbf_failnez (cbf_read_line (file, &line))

    cbf_nblen(line, &nblen);
      
    fresh_line = 0;

    line_count++;

     /* Check for premature terminations */
 
    if ( (line[0] == ';') || 
      ( cbf_cistrncmp(line,"--CIF-BINARY-FORMAT-SECTION--",29) == 0 ) )

      return CBF_FORMAT;


      /* Check for a header continuation line */
      
    continuation = line [0] == ' ' || line [0] == '\t';


      /* Check for a new item */
      
    if (continuation)

      item = 0;
      
    else
    {
      for (c = line; *c != ':' && *c > 32 && *c < 127; c++);
      
      item = c != line && *c == ':';
    }
          

      /* Check for the end of the header */

    if (line_count > 1 && cbf_is_blank (line))
            
      return 0;


      /* Check for valid header-ness of line */
      
    if (!item && (line_count == 1 || !continuation))

      return CBF_FORMAT;
        

       /* Look for the entries we are interested in */
      
    c = line;

    if (item)

      for (state = 5; state > -1; state--)

        if (cbf_cistrncmp (line, value [state], strlen (value [state])) 
                           == 0)

        {
          c = line + strlen (value [state]);
          
          break;
        }
       

      /* Skip past comments and whitespace */
        
    cbf_failnez (cbf_skip_whitespace (file, &line, &c, &fresh_line))
      

      /* Get the value */
      
    switch (state)
    {
      case 0:
          
          /* Content */
              
        if (cbf_cistrncmp (c, "application/", 12) != 0 &&
            cbf_cistrncmp (c, "image/",        6) != 0 &&
            cbf_cistrncmp (c, "text/",         5) != 0 &&
            cbf_cistrncmp (c, "audio/",        6) != 0 &&
            cbf_cistrncmp (c, "video/",        6) != 0)

          return CBF_FORMAT;

              
        while (*c)
        {
            /* Skip to the end of the section (a semicolon) */
              
          while (*c)
              
            if (*c == '\"')
            {
              c++;

              while (*c)

                if (*c == '\"')
                {
                  c++;

                  break;
                }
                else
                {
                  if (*c == '\\')

                    c++;

                  if (*c)

                    c++;
                }
            }
            else 

              if (*c == '(')

                cbf_failnez (cbf_skip_whitespace (file, &line, &c, 
                                                        &fresh_line))

              else

                if (*c == ';')
                {
                  c++;

                  break;
                }
                else

                  c++;

            
            /* We are at the end of the section or the end of the item */
              
          cbf_failnez (cbf_skip_whitespace (file, &line, &c, 
                                                  &fresh_line))

          if (cbf_cistrncmp (c, "conversions", 11) == 0) 
          {
            c += 11;

            cbf_failnez (cbf_skip_whitespace (file, &line, &c, 
                                                    &fresh_line))

            if (*c == '=') 
            {
              c++;

              cbf_failnez (cbf_skip_whitespace (file, &line, &c, 
                                                      &fresh_line))

              if (compression) 
              {
                quote = 0;

                if (*c == '\"')
                      
                  quote = 1;
                      
                *compression = CBF_NONE;

                if (cbf_cistrncmp (c + quote, "x-cbf_packed", 12) == 0)

                  *compression = CBF_PACKED;

                if (cbf_cistrncmp (c + quote, "x-cbf_canonical", 15) == 0)
                      
                  *compression = CBF_CANONICAL;

                if (cbf_cistrncmp (c + quote, "x-cbf_byte_offset", 17) == 0)
  
                  *compression = CBF_BYTE_OFFSET;

                if (cbf_cistrncmp (c + quote, "x-cbf_predictor", 15) == 0)

                  *compression = CBF_PREDICTOR;
              }
            }
          }
        }
          
      state = -1;
          
      break;

      case 1:
        
          /* Binary encoding */
              
        if (encoding)
        {
           failure = 1;

           quote = 0;

           if (*c == '\"')
                      
              quote = 1;

          if (cbf_cistrncmp (c+quote, "Quoted-Printable", 16) == 0)
              
            if (isspace (c [16]) || c [16] == '(' 
              || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_QP;
            }
                
          if (cbf_cistrncmp (c+quote, "Base64", 6) == 0)
              
            if (isspace (c [6]) || c [6] == '(' || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_BASE64;
            }
                
          if (cbf_cistrncmp (c+quote, "X-Base8", 7) == 0)

            if (isspace (c [7]) || c [7] == '(' || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_BASE8;
            }
                
          if (cbf_cistrncmp (c+quote, "X-Base10", 8) == 0)
              
            if (isspace (c [8]) || c [8] == '(' || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_BASE10;
            }
                
          if (cbf_cistrncmp (c+quote, "X-Base16", 8) == 0)
              
            if (isspace (c [8]) || c [8] == '(' || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_BASE16;
            }

          if (cbf_cistrncmp (c+quote, "7bit", 4) == 0 ||
              cbf_cistrncmp (c+quote, "8bit", 4) == 0)
              
            if (isspace (c [4]) || c [4] == '(' || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_NONE;
            }

          if (cbf_cistrncmp (c+quote, "Binary", 6) == 0)
              
            if (isspace (c [6]) || c [6] == '(' || (quote && c [16] == '\"')) {

              failure = 0;
              
              *encoding = ENC_NONE;
            }
        }

        if (failure) return CBF_FORMAT;
        
        break;
          
      case 2:
          
          /* Binary size */
            
        if (size)
          
          *size = atol (c);
            
        break;
          
      case 3:

          /* Binary ID */
          
        if (id)

          *id = atol (c);
              
        break;
            
      case 4:
          
          /* Binary element type (signed/unsigned ?-bit integer) */

        failure = 3;

        while (*c)
        {
          quote = 0;

          cbf_failnez (cbf_skip_whitespace (file, &line, &c, 
                                                  &fresh_line))
	  if (*c == '\"') {

            if (quote) break;

            c++;

            quote++;
	  }
          
          if (failure == 3) {

            if (cbf_cistrncmp (c, "signed", 6) == 0)
            {
              c += 6;
            
              if (sign) *sign = 1;

              failure --;
            }
          
            if (cbf_cistrncmp (c, "unsigned", 8) == 0)
            {
              c += 8;
              
              if (sign) *sign = 0;

              failure --;
            }
          }
          
          if (failure == 2) {

            count = 0;
            
            sscanf (c, "%d-%n", &text_bits, &count);
              
            if (cbf_cistrncmp (c+count, "bit", 3 ) == 0)

              if (count && text_bits > 0 && text_bits <= 64)
              {
                c += count;
            
                if (bits) *bits = text_bits;

                failure --;
              }
          }

          if (failure == 1) {

            if (cbf_cistrncmp (c, "integer", 7 ) == 0) failure--;

          }

          if (*c)
          
            c++;
        }
        
        if (failure) return CBF_FORMAT;
 
        break;
          
      case 5:

          /* Message digest */
            
        if (digest)
        {
          strncpy (digest, c, 24);
              
          digest [24] = '\0';
        }
            
        break;
    }
  }


    /* Success */
    
  return 0;      
}


#ifdef __cplusplus

}

#endif
