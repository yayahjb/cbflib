/**********************************************************************
 * cbf_read_mime -- read MIME-encoded binary sections                 *
 *                                                                    *
 * Version 0.4 15 November 1998                                       *
 *                                                                    *
 *       Herbert J. Bernstein (yaya@bernstein-plus-sons.com) and      *
 *             Paul Ellis (ellis@ssrl.slac.stanford.edu)              *
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
  cbf_file *infile;
  
  cbf_file *tempfile;
  
  long start;
  
  long size;
  
  long id;
  
  const char *intext;

  unsigned int compression;
  
  int errorcode;

  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int)) * CHAR_BIT) >> 2) + 16];
                
  char digest [25];

  
    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&intext, column, row))

  if (!intext)

    return CBF_ASCII;

  
    /* Parse the value */

  if (*intext != CBF_TOKEN_MIME_BIN)

    return CBF_ARGUMENT;

  size = 0;

  sscanf (intext + 1, " %lx %p %lx %lx", &id, &infile, &start, &size);
  
  if (size == 0 || !infile)

    return CBF_FORMAT;


    /* Position the file at the start of the mime section */

  cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))


    /* Get the temporary file */

  cbf_failnez (cbf_open_temporary (column->context, &tempfile))


    /* Move to the end of the temporary file */

  if (cbf_set_fileposition (tempfile, 0, SEEK_END))

    return CBF_FILESEEK | cbf_delete_fileconnection (&tempfile);


    /* Get the starting location */

  if (cbf_get_fileposition (tempfile, &start))

    return CBF_FILETELL | cbf_delete_fileconnection (&tempfile);
    

    /* Decode the binary data to the temporary file */
    
  digest [0] = '\0';

  cbf_onfailnez (cbf_read_mime (infile, tempfile, NULL, NULL, digest),
                 cbf_delete_fileconnection (&tempfile))
                 

    /* Replace the connection */
    
  sprintf (text, "%lx %p %lx %lx", id, tempfile, start, size);

  intext = cbf_copy_string (NULL, text, CBF_TOKEN_TMP_BIN);

  if (intext)
  {
    errorcode = cbf_set_columnrow (column, row, intext);

    if (errorcode)
    {
      cbf_free_string (NULL, intext);

      return errorcode | cbf_delete_fileconnection (&tempfile);
    }
  }


    /* Success */
    
  return 0;
}


  /* Convert a MIME-encoded binary section to a normal binary section */
     
int cbf_read_mime (cbf_file *infile, cbf_file   *outfile,
                                     size_t     *size,
                                     long       *id,
                                     char       *digest)
{
  int encoding;
  
  size_t file_size;

  unsigned int compression;

  char infile_digest[25];
  
  
    /* Read the header */
    
  encoding = 0;
  
  file_size = 0;
    
  cbf_failnez (cbf_parse_mimeheader (infile, &encoding, 
                                             &file_size, id, digest,
                                             &compression))
                                             
  if (file_size <= 0)
  
    return CBF_FORMAT;

  cbf_failnez (cbf_put_integer (outfile, (int) compression, 0, 64))
  
  
    /* Decode the binary data */
    
  switch (encoding)
  {
    case ENC_QP:
    
      cbf_failnez (cbf_fromqp \
        (infile, outfile, file_size, NULL, infile_digest))

      break;
      
    case ENC_BASE64:
    
      cbf_failnez (cbf_frombase64 \
        (infile, outfile, file_size, NULL, infile_digest))

      break;
      
    case ENC_BASE8:
    case ENC_BASE10:
    case ENC_BASE16:
    
      cbf_failnez (cbf_frombasex \
        (infile, outfile, file_size, NULL, infile_digest))

      break;
      
    default:
    
      return CBF_FORMAT;
  }

  if (digest[0] &&
   (infile->read_headers & MSG_DIGEST) &&
   (strcmp (digest, infile_digest) != 0)) return CBF_FORMAT;

  if (size)
  
    *size = file_size+8;
    
    
    /* Success */
    
  return 0;
}

  /* Skip whitespace and comments in a MIME header */  
  /* Derived from mpack routine SkipWhitespace     */ 

  /* 
     line is a pointer to a pointer to a null-terminated single line
     curpoint is a pointer to a pointer to the current position in line
     nblen is a pointer to the non-blank length of line
     freshline is a pointer to a logical, 1 if a fresh line is loaded
  */
     

int cbf_skip_whitespace (cbf_file *file, char **line, 
      char **curpoint, unsigned int *nblen, int *freshline )

{
      char *c = *curpoint;     

      int  comment_level = 0;

      if (freshline) *freshline = 0;

      while (isspace (*c)|| *c == '\n' || *c == '\r' 
             || *c == '(' || *c == '\0') {

        if (c >= (*line)+(*nblen) || *c == '\0') {

          *curpoint = 0;

          cbf_failnez (cbf_read_line (file, (const char **)line, nblen))

          *curpoint = c = *line;

          if (*nblen==0 || (*c != ' ' && *c != '\t')) {

            if (freshline) *freshline = 1;

            return 0;

          }

        } else if (*c == '(') {

          c++;

          comment_level++;

          while (comment_level) {

            switch (*c) {

	      case '\0':

                *curpoint = 0;

                cbf_failnez (cbf_read_line (file, (const char **)line, nblen))

                *curpoint = c = *line;

                if (*nblen==0 || (*c != ' ' && *c != '\t')) {

                  if (freshline) *freshline = 1;

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

        else  c++;

      }

      if (c >= (*line)+(*nblen) || *c == '\0' ) {

        *curpoint = 0;

        cbf_failnez (cbf_read_line (file, (const char **)line, nblen))

        *curpoint = c = *line;

        if (freshline) *freshline = 1;

      } else {

        *curpoint = c;

      }

      return 0;

}
  

  /* Parse the MIME header looking for values of type:
  
     Content-Type:
     Content-Transfer-Encoding:
     X-Binary-Size:
     X-Binary-ID:
     Content-MD5: */
     
int cbf_parse_mimeheader (cbf_file *file, int        *encoding,
                                          size_t     *size,
                                          long       *id,
                                          char       *digest,
                                 unsigned int        *compression)
{
  static const char *value [] = {
  
    "Content-Type:",
    "Content-Transfer-Encoding:",
    "X-Binary-Size:",
    "X-Binary-ID:",
    "Content-MD5:"
  
    };

  char *line, *c;
  
  int state, comment_level, line_count, i,  freshline, quoteoff;

  unsigned int nblen;
  
  
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
  
  
    /* Read the file line by line */
    
  state = -1;
  
  comment_level = 0;

  line_count = 0;

  freshline = 0;

    
  do
  {

    if (! freshline ) cbf_failnez (cbf_read_line \
      (file, (const char **)&line, &nblen))

    freshline = 0;
    
    line_count++;

    /* Check for valid header-ness of line */

            if (nblen == 0) return 0;

	    for (i = 0; i < nblen; i++) {

		if (line[i] == ':' ||
		    line[i] <= ' ' || line[i] >= '\177') break;
	    }

	    if (i == 0 || line[i] != ':') {

		/* Check for header continuation line */

		if ((line_count == 1 && i == 0) ||
                  (line[0] != ' ' && line[0] != '\t')) {

		    /* Not a valid header stop reading input. */
		    
		    return CBF_FORMAT;
		}
	    }



      /* Continuation line? */

    if (state != -1)

      if ( nblen > 0 && isspace (line [0]))

        c = line;

      else

        state = -1;


      /* Look for the entries we are interested in */
      
    if (state == -1)

      for (state = 4; state > -1; state--)

        if (cbf_cistrncmp (line, value [state], strlen (value [state])) 
                           == 0)

        {
          c = line + strlen (value [state]);

          break;
        }
        
        
    if (state != -1)
    {
        /* Skip past comments and whitespace */
        
      cbf_failnez(cbf_skip_whitespace (file, &line, &c, &nblen, &freshline ))
     
        /* Get the value */
      
      if (*c)
      {
        switch (state)
        {
          case 0:
        
              /* Content */
              
            if ((cbf_cistrncmp (c, "application/", 12) != 0)
              && ( cbf_cistrncmp(c, "image/", 6) != 0) 
              && ( cbf_cistrncmp(c, "text/", 5) != 0 )
              && ( cbf_cistrncmp(c, "audio/", 6) != 0 )
              && ( cbf_cistrncmp(c, "video/", 6) != 0 ) ) {   

              return CBF_FORMAT;

            } else {

              /* Scan for conversions */

              freshline = 0;

              while (c && (c < line+nblen) && *c && *c != ';' && 
                     (!freshline) ) {

                while (*c && !isspace(*c) && *c != '(' && *c != ';') {

                  c++;

                }

                cbf_failnez(cbf_skip_whitespace \
                   (file, &line, &c, &nblen, &freshline))

	      }

              /* we are either positioned at a semicolon, or
                 at the start of a new line.  A semicolon at the start of
                 a line would mark the end of a text section             */

              if (c && (!freshline) && c != line && *c == ';') {

                c++;

                /* scan for parameters    */

                while ( *c ) {

                  cbf_failnez(cbf_skip_whitespace \
                    (file, &line, &c, &nblen, &freshline))

                  if ( freshline || ( !c ) || (c == line) ) break;

                  if (! cbf_cistrncmp(c, "conversions",11)  ) {

                    c += 11;

                    cbf_failnez(cbf_skip_whitespace \
                      (file, &line, &c, &nblen, &freshline))

                    if (freshline || ( !c ) || (c == line) ) break;

                    if (*c == '=' ) {

                      c++;

                      cbf_failnez(cbf_skip_whitespace \
                        (file, &line, &c, &nblen, &freshline))

                      if (freshline || ( !c ) || (c == line) ) break;

                      quoteoff = 0;

                      if (*c == '\"') quoteoff++;

                      if (compression) *compression = CBF_NONE;

                      if(!cbf_cistrncmp(c+quoteoff, "x-cbf_packed", 12))
                        if (compression) *compression = CBF_PACKED;

                      if(!cbf_cistrncmp(c+quoteoff, "x-cbf_canonical", 15))
                        if (compression) *compression = CBF_CANONICAL;

                      if(!cbf_cistrncmp(c+quoteoff, "x-cbf_byte_offset", 17))
                        if (compression) *compression = CBF_BYTE_OFFSET;

                      if(!cbf_cistrncmp(c+quoteoff, "x-cbf_predictor", 15))
                        if (compression) *compression = CBF_PREDICTOR;

                    }
                  }

                  while ( *c && *c != ';') {

                    if ( *c == '\"') {

                      ++c;

                      while (*c && *c != '\"') {

		        if (*c == '\\') {

			++c;

			if (!*c) break;


		        }

		        ++c;

                      }

                      if (!*c) break;

	            } else {

                      if (*c == '(') {
                        cbf_failnez(cbf_skip_whitespace \
                          (file, &line, &c, &nblen, &freshline))

                        if (freshline) break;

		      } else {

                        ++c;

	              }

                    }

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
              if (cbf_cistrncmp (c, "Quoted-Printable", 16) == 0)
              
                if (isspace (c [16]) || c [16] == '(')
              
                  *encoding = ENC_QP;
                
              if (cbf_cistrncmp (c, "Base64", 6) == 0)
              
                if (isspace (c [6]) || c [6] == '(')
              
                  *encoding = ENC_BASE64;
                
              if (cbf_cistrncmp (c, "X-Base8", 7) == 0)

                if (isspace (c [7]) || c [7] == '(')
              
                  *encoding = ENC_BASE8;
                
              if (cbf_cistrncmp (c, "X-Base10", 8) == 0)
              
                if (isspace (c [8]) || c [8] == '(')
              
                  *encoding = ENC_BASE10;
                
              if (cbf_cistrncmp (c, "X-Base16", 8) == 0)
              
                if (isspace (c [8]) || c [8] == '(')
              
                  *encoding = ENC_BASE16;

              if (cbf_cistrncmp (c, "7bit", 4) == 0 ||
                  cbf_cistrncmp (c, "8bit", 4) == 0)
              
                if (isspace (c [4]) || c [4] == '(')
              
                  *encoding = ENC_NONE;

              if (cbf_cistrncmp (c, "Binary", 6) == 0)
              
                if (isspace (c [6]) || c [6] == '(')
              
                  *encoding = ENC_NONE;
            }
            
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

              /* Message digest */
            
            if (digest)
            {
              strncpy (digest, c, 24);
              
              digest [24] = '\0';
            }
            
            break;
        }
      }
    }
    
      /* Blank line? */

    if (line_count > 1)
          
      while (isspace (*line))
    
        line++;
  }
  while (*line);


    /* Success */
    
  return 0;      
}


#ifdef __cplusplus

}

#endif
