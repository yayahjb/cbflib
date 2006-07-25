/**********************************************************************
 * cbf_codes -- convert between encoded and unencoded binary          *
 *              calculate message digest                              *
 *                                                                    *
 * Version 0.7.4 12 January 2004                                      *
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

/**********************************************************************
 * Substantial portions of this code were derived from the mpack      *
 * routine codes.c, which contains the following two notices          *
 **********************************************************************/

/**********************************************************************
 * First notice from mpack routine codes.c:                           *
 *                                                                    *
 * (C) Copyright 1993,1994 by Carnegie Mellon University              *
 * All Rights Reserved.                                               *
 *                                                                    *
 * Permission to use, copy, modify, distribute, and sell this         *
 * software and its documentation for any purpose is hereby granted   *
 * without fee, provided that the above copyright notice appear       *
 * in all copies and that both that copyright notice and this         *
 * permission notice appear in supporting documentation, and that     *
 * the name of Carnegie Mellon University not be used in advertising  *
 * or publicity  pertaining to distribution of the software without   *
 * specific, written prior permission.  Carnegie Mellon University    *
 * makes no representations about the suitability of this software    *
 * for any purpose.  It is provided "as is" without express or        *
 * implied warranty.                                                  *
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

/**********************************************************************
 * Second  notice from mpack routine codes.c:                         *
 *                                                                    *
 * Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)   *
 *                                                                    *
 * Permission to use, copy, modify, and distribute this material      *
 * for any purpose and without fee is hereby granted, provided        *
 * that the above copyright notice and this permission notice         *
 * appear in all copies, and that the name of Bellcore not be         *
 * used in advertising or publicity pertaining to this                *
 * material without the specific, prior written permission            *
 * of an authorized representative of Bellcore.  BELLCORE             *
 * MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY         *
 * OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS",         *
 * WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.                         *
 **********************************************************************/

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_codes.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>


  /* Check a 24-character base-64 MD5 digest */

int cbf_is_base64digest (const char *encoded_digest)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
       
  if (!encoded_digest)
  
    return 0;

  if (strlen (encoded_digest) != 24)

    return 0;
    
  return strspn (encoded_digest, basis_64) == 22 &&
                 encoded_digest [22] == '=' &&
                 encoded_digest [23] == '=';
}


  /* Encode a 16-character MD5 digest in base-64 (25 characters) */

int cbf_md5digest_to64 (char *encoded_digest, const unsigned char *digest)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  int todo;
  
  if (!encoded_digest || !digest)
  
    return CBF_ARGUMENT;
  

    /* Encode the 16 characters in base 64 */
    
  for (todo = 0; todo < 18; todo += 3)
  {
    encoded_digest [0] = basis_64 [((digest [todo + 0] >> 2) & 0x03f)];

    if (todo < 15)
    {
      encoded_digest [1] = basis_64 [((digest [todo + 0] << 4) & 0x030) |
                                     ((digest [todo + 1] >> 4) & 0x00f)];
      encoded_digest [2] = basis_64 [((digest [todo + 1] << 2) & 0x03c) |
                                     ((digest [todo + 2] >> 6) & 0x003)];
      encoded_digest [3] = basis_64 [((digest [todo + 2])      & 0x03f)];
    }
    else
    {
      encoded_digest [1] = basis_64 [((digest [todo + 0] << 4) & 0x030)];

      encoded_digest [2] = encoded_digest [3] = '=';
    }

    encoded_digest += 4;
  } 
  
  *encoded_digest  = '\0';

  return 0;
}    


  /* Calculate the MD5 digest (25 characters) of a block of data */

int cbf_md5digest (cbf_file *file, size_t size, char *digest)
{
  MD5_CTX context;
  
  unsigned char rawdigest [17];

  unsigned int todo;
  
  const char *buffer;


    /* Initialise the MD5 context */

  MD5Init (&context);


    /* Update the digest in blocks of 1024 */

  while (size > 0)
  {
    if (size >= 1024)

      todo = 1024;

    else

      todo = size;

    cbf_failnez (cbf_get_block (file, todo))
    
    cbf_failnez (cbf_get_buffer (file, &buffer, NULL))

    MD5Update (&context, buffer, todo);

    size -= todo;
  }
  
  
    /* Get the final digest */
    
  MD5Final (rawdigest, &context);

  cbf_md5digest_to64 (digest, rawdigest);


    /* Success */
    
  return 0;
}


  /* Convert binary data to quoted-printable text */

int cbf_toqp (cbf_file *infile, cbf_file *outfile, size_t size)
{
  static char basis_16 [] = "0123456789ABCDEF";

  int c;
    
    
    /* Check the arguments */
      
  if (!infile || !outfile)
    
    return CBF_ARGUMENT;


    /* Copy the characters */      
    
  while (size > 0)
  {
      /* Read the next character */
        
    c = cbf_get_character (infile);
      
    if (c == EOF)
      
      return CBF_FILEREAD;
    
    size--;

    if (outfile->column > 74)
      
      cbf_failnez (cbf_write_string (outfile, "=\n"))

    if ((c <= 31)  ||
        (c >= 39 && c <= 41) ||
        (c >= 43 && c <= 47) ||
        (c == 58)  ||
        (c == 61)  ||
        (c == 63)  ||
        (c >= 127) || 
        (c == ';' && outfile->column == 0))
    {
        /* Base-16 */
        
      if (outfile->column > 72)
      
        cbf_failnez (cbf_write_string (outfile, "=\n"))
      
      cbf_failnez (cbf_write_character (outfile, '='))
      cbf_failnez (cbf_write_character (outfile, basis_16 [(c >> 4) & 0x0f]))
      cbf_failnez (cbf_write_character (outfile, basis_16 [c & 0x0f]))
    }
    else

        /* Base-256 */
        
      cbf_failnez (cbf_write_character (outfile, c))
  }
  
  if (outfile->column)

    cbf_failnez (cbf_write_string (outfile, "=\n"))


    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (outfile))

   
    /* Success */
    
  return 0;
}


  /* Convert binary data to base-64 text */

int cbf_tobase64 (cbf_file *infile, cbf_file *outfile, size_t size)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  int c [3];
    
  int read;
    
  while (size > 0)
  {
      /* Read up to 3 characters */
        
    c [1] = c [2] = 0;

    for (read = 0; read < 3 && read < size; read++)
    {
      c [read] = cbf_get_character (infile);
        
      if (c [read] == EOF)
        
        return CBF_FILEREAD;
    }

    size -= read;

    if (outfile->column > 71)
    
      cbf_failnez (cbf_write_character (outfile, '\n'))


      /* Write a 24-bit chunk in base-64 */
      
    cbf_failnez (cbf_write_character (outfile, 
                                        basis_64 [(c [0] >> 2) & 0x03f]))
    cbf_failnez (cbf_write_character (outfile, 
                                        basis_64 [((c [0] << 4) & 0x030) |
                                                  ((c [1] >> 4) & 0x00f)]))
                                                     
    if (read == 1)
    
      cbf_failnez (cbf_write_string (outfile, "=="))
      
    else
    {
      cbf_failnez (cbf_write_character (outfile, 
                                        basis_64 [((c [1] << 2) & 0x03c) |
                                                  ((c [2] >> 6) & 0x003)]))
    
      if (read == 2)

        cbf_failnez (cbf_write_character (outfile, '='))

      else 

        cbf_failnez (cbf_write_character (outfile, basis_64 [c [2] & 0x03f]))
    }
  }
  
  if (outfile->column)

    cbf_failnez (cbf_write_character (outfile, '\n'))

    
    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (outfile))

   
    /* Success */
    
  return 0;
}


  /* Convert binary data to base-8/base-10/base-16 text */

int cbf_tobasex (cbf_file *infile, cbf_file *outfile, size_t size,
                                                      size_t elsize,
                                                      unsigned int base)
{
  int c [8];
  
  int count, read;
  
  long l;
  
  unsigned long block_count;
  
  char line [96], number [64];
  

    /* Check the arguments */
    
  if (elsize > 8 || (base != 8 && base != 10 && base != 16))
  
    return CBF_ARGUMENT;
  

  block_count = 0;

  while (size > 0)
  {
      /* End of a 512-element block? */

    if ((block_count % 512) == 0)
    {
      if (outfile->column)

        cbf_failnez (cbf_write_character (outfile, '\n'))
        
      if (block_count)
      
        cbf_failnez (cbf_write_string (outfile, "#\n"))
        
      if (base == 8)

        cbf_failnez (cbf_write_string (outfile, "# Octal encoding"))
        
      else
      
        if (base == 10)

          cbf_failnez (cbf_write_string (outfile, "# Decimal encoding"))
          
        else
        
          cbf_failnez (cbf_write_string (outfile, "# Hexadecimal encoding"))

      sprintf (line, ", byte %lu", (unsigned long) block_count * elsize);

      cbf_failnez (cbf_write_string (outfile, line))

      if (outfile->write_encoding & ENC_FORWARD)

        cbf_failnez (cbf_write_string (outfile, ", byte order 1234...\n#\n"))
                             
      else
    
        cbf_failnez (cbf_write_string (outfile, ", byte order ...4321\n#\n"))
    }


      /* Read up to elsize characters */
        
    memset (c, 0, sizeof (c));

    for (read = 0; read < elsize && read < size; read++)
    {
      c [read] = cbf_get_character (infile);
        
      if (c [read] == EOF)
        
        return CBF_FILEREAD;
    }
    
    size -= read;
    
    block_count++;


      /* Make the number */

    number [0] = '\0';
    
    if ((outfile->write_encoding & ENC_BACKWARD) && read < elsize)

      for (count = read; count < elsize; count++)

        strcat (number, "==");

    l = 0;
    
    if (outfile->write_encoding & ENC_FORWARD)

      for (count = read - 1; count >= 0; count--)
        
        l = (l << 8) | (c [count] & 0x0ff);

    else

      for (count = 0; count < read; count++)
        
        l = (l << 8) | (c [count] & 0x0ff);

    if (base == 8)

      sprintf (number + strlen (number), "%lo", l);
      
    else

      if (base == 10)
    
        sprintf (number + strlen (number), "%lu", l);
        
      else

        sprintf (number + strlen (number), "%lX", l);
    
    if ((outfile->write_encoding & ENC_FORWARD) && read < elsize)

      for (count = read; count < elsize; count++)

        strcat (number, "==");


      /* Write the number */    

    if (outfile->column + strlen (number) > 74)

      cbf_failnez (cbf_write_character (outfile, '\n'))

    if (outfile->column)

      cbf_failnez (cbf_write_character (outfile, ' '))

    else
    {
        /* Start a new line */
        
      if (base == 8)
      
        cbf_failnez (cbf_write_character (outfile, 'O'))
        
      else
      
        if (base == 10)
        
          cbf_failnez (cbf_write_character (outfile, 'D'))
          
        else
        
          cbf_failnez (cbf_write_character (outfile, 'H'))
      
      sprintf (line, "%1u", (unsigned int) elsize);
      
      cbf_failnez (cbf_write_string (outfile, line))

      if (outfile->write_encoding & ENC_FORWARD)
      
        cbf_failnez (cbf_write_string (outfile, "> "))
        
      else
      
        cbf_failnez (cbf_write_string (outfile, "< "))
    }

    cbf_failnez (cbf_write_string (outfile, number))
  }
  
  if (outfile->column)

    cbf_failnez (cbf_write_character (outfile, '\n'))

    
    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (outfile))

   
    /* Success */

  return 0;
}


  /* Convert quoted-printable text to binary data */

int cbf_fromqp (cbf_file *infile, cbf_file *outfile, size_t size, 
                                                     size_t *readsize,
                                                     char *digest)
{
  MD5_CTX context;
  
  unsigned char buffer [64], rawdigest [17];

  int c, bufsize;
  
  char val [3], *end;
    
  size_t count;


    /* Initialise the MD5 context */
    
  if (digest)

    MD5Init (&context);


  bufsize = 0;    
  
  count = 0;
    
  val [2] = '\0';

  while (count < size)
  {
      /* Read the (first) character */
      
    c = cbf_read_character (infile);
    
    if (c == EOF)
    
      return CBF_FILEREAD;
      
      
      /* Decode it */
      
    if (c == '=')
    {
        /* Get the second character */
        
      c = cbf_read_character (infile);
      
      if (c == EOF)
      
        return CBF_FILEREAD;
        
      if (c != '\n')
      {
          /* Get the third character */
        
        val [0] = c;

        c = cbf_read_character (infile);

        if (c == EOF)
      
          return CBF_FILEREAD;
          
        val [1] = c;
          
        
          /* Calculate the value */
          
        c = strtoul (val, &end, 16);
        
        if (end != &val [2])
        
          return CBF_FORMAT;
      }
    } 
    
    
      /* Save it */
      
    if (outfile)

      cbf_failnez (cbf_put_character (outfile, c))

    if (digest)
    {
      buffer [bufsize] = c;
      
      bufsize++;

      if (bufsize > 63)
      {
        MD5Update (&context, buffer, 64);

        bufsize = 0;
      }
    }

    count++;
  }
  
  
    /* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)
  
    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */
    
  if (readsize)
  
    *readsize = count;
    
    
    /* Success */
    
  return 0;
}


  /* Convert base-64 text to binary data */

int cbf_frombase64 (cbf_file *infile, cbf_file *outfile, size_t size,
                                                         size_t *readsize,
                                                         char *digest)
{
  static int decode_64 [256] = {
  
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, 
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, 64, -1, -1, 
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, 
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
    
    };

  MD5_CTX context;
  
  unsigned char buffer [64], rawdigest [17];
    
  int c [4], d [3], bufsize;
  
  int read, write;
    
  size_t count;


    /* Initialise the MD5 context */
    
  if (digest)

    MD5Init (&context);


  count = 0;

  bufsize = 0;
  
  while (count < size)
  {
      /* Read 4 characters */
      
    for (read = 0; read < 4; read++)
    
      do
      {
        c [read] = cbf_read_character (infile);
        
        if (c [read] == EOF)
        
          return CBF_FILEREAD;
      }
      while (decode_64 [c [read] & 0x0ff] < 0);


      /* End of data? */
      
    if (c [0] == '=' || c [1] == '=')
    
      break;


      /* Valid combinations: xxxx xxx= xx== */
      
    c [0] = decode_64 [c [0] & 0x0ff];
    c [1] = decode_64 [c [1] & 0x0ff];
    c [2] = decode_64 [c [2] & 0x0ff];
    c [3] = decode_64 [c [3] & 0x0ff];

    d [0] = ((c [0] << 2) & 0x0fc) | ((c [1] >> 4) & 0x003);
    d [1] = ((c [1] << 4) & 0x0f0) | ((c [2] >> 2) & 0x00f);
    d [2] = ((c [2] << 6) & 0x0c0) | ((c [3]     ) & 0x03f);
    
    if (c [2] == 64)
    
      read = 1;
      
    else
    
      if (c [3] == 64)
      
        read = 2;
        
      else
      
        read = 3;
        
        
      /* Save the data */
      
    for (write = 0; write < read; write++)
    {
      if (outfile)

        cbf_failnez (cbf_put_character (outfile, d [write]))

      if (digest)
      {
        buffer [bufsize] = (unsigned char) d [write];
      
        bufsize++;

        if (bufsize > 63)
        {
          MD5Update (&context, buffer, 64);

          bufsize = 0;
        }
      }
    }

    count += read;
  }


    /* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)
  
    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */
    
  if (readsize)
  
    *readsize = count;
    
    
    /* Success */
    
  return 0;
}


  /* Convert base-8/base-10/base-16 text to binary data */

int cbf_frombasex (cbf_file *infile, cbf_file *outfile, size_t size, 
                                                        size_t *readsize,
                                                        char *digest)
{
  MD5_CTX context;
  
  unsigned char buffer [64], rawdigest [17];

  int c, bufsize;
  
  char val [80], *end;
 
  int read, write, base, direction, elsize, valcount, padding;
    
  size_t count;

  unsigned long l;


    /* Defaults */
    
  base = 10;
  
  direction = 1;
  
  elsize = 4;

  count = 0;
  
  valcount = 0;
  
  padding = 0;
  
  bufsize = 0;


    /* Initialise the MD5 context */

  if (digest)

    MD5Init (&context);


  while (count < size)
  {
      /* Read the (first) character */
      
    c = cbf_read_character (infile);
    
    if (c == EOF)
    
      return CBF_FILEREAD;
      
      
      /* Interpret it */
      
    if (c == '>')
    {
      direction = 1;
      
      c = ' ';
    }
    else
        
      if (c == '<')
      {
        direction = -1;
        
        c = ' ';
      }
      else
      
        if (c == '#')
        {
            /* Comment */
            
          do
            
            c = cbf_read_character (infile);
            
          while (c != EOF && c != '\n');
          
          if (c == EOF)
          
            return CBF_FORMAT;
        }

    switch (infile->column)
    {
      case 1:

        if (c == 'O' || c == 'o')
              
          base = 8;

        else

          if (c == 'D' || c == 'd')

            base = 10;

          else

            if (c == 'H' || c == 'h')

              base = 16;

            else

              return CBF_FORMAT;
              
        break;

      case 2:
            
        if (isdigit (c) && c != '0')
        
          elsize = c - '0';
          
      case 3:

        break;

      default:
      
        if (!isspace (c))
        
          if (c == '=')
          
            padding++;
            
          else
          {
              /* Save the character */
             
            if (valcount > 78)
          
              return CBF_FORMAT;
            
            val [valcount] = c;
          
            valcount++;
          }
        else
        
          if (valcount)
          {
              /* Convert the number */
            
            val [valcount] = '\0';
          
            l = strtoul (val, &end, base);
        
            if (end != &val [valcount])
        
              return CBF_FORMAT;
        
      
              /* Save the binary data */
              
            if ((padding % 2) || padding > 6)
            
              return CBF_FORMAT;
              
            read = elsize - padding / 2;
        
            for (write = 0; write < read; write++)
            {
              if (direction < 0)
              
                c = (unsigned char) ((l >> ((read - write - 1) * 8)) & 0x0ff);

              else

                c = (unsigned char) ((l >> (write * 8)) & 0x0ff);

              if (outfile)
              
                cbf_failnez (cbf_put_character (outfile, c))

              if (digest)
              {
                buffer [bufsize] = (unsigned char) c;
      
                bufsize++;

                if (bufsize > 63)
                {
                  MD5Update (&context, buffer, 64);

                  bufsize = 0;
                }
              }
            }
         
            count += read;
            
            valcount = 0;
            
            padding = 0;
          }
    }
  }


    /* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)
  
    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */
    
  if (readsize)
  
    *readsize = count;
    
    
    /* Success */
    
  return 0;
}  

#ifdef __cplusplus

}

#endif
