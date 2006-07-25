/**********************************************************************
 * cbf_codes -- convert between encoded and unencoded binary          *
 *              calculate message digest                              *
 *                                                                    *
 * Version 0.4 15 November 1998                                       *
 *                                                                    *
 *             Paul Ellis (ellis@ssrl.slac.stanford.edu) and          *
 *          Herbert J. Bernstein (yaya@bernstein-plus-sons.com)       *
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
 * Substantial portions of this code were derived from the mpack      *
 * routine codes.c, to which contains the following two notices       *
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

int cbf_md5context_to64(MD5_CTX *context, char *encoded_digest);


  /* Calculate the MD5 digest (25 characters) of a block of data */

int cbf_md5digest (cbf_file *file, size_t size, char *digest)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

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
    
  cbf_failnez(cbf_md5context_to64(&context, digest))


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

    
    /* Success */

  return 0;
}


  /* Convert quoted-printable text to binary data */

int cbf_fromqp (cbf_file *infile, cbf_file *outfile, size_t size, 
                                                     size_t *readsize,
                                                     char *digest)
{
  MD5_CTX context;
  
  unsigned char buffer[64], *bpoint;

  int c;
  
  char val [3], *end;
    
  size_t count;

  count = 0;
  
  val [2] = '\0';

    /* Initialise the MD5 context */

  MD5Init (&context);

  bpoint = buffer;

  while (count < size)
  {
      /* Read the (first) character */
      
    c = cbf_read_character (infile);
    
    if (c == EOF)
    
      return CBF_FILEREAD;
      
      
      /* Decode it */
      
    if (c != '=')
    {    
        /* Plain data */

      *bpoint = c;

      if (outfile) {

        cbf_failnez (cbf_put_character (outfile, c))

      }

      if (((++bpoint)-buffer) > 63 ) {

        MD5Update (&context, buffer, 64);

        bpoint = buffer;
      }

      count++;
    }
    else
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
          
          
          /* Save it */

        *bpoint = c;
        if (outfile) {
          cbf_failnez (cbf_put_character (outfile, c))
        }

        if (((++bpoint)-buffer) > 63 ) {
          MD5Update (&context, buffer, 64);
          bpoint = buffer;
        }

      
        count++;
      }
    }
  }

  if ((bpoint - buffer) > 0 ) {

    MD5Update (&context, buffer, (bpoint-buffer));    

  }

  if (digest) {

    cbf_failnez(cbf_md5context_to64(&context, digest))

  }
    
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
  
  unsigned char buffer[64], *bpoint;
    
  int c [4], d [3];
  
  int read, write;
    
  size_t count;


  count = 0;

    /* Initialise the MD5 context */

  MD5Init (&context);

  bpoint = buffer;
  
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
      
    for (write = 0; write < read; write++) {

      *bpoint = (unsigned char) d [write];

      if (outfile) {

        cbf_failnez (cbf_put_character (outfile, d [write]))

      }

      if (((++bpoint)-buffer) > 63 ) {

         MD5Update (&context, buffer, 64);

         bpoint = buffer;
      }

    }

    count += read;
  }

  if ((bpoint - buffer) > 0 ) {

    MD5Update (&context, buffer, (bpoint-buffer));    

  }

  if (digest) {

    cbf_failnez(cbf_md5context_to64(&context, digest))

  }

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
  
  unsigned char buffer[64], *bpoint;

  int c;
  
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

    /* Initialise the MD5 context */

  MD5Init (&context);

  bpoint = buffer;

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
        
            for (write = 0; write < read; write++){

              if (direction < 0) {

                *bpoint =  (unsigned char) 
                  ((l >> ((read - write - 1) * 8)) & 0x0ff);

              } else {

                *bpoint = (unsigned char)
                  ((l >> (write * 8)) & 0x0ff);

              }

              if (outfile) {
                  cbf_failnez (cbf_put_character (outfile, *bpoint))
	      }

	      if (((++bpoint)-buffer) > 63 ) {

                  MD5Update (&context, buffer, 64);

                  bpoint = buffer;

              }

	    }
         
            count += read;
            
            valcount = 0;
            
            padding = 0;
          }
    }
  }

  if ((bpoint - buffer) > 0 ) {

    MD5Update (&context, buffer, (bpoint-buffer));    

  }

  if (digest) {

    cbf_failnez(cbf_md5context_to64(&context, digest))

  }
 
  if (readsize)
  
    *readsize = count;
    
    
    /* Success */
    
  return 0;
}  

int cbf_md5context_to64(MD5_CTX *context, char *encoded_digest)
{
  static char basis_64 [] =
       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  unsigned char digest[18];

  int i;

  register char *p;

  MD5Final(digest, context);

  digest[sizeof(digest)-1] = digest[sizeof(digest)-2] = 0;

  p = encoded_digest;

  for (i=0; i < sizeof(digest); i+=3) {

    *p++ = basis_64[digest[i]>>2];

    *p++ = basis_64[((digest[i] & 0x3)<<4) | ((digest[i+1] & 0xF0)>>4)];

    *p++ = basis_64[((digest[i+1] & 0xF)<<2) | ((digest[i+2] & 0xC0)>>6)];

    *p++ = basis_64[digest[i+2] & 0x3F];

    }

    *p-- = '\0';

    *p-- = '=';

    *p-- = '=';

    return 0;

}    

#ifdef __cplusplus

}

#endif

