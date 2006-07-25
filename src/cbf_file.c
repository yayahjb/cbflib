/**********************************************************************
 * cbf_file -- file access (characterwise and bitwise)                *
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

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_file.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>


  /* Create and initialise a file */

int cbf_make_file (cbf_file **file, FILE *stream)
{
    /* Allocate the memory */

  cbf_failnez (cbf_alloc ((void **) file, NULL, sizeof (cbf_file), 1))


    /* Initialise */

  (*file)->stream = stream;

  (*file)->connections = 1;

  (*file)->bits [0]    = 0;
  (*file)->bits [1]    = 0;
  (*file)->last_read   = 0;
  (*file)->line        = 0;
  (*file)->column      = 0;
  (*file)->nscolumn    = 0;
  (*file)->buffer_size = 0;
  (*file)->buffer_used = 0;

  (*file)->buffer = NULL;
  (*file)->digest_buffer = NULL;
  (*file)->digest_bpoint = NULL;
  (*file)->context = NULL;

  (*file)->read_headers   = 0;
  (*file)->write_headers  = 0;
  (*file)->write_encoding = 0;


    /* Success */

  return 0;
}


  /* Free a file */

int cbf_free_file (cbf_file **file)
{
  int errorcode;

  errorcode = 0;

  if (file)

    if (*file)
    {
      if ((*file)->stream)

        if (fclose ((*file)->stream))

          errorcode = CBF_FILECLOSE;

      errorcode |= cbf_free ((void **) &(*file)->buffer, 
                                       &(*file)->buffer_size);

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

  if (*file)

      /* Does the stream match? */

    if (stream && (*file)->stream != stream)

      return CBF_NOTFOUND;

    else
    {
      (*file)->connections++;

      return 0;
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
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Is the size already close enough? */

  if (size > 0 && file->buffer_size >  size && 
                  file->buffer_size <= size + 4096)

    return 0;


    /* Reallocate the buffer */

  return cbf_realloc ((void **) &file->buffer, 
                                &file->buffer_size, sizeof (char), size);
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


  /* Add a character to the buffer */

int cbf_save_character (cbf_file *file, int c)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Expand the buffer? */

  if (file->buffer_used + 3 >= file->buffer_size)

    cbf_failnez (cbf_set_buffersize (file, file->buffer_size + 256))


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
    
  if (buffer)
  
    if (file->buffer_used <= 0)
    
      *buffer = NULL;
      
    else
  
      *buffer = file->buffer;
    
    
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
  
  if (file->bits [0] == 0)
  {
    file->bits [1] = getc (file->stream);

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
    file->bits [1] = getc (file->stream);

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

  size_t done;


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

    /* Update digest information, if requested */

    if (file->digest_buffer) {

      if ( !(file->digest_bpoint) ) file->digest_bpoint = file->digest_buffer;

      *(file->digest_bpoint) = (bits1 & 0x0ff);

      if ((++(file->digest_bpoint)-(file->digest_buffer)) > 63 ) {

        MD5Update (file->context, file->digest_buffer, 64);

        done = fwrite(file->digest_buffer, 1, 64, file->stream);

        if (done < 64) {

          file->bits [0] = bits0;

          file->bits [1] = bits1;

          return CBF_FILEWRITE;

	}

        file->digest_bpoint = file->digest_buffer;

      }

    } else {

      resultcode = putc (bits1 & 0x0ff, file->stream);

      if (resultcode == EOF)
      {
        file->bits [0] = bits0;
        file->bits [1] = bits1;
      
        return CBF_FILEWRITE;
      }
    }

    bits0 -= 8;


      /* Get the remaining bits */

    bits1 = *bitslist >> (bitcount - bits0);


      /* Write the remaining bits */

    while (bits0 >= 8)
    {

      /* Update digest information, if requested */

      if (file->digest_buffer) {

        if ( !(file->digest_bpoint) )

          file->digest_bpoint = file->digest_buffer;

          *(file->digest_bpoint) = (bits1 & 0x0ff);

          if ((++(file->digest_bpoint)-(file->digest_buffer)) > 63 ) {

            MD5Update (file->context, file->digest_buffer, 64);

            done = fwrite(file->digest_buffer, 1, 64, file->stream);

            if (done < 64) {

              file->bits [0] = bits0;
              file->bits [1] = bits1;

              return CBF_FILEWRITE;
          }

          file->digest_bpoint = file->digest_buffer;

        }

      } else {

        resultcode = putc (bits1 & 0x0ff, file->stream);

        if (resultcode == EOF)
        {
          file->bits [0] = bits0;
          file->bits [1] = bits1;

          return CBF_FILEWRITE;
        }
      }

      bits1 >>= 8;
    
      bits0 -= 8;
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


    /* Any bits to read? */

  if (bitcount <= 0)
  {
    *val = 0;

    return 0;
  }


    /* Number of bits in an integer */

  maxbits = sizeof (int) * CHAR_BIT;


    /* Number of bits in the value and sign parts */

  signbits = bitcount - sizeof (signed long) * CHAR_BIT;

  if (signbits > 0)

    valbits = bitcount - signbits;

  else

    valbits = bitcount;


    /* Read the value */

  cbf_failnez (cbf_get_bits (file, val, valbits))


    /* Fix the sign */

  if (valbits < maxbits && valsign == 0)

    *val &= ~-(1 << valbits);


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

    if (sign != -(*val < 0 && valsign))
    {
      errorcode = CBF_OVERFLOW;

      if (valsign)

        *val = -(sign >= 0) ^ (1 << (maxbits - 1));

      else

        *val = -1;
    }
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


  /* Discard any remaining bits */

int cbf_reset_bits (cbf_file *file)
{
  if (!file)

    return CBF_ARGUMENT;
    
  file->bits [0] =
  file->bits [1] = 0;

  return 0;
}


  /* Get the next character */

int cbf_get_character (cbf_file *file)
{
  if (file->stream)

    file->last_read = fgetc (file->stream);

  else

    file->last_read = EOF;

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

  current = cbf_get_character (file);

  if ((current == '\n' && last == '\r') ||
      (current == '\r' && last == '\n'))

    current = cbf_get_character (file);


    /* Convert the end-of-line character and update line and column */

  if (current == '\n' || current == '\r')
  {
    current = '\n';

    file->column = 0;

    file->nscolumn = 0;

    file->line++;
  }
  else

    if (current == '\t')

      file->column = (file->column & ~0x07) + 8;
      
    else
    
      file->column++;

   if ( ! isspace(current) ) 

      file->nscolumn = file->column;
      
  return current;
}


  /* Put the next character */

int cbf_put_character (cbf_file *file, int c)
{
  size_t done;

    /* Update digest information, if requested */

  if (file->digest_buffer) {

    if ( !(file->digest_bpoint) ) file->digest_bpoint = file->digest_buffer;

    *(file->digest_bpoint) = c;

    if ((++(file->digest_bpoint)-(file->digest_buffer)) > 63 ) {

      MD5Update (file->context, file->digest_buffer, 64);

      done = fwrite(file->digest_buffer, 1, 64, file->stream);

      if (done < 64) return CBF_FILEWRITE;

      file->digest_bpoint = file->digest_buffer;

    }

    return 0;

  } else {

      /* Write the character */

    if (file->stream)

      if (fputc (c, file->stream) != EOF)

        return 0;


      /* Fail */

    return CBF_FILEWRITE;
  }
}


  /* Write the next character (convert end-of-line and update line and column) */

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
  char *c;
  size_t done;

    /* Update digest information, if requested */

  if (file->digest_buffer) {

    if ( !(file->digest_bpoint) ) file->digest_bpoint = file->digest_buffer;

    c = (char *)string;

    while (*c) {

      *(file->digest_bpoint) = *c++;

      if ((++(file->digest_bpoint)-(file->digest_buffer)) > 63 ) {

        MD5Update (file->context, file->digest_buffer, 64);

        done = fwrite(file->digest_buffer, 1, 64, file->stream);

        if (done < 64) return CBF_FILEWRITE;

        file->digest_bpoint = file->digest_buffer;

      }

    }

    return 0;

  } else {

      /* Write the string */

    if (file->stream)

      if (fputs (string, file->stream) != EOF)

        return 0;

      /* Fail */

    return CBF_FILEWRITE;
  }
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

int cbf_read_line (cbf_file *file, const char **line, unsigned int *nblen)
{
  int c;
  
  
    /* Does the file exist? */
    
  if (!file)

    return CBF_ARGUMENT;


    /* Empty the buffer */
    
  file->buffer_used = 0;

  file ->column = 0;

  file ->nscolumn = 0;


    /* Read the characters */
 
  do
  {

    *nblen = file->nscolumn;

    c = cbf_read_character (file);
    
    if (c == EOF)
    
      return CBF_FILEREAD;
      
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
    if (file->stream)
    
      done = fread (file->buffer + file->buffer_used, 1,
                           nelem - file->buffer_used, file->stream);

    else

      done = 0;

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


    /* Write the characters */

  if (file->stream && nelem)
    
    done = fwrite (file->buffer, 1, nelem, file->stream);

  else

    done = 0;

  if (done < nelem)

    return CBF_FILEWRITE;


    /* Success */

  return 0;
}


  /* Copy characters between files */

int cbf_copy_file (cbf_file *destination, cbf_file *source, size_t nelem)
{
  size_t done, todo;


    /* Do the files exist? */
    
  if (!destination || !source)

    return CBF_ARGUMENT;

  if (!destination->stream || !source->stream)

    return CBF_ARGUMENT;


    /* Copy the characters in blocks of up to 1024 */
    
  while (nelem > 0)
  {
    if (nelem >= 1024)

      todo = 1024;

    else

      todo = nelem;

    cbf_failnez (cbf_get_block (source, todo))

    done = fwrite (source->buffer, 1, todo, destination->stream);

    if (done < todo)

      return CBF_FILEWRITE;

    nelem -= done;
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
    
  file_position = ftell (file->stream);
  
  if (file_position == -1L)
  
    return CBF_FILETELL;
    
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
    
 if (fseek (file->stream, position, whence) < 0)

   return CBF_FILESEEK;
    
    
    /* Success */
    
  return 0;

}


#ifdef __cplusplus

}

#endif

