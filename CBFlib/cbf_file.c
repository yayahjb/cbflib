
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
  (*file)->text_size   = 0;
  (*file)->text_used   = 0;
  (*file)->fpos        = 0;
  (*file)->fend        = 0;

  (*file)->text = NULL;


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

      errorcode |= cbf_free ((void **) &(*file)->text, &(*file)->text_size);

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

                    
  /* Set the size of the text buffer */

int cbf_set_textsize (cbf_file *file, size_t size)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Is the size already close enough? */

  if (file->text_size > size && file->text_size <= size + 4096 && size > 0)

    return 0;


    /* Reallocate the buffer */

  return cbf_realloc ((void **) &file->text, &file->text_size, sizeof (char), size);
}


  /* Add a character to the text buffer */

int cbf_save_character (cbf_file *file, int c)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;


    /* Expand the buffer? */

  if (file->text_used + 3 >= file->text_size)

    cbf_failnez (cbf_set_textsize (file, file->text_size + 256))


    /* Add the character */

  file->text [file->text_used] = (char) c;

  file->text_used++;

  file->text [file->text_used] = '\0';


    /* Success */
  
  return 0;
}


  /* Get the file position */

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
                                             

  /* Set the file position */

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
    ++(file->fpos);
    if (file->fpos > file->fend) file->fend = file->fpos;

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
    ++(file->fpos);
    if (file->fpos > file->fend) file->fend = file->fpos;

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
    resultcode = putc (bits1 & 0x0ff, file->stream);
    ++(file->fpos);
    if (file->fpos > file->fend) file->fend = file->fpos;

    if (resultcode == EOF)
    {
      file->bits [0] = bits0;
      file->bits [1] = bits1;
      
      return CBF_FILEWRITE;
    }

    bits0 -= 8;


      /* Get the remaining bits */

    bits1 = *bitslist >> (bitcount - bits0);


      /* Write the remaining bits */

    while (bits0 >= 8)
    {
      resultcode = putc (bits1 & 0x0ff, file->stream);
      ++(file->fpos);
      if (file->fpos > file->fend) file->fend = file->fpos;

      if (resultcode == EOF)
      {
        file->bits [0] = bits0;
        file->bits [1] = bits1;

        return CBF_FILEWRITE;
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
  if (file->stream) {

    file->last_read = fgetc (file->stream);
    ++(file->fpos);
    if (file->fpos > file->fend) file->fend = file->fpos;

  }
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

    file->line++;
  }
  else

    if (current == '\t')

      file->column = (file->column & ~0x07) + 8;
      
    else
    
      file->column++;
      
  return current;
}


  /* Put the next character */

int cbf_put_character (cbf_file *file, int c)
{
    /* Write the character */

  if (file->stream)

    if (fputc (c, file->stream) != EOF) {
      ++(file->fpos);
      if (file->fpos > file->fend) file->fend = file->fpos;
      return 0;
    }


    /* Fail */

  return CBF_FILEWRITE;
}


  /* Write the next character (convert end-of-line and update line and column) */

int cbf_write_character (cbf_file *file, int c)
{
    /* Does the file exist? */

  if (!file)

    return EOF;


    /* Write the next character */

  if (c == '\n') {
  if (CBForCIF == CBF || CIFCRterm) {
    cbf_failnez (cbf_put_character (file, '\r'))
  }
  if (CBForCIF == CBF || CIFNLterm) {
  cbf_failnez (cbf_put_character (file, c))
  }
  } else {
  cbf_failnez (cbf_put_character (file, c))
  }
  

    /* Update line and column */

  if (c == '\n')
  {
    file->column = 0;

    file->line++;
  }
  else

    if (c == '\t')

      file->column = (file->column & ~0x07) + 8;
      
    else
    
      file->column++;


    /* Success */

  return 0;
}


  /* Put a string */

int cbf_put_string (cbf_file *file, const char *string)
{
    /* Write the string */

  if (file->stream)

    if (fputs (string, file->stream) != EOF) {
      file->fpos += (long)strlen(string);
      if (file->fpos > file->fend) file->fend = file->fpos;
      return 0;
    }


    /* Fail */

  return CBF_FILEWRITE;
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


  /* Read nelem characters into the text buffer */

int cbf_get_text (cbf_file *file, size_t nelem)
{
  size_t done;

  
    /* Does the file exist? */
    
  if (!file)

    return CBF_ARGUMENT;


    /* Set the buffer size */

  cbf_failnez (cbf_set_textsize (file, nelem))


    /* Read the characters */

  file->text_used = 0;
  
  while (file->text_used < nelem)
  {
    if (file->stream) {
    
      done = fread (file->text + file->text_used, 1,
                          nelem - file->text_used, file->stream);
      file->fpos += (long) done;
      if (file->fpos > file->fend) file->fend = file->fpos;
    }
    else

      done = 0;

    if (done <= 0)

      return CBF_FILEREAD;

    file->text_used += done;
  }


    /* Success */

  return 0;
}


  /* Write nelem characters from the text buffer */

int cbf_put_text (cbf_file *file, size_t nelem)
{
  size_t done;

  
    /* Does the file exist? */
    
  if (!file)

    return CBF_ARGUMENT;


    /* Are there enough characters in the buffer? */

  if (nelem > file->text_size)

    return CBF_ARGUMENT;


    /* Write the characters */

  if (file->stream && nelem) {
    
    done = fwrite (file->text, 1, nelem, file->stream);
    file->fpos += (long) done;
    if (file->fpos > file->fend) file->fend = file->fpos;
  }

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

    cbf_failnez (cbf_get_text (source, todo))

    done = fwrite (source->text, 1, todo, destination->stream);
    destination->fpos += (long) done;
    if (destination->fpos > destination->fend) 
      destination->fend = destination->fpos;

    if (done < todo)

      return CBF_FILEWRITE;

    nelem -= done;
  }


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif

