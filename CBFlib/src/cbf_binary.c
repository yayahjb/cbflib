
#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"
#include "cbf_tree.h"
#include "cbf_compress.h"
#include "cbf_context.h"
#include "cbf_binary.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>


  /* Is this a binary value? */

int cbf_is_binary (const char *value)
{
  if (value)

    return *value == '\304' || *value == '\305';

  return 0;
}


  /* Free a value */

int cbf_free_value (cbf_context *context, const char **value)
{
  cbf_file *file;

  int binary_id;

  long int start;

  unsigned long size;

  const char *text;

  char text0;


    /* Check the argument */

  if (!value)

    return CBF_ARGUMENT;

  text = *value;

  if (!text)

    return 0;


    /* Is the value ascii? */

  text0 = *text;

  if (!cbf_is_binary (text))
  {
      /* Free the value */

    cbf_free_string (NULL, *value);

    *value = NULL;

    return 0;
  }


    /* Parse the (binary) value */

  size = 0;

  sscanf (text + 1, " %x %p %lx %lx", &binary_id, &file, &start, &size);

  if (size == 0 || start < 0 || !file)

    return CBF_FORMAT;


    /* Free the value */

  cbf_free_string (NULL, *value);

  *value = NULL;
  

    /* Free the binary section */

  if (text0 == '\305')

    return cbf_close_temporary (context, &file);

  return cbf_delete_fileconnection (&file);
}


  /* Set a binary value */

int cbf_set_binary (cbf_node *column, unsigned int row,
                    unsigned int compression, size_t repeat,
                    int binary_id, void *value, size_t elsize, int elsign,
                    size_t nelem)
{
  cbf_file *tempfile;

  const char *newvalue;

  int errorcode;

  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int)) * CHAR_BIT) >> 2) + 16];

  long int start, end;

  unsigned long size;


    /* Remove the old value */

  cbf_failnez (cbf_set_columnrow (column, row, NULL))


    /* Get the temporary file */

  cbf_failnez (cbf_open_temporary (column->context, &tempfile))


    /* Move to the end of the temporary file */

  if (fseek (tempfile->stream, 0, SEEK_END))

    return CBF_FILESEEK | cbf_delete_fileconnection (&tempfile);


    /* Add the binary data to the temporary file */

  start = ftell (tempfile->stream);

  if (start < 0)

    return CBF_FILETELL | cbf_delete_fileconnection (&tempfile);

  cbf_onfailnez (cbf_compress (value, elsize, elsign, nelem,
                           compression, repeat, tempfile),
                 cbf_delete_fileconnection (&tempfile))

  end = ftell (tempfile->stream);

  if (end < 0)

    return CBF_FILETELL | cbf_delete_fileconnection (&tempfile);

  size = (unsigned long) (end - start);


    /* Set the value */

  sprintf (text, "%x %p %lx %lx", binary_id, tempfile, start, size);

  newvalue = cbf_copy_string (NULL, text, '\305');

  if (newvalue)

    errorcode = cbf_set_columnrow (column, row, newvalue);

  else

    errorcode = CBF_ALLOC;

  if (errorcode)
  {
    cbf_free_string (NULL, newvalue);

    return errorcode | cbf_delete_fileconnection (&tempfile);
  }


    /* Success */

  return 0;
}


  /* Get the parameters of a binary value */
  
int cbf_binary_parameters (cbf_node *column, 
                           unsigned int row, int *binary_id, 
                           int *eltype, int *elsigned, int *elunsigned,
                           size_t *nelem,
                           int *minelem, int *maxelem)
{
  cbf_file *file;

  long int start;

  unsigned long size;

  const char *text;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))

  if (!text)

    return CBF_ASCII;

  
    /* Parse the value */

  if (*text != '\304' && *text != '\305')

    return CBF_ASCII;

  size = 0;

  sscanf (text + 1, " %x %p %lx %lx", binary_id, &file, &start, &size);

  if (size == 0 || start < 0 || !file)

    return CBF_FORMAT;


    /* Position the file at the start of the binary section */

  if (fseek (file->stream, start, SEEK_SET))

    return CBF_FILEREAD;
    
  
    /* Get the parameters */

  return cbf_decompress_parameters (eltype, elsigned, elunsigned, nelem,
                                    minelem, maxelem,
                                    NULL, file);
}

                   
  /* Get a binary value */
  
int cbf_get_binary (cbf_node *column, unsigned int row, int *binary_id,
                    void *value, size_t elsize, int elsign,
                    size_t nelem, size_t *nelem_read)
{
  cbf_file *file;

  long int start;

  unsigned long size;

  const char *text;

  int id_file, eltype_file, elsigned_file, elunsigned_file, minelem_file, maxelem_file;

  unsigned int compression;

  size_t nelem_file;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))

  if (!text)

    return CBF_ASCII;

  
    /* Parse the value */

  if (*text != '\304' && *text != '\305')

    return CBF_ASCII;

  size = 0;

  sscanf (text + 1, " %x %p %lx %lx", &id_file, &file, &start, &size);

  if (size == 0 || start < 0 || !file)

    return CBF_FORMAT;


    /* Position the file at the start of the binary section */

  if (fseek (file->stream, start, SEEK_SET))

    return CBF_FILEREAD;
    
  
    /* Get the parameters and position the file */

  cbf_failnez (cbf_decompress_parameters (&eltype_file,
                                      &elsigned_file, &elunsigned_file,
                                      &nelem_file,
                                      &minelem_file, &maxelem_file,
                                      &compression, file))

  if (binary_id)

    *binary_id = id_file;


    /* Decompress the binary data */

  return cbf_decompress (value, elsize, elsign, nelem, nelem_read,
                         compression, file);
}

                    
  /* Write a binary value */
  
int cbf_write_binary (cbf_node *column, unsigned int row, cbf_file *file, int isbuffer)
{
  cbf_file *infile;

  long int start;

  unsigned long size;

  int binary_id, errorcode;

  const char *intext;

  char text [(((sizeof (void *) +
                sizeof (long int) * 2 +
                sizeof (int)) * CHAR_BIT) >> 2) + 16];


    /* Check the arguments */

  if (!file)

    return CBF_ARGUMENT;
    

    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&intext, column, row))

  if (!intext)

    return CBF_ASCII;

  
    /* Parse the value */

  if (*intext != '\304' && *intext != '\305')

    return CBF_ASCII;

  size = 0;

  sscanf (intext + 1, " %x %p %lx %lx", &binary_id, &infile, &start, &size);

  if (size == 0 || start < 0 || !infile)

    return CBF_FORMAT;


    /* Position the file at the start of the binary section */

  if (fseek (infile->stream, start, SEEK_SET))

    return CBF_FILEREAD;
    
  
    /* Write the binary header */

  cbf_failnez (cbf_write_string (file, "\n;\nSTART OF BINARY SECTION\n"))

  cbf_failnez (cbf_put_character (file, 12))
  cbf_failnez (cbf_put_character (file, 26))
  cbf_failnez (cbf_put_character (file, 4))
  cbf_failnez (cbf_put_character (file, 213))


    /* Write the binary identifier (64 bits) */

  cbf_failnez (cbf_put_integer (file, binary_id, 1, 64))


    /* Write the size of the binary section (64 bits) */

  cbf_failnez (cbf_put_integer (file, size, 0, 64))


    /* Get the current point in the new file */

  start = ftell (file->stream);

  
    /* Copy the binary section to the output file */

  cbf_failnez (cbf_copy_file (file, infile, size))


    /* Write the binary footer */

  cbf_failnez (cbf_write_string (file, "\nEND OF BINARY SECTION\n;\n"))


    /* Replace a connection to a temporary file? */

  if (start >= 0 && *intext == '\305' && isbuffer)
  {
    sprintf (text, "%x %p %lx %lx", binary_id, file, start, size);

    intext = cbf_copy_string (NULL, text, '\304');

    if (intext)
    {
        /* Add the new connection */

      errorcode = cbf_add_fileconnection (&file, NULL);

      if (!errorcode)
      {
        errorcode = cbf_set_columnrow (column, row, intext);

        if (errorcode)

          cbf_delete_fileconnection (&file);
      }

      if (errorcode)
      
        cbf_free_string (NULL, intext);
    }
  }


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif

