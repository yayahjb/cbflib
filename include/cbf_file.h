
#ifndef CBF_FILE_H
#define CBF_FILE_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>
#include "global.h"
#include "md5.h"


  /* File structure */

typedef struct
{
  FILE        *stream;            /* File pointer                           */
  unsigned int connections;       /* Number of pointers to this structure   */
  int          bits [2];          /* Buffer for bitwise reads and writes    */
  char         characters [64];   /* Buffer for character writes            */
  size_t       characters_used;   /* Characters in the character buffer     */
  int          last_read;         /* The last character read                */
  unsigned int line;              /* Current line                           */
  unsigned int column;            /* Current column                         */
  char        *buffer;            /* Buffer                                 */
  size_t       buffer_size;       /* Size of the buffer                     */
  size_t       buffer_used;       /* Number in use                          */
  int          read_headers;      /* message digest control (read)          */
  int          write_headers;     /* message digest and header type (write) */
  int          write_encoding;    /* encoding and line terminations (write) */
  MD5_CTX     *digest;            /* message digest context                 */
}
cbf_file;


  /* Create and initialise a file */

int cbf_make_file (cbf_file **file, FILE *stream);


  /* Free a file */

int cbf_free_file (cbf_file **file);


  /* Add a file connection */

int cbf_add_fileconnection (cbf_file **file, FILE *stream);


  /* Remove a connection */

int cbf_delete_fileconnection (cbf_file **file);

                    
  /* Count the connections */

int cbf_file_connections (cbf_file *file);

                    
  /* Set the size of the buffer */

int cbf_set_buffersize (cbf_file *file, size_t size);


  /* Empty the buffer */

int cbf_reset_buffer (cbf_file *file);


  /* Add a character to the buffer */

int cbf_save_character (cbf_file *file, int c);


  /* Retrieve the buffer */

int cbf_get_buffer (cbf_file *file, const char **buffer, 
                                         size_t *buffer_size);


  /* Get the file coordinates */

int cbf_get_filecoordinates (cbf_file *file, unsigned int *line,
                                             unsigned int *column);
                                             

  /* Set the file coordinates */

int cbf_set_filecoordinates (cbf_file *file, unsigned int line,
                                             unsigned int column);
                                             

  /* Read the next bit */

int cbf_get_bit (cbf_file *file);


  /* Read the next bits (signed) */

int cbf_get_bits (cbf_file *file, int *bitslist, int bitcount);


  /* Write bits */

int cbf_put_bits (cbf_file *file, int *bitslist, int bitcount);


  /* Read an integer as a series of bits */

int cbf_get_integer (cbf_file *file, int *val, int valsign, int bitcount);


  /* Write an integer as a series of bits */

int cbf_put_integer (cbf_file *file, int val, int valsign, int bitcount);


  /* Initialize a message digest */
  
int cbf_start_digest (cbf_file *file);


  /* Get the message digest */
  
int cbf_end_digest (cbf_file *file, char *digest);


  /* Discard any bits in the buffers */

int cbf_reset_bits (cbf_file *file);


  /* Discard any characters in the character buffers */

int cbf_reset_characters (cbf_file *file);


  /* Flush any remaining bits (write) */

int cbf_flush_bits (cbf_file *file);


  /* Flush the character buffer (write) */

int cbf_flush_characters (cbf_file *file);


  /* Get the next character */

int cbf_get_character (cbf_file *file);


  /* Read the next character (convert end-of-line and update line and column) */

int cbf_read_character (cbf_file *file);


  /* Put the next character */

int cbf_put_character (cbf_file *file, int c);


  /* Write the next character (convert end-of-line and update line and column) */

int cbf_write_character (cbf_file *file, int c);


  /* Put a string */

int cbf_put_string (cbf_file *file, const char *string);


  /* Write a string (convert end-of-line and update line and column) */

int cbf_write_string (cbf_file *file, const char *string);


  /* Read a (CR/LF)-terminated line into the buffer */

int cbf_read_line (cbf_file *file, const char **line);


  /* Read nelem characters into the buffer */

int cbf_get_block (cbf_file *file, size_t nelem);


  /* Write nelem characters from the buffer */

int cbf_put_block (cbf_file *file, size_t nelem);


  /* Copy characters between files */

int cbf_copy_file (cbf_file *destination, cbf_file *source, size_t nelem);


  /* Get the file position */

int cbf_get_fileposition (cbf_file *file, long int *position);
                                             

  /* Set the file position */

int cbf_set_fileposition (cbf_file *file, long int position, int whence);
                                             

#ifdef __cplusplus

}

#endif

#endif /* CBF_FILE_H */

