
#ifndef CBF_FILE_H
#define CBF_FILE_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>


  /* File structure */

typedef struct
{
  FILE *stream;                 /* File pointer */

  unsigned int connections;     /* Number of pointers to this structure */
  int          bits [2];        /* Buffer for bitwise reads and writes  */
  int          last_read;       /* The last character read              */
  unsigned int line;            /* Current line                         */
  unsigned int column;          /* Current column                       */
  size_t       text_size;       /* Size of the text buffer              */
  size_t       text_used;       /* Number in use                        */
 long          fpos;            /* File position                        */
 long          fend;            /* File end                             */
 
  char *text;                   /* Text buffer */
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

                    
  /* Set the size of the text buffer */

int cbf_set_textsize (cbf_file *file, size_t size);


  /* Add a character to the text buffer */

int cbf_save_character (cbf_file *file, int c);


  /* Get the file position */

int cbf_get_filecoordinates (cbf_file *file, unsigned int *line,
                                             unsigned int *column);
                                             

  /* Set the file position */

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


  /* Discard any remaining bits */

int cbf_reset_bits (cbf_file *file);


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


  /* Read nelem characters into the text buffer */

int cbf_get_text (cbf_file *file, size_t nelem);


  /* Write nelem characters from the text buffer */

int cbf_put_text (cbf_file *file, size_t nelem);


  /* Copy characters between files */

int cbf_copy_file (cbf_file *destination, cbf_file *source, size_t nelem);


#ifdef __cplusplus

}

#endif

#endif /* CBF_FILE_H */

