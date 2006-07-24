
#ifndef CBF_H
#define CBF_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"

#include <stdlib.h>
#include <limits.h>


  /* Currently the cbf library assumes a 32-bit or larger integer */

#if UINT_MAX / 65535U < 65535U
#error cbflib assumes int is at least 32 bits
#endif


  /* Maximum line length */

#define CBF_LINELENGTH 80


  /* Error codes */

#define CBF_FORMAT           0x00000001
#define CBF_ALLOC            0x00000002
#define CBF_ARGUMENT         0x00000004
#define CBF_ASCII            0x00000008
#define CBF_BINARY           0x00000010
#define CBF_BITCOUNT         0x00000020
#define CBF_ENDOFDATA        0x00000040
#define CBF_FILECLOSE        0x00000080
#define CBF_FILEOPEN         0x00000100
#define CBF_FILEREAD         0x00000200
#define CBF_FILESEEK         0x00000400
#define CBF_FILETELL         0x00000800
#define CBF_FILEWRITE        0x00001000
#define CBF_IDENTICAL        0x00002000
#define CBF_NOTFOUND         0x00004000
#define CBF_OVERFLOW         0x00008000


  /* Constants used for compression */

#define CBF_INTEGER    0x0010  /* Uncompressed integer              */
#define CBF_FLOAT      0x0020  /* Uncompressed IEEE floating-point  */
#define CBF_CANONICAL  0x0050  /* Compressed (offset) integer       */
#define CBF_PACKED     0x0060  /* Compressed (offset) integer       */


  /* Convenience definitions for functions returning error codes */

#define cbf_failnez(f) { int err; err = (f); if (err) return err; }

#define cbf_onfailnez(f,c) { int err; err = (f); if (err) {{ c; } return err; }}


  /* cbf handle */

typedef struct
{
  cbf_node *node;

  int row, search_row;
}
cbf_handle_struct;

typedef cbf_handle_struct *cbf_handle;


  /* Prototypes */

  /* Create a handle */

int cbf_make_handle (cbf_handle *handle);


  /* Free a handle */

int cbf_free_handle (cbf_handle handle);


  /* Read a file */

int cbf_read_file (cbf_handle handle, FILE *stream);


  /* Write a file */

int cbf_write_file (cbf_handle handle, FILE *stream, int isbuffer);


  /* Add a data block */

int cbf_new_datablock (cbf_handle handle, const char *datablockname);


  /* Add a category to the current data block */

int cbf_new_category (cbf_handle handle, const char *categoryname);


  /* Add a column to the current category */
  
int cbf_new_column (cbf_handle handle, const char *columnname);


  /* Add a row to the current category */

int cbf_new_row (cbf_handle handle);


  /* Insert a row in the current category */

int cbf_insert_row (cbf_handle handle, const int rownumber);


  /* Delete a row from the current category */

int cbf_delete_row (cbf_handle handle, const int rownumber);


  /* Change the name of the current data block */

int cbf_set_datablockname (cbf_handle handle, const char *datablockname);


  /* Delete all categories from all the data blocks */

int cbf_reset_datablocks (cbf_handle handle);


  /* Delete all categories from the current data block */

int cbf_reset_datablock (cbf_handle handle);


  /* Delete all columns and rows from the current category */

int cbf_reset_category (cbf_handle handle);


  /* Delete the current data block */

int cbf_remove_datablock (cbf_handle handle);


  /* Delete the current category */

int cbf_remove_category (cbf_handle handle);


  /* Delete the current column */

int cbf_remove_column (cbf_handle handle);


  /* Delete the current row */

int cbf_remove_row (cbf_handle handle);


  /* Make the first data block the current data block */

int cbf_rewind_datablock (cbf_handle handle);


  /* Make the first category in the current data block the current category */

int cbf_rewind_category (cbf_handle handle);


  /* Make the first column in the current category the current column */

int cbf_rewind_column (cbf_handle handle);


  /* Make the first row in the current category the current row */

int cbf_rewind_row (cbf_handle handle);


  /* Make the next data block the current data block */

int cbf_next_datablock (cbf_handle handle);


  /* Make the next category in the current data block the current category */

int cbf_next_category (cbf_handle handle);


  /* Make the next column in the current category the current column */

int cbf_next_column (cbf_handle handle);


  /* Make the next row in the current category the current row */

int cbf_next_row (cbf_handle handle);


  /* Make the named data block the current data block */

int cbf_find_datablock (cbf_handle handle, const char *datablockname);


  /* Make the named category in the current data block the current category */

int cbf_find_category (cbf_handle handle, const char *categoryname);


  /* Make the named column in the current category the current column */

int cbf_find_column (cbf_handle handle, const char *columnname);


  /* Make the first row with matching value the current row */

int cbf_find_row (cbf_handle handle, const char *value);


  /* Make the next row with matching value the current row */

int cbf_find_nextrow (cbf_handle handle, const char *value);


  /* Count the data blocks */

int cbf_count_datablocks (cbf_handle handle, unsigned int *datablocks);


  /* Count the categories in the current data block */

int cbf_count_categories (cbf_handle handle, unsigned int *categories);


  /* Count the columns in the current category */

int cbf_count_columns (cbf_handle handle, unsigned int *columns);


  /* Count the rows in the current category */

int cbf_count_rows (cbf_handle handle, unsigned int *rows);


  /* Make the specified data block the current data block */

int cbf_select_datablock (cbf_handle handle, unsigned int datablock);


  /* Make the specified category the current category */

int cbf_select_category (cbf_handle handle, unsigned int category);


  /* Make the specified column the current column */

int cbf_select_column (cbf_handle handle, unsigned int column);


  /* Make the specified row the current row */

int cbf_select_row (cbf_handle handle, unsigned int row);


  /* Get the name of the current data block */
  
int cbf_datablock_name (cbf_handle handle, const char **datablockname);


  /* Get the name of the current category */
  
int cbf_category_name (cbf_handle handle, const char **categoryname);


  /* Get the name of the current column */
  
int cbf_column_name (cbf_handle handle, const char **columnname);


  /* Get the number of the current row */
  
int cbf_row_number (cbf_handle handle, unsigned int *row);


  /* Get the ascii value of the current (row, column) entry */
  
int cbf_get_value (cbf_handle handle, const char **value);


  /* Set the ascii value of the current (row, column) entry */
  
int cbf_set_value (cbf_handle handle, const char *value);


  /* Get the (int) numeric value of the current (row, column) entry */
  
int cbf_get_integervalue (cbf_handle handle, int *number);


  /* Get the (double) numeric value of the current (row, column) entry */
  
int cbf_get_doublevalue (cbf_handle handle, double *number);


  /* Set the ascii value of the current (row, column) entry from an int */
  
int cbf_set_integervalue (cbf_handle handle, int number);


  /* Set the ascii value of the current (row, column) entry from a double */
  
int cbf_set_doublevalue (cbf_handle handle, const char *format, double number);


  /* Get the parameters of the current (row, column) array entry */
  
int cbf_get_integerarrayparameters (cbf_handle handle,
                                    int *binary_id,
                                    int *elsigned, int *elunsigned,
                                    size_t *nelem,
                                    int *minelem, int *maxelem);


  /* Get the value of the current (row, column) array entry */
  
int cbf_get_integerarray (cbf_handle handle,
                          int *binary_id,
                          void *value, size_t elsize, int elsigned,
                          size_t nelem, size_t *nelem_read);


  /* Set the value of the current (row, column) array entry */
  
int cbf_set_integerarray (cbf_handle handle,
                          unsigned int compression, size_t repeat,
                          int binary_id, void *value, size_t elsize,
                          int elsigned, size_t nelem);


#ifdef __cplusplus

}

#endif

#endif /* CBF_H */

