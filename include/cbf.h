
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

#define CBF_FORMAT           0x00000001  /*      1 */
#define CBF_ALLOC            0x00000002  /*      2 */
#define CBF_ARGUMENT         0x00000004  /*      4 */
#define CBF_ASCII            0x00000008  /*      8 */
#define CBF_BINARY           0x00000010  /*     16 */
#define CBF_BITCOUNT         0x00000020  /*     32 */
#define CBF_ENDOFDATA        0x00000040  /*     64 */
#define CBF_FILECLOSE        0x00000080  /*    128 */
#define CBF_FILEOPEN         0x00000100  /*    256 */
#define CBF_FILEREAD         0x00000200  /*    512 */
#define CBF_FILESEEK         0x00000400  /*   1024 */
#define CBF_FILETELL         0x00000800  /*   2048 */
#define CBF_FILEWRITE        0x00001000  /*   4096 */
#define CBF_IDENTICAL        0x00002000  /*   8192 */
#define CBF_NOTFOUND         0x00004000  /*  16384 */
#define CBF_OVERFLOW         0x00008000  /*  32768 */
#define CBF_UNDEFINED        0x00010000  /*  65536 */
#define CBF_NOTIMPLEMENTED   0x00020000  /* 131072 */


  /* Token Type Strings */
  
#define CBF_TOKEN_NULL       '\377'
#define CBF_TOKEN_WORD       '\300'     /* Simple word                 */
#define CBF_TOKEN_SQSTRING   '\301'     /* Single-quoted string        */
#define CBF_TOKEN_DQSTRING   '\302'     /* Double-quoted string        */
#define CBF_TOKEN_SCSTRING   '\303'     /* Semicolon-delimited string  */
#define CBF_TOKEN_BIN        '\304'     /* Binary section              */
#define CBF_TOKEN_MIME_BIN   '\305'     /* Mime-encoded binary section */
#define CBF_TOKEN_TMP_BIN    '\306'     /* Temporary binary section    */


  /* Constants used for compression */

#define CBF_INTEGER     0x0010  /* Uncompressed integer               */
#define CBF_FLOAT       0x0020  /* Uncompressed IEEE floating-point   */
#define CBF_CANONICAL   0x0050  /* Canonical compression              */
#define CBF_PACKED      0x0060  /* Packed compression                 */
#define CBF_BYTE_OFFSET 0x0070  /* Byte Offset Compression            */
#define CBF_PREDICTOR   0x0080  /* Predictor_Huffman Compression      */
#define CBF_NONE        0x0040  /* No compression flag                */


  /* Constants used for headers */

#define PLAIN_HEADERS   0x0001  /* Use plain ASCII headers            */
#define MIME_HEADERS    0x0002  /* Use MIME headers                   */
#define MSG_NODIGEST    0x0004  /* Do not check message digests       */
#define MSG_DIGEST      0x0008  /* Check message digests              */
#define MSG_DIGESTNOW   0x0010  /* Check message digests immediately  */

#define HDR_DEFAULT (MIME_HEADERS | MSG_NODIGEST)

#define MIME_NOHEADERS  PLAIN_HEADERS


  /* CBF vs CIF */

#define CBF             0x0000  /* Use simple binary sections         */
#define CIF             0x0001  /* Use MIME-encoded binary sections   */


  /* Constants used for encoding */

#define ENC_NONE        0x0001  /* Use BINARY encoding                 */
#define ENC_BASE64      0x0002  /* Use BASE64 encoding                 */
#define ENC_QP          0x0004  /* Use QUOTED-PRINTABLE encoding       */
#define ENC_BASE10      0x0008  /* Use BASE10 encoding                 */
#define ENC_BASE16      0x0010  /* Use BASE16 encoding                 */
#define ENC_BASE8       0x0020  /* Use BASE8  encoding                 */
#define ENC_FORWARD     0x0040  /* Map bytes to words forward (1234)   */
#define ENC_BACKWARD    0x0080  /* Map bytes to words backward (4321)  */
#define ENC_CRTERM      0x0100  /* Terminate lines with CR             */
#define ENC_LFTERM      0x0200  /* Terminate lines with LF             */

#define ENC_DEFAULT (ENC_BASE64 | ENC_LFTERM | ENC_FORWARD)


  /* Convenience definitions for functions returning error codes */

#ifdef CBFDEBUG

#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d in \"x\"\n", err); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d in \"x\"\n", err); \
                         { c; } return err; }}

#else

#define cbf_failnez(f) { int err; err = (f); if (err) return err; }

#define cbf_onfailnez(f,c) { int err; err = (f); if (err) {{ c; } return err; }}

#endif


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

int cbf_read_file (cbf_handle handle, FILE *stream, int headers);


  /* Write a file */

int cbf_write_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);


  /* Add a data block */

int cbf_new_datablock (cbf_handle handle, const char *datablockname);


  /* Add a data block, allowing for duplicates */

int cbf_force_new_datablock (cbf_handle handle, const char *datablockname);


  /* Add a category to the current data block */

int cbf_new_category (cbf_handle handle, const char *categoryname);


  /* Add a category to the current data block, allowing for duplicates */

int cbf_force_new_category (cbf_handle handle, const char *categoryname);


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
  
int cbf_get_integerarrayparameters (cbf_handle    handle, 
                                    unsigned int *compression,
                                    int          *id, 
                                    size_t       *elsize, 
                                    int          *elsigned, 
                                    int          *elunsigned, 
                                    size_t       *nelem, 
                                    int          *minelem, 
                                    int          *maxelem);


  /* Get the value of the current (row, column) array entry */
  
int cbf_get_integerarray (cbf_handle  handle,
                          int        *id,
                          void       *value, 
                          size_t      elsize, 
                          int         elsign,
                          size_t      nelem, 
                          size_t     *nelem_read);


  /* Set the value of the current (row, column) array entry */
  
int cbf_set_integerarray (cbf_handle    handle,
                          unsigned int  compression, 
                          int           id, 
                          void         *value, 
                          size_t        elsize,
                          int           elsign, 
                          size_t        nelem);


#ifdef __cplusplus

}

#endif

#endif /* CBF_H */

