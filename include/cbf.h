/**********************************************************************
 * cbf.h -- cbflib basic API functions                                *
 *                                                                    *
 * Version 0.8.0 20 July 2008                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006, 2007 Herbert J. Bernstein                      *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
 **********************************************************************/

/*************************** GPL NOTICES ******************************
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * (the License, or (at your option) any later version.               *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA           *
 * 02111-1307  USA                                                    *
 *                                                                    *
 **********************************************************************/

/************************* LGPL NOTICES *******************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    * 
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    * 
 * This software                                                      *
 * -------------                                                      *
 * The term ‘this software’, as used in these Notices, refers to      *
 * those portions of the software package CBFlib that were created by *
 * employees of the Stanford Linear Accelerator Center, Stanford      *
 * University.                                                        *
 *                                                                    * 
 * Stanford disclaimer of copyright                                   *
 * --------------------------------                                   *
 * Stanford University, owner of the copyright, hereby disclaims its  *
 * copyright and all other rights in this software.  Hence, anyone    *
 * may freely use it for any purpose without restriction.             *
 *                                                                    * 
 * Acknowledgement of sponsorship                                     *
 * ------------------------------                                     *
 * This software was produced by the Stanford Linear Accelerator      *
 * Center, Stanford University, under Contract DE-AC03-76SFO0515 with *
 * the Department of Energy.                                          *
 *                                                                    * 
 * Government disclaimer of liability                                 *
 * ----------------------------------                                 *
 * Neither the United States nor the United States Department of      *
 * Energy, nor any of their employees, makes any warranty, express or *
 * implied, or assumes any legal liability or responsibility for the  *
 * accuracy, completeness, or usefulness of any data, apparatus,      *
 * product, or process disclosed, or represents that its use would    *
 * not infringe privately owned rights.                               *
 *                                                                    * 
 * Stanford disclaimer of liability                                   *
 * --------------------------------                                   *
 * Stanford University makes no representations or warranties,        *
 * express or implied, nor assumes any liability for the use of this  *
 * software.                                                          *
 *                                                                    * 
 * Maintenance of notices                                             *
 * ----------------------                                             *
 * In the interest of clarity regarding the origin and status of this *
 * software, this and all the preceding Stanford University notices   *
 * are to remain affixed to any copy or derivative of this software   *
 * made or distributed by the recipient and are to be affixed to any  *
 * copy of software made or distributed by the recipient that         *
 * contains a copy or derivative of this software.                    *
 *                                                                    * 
 * Based on SLAC Software Notices, Set 4                              *
 * OTT.002a, 2004 FEB 03                                              *
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


#ifndef CBF_H
#define CBF_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_tree.h"

#include <stdlib.h>
#include <limits.h>
#include <stdio.h>


  /* Currently the cbf library assumes a 32-bit or larger integer */

#ifndef SWIG
  /* Something wrong with the SWIG preprocessor makes it barf on this when used on a 64-bit OS! */
#if UINT_MAX / 65535U < 65535U
#error cbflib assumes int is at least 32 bits
#endif
#endif


  /* API version and assumed dictionary version */
  
#define CBF_API_VERSION     "CBFlib v0.8.0"
#define CBF_DIC_VERSION     "CBF: VERSION 1.5"

  /* Maximum line length */

#define CBF_LINELENGTH_10 80
#define CBF_LINELENGTH_11 2048

  /* Initial io buffer sizes */
  
#define CBF_INIT_READ_BUFFER 4096
#define CBF_INIT_WRITE_BUFFER 4096
#define CBF_TRANSFER_BUFFER 4096


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
#define CBF_NOCOMPRESSION    0x00040000  /* 262144 */


  /* Token Type Strings */
  
#define CBF_TOKEN_NULL       '\377'
#define CBF_TOKEN_WORD       '\300'     /* Simple word                 */
#define CBF_TOKEN_SQSTRING   '\301'     /* Single-quoted string        */
#define CBF_TOKEN_DQSTRING   '\302'     /* Double-quoted string        */
#define CBF_TOKEN_SCSTRING   '\303'     /* Semicolon-delimited string  */
#define CBF_TOKEN_BIN        '\304'     /* Binary section              */
#define CBF_TOKEN_MIME_BIN   '\305'     /* Mime-encoded binary section */
#define CBF_TOKEN_TMP_BIN    '\306'     /* Temporary binary section    */
#define CBF_TOKEN_BKTSTRING  '\311'     /* Composite string []         */
#define CBF_TOKEN_BRCSTRING  '\312'     /* Composite string {}         */
#define CBF_TOKEN_PRNSTRING  '\313'     /* Composite string ()         */
#define CBF_TOKEN_TDQSTRING  '\314'     /* Triple Double-Quoted String */
#define CBF_TOKEN_TSQSTRING  '\315'     /* Triple Single-Quoted String */
#define CBF_TOKEN_BKTITEM    '\316'     /* Bracketed item              */

#define cbf_token_term(tokentype) \
  (((tokentype)==CBF_TOKEN_WORD)?' ':                \
  (((tokentype)==CBF_TOKEN_SQSTRING)?'\'':           \
  (((tokentype)==CBF_TOKEN_DQSTRING)?'"':            \
  (((tokentype)==CBF_TOKEN_SCSTRING)?';':            \
  (((tokentype)==CBF_TOKEN_BKTSTRING)?']':           \
  (((tokentype)==CBF_TOKEN_BRCSTRING)?'}':           \
  (((tokentype)==CBF_TOKEN_PRNSTRING)?')':           \
  (((tokentype)==CBF_TOKEN_TDQSTRING)?'"':           \
  (((tokentype)==CBF_TOKEN_TDQSTRING)?'\'': '\0' )))))))) )



  /* Constants for case sensitivity */
  
#define CBF_CASE_INSENSITIVE  1
#define CBF_CASE_SENSITIVE    0


  /* Constants used for compression */

#define CBF_INTEGER     0x0010  /* Uncompressed integer               */
#define CBF_FLOAT       0x0020  /* Uncompressed IEEE floating-point   */
#define CBF_CANONICAL   0x0050  /* Canonical compression              */
#define CBF_PACKED      0x0060  /* CCP4 Packed (JPA) compression      */
#define CBF_PACKED_V2   0x0090  /* CCP4 Packed (JPA) compression V2   */
#define CBF_BYTE_OFFSET 0x0070  /* Byte Offset Compression            */
#define CBF_PREDICTOR   0x0080  /* Predictor_Huffman Compression      */
#define CBF_NONE        0x0040  /* No compression flag                */

#define CBF_COMPRESSION_MASK  \
                        0x00FF  /* Mask to separate compression
                                         type from flags              */
#define CBF_FLAG_MASK   0x0F00  /* Mask to separate flags from
                                         compression type             */
#define CBF_UNCORRELATED_SECTIONS \
                        0x0100  /* Flag for uncorrelated sections     */
#define CBF_FLAT_IMAGE  0x0200  /* Flag for flat (linear) images      */
#define CBF_NO_EXPAND   0x0400  /* Flag to try not to expand          */



  /* Flags used for logging */
  
#define CBF_LOGERROR       0x0001  /* Log a fatal error                  */
#define CBF_LOGWARNING     0x0002  /* Log a warning                      */
#define CBF_LOGWOLINE      0x0004  /* Log without the line and column    */
#define CBF_LOGWOCOLUMN    0x0008  /* Log without the column             */
#define CBF_LOGSTARTLOC    0x0010  /* Log using the start location       */
#define CBF_LOGCURRENTLOC  0x0020  /* Log using the current location     */



  /* Constants used for headers */

#define PLAIN_HEADERS   0x0001  /* Use plain ASCII headers            */
#define MIME_HEADERS    0x0002  /* Use MIME headers                   */
#define MSG_NODIGEST    0x0004  /* Do not check message digests       */
#define MSG_DIGEST      0x0008  /* Check message digests              */
#define MSG_DIGESTNOW   0x0010  /* Check message digests immediately  */
#define MSG_DIGESTWARN  0x0020  /* Warn on message digests immediately*/
#define PAD_1K          0x0020  /* Pad binaries with 1023 0's         */
#define PAD_2K          0x0040  /* Pad binaries with 2047 0's         */
#define PAD_4K          0x0080  /* Pad binaries with 4095 0's         */


  /* Constants used to control CIF parsing */
  
#define PARSE_NOBRACKETS        \
                        0x0100  /* Do not parse DDLm (,..) [,..] {,...}    */
#define PARSE_BRACKETS  0x0200  /* PARSE DDLm (,..) [,..] {,...}           */ 
#define PARSE_LIBERAL_BRACKETS  \
                        0x0400  /* PARSE DDLm (,..) [,..] {,...} liberally */ 
#define PARSE_TRIPLE_QUOTES     \
                        0x0800  /* PARSE DDLm """...""" and '''...'''      */
#define PARSE_NOTRIPLE_QUOTES   \
                        0x1000  /* Do not PARSE DDLm """...""" and '''...'''*/
#define PARSE_WIDE      0x2000  /* PARSE wide files                         */

#define HDR_DEFAULT (MIME_HEADERS | MSG_NODIGEST)

#define MIME_NOHEADERS  PLAIN_HEADERS


  /* CBF vs CIF */

#define CBF             0x0000  /* Use simple binary sections         */
#define CIF             0x0001  /* Use MIME-encoded binary sections   */


  /* Constants used for encoding */

#define ENC_NONE        0x0001  /* Use BINARY encoding                 */
#define ENC_BASE64      0x0002  /* Use BASE64 encoding                 */
#define ENC_BASE32K     0x0004  /* Use X-BASE32K encoding              */
#define ENC_QP          0x0008  /* Use QUOTED-PRINTABLE encoding       */
#define ENC_BASE10      0x0010  /* Use BASE10 encoding                 */
#define ENC_BASE16      0x0020  /* Use BASE16 encoding                 */
#define ENC_BASE8       0x0040  /* Use BASE8  encoding                 */
#define ENC_FORWARD     0x0080  /* Map bytes to words forward (1234)   */
#define ENC_BACKWARD    0x0100  /* Map bytes to words backward (4321)  */
#define ENC_CRTERM      0x0200  /* Terminate lines with CR             */
#define ENC_LFTERM      0x0400  /* Terminate lines with LF             */

#define ENC_DEFAULT (ENC_BASE64 | ENC_LFTERM | ENC_FORWARD)


  /* Convenience definitions for functions returning error codes */
  
  /* First we need to bring everything into the preprocessor */

#ifdef __STDC_VERSION__
  #if __STDC_VERSION__ < 199901L
    # if __GNUC__ >= 2
      #  define __func__ __FUNCTION__
    # endif
  #endif
#endif



#ifdef CBFDEBUG

#ifndef __FILE__

#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d \n", err); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d \n", err); \
                         { c; } return err; }}
#else
#ifndef __func__
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); \
                         { c; } return err; }}
#else
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); \
                         { c; } return err; }}

#endif
#endif

#else

#define cbf_failnez(f) { int err; err = (f); if (err) return err; }

#define cbf_onfailnez(f,c) { int err; err = (f); if (err) {{ c; } return err; }}

#endif


  /* cbf handle */

typedef struct _cbf_handle_struct
{
  cbf_node *node;
  
  struct _cbf_handle_struct *dictionary;
  
  cbf_file * file;                   /* NULL or an active cbf_file for input */
  
  int  startcolumn, startline;       /* starting location of last token */
  
  FILE * logfile;                    /* NULL or an active stream for error logging */
  
  int warnings, errors;

  int refcount, row, search_row;
}
cbf_handle_struct;

typedef cbf_handle_struct *cbf_handle;


  /* Prototypes */

  /* Create a handle */

int cbf_make_handle (cbf_handle *handle);


  /* Free a handle */

int cbf_free_handle (cbf_handle handle);


  /* Read a file */

int cbf_read_file (cbf_handle handle, FILE *stream, int flags);



  /* Read a wide file */

int cbf_read_widefile (cbf_handle handle, FILE *stream, int flags);


  /* Read a pre-read buffered file */
  
int cbf_read_buffered_file (cbf_handle handle, FILE *stream, int flags, 
                            const char * buffer, size_t buffer_len);



  /* Write a file */

int cbf_write_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);

  /* Write a file, starting at the local node */

int cbf_write_local_file (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);

  /* Write a wide file */

int cbf_write_widefile (cbf_handle handle, FILE *stream, int isbuffer,
                                                     int ciforcbf,
                                                     int headers,
                                                     int encoding);


  /* Add a data block */

int cbf_new_datablock (cbf_handle handle, const char *datablockname);


  /* Add a save frame block */

int cbf_new_saveframe (cbf_handle handle, const char *saveframename);


  /* Add a data block, allowing for duplicates */

int cbf_force_new_datablock (cbf_handle handle, const char *datablockname);


  /* Add a save frame, allowing for duplicates */

int cbf_force_new_saveframe (cbf_handle handle, const char *saveframename);


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


  /* Change the name of the current save frame */

int cbf_set_saveframename (cbf_handle handle, const char *saveframename);


  /* Delete all categories from all the data blocks */

int cbf_reset_datablocks (cbf_handle handle);


  /* Delete all categories from the current data block */

int cbf_reset_datablock (cbf_handle handle);


  /* Delete all categories from the current save frame */

int cbf_reset_saveframe (cbf_handle handle);


  /* Delete all columns and rows from the current category */

int cbf_reset_category (cbf_handle handle);


  /* Delete the current data block */

int cbf_remove_datablock (cbf_handle handle);


  /* Delete the current save frame  */

int cbf_remove_saveframe (cbf_handle handle);


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


  /* Make the first save frame in the current data block the current category */

int cbf_rewind_saveframe (cbf_handle handle);


  /* Make the first category or save frame in the current data block the current category */

int cbf_rewind_blockitem (cbf_handle handle, CBF_NODETYPE *type);


  /* Make the first column in the current category the current column */

int cbf_rewind_column (cbf_handle handle);


  /* Make the first row in the current category the current row */

int cbf_rewind_row (cbf_handle handle);


  /* Make the next data block the current data block */

int cbf_next_datablock (cbf_handle handle);

  /* Make the next save frame in the current data block the current save frame */

int cbf_next_saveframe (cbf_handle handle);


  /* Make the next category in the current data block the current category */

int cbf_next_category (cbf_handle handle);


  /* Make the next save frame or category the current data block or category */

int cbf_next_blockitem (cbf_handle handle, CBF_NODETYPE * type);



  /* Make the next column in the current category the current column */

int cbf_next_column (cbf_handle handle);


  /* Make the next row in the current category the current row */

int cbf_next_row (cbf_handle handle);


  /* Make the named data block the current data block */

int cbf_find_datablock (cbf_handle handle, const char *datablockname);


  /* Make the named save frame in the current data block the current save frame */

int cbf_find_saveframe (cbf_handle handle, const char *saveframe);


  /* Make the named category in the current data block or save frame the current category */

int cbf_find_category (cbf_handle handle, const char *categoryname);


  /* Make the named column in the current category the current column */

int cbf_find_column (cbf_handle handle, const char *columnname);


  /* Make the first row with matching value the current row */

int cbf_find_row (cbf_handle handle, const char *value);

  /* Make the first row with matching value the current row
     creating it if necessary */

int cbf_require_row (cbf_handle handle, const char *value);

  /* Make the next row with matching value the current row */

int cbf_find_nextrow (cbf_handle handle, const char *value);

  /* Make the next row with matching value the current row,
     creating the row if necessary */

int cbf_require_nextrow (cbf_handle handle, const char *value);

  /* Count the data blocks */

int cbf_count_datablocks (cbf_handle handle, unsigned int *datablocks);

  /* Count the save frames in the current data block */

int cbf_count_saveframes (cbf_handle handle, unsigned int *saveframes);

  /* Count the categories in the current data block */

int cbf_count_categories (cbf_handle handle, unsigned int *categories);


  /* Count the items in the current data block */

int cbf_count_blockitems (cbf_handle handle, unsigned int *blockitems);


  /* Count the columns in the current category */

int cbf_count_columns (cbf_handle handle, unsigned int *columns);


  /* Count the rows in the current category */

int cbf_count_rows (cbf_handle handle, unsigned int *rows);


  /* Make the specified data block the current data block */

int cbf_select_datablock (cbf_handle handle, unsigned int datablock);


  /* Make the specified save frame the current save frame */

int cbf_select_saveframe (cbf_handle handle, unsigned int saveframe);


  /* Make the specified category the current category */

int cbf_select_category (cbf_handle handle, unsigned int category);


  /* Make the specified category or save frame the current block item */

int cbf_select_blockitem (cbf_handle handle, unsigned int item, CBF_NODETYPE * type);


  /* Make the specified column the current column */

int cbf_select_column (cbf_handle handle, unsigned int column);


  /* Make the specified row the current row */

int cbf_select_row (cbf_handle handle, unsigned int row);


  /* Get the name of the current data block */
  
int cbf_datablock_name (cbf_handle handle, const char **datablockname);

  /* Get the name of the current save frame */
  
int cbf_saveframe_name (cbf_handle handle, const char **saveframename);

  /* Get the name of the current category */
  
int cbf_category_name (cbf_handle handle, const char **categoryname);


  /* Get the name of the current column */
  
int cbf_column_name (cbf_handle handle, const char **columnname);


  /* Get the number of the current row */
  
int cbf_row_number (cbf_handle handle, unsigned int *row);


  /* Get the number of the current column */

int cbf_column_number (cbf_handle handle, unsigned int *column);


  /* Get the number of the current block item */

int cbf_blockitem_number (cbf_handle handle, unsigned int *blockitem);


  /* Get the ascii value of the current (row, column) entry */
  
int cbf_get_value (cbf_handle handle, const char **value);


  /* Set the ascii value of the current (row, column) entry */
  
int cbf_set_value (cbf_handle handle, const char *value);

  /* Get the ascii value of the current (row, column) entry,
     setting it to a default value if necessary */

int cbf_require_value (cbf_handle handle, const char **value, 
                                          const char *defaultvalue);



  /* Get the ascii type of value of the current (row, column) entry */

int cbf_get_typeofvalue (cbf_handle handle, const char **typeofvalue);


  /* Set the ascii type of value of the current (row, column) entry */

int cbf_set_typeofvalue (cbf_handle handle, const char *typeofvalue);


  /* Get the (int) numeric value of the current (row, column) entry */
  
int cbf_get_integervalue (cbf_handle handle, int *number);


  /* Get the (double) numeric value of the current (row, column) entry */
  
int cbf_get_doublevalue (cbf_handle handle, double *number);


  /* Set the ascii value of the current (row, column) entry from an int */
  
int cbf_set_integervalue (cbf_handle handle, int number);


  /* Set the ascii value of the current (row, column) entry from a double */
  
int cbf_set_doublevalue (cbf_handle handle, const char *format, double number);


  /* Get the (integer) numeric value of the current (row, column) entry, setting it if necessary */
  
int cbf_require_integervalue (cbf_handle handle, int *number, int defaultvalue);


  /* Get the (double) numeric value of the current (row, column) entry, setting it if necessary */
  
int cbf_require_doublevalue (cbf_handle handle, double *number, double defaultvalue);


  /* Get the parameters of the current (row, column) array entry */
  
int cbf_get_arrayparameters (cbf_handle    handle, 
                                    unsigned int *compression,
                                    int          *id, 
                                    size_t       *elsize, 
                                    int          *elsigned, 
                                    int          *elunsigned, 
                                    size_t       *nelem, 
                                    int          *minelem, 
                                    int          *maxelem,
                                    int          *realarray);

  /* Get the parameters of the current (row, column) array entry */

int cbf_get_arrayparameters_wdims (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    int          *elsigned,
                                    int          *elunsigned,
                                    size_t       *nelem,
                                    int          *minelem,
                                    int          *maxelem,
                                    int          *realarray,
                                    const char  **byteorder,
                                    size_t       *dimfast,
                                    size_t       *dimmid,
                                    size_t       *dimslow,
                                    size_t       *padding);
#define cbf_get_arrayparameters_wdims_fs(handle, compression, id, elsize, elsigned, elunsigned, nelem, minelem, maxelem, realarray, byteorder, dimfast, dimmid, dimslow, padding) \
        cbf_get_arrayparameters_wdims((handle),(compression),(id),(elsize),(elsigned),(elunsigned),(nelem),(minelem),(maxelem),(realarray),(byteorder),(dimfast),(dimmid),(dimslow), (padding))
#define cbf_get_arrayparameters_wdims_sf(handle, compression, id, elsize, elsigned, elunsigned, nelem, minelem, maxelem, realarray, byteorder, dimslow, dimmid, dimfast, padding) \
        cbf_get_arrayparameters_wdims((handle),(compression),(id),(elsize),(elsigned),(elunsigned),(nelem),(minelem),(maxelem),(realarray),(byteorder),(dimfast),(dimmid),(dimslow), (padding))



  /* Get the parameters of the current (row, column) integer array entry */
  
int cbf_get_integerarrayparameters (cbf_handle    handle, 
                                    unsigned int *compression,
                                    int          *id, 
                                    size_t       *elsize, 
                                    int          *elsigned, 
                                    int          *elunsigned, 
                                    size_t       *nelem, 
                                    int          *minelem, 
                                    int          *maxelem);

  /* Get the parameters of the current (row, column) integer array entry */
  
int cbf_get_integerarrayparameters_wdims (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    int          *elsigned,
                                    int          *elunsigned,
                                    size_t       *nelem,
                                    int          *minelem,
                                    int          *maxelem,
                                    const char  **byteorder,
                                    size_t       *dimfast,
                                    size_t       *dimmid,
                                    size_t       *dimslow,
                                    size_t       *padding);
#define cbf_get_integerarrayparameters_wdims_fs(handle, compression, id, elsize, elsigned, elunsigned, nelem, minelem, maxelem, byteorder, dimfast, dimmid, dimslow, padding) \
        cbf_get_integerarrayparameters_wdims((handle),(compression),(id),(elsize),(elsigned),(elunsigned),(nelem),(minelem),(maxelem),(byteorder),(dimfast),(dimmid),(dimslow), (padding))
#define cbf_get_integerarrayparameters_wdims_sf(handle, compression, id, elsize, elsigned, elunsigned, nelem, minelem, maxelem, byteorder, dimslow, dimmid, dimfast, padding) \
        cbf_get_integerarrayparameters_wdims((handle),(compression),(id),(elsize),(elsigned),(elunsigned),(nelem),(minelem),(maxelem),(byteorder),(dimfast),(dimmid),(dimslow), (padding))


  /* Get the integer value of the current (row, column) array entry */
  
int cbf_get_integerarray (cbf_handle  handle,
                          int        *id,
                          void       *value, 
                          size_t      elsize, 
                          int         elsign,
                          size_t      nelem, 
                          size_t     *nelem_read);

  /* Get the real value of the current (row, column) array entry */
  
int cbf_get_realarray (cbf_handle  handle,
                          int        *id,
                          void       *value, 
                          size_t      elsize, 
                          size_t      nelem, 
                          size_t     *nelem_read);

  /* Get the parameters of the current (row, column) array entry */

int cbf_get_realarrayparameters (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    size_t       *nelem);

  /* Get the parameters of the current (row, column) array entry */

int cbf_get_realarrayparameters_wdims (cbf_handle    handle,
                                    unsigned int *compression,
                                    int          *id,
                                    size_t       *elsize,
                                    size_t       *nelem,
                                    const char  **byteorder,
                                    size_t       *dimfast,
                                    size_t       *dimmid,
                                    size_t       *dimslow,
                                    size_t       *padding);
#define cbf_get_realarrayparameters_wdims_fs(handle,compression,id,elsize,nelem,byteorder,dimfast,dimmid,dimslow,padding) \
        cbf_get_realarrayparameters_wdims((handle),(compression),(id),(elsize),(nelem),(byteorder),(dimfast),(dimmid),(dimslow),(padding))
#define cbf_get_realarrayparameters_wdims_sf(handle,compression,id,elsize,nelem,byteorder,dimslow,dimmid,dimfast,padding) \
        cbf_get_realarrayparameters_wdims((handle),(compression),(id),(elsize),(nelem),(byteorder),(dimfast),(dimmid),(dimslow),(padding))

  /* Set the integer value of the current (row, column) array entry */
  
int cbf_set_integerarray (cbf_handle    handle,
                          unsigned int  compression, 
                          int           id, 
                          void         *value, 
                          size_t        elsize,
                          int           elsign, 
                          size_t        nelem);
                          
  /* Set the integer value of the current (row, column) array entry */
  
int cbf_set_integerarray_wdims (cbf_handle    handle,
                          unsigned int  compression,
                          int           id,
                          void         *value,
                          size_t        elsize,
                          int           elsign,
                          size_t        nelem,
                          const char   *byteorder,
                          size_t        dimfast,
                          size_t        dimmid,
                          size_t        dimslow,
                          size_t        padding);
#define cbf_set_integerarray_wdims_fs(handle, compression, id, value, elsize, elsign, nelem, byteorder, dimfast, dimmid, dimslow, padding) \
         cbf_set_integerarray_wdims((handle),(compression),(id),(value),(elsize),(elsign),(nelem),(byteorder),(dimfast),(dimmid),(dimslow),(padding))
#define cbf_set_integerarray_wdims_sf(handle, compression, id, value, elsize, elsign, nelem, byteorder, dimslow, dimmid, dimfast, padding) \
         cbf_set_integerarray_wdims((handle),(compression),(id),(value),(elsize),(elsign),(nelem),(byteorder),(dimfast),(dimmid),(dimslow),(padding))


  /* Set the real value of the current (row, column) array entry */
  
int cbf_set_realarray (cbf_handle    handle,
                          unsigned int  compression, 
                          int           id, 
                          void         *value, 
                          size_t        elsize,
                          size_t        nelem);

  /* Set the real value of the current (row, column) array entry
     with dimensions */

int cbf_set_realarray_wdims (cbf_handle    handle,
                          unsigned int  compression,
                          int           id,
                          void         *value,
                          size_t        elsize,
                          size_t        nelem,
                          const char   *byteorder,
                          size_t        dimfast,
                          size_t        dimmid,
                          size_t        dimslow,
                          size_t        padding);
#define cbf_set_realarray_wdims_fs(handle, compression, id, value, elsize, nelem, byteorder, dimfast, dimmid, dimslow, padding) \
         cbf_set_realarray_wdims((handle),(compression),(id),(value),(elsize),(nelem),(byteorder),(dimfast),(dimmid),(dimslow),(padding))
#define cbf_set_realarray_wdims_sf(handle, compression, id, value, elsize, nelem, byteorder, dimslow, dimmid, dimfast, padding) \
         cbf_set_realarray_wdims((handle),(compression),(id),(value),(elsize),(nelem),(byteorder),(dimfast),(dimmid),(dimslow),(padding))

  /* Issue a warning message */

void cbf_warning (const char *message);


  /* Issue an error message */

void cbf_error (const char *message);


  /* issue a log message for a cbf */
  
void cbf_log (cbf_handle handle, const char *message, int logflags);


  /* Find a datablock, creating it if necessary */
  
int cbf_require_datablock (cbf_handle  handle,
                             const char *datablockname);

  /* Find a category, creating it if necessary */
  
int cbf_require_category (cbf_handle  handle,
                             const char *categoryname);

  /* Find a column, creating it if necessary */
  
int cbf_require_column (cbf_handle  handle,
                             const char *columnname);
   

  /* Find a column value, return a default if necessary */
  
int cbf_require_column_value (cbf_handle  handle,
                             const char *columnname,
                             const char **value,
                             const char *defaultvalue);

  /* Find a column integer value, return a default if necessary */
  
int cbf_require_column_integervalue (cbf_handle  handle,
                             const char *columnname,
                             int *number,
                             const int defaultvalue);

  /* Find a column double value, return a default if necessary */
  
int cbf_require_column_doublevalue (cbf_handle  handle,
                             const char *columnname,
                             double *number,
                             const double defaultvalue);

  /* Get the local byte order of the default integer type */
  
int cbf_get_local_integer_byte_order (char ** byte_order);

  /* Get the local byte order of the default real type */
  
int cbf_get_local_real_byte_order (char ** byte_order);

  /* Get the local real format */
  
int cbf_get_local_real_format (char ** real_format );

  /* Get the dictionary for a cbf */
  
int cbf_get_dictionary (cbf_handle handle, cbf_handle * dictionary);

  /* Set the dictionary for a cbf */
  
int cbf_set_dictionary (cbf_handle handle, cbf_handle dictionary);

  /* Get the dictionary for a cbf, or create one */
  
int cbf_require_dictionary (cbf_handle handle, cbf_handle * dictionary);

  /* Put the value into the named column, updating the hash table links */

int cbf_set_hashedvalue(cbf_handle handle, const char * value, 
                                           const char * columnname,
                                           int valuerow);
                                           
  /* Find value in the named column, using the hash table links */

int cbf_find_hashedvalue(cbf_handle handle, const char * value, 
                                            const char * columnname,
                                            int caseinsensitive);


  /* Take a defintion from a dictionary and insert it into the
      has tables of a cbf dictionary */

int cbf_convert_dictionary_definition(cbf_handle cbfdictionary, 
                                           cbf_handle dictionary,
                                           const char * name);


  /* Increment a column */

int cbf_increment_column( cbf_handle handle, const char* columnname, 
                                           int * count );

  /* Reset a column */

int cbf_reset_column( cbf_handle handle, const char* columnname);


  /* Reset reference counts for a dictionary */
  
int cbf_reset_refcounts( cbf_handle dictionary );


  /* Convert a DDL1 or DDL2 dictionary and add it to a CBF dictionary */

int cbf_convert_dictionary (cbf_handle handle, cbf_handle dictionary );


  /* Find the requested tag anywhere in the cbf, make it the current column */

int cbf_find_tag (cbf_handle handle, const char *tag);

  /* Find the requested tag in the cbf within the current
  
     save frame or data block, make it the current column */

int cbf_find_local_tag (cbf_handle handle, const char *tag);

  /* Find the requested category and column anywhere in the cbf, make it the current column */

int cbf_srch_tag (cbf_handle handle, cbf_node *node, 
                                     const char *categoryname, 
                                     const char *columnname);

  /* Find the root alias of a given category */
  
int cbf_find_category_root (cbf_handle handle, const char* categoryname,
                                            const char** categoryroot);

  /* Find the root alias of a given category, defaulting to the current one */
  
int cbf_require_category_root (cbf_handle handle, const char* categoryname,
                                            const char** categoryroot);

  /* Set the root alias of a given category */
  
int cbf_set_category_root (cbf_handle handle, const char* categoryname,
                                            const char* categoryroot);

  /* Find the root alias of a given tag */
  
int cbf_find_tag_root (cbf_handle handle, const char* tagname,
                                            const char** tagroot);

  /* Find the root alias of a given tag, defaulting to the current one */
  
int cbf_require_tag_root (cbf_handle handle, const char* tagname,
                                            const char** tagroot);

  /* Set the root alias of a given tag */
  
int cbf_set_tag_root (cbf_handle handle, const char* tagname,
                                            const char* tagroot);

  /* Find the category of a given tag */
  
int cbf_find_tag_category (cbf_handle handle, const char* tagname,
                                            const char** categoryname);
  /* Set category of a given tag */
  
int cbf_set_tag_category (cbf_handle handle, const char* tagname,
                                            const char* categoryname);

  /* Validate portion of CBF */
 
int cbf_validate (cbf_handle handle, cbf_node * node, CBF_NODETYPE type,
                                     cbf_node * catnode);

  /* Load accumulator */

int cbf_mpint_load_acc(unsigned int * acc, size_t acsize, 
                               void * source, size_t elsize, 
                               int elsign, const char * border);


  /* Store accumulator */

int cbf_mpint_store_acc(unsigned int * acc, size_t acsize, 
                                void * dest, size_t elsize,
                                int elsign, const char *border);

  /* Clear accumulator */

int cbf_mpint_clear_acc(unsigned int * acc, size_t acsize);

  /* Increment accumulator */

int cbf_mpint_increment_acc(unsigned int * acc, size_t acsize);

  /* Decrement accumulator */

int cbf_mpint_decrement_acc(unsigned int * acc, size_t acsize);

  /* Negate accumulator */

int cbf_mpint_negate_acc(unsigned int * acc, size_t acsize);

  /* Add to accumulator */
  
int cbf_mpint_add_acc(unsigned int * acc, size_t acsize, unsigned int * add, size_t addsize);

  /* Shift accumulator right */

int cbf_mpint_rightshift_acc(unsigned int * acc, size_t acsize, int shift);

  /* Shift accumulator left */

int cbf_mpint_leftshift_acc(unsigned int * acc, size_t acsize, int shift);


  /* Check value of type validity */
  
int cbf_check_type_contents(const char *type, const char *value);

  /* Regex Match function */

int cbf_match(const char *string, char *pattern);

#ifdef __cplusplus

}

#endif

#endif /* CBF_H */

