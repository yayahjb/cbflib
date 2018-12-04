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
 * The term "this software", as used in these Notices, refers to      *
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

#include "hdf5.h"

#ifdef __cplusplus

extern "C" {

#endif

/* Version numbers */
#define CBF_VERS_MAJOR   0   /* For major interface/format changes        */
#define CBF_VERS_MINOR   9   /* For minor interface/format changes        */
#define CBF_VERS_RELEASE 6   /* For tweaks, bug-fixes, or development     */
#define CBF_VERS_SUBRELEASE ""   
                             /* For pre-releases like RC1                 */
                             /* Empty string for real releases.           */
                             /* Must be a quoted string                   */
#define CBF_APIVERS_CUR  3   /* Number of major interface version         */
#define CBF_APIVERS_REV  0   /* Interface changes                         */
#define CBF_APIVERS_AGE  1   /* Number of interfaces added                */

#define _CBF_STR2(n) #n
#define _CBF_STR(n) _CBF_STR2(n)

#define CBF_VERS_STRING _CBF_STR(CBF_VERS_MAJOR) "." \
_CBF_STR(CBF_VERS_MINOR) "." \
_CBF_STR(CBF_VERS_RELEASE) CBF_VERS_SUBRELEASE

#define CBF_SVN_REVISION_STRING "$Rev: 588 $"
#define CBF_SVN_DATE_STRING "$Date: 2015-12-15 18:48:12 -0500 (Tue, 15 Dec 2015) $"
    
#ifdef _WIN32
#define CBF_SYSTEM_TMP_DIR "C:\\CBF"
#define CBF_USER_TMP_DIR "%TEMP%\\CBF"
#define CBF_PATH_DIR_SEP '\\'
#else
#define CBF_SYSTEM_TMP_DIR "/tmp/cbf"
#define CBF_USER_TMP_DIR "${HOME}/.cbf/tmp"
#define CBF_PATH_DIR_SEP '/'
#endif
    
#ifdef CBF_USE_SYSTEM_TMP
#define CBF_TMP_DIR CBF_SYSTEM_TMP_DIR
#define CBF_TMP_DIR_PERM 0777
#else
#define CBF_TMP_DIR CBF_USER_TMP_DIR
#define CBF_TMP_DIR_PERM 0700
#endif
    
#define CBF_DEFER_TMP "yes"
    
    
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
#else
    #define CBF_USE_LONG_LONG
    typedef long long CBF_sll_type;
    typedef unsigned long long CBF_ull_type;
#endif
   
#if defined(CBF_DONT_USE_LONG_LONG) || defined(__cplusplus) || defined(__MINGW32__)
#undef ULLONG_MAX
#undef CBF_USE_LONG_LONG
#endif

#ifndef SWIG
#if defined(ULLONG_MAX) && defined(LLONG_MAX)
  #if ULLONG_MAX >= 18446744073709551615U
    #define  CBF_USE_LONG_LONG
    typedef long long CBF_sll_type;
    typedef unsigned long long CBF_ull_type;
  #else
    #if UINT_MAX >= 4294967295U
      typedef struct { unsigned int el0:32;unsigned int el1:32;} CBF_sll_type;
      typedef struct { unsigned int el0:32;unsigned int el1:32;} CBF_ull_type;
      #define CBF_SLL_INTS 2
      #define CBF_ULL_INTS 2
    #else
      typedef struct { unsigned int el0:32;unsigned int el1:32;unsigned int el2:32;unsigned int el3:32;} CBF_sll_type;
      typedef struct { unsigned int el0:32;unsigned int el1:32;unsigned int el2:32;unsigned int el3:32;} CBF_ull_type;
      #define CBF_SLL_INTS 4
      #define CBF_ULL_INTS 4
    #endif
  #endif
#else
  #if UINT_MAX >= 4294967295U
    typedef struct { unsigned int el0:32;unsigned int el1:32;} CBF_sll_type;
    typedef struct { unsigned int el0:32;unsigned int el1:32;} CBF_ull_type;
    #define CBF_SLL_INTS 2
    #define CBF_ULL_INTS 2
  #else
    typedef struct { unsigned int el0:32;unsigned int el1:32;unsigned int el2:32;unsigned int el3:32;} CBF_sll_type;
    typedef struct { unsigned int el0:32;unsigned int el1:32;unsigned int el2:32;unsigned int el3:32;} CBF_ull_type;
    #define CBF_SLL_INTS 4
    #define CBF_ULL_INTS 4
  #endif
#endif
#endif

  /* API version and assumed dictionary version */
  
#define CBF_API_VERSION     "CBFlib v0.9.6"
#define CBF_DIC_VERSION     "CBF: VERSION 1.7.11"

  /* Maximum line length */

#define CBF_LINELENGTH_10 80
#define CBF_LINELENGTH_11 2048
#define CBF_LINELENGTH_20 8192

  /* Initial io buffer sizes */
  
#define CBF_INIT_READ_BUFFER 4096
#define CBF_INIT_WRITE_BUFFER 4096
#define CBF_TRANSFER_BUFFER 4096


  /* Error codes */

#define CBF_SUCCESS                   0
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
#define CBF_H5ERROR          0x00080000  /* 524288 */
#define CBF_H5DIFFERENT      0x00100000  /* 1048576 */
#define CBF_SIZE             0x00200000  /* 2097152 */
    
  /* HDF5 LZO Filter number */

#ifndef CBF_H5Z_FILTER_LZO
#ifndef H5Z_FILTER_LZO
#define CBF_H5Z_FILTER_LZO    305
#else
#define CBF_H5Z_FILTER_LZO H5Z_FILTER_LZO
#endif
#endif

    /* HDF5 BZIP2 Filter number */
    
#ifndef CBF_H5Z_FILTER_BZIP2
#ifndef H5Z_FILTER_BZIP2
#define CBF_H5Z_FILTER_BZIP2  307
#else
#define CBF_H5Z_FILTER_BZIP2 H5Z_FILTER_BZIP2
#endif
#endif

    /* HDF5 LZF Filter number */
    
#ifndef CBF_H5Z_FILTER_LZF
#ifndef H5Z_FILTER_LZF
#define CBF_H5Z_FILTER_LZF    32000
#else
#define CBF_H5Z_FILTER_LZF H5Z_FILTER_LZF
#endif
#endif

    
    /* HDF5 BLOSC Filter number */
    
#ifndef CBF_H5Z_FILTER_BLOSC
#ifndef H5Z_FILTER_BLOSC
#define CBF_H5Z_FILTER_BLOSC   32001
#else
#define CBF_H5Z_FILTER_BLOSC  H5Z_FILTER_BLOSC
#endif
#endif
    
    /* HDF5 MAFISC Filter number */
    
#ifndef CBF_H5Z_FILTER_MAFISC
#ifndef H5Z_FILTER_MAFISC
#define CBF_H5Z_FILTER_MAFISC   32002
#else
#define CBF_H5Z_FILTER_MAFISC  H5Z_FILTER_MAFISC
#endif
#endif

    /* HDF5 SNAPPY Filter number */
    
#ifndef CBF_H5Z_FILTER_SNAPPY
#ifndef H5Z_FILTER_SNAPPY
#define CBF_H5Z_FILTER_SNAPPY   32003
#else
#define CBF_H5Z_FILTER_SNAPPY  H5Z_FILTER_SNAPPY
#endif
#endif

    
  /* HDF5 LZ4 Filter number */

#ifndef CBF_H5Z_FILTER_LZ4
#ifndef H5Z_FILTER_LZ4
#define CBF_H5Z_FILTER_LZ4    32004
#else
#define CBF_H5Z_FILTER_LZ4 H5Z_FILTER_LZ4
#endif
#endif


    /* HDF5 CBF Filter number */

#ifndef CBF_H5Z_FILTER_CBF
#ifndef H5Z_FILTER_CBF
#define CBF_H5Z_FILTER_CBF    32006
#else
#define CBF_H5Z_FILTER_CBF H5Z_FILTER_CBF
#endif
#endif
    
    /* HDF5 JPEG_XR Filter number */
    
#ifndef CBF_H5Z_FILTER_JPEG_XR
#ifndef H5Z_FILTER_JPEG_XR
#define CBF_H5Z_FILTER_JPEG_XR    32007
#else
#define CBF_H5Z_FILTER_JPEG_XR H5Z_FILTER_JPEG_XR
#endif
#endif


    /* HDF5 BSHUF Filter number */
    
#ifndef CBF_H5Z_FILTER_BSHUF
#ifndef H5Z_FILTER_BSHUF
#define CBF_H5Z_FILTER_BSHUF    32008
#else
#define CBF_H5Z_FILTER_BSHUF H5Z_FILTER_BSHUF
#endif
#endif
    
    
    /* HDF5 SPDP Filter number */
    
#ifndef CBF_H5Z_FILTER_SPDP
#ifndef H5Z_FILTER_SPDP
#define CBF_H5Z_FILTER_SPDP    32009
#else
#define CBF_H5Z_FILTER_SPDP H5Z_FILTER_SPDP
#endif
#endif



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
#define CBF_TOKEN_FUNCTION   '\317'     /* Function definition         */

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
#define CBF_NIBBLE_OFFSET     \
                        0x00A0  /* Nibble Offset Compression          */
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
#define CBF_H5COMPRESSION \
                        0x0800  /* Flag to turn on HDF compression in CBF write*/
#define	CBF_H5COMPRESSION_CBF  \
                        0x0800  /* Flag to turn on CBF compression for the 
                                   a binary dataset within an HDF5 file 
                                   Note this overloads the CBF_H5COMPRESSION
                                   flag */
#define	CBF_H5COMPRESSION_ZLIB   0x00B0
    /* Flag to turn on zlib compression for the main dataset within a HDF5 file */
#define	CBF_H5COMPRESSION_LZ4    0x00C0
    /* Flag to turn on LZ4 compression for the main dataset within a HDF5 file */
#define	CBF_H5COMPRESSION_LZ4_2  0x00D0
    /* Flag to turn on LZ4**2 compression for the main dataset within a HDF5 file */
#define	CBF_H5COMPRESSION_BSLZ4  0x00E0
    /* Flag to turn on BSLZ4 compression for the main dataset within a HDF5 file */

    
    
  /* Flags for HDF5/NeXus management */
    
#define CBF_H5_OPAQUE   0x1000  /* Flag to write compressed images
                                     as opaque objects                */
#define CBF_H5_NOH5     0x2000  /* Flag to suppress the H5 group      */
#define CBF_H5_REGISTER_COMPRESSIONS \
                        0x4000  /* Flag to try to register
                                     CBF compressions                 */
#define CBF_H5_CBFNONAMES \
                        0x8000  /* Flag to not carry CBF names into HDF5  */

#define CBF_H5_NXPDB   0x10000  /* Flag to use  NXpdb conventions in HDF5 */


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
  
#define CBF_PARSE_BRC   0x0100  /* PARSE DDLm/CIF2 brace {,...}             */
#define CBF_PARSE_PRN   0x0200  /* PARSE DDLm parens     (,...)             */
#define CBF_PARSE_BKT   0x0400  /* PARSE DDLm brackets   [,...]             */
#define CBF_PARSE_BRACKETS \
                        0x0700  /* PARSE ALL brackets                       */
#define CBF_PARSE_TQ    0x0800  /* PARSE treble quotes """...""" and '''...'''       */
#define CBF_PARSE_CIF2_DELIMS  \
                        0x1000  /* Do not scan past an unescaped close quote
                                   do not accept {} , : " ' in non-delimited
                                   strings'{ */                          
#define CBF_PARSE_DDLm  0x0700  /* For DDLm parse (), [], {}                */
#define CBF_PARSE_CIF2  0x1F00  /* For CIF2 parse {}, treble quotes,
                                   stop on unescaped close quotes           */
#define CBF_PARSE_DEFINES      \
                        0x2000  /* Recognize DEFINE_name            */      
                        
  
#define CBF_PARSE_WIDE      0x4000  /* PARSE wide files                         */
#define CBF_PARSE_WS        0x8000  /* PARSE whitespace                         */
#define CBF_PARSE_UTF8      0x10000 /* PARSE UTF-8                              */

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

/*
Flag to control level of debug output.
Possible debug-level settings:
0: silent failures.
1: cause & location of 'exceptions'.
2: stack trace from 'exceptions' & deprecation warnings.
3: (mostly) complete stack trace.
4: extra information.
5: trace from significant branches.
*/
#define CBF_PRINT_DEBUG_LEVEL 2
#define CBF_USE_SAFE_PRINT_DEBUG 1
    
/* flag to control strict macro calls that will fail without a semicolon */
/* #define CBF_USE_STRICT_CALL 1 */

/* switch between safe & unsafe macros */
#ifdef CBF_USE_STRICT_CALL
#define CBFM_PROLOG do
#define CBFM_EPILOG while(0)
#else
#define CBFM_PROLOG
#define CBFM_EPILOG
#endif

/*
Define some unconditional error printing macros, to ensure that the same format is used everywhere.
NOTE: This _must_ be unconditional, because it will be used for hints to the user about how to fix
a problem; not for debug output, which is handled via a separate code path.
*/
#define CBF_PRINT_ERROR(MSG) fprintf(stderr,"%s:%d: CBFlib error: %s\n",__FILE__,__LINE__,MSG)
#define CBF_PRINT_WARNING(MSG) fprintf(stderr,"%s:%d: CBFlib warning: %s\n",__FILE__,__LINE__,MSG)
#define CBF_PRINT_ERROR2(MSG1,MSG2) fprintf(stderr,"%s:%d: CBFlib error: %s%s\n",__FILE__,__LINE__,MSG1,MSG2)
#define CBF_PRINT_WARNING2(MSG1,MSG2) fprintf(stderr,"%s:%d: CBFlib warning: %s%s\n",__FILE__,__LINE__,MSG1,MSG2)

/*
Define some conditional debug printing macros, in safe and unsafe forms.

TODO: Debug output should be enabled/disabled on a per-source-file or
per-fragment basis by (re?)defining a single flag, to give detailed output
relating to bugs; not throughout the whole library, giving an un-readable
mess of often incorrect (due to the semantically incorrect handling of
'CBF_NOTFOUND' in many functions) and almost entirely irrelevent information.

This will always expand a call of the form 'CBF_PRINT_DEBUG(LVL,MSG);' into a
single statement. This may be a 'do {..} while (0);' statement, a single call
to 'fprintf()', or the null statement ';'. It must _always_ be followed by a
semicolon, which either terminates the statement or provides it.

The value used for 'LVL' _must_ be a small literal integer or a preprocessor
token, for example:

#define INFO 3
CBF_PRINT_DEBUG(3,"foo");
CBF_PRINT_DEBUG(INFO,"bar");

The folowing is not valid:

const int level = 3;
CBF_PRINT_DEBUG(level,"foo");

For maximum portability, the value used for the 'CBF_PRINT_DEBUG_LEVEL' should
be a literal integer and for 'LVL' should be a literal integer or a symbol
which is defined as a literal integer.

The value given for 'MSG' should be a single string, as a literal character
array or as a variable which is a pointer to the start of a character array.
The result of 'cbf_strerror()' is a valid value to use here, as:

CBF_PRINT_DEBUG(3,cbf_strerror(error));
*/
#define CBF_PRINT_DEBUG_IMPL(MSG) fprintf(stderr,"%s:%d: debug: %s\n",__FILE__,__LINE__,(MSG))
#if CBF_USE_SAFE_PRINT_DEBUG
	/*
	Safe version: debug code always visible to the compiler, to prompt the
	maintainer to keep it up-to-date.

	This version allows switchable debug levels on a per-fragment basis and
	runtime-selection of the debug level to be printed, for ease of use.
	*/
	#define CBF_PRINT_DEBUG(LVL,MSG) do {if ((CBF_PRINT_DEBUG_LEVEL)>=(LVL)) CBF_PRINT_DEBUG_IMPL(MSG);} while (0)
#else
	/*
	Unsafe version: hidden from the compiler, so not as likely to work when you
	need to use it.

	This version always expands to a single unconditional statement, for speed.
	*/
	#define CBF_PRINT_DEBUG_LVL(LVL) CBF_PRINT_DEBUG_LVL##LVL
	#define CBF_PRINT_DEBUG(LVL,MSG) CBF_PRINT_DEBUG_LVL(LVL)(MSG)
	#if CBF_PRINT_DEBUG_LEVEL >= 0
		#define CBF_PRINT_DEBUG_LVL0(MSG) CBF_PRINT_DEBUG_IMPL(MSG)
	#else
		#define CBF_PRINT_DEBUG_LVL0(MSG)
	#endif
	#if CBF_PRINT_DEBUG_LEVEL >= 1
		#define CBF_PRINT_DEBUG_LVL1(MSG) CBF_PRINT_DEBUG_IMPL(MSG)
	#else
		#define CBF_PRINT_DEBUG_LVL1(MSG)
	#endif
	#if CBF_PRINT_DEBUG_LEVEL >= 2
		#define CBF_PRINT_DEBUG_LVL2(MSG) CBF_PRINT_DEBUG_IMPL(MSG)
	#else
		#define CBF_PRINT_DEBUG_LVL2(MSG)
	#endif
	#if CBF_PRINT_DEBUG_LEVEL >= 3
		#define CBF_PRINT_DEBUG_LVL3(MSG) CBF_PRINT_DEBUG_IMPL(MSG)
	#else
		#define CBF_PRINT_DEBUG_LVL3(MSG)
	#endif
	#if CBF_PRINT_DEBUG_LEVEL >= 4
		#define CBF_PRINT_DEBUG_LVL4(MSG) CBF_PRINT_DEBUG_IMPL(MSG)
	#else
		#define CBF_PRINT_DEBUG_LVL4(MSG)
	#endif
	#if CBF_PRINT_DEBUG_LEVEL >= 5
		#define CBF_PRINT_DEBUG_LVL5(MSG) CBF_PRINT_DEBUG_IMPL(MSG)
	#else
		#define CBF_PRINT_DEBUG_LVL5(MSG)
	#endif
#endif

/* debug print macros, enabled if CBFDEBUG defined */

#ifdef CBFDEBUG
#define cbf_debug_print(ARG) \
  {fprintf(stderr,__FILE__":%d: CBFlib debug: %s\n", __LINE__, ARG);}
#define cbf_debug_print2(FMT,ARG) \
  {fprintf(stderr,__FILE__":%d: CBFlib debug: " FMT "\n", __LINE__, ARG);}
#define cbf_debug_print3(FMT,ARG0,ARG1) \
  {fprintf(stderr,__FILE__":%d: CBFlib debug: " FMT "\n", __LINE__, ARG0,ARG1);}
#define cbf_debug_print4(FMT,ARG0,ARG1,ARG2) \
  {fprintf(stderr,__FILE__":%d: CBFlib debug: " FMT "\n", __LINE__, ARG0,ARG1,ARG2);}
#define cbf_debug_print5(FMT,ARG0,ARG1,ARG2,ARG3) \
  {fprintf(stderr,__FILE__":%d: CBFlib debug: " FMT "\n", __LINE__, ARG0,ARG1,ARG2,ARG3);}
#else
#define cbf_debug_print(ARG)
#define cbf_debug_print2(FMT,ARG)
#define cbf_debug_print3(FMT,ARG0,ARG1)
#define cbf_debug_print4(FMT,ARG0,ARG1,ARG2)
#define cbf_debug_print5(FMT,ARG0,ARG1,ARG2,ARG3)
#endif
    
#define cbf_printnez(f) \
{ \
const int err = (f); \
if (CBF_SUCCESS != err) \
  fprintf(stderr, __FILE__":%d: CBFlib error in '" #f "': %s\n", __LINE__, cbf_strerror(err)); \
}



#ifdef CBFDEBUG

#ifndef __FILE__

#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d \n", err); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d \n", err); \
                         { c; } return err; }}
#define cbf_reportnez(x,cerr) {int err; if (!(cerr)) {err = (x); (cerr)|=err; \
                  if (err) { fprintf (stderr, "\nCBFlib error %d \n", err);}}}

#else
#ifndef __func__
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); \
                         { c; } return err; }}
#define cbf_reportnez(x,cerr) {int err; if (!(cerr)) {err = (x); (cerr)|=err; \
                       if (err) { fprintf (stderr, \
                       "\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__);}}}
#else
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); return err; }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); \
                         { c; } return err; }}

#define cbf_reportnez(x,cerr) {int err; if (!(cerr)) {err = (x); (cerr)|=err; \
                      if (err) { fprintf (stderr, \
                      "\nCBFlib error %d at %s:%d(%s)\n", \
                      err,__FILE__,__LINE__,__func__);}}}
#endif
#endif

#else

#define cbf_failnez(f) { int err; err = (f); if (err) return err; }

#define cbf_onfailnez(f,c) { int err; err = (f); if (err) {{ c; } return err;}}

#define cbf_reportnez(x,cerr) {int err; if (!(cerr)) {err = (x); (cerr)|=err;}}

#endif

/*
Macro to suppress warnings about unused variables in functions that don't need
to use all of their variables.
    
Can be used as:
void f
	(int x)
{
	CBF_UNUSED(x);
	return;
}
*/
#define CBF_UNUSED(x) ((void)((x)))
    
/* macros to define arrays with variable dimensions */
    
#define CBF_START_ARRAY(type,name,dimension) \
    { type * name; \
      if (!cbf_alloc((void **) &(name),NULL,sizeof(type),dimension)) {

#define CBF_END_ARRAY(name) \
      cbf_free((void **) &(name), NULL); } \
    }

#define CBF_END_ARRAY_REPORTNEZ(name,error) \
     cbf_free((void **) &(name), NULL); } else {error|=CBF_ALLOC;} \
}


  /* string for an error */
    
const char * cbf_strerror(const int err);

  /* cbf handle */

typedef struct _cbf_handle_struct
{
  cbf_node *node;
  
  struct _cbf_handle_struct *dictionary;
  
  cbf_file * file;                   /* NULL or an active cbf_file for input */
      
  cbf_file * commentfile;            /* NULL or file for whitespace and comments */

  int  startcolumn, startline;       /* starting location of last token */
  
  FILE * logfile;                    /* NULL or an active stream for error logging */
  
  int warnings, errors;

  int refcount, row, search_row;
}
cbf_handle_struct;

typedef cbf_handle_struct *cbf_handle;

    
    /* 3D array 1-based indexing macro */
    
#define cbf_offset_1_3(i,j,k,dimfast,dimmid) ((i-1)+(dimfast)*(j-1)+(dimfast)*(dimmid)*(k-1))
    
    /* 3D array 0-based indexing macro */
    
#define cbf_offset_0_3(i,j,k,dimfast,dimmid) ((i)+(dimfast)*(j)+(dimfast)*(dimmid)*(k))


  /* Prototypes */
    
  /* Set a logfile in a handle */
    
int cbf_set_cbf_logfile (cbf_handle handle, FILE * logfile);

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


  /* Set the name of the current column */
    
int cbf_set_column_name (cbf_handle handle, const char *columnname);


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

    
  /* Get the (long) numeric value of the current (row, column) entry */
  
int cbf_get_longvalue (cbf_handle handle, long *number);


  /* Get the (double) numeric value of the current (row, column) entry */
  
int cbf_get_doublevalue (cbf_handle handle, double *number);


  /* Set the ascii value of the current (row, column) entry from an int */
  
int cbf_set_integervalue (cbf_handle handle, int number);


  /* Set the ascii value of the current (row, column) entry from a double */
  
int cbf_set_doublevalue (cbf_handle handle, const char *format, double number);


 /* Get the name of the current save frame */

int cbf_saveframe_name (cbf_handle handle, const char **saveframename);


  /* Get the ascii value of the current (row, column) entry,
     setting it to a default value if necessary */

int cbf_require_value (cbf_handle handle, const char **value, 
                                          const char *defaultvalue);


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

    /* Get the dimensions of the current (row, column) array entry
     from the CBF tags */
    
    
    int cbf_get_arraydimensions(cbf_handle handle,
                                size_t * dimover,
                                size_t * dimfast,
                                size_t * dimmid,
                                size_t * dimslow);
        

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

  /* issue a log message for a cbf_file */
  
void cbf_flog (cbf_file * file, const char *message, int logflags);


  /* Find a datablock, creating it if necessary */
  
int cbf_require_datablock (cbf_handle  handle,
                             const char *datablockname);

  /* Find a saveframe, creating it if necessary */
  
int cbf_require_saveframe (cbf_handle  handle,
                             const char *saveframename);

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
  /* check a category for all required tags and for parent tags */
    
int cbf_check_category_tags(cbf_handle handle, cbf_node* category, cbf_node* parent);

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
    
    /* get accumulator bit length */
    
int cbf_mpint_get_acc_bitlength(unsigned int * acc, size_t acsize, size_t * bitlength);

  /* Check value of type validity */
  
int cbf_check_type_contents(const char *type, const char *value);

  /* Regex Match function */

int cbf_match(const char *string, char *pattern);

  /* Interpreter for dREL method expression */

int cbf_drel(cbf_handle handle, cbf_handle dict, 
                               const char *mainitemname, 
                               const char *datablock, 
                               const char *expression);

  /* Construct Functions dictionary */

int cbf_construct_functions_dictionary(cbf_handle dict, const char *datablockname, const char *functionname);
    
    /* return a string for a CBF error */
    
    const char * cbf_strerror(const int err);

#ifdef __cplusplus

}

#endif

#endif /* CBF_H */

