/**********************************************************************
 *          cif2cbf -- convert a cif to a cbf file                    *
 *                                                                    *
 * Version 0.9.5.13  13 October 2015                                  *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006 -- 2015 Herbert J. Bernstein                    *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
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

/**********************************************************************
 *                                SYNOPSIS                            *
 *                                                                    *
 *  cif2cbf [-i input_cif] [-o output_cbf] \                          *
 *    [-u update_cif ] \                                              *
 *    [-b {f[orward]|b[ackwards]}] \                                  *
 *    [-B {read|liberal|noread}] [-B {write|nowrite}] \               *
 *    [-c {p[acked]|c[annonical]|{b[yte_offset]}|\                    *
 *        {v[2packed]}|{f[latpacked]}|{I|nIbble_offset}|{L|LZ4}|      *
 *        {2|LZ4**2}|n[n[one]}] \                                     *
 *    [-C highclipvalue ] \                                           *
 *    [-D ] \                                                         *
 *    [-d {d[igest]|n[odigest]|w[warndigest]} \                       *
 *    [-e {b[ase64]|k|q[uoted-printable]| \                           *
 *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
 *    [-I {0|2|4|8}] \                                                *
 *    [-L lowclipvalue ] \                                            *
 *    [-m {h[eaders]|n[oheaders]}] \                                  *
 *    [-m {dim[ensions]|nod[imensions}] \                             *
 *    [-p {0|1|2|4}] \                                                *
 *    [-R {0|4|8} \                                                   *
 *    [-S {read|noread}] [-S {write|nowrite}] \                       *
 *    [-T {read|noread}] [-T {write|nowrite}] \                       *
 *    [-v dictionary]* [-w] [-D]\                                     *
 *    [-O] \                                                          *
 *    [-5 {r|w|rw|rn|wn|rwn|n[oH5]} \                                 *
 *    [--register {manual|plugin}] \                                  *
 *    [--add-minicbf-header] \                                        *
 *    [--minicbf] \                                                   *
 *    [--data-last] \                                                 *
 *    [--{eoi|elements-of-interest} element[,elementhigh] \           *
 *    [--{foi|frames-of-interest}  frame[,framehigh] \                *
 *    [--{roi|region-of-interest} fastlow,fasthigh\                   *
 *                                [,midlow,midhigh[,slowlow,slowhigh]]*
 *    [--add-update-data]                                             *
 *    [--subtract-update-data]                                        *
 *    [--merge-datablocks-by-number]                                  *
 *    [--merge-datablocks-by-name]                                    *
 *    [-U n] \                                                        *
 *    [input_cif] [output_cbf]                                        *
 *                                                                    *
 *  the options are:                                                  *
 *                                                                    *
 *  -i input_cif (default: stdin)                                     *
 *    the input  file in CIF or CBF  format.  If input_cif is not     *
 *    specified or is given as "-", it is copied from stdin to a      *
 *    temporary file.                                                 *
 *                                                                    *
 *  -o output_cbf (default: stdout)                                   *
 *    the output cif (if base64 or quoted-printable encoding is used) *
 *    or cbf (if no encoding is used).  if no output_cif is specified *
 *    or is given as "-", the output is written to stdout             *
 *    if the output_cbf is /dev/null, no output is written.           *
 *                                                                    *
 *  -u update_cif (no default)                                        *
 *    and optional second input file in CIF or CBF format containing  *
 *    data blocks to be merged with data blocks from the primary      *
 *    input CIF or CBF                                                *
 *                                                                    *
 *  The remaining options specify the characteristics of the          *
 *  output cbf.  Most of the characteristics of the input cif are     *
 *  derived from context, except when modified by the -B, -S, -T, -v  *
 *  and -w flags.                                                     *
 *                                                                    *
 *  -b byte_order (forward or backwards, default forward (1234) on    *
 *    little-endian machines, backwards (4321) on big-endian machines *
 *                                                                    *
 *  -B [no]read or liberal (default noread)                           *
 *    read to enable reading of DDLm style brackets                   *
 *    liberal to accept whitespace for commas                         *
 *                                                                    *
 *  -B [no]write (default write)                                      *
 *    write to enable writing of DDLm style brackets                  *
 *                                                                    *
 *  -c compression_scheme (Packed, Canonical, Byte_offset,            *
 *    V2packed, Flatpacked, nIbble, zlib, LZ4, or LZ4**2 or None,     *
 *    default packed)                                                 *
 *                                                                    *
 *  -C highclipvalue                                                  *
 *    specifies a double precision value to which to clip the data    *
 *                                                                    *
 *  -d [no]digest or warndigest  (default md5 digest [R. Rivest,      *
 *    RFC 1321, April 1992 using"RSA Data Security, Inc. MD5          *
 *    Message-Digest Algorithm"] when MIME headers are selected)      *
 *                                                                    *
 *  -D test cbf_construct_detector                                    *
 *                                                                    *
 *  -e encoding (base64, k, quoted-printable or none, default base64) *
 *    specifies one of the standard MIME encodings for an ascii cif   *
 *    or "none" for a binary cbf                                      *
 *                                                                    *
 *  -I 0 or integer element size                                      *
 *    specifies integer conversion of the data, 0 to use the input    *
 *    number of bytes, 2, 4 or 8 for short, long or long long         *
 *    output integers                                                 *
 *                                                                    *
 *  -L lowclipvalue                                                   *
 *    specifies a double precision value to cut off the data from     *
 *    below                                                           *
 *                                                                    *
 *  -m [no]headers (default headers)                                  *
 *    selects MIME (N. Freed, N. Borenstein, RFC 2045, November 1996) *
 *    headers within binary data value text fields.                   *
 *                                                                    *
 *  -m [nod]imensions (default dimensions)                            *
 *    selects detailed recovery of dimensions from the input CIF      *
 *    for use in the MIME header of the output CIF                    *
 *                                                                    *
 *  -p K_of_padding (0, 1, 2, 4) for no padding after binary data     *
 *    1023, 2047 or 4095 bytes of padding after binary data           *
 *                                                                    *
 *  -R 0 or integer element size                                      *
 *    specifies real conversion of the data, 0 to use the input       *
 *    number of bytes,  4 or 8 for float or double output reals       *
 *                                                                    *
 *  -S [no]read or (default noread)                                   *
 *    read to enable reading of whitespace and comments               *
 *                                                                    *
 *  -S [no]write (default write)                                      *
 *    write to enable writing of whitespace and comments              *
 *                                                                    *
 *  -T [no]read or (default noread)                                   *
 *    read to enable reading of DDLm style triple quotes              *
 *                                                                    *
 *  -T [no]write (default write)                                      *
 *    write to enable writing of DDLm style triple quotes             *
 *                                                                    *
 *  -v dictionary specifies a dictionary to be used to validate       *
 *    the input cif and to apply aliases to the output cif.           *
 *    This option may be specified multiple times, with dictionaries  *
 *    layered in the order given.                                     *
 *                                                                    *
 *  -w process wide (2048 character) lines                            *
 *                                                                    *
 *  -W write wide (2048 character) lines                              *
 *                                                                    *
 *  -5 hdf5mode specifies whether to read and/or write in hdf5 mode   *
 *     the n parameter will cause the CIF H5 datablock to be deleted  *
 *     on both read and write, for both CIF, CBF and HDF5 files       *
 *                                                                    *
 *  --register manual or plugin (default plugin)                      *
 *     controls whether to rely on the HDF5 filter plugin mechanism   *
 *     or to manually register the CBFlib compression for HDF5        *
 *                                                                    *
 *  --add-minicbf-header                                              *
 *     add a minicbf header or replaces an exiting minicbf header     *
 *     in _array_data.header_contents                                 *
 *                                                                    *
 *  --minicbf                                                         *
 *     convert to a minicbf, implies --add-minicbf_header and         *
 *     --data-last                                                    *
 *                                                                    *
 *  --data-last                                                       *
 *     move array_data to be the last category and _array_data.data   *
 *     to be the last tag in that category                            *
 *                                                                    *
 *  --{eoi|elements-of-interest} element                              *
 *  --{eoi|elements-of-interest} element,elementhigh                  *
 *     delector elements of interest.  If only a single element       *
 *     is specified, only that one element is used                    *
 *                                                                    *
 *  --{foi|frames-of-interest} frame                                  *
 *  --{foi|frames-of-interest} frame,framehigh                        *
 *     frames of interest.  If only a single frame                    *
 *     is specified, only that one frame is used                      *
 *                                                                    *
 *  --roi fastlow,fasthigh                                            *
 *  --roi fastlow,fasthigh,slowlow,slowhigh                           *
 *  --roi fastlow,fasthigh,midlow,midhigh,slowlow,slowhigh            *
 *     restrict data transfers to the indicated range                 *
 *                                                                    *
 *  --region-of-interest  ...                                         *
 *     synonym for roi                                                *
 *                                                                    *
 *  --add-update-data                                                 *
 *  --subtract-update-data                                            *
 *     if an update cbf with data is provided via the -u option       *
 *     the add-update-data option causes the updated data to be added *
 *     pixel-by-pixel to the input cbf data, and                      *
 *     the subtract-update-data option causes the updated date to be  *
 *     subtracted pixe;-by-pixel from the input cbf data              *
 *     In both operations, ~0 in either file overrides the operation  *
 *                                                                    *
 *  --merge-datablocks-by-number                                      *
 *     when merging an update cif, align datablocks by their ordinal  *
 *     rather than by their names                                     *
 *  --merge-datablocks-by-name                                        *
 *     when merging an update cif, align datablocks by their names    *
 *     rather than by their ordinals                                  *
 *                                                                    *
 *  --U n                                                             *
 *     test cbf_construct_detector in element_id n                    *
 *                                                                    *
 *  -O when in -5 w (hdf5 write) mode, -O forces the use of opaque    *
 *     objects for CBF binaries                                       *
 *                                                                    *
 *                                                                    *
 *                                                                    *
 **********************************************************************/

/**********************************************************************
 *                                CREDITS                             *
 *                                                                    *
 *  This program is a Crystallographic Information File (CIF)         *
 *  application.  Please see the IUCR Policy below.   See the IUCR    *
 *  web page (http://www.iucr.org) or its mirrors for background      *
 *  and references on CIF.                                            *
 *                                                                    *
 *  This program is a Crystallographic Binary File (CBF) application. *
 *  Please see the ImgCIF/CBF web page at                             *
 *                                                                    *
 *          http://www.bernstein-plus-sons.com/software/CBF           *
 *                                                                    *
 *                                                                    *
 *  This program uses routines derived from mpack/munpack version     *
 *  1.5, ftp://ftp.andrew.cmu.edu/pub/mpack by John G. Myers,         *
 *  jgm+@cmu.edu.  "Mpack and munpack are utilties for encoding and   *
 *  decoding ... binary files in MIME ... format."  Please see the    *
 *  copyright notices and disclaimers in the mpack/munpack routines   *
 *                                                                    *
 *  This program uses routines derived from the "RSA Data Security,   *
 *  Inc. MD5 Message-Digest Algorithm."  Please see the copyright     *
 *  notice and disclaimer in md5c.c                                   *
 **********************************************************************/


/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term 'this software', as used in these Notices, refers to      *
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
 *                                 NOTICE                             *
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

#include "cbf.h"
#include "cbf_simple.h"
#include "img.h"
#include "cbf_string.h"
#include "cbf_copy.h"
#include "cbf_hdf5.h"
#include "cbf_alloc.h"
#include "cbf_minicbf_header.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include "cbf_getopt.h"
#include <unistd.h>

#define C2CBUFSIZ 8192
#define NUMDICTS 50

#ifdef __MINGW32__
#define NOMKSTEMP
#define NOTMPDIR
#endif

#define HDR_FINDDIMS    0x0040  /* On read, find header dims          */
#define HDR_NOFINDDIMS  0x0080  /* On read, don't find header dims    */

#define HDF5_READ_MODE  0x0001  /* Read the input file as HDF5        */
#define HDF5_WRITE_MODE 0x0002  /* Write the output file as HDF5      */
#define HDF5_NONE       0x0004  /* Flag for neither                   */



int local_exit (int status);
int outerror(int err);

int outerror(int err)
{

    if ((err&CBF_FORMAT)==CBF_FORMAT)
        fprintf(stderr, " cif2cbf: The file format is invalid.\n");
    if ((err&CBF_ALLOC)==CBF_ALLOC)
        fprintf(stderr, " cif2cbf Memory allocation failed.\n");
    if ((err&CBF_ARGUMENT)==CBF_ARGUMENT)
        fprintf(stderr, " cif2cbf: Invalid function argument.\n");
    if ((err&CBF_ASCII)==CBF_ASCII)
        fprintf(stderr, " cif2cbf: The value is ASCII (not binary).\n");
    if ((err&CBF_BINARY)==CBF_BINARY)
        fprintf(stderr, " cif2cbf: The value is binary (not ASCII).\n");
    if ((err&CBF_BITCOUNT)==CBF_BITCOUNT)
        fprintf(stderr, " cif2cbf: The expected number of bits does"
                " not match the actual number written.\n");
    if ((err&CBF_ENDOFDATA)==CBF_ENDOFDATA)
        fprintf(stderr, " cif2cbf: The end of the data was reached"
                " before the end of the array.\n");
    if ((err&CBF_FILECLOSE)==CBF_FILECLOSE)
        fprintf(stderr, " cif2cbf: File close error.\n");
    if ((err&CBF_FILEOPEN)==CBF_FILEOPEN)
        fprintf(stderr, " cif2cbf: File open error.\n");
    if ((err&CBF_FILEREAD)==CBF_FILEREAD)
        fprintf(stderr, " cif2cbf: File read error.\n");
    if ((err&CBF_FILESEEK)==CBF_FILESEEK)
        fprintf(stderr, " cif2cbf: File seek error.\n");
    if ((err&CBF_FILETELL)==CBF_FILETELL)
        fprintf(stderr, " cif2cbf: File tell error.\n");
    if ((err&CBF_FILEWRITE)==CBF_FILEWRITE)
        fprintf(stderr, " cif2cbf: File write error.\n");
    if ((err&CBF_IDENTICAL)==CBF_IDENTICAL)
        fprintf(stderr, " cif2cbf: A data block with the new name already exists.\n");
    if ((err&CBF_NOTFOUND)==CBF_NOTFOUND)
        fprintf(stderr, " cif2cbf: The data block, category, column or"
                " row does not exist.\n");
    if ((err&CBF_OVERFLOW)==CBF_OVERFLOW)
        fprintf(stderr, " cif2cbf: The number read cannot fit into the "
                "destination argument.\n        The destination has been set to the nearest value.\n");
    if ((err& CBF_UNDEFINED)==CBF_UNDEFINED)
        fprintf(stderr, " cif2cbf: The requested number is not defined (e.g. 0/0).\n");
    if ((err&CBF_NOTIMPLEMENTED)==CBF_NOTIMPLEMENTED)
        fprintf(stderr, " cif2cbf: The requested functionality is not yet implemented.\n");
    if ((err&CBF_H5ERROR)==CBF_H5ERROR)
        fprintf(stderr, " cif2cbf: HDF5 API error.\n");
    return 0;

}

#undef cbf_failnez
#undef cbf_onfailnez

#ifndef __FILE__

#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d \n", err); outerror(err); local_exit (-1); }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d \n", err); \
{ c; } outerror(err); local_exit (-1); }}
#else
#ifndef __func__
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); outerror(err); local_exit (-1); }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d\n", err,__FILE__,__LINE__); \
{ c; } outerror(err); local_exit (-1); }}
#else
#define cbf_failnez(x) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); outerror(err); local_exit (-1); }}

#define cbf_onfailnez(x,c) {int err; err = (x); if (err) { fprintf (stderr, \
"\nCBFlib error %d at %s:%d(%s)\n", err,__FILE__,__LINE__,__func__); \
{ c; } outerror(err); local_exit (-1); }}

#endif
#endif



/* Find row by multiple columns
 n is the number of columns
 column is an array of n column names
 value is an array of values to match
 
 */

static int cbf_find_row_by_columns(cbf_handle handle, int n,
                                   const char * column[],
                                   const char * value[]){
    
    int icol;
    
    int match;
    
    const char *colvalue;
    
    if (!handle || n < 1
        || !column || !value
        || !column[0] || !value[0]) return CBF_ARGUMENT;
    
    cbf_failnez(cbf_rewind_row(handle));
    
    while (!cbf_find_column(handle,column[0])
           &&!cbf_find_nextrow(handle,value[0])) {
        
        match = 1;
        
        for (icol = 1; icol < n; icol++) {
            
            if ( !column[icol] || !value[icol] ) return CBF_ARGUMENT;
            
            cbf_failnez(cbf_find_column(handle,column[icol]));
            
            if (cbf_get_value(handle,&colvalue)
                || !colvalue
                || cbf_cistrcmp(colvalue,value[icol])) {
                
                match = 0;
                
                break;
                
            }
            
        }
        
        if (match) return CBF_SUCCESS;
        
    }
    
    return CBF_NOTFOUND;
    
}


/* Get the frame_id and frame_number for a given array_id and binary_id */

static int cbf_get_frame(cbf_handle handle,
                  const char * array_id,
                  const char * binary_id,
                  const char * * frame_id,
                  unsigned int * frame_number){
    
    const char * xframe_id;
    
    const char * columns[2] = {"array_id", "binary_id"};
    
    const char * values[2] = {array_id, binary_id};
    
    int xframe_number;
    
    if (!handle || !array_id || !binary_id ) return CBF_ARGUMENT;
    
    if ((!cbf_find_category(handle,"diffrn_data_frame")
         ||!cbf_find_category(handle,"diffrn_frame_data"))
        && !cbf_rewind_row(handle)
        && !cbf_find_row_by_columns(handle,2,columns,values)
        && !cbf_find_column(handle,"id")
        && !cbf_get_value(handle,&xframe_id)){
        
        if (frame_id) *frame_id = xframe_id;
        
        if (!cbf_find_category(handle,"diffrn_scan_frame")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_column(handle,"frame_id")
            &&!cbf_find_row(handle,xframe_id)
            &&!cbf_find_column(handle,"frame_number")
            &&!cbf_get_integervalue(handle,&xframe_number)
            &&xframe_number >= 0) {
            
            if (frame_number) *frame_number = (unsigned int)xframe_number;
            return CBF_SUCCESS;
            
        }
        
        
    }
    
    return CBF_NOTFOUND;
    
}

/* Convert items of interest from a string to a range of elements or frames */

static int cbf_convertioi(const char *ioi, const size_t maxitems,
                   size_t * itemlow, size_t * itemhigh) {
    char * endptr;
    const char * str;
    if (!itemlow ) return CBF_ARGUMENT;
    *itemlow = 0;
    if (itemhigh) *itemhigh = maxitems-1;
    if (!ioi) {
        return CBF_SUCCESS;
    }
    str = ioi;
    *itemlow = (int)strtol(str,&endptr,0);
    if (*itemlow > maxitems-1) *itemlow = maxitems-1;
    if (*endptr == '\0') {
        if (itemhigh) *itemhigh = *itemlow;
        return CBF_SUCCESS;
    }
    if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
    if (!itemhigh) return CBF_SUCCESS;
    
    str = endptr+1;
    *itemhigh = (int)strtol(str,&endptr,0);
    if (*itemhigh < *itemlow) *itemhigh = *itemlow;
    if (*itemhigh > maxitems-1) *itemhigh = maxitems-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
    return CBF_SUCCESS;
}


void set_MP_terms(int crterm, int nlterm);

int main (int argc, char *argv [])
{
    FILE *in, *out=NULL, *update=NULL, *file, *dict;
    cbf_h5handle h5in, h5out;
    clock_t a,b;
    cbf_handle cif = NULL;
    cbf_handle ucif = NULL;
    cbf_handle cbf = NULL;
    cbf_handle altcbf = NULL;
    cbf_handle cbfsave = NULL;
    cbf_handle dic = NULL;
    cbf_handle odic;
    cbf_getopt_handle opts;
    int hdf5mode = 0;
    int hdf5noH5 = 0;
    int hdf5register = 0;
    int devnull = 0;
    int c;
    int errflg = 0;
    int opaquemode = 0;
    int add_minicbf_header = 0;
    int minicbf = 0;
    int data_last = 0;
    const char *cifin, *cbfout, *updatecif;
    const char *hdf5out;
    const char *dictionary[NUMDICTS];
    int dqrflags[NUMDICTS];
    char *ciftmp=NULL;
#ifndef NOMKSTEMP
    int ciftmpfd;
#endif
    int ciftmpused;
    int padflag;
    int dimflag;
    size_t nbytes;
    int ndict = 0;
    int kd;
    int wide = 0;
    int Wide = 0;
    int IorR = 0;
    int i5;
    int nelsize;
    int testconstruct;
    char buf[C2CBUFSIZ];
    unsigned int blocks, categories, blocknum, catnum, blockitems, itemnum;
    unsigned int elements;
    size_t elementlow, elementhigh;
    size_t framelow, framehigh;
    CBF_NODETYPE itemtype;
    const char *datablock_name;
    const char *saveframe_name;
    const char *category_name;
    const char *column_name;
    const char *value;
    const char *eoi;  /* elements of interest */
    const char *foi;  /* frames of interest */
    const char *roi;  /* region of interest */
    unsigned int colnum, rownum;
    unsigned int columns;
    unsigned int rows;
    int add_update_data=0;
    int subtract_update_data=0;
    int merge_datablocks_by_number=-1;
    double cliphigh, cliplow;

    int mime, digest, encoding, compression, h5compression, bytedir, cbforcif, term;
    int qrflags, qwflags;

    const char * optarg;


    /* Extract options */

    /**********************************************************************
     *  cif2cbf [-i input_cif] [-o output_cbf] \                          *
     *    [-u update_cif] \                                               *
     *    [-b {b[ackwards]|f[orwards]}] \                                 *
     *    [-B {read|liberal|noread}] [-B {write|nowrite}] \               *
     *    [-c {p[acked]|c[annonical]|{b[yte_offset]}|\                    *
     *        {v[2packed]}|{f[latpacked]}|{I|nIbble_offset}| \            *
     *        {z[lib]}|{L[Z4]}|{2|[LZ4**2}|{|n[n[one]}] \                 *
     *    [-C highclipvalue] \                                            *
     *    [-d {d[igest]|n[odigest]|w[arndigest]} \                        *
     *    [-D ] \                                                         *
     *    [-e {b[ase64]|k|q[uoted-printable]| \                           *
     *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
     *    [-I {0|2|4|8}]  \                                               *
     *    [-L lowclipvalue ] \                                            *
     *    [-m {h[eaders]|noh[eaders]}] \                                  *
     *    [-m {d[imensions]|nod[imensions]] \                             *
     *    [-R {0|4|8}] \                                                  *
     *    [-S {read|noread}] [-S {write|nowrite}] \                       *
     *    [-T {read|noread}] [-T {write|nowrite}] \                       *
     *    [-p {0|1|2|4}] \                                                *
     *    [-v dictionary]* [-w] [-W] \                                    *
     *    [-5 {r|w|rw|rn|wn|rwn|n[oH5]} \                                 *
     *    [-O] \                                                          *
     *    [--register {manual|plugin}] \                                  *
     *    [--add-minicbf-header] \                                        *
     *    [--minicbf] \                                                   *
     *    [--data-last] \                                                 *
     *    [--{eoi|elements-of-interest} element[,elementhigh] \           *
     *    [--{foi|frames-of-interest}  frame[,framehigh] \                *
     *    [--{roi|region-of-interest}  fastlow,fasthigh,.... \            *
     *    [--add-update-data] \                                           *
     *    [--subtract-update-data] \                                      *
     *    [--merge-datablocks-by-number \                                 *
     *    [--merge-datablocks-by-name \                                  *
     *    [-U n] \                                                        *
     *    [input_cif] [output_cbf]                                        *
     *                                                                    *
     **********************************************************************/

    mime = 0;
    digest = 0;
    encoding = 0;
    compression = 0;
    h5compression = 0;
    bytedir = 0;
    ndict = 0;
    padflag = 0;
    qrflags = qwflags = 0;
    dimflag = 0;
    nelsize = 0;
    hdf5mode = 0;
    hdf5noH5 = 0;
    hdf5register = 0;
    unsigned int elno = 0;


    cifin = NULL;
    cbfout = NULL;
    cbfsave = NULL;
    hdf5out = NULL;
    updatecif = NULL;
    eoi = NULL;
    foi = NULL;
    roi = NULL;
    ciftmpused = 0;
    testconstruct = 0;
    cliphigh = cliplow = 0.;
    add_update_data = 0;
    subtract_update_data = 0;

    cbf_failnez(cbf_make_getopt_handle(&opts))

    cbf_failnez(cbf_getopt_parse(opts, argc, argv, "-i(input):" \
                                 "o(output):" \
                                 "u(update):" \
                                 "b(byte-direction):" \
                                 "B(parse-brackets):" \
                                 "c(compression):" \
                                 "C(cliphigh):" \
                                 "D(test-construct-detector)" \
                                 "d(digest):" \
                                 "e(encoding):" \
                                 "I(integer):"  \
                                 "L(cliplow):"  \
                                 "m(mime-header):" \
                                 "p(pad):" \
                                 "P(parse-level):" \
                                 "R(real):" \
                                 "S(white-space):" \
                                 "T(treble-quotes):" \
                                 "v(validation-dictionary):" \
                                 "5(hdf5):" \
                                 "Z(register):" \
                                 "U(construct-detector):" \
                                 "O(opaque)" \
                                 "w(read-wide)" \
                                 "W(write-wide)" \
                                 "\1(add-minicbf-header)" \
                                 "\2(minicbf)" \
                                 "\3(data-last)" \
                                 "\4(elements-of-interest):" \
                                 "\4(eoi):" \
                                 "\5(frames-of-interest):" \
                                 "\5(foi):" \
                                 "\6(region-of-interest):" \
                                 "\6(roi):" \
                                 "\7(help)" \
                                 "\x8(add-update-data)" \
                                 "\x9(subtract-update-data)" \
                                 "\xa(merge-datablocks-by-number)" \
                                 "\xb(merge-datablocks-by-name)"
                                 ));

    if (!cbf_rewind_getopt_option(opts))
        for(;!cbf_get_getopt_data(opts,&c,NULL,NULL,&optarg);cbf_next_getopt_option(opts)) {
            if (!c) break;
            switch (c) {
                case 'i':     /* input file */
                    if (cifin) errflg++;
                    else cifin = optarg;
                    break;
                    
                case 'o':     /* output file */
                    if (cbfout) errflg++;
                    else {
                        cbfout = optarg;
                    }
                    break;
                    
                case 'u':     /* update file */
                    if (updatecif) errflg++;
                    else updatecif = optarg;
                    break;
                    
                case 'b':     /* byte order */
                    if (bytedir) errflg++;
                    if (optarg[0] == 'f' || optarg[0] == 'F') {
                        bytedir = ENC_FORWARD;
                    } else {
                        if (optarg[0] == 'b' || optarg[0] == 'B' ) {
                            bytedir = ENC_BACKWARD;
                        } else {
                            errflg++;
                        }
                    }
                    break;
                    
                case 'B':
                    if (!strcmp(optarg,"cif20read")) {
                        qrflags &= ~CBF_PARSE_BRACKETS;
                        qrflags |= CBF_PARSE_BRC;
                    } else if (!strcmp(optarg,"nocif20read")) {
                        qrflags &= ~CBF_PARSE_BRACKETS;
                    } else if (!strcmp(optarg,"cif20write")) {
                        qwflags &= ~CBF_PARSE_BRACKETS;
                        qwflags |= CBF_PARSE_BRC;
                    } else if (!strcmp(optarg,"nocif20write")) {
                        qwflags &= ~CBF_PARSE_BRACKETS;
                    } else if (!strcmp(optarg,"read")) {
                        qrflags |= CBF_PARSE_BRACKETS;
                    } else if (!strcmp(optarg,"noread")) {
                        qrflags &= ~CBF_PARSE_BRACKETS;
                    } else if (!strcmp(optarg,"write")) {
                        qwflags |= CBF_PARSE_BRACKETS;
                    } else if (!strcmp(optarg,"nowrite")) {
                        qwflags  &= ~CBF_PARSE_BRACKETS;
                    } else errflg++;
                    break;
                    
                    
                case 'c':
                    if (compression) errflg++;
                    if (optarg[0] == 'p' || optarg[0] == 'P') {
                        h5compression = compression = CBF_PACKED;
                        h5compression |= CBF_H5COMPRESSION_CBF;
                    } else {
                        if (optarg[0] == 'c' || optarg[0] == 'C') {
                            h5compression = compression = CBF_CANONICAL;
                            h5compression |= CBF_H5COMPRESSION_CBF;
                        } else {
                            if (optarg[0] == 'b' || optarg[0] == 'B') {
                                h5compression = compression = CBF_BYTE_OFFSET;
                                h5compression |= CBF_H5COMPRESSION_CBF;
                            } else {
                                if (optarg[0] == 'n' || optarg[0] == 'N') {
                                    h5compression = compression = CBF_NONE;
                                } else {
                                    if (optarg[0] == 'v' || optarg[0] == 'V') {
                                        h5compression = compression = CBF_PACKED_V2;
                                        h5compression |= CBF_H5COMPRESSION_CBF;
                                    } else {
                                        if (optarg[0] == 'f' || optarg[0] == 'F') {
                                            h5compression = compression = CBF_PACKED|CBF_FLAT_IMAGE;
                                            h5compression |= CBF_H5COMPRESSION_CBF;
                                        } else {
                                            if (optarg[0] == 'i' || optarg[0] == 'I' || !cbf_cistrcmp(optarg,"nibble_offset")) {
                                                h5compression = compression = CBF_NIBBLE_OFFSET;
                                                h5compression |= CBF_H5COMPRESSION_CBF;
                                            } else {
                                                if (optarg[0] == 'z' || optarg[0] == 'Z') {
                                                    h5compression = CBF_H5COMPRESSION_ZLIB;
                                                    compression = CBF_NIBBLE_OFFSET;
                                                } else {
                                                    if (optarg[0] == 'l' || optarg[0] == 'L'){
                                                        h5compression = CBF_H5COMPRESSION_LZ4;
                                                        compression = CBF_NIBBLE_OFFSET;
                                                    } else {
                                                        if (optarg[0] == '2' || !cbf_cistrcmp(optarg,"LZ4**2")){
                                                            h5compression = CBF_H5COMPRESSION_LZ4_2;
                                                            compression = CBF_NIBBLE_OFFSET;
                                                        } else {
                                                            errflg++;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;
                    
                case 'C':
                    cliphigh = atof(optarg);
                    break;
                    
                case 'd':
                    if (digest) errflg++;
                    if (optarg[0] == 'd' || optarg[0] == 'H' ) {
                        digest = MSG_DIGEST;
                    } else {
                        if (optarg[0] == 'n' || optarg[0] == 'N' ) {
                            digest = MSG_NODIGEST;
                        } else {
                            if (optarg[0] == 'w' || optarg[0] == 'W' ) {
                                digest = MSG_DIGESTWARN;
                            } else {
                                errflg++;
                            }
                        }
                    }
                    break;
                    
                case 'D':  /* test construct_detector */
                    if (testconstruct) errflg++;
                    else testconstruct = 1;
                    elno = 0;
                    break;
                    
                case 'U': /* test construct detector on element number n */
                    if (testconstruct) errflg++;
                    else testconstruct = 1;
                    elno = atoi(optarg);
                    break;
                    
                    
                case '5': /* set hdf5 flags */
                    if (hdf5mode || hdf5noH5){
                        errflg++;
                    } else {
                        i5 = 0;
                        if (optarg[i5] == 'r' || optarg[i5] == 'R') {
                            hdf5mode |= HDF5_READ_MODE;
                            i5++;
                        }
                        if (optarg[i5] == 'w' || optarg[i5] == 'W') {
                            hdf5mode |= HDF5_WRITE_MODE;
                            i5++;
                        }
                        if (optarg[i5] == 'n' || optarg[i5] == 'N') {
                            hdf5noH5 = CBF_H5_NOH5;
                        }
                        if (!hdf5mode && !hdf5noH5){
                            errflg++;
                        }
                    }
                    break;
                    
                case 'Z': /* register compressions */
                    if (hdf5register) errflg++;
                    if (cbf_cistrcmp(optarg,"manual")== 0) {
                        hdf5register = CBF_H5_REGISTER_COMPRESSIONS;
                    } else if (cbf_cistrcmp(optarg,"plugin") == 0) {
                        hdf5register = 0;
                    } else {
                        errflg++;
                    }
                    break;
                    
                case '\1': /* add minicbf header */
                    if (add_minicbf_header) errflg++;
                    add_minicbf_header = 1;
                    break;
                    
                case '\2': /* minicbf output only */
                    if (minicbf) errflg++;
                    minicbf = 1;
                    add_minicbf_header = 1;
                    data_last = 1;
                    break;
                    
                case '\3': /* place data last */
                    if (data_last) errflg++;
                    data_last = 1;
                    break;
                    
                case '\4': /* elements of interest */
                    if (eoi) errflg++;
                    eoi = optarg;
                    break;
                    
                case '\5': /* frames of interest */
                    if (foi) errflg++;
                    foi = optarg;
                    break;
                    
                case '\6': /* region of interest */
                    if (roi) errflg++;
                    roi = optarg;
                    break;
                    
                case '\7': /* help */
                    errflg++;
                    break;
                    
                case '\x8': /* add-update-data */
                    if (add_update_data || subtract_update_data) errflg++;
                    add_update_data = 1;
                    break;
                    
                case '\x9': /* subtract-update-data */
                    if (add_update_data || subtract_update_data) errflg++;
                    subtract_update_data = 1;
                    break;
                    
                case '\xa': /* merge-datablocks-by-number */
                    if (merge_datablocks_by_number >= 0) errflg++;
                    merge_datablocks_by_number = 1;
                    break;
                    
                case '\xb': /* merge-datablocks-by-name */
                    if (merge_datablocks_by_number >= 0) errflg++;
                    merge_datablocks_by_number = 0;
                    break;
                    
                case 'O': /* set Opaque mode */
                    if (opaquemode) errflg++;
                    opaquemode = 1;
                    break;
                    
                case 'e':
                    if (encoding) errflg++;
                    if (optarg[0] == 'b' || optarg[0] == 'B' ) {
                        encoding = ENC_BASE64;
                    } else {
                        if (optarg[0] == 'k' || optarg[0] == 'K' ) {
                            encoding = ENC_BASE32K;
                        } else {
                            if (optarg[0] == 'q' || optarg[0] == 'Q' ) {
                                encoding = ENC_QP;
                            } else {
                                if (optarg[0] == 'd' || optarg[0] == 'D' ) {
                                    encoding = ENC_BASE10;
                                } else {
                                    if (optarg[0] == 'h' || optarg[0] == 'H' ) {
                                        encoding = ENC_BASE16;
                                    } else {
                                        if (optarg[0] == 'o' || optarg[0] == 'O' ) {
                                            encoding = ENC_BASE8;
                                        } else {
                                            if (optarg[0] == 'n' || optarg[0] == 'N' ) {
                                                encoding = ENC_NONE;
                                            } else {
                                                errflg++;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break;
                    
                case 'I':
                    if (IorR) errflg++;
                    IorR = CBF_CPY_SETINTEGER;
                    nelsize = atoi(optarg);
                    if (nelsize != 0 && nelsize != 1 && nelsize !=2 && nelsize !=4 && nelsize != 8) errflg++;
                    break;
                    
                case 'L':
                    cliplow = atof(optarg);
                    break;
                    
                    
                case 'm':
                    if (optarg[0] == 'h' || optarg[0] == 'H' ) {
                        if (mime) errflg++;
                        mime = MIME_HEADERS;
                    } else if (optarg[0] == 'd' || optarg[0] == 'D' ) {
                        if (dimflag) errflg++;
                        dimflag = HDR_FINDDIMS;
                    } else if (optarg[0] == 'n' || optarg[0] == 'N' ) {
                        if (!strncasecmp(optarg,"noh",3) ){
                            if (mime) errflg++;
                            mime = PLAIN_HEADERS;
                        } else if (!strncasecmp(optarg,"nod",3)) {
                            if (dimflag) errflg++;
                            dimflag = HDR_NOFINDDIMS;
                        } else {
                            errflg++;
                        }
                    } else {
                        errflg++;
                    }
                    break;
                    
                    
                case 'p':
                    if (padflag) errflg++;
                    if (optarg[0] == '1') {
                        padflag = PAD_1K;
                    } else if (optarg[0] == '2'){
                        padflag = PAD_2K;
                    } else if (optarg[0] == '4'){
                        padflag = PAD_4K;
                    } else errflg++;
                    break;
                    
                    
                case 'P':    /* Parse level */
                    if (!strcmp(optarg,"cif20read")) {
                        qrflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                        qrflags |= CBF_PARSE_BRC|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8;
                    } else if (!strcmp(optarg,"cif20write")) {
                        qwflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                        qwflags |= CBF_PARSE_BRC|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8;
                    } else if (!strcmp(optarg,"oldddlmread")) {
                        qrflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                        qrflags |= CBF_PARSE_BRACKETS|CBF_PARSE_WIDE;
                    } else if (!strcmp(optarg,"oldddlmwrite")) {
                        qwflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                        qwflags |= CBF_PARSE_BRACKETS|CBF_PARSE_WIDE;
                    } if (!strcmp(optarg,"cif11read")) {
                        qrflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                        qrflags |= CBF_PARSE_WIDE;
                    } else if (!strcmp(optarg,"cif11write")) {
                        qwflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                        qwflags |= CBF_PARSE_WIDE;
                    } else if (!strcmp(optarg,"cif10read")) {
                        qrflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                    } else if (!strcmp(optarg,"cif10write")) {
                        qwflags &= ~(CBF_PARSE_BRACKETS|CBF_PARSE_TQ|CBF_PARSE_CIF2_DELIMS|CBF_PARSE_WIDE|CBF_PARSE_UTF8);
                    } else errflg++;
                    break;
                    
                case 'R':
                    if (IorR) errflg++;
                    IorR = CBF_CPY_SETREAL;
                    nelsize = atoi(optarg);
                    if (nelsize != 0 && nelsize !=4 && nelsize != 8) errflg++;
                    break;
                    
                case 'S':  /* Parse whitespace */
                    if (!strcmp(optarg,"read")) {
                        qrflags |= CBF_PARSE_WS;
                    } else if (!strcmp(optarg,"noread")) {
                        qrflags &= CBF_PARSE_WS;
                    } else if (!strcmp(optarg,"write")) {
                        qwflags |= CBF_PARSE_WS;
                    } else if (!strcmp(optarg,"nowrite")) {
                        qwflags &= CBF_PARSE_WS;
                    } else errflg++;
                    break;
                    
                case 'T':  /* Parse treble quotes */
                    if (!strcmp(optarg,"read")) {
                        qrflags |= CBF_PARSE_TQ;
                    } else if (!strcmp(optarg,"noread")) {
                        qrflags &= ~CBF_PARSE_TQ;
                    } else if (!strcmp(optarg,"write")) {
                        qwflags |= CBF_PARSE_TQ;
                    } else if (!strcmp(optarg,"nowrite")) {
                        qwflags &= ~CBF_PARSE_TQ;
                    } else errflg++;
                    break;
                    
                case 'v':  /* validate against dictionary */
                    if (ndict < NUMDICTS) {
                        dqrflags[ndict] = qrflags;
                        dictionary[ndict++] = optarg;
                    } else if (ndict == NUMDICTS) {
                        errflg++;
                        ndict++;
                        fprintf(stderr, " Too many dictionaries, increase NUMDICTS");
                    }
                    break;
                    
                case 'w': /* read wide files */
                    if (wide) errflg++;
                    else wide = 1;
                    break;
                    
                case 'W': /* write wide files */
                    if (Wide) errflg++;
                    else Wide = 1;
                    break;
                    
                default:
                    errflg++;
                    break;
            }
        }
    for(;!cbf_get_getopt_data(opts,&c,NULL,NULL,&optarg);cbf_next_getopt_option(opts)) {
        if (!cifin) {
            cifin = optarg;
        } else {
            if (!cbfout) {
                cbfout = optarg;
                hdf5out = "hdf5test.h5";
            } else {
                errflg++;
            }
        }
    }
    if ((roi||!updatecif) && (add_update_data || subtract_update_data)) errflg++;
    if (errflg) {
        fprintf(stderr,"cif2cbf:  Usage: \n");
        fprintf(stderr,
                "  cif2cbf [-i input_cif] [-o output_cbf] \\\n");
        fprintf(stderr,
                "    [-u update_cif] \\\n");
        fprintf(stderr,
                "    [-c {p[acked]|c[annonical]|{b[yte_offset]}|\\\n");
        fprintf(stderr,
                "        {v[2packed]}|{f[latpacked]}|{I|nIbble_offset}|{L|LZ4}| \\\n");
        fprintf(stderr,
                "        {2|LZ4**2}|n[n[one]}] \\\n");
        fprintf(stderr,
                "    [-C highclipvalue] \\\n");
        fprintf(stderr,
                "    [-D ] \\\n");
        fprintf(stderr,
                "    [-I {0|2|4|8}] \\\n");
        fprintf(stderr,
                "    [-R {0|4|8}] \\\n");
        fprintf(stderr,
                "    [-L {0|4|8}] \\\n");
        fprintf(stderr,
                "    [-m {h[eaders]|noh[eaders]}] \\\n");
        fprintf(stderr,
                "    [-m {d[imensions]|nod[imensions}] \\\n");
        fprintf(stderr,
                "    [-d {d[igest]|n[odigest]|w[arndigest]}] \\\n");
        fprintf(stderr,
                "    [-B {read|liberal|noread}] [-B {write|nowrite}] \\\n");
        fprintf(stderr,
                "    [-S {read|noread}] [-S {write|nowrite}] \\\n");
        fprintf(stderr,
                "    [-T {read|noread}] [-T {write|nowrite}] \\\n");
        fprintf(stderr,
                "    [-e {b[ase64]|q[uoted-printable]|\\\n");
        fprintf(stderr,
                "                  d[ecimal]|h[examdecimal|o[ctal]|n[one]}] \\\n");
        fprintf(stderr,
                "    [-b {f[orward]|b[ackwards]}\\\n");
        fprintf(stderr,
                "    [-p {1|2|4}\\\n");
        fprintf(stderr,
                "    [-v dictionary]* [-w] [-W]\\\n");
        fprintf(stderr,
                "    [-5 {r|w|rw|rn|wn|rwn|n[oH5]}\\\n");
        fprintf(stderr,
                "    [-O] \\\n");
        fprintf(stderr,
                "    [--register {manual|plugin}] \\\n");
        fprintf(stderr,
                "    [--add-minicbf-header] \\\n");
        fprintf(stderr,
                "    [--minicbf] \\\n");
        fprintf(stderr,
                "    [--data-last] \\\n");
        fprintf(stderr,
                "    [--{eoi|elements-of-interest} element[,elementhigh] \\\n");
        fprintf(stderr,
                "    [--{foi|frames-of-interest}  frame[,framehigh] \\\n");
        fprintf(stderr,
                "    [--{roi|region-of-interest}\n"
                "    fastlow,fasthigh[[,midlow,midhigh[,slowlow,slowhigh]] \\\n");
        fprintf(stderr,
                "    [--{add-data-array} \\\n");
        fprintf(stderr,
                "    [--{subtract-data-array} \\\n");
        fprintf(stderr,
                "    [-U n] \\\n");
        fprintf(stderr,
                "    [input_cif] [output_cbf] \n\n");
        exit(2);
    }


    /* Set up for CIF of CBF output */

    if (!encoding) {
        encoding = ENC_BASE64;
    }
    cbforcif = CBF;
    term = ENC_CRTERM | ENC_LFTERM;
    if (encoding == ENC_BASE64 || \
        encoding == ENC_BASE32K || \
        encoding == ENC_QP || \
        encoding == ENC_BASE10 || \
        encoding == ENC_BASE16 || \
        encoding == ENC_BASE8) {
        cbforcif = CIF;
        term = ENC_LFTERM;
    }

    /* Set up for headers */

    if (!mime) {
        mime = MIME_HEADERS;
    }
    if (!digest) {
        if (mime == MIME_HEADERS) {
            digest = MSG_DIGEST;
        } else {
            digest = MSG_NODIGEST;
        }
    }

    if (!dimflag) dimflag = HDR_FINDDIMS;


    /* Set up for decimal, hexadecimal or octal output */
    if (!bytedir)
        bytedir = ENC_BACKWARD;

    /* Set up for Compression */

    if (!compression)
        compression = CBF_PACKED;
    if (!h5compression)
        h5compression = CBF_NIBBLE_OFFSET|CBF_H5COMPRESSION_CBF;


    /* Read the cif */

    if (!cifin || strcmp(cifin?cifin:"","-") == 0) {
        ciftmp = (char *)malloc(strlen("/tmp/cif2cbfXXXXXX")+1);
#ifdef NOTMPDIR
        strcpy(ciftmp, "cif2cbfXXXXXX");
#else
        strcpy(ciftmp, "/tmp/cif2cbfXXXXXX");
#endif
#ifdef NOMKSTEMP
        if ((ciftmp = mktemp(ciftmp)) == NULL ) {
            fprintf(stderr,"\n cif2cbf: Can't create temporary file name %s.\n", ciftmp);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
        if ( (file = fopen(ciftmp,"wb+")) == NULL) {
            fprintf(stderr,"Can't open temporary file %s.\n", ciftmp);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
#else
        if ((ciftmpfd = mkstemp(ciftmp)) == -1 ) {
            fprintf(stderr,"\n cif2cbf: Can't create temporary file %s.\n", ciftmp);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
        if ( (file = fdopen(ciftmpfd, "w+")) == NULL) {
            fprintf(stderr,"Can't open temporary file %s.\n", ciftmp);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
#endif
        while ((nbytes = fread(buf, 1, C2CBUFSIZ, stdin))) {
            if(nbytes != fwrite(buf, 1, nbytes, file)) {
                fprintf(stderr,"Failed to write %s.\n", ciftmp);
                exit(1);
            }
        }
        fclose(file);
        cifin = ciftmp;
        ciftmpused = 1;
    }


    if ( cbf_make_handle (&cif) ) {
        fprintf(stderr,"Failed to create handle for input_cif\n");
        exit(1);
    }

    if ( cbf_make_handle (&dic) ) {
        fprintf(stderr,"Failed to create handle for dictionary\n");
        exit(1);
    }


    if ( cbf_make_handle (&cbf) ) {
        fprintf(stderr,"Failed to create handle for output_cbf\n");
        exit(1);
    }

    altcbf = NULL;
    if ( (data_last || minicbf) && cbf_make_handle (&altcbf) ) {
        fprintf(stderr,"Failed to create handle for altcbf\n");
        exit(1);
    }

    if ( cbf_make_handle (&ucif) ) {
        fprintf(stderr,"Failed to create handle for update_cif\n");
        exit(1);
    }


    for (kd=0; kd< ndict; kd++) {

        if (!(dict = fopen (dictionary[kd], "rb")))  {
            fprintf (stderr,"Couldn't open the dictionary %s\n", dictionary[kd]);
            exit (1);
        }
        cbf_failnez(cbf_read_widefile(dic, dict, MSG_DIGEST|dqrflags[kd]))
        cbf_failnez(cbf_convert_dictionary(cif,dic))
        cbf_failnez(cbf_get_dictionary(cif,&odic))
        cbf_failnez(cbf_set_dictionary(cbf,odic))

    }

    a = clock ();

    /* Read the file */
    if (hdf5mode&HDF5_READ_MODE) {
        if (cbf_open_h5handle(&h5in,cifin)) {
            fprintf (stderr,"Couldn't open the input HDF5 file %s\n", cifin);
            exit (1);
        }
    } else {
        if (!(in = fopen (cifin, "rb"))) {
            fprintf (stderr,"Couldn't open the input CIF file %s\n", cifin);
            exit (1);
        }
    }

    if (ciftmpused) {
        if (unlink(ciftmp) != 0 ) {
            fprintf(stderr,"cif2cif:  Can't unlink temporary file %s.\n", ciftmp);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
    }

    if (hdf5mode&HDF5_READ_MODE) {
        cbf_failnez (cbf_read_h5file(cif, h5in, hdf5register|MSG_DIGEST|qrflags|(digest&MSG_DIGESTWARN)))
    } else {
        if (!wide) {
            cbf_failnez (cbf_read_file (cif, in, MSG_DIGEST|qrflags|(digest&MSG_DIGESTWARN)))
        } else {
            cbf_failnez (cbf_read_widefile (cif, in, MSG_DIGEST|qrflags|(digest&MSG_DIGESTWARN)))
        }
    }

    if (hdf5noH5) {

        if (!cbf_find_datablock(cif,"H5")) {

            cbf_failnez(cbf_remove_datablock(cif));

        }

    }

    cbf_failnez (cbf_rewind_datablock(cif))

    cbf_failnez (cbf_count_datablocks(cif, &blocks))

    if (cbf_count_elements(cif,&elements)) {
        elements = elementlow = elementhigh = 0;
    } else {
        elementlow = 0; elementhigh = elements?elements-1:elements;
        if (eoi) {
            cbf_failnez(cbf_convertioi(eoi,elements,&elementlow,&elementhigh));
        }
    }
    
    framelow = 0; framehigh = (int)1.e8;
    if (foi) {
        cbf_failnez(cbf_convertioi(eoi,framehigh+1,&framelow,&framehigh));
    }
    
    cbfsave = cbf;

    for (blocknum = 0; blocknum < blocks;  blocknum++ )
    { /* start of copy loop */


        cbf_failnez (cbf_select_datablock(cif, blocknum))
        cbf_failnez (cbf_datablock_name(cif, &datablock_name))
        cbf_failnez (cbf_force_new_datablock(cbf, datablock_name))
        if (altcbf) {
            cbf_failnez (cbf_force_new_datablock(altcbf, datablock_name))
        }

        if ( !cbf_rewind_blockitem(cif, &itemtype) ) {
            cbf_failnez (cbf_count_blockitems(cif, &blockitems))

            for (itemnum = 0; itemnum < blockitems;  itemnum++) {
                const char* array_id;
                const char* binary_id;
                cbf_select_blockitem(cif, itemnum, &itemtype);
                if (itemtype == CBF_CATEGORY) {
                    cbf_category_name(cif,&category_name);
                    if (altcbf && !cbf_cistrcmp(category_name,"array_data")) {
                        cbf = altcbf;
                    } else {
                        cbf = cbfsave;
                    }
                    cbf_force_new_category(cbf, category_name);
                    cbf_count_rows(cif,&rows);
                    cbf_count_columns(cif,&columns);
                    if (altcbf && !cbf_cistrcmp(category_name,"array_data")) {
                        if (minicbf || add_minicbf_header) {
                            cbf_failnez(cbf_new_column(altcbf,"header_convention"))
                            cbf_failnez(cbf_new_column(altcbf,"header_contents"))
                        }
                        /*  Transfer the column names from cif to altcbf, header
                          first, data last */
                        if ( ! cbf_rewind_column(cif) ) {
                            do {
                                cbf_failnez(cbf_column_name(cif, &column_name));
                                if (cbf_cistrcmp(column_name,"data") ) {
                                    cbf_new_column(altcbf, column_name);
                                }
                            } while ( ! cbf_next_column(cif) );
                            cbf_new_column(altcbf,"data");
                            cbf_rewind_column(cif);
                            cbf_rewind_row(cif);
                        }
                    } else {

                    /*  Transfer the columns names from cif to cbf */
                    if ( ! cbf_rewind_column(cif) ) {
                        do {
                            cbf_failnez(cbf_column_name(cif, &column_name))
                            cbf_failnez(cbf_new_column(cbf, column_name))
                        } while ( ! cbf_next_column(cif) );
                        cbf_rewind_column(cif);
                        cbf_rewind_row(cif);
                    }
                    }
                    /* Transfer the rows from cif to cbf */
                    for (rownum = 0; rownum < rows; rownum++ ) {
                        cbf_failnez (cbf_select_row(cif, rownum))
                        /* See if this is an array_data category row, and if, so,
                         log the array_id, binary_id, frame number and detector
                         element number for this row, if any
                         
                         Skip the row if excluded by the eoi or foi
                         
                         */
                        if (foi || eoi){
                            unsigned int element_number;
                            unsigned int frame_number;

                            if (cbf_find_column(cif,"array_id")
                                || cbf_get_value(cif,&array_id)) array_id = NULL;
                            if (cbf_find_column(cif,"binary_id")
                                || cbf_get_value(cif,&binary_id)) binary_id = NULL;
                            
                            if (elements && foi && !cbf_get_element_number(cif,NULL,array_id,NULL,&element_number)) {
                                if ((element_number %elements) < elementlow
                                    || (element_number %elements) > elementhigh) continue;
                            }
                            
                            if (foi && !cbf_get_frame(cif,array_id,binary_id, NULL, &frame_number)) {
                                if (frame_number < framelow
                                    || frame_number > framehigh) continue;
                            }
                            
                            /* recover the starting position */
                            cbf_failnez (cbf_select_blockitem(cif, itemnum, &itemtype));
                            cbf_failnez (cbf_rewind_column(cif));
                            cbf_failnez (cbf_select_row(cif, rownum));
                        }
                        
                        cbf_failnez (cbf_new_row(cbf))
                        cbf_rewind_column(cif);
                        for (colnum = 0; colnum < columns; colnum++ ) {
                            const char *typeofvalue;

                            cbf_failnez (cbf_select_column(cif, colnum))
                            cbf_failnez (cbf_column_name(cif, &column_name))

                            if ( ! cbf_get_value(cif, &value) ) {
                                if (compression && value && column_name && !cbf_cistrcmp("compression_type",column_name)) {
                                    cbf_failnez (cbf_select_column(cbf, colnum))
                                    switch (compression&CBF_COMPRESSION_MASK) {
                                        case (CBF_NONE):
                                            cbf_failnez (cbf_set_value      (cbf,"none"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        case (CBF_CANONICAL):
                                            cbf_failnez (cbf_set_value      (cbf,"canonical"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        case (CBF_PACKED):
                                            cbf_failnez (cbf_set_value      (cbf,"packed"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        case (CBF_PACKED_V2):
                                            cbf_failnez (cbf_set_value      (cbf,"packed_v2"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        case (CBF_BYTE_OFFSET):
                                            cbf_failnez (cbf_set_value      (cbf,"byte_offsets"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        case (CBF_NIBBLE_OFFSET):
                                            cbf_failnez (cbf_set_value      (cbf,"nibble_offset"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        case (CBF_PREDICTOR):
                                            cbf_failnez (cbf_set_value      (cbf,"predictor"))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                            break;
                                        default:
                                            cbf_failnez (cbf_set_value      (cbf,"."))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                            break;
                                    }
                                    if (compression&CBF_FLAG_MASK) {
                                        if (compression&CBF_UNCORRELATED_SECTIONS) {
                                            cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                            cbf_failnez (cbf_set_value        (cbf, "uncorrelated_sections"))
                                            cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                        } else if (compression&CBF_FLAT_IMAGE)  {
                                            cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                            cbf_failnez (cbf_set_value        (cbf, "flat"))
                                            cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                        }
                                    } else {
                                        if (!cbf_find_column(cbf, "compression_type_flag")) {
                                            cbf_failnez (cbf_set_value      (cbf,"."))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                        }
                                    }
                                } else  if (compression && value && column_name && !cbf_cistrcmp("compression_type_flag",column_name)) {
                                    if (compression&CBF_FLAG_MASK) {
                                        if (compression&CBF_UNCORRELATED_SECTIONS) {
                                            cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                            cbf_failnez (cbf_set_value        (cbf, "uncorrelated_sections"))
                                            cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                        } else if (compression&CBF_FLAT_IMAGE)  {
                                            cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                            cbf_failnez (cbf_set_value        (cbf, "flat"))
                                            cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                        }
                                    } else {
                                        if (!cbf_find_column(cbf, "compression_type_flag")) {
                                            cbf_failnez (cbf_set_value      (cbf,"."))
                                            cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                        }
                                    }
                                } else {
                                    cbf_failnez (cbf_get_typeofvalue(cif, &typeofvalue))
                                    cbf_failnez (cbf_select_column(cbf, colnum))
                                    cbf_failnez (cbf_set_value(cbf, value))
                                    if (typeofvalue) {
                                        cbf_failnez (cbf_set_typeofvalue(cbf, typeofvalue))
                                    }
                                }
                            } else {

                                int binary_id, elsigned, elunsigned;
                                size_t elements, elsize;
                                int minelement, maxelement;
                                unsigned int cifcompression;
                                int realarray;
                                const char *byteorder;
                                size_t fastdim, middim, slowdim, padding;

                                cbf_failnez(cbf_get_arrayparameters_wdims_fs(cif,
                                                                             &cifcompression,
                                                                             &binary_id,
                                                                             &elsize,
                                                                             &elsigned,
                                                                             &elunsigned,
                                                                             &elements,
                                                                             &minelement,
                                                                             &maxelement,
                                                                             &realarray,
                                                                             &byteorder,
                                                                             &fastdim,
                                                                             &middim,
                                                                             &slowdim,
                                                                             &padding));
                                if (fastdim < 1) fastdim = 1;
                                if (middim < 1) middim = 1;
                                if (slowdim < 1) slowdim = 1;
                                {   cbf_failnez (cbf_select_column(cbf,colnum))
                                    cbf_failnez (
                                                 cbf_copy_value_with_roi(cbf,
                                                                         cif,
                                                                         category_name,
                                                                         column_name,
                                                                         rownum,
                                                                         compression,
                                                                         dimflag,
                                                                         IorR,
                                                                         nelsize?((size_t)nelsize):elsize,
                                                                         realarray?CBF_CPY_SETSIGNED:0,
                                                                         cliplow,
                                                                         cliphigh,
                                                                         roi))

                                    }
                                }
                            }
                        }
                } else {
                    cbf_saveframe_name(cif,&saveframe_name);
                    cbf_force_new_saveframe(cbf, saveframe_name);

                    if ( !cbf_rewind_category(cif) ) {
                        cbf_failnez (cbf_count_categories(cif, &categories))

                        for (catnum = 0; catnum < categories;  catnum++) {
                            cbf_select_category(cif, catnum);
                            cbf_category_name(cif,&category_name);
                            cbf_force_new_category(cbf, category_name);
                            cbf_count_rows(cif,&rows);
                            cbf_count_columns(cif,&columns);

                            /*  Transfer the columns names from cif to cbf */
                            if ( ! cbf_rewind_column(cif) ) {
                                do {
                                    cbf_failnez(cbf_column_name(cif, &column_name))
                                    cbf_failnez(cbf_new_column(cbf, column_name))
                                } while ( ! cbf_next_column(cif) );
                                cbf_rewind_column(cif);
                                cbf_rewind_row(cif);
                            }
                            /* Transfer the rows from cif to cbf */
                            for (rownum = 0; rownum < rows; rownum++ ) {
                                cbf_failnez (cbf_select_row(cif, rownum))
                                cbf_failnez (cbf_new_row(cbf))
                                cbf_rewind_column(cif);
                                for (colnum = 0; colnum < columns; colnum++ ) {
                                    const char *typeofvalue;

                                    cbf_failnez (cbf_select_column(cif, colnum))

                                    if ( ! cbf_get_value(cif, &value) ) {
                                        cbf_failnez (cbf_get_typeofvalue(cif, &typeofvalue))
                                        cbf_failnez (cbf_select_column(cbf, colnum))
                                        cbf_failnez (cbf_set_value(cbf, value))
                                        cbf_failnez (cbf_set_typeofvalue(cbf, typeofvalue))
                                    } else {

                                        void * array;
                                        int binary_id, elsigned, elunsigned;
                                        size_t elements,elements_read, elsize;
                                        int minelement, maxelement;
                                        unsigned int cifcompression;
                                        int realarray;
                                        const char * byteorder;
                                        size_t fastdim, middim, slowdim, padding;

                                        cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                                     cif, &cifcompression,
                                                                                     &binary_id, &elsize, &elsigned, &elunsigned,
                                                                                     &elements, &minelement, &maxelement, &realarray,
                                                                                     &byteorder, &fastdim, &middim, &slowdim, &padding))
                                        if ((array=malloc(elsize*elements))) {
                                            cbf_failnez (cbf_select_column(cbf,colnum))
                                            if (!realarray) {
                                                cbf_failnez (cbf_get_integerarray(
                                                                                  cif, &binary_id, array, elsize, elsigned,
                                                                                  elements, &elements_read))
                                                cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                          cbf, compression,
                                                                                          binary_id, array, elsize, elsigned, elements,
                                                                                          byteorder, fastdim, middim, slowdim, padding))
                                            } else  {
                                                cbf_failnez (cbf_get_realarray(
                                                                               cif, &binary_id, array, elsize,
                                                                               elements, &elements_read))
                                                if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                    cbf_get_arraydimensions(cif,NULL,&fastdim,&middim,&slowdim);
                                                }
                                                cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                                       cbf, compression,
                                                                                       binary_id, array, elsize, elements,
                                                                                       byteorder, fastdim, middim, slowdim, padding))
                                            }
                                            free(array);
                                        } else {
                                            fprintf(stderr,
                                                    "\nFailed to allocate memory %ld bytes",
                                                    (long) elsize*elements);
                                            exit(1);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

        }

    }

    cbf = cbfsave;
    b = clock ();
    fprintf (stderr,
             " Time to read input_cif: %.3fs\n",
             ((b - a) * 1.0) / CLOCKS_PER_SEC);


    /* Read the update file, if any */
    if (updatecif) {

        if (!(update = fopen (updatecif, "rb"))) {
            fprintf (stderr,"Couldn't open the update CIF file %s\n", updatecif);
            exit (1);
        }

        cbf_failnez (cbf_read_widefile (ucif, update, MSG_DIGEST|qrflags|(digest&MSG_DIGESTWARN)))

        cbf_failnez (cbf_rewind_datablock(ucif))

        cbf_failnez (cbf_count_datablocks(ucif, &blocks))

        for (blocknum = 0; blocknum < blocks;  blocknum++ ) {
            /* start of merge loop */


            cbf_failnez (cbf_select_datablock(ucif, blocknum))
            cbf_failnez (cbf_datablock_name(ucif, &datablock_name))

            /* either this is a new datablock, in which case we copy it
             or it has the same name as an existing datablock, in which
             case we merge it */
            
            if (merge_datablocks_by_number) {
                const char * odatablock_name;
                if (cbf_select_datablock(cbf,blocknum)){
                    cbf_failnez (cbf_force_new_datablock(cbf, datablock_name))
                }
                cbf_failnez (cbf_datablock_name(cbf, &odatablock_name));
                if (cbf_cistrcmp(datablock_name,odatablock_name)) {
                    char ndatablock_name[76];
                    const char * db = datablock_name;
                    const char * odb = odatablock_name;
                    size_t dbsz = strlen(datablock_name);
                    size_t odbsz = strlen(odatablock_name);
                    size_t ndbsz;
                    size_t idb, kdb;
                    for (idb = 0; idb < dbsz && idb < odbsz && idb < 69; idb++) {
                        if (toupper(db[idb]) == toupper(odb[idb])) continue;
                        break;
                    }
                    strncpy(ndatablock_name,odatablock_name,69);
                    ndatablock_name[69] = '\0';
                    strcat(ndatablock_name,"_");
                    
                    dbsz = strlen(datablock_name+idb);
                    ndbsz = strlen(ndatablock_name)+dbsz;
                    if (ndbsz <= 75) {
                        strncpy(ndatablock_name+ndbsz-dbsz,db+idb,dbsz);
                        ndatablock_name[ndbsz] = '\0';
                    } else {
                        /* The new length is too long truncate, keeping all of odatablock
                           if it is less then 49 characters long, take the last
                           25 from datablock_name*/
                        if (ndbsz - dbsz < 50) {
                            strncpy(ndatablock_name+ndbsz-dbsz,db+idb-(dbsz-25),25);
                            ndatablock_name[ndbsz-dbsz+25]='\0';
                        } else {
                            ndatablock_name[49] = '_';
                            strncpy(ndatablock_name+50,db+idb-(dbsz-25),25);
                            ndatablock_name[75] = '\0';
                        }
                    }
                    /* Don't report errors in changing this name */
                    cbf_set_datablockname(cbf,ndatablock_name);
                }
            } else {
                cbf_failnez (cbf_require_datablock(cbf, datablock_name))
                if (altcbf) {
                    cbf_failnez (cbf_require_datablock(altcbf, datablock_name))
                }
            }

            if ( !cbf_rewind_blockitem(ucif, &itemtype) ) {
                cbf_failnez (cbf_count_blockitems(ucif, &blockitems))

                for (itemnum = 0; itemnum < blockitems;  itemnum++) {
                    cbf_select_blockitem(ucif, itemnum, &itemtype);
                    if (itemtype == CBF_CATEGORY) {
                        cbf_category_name(ucif,&category_name);
                        if (altcbf && !cbf_cistrcmp(category_name,"array_data")) {
                            cbf = altcbf;
                        } else {
                            cbf = cbfsave;
                        }
                        cbf_require_category(cbf, category_name);
                        cbf_count_rows(ucif,&rows);
                        cbf_count_columns(ucif,&columns);
                        if (altcbf && !cbf_cistrcmp(category_name,"array_data")) {
                            if (minicbf || add_minicbf_header) {
                                cbf_failnez(cbf_require_column(altcbf,"header_convention"))
                                cbf_failnez(cbf_require_column(altcbf,"header_contents"))
                            }
                            /*  Transfer the column names from cif to altcbf, header
                             first, data last */
                            if ( ! cbf_rewind_column(ucif) ) {
                                do {
                                    cbf_failnez(cbf_column_name(ucif, &column_name));
                                    if (cbf_cistrcmp(column_name,"data") ) {
                                        cbf_require_column(altcbf, column_name);
                                    }
                                } while ( ! cbf_next_column(ucif) );
                                cbf_require_column(altcbf,"data");
                                cbf_rewind_column(ucif);
                                cbf_rewind_row(ucif);
                            }
                        } else {
                        /*  Transfer the columns names from ucif to cbf */
                        if ( ! cbf_rewind_column(ucif) ) {
                            do {
                                cbf_failnez(cbf_column_name(ucif, &column_name))
                                cbf_failnez(cbf_require_column(cbf, column_name))
                            } while ( ! cbf_next_column(ucif) );
                            cbf_rewind_column(ucif);
                            cbf_rewind_row(ucif);
                            cbf_rewind_column(cbf);
                            cbf_rewind_row(cbf);
                        }
                        }
                        /* Transfer the rows from ucif to cbf */
                        for (rownum = 0; rownum < rows; rownum++ ) {
                            cbf_failnez (cbf_select_row(ucif, rownum))
                            if (cbf_select_row(cbf,rownum)){
                                cbf_failnez (cbf_new_row(cbf))
                            }
                            cbf_rewind_column(ucif);
                            cbf_rewind_column(cbf);
                            for (colnum = 0; colnum < columns; colnum++ ) {
                                const char *typeofvalue;

                                cbf_failnez (cbf_select_column(ucif, colnum));
                                cbf_failnez (cbf_column_name(ucif, &column_name));
                                cbf_failnez (cbf_find_column(cbf,column_name));

                                if ( ! cbf_get_value(ucif, &value) ) {
                                    if (compression && value && column_name && !cbf_cistrcmp("compression_type",column_name)) {
                                        switch (compression&CBF_COMPRESSION_MASK) {
                                            case (CBF_NONE):
                                                cbf_failnez (cbf_set_value      (cbf,"none"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_CANONICAL):
                                                cbf_failnez (cbf_set_value      (cbf,"canonical"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_PACKED):
                                                cbf_failnez (cbf_set_value      (cbf,"packed"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_PACKED_V2):
                                                cbf_failnez (cbf_set_value      (cbf,"packed_v2"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_BYTE_OFFSET):
                                                cbf_failnez (cbf_set_value      (cbf,"byte_offsets"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_NIBBLE_OFFSET):
                                                cbf_failnez (cbf_set_value      (cbf,"nibble_offset"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_PREDICTOR):
                                                cbf_failnez (cbf_set_value      (cbf,"predictor"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            default:
                                                cbf_failnez (cbf_set_value      (cbf,"."))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                                break;
                                        }
                                        if (compression&CBF_FLAG_MASK) {
                                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "uncorrelated_sections"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            } else if (compression&CBF_FLAT_IMAGE)  {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "flat"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            }
                                        } else {
                                            if (!cbf_find_column(cbf, "compression_type_flag")) {
                                                cbf_failnez (cbf_set_value      (cbf,"."))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                            }
                                        }
                                    } else  if (compression && value && column_name && !cbf_cistrcmp("compression_type_flag",column_name)) {
                                        if (compression&CBF_FLAG_MASK) {
                                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "uncorrelated_sections"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            } else if (compression&CBF_FLAT_IMAGE)  {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "flat"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            }
                                        } else {
                                            if (!cbf_find_column(cbf, "compression_type_flag")) {
                                                cbf_failnez (cbf_set_value      (cbf,"."))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                            }
                                        }
                                    } else {
                                        cbf_failnez (cbf_get_typeofvalue(ucif, &typeofvalue))
                                        cbf_failnez (cbf_find_column(cbf, column_name))
                                        cbf_failnez (cbf_set_value(cbf, value))
                                        cbf_failnez (cbf_set_typeofvalue(cbf, typeofvalue))
                                    }
                                } else {
                                    
                                    void * array;
                                    void * oarray;
                                    int binary_id, elsigned, elunsigned;
                                    size_t elements,elements_read, elsize;
                                    int minelement, maxelement;
                                    unsigned int cifcompression;
                                    int realarray;
                                    const char *byteorder;
                                    size_t fastdim, middim, slowdim, padding;
                                    
                                    int obinary_id, oelsigned, oelunsigned;
                                    size_t oelements,oelements_read, oelsize;
                                    int ominelement, omaxelement;
                                    unsigned int ocifcompression;
                                    int orealarray;
                                    const char *obyteorder;
                                    size_t ofastdim, omiddim, oslowdim, opadding;
                                    
                                    double doval, odoval;
                                    
                                    oarray = NULL;
                                    
                                    cbf_failnez(cbf_get_arrayparameters_wdims_fs(ucif,
                                                                                 &cifcompression,
                                                                                 &binary_id,
                                                                                 &elsize,
                                                                                 &elsigned,
                                                                                 &elunsigned,
                                                                                 &elements,
                                                                                 &minelement,
                                                                                 &maxelement,
                                                                                 &realarray,
                                                                                 &byteorder,
                                                                                 &fastdim,
                                                                                 &middim,
                                                                                 &slowdim,
                                                                                 &padding));
                                    
                                    
                                    if ((add_update_data || subtract_update_data)
                                        && !cbf_require_column(cbf, column_name)
                                        && cbf_get_value(cbf, &value)
                                        && !cbf_get_arrayparameters_wdims_fs(cbf,
                                                                            &ocifcompression,
                                                                            &obinary_id,
                                                                            &oelsize,
                                                                            &oelsigned,
                                                                            &oelunsigned,
                                                                            &oelements,
                                                                            &ominelement,
                                                                            &omaxelement,
                                                                            &orealarray,
                                                                            &obyteorder,
                                                                            &ofastdim,
                                                                            &omiddim,
                                                                            &oslowdim,
                                                                            &opadding)
                                        && binary_id == obinary_id
                                        && elsize == oelsize
                                        && elsigned == oelsigned
                                        && elunsigned == oelunsigned
                                        && realarray == orealarray
                                        && elements == oelements) {
                                        oarray = malloc(oelsize*oelements);
                                        if (!orealarray) {
                                            cbf_failnez (cbf_get_integerarray(cbf,
                                                                              &obinary_id,
                                                                              oarray,
                                                                              oelsize,
                                                                              oelsigned,
                                                                              oelements,
                                                                              &oelements_read));
                                        } else {
                                            cbf_failnez (cbf_get_realarray(cbf,
                                                                           &obinary_id,
                                                                           oarray,
                                                                           oelsize,
                                                                           oelements,
                                                                           &oelements_read));
                                        }
                                        
                                        
                                        
                                    }
                                    
                                    
                                    if ((array=malloc(elsize*elements))) {
                                        cbf_failnez (cbf_find_column(cbf,column_name))
                                        if (!realarray)  {
                                            cbf_failnez (cbf_get_integerarray(ucif,
                                                                              &binary_id,
                                                                              array,
                                                                              elsize,
                                                                              elsigned,
                                                                              elements,
                                                                              &elements_read))
                                            if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                cbf_get_arraydimensions(ucif,NULL,&fastdim,&middim,&slowdim);
                                            }
                                            if (oarray) {
                                                size_t elno;
                                                if (elsize == sizeof(char)) {
                                                    if (add_update_data) {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((char *)array)[elno];
                                                            odoval = ((char *)oarray)[elno];
                                                            if (((char *)array)[elno] != (char)(~0) && ((char *)oarray)[elno] != (char)(~0)) {
                                                                doval += odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((char *)array)[elno] = (char) doval;
                                                            }
                                                        }
                                                    } else {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((char *)array)[elno];
                                                            odoval = ((char *)oarray)[elno];
                                                            if (((char *)array)[elno] != (char)(~0) && ((char *)oarray)[elno] != (char)(~0)) {
                                                                doval -= odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((char *)array)[elno] = (char) doval;
                                                            }
                                                        }
                                                    }
                                                } else if (elsize == sizeof(short int)) {
                                                    if (add_update_data) {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((short int *)array)[elno];
                                                            odoval = ((short int *)oarray)[elno];
                                                            if ( ( (((short int *)array)[elno] < -7) ||(((short int *)array)[elno] > -1) )
                                                                &&  ( (((short int *)oarray)[elno] < -7)||(((short int *)oarray)[elno] > -1))) {
                                                                doval += odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((short int *)array)[elno] = (short int) doval;
                                                            }
                                                        }
                                                    } else {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((short int *)array)[elno];
                                                            odoval = ((short int *)oarray)[elno];
                                                            if ( ( (((short int *)array)[elno] < -7) ||(((short int *)array)[elno] > -1) )
                                                                &&  ( (((short int *)oarray)[elno] < -7)||(((short int *)oarray)[elno] > -1))) {
                                                                doval -= odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((short int *)array)[elno] = (short int) doval;
                                                            }
                                                        }
                                                    }
                                                } else if (elsize == sizeof(int)) {
                                                    if (add_update_data) {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((int *)array)[elno];
                                                            odoval = ((int *)oarray)[elno];
                                                            if ( ( (((int *)array)[elno] < -7) ||(((int *)array)[elno] > -1) )
                                                                &&  ( (((int *)oarray)[elno] < -7)||(((int *)oarray)[elno] > -1))) {
                                                                doval += odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((int *)array)[elno] = (int) doval;
                                                            }
                                                        }
                                                    } else {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((int *)array)[elno];
                                                            odoval = ((int *)oarray)[elno];
                                                            if ( ( (((int *)array)[elno] < -7) ||(((int *)array)[elno] > -1) )
                                                                &&  ( (((int *)oarray)[elno] < -7)||(((int *)oarray)[elno] > -1))) {
                                                                doval -= odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((int *)array)[elno] = (int) doval;
                                                            }
                                                        }
                                                    }
                                                    
                                                } else if (elsize == sizeof(long int)) {
                                                    if (add_update_data) {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((long int *)array)[elno];
                                                            odoval = ((long int *)oarray)[elno];
                                                            if ( ( (((long int *)array)[elno] < -7) ||(((long int *)array)[elno] > -1) )
                                                                &&  ( (((long int *)oarray)[elno] < -7)||(((long int *)oarray)[elno] > -1))) {
                                                                doval += odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((int *)array)[elno] = (long int) doval;
                                                            }
                                                        }
                                                    } else {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((long int *)array)[elno];
                                                            odoval = ((long int *)oarray)[elno];
                                                            if ( ( (((long int *)array)[elno] < -7) ||(((long int *)array)[elno] > -1) )
                                                                &&  ( (((long int *)oarray)[elno] < -7)||(((long int *)oarray)[elno] > -1))) {
                                                                doval -= odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((int *)array)[elno] = (long int) doval;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                      cbf, compression,
                                                                                      binary_id, array, elsize, elsigned, elements,
                                                                                      "little_endian", fastdim, middim, slowdim, 0))
                                        } else {
                                            cbf_failnez (cbf_get_realarray(ucif,
                                                                           &binary_id,
                                                                           array,
                                                                           elsize,
                                                                           elements,
                                                                           &elements_read))
                                            if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                cbf_get_arraydimensions(ucif,NULL,&fastdim,&middim,&slowdim);
                                            }
                                            if (oarray) {
                                                size_t elno;
                                                if (elsize == sizeof(float)) {
                                                    if (add_update_data) {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((float *)array)[elno];
                                                            odoval = ((float *)oarray)[elno];
                                                            if (((float *)array)[elno] != (float)(~0) && ((float *)oarray)[elno] != (float)(~0)) {
                                                                doval += odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((float *)array)[elno] = (float)doval;
                                                            }
                                                        }
                                                    } else {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((float *)array)[elno];
                                                            odoval = ((float *)oarray)[elno];
                                                            if (((float *)array)[elno] != (float)(~0) && ((char *)oarray)[elno] != (char)(~0)) {
                                                                doval -= odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((float *)array)[elno] = (float)doval;
                                                            }
                                                        }
                                                    }
                                                } else if (elsize == sizeof(double)) {
                                                    if (add_update_data) {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((double *)array)[elno];
                                                            odoval = ((double *)oarray)[elno];
                                                            if (((double *)array)[elno] != (double)(~0) && ((double *)oarray)[elno] != (double)(~0)) {
                                                                doval += odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((double *)array)[elno] = doval;
                                                            }
                                                        }
                                                    } else {
                                                        for (elno= 0; elno < elements_read &&elno < oelements_read; elno++) {
                                                            doval = ((double *)array)[elno];
                                                            odoval = ((double *)oarray)[elno];
                                                            if (((double *)array)[elno] != (double)(~0) && ((double *)oarray)[elno] != (double)(~0)) {
                                                                doval -= odoval;
                                                                if (cliplow < cliphigh) {
                                                                    if (doval < cliplow) doval = cliplow;
                                                                    if (doval > cliphigh) doval = cliphigh;
                                                                }
                                                                ((double *)array)[elno] = doval;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            cbf_failnez(cbf_set_realarray_wdims_fs(cbf,
                                                                                   compression,
                                                                                   binary_id,
                                                                                   array,
                                                                                   elsize,
                                                                                   elements,
                                                                                   "little_endian",
                                                                                   fastdim,
                                                                                   middim,
                                                                                   slowdim,
                                                                                   0))
                                        }
                                        free(array);
                                    } else {
                                        fprintf(stderr,
                                                "\nFailed to allocate memory %ld bytes",
                                                (long) elsize*elements);
                                        exit(1);
                                    }
                                }
                            }
                        }
                    } else {
                        cbf_saveframe_name(ucif,&saveframe_name);
                        if (!cbf_find_saveframe(cbf,saveframe_name) ) {
                            cbf_failnez(cbf_force_new_saveframe(cbf, saveframe_name))
                        }

                        if ( !cbf_rewind_category(ucif) ) {
                            cbf_failnez (cbf_count_categories(ucif, &categories))

                            for (catnum = 0; catnum < categories;  catnum++) {
                                cbf_select_category(ucif, catnum);
                                cbf_category_name(ucif,&category_name);
                                cbf_require_category(cbf, category_name);
                                cbf_count_rows(ucif,&rows);
                                cbf_count_columns(ucif,&columns);

                                /*  Transfer the columns names from ucif to cbf */
                                if ( ! cbf_rewind_column(ucif) ) {
                                    do {
                                        cbf_failnez(cbf_column_name(ucif, &column_name))
                                        cbf_failnez(cbf_require_column(cbf, column_name))
                                    } while ( ! cbf_next_column(ucif) );
                                    cbf_rewind_column(ucif);
                                    cbf_rewind_row(ucif);
                                }
                                /* Transfer the rows from ucif to cbf */
                                for (rownum = 0; rownum < rows; rownum++ ) {
                                    cbf_failnez (cbf_select_row(ucif, rownum))
                                    if (cbf_select_row(cbf, rownum)) {
                                        cbf_failnez (cbf_new_row(cbf))
                                    }
                                    cbf_rewind_column(ucif);
                                    for (colnum = 0; colnum < columns; colnum++ ) {
                                        const char *typeofvalue;

                                        cbf_failnez (cbf_select_column(ucif, colnum))
                                        cbf_failnez (cbf_column_name(ucif, &column_name))

                                        if ( ! cbf_get_value(ucif, &value) ) {
                                            cbf_failnez (cbf_get_typeofvalue(ucif, &typeofvalue))
                                            cbf_failnez (cbf_find_column(cbf, column_name))
                                            cbf_failnez (cbf_set_value(cbf, value))
                                            cbf_failnez (cbf_set_typeofvalue(cbf, typeofvalue))
                                        } else {

                                            void * array;
                                            int binary_id, elsigned, elunsigned;
                                            size_t elements,elements_read, elsize;
                                            int minelement, maxelement;
                                            unsigned int cifcompression;
                                            int realarray;
                                            const char * byteorder;
                                            size_t fastdim, middim, slowdim, padding;

                                            cbf_failnez(cbf_get_arrayparameters_wdims_fs(ucif,
                                                                                         &cifcompression,
                                                                                         &binary_id,
                                                                                         &elsize,
                                                                                         &elsigned,
                                                                                         &elunsigned,
                                                                                         &elements,
                                                                                         &minelement,
                                                                                         &maxelement,
                                                                                         &realarray,
                                                                                         &byteorder,
                                                                                         &fastdim,
                                                                                         &middim,
                                                                                         &slowdim,
                                                                                         &padding))
                                            if ((array=malloc(elsize*elements))) {
                                                cbf_failnez (cbf_find_column(cbf,column_name))
                                                if (!realarray) {
                                                    cbf_failnez (cbf_get_integerarray(ucif,
                                                                                      &binary_id,
                                                                                      array,
                                                                                      elsize,
                                                                                      elsigned,
                                                                                      elements,
                                                                                      &elements_read))
                                                    cbf_failnez(cbf_set_integerarray_wdims_fs(cbf,
                                                                                              compression,
                                                                                              binary_id,
                                                                                              array,
                                                                                              elsize,
                                                                                              elsigned,
                                                                                              elements,
                                                                                              byteorder,
                                                                                              fastdim,
                                                                                              middim,
                                                                                              slowdim,
                                                                                              padding))
                                                } else  {
                                                    cbf_failnez (cbf_get_realarray(ucif,
                                                                                   &binary_id,
                                                                                   array,
                                                                                   elsize,
                                                                                   elements,
                                                                                   &elements_read))
                                                    if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                        cbf_get_arraydimensions(ucif,
                                                                                NULL,
                                                                                &fastdim,
                                                                                &middim,
                                                                                &slowdim);
                                                    }
                                                    cbf_failnez(cbf_set_realarray_wdims_fs(cbf,
                                                                                           compression,
                                                                                           binary_id,
                                                                                           array,
                                                                                           elsize,
                                                                                           elements,
                                                                                           byteorder,
                                                                                           fastdim,
                                                                                           middim,
                                                                                           slowdim,
                                                                                           padding))
                                                }
                                                free(array);
                                            } else {
                                                fprintf(stderr,
                                                        "\nFailed to allocate memory %ld bytes",
                                                        (long) elsize*elements);
                                                exit(1);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

            }
        }

        if (ucif) {
            cbf_failnez (cbf_free_handle (ucif));
    }

    }
    
    cbf = cbfsave;
    
    if (add_minicbf_header) {
        
        if (minicbf || data_last) {
        
            cbf_failnez(cbf_set_minicbf_header(cbf, altcbf, NULL));
            
        } else {
            
            cbf_failnez(cbf_set_minicbf_header(cbf, cbf, NULL));

    }

    }
    
    if (data_last && !minicbf && altcbf) {
        
        ucif = altcbf;
        
        cbf_failnez (cbf_count_datablocks(ucif, &blocks));
        
        for (blocknum = 0; blocknum < blocks;  blocknum++ ) {
            /* start of merge loop */
            
            
            cbf_failnez (cbf_select_datablock(ucif, blocknum));
            cbf_failnez (cbf_datablock_name(ucif, &datablock_name));
            
            /* either this is a new datablock, in which case we copy it
             or it has the same name as an existing datablock, in which
             case we merge it */
            
            cbf_failnez (cbf_require_datablock(cbf, datablock_name))
            
            if ( !cbf_rewind_blockitem(ucif, &itemtype) ) {
                cbf_failnez (cbf_count_blockitems(ucif, &blockitems))
                
                for (itemnum = 0; itemnum < blockitems;  itemnum++) {
                    cbf_select_blockitem(ucif, itemnum, &itemtype);
                    if (itemtype == CBF_CATEGORY) {
                        cbf_category_name(ucif,&category_name);
                        cbf_require_category(cbf, category_name);
                        cbf_count_rows(ucif,&rows);
                        cbf_count_columns(ucif,&columns);
                        {
                            if (minicbf || add_minicbf_header) {
                                cbf_failnez(cbf_require_column(cbf,"header_convention"))
                                cbf_failnez(cbf_require_column(cbf,"header_contents"))
                            }
                            /*  Transfer the column names from cif to altcbf, header
                             first, data last */
                            if ( ! cbf_rewind_column(ucif) ) {
                                do {
                                    cbf_failnez(cbf_column_name(ucif, &column_name));
                                    if (cbf_cistrcmp(column_name,"data") ) {
                                        cbf_require_column(cbf, column_name);
                                    }
                                } while ( ! cbf_next_column(ucif) );
                                cbf_require_column(cbf,"data");
                                cbf_rewind_column(ucif);
                                cbf_rewind_row(ucif);
                            }
                        }
                        /* Transfer the rows from ucif to cbf */
                        for (rownum = 0; rownum < rows; rownum++ ) {
                            cbf_failnez (cbf_select_row(ucif, rownum))
                            if (cbf_select_row(cbf,rownum)){
                                cbf_failnez (cbf_new_row(cbf))
                            }
                            cbf_rewind_column(ucif);
                            for (colnum = 0; colnum < columns; colnum++ ) {
                                const char *typeofvalue;
                                
                                cbf_failnez (cbf_select_column(ucif, colnum))
                                cbf_failnez (cbf_column_name(ucif, &column_name))
                                
                                if ( ! cbf_get_value(ucif, &value) ) {
                                    if (compression && value && column_name && !cbf_cistrcmp("compression_type",column_name)) {
                                        cbf_failnez (cbf_find_column(cbf, column_name))
                                        switch (compression&CBF_COMPRESSION_MASK) {
                                            case (CBF_NONE):
                                                cbf_failnez (cbf_set_value      (cbf,"none"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_CANONICAL):
                                                cbf_failnez (cbf_set_value      (cbf,"canonical"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_PACKED):
                                                cbf_failnez (cbf_set_value      (cbf,"packed"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_PACKED_V2):
                                                cbf_failnez (cbf_set_value      (cbf,"packed_v2"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_BYTE_OFFSET):
                                                cbf_failnez (cbf_set_value      (cbf,"byte_offsets"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_NIBBLE_OFFSET):
                                                cbf_failnez (cbf_set_value      (cbf,"nibble_offset"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            case (CBF_PREDICTOR):
                                                cbf_failnez (cbf_set_value      (cbf,"predictor"))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"word"))
                                                break;
                                            default:
                                                cbf_failnez (cbf_set_value      (cbf,"."))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                                break;
                                        }
                                        if (compression&CBF_FLAG_MASK) {
                                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "uncorrelated_sections"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            } else if (compression&CBF_FLAT_IMAGE)  {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "flat"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            }
                                        } else {
                                            if (!cbf_find_column(cbf, "compression_type_flag")) {
                                                cbf_failnez (cbf_set_value      (cbf,"."))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                            }
                                        }
                                    } else  if (compression && value && column_name && !cbf_cistrcmp("compression_type_flag",column_name)) {
                                        if (compression&CBF_FLAG_MASK) {
                                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "uncorrelated_sections"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            } else if (compression&CBF_FLAT_IMAGE)  {
                                                cbf_failnez (cbf_require_column   (cbf, "compression_type_flag"))
                                                cbf_failnez (cbf_set_value        (cbf, "flat"))
                                                cbf_failnez (cbf_set_typeofvalue  (cbf, "word"))
                                            }
                                        } else {
                                            if (!cbf_find_column(cbf, "compression_type_flag")) {
                                                cbf_failnez (cbf_set_value      (cbf,"."))
                                                cbf_failnez (cbf_set_typeofvalue(cbf,"null"))
                                            }
                                        }
                                    } else {
                                        cbf_failnez (cbf_get_typeofvalue(ucif, &typeofvalue))
                                        cbf_failnez (cbf_find_column(cbf, column_name))
                                        cbf_failnez (cbf_set_value(cbf, value))
                                        cbf_failnez (cbf_set_typeofvalue(cbf, typeofvalue))
                                    }
                                } else {
                                    
                                    void * array;
                                    int binary_id, elsigned, elunsigned;
                                    size_t elements,elements_read, elsize;
                                    int minelement, maxelement;
                                    unsigned int cifcompression;
                                    int realarray;
                                    const char *byteorder;
                                    size_t fastdim, middim, slowdim, padding;
                                    
                                    cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                                 ucif, &cifcompression,
                                                                                 &binary_id, &elsize, &elsigned, &elunsigned,
                                                                                 &elements, &minelement, &maxelement, &realarray,
                                                                                 &byteorder, &fastdim, &middim, &slowdim, &padding))
                                    if ((array=malloc(elsize*elements))) {
                                        cbf_failnez (cbf_find_column(cbf,column_name))
                                        if (!realarray)  {
                                            cbf_failnez (cbf_get_integerarray(
                                                                              ucif, &binary_id, array, elsize, elsigned,
                                                                              elements, &elements_read))
                                            if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                cbf_get_arraydimensions(ucif,NULL,&fastdim,&middim,&slowdim);
                                            }
                                            cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                      cbf, compression,
                                                                                      binary_id, array, elsize, elsigned, elements,
                                                                                      "little_endian", fastdim, middim, slowdim, 0))
                                        } else {
                                            cbf_failnez (cbf_get_realarray(
                                                                           ucif, &binary_id, array, elsize,
                                                                           elements, &elements_read))
                                            if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                cbf_get_arraydimensions(ucif,NULL,&fastdim,&middim,&slowdim);
                                            }
                                            cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                                   cbf, compression,
                                                                                   binary_id, array, elsize, elements,
                                                                                   "little_endian", fastdim, middim, slowdim, 0))
                                        }
                                        free(array);
                                    } else {
                                        fprintf(stderr,
                                                "\nFailed to allocate memory %ld bytes",
                                                (long) elsize*elements);
                                        exit(1);
                                    }
                                }
                            }
                        }
                    } else {
                        cbf_saveframe_name(ucif,&saveframe_name);
                        if (!cbf_find_saveframe(cbf,saveframe_name) ) {
                            cbf_failnez(cbf_force_new_saveframe(cbf, saveframe_name))
                        }
                        
                        if ( !cbf_rewind_category(ucif) ) {
                            cbf_failnez (cbf_count_categories(ucif, &categories))
                            
                            for (catnum = 0; catnum < categories;  catnum++) {
                                cbf_select_category(ucif, catnum);
                                cbf_category_name(ucif,&category_name);
                                cbf_require_category(cbf, category_name);
                                cbf_count_rows(ucif,&rows);
                                cbf_count_columns(ucif,&columns);
                                
                                /*  Transfer the columns names from ucif to cbf */
                                if ( ! cbf_rewind_column(ucif) ) {
                                    do {
                                        cbf_failnez(cbf_column_name(ucif, &column_name))
                                        cbf_failnez(cbf_require_column(cbf, column_name))
                                    } while ( ! cbf_next_column(ucif) );
                                    cbf_rewind_column(ucif);
                                    cbf_rewind_row(ucif);
                                }
                                /* Transfer the rows from ucif to cbf */
                                for (rownum = 0; rownum < rows; rownum++ ) {
                                    cbf_failnez (cbf_select_row(ucif, rownum))
                                    if (cbf_select_row(cbf, rownum)) {
                                        cbf_failnez (cbf_new_row(cbf))
                                    }
                                    cbf_rewind_column(ucif);
                                    for (colnum = 0; colnum < columns; colnum++ ) {
                                        const char *typeofvalue;
                                        
                                        cbf_failnez (cbf_select_column(ucif, colnum))
                                        cbf_failnez (cbf_column_name(ucif, &column_name))
                                        
                                        if ( ! cbf_get_value(ucif, &value) ) {
                                            cbf_failnez (cbf_get_typeofvalue(ucif, &typeofvalue))
                                            cbf_failnez (cbf_find_column(cbf, column_name))
                                            cbf_failnez (cbf_set_value(cbf, value))
                                            cbf_failnez (cbf_set_typeofvalue(cbf, typeofvalue))
                                        } else {
                                            
                                            void * array;
                                            int binary_id, elsigned, elunsigned;
                                            size_t elements,elements_read, elsize;
                                            int minelement, maxelement;
                                            unsigned int cifcompression;
                                            int realarray;
                                            const char * byteorder;
                                            size_t fastdim, middim, slowdim, padding;
                                            
                                            cbf_failnez(cbf_get_arrayparameters_wdims_fs(ucif,
                                                                                         &cifcompression,
                                                                                         &binary_id,
                                                                                         &elsize,
                                                                                         &elsigned,
                                                                                         &elunsigned,
                                                                                         &elements,
                                                                                         &minelement,
                                                                                         &maxelement,
                                                                                         &realarray,
                                                                                         &byteorder,
                                                                                         &fastdim,
                                                                                         &middim,
                                                                                         &slowdim,
                                                                                         &padding))
                                            if ((array=malloc(elsize*elements))) {
                                                cbf_failnez (cbf_find_column(cbf,column_name))
                                                if (!realarray) {
                                                    cbf_failnez (cbf_get_integerarray(ucif,
                                                                                      &binary_id,
                                                                                      array,
                                                                                      elsize,
                                                                                      elsigned,
                                                                                      elements,
                                                                                      &elements_read))
                                                    cbf_failnez(cbf_set_integerarray_wdims_fs(cbf,
                                                                                              compression,
                                                                                              binary_id,
                                                                                              array,
                                                                                              elsize,
                                                                                              elsigned,
                                                                                              elements,
                                                                                              byteorder,
                                                                                              fastdim,
                                                                                              middim,
                                                                                              slowdim,
                                                                                              padding))
                                                } else  {
                                                    cbf_failnez (cbf_get_realarray(ucif,
                                                                                   &binary_id,
                                                                                   array,
                                                                                   elsize,
                                                                                   elements,
                                                                                   &elements_read))
                                                    if (dimflag == HDR_FINDDIMS && fastdim==0) {
                                                        cbf_get_arraydimensions(ucif,
                                                                                NULL,
                                                                                &fastdim,
                                                                                &middim,
                                                                                &slowdim);
                                                    }
                                                    cbf_failnez(cbf_set_realarray_wdims_fs(cbf,
                                                                                           compression,
                                                                                           binary_id,
                                                                                           array,
                                                                                           elsize,
                                                                                           elements,
                                                                                           byteorder,
                                                                                           fastdim,
                                                                                           middim,
                                                                                           slowdim,
                                                                                           padding))
                                                }
                                                free(array);
                                            } else {
                                                fprintf(stderr,
                                                        "\nFailed to allocate memory %ld bytes",
                                                        (long) elsize*elements);
                                                exit(1);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                
            }
        }
        
    }
    
    a = clock ();

    if (hdf5mode&HDF5_WRITE_MODE) {

        if(cbf_create_h5handle(&h5out,cbfout)) {
            printf (" Couldn't open the HDF5 file %s\n", cbfout);
        }

    } else {

        out = NULL;
        if ( ! cbfout || strcmp(cbfout?cbfout:"","-") == 0 ) {
            out = stdout;
        } else if ( strcmp(cbfout?cbfout:"","/dev/null") ==0 ){
            devnull=1;
        } else {
            out = fopen (cbfout, "w+b");
        }
        if ( ( devnull == 0 ) &&  ! out ) {
            if (encoding == ENC_NONE) {
                printf (" Couldn't open the CBF file %s\n", cbfout);
            } else {
                printf (" Couldn't open the CIF file %s\n", cbfout);
            }
            exit (1);
        }

    }

    if (testconstruct) {
        void * array;
        const char * array_id;
        const char * array_section_id;
        unsigned int compression;
        int          id;
        size_t       elsize;
        int          elsigned;
        int          elunsigned;
        size_t       nelem;
        int          minelem;
        int          maxelem;
        int          realarray;
        size_t dimslow, dimmid, dimfast;
        cbf_detector detector;
        cbf_failnez(cbf_construct_detector (cbf, &detector, elno))
        cbf_failnez(cbf_get_3d_image_size(cbf,0,elno,&dimslow,&dimmid,&dimfast));
        fprintf(stderr,"dimslow = %ld, dimmid = %ld, dimfast = %ld\n",(long)dimslow,(long)dimmid,(long)dimfast);
        cbf_failnez(cbf_get_array_id(cbf,elno,&array_id));
        cbf_failnez(cbf_get_array_section_id(cbf,elno,&array_section_id));
        cbf_failnez(cbf_get_array_arrayparameters (cbf,
                                                   array_id,
                                                   0,
                                                   &compression,
                                                   &id,
                                                   &elsize,
                                                   &elsigned,
                                                   &elunsigned,
                                                   &nelem,
                                                   &minelem,
                                                   &maxelem,
                                                   &realarray));
        cbf_failnez(cbf_alloc(&array,NULL,1,dimslow*dimmid*dimfast*elsize));
        cbf_failnez(cbf_get_image(cbf,0,elno,array,elsize,elsigned,dimmid,dimfast));
        
        cbf_free_detector (detector);
        
    }


    if (hdf5noH5) {

        if (!cbf_find_datablock(cbf,"H5")) {

            cbf_failnez(cbf_remove_datablock(cbf));

        }
    }


    if ( ! devnull ){
        if (hdf5mode&HDF5_WRITE_MODE) {

            cbf_failnez(cbf_write_h5file (cbf, h5out, hdf5register|h5compression|(
                                          opaquemode?CBF_H5_OPAQUE:0)))

        } else {

            if (minicbf) {
                cbf = altcbf;
            } else {
                cbf = cbfsave;
            }

            if (Wide) {
                cbf_failnez (cbf_write_widefile (cbf, out, 1, cbforcif,
                                                 mime | (digest&(MSG_DIGEST|MSG_DIGESTNOW)) | padflag | qwflags,
                                                 encoding | bytedir | term ))
            } else {
                cbf_failnez (cbf_write_file (cbf, out, 1, cbforcif,
                                             mime | (digest&(MSG_DIGEST|MSG_DIGESTNOW)) | padflag | qwflags,
                                             encoding | bytedir | term ))
            }
        }

    }
    
    cbf = cbfsave;
    
	if (cbf) { 
        cbf_failnez (cbf_free_handle (cbf));
    }
    
	if (dic) {
        cbf_failnez (cbf_free_handle (dic));
    }
    
	if (cif) {
        cbf_failnez (cbf_free_handle (cif));
    }
    
    if (altcbf) {
        cbf_failnez (cbf_free_handle (altcbf));
    }

    b = clock ();
    if (encoding == ENC_NONE) {
        fprintf (stderr, " Time to write the CBF image: %.3fs\n",
                 ((b - a) * 1.0) / CLOCKS_PER_SEC);
    } else {
        if ( ! devnull )
            fprintf (stderr, " Time to write the CIF image: %.3fs\n",
                     ((b - a) * 1.0) / CLOCKS_PER_SEC);
    }

    cbf_failnez (cbf_free_getopt_handle(opts))

    exit(0);

}

int local_exit (int status)
{
    exit(status);
    return 1; /* avoid warnings */
}
