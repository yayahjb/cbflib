/**********************************************************************
 *          cif2cbf -- convert a cif to a cbf file                    *
 *                                                                    *
 * Version 0.9  04 August 2009                                        *
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
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
 *                                                                    *                                                                    *
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
 *        {v[2packed]}|{f[latpacked]}[n[one]}] \                      *
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
 *  -c compression_scheme (packed, canonical, byte_offset,            *
 *    v2packed, flatpacked or none,                                   *
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
 *    number of bytes,  4 or 8 for float or double output reals       *                                                 * 
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
 *            http://ndbserver.rutgers.edu/mmcif/cbf                  *
 *                                                                    *
 *  for background and references.  The CBF definition is available   *
 *  on the web page created by Andy Hammersley at                     *
 *                                                                    *
 *     http://www.ersf.fr/computing/Forum/imgCIF/cbf_definition.html  *
 *                                                                    *
 *  This program is a CBFlib application.  See "CBFLIB, An ANSI-C     *
 *  API for Crystallographic Binary Files", Version 0.1, April 1998   *
 *  by Paul J. Ellis, Stanford Synchrotron Radiation Laboratory,      *
 *  ellis@ssrl.slac.stanford.edu                                      *
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
    return 0;
    
}

#undef cbf_failnez
#define cbf_failnez(x) \
{int err; \
err = (x); \
if (err) { \
fprintf(stderr,"CBFlib fatal error %d\n",err); \
outerror(err); \
local_exit (-1); \
} \
}

void set_MP_terms(int crterm, int nlterm);

int main (int argc, char *argv [])
{
    FILE *in, *out=NULL, *update=NULL, *file, *dict;
    clock_t a,b;
    cbf_handle cif;
    cbf_handle ucif;
    cbf_handle cbf;
    cbf_handle dic;
    cbf_handle odic;
    cbf_getopt_handle opts;
    int devnull = 0;
    int c;
    int errflg = 0;
    const char *cifin, *cbfout, *updatecif;
    const char *dictionary[NUMDICTS];
    int dqrflags[NUMDICTS];
    char *ciftmp=NULL;
#ifndef NOMKSTEMP
    int ciftmpfd;
#endif
    int ciftmpused;
    int padflag;
    int dimflag;
    int nbytes;
    int ndict = 0;
    int kd;
    int wide = 0;
    int Wide = 0;
    int IorR = 0;
    int nelsize;
    int testconstruct;
    char buf[C2CBUFSIZ];
    unsigned int blocks, categories, blocknum, catnum, blockitems, itemnum;
    CBF_NODETYPE itemtype;
    const char *datablock_name;
    const char *saveframe_name;
    const char *category_name;
    const char *column_name;
    const char *value;
    unsigned int colnum, rownum;
    unsigned int columns;
    unsigned int rows;
    double cliphigh, cliplow;

    int mime, digest, encoding, compression, bytedir, cbforcif, term;
    int qrflags, qwflags;
    
    const char * optarg;
    
    
    /* Extract options */
    
    /**********************************************************************
     *  cif2cbf [-i input_cif] [-o output_cbf] \                          *
     *    [-u update_cif] \                                               *
     *    [-b {b[ackwards]|f[orwards]}] \                                 *
     *    [-B {read|liberal|noread}] [-B {write|nowrite}] \               *
     *    [-c {p[acked]|c[annonical]|{b[yte_offset]}|\                    *
     *        {v[2packed]}|{f[latpacked]}[n[one]}] \                      *
     *    [-C highclipvalue] \                                            *
     *    [-d {d[igest]|n[odigest]|w[arndigest]} \                        *
     *    [-D ] \                                                         *
     *    [-e {b[ase64]|k|q[uoted-printable]| \                           *
     *                  d[ecimal]|h[exadecimal]|o[ctal]|n[one]}] \        *
     *    [-I {0|2|4|8}]  \                                               *
     *    [-L lowclipvalue ] \                                            *
     *    [-m {h[eaders]|noh[eaders]}] \                                  *
     *    [-m {d[imensions]|nod[imensions}] \                             *
     *    [-R {0|4|8}] \                                                  *
     *    [-S {read|noread}] [-S {write|nowrite}] \                       *
     *    [-T {read|noread}] [-T {write|nowrite}] \                       *
     *    [-p {0|1|2|4}] \                                                *
     *    [-v dictionary]* [-w] [-W] \                                    *
     *    [input_cif] [output_cbf]                                        *
     *                                                                    *
     **********************************************************************/
    
    mime = 0;
    digest = 0;
    encoding = 0;
    compression = 0;
    bytedir = 0;
    ndict = 0;
    padflag = 0;
    qrflags = qwflags = 0;
    dimflag = 0;
    nelsize = 0;
    
    cifin = NULL;
    cbfout = NULL;
    updatecif = NULL;
    ciftmpused = 0;
    testconstruct = 0;
    cliphigh = cliplow = 0.;
    
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
                                 "w(read-wide)" \
                                 "W(write-wide)" \
                                 ))
    
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
                    else cbfout = optarg;
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
                        compression = CBF_PACKED;
                    } else {
                        if (optarg[0] == 'c' || optarg[0] == 'C') {
                            compression = CBF_CANONICAL;
                        } else {
                            if (optarg[0] == 'b' || optarg[0] == 'B') {
                                compression = CBF_BYTE_OFFSET;
                            } else {
                                if (optarg[0] == 'n' || optarg[0] == 'N') {
                                    compression = CBF_NONE;
                                } else {
                                    if (optarg[0] == 'v' || optarg[0] == 'V') {
                                        compression = CBF_PACKED_V2;
                                    } else {
                                        if (optarg[0] == 'f' || optarg[0] == 'F') {
                                            compression = CBF_PACKED|CBF_FLAT_IMAGE;
                                        } else {             
                                            errflg++;
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
            } else {
                errflg++;
            }
        }
    }
    if (errflg) {
        fprintf(stderr,"cif2cbf:  Usage: \n");
        fprintf(stderr,
                "  cif2cbf [-i input_cif] [-o output_cbf] \\\n");
        fprintf(stderr,
                "    [-u update_cif] \\\n");
        fprintf(stderr,
                "    [-c {p[acked]|c[annonical]|{b[yte_offset]}|\\\n");
        fprintf(stderr,
                "        {v[2packed}|{f[latpacked}[n[one]}] \\\n");
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
    if (!(in = fopen (cifin, "rb"))) {
        fprintf (stderr,"Couldn't open the input CIF file %s\n", cifin);
        exit (1);
    }
    
    if (ciftmpused) {
        if (unlink(ciftmp) != 0 ) {
            fprintf(stderr,"cif2cif:  Can't unlink temporary file %s.\n", ciftmp);
            fprintf(stderr,"%s\n",strerror(errno));
            exit(1);
        }
    }
    
    if (!wide) {
        cbf_failnez (cbf_read_file (cif, in, MSG_DIGEST|qrflags|(digest&MSG_DIGESTWARN)))
    } else {
        cbf_failnez (cbf_read_widefile (cif, in, MSG_DIGEST|qrflags|(digest&MSG_DIGESTWARN)))
    }
    
    cbf_failnez (cbf_rewind_datablock(cif))
    
    cbf_failnez (cbf_count_datablocks(cif, &blocks))
    
    for (blocknum = 0; blocknum < blocks;  blocknum++ )
    { /* start of copy loop */
        
        
        cbf_failnez (cbf_select_datablock(cif, blocknum))
        cbf_failnez (cbf_datablock_name(cif, &datablock_name))
        cbf_failnez (cbf_force_new_datablock(cbf, datablock_name))
        
        if ( !cbf_rewind_blockitem(cif, &itemtype) ) {
            cbf_failnez (cbf_count_blockitems(cif, &blockitems))
            
            for (itemnum = 0; itemnum < blockitems;  itemnum++) {
                cbf_select_blockitem(cif, itemnum, &itemtype);
                if (itemtype == CBF_CATEGORY) {
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
                                size_t dim1, dim2, dim3, padding;
                                
                                cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                             cif, &cifcompression,
                                                                             &binary_id, &elsize, &elsigned, &elunsigned,
                                                                             &elements, &minelement, &maxelement, &realarray,
                                                                             &byteorder, &dim1, &dim2, &dim3, &padding))
                                if ((array=malloc(elsize*elements))) {
                                    cbf_failnez (cbf_select_column(cbf,colnum))
                                    if (!realarray)  {
                                        cbf_failnez (cbf_get_integerarray(
                                                                          cif, &binary_id, array, elsize, elsigned,
                                                                          elements, &elements_read))
                                        if (dimflag == HDR_FINDDIMS && dim1==0) {
                                            cbf_get_arraydimensions(cif,NULL,&dim1,&dim2,&dim3);
                                        }
                                        if (IorR == 0 || (IorR == CBF_CPY_SETINTEGER && (nelsize==elsize||nelsize==0))) {
                                            cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                  cbf, compression,
                                                                                  binary_id, array, elsize, elsigned, elements,
                                                                                  "little_endian", dim1, dim2, dim3, 0))
                                        } else {
                                            cbf_failnez(cbf_copy_value(cbf,cif,category_name,column_name,rownum,compression,dimflag,IorR,
                                                           nelsize?nelsize:elsize,0,cliplow,cliphigh))
                                        }
                                    } else {
                                        cbf_failnez (cbf_get_realarray(
                                                                       cif, &binary_id, array, elsize,
                                                                       elements, &elements_read))
                                        if (dimflag == HDR_FINDDIMS && dim1==0) {
                                            cbf_get_arraydimensions(cif,NULL,&dim1,&dim2,&dim3);
                                        }
                                        if (IorR == 0 || (IorR == CBF_CPY_SETREAL && (nelsize==elsize||nelsize==0))) {
                                        cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                               cbf, compression,
                                                                               binary_id, array, elsize, elements,
                                                                               "little_endian", dim1, dim2, dim3, 0))  
                                        } else {
                                            cbf_failnez(cbf_copy_value(cbf,cif,category_name,column_name,rownum,compression,dimflag,IorR,
                                                           nelsize?nelsize:elsize,CBF_CPY_SETSIGNED,cliplow,cliphigh))
                                        }
                                        
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
                                        size_t dim1, dim2, dim3, padding;
                                        
                                        cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                                     cif, &cifcompression,
                                                                                     &binary_id, &elsize, &elsigned, &elunsigned,
                                                                                     &elements, &minelement, &maxelement, &realarray,
                                                                                     &byteorder, &dim1, &dim2, &dim3, &padding))
                                        if ((array=malloc(elsize*elements))) {
                                            cbf_failnez (cbf_select_column(cbf,colnum))
                                            if (!realarray) {
                                                cbf_failnez (cbf_get_integerarray(
                                                                                  cif, &binary_id, array, elsize, elsigned,
                                                                                  elements, &elements_read))
                                                cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                          cbf, compression,
                                                                                          binary_id, array, elsize, elsigned, elements,
                                                                                          byteorder, dim1, dim2, dim3, padding))
                                            } else  {
                                                cbf_failnez (cbf_get_realarray(
                                                                               cif, &binary_id, array, elsize,
                                                                               elements, &elements_read))
                                                if (dimflag == HDR_FINDDIMS && dim1==0) {
                                                    cbf_get_arraydimensions(cif,NULL,&dim1,&dim2,&dim3);
                                                }
                                                cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                                       cbf, compression,
                                                                                       binary_id, array, elsize, elements,
                                                                                       byteorder, dim1, dim2, dim3, padding))
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
                                    size_t dim1, dim2, dim3, padding;
                                    
                                    cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                                 ucif, &cifcompression,
                                                                                 &binary_id, &elsize, &elsigned, &elunsigned,
                                                                                 &elements, &minelement, &maxelement, &realarray,
                                                                                 &byteorder, &dim1, &dim2, &dim3, &padding))
                                    if ((array=malloc(elsize*elements))) {
                                        cbf_failnez (cbf_find_column(cbf,column_name))
                                        if (!realarray)  {
                                            cbf_failnez (cbf_get_integerarray(
                                                                              ucif, &binary_id, array, elsize, elsigned,
                                                                              elements, &elements_read))
                                            if (dimflag == HDR_FINDDIMS && dim1==0) {
                                                cbf_get_arraydimensions(ucif,NULL,&dim1,&dim2,&dim3);
                                            }
                                            cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                      cbf, compression,
                                                                                      binary_id, array, elsize, elsigned, elements,
                                                                                      "little_endian", dim1, dim2, dim3, 0))
                                        } else {
                                            cbf_failnez (cbf_get_realarray(
                                                                           ucif, &binary_id, array, elsize,
                                                                           elements, &elements_read))
                                            if (dimflag == HDR_FINDDIMS && dim1==0) {
                                                cbf_get_arraydimensions(ucif,NULL,&dim1,&dim2,&dim3);
                                            }
                                            cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                                   cbf, compression,
                                                                                   binary_id, array, elsize, elements,
                                                                                   "little_endian", dim1, dim2, dim3, 0))                 	
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
                                            size_t dim1, dim2, dim3, padding;
                                            
                                            cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                                         ucif, &cifcompression,
                                                                                         &binary_id, &elsize, &elsigned, &elunsigned,
                                                                                         &elements, &minelement, &maxelement, &realarray,
                                                                                         &byteorder, &dim1, &dim2, &dim3, &padding))
                                            if ((array=malloc(elsize*elements))) {
                                                cbf_failnez (cbf_find_column(cbf,column_name))
                                                if (!realarray) {
                                                    cbf_failnez (cbf_get_integerarray(
                                                                                      ucif, &binary_id, array, elsize, elsigned,
                                                                                      elements, &elements_read))
                                                    cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                                              cbf, compression,
                                                                                              binary_id, array, elsize, elsigned, elements,
                                                                                              byteorder, dim1, dim2, dim3, padding))
                                                } else  {
                                                    cbf_failnez (cbf_get_realarray(
                                                                                   ucif, &binary_id, array, elsize,
                                                                                   elements, &elements_read))
                                                    if (dimflag == HDR_FINDDIMS && dim1==0) {
                                                        cbf_get_arraydimensions(ucif,NULL,&dim1,&dim2,&dim3);
                                                    }
                                                    cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                                           cbf, compression,
                                                                                           binary_id, array, elsize, elements,
                                                                                           byteorder, dim1, dim2, dim3, padding))
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
    
    if (testconstruct) {
        cbf_detector detector;
        cbf_failnez(cbf_construct_detector (cbf, &detector, 0))
        cbf_free_detector (detector);
    }
    
    if ( ! devnull ){
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
    cbf_failnez (cbf_free_handle (cbf))
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
