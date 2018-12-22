/**********************************************************************
 * cbf_nibble_offset -- nibble-offset compression                     *
 *                                                                    *
 * Version 0.9.8 6 December 2012                                      *
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

#ifdef __cplusplus

extern "C" {

#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "cbf.h"
#include "cbf_file.h"
#include "cbf_nibble_offset.h"
    
/*  The nibble offset algorithm is a variant on 
    A. P. Hammersley's byte offset algorithm.  The
    major differences are that the compression modes
    are "sticky", the compression can be reset at
    any point to allow for block parallelism, and
    the basic unit of compression is the nibble,
    but for very clean data, the dibit is also supported.
 
    This versions is set up to fully support parallel
    operations by also allowing the specification of the start address
    in the image for each block after a reset.  Therefore
    the blocks do not have to be in order.
 
    The data stream starts with and in general uses
    a mode-setting octet presented in one if three
    forms, a single dibit a0, two dibits a0, a1, or
    two dibits and a nibble a0, a1, b:
 
    a0 a1 b    octet     meaning
    00 00 0000 0x00 --   reset to zero
    01         0x01 --   up 1 mode
    10         0x02 --   dibit mode
    11         0x03 --   up n modes
    00 01      0x04 --   nibble mode
    00 11      0x0C --   6-bit mode
    00 10      0x08 --   byte mode
    00 00 0011 0x30 --   12-bit word mode
    00 00 0001 0x10 --   16-bit word mode
    00 00 0010 0x20 --   32-bit word mode
    00 00 0100 0x40 --   64-bit word mode
    00 00 1100 0xC0 --   specify starting address
 
    The reset to zero is followed by a new mode octet
    A reset to zero resets the prior value for delta to zero
 
    The up n modes code is followed immediately
    by a dibit specifying 2 less than the number of modes 
    by which to change, and then by a delta in the mode.
 
    Note that up n modes has no effect until
    an actual mode has been set and can be used immediately
    after a reset to pad to nibble, octet or double-word
    boundaries.
 
    Once a mode is established, it is followed by a stream 
    of deltas of that size (for modes 2 or 4-64) or by one delta
    of that size and then a stream of deltas of the size that
    was in effect before an up or down giving little-endian 
    offsets from the currently accumulated value.  If the 
    offset is one of the following in the indicated mode
 
       dibit mode:       0x2
       nibble mode:      0x8
       6-bit mode:       0x20
       byte mode:        0x80
       12-bit word mode  0x800
       16-bit word mode  0x8000
       32-bit word mode  0x8000 0000
       64-bit word mode  0x8000 0000 0000 0000
 
     it is followed by the new mode as 1 or 2 dibits or
     2 dibits and a nibble a1 a1 b.  If a1 is 1 or 2 or 3,
     that is the new mode.  If a1 is zero and a2 is 1 or 2
     the new mode is a2*4.  If a2 is 3 the new mode is
     a2*2.  If both a1 and a2 are zero, the new mode is 
     b*16 unless b is 3.  If b is 3 the new mode is b*4
 
     The 0xC0 flag is followed by a second mode giving
     the number of bytes of image starting offset address
     followed by the image offset address followed by the
     mode of that data.   0xC0 also acts as a reset.
     Use of the 0xC0 flag is not required.  Addresses
     default to sequential starting from 0, but is
     provided to faciliate parallel compression.
 
     The array succs is indexed by mode (not mode-1) and
     returns the next larger mode, so succs[2] = 4,
     succs[4] = 6, succs[6] = 8, succs[8] = 12, etc.

    */
    
#define CBF_NIBBLE_RUN      8
#define CBF_NIBBLE_RUN_MASK 7
#define CBF_NIBBLE_UPONE    1
#define CBF_NIBBLE_UPN      3
#define CBF_NIBBLE_SET_ADDR 0xC0
 
    static size_t histogram[65];
    static size_t runs[65];
    static size_t currun[65];
    static size_t succs[65]={ 0, 2, 4, 4, 6, 6, 8, 8,12,12,    /*0 - 9*/
                      12,12,16,16,16,16,32,32,32,32,    /*10-19*/
                      32,32,32,32,32,32,32,32,32,32,    /*20-29*/
                      32,32,64,64,64,64,64,64,64,64,    /*30-39*/
                      64,64,64,64,64,64,64,64,64,64,    /*40-49*/
                      64,64,64,64,64,64,64,64,64,64,    /*50-59*/
                      64,64,64,64,128};                 /*60-64*/
    
    static long int validrange[33] =
                     /* 0   1   2   3   4    5    6    7     8     9*/
                     { 0L, 0L, 1L, 3L, 7L, 15L, 31L, 63L, 127L, 255L,
                     /*  10     11     12     13     14 */
                       511L, 1023L, 2047L, 4095L, 8191L, 
                     /*    15      16      17       18       19 */
                       16383L, 32767L, 65535L, 131071L, 262143L, 
                     /*     20        21        22        23        24*/
                       524287L, 1048575L, 2097151L, 4194303L, 8388608L, 
                     /*       25         26         27          28          29*/
                       16777215L, 33554431L, 67108863L, 134217727L, 268435455L, 
                     /*       30           31           32*/
                      536870911L, 1073741823L, 2147483647L};
    
    static unsigned long int masks[33] =
                     /*   0     1     2     3      4     5      6    7     8     9 */
                     { 0x0L, 0x1L, 0x3L, 0x7L, 0xFL, 0x1FL, 0x3FL, 0x7FL, 0xFFL, 0x1FFL,
                     /*    10      11      12       13       14 */
                       0x3FFL, 0x7FFL, 0xFFFL, 0x1FFFL, 0x3FFFL,
                     /*     15       16        17        18        19 */
                       0x7FFFL, 0xFFFFL, 0x1FFFFL, 0x3FFFFL, 0x7FFFFL,  
                     /*      20         21         22        23         24 */
                       0xFFFFFL, 0x1FFFFFL, 0x3FFFFFL, 0x7FFFFFL, 0xFFFFFFL,
                     /*       25          26          27          28           29 */
                       0x1FFFFFFL, 0x3FFFFFFL, 0x7FFFFFFL, 0xFFFFFFFL, 0x1FFFFFFFL,
                     /*       30           31           32*/
                         0x3FFFFFFFL, 0x7FFFFFFFL, 0xFFFFFFFFL};
    int numints;
    
    unsigned int bits;
    
#define setcurmode(newmode) \
{   int a0, a1, b, md;      \
    md = (newmode);         \
    if ((newmode) == 6) md = 12;      \
        if ((newmode) == 12) md = 48; \
            a0 = md&0x3;              \
            a1 = (md&0xC)>>2;         \
            b = (md&0xF0)>>4;         \
            cbf_failnez(cbf_put_bits(file,&a0,2)) \
            if ((newmode) == 0 || b != 0) {         \
                cbf_failnez(cbf_put_bits(file,&a1,2)) \
                cbf_failnez(cbf_put_bits(file,&b,4))  \
                bsize += 8;                          \
            } else if (a1 != 0) {                     \
                cbf_failnez(cbf_put_bits(file,&a1,2)) \
                bsize += 4;                           \
            } else {                                  \
                bsize += 2;                           \
            }                                         \
}

    
    static int getcurmode ( cbf_file * file, size_t * bitsread){
        int a0, a1, b;
        cbf_failnez(cbf_get_bits(file,&a0,2))
        a0 &= 3;
        if (a0 != 0) {
            *bitsread += 2;
            return a0;
        }
        cbf_failnez(cbf_get_bits(file,&a1,2))
        a1 &= 3;
        if (a1 != 0) {
            *bitsread += 4;
            if (a1==3) return 6;
            return a1*4;
        }
        cbf_failnez(cbf_get_bits(file,&b,4))
        b &= 0xF;
        *bitsread += 8;
        if (b==3) return 12;
        return b*16;
    }
    
    static int testmode(int *delta,int kint) {
        if (kint == 0) {
            if (delta[0] >= -validrange[8] && delta[0] <=validrange[8] ) {
                if (delta[0] >= -validrange[2] && delta[0] <=validrange[2] ) return 2;
                if (delta[0] >= -validrange[4] && delta[0] <=validrange[4] ) return 4;
                if (delta[0] >= -validrange[6] && delta[0] <=validrange[6] ) return 6;
                return 8;
            }
            if (delta[0] >= -validrange[12] && delta[0] <=validrange[12] ) return 12;
            if (delta[0] >= -validrange[16] && delta[0] <=validrange[16] ) return 16;
            if (sizeof(int) >3 ) {
              if (delta[0] >= -validrange[32] &&
                      delta[0] <= validrange[32]) return 32;
              return 64;
            } else {
              return 32;
            }
        } else {
            if ((kint+1)*sizeof(int) < 5) {
                return 32;
            } else
                return 64;
        }
    }


#define changemode    \
{                     \
    int prevmode;     \
    int termmode;     \
    int xzero = 0;    \
    int iint, jsize;  \
    int jrun;         \
    int succnt = 0;   \
    if (numints==1 || curmode <= (int)(sizeof(int)*CHAR_BIT)) { \
      termmode = 1<<(curmode-1);  \
      cbf_failnez(cbf_put_bits(file,&termmode,curmode)) \
    } else { \
      jsize = 0; \
      for (iint=0; (int)(iint*sizeof(int)) < curmode; iint++) { \
        cbf_failnez(cbf_put_bits(file,&xzero,sizeof(int))) \
        jsize += sizeof(int); \
      } \
      termmode = 1<<(curmode-jsize);  \
      cbf_failnez(cbf_put_bits(file,&termmode,curmode-jsize)) \
    } \
    bsize += curmode; \
    if (bsize > 7) {  \
        csize += (bsize/8);  \
        bsize %= 8;   \
    }                 \
    jrun = (inrun+CBF_NIBBLE_RUN-lagby+1)&CBF_NIBBLE_RUN_MASK; \
    prevmode = curmode; \
    curmode = usedmode = newmode; \
    if ( tempmode==CBF_NIBBLE_UPN  && prevmode <= 32) { \
        if (curmode == (int)succs[prevmode]) { \
            succnt = 1; \
        } else if (prevmode <= 16 && curmode == (int)succs[succs[prevmode]]) { \
            succnt = 2; \
        } else if (prevmode <= 12 && curmode == (int)succs[succs[succs[prevmode]]]) { \
            succnt = 3; \
        } else if (prevmode <= 8 && curmode == (int)succs[succs[succs[succs[prevmode]]]]) { \
            succnt = 4; \
        } else if (prevmode <= 6 && curmode == (int)succs[succs[succs[succs[succs[prevmode]]]]]) { \
            succnt = 5; \
        } else { tempmode = curmode;} \
    } \
    if (tempmode==CBF_NIBBLE_UPN && succnt > 0 ) { \
        if (succnt==1) { \
          setcurmode(CBF_NIBBLE_UPONE); \
        } else { \
          setcurmode(CBF_NIBBLE_UPN);  \
          succnt-=2; \
          cbf_failnez(cbf_put_bits(file,&succnt,2)) \
          bsize += 2; \
        } \
        if (kint[jrun] == 0) {    \
            cbf_failnez(cbf_put_bits(file,delta[jrun],curmode))  \
            bsize += curmode; \
        } else {            \
            for (iint = 0; iint < (kint[jrun]+1); iint++) { \
                \
                cbf_failnez (cbf_put_integer (file, delta[jrun][iint], \
                                              iint==numints-1?1:0,\
                                              iint<(numints-1)?(CHAR_BIT*sizeof (int)): \
                                              curmode-(CHAR_BIT*sizeof (int))*iint ))\
            } \
            bsize += curmode; \
        } \
        curmode = prevmode; \
    } else {                                              \
        setcurmode(curmode);                              \
        if (kint[jrun] == 0) {                                  \
            cbf_failnez(cbf_put_bits(file,delta[jrun],curmode)) \
            bsize += curmode; \
        } else {                                          \
            for (iint = 0; iint < (kint[jrun]+1); iint++) {       \
                                                          \
                cbf_failnez (cbf_put_integer (file, delta[jrun][iint],                        \
                                              iint==numints-1?1:0,                      \
                                              iint<(numints-1)?(CHAR_BIT*sizeof (int)): \
                                              curmode-(CHAR_BIT*sizeof (int))*iint ))      \
            }               \
            bsize += curmode; \
        }                   \
    }                       \
    if (bsize > 7) {        \
        csize += (bsize/8); \
        bsize %= 8;         \
    }                       \
}


  /* Compress and array with the nibble-offset algorithm */

int cbf_compress_nibble_offset (void         *source,
                              size_t        elsize,
                              int           elsign,
                              size_t        nelem,
                              unsigned int  compression,
                              cbf_file     *file,
                              size_t       *compressedsize,
                              int          *storedbits,
                              int           realarray,
                              const char   *byteorder,
                              size_t        dimfast,
                              size_t        dimmid,
                              size_t        dimslow,
                              size_t        padding)
    {
        unsigned int count, borrow, element[4], prevelement[4], 
        
        unsign, sign, limit;
        
        unsigned char *unsigned_char_data;
        
        size_t csize, bsize;
                
        int delta[CBF_NIBBLE_RUN][4];
        
        int dmode[CBF_NIBBLE_RUN],maxmode,minmode,newmode,nextmaxmode,tempmode;
        
        int iint, ii, kint[CBF_NIBBLE_RUN], inrun=0, lagby;
        
        char * border;
        
        char * rformat;
        
        int curmode = 0;
                
        int zerobyte = 0;
        
        int dflag=0x2;
        
        int nflag=0x8;
        
        int sixflag = 0x20;
        
        int byteflag = 0x80;
        
        int shortflag = 0x8000;
                        
        int usedmode;
        
        int *termflag[17];

        CBF_UNUSED( termflag );

        CBF_UNUSED( borrow );
        
        CBF_UNUSED( compression );
        
        CBF_UNUSED( byteorder );
        
        CBF_UNUSED( dimfast );
        
        CBF_UNUSED( dimmid );
        
        CBF_UNUSED( dimslow );
        
        CBF_UNUSED( padding );
        
        termflag[2] = &dflag;
        
        termflag[4] = &nflag;
        
        termflag[6] = &sixflag;
        
        termflag[8] = &byteflag;
        
        termflag[16] = &shortflag;
        
        for (iint = 0; iint < 65; iint++) {
            histogram[iint] = 0;
            runs[iint] = 0;
            currun[iint] = 0;
        }
        
        /* Is the element size valid? */
        
        if (elsize != sizeof (int) &&
            elsize != 2* sizeof (int) &&
            elsize != 4* sizeof (int) &&
            elsize != sizeof (short) &&
            elsize != sizeof (char))
            
            return CBF_ARGUMENT;
        
        /* check for compatible real format */
        
        if ( realarray ) {
            
            cbf_failnez (cbf_get_local_real_format(&rformat) )
            
            if ( strncmp(rformat,"ieee",4) ) return CBF_ARGUMENT;
            
        }
        
        bits = elsize * CHAR_BIT;
        
        if (bits < 1 || bits > 64)
            
            return CBF_ARGUMENT;
        
        if (bits != 8 && bits != 16 && bits != 32 && bits != 64)
            
            return CBF_ARGUMENT;
        
        numints = (bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));
        
        
        /* Maximum limits */
        
        sign = 1 << ((elsize-(numints-1)*sizeof(int))* CHAR_BIT - 1);
        
        if (elsize == sizeof (int) || elsize == numints*sizeof(int) )
            
            limit = ~0;
        
        else
            
            if (numints == 1) {
                
                limit = ~-(1 << (elsize * CHAR_BIT));
                
            } else {
                
                limit = ~-(1 << ((elsize-(numints-1)*sizeof(int)) * CHAR_BIT));
                
            }
        
        
        if (storedbits)
            
            *storedbits = bits;
        
        
        /* Offset to make the value unsigned */
        
        if (elsign)
            
            unsign = sign;
        
        else
            
            unsign = 0;
        
        /* Get the local byte order */
        
        if (realarray) {
            
            cbf_get_local_real_byte_order(&border);
            
        } else {
            
            cbf_get_local_integer_byte_order(&border);
            
        }
    
        
        /* Initialise the pointer */
        
        unsigned_char_data = (unsigned char *) source;
        
        
        /* Set up the previous element for comparison */
        
        prevelement[0] = prevelement[1] = prevelement[2] = prevelement[3] = 0;
        
        prevelement[numints-1] = unsign;
        
        
        /* Write the elements */
        
        
        csize = 0;
        
        bsize = 0;
        
        curmode  = 0;
        
        lagby = 0;
        
        
        for (count = 0; count < nelem; count++) {
            
            inrun = count&CBF_NIBBLE_RUN_MASK;
            
            /* Get the next element */
            
            if (numints > 1) {
                
                if (border[0] == 'b') {
                    
                    for (iint = numints; iint; iint--) {
                        
                        element[iint-1] = *((unsigned int *) unsigned_char_data);
                        
                        unsigned_char_data += sizeof (int);
                        
                    }
                    
                } else {
                    
                    for (iint = 0; iint < numints; iint++) {
                        
                        element[iint] = *((unsigned int *) unsigned_char_data);
                        
                        unsigned_char_data += sizeof (int);
                    }
                }
                
            } else {
                
                if (elsize == sizeof (int))
                    
                    element[0] = *((unsigned int *) unsigned_char_data);
                
                else
                    
                    if (elsize == sizeof (short))
                        
                        element[0] = *((unsigned short *) unsigned_char_data);
                
                    else
                        
                        element[0] = *unsigned_char_data;
                
                unsigned_char_data += elsize;
                
            }
            
            
            /* Make the element unsigned */
            
            element[numints-1] += unsign;
            
            element[numints-1] &= limit;
                        
            /* Compute the delta */
            
            borrow = 0;
            
            kint[inrun] = 0;
            
            if (numints > 1) {
                
                for (iint = 0; iint < numints; iint++) delta[inrun][iint] = prevelement[iint];
                
                cbf_failnez(cbf_mpint_negate_acc((unsigned int *)delta[inrun],numints));
                
                cbf_failnez(cbf_mpint_add_acc((unsigned int *)delta[inrun], numints, element, numints))
                
                delta[inrun][numints-1] &=limit;
                
                if (delta[inrun][numints-1] & sign) delta[inrun][numints-1] |= (~limit);
                                
            } else  {
                
                delta[inrun][0] = element[0] - prevelement[0];
                
                delta[inrun][0] &=limit;
                
                if (delta[inrun][0] & sign) delta[inrun][0] |= (~limit);
                
            }
            
            
            prevelement[0] = element[0];
            
            for (iint = 1; iint < numints; iint++) {
                
                prevelement[iint] = element[iint];
                
                if ((delta[inrun][0] >= 0 && delta[inrun][iint] != 0 )
                    || (delta[inrun][0] < 0 && (delta[inrun][iint]+1)!=0) ) kint[inrun] = iint;
                
            }

            dmode[inrun] = testmode(delta[inrun],kint[inrun]);
            currun[dmode[inrun]]++;
            if (count > 0 && dmode[inrun] != dmode[(inrun+CBF_NIBBLE_RUN-1)&CBF_NIBBLE_RUN_MASK]) runs[dmode[(inrun+CBF_NIBBLE_RUN-1)&CBF_NIBBLE_RUN_MASK]]++;
            
            lagby++;
 
            if (lagby < CBF_NIBBLE_RUN) continue;
            
            if (curmode == 0) {
                
                curmode = dmode[inrun];
                for (ii=1;ii<lagby;ii++) {
                    if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]>curmode) {
                        curmode=dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                    }
                }
                histogram[curmode]+=lagby;
                setcurmode(curmode);
                for (ii=lagby-1;ii>=0;ii--){
                    if (kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]==0) {
                        cbf_failnez(cbf_put_bits(file,delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK],curmode))
                    } else {
                        for (iint = 0; iint < kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]+1; iint++) {
                            cbf_failnez (cbf_put_integer (file, delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK][iint],
                                                          iint==numints-1?1:0, iint<(numints-1)?(CHAR_BIT*sizeof (int)): curmode-(CHAR_BIT*sizeof (int))*iint ))
                        }
                    }
                }
                
                bsize += (curmode*lagby);
                if (bsize > 7) {
                    csize += (bsize/8);
                    bsize %= 8;
                }
                lagby = 0;
                continue;
            }
            
            /* we are already in a workable mode,
               stay with it if possible, unless we can drop
               to a smaller value for at least CBF_NIBBLE_RUN values*/
            
            
            maxmode = minmode = nextmaxmode = dmode[inrun];
            for (ii=1;ii<lagby;ii++) {
                if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]>maxmode) {
                    maxmode = dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                    if (ii < lagby-1) nextmaxmode= dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                }
                if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]<minmode) {
                    minmode = dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                }
            }
            ii = lagby-1;
            if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]<=curmode
                && maxmode>= curmode) {
                histogram[curmode]++;
                if (kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]==0) {
                    cbf_failnez(cbf_put_bits(file,delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK],curmode))
                } else {
                    for (iint = 0; iint < kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]+1; iint++) {
                        cbf_failnez (cbf_put_integer (file, delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK][iint],
                                                      iint==numints-1?1:0, iint<(numints-1)?(CHAR_BIT*sizeof (int)): curmode-(CHAR_BIT*sizeof (int))*iint ))
                    }
                }
                bsize += curmode;
                if (bsize > 7) {
                    csize += (bsize/8);
                    bsize %= 8;
                }
                lagby--;
                continue;
            }
            
            /* we need to change --  see if we need to go up */
            
            if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]> curmode) {
                newmode = dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                tempmode = CBF_NIBBLE_UPN;
                if (nextmaxmode > curmode){
                    tempmode = newmode = maxmode;
                }
                changemode;
                lagby--;
                continue;
            }
            
            /* that leaves going up or down to whatever is in the next run */
            
            tempmode = newmode = maxmode;
            changemode;
            lagby--;
            continue;
            
        }
        
        if (curmode == 0) {
            
            curmode = dmode[inrun];
            for (ii=1;ii<lagby;ii++) {
                if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]>curmode) {
                    curmode=dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                }
            }
            histogram[curmode]+=lagby;
            setcurmode(curmode);
            for (ii=lagby-1;ii>=0;ii--){
                if (kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]==0) {
                    cbf_failnez(cbf_put_bits(file,delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK],curmode))
                } else {
                    for (iint = 0; iint < kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]+1; iint++) {
                        cbf_failnez (cbf_put_integer (file, delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK][iint],
                                                      iint==numints-1?1:0, iint<(numints-1)?(CHAR_BIT*sizeof (int)): curmode-(CHAR_BIT*sizeof (int))*iint ))

                    }
                }
            }
            
            bsize += (curmode*lagby);
            if (bsize > 7) {
                csize += (bsize/8);
                bsize %= 8;
            }
            lagby = 0;
        } else {
            maxmode = dmode[inrun];
            for (ii=1;ii<lagby;ii++) {
                if (dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]>maxmode) {
                    maxmode = dmode[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK];
                }
            }
            if (maxmode > curmode ) {
                tempmode=newmode=maxmode;
                changemode;
                lagby--;
            }
            if (lagby > 0) {
                histogram[curmode]+= lagby;
                for (ii=lagby-1;ii>=0; ii--) {
                    if (kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]==0) {
                        cbf_failnez(cbf_put_bits(file,delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK],curmode))
                    } else {
                        for (iint = 0; iint < kint[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK]+1; iint++) {
                            cbf_failnez (cbf_put_integer (file, delta[(CBF_NIBBLE_RUN+inrun-ii)&CBF_NIBBLE_RUN_MASK][iint],
                                                          iint==numints-1?1:0, iint<(numints-1)?(CHAR_BIT*sizeof (int)): curmode-(CHAR_BIT*sizeof (int))*iint ))
                        }
                    }
                }
                bsize += (curmode*lagby);
                if (bsize > 7) {
                    csize += (bsize/8);
                    bsize %= 8;
                }
                lagby=0;
            }
        }
        
        bsize %= 8;
        if (bsize !=0) {
            cbf_failnez(cbf_put_bits(file,&zerobyte,8-bsize))
            csize++;
            bsize = 0;
        }
        /* Return the number of characters written */
        
        if (compressedsize)
            
            *compressedsize = csize;
        
/*      fprintf(stderr," nibble offset compressed size %ld elements %ld\n",(long)csize,(long)nelem);
        for (ii=1;ii<65;ii++) {
            if (histogram[ii]) fprintf(stderr,"histogram[%d] %d\n",ii,(int)histogram[ii]);
            if (runs[ii]) fprintf(stderr,"avgrun[%d] %g\n",ii,((double)currun[ii])/((double)runs[ii]));
        }
*/
        
        
        /* Success */
        
        return 0;
    }


  /* Decompress an array with the nibble-offset algorithm */

int cbf_decompress_nibble_offset (void         *destination,
                                size_t        elsize,
                                int           elsign,
                                size_t        nelem,
                                size_t       *nelem_read,
                                size_t        compressedsize,
                                unsigned int  compression,
                                int           data_bits,
                                int           data_sign,
                                cbf_file     *file,
                                int           realarray,
                                const char   *byteorder,
                                size_t        dimover,
                                size_t        dimfast,
                                size_t        dimmid,
                                size_t        dimslow,
                                size_t        padding)
    {
        unsigned int element[4], prevelement[4], sign, unsign, limit;
        
        unsigned int data_unsign;
        
        unsigned char *unsigned_char_data;
        
        int errorcode, overflow, numints, iint;
        
        int delta[4];
        
        int curmode, prevmode, nextmode;
        
        char * border;
        
        char * rformat;
        
        size_t numread,bitsread;
        
        int succnt, sucmode;

        CBF_UNUSED( errorcode );
        
        CBF_UNUSED( compressedsize );
        
        CBF_UNUSED( compression );
        
        CBF_UNUSED( byteorder );
        
        CBF_UNUSED( dimover );
        
        CBF_UNUSED( dimfast );
        
        CBF_UNUSED( dimmid );
        
        CBF_UNUSED( dimslow );
        
        CBF_UNUSED( padding );
        
        /* prepare the errorcode */
        
        errorcode = 0;
        
        /* Is the element size valid? */
        
        if (elsize != sizeof (int) &&
            elsize != 2* sizeof (int) &&
            elsize != 4* sizeof (int) &&
            elsize != sizeof (short) &&
            elsize != sizeof (char))
            
            return CBF_ARGUMENT;
        
        /* check for compatible real format */
        
        if ( realarray ) {
            
            cbf_failnez (cbf_get_local_real_format(&rformat) )
            
            if ( strncmp(rformat,"ieee",4) ) return CBF_ARGUMENT;
            
        }
        
        /* Check the stored element size */
        
        if (data_bits < 1 || data_bits > 64)
            
            return CBF_ARGUMENT;
        
        numints = (data_bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));
        
        
        /* Initialise the pointer */
        
        unsigned_char_data = (unsigned char *) destination;
        
        
        /* Maximum limits */
        
        sign = 1 << ((elsize-(numints-1)*sizeof(int))* CHAR_BIT - 1);
        
        if (elsize == sizeof (int) || elsize == numints*sizeof(int))
            
            limit = ~0;
        
        else
            
            if (numints == 1 ) {
                
                limit = ~(-(1 << (elsize * CHAR_BIT)));
                
            } else {
                
                limit = ~(-(1 << ((elsize-(numints-1)*sizeof(int)) * CHAR_BIT)));
                
            }
        
        
        /* Offsets to make the value unsigned */
        
        if (data_sign)
            
            data_unsign = sign;
        
        else
            
            data_unsign = 0;
        
        if (elsign)
            
            unsign = sign;
        
        else
            
            unsign = 0;
        
        /* Get the local byte order */
        
        if (realarray) {
            
            cbf_get_local_real_byte_order(&border);
            
        } else {
            
            cbf_get_local_integer_byte_order(&border);
            
        }
        
        
        /* Set up the previous element for increments */
        
        prevelement[0] = prevelement[1] = prevelement[2] = prevelement[3] = 0;
        
        prevelement[numints-1] = data_unsign;
        
        
        /* Read the elements */
        
        overflow = 0;
        
        numread = 0;
        
        bitsread = 0;
        
        curmode = 0;
        
        prevmode = 0;
        
        nextmode = 0;
        
        prevelement[0] = prevelement[1] = prevelement[2] = prevelement[3] = 0;
        prevelement[numints-1] = data_unsign;

        while (numread < nelem)
        {
            
            while (curmode==0) {
                curmode = getcurmode(file,&bitsread);
                if (curmode==0 && prevmode==0) {
                    prevelement[0] = prevelement[1] = prevelement[2] = prevelement[3] = 0;
                    prevelement[numints-1] = data_unsign;                    
                }
                if (prevmode==0 && (curmode==CBF_NIBBLE_UPN||curmode==CBF_NIBBLE_UPONE)) {
                    curmode = 0;
                    prevmode=0;
                }
            }
            
            for (iint=0; iint < numints; iint++){
                
                element[iint] = prevelement[iint];
                
                delta[iint] = 0;
                
            }
            
            switch (curmode) {
                case(CBF_NIBBLE_UPN):
                    cbf_failnez(cbf_get_bits(file,&succnt,2))
                    succnt &= 3;
                    sucmode = (succnt==0)?succs[succs[prevmode]]:
                      ((succnt==1)?succs[succs[succs[prevmode]]]:
                       ((succnt==2)?succs[succs[succs[succs[prevmode]]]]:
                        succs[succs[succs[succs[succs[prevmode]]]]]));
                    cbf_failnez(cbf_get_bits(file,delta,sucmode))
                    if (succs[prevmode] < sizeof(int))
                        delta[0] &= masks[sucmode];
                    bitsread += sucmode;
                    nextmode = prevmode;
                    curmode = sucmode;
                    break;
                case(CBF_NIBBLE_UPONE):
                    succnt &= 3;
                    sucmode = succs[prevmode];
                    cbf_failnez(cbf_get_bits(file,delta,sucmode))
                    if (succs[prevmode] < sizeof(int))
                        delta[0] &= masks[sucmode];
                    bitsread += sucmode;
                    nextmode = prevmode;
                    curmode = sucmode;
                    break;
                 case(CBF_NIBBLE_SET_ADDR):
                    fprintf(stderr,"CBFLIB:  cbf-nibble_offset CBF_NIBBLE_SET_ADDR not implemented yet\n");
                    cbf_failnez(CBF_FORMAT);
                    break;

                 default:
                    cbf_failnez(cbf_get_bits(file,delta,curmode))
                    if (curmode < (int)sizeof(int))
                        delta[0] &= masks[curmode];
                    bitsread += curmode;
                    nextmode = curmode;
                    break;
            }
            
            bitsread %= 8;
            
            switch (curmode) {
                    
                case(2):
                    if ((delta[0]&0x3) == 0x2) {
                        prevmode=curmode;
                        nextmode = 0;
                    } else {
                        if ((delta[0]&0x2) == 0x2) {
                            delta[0] = -1;
                            for (iint = 1; iint < numints; iint++) {
                                delta[iint] = ~0;                            
                            }   
                        }
                    }
                    break;
                    
                case(4):
                    if ((delta[0]&0xF) == 0x8) {
                        prevmode=curmode;
                        nextmode = 0;
                    } else {
                        if ((delta[0]&0x8) == 0x8) {
                            delta[0] |= ~0xF;
                            for (iint = 1; iint < numints; iint++) {
                                delta[iint] = ~0;                            
                            } 
                        }
                    }
                    break;
                    
                case(6):
                    if ((delta[0]&0x3F) == 0x20) {
                        prevmode=curmode;
                        nextmode = 0;
                    } else {
                        if ((delta[0]&0x20) == 0x20) {
                            delta[0] |= ~0x3F;
                            for (iint = 1; iint < numints; iint++) {
                                delta[iint] = ~0;                            
                            } 
                        }
                    }
                    break;
                    
                case(8):
                    if ((delta[0]&0xFF) == 0x80) {
                        prevmode=curmode;
                        nextmode = 0;
                    } else {
                        if ((delta[0]&0x80) == 0x80) {
                            delta[0] |= ~0xFF;
                            for (iint = 1; iint < numints; iint++) {
                                delta[iint] = ~0;                            
                            }  
                        }
                    }
                    break;
                    
                case(12):
                    if ((delta[0]&0xFFF) == 0x800) {
                        prevmode=curmode;
                        nextmode = 0;
                    } else {
                        if ((delta[0]&0x800) == 0x800) {
                            delta[0] |= ~0xFFF;
                            for (iint = 1; iint < numints; iint++) {
                                delta[iint] = ~0;                            
                            }  
                        }
                    }
                    break;
                    
                case(16):
                    if ((delta[0]&0xFFFF) == 0x8000){
                        prevmode=curmode;
                        nextmode = 0;
                    } else {
                        if ((delta[0]&0x8000) == 0x8000) {
                            delta[0] |= ~0xFFFF;
                            for (iint = 1; iint < numints; iint++) {
                                delta[iint] = ~0;                            
                            }
                        }
                    }
                    break;
                    
                case(32):
                    if (sizeof(int) == 2) {
                        if ((delta[1]&0xFFFF) == 0x8000
                            && (delta[0]&0xFFFF) == 0) {
                            prevmode=curmode;
                            nextmode = 0;
                        } else {
                            if ((delta[1]&0x8000) == 0x8000) {
                                for (iint = 2; iint < numints; iint++) delta[iint] = ~0;
                            }
                        }
                        
                    } else {
                        if ((delta[0]&0xFFFFFFFF) == 0x80000000) {
                            prevmode=curmode;
                            nextmode = 0;
                        } else {
                            if ((delta[0]&0x80000000) == 0x80000000) {
                                delta[0] |= ~0xFFFFFFFFFF;
                                for (iint = 1; iint < numints; iint++) {
                                    delta[iint] = ~0;                            
                                }
                            }
                        }
                    } 
                    break;

                case(64):
                    if (sizeof(int) == 2) {
                        if ((delta[3]&0xFFFF) == 0x8000
                            && (delta[2]&0xFFFF) == 0
                            && (delta[1]&0xFFFF) == 0
                            && (delta[0]&0xFFFF) == 0) {
                            prevmode=curmode;
                            nextmode = 0;
                        } else {
                            if ((delta[3]&0x8000) == 0x8000) {
                                for (iint = 4; iint < numints; iint++) delta[iint] = ~0;
                            }
                        }

                    } else if (sizeof(int) == 4) {
                        if ((delta[1]&0xFFFFFFFF) == 0x80000000L
                            && (delta[0]&0xFFFFFFFF) == 0x00000000L) {
                            prevmode=curmode;
                            nextmode = 0;
                        } else {
                            if ((delta[1]&0x80000000L) == 0x80000000L) {
                                for (iint = 2; iint < numints; iint++) delta[iint] = ~(0L);
                            }
                        }
                        
                    } else {
                        if ((((long)delta[0]>>32)&0xFFFFFFFFL)==0x80000000L&&(((long)delta[0]&0xFFFFFFFFl)==0L)) {
                            prevmode=curmode;
                            nextmode = 0;
                        } else {
                            if ((((long)delta[0]>>32)&0x80000000L) == 0x80000000L) {
                                delta[0] |= ~(-1L);
                                for (iint = 1; iint < numints; iint++) {
                                    delta[iint] = ~(0L);
                                }
                            }
                        }
                    } 
                    break;

            }
            
            prevmode = curmode;
            
            curmode = nextmode;
            
            if (curmode == 0) continue;
            
            if (numints > 1) {
                
                for (iint = 0; iint < numints; iint++) element[iint] = prevelement[iint];
                
                cbf_failnez(cbf_mpint_add_acc(element,numints, (unsigned int *)delta,numints))
                
            } else {
                
                element[0] = prevelement[0] + delta[0];
                
                element[0] &= limit;
                
            }
            
            for (iint = 0; iint < numints; iint++)   {
                
                prevelement[iint] = element[iint];
                
            }
            
            /* Make the element signed? */
            
            element[numints-1] -= unsign;
            
                        
            /* Save the element */
            
            if (numints > 1) {
                
                if (border[0] == 'b') {
                    
                    for (iint = numints; iint; iint--) {
                        
                        *((unsigned int *) unsigned_char_data) = element[iint-1];
                        
                        unsigned_char_data += sizeof (int);
                        
                    }
                    
                } else {
                    
                    for (iint = 0; iint < numints; iint++) {
                        
                        *((unsigned int *) unsigned_char_data) = element[iint];
                        
                        unsigned_char_data += sizeof (int);
                    }
                }
                
            } else {
                
                if (elsize == sizeof (int))
                    
                    *((unsigned int *) unsigned_char_data) = element[0];
                
                else
                    
                    if (elsize == sizeof (short))
                        
                        *((unsigned short *) unsigned_char_data) = element[0];
                
                    else
                        
                        *unsigned_char_data = element[0];
                
                unsigned_char_data += elsize;
                
            }
            
            numread++;
            
        }
        
        /* Number read */
        
        if (nelem_read)
            
            *nelem_read = numread;
        
        
        /* Success */
        
        return overflow;
    }



#ifdef __cplusplus

}

#endif
