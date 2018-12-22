/**********************************************************************
 * cbf_byte_offset -- byte-offset compression                         *
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
#include "cbf_byte_offset.h"


  /* Compress and array with the byte-offset algorithm */

int cbf_compress_byte_offset (void         *source,
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
        
        unsign, sign, limit, bits;
        
        unsigned char *unsigned_char_data;
        
        unsigned char *unsigned_char_dest=NULL;
        
        int delta[4];
        
        int numints, iint, kint;
        
        char * border;
        
        char * rformat;
        
        size_t csize;
        
        int bflag=0x800080;
        
        int bbflag=0x80000000;
        
        int byte0, byte1, byte2, byte3;
        
        int sbyte0, sbyte1;
        
        unsigned char fixup0;
        
        unsigned char fixup1;
        
        CBF_UNUSED(dimfast);
        
        CBF_UNUSED(dimmid);
        
        CBF_UNUSED(dimslow);
        
        CBF_UNUSED(padding);

        CBF_UNUSED(borrow);
        
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
        
        fixup0 = 0x00;
        
        fixup1 = 0x80;
        
        sbyte0 = byte0 = 0;
        
        sbyte1 = byte1 = 1;
        
        byte2 = 2;
        
        byte3 = 3;
        
        if (toupper(border[0]) != toupper(byteorder[0])) {
            
            byte0 = 3;
            
            byte1 = 2;
            
            sbyte0 = byte2 = 1;
            
            sbyte1 = byte3 = 0;
            
        }
        
        
        if (toupper(byteorder[0]) == 'B'){
            
            fixup0 = 0x80;
            
            fixup1 = 0x0;
            
        }
        
        
        /* Initialise the pointer */
        
        unsigned_char_data = (unsigned char *) source;
        
        
        /* Set up the previous element for comparison */
        
        prevelement[0] = prevelement[1] = prevelement[2] = prevelement[3] = 0;
        
        prevelement[numints-1] = unsign;
        
        csize = 0;
        
        /* Write the elements */
        
#ifndef CBF_NOFAST_BYTE_OFFSET
        /* First try a fast memory-memory transfer */  
        
        switch (elsize) {
                
            case (1): /* Doing byte_offset with elsize 1 does
             not make much sense, but we can at
             least do it quickly */
                
                if (!cbf_set_output_buffersize(file,nelem*2))  {
                    
                    unsigned_char_dest = 
                    (unsigned char *)(file->characters+file->characters_used);
                    
                    if (elsign) {
                      char pc = 0;
                      int delta;
                      for (count = 0; count < nelem; count++) {
                        delta = *unsigned_char_data - pc;
                        if (delta < -127 || delta > 127) {
                          *unsigned_char_dest++ = 0x80;
                          *unsigned_char_dest++ = delta & 0xff;
                          *unsigned_char_dest++ = (delta >> 8) & 0xff;
                          csize += 3;
                        } else {
                          *unsigned_char_dest++ = delta;
                          csize++;
                        }
                        pc = *unsigned_char_data++;
                      }
                    } else {
                      unsigned char pc = 0;
                      int delta;
                      for (count = 0; count < nelem; count++) {
                        delta = *unsigned_char_data - pc;
                        if (delta < -127 || delta > 127) {
                          *unsigned_char_dest++ = 0x80;
                          *unsigned_char_dest++ = delta & 0xff;
                          *unsigned_char_dest++ = (delta >> 8) & 0xff;
                          csize += 3;
                        } else {
                          *unsigned_char_dest++ = delta;
                          csize++;
                        }
                        pc = *unsigned_char_data++;
                      }
                    }
                    file->characters_used+=csize;
                    
                    if (compressedsize)
                        
                        *compressedsize = csize;
                    
                    return 0;
                    
                }
                
                break;
                
            case (2): /* We are compressing 16-bit data, which should
             compress to about half.  We allow up to the
             full size of the original data */
                
                if (!cbf_set_output_buffersize(file,nelem*elsize))  {
                    
                    short int pint;
                    
                    long int dint;
                    
                    short int *sint;
                    
                    if (sizeof(short int) != 2)  break;
                    
                    pint = 0;
                                        
                    unsigned_char_dest = 
                    (unsigned char *)(file->characters+file->characters_used);
                    
                    sint = (short int *) unsigned_char_data;
                                        
                    
                    for (count = 0; 3*count < 2*nelem; count++) {
                        
                        dint = (long int)sint[count] - (long int) pint;
                        
                        pint = sint[count];
                        
                        if (dint <= 127 && dint >= -127)  {
                            
                            *unsigned_char_dest++ = (unsigned char)dint;
                            
                            csize ++;
                            
                        } else {
                            
                            if (dint <= 32767 && dint >= -32767) {
                                
                                *unsigned_char_dest++ = 0x80;
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[sbyte0];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[sbyte1];
                                
                                csize += 3;
                                
                            } else {
                                                                
                                *unsigned_char_dest++ = 0x80;
                                
                                *unsigned_char_dest++ = fixup0;
                                
                                *unsigned_char_dest++ = fixup1;
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte0];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte1];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte2];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte3];
                                
                                csize +=7;
                                
                            }
                            
                        }
                        
                    } 
                    
                    /* At this point nelem-count elements remain and
                     file->characters_size-(csize+file->characters_used)
                     characters remain in the buffer */
                    
                    if (elsize*(nelem-count) > file->characters_size-(csize+file->characters_used)) {
                        
                        if (compression&CBF_NO_EXPAND) break;
                        
                        if (cbf_set_output_buffersize(file,1024+(nelem*elsize*3)/2)) break;
                        
                        unsigned_char_dest = 
                        
                        (unsigned char *)(file->characters+file->characters_used+csize);
                        
                    }
                    
                    for (; count < nelem; count++) {
                        
                        dint = (long int)sint[count] - (long int)pint;
                        
                        pint = sint[count];
                        
                        if (dint <= 127 && dint >= -127)  {
                            
                            *unsigned_char_dest++ = (unsigned char)dint;
                            
                            csize ++;
                            
                        } else {

                            if (dint <= 32767 && dint >= -32767) {

                            if (csize > nelem*elsize-3 ) {
                                
                                if (compression&CBF_NO_EXPAND) return CBF_NOCOMPRESSION;
                                
                            }
                            
                            if (elsize*(nelem-count) > file->characters_size-(csize+file->characters_used)) {
                                
                                if (cbf_set_output_buffersize(file,1024+nelem*elsize*2)) break;
                                
                                unsigned_char_dest = 
                                
                                (unsigned char *)(file->characters+file->characters_used+csize);
                                
                            }
                            
                            *unsigned_char_dest++ = 0x80;
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte0];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte1];
                            
                            csize += 3;
                                
                            } else {
                                
                                if (csize > nelem*elsize-7 ) {
                                    
                                    if (compression&CBF_NO_EXPAND) return CBF_NOCOMPRESSION;
                                    
                                }
                                
                                if (elsize*(nelem-count) > file->characters_size-(csize+file->characters_used)) {
                                    
                                    if (cbf_set_output_buffersize(file,1024+nelem*elsize*2)) break;
                                    
                                    unsigned_char_dest = 
                                    
                                    (unsigned char *)(file->characters+file->characters_used+csize);
                                    
                                }
                                
                                *unsigned_char_dest++ = 0x80;
                                
                                *unsigned_char_dest++ = fixup0;
                                
                                *unsigned_char_dest++ = fixup1;
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte0];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte1];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte2];
                                
                                *unsigned_char_dest++ = ((unsigned char *)&dint)[byte3];
                                
                                csize +=7;                                

                           }
                            
                        }
                        
                    } 
                    
                    file->characters_used+=csize;
                    
                    if (compressedsize)
                        
                        *compressedsize = csize;
                    
                    return 0;
                    
                }
                
                break;
                
                
                
            case (4):
                
                /* We are compressing 32-bit data, which should
  	             compress to about one quarter.  We allow up to the
  	             full size of the original data */
                
                if (!cbf_set_output_buffersize(file,nelem*elsize))  {
                    
                    int pint, dint;
                    
                    short int sint;
                    
                    int *oint;
                                        
                    if (sizeof(short int) != 2)  break;
                    
                    if (sizeof(int) != 4) break;
                    
                    pint = 0;
                                        
                    unsigned_char_dest = 
                    (unsigned char *)(file->characters+file->characters_used);
                    
                    oint = (int *) unsigned_char_data;
                                        
                    for (count = 0; 7*count < 4*nelem; count++) {
                        
                        dint = oint[count] - pint;
                        
                        pint = oint[count];
                        
                        if (dint <= 127 && dint >= -127)  {
                            
                            *unsigned_char_dest++ = (unsigned char)dint;
                            
                            csize ++;
                            
                        } else if (dint <= 32767 && dint >= -32767) {
                            
                            *unsigned_char_dest++ = 0x80;
                            
                            sint = dint;
                            
                            *unsigned_char_dest++ = ((unsigned char *)&sint)[sbyte0];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&sint)[sbyte1];
                            
                            csize += 3;
                            
                        } else {
                            
                            *unsigned_char_dest++ = 0x80;
                            
                            *unsigned_char_dest++ = fixup0;
                            
                            *unsigned_char_dest++ = fixup1;
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte0];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte1];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte2];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte3];
                            
                            csize +=7;
                            
                            
                        }
                        
                    } 
                    
                    /* At this point nelem-count elements remain and
                     file->characters_size-(csize+file->characters_used)
                     characters remain in the buffer */
                    
                    if (elsize*(nelem-count) > file->characters_size-(csize+file->characters_used)) {
                        
                        if (compression&CBF_NO_EXPAND) break;
                        
                        if (cbf_set_output_buffersize(file,1024+(nelem*elsize*7)/4)) break;
                        
                        unsigned_char_dest = 
                        
                        (unsigned char *)(file->characters+file->characters_used+csize);
                        
                    }
                    
                    
                    for (; count < nelem; count++) {
                        
                        dint = oint[count] - pint;
                        
                        pint = oint[count];
                        
                        if (dint <= 127 && dint >= -127)  {
                            
                            *unsigned_char_dest++ = (unsigned char)dint;
                            
                            csize ++;
                            
                            
                            
                        } else if (dint <= 32767 && dint >= -32767) {
                            
                            *unsigned_char_dest++ = 0x80;
                            
                            sint = dint;
                            
                            *unsigned_char_dest++ = ((unsigned char *)&sint)[sbyte0];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&sint)[sbyte1];
                            
                            csize += 3;
                            
                        } else {
                            
                            if (csize > nelem*elsize-7 ) {
                                
                                if (compression&CBF_NO_EXPAND) return CBF_NOCOMPRESSION;
                                
                            }
                            
                            if (elsize*(nelem-count) > file->characters_size-(csize+file->characters_used)) {
                                
                                if (cbf_set_output_buffersize(file,1024+nelem*elsize*2)) break;
                                
                                unsigned_char_dest = 
                                
                                (unsigned char *)(file->characters+file->characters_used+csize);
                                
                            }
                            
                            *unsigned_char_dest++ = 0x80;
                            
                            *unsigned_char_dest++ = fixup0;
                            
                            *unsigned_char_dest++ = fixup1;
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte0];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte1];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte2];
                            
                            *unsigned_char_dest++ = ((unsigned char *)&dint)[byte3];
                            
                            csize +=7;
                            
                            
                        }
                        
                    } 
                    
                    file->characters_used+=csize;
                    
                    if (compressedsize)
                        
                        *compressedsize = csize;
                    
                    return 0;
                    
                }
                
                break;
                
                
            default:
                break;
        }
#endif
        
        /* If we got here, we will do it the slow, painful way */
        
        
        for (count = 0; count < nelem; count++) {
            
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
            
            kint = 0;
            
            if (numints > 1) {
                
                for (iint = 0; iint < numints; iint++) delta[iint] = prevelement[iint];
                
                cbf_failnez(cbf_mpint_negate_acc((unsigned int *)delta,numints));
                
                cbf_failnez(cbf_mpint_add_acc((unsigned int *)delta, numints, element, numints))
                
                if (delta[numints-1] & sign) delta[numints-1] |= (~limit);
                
            } else  {
                
                delta[0] = element[0] - prevelement[0];
                
                if (delta[0] & sign) delta[0] |= (~limit);
                
            }
            
            
            prevelement[0] = element[0];
            
            for (iint = 1; iint < numints; iint++) {
                
                prevelement[iint] = element[iint];
                
                if ((delta[0] >= 0 && delta[iint] != 0 )
                    || (delta[0] < 0 && (delta[iint]+1)!=0) ) kint = iint;
                
            }
            
            if (kint == 0)  {
                
                if (delta[0] <= 127 && delta[0] >= -127) {
                    
                    cbf_failnez(cbf_put_bits(file,&delta[0],8))
                    
                    csize++;
                    
                } else if (sizeof(int) > 1 && delta[0] <= 32767 && delta[0] >= -32767) {
                    
                    cbf_failnez(cbf_put_bits(file,&bflag,8))
                    
                    cbf_failnez (cbf_put_integer (file, delta[0], 1, 16))
                    
                    csize +=3;   
                    
                } else if ( sizeof(int) > 2 && 
                           (sizeof(int) < 5 || ((long)delta[0] <= 2147483647L && (long)delta[0] >= -2147483647L ) ) ){
                    
                    cbf_failnez(cbf_put_bits(file,&bflag,24))
                    
                    cbf_failnez(cbf_put_integer (file, delta[0], 1, 32))
                    
                    csize +=7;
                    
                } else if (sizeof(int) > 4 ) {
                    
                    cbf_failnez(cbf_put_bits(file,&bflag,24))
                    
                    cbf_failnez(cbf_put_bits(file,&bbflag,32))
                    
                    cbf_failnez (cbf_put_integer (file, delta[0], 1, 64))
                    
                    csize += 15;
                    
                } else {
                    
                    return CBF_ARGUMENT;
                    
                }
                
            } else {
                
                if ((kint+1)*sizeof(int) < 5  ) {
                    
                    cbf_failnez(cbf_put_bits(file,&bflag,24))
                    
                    for (iint = 0; iint < kint+1; iint++) {
                        
                        cbf_failnez (cbf_put_integer (file, delta[iint],
                                                      iint==numints-1?1:0,
                                                      iint<(numints-1)?(CHAR_BIT*sizeof (int)):
                                                      bits-(CHAR_BIT*sizeof (int))*iint ))
                    }
                    
                    csize += 7;
                    
                } else if ((kint+1)*sizeof(int) < 9 ) {
                    
                    cbf_failnez(cbf_put_bits(file,&bflag,24))
                    
                    cbf_failnez(cbf_put_bits(file,&bbflag,32))
                    
                    for (iint = 0; iint < kint+1; iint++) {
                        
                        cbf_failnez (cbf_put_integer (file, delta[iint],
                                                      iint==numints-1?1:0,
                                                      iint<(numints-1)?(CHAR_BIT*sizeof (int)):
                                                      bits-(CHAR_BIT*sizeof (int))*iint ))
                    }
                    
                    csize += 15;
                } else return CBF_ARGUMENT;
                
            }
            
        }
        
        
        /* Return the number of characters written */
        
        if (compressedsize)
            
            *compressedsize = csize;
        
        
        /* Success */
        
        return 0;
    }


  /* Decompress an array with the byte-offset algorithm */

static int cbf_decompress_byte_offset_slow (void         *destination,
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
        
        int errorcode, overflow, numints, iint, carry;
        
        int delta[4];
        
        char * border;
        
        char * rformat;
        
        size_t numread;
        
        CBF_UNUSED(compressedsize);
        
        CBF_UNUSED(compression);
        
        CBF_UNUSED(byteorder);
        
        CBF_UNUSED(dimover);
        
        CBF_UNUSED(dimfast);
        
        CBF_UNUSED(dimmid);
        
        CBF_UNUSED(dimslow);
        
        CBF_UNUSED(padding);

        CBF_UNUSED(carry);

        CBF_UNUSED(errorcode);
        
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
        
        while (numread < nelem)
        {
            
            for (iint=0; iint < numints; iint++){
                
                element[iint] = prevelement[iint];
                
                delta[iint] = 0;
                
            }
            
            carry = 0;
            
            cbf_failnez(cbf_get_bits(file,delta,8))
            
            if ((delta[0]&0xFF) == 0x80) {
                
                
                cbf_failnez(cbf_get_bits(file,delta,16))
                
                
                if ( (delta[0]& 0xFFFF) == 0x8000)  {
                    
                    cbf_failnez(cbf_get_bits(file,delta,32))
                    
                    if ( (sizeof(int)==2 && delta[0] == 0 && delta[1] == 0x8000)
                        || (sizeof(int)> 3 && (delta[0]&0xFFFFFFFF)==0x80000000) )  {
                        
                        cbf_failnez(cbf_get_bits(file,delta,64))
                        
                    } else {
                        
                        if (sizeof(int) == 2) {
                            
                            if (delta[1] & 0x8000)  {
                                
                                for (iint = 2; iint < numints; iint++) delta[iint] = ~0;
                                
                            }
                            
                        } else  {
                            
                            if (delta[0] & 0x80000000) {
                                
                                delta[0] |= ~0xFFFFFFFF;
                                
                                for (iint = 1; iint < numints; iint++) {
                                    
                                    delta[iint] = ~0;
                                    
                                }
                                
                            }
                            
                        }
                        
                    }
                    
                    
                }  else  {
                    
                    if (delta[0] & 0x8000) {
                        
                        delta[0] |= ~0xFFFF;
                        
                        for (iint = 1; iint < numints; iint++) {
                            
                            delta[iint] = ~0;
                            
                        }
                    }
                    
                }
                
                
            } else {
                
                if (delta[0]&0x80)
                {
                    delta[0] |= ~0xFF;
                    
                    for (iint = 1; iint < numints; iint++) {
                        
                        delta[iint] = ~0;
                        
                    }
                }
                
            }
            
            
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
            
#if DEBUGPRINT == 1
            fprintf(stderr, "i: %d, 1", numread);
            fprintf(stderr, " = %d", element[0]);
            for (iint = 1; iint < numints; iint++)
                fprintf(stderr, ", %d", element[iint]);
            fprintf(stderr, "\n");
#endif
            
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


/*
 * this fast version assumes chars are 8 bits
 * and signed integers are represented in two's complement format
 */
static int cbf_decompress_byte_offset_fast(void         *destination,
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
    unsigned char *unsigned_char_data;
    
    char * border;
    
    char * rformat;
    
    size_t numread;
    
    CBF_sll_type delta;
    
    int i = 0;
    
    unsigned char *rawdata = NULL;
    
    CBF_UNUSED(nelem);
    
    CBF_UNUSED(compression);
    
    CBF_UNUSED(data_sign);
    
    CBF_UNUSED(byteorder);
    
    CBF_UNUSED(dimover);
    
    CBF_UNUSED(dimfast);
    
    CBF_UNUSED(dimmid);
    
    CBF_UNUSED(dimslow);
    
    CBF_UNUSED(padding);
    
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != 2* sizeof (int) &&
        elsize != 4* sizeof (int) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char)) {
        
        return CBF_ARGUMENT;
    }
    
    if (elsize != 1 && elsize != 2 && elsize != 4 && elsize !=8 ) {
        
        return CBF_ARGUMENT;
    }
    
    /* check for compatible real format */
    
    if ( realarray ) {
        
        cbf_failnez (cbf_get_local_real_format(&rformat) )
        
        if ( strncmp(rformat,"ieee",4) ) return CBF_ARGUMENT;
        
    }
    
    /* Check the stored element size */
    
    if (data_bits < 1 || data_bits > 64)
        
        return CBF_ARGUMENT;
    
    
    /* Initialise the pointer */
    
    unsigned_char_data = (unsigned char *) destination;
    
    
    /* Get the local byte order */
    
    if (realarray) {
        
        cbf_get_local_real_byte_order(&border);
        
    } else {
        
        cbf_get_local_integer_byte_order(&border);
        
    }
    
    if (file->characters_used < compressedsize) {
        
        if (file->temporary) return CBF_FILEREAD;
        
        cbf_failnez(cbf_set_io_buffersize(file,
            compressedsize-(file->characters_used)));
                    
        if (file->stream == NULL) {
            fprintf(stderr, "No file stream associated with handle\n");
            return CBF_NOTFOUND;
        }

        if (fread(file->characters_base+file->characters_used,
                  1,compressedsize-(file->characters_used),
                  file->stream)
            != compressedsize-(file->characters_used))
                    return CBF_FILEREAD;
                    
        file->characters_used = compressedsize;
        
    }
    
    rawdata = (unsigned char *)file->characters;
                        
    numread = 0;
    
    if (elsign) {
#ifdef CBF_USE_LONG_LONG
        
        long long base = 0;
        
        unsigned char *baseaddr;
        
        
        if (border[0] == 'b') {
            
            baseaddr = (unsigned char *) &base + sizeof(CBF_sll_type) - elsize;
            
        } else {
            
            baseaddr = (unsigned char *) &base;
            
        }
        
#else
        unsigned int sign, precarry;
        
        size_t el0, el1;
        
#if CBF_SLL_INTS == 2
        CBF_sll_type base = {0,0};
        
#else
        CBF_sll_type base = {0,0,0,0};
        
        size_t el2, el3, fl0, fl1;
        
#endif
        sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);
        
        if (border[0] == 'b') {
            
#if CBF_SLL_INTS > 2
            el0 = 3;
            
            el1 = 2;
            
            el2 = 1;
            
            el3 = 0;
            
            fl0 = 1;
            
            fl1 = 0;
            
#else
            el0 = 1;
            
            el1 = 0;
            
#endif
            
        } else {
            
            el0 = 0;
            
            el1 = 1;
            
#if CBF_SLL_INTS > 2
            el2 = 2;
            
            el3 = 3;
            
            fl0 = 0;
            
            fl1 = 1;
            
#endif
            
        }
        
        
#endif
        
#ifdef CBF_USE_LONG_LONG
        while (i < compressedsize) {
            int j;
            
            delta = (signed char) rawdata[i++];
            if (delta == (signed char) 0x80) {
                delta = rawdata[i++];
                delta |= (signed char) rawdata[i++] << 8;
                
                if (delta == (short) 0x8000) {
                    delta = rawdata[i++];
                    delta |= rawdata[i++] << 8;
                    delta |= rawdata[i++] << 16;
                    delta |= (signed char) rawdata[i++] << 24;
                    
                    if ((long) (delta & 0xffffffff) == (long) 0x80000000) {
                        delta = rawdata[i++];
                        delta |= rawdata[i++] << 8;
                        delta |= rawdata[i++] << 16;
                        delta |= (unsigned long long) rawdata[i++] << 24;
                        delta |= (unsigned long long) rawdata[i++] << 32;
                        delta |= (unsigned long long) rawdata[i++] << 40;
                        delta |= (unsigned long long) rawdata[i++] << 48;
                        delta |= (signed long long) rawdata[i++] << 56;
                    }
                }
            }
            
            base += delta;
            
            for (j = 0; j < elsize; j++)
                *unsigned_char_data++ = baseaddr[j];
            numread++;
        }
#else
#if CBF_SLL_INTS==2
        while (i < (int)compressedsize) {
            delta.el1 = 0;
            delta.el0 = (signed char) rawdata[i++];
            if ((int)delta.el0 == (signed char) 0x80) {
                delta.el0 = rawdata[i++];
                delta.el0 |= (signed char) rawdata[i++] << 8;
                if (delta.el0 & 0x8000L) {
                    delta.el0 |= ~0x7FFFL;
                    delta.el1 = (unsigned int)~0L;
                }
                
                if ((delta.el0 & 0xffffL) ==  0x8000) {
                    delta.el1 = 0;
                    delta.el0 = rawdata[i++];
                    delta.el0 |= rawdata[i++] << 8;
                    delta.el0 |= rawdata[i++] << 16;
                    delta.el0 |= (signed char) rawdata[i++] << 24;
                    if (delta.el0 & 0x80000000L) {
                        delta.el0 |= ~0x7FFFFFFFL;
                        delta.el1 = (unsigned int)~0L;
                    }
                    
                    if ((delta.el0 & 0xffffffffL) == 0x80000000L) {
                        delta.el0 = 0;
                        delta.el0 = rawdata[i++];
                        delta.el0 |= rawdata[i++] << 8;
                        delta.el0 |= rawdata[i++] << 16;
                        delta.el0 |= rawdata[i++] << 24;
                        delta.el1 = rawdata[i++];
                        delta.el1 |= rawdata[i++] << 8;
                        delta.el1 |= rawdata[i++] << 16;
                        delta.el1 |= (signed char) rawdata[i++] << 24;
                    }
                }
            }
            
            precarry = 0;
            
            if (base.el0 & sign) precarry++;
            
            if (delta.el0 & sign) precarry++;
            
            base.el0 += delta.el0;
            
            if (precarry == 2 || (precarry == 1 && !(base.el0&sign) ) ) base.el1++;
            
            base.el1+= delta.el1;
            
            
            switch (elsize) {
                    
                case (sizeof(unsigned int) *2):
                    
                    ((unsigned int *)unsigned_char_data)[el0] = base.el0;
                    
                    ((unsigned int *)unsigned_char_data)[el1] = base.el1;

                    break;
                    
                case (sizeof(unsigned int) ):
                    
                    ((unsigned int *)unsigned_char_data)[0] = base.el0;
                    
                    break;
                    
                case (sizeof(unsigned short) ):
                    
                    ((unsigned short *)unsigned_char_data)[0] = (unsigned short)base.el0;
                    
                    break;
                    
                case (sizeof(unsigned char) ):
                    
                    ((unsigned char *)unsigned_char_data)[0] = (unsigned char)base.el0;

                    break;
                    
            }
                                
            
            unsigned_char_data+= elsize;
            
            numread++;
        }
#else
        while (i < compressedsize) {
            delta.el1 = delta.el2 = delta.el3 = 0;
            delta.el0 = (signed char) rawdata[i++];
            if (delta.el0 == (signed char) 0x80) {
                delta.el0 = rawdata[i++];
                delta.el0 |= (signed char) rawdata[i++] << 8;
                if (delta.el0 & 0x8000) delta.el1 = delta.el2 = delta.el3 = ~0;
                
                if ((delta.el0 & 0xffff) == 0x8000) {
                    delta.el2 = delta.el3 = 0;
                    delta.el0 = rawdata[i++];
                    delta.el0 |= rawdata[i++] << 8;
                    delta.el1 = rawdata[i++];
                    delta.el1 |= (signed char) rawdata[i++] << 8;
                    if (delta.el1 & 0x8000) delta.el2 = delta.el3 = ~0;
                    
                    if (delta.el0 == 0 && (delta.el1 & 0x8000) == 0x8000) {
                        delta.el0 = rawdata[i++];
                        delta.el0 |= rawdata[i++] << 8;
                        delta.el1 = rawdata[i++];
                        delta.el1 |= rawdata[i++] << 8;
                        delta.el2 = rawdata[i++];
                        delta.el2 |= rawdata[i++] << 8;
                        delta.el3 = rawdata[i++];
                        delta.el3 |= (signed char)rawdata[i++] << 8;
                    }
                }
            }
            
            precarry = 0;
            
            if (base.el0 & sign) precarry++;
            
            if (delta.el0 & sign) precarry++;
            
            base.el0 += delta.el0;
            
            if (precarry == 2 || (precarry == 1 && !(base.el0&sign) ) ) base.el1++;
            
            precarry = 0;
            
            if (base.el1 & sign) precarry++;
            
            if (delta.el1 & sign) precarry++;
            
            base.el1+= delta.el1;
            
            if (precarry == 2 || (precarry == 1 && !(base.el1&sign) ) ) base.el2++;
            
            precarry = 0;
            
            if (base.el2 & sign) precarry++;
            
            if (delta.el2 & sign) precarry++;
            
            base.el2+= delta.el2;
            
            if (precarry == 2 || (precarry == 1 && !(base.el1&sign) ) ) base.el3++;
            
            base.el1+= delta.el1;
            
            switch (elsize) {
                    
                case (sizeof(unsigned int) *4):
                    
                    ((unsigned int *)unsigned_char_data)[el0] = base.el0;
                    
                    ((unsigned int *)unsigned_char_data)[el1] = base.el1;
                    
                    ((unsigned int *)unsigned_char_data)[el2] = base.el2;
                    
                    ((unsigned int *)unsigned_char_data)[el3] = base.el3;

                    break;
                    
                case (sizeof(unsigned int) *2 ):
                    
                    ((unsigned int *)unsigned_char_data)[fl0] = base.el0;
                    
                    ((unsigned int *)unsigned_char_data)[fl1] = base.el1;
                    
                    break;
                                        
                case (sizeof(unsigned char) ):
                    
                    ((unsigned char *)unsigned_char_data)[0] = base.el0;
                    
                    break;
                    
            }
                        
            unsigned_char_data+= elsize;
            
            numread++;
            
        }
        
#endif
#endif
    } else {
#ifdef CBF_USE_LONG_LONG
        unsigned long long base = 0;
        
        unsigned long long basemask = 0;
        
        unsigned char *baseaddr;
        
        int j;
        
        for (j = 0; j < elsize*8; j++) {
            basemask <<= 1;
            basemask |= 1;
        }
        
        
        if (border[0] == 'b') {
            
            baseaddr = (unsigned char *) &base + sizeof(CBF_ull_type) - elsize;
            
        } else {
            
            baseaddr = (unsigned char *) &base;
            
        }
        
#else
        unsigned int sign, precarry;
        
        size_t el0, el1;
        
        
#if CBF_ULL_INTS == 2
        CBF_ull_type base = {0,0};
        
#else
        CBF_ull_type base = {0,0,0,0};
        
        size_t el2, el3, fl0, fl1;
        
#endif
        sign = 1 << (sizeof(unsigned int)*CHAR_BIT-1);
        
        if (border[0] == 'b') {
            
#if CBF_ULL_INTS > 2
            el0 = 3;
            
            el1 = 2;
            
            el2 = 1;
            
            el3 = 0;
            
            fl0 = 1;
            
            fl1 = 2;
#else
            el0 = 1;
            
            el1 = 0;
#endif
            
        } else {
            
            el0 = 0;
            
            el1 = 1;
            
#if CBF_SLL_INTS > 2
            el2 = 2;
            
            el3 = 3;
            
            fl0 = 0;
            
            fl1 = 1;
            
#endif
            
        }
        
        
#endif
        
#ifdef CBF_USE_LONG_LONG
        while (i < compressedsize) {
            delta = (signed char) rawdata[i++];
            if (delta == (signed char) 0x80) {
                delta = rawdata[i++];
                delta |= (signed char) rawdata[i++] << 8;
                
                if (delta == (short) 0x8000) {
                    delta = rawdata[i++];
                    delta |= rawdata[i++] << 8;
                    delta |= rawdata[i++] << 16;
                    delta |= (signed char) rawdata[i++] << 24;
                    
                    if ((long) (delta & 0xffffffff) == (long) 0x80000000) {
                        delta = rawdata[i++];
                        delta |= rawdata[i++] << 8;
                        delta |= rawdata[i++] << 16;
                        delta |= (unsigned long long) rawdata[i++] << 24;
                        delta |= (unsigned long long) rawdata[i++] << 32;
                        delta |= (unsigned long long) rawdata[i++] << 40;
                        delta |= (unsigned long long) rawdata[i++] << 48;
                        delta |= (signed long long) rawdata[i++] << 56;
                    }
                }
            }
            
            base += delta;
            base &= basemask;
            
            for (j = 0; j < elsize; j++)
                *unsigned_char_data++ = baseaddr[j];
            numread++;
        }
#else
#if CBF_SLL_INTS==2
        while (i < (int)compressedsize) {
            delta.el1 = 0;
            delta.el0 = (signed char) rawdata[i++];
            if ((int)delta.el0 == (signed char) 0x80) {
                delta.el0 = rawdata[i++];
                delta.el0 |= (signed char) rawdata[i++] << 8;
                if (delta.el0 & 0x8000L) {
                    delta.el0 |= ~0x7FFFL;
                    delta.el1 = (unsigned int)~0L;
                }
                
                if ((delta.el0 & 0xffff) ==  0x8000) {
                    delta.el1 = 0;
                    delta.el0 = rawdata[i++];
                    delta.el0 |= rawdata[i++] << 8;
                    delta.el0 |= rawdata[i++] << 16;
                    delta.el0 |= (signed char) rawdata[i++] << 24;
                    if (delta.el0 & 0x80000000L) delta.el1 = ~0;
                    
                    if ((delta.el0 & 0xffffffff) == 0x80000000) {
                        delta.el0 = rawdata[i++];
                        delta.el0 |= rawdata[i++] << 8;
                        delta.el0 |= rawdata[i++] << 16;
                        delta.el0 |= rawdata[i++] << 24;
                        delta.el1 = rawdata[i++];
                        delta.el1 |= rawdata[i++] << 8;
                        delta.el1 |= rawdata[i++] << 16;
                        delta.el1 |= (signed char) rawdata[i++] << 24;
                    }
                }
            }
            
            precarry = 0;
            
            if (base.el0 & sign) precarry++;
            
            if (delta.el0 & sign) precarry++;
            
            base.el0 += delta.el0;
            
            if (precarry == 2 || (precarry == 1 && !(base.el0&sign) ) ) base.el1++;
            
            base.el1+= delta.el1;
            
            base.el1 &= 0xffffffffL;
            
            switch (elsize) {
                    
                case (sizeof(unsigned int) *2):
                    
                    ((unsigned int *)unsigned_char_data)[el0] = base.el0;
                    
                    ((int *)unsigned_char_data)[el1] = (int)base.el1;
                    
                    break;
                    
                case (sizeof(int) ):
                    
                    ((int *)unsigned_char_data)[0] = (int)base.el0;
                    
                    break;
                    
                case (sizeof(short) ):
                    
                    ((short *)unsigned_char_data)[0] = (short)base.el0;
                    
                    break;
                    
                case (sizeof(char) ):
                    
                    ((char *)unsigned_char_data)[0] = (char)base.el0;
                    
                    break;
                    
            }
            
            
            unsigned_char_data+= elsize;
            
            numread++;
        }
#else
        while (i < compressedsize) {
            delta.el1 = delta.el2 = delta.el3 = 0;
            delta.el0 = (signed char) rawdata[i++];
            if (delta.el0 == (signed char) 0x80) {
                delta.el0 = rawdata[i++];
                delta.el0 |= (signed char) rawdata[i++] << 8;
                if (delta.el0 & 0x8000) delta.el1 = delta.el2 = delta.el3 = ~0;
                
                if ((delta.el0&0xffff) == 0x8000) {
                    delta.el2 = delta.el3 = 0;
                    delta.el0 = rawdata[i++];
                    delta.el0 |= rawdata[i++] << 8;
                    delta.el1 = rawdata[i++];
                    delta.el1 |= (signed char) rawdata[i++] << 8;
                    if (delta.el1 & 0x8000) delta.el2 = delta.el3 = ~0;
                    
                    if (delta.el0 == 0 && (delta.el1&0x8000) == 0x8000) {
                        delta.el0 = rawdata[i++];
                        delta.el0 |= rawdata[i++] << 8;
                        delta.el1 = rawdata[i++];
                        delta.el1 |= rawdata[i++] << 8;
                        delta.el2 = rawdata[i++];
                        delta.el2 |= rawdata[i++] << 8;
                        delta.el3 = rawdata[i++];
                        delta.el3 |= (signed char)rawdata[i++] << 8;
                    }
                }
            }
            
            precarry = 0;
            
            if (base.el0 & sign) precarry++;
            
            if (delta.el0 & sign) precarry++;
            
            base.el0 += delta.el0;
            
            if (precarry == 2 || (precarry == 1 && !(base.el0&sign) ) ) base.el1++;
            
            precarry = 0;
            
            if (base.el1 & sign) precarry++;
            
            if (delta.el1 & sign) precarry++;
            
            base.el1+= delta.el1;
            
            if (precarry == 2 || (precarry == 1 && !(base.el1&sign) ) ) base.el2++;
            
            precarry = 0;
            
            if (base.el2 & sign) precarry++;
            
            if (delta.el2 & sign) precarry++;
            
            base.el2+= delta.el2;
            
            if (precarry == 2 || (precarry == 1 && !(base.el1&sign) ) ) base.el3++;
            
            base.el3+= delta.el3;         
            
            base.el3 &= 0xffff;

            switch (elsize) {
                    
                case (sizeof(unsigned int) *4):
                    
                    ((unsigned int *)unsigned_char_data)[el0] = base.el0;
                    
                    ((unsigned int *)unsigned_char_data)[el1] = base.el1;
                    
                    ((unsigned int *)unsigned_char_data)[el2] = base.el2;
                    
                    ((int *)unsigned_char_data)[el3] = base.el3;
                    
                    break;
                    
                case (sizeof(unsigned int) *2 ):
                    
                    ((unsigned int *)unsigned_char_data)[fl0] = base.el0;
                    
                    ((int *)unsigned_char_data)[fl1] = base.el1;
                    
                    break;
                    
                case (sizeof( char) ):
                    
                    (( char *)unsigned_char_data)[0] = (char)base.el0;
                    
                    break:
                    
            }
            
            unsigned_char_data+= elsize;
            
            numread++;
            
        }
        
#endif
#endif      
    }
    
    /* Number read */
    
    if (nelem_read)
        
        *nelem_read = numread;
    
    
    /* Success */
    
    return 0;
}

int cbf_decompress_byte_offset(void         *destination,
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
  /* test for bits left in buffer, element size, chars are 8-bit, and signed
     integers are represented in two's complement */
  if (file->bits[0] != 0 || elsize > sizeof(CBF_ull_type) || CHAR_BIT != 8 ||
      ~0 != -1 || (elsize != 1 && elsize != 2 && elsize != 4 && elsize !=8 ) ) {
    return cbf_decompress_byte_offset_slow(destination, elsize, elsign,
        nelem, nelem_read, compressedsize, compression, data_bits, data_sign,
        file, realarray, byteorder, dimover, dimfast, dimmid, dimslow, padding);
  }

  return cbf_decompress_byte_offset_fast(destination, elsize, elsign,
      nelem, nelem_read, compressedsize, compression, data_bits, data_sign,
      file, realarray, byteorder, dimover, dimfast, dimmid, dimslow, padding);
}

#ifdef __cplusplus

}

#endif
