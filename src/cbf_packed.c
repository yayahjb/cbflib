/**********************************************************************
 * cbf_packed -- Packing compression                                  *
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

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_packed.h"

#define CBF_SHIFT63 (sizeof (int) * CHAR_BIT > 64 ? 63 : 0)

typedef struct
{
  unsigned int offset [128][4];

  unsigned int size [128];

  unsigned int start;

  unsigned int offsets;
}
cbf_packed_data;

#define CBF_PACKED_BITS1  4
#define CBF_PACKED_BITS2  5
#define CBF_PACKED_BITS3  6
#define CBF_PACKED_BITS4  7
#define CBF_PACKED_BITS5  8
#define CBF_PACKED_BITS6  16

#define CBF_PACKED_V2_BITS1  3
#define CBF_PACKED_V2_BITS2  4
#define CBF_PACKED_V2_BITS3  5
#define CBF_PACKED_V2_BITS4  6
#define CBF_PACKED_V2_BITS5  7
#define CBF_PACKED_V2_BITS6  8
#define CBF_PACKED_V2_BITS7  9
#define CBF_PACKED_V2_BITS8  10
#define CBF_PACKED_V2_BITS9  11
#define CBF_PACKED_V2_BITS10 12
#define CBF_PACKED_V2_BITS11 13
#define CBF_PACKED_V2_BITS12 14
#define CBF_PACKED_V2_BITS13 15
#define CBF_PACKED_V2_BITS14 16

#define CBF_PACKED_MASK1  ~15
#define CBF_PACKED_MASK2  ~31
#define CBF_PACKED_MASK3  ~63
#define CBF_PACKED_MASK4  ~127
#define CBF_PACKED_MASK5  ~255
#define CBF_PACKED_MASK6  ~65535

#define CBF_PACKED_V2_MASK1  ~7
#define CBF_PACKED_V2_MASK2  ~15
#define CBF_PACKED_V2_MASK3  ~31
#define CBF_PACKED_V2_MASK4  ~63
#define CBF_PACKED_V2_MASK5  ~127
#define CBF_PACKED_V2_MASK6  ~255
#define CBF_PACKED_V2_MASK7  ~511
#define CBF_PACKED_V2_MASK8  ~1023
#define CBF_PACKED_V2_MASK9  ~2047
#define CBF_PACKED_V2_MASK10 ~4095L
#define CBF_PACKED_V2_MASK11  ~8191
#define CBF_PACKED_V2_MASK12  ~16383
#define CBF_PACKED_V2_MASK13  ~32767
#define CBF_PACKED_V2_MASK14  ~65535


static const unsigned int cbf_packed_bits [8] = { 0, CBF_PACKED_BITS1,
                                                     CBF_PACKED_BITS2,
                                                     CBF_PACKED_BITS3,
                                                     CBF_PACKED_BITS4,
                                                     CBF_PACKED_BITS5,
                                                     CBF_PACKED_BITS6, 65 };

static const unsigned int cbf_packedv2_bits [16] = { 0, CBF_PACKED_V2_BITS1,
                                                     CBF_PACKED_V2_BITS2,
                                                     CBF_PACKED_V2_BITS3,
                                                     CBF_PACKED_V2_BITS4,
                                                     CBF_PACKED_V2_BITS5,
                                                     CBF_PACKED_V2_BITS6,
                                                     CBF_PACKED_V2_BITS7,
                                                     CBF_PACKED_V2_BITS8,
                                                     CBF_PACKED_V2_BITS9,
                                                     CBF_PACKED_V2_BITS10,
                                                     CBF_PACKED_V2_BITS11,
                                                     CBF_PACKED_V2_BITS12,
                                                     CBF_PACKED_V2_BITS13,
                                                     CBF_PACKED_V2_BITS14, 65 };


  /* Add an integer to the array of offsets to write (version 2) */

int cbf_add_offsetv2 (cbf_packed_data *data, unsigned int *element,
                                             unsigned int *last_element,
                                                      int  numints)
{
  unsigned int offset[4];

  int i, issmall;

  unsigned int index, m;


    /* Save the offset */

  index = (data->offsets + data->start) & 127;

  if (numints > 1)  {
  
    for (i = 0; i < numints; i++) offset[i] = last_element[i];
    
    cbf_failnez(cbf_mpint_negate_acc(offset,numints))
    
    cbf_failnez(cbf_mpint_add_acc(offset,numints,element,numints))
  	
  } else{
  	
    offset[0] = element[0] - last_element[0];
  
  }
  
  for (i = 0; i < numints; i++) data->offset [index][i] = offset[i];


    /* How many bits do we need to save? */

  issmall = 1;
  
  for (i = 1; i < numints; i++) {
  
    if (((int)offset[0]>=0 && (int)offset[i] != 0)
       || ((int)offset[0]<0 && 1+(int)offset[i] !=0 )) issmall = 0;
  	
  }
    
  if (!issmall)  {
  
    data->size [index] = 15;
  	
  } else  {

  if (offset[0] == 0)

    data->size [index] = 0;

  else

    if ((element[0] < last_element[0] && (int)offset[0] > 0) ||
        (element[0] > last_element[0] && (int)offset[0] < 0))
    {
    	
      for (i = 1; i < 4; i++) data->offset[index][i] = (int)offset[0]<0?0:~0;

      data->size [index] = 15;
    }
    else
    {
      m = (offset[0] ^ (offset[0] << 1));

      if ((m & CBF_PACKED_V2_MASK1) == 0)

        data->size [index] = 1;

      else

        if ((m & CBF_PACKED_V2_MASK2) == 0)

          data->size [index] = 2;

        else

          if ((m & CBF_PACKED_V2_MASK3) == 0)

            data->size [index] = 3;

          else

            if ((m & CBF_PACKED_V2_MASK4) == 0)

              data->size [index] = 4;

            else

              if ((m & CBF_PACKED_V2_MASK5) == 0)

                data->size [index] = 5;

              else

                if ((m & CBF_PACKED_V2_MASK6) == 0)

                  data->size [index] = 6;

                else

                  if ((m & CBF_PACKED_V2_MASK7) == 0)

                    data->size [index] = 7;

                  else

                    if ((m & CBF_PACKED_V2_MASK8) == 0)

                      data->size [index] = 8;

                    else

                      if ((m & CBF_PACKED_V2_MASK9) == 0)

                        data->size [index] = 9;

                      else

                        if ((m & CBF_PACKED_V2_MASK10) == 0)

                          data->size [index] = 10;

                        else

                          if ((m & CBF_PACKED_V2_MASK11) == 0)

                            data->size [index] = 11;

                          else

                            if ((m & CBF_PACKED_V2_MASK12) == 0)

                              data->size [index] = 12;

                            else

                              if ((m & CBF_PACKED_V2_MASK13) == 0)

                                data->size [index] = 13;

                              else

                                if ((m & CBF_PACKED_V2_MASK14) == 0)

                                  data->size [index] = 14;

                                else

                                  data->size [index] = 15;
    }
    
    }


    /* Success */

  data->offsets++;

  return 0;
}



  /* Add an integer to the array of offsets to write (version 1) */

int cbf_add_offset (cbf_packed_data *data, unsigned int *element,
                                           unsigned int *last_element, 
                                                    int  numints)
{
  unsigned int offset[4];
  
  int i, issmall;

  unsigned int index, m;


    /* Save the offset */

  index = (data->offsets + data->start) & 127;

  if (numints > 1)  {
  
    for (i = 0; i < numints; i++) offset[i] = last_element[i];
    
    cbf_failnez(cbf_mpint_negate_acc(offset,numints))
    
    cbf_failnez(cbf_mpint_add_acc(offset,numints,element,numints))
  	
  } else{
  	
    offset[0] = element[0] - last_element[0];
  
  }
  
  for (i = 0; i < numints; i++) data->offset [index][i] = offset[i];

    /* How many bits do we need to save? */
    
  issmall = 1;
  
  for (i = 1; i < numints; i++) {
  
    if (((int)offset[0]>=0 && offset[i] != 0)
       || ((int)offset[0]<0 && 1+(int)offset[i] !=0 )) issmall = 0;
  	
  }
    
  if (!issmall)  {
  
    data->size [index] = 7;
      	
  } else  {

  if (offset[0] == 0)

    data->size [index] = 0;

  else
  
    if ((element[0] < last_element[0] && (int)offset[0] > 0) ||
        (element[0] > last_element[0] && (int)offset[0] < 0))
    {
    
      for (i = 1; i < 4; i++) data->offset[index][i] =  (int)offset[0]<0?0:~0;

      data->size [index] = 7;

    } else {
    
      m = (offset[0] ^ (offset[0] << 1));

      if ((m & CBF_PACKED_MASK1) == 0)

        data->size [index] = 1;

      else

        if ((m & CBF_PACKED_MASK2) == 0)

          data->size [index] = 2;

        else

          if ((m & CBF_PACKED_MASK3) == 0)

            data->size [index] = 3;

          else

            if ((m & CBF_PACKED_MASK4) == 0)

              data->size [index] = 4;

            else

              if ((m & CBF_PACKED_MASK5) == 0)

                data->size [index] = 5;

             else

               if ((m & CBF_PACKED_MASK6) == 0)

                 data->size [index] = 6;

               else

                 data->size [index] = 7;
    }
    
  }


    /* Success */

  data->offsets++;

  return 0;
}


  /* Pack 1 << chunk offsets in [size] bits each 
     The flag v2flag selects version 1 (v2flag = 0)
     or version 2 (v2flag = 1)  */

int cbf_pack_chunk (cbf_packed_data *data, int size, int chunk,
                                           cbf_file *file,
                                           unsigned long *bitcount,
                                           int v2flag, int clipbits)
{
  unsigned int count, index, pbits;
  
  int zero[4] = { 0, 0, 0, 0};
  
    /* Write the codes */
    
  cbf_failnez (cbf_put_integer (file, (size << 3) | chunk, 0, 6+v2flag))

  chunk = 1 << chunk;
  
  pbits = v2flag?cbf_packedv2_bits[size]:cbf_packed_bits[size];
  
  if (clipbits && pbits==65) pbits = clipbits;

  if (size > 0) {

    index = data->start;
    
    if (pbits == 65) {

      for (count = chunk; count; count--, index++) {

        cbf_failnez (cbf_put_bits (file, (int *)data->offset [index & 127],
                                       sizeof(int)*CHAR_BIT))
    	
        cbf_failnez (cbf_put_bits (file, zero, pbits-sizeof(int)*CHAR_BIT))
      }

    } else {

    for (count = chunk; count; count--, index++)

      cbf_failnez (cbf_put_bits (file, (int *)data->offset [index & 127],
                                       pbits))
    }

  }


    /* Update the buffer count and start */

  data->start = (data->start + chunk) & 127;

  data->offsets -= chunk;


    /* Calculate the number of bits written */

  if (bitcount) {

    if (size)

      *bitcount = 6 + v2flag + chunk * pbits;

    else

      *bitcount = 6 + v2flag;

  }

    /* Success */

  return 0;
}


  /* Get the maximum size required to code 1 << chunk offsets */

unsigned int cbf_maximum_size (cbf_packed_data *data, unsigned int start,
                                                      unsigned int chunk)
{
  unsigned int maxsize, index, count;


    /* Get the maximum size */

  maxsize = 0;

  index = data->start + start;

  for (count = 1 << chunk; count; count--)
  {
    if (data->size [index & 127] > maxsize)

      maxsize = data->size [index & 127];

    index++;
  }

  return maxsize;
}


  /* Write out a block as economically as possible 
     The flag v2flag selects version 1 (v2flag = 0)
     or version 2 (v2flag = 1)  */

int cbf_pack_nextchunk (cbf_packed_data *data, cbf_file *file,
                                               unsigned long *bitcount,
                                               int v2flag, int clipbits)
{
  unsigned int bits, pbits, next_bits, chunk, size, next_size,
               combined_bits, combined_size;


    /* Number of bits to encode a single offset */

  size = cbf_maximum_size (data, 0, 0);

  pbits = v2flag?cbf_packedv2_bits[size]:cbf_packed_bits[size];

  bits = pbits + 6 + v2flag;


  chunk = 0;

  while (data->offsets >= (2 << chunk))
  {
    next_size = cbf_maximum_size (data, 1 << chunk, chunk);
    
    pbits = v2flag?cbf_packedv2_bits[next_size]:cbf_packed_bits[next_size];

    next_bits = (pbits << chunk) + 6 + v2flag;

    if (size > next_size)
    {
      combined_bits = bits * 2 - 6 - v2flag;

      combined_size = size;
    }
    else
    {
      combined_bits = next_bits * 2 - 6 - v2flag;

      combined_size = next_size;
    }

    if (combined_bits > bits + next_bits)

      return cbf_pack_chunk (data, size, chunk, file, bitcount, v2flag, clipbits);

    bits = combined_bits;

    size = combined_size;

    chunk++;
  }

  return cbf_pack_chunk (data, size, chunk, file, bitcount, v2flag, clipbits);
}


/*  Update pointers for averaging in J. P. Abrahams CCP4 compression
    algorithm.
    
    On entry, trail_char_data[0] should point to the data element
    immediately prior to the next data element to be processed, either
    in the same row (fastest index) or, at the end of the prior row
    if the next data element to be processed is at the end of a row
    
    ndimfast, ndimmid, ndimslow should point to the indices of the same
    data element as trail_char_data[0] points to.  These values
    will be incremented to be the indices of the next data element
    to be processed before populating trail_char_data.
    
    On exit, trail_char_data[0..7] will have been populated with
    pointers to the data elements to be used in forming the average.
    Elements that will not be used will be set to NULL.   Note
    that trail_char_data[0] may be set to NULL.             
    
    If we mark the next element to be processed with a "*" and the
    entries in trail_char_data with their array indices 0 .. 7, the
    possible patterns of settings in the general case are:
    
    current section:
    
         - - - - 0 * - - - -
         - - - - 3 2 1 - - - 
         - - - - - - - - - -
         
    prior section:
    
         - - - - - 4 - - - -
         - - - - 7 6 5 - - - 
         - - - - - - - - - -
             
    If there is no prior section (i.e. ndimslow is 0, or 
    the CBF_UNCORRELATED_SECTIONS flag is set
    to indicate discontinuous sections), the values
    for trail_char_data[4..7] will all be NULL.  When
    there is a prior section, trail_char_data[5..7] are
    pointers to the elements immediately below the
    elements pointed to by trail_char_data[1..3], but
    trail_char_data[4] is one element further along
    its row to be directly below the next element to
    be processed.
    
    The first element of the first row of the first section
    is a special case, with no averaging.  This function
    should not be called for that case.

    In the first row of the first section (ndimmid == 0,
    and ndimslow == 0), after the first element (ndimfast > 0), 
    only trail_char_data[0] is used
    
    current section:
    
         - - - - 0 * - - - -

    For subsequent rows of the first section (ndimmid > 0,
    and ndimslow == 0), for the first element (ndimfast == 0), 
    two elements from the prior row are used:
    
    current section:
    
         * - - - - - - - - -
         2 1 - - - - - - - -
         - - - - - - - - - -

    while for element after the first element, but before
    the last element of the row, a full set of 4 elements 
    is used:
    
    current section:
    
         - - - - 0 * - - - -
         - - - - 3 2 1 - - - 
         - - - - - - - - - -
         
    For the last element of a row (ndimfast == dimfast-1), two
    elements are used
    
    current section:
    
         - - - - - - - - 0 *
         - - - - - - - - - 2 
         - - - - - - - - - -
         
    For sections after the first section, provided the
    CBF_UNCORRELATED_SECTIONS flag is not set in compression,
    for each non-NULL entry in trail_char_data [0..3] an entry 
    is made in trail_char_data [4..7], except for the
    first element of the first row of a section.  In that
    case an entry is made in trail_char_data[4].
    
    
    
      */

int cbf_update_jpa_pointers(unsigned char * trail_char_data[8],
                               size_t *ndimfast, size_t *ndimmid, size_t *ndimslow,
                               size_t   dimfast, size_t   dimmid, size_t   dimslow,
                               size_t   elsize, 
                         unsigned int  *average,
                         unsigned int   compression) {
                               
    int i, j, k;
    
    int log2[4] = {1,2,0,3};
    
    size_t numints;
    
    int mask, signbit;
    
    average[0] = 0;
    
    numints = (elsize + sizeof(unsigned int) -1)/sizeof(unsigned int);
    
    k = (elsize - (numints-1)*sizeof(unsigned int));

    if (k == sizeof(unsigned int)) {

      mask = ~0;

    } else {
 
      mask = ~(-(1<<(k*CHAR_BIT)));

    }    

    signbit = 1<<(CHAR_BIT*(elsize - (numints-1)*sizeof(unsigned int))-1);
    
    for (i = 1; i < numints; i++) average[i] = 0;
                               
    (*ndimfast)++;
    
    if (*ndimfast == dimfast) {
    
      *ndimfast = 0;
      
      (*ndimmid)++;
      
      if (*ndimmid == dimmid) {
      
        *ndimmid = 0;
        
        (*ndimslow)++;
      	
      }
    	
    }
    
    for (i = 1 ; i < 8; i++ ) trail_char_data[i] = NULL;
 
    if (*ndimmid > 0)  {  /* Not in the first row */
    
      trail_char_data[1] = trail_char_data[0]-elsize*(dimfast-2);   /* down 1 right 2 */
      
      trail_char_data[2] = trail_char_data[0]-elsize*(dimfast-1); /* down 1 right 1 */

      if (*ndimfast > 0 )  {  /* Not in the first column */
          
        trail_char_data[3] = trail_char_data[0]-elsize*(dimfast);   /* down 1 */
        
        if (*ndimfast == dimfast-1)  { /* Last column */
        
          trail_char_data[1] = NULL;
        	
          trail_char_data[3] = NULL;

        }
      
      } else { /* First column */
      
        trail_char_data[0] = NULL;       

        /* trail_char_data[3] = NULL; -- already done */
      	
      }

      if ( *ndimslow > 0  && (compression&CBF_UNCORRELATED_SECTIONS)== 0) {
      
        if (trail_char_data[0]) 

          trail_char_data[4] = 

            trail_char_data[0] - elsize*dimfast*dimmid + elsize;
      
        for (i = 1; i < 4; i++ ) {
        
          if (trail_char_data[i]) 

            trail_char_data[i+4] = 

              trail_char_data[i] - elsize*dimfast*dimmid;
        	
        }

      }

    } else { /* First row of a section */
    
      if ( *ndimfast == 0 ) { /* First element of first row of a section */
      
        trail_char_data[4] = 

              trail_char_data[0] - elsize*(dimfast*dimmid-1);
              
        trail_char_data[0] = NULL;   
         
      }
    	
    }
    
    
      
    j = 0;
      
      
    if (numints == 1) {
    	
      for (i = 0; i < 8; i++) {
      
        if (trail_char_data[i]) {
        
          j++;
        
            if (elsize == sizeof (int))

              average[0] += *((unsigned int *) (trail_char_data[i]) );

            else

              if (elsize == sizeof (short))

                average[0] += *((unsigned short *) (trail_char_data[i]));

              else

                average[0] += *(trail_char_data[i]);
        	
          }
      }
        
      k = j>> 1;
      
      if (average[0] & signbit) average[0] |= ~mask;
      
      else average[0] &= mask;

      if (k > 0) average[0] = (unsigned int) (((int)average[0] + k) >> log2[k-1]);
        
    } else {

     for (i = 0; i < 8; i++) {
     
       cbf_failnez(cbf_mpint_add_acc(average, numints, (unsigned int *)&(trail_char_data[i]), numints))
     
     }
     
       if (j > 1) {
       	
         k = j >> 1;
       
         cbf_failnez(cbf_mpint_add_acc(average,numints, (unsigned int *)&j , 1))

         if (average[numints-1] & signbit) average[numints-1] |= ~mask;
      
         else average[numints-1] &= mask;
         
         if (k > 0) {
         	
           cbf_failnez(cbf_mpint_rightshift_acc(average,numints,log2[k-1]))
           
         }
         
       }
    	
    }

    return 0;
	
}


  /* Compress an array with ccp4 compression as per J. P Abrahams.  
     If dimensions are given, packing will be done with averaging
     to determine the base for offsets. */

int cbf_compress_packed (void         *source,
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
  unsigned int minelement, maxelement;

  unsigned int count, element[4], lastelement[4], unsign, sign, limit, bits;

  unsigned char *unsigned_char_data;
  
  unsigned char *trail_char_data[8];

  unsigned long bitcount, chunkbits;
  
  unsigned int average[4];
  
  size_t ndimfast, ndimmid, ndimslow;

  cbf_packed_data *data;
  
  void * memblock;
  
  int v2flag, avgflag, clipbits;
  
  int numints;
  
  int i, iint;

  char * border;

  char * rformat;



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

  numints = (bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));


    /* Allocate memory */

  cbf_failnez (cbf_alloc (&memblock, NULL, sizeof (cbf_packed_data), 1))
  
  data = (cbf_packed_data *) memblock;

  data->start = 0;

  data->offsets = 0;


    /* Count the expected number of bits */

  minelement = 0;

  maxelement = 0;
  
   /* Set flags */
   
  v2flag = 0;
  
  if ((compression&CBF_COMPRESSION_MASK) == CBF_PACKED_V2) v2flag = 1;
  
  avgflag = 0;
    
  if (dimfast != 0 || dimmid != 0 || dimslow != 0) avgflag = 1;
  
  if (compression&CBF_FLAT_IMAGE) avgflag = 0;
  
  clipbits = 0;
  
  if (avgflag) clipbits = bits;
  
  if (dimslow == 0) dimslow = 1;
  
  if (dimmid == 0) dimmid = 1;
  
  if (dimfast == 0) dimfast = nelem/(dimmid*dimslow);
  
  if (dimfast * dimmid * dimslow != nelem) return CBF_ARGUMENT;


    /* Write the number of elements (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, nelem, 0, 64),
                 cbf_free ((void **) data, NULL))


    /* Write the minimum element (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, minelement, elsign, 64),
                 cbf_free ((void **) data, NULL))


    /* Write the maximum element (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, maxelement, elsign, 64),
                 cbf_free ((void **) data, NULL))


    /* Write the reserved word  (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, 0, 0, 64),
                 cbf_free ((void **) data, NULL))

  bitcount = 4 * 64;


    /* Initialise the pointers */

  unsigned_char_data = (unsigned char *) source;
  
  for (i = 0; i < 8; i++) trail_char_data[i] = NULL;

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



    /* Start from 0 */

  for (i = 0; i < numints-1; i++ ) lastelement[i] = 0;
  
  lastelement[numints-1] = unsign;
  
  ndimfast = ndimmid = ndimslow = 0;

  for (count = 0; count < nelem; count++)
  {
      /* Get the next element */
      
    trail_char_data[0] = unsigned_char_data;

    if (numints > 1 ) {
    
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
    
    if (element[numints-1] & sign) element[numints-1] |= (~limit);



      /* Add the offset to the buffer */
      
    if (v2flag) cbf_add_offsetv2 (data, element, lastelement, numints);
    
    else  cbf_add_offset (data, element, lastelement, numints);


      /* Is the buffer full? */

    if (data->offsets == 128)
    {
        /* Write the next block as economically as possible */

      cbf_onfailnez (cbf_pack_nextchunk (data, file, &chunkbits, v2flag, clipbits),
                     cbf_free ((void **) data, NULL))

      bitcount += chunkbits;
    }


      /* Update the previous element */

    for (i = 0; i < numints; i++) lastelement[i] = element[i];
    
    if (avgflag) {
    
      cbf_update_jpa_pointers(trail_char_data, 
                       &ndimfast,  &ndimmid, &ndimslow,
                         dimfast,   dimmid,   dimslow,
                          elsize, average, compression);
                              	        
      for (i = 0; i < numints; i++) lastelement[i] = average[i];
        
      lastelement[numints-1] +=unsign;
      
      lastelement[numints-1] &=limit;
      
      if (lastelement[numints-1] & sign) lastelement[numints-1] |= (~limit);

    	
    }
    
  }


    /* Flush the buffers */

  while (data->offsets > 0)
  {
    cbf_onfailnez (cbf_pack_nextchunk (data, file, &chunkbits, v2flag, clipbits),
                   cbf_free ((void **) data, NULL))

    bitcount += chunkbits;
  }


    /* Return the number of characters written */

  if (compressedsize)

    *compressedsize = (bitcount + 7) / 8;


    /* Free memory */

  return cbf_free (&memblock, NULL);
}


  /* Decompress an array */

int cbf_decompress_packed (void         *destination,
                           size_t        elsize,
                           int           elsign,
                           size_t        nelem,
                           size_t       *nelem_read,
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
  unsigned int next, pixel=0, pixelcount;

  unsigned int bits, ibits, iint, element[4], sign, unsign, limit, count;

  unsigned char *unsigned_char_data;

  unsigned char *trail_char_data[8];

  unsigned int offset [4], last_element [4];
  
  size_t numints;
  
  size_t ndimfast, ndimmid, ndimslow;

  int errorcode;
  
  int v2flag, avgflag, clipbits;

  int i;

  char * border;

  char * rformat;


    /* Is the element size valid? */

  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char) &&
      elsize != 2*sizeof(unsigned int) &&
      elsize != 4*sizeof(unsigned int))

    return CBF_ARGUMENT;

    /* check for compatible real format */

  if ( realarray ) {

    cbf_failnez (cbf_get_local_real_format(&rformat) )

    if ( strncmp(rformat,"ieee",4) ) return CBF_ARGUMENT;

  }
   bits = elsize * CHAR_BIT;

   if (bits < 1 || bits > 64)

     return CBF_ARGUMENT;

   numints = (bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));


    /* Initialise the pointers */

  unsigned_char_data = (unsigned char *) destination;

  
  for (i = 0; i < 8; i++) trail_char_data[i] = NULL;
  

    /* Maximum limits */

  sign = 1 << ((elsize-(numints-1)*sizeof(int))* CHAR_BIT - 1);

  if (elsize == numints*sizeof(int) )

    limit = ~0;

  else

    if (numints == 1) {

      limit = ~-(1 << (elsize * CHAR_BIT));

    } else {

      limit = ~-(1 << ((elsize-(numints-1)*sizeof(int)) * CHAR_BIT));

    }


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



    /* Initialise the first element */

  for (count = 0; count < numints-1; count++)

    last_element [count] = 0;
    
  last_element [numints-1] = unsign;


    /* Discard the reserved entry (64 bits) */

  cbf_failnez (cbf_get_integer (file, NULL, 0, 64))


    /* Pick up the flags */

  v2flag = 0;
  
  if ((compression&CBF_COMPRESSION_MASK) == CBF_PACKED_V2) v2flag = 1;
  
  avgflag = 1;

  if (dimfast == 0  && dimmid == 0 && dimslow == 0) avgflag = 0;
  
  if (compression&CBF_FLAT_IMAGE) avgflag = 0;
  
  clipbits = 0;
  
  if (avgflag) clipbits = bits;

  if (dimslow == 0) dimslow = 1;
  
  if (dimmid == 0) dimmid = 1;
  
  if (dimfast == 0) dimfast = nelem/(dimmid*dimslow);
  
  if (dimfast * dimmid * dimslow != nelem) return CBF_ARGUMENT;

    /* Read the elements */

  count = 0;
  
  ndimfast = ndimmid = ndimslow = 0;

  while (count < nelem)
  {
      /* Get the next 6 bits of data */

    errorcode = cbf_get_integer (file, (int *) &next, 0, 6+v2flag);

    if (errorcode)
    {
      if (nelem_read)

        *nelem_read = count + pixel;

      return errorcode;
    }

      /* Decode bits 0-5 (v2flag == 0) or 0-6 (v2flag == 1) */

    pixelcount = 1 << (next & 7);

    if (v2flag) bits = cbf_packedv2_bits [(next >> 3) & 15];
    else bits = cbf_packed_bits [(next >> 3) & 7];
    
    if (avgflag && bits == 65) bits = clipbits;
    

      /* Read the offsets */

    if (pixelcount + count > nelem)

      pixelcount = nelem - count;
      
    for (pixel = 0; pixel < pixelcount; pixel++)
    {

      for (i = 0; i < numints; i++) element[i] = last_element[i];

        /* Read an offset */

      for (i = 0; i < numints; i++) offset[i] = 0;
      
      if (bits)
      {
      
        if (bits > sizeof(unsigned int)*CHAR_BIT)  {
        
          errorcode = 0;  iint = 0;
          
          for (ibits = 0; ibits < bits; ibits+=sizeof(unsigned int)*CHAR_BIT) {
          
            errorcode |= cbf_get_integer (file, (int *)&(offset[iint]),
                                    ibits<(bits-sizeof(unsigned int)*CHAR_BIT)?0:1,
                                    ibits<(bits-sizeof(unsigned int)*CHAR_BIT)?(CHAR_BIT*sizeof (int)):
                                      bits-(CHAR_BIT*sizeof (unsigned int))*iint);

            iint++;
          	
          }
        	
        } else  {
        	
          errorcode = cbf_get_bits (file, (int *) offset, bits);

          for (i = 1; i < numints; i++)  {
        	
          offset[i] = ((int)offset[0])<0?(~0):0;  
          
          }
          
        }

        if (errorcode) {

            if (nelem_read)

            *nelem_read = count + pixel;

          return errorcode;
        }
        
      }

      if (numints > 1) {
      
        cbf_failnez(cbf_mpint_add_acc(element,numints,offset,numints))
      	
      } else   {
      	
        element[0] += (int)(offset[0]);
        
        element[0] &= limit;
 
      }
      
        /* Make the element signed? */

      element[numints-1] -= unsign;
      
        /* Save the location of to which this element will be
           stored */
      
      trail_char_data[0] = unsigned_char_data;
      

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

      if (avgflag) {
      	
        cbf_failnez(cbf_update_jpa_pointers(trail_char_data, 
                       &ndimfast,  &ndimmid, &ndimslow,
                         dimfast,   dimmid,   dimslow,
                          elsize, last_element, compression))
        
        last_element[numints-1] += unsign; 
        
        last_element[numints-1] &= limit; 
        
        /* if (count < 1000) fprintf(stderr," count, offset, element, last_element: %d %x %x %x\n",
           count, offset[0], element[0], last_element[0]);  */       
       
      } else {
      	
       for (i = 0; i < numints-1; i++ )last_element[i] = element[i];
      
       last_element[numints-1] = element[numints-1]+unsign;
       
       /* if (count < 1000) fprintf(stderr," count, offset, element, last_element: %d %x %x %x\n",
           count, offset[0], element[0], last_element[0]);  */          

      }
   
    }
    
    count += pixelcount;


  }

    /* Number read */

  if (nelem_read)

    *nelem_read = count;


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif
