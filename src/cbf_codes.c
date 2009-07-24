/**********************************************************************
 * cbf_codes -- convert between encoded and unencoded binary          *
 *              calculate message digest                              *
 *                                                                    *
 * Version 0.7.7 19 February 2007                                     *
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

/**********************************************************************
 * Substantial portions of this code were derived from the mpack      *
 * routine codes.c, which contains the following two notices          *
 **********************************************************************/

/**********************************************************************
 * First notice from mpack routine codes.c:                           *
 *                                                                    *
 * (C) Copyright 1993,1994 by Carnegie Mellon University              *
 * All Rights Reserved.                                               *
 *                                                                    *
 * Permission to use, copy, modify, distribute, and sell this         *
 * software and its documentation for any purpose is hereby granted   *
 * without fee, provided that the above copyright notice appear       *
 * in all copies and that both that copyright notice and this         *
 * permission notice appear in supporting documentation, and that     *
 * the name of Carnegie Mellon University not be used in advertising  *
 * or publicity  pertaining to distribution of the software without   *
 * specific, written prior permission.  Carnegie Mellon University    *
 * makes no representations about the suitability of this software    *
 * for any purpose.  It is provided "as is" without express or        *
 * implied warranty.                                                  *
 *                                                                    *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO *
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY *
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE       *
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY   *
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,    *
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS     *
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR            *
 * PERFORMANCE OF THIS SOFTWARE.                                      *
 **********************************************************************/

/**********************************************************************
 * Second  notice from mpack routine codes.c:                         *
 *                                                                    *
 * Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)   *
 *                                                                    *
 * Permission to use, copy, modify, and distribute this material      *
 * for any purpose and without fee is hereby granted, provided        *
 * that the above copyright notice and this permission notice         *
 * appear in all copies, and that the name of Bellcore not be         *
 * used in advertising or publicity pertaining to this                *
 * material without the specific, prior written permission            *
 * of an authorized representative of Bellcore.  BELLCORE             *
 * MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY         *
 * OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS",         *
 * WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.                         *
 **********************************************************************/

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_codes.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <sys/stat.h>
#include <wchar.h>

  /* Check a 24-character base-64 MD5 digest */

int cbf_is_base64digest (const char *encoded_digest)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  if (!encoded_digest)

    return 0;

  if (strlen (encoded_digest) != 24)

    return 0;

  return strspn (encoded_digest, basis_64) == 22 &&
                 encoded_digest [22] == '=' &&
                 encoded_digest [23] == '=';
}


  /* Encode a 16-character MD5 digest in base-64 (25 characters) */

int cbf_md5digest_to64 (char *encoded_digest, const unsigned char *digest)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  int todo;

  if (!encoded_digest || !digest)

    return CBF_ARGUMENT;


    /* Encode the 16 characters in base 64 */

  for (todo = 0; todo < 18; todo += 3)
  {
    encoded_digest [0] = basis_64 [((digest [todo + 0] >> 2) & 0x03f)];

    if (todo < 15)
    {
      encoded_digest [1] = basis_64 [((digest [todo + 0] << 4) & 0x030) |
                                     ((digest [todo + 1] >> 4) & 0x00f)];
      encoded_digest [2] = basis_64 [((digest [todo + 1] << 2) & 0x03c) |
                                     ((digest [todo + 2] >> 6) & 0x003)];
      encoded_digest [3] = basis_64 [((digest [todo + 2])      & 0x03f)];
    }
    else
    {
      encoded_digest [1] = basis_64 [((digest [todo + 0] << 4) & 0x030)];

      encoded_digest [2] = encoded_digest [3] = '=';
    }

    encoded_digest += 4;
  }

  *encoded_digest  = '\0';

  return 0;
}


  /* Calculate the MD5 digest (25 characters) of a block of data */

int cbf_md5digest (cbf_file *file, size_t size, char *digest)
{
  MD5_CTX context;

  unsigned char rawdigest [17];

  unsigned int todo;

  const char *buffer;


    /* Initialise the MD5 context */

  MD5Init (&context);


    /* Update the digest in blocks of CBF_TRANSFER_BUFFER */

  while (size > 0)
  {
    if (size >= CBF_TRANSFER_BUFFER)

      todo = CBF_TRANSFER_BUFFER;

    else

      todo = size;

    cbf_failnez (cbf_get_block (file, todo))

    cbf_failnez (cbf_get_buffer (file, &buffer, NULL))

    MD5Update (&context, buffer, todo);

    size -= todo;
  }


    /* Get the final digest */

  MD5Final (rawdigest, &context);

  cbf_md5digest_to64 (digest, rawdigest);


    /* Success */

  return 0;
}


  /* Convert binary data to quoted-printable text */

int cbf_toqp (cbf_file *infile, cbf_file *outfile, size_t size)
{
  static char basis_16 [] = "0123456789ABCDEF";

  int c;


    /* Check the arguments */

  if (!infile || !outfile)

    return CBF_ARGUMENT;


    /* Copy the characters */

  while (size > 0)
  {
      /* Read the next character */

    c = cbf_get_character (infile);

    if (c == EOF)

      return CBF_FILEREAD;

    size--;

    if (outfile->column > 74)

      cbf_failnez (cbf_write_string (outfile, "=\n"))

    if ((c <= 31)  ||
        (c >= 39 && c <= 41) ||
        (c >= 43 && c <= 47) ||
        (c == 58)  ||
        (c == 61)  ||
        (c == 63)  ||
        (c >= 127) ||
        (c == ';' && outfile->column == 0))
    {
        /* Base-16 */

      if (outfile->column > 72)

        cbf_failnez (cbf_write_string (outfile, "=\n"))

      cbf_failnez (cbf_write_character (outfile, '='))
      cbf_failnez (cbf_write_character (outfile, basis_16 [(c >> 4) & 0x0f]))
      cbf_failnez (cbf_write_character (outfile, basis_16 [c & 0x0f]))
    }
    else

        /* Base-256 */

      cbf_failnez (cbf_write_character (outfile, c))
  }

  if (outfile->column)

    cbf_failnez (cbf_write_string (outfile, "=\n"))


    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (outfile))


    /* Success */

  return 0;
}


  /* Convert binary data to base-64 text */

int cbf_tobase64 (cbf_file *infile, cbf_file *outfile, size_t size)
{
  static char basis_64 [] =

       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  int c [3];

  int read;

  while (size > 0)
  {
      /* Read up to 3 characters */

    c [1] = c [2] = 0;

    for (read = 0; read < 3 && read < size; read++)
    {
      c [read] = cbf_get_character (infile);

      if (c [read] == EOF)

        return CBF_FILEREAD;
    }

    size -= read;

    if (outfile->column > 71)

      cbf_failnez (cbf_write_character (outfile, '\n'))


      /* Write a 24-bit chunk in base-64 */

    cbf_failnez (cbf_write_character (outfile,
                                        basis_64 [(c [0] >> 2) & 0x03f]))
    cbf_failnez (cbf_write_character (outfile,
                                        basis_64 [((c [0] << 4) & 0x030) |
                                                  ((c [1] >> 4) & 0x00f)]))

    if (read == 1)

      cbf_failnez (cbf_write_string (outfile, "=="))

    else
    {
      cbf_failnez (cbf_write_character (outfile,
                                        basis_64 [((c [1] << 2) & 0x03c) |
                                                  ((c [2] >> 6) & 0x003)]))

      if (read == 2)

        cbf_failnez (cbf_write_character (outfile, '='))

      else

        cbf_failnez (cbf_write_character (outfile, basis_64 [c [2] & 0x03f]))
    }
  }

  if (outfile->column)

    cbf_failnez (cbf_write_character (outfile, '\n'))


    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (outfile))


    /* Success */

  return 0;
}

int cbf_tobase32k(cbf_file *infile, cbf_file *outfile, size_t size)
{
	#define maxlen 30
	unsigned char  *txt = NULL;       /*text to be encoded null terminated*/
        char *enc = NULL;       /*encoded text */
        size_t* pencsize;       /*pointer to the size of enc null terminated*/
        size_t encsize = 0;     /*size of enc*/
        size_t sz = 0;		/*number of characters read*/
	size_t  encchars = 0;	/*number of encoded characters altogether*/
	int bigEndian = 0;
	unsigned char tmp[3];
	int count_w = 0;
	int rav = 0;
	unsigned char b;
	int count_enc=0;
	int new_l =0;
	tmp[2] = '\0';
        txt = (unsigned char *) malloc(sizeof(char) *maxlen + 1);
	txt[maxlen] = '\0'; /*freaky but makes me feel better*/
        pencsize = &encsize;
	      sz =0;
	      while(sz<maxlen)
	      {
		      int a =0;
		      if((a = cbf_get_character(infile)) !=EOF)
		      {
			      b = a;
			      txt[sz] =b;
		      }
		      else
		      {
		                     break;
		      }
		      sz++;



		      
              }
       bigEndian = cbf_isBigEndian();
	/*print the BOM*/
	if(bigEndian != 0) {
		cbf_put_character(outfile, '\xFF');
		cbf_put_character(outfile, '\xFE');
	} else {
		 cbf_put_character(outfile, '\xFE');
		 cbf_put_character(outfile, '\xFF');
	}
	
		while(sz > 0)
        	{
			if(sz <30)
			{
				rav = 15 - ((sz*8)%15);			
			}
                	enc = cbf_encode32k_bit_op(txt, sz, pencsize);
                	cbf_endianFix(enc ,*pencsize, 0, bigEndian);
            count_w = 0;
			while(count_w<*pencsize)
			{
				cbf_put_character(outfile, enc[count_w]);
				count_w++;
				count_enc++;			
			}
			if(new_l == 0)
			{
				new_l++;
			}	
			else if(new_l == 3)
			{
				cbf_put_character(outfile, '\x00');
				cbf_put_character(outfile, '\x0A');				      
				new_l =0;
			}		

			
			encchars += *pencsize;
		
                	if(enc){
                        	free(enc);
                        	*pencsize = 0;
                	}


			sz =0;
	    		  while(sz<maxlen)
	      		{
		      		int a =0;
		      		if((a = cbf_get_character(infile)) !=EOF)
		      {
			      b = a;
			      txt[sz] =b;
		      }
		      else
		      {
		                     break;
		      }
		      sz++;
			}



		}
	     if(rav>=8 && rav<15)
	      {
		      if(cbf_isBigEndian() !=0)
		      {
			      cbf_put_character(outfile, '\x3D');
			      cbf_put_character(outfile, '\x00');
		      }
		      else
		      {
			      cbf_put_character(outfile, '\x00');
			      cbf_put_character(outfile, '\x3D');
		      }
		}

			      cbf_put_character(outfile, '\xEF');
			      cbf_put_character(outfile, '\xBB');
			      cbf_put_character(outfile, '\xBF');
	free(txt);
	return 0;
}

char * cbf_encode32k_bit_op(unsigned char *txt, size_t size, size_t *size2)
{
#define offset 1
	 /*Formula:
         *
         * First loop bits taken from index-1: form n-1 to >= 0 ,right shift by + (7-n)
         * Second loop bits taken from index:  from 7 to > n, right shift by -(n+1)
	 
         * Fourth loop bits taken from index+1: from 7 to > n right shift by -(n+1)
         */
        size_t pair = 0;
	int shift = 0;
        size_t n = 0;
        size_t indx = 0;
        size_t index = 0;
        size_t pairs = 0;
	unsigned char first = 0;
        unsigned char second = 0;
        unsigned char mask = 1;
        unsigned char result = 0;
	char * res = NULL;	/*the encoded string (result)*/
	
	/*find out the number of pairs and the length of the string enc*/
	pairs = ceil(((double)size *8.0)/15.0);	 /*On every 16 bits we have one bit lost 
						   so we use 16 bits for encoding 15 bits */
	*size2 = pairs *2;
	res = (char *) malloc(sizeof(char)*(*size2));
	memset(res, 0, *size2);
/*loop through all pairs and encode them */
  for(pair = 0; pair< pairs; pair++)
  {
	  n = pair%8;
	  indx = pair*2;
	  index = indx - (pair/8);
       	  first = 0;
	  second = 0;
	  result = 0;
	  
/*encoding algorithm starts*/  
	  /*First character*/
        if(index <= size)  {
                for(shift =n-1;shift >=0;shift--)
                {
                        result = txt[index-1]>>shift;
                        result = result & mask;
                        first += result << (shift +(7 - n));
                }
                if(index < size){
                        for(shift = 7; shift > n; shift--)
                        {
                                result = txt[index] >> shift;
                                result = result & mask;
                                first += result << (shift - (n + 1));
                        }
  	/*fill in the second character*/
                        for(shift = n;shift >= 0; shift--)
                        {
                                result = txt[index] >> shift;
                                result = result & mask;
                                second += result << (shift + (7 -n));
        		}
                        if((index + 1) < size){
		       		for(shift = 7; shift > n; shift --)
                                {
                                        result = txt[index+1] >> shift;
                                        result = result & mask;
                                        second += result << (shift - (n+1));
                                }
                        }
                }
        }
	res[indx] = first + offset;
        res[indx+1] = second;
  }
  return res;		


}

/*Determine whether the machine is little endian*/
int cbf_isBigEndian()
{
	long tmp = 1;
	
	return !(*(char *)(&tmp));
}

void cbf_endianFix(char *str, size_t size, int fromEndian, int toEndian)
{
	size_t i = 0; 

	if(fromEndian != toEndian){
		for (i = 0; i < size; i+=2)
		{
			/*exchange the two bytes*/
			/*since we are in bitwise mood use the triple xor trick*/
			str[i] ^= str[i+1];
			str[i+1] ^= str[i];
			str[i]  ^= str[i+1];
		}	
	}
}

  /* Convert binary data to base-8/base-10/base-16 text */

int cbf_tobasex (cbf_file *infile, cbf_file *outfile, size_t size,
                                                      size_t elsize,
                                                      unsigned int base)
{
  int c [8];

  int count, read;

  long l;

  unsigned long block_count;

  char line [96], number [64];


    /* Check the arguments */

  if (elsize > 8 || (base != 8 && base != 10 && base != 16))

    return CBF_ARGUMENT;


  block_count = 0;

  while (size > 0)
  {
      /* End of a 512-element block? */

    if ((block_count % 512) == 0)
    {
      if (outfile->column)

        cbf_failnez (cbf_write_character (outfile, '\n'))

      if (block_count)

        cbf_failnez (cbf_write_string (outfile, "#\n"))

      if (base == 8)

        cbf_failnez (cbf_write_string (outfile, "# Octal encoding"))

      else

        if (base == 10)

          cbf_failnez (cbf_write_string (outfile, "# Decimal encoding"))

        else

          cbf_failnez (cbf_write_string (outfile, "# Hexadecimal encoding"))

      sprintf (line, ", byte %lu", (unsigned long) block_count * elsize);

      cbf_failnez (cbf_write_string (outfile, line))

      if (outfile->write_encoding & ENC_FORWARD)

        cbf_failnez (cbf_write_string (outfile, ", byte order 1234...\n#\n"))

      else

        cbf_failnez (cbf_write_string (outfile, ", byte order ...4321\n#\n"))
    }


      /* Read up to elsize characters */

    memset (c, 0, sizeof (c));

    for (read = 0; read < elsize && read < size; read++)
    {
      c [read] = cbf_get_character (infile);

      if (c [read] == EOF)

        return CBF_FILEREAD;
    }

    size -= read;

    block_count++;


      /* Make the number */

    number [0] = '\0';

    if ((outfile->write_encoding & ENC_BACKWARD) && read < elsize)

      for (count = read; count < elsize; count++)

        strcat (number, "==");

    l = 0;

    if (outfile->write_encoding & ENC_FORWARD)

      for (count = read - 1; count >= 0; count--)

        l = (l << 8) | (c [count] & 0x0ff);

    else

      for (count = 0; count < read; count++)

        l = (l << 8) | (c [count] & 0x0ff);

    if (base == 8)

      sprintf (number + strlen (number), "%lo", l);

    else

      if (base == 10)

        sprintf (number + strlen (number), "%lu", l);

      else

        sprintf (number + strlen (number), "%lX", l);

    if ((outfile->write_encoding & ENC_FORWARD) && read < elsize)

      for (count = read; count < elsize; count++)

        strcat (number, "==");


      /* Write the number */

    if (outfile->column + strlen (number) > 74)

      cbf_failnez (cbf_write_character (outfile, '\n'))

    if (outfile->column)

      cbf_failnez (cbf_write_character (outfile, ' '))

    else
    {
        /* Start a new line */

      if (base == 8)

        cbf_failnez (cbf_write_character (outfile, 'O'))

      else

        if (base == 10)

          cbf_failnez (cbf_write_character (outfile, 'D'))

        else

          cbf_failnez (cbf_write_character (outfile, 'H'))

      sprintf (line, "%1u", (unsigned int) elsize);

      cbf_failnez (cbf_write_string (outfile, line))

      if (outfile->write_encoding & ENC_FORWARD)

        cbf_failnez (cbf_write_string (outfile, "> "))

      else

        cbf_failnez (cbf_write_string (outfile, "< "))
    }

    cbf_failnez (cbf_write_string (outfile, number))
  }

  if (outfile->column)

    cbf_failnez (cbf_write_character (outfile, '\n'))


    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (outfile))


    /* Success */

  return 0;
}


  /* Convert quoted-printable text to binary data */

int cbf_fromqp (cbf_file *infile, cbf_file *outfile, size_t size,
                                                     size_t *readsize,
                                                     char *digest)
{
  MD5_CTX context;

  unsigned char buffer [64], rawdigest [17];

  int c, bufsize;

  char val [3], *end;

  size_t count;


    /* Initialise the MD5 context */

  if (digest)

    MD5Init (&context);


  bufsize = 0;

  count = 0;

  val [2] = '\0';

  while (count < size)
  {
      /* Read the (first) character */

    c = cbf_read_character (infile);

    if (c == EOF)

      return CBF_FILEREAD;


      /* Decode it */

    if (c == '=')
    {
        /* Get the second character */

      c = cbf_read_character (infile);

      if (c == EOF)

        return CBF_FILEREAD;

      if (c != '\n')
      {
          /* Get the third character */

        val [0] = c;

        c = cbf_read_character (infile);

        if (c == EOF)

          return CBF_FILEREAD;

        val [1] = c;


          /* Calculate the value */

        c = strtoul (val, &end, 16);

        if (end != &val [2])

          return CBF_FORMAT;
      }
    }


      /* Save it */

    if (outfile)

      cbf_failnez (cbf_put_character (outfile, c))

    if (digest)
    {
      buffer [bufsize] = c;

      bufsize++;

      if (bufsize > 63)
      {
        MD5Update (&context, buffer, 64);

        bufsize = 0;
      }
    }

    count++;
  }


    /* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)

    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */

  if (readsize)

    *readsize = count;


    /* Success */

  return 0;
}


  /* Convert base-64 text to binary data */

int cbf_frombase64 (cbf_file *infile, cbf_file *outfile, size_t size,
                                                         size_t *readsize,
                                                         char *digest)
{
  static int decode_64 [256] = {

    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, 64, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1

    };

  MD5_CTX context;

  unsigned char buffer [64], rawdigest [17];

  int c [4], d [3], bufsize;

  int read, write;

  size_t count;


    /* Initialise the MD5 context */

  if (digest)

    MD5Init (&context);


  count = 0;

  bufsize = 0;

  while (count < size)
  {
      /* Read 4 characters */

    for (read = 0; read < 4; read++)

      do
      {
        c [read] = cbf_read_character (infile);

        if (c [read] == EOF)

          return CBF_FILEREAD;
      }
      while (decode_64 [c [read] & 0x0ff] < 0);


      /* End of data? */

    if (c [0] == '=' || c [1] == '=')

      break;


      /* Valid combinations: xxxx xxx= xx== */

    c [0] = decode_64 [c [0] & 0x0ff];
    c [1] = decode_64 [c [1] & 0x0ff];
    c [2] = decode_64 [c [2] & 0x0ff];
    c [3] = decode_64 [c [3] & 0x0ff];

    d [0] = ((c [0] << 2) & 0x0fc) | ((c [1] >> 4) & 0x003);
    d [1] = ((c [1] << 4) & 0x0f0) | ((c [2] >> 2) & 0x00f);
    d [2] = ((c [2] << 6) & 0x0c0) | ((c [3]     ) & 0x03f);

    if (c [2] == 64)

      read = 1;

    else

      if (c [3] == 64)

        read = 2;

      else

        read = 3;


      /* Save the data */

    for (write = 0; write < read; write++)
    {
      if (outfile)

        cbf_failnez (cbf_put_character (outfile, d [write]))

      if (digest)
      {
        buffer [bufsize] = (unsigned char) d [write];

        bufsize++;

        if (bufsize > 63)
        {
          MD5Update (&context, buffer, 64);

          bufsize = 0;
        }
      }
    }

    count += read;
  }


    /* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)

    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */

  if (readsize)

    *readsize = count;


    /* Success */

  return 0;
}

int cbf_frombase32k(cbf_file *infile, cbf_file *outfile, size_t size, size_t *readsize, char *digest)
{
	MD5_CTX context;
	unsigned char buffer[64], rawdigest [17];
	int bufsize;
        char *enc = NULL;       /*encoded text */
        char *decoded = NULL;   /*decoded text (should be the same as txt)*/
        size_t sz = 0;
	int mah =0;		/*the number of bytes that should be cut from the end*/
	int clear;
	char b ='\0';
	int a = 0;
	int sc;
	int count_w =0;
	int check_range =0;
	size_t all = 0;
	
	/* Initialize the MD5 context*/
	if (digest)
		MD5Init(&context);
	bufsize =0;

	enc = (char *) malloc(32*sizeof(char));
	decoded = (char *) malloc(30*sizeof(char));
	
	sz =0;
	while(sz<2)
	{
		a =0;
		if((a = cbf_get_character(infile)) !=EOF)
	        {
			b = a;
			enc[sz] =b;
		}
		else	
		{
		           break;
		}
	        sz++;
	 }	

		
	if((enc[0] == '\xFF' && enc[1] == '\xFE' && cbf_isBigEndian() == 0) || (enc[0] == '\xFE' && enc[1] == '\xFF' && cbf_isBigEndian() !=0))
	{
		sz =0;
		while(sz<32)
                {
			a =0;
                   	if((a = cbf_get_character(infile)) !=EOF)
                       	{
				if(sz%2 == 0)
                              	{
   	                          	b = a;
					check_range =a;
                                }
                              	else if(sz%2 == 1)
                              	{
                                       	check_range = check_range+256*a;
					if((check_range < 256 || check_range > 33023) && (check_range != 61 && check_range != 61371))
                                        {
                                        	sz--;
                                              	continue;
                                      	}
                               		else
					{
                                              	enc[sz-1] =b;
                                               	b =a;
                                               	enc[sz] =b;
                                       	}
                         	}
                          }
			 else
			 {
				break;
			 }
                         sz++;
				 
                 }
		if(cbf_isBigEndian() == 0) /* it is big endian */
		{
			cbf_endianFix(enc, sz, 1, 0);
		}
		else if(cbf_isBigEndian() != 0)
		{
			cbf_endianFix(enc, sz, 0, 1);
		}
		sc=0;
       		while(sc<sz)	
		{
		 	if(enc[sc]=='\xEF' && enc[sc+1]=='\xBB')
			{
				sz=sc;
			}
			else
			{
				sc+=2;	 	
			}
		}
					
		while(sz>0)
		{
			if(sz>0 && sz<18)
			{	
				
				
					if(enc[sz-2] == '\x00' && enc[sz-1] == '\x3D')
					{
						cbf_decode32k_bit_op(enc, decoded, (sz-2));
						count_w=0;
						while(count_w<(sz-4))
			                        { 
							if(outfile)
							
							cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
							if (digest)
							{
								buffer[bufsize] = decoded[count_w];
								bufsize++;

								if (bufsize >63)
								{
									MD5Update( &context, buffer, 64);
									bufsize = 0;

								}
							}
							count_w++;
							all++;
						}
						sz=0;
					}
					else{
						size_t temp = sz;
						cbf_decode32k_bit_op(enc, decoded, temp);
						count_w=0;
				 		while(count_w<(temp-1))
				 		{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;									                                   
						}       
					}
				
				for (clear=0; clear<30; clear++)
				{
					decoded[clear]='\0';
				}
				sz = 0;	
				break;
			}
			else if(sz == 18)
			{
				
					if(enc[16] == '\x00' && enc[17] == '\x3D')
					{
						mah = 1;
						cbf_decode32k_bit_op(enc, decoded, (sz-2));
						count_w=0;
						 while(count_w<(sz-4))
						 {
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						 } 
						
							
					}
					else
					{
						
						int temp = sz;
						cbf_decode32k_bit_op(enc, decoded, temp);
						count_w=0;
						while(count_w<(temp-2))
						{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						}
						sz=-1;
					}
					for (clear=0; clear<30; clear++)
					{
						decoded[clear]='\0';
					}
					sz=0;
				
					
			}

			else if(sz>18 && sz<32)
			{		
						if(enc[sz-2]=='\x00' && enc[sz-1] =='\x3D')
						{
							cbf_decode32k_bit_op(enc, decoded, (sz-2));
							count_w=0;
							while(count_w<(sz-5))
							{
								if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
							}
							sz=0;
							
						}
				
						else{
							int temp = sz;
							cbf_decode32k_bit_op(enc, decoded, temp);
								count_w=0;
								while(count_w<(temp-2))
							{
								if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
							}
						   }
				
				for (clear=0; clear<30; clear++)
				{
					decoded[clear]='\0';
				}
				sz=0;
				
			}	
		
			else if (sz == 32)
			{
				if(enc[30] == '\x00' && enc[31] == '\x3D')
				{
				        cbf_decode32k_bit_op(enc, decoded, (sz-2));
						count_w=0;
						 while(count_w<(sz-5))
						 {
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						 }
					sz=0;		 
				}					
				else
				{		
					cbf_decode32k_bit_op(enc, decoded, 32);
		        		sz =0;
                        	 while(sz<32)
                        	 {
                                 	a =0;
                                 	if((a = cbf_get_character(infile)) !=EOF)
                                 	{
                                         	if(sz%2 == 0)
                                         	{
                                         	       b = a;
                                         	       check_range = a;
                                         	}
                                         	else if (sz%2 == 1)
                                        	{
                                                	check_range = check_range + 256*a;
                                               		if((check_range < 256 || check_range > 33023) && (check_range != 61 && check_range != 61371))
                                                	{
                                                        	sz--;
        							continue;                        
                                                	}
                                                	else{
                                                        	enc[sz-1] =b;
                                                        	b =a;
                                                        	enc[sz] =b;
                                                	}
                                        	}

					
                                	 }

					 else{
						
						break;
				 	}
                                 	sz++; 
                          	} 
				if(cbf_isBigEndian() == 0) /*it is big endian */
				{
					cbf_endianFix(enc, sz, 1, 0);
				}
				else if(cbf_isBigEndian() != 0)
				{
					cbf_endianFix(enc, sz, 0, 1);
				}
				 sc =0;
				 while(sc<sz)
				 {
				 	if(enc[sc] == '\xEF' &&  enc[sc+1]=='\xBB')
					{
						sz=sc;
					}
					else
					{
						sc+=2;
				 
				 
				 	}
			}
			
					if(enc[0] == '\x00' && enc[1] == '\x3D')
					{
	/*					fwrite(decoded, 1, 29, fout); */
						count_w=0;
						while(count_w<29)
						{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						}
						for (clear=0; clear<30; clear++)
						{
							decoded[clear]='\0';
						}

					
						sz=0;
					}		
					else
					{
						count_w=0;
						while(count_w<30)
						{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						}   
						for (clear=0; clear<30; clear++)
					 	{
						 	decoded[clear]='\0';
					 	}
					 
					}
				
				} 
	
			}	
			
		}

/* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)

    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */

  if (readsize)

    *readsize = all;


	free(enc);
	free(decoded);
		return 0;
	}
	else if((enc[0] == '\xFF' && enc[1] == '\xFE') || (enc[0] == '\xFE' && enc[1] == '\xFF') )

	{
		sz =0;
		while(sz<32)
                {
			a =0;
                   	if((a = cbf_get_character(infile)) !=EOF)
                       	{
				if(sz%2 == 0)
                              	{
   	                          	b = a;
					check_range =256*a;
                                }
                              	else if(sz%2 == 1)
                              	{
                                       	check_range = check_range+a;
					if((check_range < 256 || check_range > 33023) && (check_range != 61 && check_range != 61371))
                                        {
                                        	sz--;
                                              	continue;
                                      	}
                               		else
					{
                                              	enc[sz-1] =b;
                                               	b =a;
                                               	enc[sz] =b;
                                       	}
                         	}
                          }
			 else
			 {
				break;
			 }
                         sz++;
				 
                 }
		sc=0;
       		while(sc<sz)	
		{
		 	if(enc[sc]=='\xEF' && enc[sc+1]=='\xBB')
			{
				sz=sc;
			}
			else
			{
				sc+=2;	 	
			}
		}
					
		while(sz>0)
		{
			if(sz>0 && sz<18)
			{	
				
				
					if(enc[sz-2] == '\x00' && enc[sz-1] == '\x3D')
					{
						cbf_decode32k_bit_op(enc, decoded, (sz-2));
						count_w=0;
						while(count_w<(sz-4))
			                        { 
							if(outfile)
							
							cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
							if (digest)
							{
								buffer[bufsize] = decoded[count_w];
								bufsize++;

								if (bufsize >63)
								{
									MD5Update( &context, buffer, 64);
									bufsize = 0;

								}
							}
							count_w++;
							all++;
						}
						sz=0;
					}
					else{
						size_t temp = sz;
						cbf_decode32k_bit_op(enc, decoded, temp);
						count_w=0;
				 		while(count_w<(temp-1))
				 		{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;									                                   
						}       
					}
				
				for (clear=0; clear<30; clear++)
				{
					decoded[clear]='\0';
				}
				sz = 0;	
				break;
			}
			else if(sz == 18)
			{
				
					if(enc[16] == '\x00' && enc[17] == '\x3D')
					{
						mah = 1;
						cbf_decode32k_bit_op(enc, decoded, (sz-2));
						count_w=0;
						 while(count_w<(sz-4))
						 {
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						 } 
						
							
					}
					else
					{
						
						int temp = sz;
						cbf_decode32k_bit_op(enc, decoded, temp);
						count_w=0;
						while(count_w<(temp-2))
						{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						}
						sz=-1;
					}
					for (clear=0; clear<30; clear++)
					{
						decoded[clear]='\0';
					}
					sz=0;
				
					
			}

			else if(sz>18 && sz<32)
			{		
						if(enc[sz-2]=='\x00' && enc[sz-1] =='\x3D')
						{
							cbf_decode32k_bit_op(enc, decoded, (sz-2));
							count_w=0;
							while(count_w<(sz-5))
							{
								if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
							}
							sz=0;
							
						}
				
						else{
							int temp = sz;
							cbf_decode32k_bit_op(enc, decoded, temp);
								count_w=0;
								while(count_w<(temp-2))
							{
								if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
							}
						   }
				
				for (clear=0; clear<30; clear++)
				{
					decoded[clear]='\0';
				}
				sz=0;
				
			}	
		
			else if (sz == 32)
			{
				if(enc[30] == '\x00' && enc[31] == '\x3D')
				{
				        cbf_decode32k_bit_op(enc, decoded, (sz-2));
						count_w=0;
						 while(count_w<(sz-5))
						 {
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						 }
					sz=0;		 
				}					
				else
				{		
					cbf_decode32k_bit_op(enc, decoded, 32);
		        		sz =0;
                        	 while(sz<32)
                        	 {
                                 	a =0;
                                 	if((a = cbf_get_character(infile)) !=EOF)
                                 	{
                                         	if(sz%2 == 0)
                                         	{
                                         	       b = a;
                                         	       check_range = 256*a;
                                         	}
                                         	else if (sz%2 == 1)
                                        	{
                                                	check_range = check_range + a;
                                               		if((check_range < 256 || check_range > 33023) && (check_range != 61 && check_range != 61371))
                                                	{
                                                        	sz--;
								continue;
                                
                                                	}
                                                	else{
                                                        	enc[sz-1] =b;
                                                        	b =a;
                                                        	enc[sz] =b;
                                                	}
                                        	}

					
                                	 }

					 else{
						
						break;
				 	}
                                 	sz++; 
                          	} 
				 sc =0;
				 while(sc<sz)
				 {
				 	if(enc[sc]=='\xEF' && enc[sc+1]=='\xBB')
					{
						sz=sc;
					}
					else
					{
						sc+=2;
				 
				 
				 	}
			}
				
					if(enc[0] == '\x00' && enc[1] == '\x3D')
					{
	/*					fwrite(decoded, 1, 29, fout); */
						count_w=0;
						while(count_w<29)
						{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						}
						for (clear=0; clear<30; clear++)
						{
							decoded[clear]='\0';
						}

					
						sz=0;
					}		
					else
					{
						count_w=0;
						while(count_w<30)
						{
							if(outfile)
							
						cbf_failnez(cbf_put_character(outfile, decoded[count_w]));
						if (digest)
						{
							buffer[bufsize] = decoded[count_w];
							bufsize++;

							if (bufsize >63)
							{
								MD5Update( &context, buffer, 64);
								bufsize = 0;

							}
						}
							count_w++;
							all++;
						}   
						for (clear=0; clear<30; clear++)
					 	{
						 	decoded[clear]='\0';
					 	}
					 
					}
				
				} 
	
			}	
			
		}


/* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)

    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */

  if (readsize)

    *readsize = all;


		free(enc);
		free(decoded);
		return 0;
	}
	

	else

	{
			printf("The file given for decoding was not correctly encoded!");
			free(enc);
			free(decoded);	
			return -1;
	}
}

int cbf_decode32k_bit_op(char *encoded, char *decoded, size_t size)
{
        unsigned char tmp = '\0';
        unsigned char result = '\0';
        unsigned char mask = 1;

        size_t i = 0,j=0;
        int need= 7, have = -1;
	
        for(i = 0;i < size; i++)       
        {
                result= '\0';
                need = 7;       /*zero based*/
                /*make another loop form 0 to need and add bits to result until need is zero*/
                while(need > -1)
                {
                        /*if there are no more bits left take the next character*/
                        if(have == -1){
                         /*if this is the first character in a pair then subtract the offset
                                *and record that it has only 7 bits
				*/
                                if(j % 2== 0){
                                        tmp = encoded[j] -offset;
                                        have = 6;
                                } else {
                                        tmp = encoded[j];
                                        have = 7;
                                }
				j++;
                        }
                        result = tmp >>have;
                        result = result & mask;
                        result = result <<need;
                        decoded[i] += result;
                        have--;
 			need--;
                }
        }
	return 1;
}









  /* Convert base-8/base-10/base-16 text to binary data */

int cbf_frombasex (cbf_file *infile, cbf_file *outfile, size_t size,
                                                        size_t *readsize,
                                                        char *digest)
{
  MD5_CTX context;

  unsigned char buffer [64], rawdigest [17];

  int c, bufsize;

  char val [80], *end;

  int read, write, base, direction, elsize, valcount, padding;

  size_t count;

  unsigned long l;


    /* Defaults */

  base = 10;

  direction = 1;

  elsize = 4;

  count = 0;

  valcount = 0;

  padding = 0;

  bufsize = 0;


    /* Initialise the MD5 context */

  if (digest)

    MD5Init (&context);


  while (count < size)
  {
      /* Read the (first) character */

    c = cbf_read_character (infile);

    if (c == EOF)

      return CBF_FILEREAD;


      /* Interpret it */

    if (c == '>')
    {
      direction = 1;

      c = ' ';
    }
    else

      if (c == '<')
      {
        direction = -1;

        c = ' ';
      }
      else

        if (c == '#')
        {
            /* Comment */

          do

            c = cbf_read_character (infile);

          while (c != EOF && c != '\n');

          if (c == EOF)

            return CBF_FORMAT;
        }

    switch (infile->column)
    {
      case 1:

        if (c == 'O' || c == 'o')

          base = 8;

        else

          if (c == 'D' || c == 'd')

            base = 10;

          else

            if (c == 'H' || c == 'h')

              base = 16;

            else

              return CBF_FORMAT;

        break;

      case 2:

        if (isdigit (c) && c != '0')

          elsize = c - '0';

      case 3:

        break;

      default:

        if (!isspace (c))

          if (c == '=')

            padding++;

          else
          {
              /* Save the character */

            if (valcount > 78)

              return CBF_FORMAT;

            val [valcount] = c;

            valcount++;
          }
        else

          if (valcount)
          {
              /* Convert the number */

            val [valcount] = '\0';

            l = strtoul (val, &end, base);

            if (end != &val [valcount])

              return CBF_FORMAT;


              /* Save the binary data */

            if ((padding % 2) || padding > 6)

              return CBF_FORMAT;

            read = elsize - padding / 2;

            for (write = 0; write < read; write++)
            {
              if (direction < 0)

                c = (unsigned char) ((l >> ((read - write - 1) * 8)) & 0x0ff);

              else

                c = (unsigned char) ((l >> (write * 8)) & 0x0ff);

              if (outfile)

                cbf_failnez (cbf_put_character (outfile, c))

              if (digest)
              {
                buffer [bufsize] = (unsigned char) c;

                bufsize++;

                if (bufsize > 63)
                {
                  MD5Update (&context, buffer, 64);

                  bufsize = 0;
                }
              }
            }

            count += read;

            valcount = 0;

            padding = 0;
          }
    }
  }


    /* Get the digest */

  if (digest)
  {
    if (bufsize)

      MD5Update (&context, buffer, bufsize);

    MD5Final (rawdigest, &context);

    cbf_md5digest_to64 (digest, rawdigest);
  }


    /* Flush the buffer */

  if (outfile)

    cbf_failnez (cbf_flush_characters (outfile))


    /* Save the number of characters read */

  if (readsize)

    *readsize = count;


    /* Success */

  return 0;
}

#ifdef __cplusplus

}

#endif
