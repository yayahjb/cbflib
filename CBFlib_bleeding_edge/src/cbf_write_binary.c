/**********************************************************************
 * cbf_write_binary -- write binary sections                          *
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

#include "cbf.h"
#include "cbf_tree.h"
#include "cbf_compress.h"
#include "cbf_context.h"
#include "cbf_binary.h"
#include "cbf_codes.h"
#include "cbf_string.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#include "cbf_write_binary.h"


  /* Write a binary value */

int cbf_write_binary (cbf_node *column, unsigned int row,
                                        cbf_file *file,
                                        int isbuffer)
{
  cbf_file *infile;

  char digest [25], text [100];

  long start;

  size_t size;

  unsigned int compression;

  int id, bits, sign, type, checked_digest, elsize, realarray;
  
  const char *byteorder;
  
  size_t dimover, dimfast, dimmid, dimslow;
  
  size_t padding, ip;


    /* Check the arguments */

  if (!file)

    return CBF_ARGUMENT;

  if (((file->write_encoding & ENC_QP)     > 0) +
      ((file->write_encoding & ENC_BASE64) > 0) +
      ((file->write_encoding & ENC_BASE32K)> 0) +
      ((file->write_encoding & ENC_BASE8)  > 0) +
      ((file->write_encoding & ENC_BASE10) > 0) +
      ((file->write_encoding & ENC_BASE16) > 0) +
      ((file->write_encoding & ENC_NONE)   > 0) != 1)

    return CBF_ARGUMENT;

  if (!cbf_is_binary (column, row))

    return CBF_ARGUMENT;

  if (cbf_is_mimebinary (column, row))

    return CBF_ARGUMENT;


    /* Parse the value */

  cbf_failnez (cbf_get_bintext (column, row, &type, &id, &infile,
                                &start, &size, &checked_digest,
                                 digest, &bits, &sign, &realarray, 
                                 &byteorder, &dimover, &dimfast, &dimmid, &dimslow,
                                 &padding, &compression))

  if (padding == 0) {
  
    if (file->write_headers & PAD_1K ) padding=1023;
    if (file->write_headers & PAD_2K ) padding=2047;
    if (file->write_headers & PAD_4K ) padding=4095;
    
  }


    /* Position the file at the start of the binary section */

  cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))


    /* Calculate the digest if necessary */

  if (!cbf_is_base64digest (digest) && (file->write_headers & MSG_DIGEST))
  {


      /* Compute the message digest */

    cbf_failnez (cbf_md5digest (infile, size, digest))


      /* Go back to the start of the binary data */

    cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))


      /* Update the entry */

    checked_digest = 1;

    cbf_failnez (cbf_set_bintext (column, row, type,
                                  id, infile, start, size,
                                  checked_digest, digest, bits,
                                  sign,  realarray, 
                                  byteorder, dimover, dimfast, dimmid, dimslow,
                                  padding, compression))
  }


    /* Discard any bits in the buffers */

  
  infile->bits [0] = 0;
  infile->bits [1] = 0;



    /* Do we need MIME headers? */

  if (compression == CBF_NONE && (file->write_headers & MIME_NOHEADERS))

    return CBF_ARGUMENT;


    /* Write the header */

  cbf_failnez (cbf_write_string (file, "\n;\n"))


    /* MIME header? */

  if (file->write_headers & MIME_HEADERS)
  {
    cbf_failnez (cbf_write_string (file, "--CIF-BINARY-FORMAT-SECTION--\n"))

    if (compression == CBF_NONE)

      cbf_failnez (cbf_write_string (file,
                                "Content-Type: application/octet-stream\n"))

    else
    {
      cbf_failnez (cbf_write_string (file,
                                "Content-Type: application/octet-stream;\n"))

      switch (compression&CBF_COMPRESSION_MASK)
      {
        case CBF_PACKED:
                  	
          cbf_failnez (cbf_write_string (file,
                                "     conversions=\"x-CBF_PACKED\""))
                                
          if (compression&CBF_UNCORRELATED_SECTIONS) {
          
             cbf_failnez (cbf_write_string (file,
                                "; \"uncorrelated_sections\""))
          	
          }
          
          if (compression&CBF_FLAT_IMAGE) {
          
             cbf_failnez (cbf_write_string (file,
                                "; \"flat\""))
          	
          }

          cbf_failnez (cbf_write_string (file, "\n"))

          break;

        case CBF_PACKED_V2:
          
          cbf_failnez (cbf_write_string (file,
                                "     conversions=\"x-CBF_PACKED_V2\""))

          if (compression&CBF_UNCORRELATED_SECTIONS) {
          
             cbf_failnez (cbf_write_string (file,
                                "; \"uncorrelated_sections\""))
          	
          }
          
          if (compression&CBF_FLAT_IMAGE) {
          
             cbf_failnez (cbf_write_string (file,
                                "; \"flat\""))
          	
          }

          cbf_failnez (cbf_write_string (file, "\n"))

          break;

        case CBF_CANONICAL:

          cbf_failnez (cbf_write_string (file,
                                "     conversions=\"x-CBF_CANONICAL\"\n"))

          break;

        case CBF_BYTE_OFFSET:

          cbf_failnez (cbf_write_string (file,
                                "     conversions=\"x-CBF_BYTE_OFFSET\"\n"))

          break;

        case CBF_PREDICTOR:

          cbf_failnez (cbf_write_string (file,
                                "     conversions=\"x-CBF_PREDICTOR\"\n"))

          break;

        default:

          cbf_failnez (cbf_write_string (file,
                                "     conversions=\"x-CBF_UNKNOWN\"\n"))
      }
    }

    if (file->write_encoding & ENC_QP)

      cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: QUOTED-PRINTABLE\n"))

    else

      if (file->write_encoding & ENC_BASE64)

        cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: BASE64\n"))

      else

        if (file->write_encoding & ENC_BASE32K)
        
          cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: X-BASE32K\n"))
	
        else

          if (file->write_encoding & ENC_BASE8)

            cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: X-BASE8\n"))

          else

            if (file->write_encoding & ENC_BASE10)

              cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: X-BASE10\n"))

            else

              if (file->write_encoding & ENC_BASE16)

                cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: X-BASE16\n"))

              else

                cbf_failnez (cbf_write_string (file,
                      "Content-Transfer-Encoding: BINARY\n"))

    sprintf (text, "X-Binary-Size: %lu\n", (long)size);

    cbf_failnez (cbf_write_string (file, text))

    sprintf (text, "X-Binary-ID: %d\n", id);

    cbf_failnez (cbf_write_string (file, text))

    if (realarray) {

      sprintf (text, "X-Binary-Element-Type: \"signed %d-bit real IEEE\"\n",
                                                    bits);

    } else {

    if (sign)

      sprintf (text, "X-Binary-Element-Type: \"signed %d-bit integer\"\n",
                                                    bits);

    else

      sprintf (text, "X-Binary-Element-Type: \"unsigned %d-bit integer\"\n",
                                                    bits);

    }

    cbf_failnez (cbf_write_string (file, text))

    if ( !cbf_cistrncmp(byteorder,"big_endian",11) ) {

      sprintf (text, "X-Binary-Element-Byte-Order: %s\n", "BIG_ENDIAN");
      
      cbf_failnez (cbf_write_string (file, text))

    } else if ( !cbf_cistrncmp(byteorder,"little_endian",14) ) {
        
      sprintf (text, "X-Binary-Element-Byte-Order: %s\n", "LITTLE_ENDIAN");
      
      cbf_failnez (cbf_write_string (file, text))
    	
    } else return CBF_FORMAT;


      /* Save the digest if we have one */

    if (cbf_is_base64digest (digest))
    {
      sprintf (text, "Content-MD5: %24s\n", digest);

      cbf_failnez (cbf_write_string (file, text))
    }


    if (dimover > 0) {
    
      sprintf (text, "X-Binary-Number-of-Elements: %ld\n", (unsigned long)dimover);
    
      cbf_failnez (cbf_write_string (file, text))
    	
    }


    if (dimfast > 0) {
    
      sprintf (text, "X-Binary-Size-Fastest-Dimension: %ld\n", (unsigned long)dimfast);
    
      cbf_failnez (cbf_write_string (file, text))
    	
    }

    if (dimmid > 0) {
    
      sprintf (text, "X-Binary-Size-Second-Dimension: %ld\n", (unsigned long)dimmid);
    
      cbf_failnez (cbf_write_string (file, text))
    	
    } 

    
    if ((long)dimslow > 0) {
    
      sprintf (text, "X-Binary-Size-Third-Dimension: %ld\n", (unsigned long)dimslow);
    
      cbf_failnez (cbf_write_string (file, text))
    	
    } else if ((long)dimslow < 0 ) {

      sprintf (text, "X-Binary-Size-Third-Dimension: %ld\n", (unsigned long)(-(long)dimslow) );
    
      cbf_failnez (cbf_write_string (file, text))
    	
    }
    

    if (padding > 0) {
    
      sprintf (text, "X-Binary-Size-Padding: %ld\n", (unsigned long)padding);
    
      cbf_failnez (cbf_write_string (file, text))
    	
    }

    cbf_failnez (cbf_write_string (file, "\n"))
  }
  else

      /* Simple header */

    cbf_failnez (cbf_write_string (file, "START OF BINARY SECTION\n"))


    /* Copy the binary section to the output file */

  if (file->write_encoding & ENC_NONE)
  {
      /* Write the separators */

    cbf_failnez (cbf_put_character (file, 12))
    cbf_failnez (cbf_put_character (file, 26))
    cbf_failnez (cbf_put_character (file, 4))
    cbf_failnez (cbf_put_character (file, 213))


      /* Flush any bits in the buffers */

    cbf_failnez (cbf_flush_bits (file))


      /* If no MIME header, write the necessary data here */

    if ( !(file->write_headers & MIME_HEADERS) ) {


        /* Write the binary identifier (64 bits) */

      cbf_failnez (cbf_put_integer (file, id, 1, 64))


        /* Write the size of the binary section (64 bits) */

      cbf_failnez (cbf_put_integer (file, size, 0, 64))


        /* Write the compression type (64 bits) */

      cbf_failnez (cbf_put_integer (file, compression, 0, 64))
    }


      /* Get the current point in the new file */

    cbf_failnez (cbf_get_fileposition (file, &start))


      /* Copy the binary section to the output file */

    cbf_failnez (cbf_copy_file (file, infile, size))
    
      /* If padding requested, pad with zero */
      
    if (padding > 0)  {
    
      for (ip = 0; ip < 100; ip++) text[ip] = 0;
      
      for (ip = 0; ip < padding; ip+=100) {
      
        cbf_failnez(cbf_put_bits(file, (int *)text,CHAR_BIT*(ip+100<padding?100:padding-ip)))
      	
      }
    	
    }
  }
  else
  {
      /* Read the element size with no compression? */

    if (compression == CBF_NONE)
    {
      elsize = (bits + 4) / 8;

      if (elsize < 1 || elsize == 5)

        elsize = 4;

      else

        if (elsize == 7)

          elsize = 6;

        else

          if (elsize > 8)

            elsize = 8;
    }
    else

      elsize = 4;


      /* Go back to the start of the binary data */

    cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))


    if (file->write_encoding & ENC_QP)

      cbf_failnez (cbf_toqp (infile, file, size))

    else

      if (file->write_encoding & ENC_BASE64)

        cbf_failnez (cbf_tobase64 (infile, file, size))
		
      else 
      
        if(file->write_encoding & ENC_BASE32K)
	      
          cbf_failnez(cbf_tobase32k (infile, file, size))

        else

          if (file->write_encoding & ENC_BASE8)

            cbf_failnez (cbf_tobasex (infile, file, size, elsize, 8))

          else

            if (file->write_encoding & ENC_BASE10)

              cbf_failnez (cbf_tobasex (infile, file, size, elsize, 10))

            else

              cbf_failnez (cbf_tobasex (infile, file, size, elsize, 16))
  }


    /* Write the MIME footer */

  if (file->write_headers & MIME_HEADERS)

    cbf_failnez (cbf_write_string (file,
                               "\n--CIF-BINARY-FORMAT-SECTION----\n;\n"))

  else

    cbf_failnez (cbf_write_string (file, "\nEND OF BINARY SECTION\n;\n"))


    /* Flush the buffer */

  cbf_failnez (cbf_flush_characters (file))


    /* Replace a connection to a temporary file? */

  if (start  != 0               &&
      isbuffer                  &&
      type == CBF_TOKEN_TMP_BIN && (file->write_encoding & ENC_NONE))

    cbf_failnez (cbf_set_bintext (column, row, CBF_TOKEN_BIN,
                                  id, file, start, size, checked_digest,
                                  digest, bits, sign, realarray, 
                                  byteorder, dimover, dimfast, dimmid, dimslow, padding,
                                  compression))


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif
