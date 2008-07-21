/**********************************************************************
 * cbf_compress -- compression and decompression                      *
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
#include "cbf_file.h"
#include "cbf_compress.h"
#include "cbf_canonical.h"
#include "cbf_packed.h"
#include "cbf_byte_offset.h"
#include "cbf_predictor.h"
#include "cbf_uncompressed.h"


  /* Compress an array */

int cbf_compress (void         *source,
                  size_t        elsize,
                  int           elsign,
                  size_t        nelem,
                  unsigned int  compression,
                  cbf_file     *file,
                  size_t       *compressedsize,
                  int          *bits,
                  char         *digest,
                  int           realarray,
                  const char   *byteorder,
                  size_t        dimfast,
                  size_t        dimmid,
                  size_t        dimslow,
                  size_t        padding)
{
  int errorcode;

  size_t size;


    /* Discard any bits in the buffers */

  cbf_failnez (cbf_reset_bits (file))

  if (compressedsize)

    *compressedsize = 0;


    /* Start a digest? */

  if (digest)

    cbf_failnez (cbf_start_digest (file))


  errorcode = 0;

  size = 0;

  switch (compression&CBF_COMPRESSION_MASK)
  {
    case CBF_CANONICAL:

      errorcode = cbf_compress_canonical (source, elsize, elsign, nelem,
                                          compression, file,
                                          &size, bits, realarray,
                                          byteorder, dimfast, dimmid, dimslow, padding);
      break;

    case CBF_PACKED:
    case CBF_PACKED_V2:
    case 0:

      errorcode = cbf_compress_packed (source, elsize, elsign, nelem,
                                       compression, file,
                                       &size, bits, realarray,
                                       byteorder, dimfast, dimmid, dimslow, padding);
      break;

    case CBF_BYTE_OFFSET:

      errorcode = cbf_compress_byte_offset (source, elsize, elsign, nelem,
                                            compression, file,
                                            &size, bits, realarray,
                                            byteorder, dimfast, dimmid, dimslow, padding);
      break;

    case CBF_PREDICTOR:

      errorcode = cbf_compress_predictor (source, elsize, elsign, nelem,
                                          compression, file,
                                          &size, bits, realarray,
                                          byteorder, dimfast, dimmid, dimslow, padding);
      break;

    case CBF_NONE:

      errorcode = cbf_compress_none (source, elsize, elsign, nelem,
                                     compression, file,
                                     &size, bits, realarray,
                                     byteorder, dimfast, dimmid, dimslow, padding);
      break;

  default:

      errorcode = CBF_ARGUMENT;
  }


    /* Add the compressed size */

  if (compressedsize)

    *compressedsize += size;


    /* Flush the buffers */

  errorcode |= cbf_flush_bits (file);


    /* Get the digest? */

  if (digest)

     errorcode |= cbf_end_digest (file, digest);


    /* Done */

  return errorcode;
}


  /* Get the parameters of an array (read up to the start of the table) */

int cbf_decompress_parameters (int          *eltype,
                               size_t       *elsize,
                               int          *elsigned,
                               int          *elunsigned,
                               size_t       *nelem,
                               int          *minelem,
                               int          *maxelem,
                               unsigned int  compression,
                               cbf_file     *file)
{
  unsigned int nelem_file;

  int errorcode, minelement_file, maxelement_file,
                   elsigned_file, elunsigned_file;


    /* Discard any bits in the buffers */

  file->bits [0] = 0;
  file->bits [1] = 0;

   /* Check compression type */

  if (compression != CBF_CANONICAL   &&
      (compression&CBF_COMPRESSION_MASK) != CBF_PACKED      &&
      (compression&CBF_COMPRESSION_MASK) != CBF_PACKED_V2   &&
      compression != CBF_BYTE_OFFSET &&
      compression != CBF_PREDICTOR   &&
      compression != CBF_NONE)

    return CBF_FORMAT;

  if (compression == CBF_NONE || compression == CBF_BYTE_OFFSET )
  {
    nelem_file = 0;

    minelement_file = maxelement_file = 0;
  }
  else
  {
      /* Read the number of elements (64 bits) */

    cbf_failnez (cbf_get_integer (file, (int *) &nelem_file, 0, 64))


      /* Read the minimum element (64 bits) */

    errorcode = cbf_get_integer (file, &minelement_file, 1, 64);

    if (errorcode && errorcode != CBF_OVERFLOW)

      return errorcode;


      /* Read the maximum element (64 bits) */

    errorcode = cbf_get_integer (file, &maxelement_file, 1, 64);

    if (errorcode && errorcode != CBF_OVERFLOW)

      return errorcode;
  }


    /* Update the element sign, type, minimum, maximum and number */

  elsigned_file = !(((unsigned) minelement_file) <=
                    ((unsigned) maxelement_file) &&
                    ((signed)   minelement_file) >
                    ((signed)   maxelement_file));

  elunsigned_file = !(((signed)   minelement_file) <=
                      ((signed)   maxelement_file) &&
                      ((unsigned) minelement_file) >
                      ((unsigned) maxelement_file));

  if (elsigned)

    *elsigned = elsigned_file;

  if (elunsigned)

    *elunsigned = elunsigned_file;

  if (eltype)

    *eltype = CBF_INTEGER;

  if (elsize) {

      /* Calculate the minimum number of bytes needed to hold the elements */

    if (minelement_file == 0 && maxelement_file == 0) {

      *elsize = 0;

    } else {

      if ((!elsigned_file ||
          ((signed) minelement_file == (signed short) minelement_file &&
           (signed) maxelement_file == (signed short) maxelement_file)) ||
          (!elunsigned_file ||
          ((unsigned) minelement_file == (unsigned short) minelement_file &&
           (unsigned) maxelement_file == (unsigned short) maxelement_file))) {

        if ((!elsigned_file ||
            ((signed) minelement_file == (signed char) minelement_file &&
             (signed) maxelement_file == (signed char) maxelement_file)) ||
             (!elunsigned_file ||
            ((unsigned) minelement_file == (unsigned char) minelement_file &&
             (unsigned) maxelement_file == (unsigned char) maxelement_file))) {

          *elsize = sizeof (char);

        } else {

          *elsize = sizeof (short);

        }

      } else {

        *elsize = sizeof (int);

      }
    }
  }

  if (minelem)

    *minelem = minelement_file;

  if (maxelem)

    *maxelem = maxelement_file;

  if (nelem)

    *nelem = nelem_file;


    /* Success */

  return 0;
}


  /* Decompress an array (from the start of the table) */

int cbf_decompress (void         *destination,
                    size_t        elsize,
                    int           elsign,
                    size_t        nelem,
                    size_t       *nelem_read,
                    unsigned int  compression,
                    int           bits,
                    int           sign,
                    cbf_file     *file,
                    int           realarray,
                    const char   *byteorder,
                    size_t        dimover,
                    size_t        dimfast,
                    size_t        dimmid,
                    size_t        dimslow,
                    size_t        padding)
{
  switch (compression&CBF_COMPRESSION_MASK)
  {
    case CBF_CANONICAL:

      return cbf_decompress_canonical (destination, elsize, elsign, nelem,
                                       nelem_read, compression,
                                       bits, sign, file, realarray, byteorder,
                                       dimover, dimfast, dimmid, dimslow, padding);

    case CBF_PACKED:
    case CBF_PACKED_V2:
    case 0:

      return cbf_decompress_packed (destination, elsize, elsign, nelem,
                                    nelem_read, compression,
                                    bits, sign, file, realarray, byteorder,
                                    dimover, dimfast, dimmid, dimslow, padding);

    case CBF_BYTE_OFFSET:

      return cbf_decompress_byte_offset (destination, elsize, elsign, nelem,
                                         nelem_read, compression,
                                         bits, sign, file, realarray, byteorder,
                                         dimover, dimfast, dimmid, dimslow, padding);

    case CBF_PREDICTOR:

      return cbf_decompress_predictor (destination, elsize, elsign, nelem,
                                       nelem_read, compression,
                                       bits, sign, file, realarray, byteorder,
                                       dimover, dimfast, dimmid, dimslow, padding);

    case CBF_NONE:

      return cbf_decompress_none (destination, elsize, elsign, nelem,
                                  nelem_read, compression,
                                  bits, sign, file, realarray, byteorder,
                                  dimover, dimfast, dimmid, dimslow, padding);
  }


    /* Fail */

  return CBF_ARGUMENT;
}


#ifdef __cplusplus

}

#endif
