/**********************************************************************
 * cbf_simple -- cbflib simplified API functions                      *
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

#include <ctype.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_binary.h"
#include "cbf_simple.h"
#include "cbf_string.h"

#ifdef CBFLIB_MEM_DEBUG
extern size_t memory_allocated;
#endif


  /* Read a template file */

int cbf_read_template (cbf_handle handle, FILE *stream)
{
    /* Read the file */

  cbf_failnez (cbf_read_widefile (handle, stream, MSG_NODIGEST))


    /* Find the first datablock */

  cbf_failnez (cbf_select_datablock (handle, 0))

  return 0;
}


  /* Get the diffrn.id entry */

int cbf_get_diffrn_id (cbf_handle handle, const char **diffrn_id)
{
  cbf_failnez (cbf_find_category (handle, "diffrn"));
  cbf_failnez (cbf_find_column   (handle, "id"));
  cbf_failnez (cbf_get_value     (handle, diffrn_id))

  return 0;
}


  /* Get the diffrn.id entry, creating it if necessary */

int cbf_require_diffrn_id (cbf_handle handle, const char **diffrn_id, const char *default_id)
{
  cbf_failnez (cbf_require_category (handle, "diffrn"));
  cbf_failnez (cbf_require_column   (handle, "id"));
  cbf_failnez (cbf_require_value    (handle, diffrn_id, default_id))

  return 0;
}

  /* Change the diffrn.id entry in all the categories */

int cbf_set_diffrn_id (cbf_handle handle, const char *diffrn_id)
{
  int code;

  static char *categories [] = { "diffrn_source",
                                 "diffrn_radiation",
                                 "diffrn_detector",
                                 "diffrn_measurement",
                                 "diffrn_orient_matrix", 0 },
              **category;

  cbf_failnez (cbf_find_category (handle, "diffrn"))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_set_value     (handle, diffrn_id))

  for (category = categories; *category; category++)
  {
    code = cbf_find_category (handle, *category);

    if (code != CBF_NOTFOUND)
    {
      if (code)

        return code;

      cbf_failnez (cbf_find_column  (handle, "diffrn_id"))

      do

        cbf_failnez (cbf_set_value (handle, diffrn_id))

      while (cbf_next_row (handle));
    }
  }

  if (!cbf_find_category (handle, "cell")) {

    cbf_failnez (cbf_find_column  (handle, "entry_id"))

    cbf_failnez (cbf_set_value    (handle, diffrn_id))

  }

  return 0;
}


  /* Get the diffrn.crystal_id entry */

int cbf_get_crystal_id (cbf_handle handle, const char **crystal_id)
{
  cbf_failnez (cbf_find_category (handle, "diffrn"));
  cbf_failnez (cbf_find_column   (handle, "crystal_id"));
  cbf_failnez (cbf_get_value     (handle, crystal_id))

  return 0;
}


  /* Change the diffrn.crystal_id entry */

int cbf_set_crystal_id (cbf_handle handle, const char *crystal_id)
{
  cbf_failnez (cbf_find_category (handle, "diffrn"))
  cbf_failnez (cbf_find_column   (handle, "crystal_id"))
  cbf_failnez (cbf_set_value     (handle, crystal_id))

  return 0;
}


  /* Get the wavelength */

int cbf_get_wavelength (cbf_handle handle, double *wavelength)
{
  const char *diffrn_id, *wavelength_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Get the wavelength id */

  cbf_failnez (cbf_find_category (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column   (handle, "wavelength_id"))
  cbf_failnez (cbf_get_value     (handle, &wavelength_id))


    /* Get the wavelength */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation_wavelength"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, wavelength_id))
  cbf_failnez (cbf_find_column     (handle, "wavelength"))
  cbf_failnez (cbf_get_doublevalue (handle, wavelength))

  return 0;
}


  /* Set the wavelength */

int cbf_set_wavelength (cbf_handle handle, double wavelength)
{
    /* Get the wavelength id */

  const char *wavelength_id;

  cbf_failnez (cbf_find_category (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column   (handle, "wavelength_id"))
  cbf_failnez (cbf_get_value     (handle, &wavelength_id))


    /* Update the diffrn_radiation_wavelength category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation_wavelength"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, wavelength_id))
  cbf_failnez (cbf_find_column     (handle, "wavelength"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", wavelength))
  cbf_failnez (cbf_find_column     (handle, "wt"))
  cbf_failnez (cbf_set_value       (handle, "1.0"))

  return 0;
}


  /* Get the polarization */

int cbf_get_polarization (cbf_handle handle, double *polarizn_source_ratio,
                                             double *polarizn_source_norm)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Get the polarization */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_ratio"))
  cbf_failnez (cbf_get_doublevalue (handle, polarizn_source_ratio))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_norm"))
  cbf_failnez (cbf_get_doublevalue (handle, polarizn_source_norm))

  return 0;
}


  /* Set the polarization */

int cbf_set_polarization (cbf_handle handle, double polarizn_source_ratio,
                                             double polarizn_source_norm)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Update the diffrn_radiation category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_ratio"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", polarizn_source_ratio))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_norm"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", polarizn_source_norm))

  return 0;
}


  /* Get the divergence */

int cbf_get_divergence (cbf_handle handle, double *div_x_source,
                                           double *div_y_source,
                                           double *div_x_y_source)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Get the divergence */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "div_x_source"))
  cbf_failnez (cbf_get_doublevalue (handle, div_x_source))
  cbf_failnez (cbf_find_column     (handle, "div_y_source"))
  cbf_failnez (cbf_get_doublevalue (handle, div_y_source))
  cbf_failnez (cbf_find_column     (handle, "div_x_y_source"))
  cbf_failnez (cbf_get_doublevalue (handle, div_x_y_source))

  return 0;
}


  /* Set the divergence */

int cbf_set_divergence (cbf_handle handle, double div_x_source,
                                           double div_y_source,
                                           double div_x_y_source)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Update the diffrn_radiation category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "div_x_source"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", div_x_source))
  cbf_failnez (cbf_find_column     (handle, "div_y_source"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", div_y_source))
  cbf_failnez (cbf_find_column     (handle, "div_x_y_source"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", div_x_y_source))

  return 0;
}


    /* Get the number of elements */

int cbf_count_elements (cbf_handle handle, unsigned int *elements)
{
  const char *diffrn_id, *id;

  int errorcode;

  unsigned int count;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector_element"))
  cbf_failnez (cbf_find_column   (handle, "detector_id"))

  for (count = 0, errorcode = 0; !errorcode; count++)

    errorcode = cbf_find_nextrow (handle, id);

  count--;

  if (errorcode != CBF_NOTFOUND)

    return errorcode;

  if (elements)

    *elements = count;

  return 0;
}





  /* Get the element id */

int cbf_get_element_id (cbf_handle handle, unsigned int element_number,
                                           const char **element_id)
{
  const char *diffrn_id, *id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector_element"))
  cbf_failnez (cbf_find_column   (handle, "detector_id"))

  do

    cbf_failnez (cbf_find_nextrow (handle, id))

  while (element_number--);

  cbf_failnez (cbf_find_column (handle, "id"))
  cbf_failnez (cbf_get_value   (handle, element_id))

  return 0;
}

  /* Get the detector id */

int cbf_get_detector_id (cbf_handle handle, unsigned int element_number,
                                           const char **detector_id)
{
  const char *diffrn_id, *id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector_element"))
  cbf_failnez (cbf_find_column   (handle, "detector_id"))

  do

    cbf_failnez (cbf_find_nextrow (handle, id))

  while (element_number--);

  cbf_failnez (cbf_get_value   (handle, detector_id))

  return 0;
}

  /* Get the array id for a given detector element */

int cbf_get_array_id (cbf_handle handle, unsigned int element_number,
                                         const char **array_id)
{
  const char *element_id;

  cbf_failnez (cbf_get_element_id (handle, element_number, &element_id))
  if ( cbf_find_category  (handle, "diffrn_data_frame") ) {
      cbf_failnez (cbf_find_category  (handle, "diffrn_frame_data"))
  }
  cbf_failnez (cbf_find_column    (handle, "detector_element_id"))
  cbf_failnez (cbf_find_row       (handle, element_id))
  cbf_failnez (cbf_find_column    (handle, "array_id"))
  cbf_failnez (cbf_get_value      (handle, array_id))

  return 0;
}

  /* Get the pixel size of a detector element in a given direction 
     axis numbering is 1-based, fast to slow */

int cbf_get_pixel_size(cbf_handle handle, unsigned int element_number,
                                          int axis_number,
                                          double * psize)
{
  const char *array_id;
  int        aid, precedence, max_precedence, axis_index;

  cbf_failnez (cbf_get_array_id   (handle, element_number, &array_id))

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  precedence = max_precedence = axis_index = 0;

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 ) return CBF_FORMAT;
    if (precedence > max_precedence) max_precedence = precedence;

    if (precedence == axis_number) {
      cbf_failnez (cbf_find_column      (handle, "index"))
      cbf_failnez (cbf_get_integervalue (handle, &axis_index))
      if (axis_index < 1) return CBF_FORMAT;
    }
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }
  
  if (axis_index == 0 && axis_number < 0 ) {
    cbf_failnez (cbf_rewind_row (handle) )
    while (cbf_find_nextrow (handle, array_id) == 0) {
      cbf_failnez (cbf_find_column      (handle, "precedence"))
      cbf_failnez (cbf_get_integervalue (handle, &precedence))

      if (precedence == max_precedence+1+axis_number) {
        cbf_failnez (cbf_find_column      (handle, "index"))
        cbf_failnez (cbf_get_integervalue (handle, &axis_index))
        if (axis_index < 1) return CBF_FORMAT;
        break;
      }
      cbf_failnez (cbf_find_column (handle, "array_id"))
    }
  }

  if (axis_index == 0 ) return CBF_NOTFOUND;

  if ( cbf_find_category  (handle, "array_element_size") == 0 ) {
    cbf_failnez (cbf_rewind_row     (handle))
    cbf_failnez (cbf_find_column    (handle, "array_id"))

    while (!cbf_find_nextrow (handle, array_id)) {
      cbf_failnez (cbf_find_column        (handle, "index"))
      cbf_failnez (cbf_get_integervalue   (handle, &aid))
        if (aid == axis_index) {
          cbf_failnez (cbf_find_column       (handle, "size"))
          cbf_failnez (cbf_get_doublevalue(handle, psize))
          *psize *= 1.e3;
          return 0;
        }
      cbf_failnez (cbf_find_column    (handle, "array_id"))
    }
  }

  return CBF_NOTFOUND;

}

  /* Set the pixel size of a detector element in a given direction  
     axis numbering is 1-based, fast to slow */

int cbf_set_pixel_size(cbf_handle handle, unsigned int element_number,
                                          int axis_number,
                                          double psize)
{
  const char *array_id;
  int        aid, precedence, max_precedence, axis_index;

  cbf_failnez (cbf_get_array_id   (handle, element_number, &array_id))

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  precedence = max_precedence = axis_index = 0;

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 ) return CBF_FORMAT;
    if (precedence > max_precedence) max_precedence = precedence;

    if (precedence == axis_number) {
      cbf_failnez (cbf_find_column      (handle, "index"))
      cbf_failnez (cbf_get_integervalue (handle, &axis_index))
      if (axis_index < 1) return CBF_FORMAT;
    }
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (axis_index == 0 && axis_number < 0 ) {
    cbf_failnez (cbf_rewind_row (handle) )
    while (cbf_find_nextrow (handle, array_id) == 0) {
      cbf_failnez (cbf_find_column      (handle, "precedence"))
      cbf_failnez (cbf_get_integervalue (handle, &precedence))

      if (precedence == max_precedence+1+axis_number) {
        cbf_failnez (cbf_find_column      (handle, "index"))
        cbf_failnez (cbf_get_integervalue (handle, &axis_index))
        if (axis_index < 1) return CBF_FORMAT;
        break;
      }
      cbf_failnez (cbf_find_column (handle, "array_id"))
    }
  }

  if (axis_index == 0 ) return CBF_NOTFOUND;

  if ( cbf_find_category  (handle, "array_element_size") != 0 ) {

    cbf_failnez (cbf_new_category     (handle, "array_element_size" ))

    cbf_failnez (cbf_new_column       (handle, "array_id" ))
    cbf_failnez (cbf_set_value        (handle, array_id ))

    cbf_failnez (cbf_new_column       (handle, "index" ))
    cbf_failnez (cbf_set_integervalue (handle,  axis_index ))

    cbf_failnez (cbf_new_column       (handle, "size" ))
    cbf_failnez (cbf_set_doublevalue  (handle, "%-.15g", psize*1.e-3))

    return 0;

  } else {

    cbf_failnez (cbf_rewind_row     (handle))
    cbf_failnez (cbf_find_column    (handle, "array_id"))

    while (!cbf_find_nextrow (handle, array_id)) {
      cbf_failnez (cbf_find_column    (handle, "index"))
      cbf_failnez (cbf_get_integervalue      (handle, &aid))
        if (aid == axis_index) {
          cbf_failnez (cbf_find_column       (handle, "size"))
          cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", psize*1.e-3))
          return 0;
        }
      cbf_failnez (cbf_find_column    (handle, "array_id"))
    }
  }

  cbf_failnez (cbf_new_row            (handle))

  cbf_failnez (cbf_find_column        (handle, "array_id" ))
  cbf_failnez (cbf_set_value          (handle, array_id ))

  cbf_failnez (cbf_find_column        (handle, "index" ))
  cbf_failnez (cbf_set_integervalue   (handle, (int)axis_index ))

  cbf_failnez (cbf_find_column        (handle, "size" ))
  cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", psize*1.e-3 ))

  return 0;

}


  /* Get the bin sizes of a detector element */

int cbf_get_bin_sizes(cbf_handle handle, unsigned int element_number,
                                          double * slowbinsize,
                                          double * fastbinsize)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id   (handle, element_number, &array_id))

  
      /* Update the array_intensities category */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "pixel_slow_bin_size"))
  cbf_failnez (cbf_get_doublevalue (handle, slowbinsize ))
  cbf_failnez (cbf_find_column     (handle, "pixel_fast_bin_size"))
  cbf_failnez (cbf_get_doublevalue (handle, fastbinsize ))


  return 0;

}


  /* Set the bin sizes of a detector element */

int cbf_set_bin_sizes(cbf_handle handle, unsigned int element_number,
                                          double slowbinsize,
                                          double fastbinsize)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id   (handle, element_number, &array_id))

  
      /* Update the array_intensities category */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_require_column  (handle, "array_id"))
  cbf_failnez (cbf_require_row     (handle, array_id))
  cbf_failnez (cbf_require_column  (handle, "pixel_slow_bin_size"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", slowbinsize ))
  cbf_failnez (cbf_require_column  (handle, "pixel_fast_bin_size"))
  cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", fastbinsize ))


  return 0;

}


  /* Get the gain of a detector element */

int cbf_get_gain (cbf_handle handle, unsigned int element_number,
                                     double *gain, double *gain_esd)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Get the gain */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "gain"))
  cbf_failnez (cbf_get_doublevalue (handle, gain))
  cbf_failnez (cbf_find_column     (handle, "gain_esd"))
  cbf_failnez (cbf_get_doublevalue (handle, gain_esd))

  return 0;
}


  /* Set the gain of a detector element */

int cbf_set_gain (cbf_handle handle, unsigned int element_number,
                                     double gain, double gain_esd)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Update the array_intensities category */

  cbf_failnez (cbf_require_category   (handle, "array_intensities"))
  cbf_failnez (cbf_require_column     (handle, "array_id"))
  cbf_failnez (cbf_require_row        (handle, array_id))
  cbf_failnez (cbf_require_column     (handle, "gain"))
  cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", gain))
  cbf_failnez (cbf_require_column     (handle, "gain_esd"))
  cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", gain_esd))

  return 0;
}


  /* Get the overload value of a detector element */

int cbf_get_overload (cbf_handle handle, unsigned int element_number,
                                         double *overload)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Get the overload value */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "overload"))
  cbf_failnez (cbf_get_doublevalue (handle, overload))

  return 0;
}


  /* Set the overload value of a detector element */

int cbf_set_overload (cbf_handle handle, unsigned int element_number,
                                         double overload)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Update the array_intensities category */

  cbf_failnez (cbf_require_category   (handle, "array_intensities"))
  cbf_failnez (cbf_require_column     (handle, "array_id"))
  cbf_failnez (cbf_require_row        (handle, array_id))
  cbf_failnez (cbf_require_column     (handle, "overload"))
  cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", overload))

  return 0;
}


  /* Get the integration time */

int cbf_get_integration_time (cbf_handle    handle,
                              unsigned int  reserved,
                              double       *time)
{
  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Update the diffrn_scan_frame category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_find_column     (handle, "integration_time"))
  cbf_failnez (cbf_rewind_row      (handle))
  cbf_failnez (cbf_get_doublevalue (handle, time))

  return 0;
}


  /* Set the integration time */

int cbf_set_integration_time (cbf_handle   handle,
                              unsigned int reserved,
                              double       time)
{
  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Update the diffrn_scan_frame category */

  cbf_failnez (cbf_require_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_require_column     (handle, "integration_time"))
  cbf_failnez (cbf_rewind_row         (handle))
  cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", time))

  return 0;
}


  /* Convert gregorian to julian date (in days) */

double cbf_gregorian_julian (int    year,
                             int    month,
                             int    day,
                             int    hour,
                             int    minute,
                             double second)
{
  static int days [] = {   0,  31,  59,  90, 120, 151,
                     181, 212, 243, 273, 304, 334, 365 };

  second += minute * 60.0 + hour * 3600.0 + (day - 1) * 86400.0;

  second += days [month - 1] * 86400.0;

  if (month > 2 && (year % 4) == 0 && year != 1900 && year != 2100)

    second += 86400.0;

  second += ((365 * (year - 1)) + floor ((year - 1) / 4)
                                - floor ((year - 1) / 100)
                                + floor ((year - 1) / 400)) * 86400.0;

  return second / 86400.0 + 1721425.5;
}



  /* Get the collection date and time (1) as seconds since January 1 1970 */

int cbf_get_timestamp (cbf_handle handle, unsigned int  reserved,
                                          double       *time,
                                          int          *timezone)
{
  int year, month, day, hour, minute;

  double second;

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_get_datestamp (handle, reserved, &year, &month, &day,
                                  &hour, &minute, &second, timezone))

  if (time)

    *time = (cbf_gregorian_julian (year, month, day,
                                       hour, minute, second)
             - 2440587.5) * 86400.0;

  return 0;
}


  /* Get the collection date and time (2) as individual fields */

int cbf_get_datestamp (cbf_handle handle, unsigned int  reserved,
                                          int          *year,
                                          int          *month,
                                          int          *day,
                                          int          *hour,
                                          int          *minute,
                                          double       *second,
                                          int          *timezone)
{
  const char *date;

  char ftzsign;

  int fyear, fmonth, fday, fhour, fminute, ftzhour, ftzminute, parsed;

  double fsecond;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Read the diffrn_scan_frame category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_find_column     (handle, "date"))
  cbf_failnez (cbf_rewind_row      (handle))
  cbf_failnez (cbf_get_value       (handle, &date))


    /* Parse the string */

  fsecond =
  fyear   =
  fmonth  =
  fday    =
  fhour   =
  fminute =
  ftzsign =
  ftzhour =
  ftzminute = 0;

  parsed = sscanf (date, "%d-%d-%d%*c%d:%d:%lf%c%d:%d",
                                     &fyear, &fmonth, &fday, &fhour,
                                     &fminute, &fsecond,
                                     &ftzsign, &ftzhour, &ftzminute);

  if (parsed < 3 || (parsed == 7 && strchr (" \t\n", ftzsign) == NULL)
                 || (parsed >  7 && strchr ("+-",    ftzsign) == NULL))

    return CBF_FORMAT;

  if (fyear < 0 || fyear > 9999
                || fmonth < 1
                || fmonth > 12
                || fday < 1
                || fday > 31
                || fhour < 0
                || fhour > 23
                || fminute < 0
                || fminute > 59
                || fsecond < 0
                || fsecond >= 60
                || ftzhour < 0
                || ftzhour > 13
                || ftzminute < 0
                || ftzminute > 59)

    return CBF_FORMAT;

  if (year)     *year     = fyear;
  if (month)    *month    = fmonth;
  if (day)      *day      = fday;
  if (hour)     *hour     = fhour;
  if (minute)   *minute   = fminute;
  if (second)   *second   = fsecond;

  if (timezone) {

    if (parsed > 7)
    {
      *timezone = ftzhour * 60 + ftzminute;

      if (ftzsign == '-')

        *timezone = -*timezone;
    }
    else

      *timezone = CBF_NOTIMEZONE;

  }

  return 0;
}


  /* Set the collection date and time (1) as seconds since January 1 1970 */

int cbf_set_timestamp (cbf_handle handle, unsigned int reserved,
                                          double       time,
                                          int          timezone,
                                          double       precision)
{
  int month, monthstep, year, day, hour, minute;

  double second, date;

  if (reserved != 0)

    return CBF_ARGUMENT;


  date = time / 86400.0 + 2440587.5;

  if (date < 1721060.5 || date > 5373484.5)

    return CBF_ARGUMENT;


   /* Find the year and month with a binary search */

  for (monthstep = 65536, month = 0; monthstep; monthstep >>= 1)
  {
    month += monthstep;

    if (cbf_gregorian_julian (month / 12,
                             (month % 12) + 1, 1, 0, 0, 0) > date)

     month -= monthstep;
  }


    /* Calculate the day, hour, minute and second */

  year  =  month / 12;
  month = (month % 12) + 1;

  date -= cbf_gregorian_julian (year, month, 1, 0, 0, 0);

  day = (int) floor (date) + 1;

  date -= (day - 1);

  hour = (int) floor (date * 24.0);

  date -= hour / 24.0;

  minute = (int) floor (date * 1440.0);

  date -= minute / 1440.0;

  second = date * 86400.0;


    /* Set the new date */

  cbf_failnez (cbf_set_datestamp (handle, reserved, year, month, day,
                                     hour, minute, second,
                                     timezone, precision))

  return 0;
}


  /* Set the collection date and time (2) as individual fields */

int cbf_set_datestamp (cbf_handle handle, unsigned int reserved,
                                          int          year,
                                          int          month,
                                          int          day,
                                          int          hour,
                                          int          minute,
                                          double       second,
                                          int          timezone,
                                          double       precision)
{
  char date [256];

  int nsf;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Print the date in CIF format */

  if (year < 0 || year > 9999
               || month < 1
               || month > 12
               || day < 1
               || day > 31
               || hour < 0
               || hour > 23
               || minute < 0
               || minute > 59
               || second < 0
               || second >= 60)

    return CBF_ARGUMENT;

  if (timezone != CBF_NOTIMEZONE)

    if (timezone < -780 || timezone > 780)

      return CBF_ARGUMENT;


  nsf = 0;

  if (precision > 0 && precision < 1)

    nsf = (int) (-log10 (precision) + 0.5);

  sprintf (date, "%04d-%02d-%02dT%02d:%02d:%0*.*f", year, month, day,
                   hour, minute, nsf == 0 ? 2 : nsf + 3, nsf, second);

  if (timezone != CBF_NOTIMEZONE)

    sprintf (date + strlen (date), "%c%02d:%02d", timezone < 0 ? '-' : '+',
                                                  abs (timezone) / 60,
                                                  abs (timezone) % 60);


    /* Update the diffrn_scan_frame category */

  cbf_failnez (cbf_require_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_require_column     (handle, "date"))
  cbf_failnez (cbf_rewind_row         (handle))
  cbf_failnez (cbf_set_value          (handle, date))

  return 0;
}


  /* Set the collection date and time (3) as current time to the second */

int cbf_set_current_timestamp (cbf_handle handle, unsigned int reserved,
                                                  int timezone)
{
  time_t timer;

  if (reserved != 0)

    return CBF_ARGUMENT;

  timer = time (NULL);

  if (timezone != CBF_NOTIMEZONE)

    timer += timezone * 60;

  cbf_failnez (cbf_set_timestamp (handle, reserved, timer, timezone, 1))

  return 0;
}


  /* Get the image size.  ndimslow is the slow dimension, ndimfast is fast. */

int cbf_get_image_size (cbf_handle    handle,
                        unsigned int  reserved,
                        unsigned int  element_number,
                        size_t       *ndimslow,
                        size_t       *ndimfast)
{
  const char *array_id;
  
  size_t ndim0;
    
  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_get_3d_array_size (handle, reserved, array_id, &ndim0, ndimslow,  ndimfast));
  
  if (ndim0 != 1) return CBF_ARGUMENT;

  return 0;
}


  /* Read a binary section into an image.  ndimslow is the 
                           slow dimension, ndimfast is fast.*/

int cbf_get_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id;
  
  binary_id = 1;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, &binary_id, array,
                   CBF_INTEGER, elsize, elsign, 1, ndimslow, ndimfast));

 
  return 0;
}


  /* Read a binary section into a real image.  ndimslow is the 
                            slow dimension, ndimfast is fast.  */

int cbf_get_real_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id;
  
  binary_id = 1;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, &binary_id, array,
                   CBF_FLOAT, elsize, 1, 1, ndimslow, ndimfast));

  return 0;
}


  /* Get the 3D image size. ndimslow is the slowest dimension, 
                            ndimmid is the next faster dimension,
                            ndimfast is the fastest dimension */

int cbf_get_3d_image_size (cbf_handle    handle,
                        unsigned int  reserved,
                        unsigned int  element_number,
                        size_t       *ndimslow,
                        size_t       *ndimmid,
                        size_t       *ndimfast)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_get_3d_array_size (handle, reserved, array_id,
                   ndimslow, ndimmid, ndimfast));
                   
  return 0;
}


  /* Read a 3D binary section into an image.  
                       ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension */

int cbf_get_3d_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, &binary_id, array,
                   CBF_INTEGER, elsize, elsign, ndimslow, ndimmid, ndimfast));

  return 0;
}



  /* Read a 3D binary section into a real image.  
                       ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension */

int cbf_get_real_3d_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, &binary_id, array,
                   CBF_FLOAT, elsize, 1, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Save an image.  ndimslow is the slow dimension, ndimfast is fast. */

int cbf_set_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id=1;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));
  
  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, &binary_id, compression,
                   array, CBF_INTEGER, elsize, elsign, 1, ndimslow, ndimfast));

  return 0;
}


  /* Save a real image.  ndimslow is the slow dimension, ndimfast is fast. */

int cbf_set_real_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id = 1;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, &binary_id, compression,
                   array, CBF_FLOAT, elsize, 1, 1, ndimslow, ndimfast));

  return 0;
}


  /* Save a 3D image.  ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension. */


int cbf_set_3d_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id = 1;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));

  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, &binary_id, compression,
                   array, CBF_INTEGER, elsize, elsign, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Save a real 3D image.  
                       ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension */

int cbf_set_real_3d_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  int binary_id = 1;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));
  
  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, &binary_id,compression,
                   array, CBF_FLOAT, elsize, 1, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Get the array_id for a map segment or map segment mask.
                       ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension. */

int cbf_get_map_array_id (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *segment_id,
                   const char  **array_id,
                   int           ismask,
                   int           require,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  
  if (require) {
   
    cbf_failnez (cbf_require_category (handle, "map_segment"));
  
    cbf_failnez (cbf_require_column (handle, "id"));
    
  } else  {

    cbf_failnez (cbf_find_category (handle, "map_segment"));
  
    cbf_failnez (cbf_find_column (handle, "id"));
  	
  }
  
  if (cbf_find_row(handle,segment_id)) {
  
    if (!require) return CBF_NOTFOUND;

    cbf_failnez(cbf_new_row(handle));
    
    cbf_failnez(cbf_set_value(handle,segment_id));
  	
  }
  
  if (ismask) {
  	cbf_failnez( cbf_require_column (handle, "mask_array_id") )
  } else  {
  	cbf_failnez( cbf_require_column (handle, "array_id") )  	
  }
  
  if (cbf_get_value (handle, array_id) || !*array_id || strlen(*array_id)==0) {
  
    if (!require) return CBF_NOTFOUND;
  
    /* If no array structure has been defined, use the segment_id */
  
    cbf_failnez(cbf_set_value(handle,segment_id));
    
    cbf_failnez(cbf_require_category(handle, "axis" ) );
    
    cbf_failnez (cbf_require_column(handle,"system"))
    cbf_failnez (cbf_require_column(handle,"vector[1]"))
    cbf_failnez (cbf_require_column(handle,"vector[2]"))
    cbf_failnez (cbf_require_column(handle,"vector[3]"))    
    cbf_failnez (cbf_require_column(handle, "id" ))
    if (cbf_find_row(handle,"CELL_A_AXIS") ) {
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_set_value(handle, "CELL_A_AXIS"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "system"))
      cbf_failnez (cbf_set_value(handle, "fractional"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "vector[1]"))
      cbf_failnez (cbf_set_integervalue(handle, 1))
      cbf_failnez (cbf_find_column(handle, "vector[2]"))
      cbf_failnez (cbf_set_integervalue(handle, 0))
      cbf_failnez (cbf_find_column(handle, "vector[3]"))
      cbf_failnez (cbf_set_integervalue(handle, 0))
      cbf_failnez (cbf_find_column(handle, "id"))
    }
  	if (cbf_find_row(handle,"CELL_B_AXIS") ) {
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_set_value(handle, "CELL_B_AXIS"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "system"))
      cbf_failnez (cbf_set_value(handle, "fractional"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "vector[1]"))
      cbf_failnez (cbf_set_integervalue(handle, 0))
      cbf_failnez (cbf_find_column(handle, "vector[2]"))
      cbf_failnez (cbf_set_integervalue(handle, 1))
      cbf_failnez (cbf_find_column(handle, "vector[3]"))
      cbf_failnez (cbf_set_integervalue(handle, 0))
      cbf_failnez (cbf_find_column(handle, "id"))
    }
    if (cbf_find_row(handle,"CELL_C_AXIS") ) {
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_set_value(handle, "CELL_C_AXIS"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "system"))
      cbf_failnez (cbf_set_value(handle, "fractional"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "vector[1]"))
      cbf_failnez (cbf_set_integervalue(handle, 0))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "vector[2]"))
      cbf_failnez (cbf_set_integervalue(handle, 0))
      cbf_failnez (cbf_find_column(handle, "vector[3]"))
      cbf_failnez (cbf_set_integervalue(handle, 1))
    }
    
    cbf_failnez(cbf_require_category(handle, "array_structure_list_axis" ) );
    cbf_failnez (cbf_require_column(handle,"array_id"))
    cbf_failnez (cbf_require_column(handle,"index"))
    cbf_failnez (cbf_require_column(handle,"dimension"))
    cbf_failnez (cbf_require_column(handle,"precedence"))
    cbf_failnez (cbf_require_column(handle,"direction"))
    cbf_failnez (cbf_require_column(handle,"axis_id"))
    
    if (cbf_find_row(handle,"CELL_A_AXIS")){
      cbf_failnez (cbf_new_row(handle));
      cbf_failnez (cbf_find_column(handle, "array_id"))
      cbf_failnez (cbf_set_value(handle, segment_id))
      cbf_failnez (cbf_find_column(handle, "index"))
      cbf_failnez (cbf_set_integervalue(handle, 1))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "dimension"))
      cbf_failnez (cbf_set_integervalue(handle, ndimfast))
      cbf_failnez (cbf_find_column(handle, "precedence"))
      cbf_failnez (cbf_set_integervalue(handle, 1))
      cbf_failnez (cbf_find_column(handle, "direction"))
      cbf_failnez (cbf_set_value(handle, "increasing"))
      cbf_failnez (cbf_find_column(handle, "axis_id"))
      cbf_failnez (cbf_set_value(handle, "CELL_A_AXIS"))
    }
    if (cbf_find_row(handle,"CELL_B_AXIS")){
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_find_column(handle, "array_id"))
      cbf_failnez (cbf_set_value(handle, segment_id))
      cbf_failnez (cbf_find_column(handle, "index"))
      cbf_failnez (cbf_set_integervalue(handle, 2))
      cbf_failnez (cbf_find_column(handle, "dimension"))
      cbf_failnez (cbf_set_integervalue(handle, ndimmid))
      cbf_failnez (cbf_find_column(handle, "precedence"))
      cbf_failnez (cbf_set_integervalue(handle, 2))
      cbf_failnez (cbf_find_column(handle, "direction"))
      cbf_failnez (cbf_set_value(handle, "increasing"))
      cbf_failnez (cbf_find_column(handle, "axis_id"))
      cbf_failnez (cbf_set_value(handle, "CELL_B_AXIS"))
    }
    if (cbf_find_row(handle,"CELL_C_AXIS")){
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_find_column(handle, "array_id"))
      cbf_failnez (cbf_set_value(handle, segment_id))
      cbf_failnez (cbf_find_column(handle, "index"))
      cbf_failnez (cbf_set_integervalue(handle, 3))
      cbf_failnez (cbf_find_column(handle, "dimension"))
      cbf_failnez (cbf_set_integervalue(handle, ndimslow))
      cbf_failnez (cbf_find_column(handle, "precedence"))
      cbf_failnez (cbf_set_integervalue(handle, 3))
      cbf_failnez (cbf_find_column(handle, "direction"))
      cbf_failnez (cbf_set_value(handle, "increasing"))
      cbf_failnez (cbf_find_column(handle, "axis_id"))
      cbf_failnez (cbf_set_value(handle, "CELL_C_AXIS"))      
    }
    
    cbf_failnez (cbf_require_category(handle,"array_structure_list_axis"))
    cbf_failnez (cbf_require_column(handle,"fract_displacement"))
    cbf_failnez (cbf_require_column(handle,"fract_displacement_increment"))
    cbf_failnez (cbf_require_column(handle,"axis_id"))
    if (cbf_find_row(handle,"CELL_A_AXIS")) {
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_set_value(handle, "CELL_A_AXIS"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "fract_displacement"))
      cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", (double)1./(double)(ndimfast*2)))
      cbf_failnez (cbf_find_column(handle, "fract_displacement_increment"))
      cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", (double)1./(double)(ndimfast)))
      cbf_failnez (cbf_find_column(handle, "axis_id"))
    }
    if (cbf_find_row(handle,"CELL_B_AXIS")) {
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_set_value(handle, "CELL_A_AXIS"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "fract_displacement"))
      cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", (double)1./(double)(ndimmid*2)))
      cbf_failnez (cbf_find_column(handle, "fract_displacement_increment"))
      cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", (double)1./(double)(ndimmid)))
      cbf_failnez (cbf_find_column(handle, "axis_id"))
    }
    if (cbf_find_row(handle,"CELL_C_AXIS")) {
      cbf_failnez (cbf_new_row(handle))
      cbf_failnez (cbf_set_value(handle, "CELL_A_AXIS"))
      cbf_failnez (cbf_set_typeofvalue(handle, "word"))
      cbf_failnez (cbf_find_column(handle, "fract_displacement"))
      cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", (double)1./(double)(ndimslow*2)))
      cbf_failnez (cbf_find_column(handle, "fract_displacement_increment"))
      cbf_failnez (cbf_set_doublevalue(handle, "%-.15g", (double)1./(double)(ndimslow)))
      cbf_failnez (cbf_find_column(handle, "axis_id"))
    }
  } else {
  	*array_id = segment_id;
  }
  return 0;	
}

  /* Get the map segment size.   ndimslow is the slowest dimension, 
                                 ndimmid is the next faster dimension,
                                 ndimfast is the fastest dimension */

int cbf_get_map_segment_size (cbf_handle    handle,
                        unsigned int  reserved,
                        const char   *segment_id,
                        int          *binary_id,
                        size_t       *ndimslow,
                        size_t       *ndimmid,
                        size_t       *ndimfast)
{
  const char *array_id;
  
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             0, 0, *ndimslow, *ndimmid,  *ndimfast) )

  cbf_failnez (cbf_get_3d_array_size (handle, reserved, array_id, ndimslow, ndimmid, ndimfast));
  
  return 0;
}


  /* Read a map segment.  ndimslow is the slowest dimension, 
                          ndimmid is the next faster dimension,
                          ndimfast is the fastest dimension */
int cbf_get_map_segment (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *segment_id,
                   int          *binary_id,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             0, 0, ndimslow, ndimmid, ndimfast) )

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, binary_id, array,
                   CBF_INTEGER, elsize, elsign, ndimslow, ndimmid, ndimfast));

 
  return 0;
}

  /* Read a map segment mask.  ndimslow is the slowest dimension, 
                               ndimmid is the next faster dimension,
                               ndimfast is the fastest dimension */
int cbf_get_map_segment_mask (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *segment_id,
                   int          *binary_id,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             1, 0, ndimslow, ndimmid, ndimfast) )

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, binary_id, array,
                   CBF_INTEGER, elsize, elsign, ndimslow, ndimmid, ndimfast));

 
  return 0;
}


  /* Read a real map segment.  ndimslow is the slowest dimension, 
                               ndimmid is the next faster dimension,
                               ndimfast is the fastest dimension */

int cbf_get_real_map_segment (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *segment_id,
                   int          *binary_id,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             0, 0, ndimslow, ndimmid, ndimfast) )

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, binary_id, array,
                   CBF_FLOAT, elsize, 1, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Read a real map segment mask.  ndimslow is the slowest dimension, 
                               ndimmid is the next faster dimension,
                               ndimfast is the fastest dimension */
int cbf_get_real_map_segment_mask (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *segment_id,
                   int          *binary_id,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             1, 0, ndimslow, ndimmid, ndimfast) )

  cbf_failnez (cbf_get_3d_array (handle, reserved, array_id, binary_id, array,
                   CBF_FLOAT, elsize, 1, ndimslow, ndimmid, ndimfast));

 
  return 0;
}


  /* Save a map segment.  ndimslow is the slowest dimension, 
                          ndimmid is the next faster dimension,
                          ndimfast is the fastest dimension */


int cbf_set_map_segment (cbf_handle    handle,
                   unsigned int  reserved,
                   const char    *segment_id,
                   int           *binary_id,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             0, 1, ndimslow, ndimmid,  ndimfast) )

  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, binary_id, compression,
                   array, CBF_INTEGER, elsize, elsign, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Save a map segment mask.  ndimslow is the slowest dimension, 
                               ndimmid is the next faster dimension,
                               ndimfast is the fastest dimension */

int cbf_set_map_segment_mask (cbf_handle    handle,
                   unsigned int  reserved,
                   const char    *segment_id,
                   int           *binary_id,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             1, 1, ndimslow, ndimmid,  ndimfast) )

  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, binary_id, compression,
                   array, CBF_INTEGER, elsize, elsign, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Save a real map segment.  ndimslow is the slowest dimension, 
                               ndimmid is the next faster dimension,
                               ndimfast is the fastest dimension */

int cbf_set_real_map_segment (cbf_handle    handle,
                   unsigned int  reserved,
                   const char    *segment_id,
                   int           *binary_id,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;

  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             0, 1, ndimslow, ndimmid,  ndimfast) )

  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, binary_id, compression,
                   array, CBF_FLOAT, elsize, 1, ndimslow, ndimmid, ndimfast));

  return 0;
}


  /* Save a real map segment mask.  ndimslow is the slowest dimension, 
                                    ndimmid is the next faster dimension,
                                    ndimfast is the fastest dimension */


int cbf_set_real_map_segment_mask (cbf_handle    handle,
                   unsigned int  reserved,
                   const char    *segment_id,
                   int           *binary_id,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *array_id;
  
  cbf_failnez (cbf_get_map_array_id (handle, reserved, segment_id, &array_id,
                                             1, 1, ndimslow, ndimmid,  ndimfast) )

  cbf_failnez (cbf_set_3d_array(handle, reserved, array_id, binary_id, compression,
                   array, CBF_FLOAT, elsize, 1, ndimslow, ndimmid, ndimfast));

  return 0;
}



  /* Get the 3D array size. ndimslow is the slowest dimension, 
                            ndimmid is the next faster dimension,
                            ndimfast is the fastest dimension */

int cbf_get_3d_array_size (cbf_handle    handle,
                        unsigned int  reserved,
                        const char   *array_id,
                        size_t       *ndimslow,
                        size_t       *ndimmid,
                        size_t       *ndimfast)
{

  int done [4], precedence, dimension [4], kdim[4];

  if (reserved != 0)

    return CBF_ARGUMENT;

    /* Get the dimensions from the array_structure_list category */

  done [1] = done [2] = done [3] = 0;

  dimension [1] = dimension [2] = dimension [3] = 1;

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 3)

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column      (handle, "dimension"))
    cbf_failnez (cbf_get_integervalue (handle, &dimension [precedence]))

    if (done [precedence])

      return CBF_FORMAT;

    done [precedence] = 1;

    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!done [1])

    return CBF_NOTFOUND;

  if (!done [2])
  {
    
    kdim [3] = dimension [1];
    
    kdim [2] = 1;
    
    kdim [1] = 1;

  }
  else
  {
    kdim [1] = 1;
    
    kdim [2] = dimension [2];
    
    kdim [3] = dimension [1];
  }
  
  if (done[3])
  {
    kdim [1] = dimension[3];
  }

  if (ndimslow)

      *ndimslow = kdim [1];

  if (ndimmid)

      *ndimmid = kdim [2];

  if (ndimfast)

      *ndimfast = kdim [3];

  return 0;
}

  /* Read a 3D array.  
                       ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension */

int cbf_get_3d_array (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *array_id,
                   int          *binary_id,
                   void         *array,
                   int           eltype,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{
  const char *direction_string;

  int code, done [4], precedence, direction [4], local_binary_id, 
      dir1=1, dir2=1, dir3=1,
      index1, index2, index3,
      start1, end1, inc1, 
      start2, end2, inc2,
      start3, end3, inc3;

  size_t nelem_read, dimslow, dimmid, dimfast;

  char tmp [32], *pixel, *pixel2;

  if (reserved != 0)

    return CBF_ARGUMENT;
    
  if ( eltype != CBF_FLOAT && eltype != CBF_INTEGER)
  
    return CBF_ARGUMENT;

  if ( eltype == CBF_FLOAT && elsize != 4 && elsize != 8 )

    return CBF_ARGUMENT;
    
  if ( eltype == CBF_FLOAT && !elsign)
  
    return CBF_ARGUMENT;


    /* Get the index dimensions */

  cbf_failnez (cbf_get_3d_array_size (handle, reserved, array_id,
                                   &dimslow, &dimmid, &dimfast))


    /* Check that the fast dimensions correspond */

  if (dimmid != ndimmid || dimfast != ndimfast)

    return CBF_ARGUMENT;


    /* Get the index directions from the array_structure_list category */

  done [1] = done [2] = done[3] = 0;

  direction [1] = direction [2] = direction [3] = 1;

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 3)

      return CBF_FORMAT;

    code = cbf_find_column (handle, "direction");

    if (code == 0)
    {
      cbf_failnez (cbf_get_value (handle, &direction_string))

      if (cbf_cistrcmp ("decreasing", direction_string) == 0)

        direction [precedence] = -1;
    }
    else

      if (code != CBF_NOTFOUND)

        return code;

    if (done [precedence])

      return CBF_FORMAT;

    done [precedence] = 1;

    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!done [1])

    return CBF_NOTFOUND;

  if (!done [2])
  {
    dir1 = direction [1];

    dir2 = 1;
  }
  else
  {
    dir1 = direction [2];

    dir2 = direction [1];
  }

  if (!done [3])
  {
    dir3 = 1;  	
  }
  else
  {
  	dir3 = dir2;
  	
  	dir2 = dir1;
  	
  	dir1 = direction [3];
  }


    /* Find the binary data */

  cbf_failnez (cbf_find_category (handle, "array_data"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))
  cbf_failnez (cbf_find_row      (handle, array_id))
  
  if ( binary_id ) {
  
    if (cbf_find_column(handle, "binary_id")) {
    
      if ( *binary_id !=0 && *binary_id != 1 ) return CBF_NOTFOUND;   	
    	
    } else {
    	
      while (1)  {
        if (cbf_get_integervalue( handle, &local_binary_id) || 
          local_binary_id == 0) local_binary_id = 1;
        if (local_binary_id != *binary_id) {
          cbf_failnez (cbf_find_column   (handle, "array_id"))
          if (cbf_find_nextrow  (handle, array_id)) return CBF_NOTFOUND;
          cbf_failnez (cbf_find_column(handle, "binary_id"))
        } else break;
      }
    }	
  }

  cbf_failnez (cbf_find_column   (handle, "data"))

    /* Read the binary data */

  if ( ndimslow <= 0 || ndimmid  <= 0 ||  ndimfast <= 0)

    return CBF_ARGUMENT;

  if (eltype == CBF_INTEGER) {
    cbf_failnez (cbf_get_integerarray (handle, &local_binary_id,
               array, elsize, elsign, ndimslow * ndimmid * ndimfast, &nelem_read))
  } else {
  	cbf_failnez (cbf_get_realarray (handle, &local_binary_id,
               array, elsize, ndimslow * ndimmid * ndimfast, &nelem_read))
  }
  
  if ( binary_id ) *binary_id = local_binary_id;


    /* Reorder the data if necessary */

#ifndef CBF_0721_READS

  if (dir1 < 0 || dir2 < 0 || dir3 < 0 )
  {
    if (dir1 >= 0)
    {
      start1 = 0;
      end1 = ndimslow;
      inc1 = 1;
    }
    else
    {
      start1 = ndimslow - 1;
      end1 = -1;
      inc1 = -1;
    }

    if (dir2 >= 0)
    {
      start2 = 0;
      end2 = ndimmid;
      inc2 = 1;
    }
    else
    {
      start2 = ndimmid - 1;
      end2 = -1;
      inc2 = -1;
    }

    if (dir3 >= 0)
    {
      start3 = 0;
      end3 = ndimfast;
      inc3 = 1;
    }
    else
    {
      start3 = ndimfast - 1;
      end3 = -1;
      inc3 = -1;
    }


    pixel = (char *) array;

    for (index1 = start1; index1 != end1; index1 += inc1)

      for (index2 = start2; index2 != end2; index2 += inc2)

        for (index3 = start3; index3 != end3; index3 += inc3)
        {
          pixel2 = ((char *) array) + (index1*ndimmid*ndimfast + index2 * ndimfast + index3) * elsize;

          if (pixel < pixel2) {

            if (elsize == sizeof (int))
            {
              *((int *) tmp)    = *((int *) pixel);
              *((int *) pixel)  = *((int *) pixel2);
              *((int *) pixel2) = *((int *) tmp);
            }
            else
            {
              memcpy (tmp, pixel, elsize);
              memcpy (pixel, pixel2, elsize);
              memcpy (pixel2, tmp, elsize);
            }

          }

          pixel += elsize;
        }
  }

#endif

  if (ndimslow * ndimmid * ndimfast != nelem_read)

    return CBF_ENDOFDATA;

  return 0;
}



  /* Save a 3D array.  
                       ndimslow is the slowest dimension, 
                       ndimmid is the next faster dimension,
                       ndimfast is the fastest dimension */

int cbf_set_3d_array (cbf_handle    handle,
                   unsigned int  reserved,
                   const char   *array_id,
                   int          *binary_id,
                   unsigned int  compression,
                   void         *array,
                   int           eltype,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimmid,
                   size_t        ndimfast)
{

  char enctype[30];

  int local_binary_id, done [4], precedence, dimension [4];

  if (reserved != 0)

    return CBF_ARGUMENT;
    
  if ( eltype != CBF_FLOAT && eltype != CBF_INTEGER)
  
    return CBF_ARGUMENT;

  if ( eltype == CBF_FLOAT && elsize != 4 && elsize != 8 )

    return CBF_ARGUMENT;
    
  if ( eltype == CBF_FLOAT && !elsign)
  
    return CBF_ARGUMENT;

    /* Update the array_structure_list category */

  if (ndimslow == 0)

    dimension [3] = 1;

  else

    dimension [3] = ndimslow;

  if (ndimmid == 0)

    dimension [2] = 1;

  else

    dimension [2] = ndimmid;
    
  if (ndimfast == 0)

    dimension [1] = 1;

  else

    dimension [1] = ndimfast;
    
  
  done [1] = dimension [1] == 1;
  done [2] = dimension [2] == 1;
  done [3] = dimension [3] == 1;

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 3)

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column      (handle, "dimension"))
    cbf_failnez (cbf_set_integervalue (handle, dimension [precedence]))

    done [precedence] = 1;

    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!done [1] || !done [2] || !done[3])

    return CBF_NOTFOUND;


    /* Get the binary_id */

  cbf_failnez (cbf_require_category (handle, "array_data"))
  cbf_failnez (cbf_require_column   (handle, "array_id"))
  cbf_failnez (cbf_rewind_row       (handle))
  if (cbf_find_row (handle, array_id)) {
    cbf_failnez (cbf_new_row(handle))
    cbf_failnez (cbf_set_value(handle,array_id))
  }
  cbf_failnez (cbf_require_column   (handle, "binary_id"))
  if (binary_id)  {
    if (*binary_id == 0) *binary_id = 1;
    while (1) {
      if ( cbf_get_integervalue(handle,&local_binary_id) 
        || local_binary_id == 0) local_binary_id = 1; 
      if ( local_binary_id != *binary_id ) {
        cbf_failnez (cbf_find_column(handle, "array_id")) 
        if (cbf_find_nextrow(handle, array_id)) {
       	  cbf_failnez (cbf_new_row( handle )) 
          cbf_failnez (cbf_set_value(handle,array_id))
          cbf_failnez (cbf_find_column (handle, "binary_id"))
          cbf_failnez (cbf_set_integervalue (handle, *binary_id))
          break;
        }
        cbf_failnez (cbf_find_column(handle, "binary_id"))
      } else {
        break;
      }
    }
  } 
  else 
  {
    if (cbf_get_integervalue (handle, &local_binary_id)) {
      local_binary_id = 1;
      cbf_failnez (cbf_set_integervalue (handle, local_binary_id))
    }
  }
  cbf_failnez (cbf_find_column      (handle, "data"))


    /* Save the array */

  if (eltype == CBF_INTEGER)   {
  
    cbf_failnez (cbf_set_integerarray_wdims (handle, compression, *binary_id,
                                     array, elsize, elsign,
                                     dimension [1] * dimension [2] * dimension [3],
                                     "little_endian",
                                     dimension[1],
                                     (dimension[3]>1||dimension[2]>1)?dimension[2]:0,
                                     dimension[3]>1?dimension[3]:0,0 ))
  } else {
  	
    cbf_failnez (cbf_set_realarray_wdims (handle, compression, *binary_id,
                                     array, elsize,
                                     dimension [1] * dimension [2] * dimension [3],
                                     "little_endian",
                                     dimension[1],
                                     (dimension[3]>1||dimension[2]>1)?dimension[2]:0,
                                     dimension[3]>1?dimension[3]:0,0 ))
  }
  
    /* Update the array_structure category */

  cbf_failnez (cbf_require_category (handle, "array_structure"))
  cbf_failnez (cbf_require_column   (handle, "id"))
  cbf_failnez (cbf_rewind_row       (handle))
  if (cbf_find_row (handle, array_id)) {
    cbf_failnez (cbf_new_row(handle))
    cbf_failnez (cbf_set_value(handle,array_id))
    cbf_failnez (cbf_set_typeofvalue(handle,"word"))
  }
  cbf_failnez (cbf_require_column   (handle, "encoding_type"))
  if (eltype == CBF_INTEGER) {
  	if (elsign) {
      sprintf(enctype,"signed %d-bit integer", ((int)elsize)*8);
    } else {
      sprintf(enctype,"unsigned %d-bit integer", ((int)elsize)*8);
    }
  } else {	
    sprintf(enctype,"signed %d-bit real IEEE", ((int)elsize)*8);
  }
  cbf_failnez (cbf_set_value        (handle,enctype))
  cbf_failnez (cbf_set_typeofvalue  (handle,"dblq"))
  cbf_failnez (cbf_require_column   (handle, "compression_type"))
  switch (compression&CBF_COMPRESSION_MASK) {
    case (CBF_NONE):
      cbf_failnez (cbf_set_value      (handle,"none"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
      break;
    case (CBF_CANONICAL):
      cbf_failnez (cbf_set_value      (handle,"canonical"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
      break;
    case (CBF_PACKED):
      cbf_failnez (cbf_set_value      (handle,"packed"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
      break;
    case (CBF_PACKED_V2):
      cbf_failnez (cbf_set_value      (handle,"packed_v2"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
      break;
    case (CBF_BYTE_OFFSET):
      cbf_failnez (cbf_set_value      (handle,"byte_offsets"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
      break;
    case (CBF_PREDICTOR):
      cbf_failnez (cbf_set_value      (handle,"predictor"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
      break;
    default:
      cbf_failnez (cbf_set_value      (handle,"."))
      cbf_failnez (cbf_set_typeofvalue(handle,"null"))
      break;
  }
  if (compression&CBF_FLAG_MASK) {
    if (compression&CBF_UNCORRELATED_SECTIONS) {
      cbf_failnez (cbf_require_column   (handle, "compression_type_flag"))
      cbf_failnez (cbf_set_value        (handle, "uncorrelated_sections"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
    } else if (compression&CBF_FLAT_IMAGE)  {
      cbf_failnez (cbf_require_column   (handle, "compression_type_flag"))
      cbf_failnez (cbf_set_value        (handle, "flat"))
      cbf_failnez (cbf_set_typeofvalue(handle,"word"))
    }
    else return CBF_ARGUMENT;	
  }

  cbf_failnez (cbf_require_column     (handle, "byte_order"))
  cbf_failnez (cbf_set_value          (handle, "little_endian"))


  return 0;
}



  /* Get the type of an axis */

int cbf_get_axis_type (cbf_handle handle, const char *axis_id,
                                          cbf_axis_type *axis_type)
{
  const char *type;


    /* Get the axis type */

  cbf_failnez (cbf_find_category (handle, "axis"))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_find_row      (handle, axis_id))
  cbf_failnez (cbf_find_column   (handle, "type"))
  cbf_failnez (cbf_get_value     (handle, &type))

  if (!type)

    return CBF_NOTFOUND;

  if (toupper (*type) != 'T' &&
      toupper (*type) != 'R' &&
      toupper (*type) != 'G')

    return CBF_FORMAT;

  if (axis_type) {

    if (toupper (*type) == 'T') {

      *axis_type = CBF_TRANSLATION_AXIS;

    } else {

      if (toupper (*type) == 'R') {

        *axis_type = CBF_ROTATION_AXIS;

      } else {

        *axis_type = CBF_GENERAL_AXIS;
      }
    }
  }

  return 0;
}


  /* Get an axis vector */

int cbf_get_axis_vector (cbf_handle handle, const char *axis_id,
                                            double *vector1,
                                            double *vector2,
                                            double *vector3)
{
    /* Read from the axis category */

  cbf_failnez (cbf_find_category   (handle, "axis"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, axis_id))
  cbf_failnez (cbf_find_column     (handle, "vector[1]"))
  cbf_failnez (cbf_get_doublevalue (handle, vector1))
  cbf_failnez (cbf_find_column     (handle, "vector[2]"))
  cbf_failnez (cbf_get_doublevalue (handle, vector2))
  cbf_failnez (cbf_find_column     (handle, "vector[3]"))
  cbf_failnez (cbf_get_doublevalue (handle, vector3))

  return 0;
}


  /* Get an axis offset */

int cbf_get_axis_offset (cbf_handle handle, const char *axis_id,
                                            double *offset1,
                                            double *offset2,
                                            double *offset3)
{
    /* Read from the axis category */

  cbf_failnez (cbf_find_category   (handle, "axis"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, axis_id))
  cbf_failnez (cbf_find_column     (handle, "offset[1]"))
  cbf_failnez (cbf_get_doublevalue (handle, offset1))
  cbf_failnez (cbf_find_column     (handle, "offset[2]"))
  cbf_failnez (cbf_get_doublevalue (handle, offset2))
  cbf_failnez (cbf_find_column     (handle, "offset[3]"))
  cbf_failnez (cbf_get_doublevalue (handle, offset3))

  return 0;
}


  /* Get the setting of an axis */

int cbf_get_axis_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double       *start,
                                             double       *increment)
{
  cbf_axis_type type;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Get the axis type */

  cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

  if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS)

    return CBF_FORMAT;


    /* Read from the diffrn_scan_axis and
                     diffrn_scan_frame_axis categories */

  if (type == CBF_TRANSLATION_AXIS)
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "displacement"))
    cbf_failnez (cbf_get_doublevalue (handle, start))

    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, increment))
  }
  else
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "angle"))
    cbf_failnez (cbf_get_doublevalue (handle, start))

    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "angle_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, increment))
  }

  return 0;
}


  /* Get the reference setting of an axis */

int cbf_get_axis_reference_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double       *refsetting)
{
  cbf_axis_type type;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Get the axis type */

  cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

  if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS)

    return CBF_FORMAT;


    /* Read from the diffrn_scan_axis and
                     diffrn_scan_frame_axis categories */

  if (type == CBF_TRANSLATION_AXIS)
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    *refsetting = 0.;
    if (!cbf_find_column (handle, "reference_displacement")) {
      if (cbf_get_doublevalue (handle, refsetting)) {
        if (!cbf_find_column (handle, "displacement")) {
          if (cbf_get_doublevalue (handle, refsetting)) {
          	*refsetting = 0.;
          }
        }
      }
    } else {
      if (!cbf_find_column (handle, "displacement")) {
          if (cbf_get_doublevalue (handle, refsetting)) {
          	*refsetting = 0.;
        }
      }
      else  {
        cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
        cbf_failnez (cbf_find_column     (handle, "axis_id"))
        cbf_failnez (cbf_find_row        (handle, axis_id))
        if (!cbf_find_column (handle, "reference_displacement")) {
          if (cbf_get_doublevalue (handle, refsetting)) {
            if (!cbf_find_column (handle, "displacement")) {
              if (cbf_get_doublevalue (handle, refsetting)) {
          	    *refsetting = 0.;
              }
            }
          }
        } else  {
          if (!cbf_find_column (handle, "displacement")) {
            if (cbf_get_doublevalue (handle, refsetting)) {
          	  *refsetting = 0.;
            }
          }	
        }
      }
    }    	
  }
  else
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    *refsetting = 0.;
    if (!cbf_find_column (handle, "reference_angle")) {
      if (cbf_get_doublevalue (handle, refsetting)) {
        *refsetting = 0.;
      }
    } else {
      cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
      cbf_failnez (cbf_find_column     (handle, "axis_id"))
      cbf_failnez (cbf_find_row        (handle, axis_id))
      if (!cbf_find_column (handle, "reference_angle")) {
        if (cbf_get_doublevalue (handle, refsetting)) {
      	*refsetting = 0.;
        }
      }
    }    	
  }
  return 0;
}


  /* Change the setting of an axis */

int cbf_set_axis_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double        start,
                                             double        increment)
{
  cbf_axis_type type;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Get the axis type */

  cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

  if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS)

    return CBF_FORMAT;


    /* Update the diffrn_scan_axis and
                  diffrn_scan_frame_axis categories */

  if (type == CBF_TRANSLATION_AXIS)
  {
    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "displacement"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", start))
    if (!cbf_find_column( handle, "displacement_increment")) {
      cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", increment))
    }

    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "displacement_start"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", start))
    cbf_failnez (cbf_require_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", increment))
    cbf_failnez (cbf_require_column     (handle, "displacement_range"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", increment))
  }
  else
  {
    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "angle"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", start))
    if (!cbf_find_column     (handle, "angle_increment")) {
      cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", increment))
    }
    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "angle_start"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", start))
    cbf_failnez (cbf_require_column     (handle, "angle_increment"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", increment))
    cbf_failnez (cbf_require_column     (handle, "angle_range"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", increment))
  }

  return 0;
}


  /* Change the reference setting of an axis */

int cbf_set_axis_reference_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double        refsetting)
{
  cbf_axis_type type;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Get the axis type */

  cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

  if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS)

    return CBF_FORMAT;


    /* Update the diffrn_scan_axis and
                  diffrn_scan_frame_axis categories */

  if (type == CBF_TRANSLATION_AXIS)
  {
    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "reference_displacement"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", refsetting))

    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "reference_displacement"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", refsetting))
  }
  else
  {
    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "reference_angle"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", refsetting))

    cbf_failnez (cbf_require_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_row        (handle, axis_id))
    cbf_failnez (cbf_require_column     (handle, "reference_angle"))
    cbf_failnez (cbf_set_doublevalue    (handle, "%-.15g", refsetting))
  }

  return 0;
}


  /* Create a positioner */

int cbf_make_positioner (cbf_positioner *positioner)
{
  cbf_failnez (cbf_alloc ((void **) positioner, NULL,
               sizeof (cbf_positioner_struct), 1))

  (*positioner)->matrix [0][0] = 1;
  (*positioner)->matrix [0][1] = 0;
  (*positioner)->matrix [0][2] = 0;
  (*positioner)->matrix [0][3] = 0;
  (*positioner)->matrix [1][0] = 0;
  (*positioner)->matrix [1][1] = 1;
  (*positioner)->matrix [1][2] = 0;
  (*positioner)->matrix [1][3] = 0;
  (*positioner)->matrix [2][0] = 0;
  (*positioner)->matrix [2][1] = 0;
  (*positioner)->matrix [2][2] = 1;
  (*positioner)->matrix [2][3] = 0;

  (*positioner)->axis = NULL;

  (*positioner)->axes = 0;

  (*positioner)->matrix_is_valid = 1;

  (*positioner)->axes_are_connected = 1;

  return 0;
}


  /* Free a positioner */

int cbf_free_positioner (cbf_positioner positioner)
{
  int errorcode;
  
  void *memblock;
  
  void *vaxis;
  
  void *vname;
  
  void *adon;

  size_t i;
   
  memblock = (void *) positioner;

  if (positioner)
  {
    errorcode = 0;

    for (i = 0; i < positioner->axes; i++) {
    
      vname = (void *)(positioner->axis [i].name);
    	
      errorcode |= cbf_free ((void **) &vname, NULL);
      
      positioner->axis [i].name = NULL;
      
      if (positioner->axis [i].depends_on) {
      
        adon = (void *)(positioner->axis [i].depends_on);
        
        errorcode |= cbf_free ((void **) &adon, NULL);
        
        positioner->axis [i].depends_on = NULL;
      
      }
    
    }

    vaxis = (void *)positioner->axis;
    
    errorcode |= cbf_free ((void **) &vaxis,
                                     &positioner->axes);
                                     
    positioner->axis = NULL;

    return errorcode | cbf_free (&memblock, NULL);
  }

  return 0;
}


  /* Add a positioner axis */

int cbf_add_positioner_axis (cbf_positioner positioner,
                             const char    *name,
                             const char    *depends_on,
                             cbf_axis_type  type,
                             double         vector1,
                             double         vector2,
                             double         vector3,
                             double         offset1,
                             double         offset2,
                             double         offset3,
                             double         start,
                             double         increment)
{
  int errorcode = 0;

  cbf_axis_struct axis;
  
  void  *vaxis;
  
  void  *vname;

  double length;


    /* Check the arguments */

  if (!name || !positioner || (type != CBF_TRANSLATION_AXIS &&
                               type != CBF_ROTATION_AXIS))

    return CBF_ARGUMENT;

  length = vector1 * vector1 + vector2 * vector2 + vector3 * vector3;

  if (length <= 0.0)

    return CBF_ARGUMENT;


    /* Allocate memory and copy the axis names */
    
  axis.name = NULL;
  
  axis.name = (char *)cbf_copy_string(NULL,name,0);

  axis.depends_on = NULL;

  if (depends_on) {
  
    axis.depends_on = (char *)cbf_copy_string(NULL,depends_on,0);
                                     
  }

  if (errorcode)
  
  {  vname = (void *)axis.name;
     
  	 errorcode |= cbf_free (&vname, NULL);
  	 
  	 axis.name = NULL;
  	 
  	 return errorcode;
    
  }

  vaxis = (void *)(positioner->axis);

  errorcode = cbf_realloc ((void **) &vaxis,
                                     &(positioner->axes),
                                sizeof (cbf_axis_struct),
                                  positioner->axes + 1);
  positioner->axis = (cbf_axis_struct *)vaxis;

  if (errorcode) 
  { int nerrorcode;
  
    void * vdepends_on;
    
    vname = (void *)axis.name;
    
    vdepends_on = (void *)axis.depends_on;
  	
    nerrorcode = cbf_free (&vname, NULL) |
           cbf_free (&vdepends_on, NULL);
           
    axis.name = NULL;
    
    axis.depends_on = NULL;
    
    return nerrorcode;
    
  }

  length = sqrt (length);

  axis.type = type;

  axis.vector [0] = vector1 / length;
  axis.vector [1] = vector2 / length;
  axis.vector [2] = vector3 / length;

  axis.offset [0] = offset1;
  axis.offset [1] = offset2;
  axis.offset [2] = offset3;

  axis.start     = start;
  axis.increment = increment;
  axis.setting   = 0;

  positioner->axis [positioner->axes - 1] = axis;

  positioner->matrix_is_valid = 0;

  positioner->axes_are_connected = 0;

  return 0;
}


  /* Add a goniometer axis from a file */

int cbf_read_positioner_axis (cbf_handle      handle,
                              unsigned int    reserved,
                              cbf_positioner  positioner,
                              const char     *axis_id,
                              int             read_setting)
{
  const char *next_id;

  cbf_axis_type axis_type;

  double vector1, vector2, vector3, offset1, offset2, offset3;

  double start, increment;

  cbf_failnez (cbf_find_category    (handle, "axis"))
  cbf_failnez (cbf_find_column      (handle, "id"))
  cbf_failnez (cbf_find_row         (handle, axis_id))
  cbf_failnez (cbf_find_column      (handle, "depends_on"))
  cbf_failnez (cbf_get_value        (handle, &next_id))
  cbf_failnez (cbf_get_axis_type    (handle, axis_id,
                                             &axis_type))
  cbf_failnez (cbf_get_axis_vector  (handle, axis_id,
                                             &vector1,
                                             &vector2,
                                             &vector3))
  cbf_failnez (cbf_get_axis_offset  (handle, axis_id,
                                             &offset1,
                                             &offset2,
                                             &offset3))

  start = increment = 0;

  if (read_setting) 
  {
  	
    cbf_failnez (cbf_get_axis_setting (handle, reserved, axis_id,
                                          &start,
                                          &increment))
  
    if (read_setting < 0) {
    	cbf_failnez (cbf_get_axis_reference_setting (handle, reserved, axis_id,
                                          &start))
    }

  }

  cbf_failnez (cbf_add_positioner_axis (positioner,
                       axis_id,
                       next_id,
                       axis_type,
                       vector1, vector2, vector3,
                       offset1, offset2, offset3,
                       start, increment))

  return 0;
}


  /* Connect a set of positioner axes */

int cbf_connect_axes (cbf_positioner positioner)
{
  if (!positioner)

    return CBF_ARGUMENT;

  if (!positioner->axes_are_connected)
  {
      /* Arrange the axes in order of their connection */

    cbf_axis_struct axis;

    const char *depends_on = ".";

    int dest, search, found;

    for (dest = ((int) positioner->axes) - 1; dest >= 0; dest--)
    {
      for (search = 0; search <= dest; search++)
      {
        if (positioner->axis [search].depends_on)

          found = cbf_cistrcmp (positioner->axis [search].depends_on,
                                depends_on) == 0;

        else

          found = cbf_cistrcmp (".", depends_on) == 0;

        if (found)
        {
          depends_on = positioner->axis [search].name;

          axis                      = positioner->axis [dest];
          positioner->axis [dest]   = positioner->axis [search];
          positioner->axis [search] = axis;

          if (!depends_on && dest > 0)

            return CBF_NOTFOUND;

          break;
        }
      }

      if (search > dest)

        return CBF_NOTFOUND;
    }

    positioner->axes_are_connected = 1;
  }

  return 0;
}


  /* Calculate a position given initial coordinates */

int cbf_calculate_position (cbf_positioner positioner,
                            unsigned int   reserved,
                            double         ratio,
                            double         initial1,
                            double         initial2,
                            double         initial3,
                            double        *final1,
                            double        *final2,
                            double        *final3)
{
  size_t i;

  double setting;

  if (!positioner)

    return CBF_ARGUMENT;

  if (reserved != 0)

    return CBF_ARGUMENT;

  for (i = 0; i < positioner->axes; i++)
  {
    setting = positioner->axis [i].start + ratio *
              positioner->axis [i].increment;

    if (positioner->axis [i].setting != setting)
    {
      positioner->matrix_is_valid = 0;

      positioner->axis [i].setting = setting;
    }
  }

  if (!positioner->matrix_is_valid)
  {
    positioner->matrix [0][0] = 1;
    positioner->matrix [0][1] = 0;
    positioner->matrix [0][2] = 0;
    positioner->matrix [0][3] = 0;
    positioner->matrix [1][0] = 0;
    positioner->matrix [1][1] = 1;
    positioner->matrix [1][2] = 0;
    positioner->matrix [1][3] = 0;
    positioner->matrix [2][0] = 0;
    positioner->matrix [2][1] = 0;
    positioner->matrix [2][2] = 1;
    positioner->matrix [2][3] = 0;

    if (!positioner->axes_are_connected)

      cbf_failnez (cbf_connect_axes (positioner))

    for (i = 0; i < positioner->axes; i++)
    {
      setting = positioner->axis [i].setting;

      if (positioner->axis [i].type == CBF_TRANSLATION_AXIS)
      {
        positioner->matrix [0][3] += setting *
                                     positioner->axis [i].vector [0];
        positioner->matrix [1][3] += setting *
                                     positioner->axis [i].vector [1];
        positioner->matrix [2][3] += setting *
                                     positioner->axis [i].vector [2];
      }
      else
      {
        double s, x, y, z, w,
               xx, yy, zz, xy, xz, xw, yz, yw, zw;

        double rotation [3][3], product [3][3];

        int r1, c1r2, c2;

        s = sin (setting * 0.00872664625997164788461845384244);

        x = positioner->axis [i].vector [0] * s;
        y = positioner->axis [i].vector [1] * s;
        z = positioner->axis [i].vector [2] * s;

        w = cos (setting * 0.00872664625997164788461845384244);

        xx = x * x;
        yy = y * y;
        zz = z * z;
        xy = x * y;
        xz = x * z;
        xw = x * w;
        yz = y * z;
        yw = y * w;
        zw = z * w;

        rotation [0][0] = 1 - 2 * (yy + zz);
        rotation [0][1] =     2 * (xy - zw);
        rotation [0][2] =     2 * (xz + yw);
        rotation [1][0] =     2 * (xy + zw);
        rotation [1][1] = 1 - 2 * (xx + zz);
        rotation [1][2] =     2 * (yz - xw);
        rotation [2][0] =     2 * (xz - yw);
        rotation [2][1] =     2 * (yz + xw);
        rotation [2][2] = 1 - 2 * (xx + yy);

        for (r1 = 0; r1 < 3; r1++)

          for (c2 = 0; c2 < 3; c2++)
          {
            product [r1][c2] = 0;

            for (c1r2 = 0; c1r2 < 3; c1r2++)

              product [r1][c2] += rotation [r1][c1r2] *
                                  positioner->matrix [c1r2][c2];
          }

        for (r1 = 0; r1 < 3; r1++)

          for (c2 = 0; c2 < 3; c2++)

            positioner->matrix [r1][c2] = product [r1][c2];
      }

      positioner->matrix [0][3] += positioner->axis [i].offset [0];
      positioner->matrix [1][3] += positioner->axis [i].offset [1];
      positioner->matrix [2][3] += positioner->axis [i].offset [2];
    }

    positioner->matrix_is_valid = 1;
  }

  if (final1)

    *final1 = positioner->matrix [0][0] * initial1 +
              positioner->matrix [0][1] * initial2 +
              positioner->matrix [0][2] * initial3 +
              positioner->matrix [0][3];

  if (final2)

    *final2 = positioner->matrix [1][0] * initial1 +
              positioner->matrix [1][1] * initial2 +
              positioner->matrix [1][2] * initial3 +
              positioner->matrix [1][3];

  if (final3)

    *final3 = positioner->matrix [2][0] * initial1 +
              positioner->matrix [2][1] * initial2 +
              positioner->matrix [2][2] * initial3 +
              positioner->matrix [2][3];

  return 0;
}


  /* Calculate the initial position given final coordinates */

int cbf_calculate_initial_position (cbf_positioner positioner,
                                    unsigned int   reserved,
                                    double         ratio,
                                    double         final1,
                                    double         final2,
                                    double         final3,
                                    double        *initial1,
                                    double        *initial2,
                                    double        *initial3)
{
  double delta [3];

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Update the matrix */

  cbf_failnez (cbf_calculate_position (positioner, reserved, ratio, 0, 0, 0,
                                                   NULL, NULL, NULL))

  delta [0] = final1 - positioner->matrix [0][3];
  delta [1] = final2 - positioner->matrix [1][3];
  delta [2] = final3 - positioner->matrix [2][3];

  if (initial1)

    *initial1 = positioner->matrix [0][0] * delta [0] +
                positioner->matrix [1][0] * delta [1] +
                positioner->matrix [2][0] * delta [2];

  if (initial2)

    *initial2 = positioner->matrix [0][1] * delta [0] +
                positioner->matrix [1][1] * delta [1] +
                positioner->matrix [2][1] * delta [2];

  if (initial3)

    *initial3 = positioner->matrix [0][2] * delta [0] +
                positioner->matrix [1][2] * delta [1] +
                positioner->matrix [2][2] * delta [2];

  return 0;
}


  /* Construct a goniometer */

int cbf_construct_goniometer (cbf_handle handle,
                              cbf_goniometer *goniometer)
{
  const char *diffrn_id, *id, *this_id, *axis_id;

  unsigned int row;

  int errorcode;


  if (!goniometer)

    return CBF_ARGUMENT;


    /* Get the measurement id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_measurement"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))


    /* Construct the goniometer */

  cbf_failnez (cbf_make_positioner (goniometer))

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_find_category (handle, "diffrn_measurement_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_measurement_axis.measurement_id
                              _diffrn_measurement_axis.id  (deprecated) */

      errorcode = cbf_find_column (handle, "measurement_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle,
                                                0, /* reserved */
                                                *goniometer,
                                                axis_id, 1);
      }
  }

  if (!errorcode)

    errorcode = cbf_connect_axes (*goniometer);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (*goniometer);

    *goniometer = NULL;
  }

  return errorcode;
}


  /* Free a goniometer */

int cbf_free_goniometer (cbf_goniometer goniometer)
{
  return cbf_free_positioner (goniometer);
}


  /* Get the rotation axis */

int cbf_get_rotation_axis (cbf_goniometer goniometer, unsigned int  reserved,
                                                      double       *vector1,
                                                      double       *vector2,
                                                      double       *vector3)
{
  size_t axis;

  if (!goniometer)

    return CBF_ARGUMENT;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Currently just return the first rotation axis */

  for (axis = 0; axis < goniometer->axes; axis++)

    if (goniometer->axis [axis].type == CBF_ROTATION_AXIS)

      if (goniometer->axis [axis].increment)
      {
        if (vector1)

          *vector1 = goniometer->axis [axis].vector [0];

        if (vector2)

          *vector2 = goniometer->axis [axis].vector [1];

        if (vector3)

          *vector3 = goniometer->axis [axis].vector [2];

        return 0;
      }

  return CBF_NOTFOUND;
}


  /* Get the rotation range */

int cbf_get_rotation_range (cbf_goniometer goniometer, unsigned int  reserved,
                                                       double       *start,
                                                       double       *increment)
{
  size_t axis;

  if (!goniometer)

    return CBF_ARGUMENT;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Currently just return the range of the first rotation axis */

  for (axis = 0; axis < goniometer->axes; axis++)

    if (goniometer->axis [axis].type == CBF_ROTATION_AXIS)

      if (goniometer->axis [axis].increment)
      {
        if (start)

          *start = goniometer->axis [axis].start;

        if (increment)

          *increment = goniometer->axis [axis].increment;

        return 0;
      }

  return CBF_NOTFOUND;
}


  /* Reorient a vector */

int cbf_rotate_vector (cbf_goniometer goniometer, unsigned int reserved,
                                                  double       ratio,
                                                  double       initial1,
                                                  double       initial2,
                                                  double       initial3,
                                                  double      *final1,
                                                  double      *final2,
                                                  double      *final3)
{
  double transformed [3], origin [3];

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_calculate_position (goniometer, reserved, ratio, 0, 0, 0,
                                           &origin [0],
                                           &origin [1],
                                           &origin [2]))

  cbf_failnez (cbf_calculate_position (goniometer, reserved, ratio,
                                           initial1,
                                           initial2,
                                           initial3,
                                           &transformed [0],
                                           &transformed [1],
                                           &transformed [2]))

  if (final1)

    *final1 = transformed [0] - origin [0];

  if (final2)

    *final2 = transformed [1] - origin [1];

  if (final3)

    *final3 = transformed [2] - origin [2];

  return 0;
}


  /* Convert a vector to reciprocal space (assumes beam along -z) */

int cbf_get_reciprocal (cbf_goniometer goniometer, unsigned int reserved,
                                                   double       ratio,
                                                   double       wavelength,
                                                   double       real1,
                                                   double       real2,
                                                   double       real3,
                                                   double      *reciprocal1,
                                                   double      *reciprocal2,
                                                   double      *reciprocal3)
{
  double length, ewald [3];

  if (reserved != 0)

    return CBF_ARGUMENT;

  if (wavelength <= 0.0)

    return CBF_ARGUMENT;

  length = real1 * real1 + real2 * real2 + real3 * real3;

  if (length <= 0.0)

    return CBF_ARGUMENT;


    /* Project the vector onto the sphere */

  length = sqrt (length) * wavelength;

  ewald [0] = real1 / length;
  ewald [1] = real2 / length;
  ewald [2] = real3 / length + 1 / wavelength;


    /* Rotate the vector back to the 0 position of the goniometer */

  cbf_failnez (cbf_calculate_initial_position (goniometer, reserved, ratio,
                                               ewald [0],
                                               ewald [1],
                                               ewald [2],
                                               reciprocal1,
                                               reciprocal2,
                                               reciprocal3))

  return 0;
}


  /* Construct a detector positioner */

int cbf_construct_detector (cbf_handle    handle,
                            cbf_detector *detector,
                            unsigned int  element_number)
{
  int errorcode, precedence;

  unsigned int row, axis;

  const char *diffrn_id, *id, *this_id, *axis_id, *array_id;

  const char *surface_axis [2];  /* fast, slow */

  double displacement [2], increment [2];

  cbf_positioner positioner;

  if (!detector)

    return CBF_ARGUMENT;


    /* Get the detector id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))


    /* Construct the detector surface */

  cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id))
  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  surface_axis [0] = surface_axis [1] = NULL;

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    if (surface_axis [precedence - 1])

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column (handle, "axis_set_id"))
    cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!surface_axis [0])

    return CBF_FORMAT;

  cbf_failnez (cbf_find_category   (handle, "array_structure_list_axis"))
  cbf_failnez (cbf_find_column     (handle, "axis_set_id"))
  cbf_failnez (cbf_find_row        (handle, surface_axis [0]))
  cbf_failnez (cbf_find_column     (handle, "axis_id"))
  cbf_failnez (cbf_get_value       (handle, &surface_axis [0]))
  cbf_failnez (cbf_find_column     (handle, "displacement"))
  cbf_failnez (cbf_get_doublevalue (handle, &displacement [0]))
  cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
  cbf_failnez (cbf_get_doublevalue (handle, &increment [0]))

  if (surface_axis [1])
  {
    cbf_failnez (cbf_find_column     (handle, "axis_set_id"))
    cbf_failnez (cbf_find_row        (handle, surface_axis [1]))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_get_value       (handle, &surface_axis [1]))
    cbf_failnez (cbf_find_column     (handle, "displacement"))
    cbf_failnez (cbf_get_doublevalue (handle, &displacement [1]))
    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, &increment [1]))
  }


    /* Construct the positioner */

  cbf_failnez (cbf_make_positioner (&positioner))

  errorcode = cbf_alloc ((void **) detector, NULL,
                        sizeof (cbf_detector_struct), 1);

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_find_category (handle, "diffrn_detector_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_detector_axis.detector_id
                              _diffrn_detector_axis.id  (deprecated) */

      errorcode = cbf_find_column (handle, "detector_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle, 0,
                                                positioner,
                                                axis_id, 1);
      }
  }


    /* Add the surface axes */

  if (!errorcode)

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [0], 0);

  if (!errorcode && surface_axis [1])

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [1], 0);


    /* Connect the axes */

  if (!errorcode)

    errorcode = cbf_connect_axes (positioner);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (positioner);

    return errorcode | cbf_free ((void **) detector, NULL);
  }
  
    /* Insert the cbf handle and element into the dectector */
    
  (*detector)->handle = handle;

  (*detector)->element = element_number;


    /* Copy the start and increment values into the surface axes */

  (*detector)->displacement [0] = displacement [0];
  (*detector)->displacement [1] = displacement [1];

  (*detector)->increment [0] = increment [0];
  (*detector)->increment [1] = increment [1];

  if (surface_axis [1])

    (*detector)->axes = 2;

  else

    (*detector)->axes = 1;

  for (axis = 0; axis < (*detector)->axes; axis++)

    for (row = 0; row < positioner->axes; row++)

      if (cbf_cistrcmp (positioner->axis [row].name,
                        surface_axis [axis]) == 0)
      {
        (*detector)->index [axis] = row;

        positioner->axis [row].increment = 0;

        break;
      }

  (*detector)->positioner = positioner;

  return 0;
}


  /* Construct a detector positioner, creating the necessary categories, and columns */

int cbf_require_detector (cbf_handle    handle, cbf_detector      *detector,
                                                unsigned int      element_number)
{
  int errorcode, precedence;

  unsigned int row, axis;

  const char *diffrn_id, *id, *this_id, *axis_id, *array_id;

  const char *surface_axis [2];

  double displacement [2], increment [2];

  cbf_positioner positioner;

  if (!detector)

    return CBF_ARGUMENT;


    /* Get the detector id */

  cbf_failnez (cbf_require_diffrn_id (handle, &diffrn_id, "DIFFRN_ID"))

  cbf_failnez (cbf_require_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_require_column   (handle, "diffrn_id"))
  if (cbf_find_row (handle, diffrn_id))  {
  	 cbf_failnez(cbf_new_row(handle))
  	 cbf_failnez(cbf_set_value(handle,diffrn_id))
  }
  cbf_failnez (cbf_require_column   (handle, "id"))
  cbf_failnez (cbf_require_value    (handle, &id, diffrn_id))


    /* Construct the detector surface */

  cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id))
  cbf_failnez (cbf_require_category (handle, "array_structure_list"))
  cbf_failnez (cbf_require_column   (handle, "array_id"))

  surface_axis [0] = surface_axis [1] = NULL;
  
  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    if (surface_axis [precedence - 1])

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column (handle, "axis_set_id"))
    cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!surface_axis[0]) {
    cbf_failnez (cbf_require_column  (handle, "array_id"))
    cbf_failnez (cbf_new_row         (handle))
    cbf_failnez (cbf_set_value       (handle, array_id))
    cbf_failnez (cbf_require_column  (handle, "precedence"))
    cbf_failnez (cbf_set_integervalue(handle,1))
    cbf_failnez (cbf_require_column  (handle, "axis_set_id"))
    cbf_failnez (cbf_require_value   (handle, &surface_axis [0], "ELEMENT_X"))

  }
  
  if (!surface_axis[1]) {
  	cbf_failnez (cbf_require_column  (handle,"array_id"))    
    cbf_failnez (cbf_new_row         (handle))
    cbf_failnez (cbf_set_value       (handle, array_id))
    cbf_failnez (cbf_require_column  (handle, "precedence"))
    cbf_failnez (cbf_set_integervalue(handle,2))
    cbf_failnez (cbf_require_column  (handle, "axis_set_id"))
    cbf_failnez (cbf_require_value   (handle, &surface_axis [1], "ELEMENT_Y"))
  	
  }

  if (!surface_axis [0])

    return CBF_FORMAT;

  cbf_failnez (cbf_require_category   (handle, "array_structure_list_axis"))
  cbf_failnez (cbf_require_column     (handle, "axis_set_id"))
  cbf_failnez (cbf_require_row        (handle, surface_axis [0]))
  cbf_failnez (cbf_require_column     (handle, "axis_id"))
  cbf_failnez (cbf_require_value      (handle, &surface_axis [0], surface_axis[0]))
  cbf_failnez (cbf_require_column     (handle, "displacement"))
  cbf_failnez (cbf_require_doublevalue(handle, &displacement [0], 0.0))
  cbf_failnez (cbf_require_column     (handle, "displacement_increment"))
  cbf_failnez (cbf_require_doublevalue(handle, &(increment [0]), 0.0))

  if (surface_axis [1])
  {
    cbf_failnez (cbf_require_column     (handle, "axis_set_id"))
    cbf_failnez (cbf_require_row        (handle, surface_axis [1]))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_value      (handle, &surface_axis [1], surface_axis[1]))
    cbf_failnez (cbf_require_column     (handle, "displacement"))
    cbf_failnez (cbf_require_doublevalue(handle, &displacement [1], 0.0))
    cbf_failnez (cbf_require_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_require_doublevalue(handle, &(increment [1]), 0.0))
  }


    /* Construct the positioner */

  cbf_failnez (cbf_make_positioner (&positioner))

  errorcode = cbf_alloc ((void **) detector, NULL,
                        sizeof (cbf_detector_struct), 1);

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_require_category (handle, "diffrn_detector_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_detector_axis.detector_id
                              _diffrn_detector_axis.id  (deprecated) */

      errorcode = cbf_find_column (handle, "detector_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");

      if (errorcode)
      
        errorcode = cbf_require_column (handle, "detector_id");
      
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle, 0,
                                                positioner,
                                                axis_id, 1);
      }
  }


    /* Add the surface axes */

  if (!errorcode)

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [0], 0);

  if (!errorcode && surface_axis [1])

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [1], 0);


    /* Connect the axes */

  if (!errorcode)

    errorcode = cbf_connect_axes (positioner);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (positioner);

    return errorcode | cbf_free ((void **) detector, NULL);
  }

      /* Insert the cbf handle and element into the dectector */
    
  (*detector)->handle = handle;

  (*detector)->element = element_number;



    /* Copy the start and increment values into the surface axes */

  (*detector)->displacement [0] = displacement [0];
  (*detector)->displacement [1] = displacement [1];

  (*detector)->increment [0] = increment [0];
  (*detector)->increment [1] = increment [1];

  if (surface_axis [1])

    (*detector)->axes = 2;

  else

    (*detector)->axes = 1;

  for (axis = 0; axis < (*detector)->axes; axis++)

    for (row = 0; row < positioner->axes; row++)

      if (cbf_cistrcmp (positioner->axis [row].name,
                        surface_axis [axis]) == 0)
      {
        (*detector)->index [axis] = row;

        positioner->axis [row].increment = 0;

        break;
      }

  (*detector)->positioner = positioner;

  return 0;
}


  /* Construct a reference detector positioner */

int cbf_construct_reference_detector (cbf_handle    handle,
                            cbf_detector *detector,
                            unsigned int  element_number)
{
  int errorcode, precedence;

  unsigned int row, axis;

  const char *diffrn_id, *id, *this_id, *axis_id, *array_id;

  const char *surface_axis [2];

  double displacement [2], increment [2];

  cbf_positioner positioner;

  if (!detector)

    return CBF_ARGUMENT;


    /* Get the detector id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))


    /* Construct the detector surface */

  cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id))
  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  surface_axis [0] = surface_axis [1] = NULL;

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    if (surface_axis [precedence - 1])

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column (handle, "axis_set_id"))
    cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!surface_axis [0])

    return CBF_FORMAT;

  cbf_failnez (cbf_find_category   (handle, "array_structure_list_axis"))
  cbf_failnez (cbf_find_column     (handle, "axis_set_id"))
  cbf_failnez (cbf_find_row        (handle, surface_axis [0]))
  cbf_failnez (cbf_find_column     (handle, "axis_id"))
  cbf_failnez (cbf_get_value       (handle, &surface_axis [0]))
  if (cbf_find_column(handle,"reference_displacement")) {
    cbf_failnez(cbf_find_column(handle,"displacement"))
  } 
  cbf_failnez (cbf_get_doublevalue (handle, &displacement [0]))
  cbf_failnez (cbf_get_doublevalue (handle, &displacement [0]))
  cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
  cbf_failnez (cbf_get_doublevalue (handle, &increment [0]))

  if (surface_axis [1])
  {
    cbf_failnez (cbf_find_column     (handle, "axis_set_id"))
    cbf_failnez (cbf_find_row        (handle, surface_axis [1]))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_get_value       (handle, &surface_axis [1]))
    if (cbf_find_column(handle,"reference_displacement")) {
      cbf_failnez(cbf_find_column(handle,"displacement"))
    } 
    cbf_failnez (cbf_get_doublevalue (handle, &displacement [1]))
    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, &increment [1]))
  }


    /* Construct the positioner */

  cbf_failnez (cbf_make_positioner (&positioner))

  errorcode = cbf_alloc ((void **) detector, NULL,
                        sizeof (cbf_detector_struct), 1);

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_find_category (handle, "diffrn_detector_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_detector_axis.detector_id
                              _diffrn_detector_axis.id  (deprecated) */

      errorcode = cbf_find_column (handle, "detector_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle, 0,
                                                positioner,
                                                axis_id, -1);
      }
  }


    /* Add the surface axes */

  if (!errorcode)

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [0], 0);

  if (!errorcode && surface_axis [1])

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [1], 0);


    /* Connect the axes */

  if (!errorcode)

    errorcode = cbf_connect_axes (positioner);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (positioner);

    return errorcode | cbf_free ((void **) detector, NULL);
  }
  
    /* Insert the cbf handle and element into the dectector */
    
  (*detector)->handle = handle;

  (*detector)->element = element_number;


    /* Copy the start and increment values into the surface axes */

  (*detector)->displacement [0] = displacement [0];
  (*detector)->displacement [1] = displacement [1];

  (*detector)->increment [0] = increment [0];
  (*detector)->increment [1] = increment [1];

  if (surface_axis [1])

    (*detector)->axes = 2;

  else

    (*detector)->axes = 1;

  for (axis = 0; axis < (*detector)->axes; axis++)

    for (row = 0; row < positioner->axes; row++)

      if (cbf_cistrcmp (positioner->axis [row].name,
                        surface_axis [axis]) == 0)
      {
        (*detector)->index [axis] = row;

        positioner->axis [row].increment = 0;

        break;
      }

  (*detector)->positioner = positioner;

  return 0;
}


  /* Construct a reference detector positioner, 
     creating the necessary categories, and columns */

int cbf_require_reference_detector (cbf_handle    handle, cbf_detector      *detector,
                                                unsigned int      element_number)
{
  int errorcode, precedence;

  unsigned int row, axis;

  const char *diffrn_id, *id, *this_id, *axis_id, *array_id;

  const char *surface_axis [2];

  double displacement [2], increment [2];

  cbf_positioner positioner;

  if (!detector)

    return CBF_ARGUMENT;


    /* Get the detector id */

  cbf_failnez (cbf_require_diffrn_id (handle, &diffrn_id, "DIFFRN_ID"))

  cbf_failnez (cbf_require_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_require_column   (handle, "diffrn_id"))
  if (cbf_find_row (handle, diffrn_id))  {
  	 cbf_failnez(cbf_new_row(handle))
  	 cbf_failnez(cbf_set_value(handle,diffrn_id))
  }
  cbf_failnez (cbf_require_column   (handle, "id"))
  cbf_failnez (cbf_require_value    (handle, &id, diffrn_id))


    /* Construct the detector surface */

  cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id))
  cbf_failnez (cbf_require_category (handle, "array_structure_list"))
  cbf_failnez (cbf_require_column   (handle, "array_id"))

  surface_axis [0] = surface_axis [1] = NULL;
  

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    if (surface_axis [precedence - 1])

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column (handle, "axis_set_id"))
    cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!surface_axis[0]) {
    cbf_failnez (cbf_require_column  (handle, "array_id"))
    cbf_failnez (cbf_new_row         (handle))
    cbf_failnez (cbf_set_value       (handle, array_id))
    cbf_failnez (cbf_require_column  (handle, "precedence"))
    cbf_failnez (cbf_set_integervalue(handle,1))
    cbf_failnez (cbf_require_column  (handle, "axis_set_id"))
    cbf_failnez (cbf_require_value   (handle, &surface_axis [0], "ELEMENT_X"))
  }
  if (!surface_axis[1]){
    cbf_failnez (cbf_require_column  (handle,"array_id"))    
    cbf_failnez (cbf_new_row         (handle))
    cbf_failnez (cbf_set_value       (handle, array_id))
    cbf_failnez (cbf_require_column  (handle, "precedence"))
    cbf_failnez (cbf_set_integervalue(handle,2))
    cbf_failnez (cbf_require_column  (handle, "axis_set_id"))
    cbf_failnez (cbf_require_value   (handle, &surface_axis [1], "ELEMENT_Y"))
  	
  }

  if (!surface_axis [0])

    return CBF_FORMAT;

  cbf_failnez (cbf_require_category   (handle, "array_structure_list_axis"))
  cbf_failnez (cbf_require_column     (handle, "axis_set_id"))
  cbf_failnez (cbf_require_row        (handle, surface_axis [0]))
  cbf_failnez (cbf_require_column     (handle, "axis_id"))
  cbf_failnez (cbf_require_value      (handle, &surface_axis [0], surface_axis[0]))
  if (!cbf_find_column(handle, "reference_displacement") || 
      !cbf_require_column     (handle, "displacement")){
  	  cbf_failnez (cbf_require_doublevalue(handle, &displacement [0], 0.0))
  }
  else return CBF_NOTFOUND;
  cbf_failnez (cbf_require_column     (handle, "displacement_increment"))
  cbf_failnez (cbf_require_doublevalue(handle, &(increment [0]), 0.0))

  if (surface_axis [1])
  {
    cbf_failnez (cbf_require_column     (handle, "axis_set_id"))
    cbf_failnez (cbf_require_row        (handle, surface_axis [1]))
    cbf_failnez (cbf_require_column     (handle, "axis_id"))
    cbf_failnez (cbf_require_value      (handle, &surface_axis [1], surface_axis[1]))
    if (!cbf_find_column(handle, "reference_displacement") || 
        !cbf_require_column     (handle, "displacement")){
  	    cbf_failnez (cbf_require_doublevalue(handle, &displacement [1], 0.0))
    }
    else return CBF_NOTFOUND;
    cbf_failnez (cbf_require_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_require_doublevalue(handle, &(increment [1]), 0.0))
  }


    /* Construct the positioner */

  cbf_failnez (cbf_make_positioner (&positioner))

  errorcode = cbf_alloc ((void **) detector, NULL,
                        sizeof (cbf_detector_struct), 1);

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_require_category (handle, "diffrn_detector_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_detector_axis.detector_id
                              _diffrn_detector_axis.id  (deprecated) */

      errorcode = cbf_find_column (handle, "detector_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");

      if (errorcode)
      
        errorcode = cbf_require_column (handle, "detector_id");
      
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle, 0,
                                                positioner,
                                                axis_id, -1);
      }
  }


    /* Add the surface axes */

  if (!errorcode)

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [0], 0);

  if (!errorcode && surface_axis [1])

    errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [1], 0);


    /* Connect the axes */

  if (!errorcode)

    errorcode = cbf_connect_axes (positioner);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (positioner);

    return errorcode | cbf_free ((void **) detector, NULL);
  }

    /* Insert the cbf handle and element into the dectector */
    
  (*detector)->handle = handle;

  (*detector)->element = element_number;



    /* Copy the start and increment values into the surface axes */

  (*detector)->displacement [0] = displacement [0];
  (*detector)->displacement [1] = displacement [1];

  (*detector)->increment [0] = increment [0];
  (*detector)->increment [1] = increment [1];

  if (surface_axis [1])

    (*detector)->axes = 2;

  else

    (*detector)->axes = 1;

  for (axis = 0; axis < (*detector)->axes; axis++)

    for (row = 0; row < positioner->axes; row++)

      if (cbf_cistrcmp (positioner->axis [row].name,
                        surface_axis [axis]) == 0)
      {
        (*detector)->index [axis] = row;

        positioner->axis [row].increment = 0;

        break;
      }

  (*detector)->positioner = positioner;

  return 0;
}


  /* Free a detector */

int cbf_free_detector (cbf_detector detector)
{
  int errorcode = 0;
  
  void * memblock;
  
  memblock = (void *)detector;

  if (detector)

    errorcode = cbf_free_positioner (detector->positioner);

  return errorcode | cbf_free (&memblock, NULL);
}


  /* Update the pixel settings */

int cbf_update_pixel (cbf_detector detector, double index1,
                                             double index2)
{
  if (!detector)

    return CBF_ARGUMENT;

  detector->positioner->axis [detector->index [0]].start =
        index2 * detector->increment [0] + detector->displacement [0];

  if (detector->axes == 2)

    detector->positioner->axis [detector->index [1]].start =
        index1 * detector->increment [1] + detector->displacement [1];

  return 0;
}


  /* Get the beam center */

int cbf_get_beam_center (cbf_detector detector, double *index1,
                                                double *index2,
                                                double *center1,
                                                double *center2)
{
  double pixel00 [3], pixel01 [3], pixel10 [3], m [2][2], det, index [2];

  if (!detector)

    return CBF_ARGUMENT;

  if (detector->axes < 2)

    return CBF_NOTIMPLEMENTED;

  cbf_failnez (cbf_get_pixel_coordinates (detector, 0, 0,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, 0, 1,
                                                    &pixel01 [0],
                                                    &pixel01 [1],
                                                    &pixel01 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, 1, 0,
                                                    &pixel10 [0],
                                                    &pixel10 [1],
                                                    &pixel10 [2]))

  m [0][0] = pixel10 [0] - pixel00 [0];
  m [0][1] = pixel01 [0] - pixel00 [0];
  m [1][0] = pixel10 [1] - pixel00 [1];
  m [1][1] = pixel01 [1] - pixel00 [1];

  det = m [0][0] * m [1][1] - m [1][0] * m [0][1];

  if (det == 0.0)

    return CBF_UNDEFINED;

  index [0] = (-m [1][1] * pixel00 [0] + m [0][1] * pixel00 [1]) / det;
  index [1] =  (m [1][0] * pixel00 [0] - m [0][0] * pixel00 [1]) / det;

  if (index1)

    *index1 = index [0];

  if (index2)

    *index2 = index [1];

  if (center1)

    *center1 = index [0] * detector->increment [0];

  if (center2)

    *center2 = index [1] * detector->increment [1];

  return 0;

/*  a * delta01 + b * delta10 + pixel00 = (0 0 ?)

    a * delta01[0] + b * delta10[0] + pixel00[0] = 0
    a * delta01[1] + b * delta10[1] + pixel00[1] = 0

    (d01[0] d10[0]) (a) = -(p00[0])
    (d01[1] d10[1]) (b)    (p00[1])

    (a) = -(d01[0] d10[0])-1 (p00[0])
    (b)    (d01[1] d10[1])   (p00[1]) */
}


  /* Set the beam center */

int cbf_set_beam_center (cbf_detector detector, double *index1,
                                                double *index2,
                                                double *center1,
                                                double *center2)
{
  double oindex1, oindex2, ocenter1, ocenter2;
  
  double nindex1, nindex2, ncenter1, ncenter2;
    
  double psize1, psize2;
  
  unsigned int naxis1, naxis2;
  
  int sign1, sign2;
  
  cbf_handle handle;
  
  unsigned int element;
    
  const char *element_id;

  if (!detector)

    return CBF_ARGUMENT;

  if (detector->axes < 2)

    return CBF_NOTIMPLEMENTED;
   
  handle = detector->handle;
  
  element = detector->element;
  
  cbf_failnez(cbf_get_element_id(handle,element, &element_id))
  
  naxis1 = detector->index[1];
  
  naxis2 = detector->index[0];

  sign1 = detector->increment[1]>0.0?1.0:-1.0;
  
  sign2 = detector->increment[0]>0.0?1.0:-1.0;

  psize1 = detector->increment[1];
  
  if (psize1 < 0.) psize1 = -psize1;
  
  psize2 = detector->increment[0];
  
  if (psize1 < 0.) psize2 = -psize2;
  
  if (index1) {

  	nindex1 = *index1;

  } else {

  	if (center1 && psize1 != 0.) nindex1 = sign1*(*center1)/psize1;

  	else return CBF_ARGUMENT;
  
  }

  if (index2) {

  	nindex2 = *index2;

  } else {

  	if (center2 && psize2 != 0.) nindex2 = sign2*(*center2)/psize2;

  	else return CBF_ARGUMENT;
  
  }
  
  if (center1) {

  	ncenter1 = *center1;

  } else {

  	if (index1 && psize1 != 0.) ncenter1 = sign1*(*index1)*psize1;

  	else return CBF_ARGUMENT;
  
  }

  if (center2) {

  	ncenter2 = *center2;

  } else {

  	if (index2 && psize2 != 0.) ncenter2 = sign2*(*index2)*psize2;

  	else return CBF_ARGUMENT;
  
  }


  cbf_failnez(cbf_get_beam_center(detector, &oindex1, &oindex2, &ocenter1, &ocenter2))
  
  cbf_failnez(cbf_find_category(handle, "array_structure_list_axis"))
  
  cbf_failnez(cbf_find_column(handle, "axis_id"))
  
  if ( nindex1 < oindex1-1.e-6 || nindex1 > oindex1+1.e-6 ) {
  
    double olddisp;
  
    cbf_failnez(cbf_rewind_row(handle))

    cbf_failnez(cbf_find_row(handle,detector->positioner->axis[naxis1].name))
    
    cbf_failnez(cbf_require_column(handle, "displacement"))
    
    cbf_failnez(cbf_require_doublevalue(handle,&olddisp,0.0))
    
    cbf_failnez(cbf_set_doublevalue(handle, "%-f", 
    
      -(nindex1-oindex1)*detector->increment[1]  + detector->displacement[1]))    
  	
  }

  cbf_failnez(cbf_find_column(handle, "axis_id"))

  if ( nindex2 < oindex2-1.e-6 || nindex2 > oindex2+1.e-6 ) {
  
    double olddisp;

    cbf_failnez(cbf_rewind_row(handle))

    cbf_failnez(cbf_find_row(handle,detector->positioner->axis[naxis2].name))
    
    cbf_failnez(cbf_require_column(handle, "displacement"))

    cbf_failnez(cbf_require_doublevalue(handle,&olddisp,0.0))

    cbf_failnez(cbf_set_doublevalue(handle, "%-f", 

      -(nindex2-oindex2)*detector->increment[0]  + detector->displacement[0]))        
  	
  }
  
  if (!cbf_find_category(handle,"diffrn_data_frame")
  
    && !cbf_find_column(handle,"detector_element_id")
  
    && !cbf_find_row(handle,element_id)) {
  
      cbf_failnez(cbf_require_column(handle,"center_slow"))
  
      cbf_failnez(cbf_set_doublevalue(handle, "%-f",  nindex1*detector->increment[1]))
  
      cbf_failnez(cbf_require_column(handle,"center_fast"))
  
      cbf_failnez(cbf_set_doublevalue(handle, "%-f",  nindex2*detector->increment[0]))  

      cbf_failnez(cbf_require_column(handle,"center_units"))
  
      cbf_failnez(cbf_set_value(handle, "mm"))  
      
  }


  return 0;

}

  /* Set the reference beam center */

int cbf_set_reference_beam_center (cbf_detector detector, double *index1,
                                                double *index2,
                                                double *center1,
                                                double *center2)
{
  double oindex1, oindex2, ocenter1, ocenter2;
  
  double nindex1, nindex2, ncenter1, ncenter2;
    
  double psize1, psize2;
  
  unsigned int naxis1, naxis2;
  
  int sign1, sign2;
  
  cbf_handle handle;
  
  unsigned int element;
  
  const char *element_id;
  
  if (!detector)

    return CBF_ARGUMENT;

  if (detector->axes < 2)

    return CBF_NOTIMPLEMENTED;
   
  handle = detector->handle;
  
  element = detector->element;
  
  cbf_failnez(cbf_get_element_id(handle,element, &element_id))
  
  naxis1 = detector->index[1];
  
  naxis2 = detector->index[0];

  sign1 = detector->increment[1]>0.0?1.0:-1.0;
  
  sign2 = detector->increment[0]>0.0?1.0:-1.0;

  psize1 = detector->increment[1];
  
  if (psize1 < 0.) psize1 = -psize1;
  
  psize2 = detector->increment[0];
  
  if (psize1 < 0.) psize2 = -psize2;
  
  if (index1) {

  	nindex1 = *index1;

  } else {

  	if (center1 && psize1 != 0.) nindex1 = sign1*(*center1)/psize1;

  	else return CBF_ARGUMENT;
  
  }

  if (index2) {

  	nindex2 = *index2;

  } else {

  	if (center2 && psize2 != 0.) nindex2 = sign2*(*center2)/psize2;

  	else return CBF_ARGUMENT;
  
  }
  
  if (center1) {

  	ncenter1 = *center1;

  } else {

  	if (index1 && psize1 != 0.) ncenter1 = sign1*(*index1)*psize1;

  	else return CBF_ARGUMENT;
  
  }

  if (center2) {

  	ncenter2 = *center2;

  } else {

  	if (index2 && psize2 != 0.) ncenter2 = sign2*(*index2)*psize2;

  	else return CBF_ARGUMENT;
  
  }


  cbf_failnez(cbf_get_beam_center(detector, &oindex1, &oindex2, &ocenter1, &ocenter2))
  
  cbf_failnez(cbf_find_category(handle, "array_structure_list_axis"))
  
  cbf_failnez(cbf_find_column(handle, "axis_id"))
  
  if ( nindex1 < oindex1-1.e-6 || nindex1 > oindex1+1.e-6 ) {
  
    double olddisp;
  
    cbf_failnez(cbf_rewind_row(handle))

    cbf_failnez(cbf_find_row(handle,detector->positioner->axis[naxis1].name))
    
    cbf_failnez(cbf_require_column(handle, "reference_displacement"))
    
    cbf_failnez(cbf_require_doublevalue(handle,&olddisp,0.0))
    
    cbf_failnez(cbf_set_doublevalue(handle, "%-f", 
    
      -(nindex1-oindex1)*detector->increment[1]  + detector->displacement[1]))    
  	
  }

  cbf_failnez(cbf_find_column(handle, "axis_id"))

  if ( nindex2 < oindex2-1.e-6 || nindex2 > oindex2+1.e-6 ) {
  
    double olddisp;

    cbf_failnez(cbf_rewind_row(handle))

    cbf_failnez(cbf_find_row(handle,detector->positioner->axis[naxis2].name))
    
    cbf_failnez(cbf_require_column(handle, "reference_displacement"))

    cbf_failnez(cbf_require_doublevalue(handle,&olddisp,0.0))

    cbf_failnez(cbf_set_doublevalue(handle, "%-f", 

      -(nindex2-oindex2)*detector->increment[0]  + detector->displacement[0]))        
  	
  }

 if (!cbf_find_category(handle,"diffrn_detector_element")
  
    && !cbf_find_column(handle,"id")
  
    && !cbf_find_row(handle,element_id)) {
  
      cbf_failnez(cbf_require_column(handle,"reference_center_slow"))
  
      cbf_failnez(cbf_set_doublevalue(handle, "%-f",  nindex1*detector->increment[1]))
  
      cbf_failnez(cbf_require_column(handle,"reference_center_fast"))
  
      cbf_failnez(cbf_set_doublevalue(handle, "%-f",  nindex2*detector->increment[0]))  

      cbf_failnez(cbf_require_column(handle,"reference_center_units"))
  
      cbf_failnez(cbf_set_value(handle, "mm"))  
      
  }
  
  
  return 0;

}



  /* Get the detector distance: shortest distance to the plane */

int cbf_get_detector_distance (cbf_detector detector, double *distance)
{
  double normal [3], pixel00 [3];

  cbf_failnez (cbf_get_detector_normal (detector, &normal [0],
                                                  &normal [1],
                                                  &normal [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, 0, 0,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  if (distance)

      *distance = fabs (normal [0] * pixel00 [0] +
                        normal [1] * pixel00 [1] +
                        normal [2] * pixel00 [2]);

  return 0;
}


  /* Get the detector normal */

int cbf_get_detector_normal (cbf_detector detector, double *normal1,
                                                    double *normal2,
                                                    double *normal3)
{
  cbf_failnez (cbf_get_pixel_normal (detector, 0, 0, normal1,
                                                     normal2,
                                                     normal3))

  return 0;
}


  /* Calcluate the coordinates of a pixel */

int cbf_get_pixel_coordinates (cbf_detector detector, double index1,
                                                      double index2,
                                                      double *coordinate1,
                                                      double *coordinate2,
                                                      double *coordinate3)
{
  cbf_failnez (cbf_update_pixel (detector, index1, index2))

  cbf_failnez (cbf_calculate_position (detector->positioner,
                                                 0, 0, 0, 0, 0,
                                                 coordinate1,
                                                 coordinate2,
                                                 coordinate3))

  return 0;
}


  /* Get the pixel normal */

int cbf_get_pixel_normal (cbf_detector detector, double  index1,
                                                 double  index2,
                                                 double *normal1,
                                                 double *normal2,
                                                 double *normal3)
{
  double pixel00 [3], pixel01 [3], pixel10 [3], normal [3], length;

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 - 0.5,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 + 0.5,
                                                    &pixel01 [0],
                                                    &pixel01 [1],
                                                    &pixel01 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 + 0.5,
                                                    index2 - 0.5,
                                                    &pixel10 [0],
                                                    &pixel10 [1],
                                                    &pixel10 [2]))

  pixel01 [0] -= pixel00 [0];
  pixel01 [1] -= pixel00 [1];
  pixel01 [2] -= pixel00 [2];

  pixel10 [0] -= pixel00 [0];
  pixel10 [1] -= pixel00 [1];
  pixel10 [2] -= pixel00 [2];

  normal [0] = pixel01 [1] * pixel10 [2] - pixel10 [1] * pixel01 [2];
  normal [1] = pixel01 [2] * pixel10 [0] - pixel10 [2] * pixel01 [0];
  normal [2] = pixel01 [0] * pixel10 [1] - pixel10 [0] * pixel01 [1];

  length = normal [0] * normal [0] +
           normal [1] * normal [1] +
           normal [2] * normal [2];

  if (length <= 0.0)

    return CBF_UNDEFINED;

  length = sqrt (length);

  if (normal1)

    *normal1 = normal [0] / length;

  if (normal2)

    *normal2 = normal [1] / length;

  if (normal3)

    *normal3 = normal [2] / length;

  return 0;
}


  /* Calcluate the area of a pixel */

int cbf_get_pixel_area (cbf_detector detector, double index1,
                                               double index2,
                                               double *area,
                                               double *projected_area)
{
  double pixel00 [3], pixel01 [3], pixel10 [3], normal [3];

  double length, length00;

  if (!detector)

    return CBF_ARGUMENT;

  if (detector->axes < 2)

    return CBF_NOTIMPLEMENTED;

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 - 0.5,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 + 0.5,
                                                    &pixel01 [0],
                                                    &pixel01 [1],
                                                    &pixel01 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 + 0.5,
                                                    index2 - 0.5,
                                                    &pixel10 [0],
                                                    &pixel10 [1],
                                                    &pixel10 [2]))

  pixel01 [0] -= pixel00 [0];
  pixel01 [1] -= pixel00 [1];
  pixel01 [2] -= pixel00 [2];

  pixel10 [0] -= pixel00 [0];
  pixel10 [1] -= pixel00 [1];
  pixel10 [2] -= pixel00 [2];

  normal [0] = pixel01 [1] * pixel10 [2] - pixel10 [1] * pixel01 [2];
  normal [1] = pixel01 [2] * pixel10 [0] - pixel10 [2] * pixel01 [0];
  normal [2] = pixel01 [0] * pixel10 [1] - pixel10 [0] * pixel01 [1];

  length = normal [0] * normal [0] +
           normal [1] * normal [1] +
           normal [2] * normal [2];

  if (length <= 0.0)

    return CBF_UNDEFINED;

  length = sqrt (length);

  if (area)

    *area = length;

  if (projected_area)
  {
    length00 = pixel00 [0] * pixel00 [0] +
               pixel00 [1] * pixel00 [1] +
               pixel00 [2] * pixel00 [2];

    if (length00 <= 0.0)

      return CBF_UNDEFINED;

    length00 = sqrt (length00);

    *projected_area = fabs (pixel00 [0] * normal [0] +
                            pixel00 [1] * normal [1] +
                            pixel00 [2] * normal [2]) / length00;
  }

  return 0;
}

  /* Calcluate the size of a pixel from the detector element axis displacements */

int cbf_get_inferred_pixel_size (cbf_detector detector,
                                               int axis_number,
                                               double *psize)
{

  if (axis_number < 0) axis_number = detector->axes+1+axis_number;

  if (!detector || axis_number < 1 || detector-> axes < axis_number )

    return CBF_ARGUMENT;


  *psize = fabs( (detector-> increment)[axis_number-1] );

  return 0;

}

  /* Get the unit cell parameters */

int cbf_get_unit_cell (cbf_handle handle, double cell[6], double cell_esd[6])
{
    cbf_failnez(cbf_find_category    (handle, "cell"))
    cbf_failnez(cbf_rewind_row       (handle))

    if (cell) {

    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "length_a",    &(cell[0]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "length_b",    &(cell[1]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "length_c",    &(cell[2]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "angle_alpha", &(cell[3]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "angle_beta",  &(cell[4]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "angle_gamma", &(cell[5]),0.))

    }

    if (cell_esd) {

    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "length_a_esd",    &(cell_esd[0]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "length_b_esd",    &(cell_esd[1]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "length_c_esd",    &(cell_esd[2]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "angle_alpha_esd", &(cell_esd[3]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "angle_beta_esd",  &(cell_esd[4]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "angle_gamma_esd", &(cell_esd[5]),0.))

    }


    return 0;

}

  /* Set the unit cell parameters */

int cbf_set_unit_cell (cbf_handle handle, double cell[6], double cell_esd[6])
{
    const char * diffrn_id;
    const char * entry_id;

    cbf_failnez(cbf_get_diffrn_id    (handle, &diffrn_id))

    cbf_failnez(cbf_require_category (handle, "cell"))
    cbf_failnez(cbf_rewind_row       (handle))

    cbf_failnez(cbf_require_column   (handle, "entry_id"))

    entry_id = 0;
    if (cbf_get_value(handle, &entry_id) ||
        !entry_id || *entry_id == '\0') {
      cbf_failnez(cbf_set_value      (handle, diffrn_id))
    }

    if (cell) {

    cbf_failnez (cbf_require_column  (handle, "length_a"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[0]))
    cbf_failnez (cbf_require_column  (handle, "length_b"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[1]))
    cbf_failnez (cbf_require_column  (handle, "length_c"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[2]))
    cbf_failnez (cbf_require_column  (handle, "angle_alpha"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[3]))
    cbf_failnez (cbf_require_column  (handle, "angle_beta"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[4]))
    cbf_failnez (cbf_require_column  (handle, "angle_gamma"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[5]))

    }

    if (cell_esd) {

    cbf_failnez (cbf_require_column  (handle, "length_a_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[0]))
    cbf_failnez (cbf_require_column  (handle, "length_b_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[1]))
    cbf_failnez (cbf_require_column  (handle, "length_c_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[2]))
    cbf_failnez (cbf_require_column  (handle, "angle_alpha_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[3]))
    cbf_failnez (cbf_require_column  (handle, "angle_beta_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[4]))
    cbf_failnez (cbf_require_column  (handle, "angle_gamma_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[5]))

    }

    return 0;

}

  /* Get the reciprocal cell parameters */

int cbf_get_reciprocal_cell (cbf_handle handle, double cell[6], double cell_esd[6])
{

    cbf_failnez(cbf_find_category     (handle, "cell"))
    cbf_failnez(cbf_rewind_row        (handle))

    if (cell) {

    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_length_a",    &(cell[0]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_length_b",    &(cell[1]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_length_c",    &(cell[2]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_angle_alpha", &(cell[3]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_angle_beta",  &(cell[4]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_angle_gamma", &(cell[5]),0.))

    }

    if (cell_esd) {

    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_length_a_esd",    &(cell_esd[0]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_length_b_esd",    &(cell_esd[1]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_length_c_esd",    &(cell_esd[2]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_angle_alpha_esd", &(cell_esd[3]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_angle_beta_esd",  &(cell_esd[4]),0.))
    cbf_failnez (cbf_require_column_doublevalue (handle,
                        "reciprocal_angle_gamma_esd", &(cell_esd[5]),0.))
    }


    return 0;

}

  /* Set the reciprocal cell parameters */

int cbf_set_reciprocal_cell (cbf_handle handle, double cell[6], double cell_esd[6])
{
    const char * diffrn_id;
    const char * entry_id;

    cbf_failnez(cbf_get_diffrn_id    (handle, &diffrn_id))

    cbf_failnez(cbf_require_category (handle, "cell"))
    cbf_failnez(cbf_rewind_row       (handle))

    cbf_failnez(cbf_require_column   (handle, "entry_id"))

    entry_id = 0;
    if (cbf_get_value(handle, &entry_id) ||
        !entry_id || *entry_id == '\0') {
      cbf_failnez(cbf_set_value      (handle, diffrn_id))
    }

    if (cell) {

    cbf_failnez (cbf_require_column  (handle, "reciprocal_length_a"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[0]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_length_b"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[1]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_length_c"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[2]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_angle_alpha"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[3]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_angle_beta"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[4]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_angle_gamma"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell[5]))

    }

    if (cell_esd) {

    cbf_failnez (cbf_require_column  (handle, "reciprocal_length_a_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[0]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_length_b_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[1]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_length_c_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[2]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_angle_alpha_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[3]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_angle_beta_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[4]))
    cbf_failnez (cbf_require_column  (handle, "reciprocal_angle_gamma_esd"))
    cbf_failnez (cbf_set_doublevalue (handle, "%-.15g", cell_esd[5]))

    }

    return 0;

}

  /* Compute a cell volume */

int cbf_compute_cell_volume (double cell[6], double *volume) {

  double degtorad;

  degtorad = atan2(1.,1.)/45.;

  *volume =  cell[0]*cell[1]*cell[2]*
                     sqrt(1.
                          - cos(cell[3]*degtorad)*cos(cell[3]*degtorad)
                          - cos(cell[4]*degtorad)*cos(cell[4]*degtorad)
                          - cos(cell[5]*degtorad)*cos(cell[5]*degtorad)
                          + 2.*cos(cell[3]*degtorad)*cos(cell[4]*degtorad)*cos(cell[5]*degtorad));
  return 0;

}

  /* Compute a reciprocal cell */

int cbf_compute_reciprocal_cell (double cell[6], double rcell[6]){

   double volume, degtorad, radtodeg;
   #define acos_deg(x) (atan2(sqrt(1.-(x)*(x)),(x))*radtodeg)

   cbf_compute_cell_volume (cell, &volume);

   degtorad = atan2(1.,1.)/45.;
   radtodeg = 1./degtorad;

   if (volume <= 0. ) return CBF_ARGUMENT;

   rcell[0] = cell[1]*cell[2]*sin(cell[3]*degtorad)/volume;

   rcell[1] = cell[2]*cell[0]*sin(cell[4]*degtorad)/volume;

   rcell[2] = cell[0]*cell[1]*sin(cell[5]*degtorad)/volume;

   rcell[3] = acos_deg((cos(cell[4]*degtorad)*cos(cell[5]*degtorad) - cos(cell[3]*degtorad))/(sin(cell[4]*degtorad)*sin(cell[5]*degtorad)));

   rcell[4] = acos_deg((cos(cell[5]*degtorad)*cos(cell[3]*degtorad) - cos(cell[4]*degtorad))/(sin(cell[5]*degtorad)*sin(cell[3]*degtorad)));

   rcell[5] = acos_deg((cos(cell[3]*degtorad)*cos(cell[4]*degtorad) - cos(cell[5]*degtorad))/(sin(cell[3]*degtorad)*sin(cell[4]*degtorad)));

   return 0;

}

  /* Get the orientation matrix entry */

int cbf_get_orientation_matrix (cbf_handle handle, double ub_matrix[9])
{

  cbf_failnez(cbf_find_category    (handle, "diffrn_orient_matrix"));
  cbf_failnez(cbf_rewind_row       (handle));

  if (ub_matrix) {

    cbf_failnez (cbf_find_column   (handle, "UB[1][1]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[0])));
    cbf_failnez (cbf_find_column   (handle, "UB[1][2]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[1])));
    cbf_failnez (cbf_find_column   (handle, "UB[1][3]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[2])));
    cbf_failnez (cbf_find_column   (handle, "UB[2][1]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[3])));
    cbf_failnez (cbf_find_column   (handle, "UB[2][2]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[4])));
    cbf_failnez (cbf_find_column   (handle, "UB[2][3]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[5])));
    cbf_failnez (cbf_find_column   (handle, "UB[3][1]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[6])));
    cbf_failnez (cbf_find_column   (handle, "UB[3][2]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[7])));
    cbf_failnez (cbf_find_column   (handle, "UB[3][3]"));
    cbf_failnez (cbf_get_doublevalue     (handle, &(ub_matrix[8])));

  }

  return 0;

}

  /* Set the orientation matrix entry */

int cbf_set_orientation_matrix (cbf_handle handle, double ub_matrix[9])
{
    const char * diffrn_id;
    const char * UBdiffrn_id;

    cbf_failnez(cbf_get_diffrn_id    (handle, &diffrn_id))

    cbf_failnez(cbf_require_category (handle, "diffrn_orient_matrix"))
    cbf_failnez(cbf_rewind_row       (handle))

    cbf_failnez(cbf_require_column   (handle, "diffrn_id"))

    UBdiffrn_id = 0;
    if (cbf_get_value(handle, &UBdiffrn_id) ||
        !UBdiffrn_id || *UBdiffrn_id == '\0') {
      cbf_failnez(cbf_set_value      (handle, diffrn_id))
    }


  if (ub_matrix) {

    cbf_failnez (cbf_require_column   (handle, "UB[1][1]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[0]));
    cbf_failnez (cbf_require_column   (handle, "UB[1][2]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[1]));
    cbf_failnez (cbf_require_column   (handle, "UB[1][3]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[2]));
    cbf_failnez (cbf_require_column   (handle, "UB[2][1]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[3]));
    cbf_failnez (cbf_require_column   (handle, "UB[2][2]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[4]));
    cbf_failnez (cbf_require_column   (handle, "UB[2][3]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[5]));
    cbf_failnez (cbf_require_column   (handle, "UB[3][1]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[6]));
    cbf_failnez (cbf_require_column   (handle, "UB[3][2]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[7]));
    cbf_failnez (cbf_require_column   (handle, "UB[3][3]"));
    cbf_failnez (cbf_set_doublevalue     (handle, "%-.15g", ub_matrix[8]));

  }

  return 0;

}

#ifdef __cplusplus

}

#endif
