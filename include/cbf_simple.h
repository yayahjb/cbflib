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

#ifndef CBF_SIMPLE_H
#define CBF_SIMPLE_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf.h"

#define CBF_NOTIMEZONE 1440


  /* Geometry structures */

typedef enum
{
  CBF_ROTATION_AXIS, 
  CBF_TRANSLATION_AXIS, 
  CBF_GENERAL_AXIS
} 
cbf_axis_type;

typedef struct
{
  char *name, *depends_on;

  double vector [3], offset [3], start, increment, setting;

  cbf_axis_type type;
}
cbf_axis_struct;

typedef struct
{
  double matrix [3][4];

  cbf_axis_struct *axis;

  size_t axes;

  int matrix_is_valid, axes_are_connected;
}
cbf_positioner_struct;

typedef cbf_positioner_struct *cbf_positioner;

typedef cbf_positioner_struct *cbf_goniometer;

typedef struct
{
  cbf_positioner positioner;

  double displacement [2], increment [2];

  size_t axes, index [2];
  
  cbf_handle handle;
  
  int element;
}
cbf_detector_struct;

typedef cbf_detector_struct *cbf_detector;


  /* Read a template file */

int cbf_read_template (cbf_handle handle, FILE *stream);


  /* Get the diffrn.id entry */

int cbf_get_diffrn_id (cbf_handle handle, const char **diffrn_id);


  /* Change the diffrn.id entry in all the categories */

int cbf_set_diffrn_id (cbf_handle handle, const char *diffrn_id);


  /* Change the diffrn.id entry, creating it if necessary */

int cbf_require_diffrn_id (cbf_handle handle, const char **diffrn_id, 
                                          const char *default_id);


  /* Get the diffrn.crystal_id entry */

int cbf_get_crystal_id (cbf_handle handle, const char **crystal_id);


  /* Change the diffrn.crystal_id entry */

int cbf_set_crystal_id (cbf_handle handle, const char *crystal_id);


  /* Get the wavelength */

int cbf_get_wavelength (cbf_handle handle, double *wavelength);


  /* Set the wavelength */

int cbf_set_wavelength (cbf_handle handle, double wavelength);


  /* Get the polarization */

int cbf_get_polarization (cbf_handle handle, double *polarizn_source_ratio,
                                             double *polarizn_source_norm);


  /* Set the polarization */

int cbf_set_polarization (cbf_handle handle, double polarizn_source_ratio,
                                             double polarizn_source_norm);


  /* Get the divergence */

int cbf_get_divergence (cbf_handle handle, double *div_x_source,
                                           double *div_y_source,
                                           double *div_x_y_source);


  /* Set the divergence */

int cbf_set_divergence (cbf_handle handle, double div_x_source,
                                           double div_y_source,
                                           double div_x_y_source);


  /* Get the number of elements */

int cbf_count_elements (cbf_handle handle, unsigned int *elements);


  /* Get the element id */

int cbf_get_element_id (cbf_handle handle, unsigned int element_number,
                                           const char **element_id);
 

   /* Get the detector id */

int cbf_get_detector_id (cbf_handle handle, unsigned int element_number,
                                           const char **detector_id);
                                          

  /* Get the array id for a given detector element */

int cbf_get_array_id (cbf_handle handle, unsigned int element_number,
                                         const char **array_id);


 /* Get the pixel size of a detector element in a given direction */

int cbf_get_pixel_size(cbf_handle handle, unsigned int element_number,
                                          int axis_number,
                                          double * psize);


#define cbf_get_pixel_size_fs(handle, element_number, axis_number, psize)  \
        cbf_get_pixel_size((handle),(element_number),-(axis_number),(psize))
#define cbf_get_pixel_size_sf(handle, element_number, axis_number, psize)  \
        cbf_get_pixel_size((handle),(element_number),(axis_number),(psize))
  
  /* Set the pixel size of a detector element in a given direction */

int cbf_set_pixel_size(cbf_handle handle, unsigned int element_number,
                                          int axis_number,
                                          double psize);

#define cbf_set_pixel_size_fs(handle, element_number, axis_number, psize)  \
        cbf_set_pixel_size((handle),(element_number),-(axis_number),(psize))
#define cbf_set_pixel_size_sf(handle, element_number, axis_number, psize)  \
        cbf_set_pixel_size((handle),(element_number),(axis_number),(psize))

   
  /* Get the gain of a detector element */

int cbf_get_gain (cbf_handle handle, unsigned int element_number,
                                     double *gain, double *gain_esd);


  /* Set the gain of a detector element */

int cbf_set_gain (cbf_handle handle, unsigned int element_number,
                                     double gain, double gain_esd);


  /* Get the bin sizes of a detector element */

int cbf_get_bin_sizes(cbf_handle handle, unsigned int element_number,
                                          double * slowbinsize,
                                          double * fastbinsize);

  /* Set the bin sizes of a detector element */

int cbf_set_bin_sizes(cbf_handle handle, unsigned int element_number,
                                          double slowbinsize,
                                          double fastbinsize);


  /* Get the overload value of a detector element */

int cbf_get_overload (cbf_handle handle, unsigned int element_number,
                                         double *overload);


  /* Set the overload value of a detector element */

int cbf_set_overload (cbf_handle handle, unsigned int element_number,
                                         double overload);


  /* Get the integration time */

int cbf_get_integration_time (cbf_handle handle, unsigned int reserved,
                                                 double *time);


  /* Set the integration time */

int cbf_set_integration_time (cbf_handle handle, unsigned int reserved,
                                                 double time);


  /* Convert gregorian to julian date (in days) */

double cbf_gregorian_julian (int    year,
                             int    month,
                             int    day,
                             int    hour,
                             int    minute,
                             double second);
 
  /* Get the collection date and time (1) as seconds since January 1 1970 */

int cbf_get_timestamp (cbf_handle handle, unsigned int  reserved,
                                          double       *time,
                                          int          *timezone);


  /* Get the collection date and time (2) as individual fields */

int cbf_get_datestamp (cbf_handle handle, unsigned int  reserved,
                                          int          *year,
                                          int          *month,
                                          int          *day,
                                          int          *hour,
                                          int          *minute,
                                          double       *second,
                                          int          *timezone);


  /* Set the collection date and time (1) as seconds since January 1 1970 */

int cbf_set_timestamp (cbf_handle handle, unsigned int reserved,
                                          double       time, 
                                          int          timezone,
                                          double       precision);


  /* Set the collection date and time (2) as individual fields */

int cbf_set_datestamp (cbf_handle handle, unsigned int reserved,
                                          int          year,
                                          int          month,
                                          int          day,
                                          int          hour,
                                          int          minute,
                                          double       second,
                                          int          timezone,
                                          double       precision);

   
  /* Set the collection date and time (3) as current time to the second */

int cbf_set_current_timestamp (cbf_handle handle, unsigned int reserved,
                                                  int timezone);


  /* Get the image size */

int cbf_get_image_size (cbf_handle    handle,
                        unsigned int  reserved, 
                        unsigned int  element_number,
                        size_t       *ndimslow,
                        size_t       *ndimfast);
#define cbf_get_image_size_fs(handle, reserved, element_number, ndimfast, ndimslow) \
        cbf_get_image_size((handle),(reserved),(element_number),(ndimslow),(ndimfast))
#define cbf_get_image_size_sf(handle, reserved, element_number, ndimslow, ndimfast) \
        cbf_get_image_size((handle),(reserved),(element_number),(ndimslow),(ndimfast))

  /* Read a binary section into an image.  ndimslow is the 
                           slow dimension, ndimfast is fast dimension.*/

int cbf_get_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimfast);
#define cbf_get_image_fs(handle, reserved, element_number, array, elsize, elsign, ndimfast, ndimslow) \
        cbf_get_image ((handle),(reserved),(element_number),(array),(elsize),(elsign),(ndimslow),(ndimfast))
#define cbf_get_image_sf(handle, reserved, element_number, array, elsize, elsign, ndimslow, ndimfast) \
        cbf_get_image ((handle),(reserved),(element_number),(array),(elsize),(elsign),(ndimslow),(ndimfast))

  /* Read a binary section into a real image.  ndimslow is the 
                           slow dimension, ndimfast is fast dimension.*/

int cbf_get_real_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimfast);
#define cbf_get_real_image_fs(handle, reserved, element_number, array, elsize, ndimfast, ndimslow)\
        cbf_get_real_image ((handle),(reserved),(element_number),(array),(elsize),(ndimslow),(ndimfast)
#define cbf_get_real_image_sf(handle, reserved, element_number, array, elsize, ndimslow, ndimfast)\
        cbf_get_real_image ((handle),(reserved),(element_number),(array),(elsize),(ndimslow),(ndimfast)

  /* Get the 3D image size. ndimslow is the slowest dimension, 
                            ndimmid is the next faster dimension,
                            ndimfast is the fastest dimension */

int cbf_get_3d_image_size (cbf_handle    handle,
                        unsigned int  reserved,
                        unsigned int  element_number,
                        size_t       *ndimslow,
                        size_t       *ndimmid,
                        size_t       *ndimfast);
#define cbf_get_3d_image_size_fs(handle, reserved, element_number, ndimfast, ndimmid, ndimslow) \
        cbf_get_3d_image_size((handle),(reserved),(element_number),(ndimslow),(ndimmid),(ndimfast))
#define cbf_get_3d_image_size_sf(handle, reserved, element_number, ndimslow, ndimmid, ndimfast) \
        cbf_get_3d_image_size((handle),(reserved),(element_number),(ndimslow),(ndimmid),(ndimfast))


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
                   size_t        ndimfast);
#define cbf_get_3d_image_fs(handle, reserved, element_number, array, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_get_3d_image((handle),(reserved),(element_number),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast))
#define cbf_get_3d_image_sf(handle, reserved, element_number, array, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_get_3d_image((handle),(reserved),(element_number),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast))


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
                   size_t        ndimfast);
#define cbf_get_real_3d_image_fs(handle, reserved, element_number, array, elsize, ndimfast, ndimmid, ndimslow) \
        cbf_get_real_3d_image((handle),(reserved),(element_number),(array),(elsize),(ndimslow),(ndimmid),(ndimfast))
#define cbf_get_real_3d_image_sf(handle, reserved, element_number, array, elsize, ndimslow, ndimmid, ndimfast) \
        cbf_get_real_3d_image((handle),(reserved),(element_number),(array),(elsize),(ndimslow),(ndimmid),(ndimfast))


  /* Save an image.  ndimslow is the slow dimension, ndimfast is fast. */

int cbf_set_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   int           elsign,
                   size_t        ndimslow,
                   size_t        ndimfast);
#define cbf_set_image_fs(handle, reserved, element_number, compression, array, elsize, elsign, ndimfast, ndimslow) \
        cbf_set_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(elsign),(ndimslow),(ndimfast) )
#define cbf_set_image_sf(handle, reserved, element_number, compression, array, elsize, elsign, ndimslow, ndimfast) \
        cbf_set_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(elsign)(ndimslow),(ndimfast) )


  /* Save a real image.  ndimslow is the slow dimension, ndimfast is fast. */

int cbf_set_real_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array,
                   size_t        elsize,
                   size_t        ndimslow,
                   size_t        ndimfast);
#define cbf_set_real_image_fs(handle, reserved, element_number, compression, array, elsize, ndimfast, ndimslow) \
        cbf_set_real_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(ndimslow),(ndimfast) )
#define cbf_set_real_image_sf(handle, reserved, element_number, compression, array, elsize, ndimslow, ndimfast) \
        cbf_set_real_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(ndimslow),(ndimfast) )
        
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
                   size_t        ndimfast);
#define cbf_set_3d_image_fs(handle, reserved, element_number, compression, array, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_set_3d_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_3d_image_sf(handle, reserved, element_number, compression, array, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_set_3d_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_set_real_3d_image_fs(handle, reserved, element_number, compression, array, elsize, ndimfast, ndimmid, ndimslow) \
        cbf_set_real_3d_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_real_3d_image_sf(handle, reserved, element_number, compression, array, elsize, ndimslow, ndimmid, ndimfast) \
        cbf_set_real_3d_image ((handle),(reserved),(element_number),(compression),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_get_map_array_id_fs(handle, reserved, segment_id, array_id, ismask, require, ndimfast, ndimmid, ndimslow) \
        cbf_get_map_array_id ((handle),(reserved),(segment_id),(array_id),(ismask),(require),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_map_array_id_sf(handle, reserved, segment_id, array_id, ismask, require, ndimslow, ndimmid, ndimfast) \
        cbf_get_map_array_id ((handle),(reserved),(segment_id),(array_id),(ismask),(require),(ndimslow),(ndimmid),(ndimfast) )


  /* Get the map segment size.   ndimslow is the slowest dimension, 
                                 ndimmid is the next faster dimension,
                                 ndimfast is the fastest dimension */

int cbf_get_map_segment_size (cbf_handle    handle,
                        unsigned int  reserved,
                        const char   *segment_id,
                        int          *binary_id,
                        size_t       *ndimslow,
                        size_t       *ndimmid,
                        size_t       *ndimfast);
#define cbf_get_map_segment_size_fs(handle, reserved, segment_id, binary_id, ndimfast, ndimmid, ndimslow) \
        cbf_get_map_segment_size ((handle),(reserved),(segment_id),(binary_id),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_map_segment_size_sf(handle, reserved, segment_id, binary_id, ndimslow, ndimmid, ndimfast) \
        cbf_get_map_segment_size ((handle),(reserved),(segment_id),(binary_id),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_get_map_segment_fs(handle, reserved, segment_id, binary_id, array, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_get_map_segment ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_map_segment_sf(handle, reserved, segment_id, binary_id, array, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_get_map_segment ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_get_map_segment_mask_fs(handle, reserved, segment_id, binary_id, array, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_get_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_map_segment_mask_sf(handle, reserved, segment_id, binary_id, array, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_get_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_get_real_map_segment_fs(handle, reserved, segment_id, binary_id, array, elsize, ndimfast, ndimmid, ndimslow) \
        cbf_get_real_map_segment ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_real_map_segment_sf(handle, reserved, segment_id, binary_id, array, elsize, ndimslow, ndimmid, ndimfast) \
        cbf_get_real_map_segment ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )

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
                   size_t        ndimfast);
#define cbf_get_real_map_segment_mask_fs(handle, reserved, segment_id, binary_id, array, elsize, ndimfast, ndimmid, ndimslow) \
        cbf_get_real_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_real_map_segment_mask_sf(handle, reserved, segment_id, binary_id, array, elsize, ndimslow, ndimmid, ndimfast) \
        cbf_get_real_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_set_map_segment_fs(handle, reserved, segment_id, binary_id, compression, array, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_set_map_segment ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_map_segment_sf(handle, reserved, segment_id, binary_id, compression, array, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_set_map_segment ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_set_map_segment_mask_fs(handle, reserved, segment_id, binary_id, compression, array, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_set_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_map_segment_mask_sf(handle, reserved, segment_id, binary_id, compression, array, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_set_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_set_real_map_segment_fs(handle, reserved, segment_id, binary_id, compression, array, elsize, ndimfast, ndimmid, ndimslow) \
        cbf_set_real_map_segment ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_real_map_segment_sf(handle, reserved, segment_id, binary_id, compression, array, elsize, ndimslow, ndimmid, ndimfast) \
        cbf_set_real_map_segment ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_set_real_map_segment_mask_fs(handle, reserved, segment_id, binary_id, compression, array, elsize, ndimfast, ndimmid, ndimslow) \
        cbf_set_real_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_real_map_segment_mask_sf(handle, reserved, segment_id, binary_id, compression, array, elsize, ndimslow, ndimmid, ndimfast) \
        cbf_set_real_map_segment_mask ((handle),(reserved),(segment_id),(binary_id),(compression),(array),(elsize),(ndimslow),(ndimmid),(ndimfast) )


  /* Get the 3D array size. ndimslow is the slowest dimension, 
                            ndimmid is the next faster dimension,
                            ndimfast is the fastest dimension */

int cbf_get_3d_array_size (cbf_handle    handle,
                        unsigned int  reserved,
                        const char   *array_id,
                        size_t       *ndimslow,
                        size_t       *ndimmid,
                        size_t       *ndimfast);
#define cbf_get_3d_array_size_fs(handle, reserved, array_id, ndimfast, ndimmid, ndimslow) \
        cbf_get_3d_array_size ((handle),(reserved),(array_id),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_3d_array_size_sf(handle, reserved, array_id, ndimslow, ndimmid, ndimfast) \
        cbf_get_3d_array_size ((handle),(reserved),(array_id),(ndimslow),(ndimmid),(ndimfast) )


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
                   size_t        ndimfast);
#define cbf_get_3d_array_fs(handle, reserved, array_id, binary_id, array, eltype, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_get_3d_array ((handle),(reserved),(array_id),(binary_id),(array),(eltype),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_get_3d_array_sf(handle, reserved, array_id, binary_id, array, eltype, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_get_3d_array ((handle),(reserved),(array_id),(binary_id),(array),(eltype),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )



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
                   size_t        ndimfast);
#define cbf_set_3d_array_fs(handle, reserved, array_id, binary_id, compression, array, eltype, elsize, elsign, ndimfast, ndimmid, ndimslow) \
        cbf_set_3d_array ((handle),(reserved),(array_id),(binary_id),(compression),(array),(eltype),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )
#define cbf_set_3d_array_sf(handle, reserved, array_id, binary_id, compression, array, eltype, elsize, elsign, ndimslow, ndimmid, ndimfast) \
        cbf_set_3d_array ((handle),(reserved),(array_id),(binary_id),(compression),(array),(eltype),(elsize),(elsign),(ndimslow),(ndimmid),(ndimfast) )



  /* Get the setting of an axis */

int cbf_get_axis_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double       *start, 
                                             double       *increment);


  /* Get the reference setting of an axis */

int cbf_get_axis_reference_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double       *refsetting);


  /* Change the setting of an axis */

int cbf_set_axis_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double        start, 
                                             double        increment);


  /* Change the reference setting of an axis */

int cbf_set_axis_reference_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double        refsetting);


  /* Construct a goniometer */

int cbf_construct_goniometer (cbf_handle handle, 
                              cbf_goniometer *goniometer);


  /* Free a goniometer */

int cbf_free_goniometer (cbf_goniometer goniometer);


  /* Get the rotation axis */

int cbf_get_rotation_axis (cbf_goniometer goniometer, unsigned int  reserved,
                                                      double       *vector1,
                                                      double       *vector2,
                                                      double       *vector3);


  /* Get the rotation range */

int cbf_get_rotation_range (cbf_goniometer goniometer, unsigned int reserved,
                                                       double      *start,
                                                       double      *increment);


  /* Reorient a vector */

int cbf_rotate_vector (cbf_goniometer goniometer, unsigned int reserved,
                                                  double       ratio,
                                                  double       initial1,
                                                  double       initial2,
                                                  double       initial3,
                                                  double      *final1,
                                                  double      *final2,
                                                  double      *final3);


  /* Convert a vector to reciprocal space */

int cbf_get_reciprocal (cbf_goniometer goniometer, unsigned int reserved,
                                                   double       ratio,
                                                   double       wavelength,
                                                   double       real1,
                                                   double       real2,
                                                   double       real3,
                                                   double      *reciprocal1,
                                                   double      *reciprocal2,
                                                   double      *reciprocal3);


  /* Construct a detector positioner */

int cbf_construct_detector (cbf_handle    handle, 
                            cbf_detector *detector,
                            unsigned int  element_number);
                            

  /* Construct a reference detector positioner */

int cbf_construct_reference_detector (cbf_handle    handle,
                            cbf_detector *detector,
                            unsigned int  element_number);


 
  /* Construct a detector positioner, 
     creating the necessary categories, and columns */

int cbf_require_detector (cbf_handle    handle, cbf_detector      *detector,
                                                unsigned int      element_number);

 
  /* Construct a reference detector positioner, 
     creating the necessary categories, and columns */

int cbf_require_reference_detector (cbf_handle    handle, cbf_detector      *detector,
                                                unsigned int      element_number);


  /* Free a detector */

int cbf_free_detector (cbf_detector detector);


  /* Get the beam center */

int cbf_get_beam_center (cbf_detector detector, double *indexslow,
                                                double *indexfast,
                                                double *centerslow, 
                                                double *centerfast);
                                                
#define cbf_get_beam_center_sf(detector, indexslow, indexfast,    \
                                          centerslow, centerfast)  \
        cbf_get_beam_center((detector),(indexslow),(indexfast),    \
                                       (centerslow),(centerfast) )
#define cbf_get_beam_center_fs(detector, indexfast, indexslow,    \
                                          centerfast, centerslow)  \
        cbf_get_beam_center((detector),(indexslow),(indexfast),    \
                                       (centerslow),(centerfast) )


  /* Set the beam center */

int cbf_set_beam_center (cbf_detector detector, double *indexslow,
                                                double *indexfast,
                                                double *centerslow,
                                                double *centerfast);

#define cbf_set_beam_center_sf(detector, indexslow, indexfast,    \
                                          centerslow, centerfast)  \
        cbf_set_beam_center((detector),(indexslow),(indexfast),    \
                                       (centerslow),(centerfast) )
#define cbf_set_beam_center_fs(detector, indexfast, indexslow,    \
                                          centerfast, centerslow)  \
        cbf_set_beam_center((detector),(indexslow),(indexfast),    \
                                       (centerslow),(centerfast) )



  /* Set the reference beam center */

int cbf_set_reference_beam_center (cbf_detector detector, double *indexslow,
                                                double *indexfast,
                                                double *centerslow,
                                                double *centerfast);

#define cbf_set_reference_beam_center_sf(detector, indexslow, indexfast,    \
                                          centerslow, centerfast)            \
        cbf_set_reference_beam_center((detector),(indexslow),(indexfast),    \
                                       (centerslow),(centerfast) )
#define cbf_set_reference_beam_center_fs(detector, indexfast, indexslow,    \
                                          centerfast, centerslow)            \
        cbf_set_reference_beam_center((detector),(indexfast),(indexslow),    \
                                       (centerslow),(centerfast) )



  /* Get the detector distance */

int cbf_get_detector_distance (cbf_detector detector, double *distance);


  /* Get the detector normal */

int cbf_get_detector_normal (cbf_detector detector, double *normal1,
                                                    double *normal2,
                                                    double *normal3);


  /* Calcluate the coordinates of a pixel */

int cbf_get_pixel_coordinates (cbf_detector detector, double  indexslow,
                                                      double  indexfast,
                                                      double *coordinate1,
                                                      double *coordinate2,
                                                      double *coordinate3);
                                                      
#define cbf_get_pixel_coordinates_sf(detector, indexslow, indexfast,  \
                                coordinate1, coordinate2, coordinate3) \
        cbf_get_pixel_coordinates ((detector),(indexslow),(indexfast), \
                              (coordinate1), (coordinate2), (coordinate3)
#define cbf_get_pixel_coordinates_fs(detector, indexfast, indexslow,  \
                                coordinate1, coordinate2, coordinate3) \
        cbf_get_pixel_coordinates ((detector),(indexslow),(indexfast), \
                              (coordinate1), (coordinate2), (coordinate3)


  /* Get the pixel normal */

int cbf_get_pixel_normal (cbf_detector detector, double  indexslow,
                                                 double  indexfast,
                                                 double *normal1,
                                                 double *normal2,
                                                 double *normal3);
#define cbf_get_pixel_normal_sf(detector, indexslow, indexfast,  \
                                normal1, normal2, normal3) \
        cbf_get_pixel_normal ((detector),(indexslow),(indexfast), \
                              (normal1), (normal2), (normal3)
#define cbf_get_pixel_normal_fs(detector, indexfast, indexslow,  \
                                normal1, normal2, normal3) \
        cbf_get_pixel_normal ((detector),(indexslow),(indexfast), \
                              (normal1), (normal2), (coordinate3)


  /* Calcluate the area of a pixel */

int cbf_get_pixel_area (cbf_detector detector, double  indexslow,
                                               double  indexfast,
                                               double *area,
                                               double *projected_area);

#define cbf_get_pixel_area_sf(detector, indexslow, indexfast, area, projected_area) \
        cbf_get_pixel_area ((detector), (indexslow), (indexfast), (area), (projected_area))
#define cbf_get_pixel_area_fs(detector, indexfast, indexslow, area, projected_area) \
        cbf_get_pixel_area ((detector), (indexslow), (indexfast), (area), (projected_area))

  /* Calcluate the size of a pixel from the detector element axis displacements */

int cbf_get_inferred_pixel_size (cbf_detector detector, 
                                               int axis_number,
                                               double *psize);

#define cbf_get_inferred_pixel_size_fs(detector, axis_number, psize) \
        cbf_get_inferred_pixel_size((detector), -(axis_number), (psize))
#define cbf_get_inferred_pixel_size_sf(detector, axis_number, psize) \
        cbf_get_inferred_pixel_size((detector), (axis_number), (psize))
   
  /* Get the unit cell parameters */
  
int cbf_get_unit_cell (cbf_handle handle, double cell[6], double cell_esd[6] );

  /* Set the unit cell parameters */

int cbf_set_unit_cell (cbf_handle handle, double cell[6], double cell_esd[6] );
   
  /* Get the reciprocal cell parameters */
  
int cbf_get_reciprocal_cell (cbf_handle handle, double cell[6], double cell_esd[6] );

  /* Set the reciprocal cell parameters */

int cbf_set_reciprocal_cell (cbf_handle handle, double cell[6], double cell_esd[6] );

  /* Compute a cell volume */
  
int cbf_compute_cell_volume (double cell[6], double *volume);

  /* Compute a reciprocal cell */
  
int cbf_compute_reciprocal_cell (double cell[6], double rcell[6]); 

  /* Get the orientation matrix entry */

int cbf_get_orientation_matrix (cbf_handle handle, double ub_matrix[9]);

  /* Set the orientation matrix entry */

int cbf_set_orientation_matrix (cbf_handle handle, double ub_matrix[9]);


#ifdef __cplusplus

}

#endif

#endif /* CBF_SIMPLE_H */
