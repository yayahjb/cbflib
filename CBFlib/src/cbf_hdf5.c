/**********************************************************************
 * cbf_hdf5 -- read and write HDF5/NeXus files                        *
 *                                                                    *
 * Version 0.9.3 21 December 2012                                     *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2009, 2012 Herbert J. Bernstein                      *
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
 * The term "this software", as used in these Notices, refers to      *
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
#include "cbf_hdf5.h"
#include "cbf_ascii.h"
#include "cbf_binary.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_write.h"
#include "cbf_write_binary.h"
#include "cbf_read_mime.h"
#include "cbf_string.h"
#include "cbf_codes.h"
#include "cbf_alloc.h"
#include "cbf_simple.h"
#include "cbf_tree.h"
#include "cbf_hdf5_filter.h"
#ifdef CBF_USE_ULP
#include "cbf_ulp.h"
#endif
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <errno.h>


    /* CBF column -- NeXus DataSet Mapping Tables
     applicable to categories that map to
     datasets or to attributes

     In each of the names the constructs
     $(cifcolumn)
     $(cifcolumn,rownum)
     $(_category.cifcolumn,rownum)
     will be replaced by the value of the specified
     CIF tag at the current row, or at the specified
     row.  Failure to find the designated item, will
     cause a fail-through in $or to the next argument.

     The construct $axis(value) is replaced by x, y or z
     for values of 1, 2 and 3, respectively, after
     evaluating value.

     The construct $matchrow(tag,value) is replaced by the row number
     in the category for the tag that matches the value given

     The construct $matchrowmax(tag,value,controlcolumn) is replaced by the row number
     in the category for the tag that matches the value given, subject to maximizing
     the value of the control column in the category of tag.

     The construct $matchcase(tag1,value1,expression1,tag2,value2,expression2,...,expressionn)

     The contruct $or(arg1,arg2,arg3) is replaced by the first
     of the arguments that can be successfully evaluated

     The construct $equipment(value) is replaced by
     CBF_diffrn_detector__$(_diffrn_detector.id,0):NXdetector
     CBF_diffrn_measurement__$(_diffrn_measurement.diffrn_id,0):NXsample
     coordinate_system:NXcoordinate_system
     and $equipment_class(value) is replaced by
     NXdetector
     NXsample
     Nxcoordinate_system
     for the values "detector", "goniometer", or "general"
     respectively
     */

    /*  The top of the target tree is

     /CBF_diffrn_scan__SCANID:NXentry
     /CBF_scan_id="SCANID"
     /CBF_diffrn_id="DIFFRNID"
     /CBF_entry_id="ENTRYID"
     /instrument:NXinstrument

     */

    typedef struct {
        int    objtype;          /* CBF_H5_COLUMN_GROUP
                                  or CBF_H5_COLUMN_DATASET
                                  possibly ored with CBF_H5_TERMINATE
                                  */
        const char * h5pathelement;    /* An hdf5 path element
                                        and optional class*/
    } cbf_h5path_element;

    typedef struct {
        int    dsorat;       /* CBF_H5_COLUMN_DATASET
                              or CBF_H5_COLUMN_ATTRIBUTE */
        int    parent_index; /* index into the cbfcol_h5parent table
                              If the parent_index is -1, the new
                              items goes in at the same level
                              as the first item in the cbfcol_h5parent
                              as a sibling.
                              */

        char * cifcolumn;    /* the name of the cif column to be mapped */
        char * mappedname;   /* the hdf5 target name or NULL to use parent */
        char * units;        /* units or NULL */
        int numeric;         /* CBF_H5_TEXT, CBF_H5_FLOAT, CBF_H5_INTEGER */
        int special;         /* 0 for normal handling, 1 for special case,
                              add CBF_H5_TERMINATE for terminal */
    } cbf_colnxdsat;

    typedef struct {
        char * cifcategory;  /* the name of the cf category to be mapped */
        cbf_h5path_element * h5path;
        /* the array of nexus path elements used to map
         this category */
        cbf_colnxdsat  * nxmapping;
        /* the array of mappings of columns into datasets
         and attributes */
    } cbf_catnxmapping;


    /* column mappings for ARRAY_DATA */

    cbf_h5path_element cbf_nxmapping_array_data_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "data__$(array_id)_$(binary_id)"}
    };
    cbf_colnxdsat cbf_nxmapping_array_data[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "id", "CBF_array_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "binary_id", "CBF_binary_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "data", NULL,NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "header_contents",   "CBF_header_contents",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "header_convention", "CBF_header_convention",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,CBF_H5_TERMINATE}
    };

    /* column mappings for ARRAY_ELEMENT_SIZE */

    cbf_h5path_element cbf_nxmapping_array_element_size_data_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE,
            "$axis($(index))_pixel_size_$(array_id):m"}
    };
    cbf_colnxdsat cbf_nxmapping_array_element_size[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "id", "CBF_array_id",NULL,0,0},
        {CBF_H5_COLUMN_DATASET, 2, "size", "$axis($(index))_pixel_size_$(array_id)","m",CBF_H5_FLOAT,CBF_H5_TERMINATE}
    };

    /* column mappings for ARRAY_INTENSITIES */

    cbf_h5path_element cbf_nxmapping_array_intensities_data_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "data__$(array_id)_$(binary_id)"}
    };
    cbf_colnxdsat cbf_nxmapping_array_intensities[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "array_id", "CBF_array_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "binary_id", "CBF_binary_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "details", "CBF_array_intensities__details",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "gain", "CBF_array_intensities__gain",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "gain_esd", "CBF_array_intensities__gain_esd",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "linearity", "CBF_array_intensities__linearity",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "offset", "CBF_array_intensities__offset",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "scaling", "CBF_array_intensities__scaling",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "overload", "CBF_array_intensities__overload",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "undefined_value", "CBF_array_intensities__undefined_value",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "pixel_fast_bin_size", "CBF_array_intensities__pixel_fast_bin_size",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "pixel_slow_bin_size", "CBF_array_intensities__pixel_slow_bin_size",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "pixel_binning_method",
            "CBF_array_intensities__pixel_binning_method",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,CBF_H5_TERMINATE}
    };


    /* column mappings for ARRAY_STRUCTURE_LIST */

    cbf_h5path_element cbf_nxmapping_array_structure_list_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "CBF_array_structure_list__$(axis_set_id)"}
    };
    cbf_colnxdsat cbf_nxmapping_array_structure_list[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "axis_set_id", "CBF_array_structure_list__axis_set_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "array_id", "CBF_array_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "dimension", "CBF_array_structure_list__dimension",NULL,CBF_H5_INTEGER|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "direction", "CBF_array_structure_list__direction",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "index", "CBF_array_structure_list__index",NULL,CBF_H5_INTEGER|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "precedence", "CBF_array_structure_list__precedence",NULL,CBF_H5_INTEGER|CBF_H5_ARRAY,CBF_H5_TERMINATE}
    };

    /* column mappings for ARRAY_STRUCTURE_LIST_SECTION */

    cbf_h5path_element cbf_nxmapping_array_structure_list_section_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "CBF_array_structure_list_section__$(id)"}
    };
    cbf_colnxdsat cbf_nxmapping_array_structure_list_section[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "array_id", "CBF_array_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "id", "CBF_section_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "index", "CBF_array_structure_list_section__index",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "end", "CBF_array_structure_list_section__end",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "start", "CBF_array_structure_list_section__start",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "stride", "CBF_array_structure_list_section__stride",NULL,CBF_H5_INTEGER,CBF_H5_TERMINATE}
    };


    /* column mappings for ARRAY_STRUCTURE_LIST_AXIS */

    cbf_h5path_element cbf_nxmapping_array_structure_list_axis_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "CBF_axis__$(axis_id)"}
    };
    cbf_colnxdsat cbf_nxmapping_array_structure_list_axis[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "axis_id", "CBF_array_structure_list_axis__axis_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "axis_set_id", "CBF_array_structure_list_axis__axis_set_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "angle", "CBF_array_structure_list_axis__angle",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "angle_increment", "CBF_array_structure_list_axis__angle_FLOAT|CBF_H5_ARRAY",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "displacement", "CBF_array_structure_list_axis__displacement",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "fract_displacement", "CBF_array_structure_list_axis__fract_displacement",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "displacement_increment", "CBF_array_structure_list_axis__displacement_increment",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "fract_displacement_increment", "CBF_array_structure_list_axis__fract_displacement_increment",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "angular_pitch", "CBF_array_structure_list_axis__angular_pitch",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "radial_pitch", "CBF_array_structure_list_axis__radial_pitch",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "reference_angle", "CBF_array_structure_list_axis__reference_angle",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "reference_displacement", "CBF_array_structure_list_axis__reference_displacement",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,CBF_H5_TERMINATE}
    };

    /* column mappings for AXIS */

    cbf_h5path_element cbf_nxmapping_axis_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP,
            "$ifeq($(_axis.equipment,$match(_axis.id,$(axis_id))),detector,$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdectector,"
            "$(_axis.equipment,$match(_axis.id,$(axis_id))),goniometer,$or(CBF_diffrn_measurement__$(_diffrn_measurement.id,0),goniometer):NXsample,"
            "detector:NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "CBF_axis__$(id)"}
    };
    cbf_colnxdsat cbf_nxmapping_axis[] = {
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "depends_on", "depends_on",NULL,CBF_H5_TEXT,CBF_H5_COLUMN_PATH},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "offset[?]", "offset","mm",CBF_H5_FLOAT,CBF_H5_COLUMN_MAP_AXES},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "type", "transformation_type",NULL,CBF_H5_TEXT,CBF_H5_COLUMN_MAP_AXES},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "system", "system",NULL,CBF_H5_TEXT,CBF_H5_COLUMN_MAP_AXES},
        {CBF_H5_COLUMN_ATTRIBUTE, 3, "vector[?]", "vector",NULL,CBF_H5_FLOAT,CBF_H5_COLUMN_MAP_AXES|CBF_H5_TERMINATE},
    };

    /* column mappings for DIFFRN_DATA_FRAME */

    cbf_h5path_element cbf_nxmapping_diffrn_data_frame_path[] = {

        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_data_frame[] = {
        {CBF_H5_COLUMN_DATASET, 2, "array_section_id", "CBF_diffrn_data_frame__section_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "array_id", "CBF_diffrn_data_frame__section_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "binary_id", "CBF_diffrn_data_frame__binary_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "center_fast", "CBF_diffrn_data_frame__center_fast_slow["
            "$(_diffrn_scan_frame.frame_number,$matchrow(_diffrn_scan_frame.frame_id,_diffrn_data_frame.id),0,"
            "$matchrow(diffrn_detector_element.id,_diffrn_data_frame.detector_element_id)],$(center_units)",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "center_slow", "CBF_diffrn_data_frame__center_fast_slow["
            "$(_diffrn_scan_frame.frame_number,$matchrow(_diffrn_scan_frame.frame_id,$(_diffrn_data_frame.id)),1,"
            "$matchrow(diffrn_detector_element.id,_diffrn_data_frame.detector_element_id)],$(center_units)",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "details", "CBF_diffrn_data_frame__details["
            "$(_diffrn_scan_frame.frame_number,$matchrow(_diffrn_scan_frame.frame_id,_diffrn_data_frame.id),"
            "$matchrow(diffrn_detector_element.id,_diffrn_data_frame.detector_element_id)]",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,CBF_H5_TERMINATE}
    };


    /* column mappings for DIFFRN_DETECTOR */

    cbf_h5path_element cbf_nxmapping_diffrn_detector_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "CBF_diffrn_detector__id"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_detector[] = {
        {CBF_H5_COLUMN_DATASET, 0, "diffrn_id", "CBF_diffrn_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "id", "CBF_diffrn_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "details", "details",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "detector", "type",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "dtime", "deadtime",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "number_of_axes", "number_of_axes",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "type", "instrument",NULL,CBF_H5_TEXT,CBF_H5_TERMINATE},
    };


    /* column mappings for DIFFRN_DETECTOR_AXIS */

    cbf_h5path_element cbf_nxmapping_diffrn_detector_axis_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"},
        {CBF_H5_COLUMN_DATASET|CBF_H5_TERMINATE, "CBF_axis__$(axis_id)"}
    };
    cbf_colnxdsat * cbf_nxmapping_diffrn_detector_axis = NULL;


    /* column mappings for DIFFRN_DETECTOR_ELEMENT */

    cbf_h5path_element cbf_nxmapping_diffrn_detector_element_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_detector_element[] = {
        {CBF_H5_COLUMN_DATASET, 2, "id",
            "CBF_diffrn_detector_element__id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "detector_id",
            "CBF_diffrn_detector_element__detector_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "reference_center_fast",
            "CBF_diffrn_detector_element__reference_center_fast","$(reference_center_units)",CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "reference_center_slow",
            "CBF_diffrn_detector_element__reference_center_slow","$(reference_center_units)",CBF_H5_FLOAT|CBF_H5_ARRAY,CBF_H5_TERMINATE},
    };

    /* column mappings for DIFFRN_MEASUREMENT */

    cbf_h5path_element cbf_nxmapping_diffrn_measurement_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE, "instrument:NXinstrument"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_measurement[] = {
        {CBF_H5_COLUMN_DATASET, 0, "diffrn_id",
            "CBF_diffrn_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "details",
            "CBF_diffrn_measurement__details",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "device",
            "CBF_diffrn_measurement__device",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "device_details",
            "CBF_diffrn_measurement__device_details",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "device_type",
            "CBF_diffrn_measurement__device_type",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "id",
            "CBF_diffrn_measurement__id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "method",
            "CBF_diffrn_measurement__method",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 1, "number_of_axes",
            "CBF_diffrn_measurement__number_of_axes",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_DATASET, 1, "sample_detector_distance",
            "distance","mm",CBF_H5_FLOAT,CBF_H5_LINK},
        {CBF_H5_LINK, 1, "sample_detector_distance",
            "$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id):NXentry"
            "/instrument:NXinstrument"
            "/$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector",
            NULL,0,CBF_H5_LINK},
        {CBF_H5_COLUMN_DATASET, 1, "sample_detector_voffset",
            "CBF_diffrn_measurement__sample_detector_voffset","mm",CBF_H5_FLOAT,CBF_H5_SPECIAL},
        {CBF_H5_COLUMN_DATASET, 1, "specimen_support",
            "CBF_diffrn_measurement__specimen_support",NULL,CBF_H5_TEXT,0}
    };


    /* column mappings for DIFFRN_MEASUREMENT_AXIS */

    cbf_h5path_element cbf_nxmapping_diffrn_measurement_axis_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP,"instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE,
            "CBF_diffrn_measurement__$(measurement_id):NXsample"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_measurement_axis[] = {
        {CBF_H5_COLUMN_DATASET, 1, "axis_id",
            "CBF_axis__axis_id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 1, "measurement_device",
            "CBF_axis__measurement_device",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 1, "measurement_device",
            "CBF_axis__measurement_device",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,CBF_H5_TERMINATE},
    };

    /* column mappings for DIFFRN_RADIATION */

    cbf_h5path_element cbf_nxmapping_diffrn_radiation_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE, "collimator:NXcollimator"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_radiation[] = {
        {CBF_H5_COLUMN_DATASET, 2, "collimation",
            "CBF_diffrn_radiation__collimation",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 0, "diffrn_id",
            "CBF_diffrn_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "div_x_source",
            "divergence_x","deg",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "div_y_source",
            "divergence_y","deg",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "div_x_y_source",
            "CBF_diffrn_radiation__div_x_y_source","deg^2",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "filter_edge",
            "CBF_diffrn_radiation__filter_edge","angstroms",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "inhomogeneity",
            "CBF_diffrn_radiation__inhomogeneity","mm",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "monochromator",
            "CBF_diffrn_radiation__monochromator",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "polarisn_norm",
            "CBF_diffrn_radiation__polarisn_norm","deg",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "polarisn_ratio",
            "CBF_diffrn_radiation__polarisn_ratio",NULL,CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "polarizn_source_norm",
            "CBF_diffrn_radiation__polarizn_source_norm","deg",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "polarizn_source_ratio",
            "CBF_diffrn_radiation__polarizn_source_ratio",NULL,CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 2, "probe",
            "CBF_diffrn_radiation__probe",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "type",
            "CBF_diffrn_radiation__type",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "xray_symbol",
            "CBF_diffrn_radiation__xray_symbol",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 2, "wavelength_id",
            "CBF_diffrn_radiation__wavelength_id",NULL,CBF_H5_TEXT,CBF_H5_MORE},
        {CBF_H5_COLUMN_DATASET, 2, "$(_diffrn_radiation_wavelength.wavelength,$matchrowmax(_diffrn_radiation_wavelength.id,wavelength_id,_diffrn_radiation_wavelength.wt)","wavelength","angstroms",CBF_H5_FLOAT,0},
    };


    /* column mappings for DIFFRN_RADIATION_WAVELENGTH */

    cbf_h5path_element cbf_nxmapping_diffrn_radiation_wavelength_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE, "collimator:NXcollimator"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_radiation_wavelength[] = {
        {CBF_H5_COLUMN_DATASET, 2, "id",
            "CBF_diffrn_radiation_wavelength__id",NULL,CBF_H5_TEXT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "wavelength",
            "CBF_diffrn_radiation_wavelength__wavelength","angstroms",CBF_H5_FLOAT|CBF_H5_ARRAY,0},
        {CBF_H5_COLUMN_DATASET, 2, "wt",
            "CBF_diffrn_radiation_wavelength__wt",NULL,CBF_H5_FLOAT|CBF_H5_ARRAY,0}
    };


    /* column mappings for DIFFRN_SCAN */

    cbf_h5path_element cbf_nxmapping_diffrn_scan_path[] = {
        {CBF_H5_COLUMN_GROUP,"$or(CBF_diffrn_scan__$(_diffrn_scan.id),"
            "CBF_diffrn__$(_diffrn.id),"
            "CBF_entry__$(_entry.id),"
            "entry):NXentry"},
        {CBF_H5_COLUMN_GROUP, "instrument:NXinstrument"},
        {CBF_H5_COLUMN_GROUP|CBF_H5_TERMINATE, "$or(CBF_diffrn_detector__$(_diffrn_detector.id,0),detector):NXdetector"}
    };
    cbf_colnxdsat cbf_nxmapping_diffrn_scan[] = {
        {CBF_H5_COLUMN_DATASET, 0, "$(id)", "CBF_scan_id",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(date_end)", "end_time",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(date_start)", "start_time",NULL,CBF_H5_TEXT,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(integration_time)", "average_count_time","sec",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(_diffrn_scan_frame.frame_number,$matchrow(_diffrn_scan_frame.frame_id,$(frame_id_start))", "frame_start_number",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(_diffrn_scan_frame.frame_number,$matchrow(_diffrn_scan_frame.frame_id,$(frame_id_end))", "frame_end_number",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(frames)", "CBF_diffrn_scan__frames",NULL,CBF_H5_INTEGER,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(time_period)", "frame_time","sec",CBF_H5_FLOAT,0},
        {CBF_H5_COLUMN_DATASET, 0, "$(time_rstrt_inc$", "frame_restart_time","sec",CBF_H5_FLOAT,0}
    };

    /* Macro to check the given error code & print some
     useful output if it is set */

#define CBF_CHECK_ERROR(cbferror) \
{ \
const int __error = (cbferror); \
if (CBF_SUCCESS != __error) fprintf(stderr,__WHERE__": CBF error: %s\n",cbf_strerror(__error)); \
};



    /* Macros to get the current location in a file in form `file:line' */

#define __STR2(n) #n
#define __STR(n) __STR2(n)
#define __WHERE__ __FILE__":"__STR(__LINE__)

/*
Unconditionally call a function, always printing an error message if it fails.
Require a terminating semi-colon to make it behave as a statement.

To add debug-only output define a debug constant and use:
if (debug) {...}
not:
#ifdef debug
...
#endif
To ensure debug code is checked by the compiler.
*/
#define _CBF_CALL(exp) \
do { \
	const int err = (exp); \
	if (CBF_SUCCESS!=err) { \
		error |= err; \
		fprintf(stderr,"%s: error: %s\n", __WHERE__, cbf_strerror(err)); \
	} \
} while (0)

/*
List all valid scan types for use with the cbf->nexus conversion.
This is an implementation detail that should not be exposed in any headers.
*/
const unsigned int CBF_SCANTYPE1 = 0x1;
const unsigned int CBF_SCANTYPE_ALL = 0x1;


	/**
	Concatenate several null-terminated strings into a single string, with each component
	separated by one 'sep' character. A leading or trailing empty string will cause a
	leading or trailing 'sep' character to be written, the character may be repeated by
	inserting an empty string into the 'parts' array at the appropriate place. The 'parts'
	array must be terminated by a null pointer, not a pointer to an empty string.

	Examples:
	_cbf_str_join({0},c);
	→ ""
	_cbf_str_join({"string",0},c);
	→ "string"
	_cbf_str_join({"","",0},'#');
	→ "#"
	_cbf_str_join({"multi","part","string",0},'-');
	→ "multi-part-string"
	_cbf_str_join({"A","string","","with","more","separators",0},'_');
	→ "A_string__with_more_separators"
	_cbf_str_join({"","leading",0},'_');
	→ "_leading"
	_cbf_str_join({"trailing","",0},'_');
	→ "trailing_"

	Returns a string which must be free'd on success, NULL on failure.
	Creating an empty string is not failure to create a string.
	*/
	char * _cbf_strjoin(const char * const * const parts, const char sep)
	{
		/* define local variables & check the argument */
		size_t len = 0, n = 0;
		const char * const * p = parts;
		if (!parts) return NULL;
		/* get the combined length of all component strings, and the number of them */
		while (*p) {
			len += strlen(*p++);
			++n;
		}
		if (0 == n) {
			char * const str = malloc(sizeof(char));
			if (!str) return NULL;
			return strcpy(str,"");
		} else {
			/*
			Allocate a new string with enough room for every component of
			the path, N-1 1-character path seperators & a null-terminator.
			*/
			char * const str = malloc((len+n)*sizeof(char));
			if (!str) return NULL;
			char * c = str;
			for (p = parts; *p; ) {
				c = strcpy(c,*p) + strlen(*p);
				if (!*++p) break;
				*c++ = sep;
			}
			return str;
		}
	}


    /****************************************************************
    Store & manuipluate a set of keys identifying a set of data of interest.
    TODO: Fix CBF memory allocation routines to behave correctly, then use them to check for leaks
     ****************************************************************/

	/**
	Store some indices to a subset of the axes in a single contiguous memory block
	There may be only one variable length member, and it must appear last

	Use as:
	cbf_axisIndex_t * index = NULL;
	_cbf_realloc_axisIndex(&index,3);
	...
	unsigned int offset = 0;
	for (; offset != index->count; ++offset)
		index->value[offset] = offset;
     */
	typedef struct cbf_axisIndex_t {
		unsigned int c;
		unsigned int d[1];
	} cbf_axisIndex_t;

	static int _cbf_realloc_axisIndex(cbf_axisIndex_t * * const obj, const unsigned int count)
    {
		/* NOTE: avoid underflow if count==0 */
		*obj = realloc(*obj,sizeof(cbf_axisIndex_t)-sizeof(unsigned int)+sizeof(unsigned int)*(count));
		if (!*obj) return CBF_ALLOC;
		(*obj)->c = count;
		return CBF_SUCCESS;
	}

	static int _cbf_free_axisIndex(const cbf_axisIndex_t * const obj)
	{
		free((void*)obj);
		return CBF_SUCCESS;
	}

	/**
	Map an axis set ID to a direction, for use in extracting axis data for images.
         */
    typedef struct cbf_arrayAxisSet_t {
		unsigned int count;
		const char * * axis_set_id;
		unsigned int * precedence;
		unsigned int * dimension;
		const char * * direction;
	} cbf_arrayAxisSet_t;

	static cbf_arrayAxisSet_t _cbf_make_arrayAxisSet()
	{
		cbf_arrayAxisSet_t obj;
		obj.count = 0;
		obj.axis_set_id = NULL;
		obj.precedence = NULL;
		obj.dimension = NULL;
		obj.direction = NULL;
		return obj;
	}

	static void _cbf_free_arrayAxisSet(const cbf_arrayAxisSet_t obj)
	{
		free((void*)obj.axis_set_id);
		free((void*)obj.precedence);
		free((void*)obj.dimension);
		free((void*)obj.direction);
	}

	static int _cbf_insert_arrayAxisSet
			(cbf_arrayAxisSet_t * const obj,
			 const char * const axis_set_id,
			 unsigned int precedence,
			 unsigned int dimension,
			 const char * const direction)
	{
		if (!obj) {
			return CBF_ARGUMENT;
		} else {
			const unsigned int idx = obj->count++;
			int error = CBF_SUCCESS;
			if (!(obj->axis_set_id = realloc(obj->axis_set_id,obj->count*sizeof(char*)))) error |= CBF_ALLOC;
			if (!(obj->precedence = realloc(obj->precedence,obj->count*sizeof(unsigned int)))) error |= CBF_ALLOC;
			if (!(obj->dimension = realloc(obj->dimension,obj->count*sizeof(unsigned int)))) error |= CBF_ALLOC;
			if (!(obj->direction = realloc(obj->direction,obj->count*sizeof(char*)))) error |= CBF_ALLOC;
			if (CBF_SUCCESS == error) {
				obj->axis_set_id[idx] = axis_set_id;
				obj->precedence[idx] = precedence;
				obj->dimension[idx] = dimension;
				obj->direction[idx] = direction;
			}
			return error;
		}
        }

	static void _cbf_reset_arrayAxisSet(cbf_arrayAxisSet_t * const obj)
	{
		_cbf_free_arrayAxisSet(*obj);
		*obj = _cbf_make_arrayAxisSet();
        }

	/**
	Map an axis ID to an object, a dataset reference, a path to access it and the axis it depends on (or NULL).
	For use in converting goniometer & detector axes.

	Want to be able to partially order the list, with axes grouped by object and sorted by dependency chain.
	Require axis to appear after anything that depends on it, incremental swapping should be stable for acyclic graphs.
	*/
	typedef struct cbf_axis_t {
		unsigned int count;
		const char * * axis_id;
		const char * * object; /* ← maps to 'axis.equipment' */
		const char * * path; /* ← These need free'ing individually! */
		const char * * depends_on; /* ← match these against 'axis_id' */
		unsigned int * in_degree; /* ← for use in checking dependancy chains */
		int * is_leaf; /* ← a 'leaf' axis must have a valid dependency chain */
	} cbf_axis_t;

	static cbf_axis_t _cbf_make_axis()
	{
		cbf_axis_t obj;
		obj.count = 0;
		obj.axis_id = NULL;
		obj.object = NULL;
		obj.path = NULL;
		obj.depends_on = NULL;
		obj.in_degree = NULL;
		obj.is_leaf = NULL;
		return obj;
    }

	static void _cbf_free_axis(const cbf_axis_t obj)
    {
		const char * const * const path_end = obj.path + obj.count;
		const char * const * p = obj.path;
		for (; path_end != p; ++p) free((void*)(*p));
		free((void*)obj.axis_id);
		free((void*)obj.object);
		free((void*)obj.path);
		free((void*)obj.depends_on);
		free((void*)obj.in_degree);
		free((void*)obj.is_leaf);
	}

	static int _cbf_insert_axis
			(cbf_axis_t * const obj,
			 const char * const axis_id,
			 const char * const object,
			 const char * const path,
			 const char * const depends_on)
	{
		if (!obj) {
			return CBF_ARGUMENT;
		} else {
			int error = CBF_SUCCESS;
			const unsigned int idx = obj->count++;
			if (!(obj->axis_id = realloc(obj->axis_id,obj->count*sizeof(char*)))) error |= CBF_ALLOC;
			if (!(obj->object = realloc(obj->object,obj->count*sizeof(char*)))) error |= CBF_ALLOC;
			if (!(obj->path = realloc(obj->path,obj->count*sizeof(char*)))) error |= CBF_ALLOC;
			if (!(obj->depends_on = realloc(obj->depends_on,obj->count*sizeof(char*)))) error |= CBF_ALLOC;
			if (!(obj->in_degree = realloc(obj->in_degree,obj->count*sizeof(unsigned int*)))) error |= CBF_ALLOC;
			if (!(obj->is_leaf = realloc(obj->is_leaf,obj->count*sizeof(int*)))) error |= CBF_ALLOC;
			if (CBF_SUCCESS == error) {
				obj->axis_id[idx] = axis_id;
				obj->object[idx] = object;
				obj->path[idx] = path;
				obj->depends_on[idx] = depends_on;
				obj->in_degree[idx] = 0;
				obj->is_leaf[idx] = 0;
			}
			return error;
		}
	}

	static void _cbf_reset_axis(cbf_axis_t * const obj)
	{
		_cbf_free_axis(*obj);
		*obj = _cbf_make_axis();
	}

	/**
	Track data describing the current frame/scan that is being converted.
	May be removed once conversion code is more stable, with parameters passed one-by-one to functions.
	*/
	typedef struct cbf_key_t {
		double matrix [3][3];
		const char * scan; /* TODO: should be optional */
		const char * frame;
		const char * binary;
		const char * array; /* TODO: should be optional */
		const char * diffrn;
		const char * diffrn_detector;
		const char * diffrn_detector_element;
		const char * diffrn_measurement;
		const char * wavelength_id;
		const char * data_overload;
		const char * detector_dependency;
		const char * goniometer_dependency;
		unsigned int nScans;
		unsigned int nFrames;
		cbf_arrayAxisSet_t arrayAxisSet;
		cbf_axis_t axis;
	} cbf_key_t;

	static cbf_key_t _cbf_make_key()
	{
		cbf_key_t key;
		key.matrix[0][0] = 0.;
		key.matrix[0][1] = 0.;
		key.matrix[0][2] = 0.;
		key.matrix[1][0] = 0.;
		key.matrix[1][1] = 0.;
		key.matrix[1][2] = 0.;
		key.matrix[2][0] = 0.;
		key.matrix[2][1] = 0.;
		key.matrix[2][2] = 0.;
		key.scan = NULL;
		key.frame = NULL;
		key.binary = NULL;
		key.array = NULL;
		key.diffrn = NULL;
		key.diffrn_detector = NULL;
		key.diffrn_detector_element = NULL;
		key.diffrn_measurement = NULL;
		key.wavelength_id = NULL;
		key.data_overload = NULL;
		key.detector_dependency = NULL;
		key.goniometer_dependency = NULL;
		key.nScans = 0;
		key.nFrames = 0;
		key.arrayAxisSet = _cbf_make_arrayAxisSet();
		key.axis = _cbf_make_axis();
		return key;
    }

	static void _cbf_reset_key(cbf_key_t * const key)
	{
		/* don't touch the matrix - it's constant over scans */
		key->scan = NULL;
		key->frame = NULL;
		key->binary = NULL;
		key->array = NULL;
		key->diffrn = NULL;
		key->diffrn_detector = NULL;
		key->diffrn_detector_element = NULL;
		key->diffrn_measurement = NULL;
		key->wavelength_id = NULL;
		key->data_overload = NULL;
		key->detector_dependency = NULL;
		key->goniometer_dependency = NULL;
		key->nScans = 0;
		key->nFrames = 0;
		_cbf_reset_arrayAxisSet(&key->arrayAxisSet);
		_cbf_reset_axis(&key->axis);
	}

	static void _cbf_free_key(const cbf_key_t key)
	{
		_cbf_free_arrayAxisSet(key.arrayAxisSet);
		_cbf_free_axis(key.axis);
	}

    /****************************************************************
     The following section of code is extracted from J. Sloan's
     cbf_hdf5.i
     ****************************************************************/

    /*
     Some comparison functions for use in checking the content of
     HDF5 datasets/attributes

     Should return 0 on success, non-zero on failure
     */
#ifdef CBF_USE_ULP
	typedef struct cmp_double_param_t {
		int cmp_double_as_float; /* do I use the 64 bit version, if available? */
		unsigned int ulp32; /* 32-bit comparison parameter */
#ifndef NO_UINT64_TYPE
		uint64_t ulp64; /* 64-bit comparison parameter */
#endif
	} cmp_double_param_t;

	/** Compare two vectors of doubles */
	static int cmp_double(const void * const expected, const void * const existing, 
                          size_t length, const void * const params)
    {
		const cmp_double_param_t * const cmp_params = (cmp_double_param_t*)(params);
		const double * A = expected;
		const double * B = existing;

        /* go through each vector comparing all values */
#ifndef NO_UINT64_TYPE
		if (!cmp_params->cmp_double_as_float && cbf_has_ULP64()) {
			while (length && cbf_ULP64(*A++, *B++) < cmp_params->ulp64) --length;
		}
		else
#endif
		{
			while (length && cbf_ULP32(*A++, *B++) < cmp_params->ulp32) --length;
		}
		/* if any are not equal the loop will exit early and length is non-zero */
        return length;
    }
#else
    static int cmp_double(const void * a, const void * b, size_t N)
    {
        /* go through each vector comparing all values
         The allowed delta is 1.e-38+1.e-13*maxmimum of the
         summed pairs of absolute values
         */

        double delta = 0.;
        const double * da = a;
        const double * db = b;

        size_t i;

        for (i=0; i < N; i++) {
            if (fabs(da[i])+fabs(db[i]) > delta) delta = fabs(da[i])+fabs(db[i]);
            if (fabs(da[i]-db[i]) > 1.e-38+1.e-13*delta) return 1;
        }

        for (i=0; i < N-1; i++) {
            if (fabs(da[i]-da[i]) > 1.e-38+1.e-13*delta) return 1;
        }

        return 0;
    }
#endif

#ifdef CBF_USE_ULP
    static int cmp_int(const void * const expected,
                       const void * existing,
                       size_t length,
                       const void * const params)
#else
    static int cmp_int(const void * const expected,
                       const void * existing,
                       size_t length)
#endif
    {
		const int * A = expected;
		const int * B = existing;
        
		/* go through each vector comparing all values */
		while (length && *A++ == *B++) --length;
		/* if any are not equal the loop will exit early and length is non-zero */
		return length;
    }

#ifdef CBF_USE_ULP
    static int cmp_vlstring(const void * a, const void * b, size_t N, 
      const void * const params)
#else
    static int cmp_vlstring(const void * a, const void * b, size_t N)
#endif
    {
        const char * const * A = a;
		const char * const * B = b;
        
        /* go through each vector comparing all values */
        while (N && !cbf_cistrcmp(*A++, *B++)) --N;
        /* if any are not equal the loop will exit early and N is non-zero */
        return N;
    }

	/**
     Put a character into a buffer at a given position.

     Ensures the buffer is long enough to hold the new character, realloc'ing if needed, and then inserts it.

     \param c The character.
     \param buf A pointer to the realloc'able buffer.
     \param n A pointer to the current length of the buffer.
     \param k The offset to place the character <code>c</code> in.

     \return void
	 */
	void cbf_push_buf(const int c, char * * const buf, size_t * const n, size_t *k)
	{
        int errorcode=0;
        size_t old_nelem;
		assert(buf);
		assert(n);
		assert(k);
        old_nelem = *n;
		if (*k >= *n) {
			*n = 1+2 * (*k);
            errorcode = cbf_realloc((void **)buf, &old_nelem, 1, *n);
		}
        if (!errorcode) {
		  (*buf)[(*k)++] = c;
        }
	}

	/**
     Function to tokenise a pilatus v1.2 minicbf header

     Will split the header into null-terminated strings that are passed around via a given realloc'able buffer.

     Behviour is determined by <code>newline</code>, if <code>newline</code> is non-zero:
     A token starting with a digit, <code>[0-9]</code>, will cause a string consisting of digits
     and any number or combination of <code>[T:-.]</code> characters to be matched as a token.
     Otherwise, the stream will be tokenised according to the default rules.

     The default tokenisation method is to split the stream on groups of characters from the set <code>[#=:,() \t\f\v\r\n]</code>.
     Any number of adjacent <code>'\r'</code> and <code>'\n'</code> characters are compressed into a single
     newline (<code>"\n"</code>) token.
	 */


	static int _cbf_scan_pilatus_V1_2_miniheader
    (char * * const buf,
     size_t * const n,
     int * const newline,
     const int getRestOfLine,
     const char * * const string)
	{
#define cbf_sgetc(str) (**str ? *(*str)++ : 0)
#define cbf_seof(str) (!**str)
#define cbf_ungetc(str) (--(*str))
		int c = 0; /* current character */
		size_t k = 0; /* current line length */
		const char spaceChars[] = "#=:,() \t\f\v";
		const char newlineChars[] = "\n\r";

		/* check that sensible arguments are given */
		assert(buf);
		assert(n);
		assert(newline);
		assert(string);

		/*
         Skip space-equivalent characters to find something interesting.
         This ends with one of two states: a non-space character is found, or EOF.
         */
		do {
			c = cbf_sgetc(string);
			if (cbf_seof(string)) break;
			if (strchr(spaceChars,c)) continue;
			else break;
		} while (1);

		/* if I am at the end of the stream, free the buffer & return 0 */
		if (cbf_seof(string)) {
			free((void*)(*buf));
			*buf = 0;
			*n = 0;
			return CBF_SUCCESS;
		}

		/* I have a token, starting with the character c: read it */
		if (strchr(newlineChars,c)) {
			/* it's a newline token: consume all consecutive newline-equivalent characters */
			do {
				c = cbf_sgetc(string);
			} while (!cbf_seof(string) && strchr(newlineChars,c));
			if (!cbf_seof(string)) cbf_ungetc(string);
			cbf_push_buf('\n', buf, n, &k);
			*newline = 1;
		} else {
			/* It's a string token */
			if (getRestOfLine || (*newline && isdigit(c))) {
				/* scan to the end of the current line */
				do {
					cbf_push_buf(c, buf, n, &k);
					c = cbf_sgetc(string);
				} while (!cbf_seof(string) && !strchr(newlineChars,c));
				if (!cbf_seof(string)) cbf_ungetc(string);
			} else {
				/* scan it as an arbitrary token */
				do {
					cbf_push_buf(c, buf, n, &k);
					c = cbf_sgetc(string);
				} while (!cbf_seof(string) && !strchr(spaceChars,c) && !strchr(newlineChars,c));
				if (!cbf_seof(string)) cbf_ungetc(string);
			}
			*newline = 0;
		}
		/* null-terminate the token */
		cbf_push_buf('\0', buf, n, &k);

		return CBF_SUCCESS;
	}

	/**
     Helper function to check if a string is a valid (pilatus format) null-terminated date.

     Requires string of format:
     <code>YYYY-MM-DDThh:mm:ss.s+</code>
     <code>Y</code>: year
     <code>M</code>: month
     <code>D</code>: day
     <code>h</code>: hour
     <code>m</code>: minute
     <code>s</code>: second
     <code>+</code>: second may be fractional, any number of digits are allowed
     <code>[-T:]</code>: literal characters

     \param str The string to test.

     \return non-zero on success, zero otherwise.
	 */
	static int isDateTime(const char * str)
	{
		if (!str) return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('-'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('-'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if ('T'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (':'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (':'==*str) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		if (isdigit(*str)) ++str; else return 0;
		/* Optional '.s*' (fractional second) component: */
		if ('.'==*str) ++str; else return '\0'==*str;
		while (isdigit(*str)) ++str;
		return '\0'==*str;
	}


    /****************************************************************
     The following section of code is extracted from J. Sloan's
     cbf_hdf5_common.c
     ****************************************************************/

#define CBF_HDF5_DEBUG 0

	/*
	Call an HDF5 function unconditionally, reporting any errors.
	Only use this if 'func' should return -ve values on failure.
	This should only be defined within the cbf_H5* functions,
	as it isn't helpful everywhere.
	*/
#define CBF_H5CALL(func) \
do { \
	const int err = (func); \
	if (err < 0) { \
		error |= CBF_H5ERROR; \
		if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Error in '%s'\n",__WHERE__,#func); \
} \
} while (0)

	/** \brief Check the validity of an object identifier

     Function to check validity of a HDF5 identifier.
     HDF5's predefined types are never counted as valid by this function,
     so it can't be used to test the validity of a type constant.
     Types obtained by using H5Tcopy are safe to test.

     \param ID An HDF5 object identifier.

     \return Non-zero if the type is valid, zero otherwise.
     */
	int cbf_H5Ivalid(const hid_t ID)
	{
		const htri_t v = H5Iis_valid(ID);
		if (v < 0) fprintf(stderr, "%s:%d: H5Iis_valid call failed.\n", __FILE__, __LINE__);
		return v > 0;
	}

	/*
	find/create/free a HDF5 group if it's valid & possibly set the ID to an invalid identifier.
     */

	/** \brief Attempt to create a group

	<p>Helper function to attempt to create a HDF5 group identified by <code>name</code> and return an error
	code, to make error handling more consistant. This will fail if a link with the same name already exists
	in <code>parent</code>.</p>

	\param location The group that will contain the newly created group.
     \param group A pointer to a HDF5 ID type where the group will be stored.
     \param name The name that the group will be given.

     \return An error code.
     */
	int cbf_H5Gcreate(const hid_t location, hid_t * const group, const char * const name)
	{
		if (!group || !name || !cbf_H5Ivalid(location)) return CBF_ARGUMENT;

		/* check if the link exists */
		const htri_t l = H5Lexists(location, name, H5P_DEFAULT);
		if (l < 0) return CBF_H5ERROR;
		else if (!l) {
			/* no group exists: try to create it */
			*group = H5Gcreate2(location,name,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
			return cbf_H5Ivalid(*group) ? CBF_SUCCESS : CBF_H5ERROR;
		} else {
			/* something exists, hence I can't *create* anything: error */
			if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: A link of the same name already exists\n",__WHERE__);
			return CBF_H5ERROR;
		}
	}

	/** \brief Ensure a group exists

	<p>Checks for the existance of a group with the given <code>name</code> and <code>parent</code>. Will create the
	group if it cannot be found, or open it if it already exists. It is an error if a matching group cannot be found or
	created. This uses <code>cbf_H5Gcreate</code> to create any new groups.</p>

	\param location The group that will contain the newly created group.
     \param group A pointer to a HDF5 ID type where the group will be stored.
     \param name The name that the group will be given.

     \return An error code.
     */
	int cbf_H5Grequire(const hid_t location, hid_t * const group, const char * const name)
	{
		/* check the arguments */
		if (!group || !name || !cbf_H5Ivalid(location)) return CBF_ARGUMENT;

		/* check if the link exists */
		const htri_t l = H5Lexists(location, name, H5P_DEFAULT);
		if (l < 0) return CBF_H5ERROR;
		else if (!l) {
			/* no group exists: try to create it */
			return cbf_H5Gcreate(location,group,name);
		} else {
			/* something exists, check what it is */
			const htri_t e = H5Oexists_by_name(location, name, H5P_DEFAULT);
			if (e < 0) return CBF_H5ERROR;
			else if (!e) {
				/* The link exists but the object doesn't - remove the link & create the object */
				if (H5Ldelete(location, name, H5P_DEFAULT) < 0) return CBF_H5ERROR;
				else {
					return cbf_H5Gcreate(location,group,name);
				}
			} else {
				/* my object exists - check its type */
				hid_t g = H5Oopen(location, name, H5P_DEFAULT);
				if (H5I_GROUP == H5Iget_type(g)) {
					/* it's a group - return it */
					*group = g;
					return CBF_SUCCESS;
				} else {
					/* not a group - close the object & fail */
					H5Oclose(g);
					return CBF_H5DIFFERENT;
				}
			}
		}
	}

    /** \brief Close a HDF5 group

     Attempt to close a group, but don't modify the identifier that described it.

     \param ID The HDF5 group to be closed.

     \return An error code.
     */
	int cbf_H5Gfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Gclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		else return CBF_ARGUMENT;
	}

	/* Open/close a HDF5 file if it's valid & possibly set the ID to an invalid identifier - deliberately avoid find/create/free or
     get/set/clear naming convensions */

	/** \brief Attempt to open an HDF5 file by file name

	<p>Will try to open a file of the given name with suitable values for some of it's properties to make memory leaks
	less likely.</p>

	<p><em>Warning:</em> this function will destroy any existing data in the file, do not pass the name of any file
	containing data you want to keep.</p>

     \param file A pointer to an HDF5 ID where the newly opened file should be stored.
     \param name The name of the file to attempt to open.

     \return An error code.
     */
	int cbf_H5Fopen(hid_t * const file, const char * const name)
	{
		/* define variables & check args */
		int error = (!file || !name) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);

		/* check variables */
		reportFail(cbf_H5Ivalid(fapl), CBF_H5ERROR, error);

		/* do some work */
		reportFail(H5Pset_fclose_degree(fapl,H5F_CLOSE_STRONG)>=0, CBF_H5ERROR, error);
		/*
		in H5Fcreate
		 *	H5F_ACC_TRUNC:
			overwrites any previous file, losing any existing data
		 *	H5F_ACC_EXCL:
			fail if the file already exists, so data isn't lost
		fall back to H5Fopen to try to open the existing file safely
		*/
		reportFail(cbf_H5Ivalid(*file = H5Fcreate(name,H5F_ACC_TRUNC,H5P_DEFAULT,fapl)), CBF_H5ERROR, error);

		/* ensure variables are properly closed */
		if (cbf_H5Ivalid(fapl)) H5Pclose(fapl);

		/* done */
		return error;
	}

	/** \brief Close a HDF5 file

     Attempt to close a file, but don't modify the identifier that described it.

     \param ID The HDF5 file to be closed.

     \return An error code.
     */
	int cbf_H5Fclose(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Fclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		else return CBF_ARGUMENT;
	}

	/* Attributes */
    
    /** \brief Check for an attribute with the given space/type/value, or set one if it doesn't exist.
     
     Checks the existance of an attribute of the given name, creating it if it doesn't exist.
     
     Checks the size, type and value of an existing attribute to find out if it is the same as
     what was requested as determined by a custom comparison function which may use some extra data
     for more sophisticated tests.
     
     \param ID The HDF5 object that the attribute will be applied to.
     \param name The name of the attribute.
     \param rank The number of dimensions of the attribute data, 0 for scalar data.
     \param dim The length of each dimension, not used for scalar data.
     \param type The HDF5 type of the attribute data.
     \param value The data to be written to the attribute.
     \param buf A buffer to be used when reading an existing attribute of the same size.
     \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
     
     \sa cbf_H5Arequire_string
     
     \return An error code.
     */
	int cbf_H5Arequire_cmp
    (const hid_t ID,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hid_t type,
     const void * const value,
     void * const buf,
     int (*cmp)(const void * a, const void * b, size_t N))
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(ID) || !name || (!!rank && !dim) || rank<0) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t attrSpace = CBF_H5FAIL;
		hid_t attrType = type;
		hid_t attr = CBF_H5FAIL;
		cbf_reportFail(cbf_H5Screate(&attrSpace, rank, dim, 0), error);
        
		/* do some work */
		if (H5Aexists(ID,name)) {
			hid_t attr = H5Aopen(ID,name,H5P_DEFAULT);
			hid_t currSpace = H5Aget_space(attr);
			hid_t currType = H5Aget_type(attr);
			/* check everything was opened properly */
			if (!cbf_H5Ivalid(attr)) error |= CBF_H5ERROR;
			if (!cbf_H5Ivalid(currSpace)) error |= CBF_H5ERROR;
			if (!cbf_H5Ivalid(currType)) error |= CBF_H5ERROR;
			if (CBF_SUCCESS==error) {
                /* check that the dataspace is correct */
				const htri_t eq = H5Sextent_equal(currSpace,attrSpace);
				if (eq<0) error |= CBF_H5ERROR;
				else if (!eq) error |= CBF_H5DIFFERENT;
				else /* success */;
			}
			if (CBF_SUCCESS==error) {
                /* check the datatype is correct */
				const htri_t eq = H5Tequal(currType,attrType);
				if (eq<0) error |= CBF_H5ERROR;
				else if (!eq) error |= CBF_H5DIFFERENT;
				else /* success */;
			}
			/* check that the data is correct */
			if (CBF_SUCCESS==error) {
				const size_t N = H5Sget_simple_extent_npoints(currSpace);
				const size_t vlStr = H5Tis_variable_str(currType);
				H5Aread(attr,attrType,buf);
				if (!!cmp(value,buf,N)) {
                    fprintf(stderr,__WHERE__": Incorrect attribute value\n");
                    error |= CBF_H5DIFFERENT;
                }
				if (vlStr < 0) error |= CBF_H5ERROR;
				if (vlStr > 0) H5Dvlen_reclaim(currType, currSpace, H5P_DEFAULT, buf);
            }
			/* check local variables are properly closed */
			if (cbf_H5Ivalid(currSpace))  H5Sclose(currSpace);
			if (cbf_H5Ivalid(currType))  H5Tclose(currType);
		} else {
			reportFail(cbf_H5Ivalid(attr = H5Acreate2(ID,name,attrType,attrSpace,H5P_DEFAULT,H5P_DEFAULT)), CBF_H5ERROR, error);
			reportFail(H5Awrite(attr,attrType,value)>=0, CBF_H5ERROR, error);
		}
        
		/* check local variables are properly closed */
		if (cbf_H5Ivalid(attrSpace))  H5Sclose(attrSpace);
		if (cbf_H5Ivalid(attr))  H5Aclose(attr);
        
		/* done */
		return error;
	}


	/** \brief Check for an attribute with the given space/type/value, or set one if it doesn't exist.

	<p>Checks the existance of an attribute of the given name, size, type and value. Equal value is determined by a
	custom comparison function which may use some extra data for more sophisticated tests. A new attribute with the
	given properties will be created if none currently exist, the function will fail if an incompatible attribute
	exists.</p>

     \param ID The HDF5 object that the attribute will be applied to.
     \param name The name of the attribute.
     \param rank The number of dimensions of the attribute data, 0 for scalar data.
     \param dim The length of each dimension, not used for scalar data.
	\param fileType The HDF5 type of the attribute data in the file.
	\param memType The HDF5 type of the attribute data in memory.
     \param value The data to be written to the attribute.
     \param buf A buffer to be used when reading an existing attribute of the same size.
     \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
	\param cmp_params A pointer to a data structure which may be used by the comparison function.

     \return An error code.
     */
	int cbf_H5Arequire_cmp2
    (const hid_t ID,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
	 const hid_t fileType,
	 const hid_t memType,
     const void * const value,
     void * const buf,
	 int (*cmp)(const void *, const void *, size_t))
	{
		/* define variables & check args */
		int error = CBF_SUCCESS;
		hid_t attrSpace = CBF_H5FAIL;
		hid_t attr = CBF_H5FAIL;
		if (!cbf_H5Ivalid(ID)) error |= CBF_ARGUMENT;
		if (!name || (rank && !dim) || rank<0) error |= CBF_ARGUMENT;
		if (H5I_DATATYPE!=H5Iget_type(fileType)) error |= CBF_ARGUMENT;
		if (H5I_DATATYPE!=H5Iget_type(memType)) error |= CBF_ARGUMENT;
		if (!value) error |= CBF_ARGUMENT;
		cbf_reportFail(cbf_H5Screate(&attrSpace, rank, dim, 0), error);

		/* do some work */
		if (CBF_SUCCESS==error) {
		if (H5Aexists(ID,name)) {
			hid_t attr = H5Aopen(ID,name,H5P_DEFAULT);
			hid_t currSpace = H5Aget_space(attr);
			hid_t currType = H5Aget_type(attr);
			/* check everything was opened properly */
			if (!cbf_H5Ivalid(attr)) error |= CBF_H5ERROR;
			if (!cbf_H5Ivalid(currSpace)) error |= CBF_H5ERROR;
			if (!cbf_H5Ivalid(currType)) error |= CBF_H5ERROR;
			if (CBF_SUCCESS==error) {
                /* check that the dataspace is correct */
				const htri_t eq = H5Sextent_equal(currSpace,attrSpace);
				if (eq<0) error |= CBF_H5ERROR;
				else if (!eq) error |= CBF_H5DIFFERENT;
				else /* success */;
			}
			if (CBF_SUCCESS==error) {
                /* check the datatype is correct */
					const htri_t eq = H5Tequal(currType,fileType);
				if (eq<0) error |= CBF_H5ERROR;
				else if (!eq) error |= CBF_H5DIFFERENT;
				else /* success */;
			}
			/* check that the data is correct */
			if (CBF_SUCCESS==error) {
				const size_t N = H5Sget_simple_extent_npoints(currSpace);
				const size_t vlStr = H5Tis_variable_str(currType);
					void * const _buf = buf ? 0 : malloc(H5Sget_simple_extent_npoints(currSpace)*H5Tget_size(currType));
					void * const membuf = buf ? buf : _buf;
					H5Aread(attr,memType,membuf);
					if (cmp(value,membuf,N)) {
						if (0) fprintf(stderr,__WHERE__": Incorrect attribute value\n");
                    error |= CBF_H5DIFFERENT;
                }
				if (vlStr < 0) error |= CBF_H5ERROR;
					if (vlStr > 0) H5Dvlen_reclaim(currType, currSpace, H5P_DEFAULT, membuf);
					free(_buf);
            }
			/* check local variables are properly closed */
				cbf_H5Sfree(currSpace);
				cbf_H5Tfree(currType);
		} else {
				reportFail(cbf_H5Ivalid(attr = H5Acreate2(ID,name,fileType,attrSpace,H5P_DEFAULT,H5P_DEFAULT)), CBF_H5ERROR, error);
				reportFail(H5Awrite(attr,memType,value)>=0, CBF_H5ERROR, error);
			}
		}

		/* check local variables are properly closed */
		if (cbf_H5Ivalid(attrSpace))  H5Sclose(attrSpace);
		if (cbf_H5Ivalid(attr))  H5Aclose(attr);

		/* done */
		return error;
	}

        
        /** \brief Check for an attribute with the given space/type/value, or set one if it doesn't exist.
         
         <p>Checks the existance of an attribute of the given name, size, type and value. Equal value is determined by a
         custom comparison function which may use some extra data for more sophisticated tests. A new attribute with the
         given properties will be created if none currently exist, the function will fail if an incompatible attribute
         exists.</p>
         
         \param ID The HDF5 object that the attribute will be applied to.
         \param name The name of the attribute.
         \param rank The number of dimensions of the attribute data, 0 for scalar data.
         \param dim The length of each dimension, not used for scalar data.
         \param fileType The HDF5 type of the attribute data in the file.
         \param memType The HDF5 type of the attribute data in memory.
         \param value The data to be written to the attribute.
         \param buf A buffer to be used when reading an existing attribute of the same size.
         \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
         \param cmp_params A pointer to a data structure which may be used by the comparison function.
         
         \return An error code.
         */

        int cbf_H5Arequire_cmp2_ULP
        (const hid_t ID,
         const char * const name,
         const int rank,
         const hsize_t * const dim,
         const hid_t fileType,
         const hid_t memType,
         const void * const value,
         void * const buf,
         int (*cmp)(const void *, const void *, size_t, const void *),
         const void * const cmp_params)
        {
            /* define variables & check args */
            int error = CBF_SUCCESS;
            hid_t attrSpace = CBF_H5FAIL;
            hid_t attr = CBF_H5FAIL;
            if (!cbf_H5Ivalid(ID)) error |= CBF_ARGUMENT;
            if (!name || (rank && !dim) || rank<0) error |= CBF_ARGUMENT;
            if (H5I_DATATYPE!=H5Iget_type(fileType)) error |= CBF_ARGUMENT;
            if (H5I_DATATYPE!=H5Iget_type(memType)) error |= CBF_ARGUMENT;
            if (!value) error |= CBF_ARGUMENT;
            cbf_reportFail(cbf_H5Screate(&attrSpace, rank, dim, 0), error);
            
            /* do some work */
            if (CBF_SUCCESS==error) {
                if (H5Aexists(ID,name)) {
                    hid_t attr = H5Aopen(ID,name,H5P_DEFAULT);
                    hid_t currSpace = H5Aget_space(attr);
                    hid_t currType = H5Aget_type(attr);
                    /* check everything was opened properly */
                    if (!cbf_H5Ivalid(attr)) error |= CBF_H5ERROR;
                    if (!cbf_H5Ivalid(currSpace)) error |= CBF_H5ERROR;
                    if (!cbf_H5Ivalid(currType)) error |= CBF_H5ERROR;
                    if (CBF_SUCCESS==error) {
                        /* check that the dataspace is correct */
                        const htri_t eq = H5Sextent_equal(currSpace,attrSpace);
                        if (eq<0) error |= CBF_H5ERROR;
                        else if (!eq) error |= CBF_H5DIFFERENT;
                        else /* success */;
                    }
                    if (CBF_SUCCESS==error) {
                        /* check the datatype is correct */
                        const htri_t eq = H5Tequal(currType,fileType);
                        if (eq<0) error |= CBF_H5ERROR;
                        else if (!eq) error |= CBF_H5DIFFERENT;
                        else /* success */;
                    }
                    /* check that the data is correct */
                    if (CBF_SUCCESS==error) {
                        const size_t N = H5Sget_simple_extent_npoints(currSpace);
                        const size_t vlStr = H5Tis_variable_str(currType);
                        void * const _buf = buf ? 0 : malloc(H5Sget_simple_extent_npoints(currSpace)*H5Tget_size(currType));
                        void * const membuf = buf ? buf : _buf;
                        H5Aread(attr,memType,membuf);
                        if (cmp(value,membuf,N,cmp_params)) {
                                if (0) fprintf(stderr,__WHERE__": Incorrect attribute value\n");
                                error |= CBF_H5DIFFERENT;
                            }
                            if (vlStr < 0) error |= CBF_H5ERROR;
                            if (vlStr > 0) H5Dvlen_reclaim(currType, currSpace, H5P_DEFAULT, membuf);
                            free(_buf);
                        }
                        /* check local variables are properly closed */
                        cbf_H5Sfree(currSpace);
                        cbf_H5Tfree(currType);
                    } else {
                        reportFail(cbf_H5Ivalid(attr = H5Acreate2(ID,name,fileType,attrSpace,H5P_DEFAULT,H5P_DEFAULT)), CBF_H5ERROR, error);
                        reportFail(H5Awrite(attr,memType,value)>=0, CBF_H5ERROR, error);
                    }
                }
                
                /* check local variables are properly closed */
                if (cbf_H5Ivalid(attrSpace))  H5Sclose(attrSpace);
                if (cbf_H5Ivalid(attr))  H5Aclose(attr);
                
                /* done */
                return error;
            }

    static int cmp_string(const void * const a, const void * const b, const size_t N
#ifdef CBF_USE_ULP
                , const void * const params
#endif
    )
	{
		if (1 != N) return 1;
		else return strcmp(a,b);
	}

	/** \brief Check for a scalar string attribute with a given value, or set one if it doesn't exist.

	<p>Forwarding function that calls <code>cbf_H5Arequire_cmp</code> with the appropriate arguments to compare two
	strings. The <code>strcmp</code> function is used for string comparison, with a small wrapper to verify array
	length:</p>
	<!-- remember to replace tabs with 4 spaces! -->
<pre><code><span class="doxygen">/&#42;&#42; internal implementation of a function to compare two strings for equality &#42;/</span>
static int cmp_string
	(const void * const a,
	const void * const b,
	const size_t N,
	const void * const params)
{
	<span class="comment">/&#42; first ensure the arrays have one element each &#42;/</span>
	if (1 != N) return 1;
	<span class="comment">/&#42; then forward to 'strcmp' for the actual comparison &#42;/</span>
	else return strcmp(a,b);
}</code></pre>

     \param location HDF5 object to which the string attribute should/will belong.
     \param name The name of the attribute.
     \param value The value which the attribute should/will have.

     \return An error code.
     */
	/* TODO: allow a buffer to be provided to avoid malloc/free */
	int cbf_H5Arequire_string
    (const hid_t location,
     const char * const name,
     const char * const value)
	{
		int error = CBF_SUCCESS;
		if (!value) error |= CBF_ARGUMENT;
		if (CBF_SUCCESS==error) {
		hid_t h5atype = CBF_H5FAIL;
		error |= cbf_H5Tcreate_string(&h5atype,strlen(value));
#ifdef CBF_USE_ULP
        error |= cbf_H5Arequire_cmp2_ULP(location,name,0,0,h5atype,h5atype,value,0,cmp_string,0);
#else
        error |= cbf_H5Arequire_cmp2(location,name,0,0,h5atype,h5atype,value,0,cmp_string);
#endif
		cbf_H5Tfree(h5atype);
		}
		return error;
	}

	/*
	find/create/free hdf5 datasets without directly using hdf5 API.
	TODO: function to support arbitrary dataspace/dcpl, and forward to it.
	*/

	/** \brief Creates a new dataset in the given location.

	<p>The <code>dataset</code> parameter gives a location to store the dataset for use by the caller, for example to
	add an attribute to it. If non-zero the returned handle MUST be free'd by the caller with
	<code>cbf_H5Dfree</code>.</p>

	<p>This function will fail if a link with the same name already exists in <code>location</code>.</p>

     \param location The hdf5 group/file in which to put the dataset.
     \param dataset An optional pointer to a location where the dataset handle should be stored for further use.
	\param name The name of the new dataset.
     \param rank The rank of the data, must be equal to the length of the \c dim and \c max arrays, if they are given.
     \param dim The dimensions of the data, pointer to an array of length \c rank which should where
     \c dim[i] \> 0 for \c i = [0, \c rank ), unused if \c rank == 0.
     \param max The maximum size of each dimension, pointer or an array of length \c rank where
     \c dim[i] \<= \c max[i] \<= \c H5S_UNLIMITED for \code i = [0, rank) \endcode, unused if \code rank == 0 \endcode.
     \param chunk The chunk size for the dataset, as a pointer to an array of length \c rank (or \c 0 if chunking should not be enabled).
	\param type The type of each data element in the file.

     \return An error code.
     */
	int cbf_H5Dcreate
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max,
     const hsize_t * const chunk,
     const hid_t type)
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(location) || !name || (rank && !dim) || rank<0 || H5I_DATATYPE!=H5Iget_type(type)) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t dataSpace = CBF_H5FAIL;
		hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);
		hid_t dataset_local = CBF_H5FAIL;

		/* check variables are valid */
		cbf_reportFail(cbf_H5Screate(&dataSpace, rank, dim, max), error);
		if (!cbf_H5Ivalid(dcpl)) error |= CBF_H5ERROR;
		if (rank>0 && max) {
			const hsize_t * m;
			for (m = max; m != max+rank; ++m) {
				if (H5S_UNLIMITED == *m) {
					if (!chunk) error |= CBF_H5FAIL;
				}
			}
		}

		/* allow dataset to be chunked */
		if (CBF_SUCCESS==error && rank && chunk)
			if (H5Pset_chunk(dcpl,rank,chunk)<0) error |= CBF_H5ERROR;

		/* create the dataset */
		if (CBF_SUCCESS==error && 0==H5Lexists(location,name,H5P_DEFAULT)) {
		    dataset_local = H5Dcreate2(location,name,type,dataSpace,H5P_DEFAULT,dcpl,H5P_DEFAULT);
			if (!cbf_H5Ivalid(dataset_local)) error |= CBF_H5ERROR;
		} else error |= CBF_H5ERROR;

		/* if the dataset object is requested then return it, otherwise close it */
		if (CBF_SUCCESS==error && NULL!=dataset) *dataset = dataset_local;
		else if (cbf_H5Ivalid(dataset_local)) H5Dclose(dataset_local);

		/* check remaining local variables are properly closed */
		if (cbf_H5Ivalid(dataSpace)) H5Sclose(dataSpace);
		if (cbf_H5Ivalid(dcpl)) H5Pclose(dcpl);

		/* done */
		return error;
	}
    
    
    
	/** \brief Look for a dataset with the given properties.
     
     Succeeds without returning a valid dataset ID if no dataset exists and fails if one with different properties exists.
     Finding that the dataset doesn't exist is not a failure - the function worked and returned useful information.
     So, if it returns \c CBF_SUCCESS then the dataset must be free'd at some point, otherwise it doesn't need to be free'd.
     
     Use as:
     \code
     int error = cbf_H5Dfind(., &dataset, ...);
     if (CBF_SUCCESS==error) {
     if (cbf_H5Ivalid(dataset)) {
     use_existing_dataset(dataset);
     } else {
     cbf_H5Dcreate(...);
     use_new_datset(dataset);
     }
     } else {
     handle_error(error);
     }
     \endcode
     
     \param location The hdf5 group/file in which to put the dataset.
     \param dataset A pointer to a HDF5 object identifier that is set to the location of a valid object or an invalid value if the function
     succeeds, otherwise is left in an undefined state.
     \param name The name of the existing/new dataset.
     \param rank See \c cbf_H5Dcreate
     \param dim See \c cbf_H5Dcreate
     \param max See \c cbf_H5Dcreate
     \param chunk See \c cbf_H5Dcreate
     \param type See \c cbf_H5Dcreate
     
     \sa cbf_H5Dcreate
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dwrite
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code indicating whether the function successfully determined the presence (or otherwise) of an appropriate dataset.
     */
	int cbf_H5Dfind
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max,
     const hsize_t * const chunk,
     const hid_t type)
	{
		/* check the arguments */
		if (!cbf_H5Ivalid(location) || !dataset || !name || (!!rank && !dim) || rank<0) return CBF_ARGUMENT;
        
		/* check if the link exists */
		const htri_t l = H5Lexists(location, name, H5P_DEFAULT);
		if (l < 0) return CBF_H5ERROR;
		else if (!l) {
			*dataset = CBF_H5FAIL;
			return CBF_SUCCESS;
		} else {
			/* check if the group exists */
			const htri_t e = H5Oexists_by_name(location, name, H5P_DEFAULT);
			if (e < 0) return CBF_H5ERROR;
			else if (!e) {
				/* The link exists but the object doesn't - try to remove the link & tell the caller that there is no dataset */
				if (H5Ldelete(location, name, H5P_DEFAULT) < 0) return CBF_H5ERROR;
				else {
					*dataset = CBF_H5FAIL;
					return CBF_SUCCESS;
				}
			} else {
				/* my object exists - check its type */
				hid_t g = H5Oopen(location, name, H5P_DEFAULT);
				if (H5I_DATASET == H5Iget_type(g)) {
					int error = CBF_SUCCESS;
					/* it's a dataset - check its properties */
					hid_t currSpace = H5Dget_space(g);
					hid_t currType = H5Dget_type(g);
					hid_t dataSpace = CBF_H5FAIL;
					cbf_reportFail(cbf_H5Screate(&dataSpace, rank, dim, max), error);
					if (CBF_SUCCESS==error) {
						/* Check space */
						const htri_t eq = H5Sextent_equal(currSpace,dataSpace);
						if (eq < 0) error |= CBF_H5ERROR;
						else if (!eq) error |= CBF_H5DIFFERENT;
						else /* success */;
					}
					/* TODO: Check chunk? */
					if (CBF_SUCCESS==error) {
				 		/* check the datatype is correct */
						const htri_t eq = H5Tequal(currType,type);
						if (eq<0) error |= CBF_H5ERROR;
						else if (!eq) error |= CBF_H5DIFFERENT;
						else /* success */;
					}
					if (cbf_H5Ivalid(currType))H5Tclose(currType);
					if (cbf_H5Ivalid(currSpace))H5Sclose(currSpace);
					if (cbf_H5Ivalid(dataSpace))H5Sclose(dataSpace);
					*dataset = CBF_SUCCESS==error ? g : CBF_H5FAIL;
					return error;
				} else {
					/* not a dataset - close the object & fail */
					H5Oclose(g);
					return CBF_H5DIFFERENT;
				}
			}
		}
	}


	/** \brief Look for a dataset with the given properties.

	<p>Returns <code>CBF_NOTFOUND</code> without modifying <code>dataset</code> if no dataset exists and fails without
	modifying <code>dataset</code> if one with different properties exists. A dataset will be 'found' if it has the
	same name and a maximum size which is at least as big as the size requested in <code>max</code>.</p>

	<p>A buffer of <code>rank</code> elements pointed to by <code>buf</code> may be used to store the array of
	maximum extents for a potentially matching dataset, in order to avoid the use of <code>malloc</code> &
	<code>free</code> for very small amounts of memory.</p>

	<p>Use as:</p>
	<!-- remember to replace tabs with 4 spaces! -->
<pre><code><span class="comment">/&#42; Get the return code from the function call, &#42;/</span>
const int found = cbf_H5Dfind(location, &dataset, ...);
<span class="comment">/&#42; and check what it was: &#42;/</span>
if (CBF_SUCCESS==found) {
	<span class="comment">/&#42; A dataset already existed and I have a handle for it: &#42;/</span>
     use_existing_dataset(dataset);
} else if (CBF_NOTFOUND==found) {
	<span class="comment">/&#42; No matching dataset existed, so I can create one: &#42;/</span>
	cbf_H5Dcreate(location, &dataset, ...);
     use_new_datset(dataset);
     } else {
	<span class="comment">/&#42;
	The function call failed, do something with the error.
	In this case, store it for later use and print a message.
	&#42;/</span>
	error |= found;
	fprintf(stderr,"There was an error - %s.\n",cbf_strerror(error));
     }
<span class="comment">/&#42; clean up: &#42;/</span>
cbf_H5Dfree(dataset);</code></pre>

     \param location The hdf5 group/file in which to put the dataset.
     \param dataset A pointer to a HDF5 object identifier that is set to the location of a valid object or an invalid value if the function
     succeeds, otherwise is left in an undefined state.
     \param name The name of the existing/new dataset.
	\param rank The rank of the data, must be equal to the length of the <code>max</code> and <code>buf</code> arrays, if they are given.
	\param max The (optional) maximum size of each dimension, pointer or an array of length <code>rank</code> where
		<code>0 &lt;= max[i] &lt;= H5S_UNLIMITED</code> for <code>i = [0, rank)</code>, unused if <code>rank == 0</code>.
	\param buf An optional buffer with <code>rank</code> elements which may be used to store the
		current maximum dimensions of a potential match to avoid a malloc/free call.
	\param type The type of each data element in the file.

	\return
	<p><code>CBF_SUCCESS</code> if a matching dataset was found, <code>CBF_NOTFOUND</code> if nothing with the same
	name was found, some other error code otherwise.</p>
     */
	int cbf_H5Dfind2
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const int rank,
     const hsize_t * const max,
     hsize_t * const buf,
     const hid_t type)
	{
		htri_t l = CBF_H5FAIL;
		/* check the arguments */
		if (!cbf_H5Ivalid(location) || !dataset || !name || rank<0 || H5I_DATATYPE!=H5Iget_type(type)) return CBF_ARGUMENT;

		/* check if the link exists */
		l = H5Lexists(location, name, H5P_DEFAULT);
		if (l < 0) {
			if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Could not check if link '%s' exists",__WHERE__,name);
			return CBF_H5ERROR;
		} else if (!l) {
			return CBF_NOTFOUND;
		} else {
			/* check if the linked object exists */
			const htri_t e = H5Oexists_by_name(location, name, H5P_DEFAULT);
			if (e < 0) {
				if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Could not check if object '%s' exists\n",__WHERE__,name);
				return CBF_H5ERROR;
			} else if (!e) {
				/* The link exists but the object doesn't - try to remove the link & tell the caller that there is no dataset */
				if (H5Ldelete(location, name, H5P_DEFAULT) < 0) {
					if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Could not remove dead link '%s'\n",__WHERE__,name);
					return CBF_H5ERROR;
				} else {
					return CBF_NOTFOUND;
				}
			} else {
				/* my object exists - check its type */
				hid_t g = H5Oopen(location, name, H5P_DEFAULT);
				if (H5I_DATASET == H5Iget_type(g)) {
					int error = CBF_SUCCESS;
					/* it's a dataset - check its properties */
					const hid_t currSpace = H5Dget_space(g);
					const int currRank = H5Sget_simple_extent_dims(currSpace, 0, 0);
					if (currRank < 0) {
						if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Could not get rank of '%s'\n",__WHERE__,name);
						error |= CBF_H5ERROR;
					}
					if (currRank != rank) error |= CBF_H5DIFFERENT;
					if (CBF_SUCCESS==error && 0!=max && 0<rank) {
						/* Check dataspace if it makes sense to do so */
						hsize_t * const _buf = buf ? 0 : malloc(rank*sizeof(hsize_t));
						hsize_t * const currMax = buf ? buf : _buf;
						if (H5Sget_simple_extent_dims(currSpace, 0, currMax)<0) {
							if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Could not get extent of '%s'\n",__WHERE__,name);
							error |= CBF_H5ERROR;
						}
						int i = 0;
						for (; i != currRank; ++i) {
							if (currMax[i] != max[i] && currMax[i] < max[i]) {
								error |= CBF_H5DIFFERENT;
							}
						}
						free((void*)_buf);
					}
					H5Sclose(currSpace);
					if (CBF_SUCCESS==error) {
				 		/* check the datatype is correct */
						const hid_t currType = H5Dget_type(g);
						const htri_t eq = H5Tequal(currType,type);
						if (eq<0) {
							if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Could not test type of '%s'\n",__WHERE__,name);
							error |= CBF_H5ERROR;
						} else if (!eq) error |= CBF_H5DIFFERENT;
						else /* success */;
					H5Tclose(currType);
					}
					*dataset = CBF_SUCCESS==error ? g : CBF_H5FAIL;
					return error;
				} else {
					/* not a dataset - close the object & fail */
					H5Oclose(g);
					return CBF_H5DIFFERENT;
				}
			}
		}
	}

	/** \brief Ensure that a dataset exists, returning a handle to an existing dataset or creating a new dataset if needed.
	<p>Ensure a dataset of the given <code>rank</code> exists and can at least as many elements as specified in
	<code>max</code>. If no dataset exists then one will be created with dimensions of [0, 0, ... 0].
	<code>cbf_H5Dfind</code> and <code>cbf_H5Dcreate</code> are used in the implementation of this function.</p>

	\param location The hdf5 group/file in which to put the dataset.
	\param dataset A pointer to a HDF5 object identifier that is set to the location of a valid object or an invalid value if the function
	succeeds, otherwise is left in an undefined state.
	\param name The name of the existing/new dataset.
	\param rank The rank of the data, must be equal to the length of the <code>max</code> and <code>buf</code> arrays, if they are given.
	\param max The (optional) maximum size of each dimension, pointer or an array of length <code>rank</code> where
	<code>0 &lt;= max[i] &lt;= H5S_UNLIMITED</code> for <code>i = [0, rank)</code>, unused if <code>rank == 0</code>.
	\param chunk The chunk size for the dataset, as a pointer to an array of length <code>rank</code>
	(or <code>0</code> if chunking should not be enabled).
	\param buf An optional buffer with <code>rank</code> elements which may be used to store the current maximum dimensions of a
	potential match and/or the dimensions of the dataset to be created, to avoid using the heap for small amounts of memory.
	\param type The type of each data element in the file.

	\return An error code.
	*/
	int cbf_H5Drequire
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const int rank,
     const hsize_t * const max,
     const hsize_t * const chunk,
     hsize_t * const buf,
     const hid_t type)
	{
		int error = CBF_SUCCESS;
		int found = CBF_SUCCESS;
		hid_t dset = CBF_H5FAIL;
		hid_t * dsetp = dataset ? dataset : &dset;
		if (rank < 0) return CBF_ARGUMENT;
		found = cbf_H5Dfind2(location,dsetp,name,rank,max,buf,type);
		if (CBF_SUCCESS == found) {
			/* cbf_H5Dfind already checked the dimensions & type, so I don't need to do anything here */
		} else if (CBF_NOTFOUND==found) {
			/* create a suitable dataset */
			hsize_t * const _buf = (buf || !rank) ? NULL : malloc(rank*sizeof(hsize_t));
			hsize_t * const dim = buf ? buf : _buf;
			hsize_t * it;
			for (it = dim; it != dim+rank; ++it) *it = 0;
			error |= cbf_H5Dcreate(location,dsetp,name,rank,dim,max,chunk,type);
			free((void*)_buf);
		} else {
			error |= found;
			/* maybe report the failure? */
			if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(error));
		}
		cbf_H5Dfree(dset);
		return error;
	}

	/** \brief Add some data to a datset, expanding the dataset to the appropriate size if needed.
	<p>Insert a slice of data into <code>dataset</code> with the appropriate <code>offset</code> &
	<code>stride</code>, ensuring that no existing data is lost due to resizing the dataset but not checking that
	previous data isn't being overwritten.</p>

	\param dataset The dataset to write the data to.
	\param offset Where to start writing the data, as an array of <code>rank</code> numbers.
	\param stride The number of elements in the dataset to step for each element to be written, where
	null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of <code>rank</code> numbers.
	\param count The number of elements in each dimension to be written, as an array of <code>rank</code> numbers.
	\param buf An optional buffer with <code>rank</code> elements which may be used to store the current dimensions
	of the dataset, to avoid using the heap for small amounts of memory.
	\param value The address of the data to be written.
	\param type The type of data in memory.

	\return An error code.
	*/
	int cbf_H5Dinsert
			(const hid_t dataset,
			 const hsize_t * const offset,
			 const hsize_t * const stride,
			 const hsize_t * const count,
			 hsize_t * const buf,
			 const void * const value,
			 const hid_t type)
	{
		int error = CBF_SUCCESS;

		/* check some arguments */
		if (!cbf_H5Ivalid(dataset)) return CBF_ARGUMENT;
		if (H5I_DATASET!=H5Iget_type(dataset)) return CBF_ARGUMENT;
		if (!offset || !count || !value) return CBF_ARGUMENT;
		if (H5I_DATATYPE!=H5Iget_type(type)) return CBF_ARGUMENT;

		{
			/* get the rank and current dimensions of the dataset */
			const hid_t oldSpace = H5Dget_space(dataset);
			const int rank = H5Sget_simple_extent_dims(oldSpace,0,0);
			hsize_t * const _buf = buf ? 0 : malloc(rank*sizeof(hsize_t));
			hsize_t * dim = buf ? buf : _buf;
			if (H5Sget_simple_extent_dims(oldSpace,dim,0) != rank) error |= CBF_H5ERROR;
			if (rank >= 0) {
				hid_t memSpace = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank,count,0);
				hid_t newSpace = CBF_H5FAIL;
				/* extend the dimensions, if required */
				unsigned int i;
				for (i = 0; i != rank; ++i) {
					const hsize_t sz = offset[i] + (stride?stride[i]:1)*(count[i]-1)+1;
					dim[i] = (dim[i]>sz) ? dim[i] : sz;
				}
				CBF_H5CALL(H5Dset_extent(dataset,dim));
				newSpace = H5Dget_space(dataset);
				/* select elements & write the dataset */
				if (rank) {
					CBF_H5CALL(H5Sselect_hyperslab(newSpace, H5S_SELECT_SET, offset, stride, count, 0));
				} else {
					CBF_H5CALL(H5Sselect_all(newSpace));
				}
				CBF_H5CALL(H5Dwrite(dataset,type,memSpace,newSpace,H5P_DEFAULT,value));
				/* check local variables are properly closed */
				if (cbf_H5Ivalid(memSpace)) H5Sclose(memSpace);
				if (cbf_H5Ivalid(newSpace)) H5Sclose(newSpace);
			}
			if (cbf_H5Ivalid(oldSpace)) H5Sclose(oldSpace);
			free((void*)_buf);
		}

		return error;
	}

	/** \brief Change the extent of a chunked dataset to the values in \c dim.

	<p>Forwards to a HDF5 function to change the extent of <code>dataset</code>. This can't check that the number of
	elements in <code>dim</code> matches the rank of the dataset.</p>

     \param dataset A handle for the dataset whose extent is to be changed.
     \param dim The new extent of the dataset, if the function succeeds. Must be the same length as the rank of the dataset.

     \return An error code.
     */
	int cbf_H5Dset_extent(const hid_t dataset, const hsize_t * const dim)
	{
		if (!dim) return CBF_ARGUMENT;
		if (!cbf_H5Ivalid(dataset)) return CBF_ARGUMENT;
		if (H5I_DATASET!=H5Iget_type(dataset)) return CBF_ARGUMENT;
		if (H5Dset_extent(dataset,dim) < 0) return CBF_H5ERROR;
		return CBF_SUCCESS;
	}

    
	/** \brief Add some data to the specified position in the dataset, without checking what (if anything) was there before.
     
     Assumes the dataset has the appropriate size to contain all the data and overwrites any existing data that may be there.
     The \c rank of the dataset is assumed to be known, and the size of the array parameters is not tested.
     
     \param dataset The dataset to write the data to.
     \param offset Where to start writing the data, as an array of \c rank numbers.
     \param stride The number of elements in the dataset to step for each element to be written, where
     null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of \c rank numbers.
     \param count The number of elements in each dimension to be written, as an array of \c rank numbers.
     \param value The address of the data to be written.
     
     \sa cbf_H5Dcreate
     \sa  cbf_H5Dfind2   
     \sa cbf_H5Dset_extent
     \sa cbf_H5Dread
     \sa cbf_H5Drequire_scalar_F64LE
     \sa cbf_H5Drequire_string
     \sa cbf_H5Dfree
     \sa cbf_H5Ddestroy
     
     \return An error code.
     */
	int cbf_H5Dwrite
    (const hid_t dataset,
     const hsize_t * const offset,
     const hsize_t * const stride,
     const hsize_t * const count,
     const void * const value)
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(dataset)) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t datatype = H5Dget_type(dataset);
		hid_t filespace = H5Dget_space(dataset);
		const int rank = H5Sget_simple_extent_ndims(filespace);
		hid_t memspace = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank,count,0);
        hid_t memtype = H5Tget_native_type(datatype, H5T_DIR_ASCEND);
		if ((!!rank && (!offset || !count)) || rank<0) error |= CBF_ARGUMENT;
        
		/* check variables are valid */
		reportFail(cbf_H5Ivalid(filespace), CBF_H5ERROR, error);
		reportFail(cbf_H5Ivalid(datatype), CBF_H5ERROR, error);
        
		/* select elements & write the dataset */
		if (!!rank) {
			reportFail(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, 0)>=0, CBF_H5ERROR, error);
		} else {
			reportFail(H5Sselect_all(filespace)>=0, CBF_H5ERROR, error);
		}
		reportFail(H5Dwrite(dataset,memtype,memspace,filespace,H5P_DEFAULT,value)>=0, CBF_H5ERROR, error);
        
		/* check local variables are properly closed */
		if (cbf_H5Ivalid(memtype)) H5Tclose(memtype);
		if (cbf_H5Ivalid(memspace)) H5Sclose(memspace);
		if (cbf_H5Ivalid(filespace)) H5Sclose(filespace);
		if (cbf_H5Ivalid(datatype)) H5Tclose(datatype);
        
		/* done */
		return error;
	}

	
    /** \brief Add some data to the specified position in the dataset, without checking what (if anything) was there before.

	<p>Assumes the dataset has the appropriate size to contain all the data and overwrites any existing data that may
	be there. The <code>rank</code> of the dataset is assumed to be known, and the size of the array parameters is not
	tested. When <code>rank</code> is zero - in the case of scalar datasets - the <code>offset</code>,
	<code>stride</code> and <code>count</code> parameters are meaningless and should be omitted by setting them to
	zero.</p>

     \param dataset The dataset to write the data to.
	\param offset Where to start writing the data, as an array of <code>rank</code> numbers.
     \param stride The number of elements in the dataset to step for each element to be written, where
	null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of <code>rank</code> numbers.
	\param count The number of elements in each dimension to be written, as an array of <code>rank</code> numbers.
     \param value The address of the data to be written.
	\param type The type of data in memory.

     \return An error code.
     */
	int cbf_H5Dwrite2
    (const hid_t dataset,
     const hsize_t * const offset,
     const hsize_t * const stride,
     const hsize_t * const count,
     const void * const value,
	 const hid_t type)
	{
		/* define variables */
		int error = CBF_SUCCESS;
		hid_t filespace = CBF_H5FAIL;

		/* check types and some arguments */
		if (H5I_DATASET!=H5Iget_type(dataset) || !cbf_H5Ivalid(dataset)) return CBF_ARGUMENT;
		if (!value) return CBF_ARGUMENT;
		if (H5I_DATATYPE!=H5Iget_type(type)) return CBF_ARGUMENT;

		/* extract the dataspace from the dataset */
		filespace = H5Dget_space(dataset);

		if (cbf_H5Ivalid(filespace)) {
			/* get some data from the dataspace */
		const int rank = H5Sget_simple_extent_ndims(filespace);
			hid_t memspace = CBF_H5FAIL;

			/* check more arguments are valid */
			if ((rank && (!offset || !count)) || rank<0) error |= CBF_ARGUMENT;

			if (CBF_SUCCESS == error) {
				/* create memspace */
				if (rank) memspace = H5Screate_simple(rank,count,0);
				else memspace = H5Screate(H5S_SCALAR);
				/* check it worked */
				if (!cbf_H5Ivalid(memspace)) {
					error |= CBF_H5ERROR;
					if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(error));
				}
		}

			if (CBF_SUCCESS == error) {
				/* select elements */
				if (rank) CBF_H5CALL(H5Sselect_hyperslab(filespace,H5S_SELECT_SET,offset,stride,count,0));
				else CBF_H5CALL(H5Sselect_all(filespace));
			}

			/* write the dataset */
			if (CBF_SUCCESS == error) CBF_H5CALL(H5Dwrite(dataset,type,memspace,filespace,H5P_DEFAULT,value));

		/* check local variables are properly closed */
		if (cbf_H5Ivalid(memspace)) H5Sclose(memspace);
		} else {
			error |= CBF_H5ERROR;
			if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(error));
		}

		if (cbf_H5Ivalid(filespace)) H5Sclose(filespace);
		return error;
	}

        
        
        /** \brief Extract some existing data from a dataset at a known position.
         
         Read some data from a given location in the dataset to an existing location in memory.
         Does not check the length of the array parameters, which should all have \c rank elements or (in some cases) be null.
         
         \param dataset The dataset to read the data from.
         \param offset Where to start reading the data, as an array of \c rank numbers.
         \param stride The number of elements in the dataset to step for each element to be read, where
         null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of \c rank numbers.
         \param count The number of elements in each dimension to be read, as an array of \c rank numbers.
         \param value The location where the data is to be stored.
         
         \sa cbf_H5Dcreate
         \sa  cbf_H5Dfind2   
         \sa cbf_H5Dset_extent
         \sa cbf_H5Dwrite
         \sa cbf_H5Drequire_scalar_F64LE
         \sa cbf_H5Drequire_string
         \sa cbf_H5Dfree
         \sa cbf_H5Ddestroy
         
         \return An error code.
         */
        int cbf_H5Dread
        (const hid_t dataset,
         const hsize_t * const offset,
         const hsize_t * const stride,
         const hsize_t * const count,
         void * const value)
        {
            /* define variables & check args */
            int error = (!cbf_H5Ivalid(dataset)) ? CBF_ARGUMENT : CBF_SUCCESS;
            hid_t datatype = H5Dget_type(dataset);
            hid_t filespace = H5Dget_space(dataset);
            const int rank = H5Sget_simple_extent_ndims(filespace);
            hid_t memspace = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank,count,0);
            if ((!!rank && (!offset || !count)) || rank<0) error |= CBF_ARGUMENT;
            
            /* check variables are valid */
            reportFail(cbf_H5Ivalid(filespace), CBF_H5ERROR, error);
            reportFail(cbf_H5Ivalid(datatype), CBF_H5ERROR, error);
            
            /* select elements & read the dataset */
            if (!!rank) {
                reportFail(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, 0)>=0, CBF_H5ERROR, error);
            } else {
                reportFail(H5Sselect_all(filespace)>=0, CBF_H5ERROR, error);
            }
            reportFail(H5Dread(dataset, datatype, memspace, filespace, H5P_DEFAULT, value)>=0, CBF_H5ERROR, error);
            
            /* check local variables are properly closed */
            if (cbf_H5Ivalid(memspace)) H5Sclose(memspace);
            if (cbf_H5Ivalid(filespace)) H5Sclose(filespace);
            if (cbf_H5Ivalid(datatype)) H5Tclose(datatype);
            
            /* done */
            return error;
        }
        
	/** \brief Extract some existing data from a dataset at a known position with memtype.

	<p>Read some data from a given location in the dataset to an existing location in memory. Does not check the
	length of the array parameters, which should all have <code>rank</code> elements or (in some cases) be
	<code>null</code>. When <code>rank</code> is zero - in the case of scalar datasets - the <code>offset</code>,
	<code>stride</code> and <code>count</code> parameters are meaningless and should be omitted by setting them to
	zero.</p>

     \param dataset The dataset to read the data from.
	\param offset Where to start writing the data, as an array of <code>rank</code> numbers.
	\param stride The number of elements in the dataset to step for each element to be written, where
	null is equivalent to a stride of [1, 1, 1, ..., 1], as an array of <code>rank</code> numbers.
	\param count The number of elements in each dimension to be written, as an array of <code>rank</code> numbers.
     \param value The location where the data is to be stored.
	\param type The type of data in memory.

     \return An error code.
     */
	int cbf_H5Dread2
    (const hid_t dataset,
     const hsize_t * const offset,
     const hsize_t * const stride,
     const hsize_t * const count,
	 void * const value,
	 const hid_t type)
	{
		/* define variables & check args */
		int error = (!cbf_H5Ivalid(dataset)) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t filespace = H5Dget_space(dataset);
		const int rank = H5Sget_simple_extent_ndims(filespace);
		hid_t memspace = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank,count,0);
		if ((!!rank && (!offset || !count)) || rank<0) error |= CBF_ARGUMENT;

		/* check variables are valid */
		reportFail(cbf_H5Ivalid(filespace), CBF_H5ERROR, error);
		reportFail(cbf_H5Ivalid(memspace), CBF_H5ERROR, error);

		/* select elements & read the dataset */
		if (rank) {
			reportFail(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, stride, count, 0)>=0, CBF_H5ERROR, error);
		} else {
			reportFail(H5Sselect_all(filespace)>=0, CBF_H5ERROR, error);
		}
		reportFail(H5Dread(dataset, type, memspace, filespace, H5P_DEFAULT, value)>=0, CBF_H5ERROR, error);

		/* check local variables are properly closed */
		if (cbf_H5Ivalid(memspace)) H5Sclose(memspace);
		if (cbf_H5Ivalid(filespace)) H5Sclose(filespace);

		/* done */
		return error;
	}

        
        /** \brief Write a scalar 64-bit floating point number as a dataset.
         
         Convenience function using the HDF5 abstraction layer to avoid the need to consider array-related
         parameters for a scalar dataset and to automatically set the string type to the correct size.
         
         \param location The group containing the new dataset.
         \param dataset An optional pointer to a place to store the new dataset.
         \param name The name of the new dataset.
         \param value The value of the new dataset.
         \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
         
         \sa cbf_H5Dcreate
         \sa cbf_H5Dfind
         \sa cbf_H5Dset_extent
         \sa cbf_H5Dwrite
         \sa cbf_H5Dread
         \sa cbf_H5Drequire_string
         \sa cbf_H5Dfree
         \sa cbf_H5Ddestroy
         
         \return An error code.
         */
        int cbf_H5Drequire_scalar_F64LE
        (const hid_t location,
         hid_t * const dataset,
         const char * const name,
         const double value)
        {
            int error = CBF_SUCCESS;
            hid_t _dataset = CBF_H5FAIL;
            error |=  cbf_H5Dfind(location,&_dataset,name,0,0,0,0,H5T_IEEE_F64LE);
            if (CBF_SUCCESS==error) {
                if (!cbf_H5Ivalid(_dataset)) {
                    error |= cbf_H5Dcreate(location,&_dataset,name,0,0,0,0,H5T_IEEE_F64LE);
                    error |= cbf_H5Dwrite(_dataset,0,0,0,&value);
                } else {
                    double data = 0./0.;
                    error |= cbf_H5Dread(_dataset,0,0,0,&data);
                    if (fabs(value - data)> 1.e-38+1.e-13*(fabs(value)+fabs(data))) {
                        fprintf(stderr,"Error: data doesn't match (%g vs %g) for nexus field '%s'\n",data,value,name);
                        error |= CBF_H5DIFFERENT;
                    }
                }
                /* cleanup temporary dataset? */
                if (dataset) *dataset = _dataset;
                else cbf_H5Dfree(_dataset);
            } else {
                fprintf(stderr,"Attempt to determine existence of nexus field '%s' failed\n",name);
            }
            return error;
        }
        

	/** \brief Write a scalar 64-bit floating point number as a dataset with comparison.

	<p>Convenience function using the HDF5 abstraction layer to avoid the need to consider array-related parameters
	for a scalar dataset. Uses <code> cbf_H5Dfind2  </code>, <code>cbf_H5Dcreate</code>, <code>cbf_H5Dread</code> &
	<code>cbf_H5Dwrite</code> to ensure a scalar 64-bit IEEE floating point dataset exists with the appropriate name
	and (for an existing dataset) the correct value as determined by the comparison function <code>cmp</code>.</p>

     \param location The group containing the new dataset.
     \param dataset An optional pointer to a place to store the new dataset.
     \param name The name of the new dataset.
     \param value The value of the new dataset.
     \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
	\param cmp_params Some extra data required by the comparison function.

     \return An error code.
     */
	int cbf_H5Drequire_scalar_F64LE2
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
		const double value,
     int (*cmp)(const void *, const void *, size_t)
    )
	{
		int error = CBF_SUCCESS;
		int found = CBF_SUCCESS;
		hid_t _dataset = CBF_H5FAIL;
		hid_t * dset = dataset ? dataset : &_dataset;
		found =  cbf_H5Dfind2(location,dset,name,0,0,0,H5T_IEEE_F64LE);
		if (CBF_SUCCESS==found) {
				double data = 0./0.;
			error |= cbf_H5Dread2(*dset,0,0,0,&data,H5T_NATIVE_DOUBLE);
			if (cmp(&value, &data, 1)) {
				if (CBF_HDF5_DEBUG)
					fprintf(stderr,"%s: data doesn't match (%g vs %g) for nexus field '%s'\n",__WHERE__,data,value,name);
					error |= CBF_H5DIFFERENT;
				}
		} else if (CBF_NOTFOUND==found) {
			error |= cbf_H5Dcreate(location,dset,name,0,0,0,0,H5T_IEEE_F64LE);
			error |= cbf_H5Dwrite2(*dset,0,0,0,&value,H5T_NATIVE_DOUBLE);
		} else {
			error |= found;
			if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Attempt to determine existence of nexus field '%s' failed\n",__WHERE__,name);
		}
		/* cleanup temporary dataset */
		cbf_H5Dfree(_dataset);
		return error;
	}

        /** \brief Write a scalar 64-bit floating point number as a dataset with ULP comparison.
         
         <p>Convenience function using the HDF5 abstraction layer to avoid the need to consider array-related parameters
         for a scalar dataset. Uses <code> cbf_H5Dfind2 </code>, <code>cbf_H5Dcreate</code>, <code>cbf_H5Dread</code> &
         <code>cbf_H5Dwrite</code> to ensure a scalar 64-bit IEEE floating point dataset exists with the appropriate name
         and (for an existing dataset) the correct value as determined by the comparison function <code>cmp</code>.</p>
         
         \param location The group containing the new dataset.
         \param dataset An optional pointer to a place to store the new dataset.
         \param name The name of the new dataset.
         \param value The value of the new dataset.
         \param cmp A comparison function to test if a previously set value is equal to the value I asked for.
         \param cmp_params Some extra data required by the comparison function.
         
         \return An error code.
         */
        int cbf_H5Drequire_scalar_F64LE2_ULP
        (const hid_t location,
         hid_t * const dataset,
         const char * const name,
         const double value,
         int (*cmp)(const void *, const void *, size_t, const void *),
         const void * const cmp_params)
        {
            int error = CBF_SUCCESS;
            int found = CBF_SUCCESS;
            hid_t _dataset = CBF_H5FAIL;
            hid_t * dset = dataset ? dataset : &_dataset;
            found =  cbf_H5Dfind2(location,dset,name,0,0,0,H5T_IEEE_F64LE);
            if (CBF_SUCCESS==found) {
				double data = 0./0.;
                error |= cbf_H5Dread2(*dset,0,0,0,&data,H5T_NATIVE_DOUBLE);
                if (cmp(&value, &data, 1, cmp_params)) {
                    if (CBF_HDF5_DEBUG)
                        fprintf(stderr,"%s: data doesn't match (%g vs %g) for nexus field '%s'\n",__WHERE__,data,value,name);
					error |= CBF_H5DIFFERENT;
				}
            } else if (CBF_NOTFOUND==found) {
                error |= cbf_H5Dcreate(location,dset,name,0,0,0,0,H5T_IEEE_F64LE);
                error |= cbf_H5Dwrite2(*dset,0,0,0,&value,H5T_NATIVE_DOUBLE);
            } else {
                error |= found;
                if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Attempt to determine existence of nexus field '%s' failed\n",__WHERE__,name);
            }
            /* cleanup temporary dataset */
            cbf_H5Dfree(_dataset);
            return error;
        }

        
	/** \brief Write a single fixed-length string as a dataset.

	<p>Convenience function using the HDF5 abstraction layer to avoid the need to consider array-related parameters
	for a scalar dataset and to automatically set the string type to the correct size.</p>

     \param location The group containing the new dataset.
     \param dataset An optional pointer to a place to store the new dataset.
     \param name The name of the new dataset.
     \param value The value of the new dataset.
	\param vlen Flag to determine if the string should be stored as a variable length type

     \return An error code.
     */

	int cbf_H5Drequire_flstring
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const char * const value)
	{
		int error = CBF_SUCCESS;
		int found = CBF_SUCCESS;
		if (!value) return CBF_ARGUMENT;
		hid_t _dataset = CBF_H5FAIL;
		hid_t * dset = dataset ? dataset : &_dataset;
		hid_t dataType = H5Tcopy(H5T_C_S1);
		CBF_H5CALL(H5Tset_size(dataType,strlen(value)+1));
		found =  cbf_H5Dfind2(location,dset,name,0,0,0,dataType);
		if (CBF_SUCCESS==found) {
			hid_t currType = H5Dget_type(*dset);
			char * data = malloc(H5Tget_size(currType));
				H5Tclose(currType);
			error |= cbf_H5Dread2(*dset,0,0,0,(void * const)(data),dataType);
			if (strcmp(value, data)) {
				if (CBF_HDF5_DEBUG)
					fprintf(stderr,"%s: data doesn't match ('%s' vs '%s') for nexus field '%s'\n",__WHERE__,data,value,name);
					error |= CBF_H5DIFFERENT;
				}
			/* 'data' is either allocated by me or by the HDF5 library: always free it */
				free((void*)data);
		} else if (CBF_NOTFOUND==found) {
			error |= cbf_H5Dcreate(location,dset,name,0,0,0,0,dataType);
			error |= cbf_H5Dwrite2(*dset,0,0,0,(const void * const)(value),dataType);
		} else {
			error |= found;
			if (CBF_HDF5_DEBUG) fprintf(stderr,"%s: Attempt to determine existence of nexus field '%s' failed\n",__WHERE__,name);
		}
		/* cleanup temporary dataset */
		cbf_H5Dfree(_dataset);
		H5Tclose(dataType);
		return error;
	}

	/** \brief Close a HDF5 dataset

     Attempt to close a dataset, but don't modify the identifier that described it.

     \param ID The HDF5 dataset to be closed.

     \return An error code.
     */
	int cbf_H5Dfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Dclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		else return CBF_ARGUMENT;
	}

	/* Custom HDF5 types - to get the correct string type for datasets in a consistent way */

	/** \brief Get a HDF5 string datatype with a specified length.

	<p>Convenience function to create a string datatype suitable for use when storing a string of length
	<code>len</code>, returning it in the identifier pointed to by <code>type</code>.</p>

     \param type A pointer to a the HDF5 handle of the new datatype, which should be free'd with \c cbf_H5Tfree
     \param len The length of the string datatype - should be \c strlen() or \c H5T_VARIABLE

     \return An error code.
     */
	int cbf_H5Tcreate_string(hid_t * type, const size_t len)
	{
		*type = H5Tcopy(H5T_C_S1);
		return H5Tset_size(*type,H5T_VARIABLE==len?len:len+1) < 0 ? CBF_H5ERROR : CBF_SUCCESS;
	}

	/** \brief Close a HDF5 datatype identifier

     Attempt to close a datatype identifier, but don't modify the identifier that described it.

     \param ID The HDF5 datatype to be closed.

     \return An error code.
     */
	int cbf_H5Tfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Tclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		else return CBF_ARGUMENT;
	}

	/* HDF5 dataspace functions: I need a uniform method of creating data spaces to ensure correct operation of comparison functions */

	/** \brief Create a dataspace with some given values

     Helper function which creates a HDF5 dataspace.

	<p>Maximum dimensions can be set to infinity by passing <code>H5S_UNLIMITED</code> in the appropriate slot of the
	<code>max</code> parameter. If <code>rank</code> is zero then neither <code>dim</code> nor <code>max</code> are
	used and a scalar dataspace is created. Otherwise, if <code>max</code> is a null pointer the maximum length is set
	to the current length as given by <code>dim</code>, if <code>dim</code> is a null pointer then <code>ID</code>
	will not be modified and the function will fail.</p>

     \param ID A pointer to a HDF5 identifier that will contain the new dataspace.
     \param rank The number of dimensions of the new dataspace.
     \param dim The current size of each dimension of the dataspace, should be an array of length \c rank .
     \param max The maximum size of each dimension, should be an array of length \c rank .

     \return An error code.
     */
	int cbf_H5Screate
    (hid_t * const ID,
     const int rank,
     const hsize_t * const dim,
     const hsize_t * const max)
	{
		if (!ID || (rank && !dim) || rank<0) return CBF_ARGUMENT;
		else {
			hid_t space = !rank ? H5Screate(H5S_SCALAR) : H5Screate_simple(rank, dim, max);
			if (cbf_H5Ivalid(space)) {
				*ID = space;
				return CBF_SUCCESS;
			} else return CBF_H5ERROR;
		}
	}

	/** \brief Close a HDF5 dataspace identifier

     Attempt to close a dataspace identifier, but don't modify the identifier that described it.

     \param ID The HDF5 dataspace to be closed.

     \return An error code.
     */
	int cbf_H5Sfree(const hid_t ID)
	{
		if (cbf_H5Ivalid(ID)) return H5Sclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
		else return CBF_ARGUMENT;
	}




    /****************************************************************
     End of section of code extracted from J. Sloan's
     cbf_hdf5_common.c
     ****************************************************************/

    /****************************************************************
     The following section of code is extracted from J. Sloan's
     config.c
     ****************************************************************/


    /* Some error codes for use by the parsing functions - definitions should not be visible */
	const int cbf_configError_success = 0;
	const int cbf_configError_unexpectedInput = 1;
	const int cbf_configError_expectedDelimeter = 2;
	const int cbf_configError_expectedNumber = 3;
	const int cbf_configError_openingFile = 4;
	const int cbf_configError_expectedString = 5;
	const int cbf_configError_duplicateField = 6;
	const int cbf_configError_unexpectedEOF = 7;
	const int cbf_configError_undefinedValue = 8;
	const int cbf_configError_invalidDependency = 9;
	const int cbf_configError_missingDependency = 10;
	const int cbf_configError_loop = 11;

    /*
     Tokenise an input stream, returning one token at a time into the given buffer.

     \param buf A pointer to a realloc-able buffer for storing the input, free'd when EOF is reached.
     \param n The current size of \c buf.
     \param ln The current line number of the file.
     \param pre The previous character, needed to test for unexpected EOL state.
     \param stream The stream to be tokenised.

	\return A parser error code.
     */
	static int cbf_configParse_scan(char * * const buf, size_t * n, size_t * ln, char * const pre, FILE * stream)
    {
        int c = 0; /* current character */
        size_t k = 0; /* current line length */

        /* check that sensible arguments are given */
        assert(buf);
        assert(n);
        assert(ln);
        assert(pre);
        assert(stream);

        /* skip blanks & comments to get to an interesting character */
        do {
            c = fgetc(stream);
            if (feof(stream)) break;
            *pre = c;
            if ('\n' == c) ++*ln;
            if ('#' == c) {
                do {
                    c = fgetc(stream);
                    if (feof(stream)) break;
                    *pre = c;
                    if ('\n' == c) ++*ln;
                } while (!isspace(c) || isblank(c));
            }
            if (isblank(c)) continue;
            else break;
        } while (1);

        /* if I am at the end of the stream, free the buffer & return 0 */
        if (feof(stream)) {
            free((void*)(*buf));
            *buf = 0;
            *n = 0;
			return '\n'==*pre ? cbf_configError_success : cbf_configError_unexpectedEOF;
        }

        /* I have a token: read it */
        if (isspace(c)) {
            /* it's a newline token */
            cbf_push_buf('\n', buf, n, &k);
        } else {
            /* it's a string : leave the terminating character in the stream as it may be a token itself */
            if ('[' == c || ']' == c) {
                /* it's a vector start or end token : match the structure of a vector at a higher level */
                cbf_push_buf(c, buf, n, &k);
            } else while (1) {
                cbf_push_buf(c, buf, n, &k);
                c = fgetc(stream);
                if (feof(stream)) break;
                *pre = c;
                if (isspace(c) || '[' == c || ']' == c) {
                    ungetc(c, stream);
                    break;
                }
            }
        }
        /* null-terminate the token */
        cbf_push_buf('\0', buf, n, &k);

		return (feof(stream) && '\n'!=*pre) ? cbf_configError_unexpectedEOF : cbf_configError_success;
    }

    /**
	<p>The returned string is "none" for success, "unknown error" if the given error code is
	not recognised and a non-empty string briefly describing the error otherwise.</p>
	<p>The returned string must not be free'd.</p>
     */
    const char * cbf_config_strerror(const int error)
    {
		if (error == cbf_configError_success) return "none";
		else if (error == cbf_configError_unexpectedInput) return "unexpected input";
		else if (error == cbf_configError_expectedDelimeter) return "expected a delimiter";
		else if (error == cbf_configError_expectedNumber) return "expected a number";
		else if (error == cbf_configError_openingFile) return "could not open file";
		else if (error == cbf_configError_expectedString) return "expected a string";
		else if (error == cbf_configError_duplicateField) return "duplicate data";
		else if (error == cbf_configError_undefinedValue) return "a value was not defined at point of use";
		else if (error == cbf_configError_invalidDependency) return "invalid dependency found";
		else if (error == cbf_configError_missingDependency) return "missing dependency";
		else if (error == cbf_configError_loop) return "dependency loop detected";
        else return "unknown error";
    }

	/* POD to define a basic set of configuration settings for an axis */
	typedef struct cbf_configItem_t
	{
		double vector[3];
		const char * minicbf;
		const char * nexus;
		const char * depends_on;
		struct cbf_configItem_t * next;
		int convert;
	} cbf_configItem_t;

    /*
	Initialises name & depends_on to null, vector to [0,0,0].
     */
	static cbf_configItem_t cbf_configItem_create()
    {
        cbf_configItem_t item;
        item.vector[0] = 0.;
        item.vector[1] = 0.;
        item.vector[2] = 0.;
		item.minicbf = NULL;
		item.nexus = NULL;
		item.depends_on = NULL;
		item.next = NULL;
		item.convert = 0;
        return item;
    }

	static void cbf_configItem_free(const cbf_configItem_t * item)
    {
        free((void*)(item->minicbf));
        free((void*)(item->nexus));
        free((void*)(item->depends_on));
    }

    /*
    Should not be manipulated directly, takes ownership of the config items which it contains.
    */
	struct cbf_config_t
    {
        size_t nItems;
        size_t maxItems;
        const char * sample_depends_on;
		cbf_configItem_t * item;
	};

    /**
	<p>Allocates a new collection of configuration settings on the heap, and initialises it. The returned
	pointer should be destroyed by the caller.</p>
     */
    cbf_config_t * cbf_config_create()
    {
		cbf_config_t * const vector = malloc(sizeof(cbf_config_t));
        vector->nItems = 0;
        vector->maxItems = 0;
        vector->sample_depends_on = NULL;
        vector->item = NULL;
        return vector;
    }

    /**
	<p>Destroys an existing collection of configuration settings. The settings should have been obtained by a call to
	<code>cbf_config_create</code>.</p>
     */
	void cbf_config_free(const cbf_config_t * vector)
    {
        const cbf_configItem_t * it = vector->item;
		for (; it != vector->item+vector->nItems; ++it) cbf_configItem_free(it);
		free((void*)vector->item);
        free((void*)vector->sample_depends_on);
		free((void*)vector);
    }

    /*
     Releases any previously held dependancy and takes ownership of a new one.
     The given string will be free'd by the object when it is no longer needed.
     */
	static void cbf_config_setSampleDependsOn(cbf_config_t * vector, const char * const depends_on)
    {
        free((void*)(vector->sample_depends_on));
        vector->sample_depends_on = depends_on;
    }

    /*
     \return The current dependancy setting for the sample group, or zero if not set.
     */
	static const char * cbf_config_getSampleDependsOn(const cbf_config_t * const vector)
    {
        return vector->sample_depends_on;
    }

    /*
	\return A pointer to an item in the vector that may be modified but should not be free'd,
	subsequent vector operations may invalidate this pointer.
	 */
	static cbf_configItem_t * cbf_config_begin(const cbf_config_t * const vector)
	{
		return vector->item;
	}

    /*
	\return A pointer to an item in the vector that may be modified but should not be free'd,
	subsequent vector operations may invalidate this pointer.
	 */
	static const cbf_configItem_t * cbf_config_end(const cbf_config_t * const vector)
	{
		return vector->item+vector->nItems;
	}

    /*
     The vector will take ownership of the item's contents. This may invalidate any previously obtained pointers to items in the vector.
	\return An iterator to the new item.
     */
	static cbf_configItem_t * cbf_config_push(cbf_config_t * const vector, cbf_configItem_t item)
    {
        if (!(vector->nItems < vector->maxItems)) {
            /* increase the maximum number of items */
            const size_t k = 4;
            vector->maxItems = (size_t)(vector->nItems/k) * k + k;
            vector->item = realloc(vector->item, vector->maxItems*sizeof(cbf_configItem_t));
        }
        /* ensure I have enough items */
        assert(vector->maxItems > vector->nItems);

        /* add the item to the end of the vector & set the item count to the correct number. */
        vector->item[vector->nItems++] = item;
        return vector->item+vector->nItems-1;
    }

    /*
    \return An iterator to a matching entry, or to the current end element if there is no matching entry.
     */
	static cbf_configItem_t * cbf_config_findMinicbf(const cbf_config_t * const vector, const char * const name)
    {
        cbf_configItem_t * it = cbf_config_begin(vector);
        while (cbf_config_end(vector) != it && (!it->minicbf || strcmp(it->minicbf,name))) ++it;
        return it;
    }

    /*
	\return An iterator to a matching entry, or to the current end element if there is no matching entry.
     */
	static cbf_configItem_t * cbf_config_findNexus(const cbf_config_t * const vector, const char * const name)
    {
		cbf_configItem_t * it = cbf_config_begin(vector);
		while (cbf_config_end(vector) != it && (!it->nexus || strcmp(it->nexus,name))) ++it;
        return it;
    }

	/*
	<p>Looks for missing or cyclic dependencies:</p>
	<ul>
		<li>The base coordinate system is expected to be represented by ".", it is an error if any dependency within
		the chain leading to the sample object is not in the set and is not equal to this.</li>
		<li>Cyclic dependencies in the chain leading to the sample is also an error.</li>
		<li>Dependency chains not leading to the sample object are not tested.</li>
	</ul>
	<p>This also sets pointers in the config items to quickly locate their dependencies later on, and marks each axis
	in the chain to allow irrelevant axes to be skipped.</p>

	TODO: test to verify that loops can be caught.
	Need to factor out the loop body and count the number steps on a
	pre-defined loop to verify that it is identified on the correct step.

	\return <p>A parser error code.</p>
     */
	static int _cbf_config_validate(const cbf_config_t * const vector)
    {
		cbf_configItem_t * it1 = cbf_config_findNexus(vector,cbf_config_getSampleDependsOn(vector));
		cbf_configItem_t * it2 = it1;
		do {
			/* mark it2 for conversion & check dependency */
			it2->convert = 1;
			if (!it2->depends_on) return cbf_configError_missingDependency;
			if (!strcmp(it2->depends_on,".")) return cbf_configError_success;
			/* set & check it2->next */
			if (!it2->next) it2->next = cbf_config_findNexus(vector,it2->depends_on);
			if (cbf_config_end(vector)==it2->next) return cbf_configError_invalidDependency;
			/* increment it2 & check for loop */
			it2 = it2->next;
			if (it2==it1) return cbf_configError_loop;
			/* mark it2 for conversion & check dependency */
			it2->convert = 1;
			if (!it2->depends_on) return cbf_configError_missingDependency;
			if (!strcmp(it2->depends_on,".")) return cbf_configError_success;
			/* set & check it2->next */
			if (!it2->next) it2->next = cbf_config_findNexus(vector,it2->depends_on);
			if (cbf_config_end(vector)==it2->next) return cbf_configError_invalidDependency;
			/* increment it2 & check for loop */
			it2 = it2->next;
			if (it2==it1) return cbf_configError_loop;
			/* increment it1 */
			it1 = it1->next;
		} while (1);
		return cbf_configError_success;
    }

	static int cbf_configParse_extractVector
    (FILE * const configFile,
     FILE * const logFile,
     cbf_configItem_t * const it,
     char * * const buf,
     size_t * n,
     size_t * ln,
     char * const pre)
    {
        char * end = 0;

#define GET_TOKEN() \
do { \
  const int e = cbf_configParse_scan(buf, n, ln, pre, configFile); \
    if (cbf_configError_success!=e) { \
	fprintf(logFile,"\nError: %s\n",cbf_config_strerror(e)); \
return e; \
} \
} while (0);

#define REQUIRE_TOKEN(TKN) \
do { \
const char * const _tkn = (TKN); \
if (strcmp(_tkn,*buf)) { \
fprintf(logFile,"Config parsing error on line %lu: expected " #TKN ", got '%s'\n",*ln,*buf); \
    return cbf_configError_unexpectedInput; \
} \
} while (0);

#define REQUIRE_NOT_EOL() \
do{ \
if (!strcmp("\n",*buf)) { \
fprintf(logFile,"Config parsing error on line %lu: unexpected newline\n",*ln); \
    return cbf_configError_unexpectedInput; \
} \
} while (0);

        /* literal '['. */
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        REQUIRE_TOKEN("[");
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        errno = 0;
        it->vector[0] = strtod(*buf, &end);
        if (errno != 0 || end == *buf) {
            fprintf(logFile,"Config parsing error on line %lu: expected a number, got '%s'\n",*ln,*buf);
            return cbf_configError_expectedNumber;
        }
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        errno = 0;
        it->vector[1] = strtod(*buf, &end);
        if (errno != 0 || end == *buf) {
            fprintf(logFile,"Config parsing error on line %lu: expected a number, got '%s'\n",*ln,*buf);
			return cbf_configError_expectedNumber;
        }
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        errno = 0;
        it->vector[2] = strtod(*buf, &end);
        if (errno != 0 || end == *buf) {
            fprintf(logFile,"Config parsing error on line %lu: expected a number, got '%s'\n",*ln,*buf);
			return cbf_configError_expectedNumber;
        }
        /* literal ']'. */
        GET_TOKEN();
        REQUIRE_NOT_EOL();
        REQUIRE_TOKEN("]");

#undef GET_TOKEN
#undef REQUIRE_TOKEN
#undef REQUIRE_NOT_EOL

        return CBF_SUCCESS;
    }

	/*
	The 'strdup' function isn't available when compiling with -ansi on GCC, so provide an alternative.
	*/
	static char * _cbf_strdup(const char *s)
	{
		return strcpy(malloc(sizeof(char)*(1+strlen(s))),s);
	}

	/**
	<p>Parses a configuration file to extract a collection of configuration settings for a miniCBF file, storing them
	in the given configuration settings object. The pointer should have been obtained by a call to
	<code>cbf_config_create</code>. The configuration file format is described in the
	<code>minicbf2nexus</code> documentation.</p>

	\return <p>A parser error code.</p>
	*/
    int cbf_config_parse(FILE * const configFile, FILE * const logFile, cbf_config_t * const vec)
    {
        char * tkn = 0;
        size_t n = 0, ln = 1;
        char pre = '\0';

#define GET_TOKEN() \
do { \
	const int e = cbf_configParse_scan(&tkn, &n, &ln, &pre, configFile); \
	if (cbf_configError_success!=e) { \
		fprintf(logFile,"\nError: %s\n",cbf_config_strerror(e)); \
return e; \
} \
} while (0)

#define REQUIRE_TOKEN(TKN) \
do { \
const char * const _tkn = (TKN); \
if (strcmp(_tkn,tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: expected " #TKN ", got '%s'\n",ln,tkn); \
return cbf_configError_unexpectedInput; \
} \
} while (0)

#define REQUIRE_EOL() \
do{ \
if (strcmp("\n",tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: expected '\\n', got '%s'\n",ln,tkn); \
return cbf_configError_unexpectedInput; \
} \
} while (0)

#define REQUIRE_NOT_EOL() \
do{ \
if (!strcmp("\n",tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: unexpected newline\n",ln); \
return cbf_configError_unexpectedInput; \
} \
} while (0)

#define REQUIRE_NEXUS_AXIS() \
do { \
if (strcmp(".",tkn) && cbf_config_end(vec) == cbf_config_findNexus(vec,tkn)) { \
fprintf(logFile,"Config parsing error on line %lu: Nexus axis '%s' not defined\n",ln,tkn); \
return cbf_configError_undefinedValue; \
} \
} while (0)

#define REQUIRE_VECTOR() \
do { \
const int e = cbf_configParse_extractVector(configFile, logFile, it, &tkn, &n, &ln, &pre); \
if (cbf_configError_success!=e) { \
fprintf(logFile,"Error reading a vector: %s\n",cbf_config_strerror(e)); \
return e; \
} \
} while (0)

        /* first token of the line */
        GET_TOKEN();
        while (tkn) {
            if (!cbf_cistrcmp("map",tkn)) {
                /* storage that I don't need to free within this function */
                cbf_configItem_t * it;
                /* minicbf axis name */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                it = cbf_config_findMinicbf(vec,tkn);
                if (cbf_config_end(vec) != it) {
                    fprintf(logFile,"Config parsing error on line %lu: Duplicate axis definition for minicbf axis '%s'\n",ln,tkn);
                    return cbf_configError_duplicateField;
                }
                it = cbf_config_push(vec,cbf_configItem_create());
				it->minicbf = _cbf_strdup(tkn);
                /* literal 'to'. */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                REQUIRE_TOKEN("to");
                /* nexus axis name */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                if (cbf_config_end(vec) != cbf_config_findNexus(vec,tkn)) {
                    fprintf(logFile,"Config parsing error on line %lu: Duplicate axis definition for Nexus axis '%s'\n",ln,tkn);
                    return cbf_configError_duplicateField;
                }
				it->nexus = _cbf_strdup(tkn);
                /* newline */
                GET_TOKEN();
                REQUIRE_EOL();
            } else if (!cbf_cistrcmp("Sample",tkn)) {
                /* literal 'depends-on'. */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                REQUIRE_TOKEN("depends-on");
                /* nexus axis name */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                REQUIRE_NEXUS_AXIS();
				cbf_config_setSampleDependsOn(vec,_cbf_strdup(tkn));
                /* newline */
                GET_TOKEN();
                REQUIRE_EOL();
			} else if (!cbf_cistrcmp("\n",tkn)) {
            } else {
                /* find entry by nexus axis name */
                cbf_configItem_t * const it = cbf_config_findNexus(vec,tkn);
                if (cbf_config_end(vec) == it) {
                    fprintf(logFile,"Config parsing error on line %lu: Nexus axis '%s' not defined\n",ln,tkn);
                    return cbf_configError_undefinedValue;
                }
                /* match depends-on -> vector OR vector -> depends-on. */
                GET_TOKEN();
                REQUIRE_NOT_EOL();
                if (!cbf_cistrcmp("vector",tkn)) {
                    /* try to get a vector */
                    REQUIRE_VECTOR();
                    GET_TOKEN();
                    /* optional 'depends-on' */
                    if (!cbf_cistrcmp("depends-on",tkn)) {
                        /* nexus axis name */
                        GET_TOKEN();
                        REQUIRE_NOT_EOL();
                        REQUIRE_NEXUS_AXIS();
                        /* this is a potential memory leak */
						it->depends_on = _cbf_strdup(tkn);
                        GET_TOKEN();
                    }
                    /* check for newline */
                    REQUIRE_EOL();
                } else if (!cbf_cistrcmp("depends-on",tkn)) {
                    /* nexus axis name */
                    GET_TOKEN();
                    REQUIRE_NOT_EOL();
                    REQUIRE_NEXUS_AXIS();
                    /* this is a potential memory leak */
					it->depends_on = _cbf_strdup(tkn);
                    GET_TOKEN();
                    /* optional 'vector' */
                    if (!cbf_cistrcmp("vector",tkn)) {
                        /* try to get a vector */
                        REQUIRE_VECTOR();
                        GET_TOKEN();
                    }
                    /* check for newline */
                    REQUIRE_EOL();
                } else return cbf_configError_unexpectedInput;
            }
            GET_TOKEN();
        }
		return _cbf_config_validate(vec);
    }

	/*
	Helper function to take the data associated with a pilatus axis and write the
	axis attributes to a nexus axis.

	\return An error code
	*/
	static int _cbf_pilatusAxis2nexusAxisAttrs
			(hid_t h5data,
			 const char * const units,
			 const char * const depends_on,
			 const cbf_configItem_t * const axisItem,
#ifdef CBF_USE_ULP
			 int (*cmp)(const void *, const void *, size_t, const void * const)
			 ,const void * const cmp_params
#else
			 int (*cmp)(const void *, const void *, size_t)
#endif
             )
	{
		int error = CBF_SUCCESS;
		_CBF_CALL(cbf_H5Arequire_string(h5data,"units",strcmp(units, "deg.")?units:"deg"));
		/* transformation type */
		_CBF_CALL(cbf_H5Arequire_string(h5data,"transformation_type","rotation"));
		/* dependency */
		_CBF_CALL(cbf_H5Arequire_string(h5data,"depends_on",depends_on));
		if (!axisItem->depends_on) {
			fprintf(stderr,"%s: Error: missing dependancy for nexus axis '%s'\n",__WHERE__,axisItem->nexus);
			error |= CBF_UNDEFINED;
		}
		if (CBF_SUCCESS==error) { /* vector */
			const hsize_t vdims[] = {3};
			double buf[3] = {0./0.};
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5data,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
					  axisItem->vector,buf,cmp,cmp_params));
#else
			_CBF_CALL(cbf_H5Arequire_cmp2(h5data,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
					  axisItem->vector,buf,cmp));

#endif
		}
		return error;
	}

    /****************************************************************
     End of section of code extracted from J. Sloan's
     config.c
     ****************************************************************/

	/*
	Simple wrapper function to create a nexus group.
	Forwards to cbf_H5Grequire and cbf_H5Arequire_string, letting them check argument validity.
	*/
	static int _cbf_NXGrequire
			(const hid_t location,
			 hid_t * const dataset,
			 const char * const name,
			 const char * const class)
	{
		int error = CBF_SUCCESS;
		hid_t dset = CBF_H5FAIL;
		cbf_reportnez(cbf_H5Grequire(location,&dset,name),error);
		cbf_reportnez(cbf_H5Arequire_string(dset,"NX_class",class),error);
		if (dataset) *dataset = dset;
		else cbf_H5Dfree(dset);
		return error;
	}


	/** \brief Ensure I have a file in the handle to do stuff with.

	<p>Checks for the presence of a file in the handle with the given <code>name</code>. If no file is present it will
	attempt to open the file identified by <code>name</code>. It will fail if there isn't a file in the handle and no
	file can be opened, or if a file with a different name is already in the handle.</p>

	\param handle The HDF5 handle to use.
	\param name The file name, or <code>NULL</code>.

	\return <p>An error code.</p>
     */
	int cbf_h5handle_require_file
			(const cbf_h5handle handle,
			 const char * name)
	{
		if (!handle) return CBF_ARGUMENT;
		if (!name) {
			/* No name provided, I either have a file in the handle or I don't. */
			if (!cbf_H5Ivalid(handle->hfile)) return CBF_ARGUMENT;
			else return CBF_SUCCESS;
		} else {
			if (!cbf_H5Ivalid(handle->hfile)) {
				/* name but no file: open a file with the given name */
				return cbf_H5Fopen(&(handle->hfile), name);
			} else {
				/* check the names match eventually - could be awkward */
				return CBF_NOTIMPLEMENTED;
			}
		}
	}

	/** \brief Ensure I have a valid NXentry group in the file with a given name.

	<p>Check for an <code>NXentry</code> group in <code>handle</code>, creating a top-level group in the file with the
	given <code>name</code> and NeXus class <code>NXentry</code> if it doesn't exist. Calls
	<code>cbf_h5handle_require_file</code> to ensure that the file exists within the handle, if needed.</p>

	\param handle The HDF5 handle to use.
	\param group An optional pointer to a place where the group should be stored.
	\param name The group name.

	\return <p>An error code.</p>
     */
	int cbf_h5handle_require_entry
			(const cbf_h5handle handle,
			 hid_t * group,
			 const char * name)
	{
		if (!handle) return CBF_ARGUMENT;

		if (!cbf_H5Ivalid(handle->nxid)) {
			int error = CBF_SUCCESS;
			const char defaultName[] = "entry";
			const char * groupName = name ? name : defaultName;
			cbf_reportnez(cbf_h5handle_require_file(handle,0), error);
			cbf_reportnez(_cbf_NXGrequire(handle->hfile,&handle->nxid,groupName,"NXentry"), error);
			if (CBF_SUCCESS == error) {
				free((void*)handle->nxid_name);
				handle->nxid_name = _cbf_strdup(groupName);
			}
			if (CBF_SUCCESS == error && group) *group = handle->nxid;
			return error;
		} else {
			if (!name) {
				if (group) *group = handle->nxid;
				return CBF_SUCCESS;
			} else {
				/*
				 * Check the names match, eventually.
				 * Could be awkward as anonymous groups do not have names.
				 */
				return CBF_NOTIMPLEMENTED;
			}
		}
	}

	/** \brief Ensure I have a valid NXsample group in the file.

	<p>Check for an <code>NXsample</code> group in <code>handle</code>, creating it with the name <code>sample</code>
	and NeXus class <code>NXsample</code> if it doesn't exist. Calls <code>cbf_h5handle_require_entry</code> to ensure
	that the entry group exists within the handle, if needed.</p>

	\param handle The HDF5 handle to use.
	\param group An optional pointer to a place where the group should be stored.

	\return <p>An error code.</p>
     */
	int cbf_h5handle_require_sample
			(const cbf_h5handle handle,
			 hid_t * group)
	{
		if (!handle) return CBF_ARGUMENT;
		if (cbf_H5Ivalid(handle->nxsample)) {
			/* return it */
			if (group) *group = handle->nxsample;
			return CBF_SUCCESS;
		} else {
			/* try to create it */
			int error = CBF_SUCCESS;
		cbf_reportnez(cbf_h5handle_require_entry(handle,0,0), error);
			cbf_reportnez(_cbf_NXGrequire(handle->nxid,&handle->nxsample,"sample","NXsample"), error);
			if (CBF_SUCCESS == error && group) *group = handle->nxsample;
		return error;
	}
	}

	/** \brief Ensure I have a valid NXinstrument group in the file.

	<p>Check for an <code>NXinstrument</code> group in <code>handle</code>, creating it with the name
	<code>instrument</code> and NeXus class <code>NXinstrument</code> if it doesn't exist. Calls
	<code>cbf_h5handle_require_entry</code> to ensure that the entry group exists within the handle, if needed.</p>

	\param handle The HDF5 handle to use.
	\param group An optional pointer to a place where the group should be stored.

	\return <p>An error code.</p>
     */
	int cbf_h5handle_require_instrument
			(const cbf_h5handle handle,
			 hid_t * group)
	{
		if (!handle) return CBF_ARGUMENT;
		if (cbf_H5Ivalid(handle->nxinst)) {
			/* return it */
			if (group) *group = handle->nxinst;
			return CBF_SUCCESS;
		} else {
			/* try to create it */
			int error = CBF_SUCCESS;
		cbf_reportnez(cbf_h5handle_require_entry(handle,0,0), error);
			cbf_reportnez(_cbf_NXGrequire(handle->nxid,&handle->nxinst,"instrument","NXinstrument"), error);
			if (CBF_SUCCESS == error && group) *group = handle->nxinst;
		return error;
	}
	}

	/** \brief Ensure I have a valid NXdetector group in the file.

	<p>Check for an <code>NXdetector</code> group in <code>handle</code>, creating it with the name
	<code>detector</code> and NeXus class <code>NXdetector</code> if it doesn't exist. Calls
	<code>cbf_h5handle_require_instrument</code> to ensure that the instrument group exists within the handle, if
	needed.</p>

	\param handle The HDF5 handle to use.
	\param group An optional pointer to a place where the group should be stored.
	\param name The (optional) group name.

	\return <p>An error code.</p>
     */
	int cbf_h5handle_require_detector
			(const cbf_h5handle handle,
			 hid_t * group,
			 const char * const name)
	{
		if (!handle) return CBF_ARGUMENT;
		if (cbf_H5Ivalid(handle->nxdetector)) {
			/* check its name, or return it */
			if (name) {
				return CBF_NOTIMPLEMENTED;
			} else {
			if (group) *group = handle->nxdetector;
			return CBF_SUCCESS;
		}
		} else {
			/* try to create it */
			int error = CBF_SUCCESS;
			const char default_name[] = "detector";
			const char * const _name = name ? name : default_name;
		cbf_reportnez(cbf_h5handle_require_instrument(handle,0), error);
			cbf_reportnez(_cbf_NXGrequire(handle->nxinst,&handle->nxdetector,_name,"NXdetector"), error);
			if (CBF_SUCCESS == error) {
				free((void*)handle->nxdetector_name);
				handle->nxdetector_name = _cbf_strdup(_name);
			}
			if (CBF_SUCCESS == error && group) *group = handle->nxdetector;
		return error;
	}
	}

	/** \brief Ensure I have a valid NXmonochromator group in the file.

	<p>Check for an <code>NXmonochromator</code> group in <code>handle</code>, creating it with the name
	<code>monochromator</code> and NeXus class <code>NXmonochromator</code> if it doesn't exist. Calls
	<code>cbf_h5handle_require_instrument</code> to ensure that the instrument group exists within the handle, if
	needed.</p>

	\param handle The HDF5 handle to use.
	\param group An optional pointer to a place where the group should be stored.

	\return <p>An error code.</p>
     */
	int cbf_h5handle_require_monochromator
			(const cbf_h5handle handle,
			 hid_t * group)
	{
		if (!handle) return CBF_ARGUMENT;

		if (cbf_H5Ivalid(handle->nxmonochromator)) {
			/* return it */
			if (group) *group = handle->nxmonochromator;
			return CBF_SUCCESS;
		} else {
			/* try to create it */
			int error = CBF_SUCCESS;
			cbf_reportnez(cbf_h5handle_require_instrument(handle,0), error);
			cbf_reportnez(_cbf_NXGrequire(handle->nxinst,&handle->nxmonochromator,"monochromator","NXmonochromator"), error);
			if (CBF_SUCCESS == error && group) *group = handle->nxmonochromator;
			return error;
		}
	}
    
    /* Create a dotted CBF location string
     returns a newly allocated string that
     must be freed */

    int cbf_location_string(const char* datablock,
                            const char* category,
                            const char* column,
                            unsigned int row,
                            char * * stringout) {

        size_t dblen, catlen,collen,rowlen;

        char rownum[20];

        if (!stringout) return CBF_ARGUMENT;

        if (!datablock) datablock = "_(NULL)_";
        if (!category) category = "_(NULL)_";
        if (!column) column = "_(NULL)_";

        sprintf(rownum,"%u",row);

        dblen = strlen(datablock);
        catlen = strlen(category);
        collen = strlen(column);
        rowlen = strlen(rownum);

        if (dblen == 0) {
            datablock = "_(NULL)_"; dblen = 8;
        }
        if (catlen == 0) {
            category = "_(NULL)_"; catlen = 8;
        }
        if (collen == 0) {
            column = "_(NULL)_"; collen = 8;
        }

        cbf_failnez(cbf_alloc(((void **) stringout),NULL,
                              dblen+catlen+collen+rowlen+4,1));

        strcpy(*stringout,datablock);

        strcpy((*stringout)+dblen,".");

        strcpy((*stringout)+dblen+1,category);

        strcpy((*stringout)+dblen+1+catlen,".");

        strcpy((*stringout)+dblen+1+catlen+1,column);

        strcpy((*stringout)+dblen+1+catlen+1+collen,".");

        strcpy((*stringout)+dblen+1+catlen+1+collen+1,rownum);

        return CBF_SUCCESS;

    }

    /* Conatenate two strings, returning a newly allocated string */

    int cbf_strcat(const char * string1, const char * string2,
                   char * * stringout) {

        int errorcode;

        size_t len1, len2;

        if (!string1 || !string2 || !stringout) return CBF_ARGUMENT;

        errorcode = 0;

        len1 = strlen(string1);

        len2 = strlen(string2);

        cbf_failnez(cbf_alloc(((void **) stringout),NULL,
                              len1+len2+1,1));

        strcpy(*stringout,string1);

        strcpy((*stringout)+len1,string2);

        return CBF_SUCCESS;

    }


    /* Either open or create a NeXus group*/


    int cbf_require_nxgroup(cbf_h5handle h5handle,
                            const char * nxgroup,
                            const char * nxclass,
                            hid_t parent_id,
                            hid_t * groupid) {

        int errorcode;

        if (!h5handle || !nxgroup || !nxclass || !groupid ) return CBF_ARGUMENT;

        errorcode = 0;

        if (parent_id < 0) parent_id = h5handle->curnxid;

        if (parent_id < 0) parent_id = h5handle->nxid;

        if ((H5Lexists(parent_id,nxgroup,H5P_DEFAULT)) != 1) {

            /* ensure it goes right below the parent_id */

            if (h5handle->curnxid>=0 && h5handle->curnxid!= parent_id) {

                cbf_h5reportneg(H5Gclose(h5handle->curnxid),CBF_ARGUMENT,errorcode);

            }

            h5handle->curnxid = parent_id;

            cbf_reportnez(cbf_H5Gcreate_in_handle(h5handle,nxgroup,groupid),errorcode);

            cbf_reportnez(cbf_apply_h5text_attribute(*groupid,
                                                     "NX_class",nxclass,0),errorcode);

            h5handle->curnxid = CBF_H5FAIL;

        } else {

            if ((*groupid = H5Gopenx(parent_id,nxgroup))<0) return CBF_NOTFOUND;

        }

        return errorcode;

    }


    /* get an axis vector and offset */

    int cbf_get_axis_vector_and_offset(cbf_handle handle,
                                       const char *axis_id,
                                       double vector[3],
                                       double offset[3]) {

        /***** Allow for missing vector or offset treat as 0 ***/

        if (!handle || !axis_id ) return CBF_ARGUMENT;

        cbf_failnez (cbf_find_category   (handle, "axis"))
        cbf_failnez (cbf_find_column     (handle, "id"))
        cbf_failnez (cbf_find_row        (handle, axis_id))

        if (vector) {

            cbf_failnez (cbf_find_column     (handle, "vector[1]"))
            if (cbf_get_doublevalue (handle, vector)) vector[0] = 0;
            cbf_failnez (cbf_find_column     (handle, "vector[2]"))
            if (cbf_get_doublevalue (handle, vector+1)) vector[1] = 0.;
            cbf_failnez (cbf_find_column     (handle, "vector[3]"))
            if (cbf_get_doublevalue (handle, vector+2)) vector[2] = 0.;

        }

        if (offset) {

            cbf_failnez (cbf_find_column     (handle, "offset[1]"))
            if (cbf_get_doublevalue (handle, offset)) offset[0] = 0.;
            cbf_failnez (cbf_find_column     (handle, "offset[2]"))
            if (cbf_get_doublevalue (handle, offset+1)) offset[1] = 0.;
            cbf_failnez (cbf_find_column     (handle, "offset[3]"))
            if (cbf_get_doublevalue (handle, offset+2)) offset[2] = 0.;

        }

        return CBF_SUCCESS;


    }


    /* Compute the cross-product of 2 3-vectors */

    int cbf_cross_product(double vecin1[3],
                          double vecin2[3],
                          double vecout[3] ) {

        if (!vecin1 || !vecin2 || !vecout) return CBF_ARGUMENT;

        vecout[0] = vecin1[1]*vecin2[2] - vecin1[2]*vecin2[1];

        vecout[1] = vecin1[2]*vecin2[0] - vecin1[0]*vecin2[2];

        vecout[2] = vecin1[0]*vecin2[1] - vecin1[1]*vecin2[0];

        return CBF_SUCCESS;

    }

    /* compute the L2 norm of a 3-vector */

    double cbf_norm(double vector[3]) {

        if (!vector) return -1;

        return sqrt(vector[0]*vector[0]+vector[1]*vector[1]+vector[2]*vector[2]);

    }

    /* compute the product of a scalar and a vector */

    int cbf_scalar_product(double scalar, double vecin[3], double vecout[3]) {

        if (!vecin || ! vecout) return CBF_ARGUMENT;

        vecout[0] = scalar * vecin[0];

        vecout[1] = scalar * vecin[1];

        vecout[2] = scalar * vecin[2];

        return CBF_SUCCESS;

    }

    /* Apply a matrix to a vector */

    int cbf_apply_matrix(double matrix[3][3], double vecin[3], double vecout[3]) {

        int ii;

        if (!matrix || !vecin || !vecout ) return CBF_ARGUMENT;

        for (ii=0; ii < 3; ii++) {

            vecout[ii] = matrix[ii][0]*vecin[0]
            + matrix[ii][1]*vecin[1]
            + matrix[ii][2]*vecin[2];

            if (fabs(vecout[ii])<=1.e-15) vecout[ii] = 0.;

        }

        return CBF_SUCCESS;

    }



    /* compute the transform from CBF vectors to NeXus vectors
     Use the transpose to transfrom from NeXus vectors to CBF*/

    int cbf_get_NX_axis_transform(cbf_handle handle,
                                  double matrix [3][3]) {

        double beam[3];

        double gravity[3];

        double x_nx[3], y_nx[3];

        double normx_nx, normy_nx;

        if (!handle || !matrix) return CBF_ARGUMENT;

        /* cross multiple datablocks to find the AXIS category */

        if (cbf_find_tag(handle,"_axis.id")) return CBF_NOTFOUND;

        /* take the beam, if given, -source if source is given, or -Z */

        if (cbf_get_axis_vector_and_offset(handle,"BEAM",beam, NULL)) {

            if (cbf_get_axis_vector_and_offset(handle,"SOURCE",beam, NULL)) {

                beam[0] = 0.; beam[1] = 0.; beam[2] = -1.;

            } else {

                beam[0] = -beam[0]; beam[1] = -beam[1]; beam[2] = -beam[2];
            }

        }

        /* take gravity if given, otherwise [0, -1, 0 ],
         -up if up is givem, otherwise -Y */

        if (cbf_get_axis_vector_and_offset(handle,"GRAVITY",gravity,NULL)) {

            if (cbf_get_axis_vector_and_offset(handle,"GRAVITY",gravity,NULL)) {

                gravity[0] = 0; gravity[1] = -1.; gravity[2] = 0.;

            }

        }

        cbf_failnez(cbf_cross_product(beam,gravity,x_nx));

        normx_nx = cbf_norm(x_nx);

        if (normx_nx <= 1.e-38) return CBF_ARGUMENT;

        cbf_failnez(cbf_scalar_product(1./normx_nx,x_nx,x_nx));


        cbf_failnez(cbf_cross_product(beam,x_nx,y_nx));

        normy_nx = cbf_norm(y_nx);

        if (normy_nx <= 1.e-38) return CBF_ARGUMENT;

        cbf_failnez(cbf_scalar_product(1./normy_nx,y_nx,y_nx));

        matrix[0][0] = x_nx[0]; matrix[0][1] = x_nx[1]; matrix[0][2] = x_nx[2];

        matrix[1][0] = y_nx[0]; matrix[1][1] = y_nx[1]; matrix[1][2] = y_nx[2];

        matrix[2][0] = beam[0]; matrix[2][1] = beam[1]; matrix[2][2] = beam[2];

        return CBF_SUCCESS;

    }

	/*
	Get the axis transform for the current datablock, instead of an arbitrarily chosen datablock.
	*/

	int cbf_get_NX_axis_transform2
			(cbf_handle handle,
			 double matrix [3][3])
	{
		double beam[3];
		double gravity[3];
		double x_nx[3], y_nx[3];
		double normx_nx, normy_nx;

		if (!handle || !matrix) return CBF_ARGUMENT;

		/* take the beam direction, if given, -source if source is given, or -Z */
		if (cbf_get_axis_vector_and_offset(handle,"BEAM",beam,NULL)) {
			if (cbf_get_axis_vector_and_offset(handle,"SOURCE",beam, NULL)) {
				beam[0] = 0.;
				beam[1] = 0.;
				beam[2] = -1.;
			} else {
				beam[0] = -beam[0];
				beam[1] = -beam[1];
				beam[2] = -beam[2];
			}
		}

        /* take gravity if given, otherwise [0, -1, 0 ], -up if up is given, otherwise -Y */
		if (cbf_get_axis_vector_and_offset(handle,"GRAVITY",gravity,NULL)) {
			if (cbf_get_axis_vector_and_offset(handle,"UP",gravity,NULL)) {
				gravity[0] = 0.;
				gravity[1] = -1.;
				gravity[2] = 0.;
			} else {
				gravity[0] = -gravity[0];
				gravity[1] = -gravity[1];
				gravity[2] = -gravity[2];
			}
		}

		/* try to get normalised 'x' axis */
		cbf_failnez(cbf_cross_product(beam,gravity,x_nx));
		normx_nx = cbf_norm(x_nx);
		if (normx_nx <= 1.e-38) return CBF_ARGUMENT;
		cbf_failnez(cbf_scalar_product(1./normx_nx,x_nx,x_nx));

		/* try to get normalised 'y' axis */
		cbf_failnez(cbf_cross_product(beam,x_nx,y_nx));
		normy_nx = cbf_norm(y_nx);
		if (normy_nx <= 1.e-38) return CBF_ARGUMENT;
		cbf_failnez(cbf_scalar_product(1./normy_nx,y_nx,y_nx));

		/* form the transformation matrix */
		matrix[0][0] = x_nx[0]; matrix[0][1] = x_nx[1]; matrix[0][2] = x_nx[2];
		matrix[1][0] = y_nx[0]; matrix[1][1] = y_nx[1]; matrix[1][2] = y_nx[2];
		matrix[2][0] = beam[0]; matrix[2][1] = beam[1]; matrix[2][2] = beam[2];

		return CBF_SUCCESS;
	}


    /* Write the HDF5 version of the NeXus axis definitions, if
     the original CBF had axis definitions */

    int cbf_write_h5nxaxes(cbf_handle handle, cbf_h5handle h5handle) {


        int errorcode;

        unsigned int rows, row;

        double matrix[3][3];

        hsize_t one=1;

        hsize_t naught=0;

        double zero[1];

        hid_t instrumentid;

        const char* datablock;

        errorcode = 0;

        zero[0] = 0.;

        /* If we get a transform, there are axes to convert */

        cbf_reportnez(cbf_get_NX_axis_transform(handle, matrix),errorcode);

        if (errorcode) return errorcode;


        /* We will need use the instrument group or create it*/

        cbf_reportnez(cbf_require_nxgroup(h5handle,
                                          "instrument",
                                          "NXinstrument",
                                          h5handle->nxid,
                                          &instrumentid),errorcode);

        cbf_reportnez(cbf_find_category(handle, "axis"),errorcode);

        cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

        cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);

        cbf_reportnez(cbf_datablock_name(handle,&datablock),errorcode);

        for(row=0; row < rows; row++) {

            char * cbfloc;

            char nxequipment[2048];

            hid_t equipmentid;

            hid_t nxaxisid;

            hid_t nxaxisoffsetid;

            hid_t dtype, dspace, dprop;

            const char * equipment;

            const char * equipmentclass;

            const char * equipmentname;

            const char * axis_id;

            const char * depends_on;

            const char * rotation_axis;

            const char * type;

            const char * system;

            hsize_t scanpoints;

            size_t sscanpoints;

            const char * units;

            double vector[3], offset[3];

            double rotation;

            cbf_reportnez(cbf_find_category(handle, "axis"),errorcode);

            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

            depends_on = ".";

            rotation_axis = ".";
            
            rotation = 0.0;

            equipment = "general";

            equipmentclass = "NXcoordinate_system";

            type = "general";

            system = "laboratory";

            cbf_reportnez(cbf_select_row(handle,row),errorcode);

            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

            cbf_reportnez(cbf_get_value(handle,&axis_id),errorcode);

            if (!cbf_find_column(handle,"equipment")) {

                cbf_reportnez(cbf_get_value(handle,&equipment),errorcode);

                if (!equipment)  equipment = "general";

            }

            if (!cbf_find_column(handle,"depends_on")) {

                cbf_reportnez(cbf_get_value(handle,&depends_on),errorcode);

                if (!depends_on) depends_on = ".";

            }

            if (!cbf_find_column(handle,"rotation_axis")) {
                
                cbf_reportnez(cbf_get_value(handle,&depends_on),errorcode);
                
                if (!depends_on) rotation_axis = ".";
                
            }
            

            if (!cbf_find_column(handle,"rotation")) {
                
                cbf_reportnez(cbf_get_doublevalue(handle,&rotation),errorcode);
                
            }


            if (!cbf_find_column(handle,"type")) {

                cbf_reportnez(cbf_get_value(handle,&type),errorcode);

                if (!type) type = "general";

            }

            if (!cbf_find_column(handle,"system")) {

                cbf_reportnez(cbf_get_value(handle,&system),errorcode);

                if (!system||!system[0]) system = "laboratory";

                if (cbf_cistrcmp(system,".")||cbf_cistrcmp(system,"?"))
                    system = "laboratory";

            }

            if (cbf_cistrcmp(system,"laboratory")) {


                cbf_reportnez(cbf_get_axis_vector_and_offset(handle,axis_id,
                                                             vector, offset),errorcode);

            } else {

                double cbfvector[3], cbfoffset[3];

                cbf_reportnez(cbf_get_axis_poise(handle, 0.,
                                                 (double *)cbfvector,(double *)cbfvector+1,(double *)cbfvector+2,
                                                 (double *)cbfoffset,(double *)cbfoffset+1,(double *)cbfoffset+2,
                                                 NULL,axis_id,NULL),errorcode);

                system = "McStas_absolute";

                cbf_reportnez(cbf_apply_matrix(matrix,cbfvector,vector),errorcode);

                cbf_reportnez(cbf_apply_matrix(matrix,cbfoffset,offset),errorcode);


            }

            /*  We have the equipment type in equipment and the axis is in axis_id
             If the equipment type is detector, we need to map the axis_id
             to the appropriate detector so we can put this axis in
             /instrument:NXinstrument
             /CBF_diffrn_detector__DETECTORNAME:NXdetector
             /CBF_axis__AXISID=[]

             If the equipment type is goniometer, we need to map the axis_id
             to the appropriate goniometer, so we can put this axis in
             /instrument:NXinstrument
             /CBF_diffrn_measurement__GONIOMETERNAME:NXsample
             /CBF__axis__AXISID=[]

             For other equipment types, we put this axis in
             /instrument:NXinstrument
             /coordinate_system:NXcoordinate_system
             /CBF__axis__AXISID=[]
             */

            cbf_reportnez(cbf_get_axis_equipment_id(handle,&equipmentname,equipment,axis_id),errorcode);

            if (!equipment) equipment = "";

            if (cbf_cistrcmp(equipment,"detector")==0) {

                strcpy(nxequipment,"CBF_diffrn_detector__");

                if (equipmentname) {

                    strncat(nxequipment,equipmentname,2020);

                } else {

                    strcpy(nxequipment,"detector");
                }

                equipmentclass = "NXdetector";

            } else if (cbf_cistrcmp(equipment,"goniometer")==0) {

                strcpy(nxequipment,"CBF_diffrn_measurement__");

                if (equipmentname) {

                    strncat(nxequipment,equipmentname,2020);

                    nxequipment[2047] = 0;

                } else {

                    strcpy(nxequipment,"sample");
                }


                equipmentclass = "NXsample";

            } else {

                strcpy(nxequipment,"coordinate_system");

                equipmentclass = "NXcoordinate_system";

            }


            cbf_reportnez(cbf_require_nxgroup(h5handle,
                                              nxequipment, equipmentclass,
                                              instrumentid, &equipmentid),errorcode);

            cbf_reportnez(cbf_get_axis_parameters(handle,
                                                  &sscanpoints,
                                                  &units,
                                                  equipment,
                                                  axis_id),errorcode);

            scanpoints = (hsize_t)sscanpoints;

            /* At this point we are ready to write the field CBF_axis__AXISID[] */



            if (cbf_norm(offset) > 1.e-20) {

                char * nxaxis_offset_name;

                char * nxaxis_name;

                char * nxdepends_on_name;

                char * nxrotation_axis_name;

                hid_t mtype;

                cbf_reportnez(cbf_strcat("CBF_axis_offset__",
                                         axis_id,&nxaxis_offset_name),
                              errorcode);

                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         axis_id,&nxaxis_name),errorcode);

                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         depends_on,&nxdepends_on_name),errorcode);

                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         rotation_axis,&nxrotation_axis_name),errorcode);

                cbf_h5reportneg(dspace = H5Screate_simple(1,&naught,&one),CBF_ALLOC,errorcode);

                cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

                cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);

                cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);

                cbf_h5reportneg(H5Pset_chunk(dprop, 1, &one),CBF_ALLOC,errorcode);

                cbf_h5reportneg(nxaxisoffsetid = H5Dcreatex(equipmentid,nxaxis_offset_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);

                /* cbf_h5reportneg(H5Dwrite(nxaxisoffsetid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)zero),CBF_ALLOC,errorcode); */


                cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);

                cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);

                cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);

                cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);


                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "transformation_type",
                                                        "translation",
                                                        errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "system",system,errorcode);


                errorcode |= cbf_apply_h5vector_attribute(nxaxisoffsetid,
                                                          "vector",(double *)offset,3,errorcode);

                if (!cbf_cistrcmp(depends_on,".")) {

                    errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                            "depends_on",depends_on,errorcode);

                } else {

                    errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                            "depends_on",nxdepends_on_name,errorcode);

                }

                if (cbf_cistrcmp(rotation_axis,".")) {
                                        
                    errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                            "rotation_axis",nxrotation_axis_name,errorcode);
                    
                    errorcode |= cbf_apply_h5vector_attribute(nxaxisoffsetid,
                                                            "rotation",&rotation,1,errorcode);
                    
                }


                cbf_reportnez(cbf_location_string(datablock,"axis","offset",row,&cbfloc),errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);

                cbf_h5reportneg(H5Dclose(nxaxisoffsetid),CBF_FORMAT,errorcode);


                if (scanpoints > 0) {

                    hsize_t scanpointsfound;

                    hid_t mtype;

                    size_t sscanpointsfound;

                    double * scanarray;

                    cbf_reportnez(cbf_alloc(((void **) &scanarray),NULL,
                                            scanpoints*sizeof(double),1),errorcode);

                    cbf_reportnez(cbf_get_axis_scan_points(handle,
                                                           scanarray,
                                                           (size_t)scanpoints,
                                                           &sscanpointsfound,
                                                           units,
                                                           axis_id),errorcode);

                    scanpointsfound = (hsize_t)sscanpointsfound;

                    if (sscanpointsfound==0) scanpointsfound=1;

                    cbf_h5reportneg(dspace = H5Screate_simple(1,&scanpointsfound,&scanpoints),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)scanarray),CBF_ALLOC,errorcode);;

                    cbf_reportnez(cbf_free((void **)(&scanarray),NULL),errorcode);

                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);

                    if (units) {

                        errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                                "units",units,errorcode);
                    }

                } else {

                    hid_t mtype;

                    cbf_h5reportneg(dspace = H5Screate_simple(1,&naught,&one),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Pset_chunk(dprop, 1, &one),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);

                    /* cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)zero),CBF_ALLOC,errorcode);*/

                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);

                }

                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "transformation_type",type,errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "system",system,errorcode);

                errorcode |= cbf_apply_h5vector_attribute(nxaxisid,
                                                          "vector",(double *)vector,3,errorcode);


                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "depends_on",nxaxis_offset_name,errorcode);

                cbf_reportnez(cbf_location_string(datablock,"axis","vector",row,&cbfloc),errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);

                cbf_h5reportneg(H5Dclose(nxaxisoffsetid),CBF_ALLOC,errorcode);

                cbf_reportnez(cbf_free((void **)&nxaxis_offset_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxaxis_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxdepends_on_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxrotation_axis_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&cbfloc,NULL),errorcode);

            } else {

                char * nxaxis_name;

                char * nxdepends_on_name;

                char * nxrotation_axis_name;

                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         axis_id,&nxaxis_name),errorcode);

                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         depends_on,&nxdepends_on_name),errorcode);

                cbf_reportnez(cbf_strcat("CBF_axis__",
                                         rotation_axis,&nxrotation_axis_name),errorcode);
                

                if (scanpoints > 0) {

                    hsize_t scanpointsfound;

                    hid_t mtype;

                    size_t sscanpointsfound;

                    double * scanarray;

                    cbf_reportnez(cbf_alloc(((void **) &scanarray),NULL,
                                            scanpoints*sizeof(double),1),errorcode);

                    cbf_reportnez(cbf_get_axis_scan_points(handle,
                                                           scanarray,
                                                           (size_t)scanpoints,
                                                           &sscanpointsfound,
                                                           units,
                                                           axis_id),errorcode);

                    scanpointsfound = (hsize_t)sscanpointsfound;

                    if (sscanpointsfound == 0) scanpointsfound=1;

                    cbf_h5reportneg(dspace = H5Screate_simple(1,&scanpointsfound,&scanpoints),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)scanarray),CBF_ALLOC,errorcode);

                    cbf_reportnez(cbf_free((void **)(&scanarray),NULL),errorcode);

                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);

                    if (units) {

                        errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                                "units",units,errorcode);
                    }
                } else {

                    hid_t mtype;

                    cbf_h5reportneg(dspace = H5Screate_simple(1,&naught,&one),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(mtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(dprop = H5Pcreate(H5P_DATASET_CREATE),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Pset_chunk(dprop, 1, &one),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(nxaxisid = H5Dcreatex(equipmentid,nxaxis_name,dtype,dspace,dprop),CBF_ALLOC,errorcode);

                    /* cbf_h5reportneg(H5Dwrite(nxaxisid, mtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)zero),CBF_ALLOC,errorcode); */

                    cbf_h5reportneg(H5Sclose(dspace),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(dtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Tclose(mtype),CBF_ALLOC,errorcode);

                    cbf_h5reportneg(H5Pclose(dprop),CBF_ALLOC,errorcode);


                }

                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "transformation_type",
                                                        type,
                                                        errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "system",system,errorcode);

                errorcode |= cbf_apply_h5vector_attribute(nxaxisid,
                                                          "vector",(double *)vector,3,errorcode);

                if (!cbf_cistrcmp(depends_on,".")) {

                    errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                            "depends_on",depends_on,errorcode);

                } else {

                    errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                            "depends_on",nxdepends_on_name,errorcode);

                }

                if (cbf_cistrcmp(rotation_axis,".")) {
                    
                    errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                            "rotation_axis",nxrotation_axis_name,errorcode);
                    
                    errorcode |= cbf_apply_h5vector_attribute(nxaxisid,
                                                            "rotation",&rotation,1,errorcode);
                    
                }

                cbf_reportnez(cbf_location_string(datablock,"axis","vector",row,&cbfloc),errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);

                cbf_h5reportneg(H5Dclose(nxaxisid),CBF_FORMAT,errorcode);

                cbf_reportnez(cbf_free((void **)&nxaxis_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxdepends_on_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxrotation_axis_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&cbfloc,NULL),errorcode);
            }

            cbf_h5reportneg(H5Gclose(equipmentid),CBF_FORMAT,errorcode);

        }

        return errorcode;

    }

    /* apply a double vector attribute to a group or dataset */

    int cbf_apply_h5vector_attribute(hid_t hid,
                                     const char* attribname,
                                     const double* attribvec,
                                     const size_t dimension,
                                     int errorcode)
    {

        hid_t attribspace, attribtype, attribid;

        hid_t attribmemtype;

        hsize_t dims[1];

        attribspace = attribtype = attribmemtype = attribid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !attribname|| !attribvec || dimension < 1 ) return CBF_ARGUMENT;

        cbf_h5reportneg(attribspace = H5Screate(H5S_SIMPLE),CBF_ALLOC,errorcode);

        dims[0] = dimension;

        cbf_h5reportneg(H5Sset_extent_simple(attribspace, 1, dims, NULL),CBF_FORMAT,errorcode);

        cbf_h5reportneg(attribtype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribmemtype = H5Tcopy(H5T_NATIVE_DOUBLE),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Awrite(attribid,attribmemtype,
                                 attribvec),CBF_ALLOC,errorcode);

        if (attribspace >= 0)  H5Sclose(attribspace);

        if (attribtype >= 0)   H5Tclose(attribtype);

        if (attribmemtype >= 0)   H5Tclose(attribmemtype);

        if (attribid >= 0)     H5Aclose(attribid);

        return errorcode;

    }


    /* apply a long attribute to a group or dataset */

    int cbf_apply_h5longasstr_attribute(hid_t hid,
                                        const char* attribname,
                                        const long attriblong,
                                        int errorcode)
    {
        char buffer[20];

        if (attriblong > -10 && attriblong < 10) {

            sprintf(buffer,"%ld",attriblong);

        } else {

            sprintf(buffer,"0x%lx",attriblong);

        }

        return cbf_apply_h5text_attribute(hid,attribname,buffer,errorcode);

    }


    /* apply an integer attribute to a group or dataset as a string */

    int cbf_apply_h5intasstr_attribute(hid_t hid,
                                       const char* attribname,
                                       const int attribint,
                                       int errorcode)
    {
        char buffer[20];

        if (attribint > -10 && attribint < 10) {

            sprintf(buffer,"%d",attribint);

        } else {

            sprintf(buffer,"0x%x",attribint);

        }

        return cbf_apply_h5text_attribute(hid,attribname,buffer,errorcode);

    }



    /* apply a integer attribute to a group or dataset */

    int cbf_apply_h5integer_attribute(hid_t hid,
                                      const char* attribname,
                                      const int attribint,
                                      int errorcode)
    {

        hid_t attribspace, attribtype, attribid;

        attribspace = attribtype = attribid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !attribname ) return CBF_ARGUMENT;

        cbf_h5reportneg(attribspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribtype = H5Tcopy(H5T_NATIVE_INT),CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Tset_size(attribtype,1),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Awrite(attribid,attribtype,
                                 &attribint),CBF_ALLOC,errorcode);

        if (attribspace >= 0)  H5Sclose(attribspace);

        if (attribtype >= 0)   H5Tclose(attribtype);

        if (attribid >= 0)     H5Aclose(attribid);

        return errorcode;

    }


    /* apply a text attribute to a group or dataset */

    int cbf_apply_h5text_attribute(hid_t hid,
                                   const char* attribname,
                                   const char* attribtext,
                                   int errorcode)
    {

        hid_t attribspace, attribtype, attribid;

        htri_t attribexists;

        attribspace = attribtype = attribid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !attribname || !attribtext ) return CBF_ARGUMENT;

        /* if the attribute exists, check the value is the same */

        attribexists = H5Aexists(hid,attribname);

        if (attribexists >=0 && attribexists) {

            hsize_t attribsize;

            hsize_t memtype;

            char * attribtextbuffer;

            cbf_h5reportneg(attribid = H5Aopen(hid, attribname, H5P_DEFAULT),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(attribtype = H5Aget_type(attribid),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(attribsize = H5Tget_size(attribtype),CBF_H5ERROR,errorcode);

            cbf_reportnez(cbf_alloc(((void **) attribtextbuffer),NULL,
                                    attribsize+1,1),errorcode);

            cbf_h5reportneg(memtype = H5Tget_native_type(attribtype,H5T_DIR_ASCEND),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(H5Aread(attribid,memtype,&attribtextbuffer),CBF_H5ERROR,errorcode);

            if (cbf_cistrcmp(attribtext,attribtextbuffer)) errorcode |= CBF_H5DIFFERENT;

            if (cbf_H5Ivalid(memtype)) H5Tclose(memtype);

            if (cbf_H5Ivalid(attribtype)) H5Tclose(attribtype);

            if (cbf_H5Ivalid(attribid)) H5Aclose(attribid);

            if (attribtextbuffer) cbf_reportnez(cbf_free((void * *) &attribtextbuffer,NULL),errorcode);

            return errorcode;

        }

        cbf_h5reportneg(attribspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Tset_size(attribtype,strlen(attribtext)),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Awrite(attribid,attribtype,
                                 (const void *)attribtext),CBF_ALLOC,errorcode);

        if (attribspace >= 0)  H5Sclose(attribspace);

        if (attribtype >= 0)   H5Tclose(attribtype);

        if (attribid >= 0)     H5Aclose(attribid);

        return errorcode;

    }

    /* apply a text dataset slab to a group

     places the specified datasettext in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.

     The slabs are indexed from 0

     */

    int cbf_add_h5text_dataset_slab(hid_t hid,
                                      const char* datasetname,
                                      const char* datasettext,
                                      const hsize_t slab,
                                      int errorcode)
    {
        hid_t datasetspace, datasettype, datasetid;

        hid_t memspace, memtype;

        hid_t ndatasettype;

        hid_t ndatasetspace;

        hid_t nmemtype;

        int ndims;

        hsize_t offset[1] = {0};

        hsize_t stride[1] = {1};

        hsize_t count[1]  = {1};

        hsize_t chunk[1] = {1};

        hsize_t curdim[1];

        hsize_t memsize[1] = {1};

        htri_t dsexists;

        hsize_t dssize[1];

        hsize_t maxdssize[1];

        hsize_t dsdims[1];

        hsize_t dsmaxdims[1];

        hsize_t dsslab;

        hid_t anondataset;

        void * datasettextbuffer;

        size_t old_size, new_size;

        datasetspace = datasettype = memspace = memtype = CBF_H5FAIL;

        ndatasetspace = ndatasettype = nmemtype = CBF_H5FAIL;

        datasetid = CBF_H5FAIL;

        memsize[0] = 1;

        dssize[0] = 1;

        maxdssize[0] = H5S_UNLIMITED;

        chunk[0] = 1;


        /* ensure arguments all given */

        if (hid < 0 || !datasetname ||
            !datasettext || errorcode) return CBF_ARGUMENT;

        dsexists = H5Lexists(hid,datasetname, H5P_DEFAULT);

        if (dsexists < 0 ||
            !dsexists
            || (datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT))< 0) {

            /* Create the dataset if we were unable to open it */

            cbf_h5reportneg(datasettype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

            cbf_h5reportneg(H5Tset_size(datasettype,strlen(datasettext)),CBF_ALLOC,errorcode);

            cbf_reportnez(cbf_H5Dcreate(hid,&datasetid,datasetname,1,dssize,maxdssize,chunk,datasettype),errorcode);

            curdim[0] = 0;

            dsdims[0] = 0;

        }  else {

            if (datasetid <= 0) {

                datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT);

            }

            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(ndims = H5Sget_simple_extent_ndims(datasetspace),CBF_FORMAT,errorcode);

            if ( errorcode || ndims != 1 ) return CBF_FORMAT;

            old_size = H5Tget_size(datasettype);

            new_size = strlen(datasettext);

            cbf_h5reportneg(H5Sget_simple_extent_dims(datasetspace,
                                                      dsdims,dsmaxdims),CBF_FORMAT,errorcode);

            if ( old_size < new_size ) {

                ndatasettype = CBF_H5FAIL;

                cbf_h5reportneg(ndatasettype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

                /* rebuild the dataset with longer strings */

                if (old_size+((old_size+1)>>1)>= new_size) {

                    new_size = old_size+((old_size+1)>>1);

                }

                cbf_reportnez(cbf_alloc(&datasettextbuffer,NULL,
                                        new_size+1,1),errorcode);

                if (datasettype >= 0) H5Tclose(datasettype);

                datasettype = ndatasettype;

                cbf_h5reportneg(H5Tset_size(datasettype,new_size),CBF_ALLOC,errorcode);

                cbf_h5reportneg(memtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

                cbf_h5reportneg(nmemtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

                cbf_h5reportneg(memspace = H5Screate_simple(1,memsize,NULL),CBF_ALLOC,errorcode);

                cbf_h5reportneg(H5Tset_size(memtype,new_size),CBF_ALLOC,errorcode);

                cbf_h5reportneg(H5Tset_size(nmemtype,new_size),CBF_ALLOC,errorcode);

                cbf_reportnez(cbf_H5Dcreate(hid,&anondataset,NULL,1,dsdims,maxdssize,chunk,datasettype),errorcode);

                for (dsslab=0; dsslab < dsdims[0]; dsslab++) {

                    offset[0] = dsslab;

                    cbf_h5reportneg(H5Sselect_hyperslab(datasetspace,H5S_SELECT_SET,
                                                        offset,stride,count,0), CBF_FORMAT,errorcode);

                    if (H5Dread(datasetid, memtype, memspace, datasetspace,
                                H5P_DEFAULT, (void *)datasettextbuffer)>=0) {

                        curdim[0] = dsslab+1;

                        cbf_h5reportneg(H5Dset_extent(anondataset,curdim),CBF_FORMAT,errorcode);

                        offset[0] = dsslab;

                        stride[0] = 1;

                        count[0] = 1;

                        cbf_reportnez(cbf_H5Dwrite2(anondataset,offset,stride,count,(void *)datasettextbuffer,memtype),errorcode);

                    }

                }

                if (nmemtype >= 0) H5Tclose(nmemtype);

                if (ndatasetspace >= 0) H5Sclose(ndatasetspace);

                if (datasetspace >= 0) H5Sclose(datasetspace);

                cbf_h5reportneg(H5Ldelete(hid,datasetname,H5P_DEFAULT), CBF_FORMAT, errorcode);

                cbf_h5reportneg(H5Olink(anondataset,hid,datasetname,H5P_DEFAULT,H5P_DEFAULT), CBF_FORMAT, errorcode);

                if (datasetid >= 0) H5Dclose(datasetid);

                datasetid = anondataset;

                if (datasettextbuffer) cbf_free(&datasettextbuffer,NULL);

                if (memtype >= 0) H5Tclose(memtype);

                datasetspace = memtype = CBF_H5FAIL;

            }

            if (datasetspace >=0 )H5Sclose(datasetspace);

            datasetspace = CBF_H5FAIL;


        }

        if (slab == H5S_UNLIMITED) {

            offset[0] = dsdims[0];

            curdim[0] = dsdims[0]+1;

        } else {

            offset[0] = slab;

            curdim[0] = slab+1;

        }

        cbf_h5reportneg(H5Dset_extent(datasetid,curdim),CBF_FORMAT,errorcode);

        stride[0] = 1;

        count[0] = 1;

        cbf_reportnez(cbf_H5Dwrite2(datasetid,offset,stride,count,(void *)datasettext,datasettype),errorcode);

        if (datasetspace >= 0)  H5Sclose(datasetspace);

        if (datasettype >= 0)   H5Tclose(datasettype);

        if (datasetid >= 0)     H5Dclose(datasetid);

        if (memtype >= 0)       H5Tclose(memtype);

        return errorcode;

    }

    /* apply a text dataset to a group */

    int cbf_add_h5text_dataset(hid_t hid,
                                 const char* datasetname,
                                 const char* datasettext,
                                 int errorcode)
    {

        hid_t datasetspace, datasettype, datasetid, datasetprop;

        htri_t dsexists;

        datasetspace = datasettype = datasetid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !datasetname || !datasettext
            || errorcode) return CBF_ARGUMENT;

        dsexists = H5Lexists(hid,datasetname,H5P_DEFAULT);

        if ( dsexists >=0 && dsexists ) {

            hsize_t dssize;

            hid_t memtype;

            int datasetrank;

            char * datasettextbuffer = NULL;

            /* The dataset exists, and will be replaced with an array
             of strings, unless it is already an array, in which case
             we just add to it */

            cbf_h5reportneg(datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(datasetrank = H5Sget_simple_extent_ndims(datasetspace),CBF_FORMAT,errorcode);

            if (datasetrank == 1) {

                /* This is an array, write the next slab */

                if (datasetspace >= 0)  H5Sclose(datasetspace);

                if (datasettype >= 0)   H5Tclose(datasettype);

                if (datasetid >= 0)     H5Dclose(datasetid);

                cbf_reportnez(cbf_add_h5text_dataset_slab(hid,
                                              datasetname,
                                              datasettext,
                                              H5S_UNLIMITED, errorcode),errorcode);


                return errorcode;

            }

            dssize = H5Tget_size(datasettype);

            cbf_reportnez(cbf_alloc((void * *)(&datasettextbuffer),NULL,
                                    dssize+1,1),errorcode);

            cbf_h5reportneg(memtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

            cbf_h5reportneg(H5Tset_size(memtype,dssize),CBF_ALLOC,errorcode);

            cbf_h5reportneg(H5Dread(datasetid, memtype, H5S_ALL, datasetspace,
                                    H5P_DEFAULT, (void *)datasettextbuffer),CBF_FORMAT,errorcode);

            /* unlink the current dataset and write the old data and the new data */

            cbf_h5reportneg(H5Ldelete(hid,datasetname,H5P_DEFAULT),CBF_FORMAT,errorcode);

            cbf_reportnez(cbf_add_h5text_dataset_slab(hid,
                                                        datasetname,
                                                        datasettextbuffer,
                                                        0, errorcode),errorcode);

            cbf_reportnez(cbf_add_h5text_dataset_slab(hid,
                                                        datasetname,
                                                        datasettext,
                                                        1, errorcode),errorcode);

            if (datasettextbuffer) cbf_free((void * *)(&datasettextbuffer),NULL);

            if (datasetspace >= 0)  H5Sclose(datasetspace);

            if (datasettype >= 0)   H5Tclose(datasettype);

            if (memtype >= 0)       H5Tclose(memtype);

            if (datasetid >= 0)     H5Dclose(datasetid);


            return errorcode;

        }

        cbf_h5reportneg(datasetspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasettype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Tset_size(datasettype,strlen(datasettext)),CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasetprop = H5Pcreate(H5P_DATASET_CREATE),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasetid = H5Dcreatex(hid,datasetname,
                                               datasettype,
                                               datasetspace,
                                               datasetprop),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Dwrite(datasetid,datasettype,
                                 H5S_ALL,H5S_ALL,
                                 H5P_DEFAULT,
                                 (const void *)datasettext),
                        CBF_ALLOC,errorcode);

        if (datasetprop >= 0)   H5Pclose(datasetprop);

        if (datasetspace >= 0)  H5Sclose(datasetspace);

        if (datasettype >= 0)   H5Tclose(datasettype);

        if (datasetid >= 0)     H5Dclose(datasetid);

        return errorcode;

    }


    /* apply a text attribute to a group or dataset */

    int cbf_apply_h5text_dataset(hid_t hid,
                                   const char* attribname,
                                   const char* attribtext,
                                   int errorcode)
    {

        hid_t attribspace, attribtype, attribid;

        attribspace = attribtype = attribid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !attribname || !attribtext ) return CBF_ARGUMENT;

        cbf_h5reportneg(attribspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribtype = H5Tcopy(H5T_C_S1),CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Tset_size(attribtype,strlen(attribtext)),CBF_ALLOC,errorcode);

        cbf_h5reportneg(attribid = H5Acreatex(hid,attribname,
                                              attribtype,
                                              attribspace,
                                              H5P_DEFAULT),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Awrite(attribid,attribtype,
                                 (const void *)attribtext),CBF_ALLOC,errorcode);

        if (attribspace >= 0)  H5Sclose(attribspace);

        if (attribtype >= 0)   H5Tclose(attribtype);

        if (attribid >= 0)     H5Aclose(attribid);

        return errorcode;

    }

    /* apply a double dataset slab to a group

     places the specified datasetvalue in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.

     The slabs are indexed from 0

     */

    int cbf_add_h5double_dataset_slab(hid_t hid,
                                    const char* datasetname,
                                    const double datasetvalue,
                                    const hsize_t slab,
                                    int errorcode)
    {
        hid_t datasetspace, datasettype, datasetid;

        hid_t memspace, memtype;

        hid_t ndatasettype;

        hid_t ndatasetspace;

        hid_t nmemtype;

        int ndims;

        hsize_t offset[1] = {0};

        hsize_t stride[1] = {1};

        hsize_t count[1]  = {1};

        hsize_t chunk[1] = {1};

        hsize_t curdim[1];

        hsize_t memsize[1] = {1};

        htri_t dsexists;

        hsize_t dssize[1];

        hsize_t maxdssize[1];

        hsize_t dsdims[1];

        hsize_t dsmaxdims[1];

        datasetspace = datasettype = memspace = memtype = CBF_H5FAIL;

        ndatasetspace = ndatasettype = nmemtype = CBF_H5FAIL;

        datasetid = CBF_H5FAIL;

        memsize[0] = 1;

        dssize[0] = 1;

        maxdssize[0] = H5S_UNLIMITED;

        chunk[0] = 1;

        /* ensure arguments all given */

        if (hid < 0 || !datasetname ||
            errorcode) return CBF_ARGUMENT;

        dsexists = H5Lexists(hid,datasetname, H5P_DEFAULT);

        if ( dsexists < 0 ||
            !dsexists
            || (datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT))< 0 ) {

            /* Create the dataset if we were unable to open it */

            cbf_h5reportneg(datasettype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

            cbf_reportnez(cbf_H5Dcreate(hid,&datasetid,datasetname,1,dssize,maxdssize,chunk,datasettype),errorcode);

            curdim[0] = 0;

            dsdims[0] = 0;

        }  else {

            if ( datasetid <= 0 ) {

                datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT);

            }

            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(ndims = H5Sget_simple_extent_ndims(datasetspace),CBF_FORMAT,errorcode);

            if ( errorcode || ndims != 1 ) return CBF_FORMAT;

            cbf_h5reportneg(H5Sget_simple_extent_dims(datasetspace,
                                                      dsdims,dsmaxdims),CBF_FORMAT,errorcode);

            if (datasetspace >=0 )H5Sclose(datasetspace);

            datasetspace = CBF_H5FAIL;


        }

        if (slab == H5S_UNLIMITED) {

            offset[0] = dsdims[0];

            curdim[0] = dsdims[0]+1;

        } else {

            offset[0] = slab;

            curdim[0] = slab+1;

        }

        cbf_h5reportneg(H5Dset_extent(datasetid,curdim),CBF_FORMAT,errorcode);

        stride[0] = 1;

        count[0] = 1;

        cbf_reportnez(cbf_H5Dwrite(datasetid,offset,stride,count,(void *)(&datasetvalue)),errorcode);

        if (datasetspace >= 0)  H5Sclose(datasetspace);

        if (datasettype >= 0)   H5Tclose(datasettype);

        if (datasetid >= 0)     H5Dclose(datasetid);

        if (memtype >= 0)       H5Tclose(memtype);

        return errorcode;

    }

    /* apply a text dataset to a group */

    int cbf_add_h5double_dataset(hid_t hid,
                               const char* datasetname,
                               const double datasetvalue,
                               int errorcode)
    {

        hid_t datasetspace, datasettype, datasetid, datasetprop;

        htri_t dsexists;

        double dsvalue;

        datasetspace = datasettype = datasetid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !datasetname
            || errorcode) return CBF_ARGUMENT;

        dsexists = H5Lexists(hid,datasetname,H5P_DEFAULT);

        if ( dsexists >=0 && dsexists ) {

            hid_t memtype;

            int datasetrank;

            /* The dataset exists, and will be replaced with an array
             of strings, unless it is already an array, in which case
             we just add to it */

            cbf_h5reportneg(datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasetrank = H5Sget_simple_extent_ndims(datasetspace),CBF_H5ERROR,errorcode);

            if (datasetrank == 1) {

                /* This is an array, write the next slab */

                if (datasetspace >= 0)  H5Sclose(datasetspace);

                if (datasettype >= 0)   H5Tclose(datasettype);

                if (datasetid >= 0)     H5Dclose(datasetid);

                cbf_reportnez(cbf_add_h5double_dataset_slab(hid,
                                                          datasetname,
                                                          datasetvalue,
                                                          H5S_UNLIMITED, errorcode),errorcode);


                return errorcode;

            }

            cbf_h5reportneg(memtype=H5Tget_native_type(datasettype,H5T_DIR_ASCEND),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(H5Dread(datasetid, memtype, H5S_ALL, datasetspace,
                                    H5P_DEFAULT, (void *)(&dsvalue)),CBF_H5ERROR,errorcode);

            /* unlink the current dataset and write the old data and the new data */

            cbf_h5reportneg(H5Ldelete(hid,datasetname,H5P_DEFAULT),CBF_H5ERROR,errorcode);

            cbf_reportnez(cbf_add_h5double_dataset_slab(hid,
                                                      datasetname,
                                                      dsvalue,
                                                      0, errorcode),errorcode);

            cbf_reportnez(cbf_add_h5double_dataset_slab(hid,
                                                      datasetname,
                                                      datasetvalue,
                                                      1, errorcode),errorcode);

            if (datasetspace >= 0)  H5Sclose(datasetspace);

            if (datasettype >= 0)   H5Tclose(datasettype);

            if (memtype >= 0)       H5Tclose(memtype);

            if (datasetid >= 0)     H5Dclose(datasetid);


            return errorcode;

        }

        cbf_h5reportneg(datasetspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasettype = H5Tcopy(H5T_IEEE_F64LE),CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasetprop = H5Pcreate(H5P_DATASET_CREATE),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasetid = H5Dcreatex(hid,datasetname,
                                               datasettype,
                                               datasetspace,
                                               datasetprop),
                        CBF_ALLOC,errorcode);

        dsvalue = datasetvalue;

        cbf_h5reportneg(H5Dwrite(datasetid,datasettype,
                                 H5S_ALL,H5S_ALL,
                                 H5P_DEFAULT,
                                 (const void *)(&dsvalue)),
                        CBF_ALLOC,errorcode);

        if (datasetprop >= 0)   H5Pclose(datasetprop);

        if (datasetspace >= 0)  H5Sclose(datasetspace);

        if (datasettype >= 0)   H5Tclose(datasettype);

        if (datasetid >= 0)     H5Dclose(datasetid);

        return errorcode;

    }

    /* apply a long dataset slab to a group

     places the specified datasetvalue in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.

     The slabs are indexed from 0

     */

    int cbf_add_h5long_dataset_slab(hid_t hid,
                                      const char* datasetname,
                                      const long datasetvalue,
                                      const hsize_t slab,
                                      int errorcode)
    {
        hid_t datasetspace, datasettype, datasetid;

        hid_t memspace, memtype;

        hid_t ndatasettype;

        hid_t ndatasetspace;

        hid_t nmemtype;

        int ndims;

        hsize_t offset[1] = {0};

        hsize_t stride[1] = {1};

        hsize_t count[1]  = {1};

        hsize_t chunk[1] = {1};

        hsize_t curdim[1];

        hsize_t memsize[1] = {1};

        htri_t dsexists;

        hsize_t dssize[1];

        hsize_t maxdssize[1];

        hsize_t dsdims[1];

        hsize_t dsmaxdims[1];

        datasetspace = datasettype = memspace = memtype = CBF_H5FAIL;

        ndatasetspace = ndatasettype = nmemtype = CBF_H5FAIL;

        datasetid = CBF_H5FAIL;

        memsize[0] = 1;

        dssize[0] = 1;

        maxdssize[0] = H5S_UNLIMITED;

        chunk[0] = 1;

        /* ensure arguments all given */

        if (hid < 0 || !datasetname ||
             errorcode) return CBF_ARGUMENT;

        dsexists = H5Lexists(hid,datasetname, H5P_DEFAULT);

        if ( dsexists < 0 ||
            !dsexists
            || (datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT))< 0 ) {

            /* Create the dataset if we were unable to open it */

            cbf_h5reportneg(datasettype = H5Tcopy(H5T_STD_I64LE),CBF_ALLOC,errorcode);

            cbf_reportnez(cbf_H5Dcreate(hid,&datasetid,datasetname,1,dssize,maxdssize,chunk,datasettype),errorcode);

            curdim[0] = 0;

            dsdims[0] = 0;

        }  else {

            if ( datasetid <= 0 ) {

                datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT);

            }

            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_FORMAT,errorcode);

            cbf_h5reportneg(ndims = H5Sget_simple_extent_ndims(datasetspace),CBF_FORMAT,errorcode);

            if ( errorcode || ndims != 1 ) return CBF_FORMAT;

            cbf_h5reportneg(H5Sget_simple_extent_dims(datasetspace,
                                                      dsdims,dsmaxdims),CBF_FORMAT,errorcode);

            if (datasetspace >=0 )H5Sclose(datasetspace);

            datasetspace = CBF_H5FAIL;


        }

        if (slab == H5S_UNLIMITED) {

            offset[0] = dsdims[0];

            curdim[0] = dsdims[0]+1;

        } else {

            offset[0] = slab;

            curdim[0] = slab+1;

        }

        cbf_h5reportneg(H5Dset_extent(datasetid,curdim),CBF_FORMAT,errorcode);

        stride[0] = 1;

        count[0] = 1;

        cbf_reportnez(cbf_H5Dwrite(datasetid,offset,stride,count,(void *)(&datasetvalue)),errorcode);

        if (datasetspace >= 0)  H5Sclose(datasetspace);

        if (datasettype >= 0)   H5Tclose(datasettype);

        if (datasetid >= 0)     H5Dclose(datasetid);

        if (memtype >= 0)       H5Tclose(memtype);

        return errorcode;

    }

    /* apply a text dataset to a group */

    int cbf_add_h5long_dataset(hid_t hid,
                                 const char* datasetname,
                                 const long datasetvalue,
                                 int errorcode)
    {

        hid_t datasetspace, datasettype, datasetid, datasetprop;

        htri_t dsexists;

        long dsvalue;

        datasetspace = datasettype = datasetid = CBF_H5FAIL;

        /* ensure arguments all given */

        if (hid < 0 || !datasetname 
            || errorcode) return CBF_ARGUMENT;

        dsexists = H5Lexists(hid,datasetname,H5P_DEFAULT);

        if ( dsexists >=0 && dsexists ) {

            hid_t memtype;

            int datasetrank;

            /* The dataset exists, and will be replaced with an array
             of strings, unless it is already an array, in which case
             we just add to it */

            cbf_h5reportneg(datasetid = H5Dopen2(hid,datasetname, H5P_DEFAULT),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasettype = H5Dget_type(datasetid),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasetspace = H5Dget_space(datasetid),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(datasetrank = H5Sget_simple_extent_ndims(datasetspace),CBF_H5ERROR,errorcode);

            if (datasetrank == 1) {

                /* This is an array, write the next slab */

                if (datasetspace >= 0)  H5Sclose(datasetspace);

                if (datasettype >= 0)   H5Tclose(datasettype);

                if (datasetid >= 0)     H5Dclose(datasetid);

                cbf_reportnez(cbf_add_h5long_dataset_slab(hid,
                                                          datasetname,
                                                          datasetvalue,
                                                          H5S_UNLIMITED, errorcode),errorcode);


                return errorcode;

            }

            cbf_h5reportneg(memtype=H5Tget_native_type(datasettype,H5T_DIR_ASCEND),CBF_H5ERROR,errorcode);

            cbf_h5reportneg(H5Dread(datasetid, memtype, H5S_ALL, datasetspace,
                                    H5P_DEFAULT, (void *)(&dsvalue)),CBF_H5ERROR,errorcode);

            /* unlink the current dataset and write the old data and the new data */

            cbf_h5reportneg(H5Ldelete(hid,datasetname,H5P_DEFAULT),CBF_H5ERROR,errorcode);

            cbf_reportnez(cbf_add_h5double_dataset_slab(hid,
                                                        datasetname,
                                                        dsvalue,
                                                        0, errorcode),errorcode);

            cbf_reportnez(cbf_add_h5double_dataset_slab(hid,
                                                        datasetname,
                                                        datasetvalue,
                                                        1, errorcode),errorcode);

            if (datasetspace >= 0)  H5Sclose(datasetspace);

            if (datasettype >= 0)   H5Tclose(datasettype);

            if (memtype >= 0)       H5Tclose(memtype);

            if (datasetid >= 0)     H5Dclose(datasetid);


            return errorcode;

        }

        cbf_h5reportneg(datasetspace = H5Screate(H5S_SCALAR),CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasettype = H5Tcopy(H5T_STD_I64LE),CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasetprop = H5Pcreate(H5P_DATASET_CREATE),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(datasetid = H5Dcreatex(hid,datasetname,
                                               datasettype,
                                               datasetspace,
                                               datasetprop),
                        CBF_ALLOC,errorcode);

        dsvalue = datasetvalue;

        cbf_h5reportneg(H5Dwrite(datasetid,datasettype,
                                 H5S_ALL,H5S_ALL,
                                 H5P_DEFAULT,
                                 (const void *)(&dsvalue)),
                        CBF_ALLOC,errorcode);

        if (datasetprop >= 0)   H5Pclose(datasetprop);

        if (datasetspace >= 0)  H5Sclose(datasetspace);

        if (datasettype >= 0)   H5Tclose(datasettype);

        if (datasetid >= 0)     H5Dclose(datasetid);

        return errorcode;

    }




    /* Write a binary value to an HDF5 file */

    int cbf_write_h5binary (cbf_handle handle,
                            cbf_node *column,
                            unsigned int row,
                            cbf_h5handle h5handle)
    {
        hid_t valid, valtype, memtype, valprop, valspace;

        int errorcode;

        char rownum[10];

        cbf_file *infile;

        char digest [25];

        long start;

        size_t size;

        hsize_t hsize[3];

        unsigned int compression;

        unsigned char * rawdata;

        void * uncompressedarray;

        int id, bits, sign, type, checked_digest, realarray;

        const char *byteorder;

        size_t dimover, dimfast, dimmid, dimslow;

        size_t padding;

        size_t elsize;

        size_t nelems_read;

        unsigned int cd_values[CBF_H5Z_FILTER_CBF_NELMTS];

        /* Check the arguments */

        if (!handle || !h5handle || !h5handle->hfile)

            return CBF_ARGUMENT;

        if (!cbf_is_binary (column, row))

            return CBF_ARGUMENT;

        if (cbf_is_mimebinary (column, row))

            return CBF_ARGUMENT;

        cbf_failnez (cbf_get_bintext (column, row, &type, &id, &infile,
                                      &start, &size, &checked_digest,
                                      digest, &bits, &sign, &realarray,
                                      &byteorder, &dimover, &dimfast, &dimmid, &dimslow,
                                      &padding, &compression))

        if (dimslow == 0) dimslow = 1;

        if (dimmid == 0) dimmid = 1;

        if (dimfast == 0) dimfast = 1;

        /* Position the file at the start of the binary section */

        cbf_failnez (cbf_set_fileposition (infile, start, SEEK_SET))

        /* Calculate the digest if necessary */

        if (!cbf_is_base64digest (digest))
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

        valid = valtype = valprop = valspace = CBF_H5FAIL;

        sprintf(rownum,"%d", row);

        /* prepare the errorcode */

        errorcode = 0;

        if (h5handle->flags & CBF_H5_OPAQUE) {

            /* Treat the image as an opaque stream of size bytes */

            hsize[0] = size;

            cbf_h5reportneg(valspace = H5Screate_simple(1,hsize,NULL),
                            CBF_ALLOC,errorcode);

            cbf_h5reportneg(valtype = H5Tcreate(H5T_OPAQUE,1),
                            CBF_ALLOC,errorcode);

            cbf_h5reportneg(H5Tset_tag(valtype,"stream of opaque bytes"),
                            CBF_ALLOC,errorcode);


            cbf_h5reportneg(valprop = H5Pcreate(H5P_DATASET_CREATE),
                            CBF_ALLOC,errorcode);

            cbf_h5reportneg(valid = H5Dcreatex(h5handle->colid,rownum,
                                               valtype,valspace,
                                               valprop),
                            CBF_ALLOC,errorcode);


            /* get all the data */

            /* first ensure enough space at infile->characters
             for size characters, which means we need enough
             at characters->base for size + the old data,
             if any */

            cbf_reportnez(cbf_set_io_buffersize(infile, size),errorcode);

            /* now we can safely do the read

             because of the file positioning done, infile->characters
             is actually the same as infile_characters_base for a
             file stream based file */

            rawdata = (unsigned char*) infile->characters;

            if (infile->characters_used < size) {


                /* We cannot get any more characters from
                 a temporary file of file without a stream */

                if (infile->temporary || !infile->stream)  {

                    errorcode |= CBF_FILEREAD;

                }

                if (!errorcode && fread(rawdata, 1,
                                        size-(infile->characters_used), infile->stream)
                    != size-infile->characters_used)
                    errorcode |= CBF_FILEREAD;

                if (!errorcode) infile->characters_used = size;

            }

            cbf_h5reportneg(H5Dwrite(valid,valtype,
                                     valspace,H5S_ALL,H5P_DEFAULT,rawdata),
                            CBF_ARGUMENT,errorcode);

        } else {

            /* Treat the data as an array to be compressed

             chunking in planes dimfast x dimmid

             */

            hsize_t chunk[3];

            hsize_t maxdim[3];

            hsize[0] = dimslow;

            chunk[0] = 1;

            maxdim[0] = H5S_UNLIMITED;

            hsize[1] = chunk[1] = maxdim[1] = dimmid;

            hsize[2] = chunk[2] = maxdim[2] = dimfast;

            cbf_h5reportneg(valspace = H5Screate_simple(3,hsize,maxdim),
                            CBF_ALLOC,errorcode);

            if (realarray) {

                if (byteorder[0]=='l' || byteorder[0]=='L') {

                    if (bits <= 32) {

                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F32LE),
                                        CBF_ALLOC,errorcode);
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_FLOAT),
                                        CBF_ALLOC,errorcode)


                    } else {

                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F64LE),
                                        CBF_ALLOC,errorcode)
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_DOUBLE),
                                        CBF_ALLOC,errorcode)


                    }


                } else {

                    if (bits <= 32) {

                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F32BE),
                                        CBF_ALLOC,errorcode);
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_FLOAT),
                                        CBF_ALLOC,errorcode)

                    } else {

                        cbf_h5reportneg(valtype = H5Tcopy(H5T_IEEE_F64BE),
                                        CBF_ALLOC,errorcode)
                        cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_DOUBLE),
                                        CBF_ALLOC,errorcode)

                    }

                }


            } else {

                if (byteorder[0]=='l' || byteorder[0]=='L') {

                    if (bits <= 8) {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I8LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_CHAR),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U8LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UCHAR),
                                            CBF_ALLOC,errorcode);
                        }

                    } else if (bits <= 16) {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I16LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_SHORT),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U16LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_USHORT),
                                            CBF_ALLOC,errorcode);
                        }

                    } else if (bits <= 32) {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I32LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_INT),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U32LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UINT),
                                            CBF_ALLOC,errorcode);
                        }

                    } else {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I64LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_LLONG),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U64LE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_ULLONG),
                                            CBF_ALLOC,errorcode);
                        }

                    }


                } else {

                    if (bits <= 8) {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I8BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_CHAR),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U8BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UCHAR),
                                            CBF_ALLOC,errorcode);
                        }

                    } else if (bits <= 16) {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I16BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_SHORT),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U16BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_USHORT),
                                            CBF_ALLOC,errorcode);
                        }

                    } else if (bits <= 32) {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I32BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_INT),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U32BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_UINT),
                                            CBF_ALLOC,errorcode);

                        }

                    } else {

                        if (sign) {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_I64BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_LLONG),
                                            CBF_ALLOC,errorcode);

                        } else {

                            cbf_h5reportneg(valtype = H5Tcopy(H5T_STD_U64BE),
                                            CBF_ALLOC,errorcode);
                            cbf_h5reportneg(memtype = H5Tcopy(H5T_NATIVE_ULLONG),
                                            CBF_ALLOC,errorcode);

                        }

                    }

                }

            }


            cbf_h5reportneg(valprop = H5Pcreate(H5P_DATASET_CREATE),
                            CBF_ALLOC,errorcode);

            cbf_h5reportneg(H5Pset_chunk(valprop,3,chunk),
                            CBF_ALLOC,errorcode);

            cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION] = compression;
            cd_values[CBF_H5Z_FILTER_CBF_RESERVED]    = 0;
            cd_values[CBF_H5Z_FILTER_CBF_BINARY_ID]   = id;
            cd_values[CBF_H5Z_FILTER_CBF_PADDING]     = padding;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIZE]      = (bits+7)/8;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIGN]      = sign;
            cd_values[CBF_H5Z_FILTER_CBF_REAL]        = realarray;
            cd_values[CBF_H5Z_FILTER_CBF_DIMFAST]     = dimfast;
            cd_values[CBF_H5Z_FILTER_CBF_DIMMID]      = dimmid;
            cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW]     = dimslow;

            if (h5handle->flags & CBF_H5_REGISTER_COMPRESSIONS) {

                if (!H5Zfilter_avail(CBF_H5Z_FILTER_CBF)) {

                    cbf_h5reportneg(H5Zregister(CBF_H5Z_CBF),CBF_ALLOC,errorcode);

                }
            }

            cbf_h5reportneg(H5Pset_filter(valprop,CBF_H5Z_FILTER_CBF,
                                          H5Z_FLAG_OPTIONAL,
                                          CBF_H5Z_FILTER_CBF_NELMTS,
                                          cd_values),CBF_ALLOC,errorcode);

            /* fprintf(stderr,"errorcode on setting filter CBF_H5Z_FILTER_CBF %d\n",errorcode); */

            valid = H5Dcreatex(h5handle->colid,rownum,
                               valtype,valspace,
                               valprop);


            /* get all the data */

            elsize = (bits+7)/8;

            cbf_reportnez(cbf_alloc(((void **) &uncompressedarray),NULL,
                                    dimover*elsize,1),errorcode);

            nelems_read = 0;

            /* Get the data */

            {
                int text_realarray, text_id;

                size_t text_dimover, text_dimfast, text_dimmid, text_dimslow, text_padding;

                const char *text_byteorder;

                cbf_reportnez (cbf_get_binary (column, row,
                                               &text_id,
                                               uncompressedarray,
                                               elsize,
                                               sign,
                                               dimover,
                                               &nelems_read,
                                               &text_realarray,
                                               &text_byteorder,
                                               &text_dimover,
                                               &text_dimfast,
                                               &text_dimmid,
                                               &text_dimslow,
                                               &text_padding),errorcode);
            }

            if (nelems_read < dimover) {

                cbf_failnez(cbf_free(&uncompressedarray,NULL));

                return errorcode|CBF_ENDOFDATA;

            }

            cbf_h5reportneg(H5Dwrite(valid,memtype,H5S_ALL,
                                     H5S_ALL,H5P_DEFAULT,uncompressedarray),
                            CBF_ARGUMENT,errorcode);

            if (memtype >= 0) {

                cbf_h5failneg(H5Tclose(memtype),CBF_ARGUMENT);

            }

            cbf_failnez(cbf_free(&uncompressedarray,NULL));


        }

        errorcode |= cbf_apply_h5integer_attribute(valid,"signal",row+1,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"compression",compression,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"binid",id,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"bits",bits,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"sign",sign,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"bintype",type,errorcode);
        errorcode |= cbf_apply_h5text_attribute(valid,"cbftype","bnry",errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"checked_digest",checked_digest,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"size",(long)size,errorcode);
        errorcode |= cbf_apply_h5intasstr_attribute(valid,"real",realarray,errorcode);
        errorcode |= cbf_apply_h5text_attribute(valid,"byteorder",byteorder,errorcode);
        if (cbf_is_base64digest (digest)) {
            errorcode |= cbf_apply_h5text_attribute(valid,"MD5_digest",digest,errorcode);
        }
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimover",(long)dimover,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimfast",(long)dimfast,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimmid",(long)dimmid,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"dimslow",(long)dimslow,errorcode);
        errorcode |= cbf_apply_h5longasstr_attribute(valid,"padding",(long)padding,errorcode);


        /* now link the data to entry:NXentry/data:NXdata */

        if (h5handle->dataid<0){

            /* ensure it goes right below NXentry */

            if (h5handle->curnxid>=0) {

                cbf_h5reportneg(H5Gclose(h5handle->curnxid),CBF_ARGUMENT,errorcode);

                h5handle->curnxid = CBF_H5FAIL;

            }

            cbf_reportnez(cbf_H5Gcreate_in_handle(h5handle,"data",&(h5handle->dataid)),errorcode);

            cbf_failnez(cbf_apply_h5text_attribute(h5handle->dataid,
                                                   "NX_class","NXdata",0));

        }

        {
            char target_path[1024];

            char full_name[1024];

            size_t len;

            const char * pstr;

            strcpy(target_path,"/entry/CBF_cbf/");

            full_name[0] = '\0';

            len = strlen(target_path)+strlen(rownum)+1;

            pstr = (h5handle->bookmark).datablock?
            (h5handle->bookmark).datablock:"_(null)_";

            if (len+strlen(pstr)>1020) return CBF_FORMAT;

            strncat(target_path,pstr,1020-len);

            strcat(target_path,"/");

            strncat(full_name,pstr,1020-len);

            strcat(full_name,".");

            len = strlen(target_path)+strlen(rownum)+1;

            pstr = (h5handle->bookmark).category?
            (h5handle->bookmark).category:"_(null)_";

            if (len+strlen(pstr)>1021) return CBF_FORMAT;

            strncat(target_path,pstr,1021-len);

            strcat(target_path,"/");

            strncat(full_name,pstr,1020-len);

            strcat(full_name,".");

            len = strlen(target_path)+strlen(rownum)+1;

            pstr = (h5handle->bookmark).column?
            (h5handle->bookmark).column:"_(null)_";

            if (len+strlen(pstr)>1022) return CBF_FORMAT;

            strncat(target_path,pstr,1022-len);

            strcat(target_path,"/");

            strncat(full_name,pstr,1020-len);

            strcat(full_name,".");

            len = strlen(target_path);

            if (len+strlen(rownum)>1023) return CBF_FORMAT;

            strncat(target_path,rownum,1023-len);

            strncat(full_name,rownum,1023-len);

            errorcode |= cbf_apply_h5text_attribute(valid,"target",
                                                    target_path,errorcode);


            cbf_h5reportneg(H5Lcreate_hard(h5handle->colid,rownum,
                                           h5handle->dataid,full_name,H5P_DEFAULT,H5P_DEFAULT),
                            CBF_ARGUMENT,
                            errorcode);

        }

        if (valid >= 0) {

            cbf_h5failneg(H5Dclose(valid),CBF_ARGUMENT);

        }

        if (valspace >= 0) {

            cbf_h5failneg(H5Sclose(valspace),CBF_ARGUMENT);

        }

        if (valtype >= 0) {

            cbf_h5failneg(H5Tclose(valtype),CBF_ARGUMENT);

        }

        if (valprop >= 0) {

            cbf_h5failneg(H5Pclose(valprop),CBF_ARGUMENT);

        }

        return errorcode;



    }

    /* Write an ascii value to an HDF5 file */

    int cbf_write_h5ascii (cbf_handle handle,
                           unsigned int row,
                           const char *string,
                           cbf_h5handle h5handle)
    {
        static const char missing [] = { CBF_TOKEN_NULL, '?', '\0' };


        hid_t valid, valtype, valprop, valspace;

        char rownum[10];

        char* typecode;

        int errorcode;

        /* Check the arguments */

        if (!handle || !h5handle || h5handle->hfile < 0) return CBF_ARGUMENT;

        valid = valtype = valprop = valspace = CBF_H5FAIL;

        sprintf(rownum,"%d", row);

        if (!string)

            string = missing;

        else

            if (*string != CBF_TOKEN_WORD       &&
                *string != CBF_TOKEN_SQSTRING   &&
                *string != CBF_TOKEN_DQSTRING   &&
                *string != CBF_TOKEN_SCSTRING   &&
                *string != CBF_TOKEN_TSQSTRING  &&
                *string != CBF_TOKEN_TDQSTRING  &&
                *string != CBF_TOKEN_BKTSTRING  &&
                *string != CBF_TOKEN_BRCSTRING  &&
                *string != CBF_TOKEN_PRNSTRING  &&
                *string != CBF_TOKEN_NULL)

                return CBF_ARGUMENT;

        /* prepare the errorcode */

        errorcode = 0;

        /* Create a scalar dataspace */

        cbf_h5reportneg(valspace = H5Screate(H5S_SCALAR),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(valtype = H5Tcopy(H5T_C_S1),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Tset_size(valtype,strlen(string+1)),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(valprop = H5Pcreate(H5P_DATASET_CREATE),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(valid = H5Dcreatex(h5handle->colid,rownum,
                                           valtype,valspace,valprop),
                        CBF_ALLOC,errorcode);

        cbf_h5reportneg(H5Dwrite(valid,valtype,
                                 H5S_ALL,H5S_ALL,H5P_DEFAULT,string+1),
                        CBF_ARGUMENT,errorcode);

        errorcode |= cbf_get_value_type(string,(const char **)&typecode);

        errorcode |= cbf_apply_h5text_attribute(valid,
                                                "cbftype",typecode,errorcode);

        if (valid >= 0) {

            cbf_h5failneg(H5Dclose(valid),CBF_ARGUMENT);

        }

        if (valspace >= 0) {

            cbf_h5failneg(H5Sclose(valspace),CBF_ARGUMENT);

        }

        if (valtype >= 0) {

            cbf_h5failneg(H5Tclose(valtype),CBF_ARGUMENT);

        }

        if (valprop >= 0) {

            cbf_h5failneg(H5Pclose(valprop),CBF_ARGUMENT);

        }

        return errorcode;


    }

    /* Write a value to an HDF5 file */

    int cbf_write_h5value (cbf_handle handle, cbf_node *column, unsigned int row,
                           cbf_h5handle h5handle)
    {
        const char *text;

        /* Check the arguments */

        if (!column || !h5handle || h5handle->hfile < 0)

            return CBF_ARGUMENT;

        if (row >= column->children)

            return CBF_NOTFOUND;


        /* Get the value */

        cbf_failnez (cbf_get_columnrow (&text, column, row))


        /* Missing value? */

        if (!text)

            return cbf_write_h5ascii (handle, row, text, h5handle);


        /* Plain ASCII? */

        cbf_failnez (cbf_value_type ((char *) text))

        if (*text == CBF_TOKEN_WORD     ||
            *text == CBF_TOKEN_SQSTRING ||
            *text == CBF_TOKEN_DQSTRING ||
            *text == CBF_TOKEN_SCSTRING ||
            *text == CBF_TOKEN_TSQSTRING ||
            *text == CBF_TOKEN_TDQSTRING ||
            *text == CBF_TOKEN_PRNSTRING ||
            *text == CBF_TOKEN_BKTSTRING ||
            *text == CBF_TOKEN_BRCSTRING ||
            *text == CBF_TOKEN_NULL)

            return cbf_write_h5ascii (handle, row, text, h5handle);


        /* Plain binary? */

        if (*text == CBF_TOKEN_BIN || *text == CBF_TOKEN_TMP_BIN)

            return cbf_write_h5binary (handle, column, row, h5handle);


        /* Undecoded MIME? */

        if (*text == CBF_TOKEN_MIME_BIN)
        {
            /* Convert the value to a normal binary section */

            cbf_failnez (cbf_mime_temp (column, row))

            return cbf_write_h5binary (handle, column, row, h5handle);
        }


        /* Fail */

        return CBF_ARGUMENT;
    }

	/* Write a CBF value into a NeXus file
     Will add a piece of data with a given name to /entry/group@groupNXclass/subGroup@subGroupNXclass/name */

	int cbf_map_h5value(
                        const char * const name, const char * const value,
                        const char * const group, const char * const groupNXclass,
                        const char * const subGroup, const char * const subGroupNXclass,
                        const size_t attrc, const cbf_name_value_pair * const attrv,
                        cbf_h5handle h5handle)
	{
		/* check args & define local variables */
		int error = (attrc != 0 && !attrv) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t h5group = CBF_H5FAIL, h5subGroup = CBF_H5FAIL, h5dataset = CBF_H5FAIL, h5type = CBF_H5FAIL;
		const cbf_name_value_pair * attrit = attrv;

		/* Get the datatype */
		cbf_reportnez(cbf_H5Tcreate_string(&h5type, strlen(value)), error);

		/* Assume nxid is a valid handle, need to ensure that the /entry/instrument group exists, then ensure the detector group exists */
		cbf_reportnez(cbf_require_nxgroup(h5handle, group, groupNXclass, h5handle->nxid, &h5group), error);
		cbf_reportnez(cbf_require_nxgroup(h5handle, subGroup, subGroupNXclass, h5group, &h5subGroup), error);

		/* write some data */
		cbf_reportnez(cbf_H5Dcreate(h5subGroup,&h5dataset,name,0,0,0,0,h5type),error);
		cbf_reportnez(cbf_H5Dwrite(h5dataset,0,0,0,value),error);
		for (; attrit < attrv+attrc; ++attrit)
			cbf_reportnez(cbf_H5Arequire_string(h5dataset,attrit->name,attrit->value),error);

		/*  clean up */
		if (cbf_H5Ivalid(h5type)) cbf_H5Tfree(h5type);
		if (cbf_H5Ivalid(h5dataset)) cbf_H5Dfree(h5dataset);
		if (cbf_H5Ivalid(h5subGroup)) cbf_H5Gfree(h5subGroup);
		if (cbf_H5Ivalid(h5group)) cbf_H5Gfree(h5group);
		return error;
	}


    /* Write a category to an HDF5 file */

    int cbf_write_h5category (cbf_handle handle,
                              const cbf_node *category,
                              cbf_h5handle h5handle)
    {
		unsigned int column, row;

		const char instGroup[] = "instrument";

		const char instGroupClass[] = "NXinstrument";

        hid_t instrumentid;

        unsigned int colrow;

        cbf_fast_bookmark fbkmk;

        int errorcode;

        errorcode = 0;

        /* Check the arguments */

        if (!category || !h5handle || h5handle->rootid <0 || h5handle->dbid < 0)

            return CBF_ARGUMENT;

        /* bookmark the handle */

        cbf_failnez(cbf_get_fast_bookmark(handle,&fbkmk));


        /* If another category is open, close it */


        if (h5handle->colid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_FORMAT);

            h5handle->colid = CBF_H5FAIL;

        }

        if (h5handle->catid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->catid),CBF_FORMAT);

            h5handle->catid = CBF_H5FAIL;

        }

        /* save the category name in the read bookmark */

        (h5handle->bookmark).category = category->name;

        /* Write the name under the open save frame or datablock */

        if (h5handle->sfid <0) {

            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                                                     (category->name)?(category->name):"_(null)_"),
                          CBF_FORMAT);


        } else {
            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                                                     (category->name)?(category->name):"_(null)_"),
                          CBF_FORMAT);
        }

		cbf_debug_print(category->name);

        handle->node = (cbf_node *)category;

        handle->row = 0;

		
        /* Process diffrn_detector category */

        if (!cbf_cistrcmp(category->name,"diffrn_detector"))
		{
            unsigned int maxrows;
            unsigned int row;
            int dnamecol;
            const char * dname;
            const char * value;
            char * fullname;
            int error = 0;
            hid_t detgpid;

            maxrows = 0;
            dnamecol = -1;
            dname = NULL;
            detgpid = CBF_H5FAIL;

            for (column= 0; column < category->children; column++) {
                if ((category->child[column])->children > maxrows)
                    maxrows=(category->child[column])->children;
                if (cbf_cistrcmp((category->child[column])->name,"id")==0)
                    dnamecol = column;
            }

            for (row = 0; row < maxrows; row++) {

                /*
                 _diffrn_detector.id DETECTORNAME -->
                 /instrument:NXinstrument/CBF_diffrn_detector__DETECTORNAME:NXdetector
                 */

                if (dnamecol==-1 || row >=  (category->child[dnamecol])->children) {

                    dname = "detector";

                } else {

                    handle->node = category->child[dnamecol];

                    handle->row = row;

                    cbf_reportnez(cbf_get_value(handle,&dname),error)


                }

                if (cbf_cistrcmp(dname,"detector")==0) {

                    fullname = (char *)dname;

                } else {

                    cbf_onfailnez(cbf_strcat("CBF_diffrn_detector__",dname,&fullname),
                                  cbf_goto_fast_bookmark(handle,fbkmk))
                }

                cbf_reportnez(cbf_require_nxgroup(h5handle,
                                                  instGroup,
                                                  instGroupClass,
                                                  h5handle->nxid,
                                                  &instrumentid),error);

                cbf_reportnez(cbf_H5NXGrequire_in_handle(h5handle,
                                                         fullname,
                                                         "NXdetector",
                                                         &detgpid,
                                                         "detector"),error);

                if (fullname != dname) {

                    cbf_reportnez(cbf_free((void **)&fullname,NULL),error);

                }


                for (column= 0; column < category->children; column++) {

                    const char * mappedname;

                    if (cbf_cistrcmp((category->child[column])->name,"type")==0){

                        mappedname = "description";

                    } else if (cbf_cistrcmp((category->child[column])->name,"number_of_axes")==0) {

                        mappedname = (category->child[column])->name;

                    } else if (cbf_cistrcmp((category->child[column])->name,"dtime")==0) {

                        mappedname = "deadtime";

                    } else if (cbf_cistrcmp((category->child[column])->name,"detector")==0) {

                        mappedname = "type";

                    } else if (cbf_cistrcmp((category->child[column])->name,"detector")==0) {

                        mappedname = "type";

                    } else if (column == dnamecol) {

                        continue;

                    } else {

                        mappedname = (category->child[column])->name;

                    }

                    /* no duplicate rows are allowed because each detector
                       name must be unique */

                    if ( H5Lexists(detgpid,mappedname,H5P_DEFAULT)==1) {

                        cbf_onfailnez(CBF_FORMAT,
                        cbf_goto_fast_bookmark(handle,fbkmk));

                    }

                    handle->node = category->child[column];

                    handle->row = row;

                    if (!cbf_get_value(handle,&value) && value) {

                        cbf_onfailnez(error|cbf_add_h5text_dataset(detgpid,
                                                                     mappedname,
                                                                     value,error),
                                      cbf_goto_fast_bookmark(handle,fbkmk));

                    } else {

                        cbf_onfailnez(error|cbf_add_h5text_dataset(detgpid,
                                                                     mappedname,
                                                                     ".",error),
                                      cbf_goto_fast_bookmark(handle,fbkmk));
                    }

                }

            }

            if (detgpid >= 0) H5Gclose(detgpid);

        }


        /* Process diffrn_detector_element category */

		if (!cbf_cistrcmp(category->name,"diffrn_detector_element"))
		{
            unsigned int maxrows;
            unsigned int row;
            int dnamecol, delnamecol;
            const char * dname;
            char * fullname;
            const char * value;
            double doublevalue;
            int error = 0;
            int numeric;
            hid_t detgpid;

            maxrows = 0;
            dnamecol = -1;
            delnamecol = -1;
            numeric = 0;

            dname = NULL;
            delnamecol = -1;
            detgpid = CBF_H5FAIL;

            /* find maximum number of rows, detector id column
               and detector element id column */

            for (column= 0; column < category->children; column++) {
                if ((category->child[column])->children > maxrows)
                    maxrows=(category->child[column])->children;
                if (cbf_cistrcmp((category->child[column])->name,"id")==0)
                    delnamecol = column;
                if (cbf_cistrcmp((category->child[column])->name,"detector_id")==0)
                    dnamecol = column;

            }

            for (row = 0; row < maxrows; row++) {

                int multi_element;

                unsigned int sow;

                const char * ename;

                multi_element = 0;

                /*
                 _diffrn_detector.id DETECTORNAME -->
                 /instrument:NXinstrument/CBF_diffrn_detector__DETECTORNAME:NXdetector
                 */

                if (dnamecol==-1 || row >=  (category->child[dnamecol])->children) {

                    dname = "detector";

                } else {

                    handle->node = category->child[dnamecol];

                    handle->row = row;

                    cbf_reportnez(cbf_get_value(handle,&dname),error);

                    if (!dname) dname = "detector";

                }

                if (cbf_cistrcmp(dname,"detector")==0) {

                    fullname = (char *)dname;

                } else {

                    cbf_failnez(cbf_strcat("CBF_diffrn_detector__",dname,&fullname))
                }

                cbf_reportnez(cbf_require_nxgroup(h5handle,
                                                  instGroup,
                                                  instGroupClass,
                                                  h5handle->nxid,
                                                  &instrumentid),error);

                cbf_reportnez(cbf_H5NXGrequire_in_handle(h5handle,
                                                         fullname,
                                                         "NXdetector",
                                                         &detgpid,
                                                         "detector"),error);

                if (fullname != dname) {

                    cbf_reportnez(cbf_free((void **)&fullname,NULL),error);

                }

                /* check for the multielement case

                 if there is another row with the same detector name,
                 this will be a multielement case to be written by slabs

                 */

                if (maxrows > 1 && dnamecol == -1) {

                    multi_element = 1;

                } else {

                    for (sow = 0; sow < maxrows; sow++ ) {

                        if (sow == row) continue;

                        handle->node = category->child[dnamecol];

                        handle->row = sow;

                        cbf_reportnez(cbf_get_value(handle,&ename),error);

                        if ( (!ename && !cbf_cistrcmp(dname,"detector") )
                            || (ename && !cbf_cistrcmp(ename,dname)) ) {

                            multi_element = 1;

                            break;

                        }

                    }

                }

                for (column= 0; column < category->children; column++) {

                    const char * mappedname;

                    const char * units;

                    if (cbf_cistrcmp((category->child[column])->name,"reference_center_fast")==0){

                        mappedname = "CBF_diffrn_detector_element__reference_center_fast";

                        numeric = 1;

                        units = NULL;

                    } else if (cbf_cistrcmp((category->child[column])->name,"reference_center_slow")==0) {

                        mappedname = "CBF_diffrn_detector_element__reference_center_slow";

                        numeric = 1;

                        units = NULL;

                    } else if (cbf_cistrcmp((category->child[column])->name,"reference_center_units")==0) {

                        mappedname = "CBF_diffrn_detector_element__reference_center_units";

                        numeric = 0;

                        units = NULL;

                    } else if (column==delnamecol) {

                        mappedname = "CBF_diffrn_detector_element__id";

                        numeric = 0;

                        units = NULL;

                    } else if (cbf_cistrcmp((category->child[column])->name,"center[1]")==0){

                        mappedname = "beam_center_x";

                        numeric = 1;

                        units = "mm";

                    } else if (cbf_cistrcmp((category->child[column])->name,"center[2]")==0){

                        mappedname = "beam_center_y";

                        numeric = 1;

                        units = "mm";

                    } else if (column == dnamecol ) {

                        continue;

                    } else {

                        /* until there is support for variants, skip remaining columns */

                        continue;

                    }

                    handle->node = category->child[column];

                    handle->row = row;

                    if (!numeric) {

                        if (!cbf_get_value(handle,&value) && value) {

                            if ( multi_element ) {

                                cbf_onfailnez(error|cbf_add_h5text_dataset_slab (detgpid,
                                                                                 mappedname,
                                                                                 value,
                                                                                 H5S_UNLIMITED,
                                                                                 error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            } else {

                                cbf_onfailnez(error|cbf_add_h5text_dataset(detgpid,
                                                                           mappedname,
                                                                           value,error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            }

                        } else {

                            if ( multi_element ) {

                                cbf_onfailnez(error|cbf_add_h5text_dataset_slab (detgpid,
                                                                                 mappedname,
                                                                                 ".",
                                                                                 H5S_UNLIMITED,
                                                                                 error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            } else {

                                cbf_onfailnez(error|cbf_add_h5text_dataset(detgpid,
                                                                           mappedname,
                                                                           ".",error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            }
                        }

                    } else {

                        if (!cbf_get_doublevalue(handle,&doublevalue) ) {

                            if ( multi_element ) {

                                cbf_onfailnez(error|cbf_add_h5double_dataset_slab (detgpid,
                                                                                   mappedname,
                                                                                   doublevalue,
                                                                                   H5S_UNLIMITED,
                                                                                   error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            } else {

                                cbf_onfailnez(error|cbf_add_h5double_dataset(detgpid,
                                                                             mappedname,
                                                                             doublevalue,error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            }

                        } else {

                            if ( multi_element ) {

                                cbf_onfailnez(error|cbf_add_h5double_dataset_slab (detgpid,
                                                                                   mappedname,
                                                                                   0.e0,
                                                                                   H5S_UNLIMITED,
                                                                                   error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            } else {

                                cbf_onfailnez(error|cbf_add_h5double_dataset(detgpid,
                                                                             mappedname,
                                                                             0.e0,error),
                                              cbf_goto_fast_bookmark(handle,fbkmk));

                            }
                        }

                        if (units) {

                            hid_t datasetid=CBF_H5FAIL;

                            cbf_h5reportneg(datasetid = H5Dopen(detgpid,mappedname,H5P_DEFAULT),CBF_H5ERROR, error);

                            cbf_failnez(cbf_apply_h5text_attribute(h5handle->colid,
                                                                   "units",units,error));

                            if (cbf_H5Ivalid(datasetid)) H5Dclose(datasetid);

                        }

                    }

                }

            }

            if (detgpid >= 0) H5Gclose(detgpid);

        }
/**** Check for closing instrumentid ****/

        /* Process diffrn_radiation category */

        if (!cbf_cistrcmp(category->name,"diffrn_radiation"))
		{
            unsigned int maxrows;
            unsigned int row;
            const char * dname;
            char * fullname;
            const char * value;
            double doublevalue;
            hid_t monochromid;
            int error = 0;
            int numeric;

            maxrows = 0;
            numeric = 0;

            /* find maximum number of rows, detector id column
             and detector element id column */

            for (column= 0; column < category->children; column++) {
                if ((category->child[column])->children > maxrows)
                    maxrows=(category->child[column])->children;
            }

            cbf_reportnez(cbf_require_nxgroup(h5handle,
                                              instGroup,
                                              instGroupClass,
                                              h5handle->nxid,
                                              &instrumentid),error);

            cbf_reportnez(cbf_H5NXGrequire_in_handle(h5handle,
                                                     "monochromator",
                                                     "NXmonochromator",
                                                     &monochromid,
                                                     NULL),error);

            for (row = 0; row < maxrows; row++) {

                int multi_element;

                unsigned int sow;

                const char * ename;

                multi_element = 0;


                  /* check for the multielement case

                 if there is another row with the same detector name,
                 this will be a multielement case to be written by slabs

                 */

                for (column= 0; column < category->children; column++) {

                    const char * mappedname;

                    const char * units;

                    if (cbf_cistrcmp((category->child[column])->name,"wavelength")==0){

                        mappedname = "wavelength";

                        numeric = 1;

                        units = "Angstroms";

                    } else if (cbf_cistrcmp((category->child[column])->name,"reference_center_slow")==0) {

                        mappedname = "CBF_diffrn_detector_element__reference_center_slow";

                        numeric = 1;

                        units = NULL;

                    } else if (cbf_cistrcmp((category->child[column])->name,"reference_center_units")==0) {

                        mappedname = "CBF_diffrn_detector_element__reference_center_units";

                        numeric = 0;

                        units = NULL;


                    } else if (cbf_cistrcmp((category->child[column])->name,"center[1]")==0){

                        mappedname = "beam_center_x";

                        numeric = 1;

                        units = "mm";

                    } else if (cbf_cistrcmp((category->child[column])->name,"center[2]")==0){

                        mappedname = "beam_center_y";

                        numeric = 1;

                        units = "mm";

                    } else {

                        /* until there is support for variants, skip remaining columns */

                        continue;

                    }

                    handle->node = category->child[column];

                    handle->row = row;

                    if (!numeric) {

                        if (!cbf_get_value(handle,&value) && value) {

                            if ( multi_element ) {

                                /* cbf_onfailnez(error|cbf_add_h5text_dataset_slab (detgpid,
                                                                                 mappedname,
                                                                                 value,
                                                                                 H5S_UNLIMITED,
                                                                                 error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            } else {

                                /* cbf_onfailnez(error|cbf_add_h5text_dataset(detgpid,
                                                                           mappedname,
                                                                           value,error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            }

                        } else {

                            if ( multi_element ) {

                                /* cbf_onfailnez(error|cbf_add_h5text_dataset_slab (detgpid,
                                                                                 mappedname,
                                                                                 ".",
                                                                                 H5S_UNLIMITED,
                                                                                 error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            } else {

                               /*  cbf_onfailnez(error|cbf_add_h5text_dataset(detgpid,
                                                                           mappedname,
                                                                           ".",error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            }
                        }

                    } else {

                        if (!cbf_get_doublevalue(handle,&doublevalue) ) {

                            if ( multi_element ) {

                                /* cbf_onfailnez(error|cbf_add_h5double_dataset_slab (detgpid,
                                                                                   mappedname,
                                                                                   doublevalue,
                                                                                   H5S_UNLIMITED,
                                                                                   error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            } else {

                                /* cbf_onfailnez(error|cbf_add_h5double_dataset(detgpid,
                                                                             mappedname,
                                                                             doublevalue,error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            }

                        } else {

                            if ( multi_element ) {

                                /* cbf_onfailnez(error|cbf_add_h5double_dataset_slab (detgpid,
                                                                                   mappedname,
                                                                                   0.e0,
                                                                                   H5S_UNLIMITED,
                                                                                   error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            } else {

                                /* cbf_onfailnez(error|cbf_add_h5double_dataset(detgpid,
                                                                             mappedname,
                                                                             0.e0,error),
                                              cbf_goto_fast_bookmark(handle,fbkmk)); */

                            }
                        }

                        /* if (units) {

                            hid_t datasetid=CBF_H5FAIL;

                            cbf_h5reportneg(datasetid = H5Dopen(detgpid,mappedname,H5P_DEFAULT),CBF_H5ERROR, error);

                            cbf_failnez(cbf_apply_h5text_attribute(h5handle->colid,
                                                                   "units",units,error));

                            if (cbf_H5Ivalid(datasetid)) H5Dclose(datasetid);

                        } */

                    }

                }

            }

            /* if (detgpid >= 0) H5Gclose(detgpid); */

        }


		if (!cbf_cistrcmp(category->name,"diffrn_radiation"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) fprintf(stderr, "CBFlib: warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!cbf_cistrcmp(column_node->name,"div_x_source")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("divergence_x", text+1, instGroup, instGroupClass, "collimator", "NXcollimator", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation.div_x_source could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"div_y_source")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("divergence_y", text+1, instGroup, instGroupClass, "collimator", "NXcollimator", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation.div_y_source could not be written to nexus file");
				}
			}
		}
		if (!cbf_cistrcmp(category->name,"diffrn_radiation_wavelength"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) fprintf(stderr, "CBFlib:  warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!cbf_cistrcmp(column_node->name,"wavelength")){
					cbf_name_value_pair attr = {"units","Angstroms"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("wavelength", text+1, instGroup, instGroupClass, "monochromator", "NXmonochromator", 1,
                                                  &attr, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation_wavelength.wavelength could not be written to nexus file");
				}
				if(!cbf_cistrcmp(column_node->name,"wt")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("weight", text+1, instGroup, instGroupClass, "monochromator", "NXmonochromator", 0, 0,
                                                  h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation_wavelength.wt could not be written to nexus file");
				}
			}
		}


        cbf_failnez(cbf_apply_h5text_attribute(h5handle->catid,
                                               "NX_class","CBF_cbfcat",errorcode));


        /* now, for each column, make it into a group and
         store each row as a dataset */


        for (column= 0; column < category->children; column++)
        {
            /* save the column name in the read bookmark */

            (h5handle->bookmark).column = (category->child[column])->name;


            cbf_h5failneg(h5handle->colid=H5Gcreatex(h5handle->catid,
                                                     (category->child[column])->name?
                                                     (category->child[column])->name:"_(null)_"),
                          CBF_FORMAT);

            cbf_failnez(cbf_apply_h5text_attribute(h5handle->colid,
                                                   "NX_class","CBF_cbfcol",errorcode));

            /* For each row, create a dataset */

            for (row=0; row < category->child [column]->children; row++)
            {

                (h5handle->bookmark).row = row;

                cbf_failnez(cbf_write_h5value(handle,
                                              category->child [column],
                                              row,h5handle));
            }

            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_ARGUMENT);

            h5handle->colid = CBF_H5FAIL;
        }


        if (!cbf_cistrcmp(category->name,"diffrn_scan")) {

            cbf_node * column_node;

            const char * text = 0;

            for (column= 0; column < category->children; column++) {

                column_node = category->child[column];

                if(!cbf_cistrcmp(column_node->name,"id")){

                    if (column_node->children > 1 ) {

                        for (colrow=0; colrow < column_node->children; colrow++) {

                            if(cbf_get_columnrow (&text, column_node, colrow)) {

                                text = " .";

                            }

                            if (cbf_add_h5text_dataset_slab(h5handle->nxid,
                                                              "CBF_scan_id",
                                                              text+1,
                                                              colrow,errorcode)) break;

                        }

                    } else {


                        if(cbf_get_columnrow (&text, column_node, 0)) break;

                        if (!text) break;

                        if(cbf_add_h5text_dataset(h5handle->nxid,
                                                    "CBF_scan_id",text+1,errorcode)) break;

                        break;

                    }

                }
            }
        }

        if (!cbf_cistrcmp(category->name,"entry")) {

            cbf_node * column_node;

            const char * text = 0;

            for (column= 0; column < category->children; column++) {

                column_node = category->child[column];

                if(!cbf_cistrcmp(column_node->name,"id")){

                    if (column_node->children > 1 ) {


                        for (colrow=0; colrow < column_node->children; colrow++) {

                            if(cbf_get_columnrow (&text, column_node, colrow)) {

                                text = " .";

                            }

                            if (cbf_add_h5text_dataset_slab(h5handle->nxid,
                                                              "CBF_entry_id",
                                                              text+1,
                                                              colrow,errorcode)) break;

                        }

                    } else {


                        if(cbf_get_columnrow (&text, column_node, 0)) break;

                        if (!text) break;

                        if(cbf_add_h5text_dataset(h5handle->nxid,
                                                    "CBF_entry_id",text+1,errorcode)) break;

                        break;

                    }

                }
            }
        }

        if (!cbf_cistrcmp(category->name,"diffrn")) {

            cbf_node * column_node;

            const char * text = 0;

            for (column= 0; column < category->children; column++) {

                column_node = category->child[column];

                if(!cbf_cistrcmp(column_node->name,"id")){

                    if (column_node->children > 1 ) {

                        for (colrow=0; colrow < column_node->children; colrow++) {

                            if(cbf_get_columnrow (&text, column_node, colrow)) {

                                text = " .";

                            }

                            if (cbf_add_h5text_dataset_slab(h5handle->nxid,
                                                              "CBF_diffrn_id",
                                                              text+1,
                                                              colrow, errorcode)) break;

                        }

                    } else {


                        if(cbf_get_columnrow (&text, column_node, 0)) break;

                        if (!text) break;

                        if(cbf_add_h5text_dataset(h5handle->nxid,
                                                    "CBF_diffrn_id",text+1,errorcode)) break;

                        break;

                    }

                }
            }
        }

        cbf_failnez(cbf_goto_fast_bookmark(handle,fbkmk));

        /* Success */

        return CBF_SUCCESS;

    }



    /*  create top-level NXentry */

    int cbf_create_NXentry(cbf_h5handle h5handle)
    {

        if (!h5handle ||
            h5handle->nxid >= 0 ||
            h5handle->hfile < 0) return CBF_ARGUMENT;

        cbf_h5failneg(h5handle->nxid=H5Gcreatex(h5handle->hfile,
                                                (const char *)"entry"),
                      CBF_ARGUMENT);


        cbf_failnez(cbf_apply_h5text_attribute(h5handle->nxid,
                                               "NX_class","NXentry",0));

        h5handle->curnxid=CBF_H5FAIL;

        h5handle->dataid =CBF_H5FAIL;

        return CBF_SUCCESS;

    }


    /*  get the current hdf5 group from the handle */

    int cbf_get_H5groupid(const cbf_h5handle h5handle,
                          hid_t * curnxid) {

        if (!h5handle || !curnxid) return CBF_ARGUMENT;

        if (h5handle->nxid < 0) {

            cbf_failnez(cbf_create_NXentry(h5handle));

        }

        *curnxid = (h5handle->curnxid >= 0)? h5handle->curnxid: h5handle->nxid;

        return CBF_SUCCESS;


    }

    /*  Create an HDF5 Group below NX entry or below curnxid */

    int cbf_H5Gcreate_in_handle(cbf_h5handle h5handle,
                                const char * groupname,
                                hid_t * newgroup)
    {
        hid_t parent;
        
        if (!h5handle) return CBF_ARGUMENT;
        
        if (h5handle->nxid < 0) {
            
            cbf_failnez(cbf_create_NXentry(h5handle));
            
        }

        parent = (h5handle->curnxid >= 0)? h5handle->curnxid: h5handle->nxid;

        cbf_h5failneg(*newgroup=H5Gcreatex(parent,groupname),
                      CBF_FORMAT);

        return CBF_SUCCESS;

    }

    /*  Create an HDF5 NeXus Group below NX entry or below curnxid */

    int cbf_H5NXGcreate(cbf_h5handle h5handle,
                        const char * groupname,
                        const char * nxclass,
                        hid_t * newgroup )
    {
        cbf_failnez(cbf_H5Gcreate_in_handle(h5handle, groupname, newgroup));
        
        cbf_failnez(cbf_apply_h5text_attribute(*newgroup,
                                               "NX_class",nxclass,0));
        
        return CBF_SUCCESS;

    }

    /* Require a group below NXentry or below curnxid
       groupname     -- the final path to be used
       group_id      -- optional pointer to the newly opened group
                        if NULL, no return and the group will be
                        closed
       oldgroupname  -- an optional existing path name or NULL
                  to be renamed to new_name if old_name exists
                  and if groupname does not yet exist

     returns 0 for success
     */
    int cbf_H5Grequire_in_handle(const cbf_h5handle h5handle,
                                 const char * groupname,
                                 hid_t * group_id,
                                 const char * oldgroupname) {

        htri_t gpexists;

        hid_t child_id;

        hid_t parent_id;

        /* Check arguments */

        if (!h5handle || ! groupname) return CBF_ARGUMENT;

        /*  Get (or create) the parent_id */

        cbf_failnez(cbf_get_H5groupid(h5handle,&parent_id));

        /* see if the new name is already there */

        gpexists = H5Lexists(parent_id, groupname, H5P_DEFAULT);

        if (gpexists < 0 || !gpexists ) {

            /* see if the old name is there */

            if (oldgroupname) {

                gpexists = H5Lexists(parent_id, oldgroupname, H5P_DEFAULT);

                if (gpexists < 0 || !gpexists ) {

                    /* Neither path exists, just create the new group */

                    cbf_failnez(cbf_H5Gcreate_in_handle(h5handle,
                                                        groupname,
                                                        group_id));

                    return CBF_SUCCESS;

                } else {

                    /* the old path exists, rename it */

                    cbf_h5failneg(child_id=H5Gopenx(parent_id,oldgroupname),
                                  CBF_FORMAT);

                    cbf_h5failneg(H5Ldelete(parent_id,oldgroupname,H5P_DEFAULT),CBF_FORMAT);

                    if (H5Olink(child_id,parent_id,groupname,
                                H5P_DEFAULT,H5P_DEFAULT)< 0) {

                        H5Olink(child_id,parent_id,oldgroupname,
                                H5P_DEFAULT,H5P_DEFAULT);

                        H5Gclose(child_id);

                        if (*group_id) *group_id = CBF_H5FAIL;

                        return CBF_FORMAT;

                    }

                    if (group_id) {

                        *group_id = child_id;

                    } else {

                        cbf_h5failneg(H5Gclose(child_id),CBF_FORMAT);

                    }

                }

            } else {

                /* create the new group */

                cbf_failnez(cbf_H5Gcreate_in_handle(h5handle,
                                                    groupname,
                                                    group_id));

            }

            return CBF_SUCCESS;

        }

        if (group_id) {

            cbf_h5failneg(*group_id=H5Gopenx(parent_id,groupname),
                          CBF_FORMAT);

            if (*group_id < 0) return CBF_FORMAT;

        }

        return CBF_SUCCESS;

    }

    /* Require NeXus group below NXentry or below curnxid
     groupname     -- the final path to be used
     group_id      -- optional pointer to the newly opened group
     if NULL, no return and the group will be
     closed
     oldgroupname  -- an optional existing path name or NULL
     to be renamed to new_name if old_name exists
     and if groupname does not yet exist

     returns 0 for success
     */
    int cbf_H5NXGrequire_in_handle(const cbf_h5handle h5handle,
                                 const char * groupname,
                                 const char * nxclass,
                                 hid_t * group_id,
                                 const char * oldgroupname) {

        hid_t child_id;

        htri_t aexists;

        if (!h5handle || !groupname || !nxclass) return CBF_ARGUMENT;

        cbf_failnez(cbf_H5Grequire_in_handle(h5handle,
                                             groupname,
                                             &child_id,
                                             oldgroupname));

        aexists = H5Aexists(child_id,"NX_class");

        if (aexists < 0 || !aexists ) {

            cbf_failnez(cbf_apply_h5text_attribute(child_id,
                                               "NX_class",nxclass,0));

        }

        if (group_id) {

            *group_id = child_id;

        } else {

            cbf_h5failneg(H5Gclose(child_id),CBF_FORMAT);

        }

        return CBF_SUCCESS;


    }


    /* Free an H5File handle */
    int cbf_free_h5handle(cbf_h5handle h5handle)
	{
		int error = CBF_SUCCESS;
        void * memblock = (void *) h5handle;

		if (cbf_H5Ivalid(h5handle->colid)) {
			CBF_H5CALL(H5Gclose(h5handle->colid));
        }

		if (cbf_H5Ivalid(h5handle->catid)) {
			CBF_H5CALL(H5Gclose(h5handle->catid));
        }

		if (cbf_H5Ivalid(h5handle->sfid)) {
			CBF_H5CALL(H5Gclose(h5handle->sfid));
        }

		if (cbf_H5Ivalid(h5handle->dbid)) {
			CBF_H5CALL(H5Gclose(h5handle->dbid));
        }

		if (cbf_H5Ivalid(h5handle->rootid)) {
			CBF_H5CALL(H5Gclose(h5handle->rootid));
        }

		if (cbf_H5Ivalid(h5handle->curnxid)) {
			CBF_H5CALL(H5Gclose(h5handle->curnxid));
        }

		if (cbf_H5Ivalid(h5handle->dataid)) {
			CBF_H5CALL(H5Gclose(h5handle->dataid));
        }

		if (cbf_H5Ivalid(h5handle->nxid)) {
			CBF_H5CALL(H5Gclose(h5handle->nxid));
		}

		if (cbf_H5Ivalid(h5handle->nxdata)) {
			CBF_H5CALL(H5Gclose(h5handle->nxdata));
		}

		if (cbf_H5Ivalid(h5handle->nxinst)) {
			CBF_H5CALL(H5Gclose(h5handle->nxinst));
		}

		if (cbf_H5Ivalid(h5handle->nxsample)) {
			CBF_H5CALL(H5Gclose(h5handle->nxsample));
		}

		if (cbf_H5Ivalid(h5handle->nxdetector)) {
			CBF_H5CALL(H5Gclose(h5handle->nxdetector));
		}

		if (cbf_H5Ivalid(h5handle->nxmonochromator)) {
			CBF_H5CALL(H5Gclose(h5handle->nxmonochromator));
		}

		if (cbf_H5Ivalid(h5handle->hfile)) {
			CBF_H5CALL(H5Fclose(h5handle->hfile));
        }

		free((void*)h5handle->nxid_name);
		free((void*)h5handle->nxdetector_name);

        return error | cbf_free(&memblock,NULL);
    }

    /* Make an (empty) H5File handle */

    int cbf_make_h5handle(cbf_h5handle *h5handle) {

        cbf_failnez (cbf_alloc ((void **) h5handle, NULL,
                                sizeof(cbf_h5handle_struct), 1));

        (*h5handle)->hfile   = (hid_t)CBF_H5FAIL;
        (*h5handle)->rootid  = (hid_t)CBF_H5FAIL;
        (*h5handle)->dbid    = (hid_t)CBF_H5FAIL;
        (*h5handle)->sfid    = (hid_t)CBF_H5FAIL;
        (*h5handle)->catid   = (hid_t)CBF_H5FAIL;
		(*h5handle)->colid   = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxid    = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxdata  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxinst  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxsample  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxdetector  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxmonochromator = (hid_t)CBF_H5FAIL;
        (*h5handle)->curnxid = (hid_t)CBF_H5FAIL;
        (*h5handle)->dataid  = (hid_t)CBF_H5FAIL;
		(*h5handle)->nxid_name = NULL;
		(*h5handle)->nxdetector_name = NULL;
        (*h5handle)->rwmode  = 0;
        (*h5handle)->flags = 0;
#ifdef CBF_USE_ULP
		(*h5handle)->cmp_double_as_float = 0;
		(*h5handle)->float_ulp = 0;
#ifndef NO_UINT64_TYPE
		(*h5handle)->double_ulp = 0;
#endif
#endif
        return CBF_SUCCESS;

    }

    /* Close the current saveframe in an HDF5 file */

    int cbf_close_h5saveframe (cbf_h5handle h5handle)
    {

        /* Does the node exist? */

        if (!h5handle || h5handle->rootid <0 || h5handle->dbid <0)

            return CBF_ARGUMENT;

        if (h5handle->colid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_FORMAT);

            h5handle->colid = (hid_t)-1;

        }

        if (h5handle->catid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->catid),CBF_FORMAT);

            h5handle->catid = (hid_t)-1;

        }

        if (h5handle->sfid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->sfid),CBF_FORMAT);

            h5handle->sfid = (hid_t)-1;

        }

        return CBF_SUCCESS;
    }


    /* Write a saveframe name to an HDF5 file
     Make a new group of NeXus class CBF_cbfsf
     in the CBF_cbf current datablock
     */

    int cbf_write_h5saveframename (const cbf_node *saveframe,
                                   cbf_h5handle h5handle)
    {

        /* Does the node exist? */

        if (!saveframe || !h5handle || h5handle->rootid <0 || h5handle->dbid <0)

            return CBF_ARGUMENT;

        /* If another saveframe is open, close all its children
         and the saveframe itself */

        if (h5handle->sfid >= 0) {

            cbf_failnez(cbf_close_h5saveframe(h5handle));

        }


        /* Write the name */

        cbf_h5failneg(h5handle->sfid=H5Gcreatex(h5handle->dbid,
                                                saveframe->name),
                      CBF_FORMAT);

        cbf_failnez(cbf_apply_h5text_attribute(h5handle->sfid,
                                               "NX_class", "CBF_cbfsf",0));

        return CBF_SUCCESS;
    }




    /* Write a datablock name to an HDF5 file
     Make a new group of NeXus class CBF_cbfdb in the CBF_cbf class root
     */

    int cbf_write_h5datablockname (const cbf_node *datablock, cbf_h5handle h5handle)
    {

        /* Does the node exist? */

        if (!datablock || !h5handle || h5handle->rootid <0)

            return CBF_ARGUMENT;

        /* If another datablock is open, close all its children
         and the datablock itself */

        if (h5handle->colid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->colid),CBF_FORMAT);

            h5handle->colid = (hid_t)-1;

        }

        if (h5handle->catid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->catid),CBF_FORMAT);

            h5handle->catid = (hid_t)-1;

        }

        if (h5handle->sfid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->sfid),CBF_FORMAT);

            h5handle->sfid = (hid_t)-1;

        }

        if (h5handle->dbid >= 0) {

            cbf_h5failneg(H5Gclose(h5handle->dbid),CBF_FORMAT);

            h5handle->dbid = (hid_t)-1;

        }

        /* save the datablock name in the read bookmark */

        (h5handle->bookmark).datablock = datablock->name;


        /* Write the name */

        cbf_h5failneg(h5handle->dbid=H5Gcreatex(h5handle->rootid,
                                                datablock->name),
                      CBF_FORMAT);

        cbf_failnez(cbf_apply_h5text_attribute(h5handle->dbid,
                                               "NX_class", "CBF_cbfdb",0));

        return CBF_SUCCESS;
    }


    /* Write a node to an HDF5 file */

    int cbf_write_h5node (cbf_handle handle, const cbf_node *node,
                          const cbf_h5handle h5handle)
    {
        unsigned int count;


        /* Follow any links */

        node = cbf_get_link (node);


        /* Does the node exist? */

        if (!node)

            return CBF_ARGUMENT;

        /* Node type */

        switch (node->type)
        {


                /* For the root, start the file with a CBF group */

            case CBF_ROOT:

                break;

            case CBF_DATABLOCK:

                if (h5handle->rootid < 0) return CBF_FORMAT;

                cbf_failnez (cbf_write_h5datablockname (node, h5handle))

                break;

            case CBF_CATEGORY:

                cbf_failnez (cbf_write_h5category (handle, node, h5handle))

                break;

            case CBF_SAVEFRAME:

                cbf_failnez (cbf_write_h5saveframename (node, h5handle))

                break;


            default:

                return CBF_ARGUMENT;
        }


        /* Write the children */

        if (node->type == CBF_ROOT || node->type == CBF_DATABLOCK || node->type == CBF_SAVEFRAME)

            for (count = 0; count < node->children; count++)
            {

                cbf_failnez (cbf_write_h5node (handle, node->child [count], h5handle))

            }

        if (node->type == CBF_SAVEFRAME) {

            cbf_failnez(cbf_close_h5saveframe(h5handle));

        }


        /* Flush the buffers */

        cbf_h5failneg(H5Fflush(h5handle->hfile,H5F_SCOPE_LOCAL),CBF_ARGUMENT);

        return CBF_SUCCESS;


    }



    /* Create an HDF5 File handle */

    int cbf_create_h5handle(cbf_h5handle *h5handle,const char * h5filename)
	{
        hid_t fcreate_prop_list;

        cbf_failnez(cbf_make_h5handle(h5handle));

        cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));

        (*h5handle)->rwmode = 1;

        cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_h5onfailneg((*h5handle)->hfile = H5Fcreate(h5filename,H5F_ACC_TRUNC,
                                                       H5P_DEFAULT,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_h5onfailneg(H5Pclose(fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_onfailnez(cbf_H5Gcreate_in_handle(*h5handle,"CBF_cbf",
                                              &((*h5handle)->rootid)),
                      cbf_free_h5handle(*h5handle));

        cbf_failnez(cbf_apply_h5text_attribute((*h5handle)->rootid,"NX_class",
                                               "CBF_cbf",0));


        return CBF_SUCCESS;

	}

	/* Create an HDF5 File handle without adding an CBF_cbf group to it */

	int cbf_create_h5handle2(cbf_h5handle *h5handle,const char * h5filename)
	{
		hid_t fcreate_prop_list;

		cbf_failnez(cbf_make_h5handle(h5handle));

		cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));

		(*h5handle)->rwmode = 1;

		cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

		cbf_h5onfailneg((*h5handle)->hfile = H5Fcreate(h5filename,H5F_ACC_TRUNC,
                                                       H5P_DEFAULT,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

		cbf_h5onfailneg(H5Pclose(fcreate_prop_list), CBF_ARGUMENT,
                        cbf_free((void**) h5handle, NULL));

		return CBF_SUCCESS;
	}


    /*  Write cbf to HDF5 file hfile 

	Should check the type of CBF file we have (miniCBF [+header convention, eventually...] vs full CBF) and call the appropriate function.
	*/
    int cbf_write_h5file (cbf_handle handle, cbf_h5handle h5handle, int flags)
    {
		cbf_node *node = NULL;

        int errorcode = CBF_SUCCESS;

        if (!handle || !h5handle)

            return CBF_ARGUMENT;

        /* Transfer the flags into h5handle */

        h5handle->flags = flags;

        /* Find the root node */

		cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT));

        /* Reset the reference counts */

		cbf_failnez( cbf_reset_refcounts(handle->dictionary) );

		/* ensure the handle contains some basic structure */

		cbf_reportnez(cbf_h5handle_require_entry(h5handle,0,"entry"), errorcode);
		cbf_reportnez(cbf_h5handle_require_instrument(h5handle,0), errorcode);

		/* Do the mappings from CBF to nexus */

		/* Write the CBF data into the file in a special node to keep
         it separate from actual mappings */

        errorcode = cbf_write_h5node (handle, node, h5handle);

        if (!errorcode) {

            cbf_write_h5nxaxes(handle, h5handle);

        }

        return errorcode;

	}

	/*
	The following set of functions - 'cbf_write_cbf_h5file__XYZ' - convert a CBF category 'XYZ' into NeXus format,
	packing the resulting data into a location given by the 'h5handle' argument.
     */

	static int cbf_write_cbf_h5file__diffrn_detector_element
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		hid_t detector = CBF_H5FAIL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_detector_element\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn_detector_element) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* select the category & row to work with, can then switch columns at will */
		_CBF_CALL(cbf_find_category(handle, "diffrn_detector_element"));
		_CBF_CALL(cbf_find_column(handle, "id"));
		_CBF_CALL(cbf_find_row(handle, key->diffrn_detector_element));
		_CBF_CALL(cbf_find_column(handle, "detector_id"));
		_CBF_CALL(cbf_get_value(handle, &key->diffrn_detector));

		/* Ensure some basic structure is present */
		{
			unsigned int nRows = 0;
			_CBF_CALL(cbf_count_rows(handle,&nRows));
			if (1 != nRows) fprintf(stderr,"%s: Multiple element support not yet implemented\n",__WHERE__);
		}
		_CBF_CALL(cbf_h5handle_require_detector(h5handle,&detector,0));

		/* Convert beam_center_x: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "center[1]")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			const hsize_t max[] = {H5S_UNLIMITED};
			const hsize_t cnk[] = {1};
			const hsize_t off[] = {h5handle->slice};
			const hsize_t cnt[] = {1};
			hsize_t buf[] = {0};
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
			_CBF_CALL(cbf_H5Drequire(detector,&dset,"beam_center_x",1,max,cnk,buf,H5T_IEEE_F64LE));
			_CBF_CALL(cbf_H5Dinsert(dset,off,0,cnt,buf,&num,H5T_NATIVE_DOUBLE));
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","mm"));
			cbf_H5Dfree(dset);
		}

		/* Convert beam_center_y: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "center[2]")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			const hsize_t max[] = {H5S_UNLIMITED};
			const hsize_t cnk[] = {1};
			const hsize_t off[] = {h5handle->slice};
			const hsize_t cnt[] = {1};
			hsize_t buf[] = {0};
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
			_CBF_CALL(cbf_H5Drequire(detector,&dset,"beam_center_x",1,max,cnk,buf,H5T_IEEE_F64LE));
			_CBF_CALL(cbf_H5Dinsert(dset,off,0,cnt,buf,&num,H5T_NATIVE_DOUBLE));
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","mm"));
			cbf_H5Dfree(dset);
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_detector
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		hid_t detector = CBF_H5FAIL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_detector\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn_detector) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* select the category & row to work with, can then switch columns at will */
		_CBF_CALL(cbf_find_category(handle, "diffrn_detector"));
		_CBF_CALL(cbf_find_column(handle, "id"));
		_CBF_CALL(cbf_find_row(handle, key->diffrn_detector));
		_CBF_CALL(cbf_find_column(handle, "diffrn_id"));
		_CBF_CALL(cbf_get_value(handle, &key->diffrn));

		/* Ensure some basic structure is present */
		_CBF_CALL(cbf_h5handle_require_detector(h5handle,&detector,0));


		/* Convert the type: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "type")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"description",value));
		}

		/* Convert details: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "details")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"details",value));
		}

		/* Convert detector: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "detector")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"type",value));
		}

		/* Convert dtime: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "dtime")) {
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector,0,"dead_time",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector,0,"dead_time",num,cmp_double));
#endif
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_measurement
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"diffrn_measurement\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* select the category & row to work with, can then switch columns at will */
		_CBF_CALL(cbf_find_category(handle, "diffrn_measurement"));
		_CBF_CALL(cbf_find_column(handle, "diffrn_id"));
		_CBF_CALL(cbf_find_row(handle, key->diffrn));

		/*
		NOTE: Don't convert keys: diffrn_id, id
		*/
		_CBF_CALL(cbf_find_column(handle, "id"));
		_CBF_CALL(cbf_get_value(handle, &key->diffrn_measurement));

		/* Convert the details: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "details")) {
			const char * value;
			hid_t goniometer = CBF_H5FAIL;
			_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&goniometer,"goniometer"));
			_CBF_CALL(cbf_H5Arequire_string(goniometer,"NX_class","NXgoniometer"));
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(goniometer,0,"details",value));
			cbf_H5Gfree(goniometer);
		}

		/* Convert the device_type: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "device_type")) {
			const char * value;
			hid_t goniometer = CBF_H5FAIL;
			_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&goniometer,"goniometer"));
			_CBF_CALL(cbf_H5Arequire_string(goniometer,"NX_class","NXgoniometer"));
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(goniometer,0,"type",value));
			cbf_H5Gfree(goniometer);
		}

		/* Convert the device_details: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "device_details")) {
			const char * value;
			hid_t goniometer = CBF_H5FAIL;
			_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&goniometer,"goniometer"));
			_CBF_CALL(cbf_H5Arequire_string(goniometer,"NX_class","NXgoniometer"));
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(goniometer,0,"description",value));
			cbf_H5Gfree(goniometer);
		}

		/* Convert the device: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "device")) {
			const char * value;
			hid_t goniometer = CBF_H5FAIL;
			_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&goniometer,"goniometer"));
			_CBF_CALL(cbf_H5Arequire_string(goniometer,"NX_class","NXgoniometer"));
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(goniometer,0,"local_name",value));
			cbf_H5Gfree(goniometer);
		}

		/* Convert the sample_detector_distance: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "sample_detector_distance")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			const hsize_t max[] = {H5S_UNLIMITED};
			const hsize_t cnk[] = {1};
			const hsize_t offset[] = {h5handle->slice};
			const hsize_t count[] = {1};
			hsize_t buf[] = {0};
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
			_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,"distance",1,max,cnk,buf,H5T_IEEE_F64LE));
			_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","mm"));
			cbf_H5Dfree(dset);
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_radiation_wavelength
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		hid_t monochromator = CBF_H5FAIL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif
		if (0) fprintf(stderr,"diffrn_radiation_wavelength\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->wavelength_id) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* select the category & row to work with, can then switch columns at will */
		_CBF_CALL(cbf_find_category(handle, "diffrn_radiation_wavelength"));
		_CBF_CALL(cbf_find_column(handle, "id"));
		_CBF_CALL(cbf_find_row(handle, key->wavelength_id));

		_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&monochromator,"monochromator"));
		_CBF_CALL(cbf_H5Arequire_string(monochromator,"NX_class","NXmonochromator"));

		/* convert the wavelength */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "wavelength")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			const hsize_t max[] = {H5S_UNLIMITED};
			const hsize_t cnk[] = {1};
			const hsize_t offset[] = {h5handle->slice};
			const hsize_t count[] = {1};
			hsize_t buf[] = {0};
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
			_CBF_CALL(cbf_H5Drequire(monochromator,&dset,"wavelength",1,max,cnk,buf,H5T_IEEE_F64LE));
			_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","A"));
			cbf_H5Dfree(dset);
		}

		/* Convert the weight */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "wt")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			const hsize_t max[] = {H5S_UNLIMITED};
			const hsize_t cnk[] = {1};
			const hsize_t offset[] = {h5handle->slice};
			const hsize_t count[] = {1};
			hsize_t buf[] = {0};
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
			_CBF_CALL(cbf_H5Drequire(monochromator,&dset,"weight",1,max,cnk,buf,H5T_IEEE_F64LE));
			_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
			cbf_H5Dfree(dset);
		}

		cbf_H5Gfree(monochromator);
		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_source
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		hid_t source = CBF_H5FAIL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif
		if (0) fprintf(stderr,"diffrn_source\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* select the category & row to work with, can then switch columns at will */
		_CBF_CALL(cbf_find_category(handle, "diffrn_source"));
		_CBF_CALL(cbf_find_column(handle, "diffrn_id"));
		_CBF_CALL(cbf_find_row(handle, key->diffrn));

		_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&source,"source"));
		_CBF_CALL(cbf_H5Arequire_string(source,"NX_class","NXsource"));

		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "current")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(source,&dset,"current",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(source,&dset,"current",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","mA"));
			cbf_H5Dfree(dset);
		}

		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "power")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(source,&dset,"power",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(source,&dset,"power",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","kW"));
			cbf_H5Dfree(dset);
		}

		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "source")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(source,0,"type",value));
		}

		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "target")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(source,0,"target_material",value));
		}

		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "type")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(source,0,"name",value));
		}

		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "voltage")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(source,&dset,"voltage",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(source,&dset,"voltage",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","kV"));
			cbf_H5Dfree(dset);
		}

		cbf_H5Gfree(source);
		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_radiation
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		hid_t collimator = CBF_H5FAIL;
		hid_t monochromator = CBF_H5FAIL;
		hid_t source = CBF_H5FAIL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_radiation\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* select the category & row to work with, can then switch columns at will */
		_CBF_CALL(cbf_find_category(handle, "diffrn_radiation"));
		_CBF_CALL(cbf_find_column(handle, "diffrn_id"));
		_CBF_CALL(cbf_find_row(handle, key->diffrn));

		/* Ensure some basic structure is present */
		_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&collimator,"collimator"));
		_CBF_CALL(cbf_H5Arequire_string(collimator,"NX_class","NXcollimator"));
		_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&source,"source"));
		_CBF_CALL(cbf_H5Arequire_string(source,"NX_class","NXsource"));
		_CBF_CALL(cbf_H5Grequire(h5handle->nxinst,&monochromator,"monochromator"));
		_CBF_CALL(cbf_H5Arequire_string(monochromator,"NX_class","NXmonochromator"));

		/* Convert the collimation: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "collimation")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(collimator,0,"description",value));
		}

		/* Convert the divergence: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "div_x_source")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(collimator,&dset,"divergence_x",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(collimator,&dset,"divergence_x",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","degrees"));
			cbf_H5Dfree(dset);
		}
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "div_y_source")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(collimator,&dset,"divergence_y",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(collimator,&dset,"divergence_y",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","degrees"));
			cbf_H5Dfree(dset);
		}
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "div_x_y_source")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = strtod(value,0);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(collimator,&dset,"divergence_xy",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(collimator,&dset,"divergence_xy",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","degrees-2"));
			cbf_H5Dfree(dset);
		}

		/* Convert the imhomgeneity: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "inhomogeneity")) {
			hid_t dset = CBF_H5FAIL;
			const char * value;
			double num = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &value));
			num = 2.0*strtod(value,0);
#ifdef CBF_USE_ULP            
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(source,&dset,"sigma_x",num,cmp_double,&cmp_params));
#else
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2(source,&dset,"sigma_x",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","mm"));
			cbf_H5Dfree(dset);
#ifdef CBF_USE_ULP
			_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(source,&dset,"sigma_y",num,cmp_double,&cmp_params));
#else
            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(source,&dset,"sigma_y",num,cmp_double));
#endif
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","mm"));
			cbf_H5Dfree(dset);
		}

		/* Convert the probe: */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "probe")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(source,0,"probe",value));
		}

		/* convert the monochromator */
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "monochromator")) {
			const char * value;
			_CBF_CALL(cbf_get_value(handle, &value));
			_CBF_CALL(cbf_H5Drequire_flstring(monochromator,0,"description",value));
		}

		/* convert the polarisn_source{_norm,_ratio}
		NOTE: don't define PI, use acos(-1.0) to get it to machine accuracy for every machine - including
		double-extended machines & hypothetical future quad-precision machines - without risking typos.
		*/
		if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "polarizn_source_norm")) {
			const char * norm;
			double two_theta = 0./0.;
			_CBF_CALL(cbf_get_value(handle, &norm));
			two_theta = strtod(norm,0);
			two_theta *= acos(-1.0)/90.0;
			if (CBF_SUCCESS == error && CBF_SUCCESS == cbf_find_column(handle, "polarizn_source_ratio")) {
				const char * ratio;
				double rho = 0./0.;
				const hsize_t max[] = {H5S_UNLIMITED,4};
				const hsize_t chunk[] = {1,4};
				hsize_t buf[] = {0,0};
				hid_t sample = CBF_H5FAIL;
				hid_t beam = CBF_H5FAIL;
				_CBF_CALL(cbf_get_value(handle, &ratio));
				rho = strtod(ratio,0);
				_CBF_CALL(cbf_h5handle_require_sample(h5handle,&sample));
				_CBF_CALL(cbf_H5Grequire(sample,&beam,"beam"));
				_CBF_CALL(cbf_H5Arequire_string(beam,"NX_class","NXbeam"));
				if (CBF_SUCCESS == error) {
				hid_t dset = CBF_H5FAIL;
					const hsize_t offset[] = {h5handle->slice,0};
					const hsize_t count[] = {1,4};
					const double value[] = {1.0,rho*cos(two_theta),rho*sin(two_theta),0.0};
					_CBF_CALL(cbf_H5Drequire(beam,&dset,"incident_polarization_stokes",2,max,chunk,buf,H5T_IEEE_F64LE));
					_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,value,H5T_NATIVE_DOUBLE));
					cbf_H5Dfree(dset);
				}
				cbf_H5Gfree(beam);
			}
		}

		_CBF_CALL(cbf_find_column(handle, "wavelength_id"));
		_CBF_CALL(cbf_get_value(handle, &key->wavelength_id));

		cbf_H5Gfree(collimator);
		cbf_H5Gfree(monochromator);
		cbf_H5Gfree(source);
		return error;
	}

	/*
	Function to convert a CBF axis to a NeXus axis, writing to a specified HDF5 group & returning the resulting HDF5 dataset.
	*/
	static int cbf_write_cbfaxis_h5file
			(cbf_handle handle,
			const char * const axis_id,
			const char * * const axis_type,
			double matrix[3][3],
			hid_t dset,
#ifdef CBF_USE_ULP
			int (*cmp)(const void *, const void *, size_t, const void * const)
            , const void * const cmp_params
#else
            int (*cmp)(const void *, const void *, size_t)
#endif
             )
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"write_cbfaxis_h5file\n");

		/* check arguments */
		if (!handle || !axis_id || !cbf_H5Ivalid(dset)) return CBF_ARGUMENT;

		/* find the axis in the cbf file */
		_CBF_CALL(cbf_find_category(handle, "axis"));
		_CBF_CALL(cbf_find_column(handle, "id"));
		_CBF_CALL(cbf_find_row(handle, axis_id));

		/* check if I got the axis before trying to use it */
		if (CBF_SUCCESS == error) {
			{ /* verify the 'type' of axis */
				const char * type = 0;
				_CBF_CALL(cbf_find_column(handle, "type"));
				_CBF_CALL(cbf_get_value(handle, &type));
				if (!cbf_cistrcmp(type,"rotation")) {
					_CBF_CALL(cbf_H5Arequire_string(dset,"transformation_type","rotation"));
					_CBF_CALL(cbf_H5Arequire_string(dset,"units","degrees"));
				} else if (!cbf_cistrcmp(type,"translation")) {
					_CBF_CALL(cbf_H5Arequire_string(dset,"transformation_type","translation"));
					_CBF_CALL(cbf_H5Arequire_string(dset,"units","mm"));
				} else {
					_CBF_CALL(cbf_H5Arequire_string(dset,"transformation_type","general"));
				}
				if (axis_type) *axis_type = type;
			}
			{ /* get the coordinate system and transform the vector & offset */
				const char * system = "laboratory";
				if (!cbf_find_column(handle,"system")) {
					_CBF_CALL(cbf_get_value(handle,&system));
					if (!system||!system[0]) system = "laboratory";
					if (cbf_cistrcmp(system,".")||cbf_cistrcmp(system,"?")) system = "laboratory";
				}
				if (cbf_cistrcmp(system,"laboratory")) {
					fprintf(stderr,"%s: Error: unsupported coordinate system '%s' for axis '%s'\n",__WHERE__,system,axis_id);
					error |= CBF_FORMAT;
				} else {
					double cbfvector[3], cbfoffset[3];
					double vector[3], offset[3];
					const hsize_t vdims[] = {3};
					double buf[3] = {0./0.};
					_CBF_CALL(cbf_get_axis_poise(handle, 0.,
						(double *)cbfvector,(double *)cbfvector+1,(double *)cbfvector+2,
						(double *)cbfoffset,(double *)cbfoffset+1,(double *)cbfoffset+2,
						NULL,axis_id,NULL));
					_CBF_CALL(cbf_apply_matrix(matrix,cbfvector,vector));
					_CBF_CALL(cbf_apply_matrix(matrix,cbfoffset,offset));
					/* Write the vector & offset */
#ifdef CBF_USE_ULP
					_CBF_CALL(cbf_H5Arequire_cmp2_ULP(dset,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,vector,buf,cmp,&cmp_params));
					_CBF_CALL(cbf_H5Arequire_cmp2_ULP(dset,"offset",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,offset,buf,cmp,&cmp_params));
#else
					_CBF_CALL(cbf_H5Arequire_cmp2(dset,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,vector,buf,cmp));
					_CBF_CALL(cbf_H5Arequire_cmp2(dset,"offset",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,offset,buf,cmp));
#endif
				}
			}
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_scan_axis
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 hid_t dset,
			 double * const data,
			 const char * const axis,
			 const char * axis_type,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		unsigned int data_row = 0;

		if (0) fprintf(stderr,"diffrn_scan_axis\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!data) {
			fprintf(stderr,"%s: No data pointer given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!axis) {
			fprintf(stderr,"%s: No axis given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!axis_type) {
			fprintf(stderr,"%s: No axis type given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!cbf_H5Ivalid(dset)) {
			fprintf(stderr,"%s: Invalid dataset handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->scan) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		_CBF_CALL(cbf_find_category(handle, "diffrn_scan_axis"));
		_CBF_CALL(cbf_rewind_column(handle));
		data_row = 0;
		while (CBF_SUCCESS == error) {
			const char * dsa_axis = 0;
			const char * dsa_scan = 0;
			if (CBF_SUCCESS == error) {
				const int match_error = cbf_select_row(handle,data_row);
				if (CBF_NOTFOUND == match_error) {
					fprintf(stderr,"%s: Row could not be found\n",__WHERE__);
					break;
				}
				error |= match_error;
				if (CBF_SUCCESS != match_error) {
					fprintf(stderr,"%s: Error selecting a row: %s\n",__WHERE__,cbf_strerror(match_error));
					break;
				}
			}
			_CBF_CALL(cbf_find_column(handle, "axis_id"));
			_CBF_CALL(cbf_get_value(handle, &dsa_axis));
			_CBF_CALL(cbf_find_column(handle, "scan_id"));
			_CBF_CALL(cbf_get_value(handle, &dsa_scan));
			if (CBF_SUCCESS == error && !cbf_cistrcmp(dsa_axis,axis) && !cbf_cistrcmp(dsa_scan,key->scan)) {
				if (0 == h5handle->slice) {
					/*
					I am at the start of a scan, so can read the data from CBF's 'xyz_start'. If it
					doesn't exist then assume it starts at 0.0, report any other error as actual problems.
					*/
					int err = CBF_SUCCESS;
					/* select the appropriate data to read */
					if (!cbf_cistrcmp(axis_type, "translation")) {
						err = cbf_find_column(handle, "displacement_start");
					} else if (!cbf_cistrcmp(axis_type, "rotation")) {
						err = cbf_find_column(handle, "angle_start");
					} else {
						fprintf(stderr,"%s: Error: Unexpected axis type\n",__WHERE__);
						err = CBF_FORMAT;
					}
					/* I have a suitable source of data selected in the handle */
					if (CBF_SUCCESS == err) {
						const char * num = 0;
						_CBF_CALL(cbf_get_value(handle, &num));
						*data = strtod(num,0);
					} else if (CBF_NOTFOUND == err) {
						/* assume a start value of 0.0 if no data given */
						*data = 0.;
					} else {
						error |= err;
					}
				} else {
					/*
					Look for some approprite data for the axis type. Get the value in the previous
					NeXus frame (by index) for extrapolation, or NaN. Increment it by a value
					obtained from the CBF file. Both the 'xyz_increment' and 'xyz_rstrt_incr' need
					to be found and added to the previous data item.
					*/
					double prev = 0./0., inc = 0.;
					hsize_t offset[] = {h5handle->slice-1};
					hsize_t count[] = {1};
					/* extract the previous datapoint */
					_CBF_CALL(cbf_H5Dread2(dset,offset,0,count,&prev,H5T_NATIVE_DOUBLE));
					/* find how much it should be incremented */
					if (!cbf_cistrcmp(axis_type, "translation")) {
						int err = CBF_SUCCESS;
						err = cbf_find_column(handle, "displacement_increment");
						if (CBF_SUCCESS == err) {
							const char * num = 0;
							_CBF_CALL(cbf_get_value(handle, &num));
							inc += strtod(num,0);
						} else if (CBF_NOTFOUND != err) {
							error |= err;
						}
						err = cbf_find_column(handle, "displacement_rstrt_incr");
						if (CBF_SUCCESS == err) {
							const char * num = 0;
							_CBF_CALL(cbf_get_value(handle, &num));
							inc += strtod(num,0);
						} else if (CBF_NOTFOUND != err) {
							error |= err;
						}
					} else if (!cbf_cistrcmp(axis_type, "rotation")) {
						int err = CBF_SUCCESS;
						err = cbf_find_column(handle, "angle_increment");
						if (CBF_SUCCESS == err) {
							const char * num = 0;
							_CBF_CALL(cbf_get_value(handle, &num));
							inc += strtod(num,0);
						} else if (CBF_NOTFOUND != err) {
							error |= err;
						}
						err = cbf_find_column(handle, "angle_rstrt_incr");
						if (CBF_SUCCESS == err) {
							const char * num = 0;
							_CBF_CALL(cbf_get_value(handle, &num));
							inc += strtod(num,0);
						} else if (CBF_NOTFOUND != err) {
							error |= err;
						}
					} else {
						fprintf(stderr,"%s: Error: Unexpected axis type\n",__WHERE__);
						error |= CBF_FORMAT;
					}
					/* get the extrapolated datapoint */
					*data = prev+inc;
				}
				break;
			}
			++data_row;
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_scan_frame_axis
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 hid_t dset,
			 const char * const axis,
			 const char * axis_type,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"diffrn_scan_frame_axis\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!axis) {
			fprintf(stderr,"%s: No axis given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!axis_type) {
			fprintf(stderr,"%s: No axis type given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!cbf_H5Ivalid(dset)) {
			fprintf(stderr,"%s: Invalid dataset handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->frame) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		{
			int match_error = CBF_SUCCESS;
			/* the data to be written to the file */
			double data = 0./0.;
			unsigned int data_row = 0;
			/* check for absolute settings in the CBF file, use them if available */
			_CBF_CALL(cbf_find_category(handle, "diffrn_scan_frame_axis"));
			_CBF_CALL(cbf_rewind_column(handle));
			while (CBF_SUCCESS == error) {
				const char * dsfa_axis = 0;
				const char * dsfa_frame = 0;
				if (CBF_SUCCESS == error) {
					match_error = cbf_select_row(handle,data_row);
					if (CBF_NOTFOUND == match_error) {
						fprintf(stderr,"%s: Row could not be found\n",__WHERE__);
						break;
					}
					error |= match_error;
					if (CBF_SUCCESS != match_error) {
						fprintf(stderr,"%s: Error selecting a row: %s\n",__WHERE__,cbf_strerror(match_error));
						break;
					}
				}
				_CBF_CALL(cbf_find_column(handle, "axis_id"));
				_CBF_CALL(cbf_get_value(handle, &dsfa_axis));
				_CBF_CALL(cbf_find_column(handle, "frame_id"));
				_CBF_CALL(cbf_get_value(handle, &dsfa_frame));
				if (CBF_SUCCESS == error && !cbf_cistrcmp(dsfa_axis,axis) && !cbf_cistrcmp(dsfa_frame,key->frame)) {
					/* I have found some valid data, extract it or complain that it doesn't exist */
					if (!cbf_cistrcmp(axis_type, "translation")) {
						const char * num = 0;
						_CBF_CALL(cbf_find_column(handle, "displacement"));
						_CBF_CALL(cbf_get_value(handle, &num));
						data = strtod(num,0);
					} else if (!cbf_cistrcmp(axis_type, "rotation")) {
						const char * num = 0;
						_CBF_CALL(cbf_find_column(handle, "angle"));
						_CBF_CALL(cbf_get_value(handle, &num));
						data = strtod(num,0);
					} else {
						fprintf(stderr,"%s: Error: Unexpected axis type\n",__WHERE__);
						error |= CBF_FORMAT;
					}
					break;
				}
				++data_row;
			}
			/* otherwise, extrapolate a value from the preceeding frame and other CBF data */
			if (CBF_NOTFOUND == match_error) {
				_CBF_CALL(cbf_write_cbf_h5file__diffrn_scan_axis(handle, h5handle, dset, &data, axis, axis_type, key));
			}
			/* write the data, if I have any */
			if (CBF_SUCCESS == error) {
				/* size parameters defining the data size & location */
				const hsize_t offset[] = {h5handle->slice};
				const hsize_t count[] = {1};
				hsize_t buf[] = {0};
				_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&data,H5T_NATIVE_DOUBLE));
			} else {
				/* if I can't do anything, complain: it's a big problem */
				fprintf(stderr,"%s: Error: Could not write axis data\n",__WHERE__);
				error |= CBF_FORMAT;
			}
		}

		return error;
	}

	/* check the dependency chain */
	static int cbf_check_axis_dependency_chain
			(const cbf_axisIndex_t * const axisIndex,
			 const char * * const object_dependency,
			 const cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		const unsigned int * it;
		const unsigned int * const end = axisIndex->d+axisIndex->c;
		const unsigned int * in_zero = end;

		if (!axisIndex) {
			fprintf(stderr,"%s: Error: Bad axis index given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key) {
			fprintf(stderr,"%s: Error: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* set in_degree to zero for all axes in this subset */
		for (it = axisIndex->d; it != end; ++it) {
			if (0) {
				fprintf(stderr,"%d: ('%s','%s','%s')\n",*it,
						key->axis.axis_id[*it],
						key->axis.depends_on[*it],
						key->axis.path[*it]);
			}
			key->axis.in_degree[*it] = 0;
		}

		/* calculate the in_degree for axes in this subset */
		for (it = axisIndex->d; it != end; ++it) {
			const char * const depends_on = key->axis.depends_on[*it];
			const unsigned int * it2;
			for (it2 = axisIndex->d; it2 != end; ++it2) {
				if (!cbf_cistrcmp(depends_on,key->axis.axis_id[*it2])) {
					++key->axis.in_degree[*it2];
					break;
				}
			}
		}

		/* ensure there is only one axis in the subset with in_degree == 0, and find it */
		for (it = axisIndex->d; it != end; ++it) {
			if (key->axis.in_degree[*it] == 0) {
				if (in_zero == end) {
					in_zero = it;
				} else {
					fprintf(stderr,"%s: Error: Axis branching detected.\n",__WHERE__);
					error |= CBF_UNDEFINED;
				}
			} else if (key->axis.in_degree[*it] > 1) {
				fprintf(stderr,"%s: Error: Axis branching detected.\n",__WHERE__);
				error |= CBF_UNDEFINED;
			}
		}

		/* check the dependency chain has 'axisIndex->count' items, to ensure it's free of cycles */
		if (CBF_SUCCESS == error) {
			unsigned int count = 0;
			it = in_zero;
			/* whilst I have a valid iterator: */
			while (it != end) {
				/* increment the number of visited indices, and cache the current 'depends_on' value */
				++count;
				const char * depends_on = key->axis.depends_on[*it];
				/*
				Search for a matching axis in this subset, finishing in a state
				where 'it' will be valid on the next iteration iff I have a match.
				*/
				for (it = axisIndex->d; it != end; ++it) {
					if (!cbf_cistrcmp(depends_on,key->axis.axis_id[*it])) break;
				}
			}
			/* I have no cycles iff every axis in the subset was visited */
			if (count != axisIndex->c) {
				fprintf(stderr,"%s: Error: One or more dependency cycles have been detected.\n",__WHERE__);
				error |= CBF_UNDEFINED;
			}
		}

		/* if all went well record the dependency for the object */
		if (CBF_SUCCESS == error) {
			if (0) fprintf(stderr,"Can write dependencies!\n");
			*object_dependency = key->axis.axis_id[*in_zero];
			key->axis.is_leaf[*in_zero] = 1;
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_detector_axis
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		unsigned int axis_row = 0;
		cbf_axisIndex_t * axisIndex = NULL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_detector_axis\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle || !cbf_H5Ivalid(h5handle->nxdetector)) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn_detector || !key->frame) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* Ensure some basic structure is present */
		_CBF_CALL(cbf_h5handle_require_detector(h5handle,0,0));
		_CBF_CALL(_cbf_realloc_axisIndex(&axisIndex,0));

		while (CBF_SUCCESS==error) {
			const char * id = 0;
			/* get a valid row of the 'detector_id' column */
			_CBF_CALL(cbf_find_category(handle, "diffrn_detector_axis"));
			_CBF_CALL(cbf_find_column(handle, "detector_id"));
			if (CBF_SUCCESS == error) {
				const int err = cbf_select_row(handle,axis_row);
				if (CBF_NOTFOUND == err) break;
				error |= err;
				if (CBF_SUCCESS != err) {
					fprintf(stderr,"%s: Error selecting a row: %s\n",__WHERE__,cbf_strerror(err));
					break;
				}
			}
			/* check for a relevant entry */
			_CBF_CALL(cbf_get_value(handle, &id));
			if (CBF_SUCCESS == error && !cbf_cistrcmp(id,key->diffrn_detector)) {
				const char * axis = 0;
				const char * axis_type = 0;
				/* I have a match - find the relevant axis */
				_CBF_CALL(cbf_find_column(handle, "axis_id"));
				_CBF_CALL(cbf_get_value(handle, &axis));
				_CBF_CALL(cbf_find_category(handle, "axis"));
				_CBF_CALL(cbf_find_column(handle, "id"));
				_CBF_CALL(cbf_find_row(handle, axis));
				if (0) fprintf(stdout,"Converting axis data for '%s'\n",axis);
				if (CBF_SUCCESS == error) {
					hid_t dset = CBF_H5FAIL;
					const char path_empty[] = "";
					const char path_inst[] = "instrument";
					const char axis_group_name[] = "pose";
					const char * path_parts[] = {
						path_empty,
						h5handle->nxid_name,
						path_inst,
						h5handle->nxdetector_name,
						axis_group_name,
						axis,
						0
					};
					const char * const axis_path = _cbf_strjoin(path_parts,'/');
					const char * equipment = NULL;
					const char * depends_on = NULL;
					const hsize_t max[] = {H5S_UNLIMITED};
					hsize_t buf[] = {0};
					const hsize_t chunk[] = {1};
					if (0) {
						const char * const * p = path_parts;
						printf("path_parts = [\n");
						for (; *p; ++p) printf("    '%s',\n",*p);
						printf("]\n");
						printf("path = '%s'\n",axis_path);
					}
					_CBF_CALL(cbf_find_column(handle, "equipment"));
					_CBF_CALL(cbf_get_value(handle, &equipment));
					_CBF_CALL(cbf_find_column(handle, "depends_on"));
					_CBF_CALL(cbf_get_value(handle, &depends_on));
					/* add an entry to the index pointing to the offset where the new axis will be inserted */
					_CBF_CALL(_cbf_realloc_axisIndex(&axisIndex,axisIndex->c+1));
					axisIndex->d[axisIndex->c-1] = key->axis.count;
					_CBF_CALL(_cbf_insert_axis(&key->axis, axis, equipment, axis_path, depends_on));
					{ /* make sure an axis container group exists */
						hid_t axisGroup = CBF_H5FAIL;
						_CBF_CALL(cbf_H5Grequire(h5handle->nxdetector,&axisGroup,axis_group_name));
						_CBF_CALL(cbf_H5Arequire_string(axisGroup,"NX_class","NXcollection"));
						cbf_H5Gfree(axisGroup);
					}
					/* put the axis in the HDF file */
					_CBF_CALL(cbf_H5Drequire(h5handle->hfile,&dset,axis_path,1,max,chunk,buf,H5T_IEEE_F64LE));
					/* convert the meta-data */
					_CBF_CALL(cbf_write_cbfaxis_h5file(handle, axis, &axis_type, key->matrix, dset, cmp_double
#ifdef CBF_USE_ULP
                                                       ,&cmp_params
#endif
                                                       ));
					/* convert the data */
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_scan_frame_axis(handle,h5handle,dset,axis,axis_type,key));
					cbf_H5Dfree(dset);
				}
			}
			/* iterate to the next row */
			++axis_row;
		}

		_CBF_CALL(cbf_check_axis_dependency_chain(axisIndex,&key->detector_dependency,key));

		if (CBF_SUCCESS == error) {
			/* write detector dependency */
			const unsigned int * index;
			const unsigned int * const indexEnd = axisIndex->d+axisIndex->c;
			for (index = axisIndex->d; indexEnd != index; ++index) {
				if (key->axis.is_leaf[*index]) break;
			}
			if (indexEnd != index) {
				/* I have a valid leaf, put it in the detector */
				if (0) fprintf(stderr,"detector leaf = %s\n",key->axis.axis_id[*index]);
				_CBF_CALL(cbf_H5Drequire_flstring(h5handle->nxdetector,0,"depends_on",key->axis.path[*index]));
			}
		}

		_cbf_free_axisIndex(axisIndex);
		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_measurement_axis
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		int axis_row = 0;
		cbf_axisIndex_t * axisIndex = NULL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_measurement_axis\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->diffrn_measurement) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		_CBF_CALL(_cbf_realloc_axisIndex(&axisIndex,0));

		while (CBF_SUCCESS==error) {
			const char * id = 0;
			/* get a valid row of the 'detector_id' column */
			_CBF_CALL(cbf_find_category(handle, "diffrn_measurement_axis"));
			_CBF_CALL(cbf_find_column(handle, "measurement_id"));
			if (CBF_SUCCESS == error) {
				const int err = cbf_select_row(handle,axis_row);
				if (CBF_NOTFOUND == err) break;
				error |= err;
				if (CBF_SUCCESS != err) {
					fprintf(stderr,"%s: Error selecting a row: %s\n",__WHERE__,cbf_strerror(err));
					break;
				}
			}
			/* check for a relevant entry */
			_CBF_CALL(cbf_get_value(handle, &id));
			if (!cbf_cistrcmp(id,key->diffrn_measurement)) {
				const char * axis = 0;
				const char * axis_type = 0;
				/* I have a match - find the relevant axis */
				_CBF_CALL(cbf_find_column(handle, "axis_id"));
				_CBF_CALL(cbf_get_value(handle, &axis));
				_CBF_CALL(cbf_find_category(handle, "axis"));
				_CBF_CALL(cbf_find_column(handle, "id"));
				_CBF_CALL(cbf_find_row(handle, axis));
				/* convert the data */
				if (0) fprintf(stdout,"Converting axis data for '%s'\n",axis);
				if (CBF_SUCCESS == error) {
					hid_t dset = CBF_H5FAIL;
					const char path_empty[] = "";
					const char path_sample[] = "sample";
					const char axis_group_name[] = "pose";
					const char * path_parts[] = {
						path_empty,
						h5handle->nxid_name,
						path_sample,
						axis_group_name,
						axis,
						0
					};
					const char * const axis_path = _cbf_strjoin(path_parts,'/');
					const char * equipment = NULL;
					const char * depends_on = NULL;
					const hsize_t max[] = {H5S_UNLIMITED};
					hsize_t buf[] = {0};
					const hsize_t chunk[] = {1};
					if (0) {
						const char * const * p = path_parts;
						printf("path_parts = [\n");
						for (; *p; ++p) printf("    '%s',\n",*p);
						printf("]\n");
						printf("path = '%s'\n",axis_path);
					}
					_CBF_CALL(cbf_find_column(handle, "equipment"));
					_CBF_CALL(cbf_get_value(handle, &equipment));
					_CBF_CALL(cbf_find_column(handle, "depends_on"));
					_CBF_CALL(cbf_get_value(handle, &depends_on));
					/* add an entry to the index pointing to the offset where the new axis will be inserted */
					_CBF_CALL(_cbf_realloc_axisIndex(&axisIndex,axisIndex->c+1));
					axisIndex->d[axisIndex->c-1] = key->axis.count;
					_CBF_CALL(_cbf_insert_axis(&key->axis, axis, equipment, axis_path, depends_on));
					{ /* make sure an axis container group exists */
						hid_t axisGroup = CBF_H5FAIL;
						_CBF_CALL(cbf_h5handle_require_sample(h5handle,0));
						_CBF_CALL(cbf_H5Grequire(h5handle->nxsample,&axisGroup,axis_group_name));
						_CBF_CALL(cbf_H5Arequire_string(axisGroup,"NX_class","NXcollection"));
						cbf_H5Gfree(axisGroup);
					}
					/* put the axis in the HDF file */
					_CBF_CALL(cbf_H5Drequire(h5handle->hfile,&dset,axis_path,1,max,chunk,buf,H5T_IEEE_F64LE));
					/* convert the meta-data */
					_CBF_CALL(cbf_write_cbfaxis_h5file(handle, axis, &axis_type, key->matrix, dset, cmp_double
#ifdef CBF_USE_ULP
                                                       ,&cmp_params
#endif
                                                       ));
					/* convert the data */
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_scan_frame_axis(handle,h5handle,dset,axis,axis_type,key));
					cbf_H5Dfree(dset);
				}
			}
			/* iterate to the next row */
			++axis_row;
		}

		_CBF_CALL(cbf_check_axis_dependency_chain(axisIndex,&key->goniometer_dependency,key));

		if (CBF_SUCCESS == error) {
			/* write sample dependency */
			const unsigned int * const indexBegin = axisIndex->d;
			const unsigned int * const indexEnd = axisIndex->d+axisIndex->c;
			const unsigned int * index;
			for (index = indexBegin; indexEnd != index; ++index) {
				if (key->axis.is_leaf[*index]) break;
			}
			if (indexEnd != index) {
				/* I have a valid leaf, put it in the sample */
				if (0) fprintf(stderr,"goniometer leaf = %s\n",key->axis.axis_id[*index]);
				_CBF_CALL(cbf_H5Drequire_flstring(h5handle->nxsample,0,"depends_on",key->axis.path[*index]));
			}
		}

		free((void*)axisIndex);
		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_refln
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_refln\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* I don't actually have anything to convert yet */

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* I don't actually have anything to convert yet */

		return error;
	}

	static int cbf_write_cbf_h5file__array_structure
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"array_structure\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* I don't actually have anything to convert yet */

		return error;
	}

	static int cbf_write_cbf_h5file__array_intensities
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		unsigned int data_id = 0;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"array_intensities\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->array || !key->binary) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* Locate the row containing relevant data */
		_CBF_CALL(cbf_find_category(handle, "array_intensities"));
		while (CBF_SUCCESS == error) {
			const char * array = 0, * binary = 0;
			_CBF_CALL(cbf_find_column(handle, "array_id"));
			_CBF_CALL(cbf_select_row(handle,data_id));
			_CBF_CALL(cbf_get_value(handle, &array));
			_CBF_CALL(cbf_find_column(handle, "binary_id"));
			_CBF_CALL(cbf_get_value(handle, &binary));
			if (CBF_SUCCESS == error && !cbf_cistrcmp(array,key->array) && !cbf_cistrcmp(binary,key->binary)) {
				/* I have the correct row, ensure relevant hdf5 groups exist */
				{ /* extract linearity data */
					hid_t dset = CBF_H5FAIL;
					const char * linearity = NULL;
					const char str_scale[] = "scaling_factor";
					const char str_offset[] = "offset";
					hsize_t max[] = {H5S_UNLIMITED};
					hsize_t cnk[] = {1};
					hsize_t buf[] = {0};
					hsize_t offset[] = {h5handle->slice};
					hsize_t count[] = {1};
					_CBF_CALL(cbf_find_column(handle, "linearity"));
					_CBF_CALL(cbf_get_value(handle, &linearity));
					if (CBF_SUCCESS != error) break;
					if (!cbf_cistrcmp(linearity,"linear")) {
						/* store gain */
						const char * string = NULL;
						double num = 0./0.;
						_CBF_CALL(cbf_find_column(handle, "gain"));
						_CBF_CALL(cbf_get_value(handle, &string));
						num = strtod(string,0);
						_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,str_scale,1,max,cnk,buf,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
						cbf_H5Dfree(dset);
					} else if (!cbf_cistrcmp(linearity,"offset_scaling")) {
						/* store scaling & offset */
						const char * string = NULL;
						double num = 0./0.;
						_CBF_CALL(cbf_find_column(handle, "scaling"));
						_CBF_CALL(cbf_get_value(handle, &string));
						num = strtod(string,0);
						_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,str_scale,1,max,cnk,buf,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
						cbf_H5Dfree(dset);
						_CBF_CALL(cbf_find_column(handle, "offset"));
						_CBF_CALL(cbf_get_value(handle, &string));
						num = strtod(string,0);
						_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,str_offset,1,max,cnk,buf,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
						cbf_H5Dfree(dset);
					} else if (!cbf_cistrcmp(linearity,"offset")) {
						/* store offset */
						const char * string = NULL;
						double num = 0./0.;
						_CBF_CALL(cbf_find_column(handle, "offset"));
						_CBF_CALL(cbf_get_value(handle, &string));
						num = strtod(string,0);
						_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,str_offset,1,max,cnk,buf,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
						cbf_H5Dfree(dset);
					} else if (!cbf_cistrcmp(linearity,"scaling")) {
						/* store scaling */
						const char * string = NULL;
						double num = 0./0.;
						_CBF_CALL(cbf_find_column(handle, "scaling"));
						_CBF_CALL(cbf_get_value(handle, &string));
						num = strtod(string,0);
						_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,str_scale,1,max,cnk,buf,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
						cbf_H5Dfree(dset);
					} else if (!cbf_cistrcmp(linearity,"sqrt_scaled")) {
						/* Extract raw data, calculate metrics & rescale. For now: complain. */
						if (1) fprintf(stderr,"%s: Error: Nonlinear scaling is not supported.\n",__WHERE__);
						error |= CBF_NOTIMPLEMENTED;
					} else if (!cbf_cistrcmp(linearity,"logarithmic_scaled")) {
						/* Extract raw data, calculate metrics & rescale. For now: complain. */
						if (1) fprintf(stderr,"%s: Error: Nonlinear scaling is not supported.\n",__WHERE__);
						error |= CBF_NOTIMPLEMENTED;
					} else if (!cbf_cistrcmp(linearity,"raw")) {
						/* no-op! */
					} else error |= CBF_FORMAT;
				}
				{ /* extract the saturation value */
					_CBF_CALL(cbf_find_column(handle, "overload"));
					_CBF_CALL(cbf_get_value(handle, &key->data_overload));
				}
				break;
			}
			++data_id;
		}

		return error;
	}

	/**
	Find a suitable HDF5 datatype for the given parameters.
	 */
	static int cbf_find_array_data_h5type
			(hid_t * const type,
			 unsigned int bits,
			 int sign,
			 int real,
			 const char *byteorder)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"find_array_data_h5type\n");

		/* check arguments */
		if (!type) {
			fprintf(stderr,"%s: Invalid type pointer given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!byteorder) {
			fprintf(stderr,"%s: No byte order given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		if (real) {
			const int order = ('l'==*byteorder||'L'==*byteorder) ? 0 : 1;
			if (!sign) error |= CBF_FORMAT;
			else {
				const hid_t tbl[2][2] = {
					{H5T_IEEE_F32LE,H5T_IEEE_F64LE},
					{H5T_IEEE_F32BE,H5T_IEEE_F64BE}
				};
				unsigned int idx = 2;
				idx = bits<=64 ? 1 : idx;
				idx = bits<=32 ? 0 : idx;
				if (idx > 1) error |= CBF_FORMAT;
				else *type = tbl[order][idx];
			}
		} else {
			const int order = ('l'==*byteorder||'L'==*byteorder) ? 0 : 1;
			/* define a lookup table for integer types */
			const hid_t tbl[2][2][4] = {
				{
					{H5T_STD_I8LE,H5T_STD_I16LE,H5T_STD_I32LE,H5T_STD_I64LE},
					{H5T_STD_U8LE,H5T_STD_U16LE,H5T_STD_U32LE,H5T_STD_U64LE}
				}, {
					{H5T_STD_I8BE,H5T_STD_I16BE,H5T_STD_I32BE,H5T_STD_I64BE},
					{H5T_STD_U8BE,H5T_STD_U16BE,H5T_STD_U32BE,H5T_STD_U64BE}
				}
			};
			/* select a value from the table */
			unsigned int idx = 4;
			idx = bits<=64 ? 3 : idx;
			idx = bits<=32 ? 2 : idx;
			idx = bits<=16 ? 1 : idx;
			idx = bits<=8  ? 0 : idx;
			if (idx > 3) error |= CBF_FORMAT;
			else *type = tbl[order][sign?0:1][idx];
		}

		return error;
	}

	/**
	Decompress the data selected in the handle, ensure an appropriate HDF5 dataset exists to store it,
	insert it a the given index with some parameter values set according to the given flags.

	Writes saturation_value and data to h5handle->nxdetector
	*/
	static int cbf_write_array_h5file
			(const cbf_node * node,
			 const unsigned int row,
			 cbf_h5handle h5handle,
			 const char * const saturation_value,
			 hsize_t * dims)
	{
		int error = CBF_SUCCESS, found = CBF_SUCCESS, id, bits, sign, real;
		cbf_file *file;
		long start;
		const char *byteorder;
		size_t size, nelem, cbfdim[3], padding;
		unsigned int compression;
		hid_t h5type = CBF_H5FAIL;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif
		if (0) fprintf(stderr,"write_array_h5file\n");

		if (!node) {
			fprintf(stderr,"%s: Invalid node given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/** * find the datatype and array size */
		_CBF_CALL(cbf_get_bintext(node, row, NULL,
			 &id, &file, &start, &size,
			 NULL, NULL, &bits, &sign, &real,
			 &byteorder, &nelem, cbfdim+2, cbfdim+1, cbfdim+0, &padding,
			 &compression));
		_CBF_CALL(cbf_find_array_data_h5type(&h5type,bits,sign,real,byteorder));

		if (dims) {
			dims[0] = 0;
			dims[1] = cbfdim[1];
			dims[2] = cbfdim[2];
		}

		/** * check the saturation value */
		if (saturation_value) {
			hid_t dataset = CBF_H5FAIL;
			hsize_t max[] = {H5S_UNLIMITED};
			hsize_t cnk[] = {1};
			hsize_t off[] = {h5handle->slice};
			hsize_t cnt[] = {1};
			hsize_t buf[] = {0};
			_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dataset,"saturation_value",1,max,cnk,buf,h5type));
			if (real) {
				/* every float can be represented exactly by a double, so no need for float intermediate */
				const double num = strtod(saturation_value,0);
				_CBF_CALL(cbf_H5Dinsert(dataset,off,0,cnt,buf,&num,H5T_NATIVE_DOUBLE));
			} else {
				if (sign) {
					/* use longest signed integer: all smaller types represent a subset of these values */
					const signed long num = strtol(saturation_value,0,10);
					_CBF_CALL(cbf_H5Dinsert(dataset,off,0,cnt,buf,&num,H5T_NATIVE_LONG));
				} else {
					/* use longest unsigned integer: all smaller types represent a subset of these values */
					const unsigned long num = strtoul(saturation_value,0,10);
					_CBF_CALL(cbf_H5Dinsert(dataset,off,0,cnt,buf,&num,H5T_NATIVE_ULONG));
				}
			}
			cbf_H5Dfree(dataset);
		}

		/* allocate space for the decompressed data */
		if (CBF_SUCCESS == error) {
			void * value;
			const unsigned int elsize = (bits+7)/8;
			size_t nelem_read;
			const int rank = 3;
			hsize_t buf[] = {0, 0, 0};
			hsize_t h5dim[] = {0, cbfdim[1], cbfdim[2]};
			hsize_t h5max[] = {H5S_UNLIMITED, cbfdim[1], cbfdim[2]};
			hsize_t h5chunk[] = {1, cbfdim[1], cbfdim[2]};
			hid_t dset = CBF_H5FAIL;
			value = malloc(nelem*elsize);
			_CBF_CALL(cbf_set_fileposition(file, start, SEEK_SET));
			_CBF_CALL(cbf_decompress_parameters(NULL, NULL, NULL, NULL, NULL, NULL, NULL, compression, file));
			if (0) {
				fprintf(stderr,"masked compression: %d\n",compression&CBF_COMPRESSION_MASK);
				fprintf(stderr,"compression type: ");
				if (compression == CBF_CANONICAL) fprintf(stderr,"CBF_CANONICAL\n");
				else if ((compression&CBF_COMPRESSION_MASK) == CBF_PACKED) fprintf(stderr,"CBF_PACKED\n");
				else if ((compression&CBF_COMPRESSION_MASK) == CBF_PACKED_V2) fprintf(stderr,"CBF_PACKED_V2\n");
				else if (compression == CBF_BYTE_OFFSET) fprintf(stderr,"CBF_BYTE_OFFSET\n");
				else if (compression == CBF_NIBBLE_OFFSET) fprintf(stderr,"CBF_NIBBLE_OFFSET\n");
				else if (compression == CBF_PREDICTOR) fprintf(stderr,"CBF_PREDICTOR\n");
				else if (compression == CBF_NONE) fprintf(stderr,"CBF_NONE\n");
				else fprintf(stderr,"Unknown\n");
				fprintf(stderr,"element size: %d\n",(unsigned int)(elsize));
				fprintf(stderr,"real?: %s\n",real?"yes":"no");
			}

			/** * ensure a dataset exists in the detector */
			found =  cbf_H5Dfind2(h5handle->nxdetector,&dset,"data",rank,h5max,buf,h5type);
			if (CBF_SUCCESS==found) {
			} else if (CBF_NOTFOUND==found) {
				/* define variables & check args */
				hid_t dataSpace = CBF_H5FAIL;
				hid_t dcpl = H5Pcreate(H5P_DATASET_CREATE);

				/* check variables are valid */
				_CBF_CALL(cbf_H5Screate(&dataSpace, rank, h5dim, h5max));

				/* allow dataset to be chunked */
				H5Pset_chunk(dcpl,rank,h5chunk);
				/* allow compression */
				if (h5handle->flags & CBF_H5COMPRESSION_ZLIB) {
					H5Pset_deflate(dcpl, 2);
				} else if (h5handle->flags & CBF_H5COMPRESSION_CBF) {
					unsigned int cd_values[CBF_H5Z_FILTER_CBF_NELMTS];
					cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION] = compression;
					cd_values[CBF_H5Z_FILTER_CBF_RESERVED]    = 0;
					cd_values[CBF_H5Z_FILTER_CBF_BINARY_ID]   = id;
					cd_values[CBF_H5Z_FILTER_CBF_PADDING]     = padding;
					cd_values[CBF_H5Z_FILTER_CBF_ELSIZE]      = elsize;
					cd_values[CBF_H5Z_FILTER_CBF_ELSIGN]      = sign;
					cd_values[CBF_H5Z_FILTER_CBF_REAL]        = real;
					cd_values[CBF_H5Z_FILTER_CBF_DIMFAST]     = *(h5chunk+2);
					cd_values[CBF_H5Z_FILTER_CBF_DIMMID]      = *(h5chunk+1);
					cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW]     = *(h5chunk+0);

					if (h5handle->flags & CBF_H5_REGISTER_COMPRESSIONS) {
						if (!H5Zfilter_avail(CBF_H5Z_FILTER_CBF)) {
							cbf_h5reportneg(H5Zregister(CBF_H5Z_CBF),CBF_H5ERROR,error);
						}
					}

					cbf_h5reportneg
							(H5Pset_filter
							(dcpl,
							 CBF_H5Z_FILTER_CBF,
							 H5Z_FLAG_OPTIONAL,
							 CBF_H5Z_FILTER_CBF_NELMTS,
							 cd_values),
					CBF_H5ERROR, error);

				}

				/* create the dataset */
				if (CBF_SUCCESS == error)
					dset = H5Dcreate2(h5handle->nxdetector,"data",h5type,dataSpace,H5P_DEFAULT,dcpl,H5P_DEFAULT);

				/* check local variables are properly closed */
				if (cbf_H5Ivalid(dataSpace)) H5Sclose(dataSpace);
				if (cbf_H5Ivalid(dcpl)) H5Pclose(dcpl);
			} else {
				error |= found;
				fprintf(stderr,__WHERE__": error locating primary dataset: %s\n", cbf_strerror(found));
			}
			if (CBF_SUCCESS==error) {
				const hsize_t h5offset[] = {h5handle->slice, 0, 0};
				const int sig[] = {1};
				int sigbuf[] = {0};

				/** * extract the image data from CBF */
				_CBF_CALL(cbf_decompress(value, elsize, sign, nelem, &nelem_read,
						size, compression, bits, sign, file, real, byteorder,
						nelem, cbfdim[2], cbfdim[1], cbfdim[0], padding));
				if (nelem_read != nelem) error |= CBF_ENDOFDATA;

				/** * store the image data in HDF5 */
				_CBF_CALL(cbf_H5Dinsert(dset,h5offset,0,h5chunk,buf,value,h5type));
#ifdef CBF_USE_ULP
				_CBF_CALL(cbf_H5Arequire_cmp2_ULP(dset,"signal",0,0,H5T_STD_I32LE,H5T_NATIVE_INT,sig,sigbuf,cmp_int,0));
#else
				_CBF_CALL(cbf_H5Arequire_cmp2(dset,"signal",0,0,H5T_STD_I32LE,H5T_NATIVE_INT,sig,sigbuf,cmp_int));
#endif
				cbf_H5Dfree(dset);
				free((void*)value);
			}
		}

		return error;
	}

	/**
	Check the array_structure_list_axis category to ensure it only contains displacement-related axes.
	Eventually, use this to get a 'scan type' to decide how to convert axis data.
	*/
	static unsigned int cbf_check_array_structure_list_axis(cbf_handle handle)
	{
		int error = CBF_SUCCESS;
		/*
		Initialise the current type to 'could be any',
		reduce possibilities by looking at the data.
		*/
		unsigned int type = CBF_SCANTYPE_ALL;

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		_CBF_CALL(cbf_find_category(handle, "array_structure_list_axis"));
		for (error |= cbf_rewind_column(handle); CBF_SUCCESS == error; error = cbf_next_column(handle)) {
			const char * column = NULL;
			_CBF_CALL(cbf_column_name(handle,&column));
			if (cbf_cistrcmp(column,"axis_set_id") &&
				cbf_cistrcmp(column,"axis_id") &&
				cbf_cistrcmp(column,"displacement") &&
				cbf_cistrcmp(column,"displacement_increment"))
			{
				type &= ~CBF_SCANTYPE1;
			}
		}

		return type;
	}

	static int cbf_write_cbf_h5file__array_structure_list_axis
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key,
			 const unsigned int scantype)
	{
		int error = CBF_SUCCESS;
		unsigned int asla_idx = 0;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"array_structure_list_axis\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->arrayAxisSet.axis_set_id ||
				   !key->arrayAxisSet.direction ||
				   !key->arrayAxisSet.precedence ||
				   0==key->arrayAxisSet.count)
		{
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		_CBF_CALL(cbf_find_category(handle, "array_structure_list_axis"));
		_CBF_CALL(cbf_rewind_column(handle));
		_CBF_CALL(cbf_rewind_row(handle));

		for (asla_idx = 0; asla_idx < key->arrayAxisSet.count; ++asla_idx) {
			unsigned int row = 0;
			while (CBF_SUCCESS == error) {
				const char * axisSet_id = NULL;
				_CBF_CALL(cbf_find_category(handle, "array_structure_list_axis"));
				_CBF_CALL(cbf_find_column(handle, "axis_set_id"));
				_CBF_CALL(cbf_select_row(handle,row));
				_CBF_CALL(cbf_get_value(handle, &axisSet_id));
				if (CBF_SUCCESS == error && !cbf_cistrcmp(axisSet_id,key->arrayAxisSet.axis_set_id[asla_idx])) {
					if (CBF_SCANTYPE1 == scantype) {
						const char * axis = NULL;
						const char * str_disp = NULL;
						const char * str_incr = NULL;
						const unsigned int precedence = key->arrayAxisSet.precedence[asla_idx];
						const unsigned int dimension = key->arrayAxisSet.dimension[asla_idx];
						/* I have a match - find the relevant axis */
						_CBF_CALL(cbf_find_column(handle, "axis_id"));
						_CBF_CALL(cbf_get_value(handle, &axis));
						_CBF_CALL(cbf_find_column(handle, "displacement"));
						_CBF_CALL(cbf_get_value(handle, &str_disp));
						_CBF_CALL(cbf_find_column(handle, "displacement_increment"));
						_CBF_CALL(cbf_get_value(handle, &str_incr));
						_CBF_CALL(cbf_find_category(handle, "axis"));
						_CBF_CALL(cbf_find_column(handle, "id"));
						_CBF_CALL(cbf_find_row(handle, axis));
						/* convert the data */
						if (0) fprintf(stdout,"Converting axis data for '%s'\n",axis);
						if (0==precedence || 2<precedence) error |= CBF_FORMAT;
						if (CBF_SUCCESS == error) {
							hid_t dset = CBF_H5FAIL;
							/* axis name table - don't free any part of it */
							const char axis_name1[] = "fast_pixel_direction";
							const char axis_name2[] = "slow_pixel_direction";
							const char * const axis_names[] = {
								axis_name1,
								axis_name2
							};
							const char path_empty[] = "";
							const char path_inst[] = "instrument";
							const char * path_parts[] = {
								path_empty,
								h5handle->nxid_name,
								path_inst,
								h5handle->nxdetector_name,
								/* select the correct axis name */
								axis_names[precedence-1],
								0
							};
							const char * const axis_path = _cbf_strjoin(path_parts,'/');
							const char * equipment = NULL;
							const char * depends_on = NULL;
							const hsize_t dim[] = {dimension};
							const hsize_t max[] = {H5S_UNLIMITED};
							const hsize_t count[] = {dim[0]};
							hsize_t buf[] = {0};
							_CBF_CALL(cbf_find_column(handle, "equipment"));
							_CBF_CALL(cbf_get_value(handle, &equipment));
							_CBF_CALL(cbf_find_column(handle, "depends_on"));
							_CBF_CALL(cbf_get_value(handle, &depends_on));
							/* record some data for the axis & mark it as a 'leaf' axis */
							_CBF_CALL(_cbf_insert_axis(&key->axis, axis, equipment, axis_path, depends_on));
							key->axis.is_leaf[key->axis.count-1] = 1;
							/* write it to the HDF5 file */
							if (CBF_SUCCESS==error) {
								const int found =  cbf_H5Dfind2(h5handle->hfile,&dset,axis_path,1,max,buf,H5T_IEEE_F64LE);
								const double disp = strtod(str_disp,0);
								const double incr = strtod(str_incr,0);
								const hsize_t offset[] = {0};
								double * const axisData = malloc(sizeof(double)*dimension);
								{ /* initialise the axisData array */
									unsigned int i;
									for (i = 0; i < dimension; ++i) axisData[i] = i*incr+disp;
								}
								if (CBF_SUCCESS == found) {
									double * const currData = malloc(sizeof(double)*dimension);
									_CBF_CALL(cbf_H5Dread2(dset,offset,0,count,currData,H5T_NATIVE_DOUBLE));
#ifdef CBF_USE_ULP
									if (cmp_double(axisData, currData, dimension, &cmp_params)) {
#else
                                    if (cmp_double(axisData, currData, dimension)) {
#endif
										fprintf(stderr,"%s: error: image axis data doesn't match\n",__WHERE__);
										error |= CBF_H5DIFFERENT;
									}
									free((void*)currData);
								} else if (CBF_NOTFOUND == found) {
									_CBF_CALL(cbf_H5Dcreate(h5handle->hfile,&dset,axis_path,1,dim,max,count,H5T_IEEE_F64LE));
									_CBF_CALL(cbf_H5Dwrite2(dset,offset,0,count,axisData,H5T_NATIVE_DOUBLE));
								} else {
									error |= found;
									fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(found));
								}
#ifdef CBF_USE_ULP
								_CBF_CALL(cbf_write_cbfaxis_h5file(handle, axis, 0, key->matrix, dset, cmp_double, &cmp_params));
#else
								_CBF_CALL(cbf_write_cbfaxis_h5file(handle, axis, 0, key->matrix, dset, cmp_double));
#endif
								free((void*)axisData);
								cbf_H5Dfree(dset);
							}
						}
						break;
					}
				}
				++row;
			}
		}

		return error;
	}

	/**
	re-written 'cbf_get_value' to use a cbf_node instead of a cbf_handle.
     */
	static int _cbf_node_get_string(const cbf_node * node, const unsigned int row, const char * * const string)
	{
		const char * value = NULL;
		if (!node || !string) return CBF_ARGUMENT;
		if (cbf_is_binary(node,row)) return CBF_BINARY;
		{
			const int gcr = cbf_get_columnrow(&value,node,row);
			if (CBF_SUCCESS != gcr) return gcr;
		}
		*string = value ? value+1 : NULL;
		return CBF_SUCCESS;
	}

	static int cbf_write_cbf_h5file__array_structure_list
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
		int rank = -1;
		hsize_t * dim = NULL;
		cbf_node * array_id = NULL;
		cbf_node * precedence = NULL;
		cbf_node * dimension = NULL;
		cbf_node * axis_set_id = NULL;
		cbf_node * direction = NULL;
		unsigned int rows = 0, i;

		if (0) fprintf(stderr,"array_structure_list\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->array) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		{ /* get the data extent */
			const hid_t dataset = H5Dopen2(h5handle->nxdetector,"data",H5P_DEFAULT);
			if (cbf_H5Ivalid(dataset)) {
				const hid_t dataspace = H5Dget_space(dataset);
				if (cbf_H5Ivalid(dataspace)) {
					rank = H5Sget_simple_extent_dims(dataspace,0,0);
					dim = malloc(rank>0 ? rank*sizeof(hsize_t) : 0);
					if (rank <= 0 || rank != H5Sget_simple_extent_dims(dataspace,dim,0)) {
						fprintf(stderr,"%s: could not get dimensions\n",__WHERE__);
						error |= CBF_H5ERROR;
					}
					cbf_H5Sfree(dataspace);
				} else {
					fprintf(stderr,"%s: could not open dataspace\n",__WHERE__);
					error |= CBF_H5ERROR;
				}
				cbf_H5Dfree(dataset);
			} else {
				fprintf(stderr,"%s: could not open main dataset\n",__WHERE__);
				error |= CBF_H5ERROR;
			}
		}

		/* get some relevant nodes so i can index them instead of re-finding them */
		_CBF_CALL(cbf_find_category(handle, "array_structure_list"));
		_CBF_CALL(cbf_rewind_column(handle));
		_CBF_CALL(cbf_count_rows(handle, &rows));
		_CBF_CALL(cbf_find_column(handle, "array_id"));
		array_id = handle->node;
		_CBF_CALL(cbf_find_column(handle, "precedence"));
		precedence = handle->node;
		_CBF_CALL(cbf_find_column(handle, "dimension"));
		dimension = handle->node;
		_CBF_CALL(cbf_find_column(handle, "axis_set_id"));
		axis_set_id = handle->node;
		_CBF_CALL(cbf_find_column(handle, "direction"));
		direction = handle->node;
		/* iterate over the rows, extracting data */
		for (i = 0; CBF_SUCCESS == error && i != rows; ++i) {
			unsigned int prec = 0, dim_i = 0;
			const char * string = NULL;
			const char * axis_set;
			const char * dirn;
			_CBF_CALL(_cbf_node_get_string(array_id,i,&string));
			if (cbf_cistrcmp(string,key->array)) continue;
			_CBF_CALL(_cbf_node_get_string(precedence,i,&string));
			prec = strtoul(string,0,10);
			_CBF_CALL(_cbf_node_get_string(dimension,i,&string));
			dim_i = strtoul(string,0,10);
			if (dim_i != dim[rank-prec]) {
				/* ensure that the axis dimension is correct */
				fprintf(stderr,"%s: Error: %s\n",__WHERE__,"unexpected dataset extent");
				error |= CBF_H5DIFFERENT;
			}
			_CBF_CALL(_cbf_node_get_string(axis_set_id,i,&axis_set));
			_CBF_CALL(_cbf_node_get_string(direction,i,&dirn));
			_CBF_CALL(_cbf_insert_arrayAxisSet(&key->arrayAxisSet,axis_set,prec,dim_i,dirn));
		}

		free((void*)dim);
		return error;
	}

	static int cbf_write_cbf_h5file__axis_dependency_chain
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"axis_dependency_chain\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		{
			unsigned int idx;
			int * const is_processed = malloc(sizeof(int)*key->axis.count);
			for (idx = 0; idx != key->axis.count; ++idx) is_processed[idx] = 0;
			for (idx = 0; idx != key->axis.count; ++idx) {
				if (is_processed[idx] || !key->axis.is_leaf[idx]) continue;
				/*
				For each axis in the chain following a leaf axis:
				Look up parent axis, write its path in the depends_on field for this axis.
				Do the same - iteratively - for all dependent axes,
				all the way down to the root coordinate system.
				*/
				const char * const * const axis_id_begin = key->axis.axis_id;
				const char * const * const axis_id_end = key->axis.axis_id+key->axis.count;
				const char * const * it = key->axis.axis_id+idx;
				if (0) fprintf(stderr,"found a leaf: '%s' → '%s'\n",key->axis.axis_id[idx],key->axis.depends_on[idx]);
				while (CBF_SUCCESS == error && axis_id_end != it) {
					const unsigned int idx2 = it-key->axis.axis_id;
					for (it = axis_id_begin; axis_id_end != it; ++it)
						if (!cbf_cistrcmp(key->axis.depends_on[idx2],*it)) break;
					if (it != axis_id_end) {
						/*
						I have a path to store and (possibly) another axis to traverse,
						make use of the 'is_processed' variables to reduce the amount of work required.
						*/
						const unsigned int idx3 = it-key->axis.axis_id;
						hid_t dset = H5Dopen2(h5handle->hfile, key->axis.path[idx2], H5P_DEFAULT);
						if (!cbf_H5Ivalid(dset)) {
							error |= CBF_H5ERROR;
						} else {
							_CBF_CALL(cbf_H5Arequire_string(dset,"depends_on",key->axis.path[idx3]));
							is_processed[idx2] = 1;
						}
						cbf_H5Dfree(dset);
						if (is_processed[idx3]) break;
					} else {
						/*
						This *should* be the last axis in this chain,
						if it doesn't appear to be then complain and give up.
						TODO: try to extract extra axes from the CBF file.
						*/
						if (0) fprintf(stderr,"found a root: '%s' → '%s'\n",key->axis.axis_id[idx2],key->axis.depends_on[idx2]);
						if (!cbf_cistrcmp(key->axis.depends_on[idx2],".")) {
							hid_t dset = H5Dopen2(h5handle->hfile, key->axis.path[idx2], H5P_DEFAULT);
							if (!cbf_H5Ivalid(dset)) {
								error |= CBF_H5ERROR;
							} else {
								_CBF_CALL(cbf_H5Arequire_string(dset,"depends_on","."));
								is_processed[idx2] = 1;
							}
							cbf_H5Dfree(dset);
						} else {
							if (1) fprintf(stderr,"%s: Error: Unexpected axis dependency of '%s' on '%s'\n",
									__WHERE__,key->axis.depends_on[idx2],key->axis.axis_id[idx2]);
							error |= CBF_UNDEFINED;
						}
					}
				}
			}
			free((void*)is_processed);
		}

		return error;
	}

	static int cbf_write_cbf_h5file__link_h5data
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 const unsigned int scantype)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"link_h5data\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		/* ensure an entry/data group exists and has a link to the dataset */
		if (!cbf_H5Ivalid(h5handle->nxdata)) {
			_CBF_CALL(cbf_H5Grequire(h5handle->nxid,&h5handle->nxdata,"data"));
		}

		/* use it to link to data */
		if (CBF_SUCCESS==error && cbf_H5Ivalid(h5handle->nxdata)) {
			const htri_t data_exists = H5Lexists(h5handle->nxdata,"data",H5P_DEFAULT);
			const htri_t data_scaling_exists = H5Lexists(h5handle->nxdetector,"scaling_factor",H5P_DEFAULT);
			const htri_t data_offset_exists = H5Lexists(h5handle->nxdetector,"offset",H5P_DEFAULT);
			_CBF_CALL(cbf_H5Arequire_string(h5handle->nxdata,"NX_class","NXdata"));
			_CBF_CALL(cbf_H5Arequire_string(h5handle->nxdata,"signal","data"));
			if (data_exists < 0) {
				error |= CBF_H5ERROR;
			} else if (data_exists) {
				/* it exists - done */
			} else {
				H5Lcreate_hard(h5handle->nxdetector,"data",h5handle->nxdata,"data",H5P_DEFAULT,H5P_DEFAULT);
			}
			if (data_scaling_exists < 0) {
				error |= CBF_H5ERROR;
			} else if (data_scaling_exists) {
				const htri_t scaling_exists = H5Lexists(h5handle->nxdata,"scaling_factor",H5P_DEFAULT);
				if (scaling_exists < 0) {
					error |= CBF_H5ERROR;
				} else if(scaling_exists) {
					/* it exists - done */
				} else {
					H5Lcreate_hard(h5handle->nxdetector,"scaling_factor",
								   h5handle->nxdata,"scaling_factor",
								   H5P_DEFAULT,H5P_DEFAULT);
				}
			} else {
				/* I don't need to link to it - done */
			}
			if (data_offset_exists < 0) {
				error |= CBF_H5ERROR;
			} else if (data_offset_exists) {
				const htri_t offset_exists = H5Lexists(h5handle->nxdata,"offset",H5P_DEFAULT);
				if (offset_exists < 0) {
					error |= CBF_H5ERROR;
				} else if(offset_exists) {
					/* it exists - done */
				} else {
					H5Lcreate_hard(h5handle->nxdetector,"offset",
								   h5handle->nxdata,"offset",
								   H5P_DEFAULT,H5P_DEFAULT);
				}
			} else {
				/* I don't need to link to it - done */
			}

			/* extract some axes based on the scan type */
			if (CBF_SCANTYPE1 == scantype) {
				const htri_t axis_fast_exists = H5Lexists(h5handle->nxdetector,"fast_pixel_direction",H5P_DEFAULT);
				const htri_t axis_slow_exists = H5Lexists(h5handle->nxdetector,"slow_pixel_direction",H5P_DEFAULT);
				if (axis_fast_exists < 0) {
					error |= CBF_H5ERROR;
				} else if (axis_fast_exists) {
					const htri_t axis_exists = H5Lexists(h5handle->nxdata,"y",H5P_DEFAULT);
					if (axis_exists < 0) {
						error |= CBF_H5ERROR;
					} else if(axis_exists) {
						/* it exists - done */
					} else {
						H5Lcreate_hard(h5handle->nxdetector,"fast_pixel_direction",
									   h5handle->nxdata,"y",
									   H5P_DEFAULT,H5P_DEFAULT);
					}
				} else {
					/* I can't need to link to it - complain & fail */
					fprintf(stderr,"%s: Error: cannot find axis data\n",__WHERE__);
					error |= CBF_UNDEFINED;
				}
				if (axis_slow_exists < 0) {
					error |= CBF_H5ERROR;
				} else if (axis_slow_exists) {
					const htri_t axis_exists = H5Lexists(h5handle->nxdata,"x",H5P_DEFAULT);
					if (axis_exists < 0) {
						error |= CBF_H5ERROR;
					} else if(axis_exists) {
						/* it exists - done */
					} else {
						H5Lcreate_hard(h5handle->nxdetector,"slow_pixel_direction",
									   h5handle->nxdata,"x",
									   H5P_DEFAULT,H5P_DEFAULT);
					}
				} else {
					/* I can't need to link to it - complain & fail */
					fprintf(stderr,"%s: Error: cannot find axis data\n",__WHERE__);
					error |= CBF_UNDEFINED;
				}
				if (CBF_SUCCESS==error) { /* axes=[...] */
					hid_t h5atype = CBF_H5FAIL;
					const char axis0[] = "";
					const char axis1[] = "x";
					const char axis2[] = "y";
					const char * axes[] = {axis0,axis1,axis2};
					const hsize_t dim[] = {3};
					char * buf[3] = {0};
					_CBF_CALL(cbf_H5Tcreate_string(&h5atype,H5T_VARIABLE));
#ifdef CBF_USE_ULP
					_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5handle->nxdata,"axes",1,dim,h5atype,h5atype,axes,buf,cmp_vlstring,0));
#else
                    _CBF_CALL(cbf_H5Arequire_cmp2(h5handle->nxdata,"axes",1,dim,h5atype,h5atype,axes,buf,cmp_vlstring));
#endif
					cbf_H5Tfree(h5atype);
				}
				if (CBF_SUCCESS==error) { /* x_indices=1 */
					const int idx[] = {1};
					int buf[] = {0};
#ifdef CBF_USE_ULP
					_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5handle->nxdata,"x_indices",0,0,H5T_STD_I32LE,H5T_NATIVE_INT,idx,buf,cmp_int,0));
#else
					_CBF_CALL(cbf_H5Arequire_cmp2(h5handle->nxdata,"x_indices",0,0,H5T_STD_I32LE,H5T_NATIVE_INT,idx,buf,cmp_int));
#endif
				}
				if (CBF_SUCCESS==error) { /* y_indices=2 */
					const int idx[] = {2};
					int buf[] = {0};
#ifdef CBF_USE_ULP
					_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5handle->nxdata,"y_indices",0,0,H5T_STD_I32LE,H5T_NATIVE_INT,idx,buf,cmp_int,0));
#else
					_CBF_CALL(cbf_H5Arequire_cmp2(h5handle->nxdata,"y_indices",0,0,H5T_STD_I32LE,H5T_NATIVE_INT,idx,buf,cmp_int));
#endif
				}
			} else {
				fprintf(stderr,"%s: Error: unrecognised or multiple scan type(s)\n",__WHERE__);
			}
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_scan_frame
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;
#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_params;

		/* set up the comparison parameters */
		cmp_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		if (0) fprintf(stderr,"diffrn_scan_frame\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		if (CBF_SUCCESS==error) {
			hid_t dset = CBF_H5FAIL;
			const hid_t h5type = H5T_IEEE_F64LE;
			const char h5name[] = "count_time";
			const hsize_t max[] = {H5S_UNLIMITED};
			const hsize_t chunk[] = {1};
			const hsize_t offset[] = {h5handle->slice};
			const hsize_t count[] = {1};
			hsize_t buf[] = {0};
			const char * val = NULL;
			double num = 0./0.;
			_CBF_CALL(cbf_find_column(handle, "integration_time"));
			_CBF_CALL(cbf_get_value(handle, &val));
			num = strtod(val,0);
			_CBF_CALL(cbf_H5Drequire(h5handle->nxdetector,&dset,h5name,1,max,chunk,buf,h5type));
			_CBF_CALL(cbf_H5Dinsert(dset,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
			_CBF_CALL(cbf_H5Arequire_string(dset,"units","s"));
			cbf_H5Dfree(dset);
		}
		/*
		Convert 'date' column if it's in a parsable format, like "YYYY-MM-DDThh:mm:ss.s*"
		*/
		if (CBF_SUCCESS==error) {
			const char * val = NULL;
			_CBF_CALL(cbf_find_column(handle, "date"));
			_CBF_CALL(cbf_get_value(handle, &val));
			if (isDateTime(val)) {
				/* Convert any sortable dates that I have */
				int found = CBF_SUCCESS;
				hid_t type = CBF_H5FAIL;
				hid_t dataset = CBF_H5FAIL;
				if (0) fprintf(stderr,"Writing 'date' items\n");
				_CBF_CALL(cbf_H5Tcreate_string(&type,H5T_VARIABLE));
				found =  cbf_H5Dfind2(h5handle->nxid,&dataset,"start_time",0,0,0,type);
				if (CBF_SUCCESS==found) {
					/* comparison with existing data, to extract earliest time */
					/* time is in YYYY-MM-DDThh:mm:ss.s* format, should be able to use strcmp to order them */
					/* first read the existing timestamp */
					const hid_t currType = H5Dget_type(dataset);
					const char * buf = 0;
					hid_t currMemType = CBF_H5FAIL;
					cbf_H5Tcreate_string(&currMemType,H5T_VARIABLE);
					cbf_H5Dread2(dataset,0,0,0,&buf,currMemType);
					cbf_H5Tfree(currMemType);
					/* then compare them */
					if (strcmp(val,buf) < 0) {
						/* store the oldest timestamp */
						_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&val,type));
					}
					free((void*)buf);
					cbf_H5Tfree(currType);
				} else if (CBF_NOTFOUND==found) {
					/* create the dataset & write the data */
					_CBF_CALL(cbf_H5Dcreate(h5handle->nxid,&dataset,"start_time",0,0,0,0,type));
					_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&val,type));
				} else {
					error |= found;
					if (1) fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(found));
				}
				cbf_H5Dfree(dataset);
				found =  cbf_H5Dfind2(h5handle->nxid,&dataset,"end_time",0,0,0,type);
				if (CBF_SUCCESS==found) {
					/* comparison with existing data, to extract earliest time */
					/* time is in YYYY-MM-DDThh:mm:ss.s* format, should be able to use strcmp to order them */
					/* first read the existing timestamp */
					const hid_t currType = H5Dget_type(dataset);
					const char * buf = 0;
					hid_t currMemType = CBF_H5FAIL;
					cbf_H5Tcreate_string(&currMemType,H5T_VARIABLE);
					cbf_H5Dread2(dataset,0,0,0,&buf,currMemType);
					cbf_H5Tfree(currMemType);
					/* then compare them */
					if (strcmp(val,buf) > 0) {
						/* store the oldest timestamp */
						_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&val,type));
					}
					free((void*)buf);
					cbf_H5Tfree(currType);
				} else if (CBF_NOTFOUND==found) {
					/* create the dataset & write the data */
					_CBF_CALL(cbf_H5Dcreate(h5handle->nxid,&dataset,"end_time",0,0,0,0,type));
					_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&val,type));
				} else {
					error |= found;
					if (1) fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(found));
				}
				cbf_H5Dfree(dataset);
				cbf_H5Tfree(type);
			} else {
				/*
				If I can't sort dates I shouldn't write a random date:
				require no date, it may be added later by a user.
				Alternatively, use a flag to skip this check if using a 'template' entry group.
				TODO: Implement this flag.
				*/
				fprintf(stderr,"%s: Warning: %s\n",__WHERE__,"skipping 'date' items");
				error |= CBF_FORMAT;
			}
		}

		return error;
	}

	static int cbf_write_cbf_h5file__diffrn_data_frame
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 cbf_key_t * const key)
	{
		int error = CBF_SUCCESS;

		if (0) fprintf(stderr,"diffrn_data_frame\n");

		/* check arguments */
		if (!handle) {
			fprintf(stderr,"%s: Invalid handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!h5handle) {
			fprintf(stderr,"%s: Invalid hdf5 handle given\n",__WHERE__);
			return CBF_ARGUMENT;
		}
		if (!key || !key->frame) {
			fprintf(stderr,"%s: Bad key given\n",__WHERE__);
			return CBF_ARGUMENT;
		}

		_CBF_CALL(cbf_find_category(handle, "diffrn_data_frame"));
		_CBF_CALL(cbf_find_column(handle, "id"));
		_CBF_CALL(cbf_find_row(handle, key->frame));
		_CBF_CALL(cbf_count_rows(handle, &key->nFrames));
		/* extract the other ids */
		if (CBF_SUCCESS == error) {
			/* The array ID may be optional iff there is only 1 ID */
			const int fc = cbf_find_column(handle, "array_id");
			if (CBF_SUCCESS == fc) {
				_CBF_CALL(cbf_get_value(handle, &key->array));
			} else if (CBF_NOTFOUND == fc) {
				fprintf(stderr,"%s: Warning: %s\n",__WHERE__,"'diffrn_data_frame.array_id' not found");
				if (1 != key->nFrames) {
					error |= CBF_FORMAT;
				}
			} else {
				error |= fc;
				fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(fc));
			}
		}
		_CBF_CALL(cbf_find_column(handle, "binary_id"));
		_CBF_CALL(cbf_get_value(handle, &key->binary));
		_CBF_CALL(cbf_find_column(handle, "detector_element_id"));
		_CBF_CALL(cbf_get_value(handle, &key->diffrn_detector_element));

		return error;
	}

	/** \brief Extract the data from a CBF file & put it into a NeXus file.

	<p>Extracts data from <code>handle</code> and generates a NeXus file in <code>h5handle</code>. This can extract
	metadata and image data from a single scan within a single datablock and insert it into a given index into the
	NXentry group specified in <code>h5handle</code>. Support for multiple-scans-per-datablock and
	multiple-datablocks-per-file will be added at a later time.</p>

	<p>The flags (within <code>h5handle</code>) determine</p>
	<ul>
	<li>Compression algorithm: zlib/CBF/none</li>
	<li>Plugin registration method: automatic/manual</li>
	</ul>
	*/
	int cbf_write_cbf_h5file
			(cbf_handle handle,
			 cbf_h5handle h5handle)
	{
		cbf_node *node = NULL;
		int error = CBF_SUCCESS;
		cbf_key_t key[] = {_cbf_make_key()};

		hid_t detector = CBF_H5FAIL, instrument = CBF_H5FAIL; /* do not free */

		/* ensure the handle contains some basic structure */
		_CBF_CALL(cbf_h5handle_require_instrument(h5handle,&instrument));
		_CBF_CALL(cbf_h5handle_require_detector(h5handle,&detector,0));

		if (!handle || !h5handle) return CBF_ARGUMENT;

		/* prepare the CBF handle */
		_CBF_CALL(cbf_reset_refcounts(handle->dictionary));
		_CBF_CALL(cbf_find_parent(&node, handle->node, CBF_ROOT));
		_CBF_CALL(cbf_rewind_datablock(handle));

		while (CBF_SUCCESS==error) {
			unsigned int row_id = 0;
			unsigned int nScans = 0;
			const char * scan = 0;
			/*
			Get the first scan for now
			TODO: implement multi-scan datablocks
			*/
			_CBF_CALL(cbf_find_category(handle, "diffrn_scan"));
			_CBF_CALL(cbf_find_column(handle, "id"));
			_CBF_CALL(cbf_count_rows(handle,&nScans));
			if (0 == nScans) {
				/* if there are no scans avalable I can't convert anything */
				error |= CBF_FORMAT;
				fprintf(stderr,"%s: Error: %s\n",__WHERE__,"No scans found");
			}
			if (1 < nScans) {
				error |= CBF_NOTIMPLEMENTED;
				fprintf(stderr,"%s: Multiple scan support not yet implemented\n",__WHERE__);
			}
			_CBF_CALL(cbf_select_row(handle,0));
			_CBF_CALL(cbf_get_value(handle, &scan));
			{ /* if 'diffrn_scan.frames' exists then store the number of frames */
				const int fc = cbf_find_column(handle, "frames");
				if (CBF_SUCCESS == fc) {
					/* it exists, so read it */
					const char * value = NULL;
					_CBF_CALL(cbf_get_value(handle, &value));
					key->nFrames = strtoul(value,0,10);
				} else if (CBF_NOTFOUND == fc) {
					/* this is not a fatal error (or worth a warning), just carry on */
				} else {
					/* this *is* a fatal error */
					error |= fc;
					fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(fc));
				}
			}

			/* get the axis transformation matrix, which is constant within a datablock */
			_CBF_CALL(cbf_get_NX_axis_transform2(handle,key->matrix));

			/* loop though all frames related to this scan */
			while (CBF_SUCCESS == error) {
				_cbf_reset_key(key);
				key->nScans = nScans;
				/* select the row to work with, extract the 'scan' value */
				_CBF_CALL(cbf_find_category(handle, "diffrn_scan_frame"));
				_CBF_CALL(cbf_rewind_column(handle));
				/* Not being able to select the row is not always an error: I may have processed all rows already. */
				if (CBF_SUCCESS != cbf_select_row(handle,row_id)) break;
				if (CBF_SUCCESS == error) { /* allow 'diffrn_scan_frame.scan_id' to be skipped iff I have only one scan */
					const int fc = cbf_find_column(handle, "scan_id");
					if (CBF_SUCCESS == fc) {
						/* The column exists, so try to read a value from the selected row. */
						_CBF_CALL(cbf_get_value(handle, &key->scan));
					} else if (CBF_NOTFOUND == fc) {
						/* This might not be an error, the scan id may not be ambiguous */
						const char msg[]= "'diffrn_scan_frame.scan_id' not found";
						if (1 == key->nScans) {
							/* not ambiguous - allow it, but not quietly */
							fprintf(stderr,"%s: Warning: %s\n",__WHERE__,msg);
						} else {
							/* fail */
							error |= fc;
							fprintf(stderr,"%s: Error: %s\n",__WHERE__,msg);
						}
					} else {
						error |= fc;
						fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(fc));
					}
				}
				if (CBF_SUCCESS != error) break;
				/* convert the row if the obtained scan value matches the expected value, always go to the next row */
				if ((1 == key->nScans && !key->scan) || !cbf_cistrcmp(scan,key->scan)) {
					unsigned int structure_flag = 0;
					/*
					^ Note the kind of structure found in the image axes for this scan.
					This will determine how it will be converted later.
					*/
					_CBF_CALL(cbf_find_column(handle, "frame_id"));
					_CBF_CALL(cbf_get_value(handle, &key->frame));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_scan_frame(handle, h5handle, key));
					/* navigate to the frame data & extract some IDs */
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_data_frame(handle, h5handle, key));
					/* convert the metadata */
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_detector_element(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_detector(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_source(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_measurement(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_radiation(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_radiation_wavelength(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_detector_axis(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_measurement_axis(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__diffrn_refln(handle, h5handle, key));
					/* convert the data */
					_CBF_CALL(cbf_write_cbf_h5file__array_intensities(handle, h5handle, key));
					_CBF_CALL(cbf_find_category(handle, "array_data"));
					if (CBF_SUCCESS == error) {
						/* locate the row containing the relevant data */
						unsigned int row = 0;
						const cbf_node * cat = handle->node;
						const cbf_node * data = NULL;
						const cbf_node * arr_id = NULL;
						const cbf_node * bin_id = NULL;
						const int E_data = cbf_find_child((cbf_node**)(&data),cat,"data");
						const int E_arr = cbf_find_child((cbf_node**)(&arr_id),cat,"array_id");
						const int E_bin = cbf_find_child((cbf_node**)(&bin_id),cat,"binary_id");
						if (CBF_SUCCESS == E_arr) {
							/* don't need to do anything here */
						} else if (CBF_NOTFOUND == E_arr) {
							/* non-fatal error */
							fprintf(stderr,"%s: Warning: %s\n",__WHERE__,"'array_data.array_id' not found");
						} else {
							/* fail */
							error |= E_arr;
							fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(E_arr));
						}
						if (CBF_SUCCESS == E_bin) {
							/* don't need to do anything here */
						} else if (CBF_NOTFOUND == E_bin) {
							/* non-fatal error */
							fprintf(stderr,"%s: Warning: %s\n",__WHERE__,"'array_data.binary_id' not found");
						} else {
							/* fail */
							error |= E_bin;
							fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(E_bin));
						}
						if (CBF_SUCCESS == E_data) {
							const unsigned int count = data->children;
							if (arr_id && count!=arr_id->children) error |= CBF_FORMAT;
							if (bin_id && count!=bin_id->children) error |= CBF_FORMAT;
							for (row = 0; CBF_SUCCESS == error; ++row) {
								int arr_match, bin_match;
								if (arr_id) {
									const char * string = NULL;
									_CBF_CALL(_cbf_node_get_string(arr_id,row,&string));
									arr_match = cbf_cistrcmp(key->array,string) ? 0 : 1;
								} else {
									arr_match = (1 == count && 1 == key->nFrames) ? 1 : 0;
								}
								if (bin_id) {
									const char * string = NULL;
									_CBF_CALL(_cbf_node_get_string(bin_id,row,&string));
									bin_match = cbf_cistrcmp(key->binary,string) ? 0 : 1;
								} else {
									bin_match = (1 == count && 1 == key->nFrames) ? 1 : 0;
								}
								if (arr_match && bin_match) break;
							}
							if (count != row && CBF_SUCCESS == error) {
								/* I have some data to extract */
								_CBF_CALL(cbf_write_array_h5file(data,row,h5handle,key->data_overload,0));
							} else {
								/* failure */
								error |= CBF_FORMAT;
								fprintf(stderr,"%s: Error: could not locate data for current frame\n",__WHERE__);
							}
						} else {
							error |= E_data;
							fprintf(stderr,"%s: Error: cannot find 'array_data.data': %s\n",__WHERE__,cbf_strerror(E_data));
						}
					}
					_CBF_CALL(cbf_write_cbf_h5file__array_structure(handle, h5handle, key));
					structure_flag = cbf_check_array_structure_list_axis(handle);
					if (CBF_SCANTYPE1 == structure_flag) {
						_CBF_CALL(cbf_write_cbf_h5file__array_structure_list(handle, h5handle, key));
						_CBF_CALL(cbf_write_cbf_h5file__array_structure_list_axis(handle, h5handle, key,structure_flag));
					} else {
						fprintf(stderr,"%s: Error: unsupported scan geometry\n",__WHERE__);
					}
					_CBF_CALL(cbf_write_cbf_h5file__axis_dependency_chain(handle, h5handle, key));
					_CBF_CALL(cbf_write_cbf_h5file__link_h5data(handle, h5handle, structure_flag));
					++h5handle->slice;
				}
				++row_id;
			}
			if (CBF_SUCCESS != cbf_next_datablock(handle)) break;
		}

		_cbf_free_key(*key);
		return error;
	}

	/** \brief Extract the data from a miniCBF file & put it into a NeXus file.

	<p>Extracts the miniCBF data directly - by parsing the header - and uses that plus the configuration options from
	<code>axisConfig</code> to generate a NeXus file in <code>h5handle</code>. This can extract metadata and image
	data from miniCBF files containing multiple datablocks which each contain a single image and insert it into a
	given index into the NXentry group specified in <code>h5handle</code>.</p>

	<p>Currently, only <code>Pilatus 1.2</code> format headers are supported.</p>

	<p>The flags determine</p>
	<ul>
		<li>Compression algorithm: zlib/CBF/none</li>
		<li>Plugin registration method: automatic/manual</li>
	</ul>
	*/
	int cbf_write_minicbf_h5file
		(cbf_handle handle,
		 cbf_h5handle h5handle,
		 const cbf_config_t * const axisConfig)
	{
		int error = CBF_SUCCESS;
		cbf_node *node = NULL;
		const char * saturation_value = NULL;

#ifdef CBF_USE_ULP
		cmp_double_param_t cmp_double_params;

		/* set up the comparison parameters */
		cmp_double_params.cmp_double_as_float = cbf_has_ULP64() ? h5handle->cmp_double_as_float : 1;
		cmp_double_params.ulp32 = h5handle->float_ulp;
#ifndef NO_UINT64_TYPE
		cmp_double_params.ulp64 = h5handle->double_ulp;
#endif
#endif

		hid_t detector = CBF_H5FAIL, instrument = CBF_H5FAIL; /* do not free */

		if (!handle || !h5handle) return CBF_ARGUMENT;


		/* Find the root node */
		cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT));

		/* Reset the reference counts */
		cbf_failnez( cbf_reset_refcounts(handle->dictionary) );

		/* ensure the handle contains some basic structure */
		cbf_reportnez(cbf_h5handle_require_instrument(h5handle,&instrument), error);
		cbf_reportnez(cbf_h5handle_require_detector(h5handle,&detector,0), error);

		/* Do the mappings from CBF to nexus */
		cbf_onfailnez(error |= cbf_rewind_datablock(handle), fprintf(stderr,__WHERE__": CBF error: cannot find datablock.\n"));
		while (CBF_SUCCESS==error) {
			/* get some useful parameters out of the metadata as it's converted */
			double pixel_x = 0./0., pixel_y = 0./0.;

			/* then search for the 'array_data' category */
			cbf_onfailnez(cbf_find_category(handle,"array_data"),
						  fprintf(stderr,__WHERE__": CBF error: cannot find category 'array_data'.\n"));

			/* First: extract the metadata from the CBF, put it in nexus */
			cbf_failnez(cbf_find_column(handle,"header_convention"));
			if (1) { /* get the header convention, check it is a value I understand */
				const char * value = NULL;
				const char vendor_pilatus[] = "PILATUS";
				cbf_failnez(cbf_get_value(handle,&value));
				if (0 == strncmp(value,vendor_pilatus,strlen(vendor_pilatus))) {
					const char version_1_2[] = "1.2";
					value += strlen(vendor_pilatus) + 1;
					if (0 == strncmp(value,version_1_2,strlen(version_1_2))) {
						/* define tokenisation variables, with default buffer sized to almost always be big enough */
						size_t n = 128;
						char * token = malloc(n*sizeof(char));
						int newline = 1;
						/* Numerical values for use after main parsing loop */
						double beam_x = 0./0., beam_y = 0./0.;
						double detector_distance = 0./0.;
						/* Flags to determine what information I actually have */
						/* Other useful values */
						hid_t pilatusDiagnostics = CBF_H5FAIL; /* <- non-nexus group to dump some possibly useful information into */
						/* Get the header data */
						if (0) fprintf(stderr,__WHERE__": %s_%s header found.\n",vendor_pilatus,version_1_2);
						cbf_onfailnez(cbf_find_column(handle,"header_contents"),
									  fprintf(stderr,__WHERE__": 'header_contents' not found.\n"));
						/* re-use the 'value' variable, I won't need the old value anymore */
						cbf_onfailnez(cbf_get_value(handle,&value), fprintf(stderr,__WHERE__": 'header_contents' inaccessible.\n"));
                        cbf_H5Drequire_flstring(detector,0,"type","pixel array");

						if (0) {
							const char * _value = value;
							do {
								_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &_value);
								if (!token) break;
								newline = !strcmp("\n",token);
								printf("token: %s\n", token);
							} while (1);
						}
						if (0) fprintf(stderr,"%s: header:\n%s\n",__WHERE__,value);

						/*
						Do the mapping, iterating over each line of the header.
						The entire header can be parsed using a trivial FSA, so don't bother with anything particularly complex.
						*/
						do {
							int noMatch = 0;
							{ /* Get the first token */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								if (!token) break;
								if (!strcmp("\n",token)) continue;
							}
							/* check for a time */
							if (isDateTime(token)) {
								int found = CBF_SUCCESS;
								/* Put the time string into the hdf5 file. */
								hid_t type = CBF_H5FAIL;
								hid_t dataset = CBF_H5FAIL;
								_CBF_CALL(cbf_H5Tcreate_string(&type,H5T_VARIABLE));
								found =  cbf_H5Dfind2(h5handle->nxid,&dataset,"start_time",0,0,0,type);
								if (CBF_SUCCESS==found) {
									/* comparison with existing data, to extract earliest time */
									/* time is in YYYY-MM-DDThh:mm:ss.s* format, should be able to use strcmp to order them */
									/* first read the existing timestamp */
									const hid_t currType = H5Dget_type(dataset);
									const char * buf = 0;
									hid_t currMemType = CBF_H5FAIL;
									cbf_H5Tcreate_string(&currMemType,H5T_VARIABLE);
									cbf_H5Dread2(dataset,0,0,0,&buf,currMemType);
									cbf_H5Tfree(currMemType);
									/* then compare them */
									if (strcmp(token,buf) < 0) {
										/* store the oldest timestamp */
										_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&token,type));
									}
									free((void*)buf);
									cbf_H5Tfree(currType);
								} else if (CBF_NOTFOUND==found) {
									/* create the dataset & write the data */
									_CBF_CALL(cbf_H5Dcreate(h5handle->nxid,&dataset,"start_time",0,0,0,0,type));
									_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&token,type));
								} else {
									error |= found;
									fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(found));
								}
								cbf_H5Dfree(dataset);
								found =  cbf_H5Dfind2(h5handle->nxid,&dataset,"end_time",0,0,0,type);
								if (CBF_SUCCESS==found) {
									/* comparison with existing data, to extract latest time */
									/* time is in YYYY-MM-DDThh:mm:ss.s* format, should be able to use strcmp to order them */
									/* first read the existing timestamp */
									const hid_t currType = H5Dget_type(dataset);
									const char * buf = 0;
									hid_t currMemType = CBF_H5FAIL;
									cbf_H5Tcreate_string(&currMemType,H5T_VARIABLE);
									cbf_H5Dread2(dataset,0,0,0,&buf,currMemType);
									cbf_H5Tfree(currMemType);
									/* then compare them */
									if (strcmp(token,buf) > 0) {
										/* store the oldest timestamp */
										_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&token,type));
									}
									free((void*)buf);
									cbf_H5Tfree(currType);
								} else if (CBF_NOTFOUND==found) {
									/* create the dataset & write the data */
									_CBF_CALL(cbf_H5Dcreate(h5handle->nxid,&dataset,"end_time",0,0,0,0,type));
									_CBF_CALL(cbf_H5Dwrite2(dataset,0,0,0,&token,type));
								} else {
									error |= found;
									fprintf(stderr,"%s: Error: %s\n",__WHERE__,cbf_strerror(found));
								}
								cbf_H5Dfree(dataset);
								cbf_H5Tfree(type);
							} else if (!cbf_cistrcmp("Pixel_size",token)) {
								const char errstr[] = "expected units of 'm' for 'Pixel_size'";
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								{ /* Get x pixel size */
									hid_t h5data = CBF_H5FAIL;
									const double num = strtod(token,0);
#ifdef CBF_USE_ULP
									_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector,&h5data,"x_pixel_size",num,cmp_double,&cmp_double_params));
#else
                                    _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector,&h5data,"x_pixel_size",num,cmp_double));
#endif
									_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
									if (cbf_cistrcmp(token,"m")) {
										error |= CBF_H5DIFFERENT;
										fprintf(stderr,"%s: Error: %s\n",__WHERE__,errstr);
									} else {
										pixel_x = num;
									}
									_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
									cbf_H5Dfree(h5data);
								}
								/* Get next useful token, just skip over useless stuff */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								{ /* Get y pixel size */
									hid_t h5data = CBF_H5FAIL;
									const double num = strtod(token,0);
#ifdef CBF_USE_ULP
									_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector,&h5data,"y_pixel_size",num,cmp_double,&cmp_double_params));
#else
                                    _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector,&h5data,"y_pixel_size",num,cmp_double));
#endif
									_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
									if (cbf_cistrcmp(token,"m")) {
										error |= CBF_H5DIFFERENT;
										fprintf(stderr,"%s: Error: %s\n",__WHERE__,errstr);
									} else {
										pixel_y = num;
									}
									_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
									cbf_H5Dfree(h5data);
								}
							} else if (!cbf_cistrcmp("Silicon",token)) {
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								if (!strcmp("sensor",token)) {
									_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
									if (!strcmp("thickness",token)) {
										_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"sensor_material","Silicon"));
										_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
										{
											hid_t h5data = CBF_H5FAIL;
											double num = strtod(token,0);
#ifdef CBF_USE_ULP
											_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector,&h5data,"sensor_thickness",
                                                                                  num,cmp_double,&cmp_double_params));
#else
                                            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector,&h5data,"sensor_thickness",
                                                                                  num,cmp_double));
#endif
											_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
											if (cbf_cistrcmp(token,"m")) {
												error |= CBF_H5DIFFERENT;
												fprintf(stderr,"%s: Error: %s\n",__WHERE__,
														"expected units of 'm' for 'Silicon sensor thickness'");
											}
											_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
											cbf_H5Dfree(h5data);
										}
									} else noMatch = 1;
								} else noMatch = 1;
							} else if (!cbf_cistrcmp("Detector_distance",token)) {
								/* Get value & units */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								detector_distance = strtod(token,0);
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								if (!cbf_cistrcmp(token,"m")) {
									hid_t h5data = CBF_H5FAIL;
									const hsize_t max[] = {H5S_UNLIMITED};
									const hsize_t cnk[] = {1};
									const hsize_t off[] = {h5handle->slice};
									const hsize_t cnt[] = {1};
									hsize_t buf[] = {0};
									_CBF_CALL(cbf_H5Drequire(detector,&h5data,"distance",1,max,cnk,buf,H5T_IEEE_F64LE));
									_CBF_CALL(cbf_H5Dinsert(h5data,off,0,cnt,buf,&detector_distance,H5T_NATIVE_DOUBLE));
									_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
									cbf_H5Dfree(h5data);
								} else {
									error |= CBF_H5DIFFERENT;
									fprintf(stderr,"%s: Error: %s\n",__WHERE__,"expected units of 'm' for 'Detector_distance'");
								}
							} else if (!cbf_cistrcmp("Detector",token)) {
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value));
								_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"description",token));
							} else if (!cbf_cistrcmp("N_excluded_pixels",token)) {
								hid_t h5location = CBF_H5FAIL;
								_CBF_CALL(cbf_H5Grequire(detector,&h5location,"pilatus_diagnostics"));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value));
								_CBF_CALL(cbf_H5Drequire_flstring(h5location,0,"N_excluded_pixels",token));
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Excluded_pixels",token)) {
								hid_t h5location = CBF_H5FAIL;
								_CBF_CALL(cbf_H5Grequire(detector,&h5location,"pilatus_diagnostics"));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value));
								_CBF_CALL(cbf_H5Drequire_flstring(h5location,0,"Excluded_pixels",token));
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Flat_field",token)) {
								hid_t h5location = CBF_H5FAIL;
								_CBF_CALL(cbf_H5Grequire(detector,&h5location,"pilatus_diagnostics"));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value));
								_CBF_CALL(cbf_H5Drequire_flstring(h5location,0,"Flat_field",token));
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Trim_file",token)) {
								hid_t h5location = CBF_H5FAIL;
								_CBF_CALL(cbf_H5Grequire(detector,&h5location,"pilatus_diagnostics"));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value));
								_CBF_CALL(cbf_H5Drequire_flstring(h5location,0,"Trim_file",token));
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Exposure_time",token)) {
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "count_time";
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								hsize_t buf[] = {0};
								double num = 0./0.;
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num = strtod(token,0);
								_CBF_CALL(cbf_H5Drequire(h5location,&h5data,h5name,1,max,chunk,buf,h5type));
								_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Exposure_period",token)) {
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "frame_time";
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								hsize_t buf[] = {0};
								double num = 0./0.;
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num = strtod(token,0);
								_CBF_CALL(cbf_H5Drequire(h5location,&h5data,h5name,1,max,chunk,buf,h5type));
								_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Tau",token)) {
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = detector;
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "dead_time";
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								hsize_t buf[] = {0};
								double num = 0./0.;
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num = strtod(token,0);
								_CBF_CALL(cbf_H5Drequire(h5location,&h5data,h5name,1,max,chunk,buf,h5type));
								_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Count_cutoff",token)) {
								/* store the string for later interpretation */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								if (CBF_SUCCESS==error) {
									saturation_value = _cbf_strdup(token);
								}
							} else if (!cbf_cistrcmp("Threshold_setting",token)) {
								double num = 0./0.;
								hid_t h5data = CBF_H5FAIL;
								/* Get value & units */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num = strtod(token,0);
#ifdef CBF_USE_ULP
								_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector,&h5data,"threshold_energy",
                                                                      num,cmp_double,&cmp_double_params));
#else
 								_CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector,&h5data,"threshold_energy",
                                                                      num,cmp_double));
#endif
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Gain_setting",token)) {
								int error = CBF_SUCCESS;
								/* [1,end): gain setting string */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 1, &value));
								_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"gain_setting",token));
							} else if (!cbf_cistrcmp("Wavelength",token)) {
								/* Get value & units */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5location = CBF_H5FAIL; /* DO NOT FREE THIS! */
								const hid_t h5type = H5T_IEEE_F64LE;
								const char h5name[] = "wavelength";
								const hsize_t max[] = {H5S_UNLIMITED};
								const hsize_t chunk[] = {1};
								const hsize_t offset[] = {h5handle->slice};
								const hsize_t count[] = {1};
								hsize_t buf[] = {0};
								double num = 0./0.;
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num = strtod(token,0);
								cbf_h5handle_require_monochromator(h5handle, &h5location);
								_CBF_CALL(cbf_H5Drequire(h5location,&h5data,h5name,1,max,chunk,buf,h5type));
								_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE));
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units",token));
								cbf_H5Dfree(h5data);
							} else if (!cbf_cistrcmp("Beam_xy",token)) {
								/*
								Extract x & y positions from the header, put them into the file later.
								I might need to read all the header to know if I can actually convert these values to NeXus data.
								*/
								double num_x = 0./0., num_y = 0./0.;
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num_x = strtod(token,0);
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num_y = strtod(token,0);
								/* Extract the units (should be pixels, but I don't want to lose important information if it isn't) */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								if (cbf_cistrcmp(token,"pixels")) {
									error |= CBF_H5DIFFERENT;
									fprintf(stderr,"%s: Error: %s\n",__WHERE__,"expected units of 'pixels' for 'Beam_xy'");
								} else {
									beam_x = num_x;
									beam_y = num_y;
								}
							} else if (!cbf_cistrcmp("Flux",token)) {
								/*
								Either a number with some units or a random string, only do anything if it's a number.
								*/
								const char * end = 0;
								double num = 0./0.;
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								num = strtod(token,(char**)(&end));
								if (end != token && 0.0 != num) {
									/* I have a valid & useful flux, map it to /entry/sample/beam/flux */
									hid_t sample = CBF_H5FAIL; /* DO NOT FREE THIS! */
									hid_t h5data = CBF_H5FAIL;
									hid_t h5location = CBF_H5FAIL;
									/* Ensure I have a valid sample group */
									_CBF_CALL(cbf_h5handle_require_sample(h5handle, &sample));
									/* Ensure I have a valid beam group */
									_CBF_CALL(cbf_H5Grequire(sample,&h5location,"beam"));
									_CBF_CALL(cbf_H5Arequire_string(h5location, "NX_class", "NXbeam"));
									/* Store value & units */
#ifdef CBF_USE_ULP
									_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(h5location, &h5data, "flux", num, cmp_double,&cmp_double_params));
#else
                                    _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(h5location, &h5data, "flux", num, cmp_double));
#endif
									_CBF_CALL(cbf_H5Arequire_string(h5data, "units", "s-1"));
									/* cleanup temporary dataset */
									cbf_H5Dfree(h5data);
									cbf_H5Gfree(h5location);
								}
							} else if (!cbf_cistrcmp("Filter_transmission",token)) {
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								/* write it to the file */
								if (cbf_H5Ivalid(instrument)) {
									/* Get value */
									const double num = strtod(token,0);
									hid_t h5location = CBF_H5FAIL;
									_CBF_CALL(cbf_H5Grequire(instrument,&h5location,"attenuator"));
									_CBF_CALL(cbf_H5Arequire_string(h5location, "NX_class", "NXattenuator"));
									/* Get value & units */
#ifdef CBF_USE_ULP
									_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(h5location, 0, "attenuator_transmission",
                                                                        num, cmp_double,&cmp_double_params));
#else
                                    _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(h5location, 0, "attenuator_transmission",
                                                                        num, cmp_double));
#endif
									/* cleanup temporary dataset */
									cbf_H5Gfree(h5location);
								}
							} else if (!cbf_cistrcmp("Polarization",token)) {
								hid_t sample = CBF_H5FAIL; /* DO NOT FREE THIS! */
								hid_t h5location = CBF_H5FAIL;
								/* Ensure I have a valid sample group */
								_CBF_CALL(cbf_h5handle_require_sample(h5handle, &sample));
								/* Ensure I have a valid beam group */
								_CBF_CALL(cbf_H5Grequire(sample,&h5location,"beam"));
								_CBF_CALL(cbf_H5Arequire_string(h5location, "NX_class", "NXbeam"));
								/* Get value & units */
								_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value));
								if (CBF_SUCCESS==error) {
									/* extract value from header */
									const double p = strtod(token, 0);
									/* convert to nexus format */
									const double polarisation[] = {1.0, p, 0.0, 0.0};
									const hsize_t max[] = {H5S_UNLIMITED,4};
									const hsize_t chunk[] = {1,4};
									const hsize_t offset[] = {h5handle->slice,0};
									const hsize_t count[] = {1,4};
									hsize_t buf[] = {0,0};
									hid_t h5data = CBF_H5FAIL;
									hid_t h5type = H5T_IEEE_F64LE;
									const char h5name[] = "incident_polarisation_stokes";
									_CBF_CALL(cbf_H5Drequire(h5location,&h5data,h5name,2,max,chunk,buf,h5type));
									_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,polarisation,H5T_NATIVE_DOUBLE));
									cbf_H5Dfree(h5data);
								}
								cbf_H5Gfree(h5location);
							} else if (!cbf_cistrcmp("Alpha",token)) {
#ifdef CBF_USE_ULP
#define _CBF_CONVERT_AXIS(axisName) \
do { \
	/* Find the data for the current axis, or fail */ \
	cbf_configItem_t * const axisItem = cbf_config_findMinicbf(axisConfig, axisName); \
	_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value)); \
	if (cbf_config_end(axisConfig) == axisItem) { \
		error |= CBF_NOTFOUND; \
		fprintf(stderr,"Config settings for axis '%s' could not be found: " \
				"this will eventually be a fatal error\n", axisName); \
							} else if (axisItem->convert) { \
		hid_t h5data = CBF_H5FAIL; \
		hid_t sample = CBF_H5FAIL; \
		hid_t axes = CBF_H5FAIL; \
		const hsize_t max[] = {H5S_UNLIMITED}; \
		const hsize_t chunk[] = {1}; \
		const hsize_t offset[] = {h5handle->slice}; \
		const hsize_t count[] = {1}; \
		hsize_t buf[] = {0}; \
		const double num = strtod(token,0); \
		_CBF_CALL(cbf_h5handle_require_sample(h5handle, &sample)); \
		_CBF_CALL(cbf_H5Grequire(sample,&axes,"pose")); \
		_CBF_CALL(cbf_H5Arequire_string(axes,"NX_class","NXcollection")); \
		_CBF_CALL(cbf_H5Drequire(axes,&h5data,axisItem->nexus,1,max,chunk,buf,H5T_IEEE_F64LE)); \
		_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE)); \
		_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value)); \
		if (axisItem->depends_on) { \
			const char dot[] = "."; \
			if (strcmp(axisItem->depends_on,dot)) { \
				const char path_empty[] = ""; \
				const char path_sample[] = "sample"; \
				const char axis_group_name[] = "pose"; \
				const char * path_parts[] = { \
					path_empty, \
					h5handle->nxid_name, \
					path_sample, \
					axis_group_name, \
					axisItem->depends_on, \
					0 \
							}; \
				const char * const axis_path = _cbf_strjoin(path_parts,'/'); \
				_CBF_CALL(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axis_path,axisItem,cmp_double ,&cmp_double_params )); \
				free((void*)axis_path); \
							} else { \
				_CBF_CALL(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,dot,axisItem,cmp_double,&cmp_double_params)); \
							} \
							} else { \
			_CBF_CALL(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,"",axisItem,cmp_double,&cmp_double_params)); \
							} \
		cbf_H5Dfree(h5data); \
		cbf_H5Gfree(axes); \
							} \
} while (0)
#else
#define _CBF_CONVERT_AXIS(axisName) \
do { \
	/* Find the data for the current axis, or fail */ \
	cbf_configItem_t * const axisItem = cbf_config_findMinicbf(axisConfig, axisName); \
	_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value)); \
	if (cbf_config_end(axisConfig) == axisItem) { \
		error |= CBF_NOTFOUND; \
		fprintf(stderr,"Config settings for axis '%s' could not be found: " \
				"this will eventually be a fatal error\n", axisName); \
							} else if (axisItem->convert) { \
		hid_t h5data = CBF_H5FAIL; \
		hid_t sample = CBF_H5FAIL; \
		hid_t axes = CBF_H5FAIL; \
		const hsize_t max[] = {H5S_UNLIMITED}; \
		const hsize_t chunk[] = {1}; \
		const hsize_t offset[] = {h5handle->slice}; \
		const hsize_t count[] = {1}; \
		hsize_t buf[] = {0}; \
		const double num = strtod(token,0); \
		_CBF_CALL(cbf_h5handle_require_sample(h5handle, &sample)); \
		_CBF_CALL(cbf_H5Grequire(sample,&axes,"pose")); \
		_CBF_CALL(cbf_H5Arequire_string(axes,"NX_class","NXcollection")); \
		_CBF_CALL(cbf_H5Drequire(axes,&h5data,axisItem->nexus,1,max,chunk,buf,H5T_IEEE_F64LE)); \
		_CBF_CALL(cbf_H5Dinsert(h5data,offset,0,count,buf,&num,H5T_NATIVE_DOUBLE)); \
		_CBF_CALL(_cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value)); \
		if (axisItem->depends_on) { \
			const char dot[] = "."; \
			if (strcmp(axisItem->depends_on,dot)) { \
				const char path_empty[] = ""; \
				const char path_sample[] = "sample"; \
				const char axis_group_name[] = "pose"; \
				const char * path_parts[] = { \
					path_empty, \
					h5handle->nxid_name, \
					path_sample, \
					axis_group_name, \
					axisItem->depends_on, \
					0 \
							}; \
				const char * const axis_path = _cbf_strjoin(path_parts,'/'); \
				_CBF_CALL(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,axis_path,axisItem,cmp_double )); \
				free((void*)axis_path); \
							} else { \
				_CBF_CALL(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,dot,axisItem,cmp_double)); \
							} \
							} else { \
			_CBF_CALL(_cbf_pilatusAxis2nexusAxisAttrs(h5data,token,"",axisItem,cmp_double)); \
							} \
		cbf_H5Dfree(h5data); \
		cbf_H5Gfree(axes); \
							} \
} while (0)
#endif
								_CBF_CONVERT_AXIS("Alpha");
							} else if (!cbf_cistrcmp("Kappa",token)) {
								_CBF_CONVERT_AXIS("Kappa");
							} else if (!cbf_cistrcmp("Phi",token)) {
								_CBF_CONVERT_AXIS("Phi");
							} else if (!cbf_cistrcmp("Chi",token)) {
								_CBF_CONVERT_AXIS("Chi");
							} else if (!cbf_cistrcmp("Omega",token)) {
								_CBF_CONVERT_AXIS("Omega");
							} else if (!cbf_cistrcmp("Start_angle",token)) {
								_CBF_CONVERT_AXIS("Start_angle");
							} else if (!cbf_cistrcmp("Detector_2theta",token)) {
								_CBF_CONVERT_AXIS("Detector_2theta");
							} else if (!cbf_cistrcmp("Image_path",token)) {
							} else if (!cbf_cistrcmp("Angle_increment",token)) {
							} else noMatch = 1;
							if (noMatch) {
								fprintf(stderr,"%s: error: Could not match entry in pilatus header: '%s'\n",__WHERE__,token);
							}
							/* Done matching the entry from this line, go on to the next one */
							while (token && strcmp("\n",token)) {
								error = _cbf_scan_pilatus_V1_2_miniheader(&token, &n, &newline, 0, &value);
								if (CBF_SUCCESS != error) fprintf(stderr,"%s: error: %s\n",__WHERE__,cbf_strerror(error));
							}
						} while (1);
						free(token);

						{
							/*
							Record the axis that the sample depends on
							*/
                            const char * const depends_on = cbf_config_getSampleDependsOn(axisConfig);
							if (depends_on) {
								hid_t h5location = CBF_H5FAIL; /* DO NOT FREE THIS */
								const char path_empty[] = "";
								const char path_sample[] = "sample";
								const char axis_group_name[] = "pose";
								const char * path_parts[] = {
									path_empty,
									h5handle->nxid_name,
									path_sample,
									axis_group_name,
									depends_on,
									0
								};
								const char * const axis_path = _cbf_strjoin(path_parts,'/');
								_CBF_CALL(cbf_h5handle_require_sample(h5handle, &h5location));
								_CBF_CALL(cbf_H5Drequire_flstring(h5location,0,"depends_on",axis_path));
								free((void*)axis_path);
							} else {
								fprintf(stderr,"Config settings for 'Sample' could not be found: "
										"this will eventually be a fatal error\n");
							}
							if (0) fprintf(stderr, __WHERE__ ": 'sample/depends_on' written\n");
						}
						{ /* write beam_center_x */
							hid_t h5data = CBF_H5FAIL;
#ifdef CBF_USE_ULP
							_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector, &h5data, "beam_center_x",
                                                                  beam_x*pixel_x, cmp_double, &cmp_double_params));
#else
							_CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector, &h5data, "beam_center_x",
                                                                  beam_x*pixel_x, cmp_double));
#endif
							_CBF_CALL(cbf_H5Arequire_string(h5data,"units","m"));
							cbf_H5Dfree(h5data);
						}
						{ /* write beam_center_y */
							hid_t h5data = CBF_H5FAIL;
#ifdef CBF_USE_ULP
							_CBF_CALL(cbf_H5Drequire_scalar_F64LE2_ULP(detector, &h5data, "beam_center_y",
                                                            beam_y*pixel_y, cmp_double, &cmp_double_params));
#else
                            _CBF_CALL(cbf_H5Drequire_scalar_F64LE2(detector, &h5data, "beam_center_y",
                                                                  beam_y*pixel_y, cmp_double));
#endif
							_CBF_CALL(cbf_H5Arequire_string(h5data,"units","m"));
							cbf_H5Dfree(h5data);
						}
						{
							/* Detector axes
							Requires:
							* beam_center_x
							* beam_center_y
							* detector_distance
							Creates dependancy chain:
							detector -> rotation -> translation -> .
							*/
							/* TODO: write depends_on attributes with full paths */
							/* Common settings */
							const hsize_t vdims[] = {3};
                            double vbuf[3] = {0./0.};
							const hsize_t max[] = {H5S_UNLIMITED};
							hsize_t buf[] = {0};
							const hsize_t chunk[] = {1};
							const hsize_t h5offset[] = {h5handle->slice};
							const hsize_t h5count[] = {1};
							hid_t h5location = detector;
							hid_t axis_group = CBF_H5FAIL;
							const char * axis_translation = "translation";
							const char * axis_rotation = "rotation";
							const char path_empty[] = "";
							const char path_inst[] = "instrument";
							const char axis_group_name[] = "pose";
							_CBF_CALL(cbf_H5Grequire(h5location,&axis_group,axis_group_name));
							_CBF_CALL(cbf_H5Arequire_string(axis_group,"NX_class","NXcollection"));
							{ /* Translation-specific */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5type = H5T_IEEE_F64LE;
								const double num = detector_distance;
								const double vector[] = {0.0, 0.0, 1.0};
								const double offset[] = {-beam_x*pixel_x, -beam_y*pixel_y, 0.0};
								_CBF_CALL(cbf_H5Drequire(axis_group,&h5data,axis_translation,1,max,chunk,buf,h5type));
								_CBF_CALL(cbf_H5Dinsert(h5data,h5offset,0,h5count,buf,&num,H5T_NATIVE_DOUBLE));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units","m"));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"offset_units","m"));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"transformation_type","translation"));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"depends_on","."));
#ifdef CBF_USE_ULP
								_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5data,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,vector,
														vbuf,cmp_double,&cmp_double_params));
								_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5data,"offset",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,offset,
														vbuf,cmp_double,&cmp_double_params));
#else
								_CBF_CALL(cbf_H5Arequire_cmp2(h5data,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,vector,
														vbuf,cmp_double));
								_CBF_CALL(cbf_H5Arequire_cmp2(h5data,"offset",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,offset,
														vbuf,cmp_double));
#endif
								/* cleanup temporary datasets */
								cbf_H5Dfree(h5data);
								if (0) fprintf(stderr, __WHERE__ ": 'detector/%s' written\n",axis_translation);
							}
							{ /* Rotation-specific */
								hid_t h5data = CBF_H5FAIL;
								hid_t h5type = H5T_IEEE_F64LE;
								const double num = 180.0;
								const double vector[] = {0.0, 0.0, 1.0};
								const char * path_parts[] = {
									path_empty,
									h5handle->nxid_name,
									path_inst,
									h5handle->nxdetector_name,
									axis_group_name,
									axis_translation,
									0
								};
								const char * const axis_path = _cbf_strjoin(path_parts,'/');
								_CBF_CALL(cbf_H5Drequire(axis_group,&h5data,axis_rotation,1,max,chunk,buf,h5type));
								_CBF_CALL(cbf_H5Dinsert(h5data,h5offset,0,h5count,buf,&num,H5T_NATIVE_DOUBLE));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"units","deg"));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"transformation_type","rotation"));
								_CBF_CALL(cbf_H5Arequire_string(h5data,"depends_on",axis_path));
#ifdef CBF_USE_ULP
								_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5data,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
														vector, vbuf,cmp_double,&cmp_double_params));
#else
								_CBF_CALL(cbf_H5Arequire_cmp2(h5data,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
														vector, vbuf,cmp_double));
#endif
								/* cleanup temporary datasets */
								cbf_H5Dfree(h5data);
								free((void*)axis_path);
								if (0) fprintf(stderr, __WHERE__ ": 'detector/%s' written\n",axis_rotation);
							}
							cbf_H5Gfree(axis_group);
							{ /* tie everything to the detector */
								const char * path_parts[] = {
									path_empty,
									h5handle->nxid_name,
									path_inst,
									h5handle->nxdetector_name,
									axis_group_name,
									axis_rotation,
									0
								};
								const char * const axis_path = _cbf_strjoin(path_parts,'/');
								_CBF_CALL(cbf_H5Drequire_flstring(detector,0,"depends_on",axis_path));
								free((void*)axis_path);
							}
						}
						cbf_H5Gfree(pilatusDiagnostics);
					}
				}
			}
			/* Second: extract the raw data from the CBF, put it in nexus */
			cbf_onfailnez(cbf_find_column(handle,"data"), fprintf(stderr,__WHERE__": CBF error: cannot find column `data'.\n"));
			/* get the first row, TODO: convert every row */
			cbf_onfailnez(cbf_select_row(handle,0), fprintf(stderr,__WHERE__": CBF error: cannot find row 0.\n"));
			if(1) {
				hsize_t h5dim[] = {0, 0, 0};
				/* convert the data */
				_CBF_CALL(cbf_write_array_h5file(handle->node, handle->row, h5handle, saturation_value, h5dim));
				/* ensure I have an axis for each index of the image - mapping pixel indices to spatial coordinates */
				if (1) {
					hid_t h5axis = CBF_H5FAIL;
					const char h5name[] = "slow_pixel_direction";
					hsize_t buf[] = {0};
					const int found =  cbf_H5Dfind2(detector,&h5axis,h5name,1,h5dim+1,buf,H5T_IEEE_F64LE);
					const hsize_t offset[] = {0};
					const hsize_t * count = h5dim+1;
					double * const expected_data = malloc((*count) * sizeof(double));
					{
						hsize_t n = 0;
						for (n=0; n!=h5dim[1]; ++n)
							expected_data[n] = (double)(n)*pixel_x;
					}
					if (CBF_SUCCESS==found) {
						double * const actual_data = malloc((*count) * sizeof(double));
						_CBF_CALL(cbf_H5Dread2(h5axis,offset,0,count,actual_data,H5T_NATIVE_DOUBLE));
						if (cmp_double(expected_data, actual_data, *count
#ifdef CBF_USE_ULP
                                       , &cmp_double_params
#endif
                                       )) {
							fprintf(stderr,__WHERE__": error: data doesn't match in %s, x pixel size might not match\n",h5name);
							error |= CBF_H5DIFFERENT;
						}
						free((void*)(actual_data));
					} else if (CBF_NOTFOUND==found) {
						_CBF_CALL(cbf_H5Dcreate(detector,&h5axis,h5name,1,h5dim+1,h5dim+1,0,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dwrite2(h5axis,offset,0,count,expected_data,H5T_NATIVE_DOUBLE));
					} else {
						error |= found;
						fprintf(stderr,"%s: Error locating axis: %s\n",__WHERE__,cbf_strerror(found));
					}
					free((void*)(expected_data));
					_CBF_CALL(cbf_H5Arequire_string(h5axis,"units","m"));
					{
						const double vector[] = {1.0, 0.0, 0.0};
						const hsize_t vdims[] = {3};
						double vbuf[3] = {0./0.};
#ifdef CBF_USE_ULP
						_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5axis,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
								  vector,vbuf,cmp_double,&cmp_double_params));
#else
						_CBF_CALL(cbf_H5Arequire_cmp2(h5axis,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
								  vector,vbuf,cmp_double));
#endif
					}
					_CBF_CALL(cbf_H5Arequire_string(h5axis,"transformation_type","translation"));
					{
						const char path_empty[] = "";
						const char path_inst[] = "instrument";
						const char path_pose[] = "pose";
						const char path_axis[] = "rotation";
						const char * const path_parts[] = {
							path_empty,
							h5handle->nxid_name,
							path_inst,
							h5handle->nxdetector_name,
							path_pose,
							path_axis,
							0
						};
						const char * axis_path = _cbf_strjoin(path_parts,'/');
						_CBF_CALL(cbf_H5Arequire_string(h5axis,"depends_on",axis_path));
						free((void*)axis_path);
					}
					cbf_H5Dfree(h5axis);
				}
				if (1) {
					hid_t h5axis = CBF_H5FAIL;
					const char h5name[] = "fast_pixel_direction";
					hsize_t buf[] = {0};
					const int found =  cbf_H5Dfind2(detector,&h5axis,h5name,1,h5dim+2,buf,H5T_IEEE_F64LE);
					const hsize_t offset[] = {0};
					const hsize_t * count = h5dim+2;
					double * const expected_data = malloc((*count) * sizeof(double));
					{
						hsize_t n = 0;
						for (n=0; n!=h5dim[2]; ++n)
							expected_data[n] = (double)(n)*pixel_y;
					}
					if (CBF_SUCCESS==found) {
						double * const actual_data = malloc((*count) * sizeof(double));
						_CBF_CALL(cbf_H5Dread2(h5axis,offset,0,count,actual_data,H5T_NATIVE_DOUBLE));
						if (cmp_double(expected_data, actual_data, *count
#ifdef CBF_USE_ULP
                                       , &cmp_double_params
#endif
                                       )) {
								fprintf(stderr,__WHERE__": error: data doesn't match in %s, y pixel size might not match\n",h5name);
							error |= CBF_H5DIFFERENT;
						}
						free((void*)(actual_data));
					} else if (CBF_NOTFOUND==found) {
						_CBF_CALL(cbf_H5Dcreate(detector,&h5axis,h5name,1,h5dim+2,h5dim+2,0,H5T_IEEE_F64LE));
						_CBF_CALL(cbf_H5Dwrite2(h5axis,offset,0,count,expected_data,H5T_NATIVE_DOUBLE));
					} else {
						error |= found;
						fprintf(stderr,"%s: Error locating axis: %s\n",__WHERE__,cbf_strerror(found));
					}
					free((void*)(expected_data));
					_CBF_CALL(cbf_H5Arequire_string(h5axis,"units","m"));
					{
						const double vector[] = {0.0, 1.0, 0.0};
						const hsize_t vdims[] = {3};
						double vbuf[3] = {0./0.};
#ifdef CBF_USE_ULP
						_CBF_CALL(cbf_H5Arequire_cmp2_ULP(h5axis,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
								  vector,vbuf,cmp_double,&cmp_double_params));
#else
						_CBF_CALL(cbf_H5Arequire_cmp2(h5axis,"vector",1,vdims,H5T_IEEE_F64LE,H5T_NATIVE_DOUBLE,
								  vector,vbuf,cmp_double));
#endif
					}
					_CBF_CALL(cbf_H5Arequire_string(h5axis,"transformation_type","translation"));
					{
						const char path_empty[] = "";
						const char path_inst[] = "instrument";
						const char path_pose[] = "pose";
						const char path_axis[] = "rotation";
						const char * const path_parts[] = {
							path_empty,
							h5handle->nxid_name,
							path_inst,
							h5handle->nxdetector_name,
							path_pose,
							path_axis,
							0
						};
						const char * axis_path = _cbf_strjoin(path_parts,'/');
						_CBF_CALL(cbf_H5Arequire_string(h5axis,"depends_on",axis_path));
						free((void*)axis_path);
					}
					cbf_H5Dfree(h5axis);
				}
				_CBF_CALL(cbf_write_cbf_h5file__link_h5data(handle, h5handle, CBF_SCANTYPE1));
			}
			free((void*)saturation_value);
			++h5handle->slice;
			if (CBF_SUCCESS != cbf_next_datablock(handle)) break;
		}

		return error;
	}


    /* Open an HDF5 File handle */

    int cbf_open_h5handle(cbf_h5handle *h5handle,
                          const char * h5filename) {

        hid_t fcreate_prop_list;

        /* check that the file name has been specified and
         is an HDF5 file */

        if (!h5filename || !H5Fis_hdf5(h5filename)) return CBF_ARGUMENT;

        /* ensure the HDF5 library is ready */

        cbf_h5failneg(H5open(),CBF_ARGUMENT);

        cbf_failnez(cbf_make_h5handle(h5handle));

        cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));

        (*h5handle)->rwmode = 0;

        cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_h5onfailneg((*h5handle)->hfile = H5Fopen(h5filename,
                                                     H5F_ACC_RDONLY,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_h5onfailneg(H5Pclose(fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        return CBF_SUCCESS;

    }

    /* Convert an HDF5 typeclass to a string
     and flag for atomic or not
     copies up to n-1 characters of the
     type string to buffer*/

    int cbf_h5type_class_string(H5T_class_t type_class,
                                char * buffer,
                                int * atomic, size_t n ) {

        int good_type;

        good_type = 0;

        *atomic = 1;

        buffer[n-1] = '\0';

        switch(type_class) {
            case H5T_INTEGER:
                strncpy(buffer,"H5T_INTEGER",n-1);
                break;
            case H5T_FLOAT:
                strncpy(buffer,"H5T_FLOAT",n-1);
                break;
            case H5T_STRING:
                strncpy(buffer,"H5T_STRING",n-1);
                break;
            case H5T_BITFIELD:
                strncpy(buffer,"H5T_BITFIELD",n-1);
                break;
            case H5T_OPAQUE:
                strncpy(buffer,"H5T_OPAQUE",n-1);
                break;
            case H5T_COMPOUND:
                strncpy(buffer,"H5T_COMPOUND",n-1);
                *atomic = 0;
                break;
            case H5T_REFERENCE:
                strncpy(buffer,"H5T_REFERENCE",n-1);
                break;
            case H5T_ENUM:
                strncpy(buffer,"H5T_ENUM",n-1);
                *atomic = 0;
                break;
            case H5T_VLEN:
                strncpy(buffer,"H5T_VLEN",n-1);
                *atomic = 0;
                break;
            case H5T_ARRAY:
                strncpy(buffer,"H5T_ARRAY",n-1);
                *atomic = 0;
                break;
            case -1:
                strncpy(buffer,".",n-1);
                good_type = CBF_ARGUMENT;
                *atomic = 0;
                break;
            default:
                strncpy(buffer,"UNKNOWN",n-1);
                good_type = CBF_ARGUMENT;
                *atomic = 0;
                break;
        }

        return good_type;

    }

    /* Store an HDF5 Dataset in CBF handle, using
     category categoryname, ...*/

    int cbf_h5ds_store(cbf_handle handle, haddr_t parent,
                       const char * parent_name,
                       const int target_row,
                       const char * categoryname, hid_t obj_id,
                       hid_t space, hid_t type,
                       const char * name,
                       const int readattrib,
                       void ** value) {

        char buffer[25];

        int errorcode;

        unsigned char* data;

        char h5t_type_class[14], h5t_base_type_class[14];

        hid_t base_type;

        hid_t native_type;

        int atomic;

        int ndims, kdims, ii;

        unsigned int rows;

        hsize_t dims[H5S_MAX_RANK];

        hsize_t maxdims[H5S_MAX_RANK];

        char * byte_order;

        size_t type_size, total_size, total_dim;

        H5T_class_t type_class, base_type_class;

        H5T_order_t type_order;

        H5T_sign_t type_sign;

        errorcode = 0;

        cbf_reportnez(cbf_require_category(handle,categoryname),errorcode);

        /*  Give the name of this dataset as its own id */

        cbf_reportnez(cbf_require_column(handle,"id"),errorcode);

        cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);

        if (target_row==-1) {

            cbf_reportnez(cbf_new_row(handle),errorcode);

        } else {

            if ((unsigned int)target_row >= rows ) {

                for (ii=rows; ii <= target_row; ii++) {

                    cbf_reportnez(cbf_new_row(handle),errorcode);

                }
            }

            cbf_reportnez(cbf_select_row(handle,target_row),errorcode);
        }

        cbf_reportnez(cbf_set_value(handle,name),errorcode);

        /*  Give the parent name and id for this dataset */

        cbf_reportnez(cbf_require_column(handle,"parent_name"),errorcode);

        cbf_reportnez(cbf_set_value(handle,parent_name),errorcode);

        cbf_reportnez(cbf_require_column(handle,"parent_id"),errorcode);

        sprintf(buffer,"0x%lx",(unsigned long)parent);

        cbf_reportnez(cbf_set_value(handle,buffer),errorcode);


        /* get the class, and, if not atomic
         try to get the base class for an array
         give up otherwise */

        type_class = H5Tget_class(type);

        native_type = H5Tget_native_type(type,H5T_DIR_ASCEND);

        base_type = CBF_H5FAIL;

        cbf_reportnez(cbf_require_column(handle,"type"),errorcode);

        type_size = 0;

        if (value) *value = 0;

        type_order = -1;

        kdims= ndims = H5Sget_simple_extent_ndims(space);

        if (ndims <= 0) ndims = 1;

        H5Sget_simple_extent_dims(space,dims,maxdims);

        if (!cbf_h5type_class_string(
                                     type_class,
                                     h5t_type_class,&atomic,14)) {

            cbf_reportnez(cbf_set_value(handle, h5t_type_class),errorcode);

            if (!atomic && type_class==H5T_ARRAY){
                base_type = H5Tget_super(type);

                base_type_class = H5Tget_class(base_type);

                if (!cbf_h5type_class_string(
                                             base_type_class,
                                             h5t_base_type_class,&atomic,14)) {
                    if (!atomic) {
                        strncpy (h5t_base_type_class,".",14);
                        cbf_h5failneg(H5Tclose(base_type),CBF_FORMAT);
                        base_type = CBF_H5FAIL;
                    } else {

                        type_size = H5Tget_size(base_type);

                        type_order = H5Tget_order(base_type);

                        type_sign = H5Tget_sign(base_type);

                        cbf_reportnez(cbf_require_column(handle,"base_type"),errorcode);

                        cbf_reportnez(cbf_set_value(handle,h5t_base_type_class),errorcode);
                    }
                } else {
                    strncpy (h5t_base_type_class,".",14);
                    cbf_h5failneg(H5Tclose(base_type),CBF_FORMAT);
                    base_type = CBF_H5FAIL;
                }

            } else if (atomic) {

                type_size = H5Tget_size(native_type);

                type_order = H5Tget_order(type);

                type_sign = H5Tget_sign(type);

            }

        }

        total_size = type_size;

        total_dim = 1;

        for (ii=0; ii < kdims; ii ++) {

            total_size *= dims[ii];

            total_dim *= dims[ii];

        }


        if (total_size < type_size) total_size = type_size;

        if (total_dim < 1 ) total_dim = 1;

        cbf_reportnez(cbf_require_column(handle,"value"),errorcode);

        if(total_size > 0) {

            if(readattrib) {

                /* Process an attribute */

                cbf_reportnez(cbf_alloc(((void **) value),NULL,
                                        total_size+1,1),errorcode);

                cbf_h5failneg(H5Aread(obj_id,native_type,(void *)*value),
                              CBF_ARGUMENT);

                (*((char **)value))[total_size]='\0';

                if (type_class==H5T_STRING) {

                    cbf_reportnez(cbf_set_value(handle,(const char *)(*value)),errorcode);

                } else if (type_class==H5T_INTEGER){

                    /* Read of a single integer or an integer array of
                     up to 3 dimensions */

                    char * ivalue;

                    long xdata;

                    unsigned long uxdata;

                    int sign;

                    sign = (type_sign==H5T_SGN_2)?1:0;

                    if (total_dim ==1) {

                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*3+1,1),errorcode);

                        if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&sign) xdata = **((signed char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&!sign) uxdata = **((unsigned char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_SCHAR)) xdata = **((signed char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_UCHAR)) uxdata = **((unsigned char **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_SHORT)) xdata = **((unsigned short **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_USHORT)) uxdata = **((unsigned short **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_INT)) xdata = **((int **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_UINT)) uxdata = **((unsigned int **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_LONG)) xdata = **((long **)value);
                        if (H5Tequal(native_type,H5T_NATIVE_ULONG)) uxdata = **((unsigned long **)value);

                        if (sign) {

                            sprintf(ivalue,"%ld",xdata);

                        } else {

                            sprintf(ivalue,"%lu",uxdata);

                        }

                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                        cbf_reportnez(cbf_free((void**)value,NULL),errorcode);

                        *value = (unsigned char *)ivalue;

                    } else {



                        /* process arrays of up to 100 as
                         bracketed strings
                         */

                        if (total_dim < 101) {

                            size_t indices[H5S_MAX_RANK];

                            size_t master_index, ival_index;

                            int idim, level;

                            char buffer[40];

                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*3)+kdims*2)+1,1),errorcode);

                            for (idim = 0; idim < kdims; idim ++) {

                                indices[idim] = 0;

                                ivalue[idim] = '[';

                            }

                            level = kdims-1;

                            ival_index = kdims;

                            master_index = 0;

                            while (master_index < total_dim) {

                                for (indices[level]=0; indices[level] < dims[level];) {

                                    if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&sign) xdata = ((*(signed char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_CHAR)&&!sign) uxdata = ((*(unsigned char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_SCHAR)) xdata = ((*(signed char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_UCHAR)) uxdata = ((*(unsigned char **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_SHORT)) xdata = ((*(unsigned short **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_USHORT)) uxdata = ((*(unsigned short **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_INT)) xdata = ((*(int **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_UINT)) uxdata = ((*(unsigned int **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_LONG)) xdata = ((*(long **)value)[master_index]);
                                    if (H5Tequal(native_type,H5T_NATIVE_ULONG)) uxdata = ((*(unsigned long **)value)[master_index]);


                                    if (sign) {

                                        sprintf(buffer,"%ld",xdata);

                                    } else {

                                        sprintf(buffer,"%lu",uxdata);

                                    }

                                    strcat(ivalue+ival_index,buffer);

                                    ival_index+=strlen(buffer);

                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';

                                    master_index++;

                                    indices[level]++;

                                    if (indices[level] == dims[level]) {

                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */

                                        indices[level] = 0;

                                        level --;

                                        while (level >= 0) {

                                            indices[level]++;

                                            if (indices[level] < dims[level]) {

                                                ivalue[ival_index++] = ',';

                                                ivalue[ival_index++] = '[';

                                                level++;

                                                break;

                                            } else {

                                                ivalue[ival_index++] = ']';

                                                indices[level] = 0;

                                                level --;

                                            }

                                        }

                                        if (level < 0) break;

                                        while (level > kdims-1) {

                                            ivalue[ival_index++] = '[';

                                            level++;

                                        }

                                    }

                                }

                            }

                            ivalue[ival_index++] = '\0';

                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);

                            cbf_reportnez(cbf_free((void**)value,NULL),errorcode);

                            *value = (unsigned char *)ivalue;

                        } else {

                            size_t dimfast, dimmid, dimslow;

                            dimmid = dimslow = 1;

                            dimfast = dims[kdims-1];

                            if (kdims > 1) dimmid = dims[kdims-2];

                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);

                            cbf_reportnez(cbf_set_integerarray_wdims_fs(handle,
                                                                        CBF_NIBBLE_OFFSET,target_row,*value,
                                                                        type_size,sign,total_dim,"little_endian",
                                                                        dimfast,dimmid,dimslow,0),errorcode);

                        }


                    }


                } else if (type_class==H5T_FLOAT){

                    /* Read of a single float or double or a float or
                     double array of up to 3 dimensions */

                    char * ivalue;

                    double dxdata;

                    float xdata;

                    if (total_dim ==1) {

                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*2+6,1),errorcode);

                        if (H5Tequal(native_type,H5T_NATIVE_FLOAT)) {

                            xdata = **((float **)value);

                            snprintf(ivalue,type_size*2+5,"%.7g",(double) xdata);

                        } else {

                            dxdata = **((double **)value);

                            snprintf(ivalue,type_size*2+5,"%.15g",dxdata);

                        }

                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                        cbf_reportnez(cbf_free((void**)value,NULL),errorcode);

                        *value = (unsigned char *)ivalue;

                    } else {



                        /* process arrays of up to 100 as
                         bracketed strings
                         */

                        if (total_dim < 101) {

                            size_t indices[H5S_MAX_RANK];

                            size_t master_index, ival_index;

                            int idim, level;

                            char buffer[40];

                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*2)+5+kdims*2)+1,1),errorcode);

                            for (idim = 0; idim < kdims; idim ++) {

                                indices[idim] = 0;

                                ivalue[idim] = '[';

                            }

                            level = kdims-1;

                            ival_index = kdims;

                            master_index = 0;

                            while (master_index < total_dim) {

                                for (indices[level]=0; indices[level] < dims[level];) {

                                    if (H5Tequal(native_type,H5T_NATIVE_FLOAT)) {

                                        xdata = ((*(float **)value)[master_index]);

                                        snprintf(buffer,type_size*2+5,"%.7g",(double) xdata);

                                    } else {

                                        dxdata = ((*(double **)value)[master_index]);

                                        snprintf(buffer,type_size*2+5,"%.15g",dxdata);

                                    }

                                    strcat(ivalue+ival_index,buffer);

                                    ival_index+=strlen(buffer);

                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';

                                    master_index++;

                                    indices[level]++;

                                    if (indices[level] == dims[level]) {

                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */

                                        indices[level] = 0;

                                        level --;

                                        while (level >= 0) {

                                            indices[level]++;

                                            if (indices[level] < dims[level]) {

                                                ivalue[ival_index++] = ',';

                                                ivalue[ival_index++] = '[';

                                                level++;

                                                break;

                                            } else {

                                                ivalue[ival_index++] = ']';

                                                indices[level] = 0;

                                                level --;

                                            }

                                        }

                                        if (level < 0) break;

                                        while (level > kdims-1) {

                                            ivalue[ival_index++] = '[';

                                            level++;

                                        }

                                    }

                                }

                            }

                            ivalue[ival_index++] = '\0';

                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);

                            cbf_reportnez(cbf_free((void**)value,NULL),errorcode);

                            *value = (unsigned char *)ivalue;

                        } else {

                            size_t dimfast, dimmid, dimslow;

                            dimmid = dimslow = 1;

                            dimfast = dims[kdims-1];

                            if (kdims > 1) dimmid = dims[kdims-2];

                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);

                            cbf_reportnez(cbf_set_realarray_wdims_fs(handle,
                                                                     CBF_NIBBLE_OFFSET,target_row,*value,
                                                                     type_size,total_dim,"little_endian",
                                                                     dimfast,dimmid,dimslow,0),errorcode);

                        }


                    }


                } else if (type_class != H5T_OPAQUE) {

                    unsigned char * hexvalue;

                    unsigned char hexdigs[16] = {'0','1','2','3','4','5','6','7','8','9',
                        'a','b','c','d','e','f'};

                    size_t ii;

                    cbf_reportnez(cbf_alloc(((void **) &hexvalue),NULL,
                                            2*total_size+1,1),errorcode);

                    hexvalue[2*total_size+1] = '\0';

                    for (ii=0; ii< total_size; ii++) {

                        hexvalue[(total_size-ii)*2-2] =
                        hexdigs[((int)(*((unsigned char **)value))[ii])&0xF];

                        hexvalue[(total_size-ii)*2-1] =
                        hexdigs[((int)((*((unsigned char **)value))[ii])>>4)&0xF];

                    }

                    cbf_reportnez(cbf_set_value(handle,(const char *)(hexvalue)),errorcode);

                    cbf_reportnez(cbf_free((void**)value,NULL),errorcode);

                    *value = hexvalue;

                }

            } else {

                /* process a dataset */

                hid_t memspace;

                memspace=H5Screate_simple(kdims,dims,NULL);

                cbf_reportnez(cbf_alloc(((void **) &data),NULL,
                                        total_size+1,1),errorcode);

                cbf_h5failneg(H5Dread(obj_id,native_type,
                                      H5S_ALL,memspace,H5P_DEFAULT,data),
                              CBF_ARGUMENT);

                data[total_size]='\0';

                if (type_class==H5T_STRING) {

                    cbf_reportnez(cbf_set_value(handle,(const char *)data),errorcode)

                } else if (type_class==H5T_INTEGER){

                    /* Read of a single integer or an integer array of
                     up to 3 dimensions */

                    char * ivalue;

                    long xdata;

                    unsigned long uxdata;

                    int sign;

                    sign = (type_sign==H5T_SGN_2)?1:0;

                    if (total_dim ==1) {

                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*3+1,1),errorcode);

                        if (H5Tequal(native_type, H5T_NATIVE_CHAR)&&sign) xdata = *((signed char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_CHAR)&&!sign) uxdata = *((unsigned char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_SCHAR)) xdata = *((signed char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_UCHAR)) uxdata = *((unsigned char *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_SHORT)) xdata = *((unsigned short *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_USHORT)) uxdata = *((unsigned short *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_INT)) xdata = *((int *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_UINT)) uxdata = *((unsigned int *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_LONG)) xdata = *((long *)data);
                        if (H5Tequal(native_type, H5T_NATIVE_ULONG)) uxdata = *((unsigned long *)data);

                        if (sign) {

                            sprintf(ivalue,"%ld",xdata);

                        } else {

                            sprintf(ivalue,"%lu",uxdata);

                        }

                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                        cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);

                        data = (unsigned char *)ivalue;

                    } else {



                        /* process arrays of up to 100 as
                         bracketed strings
                         */

                        if (total_dim < 101) {

                            size_t indices[H5S_MAX_RANK];

                            size_t master_index, ival_index;

                            int idim, level;

                            char buffer[40];

                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*3)+kdims*2)+1,1),errorcode);

                            for (idim = 0; idim < kdims; idim ++) {

                                indices[idim] = 0;

                                ivalue[idim] = '[';

                            }

                            level = kdims-1;

                            ival_index = kdims;

                            master_index = 0;

                            while (master_index < total_dim) {

                                for (indices[level]=0; indices[level] < dims[level];) {

                                    if (H5Tequal(native_type, H5T_NATIVE_CHAR)
                                        &&sign) xdata = (((signed char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_CHAR)
                                        &&!sign) uxdata = (((unsigned char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_SCHAR)) xdata = (((signed char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_UCHAR)) uxdata = (((unsigned char *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_SHORT)) xdata = (((unsigned short *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_USHORT)) uxdata = (((unsigned short *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_INT)) xdata = (((int *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_UINT)) uxdata = (((unsigned int *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_LONG)) xdata = (((long *)data)[master_index]);
                                    if (H5Tequal(native_type, H5T_NATIVE_ULONG)) uxdata = (((unsigned long *)data)[master_index]);


                                    if (sign) {

                                        sprintf(buffer,"%ld",xdata);

                                    } else {

                                        sprintf(buffer,"%lu",uxdata);

                                    }

                                    strcat(ivalue+ival_index,buffer);

                                    ival_index+=strlen(buffer);

                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';

                                    master_index++;

                                    indices[level]++;

                                    if (indices[level] == dims[level]) {

                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */

                                        indices[level] = 0;

                                        level --;

                                        while (level >= 0) {

                                            indices[level]++;

                                            if (indices[level] < dims[level]) {

                                                ivalue[ival_index++] = ',';

                                                ivalue[ival_index++] = '[';

                                                level++;

                                                break;

                                            } else {

                                                ivalue[ival_index++] = ']';

                                                indices[level] = 0;

                                                level --;

                                            }

                                        }

                                        if (level < 0) break;

                                        while (level > kdims-1) {

                                            ivalue[ival_index++] = '[';

                                            level++;

                                        }

                                    }

                                }

                            }

                            ivalue[ival_index++] = '\0';

                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);

                            cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);

                            data = (unsigned char *)ivalue;

                        } else {

                            size_t dimfast, dimmid, dimslow;

                            dimmid = dimslow = 1;

                            dimfast = dims[kdims-1];

                            if (kdims > 1) dimmid = dims[kdims-2];

                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);

                            cbf_reportnez(cbf_set_integerarray_wdims_fs(handle,
                                                                        CBF_NIBBLE_OFFSET,target_row,data,
                                                                        type_size,sign,total_dim,"little_endian",
                                                                        dimfast,dimmid,dimslow,0),errorcode);

                        }


                    }


                } else if (type_class==H5T_FLOAT){

                    /* Read of a single float or double or a float or
                     double array of up to 3 dimensions */

                    char * ivalue;

                    double dxdata;

                    float xdata;

                    if (total_dim ==1) {

                        cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                type_size*2+6,1),errorcode);

                        if (H5Tequal(native_type, H5T_NATIVE_FLOAT)) {

                            xdata = *((float *)data);

                            snprintf(ivalue,type_size*2+5,"%.7g",(double) xdata);

                        } else {

                            dxdata = *((double *)data);

                            snprintf(ivalue,type_size*2+5,"%.15g",dxdata);

                        }

                        cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                        cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);

                        data = (unsigned char *)ivalue;

                    } else {



                        /* process arrays of up to 100 as
                         bracketed strings
                         */

                        if (total_dim < 101) {

                            size_t indices[H5S_MAX_RANK];

                            size_t master_index, ival_index;

                            int idim, level;

                            char buffer[40];

                            cbf_reportnez(cbf_alloc(((void **) &ivalue),NULL,
                                                    total_dim*((type_size*2)+5+kdims*2)+1,1),errorcode);

                            for (idim = 0; idim < kdims; idim ++) {

                                indices[idim] = 0;

                                ivalue[idim] = '[';

                            }

                            level = kdims-1;

                            ival_index = kdims;

                            master_index = 0;

                            while (master_index < total_dim) {

                                for (indices[level]=0; indices[level] < dims[level];) {

                                    if (H5Tequal(native_type, H5T_NATIVE_FLOAT)) {

                                        xdata = (((float *)data)[master_index]);

                                        snprintf(buffer,type_size*2+5,"%.7g",(double) xdata);

                                    } else {

                                        dxdata = (((double *)data)[master_index]);

                                        snprintf(buffer,type_size*2+5,"%.15g",dxdata);

                                    }

                                    strcat(ivalue+ival_index,buffer);

                                    ival_index+=strlen(buffer);

                                    ivalue[ival_index++]= (indices[level] < dims[level]-1)?',':']';

                                    master_index++;

                                    indices[level]++;

                                    if (indices[level] == dims[level]) {

                                        /* We are at the end of a fast-dimension row
                                         and therefore need to update higher level indices
                                         if any.  */

                                        indices[level] = 0;

                                        level --;

                                        while (level >= 0) {

                                            indices[level]++;

                                            if (indices[level] < dims[level]) {

                                                ivalue[ival_index++] = ',';

                                                ivalue[ival_index++] = '[';

                                                level++;

                                                break;

                                            } else {

                                                ivalue[ival_index++] = ']';

                                                indices[level] = 0;

                                                level --;

                                            }

                                        }

                                        if (level < 0) break;

                                        while (level > kdims-1) {

                                            ivalue[ival_index++] = '[';

                                            level++;

                                        }

                                    }

                                }

                            }

                            ivalue[ival_index++] = '\0';

                            cbf_reportnez(cbf_set_value(handle,(const char *)(ivalue)),errorcode);

                            cbf_reportnez(cbf_set_typeofvalue(handle,"bkts"),errorcode);

                            cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);

                            data = (unsigned char *)ivalue;

                        } else {

                            size_t dimfast, dimmid, dimslow;

                            dimmid = dimslow = 1;

                            dimfast = dims[kdims-1];

                            if (kdims > 1) dimmid = dims[kdims-2];

                            if (kdims > 2) dimslow = total_dim/(dimfast*dimmid);

                            cbf_reportnez(cbf_set_realarray_wdims_fs(handle,
                                                                     CBF_NIBBLE_OFFSET,target_row,data,
                                                                     type_size,total_dim,"little_endian",
                                                                     dimfast,dimmid,dimslow,0),errorcode);

                        }


                    }


                } else if (type_class!= H5T_OPAQUE){

                    char * hexvalue;

                    char hexdigs[16] = {'0','1','2','3','4','5','6','7','8','9',
                        'a','b','c','d','e','f'};

                    size_t ii;

                    cbf_reportnez(cbf_alloc(((void **) &hexvalue),NULL,
                                            2*total_size+1,1),errorcode);

                    hexvalue[2*total_size+1] = '\0';

                    for (ii=0; ii< total_size; ii++) {

                        hexvalue[(total_size-ii)*2-2] =
                        hexdigs[((int)(((unsigned char *)data))[ii])&0xF];

                        hexvalue[(total_size-ii)*2-1] =
                        hexdigs[((int)((((unsigned char *)data))[ii])>>4)&0xF];

                    }

                    cbf_reportnez(cbf_set_value(handle,(const char *)(hexvalue)),errorcode);

                    cbf_reportnez(cbf_free((void**)&data,NULL),errorcode);

                    data = (unsigned char *)hexvalue;

                }


                if (value) {

                    *value=data;

                } else {

                    cbf_reportnez(cbf_free((void **)&data,NULL),errorcode);
                }

            }

        }
        switch(type_order) {
            case H5T_ORDER_LE:
                byte_order = "H5T_ORDER_LE";
                break;
            case H5T_ORDER_BE:
                byte_order = "H5T_ORDER_BE";
                break;
            case H5T_ORDER_VAX:
                byte_order = "H5T_ORDER_VAX";
                break;
            case H5T_ORDER_MIXED:
                byte_order = "H5T_ORDER_MIXED";
                break;
            case H5T_ORDER_NONE:
                byte_order = "H5T_ORDER_LE";
                break;
            case -1:
                byte_order = ".";
                break;
            default: byte_order="UNKNOWN";
                break;
        }

        cbf_reportnez(cbf_require_column(handle,"h5_byte_order"),errorcode);

        cbf_reportnez(cbf_set_value(handle,byte_order),errorcode);

        if (type_size < 10) {

            sprintf(buffer,"%ld",(unsigned long)type_size);

        } else {

            sprintf(buffer,"0x%lx",(unsigned long)type_size);

        }

        cbf_reportnez(cbf_require_column(handle,"size"),errorcode);

        cbf_reportnez(cbf_set_value(handle,buffer),errorcode);

        if (base_type >=0) H5Tclose(base_type);

        if (native_type>=0) H5Tclose(native_type);

        H5garbage_collect();

        return CBF_SUCCESS;
    }


    /* Callback routine for objects in a group */


    herr_t cbf_object_visit(hid_t loc_id, const char *name,
                            const H5L_info_t *info,
                            void *op_data){

        int cbfrow;

        int errorcode;

        cbf_handle handle;

        haddr_t parent_addr;

        hid_t parent_id;

        unsigned int row;

        const char* parent_name;

        const char* grand_parent_name;

        int innexus;

        int incbf, incbfdb, incbfcat, incbfcol;

        hid_t group_id, dataset_id;

        herr_t retval;

        hsize_t i;

        char buffer[25];

        char digest[25];

        char *value;

        char cbftype[5];

        cbf_bookmark bookmark;

        cbf_bookmark saved_bookmark;

        H5O_info_t  objinfo;

        hid_t attrib_id,attrib_ds,attrib_type;

        hid_t dataset_ds, dataset_type;

        H5T_class_t dataset_type_class;

        ssize_t attrib_name_size;

        int attrib_num;

        unsigned int compression;

        int binary_id, bits, sign, type, checked_digest, realarray;

        const char *byteorder;

        size_t binsize;

        size_t dimover, dimfast, dimmid, dimslow;

        size_t padding;

        errorcode = 0;

        handle = ((cbf_h5Ovisithandle)op_data)->handle;

        if (!handle) return -1;

        /* skip the root group itself */

        if (name[0]== '.') return 0;

        cbf_h5failneg(H5Oget_info_by_name(loc_id,
                                          name, &objinfo, H5P_DEFAULT),CBF_FORMAT);
        parent_id = ((cbf_h5Ovisithandle)op_data)->parent_id;

        parent_addr = ((cbf_h5Ovisithandle)op_data)->parent_addr;

        parent_name = ((cbf_h5Ovisithandle)op_data)->parent_name;

        grand_parent_name = ((cbf_h5Ovisithandle)op_data)->grand_parent_name;

        innexus = ((cbf_h5Ovisithandle)op_data)->innexus;

        incbf = ((cbf_h5Ovisithandle)op_data)->incbf;

        incbfdb = ((cbf_h5Ovisithandle)op_data)->incbfdb;

        incbfcat = ((cbf_h5Ovisithandle)op_data)->incbfcat;

        incbfcol = ((cbf_h5Ovisithandle)op_data)->incbfcol;

        memmove(&saved_bookmark,&(((cbf_h5Ovisithandle)op_data)->bookmark),sizeof(cbf_bookmark));

        switch (objinfo.type) {

            case H5O_TYPE_GROUP:

                /* Skip duplicates */

                for (i=0; i < ((cbf_h5Ovisithandle)op_data)->path_size; i++) {

                    if (objinfo.addr ==
                        ((cbf_h5Ovisithandle)op_data)->haddr_path[i])
                        return 0;

                }

                if (((cbf_h5Ovisithandle)op_data)->path_size >=
                    ((cbf_h5Ovisithandle)op_data)->capacity) {

                    size_t newcap;

                    newcap = 2*((cbf_h5Ovisithandle)op_data)->capacity;

                    cbf_reportnez(
                                  cbf_realloc(
                                              (void **)(&((cbf_h5Ovisithandle)op_data)->hid_path),
                                              NULL,sizeof(hid_t),newcap),errorcode);
                    cbf_reportnez(
                                  cbf_realloc(
                                              (void **)(&((cbf_h5Ovisithandle)op_data)->haddr_path),
                                              NULL,sizeof(haddr_t),newcap),errorcode);

                    ((cbf_h5Ovisithandle)op_data)->capacity=newcap;

                }

                (((cbf_h5Ovisithandle)op_data)->
                 haddr_path)[((cbf_h5Ovisithandle)op_data)->path_size] =
                objinfo.addr;

                group_id = H5Gopenx(loc_id,name);
                (((cbf_h5Ovisithandle)op_data)->
                 haddr_path)[((cbf_h5Ovisithandle)op_data)->path_size] =
                group_id;
                (((cbf_h5Ovisithandle)op_data)->path_size)++;

                /* We have a group
                 We need to add it to the H5_Groups category
                 in the H5 data block.

                 If it has attributes, we need to add them to
                 the H5Attributes category

                 If it has datasets, we will catch them when we
                 iterate again

                 */

                cbf_reportnez(cbf_rewind_datablock(handle),errorcode);

                if (cbf_find_datablock(handle,"H5")) {

                    cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);

                }

                cbf_reportnez(cbf_require_category(handle,"H5_Groups"),errorcode);

                cbf_reportnez(cbf_new_row(handle),errorcode);

                cbf_reportnez(cbf_row_number(handle,&row),errorcode);

                cbf_reportnez(cbf_require_column(handle,"name"),errorcode);

                cbf_reportnez(cbf_set_value(handle,name),errorcode);

                cbf_reportnez(cbf_require_column(handle,"parent_name"),errorcode);

                cbf_reportnez(cbf_set_value(handle,parent_name),errorcode);

                cbf_reportnez(cbf_require_column(handle,"parent_id"),errorcode);

                if (!parent_addr) {

                    cbf_reportnez(cbf_set_value(handle,"."),errorcode);

                    cbf_reportnez(cbf_set_typeofvalue(handle,"null"),errorcode);

                } else {

                    sprintf(buffer,"0x%lx",(unsigned long)parent_addr);

                    cbf_reportnez(cbf_set_value(handle,buffer),errorcode);

                }

                cbf_reportnez(cbf_require_column(handle,"id"),errorcode);

                sprintf(buffer,"0x%lx",(unsigned long)objinfo.addr);

                cbf_reportnez(cbf_set_value(handle,buffer),errorcode);

                attrib_num = objinfo.num_attrs;

                cbf_reportnez(cbf_require_column(handle,"no_attributes"),errorcode);

                cbf_reportnez(cbf_set_integervalue(handle,attrib_num),errorcode);

                for (i=0; i < attrib_num; i++) {

                    char * attrib_name;
                    attrib_id=H5Aopen_by_idx(group_id,".",
                                             H5_INDEX_NAME,
                                             H5_ITER_INC,
                                             i,H5P_DEFAULT,H5P_DEFAULT);
                    attrib_ds = H5Aget_space(attrib_id);
                    attrib_type = H5Aget_type(attrib_id);
                    attrib_name_size = H5Aget_name(attrib_id,0,NULL);
                    cbf_reportnez(cbf_alloc(((void **) &attrib_name),NULL,
                                            attrib_name_size+1,1),errorcode);

                    cbf_h5failneg(H5Aget_name(attrib_id,
                                              attrib_name_size+1,attrib_name),
                                  CBF_ARGUMENT);
                    cbf_h5ds_store(handle,objinfo.addr,
                                   name,-1,
                                   "H5_Group_attribute",
                                   attrib_id,
                                   attrib_ds,
                                   attrib_type,
                                   attrib_name,1, (void **)&value);
                    if (!cbf_cistrcmp(attrib_name,"NX_class")&& value) {

                        cbf_reportnez(cbf_rewind_datablock(handle),errorcode);

                        if (cbf_find_datablock(handle,"H5")) {

                            cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);

                        }


                        cbf_reportnez(cbf_require_category(handle,"H5_Groups"),errorcode);

                        cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

                        cbf_reportnez(cbf_select_row(handle,row),errorcode);

                        cbf_reportnez(cbf_require_column(handle,"NX_class"),errorcode);

                        cbf_reportnez(cbf_set_value(handle,value),errorcode);

                        if (!cbf_cistrcmp(value,"NXentry")) {

                            ((cbf_h5Ovisithandle)op_data)->innexus = 1;

                        }

                        if (!cbf_cistrcmp(value,"CBF_cbf")||!cbf_cistrcmp(value,"NXcbf")) {

                            ((cbf_h5Ovisithandle)op_data)->incbf = 1;

                        }

                        if (!cbf_cistrcmp(value,"CBF_cbfdb") || !cbf_cistrcmp(value,"NXcbfdb")) {

                            ((cbf_h5Ovisithandle)op_data)->incbfdb = 1;

                            cbf_get_bookmark(handle,&bookmark);

                            if (cbf_find_datablock(handle,name)) {

                                cbf_reportnez(cbf_new_datablock(handle,name),errorcode);

                            }

                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));

                            cbf_goto_bookmark(handle,bookmark);

                        }

                        if ((!cbf_cistrcmp(value,"CBF_cbfcat")||!cbf_cistrcmp(value,"NXcbfcat"))&& saved_bookmark.datablock) {

                            ((cbf_h5Ovisithandle)op_data)->incbfcat = 1;

                            cbf_get_bookmark(handle,&bookmark);

                            cbf_goto_bookmark(handle,saved_bookmark);

                            if (cbf_find_category(handle,name)) {

                                cbf_reportnez(cbf_new_category(handle,name),errorcode);

                            }

                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));

                            cbf_goto_bookmark(handle,bookmark);


                        }

                        if ((!cbf_cistrcmp(value,"CBF_cbfcol")
                             ||!cbf_cistrcmp(value,"NXcbfcol"))
                            && saved_bookmark.category) {

                            ((cbf_h5Ovisithandle)op_data)->incbfcol = 1;

                            cbf_get_bookmark(handle,&bookmark);

                            cbf_goto_bookmark(handle,saved_bookmark);

                            if (cbf_find_column(handle,name)) {

                                cbf_reportnez(cbf_new_column(handle,name),errorcode);

                            }

                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));

                            cbf_goto_bookmark(handle,bookmark);

                        }

                    }


                    cbf_reportnez(cbf_free((void **)&attrib_name,NULL),errorcode);
                    if (value) {
                        cbf_reportnez(cbf_free((void **)&value, NULL),errorcode);
                    }
                    H5Tclose(attrib_type);
                    H5Sclose(attrib_ds);
                    H5Aclose(attrib_id);
                }

                ((cbf_h5Ovisithandle)op_data)->parent_addr = objinfo.addr;

                ((cbf_h5Ovisithandle)op_data)->parent_id = group_id;

                cbf_reportnez(cbf_alloc((void **) &(((cbf_h5Ovisithandle)op_data)->parent_name),NULL,
                                        strlen(name)+1,1),errorcode);

                ((cbf_h5Ovisithandle)op_data)->grand_parent_name = parent_name;

                if (!name) return -1;

                strcpy((char *)((cbf_h5Ovisithandle)op_data)->parent_name,name);

                retval = H5Literate_by_name(loc_id, name,H5_INDEX_NAME,
                                            H5_ITER_INC,
                                            NULL,
                                            cbf_object_visit,op_data,H5P_DEFAULT);

                H5Gclose(group_id);

                cbf_reportnez(cbf_free((void **)(&((cbf_h5Ovisithandle)op_data)->parent_name),NULL),errorcode);

                (((cbf_h5Ovisithandle)op_data)->path_size)--;

                ((cbf_h5Ovisithandle)op_data)->parent_id = parent_id;

                ((cbf_h5Ovisithandle)op_data)->parent_addr = parent_addr;

                ((cbf_h5Ovisithandle)op_data)->parent_name = parent_name;

                ((cbf_h5Ovisithandle)op_data)->grand_parent_name = grand_parent_name;

                ((cbf_h5Ovisithandle)op_data)->innexus = innexus;

                ((cbf_h5Ovisithandle)op_data)->incbf = incbf;

                ((cbf_h5Ovisithandle)op_data)->incbfdb = incbfdb;

                ((cbf_h5Ovisithandle)op_data)->incbfcat = incbfcat;

                ((cbf_h5Ovisithandle)op_data)->incbfcol = incbfcol;

                return retval;
                break;

            case H5O_TYPE_DATASET:

                dataset_id = H5Dopen2(loc_id,name,H5P_DEFAULT);

                /* We have a dataset
                 We need to add it to the H5_Datasets category
                 in the current data block.

                 If it has attributes, we need to add them to
                 the H5_Attributes category

                 */


                cbf_reportnez(cbf_rewind_datablock(handle),errorcode);

                if (cbf_find_datablock(handle,"H5")) {

                    cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);

                }

                cbf_reportnez(cbf_require_category(handle,"H5_Datasets"),errorcode);

                cbf_reportnez(cbf_new_row(handle),errorcode);

                cbf_reportnez(cbf_row_number(handle,&row),errorcode);

                cbf_reportnez(cbf_require_column(handle,"name"),errorcode);

                cbf_reportnez(cbf_set_value(handle,name),errorcode);

                cbf_reportnez(cbf_require_column(handle,"parent_name"),errorcode);

                cbf_reportnez(cbf_set_value(handle,parent_name),errorcode);

                cbf_reportnez(cbf_require_column(handle,"parent_id"),errorcode);

                if (!parent_addr) {

                    cbf_reportnez(cbf_set_value(handle,"."),errorcode);

                    cbf_reportnez(cbf_set_typeofvalue(handle,"null"),errorcode);

                } else {

                    sprintf(buffer,"0x%lx",(unsigned long)parent_addr);

                    cbf_reportnez(cbf_set_value(handle,buffer),errorcode);

                }

                cbf_reportnez(cbf_require_column(handle,"id"),errorcode);

                sprintf(buffer,"0x%lx",(unsigned long)objinfo.addr);

                cbf_reportnez(cbf_set_value(handle,buffer),errorcode);

                attrib_num = objinfo.num_attrs;

                cbf_reportnez(cbf_require_column(handle,"no_attributes"),errorcode);

                cbf_reportnez(cbf_set_integervalue(handle,attrib_num),errorcode);

                dimover = 0;

                binsize = 0;

                compression = 0;

                binary_id = 0;

                bits = 0;

                sign = 0;

                type = 0;

                byteorder = " ";

                dimfast=dimslow=dimmid = 0;

                padding = 0;

                cbftype[0] = '\0';

                for (i=0; i < attrib_num; i++) {

                    char * attrib_name;
                    attrib_id=H5Aopen_by_idx(dataset_id,".",
                                             H5_INDEX_NAME,
                                             H5_ITER_INC,
                                             i,H5P_DEFAULT,H5P_DEFAULT);
                    attrib_ds = H5Aget_space(attrib_id);
                    attrib_type = H5Aget_type(attrib_id);
                    attrib_name_size = H5Aget_name(attrib_id,0,NULL);
                    cbf_reportnez(cbf_alloc(((void **) &attrib_name),NULL,
                                            attrib_name_size+1,1),errorcode);

                    cbf_h5failneg(H5Aget_name(attrib_id,
                                              attrib_name_size+1,attrib_name),
                                  CBF_ARGUMENT);
                    cbf_h5ds_store(handle,objinfo.addr,
                                   name,-1,
                                   "H5_Dataset_attribute",
                                   attrib_id,
                                   attrib_ds,
                                   attrib_type,
                                   attrib_name,1,(void **)&value);
                    if (*value) {


                        cbf_reportnez(cbf_rewind_datablock(handle),errorcode);

                        if (cbf_find_datablock(handle,"H5")) {

                            cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);

                        }

                        cbf_reportnez(cbf_require_category(handle,"H5_Datasets"),errorcode);

                        cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

                        cbf_reportnez(cbf_select_row(handle,row),errorcode);

                        cbf_reportnez(cbf_require_column(handle,attrib_name),errorcode);

                        cbf_reportnez(cbf_set_value(handle,value),errorcode);

                        if (!cbf_cistrcmp(attrib_name,"compression")) {
                            compression=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"binid")) {
                            binary_id=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"bits")) {
                            bits=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"sign")) {
                            sign = (int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"bintype")) {
                            type=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"checked_digest")) {
                            checked_digest=(int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"real")) {
                            realarray = (int)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"byteorder")) {
                            byteorder=
                            (value[0]=='l'||value[0]=='L')?
                            "little_endian":"big_endian";
                        } else if (!cbf_cistrcmp(attrib_name,"size")) {
                            binsize = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"digest")
                                   ||!cbf_cistrcmp(attrib_name,"MD5_digest")) {
                            strncpy(digest,value,24);
                            digest[24] = 0;
                        } else if (!cbf_cistrcmp(attrib_name,"dimover")) {
                            dimover = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"dimfast")) {
                            dimfast = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"dimmid")) {
                            dimmid = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"dimslow")) {
                            dimslow = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"padding")) {
                            padding = (size_t)strtol(value,NULL,0);
                        } else if (!cbf_cistrcmp(attrib_name,"cbftype")) {
                            strncpy(cbftype,value,4);
                            cbftype[4] = '\0';
                        }
                    }

                    cbf_reportnez(cbf_free((void **)&attrib_name,NULL),errorcode);
                    if (value) cbf_reportnez(cbf_free((void **)&value, NULL),errorcode);
                    H5Tclose(attrib_type);
                    H5Sclose(attrib_ds);
                    H5Aclose(attrib_id);
                }

                dataset_ds         = H5Dget_space(dataset_id);
                dataset_type       = H5Dget_type(dataset_id);
                dataset_type_class = H5Tget_class(dataset_type);

                cbf_h5ds_store(handle,objinfo.addr,
                               parent_name,row,
                               "H5_Datasets",
                               dataset_id,
                               dataset_ds,
                               dataset_type,
                               name,0,(void **)&value);

                if (incbfcol&&value) {

                    cbfrow = (int)strtol(name,NULL,0);

                    cbf_get_bookmark(handle,&bookmark);

                    cbf_goto_bookmark(handle,saved_bookmark);

                }

                if (incbfcol&&binsize&&value) {

                    size_t elsize, nelem;

                    cbf_node * column;

                    cbf_file *tempfile;

                    long start;

                    unsigned int localrow, ii;

                    elsize = (bits+CHAR_BIT-1)/CHAR_BIT;

                    nelem = (binsize+elsize-1)/elsize;

                    if (dimover <=0) dimover = nelem;

                    localrow = row;

                    if (nelem > 0 && elsize > 0) {

                        if (incbfcol) {

                            unsigned int rows;

                            cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);

                            if (cbfrow >= rows) {

                                for (ii=rows; ii <= cbfrow; ii++) {

                                    cbf_reportnez(cbf_new_row(handle),errorcode);
                                }

                            }

                            cbf_select_row(handle,cbfrow);

                            localrow = cbfrow;

                        } else {


                            cbf_reportnez(cbf_rewind_datablock(handle),errorcode);

                            if (cbf_find_datablock(handle,"H5")) {

                                cbf_reportnez(cbf_new_datablock(handle,"H5"),errorcode);

                            }


                            cbf_reportnez(cbf_require_category(handle,"H5_Datasets"),errorcode);

                            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

                            cbf_reportnez(cbf_select_row(handle,localrow),errorcode);

                            cbf_reportnez(cbf_require_column(handle,"value"),errorcode);

                        }

                        column = handle->node;

                        if (dataset_type_class == H5T_OPAQUE) {

                            /* If we have stored as an opqaue dataset, keep it that way */


                            /* Remove the old value */

                            cbf_reportnez (cbf_set_columnrow (column, localrow, NULL, 1),errorcode)


                            /* Get the temporary file */

                            cbf_reportnez (cbf_open_temporary (column->context, &tempfile),errorcode)


                            /* Move to the end of the temporary file */

                            if (cbf_set_fileposition (tempfile, 0, SEEK_END))

                                return CBF_FILESEEK | cbf_delete_fileconnection (&tempfile);


                            /* Get the starting location */

                            if (cbf_get_fileposition (tempfile, &start))

                                return CBF_FILETELL | cbf_delete_fileconnection (&tempfile);


                            /* Discard any bits in the buffers */

                            cbf_reportnez (cbf_reset_bits (tempfile),errorcode)

                            /* Add the binary data to the temporary file */

                            if (!cbf_set_output_buffersize(tempfile,binsize))  {

                                memmove((void *)(tempfile->characters+tempfile->characters_used),
                                        (void *)value,binsize);

                                tempfile->characters_used+=binsize;

                            }

                            cbf_onfailnez(cbf_set_bintext(column,localrow,CBF_TOKEN_TMP_BIN,
                                                          binary_id,tempfile,start,binsize,
                                                          1,digest,bits,sign,realarray,byteorder,
                                                          dimover, dimfast, dimmid, dimslow,
                                                          padding,compression),
                                          cbf_delete_fileconnection (&tempfile));

                            cbf_onfailnez(cbf_flush_bits(tempfile),
                                          cbf_delete_fileconnection (&tempfile));
                        } else {

                            /* If this is not an opqaue object, then recompress
                             using the attributes */

                            cbf_reportnez(cbf_set_binary(handle->node,
                                                         handle->row,
                                                         compression,
                                                         binary_id,
                                                         (void *) value,
                                                         (bits+7)/8,
                                                         sign,
                                                         dimover,
                                                         realarray,
                                                         "little_endian",
                                                         dimover,
                                                         dimfast,
                                                         dimmid,
                                                         dimslow,
                                                         padding ), errorcode);

                        }

                    }

                } else {

                    if (incbfcol && value) {

                        unsigned int rows;

                        cbf_reportnez(cbf_count_rows(handle,&rows),errorcode);

                        if (cbfrow >= rows) {

                            cbf_reportnez(cbf_insert_row(handle,cbfrow),errorcode);

                        }

                        cbf_reportnez(cbf_select_row(handle,cbfrow),errorcode);

                        cbf_reportnez(cbf_set_value(handle,value),errorcode);

                        if (cbftype[0] && cbf_cistrcmp(cbftype,"(null)")
                            && strlen(cbftype) == 4) {

                            cbf_reportnez(cbf_set_typeofvalue(handle,cbftype),errorcode);

                        }

                    }

                }

                if (incbfcol&&value) {

                    cbf_reportnez(cbf_goto_bookmark(handle,bookmark),errorcode);

                }
                if (value) cbf_reportnez(cbf_free((void **)&value, NULL),errorcode);
                H5Dclose(dataset_id);
                H5Sclose(dataset_ds);
                H5Tclose(dataset_type);

                break;

            case H5O_TYPE_NAMED_DATATYPE:

                break;

            default:

                return CBF_FORMAT;



        }

        return 0;
    }
    /* Read an HDF5 file */

    int cbf_read_h5file(const cbf_handle handle,
                        const cbf_h5handle h5handle,
                        const int flags) {

        cbf_node *node;

        cbf_h5Ovisit_struct h5Ovisit;

        if (!handle || !h5handle || !h5handle->hfile ) return CBF_ARGUMENT;

        /* Move the flags into the h5handle */

        h5handle -> flags = flags;

        /* Delete the old datablocks */

        if( handle->commentfile) cbf_failnez (cbf_free_file (&(handle->commentfile)));

        cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT));

        cbf_failnez (cbf_set_children (node, 0))

        handle->node = node;

        cbf_failnez (cbf_reset_refcounts(handle->dictionary));

        /* register the CBF compressions if requested */

        if (h5handle->flags & CBF_H5_REGISTER_COMPRESSIONS) {

            if (!H5Zfilter_avail(CBF_H5Z_FILTER_CBF)) {

                cbf_h5failneg(H5Zregister(CBF_H5Z_CBF),CBF_ALLOC);

            }
        }

        h5Ovisit.handle = handle;

        h5Ovisit.h5handle = h5handle;

        h5Ovisit.parent_addr = 0;

        h5Ovisit.parent_id = h5handle->hfile;

        h5Ovisit.parent_name = "/";

        h5Ovisit.grand_parent_name = NULL;

        h5Ovisit.incbf = h5Ovisit.incbfdb = h5Ovisit.incbfcat = h5Ovisit.incbfcol = 0;

        h5Ovisit.innexus = 0;

        h5Ovisit.bookmark.datablock = NULL;

        h5Ovisit.bookmark.category = NULL;

        h5Ovisit.bookmark.column = NULL;

        h5Ovisit.bookmark.haverow = 0;


        cbf_failnez(cbf_alloc ((void **) (&(h5Ovisit.hid_path)), NULL,
                               sizeof(hid_t), 1));

        cbf_failnez(cbf_alloc ((void **) (&(h5Ovisit.haddr_path)), NULL,
                               sizeof(haddr_t), 1));

        h5Ovisit.capacity = 1;

        h5Ovisit.path_size = 0;

        cbf_failnez(cbf_new_datablock(handle,"H5"));

        /* visit the groups in the file, starting with the root group */

        cbf_h5failneg(H5Literate(h5handle->hfile,
                                 H5_INDEX_NAME,
                                 H5_ITER_INC,
                                 NULL,
                                 cbf_object_visit,(void *)&h5Ovisit),CBF_FORMAT);

        return CBF_SUCCESS;

    }
    
    /* get a fast bookmark from the current information in a cbf handle */

    int cbf_get_fast_bookmark(const cbf_handle handle,
                              cbf_fast_bookmark * bookmark) {

        if (!handle || !bookmark) return CBF_ARGUMENT;

        bookmark->node = handle->node;

        bookmark->row = handle->row;

        return CBF_SUCCESS;

    }

    /* go to a fast bookmark in a cbf handle */

    int cbf_goto_fast_bookmark(const cbf_handle handle,
                              const cbf_fast_bookmark bookmark) {

        if (!handle) return CBF_ARGUMENT;

        handle->node = bookmark.node;

        handle->row = bookmark.row;

        return CBF_SUCCESS;

    }

    /* get a bookmark from the current information in a cbf handle */

    int cbf_get_bookmark(const cbf_handle handle,
                         cbf_bookmark * bookmark) {

        if (cbf_datablock_name(handle,&(bookmark->datablock))) {

            bookmark->datablock = NULL;

        }

        if (cbf_category_name(handle,&(bookmark->category))) {

            bookmark->category = NULL;

        }

        if (cbf_column_name(handle,&(bookmark->column))) {

            bookmark->column = NULL;

        }

        bookmark->haverow = 1;

        if (cbf_row_number(handle,&(bookmark->row))) {

            bookmark->row = 0;

            bookmark->haverow = 0;

        }

        return CBF_SUCCESS;

    }

    /* go to a bookmark in the cbf handle */

    int cbf_goto_bookmark(const cbf_handle handle,
                          const cbf_bookmark bookmark) {

        unsigned int rows;

        if (bookmark.datablock) {

            cbf_failnez(cbf_rewind_datablock(handle));

            cbf_failnez(cbf_find_datablock(handle,bookmark.datablock));

            if (bookmark.category) {

                cbf_failnez(cbf_rewind_category(handle));

                cbf_failnez(cbf_find_category(handle,bookmark.category));

                if (bookmark.column) {

                    cbf_failnez(cbf_rewind_column(handle));

                    cbf_failnez(cbf_find_column(handle,bookmark.column));

                    if (bookmark.haverow) {

                        cbf_failnez(cbf_count_rows(handle,&rows));

                        if (bookmark.row < rows) {

                            cbf_failnez(cbf_select_row(handle,bookmark.row));

                        }

                    }

                }


            }

        }

        return CBF_SUCCESS;
	}

#ifdef __cplusplus

}

#endif
