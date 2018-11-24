/**********************************************************************
 * cbf_hdf5.h -- read and write HDF5/NeXus files                      *
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

/** \file cbf_hdf5.h */

#ifndef CBF_HDF5_H
#define CBF_HDF5_H

#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>
#include <hdf5.h>
#include "cbf.h"
#include "cbf_tree.h"
#include <string.h>
#ifdef CBF_USE_ULP
#include <stdint.h>
#ifndef UINT64_MAX
#define NO_UINT64_TYPE
#endif
#endif
    
    
#define CBF_H5_COLUMN_GROUP       0x00
#define CBF_H5_COLUMN_DATASET     0x01
#define CBF_H5_COLUMN_ATTRIBUTE   0x02
#define CBF_H5_COLUMN_MAP_AXES    0x04
#define CBF_H5_COLUMN_PATH        0x08
#define CBF_H5_TEXT               0x10
#define CBF_H5_FLOAT              0x20
#define CBF_H5_INTEGER            0x40
#define CBF_H5_PRIMARY           0x100
#define CBF_H5_TERMINATE        0x1000
#define CBF_H5_SPECIAL          0x2000
#define CBF_H5_ARRAY            0x4000
#define CBF_H5_LINK             0x8000  /* should appear both in the special
field of the target and the dsorat
field of the source */
#define CBF_H5_MORE             0x10000 /* more mappings of same column follow
field of the target and the dsorat
field of the source */
    
    
    
    /** return the maximum of two numeric values */
#define cbf_max(a,b) ((a)>(b)?(a):(b))    
    
    
    typedef enum cbf_axisEquipment_e
	{
		/* actual physical equipment */
		axisEquipment_detector,
		axisEquipment_image,
		axisEquipment_goniometer,
		/* special values */
		axisEquipment_gravity,
		axisEquipment_source,
		/* other/undefined */
		axisEquipment_general
	} cbf_axisEquipment_e;
    
	typedef struct _cbf_axisData_t
	{
		const char * name;  /* either from the last path component or
                             from the name attribute "long_name" */
		hid_t axis;
		cbf_axisEquipment_e equipment;
        int flags;
		struct _cbf_axisData_t * depends_on;
        struct _cbf_axisData_t * rotation_axis;
        struct _cbf_axisData_t * axis_hash_next_name;
        struct _cbf_axisData_t * axis_hash_next_path;
        struct _cbf_axisData_t * axis_hash_next_object;
        ssize_t axisData_index;                  /* the index at which this entry appears */
        const char * axis_path; /* A path to this axis */
        const char * do_path;  
        const char * ra_path;
	}  cbf_axisData_t;
    
/*  Note the number of hash bins must be a power of 2 */
#define CBF_NX2CBF_HASH_BINS 64

    typedef struct cbf_nx2cbf_key_t
	{
		/*
         keys - should not be modified once created.
         */
		const char * array_id;
		int binary_id;
		const char * datablock_id;
		const char * diffrn_id;
		const char * diffrn_detector_id;
		const char * diffrn_detector_element_id;
		const char * diffrn_measurement_id;
		const char * frame_id;
		const char * scan_id;
		const char * wavelength_id;
		/*
         tables - should be modified as data is added and
         will be modified when the table is first created.
         */
		cbf_node * array_data;
		cbf_node * array_element_size;
		cbf_node * array_intensities;
		cbf_node * diffrn;
		cbf_node * diffrn_data_frame;
		cbf_node * diffrn_detector;
		cbf_node * diffrn_detector_element;
		cbf_node * diffrn_measurement;
		cbf_node * diffrn_radiation;
		cbf_node * diffrn_radiation_wavelength;
		cbf_node * diffrn_scan;
		cbf_node * diffrn_scan_frame;
		cbf_node * diffrn_source;
		/*
         number of frames of data in the file - many array
         data items must be scalar, have 1 element or be
         this length.
         */
		hsize_t frames;  /* slowest index */
		hsize_t xdim;    /* fast index */
		hsize_t ydim;    /* mid index */
        hsize_t zdim;    /* slow index */
        int rank;    /* rank including the frame index */
		/* axes */
		cbf_axisData_t * * axisData;
        cbf_axisData_t * axispathhash[CBF_NX2CBF_HASH_BINS];  /* path hash links */
        cbf_axisData_t * axisnamehash[CBF_NX2CBF_HASH_BINS];  /* name hash links */
        cbf_axisData_t * axisobjecthash[CBF_NX2CBF_HASH_BINS];/* object hash links */
		size_t nAxes;
		/* other data */
		int has_scaling_factor;
		int has_offset;
		unsigned int indent;
	} cbf_nx2cbf_key_t;

    
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     cbf_hdf5_common.h
     ****************************************************************/

#define CBF_H5FAIL ((hid_t)(-1))
    
	/**
	\defgroup section_HDF5_H5A
	\defgroup section_HDF5_H5D
	\defgroup section_HDF5_H5F
	\defgroup section_HDF5_H5G
	\defgroup section_HDF5_H5I
	\defgroup section_HDF5_H5O
	\defgroup section_HDF5_H5S
	\defgroup section_HDF5_H5T

	\defgroup section_H5Handle
	\defgroup section_minicbf_config
	*/

    
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
	char * _cbf_str_join(const char * const * const parts, const char sep);
    
	/**
	\brief Check the validity of an object identifier.
	\ingroup section_HDF5_H5I
	*/
	int cbf_H5Ivalid(const hid_t ID);

	/*
	Find/create/free a HDF5 group if it's valid & possibly set the ID to an invalid identifier
    can write requireGroup function as {if (!find(group)) create(group); return group;}
	*/

	/**
	\brief Attempt to create a group.
	\ingroup section_HDF5_H5G
	*/
	int cbf_H5Gcreate(const hid_t location, hid_t * const group, const char * const name);

	/**
	\brief Check if a group exists.
	\ingroup section_HDF5_H5G
	 */
	int cbf_H5Gfind(const hid_t location, hid_t * const group, const char * const name);

	/**
	\brief Ensure a group exists.
	\ingroup section_HDF5_H5G
	 */
	int cbf_H5Grequire(const hid_t location, hid_t * const group, const char * const name);

	/**
	\brief Close a HDF5 group.
	\ingroup section_HDF5_H5G
	 */
	int cbf_H5Gfree(const hid_t ID);

	/* Open/close a HDF5 file if it's valid & possibly set the ID to an invalid identifier */

	/**
	\brief Attempt to open an HDF5 file by file name
	\ingroup section_HDF5_H5F
	 */
	int cbf_H5Fopen(hid_t * const file, const char * const name);

	/**
	\brief Close a HDF5 file
	\ingroup section_HDF5_H5F
	 */
	int cbf_H5Fclose(const hid_t ID);

	/* Attributes */
    
	/**
	\brief Create a new attribute
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Acreate
			(const hid_t location,
			 hid_t * const attr,
			 const char * const name,
			 const hid_t type,
			 const hid_t space);

	/**
	\brief Try to locate an existing attribute
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Afind
			(const hid_t location,
			 hid_t * const attr,
			 const char * const name,
			 const hid_t type,
			 const hid_t space);

	/**
	\brief Read an entire attribute from a file
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Aread
			(const hid_t attr,
			 const hid_t type,
			 void * const buf);

	/**
	\brief Read an entire string attribute from a file
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Aread_string
			(const hid_t attr,
			 const char * * const val);

	/**
	\brief Write an entire attribute to a file.
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Awrite
			(const hid_t attr,
			 const hid_t type,
			 void * const buf);

	int cbf_H5Arequire_cmp
    (const hid_t ID,
     const char * const name,
     const int rank,
     const hsize_t * const dim,
     const hid_t type,
     const void * const value,
     void * const buf,
         int (*cmp)(const void *, const void *, size_t));

    /* We provide a macro and 2 versions of each of the calls with _ULP
     variants. */

#ifdef CBF_USE_ULP
#define CBFM_H5Arequire_cmp2(id,nm,rk,dm,ft,mt,vl,bf,cmp,prm) \
cbf_H5Arequire_cmp2_ULP(id,nm,rk,dm,ft,mt,vl,bf,cmp,prm)
#else
#define CBFM_H5Arequire_cmp2(id,nm,rk,dm,ft,mt,vl,bf,cmp,prm) \
cbf_H5Arequire_cmp2(id,nm,rk,dm,ft,mt,vl,bf,cmp)
#endif

    
	/**
	\brief Check for an attribute with the given space/type/value, or set one if it doesn't exist.
	\ingroup section_HDF5_H5A
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
		int (*cmp)(const void *, const void *, size_t));
	
	/**
	\brief Check for an attribute with the given space/type/value, or set one if it doesn't exist.
	\ingroup section_HDF5_H5A
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
         const void * const cmp_params);

	/**
	\brief Check for a scalar string attribute with a given value, or set one if it doesn't exist.
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Arequire_string
		(const hid_t location,
		const char * const name,
		const char * const value);

	/**
	\brief Close a HDF5 attribute.
	\ingroup section_HDF5_H5A
	 */
	int cbf_H5Afree(const hid_t ID);

	/*  find/create/free hdf5 datasets without directly using hdf5 API */

	/**
	\brief Creates a new dataset in the given location.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Dcreate
		(const hid_t location,
		hid_t * const dataset,
		const char * const name,
		const int rank,
		const hsize_t * const dim,
		const hsize_t * const max,
		const hsize_t * const chunk,
		const hid_t type);

    /* Look for a dataset with the given properties. */
     
	int cbf_H5Dfind
        (const hid_t location,
         hid_t * const dataset,
         const char * const name,
         const int rank,
         const hsize_t * const dim,
         const hsize_t * const max,
         const hsize_t * const chunk,
         const hid_t type);
	
	/**
	\brief Look for a dataset with the given properties.
	\ingroup section_HDF5_H5D
	 */
    int cbf_H5Dfind2
		(const hid_t location,
		hid_t * const dataset,
		const char * const name,
		const int rank,
		const hsize_t * const max,
		hsize_t * const buf,
		const hid_t type);

	/**
	\brief Ensure that a dataset exists, returning a handle to an existing dataset or creating a new dataset if needed.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Drequire
			(const hid_t location,
			 hid_t * const dataset,
			 const char * const name,
			 const int rank,
			 const hsize_t * const max,
			 const hsize_t * const chunk,
			 hsize_t * const buf,
			 const hid_t type);

	/**
	\brief Add some data to a datset, expanding the dataset to the appropriate size if needed.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Dinsert
			(const hid_t dataset,
			 const hsize_t * const offset,
			 const hsize_t * const stride,
			 const hsize_t * const count,
			 hsize_t * const buf,
			 const void * const value,
			 const hid_t type);

	/**
	\brief Change the extent of a chunked dataset to the values in <code>dim</code>.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Dset_extent(const hid_t dataset, const hsize_t * const dim);

	int cbf_H5Dwrite
        (const hid_t dataset,
        const hsize_t * const offset,
        const hsize_t * const stride,
        const hsize_t * const count,
        const void * const value);

	/**
	\brief Add some data to the specified position in the dataset, without checking what (if anything) was there before.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Dwrite2
		(const hid_t dataset,
		const hsize_t * const offset,
		const hsize_t * const stride,
		const hsize_t * const count,
		const void * const value,
		const hid_t type);

	int cbf_H5Dread
		(const hid_t dataset,
		const hsize_t * const offset,
		const hsize_t * const stride,
		const hsize_t * const count,
        void * const value);

	/**
     \brief Extract some existing data from a dataset at a known position.
     \ingroup section_HDF5_H5D
     */
    int cbf_H5Dread_element_as_string
    (const hid_t dataset,
     const hsize_t * const offsets,
     const size_t noffsets,
     char * * value);
    
	/**
	\brief Extract some existing data from a dataset at a known position with memtype.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Dread2
		(const hid_t dataset,
		const hsize_t * const offset,
		const hsize_t * const stride,
		const hsize_t * const count,
		void * const value,
		const hid_t type);

	int cbf_H5Drequire_scalar_F64LE
		(const hid_t location,
		hid_t * const dataset,
		const char * const name,
        const double value);

    /* We provide a macro and 2 versions of each of the calls with _ULP
     variants. */

#ifdef CBF_USE_ULP
#define CBFM_H5Drequire_scalar_F64LE2(loc,ds,nm,val,cmp,prm) \
cbf_H5Drequire_scalar_F64LE2_ULP(loc,ds,nm,val,cmp,prm)
#else
#define CBFM_H5Drequire_scalar_F64LE2(loc,ds,nm,val,cmp,prm) \
cbf_H5Drequire_scalar_F64LE2(loc,ds,nm,val,cmp)
#endif

	/**
	\brief Write a scalar 64-bit floating point number as a dataset with comparison.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Drequire_scalar_F64LE2
		(const hid_t location,
		hid_t * const dataset,
		const char * const name,
		const double value,
		int (*cmp)(const void *, const void *, size_t)
        );

	/**
	\brief Write a scalar 64-bit floating point number as a dataset with a user-defined comparison.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Drequire_scalar_F64LE2_ULP
    (const hid_t location,
     hid_t * const dataset,
     const char * const name,
     const double value,
     int (*cmp)(const void *, const void *, size_t, const void *),
     const void * const cmp_params);

	/**
	\brief Write a single fixed-length string as a dataset.
	\ingroup section_HDF5_H5D
	 */
    int cbf_H5Drequire_flstring
		(const hid_t location,
		hid_t * const dataset,
		const char * const name,
		const char * const value);

	/**
	\brief Close a HDF5 dataset.
	\ingroup section_HDF5_H5D
	 */
	int cbf_H5Dfree(const hid_t ID);

	/* Custom HDF5 types - to get the correct string type for datasets in a consistent way */

	/**
	\brief Get a HDF5 string datatype to describe a string of the specified length.
	\ingroup section_HDF5_H5T
	 */
	int cbf_H5Tcreate_string(hid_t * const type, const size_t len);

	/**
	\brief Close a HDF5 datatype identifier.
	\ingroup section_HDF5_H5T
	 */
	int cbf_H5Tfree(const hid_t ID);

	/* HDF5 dataspace functions: I need a uniform method of creating data spaces to ensure correct operation of comparison functions */

	/**
	\brief Create a dataspace with some given values.
	\ingroup section_HDF5_H5S
	 */
	int cbf_H5Screate
		(hid_t * const ID,
		const int rank,
		const hsize_t * const dim,
		const hsize_t * const max);

	/**
	\brief Close a HDF5 dataspace identifier.
	\ingroup section_HDF5_H5S
	 */
	int cbf_H5Sfree(const hid_t ID);

	/**
	\brief A missing HDF5 function.
	\ingroup section_HDF5_H5O
	*/
	htri_t cbf_H5Ocmp
		(const hid_t id0,
		const hid_t id1);

	/**
	\brief Close a HDF5 object identifier.
	\ingroup section_HDF5_H5O
	 */
	int cbf_H5Ofree(const hid_t ID);


    /****************************************************************
     End of section of code extracted from J. Sloan's
     cbf_hdf5_common.h
     ****************************************************************/
    /****************************************************************
     The following section of code is extracted from J. Sloan's
     config.h
     ****************************************************************/
    
	extern const int cbf_configError_success;

    /* Opaque type for a collection of configuration items */
    struct cbf_config_t;
	typedef struct cbf_config_t cbf_config_t;

	/**
	\brief Obtain a new handle for some configuration settings.
	\ingroup section_minicbf_config
	*/
	cbf_config_t * cbf_config_create( void );

	/**
	\brief Read a minicbf configuration file into the given handle, writing errors to <code>logfile</code>.
	\ingroup section_minicbf_config
	*/
	int cbf_config_parse(FILE * const configFile, FILE * const logFile, cbf_config_t * const vec);

	/**
	\brief Free any heap memory associated with the given cbf_hdf5_configItemVectorhandle object.
	\ingroup section_minicbf_config
	*/
	void cbf_config_free(const cbf_config_t * const vector);

    /**
	\brief Convert a parse error to a descriptive string.
	\ingroup section_minicbf_config
	*/
    const char * cbf_config_strerror(const int error);

    /****************************************************************
     End of section of code extracted from J. Sloan's
     config.h
     ****************************************************************/


    /* Attribute type definition, agrees with CBFlib data convensions */
    
	typedef struct cbf_name_value_pair_def
	{
		const char * name;
		const void * value;
	} cbf_name_value_pair;
    

#define cbf_h5failneg(x,code) {int err; err = (x); if (err < 0) {return (code);}}
#define cbf_h5onfailneg(x,code,y) {int err; err = (x); if (err < 0) {{y;} return (code);}}
#define cbf_h5reportneg(x,code,cerr) \
{int err; if (!(cerr)) {err = (x); if (err < 0) {(cerr)|=code;}}}

#define reportFail(f, errorCode, errorVar) \
do { \
if (CBF_SUCCESS == errorVar && !(f)) { \
errorVar |= (errorCode); \
fprintf(stderr, "%s:%d: '" #f "' failed.\n", __FILE__, __LINE__); \
} \
} while (0)

#define cbf_reportFail(f, errorVar) \
do { \
if (CBF_SUCCESS == errorVar) { \
const int errorCode = (f); \
if (CBF_SUCCESS != errorCode) \
errorVar |= errorCode; \
} \
} while (0)

    
#ifndef H5_VERS_MAJOR
#define H5_VERS_MAJOR 0
#endif
    
#ifndef H5_VERS_MINOR
#define H5_VERS_MINOR 0
#endif
    
    
#if (H5_VERS_MAJOR>1)||((H5_VERS_MAJOR==1)&&(H5_VERS_MINOR>=8))
    
#define H5Acreatex(loc_id,name,type_id,space_id,acpl_id) \
H5Acreate2(loc_id,name,type_id,space_id,acpl_id,H5P_DEFAULT)
#define H5Dcreatex(loc_id,name,type_id,space_id,dcpl_id) \
H5Dcreate2(loc_id,name,type_id,space_id,H5P_DEFAULT,dcpl_id,H5P_DEFAULT)
#define H5Gcreatex(loc_id,name) \
H5Gcreate2(loc_id,name,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT)
#define H5Gopenx(loc_id,name) H5Gopen2(loc_id,name,H5P_DEFAULT)
    
#else
    
#error HDF5 Version >= 1.8 Required
    
#endif
    
    /* CBF Bookmark */
    
    typedef struct {
        const char * datablock;
        const char * saveframe;
        const char * category;
        const char * column;
        unsigned int row;
        int haverow;
    } cbf_bookmark;
    
    /* CBF Fast Bookmark */
    
    typedef struct {
        cbf_node *node;
        int row;
    } cbf_fast_bookmark;
    
    
    /* H5File structure */
    
    typedef struct
    {
        int   rwmode;  /* 0 for read-only, 1 for read-write */
		unsigned int slice; /* The slice within the HDF5 data arrays where data will be added */
        unsigned int block; /* The block of data arrays, or zero if no blocking */
        unsigned int blocksize;  /* The number of arrays in a block */
        unsigned int num_detectors; /* The number of NXdetector groups */
        unsigned int cap_detectors; /* The capacity in terms of NXdetector groups */
        unsigned int cur_detector;  /* The currently active detector, numbered from 1 */
		hid_t hfile;   /* The HDF5 file */
		hid_t nxid;    /* /entry@NXentry */
		hid_t nxdata; /* /entry/data@NXdata */
		hid_t nxsample; /* /entry/sample@NXsample group */
		hid_t nxbeam; /* /entry/beam@NXbeam group */
		hid_t nxinst;  /* /entry/instrument@NXinstrument */
        hid_t nxdetector_group; /* /entry/instrument/detector_group@NXdetector_group */
		hid_t * nxdetectors; /* /entry/instrument/detector@NXdetector */
		hid_t nxmonochromator;  /* /entry/instrument/monochromator@NXmonochromator */
        hid_t nxgoniometer;  /* /entry/instrument/goniometer@NXgoniometer */
		hid_t nxsource;  /* /entry/instrument/source@NXsource */
        hid_t rootid;  /* The root CBF database group */
        hid_t dbid;    /* The current datablock in the CBF */
        hid_t sfid;    /* The current saveframe in the current datablock or -1 */
        hid_t catid;   /* The current category */
        hid_t colid;   /* The current column */
        hid_t curnxid; /* The current NeXus group */
        hid_t dataid;  /* The NeXus NXdata group */
		/* Names of various groups, used to construct paths to the axes */
		const char * nxid_name;
        const char * nxdetector_group_name;
		const char * * nxdetector_names;
        const char * scan_id; /* a unique identifier for the scan */
        const char * sample_id; /* a unique identifier for the sample */
        const char * nxsample_name;
        const char * nxbeam_name;
        const char * nxinstrument_name;
        const char * nxgoniometer_name;
        const char * nxmonochromator_name;
        const char * nxsource_name;
        const char * nxdata_name;
        const char * nxfilename;
        const char * dbid_name;
        const char * sfid_name;
        const char * catid_name;
        const char * colid_name;
                /* Names of corresponding CBF datablock, saveframe, category, column
                   each case these must have already been created in the cbf and should
                   not be freed or modified
                */ 
        const char * cbf_datablock;
        const char * cbf_saveframe;
        const char * cbf_category;
        const char * cbf_column;

		/* Flags for various options */
        unsigned long int flags;
#ifdef CBF_USE_ULP
		/* Parameters controlling floating point comparisons */
		int cmp_double_as_float;
		unsigned int float_ulp;
#ifndef NO_UINT64_TYPE
		uint64_t double_ulp;
#endif
#endif
        cbf_bookmark
        bookmark;/* Read bookmark to save names for paths */
        cbf_handle scratch_tables;
        FILE * logfile;
    }
    cbf_h5handle_struct;
    
    typedef cbf_h5handle_struct *cbf_h5handle;
    
    typedef struct
    {
        cbf_handle handle;
        cbf_h5handle h5handle;
        hid_t parent_id;
        haddr_t parent_addr;
        const char * great_grand_parent_name;
        const char * grand_parent_name;
        const char * parent_name;
        /* when at column, parent is category,
           grand-parent is data-block (or save frame if there is a grand parent)
           and great_grand_parent is data-block for a save_fame */

        size_t capacity;
        size_t path_size;
        hid_t *hid_path;
        haddr_t *haddr_path;
        cbf_bookmark bookmark; /* bookmark in the CBF */
        int incbf;     /* set to 1 when we have descended
                        into a NeXus NXcbf    */
        int incbfdb;   /* set to 1 when we have descended
                        into a NeXus NXcbfdb  */
        int incbfsf;   /* set to 1 when we have descended
                        into a NeXus NXcbfsf  */
        int incbfcat;  /* set to 1 when we have descended
                        into a NeXus NXcbfcat */
        int incbfcol;  /* set to 1 when we have descended
                        into a NeXus NXcbfcol */
        int innexus;   /* set to 1 when we have descended
                        into a NeXus NXexntry */
        int innxpdb;   /* set to 1 when we have descended
                        into a NeXus NXpdb, updated by 1
                        for each subgroup level */
    }
    cbf_h5Ovisit_struct;
    
	typedef cbf_h5Ovisit_struct *cbf_h5Ovisithandle;
    
	/**
	\brief Get the current id of the file within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_file
			(const cbf_h5handle nx,
			 hid_t * const file);
    
	/**
	\brief Set the id of the file within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_file
			(const cbf_h5handle nx,
			 const hid_t file);

        /**
        \brief Get the current id and name of the data group within the given handle.
        \ingroup section_H5Handle
         */
        int cbf_h5handle_get_data
                        (const cbf_h5handle nx,
                         hid_t * const group,
                         const char * * const name);

	/**
	\brief Get the current id and name of the entry group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_entry
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);
    
	/**
	\brief Set the id and name of the entry group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_entry
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);
    
	/**
	\brief Ensure I have an entry in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_entry
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

	/**
     \brief Ensure I have an entry in the hdf5 handle with definition
     \ingroup section_H5Handle
	 */
	int cbf_h5handle_require_entry_definition
    (const cbf_h5handle nx,
     hid_t * const group,
     const char * name,
     const char * definition,
     const char * version,
     const char * URL);

	/**
	\brief Get the current id and name of the sample group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_sample
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the sample group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_sample
			(const cbf_h5handle nx,
			 const hid_t group,
                                      const char * const name);
    
	/**
	\brief Ensure I have a sample in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_sample
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);
    
	/**
	\brief Get the current id and name of the cbfdb group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_cbfdb
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the cbfdb group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_cbfdb
			(const cbf_h5handle nx,
			 const hid_t group,
                                      const char * const name);
    
	/**
	\brief Ensure I have a cfbdb in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_cbfdb
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);
    
	/**
	\brief Get the current id and name of the beam group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_beam
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the beam group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_beam
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
	\brief Ensure I have a beam in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_beam
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

	/**
	\brief Get the current id and name of the instrument group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_instrument
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the instrument group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_instrument
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
	\brief Find an existing instrument group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_find_instrument
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Ensure I have an instrument in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_instrument
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

    
	/**
     \brief Get the current id and name of the detector_group group within the given handle.
	\ingroup section_H5Handle
	 */
    int cbf_h5handle_get_detector_group
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
     \brief Set the id and name of the detector_group group within the given handle.
	\ingroup section_H5Handle
	 */
    int cbf_h5handle_set_detector_group
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
     \brief Ensure I have a detector_group in the hdf5 handle.
	\ingroup section_H5Handle
	 */
    int cbf_h5handle_require_detector_group
			(const cbf_h5handle nx,
			 hid_t * const group,
     const char * name);

    
    /**
	\brief Get the current id and name of the detector group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_detector
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the detector group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_detector
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
	\brief Ensure I have a detector in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_detector
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

	/**
	\brief Get the current id and name of the goniometer group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_goniometer
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the goniometer group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_goniometer
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
	\brief Ensure I have a goniometer in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_goniometer
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

	/**
	\brief Get the current id and name of the monochromator group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_monochromator
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the monochromator group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_set_monochromator
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
	\brief Ensure I have a monochromator in the hdf5 handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_require_monochromator
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

	/**
	\brief Get the current id and name of the source group within the given handle.
	\ingroup section_H5Handle
	 */
	int cbf_h5handle_get_source
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * * const name);

	/**
	\brief Set the id and name of the source group within the given handle.
	\ingroup section_H5Handle
	*/
	int cbf_h5handle_set_source
			(const cbf_h5handle nx,
			 const hid_t group,
			 const char * const name);

	/**
	\brief Ensure I have a source in the hdf5 handle.
	\ingroup section_H5Handle
	*/
	int cbf_h5handle_require_source
			(const cbf_h5handle nx,
			 hid_t * const group,
			 const char * name);

    /* Create a dotted CBF location string
     returns a newly allocated string that
     must be freed */
    
    int cbf_location_string(const char* datablock,
                            const char* category,
                            const char* column,
                            unsigned int row,
                            char * * stringout);
    
    /* Conatenate two strings, returning a newly allocated string */
    
    int cbf_strcat(const char * string1, const char * string2,
                   char * * stringout);
    
    /* Either open or create a NeXus group*/
    
    int cbf_require_nxgroup(cbf_h5handle h5handle,
                            const char * nxgroup,
                            const char * nxclass,
                            hid_t parent_id,
                            hid_t * groupid);

    
    /* get an axis vector and offset */
    
    
    int cbf_get_axis_vector_and_offset(cbf_handle handle,
                                       const char *axis_id,
                                       double vector[3],
                                       double offset[3]);
    
    /* Compute the cross-product of 2 3-vectors */
    
    int cbf_cross_product(double vecin1[3],
                          double vecin2[3],
                          double vecout[3] );
    
    /* compute the L2 norm of a 3-vector */
    
    double cbf_norm(double vector[3]);
    
    /* compute the product of a scalar and a vector */
    
    int cbf_scalar_product(double scalar, double vecin[3], double vecout[3]);
    
    
    /* Apply a matrix to a vector */
    
    int cbf_apply_matrix(double matrix[3][3], double vecin[3], double vecout[3]);
    
    /* compute the transform from CBF vectors to NeXus vectors
     Use the transpose to transfrom from NeXus vectors to CBF*/
    
    int cbf_get_NX_axis_transform(cbf_handle handle,
                                  double matrix [3][3]);
    
    
    
    
    /* Get a nexus detector name */
    
    int cbf_get_NX_detector_name(cbf_h5handle h5handle,
                                 unsigned int row,
                                 const char * * detector_element);

    
    /* Set a nexus detector name */
    
    int cbf_set_NX_detector_name(cbf_h5handle h5handle, const char * detector_element);
    
    /* Count the nexus detector names */
    
    int cbf_count_NX_detector_names(cbf_h5handle h5handle, unsigned int * count);
    
    
    /* get the Nexus axis path if previously established.
     If not, try to create both the path and the intervening groups
     */
    
    int cbf_require_NX_axis_path(cbf_handle handle,
                                 cbf_h5handle h5handle,
                                 const char * axis_id,
                                 const char * * nexus_path);

    
    /* Get the nexus path of an axis, if previously set */
    
    int cbf_get_NX_axis_path(cbf_h5handle h5handle, const char * axis_id, const char * * nexus_path);

    /* Get the nexus poise path of an axis, if previously set. */
    
    int cbf_get_NX_axis_poise_path(cbf_h5handle h5handle, const char * axis_id, const char * * poise_path);

    /* Set the parent path of an axis */
    
    int cbf_set_NX_parent_path(cbf_h5handle h5handle, const char * axis_id, const char * parent_path);
    
    /* Get the flag that this is an array_axis to the value given */
    
    int cbf_get_NX_axis_array_axis(cbf_h5handle h5handle,
                                   const char * axis_id, int *flag);

    /* Set the flag that this is an array_axis to the value given */
    
    int cbf_set_NX_axis_array_axis(cbf_h5handle h5handle,
                                   const char * axis_id, const int flag);
        
    
    /* increment the count of axes dependent on this axis */
    
    int cbf_increment_NX_axis_depcount(cbf_h5handle h5handle, const char * axis_id);

    /* Set the nexus path of an axis */
    
    int cbf_set_NX_axis_path(cbf_h5handle h5handle, const char * axis_id, const char * nexus_path);

    /* Set the parent path of an axis */
    
    int cbf_set_NX_parent_path(cbf_h5handle h5handle, const char * axis_id, const char * parent_path);
    
    
    /* Write the HDF5 version of the NeXus axis definitions, if
         the original CBF had axis definitions */

    
    int cbf_write_h5nxaxes(cbf_handle handle, cbf_h5handle h5handle);
    
    
    /* apply a double vector attribute to a group or dataset */
    
    int cbf_apply_h5vector_attribute(hid_t hid,
                                     const char* attribname,
                                     const double* attribvec,
                                     const size_t dimension,
                                     int errorcode);
    
    /* apply a long attribute to a group or dataset */
    
    int cbf_apply_h5longasstr_attribute(hid_t hid,
                                        const char* attribname,
                                        const long attriblong,
                                        int errorcode);
    
    /* apply an integer attribute to a group or dataset as a string */
    
    int cbf_apply_h5intasstr_attribute(hid_t hid,
                                       const char* attribname,
                                       const int attribint,
                                       int errorcode);
    
    /* apply a integer attribute to a group or dataset */
    
    int cbf_apply_h5integer_attribute(hid_t hid,
                                      const char* attribname,
                                      const int attribint,
                                      int errorcode);
    
    /* apply a text attribute to a group or dataset */
    
    int cbf_apply_h5text_attribute(hid_t hid,
                                   const char* attribname,
                                   const char* attribtext,
                                   int errorcode);
    
    /* apply a text attribute slab to a dataset
     
     Actually works with a text data set and makes
     the attribute be an object reference to that
     dataset.  The name of the dataset is the
     name of the attribute with the optional
     prefix and suffix
     
     */
    
    int cbf_add_h5text_attribute_slab(hid_t datasetid,
                                      hid_t groupid,
                                      const char* attributename,
                                      const char* attributetext,
                                      const char* datasetnameprefix,
                                      const char* datasetnamesuffix,
                                      const hsize_t slab,
                                      int errorcode);
    
    
    
    /* find a text dataset slab in a dataset in a group, with slab location
     
     places the specified datasettext in some slab of the
     specified datasetname for group hid.  The slab where the text is
     found or placed is reported in textslab.
     
     If the new text is the same as the existing text in any slab, nothing changes.
     A case-insensitive compare is used.
     
     The slabs are indexed from 0
     
     */
    
    int cbf_find_h5text_dataset_slab(hid_t hid,
                                     const char* datasetname,
                                     const char* datasettext,
                                     hsize_t * textslab,
                                     int errorcode);

    
    /* require a text dataset slab in a dataset in a group
     
     places the specified datasettext in some slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.
     
     If the new text is the same as the existing text in any slab, nothing changes.
     A case-insensitive patch is used.
     
     The first slab goes in rank = 0.  Additional slabs go in rank 1.
     
     If the new text is different, a new slab is appended.
     
     The slabs are indexed from 0
     
     */
    
    int cbf_require_h5text_dataset_slab(hid_t hid,
                                        const char* datasetname,
                                        const char* datasettext,
                                        int errorcode);


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
                                      int errorcode);
    
    /* get the length of text list attribute from a group or dataset
     this uses variable length strings*/
    
    int cbf_get_h5text_list_attribute_length(hid_t hid,
                                             const char* attribname,
                                             size_t *length,
                                             int errorcode);
    
    /* apply a text list attribute to a group or dataset
     this uses variable length strings*/
    
    int cbf_apply_h5text_list_attribute(hid_t hid,
                                        const char* attribname,
                                        const size_t length,
                                        const char** attribtext,
                                        int errorcode);
    
    /* read a text list attribute from a group or dataset
     this uses variable length strings.  To use this code,
     The calling routine should first call
     cbf_get_h5text_list_attribute_length and allocate
     attribtext as an array of char * of that length.
     The call to the cbf_get_h5text_list_attribute will allocate
     the individual text strings, or return NULL for each
     string.
     
     */
    
    int cbf_get_h5text_list_attribute(hid_t hid,
                                      const char* attribname,
                                      const size_t length,
                                      const char** attribtext,
                                      int errorcode);
    
    
    /* add a text list attribute slab to a text list attribute of a group or dataset
     this uses variable length strings*/
    
    int cbf_add_h5text_list_attribute_slab(hid_t hid,
                                           const char* attribname,
                                           const char* attribtext,
                                           const hsize_t slab,
                                           int errorcode);
    
    /* add a double dataset to a group */
    
    int cbf_add_h5double_dataset(hid_t hid,
                                 const char* datasetname,
                                 const double datasetvalue,
                                 int errorcode);
    
    /* add a double dataset array slab to a double dataset array dataset */
    

    
    int cbf_add_h5double_dataset_slab(hid_t hid,
                                      const char* datasetname,
                                      const double datasetvalue,
                                      const hsize_t slab,
                                      int errorcode);
    
    
    /* add a double vector dataset slab to a group
     
     places the specified dataset vector in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.
     
     The slabs are indexed from 0 in the slowest dimension
     
     */
    
    int cbf_add_h5double_vector_dataset_slab(hid_t hid,
                                             const char* datasetname,
                                             const double* datasetvalue,
                                             const size_t dimension,
                                             const hsize_t slab,
                                             int errorcode);
    
    /* Count the number of links in an HDF5 group */
    
    int cbf_count_h5group_links (const hid_t parent,
                                 const char * group,
                                 hsize_t * numlinks);
    
    /* Count the number of attributes of an HDF5 object */
    
    int cbf_count_h5object_attributes (const hid_t parent,
                                       const char * object,
                                       hsize_t * numattribs);
    
    /* Find an hdf5 group link by name pattern */
    
    int cbf_find_h5group_link(const hid_t parent,
                              const char * group,
                              const char * name_pattern,
                              hsize_t search_index,
                              hsize_t *found_index,
                              char ** name);
    
    /* Find an hdf5 attribute by name pattern */
    
    int cbf_find_h5attribute(const hid_t parent,
                             const char * object_name,
                             const char * name_pattern,
                             hsize_t search_index,
                             hsize_t *found_index,
                             char ** name );

    
    /* Get the rank of a dataset */
    
    int cbf_get_h5dataset_rank(hid_t hid, const char * datasetname, size_t * rank);
    
    /* Get the dimensions of a dataset */
    
    int cbf_get_h5dataset_dims(hid_t hid, const char * datasetname, size_t rank,
                               size_t * dims,
                               size_t * maxdims);

    
    /* apply a text dataset to a group */
    
    int cbf_add_h5text_dataset(hid_t hid,
                                 const char* datasetname,
                                 const char* datasettext,
                                 int errorcode);
    
    /* add a long dataset slab to a group
     
     places the specified datasetvalue in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.
     
     The slabs are indexed from 0
     
     */
    
    int cbf_add_h5long_dataset_slab(hid_t hid,
                                    const char* datasetname,
                                    const long datasetvalue,
                                    const hsize_t slab,
                                    int errorcode);
    
    /* add a long dataset to a group */
    
    int cbf_add_h5long_dataset(hid_t hid,
                               const char* datasetname,
                               const long datasetvalue,
                               int errorcode);
    
    
    /* add an unsigned long dataset slab to a group
     
     places the specified datasetvalue in the specified slab of the
     specified datasetname for group hid.  The dataset is created
     if it does not already exist.
     
     The slabs are indexed from 0
     
     */
    
    int cbf_add_h5ulong_dataset_slab(hid_t hid,
                                     const char* datasetname,
                                     const unsigned long datasetvalue,
                                     const hsize_t slab,
                                     int errorcode);

    /* add an unsigned long dataset to a group */
    
    int cbf_add_h5ulong_dataset(hid_t hid,
                                const char* datasetname,
                                const long datasetvalue,
                                int errorcode);

    
    /* Write a binary value to an HDF5 file */
    
    int cbf_write_h5binary (cbf_handle handle,
                            cbf_node *column,
                            unsigned int row,
                            cbf_h5handle h5handle);
    
    /* Write an ascii value to an HDF5 file */
    
    int cbf_write_h5ascii (cbf_handle handle,
                           unsigned int row,
                           const char *string,
                           cbf_h5handle h5handle);
    
    /* Write a value to an HDF5 file */
    
    int cbf_write_h5value (cbf_handle handle, cbf_node *column, unsigned int row,
                           cbf_h5handle h5handle);
    
    /* Write a category to an HDF5 file */
    
    int cbf_write_h5category (cbf_handle handle,
                              const cbf_node *category,
                              cbf_h5handle h5handle);
    
    /*  create top-level NXentry */
    
    int cbf_create_NXentry(cbf_h5handle h5handle);
    
    /*  Create an HDF5 Group below NX entry or below curnxid */
    
    int cbf_H5Gcreate_in_handle(cbf_h5handle h5handle,
                                const char * groupname,
                                hid_t * newgroup);
    
    /*  Create an HDF5 NeXus Group below NX entry or below curnxid */
    
    int cbf_H5NXGcreate(cbf_h5handle h5handle,
                        const char * groupname,
                        const char * nxclass,
                        hid_t * newgroup);
    
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
                                 const char * oldgroupname);
    
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
                                   const char * oldgroupname);
    
    /* Free an H5File handle */
    
	/**
	\brief Free a handle for an HDF5 file.
	\ingroup section_H5Handle
	*/
    int cbf_free_h5handle(cbf_h5handle h5handle);
    
    /* Make an (empty) H5File handle */
    
    int cbf_make_h5handle(cbf_h5handle *h5handle);
    
    /* Close the current saveframe in an HDF5 file */
    
    int cbf_close_h5saveframe (cbf_h5handle h5handle);
    
    /* Write a saveframe name to an HDF5 file
     Make a new group of NeXus class NXcifsf in the NXcif current datablock
     */
    
    int cbf_write_h5saveframename (const cbf_node *saveframename,
                                   cbf_h5handle h5handle);
    
    /* Write a datablock name to an HDF5 file
     Make a new group of NeXus class NXcifdb in the NXcif class root
     */
    
    int cbf_write_h5datablockname (const cbf_node *datablock, cbf_h5handle h5handle);
    
    /* Write a node to an HDF5 file */
    
    int cbf_write_h5node (cbf_handle handle, const cbf_node *node,
                          const cbf_h5handle h5handle);
    
    /* Create an H5File handle */
    
    int cbf_create_h5handle(cbf_h5handle *h5handle, const char * h5filename);
    
    /* Create an H5File handle for NXpdb */
    
    int cbf_create_h5handle_nxpdb(cbf_h5handle *h5handle, const char * h5filename);
    
    
    /* Require the filename in the H5 file handle */
    
    int cbf_require_h5handle_filename(cbf_h5handle h5handle);

    
	/* Create an HDF5 File handle without adding an NXcbf group to it */
    
	int cbf_create_h5handle2(cbf_h5handle *h5handle,const char * h5filename);
    
    /* Create an HDF5 File handle without adding a CBF_cbf group to it
     in update mode*/
    
    int cbf_create_h5handle2u(cbf_h5handle *h5handle,const char * h5filename);
    
	/**
	\brief Allocates space for a HDF5 file handle and associates it with the given file.
	\ingroup section_H5Handle
	*/
	int cbf_create_h5handle3(cbf_h5handle * handle, hid_t file);

    /*  Write cbf to HDF5 file hfile */
    
	int cbf_write_h5file (cbf_handle handle, cbf_h5handle h5handle, unsigned long int flags);
    
	/**
	\brief Extract the data from a CBF file & put it into a NeXus file.
	\ingroup section_H5Handle
	 */
	int cbf_write_cbf_h5file
			(cbf_handle handle,
			 cbf_h5handle h5handle);

	/**
	\brief Extract the data from a CBF file & put it into a NeXus file.
	\ingroup section_H5Handle
	 */
	int cbf_write_cbf2nx
			(cbf_handle handle,
			 cbf_h5handle h5handle,
			 const char * const datablock,
			 const char * const scan,
			 const int list);

    
    /**
     \brief Extract the data from a CBF file & put it into a NeXus metadata file data file.
     \ingroup section_H5Handle
     */
    int cbf_write_cbf2nx2
    (cbf_handle handle,
     cbf_h5handle h5handle,
     cbf_h5handle h5datahandle,
     const char * const datablock,
     const char * const scan,
     const int list);

	/**
	\brief Extract the data from a miniCBF file & put it into a NeXus file.
	\ingroup section_H5Handle
	 */
	int cbf_write_minicbf_h5file (cbf_handle handle, cbf_h5handle h5handle, const cbf_config_t * const axisConfig);

	/**
	\brief Extract data from a nexus file and store it in a CBF file
	\ingroup section_H5Handle
	*/
	int cbf_write_nx2cbf
			(cbf_h5handle nx,
			 cbf_handle cbf);

    /* Open an HDF5 File handle */
    
    int cbf_open_h5handle(cbf_h5handle *h5handle,
                          const char * h5filename);
    
    /* Convert an HDF5 typeclass to a string
     and flag for atomic or not
     copies up to n-1 characters of the
     type string to buffer*/
    
    int cbf_h5type_class_string(H5T_class_t type_class,
                                char * buffer,
                                int * atomic, size_t n );
    
    
    /* Store an HDF5 Dataset in CBF handle as a column, using
     category categoryname, ...
     If target_row is -1, the new column is appended to any
     existing column
     If target row is >= 0, overwrites any existing rows starting
     at target_row
     
     */
    
    int cbf_h5ds_store_as_column(cbf_handle handle,
                             int target_row,
                       const char * columnname,
                       const char * categoryname, 
                       hid_t obj_id,
                       hid_t space, 
                       hid_t type,
                       void ** value);
    
    
    /* Store an HDF5 Dataset in CBF handle, using
     category categoryname, ...*/
    
    int cbf_h5ds_store(cbf_handle handle, haddr_t parent,
                       const char * parent_name,
                       const int target_row,
                       const char * categoryname, hid_t obj_id,
                       hid_t space, hid_t type,
                       const char * name,
                       const int readattrib,
                       void ** value);
    
    
    /* Callback routine for objects in a group */
    
    
    herr_t cbf_object_visit(hid_t loc_id, const char *name,
                            const H5L_info_t *info,
                            void *op_data);
    
    
    /* Read an HDF5 file */
    
    int cbf_read_h5file(const cbf_handle handle,
                        const cbf_h5handle h5handle,
                        const unsigned long int flags);
    
    /* get a fast bookmark from the current information in a cbf handle */
    
    int cbf_get_fast_bookmark(const cbf_handle handle, cbf_fast_bookmark * bookmark);
    
    /* go to a fast bookmark in a cbf handle */
    
    int cbf_goto_fast_bookmark(const cbf_handle handle, const cbf_fast_bookmark bookmark);
    
    
    /* go to a bookmark in the cbf handle */
    
    int cbf_goto_bookmark(const cbf_handle handle, const cbf_bookmark bookmark);
    
    /* get a bookmark from the current information in a cbf handle */
    
    int cbf_get_bookmark(const cbf_handle handle, cbf_bookmark * bookmark);
    
    int cbf_map_h5value(
                        const char * const name, const char * const value,
                        const char * const group, const char * const groupNXclass,
                        const char * const subGroup, const char * const subGroupNXclass,
                        const size_t attrc, const cbf_name_value_pair * const attrv,
                        cbf_h5handle h5handle);
    
    int cbf_cimatch(const char * name, const char * pattern);
    
    
#ifdef __cplusplus

}

#endif


#endif /* CBF_HDF5_H */
