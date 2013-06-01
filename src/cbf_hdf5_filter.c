/**********************************************************************
 * cbf_hdf5_filter.c -- hdf5 filter interface to CBF compressions     *
 *                                                                    *
 * Version 0.9.3 13 May 2013                                          *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2013 Herbert J. Bernstein                            *
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

#include "cbf_hdf5_filter.h"
#include "cbf_codes.h"
#include "cbf_context.h"
#include "cbf_compress.h"
#include "cbf_alloc.h"
#include <string.h>

    static herr_t cbf_h5z_set_local(hid_t dcpl_id, 
                                    hid_t type_id, 
                                    hid_t space_id);

    static size_t cbf_h5z_filter(unsigned int flags,
                                 size_t cd_nelmts,
                                 const unsigned int cd_values[],
                                 size_t nbytes,
                                 size_t *buf_size,
                                 void **buf);

    
    const H5Z_class2_t H5Z_CBF[1] = {{
        H5Z_CLASS_T_VERS,                   /* H5Z_class_t version */
        (H5Z_filter_t)CBF_H5Z_FILTER_CBF,   /* Filter id number             */
        1,                                  /* encoder_present flag (set to true) */
        1,                                  /* decoder_present flag (set to true) */
        "HDF5 CBF compression filters",     /* Filter name for debugging    */
        NULL,                               /* The "can apply" callback     */
        (H5Z_set_local_func_t)
                    cbf_h5z_set_local,      /* The "set local" callback     */
        (H5Z_func_t)cbf_h5z_filter,         /* The actual filter function   */
    }};

#ifdef CBF_H5_SETUP_FILTER_PLUGIN
    H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_FILTER;}
    const void *H5PLget_plugin_info(void) {return H5Z_CBF;}
#endif

    static herr_t cbf_h5z_set_local(hid_t dcpl_id, 
                                    hid_t type_id, 
                                    hid_t space_id){
        unsigned filt_flags, flags;
        size_t filt_cd_nelmts=CBF_H5Z_FILTER_CBF_NELMTS;
        unsigned filt_cd_values[CBF_H5Z_FILTER_CBF_NELMTS],
                      cd_values[CBF_H5Z_FILTER_CBF_NELMTS];
        herr_t errorcode = 0;
        hid_t base_type, native_type;
        H5T_class_t type_class, base_type_class;
        H5T_class_t native_type_class;
        char h5t_type_class[14], h5t_base_type_class[14];
        size_t type_size, total_size, total_dim;
        H5T_order_t type_order;
        H5T_sign_t type_sign;
        int atomic;
        int ndims, kdims, ii;
        unsigned int filter_config;
        hsize_t dims[H5S_MAX_RANK];
        hsize_t maxdims[H5S_MAX_RANK];
        
        /* Get the existing filter data */
        
        if (H5Pget_filter_by_id2(dcpl_id,
                                CBF_H5Z_FILTER_CBF,
                                &filt_flags,
                                &filt_cd_nelmts,
                                filt_cd_values,0,NULL,&filter_config) < 0){
            filt_cd_nelmts = 0;
            errorcode=CBF_FORMAT;
        }
        
        /* Get the type and space information */
        
        type_class = H5Tget_class(type_id);
        base_type = CBF_H5FAIL;
        type_order = -1;
        kdims= ndims = H5Sget_simple_extent_ndims(space_id);
        if (ndims <= 0) ndims = 1;
        H5Sget_simple_extent_dims(space_id,dims,maxdims);
        
        /* extract the parameters of the type and space */
        
        if (!cbf_h5type_class_string(type_class,
                                     h5t_type_class,&atomic,14)) {
            if (!atomic && type_class==H5T_ARRAY){
                base_type = H5Tget_super(type_id);
                
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
                    }
                } else {
                    strncpy (h5t_base_type_class,".",14);
                    cbf_h5failneg(H5Tclose(base_type),CBF_FORMAT);
                    base_type = CBF_H5FAIL;
                }
            } else if (atomic) {
                type_size = H5Tget_size(type_id);
                type_order = H5Tget_order(type_id);
                type_sign = H5Tget_sign(type_id);
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
        native_type = H5Tget_native_type(type_id,H5T_DIR_ASCEND);
        native_type_class = H5Tget_class(native_type);
        cd_values[CBF_H5Z_FILTER_CBF_DIMFAST]
          =cd_values[CBF_H5Z_FILTER_CBF_DIMMID]
          =cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW]=1;
        if (type_class==H5T_INTEGER){
            cd_values[CBF_H5Z_FILTER_CBF_ELSIGN] = (type_sign==H5T_SGN_2)?1:0;
            cd_values[CBF_H5Z_FILTER_CBF_REAL] = 0;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIZE] = type_size;
        } else if (type_class==H5T_FLOAT) {
            cd_values[CBF_H5Z_FILTER_CBF_ELSIGN] = 1;
            cd_values[CBF_H5Z_FILTER_CBF_REAL] = 1;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIZE] = type_size;
        } else {
            cd_values[CBF_H5Z_FILTER_CBF_ELSIGN] = 0;
            cd_values[CBF_H5Z_FILTER_CBF_REAL] = 0;
            cd_values[CBF_H5Z_FILTER_CBF_ELSIZE] = 1;
        }
        if (total_dim > 1) {
            cd_values[CBF_H5Z_FILTER_CBF_DIMFAST] = dims[kdims-1];
            if (kdims > 1) cd_values[CBF_H5Z_FILTER_CBF_DIMMID] = dims[kdims-2];
            if (cd_values[CBF_H5Z_FILTER_CBF_DIMMID] < 1)
                cd_values[CBF_H5Z_FILTER_CBF_DIMMID]=1;
            if (kdims > 2) cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW] = total_dim/
                (cd_values[CBF_H5Z_FILTER_CBF_DIMFAST]
                 *cd_values[CBF_H5Z_FILTER_CBF_DIMMID]);
            if (cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW] < 1)
                cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW]=1;

        }

        if (filt_cd_nelmts > CBF_H5Z_FILTER_CBF_COMPRESSION ) {
            cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION]
            = filt_cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION];
        } else {
            cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION] = CBF_NONE;
        }

        if (filt_cd_nelmts > CBF_H5Z_FILTER_CBF_MIME_FLAG ) {
            cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG]
            = filt_cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG];
        } else {
            cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG] = 1;
        }

        
        if (filt_cd_nelmts > CBF_H5Z_FILTER_CBF_PADDING
            && filt_cd_values[CBF_H5Z_FILTER_CBF_PADDING] > 0 ) {
            cd_values[CBF_H5Z_FILTER_CBF_PADDING] = filt_cd_values[CBF_H5Z_FILTER_CBF_PADDING];
        } else {
            cd_values[CBF_H5Z_FILTER_CBF_PADDING] = 0;
        }
        
        /* Modify or set the filter params */
        cbf_h5reportneg(H5Pmodify_filter(dcpl_id, CBF_H5Z_FILTER_CBF, flags,
                                         CBF_H5Z_FILTER_CBF_NELMTS, cd_values),CBF_FORMAT,errorcode);
        if (errorcode) return CBF_H5FAIL;
        return 0;
    }


    static size_t cbf_h5z_filter(unsigned int flags,
                                 size_t cd_nelmts,
                                 const unsigned int cd_values[],
                                 size_t nbytes,
                                 size_t *buf_size,
                                 void **buf){
        
        cbf_file *tempfile;
        int errorcode;
        size_t elsize;
        int elsign;
        size_t nelem;
        unsigned int compression;
        size_t size;
        int bits;
        char digest[25];
        int realarray;
        size_t ip;
        size_t dimfast;
        size_t dimmid;
        size_t dimslow;
        size_t padding;
        char text[100];
        long int digest_pos;
        void *vcharacters;
        
        if (flags & H5Z_FLAG_REVERSE) {
            /* decompression */
            errorcode = 0;
            tempfile = NULL;
            cbf_reportnez(cbf_make_file(&tempfile,NULL),errorcode);
            if (tempfile->characters_base) vcharacters = (void *)(tempfile->characters_base);
            errorcode |= cbf_free ((void **) &vcharacters,
                                   &(tempfile->characters_size));
            tempfile->characters_base = tempfile->characters = *buf;
            tempfile->characters_used = tempfile->characters_size = nbytes;
            
            
        } else {
            /* compression */
            errorcode = 0;
            tempfile = NULL;
            cbf_reportnez(cbf_make_file(&tempfile,NULL),errorcode);
            /* load compression parameters from cd_values */
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_ELSIZE
                && cd_values[CBF_H5Z_FILTER_CBF_ELSIZE] > 0
                && cd_values[CBF_H5Z_FILTER_CBF_ELSIZE] <= 16 ) {
                elsize = cd_values[CBF_H5Z_FILTER_CBF_ELSIZE];
            } else {
                elsize = 1;
            }
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_ELSIGN ) {
                elsign = cd_values[CBF_H5Z_FILTER_CBF_ELSIGN];
            } else {
                elsign = 0;
            }
            nelem = (nbytes+ elsize-1)/elsize;
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_COMPRESSION ) {
                compression = cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION];
            } else {
                compression = CBF_NONE;
            }
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_REAL  ) {
                realarray = cd_values[CBF_H5Z_FILTER_CBF_REAL];
            } else {
                realarray = 0;
            }
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_DIMFAST
                && cd_values[CBF_H5Z_FILTER_CBF_DIMFAST] > 0
                && cd_values[CBF_H5Z_FILTER_CBF_DIMFAST] <= nelem) {
                dimfast = cd_values[CBF_H5Z_FILTER_CBF_DIMFAST];
            } else {
                dimfast = nelem;
            }
            if (dimfast < 1) dimfast = 1;
            if (cd_nelmts >CBF_H5Z_FILTER_CBF_DIMMID
                && cd_values[CBF_H5Z_FILTER_CBF_DIMMID] > 0 &&
                cd_values[CBF_H5Z_FILTER_CBF_DIMMID] <= nelem/dimfast) {
                dimmid = cd_values[CBF_H5Z_FILTER_CBF_DIMMID];
            } else {
                dimmid = nelem/dimfast;
            }
            if (dimmid < 1) dimmid = 1;
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_DIMSLOW
                && cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW] > 0
                && cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW]
                <= nelem/(dimfast*dimmid)) {
                dimslow = cd_values[CBF_H5Z_FILTER_CBF_DIMSLOW];
            } else {
                dimslow = nelem/(dimfast*dimmid);
            }
            if (cd_nelmts > CBF_H5Z_FILTER_CBF_PADDING
                && cd_values[CBF_H5Z_FILTER_CBF_PADDING] > 0 ) {
                padding = cd_values[CBF_H5Z_FILTER_CBF_PADDING];
            } else {
                padding = 0;
            }
            
            if (dimslow < 1) dimslow = 1;
            
            if (cd_nelmts > cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG]
                && cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG] != 0) {
                
                cbf_reportnez(cbf_write_string (tempfile,
                                                "--CIF-BINARY-FORMAT-SECTION--\n"),
                              errorcode);
                
                if (cd_values[CBF_H5Z_FILTER_CBF_COMPRESSION] == CBF_NONE) {
                    cbf_reportnez(cbf_write_string (tempfile,
                                                    "Content-Type: application/octet-stream\n"),
                                  errorcode);
                    
                } else {
                    cbf_reportnez(cbf_write_string (tempfile,
                                                    "Content-Type: application/octet-stream;\n"),
                                  errorcode);
                    
                    switch (compression&CBF_COMPRESSION_MASK)
                    {
                        case CBF_PACKED:
                            
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_PACKED\""),
                                           errorcode);
                            
                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                cbf_reportnez (cbf_write_string (tempfile,
                                                                 "; \"uncorrelated_sections\""),
                                               errorcode);
                            }
                            
                            if (compression&CBF_FLAT_IMAGE) {
                                cbf_reportnez (cbf_write_string (tempfile,
                                                                 "; \"flat\""),
                                               errorcode);
                            }
                            
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "\n"),
                                           errorcode);
                            
                            break;
                            
                        case CBF_PACKED_V2:
                            
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_PACKED_V2\""),
                                           errorcode);
                            
                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                cbf_reportnez (cbf_write_string (tempfile,
                                                                 "; \"uncorrelated_sections\""),
                                               errorcode);
                                
                            }
                            
                            if (compression&CBF_FLAT_IMAGE) {
                                cbf_reportnez (cbf_write_string (tempfile,
                                                                 "; \"flat\""), errorcode)
                            }
                            cbf_reportnez (cbf_write_string (tempfile, "\n"), errorcode);
                            break;
                            
                        case CBF_CANONICAL:
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_CANONICAL\"\n"), errorcode);
                            break;
                            
                        case CBF_BYTE_OFFSET:
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_BYTE_OFFSET\"\n"), errorcode);
                            break;
                            
                        case CBF_NIBBLE_OFFSET:
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_NIBBLE_OFFSET\"\n"), errorcode);
                            break;
                            
                        case CBF_PREDICTOR:
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_PREDICTOR\"\n"), errorcode);
                            break;
                            
                        default:
                            cbf_reportnez (cbf_write_string (tempfile,
                                                             "     conversions=\"x-CBF_UNKNOWN\"\n"), errorcode);
                    }
                }
                
                cbf_reportnez (cbf_write_string (tempfile,
                                                 "Content-Transfer-Encoding: BINARY\n"), errorcode);
                if (realarray) {
                    sprintf (text, "X-Binary-Element-Type: \"signed %ld-bit real IEEE\"\n",
                             elsize*CHAR_BIT);
                } else {
                    if (elsign)
                        sprintf (text, "X-Binary-Element-Type: \"signed %ld-bit integer\"\n",
                                 elsize*CHAR_BIT);
                    else
                        sprintf (text, "X-Binary-Element-Type: \"unsigned %ld-bit integer\"\n",
                                 elsize*CHAR_BIT);
                }
                
                cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                cbf_reportnez (cbf_write_string (tempfile,
                                                 "Content-MD5: "),errorcode);
                cbf_reportnez (cbf_get_fileposition (tempfile, &digest_pos), errorcode);
                cbf_reportnez (cbf_write_string (tempfile,
                                                 "========================\n"), errorcode);
                if (nelem > 0) {
                    sprintf (text, "X-Binary-Number-of-Elements: %ld\n", (unsigned long)nelem);
                    cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                }
                
                
                if (dimfast > 0) {
                    sprintf (text, "X-Binary-Size-Fastest-Dimension: %ld\n", (unsigned long)dimfast);
                    cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                }
                
                if (dimmid > 0) {
                    sprintf (text, "X-Binary-Size-Second-Dimension: %ld\n", (unsigned long)dimmid);
                    cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                }
                
                if ((long)dimslow > 0) {
                    sprintf (text, "X-Binary-Size-Third-Dimension: %ld\n", (unsigned long)dimslow);
                    cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                    
                } else if ((long)dimslow < 0 ) {
                    sprintf (text, "X-Binary-Size-Third-Dimension: %ld\n", (unsigned long)(-(long)dimslow) );
                    cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                }
                
                
                if (padding > 0) {
                    sprintf (text, "X-Binary-Size-Padding: %ld\n", (unsigned long)padding);
                    cbf_reportnez (cbf_write_string (tempfile, text), errorcode);
                }
                cbf_reportnez (cbf_write_string (tempfile, "\n"), errorcode);
                
                /* Write the separators */
                
                cbf_reportnez (cbf_put_character (tempfile, 12), errorcode);
                cbf_reportnez (cbf_put_character (tempfile, 26), errorcode);
                cbf_reportnez (cbf_put_character (tempfile, 4), errorcode);
                cbf_reportnez (cbf_put_character (tempfile, 213), errorcode);
                
                
                /* Flush any bits in the buffers */
                
                cbf_reportnez (cbf_flush_bits (tempfile), errorcode);
                
            }
            
            if (!errorcode &&
                cbf_compress (*buf, elsize, elsign, nelem,
                              compression, tempfile,
                              &size, &bits, digest, realarray,
                              "little_endian", dimfast, dimmid, dimslow, padding)) {
                    errorcode |= CBF_FORMAT;
                    cbf_delete_fileconnection (&tempfile);
                }
            if (!errorcode) {
                void * oldbuf;
                oldbuf = *buf;
                if (padding > 0)  {
                    for (ip = 0; ip < 100; ip++) text[ip] = 0;
                    for (ip = 0; ip < padding; ip+=100) {
                        cbf_reportnez ((cbf_put_bits(tempfile, (int *)text,CHAR_BIT*(ip+100<padding?100:padding-ip))),errorcode)
                    }
                }
                for (ip = 0; ip < 24; ip ++) {
                    (tempfile->characters_base)[digest_pos+ip]= digest[ip];
                }
                
                if (cd_nelmts > cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG]
                    && cd_values[CBF_H5Z_FILTER_CBF_MIME_FLAG] != 0) {
                    cbf_reportnez (cbf_write_string (tempfile,
                                                     "\n--CIF-BINARY-FORMAT-SECTION----\n;\n"),errorcode);
                }
                cbf_reportnez (cbf_flush_characters (tempfile), errorcode)
                *buf = tempfile->characters_base;
                *buf_size = tempfile->characters-tempfile->characters_base;
                tempfile->characters_base = oldbuf;
                cbf_free_file(&tempfile);
                return *buf_size;
            }
            
            
        }
        
        return 0;
        
    }
 
    
    
#ifdef __cplusplus
    
}

#endif
