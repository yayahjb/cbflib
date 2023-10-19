/**********************************************************************
 * cbf_copy.c -- cbflib copy functions                                *
 *                                                                    *
 * Version 0.9.8 30 May 2023                                          *
 *                                                                    *
 * (C) Copyright 2010 Herbert J. Bernstein                            *
 *                                                                    *
 *                      Part of the CBFlib API                        *
 *                              by                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
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
    
#include "cbf_copy.h"
#include "cbf_alloc.h"
#include "cbf_string.h"
    
#include <float.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <stdio.h>
    
    
    /* cbf_copy_cbf -- copy cbfin to cbfout */
    
    int cbf_copy_cbf(cbf_handle cbfout, cbf_handle cbfin,
                     const int compression,
                     const int dimflag) {
        
        unsigned int blocknum, blocks;
        
        const char * datablock_name;
        
        cbf_failnez (cbf_rewind_datablock(cbfin))
        
        cbf_failnez (cbf_count_datablocks(cbfin, &blocks))
        
        for (blocknum = 0; blocknum < blocks;  blocknum++ ) {
            
            cbf_failnez (cbf_select_datablock(cbfin, blocknum))
            cbf_failnez (cbf_datablock_name(cbfin, &datablock_name))
            cbf_failnez (cbf_copy_datablock(cbfout, cbfin, datablock_name, compression, dimflag))
        }
        
        return 0;
        
    }
    
    /* cbf_copy_category -- copy the current category from cbfin
     specified category in cbfout */
    
    int cbf_copy_category(cbf_handle cbfout, cbf_handle cbfin,
                          const char * category_name,
                          const int compression,
                          const int dimflag) {
        
        unsigned int rows, columns;
        
        unsigned int rownum, colnum;
        
        const char * column_name;
        
        const char * value;
        
        cbf_failnez(cbf_force_new_category(cbfout,category_name))
        
        cbf_failnez(cbf_count_rows(cbfin,&rows));
        
        cbf_failnez(cbf_count_columns(cbfin,&columns));
        
        /*  Transfer the column names from cbfin to cbfout */
        
        if ( ! cbf_rewind_column(cbfin) ) {
            
            do {
                
                cbf_failnez(cbf_column_name(cbfin, &column_name))
                
                cbf_failnez(cbf_new_column(cbfout, column_name))
                
            } while ( ! cbf_next_column(cbfin) );
            
            cbf_failnez(cbf_rewind_column(cbfin))
            
            cbf_failnez(cbf_rewind_row(cbfin))
        }
        
        /* Transfer to rows from cbfin to cbfout */
        
        for (rownum = 0; rownum < rows; rownum++ ) {
            
            cbf_failnez (cbf_select_row(cbfin, rownum))
            
            cbf_failnez (cbf_new_row(cbfout))
            
            cbf_rewind_column(cbfin);
            
            for (colnum = 0; colnum < columns; colnum++ ) {
                
                const char *typeofvalue;
                
                cbf_failnez (cbf_select_column(cbfin, colnum))
                
                cbf_failnez (cbf_column_name(cbfin, &column_name))
                
                if ( ! cbf_get_value(cbfin, &value) ) {
                    
                    if (compression && value && column_name && !cbf_cistrcmp("compression_type",column_name)) {
                        
                        cbf_failnez (cbf_select_column(cbfout, colnum))
                        
                        switch (compression&CBF_COMPRESSION_MASK) {
                                
                            case (CBF_NONE):
                                cbf_failnez (cbf_set_value      (cbfout,"none"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                            case (CBF_CANONICAL):
                                cbf_failnez (cbf_set_value      (cbfout,"canonical"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                            case (CBF_PACKED):
                                cbf_failnez (cbf_set_value      (cbfout,"packed"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                            case (CBF_PACKED_V2):
                                cbf_failnez (cbf_set_value      (cbfout,"packed_v2"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                            case (CBF_BYTE_OFFSET):
                                cbf_failnez (cbf_set_value      (cbfout,"byte_offsets"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                            case (CBF_NIBBLE_OFFSET):
                                cbf_failnez (cbf_set_value      (cbfout,"nibble_offset"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                            case (CBF_PREDICTOR):
                                cbf_failnez (cbf_set_value      (cbfout,"predictor"))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                                break;
                                
                                
                            default:
                                cbf_failnez (cbf_set_value      (cbfout,"."))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"null"))
                                break;
                        }
                        if (compression&CBF_FLAG_MASK) {
                            
                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                
                                cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                                cbf_failnez (cbf_set_value        (cbfout, "uncorrelated_sections"))
                                cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                                
                            } else if (compression&CBF_FLAT_IMAGE)  {
                                
                                cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                                cbf_failnez (cbf_set_value        (cbfout, "flat"))
                                cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                                
                            }
                        } else {
                            
                            if (!cbf_find_column(cbfout, "compression_type_flag")) {
                                cbf_failnez (cbf_set_value      (cbfout,"."))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"null"))
                            }
                            
                        }
                        
                    } else  if (compression && value && column_name && !cbf_cistrcmp("compression_type_flag",column_name)) {
                        
                        if (compression&CBF_FLAG_MASK) {
                            
                            if (compression&CBF_UNCORRELATED_SECTIONS) {
                                cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                                cbf_failnez (cbf_set_value        (cbfout, "uncorrelated_sections"))
                                cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                                
                            } else if (compression&CBF_FLAT_IMAGE)  {
                                cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                                cbf_failnez (cbf_set_value        (cbfout, "flat"))
                                cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                            }
                            
                        } else {
                            
                            if (!cbf_find_column(cbfout, "compression_type_flag")) {
                                cbf_failnez (cbf_set_value      (cbfout,"."))
                                cbf_failnez (cbf_set_typeofvalue(cbfout,"null"))
                            }
                            
                        }
                    } else {
                        
                        cbf_failnez (cbf_get_typeofvalue(cbfin, &typeofvalue))
                        cbf_failnez (cbf_select_column(cbfout, colnum))
                        cbf_failnez (cbf_set_value(cbfout, value))
                        cbf_failnez (cbf_set_typeofvalue(cbfout, typeofvalue))
                    }
                    
                } else {
                    
                    void * array;
                    
                    int binary_id, elsigned, elunsigned;
                    
                    size_t elements,elements_read, elsize;
                    
                    int minelement, maxelement;
                    
                    unsigned int cifcompression;
                    
                    int realarray;
                    
                    const char *byteorder;
                    
                    size_t dimfast, dimmid, dimslow, padding;
                    
                    cbf_failnez(cbf_get_arrayparameters_wdims_fs(
                                                                 cbfin, &cifcompression,
                                                                 &binary_id, &elsize, &elsigned, &elunsigned,
                                                                 &elements, &minelement, &maxelement, &realarray,
                                                                 &byteorder, &dimfast, &dimmid, &dimslow, &padding))
                    
                    if ((array=malloc(elsize*elements))) {
                        
                        cbf_failnez (cbf_select_column(cbfout,colnum))
                        
                        if (!realarray)  {
                            
                            cbf_failnez (cbf_get_integerarray(
                                                              cbfin, &binary_id, array, elsize, elsigned,
                                                              elements, &elements_read))
                            
                            if (dimflag == CBF_HDR_FINDDIMS && dimfast==0) {
                                cbf_get_arraydimensions(cbfin,NULL,&dimfast,&dimmid,&dimslow);
                            }
                            
                            cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                      cbfout, compression,
                                                                      binary_id, array, elsize, elsigned, elements,
                                                                      "little_endian", dimfast, dimmid, dimslow, 0))
                        } else {
                            
                            cbf_failnez (cbf_get_realarray(
                                                           cbfin, &binary_id, array, elsize,
                                                           elements, &elements_read))
                            
                            if (dimflag == CBF_HDR_FINDDIMS && dimfast==0) {
                                cbf_get_arraydimensions(cbfin,NULL,&dimfast,&dimmid,&dimslow);
                            }
                            
                            cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                   cbfout, compression,
                                                                   binary_id, array, elsize, elements,
                                                                   "little_endian", dimfast, dimmid, dimslow, 0))
                        }
                        
                        free(array);
                        
                    } else {
                        
                        return CBF_ALLOC;
                    }
                }
            }
        }
        
        return 0;
        
    }
    
    /* cbf_copy_datablock -- copy the current datablock from cbfin
     to the next datablock in cbfout
     */
    
    int cbf_copy_datablock (cbf_handle cbfout, cbf_handle cbfin,
                            const char * datablock_name,
                            const int compression,
                            const int dimflag) {
        
        CBF_NODETYPE itemtype;
        
        const char *category_name;
        
        const char *saveframe_name;
        
        unsigned int itemnum, blockitems,catnum,categories;
        
        cbf_failnez (cbf_force_new_datablock(cbfout, datablock_name))
        
        if ( !cbf_rewind_blockitem(cbfin, &itemtype) ) {
            cbf_failnez (cbf_count_blockitems(cbfin, &blockitems))
            
            for (itemnum = 0; itemnum < blockitems;  itemnum++) {
                
                cbf_failnez(cbf_select_blockitem(cbfin, itemnum, &itemtype))
                
                if (itemtype == CBF_CATEGORY) {
                    
                    cbf_failnez(cbf_category_name(cbfin,&category_name))
                    cbf_failnez(cbf_copy_category(cbfout,cbfin,category_name, compression, dimflag))
                    
                } else {
                    
                    cbf_failnez(cbf_saveframe_name(cbfin,&saveframe_name))
                    cbf_force_new_saveframe(cbfout, saveframe_name);
                    
                    if ( !cbf_rewind_category(cbfin) ) {
                        
                        cbf_failnez (cbf_count_categories(cbfin, &categories))
                        
                        for (catnum = 0; catnum < categories;  catnum++) {
                            
                            cbf_select_category(cbfin, catnum);
                            cbf_category_name(cbfin,&category_name);
                            cbf_failnez(cbf_copy_category(cbfout,cbfin,category_name, compression, dimflag))
                            
                        }
                        
                    }
                    
                }
                
            }
            
        }
        
        return 0;
        
    }
    
    
    
    /* cbf_copy_value -- copy the current value from cbfin to cbfout,
     specifying the target category, column, rownum, compression, dimension details,
     element type, size and sign */
    
    int cbf_copy_value(cbf_handle cbfout,
                       cbf_handle cbfin,
                       const char * category_name,
                       const char * column_name,
                       const unsigned int rownum,
                       const int compression,
                       const int dimflag,
                       const int eltype,
                       const size_t elsize,
                       const size_t elsign,
                       const double cliplow,
                       const double cliphigh) {
        
        return cbf_copy_value_with_roi_binoi(cbfout,
                                             cbfin,
                                             category_name,
                                             column_name,
                                             rownum,
                                             compression,
                                             dimflag,
                                             eltype,
                                             elsize,
                                             elsign,
                                             cliplow,
                                             cliphigh,
                                             NULL, NULL);
        
    }
    
    /* cbf_copy_value_with_roi_binoi -- copy the current value from cbfin to cbfout,
     specifying the target category, column, rownum, compression, dimension details,
     element type, size and sign, with an optional roi */
    
    int cbf_copy_value_with_roi(cbf_handle cbfout,
                                cbf_handle cbfin,
                                const char * category_name,
                                const char * column_name,
                                const unsigned int rownum,
                                const int compression,
                                const int dimflag,
                                const int eltype,
                                const size_t elsize,
                                const int elsign,
                                const double cliplow,
                                const double cliphigh,
                                const char * roi) {
        
        return cbf_copy_value_with_roi_binoi(cbfout,
                                             cbfin,
                                             category_name,
                                             column_name,
                                             rownum,
                                             compression,
                                             dimflag,
                                             eltype,
                                             elsize,
                                             elsign,
                                             cliplow,
                                             cliphigh,
                                             roi,
                                             NULL);
        
    }
    
    /* cbf_copy_value_with_roi_binoi -- copy the current value from cbfin to cbfout,
     specifying the target category, column, rownum, compression, dimension details,
     element type, size and sign, with an optional roi and optional binoi */
    
    int cbf_copy_value_with_roi_binoi(cbf_handle cbfout,
                                      cbf_handle cbfin,
                                      const char * category_name,
                                      const char * column_name,
                                      const unsigned int rownum,
                                      const int compression,
                                      const int dimflag,
                                      const int eltype,
                                      const size_t elsize,
                                      const int elsign,
                                      const double cliplow,
                                      const double cliphigh,
                                      const char * roi,
                                      const char * binoi) {
        
        unsigned int rows;
        
        const char * value;
        
        char * border;
        
#ifndef CBF_USE_LONG_LONG
        
        size_t lobyte, hibyte;
        
        double vallow, valhigh;
        
#endif
        
        
        cbf_get_local_integer_byte_order(&border);
        
        
        if ( ! (eltype==0
                || eltype==CBF_CPY_SETINTEGER
                || eltype==CBF_CPY_SETREAL)) return CBF_ARGUMENT;
        
        if ( ! (elsign==0
                || elsign==CBF_CPY_SETUNSIGNED
                || elsign==CBF_CPY_SETSIGNED)) return CBF_ARGUMENT;
        
        if (elsize != 0 &&
            elsize != sizeof (long int) &&
#ifdef CBF_USE_LONG_LONG
            elsize != sizeof(long long int) &&
#else
            elsize != 2* sizeof (long int) &&
#endif
            elsize != sizeof (int) &&
            elsize != sizeof (short int) &&
            elsize != sizeof (char))
            return CBF_ARGUMENT;
        
        cbf_failnez(cbf_require_category(cbfout,category_name));
        
        cbf_failnez(cbf_count_rows(cbfout,&rows));
        
        while (rows < rownum+1) {
            
            cbf_failnez(cbf_new_row(cbfout))
            
            rows++;
            
        }
        
        cbf_failnez(cbf_require_column(cbfout,column_name))
        
        cbf_failnez(cbf_select_row(cbfout,rownum))
        
        if ( ! cbf_get_value(cbfin, &value) ) {
            
            if (compression && value && !cbf_cistrcmp("compression_type",column_name)) {
                
                switch (compression&CBF_COMPRESSION_MASK) {
                        
                    case (CBF_NONE):
                        cbf_failnez (cbf_set_value      (cbfout,"none"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                    case (CBF_CANONICAL):
                        cbf_failnez (cbf_set_value      (cbfout,"canonical"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                    case (CBF_PACKED):
                        cbf_failnez (cbf_set_value      (cbfout,"packed"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                    case (CBF_PACKED_V2):
                        cbf_failnez (cbf_set_value      (cbfout,"packed_v2"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                    case (CBF_BYTE_OFFSET):
                        cbf_failnez (cbf_set_value      (cbfout,"byte_offsets"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                    case (CBF_NIBBLE_OFFSET):
                        cbf_failnez (cbf_set_value      (cbfout,"nibble_offset"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                    case (CBF_PREDICTOR):
                        cbf_failnez (cbf_set_value      (cbfout,"predictor"))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"word"))
                        break;
                        
                        
                    default:
                        cbf_failnez (cbf_set_value      (cbfout,"."))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"null"))
                        break;
                }
                if (compression&CBF_FLAG_MASK) {
                    
                    if (compression&CBF_UNCORRELATED_SECTIONS) {
                        
                        cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                        cbf_failnez (cbf_set_value        (cbfout, "uncorrelated_sections"))
                        cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                        
                    } else if (compression&CBF_FLAT_IMAGE)  {
                        
                        cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                        cbf_failnez (cbf_set_value        (cbfout, "flat"))
                        cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                        
                    }
                } else {
                    
                    if (!cbf_find_column(cbfout, "compression_type_flag")) {
                        cbf_failnez (cbf_set_value      (cbfout,"."))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"null"))
                    }
                    
                }
                
            } else  if (compression && value && !cbf_cistrcmp("compression_type_flag",column_name)) {
                
                if (compression&CBF_FLAG_MASK) {
                    
                    if (compression&CBF_UNCORRELATED_SECTIONS) {
                        cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                        cbf_failnez (cbf_set_value        (cbfout, "uncorrelated_sections"))
                        cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                        
                    } else if (compression&CBF_FLAT_IMAGE)  {
                        cbf_failnez (cbf_require_column   (cbfout, "compression_type_flag"))
                        cbf_failnez (cbf_set_value        (cbfout, "flat"))
                        cbf_failnez (cbf_set_typeofvalue  (cbfout, "word"))
                    }
                    
                } else {
                    
                    if (!cbf_find_column(cbfout, "compression_type_flag")) {
                        cbf_failnez (cbf_set_value      (cbfout,"."))
                        cbf_failnez (cbf_set_typeofvalue(cbfout,"null"))
                    }
                    
                }
            } else {
                
                const char *typeofvalue;
                
                cbf_failnez (cbf_get_typeofvalue(cbfin, &typeofvalue))
                cbf_failnez (cbf_set_value(cbfout, value))
                cbf_failnez (cbf_set_typeofvalue(cbfout, typeofvalue))
            }
            
        } else {
            
            void * array;
            
            int binary_id, elsigned, elunsigned;
            
            size_t elements, elements_read;
            
            size_t oelsize;
            
            int minelement, maxelement;
            
            unsigned int cifcompression;
            
            int realarray;
            
            const char *byteorder;
            
            size_t dimfast, dimmid, dimslow, padding;
            
            size_t bndfast, bndmid, bndslow;
            
            size_t fastlow=0, fasthigh=0, midlow=0, midhigh=0, slowlow=0, slowhigh=0;
            
            size_t bndfastlow=0, bndfasthigh=0, bndmidlow=0, bndmidhigh=0, bndslowlow=0, bndslowhigh=0;
            
            double valuelow=-DBL_MAX, valuehigh=DBL_MAX;
            
            size_t binratio=1, modulefast=0, modulemid=0, moduleslow=0, gapfast=0, gapmid=0, gapslow=0;
            
            size_t bndmodfast=0, bndmodmid=0, bndmodslow=0;
            
            
            cbf_failnez(cbf_get_arrayparameters_wdims_fs(cbfin,
                                                         &cifcompression,
                                                         &binary_id,
                                                         &oelsize,
                                                         &elsigned,
                                                         &elunsigned,
                                                         &elements,
                                                         &minelement,
                                                         &maxelement,
                                                         &realarray,
                                                         &byteorder,
                                                         &dimfast,
                                                         &dimmid,
                                                         &dimslow, &padding));
            if (dimfast == 0) dimfast = 1;
            fasthigh = dimfast-1;
            if (dimmid == 0) dimmid = 1;
            midhigh = dimmid-1;
            if (dimslow == 0) dimslow = 1;
            slowhigh = dimslow-1;
            modulefast = dimfast;
            gapfast = 0;
            modulemid = dimmid;
            gapmid = 0;
            moduleslow = dimslow;
            gapslow = 0;
            
            if (roi) {
                
                cbf_failnez(cbf_convertroi((char *)roi,dimfast,dimmid,dimslow,
                                           &fastlow,&fasthigh,
                                           &midlow,&midhigh,
                                           &slowlow,&slowhigh,
                                           &valuelow,&valuehigh));
                
            }
            
            if (binoi) {
                cbf_failnez(cbf_convertbinoi((char *)binoi,&binratio,
                                             &modulefast,&modulemid,&moduleslow,
                                             &gapfast,&gapmid,&gapslow));
                if (binratio == 0) binratio = 1;
                bndfastlow = (fastlow+binratio-1)/binratio;
                bndfasthigh = (fasthigh+binratio-1)/binratio;
                bndmidlow = (midlow+binratio-1)/binratio;
                bndmidhigh = (midhigh+binratio-1)/binratio;
                bndslowlow = (slowlow+binratio-1)/binratio;
                bndslowhigh = (slowhigh+binratio-1)/binratio;
                
            }
            
            
            
            if (oelsize != sizeof (long int) &&
#ifdef CBF_USE_LONG_LONG
                oelsize != sizeof(long long int) &&
#else
                oelsize != 2* sizeof (long int) &&
#endif
                oelsize != sizeof (int) &&
                oelsize != sizeof (short int) &&
                oelsize != sizeof (char))
                return CBF_ARGUMENT;
            
            
            if ((array=malloc(oelsize*elements))) {
                
                size_t nelsize;
                
                int nelsigned, nelunsigned;
                
                int icount, jcount, fill;
                
                size_t xelsize;
                
                nelsize = oelsize;
                
                if (elsize != 0) nelsize = elsize;
                
                xelsize = nelsize;
                
                if (oelsize < nelsize) xelsize = oelsize;
                
                nelsigned = elsigned;
                
                nelunsigned = elunsigned;
                
                if (elsign & CBF_CPY_SETSIGNED) {
                    nelsigned = 1;
                    nelunsigned = 0;
                }
                
                if (elsign & CBF_CPY_SETUNSIGNED) {
                    nelunsigned = 1;
                    nelsigned = 0;
                }
                
                if (!realarray)  {
                    
                    /* The current array is integer */
                    
                    cbf_onfailnez (cbf_get_integerarray(cbfin,
                                                        &binary_id,
                                                        array,
                                                        oelsize,
                                                        elsigned,
                                                        elements,
                                                        &elements_read),
                                   {free(array); array=NULL;})
                    
                    if (dimfast < 1) dimfast = 1;
                    if (dimmid < 1) dimmid = 1;
                    if (dimslow < 1) dimslow = 1;
                    
                    if (roi) {
                        
                        void * roi_array;
                        
                        if (!binoi) {
                            roi_array=malloc(oelsize*(fasthigh-fastlow+1)*(midhigh-midlow+1)*(slowhigh-slowlow+1));
                            cbf_failnez(cbf_extract_roi(array,
                                                        roi_array,
                                                        elsize,
                                                        fastlow,
                                                        fasthigh,
                                                        midlow,
                                                        midhigh,
                                                        slowlow,
                                                        slowhigh,
                                                        dimfast,
                                                        dimmid,
                                                        dimslow));
                        } else {
                            roi_array=malloc(oelsize*(bndfasthigh-bndfastlow+1)*(bndmidhigh-bndmidlow+1)*(bndslowhigh-bndslowlow+1));
                            cbf_failnez(cbf_extract_roi_binoi(array,
                                                              roi_array,
                                                              elsize,
                                                              elsigned,
                                                              realarray,
                                                              fastlow,
                                                              fasthigh,
                                                              midlow,
                                                              midhigh,
                                                              slowlow,
                                                              slowhigh,
                                                              dimfast,
                                                              dimmid,
                                                              dimslow,
                                                              binratio,
                                                              bndfastlow,
                                                              bndfasthigh,
                                                              bndmidlow,
                                                              bndmidhigh,
                                                              bndslowlow,
                                                              bndslowhigh,
                                                              modulefast,
                                                              modulemid,
                                                              moduleslow,
                                                              gapfast,
                                                              gapmid,
                                                              gapslow
                                                              ));
                            
                        }
                        if (!roi_array) {
                            
                            cbf_onfailnez(CBF_ALLOC,{free(array);});
                            
                        }
                        
                        
                        free(array);
                        
                        array = roi_array;
                        
                        dimfast = fasthigh- fastlow + 1;
                        
                        dimmid  = midhigh - midlow + 1;
                        
                        dimslow = slowhigh- slowlow + 1;
                        
                        elements = elements_read = dimfast*dimmid*dimslow;
                        
                        
                    }
                    
                    if (((eltype &(CBF_CPY_SETINTEGER)) || eltype == 0)
                        && (elsize == 0 || elsize==(ssize_t)oelsize)
                        && (elsign == 0 ||
                            ((elsign & CBF_CPY_SETSIGNED) && elsigned) ||
                            ((elsign & CBF_CPY_SETUNSIGNED) && elunsigned))
                        && cliplow >= cliphigh
                        && valuelow == -DBL_MAX && valuehigh == DBL_MAX ) {
                        
                        cbf_onfailnez(cbf_set_integerarray_wdims_fs(
                                                                    cbfout, compression,
                                                                    binary_id, array, oelsize, elsigned, elements,
                                                                    "little_endian", dimfast, dimmid, dimslow, 0),{free(array);} )
                        free(array);
                        
                    } else {
                        
                        void * narray;
                        
                        int loword, hiword;
                        
                        unsigned long maxlonguint;
                        
                        double onemore;
                        
                        CBF_UNUSED( onemore );
                        
                        CBF_UNUSED( loword );
                        
                        CBF_UNUSED( hiword );
                        
                        maxlonguint = ~0;
                        
                        onemore = ((double)maxlonguint)+1.;
                        
                        if (toupper(border[0])=='L') {
                            
                            loword = 0;
                            
                            hiword = 1;
                            
                        } else {
                            
                            loword = 1;
                            
                            hiword = 0;
                            
                        }
                        
                        if ((narray=malloc(nelsize*elements))) {
                            
                            double minval, maxval, valtemp;
                            size_t icount;
                            int innarray;
                            
                            if (nelunsigned) {
                                minval = 0.;
                                switch( nelsize )  {
                                    case 1:  maxval = (double)(0xFF); break;
                                    case 2:  maxval = (double)(0xFFFFU); break;
                                    case 4:  maxval = (double)(0xFFFFFFFFUL); break;
                                    case 8:  maxval = ((double)(0xFFFFFFFFUL))*(2.+((double)(0xFFFFFFFFUL))); break;
                                    default: free(array); free(narray); return CBF_ARGUMENT;
                                }
                            } else if (nelsigned) {
                                switch( nelsize ) {
                                    case 1:  maxval = (double)(0x7F); break;
                                    case 2:  maxval = (double)(0x7FFFU); break;
                                    case 4:  maxval = (double)(0x7FFFFFFFUL); break;
                                    case 8:  maxval = ((double)(0xFFFFFFFFUL)) +
                                        ((double)(0x7FFFFFFFL))*(1.+((double)(0xFFFFFFFFUL))); break;
                                    default: free(array); free(narray); return CBF_ARGUMENT;
                                }
                                minval = -maxval;
                                
                                if ((int)(~0)+1 == 0) minval = minval -1;
                                
                            } else {
                                free(array); free(narray); return CBF_ARGUMENT;
                            }
                            
                            innarray=0;
                            
                            if (cliplow < cliphigh || valuelow > -DBL_MAX || valuehigh < DBL_MAX) {
                                
                                double doval;
                                
                                for (icount = 0; icount < (int)elements; icount++) {
                                    
                                    if ((ssize_t)oelsize == sizeof(char)){
                                        if (elsigned) doval = (double)((signed char *)array)[icount];
                                        else doval = (double)((unsigned char *)array)[icount];
                                        if (cliplow < cliphigh) {
                                            if (doval < cliplow) doval = cliplow;
                                            if (doval > cliphigh) doval = cliphigh;
                                        }
                                        if (doval+0.5 < valuelow || doval-0.5 > valuehigh) {
                                            doval = 0.;
                                        } else {
                                            doval -= (valuelow-1);
                                        }
                                        if (elsigned) ((signed char *)array)[icount] = (signed char)doval;
                                        else ((unsigned char *)array)[icount] = (unsigned char)doval;
                                        
                                    } else if ((ssize_t)oelsize == sizeof(short int)){
                                        if (elsigned) doval = (double)((signed short int *)array)[icount];
                                        else doval = (double)((unsigned short int *)array)[icount];
                                        if (cliplow < cliphigh) {
                                            if (doval < cliplow) doval = cliplow;
                                            if (doval > cliphigh) doval = cliphigh;
                                        }
                                        if (doval+0.5 < valuelow || doval-0.5 > valuehigh) {
                                            doval = 0.;
                                        } else {
                                            doval -= (valuelow-1);
                                        }
                                        if (elsigned) ((signed short int *)array)[icount] = (signed short int)doval;
                                        else ((unsigned short int *)array)[icount] = (unsigned short int)doval;
                                        
                                    } else if ((ssize_t)oelsize == sizeof(int)){
                                        if (elsigned) doval = (double)((signed int *)array)[icount];
                                        else doval = (double)((unsigned int *)array)[icount];
                                        if (cliplow < cliphigh) {
                                            if (doval < cliplow) doval = cliplow;
                                            if (doval > cliphigh) doval = cliphigh;
                                        }
                                        if (doval+0.5 < valuelow || doval-0.5 > valuehigh) {
                                            doval = 0.;
                                        } else {
                                            doval -= (valuelow-1);
                                        }
                                        if (elsigned) ((signed int *)array)[icount] = (signed int)doval;
                                        else ((unsigned int *)array)[icount] = (unsigned int)doval;
                                        
                                    } else if ((ssize_t)oelsize == sizeof(long int)){
                                        if (elsigned) doval = (double)((signed long int *)array)[icount];
                                        else doval = (double)((unsigned long int *)array)[icount];
                                        if (cliplow < cliphigh) {
                                            if (doval < cliplow) doval = cliplow;
                                            if (doval > cliphigh) doval = cliphigh;
                                        }
                                        if (doval+0.5 < valuelow || doval-0.5 > valuehigh) {
                                            doval = 0.;
                                        } else {
                                            doval -= (valuelow-1);
                                        }
                                        if (elsigned) ((signed long int *)array)[icount] = (signed long int)doval;
                                        else ((unsigned long int *)array)[icount] = (unsigned long int)doval;
                                        
#ifdef CBF_USE_LONG_LONG
                                    } else if ((ssize_t)oelsize == sizeof(long long int)){
                                        if (elsigned) doval = (double)((signed long long int *)array)[icount];
                                        else doval = (double)((unsigned long long int *)array)[icount];
                                        if (cliplow < cliphigh) {
                                            if (doval < cliplow) doval = cliplow;
                                            if (doval > cliphigh) doval = cliphigh;
                                        }
                                        if (doval+0.5 < valuelow || doval-0.5 > valuehigh) {
                                            doval = 0.;
                                        } else {
                                            doval -= (valuelow-1);
                                        }
                                        if (elsigned) ((signed long long int *)array)[icount] = (signed long long int)doval;
                                        else ((unsigned long long int *)array)[icount] = (unsigned long long int)doval;
#endif
                                    } else {
                                        free(narray); free(array); return CBF_ARGUMENT;
                                        
                                    }
                                    
                                }
                                
                            }
                            
                            if ((eltype & CBF_CPY_SETINTEGER) || eltype == 0 ) {
                                
                                
                                /* integer to integer conversion */
                                
                                
                                
                                if (nelsize < oelsize) {
                                    if (nelsize == sizeof(char)) {
                                        if ((ssize_t)oelsize == sizeof(short)) {
                                            innarray=1;
                                            if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert short to signed char\n");
                                            if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert short to unsigned char\n");
                                            if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned short to signed char\n");
                                            if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned short to unsigned char\n");
                                            for (icount = 0; icount < (size_t)elements; icount++) {
                                                if (elsigned) {valtemp=((short *)array)[icount];}
                                                else {valtemp=((unsigned short *)array)[icount];}
                                                if (valtemp > maxval) valtemp=maxval;
                                                if (valtemp < minval) valtemp=minval;
                                                if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                                if (nelsigned) {((signed char *)narray)[icount] = (signed char)valtemp;}
                                                else {((unsigned char *)narray)[icount] = (unsigned char)valtemp;}
                                            }
                                        } else if ((ssize_t)oelsize == sizeof(int)) {
                                            innarray=1;
                                            if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert int to signed char\n");
                                            if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert int to unsigned char\n");
                                            if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned int to signed char\n");
                                            if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned int to unsigned char\n");
                                            for (icount = 0; icount < (size_t)elements; icount++) {
                                                if (elsigned) {valtemp=((int *)array)[icount];}
                                                else {valtemp=((unsigned int *)array)[icount];}
                                                if (valtemp > maxval) valtemp=maxval;
                                                if (valtemp < minval) valtemp=minval;
                                                if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                                if (nelsigned) {((signed char *)narray)[icount] = (signed char)valtemp;}
                                                else {((unsigned char *)narray)[icount] = (unsigned char)valtemp;}
                                            }
                                        } else if ((ssize_t)oelsize == sizeof(long)) {
                                            innarray=1;
                                            if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long to signed char\n");
                                            if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long to unsigned char\n");
                                            if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long to signed char\n");
                                            if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long to unsigned char\n");
                                            for (icount = 0; icount < (size_t)elements; icount++) {
                                                if (elsigned) {valtemp=((long *)array)[icount];}
                                                else {valtemp=((unsigned long *)array)[icount];}
                                                if (valtemp > maxval) valtemp=maxval;
                                                if (valtemp < minval) valtemp=minval;
                                                if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                                if (nelsigned) {((signed char *)narray)[icount] = (signed char)valtemp;}
                                                else {((unsigned char *)narray)[icount] = (unsigned char)valtemp;}
                                            }
#ifdef CBF_USE_LONG_LONG
                                        } else if ((ssize_t)oelsize == sizeof(long long)) {
                                            innarray=1;
                                            if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to signed char\n");
                                            if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to unsigned char\n");
                                            if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to signed char\n");
                                            if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to unsigned char\n");
                                            for (icount = 0; icount < (size_t)elements; icount++) {
                                                if (elsigned) {valtemp=((long long *)array)[icount];}
                                                else {valtemp=((unsigned long long *)array)[icount];}
                                                if (valtemp > maxval) valtemp=maxval;
                                                if (valtemp < minval) valtemp=minval;
                                                if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                                if (nelsigned) {((signed char *)narray)[icount] = (signed char)valtemp;}
                                                else {((unsigned char *)narray)[icount] = (unsigned char)valtemp;}
                                            }
                                        }
#else
                                    }
#endif
                                } else if (nelsize == sizeof(short)) {
                                    if ((ssize_t)oelsize == sizeof(int)) {
                                        innarray=1;
                                        if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert int to short\n");
                                        if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert int to unsigned short\n");
                                        if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned int to short\n");
                                        if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned int to unsigned short\n");
                                        for (icount = 0; icount < (size_t)elements; icount++) {
                                            if (elsigned) {valtemp=((int *)array)[icount];}
                                            else {valtemp=((unsigned int *)array)[icount];}
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                            if (nelsigned) {((short *)narray)[icount] = (short)valtemp;}
                                            else {((unsigned short *)narray)[icount] = (unsigned short)valtemp;}
                                        }
                                    } else if ((ssize_t)oelsize == sizeof(long)) {
                                        innarray=1;
                                        if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long to short\n");
                                        if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long to unsigned short\n");
                                        if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long to short\n");
                                        if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long to unsigned short\n");
                                        for (icount = 0; icount < (size_t)elements; icount++) {
                                            if (elsigned) {valtemp=((long *)array)[icount];}
                                            else {valtemp=((unsigned long *)array)[icount];}
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                            if (nelsigned) {((short *)narray)[icount] = (short)valtemp;}
                                            else {((unsigned short *)narray)[icount] = (unsigned short)valtemp;}
                                        }
#ifdef CBF_USE_LONG_LONG
                                    } else if ((ssize_t)oelsize == sizeof(long long)) {
                                        innarray=1;
                                        if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to short\n");
                                        if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to unsigned short\n");
                                        if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to short\n");
                                        if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to unsigned short\n");
                                        for (icount = 0; icount < (size_t)elements; icount++) {
                                            if (elsigned) {valtemp=((long long *)array)[icount];}
                                            else {valtemp=((unsigned long long *)array)[icount];}
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                            if (nelsigned) {((short *)narray)[icount] = (short)valtemp;}
                                            else {((unsigned short *)narray)[icount] = (unsigned short)valtemp;}
                                        }
#endif
                                    }
                                } else if (nelsize == sizeof(int)) {
                                    if ((ssize_t)oelsize == sizeof(long)) {
                                        innarray=1;
                                        if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long to int\n");
                                        if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long to unsigned int\n");
                                        if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long to int\n");
                                        if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long to unsigned int\n");
                                        for (icount = 0; icount < (size_t)elements; icount++) {
                                            if (elsigned) {valtemp=((long *)array)[icount];}
                                            else {valtemp=((unsigned long *)array)[icount];}
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                            if (nelsigned) {((int *)narray)[icount] = (int)valtemp;}
                                            else {((unsigned int *)narray)[icount] = (unsigned int)valtemp;}
                                        }
#ifdef CBF_USE_LONG_LONG
                                    } else if ((ssize_t)oelsize == sizeof(long long)) {
                                        innarray=1;
                                        if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to int\n");
                                        if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to unsigned int\n");
                                        if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to int\n");
                                        if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to unsigned int\n");
                                        for (icount = 0; icount < (size_t)elements; icount++) {
                                            if (elsigned) {valtemp=((long long *)array)[icount];}
                                            else {valtemp=((unsigned long long*)array)[icount];}
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                            if (nelsigned) {((int *)narray)[icount] = (int)valtemp;}
                                            else {((unsigned int *)narray)[icount] = (unsigned int)valtemp;}
                                        }
#endif
                                    }
#ifdef CBF_USE_LONG_LONG
                                } else if (nelsize == sizeof(long)) {
                                    if ((ssize_t)oelsize == sizeof(long long)) {
                                        innarray=1;
                                        if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to long\n");
                                        if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert long long to unsigned long\n");
                                        if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to long\n");
                                        if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned long long to unsigned long\n");
                                        for (icount = 0; icount < (size_t)elements; icount++) {
                                            if (elsigned) {valtemp=((long long *)array)[icount];}
                                            else {valtemp=((unsigned long long*)array)[icount];}
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            if (valtemp < 0.) valtemp-=0.5; else valtemp+=0.5;
                                            if (nelsigned) {((long *)narray)[icount] = (long)valtemp;}
                                            else {((unsigned long *)narray)[icount] = (unsigned long)valtemp;}
                                        }
                                    }
#endif
                                }
                            }
                            
                            if (!innarray) {
                                if ( nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert I%u to I%u\n", (unsigned int)oelsize, (unsigned int)elsize);
                                if (!nelsigned &&  elsigned) fprintf(stdout,"cbf_copy: convert I%u to unsigned I%u\n", (unsigned int)oelsize, (unsigned int)elsize);
                                if ( nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned I%u to I%u\n", (unsigned int)oelsize, (unsigned int)elsize);
                                if (!nelsigned && !elsigned) fprintf(stdout,"cbf_copy: convert unsigned I%u to unsigned I%u\n", (unsigned int)oelsize, (unsigned int)elsize);
                                if (toupper(border[0])=='L') {
                                    for (icount = 0; icount < (int)elements; icount++ ) {
                                        memmove(((unsigned char *)narray)+icount*elsize,((unsigned char *)array)+icount*oelsize,xelsize);
                                        if (xelsize < nelsize) {
                                            fill = 0;
                                            if (nelsigned) fill =
                                                (((signed char *)array)[icount*oelsize+oelsize-1]<0)?(~0):0;
                                            if (nelunsigned)
                                                for(jcount=0;jcount<=(int)(nelsize-oelsize);jcount++) {
                                                    ((signed char *)narray)[icount*elsize+xelsize+jcount]=fill;
                                                }
                                        }
                                    }
                                } else {
                                    for (icount = 0; icount < (int)elements; icount++ ) {
                                        for (jcount = xelsize-1; jcount>=0; jcount--) {
                                            ((unsigned char *)narray)[icount*elsize+jcount] =  ((unsigned char *)array)[icount*oelsize+jcount];
                                            if (xelsize < nelsize) {
                                                fill = 0;
                                                if (nelsigned) fill =
                                                    (((signed char *)array)[icount*oelsize]<0)?(~0):0;
                                                if (nelunsigned)
                                                    for(jcount=0;jcount<=(int)(nelsize-oelsize);jcount++) {
                                                        ((signed char *)narray)[icount*elsize+jcount]=fill;
                                                    }
                                            }
                                        }
                                    }
                                }
                            }
                            
                            cbf_onfailnez(cbf_set_integerarray_wdims_fs(
                                                                        cbfout, compression,
                                                                        binary_id, narray, elsize, nelsigned, elements,
                                                                        "little_endian", dimfast, dimmid, dimslow, 0), {free(array); free(narray);})
                            free(narray);
                            free(array);
                        } else {
                            
                            /* integer to real conversion */
                            
                            double xvalue;
                            
                            if (oelsize==sizeof(char)){
                                
                                if (elsigned) {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((signed char *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                } else {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((unsigned char *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                }
                                
                            } else if (oelsize==sizeof(short int)){
                                
                                if (elsigned) {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((signed short int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                } else {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((unsigned short int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else  { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                }
                                
                            } else if (oelsize==sizeof(int)){
                                
                                if (elsigned) {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((signed int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                } else {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((unsigned int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else  { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                }
                                
                            } else if (oelsize==sizeof(long int)){
                                
                                if (elsigned) {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((signed long int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                } else {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((unsigned long int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                }
                                
                                
#ifdef CBF_USE_LONG_LONG
                            } else if (oelsize==sizeof(long long int)){
                                
                                if (elsigned) {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((signed long long int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                } else {
                                    
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        
                                        xvalue = ((unsigned long long int *)array)[icount];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                }
                                
                                
#else
                            } else if (oelsize== 2* sizeof(long int)) {
                                
                                if (elsigned) {
                                    
                                    unsigned long yvalue[2];
                                    
                                    for (icount = 0; icount < 2*((int)elements); icount++) {
                                        
                                        yvalue[0] = ((unsigned long int *)array)[2*icount];
                                        
                                        yvalue[1] = ((unsigned long int *)array)[2*icount+1];
                                        
                                        if ((long)yvalue[hiword]>0) {
                                            
                                            xvalue = ((double)yvalue[hiword])*onemore+(double)yvalue[loword];
                                            
                                        } else {
                                            
                                            xvalue = -((double)(-yvalue[hiword])*onemore-(double)yvalue[loword]);
                                            
                                        }
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                } else {
                                    
                                    unsigned long yvalue[2];
                                    
                                    for (icount = 0; icount < 2*((int)elements); icount++) {
                                        
                                        yvalue[0] = ((unsigned long int *)array)[2*icount];
                                        
                                        yvalue[1] = ((unsigned long int *)array)[2*icount+1];
                                        
                                        xvalue = ((double)yvalue[hiword])*onemore+(double)yvalue[loword];
                                        
                                        if (elsize == sizeof(double)) ((double *)narray)[icount] = xvalue;
                                        
                                        else if (elsize == sizeof(float)) ((float *)narray)[icount] = xvalue;
                                        
                                        else { free(narray); free(array); return CBF_ARGUMENT;}
                                    }
                                    
                                }
                                
                                
#endif
                                
                            } else {
                                
                                free(narray); free(array); return CBF_ARGUMENT;
                                
                            }
                            
                            cbf_onfailnez(cbf_set_realarray_wdims_fs(
                                                                     cbfout, compression,
                                                                     binary_id, narray, elsize, elements,
                                                                     "little_endian", dimfast, dimmid, dimslow, 0    ),
                                          
                                          { free(narray); free(array);})
                            
                            free(narray);
                            
                            free(array);
                        }
                        
                    } else {
                        
                        free(array);
                        
                        return CBF_ALLOC;
                    }
                }
                
            } else {
                
                /* the current array is real */
                
                cbf_onfailnez (cbf_get_realarray(
                                                 cbfin, &binary_id, array, oelsize,
                                                 elements, &elements_read), {free(array);})
                
                if (dimflag == CBF_HDR_FINDDIMS && dimfast==0) {
                    cbf_get_arraydimensions(cbfin,NULL,&dimfast,&dimmid,&dimslow);
                }
                
                if (dimfast < 1) dimfast = 1;
                if (dimmid < 1) dimmid = 1;
                if (dimslow < 1) dimslow = 1;
                
                if (roi) {
                    
                    void * roi_array;
                    
                    size_t fastlow, fasthigh, midlow, midhigh, slowlow, slowhigh;
                    double valuelow, valuehigh;
                    
                    cbf_failnez(cbf_convertroi((char *)roi,dimfast,dimmid,dimslow,
                                               &fastlow,&fasthigh,
                                               &midlow,&midhigh,
                                               &slowlow,&slowhigh,
                                               &valuelow,&valuehigh));
                    
                    roi_array=malloc(oelsize*(fasthigh-fastlow+1)*(midhigh-midlow+1)*(slowhigh-slowlow+1));
                    
                    if (!roi_array) {
                        
                        cbf_onfailnez(CBF_ALLOC,{free(array);});
                        
                    }
                    
                    cbf_failnez(cbf_extract_roi(array,
                                                roi_array,
                                                elsize,
                                                fastlow,
                                                fasthigh,
                                                midlow,
                                                midhigh,
                                                slowlow,
                                                slowhigh,
                                                dimfast,
                                                dimmid,
                                                dimslow));
                    
                    free(array);
                    
                    array = roi_array;
                    
                    dimfast = fasthigh- fastlow + 1;
                    
                    dimmid  = midhigh - midlow + 1;
                    
                    dimslow = slowhigh- slowlow + 1;
                    
                    elements = elements_read = dimfast*dimmid*dimslow;
                    
                    
                }
                
                if (((eltype &(CBF_CPY_SETREAL)) || eltype == 0)
                    && (elsize == 0 || elsize==(ssize_t)oelsize)
                    && cliplow >= cliphigh && valuelow == -DBL_MAX && valuehigh == DBL_MAX) {
                    
                    
                    cbf_failnez(cbf_set_realarray_wdims_fs(
                                                           cbfout, compression,
                                                           binary_id, array, oelsize, elements,
                                                           "little_endian", dimfast, dimmid, dimslow, 0))
                    
                    free(array);
                    
                } else {
                    
                    void * narray;
                    
                    double valtemp;
                    
                    if ((narray=malloc(nelsize*elements))) {
                        
                        if (cliplow < cliphigh) {
                            
                            double doval;
                            
                            for (icount = 0; icount < (int)elements; icount++) {
                                
                                if (oelsize==sizeof(float)){
                                    doval = (double)((float *)array)[icount];
                                    if (cliplow < cliphigh) {
                                        if (doval < cliplow) doval = cliplow;
                                        if (doval > cliphigh) doval = cliphigh;
                                    }
                                    if (doval < valuelow || doval > valuehigh) {
                                        doval = 0.;
                                    } else {
                                        doval -= (valuelow - 1.);
                                    }
                                    ((float *)array)[icount] = (float)doval;
                                    
                                } else if (oelsize==sizeof(double)){
                                    doval = ((double *)array)[icount];
                                    if (cliplow < cliphigh) {
                                        if (doval < cliplow) doval = cliplow;
                                        if (doval > cliphigh) doval = cliphigh;
                                    }
                                    if (doval < valuelow || doval > valuehigh) {
                                        doval = 0.;
                                    } else {
                                        doval -= (valuelow - 1.);
                                    }
                                    ((double *)array)[icount] = doval;
                                    
                                } else {
                                    free(narray); free(array); return CBF_ARGUMENT;
                                    
                                }
                                
                            }
                            
                        }
                        
                        if ((eltype & CBF_CPY_SETINTEGER) || eltype == 0 ) {
                            
                            /* real to integer conversion */
                            
                            double maxval, minval;
                            
#ifndef CBF_USE_LONG_LONG
                            double onemore;
                            
                            unsigned long int maxlongval;
                            
                            maxlongval = ~0L;
                            
                            onemore = ((double)maxlongval)+1.;
#endif
                            
                            if (nelunsigned) {
                                
                                minval = 0.;
                                
                                switch( nelsize )  {
                                        
                                    case 1:  maxval = (double)(0xFF); break;
                                    case 2:  maxval = (double)(0xFFFFU); break;
                                    case 4:  maxval = (double)(0xFFFFFFFFUL); break;
                                    case 8:  maxval = ((double)(0xFFFFFFFFUL))*(2.+((double)(0xFFFFFFFFUL))); break;
                                    default: free(array); free(narray); return CBF_ARGUMENT;
                                        
                                }
                                
                            } else if (nelsigned) {
                                
                                switch( nelsize ) {
                                        
                                    case 1:  maxval = (double)(0x7F); break;
                                    case 2:  maxval = (double)(0x7FFFU); break;
                                    case 4:  maxval = (double)(0x7FFFFFFFUL); break;
                                    case 8:  maxval = ((double)(0xFFFFFFFFUL)) +
                                        ((double)(0x7FFFFFFFL))*(1.+((double)(0xFFFFFFFFUL))); break;
                                    default: free(array); free(narray); return CBF_ARGUMENT;
                                        
                                }
                                
                                minval = -maxval;
                                
                                if ((int)(~0)+1 == 0) minval = minval -1;
                                
                            } else {free(array); free(narray); return CBF_ARGUMENT;}
                            
                            
                            if (nelsize==sizeof(char)){
                                
                                if ((ssize_t)oelsize == sizeof(float)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /* if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed char *)narray)[icount] = (signed char)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned char *)narray)[icount] = (unsigned char)valtemp;
                                        }
                                        
                                    }
                                    
                                } else if ((ssize_t)oelsize == sizeof(double)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /* if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed char *)narray)[icount] = (signed char)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned char *)narray)[icount] = (unsigned char)valtemp;
                                        }
                                        
                                    }
                                    
                                } else { free(narray); free(array); return CBF_ARGUMENT;}
                                
                                
                            } else if (nelsize==sizeof(short int)){
                                
                                if ((ssize_t)oelsize == sizeof(float)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed short int *)narray)[icount] = (signed short int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned short int *)narray)[icount] = (unsigned short int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else if ((ssize_t)oelsize == sizeof(double)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed short int *)narray)[icount] = (signed short int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned short int *)narray)[icount] = (unsigned short int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else { free(narray); free(array); return CBF_ARGUMENT;}
                                
                            } else if (nelsize==sizeof(int)){
                                
                                if ((ssize_t)oelsize == sizeof(float)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed int *)narray)[icount] = (signed int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned int *)narray)[icount] = (unsigned int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else if ((ssize_t)oelsize == sizeof(double)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed int *)narray)[icount] = (signed int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned int *)narray)[icount] = (unsigned int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else { free(narray); free(array); return CBF_ARGUMENT;}
                                
                                
                            } else if (nelsize==sizeof(long int)){
                                
                                if ((ssize_t)oelsize == sizeof(float)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed long int *)narray)[icount] = (signed long int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned long int *)narray)[icount] = (unsigned long int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else if ((ssize_t)oelsize == sizeof(double)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed long int *)narray)[icount] = (signed long int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned long int *)narray)[icount] = (unsigned long int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else { free(narray); free(array); return CBF_ARGUMENT;}
                                
                                
#ifdef CBF_USE_LONG_LONG
                            } else if (nelsize==sizeof(long long int)){
                                
                                if ((ssize_t)oelsize == sizeof(float)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed long long int *)narray)[icount] = (signed long long int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned long long int *)narray)[icount] = (unsigned long long int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else if ((ssize_t)oelsize == sizeof(double)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((signed long long int *)narray)[icount] = (signed long long int)valtemp;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            ((unsigned long long int *)narray)[icount] = (unsigned long long int)valtemp;
                                        }
                                        
                                    }
                                    
                                } else { free(narray); free(array); return CBF_ARGUMENT;}
                                
#else
                            } else if (nelsize==2* sizeof(long int)){
                                
                                if (toupper(border[0])=='L') {
                                    
                                    lobyte = 0;
                                    
                                    hibyte = 1;
                                    
                                } else {
                                    
                                    lobyte = 1;
                                    
                                    hibyte = 0;
                                    
                                }
                                
                                
                                if ((ssize_t)oelsize == sizeof(float)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            vallow = fmod(valtemp,onemore);
                                            
                                            valhigh = (valtemp-vallow)/onemore;
                                            
                                            ((unsigned long int *)narray)[2*icount+lobyte] = (unsigned long int)vallow;
                                            
                                            ((signed long int *)narray)[2*icount+hibyte] = (signed long int)valhigh;
                                            
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((float *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            vallow = fmod(valtemp,onemore);
                                            
                                            valhigh = (valtemp-vallow)/onemore;
                                            
                                            ((unsigned long int *)narray)[2*icount+lobyte] = (unsigned long int)vallow;
                                            
                                            ((unsigned long int *)narray)[2*icount+hibyte] = (unsigned long int)valhigh;
                                            
                                        }
                                        
                                    }
                                    
                                } else if ((ssize_t)oelsize == sizeof(double)) {
                                    
                                    if (nelsigned) {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            vallow = fmod(valtemp,onemore);
                                            
                                            valhigh = (valtemp-vallow)/onemore;
                                            
                                            ((unsigned long int *)narray)[2*icount+lobyte] = (unsigned long int)vallow;
                                            
                                            ((signed long int *)narray)[2*icount+hibyte] = (signed long int)valhigh;
                                        }
                                        
                                    } else {
                                        
                                        for (icount = 0; icount < (int)elements; icount++) {
                                            
                                            valtemp = ((double *)array)[icount];
                                            
                                            /*if (valtemp < minval || valtemp > maxval) {
                                             
                                             free(array); free(narray); return CBF_OVERFLOW;
                                             }*/
                                            
                                            if (valtemp > maxval) valtemp=maxval;
                                            if (valtemp < minval) valtemp=minval;
                                            
                                            vallow = fmod(valtemp,onemore);
                                            
                                            valhigh = (valtemp-vallow)/onemore;
                                            
                                            ((unsigned long int *)narray)[2*icount+lobyte] = (unsigned long int)vallow;
                                            
                                            ((unsigned long int *)narray)[2*icount+hibyte] = (signed long int)valhigh;
                                            
                                        }
                                        
                                    }
                                    
                                } else { free(narray); free(array); return CBF_ARGUMENT;}
                                
                                
#endif
                                
                            } else {
                                
                                free(array);
                                
                                free(narray);
                                
                                return CBF_ARGUMENT;
                                
                            }
                            
                            
                            
                            cbf_failnez(cbf_set_integerarray_wdims_fs(
                                                                      cbfout, compression,
                                                                      binary_id, narray, nelsize, nelsigned, elements,
                                                                      "little_endian", dimfast, dimmid, dimslow, 0))
                            free(narray);
                            
                            free(array);
                            
                            
                        } else {
                            
                            /* real to real conversion */
                            
                            if (oelsize==sizeof(float)){
                                if (nelsize == sizeof(float)) {
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        ((float *)narray)[icount] = ((float *)array)[icount];
                                    }
                                } else if (nelsize == sizeof(double)) {
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        ((double *)narray)[icount] = ((float *)array)[icount];
                                    }
                                } else {free(array); free(narray); return CBF_ARGUMENT;}
                            } else if (oelsize==sizeof(double)){
                                if (nelsize == sizeof(float)) {
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        ((float *)narray)[icount] = ((double *)array)[icount];
                                    }
                                } else if (nelsize == sizeof(double)) {
                                    for (icount = 0; icount < (int)elements; icount++) {
                                        ((double *)narray)[icount] = ((double *)array)[icount];
                                    }
                                } else {free(array); free(narray); return CBF_ARGUMENT;}
                            } else { free(array); free(narray); return CBF_ARGUMENT;
                            }
                            
                            cbf_failnez(cbf_set_realarray_wdims_fs(
                                                                   cbfout, compression,
                                                                   binary_id, narray, nelsize, elements,
                                                                   "little_endian", dimfast, dimmid, dimslow, 0))
                            
                            free(array);
                            
                            free(narray);
                            
                            return 0;
                            
                        }
                        
                    } else {
                        
                        return CBF_ALLOC;
                    }
                }
            }
            
            
        } else {
            
            return CBF_ALLOC;
        }
    }
    
    return 0;
}


/* Convert an roi from a string to dimension limits */

int cbf_convertroi(char *roi,
                   size_t dimfast, size_t dimmid, size_t dimslow,
                   size_t * fastlow, size_t * fasthigh,
                   size_t * midlow,  size_t * midhigh,
                   size_t * slowlow, size_t * slowhigh,
                   double * valuelow, double * valuehigh) {
    size_t  xfastlow,  xfasthigh,
    xmidlow,   xmidhigh,
    xslowlow,  xslowhigh;
    double xvaluelow,  xvaluehigh;
    char * endptr;
    char * str;
    xfastlow = xslowlow = xmidlow = 0;
    if (dimfast == 0) dimfast = 1;
    if (dimmid  == 0) dimmid = 1;
    if (dimslow == 0) dimslow = 1;
    xfasthigh = dimfast-1;
    xslowhigh = dimslow-1;
    xmidhigh = dimmid-1;
    xvaluelow = -DBL_MAX;
    xvaluehigh = DBL_MAX;
    if (fastlow) *fastlow = 0;
    if (midlow) *midlow = 0;
    if (slowlow) *slowlow = 0;
    if (fasthigh) *fasthigh = dimfast-1;
    if (midhigh) *midhigh = dimmid-1;
    if (slowhigh) *slowhigh = dimslow-1;
    if (valuelow) *valuelow = -DBL_MAX;
    if (valuehigh) *valuehigh = DBL_MAX;
    if (!roi) {
        return CBF_SUCCESS;
    }
    str = roi;
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xfastlow = (int)strtol(str,&endptr,0);
        if (xfastlow > dimfast-1) xfastlow = dimfast-1;
        if (fastlow) *fastlow = xfastlow;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xfasthigh = (int)strtol(str,&endptr,0);
        if (xfasthigh < xfastlow) xfasthigh = xfastlow;
        if (xfasthigh > dimfast-1) xfasthigh = dimfast-1;
        if (fasthigh) *fasthigh = xfasthigh;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xmidlow = (int)strtol(str,&endptr,0);
        if (xmidlow > dimmid-1) xmidlow = dimmid-1;
        if(midlow) *midlow = xmidlow;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xmidhigh = (int)strtol(str,&endptr,0);
        if (xmidhigh < xmidlow) xmidhigh = xmidlow;
        if (xmidhigh > dimmid-1) xmidhigh = dimmid-1;
        if(midhigh) *midhigh = xmidhigh;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xslowlow = (int)strtol(str,&endptr,0);
        if (xslowlow > dimslow-1) xslowlow = dimslow-1;
        if(slowlow) *slowlow = xslowlow;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xslowhigh = (int)strtol(str,&endptr,0);
        if (xslowhigh < xslowlow) xslowhigh = xslowlow;
        if (xslowhigh > dimslow-1) xslowhigh = dimslow-1;
        if(slowhigh) *slowhigh = xslowhigh;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xvaluelow = (double)strtod(str,&endptr);
        if(valuelow) *valuelow = xvaluelow;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xvaluehigh = (double)strtod(str,&endptr);
        if (xvaluehigh < xvaluelow) xvaluehigh=xvaluelow;
        if(valuehigh) *valuehigh = xvaluehigh;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    cbf_debug_print3("fastlow  %ld, fasthigh %ld\n", (long)fastlow,(long)fasthigh);
    cbf_debug_print3("midlow  %ld, midhigh %ld\n", (long)midlow,(long)midhigh);
    cbf_debug_print3("slowlow  %ld, slowhigh %ld\n", (long)slowlow,(long)slowhigh);
    cbf_debug_print3("valuelow  %ld, valuehigh %ld\n", (long)valuelow,(long)valuehigh);
    return CBF_SUCCESS;
    
}

/* Convert a binoi from a string to integer parameters */

int cbf_convertbinoi(char *binoi,
                     size_t * binratio,
                     size_t * modulefast, size_t * modulemid, size_t * moduleslow,
                     size_t * gapfast,  size_t * gapmid, size_t * gapslow) {
    size_t  xbinratio, xmodulefast, xmodulemid, xmoduleslow, xgapfast, xgapmid, xgapslow;
    char * endptr;
    char * str;
    xbinratio = xmodulefast = xmodulemid = xmoduleslow = xgapfast = xgapmid = xgapslow = 0;
    if (!binoi) {
        return CBF_SUCCESS;
    }
    str = binoi;
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xbinratio = (size_t)strtol(str,&endptr,0);
        if (xbinratio < 1 ) xbinratio = 1;
        if (binratio) *binratio = xbinratio;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xmodulefast = (size_t)strtol(str,&endptr,0);
        if (xmodulefast < 1) xmodulefast = 1;
        if (modulefast) *modulefast = xmodulefast;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xmodulemid = (size_t)strtol(str,&endptr,0);
        if (xmodulemid < 1) xmodulemid = 1;
        if (modulefast) *modulemid = xmodulemid;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xmoduleslow = (size_t)strtol(str,&endptr,0);
        if (xmoduleslow < 1) xmoduleslow = 1;
        if (moduleslow) *moduleslow = xmoduleslow;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xgapmid = (size_t)strtol(str,&endptr,0);
        if(gapmid) *gapmid = xgapmid;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xgapfast = (size_t)strtol(str,&endptr,0);
        if(gapfast) *gapfast = xgapfast;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    if (*str != ',') {
        xgapslow = (size_t)strtol(str,&endptr,0);
        if(gapslow) *gapslow = xgapslow;
        if (*endptr == '\0') return CBF_SUCCESS;
        if (*endptr != ',' && *endptr != ' ') return CBF_FORMAT;
        str = endptr+1;
    } else {
        str++;
    }
    while (*str && isspace(*str)) str++;
    cbf_debug_print2("binratio %ld, fasthigh %ld\n", (long)binratio);
    cbf_debug_print4("modulefast %ld, modulemid %ld, moduleslow %ld\n", (long)modulefast, (long)modulemid, (long)moduleslow);
    cbf_debug_print4("gapfast %ld, gapmid %ld, gapslow %ld\n", (long)gapfast, (long)gapmid, (long)gapslow);
    return CBF_SUCCESS;
}


/* Extract an ROI from an image array */

int cbf_extract_roi(void        * src,
                    void        * dst,
                    size_t        elsize,
                    size_t        fastlow,
                    size_t        fasthigh,
                    size_t        midlow,
                    size_t        midhigh,
                    size_t        slowlow,
                    size_t        slowhigh,
                    size_t        dimfast,
                    size_t        dimmid,
                    size_t        dimslow
                    ) {
    
    size_t indexmid, indexslow, index;
    
    void * tdst;
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != 2* sizeof (int) &&
        elsize != 4* sizeof (int) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char))
        
        return CBF_ARGUMENT;
    
    if (fasthigh < fastlow
        || fasthigh >= dimfast
        || midhigh < midlow
        || midhigh >= dimmid
        || slowhigh > slowlow
        || slowhigh >= dimslow )
        
        return CBF_ARGUMENT;
    
    tdst = dst;
    
    for (indexslow = slowlow; indexslow <= slowhigh; indexslow++) {
        
        for (indexmid = midlow; indexmid <= midhigh; indexmid++) {
            
            index = elsize*(fastlow +indexmid*dimfast+indexslow*dimfast*dimmid);
            
            memmove(tdst,(char *)src+index,(1+fasthigh-fastlow)*elsize);
            
            tdst = (char *)tdst + (1+fasthigh-fastlow)*elsize;
            
        }
    }
    
    return CBF_SUCCESS;
}
#define proc_extract(type,zero)                                                                    {\
    type * binned;                                                                                  \
    type * rawel;                                                                                   \
    binned = ( type *)dst;                                                                          \
    rawel = ( type *)src;                                                                           \
    for (indexslow = slowlow; indexslow <= slowhigh; indexslow++) {                                 \
        slowmodoff=indexslow%modszslow;                                                             \
        slowbinoff=indexslow%binratio;                                                              \
        indexbinslow=(indexslow+binratio-1)/binratio-(slowlow+binratio-1)/binratio;                 \
        for (indexmid = midlow; indexmid <= midhigh; indexmid++) {                                  \
            midmodoff=indexmid%modszmid;                                                            \
            midbinoff=indexmid%binratio;                                                            \
            indexbinmid=(indexmid+binratio-1)/binratio-(midlow+binratio-1)/binratio;                \
            for (indexfast = fastlow; indexfast <= fasthigh; indexfast++) {                         \
                fastmodoff=indexfast%modszfast;                                                     \
                fastbinoff=indexfast%binratio;                                                      \
                indexbinfast=(indexfast+binratio-1)/binratio-(fastlow+binratio-1)/binratio;         \
                if ( (indexfast==fastlow && indexmid==midlow && indexslow==slowlow)                 \
                    || (fastbinoff==0 && midbinoff==0 && slowbinoff==0)) {                          \
                    binned[indexbinfast+(bndfasthigh-bndfastlow-1)*indexbinmid                     \
                      +(bndfasthigh-bndfastlow+1)*(bndmidhigh-bndmidlow+1)*indexbinslow] = (zero);\
                }                                                                                   \
                if (slowmodoff < moduleslow && midmodoff < modulemid  && fastmodoff < modulefast) { \
                    binned[indexbinfast+(bndfasthigh-bndfastlow-1)*indexbinmid                     \
                    +       (bndfasthigh-bndfastlow+1)*(bndmidhigh-bndmidlow+1)*indexbinslow] +=  \
                    rawel[indexfast + dimfast*indexmid + dimfast*dimmid*indexslow];                 \
                }                                                                                   \
            }                                                                                       \
        }                                                                                           \
    }                                                                                               \
    return CBF_SUCCESS;                                                                             \
}


/* Extract an ROI from an image array into a BINOI */

int cbf_extract_roi_binoi(void        * src,
                          void        * dst,
                          size_t        elsize,
                          int           elsigned,
                          int           realarray,
                          size_t        fastlow,
                          size_t        fasthigh,
                          size_t        midlow,
                          size_t        midhigh,
                          size_t        slowlow,
                          size_t        slowhigh,
                          size_t        dimfast,
                          size_t        dimmid,
                          size_t        dimslow,
                          size_t        binratio,
                          size_t        bndfastlow,
                          size_t        bndfasthigh,
                          size_t        bndmidlow,
                          size_t        bndmidhigh,
                          size_t        bndslowlow,
                          size_t        bndslowhigh,
                          size_t        modulefast,
                          size_t        modulemid,
                          size_t        moduleslow,
                          size_t        gapfast,
                          size_t        gapmid,
                          size_t        gapslow
                          ) {
    
    size_t indexmid, indexslow, indexfast, index;
    
    size_t faststart, midstart, slowstart;
    
    size_t modszfast, modszmid, modszslow;
    
    size_t indexbinfast, indexbinmid, indexbinslow, indexbin;
    
    modszfast=modulefast+gapfast;
    
    modszmid=modulemid+gapmid;
    
    modszslow=moduleslow+gapslow;
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != 2* sizeof (int) &&
        elsize != 4* sizeof (int) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char) &&
        elsize != sizeof (float) &&
        elsize != sizeof (double))
        
        return CBF_ARGUMENT;
    
    if (fasthigh < fastlow
        || fasthigh >= dimfast
        || midhigh < midlow
        || midhigh >= dimmid
        || slowhigh > slowlow
        || slowhigh >= dimslow
        || bndfasthigh < bndfastlow
        || bndfasthigh >= dimfast+binratio-1/binratio
        || bndmidhigh < bndmidlow
        || bndmidhigh >= dimmid+binratio-1/binratio
        || bndslowhigh < bndslowlow
        || bndslowhigh >= dimslow+binratio-1/binratio )
        
        return CBF_ARGUMENT;
    
    size_t fastmodoff, fastbinoff, midmodoff, midbinoff, slowmodoff, slowbinoff;
    
    if (realarray) {
        if (elsize == sizeof (float) ){
            proc_extract( float , 0. );
        } else if (elsize == sizeof (double) ) {
            proc_extract( double , 0. );
        } else return CBF_ARGUMENT;
    } else if (elsigned) {  // signed integer types 
        if (elsize == sizeof (char) ) {
            proc_extract( char , 0 );
        } else if (elsize == sizeof (int) ) {
            proc_extract( int , 0 );
        } else if (elsize == sizeof (long) ) {
            proc_extract( long, 0L );
#ifdef CBF_USE_LONG_LONG
        } else if (elsize == sizeof (long long) ) {
            proc_extract( long long , 0L );
#endif 
        } else if (elsize == sizeof (short) ) {
            proc_extract( short , 0 );
        } else return CBF_ARGUMENT;
    } else { // unsigned integer types 
        if (elsize == sizeof (unsigned char) ) {
            proc_extract( unsigned char , 0 );
        } else if (elsize == sizeof (unsigned int) ) {
            proc_extract( unsigned int , 0 );
        } else if (elsize == sizeof (unsigned long) ) {
            proc_extract( unsigned long , 0L );
#ifdef CBF_USE_LONG_LONG
        } else if (elsize == sizeof (unsigned long long) ) {
            proc_extract( unsigned long long , 0L );
#endif
        } else if (elsize == sizeof (unsigned short) ) {
            proc_extract( unsigned short , 0);
        } else return CBF_ARGUMENT;
    } 
    return CBF_SUCCESS;
}

/* Multiply a 3x3 matrix times a 3-vector to produce a 3-vector */

int cbf_mat33_vec(double mat[3][3], double vecin[3], double vecout[3]) {
    
    size_t i, j;
    
    for (i=0; i < 3; i++) {
        
        vecout[i] = 0.;
        
        for (j=0; j <3; j++) {
            
            vecout[i]+=mat[i][j]*vecin[j];
            
        }
        
    }
    
    return CBF_SUCCESS;
    
}


/* Convert index to dimension-by-dimension indices  */

int cbf_convert_index(const ssize_t index,
                      const size_t dimfast, const size_t dimmid, const size_t dimslow,
                      size_t * indexfast, size_t * indexmid, size_t * indexslow) {
    
    size_t balance;
    
    if (index < 0 || index >= dimfast*dimmid*dimslow) return CBF_ARGUMENT;
    
    *indexslow = (size_t)(index/(dimfast*dimmid));
    balance= index-(*indexslow)*(dimfast*dimmid);
    *indexmid = (size_t)(balance/dimfast);
    *indexfast = balance-(*indexmid)*dimfast;
    return CBF_SUCCESS;
}

/* Extract a 2D ROI from an image array, rotate in 3D and project back to 2D */


int cbf_extract_rotated_roi_2D(void        * src,
                               void        * dst,
                               size_t        elsize,
                               int           elsigned,
                               int           real,
                               size_t        fastlow,
                               size_t        fasthigh,
                               size_t        midlow,
                               size_t        midhigh,
                               size_t        dimfast,
                               size_t        dimmid,
                               double        rotmat[3][3],
                               double        center[2],
                               double        pixsize[2]
                               ) {
    
    ssize_t indexfast, indexmid, indexslow, index, newindex;
    
    ssize_t newindexleftdown,newindexleftmid,newindexleftup;
    ssize_t newindexmiddown,newindexmidup;
    ssize_t newindexrightdown,newindexrightmid,newindexrightup;
    
    ssize_t newvalueleftdown,newvalueleftmid,newvalueleftup;
    ssize_t newvaluemiddown,newvaluemidup;
    ssize_t newvaluerightdown,newvaluerightmid,newvaluerightup;
    
    size_t dimslow=1;
    
    double dist_hl0, dist_hh0, dist_ll0, dist_lh0;
    
    double newdist_hl0, newdist_hh0, newdist_ll0, newdist_lh0;
    
    double newscale;
    
    double roi_hl0[3], roi_hh0[3];
    
    double roi_ll0[3], roi_lh0[3];
    
    double newroi_hl0[3], newroi_hh0[3];
    
    double newroi_ll0[3], newroi_lh0[3];
    
    double posfastlow, posfasthigh, posmidlow, posmidhigh;
    
    double newfastlow, newfasthigh, newmidlow, newmidhigh;
    
    double newindexlow, newindexhigh;
    
    long lnewfast, lnewmid;
    
    double deltafast, deltamid, deltafastlow, deltamidlow, deltafasthigh, deltamidhigh;;
    
    double dvalue;
    
    float fvalue;
    
    unsigned char ucvalue;
    
    unsigned short usvalue;
    
    unsigned int uivalue;
    
    unsigned long ulvalue;
    
    char cvalue;
    
    short svalue;
    
    int ivalue;
    
    long lvalue;
    
    void * tdst;
    
    size_t i, j, k, ii, jj;
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != 2* sizeof (int) &&
        elsize != 4* sizeof (int) &&
        elsize != sizeof (long) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char))
        
        return CBF_ARGUMENT;
    
    if (fasthigh < fastlow
        || fasthigh >= dimfast
        || midhigh < midlow
        || midhigh >= dimmid )
        
        return CBF_ARGUMENT;
    
    roi_hl0[0]=(((double)(fasthigh+1))-center[0])*pixsize[0];
    
    roi_hl0[1]=(((double)(midlow))-center[1])*pixsize[1];
    
    roi_hl0[2]=0.;
    
    roi_hh0[0]=(((double)(fasthigh+1))-center[0])*pixsize[0];
    
    roi_hh0[1]=(((double)(midhigh+1))-center[1])*pixsize[1];
    
    roi_hh0[2]=0.;
    
    roi_ll0[0]=(((double)(fastlow))-center[0])*pixsize[0];
    
    roi_ll0[1]=(((double)(midlow))-center[1])*pixsize[1];
    
    roi_ll0[2]=0.;
    
    roi_lh0[0]=(((double)(fastlow))-center[0])*pixsize[0];
    
    roi_lh0[1]=(((double)(midhigh+1))-center[1])*pixsize[1];
    
    roi_lh0[2]=0.;
    
    cbf_failnez(cbf_mat33_vec(rotmat, roi_hl0, newroi_hl0));
    
    cbf_failnez(cbf_mat33_vec(rotmat, roi_hh0, newroi_hh0));
    
    cbf_failnez(cbf_mat33_vec(rotmat, roi_ll0, newroi_ll0));
    
    cbf_failnez(cbf_mat33_vec(rotmat, roi_lh0, newroi_lh0));
    
    /* The points roi_hl0, roi_hh0, roi_ll0 and roi_lh0
     map to newroi_hl0, newroi_hh0, newroi_ll0 and newroi_lh0
     respectively  including scaling by pixel size and projection
     of the rotation onto the x-y plane
     
     These are the corners of the full roi  rounded up at the high
     corners
     
     */
    
    dist_hl0 = sqrt(roi_hl0[0]*roi_hl0[0]+roi_hl0[1]*roi_hl0[1]);
    dist_hh0 = sqrt(roi_hh0[0]*roi_hh0[0]+roi_hh0[1]*roi_hh0[1]);
    dist_ll0 = sqrt(roi_ll0[0]*roi_ll0[0]+roi_ll0[1]*roi_ll0[1]);
    dist_lh0 = sqrt(roi_lh0[0]*roi_lh0[0]+roi_lh0[1]*roi_lh0[1]);
    newdist_hl0 = sqrt(newroi_hl0[0]*newroi_hl0[0]+newroi_hl0[1]*newroi_hl0[1]);
    newdist_hh0 = sqrt(newroi_hh0[0]*newroi_hh0[0]+newroi_hh0[1]*newroi_hh0[1]);
    newdist_ll0 = sqrt(newroi_ll0[0]*newroi_ll0[0]+newroi_ll0[1]*newroi_ll0[1]);
    newdist_lh0 = sqrt(newroi_lh0[0]*newroi_lh0[0]+newroi_lh0[1]*newroi_lh0[1]);
    
    newscale=(dist_hl0+dist_hh0+dist_ll0+dist_lh0)/(newdist_hl0+newdist_hh0+newdist_ll0+newdist_lh0);
    
    newroi_hl0[0] /= pixsize[0]; newroi_hh0[0] /= pixsize[0]; newroi_ll0[0] /= pixsize[0]; newroi_lh0[0] /= pixsize[0];
    
    newroi_hl0[1] /= pixsize[1]; newroi_hh0[1] /= pixsize[1]; newroi_ll0[1] /= pixsize[1]; newroi_lh0[1] /= pixsize[1];
    
    newroi_hl0[0] *= newscale; newroi_hh0[0] *= newscale; newroi_ll0[0] *= newscale; newroi_lh0[0] *=newscale;
    
    newroi_hl0[1] *= newscale; newroi_hh0[1] *= newscale; newroi_ll0[1] *=newscale; newroi_lh0[1] *=newscale;
    
    newroi_hl0[2] = newroi_hh0[2] = newroi_ll0[2] = newroi_lh0[2] =0.;
    
    newroi_hl0[0] += center[0]; newroi_hh0[0] += center[0]; newroi_ll0[0] += center[0]; newroi_lh0[0] += center[0];
    
    newroi_hl0[1] += center[1]; newroi_hh0[1] += center[1]; newroi_ll0[1] += center[1]; newroi_lh0[1] += center[1];
    
    /* fprintf(stderr,"newroi_hl0:  %15.6g, %15.6g,%15.6g\n", newroi_hl0[0], newroi_hl0[1],newroi_hl0[2]);
     fprintf(stderr,"newroi_hh0:  %15.6g, %15.6g,%15.6g\n", newroi_hh0[0], newroi_hh0[1],newroi_hh0[2]);
     fprintf(stderr,"newroi_ll0:  %15.6g, %15.6g,%15.6g\n", newroi_ll0[0], newroi_ll0[1],newroi_ll0[2]);
     fprintf(stderr,"newroi_lh0:  %15.6g, %15.6g,%15.6g\n", newroi_lh0[0], newroi_lh0[1],newroi_lh0[2]);
     fprintf(stderr,"newscale: %15.6g\n", newscale); */
    
    tdst = dst;
    
    indexslow = 0;
    
    /* clear the roi */
    
    {
        
        for (indexmid = midlow; indexmid <= midhigh; indexmid++) {
            
            memset(tdst,0,(1+fasthigh-fastlow)*elsize);
            
            tdst = (char *)tdst + (1+fasthigh-fastlow)*elsize;
            
        }
    }
    /* transfer each row one element at a time */
    {   double oldpos[3], newpos[3];
        
        double dhpos[3][3], dvpos[3][3];
        
        ssize_t vertprev=0, vertcur, horzprev=0, horzcur;
        
        double dvertcur, dhorzcur, dvertfrac, dhorzfrac;
        double dvertfracup, dvertfracdown, dhorzfracleft, dhorzfracright;
        double dvertfracmid, dhorzfracmid;
        
        size_t xnewindexleftdownfast, xindexleftdownmid, xindexleftdownslow;
        size_t xnewindexleftmidfast,  xindexleftmidmid,  xindexleftmidslow;
        size_t xnewindexleftupfast,   xindexleftupmid,   xindexleftupslow;
        size_t xnewindexmiddownfast,  xindexmiddownmid,  xindexmiddownslow;
        size_t xnewindexmidupfast,    xindexmidupmid,    xindexmidupslow;
        size_t xnewindexrightdownfast,xindexrightdownmid,xindexrightdownslow;
        size_t xnewindexrightmidfast, xindexrightmidmid, xindexrightmidslow;
        size_t xnewindexrightupfast,  xindexrightupmid,  xindexrightupslow;
        
        
        
        oldpos[2] = 0.;
        
        for (indexmid = midlow; indexmid <= midhigh; indexmid++) {
            
            oldpos[1]=((double)(indexmid)-center[1])*pixsize[1];
            
            for (indexfast = fastlow; indexfast <= fasthigh; indexfast++) {
                
                oldpos[0]=((double)(indexfast)-center[0])*pixsize[0];
                
                cbf_failnez(cbf_mat33_vec(rotmat, oldpos, newpos));
                
                /* if (indexmid < 12 && indexfast < 12) {
                 fprintf(stderr,"indexfast, indexmid: %15.6g %15.6g, oldpos: %15.6g %15.6g %15.6g\n", indexfast, indexmid, oldpos[0], oldpos[1], oldpos[2]);
                 fprintf(stderr,"newfast, newmid: %15.6g %15.6g, newpos: %15.6g %15.6g %15.6g\n",
                 newpos[0]/pixsize[0]*newscale+center[0], newpos[1]/pixsize[1]*newscale+center[1], newpos[0], newpos[1], newpos[2]);
                 } */
                
                if (indexfast > fastlow) {
                    
                    vertprev = vertcur;
                    
                    horzprev = horzcur;
                    
                }
                
                /*  (dhorzcur, dvertcur) is the lower left corner of  the box
                 containing (horzcor, vertcur) */
                
                dhorzcur = newpos[0]/pixsize[0]*newscale+center[0];
                
                dvertcur = newpos[1]/pixsize[1]*newscale+center[1];
                
                horzcur=(ssize_t)(dhorzcur+0.5);
                
                vertcur=(ssize_t)(dvertcur+0.5);
                
                if (horzcur < fastlow) horzcur=fastlow;
                
                if (horzcur > fasthigh) horzcur=fasthigh;
                
                if (vertcur < midlow) vertcur=midlow;
                
                if (vertcur > midhigh) vertcur=midhigh;
                
                /* if dhorzcur is between horzcur-.5 and horzcor,  a fraction
                 of the value is donated to the left
                 if dhorzcur is between horzcur and horzcur+.5, a fraction
                 of the value os donated to the right
                 if dvertcur is between vertcuz-.5 and vertcur,  a fraction
                 of the value is donated below
                 if dvertcur is between vertcur and vertcur+.5, a fraction
                 of the value os donated above
                 
                 when the difference is 0, the  donated fraction is 0
                 when the magnitude of the difference is > .5, the donated
                 fraction is 1
                 */
                
                dhorzfrac=dhorzcur-(double)horzcur;
                
                if (dhorzfrac < 0.)  {
                    
                    dhorzfracleft = -2.*dhorzfrac;
                    if (dhorzfracleft > 1.) dhorzfracleft =1.;
                    dhorzfracright = 0.;
                    dhorzfracmid = 1.-dhorzfracleft;
                    
                } else {
                    
                    dhorzfracright = 2.*dhorzfrac;
                    if (dhorzfracright > 1.) dhorzfracright =1.;
                    dhorzfracleft = 0.;
                    dhorzfracmid = 1.-dhorzfracright;
                    
                }
                
                dvertfrac=dvertcur-(double)vertcur;
                
                if (dvertfrac < 0.)  {
                    
                    dvertfracdown = -2.*dvertfrac;
                    if (dvertfracdown > 1.) dvertfracdown =1.;
                    dvertfracup = 0.;
                    dvertfracmid = 1.-dvertfracdown;
                    
                } else {
                    
                    dvertfracup = 2.*dvertfrac;
                    if (dvertfracup > 1.) dvertfracup =1.;
                    dvertfracdown = 0.;
                    dvertfracmid = 1.-dvertfracup;
                    
                }
                
                /* dhorzfracleft = 0.;
                 dhorzfracright= 0.;
                 dvertfracup = 0.;
                 dvertfracdown = 0.; */
                
                
                if (vertcur <  midlow || vertcur > midhigh || horzcur <  fastlow || horzcur > fasthigh) continue;
                
                if (indexfast == fastlow) {
                    
                    vertprev = vertcur;
                    
                    horzprev = horzcur;
                    
                }
                
                for (ii=0; ii<3; ii++) {
                    for (jj=0; jj<3; jj++) {
                        dhpos[ii][jj]=dhorzcur;
                        dvpos[ii][jj]=dvertcur;
                        if(dhorzcur+(double)(ii-1) >= (double)fastlow && dhorzcur+(double)(ii-1) <=(double)fasthigh)
                            dhpos[ii][jj] = dhorzcur+(double)(ii-1);
                        if(dvertcur+(double)(jj-1) >= (double)midlow && dvertcur+(double)(jj-1) <=(double)midhigh)
                            dvpos[ii][jj] = dvertcur+(double)(jj-1);
                    }
                }
                
                index = indexfast +indexmid*dimfast+indexslow*dimfast*dimmid;
                
                newindex = horzcur +vertcur*dimfast+indexslow*dimfast*dimmid;
                newindexlow = fastlow + midlow*dimfast+indexslow*dimfast*dimmid;
                newindexhigh = fasthigh + midhigh*dimfast+indexslow*dimfast*dimmid;
                
                newindexleftdown = dhpos[0][0]+dvpos[0][0]*dimfast+indexslow*dimfast*dimmid;
                newindexleftmid = dhpos[0][1]+dvpos[0][1]*dimfast+indexslow*dimfast*dimmid;
                newindexleftup = dhpos[0][2]+dvpos[0][2]*dimfast+indexslow*dimfast*dimmid;
                newindexmiddown = dhpos[1][0]+dvpos[1][0]*dimfast+indexslow*dimfast*dimmid;
                newindexmidup = dhpos[1][2]+dvpos[1][2]*dimfast+indexslow*dimfast*dimmid;
                newindexrightdown = dhpos[2][0]+dvpos[2][0]*dimfast+indexslow*dimfast*dimmid;
                newindexrightmid = dhpos[2][1]+dvpos[2][1]*dimfast+indexslow*dimfast*dimmid;
                newindexrightup = dhpos[2][2]+dvpos[2][2]*dimfast+indexslow*dimfast*dimmid;
                
                if (newindexleftdown < newindexlow) newindexleftdown = newindexlow;
                if (newindexleftmid < newindexlow) newindexleftmid = newindexlow;
                if (newindexleftup < newindexlow) newindexleftup = newindexlow;
                if (newindexmiddown < newindexlow) newindexmiddown = newindexlow;
                if (newindexmidup < newindexlow) newindexmidup = newindexlow;
                if (newindexrightdown < newindexlow) newindexrightdown = newindexlow;
                if (newindexrightmid < newindexlow) newindexrightmid = newindexlow;
                if (newindexrightup < newindexlow) newindexrightup = newindexlow;
                
                if (newindexleftdown > newindexhigh) newindexleftdown = newindexhigh;
                if (newindexleftmid > newindexhigh) newindexleftmid = newindexhigh;
                if (newindexleftup > newindexhigh) newindexleftup = newindexhigh;
                if (newindexmiddown > newindexhigh) newindexmiddown = newindexhigh;
                if (newindexmidup > newindexhigh) newindexmidup = newindexhigh;
                if (newindexrightdown > newindexhigh) newindexrightdown = newindexhigh;
                if (newindexrightmid > newindexhigh) newindexrightmid = newindexhigh;
                if (newindexrightup > newindexhigh) newindexrightup = newindexhigh;
                
                cbf_failnez(cbf_convert_index(newindexleftdown, dimfast, dimmid, dimslow, &xnewindexleftdownfast, &xindexleftdownmid, &xindexleftdownslow));
                cbf_failnez(cbf_convert_index(newindexleftmid,  dimfast, dimmid, dimslow, &xnewindexleftmidfast,  &xindexleftmidmid,  &xindexleftmidslow));
                cbf_failnez(cbf_convert_index(newindexleftup,   dimfast, dimmid, dimslow, &xnewindexleftupfast,   &xindexleftupmid,   &xindexleftupslow));
                cbf_failnez(cbf_convert_index(newindexmiddown,  dimfast, dimmid, dimslow, &xnewindexmiddownfast,  &xindexmiddownmid,  &xindexmiddownslow));
                cbf_failnez(cbf_convert_index(newindexmidup,    dimfast, dimmid, dimslow, &xnewindexmidupfast,    &xindexmidupmid,    &xindexmidupslow));
                cbf_failnez(cbf_convert_index(newindexrightdown,dimfast, dimmid, dimslow, &xnewindexrightdownfast,&xindexrightdownmid,&xindexrightdownslow));
                cbf_failnez(cbf_convert_index(newindexrightmid, dimfast, dimmid, dimslow, &xnewindexrightmidfast, &xindexrightmidmid, &xindexrightmidslow));
                cbf_failnez(cbf_convert_index(newindexrightup,  dimfast, dimmid, dimslow, &xnewindexrightupfast,  &xindexrightupmid,  &xindexrightupslow));
                
                /* if (indexfast< 12 && indexmid < 12)
                 
                 {
                 fprintf(stderr,"newindexleftdown, xnewindexleftdownfast, xindexleftdownmid, xindexleftdownslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexleftdown, (double)xnewindexleftdownfast, (double)xindexleftdownmid, (double)xindexleftdownslow);
                 fprintf(stderr,"newindexleftmid, xnewindexleftmidfast,  xindexleftmidmid,  xindexleftmidslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexleftmid, (double)xnewindexleftmidfast, (double)xindexleftmidmid, (double)xindexleftmidslow);
                 fprintf(stderr,"newindexleftup, xnewindexleftupfast, xindexleftupmid, xindexleftupslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexleftup, (double)xnewindexleftupfast, (double)xindexleftupmid, (double)xindexleftupslow);
                 fprintf(stderr,"newindexmiddown, xnewindexmiddownfast, xindexmiddownmid, xindexmiddownslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexmiddown, (double)xnewindexmiddownfast, (double)xindexmiddownmid, (double)xindexmiddownslow);
                 fprintf(stderr,"newindexmidup, xnewindexmidupfast, xindexmidupmid, xindexmidupslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexmidup, (double)xnewindexmidupfast, (double)xindexmidupmid, (double)xindexmidupslow);
                 fprintf(stderr,"newindexrightdown, xnewindexrightdownfast, xindexrightdownmid, xindexrightdownslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexrightdown, (double)xnewindexrightdownfast, (double)xindexrightdownmid, (double)xindexleftdownslow);
                 fprintf(stderr,"newindexrightmid, xnewindexrightmidfast, xindexrightmidmid, xindexleftrightmidslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexrightmid, (double)xnewindexrightmidfast, (double)xindexrightmidmid, (double)xindexrightmidslow);
                 fprintf(stderr,"newindexrightup, xnewindexrightupfast, xindexrightupmid, xindexrightupslow: %15.6g %15.6g %15.6g %15.6g\n",
                 (double)newindexrightup, (double)xnewindexrightupfast, (double)xindexrightupmid, (double)xindexrightupslow);
                 
                 fprintf(stderr," orig pos(%15.6g,%15.6g), "
                 
                 " cur pos(%15.6g,%15.6g)\n", (double)indexfast, (double)indexmid, (double)horzcur, (double)vertcur);
                 
                 fprintf(stderr, "index, newindex, newindexleftdown, newindexleftmid, newindexleftup, "
                 
                 " newindexmiddown, newindexmidup, newindexrightdown, newindexrightmid, newindexrightup: "
                 
                 " %15.6g, %15.6g, %15.6g, %15.6g, %15.6g, %15.6g, %15.6g, %15.6g, %15.6g, %15.6g\n",
                 
                 (double)index, (double)newindex, (double)newindexleftdown, (double)newindexleftmid, (double)newindexleftup,
                 
                 (double)newindexmiddown, (double)newindexmidup, (double)newindexrightdown, (double)newindexrightmid, (double)newindexrightup);
                 
                 
                 }
                 */
                
                /* if (index != newindex && abs(index-newindex) > 20000) {
                 
                 fprintf(stderr," orig pos(%15.6g,%15.6g), "
                 
                 "cur pos(%15.6g,%15.6g)\n", (double)indexfast, (double)indexmid, (double)horzcur, (double)vertcur);
                 
                 } */
                
                if (real) {
                    
                    if (sizeof(double)==elsize) {
                        dvalue=((double *)src)[index];
                        if (vertprev <= vertcur+1 && vertprev+1 >= vertcur && horzprev <= horzcur+1 && horzprev+1 >= horzcur) {
                            ((double *)dst)[newindex] += (dvalue*dhorzfracmid*dvertfracmid);
                            ((double *)dst)[newindexleftdown]  += (dvalue*dhorzfracleft*dvertfracdown);
                            ((double *)dst)[newindexleftmid]   += (dvalue*dhorzfracleft*dvertfracmid);
                            ((double *)dst)[newindexleftup]    += (dvalue*dhorzfracleft*dvertfracup);
                            ((double *)dst)[newindexmiddown]   += (dvalue*dhorzfracmid*dvertfracdown);
                            ((double *)dst)[newindexmidup]     += (dvalue*dhorzfracmid*dvertfracup);
                            ((double *)dst)[newindexrightdown] += (dvalue*dhorzfracright*dvertfracdown);
                            ((double *)dst)[newindexrightmid]  += (dvalue*dhorzfracright*dvertfracmid);
                            ((double *)dst)[newindexrightup]   += (dvalue*dhorzfracright*dvertfracup);
                        } else if (vertprev > vertcur+1) {
                            for (ii=0; ii < vertprev-vertcur; ii++) {
                                if (newindex+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindex+ii*(dimfast)]          += dvalue*dhorzfracmid*dvertfracmid/((double)(vertprev-vertcur));
                                if (newindexleftdown+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexleftdown+ii*(dimfast)]  += (dvalue*dhorzfracleft*dvertfracdown)/((double)(vertprev-vertcur));
                                if (newindexleftmid+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexleftmid+ii*(dimfast)]   += (dvalue*dhorzfracleft*dvertfracmid)/((double)(vertprev-vertcur));
                                if (newindexleftup+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexleftup+ii*(dimfast)]    += (dvalue*dhorzfracleft*dvertfracup)/((double)(vertprev-vertcur));
                                if (newindexmiddown+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexmiddown+ii*(dimfast)]   += (dvalue*dhorzfracmid*dvertfracdown)/((double)(vertprev-vertcur));
                                if (newindexmidup+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexmidup+ii*(dimfast)]     += (dvalue*dhorzfracmid*dvertfracup)/((double)(vertprev-vertcur));
                                if (newindexrightdown+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexrightdown+ii*(dimfast)] += (dvalue*dhorzfracright*dvertfracdown)/((double)(vertprev-vertcur));
                                if (newindexrightmid+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexrightmid+ii*(dimfast)]  += (dvalue*dhorzfracright*dvertfracmid)/((double)(vertprev-vertcur));
                                if (newindexrightup+ii*(dimfast)<=newindexhigh)
                                    ((double *)dst)[newindexrightup+ii*(dimfast)]   += (dvalue*dhorzfracright*dvertfracup)/((double)(vertprev-vertcur));
                            }
                        } else if (vertprev+1 < vertcur) {
                            for (ii=0; ii < vertcur-vertprev; ii++) {
                                if (newindex>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindex-ii*(dimfast)]          += dvalue*dhorzfracmid*dvertfracmid/((double)(vertcur-vertprev));
                                if (newindexleftdown>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexleftdown-ii*(dimfast)]  += (dvalue*dhorzfracleft*dvertfracdown)/((double)(vertcur-vertprev));
                                if (newindexleftmid>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexleftmid-ii*(dimfast)]   += (dvalue*dhorzfracleft*dvertfracmid)/((double)(vertcur-vertprev));
                                if (newindexleftup>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexleftup-ii*(dimfast)]    += (dvalue*dhorzfracleft*dvertfracup)/((double)(vertcur-vertprev));
                                if (newindexmiddown>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexmiddown-ii*(dimfast)]   += (dvalue*dhorzfracmid*dvertfracdown)/((double)(vertcur-vertprev));
                                if(newindexmidup>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexmidup-ii*(dimfast)]     += (dvalue*dhorzfracmid*dvertfracup)/((double)(vertcur-vertprev));
                                if(newindexrightdown>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexrightdown-ii*(dimfast)] += (dvalue*dhorzfracright*dvertfracdown)/((double)(vertcur-vertprev));
                                if(newindexrightmid>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexrightmid-ii*(dimfast)]  += (dvalue*dhorzfracright*dvertfracmid)/((double)(vertcur-vertprev));
                                if(newindexrightup>= ii*(dimfast)+newindexlow)
                                    ((double *)dst)[newindexrightup-ii*(dimfast)]   += (dvalue*dhorzfracright*dvertfracup)/((double)(vertcur-vertprev));
                            }
                        } else if (horzprev >  horzcur+1) {
                            for (ii=0; ii < horzprev-horzcur; ii++) {
                                if (newindex+ii<=newindexhigh)
                                    ((double *)dst)[newindex+ii]          += dvalue*dhorzfracmid*dvertfracmid/((double)(horzprev-horzcur));
                                if (newindexleftdown+ii<=newindexhigh)
                                    ((double *)dst)[newindexleftdown+ii]  += (dvalue*dhorzfracleft*dvertfracdown)/((double)(horzprev-horzcur));
                                if (newindexleftmid+ii<=newindexhigh)
                                    ((double *)dst)[newindexleftmid+ii]   += (dvalue*dhorzfracleft*dvertfracmid)/((double)(horzprev-horzcur));
                                if (newindexleftup+ii<=newindexhigh)
                                    ((double *)dst)[newindexleftup+ii]    += (dvalue*dhorzfracleft*dvertfracup)/((double)(horzprev-horzcur));
                                if (newindexmiddown+ii<=newindexhigh)
                                    ((double *)dst)[newindexmiddown+ii]   += (dvalue*dhorzfracmid*dvertfracdown)/((double)(horzprev-horzcur));
                                if (newindexmidup+ii<=newindexhigh)
                                    ((double *)dst)[newindexmidup+ii]     += (dvalue*dhorzfracmid*dvertfracup)/((double)(horzprev-horzcur));
                                if (newindexrightdown+ii<=newindexhigh)
                                    ((double *)dst)[newindexrightdown+ii] += (dvalue*dhorzfracright*dvertfracdown)/((double)(horzprev-horzcur));
                                if (newindexrightmid+ii<=newindexhigh)
                                    ((double *)dst)[newindexrightmid+ii]  += (dvalue*dhorzfracright*dvertfracmid)/((double)(horzprev-horzcur));
                                if (newindexrightup+ii<=newindexhigh)
                                    ((double *)dst)[newindexrightup+ii]   += (dvalue*dhorzfracright*dvertfracup)/((double)(horzprev-horzcur));
                            }
                        } else if (horzprev+1 < horzcur) {
                            for (ii=0; ii < horzcur-horzprev; ii++) {
                                if (newindex>= ii+ newindexlow)
                                    ((double *)dst)[newindex-ii]          += dvalue*dhorzfracmid*dvertfracmid/((double)(horzcur-horzprev));
                                if (newindexleftdown>= ii+ newindexlow)
                                    ((double *)dst)[newindexleftdown-ii]  += (dvalue*dhorzfracleft*dvertfracdown)/((double)(horzcur-horzprev));
                                if (newindexleftmid>= ii+ newindexlow)
                                    ((double *)dst)[newindexleftmid-ii]   += (dvalue*dhorzfracleft*dvertfracmid)/((double)(horzcur-horzprev));
                                if (newindexleftup>= ii+ newindexlow)
                                    ((double *)dst)[newindexleftup-ii]    += (dvalue*dhorzfracleft*dvertfracup)/((double)(horzcur-horzprev));
                                if (newindexmiddown>= ii+ newindexlow)
                                    ((double *)dst)[newindexmiddown-ii]   += (dvalue*dhorzfracmid*dvertfracdown)/((double)(horzcur-horzprev));
                                if (newindexmidup>= ii+ newindexlow)
                                    ((double *)dst)[newindexmidup-ii]     += (dvalue*dhorzfracmid*dvertfracup)/((double)(horzcur-horzprev));
                                if (newindexrightdown>= ii+ newindexlow)
                                    ((double *)dst)[newindexrightdown-ii] += (dvalue*dhorzfracright*dvertfracdown)/((double)(horzcur-horzprev));
                                if (newindexrightmid>= ii+ newindexlow)
                                    ((double *)dst)[newindexrightmid-ii]  += (dvalue*dhorzfracright*dvertfracmid)/((double)(horzcur-horzprev));
                                if (newindexrightup>= ii+ newindexlow)
                                    ((double *)dst)[newindexrightup-ii]   += (dvalue*dhorzfracright*dvertfracup)/((double)(horzcur-horzprev));
                            }
                        }
                    } else if (sizeof(float)==elsize) {
                        fvalue=((float *)src)[index];
                        if (vertprev <= vertcur+1 && vertprev+1 >= vertcur && horzprev <= horzcur+1 && horzprev+1 >= horzcur) {
                            ((float *)dst)[newindex] += (fvalue*dhorzfracmid*dvertfracmid);
                            ((float *)dst)[newindexleftdown]  += (fvalue*dhorzfracleft*dvertfracdown);
                            ((float *)dst)[newindexleftmid]   += (fvalue*dhorzfracleft*dvertfracmid);
                            ((float *)dst)[newindexleftup]    += (fvalue*dhorzfracleft*dvertfracup);
                            ((float *)dst)[newindexmiddown]   += (fvalue*dhorzfracmid*dvertfracdown);
                            ((float *)dst)[newindexmidup]     += (fvalue*dhorzfracmid*dvertfracup);
                            ((float *)dst)[newindexrightdown] += (fvalue*dhorzfracright*dvertfracdown);
                            ((float *)dst)[newindexrightmid]  += (fvalue*dhorzfracright*dvertfracmid);
                            ((float *)dst)[newindexrightup]   += (fvalue*dhorzfracright*dvertfracup);
                        } else if (vertprev > vertcur+1) {
                            for (ii=0; ii < vertprev-vertcur; ii++) {
                                if(newindex+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindex+ii*(dimfast)]          += fvalue*dhorzfracmid*dvertfracmid/((float)(vertprev-vertcur));
                                if(newindexleftdown+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexleftdown+ii*(dimfast)]  += (fvalue*dhorzfracleft*dvertfracdown)/((float)(vertprev-vertcur));
                                if(newindexleftmid+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexleftmid+ii*(dimfast)]   += (fvalue*dhorzfracleft*dvertfracmid)/((float)(vertprev-vertcur));
                                if(newindexleftup+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexleftup+ii*(dimfast)]    += (fvalue*dhorzfracleft*dvertfracup)/((float)(vertprev-vertcur));
                                if(newindexmiddown+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexmiddown+ii*(dimfast)]   += (fvalue*dhorzfracmid*dvertfracdown)/((float)(vertprev-vertcur));
                                if(newindexmidup+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexmidup+ii*(dimfast)]     += (fvalue*dhorzfracmid*dvertfracup)/((float)(vertprev-vertcur));
                                if(newindexrightdown+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexrightdown+ii*(dimfast)] += (fvalue*dhorzfracright*dvertfracdown)/((float)(vertprev-vertcur));
                                if(newindexrightmid+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexrightmid+ii*(dimfast)]  += (fvalue*dhorzfracright*dvertfracmid)/((float)(vertprev-vertcur));
                                if(newindexrightup+ii*(dimfast)<=newindexhigh)
                                    ((float *)dst)[newindexrightup+ii*(dimfast)]   += (fvalue*dhorzfracright*dvertfracup)/((float)(vertprev-vertcur));
                            }
                        } else if (vertprev+1 < vertcur) {
                            for (ii=0; ii < vertcur-vertprev; ii++) {
                                if(newindex>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindex-ii*(dimfast)]          += fvalue*dhorzfracmid*dvertfracmid/((float)(vertcur-vertprev));
                                if(newindexleftdown>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexleftdown-ii*(dimfast)]  += (fvalue*dhorzfracleft*dvertfracdown)/((float)(vertcur-vertprev));
                                if(newindexleftmid>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexleftmid-ii*(dimfast)]   += (fvalue*dhorzfracleft*dvertfracmid)/((float)(vertcur-vertprev));
                                if(newindexleftup>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexleftup-ii*(dimfast)]    += (fvalue*dhorzfracleft*dvertfracup)/((float)(vertcur-vertprev));
                                if(newindexmiddown>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexmiddown-ii*(dimfast)]   += (fvalue*dhorzfracmid*dvertfracdown)/((float)(vertcur-vertprev));
                                if(newindexmidup>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexmidup-ii*(dimfast)]     += (fvalue*dhorzfracmid*dvertfracup)/((float)(vertcur-vertprev));
                                if(newindexrightdown>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexrightdown-ii*(dimfast)] += (fvalue*dhorzfracright*dvertfracdown)/((float)(vertcur-vertprev));
                                if(newindexrightmid>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexrightmid-ii*(dimfast)]  += (fvalue*dhorzfracright*dvertfracmid)/((float)(vertcur-vertprev));
                                if(newindexrightup>=ii*(dimfast)+newindexlow)
                                    ((float *)dst)[newindexrightup-ii*(dimfast)]   += (fvalue*dhorzfracright*dvertfracup)/((float)(vertcur-vertprev));
                            }
                        } else if (horzprev >  horzcur+1) {
                            for (ii=0; ii < horzprev-horzcur; ii++) {
                                if (newindex+ii <= newindexhigh)
                                    ((float *)dst)[newindex+ii]          += fvalue*dhorzfracmid*dhorzfracmid/((float)(horzprev-horzcur));
                                if (newindexleftdown+ii <= newindexhigh)
                                    ((float *)dst)[newindexleftdown+ii]  += (fvalue*dhorzfracleft*dvertfracdown)/((float)(horzprev-horzcur));
                                if (newindexleftmid+ii <= newindexhigh)
                                    ((float *)dst)[newindexleftmid+ii]   += (fvalue*dhorzfracleft*dvertfracmid)/((float)(horzprev-horzcur));
                                if (newindexleftup+ii <= newindexhigh)
                                    ((float *)dst)[newindexleftup+ii]    += (fvalue*dhorzfracleft*dvertfracup)/((float)(horzprev-horzcur));
                                if (newindexmiddown+ii <= newindexhigh)
                                    ((float *)dst)[newindexmiddown+ii]   += (fvalue*dhorzfracmid*dvertfracdown)/((float)(horzprev-horzcur));
                                if (newindexmidup+ii <= newindexhigh)
                                    ((float *)dst)[newindexmidup+ii]     += (fvalue*dhorzfracmid*dvertfracup)/((float)(horzprev-horzcur));
                                if (newindexrightdown+ii <= newindexhigh)
                                    ((float *)dst)[newindexrightdown+ii] += (fvalue*dhorzfracright*dvertfracdown)/((float)(horzprev-horzcur));
                                if (newindexrightmid+ii <= newindexhigh)
                                    ((float *)dst)[newindexrightmid+ii]  += (fvalue*dhorzfracright*dvertfracmid)/((float)(horzprev-horzcur));
                                if (newindexrightup+ii <= newindexhigh)
                                    ((float *)dst)[newindexrightup+ii]   += (fvalue*dhorzfracright*dvertfracup)/((float)(horzprev-horzcur));
                            }
                        } else if (horzprev+1 < horzcur) {
                            for (ii=0; ii < horzcur-horzprev; ii++) {
                                if (newindex>= ii+ newindexlow)
                                    ((float *)dst)[newindex-ii]          += fvalue*dhorzfracmid*dhorzfracmid/((float)(horzcur-horzprev));
                                if (newindexleftdown>= ii+ newindexlow)
                                    ((float *)dst)[newindexleftdown-ii]  += (fvalue*dhorzfracleft*dvertfracdown)/((float)(horzcur-horzprev));
                                if (newindexleftmid>= ii+ newindexlow)
                                    ((float *)dst)[newindexleftmid-ii]   += (fvalue*dhorzfracleft*dvertfracmid)/((float)(horzcur-horzprev));
                                if (newindexleftup>= ii+ newindexlow)
                                    ((float *)dst)[newindexleftup-ii]    += (fvalue*dhorzfracleft*dvertfracup)/((float)(horzcur-horzprev));
                                if (newindexmiddown>= ii+ newindexlow)
                                    ((float *)dst)[newindexmiddown-ii]   += (fvalue*dhorzfracmid*dvertfracdown)/((float)(horzcur-horzprev));
                                if (newindexmidup>= ii+ newindexlow)
                                    ((float *)dst)[newindexmidup-ii]     += (fvalue*dhorzfracmid*dvertfracup)/((float)(horzcur-horzprev));
                                if (newindexrightdown>= ii+ newindexlow)
                                    ((float *)dst)[newindexrightdown-ii] += (fvalue*dhorzfracright*dvertfracdown)/((float)(horzcur-horzprev));
                                if (newindexrightmid>= ii+ newindexlow)
                                    ((float *)dst)[newindexrightmid-ii]  += (fvalue*dhorzfracright*dvertfracmid)/((float)(horzcur-horzprev));
                                if (newindexrightup>= ii+ newindexlow)
                                    ((float *)dst)[newindexrightup-ii]   += (fvalue*dhorzfracright*dvertfracup)/((float)(horzcur-horzprev));
                            }
                        }
                    } else return CBF_ARGUMENT;
                    
                } else {
                    
                    if (elsigned && sizeof(char) == elsize ) {
                        cvalue=((char *)src)[index];
                        dvalue=(double)cvalue;
                        ((char *)dst)[newindexleftdown]  += (newvalueleftdown=(char)(dvalue*dhorzfracleft*dvertfracdown));
                        ((char *)dst)[newindexleftmid]   += (newvalueleftmid=(char)(dvalue*dhorzfracleft*dvertfracmid));
                        ((char *)dst)[newindexleftup]    += (newvalueleftup=(char)(dvalue*dhorzfracleft*dvertfracup));
                        ((char *)dst)[newindexmiddown]   += (newvaluemiddown=(char)(dvalue*dhorzfracmid*dvertfracdown));
                        ((char *)dst)[newindexmidup]     += (newvaluemidup=(char)(dvalue*dhorzfracmid*dvertfracup));
                        ((char *)dst)[newindexrightdown] += (newvaluerightdown=(char)(dvalue*dhorzfracright*dvertfracdown));
                        ((char *)dst)[newindexrightmid]  += (newvaluerightmid=(char)(dvalue*dhorzfracright*dvertfracmid));
                        ((char *)dst)[newindexrightup]   += (newvaluerightup=(char)(dvalue*dhorzfracright*dvertfracup));
                        ((char *)dst)[newindex] += (cvalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                    newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if ((!elsigned) && sizeof(char) == elsize ) {
                        ucvalue=((unsigned char *)src)[index];
                        dvalue=(double)ucvalue;
                        ((unsigned char *)dst)[newindexleftdown]  += (newvalueleftdown=(char)(dvalue*dhorzfracleft*dvertfracdown));
                        ((unsigned char *)dst)[newindexleftmid]   += (newvalueleftmid=(char)(dvalue*dhorzfracleft*dvertfracmid));
                        ((unsigned char *)dst)[newindexleftup]    += (newvalueleftup=(char)(dvalue*dhorzfracleft*dvertfracup));
                        ((unsigned char *)dst)[newindexmiddown]   += (newvaluemiddown=(char)(dvalue*dhorzfracmid*dvertfracdown));
                        ((unsigned char *)dst)[newindexmidup]     += (newvaluemidup=(char)(dvalue*dhorzfracmid*dvertfracup));
                        ((unsigned char *)dst)[newindexrightdown] += (newvaluerightdown=(char)(dvalue*dhorzfracright*dvertfracdown));
                        ((unsigned char *)dst)[newindexrightmid]  += (newvaluerightmid=(char)(dvalue*dhorzfracright*dvertfracmid));
                        ((unsigned char *)dst)[newindexrightup]   += (newvaluerightup=(char)(dvalue*dhorzfracright*dvertfracup));
                        ((unsigned char *)dst)[newindex] += (ucvalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                             newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if (elsigned && sizeof(short) == elsize ) {
                        svalue=((short *)src)[index];
                        dvalue=(double)svalue;
                        ((short *)dst)[newindexleftdown]  += (newvalueleftdown=(short)(dvalue*dhorzfracleft*dvertfracdown));
                        ((short *)dst)[newindexleftmid]   += (newvalueleftmid=(short)(dvalue*dhorzfracleft*dvertfracmid));
                        ((short *)dst)[newindexleftup]    += (newvalueleftup=(short)(dvalue*dhorzfracleft*dvertfracup));
                        ((short *)dst)[newindexmiddown]   += (newvaluemiddown=(short)(dvalue*dhorzfracmid*dvertfracdown));
                        ((short *)dst)[newindexmidup]     += (newvaluemidup=(short)(dvalue*dhorzfracmid*dvertfracup));
                        ((short *)dst)[newindexrightdown] += (newvaluerightdown=(short)(dvalue*dhorzfracright*dvertfracdown));
                        ((short *)dst)[newindexrightmid]  += (newvaluerightmid=(short)(dvalue*dhorzfracright*dvertfracmid));
                        ((short *)dst)[newindexrightup]   += (newvaluerightup=(short)(dvalue*dhorzfracright*dvertfracup));
                        ((short *)dst)[newindex] += (svalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                     newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if ((!elsigned) && sizeof(short) == elsize ) {
                        usvalue=((unsigned short *)src)[index];
                        dvalue=(double)usvalue;
                        ((unsigned short *)dst)[newindexleftdown]  += (newvalueleftdown=(unsigned short)(dvalue*dhorzfracleft*dvertfracdown));
                        ((unsigned short *)dst)[newindexleftmid]   += (newvalueleftmid=(unsigned short)(dvalue*dhorzfracleft*dvertfracmid));
                        ((unsigned short *)dst)[newindexleftup]    += (newvalueleftup=(unsigned short)(dvalue*dhorzfracleft*dvertfracup));
                        ((unsigned short *)dst)[newindexmiddown]   += (newvaluemiddown=(unsigned short)(dvalue*dhorzfracmid*dvertfracdown));
                        ((unsigned short *)dst)[newindexmidup]     += (newvaluemidup=(unsigned short)(dvalue*dhorzfracmid*dvertfracup));
                        ((unsigned short *)dst)[newindexrightdown] += (newvaluerightdown=(unsigned short)(dvalue*dhorzfracright*dvertfracdown));
                        ((unsigned short *)dst)[newindexrightmid]  += (newvaluerightmid=(unsigned short)(dvalue*dhorzfracright*dvertfracmid));
                        ((unsigned short *)dst)[newindexrightup]   += (newvaluerightup=(unsigned short)(dvalue*dhorzfracright*dvertfracup));
                        ((unsigned short *)dst)[newindex] += (usvalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                              newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if (elsigned && sizeof(int) == elsize ) {
                        ivalue=((int *)src)[index];
                        dvalue=(double)ivalue;
                        ((int *)dst)[newindexleftdown]  += (newvalueleftdown=(int)(dvalue*dhorzfracleft*dvertfracdown));
                        ((int *)dst)[newindexleftmid]   += (newvalueleftmid=(int)(dvalue*dhorzfracleft*dvertfracmid));
                        ((int *)dst)[newindexleftup]    += (newvalueleftup=(int)(dvalue*dhorzfracleft*dvertfracup));
                        ((int *)dst)[newindexmiddown]   += (newvaluemiddown=(int)(dvalue*dhorzfracmid*dvertfracdown));
                        ((int *)dst)[newindexmidup]     += (newvaluemidup=(int)(dvalue*dhorzfracmid*dvertfracup));
                        ((int *)dst)[newindexrightdown] += (newvaluerightdown=(int)(dvalue*dhorzfracright*dvertfracdown));
                        ((int *)dst)[newindexrightmid]  += (newvaluerightmid=(int)(dvalue*dhorzfracright*dvertfracmid));
                        ((int *)dst)[newindexrightup]   += (newvaluerightup=(int)(dvalue*dhorzfracright*dvertfracup));
                        ((int *)dst)[newindex] += (ivalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                   newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if ((!elsigned) && sizeof(int) == elsize ) {
                        uivalue=((unsigned int *)src)[index];
                        dvalue=(double)uivalue;
                        ((unsigned int *)dst)[newindexleftdown]  += (newvalueleftdown=(unsigned int)(dvalue*dhorzfracleft*dvertfracdown));
                        ((unsigned int *)dst)[newindexleftmid]   += (newvalueleftmid=(unsigned int)(dvalue*dhorzfracleft*dvertfracmid));
                        ((unsigned int *)dst)[newindexleftup]    += (newvalueleftup=(unsigned int)(dvalue*dhorzfracleft*dvertfracup));
                        ((unsigned int *)dst)[newindexmiddown]   += (newvaluemiddown=(unsigned int)(dvalue*dhorzfracmid*dvertfracdown));
                        ((unsigned int *)dst)[newindexmidup]     += (newvaluemidup=(unsigned int)(dvalue*dhorzfracmid*dvertfracup));
                        ((unsigned int *)dst)[newindexrightdown] += (newvaluerightdown=(unsigned int)(dvalue*dhorzfracright*dvertfracdown));
                        ((unsigned int *)dst)[newindexrightmid]  += (newvaluerightmid=(unsigned int)(dvalue*dhorzfracright*dvertfracmid));
                        ((unsigned int *)dst)[newindexrightup]   += (newvaluerightup=(unsigned int)(dvalue*dhorzfracright*dvertfracup));
                        ((unsigned int *)dst)[newindex] += (uivalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                            newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if (elsigned && sizeof(long) == elsize ) {
                        lvalue=((long *)src)[index];
                        dvalue=(double)lvalue;
                        ((long *)dst)[newindexleftdown]  += (newvalueleftdown=(long)(dvalue*dhorzfracleft*dvertfracdown));
                        ((long *)dst)[newindexleftmid]   += (newvalueleftmid=(long)(dvalue*dhorzfracleft*dvertfracmid));
                        ((long *)dst)[newindexleftup]    += (newvalueleftup=(long)(dvalue*dhorzfracleft*dvertfracup));
                        ((long *)dst)[newindexmiddown]   += (newvaluemiddown=(long)(dvalue*dhorzfracmid*dvertfracdown));
                        ((long *)dst)[newindexmidup]     += (newvaluemidup=(long)(dvalue*dhorzfracmid*dvertfracup));
                        ((long *)dst)[newindexrightdown] += (newvaluerightdown=(long)(dvalue*dhorzfracright*dvertfracdown));
                        ((long *)dst)[newindexrightmid]  += (newvaluerightmid=(long)(dvalue*dhorzfracright*dvertfracmid));
                        ((long *)dst)[newindexrightup]   += (newvaluerightup=(long)(dvalue*dhorzfracright*dvertfracup));
                        ((long *)dst)[newindex] += (lvalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                    newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else if ((!elsigned) && sizeof(unsigned long) == elsize ) {
                        ulvalue=((unsigned long *)src)[index];
                        dvalue=(double)ulvalue;
                        ((unsigned long *)dst)[newindexleftdown]  += (newvalueleftdown=(unsigned long)(dvalue*dhorzfracleft*dvertfracdown));
                        ((unsigned long *)dst)[newindexleftmid]   += (newvalueleftmid=(unsigned long)(dvalue*dhorzfracleft*dvertfracmid));
                        ((unsigned long *)dst)[newindexleftup]    += (newvalueleftup=(unsigned long)(dvalue*dhorzfracleft*dvertfracup));
                        ((unsigned long *)dst)[newindexmiddown]   += (newvaluemiddown=(unsigned long)(dvalue*dhorzfracmid*dvertfracdown));
                        ((unsigned long *)dst)[newindexmidup]     += (newvaluemidup=(unsigned long)(dvalue*dhorzfracmid*dvertfracup));
                        ((unsigned long *)dst)[newindexrightdown] += (newvaluerightdown=(unsigned long)(dvalue*dhorzfracright*dvertfracdown));
                        ((unsigned long *)dst)[newindexrightmid]  += (newvaluerightmid=(unsigned long)(dvalue*dhorzfracright*dvertfracmid));
                        ((unsigned long *)dst)[newindexrightup]   += (newvaluerightup=(unsigned long)(dvalue*dhorzfracright*dvertfracup));
                        ((unsigned long *)dst)[newindex] += (ulvalue-newvalueleftdown-newvalueleftmid-newvalueleftup-newvaluemiddown-
                                                             newvaluemidup-newvaluerightdown-newvaluerightmid-newvaluerightup);
                    } else return CBF_ARGUMENT;
                }
            }
        }
    }
    
    return CBF_SUCCESS;
}


#ifdef __cplusplus

}

#endif



