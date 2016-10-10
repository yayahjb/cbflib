/**********************************************************************
 * cbff -- cbflib C rouitnes for fortran access                       *
 *                                                                    *
 * Version 0.8.1 1 March 2009                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2009 Herbert J. Bernstein                            *
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

#ifndef CBFF_H
#define CBFF_H

#ifdef __cplusplus

extern "C" {
    
#endif
    
#include <stdio.h>
#include "cbf.h"
#include "cbf_simple.h"
    
    /* Return the bit pattern of a FILE * pointer as a size_t opaque
     handle */
    
    size_t cbff_file(FILE * file);
    
    /* Return the FILE * pointer for a size_t opaque
     handle */
    
    FILE * cbff_file_handle(const size_t cbffFile);
    
    /* Return the bit pattern of a cbf_handle as a size_t opaque
     handle */
    
    size_t cbff_handle(cbf_handle cbfHandle);
    
    /* Return the cbf_handle for a size_t opaque
     handle */
    
    cbf_handle cbff_cbf_handle(size_t CBFFhandle);
    
    /* Return the bit pattern of a goniometer as a size_t opaque
     handle */
    
    size_t cbff_goniometer_handle(cbf_goniometer cbfGoniometer);
    
    /* Return the goniometer handle for a size_t opaque
     handle */
    
    cbf_goniometer cbff_cbf_goniometer(size_t CBFFgoniometer);
    
    /* Return the bit pattern of a detector as a size_t opaque
     handle */
    
    size_t cbff_detector_handle(cbf_detector cbfDetector);
    
    /* Return the detector handle for a size_t opaque
     handle */
    
    cbf_detector cbff_cbf_detector_handle(size_t CBFFdetector);
    
    
    /* Return the bit pattern of a node handle as a size_t opaque
     handle */
    
    size_t cbff_cbf_node(cbf_node * cbfNode);
    
    /* Return the bit pattern of a node handle as a size_t opaque
     handle */
    
    size_t cbff_node_handle(cbf_node * cbfNode);
    
    /* Return the node handle for a size_t opaque
     handle */
    
    cbf_node * cbff_cbf_node_handle(size_t cbffNode);
    
    CBF_NODETYPE cbff_cbf_nodetype(char * str) ;
    
    int cbff_nodetype(CBF_NODETYPE nodetype, 
                      char * nodetypestring, 
                      int start_nodetypestring,
                      int end_nodetypestring,
                      int * status_nodetypestring);
    
    /* Return a size_t opaque handle from an fopen */
    
    size_t cbff_fopen(const char * filename, const char * mode);
    
    int cbff_fclose(const size_t cbffFile);
    
    /* Create a handle */
    
    int cbff_make_handle(size_t * CBFFhandle);
    
    
    /* Free a handle */
    
    int cbff_free_handle(
                         size_t CBFFhandle);
    
    
    
    /* Read a file */
    
    int cbff_read_file(
                       size_t CBFFhandle,
        size_t CBFFstream,
                       int flags);
    
    
    
    
    /* Read a wide file */
    
    int cbff_read_widefile(
                           size_t CBFFhandle,
        size_t CBFFstream,
                           int flags);
    
    
    
    /* Read a pre-read buffered file */
    
    int cbff_read_buffered_file(
                                size_t CBFFhandle,
        size_t CBFFstream,
                                int flags,
                                const char * buffer,
                                size_t buffer_len);
    
    
    
    
    /* Write a file */
    
    int cbff_write_file(
                        size_t CBFFhandle,
        size_t CBFFstream,
                        int isbuffer,
                        int ciforcbf,
                        int headers,
                        int encoding);
    
    
    /* Write a file, starting at the local node */
    
    int cbff_write_local_file(
                              size_t CBFFhandle,
        size_t CBFFstream,
                              int isbuffer,
                              int ciforcbf,
                              int headers,
                              int encoding);
    
    
    /* Write a wide file */
    
    int cbff_write_widefile(
                            size_t CBFFhandle,
        size_t CBFFstream,
                            int isbuffer,
                            int ciforcbf,
                            int headers,
                            int encoding);
    
    
    
    /* Add a data block */
    
    int cbff_new_datablock(
                           size_t CBFFhandle,
                           const char * datablockname);
    
    
    
    /* Add a save frame block */
    
    int cbff_new_saveframe(
                           size_t CBFFhandle,
                           const char * saveframename);
    
    
    
    /* Add a data block, allowing for duplicates */
    
    int cbff_force_new_datablock(
                                 size_t CBFFhandle,
                                 const char * datablockname);
    
    
    
    /* Add a save frame, allowing for duplicates */
    
    int cbff_force_new_saveframe(
                                 size_t CBFFhandle,
                                 const char * saveframename);
    
    
    
    /* Add a category to the current data block */
    
    int cbff_new_category(
                          size_t CBFFhandle,
                          const char * categoryname);
    
    
    
    /* Add a category to the current data block, allowing for duplicates */
    
    int cbff_force_new_category(
                                size_t CBFFhandle,
                                const char * categoryname);
    
    
    
    /* Add a column to the current category */
    
    int cbff_new_column(
                        size_t CBFFhandle,
                        const char * columnname);
    
    
    
    /* Add a row to the current category */
    
    int cbff_new_row(
                     size_t CBFFhandle);
    
    
    
    /* Insert a row in the current category */
    
    int cbff_insert_row(
                        size_t CBFFhandle,
                        const int rownumber);
    
    
    
    /* Delete a row from the current category */
    
    int cbff_delete_row(
                        size_t CBFFhandle,
                        const int rownumber);
    
    
    
    /* Change the name of the current data block */
    
    int cbff_set_datablockname(
                               size_t CBFFhandle,
                               const char * datablockname);
    
    
    
    /* Change the name of the current save frame */
    
    int cbff_set_saveframename(
                               size_t CBFFhandle,
                               const char * saveframename);
    
    
    
    /* Delete all categories from all the data blocks */
    
    int cbff_reset_datablocks(
                              size_t CBFFhandle);
    
    
    
    /* Delete all categories from the current data block */
    
    int cbff_reset_datablock(
                             size_t CBFFhandle);
    
    
    
    /* Delete all categories from the current save frame */
    
    int cbff_reset_saveframe(
                             size_t CBFFhandle);
    
    
    
    /* Delete all columns and rows from the current category */
    
    int cbff_reset_category(
                            size_t CBFFhandle);
    
    
    
    /* Delete the current data block */
    
    int cbff_remove_datablock(
                              size_t CBFFhandle);
    
    
    
    /* Delete the current save frame  */
    
    int cbff_remove_saveframe(
                              size_t CBFFhandle);
    
    
    
    /* Delete the current category */
    
    int cbff_remove_category(
                             size_t CBFFhandle);
    
    
    
    /* Delete the current column */
    
    int cbff_remove_column(
                           size_t CBFFhandle);
    
    
    
    /* Delete the current row */
    
    int cbff_remove_row(
                        size_t CBFFhandle);
    
    
    
    /* Make the first data block the current data block */
    
    int cbff_rewind_datablock(
                              size_t CBFFhandle);
    
    
    
    /* Make the first category in the current data block the current category */
    
    int cbff_rewind_category(
                             size_t CBFFhandle);
    
    
    
    /* Make the first save frame in the current data block the current category */
    
    int cbff_rewind_saveframe(
                              size_t CBFFhandle);
    
    
    
    /* Make the first category or save frame in the current data block the current category */
    
    int cbff_rewind_blockitem(
                              size_t CBFFhandle,
                              char * copy_type, size_t start_type, size_t end_type, int * status_type);
    
    
    
    /* Make the first column in the current category the current column */
    
    int cbff_rewind_column(
                           size_t CBFFhandle);
    
    
    
    /* Make the first row in the current category the current row */
    
    int cbff_rewind_row(
                        size_t CBFFhandle);
    
    
    
    /* Make the next data block the current data block */
    
    int cbff_next_datablock(
                            size_t CBFFhandle);
    
    
    /* Make the next save frame in the current data block the current save frame */
    
    int cbff_next_saveframe(
                            size_t CBFFhandle);
    
    
    
    /* Make the next category in the current data block the current category */
    
    int cbff_next_category(
                           size_t CBFFhandle);
    
    
    
    /* Make the next save frame or category the current data block or category */
    
    int cbff_next_blockitem(
                            size_t CBFFhandle,
                            char * copy_type, size_t start_type, size_t end_type, int * status_type);    
    
    
    
    /* Make the next column in the current category the current column */
    
    int cbff_next_column(
                         size_t CBFFhandle);
    
    
    
    /* Make the next row in the current category the current row */
    
    int cbff_next_row(
                      size_t CBFFhandle);
    
    
    
    /* Make the named data block the current data block */
    
    int cbff_find_datablock(
                            size_t CBFFhandle,
                            const char * datablockname);
    
    
    
    /* Make the named save frame in the current data block the current save frame */
    
    int cbff_find_saveframe(
                            size_t CBFFhandle,
                            const char * saveframe);
    
    
    
    /* Make the named category in the current data block or save frame the current category */
    
    int cbff_find_category(
                           size_t CBFFhandle,
                           const char * categoryname);
    
    
    
    /* Make the named column in the current category the current column */
    
    int cbff_find_column(
                         size_t CBFFhandle,
                         const char * columnname);
    
    
    
    /* Make the first row with matching value the current row */
    
    int cbff_find_row(
                      size_t CBFFhandle,
                      const char * value);
    
    
    /* Make the first row with matching value the current row
     creating it if necessary */
    
    int cbff_require_row(
                         size_t CBFFhandle,
                         const char * value);
    
    
    /* Make the next row with matching value the current row */
    
    int cbff_find_nextrow(
                          size_t CBFFhandle,
                          const char * value);
    
    
    /* Make the next row with matching value the current row,
     creating the row if necessary */
    
    int cbff_require_nextrow(
                             size_t CBFFhandle,
                             const char * value);
    
    
    /* Count the data blocks */
    
    int cbff_count_datablocks(
                              size_t CBFFhandle,
                              unsigned int * datablocks);
    
    
    /* Count the save frames in the current data block */
    
    int cbff_count_saveframes(
                              size_t CBFFhandle,
                              unsigned int * saveframes);
    
    
    /* Count the categories in the current data block */
    
    int cbff_count_categories(
                              size_t CBFFhandle,
                              unsigned int * categories);
    
    
    
    /* Count the items in the current data block */
    
    int cbff_count_blockitems(
                              size_t CBFFhandle,
                              unsigned int * blockitems);
    
    
    
    /* Count the columns in the current category */
    
    int cbff_count_columns(
                           size_t CBFFhandle,
                           unsigned int * columns);
    
    
    
    /* Count the rows in the current category */
    
    int cbff_count_rows(
                        size_t CBFFhandle,
                        unsigned int * rows);
    
    
    
    /* Make the specified data block the current data block */
    
    int cbff_select_datablock(
                              size_t CBFFhandle,
                              unsigned int datablock);
    
    
    
    /* Make the specified save frame the current save frame */
    
    int cbff_select_saveframe(
                              size_t CBFFhandle,
                              unsigned int saveframe);
    
    
    
    /* Make the specified category the current category */
    
    int cbff_select_category(
                             size_t CBFFhandle,
                             unsigned int category);
    
    
    
    /* Make the specified category or save frame the current block item */
    
    int cbff_select_blockitem(
                              size_t CBFFhandle,
                              unsigned int item,
                              char * copy_type, size_t start_type, size_t end_type, int * status_type);
    
    
    
    /* Make the specified column the current column */
    
    int cbff_select_column(
                           size_t CBFFhandle,
                           unsigned int column);
    
    
    
    /* Make the specified row the current row */
    
    int cbff_select_row(
                        size_t CBFFhandle,
                        unsigned int row);
    
    
    
    /* Get the name of the current data block */
    
    int cbff_datablock_name(
                            size_t CBFFhandle,
                            char * copy_datablockname, size_t start_datablockname, size_t end_datablockname, int * status_datablockname);    
    
    /* Get the name of the current save frame */
    
    int cbff_saveframe_name(
                            size_t CBFFhandle,
                            char * copy_saveframename, size_t start_saveframename, size_t end_saveframename, int * status_saveframename);
    
    
    /* Get the name of the current category */
    
    int cbff_category_name(
                           size_t CBFFhandle,
                           char * copy_categoryname, size_t start_categoryname, size_t end_categoryname, int * status_categoryname);
    
    
    
    /* Get the name of the current column */
    
    int cbff_column_name(
                         size_t CBFFhandle,
                         char * copy_columnname, size_t start_columnname, size_t end_columnname, int * status_columnname);
    
    
    
    /* Get the number of the current row */
    
    int cbff_row_number(
                        size_t CBFFhandle,
                        unsigned int * row);
    
    
    
    /* Get the number of the current column */
    
    int cbff_column_number(
                           size_t CBFFhandle,
                           unsigned int * column);
    
    
    
    /* Get the number of the current block item */
    
    int cbff_blockitem_number(
                              size_t CBFFhandle,
                              unsigned int * blockitem);
    
    
    
    /* Get the ascii value of the current (row, column) entry */
    
    int cbff_get_value(
                       size_t CBFFhandle,
                       char * copy_value, size_t start_value, size_t end_value, int * status_value);

    
    
    /* Set the ascii value of the current (row, column) entry */
    
    int cbff_set_value(
                       size_t CBFFhandle,
                       const char * value);
    
    
    /* Get the ascii value of the current (row, column) entry,
     setting it to a default value if necessary */
    
    int cbff_require_value(
                           size_t CBFFhandle,
                           char * copy_value, size_t start_value, size_t end_value, int * status_value,
                           const char * defaultvalue);
    
    
    
    
    /* Get the ascii type of value of the current (row, column) entry */
    
    int cbff_get_typeofvalue(
                             size_t CBFFhandle,
                             char * copy_typeofvalue, size_t start_typeofvalue, size_t end_typeofvalue, int * status_typeofvalue);
    
    
    
    /* Set the ascii type of value of the current (row, column) entry */
    
    int cbff_set_typeofvalue(
                             size_t CBFFhandle,
                             const char * typeofvalue);
    
    
    
    /* Get the (int) numeric value of the current (row, column) entry */
    
    int cbff_get_integervalue(
                              size_t CBFFhandle,
                              int * number);
    
    
    
    /* Get the (double) numeric value of the current (row, column) entry */
    
    int cbff_get_doublevalue(
                             size_t CBFFhandle,
                             double * number);
    
    
    
    /* Set the ascii value of the current (row, column) entry from an int */
    
    int cbff_set_integervalue(
                              size_t CBFFhandle,
                              int number);
    
    
    
    /* Set the ascii value of the current (row, column) entry from a double */
    
    int cbff_set_doublevalue(
                             size_t CBFFhandle,
                             const char * format,
                             double number);
    
    
    
    /* Get the (integer) numeric value of the current (row, column) entry, setting it if necessary */
    
    int cbff_require_integervalue(
                                  size_t CBFFhandle,
                                  int * number,
                                  int defaultvalue);
    
    
    
    /* Get the (double) numeric value of the current (row, column) entry, setting it if necessary */
    
    int cbff_require_doublevalue(
                                 size_t CBFFhandle,
                                 double * number,
                                 double defaultvalue);
    
    
    
    /* Get the parameters of the current (row, column) array entry */
    
    int cbff_get_arrayparameters(
                                 size_t CBFFhandle,
                                 unsigned int * compression,
                                 int * id,
                                 size_t * elsize,
                                 int * elsigned,
                                 int * elunsigned,
                                 size_t * nelem,
                                 int * minelem,
                                 int * maxelem,
                                 int * realarray);
    
    
    /* Get the parameters of the current (row, column) array entry */
    
    int cbff_get_arrayparameters_wdims(
                                       size_t CBFFhandle,
                                       unsigned int * compression,
                                       int * id,
                                       size_t * elsize,
                                       int * elsigned,
                                       int * elunsigned,
                                       size_t * nelem,
                                       int * minelem,
                                       int * maxelem,
                                       int * realarray,
                                       char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                       size_t * dimfast,
                                       size_t * dimmid,
                                       size_t * dimslow,
                                       size_t * padding);

    int cbff_get_arrayparameters_wdims_fs(
                                       size_t CBFFhandle,
                                       unsigned int * compression,
                                       int * id,
                                       size_t * elsize,
                                       int * elsigned,
                                       int * elunsigned,
                                       size_t * nelem,
                                       int * minelem,
                                       int * maxelem,
                                       int * realarray,
                                       char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                       size_t * dimfast,
                                       size_t * dimmid,
                                       size_t * dimslow,
                                       size_t * padding);

    int cbff_get_arrayparameters_wdims_sf(
                                       size_t CBFFhandle,
                                       unsigned int * compression,
                                       int * id,
                                       size_t * elsize,
                                       int * elsigned,
                                       int * elunsigned,
                                       size_t * nelem,
                                       int * minelem,
                                       int * maxelem,
                                       int * realarray,
                                       char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                       size_t * dimslow,
                                       size_t * dimmid,
                                       size_t * dimfast,
                                       size_t * padding);
    
    /* Get the dimensions of the current (row, column) array entry
     from the CBF tags */
    
    
    int cbff_get_arraydimensions(size_t CBFFhandle,
                                 size_t * dimover,
                                 size_t * dimfast,
                                 size_t * dimmid,
                                 size_t * dimslow);
    
    /* Get the parameters of the current (row, column) integer array entry */
    
    int cbff_get_integerarrayparameters(
                                        size_t CBFFhandle,
                                        unsigned int * compression,
                                        int * id,
                                        size_t * elsize,
                                        int * elsigned,
                                        int * elunsigned,
                                        size_t * nelem,
                                        int * minelem,
                                        int * maxelem);
    
    
    /* Get the parameters of the current (row, column) integer array entry */
    
    int cbff_get_integerarrayparameters_wdims(
                                              size_t CBFFhandle,
                                              unsigned int * compression,
                                              int * id,
                                              size_t * elsize,
                                              int * elsigned,
                                              int * elunsigned,
                                              size_t * nelem,
                                              int * minelem,
                                              int * maxelem,
                                              char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                              size_t * dimfast,
                                              size_t * dimmid,
                                              size_t * dimslow,
                                              size_t * padding);
    
    int cbff_get_integerarrayparameters_wdims_fs(
                                              size_t CBFFhandle,
                                              unsigned int * compression,
                                              int * id,
                                              size_t * elsize,
                                              int * elsigned,
                                              int * elunsigned,
                                              size_t * nelem,
                                              int * minelem,
                                              int * maxelem,
                                              char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                              size_t * dimfast,
                                              size_t * dimmid,
                                              size_t * dimslow,
                                              size_t * padding);
    
    int cbff_get_integerarrayparameters_wdims_sf(
                                              size_t CBFFhandle,
                                              unsigned int * compression,
                                              int * id,
                                              size_t * elsize,
                                              int * elsigned,
                                              int * elunsigned,
                                              size_t * nelem,
                                              int * minelem,
                                              int * maxelem,
                                              char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                              size_t * dimslow,
                                              size_t * dimmid,
                                              size_t * dimfast,
                                              size_t * padding);
        
    
    /* Get the integer value of the current (row, column) array entry */
    
    int cbff_get_integerarray(
                              size_t CBFFhandle,
                              int * id,
                              void * value,
                              size_t elsize,
                              int elsign,
                              size_t nelem,
                              size_t * nelem_read);
    
    
    /* Get the real value of the current (row, column) array entry */
    
    int cbff_get_realarray(
                           size_t CBFFhandle,
                           int * id,
                           void * value,
                           size_t elsize,
                           size_t nelem,
                           size_t * nelem_read);
    
    
    /* Get the parameters of the current (row, column) array entry */
    
    int cbff_get_realarrayparameters(
                                     size_t CBFFhandle,
                                     unsigned int * compression,
                                     int * id,
                                     size_t * elsize,
                                     size_t * nelem);
    
    
    /* Get the parameters of the current (row, column) array entry */
    
    int cbff_get_realarrayparameters_wdims(
                                           size_t CBFFhandle,
                                           unsigned int * compression,
                                           int * id,
                                           size_t * elsize,
                                           size_t * nelem,
                                           char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                           size_t * dimfast,
                                           size_t * dimmid,
                                           size_t * dimslow,
                                           size_t * padding);

    int cbff_get_realarrayparameters_wdims_fs(
                                           size_t CBFFhandle,
                                           unsigned int * compression,
                                           int * id,
                                           size_t * elsize,
                                           size_t * nelem,
                                           char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                           size_t * dimfast,
                                           size_t * dimmid,
                                           size_t * dimslow,
                                           size_t * padding);

    int cbff_get_realarrayparameters_wdims_sf(
                                           size_t CBFFhandle,
                                           unsigned int * compression,
                                           int * id,
                                           size_t * elsize,
                                           size_t * nelem,
                                           char * copy_byteorder, size_t start_byteorder, size_t end_byteorder, int * status_byteorder,
                                           size_t * dimslow,
                                           size_t * dimmid,
                                           size_t * dimfast,
                                           size_t * padding);
    

    /* Set the integer value of the current (row, column) array entry */
    
    int cbff_set_integerarray(
                              size_t CBFFhandle,
                              unsigned int compression,
                              int id,
                              void * value,
                              size_t elsize,
                              int elsign,
                              size_t nelem);
    
    
    /* Set the integer value of the current (row, column) array entry */
    
    int cbff_set_integerarray_wdims(
                                    size_t CBFFhandle,
                                    unsigned int compression,
                                    int id,
                                    void * value,
                                    size_t elsize,
                                    int elsign,
                                    size_t nelem,
                                    const char * byteorder,
                                    size_t dimfast,
                                    size_t dimmid,
                                    size_t dimslow,
                                    size_t padding);

    
    int cbff_set_integerarray_wdims_fs(
                                    size_t CBFFhandle,
                                    unsigned int compression,
                                    int id,
                                    void * value,
                                    size_t elsize,
                                    int elsign,
                                    size_t nelem,
                                    const char * byteorder,
                                    size_t dimfast,
                                    size_t dimmid,
                                    size_t dimslow,
                                    size_t padding);
    int cbff_set_integerarray_wdims_sf(
                                    size_t CBFFhandle,
                                    unsigned int compression,
                                    int id,
                                    void * value,
                                    size_t elsize,
                                    int elsign,
                                    size_t nelem,
                                    const char * byteorder,
                                    size_t dimslow,
                                    size_t dimmid,
                                    size_t dimfast,
                                    size_t padding);
    
    

    /* Set the real value of the current (row, column) array entry */
    
    int cbff_set_realarray(
                           size_t CBFFhandle,
                           unsigned int compression,
                           int id,
                           void * value,
                           size_t elsize,
                           size_t nelem);
    
    
    /* Set the real value of the current (row, column) array entry
     with dimensions */
    
    int cbff_set_realarray_wdims(
                                 size_t CBFFhandle,
                                 unsigned int compression,
                                 int id,
                                 void * value,
                                 size_t elsize,
                                 size_t nelem,
                                 const char * byteorder,
                                 size_t dimfast,
                                 size_t dimmid,
                                 size_t dimslow,
                                 size_t padding);

    int cbff_set_realarray_wdims_fs(
                                 size_t CBFFhandle,
                                 unsigned int compression,
                                 int id,
                                 void * value,
                                 size_t elsize,
                                 size_t nelem,
                                 const char * byteorder,
                                 size_t dimfast,
                                 size_t dimmid,
                                 size_t dimslow,
                                 size_t padding);

    int cbff_set_realarray_wdims_sf(
                                 size_t CBFFhandle,
                                 unsigned int compression,
                                 int id,
                                 void * value,
                                 size_t elsize,
                                 size_t nelem,
                                 const char * byteorder,
                                 size_t dimslow,
                                 size_t dimmid,
                                 size_t dimfast,
                                 size_t padding);

     
    /* Issue a warning message */
    
    void cbff_warning (const char *message);
    
    
    
    /* Issue an error message */
    
    void cbff_error (const char *message);
    
    
    
    /* issue a log message for a cbf */
    
    void cbff_log (size_t CBFFhandle, const char *message, int logflags);
    

    
    /* Find a datablock, creating it if necessary */
    
    int cbff_require_datablock(
                               size_t CBFFhandle,
                               const char * datablockname);
    
    
    /* Find a category, creating it if necessary */
    
    int cbff_require_category(
                              size_t CBFFhandle,
                              const char * categoryname);
    
    
    /* Find a column, creating it if necessary */
    
    int cbff_require_column(
                            size_t CBFFhandle,
                            const char * columnname);
    
    
    
    /* Find a column value, return a default if necessary */
    
    int cbff_require_column_value(
                                  size_t CBFFhandle,
                                  const char * columnname,
                                  char * copy_value, size_t start_value, size_t end_value, int * status_value,
                                  const char * defaultvalue);
    
    
    /* Find a column integer value, return a default if necessary */
    
    int cbff_require_column_integervalue(
                                         size_t CBFFhandle,
                                         const char * columnname,
                                         int * number,
                                         const int defaultvalue);
    
    
    /* Find a column double value, return a default if necessary */
    
    int cbff_require_column_doublevalue(
                                        size_t CBFFhandle,
                                        const char * columnname,
                                        double * number,
                                        const double defaultvalue);
    
    
    /* Get the local byte order of the default integer type */
    
    int cbff_get_local_integer_byte_order(
        char * copy_byte_order, size_t start_byte_order, size_t end_byte_order, int * status_byte_order);
    
    
    /* Get the local byte order of the default real type */
    
    int cbff_get_local_real_byte_order(
        char * copy_byte_order, size_t start_byte_order, size_t end_byte_order, int * status_byte_order);
    
    
    /* Get the local real format */
    
    int cbff_get_local_real_format(
        char * copy_real_format, size_t start_real_format, size_t end_real_format, int * status_real_format);
    
    
    /* Get the dictionary for a cbf */
    
    int cbff_get_dictionary(
                            size_t CBFFhandle,
                            size_t * CBFFdictionary);
    
    
    /* Set the dictionary for a cbf */
    
    int cbff_set_dictionary(
                            size_t CBFFhandle,
                            size_t CBFFdictionary);
    
    
    /* Get the dictionary for a cbf, or create one */
    
    int cbff_require_dictionary(
                                size_t CBFFhandle,
                                size_t * CBFFdictionary);
    
    
    /* Put the value into the named column, updating the hash table links */
    
    int cbff_set_hashedvalue(
                             size_t CBFFhandle,
                             const char * value,
                             const char * columnname,
                             int valuerow);
    
    
    /* Find value in the named column, using the hash table links */
    
    int cbff_find_hashedvalue(
                              size_t CBFFhandle,
                              const char * value,
                              const char * columnname,
                              int caseinsensitive);
    
    
    
    /* Take a defintion from a dictionary and insert it into the
     has tables of a cbf dictionary */
    
    int cbff_convert_dictionary_definition(
                                           size_t CBFFcbfdictionary,
                                           size_t CBFFdictionary,
                                           const char * name);
    
    
    
    /* Increment a column */
    
    int cbff_increment_column(
                              size_t CBFFhandle,
                              const char* columnname,
                              int * count);
    
    
    /* Reset a column */
    
    int cbff_reset_column(
                          size_t CBFFhandle,
                          const char* columnname);
    
    
    
    /* Reset reference counts for a dictionary */
    
    int cbff_reset_refcounts(
                             size_t CBFFdictionar);
    
    
    
    /* Convert a DDL1 or DDL2 dictionary and add it to a CBF dictionary */
    
    int cbff_convert_dictionary(
                                size_t CBFFhandle,
                                size_t CBFFdictionary);
    
    
    
    /* Find the requested tag anywhere in the cbf, make it the current column */
    
    int cbff_find_tag(
                      size_t CBFFhandle,
                      const char * tag);
    
    
    /* Find the requested tag in the cbf within the current
     
     save frame or data block, make it the current column */
    
    int cbff_find_local_tag(
                            size_t CBFFhandle,
                            const char * tag);
    
    
    /* Find the requested category and column anywhere in the cbf, make it the current column */
    
    int cbff_srch_tag(
                      size_t CBFFhandle,
                      size_t CBFFnode,
                      const char * categoryname,
                      const char * columnname);
    
    
    /* Find the root alias of a given category */
    
    int cbff_find_category_root(
                                size_t CBFFhandle,
                                const char* categoryname,
        char * copy_categoryroot, size_t start_categoryroot, size_t end_categoryroot, int * status_categoryroot);
    
    
    /* Find the root alias of a given category, defaulting to the current one */
    
    int cbff_require_category_root(
                                   size_t CBFFhandle,
                                   const char* categoryname,
        char * copy_categoryroot, size_t start_categoryroot, size_t end_categoryroot, int * status_categoryroot);
    
    
    /* Set the root alias of a given category */
    
    int cbff_set_category_root(
                               size_t CBFFhandle,
                               const char* categoryname,
                               const char* categoryroot);
    
    
    /* Find the root alias of a given tag */
    
    int cbff_find_tag_root(
                           size_t CBFFhandle,
                           const char* tagname,
        char * copy_tagroot, size_t start_tagroot, size_t end_tagroot, int * status_tagroot);
    
    
    /* Find the root alias of a given tag, defaulting to the current one */
    
    int cbff_require_tag_root(
                              size_t CBFFhandle,
                              const char* tagname,
        char * copy_tagroot, size_t start_tagroot, size_t end_tagroot, int * status_tagroot);
    
    
    /* Set the root alias of a given tag */
    
    int cbff_set_tag_root(
                          size_t CBFFhandle,
                          const char* tagname,
                          const char* tagroot);
    
    
    /* Find the category of a given tag */
    
    int cbff_find_tag_category(
                               size_t CBFFhandle,
                               const char* tagname,
        char * copy_categoryname, size_t start_categoryname, size_t end_categoryname, int * status_categoryname);
    
    /* Set category of a given tag */
    
    int cbff_set_tag_category(
                              size_t CBFFhandle,
                              const char* tagname,
                              const char* categoryname);
    
    
    /* Validate portion of CBF */
    
    int cbff_validate(
                      size_t CBFFhandle,
                      size_t CBFFnode,
                      char * CBFFtype,
                      size_t CBFFcatnode);
    
    
    /* Load accumulator */
    
    int cbff_mpint_load_acc(
                            unsigned int * acc,
                            size_t acsize,
                            void * source,
                            size_t elsize,
                            int elsign,
                            const char * border);
    
    
    
    /* Store accumulator */
    
    int cbff_mpint_store_acc(
                             unsigned int * acc,
                             size_t acsize,
                             void * dest,
                             size_t elsize,
                             int elsign,
                             const char * border);
    
    
    /* Clear accumulator */
    
    int cbff_mpint_clear_acc(
                             unsigned int * acc,
                             size_t acsize);
    
    
    /* Increment accumulator */
    
    int cbff_mpint_increment_acc(
                                 unsigned int * acc,
                                 size_t acsize);
    
    
    /* Decrement accumulator */
    
    int cbff_mpint_decrement_acc(
                                 unsigned int * acc,
                                 size_t acsize);
    
    
    /* Negate accumulator */
    
    int cbff_mpint_negate_acc(
                              unsigned int * acc,
                              size_t acsize);
    
    
    /* Add to accumulator */
    
    int cbff_mpint_add_acc(
                           unsigned int * acc,
                           size_t acsize,
                           unsigned int * add,
                           size_t addsize);
    
    
    /* Shift accumulator right */
    
    int cbff_mpint_rightshift_acc(
                                  unsigned int * acc,
                                  size_t acsize,
                                  int shift);
    
    
    /* Shift accumulator left */
    
    int cbff_mpint_leftshift_acc(
                                 unsigned int * acc,
                                 size_t acsize,
                                 int shift);
    
    
    
    /* Check value of type validity */
    
    int cbff_check_type_contents(
                                 const char * type,
                                 const char * value);
    
    
    /* Regex Match function */
    
    int cbff_match(
                   const char * string,
                   char * pattern);
    

    
    /* Read a template file */
    
    int cbff_read_template(
                           size_t CBFFhandle,
                           size_t CBFFstream);
    
    
    
    /* Get the diffrn.id entry */
    
    int cbff_get_diffrn_id(
                           size_t CBFFhandle,
                           char * copy_diffrn_id, size_t start_diffrn_id, size_t end_diffrn_id, int * status_diffrn_id);
    
    
    
    /* Change the diffrn.id entry in all the categories */
    
    int cbff_set_diffrn_id(
                           size_t CBFFhandle,
                           const char * diffrn_id);
    
    
    
    /* Change the diffrn.id entry, creating it if necessary */
    
    int cbff_require_diffrn_id(
                               size_t CBFFhandle,
                               char * copy_diffrn_id, size_t start_diffrn_id, size_t end_diffrn_id, int * status_diffrn_id,
                               const char * default_id);
    
    
    
    /* Get the diffrn.crystal_id entry */
    
    int cbff_get_crystal_id(
                            size_t CBFFhandle,
                            char * copy_crystal_id, size_t start_crystal_id, size_t end_crystal_id, int * status_crystal_id);
    
    
    
    /* Change the diffrn.crystal_id entry */
    
    int cbff_set_crystal_id(
                            size_t CBFFhandle,
                            const char * crystal_id);
    
    
    
    /* Get the wavelength */
    
    int cbff_get_wavelength(
                            size_t CBFFhandle,
                            double * wavelength);
    
    
    
    /* Set the wavelength */
    
    int cbff_set_wavelength(
                            size_t CBFFhandle,
                            double wavelength);
    
    
    
    /* Get the polarization */
    
    int cbff_get_polarization(
                              size_t CBFFhandle,
                              double * polarizn_source_ratio,
                              double * polarizn_source_norm);
    
    
    
    /* Set the polarization */
    
    int cbff_set_polarization(
                              size_t CBFFhandle,
                              double polarizn_source_ratio,
                              double polarizn_source_norm);
    
    
    
    /* Get the divergence */
    
    int cbff_get_divergence(
                            size_t CBFFhandle,
                            double * div_x_source,
                            double * div_y_source,
                            double * div_x_y_source);
    
    
    
    /* Set the divergence */
    
    int cbff_set_divergence(
                            size_t CBFFhandle,
                            double div_x_source,
                            double div_y_source,
                            double div_x_y_source);
    
    
    
    /* Get the number of elements */
    
    int cbff_count_elements(
                            size_t CBFFhandle,
                            unsigned int * elements);
    
    
    
    /* Get the element id */
    
    int cbff_get_element_id(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            char * copy_element_id, size_t start_element_id, size_t end_element_id, int * status_element_id);
    
    
    
    /* Get the detector id */
    
    int cbff_get_detector_id(
                             size_t CBFFhandle,
                             unsigned int element_number,
                             char * copy_detector_id, size_t start_detector_id, size_t end_detector_id, int * status_detector_id);
     
    
    
    /* Get the array id for a given detector element */
    
    int cbff_get_array_id(
                          size_t CBFFhandle,
                          unsigned int element_number,
                          char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id);
    
    
    
    /* Get the pixel size of a detector element in a given direction */
    
    int cbff_get_pixel_size(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            int axis_number,
                            double * psize);
    
    int cbff_get_pixel_size_fs(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double * psize);
    
    int cbff_get_pixel_size_sf(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            int axis_number,
                               double * psize);
    
    
    
    /* Set the pixel size of a detector element in a given direction */
    
    int cbff_set_pixel_size(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            int axis_number,
                            double psize);
    
    int cbff_set_pixel_size_fs(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double psize);
    
    int cbff_set_pixel_size_sf(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double psize);
    
    
    /* Get the gain of a detector element */
    
    int cbff_get_gain(
                      size_t CBFFhandle,
                      unsigned int element_number,
                      double * gain,
                      double * gain_esd);
    
    
    
    /* Set the gain of a detector element */
    
    int cbff_set_gain(
                      size_t CBFFhandle,
                      unsigned int element_number,
                      double gain,
                      double gain_esd);
    
    
    
    /* Get the bin sizes of a detector element */
    
    int cbff_get_bin_sizes(
                           size_t CBFFhandle,
                           unsigned int element_number,
                           double * slowbinsize,
                           double * fastbinsize);
    
    
    /* Set the bin sizes of a detector element */
    
    int cbff_set_bin_sizes(
                           size_t CBFFhandle,
                           unsigned int element_number,
                           double slowbinsize,
                           double fastbinsize);
    
    
    
    /* Get the overload value of a detector element */
    
    int cbff_get_overload(
                          size_t CBFFhandle,
                          unsigned int element_number,
                          double * overload);
    
    
    
    /* Set the overload value of a detector element */
    
    int cbff_set_overload(
                          size_t CBFFhandle,
                          unsigned int element_number,
                          double overload);
    
    
    
    /* Get the integration time */
    
    int cbff_get_integration_time(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  double * time);
    
    
    
    /* Set the integration time */
    
    int cbff_set_integration_time(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  double time);
    
    
    
    /* Convert gregorian to julian date (in days) */
    
    double cbff_gregorian_julian (int    year,
                                 int    month,
                                 int    day,
                                 int    hour,
                                 int    minute,
                                 double second);
    
    
    /* Get the collection date and time (1) as seconds since January 1 1970 */
    
    int cbff_get_timestamp(
                           size_t CBFFhandle,
                           unsigned int reserved,
                           double * time,
                           int * timezone);
    
    
    
    /* Get the collection date and time (2) as individual fields */
    
    int cbff_get_datestamp(
                           size_t CBFFhandle,
                           unsigned int reserved,
                           int * year,
                           int * month,
                           int * day,
                           int * hour,
                           int * minute,
                           double * second,
                           int * timezone);
    
    
    
    /* Set the collection date and time (1) as seconds since January 1 1970 */
    
    int cbff_set_timestamp(
                           size_t CBFFhandle,
                           unsigned int reserved,
                           double time,
                           int timezone,
                           double precision);
    
    
    
    /* Set the collection date and time (2) as individual fields */
    
    int cbff_set_datestamp(
                           size_t CBFFhandle,
                           unsigned int reserved,
                           int year,
                           int month,
                           int day,
                           int hour,
                           int minute,
                           double second,
                           int timezone,
                           double precision);
    
    
    
    /* Set the collection date and time (3) as current time to the second */
    
    int cbff_set_current_timestamp(
                                   size_t CBFFhandle,
                                   unsigned int reserved,
                                   int timezone);
    
    
    
    /* Get the image size */
    
    int cbff_get_image_size(
                            size_t CBFFhandle,
                            unsigned int reserved,
                            unsigned int element_number,
                            size_t * ndimslow,
                            size_t * ndimfast);
    
    int cbff_get_image_size_fs(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               size_t * ndimfast,
                               size_t * ndimslow);
    
    int cbff_get_image_size_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               size_t * ndimslow,
                               size_t * ndimfast);
    
    
    /* Read a binary section into an image.  ndimslow is the 
     slow dimension, ndimfast is fast dimension.*/
    
    int cbff_get_image(
                       size_t CBFFhandle,
                       unsigned int reserved,
                       unsigned int element_number,
                       void * array,
                       size_t elsize,
                       int elsign,
                       size_t ndimslow,
                       size_t ndimfast);
    
    int cbff_get_image_fs(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimfast,
                          size_t ndimslow);
    
    int cbff_get_image_sf(
                       size_t CBFFhandle,
                       unsigned int reserved,
                       unsigned int element_number,
                       void * array,
                       size_t elsize,
                       int elsign,
                       size_t ndimslow,
                          size_t ndimfast);
    
    
    /* Read a binary section into a real image.  ndimslow is the 
     slow dimension, ndimfast is fast dimension.*/
    
    int cbff_get_real_image(
                            size_t CBFFhandle,
                            unsigned int reserved,
                            unsigned int element_number,
                            void * array,
                            size_t elsize,
                            size_t ndimslow,
                            size_t ndimfast);
    
    int cbff_get_real_image_fs(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               void * array,
                               size_t elsize,
                               size_t ndimfast,
                               size_t ndimslow);
    
    int cbff_get_real_image_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               void * array,
                               size_t elsize,
                               size_t ndimslow,
                               size_t ndimfast);
    
    
    /* Get the 3D image size. ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_3d_image_size(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               size_t * ndimslow,
                               size_t * ndimmid,
                               size_t * ndimfast);
    
    int cbff_get_3d_image_size_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  size_t * ndimfast,
                                  size_t * ndimmid,
                                  size_t * ndimlow);
    
    int cbff_get_3d_image_size_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  size_t * ndimslow,
                                  size_t * ndimmid,
                                  size_t * ndimfast);
    
    
    
    /* Read a 3D binary section into an image.  
     ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_3d_image(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimmid,
                          size_t ndimfast);
    
    int cbff_get_3d_image_fs(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             unsigned int element_number,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimfast,
                             size_t ndimmid,
                             size_t ndimslow);
    
    int cbff_get_3d_image_sf(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             unsigned int element_number,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimslow,
                             size_t ndimmid,
                             size_t ndimfast);
    
    
    /* Read a 3D binary section into a real image.  
     ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_real_3d_image(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               void * array,
                               size_t elsize,
                               size_t ndimslow,
                               size_t ndimmid,
                               size_t ndimfast);
    
    
    int cbff_get_real_3d_image_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimfast,
                                  size_t ndimmid,
                                  size_t ndimslow);
    
    int cbff_get_real_3d_image_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast);
     
    
    /* Save an image.  ndimslow is the slow dimension, ndimfast is fast. */
    
    int cbff_set_image(
                       size_t CBFFhandle,
                       unsigned int reserved,
                       unsigned int element_number,
                       unsigned int compression,
                       void * array,
                       size_t elsize,
                       int elsign,
                       size_t ndimslow,
                       size_t ndimfast);
    
    int cbff_set_image_fs(
                       size_t CBFFhandle,
                       unsigned int reserved,
                       unsigned int element_number,
                       unsigned int compression,
                       void * array,
                       size_t elsize,
                       int elsign,
                          size_t ndimfast,
                          size_t ndimslow);
    
    int cbff_set_image_sf(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          unsigned int compression,
                          void * array,
                          size_t elsize,
                          int elsign,
                       size_t ndimslow,
                          size_t ndimfast);
    
    
    /* Save a real image.  ndimslow is the slow dimension, ndimfast is fast. */
    
    int cbff_set_real_image(
                            size_t CBFFhandle,
                            unsigned int reserved,
                            unsigned int element_number,
                            unsigned int compression,
                            void * array,
                            size_t elsize,
                            size_t ndimslow,
                            size_t ndimfast);
    
    int cbff_set_real_image_fs(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               unsigned int compression,
                               void * array,
                               size_t elsize,
                               size_t ndimfast,
                               size_t ndimslow);
    
    int cbff_set_real_image_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               unsigned int compression,
                               void * array,
                               size_t elsize,
                               size_t ndimslow,
                               size_t ndimfast);
    
    /* Save a 3D image.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension. */
    
    
    int cbff_set_3d_image(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          unsigned int compression,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimmid,
                          size_t ndimfast);
    
    int cbff_set_3d_image_fs(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          unsigned int compression,
                          void * array,
                          size_t elsize,
                          int elsign,
                             size_t ndimfast,
                             size_t ndimmid,
                             size_t ndimslow);
    
    int cbff_set_3d_image_sf(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             unsigned int element_number,
                             unsigned int compression,
                             void * array,
                             size_t elsize,
                             int elsign,
                          size_t ndimslow,
                          size_t ndimmid,
                             size_t ndimfast);
    
    
    /* Save a real 3D image.  
     ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_set_real_3d_image(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               unsigned int compression,
                               void * array,
                               size_t elsize,
                               size_t ndimslow,
                               size_t ndimmid,
                               size_t ndimfast);
    
    int cbff_set_real_3d_image_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  unsigned int compression,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimfast,
                                  size_t ndimmid,
                                  size_t ndimslow);
    
    int cbff_set_real_3d_image_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  unsigned int compression,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast);
    
    
    
    /* Get the array_id for a map segment or map segment mask.
     ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension. */
    
    int cbff_get_map_array_id(
                              size_t CBFFhandle,
                              unsigned int reserved,
                              const char * segment_id,
                              char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id,
                              int ismask,
                              int require,
                              size_t ndimslow,
                              size_t ndimmid,
                              size_t ndimfast);
    
    int cbff_get_map_array_id_fs(
                                 size_t CBFFhandle,
                                 unsigned int reserved,
                                 const char * segment_id,
                                 char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id,
                                 int ismask,
                                 int require,
                                 size_t ndimfast,
                              size_t ndimmid,
                                 size_t ndimslow);
    
    int cbff_get_map_array_id_sf(
                                 size_t CBFFhandle,
                                 unsigned int reserved,
                                 const char * segment_id,
                                 char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id,
                                 int ismask,
                                 int require,
                                 size_t ndimslow,
                                 size_t ndimmid,
                                 size_t ndimfast);
    
    
    
    /* Get the map segment size.   ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_map_segment_size(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * segment_id,
                                  int * binary_id,
                                  size_t * ndimslow,
                                  size_t * ndimmid,
                                  size_t * ndimfast);
    
    
    int cbff_get_map_segment_size_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     size_t * ndimfast,
                                     size_t * ndimmid,
                                     size_t * ndimslow);
    
    
    int cbff_get_map_segment_size_sf(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     size_t * ndimslow,
                                     size_t * ndimmid,
                                     size_t * ndimfast);
    
    
    /* Read a map segment.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    int cbff_get_map_segment(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             const char * segment_id,
                             int * binary_id,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimslow,
                             size_t ndimmid,
                             size_t ndimfast);
    
    int cbff_get_map_segment_fs(
                                size_t CBFFhandle,
                                unsigned int reserved,
                                const char * segment_id,
                                int * binary_id,
                                void * array,
                                size_t elsize,
                                int elsign,
                                size_t ndimfast,
                                size_t ndimmid,
                                size_t ndimslow);
    int cbff_get_map_segment_sf(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             const char * segment_id,
                             int * binary_id,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimslow,
                             size_t ndimmid,
                                size_t ndimfast);
    
    
    
    /* Read a map segment mask.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    int cbff_get_map_segment_mask(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * segment_id,
                                  int * binary_id,
                                  void * array,
                                  size_t elsize,
                                  int elsign,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast);
    
    int cbff_get_map_segment_mask_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     void * array,
                                     size_t elsize,
                                     int elsign,
                                     size_t ndimfast,
                                  size_t ndimmid,
                                     size_t ndimslow);
    
    int cbff_get_map_segment_mask_sf(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     void * array,
                                     size_t elsize,
                                     int elsign,
                                     size_t ndimslow,
                                     size_t ndimmid,
                                     size_t ndimfast);
    
    
    /* Read a real map segment.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_real_map_segment(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * segment_id,
                                  int * binary_id,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast);
    
    int cbff_get_real_map_segment_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     void * array,
                                     size_t elsize,
                                     size_t ndimfast,
                                     size_t ndimmid,
                                     size_t ndimslow);
    
    int cbff_get_real_map_segment_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * segment_id,
                                  int * binary_id,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                     size_t ndimfast);
    
    
    /* Read a real map segment mask.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    int cbff_get_real_map_segment_mask(
                                       size_t CBFFhandle,
                                       unsigned int reserved,
                                       const char * segment_id,
                                       int * binary_id,
                                       void * array,
                                       size_t elsize,
                                       size_t ndimslow,
                                       size_t ndimmid,
                                       size_t ndimfast);
    
    
    int cbff_get_real_map_segment_mask_fs(
                                          size_t CBFFhandle,
                                          unsigned int reserved,
                                          const char * segment_id,
                                          int * binary_id,
                                          void * array,
                                          size_t elsize,
                                          size_t ndimfast,
                                          size_t ndimmid,
                                          size_t ndimslow);
    
    int cbff_get_real_map_segment_mask_sf(
                                          size_t CBFFhandle,
                                          unsigned int reserved,
                                          const char * segment_id,
                                          int * binary_id,
                                          void * array,
                                          size_t elsize,
                                          size_t ndimslow,
                                          size_t ndimmid,
                                          size_t ndimfast);
    
    
    /* Save a map segment.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    
    int cbff_set_map_segment(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             const char * segment_id,
                             int * binary_id,
                             unsigned int compression,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimslow,
                             size_t ndimmid,
                             size_t ndimfast);
    
    
    int cbff_set_map_segment_fs(
                                size_t CBFFhandle,
                                unsigned int reserved,
                                const char * segment_id,
                                int * binary_id,
                                unsigned int compression,
                                void * array,
                                size_t elsize,
                                int elsign,
                                size_t ndimfast,
                             size_t ndimmid,
                                size_t ndimslow);
    
    int cbff_set_map_segment_sf(
                                size_t CBFFhandle,
                                unsigned int reserved,
                                const char * segment_id,
                                int * binary_id,
                                unsigned int compression,
                                void * array,
                                size_t elsize,
                                int elsign,
                                size_t ndimslow,
                                size_t ndimmid,
                                size_t ndimfast);
    
    /* Save a map segment mask.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_set_map_segment_mask(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * segment_id,
                                  int * binary_id,
                                  unsigned int compression,
                                  void * array,
                                  size_t elsize,
                                  int elsign,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast);
    
    int cbff_set_map_segment_mask_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     unsigned int compression,
                                     void * array,
                                     size_t elsize,
                                     int elsign,
                                     size_t ndimfast,
                                     size_t ndimmid,
                                     size_t ndimslow);
    
    int cbff_set_map_segment_mask_sf(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     unsigned int compression,
                                     void * array,
                                     size_t elsize,
                                     int elsign,
                                     size_t ndimslow,
                                     size_t ndimmid,
                                     size_t ndimfast);
    
    /* Save a real map segment.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_set_real_map_segment(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * segment_id,
                                  int * binary_id,
                                  unsigned int compression,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast);
    
    int cbff_set_real_map_segment_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     unsigned int compression,
                                     void * array,
                                     size_t elsize,
                                     size_t ndimfast,
                                     size_t ndimmid,
                                     size_t ndimslow);
    
    int cbff_set_real_map_segment_sf(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     unsigned int compression,
                                     void * array,
                                     size_t elsize,
                                     size_t ndimslow,
                                     size_t ndimmid,
                                     size_t ndimfast);
    
    
    /* Save a real map segment mask.  ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    
    int cbff_set_real_map_segment_mask(
                                       size_t CBFFhandle,
                                       unsigned int reserved,
                                       const char * segment_id,
                                       int * binary_id,
                                       unsigned int compression,
                                       void * array,
                                       size_t elsize,
                                       size_t ndimslow,
                                       size_t ndimmid,
                                       size_t ndimfast);
    
    int cbff_set_real_map_segment_mask_fs(
                                          size_t CBFFhandle,
                                          unsigned int reserved,
                                          const char * segment_id,
                                          int * binary_id,
                                          unsigned int compression,
                                          void * array,
                                          size_t elsize,
                                          size_t ndimfast,
                                          size_t ndimmid,
                                          size_t ndimslow);
    
    int cbff_set_real_map_segment_mask_sf(
                                          size_t CBFFhandle,
                                          unsigned int reserved,
                                          const char * segment_id,
                                          int * binary_id,
                                          unsigned int compression,
                                          void * array,
                                          size_t elsize,
                                          size_t ndimslow,
                                          size_t ndimmid,
                                          size_t ndimfast);
    
    
    /* Get the 3D array size. ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_3d_array_size(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               const char * array_id,
                               size_t * ndimslow,
                               size_t * ndimmid,
                               size_t * ndimfast);
    
    
    int cbff_get_3d_array_size_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * array_id,
                                  size_t * ndimfast,
                                  size_t * ndimmid,
                                  size_t * ndimslow);
    
    
    int cbff_get_3d_array_size_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               const char * array_id,
                               size_t * ndimslow,
                               size_t * ndimmid,
                                  size_t * ndimfast);
    
    
    
    /* Read a 3D array.  
     ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_3d_array(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          const char * array_id,
                          int * binary_id,
                          void * array,
                          int eltype,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimmid,
                          size_t ndimfast);
    
    int cbff_get_3d_array_fs(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             const char * array_id,
                             int * binary_id,
                             void * array,
                             int eltype,
                             size_t elsize,
                             int elsign,
                             size_t ndimfast,
                             size_t ndimmid,
                             size_t ndimslow);
    
    int cbff_get_3d_array_sf(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          const char * array_id,
                          int * binary_id,
                          void * array,
                          int eltype,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimmid,
                             size_t ndimfast);
    
    
    /* Save a 3D array.  
     ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_set_3d_array(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          const char * array_id,
                          int * binary_id,
                          unsigned int compression,
                          void * array,
                          int eltype,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimmid,
                          size_t ndimfast);
    
    int cbff_set_3d_array_fs(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             const char * array_id,
                             int * binary_id,
                             unsigned int compression,
                             void * array,
                             int eltype,
                             size_t elsize,
                             int elsign,
                             size_t ndimfast,
                          size_t ndimmid,
                             size_t ndimslow);
    
    int cbff_set_3d_array_sf(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             const char * array_id,
                             int * binary_id,
                             unsigned int compression,
                             void * array,
                             int eltype,
                             size_t elsize,
                             int elsign,
                             size_t ndimslow,
                             size_t ndimmid,
                             size_t ndimfast);
    
    
    /* Get the setting of an axis */
    
    int cbff_get_axis_setting(
                              size_t CBFFhandle,
                              unsigned int reserved,
                              const char * axis_id,
                              double * start,
                              double * increment);
    
    
    
    /* Get the reference setting of an axis */
    
    int cbff_get_axis_reference_setting(
                                        size_t CBFFhandle,
                                        unsigned int reserved,
                                        const char * axis_id,
                                        double * refsetting);
    
    
    
    /* Change the setting of an axis */
    
    int cbff_set_axis_setting(
                              size_t CBFFhandle,
                              unsigned int reserved,
                              const char * axis_id,
                              double start,
                              double increment);
    
    
    
    /* Change the reference setting of an axis */
    
    int cbff_set_axis_reference_setting(
                                        size_t CBFFhandle,
                                        unsigned int reserved,
                                        const char * axis_id,
                                        double refsetting);
    
    
    
    /* Construct a goniometer */
    
    int cbff_construct_goniometer(
                                  size_t CBFFhandle,
                                  size_t * CBFFgoniometer);
    
    
    
    /* Free a goniometer */
    
    int cbff_free_goniometer(
                             size_t CBFFgoniometer);
    
    
    
    /* Get the rotation axis */
    
    int cbff_get_rotation_axis(
                               size_t CBFFgoniometer,
                               unsigned int reserved,
                               double * vector1,
                               double * vector2,
                               double * vector3);
    
    
    
    /* Get the rotation range */
    
    int cbff_get_rotation_range(
                                size_t CBFFgoniometer,
                                unsigned int reserved,
                                double * start,
                                double * increment);
    
    
    
    /* Reorient a vector */
    
    int cbff_rotate_vector(
                           size_t CBFFgoniometer,
                           unsigned int reserved,
                           double ratio,
                           double initial1,
                           double initial2,
                           double initial3,
                           double * final1,
                           double * final2,
                           double * final3);
    
    
    
    /* Convert a vector to reciprocal space */
    
    int cbff_get_reciprocal(
                            size_t CBFFgoniometer,
                            unsigned int reserved,
                            double ratio,
                            double wavelength,
                            double real1,
                            double real2,
                            double real3,
                            double * reciprocal1,
                            double * reciprocal2,
                            double * reciprocal3);
    
    
    
    /* Construct a detector positioner */
    
    int cbff_construct_detector(
                                size_t CBFFhandle,
                                size_t * CBFFdetector,
                                unsigned int element_number);
    
    
    
    /* Construct a reference detector positioner */
    
    int cbff_construct_reference_detector(
                                          size_t CBFFhandle,
                                          size_t * CBFFdetector,
                                          unsigned int element_number);
    
    
    
    
    /* Construct a detector positioner, 
     creating the necessary categories, and columns */
    
    int cbff_require_detector(
                              size_t CBFFhandle,
                              size_t * CBFFdetector,
                              unsigned int element_number);
    
    
    
    /* Construct a reference detector positioner, 
     creating the necessary categories, and columns */
    
    int cbff_require_reference_detector(
                                        size_t CBFFhandle,
                                        size_t * CBFFdetector,
                                        unsigned int element_number);
    
    
    
    /* Free a detector */
    
    int cbff_free_detector(
                           size_t CBFFdetector);
    
    
    
    /* Get the beam center */
    
    int cbff_get_beam_center(
                             size_t CBFFdetector,
                             double * indexslow,
                             double * indexfast,
                             double * centerslow,
                             double * centerfast);

    int cbff_get_beam_center_fs(
                             size_t CBFFdetector,
                             double * indexfast,
                             double * indexslow,
                             double * centerfast,
                             double * centerslow);

    int cbff_get_beam_center_sf(
                             size_t CBFFdetector,
                             double * indexslow,
                             double * indexfast,
                             double * centerslow,
                             double * centerfast);
    
    
    
    /* Set the beam center */
    
    int cbff_set_beam_center(
                             size_t CBFFdetector,
                             double * indexslow,
                             double * indexfast,
                             double * centerslow,
                             double * centerfast);

    int cbff_set_beam_center_fs(
                             size_t CBFFdetector,
                             double * indexfast,
                             double * indexslow,
                             double * centerfast,
                             double * centerslow);

    int cbff_set_beam_center_sf(
                             size_t CBFFdetector,
                             double * indexslow,
                             double * indexfast,
                             double * centerslow,
                             double * centerfast);
    
    
    /* Set the reference beam center */
    
    int cbff_set_reference_beam_center(
                                       size_t CBFFdetector,
                                       double * indexslow,
                                       double * indexfast,
                                       double * centerslow,
                                       double * centerfast);
    
    
    int cbff_set_reference_beam_center_fs(
                                       size_t CBFFdetector,
                                       double * indexfast,
                                       double * indexslow,
                                       double * centerfast,
                                       double * centerslow);

    int cbff_set_reference_beam_center_sf(
                                       size_t CBFFdetector,
                                       double * indexslow,
                                       double * indexfast,
                                       double * centerslow,
                                       double * centerfast);
    
    
    
    
    /* Get the detector distance */
    
    int cbff_get_detector_distance(
                                   size_t CBFFdetector,
                                   double * distance);
    
    
    
    /* Get the detector normal */
    
    int cbff_get_detector_normal(
                                 size_t CBFFdetector,
                                 double * normal1,
                                 double * normal2,
                                 double * normal3);
    
    
    
    /* Calcluate the coordinates of a pixel */
    
    int cbff_get_pixel_coordinates(
                                   size_t CBFFdetector,
                                   double indexslow,
                                   double indexfast,
                                   double * coordinate1,
                                   double * coordinate2,
                                   double * coordinate3);

    int cbff_get_pixel_coordinates_fs(
                                   size_t CBFFdetector,
                                   double indexfast,
                                   double indexslow,
                                   double * coordinate1,
                                   double * coordinate2,
                                   double * coordinate3);

    int cbff_get_pixel_coordinates_sf(
                                   size_t CBFFdetector,
                                   double indexslow,
                                   double indexfast,
                                   double * coordinate1,
                                   double * coordinate2,
                                   double * coordinate3);
    
    
    
    
    /* Get the pixel normal */
    
    int cbff_get_pixel_normal(
                              size_t CBFFdetector,
                              double indexslow,
                              double indexfast,
                              double * normal1,
                              double * normal2,
                              double * normal3);

    int cbff_get_pixel_normal_fs(
                              size_t CBFFdetector,
                              double indexfast,
                              double indexslow,
                              double * normal1,
                              double * normal2,
                              double * normal3);

    int cbff_get_pixel_normal_sf(
                              size_t CBFFdetector,
                              double indexslow,
                              double indexfast,
                              double * normal1,
                              double * normal2,
                              double * normal3);
    
    
    
    /* Calcluate the area of a pixel */
    
    int cbff_get_pixel_area(
                            size_t CBFFdetector,
                            double indexslow,
                            double indexfast,
                            double * area,
                            double * projected_area);

    int cbff_get_pixel_area_fs(
                            size_t CBFFdetector,
                            double indexfast,
                            double indexslow,
                            double * area,
                            double * projected_area);

    int cbff_get_pixel_area_sf(
                            size_t CBFFdetector,
                            double indexslow,
                            double indexfast,
                            double * area,
                            double * projected_area);
    
    
    
    /* Calcluate the size of a pixel from the detector element axis displacements */
    
    int cbff_get_inferred_pixel_size(
                                     size_t CBFFdetector,
                                     int axis_number,
                                     double * psize);

    int cbff_get_inferred_pixel_size_fs(
                                     size_t CBFFdetector,
                                     int axis_number,
                                     double * psize);

    int cbff_get_inferred_pixel_size_sf(
                                     size_t CBFFdetector,
                                     int axis_number,
                                     double * psize);
    
    
    
    /* Get the unit cell parameters */
    
    int cbff_get_unit_cell(
                           size_t CBFFhandle,
                           double cell[6],
                           double cell_esd[6]);
    
    
    /* Set the unit cell parameters */
    
    int cbff_set_unit_cell(
                           size_t CBFFhandle,
                           double cell[6],
                           double cell_esd[6]);
    
    
    /* Get the reciprocal cell parameters */
    
    int cbff_get_reciprocal_cell(
                                 size_t CBFFhandle,
                                 double cell[6],
                                 double cell_esd[6]);
    
    
    /* Set the reciprocal cell parameters */
    
    int cbff_set_reciprocal_cell(
                                 size_t CBFFhandle,
                                 double cell[6],
                                 double cell_esd[6]);
    
    
    /* Compute a cell volume */
    
    int cbff_compute_cell_volume(
                                 double cell[6],
                                 double * volume);
    
    
    /* Compute a reciprocal cell */
    
    int cbff_compute_reciprocal_cell(
                                     double cell[6],
                                     double rcell[6]);
    
    
    /* Get the orientation matrix entry */
    
    int cbff_get_orientation_matrix(
                                    size_t CBFFhandle,
                                    double ub_matrix[9]);
    
    
    /* Set the orientation matrix entry */
    
    int cbff_set_orientation_matrix(
                                    size_t CBFFhandle,
                                    double ub_matrix[9]);
    
    
    
#ifdef __cplusplus
    
}

#endif
#endif
