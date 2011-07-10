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



#ifdef __cplusplus

extern "C" {
    
#endif
    
#include <stdio.h>
#include <string.h>
#include "cbf.h"
#include "cbf_simple.h"
#include "cbff.h"
    
    /* Return the bit pattern of a FILE * pointer as a size_t opaque
     handle */
    
    size_t cbff_file(FILE * file) {
        return (size_t)file;
    }
    
    /* Return the FILE * pointer for a size_t opaque
     handle */
    
    FILE * cbff_file_handle(const size_t cbffFile) {
        return (FILE *)cbffFile;
    }
    
    /* Return the bit pattern of a cbf_handle as a size_t opaque
     handle */
    
    size_t cbff_handle(cbf_handle cbfHandle) {
        return (size_t)cbfHandle;
    }
    
    /* Return the cbf_handle for a size_t opaque
     handle */
    
    cbf_handle cbff_cbf_handle(size_t CBFFhandle) {
        return (cbf_handle)CBFFhandle;
    }
    
    /* Return the bit pattern of a goniometer as a size_t opaque
     handle */
    
    size_t cbff_goniometer_handle(cbf_goniometer cbfGoniometer) {
        return (size_t)cbfGoniometer;
    }
    
    /* Return the goniometer handle for a size_t opaque
     handle */
    
    cbf_goniometer cbff_cbf_goniometer(size_t CBFFgoniometerhandle) {
        return (cbf_goniometer)CBFFgoniometerhandle;
    }
    
    /* Return the bit pattern of a detector as a size_t opaque
     handle */
    
    size_t cbff_detector_handle(cbf_detector cbfDetector) {
        return (size_t)cbfDetector;
    }
    
    /* Return the detector handle for a size_t opaque
     handle */
    
    cbf_detector cbff_cbf_detector_handle(size_t CBFFdetector) {
        return (cbf_detector)CBFFdetector;
    }
    
    
    /* Return the bit pattern of a node handle as a size_t opaque
     handle */
    
    size_t cbff_cbf_node(cbf_node * cbfNode) {
        return (size_t)cbfNode;
    }
    
    /* Return the node handle for a size_t opaque
     handle */
    
    cbf_node * cbff_cbf_node_handle(size_t cbffNode) {
        return (cbf_node *)cbffNode;
    }
    
    CBF_NODETYPE cbff_cbf_nodetype(char * str) {
        CBF_NODETYPE nodetype;
        if (!strcasecmp(str,"CBF_UNDEFNODE")) {
            nodetype = CBF_UNDEFNODE;
        } else if (!strcasecmp(str,"CBF_LINK")) {
            nodetype = CBF_LINK;
        } else if (!strcasecmp(str,"CBF_ROOT")) {
            nodetype = CBF_ROOT;
        } else if (!strcasecmp(str,"CBF_DATABLOCK")) {
            nodetype = CBF_DATABLOCK;
        } else if (!strcasecmp(str,"CBF_SAVEFRAME")) {
            nodetype = CBF_SAVEFRAME;
        } else if (!strcasecmp(str,"CBF_CATEGORY")) {
            nodetype = CBF_CATEGORY;
        } else if (!strcasecmp(str,"CBF_COLUMN")) {
            nodetype = CBF_COLUMN;
        } else if (!strcasecmp(str,"CBF_VALUE")) {
            nodetype = CBF_VALUE;
        } else nodetype = CBF_UNDEFNODE;
        return nodetype;
    }
    
    int cbff_nodetype(CBF_NODETYPE nodetype, 
                      char * nodetypestring, 
                      int start_nodetypestring,
                      int end_nodetypestring,
                      int * status_nodetypestring) {
        char rstring[14];
        size_t length;
        size_t index;
        switch(nodetype) {
            case  CBF_UNDEFNODE:        /* Undefined */
                strcpy(rstring,"CBF_UNDEFNODE"); break;
            case  CBF_LINK:             /* Link      */
                strcpy(rstring,"CBF_LINK"); break;
            case  CBF_ROOT:             /* Root      */
                strcpy(rstring,"CBF_ROOT"); break;
            case  CBF_DATABLOCK:        /* Datablock */
                strcpy(rstring,"CBF_DATABLOCK"); break;
            case  CBF_SAVEFRAME:        /* Saveframe */
                strcpy(rstring,"CBF_SAVEFRAME"); break;
            case  CBF_CATEGORY:         /* Category  */
                strcpy(rstring,"CBF_CATEGORY"); break;
            case  CBF_COLUMN:           /* Column    */
                strcpy(rstring,"CBF_COLUMN"); break;
            case  CBF_VALUE:            /* Value     */  /* Not a visible node type */
                strcpy(rstring,"CBF_UNDEFNODE"); break;
            default:
                strcpy(rstring,"CBF_UNDEFNODE"); break;                
        }
        length = strlen(rstring);
        for (index = 0; index < length-start_nodetypestring+1
             && index < end_nodetypestring-start_nodetypestring+1; index++) {
            nodetypestring[index] = rstring[index+start_nodetypestring-1];
        }
        if (index < end_nodetypestring-start_nodetypestring+1) {
            for (; index < end_nodetypestring-start_nodetypestring+1; index++) {
                nodetypestring[index] = ' ';
            }
            *status_nodetypestring = 0; /* transfer complete */
        } else {
            if (length > end_nodetypestring) {
                *status_nodetypestring = 1; /* more to transfer */
            } else {
                *status_nodetypestring = 0; /* transfer complete */
            }
        }
        return 0;
    }
    
    /* Return a size_t opaque handle from an fopen */
    
    size_t cbff_fopen(const char * filename, const char * mode) {
        return cbff_file(fopen(filename,mode));
    }
    
    int cbff_fclose(const size_t cbffFile) {
        return fclose(cbff_file_handle(cbffFile));
    }
    
    /* Create a handle */
    
    int cbff_make_handle(size_t * CBFFhandle) {
        int errorcode;
        cbf_handle handle;
        if (!CBFFhandle) return CBF_ARGUMENT;
        errorcode = cbf_make_handle(&handle);
        *CBFFhandle = cbff_handle(handle);
        return errorcode;
    }
    
    /* Free a handle */
    
    int cbff_free_handle(
                         size_t CBFFhandle){
        return cbf_free_handle(
                               cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Read a file */
    
    int cbff_read_file(
                       size_t CBFFhandle,
                       size_t CBFFstream,
                       int flags){
        return cbf_read_file(
                             cbff_cbf_handle(CBFFhandle),
                             cbff_file_handle(CBFFstream),
                             flags);
    }
    
    
    
    
    /* Read a wide file */
    
    int cbff_read_widefile(
                           size_t CBFFhandle,
                           size_t CBFFstream,
                           int flags){
        return cbf_read_widefile(
                                 cbff_cbf_handle(CBFFhandle),
                                 cbff_file_handle(CBFFstream),
                                 flags);
    }
    
    
    
    /* Read a pre-read buffered file */
    
    int cbff_read_buffered_file(
                                size_t CBFFhandle,
                                size_t CBFFstream,
                                int flags,
                                const char * buffer,
                                size_t buffer_len){
        return cbf_read_buffered_file(
                                      cbff_cbf_handle(CBFFhandle),
                                      cbff_file_handle(CBFFstream),
                                      flags,
                                      buffer,
                                      buffer_len);
    }
    
    
    
    
    /* Write a file */
    
    int cbff_write_file(
                        size_t CBFFhandle,
                        size_t CBFFstream,
                        int isbuffer,
                        int ciforcbf,
                        int headers,
                        int encoding){
        return cbf_write_file(
                              cbff_cbf_handle(CBFFhandle),
                              cbff_file_handle(CBFFstream),
                              isbuffer,
                              ciforcbf,
                              headers,
                              encoding);
    }
    
    
    /* Write a file, starting at the local node */
    
    int cbff_write_local_file(
                              size_t CBFFhandle,
                              size_t CBFFstream,
                              int isbuffer,
                              int ciforcbf,
                              int headers,
                              int encoding){
        return cbf_write_local_file(
                                    cbff_cbf_handle(CBFFhandle),
                                    cbff_file_handle(CBFFstream),
                                    isbuffer,
                                    ciforcbf,
                                    headers,
                                    encoding);
    }
    
    
    /* Write a wide file */
    
    int cbff_write_widefile(
                            size_t CBFFhandle,
                            size_t CBFFstream,
                            int isbuffer,
                            int ciforcbf,
                            int headers,
                            int encoding){
        return cbf_write_widefile(
                                  cbff_cbf_handle(CBFFhandle),
                                  cbff_file_handle(CBFFstream),
                                  isbuffer,
                                  ciforcbf,
                                  headers,
                                  encoding);
    }
    
    
    
    /* Add a data block */
    
    int cbff_new_datablock(
                           size_t CBFFhandle,
                           const char * datablockname){
        return cbf_new_datablock(
                                 cbff_cbf_handle(CBFFhandle),
                                 datablockname);
    }
    
    
    
    /* Add a save frame block */
    
    int cbff_new_saveframe(
                           size_t CBFFhandle,
                           const char * saveframename){
        return cbf_new_saveframe(
                                 cbff_cbf_handle(CBFFhandle),
                                 saveframename);
    }
    
    
    
    /* Add a data block, allowing for duplicates */
    
    int cbff_force_new_datablock(
                                 size_t CBFFhandle,
                                 const char * datablockname){
        return cbf_force_new_datablock(
                                       cbff_cbf_handle(CBFFhandle),
                                       datablockname);
    }
    
    
    
    /* Add a save frame, allowing for duplicates */
    
    int cbff_force_new_saveframe(
                                 size_t CBFFhandle,
                                 const char * saveframename){
        return cbf_force_new_saveframe(
                                       cbff_cbf_handle(CBFFhandle),
                                       saveframename);
    }
    
    
    
    /* Add a category to the current data block */
    
    int cbff_new_category(
                          size_t CBFFhandle,
                          const char * categoryname){
        return cbf_new_category(
                                cbff_cbf_handle(CBFFhandle),
                                categoryname);
    }
    
    
    
    /* Add a category to the current data block, allowing for duplicates */
    
    int cbff_force_new_category(
                                size_t CBFFhandle,
                                const char * categoryname){
        return cbf_force_new_category(
                                      cbff_cbf_handle(CBFFhandle),
                                      categoryname);
    }
    
    
    
    /* Add a column to the current category */
    
    int cbff_new_column(
                        size_t CBFFhandle,
                        const char * columnname){
        return cbf_new_column(
                              cbff_cbf_handle(CBFFhandle),
                              columnname);
    }
    
    
    
    /* Add a row to the current category */
    
    int cbff_new_row(
                     size_t CBFFhandle){
        return cbf_new_row(
                           cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Insert a row in the current category */
    
    int cbff_insert_row(
                        size_t CBFFhandle,
                        const int rownumber){
        return cbf_insert_row(
                              cbff_cbf_handle(CBFFhandle),
                              rownumber);
    }
    
    
    
    /* Delete a row from the current category */
    
    int cbff_delete_row(
                        size_t CBFFhandle,
                        const int rownumber){
        return cbf_delete_row(
                              cbff_cbf_handle(CBFFhandle),
                              rownumber);
    }
    
    
    
    /* Change the name of the current data block */
    
    int cbff_set_datablockname(
                               size_t CBFFhandle,
                               const char * datablockname){
        return cbf_set_datablockname(
                                     cbff_cbf_handle(CBFFhandle),
                                     datablockname);
    }
    
    
    
    /* Change the name of the current save frame */
    
    int cbff_set_saveframename(
                               size_t CBFFhandle,
                               const char * saveframename){
        return cbf_set_saveframename(
                                     cbff_cbf_handle(CBFFhandle),
                                     saveframename);
    }
    
    
    
    /* Delete all categories from all the data blocks */
    
    int cbff_reset_datablocks(
                              size_t CBFFhandle){
        return cbf_reset_datablocks(
                                    cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete all categories from the current data block */
    
    int cbff_reset_datablock(
                             size_t CBFFhandle){
        return cbf_reset_datablock(
                                   cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete all categories from the current save frame */
    
    int cbff_reset_saveframe(
                             size_t CBFFhandle){
        return cbf_reset_saveframe(
                                   cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete all columns and rows from the current category */
    
    int cbff_reset_category(
                            size_t CBFFhandle){
        return cbf_reset_category(
                                  cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete the current data block */
    
    int cbff_remove_datablock(
                              size_t CBFFhandle){
        return cbf_remove_datablock(
                                    cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete the current save frame  */
    
    int cbff_remove_saveframe(
                              size_t CBFFhandle){
        return cbf_remove_saveframe(
                                    cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete the current category */
    
    int cbff_remove_category(
                             size_t CBFFhandle){
        return cbf_remove_category(
                                   cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete the current column */
    
    int cbff_remove_column(
                           size_t CBFFhandle){
        return cbf_remove_column(
                                 cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Delete the current row */
    
    int cbff_remove_row(
                        size_t CBFFhandle){
        return cbf_remove_row(
                              cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the first data block the current data block */
    
    int cbff_rewind_datablock(
                              size_t CBFFhandle){
        return cbf_rewind_datablock(
                                    cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the first category in the current data block the current category */
    
    int cbff_rewind_category(
                             size_t CBFFhandle){
        return cbf_rewind_category(
                                   cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the first save frame in the current data block the current category */
    
    int cbff_rewind_saveframe(
                              size_t CBFFhandle){
        return cbf_rewind_saveframe(
                                    cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the first category or save frame in the current data block the current category */
    
    int cbff_rewind_blockitem(
                              size_t CBFFhandle,
                              char * copy_type, size_t start_type, size_t end_type, int * status_type){
        CBF_NODETYPE type;
        int errorcode;
        errorcode = cbf_rewind_blockitem(
                                         cbff_cbf_handle(CBFFhandle),
                                         &type);
        cbff_nodetype (type, copy_type,
                       start_type, end_type,
                       status_type);
        return errorcode;
    }
    
    
    
    /* Make the first column in the current category the current column */
    
    int cbff_rewind_column(
                           size_t CBFFhandle){
        return cbf_rewind_column(
                                 cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the first row in the current category the current row */
    
    int cbff_rewind_row(
                        size_t CBFFhandle){
        return cbf_rewind_row(
                              cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the next data block the current data block */
    
    int cbff_next_datablock(
                            size_t CBFFhandle){
        return cbf_next_datablock(
                                  cbff_cbf_handle(CBFFhandle));
    }
    
    
    /* Make the next save frame in the current data block the current save frame */
    
    int cbff_next_saveframe(
                            size_t CBFFhandle){
        return cbf_next_saveframe(
                                  cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the next category in the current data block the current category */
    
    int cbff_next_category(
                           size_t CBFFhandle){
        return cbf_next_category(
                                 cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the next save frame or category the current data block or category */
    
    int cbff_next_blockitem(
                            size_t CBFFhandle,
                            char * copy_type, size_t start_type, size_t end_type, int * status_type){
        CBF_NODETYPE type;
        int errorcode;
        errorcode = cbf_next_blockitem(
                                       cbff_cbf_handle(CBFFhandle),
                                       &type);
        cbff_nodetype (type, copy_type,
                       start_type, end_type,
                       status_type);
        return errorcode;
    }
    
    
    
    
    /* Make the next column in the current category the current column */
    
    int cbff_next_column(
                         size_t CBFFhandle){
        return cbf_next_column(
                               cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the next row in the current category the current row */
    
    int cbff_next_row(
                      size_t CBFFhandle){
        return cbf_next_row(
                            cbff_cbf_handle(CBFFhandle));
    }
    
    
    
    /* Make the named data block the current data block */
    
    int cbff_find_datablock(
                            size_t CBFFhandle,
                            const char * datablockname){
        return cbf_find_datablock(
                                  cbff_cbf_handle(CBFFhandle),
                                  datablockname);
    }
    
    
    
    /* Make the named save frame in the current data block the current save frame */
    
    int cbff_find_saveframe(
                            size_t CBFFhandle,
                            const char * saveframe){
        return cbf_find_saveframe(
                                  cbff_cbf_handle(CBFFhandle),
                                  saveframe);
    }
    
    
    
    /* Make the named category in the current data block or save frame the current category */
    
    int cbff_find_category(
                           size_t CBFFhandle,
                           const char * categoryname){
        return cbf_find_category(
                                 cbff_cbf_handle(CBFFhandle),
                                 categoryname);
    }
    
    
    
    /* Make the named column in the current category the current column */
    
    int cbff_find_column(
                         size_t CBFFhandle,
                         const char * columnname){
        return cbf_find_column(
                               cbff_cbf_handle(CBFFhandle),
                               columnname);
    }
    
    
    
    /* Make the first row with matching value the current row */
    
    int cbff_find_row(
                      size_t CBFFhandle,
                      const char * value){
        return cbf_find_row(
                            cbff_cbf_handle(CBFFhandle),
                            value);
    }
    
    
    /* Make the first row with matching value the current row
     creating it if necessary */
    
    int cbff_require_row(
                         size_t CBFFhandle,
                         const char * value){
        return cbf_require_row(
                               cbff_cbf_handle(CBFFhandle),
                               value);
    }
    
    
    /* Make the next row with matching value the current row */
    
    int cbff_find_nextrow(
                          size_t CBFFhandle,
                          const char * value){
        return cbf_find_nextrow(
                                cbff_cbf_handle(CBFFhandle),
                                value);
    }
    
    
    /* Make the next row with matching value the current row,
     creating the row if necessary */
    
    int cbff_require_nextrow(
                             size_t CBFFhandle,
                             const char * value){
        return cbf_require_nextrow(
                                   cbff_cbf_handle(CBFFhandle),
                                   value);
    }
    
    
    /* Count the data blocks */
    
    int cbff_count_datablocks(
                              size_t CBFFhandle,
                              unsigned int * datablocks){
        return cbf_count_datablocks(
                                    cbff_cbf_handle(CBFFhandle),
                                    datablocks);
    }
    
    
    /* Count the save frames in the current data block */
    
    int cbff_count_saveframes(
                              size_t CBFFhandle,
                              unsigned int * saveframes){
        return cbf_count_saveframes(
                                    cbff_cbf_handle(CBFFhandle),
                                    saveframes);
    }
    
    
    /* Count the categories in the current data block */
    
    int cbff_count_categories(
                              size_t CBFFhandle,
                              unsigned int * categories){
        return cbf_count_categories(
                                    cbff_cbf_handle(CBFFhandle),
                                    categories);
    }
    
    
    
    /* Count the items in the current data block */
    
    int cbff_count_blockitems(
                              size_t CBFFhandle,
                              unsigned int * blockitems){
        return cbf_count_blockitems(
                                    cbff_cbf_handle(CBFFhandle),
                                    blockitems);
    }
    
    
    
    /* Count the columns in the current category */
    
    int cbff_count_columns(
                           size_t CBFFhandle,
                           unsigned int * columns){
        return cbf_count_columns(
                                 cbff_cbf_handle(CBFFhandle),
                                 columns);
    }
    
    
    
    /* Count the rows in the current category */
    
    int cbff_count_rows(
                        size_t CBFFhandle,
                        unsigned int * rows){
        return cbf_count_rows(
                              cbff_cbf_handle(CBFFhandle),
                              rows);
    }
    
    
    
    /* Make the specified data block the current data block */
    
    int cbff_select_datablock(
                              size_t CBFFhandle,
                              unsigned int datablock){
        return cbf_select_datablock(
                                    cbff_cbf_handle(CBFFhandle),
                                    datablock);
    }
    
    
    
    /* Make the specified save frame the current save frame */
    
    int cbff_select_saveframe(
                              size_t CBFFhandle,
                              unsigned int saveframe){
        return cbf_select_saveframe(
                                    cbff_cbf_handle(CBFFhandle),
                                    saveframe);
    }
    
    
    
    /* Make the specified category the current category */
    
    int cbff_select_category(
                             size_t CBFFhandle,
                             unsigned int category){
        return cbf_select_category(
                                   cbff_cbf_handle(CBFFhandle),
                                   category);
    }
    
    
    
    /* Make the specified category or save frame the current block item */
    
    int cbff_select_blockitem(
                              size_t CBFFhandle,
                              unsigned int item,
                              char * copy_type, size_t start_type, size_t end_type, int * status_type){
        CBF_NODETYPE type;
        int errorcode;
        errorcode = cbf_select_blockitem(
                                         cbff_cbf_handle(CBFFhandle),
                                         item,
                                         &type);
        cbff_nodetype (type, copy_type,
                       start_type, end_type,
                       status_type);
        return errorcode;
    }
    
    
    
    /* Make the specified column the current column */
    
    int cbff_select_column(
                           size_t CBFFhandle,
                           unsigned int column){
        return cbf_select_column(
                                 cbff_cbf_handle(CBFFhandle),
                                 column);
    }
    
    
    
    /* Make the specified row the current row */
    
    int cbff_select_row(
                        size_t CBFFhandle,
                        unsigned int row){
        return cbf_select_row(
                              cbff_cbf_handle(CBFFhandle),
                              row);
    }
    
    
    
    /* Get the name of the current data block */
    
    int cbff_datablock_name(
                            size_t CBFFhandle,
                            char * copy_datablockname, size_t start_datablockname, size_t end_datablockname, int * status_datablockname){
        const char * datablockname;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_datablock_name(
                                       cbff_cbf_handle(CBFFhandle),
                                       &datablockname);
        if (datablockname) {
            length = strlen(datablockname);
            for (index = 0; index < length-start_datablockname+1
                 && index < end_datablockname-start_datablockname+1; index++) {
                copy_datablockname[index] = datablockname[index+start_datablockname-1];
            }
            if (index < end_datablockname-start_datablockname+1) {
                for (; index < end_datablockname-start_datablockname+1; index++) {
                    copy_datablockname[index] = ' ';
                }
                *status_datablockname = 0; /* transfer complete */
            } else {
                if (length > end_datablockname) {
                    *status_datablockname = 1; /* more to transfer */
                } else {
                    *status_datablockname = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_datablockname-start_datablockname+1; index++) {
                copy_datablockname[index] = ' ';
            }
            *status_datablockname = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Get the name of the current save frame */
    
    int cbff_saveframe_name(
                            size_t CBFFhandle,
                            char * copy_saveframename, size_t start_saveframename, size_t end_saveframename, int * status_saveframename){
        const char * saveframename;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_saveframe_name(
                                       cbff_cbf_handle(CBFFhandle),
                                       &saveframename);
        if (saveframename) {
            length = strlen(saveframename);
            for (index = 0; index < length-start_saveframename+1
                 && index < end_saveframename-start_saveframename+1; index++) {
                copy_saveframename[index] = saveframename[index+start_saveframename-1];
            }
            if (index < end_saveframename-start_saveframename+1) {
                for (; index < end_saveframename-start_saveframename+1; index++) {
                    copy_saveframename[index] = ' ';
                }
                *status_saveframename = 0; /* transfer complete */
            } else {
                if (length > end_saveframename) {
                    *status_saveframename = 1; /* more to transfer */
                } else {
                    *status_saveframename = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_saveframename-start_saveframename+1; index++) {
                copy_saveframename[index] = ' ';
            }
            *status_saveframename = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Get the name of the current category */
    
    int cbff_category_name(
                           size_t CBFFhandle,
                           char * copy_categoryname, size_t start_categoryname, size_t end_categoryname, int * status_categoryname){
        const char * categoryname;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_category_name(
                                      cbff_cbf_handle(CBFFhandle),
                                      &categoryname);
        if (categoryname) {
            length = strlen(categoryname);
            for (index = 0; index < length-start_categoryname+1
                 && index < end_categoryname-start_categoryname+1; index++) {
                copy_categoryname[index] = categoryname[index+start_categoryname-1];
            }
            if (index < end_categoryname-start_categoryname+1) {
                for (; index < end_categoryname-start_categoryname+1; index++) {
                    copy_categoryname[index] = ' ';
                }
                *status_categoryname = 0; /* transfer complete */
            } else {
                if (length > end_categoryname) {
                    *status_categoryname = 1; /* more to transfer */
                } else {
                    *status_categoryname = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_categoryname-start_categoryname+1; index++) {
                copy_categoryname[index] = ' ';
            }
            *status_categoryname = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Get the name of the current column */
    
    int cbff_column_name(
                         size_t CBFFhandle,
                         char * copy_columnname, size_t start_columnname, size_t end_columnname, int * status_columnname){
        const char * columnname;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_column_name(
                                    cbff_cbf_handle(CBFFhandle),
                                    &columnname);
        if (columnname) {
            length = strlen(columnname);
            for (index = 0; index < length-start_columnname+1
                 && index < end_columnname-start_columnname+1; index++) {
                copy_columnname[index] = columnname[index+start_columnname-1];
            }
            if (index < end_columnname-start_columnname+1) {
                for (; index < end_columnname-start_columnname+1; index++) {
                    copy_columnname[index] = ' ';
                }
                *status_columnname = 0; /* transfer complete */
            } else {
                if (length > end_columnname) {
                    *status_columnname = 1; /* more to transfer */
                } else {
                    *status_columnname = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_columnname-start_columnname+1; index++) {
                copy_columnname[index] = ' ';
            }
            *status_columnname = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Get the number of the current row */
    
    int cbff_row_number(
                        size_t CBFFhandle,
                        unsigned int * row){
        return cbf_row_number(
                              cbff_cbf_handle(CBFFhandle),
                              row);
    }
    
    
    
    /* Get the number of the current column */
    
    int cbff_column_number(
                           size_t CBFFhandle,
                           unsigned int * column){
        return cbf_column_number(
                                 cbff_cbf_handle(CBFFhandle),
                                 column);
    }
    
    
    
    /* Get the number of the current block item */
    
    int cbff_blockitem_number(
                              size_t CBFFhandle,
                              unsigned int * blockitem){
        return cbf_blockitem_number(
                                    cbff_cbf_handle(CBFFhandle),
                                    blockitem);
    }
    
    
    
    /* Get the ascii value of the current (row, column) entry */
    
    int cbff_get_value(
                       size_t CBFFhandle,
                       char * copy_value, size_t start_value, size_t end_value, int * status_value){
        const char * value;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_value(
                                  cbff_cbf_handle(CBFFhandle),
                                  &value);
        if (value) {
            length = strlen(value);
            for (index = 0; index < length-start_value+1
                 && index < end_value-start_value+1; index++) {
                copy_value[index] = value[index+start_value-1];
            }
            if (index < end_value-start_value+1) {
                for (; index < end_value-start_value+1; index++) {
                    copy_value[index] = ' ';
                }
                *status_value = 0; /* transfer complete */
            } else {
                if (length > end_value) {
                    *status_value = 1; /* more to transfer */
                } else {
                    *status_value = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_value-start_value+1; index++) {
                copy_value[index] = ' ';
            }
            *status_value = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Set the ascii value of the current (row, column) entry */
    
    int cbff_set_value(
                       size_t CBFFhandle,
                       const char * value){
        return cbf_set_value(
                             cbff_cbf_handle(CBFFhandle),
                             value);
    }
    
    
    /* Get the ascii value of the current (row, column) entry,
     setting it to a default value if necessary */
    
    int cbff_require_value(
                           size_t CBFFhandle,
                           char * copy_value, size_t start_value, size_t end_value, int * status_value,
                           const char * defaultvalue){
        const char * value;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_require_value(
                                      cbff_cbf_handle(CBFFhandle),
                                      &value,
                                      defaultvalue);
        if (value) {
            length = strlen(value);
            for (index = 0; index < length-start_value+1
                 && index < end_value-start_value+1; index++) {
                copy_value[index] = value[index+start_value-1];
            }
            if (index < end_value-start_value+1) {
                for (; index < end_value-start_value+1; index++) {
                    copy_value[index] = ' ';
                }
                *status_value = 0; /* transfer complete */
            } else {
                if (length > end_value) {
                    *status_value = 1; /* more to transfer */
                } else {
                    *status_value = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_value-start_value+1; index++) {
                copy_value[index] = ' ';
            }
            *status_value = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    
    /* Get the ascii type of value of the current (row, column) entry */
    
    int cbff_get_typeofvalue(
                             size_t CBFFhandle,
                             char * copy_typeofvalue, size_t start_typeofvalue, size_t end_typeofvalue, int * status_typeofvalue){
        const char * typeofvalue;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_typeofvalue(
                                        cbff_cbf_handle(CBFFhandle),
                                        &typeofvalue);
        if (typeofvalue) {
            length = strlen(typeofvalue);
            for (index = 0; index < length-start_typeofvalue+1
                 && index < end_typeofvalue-start_typeofvalue+1; index++) {
                copy_typeofvalue[index] = typeofvalue[index+start_typeofvalue-1];
            }
            if (index < end_typeofvalue-start_typeofvalue+1) {
                for (; index < end_typeofvalue-start_typeofvalue+1; index++) {
                    copy_typeofvalue[index] = ' ';
                }
                *status_typeofvalue = 0; /* transfer complete */
            } else {
                if (length > end_typeofvalue) {
                    *status_typeofvalue = 1; /* more to transfer */
                } else {
                    *status_typeofvalue = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_typeofvalue-start_typeofvalue+1; index++) {
                copy_typeofvalue[index] = ' ';
            }
            *status_typeofvalue = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Set the ascii type of value of the current (row, column) entry */
    
    int cbff_set_typeofvalue(
                             size_t CBFFhandle,
                             const char * typeofvalue){
        return cbf_set_typeofvalue(
                                   cbff_cbf_handle(CBFFhandle),
                                   typeofvalue);
    }
    
    
    
    /* Get the (int) numeric value of the current (row, column) entry */
    
    int cbff_get_integervalue(
                              size_t CBFFhandle,
                              int * number){
        return cbf_get_integervalue(
                                    cbff_cbf_handle(CBFFhandle),
                                    number);
    }
    
    
    
    /* Get the (double) numeric value of the current (row, column) entry */
    
    int cbff_get_doublevalue(
                             size_t CBFFhandle,
                             double * number){
        return cbf_get_doublevalue(
                                   cbff_cbf_handle(CBFFhandle),
                                   number);
    }
    
    
    
    /* Set the ascii value of the current (row, column) entry from an int */
    
    int cbff_set_integervalue(
                              size_t CBFFhandle,
                              int number){
        return cbf_set_integervalue(
                                    cbff_cbf_handle(CBFFhandle),
                                    number);
    }
    
    
    
    /* Set the ascii value of the current (row, column) entry from a double */
    
    int cbff_set_doublevalue(
                             size_t CBFFhandle,
                             const char * format,
                             double number){
        return cbf_set_doublevalue(
                                   cbff_cbf_handle(CBFFhandle),
                                   format,
                                   number);
    }
    
    
    
    /* Get the (integer) numeric value of the current (row, column) entry, setting it if necessary */
    
    int cbff_require_integervalue(
                                  size_t CBFFhandle,
                                  int * number,
                                  int defaultvalue){
        return cbf_require_integervalue(
                                        cbff_cbf_handle(CBFFhandle),
                                        number,
                                        defaultvalue);
    }
    
    
    
    /* Get the (double) numeric value of the current (row, column) entry, setting it if necessary */
    
    int cbff_require_doublevalue(
                                 size_t CBFFhandle,
                                 double * number,
                                 double defaultvalue){
        return cbf_require_doublevalue(
                                       cbff_cbf_handle(CBFFhandle),
                                       number,
                                       defaultvalue);
    }
    
    
    
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
                                 int * realarray){
        return cbf_get_arrayparameters(
                                       cbff_cbf_handle(CBFFhandle),
                                       compression,
                                       id,
                                       elsize,
                                       elsigned,
                                       elunsigned,
                                       nelem,
                                       minelem,
                                       maxelem,
                                       realarray);
    }
    
    
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
                                       size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_arrayparameters_wdims(
                                                  cbff_cbf_handle(CBFFhandle),
                                                  compression,
                                                  id,
                                                  elsize,
                                                  elsigned,
                                                  elunsigned,
                                                  nelem,
                                                  minelem,
                                                  maxelem,
                                                  realarray,
                                                  &byteorder,
                                                  dimfast,
                                                  dimmid,
                                                  dimslow,
                                                  padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
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
                                          size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_arrayparameters_wdims(
                                                  cbff_cbf_handle(CBFFhandle),
                                                  compression,
                                                  id,
                                                  elsize,
                                                  elsigned,
                                                  elunsigned,
                                                  nelem,
                                                  minelem,
                                                  maxelem,
                                                  realarray,
                                                  &byteorder,
                                                  dimfast,
                                                  dimmid,
                                                  dimslow,
                                                  padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
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
                                          size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_arrayparameters_wdims(
                                                  cbff_cbf_handle(CBFFhandle),
                                                  compression,
                                                  id,
                                                  elsize,
                                                  elsigned,
                                                  elunsigned,
                                                  nelem,
                                                  minelem,
                                                  maxelem,
                                                  realarray,
                                                  &byteorder,
                                                  dimfast,
                                                  dimmid,
                                                  dimslow,
                                                  padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
    /* Get the dimensions of the current (row, column) array entry
     from the CBF tags */
    
    
    int cbff_get_arraydimensions(size_t CBFFhandle,
                                size_t * dimover,
                                size_t * dimfast,
                                size_t * dimmid,
                                size_t * dimslow) {
        
        return cbf_get_arraydimensions(cbff_cbf_handle(CBFFhandle),
                                     dimover,
                                     dimfast,
                                     dimmid,
                                     dimslow);
            
    }
    
    
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
                                        int * maxelem){
        return cbf_get_integerarrayparameters(
                                              cbff_cbf_handle(CBFFhandle),
                                              compression,
                                              id,
                                              elsize,
                                              elsigned,
                                              elunsigned,
                                              nelem,
                                              minelem,
                                              maxelem);
    }
    
    
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
                                              size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_integerarrayparameters_wdims(
                                                         cbff_cbf_handle(CBFFhandle),
                                                         compression,
                                                         id,
                                                         elsize,
                                                         elsigned,
                                                         elunsigned,
                                                         nelem,
                                                         minelem,
                                                         maxelem,
                                                         &byteorder,
                                                         dimfast,
                                                         dimmid,
                                                         dimslow,
                                                         padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
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
                                                 size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_integerarrayparameters_wdims(
                                                         cbff_cbf_handle(CBFFhandle),
                                                         compression,
                                                         id,
                                                         elsize,
                                                         elsigned,
                                                         elunsigned,
                                                         nelem,
                                                         minelem,
                                                         maxelem,
                                                         &byteorder,
                                                         dimfast,
                                                         dimmid,
                                                         dimslow,
                                                         padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
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
                                                 size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_integerarrayparameters_wdims(
                                                         cbff_cbf_handle(CBFFhandle),
                                                         compression,
                                                         id,
                                                         elsize,
                                                         elsigned,
                                                         elunsigned,
                                                         nelem,
                                                         minelem,
                                                         maxelem,
                                                         &byteorder,
                                                         dimfast,
                                                         dimmid,
                                                         dimslow,
                                                         padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Get the integer value of the current (row, column) array entry */
    
    int cbff_get_integerarray(
                              size_t CBFFhandle,
                              int * id,
                              void * value,
                              size_t elsize,
                              int elsign,
                              size_t nelem,
                              size_t * nelem_read){
        return cbf_get_integerarray(
                                    cbff_cbf_handle(CBFFhandle),
                                    id,
                                    value,
                                    elsize,
                                    elsign,
                                    nelem,
                                    nelem_read);
    }
    
    
    /* Get the real value of the current (row, column) array entry */
    
    int cbff_get_realarray(
                           size_t CBFFhandle,
                           int * id,
                           void * value,
                           size_t elsize,
                           size_t nelem,
                           size_t * nelem_read){
        return cbf_get_realarray(
                                 cbff_cbf_handle(CBFFhandle),
                                 id,
                                 value,
                                 elsize,
                                 nelem,
                                 nelem_read);
    }
    
    
    /* Get the parameters of the current (row, column) array entry */
    
    int cbff_get_realarrayparameters(
                                     size_t CBFFhandle,
                                     unsigned int * compression,
                                     int * id,
                                     size_t * elsize,
                                     size_t * nelem){
        return cbf_get_realarrayparameters(
                                           cbff_cbf_handle(CBFFhandle),
                                           compression,
                                           id,
                                           elsize,
                                           nelem);
    }
    
    
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
                                           size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_realarrayparameters_wdims(
                                                      cbff_cbf_handle(CBFFhandle),
                                                      compression,
                                                      id,
                                                      elsize,
                                                      nelem,
                                                      &byteorder,
                                                      dimfast,
                                                      dimmid,
                                                      dimslow,
                                                      padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
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
                                              size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_realarrayparameters_wdims(
                                                      cbff_cbf_handle(CBFFhandle),
                                                      compression,
                                                      id,
                                                      elsize,
                                                      nelem,
                                                      &byteorder,
                                                      dimfast,
                                                      dimmid,
                                                      dimslow,
                                                      padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
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
                                              size_t * padding){
        const char * byteorder;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_realarrayparameters_wdims(
                                                      cbff_cbf_handle(CBFFhandle),
                                                      compression,
                                                      id,
                                                      elsize,
                                                      nelem,
                                                      &byteorder,
                                                      dimfast,
                                                      dimmid,
                                                      dimslow,
                                                      padding);
        if (byteorder) {
            length = strlen(byteorder);
            for (index = 0; index < length-start_byteorder+1
                 && index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = byteorder[index+start_byteorder-1];
            }
            if (index < end_byteorder-start_byteorder+1) {
                for (; index < end_byteorder-start_byteorder+1; index++) {
                    copy_byteorder[index] = ' ';
                }
                *status_byteorder = 0; /* transfer complete */
            } else {
                if (length > end_byteorder) {
                    *status_byteorder = 1; /* more to transfer */
                } else {
                    *status_byteorder = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byteorder-start_byteorder+1; index++) {
                copy_byteorder[index] = ' ';
            }
            *status_byteorder = -1; /* null string case */
        }
        return errorcode;
    }
    
    /* Set the integer value of the current (row, column) array entry */
    
    int cbff_set_integerarray(
                              size_t CBFFhandle,
                              unsigned int compression,
                              int id,
                              void * value,
                              size_t elsize,
                              int elsign,
                              size_t nelem){
        return cbf_set_integerarray(
                                    cbff_cbf_handle(CBFFhandle),
                                    compression,
                                    id,
                                    value,
                                    elsize,
                                    elsign,
                                    nelem);
    }
    
    
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
                                    size_t padding){
        return cbf_set_integerarray_wdims(
                                          cbff_cbf_handle(CBFFhandle),
                                          compression,
                                          id,
                                          value,
                                          elsize,
                                          elsign,
                                          nelem,
                                          byteorder,
                                          dimfast,
                                          dimmid,
                                          dimslow,
                                          padding);
    }
    
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
                                       size_t padding){
        return cbf_set_integerarray_wdims(
                                          cbff_cbf_handle(CBFFhandle),
                                          compression,
                                          id,
                                          value,
                                          elsize,
                                          elsign,
                                          nelem,
                                          byteorder,
                                          dimfast,
                                          dimmid,
                                          dimslow,
                                          padding);
    }
    
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
                                       size_t padding){
        return cbf_set_integerarray_wdims(
                                          cbff_cbf_handle(CBFFhandle),
                                          compression,
                                          id,
                                          value,
                                          elsize,
                                          elsign,
                                          nelem,
                                          byteorder,
                                          dimfast,
                                          dimmid,
                                          dimslow,
                                          padding);
    }
    
    
    /* Set the real value of the current (row, column) array entry */
    
    int cbff_set_realarray(
                           size_t CBFFhandle,
                           unsigned int compression,
                           int id,
                           void * value,
                           size_t elsize,
                           size_t nelem){
        return cbf_set_realarray(
                                 cbff_cbf_handle(CBFFhandle),
                                 compression,
                                 id,
                                 value,
                                 elsize,
                                 nelem);
    }
    
    
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
                                 size_t padding){
        return cbf_set_realarray_wdims(
                                       cbff_cbf_handle(CBFFhandle),
                                       compression,
                                       id,
                                       value,
                                       elsize,
                                       nelem,
                                       byteorder,
                                       dimfast,
                                       dimmid,
                                       dimslow,
                                       padding);
    }
    
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
                                    size_t padding){
        return cbf_set_realarray_wdims(
                                       cbff_cbf_handle(CBFFhandle),
                                       compression,
                                       id,
                                       value,
                                       elsize,
                                       nelem,
                                       byteorder,
                                       dimfast,
                                       dimmid,
                                       dimslow,
                                       padding);
    }
    
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
                                    size_t padding){
        return cbf_set_realarray_wdims(
                                       cbff_cbf_handle(CBFFhandle),
                                       compression,
                                       id,
                                       value,
                                       elsize,
                                       nelem,
                                       byteorder,
                                       dimfast,
                                       dimmid,
                                       dimslow,
                                       padding);
    }
    
    /* Issue a warning message */
    
    void cbff_warning (const char *message) {
        cbf_warning(message);
    }
    
    
    
    /* Issue an error message */
    
    void cbff_error (const char *message) {
        cbf_error(message);
    }
    
    
    
    /* issue a log message for a cbf */
    
    void cbff_log (size_t CBFFhandle, const char *message, int logflags) {
        cbf_log(cbff_cbf_handle(CBFFhandle), message, logflags);
    }
    
    
    
    /* Find a datablock, creating it if necessary */
    
    int cbff_require_datablock(
                               size_t CBFFhandle,
                               const char * datablockname){
        return cbf_require_datablock(
                                     cbff_cbf_handle(CBFFhandle),
                                     datablockname);
    }
    
    
    /* Find a category, creating it if necessary */
    
    int cbff_require_category(
                              size_t CBFFhandle,
                              const char * categoryname){
        return cbf_require_category(
                                    cbff_cbf_handle(CBFFhandle),
                                    categoryname);
    }
    
    
    /* Find a column, creating it if necessary */
    
    int cbff_require_column(
                            size_t CBFFhandle,
                            const char * columnname){
        return cbf_require_column(
                                  cbff_cbf_handle(CBFFhandle),
                                  columnname);
    }
    
    
    
    /* Find a column value, return a default if necessary */
    
    int cbff_require_column_value(
                                  size_t CBFFhandle,
                                  const char * columnname,
                                  char * copy_value, size_t start_value, size_t end_value, int * status_value,
                                  const char * defaultvalue){
        const char * value;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_require_column_value(
                                             cbff_cbf_handle(CBFFhandle),
                                             columnname,
                                             &value,
                                             defaultvalue);
        if (value) {
            length = strlen(value);
            for (index = 0; index < length-start_value+1
                 && index < end_value-start_value+1; index++) {
                copy_value[index] = value[index+start_value-1];
            }
            if (index < end_value-start_value+1) {
                for (; index < end_value-start_value+1; index++) {
                    copy_value[index] = ' ';
                }
                *status_value = 0; /* transfer complete */
            } else {
                if (length > end_value) {
                    *status_value = 1; /* more to transfer */
                } else {
                    *status_value = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_value-start_value+1; index++) {
                copy_value[index] = ' ';
            }
            *status_value = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Find a column integer value, return a default if necessary */
    
    int cbff_require_column_integervalue(
                                         size_t CBFFhandle,
                                         const char * columnname,
                                         int * number,
                                         const int defaultvalue){
        return cbf_require_column_integervalue(
                                               cbff_cbf_handle(CBFFhandle),
                                               columnname,
                                               number,
                                               defaultvalue);
    }
    
    
    /* Find a column double value, return a default if necessary */
    
    int cbff_require_column_doublevalue(
                                        size_t CBFFhandle,
                                        const char * columnname,
                                        double * number,
                                        const double defaultvalue){
        return cbf_require_column_doublevalue(
                                              cbff_cbf_handle(CBFFhandle),
                                              columnname,
                                              number,
                                              defaultvalue);
    }
    
    
    /* Get the local byte order of the default integer type */
    
    int cbff_get_local_integer_byte_order(
                                          char * copy_byte_order, size_t start_byte_order, size_t end_byte_order, int * status_byte_order){
        char * byte_order;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_local_integer_byte_order(
                                                     &byte_order);
        if (byte_order) {
            length = strlen(byte_order);
            for (index = 0; index < length-start_byte_order+1
                 && index < end_byte_order-start_byte_order+1; index++) {
                copy_byte_order[index] = byte_order[index+start_byte_order-1];
            }
            if (index < end_byte_order-start_byte_order+1) {
                for (; index < end_byte_order-start_byte_order+1; index++) {
                    copy_byte_order[index] = ' ';
                }
                *status_byte_order = 0; /* transfer complete */
            } else {
                if (length > end_byte_order) {
                    *status_byte_order = 1; /* more to transfer */
                } else {
                    *status_byte_order = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byte_order-start_byte_order+1; index++) {
                copy_byte_order[index] = ' ';
            }
            *status_byte_order = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Get the local byte order of the default real type */
    
    int cbff_get_local_real_byte_order(
                                       char * copy_byte_order, size_t start_byte_order, size_t end_byte_order, int * status_byte_order){
        char * byte_order;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_local_real_byte_order(
                                                  &byte_order);
        if (byte_order) {
            length = strlen(byte_order);
            for (index = 0; index < length-start_byte_order+1
                 && index < end_byte_order-start_byte_order+1; index++) {
                copy_byte_order[index] = byte_order[index+start_byte_order-1];
            }
            if (index < end_byte_order-start_byte_order+1) {
                for (; index < end_byte_order-start_byte_order+1; index++) {
                    copy_byte_order[index] = ' ';
                }
                *status_byte_order = 0; /* transfer complete */
            } else {
                if (length > end_byte_order) {
                    *status_byte_order = 1; /* more to transfer */
                } else {
                    *status_byte_order = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_byte_order-start_byte_order+1; index++) {
                copy_byte_order[index] = ' ';
            }
            *status_byte_order = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Get the local real format */
    
    int cbff_get_local_real_format(
                                   char * copy_real_format, size_t start_real_format, size_t end_real_format, int * status_real_format){
        char * real_format;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_local_real_format(
                                              &real_format);
        if (real_format) {
            length = strlen(real_format);
            for (index = 0; index < length-start_real_format+1
                 && index < end_real_format-start_real_format+1; index++) {
                copy_real_format[index] = real_format[index+start_real_format-1];
            }
            if (index < end_real_format-start_real_format+1) {
                for (; index < end_real_format-start_real_format+1; index++) {
                    copy_real_format[index] = ' ';
                }
                *status_real_format = 0; /* transfer complete */
            } else {
                if (length > end_real_format) {
                    *status_real_format = 1; /* more to transfer */
                } else {
                    *status_real_format = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_real_format-start_real_format+1; index++) {
                copy_real_format[index] = ' ';
            }
            *status_real_format = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Get the dictionary for a cbf */
    
    int cbff_get_dictionary(
                            size_t CBFFhandle,
                            size_t * CBFFdictionary){
        cbf_handle dictionary;
        int errorcode;
        errorcode = cbf_get_dictionary(
                                       cbff_cbf_handle(CBFFhandle),
                                       &dictionary);
        *CBFFdictionary = cbff_handle(dictionary);
        return errorcode;
    }
    
    
    /* Set the dictionary for a cbf */
    
    int cbff_set_dictionary(
                            size_t CBFFhandle,
                            size_t CBFFdictionary){
        return cbf_set_dictionary(
                                  cbff_cbf_handle(CBFFhandle),
                                  cbff_cbf_handle(CBFFdictionary));
    }
    
    
    /* Get the dictionary for a cbf, or create one */
    
    int cbff_require_dictionary(
                                size_t CBFFhandle,
                                size_t * CBFFdictionary){
        cbf_handle dictionary;
        int errorcode;
        errorcode = cbf_require_dictionary(
                                           cbff_cbf_handle(CBFFhandle),
                                           &dictionary);
        *CBFFdictionary = cbff_handle(dictionary);
        return errorcode;
    }
    
    
    /* Put the value into the named column, updating the hash table links */
    
    int cbff_set_hashedvalue(
                             size_t CBFFhandle,
                             const char * value,
                             const char * columnname,
                             int valuerow){
        return cbf_set_hashedvalue(
                                   cbff_cbf_handle(CBFFhandle),
                                   value,
                                   columnname,
                                   valuerow);
    }
    
    
    /* Find value in the named column, using the hash table links */
    
    int cbff_find_hashedvalue(
                              size_t CBFFhandle,
                              const char * value,
                              const char * columnname,
                              int caseinsensitive){
        return cbf_find_hashedvalue(
                                    cbff_cbf_handle(CBFFhandle),
                                    value,
                                    columnname,
                                    caseinsensitive);
    }
    
    
    
    /* Take a definition from a dictionary and insert it into the
     hash tables of a cbf dictionary */
    
    int cbff_convert_dictionary_definition(
                                           size_t CBFFcbfdictionary,
                                           size_t CBFFdictionary,
                                           const char * name){
        return cbf_convert_dictionary_definition(
                                                 cbff_cbf_handle(CBFFcbfdictionary),
                                                 cbff_cbf_handle(CBFFdictionary),
                                                 name);
    }
    
    
    
    /* Increment a column */
    
    int cbff_increment_column(
                              size_t CBFFhandle,
                              const char* columnname,
                              int * count){
        return cbf_increment_column(
                                    cbff_cbf_handle(CBFFhandle),
                                    columnname,
                                    count);
    }
    
    
    /* Reset a column */
    
    int cbff_reset_column(
                          size_t CBFFhandle,
                          const char* columnname){
        return cbf_reset_column(
                                cbff_cbf_handle(CBFFhandle),
                                columnname);
    }
    
    
    
    /* Reset reference counts for a dictionary */
    
    int cbff_reset_refcounts(
                             size_t CBFFdictionary){
        return cbf_reset_refcounts(
                                   cbff_cbf_handle(CBFFdictionary));
    }
    
    
    
    /* Convert a DDL1 or DDL2 dictionary and add it to a CBF dictionary */
    
    int cbff_convert_dictionary(
                                size_t CBFFhandle,
                                size_t CBFFdictionary){
        return cbf_convert_dictionary(
                                      cbff_cbf_handle(CBFFhandle),
                                      cbff_cbf_handle(CBFFdictionary));
    }
    
    
    
    /* Find the requested tag anywhere in the cbf, make it the current column */
    
    int cbff_find_tag(
                      size_t CBFFhandle,
                      const char * tag){
        return cbf_find_tag(
                            cbff_cbf_handle(CBFFhandle),
                            tag);
    }
    
    
    /* Find the requested tag in the cbf within the current
     
     save frame or data block, make it the current column */
    
    int cbff_find_local_tag(
                            size_t CBFFhandle,
                            const char * tag){
        return cbf_find_local_tag(
                                  cbff_cbf_handle(CBFFhandle),
                                  tag);
    }
    
    
    /* Find the requested category and column anywhere in the cbf, make it the current column */
    
    int cbff_srch_tag(
                      size_t CBFFhandle,
                      size_t CBFFnode,
                      const char * categoryname,
                      const char * columnname){
        return cbf_srch_tag(
                            cbff_cbf_handle(CBFFhandle),
                            cbff_cbf_node_handle(CBFFnode),
                            categoryname,
                            columnname);
    }
    
    
    /* Find the root alias of a given category */
    
    int cbff_find_category_root(
                                size_t CBFFhandle,
                                const char* categoryname,
                                char * copy_categoryroot, size_t start_categoryroot, size_t end_categoryroot, int * status_categoryroot){
        const char * categoryroot;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_find_category_root(
                                           cbff_cbf_handle(CBFFhandle),
                                           categoryname,
                                           &categoryroot);
        if (categoryroot) {
            length = strlen(categoryroot);
            for (index = 0; index < length-start_categoryroot+1
                 && index < end_categoryroot-start_categoryroot+1; index++) {
                copy_categoryroot[index] = categoryroot[index+start_categoryroot-1];
            }
            if (index < end_categoryroot-start_categoryroot+1) {
                for (; index < end_categoryroot-start_categoryroot+1; index++) {
                    copy_categoryroot[index] = ' ';
                }
                *status_categoryroot = 0; /* transfer complete */
            } else {
                if (length > end_categoryroot) {
                    *status_categoryroot = 1; /* more to transfer */
                } else {
                    *status_categoryroot = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_categoryroot-start_categoryroot+1; index++) {
                copy_categoryroot[index] = ' ';
            }
            *status_categoryroot = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Find the root alias of a given category, defaulting to the current one */
    
    int cbff_require_category_root(
                                   size_t CBFFhandle,
                                   const char* categoryname,
                                   char * copy_categoryroot, size_t start_categoryroot, size_t end_categoryroot, int * status_categoryroot){
        const char * categoryroot;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_require_category_root(
                                              cbff_cbf_handle(CBFFhandle),
                                              categoryname,
                                              &categoryroot);
        if (categoryroot) {
            length = strlen(categoryroot);
            for (index = 0; index < length-start_categoryroot+1
                 && index < end_categoryroot-start_categoryroot+1; index++) {
                copy_categoryroot[index] = categoryroot[index+start_categoryroot-1];
            }
            if (index < end_categoryroot-start_categoryroot+1) {
                for (; index < end_categoryroot-start_categoryroot+1; index++) {
                    copy_categoryroot[index] = ' ';
                }
                *status_categoryroot = 0; /* transfer complete */
            } else {
                if (length > end_categoryroot) {
                    *status_categoryroot = 1; /* more to transfer */
                } else {
                    *status_categoryroot = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_categoryroot-start_categoryroot+1; index++) {
                copy_categoryroot[index] = ' ';
            }
            *status_categoryroot = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Set the root alias of a given category */
    
    int cbff_set_category_root(
                               size_t CBFFhandle,
                               const char* categoryname,
                               const char* categoryroot){
        return cbf_set_category_root(
                                     cbff_cbf_handle(CBFFhandle),
                                     categoryname,
                                     categoryroot);
    }
    
    
    /* Find the root alias of a given tag */
    
    int cbff_find_tag_root(
                           size_t CBFFhandle,
                           const char* tagname,
                           char * copy_tagroot, size_t start_tagroot, size_t end_tagroot, int * status_tagroot){
        const char * tagroot;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_find_tag_root(
                                      cbff_cbf_handle(CBFFhandle),
                                      tagname,
                                      &tagroot);
        if (tagroot) {
            length = strlen(tagroot);
            for (index = 0; index < length-start_tagroot+1
                 && index < end_tagroot-start_tagroot+1; index++) {
                copy_tagroot[index] = tagroot[index+start_tagroot-1];
            }
            if (index < end_tagroot-start_tagroot+1) {
                for (; index < end_tagroot-start_tagroot+1; index++) {
                    copy_tagroot[index] = ' ';
                }
                *status_tagroot = 0; /* transfer complete */
            } else {
                if (length > end_tagroot) {
                    *status_tagroot = 1; /* more to transfer */
                } else {
                    *status_tagroot = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_tagroot-start_tagroot+1; index++) {
                copy_tagroot[index] = ' ';
            }
            *status_tagroot = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Find the root alias of a given tag, defaulting to the current one */
    
    int cbff_require_tag_root(
                              size_t CBFFhandle,
                              const char* tagname,
                              char * copy_tagroot, size_t start_tagroot, size_t end_tagroot, int * status_tagroot){
        const char * tagroot;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_require_tag_root(
                                         cbff_cbf_handle(CBFFhandle),
                                         tagname,
                                         &tagroot);
        if (tagroot) {
            length = strlen(tagroot);
            for (index = 0; index < length-start_tagroot+1
                 && index < end_tagroot-start_tagroot+1; index++) {
                copy_tagroot[index] = tagroot[index+start_tagroot-1];
            }
            if (index < end_tagroot-start_tagroot+1) {
                for (; index < end_tagroot-start_tagroot+1; index++) {
                    copy_tagroot[index] = ' ';
                }
                *status_tagroot = 0; /* transfer complete */
            } else {
                if (length > end_tagroot) {
                    *status_tagroot = 1; /* more to transfer */
                } else {
                    *status_tagroot = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_tagroot-start_tagroot+1; index++) {
                copy_tagroot[index] = ' ';
            }
            *status_tagroot = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    /* Set the root alias of a given tag */
    
    int cbff_set_tag_root(
                          size_t CBFFhandle,
                          const char* tagname,
                          const char* tagroot){
        return cbf_set_tag_root(
                                cbff_cbf_handle(CBFFhandle),
                                tagname,
                                tagroot);
    }
    
    
    /* Find the category of a given tag */
    
    int cbff_find_tag_category(
                               size_t CBFFhandle,
                               const char* tagname,
                               char * copy_categoryname, size_t start_categoryname, size_t end_categoryname, int * status_categoryname){
        const char * categoryname;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_find_tag_category(
                                          cbff_cbf_handle(CBFFhandle),
                                          tagname,
                                          &categoryname);
        if (categoryname) {
            length = strlen(categoryname);
            for (index = 0; index < length-start_categoryname+1
                 && index < end_categoryname-start_categoryname+1; index++) {
                copy_categoryname[index] = categoryname[index+start_categoryname-1];
            }
            if (index < end_categoryname-start_categoryname+1) {
                for (; index < end_categoryname-start_categoryname+1; index++) {
                    copy_categoryname[index] = ' ';
                }
                *status_categoryname = 0; /* transfer complete */
            } else {
                if (length > end_categoryname) {
                    *status_categoryname = 1; /* more to transfer */
                } else {
                    *status_categoryname = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_categoryname-start_categoryname+1; index++) {
                copy_categoryname[index] = ' ';
            }
            *status_categoryname = -1; /* null string case */
        }
        return errorcode;
    }
    
    /* Set category of a given tag */
    
    int cbff_set_tag_category(
                              size_t CBFFhandle,
                              const char* tagname,
                              const char* categoryname){
        return cbf_set_tag_category(
                                    cbff_cbf_handle(CBFFhandle),
                                    tagname,
                                    categoryname);
    }
    
    
    /* Validate portion of CBF */
    
    int cbff_validate(
                      size_t CBFFhandle,
                      size_t CBFFnode,
                      char * CBFFtype,
                      size_t CBFFcatnode){
        return cbf_validate(
                            cbff_cbf_handle(CBFFhandle),
                            cbff_cbf_node_handle(CBFFnode),
                            cbff_cbf_nodetype(CBFFtype),
                            cbff_cbf_node_handle(CBFFcatnode));
    }
    
    
    /* Load accumulator */
    
    int cbff_mpint_load_acc(
                            unsigned int * acc,
                            size_t acsize,
                            void * source,
                            size_t elsize,
                            int elsign,
                            const char * border){
        return cbf_mpint_load_acc(
                                  acc,
                                  acsize,
                                  source,
                                  elsize,
                                  elsign,
                                  border);
    }
    
    
    
    /* Store accumulator */
    
    int cbff_mpint_store_acc(
                             unsigned int * acc,
                             size_t acsize,
                             void * dest,
                             size_t elsize,
                             int elsign,
                             const char * border){
        return cbf_mpint_store_acc(
                                   acc,
                                   acsize,
                                   dest,
                                   elsize,
                                   elsign,
                                   border);
    }
    
    
    /* Clear accumulator */
    
    int cbff_mpint_clear_acc(
                             unsigned int * acc,
                             size_t acsize){
        return cbf_mpint_clear_acc(
                                   acc,
                                   acsize);
    }
    
    
    /* Increment accumulator */
    
    int cbff_mpint_increment_acc(
                                 unsigned int * acc,
                                 size_t acsize){
        return cbf_mpint_increment_acc(
                                       acc,
                                       acsize);
    }
    
    
    /* Decrement accumulator */
    
    int cbff_mpint_decrement_acc(
                                 unsigned int * acc,
                                 size_t acsize){
        return cbf_mpint_decrement_acc(
                                       acc,
                                       acsize);
    }
    
    
    /* Negate accumulator */
    
    int cbff_mpint_negate_acc(
                              unsigned int * acc,
                              size_t acsize){
        return cbf_mpint_negate_acc(
                                    acc,
                                    acsize);
    }
    
    
    /* Add to accumulator */
    
    int cbff_mpint_add_acc(
                           unsigned int * acc,
                           size_t acsize,
                           unsigned int * add,
                           size_t addsize){
        return cbf_mpint_add_acc(
                                 acc,
                                 acsize,
                                 add,
                                 addsize);
    }
    
    
    /* Shift accumulator right */
    
    int cbff_mpint_rightshift_acc(
                                  unsigned int * acc,
                                  size_t acsize,
                                  int shift){
        return cbf_mpint_rightshift_acc(
                                        acc,
                                        acsize,
                                        shift);
    }
    
    
    /* Shift accumulator left */
    
    int cbff_mpint_leftshift_acc(
                                 unsigned int * acc,
                                 size_t acsize,
                                 int shift){
        return cbf_mpint_leftshift_acc(
                                       acc,
                                       acsize,
                                       shift);
    }
    
    
    
    /* Check value of type validity */
    
    int cbff_check_type_contents(
                                 const char * type,
                                 const char * value){
        return cbf_check_type_contents(
                                       type,
                                       value);
    }
    
    
    /* Regex Match function */
    
    int cbff_match(
                   const char * string,
                   char * pattern){
        return cbf_match(
                         string,
                         pattern);
    }
    
    /* Read a template file */
    
    int cbff_read_template(
                           size_t CBFFhandle,
                           size_t CBFFstream){
        return cbf_read_template(
                                 cbff_cbf_handle(CBFFhandle),
                                 cbff_file_handle(CBFFstream));
    }
    
    
    
    /* Get the diffrn.id entry */
    
    int cbff_get_diffrn_id(
                           size_t CBFFhandle,
                           char * copy_diffrn_id, size_t start_diffrn_id, size_t end_diffrn_id, int * status_diffrn_id){
        const char * diffrn_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_diffrn_id(
                                      cbff_cbf_handle(CBFFhandle),
                                      &diffrn_id);
        if (diffrn_id) {
            length = strlen(diffrn_id);
            for (index = 0; index < length-start_diffrn_id+1
                 && index < end_diffrn_id-start_diffrn_id+1; index++) {
                copy_diffrn_id[index] = diffrn_id[index+start_diffrn_id-1];
            }
            if (index < end_diffrn_id-start_diffrn_id+1) {
                for (; index < end_diffrn_id-start_diffrn_id+1; index++) {
                    copy_diffrn_id[index] = ' ';
                }
                *status_diffrn_id = 0; /* transfer complete */
            } else {
                if (length > end_diffrn_id) {
                    *status_diffrn_id = 1; /* more to transfer */
                } else {
                    *status_diffrn_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_diffrn_id-start_diffrn_id+1; index++) {
                copy_diffrn_id[index] = ' ';
            }
            *status_diffrn_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Change the diffrn.id entry in all the categories */
    
    int cbff_set_diffrn_id(
                           size_t CBFFhandle,
                           const char * diffrn_id){
        return cbf_set_diffrn_id(
                                 cbff_cbf_handle(CBFFhandle),
                                 diffrn_id);
    }
    
    
    
    /* Change the diffrn.id entry, creating it if necessary */
    
    int cbff_require_diffrn_id(
                               size_t CBFFhandle,
                               char * copy_diffrn_id, size_t start_diffrn_id, size_t end_diffrn_id, int * status_diffrn_id,
                               const char * default_id){
        const char * diffrn_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_require_diffrn_id(
                                          cbff_cbf_handle(CBFFhandle),
                                          &diffrn_id,
                                          default_id);
        if (diffrn_id) {
            length = strlen(diffrn_id);
            for (index = 0; index < length-start_diffrn_id+1
                 && index < end_diffrn_id-start_diffrn_id+1; index++) {
                copy_diffrn_id[index] = diffrn_id[index+start_diffrn_id-1];
            }
            if (index < end_diffrn_id-start_diffrn_id+1) {
                for (; index < end_diffrn_id-start_diffrn_id+1; index++) {
                    copy_diffrn_id[index] = ' ';
                }
                *status_diffrn_id = 0; /* transfer complete */
            } else {
                if (length > end_diffrn_id) {
                    *status_diffrn_id = 1; /* more to transfer */
                } else {
                    *status_diffrn_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_diffrn_id-start_diffrn_id+1; index++) {
                copy_diffrn_id[index] = ' ';
            }
            *status_diffrn_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Get the diffrn.crystal_id entry */
    
    int cbff_get_crystal_id(
                            size_t CBFFhandle,
                            char * copy_crystal_id, size_t start_crystal_id, size_t end_crystal_id, int * status_crystal_id){
        const char * crystal_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_crystal_id(
                                       cbff_cbf_handle(CBFFhandle),
                                       &crystal_id);
        if (crystal_id) {
            length = strlen(crystal_id);
            for (index = 0; index < length-start_crystal_id+1
                 && index < end_crystal_id-start_crystal_id+1; index++) {
                copy_crystal_id[index] = crystal_id[index+start_crystal_id-1];
            }
            if (index < end_crystal_id-start_crystal_id+1) {
                for (; index < end_crystal_id-start_crystal_id+1; index++) {
                    copy_crystal_id[index] = ' ';
                }
                *status_crystal_id = 0; /* transfer complete */
            } else {
                if (length > end_crystal_id) {
                    *status_crystal_id = 1; /* more to transfer */
                } else {
                    *status_crystal_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_crystal_id-start_crystal_id+1; index++) {
                copy_crystal_id[index] = ' ';
            }
            *status_crystal_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Change the diffrn.crystal_id entry */
    
    int cbff_set_crystal_id(
                            size_t CBFFhandle,
                            const char * crystal_id){
        return cbf_set_crystal_id(
                                  cbff_cbf_handle(CBFFhandle),
                                  crystal_id);
    }
    
    
    
    /* Get the wavelength */
    
    int cbff_get_wavelength(
                            size_t CBFFhandle,
                            double * wavelength){
        return cbf_get_wavelength(
                                  cbff_cbf_handle(CBFFhandle),
                                  wavelength);
    }
    
    
    
    /* Set the wavelength */
    
    int cbff_set_wavelength(
                            size_t CBFFhandle,
                            double wavelength){
        return cbf_set_wavelength(
                                  cbff_cbf_handle(CBFFhandle),
                                  wavelength);
    }
    
    
    
    /* Get the polarization */
    
    int cbff_get_polarization(
                              size_t CBFFhandle,
                              double * polarizn_source_ratio,
                              double * polarizn_source_norm){
        return cbf_get_polarization(
                                    cbff_cbf_handle(CBFFhandle),
                                    polarizn_source_ratio,
                                    polarizn_source_norm);
    }
    
    
    
    /* Set the polarization */
    
    int cbff_set_polarization(
                              size_t CBFFhandle,
                              double polarizn_source_ratio,
                              double polarizn_source_norm){
        return cbf_set_polarization(
                                    cbff_cbf_handle(CBFFhandle),
                                    polarizn_source_ratio,
                                    polarizn_source_norm);
    }
    
    
    
    /* Get the divergence */
    
    int cbff_get_divergence(
                            size_t CBFFhandle,
                            double * div_x_source,
                            double * div_y_source,
                            double * div_x_y_source){
        return cbf_get_divergence(
                                  cbff_cbf_handle(CBFFhandle),
                                  div_x_source,
                                  div_y_source,
                                  div_x_y_source);
    }
    
    
    
    /* Set the divergence */
    
    int cbff_set_divergence(
                            size_t CBFFhandle,
                            double div_x_source,
                            double div_y_source,
                            double div_x_y_source){
        return cbf_set_divergence(
                                  cbff_cbf_handle(CBFFhandle),
                                  div_x_source,
                                  div_y_source,
                                  div_x_y_source);
    }
    
    
    
    /* Get the number of elements */
    
    int cbff_count_elements(
                            size_t CBFFhandle,
                            unsigned int * elements){
        return cbf_count_elements(
                                  cbff_cbf_handle(CBFFhandle),
                                  elements);
    }
    
    
    
    /* Get the element id */
    
    int cbff_get_element_id(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            char * copy_element_id, size_t start_element_id, size_t end_element_id, int * status_element_id){
        const char * element_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_element_id(
                                       cbff_cbf_handle(CBFFhandle),
                                       element_number,
                                       &element_id);
        if (element_id) {
            length = strlen(element_id);
            for (index = 0; index < length-start_element_id+1
                 && index < end_element_id-start_element_id+1; index++) {
                copy_element_id[index] = element_id[index+start_element_id-1];
            }
            if (index < end_element_id-start_element_id+1) {
                for (; index < end_element_id-start_element_id+1; index++) {
                    copy_element_id[index] = ' ';
                }
                *status_element_id = 0; /* transfer complete */
            } else {
                if (length > end_element_id) {
                    *status_element_id = 1; /* more to transfer */
                } else {
                    *status_element_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_element_id-start_element_id+1; index++) {
                copy_element_id[index] = ' ';
            }
            *status_element_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Get the detector id */
    
    int cbff_get_detector_id(
                             size_t CBFFhandle,
                             unsigned int element_number,
                             char * copy_detector_id, size_t start_detector_id, size_t end_detector_id, int * status_detector_id){
        const char * detector_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_detector_id(
                                        cbff_cbf_handle(CBFFhandle),
                                        element_number,
                                        &detector_id);
        if (detector_id) {
            length = strlen(detector_id);
            for (index = 0; index < length-start_detector_id+1
                 && index < end_detector_id-start_detector_id+1; index++) {
                copy_detector_id[index] = detector_id[index+start_detector_id-1];
            }
            if (index < end_detector_id-start_detector_id+1) {
                for (; index < end_detector_id-start_detector_id+1; index++) {
                    copy_detector_id[index] = ' ';
                }
                *status_detector_id = 0; /* transfer complete */
            } else {
                if (length > end_detector_id) {
                    *status_detector_id = 1; /* more to transfer */
                } else {
                    *status_detector_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_detector_id-start_detector_id+1; index++) {
                copy_detector_id[index] = ' ';
            }
            *status_detector_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Get the array id for a given detector element */
    
    int cbff_get_array_id(
                          size_t CBFFhandle,
                          unsigned int element_number,
                          char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id){
        const char * array_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_array_id(
                                     cbff_cbf_handle(CBFFhandle),
                                     element_number,
                                     &array_id);
        if (array_id) {
            length = strlen(array_id);
            for (index = 0; index < length-start_array_id+1
                 && index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = array_id[index+start_array_id-1];
            }
            if (index < end_array_id-start_array_id+1) {
                for (; index < end_array_id-start_array_id+1; index++) {
                    copy_array_id[index] = ' ';
                }
                *status_array_id = 0; /* transfer complete */
            } else {
                if (length > end_array_id) {
                    *status_array_id = 1; /* more to transfer */
                } else {
                    *status_array_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = ' ';
            }
            *status_array_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
    /* Get the pixel size of a detector element in a given direction */
    
    int cbff_get_pixel_size(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            int axis_number,
                            double * psize){
        return cbf_get_pixel_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  element_number,
                                  axis_number,
                                  psize);
    }
    
    int cbff_get_pixel_size_fs(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double * psize){
        return cbf_get_pixel_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  element_number,
                                  -axis_number,
                                  psize);
    }
    
    int cbff_get_pixel_size_sf(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double * psize){
        return cbf_get_pixel_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  element_number,
                                  -axis_number,
                                  psize);
    }
    
    
    
    /* Set the pixel size of a detector element in a given direction */
    
    int cbff_set_pixel_size(
                            size_t CBFFhandle,
                            unsigned int element_number,
                            int axis_number,
                            double psize){
        return cbf_set_pixel_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  element_number,
                                  axis_number,
                                  psize);
    }
    
    int cbff_set_pixel_size_fs(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double psize){
        return cbf_set_pixel_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  element_number,
                                  -axis_number,
                                  psize);
    }
    
    int cbff_set_pixel_size_sf(
                               size_t CBFFhandle,
                               unsigned int element_number,
                               int axis_number,
                               double psize){
        return cbf_set_pixel_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  element_number,
                                  axis_number,
                                  psize);
    }
    
    
    /* Get the gain of a detector element */
    
    int cbff_get_gain(
                      size_t CBFFhandle,
                      unsigned int element_number,
                      double * gain,
                      double * gain_esd){
        return cbf_get_gain(
                            cbff_cbf_handle(CBFFhandle),
                            element_number,
                            gain,
                            gain_esd);
    }
    
    
    
    /* Set the gain of a detector element */
    
    int cbff_set_gain(
                      size_t CBFFhandle,
                      unsigned int element_number,
                      double gain,
                      double gain_esd){
        return cbf_set_gain(
                            cbff_cbf_handle(CBFFhandle),
                            element_number,
                            gain,
                            gain_esd);
    }
    
    
    
    /* Get the bin sizes of a detector element */
    
    int cbff_get_bin_sizes(
                           size_t CBFFhandle,
                           unsigned int element_number,
                           double * slowbinsize,
                           double * fastbinsize){
        return cbf_get_bin_sizes(
                                 cbff_cbf_handle(CBFFhandle),
                                 element_number,
                                 slowbinsize,
                                 fastbinsize);
    }
    
    
    /* Set the bin sizes of a detector element */
    
    int cbff_set_bin_sizes(
                           size_t CBFFhandle,
                           unsigned int element_number,
                           double slowbinsize,
                           double fastbinsize){
        return cbf_set_bin_sizes(
                                 cbff_cbf_handle(CBFFhandle),
                                 element_number,
                                 slowbinsize,
                                 fastbinsize);
    }
    
    
    
    /* Get the overload value of a detector element */
    
    int cbff_get_overload(
                          size_t CBFFhandle,
                          unsigned int element_number,
                          double * overload){
        return cbf_get_overload(
                                cbff_cbf_handle(CBFFhandle),
                                element_number,
                                overload);
    }
    
    
    
    /* Set the overload value of a detector element */
    
    int cbff_set_overload(
                          size_t CBFFhandle,
                          unsigned int element_number,
                          double overload){
        return cbf_set_overload(
                                cbff_cbf_handle(CBFFhandle),
                                element_number,
                                overload);
    }
    
    
    
    /* Get the integration time */
    
    int cbff_get_integration_time(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  double * time){
        return cbf_get_integration_time(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        time);
    }
    
    
    
    /* Set the integration time */
    
    int cbff_set_integration_time(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  double time){
        return cbf_set_integration_time(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        time);
    }
    
    
    
    /* Convert gregorian to julian date (in days) */
    
    double cbff_gregorian_julian (int    year,
                                  int    month,
                                  int    day,
                                  int    hour,
                                  int    minute,
                                  double second) {
        return cbf_gregorian_julian (year,
                                     month,
                                     day,
                                     hour,
                                     minute,
                                     second);
    }
    
    
    /* Get the collection date and time (1) as seconds since January 1 1970 */
    
    int cbff_get_timestamp(
                           size_t CBFFhandle,
                           unsigned int reserved,
                           double * time,
                           int * timezone){
        return cbf_get_timestamp(
                                 cbff_cbf_handle(CBFFhandle),
                                 reserved,
                                 time,
                                 timezone);
    }
    
    
    
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
                           int * timezone){
        return cbf_get_datestamp(
                                 cbff_cbf_handle(CBFFhandle),
                                 reserved,
                                 year,
                                 month,
                                 day,
                                 hour,
                                 minute,
                                 second,
                                 timezone);
    }
    
    
    
    /* Set the collection date and time (1) as seconds since January 1 1970 */
    
    int cbff_set_timestamp(
                           size_t CBFFhandle,
                           unsigned int reserved,
                           double time,
                           int timezone,
                           double precision){
        return cbf_set_timestamp(
                                 cbff_cbf_handle(CBFFhandle),
                                 reserved,
                                 time,
                                 timezone,
                                 precision);
    }
    
    
    
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
                           double precision){
        return cbf_set_datestamp(
                                 cbff_cbf_handle(CBFFhandle),
                                 reserved,
                                 year,
                                 month,
                                 day,
                                 hour,
                                 minute,
                                 second,
                                 timezone,
                                 precision);
    }
    
    
    
    /* Set the collection date and time (3) as current time to the second */
    
    int cbff_set_current_timestamp(
                                   size_t CBFFhandle,
                                   unsigned int reserved,
                                   int timezone){
        return cbf_set_current_timestamp(
                                         cbff_cbf_handle(CBFFhandle),
                                         reserved,
                                         timezone);
    }
    
    
    
    /* Get the image size */
    
    int cbff_get_image_size(
                            size_t CBFFhandle,
                            unsigned int reserved,
                            unsigned int element_number,
                            size_t * ndimslow,
                            size_t * ndimfast){
        return cbf_get_image_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  ndimslow,
                                  ndimfast);
    }
    
    int cbff_get_image_size_fs(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               size_t * ndimfast,
                               size_t * ndimslow){
        return cbf_get_image_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  ndimslow,
                                  ndimfast);
    }
    
    int cbff_get_image_size_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               size_t * ndimslow,
                               size_t * ndimfast){
        return cbf_get_image_size(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  ndimslow,
                                  ndimfast);
    }
    
    
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
                       size_t ndimfast){
        return cbf_get_image(
                             cbff_cbf_handle(CBFFhandle),
                             reserved,
                             element_number,
                             array,
                             elsize,
                             elsign,
                             ndimslow,
                             ndimfast);
    }
    
    int cbff_get_image_fs(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimfast,
                          size_t ndimslow){
        return cbf_get_image(
                             cbff_cbf_handle(CBFFhandle),
                             reserved,
                             element_number,
                             array,
                             elsize,
                             elsign,
                             ndimslow,
                             ndimfast);
    }
    
    int cbff_get_image_sf(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimfast){
        return cbf_get_image(
                             cbff_cbf_handle(CBFFhandle),
                             reserved,
                             element_number,
                             array,
                             elsize,
                             elsign,
                             ndimslow,
                             ndimfast);
    }
    
    
    /* Read a binary section into a real image.  ndimslow is the 
     slow dimension, ndimfast is fast dimension.*/
    
    int cbff_get_real_image(
                            size_t CBFFhandle,
                            unsigned int reserved,
                            unsigned int element_number,
                            void * array,
                            size_t elsize,
                            size_t ndimslow,
                            size_t ndimfast){
        return cbf_get_real_image(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  array,
                                  elsize,
                                  ndimslow,
                                  ndimfast);
    }
    
    int cbff_get_real_image_fs(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               void * array,
                               size_t elsize,
                               size_t ndimfast,
                               size_t ndimslow){
        return cbf_get_real_image(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  array,
                                  elsize,
                                  ndimslow,
                                  ndimfast);
    }
    
    int cbff_get_real_image_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               void * array,
                               size_t elsize,
                               size_t ndimslow,
                               size_t ndimfast){
        return cbf_get_real_image(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  array,
                                  elsize,
                                  ndimslow,
                                  ndimfast);
    }
    
    
    /* Get the 3D image size. ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_3d_image_size(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               size_t * ndimslow,
                               size_t * ndimmid,
                               size_t * ndimfast){
        return cbf_get_3d_image_size(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    int cbff_get_3d_image_size_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  size_t * ndimfast,
                                  size_t * ndimmid,
                                  size_t * ndimslow){
        return cbf_get_3d_image_size(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    int cbff_get_3d_image_size_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  size_t * ndimslow,
                                  size_t * ndimmid,
                                  size_t * ndimfast){
        return cbf_get_3d_image_size(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
    
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
                          size_t ndimfast){
        return cbf_get_3d_image(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                element_number,
                                array,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
    int cbff_get_3d_image_fs(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             unsigned int element_number,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimfast,
                             size_t ndimmid,
                             size_t ndimslow){
        return cbf_get_3d_image(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                element_number,
                                array,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
    int cbff_get_3d_image_sf(
                             size_t CBFFhandle,
                             unsigned int reserved,
                             unsigned int element_number,
                             void * array,
                             size_t elsize,
                             int elsign,
                             size_t ndimslow,
                             size_t ndimmid,
                             size_t ndimfast){
        return cbf_get_3d_image(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                element_number,
                                array,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
    
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
                               size_t ndimfast){
        return cbf_get_real_3d_image(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     array,
                                     elsize,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
    int cbff_get_real_3d_image_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimfast,
                                  size_t ndimmid,
                                  size_t ndimslow){
        return cbf_get_real_3d_image(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     array,
                                     elsize,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    int cbff_get_real_3d_image_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast){
        return cbf_get_real_3d_image(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     array,
                                     elsize,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
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
                       size_t ndimfast){
        return cbf_set_image(
                             cbff_cbf_handle(CBFFhandle),
                             reserved,
                             element_number,
                             compression,
                             array,
                             elsize,
                             elsign,
                             ndimslow,
                             ndimfast);
    }
    
    int cbff_set_image_fs(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          unsigned int compression,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimfast,
                          size_t ndimslow){
        return cbf_set_image(
                             cbff_cbf_handle(CBFFhandle),
                             reserved,
                             element_number,
                             compression,
                             array,
                             elsize,
                             elsign,
                             ndimslow,
                             ndimfast);
    }
    
    int cbff_set_image_sf(
                          size_t CBFFhandle,
                          unsigned int reserved,
                          unsigned int element_number,
                          unsigned int compression,
                          void * array,
                          size_t elsize,
                          int elsign,
                          size_t ndimslow,
                          size_t ndimfast){
        return cbf_set_image(
                             cbff_cbf_handle(CBFFhandle),
                             reserved,
                             element_number,
                             compression,
                             array,
                             elsize,
                             elsign,
                             ndimslow,
                             ndimfast);
    }
    
    
    /* Save a real image.  ndimslow is the slow dimension, ndimfast is fast. */
    
    int cbff_set_real_image(
                            size_t CBFFhandle,
                            unsigned int reserved,
                            unsigned int element_number,
                            unsigned int compression,
                            void * array,
                            size_t elsize,
                            size_t ndimslow,
                            size_t ndimfast){
        return cbf_set_real_image(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  compression,
                                  array,
                                  elsize,
                                  ndimslow,
                                  ndimfast);
    }
    
    int cbff_set_real_image_fs(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               unsigned int compression,
                               void * array,
                               size_t elsize,
                               size_t ndimfast,
                               size_t ndimslow){
        return cbf_set_real_image(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  compression,
                                  array,
                                  elsize,
                                  ndimslow,
                                  ndimfast);
    }
    
    int cbff_set_real_image_sf(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               unsigned int element_number,
                               unsigned int compression,
                               void * array,
                               size_t elsize,
                               size_t ndimslow,
                               size_t ndimfast){
        return cbf_set_real_image(
                                  cbff_cbf_handle(CBFFhandle),
                                  reserved,
                                  element_number,
                                  compression,
                                  array,
                                  elsize,
                                  ndimslow,
                                  ndimfast);
    }
    
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
                          size_t ndimfast){
        return cbf_set_3d_image(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                element_number,
                                compression,
                                array,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
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
                             size_t ndimslow){
        return cbf_set_3d_image(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                element_number,
                                compression,
                                array,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
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
                             size_t ndimfast){
        return cbf_set_3d_image(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                element_number,
                                compression,
                                array,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
    
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
                               size_t ndimfast){
        return cbf_set_real_3d_image(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     compression,
                                     array,
                                     elsize,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    int cbff_set_real_3d_image_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  unsigned int compression,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimfast,
                                  size_t ndimmid,
                                  size_t ndimslow){
        return cbf_set_real_3d_image(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     compression,
                                     array,
                                     elsize,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    int cbff_set_real_3d_image_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  unsigned int element_number,
                                  unsigned int compression,
                                  void * array,
                                  size_t elsize,
                                  size_t ndimslow,
                                  size_t ndimmid,
                                  size_t ndimfast){
        return cbf_set_real_3d_image(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     element_number,
                                     compression,
                                     array,
                                     elsize,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
    
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
                              size_t ndimfast){
        const char * array_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_map_array_id(
                                         cbff_cbf_handle(CBFFhandle),
                                         reserved,
                                         segment_id,
                                         &array_id,
                                         ismask,
                                         require,
                                         ndimslow,
                                         ndimmid,
                                         ndimfast);
        if (array_id) {
            length = strlen(array_id);
            for (index = 0; index < length-start_array_id+1
                 && index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = array_id[index+start_array_id-1];
            }
            if (index < end_array_id-start_array_id+1) {
                for (; index < end_array_id-start_array_id+1; index++) {
                    copy_array_id[index] = ' ';
                }
                *status_array_id = 0; /* transfer complete */
            } else {
                if (length > end_array_id) {
                    *status_array_id = 1; /* more to transfer */
                } else {
                    *status_array_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = ' ';
            }
            *status_array_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    int cbff_get_map_array_id_fs(
                                 size_t CBFFhandle,
                                 unsigned int reserved,
                                 const char * segment_id,
                                 char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id,
                                 int ismask,
                                 int require,
                                 size_t ndimfast,
                                 size_t ndimmid,
                                 size_t ndimslow){
        const char * array_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_map_array_id(
                                         cbff_cbf_handle(CBFFhandle),
                                         reserved,
                                         segment_id,
                                         &array_id,
                                         ismask,
                                         require,
                                         ndimslow,
                                         ndimmid,
                                         ndimfast);
        if (array_id) {
            length = strlen(array_id);
            for (index = 0; index < length-start_array_id+1
                 && index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = array_id[index+start_array_id-1];
            }
            if (index < end_array_id-start_array_id+1) {
                for (; index < end_array_id-start_array_id+1; index++) {
                    copy_array_id[index] = ' ';
                }
                *status_array_id = 0; /* transfer complete */
            } else {
                if (length > end_array_id) {
                    *status_array_id = 1; /* more to transfer */
                } else {
                    *status_array_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = ' ';
            }
            *status_array_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    int cbff_get_map_array_id_sf(
                                 size_t CBFFhandle,
                                 unsigned int reserved,
                                 const char * segment_id,
                                 char * copy_array_id, size_t start_array_id, size_t end_array_id, int * status_array_id,
                                 int ismask,
                                 int require,
                                 size_t ndimslow,
                                 size_t ndimmid,
                                 size_t ndimfast){
        const char * array_id;
        int index;
        int length;
        int errorcode;
        errorcode = cbf_get_map_array_id(
                                         cbff_cbf_handle(CBFFhandle),
                                         reserved,
                                         segment_id,
                                         &array_id,
                                         ismask,
                                         require,
                                         ndimslow,
                                         ndimmid,
                                         ndimfast);
        if (array_id) {
            length = strlen(array_id);
            for (index = 0; index < length-start_array_id+1
                 && index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = array_id[index+start_array_id-1];
            }
            if (index < end_array_id-start_array_id+1) {
                for (; index < end_array_id-start_array_id+1; index++) {
                    copy_array_id[index] = ' ';
                }
                *status_array_id = 0; /* transfer complete */
            } else {
                if (length > end_array_id) {
                    *status_array_id = 1; /* more to transfer */
                } else {
                    *status_array_id = 0; /* transfer complete */
                }
            }
        } else {
            for (index = 0; index < end_array_id-start_array_id+1; index++) {
                copy_array_id[index] = ' ';
            }
            *status_array_id = -1; /* null string case */
        }
        return errorcode;
    }
    
    
    
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
                                  size_t * ndimfast){
        return cbf_get_map_segment_size(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
    int cbff_get_map_segment_size_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     size_t * ndimfast,
                                     size_t * ndimmid,
                                     size_t * ndimslow){
        return cbf_get_map_segment_size(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
    int cbff_get_map_segment_size_sf(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     size_t * ndimslow,
                                     size_t * ndimmid,
                                     size_t * ndimfast){
        return cbf_get_map_segment_size(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
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
                             size_t ndimfast){
        return cbf_get_map_segment(
                                   cbff_cbf_handle(CBFFhandle),
                                   reserved,
                                   segment_id,
                                   binary_id,
                                   array,
                                   elsize,
                                   elsign,
                                   ndimslow,
                                   ndimmid,
                                   ndimfast);
    }
    
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
                                size_t ndimslow){
        return cbf_get_map_segment(
                                   cbff_cbf_handle(CBFFhandle),
                                   reserved,
                                   segment_id,
                                   binary_id,
                                   array,
                                   elsize,
                                   elsign,
                                   ndimslow,
                                   ndimmid,
                                   ndimfast);
    }
    
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
                                size_t ndimfast){
        return cbf_get_map_segment(
                                   cbff_cbf_handle(CBFFhandle),
                                   reserved,
                                   segment_id,
                                   binary_id,
                                   array,
                                   elsize,
                                   elsign,
                                   ndimslow,
                                   ndimmid,
                                   ndimfast);
    }
    
    
    
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
                                  size_t ndimfast){
        return cbf_get_map_segment_mask(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        array,
                                        elsize,
                                        elsign,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
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
                                     size_t ndimslow){
        return cbf_get_map_segment_mask(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        array,
                                        elsize,
                                        elsign,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
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
                                     size_t ndimfast){
        return cbf_get_map_segment_mask(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        array,
                                        elsize,
                                        elsign,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
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
                                  size_t ndimfast){
        return cbf_get_real_map_segment(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        array,
                                        elsize,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    int cbff_get_real_map_segment_fs(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     void * array,
                                     size_t elsize,
                                     size_t ndimfast,
                                     size_t ndimmid,
                                     size_t ndimslow){
        return cbf_get_real_map_segment(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        array,
                                        elsize,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    int cbff_get_real_map_segment_sf(
                                     size_t CBFFhandle,
                                     unsigned int reserved,
                                     const char * segment_id,
                                     int * binary_id,
                                     void * array,
                                     size_t elsize,
                                     size_t ndimslow,
                                     size_t ndimmid,
                                     size_t ndimfast){
        return cbf_get_real_map_segment(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        array,
                                        elsize,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
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
                                       size_t ndimfast){
        return cbf_get_real_map_segment_mask(
                                             cbff_cbf_handle(CBFFhandle),
                                             reserved,
                                             segment_id,
                                             binary_id,
                                             array,
                                             elsize,
                                             ndimslow,
                                             ndimmid,
                                             ndimfast);
    }
    
    
    int cbff_get_real_map_segment_mask_fs(
                                          size_t CBFFhandle,
                                          unsigned int reserved,
                                          const char * segment_id,
                                          int * binary_id,
                                          void * array,
                                          size_t elsize,
                                          size_t ndimfast,
                                          size_t ndimmid,
                                          size_t ndimslow){
        return cbf_get_real_map_segment_mask(
                                             cbff_cbf_handle(CBFFhandle),
                                             reserved,
                                             segment_id,
                                             binary_id,
                                             array,
                                             elsize,
                                             ndimslow,
                                             ndimmid,
                                             ndimfast);
    }
    
    int cbff_get_real_map_segment_mask_sf(
                                          size_t CBFFhandle,
                                          unsigned int reserved,
                                          const char * segment_id,
                                          int * binary_id,
                                          void * array,
                                          size_t elsize,
                                          size_t ndimslow,
                                          size_t ndimmid,
                                          size_t ndimfast){
        return cbf_get_real_map_segment_mask(
                                             cbff_cbf_handle(CBFFhandle),
                                             reserved,
                                             segment_id,
                                             binary_id,
                                             array,
                                             elsize,
                                             ndimslow,
                                             ndimmid,
                                             ndimfast);
    }
    
    
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
                             size_t ndimfast){
        return cbf_set_map_segment(
                                   cbff_cbf_handle(CBFFhandle),
                                   reserved,
                                   segment_id,
                                   binary_id,
                                   compression,
                                   array,
                                   elsize,
                                   elsign,
                                   ndimslow,
                                   ndimmid,
                                   ndimfast);
    }
    
    
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
                                size_t ndimslow){
        return cbf_set_map_segment(
                                   cbff_cbf_handle(CBFFhandle),
                                   reserved,
                                   segment_id,
                                   binary_id,
                                   compression,
                                   array,
                                   elsize,
                                   elsign,
                                   ndimslow,
                                   ndimmid,
                                   ndimfast);
    }
    
    
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
                                size_t ndimfast){
        return cbf_set_map_segment(
                                   cbff_cbf_handle(CBFFhandle),
                                   reserved,
                                   segment_id,
                                   binary_id,
                                   compression,
                                   array,
                                   elsize,
                                   elsign,
                                   ndimslow,
                                   ndimmid,
                                   ndimfast);
    }
    
    
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
                                  size_t ndimfast){
        return cbf_set_map_segment_mask(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        compression,
                                        array,
                                        elsize,
                                        elsign,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
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
                                     size_t ndimslow){
        return cbf_set_map_segment_mask(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        compression,
                                        array,
                                        elsize,
                                        elsign,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
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
                                     size_t ndimfast){
        return cbf_set_map_segment_mask(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        compression,
                                        array,
                                        elsize,
                                        elsign,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
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
                                  size_t ndimfast){
        return cbf_set_real_map_segment(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        compression,
                                        array,
                                        elsize,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
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
                                     size_t ndimslow){
        return cbf_set_real_map_segment(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        compression,
                                        array,
                                        elsize,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
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
                                     size_t ndimfast){
        return cbf_set_real_map_segment(
                                        cbff_cbf_handle(CBFFhandle),
                                        reserved,
                                        segment_id,
                                        binary_id,
                                        compression,
                                        array,
                                        elsize,
                                        ndimslow,
                                        ndimmid,
                                        ndimfast);
    }
    
    
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
                                       size_t ndimfast){
        return cbf_set_real_map_segment_mask(
                                             cbff_cbf_handle(CBFFhandle),
                                             reserved,
                                             segment_id,
                                             binary_id,
                                             compression,
                                             array,
                                             elsize,
                                             ndimslow,
                                             ndimmid,
                                             ndimfast);
    }
    
    
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
                                          size_t ndimslow){
        return cbf_set_real_map_segment_mask(
                                             cbff_cbf_handle(CBFFhandle),
                                             reserved,
                                             segment_id,
                                             binary_id,
                                             compression,
                                             array,
                                             elsize,
                                             ndimslow,
                                             ndimmid,
                                             ndimfast);
    }
    
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
                                          size_t ndimfast){
        return cbf_set_real_map_segment_mask(
                                             cbff_cbf_handle(CBFFhandle),
                                             reserved,
                                             segment_id,
                                             binary_id,
                                             compression,
                                             array,
                                             elsize,
                                             ndimslow,
                                             ndimmid,
                                             ndimfast);
    }
    
    
    /* Get the 3D array size. ndimslow is the slowest dimension, 
     ndimmid is the next faster dimension,
     ndimfast is the fastest dimension */
    
    int cbff_get_3d_array_size(
                               size_t CBFFhandle,
                               unsigned int reserved,
                               const char * array_id,
                               size_t * ndimslow,
                               size_t * ndimmid,
                               size_t * ndimfast){
        return cbf_get_3d_array_size(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     array_id,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
    int cbff_get_3d_array_size_fs(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * array_id,
                                  size_t * ndimfast,
                                  size_t * ndimmid,
                                  size_t * ndimslow){
        return cbf_get_3d_array_size(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     array_id,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
    int cbff_get_3d_array_size_sf(
                                  size_t CBFFhandle,
                                  unsigned int reserved,
                                  const char * array_id,
                                  size_t * ndimslow,
                                  size_t * ndimmid,
                                  size_t * ndimfast){
        return cbf_get_3d_array_size(
                                     cbff_cbf_handle(CBFFhandle),
                                     reserved,
                                     array_id,
                                     ndimslow,
                                     ndimmid,
                                     ndimfast);
    }
    
    
    
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
                          size_t ndimfast){
        return cbf_get_3d_array(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                array_id,
                                binary_id,
                                array,
                                eltype,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
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
                             size_t ndimslow){
        return cbf_get_3d_array(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                array_id,
                                binary_id,
                                array,
                                eltype,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
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
                             size_t ndimfast){
        return cbf_get_3d_array(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                array_id,
                                binary_id,
                                array,
                                eltype,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
    
    
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
                          size_t ndimfast){
        return cbf_set_3d_array(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                array_id,
                                binary_id,
                                compression,
                                array,
                                eltype,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
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
                             size_t ndimslow){
        return cbf_set_3d_array(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                array_id,
                                binary_id,
                                compression,
                                array,
                                eltype,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
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
                             size_t ndimfast){
        return cbf_set_3d_array(
                                cbff_cbf_handle(CBFFhandle),
                                reserved,
                                array_id,
                                binary_id,
                                compression,
                                array,
                                eltype,
                                elsize,
                                elsign,
                                ndimslow,
                                ndimmid,
                                ndimfast);
    }
    
    
    /* Get the setting of an axis */
    
    int cbff_get_axis_setting(
                              size_t CBFFhandle,
                              unsigned int reserved,
                              const char * axis_id,
                              double * start,
                              double * increment){
        return cbf_get_axis_setting(
                                    cbff_cbf_handle(CBFFhandle),
                                    reserved,
                                    axis_id,
                                    start,
                                    increment);
    }
    
    
    
    /* Get the reference setting of an axis */
    
    int cbff_get_axis_reference_setting(
                                        size_t CBFFhandle,
                                        unsigned int reserved,
                                        const char * axis_id,
                                        double * refsetting){
        return cbf_get_axis_reference_setting(
                                              cbff_cbf_handle(CBFFhandle),
                                              reserved,
                                              axis_id,
                                              refsetting);
    }
    
    
    
    /* Change the setting of an axis */
    
    int cbff_set_axis_setting(
                              size_t CBFFhandle,
                              unsigned int reserved,
                              const char * axis_id,
                              double start,
                              double increment){
        return cbf_set_axis_setting(
                                    cbff_cbf_handle(CBFFhandle),
                                    reserved,
                                    axis_id,
                                    start,
                                    increment);
    }
    
    
    
    /* Change the reference setting of an axis */
    
    int cbff_set_axis_reference_setting(
                                        size_t CBFFhandle,
                                        unsigned int reserved,
                                        const char * axis_id,
                                        double refsetting){
        return cbf_set_axis_reference_setting(
                                              cbff_cbf_handle(CBFFhandle),
                                              reserved,
                                              axis_id,
                                              refsetting);
    }
    
    
    
    /* Construct a goniometer */
    
    int cbff_construct_goniometer(
                                  size_t CBFFhandle,
                                  size_t * CBFFgoniometer){
        int errorcode;
        cbf_goniometer goniometer;
        if (!CBFFgoniometer) return CBF_ARGUMENT;
        errorcode = cbf_construct_goniometer(cbff_cbf_handle(CBFFhandle),&goniometer);
        *CBFFgoniometer = cbff_goniometer_handle(goniometer);
        return errorcode;
    }
    
    
    
    /* Free a goniometer */
    
    int cbff_free_goniometer(
                             size_t CBFFgoniometer){
        return cbf_free_goniometer(
                                   cbff_cbf_goniometer(CBFFgoniometer));
    }
    
    
    
    /* Get the rotation axis */
    
    int cbff_get_rotation_axis(
                               size_t CBFFgoniometer,
                               unsigned int reserved,
                               double * vector1,
                               double * vector2,
                               double * vector3){
        return cbf_get_rotation_axis(
                                     cbff_cbf_goniometer(CBFFgoniometer),
                                     reserved,
                                     vector1,
                                     vector2,
                                     vector3);
    }
    
    
    
    /* Get the rotation range */
    
    int cbff_get_rotation_range(
                                size_t CBFFgoniometer,
                                unsigned int reserved,
                                double * start,
                                double * increment){
        return cbf_get_rotation_range(
                                      cbff_cbf_goniometer(CBFFgoniometer),
                                      reserved,
                                      start,
                                      increment);
    }
    
    
    
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
                           double * final3){
        return cbf_rotate_vector(
                                 cbff_cbf_goniometer(CBFFgoniometer),
                                 reserved,
                                 ratio,
                                 initial1,
                                 initial2,
                                 initial3,
                                 final1,
                                 final2,
                                 final3);
    }
    
    
    
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
                            double * reciprocal3){
        return cbf_get_reciprocal(
                                  cbff_cbf_goniometer(CBFFgoniometer),
                                  reserved,
                                  ratio,
                                  wavelength,
                                  real1,
                                  real2,
                                  real3,
                                  reciprocal1,
                                  reciprocal2,
                                  reciprocal3);
    }
    
    
    
    /* Construct a detector positioner */
    
    int cbff_construct_detector(
                                size_t CBFFhandle,
                                size_t * CBFFdetector,
                                unsigned int element_number){
        int errorcode;
        cbf_detector detector;
        if (!CBFFdetector) return CBF_ARGUMENT;
        
        errorcode = cbf_construct_detector(
                                           cbff_cbf_handle(CBFFhandle),
                                           &detector,
                                           element_number);
        *CBFFdetector = cbff_detector_handle(detector);
        return errorcode;
    }
    
    
    
    /* Construct a reference detector positioner */
    
    int cbff_construct_reference_detector(
                                          size_t CBFFhandle,
                                          size_t * CBFFdetector,
                                          unsigned int element_number){
        int errorcode;
        cbf_detector detector;
        if (!CBFFdetector) return CBF_ARGUMENT;
        errorcode = cbf_construct_reference_detector(
                                                     cbff_cbf_handle(CBFFhandle),
                                                     &detector,
                                                     element_number);
        *CBFFdetector = cbff_detector_handle(detector);
        return errorcode;
    }
    
    
    
    
    /* Construct a detector positioner, 
     creating the necessary categories, and columns */
    
    int cbff_require_detector(
                              size_t CBFFhandle,
                              size_t * CBFFdetector,
                              unsigned int element_number){
        int errorcode;
        cbf_detector detector;
        if (!CBFFdetector) return CBF_ARGUMENT;
        errorcode = cbf_require_detector(
                                         cbff_cbf_handle(CBFFhandle),
                                         & detector,
                                         element_number);
        *CBFFdetector = cbff_detector_handle(detector);
        return errorcode;
    }
    
    
    
    /* Construct a reference detector positioner, 
     creating the necessary categories, and columns */
    
    int cbff_require_reference_detector(
                                        size_t CBFFhandle,
                                        size_t * CBFFdetector,
                                        unsigned int element_number){
        int errorcode;
        cbf_detector detector;
        if (!CBFFdetector) return CBF_ARGUMENT;
        errorcode =  cbf_require_reference_detector(
                                                    cbff_cbf_handle(CBFFhandle),
                                                    & detector,
                                                    element_number);
        *CBFFdetector = cbff_detector_handle(detector);
        return errorcode;
        
    }
    
    
    
    /* Free a detector */
    
    int cbff_free_detector(
                           size_t CBFFdetector){
        return cbf_free_detector(
                                 cbff_cbf_detector_handle(CBFFdetector));
    }
    
    
    
    /* Get the beam center */
    
    int cbff_get_beam_center(
                             size_t CBFFdetector,
                             double * indexslow,
                             double * indexfast,
                             double * centerslow,
                             double * centerfast){
        return cbf_get_beam_center(
                                   cbff_cbf_detector_handle(CBFFdetector),
                                   indexslow,
                                   indexfast,
                                   centerslow,
                                   centerfast);
    }
    
    int cbff_get_beam_center_fs(
                                size_t CBFFdetector,
                                double * indexfast,
                                double * indexslow,
                                double * centerfast,
                                double * centerslow){
        return cbf_get_beam_center(
                                   cbff_cbf_detector_handle(CBFFdetector),
                                   indexslow,
                                   indexfast,
                                   centerslow,
                                   centerfast);
    }
    
    int cbff_get_beam_center_sf(
                                size_t CBFFdetector,
                                double * indexslow,
                                double * indexfast,
                                double * centerslow,
                                double * centerfast){
        return cbf_get_beam_center(
                                   cbff_cbf_detector_handle(CBFFdetector),
                                   indexslow,
                                   indexfast,
                                   centerslow,
                                   centerfast);
    }
    
    
    
    /* Set the beam center */
    
    int cbff_set_beam_center(
                             size_t CBFFdetector,
                             double * indexslow,
                             double * indexfast,
                             double * centerslow,
                             double * centerfast){
        return cbf_set_beam_center(
                                   cbff_cbf_detector_handle(CBFFdetector),
                                   indexslow,
                                   indexfast,
                                   centerslow,
                                   centerfast);
    }
    
    int cbff_set_beam_center_fs(
                                size_t CBFFdetector,
                                double * indexfast,
                                double * indexslow,
                                double * centerfast,
                                double * centerslow){
        return cbf_set_beam_center(
                                   cbff_cbf_detector_handle(CBFFdetector),
                                   indexslow,
                                   indexfast,
                                   centerslow,
                                   centerfast);
    }
    
    int cbff_set_beam_center_sf(
                                size_t CBFFdetector,
                                double * indexslow,
                                double * indexfast,
                                double * centerslow,
                                double * centerfast){
        return cbf_set_beam_center(
                                   cbff_cbf_detector_handle(CBFFdetector),
                                   indexslow,
                                   indexfast,
                                   centerslow,
                                   centerfast);
    }
    
    
    
    /* Set the reference beam center */
    
    int cbff_set_reference_beam_center(
                                       size_t CBFFdetector,
                                       double * indexslow,
                                       double * indexfast,
                                       double * centerslow,
                                       double * centerfast){
        return cbf_set_reference_beam_center(
                                             cbff_cbf_detector_handle(CBFFdetector),
                                             indexslow,
                                             indexfast,
                                             centerslow,
                                             centerfast);
    }
    
    
    int cbff_set_reference_beam_center_fs(
                                          size_t CBFFdetector,
                                          double * indexfast,
                                          double * indexslow,
                                          double * centerfast,
                                          double * centerslow){
        return cbf_set_reference_beam_center(
                                             cbff_cbf_detector_handle(CBFFdetector),
                                             indexslow,
                                             indexfast,
                                             centerslow,
                                             centerfast);
    }
    
    int cbff_set_reference_beam_center_sf(
                                          size_t CBFFdetector,
                                          double * indexslow,
                                          double * indexfast,
                                          double * centerslow,
                                          double * centerfast){
        return cbf_set_reference_beam_center(
                                             cbff_cbf_detector_handle(CBFFdetector),
                                             indexslow,
                                             indexfast,
                                             centerslow,
                                             centerfast);
    }
    
    
    
    
    /* Get the detector distance */
    
    int cbff_get_detector_distance(
                                   size_t CBFFdetector,
                                   double * distance){
        return cbf_get_detector_distance(
                                         cbff_cbf_detector_handle(CBFFdetector),
                                         distance);
    }
    
    
    
    /* Get the detector normal */
    
    int cbff_get_detector_normal(
                                 size_t CBFFdetector,
                                 double * normal1,
                                 double * normal2,
                                 double * normal3){
        return cbf_get_detector_normal(
                                       cbff_cbf_detector_handle(CBFFdetector),
                                       normal1,
                                       normal2,
                                       normal3);
    }
    
    
    
    /* Calcluate the coordinates of a pixel */
    
    int cbff_get_pixel_coordinates(
                                   size_t CBFFdetector,
                                   double indexslow,
                                   double indexfast,
                                   double * coordinate1,
                                   double * coordinate2,
                                   double * coordinate3){
        return cbf_get_pixel_coordinates(
                                         cbff_cbf_detector_handle(CBFFdetector),
                                         indexslow,
                                         indexfast,
                                         coordinate1,
                                         coordinate2,
                                         coordinate3);
    }
    
    int cbff_get_pixel_coordinates_fs(
                                      size_t CBFFdetector,
                                      double indexfast,
                                      double indexslow,
                                      double * coordinate1,
                                      double * coordinate2,
                                      double * coordinate3){
        return cbf_get_pixel_coordinates(
                                         cbff_cbf_detector_handle(CBFFdetector),
                                         indexslow,
                                         indexfast,
                                         coordinate1,
                                         coordinate2,
                                         coordinate3);
    }
    
    int cbff_get_pixel_coordinates_sf(
                                      size_t CBFFdetector,
                                      double indexslow,
                                      double indexfast,
                                      double * coordinate1,
                                      double * coordinate2,
                                      double * coordinate3){
        return cbf_get_pixel_coordinates(
                                         cbff_cbf_detector_handle(CBFFdetector),
                                         indexslow,
                                         indexfast,
                                         coordinate1,
                                         coordinate2,
                                         coordinate3);
    }
    
    
    
    
    /* Get the pixel normal */
    
    int cbff_get_pixel_normal(
                              size_t CBFFdetector,
                              double indexslow,
                              double indexfast,
                              double * normal1,
                              double * normal2,
                              double * normal3){
        return cbf_get_pixel_normal(
                                    cbff_cbf_detector_handle(CBFFdetector),
                                    indexslow,
                                    indexfast,
                                    normal1,
                                    normal2,
                                    normal3);
    }
    
    int cbff_get_pixel_normal_fs(
                                 size_t CBFFdetector,
                                 double indexfast,
                                 double indexslow,
                                 double * normal1,
                                 double * normal2,
                                 double * normal3){
        return cbf_get_pixel_normal(
                                    cbff_cbf_detector_handle(CBFFdetector),
                                    indexslow,
                                    indexfast,
                                    normal1,
                                    normal2,
                                    normal3);
    }
    
    int cbff_get_pixel_normal_sf(
                                 size_t CBFFdetector,
                                 double indexslow,
                                 double indexfast,
                                 double * normal1,
                                 double * normal2,
                                 double * normal3){
        return cbf_get_pixel_normal(
                                    cbff_cbf_detector_handle(CBFFdetector),
                                    indexslow,
                                    indexfast,
                                    normal1,
                                    normal2,
                                    normal3);
    }
    
    
    
    /* Calcluate the area of a pixel */
    
    int cbff_get_pixel_area(
                            size_t CBFFdetector,
                            double indexslow,
                            double indexfast,
                            double * area,
                            double * projected_area){
        return cbf_get_pixel_area(
                                  cbff_cbf_detector_handle(CBFFdetector),
                                  indexslow,
                                  indexfast,
                                  area,
                                  projected_area);
    }
    
    int cbff_get_pixel_area_fs(
                               size_t CBFFdetector,
                               double indexfast,
                               double indexslow,
                               double * area,
                               double * projected_area){
        return cbf_get_pixel_area(
                                  cbff_cbf_detector_handle(CBFFdetector),
                                  indexslow,
                                  indexfast,
                                  area,
                                  projected_area);
    }
    
    int cbff_get_pixel_area_sf(
                               size_t CBFFdetector,
                               double indexslow,
                               double indexfast,
                               double * area,
                               double * projected_area){
        return cbf_get_pixel_area(
                                  cbff_cbf_detector_handle(CBFFdetector),
                                  indexslow,
                                  indexfast,
                                  area,
                                  projected_area);
    }
    
    
    
    /* Calcluate the size of a pixel from the detector element axis displacements */
    
    int cbff_get_inferred_pixel_size(
                                     size_t CBFFdetector,
                                     int axis_number,
                                     double * psize){
        return cbf_get_inferred_pixel_size(
                                           cbff_cbf_detector_handle(CBFFdetector),
                                           axis_number,
                                           psize);
    }
    
    int cbff_get_inferred_pixel_size_fs(
                                        size_t CBFFdetector,
                                        int axis_number,
                                        double * psize){
        return cbf_get_inferred_pixel_size(
                                           cbff_cbf_detector_handle(CBFFdetector),
                                           -axis_number,
                                           psize);
    }
    
    int cbff_get_inferred_pixel_size_sf(
                                        size_t CBFFdetector,
                                        int axis_number,
                                        double * psize){
        return cbf_get_inferred_pixel_size(
                                           cbff_cbf_detector_handle(CBFFdetector),
                                           axis_number,
                                           psize);
    }
    
    
    
    /* Get the unit cell parameters */
    
    int cbff_get_unit_cell(
                           size_t CBFFhandle,
                           double cell[6],
                           double cell_esd[6]){
        return cbf_get_unit_cell(
                                 cbff_cbf_handle(CBFFhandle),
                                 cell,
                                 cell_esd);
    }
    
    
    /* Set the unit cell parameters */
    
    int cbff_set_unit_cell(
                           size_t CBFFhandle,
                           double cell[6],
                           double cell_esd[6]){
        return cbf_set_unit_cell(
                                 cbff_cbf_handle(CBFFhandle),
                                 cell,
                                 cell_esd);
    }
    
    
    /* Get the reciprocal cell parameters */
    
    int cbff_get_reciprocal_cell(
                                 size_t CBFFhandle,
                                 double cell[6],
                                 double cell_esd[6]){
        return cbf_get_reciprocal_cell(
                                       cbff_cbf_handle(CBFFhandle),
                                       cell,
                                       cell_esd);
    }
    
    
    /* Set the reciprocal cell parameters */
    
    int cbff_set_reciprocal_cell(
                                 size_t CBFFhandle,
                                 double cell[6],
                                 double cell_esd[6]){
        return cbf_set_reciprocal_cell(
                                       cbff_cbf_handle(CBFFhandle),
                                       cell,
                                       cell_esd);
    }
    
    
    /* Compute a cell volume */
    
    int cbff_compute_cell_volume(
                                 double cell[6],
                                 double * volume){
        return cbf_compute_cell_volume(
                                       cell,
                                       volume);
    }
    
    
    /* Compute a reciprocal cell */
    
    int cbff_compute_reciprocal_cell(
                                     double cell[6],
                                     double rcell[6]){
        return cbf_compute_reciprocal_cell(
                                           cell,
                                           rcell);
    }
    
    
    /* Get the orientation matrix entry */
    
    int cbff_get_orientation_matrix(
                                    size_t CBFFhandle,
                                    double ub_matrix[9]){
        return cbf_get_orientation_matrix(
                                          cbff_cbf_handle(CBFFhandle),
                                          ub_matrix);
    }
    
    
    /* Set the orientation matrix entry */
    
    int cbff_set_orientation_matrix(
                                    size_t CBFFhandle,
                                    double ub_matrix[9]){
        return cbf_set_orientation_matrix(
                                          cbff_cbf_handle(CBFFhandle),
                                          ub_matrix);
    }
    
    
    
    
#ifdef __cplusplus
    
}

#endif
