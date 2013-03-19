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

/**********************************************************************
 *                             TO DO list                             *
 * 1.  Check digest on transfers, using flags argument                *
 *     using the MSG_NODIGEST, MSG_DIGEST flags                       *
 * 2.  Fix interaction of indices with H5open/H5close                 *
 * 3.  Enable CRT_ORDER sort                                          *
 * 4.  Allow stdout output for filters                                *
 * 5.  Add NXdata and links                                           *
 *                                                                    *
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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <math.h>

	/* Attribute type definition, agrees with CBFlib data convensions */

	typedef struct cbf_name_value_pair_def
	{
		const char * name;
		const void * value;
	} cbf_name_value_pair;

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


    /* Write the HDF5 version of the NeXus axis definitions, if
     the original CBF had axis definitions */

    int cbf_write_h5nxaxes(cbf_handle handle, cbf_h5handle h5handle) {


        int errorcode;

        unsigned int rows, row;

        double matrix[3][3];

        hid_t instrumentid;

        const char* datablock;

        errorcode = 0;

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

            const char* equipment;

            const char* axis_id;

            const char* depends_on;

            const char * type;

            const char * system;

            double vector[3], offset[3];

            cbf_reportnez(cbf_find_category(handle, "axis"),errorcode);

            cbf_reportnez(cbf_find_column(handle,"id"),errorcode);

            depends_on = ".";

            equipment = "general";

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

            strcpy(nxequipment,"NX");

            strncat(nxequipment,equipment,252);

            nxequipment[255] = '\0';

            cbf_reportnez(cbf_require_nxgroup(h5handle,
                                              equipment, nxequipment,
                                              instrumentid, &equipmentid),errorcode);

            if (cbf_norm(offset) > 1.e-20) {

                char * nxaxis_offset_name;

                char * nxaxis_name;

                char * nxdepends_on_name;

                cbf_reportnez(cbf_strcat("axis_offset.",
                              axis_id,&nxaxis_offset_name),
                              errorcode);

                cbf_reportnez(cbf_strcat("axis.",
                              axis_id,&nxaxis_name),errorcode);

                cbf_reportnez(cbf_strcat("axis.",
                              depends_on,&nxdepends_on_name),errorcode);

                cbf_reportnez(cbf_require_nxgroup(h5handle,
                                                  nxaxis_offset_name,
                                                  "NXaxis",
                                                  equipmentid,
                                                  &nxaxisoffsetid),
                              errorcode);

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

                cbf_reportnez(cbf_location_string(datablock,"axis","offset",row,&cbfloc),errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisoffsetid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);

                cbf_h5reportneg(H5Gclose(nxaxisoffsetid),CBF_FORMAT,errorcode);

                cbf_reportnez(cbf_require_nxgroup(h5handle,
                                                  nxaxis_name, "NXaxis",equipmentid, &nxaxisid),errorcode);

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

                cbf_h5reportneg(H5Gclose(nxaxisoffsetid),CBF_FORMAT,errorcode);

                cbf_reportnez(cbf_free((void **)&nxaxis_offset_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxaxis_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxdepends_on_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&cbfloc,NULL),errorcode);

            } else {

                char * nxaxis_name;

                char * nxdepends_on_name;

                cbf_reportnez(cbf_strcat("axis.",
                                         axis_id,&nxaxis_name),errorcode);

                cbf_reportnez(cbf_strcat("axis.",
                                         depends_on,&nxdepends_on_name),errorcode);

                cbf_reportnez(cbf_require_nxgroup(h5handle,
                                                  nxaxis_name,
                                                  "NXaxis",
                                                  equipmentid,
                                                  &nxaxisid),
                              errorcode);

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

                cbf_reportnez(cbf_location_string(datablock,"axis","vector",row,&cbfloc),errorcode);

                errorcode |= cbf_apply_h5text_attribute(nxaxisid,
                                                        "cbf_location",
                                                        cbfloc,
                                                        errorcode);

                cbf_h5reportneg(H5Gclose(nxaxisid),CBF_FORMAT,errorcode);

                cbf_reportnez(cbf_free((void **)&nxaxis_name,NULL),errorcode);

                cbf_reportnez(cbf_free((void **)&nxdepends_on_name,NULL),errorcode);

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

        cbf_h5reportneg(H5Awrite(attribid,attribtype,
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


    /* apply an integer attribute to a group or dataset */

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

    /* Write a binary value to an HDF5 file */

    int cbf_write_h5binary (cbf_node *column, unsigned int row,
                            cbf_h5handle h5handle)
    {
        hid_t valid, valtype, valprop, valspace;

        int errorcode;

        char rownum[10];

        cbf_file *infile;

        char digest [25];

        long start;

        size_t size;

        hsize_t hsize[1];

        unsigned int compression;

        unsigned char * rawdata;

        int id, bits, sign, type, checked_digest, realarray;

        const char *byteorder;

        size_t dimover, dimfast, dimmid, dimslow;

        size_t padding;

        /* Check the arguments */

        if (!h5handle || !h5handle->hfile)

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

        /* Create treat the image as an opaque stream of size bytes */

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

         because of the file positioning done, infile->chatracters
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

        cbf_h5reportneg(H5Dwrite(valid,valtype,
                                 valspace,H5S_ALL,H5P_DEFAULT,rawdata),
                        CBF_ARGUMENT,errorcode);

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

            strcpy(target_path,"/entry/NXcbf/");

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

            return cbf_write_h5binary (column, row, h5handle);


        /* Undecoded MIME? */

        if (*text == CBF_TOKEN_MIME_BIN)
        {
            /* Convert the value to a normal binary section */

            cbf_failnez (cbf_mime_temp (column, row))

            return cbf_write_h5binary (column, row, h5handle);
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
		hid_t h5group = CBF_H5FAIL, h5subGroup = CBF_H5FAIL, h5dataset = CBF_H5FAIL;
		const cbf_name_value_pair * attrit = attrv;

		/* Assume nxid is a valid handle, need to ensure that the /entry/instrument group exists, then ensure the detector group exists */
		cbf_reportnez(cbf_require_nxgroup(h5handle, group, groupNXclass, h5handle->nxid, &h5group), error);
		cbf_reportnez(cbf_require_nxgroup(h5handle, subGroup, subGroupNXclass, h5group, &h5subGroup), error);

		/* write some data */
		cbf_reportnez(cbf_H5Dcreate_string(h5subGroup,&h5dataset,name,value),error);
		for (; attrit < attrv+attrc; ++attrit)
			cbf_reportnez(cbf_H5Aset_string(h5dataset,attrit->name,attrit->value),error);

		/*  clean up */
		if (cbf_is_valid_h5id(h5dataset)) cbf_H5Dfree(h5dataset);
		if (cbf_is_valid_h5id(h5subGroup)) cbf_H5Gfree(h5subGroup);
		if (cbf_is_valid_h5id(h5group)) cbf_H5Gfree(h5group);
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

        /* Check the arguments */

        if (!category || !h5handle || h5handle->rootid <0 || h5handle->dbid < 0)

            return CBF_ARGUMENT;


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

        /* Write the name under the opem save frame or datablock */

        if (h5handle->sfid <0) {

            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                                                     (category->name)?(category->name):"_(null)_"),
                          CBF_FORMAT);


        } else {
            cbf_h5failneg(h5handle->catid=H5Gcreatex(h5handle->dbid,
                                                     (category->name)?(category->name):"_(null)_"),
                          CBF_FORMAT);


        }

		/* Some basic code to ensure the utility of the low-level HDF5 abstraction functions.
		   This will be striped out & refactored when a suitable higher-level framework is implemented. */
		cbf_debug_print(category->name);
		if (!strcmp(category->name,"diffrn_detector"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) printf("warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!strcmp(column_node->name,"type")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("type", text+1, instGroup, instGroupClass, "detector", "NXdetector", 0, 0, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.type could not be written to nexus file");
				}
				if(!strcmp(column_node->name,"details")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("description", text+1, instGroup, instGroupClass, "detector", "NXdetector", 0, 0, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.details could not be written to nexus file");
				}
				if(!strcmp(column_node->name,"detector")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("local_name", text+1, instGroup, instGroupClass, "detector", "NXdetector", 0, 0, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.detector could not be written to nexus file");
				}
				if(!strcmp(column_node->name,"dtime")) {
					cbf_name_value_pair attr = {"units","us"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("dead_time", text+1, instGroup, instGroupClass, "detector", "NXdetector", 1, &attr, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector.dtime could not be written to nexus file");
				}
			}
		}
		if (!strcmp(category->name,"diffrn_detector_element"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) printf("warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!strcmp(column_node->name,"center[1]")){
					cbf_name_value_pair attr = {"units","mm"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					/* TODO: give the correct value here */
					cbf_reportnez(cbf_map_h5value("beam_center_x", text+1, instGroup, instGroupClass, "detector", "NXdetector", 1, &attr, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector_element.center[1] could not be written to nexus file");
				}
				if(!strcmp(column_node->name,"center[2]")){
					cbf_name_value_pair attr = {"units","mm"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					/* TODO: give the correct value here */
					cbf_reportnez(cbf_map_h5value("beam_center_y", text+1, instGroup, instGroupClass, "detector", "NXdetector", 1, &attr, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_detector_element.center[2] could not be written to nexus file");
				}
			}
		}
		if (!strcmp(category->name,"diffrn_radiation"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) printf("warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!strcmp(column_node->name,"div_x_source")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("divergence_x", text+1, instGroup, instGroupClass, "collimator", "NXcollimator", 0, 0, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation.div_x_source could not be written to nexus file");
				}
				if(!strcmp(column_node->name,"div_y_source")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("divergence_y", text+1, instGroup, instGroupClass, "collimator", "NXcollimator", 0, 0, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation.div_y_source could not be written to nexus file");
				}
			}
		}
		if (!strcmp(category->name,"diffrn_radiation_wavelength"))
		{
			for (column= 0; column < category->children; column++)
			{
				cbf_node * column_node = category->child[column];
				cbf_debug_print2("\t%s",column_node->name);
				if(column_node->children > 1) printf("warning: too many rows in '%s', only writing first row.\n",category->name);
				if(!strcmp(column_node->name,"wavelength")){
					cbf_name_value_pair attr = {"units","Angstroms"};
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("wavelength", text+1, instGroup, instGroupClass, "monochromator", "NXmonochromator", 1, &attr, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation_wavelength.wavelength could not be written to nexus file");
				}
				if(!strcmp(column_node->name,"wt")) {
					const char * text = 0;
					int error = CBF_SUCCESS;
					cbf_reportnez(cbf_get_columnrow (&text, column_node, 0),error);
					/* check 'text' is suitable for use as an ascii data value */
					cbf_reportnez(cbf_map_h5value("weight", text+1, instGroup, instGroupClass, "monochromator", "NXmonochromator", 0, 0, h5handle),error);
					if (CBF_SUCCESS != error) cbf_debug_print("diffrn_radiation_wavelength.wt could not be written to nexus file");
				}
			}
		}


        cbf_failnez(cbf_apply_h5text_attribute(h5handle->catid,
                                               "NX_class","NXcbfcat",0));


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
                                                   "NX_class","NXcbfcol",0));

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


    /* Free an H5File handle */

    int cbf_free_h5handle(cbf_h5handle h5handle) {

        void * memblock;

        memblock = (void *) h5handle;


        if (h5handle->colid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->colid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }

        if (h5handle->catid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->catid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }


        if (h5handle->sfid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->sfid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }

        if (h5handle->dbid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->dbid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }


        if (h5handle->rootid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->rootid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }

        if (h5handle->curnxid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->curnxid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }

        if (h5handle->dataid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->dataid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));


        }

        if (h5handle->nxid >= 0) {

            cbf_h5onfailneg(H5Gclose(h5handle->nxid),
                            CBF_UNDEFINED,cbf_free(&memblock,NULL));

        }

        if (h5handle->hfile >= 0) {

            cbf_h5onfailneg(H5Fclose(h5handle->hfile),
                            CBF_FILECLOSE,cbf_free(&memblock,NULL));

        }

        return cbf_free(&memblock,NULL);
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
        (*h5handle)->curnxid = (hid_t)CBF_H5FAIL;
        (*h5handle)->dataid  = (hid_t)CBF_H5FAIL;
        (*h5handle)->rwmode  = 0;

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
     Make a new group of NeXus class NXcbfsf
     in the NXcbf current datablock
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
                                               "NX_class", "NXcbfsf",0));

        return CBF_SUCCESS;
    }




    /* Write a datablock name to an HDF5 file
     Make a new group of NeXus class NXcbfdb in the NXcbf class root
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
                                               "NX_class", "NXcbfdb",0));

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

    int cbf_create_h5handle(cbf_h5handle *h5handle,
                            const char * h5filename) {


        hid_t fcreate_prop_list;

        cbf_failnez(cbf_make_h5handle(h5handle));

        cbf_h5onfailneg(fcreate_prop_list = H5Pcreate(H5P_FILE_ACCESS),
                        CBF_ALLOC,cbf_free((void**) h5handle, NULL));

        (*h5handle)->rwmode = 1;

        cbf_h5onfailneg(H5Pset_fclose_degree(fcreate_prop_list,H5F_CLOSE_STRONG),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_h5onfailneg((*h5handle)->hfile = H5Fcreate(h5filename,
                                                       H5F_ACC_TRUNC, H5P_DEFAULT,fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_h5onfailneg(H5Pclose(fcreate_prop_list),
                        CBF_ARGUMENT,cbf_free((void**) h5handle, NULL));

        cbf_onfailnez(cbf_H5Gcreate_in_handle(*h5handle,"NXcbf",&((*h5handle)->rootid)),
                      cbf_free_h5handle(*h5handle));

        cbf_failnez(cbf_apply_h5text_attribute((*h5handle)->rootid,
                                               "NX_class","NXcbf",0));


        return CBF_SUCCESS;

    }


    /*  Write cbf to HDF5 file hfile */

    int cbf_write_h5file (cbf_handle handle, cbf_h5handle h5handle, int flags)
    {
        cbf_node *node;

        int errorcode;

        if (!handle || !h5handle)

            return CBF_ARGUMENT;

        /* Transfer the flags into h5handle */

        h5handle->flags = flags;

        /* Find the root node */

        cbf_failnez (cbf_find_parent (&node, handle->node, CBF_ROOT))

        /* Reset the reference counts */

        cbf_failnez( cbf_reset_refcounts(handle->dictionary) )

        /* Write the file */

        errorcode = cbf_write_h5node (handle, node, h5handle);

        if (!errorcode) {


            cbf_write_h5nxaxes(handle, h5handle);


        }

        return errorcode;

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

        H5T_class_t native_type_class;

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

                type_size = H5Tget_size(type);

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

        native_type = H5Tget_native_type(type,H5T_DIR_ASCEND);

        native_type_class = H5Tget_class(native_type);

        if(total_size > 0) {

            if(readattrib) {

                /* Process an attiribute */

                cbf_reportnez(cbf_alloc(((void **) value),NULL,
                                        total_size+1,1),errorcode);

                cbf_h5failneg(H5Aread(obj_id,native_type,(void *)*value),
                              CBF_ARGUMENT);

                (*((char **)value))[total_size]='\0';

                if (type_class==H5T_STRING &&(native_type_class==H5T_NATIVE_CHAR
                                              || native_type_class == H5T_STRING)) {

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

                        if (native_type_class == H5T_NATIVE_CHAR&&sign) xdata = **((signed char **)value);
                        if (native_type_class == H5T_NATIVE_CHAR&&!sign) uxdata = **((unsigned char **)value);
                        if (native_type_class == H5T_NATIVE_SCHAR) xdata = **((signed char **)value);
                        if (native_type_class == H5T_NATIVE_UCHAR) uxdata = **((unsigned char **)value);
                        if (native_type_class == H5T_NATIVE_SHORT) xdata = **((unsigned short **)value);
                        if (native_type_class == H5T_NATIVE_USHORT) uxdata = **((unsigned short **)value);
                        if (native_type_class == H5T_NATIVE_INT) xdata = **((int **)value);
                        if (native_type_class == H5T_NATIVE_UINT) uxdata = **((unsigned int **)value);
                        if (native_type_class == H5T_NATIVE_LONG) xdata = **((long **)value);
                        if (native_type_class == H5T_NATIVE_ULONG) uxdata = **((unsigned long **)value);

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

                                    if (native_type_class == H5T_NATIVE_CHAR&&sign) xdata = ((*(signed char **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_CHAR&&!sign) uxdata = ((*(unsigned char **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_SCHAR) xdata = ((*(signed char **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_UCHAR) uxdata = ((*(unsigned char **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_SHORT) xdata = ((*(unsigned short **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_USHORT) uxdata = ((*(unsigned short **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_INT) xdata = ((*(int **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_UINT) uxdata = ((*(unsigned int **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_LONG) xdata = ((*(long **)value)[master_index]);
                                    if (native_type_class == H5T_NATIVE_ULONG) uxdata = ((*(unsigned long **)value)[master_index]);


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

                        if (native_type_class == H5T_NATIVE_FLOAT) {

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

                                    if (native_type_class == H5T_NATIVE_FLOAT) {

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

                if (type_class==H5T_STRING&& (native_type_class==H5T_NATIVE_CHAR
                                              || native_type_class == H5T_STRING)) {

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

                        if (native_type_class == H5T_NATIVE_CHAR&&sign) xdata = *((signed char *)data);
                        if (native_type_class == H5T_NATIVE_CHAR&&!sign) uxdata = *((unsigned char *)data);
                        if (native_type_class == H5T_NATIVE_SCHAR) xdata = *((signed char *)data);
                        if (native_type_class == H5T_NATIVE_UCHAR) uxdata = *((unsigned char *)data);
                        if (native_type_class == H5T_NATIVE_SHORT) xdata = *((unsigned short *)data);
                        if (native_type_class == H5T_NATIVE_USHORT) uxdata = *((unsigned short *)data);
                        if (native_type_class == H5T_NATIVE_INT) xdata = *((int *)data);
                        if (native_type_class == H5T_NATIVE_UINT) uxdata = *((unsigned int *)data);
                        if (native_type_class == H5T_NATIVE_LONG) xdata = *((long *)data);
                        if (native_type_class == H5T_NATIVE_ULONG) uxdata = *((unsigned long *)data);

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

                                    if (native_type_class == H5T_NATIVE_CHAR&&sign) xdata = (((signed char *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_CHAR&&!sign) uxdata = (((unsigned char *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_SCHAR) xdata = (((signed char *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_UCHAR) uxdata = (((unsigned char *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_SHORT) xdata = (((unsigned short *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_USHORT) uxdata = (((unsigned short *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_INT) xdata = (((int *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_UINT) uxdata = (((unsigned int *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_LONG) xdata = (((long *)data)[master_index]);
                                    if (native_type_class == H5T_NATIVE_ULONG) uxdata = (((unsigned long *)data)[master_index]);


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

                        if (native_type_class == H5T_NATIVE_FLOAT) {

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

                                    if (native_type_class == H5T_NATIVE_FLOAT) {

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

                        if (!cbf_cistrcmp(value,"NXcbf")) {

                            ((cbf_h5Ovisithandle)op_data)->incbf = 1;

                        }

                        if (!cbf_cistrcmp(value,"NXcbfdb")) {

                            ((cbf_h5Ovisithandle)op_data)->incbfdb = 1;

                            cbf_get_bookmark(handle,&bookmark);

                            if (cbf_find_datablock(handle,name)) {

                                cbf_reportnez(cbf_new_datablock(handle,name),errorcode);

                            }

                            cbf_get_bookmark(handle,
                                             &(((cbf_h5Ovisithandle)op_data)->bookmark));

                            cbf_goto_bookmark(handle,bookmark);

                        }

                        if (!cbf_cistrcmp(value,"NXcbfcat")&& saved_bookmark.datablock) {

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

                        if (!cbf_cistrcmp(value,"NXcbfcol")&& saved_bookmark.category) {

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
                        } else if (!cbf_cistrcmp(attrib_name,"digest")) {
                            strcpy(digest,value);
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

                dataset_ds = H5Dget_space(dataset_id);
                dataset_type = H5Dget_type(dataset_id);

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

    int cbf_read_h5file(cbf_handle handle, cbf_h5handle h5handle, int flags) {

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

    /* get a bookmark from the current information in a cbf handle */

    int cbf_get_bookmark(cbf_handle handle, cbf_bookmark * bookmark) {

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

    int cbf_goto_bookmark(cbf_handle handle, cbf_bookmark bookmark) {

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

	/* basic check to find out if a group is valid, without remembering what the test actually is */
	int cbf_is_valid_h5id(const hid_t ID) {return ID >= 0;}

	/* find/create/free a HDF5 group if it's valid & possibly set the ID to an invalid identifier
	can write requireGroup function as {if (!find(group)) create(group); return group;} */

	int cbf_H5Gcreate(hid_t * const group, const char * const name, const hid_t parent)
	{
		if (!group || !name || !cbf_is_valid_h5id(parent)) return CBF_ARGUMENT;
		return cbf_is_valid_h5id(*group = H5Gcreate2(parent,name,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT)) ? CBF_SUCCESS : CBF_H5ERROR;
	}

	int cbf_H5Gfree(const hid_t ID)
	{
		return H5Gclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
	}

	int cbf_H5Gdelete(hid_t * const ID)
	{
		const int err = cbf_H5Gfree(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}

	/* Open/close a HDF5 file if it's valid & possibly set the ID to an invalid identifier - deliberately avoid find/create/free or get/set/clear naming convensions */

	int cbf_H5Fopen(hid_t * const file, const char * const name)
	{
		/* define variables & check args */
		int error = (!file || !name) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);

		/* check variables */
		cbf_h5reportneg(fapl, CBF_H5ERROR, error);

		/* do some work */
		cbf_h5reportneg(H5Pset_fclose_degree(fapl,H5F_CLOSE_STRONG), CBF_H5ERROR, error);
		cbf_h5reportneg(*file = H5Fcreate(name,H5F_ACC_TRUNC,H5P_DEFAULT,fapl), CBF_H5ERROR, error);

		/* ensure variables are properly closed */
		if (cbf_is_valid_h5id(fapl)) H5Pclose(fapl);

		/* done */
		return error;
	}

	int cbf_H5Fclose(const hid_t ID)
	{
		return H5Fclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
	}

	/* This needs a better name - delete is misleading */
	int cbf_H5Fdelete(hid_t * const ID)
	{
		const int err = cbf_H5Fclose(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}

	/* get/set(/clear?) a HDF5 variable length string attribute, don't attempt to change any attribute's value - avoid find/create/free naming convension */

	/* get the value of an attribute if it exists, else return an error */
	int cbf_H5Aget_string(const hid_t ID, const char * const name, const char * * const value)
	{
		return CBF_NOTIMPLEMENTED;
	}

	/* create an attribute with the given name & ASCII value, try to write it to the HDF5 id */
	int cbf_H5Aset_string(const hid_t ID, const char * const name, const char * value)
	{
		/* define variables & check args */
		int error = (!cbf_is_valid_h5id(ID) || !name) ? CBF_ARGUMENT : CBF_SUCCESS;
		const void * const str = value ? value : "";
		hid_t attribspace = H5Screate(H5S_SCALAR), attribtype = H5Tcopy(H5T_C_S1), attribid = CBF_H5FAIL;

		/* check variables are valid */
		cbf_h5reportneg(attribspace, CBF_H5ERROR, error);
		cbf_h5reportneg(attribtype, CBF_H5ERROR, error);

		/* do some work */
		cbf_h5reportneg(H5Tset_size(attribtype,strlen(str)), CBF_H5ERROR, error);
		cbf_h5reportneg(attribid = H5Acreatex(ID,name,attribtype,attribspace,H5P_DEFAULT), CBF_H5ERROR, error);
		cbf_h5reportneg(H5Awrite(attribid,attribtype,str), CBF_H5ERROR, error);

		/* check local variables are properly closed */
		if (cbf_is_valid_h5id(attribspace))  H5Sclose(attribspace);
		if (cbf_is_valid_h5id(attribtype))   H5Tclose(attribtype);
		if (cbf_is_valid_h5id(attribid))     H5Aclose(attribid);

		/* done */
		return error;
	}

	/*  find/create/free hdf5 datasets without directly using hdf5 API */

	int cbf_H5Dcreate_string(const hid_t location, hid_t * const dataset, const char * const name, const char * value)
	{
		/* define variables & check args */
		int error = (!name || !value) ? CBF_ARGUMENT : CBF_SUCCESS;
		hid_t dataspace = H5Screate(H5S_SCALAR),  datatype = H5Tcopy(H5T_C_S1), dataprop = H5Pcreate(H5P_DATASET_CREATE), dataset_local = CBF_H5FAIL;

		/* check variables are valid */
		cbf_h5reportneg(dataspace, CBF_H5ERROR, error);
		cbf_h5reportneg(datatype, CBF_H5ERROR, error);
		cbf_h5reportneg(dataprop, CBF_H5ERROR, error);

		/* do some work */
		cbf_h5reportneg(H5Tset_size(datatype,strlen(value)), CBF_H5ERROR, error);
		cbf_h5reportneg(dataset_local = H5Dcreatex(location,name,datatype,dataspace,dataprop), CBF_H5ERROR, error);
		cbf_h5reportneg(H5Dwrite(dataset_local,datatype,H5S_ALL,H5S_ALL,H5P_DEFAULT,value), CBF_H5ERROR, error);

		/* check local variables are properly closed */
		if (cbf_is_valid_h5id(dataspace)) H5Sclose(dataspace);
		if (cbf_is_valid_h5id(datatype)) H5Tclose(datatype);
		if (cbf_is_valid_h5id(dataprop)) H5Pclose(dataprop);

		/* if the dataset object is requested then return it, otherwise close it */
		if (dataset) *dataset = dataset_local;
		else if (cbf_is_valid_h5id(dataset_local)) H5Dclose(dataset_local);

		/* done */
		return error;
	}

	/* A dataset object may be returned, so provide CBF-style methods to close it */
	int cbf_H5Dfree(const hid_t ID)
	{
		return H5Dclose(ID)>=0 ? CBF_SUCCESS : CBF_H5ERROR;
	}

	int cbf_H5Ddelete(hid_t * const ID)
	{
		const int err = cbf_H5Dfree(*ID);
		*ID = CBF_H5FAIL;
		return err;
	}


#ifdef __cplusplus

}

#endif
