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

#include <ctype.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <limits.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_binary.h"
#include "cbf_simple.h"
#include "cbf_string.h"


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

    /* Get the number of scans */

    int cbf_count_scans (cbf_handle handle, unsigned int *scans)
    {
        const char *scan_id;
        
        int errorcode;
        
        unsigned int count;
        
        unsigned int crow, row, rows;
        
        if (!handle || !scans) return CBF_ARGUMENT;
        
        errorcode = 0;
        
        if (!cbf_find_category(handle,"diffrn_scan")
            && !cbf_find_column(handle,"id")
            && !cbf_rewind_row(handle)
            && !cbf_count_rows(handle,&rows)
            && rows > 0) {
            
            count = 0;
            
            CBF_START_ARRAY(const char *,scanids,rows);
            
            for (row = 0; row < rows; row++) {
                
                cbf_reportnez(cbf_select_row(handle,row),errorcode);
                
                cbf_reportnez(cbf_get_value(handle,&scan_id),errorcode);
                
                if (!errorcode && scan_id) {
                    
                    int match;
                    
                    match = 0;
                    
                    for(crow = 0; crow < count; crow++) {
                        
                        if (!cbf_cistrcmp(scan_id,scanids[crow])) {
                            
                            match = 1; break;
                            
                        }
                        
                    }
                    
                    if (!match) {
                        
                        scanids[count++] = scan_id;
                        
                    }
                    
                }
                
            }
            
            CBF_END_ARRAY_REPORTNEZ(scanids,errorcode);
            
            *scans = count;
            
        } else {
            
            *scans = 0;
            
        }
        
        return errorcode;
        
    }
    
    /* Get the scan id for a given scan number */
    
    int cbf_get_scan_id (cbf_handle handle, unsigned int scan_number, const char ** scan_id)
    {
        
        int errorcode;
        
        unsigned int count;
        
        unsigned int crow, row, rows;
        
        const char * xscan_id;
        
        if (!handle || !scan_id) return CBF_ARGUMENT;
        
        errorcode = 0;
        
        *scan_id = NULL;
        
        if (!cbf_find_category(handle,"diffrn_scan")
            && !cbf_find_column(handle,"id")
            && !cbf_rewind_row(handle)
            && !cbf_count_rows(handle,&rows)
            && rows > 0) {
            
            count = 0;
            
            CBF_START_ARRAY(const char *,scanids,rows);
            
            for (row = 0; row < rows; row++) {
                
                cbf_reportnez(cbf_select_row(handle,row),errorcode);
                
                cbf_reportnez(cbf_get_value(handle,&xscan_id),errorcode);
                
                if (!errorcode && xscan_id) {
                    
                    int match;
                    
                    match = 0;
                    
                    for(crow = 0; crow < count+1; crow++) {
                        
                        if (!cbf_cistrcmp(xscan_id,scanids[crow])) {
                            
                            match = 1; break;
                            
                        }
                        
                    }
                    
                    if (!match) {
                        
                        scanids[count++] = xscan_id;
                        
                        if (count == scan_number+1) {
                            
                            *scan_id = xscan_id;
                            
                        }
                        
                    }
                    
                    if (*scan_id) break;
                    
                }
                
            }
            
            CBF_END_ARRAY_REPORTNEZ(scanids,errorcode);
            
        }
        
        return errorcode;
        
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
    
    
    /* convert an array_id or element_id and optional 
       array_section_id to an element_number = ordinal 
       from 0 of the
       detector_element for the array_id
       + (the number of detector elements)
         *(the ordinal of the array_section_id
            from 0 for that array_id)
     */
       
    
    int cbf_get_element_number(cbf_handle handle,
                               const char *element_id,
                               const char *array_id,
                               const char *array_section_id,
                               unsigned int * element_number) {
        
        unsigned int elements, elno;
        
        unsigned int elementidrow, arrayidrow, arraysectionrow;
        
        int index;
        
        const char * xarray_id = NULL;
        
        const char * xarray_section_id = NULL;
        
        if (!handle || (!array_id && !element_id) ) return CBF_ARGUMENT;
        
        if (array_section_id && !array_id) return CBF_ARGUMENT;
        
        elno = elements = elementidrow = arrayidrow = INT_MAX;
        
        cbf_failnez(cbf_count_elements(handle,&elements));
        
        if (!cbf_find_category  (handle, "diffrn_data_frame") ||
             !cbf_find_category  (handle, "diffrn_frame_data")) {
                
            elementidrow = arrayidrow = INT_MAX;
            
            if (element_id) {
                
                cbf_failnez (cbf_find_column    (handle, "detector_element_id"));
                
                cbf_failnez (cbf_find_row       (handle, element_id));
                
                cbf_failnez (cbf_row_number     (handle, &elementidrow));
                
                elno = elementidrow;
                
                if (array_id) {
                    
                    cbf_failnez (cbf_find_column    (handle, "array_id"));
                    
                    cbf_failnez (cbf_get_value      (handle, &xarray_id));
                    
                    if (!xarray_id || cbf_cistrcmp(xarray_id,array_id)) return CBF_FORMAT;
                    
                    arrayidrow = elementidrow;
                    
                }
                
            }
            
            
            if (arrayidrow == INT_MAX && array_id) {
                
                cbf_failnez (cbf_find_column    (handle, "array_id"));
                
                cbf_failnez (cbf_find_row       (handle, array_id));
                
                cbf_failnez (cbf_row_number     (handle, &arrayidrow));
                
                elno = arrayidrow;
                
            }
            
        }
        
        if (array_section_id) {
            
            arraysectionrow = 0;
            
            cbf_failnez(cbf_find_category(handle,"array_structure_list_section"));
            
            cbf_failnez(cbf_find_column(handle,"array_id"));
            
            cbf_failnez(cbf_find_row(handle,array_id));
            
            while (!cbf_find_column(handle,"id")
                   && !cbf_get_value(handle,&xarray_section_id)) {

                if (!cbf_cistrcmp(xarray_section_id,array_section_id)) {

                    elno += elements*arraysectionrow;

                    if (element_number) *element_number = elno;

                    return CBF_SUCCESS;

                }
                
                cbf_failnez(cbf_find_column(handle,"index"));
                
                cbf_failnez(cbf_get_integervalue(handle,&index));
                
                if (index == 1) arraysectionrow++;
                
                cbf_failnez(cbf_find_column(handle,"array_id"));
                
                cbf_failnez(cbf_find_nextrow(handle,array_id));
                
                
            }
            
            return CBF_NOTFOUND;
            
            
        }
        
        if (element_number) *element_number = elno;
 
        return CBF_SUCCESS;
        
    }
    
    /* Get the element id 
       The elements are taken mod the number of elements
     */

    int cbf_get_element_id (cbf_handle handle, unsigned int element_number,
                            const char **element_id)
    {
        const char *diffrn_id, *id;
        
        unsigned int elements, elno;

        /* count the elements */
        
        cbf_failnez (cbf_count_elements (handle, &elements));
        elno = element_number%elements;
        
        /* fprintf(stderr,"element number %d elno %d section %d\n",element_number,elno,element_number/elements); */


        /* Get the diffrn.id */

        cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

        cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
        cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
        cbf_failnez (cbf_find_row      (handle, diffrn_id))
        cbf_failnez (cbf_find_column   (handle, "id"))
        cbf_failnez (cbf_get_value     (handle, &id))

        cbf_failnez (cbf_find_category (handle, "diffrn_detector_element"));
        cbf_failnez (cbf_find_column   (handle, "detector_id"))

        do

            cbf_failnez (cbf_find_nextrow (handle, id))

            while (elno--);

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

    
    /* Get the array section id for a given detector element 
       Returns the array id if no section id is found or
       is "." */
    
    int cbf_get_array_section_id (cbf_handle handle,
                                  unsigned int element_number,
                                  const char **array_section_id)
    {
        const char *array_id;
        
        unsigned int elements;
        
        unsigned int count;
        
        unsigned int ii;
        
        int index;
        
        
        if (!handle || !array_section_id) return CBF_ARGUMENT;
        
        *array_section_id = NULL;
        
        cbf_failnez (cbf_get_array_id(handle,element_number,&array_id));
        
        cbf_failnez (cbf_count_elements(handle,&elements));
        
        if (elements == 0) return CBF_FORMAT;
        
        count = element_number/elements;
        
        /* fprintf(stderr,"element number %d elno %d section %d\n",element_number,element_number%elements,element_number/elements); */

        
        if (!cbf_find_category(handle,"array_structure_list_section")
            && !cbf_find_column(handle,"array_id")
            && !cbf_rewind_row(handle)) {
            
            for (ii = 0; ii <= count; ii ++) {
                
                index = -1;
                
                do {
                
                    cbf_failnez(cbf_find_nextrow(handle,array_id));
                
                    cbf_failnez(cbf_find_column(handle,"index"));
                
                    cbf_failnez(cbf_get_integervalue(handle,&index));
                    
                    cbf_failnez(cbf_find_column(handle,"array_id"));
                    
                } while (index != 1);
                
            }
            
            if (!cbf_find_column(handle,"id")
                && !cbf_get_value(handle,array_section_id)) return CBF_SUCCESS;
            
        }
        
        return (cbf_get_array_id(handle,element_number,array_section_id));
        
    }
    
    int cbf_get_array_section_type (cbf_handle handle,
                                    const char *array_id,
                                    int * bits, int * sign, int * real) {
        
        
        const char * xarray_id;
        
        const char * encoding_type;
        
        char * ptr;
        
        int xbits, xsign, xreal;
        
        int failure = 3;
        
        if (!handle || !array_id ) return CBF_ARGUMENT;
        
        cbf_failnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id));
        
        if (!cbf_find_category(handle,"array_structure")
            &&!cbf_find_column(handle,"id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_row(handle,xarray_id)
            &&!cbf_find_column(handle,"encoding_type")
            &&!cbf_get_value(handle,&encoding_type)
            &&encoding_type) {
            
            ptr = (char *)encoding_type;
            
            xbits = 32;
            
            xsign = 1;
            
            xreal = 0;
            
            failure = 3;
            
            while (*ptr) {
                
                if (*ptr==' '||*ptr=='\t'||*ptr=='\n'||*ptr=='\r') {
                    
                    ptr++; continue;
                    
                }
                
                if (cbf_cistrncmp(ptr,"signed", 6) == 0) {
                    
                    ptr += 6;  xsign = 1; failure--; continue;

                }

                if (cbf_cistrncmp(ptr,"unsigned", 8) == 0) {
                    
                    ptr += 8;  xsign = 0; failure--; continue;
                    
                }
                
                if (failure == 2) {
                    
                    int count;
                    
                    count = 0;
                    
                    sscanf (ptr, "%d-%n", &xbits, &count);
                    
                    if (cbf_cistrncmp (ptr+count, "bit", 3 ) == 0) {
                        
                        if (count && xbits > 0 && xbits <= 64)
                        {
                            ptr += count;
                            
                            if (*ptr == ' '||*ptr=='\t'||*ptr=='\n'||*ptr=='\r' ) ptr++;
                            
                            failure --;
                        }

                    }
                }
                if (failure == 1) {
                    
                    if (cbf_cistrncmp (ptr, "integer", 7 ) == 0) {
                        
                        ptr+= 7; xreal = 0; failure--;
                        
                     } else {
                        
                        if (cbf_cistrncmp(ptr, "real", 4 ) == 0 ) {
                            
                            ptr+=4;  if(*ptr == ' '||*ptr=='\t'||*ptr=='\n'||*ptr=='\r') ptr++;
                            
                            if (cbf_cistrncmp(ptr, "ieee", 4 ) == 0 ) {
                                
                                ptr+= 4; xreal = 1; failure--;
                                
                            }
                            
                        } else {
                            
                            if (cbf_cistrncmp(ptr, "complex", 7 ) == 0 ) {
                                
                                ptr+=7;  if(*ptr == ' '||*ptr=='\t'||*ptr=='\n'||*ptr=='\r') ptr++;
                                
                                if (cbf_cistrncmp(ptr, "ieee", 4 ) == 0 ) {
                                    
                                    ptr+=4; xreal = 1; failure--;
                                    
                                }
                                
                            }
                            
                        }
                        
                    }
                    
                }
                
                if (*ptr) ptr++;
                
            }
            
            if (!failure) {
                
                if (bits) *bits = xbits;
                
                if (sign) *sign = xsign;
                
                if (real) *real = xreal;
                
                return CBF_SUCCESS;
                
            }
            
        }
        
        /* as a fall-back for miniCBFs, try to get the information
         from the data */
            
        if (!cbf_find_category(handle,"array_data")
            &&!cbf_find_column(handle,"array_id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_row(handle,"xarray_id")
            &&!cbf_find_column(handle,"data")){
            
            int elsigned, elunsigned, realarray;
            
            size_t elsize;
            
            cbf_failnez(cbf_get_arrayparameters(handle,NULL,NULL,&elsize,&elsigned,&elunsigned,NULL,NULL,NULL,&realarray));
            
            if (real) *real = realarray;
            
            if (sign) *sign = elsigned;
            
            if (bits) *bits = CHAR_BIT*elsize;
            
            return CBF_SUCCESS;
            
        }

        return CBF_NOTFOUND;
        
    }
    
    
    /* Get the size of an array or array section.  The arrray dim
       is filled with dimesions from fast to slow up to rank.  Unused
       dimensions are set to 1.  rank must be at least 1 and not more
       than 100*/
    
    int cbf_get_array_section_size (cbf_handle    handle,
                               const char   *array_id,
                               size_t       rank,
                               size_t       *dims)
    {
        
        int precedence;
        
        int ii;
        
        const char * xarray_id;
        
        if (rank < 1 || rank > 100 || !dims)
            
            return CBF_ARGUMENT;
        
        for (ii=0; ii < (ssize_t)rank; ii++) dims[ii] = 1;
        
        CBF_START_ARRAY(int,done,rank);
        
        CBF_START_ARRAY(size_t,kdim,rank);
        
        /* If array_id is NULL, try directly from the first entry
         in _array_data.data */
        
        if (!array_id) {
            
            size_t nelem;
            
            unsigned int compression;
            
            if (rank > 3) {
                
                cbf_free((void **) &kdim,NULL);
                
                cbf_free((void **) &done,NULL);
                
                return CBF_ARGUMENT;
                
            }
            
            cbf_onfailnez (cbf_find_category (handle, "array_data"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            cbf_onfailnez (cbf_find_column(handle,"data"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            cbf_onfailnez (cbf_rewind_row(handle),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            cbf_onfailnez (cbf_get_arrayparameters_wdims(handle,
                                                         &compression,NULL,NULL,NULL,NULL,&nelem,NULL,NULL,NULL,NULL,
                                                         dims,kdim+1,kdim+2,NULL),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            if (rank > 1) {
                
                dims[1] = kdim[1];
                
                if (dims[1] == 0) dims[1] = 1;
                
            }
            
            if (rank > 2) {
                
                dims[2] = kdim[2];
                
                if (dims[2] == 0) dims[2] = 1;
                
            }
            
            if (dims[0] == 0) dims[0] = 1;
            
            cbf_free((void **) &kdim,NULL);
            
            cbf_free((void **) &done,NULL);
            
            return CBF_SUCCESS;
            
        }
        
        /* See if this is an array section is or an array id */
        
        cbf_onfailnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id),
                      {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);})
        
        if (cbf_cistrcmp(array_id,xarray_id)) {
            
            /* This is actually an array section */
            
            size_t index;
            
            size_t kstart,kend;
            
            long kstride;
            
            for (index = 1L; index < rank+1; index++) {
                
                ssize_t tdim;
                
                /* fprintf(stderr,"cbf_get_array_section_section array_id = '%s'"
                 ", xarray_id = '%s'"
                 ", index= %d\n",array_id,xarray_id,index); */
                
                cbf_onfailnez(cbf_get_array_section_section(handle,
                                                            array_id,
                                                            index,&kstart,
                                                            &kend,
                                                            &kstride),
                              {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
                
                tdim = kend - kstart;
                
                if (tdim < 0) tdim = -tdim;
                    
                dims[index-1] = tdim;
                
                if (kstride < 0) kstride = -kstride;
                
                if (kstride == 0) kstride = 1;
                
                dims[index-1] += kstride;
                
                dims[index-1] /= kstride;
                
            }
            
            cbf_free((void **) &kdim,NULL);
            
            cbf_free((void **) &done,NULL);
            
            return CBF_SUCCESS;
            
        }
        
        /* Get the dimensions from the array_structure_list category */
        
        for (ii = 0; ii < (ssize_t)rank; ii++) {
            
            done[ii] = 0;
            
        };
        
        cbf_onfailnez (cbf_find_category (handle, "array_structure_list"),
                       {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
        cbf_onfailnez (cbf_find_column   (handle, "array_id"),
                       {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
        
        while (cbf_find_nextrow (handle, array_id) == 0)
        {
            long xdim;
            
            cbf_onfailnez (cbf_find_column      (handle, "precedence"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);})
            cbf_onfailnez (cbf_get_integervalue (handle, &precedence),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);})
            
            if (precedence < 1 || precedence > (ssize_t)rank)
                
                return CBF_FORMAT;
            
            cbf_onfailnez (cbf_find_column      (handle, "dimension"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);})
            cbf_onfailnez (cbf_get_longvalue    (handle, &xdim),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);})
            dims[precedence-1] = xdim;
            
            if (done [precedence-1])
                
                return CBF_FORMAT;
            
            done [precedence-1] = 1;
            
            cbf_onfailnez (cbf_find_column (handle, "array_id"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);})
        }
        
        for (ii=0; ii < (ssize_t)rank; ii++) {
            
            if (dims[ii] == 0) dims[ii] = 1;
            
        }
        
        if (!done [1]) {
            
            cbf_free((void **) &kdim,NULL);
            
            cbf_free((void **) &done,NULL);
            
            return CBF_NOTFOUND;
            
        }
        
        CBF_END_ARRAY(kdim);
        
        CBF_END_ARRAY(done);
        
        return CBF_SUCCESS;
    }

    
    
    /* Get the size, origins and strides of an array or array section.  
     The arrray dim is filled with dimesions from fast to slow up to rank.  
     Unused dimensions are set to 1.  rank must be at least 1 and not more
     than 100*  Unused strides are set to 1, unused origins are set to 1*/
    
    int cbf_get_array_section_sizes (cbf_handle    handle,
                                    const char   *array_id,
                                    size_t       rank,
                                    size_t       *dims,
                                    size_t       *origins,
                                    long         *strides)
    {
        
        int precedence;
        
        int ii;
        
        const char * xarray_id;
        
        if (rank < 1 || rank > 100 || !dims)
            
            return CBF_ARGUMENT;
        
        for (ii=0; ii < (ssize_t)rank; ii++) {
            
            if (dims) dims[ii] = 1;
            
            if (strides) strides[ii] = 1;
            
            if (origins) origins[ii] = 1;
            
        }
        
        CBF_START_ARRAY(int,done,rank);
        
        CBF_START_ARRAY(size_t,kdim,rank);
        
        /* If array_id is NULL, try directly from the first entry
         in _array_data.data */
        
        if (!array_id) {
            
            size_t nelem;
            
            unsigned int compression;
            
            if (rank > 3) {
                
                cbf_free((void **) &kdim,NULL);
                
                cbf_free((void **) &done,NULL);
                
                return CBF_ARGUMENT;
                
            }
            
            cbf_onfailnez (cbf_find_category (handle, "array_data"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            cbf_onfailnez (cbf_find_column(handle,"data"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            cbf_onfailnez (cbf_rewind_row(handle),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            cbf_onfailnez (cbf_get_arrayparameters_wdims(handle,
                                                         &compression,NULL,NULL,NULL,NULL,&nelem,NULL,NULL,NULL,NULL,
                                                         kdim,kdim+1,kdim+2,NULL),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            if (dims) {
                
                dims[0] = kdim[0];
                
                if (dims[0] == 0) dims[0] = 1;
                
            }
            
            if (rank > 1 && dims) {
                
                dims[1] = kdim[1];
                
                if (dims[1] == 0) dims[1] = 1;
                
            }
            
            if (rank > 2 && dims ) {
                
                dims[2] = kdim[2];
                
                if (dims[2] == 0) dims[2] = 1;
                
            }
            
            cbf_free((void **) &kdim,NULL);
            
            cbf_free((void **) &done,NULL);
            
            return CBF_SUCCESS;
            
        }
        
        /* See if this is an array section is or an array id */
        
        cbf_onfailnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id),
                      {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
        
        if (cbf_cistrcmp(array_id,xarray_id)) {
            
            /* This is actually an array section */
            
            size_t index;
            
            size_t kstart,kend;
            
            long kstride;
            
            for (index = 1L; index < rank+1; index++) {
                
                /* fprintf(stderr,"cbf_get_array_section_section array_id = '%s'"
                 ", xarray_id = '%s'"
                 ", index= %d\n",array_id,xarray_id,index); */
                
                cbf_onfailnez(cbf_get_array_section_section(handle,
                                                            array_id,
                                                            index,&kstart,
                                                            &kend,
                                                            &kstride),
                              {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
                
                
                
                if (strides) {
                    
                    strides[index-1] = kstride;
                    
                }
                
                if (origins) {
                    
                    origins[index-1] = kend;
                    
                }
                
                
                if (dims) {
                    
                    ssize_t tdim;
                    
                    tdim = kend - kstart;
                    
                    if (tdim < 0) tdim = -tdim;
                        
                    dims[index-1] = tdim;
                    
                    if (kstride < 0) kstride = -kstride;
                    
                    if (kstride == 0) kstride = 1;
                    
                    dims[index-1] += kstride;
                    
                    dims[index-1] /= kstride;
                    
                }
                
                
            }
            
            cbf_free((void **) &kdim,NULL);
            
            cbf_free((void **) &done,NULL);
            
            return CBF_SUCCESS;
            
        }
        
        /* Get the dimensions from the array_structure_list category */
        
        for (ii = 0; ii < (ssize_t)rank; ii++) {
            
            done[ii] = 0;
            
        };
        
        cbf_onfailnez (cbf_find_category (handle, "array_structure_list"),
                       {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
        cbf_onfailnez (cbf_find_column   (handle, "array_id"),
                       {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
        
        while (cbf_find_nextrow (handle, array_id) == 0)
        {
            long xdim;
            
            cbf_onfailnez (cbf_find_column      (handle, "precedence"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            cbf_onfailnez (cbf_get_integervalue (handle, &precedence),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            
            if (precedence < 1 || precedence > (ssize_t)rank)
                
                return CBF_FORMAT;
            
            cbf_onfailnez (cbf_find_column      (handle, "dimension"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            cbf_onfailnez (cbf_get_longvalue    (handle, &xdim),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
            dims[precedence-1] = xdim;
            
            if (done [precedence-1])
                
                return CBF_FORMAT;
            
            done [precedence-1] = 1;
            
            cbf_onfailnez (cbf_find_column (handle, "array_id"),
                           {cbf_free((void **) &kdim,NULL);cbf_free((void **) &done,NULL);});
        }
        
        for (ii=0; ii < (ssize_t)rank; ii++) {
            
            if (dims[ii] == 0) dims[ii] = 1;
            
        }
        
        if (!done [1]){
            
            cbf_free((void **) &kdim,NULL);
            
            cbf_free((void **) &done,NULL);
            
            return CBF_NOTFOUND;
            
        }
        
        CBF_END_ARRAY(kdim);
        
        CBF_END_ARRAY(done);
        
        return CBF_SUCCESS;
    }
    
    /* Get the pixel sizes for the given array section
       Undetermined pixel sizes are set to zero
     */
    
    int cbf_get_array_section_pixel_sizes (cbf_handle    handle,
                                     const char   *array_id,
                                     size_t       rank,
                                     double       *psizes)
    {
        
        long index, precedence;
        
        unsigned int ii;
        
        unsigned int numrows;
        
        int error;
        
        const char * xarray_id, *yarray_id;
        
        if (rank < 1 || rank > 100 || !array_id || !psizes)
            
            return CBF_ARGUMENT;
        
        for (ii=0; ii < (ssize_t)rank; ii++) {
            
            psizes[ii] = 0.;
            
        }
        
        error = 0.;
        
        xarray_id = yarray_id = "";
        

        /* Get the array id for the array_section, if this is an
           array section */
        
        cbf_reportnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id),error);
        
        cbf_reportnez(cbf_find_category(handle,"array_element_size"),error);
        
        cbf_reportnez(cbf_find_column(handle,"size"),error);
        
        cbf_reportnez(cbf_count_rows(handle,&numrows),error);
        
        CBF_START_ARRAY(double,psize_by_index,rank);
        
        for (ii = 0; ii < rank; ii++) {
            
            psize_by_index[ii] = 0.;
            
        }
        
        /* collect the pixel sizes by index */
        
        for (ii = 0; ii < numrows && !error; ii++) {
            
            cbf_reportnez(cbf_find_column(handle,"array_id"),error);
            
            cbf_reportnez(cbf_select_row(handle,ii),error);
            
            cbf_reportnez(cbf_get_value(handle,&yarray_id),error);
            
            if (!cbf_cistrcmp(xarray_id,yarray_id)) {
                
                cbf_reportnez(cbf_find_column(handle,"index"),error);
                
                cbf_reportnez(cbf_get_longvalue(handle,&index),error);
                
                if (index < 1 || index > (long)rank) error |= CBF_FORMAT;
                
                cbf_reportnez(cbf_find_column(handle,"size"),error);
                
                cbf_reportnez(cbf_get_doublevalue(handle,&(psize_by_index[index-1])),error);
                
            }
            
        }
        
        /* Convert the pixel sizes to be by precedence rather than by index */
        
        
        cbf_reportnez(cbf_find_category(handle,"array_structure_list"),error);
        
        cbf_reportnez(cbf_find_column(handle,"precedence"),error);
        
        cbf_reportnez(cbf_count_rows(handle,&numrows),error);
        
        for (ii = 0; ii < numrows && !error; ii++) {
            
            cbf_reportnez(cbf_find_column(handle,"array_id"),error);
            
            if (error) error = cbf_find_column(handle,"array_section_id");
            
            cbf_reportnez(cbf_select_row(handle,ii),error);
            
            cbf_reportnez(cbf_get_value(handle,&yarray_id),error);
            
            if (!cbf_cistrcmp(xarray_id,yarray_id)|| (!cbf_cistrcmp(array_id,yarray_id))) {
                
                cbf_reportnez(cbf_find_column(handle,"index"),error);
                
                cbf_reportnez(cbf_get_longvalue(handle,&index),error);

                cbf_reportnez(cbf_find_column(handle,"precedence"),error);
                
                cbf_reportnez(cbf_get_longvalue(handle,&precedence),error);

                if (index < 1 || index > (long)rank
                    || precedence < 1 || precedence > (long)rank) error |= CBF_FORMAT;
                
                if (!error) {
                    
                    psizes[precedence-1] = 1.e3*psize_by_index[index-1];
                    
                }
                
            }
            
        }
        
        CBF_END_ARRAY_REPORTNEZ(psize_by_index,error);
        
        if (error || psizes[0] == 0.||xarray_id[0] == '\0') {
            
            int oerror;
            
            unsigned int element_number;
            
            cbf_detector detector;
            
            oerror = error;
            
            error = 0;
            
            cbf_reportnez(cbf_get_element_number(handle,NULL,
                                                 xarray_id,
                                                 array_id,
                                                 &element_number),error);
            
            cbf_reportnez(cbf_construct_detector(handle,
                                                 &detector,
                                                 element_number),error);
            
            for (ii=0; ii < rank && ii < (ssize_t)(detector-> axes); ii++) {
                
                cbf_reportnez(cbf_get_inferred_pixel_size(detector,ii+1,&(psizes[ii])),error);
                
                
            }
            
            if (!error) return error;
            
            error |= oerror;
            
        }
        
        return error;

     }


    
    /* Determine a rank for an array_section_id
     as the maximum of the indices in
     ARRAY_STRUCTURE_LIST_SECTION or by counting the
     comma-separated components of the name
     
     If the array_section_id is actually an array_id,
     the rank is computed from ARRAY_STRUCTURE_LIST
     
     */

    
    int cbf_get_array_section_rank(cbf_handle handle,
                                   const char * array_section_id,
                                   size_t * rank) {
        
        const char * ptr;
        
        const char * array_id;
        
        size_t len;
        
        int index;

        int error = 0;
        
        long precedence, maxprecedence;
        
        maxprecedence = 0;

        if ( !handle
            || !array_section_id
            || !rank ) return CBF_ARGUMENT;
        
        /* See if this is an array id */
        
        if (!cbf_get_array_section_array_id(handle,array_section_id,&array_id)
            && array_id
            && !cbf_cistrcmp(array_section_id,array_id)) {
            
            if (!cbf_find_category(handle,"array_structure_list")
                && !cbf_find_column(handle,"array_id")
                && !cbf_rewind_row(handle)
                && !cbf_find_row(handle,array_id)) {
                
                do {
                    
                    cbf_failnez(cbf_find_column(handle,"precedence"));
                    
                    cbf_failnez(cbf_get_longvalue(handle,&precedence));
                    
                    if (precedence > maxprecedence){
                        
                        maxprecedence = precedence;
                        
                    }
        
                    cbf_failnez(cbf_find_column(handle,"array_id"));
                }
                
                while (!cbf_find_nextrow(handle,array_id));
                
                if (maxprecedence > 0) {
                    
                    *rank = (size_t)maxprecedence;
                    
                    return CBF_SUCCESS;
                    
                }
                 
            }

            
            
        }
        
        
        /* Try for the explicit mapping */
        
        if (!cbf_find_category(handle,"array_structure_list_section")
            &&!cbf_find_column(handle,"id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_row(handle,array_section_id)
            &&!cbf_find_column(handle,"index")
            &&!cbf_get_integervalue(handle,&index)){
            
            *rank = index;
            
            while (!cbf_find_column(handle,"id")
                   &&!cbf_find_nextrow(handle,array_section_id)){
                
                cbf_failnez(cbf_find_column(handle,"index"));
                
                cbf_failnez(cbf_get_integervalue(handle,&index));
                
                if (index > (ssize_t)(*rank)) *rank = index;
                
            }
            
            return CBF_SUCCESS;
            
        }
            
        
        ptr = array_section_id;
        
        len = 0;
        
        *rank = 0;
        
        error = CBF_SUCCESS;
        
        while (*ptr && *ptr != '(') {
            
            ptr++;
            
            len++;
            
        }
        
        if (*ptr) return CBF_NOTFOUND;
        
        (*rank)++;
        
        ptr++;
        
        while (*ptr && *ptr != ',' && *ptr != ')') {
            
            if (*ptr == ')') return CBF_SUCCESS;
            
            if (*ptr == ',') (*rank)++;
            
            ptr++;
               
        }
        
        return CBF_ARGUMENT;
         
    }

    /* Determine start, end and stride for an array_section_id
     for a given index, either from  ARRAY_STRUCTURE_LIST_SECTION
     or by parsing comma-separated components of the name
     */
    
    int cbf_get_array_section_section(cbf_handle handle,
                                         const char * array_section_id,
                                         size_t index,
                                         size_t * start,
                                         size_t * end,
                                         long * stride ) {
        
        const char * ptr;
        
        size_t len;
        
        const char * array_id;
        
        char * endptr;
                
        int error;
        
        long xindex;
        
        long xstride = 1;
        
        size_t xstart = 0;
        
        size_t xend = 0;
        
        long xtemp;
        
        long array_dim;
        
        const char * direction;
        
        
        if ( !handle
            || !array_section_id ) return CBF_ARGUMENT;
        
        array_dim = 0L;
        
        /* fprintf(stderr,"Entering cbf_get_section_section, array_section_id = %s\n",
                array_section_id); */
        
        if (!cbf_get_array_section_array_id(handle,array_section_id,&array_id)){
            
            if (!cbf_find_category(handle,"array_structure_list")
                && !cbf_find_column(handle,"array_id")
                && !cbf_rewind_row(handle)
                && !cbf_find_row(handle,array_id)){
                
                do {
                    
                    if (!cbf_find_column(handle,"precedence")
                        && !cbf_get_longvalue(handle,&xindex)
                        && (size_t)xindex == index) {
                        
                        cbf_failnez(cbf_find_column(handle,"dimension"));
                        
                        cbf_failnez(cbf_get_longvalue(handle,&array_dim));
                        
                        cbf_failnez(cbf_find_column(handle,"direction"));
                        
                        cbf_failnez(cbf_get_value(handle,&direction));
                        
                        break;
                    }
                    
                    
                    cbf_failnez(cbf_find_column(handle,"array_id"));
                    
                } while (!cbf_find_nextrow(handle,array_id));
                
            }
            
            if (!cbf_cistrcmp(array_id, array_section_id)) {
                
                if (!cbf_cistrcmp(direction,"decreasing")) {
                    
                    if (stride) *stride = -1L;
                    
                    if (start)  *start = (size_t) array_dim;
                    
                    if (end) *end = (size_t) 1L;
                    
                } else {
                    
                    if (stride) *stride = 1L;
                    
                    if (start)  *start = (size_t) 1L;
                    
                    if (end) *end = (size_t) array_dim;

                }
                
                return CBF_SUCCESS;
                
            }
            
        }
        
        /* Try for the explicit mapping */
        
        if (!cbf_find_category(handle,"array_structure_list_section")
            &&!cbf_find_column(handle,"id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_column(handle,"index")) {
            
            while (!cbf_find_column(handle,"id")
                   &&!cbf_find_nextrow(handle,array_section_id)){
                
                
                cbf_failnez(cbf_find_column(handle,"index"));
                
                cbf_failnez(cbf_get_longvalue(handle,&xindex));
                
                if ((ssize_t)index != xindex) continue;
                
                xstride = 1;
                
                if (cbf_find_column(handle,"stride")
                             || cbf_get_longvalue(handle,&xstride)) {
                    
                    xstride = 1;
                    
                }
                
                
                if (stride) *stride = xstride;
                
                if (start && (cbf_find_column(handle,"start")
                    || cbf_get_longvalue(handle,&xtemp))) {
                    
                    if (xstride > 0) {
                    
                        *start = (size_t)1L;
                        
                    } else {
                        
                        *start = (size_t)array_dim;
                    }
                    
                } else {
                    
                    if (start) *start = (size_t)xtemp;
                    
                }

                if (end && (cbf_find_column(handle,"end")
                    || cbf_get_longvalue(handle,&xtemp))) {
                    
                    if (xstride > 0) {
                        
                        *end = (size_t)array_dim;
                        
                    } else {
                    
                        *end = (size_t)1L;
                        
                    }
                    
                } else {
                    
                    if (end) *end = (size_t)xtemp;
                    
                }
                
                 
                return CBF_SUCCESS;

                
            }
                        
        }
        
        
        ptr = array_section_id;
        
        len = 0;
        
        error = CBF_SUCCESS;
        
        while (*ptr && *ptr != '(') {
            
            ptr++;
            
            len++;
            
        }
        
        if (!*ptr) ptr++;
        
        xindex = index-1;
        
        while (xindex > 0) {
            
            while (*ptr != ',' ) {
                
                if (!(*ptr) || *ptr == ')') {
                    
                    if (!cbf_cistrcmp(direction,"decreasing")) {
                        
                        if (stride) *stride = -1L;
                        
                        if (start)  *start = (size_t)array_dim;
                        
                        if (end) *end = (size_t)1L;
                        
                    } else {
                        
                        if (stride) *stride = 1L;
                        
                        if (start)  *start = (size_t)1L;
                        
                        if (end) *end = (size_t)array_dim;
                        
                    }
                    
                    return CBF_SUCCESS;
                    
                }
                
                ptr++;
                
            }
            
            ptr++;
            
            xindex--;
            
        }
        
        xstart = strtol(ptr,&endptr,10);
        
        if (endptr==ptr) xstart = 1;
        
        xend = array_dim;
        
        xstride = 1;
        
        if (!(*endptr)
            &&*endptr!=','
            &&*endptr!=')') {
            
            ptr = endptr+1;
            
            xend = strtol(ptr,&endptr,10);
            
            if (endptr==ptr) xend = array_dim;
            
            if (!(*endptr)
                &&*endptr!=','
                &&*endptr!=')') {
                
                ptr = endptr+1;
                
                xstride = strtol(ptr,&endptr,10);
                
                if (endptr==ptr) xstride = 1;
                
            }

            
        }
        
        if ((xstart > xend && xstride > 0)
            || (xstart < xend && xstride < 0)){
                
                xtemp = xstart;
                
                xstart = xend;
                
                xend = xtemp;
            
        }
        
        if (start) *start = xstart;
        
        if (end) *end = xend;
        
        if (stride) *stride = xstride;
        
        return CBF_SUCCESS;
        
    }


    /* Extract an array_id from an array_section_id either
       as the entire section id, or as the portion of the
       string up to the first paren.
     
       If this is a valid array_section, the mapping to an
       array_id in ARRAY_STRUCTURE_LIST_SETCTION takes
       presedence over parsing the section name.
     
     */
    
    int cbf_get_array_section_array_id(cbf_handle handle,
                                       const char * array_section_id,
                                       const char ** array_id) {
        
        const char * ptr;
        
        size_t len;
        
        char * xarray_id;
        
        int error;
        
        if ( !handle
            || !array_section_id
            || !array_id ) return CBF_ARGUMENT;
        
        /* Try for the explicit mapping */
        
        if (!cbf_find_category(handle,"array_structure_list_section")
            &&!cbf_find_column(handle,"id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_row(handle,array_section_id)
            &&!cbf_find_column(handle,"array_id")
            &&!cbf_get_value(handle,array_id)
            &&*array_id)  return CBF_SUCCESS;
        
        /* Try for the implicit mapping */
        
        ptr = array_section_id;
        
        len = 0;
        
        error = CBF_SUCCESS;
        
        while (*ptr && *ptr != '(') {
            
            ptr++;
            
            len++;
            
        }
        
        cbf_failnez(cbf_alloc((void **) &xarray_id,NULL,1,len+1));
        
        strncpy(xarray_id,array_section_id,len);
        
        xarray_id[len] = '\0';
        
        if ((!cbf_find_category(handle,"array_structure"))
            &&(!cbf_find_column(handle,"id"))
            &&(!cbf_rewind_row(handle))
            &&(!cbf_find_row(handle,xarray_id))
            &&(!cbf_get_value(handle,array_id))
            && (*array_id)) {
            
            cbf_free((void **) &xarray_id, NULL);
            
            return CBF_SUCCESS;
        
        }
    
        if ((!cbf_find_category(handle,"array_structure_list"))
            &&(!cbf_find_column(handle,"array_id"))
            &&(!cbf_rewind_row(handle))
            &&(!cbf_find_row(handle,xarray_id))
            &&(!cbf_get_value(handle,array_id))
            && (*array_id)) {
            
            cbf_free((void **) &xarray_id, NULL);
            
            return CBF_SUCCESS;
            
        }

        cbf_free((void **) &xarray_id, NULL);

        return CBF_NOTFOUND;
        
        
        return error;
        
    }
    
    
    /* Get the array id for a given detector element 
       the element numbers are taken mod the number
       of elements.
     
     */

    int cbf_get_array_id (cbf_handle handle, unsigned int element_number,
                          const char **array_id)
    {
        const char *element_id;
        const char *array_section_id;

        if (!handle || !array_id) return CBF_ARGUMENT;

        *array_id = NULL;
        
        if (!cbf_get_element_id (handle, element_number, &element_id)&&
            (!cbf_find_category  (handle, "diffrn_data_frame") ||
             !cbf_find_category  (handle, "diffrn_frame_data")))
        {
            cbf_failnez (cbf_find_column    (handle, "detector_element_id"));
            
            cbf_failnez (cbf_find_row       (handle, element_id));
            
            if (cbf_find_column    (handle, "array_id")
                || cbf_get_value      (handle, array_id)
                || ! *array_id) {
                
                *array_id = NULL;
                
                cbf_failnez(cbf_find_column(handle,"array_section_id"));
                
                cbf_failnez(cbf_get_value(handle,&array_section_id));
                
                if (array_section_id && cbf_cistrcmp(".",array_section_id)) {
                    
                    if (!cbf_find_category(handle,"array_structure_list_section")
                        && !cbf_find_column(handle,"id")
                        && !cbf_find_row(handle,array_section_id)
                        && !cbf_find_column(handle,"array_id")){
                        
                        cbf_failnez (cbf_get_value (handle, array_id));
                        
                    } else {
                        
                        cbf_failnez(cbf_get_array_section_array_id(handle,array_section_id,array_id));
                        
                    }
                    
                } else {
                    
                    return CBF_NOTFOUND;
                }
                
            } else {
                
                return CBF_SUCCESS;
                
            }

        } else {

            return CBF_NOTFOUND;


        }

        return 0;
    }

    /* Get the pixel size of a detector element in a given direction
     axis numbering is 1-based, fast to slow */

    int cbf_get_pixel_size(cbf_handle handle, unsigned int element_number,
                           int axis_number,
                           double * psize)
    {
        const char *array_id;
        const char *array_section_id;
        int        aid, precedence, max_precedence, axis_index;

        cbf_failnez (cbf_get_array_id   (handle, element_number, &array_id));
        cbf_failnez (cbf_get_array_section_id   (handle, element_number, &array_section_id));

        cbf_failnez (cbf_find_category (handle, "array_structure_list"))
        if (cbf_find_column(handle,"array_section_id")) {
          cbf_failnez (cbf_find_column   (handle, "array_id"))
        }

        precedence = max_precedence = axis_index = 0;

        while (cbf_find_nextrow (handle, array_section_id) == 0)
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
            if (cbf_find_column(handle,"array_section_id")) {
                cbf_failnez (cbf_find_column   (handle, "array_id"))
            }
        }

        if (axis_index == 0 && axis_number < 0 ) {
            cbf_failnez (cbf_rewind_row (handle) )
            while (cbf_find_nextrow (handle, array_section_id) == 0) {
                cbf_failnez (cbf_find_column      (handle, "precedence"))
                cbf_failnez (cbf_get_integervalue (handle, &precedence))

                if (precedence == max_precedence+1+axis_number) {
                    cbf_failnez (cbf_find_column      (handle, "index"))
                    cbf_failnez (cbf_get_integervalue (handle, &axis_index))
                    if (axis_index < 1) return CBF_FORMAT;
                    break;
                }
                if (cbf_find_column(handle,"array_section_id")) {
                    cbf_failnez (cbf_find_column   (handle, "array_id"))
                }
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
        const char *array_section_id;
        int        aid, precedence, max_precedence, axis_index;

        cbf_failnez (cbf_get_array_id   (handle, element_number, &array_id))
        cbf_failnez (cbf_get_array_section_id   (handle, element_number, &array_section_id))

        cbf_failnez (cbf_find_category (handle, "array_structure_list"))
        if (cbf_find_column(handle,"array_section_id")) {
            cbf_failnez (cbf_find_column   (handle, "array_id"));
        }

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
            if (cbf_find_column(handle,"array_section_id")) {
                cbf_failnez (cbf_find_column   (handle, "array_id"));
            }
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
                if (cbf_find_column(handle,"array_section_id")) {
                    cbf_failnez (cbf_find_column   (handle, "array_id"));
                }
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
        size_t ndim0;

        cbf_failnez (cbf_get_3d_image_size (handle, reserved, element_number, &ndim0, ndimslow,  ndimfast));

        if (ndim0 != 1) return CBF_ARGUMENT;

        return CBF_SUCCESS;
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
        const char *array_section_id;

        int binary_id;

        int errorcode;
        
        size_t ndim0;
        
        CBF_UNUSED( reserved );

        binary_id = 0;
        
        ndim0 = 1;

        errorcode = cbf_get_array_section_id (handle, element_number, &array_section_id);
        
        if (errorcode) array_section_id = NULL;

        cbf_failnez (cbf_get_3d_array (handle, 0, array_section_id,
                                           &binary_id,
                                           array,
                                           CBF_INTEGER,
                                           elsize,
                                           elsign,
                                           ndim0,
                                           ndimslow,
                                           ndimfast));

            
 
        return CBF_SUCCESS;
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

        const char *array_section_id;
        
        int binary_id;

        binary_id = 1;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_get_3d_array (handle, reserved, array_section_id,
                                       &binary_id, array,
                                       CBF_FLOAT,
                                       elsize,
                                       1,
                                       1,
                                       ndimslow,
                                       ndimfast));

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
        const char *array_section_id;
        
        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));
        
        cbf_failnez (cbf_get_3d_array_size (handle, reserved, array_section_id,
                                            ndimslow, ndimmid, ndimfast));
            
        return CBF_SUCCESS;
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
        const char *array_section_id;

        int binary_id;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_get_3d_array (handle, reserved, array_section_id,
                                       &binary_id,
                                       array,
                                       CBF_INTEGER,
                                       elsize,
                                       elsign,
                                       ndimslow,
                                       ndimmid,
                                       ndimfast));

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
        const char *array_section_id;

        int binary_id;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_get_3d_array (handle, reserved, array_section_id,
                                       &binary_id, array,
                                       CBF_FLOAT,
                                       elsize,
                                       1,
                                       ndimslow,
                                       ndimmid,
                                       ndimfast));

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
        const char *array_section_id;
        
        int binary_id=1;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_set_3d_array(handle, reserved, array_section_id,
                                      &binary_id,
                                      compression,
                                      array,
                                      CBF_INTEGER,
                                      elsize,
                                      elsign,
                                      1,
                                      ndimslow,
                                      ndimfast));

        return CBF_SUCCESS;
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
        const char *array_section_id;

        int binary_id = 1;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_set_3d_array(handle, reserved, array_section_id,
                                      &binary_id,
                                      compression,
                                      array,
                                      CBF_FLOAT,
                                      elsize,
                                      1,
                                      1,
                                      ndimslow,
                                      ndimfast));

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
        const char *array_section_id;

        int binary_id = 1;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_set_3d_array(handle, reserved, array_section_id,
                                      &binary_id,
                                      compression,
                                      array,
                                      CBF_INTEGER,
                                      elsize,
                                      elsign,
                                      ndimslow,
                                      ndimmid,
                                      ndimfast));

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
        const char *array_section_id;

        int binary_id = 1;

        cbf_failnez (cbf_get_array_section_id (handle, element_number, &array_section_id));

        cbf_failnez (cbf_set_3d_array(handle, reserved, array_section_id,
                                      &binary_id,
                                      compression,
                                      array,
                                      CBF_FLOAT,
                                      elsize,
                                      1,
                                      ndimslow,
                                      ndimmid,
                                      ndimfast));

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
        CBF_UNUSED( reserved );

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
        
        CBF_UNUSED( binary_id );
        
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

    
    /* Get the array parameters */
    
    int cbf_get_array_arrayparameters (cbf_handle    handle,
                                     const char   *array_id,
                                     int          binary_id,
                                     unsigned int *compression,
                                     int          *id,
                                     size_t       *elsize,
                                     int          *elsigned,
                                     int          *elunsigned,
                                     size_t       *nelem,
                                     int          *minelem,
                                     int          *maxelem,
                                     int          *realarray)
    {
        int found;
        
        /* If array_id is NULL, try directly from the first entry
         in _array_data.data, otherwise take the first entry with
         this array_id and, if binary id is not 0, the binary id */
        
        cbf_failnez (cbf_find_category (handle, "array_data"));
        
        cbf_failnez (cbf_find_column(handle, "array_id"));
        
        cbf_failnez (cbf_rewind_row(handle));
        
        if (array_id) {
            
            found = 0;
            
            while (!found) {
                
                if (cbf_find_nextrow(handle, array_id)) break;
                
                if (binary_id) {
                    
                    int xbinary_id;
                    
                    cbf_failnez(cbf_find_column(handle, "binary_id"));
                    
                    cbf_failnez(cbf_get_integervalue(handle, &xbinary_id));
                    
                    if (binary_id == xbinary_id) {
                        
                        found = 1;
                        
                        break;
                    }
                    
                } else {
                    
                    found = 1;
                    
                    break;
                }
                
                cbf_failnez(cbf_find_column(handle, "array_id"));
                
            }
            
            if (!found) return CBF_NOTFOUND;
            
        }
        
        cbf_failnez (cbf_find_column(handle,"data"))
        
        
        cbf_failnez (cbf_get_arrayparameters(handle,
                                             compression,
                                             id,
                                             elsize,
                                             elsigned,
                                             elunsigned,
                                             nelem,
                                             minelem,
                                             maxelem,
                                             realarray))
        return CBF_SUCCESS;
        
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
        
        const char * xarray_id;

        if (reserved != 0)

            return CBF_ARGUMENT;

        /* If array_id is NULL, try directly from the first entry
           in _array_data.data */

        if (!array_id) {

            size_t nelem;

            unsigned int compression;

            cbf_failnez (cbf_find_category (handle, "array_data"))

            cbf_failnez (cbf_find_column(handle,"data"))

            cbf_failnez (cbf_rewind_row(handle))

            cbf_failnez (cbf_get_arrayparameters_wdims(handle,
                    &compression,NULL,NULL,NULL,NULL,&nelem,NULL,NULL,NULL,NULL,
                    ndimfast,ndimmid,ndimslow,NULL))

            if (ndimslow && *ndimslow==0) *ndimslow = 1;

            if (ndimmid && *ndimmid== 0) *ndimmid = 1;

            if (ndimfast && *ndimfast== 0) *ndimfast = nelem;

            return CBF_SUCCESS;

        }
        
        /* See if this is an array section is or an array id */
        
        cbf_failnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id));
        
        if (cbf_cistrcmp(array_id,xarray_id)) {
            
            /* This is actually an array section */
            
            size_t rank;
            
            size_t index;
            
            size_t kstart[3],kend[3];
            
            ssize_t kdim[3];
            
            long kstride[3];
            
            kdim[0] = kdim[1] = kdim[2] = 1;
            
            cbf_failnez (cbf_get_array_section_rank (handle, array_id, &rank));
            
            for (index = 1L; index < rank+1; index++) {
                
                /* fprintf(stderr,"cbf_get_array_section_section array_id = '%s'"
                        ", xarray_id = '%s'"
                        ", index= %d\n",array_id,xarray_id,index); */
                
                cbf_failnez(cbf_get_array_section_section(handle,
                                                          array_id,
                                                          index,kstart+index-1,
                                                          kend+index-1,
                                                          kstride+index-1));
                
                kdim[index-1] = kend[index-1] - kstart[index-1];
                
                if (kdim[index-1] < 0) kdim[index-1] = -kdim[index-1];
                
                if (kstride[index-1] < 0) kstride[index-1] = -kstride[index-1];
                    
                if (kstride[index-1] == 0) kstride[index-1] = 1;
                
                kdim[index-1] += kstride[index-1];
                
                kdim[index-1] /= kstride[index-1];
                
            }
            
            if (ndimfast) *ndimfast = kdim[0];
            
            if (ndimmid) *ndimmid = kdim[1];
            
            if (ndimslow) *ndimslow = kdim[2];
            
            return CBF_SUCCESS;
            
        }

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

        return CBF_SUCCESS;
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
        
        const char *xarray_id;

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
        
        /* See if the array_id is actually an array_section_id */
        
        cbf_failnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id));
        
        if (cbf_cistrcmp(array_id,xarray_id)) {
            
            /* This is actually an array_section, to get the data we need
             a scratch array the size of the entire array, and then
             we need to extract the section from that */
            
            size_t xdimslow;
            
            size_t xdimmid;
            
            size_t xdimfast;
            
            size_t xtotalbytes;
            
            size_t array_offset, section_offset;
            
            void * temparray;
            
            size_t rank;
            
            int ii, iii, jj, jjj, kk, kkk;
            
            size_t start[3];
            
            size_t end[3];
            
            long stride[3];
            
            int dir[3];
            
            size_t arraydim[3];
            
            size_t sectiondim[3];
            
            int errorcode;
            
            cbf_failnez (cbf_get_array_section_rank(handle, array_id, &rank));
            
            if (rank > 3) return CBF_ARGUMENT;
            
            cbf_failnez (cbf_get_3d_array_size (handle, reserved, xarray_id,
                                                &xdimslow, &xdimmid, &xdimfast));
            
            start[0] = start[1] = start[2] = 1L;
            
            end[0] = arraydim[0] = sectiondim[0] = xdimfast;
            
            end[1] = arraydim[1] = sectiondim[1] = xdimmid;
            
            end[2] = arraydim[2] = sectiondim[2] = xdimslow;
            
            stride[0] = stride[1] = stride[2] = dir[0] = dir[1] = dir[2] = 1;
            
            for (ii = 0; ii < (ssize_t)rank; ii++) {
                
                cbf_failnez(cbf_get_array_section_section(handle,array_id,ii+1,
                                                          start+ii, end+ii, stride+ii));
                
                /* fprintf(stderr,"index %d start %ld end %ld stride% ld\n",ii, (long)start[ii], (long)end[ii], (long)stride[ii]); */
                
                if (stride[ii] < 0) dir[ii] = -1;
                
                sectiondim[ii] = ((end[ii]-start[ii]+stride[ii])*dir[ii])/stride[ii];
                
                /* fprintf(stderr,"index %d, dim %ld\n",ii,(long)sectiondim[ii]); */
                
            }
            
            
            
            xtotalbytes = xdimslow*xdimmid*xdimfast*elsize;
            
            cbf_failnez(cbf_alloc(&temparray,NULL,1,xtotalbytes));
            
            errorcode = cbf_get_3d_array(handle, reserved,
                                         xarray_id,
                                         binary_id,
                                         temparray,
                                         eltype,
                                         elsize,
                                         elsign,
                                         xdimslow,
                                         xdimmid,
                                         xdimfast);
            
            /* we need to extract the data elsize bytes at a time
               following the indexing of both the section and the
               overall array */
            
            for (kk = start[2]; kk*dir[2] <  (int)(end[2]*dir[2]); kk += stride[2]) {
                kkk = (kk-start[2])/stride[2];
                for (jj = start[1]; jj*dir[1] <  (int)(end[1]*dir[1]); jj += stride[1]) {
                    jjj = (jj-start[1])/stride[1];
                    for (ii = start[0]; ii*dir[0] <  (int)(end[0]*dir[0]); ii += stride[0]) {
                        iii = (ii-start[0])/stride[0];
                        array_offset = elsize*cbf_offset_1_3(ii,jj,kk,xdimfast,xdimmid);
                        section_offset = elsize*cbf_offset_0_3(iii,jjj,kkk,sectiondim[0],sectiondim[1]);
                        /* fprintf(stderr,"ii,jj,kk: %d %d %d array_offset: %ld,iii,jjj,kkk: %d %d %d section_offset: %ld\n",
                                ii,jj,kk,(unsigned long)array_offset,iii,jjj,kkk,(unsigned long)section_offset); */
                        memcpy(((char *)array)+section_offset,((char *)temparray)+array_offset,elsize);
                                                            
                    }
                }
            }
            
            errorcode |= cbf_free(&temparray,NULL);
            
            return errorcode;
            
        }
            
            
        /* Get the index directions from the array_structure_list category */

        done [1] = done [2] = done[3] = 0;

        direction [1] = direction [2] = direction [3] = 1;

        /* If no array_id, use default directions */

        if (array_id) {

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

        }


        /* Find the binary data */

        cbf_failnez (cbf_find_category (handle, "array_data"))

        if (array_id) {
            cbf_failnez (cbf_find_column   (handle, "array_id"))
            cbf_failnez (cbf_find_row      (handle, array_id))
        } else {
            cbf_failnez(cbf_rewind_row(handle));
        }

        if ( binary_id && *binary_id != 0) {

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
        
        const char * xarray_id;

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
        
        /* See if the array_id is actually an array_section_id */
        
        cbf_failnez(cbf_get_array_section_array_id(handle,array_id,&xarray_id));
        
        if (cbf_cistrcmp(array_id,xarray_id)) {
            
            /* This is actually an array section.  To write the section,
             we need the full array.  If it does not exist, we have to
             start it off as an array of zeros.  In either case we can then
             insert the section into it and write it back.  */
            
            size_t xdimslow;
            
            size_t xdimmid;
            
            size_t xdimfast;
            
            size_t xtotalbytes;
            
            size_t array_offset, section_offset;
            
            void * temparray;
            
            size_t rank;
            
            int ii, iii, jj, jjj, kk, kkk;
            
            size_t start[3];
            
            size_t end[3];
            
            long stride[3];
            
            int dir[3];
            
            size_t arraydim[3];
            
            size_t sectiondim[3];
            
            int errorcode;
            
            cbf_failnez (cbf_get_array_section_rank(handle, array_id, &rank));
            
            if (rank > 3) return CBF_ARGUMENT;
            
            cbf_failnez (cbf_get_3d_array_size (handle, reserved, xarray_id,
                                                &xdimslow, &xdimmid, &xdimfast));
            
            start[0] = start[1] = start[2] = 1;
            
            end[0] = arraydim[0] = sectiondim[0] = xdimfast;
            
            end[1] = arraydim[1] = sectiondim[1] = xdimmid;
            
            end[2] = arraydim[2] = sectiondim[2] = xdimslow;
            
            stride[0] = stride[1] = stride[2] = dir[0] = dir[1] = dir[2] = 1;
            
            for (ii = 0; ii < (ssize_t)rank; ii++) {
                
                cbf_failnez(cbf_get_array_section_section(handle,array_id,ii+1,
                                                          start+ii, end+ii, stride+ii));
                
                if (stride[ii] < 0) dir[ii] = -1;
                
                sectiondim[ii] = ((end[ii]-start[ii]+stride[ii])*dir[ii])/stride[ii];
                
            }
            
            
            
            xtotalbytes = xdimslow*xdimmid*xdimfast*elsize;
            
            cbf_failnez(cbf_alloc(&temparray,NULL,1,xtotalbytes));
            
            if (cbf_get_3d_array(handle, reserved,
                                 xarray_id,
                                 binary_id,
                                 temparray,
                                 eltype,
                                 elsize,
                                 elsign,
                                 xdimslow,
                                 xdimmid,
                                 xdimfast)) {
                
                memset(temparray,0,xtotalbytes);
                
            }
            
            /* we need to store the data elsize bytes at a time
             following the indexing of both the section and the
             overall array */
            
            for (kk = start[2]; kk*dir[2] <  (int)(end[2]*dir[2]); kk += stride[2]) {
                kkk = (kk-start[2])/stride[2];
                for (jj = start[1]; jj*dir[1] <  (int)(end[1]*dir[1]); jj += stride[1]) {
                    jjj = (jj-start[1])/stride[1];
                    for (ii = start[0]; ii*dir[0] <  (int)(end[0]*dir[0]); ii += stride[0]) {
                        iii = (ii-start[0])/stride[0];
                        array_offset = elsize*cbf_offset_1_3(ii,jj,kk,xdimfast,xdimmid);
                        section_offset = elsize*cbf_offset_0_3(iii,jjj,kkk,sectiondim[0],sectiondim[1]);
                        memcpy(((char *)temparray)+array_offset,((char *)array)+section_offset,elsize);
                        
                    }
                }
            }
            
            /* Now write back the entire array */
            
            errorcode = cbf_set_3d_array ( handle, reserved, xarray_id,
                                          binary_id, compression,temparray,
                                          eltype,elsize,elsign,xdimslow,xdimmid,xdimfast);
            
            errorcode |= cbf_free(&temparray,NULL);
            
            return errorcode;
            
        }

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
            case (CBF_NIBBLE_OFFSET):
                cbf_failnez (cbf_set_value      (handle,"nibble_offset"))
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


    /* Count the ancestors of an axis */

    int cbf_count_axis_ancestors (cbf_handle handle,
                               const char *axis_id,
                               unsigned int *ancestors) {

        const char * cur_axis;
        const char * depends_on;
        const char * deptype;
        int curlevel;
        unsigned int maxlevel;

        /* Check for valid arguments */

        if (!handle || !axis_id || !ancestors ) return CBF_ARGUMENT;

        /* Get the axis dependencies */

        cbf_failnez (cbf_find_category (handle, "axis"));
        cbf_failnez (cbf_count_rows(handle,&maxlevel));
        if (maxlevel < 1) return CBF_FORMAT;
        maxlevel --;
        curlevel = maxlevel;
        cbf_failnez (cbf_find_column   (handle, "id"));
        cbf_failnez (cbf_find_row      (handle, axis_id));
        cbf_failnez (cbf_get_value     (handle, &cur_axis));

        *ancestors = 0;

        while (curlevel >= 0) {

            if (curlevel==0
                || cbf_find_column   (handle, "depends_on")
                || cbf_get_value     (handle, &depends_on)
                || !(depends_on)
                || cbf_get_typeofvalue(handle, &deptype)
                || !cbf_cistrcmp(deptype,"null")) {
                
                return CBF_SUCCESS;
            }
            
            if ( !cbf_cistrcmp(depends_on,".")
                 || !cbf_cistrcmp(depends_on,"?") ){
                
                cbf_debug_print3("non-null dependency '%s' for axis '%s'\n",
                                 depends_on,
                                 axis_id);
                
                return CBF_SUCCESS;

            }

            cur_axis = depends_on;
            curlevel--;
            (*ancestors)++;
            cbf_failnez (cbf_find_column(handle, "id"));
            cbf_failnez (cbf_find_row   (handle, cur_axis));

        }
        return CBF_FORMAT;

    }


    /* Get the specified ancestor of an axis */

    int cbf_get_axis_ancestor (cbf_handle handle,
                               const char *axis_id,
                               const unsigned int ancestor_index,
                               const char * *ancestor) {

        const char * cur_axis;
        const char * depends_on;
        const char * deptype;
        int curlevel;

        /* Check for valid arguments */

        if (!handle || !axis_id || !ancestor ) return CBF_ARGUMENT;

        *ancestor = NULL;
        curlevel = ancestor_index;

        /* Get the axis dependencies */

        cbf_failnez (cbf_find_category (handle, "axis"));
        cbf_failnez (cbf_find_column   (handle, "id"));
        cbf_failnez (cbf_find_row      (handle, axis_id));
        cbf_failnez (cbf_get_value     (handle, &cur_axis));

        while (curlevel >= 0) {
            
            if (curlevel==0
                || cbf_find_column   (handle, "depends_on")
                || cbf_get_value     (handle, &depends_on)
                || !((depends_on)[0])
                || cbf_get_typeofvalue(handle, &deptype)
                || !cbf_cistrcmp(deptype,"null")) {
                
                if (curlevel > 0) return CBF_NOTFOUND;
                *ancestor = cur_axis;
                return CBF_SUCCESS;
                
                
            }
            if (!cbf_cistrcmp(depends_on,".")
                || !cbf_cistrcmp(depends_on,"?")){
                
                cbf_debug_print3("non-null dependency '%s' for axis '%s'\n",
                                 depends_on,
                                 axis_id);
                
                
                if (curlevel > 0) return CBF_NOTFOUND;
                *ancestor = cur_axis;
                return CBF_SUCCESS;
                
            }
            
            cur_axis = depends_on;
            curlevel--;
            cbf_failnez (cbf_find_column(handle, "id"));
            cbf_failnez (cbf_find_row   (handle, cur_axis));
            
        }
        return CBF_NOTFOUND;

    }


    /* Get the axis, if any, on which this axis depends
     Will always return "." for the null case */

    int cbf_get_axis_depends_on (cbf_handle handle, const char *axis_id,
                                 const char * *depends_on)
    {
        
        const char * deptype;
        
        /* Check for valid arguments */
        
        if (!handle || !depends_on) return CBF_ARGUMENT;
        
        
        /* Get the axis dependency */
        
        cbf_failnez (cbf_find_category (handle, "axis"))
        cbf_failnez (cbf_find_column   (handle, "id"))
        cbf_failnez (cbf_find_row      (handle, axis_id))
        
        if (cbf_find_column   (handle, "depends_on")) {
            
            *depends_on = ".";  return CBF_SUCCESS;

        }
        
        if (cbf_get_value     (handle, depends_on)
            || !(*depends_on)) {
            
            *depends_on = ".";  return CBF_SUCCESS;
            
        }
        
        if (cbf_get_typeofvalue(handle,&deptype)
            || !cbf_cistrcmp(deptype,"null")) {
            
            *depends_on = ".";  return CBF_SUCCESS;
            
        }
        
        
        if (!cbf_cistrcmp(*depends_on,".")
            || !cbf_cistrcmp(*depends_on,"?")) {
            
            cbf_debug_print3("non-null dependency '%s' for axis '%s'\n",
                             *depends_on,
                             axis_id);

            
            *depends_on = ".";  return CBF_SUCCESS;
            
        }
        
        return CBF_SUCCESS;
        
    }



    /* Get the axis equipment */

    int cbf_get_axis_equipment (cbf_handle handle, const char *axis_id,
                           const char * *equipment)
    {

        /* Get the axis equipment */
        
        cbf_failnez (cbf_find_category (handle, "axis"))
        cbf_failnez (cbf_find_column   (handle, "id"))
        cbf_failnez (cbf_find_row      (handle, axis_id))
        if (cbf_find_column   (handle, "equipment")) {
            *equipment = ".";  return CBF_SUCCESS;
        }
        if (cbf_get_value     (handle, equipment)
            || !(*equipment)
            || !((*equipment)[0])) {
            *equipment = ".";  return CBF_SUCCESS;
        }
        return CBF_SUCCESS;

    }

    /* Get the axis equipment_component */

    int cbf_get_axis_equipment_component (cbf_handle handle,
                                          const char *axis_id,
                                const char * *equipment_component)
    {

        /* Get the axis equipment component */
        
        cbf_failnez (cbf_find_category (handle, "axis"))
        cbf_failnez (cbf_find_column   (handle, "id"))
        cbf_failnez (cbf_find_row      (handle, axis_id))
        if (cbf_find_column   (handle, "equipment_component")) {
            *equipment_component = ".";  return CBF_SUCCESS;
        }
        if (cbf_get_value     (handle, equipment_component)||
            !((*equipment_component)[0])) {
            *equipment_component = ".";  return CBF_SUCCESS;
        }
        return CBF_SUCCESS;

    }

    /* Get an axis rotation */

    int cbf_get_axis_rotation (cbf_handle handle, const char *axis_id,
                               double *rotation)
    {
        if (!handle || !axis_id || !rotation )
            return CBF_ARGUMENT;

        /* Read from the axis category */

        cbf_failnez (cbf_find_category   (handle, "axis"))
        cbf_failnez (cbf_find_column     (handle, "id"))
        cbf_failnez (cbf_find_row        (handle, axis_id))
        if (cbf_find_column( handle, "rotation")) {

            *rotation = 0.0;
            return CBF_SUCCESS;

        }

        if (cbf_get_doublevalue (handle, rotation)) *rotation = 0.;

        return 0;

    }


    /* Get the axis rotation_axis */

    int cbf_get_axis_rotation_axis (cbf_handle handle,
                                          const char *axis_id,
                                          const char * *rotation_axis)
    {

        /* Get the axis rotation_axis */

        cbf_failnez (cbf_find_category (handle, "axis"))
        cbf_failnez (cbf_find_column   (handle, "id"))
        cbf_failnez (cbf_find_row      (handle, axis_id))
        if (cbf_find_column   (handle, "rotation_axis")) {
            *rotation_axis = ".";  return CBF_SUCCESS;
        }
        if (cbf_get_value     (handle, rotation_axis)||
            !((*rotation_axis)[0])) {
            *rotation_axis = ".";  return CBF_SUCCESS;
        }
        return CBF_SUCCESS;

    }



    /* Get an axis offset */

    int cbf_get_axis_offset (cbf_handle handle, const char *axis_id,
                             double *offset1,
                             double *offset2,
                             double *offset3)
    {
        if (!handle || !axis_id || !offset1 || !offset2 || !offset3)
            return CBF_ARGUMENT;

        /* Read from the axis category */

        cbf_failnez (cbf_find_category   (handle, "axis"))
        cbf_failnez (cbf_find_column     (handle, "id"))
        cbf_failnez (cbf_find_row        (handle, axis_id))
        cbf_failnez (cbf_find_column     (handle, "offset[1]"))
        if (cbf_get_doublevalue (handle, offset1)) *offset1 = 0.;
        cbf_failnez (cbf_find_column     (handle, "offset[2]"))
        if (cbf_get_doublevalue (handle, offset2)) *offset2 = 0.;
        cbf_failnez (cbf_find_column     (handle, "offset[3]"))
        if (cbf_get_doublevalue (handle, offset3)) *offset3 = 0.;

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
        if (!handle || !axis_id || !vector1 || !vector2 || !vector3)
            return CBF_ARGUMENT;

        /* Read from the axis category */

        cbf_failnez (cbf_find_category   (handle, "axis"))
        cbf_failnez (cbf_find_column     (handle, "id"))
        cbf_failnez (cbf_find_row        (handle, axis_id))
        cbf_failnez (cbf_find_column     (handle, "vector[1]"))
        if (cbf_get_doublevalue (handle, vector1)) *vector1 = 0.;
        cbf_failnez (cbf_find_column     (handle, "vector[2]"))
        if (cbf_get_doublevalue (handle, vector2)) *vector2 = 0.;
        cbf_failnez (cbf_find_column     (handle, "vector[3]"))
        if (cbf_get_doublevalue (handle, vector3)) *vector3 = 0.;

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

        if (reserved != 0 || !refsetting || !axis_id )

            return CBF_ARGUMENT;

        /* Get the axis type */

        cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

        if (type == CBF_GENERAL_AXIS || !cbf_cistrcmp(axis_id,".")) {

            *refsetting = 0.;

        }

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

    static int cbf_make_positioner (cbf_positioner *positioner)
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

        (*positioner)->matrix_ratio_used = 0;

        (*positioner)->axis_index_limit = 1000000;

        return 0;
    }


    /* Free a positioner */

    int cbf_free_positioner (cbf_positioner positioner)
    {
        int errorcode;

        void *memblock;

        void *vaxis;

        void *vname;

        void *adon, *arot;

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

                if (positioner->axis [i].rotation_axis) {

                    arot = (void *)(positioner->axis [i].rotation_axis);

                    errorcode |= cbf_free ((void **) &arot, NULL);

                    positioner->axis [i].rotation_axis = NULL;

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


     /* Add a positioner axis with rotation*/

    static int cbf_add_positioner_axis_wrot (cbf_positioner positioner,
                                 const char    *name,
                                 const char    *depends_on,
                                 const char    *rotation_axis,
                                 cbf_axis_type  type,
                                 double         vector1,
                                 double         vector2,
                                 double         vector3,
                                 double         offset1,
                                 double         offset2,
                                 double         offset3,
                                 double         start,
                                 double         increment,
                                 double         rotation)
    {
        int errorcode = 0;

        cbf_axis_struct axis;

        void  *vaxis;

        void  *vname;

        double length;

        /* Check the arguments */

        if (!name || !positioner || (type != CBF_TRANSLATION_AXIS &&
                                     type != CBF_ROTATION_AXIS &&
                                     type != CBF_GENERAL_AXIS ))

            return CBF_ARGUMENT;

        length = vector1 * vector1 + vector2 * vector2 + vector3 * vector3;

        if (length <= 0.0)

            return CBF_ARGUMENT;

        /* Ensure we are not in a loop */

        if (positioner->axes >= positioner->axis_index_limit) return CBF_ALLOC;


        /* Allocate memory and copy the axis names */

        axis.name = NULL;

        axis.name = (char *)cbf_copy_string(NULL,name,0);

        axis.depends_on = NULL;

        axis.rotation_axis = NULL;

        if (depends_on) {

            axis.depends_on = (char *)cbf_copy_string(NULL,depends_on,0);

        }

        if (rotation_axis) {

            axis.rotation_axis = (char *)cbf_copy_string(NULL,rotation_axis,0);

        }

        axis.depends_on_index = axis.rotation_axis_index = -1;

        axis.depdepth = 0;

        vaxis = (void *)(positioner->axis);

        errorcode = cbf_realloc ((void **) &vaxis,
                                 &(positioner->axes),
                                 sizeof (cbf_axis_struct),
                                 positioner->axes + 1);

        positioner->axis = (cbf_axis_struct *)vaxis;

        if (errorcode)
        { int nerrorcode;

            void * vdepends_on;

            void * vrotation_axis;

            vname = (void *)axis.name;

            vdepends_on = (void *)axis.depends_on;

            vrotation_axis = (void *)axis.rotation_axis;

            nerrorcode = errorcode |
              cbf_free (&vname, NULL) |
              cbf_free (&vdepends_on, NULL) |
              cbf_free (&vrotation_axis, NULL);

            axis.name = NULL;

            axis.depends_on = NULL;

            axis.rotation_axis = NULL;

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
        axis.rotation  = rotation;

        positioner->axis [positioner->axes - 1] = axis;

        positioner->matrix_is_valid = 0;

        return 0;
    }


    /* Add a goniometer axis from a file */

    static int cbf_read_positioner_axis (cbf_handle      handle,
                                  unsigned int    reserved,
                                  cbf_positioner  positioner,
                                  const char     *axis_id,
                                  int             read_setting)
    {
        const char *prev_id;

        const char *rot_id;

        cbf_axis_type axis_type;
        
        const char *deptype;

        double vector1, vector2, vector3, offset1, offset2, offset3;

        double start, increment, rot;

        int errorcode;

        cbf_failnez (cbf_find_category    (handle, "axis"));
        cbf_failnez (cbf_find_column      (handle, "id"));
        cbf_failnez (cbf_find_row         (handle, axis_id));
        cbf_failnez (cbf_find_column      (handle, "depends_on"));
        cbf_failnez (cbf_get_value        (handle, &prev_id));
        cbf_failnez (cbf_get_typeofvalue  (handle, &deptype));
        if (cbf_cistrcmp(deptype, "null") == 0) {
            prev_id = NULL;
        }

        if (!cbf_find_column (handle, "rotation_axis")) {

            cbf_failnez (cbf_get_value    (handle, &rot_id));

        } else {

            rot_id = NULL;

        }

        if (!cbf_find_column (handle, "rotation")) {

            cbf_failnez (cbf_get_doublevalue (handle, &rot));

        } else {

            rot = 0.0;

        }

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

        start = increment = 0.;

        errorcode = 0;

        /* fprintf(stderr," cbf_read_positioner_axis , axis = %s, read_setting = %d\n",axis_id, read_setting);*/

        if (read_setting && axis_type != CBF_GENERAL_AXIS)
        {

            errorcode = cbf_get_axis_setting (handle, reserved, axis_id,
                                              &start,
                                              &increment);

            if (read_setting < 0) {
                errorcode = cbf_get_axis_reference_setting (handle, reserved, axis_id,
                                                            &start);
            }

            if ( (read_setting == -2 || read_setting == 2)
                && (errorcode == CBF_NOTFOUND || errorcode == CBF_FORMAT) ) {

                start = increment = 0;

                errorcode = 0;

            }

            cbf_failnez(errorcode);

        }

        cbf_failnez (cbf_add_positioner_axis_wrot (positioner,
                                              axis_id,
                                              prev_id,
                                              rot_id,
                                              axis_type,
                                              vector1, vector2, vector3,
                                              offset1, offset2, offset3,
                                              start, increment, rot))

        return 0;
    }



    /* construct a positioner for a given final axis */

    int cbf_construct_positioner (cbf_handle handle,
                                  cbf_positioner *positioner, const char *axis_id)
    {

        int errorcode;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

        unsigned int axis_index_limit;

        if (!positioner || !axis_id)

            return CBF_ARGUMENT;

        /* fprintf(stderr," cbf_construct_positioner, axis %s\n",axis_id); */

        errorcode = 0;


        /* Construct the positioner */

        cbf_failnez (cbf_make_positioner (positioner));

        /* Set a limit on the number of axes in the positioner

           As a crude limit to break dependency loops, N*(N-1)/2

           will suffice.

         */

        cbf_failnez(cbf_find_category(handle,"axis"));

        cbf_failnez(cbf_count_rows(handle,&axis_index_limit));

        axis_index_limit *= (axis_index_limit-1);

        axis_index_limit /= 2;

        (*positioner)->axis_index_limit = axis_index_limit;

        /* read the first axis */

        errorcode = cbf_read_positioner_axis (handle,
                                              0, /* reserved */
                                              *positioner,
                                              axis_id, 2);
        axis_index = 0;

        if (!errorcode) {

            do{

                target_axis = (*positioner)->axis[axis_index].depends_on;

                rotation_axis = (*positioner)->axis[axis_index].rotation_axis;

                if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *positioner,
                                                          target_axis, 2);

                    (*positioner)->axis[axis_index].depends_on_index
                        = (*positioner)->axes-1;

                    (*positioner)->axis[(*positioner)->axes-1].depdepth
                        = cbf_max( (*positioner)->axis[(*positioner)->axes-1].depdepth,
                           (*positioner)->axis[axis_index].depdepth+1);

                    if (!errorcode) break;

                }

                if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

            errorcode = cbf_read_positioner_axis (handle,
                                                  0, /* reserved */
                                                  *positioner,
                                                          rotation_axis, 2);

                    (*positioner)->axis[axis_index].rotation_axis_index
                        = (*positioner)->axes-1;

                    (*positioner)->axis[(*positioner)->axes-1].depdepth
                    = cbf_max( (*positioner)->axis[(*positioner)->axes-1].depdepth,
                              (*positioner)->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

            }

                axis_index++;

            } while (axis_index < (*positioner)->axes);

        }

        if (errorcode)
        {
            errorcode |= cbf_free_positioner (*positioner);

            *positioner = NULL;
        }

        return errorcode;
    }

    /* construct a reference positioner for a given final axis */

    int cbf_construct_reference_positioner (cbf_handle handle,
                                            cbf_positioner *positioner, const char *axis_id)
    {

        int errorcode;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

        unsigned int axis_index_limit;

        if (!positioner || !axis_id)

            return CBF_ARGUMENT;

        /* fprintf(stderr," cbf_construct_positioner, axis %s\n",axis_id); */

        errorcode = 0;


        /* Construct the positioner */

        cbf_failnez (cbf_make_positioner (positioner))

        /* Set a limit on the number of axes in the positioner

         As a crude limit to break dependency loops, N*(N-1)/2

         will suffice.

         */

        cbf_failnez(cbf_find_category(handle,"axis"));

        cbf_failnez(cbf_count_rows(handle,&axis_index_limit));

        axis_index_limit *= (axis_index_limit-1);

        axis_index_limit /= 2;

        (*positioner)->axis_index_limit = axis_index_limit;

        /* read the first axis */

            errorcode = cbf_read_positioner_axis (handle,
                                                  0, /* reserved */
                                                  *positioner,
                                              axis_id, -2);
        axis_index = 0;

            if (!errorcode) {

            do{

                target_axis = (*positioner)->axis[axis_index].depends_on;

                rotation_axis = (*positioner)->axis[axis_index].rotation_axis;

                if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *positioner,
                                                          target_axis, -2);

                    (*positioner)->axis[axis_index].depends_on_index
                    = (*positioner)->axes-1;

                    (*positioner)->axis[(*positioner)->axes-1].depdepth
                    = cbf_max( (*positioner)->axis[(*positioner)->axes-1].depdepth,
                              (*positioner)->axis[axis_index].depdepth+1);

                    if (!errorcode) break;

            }

                if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *positioner,
                                                          rotation_axis, -2);

                    (*positioner)->axis[axis_index].depends_on_index
                    = (*positioner)->axes-1;

                    (*positioner)->axis[(*positioner)->axes-1].depdepth
                    = cbf_max( (*positioner)->axis[(*positioner)->axes-1].depdepth,
                              (*positioner)->axis[axis_index].depdepth+1);

                    if (!errorcode) break;

        }

                axis_index++;

            } while (axis_index < (*positioner)->axes);

        }

        if (errorcode)
        {
            errorcode |= cbf_free_positioner (*positioner);

            *positioner = NULL;
        }

        return errorcode;

    }


    /* Calculate a position given initial coordinates */

    static int cbf_calculate_position (cbf_positioner positioner,
                                unsigned int   reserved,
                                double         ratio,
                                double         initial1,
                                double         initial2,
                                double         initial3,
                                double        *final1,
                                double        *final2,
                                double        *final3)
    {
        size_t i, istart;

        int idep;

        int irot;

        double setting;

        double rangle;

        if (!positioner)

            return CBF_ARGUMENT;

        if (reserved != 0)

            return CBF_ARGUMENT;

        istart = 0;

        for (i = 0; i < positioner->axes; i++)
        {
            setting = positioner->axis [i].start + ratio *
            positioner->axis [i].increment;

            if (positioner->axis [i].setting != setting)
            {
                positioner->matrix_is_valid = 0;

                positioner->axis [i].setting = setting;
            }

            if (positioner->axis [i].depdepth == 0) istart = i;
        }


        if (!positioner->matrix_is_valid || positioner->matrix_ratio_used != ratio )
        {


            positioner->matrix_ratio_used = ratio;

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

            idep = 0;

            irot = -1;

            rangle = 0.;

            i = istart;

            while (1) {

                setting = positioner->axis [i].setting;

                if (positioner->axis [i].type == CBF_TRANSLATION_AXIS && irot == -1)
                {
                    positioner->matrix [0][3] += setting * positioner->axis [i].vector [0];
                    positioner->matrix [1][3] += setting * positioner->axis [i].vector [1];
                    positioner->matrix [2][3] += setting * positioner->axis [i].vector [2];
                    /* fprintf(stderr," calculate position, axis %s, translate [%g, %g, %g]\n",
                            positioner->axis [i].name,
                            setting * positioner->axis [i].vector [0],
                            setting * positioner->axis [i].vector [1],
                            setting * positioner->axis [i].vector [2]); */


                }
                else
                {
                    double s, x, y, z, w,
                    xx, yy, zz, xy, xz, xw, yz, yw, zw;

                    double rotation [3][3], product [3][4];

                    int r1, c1r2, c2;

                    setting += rangle;

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

                    /* fprintf(stderr," calculate position, axis %s, rotate [%g + i*%g + j*%g + k*%g]\n",
                            positioner->axis [i].name, w, x, y, z); */


                    for (r1 = 0; r1 < 3; r1++)

                        for (c2 = 0; c2 < 4; c2++)
                        {
                            product [r1][c2] = 0;

                            for (c1r2 = 0; c1r2 < 3; c1r2++)

                                product [r1][c2] += rotation [r1][c1r2] *
                                positioner->matrix [c1r2][c2];
                        }

                    for (r1 = 0; r1 < 3; r1++)

                        for (c2 = 0; c2 < 4; c2++)

                            positioner->matrix [r1][c2] = product [r1][c2];
                }

                positioner->matrix [0][3] += positioner->axis [i].offset [0];
                positioner->matrix [1][3] += positioner->axis [i].offset [1];
                positioner->matrix [2][3] += positioner->axis [i].offset [2];

                if (irot == -1) {

                    irot = positioner->axis[i].rotation_axis_index;

                    rangle = positioner->axis[i].rotation;

                    if (irot >= 0 && fabs(rangle) >= 1.e-38) {

                        if (cbf_cistrcmp(positioner->axis[i].depends_on,
                                         positioner->axis[irot].depends_on)
                            && cbf_cistrcmp(positioner->axis[i].depends_on,
                                            positioner->axis[irot].name)) {

                            /* fprintf (stderr," CBFlib: cbf_calculate_position: "
                                     "different rotation axis %s dependency for %s, %s vs. %s "
                                     "not supported\n",
                                     positioner->axis[irot].name,
                                     positioner->axis[i].name,
                                     positioner->axis[i].depends_on,
                                     positioner->axis[irot].depends_on); */

                            return CBF_FORMAT;

                        }

                        i = irot;

                        continue;
                    }

                    irot = -1;

                }

                idep = positioner->axis[i].depends_on_index;

                if (idep < 0) break;

                i = idep;

                irot = -1;

                rangle = 0.0;

            }

            positioner->matrix_is_valid = 1;
        }

        /* fprintf(stderr," Overall matrix [[%g %g %g]] + %g\n"
                "                [[%g %g %g]] + %g\n"
                "                [[%g %g %g]] + %g\n",
                positioner->matrix [0][0],
                positioner->matrix [0][1],
                positioner->matrix [0][2],
                positioner->matrix [0][3],
                positioner->matrix [1][0],
                positioner->matrix [1][1],
                positioner->matrix [1][2],
                positioner->matrix [1][3],
                positioner->matrix [2][0],
                positioner->matrix [2][1],
                positioner->matrix [2][2],
                positioner->matrix [2][3]); */


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

    static int cbf_calculate_initial_position (cbf_positioner positioner,
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

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

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

        /* Complete the connectivity of the positioner */

        axis_index = 0;

        do {

            size_t index;

            int found;

            target_axis = (*goniometer)->axis[axis_index].depends_on;

            rotation_axis = (*goniometer)->axis[axis_index].rotation_axis;
            
            if (target_axis
                && (!cbf_cistrcmp (target_axis,".")
                    ||!cbf_cistrcmp (target_axis,"?"))) {
                    
                    cbf_debug_print3("non-null dependency '%s' for axis '%s'\n",
                                     target_axis,
                                     (*goniometer)->axis[axis_index].name);
                    target_axis = NULL;
                    
                }

            if (rotation_axis
                && (!cbf_cistrcmp (rotation_axis,".")
                    ||!cbf_cistrcmp (rotation_axis,"?"))) {
                    
                    cbf_debug_print3("non-null rotation_axis '%s' for axis '%s'\n",
                                     rotation_axis,
                                     (*goniometer)->axis[axis_index].name);
                    rotation_axis = NULL;
                    
                }

            if ( target_axis ) {

                found = 0;

                for (index = 0; index < (*goniometer)->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (target_axis,(*goniometer)->axis[index].name)== 0)
                    {

                        (*goniometer)->axis[axis_index].depends_on_index = index;

                        found = 1;

                        (*goniometer)->axis[index].depdepth
                        = cbf_max( (*goniometer)->axis[index].depdepth,
                                  (*goniometer)->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *goniometer,
                                                          target_axis, 2);

                    (*goniometer)->axis[axis_index].depends_on_index
                    = (*goniometer)->axes-1;

                    (*goniometer)->axis[(*goniometer)->axes-1].depdepth
                    = cbf_max( (*goniometer)->axis[(*goniometer)->axes-1].depdepth,
                              (*goniometer)->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            if ( rotation_axis ) {

                found = 0;

                for (index = 0; index < (*goniometer)->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (rotation_axis,(*goniometer)->axis[index].name)== 0)
                    {

                        (*goniometer)->axis[axis_index].rotation_axis_index = index;

                        found = 1;

                        (*goniometer)->axis[index].depdepth
                        = cbf_max( (*goniometer)->axis[index].depdepth,
                                  (*goniometer)->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {


                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *goniometer,
                                                          rotation_axis, 2);

                    (*goniometer)->axis[axis_index].rotation_axis_index
                    = (*goniometer)->axes-1;

                    (*goniometer)->axis[(*goniometer)->axes-1].depdepth
                    = cbf_max( (*goniometer)->axis[(*goniometer)->axes-1].depdepth,
                              (*goniometer)->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            axis_index++;

        } while (axis_index < (*goniometer)->axes);

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
        
        const char *array_section_id;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

        const char *surface_axis [2];  /* fast, slow */
        
        const char *array_colname;

        double displacement [2], increment [2];

        cbf_positioner positioner;
        
        /* fprintf(stderr,"cbf_construct_detector elno %d\n",element_number); */

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

        cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id));
        cbf_failnez (cbf_get_array_section_id  (handle, element_number, &array_section_id))
        cbf_failnez (cbf_find_category (handle, "array_structure_list"))
        array_colname = "array_section_id";
        if (cbf_find_column(handle,array_colname)) {
            array_colname = "array_id";
            cbf_failnez (cbf_find_category (handle, "array_structure_list"))
            cbf_failnez (cbf_find_column   (handle, array_colname));
        }

        surface_axis [0] = surface_axis [1] = NULL;

        while (cbf_find_nextrow (handle, array_section_id) == 0)
        {
            cbf_failnez (cbf_find_column      (handle, "precedence"))
            cbf_failnez (cbf_get_integervalue (handle, &precedence))

            if (precedence < 1 || precedence > 2)

                return CBF_FORMAT;

            if (surface_axis [precedence - 1])

                return CBF_FORMAT;

            cbf_failnez (cbf_find_column (handle, "axis_set_id"))
            cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
            cbf_failnez (cbf_find_column   (handle, array_colname));
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

       /* fprintf(stderr," Construct the positioner\n"); */

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

        if (errorcode)
        {
            errorcode |= cbf_free_positioner (positioner);

            return errorcode | cbf_free ((void **) detector, NULL);
        }

        /* Complete the connectivity of the positioner */

        /* fprintf(stderr," Complete the connectivity of the positioner\n"); */

        axis_index = 0;

        do {

            size_t index;

            int found;
            
            /* fprintf(stderr,"connecting %s\n",positioner->axis[axis_index].name); */

            target_axis = positioner->axis[axis_index].depends_on;

            rotation_axis = positioner->axis[axis_index].rotation_axis;

            /* fprintf(stderr," axis %s depends_on %s rotation_axis %s rotation %g\n",
                    positioner->axis[axis_index].name,
                    target_axis, rotation_axis,
                    positioner->axis[axis_index].rotation); */

            if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (target_axis,positioner->axis[index].name)== 0)
                    {

                        positioner->axis[axis_index].depends_on_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          target_axis, 2);

                    positioner->axis[axis_index].depends_on_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (rotation_axis,positioner->axis[index].name)== 0)
                    {

                        positioner->axis[axis_index].rotation_axis_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {


                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          rotation_axis, 2);

                    positioner->axis[axis_index].rotation_axis_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            axis_index++;

        } while (axis_index < positioner->axes);

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
        
        const char *array_section_id;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

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
        cbf_failnez (cbf_get_array_section_id  (handle, element_number, &array_section_id))
        cbf_failnez (cbf_require_category (handle, "array_structure_list"))
        if (cbf_find_column(handle,"array_section_id")) {
            cbf_failnez (cbf_find_column   (handle, "array_id"));
        }

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
            if (cbf_find_column(handle,"array_section_id")) {
                cbf_failnez (cbf_find_column   (handle, "array_id"));
            }
        }

        if (!surface_axis[0]) {
            cbf_failnez (cbf_require_column  (handle, "array_id"))
            cbf_failnez (cbf_new_row         (handle))
            cbf_failnez (cbf_set_value       (handle, array_id))
            cbf_failnez (cbf_require_column  (handle, "array_section_id"))
            cbf_failnez (cbf_set_value       (handle, array_section_id))
            cbf_failnez (cbf_require_column  (handle, "precedence"))
            cbf_failnez (cbf_set_integervalue(handle,1))
            cbf_failnez (cbf_require_column  (handle, "axis_set_id"))
            cbf_failnez (cbf_require_value   (handle, &surface_axis [0], "ELEMENT_X"))

        }

        if (!surface_axis[1]) {
            cbf_failnez (cbf_require_column  (handle,"array_id"))
            cbf_failnez (cbf_new_row         (handle))
            cbf_failnez (cbf_set_value       (handle, array_id))
            cbf_failnez (cbf_require_column  (handle, "array_section_id"))
            cbf_failnez (cbf_set_value       (handle, array_section_id))
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

        if (errorcode)
        {
            errorcode |= cbf_free_positioner (positioner);

            return errorcode | cbf_free ((void **) detector, NULL);
        }

        /* Complete the connectivity of the positioner */

        axis_index = 0;

        do {

            size_t index;

            int found;

            target_axis = positioner->axis[axis_index].depends_on;

            rotation_axis = positioner->axis[axis_index].rotation_axis;

            if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (target_axis,positioner->axis[index].name)== 0)
        {

                        positioner->axis[axis_index].depends_on_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

        }

                }

                if (!found) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          target_axis, 2);

                    positioner->axis[axis_index].depends_on_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (rotation_axis,positioner->axis[index].name)== 0)
                    {

                        positioner->axis[axis_index].rotation_axis_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {


                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          rotation_axis, 2);

                    positioner->axis[axis_index].rotation_axis_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            axis_index++;

        } while (axis_index < positioner->axes);


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
        
        const char * array_section_id;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

        const char *surface_axis [2];

        double displacement [2], increment [2];

        cbf_positioner positioner;

        if (!detector)

            return CBF_ARGUMENT;


        /* Get the detector id */

        cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id));

        cbf_failnez (cbf_find_category (handle, "diffrn_detector"));
        cbf_failnez (cbf_find_column   (handle, "diffrn_id"));
        cbf_failnez (cbf_find_row      (handle, diffrn_id));
        cbf_failnez (cbf_find_column   (handle, "id"));
        cbf_failnez (cbf_get_value     (handle, &id));


        /* Construct the detector surface */

        cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id));
        cbf_failnez (cbf_get_array_section_id( handle,element_number,&array_section_id));
        cbf_failnez (cbf_find_category (handle, "array_structure_list"));
        if (cbf_find_column(handle,"array_section_id")) {
            cbf_failnez (cbf_find_column   (handle, "array_id"));
        }

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
            if (cbf_find_column(handle,"array_section_id")) {
                cbf_failnez (cbf_find_column   (handle, "array_id"));
            }
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
        if (errorcode)
        {
            errorcode |= cbf_free_positioner (positioner);

            return errorcode | cbf_free ((void **) detector, NULL);
        }


        /* Complete the connectivity of the positioner */

        axis_index = 0;

        do {

            size_t index;

            int found;

            target_axis = positioner->axis[axis_index].depends_on;

            rotation_axis = positioner->axis[axis_index].rotation_axis;

            if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (target_axis,positioner->axis[index].name)== 0)
        {

                        positioner->axis[axis_index].depends_on_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

        }

                }

                if (!found) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          target_axis, 2);

                    positioner->axis[axis_index].depends_on_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (rotation_axis,positioner->axis[index].name)== 0)
                    {

                        positioner->axis[axis_index].rotation_axis_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {


                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          rotation_axis, 2);

                    positioner->axis[axis_index].rotation_axis_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            axis_index++;

        } while (axis_index < positioner->axes);


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
        
        const char *array_section_id;

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
        cbf_failnez (cbf_get_array_section_id  (handle, element_number, &array_section_id))
        cbf_failnez (cbf_require_category (handle, "array_structure_list"))
        if (cbf_find_column(handle,"array_section_id")) {
            cbf_failnez (cbf_require_column   (handle, "array_id"))
        }

        surface_axis [0] = surface_axis [1] = NULL;


        while (cbf_find_nextrow (handle, array_section_id) == 0)
        {
            cbf_failnez (cbf_find_column      (handle, "precedence"))
            cbf_failnez (cbf_get_integervalue (handle, &precedence))

            if (precedence < 1 || precedence > 2)

                return CBF_FORMAT;

            if (surface_axis [precedence - 1])

                return CBF_FORMAT;

            cbf_failnez (cbf_find_column (handle, "axis_set_id"))
            cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
            if (cbf_find_column(handle,"array_section_id")) {
                cbf_failnez (cbf_require_column   (handle, "array_id"))
            }
        }

        if (!surface_axis[0]) {
            cbf_failnez (cbf_require_column  (handle, "array_id"))
            cbf_failnez (cbf_new_row         (handle))
            cbf_failnez (cbf_set_value       (handle, array_id))
            cbf_failnez (cbf_require_column  (handle, "array_section_id"))
            cbf_failnez (cbf_set_value       (handle, array_section_id))
            cbf_failnez (cbf_require_column  (handle, "precedence"))
            cbf_failnez (cbf_set_integervalue(handle,1))
            cbf_failnez (cbf_require_column  (handle, "axis_set_id"))
            cbf_failnez (cbf_require_value   (handle, &surface_axis [0], "ELEMENT_X"))
        }
        if (!surface_axis[1]){
            cbf_failnez (cbf_require_column  (handle,"array_id"))
            cbf_failnez (cbf_new_row         (handle))
            cbf_failnez (cbf_set_value       (handle, array_id))
            cbf_failnez (cbf_require_column  (handle, "array_section_id"))
            cbf_failnez (cbf_set_value       (handle, array_section_id))
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

    static int cbf_update_pixel (cbf_detector detector, double index1,
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


    /* Calculate the coordinates of a pixel */

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

    /* Get the names of the detector surface axes */

    int cbf_get_detector_surface_axes(cbf_detector detector,
                                 const char * * axis_id1,
                                 const char * * axis_id2)
    {
        unsigned int row;

        if ( !detector || !(detector->positioner)) return CBF_ARGUMENT;

        if (axis_id1) {

            row = detector->index[0];

            if (row < detector->positioner->axes) {

                *axis_id1 = (detector->positioner->axis[row]).name;

            } else {

                *axis_id1 = ".";
            }

        }


        if (axis_id2) {

            row = detector->index[1];

            if (row < detector->positioner->axes) {

                *axis_id2 = (detector->positioner->axis[row]).name;

            } else {

                *axis_id2 = ".";
            }

        }

        return CBF_SUCCESS;

    }


    /* Calcluate the slow axis of a detector */

    int cbf_get_detector_axis_slow (cbf_detector detector,
                                    double *slowaxis1,
                                    double *slowaxis2,
                                    double *slowaxis3)
    {
        double pixel00[3], pixel10[3], length;

        cbf_failnez (cbf_get_pixel_coordinates (detector, - 0.5,
                                                - 0.5,
                                                &pixel00 [0],
                                                &pixel00 [1],
                                                &pixel00 [2]))

        cbf_failnez (cbf_get_pixel_coordinates (detector, 0.5,
                                                - 0.5,
                                                &pixel10 [0],
                                                &pixel10 [1],
                                                &pixel10 [2]))

        pixel10 [0] -= pixel00 [0];
        pixel10 [1] -= pixel00 [1];
        pixel10 [2] -= pixel00 [2];

        length =  pixel10 [0]* pixel10 [0] +
        pixel10 [1]* pixel10 [1] +
        pixel10 [2]* pixel10 [2];

        if (length <= 0.0)

            return CBF_UNDEFINED;

        length = sqrt (length);

        if (slowaxis1)

            *slowaxis1 = pixel10 [0] / length;

        if (slowaxis2)

            *slowaxis2 = pixel10 [1] / length;

        if (slowaxis3)

            *slowaxis3 = pixel10 [2] / length;


        return 0;
    }

    /* Calcluate the fast axis of a detector */

    int cbf_get_detector_axis_fast (cbf_detector detector,
                                    double *fastaxis1,
                                    double *fastaxis2,
                                    double *fastaxis3)
    {
        double pixel00[3], pixel01[3], length;

        cbf_failnez (cbf_get_pixel_coordinates (detector, -0.5,
                                                -0.5,
                                                &pixel00 [0],
                                                &pixel00 [1],
                                                &pixel00 [2]))

        cbf_failnez (cbf_get_pixel_coordinates (detector, -0.5,
                                                0.5,
                                                &pixel01 [0],
                                                &pixel01 [1],
                                                &pixel01 [2]))

        pixel01 [0] -= pixel00 [0];
        pixel01 [1] -= pixel00 [1];
        pixel01 [2] -= pixel00 [2];

        length =  pixel01 [0]* pixel01 [0] +
        pixel01 [1]* pixel01 [1] +
        pixel01 [2]* pixel01 [2];

        if (length <= 0.0)

            return CBF_UNDEFINED;

        length = sqrt (length);

        if (fastaxis1)

            *fastaxis1 = pixel01 [0] / length;

        if (fastaxis2)

            *fastaxis2 = pixel01 [1] / length;

        if (fastaxis3)

            *fastaxis3 = pixel01 [2] / length;


        return 0;
    }

    /* Calcluate the axes of a detector */

    int cbf_get_detector_axes (cbf_detector detector,
                               double *slowaxis1,
                               double *slowaxis2,
                               double *slowaxis3,
                               double *fastaxis1,
                               double *fastaxis2,
                               double *fastaxis3)
    {

        cbf_failnez (cbf_get_detector_axis_slow (detector, slowaxis1, slowaxis2, slowaxis3) )

        cbf_failnez (cbf_get_detector_axis_fast (detector, fastaxis1, fastaxis2, fastaxis3) )

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

        if (!detector || axis_number < 1 || (ssize_t)(detector-> axes) < axis_number )

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


        if (!(cbf_get_diffrn_id    (handle, &diffrn_id))) diffrn_id = NULL;

        cbf_failnez(cbf_require_category (handle, "cell"))
        cbf_failnez(cbf_rewind_row       (handle))

        cbf_failnez(cbf_require_column   (handle, "entry_id"))

        entry_id = NULL;
        if (diffrn_id && (cbf_get_value(handle, &entry_id) ||
                          !entry_id || *entry_id == '\0')) {
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

        if (!(cbf_get_diffrn_id    (handle, &diffrn_id))) diffrn_id = NULL;

        cbf_failnez(cbf_require_category (handle, "cell"))
        cbf_failnez(cbf_rewind_row       (handle))

        cbf_failnez(cbf_require_column   (handle, "entry_id"))

        entry_id = NULL;
        if (diffrn_id && (cbf_get_value(handle, &entry_id) ||
                          !entry_id || *entry_id == '\0')) {
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


    /* get the axis upon which an axis depends */

    int cbf_get_parent_axis(cbf_handle handle,
                            const char * *parent_axis,
                            const char *axis_id) {

        cbf_failnez (cbf_find_category    (handle, "axis"))
        cbf_failnez (cbf_find_column      (handle, "id"))
        cbf_failnez (cbf_find_row         (handle, axis_id))
        cbf_failnez (cbf_find_column      (handle, "depends_on"))
        cbf_failnez (cbf_get_value        (handle, parent_axis))

        return 0;

    }


    /* get the reference axis vector and offset of a given axis */

    int cbf_get_axis_reference_poise(cbf_handle handle,
                                     double *vector1, double *vector2, double *vector3,
                                     double *offset1, double *offset2, double *offset3,
                                     const char *axis_id)
    {
        cbf_failnez (cbf_get_axis_vector  (handle, axis_id,
                                           vector1,
                                           vector2,
                                           vector3))
        cbf_failnez (cbf_get_axis_offset  (handle, axis_id,
                                           offset1,
                                           offset2,
                                           offset3))

        return 0;
    }

    /* get the absolute axis vector and offset of a given axis
     ratio is how far into a frame we are, 0. at the start
     of the frame, 1. at the end

     The three offset values are the absolute position to which
     the origin has been moved */

    int cbf_get_axis_poise(cbf_handle handle, double ratio,
                           double *vector1, double *vector2, double *vector3,
                           double *offset1, double *offset2, double *offset3,
                           double *angle,
                           const char *axis_id,
                           const char *frame_id)
    {
        cbf_positioner positioner;

        const char *parent_axis;

        double refvector1, refvector2, refvector3;

        double refoffset1, refoffset2, refoffset3;

        double start, increment;

        cbf_get_axis_reference_poise(handle,
                                     &refvector1, &refvector2, &refvector3,
                                     &refoffset1, &refoffset2, &refoffset3,
                                     axis_id);

        if (angle) {

            cbf_failnez(cbf_get_frame_axis_setting(handle,0,
                                    axis_id, frame_id, &start, &increment));

            *angle = ratio*increment;

        }

        cbf_failnez(cbf_get_parent_axis(handle,&parent_axis,axis_id));

        /* fprintf(stderr," axis_id %s parent %s\n",axis_id,parent_axis); */

        /* If this axis is not dependent, just return the reference poise */

        if (!parent_axis || cbf_cistrcmp(".",parent_axis) == 0) {


            if (vector1) *vector1 = refvector1;
            if (vector2) *vector2 = refvector2;
            if (vector3) *vector3 = refvector3;

            if (offset1) *offset1 = refoffset1;
            if (offset2) *offset2 = refoffset2;
            if (offset3) *offset3 = refoffset3;

            return 0;

        }

        cbf_failnez(cbf_construct_frame_positioner(handle, &positioner, parent_axis, frame_id))


        cbf_failnez(cbf_calculate_position(positioner,0,ratio,
                                      refoffset1, refoffset2, refoffset3,
                                      offset1,offset2,offset3))

        cbf_failnez(cbf_rotate_vector(positioner,0,ratio,
                                      refvector1, refvector2, refvector3,
                                      vector1,vector2,vector3))

                return cbf_free_positioner(positioner);


    }

    /* Get the positioner matrix

     The position matrix return is a 3x4 matrix of doubles.  The upper left 3x3
     is the rotation, the upper right 3x1 is the (post-) translation*/


    int cbf_get_positioner_matrix (cbf_positioner positioner, double ratio, double matrix[3][4]) {

        size_t i, j;

        double setting;

        if (!positioner)

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

        if (!positioner->matrix_is_valid || positioner->matrix_ratio_used != ratio )
        {


            positioner->matrix_ratio_used = ratio;

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


            for (i = 0; i < positioner->axes; i++)
            {
                setting = positioner->axis [i].setting;

                if (positioner->axis [i].type == CBF_TRANSLATION_AXIS)
                {
                    positioner->matrix [0][3] += setting * positioner->axis [i].vector [0];
                    positioner->matrix [1][3] += setting * positioner->axis [i].vector [1];
                    positioner->matrix [2][3] += setting * positioner->axis [i].vector [2];
                    /* fprintf(stderr," calculate position, axis %d, translate [%g, %g, %g]\n",
                     i,
                     setting * positioner->axis [i].vector [0],
                     setting * positioner->axis [i].vector [1],
                     setting * positioner->axis [i].vector [2]);
                     */

                }
                else
                {
                    double s, x, y, z, w,
                    xx, yy, zz, xy, xz, xw, yz, yw, zw;

                    double rotation [3][3], product [3][4];

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

                    /* fprintf(stderr," calculate position, axis %d, rotate [%g + i*%g + j*%g + k*%g]\n",
                     i, w, x, y, z);
                     */

                    for (r1 = 0; r1 < 3; r1++)

                        for (c2 = 0; c2 < 4; c2++)
                        {
                            product [r1][c2] = 0;

                            for (c1r2 = 0; c1r2 < 3; c1r2++)

                                product [r1][c2] += rotation [r1][c1r2] *
                                positioner->matrix [c1r2][c2];
                        }

                    for (r1 = 0; r1 < 3; r1++)

                        for (c2 = 0; c2 < 4; c2++)

                            positioner->matrix [r1][c2] = product [r1][c2];
                }

                positioner->matrix [0][3] += positioner->axis [i].offset [0];
                positioner->matrix [1][3] += positioner->axis [i].offset [1];
                positioner->matrix [2][3] += positioner->axis [i].offset [2];
            }

            positioner->matrix_is_valid = 1;
        }

        if (matrix) {

            for (i=0;i<3;i++) {
                for (j=0;j<4;j++) {
                    matrix[i][j]=positioner->matrix[i][j];
                }
            }

        }

        return 0;

    }

    /* Get goniometer poise -- returns the pre-offset, post-translation and angle */

    int cbf_get_goniometer_poise(cbf_goniometer goniometer, double ratio,
                                 double *vector1, double *vector2, double* vector3,
                                 double *offset1, double *offset2, double* offset3,
                                 double *angle) {
        double matrix0[3][4];

        double matrix1[3][4];

        double matrix[3][4];

        double axis[3], alength;

        double theta, costheta, sintheta;

        int i,j;

        cbf_failnez(cbf_get_positioner_matrix(goniometer, 0., matrix0))

        cbf_failnez(cbf_get_positioner_matrix(goniometer, 1., matrix1))

        /* compute matrix1*inverse(matrix2) forcing the offset to the
           ratio weighted average of the two offsets */

        for (i=0; i < 3; i++) {

            for (j=0; j < 3; j++) {

                matrix[i][j] = matrix0[i][0]*matrix1[j][0]
                + matrix0[i][1]*matrix1[j][1]
                + matrix0[i][2]*matrix1[j][2];

            }

            matrix[i][3] = matrix0[i][3]*(1.-ratio)+matrix1[i][3]*ratio;
        }

        costheta = .5*(matrix[0][0]+matrix[1][1]+matrix[2][2]-1.);

        if (costheta < -1.0000000000001 || costheta > 1.0000000000001) return CBF_ARGUMENT;

        sintheta = sqrt(fabs(1.-costheta*costheta));

        theta = atan2(sintheta,costheta);

        axis[0] = -(matrix[2][1]-matrix[1][2]);

        axis[1] = -(matrix[0][2]-matrix[2][0]);

        axis[2] = -(matrix[1][0]-matrix[0][1]);

        alength = sqrt(axis[0]*axis[0]+axis[1]*axis[1]+axis[2]*axis[2]);

        if (alength > 1.e-10) {

            axis[0] = axis[0]/alength;

            axis[1] = axis[1]/alength;

            axis[2] = axis[2]/alength;

        } else {

            axis[0] = 1.;

            axis[1] = 0.;

            axis[2] = 0.;
        }

        if (angle) *angle = ratio*theta*45./atan2(1.,1.);

        if (vector1) *vector1 = axis[0];

        if (vector2) *vector2 = axis[1];

        if (vector3) *vector3 = axis[2];

        if (offset1) {

            *offset1 = matrix[0][0]*matrix[0][3]
            + matrix[1][0]*matrix[1][3]
            + matrix[2][0]*matrix[2][3];

        }

        if (offset2) {

            *offset2 = matrix[0][1]*matrix[0][3]
            + matrix[1][1]*matrix[1][3]
            + matrix[2][1]*matrix[2][3];

        }

        if (offset3) {

            *offset3 = matrix[0][2]*matrix[0][3]
            + matrix[1][2]*matrix[1][3]
            + matrix[2][2]*matrix[2][3];

        }

        return 0;

    }

    /* Get the setting of an axis specific to a given frame */

    int cbf_get_frame_axis_setting (cbf_handle handle, unsigned int  reserved,
                                    const char   *axis_id,
                                    const char   *frame_id,
                                    double       *start,
                                    double       *increment)
    {
        cbf_axis_type type;

        if (reserved != 0 || !start || !increment )

            return CBF_ARGUMENT;


        /* Get the axis type */

        cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

        if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS && type != CBF_GENERAL_AXIS )

            return CBF_FORMAT;

        /* For a general axis, the settings are always 0 */

        if (type == CBF_GENERAL_AXIS ) {

            *start = 0.;

            *increment = 0.;

        }


        /* Read from the diffrn_scan_axis and
         diffrn_scan_frame_axis categories */

        else if (type == CBF_TRANSLATION_AXIS)
        {
            const char *test_axis_id;

            int foundincr, founddisp;
            foundincr = 0;
            founddisp = 0;

            if (frame_id && cbf_cistrcmp(frame_id,".")!=0 ) {
                if (!cbf_find_category   (handle, "diffrn_scan_frame_axis") &&
                    !cbf_find_column     (handle, "frame_id")){
                    cbf_failnez (cbf_rewind_row      (handle))
                    while (!cbf_find_nextrow(handle,frame_id)) {
                        if(!cbf_find_column(handle,"axis_id") &&
                           !cbf_get_value(handle,&test_axis_id) &&
                           test_axis_id && *test_axis_id &&
                           !cbf_cistrcmp(test_axis_id,axis_id)) {
                            if (!cbf_find_column     (handle, "displacement") &&
                                !cbf_get_doublevalue (handle, start)) founddisp = 1;
                            if (!cbf_find_column     (handle, "displacement_increment") &&
                                !cbf_get_doublevalue (handle, increment)) foundincr = 1;
                            break;
                        }
                        cbf_failnez (cbf_find_column     (handle, "frame_id"))
                    }
                }
            }
            if ((!foundincr||!founddisp)&&frame_id && cbf_cistrcmp(frame_id,".")==0 ) {
                if (!cbf_find_category   (handle, "diffrn_scan_frame_axis") &&
                    !cbf_find_column     (handle, "axis_id")){
                    cbf_failnez (cbf_rewind_row      (handle))
                    if (!cbf_find_nextrow(handle,axis_id)) {
                        if (!founddisp&&!cbf_find_column(handle, "displacement") &&
                            !cbf_get_doublevalue (handle, start)) founddisp = 1;
                        if (!foundincr&&!cbf_find_column(handle, "displacement_increment") &&
                            !cbf_get_doublevalue (handle, increment)) foundincr = 1;
                    }
                }
            }
            if (!foundincr || !founddisp) {
                cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
                cbf_failnez (cbf_find_column     (handle, "axis_id"))
                cbf_failnez (cbf_find_row        (handle, axis_id))
                if (!founddisp) {
                    cbf_failnez (cbf_find_column     (handle, "displacement"))
                    cbf_failnez (cbf_get_doublevalue (handle, start))
                }
                if (!foundincr) {
                    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
                    cbf_failnez (cbf_get_doublevalue (handle, increment))
                }
            }
            if (!foundincr) return CBF_NOTFOUND;
            if (!founddisp) return CBF_NOTFOUND;
        }
        else
        {
            const char *test_axis_id;

            int foundincr, foundangle;
            foundincr = 0;
            foundangle = 0;

            if (frame_id && cbf_cistrcmp(frame_id,".")!=0 ) {
                if (!cbf_find_category   (handle, "diffrn_scan_frame_axis") &&
                    !cbf_find_column     (handle, "frame_id")){
                    cbf_failnez (cbf_rewind_row      (handle))
                    while (!cbf_find_nextrow(handle,frame_id)) {
                        if(!cbf_find_column(handle,"axis_id") &&
                           !cbf_get_value(handle,&test_axis_id) &&
                           test_axis_id && *test_axis_id &&
                           !cbf_cistrcmp(test_axis_id,axis_id)) {
                            if (!cbf_find_column     (handle, "angle") &&
                                !cbf_get_doublevalue (handle, start)) foundangle = 1;
                            if (!cbf_find_column     (handle, "angle_increment") &&
                                !cbf_get_doublevalue (handle, increment)) foundincr = 1;
                            break;
                        }
                        cbf_failnez (cbf_find_column     (handle, "frame_id"))
                    }
                }
            }
            if ((!foundincr||!foundangle)&&frame_id && cbf_cistrcmp(frame_id,".")==0 ) {
                if (!cbf_find_category   (handle, "diffrn_scan_frame_axis") &&
                    !cbf_find_column     (handle, "axis_id")){
                    cbf_failnez (cbf_rewind_row      (handle))
                    if (!cbf_find_nextrow(handle,axis_id)) {
                        if (!foundangle&&!cbf_find_column(handle, "angle") &&
                            !cbf_get_doublevalue (handle, start)) foundangle = 1;
                        if (!foundincr&&!cbf_find_column(handle, "angle_increment") &&
                            !cbf_get_doublevalue (handle, increment)) foundincr = 1;
                    }
                }
            }
            if (!foundincr || !foundangle) {
                cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
                cbf_failnez (cbf_find_column     (handle, "axis_id"))
                cbf_failnez (cbf_find_row        (handle, axis_id))
                if (!foundangle) {
                    cbf_failnez (cbf_find_column     (handle, "angle"))
                    cbf_failnez (cbf_get_doublevalue (handle, start))
                    foundangle=1;
                }
                if (!foundincr) {
                    cbf_failnez (cbf_find_column     (handle, "angle_increment"))
                    cbf_failnez (cbf_get_doublevalue (handle, increment))
                    foundincr=1;
                }
            }
            if (!foundincr) return CBF_NOTFOUND;
            if (!foundangle) return CBF_NOTFOUND;
        }


        return 0;
    }

    /* Add frame-specific data for an axis to a positioner */

    int cbf_read_positioner_frame_axis (cbf_handle      handle,
                                        unsigned int    reserved,
                                        cbf_positioner  positioner,
                                        const char     *axis_id,
                                        const char     *frame_id,
                                        int             read_setting)
    {
        const char *next_id;

        const char *rot_id;

        cbf_axis_type axis_type;
        
        const char *deptype;

        double vector1, vector2, vector3, offset1, offset2, offset3;

        double start, increment, rot;

        int errorcode;

        cbf_failnez (cbf_find_category    (handle, "axis"));
        cbf_failnez (cbf_find_column      (handle, "id"));
        cbf_failnez (cbf_find_row         (handle, axis_id));
        cbf_failnez (cbf_find_column      (handle, "depends_on"));
        cbf_failnez (cbf_get_value        (handle, &next_id));
        cbf_failnez (cbf_get_typeofvalue  (handle, &deptype));
        if (cbf_cistrcmp(deptype, "null") == 0) {
            next_id = NULL;
        }

        if (!cbf_find_column (handle, "rotation_axis")) {

            cbf_failnez (cbf_get_value    (handle, &rot_id));

        } else {

            rot_id = NULL;

        }

        if (!cbf_find_column (handle, "rotation")) {

            cbf_failnez (cbf_get_doublevalue (handle, &rot));

        } else {

            rot = 0.0;

        }

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

        start = increment = 0.;

        errorcode = 0;

        /* fprintf(stderr," cbf_read_positioner_axis , axis = %s, read_setting = %d\n",axis_id, read_setting);*/

        if (read_setting && axis_type != CBF_GENERAL_AXIS)
        {

            errorcode = cbf_get_frame_axis_setting (handle, reserved, axis_id,
                                                    frame_id,
                                                    &start,
                                                    &increment);

            if (read_setting < 0) {

                increment = 0.;

                errorcode = cbf_get_axis_reference_setting (handle, reserved, axis_id,
                                                            &start);
            }

            if ( (read_setting == -2 || read_setting == 2)
                && (errorcode == CBF_NOTFOUND || errorcode == CBF_FORMAT) ) {

                start = increment = 0;

                errorcode = 0;

            }

            cbf_failnez(errorcode);

        }

        cbf_failnez (cbf_add_positioner_axis_wrot (positioner,
                                              axis_id,
                                              next_id,
                                              rot_id,
                                              axis_type,
                                              vector1, vector2, vector3,
                                              offset1, offset2, offset3,
                                              start, increment, rot))

        return 0;
    }

    /* Construct a frame goniometer positioner*/

    int cbf_construct_frame_goniometer (cbf_handle handle,
                                        cbf_goniometer *goniometer,
                                        const char *frame_id)
    {
        const char *diffrn_id, *id, *this_id, *axis_id;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

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

                        errorcode = cbf_read_positioner_frame_axis (handle,
                                                                    0, /* reserved */
                                                                    *goniometer,
                                                                    axis_id,
                                                                    frame_id, 1);
                }
        }

        /* Complete the connectivity of the positioner */

        axis_index = 0;

        do {

            size_t index;

            int found;

            target_axis = (*goniometer)->axis[axis_index].depends_on;

            rotation_axis = (*goniometer)->axis[axis_index].rotation_axis;

            if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < (*goniometer)->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (target_axis,(*goniometer)->axis[index].name)== 0)
                    {

                        (*goniometer)->axis[axis_index].depends_on_index = index;

                        found = 1;

                        (*goniometer)->axis[index].depdepth
                        = cbf_max( (*goniometer)->axis[index].depdepth,
                                  (*goniometer)->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *goniometer,
                                                          target_axis, 2);

                    (*goniometer)->axis[axis_index].depends_on_index
                    = (*goniometer)->axes-1;

                    (*goniometer)->axis[(*goniometer)->axes-1].depdepth
                    = cbf_max( (*goniometer)->axis[(*goniometer)->axes-1].depdepth,
                              (*goniometer)->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < (*goniometer)->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (rotation_axis,(*goniometer)->axis[index].name)== 0)
                    {

                        (*goniometer)->axis[axis_index].rotation_axis_index = index;

                        found = 1;

                        (*goniometer)->axis[index].depdepth
                        = cbf_max( (*goniometer)->axis[index].depdepth,
                                  (*goniometer)->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {


                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          *goniometer,
                                                          rotation_axis, 2);

                    (*goniometer)->axis[axis_index].rotation_axis_index
                    = (*goniometer)->axes-1;

                    (*goniometer)->axis[(*goniometer)->axes-1].depdepth
                    = cbf_max( (*goniometer)->axis[(*goniometer)->axes-1].depdepth,
                              (*goniometer)->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            axis_index++;

        } while (axis_index < (*goniometer)->axes);


        if (errorcode)
        {
            errorcode |= cbf_free_positioner (*goniometer);

            *goniometer = NULL;
        }

        return errorcode;
    }


    /* Construct a frame detector positioner */

    int cbf_construct_frame_detector (cbf_handle    handle,
                                      cbf_detector *detector,
                                      unsigned int  element_number,
                                      const char *frame_id)
    {
        int errorcode, precedence;

        unsigned int row, axis;

        const char *diffrn_id, *id, *this_id, *axis_id, *array_id;
        
        const char *array_section_id;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

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
        cbf_failnez (cbf_get_array_section_id  (handle, element_number, &array_section_id))
        cbf_failnez (cbf_find_category (handle, "array_structure_list"))
        if (cbf_find_column(handle,"array_section_id")) {
            cbf_failnez (cbf_find_column   (handle, "array_id"));
        }

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
            if (cbf_find_column(handle,"array_section_id")) {
                cbf_failnez (cbf_find_column   (handle, "array_id"));
            }
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

                        errorcode = cbf_read_positioner_frame_axis (handle, 0,
                                                                    positioner,
                                                                    axis_id,
                                                                    frame_id,
                                                                    1);
                }
        }


        /* Add the surface axes */

        if (!errorcode)

            errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [0], 0);

        if (!errorcode && surface_axis [1])

            errorcode = cbf_read_positioner_axis (handle, 0, positioner,
                                                  surface_axis [1], 0);

        if (errorcode)
        {
            errorcode |= cbf_free_positioner (positioner);

            return errorcode | cbf_free ((void **) detector, NULL);
        }

        /* Complete the connectivity of the positioner */

        axis_index = 0;

        do {

            size_t index;

            int found;

            target_axis = positioner->axis[axis_index].depends_on;

            rotation_axis = positioner->axis[axis_index].rotation_axis;

            if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (target_axis,positioner->axis[index].name)== 0)
        {

                        positioner->axis[axis_index].depends_on_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

        }

                }

                if (!found) {

                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          target_axis, 2);

                    positioner->axis[axis_index].depends_on_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                found = 0;

                for (index = 0; index < positioner->axes; index++) {

                    if (index != axis_index &&
                        cbf_cistrcmp (rotation_axis,positioner->axis[index].name)== 0)
                    {

                        positioner->axis[axis_index].rotation_axis_index = index;

                        found = 1;

                        positioner->axis[index].depdepth
                        = cbf_max( positioner->axis[index].depdepth,
                                  positioner->axis[axis_index].depdepth+1);


                        break;

                    }

                }

                if (!found) {


                    errorcode = cbf_read_positioner_axis (handle,
                                                          0, /* reserved */
                                                          positioner,
                                                          rotation_axis, 2);

                    positioner->axis[axis_index].rotation_axis_index
                    = positioner->axes-1;

                    positioner->axis[positioner->axes-1].depdepth
                    = cbf_max( positioner->axis[positioner->axes-1].depdepth,
                              positioner->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

                }

            }

            axis_index++;

        } while (axis_index < positioner->axes);

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

    /* construct a positioner for a given final axis */

    int cbf_construct_frame_positioner (cbf_handle handle,
                                        cbf_positioner *positioner,
                                        const char *axis_id,
                                        const char *frame_id)
    {

        int errorcode;

        const char * target_axis;

        const char * rotation_axis;

        size_t axis_index;

        unsigned int axis_index_limit;

        if (!positioner || !axis_id)

            return CBF_ARGUMENT;

        /* fprintf(stderr," cbf_construct_positioner, axis %s\n",axis_id);*/

        errorcode = 0;


        /* Construct the positioner */

        cbf_failnez (cbf_make_positioner (positioner));

        /* Set a limit on the number of axes in the positioner

         As a crude limit to break dependency loops, N*(N-1)/2

         will suffice.

         */

        cbf_failnez(cbf_find_category(handle,"axis"));

        cbf_failnez(cbf_count_rows(handle,&axis_index_limit));

        axis_index_limit *= (axis_index_limit-1);

        axis_index_limit /= 2;

        (*positioner)->axis_index_limit = axis_index_limit;

        /* read the first axis */

            errorcode = cbf_read_positioner_frame_axis (handle,
                                                        0, /* reserved */
                                                        *positioner,
                                                    axis_id,
                                                        frame_id, 2);

        axis_index = 0;

            if (!errorcode) {

            do{

                target_axis = (*positioner)->axis[axis_index].depends_on;

                rotation_axis = (*positioner)->axis[axis_index].rotation_axis;

                if (target_axis && cbf_cistrcmp (target_axis,".") !=0 ) {

                    errorcode = cbf_read_positioner_frame_axis (handle,
                                                          0, /* reserved */
                                                          *positioner,
                                                          target_axis,
                                                          frame_id, 2);

                    (*positioner)->axis[axis_index].depends_on_index
                    = (*positioner)->axes-1;

                    (*positioner)->axis[(*positioner)->axes-1].depdepth
                    = cbf_max( (*positioner)->axis[(*positioner)->axes-1].depdepth,
                              (*positioner)->axis[axis_index].depdepth+1);

                    if (!errorcode) break;

            }

                if (rotation_axis && cbf_cistrcmp (rotation_axis,".") !=0 ) {

                    errorcode = cbf_read_positioner_frame_axis (handle,
                                                          0, /* reserved */
                                                          *positioner,
                                                          rotation_axis,
                                                          frame_id, 2);

                    (*positioner)->axis[axis_index].rotation_axis_index
                    = (*positioner)->axes-1;

                    (*positioner)->axis[(*positioner)->axes-1].depdepth
                    = cbf_max( (*positioner)->axis[(*positioner)->axes-1].depdepth,
                              (*positioner)->axis[axis_index].depdepth+1);


                    if (!errorcode) break;

        }

                axis_index++;

            } while (axis_index < (*positioner)->axes);

        }

        if (errorcode)
        {
            errorcode |= cbf_free_positioner (*positioner);

            *positioner = NULL;
        }

        return errorcode;
    }
    
    /*  For a given axis, return the first element_id
        associated with it for the given equipment
        and equipment_id */

    int cbf_get_axis_element_id(cbf_handle handle,
                                const char ** element_id,
                                const char * equipment_id,
                                const char * equipment,
                                const char * axis_id) {
        
        int errorcode, errorcode2;
        
        /* check the arguments */
        
        if (!handle || !element_id || !equipment_id || !equipment || !axis_id)
            
            return CBF_ARGUMENT;
        
        *element_id = NULL;
        
        errorcode = errorcode2 = 0;
        
        if (cbf_cistrcmp(equipment,"detector")==0 ) {
            
            const char * detector_id;
            
            const char * detector_element_id;
            
            const char * axis_set_id;
            
            const char * array_id;
            
            axis_set_id = array_id  = detector_element_id = NULL;
            
            errorcode2 |= cbf_find_category(handle, "array_structure_list_axis");
            
            errorcode2 |= cbf_find_column(handle,"axis_id");
            
            errorcode2 |= cbf_rewind_row(handle);
            
            errorcode2 |= cbf_find_row(handle, axis_id);
            
            errorcode2 |= cbf_find_column(handle,"axis_set_id");
            
            errorcode2 |= cbf_get_value(handle,&axis_set_id);
            
            if (errorcode2) axis_set_id = axis_id;
            
            errorcode |= errorcode2;
            
            errorcode2 = 0;
            
            errorcode2 |= cbf_find_category(handle, "array_structure_list");
            
            errorcode2 |= cbf_find_column(handle,"axis_set_id");
            
            errorcode2 |= cbf_rewind_row(handle);
            
            errorcode2 |= cbf_find_row(handle, axis_set_id);
            
            errorcode2 |= cbf_find_column(handle, "array_id");
            
            errorcode2 |= cbf_get_value(handle,&array_id);
            
            if (!errorcode2 && array_id) {
                
                errorcode2 |= cbf_find_category(handle, "diffrn_data_frame");
                
                errorcode2 |= cbf_find_column(handle,"array_id");
                
                errorcode2 |= cbf_rewind_row(handle);
                
                errorcode2 |= cbf_find_row(handle, array_id);
                
                errorcode2 |= cbf_find_column(handle, "detector_element_id");
                
                errorcode2 |= cbf_get_value(handle,&detector_element_id);
                
                if (!errorcode2 && detector_element_id) {
                    
                    errorcode2 |= cbf_find_category(handle, "diffrn_detector_element");
                    
                    errorcode2 |= cbf_find_column(handle,"id");
                    
                    errorcode2 |= cbf_rewind_row(handle);
                    
                    errorcode2 |= cbf_find_row(handle, detector_element_id);
                    
                    errorcode2 |= cbf_find_column(handle, "detector_id");
                    
                    errorcode2 |= cbf_get_value(handle,&detector_id);
                    
                    if (errorcode2) detector_id = NULL;
                    
                    if (!detector_id || cbf_cistrcmp(detector_id,equipment_id)) return CBF_NOTFOUND;
                    
                }
                
            }
            
            
            *element_id = detector_element_id;
            
            return errorcode|errorcode2;
            
        }
        
        return CBF_NOTFOUND;
        
        
    }
    
    /* get the id of the particular equipment associated with
       the specified axis_id for the specified equipment type */
    

    int cbf_get_axis_equipment_id(cbf_handle handle,
                                    const char ** equipment_id,
                                    const char * equipment,
                                    const char * axis_id)
    {
        int errorcode, errorcode2;

        /* check the arguments */

        if (!handle || !equipment_id || !equipment || !axis_id)

            return CBF_ARGUMENT;

        *equipment_id = NULL;

        errorcode = errorcode2 = 0;

        if (cbf_cistrcmp(equipment,"detector")==0 ) {

            errorcode |= cbf_find_category(handle, "diffrn_detector_axis");

            errorcode |= cbf_find_column(handle,"axis_id");

            errorcode |= cbf_rewind_row(handle);

            errorcode |= cbf_find_row(handle, axis_id);

            errorcode |= cbf_find_column(handle,"detector_id");

            errorcode |= cbf_get_value(handle,equipment_id);

            if (errorcode) *equipment_id = NULL;
            
            if (!*equipment_id || !cbf_cistrcmp(*equipment_id,".")) {
                
                const char * axis_set_id;
                
                const char * array_id;
                
                const char * detector_element_id;
                
                axis_set_id = array_id = detector_element_id = NULL;
                
                errorcode2 |= cbf_find_category(handle, "array_structure_list_axis");
                
                errorcode2 |= cbf_find_column(handle,"axis_id");
                
                errorcode2 |= cbf_rewind_row(handle);
                
                errorcode2 |= cbf_find_row(handle, axis_id);
                
                errorcode2 |= cbf_find_column(handle,"axis_set_id");
                
                errorcode2 |= cbf_get_value(handle,&axis_set_id);
                
                if (errorcode2) axis_set_id = axis_id;
                
                errorcode |= errorcode2;
                
                errorcode2 = 0;
                
                errorcode2 |= cbf_find_category(handle, "array_structure_list");
                
                errorcode2 |= cbf_find_column(handle,"axis_set_id");
                
                errorcode2 |= cbf_rewind_row(handle);
                
                errorcode2 |= cbf_find_row(handle, axis_set_id);
                
                errorcode2 |= cbf_find_column(handle, "array_id");
                
                errorcode2 |= cbf_get_value(handle,&array_id);
                
                if (!errorcode2 && array_id) {
                    
                    errorcode2 |= cbf_find_category(handle, "diffrn_data_frame");
                    
                    errorcode2 |= cbf_find_column(handle,"array_id");
                    
                    errorcode2 |= cbf_rewind_row(handle);
                    
                    errorcode2 |= cbf_find_row(handle, array_id);
                    
                    errorcode2 |= cbf_find_column(handle, "detector_element_id");
                    
                    errorcode2 |= cbf_get_value(handle,&detector_element_id);
                    
                    if (!errorcode2 && detector_element_id) {
                        
                        errorcode2 |= cbf_find_category(handle, "diffrn_detector_element");
                        
                        errorcode2 |= cbf_find_column(handle,"id");
                        
                        errorcode2 |= cbf_rewind_row(handle);
                        
                        errorcode2 |= cbf_find_row(handle, detector_element_id);
                        
                        errorcode2 |= cbf_find_column(handle, "detector_id");
                        
                        errorcode2 |= cbf_get_value(handle,equipment_id);
                        
                        if (errorcode2) *equipment_id = NULL;
                      
                    }
                    
                }
 
            }

            return CBF_SUCCESS;

        } else if (cbf_cistrcmp(equipment,"goniometer")==0 ) {

            errorcode |= cbf_find_category(handle, "diffrn_measurement_axis");

            errorcode |= cbf_find_column(handle,"axis_id");

            errorcode |= cbf_rewind_row(handle);

            errorcode |= cbf_find_row(handle, axis_id);

            errorcode |= cbf_find_column(handle,"measurement_id");

            errorcode |= cbf_get_value(handle,equipment_id);

            if (errorcode) *equipment_id = NULL;

            return CBF_SUCCESS;

        } else {

            *equipment_id = NULL;

        }

        return CBF_SUCCESS;


    }

    /* get the dimension and units of the available scan points for an axis
     If the axis is an array axis or array section axis, the number
     of scan points in the number of pixels for that axis.  If axis is not
     a general axis, scanpoints will be at least 1 */
    
    int cbf_get_axis_parameters(cbf_handle handle,
                                size_t * scanpoints,
                                const char ** units,
                                const char * equipment,
                                const char * axis_id) {
        
        const char * axis_set_id = NULL;
        
        const char * detector_element_id = NULL;
        
        const char * equipmentid = NULL;
        
        const char * axistype = NULL;
        
        int dimension = 1;
        
        int isarrayaxis, isscanaxis;
        
        isarrayaxis = 0;
        
        isscanaxis = 0;
        
        /* check the arguments */
        
        if (!handle || !scanpoints || !units || !equipment || !axis_id)
            
            return CBF_ARGUMENT;
        
        *units = NULL;
        
        *scanpoints = 0;
        
        if (cbf_find_category(handle, "axis")
            || cbf_find_column(handle,"id")
            || cbf_rewind_row(handle)) return CBF_SUCCESS;
        
        
        while (!cbf_find_nextrow(handle,axis_id)) {
            
            if (cbf_find_column(handle,"equipment")) return CBF_SUCCESS;
            
            if (cbf_get_value(handle,&equipmentid)) return CBF_SUCCESS;
            
            if (cbf_find_column(handle,"id")) return CBF_SUCCESS;
            
            if (equipmentid
                && (!cbf_cistrcmp(equipmentid,"detector")
                    || !cbf_cistrcmp(equipmentid,"goniometer")
                    )) {
                    
                    if (!equipmentid || cbf_cistrcmp(equipmentid,equipment)) continue;
                    
                }
            
            if (cbf_find_column(handle,"type")) return CBF_SUCCESS;
            
            if (cbf_get_value(handle,&axistype)) return CBF_SUCCESS;
            
            if (!axistype 
                ||!cbf_cistrcmp(axistype,"general")) return CBF_SUCCESS;
            
            if (!cbf_cistrcmp(axistype,"rotation")) {
                
                *units = "degrees";
                
                *scanpoints = 1;
                
            } else if (!cbf_cistrcmp(axistype,"translation")) {
                
                *units = "mm";
                
                *scanpoints = 1;
                
            }
            
            break;
            
        }
        
        if (!units) return CBF_SUCCESS;
        
        if (!cbf_find_category(handle, "diffrn_scan_axis")
            &&!cbf_find_column(handle,"axis_id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_row(handle, axis_id)) {
            
            isscanaxis = 1;
            
        } else {
            
            if (!cbf_find_category(handle, "diffrn_scan_frame_axis")
                &&!cbf_find_column(handle,"axis_id")
                &&!cbf_rewind_row(handle)
                &&!cbf_find_row(handle, axis_id)) {
                
                isscanaxis = 1;
                
            }
            
            if (!cbf_cistrcmp(equipmentid,"detector")) {
                
                axis_set_id = detector_element_id = NULL;
                
                if (!cbf_find_category(handle, "array_structure_list_axis")
                    &&!cbf_find_column(handle,"axis_id")
                    &&!cbf_rewind_row(handle)
                    &&!cbf_find_row(handle, axis_id)
                    &&!cbf_find_column(handle,"axis_set_id")
                    &&!cbf_get_value(handle,&axis_set_id))
                {
                    
                    if (!cbf_find_category(handle, "array_structure_list")
                        &&!cbf_find_column(handle,"axis_set_id")
                        &&!cbf_rewind_row(handle)
                        &&!cbf_find_row(handle, axis_set_id)
                        &&!cbf_find_column(handle,"dimension")
                        &&!cbf_require_integervalue(handle,&dimension,0)){
                        
                        isarrayaxis = 1;
                        
                        if (!isscanaxis){
                            
                            *scanpoints = dimension;
                            
                            return CBF_SUCCESS;
                            
                        }
                        
                    }
                    
                }
                
            }
            
            if (!cbf_find_category(handle,"diffrn_scan")) {
                
                const char * framesstr;
                
                if (!cbf_find_column(handle,"frames")
                    
                    && !cbf_rewind_row(handle)
                    
                    && !cbf_get_value(handle,&framesstr)
                    
                    && framesstr) {
                    
                    long tscp;
                    
                    sscanf(framesstr,"%ld",&tscp);
                    
                    *scanpoints = tscp;
                    
                } else {
                    
                    *scanpoints = 0;
                }
                
                if (isscanaxis) {
                    
                    if (*scanpoints < 1) *scanpoints = 1;
                    
                    *scanpoints *= dimension;
                    
                }
                
                return CBF_SUCCESS;
                
            }
            
            return CBF_SUCCESS;
            
        }
        
        return CBF_SUCCESS;
        
    }


    /* get the scan points for an axis.  If the axis
       is an array axis, with no scan changes, an array
       of the pixel centers is provided.  If there are
       scan changes, a linearized 2-dimensional array
       will be returned, with the pixel positions as
       the fast index and the frame as the slow.  */

    int cbf_get_axis_scan_points(cbf_handle handle,
                                double * scanarray,
                                size_t scanpoints,
                                size_t *scanpointsfound,
                                const char * units,
                                const char * axis_id) {
        
        const char * axis_set_id;
        
        const char * detector_element_id;
        
        cbf_axis_type axis_type;
        
        int dimension;
        
        int isarrayaxis, isscanaxis;
        
        double displacement, displacement_increment, displacement_rstrt_incr;
        
        double displacement_start, displacement_range;
        
        double angle, angle_increment, angle_rstrt_incr;
        
        double angle_start, angle_range;
        
        const char * direction;
        
        isarrayaxis = 0;
        
        isscanaxis = 0;
        
        /* check the arguments */

        if (!handle || !scanarray || scanpoints < 1 || !units || !axis_id)

            return CBF_ARGUMENT;

        cbf_debug_print2("entering cbf_get_axis_scan_points axis_id = %s\n", axis_id);
        
        *scanpointsfound = 0;
        
        axis_set_id = detector_element_id = NULL;
        
        displacement = displacement_start
        = displacement_increment = displacement_rstrt_incr = 0.;
        
        angle = angle_start = angle_increment = angle_rstrt_incr = 0.;
        
        if (cbf_get_axis_type(handle,axis_id,&axis_type)) {
            
            axis_type=CBF_GENERAL_AXIS;
            
        }
        
        
        direction = "increasing";
        
        if (!cbf_find_category(handle, "array_structure_list_axis")
            &&!cbf_find_column(handle,"axis_id")
            &&!cbf_rewind_row(handle)
            &&!cbf_find_row(handle, axis_id)
            &&!cbf_find_column(handle,"axis_set_id")
            &&!cbf_get_value(handle,&axis_set_id))
        {
            
            if (!cbf_find_column(handle,"displacement_increment")) {
                
                cbf_require_doublevalue(handle,&displacement_increment,0.);
                
            }
            
            if (!cbf_find_column(handle,"displacement")) {
                
                cbf_require_doublevalue(handle,&displacement,0.);
                
            }

            if (!cbf_find_column(handle,"angle_increment")) {
                
                cbf_require_doublevalue(handle,&angle_increment,0.);
                
            }
            
            if (!cbf_find_column(handle,"angle")) {
                
                cbf_require_doublevalue(handle,&angle,0.);
                
            }

            if (!cbf_find_category(handle, "array_structure_list")
                &&!cbf_find_column(handle,"axis_set_id")
                &&!cbf_rewind_row(handle)
                &&!cbf_find_row(handle, axis_set_id)
                &&!cbf_find_column(handle,"dimension")
                &&!cbf_require_integervalue(handle,&dimension,0)){
                
                if (!cbf_find_column(handle,"direction")) {
                    
                    cbf_require_value(handle,&direction,"increasing");
                    
                }
                
                isarrayaxis = 1;
                
            }
            
        }

        if (!isarrayaxis
            && !cbf_find_category(handle, "diffrn_scan_axis")
            && !cbf_find_column(handle,"axis_id")
            && !cbf_rewind_row(handle)
            && !cbf_find_nextrow(handle,axis_id)) {

            if (!cbf_find_column(handle,"displacement_increment")) {
                
                cbf_require_doublevalue(handle,&displacement_increment,0.);
                
            }
            
            if (!cbf_find_column(handle,"displacement_start")) {
                
                cbf_require_doublevalue(handle,&displacement_start,0.);
                
            }
            
            if (!cbf_find_column(handle,"displacement_range")) {
                
                cbf_require_doublevalue(handle,&displacement_range,0.);
                
            }
            
            if (!cbf_find_column(handle,"displacement_rstrt_incr")) {
                
                cbf_require_doublevalue(handle,&displacement_rstrt_incr,0.);
                
            }
            
            
            if (!cbf_find_column(handle,"angle_increment")) {
                
                cbf_require_doublevalue(handle,&angle_increment,0.);
                
            }
            
            if (!cbf_find_column(handle,"angle_start")) {
                
                cbf_require_doublevalue(handle,&angle_start,0.);
                
            }
            
            if (!cbf_find_column(handle,"angle_range")) {
                
                cbf_require_doublevalue(handle,&angle_range,0.);
                
            }
            
            isscanaxis = 1;
            
            /* The diffraction_scan_axis data points can be used to populate the
             scan points array, but if there are diffrn_scan_frame_axis points
             they will be used in preference */
            
            if (axis_type == CBF_TRANSLATION_AXIS) {
                
                while (*scanpointsfound < scanpoints) {
                    
                    double next_displacement;
                    
                    next_displacement =
                    
                    displacement_start + (displacement_increment+displacement_rstrt_incr)*(*scanpointsfound);
                    
                    if (fabs(displacement_range)+1.e-10 >=
                        fabs(next_displacement) || *scanpointsfound == 0) {
                        
                        scanarray[*scanpointsfound] =  next_displacement;
                        
                        (*scanpointsfound)++;
                        
                        
                    } else {
                        
                        break;
                        
                    }
                    
                }
                
            } else if (axis_type == CBF_ROTATION_AXIS) {
                
                while (*scanpointsfound < scanpoints) {
                    
                    double next_angle;
                    
                    next_angle =
                    
                    angle_start + (angle_increment+angle_rstrt_incr)*(*scanpointsfound);
                    
                    if (fabs(angle_range)+1.e-10 >=
                        fabs(next_angle) || *scanpointsfound == 0 ) {
                        
                        scanarray[*scanpointsfound] =  next_angle;
                        
                        (*scanpointsfound)++;
                        
                    } else {
                        
                        break;
                        
                    }
                    
                }
                
            }
            
        }
        
        
        if (cbf_find_category(handle, "diffrn_scan_frame_axis")
            || cbf_find_column(handle,"axis_id")
            || cbf_rewind_row(handle)
            || cbf_find_nextrow(handle,axis_id)
            || cbf_rewind_row(handle))  {
            
            if (isarrayaxis) {
                
                if (!direction || !cbf_cistrcmp(direction,"increasing" )) {
                    
                    if (axis_type == CBF_TRANSLATION_AXIS) {
                        
                    while (*scanpointsfound < scanpoints
                           && *scanpointsfound < (size_t)dimension) {
                        
                        scanarray[*scanpointsfound] =
                        displacement + displacement_increment*(*scanpointsfound);
                        
                        (*scanpointsfound)++;
                        
                    }
                    
                    
                    } else if (axis_type == CBF_ROTATION_AXIS){
                        
                        
                    while (*scanpointsfound < scanpoints
                           && *scanpointsfound < (size_t)dimension) {
                        
                        scanarray[*scanpointsfound] =
                            angle + angle_increment*(*scanpointsfound);
                        
                        (*scanpointsfound)++;
                        
                        
                    }
                    
                }
            
                } else {
            
                    if (axis_type == CBF_TRANSLATION_AXIS) {

            
                        while (*scanpointsfound < scanpoints
                               && *scanpointsfound < (size_t)dimension) {

                            scanarray[*scanpointsfound] =
                            displacement + displacement_increment
                            *(dimension - *scanpointsfound - 1);

                            (*scanpointsfound)++;
                
                
                }
                

                    } else if (axis_type == CBF_ROTATION_AXIS){

                        while (*scanpointsfound < scanpoints
                               && *scanpointsfound < (size_t)dimension) {
                
                            scanarray[*scanpointsfound] =
                            angle + angle_increment
                            *(dimension - *scanpointsfound - 1);
                
                            (*scanpointsfound)++;
                    
                    
                        }
                    
                }
                }

                return CBF_SUCCESS;
                
            }

            while (!cbf_find_nextrow(handle,axis_id)&& scanpoints > 0) {
            
                unsigned int row;
                                            
                const char * frame_id;
                        
                double newstart;
            
                unsigned int frame_no;
                
                row = handle->row;
                
                frame_no = 0;
                
                if ( axis_type == CBF_ROTATION_AXIS ) {
                    
                    cbf_failnez(cbf_find_column(handle,"angle"));
                    
                    cbf_failnez(cbf_get_doublevalue(handle, &newstart));
                    
                    frame_no = 1;
                    
                } else if ( axis_type == CBF_TRANSLATION_AXIS ) {
                    
                    cbf_failnez(cbf_find_column(handle,"displacement"));
                    
                    cbf_failnez(cbf_get_doublevalue(handle, &newstart));
                    
                    frame_no = 1;
                    
                }
                
                cbf_debug_print3("axis %s newstart %g\n", axis_id,newstart);
                
                if ( frame_no == 1 ) {
                    
                    if (!cbf_find_column(handle,"frame_id")
                        &&!cbf_get_value(handle,&frame_id)
                        &&frame_id) {
                    
                        if (!cbf_find_category(handle,"diffrn_scan_frame")
                            && !cbf_find_column(handle,"frame_id")
                            && !cbf_rewind_row(handle)
                            && !cbf_find_row(handle,frame_id)
                            && !cbf_find_column(handle,"frame_number")) {
                    
                            if (cbf_get_integervalue(handle,&frame_no)) {
                                frame_no = 1;
                            }
                    
                        
                            cbf_failnez(cbf_find_category(handle, "diffrn_scan_frame_axis"));
                            cbf_failnez(cbf_find_column(handle,"axis_id"));
                            cbf_failnez(cbf_rewind_row(handle))
                            if (cbf_select_row(handle,row+1)) break;
                            
                            
                    } else {
                        
                            frame_no = 1;
                        
                    }
                    
                        if (frame_no > 0 && frame_no <= scanpoints) {
                    
                            scanarray[frame_no-1] = newstart;
                
            }


        }

                }
                
            }
            
        }
        
        if (scanpointsfound == 0) {

            scanarray[0] = 0.;

        }

        return CBF_SUCCESS;

    }


    /* Unit conversion tables */

    /* Unit Prefix Table */
    
    typedef struct{
        
        char * prefix;
        
        char * abbrev;
        
        double convfactor;
        
    } cbf_unit_prefix_struct;
    
#define CBF_UNITS_NUM_PREFIXES 26
    
    cbf_unit_prefix_struct cbf_unit_prefix_table[26] = {
        {"yotta","Y",1.e24},
        {"zetta","Z",1.e21},
        {"exa","E",1.e18},
        {"peta","P",1.e15},
        {"tera","T",1.e12},
        {"giga","G",1.e9},
        {"mega","M",1.e6},
        {"kilo","k",1.e3},
        {"hecto","h",1.e2},
        {"deka","da",10.},
        {"deci","d",1.e-1},
        {"centi","c",1.e-2},
        {"milli","m",1.e-3},
        {"micro","u",1.e-6},
        {"nano","n",1.e-9},
        {"pico","p",1.e-12},
        {"femto","f",1.e-15},
        {"atto","a",1.e-18},
        {"zepto","z",1.e-21},
        {"yocto","y",1.e-24},
        {"kibbi","Ki",1024.},
        {"mebi","Mi",1048576.},
        {"gibi","Gi",1073741824.},
        {"tebi","Ti",1099511627776.},
        {"peti","Pi",1125899906842624.},
        {"exbi","Ei",1152921504606846976.}};
    
    typedef struct {
        
        char * ln1;
        char * ln2;
        char * abbrev;
        double convfactor;
        
    } cbf_unit_name_struct;
    
#define CBF_UNITS_NUM_UNITS 33
    
    cbf_unit_name_struct cbf_unit_name_table[CBF_UNITS_NUM_UNITS] = {
        {"metre","meter","m",1.},              /*1*/
        {"inch","in","m",0.0254},              /*2*/
        {"foot","ft","m",0.3048},              /*3*/
        {"yard","yd","m",0.9144},              /*4*/
        {"mile","mi","m",1609.344},            /*5*/
        {"angstrom","\xc5","m",1.e-10},        /*6*/
        {"litre","liter","L",1.0},             /*7*/
        {"cubic_metre","m^3","L",1.0},         /*8*/
        {"gram",NULL,"g",1.0},                 /*9*/
        {"second","sec","s",1.0},              /*10*/
        {"ampere","amp","A",1.},               /*11*/
        {"kelvin",NULL,"K",1.},                /*12*/
        {"mole",NULL,"mol",1.},                /*13*/
        {"candela",NULL,"cd",1.},              /*14*/
        {"radian",NULL,"rad",1.},              /*15*/
        {"steradian",NULL,"sr",1.},            /*16*/
        {"hertz",NULL,"Hz",1.},                /*17*/
        {"newton",NULL,"N",1.},                /*18*/
        {"joule",NULL,"J",1.},                 /*19*/
        {"electronvolt","eV","J",1.60218e-19}, /*20*/
        {"watt",NULL,"W",1.},                  /*21*/
        {"volt",NULL,"V",1.},                  /*22*/
        {"weber",NULL,"Wb",1.},                /*23*/
        {"becqueral",NULL,"Bq",1.},            /*24*/
        {"curie","Ci","Bq",3.7e10},            /*25*/
        {"pascal",NULL,"Pa",1.},               /*26*/
        {"gray",NULL,"Gy",1.},                 /*27*/
        {"rad",NULL,"Gy",1.e-2},               /*28*/
        {"sievert",NULL,"Sv",1.},              /*29*/
        {"rem",NULL,"Sv",1.e-2},               /*30*/
        {"minute","min","s",60.},              /*31*/
        {"hour","hr","s",3600.},               /*32*/
        {"day","dy","s",86400.}                /*33*/
    };

    
    int cbf_scale_unit(const char * unit, char * *  rev_unit,
                        double * unit_per_rev_unit) {
        
        double convfactor;
        
        /* For internal use, we will convert angstroms,
         meters, and metres to m */
        
        int ii, jj, kk;
        
        long lu, lucut, lp;
        
        int error;
        
        int found_prefix, found_unit;
        
        int pass;
        
        error = CBF_SUCCESS;
        
        if (!unit || ! rev_unit) return CBF_ARGUMENT;
        
        if (unit)  {
            cbf_debug_print2("Scale unit |%s|\n",unit);
        }
        
        if (!unit || !rev_unit) return CBF_FORMAT;
        
        *rev_unit = NULL;
         
        lu = lucut = strlen(unit);
                
        *rev_unit = (char *)malloc(lu+1);
        
        if (!rev_unit)return CBF_ALLOC;
                    
        for (ii=0; ii < lu+1; ii++) (*rev_unit)[ii] = unit[ii];
        
        convfactor = 1.;
        
        /* First remove all long form prefixes, treating them
           as case insensitive.  This will leave only 1 and
           2 character short prefixes.*/
        
        found_prefix = 1;
        
        while (found_prefix) {
            
            found_prefix = 0;
            
            for (ii=0; ii < CBF_UNITS_NUM_PREFIXES; ii++) {
                
                cbf_unit_prefix_struct * upp;
                
                char * prefix;
                                
                upp = cbf_unit_prefix_table+ii;
                
                prefix = upp->prefix;
                
                lp = strlen(prefix);
                
                if (lp <= lu && !cbf_cistrncmp(prefix,*rev_unit,lp)) {
                    
                    found_prefix = 1;
                    
                    convfactor *= upp->convfactor;
                    
                    for (jj = lp; jj < lu+1; jj++) (*rev_unit)[jj-lp] = (*rev_unit)[jj];
                    
                    lu -= lp;
                    
                    lucut = lu;
                    
                }
                
            }
        }
        
        /* Now convert all units to their abbreviations.  But no
           conversion will be done for units followed by '^'.
           A check will be made for a '/'.  If to the right of
           a '/' then the reciprocal of convfactor will be applied.*/
        
        found_unit = 1;
        
        pass = 0;
        
        while (found_unit) {
            
            found_unit = 0;
            
            for (ii=0; ii < CBF_UNITS_NUM_UNITS; ii++) {
            
                cbf_unit_name_struct* unp;
            
                char * ln;
                        
                long lln;
                
                unp = cbf_unit_name_table+ii;
                
                ln = (pass==0)?(unp->ln1):(unp->ln2);
                
                if (ln) {
                    
                  lln =strlen(ln);
                    
                }
                
                lucut = lu;
                
                if (ln && lln<=lucut) {
                    
                    long ilu;
                    
                    for (ilu = lucut-lln; ilu >= 0; ilu--) {
                        
                        int notmatch;
                        
                        if (pass==0) {
                            
                            notmatch = cbf_cistrncmp((*rev_unit)+ilu,ln,lln);

                        } else {
                            
                            notmatch = strncmp((*rev_unit)+ilu,ln,lln);

                        }
                        
                        if (!notmatch) {
                            
                            char * abbrev;
                            
                            size_t labbrev;
                            
                            int okconv = 1;
                            
                            if (ilu < lucut-lln && (*rev_unit)[ilu+lln]=='s') lln++;
                            
                            if (ilu < lucut-lln && (*rev_unit)[ilu+lln]=='^') okconv = 0;
                            
                            if (okconv) {
                                
                                int divide = 0;
                                
                                for (jj= ilu; jj >= 0; jj--) {
                                    
                                    if ((*rev_unit)[jj]=='/') {
                                        
                                        divide = 1;
                                        
                                        break;
                                        
                                    }
                                }
                                
                                if (! divide ) {
                                    
                                    convfactor *= unp->convfactor;
                                    
                                } else {
                                    
                                    convfactor /= unp->convfactor;
                                    
                                }
                                
                                abbrev = unp->abbrev;
                                
                                labbrev = strlen(abbrev);
                                
                                for (jj=0 ; jj< (ssize_t)labbrev; jj++) {
                                      
                                    (*rev_unit)[ilu+jj]=abbrev[jj];
                                    
                                }
                                
                                if ((ssize_t)labbrev < lln) {
                                    
                                    for (kk = labbrev; (ssize_t)(ilu+lln+kk-labbrev) < lu+1; kk++) {
                                        
                                        (*rev_unit)[ilu+kk] = (*rev_unit)[ilu+lln+kk-labbrev];
                                        
                                    }
                                    
                                }
                                
                                lu -= (lln-labbrev);
                                
                            }
                            
                            lucut = ilu;
                            
                            found_unit = 1;
                            
                        }
                        
                    }
                    
                    
                }
                
            }
            
            if (!found_unit && pass==0) {
                
                pass++;
                
                found_unit = 1;

            }
            
        }
        
        if (unit_per_rev_unit) *unit_per_rev_unit = convfactor;
        
        return error;
        
    }

    
    /* cbf_scale_units: return the number of actual units per standard
       unit for an SI or IEC binary-prefixed unit, with special coding
       to relate angstroms to metres.  See 
        http://physics.nist.gov/cuu/Units/prefixes.html
     */
    
    int cbf_scale_units(const char * actual_units, const char * std_units,
                        double * actual_per_std) {
        
        double inconvfactor, outconvfactor;
        
        /* For internal use, we will convert angstroms,
           meters, and metres to m */
        
        char * rev_actual_units;
        char * rev_std_units;
        size_t lau, lsu;
        
        int error;
        
        error = CBF_SUCCESS;
        
        if (std_units && actual_units && cbf_cistrcmp(actual_units, std_units))  {
            cbf_debug_print3("Scale actual |%s| to standard |%s|\n",actual_units,std_units);
        }
        
        if (!std_units ||
            !actual_units ||
            !cbf_cistrcmp(actual_units, std_units)) {
            
            *actual_per_std = 1.;
            
            return error;
            
        }
        
        rev_actual_units = rev_std_units = NULL;
        
        error |= cbf_scale_unit(actual_units,&rev_actual_units,&inconvfactor);
        
        error |= cbf_scale_unit(std_units,&rev_std_units,&outconvfactor);
                
        if (error || !rev_actual_units || !rev_std_units) {
            
            if (rev_actual_units) free (rev_actual_units);
            
            if (rev_std_units) free (rev_std_units);

            return (!error)?CBF_ALLOC:error;
            
        }
        
        lau = strlen(rev_actual_units);
        
        lsu = strlen(rev_std_units);
        
        
        if (lau >= strlen("angstrom") && !cbf_cistrnrcmp(rev_actual_units,"angstrom",lau)) {
            
            inconvfactor = 1.e-10;
            
            rev_actual_units[lau-strlen("angstrom")] = 'm';
            
            rev_actual_units[lau-strlen("angstrom")+1] = '\0';
            
            
        } else if (lau >= strlen("angstroms") && !cbf_cistrnrcmp(rev_actual_units,"angstroms",lau)) {
            
            inconvfactor = 1.e-10;
            
            rev_actual_units[lau-strlen("angstroms")] = 'm';
            
            rev_actual_units[lau-strlen("angstroms")+1] = '\0';
            
            
        } else if (lau >= strlen("metres") && (!cbf_cistrnrcmp(rev_actual_units,"metres",lau) ||
                   !cbf_cistrnrcmp(rev_actual_units,"meters",lau))) {
            
            inconvfactor = 1.;
            
            rev_actual_units[lau-strlen("metres")] = 'm';
            
            rev_actual_units[lau-strlen("metres")+1] = '\0';
            
            
        } else if (lau >= strlen("metre") && (!cbf_cistrnrcmp(rev_actual_units,"metre",lau) ||
                   !cbf_cistrnrcmp(rev_actual_units,"meter",lau))) {
            
            inconvfactor = 1.;
            
            rev_actual_units[lau-strlen("metre")] = 'm';
            
            rev_actual_units[lau-strlen("metre")+1] = '\0';
            
            
        }
        
        if (lsu >= strlen("angstrom") && !cbf_cistrnrcmp(rev_std_units,"angstrom",lsu)) {
            
            outconvfactor *= 1.e-10;
            
            rev_std_units[lsu-strlen("angstrom")] = 'm';
            
            rev_std_units[lsu-strlen("angstrom")+1] = '\0';
            
            
        } else if (lsu >= strlen("angstroms") && !cbf_cistrnrcmp(rev_std_units,"angstroms",lsu)) {
            
            outconvfactor *= 1.e-10;
            
            rev_std_units[lsu-strlen("angstroms")] = 'm';
            
            rev_std_units[lsu-strlen("angstroms")+1] = '\0';
            
            
        } else if (lsu >= strlen("metres") && (!cbf_cistrnrcmp(rev_std_units,"metres",lsu) ||
                   !cbf_cistrnrcmp(rev_std_units,"meters",lsu))) {
            
            rev_std_units[lsu-strlen("metres")] = 'm';
            
            rev_std_units[lsu-strlen("metres")+1] = '\0';
            
            
        } else if (lsu >= strlen("metre") && (!cbf_cistrnrcmp(rev_std_units,"metre",lsu) ||
                   !cbf_cistrnrcmp(rev_std_units,"meter",lsu))) {
            
            rev_std_units[lsu-strlen("metre")] = 'm';
            
            rev_std_units[lsu-strlen("metre")+1] = '\0';
            
            
        }
        
        lau = strlen(rev_actual_units);
        
        lsu = strlen(rev_std_units);
        
        if (cbf_cistrcmp(rev_actual_units,rev_std_units) || inconvfactor != 1. || outconvfactor != 1.) {
        cbf_debug_print3("Scale actual |%s| to standard |%s|\n",rev_actual_units,rev_std_units);
        cbf_debug_print3("inconvfactor |%g| outconvfactor |%g|\n",inconvfactor,outconvfactor);
        }
        
        if (lau == lsu+1
                   
            && !cbf_cistrcmp(rev_actual_units+1,rev_std_units)) {
            
            switch (rev_actual_units[0]) {
                    
                case ('d') : *actual_per_std =  1.e-1; break;
                case ('c') : *actual_per_std =  1.e-2; break;
                case ('m') : *actual_per_std =  1.e-3; break;
                case ('u') : *actual_per_std =  1.e-6; break;
                case ('n') : *actual_per_std =  1.e-9; break;
                case ('p') : *actual_per_std = 1.e-12; break;
                case ('f') : *actual_per_std = 1.e-15; break;
                case ('a') : *actual_per_std = 1.e-18; break;
                case ('z') : *actual_per_std = 1.e-21; break;
                case ('y') : *actual_per_std = 1.e-24; break;
                case ('h') : *actual_per_std =   1.e2; break;
                case ('K') : *actual_per_std =   1.e3; break;
                case ('k') : *actual_per_std =   1.e3; break;
                case ('M') : *actual_per_std =   1.e6; break;
                case ('G') : *actual_per_std =   1.e9; break;
                case ('T') : *actual_per_std =  1.e12; break;
                case ('P') : *actual_per_std =  1.e15; break;
                case ('E') : *actual_per_std =  1.e18; break;
                case ('Z') : *actual_per_std =  1.e21; break;
                case ('Y') : *actual_per_std =  1.e24; break;
                default:  error = CBF_FORMAT;
                    
            }
            
            if (!error) {
                
                *actual_per_std *= (inconvfactor/outconvfactor);
                
            }
            
            if (rev_actual_units) free (rev_actual_units);
            
            if (rev_std_units) free (rev_std_units);
            
            cbf_debug_print2("actual units per standard unit = %g\n", *actual_per_std);
            
            return error;
            
        } else if (lau == lsu+2
                   
                   && !cbf_cistrcmp(actual_units+2,std_units)) {
            
            switch (rev_actual_units[0]) {
                case ('d') :
                    if (rev_actual_units[1]=='a') {
                        *actual_per_std =  10.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                case ('K') :
                    if (rev_actual_units[1]=='i') {
                        *actual_per_std =  1024.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                case ('M') :
                    if (rev_actual_units[1]=='i') {
                        *actual_per_std =  1048576.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                case ('G') :
                    if (rev_actual_units[1]=='i') {
                        *actual_per_std =  1073741824.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                case ('T') :
                    if (rev_actual_units[1]=='i') {
                        *actual_per_std =  1048576.*1048576.; break;
                    } else {
                         error = CBF_FORMAT;
                    }
                case ('P') :
                    if (rev_actual_units[1]=='i') {
                        *actual_per_std =  1048576.*1073741824.; break;
                    } else {
                         error = CBF_FORMAT;
                    }
                case ('E') :
                    if (rev_actual_units[1]=='i') {
                        *actual_per_std =  1073741824.*1073741824.; break;
                    } else {
                         error = CBF_FORMAT;
                    }
                default:  error = CBF_FORMAT;
                    
            }
            
            *actual_per_std *= (inconvfactor/outconvfactor);
            
            if (rev_actual_units) free (rev_actual_units);
            
            if (rev_std_units) free (rev_std_units);
            
            cbf_debug_print2("actual units per standard unit = %g\n", *actual_per_std);
            
            return CBF_SUCCESS;
            
            
        } else if (lau == lsu-1
                   
                   && !cbf_cistrcmp(actual_units,std_units+1)) {
            
            int error;
            
            error = CBF_SUCCESS;
            
            switch (std_units[0]) {
                    
                case ('d') : *actual_per_std =   1.e1; break;
                case ('c') : *actual_per_std =   1.e2; break;
                case ('m') : *actual_per_std =   1.e3; break;
                case ('u') : *actual_per_std =   1.e6; break;
                case ('n') : *actual_per_std =   1.e9; break;
                case ('p') : *actual_per_std =  1.e12; break;
                case ('f') : *actual_per_std =  1.e15; break;
                case ('a') : *actual_per_std =  1.e18; break;
                case ('z') : *actual_per_std =  1.e21; break;
                case ('y') : *actual_per_std =  1.e24; break;
                case ('h') : *actual_per_std =  1.e-2; break;
                case ('K') : *actual_per_std =  1.e-3; break;
                case ('k') : *actual_per_std =  1.e-3; break;
                case ('M') : *actual_per_std =  1.e-6; break;
                case ('G') : *actual_per_std =  1.e-9; break;
                case ('T') : *actual_per_std = 1.e-12; break;
                case ('P') : *actual_per_std = 1.e-15; break;
                case ('E') : *actual_per_std = 1.e-18; break;
                case ('Z') : *actual_per_std = 1.e-21; break;
                case ('Y') : *actual_per_std = 1.e-24; break;
                default:  error = CBF_FORMAT;

            }
            
            if (!error) {
                
                *actual_per_std *= (inconvfactor/outconvfactor);
                
            }
            
            if (rev_actual_units) free (rev_actual_units);
            
            if (rev_std_units) free (rev_std_units);
            
            cbf_debug_print2("actual units per standard unit = %g\n", *actual_per_std);
            
            return error;
            
        } else if (lau == lsu-2
                   
                   && !cbf_cistrcmp(actual_units,std_units+2)) {
            
            int error;
            
            error = CBF_SUCCESS;
            
            switch (rev_std_units[0]) {
                case ('d') :
                    if (rev_std_units[1]=='a') {
                        *actual_per_std =  10.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                case ('K') :
                    if (rev_std_units[1]=='i') {
                        *actual_per_std =  1024.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                case ('M') :
                    if (rev_std_units[1]=='i') {
                        *actual_per_std =  1048576.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                case ('G') :
                    if (rev_std_units[1]=='i') {
                        *actual_per_std =  1073741824.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                case ('T') :
                    if (rev_std_units[1]=='i') {
                        *actual_per_std =  1048576.*1048576.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                case ('P') :
                    if (rev_std_units[1]=='i') {
                        *actual_per_std =  1048576.*1073741824.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                case ('E') :
                    if (rev_std_units[1]=='i') {
                        *actual_per_std =  1073741824.*1073741824.; break;
                    } else {
                        error = CBF_FORMAT;
                    }
                    break;
                default:  error = CBF_FORMAT;
                    
            }
            
            if (!error) {
                
                *actual_per_std *= (inconvfactor/outconvfactor);
                
            }
            
            if (rev_actual_units) free (rev_actual_units);
            
            if (rev_std_units) free (rev_std_units);
            
            cbf_debug_print2("actual units per standard unit = %g\n", *actual_per_std);
            
            return error;
            
            
       } else if (lau == lsu
                   
                   && lau > 1
                   
                   && !cbf_cistrcmp(rev_actual_units+1,rev_std_units+1)) {
            
            switch (actual_units[0]) {
                    
                case ('d') : *actual_per_std =  1.e-1; break;
                case ('c') : *actual_per_std =  1.e-2; break;
                case ('m') : *actual_per_std =  1.e-3; break;
                case ('u') : *actual_per_std =  1.e-6; break;
                case ('n') : *actual_per_std =  1.e-9; break;
                case ('p') : *actual_per_std = 1.e-12; break;
                case ('f') : *actual_per_std = 1.e-15; break;
                case ('a') : *actual_per_std = 1.e-18; break;
                case ('z') : *actual_per_std = 1.e-21; break;
                case ('y') : *actual_per_std = 1.e-24; break;
                case ('h') : *actual_per_std =   1.e2; break;
                case ('K') : *actual_per_std =   1.e3; break;
                case ('k') : *actual_per_std =   1.e3; break;
                case ('M') : *actual_per_std =   1.e6; break;
                case ('G') : *actual_per_std =   1.e9; break;
                case ('T') : *actual_per_std =  1.e12; break;
                case ('P') : *actual_per_std =  1.e15; break;
                case ('E') : *actual_per_std =  1.e18; break;
                case ('Z') : *actual_per_std =  1.e21; break;
                case ('Y') : *actual_per_std =  1.e24; break;
                default:  error = CBF_FORMAT;
                    
            }
            
            switch (std_units[0]) {
                    
                case ('d') : *actual_per_std *=   1.e1; break;
                case ('c') : *actual_per_std *=   1.e2; break;
                case ('m') : *actual_per_std *=   1.e3; break;
                case ('u') : *actual_per_std *=   1.e6; break;
                case ('n') : *actual_per_std *=   1.e9; break;
                case ('p') : *actual_per_std *=  1.e12; break;
                case ('f') : *actual_per_std *=  1.e15; break;
                case ('a') : *actual_per_std *=  1.e18; break;
                case ('z') : *actual_per_std *=  1.e21; break;
                case ('y') : *actual_per_std *=  1.e24; break;
                case ('h') : *actual_per_std *=  1.e-2; break;
                case ('K') : *actual_per_std *=  1.e-3; break;
                case ('k') : *actual_per_std *=  1.e-3; break;
                case ('M') : *actual_per_std *=  1.e-6; break;
                case ('G') : *actual_per_std *=  1.e-9; break;
                case ('T') : *actual_per_std *= 1.e-12; break;
                case ('P') : *actual_per_std *= 1.e-15; break;
                case ('E') : *actual_per_std *= 1.e-18; break;
                case ('Z') : *actual_per_std *= 1.e-21; break;
                case ('Y') : *actual_per_std *= 1.e-24; break;
                default:  error = CBF_FORMAT;
            }
            
           if (!error) {
               
               *actual_per_std *= (inconvfactor/outconvfactor);
               
           }
           
           if (rev_actual_units) free (rev_actual_units);
           
           if (rev_std_units) free (rev_std_units);
           
           cbf_debug_print2("actual units per standard unit = %g\n", *actual_per_std);
           
           return error;
           
        }
        
        if (rev_actual_units) free (rev_actual_units);
        
        if (rev_std_units) free (rev_std_units);
        
        cbf_debug_print2("actual units per standard unit = %g\n", *actual_per_std);
        return CBF_FORMAT;

    }




#ifdef __cplusplus

}
#endif
