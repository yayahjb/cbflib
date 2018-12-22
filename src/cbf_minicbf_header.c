/**********************************************************************
 * cbf_minicbf_header.c -- handle minicbf header in a cbf             *
 *                                                                    *
 * based in part on adsccbf2img by Chris Nielsen of ADSC in 2007      *
 *                                                                    *
 * Version 0.9.5 19 September 2015                                    *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006 -- 2015 Herbert J. Bernstein                    *
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
    
#include "cbf_minicbf_header.h"
#include <time.h>
    
/* cbf_set_minicbf_header -- format a minicbf header conforming both
 to DECTRIS "PILATUS CBF Header Specification" version 1.4 and to the
 requirements of Winter, Graeme, and Alun Ashton. "fast_dp." Methods 
 55 (2014): 81-93, https://zenodo.org/record/13039, 
 
 */
    
#define MAX_SRCLEN 132
    
#define CBF_GET_COLUMN_VALUE(cbf, colname, scratch_value, result_value) \
/**/    result_value = NULL;                                 \
/**/    if (!cbf_find_column((cbf),(colname))                \
/**/        &&!cbf_rewind_row((cbf))                         \
/**/        &&!cbf_get_value((cbf),&(scratch_value))         \
/**/        &&(scratch_value)                                \
/**/        &&strlen((scratch_value))> 0) {                  \
/**/                                                         \
/**/        result_value = (scratch_value);                  \
/**/                                                         \
/**/    }
    
    static const char * cbf_trim_left(const char * src) {
        
        const char * trimmed = src;
        
        while (*trimmed && isspace(*trimmed)) trimmed++;
        
        return trimmed;
            
    }
    
    static int cbf_append_string(char * * dst,
                                 size_t * dstcapacity,
                                 size_t * dstsize,
                                 const char * src) {
        
        size_t srclen;
        
        if (!dst || !dstcapacity || !dstsize || ! src) return CBF_ARGUMENT;
        
        srclen = strlen(src);
        
        if (*dst) {
            
            if (*dstsize + srclen + 1 > *dstcapacity) {
                
                cbf_failnez(cbf_realloc((void **)dst,dstcapacity, sizeof(char),
                                        *dstsize + srclen + 1 + *dstcapacity ));
                
            }
            
            strncat(*dst,src,srclen);
            
            *dstsize += srclen;
            
            return CBF_SUCCESS;
            
        }
        
        cbf_failnez(cbf_alloc((void **)dst,NULL,sizeof(char),1+srclen));
        
        *dstcapacity = 1+srclen;
        
        strncpy(*dst,src,srclen);
        
        (*dst)[srclen] = '\0';
        
        *dstsize = srclen;
        
        return CBF_SUCCESS;
        
    }
    
    /* Populate or rewrite a minicbf header in _array_data.header_contents */
    
    int cbf_set_minicbf_header(cbf_handle cbf, cbf_handle datacbf, char ** log) {
        
        char * header;
        
        size_t header_capacity, header_size, log_capacity, log_size;
        
        const char * array_id;
        
        char buffer[MAX_SRCLEN+1];
        
        int dimension[2], precedence[2];
        
        const char * array_intensities_details;
        
        double thickness;
        
        double wavelength;
        
        double beam_center[4];
        
        double twotheta;
        
        const char * detector_type;
        
        const char * detector_details;
        
        const char * detector_serial_number;
        
        cbf_detector detector_handle;
        
        cbf_goniometer goniometer_handle;
        
        double goniometer_start, goniometer_increment;
        
        double sample_detector_distance;
        
        int reserved=0, year, month, day, hour, minute, timezone;
        
        double second;
        
        size_t rank;
        
        const char * integration_time;
        
        const char * integration_period;
        
        double dtime;
        
        const char * overload;
        
        const char * threshold;
        
        const char * gain;
        
        const char * energy_range_low;
        
        const char * energy_range_high;
        
        double sample_detector_voffset;
        
        const char * flux;
        
        const char * filter_transmission;
        
        const char * polarizn_source_ratio;
        
        const char * oscillations;
        
        int result;

        CBF_UNUSED( precedence );
        
        /* check the arguments */
        
        if (!cbf || !datacbf)
            
            return CBF_ARGUMENT;
        
        if (log) {
            
            *log = NULL;
            
            log_capacity = log_size = 0;
            
        }
        
        header = NULL;
        
        header_capacity = header_size = 0;
        
        cbf_failnez(cbf_rewind_datablock(cbf));
        
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                      "\n"));
            
        /* fprintf(stderr,"header:\n%s\n",header); */
        
        /* Get the image dimensions */
        
        
        if (cbf_find_category(cbf, "array_structure_list")
            || cbf_find_column(cbf,"array_id")
            || cbf_rewind_row(cbf)
            || cbf_get_value(cbf,&array_id)
            || !(array_id) ) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: "
                                              "unable to locate 'array_structure_list.array_id' \n"));
            }
            
            return CBF_FORMAT;
            
        }
        
        if (cbf_get_array_section_rank(cbf,array_id,&rank) || rank!=2) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: "
                                              "unable to locate image array rank \n"));
            }
            
            return CBF_FORMAT;
            
            
        }
        
        
        cbf_failnez(cbf_find_category(cbf, "array_structure_list"));
        cbf_failnez(cbf_find_column(cbf,"array_id"));
        cbf_failnez( cbf_rewind_row(cbf) );
        
        dimension[0] = 0;
        dimension[1] = 0;
        
        while (cbf_find_nextrow (cbf, array_id) == 0) {
            
            int i, index;
            const char * direction[rank];
            
            cbf_failnez (cbf_find_column      (cbf, "index"))
            cbf_failnez (cbf_get_integervalue (cbf, &index))
            i = index;
            cbf_failnez (cbf_find_column      (cbf, "precedence"))
            cbf_failnez (cbf_get_integervalue (cbf, &index))
            if (index >= 1 && index <= 2)
                precedence[i-1] = index;
            
            cbf_failnez (cbf_find_column      (cbf, "dimension"))
            cbf_failnez (cbf_get_integervalue (cbf, &dimension[i-1]))
            
            cbf_failnez (cbf_find_column      (cbf, "direction"))
            cbf_failnez (cbf_get_value        (cbf, &direction[i-1]))
            
            cbf_failnez (cbf_find_column      (cbf, "array_id"))
            
        }
        
        if (dimension [0] == 0 || dimension [1] == 0) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: error: "
                                              "unable to locate image array dimensions' \n"));
                
            }
            
            return CBF_FORMAT;
            
        }
        
        /* Get the wavelength and distance */
        
        if (cbf_get_wavelength (cbf, &wavelength) || wavelength <= 0.) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: error: "
                                              "unable to locate wavelength' \n"));
            }
            
            return CBF_FORMAT;
            
        }
        
        goniometer_handle = NULL;
        
        if (cbf_construct_goniometer (cbf, &goniometer_handle)) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: error: "
                                              "unable to locate goniomete axes \n"));
            }
            
            return CBF_FORMAT;

        }
        
        /*  Pick up goniometer start and increment */
        
        result = cbf_get_rotation_range(goniometer_handle, reserved,
                                           &goniometer_start, &goniometer_increment);
        
        if (result) {
            
            goniometer_start = 0.0;
            
            goniometer_increment = 0.0;
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "unable to get goniometer start and increment \n"));
            }
            
        }
        
        cbf_failnez(cbf_free_goniometer(goniometer_handle));
        
        detector_handle = NULL;
        
        if (cbf_construct_detector (cbf, &detector_handle, 0 )
            || cbf_get_beam_center (detector_handle,
                                    &beam_center[0],
                                    &beam_center[1],
                                    &beam_center[2],
                                    &beam_center[3])) {
                
                if (log) {
                    
                    cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                                  "cbf_set_minicbf_header: error: "
                                                  "unable to locate beam center \n"));
                }
                
                return CBF_FORMAT;
                
            }
        
        if (!detector_handle
            || cbf_get_detector_distance(detector_handle, &sample_detector_distance)) {
            
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: error: "
                                              "unable to locate sample_detector_distancer \n"));
            }
            
            return CBF_FORMAT;
            
            
        }
        
        /* Pick up 2 theta  */
        
        {
            double normal[3];
            
            cbf_failnez(cbf_get_detector_normal(detector_handle,&(normal[0]), &(normal[1]), &(normal[2])));
            
            twotheta = atan2(sqrt(normal[0]*normal[0]+normal[1]*normal[1]),-normal[2])*45./atan2(1.,1.);
            
        }
        
        cbf_failnez(cbf_free_detector(detector_handle));
                    
        
        /* Gather gain and overload */
        
        if (cbf_find_category      (cbf, "array_intensities")
            || cbf_find_column     (cbf, "gain")
            || cbf_rewind_row      (cbf)
            || cbf_get_value       (cbf, &gain)) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "unable to locate gain' \n"));
            }
            
            gain = NULL;
            
            
        }
        
        if (cbf_find_category      (cbf, "array_intensities")
            || cbf_find_column     (cbf, "overload")
            || cbf_rewind_row      (cbf)
            || cbf_get_value (cbf, &overload)) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "unable to locate overload' \n"));
            }
            
            overload = NULL;
            
        }
        
        
        /* Construct the Detector: PILATUS 6M S/N01 line */
        
        if (cbf_find_category(cbf,"diffrn_detector")) {
            
            detector_type = NULL;
            
            detector_details = NULL;
            
            detector_serial_number = NULL;
            
        } else {
            
            const char * value;
            
            
            CBF_GET_COLUMN_VALUE(cbf,"type",value,detector_type);
            CBF_GET_COLUMN_VALUE(cbf,"details",value,detector_details);
            CBF_GET_COLUMN_VALUE(cbf,"serial_number",value,detector_serial_number);
            
        }
        
        if (!detector_type) {
            
            if (log) {
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: error: "
                                              "unable to find diffrn_detector.type\n"));
                
            }
            
            return CBF_FORMAT;
        }
        
        {
            const char * idp;
            
            size_t idpcount;
            
            idpcount = 0;
            
            idp = detector_type;
            
            while (*idp && idpcount < MAX_SRCLEN) {
                
                buffer[idpcount++] = toupper(*idp++);
                
            }
            
            buffer[idpcount] = '\0';
            
            
        }
        
        cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                      "# Detector: "));
        cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                      buffer));
        if (detector_serial_number) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_serial_number,"SN",2))) {
                
                snp += 2;
                
            } else if ((snp=cbf_cistrnstr(detector_serial_number,"S/N",3))) {
                
                snp += 3;
                
            } else {
                
                snp = detector_serial_number;
            }
            
            while (*snp) {
                
                if (*snp == ' '
                    || *snp == '='
                    || *snp == ':'
                    || *snp == ','
                    || *snp == '('
                    || *snp == ')'
                    || *snp == '#') snp++;
                
                else break;
                
            }
            
            strcpy(buffer,"SN: ");
            
            snpdst = (char *)buffer;
            
            snpdst += 4;
            
            isnp = 4;
            
            while (*snp && isnp <= MAX_SRCLEN) {
                
                if (*snp != ' '
                    && *snp != '='
                    && *snp != ':'
                    && *snp != '\n'
                    && *snp != '\r') *(snpdst++) = *(snp++);
                else break;
                
                isnp++;
            }
            
            snpdst[0]='\0';
            
            snpdst++;
            
            
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          " "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          buffer));
            
            
        } else if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_details,"SN",2))) {
                
                snp += 2;
                
            } else if ((snp=cbf_cistrnstr(detector_details,"S/N",3))) {
                
                snp += 3;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                strcpy(buffer,"SN: ");
                
                snpdst = (char *)buffer;
                
                snpdst += 4;
                
                isnp = 4;
                
                while (*snp && isnp <= MAX_SRCLEN) {
                    
                    if (*snp != ' '
                        && *snp != '='
                        && *snp != ':'
                        && *snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              " "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        }
        
        cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                      "\n"));
        
        /* Get the date and time */
        
        if (!cbf_get_datestamp(cbf,reserved,
                               &year,&month,&day,&hour,&minute,&second,&timezone)) {
            
            sprintf(buffer,"# %04d-%02d-%02dT%02d:%02d:%06.3f\n", year, month, day,
                    hour, minute, second);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          buffer));
            
        } else {
            

            time_t time_t_time;
            
            struct tm * tm_time;
            
            size_t strftlen;
            
            tm_time = localtime (&time_t_time);
            
            strftlen = strftime(buffer,MAX_SRCLEN,"# %Y-%m-%dT%H:%M:%S",tm_time);
            
            buffer[MAX_SRCLEN] = '\0';
            
            if (log){
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "unable to find date and time\n"));
                
            }
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          buffer));
            
            if (!strftlen) return CBF_FORMAT;
            
        }
        
        /* Get the pixel sizes Pixel_size 172e-6 m x 172e-6 m */
        
        {
            
            double psizes[rank];
            
            if (!cbf_get_array_section_pixel_sizes(cbf,array_id,rank,psizes)) {
                
                int psizex, psizey;
                
                psizex = (int)(psizes[0]*1.e3+.05);
                
                psizey = (int)(psizes[1]*1.e3 +.05);
                
                sprintf(buffer,"# Pixel_size %de-6 m x %de-6 m\n", psizex, psizey);
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
                
                
            } else {
                
                if (log){
                    
                    cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                                  "cbf_set_minicbf_header: error: "
                                                  "unable to find pixel size\n"));
                    
                }
                
                return CBF_FORMAT;
                
                
            }
            
        }
        
        /* Set the Silicon sensor, thickness 0.000320 m line*/
        
        if (!cbf_find_category(cbf,"diffrn_detector")
            &&!cbf_find_column(cbf,"layer_thickness")
            &&!cbf_get_doublevalue(cbf,&thickness)
            &&thickness > 0.) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Silicon sensor, thickness "));
            
            sprintf(buffer,"%12.7f m\n", thickness*1.e-3);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));

        
        } else if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            /* fprintf(stderr,detector_details); */
            
            if ((snp=cbf_cistrnstr(detector_details,"thickness",9))) {
                
                snp += 9;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') {
                        
                        *snpdst = *snp;
                        
                        snpdst++;
                        
                        snp++;
                        
                    }
                    else break;
                    
                    isnp++;
                }
                
                *snpdst = '\n'; snpdst++;
                
                *snpdst = '\0'; snpdst++;
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# Silicon sensor, thickness "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        } else if (log) {
         
            cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                          "cbf_set_minicbf_header: warning: "
                                          "layer_thickness not provided\n"));
        }
        
        /* Set N_excluded_pixels = 293 line*/
        
        if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_details,"N_excluded_pixels",17))) {
                
                snp += 15;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\n'; snpdst++;
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# N_excluded_pixels = "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        }
        
        
        /* Set Excluded_pixels: badpix_mask.tif line */
        
        if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_details,"Excluded_pixels:",16))) {
                
                snp += 16;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\n'; snpdst++;
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# Excluded_pixels: "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        }
        
        /* Set Flat_field: FF_p2m0109_E15000_T7500_vrf_m0p30.tif  line*/
        
        if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_details,"Flat_field",10))) {
                
                snp += 10;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\n'; snpdst++;
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# Flat_field: "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        }
        
        /* Trim_file: p2m0109_E15000_T7500_vrf_m0p30.bin */
        
        if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_details,"Trim_file",9))) {
                
                snp += 9;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\n'; snpdst++;
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# Trim_file: "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        }
        
        /* Image_path: /ramdisk/e13295/20120404/6D/BCW/1947_9_3/short/15kev/ */
        
        if (detector_details) {
            
            const char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=cbf_cistrnstr(detector_details,"Image_path",10))) {
                
                snp += 10;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\n'; snpdst++;
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# Image_path: "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              buffer));
            }
            
        }
        
        /* Set Exposure_time 0.095000 s line*/
        
        if (!cbf_find_category(cbf,"diffrn_scan_frame")
            && !cbf_find_column(cbf,"integration_time")
            && !cbf_rewind_row(cbf)
            && !cbf_get_value(cbf,&integration_time)
            && (integration_time) && (*integration_time) ) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Exposure_time "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          integration_time));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "s \n"));
            
        }
        
        /* Set Exposure_period 0.100000 s line*/
        
        if (!cbf_find_category(cbf,"diffrn_scan_frame")
            && !cbf_find_column(cbf,"integration_period")
            && !cbf_rewind_row(cbf)
            && !cbf_get_value(cbf,&integration_period)
            && (integration_period) && (*integration_period) ) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Exposure_period "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          integration_period));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          " s\n"));
            
        }
        
        /* Set Tau = 0 s line */
        
        if (!cbf_find_category(cbf,"diffrn_detector")
            && !cbf_find_column(cbf,"dtime")
            && !cbf_rewind_row(cbf)
            && !cbf_get_doublevalue(cbf,&dtime)
            && dtime >=0.) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Tau = "));
            
            sprintf(buffer,"%15.6g s\n",dtime/1.e6);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        } else if (!cbf_find_category(cbf,"diffrn_radiation")
                                      && !cbf_find_column(cbf,"tau")
                                      && !cbf_rewind_row(cbf)
                                      && !cbf_get_doublevalue(cbf,&dtime)
                                      && dtime >=0.) {
            /* handle the older, misplaced tau */

            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Tau = "));
            
            sprintf(buffer,"%15.6g s\n",dtime/1.e6);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
        } else {
            
            /* Dummy in a 0 */
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Tau = 0 s\n"));

            if (log){
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "dummy Tau = 0 s used\n"));
                
        }
        
                                      
        }
                   
        
                   
        /* Set Count_cutoff 1048575 counts line */
        
        if (!(overload)) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Count_cutoff "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          overload));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "\n"));
            
        }
        
        /* Set Threshold_setting 5000 eV line */
        
        if (!cbf_find_category(cbf,"diffrn_detector")
            && !cbf_find_column(cbf,"threshold")
            && !cbf_rewind_row(cbf)
            && !cbf_get_value(cbf,&threshold)
            && (threshold) && (*threshold)) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Threshold_setting "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          threshold));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          " eV\n"));
            
        }
        
        
        /* Set Gain: 1.00 line */
        
        if (!(gain)) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Gain: "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          gain));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "\n"));
            
        }
        
        /* Set Gain_setting: low gain (vrf = -0.300) line */
        
        if (!cbf_find_category(cbf,"array_intensities")
            &&!cbf_find_column(cbf,"details")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_value(cbf,&array_intensities_details)
            &&(array_intensities_details)
            &&(*array_intensities_details)) {
            
            char * snp;
            
            char * snpdst;
            
            size_t isnp;
            
            if ((snp=(char *)cbf_cistrnstr(detector_details,"Gain_setting",12))) {
                
                snp += 12;
                
            } else {
                
                snp = NULL;
            }
            
            if (snp) {
                
                while (*snp) {
                    
                    if (*snp == ' '
                        || *snp == '='
                        || *snp == ':'
                        || *snp == ','
                        || *snp == '('
                        || *snp == ')'
                        || *snp == '#') snp++;
                    
                    else break;
                    
                }
                
                snpdst = (char *)buffer;
                
                isnp = 0;
                
                while (*snp && isnp <= MAX_SRCLEN-1) {
                    
                    if (*snp != '\n'
                        && *snp != '\r') *(snpdst++) = *(snp++);
                    else break;
                    
                    isnp++;
                }
                
                snpdst[0]='\n'; snpdst++;
                
                snpdst[0]='\0'; snpdst++;
                
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              "# Gain_setting: "));
                
                cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                              cbf_trim_left(buffer)));
            }
            
        }
        
        if (wavelength > 0.) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Wavelength "));
            
            sprintf(buffer,"%9.4f A\n",wavelength);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        }
        
        if (!cbf_find_category(cbf,"diffrn_detector")
            &&!cbf_find_column(cbf,"energy_range_low")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_value(cbf,&energy_range_low)
            &&energy_range_low && (*energy_range_low)
            &&!cbf_find_column(cbf,"energy_range_high")
            &&!cbf_get_value(cbf,&energy_range_high)
            &&energy_range_high && (*energy_range_high) ) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Energy_range ( "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          energy_range_low));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          ", "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          energy_range_high));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          ") eV\n"));
            
            
        } else {
            
            /*Dummy in a Energy_range (0, 0) eV\n */
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Energy_range (0, 0) eV\n"));
            
            if (log){
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "dummy Energy_range (0, 0) eV used\n"));
                
        }
        
        
        }
        
        
        
        /* Set Detector_distance 0.79988 m line */
        
        if (sample_detector_distance > 0.) {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Detector_distance "));
            
            sprintf(buffer,"%9.5f m\n",sample_detector_distance/1.e3);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        }
        
        /* Set Detector_Voffset -0.00002 m line */
        
        if (!cbf_find_category(cbf,"diffrn_measurement")
            &&!cbf_find_column(cbf,"sample_detector_voffset")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_doublevalue(cbf,&sample_detector_voffset)){
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Detector_Voffset "));
            
            sprintf(buffer,"%14.5f m\n",sample_detector_voffset/1.e3);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        } else {
            
            /*Dummy in a Voffset of 0 */
             
             cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                         "# Detector_Voffset 0.00000 m\n"));
            
            if (log){
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "dummy Detector_Voffset 0 m  used\n"));
                
        }
        
        }
        
        /* Set Beam_xy (1231.50, 1263.50) pixels line */
        
        {
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Beam_xy ("));
            
            sprintf(buffer,"%10.2f, ", beam_center[1]);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));

            sprintf(buffer,"%10.2f) pixels\n", beam_center[0]);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        }
        
        /* Set Flux 0 ph/s line */
        
        if (!cbf_find_category(cbf,"diffrn_radiation")
            &&!cbf_find_column(cbf,"flux")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_value(cbf,&flux)
            && (flux) && (*flux)){
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Flux "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          flux));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          " ph/s\n"));
            
        } else {
            
            /*Dummy in a Flux of 0 */
            
            if (log){
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "dummy Flux 0 ph/s used\n"));
                
        }
        }

        
        /* Set Filter_transmission 1.0000 line */
        
        if (!cbf_find_category(cbf,"diffrn_radiation")
            &&!cbf_find_column(cbf,"filter_transmission")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_value(cbf,&filter_transmission)
            && (filter_transmission) && (*filter_transmission)){
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Filter_transmission "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          filter_transmission));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "\n"));
            
        }

        
        /* Set Start_angle 0.0900 deg. */
        
        {
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Start_angle "));
            
            sprintf(buffer,"%11.4f deg.\n", goniometer_start);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        }

        
        /* Set Oscillation_axis  X, CW line */
        
        cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                      "# Oscillation_axis  X, CW\n"));
        
        
        /* Set Angle_increment 0.0100 deg. line */
        
        {
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Angle_increment "));
            
            sprintf(buffer,"%11.4f deg.\n", goniometer_increment);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        }

        
        /* Set Detector_2theta 0.0000 deg. line */
        
        {
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Detector_2theta "));
            
            sprintf(buffer,"%11.4f deg.\n", twotheta);
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          cbf_trim_left(buffer)));
            
        }
        
        /* Set Polarization 0.950 line */
        
        if (!cbf_find_category(cbf,"diffrn_radiation")
            &&!cbf_find_column(cbf,"polarizn_source_ratio")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_value(cbf,&polarizn_source_ratio)
            && (polarizn_source_ratio) && (*polarizn_source_ratio)){
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Polarization "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          polarizn_source_ratio));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "\n"));
            
        } else {
            /* dummy in 0.000 */
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# Polarization 0.000\n"));
            
            if (log){
                
                cbf_failnez(cbf_append_string(log,&log_capacity,&log_size,
                                              "cbf_set_minicbf_header: warning: "
                                              "dummy Polarization 0.000 used\n"));
                
        }

        
        }
        

        
        /* For each goniometer angle ANGLE
         set ANGLE 0.0000 deg. line
         set ANGLE_increment 0.0000 deg. line
         removing GONIOMETER_ prefixes */
        
        /* Set N_oscillations 1 line */
        
        if (!cbf_find_category(cbf,"diffrn_scan_frame")
            &&!cbf_find_column(cbf,"oscillations")
            &&!cbf_rewind_row(cbf)
            &&!cbf_get_value(cbf,&oscillations)
            && (oscillations) && (*oscillations)){
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# N_oscillations "));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          oscillations));
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "\n"));
        } else {
            
            cbf_failnez(cbf_append_string(&header,&header_capacity,&header_size,
                                          "# N_oscillations 1\n"));
        }

        
        /* Write the header to the CBF */
        
        
        if ((!cbf_require_category(datacbf, "array_data"))
            && !cbf_require_column(datacbf,"header_convention")
            && !cbf_rewind_row(datacbf)) {
            
            cbf_failnez(cbf_set_value(datacbf,"PILATUS_1.4"));
        }
            
        if ((!cbf_require_category(datacbf, "array_data"))
            && !cbf_require_column(datacbf,"header_contents")
            && !cbf_rewind_row(datacbf)) {
            
            cbf_failnez(cbf_set_value(datacbf,header));
            
        }
        
        cbf_failnez(cbf_free_text((const char * *)&header,NULL));
        
        return CBF_SUCCESS;
        
    }
    


#ifdef __cplusplus

}
#endif

