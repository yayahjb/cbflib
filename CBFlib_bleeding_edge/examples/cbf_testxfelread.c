/**********************************************************************
 * cbf_testxfelread.c -- example of reading XFEL file                 *
 *                                                                    *
 * cbf_testxfelread.c version 1                                       *
 * CBFlib Version 0.9.5.4 14 June 2015                                *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2015 Herbert J. Bernstein                            *
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

  /*  This test program will read the XFEL data from the test file
      reorganize it into a single pixel array
      and write out that array
   */

#ifdef __cplusplus

extern "C" {
    
#endif

#define XFEL_ELEMENTS 64
#define XFEL_FAST_DIM 194
#define XFEL_SLOW_DIM 185
#define XFEL_OVERLOAD 131072.
#define XFEL_UNDEFINED -131072.
#define XFEL_DIAMETER 1725
#define XFEL_PIXEL_SIZE 0.10992
#define XFEL_PSF_FWHM 0.030
    
    /*  XFEL_PSF_FWHM estimated by eye from From Fig 6 line spread function
     in [Hugh T. Philipp, Lucas 1. Koerner, Marianne S. Hromalik, Mark W. Tate, 
     Sol M. Gruner (2008), "Femtosecond Radiation Experiment Detector for
     X-ray Free-Electron Laser (XFEL) Coherent X-ray Imaging",  2008 Nuclear 
     Science Symposium, Medical Imaging Conference and 16th Room Temperature 
     Semiconductor Detector Workshop 19 - 25 October 2008 Dresden, Germany
     Session N14: Synchrotron Radiation and FEL Instrumentation Tuesday,
     Oct. 21 10:30-12:30 
     
     The geometry used in this code is based on 
     
     https://confluence.slac.stanford.edu/display/PSDM/CSPAD+Geometry+and+Alignment#CSPADGeometryandAlignment-2x1SensorGeometry
     
     which report a regular pixel size of 109.92 um x 109.92 um, rather
     than 110 um x 110 um.  The edge pixels are 2.5 times the width
     of the regular pixels.
     
     */
    

    
#include "cbf_testxfelread.h"
#include "cbf_string.h"
    
#ifndef CBF_ARGUMENT
#define CBF_ARGUMENT 4
#endif
    
    FILE *in, *out=NULL;
    cbf_handle cbf_in = NULL;
    cbf_handle cbf_out = NULL;
    const char * cbf_inname = "idx-s00-20131106040304531.cbf";
    const char * cbf_outname = "idx-s00-20131106040304531_flat.cbf";
    unsigned int element, elements;
    cbf_detector xfel_element[XFEL_ELEMENTS];
    double xfel_rawdata[XFEL_ELEMENTS][XFEL_SLOW_DIM][XFEL_FAST_DIM];
    double xfel_elongate[XFEL_ELEMENTS][XFEL_SLOW_DIM][2];
    double outarray[XFEL_DIAMETER][XFEL_DIAMETER];
    int intoutarray[XFEL_DIAMETER][XFEL_DIAMETER];
    char elementassign[XFEL_DIAMETER][XFEL_DIAMETER];
    double pixel_0_0[XFEL_ELEMENTS][3];
    double pixel_0_1[XFEL_ELEMENTS][3];
    double pixel_1_0[XFEL_ELEMENTS][3];
    double pixel_1_1[XFEL_ELEMENTS][3];
    double pixel_low[3];
    double pixel_high[3];
    double pixel_mapped_0_0[XFEL_ELEMENTS][3];
    double pixel_mapped_0_1[XFEL_ELEMENTS][3];
    double pixel_mapped_1_0[XFEL_ELEMENTS][3];
    double pixel_mapped_1_1[XFEL_ELEMENTS][3];
    double pixel_normal[XFEL_ELEMENTS][3];
    double pixel_normal_avg[3];
    double prj[3][3];
    double rotmat[3][3];
    int ii, jj;
    int slowactual,fastactual;
    int revnormal;
    double dlen;
    int elongate=0;


    /* Get the rotation matrix to rotate a given vector to Z
     code by Kaden Badalian based on
     https://en.wikipedia.org/wiki/Rotation_matrix#Axis_and_angle */
    
    int cbf_get_rotation_to_Z(double r[3][3], double vec[3]) {
        double len, cosTheta, sinTheta;
        double ux, uy, uz, ulen;
        
        len = sqrt(vec[0]*vec[0]+vec[1]*vec[1]+vec[2]*vec[2]);
        if (len < 1.e-38) return CBF_ARGUMENT;
        cosTheta = vec[2]/len;
        sinTheta = sqrt(1-cosTheta*cosTheta);
        ux = vec[1];
        uy = -vec[0];
        uz = 0.;
        ulen = sqrt(vec[0]*vec[0]+vec[1]*vec[1]);
        if (len >= 1.e-38) {
        ux = ux/ulen;
        uy = uy/ulen;
        } else {
            ux = uy = 0;
            cosTheta = 1.;
        }
        r[0][0] = cosTheta+ux*ux*(1-cosTheta);
        r[0][1] = ux*uy*(1-cosTheta)-uz*sinTheta;
        r[0][2] = ux*uz*(1-cosTheta)+uy*sinTheta;
        r[1][0] = ux*uy*(1-cosTheta)+uz*sinTheta;
        r[1][1] = cosTheta+uy*uy*(1-cosTheta);
        r[1][2] = uy*uz*(1-cosTheta)-ux*sinTheta;
        r[2][0] = ux*uz*(1-cosTheta)-uy*sinTheta;
        r[2][1] = uy*uz*(1-cosTheta)+ux*sinTheta;
        r[2][2] = cosTheta+uz*uz*(1-cosTheta);
        return 0;
    }
    
    static int apply_matrix(double M[3][3],double vector[3],double result[3]) {
        
        result[0] = M[0][0]*vector[0] +  M[0][1]*vector[1] + M[0][2]*vector[2];
        result[1] = M[1][0]*vector[0] +  M[1][1]*vector[1] + M[1][2]*vector[2];
        result[2] = M[2][0]*vector[0] +  M[2][1]*vector[1] + M[2][2]*vector[2];
        
        return 0;
    }
    

    int main (int argc, char *argv [])
    {
        elongate = 0;
        
        if (argc > 1) {
            if (cbf_cistrcmp(argv[1],"--elongate")==0) {
                elongate = 1;
                fprintf(stdout,"elongate the middle pixels\n");
            }
        }
        
        if ( cbf_make_handle (&cbf_in) ) {
            fprintf(stderr,"Failed to create handle for cif_in\n");
            exit(1);
        }
        
        if ( cbf_make_handle (&cbf_out) ) {
            fprintf(stderr,"Failed to create handle for cif_out\n");
            exit(1);
        }
        
        if (!(in = fopen (cbf_inname, "rb"))) {
            fprintf (stderr,"Couldn't open the input CBF file %s\n", cbf_inname);
            exit (1);
        }
        
        if (cbf_read_widefile(cbf_in, in, MSG_DIGEST)) {
            
            fprintf (stderr,"Failed to read the file\n");
            return 1;
            
        }
        
        
        if (cbf_select_datablock(cbf_in, 0)) {
            
            fprintf (stderr,"Failed to open first datablock\n");
            return 1;
            
        }
        
        if (cbf_count_elements(cbf_in, &elements)){
            
            fprintf (stderr,"Failed to count detector elements\n");
            return 1;
            
        }
        
        if (elements != XFEL_ELEMENTS) {
            
            fprintf (stderr,"Expected %d detector elements, %d found\n", XFEL_ELEMENTS, elements);
            return 1;
            
        }
        
        /* construct the detectors  and find the pixel box boundaries */
        
        for (ii=0; ii < 3; ii++){
            pixel_low[ii] = 1.e38;
            pixel_high[ii] = -1.e38;
        }
        
        for (element=0; element < elements; element++) {
            
            cbf_failnez(cbf_construct_detector(cbf_in, &(xfel_element[element]),element));
            cbf_failnez(cbf_get_pixel_coordinates_sf(xfel_element[element],
                                                     0.0,
                                                     0.0,
                                                     pixel_0_0[element],
                                                     pixel_0_0[element]+1,
                                                     pixel_0_0[element]+2));
            cbf_failnez(cbf_get_pixel_coordinates_sf(xfel_element[element],
                                                     0.0,
                                                     (double)(XFEL_FAST_DIM-1),
                                                     pixel_0_1[element],
                                                     pixel_0_1[element]+1,
                                                     pixel_0_1[element]+2));
            cbf_failnez(cbf_get_pixel_coordinates_sf(xfel_element[element],
                                                     (double)(XFEL_SLOW_DIM-1),
                                                     0.0,
                                                     pixel_1_0[element],
                                                     pixel_1_0[element]+1,
                                                     pixel_1_0[element]+2));
            cbf_failnez(cbf_get_pixel_coordinates_sf(xfel_element[element],
                                                     (double)(XFEL_SLOW_DIM-1),
                                                     (double)(XFEL_FAST_DIM-1),
                                                     pixel_1_1[element],
                                                     pixel_1_1[element]+1,
                                                     pixel_1_1[element]+2));
            cbf_failnez(cbf_get_pixel_normal(xfel_element[element],
                                             (double)(XFEL_SLOW_DIM-1)/2.,
                                             (double)(XFEL_FAST_DIM-1)/2.,
                                             pixel_normal[element],
                                             pixel_normal[element]+1,
                                             pixel_normal[element]+2));
            
        }
        
        
        /* average the normals
         Note -- this is not the nest way for large excursions
         SLERP or, better, HLERP is needed
         
         Call this normal n
         For a pixel at p=[x,y,z] the projector onto the virtual detector
         will be p-(p.n)n
         
         */
        
        pixel_normal_avg[0] = pixel_normal_avg[1] = pixel_normal_avg[2];
        
        for (element=0; element < elements; element++) {
            
            revnormal = 0;
            
            if (pixel_normal[element][2] < 0.) revnormal = 1;
            
            for (ii = 0; ii < 3; ii++){
                
                if (revnormal ) {
                    
                    pixel_normal_avg[ii] -= pixel_normal[element][ii];
                    
                } else {
                    
                    pixel_normal_avg[ii] += pixel_normal[element][ii];
                    
                }
                
            }
            
        }
        
        
        dlen = sqrt(pixel_normal_avg[0]*pixel_normal_avg[0]
                    + pixel_normal_avg[1]*pixel_normal_avg[1]
                    + pixel_normal_avg[2]*pixel_normal_avg[2]);
        
        if (dlen > 1.e-10) {
            for (ii = 0; ii < 3; ii++) {
                pixel_normal_avg[ii] /= dlen;
            }
        }
        
        
        prj[0][0] = 1. - pixel_normal_avg[0]*pixel_normal_avg[0];
        prj[0][1] =    - pixel_normal_avg[0]*pixel_normal_avg[1];
        prj[0][2] =    - pixel_normal_avg[0]*pixel_normal_avg[2];
        
        prj[1][0] =    - pixel_normal_avg[1]*pixel_normal_avg[0];
        prj[1][1] = 1. - pixel_normal_avg[1]*pixel_normal_avg[1];
        prj[1][2] =    - pixel_normal_avg[1]*pixel_normal_avg[2];
        
        prj[2][0] =    - pixel_normal_avg[2]*pixel_normal_avg[0];
        prj[2][1] =    - pixel_normal_avg[2]*pixel_normal_avg[1];
        prj[2][2] = 1. - pixel_normal_avg[2]*pixel_normal_avg[2];
        
        fprintf(stdout," prj [%g,%g,%g]\n"
                "     [%g,%g,%g]\n"
                "     [%g,%g,%g]\n",
                prj[0][0], prj[0][1], prj[0][2],
                prj[1][0], prj[1][1], prj[1][2],
                prj[2][0], prj[2][1], prj[2][2]);
        
        
        /* Compute the rotation matrix.  This rotation will either take
         the normal to Z or the negative of the normal to Z.  In either
         case it is minimal rotation to take the average detector plane
         back to the ideal detector plane (Z=0) */
        
        cbf_failnez(cbf_get_rotation_to_Z(rotmat, pixel_normal_avg));
        
        fprintf(stdout," rotmat [%g,%g,%g]\n"
                "        [%g,%g,%g]\n"
                "        [%g,%g,%g]\n",
                rotmat[0][0], rotmat[0][1], rotmat[0][2],
                rotmat[1][0], rotmat[1][1], rotmat[1][2],
                rotmat[2][0], rotmat[2][1], rotmat[2][2]);
        
        /* check the image sizes and load the images */
        
        for (element=0; element < elements; element++) {
            
            size_t slowdim, fastdim, islow, ifast;
            size_t num_undef, num_overload, num_pix;
            double pixavg, pixmax, pixmin;
            
            fprintf(stdout,"processing element %d\n",element);
            if(cbf_get_image_size(cbf_in, 0, element, &slowdim, &fastdim)){
                
                fprintf(stderr,"Failed to get image size for element %d\n",element);
                exit(1);
                
            }
            
            if (slowdim != XFEL_SLOW_DIM || fastdim != XFEL_FAST_DIM ) {
                
                fprintf (stderr,"Expected element %d to be %d x %d, found %d x %d\n",
                         element,(int)XFEL_SLOW_DIM,(int)XFEL_FAST_DIM,
                         (int)slowdim,(int)fastdim );
                exit(1);
                
            }
            
            fprintf (stdout,"Confirmed element %d size %d x %d\n",
                     element,(int)slowdim,(int)fastdim);
            
            cbf_failnez(cbf_get_real_image (cbf_in, 0, element,
                                            xfel_rawdata[element],8,
                                            slowdim, fastdim));
            
            
            if (!elongate) {
                for (islow = 0; islow < slowdim; islow++) {
                    xfel_elongate[element][islow][0]= xfel_elongate[element][islow][1] = 0.;
                }
            } else {
                double elongavg;
                double datavalue;
                elongavg = 0;
                for (islow = 0; islow < slowdim; islow++) {
                    datavalue = xfel_rawdata[element][islow][XFEL_FAST_DIM-1];
                    elongavg += datavalue;
                    if (datavalue == XFEL_UNDEFINED || datavalue == XFEL_OVERLOAD ) {
                        xfel_elongate[element][islow][0] = xfel_elongate[element][islow][1] = datavalue;
                    } else {
                        xfel_elongate[element][islow][1] = datavalue/5.;
                        xfel_elongate[element][islow][0] = 2.*datavalue/5.;
                    }

                    if ((element%2) == 0) {
                        xfel_rawdata[element][islow][XFEL_FAST_DIM-1] = 2.*datavalue/5.;
                    } else {
                        xfel_rawdata[element][islow][0] = 2.*datavalue/5.;
                    }
                }
                elongavg /= 2.5*((double)slowdim);
                fprintf(stdout," element %d elongated pixel average %g\n", element, elongavg);
            }
            
            
            pixavg = 0.;
            pixmax = -1.e-38;
            pixmin = 1.e38;
            num_undef = num_overload = num_pix = 0;
            for (islow = 0; islow < slowdim; islow++) {
                for (ifast = 0; ifast < fastdim; ifast++) {
                    if (xfel_rawdata[element][islow][ifast] == XFEL_UNDEFINED) {
                        num_undef++;
                    } else if (xfel_rawdata[element][islow][ifast] == XFEL_OVERLOAD ) {
                        num_overload++;
                    } else {
                        num_pix++;
                        pixavg += xfel_rawdata[element][islow][ifast];
                        if (xfel_rawdata[element][islow][ifast] > pixmax)
                            pixmax = xfel_rawdata[element][islow][ifast];
                        if (xfel_rawdata[element][islow][ifast] < pixmin)
                            pixmin = xfel_rawdata[element][islow][ifast];
                    }
                }
            }
            
            if (num_pix > 0)pixavg /= (double)(num_pix);
            fprintf(stdout,"element %d, %d good pixels, %d undefined, %d overload\n",
                    element,(int)num_pix,(int)num_undef,(int)num_overload);
            fprintf(stdout,"            average %g, min %g, max %g\n",
                    pixavg, pixmin, pixmax);
            
            
            apply_matrix(rotmat,pixel_0_0[element],pixel_mapped_0_0[element]);
            apply_matrix(rotmat,pixel_0_1[element],pixel_mapped_0_1[element]);
            apply_matrix(rotmat,pixel_1_0[element],pixel_mapped_1_0[element]);
            apply_matrix(rotmat,pixel_1_1[element],pixel_mapped_1_1[element]);
            fprintf(stdout,"mapped corners of element %d [0,0] [%g,%g,%g], [0,1]  [%g,%g,%g]\n"
                    " [1,0] [%g,%g,%g], [1,1]  [%g,%g,%g]\n",
                    element,
                    pixel_mapped_0_0[element][0],pixel_mapped_0_0[element][1],pixel_mapped_0_0[element][2],
                    pixel_mapped_0_1[element][0],pixel_mapped_0_1[element][1],pixel_mapped_0_1[element][2],
                    pixel_mapped_1_0[element][0],pixel_mapped_1_0[element][1],pixel_mapped_1_1[element][2],
                    pixel_mapped_1_1[element][0],pixel_mapped_1_1[element][1],pixel_mapped_1_1[element][2]);
            for (ii = 0; ii < 3; ii++) {
                if (pixel_mapped_0_0[element][ii] < pixel_low[ii]) pixel_low[ii] = pixel_mapped_0_0[element][ii];
                if (pixel_mapped_0_1[element][ii] < pixel_low[ii]) pixel_low[ii] = pixel_mapped_0_1[element][ii];
                if (pixel_mapped_1_0[element][ii] < pixel_low[ii]) pixel_low[ii] = pixel_mapped_1_0[element][ii];
                if (pixel_mapped_1_1[element][ii] < pixel_low[ii]) pixel_low[ii] = pixel_mapped_1_1[element][ii];
                if (pixel_mapped_0_0[element][ii] > pixel_high[ii]) pixel_high[ii] = pixel_mapped_0_0[element][ii];
                if (pixel_mapped_0_1[element][ii] > pixel_high[ii]) pixel_high[ii] = pixel_mapped_0_1[element][ii];
                if (pixel_mapped_1_0[element][ii] > pixel_high[ii]) pixel_high[ii] = pixel_mapped_1_0[element][ii];
                if (pixel_mapped_1_1[element][ii] > pixel_high[ii]) pixel_high[ii] = pixel_mapped_1_1[element][ii];
            }
            
            fprintf(stdout, " pixel normal [%g,%g,%g]\n",
                    pixel_normal[element][0],pixel_normal[element][1],pixel_normal[element][2]);
            
        }
        
        fprintf(stdout, " pixel_normal_avg [%g,%g,%g]\n",
                pixel_normal_avg[0],pixel_normal[element][1],pixel_normal[element][2]);
        
        fprintf(stdout, " prj [%g,%g,%g]/[%g,%g,%g]/[%g,%g,%g] \n",
                prj[0][0],prj[0][1],prj[0][2],prj[1][0],prj[1][1],prj[1][2],prj[2][0],prj[2][1],prj[2][2]);
        
        fprintf (stdout," pixel position ranges: [%g %g], [%g %g], [%g %g]\n",
                 pixel_low[0],pixel_high[0],pixel_low[1],pixel_high[1],pixel_low[2],pixel_high[2]);
        
        
        slowactual = (pixel_high[1]-pixel_low[1]+XFEL_PIXEL_SIZE*1.5)/XFEL_PIXEL_SIZE;
        fastactual = (pixel_high[0]-pixel_low[0]+XFEL_PIXEL_SIZE*1.5)/XFEL_PIXEL_SIZE;
        
        if (fastactual > XFEL_DIAMETER || slowactual > XFEL_DIAMETER) {
            
            fprintf (stderr, "Increase XFEL_DIAMETER to %d\n", fastactual>slowactual?fastactual:slowactual);
            
        }
        
        fprintf(stdout," Detector is %d x %d ", fastactual, slowactual);
        
        /* Set the entire outarray to XFEL_UNDEFINED */
        
        for (ii=0; ii < XFEL_DIAMETER; ii++) {
            for (jj=0; jj < XFEL_DIAMETER; jj++) {
                outarray[ii][jj] = XFEL_UNDEFINED;
                intoutarray[ii][jj] = -3;
            }
        }
        
        /* Map the elements to the outarray */
        
        for (element=0; element < elements; element++) {
            double pixel_fast,pixel_slow;
            double xalpha, yalpha;
            double bottom_edge_x, bottom_edge_y;
            double top_edge_x, top_edge_y;
            double xcen, ycen;
            double xlo, ylo, xhi, yhi;
            int xpos, ypos;
            
            
            for (ii = 0; ii < XFEL_FAST_DIM+(elongate?2:0); ii++) {
                /* xalpha runs from 0. to 1. for the main array
                   but is adjusted for the elongated pixel*/
                xalpha = ((double)ii)/((double)(XFEL_FAST_DIM-1));
                if (ii == XFEL_FAST_DIM) {
                    if ((element%2) == 0) {
                        xalpha = ((double)XFEL_FAST_DIM)/((double)(XFEL_FAST_DIM-1));
                    } else {
                        xalpha = (-1.)/((double)(XFEL_FAST_DIM-1));
                    }
                } else if (ii == XFEL_FAST_DIM+1) {
                    if ((element%2) == 0) {
                        xalpha = ((double)XFEL_FAST_DIM+.5)/((double)(XFEL_FAST_DIM-1));
                    } else {
                        xalpha = (-1.5)/((double)(XFEL_FAST_DIM-1));
                    }
                }

                for (jj = 0; jj < XFEL_SLOW_DIM; jj++) {
                    double partial_pixel[3][3];
                    /* yalpha runs from 0. to 1.*/
                    yalpha = ((double)jj)/((double)(XFEL_SLOW_DIM-1));
                    bottom_edge_x =
                        (1.-xalpha)*pixel_mapped_0_0[element][0]
                        + xalpha*pixel_mapped_0_1[element][0];
                    bottom_edge_y =
                        (1.-xalpha)*pixel_mapped_0_0[element][1]
                        + xalpha*pixel_mapped_0_1[element][1];
                    top_edge_x =
                        (1.-xalpha)*pixel_mapped_1_0[element][0]
                        + xalpha*pixel_mapped_1_1[element][0];
                    top_edge_y =
                        (1.-xalpha)*pixel_mapped_1_0[element][1]
                        + xalpha*pixel_mapped_1_1[element][1];


                    pixel_fast =
                        (1.-yalpha)*bottom_edge_x +
                        yalpha*top_edge_x;
                    pixel_slow =
                        (1.-yalpha)*bottom_edge_y +
                        yalpha*top_edge_y;
                    
                    xpos = (pixel_fast-pixel_low[0]+.5)/XFEL_PIXEL_SIZE;
                    ypos = (pixel_slow-pixel_low[1]+.5)/XFEL_PIXEL_SIZE;
                    
                    if ((ii==0 || ii==XFEL_FAST_DIM-1)&&(jj==0 || jj ==XFEL_SLOW_DIM-1)){
                        fprintf(stdout,"processing element %d, pixel [%d,%d]\n",element,ii,jj);
                        fprintf(stdout,"xalpha: %g, yalpha: %g, pixel_fast: %g, pixel_slow: %g\n",
                                xalpha, yalpha, pixel_fast, pixel_slow);
                        fprintf(stdout,"xpos: %d, ypos: %d, xcen: %g, ycen %g\n", xpos,ypos,xcen,ycen);
                    }
                    
                    if ((xpos < XFEL_DIAMETER && ypos < XFEL_DIAMETER)
                        && xpos >= 0 && ypos >= 0){
                        double datavalue;
                        if (ii < XFEL_FAST_DIM) {
                            datavalue = xfel_rawdata[element][jj][ii];
                        } else {
                            datavalue = xfel_elongate[element][jj][ii-XFEL_FAST_DIM];
                        }
                        xlo = xpos*XFEL_PIXEL_SIZE+pixel_low[0];
                        ylo = ypos*XFEL_PIXEL_SIZE+pixel_low[1];
                        xcen = xlo+XFEL_PIXEL_SIZE/2.;
                        ycen = ylo+XFEL_PIXEL_SIZE/2.;
                        xcen = pixel_fast;
                        ycen = pixel_slow;
                        xhi = xlo+XFEL_PIXEL_SIZE;
                        yhi = ylo+XFEL_PIXEL_SIZE;
                        if (datavalue== XFEL_UNDEFINED
                            || datavalue == XFEL_OVERLOAD) {
                            outarray[ypos][xpos] = datavalue;
                        } else {
                            cbf_airy_disk_volume(xlo-XFEL_PIXEL_SIZE, ylo-XFEL_PIXEL_SIZE,
                                                 xhi-XFEL_PIXEL_SIZE, yhi-XFEL_PIXEL_SIZE,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[0][0]));
                            cbf_airy_disk_volume(xlo, ylo-XFEL_PIXEL_SIZE,
                                                 xhi, yhi-XFEL_PIXEL_SIZE,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[1][0]));
                            cbf_airy_disk_volume(xlo+XFEL_PIXEL_SIZE, ylo-XFEL_PIXEL_SIZE,
                                                 xhi+XFEL_PIXEL_SIZE, yhi-XFEL_PIXEL_SIZE,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[2][0]));
                            cbf_airy_disk_volume(xlo-XFEL_PIXEL_SIZE, ylo,
                                                 xhi-XFEL_PIXEL_SIZE, yhi,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[0][1]));
                            cbf_airy_disk_volume(xlo+XFEL_PIXEL_SIZE, ylo,
                                                 xhi+XFEL_PIXEL_SIZE, yhi,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[2][1]));
                            cbf_airy_disk_volume(xlo-XFEL_PIXEL_SIZE, ylo+XFEL_PIXEL_SIZE,
                                                 xhi-XFEL_PIXEL_SIZE, yhi+XFEL_PIXEL_SIZE,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[0][2]));
                            cbf_airy_disk_volume(xlo, ylo+XFEL_PIXEL_SIZE,
                                                 xhi, yhi+XFEL_PIXEL_SIZE,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[1][2]));
                            cbf_airy_disk_volume(xlo+XFEL_PIXEL_SIZE, ylo+XFEL_PIXEL_SIZE,
                                                 xhi+XFEL_PIXEL_SIZE, yhi+XFEL_PIXEL_SIZE,
                                                 xcen, ycen,
                                                 datavalue,
                                                 XFEL_PSF_FWHM,
                                                 &(partial_pixel[2][2]));
                            partial_pixel[1][1] = datavalue
                            - partial_pixel[0][0] - partial_pixel[0][1] - partial_pixel[0][2]
                            - partial_pixel[1][0]                       - partial_pixel[1][2]
                            - partial_pixel[2][0] - partial_pixel[2][1] - partial_pixel[2][2];
                            if (partial_pixel[1][1] < 0.) partial_pixel[1][1] = 0;
                            elementassign[ypos][xpos] = element;
                            if (outarray[ypos][xpos] == XFEL_UNDEFINED) {
                                outarray[ypos][xpos] = partial_pixel[1][1];
                            } else {
                                outarray[ypos][xpos] += partial_pixel[1][1];
                            }
                            if (ypos > 0) {
                                if (outarray[ypos-1][xpos] == XFEL_UNDEFINED) {
                                    outarray[ypos-1][xpos] = partial_pixel[1][0];
                                } else {
                                    outarray[ypos-1][xpos] += partial_pixel[1][0];
                                }
                            }
                            if (ypos < XFEL_DIAMETER-1) {
                                if (outarray[ypos+1][xpos] == XFEL_UNDEFINED) {
                                    outarray[ypos+1][xpos] = partial_pixel[1][2];
                                } else {
                                    outarray[ypos+1][xpos] += partial_pixel[1][2];
                                }
                            }
                            if (xpos > 0) {
                                if (outarray[ypos][xpos-1] == XFEL_UNDEFINED) {
                                    outarray[ypos][xpos-1] = partial_pixel[0][1];
                                } else {
                                    outarray[ypos][xpos-1] += partial_pixel[0][1];
                                }
                                if (ypos > 0) {
                                    if (outarray[ypos-1][xpos-1] == XFEL_UNDEFINED) {
                                        outarray[ypos-1][xpos-1] = partial_pixel[0][0];
                                    } else {
                                        outarray[ypos-1][xpos-1] += partial_pixel[0][0];
                                    }
                                }
                                if (ypos < XFEL_DIAMETER-1) {
                                    if (outarray[ypos+1][xpos-1] == XFEL_UNDEFINED) {
                                        outarray[ypos+1][xpos-1] = partial_pixel[0][2];
                                    } else {
                                        outarray[ypos+1][xpos-1] += partial_pixel[0][2];
                                    }
                                }
                            }
                            if (xpos < XFEL_DIAMETER-1) {
                                if (outarray[ypos][xpos+1] == XFEL_UNDEFINED) {
                                    outarray[ypos][xpos+1] = partial_pixel[2][1];
                                } else {
                                    outarray[ypos][xpos+1] += partial_pixel[2][1];
                                }
                                if (ypos > 0) {
                                    if (outarray[ypos-1][xpos+1] == XFEL_UNDEFINED) {
                                        outarray[ypos-1][xpos+1] = partial_pixel[2][0];
                                    } else {
                                        outarray[ypos-1][xpos+1] += partial_pixel[2][0];
                                    }
                                }
                                if (ypos < XFEL_DIAMETER-1) {
                                    if (outarray[ypos+1][xpos+1] == XFEL_UNDEFINED) {
                                        outarray[ypos+1][xpos+1] = partial_pixel[2][2];
                                    } else {
                                        outarray[ypos+1][xpos+1] += partial_pixel[2][2];
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        for (ii=0; ii < XFEL_DIAMETER; ii++) {
            for (jj=0; jj < XFEL_DIAMETER; jj++) {
                if (outarray[ii][jj] != XFEL_UNDEFINED
                    && outarray[ii][jj] != XFEL_OVERLOAD);
                intoutarray[ii][jj] = (int)(outarray[ii][jj]+0.5);
            }
        }

        /* write just the data to the out cif.  For a real case, we should also write
         out the metadata revised for the single virtual image and provide an option
         to do it as integer data */
        
        cbf_failnez(cbf_force_new_datablock(cbf_out,"virtual_cspad"));
        cbf_failnez(cbf_require_category(cbf_out,"array_data"));
        cbf_failnez(cbf_require_column(cbf_out,"array_id"));
        cbf_failnez(cbf_set_value(cbf_out,"virtual_cspad"));
        cbf_failnez(cbf_require_column(cbf_out,"binary_id"));
        cbf_failnez(cbf_set_value(cbf_out,"1"));
        cbf_failnez(cbf_require_column(cbf_out,"data"));
        /* cbf_failnez(cbf_set_realarray_wdims_fs (cbf_out, CBF_PACKED, 1,
         (void *)outarray, sizeof(double),
         XFEL_DIAMETER*XFEL_DIAMETER,
         "little_endian",
         XFEL_DIAMETER, XFEL_DIAMETER,
         1, 0)); */
        cbf_failnez(cbf_set_integerarray_wdims_fs (cbf_out, CBF_BYTE_OFFSET, 1,
                                                   (void *)intoutarray, sizeof(int),1,
                                                   XFEL_DIAMETER*XFEL_DIAMETER,
                                                   "little_endian",
                                                   XFEL_DIAMETER, XFEL_DIAMETER,
                                                   1, 0));
        /* cbf_failnez(cbf_set_integerarray_wdims_fs (cbf_out, CBF_NONE, 1,
         (void *)elementassign, sizeof(char),1,
         XFEL_DIAMETER*XFEL_DIAMETER,
         "little_endian",
         XFEL_DIAMETER, XFEL_DIAMETER,
         1, 0));*/
        
        out = fopen (cbf_outname, "w+b");
        cbf_failnez(cbf_write_widefile (cbf_out, out, 1, CBF,
                                        MIME_HEADERS | MSG_DIGEST | PAD_4K | 0,
                            ENC_NONE | ENC_CRTERM | ENC_LFTERM ));
        
    }
    
#ifdef __cplusplus
    
}

#endif

