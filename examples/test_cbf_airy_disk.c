/**********************************************************************
 * test_cbf_airy_disk.c -- tests for airy-disk point spread           *
 *                                                                    *
 * test_cbf_airy_disk.c version 1                                     *
 *                                                                    *
 * Kaden Badalian and Herbert J. Bernstein                            *
 * CBFlib Version 0.9.5.4 23 June 2015                                *
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

/*  Correct execution should produce no output */

#ifdef __cplusplus

extern "C" {
    
#endif
    
#include "cbf_airy_disk.h"
    
    int main() {
        double xhi,xlo,yhi,ylo, xx, yy;
        double g[41][41];
        double partvol[40][40];
        double partvol2[40][40];
        double test_slices_1D[20];
        double test_partvol_1D[20];
        double test_squares_2D[20][20];
        double test_partvol_2D[20][20];
        int ia, ib;
        int error=0;
        
        
        /* generate the function table */
        
        for (ia = 0; ia < 41; ia++) {
            xx = (double)(ia-20)/20.;
            for (ib = 0; ib < 41; ib++) {
                yy = (double)(ib-20)/20.;
                cbf_airy_unit_disk(xx, yy, &(g[ia][ib]));
                if (g[ia][ib] < 0. || g[ia][ib] > 1.395330318373548+1.e-20) {
                    fprintf(stdout,"Airy function error at ia %d ib %d value %g\n",
                            ia, ib, g[ia][ib]);
                }
            }
        }
        
        /* test the simpson's rule code */
        
        for (ia = 0; ia < 20; ia++) {
            xlo = -1. + ((double)ia)*.1;
            xhi = xlo + 0.1;
            error|= cbf_airy_unit_disk_volume(xlo,-1.,xhi,1.,333,&(test_slices_1D[ia]));
            test_partvol_1D[ia]=test_slices_1D[ia];
            if (ia > 0) test_partvol_1D[ia] += test_partvol_1D[ia-1];
            if (error ||
                fabs(test_partvol_1D[ia] - cbf_airy_unit_disk_partvol_1D[ia]) > 1.e-8) {
                fprintf (stderr,"test_partvol_1D error %d %g %g\n",
                         ia,test_partvol_1D[ia],
                         cbf_airy_unit_disk_partvol_1D[ia]);
            }

            for (ib = 0; ib < 20; ib++) {
                ylo = ((double)ib)*0.05;
                yhi = ylo+0.05;
                error|= cbf_airy_unit_disk_volume((xlo+1.)/2.,ylo,(xlo+1.)/2.+0.05,yhi,40,&(test_squares_2D[ia][ib]));
                test_partvol_2D[ia][ib] = test_squares_2D[ia][ib];
                if (ia == 0) {
                    if (ib > 0) {
                        test_partvol_2D[ia][ib] += test_partvol_2D[ia][ib-1];
                    }
                } else {
                    if (ib > 0) {
                        test_partvol_2D[ia][ib] += (test_partvol_2D[ia][ib-1]
                                        + test_partvol_2D[ia-1][ib]
                                        - test_partvol_2D[ia-1][ib-1]);
                    } else {
                        test_partvol_2D[ia][ib] += test_partvol_2D[ia-1][ib];
                    }
                                        
                }
                if (error || fabs(test_partvol_2D[ia][ib]
                                  -cbf_airy_unit_disk_partvol_2D[ia][ib])> 5.e-8) {
                    fprintf (stderr,"test_partvol_2D error %d %d %g %g\n",
                             ia,ib,test_partvol_2D[ia][ib],
                             cbf_airy_unit_disk_partvol_2D[ia][ib]);
                    
                }
            }
            
        }
        

        for (ia = 0; ia < 20; ia++) {
            xlo = 0.+.05*(double)ia;
            xhi = xlo+.05;
            for (ib = 0; ib < 20; ib++) {
                ylo = 0.+.05*(double)ib;
                yhi = ylo+.05;
                error |= cbf_airy_unit_disk_volume(xlo,ylo,xhi,yhi,20,&(partvol[ia][ib]));
                if (fabs(partvol[ia][ib]-test_squares_2D[ia][ib]) > 1.e-8) {
                    fprintf(stderr, "error in cbf_airy_unit_disk_volume %d %d %g %g\n",
                            ia, ib, partvol[ia][ib], test_squares_2D[ia][ib]);
                    
                }
            }
        }
        
        for (ia = 0; ia < 20; ia++) {
            xlo = 0.+.05*(double)ia;
            xhi = xlo+.05;
            for (ib = 0; ib < 20; ib++) {
                ylo = 0.+.05*(double)ib;
                yhi = ylo+.05;
                error |= cbf_airy_disk_volume(xlo*10./3.+5.7,ylo*10./3.+2.3,xhi*10./3.+5.7,yhi*10./3.+2.3,5.7,2.3,255.,CBF_AIRY_UNIT_DISK_FWHM*10./3.,&(partvol2[ia][ib]));
                if (fabs(partvol[ia][ib]-partvol2[ia][ib]/255.) > 1.e-7) {
                    fprintf(stdout,"%d %d partvol %15.9g partvol2 %15.9g \n",ia, ib,partvol[ia][ib],partvol2[ia][ib]/255.);
                }
            }
        }
        
        return 0;
    }
    
    
#ifdef __cplusplus
    
}

#endif

