/**********************************************************************
 * convert_mincbf -- convert a minimal cbf to a full cbf file         *
 *                                                                    *
 * Version 0.7.7 2 April 2007                                         *
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
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
 *                                                                    *                                                                    *
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


/**********************************************************************
 *                                                                    *
 *                    Stanford University Notices                     *
 *  for the CBFlib software package that incorporates SLAC software   *
 *                 on which copyright is disclaimed                   *
 *                                                                    *
 * This software                                                      *
 * -------------                                                      *
 * The term 'this software', as used in these Notices, refers to      *
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
 *                                 NOTICE                             *
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
 *                            SYNOPSIS                                *
 *                                                                    *
 *  convert_minicbf [-i input_cbf] [-o output_cbf] [-p template_cbf]\ *
 *    [-d detector name]  -m [x|y|x=y] [-z distance]              \   *
 *    [-c category_alias=category_root]*                          \   *
 *    [-t tag_alias=tag_root]* [-F] [-R]                          \   *
 *    [input_cbf] [output_cbf]                                        *
 *                                                                    *
 *  the options are:                                                  *
 *                                                                    *
 *  -i input_cbf (default: stdin)                                     *
 *    the input file as a CBF with at least an image.                 *
 *                                                                    *
 *  -p template_cbf                                                   *
 *    the template for the final cbf to be produced.  If template_cbf *
 *    is not specified the name is constructed from the first token   *
 *    of the detector name and the image size as                      *
 *       template_<type>_<columns>x<rows>.cbf                         *
 *                                                                    *
 *  -o output_cbf (default: stdout )                                  *
 *    the output cbf combining the image and the template.  If the    *
 *    output_cbf is not specified or is given as "-", it is written   *
 *    to stdout.                                                      *
 *                                                                    *
 *  -d detectorname                                                   *
 *    a detector name to be used if none is provided in the image     *
 *    header.                                                         *
 *                                                                    *
 *  -F                                                                *
 *    when writing packed compression, treat the entire image as      *
 *    one line with no averaging                                      *
 *                                                                    *
 *  -m [x|y|x=y] (default x=y, square arrays only)                    *
 *    mirror the array in the x-axis (y -> -y)                        *
 *                     in the y-axis (x -> -x)                        *
 *                  or in x=y ( x -> y, y-> x)                        *
 *                                                                    *
 *  -r n                                                              *
 *    rotate the array n times 90 degrees counter clockwise           *
 *    x -> y, y -> -x for each rotation, n = 1, 2 or 3                *
 *                                                                    *
 *  -R                                                                *
 *    if setting a beam center, set reference values of               *
 *    axis settings as well as standard settings                      *
 *                                                                    *
 *  -z distance                                                       *
 *    detector distance along Z-axis                                  *
 *                                                                    *
 *  -c category_alias=category_root                                   *
 *  -t tag_alias=tagroot                                              *
 *    map the given alias to the given root, so that instead          *
 *    of outputting the alias, the root will be presented in the      *
 *    output cbf instead.  These options may be repeated as many      *
 *    times as needed.                                                *
 *                                                                    *
 **********************************************************************/


#include "cbf.h"
#include "cbf_simple.h"
#include "cbf_string.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#ifdef GNUGETOPT
#include "getopt.h"
#endif
#include <unistd.h>

#define CVTBUFSIZ 8192


  /* parse a string from an sls comment style header and
     add it to cbf                                      */
     

int cbf_parse_sls_header(cbf_handle cbf, char * slsstr) {

    double pscalex=1., pscaley=1.;

    double bcx, bcy, bcscalex, bcscaley;
    
    double tempdouble;
    
    char * tempendptr;
    
    static const char *monthname [] =

        { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
     
#define cbf_set_value_from_string(cbf,cbfstring,cbfcat,cbfcol)   \
                                                                 \
    cbf_failnez(cbf_require_category((cbf),(cbfcat)))            \
                                                                 \
    while (*(cbfstring) && isspace(*(cbfstring))) (cbfstring)++; \
                                                                 \
    cbf_failnez(cbf_require_column((cbf),(cbfcol)))              \
                                                                 \
    cbf_failnez(cbf_set_value((cbf),(cbfstring)))

#define cbf_set_doublevalue_from_string(cbf,cbfstring,cbfcat,cbfcol,unit)   \
                                                                 \
    cbf_failnez(cbf_require_category((cbf),(cbfcat)))            \
                                                                 \
    while (*(cbfstring) && isspace(*(cbfstring))) (cbfstring)++; \
                                                                 \
    tempdouble = strtod(cbfstring,&tempendptr);                  \
                                                                 \
    cbfstring = tempendptr;                                      \
                                                                 \
    while (*(cbfstring) && isspace(*(cbfstring))) (cbfstring)++; \
                                                                 \
    if (cbf_cistrcmp(unit,cbfstring)) {                          \
                                                                 \
    	if (*cbfstring=='m' && cbf_cistrcmp(unit,cbfstring+1) ) tempdouble *= 1.e-3; \
                                                                 \
        if (*cbfstring=='u' && cbf_cistrcmp(unit,cbfstring+1) ) tempdouble *= 1.e-6; \
                                                                 \
    }                                                            \
                                                                 \
    cbf_failnez(cbf_require_column((cbf),(cbfcol)))              \
                                                                 \
    cbf_failnez(cbf_set_doublevalue((cbf),"%-15g",tempdouble)) 
    
  if (strlen(slsstr) < 2 
    || slsstr[0] != '#' 
    || !isspace(slsstr[1])) return CBF_FORMAT;
    
  slsstr += 2;
  
  while (*slsstr && isspace(*slsstr)) slsstr++;
  
  if (!cbf_cistrncmp(slsstr,"Detector: ",strlen("Detector: "))) {
  
    slsstr += strlen("Detector: ");
  
    cbf_set_value_from_string(cbf,slsstr,"diffrn_detector","type")
    	
  } else if (!cbf_cistrncmp(slsstr,"Exposure time: ",strlen("Exposure time: "))) {
  
    slsstr += strlen("Exposure time: ");

    cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_scan_frame","integration_time","s")

  } else if (!cbf_cistrncmp(slsstr,"Pixel_size ",strlen("Pixel_size "))) {
  
     char *endptr;
     
     double psx, psy;
          
     slsstr += strlen("Pixel_size ");
     
     pscalex = pscaley = 1.;
    
     psy = psx = strtod (slsstr, &endptr);
     
     if (endptr && *endptr) {
     
       slsstr = endptr;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       if (!cbf_cistrncmp("m ",slsstr,2))  slsstr += 2;
       
       else if (!cbf_cistrncmp("mm ",slsstr,2)) {
       
         slsstr +=3;
         
         pscaley = pscalex = 1.e-3;
       	
       } else  {
       
         pscaley = pscalex = 1.e-6;
       	
       }
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       if (*slsstr) {
       
         psy = strtod (slsstr, &endptr);

         while (*slsstr && isspace(*slsstr)) slsstr++;

         if (!cbf_cistrncmp("m ",slsstr,2))  slsstr += 2;
       
         else if (!cbf_cistrncmp("mm ",slsstr,2)) {
       
           slsstr +=3;
         
           pscaley = 1.e-3;
       	
         } else  {
       
           pscaley = 1.e-6;
           
         }
       	
       }
            	
     }
     
     psx *= pscalex;
     
     psy *= pscaley;
     
     cbf_failnez(cbf_require_category(cbf,"array_element_size"))
     
     cbf_failnez(cbf_require_column(cbf,"index"))
     
     cbf_failnez(cbf_set_integervalue(cbf,1))
     
     cbf_failnez(cbf_require_column(cbf,"size"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",psx))
     
     cbf_failnez(cbf_next_row(cbf))
     
     cbf_failnez(cbf_require_column(cbf,"index"))
     
     cbf_failnez(cbf_set_integervalue(cbf,2))
     
     cbf_failnez(cbf_require_column(cbf,"size"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",psy))
     
  }  else if (!cbf_cistrncmp(slsstr,"Count_cutoff ",
       strlen("Count_cutoff "))) {
  
    cbf_set_value_from_string(cbf,slsstr,"array_intensities","overload")

  }  else if (!cbf_cistrncmp(slsstr,"wavelength ",strlen("wavelength "))) {
  
    slsstr += strlen("wavelength ");
  
    cbf_set_value_from_string(cbf,slsstr,
      "diffrn_radiation_wavelength","wavelength")

  }  else if (!cbf_cistrncmp(slsstr,"Detector_distance ",
      strlen("Detector_distance "))) {
 
     slsstr += strlen("Detector_distance ");
   
     cbf_set_value_from_string(cbf,slsstr,"diffrn_measurement",
      "distance")

  }  else if (!cbf_cistrncmp(slsstr,"beam_xy ",
      strlen("beam_xy "))) {
  
     char *endptr;
     
     slsstr += strlen("beam_xy ");
     
     bcscalex = bcscaley = 1.;
    
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (*slsstr == '(' || *slsstr == '[' || *slsstr == '{') slsstr++;

     while (*slsstr && isspace(*slsstr)) slsstr++;     
     
     bcy = bcx = strtod (slsstr, &endptr);
     
     if (endptr && *endptr) {
     
       slsstr = endptr;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
              
       if (!cbf_cistrncmp("m ",slsstr,2))  slsstr += 2;
       
       else if (!cbf_cistrncmp("mm ",slsstr,2)) {
       
         slsstr +=3;
         
         bcscaley = bcscalex = 1.e-3;
       	
       } else  {
       
         bcscaley = bcscalex = 1.e-6;
       	
       }
       
       while (*slsstr && isspace(*slsstr)) slsstr++;

       if (*slsstr == ',' ) slsstr++;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       if (*slsstr) {
       
         bcy = strtod (slsstr, &endptr);

         while (*slsstr && isspace(*slsstr)) slsstr++;

         if (!cbf_cistrncmp("m ",slsstr,2))  slsstr += 2;
       
         else if (!cbf_cistrncmp("mm ",slsstr,2)) {
       
           slsstr +=3;
         
           bcscaley = 1.e-3;
       	
         } else  {
       
           bcscaley = 1.e-6;
           
         }
       	
       }
            	
     }

     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (*slsstr == ')' || *slsstr == ']' || *slsstr == '}') slsstr++;
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (!cbf_cistrncmp("pixel",slsstr,5)) {
     
       bcscalex = pscalex;
       
       bcscaley = pscaley;
     	
     }
     
     bcx *= bcscalex;
     
     bcy *= bcscaley;
     
     cbf_failnez(cbf_require_category(cbf,"diffrn_detector_element"))
          
     cbf_failnez(cbf_require_column(cbf,"reference_center_fast"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",bcx*1.e-6))
          
     cbf_failnez(cbf_require_column(cbf,"reference_center_slow"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",bcy*1.e-6))
     
   } else if (!cbf_cistrncmp(slsstr,"Start_angle ",
       strlen("Start_angle "))) {
       
     double phi;
     
     char *endptr;
      
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_PHI"))
     
     cbf_failnez(cbf_require_column(cbf,"angle_start"))
     
     slsstr += strlen("Start_angle ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     phi = strtod(slsstr,&endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",phi))
      
   } else if (!cbf_cistrncmp(slsstr,"Angle_increment ",
       strlen("Angle_increment "))) {
       
     double phi;

     char * endptr;
           
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_PHI"))
     
     cbf_failnez(cbf_require_column(cbf,"angle_increment"))
     
     slsstr += strlen("Angle_increment ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     phi = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",phi))

   } else if (!cbf_cistrncmp(slsstr,"Detector_2theta ",
       strlen("Detector_2theta "))) {
       
     double twotheta;
     
     char *endptr;
      
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"DETECTOR_TWOTHETA"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     slsstr += strlen("Detector_2theta ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     twotheta = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",twotheta))
     
   } else if (!cbf_cistrncmp(slsstr,"Kappa ",
       strlen("Kappa "))) {
       
     double kappa;
     
     char *endptr;
      
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_KAPPA"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     slsstr += strlen("Kappa ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     kappa = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",kappa))

   } else if (!cbf_cistrncmp(slsstr,"Chi ",
       strlen("Chi "))) {
       
     double chi;
     
     char *endptr;
      
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_CHI"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     slsstr += strlen("Chi ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     chi = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",chi))
     
   } else if (strlen(slsstr)>=11 
     && (slsstr[4]=='/' || slsstr[4]=='-' ) 
     && (slsstr[8]=='/' || slsstr[8]=='-') ){
     
     char *endptr;
     
     int errflg;
     
     int notime;
     
     int yyyy,mm,dd,hr,mn;
     
     double ss;
     
     errflg = 0;
     
     yyyy = strtol(slsstr,&endptr,10);
     
     if (*endptr == '/' || *endptr == '-') {
     
       slsstr = endptr+1;
       
       for (mm = 1; mm < 13; mm++) {
       
         if (!cbf_cistrncmp(monthname[mm-1],slsstr,3)) break;

       }
       
       if (mm > 12) errflg++;
       
       else {
       
         slsstr+=4;
         
         dd = strtol(slsstr,&endptr,10);
         
         hr = mn = ss =  0; notime=1;
         
         if (*endptr && (isspace(*endptr)||*endptr=='T'||*endptr=='t'||*endptr==':')) {
         
           slsstr = endptr+1;
           
           hr = strtol(slsstr,&endptr,10);
           
           if (*endptr == ':') {
           
             notime = 0;
           
             slsstr = endptr+1;
           
             mn = strtol(slsstr,&endptr,10);
             
             if (*endptr==':') {
             
               slsstr = endptr+1;
               
               ss = strtod(slsstr,&endptr);
               
             }

           }
                    
         }
       
       }
       
     
     } else errflg++;
     
     if (!errflg) {
     
       cbf_failnez (cbf_set_datestamp (cbf, 0, yyyy, mm, dd,
                                        hr, mn, ss,
                                       CBF_NOTIMEZONE,.001))
     } else {
     
       return CBF_FORMAT;
     
     }
   }
   
   /* Still to be processed:
   
    # Flux 0 ph/s
    # Filter_transmission 1.0000
    # N_oscillations 1
    # Threshold_setting 6.0 keV
    
    */

   
   return 0;
}


double rint(double);
int local_exit (int status);
int outerror(int err);

int outerror(int err) 
{
	
  if ((err&CBF_FORMAT)==CBF_FORMAT)
    fprintf(stderr, " convert_minicbf: The file format is invalid.\n");
  if ((err&CBF_ALLOC)==CBF_ALLOC)
    fprintf(stderr, " convert_minicbf Memory allocation failed.\n");
  if ((err&CBF_ARGUMENT)==CBF_ARGUMENT)
    fprintf(stderr, " convert_minicbf: Invalid function argument.\n");
  if ((err&CBF_ASCII)==CBF_ASCII)
    fprintf(stderr, " convert_minicbf: The value is ASCII (not binary).\n");
  if ((err&CBF_BINARY)==CBF_BINARY)
    fprintf(stderr, " convert_minicbf: The value is binary (not ASCII).\n");
  if ((err&CBF_BITCOUNT)==CBF_BITCOUNT)
    fprintf(stderr, " convert_minicbf: The expected number of bits does" 
      " not match the actual number written.\n");
  if ((err&CBF_ENDOFDATA)==CBF_ENDOFDATA)
    fprintf(stderr, " convert_minicbf: The end of the data was reached"
     " before the end of the array.\n");
  if ((err&CBF_FILECLOSE)==CBF_FILECLOSE)
    fprintf(stderr, " convert_minicbf: File close error.\n");
  if ((err&CBF_FILEOPEN)==CBF_FILEOPEN)
    fprintf(stderr, " convert_minicbf: File open error.\n");
  if ((err&CBF_FILEREAD)==CBF_FILEREAD)
    fprintf(stderr, " convert_minicbf: File read error.\n");
  if ((err&CBF_FILESEEK)==CBF_FILESEEK)
    fprintf(stderr, " convert_minicbf: File seek error.\n");
  if ((err&CBF_FILETELL)==CBF_FILETELL)
    fprintf(stderr, " convert_minicbf: File tell error.\n");
  if ((err&CBF_FILEWRITE)==CBF_FILEWRITE)
    fprintf(stderr, " convert_minicbf: File write error.\n");
  if ((err&CBF_IDENTICAL)==CBF_IDENTICAL)
    fprintf(stderr, " convert_minicbf: A data block with the new name already exists.\n");
  if ((err&CBF_NOTFOUND)==CBF_NOTFOUND)
    fprintf(stderr, " convert_minicbf: The data block, category, column or"
      " row does not exist.\n");
  if ((err&CBF_OVERFLOW)==CBF_OVERFLOW)
    fprintf(stderr, " convert_minicbf: The number read cannot fit into the"
      "destination argument.\n        The destination has been set to the nearest value.\n");
  if ((err& CBF_UNDEFINED)==CBF_UNDEFINED)
    fprintf(stderr, " convert_minicbf: The requested number is not defined (e.g. 0/0).\n");
  if ((err&CBF_NOTIMPLEMENTED)==CBF_NOTIMPLEMENTED)
    fprintf(stderr, " convert_minicbf: The requested functionality is not yet implemented.\n");
  return 0;

}


#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr," convert_minicbf: CBFlib fatal error %d\n",err); \
    outerror(err);   \
    outusage();      \
    local_exit (-1); \
  } \
 }

typedef enum {  posx=1, posy=2, negx=-1, negy=-2 } axes;

typedef struct
{
    axes posxtarg, posytarg;
} axisxform;



int outusage ( void ) {

 fprintf(stderr," \n Usage:\n");
 fprintf(stderr,"  convert_minicbf [-i input_cbf] [-o output_cbf] [-p template_cbf]\\\n");
 fprintf(stderr,"    [-d detector name] -m [x|y|x=y] [-z distance] \\\n");
 fprintf(stderr,"    [-c category_alias=category_root]* \\\n");
 fprintf(stderr,"    [-t tag_alias=tag_root]* [-F] [-R]\\\n");
 fprintf(stderr,"    [input_cbf] [output_cbf]\n");

 fprintf(stderr,"  the options are:\n");

 fprintf(stderr,"  -i input_cbf (default: stdin)\n");
 fprintf(stderr,"    the input file as a CBF with at least an image.\n");

 fprintf(stderr,"  -p template_cbf\n");
 fprintf(stderr,"    the template for the final cbf to be produced.  If template_cbf\n");
 fprintf(stderr,"    is not specified the name is constructed from the first token\n");
 fprintf(stderr,"    of the detector name and the image size as\n");
 fprintf(stderr,"       template_<type>_<columns>x<rows>.cbf\n");

 fprintf(stderr,"  -o output_cbf (default: stdout )\n");
 fprintf(stderr,"    the output cbf combining the image and the template.  If the\n");
 fprintf(stderr,"    output_cbf is not specified or is given as \"-\", it is written\n");
 fprintf(stderr,"    to stdout.\n");

 fprintf(stderr,"  -d detectorname\n");
 fprintf(stderr,"    a detector name to be used if none is provided in the image\n");
 fprintf(stderr,"    header.\n");

 fprintf(stderr,"  -F\n");
 fprintf(stderr,"    when writing packed compression, treat the entire image as\n");
 fprintf(stderr,"    one line with no averaging  \n");

 fprintf(stderr,"  -m [x|y|x=y] (default x=y, square arrays only)\n");
 fprintf(stderr,"    mirror the array in the x-axis (y -> -y)\n");
 fprintf(stderr,"                     in the y-axis (x -> -x)\n");
 fprintf(stderr,"                  or in x=y ( x -> y, y-> x)\n");

 fprintf(stderr,"  -r n\n");
 fprintf(stderr,"    rotate the array n times 90 degrees counter clockwise\n");
 fprintf(stderr,"    x -> y, y -> -x for each rotation, n = 1, 2 or 3\n");

 fprintf(stderr,"  -R \n");
 fprintf(stderr,"    if setting a beam center, set reference values of\n");
 fprintf(stderr,"    axis settings as well as standard settings\n");

 fprintf(stderr,"  -z distance\n");
 fprintf(stderr,"    detector distance along Z-axis.\n");
 
 fprintf(stderr,"  -c category_alias=category_root\n");
 fprintf(stderr,"  -t tag_alias=tagroot\n");
 fprintf(stderr,"    map the given alias to the given root, so that instead\n");
 fprintf(stderr,"    of outputting the alias, the root will be presented in the\n");
 fprintf(stderr,"    output cbf instead.  These options may be repeated as many\n");
 fprintf(stderr,"    times as needed.\n");

 return -1;

}

void applyxform(axisxform * current, axisxform * xform) {

    switch (current->posxtarg) {
        case (posx): current->posxtarg = xform->posxtarg; break;
        case (posy): current->posxtarg = xform->posytarg; break;
        case (negx): current->posxtarg = xform->posxtarg==posx?negx:
                                          (xform->posxtarg==negx?posx:
                                          (xform->posxtarg==posy?negy:
                                          (xform->posxtarg==negy?posy:0)));
                                          break;
        case (negy): current->posxtarg = xform->posytarg==posx?negx:
                                          (xform->posytarg==negx?posx:
                                          (xform->posytarg==posy?negy:
                                          (xform->posytarg==negy?posy:0)));
                                          break;
    }
    switch (current->posytarg) {
        case (posx): current->posytarg = xform->posxtarg; break;
        case (posy): current->posytarg = xform->posytarg; break;
        case (negx): current->posytarg = xform->posxtarg==posx?negx:
                                          (xform->posxtarg==negx?posx:
                                          (xform->posxtarg==posy?negy:
                                          (xform->posxtarg==negy?posy:0)));
                                          break;
        case (negy): current->posytarg = xform->posytarg==posx?negx:
                                          (xform->posytarg==negx?posx:
                                          (xform->posytarg==posy?negy:
                                          (xform->posytarg==negy?posy:0)));
                                          break;
    }
    return;
}

int main (int argc, char *argv [])
{
  FILE *in, *out, *file;

  cbf_handle minicbf;

  cbf_handle cbf;

  char detector_type [64], template_name [256], *c;

  const char *detector_name, *detector_opt,
     *array_id;

  char *header_info;
  
  size_t header_info_size;
  
  size_t header_info_cap;

  int dorefs;

  axisxform overall  = { posx, posy };
  axisxform mirrorx  = { posx, negy };
  axisxform mirrory  = { negx, posy };
  axisxform mirrorxy = { posy, posx };
  axisxform rotate1  = { posy, negx };
  axisxform rotate2  = { negx, negy };
  axisxform rotate3  = { negy, posx };
  axisxform * currentxform;

  int copt;
  int errflg = 0;
  char cbfintmp[19];
  int cbfintmpfd;
  int cbfintmpused = 0;
  char buf[CVTBUFSIZ];
  char *cbfin, *cbfout, *template, *distancestr;
  char *alias, *root;
  char xalias[81];
  int transpose;
  int fastlen, slowlen;
  int flat;
  unsigned int compression;
  int binary_id;
  size_t elsize;
  int elsigned;
  int elunsigned;
  size_t elements, elements_read;
  int minelement;
  int maxelement; 
  char *byteorder ="little_endian";
  size_t dim1;
  size_t dim2;
  size_t dim3; 
  size_t padding;
  unsigned char *image;
  size_t nbytes;

    /* Usage */

  cbfin = NULL;
  cbfout = NULL;
  template = NULL;
  detector_opt = NULL;
  transpose = 0;
  distancestr = NULL;
  dorefs = 0;
  flat = 0;
  
  cbf_failnez (cbf_make_handle (&cbf))

  while ((copt = getopt(argc,argv, "FRi:o:p:d:m:r:z:c:t:")) != EOF) {

    switch(copt) {
      case 'i':
         if (cbfin) errflg++;
         else cbfin = optarg;
         break;

      case 'o':
         if (cbfout) errflg++;
         else cbfout = optarg;
         break;

      case 'p':
         if (template) errflg++;
         else template = optarg;
         break;

      case 'F':
         flat = 1;
         break;

      case 'm':
         currentxform = (axisxform *)NULL;
         if (!strcmp(optarg,"x")) currentxform = &mirrorx;
         if (!strcmp(optarg,"y")) currentxform = &mirrory;
         if (!strcmp(optarg,"x=y")) currentxform = &mirrorxy;
         if (!currentxform) errflg++;
         else applyxform(&overall,currentxform);
         break;

      case 'r':
         currentxform = (axisxform *)NULL;
         if (!strcmp(optarg,"1")) currentxform = &rotate1;
         if (!strcmp(optarg,"2")) currentxform = &rotate2;
         if (!strcmp(optarg,"3")) currentxform = &rotate3;
         if (!currentxform) errflg++;
         else applyxform(&overall,currentxform);
         break;

      case 'R':
         dorefs = 1;
         break;

      case 'd':
         if (detector_opt) errflg++;
         else detector_opt = optarg;
         break;

      case 'z':
         if (distancestr) errflg++;
         else distancestr = optarg;
         break;
         
      case 'c':
      case 't':
          alias = optarg;
          if (alias == NULL || *alias == '\0') {
            errflg++; break;  	
          }
          root = strchr(alias,'=');
          if (root == NULL || root-alias > 80 || root-alias < 2 || *(root+1) =='\0') {
            errflg++; break;  	
          }
          strncpy(xalias,optarg,root-alias);
          xalias[root-alias] = '\0';
          root++;
          if(copt == 'c') 
          {
          	cbf_failnez (cbf_set_category_root(cbf, (const char *)xalias, (const char *) root))
          }
          else {
          	cbf_failnez (cbf_set_tag_root(cbf, (const char *)xalias, (const char *) root))
          }
          break;

      default:
         errflg++;
         break;

    }

  }

  for (; optind < argc; optind++) {
    if (!cbfin) {
      cbfin = argv[optind];
    } else {
       if (!cbfout) {
         cbfout = argv[optind];
       } else {
         errflg++;
       }
    }
  }

  if (errflg) {
    outusage();
    exit(-1);
  }


  if (!cbfin || strcmp(cbfin?cbfin:"","-") == 0) {
     strcpy(cbfintmp, "/tmp/cvt_cbfXXXXXX");
     if ((cbfintmpfd = mkstemp(cbfintmp)) == -1 ) {
       fprintf(stderr,"\n convert_minicbf: Can't create temporary file name %s.\n", cbfintmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     if ( (file = fdopen(cbfintmpfd, "w+")) == NULL) {
       fprintf(stderr,"Can't open temporary file %s.\n", cbfintmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     while ((nbytes = fread(buf, 1, CVTBUFSIZ, stdin))) {
       if(nbytes != fwrite(buf, 1, nbytes, file)) {
         fprintf(stderr,"Failed to write %s.\n", cbfintmp);
         exit(1);
       }
     }
     fclose(file);
     cbfin = cbfintmp;
     cbfintmpused = 1;
  }

    /* Read the minicbf */


  if (!(in = fopen (cbfin, "rb"))) {
     fprintf (stderr,"Couldn't open the input minicbf file %s\n", cbfin);
     exit (1);
   }
   
   
  

  cbf_failnez (cbf_make_handle (&minicbf))
  
  cbf_failnez (cbf_read_widefile (minicbf, in, MSG_DIGEST))

  if (!(in = fopen (cbfin, "rb"))) {
     fprintf (stderr,"Couldn't open the input minicbf file %s\n", cbfin);
     exit (1);
   }
   
  header_info_size = 0;
  
  header_info_cap = 0;
  
  header_info = NULL;
   
  
  while (fgets(buf,CVTBUFSIZ,in)) {
  
    size_t slen;
    
    int ignore;
    
    char * bufptr;
    
    slen = strlen(buf);
  
    if (slen > 0 && buf[slen-1]=='\n') buf[slen] = '\0';
    
  	if (!strncmp(buf,"##",2)) continue;
  	
  	ignore = 1;
  	
  	bufptr = buf;
  	
  	while(*bufptr) {
  	
  	  if (!isspace(*bufptr)) break;
  	  
  	  bufptr++;
  		
  	}
  	
  	if (!*bufptr) continue;
  	
  	if (*bufptr != '#') break;
    
    if (!strncmp(bufptr,"# ",2)) {
    
      if (header_info_size+strlen(bufptr+2)+4 > header_info_cap) {
      
        char * nheader_info;
        
        size_t nheader_info_cap;
        
        size_t ii;
        
        nheader_info_cap = 2*(header_info_cap + strlen(bufptr+2)+4);
        
        cbf_failnez((nheader_info = malloc(sizeof(char)*nheader_info_cap))==NULL?CBF_ALLOC:0)
        
        if (header_info_size) for(ii=0;ii<header_info_size;ii++) nheader_info[ii]=header_info[ii];
        
        else {nheader_info[0] = '\n'; header_info_size=1;}
        
        if (header_info) free(header_info);
        
        header_info = nheader_info;
        
        header_info_cap = nheader_info_cap;
      
      }
      
      strcpy(header_info+header_info_size,"  ");  header_info_size += 2;
      
      strcpy(header_info+header_info_size,bufptr+2); header_info_size += strlen(bufptr+2);
      
      while (header_info_size && 
            (*(header_info+header_info_size-1)=='\r' || 
             *(header_info+header_info_size-1)=='\n' ||
             !*(header_info+header_info_size-1))) header_info_size--;
      
      strcpy(header_info+header_info_size,"\n"); header_info_size++;
    
      cbf_parse_sls_header(minicbf, bufptr); 
    	
    }
  	
  }

  fclose(in);
  
  if (cbfintmpused)
  {
       if (unlink(cbfintmp) != 0 ) {
       fprintf(stderr," convert_minicbf:  Can't unlink temporary file %s.\n", cbfintmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }

  }
  
    /* Find the image */
    
  cbf_failnez(cbf_find_tag(minicbf, "_array_data.data"))
  
  cbf_failnez(cbf_rewind_row(minicbf))
  
  cbf_failnez(cbf_get_integerarrayparameters_wdims (minicbf, &compression, &binary_id, 
     &elsize, &elsigned, &elunsigned, &elements, &minelement, &maxelement,(const char **) &byteorder, 
     &dim1, &dim2, &dim3, &padding))

  cbf_failnez((image = (unsigned char*)malloc(elements*elsize))!=NULL?0:CBF_ALLOC)
  cbf_failnez(cbf_get_integerarray (minicbf, &binary_id, (void *)image, elsize, elsigned, 
  elements, &elements_read))
  if (elements != elements_read) { cbf_failnez(CBF_FORMAT) }
  

    /* Identify the detector */
    
  if (!cbf_find_tag(minicbf,"_diffrn_detector.type")) {
  
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (cbf_get_value(minicbf,&detector_name) || !detector_name || !*detector_name) {
    	
      if (detector_opt == NULL) {

        fprintf (stderr, "\n convert_inage: No detector name provided in minicbf or on the command line!");
        outusage();
        exit (3);

      }

      detector_name = detector_opt;

    }
  	
  } else {

       if (detector_opt == NULL) {

        fprintf (stderr, "\n convert_inage: No detector name provided in mincbf or on the command line!");
        outusage();
        exit (3);

      }

      detector_name = detector_opt;
 	
  }


  for (c = detector_type; *detector_name; detector_name++)

    if (!isspace (*detector_name))

      *c++ = tolower (*detector_name);

  *c = '\0';


    /* Construct the template name */

  if (template) {

    in = fopen (template, "rb");
  
  } else {

    sprintf (template_name, "template_%s_%dx%d.cbf", detector_type,
                             (int)dim1,
                             (int)dim2);

    fprintf(stderr," convert_minicbf: template_name: %s\n", template_name);

      /* Read and modify the template */

    in = fopen (template_name, "rb");
  
  }

  if (!in)  {
    fprintf (stderr," convert_minicbf: unable to open template_name: %s\n",
      template?template:template_name);
    
    exit (4);

  }

  cbf_failnez (cbf_read_template (cbf, in))



  cbf_failnez(cbf_get_array_id(cbf, 0, &array_id))
  
  cbf_failnez(cbf_require_column(cbf, "details"))

  cbf_failnez(cbf_set_value(cbf, header_info))

  cbf_failnez(cbf_set_typeofvalue(cbf,"text"))




    /* diffrn.id */

  cbf_failnez (cbf_set_diffrn_id (cbf, "DS1"))
  
  
    /* Exposure time */

    
  if (!cbf_find_tag(minicbf,"_diffrn_scan_frame.integration_time")) {

    const char *time;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&time) && time && *time) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame"))
      
      cbf_failnez(cbf_require_column(cbf,"integration_time"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,time))
    
    }
  
  }
  

  
      /* Date stamp */
    
  if (!cbf_find_tag(minicbf,"_diffrn_scan_frame.date")) {
  
    const char *date;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&date) && date && *date) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame"))
      
      cbf_failnez(cbf_require_column(cbf,"date"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,date))
    
    }

  }




    /* Image */
    
  fastlen = dim1;
  slowlen = dim2;


  if (overall.posxtarg != posx || overall.posytarg != posy )
  { int fastorig, faststep, sloworig, slowstep, curpos, i, j;

    int * tempimg;

      if (overall.posxtarg==0 || overall.posytarg==0) {
          fprintf (stderr,"\n convert_minicbf: invalid image transform.\n");
          exit(1);
      }

      if (dim2 != dim1 ) {
      fprintf(stderr,"\n convert_img: Unable to transpose image\n");
      exit(-1);
    }

    /* The fast index is the x axis, counting
       the columns, and the slow index is the y axis, counting the
       rows */
       
 
      fastorig = sloworig = 0;
      faststep = 1;
      slowstep = dim1;

      switch (overall.posxtarg) {
        case (posx): break;
        case (negx): fastorig = dim1-1; faststep = -1; break;
        case (posy): faststep = dim1; 
                     fastlen = dim2;
                     slowlen = dim1; break;
        case (negy): fastorig = (dim1)*(dim2-1);
                     faststep = -dim1;
                     fastlen = dim2;
                     slowlen = dim1; break;
      }
      switch (overall.posytarg) {
        case (posx): slowstep = 1;
                     fastlen = dim2;
                     slowlen = dim1; break;
        case (negx): sloworig = dim1-1; slowstep= -1;
                     fastlen = dim2;
                     slowlen = dim1; break;
        case (posy): break;
        case (negy): sloworig = dim1*(dim2-1);
                     slowstep = -dim1; break;
      }
      

    curpos = fastorig+sloworig;

    tempimg = malloc(dim1*dim2*elsize);
    if (!tempimg) {
      fprintf(stderr,"\n unable to allocate temporary image array\n");
    }

    if (elsize == sizeof(int)) {
    
      for (i=0;i<fastlen;i++) {
        curpos = fastorig+i*faststep+sloworig;
        for (j=0; j<slowlen;j++) {
          *((int *)tempimg+curpos) = *((int *)image+i+j*fastlen);
          curpos = curpos+slowstep;
        }
      }

      for (i=0;i<fastlen;i++) {
        for (j=0; j<slowlen;j++) {
          *(int *)((int *)image+i+j*fastlen) = *((int *)tempimg+i+j*fastlen);
        }
      }
    } else if (elsize == sizeof(short))  {
      for (i=0;i<fastlen;i++) {
        curpos = fastorig+i*faststep+sloworig;
        for (j=0; j<slowlen;j++) {
          *((short *)tempimg+curpos) = *((short *)image+i+j*fastlen);
          curpos = curpos+slowstep;
        }
      }

      for (i=0;i<fastlen;i++) {
        for (j=0; j<slowlen;j++) {
          *(int *)((short *)image+i+j*fastlen) = *((short *)tempimg+i+j*fastlen);
        }
      }
    	
    }
    else cbf_failnez(CBF_FORMAT)

  }


  if (flat) {
  
  cbf_failnez (cbf_set_image (cbf, 0, 0, CBF_PACKED|CBF_FLAT_IMAGE,
                               image, elsize, elsigned, 
                               slowlen, fastlen))
  	
  } else {
  cbf_failnez (cbf_set_image (cbf, 0, 0, compression,
                               image, elsize, elsigned,
                               slowlen, fastlen))
  }

 
   /* fix up the array_structure_list.direction and .precedence */


  if (overall.posxtarg != posx || overall.posytarg != posy) {

    unsigned int arow[2], precedence[2], temp;
    
    char * direction[2], * dtemp;
    
    arow[0] = arow[1] = 0;
    
    precedence[0] = 1;
    
    precedence[1] = 2;    
        
    direction[0] = direction[1] = NULL;
    
    cbf_failnez (cbf_find_category (cbf, "array_structure_list"))
    cbf_failnez (cbf_find_column   (cbf, "array_id"))
    
    while (!cbf_find_nextrow (cbf, array_id)) {
 
      cbf_failnez (cbf_find_column      (cbf, "precedence"))
      cbf_failnez (cbf_get_integervalue (cbf, (int *)&temp))

     if (temp == 1 || temp == 2) {
    	cbf_failnez(cbf_row_number(cbf,&(arow[temp-1])))
    	arow[temp-1]++;
    	cbf_failnez(cbf_find_column(cbf,"direction"))
    	cbf_failnez(cbf_get_value(cbf,(const char**)&(direction[temp-1])))
    	cbf_failnez(cbf_find_column   (cbf, "array_id"))
     }

    }
    

    switch (overall.posxtarg) {
      case (posx): break;
      case (negx): if (!cbf_cistrcmp(direction[0],"increasing")) {
                     direction[0] = "decreasing";
                   } else {
                     direction[0] = "increasing";
                   } 
                   break;
      case (posy): precedence[0] = 2; precedence[1] = 1;
                   dtemp = direction[0]; direction[0]=direction[1]; direction[1]=dtemp;
                   break;
      case (negy): precedence[0] = 2; precedence[1] = 1;
                   dtemp = direction[0]; direction[0]=direction[1]; direction[1]=dtemp;
                   if (!cbf_cistrcmp(direction[0],"increasing")) {
                     direction[0] = "decreasing";
                   } else {
                     direction[0] = "increasing";
                   } 
                   break;
    }
    switch (overall.posytarg) {
      case (posx): break;
      case (negx): if (!cbf_cistrcmp(direction[1],"increasing")) {
                     direction[1] = "decreasing";
                   } else {
                     direction[1] = "increasing";
                   } 
                   break;
      case (posy): break;
      case (negy): if (!cbf_cistrcmp(direction[1],"increasing")) {
                     direction[1] = "decreasing";
                   } else {
                     direction[1] = "increasing";
                   }  break;
    }
    
    if (arow[0]) {
      cbf_failnez (cbf_select_row       (cbf, arow[0]-1))
      cbf_failnez (cbf_find_column      (cbf, "precedence"))
      cbf_failnez (cbf_set_integervalue (cbf, precedence[0]))
      cbf_failnez (cbf_find_column      (cbf, "direction"))
      cbf_failnez (cbf_set_value        (cbf, direction[0]))
    }
    
    if (arow[1]) {
      cbf_failnez (cbf_select_row       (cbf, arow[1]-1))
      cbf_failnez (cbf_find_column      (cbf, "precedence"))
      cbf_failnez (cbf_set_integervalue (cbf, precedence[1]))
      cbf_failnez (cbf_find_column      (cbf, "direction"))
      cbf_failnez (cbf_set_value        (cbf, direction[1]))
    }

  }

   




/*****************************************************************************/



    /* Write the new file */

  out = stdout;

  if (cbfout && strcmp(cbfout,"-"))out = fopen (cbfout, "w+b");

  if (!out)
  {
    fprintf (stderr, " convert_minicbf:  Couldn't open the output CBF file %s\n", cbfout);

    exit (1);
  }

  cbf_failnez (cbf_write_file (cbf, out, 1, CBF,
                               MSG_DIGEST | MIME_HEADERS, 0))


    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))


    /* Free the minicbf */

  cbf_failnez (cbf_free_handle (minicbf))


    /* Success */

  return 0;
}

int local_exit (int status)
{
  exit(status);
  return 1; /* avoid warnings */
}

