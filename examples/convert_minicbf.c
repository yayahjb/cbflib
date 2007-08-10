/**********************************************************************
 * convert_minicbf -- convert a minimal cbf to a full cbf file        *
 *                                                                    *
 * Version 0.7.8 2 July 2007                                          *
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
 * WHILE YOU MAY ALTERNATIVELY DISTRIBUTE THE API UNDER THE LGPL      *
 * YOU MAY ***NOT*** DISTRIBUTE THIS PROGRAM UNDER THE LGPL           *
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
 *    [-q] [-C convention]                                        \   *
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
 *  -q                                                                *
 *    exit quickly with just the miniheader expanded                  *
 *    after the data.  No template is used.                           *
 *                                                                    *
 *  -Q                                                                *
 *    exit quickly with just the miniheader unexpanded                *
 *    before the data.  No template is used.                          *
 *                                                                    *
 *  -C convention                                                     *
 *    convert the comment form of miniheader into the                 *
 *        _array_data.header_convention convention                    *
 *        _array_data.header_contents                                 *
 *    overriding any existing values                                  *
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

#ifdef __MINGW32__
#define NOMKSTEMP
#define NOTMPDIR
#endif

int outusage ( void );
double rint(double);
int local_exit (int status);
int outerror(int err);


  /* parse a string from an sls comment style header and
     add it to cbf                                      
     
     The strings specified by E. Eikenberry as of 13 June 2007
     are:
     
     # Detector: PILATUS 6M SN01
     # 2007/Jun/13 13:13:16.286
     # Pixel_size 172e-6 m x 172e-6 m
     # Silicon sensor, thickness 0.000320 m
     # Exposure_time 0.095000 s
     # Exposure_period 0.100000 s
     # Tau = 0 s
     # Count_cutoff 1048575 counts
     # Threshold_setting 0 eV
     # Wavelength 0.7085 A
     # Energy_range (0, 0) eV
     # Detector_distance 0.79988 m
     # Detector_Voffset -0.00002 m
     # Beam_xy (1231.50, 1263.50) pixels
     # Flux 0 ph/s
     # Filter_transmission 1.0000
     # Start_angle 0.0900 deg.
     # Angle_increment 0.0100 deg.
     # Detector_2theta 0.0000 deg.
     # Polarization 0.950
     # Alpha 0.0000 deg.
     # Kappa 0.0000 deg.
     # Phi 0.0000 deg.
     # Chi 0.0000 deg.
     # Oscillation_axis  X, CW
     # N_oscillations 1
     
     */
     
     
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

     
int cbf_scale_units(char * actual_units, char * std_units, 
                                         double * actual_per_std) {
                                         
#ifdef DEBUG
  if (std_units) fprintf(stderr,"Scale actual |%s| to standard |%s|\n",actual_units,std_units);
#endif
                                         
  if (!std_units || !cbf_cistrcmp(actual_units, std_units)) {
  
    *actual_per_std = 1.;
    
    return 0;

  } else if (strlen(actual_units) == strlen(std_units)+1

    && !cbf_cistrcmp(actual_units+1,std_units)) {
    
    switch (actual_units[0]) {
    
      case ('m') : *actual_per_std =  1.e-3; break;
      case ('u') : *actual_per_std =  1.e-6; break;
      case ('n') : *actual_per_std =  1.e-9; break;
      case ('p') : *actual_per_std = 1.e-12; break;
      case ('K') : *actual_per_std =   1.e3; break;
      case ('M') : *actual_per_std =   1.e6; break;
      case ('G') : *actual_per_std =   1.e9; break;
      case ('T') : *actual_per_std =  1.e12; break;
      case ('P') : *actual_per_std =  1.e15; break;
      default:  return CBF_FORMAT;
      
    }

#ifdef DEBUG
    fprintf(stderr,"actual units per standard unit = %g\n", *actual_per_std);
#endif

    return 0;

  } else if (strlen(actual_units) == strlen(std_units)-1

    && !cbf_cistrcmp(actual_units,std_units+1)) {
    
    switch (std_units[0]) {
    
      case ('m') : *actual_per_std =   1.e3; break;
      case ('u') : *actual_per_std =   1.e6; break;
      case ('n') : *actual_per_std =   1.e9; break;
      case ('p') : *actual_per_std =  1.e12; break;
      case ('K') : *actual_per_std =  1.e-3; break;
      case ('M') : *actual_per_std =  1.e-6; break;
      case ('G') : *actual_per_std =  1.e-9; break;
      case ('T') : *actual_per_std = 1.e-12; break;
      case ('P') : *actual_per_std = 1.e-15; break;
      default:  return CBF_FORMAT;
    }

#ifdef DEBUG
    fprintf(stderr,"actual units per standard unit = %g\n", *actual_per_std);
#endif
    
    return 0;
     
  } else if (strlen(actual_units) == strlen(std_units)
  
    && strlen(actual_units) > 1
  
    && !cbf_cistrcmp(actual_units+1,std_units+1)) {
    
      switch (actual_units[0]) {
    
        case ('m') : *actual_per_std =  1.e-3; break;
        case ('u') : *actual_per_std =  1.e-6; break;
        case ('n') : *actual_per_std =  1.e-9; break;
        case ('p') : *actual_per_std = 1.e-12; break;
        case ('K') : *actual_per_std =   1.e3; break;
        case ('M') : *actual_per_std =   1.e6; break;
        case ('G') : *actual_per_std =   1.e9; break;
        case ('T') : *actual_per_std =  1.e12; break;
        case ('P') : *actual_per_std =  1.e15; break;
        default:  return CBF_FORMAT;
      
      }

      switch (std_units[0]) {
    
        case ('m') : *actual_per_std *=   1.e3; break;
        case ('u') : *actual_per_std *=   1.e6; break;
        case ('n') : *actual_per_std *=   1.e9; break;
        case ('p') : *actual_per_std *=  1.e12; break;
        case ('K') : *actual_per_std *=  1.e-3; break;
        case ('M') : *actual_per_std *=  1.e-6; break;
        case ('G') : *actual_per_std *=  1.e-9; break;
        case ('T') : *actual_per_std *= 1.e-12; break;
        case ('P') : *actual_per_std *= 1.e-15; break;
        default:  return CBF_FORMAT;
     }
    
#ifdef DEBUG
    fprintf(stderr,"actual units per standard unit = %g\n", *actual_per_std);
#endif
    
     return 0;
    
  } 
  
  return CBF_FORMAT;
}

int cbf_parse_sls_header(cbf_handle cbf, const char * buffer, 
    int commentflg) {

    double pscalex=1., pscaley=1.;

    double bcx, bcy, bcscalex, bcscaley, erlow, erhigh;
    
    double tempdouble, unitsratio;
    
    char * tempendptr;
    
    char * slsstr;
    
    const char * valstr;
    
    char slsbuf[2049];
    
    char valbuf[4097];
    
    char mxunits[33], myunits[33];
    
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
    if (!cbf_get_value((cbf),(const char **)&valstr) && valstr && *valstr){\
                                                                 \
      if (strlen(valstr)+strlen((cbfstring))<4095) {             \
                                                                 \
        if (*valstr != '\n') {                                   \
                                                                 \
          strcpy(valbuf,"\n"); strcat(valbuf,valstr);            \
                                                                 \
        } else {                                                 \
                                                                 \
          strcpy(valbuf,valstr);                                 \
                                                                 \
        }                                                        \
                                                                 \
        strcat(valbuf,"\n"); strcat(valbuf,cbfstring);           \
                                                                 \
        cbf_failnez(cbf_set_value((cbf),valbuf))                 \
                                                                 \
      } else {                                                   \
                                                                 \
        return CBF_FORMAT;                                       \
                                                                 \
      }                                                          \
                                                                 \
                                                                 \
    } else {                                                     \
                                                                 \
      cbf_failnez(cbf_set_value((cbf),(cbfstring)))              \
                                                                 \
    }                                                            \

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
    cbf_failnez(cbf_scale_units(cbfstring, unit, &unitsratio))   \
                                                                 \
    tempdouble *= unitsratio;                                    \
                                                                 \
    cbf_failnez(cbf_require_column((cbf),(cbfcol)))              \
                                                                 \
    cbf_failnez(cbf_set_doublevalue((cbf),"%-15g",tempdouble)) 
        
  while(*buffer) {
  
  slsstr = (char *)slsbuf;
  
  while(*buffer && (*buffer!='\r') && (*buffer!='\n') && (slsstr-slsbuf)<2047) *slsstr++=*buffer++;
  
  while(slsstr != slsbuf && isspace(*(slsstr-1))) slsstr--;
  
  *slsstr='\0';
  
  slsstr = (char *)slsbuf;
   
  /* skip all initial whitespace */
    
  while (*slsstr && isspace(*slsstr)) slsstr++;
  
  if (strlen(slsstr) == 0) {
  
    while (*buffer && ((*buffer=='\r') || (*buffer=='\n'))) buffer++; 
  
    continue;
  
  }
  
#ifdef DEBUG
  fprintf(stderr,"Processing %s\n",slsstr);
#endif

  /* if we have specified that this must be a comment
     require a leading "# " abort if not                        */
     
  if (commentflg) {
    if (strlen(slsstr) < 2 
      || slsstr[0] != '#' 
      || !isspace(slsstr[1])) return CBF_FORMAT;
    slsstr += 2;
  }

  
  while (*slsstr && isspace(*slsstr)) slsstr++;
  
  if (slsstr[0] == '#') {
  
    slsstr++;
    
    while (*slsstr && isspace(*slsstr)) slsstr++;
  
  }

  /* check for   Detector: PILATUS 6M SN01 */

  if (!cbf_cistrncmp(slsstr,"Detector: ",strlen("Detector: "))) {
  
    char * ptr;
  
    slsstr += strlen("Detector: ");
    
    while (*slsstr && isspace(*slsstr)) slsstr++;
    
    ptr = slsstr;
    
    while (*ptr && (cbf_cistrncmp(ptr," SN",3))) ptr++;
    
    if (*ptr) *ptr++ = 0;
    
    while (*ptr && isspace(*ptr)) ptr++;
  
    cbf_set_value_from_string(cbf,slsstr,"diffrn_detector","type")
    
    cbf_set_value_from_string(cbf,ptr,"diffrn_detector","details")
    
    	
  } else 
  
  /* check for 2007/Jun/13 13:13:16.286 */

    if (strlen(slsstr)>=11 
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
                                       
     }

  } else 
  
    /* check for Pixel_size 172e-6 m x 172e-6 m */

    if (!cbf_cistrncmp(slsstr,"Pixel_size ",strlen("Pixel_size "))) {
  
    char *endptr;
     
    double psx, psy;
          
    slsstr += strlen("Pixel_size ");
     
    pscalex = pscaley = 1.;
    
    psy = psx = strtod (slsstr, &endptr);
     
    if (endptr && *endptr) {
     
      slsstr = endptr;
       
      while (*slsstr && isspace(*slsstr)) slsstr++;
       
      if (!cbf_cistrncmp("m ",slsstr,2))  slsstr += 2;
       
      else if (!cbf_cistrncmp("mm ",slsstr,2) || !cbf_cistrcmp("mm",slsstr)) {
       
        slsstr +=2;
         
        pscaley = pscalex = 1.e-3;
       	
        } else  {
       
          pscaley = pscalex = 1.e-6;
       	
        }
       
        while (*slsstr && isspace(*slsstr)) slsstr++;
        
        if (*slsstr == 'x' || *slsstr == 'X' || *slsstr==',') {
        
          slsstr++;
        
          while (*slsstr && isspace(*slsstr)) slsstr++;

        }
       
        if (*slsstr) {
       
          psy = strtod (slsstr, &endptr);

          if (endptr && *endptr) {
          
          slsstr = endptr;

          while (*slsstr && isspace(*slsstr)) slsstr++;

          if (!cbf_cistrncmp("m ",slsstr,2)|| !cbf_cistrcmp("m",slsstr))  slsstr ++;
       
          else if ( !cbf_cistrncmp("mm ",slsstr,2) || !cbf_cistrcmp("mm",slsstr) ) {
       
            slsstr +=2;
          
            pscaley = 1.e-3;
       	
          } else  {
       
            pscaley = 1.e-6;
           
          }
          
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
     
  } else 
    
    /* Check for Silicon sensor, thickness 0.000320 m */
      
    if  (!cbf_cistrncmp(slsstr,"Silicon sensor, thickness ",strlen("Silicon sensor, thickness "))) {
    
          cbf_set_value_from_string(cbf,slsstr,"diffrn_detector","details")
     
  } else 
  
    /* check for Exposure_time 0.095000 s */
    
    if (!cbf_cistrncmp(slsstr,"Exposure_time ",strlen("Exposure time "))) {
  
    slsstr += strlen("Exposure time: ");

    cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_scan_frame","integration_time","s")

  } else 
  
    /* check for Exposure_period 0.100000 s */
    
    if (!cbf_cistrncmp(slsstr,"Exposure_period ",strlen("Exposure_period "))) {
  
    slsstr += strlen("Exposure_period ");

    cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_scan_frame","integration_period","s")

  }  else 
  
    /* check for Tau = 0 s */
    
    if (!cbf_cistrncmp(slsstr,"Tau = ",strlen("Tau = "))) {
  
    slsstr += strlen("Tau = ");

    cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_radiation","tau","s")
    
  } else
  
    /* check for Count_cutoff 1048575 counts */

    if (!cbf_cistrncmp(slsstr,"Count_cutoff ",
    
       strlen("Count_cutoff "))) {

    slsstr += strlen("Count_cutoff ");
  
    cbf_set_doublevalue_from_string(cbf,slsstr,"array_intensities","overload","counts")
    
  } else
  
    /* check for Threshold_setting 0 eV */

    if (!cbf_cistrncmp(slsstr,"Threshold_setting ",
    
       strlen("Threshold_setting "))) {
       
     slsstr += strlen("Threshold_setting ");

     cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_detector","threshold","eV")

  } else
  
    /* check for Wavelength 0.7085 A */
  
    if (!cbf_cistrncmp(slsstr,"wavelength ",strlen("wavelength "))) {
  
    slsstr += strlen("wavelength ");
    
    cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_radiation_wavelength","wavelength","A")  
  
  } else 
  
    /* check for Energy_range (0, 0) eV */

    if (!cbf_cistrncmp(slsstr,"Energy_range ",
      strlen("Energy_range "))) {

     char *endptr;
     
     slsstr += strlen("Energy_range ");
     
     erlow = erhigh = 0.;
   
     if (*slsstr == '(' || *slsstr == '[' || *slsstr == '{') slsstr++;

     while (*slsstr && isspace(*slsstr)) slsstr++;     
     
     erlow = erhigh = strtod (slsstr, &endptr);
     
     if (endptr && *endptr) {
     
       slsstr = endptr;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       if (*slsstr == ',' ) slsstr++;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       if (*slsstr) {
       
         erhigh = strtod (slsstr, &endptr);
         
         if (endptr && *endptr) slsstr = endptr;

         while (*slsstr && isspace(*slsstr)) slsstr++;
       	
       }
            	
     }

     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (*slsstr == ')' || *slsstr == ']' || *slsstr == '}') slsstr++;
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (!cbf_cistrncmp("eV",slsstr,2)) {
     
       cbf_failnez(cbf_require_category(cbf,"diffrn_detector"))
          
       cbf_failnez(cbf_require_column(cbf,"energy_range_low"))
     
       cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",erlow))

       cbf_failnez(cbf_require_column(cbf,"energy_range_high"))
        	
     } else {
     
       cbf_failnez(CBF_FORMAT);
        
     }

  }  else
  
    /* check for Detector_distance 0.79988 m */
    
    if (!cbf_cistrncmp(slsstr,"Detector_distance ",
      strlen("Detector_distance "))) {
 
     slsstr += strlen("Detector_distance ");
   
     cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_measurement",
      "sample_detector_distance", "mm");

  }  else 
  
    /* check for Detector_Voffset -0.00002 m */

    if (!cbf_cistrncmp(slsstr,"Detector_Voffset ",
      strlen("Detector_Voffset "))) {
 
     slsstr += strlen("Detector_Voffset ");
   
     cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_measurement",
      "sample_detector_voffset", "mm");

  }  else 

    /* check for Beam_xy (1231.50, 1263.50) pixels */
  
  if (!cbf_cistrncmp(slsstr,"beam_xy ",
      strlen("beam_xy "))) {
  
     char *endptr;
     
     char *bcunits;
     
     slsstr += strlen("beam_xy ");
     
     bcscalex = bcscaley = 1.;
     
     bcunits = "unknown";
    
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (*slsstr == '(' || *slsstr == '[' || *slsstr == '{') slsstr++;

     while (*slsstr && isspace(*slsstr)) slsstr++;     
     
     bcy = bcx = strtod (slsstr, &endptr);
     
     if (endptr && *endptr) {
     
       slsstr = endptr;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       endptr = mxunits;
       
       while ((endptr-mxunits)<32 
         && *slsstr 
         && *slsstr!=',' 
         && *slsstr!=')' 
         && *slsstr!=']' 
         && *slsstr!='}'
         && !(isspace(*slsstr))
         && !(isdigit(*slsstr))) *endptr++=*slsstr++;
         
       *endptr='\0';
       
       while (*slsstr && isspace(*slsstr)) slsstr++; 
              
       if (!cbf_scale_units(mxunits, "mm",&bcscalex)) {
       
         bcscaley = bcscalex;  
         
         bcunits = "mm";
         
       } else if (!cbf_scale_units(mxunits, "pixels",&bcscalex)) {
       
         bcscaley = bcscalex;  
         
         bcunits = "pixels";
         
       } else if (!cbf_scale_units(mxunits, "bins",&bcscalex)) {
       
         bcscaley = bcscalex;  
         
         bcunits = "bins";
         
       } else {
       
         bcscaley = bcscalex = 1.;
         
         bcunits = mxunits;
       
       }

       if (*slsstr == ',' ) slsstr++;
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       if (*slsstr) {
       
         bcy = strtod (slsstr, &endptr);

         while (*slsstr && isspace(*slsstr)) slsstr++;
         
         endptr = myunits;
       
         while ((endptr-myunits)<32 
           && *slsstr 
           && *slsstr!=',' 
           && *slsstr!=')' 
           && *slsstr!=']' 
           && *slsstr!='}'
           && !(isspace(*slsstr))
           && !(isdigit(*slsstr))) *endptr++=*slsstr++;
         
         *endptr='\0';
       
         while (*slsstr && isspace(*slsstr)) slsstr++; 

         if (!cbf_cistrcmp(bcunits,"unknown") || !*bcunits) {
                       
           if (!cbf_scale_units(myunits, "mm",&bcscalex)) {
       
             bcscaley = bcscalex;  

             bcunits = "mm";
         
           } else if (!cbf_scale_units(myunits, "pixels",&bcscalex)) {
       
             bcscaley = bcscalex;  
         
             bcunits = "pixels";
         
           } else if (!cbf_scale_units(myunits, "bins",&bcscalex)) {
       
             bcscaley = bcscalex;  
         
             bcunits = "bins";
         
           } else {
       
             bcscaley = bcscalex = 1.;
         
             bcunits = myunits;
       
           }
         
         } else {
         
           if (!cbf_scale_units(myunits, "mm",&bcscaley)) {
        
             if (cbf_cistrcmp(bcunits,"mm")) return CBF_FORMAT;
       
           } else if (!cbf_scale_units(myunits, "pixels",&bcscaley)) {
       
            if (cbf_cistrcmp(bcunits,"pixels")) return CBF_FORMAT;
          
           } else if (!cbf_scale_units(myunits, "bins",&bcscaley)) {
       
            if (cbf_cistrcmp(bcunits,"bins")) return CBF_FORMAT;
         
           } else {
       
             bcscaley = 1.;
             
             if (cbf_cistrcmp(bcunits,myunits)) return CBF_FORMAT;
         
           }
         
           
         }
         
       } else {
       
        if (*slsstr == ')' || *slsstr == ']' || *slsstr != ']') slsstr++;

      }
 
     }

     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (*slsstr == ')' || *slsstr == ']' || *slsstr == '}') slsstr++;
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     if (*bcunits || !cbf_cistrcmp(bcunits,"unknown") ){
         
         endptr = mxunits;
         
         while ((endptr-myunits)<32 
                && *slsstr 
                && *slsstr!=',' 
                && *slsstr!=')' 
                && *slsstr!=']' 
                && *slsstr!='}'
                && !(isspace(*slsstr))
                && !(isdigit(*slsstr))) *endptr++=*slsstr++;
         
         *endptr='\0';
         
         while (*slsstr && isspace(*slsstr)) slsstr++; 
         
         if (!cbf_scale_units(mxunits, "mm",&bcscalex)) {
             
             bcscaley = bcscalex;  
             
             bcunits = "mm";
             
         } else if (!cbf_scale_units(mxunits, "pixels",&bcscalex)) {
             
             bcscaley = bcscalex;  
             
             bcunits = "pixels";
             
         } else if (!cbf_scale_units(mxunits, "bins",&bcscalex)) {
             
             bcscaley = bcscalex;  
             
             bcunits = "bins";
             
         } else {
             
             bcscaley = bcscalex = 1.;
             
             bcunits = mxunits;
             
         }
         
     }
     
     
    if (!*bcunits || !cbf_cistrcmp(bcunits,"unknown") ) {
     
         bcunits = "pixels";
       
         bcscaley = bcscalex = 1.;
     
     }
     
     
     bcx *= bcscalex;
     
     bcy *= bcscaley;
     
     cbf_failnez(cbf_require_category(cbf,"diffrn_detector_element"))
          
     cbf_failnez(cbf_require_column(cbf,"reference_center_fast"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",bcx))
          
     cbf_failnez(cbf_require_column(cbf,"reference_center_slow"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",bcy))
     
     cbf_failnez(cbf_require_column(cbf,"reference_center_units"))
     
     cbf_failnez(cbf_set_value(cbf,bcunits))
     
   } else
   
     /* check for Flux 0 ph/s */
     
     if (!cbf_cistrncmp(slsstr,"Flux ",
       strlen("Flux "))) {
       
       slsstr += strlen("Flux ");
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_radiation","flux","ph/s");
       
   } else
   
     /* check for Filter_transmission 1.0000 */
     
     if (!cbf_cistrncmp(slsstr,"Filter_transmission ",
       strlen("Filter_transmission "))) {
       
       slsstr += strlen("Filter_transmission ");
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_radiation","filter_transmission",NULL);
       

   } else 
   
     /* check for Start_angle 0.0900 deg. */
     
     if (!cbf_cistrncmp(slsstr,"Start_angle ",
       strlen("Start_angle "))) {
       
     double scan;
     
     char *endptr;
      
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_SCAN"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     slsstr += strlen("Start_angle ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     scan = strtod(slsstr,&endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",scan))
      
     } else
   
     /* check for Oscillation_axis  X, CW */
     
     if (!cbf_cistrncmp(slsstr,"Oscillation_axis ",
       strlen("Oscillation_axis "))) {
       
       slsstr += strlen("Oscillation_axis ");
       
       while (*slsstr && isspace(*slsstr)) slsstr++;
       
       cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
       cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
       cbf_failnez(cbf_require_row(cbf,"GONIOMETER_SCAN"))
     
       cbf_failnez(cbf_require_column(cbf,"axis_description"))
     
       cbf_failnez(cbf_set_value(cbf,slsstr))

   } else 
   
     /* check for Angle_increment 0.0100 deg. */
     
     if (!cbf_cistrncmp(slsstr,"Angle_increment ",
       strlen("Angle_increment "))) {
       
     double scan;

     char * endptr;
           
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_SCAN"))
     
     cbf_failnez(cbf_require_column(cbf,"angle_increment"))
     
     slsstr += strlen("Angle_increment ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     scan = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",scan))

   } else
   
     /* check for Detector_2theta 0.0000 deg. */
     
     if (!cbf_cistrncmp(slsstr,"Detector_2theta ",
       strlen("Detector_2theta "))) {
       
     double twotheta;
     
     char *endptr;
      
     slsstr += strlen("Detector_2theta ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     twotheta = strtod(slsstr, &endptr);

     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"DETECTOR_TWOTHETA"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))     
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",twotheta))
     
   } else
   
     /* check for Polarization 0.950 */
     
     if (!cbf_cistrncmp(slsstr,"Polarization ",
       strlen("Polarization "))) {
       
       slsstr += strlen("Polarization ");
       
       while (*slsstr && isspace(*slsstr)) slsstr++;

       cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_radiation","polarizn_source_ratio",NULL);
       
   } else 
   
     /* check for Alpha 0.0000 deg. */
     
     if (!cbf_cistrncmp(slsstr,"Alpha ",
       strlen("Alpha "))) {
       
     double alpha;
     
     char *endptr;
       
     slsstr += strlen("Alpha ");

     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     alpha = strtod(slsstr, &endptr);
    
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_ALPHA"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
 
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",alpha))
    
   } else 

     /* check for Omega 0.0000 deg. */
     
     if (!cbf_cistrncmp(slsstr,"Omega ",
       strlen("Omega "))) {
       
     double omega;
     
     char *endptr;
       
     slsstr += strlen("Omega ");

     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     omega = strtod(slsstr, &endptr);
    
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_OMEGA"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
 
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",omega))
    
   } else 
   
     /* check for Kappa 0.0000 deg. */
     
     if (!cbf_cistrncmp(slsstr,"Kappa ",
       strlen("Kappa "))) {
       
     double kappa;
     
     char *endptr;
      
     slsstr += strlen("Kappa ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     kappa = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_KAPPA"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",kappa))

   } else if (!cbf_cistrncmp(slsstr,"Chi ",
       strlen("Chi "))) {
       
     double chi;
     
     char *endptr;
      
     slsstr += strlen("Chi ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     chi = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_CHI"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",chi))
     
   } else if (!cbf_cistrncmp(slsstr,"Phi ",
       strlen("Phi "))) {
       
     double phi;
     
     char *endptr;
      
     slsstr += strlen("Phi ");
     
     while (*slsstr && isspace(*slsstr)) slsstr++;
     
     phi = strtod(slsstr, &endptr);
     
     cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame_axis"))
      
     cbf_failnez(cbf_require_column(cbf,"axis_id"))
     
     cbf_failnez(cbf_require_row(cbf,"GONIOMETER_PHI"))
     
     cbf_failnez(cbf_require_column(cbf,"angle"))
     
     cbf_failnez(cbf_set_doublevalue(cbf,"%-15g",phi))
     
     } else
   
     /* check for N_oscillations 1 */
     
     if (!cbf_cistrncmp(slsstr,"N_oscillations ",
       strlen("N_oscillations "))) {
       
       slsstr += strlen("N_oscillations ");
       
       while (*slsstr && isspace(*slsstr)) slsstr++;

       cbf_set_doublevalue_from_string(cbf,slsstr,"diffrn_scan_frame","oscillations",NULL);
       
     } else {
     
       cbf_failnez(CBF_FORMAT);
     
     }
   
    
   while (*buffer && ((*buffer=='\r') || (*buffer=='\n'))) buffer++; 

   }
   
   return 0;
}



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
 fprintf(stderr,"    [-q] [-C convention]  \\\n");
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
 
 fprintf(stderr,"  -q\n");
 fprintf(stderr,"    exit quickly with just the miniheader expanded\n");
 fprintf(stderr,"    after the data.  No template is used.\n");

 fprintf(stderr,"  -Q\n");
 fprintf(stderr,"    exit quickly with just the miniheader unexpanded\n");
 fprintf(stderr,"    before the data.  No template is used.\n");

 fprintf(stderr,"  -C convention\n");
 fprintf(stderr,"    convert the comment form of miniheader into the\n");
 fprintf(stderr,"        _array_data.header_convention convention\n");
 fprintf(stderr,"        _array_data.header_contents\n");
 fprintf(stderr,"    overriding any existing values\n");


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

 fprintf(stderr,"  -u \n");
 fprintf(stderr,"    write the image as unsigned short.\n");

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
     
  const char *datablockname;

  char *header_info;
  
  char *header_info_copy;
  
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
 #ifndef NOMKSTEMP
  int cbfintmpfd;
 #endif
  int cbfintmpused = 0;
  char buf[CVTBUFSIZ];
  char *cbfin, *cbfout, *template, *distancestr;
  char *convention;
  char *alias, *root;
  char xalias[81];
  int transpose;
  int fastlen, slowlen;
  int flat;
  int quick;
  int unshort;
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
  convention = NULL;
  dorefs = 0;
  flat = 0;
  quick = 0;
  unshort = 0;
  
  cbf_failnez (cbf_make_handle (&cbf))

  while ((copt = getopt(argc,argv, "FRQqui:o:p:C:d:m:r:z:c:t:")) != EOF) {

    switch(copt) {
      case 'i':
         if (cbfin) errflg++;
         else cbfin = optarg;
         break;

      case 'o':
         if (cbfout) errflg++;
         else cbfout = optarg;
         break;

      case 'q':
         if (quick) errflg++;
         else quick=1;
         break;

      case 'Q':
         if (quick) errflg++;
         else quick=-1;
         break;

      case 'p':
         if (template) errflg++;
         else template = optarg;
         break;

      case 'F':
         flat = 1;
         break;
         
      case 'C':
         if (convention) errflg++;
         else convention = optarg;
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

      case 'u':
         unshort = 1;
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
#ifdef NOTMPDIR
     strcpy(cbfintmp, "cif2cbfXXXXXX");
#else
     strcpy(cbfintmp, "/tmp/cif2cbfXXXXXX");
#endif
#ifdef NOMKSTEMP
     if (mktemp(cbfintmp) == NULL ) {
       fprintf(stderr,"\n convert_minicbf: Can't create temporary file name %s.\n", cbfintmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     if ( (file = fopen(cbfintmp,"wb+")) == NULL) {
       fprintf(stderr,"Can't open temporary file %s.\n", cbfintmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);     	
     }
#else
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
#endif
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

   
  header_info_size = 0;
  
  header_info_cap = 0;
  
  header_info = NULL;
  
  if (!convention && !cbf_find_tag(minicbf,"_array_data.header_contents")) {
  
    cbf_failnez(cbf_get_value(minicbf,(const char * *)&header_info))
    
    cbf_parse_sls_header(minicbf, header_info, 0); 

  } else {
  
    char * lead = "  ";
    
    if (quick) lead = "# ";
    
    if (!(in = fopen (cbfin, "rb"))) {
      fprintf (stderr,"Couldn't open the input minicbf file %s\n", cbfin);
      exit (1);
    }

  
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
      
        strcpy(header_info+header_info_size,lead);  header_info_size += 2;
      
        strcpy(header_info+header_info_size,bufptr+2); header_info_size += strlen(bufptr+2);
      
        while (header_info_size && 
            (*(header_info+header_info_size-1)=='\r' || 
             *(header_info+header_info_size-1)=='\n' ||
             !*(header_info+header_info_size-1))) header_info_size--;
      
        strcpy(header_info+header_info_size,"\n"); header_info_size++;
    
        cbf_parse_sls_header(minicbf, bufptr, 1); 
    	
      }
  	
    }

    fclose(in);
  
    if (convention) {
  
      cbf_failnez(cbf_require_category(minicbf,"array_data"))
    
      cbf_failnez(cbf_require_column(minicbf,"header_convention"))
    
      cbf_failnez(cbf_set_value(minicbf,convention))
    
      cbf_failnez(cbf_require_column(minicbf,"header_contents"))
    
      cbf_failnez(cbf_set_value(minicbf,header_info))
  
    }
  
  }
  
  
    /* See if we want a quick exit */
    
    
  if (quick > 0) 
  {
 
    /* Write the new file */

     out = stdout;

     if (cbfout && strcmp(cbfout,"-"))out = fopen (cbfout, "w+b");

     if (!out)
     {
       fprintf (stderr, " convert_minicbf:  Couldn't open the output CBF file %s\n", cbfout);

       exit (1);
     }

     cbf_failnez (cbf_write_file (minicbf, out, 1, CBF,
                               MSG_DIGEST | MIME_HEADERS | PAD_4K, 0))

    /* Free the cbf */

    cbf_failnez (cbf_free_handle (cbf))


    /* Free the minicbf */

    cbf_failnez (cbf_free_handle (minicbf))


    /* Success */

    if (cbfintmpused) {
       if (unlink(cbfintmp) != 0 ) {
         fprintf(stderr," convert_minicbf:  Can't unlink temporary file %s.\n", cbfintmp);
         fprintf(stderr,"%s\n",strerror(errno));
         exit(1);
       }

    }


    return 0;
    
  } else if (quick < 0) {
  
    const char * value;
    
    cbf_failnez(cbf_datablock_name(minicbf,&datablockname))
    
    cbf_failnez(cbf_force_new_datablock(cbf,datablockname))
    
    cbf_failnez(cbf_find_tag(minicbf, "_array_data.data"))
  
    cbf_failnez(cbf_require_category(cbf,"array_data"));
  
    cbf_failnez(cbf_rewind_row(minicbf))
  
    if (!cbf_find_column(minicbf,"header_convention") && !cbf_get_value(minicbf,&value) && value) {
  
      cbf_failnez(cbf_require_column(cbf,"header_convention"))
      
      cbf_failnez(cbf_set_value(cbf,value))

    }

    if (!cbf_find_column(minicbf,"header_contents") && !cbf_get_value(minicbf,&value) && value) {
  
      cbf_failnez(cbf_require_column(cbf,"header_contents"))
      
      cbf_failnez(cbf_set_value(cbf,value))

    } else if (header_info) {
    
      cbf_failnez(cbf_require_column(cbf,"header_contents"))

      cbf_failnez(cbf_set_value(cbf,header_info))
    }
    
    cbf_failnez(cbf_find_column(minicbf,"data"))
  
    cbf_failnez(cbf_get_integerarrayparameters_wdims (minicbf, &compression, &binary_id, 
      &elsize, &elsigned, &elunsigned, &elements, &minelement, &maxelement,(const char **) &byteorder, 
      &dim1, &dim2, &dim3, &padding))
      
    fastlen = dim1;
    slowlen = dim2;

    cbf_failnez((image = (unsigned char*)malloc(elements*elsize))!=NULL?0:CBF_ALLOC)
    cbf_failnez(cbf_get_integerarray (minicbf, &binary_id, (void *)image, elsize, elsigned, 
      elements, &elements_read))
    if (elements != elements_read) { cbf_failnez(CBF_FORMAT) }
    
    cbf_failnez (cbf_require_column(cbf,"data"))
  
    if (flat) {
    
      cbf_failnez( cbf_set_integerarray_wdims (cbf, CBF_PACKED|CBF_FLAT_IMAGE, binary_id, 
        image, elsize, elsigned, elements, byteorder, dim1, dim2, dim3, padding))
  	
    } else {
      cbf_failnez( cbf_set_integerarray_wdims (cbf, compression, binary_id, 
        image, elsize, elsigned, elements, byteorder, dim1, dim2, dim3, padding))
    }

  
    /* Write the new file */

    out = stdout;

    if (cbfout && strcmp(cbfout,"-"))out = fopen (cbfout, "w+b");

    if (!out) {

      fprintf (stderr, " convert_minicbf:  Couldn't open the output CBF file %s\n", cbfout);

      exit (1);

    }

    cbf_failnez (cbf_write_file (cbf, out, 1, CBF,
                               MSG_DIGEST | MIME_HEADERS | PAD_4K, 0))

    /* Free the cbf */

    cbf_failnez (cbf_free_handle (cbf))


    /* Free the minicbf */

    cbf_failnez (cbf_free_handle (minicbf))


    /* Success */

    if (cbfintmpused) {
       if (unlink(cbfintmp) != 0 ) {
         fprintf(stderr," convert_minicbf:  Can't unlink temporary file %s.\n", cbfintmp);
         fprintf(stderr,"%s\n",strerror(errno));
         exit(1);
       }

    }

    return 0;

  
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

        fprintf (stderr, "\n convert_inage: No detector name provided in minicbf or on the command line!");
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
  
  if (header_info) {
  
    char * src;
    
    char * dst;
    
    char ccur, cprev;
  
    header_info_copy=(char *)malloc(strlen(header_info)+1);
    
    src = header_info;
    
    dst = header_info_copy;
    
    cprev = '\n';
    
    while((ccur=*src++)) {
    
      if (ccur == '#' 
        && (cprev == '\n'|| cprev == '\r' ) ) *dst++ = ' ';
      else *dst++=ccur;
      
      cprev = ccur;
    }
    
    *dst++ = '\0';
    
    cbf_failnez(cbf_set_value(cbf, header_info_copy))

    cbf_failnez(cbf_set_typeofvalue(cbf,"text"))
        
  }




    /* diffrn.id */

  cbf_failnez (cbf_set_diffrn_id (cbf, "DS1"))
  
    /* diffrn_detector.details */
    
  if (!cbf_find_tag(minicbf,"_diffrn_detector.details")) {
  
      const char *details;
  
      cbf_failnez(cbf_rewind_row(minicbf))
      
     if (!cbf_get_value(minicbf,&details) && details && *details) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_detector"))
      
      cbf_failnez(cbf_require_column(cbf,"details"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,details))
  	
    }
  
  }
  
  
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


      /* Oscillations */
    
  if (!cbf_find_tag(minicbf,"_diffrn_scan_frame.oscillations")) {
  
    const char *oscillations;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&oscillations) && oscillations && *oscillations) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_scan_frame"))
      
      cbf_failnez(cbf_require_column(cbf,"oscillations"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,oscillations))
    
    }

  }

    /* Element size */


  if (!cbf_find_tag(minicbf,"_array_element_size.size")) {
  
    const char *size, *index;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&size) && size && *size) {
    
      cbf_failnez(cbf_require_category(cbf,"array_element_size"))
      
      cbf_failnez(cbf_require_column(cbf,"size"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,size))
      
      cbf_failnez(cbf_require_column(cbf,"index"))
      
      cbf_failnez(cbf_require_column(minicbf,"index"))
      
      if (!cbf_get_value(minicbf,&index) && index && *index) {
      
        cbf_failnez(cbf_set_value(cbf,index))
      
      }
      
      if (!cbf_next_row(minicbf)) {
      
        cbf_failnez(cbf_find_column(minicbf,"size"))
        
        if (!cbf_get_value(minicbf,&size) && size && *size) {
      
          cbf_failnez(cbf_find_column(cbf,"size"))
          
          cbf_failnez(cbf_next_row(cbf))
          
          cbf_failnez(cbf_set_value(cbf,size))
          
          cbf_failnez(cbf_find_column(cbf,"index"))
          
          cbf_failnez(cbf_find_column(minicbf,"index"))
          
          if (!cbf_get_value(minicbf,&index) && index && *index) {
      
            cbf_failnez(cbf_set_value(cbf,index))
      
          }
          
       }
      	
      }
      
    }

  }

      /* diffrn_radiation.tau */
    
  if (!cbf_find_tag(minicbf,"_diffrn_radiation.tau")) {
  
    const char *tau;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&tau) && tau && *tau) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_radiation"))
      
      cbf_failnez(cbf_require_column(cbf,"tau"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,tau))
    
    }

  }


      /* diffrn_radiation.flux */
    
  if (!cbf_find_tag(minicbf,"_diffrn_radiation.flux")) {
  
    const char *flux;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&flux) && flux && *flux) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_radiation"))
      
      cbf_failnez(cbf_require_column(cbf,"flux"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,flux))
    
    }

  }



      /* diffrn_radiation.polarizn_source_ratio */
    
  if (!cbf_find_tag(minicbf,"_diffrn_radiation.polarizn_source_ratio")) {
  
    const char *polarizn_source_ratio;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&polarizn_source_ratio) && polarizn_source_ratio && *polarizn_source_ratio) {
    
      cbf_failnez(cbf_require_category(cbf,"diffrn_radiation"))
      
      cbf_failnez(cbf_require_column(cbf,"polarizn_source_ratio"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,polarizn_source_ratio))
    
    }

  }

      /* array_intensities.overload */
    
  if (!cbf_find_tag(minicbf,"_array_intensities.overload")) {
  
    const char *overload;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    if (!cbf_get_value(minicbf,&overload) && overload && *overload) {
    
      cbf_failnez(cbf_require_category(cbf,"array_intensities"))
      
      cbf_failnez(cbf_require_column(cbf,"overload"))
      
      cbf_failnez(cbf_rewind_row(cbf))
      
      cbf_failnez(cbf_set_value(cbf,overload))
    
    }

  }

      /* Wavelength */

  if (!cbf_find_tag(minicbf,"_diffrn_radiation_wavelength.wavelength")) {
  
    double wavelength;
  
    cbf_failnez(cbf_get_doublevalue(minicbf,&wavelength))

    if (wavelength)

    cbf_failnez (cbf_set_wavelength (cbf, wavelength))
    
  }


    /* Distance */


  if (!cbf_find_tag(minicbf,"_diffrn_measurement.sample_detector_distance")) {
  
    double distance;
  
    cbf_failnez(cbf_get_doublevalue(minicbf,&distance))

    if (distance == 0.) {
  
      distance = atof (distancestr);

    }

    cbf_failnez (cbf_set_axis_setting (cbf, 0, "DETECTOR_Z", distance, 0))

    cbf_failnez(cbf_require_category(cbf,"diffrn_measurement"))

    cbf_failnez(cbf_require_column(cbf,"sample_detector_distance"))

    cbf_failnez(cbf_set_doublevalue(cbf,"%g", distance))
    
  }


    /* Vertical Offset */


  if (!cbf_find_tag(minicbf,"_diffrn_measurement.sample_detector_voffset")) {
  
    double voffset;
  
    cbf_failnez(cbf_get_doublevalue(minicbf,&voffset))

    cbf_failnez (cbf_set_axis_setting (cbf, 0, "DETECTOR_Y", voffset, 0))

    cbf_failnez(cbf_require_category(cbf,"diffrn_measurement"))

    cbf_failnez(cbf_require_column(cbf,"sample_detector_voffset"))

    cbf_failnez(cbf_set_doublevalue(cbf,"%g", voffset))
    
  }
  
  
    /* Oscillation start and range */

  if (!cbf_find_tag(minicbf,"_diffrn_scan_frame_axis.axis_id")) {
  
    char * axis;
    
    char oscaxis[40];
    
    double osc_start, osc_range;
  
    cbf_failnez(cbf_rewind_row(minicbf))
    
    cbf_failnez(cbf_find_row(minicbf,"GONIOMETER_SCAN"))
  
    axis = "PHI";
    
    cbf_failnez(cbf_find_column(minicbf,"angle"))
    
    cbf_failnez(cbf_get_doublevalue(minicbf,&osc_start))

    cbf_failnez(cbf_find_column(minicbf,"angle_increment"))
    
    cbf_failnez(cbf_get_doublevalue(minicbf,&osc_range))

    sprintf (oscaxis, "GONIOMETER_%s", axis);

    cbf_failnez (cbf_set_axis_setting (cbf, 0, oscaxis,
                                         osc_start, osc_range))
  }


  /* Beam Center */
  
  if (!cbf_find_tag(minicbf,"_diffrn_detector_element.reference_center_fast")){
  
    cbf_detector detector;
  
    const char * units;
    
    double bcx, bcy;
    
    cbf_failnez(cbf_rewind_row(minicbf))
    
    cbf_failnez(cbf_require_column(minicbf,"reference_center_fast"))
    
    cbf_failnez(cbf_get_doublevalue(minicbf,&bcx))
    
    cbf_failnez(cbf_require_column(minicbf,"reference_center_slow"))
    
    cbf_failnez(cbf_get_doublevalue(minicbf,&bcy))
    
    cbf_failnez(cbf_require_column(minicbf,"reference_center_units"))
    
    cbf_failnez(cbf_require_value(minicbf,&units,"mm"))
    
    if (!cbf_cistrcmp(units,"pixels")) {
    
      cbf_failnez(cbf_construct_detector (cbf, &detector, 0))
    
      cbf_failnez(cbf_set_beam_center(detector, &bcy, &bcx, NULL, NULL))
      
      cbf_failnez(cbf_free_detector(detector))
      
      if (dorefs) {

        cbf_failnez(cbf_require_reference_detector (cbf, &detector, 0))
    
        cbf_failnez(cbf_set_reference_beam_center(detector, &bcy, &bcx, NULL, NULL))
       
        cbf_failnez(cbf_free_detector(detector))
       
      }

    } else if (!cbf_cistrcmp(units,"mm")) {
    
      cbf_failnez(cbf_construct_detector (cbf, &detector, 0))
    
      cbf_failnez(cbf_set_beam_center(detector, NULL, NULL, &bcy, &bcx))
            
      cbf_failnez(cbf_free_detector(detector))
      
      if (dorefs) {

        cbf_failnez(cbf_require_reference_detector (cbf, &detector, 0))
    
        cbf_failnez(cbf_set_reference_beam_center(detector, NULL, NULL, &bcy, &bcx))
       
        cbf_failnez(cbf_free_detector(detector))
   
      }

    } else cbf_failnez(CBF_FORMAT)

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
    
    free (tempimg);

  }
  
  if (!unshort || (elsize==sizeof(short) && elunsigned ) ) {


    if (flat) {
  
      cbf_failnez (cbf_set_image (cbf, 0, 0, CBF_PACKED|CBF_FLAT_IMAGE,
                               image, elsize, elsigned, 
                               slowlen, fastlen))
  	
    } else {
    cbf_failnez (cbf_set_image (cbf, 0, 0, compression,
                               image, elsize, elsigned,
                               slowlen, fastlen))
    }
    
  } else {
  
    unsigned short *tempimg;
    
    int i;
    
    tempimg = malloc(dim1*dim2*sizeof(short));
    if (!tempimg) {
      fprintf(stderr,"\n unable to allocate temporary image array\n");
    }

    if (elsize == sizeof(short)){
      for (i = 0; i < dim1*dim2; i++) {
        short j;
        j = ((short *)image)[i];
        if (j < 0) j = 0;
        tempimg[i] = j;
      }
    } else {
      for (i = 0; i < dim1*dim2; i++) {
        int j;
        j = ((int *)image)[i];
        if (j < 0) j = 0;
        if (j > 0xFFFF) j = 0xFFFF;
        tempimg[i] = j;
      }
    }
    
    
    if (flat) {
  
      cbf_failnez (cbf_set_image (cbf, 0, 0, CBF_PACKED|CBF_FLAT_IMAGE,
                               image, sizeof(short), 0, 
                               slowlen, fastlen))
  	
    } else {
    cbf_failnez (cbf_set_image (cbf, 0, 0, compression,
                               image, sizeof(short), 0,
                               slowlen, fastlen))
    }

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
                               MSG_DIGEST | MIME_HEADERS | PAD_4K, 0))


    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))


    /* Free the minicbf */

  cbf_failnez (cbf_free_handle (minicbf))


    /* Success */

  if (cbfintmpused) {
    if (unlink(cbfintmp) != 0 ) {
      fprintf(stderr," convert_minicbf:  Can't unlink temporary file %s.\n", cbfintmp);
      fprintf(stderr,"%s\n",strerror(errno));
      exit(1);
    }

  }

  return 0;
}

int local_exit (int status)
{
  exit(status);
  return 1; /* avoid warnings */
}

