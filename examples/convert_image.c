/**********************************************************************
 * convert_image -- convert an image file to a cbf file               *
 *                                                                    *
 * Version 0.7.9 30 December 2007                                     *
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
 *  convert_image [-i input_img] [-o output_cbf] [-p template_cbf]\   *
 *    [-d detector name]  -m [x|y|x=y] [-z distance]              \   *
 *    [-c category_alias=category_root]*                          \   *
 *    [-t tag_alias=tag_root]* [-F] [-R] [-S ]                    \   *
 *    [input_img] [output_cbf]                                        *
 *                                                                    *
 *  the options are:                                                  *
 *                                                                    *
 *  -i input_img (default: stdin)                                     *
 *    the input file as an image in smv, mar300, or mar345  format.   *
 *    If input_img is not specified or is given as "-", it is copied  *
 *    from stdin to a temporary file.                                 *
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
 *  -S                                                                *
 *    when generating a copy of the img header in the .details field, *
 *    insert a space in front and before and after the equals sign    *
 *    for compatability with older versions of convert_image          *
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
#include "img.h"

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



double rint(double);
int local_exit (int status);
int outerror(int err);

int outerror(int err) 
{
	
  if ((err&CBF_FORMAT)==CBF_FORMAT)
    fprintf(stderr, " convert_image: The file format is invalid.\n");
  if ((err&CBF_ALLOC)==CBF_ALLOC)
    fprintf(stderr, " convert_image Memory allocation failed.\n");
  if ((err&CBF_ARGUMENT)==CBF_ARGUMENT)
    fprintf(stderr, " convert_image: Invalid function argument.\n");
  if ((err&CBF_ASCII)==CBF_ASCII)
    fprintf(stderr, " convert_image: The value is ASCII (not binary).\n");
  if ((err&CBF_BINARY)==CBF_BINARY)
    fprintf(stderr, " convert_image: The value is binary (not ASCII).\n");
  if ((err&CBF_BITCOUNT)==CBF_BITCOUNT)
    fprintf(stderr, " convert_image: The expected number of bits does" 
      " not match the actual number written.\n");
  if ((err&CBF_ENDOFDATA)==CBF_ENDOFDATA)
    fprintf(stderr, " convert_image: The end of the data was reached"
     " before the end of the array.\n");
  if ((err&CBF_FILECLOSE)==CBF_FILECLOSE)
    fprintf(stderr, " convert_image: File close error.\n");
  if ((err&CBF_FILEOPEN)==CBF_FILEOPEN)
    fprintf(stderr, " convert_image: File open error.\n");
  if ((err&CBF_FILEREAD)==CBF_FILEREAD)
    fprintf(stderr, " convert_image: File read error.\n");
  if ((err&CBF_FILESEEK)==CBF_FILESEEK)
    fprintf(stderr, " convert_image: File seek error.\n");
  if ((err&CBF_FILETELL)==CBF_FILETELL)
    fprintf(stderr, " convert_image: File tell error.\n");
  if ((err&CBF_FILEWRITE)==CBF_FILEWRITE)
    fprintf(stderr, " convert_image: File write error.\n");
  if ((err&CBF_IDENTICAL)==CBF_IDENTICAL)
    fprintf(stderr, " convert_image: A data block with the new name already exists.\n");
  if ((err&CBF_NOTFOUND)==CBF_NOTFOUND)
    fprintf(stderr, " convert_image: The data block, category, column or"
      " row does not exist.\n");
  if ((err&CBF_OVERFLOW)==CBF_OVERFLOW)
    fprintf(stderr, " convert_image: The number read cannot fit into the"
      "destination argument.\n        The destination has been set to the nearest value.\n");
  if ((err& CBF_UNDEFINED)==CBF_UNDEFINED)
    fprintf(stderr, " convert_image: The requested number is not defined (e.g. 0/0).\n");
  if ((err&CBF_NOTIMPLEMENTED)==CBF_NOTIMPLEMENTED)
    fprintf(stderr, " convert_image: The requested functionality is not yet implemented.\n");
  return 0;

}


#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr," convert_image: CBFlib fatal error %d\n",err); \
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
 fprintf(stderr,"  convert_image [-i input_img] [-o output_cbf] [-p template_cbf]\\\n");
 fprintf(stderr,"    [-d detector name] -m [x|y|x=y] [-z distance] \\\n");
 fprintf(stderr,"    [-c category_alias=category_root]* \\\n");
 fprintf(stderr,"    [-t tag_alias=tag_root]* [-F] [-R] [-S]\\\n");
 fprintf(stderr,"    [input_img] [output_cbf]\n");

 fprintf(stderr,"  the options are:\n");

 fprintf(stderr,"  -i input_img (default: stdin)\n");
 fprintf(stderr,"    the input file as an image in smv, mar300, or mar345  format.\n");
 fprintf(stderr,"    If input_img is not specified or is given as \"-\", it is copied\n");
 fprintf(stderr,"    from stdin to a temporary file.\n");

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

 fprintf(stderr,"  -R\n");
 fprintf(stderr,"    if setting a beam center, set reference values of\n");
 fprintf(stderr,"    axis settings as well as standard settings\n");
 
 fprintf(stderr,"  -S\n");
 fprintf(stderr,"    when generating a copy of the img header in the .details field,\n");
 fprintf(stderr,"    insert a space in front and before and after the equals sign\n");
 fprintf(stderr,"    for compatability with older versions of convert_image\n");


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
  FILE *in, *out;

  img_handle img;

  cbf_handle cbf;

  char detector_type [64], template_name [256], oscaxis [20], *c;

  const char *detector_name, *detector_opt, *beam_center, *pixel_size, 
    *axis, *array_id, *binning;

  char *header_info;

  double wavelength, distance, osc_start, osc_range, time, bcx, bcy, 
    psx, psy, binx, biny;

  size_t header_info_size;
  
  int dorefs;

  const char *date;

  static const char *monthname [] =

        { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

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
  char * imgtmp=NULL;
  int imgtmpused = 0;
  char *imgin, *cbfout, *template, *distancestr;
  cbf_detector detector;
  char *tag, *data, *alias, *root;
  char xalias[81];
  int index;
  int transpose;
  int fastlen, slowlen;
  int flat;
  int sequal;

    /* Usage */

  imgin = NULL;
  cbfout = NULL;
  template = NULL;
  detector_opt = NULL;
  transpose = 0;
  distancestr = NULL;
  dorefs = 0;
  flat = 0;
  sequal = 0;
  
  cbf_failnez (cbf_make_handle (&cbf))

  while ((copt = getopt(argc,argv, "FRSi:o:p:d:m:r:z:c:t:")) != EOF) {

    switch(copt) {
      case 'i':
         if (imgin) errflg++;
         else imgin = optarg;
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

      case 'S':
         if (sequal) errflg++;
         sequal = 1;
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
    if (!imgin) {
      imgin = argv[optind];
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


  if (!imgin || strcmp(imgin?imgin:"","-") == 0) {
     imgtmp = (char *)malloc(strlen("/tmp/cvt_imgXXXXXX")+1);
     strcpy(imgtmp, "/tmp/cvt_imgXXXXXX");
     if ((imgin = mktemp(imgtmp)) == NULL ) {
       fprintf(stderr,"\n convert_image: Can't create temporary file name %s.\n", imgtmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }
     imgtmpused = 1;
  }

    /* Read the image */

  img = img_make_handle ();

  cbf_failnez (img_read (img, imgin))

  if (imgtmpused)
  {
       if (unlink(imgtmp) != 0 ) {
       fprintf(stderr," convert_image:  Can't unlink temporary file %s.\n", imgtmp);
       fprintf(stderr,"%s\n",strerror(errno));
       exit(1);
     }

  }


    /* Identify the detector */

  detector_name = img_get_field (img, "DETECTOR");

  if (!detector_name || !strcmp(detector_name,"(null)")) {

    if (detector_opt == NULL) {

      fprintf (stderr, "\n convert_inage: No detector name provided in image or on the command line!");
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
                             img_columns (img),
                             img_rows (img));

    fprintf(stderr," convert_image: template_name: %s\n", template_name);

    /* Read and modify the template */

    in = fopen (template_name, "rb");

  }
  
  if (!in) {
    fprintf (stderr," convert_image: unable to open template_name: %s\n", 
      template?template:template_name);
    
    exit (4);

  }

  cbf_failnez (cbf_read_template (cbf, in))


    /* report the header */

  header_info_size = 0;

  for (index = 0; !img_get_next_field(img,(const char **) &tag, (const char **) &data, &index);)
  {
      if (tag && data) {

      header_info_size += (strlen(tag) + strlen(data)+4+sequal*3);

      } else {

      if (tag && !data) {

        header_info_size += (strlen(tag) +2+sequal);

      }

    }
  }

  header_info_size+=2;

  cbf_failnez((header_info = malloc(sizeof(char)*header_info_size))==NULL?CBF_ALLOC:0)

  *header_info = '\0' ; header_info_size = 0;

  for (index = 0; !img_get_next_field(img,(const char **) &tag, (const char **) &data, &index);)
  {
      if (tag && data) {
      
        if (sequal)  {

          sprintf (header_info+header_info_size, "\n %s = %s;", tag, data);

          header_info_size += (strlen(tag) + strlen(data)+6);
      	
        } else {

          sprintf (header_info+header_info_size, "\n%s=%s;", tag, data);

          header_info_size += (strlen(tag) + strlen(data)+3);
        	
        }


      } else {

      if (tag && !data) {

        if (sequal)  {

          sprintf (header_info+header_info_size, " %s;\n", tag);

          header_info_size += (strlen(tag) +3);
        
        } else {

          sprintf (header_info+header_info_size, "%s;\n", tag);

          header_info_size += (strlen(tag) +2);
        	
        }

      }

    }
  }

  cbf_failnez(cbf_get_array_id(cbf, 0, &array_id))

  cbf_failnez(cbf_require_column(cbf, "details"))

  cbf_failnez(cbf_set_value(cbf, header_info))

  cbf_failnez(cbf_set_typeofvalue(cbf,"text"))


    /* Wavelength */

  wavelength = img_get_number (img, "WAVELENGTH");

  if (wavelength)

    cbf_failnez (cbf_set_wavelength (cbf, wavelength))


    /* Distance */

  distance = img_get_number (img, "DISTANCE");

  if (distance == 0.) {

    distance = atof (distancestr);

  }

  cbf_failnez (cbf_set_axis_setting (cbf, 0, "DETECTOR_Z", distance, 0))

  cbf_failnez(cbf_require_category(cbf,"diffrn_measurement"))

  cbf_failnez(cbf_require_column(cbf,"sample_detector_distance"))

  cbf_failnez(cbf_set_doublevalue(cbf,"%g", distance))



    /* Oscillation start and range */

  axis = img_get_field (img, "OSCILLATION AXIS");

  if (!axis)

    axis = "PHI";

  if (img_get_field(img, "OSC_START")) osc_start = img_get_number (img, "OSC_START");

  else  osc_start = img_get_number (img, axis);

  if (img_get_field(img, "OSC_RANGE")) osc_range = img_get_number (img, "OSC_RANGE");

  else osc_range = img_get_number (img, "OSCILLATION RANGE");

  sprintf (oscaxis, "GONIOMETER_%s", axis);

  cbf_failnez (cbf_set_axis_setting (cbf, 0, oscaxis,
                                         osc_start, osc_range))


    /* Exposure time */

  time = img_get_number (img, "EXPOSURE TIME");

  if (time)

    cbf_failnez (cbf_set_integration_time (cbf, 0, time))


    /* Date stamp */

  date = img_get_field (img, "DATE");

  if (date)
  {
    char monthstring [16];

    int month, day, hour, minute, year;
    
    double second;

    year = 0;

    sscanf (date, "%*s %s %d %d:%d:%lf %d", monthstring,
                   &day, &hour, &minute, &second, &year);

    if (year != 0)
    {
      for (month = 0; month < 12; month++)

        if (strcmp (monthname [month], monthstring) == 0)

          break;

      month++;

      if (month <= 12)

        cbf_failnez (cbf_set_datestamp (cbf, 0, year, month, day,
                                        hour, minute, second,
                                        CBF_NOTIMEZONE, 0))
    }
  }


    /* diffrn.id */

  cbf_failnez (cbf_set_diffrn_id (cbf, "DS1"))


    /* Image */
    
  if (img->rowmajor) {
    fastlen = img_columns(img);
    slowlen = img_rows(img);
  } else  {      
    fastlen = img_rows(img);
    slowlen = img_columns(img);
  }


  if (overall.posxtarg != posx || overall.posytarg != posy || img->rowmajor)
  { int fastorig, faststep, sloworig, slowstep, curpos, i, j;

    int * tempimg;

      if (overall.posxtarg==0 || overall.posytarg==0) {
          fprintf (stderr,"\n convert_image: invalid image transform.\n");
          exit(1);
      }

      if (img_rows(img) != img_columns(img) ) {
      fprintf(stderr,"\n convert_img: Unable to transpose image\n");
      exit(-1);
    }

    /* if in row major order, the fast index is the x axis, counting
       the columns, and the slow index is the y axis, counting the
       rows */
       
    if (img->rowmajor)  {
 
      fastorig = sloworig = 0;
      faststep = 1;
      slowstep = img_columns(img);

      switch (overall.posxtarg) {
        case (posx): break;
        case (negx): fastorig = img_columns(img)-1; faststep = -1; break;
        case (posy): faststep = img_columns(img); 
                     fastlen = img_rows(img);
                     slowlen = img_columns(img); break;
        case (negy): fastorig = (img_columns(img))*(img_rows(img)-1);
                     faststep = -img_columns(img);
                     fastlen = img_rows(img);
                     slowlen = img_columns(img); break;
      }
      switch (overall.posytarg) {
        case (posx): slowstep = 1;
                     fastlen = img_rows(img);
                     slowlen = img_columns(img); break;
        case (negx): sloworig = img_columns(img)-1; slowstep= -1;
                     fastlen = img_rows(img);
                     slowlen = img_columns(img); break;
        case (posy): break;
        case (negy): sloworig = img_columns(img)*(img_rows(img)-1);
                     slowstep = -img_columns(img); break;
      }
      
    } else {

      fastorig = sloworig = 0;
      faststep = 1;
      slowstep = img_rows(img);

      switch (overall.posxtarg) {
        case (posx): break;
      	case (negx): sloworig = img_rows(img)*(img_columns(img)-1);
                     slowstep = -img_rows(img); break;
        case (posy): slowstep = 1;
                     fastlen = img_columns(img);
                     slowlen = img_rows(img); break;
        case (negy): sloworig = img_rows(img)-1; slowstep = -1; 
                     fastlen = img_columns(img);
                     slowlen = img_rows(img); break;
                     break;
      }
      switch (overall.posytarg) {
        case (posx): faststep = img_rows(img);
                     fastlen = img_columns(img);
                     slowlen = img_rows(img); break;
        case (negx): fastorig = (img_rows(img))*(img_columns(img)-1);
                     faststep = -img_rows(img);
                     fastlen = img_columns(img);
                     slowlen = img_rows(img); break;
        case (posy): break;
        case (negy): fastorig = img_rows(img)-1; faststep = -1; break;
      }

    	
    }

    curpos = fastorig+sloworig;

    tempimg = malloc(img_columns(img)*img_rows(img)*sizeof(int));
    if (!tempimg) {
      fprintf(stderr,"\n unable to allocate temporary image array\n");
    }

    for (i=0;i<fastlen;i++) {
      curpos = fastorig+i*faststep+sloworig;
      for (j=0; j<slowlen;j++) {
        *((int *)tempimg+curpos) = *((img->image)+i+j*fastlen);
       /*  *((int *)tempimg+curpos) = img_pixel(img, j, i); */
        curpos = curpos+slowstep;
      }
    }

    for (i=0;i<fastlen;i++) {
      for (j=0; j<slowlen;j++) {
        *(int *)((img->image)+i+j*fastlen) = *((int *)tempimg+i+j*fastlen);
      }
    }


  }


  if (flat) {
  
  cbf_failnez (cbf_set_image (cbf, 0, 0, CBF_PACKED|CBF_FLAT_IMAGE,
                               img->image, sizeof (int), 1, 
                               slowlen, fastlen))
  	
  } else {
  cbf_failnez (cbf_set_image (cbf, 0, 0, CBF_PACKED,
                               img->image, sizeof (int), 1,
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

 /* binning */
  
  binx = biny = 0.0;
  
  if ((binning = img_get_field(img,"BIN"))) {
  
    char *endptr;
    
    biny = binx = strtod (binning, &endptr);
    
    if (*endptr && *(endptr+1)) biny = strtod (endptr+1, &endptr);
    
    if (binx <= 0.0 || biny <= 0.0) {
    
      fprintf(stderr," Bad bin values %g x %g ignored\n", binx, biny);
    	
    } else {
    
      cbf_failnez(cbf_set_bin_sizes(cbf, 0, binx, biny))
    	
    }
  	
  }


 
  /* beam center and pixel size */

  bcx = bcy = psx = psy = 0.0;

  if ((pixel_size = img_get_field(img,"PIXEL SIZE")) ||
    (pixel_size = img_get_field(img,"PIXEL_SIZE")) ) {

    char *endptr;

    psy = psx = strtod (pixel_size, &endptr);

    if (*endptr) psy = strtod (endptr, &endptr);

  }

  if ((beam_center = img_get_field(img,"BEAM CENTRE")) ) {

    char *endptr;

    bcx = strtod (beam_center, &endptr);

    if (*endptr) bcy = strtod (endptr, &endptr);

    if (psx) bcx /= psx;

    if (psy) bcy /= psy;

    bcx = .5*rint(2.*bcx);

    bcy = .5*rint(2.*bcy);

  }

  if ((beam_center = img_get_field(img,"CENTER")) ) {

    char *endptr;

    endptr = strstr(beam_center,"X ");

    bcx = strtod (endptr+2, &endptr);

    if (*endptr) {

      endptr = strstr(endptr,"Y ");

      bcy = strtod (endptr+2, &endptr);

    }
  }

  if (overall.posxtarg != posx || overall.posytarg != posy) {

    double obcx = bcx, obcy = bcy, opsx = psx, opsy = psy;

    switch (overall.posxtarg) {
      case (posx): break;
      case (negx): bcx = img_columns(img)-1-obcx; break;
      case (posy): bcx = obcy; psx = opsy; break;
      case (negy): bcx = img_rows(img)-1-obcy; psx = opsy; break;
    }
    switch (overall.posytarg) {
      case (posx): bcy = obcx; psy = opsx; break;
      case (negx): bcy = img_columns(img)-1-obcx; psy = opsx; break;
      case (posy): bcy = obcy; break;
      case (negy): bcy = img_rows(img)-1-obcy; break;
    }
  }

  cbf_failnez (cbf_set_pixel_size (cbf, 0, 1, psx))

  cbf_failnez (cbf_set_pixel_size (cbf, 0, 2, psy))

  /* fprintf(stderr, "header pixel center indices: %g %g\n",bcx, bcy); */

  cbf_failnez(cbf_construct_detector (cbf, &detector, 0))

  cbf_failnez(cbf_set_beam_center(detector,&bcx,&bcy,NULL,NULL))

  cbf_failnez(cbf_free_detector(detector))
  
  if (dorefs) {
  	
    cbf_failnez(cbf_require_reference_detector (cbf, &detector, 0))

    cbf_failnez(cbf_set_reference_beam_center(detector,&bcx,&bcy,NULL,NULL))

    cbf_failnez(cbf_free_detector(detector))
  
  }
  




/*****************************************************************************/

  {
      const char *id;

      /* double d [4]; */

      /* int i [4]; */

      /* unsigned int u [4]; */

      /* size_t s [4]; */

      /* double cell[6], cell_esd[6], rcell[6], rcell_esd[6]; */

      cbf_goniometer goniometer;


  /* Change the diffrn.id entry in all the categories */

   /*  cbf_set_diffrn_id (cbf, "TEST"); */


  /* Get the diffrn.id entry */

    /* cbf_get_diffrn_id (cbf, &id); */


  /* Change the diffrn.crystal_id entry */

    /* cbf_set_crystal_id (cbf, "CTEST"); */


  /* Get the diffrn.crystal_id entry */

    cbf_get_crystal_id (cbf, &id);

  /* Test the cell functions */

    /* cell[0]      = cell[1]      = cell[2]      = cell[3]      = cell[4]      = cell[5]      = 0.;
    cell_esd[0]  = cell_esd[1]  = cell_esd[2]  = cell_esd[3]  = cell_esd[4]  = cell_esd[5]  = 0.;
    rcell[0]     = rcell[1]     = rcell[2]     = rcell[3]     = rcell[4]     = rcell[5]     = 0.;
    rcell_esd[0] = rcell_esd[1] = rcell_esd[2] = rcell_esd[3] = rcell_esd[4] = rcell_esd[5] = 0.;

    if (cbf_get_unit_cell(cbf, cell, cell_esd) || cbf_get_reciprocal_cell(cbf, rcell, rcell_esd)) {

      fprintf(stdout," No cell in the template, putting in rcell, no cell\n");

      rcell[0]=rcell[1]=rcell[2]=1.;
      rcell[3]=rcell[4]=rcell[5]=90.;
      rcell_esd[0]=rcell_esd[1]=rcell_esd[2]=0.;
      rcell_esd[3]=rcell_esd[4]=rcell_esd[5]=0.;

      cbf_failnez(cbf_set_reciprocal_cell(cbf,rcell,rcell_esd))

    }

    cbf_failnez(cbf_get_unit_cell(cbf,cell,cell_esd))
    cbf_failnez(cbf_get_reciprocal_cell(cbf,rcell,rcell_esd))

    fprintf(stdout," Starting cell, rcell:\n {%g,%g,%g,%g,%g,%g} {%g,%g,%g,%g,%g,%g}\n",
            cell[0], cell[1], cell[2], cell[3], cell[4], cell[5],
            rcell[0], rcell[1], rcell[2], rcell[3], rcell[4], rcell[5]);

    cell[0]     = 85.;     cell[1]     = 90.;     cell[2]     = 95.;
    cell_esd[0] = .05;     cell_esd[1] = .035;    cell_esd[2] = .15;
    cell[3]     = 78.13;   cell[4]     = 103.12;  cell[5]     = 101.48;
    cell_esd[3] = .055;    cell_esd[4] = .065;    cell_esd[5] = .11;

     cbf_failnez(cbf_compute_reciprocal_cell(cell,rcell))

     cbf_failnez(cbf_set_unit_cell(cbf,cell,cell_esd))
     cbf_failnez(cbf_set_reciprocal_cell(cbf,rcell,rcell_esd))

    cell[0]      = cell[1]      = cell[2]      = cell[3]      = cell[4]      = cell[5]      = 0.;
    cell_esd[0]  = cell_esd[1]  = cell_esd[2]  = cell_esd[3]  = cell_esd[4]  = cell_esd[5]  = 0.;
    rcell[0]     = rcell[1]     = rcell[2]     = rcell[3]     = rcell[4]     = rcell[5]     = 0.;
    rcell_esd[0] = rcell_esd[1] = rcell_esd[2] = rcell_esd[3] = rcell_esd[4] = rcell_esd[5] = 0.;

    cbf_failnez(cbf_get_unit_cell       (cbf,cell,cell_esd))
    cbf_failnez(cbf_get_reciprocal_cell (cbf,rcell,rcell_esd))

    fprintf(stdout," Final cell, rcell:\n {%g,%g,%g,%g,%g,%g} {%g,%g,%g,%g,%g,%g}\n",
            cell[0], cell[1], cell[2], cell[3], cell[4], cell[5],
            rcell[0], rcell[1], rcell[2], rcell[3], rcell[4], rcell[5]); */


  /* Set the wavelength */

    /* cbf_set_wavelength (cbf, 2.14); */


  /* Get the wavelength */

    /* cbf_get_wavelength (cbf, &wavelength); */


  /* Set the polarization */

    /* cbf_set_polarization (cbf, 0.5, 0.75); */


  /* Get the polarization */

    /* cbf_get_polarization (cbf, &d [0], &d [1]); */


  /* Set the divergence */

    /* cbf_set_divergence (cbf, 0.3, 0.4, 0.5); */


  /* Get the divergence */

    /* cbf_get_divergence (cbf, &d [0], &d [1], &d [2]); */


  /* Get the number of elements */

   /*  cbf_count_elements (cbf, &u [0]); */


  /* Get the element id */

    cbf_get_element_id (cbf, 0, &id);
    /* fprintf(stdout," Element ID: %s\n", id); */


  /* Set the gain of a detector element */

    /* cbf_set_gain (cbf, 0, 0.24, 0.04); */


  /* Get the gain of a detector element */

    /* cbf_get_gain (cbf, 0, &d [0], &d [1]); */


  /* Set the overload value of a detector element */

    /* cbf_set_overload (cbf, 0, 100000); */


  /* Get the overload value of a detector element */

    /* cbf_get_overload (cbf, 0, &d [0]); */


  /* Set the integration time */

    /* cbf_set_integration_time (cbf, 0, 10.1); */


  /* Get the integration time */

    /* cbf_get_integration_time (cbf, 0, &d [0]); */


  /* Set the collection date and time (1) as seconds since January 1 1970 */

    /* cbf_set_timestamp (cbf, 0, 1000.0, CBF_NOTIMEZONE, 0.1); */


  /* Get the collection date and time (1) as seconds since January 1 1970 */

    /* cbf_get_timestamp (cbf, 0, &d [0], &i [0]); */


  /* Get the image size */

    /* cbf_get_image_size (cbf, 0, 0, &s [0], &s [1]); */


  /* Change the setting of an axis */

    /* cbf_set_axis_setting (cbf, 0, "GONIOMETER_PHI", 27.0, 0.5); */

  /* Get the setting of an axis */

    /* cbf_get_axis_setting (cbf, 0, "GONIOMETER_PHI", &d [0], &d [1]); */


  /* Construct a goniometer */

    cbf_failnez(cbf_construct_goniometer (cbf, &goniometer))


  /* Get the rotation axis */

    /* cbf_get_rotation_axis (goniometer, 0, &d [0], &d [1], &d [2]); */


  /* Get the rotation range */

    /* cbf_get_rotation_range (goniometer, 0, &d [0], &d [1]); */


  /* Reorient a vector */

    /* cbf_rotate_vector (goniometer, 0, 0.5, 0.3, 0, 1, &d [0], &d [1], &d [2]); */


  /* Convert a vector to reciprocal space */

    /* cbf_get_reciprocal (goniometer, 0, 0.3, 0.98, 1, 2, -3, &d [0], &d [1], &d [2]); */


  /* Construct a detector positioner */

    /* cbf_failnez(cbf_construct_detector (cbf, &detector, 0)); */


  /* Get the beam center */

     /* cbf_get_beam_center (detector, &d [0], &d [1], &d [2], &d [3]); 

      fprintf(stderr," convert_image: beam center:  %g %g %g %g\n", d[0], d[1], d[2], d[3]); */


  /* Get the detector distance */

     /*  cbf_get_detector_distance (detector, &d [0]);       */
     /* fprintf(stdout, " detector distance: %-15g\n",d[0]); */

  /* Get the detector normal */

    /* cbf_get_detector_normal (detector, &d [0], &d [1], &d [2]); */


  /* Calcluate the coordinates of a pixel */

     /* cbf_get_pixel_coordinates (detector, 1, 3, &d [0], &d [1], &d [2]); */


  /* Calcluate the area of a pixel */

    /* cbf_get_pixel_area (detector, 1, 3, &d [0], &d [1]);
    fprintf(stdout, " Pixel area, projected area at pixel(3,1): %-15g, %-15g\n",d[0], d[1]);

    cbf_get_pixel_area_fs(detector, 12, 25, &d [0], &d [1]);
    fprintf(stdout, " Pixel area, projected area at  pixel(12,25): %-15g, %-15g\n",d[0], d[1]); */


  /* Calculate the dimensions of a pixel */

    /* cbf_failnez (cbf_get_inferred_pixel_size (detector, 1, &d [0]))
    cbf_failnez (cbf_get_inferred_pixel_size (detector, 2, &d [1]))

    fprintf(stdout, " Template detector size: %-15g x %-15g \n", d[0], d[1]);
    
    cbf_failnez (cbf_get_inferred_pixel_size_sf(detector, 1, &d [0]))
    cbf_failnez (cbf_get_inferred_pixel_size_sf(detector, 2, &d [1]))

    fprintf(stdout, " Inferred detector size (sf) : %-15g x %-15g \n", d[0], d[1]);

    cbf_failnez (cbf_set_pixel_size (cbf, 0, 1, d [0]))
    cbf_failnez (cbf_set_pixel_size (cbf, 0, 2, d [1]))

    cbf_failnez (cbf_get_pixel_size_fs(cbf, 0, 1, &d [2]))
    cbf_failnez (cbf_get_pixel_size_fs(cbf, 0, 2, &d [3]))

    fprintf(stdout, " Array element size (fs):  %-15g x %-15g \n", d[2], d[3]);

    cbf_failnez (cbf_set_pixel_size_sf(cbf, 0, 1, d [1]))
    cbf_failnez (cbf_set_pixel_size_sf(cbf, 0, 2, d [0]))

    cbf_failnez (cbf_get_pixel_size_sf(cbf, 0, 1, &d [2]))
    cbf_failnez (cbf_get_pixel_size_sf(cbf, 0, 2, &d [3]))

    fprintf(stdout, " Array element size (sf):  %-15g x %-15g \n", d[2], d[3]); */

  /* Get the bin sizes */
  
    /*if(cbf_get_bin_sizes(cbf,0,&d[0],&d[1])) 
    {
    	fprintf (stdout," Pixel bin sizes not specified \n");
    } else {
    	fprintf(stdout, " Pixel bin sizes %-.15g x %-.15g \n", d[0], d[1]);
    }
    */

  /* Free a detector */

    /* cbf_free_detector (detector); */


  /* Free a goniometer */

    /* cbf_free_goniometer (goniometer);*/
    }


/*****************************************************************************/


    /* Write the new file */

  out = stdout;

  if (cbfout && strcmp(cbfout,"-"))out = fopen (cbfout, "w+b");

  if (!out)
  {
    fprintf (stderr, " convert_image:  Couldn't open the CBF file %s\n", cbfout);

    exit (1);
  }

  cbf_failnez (cbf_write_file (cbf, out, (cbfout && strcmp(cbfout,"-"))?1:0, CBF,
                               MSG_DIGEST | MIME_HEADERS, 0))


    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))


    /* Free the image */

  img_free_handle (img);


    /* Success */

  return 0;
}

int local_exit (int status)
{
  exit(status);
  return 1; /* avoid warnings */
}

