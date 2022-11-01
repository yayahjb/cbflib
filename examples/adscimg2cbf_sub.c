/**********************************************************************
 * adscimg2cbf -- convert an ADSC SMV image file to a cbf file        *
 *                                                                    *
 * Chris Nielsen, 5 December 2007                                     *
 * ADSC                                                               *
 *                                                                    *
 * based on img2cbf from                                              *
 * CBFlib Version 0.7.6 28 June 2006                                  *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006, 2008 Herbert J. Bernstein                      *
 **********************************************************************/

/**********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE CBFLIB PACKAGE UNDER THE TERMS OF THE GPL *
 * WHILE YOU MAY ALTERNATIVE DISTRIBUTE THE API UNDER THE LGPL        *
 * YOU MAY ***NOT*** DISTRBUTE THIS PROGRAM UNDER THE LGPL            *
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

#include "cbf.h"
#include "cbf_simple.h"
#include "cbf_minicbf_header.h"
#include "cbf_string.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

int local_exit(int status);

int local_exit(int status) {
  exit(status);
  return status;    /* to avoid warning messages */
}

#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr,"\nCBFlib fatal error %x \n",err); \
    local_exit(-1); \
  } \
 }

/****************************************************************/

/*
 *     GETBO  -  Return the byte-order of the computer.
 *
 *        0 if little-endian
 *        1 if big-endian
 *        2 if unknown-endian
 *
 *  14-Sep-1994      Marty Stanton       Brandeis University
 *
 */
/* Commented out 19 June 2008 because of optimization bug in gcc
   -- HJB
int getbo()
{
  long i4;
  long *pi4;
  short *i2;

  i4=1;
  pi4 = &i4;
  i2 = (short *) pi4;

  if ( *i2 == 1 && *(i2+1) == 0 )
    return (0);
  else if ( *i2 == 0 && *(i2+1) == 1 )
    return (1);
  else
    return(2);
}
*/

static void	short_swap(unsigned short  *p,int     n)
{
        register int            i,j;
        register unsigned short *q;

        for(i = 0, q = p; i < n;i++,q++)
        {
            j = *q;
            *q = ((j << 8 ) | (j >> 8)) & 0x0000ffff;
        }
}

static void gethd ( char* field, char* value, char* header )
{
  char *hp, *lhp, *fp, *vp;
  int l, j;
  char *newfield;

  /*
   * Find the last occurance of "field" in "header"
   */
  l = strlen (field);
  newfield = (char*) malloc ( l + 3 );
  *newfield = 10;
  strncpy (newfield+1, field, l);
  *(newfield+l+1) = '=';
  *(newfield+l+2) = (char) 0;
  l += 2;

  lhp = 0;
  for (hp=header; *hp != '}'; hp++)
    {
      for (fp=newfield, j=0; j<l && *(hp+j) == *fp; fp++, j++);
      if ( j == l ) lhp=hp;
    }

  if ( lhp == 0 )
    value[0] = 0;
  else
    {
      /*
       * Find the '='.  hp will now point to the '='
       */
      for (hp=lhp; *hp!='='; hp++);
      
      /*
       * Copy into the returned value until the ';'
       */
      for (lhp=hp+1, vp=value; *lhp!=';' && *lhp!=0; lhp++, vp++) *vp = *lhp;
      *(vp++)=0;
    }
  free (newfield);
}

/*
 *	Return the 'n'th header item in field and value from header header
 *	returning 1 if there is an 'n'th item, else 0.
 */

static int gethdn ( int n, char* field, char* value, char* header )
{
  char *hp, *sp;
  int i;

  /*
   * Find the nth occurance of a ";"
   */
  sp = header;
  for (hp=header, i = -1; *hp != '}' && i<n; hp++)
    if ( *hp == ';' ) 
      {
	i++;
	if ( i==(n-1) ) sp=hp;
      }
  /*
   * Return if couldn't find nth field
   */
  if ( i<n ) 
    {
      field[0]=value[0]=0;
      return 0;
    }

  /*
   * Copy the field string 
   */
  for (hp=sp+2; *hp!='='; field++, hp++) *field = *hp;
  *field = 0;
  /*
   * Copy the value string 
   */
  for (hp++; *hp!=';'; value++, hp++) *value = *hp;
  *value = 0;

  return 1;
}

static char	*which_facility(int serial_number)
{
	switch(serial_number)
	{
		case 901:
		case 902:
		case 908:
			return("ssrl");
			break;
		case 903:
		case 906:
			return("nsls");
			break;	
		case 904:
		case 911:
		case 916:
			return("necat");
			break;
		case 907:
		case 926:
		case 925:
		case 923:
		case 913:
			return("als");
			break;
		case 912:
			return("kek");
			break;
		case 914:
			return("sbccat");
			break;
		case 910:
		case 905:
			return("biocars");
			break;
		case 909:
			return("nsrrc");
			break;
		case 915:
			return("spring8");
			break;
		case 917:
		case 918:
		case 919:
			return("esrf");
			break;
		case 920:
		case 921:
		case 922:
			return("dls");
			break;
		case 924:
			return("fip");
			break;
		case 927:	
			return("soleil");
			break;
                case 457:
		case 928:
			return("as_mx1");
			break;
		case 929:
			return("pohang");
			break;
		default:
			return(" ");
			break;
	}
}
static void	smv_date_to_cbf_date(char *smv_date, char *cbf_date)
{
    char 	monthstring [16];
    int 	month, day, hour, minute, year;
    double 	second;
    int		nsf;
    static const char *monthname [] =
        { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };


    year = 0;

    sscanf (smv_date, "%*s %s %d %d:%d:%lf %d", monthstring,
                   &day, &hour, &minute, &second, &year);

    for (month = 0; month < 12; month++)
        if (strcmp (monthname [month], monthstring) == 0)
          break;

      month++;

      nsf = 0;

      sprintf (cbf_date, "%04d-%02d-%02dT%02d:%02d:%0*.*f", year, month, day,
                   hour, minute, nsf == 0 ? 2 : nsf + 3, nsf, second);

}

int convertroi(char *roi, int fastdim, int slowdim,
               int * fastlow, int * fasthigh,
               int * slowlow, int * slowhigh) {
    char * endptr;
    char * str;
    if (!fastlow || !fasthigh || !slowlow || !slowhigh) return CBF_ARGUMENT;
    *fastlow = *slowlow = 0;
    *fasthigh = fastdim-1;
    *slowhigh = slowdim-1;
    if (!roi) {
      return CBF_SUCCESS;
    }
    str = roi;
    *fastlow = (int)strtol(str,&endptr,0);
    if (*fastlow < 0) *fastlow = 0;
    if (*fastlow > fastdim-1) *fastlow = fastdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',') return CBF_FORMAT;
    str = endptr+1;
    *fasthigh = (int)strtol(str,&endptr,0);
    if (*fasthigh < *fastlow) *fasthigh = *fastlow;
    if (*fasthigh > fastdim-1) *fasthigh = fastdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',') return CBF_FORMAT;
    str = endptr+1;
    *slowlow = (int)strtol(str,&endptr,0);
    if (*slowlow < 0) *slowlow = 0;
    if (*slowlow > slowdim-1) *slowlow = slowdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    if (*endptr != ',') return CBF_FORMAT;
    str = endptr+1;
    *slowhigh = (int)strtol(str,&endptr,0);
    if (*slowhigh < *slowlow) *slowhigh = *slowlow;
    if (*slowhigh > slowdim-1) *slowhigh = slowdim-1;
    if (*endptr == '\0') return CBF_SUCCESS;
    return CBF_FORMAT;
}



#define	BEAM_CENTER_FROM_HEADER	0
#define BEAM_CENTER_MOSFLM	1
#define	BEAM_CENTER_ULHC	2
#define BEAM_CENTER_LLHC	3

int	adscimg2cbf_sub2(char *header,
                     unsigned short *data,
                     int * old_int_data,
                     size_t old_int_data_size,
                     int * * new_int_data,
                     size_t * new_int_data_size,
                     char *cbf_filename,
                     int pack_flags,
                     int beam_center_convention,
                     int pad_flag,
                     const char *roi,
                     const char *thickstr,
                     int add_minicbf_header,
                     const char * polarstr,
                     int bin,
                     const char * scalestr,
                     const char * offsetstr,
                     const char * cliplowstr,
                     const char * cliphighstr,
                     const char * rad_smoothstr,
                     int transpose,
                     int interleave,
                     int reverse)
{
    FILE *out;
    
    clock_t a,b;
    
    cbf_handle cbf;
    
    double pixel_size, gain, wavelength, distance;
    
    int overload, dimension [2], precedence [2];
    
    
    char detector_id [64], detector_type[64];
    
    const char *direction [2];
    
    char	s[1024], s1[1024], facility[256], temp[1024];
    char  	detector_mode[40];
    char	smv_date[100], cbf_date[100];
    int   	smv_size1, smv_size2;
    int     fastlow, fasthigh, slowlow, slowhigh;
    int     fastpadlow, fastpadhigh, slowpadlow, slowpadhigh;
    int		smv_bin, smv_adc, smv_bin_type;
    double	smv_time;
    int		detector_sn;
    int		*data_as_int;
    int     *smooth_data_as_int;
    int		*ip;
    unsigned short *up;
    int		i, j;
    int     ki, kj, kicm, kjcm;
    double  cmi, cmj, cmv;
    char *  local_bo;
    int		this_bo, smv_bo;
    char *  header_as_details;
    int		header_size;
    double	oscillation_start, oscillation_range;
    double	detector_center_x, detector_center_y;
    double	det_beam_center_to_origin_dist_x, det_beam_center_to_origin_dist_y;
    double	header_beam_x, header_beam_y;
    double  thickness;
    double  polarization_ratio;
    double  scale;
    double  offset;
    double  cliplow, cliphigh;
    int     rad_smooth;
    
    CBF_UNUSED( smv_bin_type);
    CBF_UNUSED( kicm );
    CBF_UNUSED( kjcm );
 
    if(0 == pack_flags)
        pack_flags = CBF_BYTE_OFFSET;	/* default if 0 is given for "pack_flags" */
    
    /* Get some detector parameters */
    
    /* Detector identifier */
    
    /*
     *	Figure out which detector it is.
     *
     *	Newer ADSC headers have more information on which type of detector it is, but older
     *	images may only have image sizes, etc., to go on.
     *
     *	Data is stored in the file by rows.  There are size1 columns in each row, and
     *	the image is made up of size2 rows.  The (0,0) index is in the upper left hand
     *	corner of the detector, viewed from the sample towards the detector.
     *
     *	Assign the detector gain here as well.  Different for "r" versions of Q210s and Q315s
     *	so take that into account.  Additionally, when the wavelength is extracted, multiply
     *	the "gain" to take into account the wavelength so it appxoximates photons/adu at
     *	the specified wavelength.
     */
    
    s[0] = '\0';
    gethd("HEADER_BYTES", s, header);
    if('\0' == s[0])
    {
        fprintf(stderr, "adscimg2cbf_sub2: Error: ADSC header has no HEADER_BYTES keyword\n");
        return(1);
    }
    header_size = atoi(s);
    
    if(NULL == (header_as_details = (char *) malloc(header_size + 2)))
    {
        fprintf(stderr,"adscimg2cbf_sub2: Error allocating %d bytes of memory for header_as_details.\n", header_size + 2);
        return(1);
    }
    strcpy(header_as_details, "\n");
    for(i = 0; 0 != gethdn(i, s, s1, header); i++)
    {
        sprintf(temp, "%s=%s;\n", s, s1);
        strcat(header_as_details, temp);
    }
    
    s[0] = '\0';
    gethd("SIZE1", s, header);
    if('\0' == s[0])
    {
        fprintf(stderr, "adscimg2cbf_sub2: Error: ADSC header has no SIZE1 keyword\n");
        return(1);
    }
    smv_size1 = atoi(s);
    
    s[0] = '\0';
    gethd("SIZE2", s, header);
    if('\0' == s[0])
    {
        fprintf(stderr, "adscimg2cbf_sub2: Error: ADSC header has no SIZE2 keyword\n");
        return(1);
    }
    smv_size2 = atoi(s);
    
    /* Decode the user-provided sensor thickness */
    
    thickness = 0.;
    if (thickstr && *thickstr) {
        char * endptr;
        thickness = strtod(thickstr,&endptr);
        if (thickness < 0. || endptr==thickstr) {
            fprintf(stderr,
                    "adscimg2cbf_sub2: Error: invalid sensor thickness '%s'\n",
                    thickstr);
            return(1);
        }
        /* automatically rescale thickness in um or m to mm
         restricts the range to .01 to 99.999 mm.
         To provide values above this range use um.
         To provide values below this range use m.
         */
        if (thickness > 100.) thickness *= 1.e-3;
        else if (thickness < .01) thickness *= 1.e3;
        
    }
    
    polarization_ratio = 0.;
    if (polarstr && *polarstr) {
        char * endptr;
        polarization_ratio = strtod(polarstr,&endptr);
        if (polarization_ratio < 0.
            || polarization_ratio > 1.0
            ||endptr==polarstr) {
            fprintf(stderr,
                    "adscimg2cbf_sub2: Error: invalid polarization source ratio '%s'\n",
                    polarstr);
            return(1);
        }
        
    }
    
    rad_smooth = 0;
    if (rad_smoothstr && *rad_smoothstr) {
        rad_smooth = atoi(rad_smoothstr);
        if (rad_smooth < 1 || rad_smooth > 100) {
            fprintf(stderr,
                    "adscimg2cbf_sub2: Error: invalid radial smoothing pixel range '%s'\n",
                    polarstr);
            return(1);
        }
    }

    
    /*
     *	Decode the header items having to do with adc speed, binning, etc.
     */
    
    smv_bin = 0;
    smv_bin_type = -1;
    smv_adc = -1;
    
    s[0] = '\0';
    gethd("ADC", s, header);
    if('\0' != s[0])
        smv_adc = atoi(s);
    
    s[0] = '\0';
    gethd("BIN", s, header);
    if('\0' != s[0])
    {
        if(0 == strcmp("2x2", s))
            smv_bin = 2;
        else
            smv_bin = 1;
        s1[0] = '\0';
        gethd("BIN_TYPE", s1, header);
        if('\0' != s1[0])
        {
            if(NULL != cbf_cistrnstr(s1, "SW",2))
            {
                smv_bin = 2;
                smv_adc = 0;
            }
            else
            {
                smv_bin = 2;
                smv_adc = 1;
            }
        }
    }
    
    if(1 == smv_bin)
        strcpy(detector_mode, "bin none");
    else if(1 == smv_adc)
        strcpy(detector_mode, "bin 2x2 hardware");
    else
        strcpy(detector_mode, "bin 2x2 software");
    
    if (bin) {
        strcat(detector_mode, ", adscimg2cbf bin 2x2");
    }
    
    s[0] = '\0';
    gethd("DETECTOR_SN", s, header);
    if('\0' == s[0])
        detector_sn = 0;
    else
        detector_sn = atoi(s);
    
    sprintf(facility, "%s crystallography", which_facility(detector_sn));
    
    if(0 == (smv_size1 % 576))
    {
        sprintf(detector_id, "ADSCQ4-SN%d", detector_sn);
        strcpy(detector_type, "ADSC QUANTUM4");
        gain = 3.1;
        detector_center_x = -94.;
        detector_center_y = 94.;
    } else
        if(0 == (smv_size1 % 1042))
        {
            sprintf(detector_id, "ADSCQ270-SN%d", detector_sn);
            strcpy(detector_type, "ADSC QUANTUM270");
            gain = 2.8;
            detector_center_x = -135.;
            detector_center_y = 135.;
        } else if (0 == (smv_size1 % 1536))
            {
                sprintf(detector_id, "ADSCQ315-SN%d", detector_sn);
                strcpy(detector_type, "ADSC QUANTUM315");
                gain = 2.4;
                if(2 == smv_bin && -1 != smv_adc)
                {
                    if(0 == smv_adc)
                        gain /= 4;
                    else
                        gain = 1.8;
                }
                detector_center_x = -157.5;
                detector_center_y = 157.5;
        } else if (0 == (smv_size1 % 1024))
                {
                    sprintf(detector_id, "ADSCQ210-SN%d", detector_sn);
                    strcpy(detector_type, "ADSC QUANTUM210");
                    gain = 2.4;
                    if(2 == smv_bin && -1 != smv_adc)
                    {
                        if(0 == smv_adc)
                            gain /= 4;
                        else
                            gain = 1.8;
                    }
                    detector_center_x = -105.;
                    detector_center_y = 105.;
                } else if ((smv_size2 == 2527 && smv_size1 == 2463)
                           || (smv_size2 == 2667 && smv_size1 == 2667))
                {
                    sprintf(detector_id, "PILATUS6M-SN%d", detector_sn);
                    strcpy(detector_type, "DECTRIS Pilatus 6M");
                    gain = 1;
                    detector_center_x = -211.732;
                    detector_center_y = 219.644;
                    if (thickness == 0.) thickness = 0.000320;
                } else if ((smv_size2 == 4371 && smv_size1 == 4150) ||
                           (smv_size2 == 4000 && smv_size1 == 4000))
                {
                    sprintf(detector_id, "EIGER16M-SN%d", detector_sn);
                    strcpy(detector_type, "DECTRIS Eiger 16M");
                    gain = 1;
                    detector_center_x = -155;
                    detector_center_y = 163.825;
                    if (thickness == 0.) thickness = 0.000450;
        } else if ((smv_size2 == 3269 && smv_size1 == 3110))
                {
                    sprintf(detector_id, "EIGER9M-SN%d", detector_sn);
                    strcpy(detector_type, "DECTRIS Eiger 9M");
                    gain = 1;
                    detector_center_x = -116.625;
                    detector_center_y = 122.586;
                    if (thickness == 0.) thickness = 0.000450;
                } else
                {
                    fprintf(stderr,
                    "adscimg2cbf_sub2: Error: Detector size of %d rows x %d columns does not correspond to a known detector type\n",
                            smv_size2, smv_size1);
                    return(1);
                }
    if (convertroi((char *)roi, smv_size1, smv_size2,
                   &fastlow, &fasthigh, &slowlow, &slowhigh)) {
        fprintf(stderr,
                "adscimg2cbf_sub2: Error: Invalid region of interest '%s'\n", roi);
    }
    
    
    /*
     *	Pixel size.
     *
     *	If it is in the header, good.
     *
     *	If not, it can be constructed, so do that.
     */
    
    s[0] = '\0';
    gethd("PIXEL_SIZE", s, header);
    if('\0' == s[0])
    {
        if(NULL != strstr(detector_id, "270"))
            pixel_size = 0.06478;
        else if(NULL != strstr(detector_id, "210"))
            pixel_size = 0.0512;
        else if(NULL != strstr(detector_id, "315"))
            pixel_size = 0.051296;
        else if(NULL != cbf_cistrnstr(detector_id, "PILATUS",7))
            pixel_size = 0.172;
        else if(NULL != cbf_cistrnstr(detector_id, "EIGER",5))
            pixel_size = 0.075;
        else	pixel_size = 0.0816;
        s1[0] = '\0';
        gethd("BIN", s1, header);
        if('\0' != s1[0])
        {
            if(0 == strcmp(s1, "2x2"))
                pixel_size *= 2;
        }
        if (bin) pixel_size *=2;
    }
    else
        pixel_size = atof(s);
    
    /* beam center in x and y */
    
    /*
     *	Coordinate system ends up being:
     *
     *    (0,0)
     *	Y
     *	^
     *	|
     *	|
     *	|
     *	|
     *	----------> X
     *
     *	Define the origin to be the upper left hand corner, since
     *	this is the "storage origin" of the data array.
     *
     *	The incoming beam center value is nominally defined as the
     *	adxv mm coordinate system:  origin in mm in the lower left hand
     *	corner.
     *
     *	Other conventions are recognized either through additional header
     *	keywords or "BEAM_CENTER" variants (future) found in the ADSC
     *	header, or forced through the beam_center_convention parameter
     *	to adscimg2cbf_sub() function.
     */
    
    s[0] = '\0';
    gethd("BEAM_CENTER_X", s, header);
    if('\0' == s[0])
        header_beam_x = detector_center_x;
    else
        header_beam_x = atof(s);
    
    det_beam_center_to_origin_dist_x = - header_beam_x;
    
    s[0] = '\0';
    gethd("BEAM_CENTER_Y", s, header);
    if('\0' == s[0])
        header_beam_y = detector_center_y;
    else
        header_beam_y = atof(s);
    
    switch(beam_center_convention)
    {
        case BEAM_CENTER_FROM_HEADER:
        case BEAM_CENTER_LLHC:
        default:
            
            det_beam_center_to_origin_dist_x = - header_beam_x;
            det_beam_center_to_origin_dist_y =   (smv_size1 - 1.5) * pixel_size - header_beam_y;
            break;
            
        case BEAM_CENTER_ULHC:
            det_beam_center_to_origin_dist_x = - header_beam_x;
            det_beam_center_to_origin_dist_y =   header_beam_y;
            break;
            
        case BEAM_CENTER_MOSFLM:
            det_beam_center_to_origin_dist_x = - header_beam_y;
            det_beam_center_to_origin_dist_y =   header_beam_x;
            break;
    }
    
    /* Date */
    
    s[0] = '\0';
    gethd("DATE", s, header);
    if('\0' == s[0])
    {
        smv_date[0] = '\0';
        cbf_date[0] = '\0';
    }
    else
    {
        strcpy(smv_date, s);
        smv_date_to_cbf_date(smv_date, cbf_date);
    }
    
    
    /* Wavelength */
    
    s[0] = '\0';
    gethd("WAVELENGTH", s, header);
    if('\0' == s[0])
        wavelength = -1;
    else
    {
        wavelength = atof(s);
        gain = gain / wavelength;
    }
    
    /* Oscillation start */
    
    s[0] = '\0';
    gethd("OSC_START", s, header);
    if('\0' == s[0])
        oscillation_start = 0.0;
    else
        oscillation_start = atof(s);
    
    /* Oscillation range */
    
    s[0] = '\0';
    gethd("OSC_RANGE", s, header);
    if('\0' == s[0])
        oscillation_range = 0.0;
    else
        oscillation_range = atof(s);
    
    /* Distance */
    
    s[0] = '\0';
    gethd("DISTANCE", s, header);
    if('\0' == s[0])
        distance = -1;
    else
        distance = atof(s);
    
    /* Time */
    
    s[0] = '\0';
    gethd("TIME", s, header);
    if('\0' == s[0])
        smv_time = -1;
    else
        smv_time = atof(s);
    
    overload = 65535;
    
    /* Image size and orientation & gain and overload */
    
    dimension [0] = 1+fasthigh-fastlow;
    dimension [1] = 1+slowhigh-slowlow;
    
    precedence [0] = 1;
    precedence [1] = 2;
    
    direction [0] = "increasing";
    direction [1] = "increasing";
    
    fastpadlow = fastpadhigh = slowpadlow = slowpadhigh = 0;
    
    if (!strcmp(detector_type, "DECTRIS Eiger 16M")
        && fastlow == 0 && fasthigh == 3999
        && slowlow == 0 && slowhigh == 3999
        && !roi) {
        
        fastpadlow = fastpadhigh = 75;
        slowpadlow = 186;
        slowpadhigh = 185;
        
        dimension[0] += 150;
        dimension[1] += 371;
        
        fprintf(stderr,"Expanded 4000x4000 image to Eiger 16M\n");

        
    }
    
    if (!strcmp(detector_type, "DECTRIS Pilatus 6M")
        && fastlow == 0 && fasthigh == 2666
        && slowlow == 0 && slowhigh == 2666
        && !roi) {
        
        
        fastpadlow = fastpadhigh = -102;
        slowpadlow = -70;
        slowpadhigh = -70;
        
        dimension[0] -= 204;
        dimension[1] -= 140;
        
        fprintf(stderr,"Cut 2667x2667 image to Pilatus 6M\n");

        
    }
    
    if (bin) {
        
        dimension[0] /= 2;
        dimension[1] /= 2;
        
    }
    
    dimension[1] += old_int_data_size/sizeof(int)/
    ((1+fasthigh-fastlow
      +fastpadlow+fastpadhigh)/(bin+1));
    
    
    /* Make sure to swap bytes if there is a change in byte order
     * between the machine which made the SMV file and this machine */
    
    /* this_bo = getbo();*/
    cbf_get_local_integer_byte_order(&local_bo);
    this_bo = (local_bo[0]=='l'||local_bo[0]=='L')?0:1;
    smv_bo = this_bo;
    
    s[0] = '\0';
    gethd("BYTE_ORDER", s, header);
    if('\0' != s[0])
    {
        if(0 == strcmp(s, "little_endian"))
            smv_bo = 0;
        else if(0 == strcmp(s, "big_endian"))
            smv_bo = 1;
    }
    if(this_bo != smv_bo)
        short_swap(data, smv_size1 * smv_size1);
    
    /* Make a cbf version of the image */
    
    a = clock ();
    
    if (cbf_filename) {
        
        
        /* Create the cbf */
        
        cbf_failnez (cbf_make_handle (&cbf));
        
        
        /* Make a new data block */
        
        cbf_failnez (cbf_new_datablock (cbf, "image_1"));
        
        
        /* Make the _diffrn category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "DS1"));
        
        
        /* Make the _diffrn_source category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_source"));
        cbf_failnez (cbf_require_column   (cbf, "diffrn_id"));
        cbf_failnez (cbf_set_value        (cbf, "DS1"));
        cbf_failnez (cbf_require_column   (cbf, "source"));
        cbf_failnez (cbf_set_value        (cbf, "synchrotron"));
        cbf_failnez (cbf_require_column   (cbf, "type"));
        cbf_failnez (cbf_set_value        (cbf, facility));
        
        
        /* Make the _diffrn_radiation category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_radiation"));
        cbf_failnez (cbf_require_column   (cbf, "diffrn_id"));
        cbf_failnez (cbf_set_value    (cbf, "DS1"));
        cbf_failnez (cbf_require_column   (cbf, "wavelength_id"));
        cbf_failnez (cbf_set_value    (cbf, "L1"));
        if (polarization_ratio) {
            cbf_failnez (cbf_require_column   (cbf, "polarizn_source_ratio"));
            cbf_failnez (cbf_set_doublevalue  (cbf, "%.3f",polarization_ratio));
        }
        
        
        
        
        /* Make the _diffrn_radiation_wavelength category */
        
        cbf_failnez (cbf_require_category(cbf, "diffrn_radiation_wavelength"));
        cbf_failnez (cbf_require_column  (cbf, "id"));
        cbf_failnez (cbf_set_value       (cbf, "L1"));
        cbf_failnez (cbf_require_column  (cbf, "wavelength"));
        
        if (wavelength > 0.0) {
            
            cbf_failnez (cbf_set_doublevalue (cbf, "%.6f", wavelength));
            
            cbf_failnez (cbf_require_column  (cbf, "wt"));
            cbf_failnez (cbf_set_value       (cbf, "1.0"));
            
        }
        
        
        /* Make the _diffrn_measurement category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_measurement"));
        cbf_failnez (cbf_require_column   (cbf, "diffrn_id"));
        cbf_failnez (cbf_set_value        (cbf, "DS1"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "GONIOMETER"));
        cbf_failnez (cbf_require_column   (cbf, "method"));
        cbf_failnez (cbf_set_value        (cbf, "oscillation"));
        cbf_failnez (cbf_require_column   (cbf, "number_of_axes"));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        cbf_failnez (cbf_require_column   (cbf, "sample_detector_distance"));
        
        if (distance > 0.0) {
            cbf_failnez (cbf_set_doublevalue (cbf, "%.4f", distance));
        } else {
            cbf_failnez (cbf_set_value       (cbf, "unknown"));
        }
        
        /* Make the _diffrn_measurement_axis category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_measurement_axis"));
        cbf_failnez (cbf_require_column   (cbf, "measurement_id"));
        cbf_failnez (cbf_set_value        (cbf, "GONIOMETER"));
        cbf_failnez (cbf_require_column   (cbf, "axis_id"));
        cbf_failnez (cbf_set_value        (cbf, "GONIOMETER_PHI"));
        
        /* Make the _diffrn_scan category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_scan"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_require_column   (cbf, "frame_id_start"));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_require_column   (cbf, "frame_id_end"));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_require_column   (cbf, "frames"));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        
        /* Make the _diffrn_scan_axis category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_scan_axis"));
        cbf_failnez (cbf_require_column   (cbf, "scan_id"));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_require_column   (cbf, "axis_id"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "GONIOMETER_PHI"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Z"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Y"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_X"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_PITCH"));
        cbf_failnez (cbf_require_column   (cbf, "angle_start"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", oscillation_start));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_require_column   (cbf, "angle_range"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", oscillation_range));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_require_column   (cbf, "angle_increment"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", oscillation_range));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_require_column   (cbf, "displacement_start"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", distance));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_require_column   (cbf, "displacement_range"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_require_column   (cbf, "displacement_increment"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        
        /* Make the _diffrn_scan_frame category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_scan_frame"));
        cbf_failnez (cbf_require_column   (cbf, "scan_id"));
        cbf_failnez (cbf_set_value        (cbf, "SCAN1"));
        cbf_failnez (cbf_require_column   (cbf, "frame_id"));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_require_column   (cbf, "frame_number"));
        cbf_failnez (cbf_set_value        (cbf, "1"));
        if(-1 != smv_time)
        {
            cbf_failnez (cbf_require_column   (cbf, "integration_time"));
            cbf_failnez (cbf_set_doublevalue  (cbf, "%.4f", smv_time));
        }
        if('\0' != cbf_date[0])
        {
            cbf_failnez (cbf_require_column   (cbf, "date"));
            cbf_failnez (cbf_set_value        (cbf, cbf_date));
        }
        
        /* Make the _diffrn_scan_frame_axis category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_scan_frame_axis"));
        cbf_failnez (cbf_require_column   (cbf, "frame_id"));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_require_column   (cbf, "axis_id"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "GONIOMETER_PHI"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Z"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Y"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_X"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_PITCH"));
        cbf_failnez (cbf_require_column   (cbf, "angle"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", oscillation_start));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_require_column   (cbf, "displacement"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.00));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", distance));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.2f", 0.0));
        
        /* Make the _axis category */

        /* The default detector axis definitions assume that
        the rotation axis points in the same direction as 
        ELEMENT_X. If reverse is set, the rotation axis points 
        in the opposite direction to ELEMENT_X which is
        equivalent to a 180 degree rotation about the Z axis
        and thus X-> -X and Y -> -Y compared to the default
        axes.
        */
        
        cbf_failnez (cbf_require_category (cbf, "axis"));
        
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "GONIOMETER_PHI"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "SOURCE"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "GRAVITY"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Z"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Y"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_X"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_PITCH"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_X"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_Y"));
        
        cbf_failnez (cbf_require_column   (cbf, "type"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "rotation"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "general"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "general"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "translation"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "translation"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "translation"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "rotation"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "translation"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "translation"));
        
        cbf_failnez (cbf_require_column   (cbf, "equipment"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "goniometer"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "source"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "gravity"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "detector"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "detector"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "detector"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "detector"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "detector"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "detector"));
        
        cbf_failnez (cbf_require_column   (cbf, "depends_on"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Z"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Y"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_X"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_PITCH"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_X"));
        
        cbf_failnez (cbf_require_column   (cbf, "vector[1]"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "1"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        
        cbf_failnez (cbf_require_column   (cbf, "vector[2]"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, -1*reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, -1*reverse));
        
        cbf_failnez (cbf_require_column   (cbf, "vector[3]"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "1"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "-1"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        
        cbf_failnez (cbf_require_column   (cbf, "offset[1]"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.3f", det_beam_center_to_origin_dist_x * reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        
        cbf_failnez (cbf_require_column   (cbf, "offset[2]"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.3f", det_beam_center_to_origin_dist_y * reverse));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        
        cbf_failnez (cbf_require_column   (cbf, "offset[3]"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "."));
        cbf_failnez (cbf_set_typeofvalue  (cbf, "null"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "0"));
        
        /* Make the _diffrn_detector category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_detector"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        cbf_failnez (cbf_require_column   (cbf, "diffrn_id"));
        cbf_failnez (cbf_set_value        (cbf, "DS1"));
        cbf_failnez (cbf_require_column   (cbf, "type"));
        cbf_failnez (cbf_set_value        (cbf, detector_type));
        cbf_failnez (cbf_require_column   (cbf, "details"));
        cbf_failnez (cbf_set_value        (cbf, detector_mode));
        cbf_failnez (cbf_require_column   (cbf, "number_of_axes"));
        cbf_failnez (cbf_set_integervalue (cbf, 4));
        cbf_failnez (cbf_require_column   (cbf, "layer_thickness"));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%12.5f", thickness));
        
        /* Make the _diffrn_detector_axis category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_detector_axis"));
        cbf_failnez (cbf_require_column   (cbf, "detector_id"));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        cbf_failnez (cbf_require_column   (cbf, "axis_id"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_X"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Y"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_Z"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "DETECTOR_PITCH"));
        cbf_failnez (cbf_next_row         (cbf));
        
        /* Make the _diffrn_detector_element category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_detector_element"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT1"));
        cbf_failnez (cbf_require_column   (cbf, "detector_id"));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        
        
        /* Make the _diffrn_frame_data category */
        
        cbf_failnez (cbf_require_category (cbf, "diffrn_data_frame"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "FRAME1"));
        cbf_failnez (cbf_require_column   (cbf, "detector_element_id"));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT1"));
        cbf_failnez (cbf_require_column   (cbf, "detector_id"));
        cbf_failnez (cbf_set_value        (cbf, detector_id));
        cbf_failnez (cbf_require_column   (cbf, "array_id"));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_require_column   (cbf, "binary_id"));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        cbf_failnez (cbf_require_column   (cbf, "details"));
        cbf_failnez (cbf_set_value        (cbf, header_as_details));
        cbf_failnez (cbf_set_typeofvalue  (cbf,"text"));
        free(header_as_details);
        
        /* Make the _array_structure_list category */
        
        cbf_failnez (cbf_require_category (cbf, "array_structure_list"));
        cbf_failnez (cbf_require_column   (cbf, "array_id"));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_require_column   (cbf, "index"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, 2));
        cbf_failnez (cbf_require_column   (cbf, "dimension"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, dimension [0]));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, dimension [1]));
        cbf_failnez (cbf_require_column   (cbf, "precedence"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, precedence [0]));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, precedence [1]));
        cbf_failnez (cbf_require_column   (cbf, "direction"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, direction [0]));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, direction [1]));
        cbf_failnez (cbf_require_column   (cbf, "axis_set_id"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_X"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_Y"));
        
        /* Make the _array_element_size category */
        
        cbf_failnez (cbf_require_category (cbf, "array_element_size"));
        cbf_failnez (cbf_require_column   (cbf, "array_id"));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_require_column   (cbf, "index"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_integervalue (cbf, 2));
        cbf_failnez (cbf_require_column   (cbf, "size"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.3fe-6", pixel_size * 1000.));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.3fe-6", pixel_size * 1000.));
        
        /* Make the _array_structure category */
        
        cbf_failnez (cbf_require_category (cbf, "array_structure"));
        cbf_failnez (cbf_require_column   (cbf, "id"));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_require_column   (cbf, "encoding_type"));
        cbf_failnez (cbf_set_value        (cbf, "signed 32-bit integer"));
        
        /* Make the _array_structure_list_axis category */
        
        cbf_failnez (cbf_require_category (cbf, "array_structure_list_axis"));
        cbf_failnez (cbf_require_column   (cbf, "axis_set_id"));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_X"));
        cbf_failnez (cbf_new_row          (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_Y"));
        cbf_failnez (cbf_require_column   (cbf, "axis_id"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_X"));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_value        (cbf, "ELEMENT_Y"));
        cbf_failnez (cbf_require_column   (cbf, "displacement"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.6f", (fastlow-fastpadlow)*pixel_size));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.6f", -(slowlow-slowpadlow)*pixel_size));
        cbf_failnez (cbf_require_column   (cbf, "displacement_increment"));
        cbf_failnez (cbf_rewind_row       (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.6f", pixel_size));
        cbf_failnez (cbf_next_row         (cbf));
        cbf_failnez (cbf_set_doublevalue  (cbf, "%.6f", pixel_size));
        
        
        /* Make the _array_intensities category */
        
        cbf_failnez (cbf_require_category (cbf, "array_intensities"));
        cbf_failnez (cbf_require_column   (cbf, "array_id"));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_require_column   (cbf, "binary_id"));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        cbf_failnez (cbf_require_column   (cbf, "linearity"));
        cbf_failnez (cbf_set_value        (cbf, "linear"));
        cbf_failnez (cbf_require_column   (cbf, "gain"));
        
        if (gain > 0.0) {
            
            cbf_failnez (cbf_set_doublevalue  (cbf, "%.3g", gain));
            
        }
        
        cbf_failnez (cbf_require_column   (cbf, "overload"));
        
        if (overload > 0.0) {
            
            cbf_failnez (cbf_set_integervalue (cbf, overload));
            
        }
        
        cbf_failnez (cbf_require_column   (cbf, "undefined_value"));
        cbf_failnez (cbf_set_integervalue (cbf, 0));
        
        cbf_failnez (cbf_require_column   (cbf, "pixel_slow_bin_size"));
        cbf_failnez (cbf_set_integervalue (cbf, smv_bin*(bin?2:1)));
        cbf_failnez (cbf_require_column   (cbf, "pixel_fast_bin_size"));
        cbf_failnez (cbf_set_integervalue (cbf, smv_bin*(bin?2:1)));
        
        
        /* Make the _array_data category */
        
        cbf_failnez (cbf_require_category (cbf, "array_data"));
        if (add_minicbf_header) {
            cbf_failnez (cbf_require_column   (cbf, "header_convention"));
            cbf_failnez (cbf_set_value        (cbf, "."));
            cbf_failnez (cbf_require_column   (cbf, "header_contents"));
            cbf_failnez (cbf_set_value        (cbf, "."));
        }
        cbf_failnez (cbf_require_column   (cbf, "array_id"));
        cbf_failnez (cbf_set_value        (cbf, "image_1"));
        cbf_failnez (cbf_require_column   (cbf, "binary_id"));
        cbf_failnez (cbf_set_integervalue (cbf, 1));
        if (add_minicbf_header) {
            char * log = NULL;
            if (cbf_set_minicbf_header(cbf,cbf,&log)) {
                if (log) fprintf(stderr,"minicbf_header error messages: \n%s\n", log);
                return(1);
            };
            if (log) fprintf(stderr,"minicbf_header messages: \n%s\n", log);
            cbf_free_text((const char **)(&log),NULL);
        }
        
        cbf_failnez (cbf_require_category (cbf, "array_data"));
        cbf_failnez (cbf_require_column   (cbf, "data"));
        
    }
    
    /* Save the binary data */
    if (bin) bin=1;

    if(NULL ==
       (data_as_int =
        (int *) malloc(old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                       *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1)))))
    {
        fprintf(stderr, "Error mallocing %d bytes of temporary memory for image conversion\n",
                (int) (old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                                          *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1))));
        return(1);
    } else {
        fprintf(stderr, "Allocated %d bytes of temporary memory for image conversion\n",
                (int) (old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                                          *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1))));
        
    }
    
    
    if (old_int_data_size) {
        memmove(data_as_int,old_int_data,old_int_data_size);
        data_as_int += old_int_data_size/sizeof(int);
    }
    
    ip = data_as_int;
    up = data;
    scale = 1.0;
    if (scalestr) {
        char * endptr;
        scale = strtod(scalestr,&endptr);
        if (*endptr != '\0') {
            fprintf(stderr, "Warning, specified scale had extraneous characters, scale set to 1\n");
            scale = 1.0;
        }
    }
    offset = 0.0;
    if (offsetstr) {
        char * endptr;
        offset = strtod(offsetstr,&endptr);
        if (*endptr != '\0') {
            fprintf(stderr, "Warning, specified offset had extraneous characters, offset set to 0\n");
            offset = 0.0;
        }
    }
    cliplow = 0.0;
    if (cliplowstr) {
        char * endptr;
        cliplow = strtod(cliplowstr,&endptr);
        if (*endptr != '\0') {
            fprintf(stderr, "Warning, specified cliplow had extraneous characters, cliplow set to 0\n");
            cliplow = 0.0;
        }
    }
    cliphigh = 16384.0;
    if (cliphighstr) {
        char * endptr;
        cliphigh = strtod(cliphighstr,&endptr);
        if (*endptr != '\0') {
            fprintf(stderr, "Warning, specified cliphigh had extraneous characters, cliphigh set to 16384\n");
            cliphigh = 16384.0;
        }
    }
    
    cmi = 0.;
    cmj = 0.;
    cmv = 1.e-12;
    
    if (rad_smooth) {
        if(NULL ==
           (smooth_data_as_int =
            (int *) malloc((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                           *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1))))
        {
            fprintf(stderr, "Error mallocing %d bytes of temporary memory for image conversion\n",
                    (int) ((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                           *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1)));
            return(1);
        }
    }
    
    for(i = slowlow-slowpadlow; i <= slowhigh+slowpadhigh-bin; i+= (bin+1)) {
        /* if (i%1000==0) fprintf(stderr,"i = %d\n",i); */
        for(j = fastlow-fastpadlow; j <= fasthigh+fastpadhigh-bin; j+= (bin+1)) {
            /* f (j%1000==0) fprintf(stderr,"j = %d\n",j); */
            if (i >= slowlow && i <= slowhigh-bin && j >=fastlow && j <= fasthigh-bin) {
                ki = (i-slowlow+slowpadlow+bin)/(bin+1);
                kj = (j-fastlow+fastpadlow+bin)/(bin+1);
                *ip = 0x0000ffff &
                (int)up[i*smv_size1+j];
                if ((*ip & 0x0000fff0) == 0x0000fff0) *ip |= 0xffff0000;
                if (bin) {
                    *ip = *ip+(0x0000ffff &
                            (int)up[i*smv_size1+j+1])
                           +(0x0000ffff &
                            (int)up[(i+1)*smv_size1+j])
                            +(0x0000ffff &
                              (int)up[(i+1)*smv_size1+j+1]);
                };
                 /* If radial smoothing is specified, accumulate a smoothed image in smooth_data_as_int */
                if (rad_smooth) {
                    int sv;
                    sv = *ip;
                    if ((sv & 0x0000fff0) == 0x0000fff0) sv = 0;
                    if (kj > 0 && smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj-1]
                        > 0 ) {
                        smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj]
                        = (smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj-1] +sv)/2;
                    } else {
                        smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj] =sv;
                    }
                    if (ki > 0 && smooth_data_as_int[(ki-1)*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj] > 0) {
                        smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj]
                        = ( smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj]+
                           smooth_data_as_int[(ki-1)*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj])/2.;
                    }
                }
                if (scalestr || offsetstr) *ip = scale*((double)(*ip))+offset;
                if (cliplowstr && (*ip) < (int)cliplow ) *ip = (int) cliplow;
                if (cliphighstr && (*ip) > (int)cliphigh ) *ip = (int) cliphigh;
                cmi += ((double)ki)*((double)(*ip));
                cmj += ((double)kj)*((double)(*ip));
                cmv += (double)(*ip);
                ip++;
                
            } else {
                *ip++ = 0xfffffffd;
            }
        }
    }
    
    if (rad_smooth) {
        double smoothval;
        double valnew,valmin,valmax;
        double vec2cm[2];
        int ivec2cm[2];
        double dist2cm;
        int id,ir;

        CBF_UNUSED( valmax );
        CBF_UNUSED( valnew );
        
        cmi = rint(cmi/cmv);
        cmj = rint(cmj/cmv);
        fprintf(stderr,"cmj, cmj %g %g\n", cmj, cmi);
        for (ki = 0; ki < (1+slowhigh-slowlow+slowpadlow+slowpadhigh)/(bin+1); ki++) {
            if (ki*(bin+1) < slowpadlow || ki*(bin+1) + slowlow-slowpadlow > slowhigh+slowpadhigh-bin ) continue;
            for (kj = 0; kj < (1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1); kj++) {
                if (kj*(bin+1) < fastpadlow || ki*(bin+1) + fastlow-fastpadlow >= fasthigh-bin ) continue;
                smoothval = (double)(smooth_data_as_int[ki*(fasthigh-fastlow+1)/(bin+1)+kj]);
                if (smoothval < 0.) continue;
                if (smoothval > smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj])
                    smoothval = smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj];
                valmax = valmin = smoothval;
                vec2cm[0] = (double)kj - cmj;
                vec2cm[1] = (double)ki - cmi;
                dist2cm = sqrt(vec2cm[0]*vec2cm[0]+vec2cm[1]*vec2cm[1]);
                if (dist2cm > 1) {
                    double rat, rrat;
                    int ptcount;
                    vec2cm[0] /= dist2cm;
                    vec2cm[1] /= dist2cm;
                    if (ki%100==0 && kj%100==0)
                        fprintf(stderr,"kj, ki, %d %d vec2cm %g %g\n", kj, ki, vec2cm[0], vec2cm[1]);
                    ptcount = 1;
                    for (id = 1; id <= rad_smooth; id ++ ){
                        for (ir = -id; ir <= id; ir+=2) {
                            rat = (double)id;
                            rrat = ((double)ir)/2.;
                            ivec2cm[0] = (int)rint(kj + vec2cm[0]*rat - vec2cm[1]*rrat);
                            ivec2cm[1] = (int)rint(ki + vec2cm[1]*rat + vec2cm[0]*rrat);
                            if (ivec2cm[1]*(bin+1) >= slowpadlow
                                && ivec2cm[1]*(bin+1) + slowlow-slowpadlow <= slowhigh+slowpadhigh-bin
                                && ivec2cm[0]*(bin+1) >= fastpadlow
                                && ivec2cm[0]*(bin+1) + fastlow-fastpadlow <= fasthigh+fastpadhigh-bin) {
                                if (smooth_data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj] != 0xfffffffd) {
                                    valnew = (double)(smooth_data_as_int[ivec2cm[1]*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+ivec2cm[0]]);
                                    /*if (valnew > valmax) valmax = valnew;
                                    if (valnew < valmin) valmin = valnew;
                                    if ((valmax - valmin) > sqrt(smoothval) && valnew < smoothval ) {
                                        smoothval = valnew + .05*(smoothval-valnew);
                                        valmax = valmin = smoothval;
                                    } */
                                    smoothval=(smoothval*((double)ptcount)+
                                               (double)(smooth_data_as_int[ivec2cm[1]*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+ivec2cm[0]]))/(((double)ptcount+1));
                                    /* if (smoothval > valmax) valmax = smoothval;
                                    if (smoothval < valmin) valmin = smoothval; */
                                    ptcount++;
                                }
                                
                            }
                        }
                    }
                }
                data_as_int[ki*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1)+kj]= (int)rint(smoothval);
                
            }
        }
        free (smooth_data_as_int);
    }
    
    if (new_int_data) {
        *new_int_data = data_as_int-old_int_data_size/sizeof(int);
    }
    if (new_int_data_size) {
        *new_int_data_size = old_int_data_size+sizeof(int)*(1+fasthigh-fastlow+fastpadlow+fastpadhigh)
        * (1+slowhigh-slowlow+slowpadlow+slowpadhigh)/(bin+1)/(bin+1);
    }
    
    if (!cbf_filename) return 0;
    {
        size_t dim_fast = (size_t)(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1);
        size_t dim_slow = old_int_data_size/sizeof(int)/(1+fasthigh-fastlow +fastpadlow+fastpadhigh)
        + (1+slowhigh-slowlow+slowpadlow+slowpadhigh)/(bin+1);
        size_t dim_total = dim_fast*dim_slow;
        size_t big_step = dim_fast*(1+slowhigh-slowlow+slowpadlow+slowpadhigh)/(bin+1);
        size_t repeat = dim_total/big_step;
        int * data_to_write;
        
        data_to_write = data_as_int-old_int_data_size/sizeof(int);
        
        if (transpose) {
            size_t ifast;
            size_t islow;
            size_t oindex;
            size_t nindex;
            int * transposed_data;
            if(NULL ==
               (transposed_data =
                (int *) malloc(old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                                                  *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1)))))
            {
                fprintf(stderr, "Error mallocing %d bytes of temporary memory for image transpose\n",
                        (int) (old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                                                  *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1))));
                return(1);
            }
            for (islow = 0; islow < dim_slow; islow ++) {
                for (ifast = 0; ifast < dim_fast; ifast ++) {
                    oindex = ifast + islow*dim_fast;
                    nindex = islow + ifast*dim_slow;
                    transposed_data[nindex] = data_to_write[oindex];
                }
            }
            free (data_to_write);
            data_to_write = transposed_data;
            dim_fast = dim_slow;
            dim_slow = (size_t)(1+fasthigh-fastlow+fastpadlow+fastpadhigh)/(bin+1);
            big_step = (1+slowhigh-slowlow+slowpadlow+slowpadhigh)/(bin+1);
        }
        
        if (interleave) {
            size_t ifast;
            size_t islow;
            size_t iblock;
            size_t oindex;
            size_t nindex;
            size_t offsets[4];
            int * interleaved_data;
            if (repeat < 2) big_step = (big_step+1)/2;
            if(NULL ==
               (interleaved_data =
                (int *) malloc(old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                                                  *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1)))))
            {
                fprintf(stderr, "Error mallocing %d bytes of temporary memory for image interleave\n",
                        (int) (old_int_data_size+((1+fasthigh-fastlow+fastpadlow+fastpadhigh)
                                                  *(1+slowhigh-slowlow+slowpadlow+slowpadhigh)*sizeof(int)/(bin+1)/(bin+1))));
                return(1);
            }
            
            offsets[0] = offsets[2] = 0;
            offsets[1] = big_step;
            offsets[3] = dim_total-big_step;
            for (islow = 0; islow < dim_slow; islow ++) {
                for (ifast = 0; ifast < dim_fast; ifast ++) {
                    oindex = nindex = ifast + islow*dim_fast;
                    iblock = nindex/(interleave);
                    oindex += offsets[iblock%4];
                    oindex %= dim_total;
                    interleaved_data[nindex] = data_to_write[oindex];
                }
            }
            free (data_to_write);
            data_to_write = interleaved_data;
            
        }
        
        cbf_failnez( cbf_set_integerarray_wdims_fs ((cbf_handle)    cbf,
                                                    (unsigned int) pack_flags,
                                                    (int)          1,
                                                    data_to_write,
                                                    sizeof(int),
                                                    (int)           1,
                                                    dim_total,
                                                    "little_endian",
                                                    dim_fast,
                                                    dim_slow,
                                                    (size_t)        0,
                                                    (size_t)        0));
        
        data_as_int = data_to_write + old_int_data_size/sizeof(int);
        
    }
    
    /* Write the new file */
    
    out = fopen (cbf_filename, "w+b");
    
    if (!out)
    {
        fprintf (stderr, " Couldn't open the CBF file %s\n", cbf_filename);
        
        exit (1);
    }
    
    cbf_failnez (cbf_write_file (cbf, out, 1, CBF, MSG_DIGEST | MIME_HEADERS | pad_flag, 0));
    
    
    /* Free the cbf */
    
    cbf_failnez (cbf_free_handle (cbf));
    free(data_as_int-old_int_data_size/sizeof(int));
    if (new_int_data) {
        *new_int_data = NULL;
    }
    
    b = clock ();
    
    fprintf (stderr, " Time to write the CBF image: %.3fs\n",
             ((b - a) * 1.0) / CLOCKS_PER_SEC);
    
    return 0;
}

int     adscimg2cbf_sub(char *header,
		         unsigned short *data,
			 char *cbf_filename,
			 int pack_flags,
			 int beam_center_convention,
		         int pad_flag){
	return adscimg2cbf_sub2(header,
                            data,
                            NULL,0,NULL,NULL,
                            cbf_filename,
                            pack_flags,
                            beam_center_convention,
                            pad_flag,
                            NULL,
                            NULL,
                            0,
                            NULL,
                            0,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            NULL,
                            0,
                            0,
                            1);
}

