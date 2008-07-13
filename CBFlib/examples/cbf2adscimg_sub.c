/* $Id */
/**********************************************************************
 * cbf2adscimg_sub -- jiffy to read a CBF file and make an adsc (SMV) *
 *		      image file.				      *
 *                                                                    *
 * Version 0.1                                                        *
 * 	                                                              *
 * 	       Chris Nielsen (cn@adsc-xray.com)                       *
 *                                                                    *
 * 	Code starting point taken from cbfwrap.c                      *
 *                                                                    *
 *  cbfwrap.c:                                                        *
 *             Harry Powell (harry@mrc-lmb.cam.ac.uk)                 *
 * developed from makecbf by                                          *
 *             Paul Ellis (ellis@ssrl.slac.stanford.edu)              *
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
 *                             The IUCr Policy                        *
 *                                    on                              *
 *     the Use of the Crystallographic Information File (CIF)         *
 *                                                                    *
 * The Crystallographic Information File (Hall, Allen & Brown,        *
 * 1991) is, as of January 1992, the recommended method for           *
 * submitting publications to Acta Crystallographica Section C. The   *
 * International Union of Crystallography holds the Copyright on      *
 * the CIF, and has applied for Patents on the STAR File syntax       *
 * which is the basis for the CIF format.                             *
 *                                                                    *
 * It is a principal objective of the IUCr to promote the use of      *
 * CIF for the exchange and storage of scientific data. The IUCr's    *
 * sponsorship of the CIF development was motivated by its            *
 * responsibility to its scientific journals, which set the           *
 * standards in crystallographic publishing. The IUCr intends that    *
 * CIFs will be used increasingly for electronic submission of        *
 * manuscripts to these journals in future. The IUCr recognises       *
 * that, if the CIF and the STAR File are to be adopted as a means    *
 * for universal data exchange, the syntax of these files must be     *
 * strictly and uniformly adhered to. Even small deviations from      *
 * the syntax would ultimately cause the demise of the universal      *
 * file concept. Through its Copyrights and Patents the IUCr has      *
 * taken the steps needed to ensure strict conformance with this      *
 * syntax.                                                            *
 *                                                                    *
 * The IUCr policy on the use of the CIF and STAR File processes is   *
 * as follows:                                                        *
 * _________________________________________________________________  *
 *                                                                    *
 *  * 1 CIFs and STAR Files may be generated, stored or transmitted,  *
 *    without permission or charge, provided their purpose is not     *
 *    specifically for profit or commercial gain, and provided that   *
 *    the published syntax is strictly adhered to.                    *
 *  * 2 Computer software may be developed for use with CIFs or STAR  *
 *    files, without permission or charge, provided it is distributed *
 *    in the public domain. This condition also applies to software   *
 *    for which a charge is made, provided that its primary function  *
 *    is for use with files that satisfy condition 1 and that it is   *
 *    distributed as a minor component of a larger package of         *
 *    software.                                                       *
 *  * 3 Permission will be granted for the use of CIFs and STAR Files *
 *    for specific commercial purposes (such as databases or network  *
 *    exchange processes), and for the distribution of commercial     *
 *    CIF/STAR software, on written application to the IUCr Executive *
 *    Secretary, 2 Abbey Square, Chester CH1 2HU, England. The        *
 *    nature, terms and duration of the licences granted will be      *
 *    determined by the IUCr Executive and Finance Committees.        *
 *                                                                    *
 * _________________________________________________________________  *
 *                                                                    *
 * In summary, the IUCr wishes to promote the use of the STAR File    *
 * concepts as a standard universal data file. It will insist on      *
 * strict compliance with the published syntax for all                *
 * applications. To assist with this compliance, the IUCr provides    *
 * public domain software for checking the logical integrity of a     *
 * CIF, and for validating the data name definitions contained        *
 * within a CIF. Detailed information on this software, and the       *
 * associated dictionaries, may be obtained from the IUCr Office at   *
 * 5 Abbey Square, Chester CH1 2HU, England.                          *
 **********************************************************************/

#include "cbf.h"
#include "cbf_simple.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>


#undef cbf_failnez
#define cbf_failnez(x) \
 {int err; \
  err = (x); \
  if (err) { \
    fprintf(stderr,"\nCBFlib fatal error %x \n",err); \
    fprintf(stderr,"caused by call " #x "\n"); \
   } \
 }

static char * array_id;

void puthd (char* field, char* value, char* header)
{
  char   temp[5];
  int    i, diff;
  char   *hp, *vp, *tp, *fp;

/*
 * find the } marking the end of the information in the header
 */
  for ( hp=header; *hp != '}'; hp++);

/*
 * Write the field name starting at the position of the }
 */
  for ( fp=field; *fp!=0; hp++, fp++) *hp = *fp;

/*
 * The field and the values are seperated by an = sign
 */
  *hp++ = '=';

/*
 * Write the field name starting at the position of the }
 */
  for ( vp=value; *vp!=0; hp++, vp++) *hp = *vp;

/*
 * End this field with a ; and new line
 * and mark the end of the header with a }
 */
  *hp++ = ';';
  *hp++ = 10;
  *hp++ = '}';

/*
 * Make the header a multiple of 4 by padding with spaces
 */
  i = (int) (hp-header);
  diff = 4 - i%4;
  if ( diff < 4 ) 
    for (i=0; i<diff; i++) *hp++=' ';

/*
 * Write the header length field
 */
  sprintf (temp, "%5d", (int) (hp-header));
  for (i=0,hp=header+15,tp=temp; i<5; i++) *hp++ = *tp++;

}

void clrhd ( char* header )
{
static char temp[] = "{ HEADER_BYTES";

header[0] = '}';
temp[1]= 10;

puthd (temp, "    0", header);

}

void padhd (char* header, int size)
{
  int i, diff;
  char temp[5], *hp, *tp;

/*
 * find the } marking the end of the header
 */
  for ( hp=header; *hp != '}'; hp++); hp++;

/*
 * End this field with a ; and new line
 * and mark the end of the header with a }
 */
  *hp++ = 0x0c; /* ASA 7/2/96, redone as hex HJB 1/22/2008 */
  /* *hp++ = 10;*/

/*
 * Make the header a multiple of "size" by padding with spaces
 */
  i = (int) (hp-header);
  diff = size - i%size;
  if ( diff < size ) 
    for (i=0; i<diff; i++) *hp++=' ';

/*
 * Write the header length field
 */
  sprintf (temp, "%5d", (int) (hp-header));
  for (i=0,hp=header+15,tp=temp; i<5; i++) *hp++ = *tp++;


}

int cbfhandle2img_sub(cbf_handle cbf, char **header, unsigned short **data)
{
	cbf_detector	this_detector;
	int 		id, index;
	int		i, k, colrow, first, second,dirsta[2], dirend[2], dirinc[2];
	size_t		nelem_read;
	double		pixel_size, gain, wavelength, distance, phi_start, phi_range;
	double		beam_centre[4];
	int		overload, dimension[2], precedence[2];
	const char	*detector;
	const char	*smv_header;
	unsigned short	*ushort_data, *up;
	unsigned int	*uint_data, *ip;
	int		header_length;
	int		make_smv_header_from_cbf;
	char		*char_header;
	char		*hp, *hpe, *hpequal;
	char		field[80], value[80];
	char		*fps, *fpe, *vps, *vpe, *cp;

	const char	*direction [2];
  
	/*
	 *	Legal, but uninteresting case.
	 */

	if(NULL == data && NULL == header)
		return(0);

	/* Get the image identifier */

	cbf_failnez (cbf_rewind_datablock (cbf))

	/* There may be two different ways to define the array_id we need. */

	if (cbf_find_category    (cbf, "diffrn_frame_data") !=0 )
		cbf_failnez (cbf_find_category    (cbf, "diffrn_data_frame"))

	cbf_failnez (cbf_find_column      (cbf, "array_id"))
    
	cbf_failnez (cbf_rewind_row       (cbf))

	cbf_failnez (cbf_get_value        (cbf, (const char **)&array_id))

	/* Get the image dimensions (second dimension = fast, first = slow) */

	cbf_failnez (cbf_find_category    (cbf, "array_structure_list"))

	cbf_failnez (cbf_rewind_row       (cbf))

	cbf_failnez (cbf_find_column      (cbf, "array_id"))

	dimension[0] = 0;
	dimension[1] = 0;

	while (cbf_find_nextrow (cbf, array_id) == 0)
	{
		cbf_failnez (cbf_find_column      (cbf, "index"))
		cbf_failnez (cbf_get_integervalue (cbf, &index))
		i = index;
		cbf_failnez (cbf_find_column      (cbf, "precedence"))
		cbf_failnez (cbf_get_integervalue (cbf, &index))
		if (index >= 1 && index <= 2)
			precedence[i-1] = index;
    
		cbf_failnez (cbf_find_column (cbf, "dimension"))
		cbf_failnez (cbf_get_integervalue (cbf, &dimension[i-1]))

		cbf_failnez (cbf_find_column      (cbf, "direction"))
		cbf_failnez (cbf_get_value (cbf, &direction[i-1]))
      
		cbf_failnez (cbf_find_column (cbf, "array_id"))

	}

	if (dimension [0] == 0 || dimension [1] == 0)
		return (1);
  
	/*
	 *	If we locate the diffrn_frame_data.details column then
	 *	the header from the original image is stored there.
	 *
	 *	This becomes the header of the output image without
	 *	modification.  This could change, with CBF header items
	 *	duplicating original header items superceeding the
	 *	original SMV header values.
	 */

	make_smv_header_from_cbf = 0;

	if(NULL != header)
	{
		make_smv_header_from_cbf = 1;
		if(0 == cbf_find_category (cbf, "diffrn_frame_data") || 0 == cbf_find_category(cbf, "diffrn_data_frame"))
		{
			if(0 == cbf_find_column (cbf, "details"))
			{
				cbf_failnez(cbf_get_value (cbf, &smv_header))
				if(0)
					fprintf(stdout,"Embedded SMV header in CBF header is:\n%s\n", smv_header);

				/*
				 *	Construct the SMV header directly from this item.
				 */

				if(NULL == (hp = strstr(smv_header, "HEADER_BYTES")))
				{
					fprintf(stderr,"cbf2adscimg_sub:  HEADER_BYTES not found in item diffrn_data_frame.details\n");
					fprintf(stderr,"\tIt is assumed this item contains the original SMV image header.\n");
					fprintf(stderr,"\tTry to reconstruct an SMV header from the CBF header items.\n");
				}
				else
				{
					if (NULL == (hpequal = strstr(hp, "=")))
				    {
					  fprintf(stderr,"cbf2adscimg_sub:  HEADER_BYTES value not found in item diffrn_data_frame.details\n");
					  fprintf(stderr,"\tIt is assumed this item contains the original SMV image header.\n");
					  fprintf(stderr,"\tTry to reconstruct an SMV header from the CBF header items.\n");
				    }
				    else 
				    {
					  sscanf(hpequal + 1, "%d", &header_length);
					  if(0)
						fprintf(stdout,"header length decoded as: %d\n", header_length);
					  if(NULL == (char_header = (char *) malloc(header_length)))
					  {
						fprintf(stderr,"cbf2adscimg_sub: Error allocating %d bytes for header\n",
										header_length);
						return(1);
					  }
					  clrhd(char_header);

					  hpe = ((char *) smv_header) + strlen(smv_header);
					while (hp && hp <= hpe && *hp 
					  && *hp != ';' && *hp != '\n' 
					  && *hp != '\r') hp++;
					  while (hp && hp <= hpe && *hp && *hp != '\n' && *hp != '\r') hp++;
					  if (hp && *hp == '\n') hp++;

					while(hp && hp <= hpe && *hp )
					  {
					    int tokencnt, tokenstate;
						fps = hp;
						while (fps <= hpe && (isspace(*fps)||*fps=='#')) fps++;
						fpe = strstr(hp, "=");
						fpe--;
						vps = fpe + 2;
						vpe = strstr(hp, ";");
						vpe--;
						for(i = tokencnt = tokenstate = 0, cp = fps; cp <= fpe; ) {
						    if (isspace(*cp)) {
						      tokenstate = 1;
						    } else {
						      if (tokenstate) tokencnt++;
						      tokenstate = 0;
						    }
							field[i++] = *cp++;
					    }
					    if (tokencnt > 0) break;
						field[i] = '\0';
						for(i = 0, cp = vps; cp <= vpe; )
							value[i++] = *cp++;
						value[i] = '\0';
						puthd(field, value, char_header);
						padhd(char_header, 512);
						hp = vpe + 3;
					  }
					  if(0)
						fprintf(stdout,"Reconstructed header:\n%s\n", char_header);
					  make_smv_header_from_cbf = 0;
					  *header = char_header;
					
				   }
				}
			}
		}
	}

 
	if(make_smv_header_from_cbf)
	{
	/* Detector */

  if (cbf_find_category (cbf, "diffrn_detector") == 0) 
  {
    cbf_failnez (cbf_find_column    (cbf, "id"))
      cbf_failnez(cbf_get_value (cbf, &detector))
  }

  /* Crystal to detector distance - obsolete, use diffrn_scan_axis detector_z
  if ((cbf_find_category (cbf, "diffrn_measurement") == 0) &&
     (cbf_find_column    (cbf, "sample_detector_distance") == 0) &&
	 (cbf_get_doublevalue (cbf, &distance) == 0) &&
	 (cbf_get_doublevalue (cbf, &distance) == 0) ) {
        cbf_double[1] = distance;
  }
  else {
    cbf_double[1] = -999.0;
  }
  */  
  /* hrp 18.01.2007 - new simple way to read header info, using CBFlib 0.7.6.1 + img.c/img.h*/

  cbf_get_wavelength (cbf, &wavelength);

  /*  if (cbf_find_category (cbf, "diffrn_radiation_wavelength") == 0) {
      cbf_failnez (cbf_find_column    (cbf, "wavelength"))
      cbf_failnez(cbf_get_doublevalue (cbf, &wavelength)) 
	}
   */

  i = cbf_construct_detector (cbf, &this_detector, 0);
  fprintf(stdout, "return from cbf_construct_detector: %x (hex)\n", i);
  /* b_c[0],[1] slow and fast changing direction in pixels, [2],[3] in mm. */

  cbf_get_beam_center (this_detector, &beam_centre [0], &beam_centre [1], &beam_centre [2], &beam_centre [3]);

  printf("beam centre values = %f %f %f %f\n", beam_centre [0], beam_centre [1], beam_centre [2], beam_centre [3]);

  cbf_get_detector_distance (this_detector, &distance);

  /*  printf("distance from header is %f metres\n",cbf_double[1]);
    if ((cbf_find_category (cbf, "diffrn_scan_axis") == 0) &&
       (cbf_find_column    (cbf, "displacement_start") == 0) &&
       (cbf_find_column    (cbf, "SCAN1") == 0) &&
  	 (cbf_get_doublevalue (cbf, &distance) == 0) &&
  	 (cbf_get_doublevalue (cbf, &distance) == 0) ) {
          cbf_double[1] = distance;
    }
    else {
      cbf_double[1] = -999.0;
    } 
   */
     
  /* 
   	scan angle and size - assume PHI for the moment

  cbf_get_axis_setting (cbf, 0, "GONIOMETER_OMEGA", &phi_start, &phi_range);
  cbf_double[5] = phi_start;
  cbf_double[7] = phi_range;
  cbf_double[6] = phi_range + phi_start;
  printf("omega range is %f %f %f \n",cbf_double[5],cbf_double[6],cbf_double[7]);
  cbf_get_axis_setting (cbf, 0, "GONIOMETER_KAPPA", &phi_start, &phi_range);
  cbf_double[5] = phi_start;
  cbf_double[7] = phi_range;
  cbf_double[6] = phi_range + phi_start;
  printf("kappa range is %f %f %f \n",cbf_double[5],cbf_double[6],cbf_double[7]); 
  */

  cbf_get_axis_setting (cbf, 0, "GONIOMETER_PHI", &phi_start, &phi_range);

  /* printf("phi range is %f %f %f \n",cbf_double[5],cbf_double[6],cbf_double[7]); */

  /* Pixel size(s) */

  if (cbf_find_category    (cbf, "array_element_size") == 0 ){
    cbf_failnez (cbf_find_column    (cbf, "index"))
      cbf_failnez (cbf_get_integervalue (cbf, &index))
      cbf_failnez (cbf_find_column      (cbf, "size"))
      cbf_failnez (cbf_get_doublevalue        (cbf, &pixel_size))
    
    cbf_failnez(cbf_next_row (cbf))
      cbf_failnez (cbf_find_column    (cbf, "index"))
      cbf_failnez (cbf_get_integervalue (cbf, &index))
      cbf_failnez (cbf_find_column      (cbf, "size"))
      cbf_failnez (cbf_get_doublevalue        (cbf, &pixel_size))
  }
  else {
  }
  /* other pixel information */
  
  if (cbf_find_category  (cbf, "array_intensities") == 0) {
    cbf_failnez (cbf_find_column  (cbf, "gain"))
      cbf_failnez (cbf_get_doublevalue (cbf, &gain))
    cbf_failnez (cbf_find_column  (cbf, "overload"))
      cbf_failnez (cbf_get_integervalue (cbf, &overload))
  }
  else {
  }

  /* Polarization of the incident radiation - daft definition at the moment
     so this code is serving only as a place-holder

     if (cbf_find_category  (cbf, "diffrn_radiation_polarizn_ratio") == 0) {
     cbf_failnez (cbf_get_doublevalue (cbf, &polarrat))
     cbf_double[8] = polarrat;
     cbf_failnez (cbf_find_column  (cbf, "polarization_collimation"))
     cbf_failnez (cbf_get_value (cbf, &polarcoll))
     strcpy(&cbf_char[6][0],polarcoll);
     }
     else {
     cbf_double[8] = -999.0;
     strcpy(cbf_char[6][0],"unspecified collimation");
     }
  */

	}
	/* 
	 *	If the user has supplied NULL for data (return only header info) return.
	 */

	if(NULL == data)
		return(0);

	/*
	 *	Extract the data block.
	 *
	 *	This routine allocates the memory needed to output the data.
	 *	The caller is responsible for freeing this memory after use.
	 */

	if(NULL == (uint_data = (unsigned int *) malloc(dimension[0] * dimension[1] * sizeof (unsigned int))))
	{
		fprintf(stderr, "cbfhandle2img: Error allocating %lu bytes of memory for integer data image\n", 
				(unsigned long)dimension[0] * dimension[1] * sizeof (unsigned int));
		return(1);
	}

	if(NULL == (ushort_data = (unsigned short *) malloc(dimension[0] * dimension[1] * sizeof (unsigned short))))
	{
		fprintf(stderr, "cbfhandle2img: Error allocating %lu bytes of memory for unsigned short data image\n", 
				(unsigned long)dimension[0] * dimension[1] * sizeof (unsigned short));
		return(1);
	}

	/* Find the binary data */
  
	cbf_failnez (cbf_find_category (cbf, "array_data"))
	cbf_failnez (cbf_find_column   (cbf, "array_id"))

	/* fprintf(stderr,"array id: %s\n",array_id); */

	cbf_failnez (cbf_rewind_row    (cbf))
	cbf_failnez (cbf_find_row      (cbf, array_id))
	cbf_failnez (cbf_find_column   (cbf, "data"))
    
	cbf_failnez (cbf_get_integerarray (cbf, &id, uint_data, sizeof(int), 1, dimension[0] * dimension[1], &nelem_read))

	/* 
	 *	first we need to decide which way round the image should be read into the 
	 *	array. This depends on things like the precedence and direction 
	 */

    if(strcmp (direction[0],"increasing") == 0){
      dirsta[0] = dimension[0] - 1;
      dirend[0] = -1;
      dirinc[0] = -1;
    }
    else {
      dirsta[0] = 0;
      dirend[0] = dimension[0];
      dirinc[0] = 1;
    }
    if(strcmp (direction[1],"increasing") == 0){
      dirsta[1] = 0;
      dirend[1] = dimension[1];
      dirinc[1] = 1;
    }
    else{
      dirsta[1] = dimension[1];
      dirend[1] = -1;
      dirinc[1] = -1;
    }
    if (precedence[0]==1){
      first = 1;
      second = 0;
    }
    else{
      first = 0;
      second = 1;
    }

    colrow = 0;

	up = ushort_data;
	ip = uint_data;
	k = dimension[0] * dimension[1];

	for(i = 0; i < k; i++)
		*up++ = (unsigned short) (0x0000ffff & *ip++);
    
	*data = ushort_data;
	free(uint_data);

	/* Success */

	return(0);
}

int cbf2adscimg_sub(char *filename, char **header, unsigned short **data)
{
	FILE		*in;
	cbf_handle	cbf;
	int		err;

	/* Create the cbf handle for the image file*/

	cbf_failnez (cbf_make_handle (&cbf))

	/* Read the file */

	if (NULL == ( in = fopen (filename, "rb")))
	{
		fprintf (stderr, " Couldn't open the CBF file %s\n", filename);
		return (1);
	}

	/* check for CBF format file */

	if(1 == (err = (cbf_read_file (cbf, in, MSG_DIGESTNOW))))
		return(1);

	err = cbfhandle2img_sub(cbf, header, data);

	cbf_failnez (cbf_free_handle (cbf))

	return(err);
}

