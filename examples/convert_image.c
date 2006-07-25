/**********************************************************************
 * convert_image -- convert an image file to a cbf file               *
 *                                                                    *
 * Version 0.7.2.3 2 October 2002                                     *
 *                                                                    *
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
#include "img.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>


int main (int argc, char *argv [])
{
  FILE *in, *out;

  img_handle img;

  cbf_handle cbf;

  char detector_type [64], template_name [256], *c;

  const char *detector_name, *axis;

  double wavelength, distance, osc_start, osc_range, time;

  const char *date;

  static const char *monthname [] = 
  
        { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };


    /* Usage */ 

  if (argc < 3)
  {
    fprintf (stderr, "\n Usage: %s imagefile cbffile\n", argv [0]);

    exit (2);
  }


    /* Read the image */

  img = img_make_handle ();

  cbf_failnez (img_read (img, argv [1]))


    /* Identify the detector */

  detector_name = img_get_field (img, "DETECTOR");

  if (!detector_name)

    exit (3);

  for (c = detector_type; *detector_name; detector_name++)

    if (!isspace (*detector_name))

      *c++ = tolower (*detector_name);

  *c = '\0';


    /* Construct the template name */

  sprintf (template_name, "template_%s_%dx%d.cbf", detector_type,
                             img_columns (img),
                             img_rows (img));


    /* Read and modify the template */

  cbf_failnez (cbf_make_handle (&cbf))

  in = fopen (template_name, "rb");

  if (!in)

    exit (4);

  cbf_failnez (cbf_read_template (cbf, in))

    
    /* Wavelength */

  wavelength = img_get_number (img, "WAVELENGTH");

  if (wavelength)

    cbf_failnez (cbf_set_wavelength (cbf, wavelength))
  

    /* Distance */

  distance = img_get_number (img, "DISTANCE");
  
  cbf_failnez (cbf_set_axis_setting (cbf, 0, "DETECTOR_Z", distance, 0))


    /* Oscillation start and range */

  axis = img_get_field (img, "OSCILLATION AXIS");

  if (!axis)

    axis = "PHI";

  osc_start = img_get_number (img, axis);

  osc_range = img_get_number (img, "OSCILLATION RANGE");

  cbf_failnez (cbf_set_axis_setting (cbf, 0, "GONIOMETER_PHI", 
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

    int month, day, hour, minute, second, year;

    year = 0;

    sscanf (date, "%*s %s %d %d:%d:%d %d", monthstring,
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

  cbf_failnez (cbf_set_image (cbf, 0, 0, CBF_PACKED,
                              &img_pixel (img, 0, 0), sizeof (int), 1,
                               img_columns (img), img_rows (img)))


    /* Write the new file */

  out = fopen (argv [2], "w+b");

  if (!out)
  {
    fprintf (stderr, " Couldn't open the CBF file %s\n", argv [2]);

    exit (1);
  }

  cbf_failnez (cbf_write_file (cbf, out, 1, CBF, 
                               MSG_DIGEST | MIME_HEADERS, 0))

/*****************************************************************************/

  {
	  const char *id;

	  double d [4];

	  int i [4];

	  unsigned int u [4];

	  size_t s [4];

	  cbf_goniometer goniometer;

	  cbf_detector detector;


  /* Change the diffrn.id entry in all the categories */

	cbf_set_diffrn_id (cbf, "TEST");


  /* Get the diffrn.id entry */

	cbf_get_diffrn_id (cbf, &id);

    
  /* Change the diffrn.crystal_id entry */

	cbf_set_crystal_id (cbf, "CTEST");


  /* Get the diffrn.crystal_id entry */

	cbf_get_crystal_id (cbf, &id);

    
  /* Set the wavelength */

	cbf_set_wavelength (cbf, 2.14);


  /* Get the wavelength */

	cbf_get_wavelength (cbf, &wavelength);


  /* Set the polarization */

	cbf_set_polarization (cbf, 0.5, 0.75);


  /* Get the polarization */

	cbf_get_polarization (cbf, &d [0], &d [1]);


  /* Set the divergence */

	cbf_set_divergence (cbf, 0.3, 0.4, 0.5);


  /* Get the divergence */

	cbf_get_divergence (cbf, &d [0], &d [1], &d [2]);


  /* Get the number of elements */

	cbf_count_elements (cbf, &u [0]);


  /* Get the element id */

	cbf_get_element_id (cbf, 0, &id);

                                           
  /* Set the gain of a detector element */

	cbf_set_gain (cbf, 0, 0.24, 0.04);


  /* Get the gain of a detector element */

	cbf_get_gain (cbf, 0, &d [0], &d [1]);


  /* Set the overload value of a detector element */

	cbf_set_overload (cbf, 0, 100000);


  /* Get the overload value of a detector element */

	cbf_get_overload (cbf, 0, &d [0]);


  /* Set the integration time */

	cbf_set_integration_time (cbf, 0, 10.1);

                                                 
  /* Get the integration time */

	cbf_get_integration_time (cbf, 0, &d [0]);


  /* Set the collection date and time (1) as seconds since January 1 1970 */

	cbf_set_timestamp (cbf, 0, 1000.0, CBF_NOTIMEZONE, 0.1);


  /* Get the collection date and time (1) as seconds since January 1 1970 */

	cbf_get_timestamp (cbf, 0, &d [0], &i [0]);


  /* Get the image size */

	cbf_get_image_size (cbf, 0, 0, &s [0], &s [1]);


  /* Change the setting of an axis */

	cbf_set_axis_setting (cbf, 0, "GONIOMETER_PHI", 27.0, 0.5);


  /* Get the setting of an axis */

	cbf_get_axis_setting (cbf, 0, "GONIOMETER_PHI", &d [0], &d [1]);


  /* Construct a goniometer */

	cbf_construct_goniometer (cbf, &goniometer);


  /* Get the rotation axis */

	cbf_get_rotation_axis (goniometer, 0, &d [0], &d [1], &d [2]);


  /* Get the rotation range */

	cbf_get_rotation_range (goniometer, 0, &d [0], &d [1]);


  /* Reorient a vector */

	cbf_rotate_vector (goniometer, 0, 0.5, 0.3, 0, 1, &d [0], &d [1], &d [2]);


  /* Convert a vector to reciprocal space */

	cbf_get_reciprocal (goniometer, 0, 0.3, 0.98, 1, 2, -3, &d [0], &d [1], &d [2]);


  /* Construct a detector positioner */

	cbf_construct_detector (cbf, &detector, 0);


  /* Get the beam center */

	cbf_get_beam_center (detector, &d [0], &d [1], &d [2], &d [3]);


  /* Get the detector distance */

	cbf_get_detector_distance (detector, &d [0]);


  /* Get the detector normal */

	cbf_get_detector_normal (detector, &d [0], &d [1], &d [2]);


  /* Calcluate the coordinates of a pixel */

	cbf_get_pixel_coordinates (detector, 1, 3, &d [0], &d [1], &d [2]);


  /* Calcluate the area of a pixel */

	cbf_get_pixel_area (detector, 1, 3, &d [0], &d [1]);


  /* Free a detector */

	cbf_free_detector (detector);


  /* Free a goniometer */

	cbf_free_goniometer (goniometer);
	}


/*****************************************************************************/

    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))


    /* Free the image */

  img_free_handle (img);


    /* Success */

  return 0;
}
