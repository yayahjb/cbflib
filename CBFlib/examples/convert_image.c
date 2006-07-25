/**********************************************************************
 * convert_image -- convert an image file to a cbf file               *
 *                                                                    *
 * Version 0.7.1 30 March 2001                                        *
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

	cbf_count_elements (cbf, &i [0]);


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

	cbf_get_image_size (cbf, 0, 0, &i [0], &i [1]);


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
