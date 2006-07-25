/**********************************************************************
 * makecbf -- convert an image file to a cbf file                     *
 *                                                                    *
 * Version 0.6 26 December 1998                                        *
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
#include "img.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

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


int main (int argc, char *argv [])
{
  FILE *in, *out;

  clock_t a,b;

  img_handle img, cbf_img;

  cbf_handle cbf;

  int id, index;

  unsigned int column, row;

  size_t nelem_read;

  double pixel_size, gain, wavelength, distance;

  int overload, dimension [2], precedence [2];

  const char *detector;

  char *detector_char;

  char detector_id [64];

  const char *direction [2], *array_id;


     /* Usage */ 

  if (argc < 3)
  {
    fprintf (stderr, "\n Usage: %s imagefile cbffile\n", argv [0]);

    exit (2);
  }


    /* Read the image */

  img = img_make_handle ();

  a = clock ();

  cbf_failnez (img_read (img, argv [1]))

  b = clock ();

  fprintf (stderr, " Time to read the image: %.3fs\n", ((b - a) * 1.0) / CLOCKS_PER_SEC);


    /* Get some detector parameters */

    /* Detector identifier */

  detector = img_get_field (img, "DETECTOR");

  if (!detector)

    detector = "unknown";

  strncpy (detector_id, detector, 63);

  detector_id [63] = 0;

  detector_char = detector_id;

  while (*detector_char)

    if (isspace (*detector_char))

      memmove (detector_char, detector_char + 1, strlen (detector_char));

    else
    {
      *detector_char = tolower (*detector_char);

      detector_char++;
    }


    /* Pixel size */
    
  pixel_size = img_get_number (img, "PIXEL SIZE") * 0.001;


    /* Wavelength */

  wavelength = img_get_number (img, "WAVELENGTH");
  

    /* Distance */

  distance = img_get_number (img, "DISTANCE") * 0.001;
  

    /* Image size and orientation & gain and overload */

  if (strcmp (detector_id, "mar180") == 0 ||
      strcmp (detector_id, "mar300") == 0)
  {
    gain = 1.08;

    overload = 120000;

    dimension [0] = img_rows (img);
    dimension [1] = img_columns (img);

    precedence [0] = 1;
    precedence [1] = 2;

    direction [0] = "decreasing";
    direction [1] = "increasing";
  }
  else

    if (strcmp (detector_id, "mar345") == 0)
    {
      gain = 1.55;

      overload = 240000;

      dimension [0] = img_columns (img);
      dimension [1] = img_rows (img);

      precedence [0] = 2;
      precedence [1] = 1;

      direction [0] = "increasing";
      direction [1] = "increasing";
    }
    else

      if (strncmp (detector_id, "adscquantum", 11) == 0)
      {
        gain = 0.20;

        overload = 65000;

        dimension [0] = img_columns (img);
        dimension [1] = img_rows (img);

        precedence [0] = 2;
        precedence [1] = 1;

        direction [0] = "increasing";
        direction [1] = "increasing";
      }
      else
      {
        gain = 0.0;

        overload = 0;

        dimension [0] = img_rows (img);
        dimension [1] = img_columns (img);

        precedence [0] = 1;
        precedence [1] = 2;

        direction [0] = NULL;
        direction [1] = NULL;
      }


    /* Make a cbf version of the image */

  a = clock ();
                                                

    /* Create the cbf */

  cbf_failnez (cbf_make_handle (&cbf))


    /* Make a new data block */

  cbf_failnez (cbf_new_datablock (cbf, "image_1"))


    /* Make the _diffrn category */

  cbf_failnez (cbf_new_category (cbf, "diffrn"))
  cbf_failnez (cbf_new_column   (cbf, "id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))


    /* Make the _diffrn_source category */

  cbf_failnez (cbf_new_category (cbf, "diffrn_source"))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "source"))
  cbf_failnez (cbf_set_value    (cbf, "synchrotron"))
  cbf_failnez (cbf_new_column   (cbf, "type"))
  cbf_failnez (cbf_set_value    (cbf, "ssrl crystallography"))


    /* Make the _diffrn_radiation category */  

  cbf_failnez (cbf_new_category (cbf, "diffrn_radiation"))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "wavelength_id"))
  cbf_failnez (cbf_set_value    (cbf, "L1"))


    /* Make the _diffrn_radiation_wavelength category */

  cbf_failnez (cbf_new_category    (cbf, "diffrn_radiation_wavelength"))
  cbf_failnez (cbf_new_column      (cbf, "id"))
  cbf_failnez (cbf_set_value       (cbf, "L1"))
  cbf_failnez (cbf_new_column      (cbf, "wavelength"))

  if (wavelength)
  
    cbf_failnez (cbf_set_doublevalue (cbf, "%.4f", wavelength))

  cbf_failnez (cbf_new_column      (cbf, "wt"))
  cbf_failnez (cbf_set_value       (cbf, "1.0"))


    /* Make the _diffrn_measurement category */  

  cbf_failnez (cbf_new_category (cbf, "diffrn_measurement"))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "method"))
  cbf_failnez (cbf_set_value    (cbf, "oscillation"))
  cbf_failnez (cbf_new_column   (cbf, "sample_detector_distance"))

  if (distance)

    cbf_failnez (cbf_set_doublevalue (cbf, "%.4f", distance))


    /* Make the _diffrn_detector category */  

  cbf_failnez (cbf_new_category (cbf, "diffrn_detector"))
  cbf_failnez (cbf_new_column   (cbf, "id"))
  cbf_failnez (cbf_set_value    (cbf, detector_id))
  cbf_failnez (cbf_new_column   (cbf, "diffrn_id"))
  cbf_failnez (cbf_set_value    (cbf, "DS1"))
  cbf_failnez (cbf_new_column   (cbf, "type"))
  cbf_failnez (cbf_set_value    (cbf, detector))


    /* Make the _diffrn_detector_element category */  

  cbf_failnez (cbf_new_category     (cbf, "diffrn_detector_element"))
  cbf_failnez (cbf_new_column       (cbf, "id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "detector_id"))
  cbf_failnez (cbf_set_value        (cbf, detector_id))


    /* Make the _diffrn_frame_data category */  

  cbf_failnez (cbf_new_category     (cbf, "diffrn_frame_data"))
  cbf_failnez (cbf_new_column       (cbf, "id"))
  cbf_failnez (cbf_set_value        (cbf, "frame_1"))
  cbf_failnez (cbf_new_column       (cbf, "detector_element_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "detector_id"))
  cbf_failnez (cbf_set_value        (cbf, detector_id))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "binary_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))


    /* Make the _array_structure_list category */  

  cbf_failnez (cbf_new_category     (cbf, "array_structure_list"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_row          (cbf))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "index"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 2))
  cbf_failnez (cbf_new_column       (cbf, "dimension"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, dimension [0]))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, dimension [1]))
  cbf_failnez (cbf_new_column       (cbf, "precedence"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, precedence [0]))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, precedence [1]))
  cbf_failnez (cbf_new_column       (cbf, "direction"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_value        (cbf, direction [0]))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_value        (cbf, direction [1]))


    /* Make the _array_element_size category */

  cbf_failnez (cbf_new_category     (cbf, "array_element_size"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_row          (cbf))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "index"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_next_row         (cbf))
  cbf_failnez (cbf_set_integervalue (cbf, 2))
  cbf_failnez (cbf_new_column       (cbf, "size"))

  if (pixel_size > 0)
  {
    cbf_failnez (cbf_rewind_row       (cbf))
    cbf_failnez (cbf_set_doublevalue  (cbf, "%.1fe-6", pixel_size * 1e6))
    cbf_failnez (cbf_next_row         (cbf))
    cbf_failnez (cbf_set_doublevalue  (cbf, "%.1fe-6", pixel_size * 1e6))
  }


    /* Make the _array_intensities category */

  cbf_failnez (cbf_new_category     (cbf, "array_intensities"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "binary_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "linearity"))
  cbf_failnez (cbf_set_value        (cbf, "linear"))
  cbf_failnez (cbf_new_column       (cbf, "gain"))

  if (gain)
  
    cbf_failnez (cbf_set_doublevalue  (cbf, "%.3g", gain))
    
  cbf_failnez (cbf_new_column       (cbf, "overload"))

  if (overload)
    
    cbf_failnez (cbf_set_integervalue (cbf, overload))
    
  cbf_failnez (cbf_new_column       (cbf, "undefined"))
  cbf_failnez (cbf_set_integervalue (cbf, 0))


    /* Make the _array_data category */

  cbf_failnez (cbf_new_category     (cbf, "array_data"))
  cbf_failnez (cbf_new_column       (cbf, "array_id"))
  cbf_failnez (cbf_set_value        (cbf, "image_1"))
  cbf_failnez (cbf_new_column       (cbf, "binary_id"))
  cbf_failnez (cbf_set_integervalue (cbf, 1))
  cbf_failnez (cbf_new_column       (cbf, "data"))


    /* Save the binary data */

  cbf_failnez (cbf_set_integerarray (cbf, CBF_PACKED, 1,
                                 &img_pixel (img, 0, 0), sizeof (int), 1,
                                 img_rows (img) * img_columns (img)))
  

    /* Write the new file */

  out = fopen (argv [2], "w+b");

  if (!out)
  {
    fprintf (stderr, " Couldn't open the CBF file %s\n", argv [2]);

    exit (1);
  }

  cbf_failnez (cbf_write_file (cbf, out, 1, CBF, MSG_DIGEST | MIME_HEADERS, 0))
  

    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))

  b = clock ();

  fprintf (stderr, " Time to write the CBF image: %.3fs\n", 
                                      ((b - a) * 1.0) / CLOCKS_PER_SEC); 


    /* Read the CBF file and compare the image to the original */
    
  a = clock ();


    /* Create the cbf */

  cbf_failnez (cbf_make_handle (&cbf))


    /* Read the file */

  in = fopen (argv [2], "rb");

  if (!in)
  {
    fprintf (stderr, " Couldn't reopen the CBF file %s\n", argv [2]);

    exit (1);
  }

  cbf_failnez (cbf_read_file (cbf, in, MSG_DIGEST))


    /* Get the image identifier */

  cbf_failnez (cbf_rewind_datablock (cbf))
  cbf_failnez (cbf_find_category    (cbf, "diffrn_frame_data"))
  cbf_failnez (cbf_find_column      (cbf, "array_id"))
  cbf_failnez (cbf_get_value        (cbf, &array_id))
  

    /* Get the image dimensions (second dimension = fast, first = slow) */

  cbf_failnez (cbf_find_category    (cbf, "array_structure_list"))
  cbf_failnez (cbf_rewind_row       (cbf))
  cbf_failnez (cbf_find_column      (cbf, "array_id"))

  dimension [0] = dimension [1] = 0;

  while (cbf_find_nextrow (cbf, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (cbf, "precedence"))
    cbf_failnez (cbf_get_integervalue (cbf, &index))

    if (index >= 1 && index <= 2)
    {
      cbf_failnez (cbf_find_column (cbf, "dimension"))

      cbf_failnez (cbf_get_integervalue (cbf, &dimension [2 - index]))
    }
    else

      exit (1);

    cbf_failnez (cbf_find_column (cbf, "array_id"))
  }

  if (dimension [0] == 0 || dimension [1] == 0)

    exit (1);


    /* Create the new image */

  cbf_img = img_make_handle ();

  img_set_dimensions (cbf_img, dimension [0], dimension [1]);


    /* Find the binary data */
  
  cbf_failnez (cbf_find_category (cbf, "array_data"))
  cbf_failnez (cbf_find_column   (cbf, "array_id"))
  cbf_failnez (cbf_find_row      (cbf, array_id))
  cbf_failnez (cbf_find_column   (cbf, "data"))


    /* Read the binary data */
  
  cbf_failnez (cbf_get_integerarray (cbf,
                                 &id, &img_pixel (cbf_img, 0, 0), sizeof (int), 1,
                                 img_rows (cbf_img) * img_columns (cbf_img), &nelem_read))


    /* Free the cbf */

  cbf_failnez (cbf_free_handle (cbf))

  b = clock ();

  fprintf (stderr, " Time to read the CBF image: %.3fs\n", 
                                     ((b - a) * 1.0) / CLOCKS_PER_SEC);


    /* Compare the images */

  if (img_rows (img) != img_rows (cbf_img) || img_columns (img) != img_columns (cbf_img))
  {
    fprintf (stderr, " The dimensions of the CBF image don't match the original\n");

    exit (1);
  }

  for (column = 0; column < (unsigned int) img_columns (cbf_img); column++)

    for (row = 0; row < (unsigned int) img_rows (cbf_img); row++)

      if (img_pixel (cbf_img, column, row) != img_pixel (img, column, row))
      {
        fprintf (stderr, " The CBF image differs from the original at (%d, %d)\n", column, row);

        exit (1);
      }

  fprintf (stderr, " The CBF image matches the original\n");


    /* Free the images */

  img_free_handle (img);

  img_free_handle (cbf_img);


    /* Success */

  return 0;
}
