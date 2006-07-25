/**********************************************************************
 * cbf_simple -- cbflib simplified API functions                      *
 *                                                                    *
 * Version 0.7.2 22 April 2001                                        *
 * Version 0.7.2.1 7 May 2001                                         *
 * Version 0.7.2.2 17 July 2001                                       *
 * Version 0.7.2.3 14 August 2002                                     *
 *                                                                    *
 *            Paul Ellis (ellis@ssrl.slac.stanford.edu) and           *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
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

#ifdef __cplusplus

extern "C" {

#endif

#include <ctype.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_binary.h"
#include "cbf_simple.h"
#include "cbf_string.h"


  /* Read a template file */

int cbf_read_template (cbf_handle handle, FILE *stream)
{
    /* Read the file */

  cbf_failnez (cbf_read_file (handle, stream, MSG_NODIGEST))


    /* Find the first datablock */

  cbf_failnez (cbf_select_datablock (handle, 0))

  return 0;
}


  /* Get the diffrn.id entry */

int cbf_get_diffrn_id (cbf_handle handle, const char **diffrn_id)
{
  cbf_failnez (cbf_find_category (handle, "diffrn"));
  cbf_failnez (cbf_find_column   (handle, "id"));
  cbf_failnez (cbf_get_value     (handle, diffrn_id))

  return 0;
}

    
  /* Change the diffrn.id entry in all the categories */

int cbf_set_diffrn_id (cbf_handle handle, const char *diffrn_id)
{
  int code;

  static char *categories [] = { "diffrn_source",
                                 "diffrn_radiation",
                                 "diffrn_detector",
                                 "diffrn_measurement", 0 },
              **category;

  cbf_failnez (cbf_find_category (handle, "diffrn"))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_set_value     (handle, diffrn_id))

  for (category = categories; *category; category++)
  {
    code = cbf_find_category (handle, *category);

    if (code != CBF_NOTFOUND)
    {
      if (code)

        return code;

      cbf_failnez (cbf_find_column  (handle, "diffrn_id"))

      do

        cbf_failnez (cbf_set_value (handle, diffrn_id))

      while (cbf_next_row (handle));
    }
  }

  return 0;
}


  /* Get the diffrn.crystal_id entry */

int cbf_get_crystal_id (cbf_handle handle, const char **crystal_id)
{
  cbf_failnez (cbf_find_category (handle, "diffrn"));
  cbf_failnez (cbf_find_column   (handle, "crystal_id"));
  cbf_failnez (cbf_get_value     (handle, crystal_id))

  return 0;
}

    
  /* Change the diffrn.crystal_id entry */

int cbf_set_crystal_id (cbf_handle handle, const char *crystal_id)
{
  cbf_failnez (cbf_find_category (handle, "diffrn"))
  cbf_failnez (cbf_find_column   (handle, "crystal_id"))
  cbf_failnez (cbf_set_value     (handle, crystal_id))

  return 0;
}


  /* Get the wavelength */

int cbf_get_wavelength (cbf_handle handle, double *wavelength)
{
  const char *diffrn_id, *wavelength_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Get the wavelength id */

  cbf_failnez (cbf_find_category (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column   (handle, "wavelength_id"))
  cbf_failnez (cbf_get_value     (handle, &wavelength_id))


    /* Get the wavelength */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation_wavelength"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, wavelength_id))
  cbf_failnez (cbf_find_column     (handle, "wavelength"))
  cbf_failnez (cbf_get_doublevalue (handle, wavelength))

  return 0;
}


  /* Set the wavelength */

int cbf_set_wavelength (cbf_handle handle, double wavelength)
{
    /* Get the wavelength id */

  const char *wavelength_id;

  cbf_failnez (cbf_find_category (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column   (handle, "wavelength_id"))
  cbf_failnez (cbf_get_value     (handle, &wavelength_id))


    /* Update the diffrn_radiation_wavelength category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation_wavelength"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, wavelength_id))
  cbf_failnez (cbf_find_column     (handle, "wavelength"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", wavelength))
  cbf_failnez (cbf_find_column     (handle, "wt"))
  cbf_failnez (cbf_set_value       (handle, "1.0"))

  return 0;
}


  /* Get the polarization */

int cbf_get_polarization (cbf_handle handle, double *polarizn_source_ratio,
                                             double *polarizn_source_norm)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Get the polarization */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_ratio"))
  cbf_failnez (cbf_get_doublevalue (handle, polarizn_source_ratio))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_norm"))
  cbf_failnez (cbf_get_doublevalue (handle, polarizn_source_norm))

  return 0;
}


  /* Set the polarization */

int cbf_set_polarization (cbf_handle handle, double polarizn_source_ratio,
                                             double polarizn_source_norm)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Update the diffrn_radiation category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_ratio"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", polarizn_source_ratio))
  cbf_failnez (cbf_find_column     (handle, "polarizn_source_norm"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", polarizn_source_norm))

  return 0;
}


  /* Get the divergence */

int cbf_get_divergence (cbf_handle handle, double *div_x_source,
                                           double *div_y_source,
                                           double *div_x_y_source)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Get the divergence */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "div_x_source"))
  cbf_failnez (cbf_get_doublevalue (handle, div_x_source))
  cbf_failnez (cbf_find_column     (handle, "div_y_source"))
  cbf_failnez (cbf_get_doublevalue (handle, div_y_source))
  cbf_failnez (cbf_find_column     (handle, "div_x_y_source"))
  cbf_failnez (cbf_get_doublevalue (handle, div_x_y_source))

  return 0;
}


  /* Set the divergence */

int cbf_set_divergence (cbf_handle handle, double div_x_source,
                                           double div_y_source,
                                           double div_x_y_source)
{
  const char *diffrn_id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))


    /* Update the diffrn_radiation category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_radiation"))
  cbf_failnez (cbf_find_column     (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row        (handle, diffrn_id))
  cbf_failnez (cbf_find_column     (handle, "div_x_source"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", div_x_source))
  cbf_failnez (cbf_find_column     (handle, "div_y_source"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", div_y_source))
  cbf_failnez (cbf_find_column     (handle, "div_x_y_source"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", div_x_y_source))

  return 0;
}


    /* Get the number of elements */

int cbf_count_elements (cbf_handle handle, unsigned int *elements)
{
  const char *diffrn_id, *id;

  int errorcode;

  unsigned int count;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector_element"))
  cbf_failnez (cbf_find_column   (handle, "detector_id"))

  for (count = 0, errorcode = 0; !errorcode; count++)

    errorcode = cbf_find_nextrow (handle, id);

  count--;

  if (errorcode != CBF_NOTFOUND)

    return errorcode;

  if (elements)

    *elements = count;

  return 0;
}


  /* Get the element id */

int cbf_get_element_id (cbf_handle handle, unsigned int element_number,
                                           const char **element_id)
{
  const char *diffrn_id, *id;


    /* Get the diffrn.id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector_element"))
  cbf_failnez (cbf_find_column   (handle, "detector_id"))

  do

    cbf_failnez (cbf_find_nextrow (handle, id))

  while (element_number--);

  cbf_failnez (cbf_find_column (handle, "id"))
  cbf_failnez (cbf_get_value   (handle, element_id))

  return 0;
}


int cbf_get_array_id (cbf_handle handle, unsigned int element_number,
                                         const char **array_id)
{
  const char *element_id;

  cbf_failnez (cbf_get_element_id (handle, element_number, &element_id))
  cbf_failnez (cbf_find_category  (handle, "diffrn_data_frame"))
  cbf_failnez (cbf_find_column    (handle, "detector_element_id"))
  cbf_failnez (cbf_find_row       (handle, element_id))
  cbf_failnez (cbf_find_column    (handle, "array_id"))
  cbf_failnez (cbf_get_value      (handle, array_id))

  return 0;
}


  /* Get the gain of a detector element */

int cbf_get_gain (cbf_handle handle, unsigned int element_number,
                                     double *gain, double *gain_esd)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Get the gain */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "gain"))
  cbf_failnez (cbf_get_doublevalue (handle, gain))
  cbf_failnez (cbf_find_column     (handle, "gain_esd"))
  cbf_failnez (cbf_get_doublevalue (handle, gain_esd))

  return 0;
}


  /* Set the gain of a detector element */

int cbf_set_gain (cbf_handle handle, unsigned int element_number,
                                     double gain, double gain_esd)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Update the array_intensities category */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "gain"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", gain))
  cbf_failnez (cbf_find_column     (handle, "gain_esd"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", gain_esd))

  return 0;
}


  /* Get the overload value of a detector element */

int cbf_get_overload (cbf_handle handle, unsigned int element_number,
                                         double *overload)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Get the overload value */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "overload"))
  cbf_failnez (cbf_get_doublevalue (handle, overload))

  return 0;
}


  /* Set the overload value of a detector element */

int cbf_set_overload (cbf_handle handle, unsigned int element_number,
                                         double overload)
{
  const char *array_id;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Update the array_intensities category */

  cbf_failnez (cbf_find_category   (handle, "array_intensities"))
  cbf_failnez (cbf_find_column     (handle, "array_id"))
  cbf_failnez (cbf_find_row        (handle, array_id))
  cbf_failnez (cbf_find_column     (handle, "overload"))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", overload))

  return 0;
}


  /* Get the integration time */

int cbf_get_integration_time (cbf_handle    handle, 
                              unsigned int  reserved,
                              double       *time)
{
  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Update the diffrn_scan_frame category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_find_column     (handle, "integration_time"))
  cbf_failnez (cbf_rewind_row      (handle))
  cbf_failnez (cbf_get_doublevalue (handle, time))

  return 0;
}


  /* Set the integration time */

int cbf_set_integration_time (cbf_handle   handle,
                              unsigned int reserved,
                              double       time)
{
  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Update the diffrn_scan_frame category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_find_column     (handle, "integration_time"))
  cbf_failnez (cbf_rewind_row      (handle))
  cbf_failnez (cbf_set_doublevalue (handle, "%.6g", time))

  return 0;
}


  /* Convert gregorian to julian date (in days) */

double cbf_gregorian_julian (int    year, 
                             int    month, 
                             int    day, 
                             int    hour, 
                             int    minute,
                             double second)
{
  static days [] = {   0,  31,  59,  90, 120, 151,
                     181, 212, 243, 273, 304, 334, 365 };

  second += minute * 60.0 + hour * 3600.0 + (day - 1) * 86400.0;

  second += days [month - 1] * 86400.0;

  if (month > 2 && (year % 4) == 0 && year != 1900 && year != 2100)

    second += 86400.0;

  second += ((365 * (year - 1)) + floor ((year - 1) / 4)
                                - floor ((year - 1) / 100)
                                + floor ((year - 1) / 400)) * 86400.0;

  return second / 86400.0 + 1721425.5;
}



  /* Get the collection date and time (1) as seconds since January 1 1970 */

int cbf_get_timestamp (cbf_handle handle, unsigned int  reserved,
                                          double       *time,
                                          int          *timezone)
{
  int year, month, day, hour, minute;

  double second;

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_get_datestamp (handle, reserved, &year, &month, &day,
                                  &hour, &minute, &second, timezone))

  if (time)

    *time = (cbf_gregorian_julian (year, month, day, 
                                       hour, minute, second)
             - 2440587.5) * 86400.0;

  return 0;
}


  /* Get the collection date and time (2) as individual fields */

int cbf_get_datestamp (cbf_handle handle, unsigned int  reserved,
                                          int          *year,
                                          int          *month,
                                          int          *day,
                                          int          *hour,
                                          int          *minute,
                                          double       *second,
                                          int          *timezone)
{
  const char *date;
    
  char ftzsign;

  int fyear, fmonth, fday, fhour, fminute, ftzhour, ftzminute, parsed;

  double fsecond;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Read the diffrn_scan_frame category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_find_column     (handle, "date"))
  cbf_failnez (cbf_rewind_row      (handle))
  cbf_failnez (cbf_get_value       (handle, &date))


    /* Parse the string */

  fsecond = 
  fyear   = 
  fmonth  = 
  fday    = 
  fhour   = 
  fminute = 
  ftzsign =
  ftzhour = 
  ftzminute = 0;

  parsed = sscanf (date, "%d-%d-%d%*c%d:%d:%lf%c%d:%d", 
                                     &fyear, &fmonth, &fday, &fhour,
                                     &fminute, &fsecond, 
                                     &ftzsign, &ftzhour, &ftzminute);

  if (parsed < 3 || (parsed == 7 && strchr (" \t\n", ftzsign) == NULL)
                 || (parsed >  7 && strchr ("+-",    ftzsign) == NULL))

    return CBF_FORMAT;

  if (fyear < 0 || fyear > 9999 
                || fmonth < 1 
                || fmonth > 12 
                || fday < 1 
                || fday > 31 
                || fhour < 0
                || fhour > 23
                || fminute < 0
                || fminute > 59
                || fsecond < 0
                || fsecond >= 60
                || ftzhour < 0
                || ftzhour > 13
                || ftzminute < 0
                || ftzminute > 59)

    return CBF_FORMAT;

  if (year)     *year     = fyear;
  if (month)    *month    = fmonth;
  if (day)      *day      = fday;
  if (hour)     *hour     = fhour;
  if (minute)   *minute   = fminute;
  if (second)   *second   = fsecond;

  if (timezone)

    if (parsed > 7)
    {
      *timezone = ftzhour * 60 + ftzminute;

      if (ftzsign == '-')

        *timezone = -*timezone;
    }
    else

      *timezone = CBF_NOTIMEZONE;

  return 0;
}


  /* Set the collection date and time (1) as seconds since January 1 1970 */

int cbf_set_timestamp (cbf_handle handle, unsigned int reserved,
                                          double       time, 
                                          int          timezone,
                                          double       precision)
{
  int month, monthstep, year, day, hour, minute;

  double second, date;
    
  if (reserved != 0)

    return CBF_ARGUMENT;

    
  date = time / 86400.0 + 2440587.5;

  if (date < 1721060.5 || date > 5373484.5)

    return CBF_ARGUMENT;


   /* Find the year and month with a binary search */

  for (monthstep = 65536, month = 0; monthstep; monthstep >>= 1)
  {
    month += monthstep;

    if (cbf_gregorian_julian (month / 12, 
                             (month % 12) + 1, 1, 0, 0, 0) > date)

     month -= monthstep;
  }


    /* Calculate the day, hour, minute and second */

  year  =  month / 12;
  month = (month % 12) + 1;

  date -= cbf_gregorian_julian (year, month, 1, 0, 0, 0);

  day = (int) floor (date) + 1;

  date -= (day - 1);

  hour = (int) floor (date * 24.0);

  date -= hour / 24.0;

  minute = (int) floor (date * 1440.0);

  date -= minute / 1440.0;

  second = date * 86400.0;

    
    /* Set the new date */

  cbf_failnez (cbf_set_datestamp (handle, reserved, year, month, day,
                                     hour, minute, second, 
                                     timezone, precision))

  return 0;
}


  /* Set the collection date and time (2) as individual fields */

int cbf_set_datestamp (cbf_handle handle, unsigned int reserved,
                                          int          year,
                                          int          month,
                                          int          day,
                                          int          hour,
                                          int          minute,
                                          double       second,
                                          int          timezone,
                                          double       precision)
{
  char date [256];

  int nsf;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Print the date in CIF format */

  if (year < 0 || year > 9999 
               || month < 1 
               || month > 12 
               || day < 1 
               || day > 31 
               || hour < 0
               || hour > 23
               || minute < 0
               || minute > 59
               || second < 0
               || second >= 60)

    return CBF_ARGUMENT;

  if (timezone != CBF_NOTIMEZONE)

    if (timezone < -780 || timezone > 780)

      return CBF_ARGUMENT;


  nsf = 0;

  if (precision > 0 && precision < 1)

    nsf = (int) (-log10 (precision) + 0.5);

  sprintf (date, "%04d-%02d-%02dT%02d:%02d:%0*.*f", year, month, day, 
                   hour, minute, nsf == 0 ? 2 : nsf + 3, nsf, second);

  if (timezone != CBF_NOTIMEZONE)

    sprintf (date + strlen (date), "%c%02d:%02d", timezone < 0 ? '-' : '+',
                                                  abs (timezone) / 60,
                                                  abs (timezone) % 60); 


    /* Update the diffrn_scan_frame category */

  cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame"))
  cbf_failnez (cbf_find_column     (handle, "date"))
  cbf_failnez (cbf_rewind_row      (handle))
  cbf_failnez (cbf_set_value       (handle, date))

  return 0;
}


  /* Set the collection date and time (3) as current time to the second */

int cbf_set_current_timestamp (cbf_handle handle, unsigned int reserved,
                                                  int timezone)
{
  time_t timer;

  if (reserved != 0)

    return CBF_ARGUMENT;

  timer = time (NULL);

  if (timezone != CBF_NOTIMEZONE)

    timer += timezone * 60;

  cbf_failnez (cbf_set_timestamp (handle, reserved, timer, timezone, 1))

  return 0;
}


  /* Read an image */

int cbf_get_image_size (cbf_handle    handle,
                        unsigned int  reserved, 
                        unsigned int  element_number,
                        size_t       *ndim1,
                        size_t       *ndim2)
{
  const char *array_id;

  int done [3], precedence, dimension [3];

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Get the dimensions from the array_structure_list category */

  done [1] = done [2] = 0;

  dimension [1] = dimension [2] = 1;

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column      (handle, "dimension"))
    cbf_failnez (cbf_get_integervalue (handle, &dimension [precedence]))

    if (done [precedence])

      return CBF_FORMAT;

    done [precedence] = 1;

    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!done [1])

    return CBF_NOTFOUND;

  if (!done [2])
  {
    if (ndim1)

      *ndim1 = dimension [1];

    if (ndim2)

      *ndim2 = 1;
  }
  else
  {
    if (ndim1)

      *ndim1 = dimension [2];

    if (ndim2)

      *ndim2 = dimension [1];
  }

  return 0;
}


  /* Read a binary section into an image */

int cbf_get_image (cbf_handle    handle,
                   unsigned int  reserved, 
                   unsigned int  element_number, 
                   void         *array, 
                   size_t        elsize, 
                   int           elsign,
                   size_t        ndim1,
                   size_t        ndim2)
{
  const char *direction_string, *array_id;
  
  int code, done [3], precedence, direction [3], binary_id, dir1, dir2, 
      index1, index2, start1, end1, inc1, start2, end2, inc2;

  size_t nelem_read, dim1, dim2;

  char tmp [32], *pixel, *pixel2, *last;

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Get the index dimensions */
    
  cbf_failnez (cbf_get_image_size (handle, reserved, element_number,
                                   &dim1, &dim2))
                                   
                                   
    /* Check that the fast dimensions correspond */
    
  if (dim2 != ndim2)
  
    return CBF_ARGUMENT;

    
    /* Get the index directions from the array_structure_list category */

  done [1] = done [2] = 0;

  direction [1] = direction [2] = 1;

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    code = cbf_find_column (handle, "direction");
    
    if (code == 0)
    {
      cbf_failnez (cbf_get_value (handle, &direction_string))
    
      if (cbf_cistrcmp ("decreasing", direction_string) == 0)
    
        direction [precedence] = -1;
    }
    else
    
      if (code != CBF_NOTFOUND)
      
        return code;
    
    if (done [precedence])

      return CBF_FORMAT;

    done [precedence] = 1;

    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!done [1])

    return CBF_NOTFOUND;

  if (!done [2])
  {
    dir1 = direction [1];

    dir2 = 1;
  }
  else
  {
    dir1 = direction [2];

    dir2 = direction [1];
  }


    /* Find the binary data */
  
  cbf_failnez (cbf_find_category (handle, "array_data"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))
  cbf_failnez (cbf_find_row      (handle, array_id))
  cbf_failnez (cbf_find_column   (handle, "data"))


    /* Read the binary data */

  if (ndim1 * ndim2 <= 0)

    return CBF_ARGUMENT;
  
  cbf_failnez (cbf_get_integerarray (handle, &binary_id, 
               array, elsize, elsign, ndim1 * ndim2, &nelem_read))


    /* Reorder the data if necessary */
    
#ifndef CBF_0721_READS

  if (dir1 < 0 || dir2 < 0)
  {
    if (dir1 >= 0)
    {
      start1 = 0;
      end1 = ndim1;
      inc1 = 1;
    }
    else
    {
      start1 = ndim1 - 1;
      end1 = -1;
      inc1 = -1;
    }

    if (dir2 >= 0)
    {
      start2 = 0;
      end2 = ndim2;
      inc2 = 1;
    }
    else
    {
      start2 = ndim2 - 1;
      end2 = -1;
      inc2 = -1;
    }
    
    pixel = (char *) array;
    
    for (index1 = start1; index1 != end1; index1 += inc1)
    
      for (index2 = start2; index2 != end2; index2 += inc2)
      {
        pixel2 = ((char *) array) + (index1 * ndim2 + index2) * elsize;

        if (pixel < pixel2)

          if (elsize == sizeof (int))
          {
            *((int *) tmp)    = *((int *) pixel);
            *((int *) pixel)  = *((int *) pixel2);
            *((int *) pixel2) = *((int *) tmp);
          }
          else
          {
            memcpy (tmp, pixel, elsize);
            memcpy (pixel, pixel2, elsize);
            memcpy (pixel2, tmp, elsize);
          }

        pixel += elsize;        
      }
  }

#endif
    
  if (ndim1 * ndim2 != nelem_read)

    return CBF_ENDOFDATA;

  return 0;
}


  /* Save an image.  ndim1 is the slow dimension, ndim2 is fast. */

int cbf_set_image (cbf_handle    handle,
                   unsigned int  reserved,
                   unsigned int  element_number,
                   unsigned int  compression,
                   void         *array, 
                   size_t        elsize,
                   int           elsign, 
                   size_t        ndim1,
                   size_t        ndim2)
{
  const char *array_id;

  int binary_id, done [3], precedence, dimension [3];

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_get_array_id (handle, element_number, &array_id));


    /* Update the array_structure_list category */

  if (ndim1 == 0)
        
    dimension [2] = 1;

  else

    dimension [2] = ndim1;

  if (ndim2 == 0)
        
    dimension [1] = 1;

  else

    dimension [1] = ndim2;

  done [1] = dimension [1] == 1;
  done [2] = dimension [2] == 1;

  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column      (handle, "dimension"))
    cbf_failnez (cbf_set_integervalue (handle, dimension [precedence]))

    done [precedence] = 1;

    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!done [1] || !done [2])

    return CBF_NOTFOUND;


    /* Get the binary_id */

  cbf_failnez (cbf_find_category    (handle, "array_data"))
  cbf_failnez (cbf_find_column      (handle, "array_id"))
  cbf_failnez (cbf_find_row         (handle, array_id))
  cbf_failnez (cbf_find_column      (handle, "binary_id"))
  cbf_failnez (cbf_get_integervalue (handle, &binary_id))
  cbf_failnez (cbf_find_column      (handle, "data"))


    /* Save the array */

  cbf_failnez (cbf_set_integerarray (handle, compression, binary_id,
                                     array, elsize, elsign,
                                     dimension [1] * dimension [2]))

  return 0;
}


  /* Get the type of an axis */

int cbf_get_axis_type (cbf_handle handle, const char *axis_id,
                                          cbf_axis_type *axis_type)
{
  const char *type;


    /* Get the axis type */

  cbf_failnez (cbf_find_category (handle, "axis"))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_find_row      (handle, axis_id))
  cbf_failnez (cbf_find_column   (handle, "type"))
  cbf_failnez (cbf_get_value     (handle, &type))

  if (!type)

    return CBF_NOTFOUND;

  if (toupper (*type) != 'T' && 
      toupper (*type) != 'R' &&
      toupper (*type) != 'G')

    return CBF_FORMAT;

  if (axis_type)

    if (toupper (*type) == 'T')
        
      *axis_type = CBF_TRANSLATION_AXIS;

    else
    
      if (toupper (*type) == 'R')
        
        *axis_type = CBF_ROTATION_AXIS;

      else
                
        *axis_type = CBF_GENERAL_AXIS;

  return 0;
}

    
  /* Get an axis vector */

int cbf_get_axis_vector (cbf_handle handle, const char *axis_id,
                                            double *vector1,
                                            double *vector2,
                                            double *vector3)
{
    /* Read from the axis category */

  cbf_failnez (cbf_find_category   (handle, "axis"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, axis_id))
  cbf_failnez (cbf_find_column     (handle, "vector[1]"))
  cbf_failnez (cbf_get_doublevalue (handle, vector1))
  cbf_failnez (cbf_find_column     (handle, "vector[2]"))
  cbf_failnez (cbf_get_doublevalue (handle, vector2))
  cbf_failnez (cbf_find_column     (handle, "vector[3]"))
  cbf_failnez (cbf_get_doublevalue (handle, vector3))

  return 0;
}


  /* Get an axis offset */

int cbf_get_axis_offset (cbf_handle handle, const char *axis_id,
                                            double *offset1,
                                            double *offset2,
                                            double *offset3)
{
    /* Read from the axis category */

  cbf_failnez (cbf_find_category   (handle, "axis"))
  cbf_failnez (cbf_find_column     (handle, "id"))
  cbf_failnez (cbf_find_row        (handle, axis_id))
  cbf_failnez (cbf_find_column     (handle, "offset[1]"))
  cbf_failnez (cbf_get_doublevalue (handle, offset1))
  cbf_failnez (cbf_find_column     (handle, "offset[2]"))
  cbf_failnez (cbf_get_doublevalue (handle, offset2))
  cbf_failnez (cbf_find_column     (handle, "offset[3]"))
  cbf_failnez (cbf_get_doublevalue (handle, offset3))

  return 0;
}


  /* Get the setting of an axis */

int cbf_get_axis_setting (cbf_handle handle, unsigned int  reserved, 
                                             const char   *axis_id,
                                             double       *start, 
                                             double       *increment)
{
  cbf_axis_type type;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Get the axis type */

  cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

  if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS)

    return CBF_FORMAT;


    /* Read from the diffrn_scan_axis and 
                     diffrn_scan_frame_axis categories */

  if (type == CBF_TRANSLATION_AXIS)
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "displacement"))
    cbf_failnez (cbf_get_doublevalue (handle, start))

    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, increment))
  }
  else
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "angle"))
    cbf_failnez (cbf_get_doublevalue (handle, start))

    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "angle_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, increment))
  }

  return 0;
}


  /* Change the setting of an axis */

int cbf_set_axis_setting (cbf_handle handle, unsigned int  reserved,
                                             const char   *axis_id,
                                             double        start, 
                                             double        increment)
{
  cbf_axis_type type;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Get the axis type */

  cbf_failnez (cbf_get_axis_type (handle, axis_id, &type))

  if (type != CBF_TRANSLATION_AXIS && type != CBF_ROTATION_AXIS)

    return CBF_FORMAT;


    /* Update the diffrn_scan_axis and 
                  diffrn_scan_frame_axis categories */

  if (type == CBF_TRANSLATION_AXIS)
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "displacement"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", start))

    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "displacement_start"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", start))
    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", increment))
    cbf_failnez (cbf_find_column     (handle, "displacement_range"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", increment))
  }
  else
  {
    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_frame_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "angle"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", start))

    cbf_failnez (cbf_find_category   (handle, "diffrn_scan_axis"))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_find_row        (handle, axis_id))
    cbf_failnez (cbf_find_column     (handle, "angle_start"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", start))
    cbf_failnez (cbf_find_column     (handle, "angle_increment"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", increment))
    cbf_failnez (cbf_find_column     (handle, "angle_range"))
    cbf_failnez (cbf_set_doublevalue (handle, "%.6g", increment))
  }

  return 0;
}


  /* Create a positioner */

int cbf_make_positioner (cbf_positioner *positioner)
{
  cbf_failnez (cbf_alloc ((void **) positioner, NULL, 
               sizeof (cbf_positioner_struct), 1))

  (*positioner)->matrix [0][0] = 1;
  (*positioner)->matrix [0][1] = 0;
  (*positioner)->matrix [0][2] = 0;
  (*positioner)->matrix [0][3] = 0;
  (*positioner)->matrix [1][0] = 0;
  (*positioner)->matrix [1][1] = 1;
  (*positioner)->matrix [1][2] = 0;
  (*positioner)->matrix [1][3] = 0;
  (*positioner)->matrix [2][0] = 0;
  (*positioner)->matrix [2][1] = 0;
  (*positioner)->matrix [2][2] = 1;
  (*positioner)->matrix [2][3] = 0;

  (*positioner)->axis = NULL;

  (*positioner)->axes = 0;

  (*positioner)->matrix_is_valid = 1;

  (*positioner)->axes_are_connected = 1;

  return 0;
}


  /* Free a positioner */

int cbf_free_positioner (cbf_positioner positioner)
{
  int errorcode;
    
  size_t i;
  
  if (positioner)
  {
    errorcode = 0;

    for (i = 0; i < positioner->axes; i++)

      errorcode |= cbf_free ((void **) &positioner->axis [i].name, NULL);

    errorcode |= cbf_free ((void **) &positioner->axis, 
                                     &positioner->axes);

    return errorcode | cbf_free ((void **) &positioner, NULL);
  }

  return 0;
}


  /* Add a positioner axis */

int cbf_add_positioner_axis (cbf_positioner positioner,
                             const char    *name, 
                             const char    *depends_on,
                             cbf_axis_type  type, 
                             double         vector1, 
                             double         vector2, 
                             double         vector3,
                             double         offset1, 
                             double         offset2, 
                             double         offset3,
                             double         start,
                             double         increment)
{
  int errorcode;

  cbf_axis_struct axis;

  double length;


    /* Check the arguments */

  if (!name || !positioner || (type != CBF_TRANSLATION_AXIS &&
                               type != CBF_ROTATION_AXIS))

    return CBF_ARGUMENT;

  length = vector1 * vector1 + vector2 * vector2 + vector3 * vector3;

  if (length <= 0.0)

    return CBF_ARGUMENT;


    /* Allocate memory and copy the axis names */

  cbf_failnez (cbf_alloc ((void **) &axis.name, NULL, strlen (name) + 1, 1))

  axis.depends_on = NULL;

  if (depends_on)

    errorcode = cbf_alloc ((void **) &axis.depends_on, NULL, 
                                   strlen (depends_on) + 1, 1);

  if (errorcode)

    return errorcode | cbf_free ((void **) &axis.name, NULL);

  errorcode = cbf_realloc ((void **) &(positioner->axis), 
                                     &(positioner->axes),
                                sizeof (cbf_axis_struct),
                                  positioner->axes + 1);

  if (errorcode)

    return cbf_free ((void **) &axis.name, NULL) |
           cbf_free ((void **) &axis.depends_on, NULL);

  strcpy (axis.name, name);

  if (depends_on)

    strcpy (axis.depends_on, depends_on);

  length = sqrt (length);

  axis.type = type;

  axis.vector [0] = vector1 / length;
  axis.vector [1] = vector2 / length;
  axis.vector [2] = vector3 / length;

  axis.offset [0] = offset1;
  axis.offset [1] = offset2;
  axis.offset [2] = offset3;

  axis.start     = start;
  axis.increment = increment;
  axis.setting   = 0;

  positioner->axis [positioner->axes - 1] = axis;

  positioner->matrix_is_valid = 0;

  positioner->axes_are_connected = 0;

  return 0;
}


  /* Add a goniometer axis from a file */

int cbf_read_positioner_axis (cbf_handle      handle,
                              unsigned int    reserved,
                              cbf_positioner  positioner,
                              const char     *axis_id,
                              int             read_setting)
{
  const char *next_id;

  cbf_axis_type axis_type;

  double vector1, vector2, vector3, offset1, offset2, offset3;

  double start, increment;

  cbf_failnez (cbf_find_category    (handle, "axis"))
  cbf_failnez (cbf_find_column      (handle, "id"))
  cbf_failnez (cbf_find_row         (handle, axis_id))
  cbf_failnez (cbf_find_column      (handle, "depends_on"))
  cbf_failnez (cbf_get_value        (handle, &next_id))
  cbf_failnez (cbf_get_axis_type    (handle, axis_id,
                                             &axis_type))
  cbf_failnez (cbf_get_axis_vector  (handle, axis_id,
                                             &vector1,
                                             &vector2,
                                             &vector3))
  cbf_failnez (cbf_get_axis_offset  (handle, axis_id,
                                             &offset1,
                                             &offset2,
                                             &offset3))

  start = increment = 0;

  if (read_setting)

    cbf_failnez (cbf_get_axis_setting (handle, reserved, axis_id,
                                          &start, 
                                          &increment))
    
  cbf_failnez (cbf_add_positioner_axis (positioner,
                       axis_id, 
                       next_id,
                       axis_type, 
                       vector1, vector2, vector3,
                       offset1, offset2, offset3,
                       start, increment))

  return 0;
}

                            
  /* Connect a set of positioner axes */

int cbf_connect_axes (cbf_positioner positioner)
{
  if (!positioner)

    return CBF_ARGUMENT;

  if (!positioner->axes_are_connected)
  {
      /* Arrange the axes in order of their connection */

    cbf_axis_struct axis;

    const char *depends_on = ".";

    int count = 0, dest, search, found;

    for (dest = ((int) positioner->axes) - 1; dest >= 0; dest--)
    {
      for (search = 0; search <= dest; search++)
      {
        if (positioner->axis [search].depends_on)

          found = cbf_cistrcmp (positioner->axis [search].depends_on,
                                depends_on) == 0;

        else

          found = cbf_cistrcmp (".", depends_on) == 0;

        if (found)
        {
          depends_on = positioner->axis [search].name;

          axis                      = positioner->axis [dest];
          positioner->axis [dest]   = positioner->axis [search];
          positioner->axis [search] = axis;

          if (!depends_on && dest > 0)

            return CBF_NOTFOUND;

          break;
        }
      }

      if (search > dest)

        return CBF_NOTFOUND;
    }

    positioner->axes_are_connected = 1;
  }

  return 0;
}

                            
  /* Calculate a position given initial coordinates */

int cbf_calculate_position (cbf_positioner positioner,
                            unsigned int   reserved,
                            double         ratio,
                            double         initial1,
                            double         initial2,
                            double         initial3,
                            double        *final1,
                            double        *final2,
                            double        *final3)
{
  size_t i;

  double setting;

  if (!positioner)

    return CBF_ARGUMENT;

  if (reserved != 0)

    return CBF_ARGUMENT;

  for (i = 0; i < positioner->axes; i++)
  {
    setting = positioner->axis [i].start + ratio *
              positioner->axis [i].increment;

    if (positioner->axis [i].setting != setting)
    {
      positioner->matrix_is_valid = 0;

      positioner->axis [i].setting = setting;
    }
  }

  if (!positioner->matrix_is_valid)
  {
    positioner->matrix [0][0] = 1;
    positioner->matrix [0][1] = 0;
    positioner->matrix [0][2] = 0;
    positioner->matrix [0][3] = 0;
    positioner->matrix [1][0] = 0;
    positioner->matrix [1][1] = 1;
    positioner->matrix [1][2] = 0;
    positioner->matrix [1][3] = 0;
    positioner->matrix [2][0] = 0;
    positioner->matrix [2][1] = 0;
    positioner->matrix [2][2] = 1;
    positioner->matrix [2][3] = 0;

    if (!positioner->axes_are_connected)

      cbf_failnez (cbf_connect_axes (positioner))

    for (i = 0; i < positioner->axes; i++)
    {
      setting = positioner->axis [i].setting;

      if (positioner->axis [i].type == CBF_TRANSLATION_AXIS)
      {
        positioner->matrix [0][3] += setting *
                                     positioner->axis [i].vector [0];
        positioner->matrix [1][3] += setting *
                                     positioner->axis [i].vector [1];
        positioner->matrix [2][3] += setting *
                                     positioner->axis [i].vector [2];
      }
      else
      {
        double s, x, y, z, w, 
               xx, yy, zz, xy, xz, xw, yz, yw, zw;

        double rotation [3][3], product [3][3];

        int r1, c1r2, c2;

        s = sin (setting * 0.00872664625997164788461845384244);

        x = positioner->axis [i].vector [0] * s;
        y = positioner->axis [i].vector [1] * s;
        z = positioner->axis [i].vector [2] * s;

        w = cos (setting * 0.00872664625997164788461845384244);

        xx = x * x;
        yy = y * y;
        zz = z * z;
        xy = x * y;
        xz = x * z;
        xw = x * w;
        yz = y * z;
        yw = y * w;
        zw = z * w;

        rotation [0][0] = 1 - 2 * (yy + zz);
        rotation [0][1] =     2 * (xy - zw);
        rotation [0][2] =     2 * (xz + yw);
        rotation [1][0] =     2 * (xy + zw);
        rotation [1][1] = 1 - 2 * (xx + zz);
        rotation [1][2] =     2 * (yz - xw);
        rotation [2][0] =     2 * (xz - yw);
        rotation [2][1] =     2 * (yz + xw);
        rotation [2][2] = 1 - 2 * (xx + yy);

        for (r1 = 0; r1 < 3; r1++)

          for (c2 = 0; c2 < 3; c2++)
          {
            product [r1][c2] = 0;

            for (c1r2 = 0; c1r2 < 3; c1r2++)

              product [r1][c2] += rotation [r1][c1r2] *
                                  positioner->matrix [c1r2][c2];
          }

        for (r1 = 0; r1 < 3; r1++)

          for (c2 = 0; c2 < 3; c2++)

            positioner->matrix [r1][c2] = product [r1][c2];
      }

      positioner->matrix [0][3] += positioner->axis [i].offset [0];
      positioner->matrix [1][3] += positioner->axis [i].offset [1];
      positioner->matrix [2][3] += positioner->axis [i].offset [2];
    }

    positioner->matrix_is_valid = 1;
  }
    
  if (final1)

    *final1 = positioner->matrix [0][0] * initial1 +
              positioner->matrix [0][1] * initial2 +
              positioner->matrix [0][2] * initial3 +
              positioner->matrix [0][3];
    
  if (final2)

    *final2 = positioner->matrix [1][0] * initial1 +
              positioner->matrix [1][1] * initial2 +
              positioner->matrix [1][2] * initial3 +
              positioner->matrix [1][3];
    
  if (final3)

    *final3 = positioner->matrix [2][0] * initial1 +
              positioner->matrix [2][1] * initial2 +
              positioner->matrix [2][2] * initial3 +
              positioner->matrix [2][3];

  return 0;
}


  /* Calculate the initial position given final coordinates */

int cbf_calculate_initial_position (cbf_positioner positioner,
                                    unsigned int   reserved,
                                    double         ratio,
                                    double         final1,
                                    double         final2,
                                    double         final3,
                                    double        *initial1,
                                    double        *initial2,
                                    double        *initial3)
{
  double delta [3];

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Update the matrix */

  cbf_failnez (cbf_calculate_position (positioner, reserved, ratio, 0, 0, 0,
                                                   NULL, NULL, NULL))

  delta [0] = final1 - positioner->matrix [0][3];
  delta [1] = final2 - positioner->matrix [1][3];
  delta [2] = final3 - positioner->matrix [2][3];

  if (initial1)

    *initial1 = positioner->matrix [0][0] * delta [0] +
                positioner->matrix [1][0] * delta [1] +
                positioner->matrix [2][0] * delta [2];
    
  if (initial2)

    *initial2 = positioner->matrix [0][1] * delta [0] +
                positioner->matrix [1][1] * delta [1] +
                positioner->matrix [2][1] * delta [2];
    
  if (initial3)

    *initial3 = positioner->matrix [0][2] * delta [0] +
                positioner->matrix [1][2] * delta [1] +
                positioner->matrix [2][2] * delta [2];

  return 0;
}


  /* Construct a goniometer */

int cbf_construct_goniometer (cbf_handle handle, 
                              cbf_goniometer *goniometer)
{
  const char *diffrn_id, *id, *this_id, *axis_id;

  unsigned int row;
    
  int errorcode;


  if (!goniometer)

    return CBF_ARGUMENT;


    /* Get the measurement id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_measurement"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))


    /* Construct the goniometer */

  cbf_failnez (cbf_make_positioner (goniometer))

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_find_category (handle, "diffrn_measurement_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_measurement_axis.measurement_id
                              _diffrn_measurement_axis.id  (deprecated) */
                            
      errorcode = cbf_find_column (handle, "measurement_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle,
                                                0, /* reserved */
                                                *goniometer, 
                                                axis_id, 1);
      }
  }

  if (!errorcode)

    errorcode = cbf_connect_axes (*goniometer);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (*goniometer);

    *goniometer = NULL;
  }

  return errorcode;
}


  /* Free a goniometer */

int cbf_free_goniometer (cbf_goniometer goniometer)
{
  return cbf_free_positioner (goniometer);
}


  /* Get the rotation axis */

int cbf_get_rotation_axis (cbf_goniometer goniometer, unsigned int  reserved,
                                                      double       *vector1,
                                                      double       *vector2,
                                                      double       *vector3)
{
  size_t axis;

  if (!goniometer)

    return CBF_ARGUMENT;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Currently just return the first rotation axis */

  for (axis = 0; axis < goniometer->axes; axis++)

    if (goniometer->axis [axis].type == CBF_ROTATION_AXIS)
            
      if (goniometer->axis [axis].increment)
      {
        if (vector1)

          *vector1 = goniometer->axis [axis].vector [0];

        if (vector2)

          *vector2 = goniometer->axis [axis].vector [1];

        if (vector3)

          *vector3 = goniometer->axis [axis].vector [2];

        return 0;
      }

  return CBF_NOTFOUND;
}


  /* Get the rotation range */

int cbf_get_rotation_range (cbf_goniometer goniometer, unsigned int  reserved,
                                                       double       *start,
                                                       double       *increment)
{
  size_t axis;

  if (!goniometer)

    return CBF_ARGUMENT;

  if (reserved != 0)

    return CBF_ARGUMENT;


    /* Currently just return the range of the first rotation axis */

  for (axis = 0; axis < goniometer->axes; axis++)

    if (goniometer->axis [axis].type == CBF_ROTATION_AXIS)
            
      if (goniometer->axis [axis].increment)
      {
        if (start)

          *start = goniometer->axis [axis].start;

        if (increment)

          *increment = goniometer->axis [axis].increment;

        return 0;
      }

  return CBF_NOTFOUND;
}


  /* Reorient a vector */

int cbf_rotate_vector (cbf_goniometer goniometer, unsigned int reserved,
                                                  double       ratio,
                                                  double       initial1,
                                                  double       initial2,
                                                  double       initial3,
                                                  double      *final1,
                                                  double      *final2,
                                                  double      *final3)
{
  double transformed [3], origin [3];

  if (reserved != 0)

    return CBF_ARGUMENT;

  cbf_failnez (cbf_calculate_position (goniometer, reserved, ratio, 0, 0, 0, 
                                           &origin [0],
                                           &origin [1],
                                           &origin [2]))

  cbf_failnez (cbf_calculate_position (goniometer, reserved, ratio, 
                                           initial1, 
                                           initial2, 
                                           initial3,
                                           &transformed [0],
                                           &transformed [1],
                                           &transformed [2]))

  if (final1)

    *final1 = transformed [0] - origin [0];

  if (final2)

    *final2 = transformed [1] - origin [1];

  if (final3)

    *final3 = transformed [2] - origin [2];

  return 0;
}


  /* Convert a vector to reciprocal space (assumes beam along -z) */

int cbf_get_reciprocal (cbf_goniometer goniometer, unsigned int reserved,
                                                   double       ratio,
                                                   double       wavelength,
                                                   double       real1,
                                                   double       real2,
                                                   double       real3,
                                                   double      *reciprocal1,
                                                   double      *reciprocal2,
                                                   double      *reciprocal3)
{
  double length, ewald [3];

  if (reserved != 0)

    return CBF_ARGUMENT;

  if (wavelength <= 0.0)

    return CBF_ARGUMENT;

  length = real1 * real1 + real2 * real2 + real3 * real3;

  if (length <= 0.0)

    return CBF_ARGUMENT;


    /* Project the vector onto the sphere */

  length = sqrt (length) * wavelength;

  ewald [0] = real1 / length;
  ewald [1] = real2 / length;
  ewald [2] = real3 / length + 1 / wavelength;


    /* Rotate the vector back to the 0 position of the goniometer */

  cbf_failnez (cbf_calculate_initial_position (goniometer, reserved, ratio,
                                               ewald [0], 
                                               ewald [1], 
                                               ewald [2],
                                               reciprocal1,
                                               reciprocal2,
                                               reciprocal3))

  return 0;
}


  /* Construct a detector positioner */

int cbf_construct_detector (cbf_handle    handle, 
                            cbf_detector *detector,
                            unsigned int  element_number)
{
  int errorcode, precedence;

  unsigned int row, axis;

  const char *diffrn_id, *id, *this_id, *axis_id, *array_id;

  const char *surface_axis [2];

  double displacement [2], increment [2];

  cbf_positioner positioner;

  if (!detector)

    return CBF_ARGUMENT;


    /* Get the detector id */

  cbf_failnez (cbf_get_diffrn_id (handle, &diffrn_id))

  cbf_failnez (cbf_find_category (handle, "diffrn_detector"))
  cbf_failnez (cbf_find_column   (handle, "diffrn_id"))
  cbf_failnez (cbf_find_row      (handle, diffrn_id))
  cbf_failnez (cbf_find_column   (handle, "id"))
  cbf_failnez (cbf_get_value     (handle, &id))


    /* Construct the detector surface */

  cbf_failnez (cbf_get_array_id  (handle, element_number, &array_id))
  cbf_failnez (cbf_find_category (handle, "array_structure_list"))
  cbf_failnez (cbf_find_column   (handle, "array_id"))

  surface_axis [0] = surface_axis [1] = NULL;

  while (cbf_find_nextrow (handle, array_id) == 0)
  {
    cbf_failnez (cbf_find_column      (handle, "precedence"))
    cbf_failnez (cbf_get_integervalue (handle, &precedence))

    if (precedence < 1 || precedence > 2)

      return CBF_FORMAT;

    if (surface_axis [precedence - 1])

      return CBF_FORMAT;

    cbf_failnez (cbf_find_column (handle, "axis_set_id"))
    cbf_failnez (cbf_get_value   (handle, &surface_axis [precedence - 1]))
    cbf_failnez (cbf_find_column (handle, "array_id"))
  }

  if (!surface_axis [0])

    return CBF_FORMAT;

  cbf_failnez (cbf_find_category   (handle, "array_structure_list_axis"))
  cbf_failnez (cbf_find_column     (handle, "axis_set_id"))
  cbf_failnez (cbf_find_row        (handle, surface_axis [0]))
  cbf_failnez (cbf_find_column     (handle, "axis_id"))
  cbf_failnez (cbf_get_value       (handle, &surface_axis [0]))
  cbf_failnez (cbf_find_column     (handle, "displacement"))
  cbf_failnez (cbf_get_doublevalue (handle, &displacement [0]))
  cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
  cbf_failnez (cbf_get_doublevalue (handle, &increment [0]))

  if (surface_axis [1])
  {
    cbf_failnez (cbf_find_column     (handle, "axis_set_id"))
    cbf_failnez (cbf_find_row        (handle, surface_axis [1]))
    cbf_failnez (cbf_find_column     (handle, "axis_id"))
    cbf_failnez (cbf_get_value       (handle, &surface_axis [1]))
    cbf_failnez (cbf_find_column     (handle, "displacement"))
    cbf_failnez (cbf_get_doublevalue (handle, &displacement [1]))
    cbf_failnez (cbf_find_column     (handle, "displacement_increment"))
    cbf_failnez (cbf_get_doublevalue (handle, &increment [1]))
  }


    /* Construct the positioner */

  cbf_failnez (cbf_make_positioner (&positioner))

  errorcode = cbf_alloc ((void **) detector, NULL, 
                        sizeof (cbf_detector_struct), 1);

  for (row = errorcode = 0; !errorcode; row++)
  {
    errorcode = cbf_find_category (handle, "diffrn_detector_axis");

    if (!errorcode)
    {
        /* allow for aliases  _diffrn_detector_axis.detector_id
                              _diffrn_detector_axis.id  (deprecated) */
                            
      errorcode = cbf_find_column (handle, "detector_id");

      if (errorcode)

        errorcode = cbf_find_column (handle, "id");
    }

    if (!errorcode)
    {
      errorcode = cbf_select_row (handle, row);

      if (errorcode == CBF_NOTFOUND)
      {
        errorcode = 0;

        break;
      }
    }

    if (!errorcode)

      errorcode = cbf_get_value (handle, &this_id);

    if (!errorcode)

      if (cbf_cistrcmp (id, this_id) == 0)
      {
        errorcode = cbf_find_column (handle, "axis_id");

        if (!errorcode)

          errorcode = cbf_get_value (handle, &axis_id);

        if (!errorcode)

          errorcode = cbf_read_positioner_axis (handle, 0,
                                                positioner, 
                                                axis_id, 1);
      }
  }


    /* Add the surface axes */

  if (!errorcode)

    errorcode = cbf_read_positioner_axis (handle, 0, positioner, 
                                                  surface_axis [0], 0);

  if (!errorcode && surface_axis [1])

    errorcode = cbf_read_positioner_axis (handle, 0, positioner, 
                                                  surface_axis [1], 0);


    /* Connect the axes */

  if (!errorcode)

    errorcode = cbf_connect_axes (positioner);

  if (errorcode)
  {
    errorcode |= cbf_free_positioner (positioner);

    return errorcode | cbf_free ((void **) detector, NULL);
  }


    /* Copy the start and increment values into the surface axes */

  (*detector)->displacement [0] = displacement [0];
  (*detector)->displacement [1] = displacement [1];

  (*detector)->increment [0] = increment [0];
  (*detector)->increment [1] = increment [1];

  if (surface_axis [1])

    (*detector)->axes = 2;

  else

    (*detector)->axes = 1;

  for (axis = 0; axis < (*detector)->axes; axis++)

    for (row = 0; row < positioner->axes; row++)

      if (cbf_cistrcmp (positioner->axis [row].name, 
                        surface_axis [axis]) == 0)
      {
        (*detector)->index [axis] = row;

        positioner->axis [row].increment = 0;

        break;
      }

  (*detector)->positioner = positioner;

  return 0;
}


  /* Free a detector */

int cbf_free_detector (cbf_detector detector)
{
  int errorcode = 0;

  if (detector)

    errorcode = cbf_free_positioner (detector->positioner);

  return errorcode | cbf_free ((void **) &detector, NULL);
}


  /* Update the pixel settings */

int cbf_update_pixel (cbf_detector detector, double index1, 
                                             double index2)
{
  if (!detector)

    return CBF_ARGUMENT;

  detector->positioner->axis [detector->index [0]].start =
        index2 * detector->increment [0] + detector->displacement [0];

  if (detector->axes == 2)

    detector->positioner->axis [detector->index [1]].start =
        index1 * detector->increment [1] + detector->displacement [1];

  return 0;
}


  /* Get the beam center */

int cbf_get_beam_center (cbf_detector detector, double *index1,
                                                double *index2,
                                                double *center1, 
                                                double *center2)
{
  double pixel00 [3], pixel01 [3], pixel10 [3], m [2][2], det, index [2];

  if (!detector)

    return CBF_ARGUMENT;

  if (detector->axes < 2)

    return CBF_NOTIMPLEMENTED;

  cbf_failnez (cbf_get_pixel_coordinates (detector, 0, 0,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, 0, 1,
                                                    &pixel01 [0],
                                                    &pixel01 [1],
                                                    &pixel01 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, 1, 0,
                                                    &pixel10 [0],
                                                    &pixel10 [1],
                                                    &pixel10 [2]))

  m [0][0] = pixel10 [0] - pixel00 [0];
  m [0][1] = pixel01 [0] - pixel00 [0];
  m [1][0] = pixel10 [1] - pixel00 [1];
  m [1][1] = pixel01 [1] - pixel00 [1];

  det = m [0][0] * m [1][1] - m [1][0] * m [0][1];

  if (det == 0.0)

    return CBF_UNDEFINED;

  index [0] = (-m [1][1] * pixel00 [0] + m [0][1] * pixel00 [1]) / det;
  index [1] =  (m [1][0] * pixel00 [0] - m [0][0] * pixel00 [1]) / det;

  if (index1)

    *index1 = index [0];

  if (index2)

    *index2 = index [1];

  if (center1)

    *center1 = index [0] * detector->increment [0];

  if (center2)

    *center2 = index [1] * detector->increment [1];

  return 0;

/*  a * delta01 + b * delta10 + pixel00 = (0 0 ?)

    a * delta01[0] + b * delta10[0] + pixel00[0] = 0
    a * delta01[1] + b * delta10[1] + pixel00[1] = 0

    (d01[0] d10[0]) (a) = -(p00[0])
    (d01[1] d10[1]) (b)    (p00[1])

    (a) = -(d01[0] d10[0])-1 (p00[0])
    (b)    (d01[1] d10[1])   (p00[1]) */
}


  /* Get the detector distance: shortest distance to the plane */

int cbf_get_detector_distance (cbf_detector detector, double *distance)
{
  double normal [3], pixel00 [3];

  cbf_failnez (cbf_get_detector_normal (detector, &normal [0],
                                                  &normal [1],
                                                  &normal [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, 0, 0,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  if (distance)

      *distance = fabs (normal [0] * pixel00 [0] +
                        normal [1] * pixel00 [1] +
                        normal [2] * pixel00 [2]);

  return 0;
}


  /* Get the detector normal */

int cbf_get_detector_normal (cbf_detector detector, double *normal1,
                                                    double *normal2,
                                                    double *normal3)
{
  cbf_failnez (cbf_get_pixel_normal (detector, 0, 0, normal1,
                                                     normal2,
                                                     normal3))

  return 0;
}


  /* Calcluate the coordinates of a pixel */

int cbf_get_pixel_coordinates (cbf_detector detector, double index1,
                                                      double index2,
                                                      double *coordinate1,
                                                      double *coordinate2,
                                                      double *coordinate3)
{
  cbf_failnez (cbf_update_pixel (detector, index1, index2))

  cbf_failnez (cbf_calculate_position (detector->positioner,
                                                 0, 0, 0, 0, 0,
                                                 coordinate1,
                                                 coordinate2,
                                                 coordinate3))

  return 0;
}


  /* Get the pixel normal */

int cbf_get_pixel_normal (cbf_detector detector, double  index1,
                                                 double  index2,
                                                 double *normal1,
                                                 double *normal2,
                                                 double *normal3)
{
  double pixel00 [3], pixel01 [3], pixel10 [3], normal [3], length;

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 - 0.5,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 + 0.5,
                                                    &pixel01 [0],
                                                    &pixel01 [1],
                                                    &pixel01 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 + 0.5,
                                                    index2 - 0.5,
                                                    &pixel10 [0],
                                                    &pixel10 [1],
                                                    &pixel10 [2]))

  pixel01 [0] -= pixel00 [0];
  pixel01 [1] -= pixel00 [1];
  pixel01 [2] -= pixel00 [2];

  pixel10 [0] -= pixel00 [0];
  pixel10 [1] -= pixel00 [1];
  pixel10 [2] -= pixel00 [2];

  normal [0] = pixel01 [1] * pixel10 [2] - pixel10 [1] * pixel01 [2];
  normal [1] = pixel01 [2] * pixel10 [0] - pixel10 [2] * pixel01 [0];
  normal [2] = pixel01 [0] * pixel10 [1] - pixel10 [0] * pixel01 [1];

  length = normal [0] * normal [0] + 
           normal [1] * normal [1] +
           normal [2] * normal [2];

  if (length <= 0.0)

    return CBF_UNDEFINED;

  length = sqrt (length);

  if (normal1)

    *normal1 = normal [0] / length;

  if (normal2)

    *normal2 = normal [1] / length;

  if (normal3)

    *normal3 = normal [2] / length;

  return 0;
}


  /* Calcluate the area of a pixel */

int cbf_get_pixel_area (cbf_detector detector, double index1,
                                               double index2,
                                               double *area,
                                               double *projected_area)
{
  double pixel00 [3], pixel01 [3], pixel10 [3], normal [3];
    
  double length, length00;

  if (!detector)

    return CBF_ARGUMENT;

  if (detector->axes < 2)

    return CBF_NOTIMPLEMENTED;

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 - 0.5,
                                                    &pixel00 [0],
                                                    &pixel00 [1],
                                                    &pixel00 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 - 0.5,
                                                    index2 + 0.5,
                                                    &pixel01 [0],
                                                    &pixel01 [1],
                                                    &pixel01 [2]))

  cbf_failnez (cbf_get_pixel_coordinates (detector, index1 + 0.5,
                                                    index2 - 0.5,
                                                    &pixel10 [0],
                                                    &pixel10 [1],
                                                    &pixel10 [2]))

  pixel01 [0] -= pixel00 [0];
  pixel01 [1] -= pixel00 [1];
  pixel01 [2] -= pixel00 [2];

  pixel10 [0] -= pixel00 [0];
  pixel10 [1] -= pixel00 [1];
  pixel10 [2] -= pixel00 [2];

  normal [0] = pixel01 [1] * pixel10 [2] - pixel10 [1] * pixel01 [2];
  normal [1] = pixel01 [2] * pixel10 [0] - pixel10 [2] * pixel01 [0];
  normal [2] = pixel01 [0] * pixel10 [1] - pixel10 [0] * pixel01 [1];

  length = normal [0] * normal [0] + 
           normal [1] * normal [1] +
           normal [2] * normal [2];

  if (length <= 0.0)

    return CBF_UNDEFINED;

  length = sqrt (length);

  if (area)

    *area = length;

  if (projected_area)
  {
    length00 = pixel00 [0] * pixel00 [0] +
               pixel00 [1] * pixel00 [1] +
               pixel00 [2] * pixel00 [2];

    if (length00 <= 0.0)

      return CBF_UNDEFINED;

    length00 = sqrt (length00);

    *projected_area = fabs (pixel00 [0] * normal [0] + 
                            pixel00 [1] * normal [1] + 
                            pixel00 [2] * normal [2]) / length00;
  }

  return 0;
}


#ifdef __cplusplus

}

#endif
