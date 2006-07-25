/**********************************************************************
 * img -- handle MAR and ADSC CCD images                              *
 *                                                                    *
 * Version 1.0.0-alpha  9 September 1998                              *
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

#ifdef __cplusplus

extern "C" {

#endif

#include "img.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>

#define FailNEZ(x) { int FailNEZ_status = (x); \
                     if (FailNEZ_status != 0)  \
                       return FailNEZ_status; }

#define isNaN(x) ((((x) & 0x7F800000L) == 0x7F800000L) && \
                  (((x) & 0x007FFFFFL) != 0x00000000L))

                         
  /* I/O functions */

int img_swap_i4 (int i4)
{
  return ((i4 << 24) & 0x0FF000000) |
         ((i4 <<  8) & 0x000FF0000) |
         ((i4 >>  8) & 0x00000FF00) |
         ((i4 >> 24) & 0x0000000FF);
}


float img_float_i4 (int i4, int VAX)
{
  int O;

  float f;
  
  if (VAX)

    i4 = ((i4 << 16) & 0x0FFFF0000) |
         ((i4 >> 16) & 0x00000FFFF);

  O = 1;

  if (*((char *) &O) == 1)

    O = 0;

  else

    O = sizeof (int) - 4;

  f = 0.0;

  if (!isNaN (i4))
  {
      /* Translate a non-denormalised IEEE number */

    double d;

    d = (i4 & 0x007FFFFF) | 0x00800000;

    d *= pow (2.0, ((i4 >> 23) & 0xFF) - 150.0);

    if (i4 & 0x80000000)

      d = -d;

    f = (float) d;
  }

  if (VAX)

    f /= 4;

  return f;
}


int img_read_i4 (FILE *file, int *i4)
{
  int O = 1;

  if (*((char *) &O) == 1)

    O = 0;

  else

    O = sizeof (int) - 4;

  return fread (((char *) i4) + O, 4, 1, file) != 1;
}


  /* Read the header of an smv file */

int img_read_smvheader (img_handle img, FILE *file)
{
    /* Start : { 
       Line  : tag=data;
       End   : }          */

  int end_code = 0, eol = 0, start = 0;

  char *line = NULL;

  int max_line = 0;

  int c, count = 0, total = 0, header_bytes = 0, tags = 0;

  double centre [2], time, oscillation, twotheta, pixel;

  int dimension [2], status;

  char C64 [64];


  while ((c = getc (file)) != EOF)
  {
    total++;

    if (c == '\r')

      continue;

    if (c == '\t')

      c = ' ';

    if (count == end_code && c == ' ')

      continue;


      /* Header start or end? */

    if (!start)
    {
      start = c == '{';

      continue;
    }

    if (c == '}')

      break;


      /* Add the character to the line */

    if (count >= max_line)
    {
      char * oline = line;

      if (line)

        free (line);

      max_line += 256;

      line = (char *) malloc (max_line);

      if (!line)

        return IMG_ALLOC;

      if (count)

        memcpy (line, oline, count);
    }

    line [count] = (char) c;

    count++;


      /* Premature end? */

    if (c == '\n')
    {
      count = end_code = eol = 0;

      continue;
    }

    if (eol)

      continue;


      /* End of value? */

    if (end_code && c == ';')
    {
      char * value = line + end_code;

      line [count - 1] = 0;


        /* Remove trailing spaces */

      for (c = count - end_code - 2; c >= 0; c--)

        if (value [c] == ' ')

          value [c] = 0;

        else

          break;


        /* Save it */

      img_set_field (img, line, value);

      tags++;

      if (tags == 1)

        if (strcmp (line, "HEADER_BYTES") != 0)

          return IMG_FORMAT;

      eol = 1;
    }


      /* End of key? */

    if (c == '=')
    {
        /* Remove trailing spaces */

      end_code = count;

      line [count - 1] = 0;

      for (c = count - 2; c >= 0; c--)

        if (line [c] == ' ')

          line [c] = 0;

        else

          break;
    }
  }


    /* Free the buffer */

  if (line)
  
    free (line);


    /* Check the format */

  if (c != '}')

    return IMG_FORMAT;

  if (header_bytes <= 0)

    header_bytes = (int) img_get_number (img, "HEADER_BYTES");
    
  if (header_bytes <= 0)

    return IMG_FORMAT;


    /* Read and discard the remainder of the header */

  for (; total < header_bytes; total++)

    if (getc (file) == EOF)

      return IMG_READ;


    /* Translate the header entries to the standard */

  dimension [0] = (int) img_get_number (img, "SIZE1");
  dimension [1] = (int) img_get_number (img, "SIZE2");
  centre [0]    = img_get_number (img, "BEAM_CENTER_X");
  centre [1]    = img_get_number (img, "BEAM_CENTER_Y");
  time          = img_get_number (img, "TIME");
  oscillation   = img_get_number (img, "OSC_RANGE");
  twotheta      = img_get_number (img, "TWOTHETA");
  pixel         = img_get_number (img, "PIXEL_SIZE");

  status  = img_set_number (img, "PIXEL SIZE",        "%.6g", pixel);
  status |= img_set_number (img, "OSCILLATION RANGE", "%.6g", oscillation);
  status |= img_set_number (img, "EXPOSURE TIME",     "%.6g", time);
  status |= img_set_number (img, "TWO THETA",         "%.6g", twotheta);

  if (dimension [0] == 1152 && dimension [1] == 1152)

    status |= img_set_field (img, "DETECTOR", "ADSC QUANTUM1");

  if (dimension [0] == 2304 && dimension [1] == 2304)

    status |= img_set_field (img, "DETECTOR", "ADSC QUANTUM4");

  sprintf (C64, "%.6g %.6g", centre [0], centre [1]);

  status |= img_set_field (img, "BEAM CENTRE", C64);

  if (img_get_field (img, "AXIS"))
  {
    strncpy (C64, img_get_field (img, "AXIS"), 63);

    C64 [63] = 0;

    for (c = 0; C64 [c]; c++)

      C64 [c] = (char) toupper (C64 [c]);

    status |= img_set_field (img, "OSCILLATION AXIS", C64);
  }

  return status;
}


  /* Read the image data from an smv file */

int img_read_smvdata (img_handle img, FILE *file)
{
  const char *order, *type;

  int little, size, sign, rows, cols;

  int xcount, ycount, readcount, datacount;

  unsigned char *data;
  

    /* Get the byte order */

  order = img_get_field (img, "BYTE_ORDER");

  if (!order)

    return IMG_FORMAT;
    
  little = order [0] == 'l' || order [0] == 'L';


    /* Get the data type */
    
  type = img_get_field (img, "TYPE");

  if (!type)

    return IMG_FORMAT;
    
  size = 1;

  sign = 0;

  if (strstr (type, "short") || strstr (type, "mad"))

    size = 2;

  if (strstr (type, "long"))

    sign = size = 4;

  if (strstr (type, "unsigned"))

    sign = 0;

  else

    if (strstr (type, "signed"))

      sign = 1;

  if (sign)

    if (size < sizeof (int))

      sign = -(1 << (size * 8));

    else

      sign = 0;


    /* Get the image size */

  cols = (int) img_get_number (img, "SIZE1");
  rows = (int) img_get_number (img, "SIZE2");

  if (img_set_dimensions (img, cols, rows))

    return IMG_FORMAT;

  if (img->size [0] == 0 ||
      img->size [1] == 0)

    return 0;


    /* Read the data */

  data = (unsigned char *) malloc (1024);

  if (!data)

    return IMG_ALLOC;
    

    /* Note that the smv file has the first dimension fast */

  xcount    = 0;
  ycount    = 0;
  datacount = 0;

  while ((readcount = fread (data + datacount, 1, 1024 - datacount, file)) != 0)
  {
    int c;

    datacount += readcount;

    for (c = 0; c + size <= datacount; c += size)
    {
      int pixel, o;

      pixel = 0;

      if (little)

        for (o = 0; o < size; o++)

          pixel |= data [c + o] << (o * 8);

      else

        for (o = 0; o < size; o++)

          pixel |= data [c + o] << ((size - o - 1) * 8);

      if (sign)

        if ((pixel >> ((size - 1) * 8)) & 0x080)

          pixel |= sign;

      img_pixel (img, xcount, ycount) = pixel;

      xcount++;

      if (xcount == cols)
      {
        xcount = 0;

        ycount++;

        if (ycount == rows)

          return 0;
      }
    }

    datacount -= c;

    if (datacount && c)

      memmove (data, data + c, datacount);
  }


    /* Failure */

  return IMG_READ;
}


int img_read_smv (img_handle img, const char *name)
{
  FILE * file;

  int status;
  
  if (!img)

    return IMG_ARGUMENT;

  file = fopen (name, "rb");

  if (!file)

    return IMG_OPEN;

  status = img_read_smvheader (img, file);

  if (status == 0)

    status = img_read_smvdata (img, file);

  fclose (file);

  return status;
}


  /* Read the header of an old-style MAR file */

int img_read_mar300header (img_handle img, FILE *file, int *org_data)
{
  int i4_data [25], count, swap, status, model;

  float f4_data [25];

  char C64 [64];

  double pixel_size;


    /* Read the start of the header */

  for (count = 0; count < 25; count++)

    if (img_read_i4 (file, &i4_data [count]))

      return IMG_READ;


    /* Do we need to swap the bytes? */

  swap = (i4_data [0] != 1200 && i4_data [0] != 1600 &&
          i4_data [0] != 1800 && i4_data [0] != 2000 &&
          i4_data [0] != 2300 && i4_data [0] != 2400 &&
          i4_data [0] != 3000 && i4_data [0] != 3450);


    /* Swap? */

  if (swap)

    for (count = 0; count < 25; count++)

      i4_data [count] = img_swap_i4 (i4_data [count]);


    /* Convert the floating-point data */

  for (count = 10; count < 25; count++)

    f4_data [count] = img_float_i4 (i4_data [count], 0);


    /* Check the wavelength: this indicates if we should try the VAX fix */

  if (f4_data [19] <= 0.25 || f4_data [19] >= 4.0)

    for (count = 10; count < 25; count++)

      f4_data [count] = img_float_i4 (i4_data [count], 1);

                
    /* Check the header again */

  for (count = 0; count < 2; count++)

    if (i4_data [count] != 1200 && i4_data [count] != 1800 &&
        i4_data [count] != 1600 && i4_data [count] != 2400 &&
        i4_data [count] != 2000 && i4_data [count] != 3000 &&
        i4_data [count] != 3000 && i4_data [count] != 3450)

      return IMG_FORMAT;


    /* Copy the data needed to read the image */

  org_data [0] = i4_data [0];
  org_data [1] = i4_data [1];
  org_data [2] = i4_data [2];
  org_data [3] = i4_data [3];
  org_data [4] = i4_data [4];
  
  org_data [5] = swap;

  if (org_data [2] <= 0)

    org_data [2] = 2 * org_data [0];


    /* Header data */

  model = 0;

  if (f4_data [11] > 50.0 && f4_data [11] <= 300.0)

    model = (int) floor (f4_data [11] * 2 + 0.5);

  strcpy (C64, "MAR");
  
  if (model)

    sprintf (C64, "MAR %d", model);

  status = img_set_field (img, "DETECTOR", C64);

  if (status)

    return status;

  if (i4_data [6] <= 0)
  {
    i4_data [6] = i4_data [7];

    i4_data [7] = 0;
  }

  if (i4_data [6] > 0)
  {
    sprintf (C64, "%d", i4_data [6]);

    if (i4_data [7] > 0)

      sprintf (C64 + strlen (C64), " %d", i4_data [7]);

    status = img_set_field (img, "BEAM INTENSITY", C64);

    if (status)

      return status;
  }

  if (i4_data [9] > 0)
  {
    status = img_set_number (img, "DOSE", "%.6f", i4_data [9]);

    if (status)

      return status;
  }


    /* If the wavelength is out of range, the floats are probably invalid */

  if (f4_data [19] > 0.25 && f4_data [19] < 4.0)
  {
    status = img_set_number (img, "WAVELENGTH", "%.6f", f4_data [19]);

    if (status)

      return status;

    if (f4_data [10] > 0)
    {
      status = img_set_number (img, "EXPOSURE TIME", "%.6f", f4_data [10] * 0.001);

      if (status)

        return status;
    }

    pixel_size = 0.0;

    if (f4_data [11] >   50.0 &&
        f4_data [11] <= 400.0 && org_data [0] == org_data [1])

      pixel_size = (f4_data [11] * 2) / org_data [0];

    if (pixel_size > 0.0)
    {
      status = img_set_number (img, "PIXEL SIZE", "%.6g", pixel_size);

      if (status)

        return status;
    }

    if (f4_data [17] || f4_data [18])
    {
      sprintf (C64, "%.6f %.6f", f4_data [17], f4_data [18]);

      status = img_set_field (img, "BEAM CENTRE", C64);

      if (status)

        return status;
    }

    if (f4_data [20])
    {
      status = img_set_number (img, "DISTANCE", "%.6g", f4_data [20]);

      if (status)

        return status;
    }

    if (f4_data [21])
    {
      status = img_set_field (img, "OSCILLATION AXIS", "PHI");

      status |= img_set_number (img, "PHI", ".6g", f4_data [21]);

      if (f4_data [22] != f4_data [21])

        status |= img_set_number (img, "OSCILLATION RANGE", ".6g",
                                     f4_data [22] - f4_data [21]);
      if (status)

        return status;
    }

    if (f4_data [23])
    {
      status = img_set_number (img, "OMEGA", "%.6g", f4_data [23]);

      if (status)

        return status;
    }
  }


    /* Date and time */

  if (fread (C64, 24, 1, file) == 0)

    return IMG_READ;

  C64 [24] = 0;

  while (strchr (C64, '-'))

    *strchr (C64, '-') = ' ';

  for (count = 23; count >= 0; count--)

    if (C64 [count] == ' ' || C64 [count] == 0)

      C64 [count] = 0;

    else

      break;

  if (count >= 0 && C64 [0])

    status = img_set_field (img, "DATE", C64);
  

    /* Skip the rest of the header */

  for (count = org_data [2] - 124; count > 0; count--)

    if (getc (file) == EOF)

      return IMG_READ;

  return 0;
}


  /* Read the image data from an old-style MAR file */

int img_read_mar300data (img_handle img, FILE *file, int *org_data)
{
  int x, y, little;

  unsigned char *data, *cdata;


    /* Get the image size */

  if (img_set_dimensions (img, org_data [0], org_data [1]))

    return IMG_FORMAT;

  if (img->size [0] == 0 ||
      img->size [1] == 0)

    return 0;
  
  
    /* Read the unsigned short data */

  little = org_data [5];

  x = 1;

  if (*((char *) &x) == 0)

    little = !little;

  data = (unsigned char *) malloc (org_data [2]);

  if (!data)

    return IMG_ALLOC;

  for (x = 0; x < img_columns (img); x++) {

    if (fread (data, org_data [2], 1, file) == 0)

      return IMG_READ;

    cdata = data;

    for (y = 0; y < img_rows (img); y++, cdata += 2)

      img_pixel (img, x, y) = (int) cdata [little] + ((int) cdata [1 - little] << 8);

    }

  free (data);


    /* Read the overflows */

  for (; org_data [4] > 0; org_data [4]--)
  {
    int O [2], c;

    for (c = 0; c < 2; c++)
    {
      if (img_read_i4 (file, &O [c]))

        return IMG_READ; 

      if (org_data [5])

        O [c] = img_swap_i4 (O [c]);
    }

    x = (O [0] - 1) / img_rows (img);
    y = (O [0] - 1) % img_rows (img);

    if (x >= 0 && x < img_columns (img) &&
        y >= 0 && y < img_rows (img))

      img_pixel (img, x, y) = O [1];

    else

      return IMG_FORMAT;
  }
  
  return 0;
}


  /* Read an old-style MAR file */

int img_read_mar300 (img_handle img, const char *name)
{
  FILE * file;

  int status, org_data [6];
  
  if (!img)

    return IMG_ARGUMENT;

  file = fopen (name, "rb");

  if (!file)

    return IMG_OPEN;

  status = img_read_mar300header (img, file, org_data);

  if (status == 0)

    status = img_read_mar300data (img, file, org_data);

  fclose (file);

  return status;
}


  /* Read the header of a new-style MAR file */

int img_read_mar345header (img_handle img, FILE *file, int *org_data)
{
  int i4_data [16], count, swap;

  char C64 [64];


    /* Read the start of the header */

  for (count = 0; count < 16; count++)

    if (img_read_i4 (file, &i4_data [count]))

      return IMG_READ;


    /* Do we need to swap the bytes? */

  swap = i4_data [0] != 1234;

  if (swap)
  {
    for (count = 0; count < 16; count++)

      i4_data [count] = img_swap_i4 (i4_data [count]);


      /* Check the header again */

    if (i4_data [0] != 1234)

      return IMG_FORMAT;
  }


    /* Copy the data needed to read the image */

    /* x = y */
    
  org_data [0] = org_data [1] = i4_data [1];


    /* Overflows */
    
  org_data [2] = i4_data [2];


    /* Byteswap? */

  org_data [3] = swap;


    /* Header data */
    
  FailNEZ (img_set_field (img, "DETECTOR", "MAR 345"));

  if (i4_data [6] <= 0)

    i4_data [6] = i4_data [7];

  if (i4_data [7] <= 0)

    i4_data [7] = i4_data [6];

  if (i4_data [6] > 0)
  {
    sprintf (C64, "%.6g %.6g", i4_data [6] / 1000.0, i4_data [7] / 1000.0);

    FailNEZ (img_set_field (img, "PIXEL SIZE", C64));
  }

  FailNEZ (img_set_number (img, "WAVELENGTH", "%.6f", i4_data [8] / 1000000.0));

  FailNEZ (img_set_number (img, "DISTANCE", "%.6g", i4_data [9] / 1000.0));

  FailNEZ (img_set_field (img, "OSCILLATION AXIS", "PHI"));

  FailNEZ (img_set_number (img, "PHI", "%.6g", i4_data [10] / 1000.0));

  FailNEZ (img_set_number (img, "OSCILLATION RANGE", "%.6g",
                                           (i4_data [11] - i4_data [10]) / 1000.0));



    /* Skip up to the overflows? */

  if (i4_data [2] > 0)

    for (count = 4096 - 64; count > 0; count--)

      if (getc (file) == EOF)

        return IMG_READ;

  return 0;
}


  /* Read the image data from a new-style MAR file */

int img_read_mar345data (img_handle img, FILE *file, int *org_data)
{
  int *O_data;

  int count, C, a, x, y, PACK, pixels, pixel, get, in, incount,
      pixcount, init, next, need;

  int *cimg;

  char C64 [64];
  
  
    /* Get the image size */

  FailNEZ (img_set_dimensions (img, org_data [0], org_data [1]))

  if (img->size [0] == 0 ||
      img->size [1] == 0)

    return 0;


    /* Save the overflows */

  if (org_data [2] > 0)
  {
    O_data = (int *) malloc (2 * org_data [2] * sizeof (int));

    if (!O_data)

      return IMG_ALLOC;

    for (count = 0; count < org_data [2] * 2; count++)

      if (img_read_i4 (file, &O_data [count]))

        return IMG_READ;
        
    if (org_data [3])

      for (count = 0; count < org_data [2] * 2; count++)

        O_data [count] = img_swap_i4 (O_data [count]);
  }
    
  
    /* Find the "CCP4 packed image" identifier */

  for (C = '\n', count = 0; C != EOF; C = getc (file))
  {
    if (count == 63)
    {
      C = '-';

      count = 0;
    }

    C64 [count] = (char) C;

    count++;
    
    C64 [count] = 0;

    if (C == '\n')
    {
      x = y = 0;
      
      sscanf (C64, "CCP4 packed image, X: %04d, Y: %04d", &x, &y);

      if (x && y)
      {
        PACK = 1;

        break;
      }

      x = y = 0;

      sscanf (C64, "CCP4 packed image V%d, X: %04d, Y: %04d", &PACK, &x, &y);

      if (x && y)

        break;

      count = 0;
    }
  } 

  if (C == EOF || PACK > 1)
  {
    if (org_data [2] > 0)

      free (O_data);
    
    return IMG_FORMAT;
  }

  
    /* MAR 345 images have the first dimension fast? */

    /* Read the packed unsigned short data */

  pixels = org_data [0] * org_data [1];

  pixel = 0;

  in = incount = 0;

  get = 6;

  init = 1;

  x = org_data [0];

  cimg = &img_pixel (img, 0, 0);

  while (pixel < pixels)
  {
      /* Get the next "get" bits of data into "next" */

    next = 0;

    need = get;

    while (need)
    {
      if (incount == 0)
      {
        in = getc (file);

        if (in == EOF)
        {
          if (org_data [2] > 0)

            free (O_data);

          return IMG_READ;
        }

        incount = 8;
      }

      if (need > incount)
      {
        next |= in << (get - need);

        need -= incount;

        in = 0;

        incount = 0;
      }
      else
      {
        next |= (in & ((1 << need) - 1)) << (get - need);

        in = (in >> need) & 0x0FF;

        incount -= need;

        break;
      }
    }


      /* Decode bits 0-5 */

    if (init)
    {
      static int decode [8] = { 0, 4, 5, 6, 7, 8, 16, 32 };

      pixcount = 1 << (next & 7);

      get = decode [(next >> 3) & 7];

      init = 0;
    }
    else
    {
        /* Decode a pixel */

        /* Sign-extend? */

      if (get)

        next |= -(next & (1 << (get - 1)));


        /* Calculate the final pixel value */

      if (pixel > x)
      {
        int A, B, C, D;

        A = cimg [-1 - x];
        B = cimg [   - x];
        C = cimg [ 1 - x];
        D = cimg [-1    ];

        *cimg = (next + (((A & 0x07FFF) + 
                          (B & 0x07FFF) + 
                          (C & 0x07FFF) +
                          (D & 0x07FFF) -
                          (A & 0x08000) -
                          (B & 0x08000) -
                          (C & 0x08000) -
                          (D & 0x08000) + 2) / 4)) & 0x0FFFF;
      }
      else
          
        if (pixel)
        
          *cimg = (cimg [-1] + next) & 0x0FFFF;

        else

          *cimg = next & 0x0FFFF;

      pixel++;

      cimg++;

      pixcount--;


        /* New set? */

      if (pixcount == 0)
      {
        init = 1;

        get = 6;
      }
    }
  }


    /* Overflows? */
        
  for (count = 0; count < org_data [2]; count++)
  {
    a = O_data [count * 2];
    
    x = a / img_rows (img);
    y = a % img_rows (img);

    if (x >= 0 && x < img_columns (img) &&
        y >= 0 && y < img_rows (img))

      img_pixel (img, x, y) = O_data [count * 2 + 1];

    else

      return IMG_FORMAT;
  }
    
  if (org_data [2] > 0)

    free (O_data);

  return 0;
}


  /* Read a new-style MAR file */

int img_read_mar345 (img_handle img, const char *name)
{
  FILE * file;

  int status, org_data [4];
  
  if (!img)

    return IMG_ARGUMENT;

  file = fopen (name, "rb");

  if (!file)

    return IMG_OPEN;

  status = img_read_mar345header (img, file, org_data);

  if (status == 0)

    status = img_read_mar345data (img, file, org_data);

  fclose (file);

  return status;
}


int img_set_tags (img_handle img, int tags);


  /* Read a file */

int img_read (img_handle img, const char *name)
{
  int status;

  img_set_tags (img, 0);

  img_set_dimensions (img, 0, 0);

  status = img_read_mar345 (img, name);

  if (status == 0)

    return 0;

  status = img_read_mar300 (img, name);

  if (status == 0)

    return 0;

  status = img_read_smv (img, name);

  if (status == 0)

    return 0;

  return IMG_ARGUMENT;
}


img_handle img_make_handle ()
{
  img_handle img = (img_handle) malloc (sizeof (img_object));

  if (img)
  {
    img->tags     = 0;
    img->tag      = NULL;
    img->size [0] = 0;
    img->size [1] = 0;
    img->image    = NULL;
  }

  return img;
}


int img_set_tags (img_handle img, int tags)
{
  if (!img || tags < 0)

    return IMG_ARGUMENT;

  tags = (tags + 0x03F) & ~0x03F;

  if (tags > img->tags)
  {
    img_tag * old_tag = img->tag;

    img->tag = (img_tag *) malloc (tags * sizeof (img_tag));

    if (!img->tag)
    {
      img->tag = old_tag;
      
      return IMG_ALLOC;
    }

    if (old_tag)
    {
      memcpy (img->tag, old_tag, img->tags * sizeof (img_tag));

      free (old_tag);
    }

    memset (img->tag + img->tags, 0, (tags - img->tags) * sizeof (img_tag));

    img->tags = tags;
  }

  if (tags == 0) {

    if (img->tag)
    {
      while (--img->tags >= 0)
      {
        free (img->tag [img->tags].tag);
        free (img->tag [img->tags].data);
      }

      free (img->tag);
    }

    img->tags = 0;
    img->tag  = NULL;
  }

  return 0;
}


int img_get_tags (img_handle img)
{
  int x;
  
  if (!img)

    return 0;
    
  for (x = img->tags - 1; x >= 0; x--)

    if (img->tag [x].tag)

      break;

  return x + 1;
}

 
int img_free_handle (img_handle img)
{
  if (!img)

    return IMG_ARGUMENT;

  img_set_tags (img, 0);

  if (img->image)

    free (img->image);

  free (img);

  return 0;
}


int img_delete_fieldnumber (img_handle img, int x)
{
  if (!img)

    return IMG_ARGUMENT;

  if (x < 0)

    return IMG_ARGUMENT;

  if (x >= img->tags)
  
    return IMG_FIELD;      

  if (!img->tag [x].tag)

    return IMG_FIELD;      

  free (img->tag [x].tag);
  free (img->tag [x].data);

  if (x < img->tags - 1)

    memmove (&img->tag [x], 
             &img->tag [x + 1],
             (img->tags - 1 - x) * sizeof (img_tag));

  img->tag [img->tags - 1].tag  = NULL;
  img->tag [img->tags - 1].data = NULL;

  return 0;
}


int img_delete_field (img_handle img, const char * tag)
{
  int x;
  
  if (!img || !tag)

    return IMG_ARGUMENT;

  for (x = img->tags - 1; x >= 0; x--)
  {
    if (!img->tag [x].tag)

      continue;

    if (strcmp (img->tag [x].tag, tag) == 0)

      return img_delete_fieldnumber (img, x);
  }

  return IMG_FIELD;      
}


const char *img_get_field (img_handle img, const char *tag)
{
  int x;
  
  if (!img || !tag)

    return NULL;


    /* Find the entry with the given tag */

  for (x = img->tags - 1; x >= 0; x--)

    if (img->tag [x].tag)

      if (strcmp (img->tag [x].tag, tag) == 0)

        return img->tag [x].data;

  return NULL;
}


int img_set_field (img_handle img, const char *tag, const char *data)
{
  int x, x0;
  
  if (!img || !tag)

    return IMG_ARGUMENT;


    /* Find the entry with the given tag */

  x0 = img->tags;

  for (x = x0 - 1; x >= 0; x--)
  {
    if (!img->tag [x].tag)
    {
      x0 = x;

      continue;
    }

    if (strcmp (img->tag [x].tag, tag) == 0)
    {
      free (img->tag [x].data);

      img->tag [x].data = (char *) malloc (strlen (data) + 1);

      if (!img->tag [x].data)

        return IMG_ALLOC;

      strcpy (img->tag [x].data, data);

      return 0;
    }
  }


    /* Create a new entry */

  if (img_set_tags (img, x0 + 1))

    return IMG_ALLOC;
  
  img->tag [x0].tag = (char *) malloc (strlen (tag) + 1);

  if (!img->tag [x0].tag)

    return IMG_ALLOC;

  img->tag [x0].data = (char *) malloc (strlen (data) + 1);

  if (!img->tag [x0].data)

    return IMG_ALLOC;

  strcpy (img->tag [x0].tag, tag);
  strcpy (img->tag [x0].data, data);

  return 0;

  }


double img_get_number (img_handle img, const char *tag)
{
  const char *field;

  field = img_get_field (img, tag);

  if (!field)

    return 0;

  return atof (field);
}


int img_set_number (img_handle img, const char *tag,
                                  const char *format,
                                  double data)
{
  char number [128];

  if (!img || !tag || !format)

    return IMG_ARGUMENT;

  sprintf (number, format, data);

  return img_set_field (img, tag, number);
}


int img_get_pixel (img_handle img, int x, int y)
{
  if (!img)

    return 0;
    
  if (x < 0              ||
      x >= img->size [0] ||
      y > 0              ||
      y <= img->size [1])

    return 0;

  return img->image [x * img->size [1] + y];
}
 

int img_set_pixel (img_handle img, int x, int y, int data)
{
  if (!img)

    return 0;
    
  if (x < 0              ||
      x >= img->size [0] ||
      y > 0              ||
      y <= img->size [1])

    return 0;

  return img->image [x * img->size [1] + y] = data;
}



int img_set_dimensions (img_handle img, int columns, int rows) {

  if (columns < 0 || rows < 0 || !img)

    return IMG_ARGUMENT;

  if (columns != img->size [0] || rows != img->size [1])
  {
    if (img->image)
    {
      free (img->image);

      img->size [0] =
      img->size [1] = 0;
    }

    if (columns > 0 && rows > 0)
    {
      img->image = (int *) malloc (columns * rows * sizeof (int));

      if (!img->image)

        return IMG_ALLOC;
    }
  }

  img->size [0] = columns;
  img->size [1] = rows;

  return 0;
}


int img_get_dimension (img_handle img, int dimension)
{
  if (!img)

    return 0;

  if (dimension < 1 || dimension > 2)

    return 1;

  return img->size [dimension - 1];
}


#ifdef __cplusplus

}

#endif

