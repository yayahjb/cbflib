
#ifdef __cplusplus

extern "C" {

#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_packed.h"

#define CBF_SHIFT63 (sizeof (int) * CHAR_BIT > 64 ? 63 : 0)

typedef struct
{
  int offset [128][2];

  unsigned int size [128];

  unsigned int start;

  unsigned int offsets;
}
cbf_packed_data;

#define CBF_PACKED_BITS1  4
#define CBF_PACKED_BITS2  5
#define CBF_PACKED_BITS3  6
#define CBF_PACKED_BITS4  7
#define CBF_PACKED_BITS5  8
#define CBF_PACKED_BITS6  16

#define CBF_PACKED_MASK1  ~15
#define CBF_PACKED_MASK2  ~31
#define CBF_PACKED_MASK3  ~63
#define CBF_PACKED_MASK4  ~127
#define CBF_PACKED_MASK5  ~255
#define CBF_PACKED_MASK6  ~65535

static const unsigned int cbf_packed_bits [8] = { 0, CBF_PACKED_BITS1,
                                                     CBF_PACKED_BITS2,
                                                     CBF_PACKED_BITS3,
                                                     CBF_PACKED_BITS4,
                                                     CBF_PACKED_BITS5,
                                                     CBF_PACKED_BITS6, 65 };


  /* Add an integer to the array of offsets to write */

int cbf_add_offset (cbf_packed_data *data, unsigned int element,
                                           unsigned int last_element)
{
  int offset;
  
  unsigned int index, m;


    /* Save the offset */

  index = (data->offsets + data->start) & 127;

  offset = element - last_element;
    
  data->offset [index][0] = offset;


    /* How many bits do we need to save? */

  if (offset == 0)

    data->size [index] = 0;

  else

    if ((element < last_element && offset > 0) ||
        (element > last_element && offset < 0))
    {
      data->offset [index][1] = -(offset > 0);

      data->size [index] = 7;
    }
    else
    {
      m = (offset ^ (offset << 1));

      if ((m & CBF_PACKED_MASK1) == 0)

        data->size [index] = 1;

      else

        if ((m & CBF_PACKED_MASK2) == 0)

          data->size [index] = 2;

        else

          if ((m & CBF_PACKED_MASK3) == 0)

            data->size [index] = 3;

          else

            if ((m & CBF_PACKED_MASK4) == 0)

              data->size [index] = 4;

            else

              if ((m & CBF_PACKED_MASK5) == 0)

                data->size [index] = 5;

             else

               if ((m & CBF_PACKED_MASK6) == 0)

                 data->size [index] = 6;

               else

                 data->size [index] = 7;
    }

  
    /* Success */

  data->offsets++;

  return 0;
}


  /* Pack 1 << chunk offsets in [size] bits each */

int cbf_pack_chunk (cbf_packed_data *data, int size, int chunk, cbf_file *file)
{
  unsigned int count, index;


    /* Write the codes */

  cbf_failnez (cbf_put_integer (file, (size << 3) | chunk, 0, 6))

  chunk = 1 << chunk;

  if (size > 0)
  {
    index = data->start;
    
    for (count = chunk; count; count--, index++)

      cbf_failnez (cbf_put_bits (file, data->offset [index & 127], cbf_packed_bits [size]))
  }


    /* Update the buffer count and start */

  data->start = (data->start + chunk) & 127;

  data->offsets -= chunk;


    /* Success */

  return 0;
}


  /* Get the maximum size required to code 1 << chunk offsets */

unsigned int cbf_maximum_size (cbf_packed_data *data, unsigned int start,
                                                      unsigned int chunk)
{
  unsigned int maxsize, index, count;


    /* Get the maximum size */

  maxsize = 0;

  index = data->start + start;

  for (count = 1 << chunk; count; count--)
  {
    if (data->size [index & 127] > maxsize)

      maxsize = data->size [index & 127];
    
    index++;
  }

  return maxsize;
}


  /* Write out a block as economically as possible */

int cbf_pack_nextchunk (cbf_packed_data *data, cbf_file *file)
{
  unsigned int bits, next_bits, chunk, size, next_size,
               combined_bits, combined_size;


    /* Number of bits to encode a single offset */

  size = cbf_maximum_size (data, 0, 0);

  bits = cbf_packed_bits [size] + 6;


  chunk = 0;

  while (data->offsets >= (2 << chunk))
  {
    next_size = cbf_maximum_size (data, 1 << chunk, chunk);

    next_bits = (cbf_packed_bits [next_size] << chunk) + 6;

    if (size > next_size)
    {
      combined_bits = bits * 2 - 6;

      combined_size = size;
    }
    else
    {
      combined_bits = next_bits * 2 - 6;

      combined_size = next_size;
    }

    if (combined_bits > bits + next_bits)

      return cbf_pack_chunk (data, size, chunk, file);

    bits = combined_bits;

    size = combined_size;

    chunk++;
  }

  return cbf_pack_chunk (data, size, chunk, file);
}


  /* Compress an array */
  
int cbf_compress_packed (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file)
{
  int minelement, maxelement;

  unsigned int count, element, lastelement,
           unsign, sign, limit;

  unsigned char *unsigned_char_data;

  cbf_packed_data *data;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;


    /* Allocate memory */

  cbf_failnez (cbf_alloc ((void **) &data, NULL, sizeof (cbf_packed_data), 1))

  data->start = 0;

  data->offsets = 0;

    /* Count the expected number of bits */

  minelement = 0;

  maxelement = 0;


    /* Discard any bits in the buffers */

  cbf_onfailnez (cbf_reset_bits (file),
             cbf_free ((void **) data, NULL))


    /* Write the coding id (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, compression, 0, 64),
             cbf_free ((void **) data, NULL))

    /* Write the number of elements (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, nelem, 0, 64),
             cbf_free ((void **) data, NULL))

    /* Write the minimum element (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, minelement, elsign, 64),
             cbf_free ((void **) data, NULL))


    /* Write the maximum element (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, maxelement, elsign, 64),
             cbf_free ((void **) data, NULL))


    /* Write the repeat size (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, repeat, 0, 64),
               cbf_free ((void **) data, NULL))

    /* Initialise the pointers */

  unsigned_char_data = (unsigned char *) buf;


    /* Maximum limit (unsigned) is 64 bits */

  if (elsize * CHAR_BIT > 64)
  {
    sign = 1 << CBF_SHIFT63;

    limit = ~-(sign << 1);
  }
  else
  {
    sign = 1 << (elsize * CHAR_BIT - 1);

    limit = ~0;
  }


    /* Offset to make the value unsigned */

  if (elsign)

    unsign = sign;

  else

    unsign = 0;


    /* Start from 0 */

  lastelement = unsign;

  for (count = 0; count < nelem; count++)
  {
      /* Get the next element */

    if (elsize == sizeof (int))

      element = *((unsigned int *) unsigned_char_data);

    else

      if (elsize == sizeof (short))

        element = *((unsigned short *) unsigned_char_data);

      else

        element = *unsigned_char_data;

    unsigned_char_data += elsize;


      /* Make the element unsigned */

    element += unsign;


      /* Limit the value to 64 bits */

    if (element > limit)

      if (elsign && (int) (element - unsign) < 0)

        element = 0;

      else

        element = limit;


      /* Add the offset to the buffer */

    cbf_add_offset (data, element, lastelement);


      /* Is the buffer full? */

    if (data->offsets == 128)

        /* Write the next block as economically as possible */

      cbf_onfailnez (cbf_pack_nextchunk (data, file),
                 cbf_free ((void **) data, NULL))


      /* Update the previous element */
        
    lastelement = element;
  }


    /* Flush the buffer */

  while (data->offsets > 0)

    cbf_onfailnez (cbf_pack_nextchunk (data, file),
               cbf_free ((void **) data, NULL))

  cbf_onfailnez (cbf_put_integer (file, 0, 0, 7),
             cbf_free ((void **) data, NULL))

    /* Free memory */

  return cbf_free ((void **) &data, NULL);
}


  /* Decompress an array */

int cbf_decompress_packed (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file)
{
  unsigned int next, pixel, pixelcount, repeat;

  unsigned int bits, element, sign, unsign, limit, count64, count;

  unsigned char *unsigned_char_data;

  unsigned int offset [4], last_element [4];

  int errorcode;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;


    /* Initialise the pointer */

  unsigned_char_data = (unsigned char *) buf;


    /* Maximum limit (unsigned) is 64 bits */

  if (elsize * CHAR_BIT > 64)
  {
    sign = 1 << CBF_SHIFT63;

    limit = ~-(sign << 1);
  }
  else
  {
    sign = 1 << (elsize * CHAR_BIT - 1);

    if (elsize == sizeof (int))

      limit = ~0;

    else

      limit = ~-(1 << (elsize * CHAR_BIT));
  }


    /* Offset to make the value unsigned */

  if (elsign)

    unsign = sign;

  else

    unsign = 0;


    /* How many ints do we need to hold 64 bits? */

  count64 = (64 + sizeof (int) * CHAR_BIT - 1) / (sizeof (int) * CHAR_BIT);


    /* Initialise the first element */

  last_element [0] = unsign;

  for (count = 1; count < count64; count++)
        
    last_element [count] = 0;



    /* Read the elements */

  count = 0;

  while (count < nelem)
  {
      /* Get the next 6 bits of data */

    errorcode = cbf_get_integer (file, (int *) &next, 0, 6);

    if (errorcode)
    {
      if (nelem_read)

        *nelem_read = count + pixel;
          
      return errorcode;
    }


      /* Decode bits 0-5 */

    pixelcount = 1 << (next & 7);

    bits = cbf_packed_bits [(next >> 3) & 7];


      /* Read the offsets */

    if (pixelcount + count > nelem)

      pixelcount = nelem - count;

    for (pixel = 0; pixel < pixelcount; pixel++)
    {
        /* Read an offset */

      if (bits)
      {
        errorcode = cbf_get_bits (file, (int *) offset, bits);

        if (errorcode)
        {
          if (nelem_read)

            *nelem_read = count + pixel;
          
          return errorcode;
        }


          /* Update the current element */

        last_element [0] += offset [0];
      }

      element = last_element [0];


        /* Limit the value to fit the element size */

      if (element > limit)

        if (elsign && (int) (element - unsign) < 0)

          element = 0;

        else

          element = limit;


        /* Make the element signed? */

      element -= unsign;


        /* Save the element */

      if (elsize == sizeof (int))

        *((unsigned int *) unsigned_char_data) = element;

      else

        if (elsize == sizeof (short))

          *((unsigned short *) unsigned_char_data) = element;

        else

          *unsigned_char_data = element;

      unsigned_char_data += elsize;
    }

    count += pixelcount;
  }


    /* Number read */

  if (nelem_read)
  
    *nelem_read = count;


    /* Success */

  return 0;
}

  /* Copy an array without compression */
  
int cbf_compress_none (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file)
{
  int minelement, maxelement;
  unsigned int count, element, 
           unsign, sign, limit;
  unsigned char *unsigned_char_data;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;

    /* Initialise the pointer */
  unsigned_char_data = (unsigned char *) buf;

    /* Initialise the minimum and maximum elements */
  minelement = ~0;
  maxelement = 0;
  for (count = 0; count < nelem; count++)
  {
      /* Get the next element */

    if (elsize == sizeof (int))
      element = *((unsigned int *) unsigned_char_data);
    else
      if (elsize == sizeof (short))
        element = *((unsigned short *) unsigned_char_data);
      else
        element = *unsigned_char_data;
    unsigned_char_data += elsize;

      /* Make the element unsigned */
    element += unsign;

      /* Limit the value to 64 bits */
    if (element > limit)
      if (elsign && (int) (element - unsign) < 0)
        element = 0;
      else
        element = limit;

      /* Update the minimum and maximum values */
    if (element < minelement)
      minelement = element;
    if (element > maxelement)
      maxelement = element;
   }


    /* Discard any bits in the buffers */
  cbf_failnez (cbf_reset_bits (file))


    /* Write the coding id (64 bits) */
  cbf_failnez (cbf_put_integer (file, compression, 0, 64))


    /* Maximum limit (unsigned) is 64 bits */
  if (elsize * CHAR_BIT > 64)
  {
    sign = 1 << CBF_SHIFT63;
    limit = ~-(sign << 1);
  }
  else
  {
    sign = 1 << (elsize * CHAR_BIT - 1);
    limit = ~0;
  }

    /* Offset to make the value unsigned */
  if (elsign)
    unsign = sign;
  else
    unsign = 0;

    /* Initialise the pointer */
  unsigned_char_data = (unsigned char *) buf;

  for (count = 0; count < nelem; count++)
  {
      /* Get the next element */
    if (elsize == sizeof (int))
      element = *((unsigned int *) unsigned_char_data);
    else
      if (elsize == sizeof (short))
        element = *((unsigned short *) unsigned_char_data);
      else
        element = *unsigned_char_data;
    unsigned_char_data += elsize;


      /* Make the element unsigned */
    element += unsign;


      /* Limit the value to 64 bits */
    if (element > limit)
      if (elsign && (int) (element - unsign) < 0)
        element = 0;
      else
        element = limit;
        
      /* Write the element to the file */
    cbf_failnez (cbf_put_integer (file, element, 0, elsize*CHAR_BIT))
  }

  return 0;
}


  /* Recover an array without decompression */

int cbf_decompress_none (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file)
{
  unsigned int element, sign, unsign, limit, count;
  unsigned char *unsigned_char_data;
  int errorcode;


    /* Is the element size valid? */
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))
    return CBF_ARGUMENT;


    /* Initialise the pointer */
  unsigned_char_data = (unsigned char *) buf;


    /* Maximum limit (unsigned) is 64 bits */
  if (elsize * CHAR_BIT > 64)
  {
    sign = 1 << CBF_SHIFT63;
    limit = ~-(sign << 1);
  }
  else
  {
    sign = 1 << (elsize * CHAR_BIT - 1);
    if (elsize == sizeof (int))
      limit = ~0;
    else
      limit = ~-(1 << (elsize * CHAR_BIT));
  }


    /* Offset to make the value unsigned */
  if (elsign)
    unsign = sign;
  else
    unsign = 0;


    /* Read the elements */

  count = 0;

  while (count < nelem)
  {
      /* Get the next elsize bits of data */

    errorcode = cbf_get_integer (file, (int *) &element, 0, elsize*CHAR_BIT);

    if (errorcode)
    {
      if (nelem_read)
        *nelem_read = count;
      return errorcode;
    }

        /* Limit the value to fit the element size */
      if (element > limit)
        if (elsign && (int) (element - unsign) < 0)
          element = 0;
        else
          element = limit;
        /* Make the element signed? */
      element -= unsign;


        /* Save the element */
      if (elsize == sizeof (int))
        *((unsigned int *) unsigned_char_data) = element;
      else
        if (elsize == sizeof (short))
          *((unsigned short *) unsigned_char_data) = element;
        else
          *unsigned_char_data = element;
      unsigned_char_data += elsize;
      count++;
  }

    /* Number read */
  if (nelem_read)
    *nelem_read = count;

    /* Success */
  return 0;
}

  /* Compress and array with byte offset algorithm */
  
int cbf_compress_byte_off (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file)
{
    fprintf(stderr,
      "\n*** Byte-Offset Algorithm Not Implemented Yet -- Abort ***\n");
    exit(1);
    return 1;
}

  /* Decompress an array with byte offset algorithm */

int cbf_decompress_byte_off (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file)
{
    fprintf(stderr,
      "\n*** Byte-Offset Algorithm Not Implemented Yet -- Abort ***\n");
    exit(1);
    return 1;
}

  /* Compress and array with Predictor-Huffman algorithm */
  
int cbf_compress_predict (void *buf, size_t elsize, int elsign, size_t nelem,
                         unsigned int compression, size_t repeat,
                         cbf_file *file)
{
    fprintf(stderr,
      "\n*** Byte-Offset Algorithm Not Implemented Yet -- Abort ***\n");
    exit(1);
    return 1;
}

  /* Decompress an array with Predictor-Huffman algorithm */

int cbf_decompress_predict (void *buf, size_t elsize, int elsign, 
                           size_t nelem, size_t *nelem_read,
                           unsigned int compression, cbf_file *file)
{
    fprintf(stderr,
      "\n*** Byte-Offset Algorithm Not Implemented Yet -- Abort ***\n");
    exit(1);
    return 1;
}

#ifdef __cplusplus

}

#endif



