
#ifdef __cplusplus

extern "C" {

#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_canonical.h"
#include "cbf_compress.h"
#include "cbf_file.h"

#define CBF_TABLEENTRYBITS  8  /* Bits in a table entry             */
#define CBF_MAXBITS        15  /* Maximum bits in a simple code     */
#define CBF_MAXMAXBITS     65  /* Bits in an coded integer          */
#define CBF_MAXCODEBITS    64  /* Bits in a code                    */

#define CBF_SHIFT63 (sizeof (int) * CHAR_BIT > 64 ? 63 : 0)


  /* Compression tree node */

typedef struct cbf_compress_nodestruct
{
  size_t       count;           /* Number in the file                  */
  unsigned int code;            /* Code                                */
  unsigned int bitcount;        /* Bits in the minimum-redundancy code */
  unsigned int bitcode [4];     /* Minimum-redundancy code             */

  struct cbf_compress_nodestruct *next,
                                 *child [2];
}
cbf_compress_node;


  /* Compression data */

typedef struct
{
  cbf_file *file;                /* File                           */

  unsigned int bits;             /* Coded bits                     */
  unsigned int maxbits;          /* Maximum saved bits             */

  unsigned int endcode;          /* End-of-data code               */

  size_t       nodes;            /* Number of nodes                */
  size_t       nextnode;         /* Number of nodes used           */

  cbf_compress_node *node;       /* Nodes                       */
}
cbf_compress_data;


  /* Create compression data */

int cbf_make_compressdata (cbf_compress_data **data, cbf_file *file)
{
    /* Does the file exist? */

  if (!file)

    return CBF_ARGUMENT;

  if (!file->stream)

    return CBF_ARGUMENT;


    /* Allocate memory */

  cbf_failnez (cbf_alloc ((void **) data, NULL, sizeof (cbf_compress_data), 1))


    /* Initialise */

  (*data)->file = file;
  
  (*data)->bits     =
  (*data)->maxbits  = 0;

  (*data)->endcode  = 0;

  (*data)->nodes    = 
  (*data)->nextnode = 0;
 
  (*data)->node = NULL;


    /* Success */

  return 0;
}


  /* Free data */

void cbf_free_compressdata (cbf_compress_data *data)
{
    /* Free storage */

  if (data)
  {
    cbf_free ((void **) &data->node, &data->nodes);

    cbf_free ((void **) &data, NULL);
  }
}


  /* Initialise compression data arrays */

int cbf_initialise_compressdata (cbf_compress_data *data, unsigned int bits, 
                                                          unsigned int maxbits)
{
  size_t count;
  
  cbf_compress_node *node;


    /* Coded bits */

  if (bits > CBF_MAXBITS)

    return CBF_FORMAT;


    /* Codes must fit int + 1 bit */

  if (maxbits > CBF_MAXMAXBITS)

    return CBF_FORMAT;

  if (maxbits < sizeof (int) * CHAR_BIT + 1)
  {
    maxbits = sizeof (int) * CHAR_BIT + 1;

    if (maxbits > CBF_MAXMAXBITS)

      maxbits = CBF_MAXMAXBITS;
  }

  if (maxbits < bits)

    return CBF_FORMAT;


    /* Update the values */

  data->bits = bits;

  data->maxbits = maxbits;


    /* end-of-code code */

  data->endcode = 1 << bits;


    /* Allocate memory for the nodes */

  count = (data->endcode + maxbits) * 2 + 1;

  cbf_failnez (cbf_realloc ((void **) &data->node, &data->nodes,
                                      sizeof (cbf_compress_node), count))


    /* Initialise the nodes */

  node = data->node;

  for (count = 0; count < data->nodes; count++, node++)
  {
    node->bitcount  = 0;
    node->count     = 0;

    node->next      =
    node->child [0] =
    node->child [1] = NULL;

    if (count < data->endcode)

      node->code = count - ((count << 1) & data->endcode);

    else

      node->code = count;
  }

  data->nextnode = 0;


    /* Success */

  return 0;
}


  /* Write a compression table */

int cbf_put_table (cbf_compress_data *data, unsigned int *bitcount)
{
  unsigned int count, codes, endcode, maxbits;


    /* Coded bits */

  cbf_failnez (cbf_put_integer (data->file, data->bits, 0, CBF_TABLEENTRYBITS))

  *bitcount = CBF_TABLEENTRYBITS;
  

    /* How many symbols do we actually use? */

  endcode = 1 << data->bits;

  for (codes = endcode + data->maxbits; data->node [codes].bitcount == 0; codes--);

  codes++;
    

    /* Maximum bits used */

  if (codes > endcode + data->bits)
    
    maxbits = codes - endcode - 1;

  else

    maxbits = data->bits;
 
  cbf_failnez (cbf_put_integer (data->file, maxbits, 0, CBF_TABLEENTRYBITS))

  *bitcount += CBF_TABLEENTRYBITS;


    /* Minimum-redundancy code lengths */

  for (count = 0; count < codes; count++)
  {
    if (count == endcode + 1)

      count = endcode + data->bits + 1;

    cbf_failnez (cbf_put_integer (data->file, data->node [count].bitcount, 0, CBF_TABLEENTRYBITS))

    *bitcount += CBF_TABLEENTRYBITS;
  }


    /* Success */

  return 0;
}


  /* Read a compression table */

int cbf_get_table (cbf_compress_data *data)
{
  unsigned int bits, maxbits, endcode, count;


    /* Coded bits */

  cbf_failnez (cbf_get_integer (data->file, (int *) &bits, 0, CBF_TABLEENTRYBITS))


    /* Maximum number of bits */

  cbf_failnez (cbf_get_integer (data->file, (int *) &maxbits, 0, CBF_TABLEENTRYBITS))


    /* Initialise the data */

  cbf_failnez (cbf_initialise_compressdata (data, bits, maxbits))


    /* Reserve nodes */

  endcode = 1 << data->bits;

  data->nextnode = endcode + data->maxbits + 1;


    /* Read the table */

  for (count = 0; count <= endcode + maxbits; count++)
  {
    cbf_failnez (cbf_get_integer (data->file, (int *) &bits, 0, CBF_TABLEENTRYBITS))

    if (count == endcode + 1)

      count = endcode + data->bits + 1;

    data->node [count].bitcount = bits;
  }


    /* Success */
    
  return 0;
}


  /* End the bitstream */

int cbf_put_stopcode (cbf_compress_data *data, unsigned int *bitcount)
{
  unsigned int endcode;
  
  endcode = 1 << data->bits;

  cbf_failnez (cbf_put_bits (data->file, (int *) data->node [endcode].bitcode,
                                             data->node [endcode].bitcount))

  cbf_failnez (cbf_put_integer (data->file, 0, 0, 7))
     
  *bitcount = data->node [endcode].bitcount;


    /* Success */

  return 0;
}


  /* Insert a node into a tree */

cbf_compress_node *cbf_insert_node (cbf_compress_node *tree, cbf_compress_node *node)
{
  if (tree)
  {
    if (node->count > tree->count)

      tree->child [1] = cbf_insert_node (tree->child [1], node);

    else

      tree->child [0] = cbf_insert_node (tree->child [0], node);

    return tree;
  }
  
  return node;
}


  /* Append a node to a list */

cbf_compress_node *cbf_append_node (cbf_compress_node *list, cbf_compress_node *node)
{
  cbf_compress_node *next;

  if (list)
  {
    next = list;
    
    while (next->next)
    
      next = next->next;
      
    next->next = node;
    
    return list;
  }
  
  return node;
}


  /* Convert an ordered tree into an ordered list */

cbf_compress_node *cbf_order_node (cbf_compress_node *tree)
{
  if (tree)
  
    return cbf_append_node (cbf_append_node (cbf_order_node (tree->child [0]), tree),
                                             cbf_order_node (tree->child [1]));

  return NULL;
}


  /* Create an ordered list */

cbf_compress_node *cbf_create_list (cbf_compress_data *data) {
  
  unsigned int count, endcode, codes;

  cbf_compress_node *tree, *list, *node;


    /* Sort the nodes */

  endcode = 1 << data->bits;

  codes = endcode + data->maxbits + 1;
  
  node = data->node;

  tree = NULL;

  for (count = 0; count < codes; count++)

    if (node [count].count)
    
      tree = cbf_insert_node (tree, node + count);

  list = cbf_order_node (tree);


    /* Dismantle the tree */

  for (count = 0; count < codes; count++)
  
    node [count].child [0] = 
    node [count].child [1] = NULL;

  return list;
}


  /* Combine the two nodes with minimum count */

cbf_compress_node *cbf_reduce_list (cbf_compress_data *data, cbf_compress_node *list)
{
  cbf_compress_node *node, *next, *cnext;
   

    /* Construct a node */

  node = data->node + data->nextnode;

  data->nextnode++;


    /* Attach the top nodes */

  node->child [0] = list;
  node->child [1] = list->next;
  node->count     = list->count + list->next->count;


    /* Put it at the top */

  next = node->next = list->next->next;


    /* Order correct?  */

  if (next == NULL)

    return node;
    
  if (node->count <= next->count)

    return node;


    /* Otherwise move the node down to the correct position */

  cnext = next;

  while (cnext->next)

    if (node->count < cnext->count || node->count > cnext->next->count)
  
      cnext = cnext->next;

    else

      break;
    
  node->next  = cnext->next;
  cnext->next = node;

  return next;
}


  /* Generate the minimum-redundancy code lengths */

int cbf_generate_codelengths (cbf_compress_node *tree, int bitcount)
{
  if (tree)
  {
    tree->bitcount = bitcount;

    cbf_generate_codelengths (tree->child [0], bitcount + 1);
    cbf_generate_codelengths (tree->child [1], bitcount + 1);
  }


    /* Success */

  return 0;
}


  /* Reverse the order of the bits in the bit-codes */

int cbf_reverse_bitcodes (cbf_compress_data *data)
{
  unsigned int node, endcode, codes, count, index [2][2], bit [2];

  endcode = 1 << data->bits;

  codes = endcode + data->maxbits + 1;

  
    /* Reverse the order of the bits in the code */

  for (node = 0; node < codes; node++)

    if (data->node [node].bitcount > 0)

      for (count = 0; count < data->node [node].bitcount - count - 1; count++)
      {
        bit [0] = count;
        bit [1] = data->node [node].bitcount - count - 1;

        index [0][0] = bit [0] % (sizeof (unsigned int) * CHAR_BIT);
        index [0][1] = bit [0] / (sizeof (unsigned int) * CHAR_BIT);
        index [1][0] = bit [1] % (sizeof (unsigned int) * CHAR_BIT);
        index [1][1] = bit [1] / (sizeof (unsigned int) * CHAR_BIT);

        bit [0] = (data->node [node].bitcode [index [0][1]] >> (index [0][0])) & 1;
        bit [1] = (data->node [node].bitcode [index [1][1]] >> (index [1][0])) & 1;

        data->node [node].bitcode [index [0][1]] ^= (bit [0] ^ bit [1]) << index [0][0];
        data->node [node].bitcode [index [1][1]] ^= (bit [0] ^ bit [1]) << index [1][0];
      }
 

    /* Success */

  return 0;
}


  /* Generate the canonical bit-codes */

int cbf_generate_canonicalcodes (cbf_compress_data *data)
{
  unsigned int count [2],
               base [CBF_MAXCODEBITS],
               node, codes, endcode, bits;

  endcode = 1 << data->bits;

  codes = endcode + data->maxbits + 1;


    /* Count the number of symbols with the same number of bits */

  memset (base, 0, sizeof (base));

  for (node = 0; node < codes; node++)
  {
    bits = data->node [node].bitcount;

    if (bits > CBF_MAXCODEBITS)

      return CBF_ARGUMENT;

    if (bits > 0)
    {
      memset (data->node [node].bitcode, 0, 4 * sizeof (unsigned int));

      data->node [node].bitcode [0] = base [bits - 1];

      base [bits - 1]++;
    }
  }


    /* Generate the initial code values */

  count [0] = 0;

  for (bits = CBF_MAXCODEBITS - 1; bits > 0; bits--)
  {
    count [1] = base [bits - 1];
    
    base [bits - 1] = (base [bits] + count [0]) / 2;

    count [0] = count [1];
  }


    /* Add the initial code to the count */

  for (node = 0; node < codes; node++)
  {
    bits = data->node [node].bitcount;

    if (bits > 0)

      data->node [node].bitcode [0] += base [bits - 1];
  }


    /* Reverse the order of the bits in the code */

  return cbf_reverse_bitcodes (data);
}


  /* Compare the bitcodes of two nodes */

int cbf_compare_bitcodes (const void *void1, const void *void2)
{
  const cbf_compress_node *node1, *node2;

  const unsigned int *code1, *code2;
  
  unsigned int bit, bits;

  node1 = (const cbf_compress_node *) void1;
  node2 = (const cbf_compress_node *) void2;


    /* Get the codes */
  
  code1 = node1->bitcode;
  code2 = node2->bitcode;

  bits = node1->bitcount;

  if (bits > node2->bitcount)

    bits = node2->bitcount;


    /* Is either node not used? */

  if (bits == 0)
  {
    if (node1->bitcount == node2->bitcount)

      return 0;

    return 1 - ((node1->bitcount != 0) << 1);
  }


    /* Compare the codes bit-by-bit */

  for (bit = 0; bits > 0; bit++, bits--)
  {
    if (bit == sizeof (int) * CHAR_BIT)
    {
      bit = 0;

      code1++;
      code2++;
    }

    if (((*code1 ^ *code2) >> bit) & 1)

      return ((*code1 >> bit) & 1) - ((*code2 >> bit) & 1);
  }


    /* Same code */

  return 0;
}


  /* Construct a tree from an ordered set of nodes */

int cbf_construct_tree (cbf_compress_data *data, cbf_compress_node **node,
                                       int bits, cbf_compress_node **root)
{
  cbf_compress_node *nextnode;

  if (node == NULL)
  {
    nextnode = data->node;
    
    node = &nextnode;
  }

  
    /* Create the node */

  *root = data->node + data->nextnode;

  data->nextnode++;

  
    /* Make the 0 branch then the 1 branch */

  if ((*node)->bitcount == bits)
  {
    (*root)->child [0] = *node;

    (*node)++;
  }
  else

    cbf_failnez (cbf_construct_tree (data, node, bits + 1, &(*root)->child [0]))

  if ((*node)->bitcount == bits)
  {
    (*root)->child [1] = *node;

    (*node)++;
  }
  else

    cbf_failnez (cbf_construct_tree (data, node, bits + 1, &(*root)->child [1]))


    /* Success */

  return 0;
}


  /* Sort the nodes and set up the decoding arrays */

int cbf_setup_decode (cbf_compress_data *data, cbf_compress_node **start)
{
    /* Generate the codes */

  cbf_failnez (cbf_generate_canonicalcodes (data))


    /* Sort the nodes in order of the codes */
    
  qsort (data->node, data->nextnode, sizeof (cbf_compress_node),
                                             cbf_compare_bitcodes);


    /* Construct the tree */

  return cbf_construct_tree (data, NULL, 1, start);
}


  /* Calculate the expected bit count */

unsigned int cbf_count_bits (cbf_compress_data *data)
{
  unsigned int endcode, codes, bitcount, code;

  cbf_compress_node *node;

  endcode = 1 << data->bits;

  node = data->node;


    /* How many symbols do we actually use? */

  for (codes = endcode + data->maxbits; node [codes].bitcount == 0; codes--);

  codes++;
    

    /* Compression table */

  if (codes > endcode + data->bits)
    
    bitcount = 2 * CBF_TABLEENTRYBITS + (codes - data->bits) * CBF_TABLEENTRYBITS;

  else

    bitcount = 2 * CBF_TABLEENTRYBITS + (endcode + 1) * CBF_TABLEENTRYBITS;


    /* Compressed data */

  for (code = 0; code < endcode; code++, node++)

    bitcount += node->count * node->bitcount;

  for (; code < codes; code++, node++)

    bitcount += node->count * (node->bitcount + code - endcode);

  return bitcount;
}


  /* Read a code */

int cbf_get_code (cbf_compress_data *data, cbf_compress_node *root, 
                                                unsigned int *code, 
                                                unsigned int *bitcount)
{
  int bits0, bits1;
 
    /* Decode the bitstream  */

  bits0 = data->file->bits [0];
  bits1 = data->file->bits [1];

  while (*(root->child))
  {
    if (bits0 == 0)
    {
      bits1 = getc (data->file->stream);

      if (bits1 == EOF)
      {
        data->file->bits [0] =
        data->file->bits [1] = 0;

        return CBF_FILEREAD;
      }

      bits0 = 8;
    }

    root = root->child [bits1 & 1];

    bits1 >>= 1;

    bits0--; 
  }

  data->file->bits [0] = bits0;
  data->file->bits [1] = bits1;

  *code = root->code;


    /* Simple coding? */

  if ((int) *code < (int) data->endcode)
  {
    *bitcount = data->bits;

    return 0;
  }


    /* Coded bit count? */

  *code -= data->endcode;

  if (*code)

    if (*code > data->maxbits)

      return CBF_FORMAT;

    else
    {
      *bitcount = *code;

      return cbf_get_bits (data->file, (int *) code, *code);
    }


    /* End code */

  return CBF_ENDOFDATA;
}


  /* Write a coded integer */

int cbf_put_code (cbf_compress_data *data, int code, unsigned int overflow,
                                                     unsigned int *bitcount)
{
  unsigned int bits, m, endcode;

  int overcode [2], *usecode;

  cbf_compress_node *node;

  endcode = 1 << data->bits;


    /* Does the number fit in an integer? */

  if (!overflow)
  {
      /* Code direct? */

    m = (code ^ (code << 1));

    if ((m & -((int) endcode)) == 0)
    {
        /* Code the number */
      
      node = data->node + (code & (endcode - 1));

      bits = node->bitcount;

      cbf_put_bits (data->file, (int *) node->bitcode, bits);

      *bitcount = bits;

      return 0;
    }

      /* Count the number of bits */
      
    bits = sizeof (int) * CHAR_BIT;

    while (((m >> (bits - 1)) & 1) == 0)
    
      bits--;

    usecode = &code;
  }
  else
  {
      /* Overflow */

    overcode [0] = code;

    overcode [1] = -(code < 0);

    usecode = overcode;

    bits = sizeof (int) * CHAR_BIT;
  }


    /* Code the number of bits */

  node = data->node + endcode + bits;

  cbf_put_bits (data->file, (int *) node->bitcode, node->bitcount);


    /* Write the number */

  cbf_put_bits (data->file, usecode, bits);
    
  *bitcount = bits + node->bitcount;


    /* Success */

  return 0;
}


  /* Count the values */

int cbf_count_values (cbf_compress_data *data,
                      void *buf, size_t elsize, int elsign, size_t nelem,
                      int *minelem, int *maxelem)
{
  int code;

  unsigned int count, element, lastelement, minelement, maxelement,
           unsign, sign, bitcount, m, endcode, limit;

  unsigned char *unsigned_char_data;

  cbf_compress_node *node;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;


    /* Initialise the pointers */

  unsigned_char_data = (unsigned char *) buf;

  node = data->node;


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


    /* Initialise the minimum and maximum elements */

  minelement = ~0;

  maxelement = 0;


    /* Start from 0 */

  lastelement = unsign;

  endcode = 1 << data->bits;

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


      /* Calculate the offset to save */

    code = element - lastelement;


      /* Overflow? */

    if ((element < lastelement) ^ (code < 0))

      node [endcode + sizeof (int) * CHAR_BIT + 1].count++;

    else
    {
        /* Encode the offset */
    
      m = (code ^ (code << 1));

      if ((m & -((int) endcode)) == 0)

          /* Simple code */

        node [code & (endcode - 1)].count++;

      else
      {
          /* Count the number of bits */
      
        bitcount = sizeof (int) * CHAR_BIT;

        while (((m >> (bitcount - 1)) & 1) == 0)
    
          bitcount--;

        node [endcode + bitcount].count++;
      }
    }


      /* Update the previous element */
        
    lastelement = element;
  }


    /* Make the minimum and maxium signed? */

  minelement -= unsign;
  maxelement -= unsign;


    /* Save the minimum and maximum */

  if (nelem)
  {
    *minelem = (int) minelement;
    *maxelem = (int) maxelement;
  }
      

    /* End code */

  node [endcode].count = 1;

  data->nextnode = endcode + data->maxbits + 1;


    /* Success */

  return 0;
}


  /* Compress an array */

int cbf_compress_canonical (void *buf, size_t elsize, int elsign, size_t nelem,
                            unsigned int compression, size_t repeat,
                            cbf_file *file)
{
  int code, minelement, maxelement;

  unsigned int count, element, lastelement, bits,
           unsign, sign, limit, bitcount, endcode, expected_bitcount;

  unsigned char *unsigned_char_data;

  cbf_compress_node *node, *start;

  cbf_compress_data *data;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;


    /* Create and initialise the compression data */

  cbf_failnez (cbf_make_compressdata (&data, file))

  cbf_onfailnez (cbf_initialise_compressdata (data, 8, 0), 
                   cbf_free_compressdata (data))


    /* Count the symbols */

  cbf_onfailnez (cbf_count_values (data, buf, elsize, elsign, nelem, 
                               &minelement, &maxelement),
                               cbf_free_compressdata (data))


    /* Generate the code lengths */

  start = cbf_create_list (data);

  while (start->next)

    start = cbf_reduce_list (data, start);

  cbf_generate_codelengths (start, 0);


    /* Count the expected number of bits */

  expected_bitcount = cbf_count_bits (data);


    /* Discard any bits in the buffers */

  cbf_onfailnez (cbf_reset_bits (file),
             cbf_free_compressdata (data))


    /* Write the coding id (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, compression, 0, 64),
             cbf_free_compressdata (data))


    /* Write the number of elements (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, nelem, 0, 64),
             cbf_free_compressdata (data))


    /* Write the minimum element (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, minelement, elsign, 64),
             cbf_free_compressdata (data))


    /* Write the maximum element (64 bits) */

  cbf_onfailnez (cbf_put_integer (file, maxelement, elsign, 64),
             cbf_free_compressdata (data))


    /* Write the repeat size (64 bits)? */

  if (compression > CBF_CANONICAL)

    cbf_onfailnez (cbf_put_integer (file, repeat, 0, 64),
               cbf_free_compressdata (data))


    /* Write the table */
 
  cbf_onfailnez (cbf_put_table (data, &bitcount), cbf_free_compressdata (data))


    /* Generate the canonical bitcodes */

  cbf_onfailnez (cbf_generate_canonicalcodes (data), cbf_free_compressdata (data))
  
  
    /* Initialise the pointers */

  unsigned_char_data = (unsigned char *) buf;

  node = data->node;


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

  endcode = 1 << data->bits;

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


      /* Calculate the offset to save */

    code = element - lastelement;


      /* Write the (overflowed?) code */

    cbf_onfailnez (cbf_put_code (data, code, 
               (element < lastelement) ^ (code < 0), &bits),
                cbf_free_compressdata (data))

    bitcount += bits;


      /* Update the previous element */
        
    lastelement = element;
  }


    /* End code */

  cbf_onfailnez (cbf_put_stopcode (data, &bits), cbf_free_compressdata (data))

  bitcount += bits;


    /* Free memory */

  cbf_free_compressdata (data);


    /* Does the actual bit count match the expected bit count? */

  if (bitcount != expected_bitcount)

    return CBF_BITCOUNT;


    /* Success */

  return 0;
}


  /*****************************************************************/
  /* THIS FUNCTION WILL FAIL WITH VALUES OUTSIDE THE INTEGER RANGE */
  /*****************************************************************/

  /* Decompress an array (from the start of the table) */

int cbf_decompress_canonical (void *buf, size_t elsize, int elsign, 
                              size_t nelem, size_t *nelem_read,
                              unsigned int compression, cbf_file *file)
{
  unsigned int bits, element, sign, unsign, limit, count64, count, repeat;

  unsigned char *unsigned_char_data;

  cbf_compress_data *data;

  cbf_compress_node *start;

  unsigned int offset [4], last_element [4];

  int errorcode;


    /* Is the element size valid? */
    
  if (elsize != sizeof (int) &&
      elsize != sizeof (short) &&
      elsize != sizeof (char))

    return CBF_ARGUMENT;


    /* Create and initialise the compression data */

  cbf_failnez (cbf_make_compressdata (&data, file))


    /* Read the repeat size (64 bits)? */

  if (compression > CBF_CANONICAL)

    cbf_onfailnez (cbf_get_integer (file, (int *) &repeat, 0, 64),
               cbf_free_compressdata (data))


    /* Read the compression table */

  cbf_onfailnez (cbf_get_table (data), cbf_free_compressdata (data))


    /* Set up the decode data */
  
  cbf_onfailnez (cbf_setup_decode (data, &start), cbf_free_compressdata (data))


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

  for (count = 0; count < nelem; count++)
  {
      /* Read the offset */

    errorcode = cbf_get_code (data, start, offset, &bits);

    if (errorcode)
    {
      if (nelem_read)
      
        *nelem_read = count;

      cbf_free_compressdata (data);
      
      return errorcode;
    }


      /* Update the current element */

    last_element [0] += offset [0];

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


    /* Number read */

  if (nelem_read)
  
    *nelem_read = count;


    /* Free memory */

  cbf_free_compressdata (data);


    /* Success */

  return 0;
}


#ifdef __cplusplus

}

#endif



