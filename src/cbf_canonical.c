/**********************************************************************
 * cbf_canonical -- canonical-code compression                        *
 *                                                                    *
 * Version 0.8.0 20 July 2008                                         *
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
 *                                                                    *
 * ALTERNATIVELY YOU MAY REDISTRIBUTE THE CBFLIB API UNDER THE TERMS  *
 * OF THE LGPL                                                        *
 *                                                                    *
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

/************************* LGPL NOTICES *******************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
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
 * The term ‘this software’, as used in these Notices, refers to      *
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


  /* Create compression data */

int cbf_make_compressdata (cbf_compress_data **data, cbf_file *file)
{
    /* Does the file exist? */
    
    if (!file)
        
        return CBF_ARGUMENT;
    
    /* Allocate memory */
    
    cbf_failnez (cbf_alloc ((void **) data, NULL, sizeof (cbf_compress_data), 1))
    
    
    /* Initialise */
    
    (*data)->file     = file;
    
    (*data)->bits     = 0;
    (*data)->maxbits  = 0;
    (*data)->endcode  = 0;
    (*data)->nodes    = 0;
    (*data)->nextnode = 0;
    
    (*data)->node = NULL;
    
    
    /* Success */
    
    return 0;
}


  /* Free data */

void cbf_free_compressdata (cbf_compress_data *data)
{
    void * memblock;
    
    void * vnode;
    
    memblock = (void *)data;
    
    /* Free storage */
    
    if (data)
    {
        vnode =  (void *)data->node;
        
        cbf_free ((void **) &vnode, &data->nodes);
        
        data->node = NULL;
        
        cbf_free ((void **) &memblock, NULL);
        
    }
}


  /* Initialise compression data arrays */

int cbf_initialise_compressdata (cbf_compress_data *data, unsigned int bits,
                                                          unsigned int maxbits)
{
    size_t count;
    
    cbf_compress_node *node;
    
    void *vnode;
    
    
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
    
    vnode = (void *)data->node;
    
    cbf_failnez (cbf_realloc ((void **) &vnode, &data->nodes,
                              sizeof (cbf_compress_node), count))
    
    data->node = (cbf_compress_node *)vnode;
    
    
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
    
    for (codes = endcode + data->maxbits; data->node [codes].bitcount == 0;
         codes--);
    
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
        
        cbf_failnez (cbf_put_integer (data->file,
                                      data->node [count].bitcount, 0,
                                      CBF_TABLEENTRYBITS))
        
        *bitcount += CBF_TABLEENTRYBITS;
    }
    
    /* Success */
    
    return 0;
}


  /* Read a compression table */

int cbf_get_table (cbf_compress_data *data)
{
    unsigned int bits, maxbits, endcode, count, tbits;
    
    
    /* Coded bits */
    
    cbf_failnez (cbf_get_integer (data->file, (int *) &bits, 0,
                                  CBF_TABLEENTRYBITS))
    
    
    /* Maximum number of bits */
    
    cbf_failnez (cbf_get_integer (data->file, (int *) &maxbits, 0,
                                  CBF_TABLEENTRYBITS))
    
    /* Initialise the data */
    
    cbf_failnez (cbf_initialise_compressdata (data, bits, maxbits))
    
    
    /* Reserve nodes */
    
    endcode = 1 << data->bits;
    
    data->nextnode = endcode + data->maxbits + 1;
    
    
    
    /* Read the table */
    
    tbits = bits;
    
    for (count = 0; count <= endcode+maxbits; count++)
    {
        
        if (tbits == maxbits && count ==endcode+1) break;
        
        cbf_failnez (cbf_get_integer (data->file, (int *) &bits, 0,
                                      CBF_TABLEENTRYBITS))
        
        if (count == endcode + 1) {
            
            count = endcode + data->bits + 1;
            
        }
        
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
    
    cbf_failnez (cbf_put_bits (data->file,
                               (int *) data->node [endcode].bitcode,
                               data->node [endcode].bitcount))
    
    *bitcount = data->node [endcode].bitcount;
    
    
    /* Success */
    
    return 0;
}


  /* Insert a node into a tree */

cbf_compress_node *cbf_insert_node (cbf_compress_node *tree,
                                    cbf_compress_node *node)
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

cbf_compress_node *cbf_append_node (cbf_compress_node *list,
                                    cbf_compress_node *node)
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
        
        return cbf_append_node (cbf_append_node (cbf_order_node (tree->child [0]),
                                                 tree),  cbf_order_node (tree->child [1]));
    
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

cbf_compress_node *cbf_reduce_list (cbf_compress_data *data,
                                    cbf_compress_node *list)
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
                
                bit [0] = (data->node [node].bitcode [index [0][1]]
                           >> (index [0][0])) & 1;
                bit [1] = (data->node [node].bitcode [index [1][1]]
                           >> (index [1][0])) & 1;
                
                data->node [node].bitcode [index [0][1]] ^= (bit [0] ^ bit [1])
                << index [0][0];
                data->node [node].bitcode [index [1][1]] ^= (bit [0] ^ bit [1])
                << index [1][0];
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
    
    if (bits > CBF_MAXMAXBITS) {
        
        return CBF_ARGUMENT;
        
    }
    
    if (node == NULL)
    {
        nextnode = data->node;
        
        node = &nextnode;
    }
    
    
    /* Create the node */
    
    *root = data->node + data->nextnode;
    
    data->nextnode++;
    
    
    /* Make the 0 branch then the 1 branch */
    
    if ((int)((*node)->bitcount) == bits)
    {
        (*root)->child [0] = *node;
        
        (*node)++;
        
    } else {
        
        cbf_failnez (cbf_construct_tree (data, node, bits + 1,
                                         &(*root)->child [0]))
    }
    
    if ((int)((*node)->bitcount) == bits)
    {
        (*root)->child [1] = *node;
        
        (*node)++;
        
    } else {
        
        cbf_failnez (cbf_construct_tree (data, node, bits + 1,
                                         &(*root)->child [1]))
    }
    
    
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

unsigned long cbf_count_bits (cbf_compress_data *data)
{
    unsigned int endcode, codes, code;
    
    unsigned long bitcount;
    
    cbf_compress_node *node;
    
    endcode = 1 << data->bits;
    
    node = data->node;
    
    
    /* Basic entries */
    
    bitcount = 4 * 64;
    
    
    /* How many symbols do we actually use? */
    
    for (codes = endcode + data->maxbits; node [codes].bitcount == 0; codes--);
    
    codes++;
    
    
    /* Compression table */
    
    if (codes > endcode + data->bits) {
        
        bitcount += 2 * CBF_TABLEENTRYBITS +
        (codes - data->bits) * CBF_TABLEENTRYBITS;
        
    } else {
        
        bitcount += 2 * CBF_TABLEENTRYBITS + (endcode + 1) * CBF_TABLEENTRYBITS;
        
    }
    
    
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
        if (bits0 == 0) {
            
            if (data->file->temporary || !(data->file->stream) ) {
                
                if (data->file->characters_used) {
                    
                    bits1 = *((data->file->characters)++);
                    
                    bits1 &= 0xFF;
                    
                    data->file->characters_used--;
                    
                    data->file->characters_size--;
                    
                } else {
                    
                    bits1 = EOF;
                    
                }
                
            } else  {
                
                bits1 = getc (data->file->stream);
                
            }
            
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
    
    if (*code) {
        
        if (*code > data->maxbits)
            
            return CBF_FORMAT;
        
        else
        {
            *bitcount = *code;
            
            return cbf_get_bits (data->file, (int *) code, *code);
        }
        
    }
    
    /* End code */
    
    return CBF_ENDOFDATA;
}

    /* Read a multi-precision integer code */

static int cbf_get_mpint_code (cbf_compress_data *data, cbf_compress_node *root,
                        unsigned int code[4],
                        unsigned int *bitcount, int numints)
{
    int bits0, bits1;
    
    CBF_UNUSED( numints );
    
    /* Decode the bitstream  */
    
    bits0 = data->file->bits [0];
    bits1 = data->file->bits [1];
    code[0] = code[1] = code[2] = code[3] = 0;
    
    while (*(root->child))
    {
        if (bits0 == 0) {
            
            if (data->file->temporary || !(data->file->stream)) {
                
                if (data->file->characters_used) {
                    
                    bits1 = *((data->file->characters)++);
                    
                    bits1 &= 0xFF;
                    
                    data->file->characters_used--;
                    
                    data->file->characters_size--;
                    
                } else {
                    
                    bits1 = EOF;
                    
                }
                
            } else  {
                
                bits1 = getc (data->file->stream);
                
            }
            
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
    
    code[0] = root->code;
    
    
    /* Simple coding? */
    
    if ((int) code[0] < (int) data->endcode)
    {
        *bitcount = data->bits;
        
        return 0;
    }
    
    
    /* Coded bit count? */
    
    code[0] -= data->endcode;
    
    if (code[0]) {
        
        if (code[0] > data->maxbits)
            
            return CBF_FORMAT;
        
        else
        {
            *bitcount = code[0];
            
            return cbf_get_bits (data->file, (int *) code, *bitcount);
        }
        
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


    /* Write a coded multi-precision integer */

static int cbf_put_mpint_code (cbf_compress_data *data, int code[5],
                               unsigned int overflow,
                               unsigned int *bitcount,
                               int numints)
{
    unsigned int bits, kbits, m, endcode;
    
    int *usecode;
    
    int  j;
    
    cbf_compress_node *node;
    
    endcode = 1 << data->bits;
    
    if (overflow) {
        
        usecode = code;
        
        bits = numints * sizeof( int ) * CHAR_BIT;
        
    } else {
        
        /* Code direct? */
        
        if (numints == 1) {
            
            size_t xbc;
            
            m = (code[0] ^ (code[0] << 1));
            
            cbf_mpint_get_acc_bitlength((unsigned int *)code,numints,&xbc);
            
            if ((m & -((int) endcode)) == 0)
            {
                /* Code the number */
                
                node = data->node + (code[0] & (endcode - 1));
                
                bits = node->bitcount;
                
                cbf_put_bits (data->file, (int *) node->bitcode, bits);
                
                *bitcount = bits;
                
                return 0;
            }
            
            /* Count the number of bits */
            
            bits = sizeof (int) * CHAR_BIT;
            
            while (((m >> (bits - 1)) & 1) == 0)
                
                bits--;
            
            
        } else {
            
            size_t xbc;
            
            cbf_mpint_get_acc_bitlength((unsigned int *)code,numints,&xbc);
            
            if (xbc < data->bits) {
                
                node = data->node + (code[0] & (endcode - 1));
                
                bits = node->bitcount;
                
                cbf_put_bits (data->file, (int *) node->bitcode, bits);
                
                *bitcount = bits;
                
                return 0;
                
            }
            
            bits = xbc;
            
        }
        
        usecode = code;
        
    }
    
    /* Code the number of bits */
    
    node = data->node + endcode + bits;
    
    cbf_put_bits (data->file, (int *) node->bitcode, node->bitcount);
    
    
    /* Write the number */
    
    
    for (j = 0; j < (int)bits; j += sizeof(int)*CHAR_BIT) {
        
        kbits = sizeof(int)*CHAR_BIT;
        
        if (j+kbits > bits) kbits = bits - j;
        
        cbf_put_bits (data->file,usecode, kbits);
        
        usecode++;
        
    }
    
    *bitcount = bits + node->bitcount;
    
    
    /* Success */
    
    return 0;
}



    /* Count the values */

int cbf_count_values (cbf_compress_data *data,
                      void *source, size_t elsize, int elsign, size_t nelem,
                      int *minelem, int *maxelem, char *border)
{
    int code[5] = {0,0,0,0,0};
    
    unsigned int bits, count, element[4], lastelement[4],
    minelement[4], maxelement[4], iint,
    unsign, sign, bitcount, m, endcode, limit;
    
    unsigned char *unsigned_char_data;
    
    size_t numints;
    
    cbf_compress_node *node;
    
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char) &&
        elsize != 2*sizeof(unsigned int) &&
        elsize != 4*sizeof(unsigned int))
        
        return CBF_ARGUMENT;
    
    bits = elsize * CHAR_BIT;
    
    if (bits < 1 || bits > 64)
        
        return CBF_ARGUMENT;
    
    numints = (bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));
    
    
    /* Initialise the pointers */
    
    unsigned_char_data = (unsigned char *) source;
    
    node = data->node;
    
    
    /* Maximum limit (unsigned) is 64 bits */
    
    sign = 1 << ((elsize-(numints-1)*sizeof(int))* CHAR_BIT - 1);
    
    if (elsize == numints*sizeof(int) )
        
        limit = ~0;
    
    else
        
        if (numints == 1) {
            
            limit = ~-(1 << (elsize * CHAR_BIT));
            
        } else {
            
            limit = ~-(1 << ((elsize-(numints-1)*sizeof(int)) * CHAR_BIT));
            
        }
    
    
    /* Offset to make the value unsigned */
    
    if (elsign)
        
        unsign = sign;
    
    else
        
        unsign = 0;
    
    
    /* Initialise the minimum and maximum elements */
    
    minelement[0] = minelement[1] = minelement[2] = minelement[3] = ~0;
    
    maxelement[0] = maxelement[1] = maxelement[2] = maxelement[3] = 0;
    
    
    /* Start from 0 */
    
    for (iint = 0; iint < numints; iint++)
        
        lastelement [iint] = 0;
    
    lastelement [numints-1] = unsign;
    
    
    endcode = 1 << data->bits;
    
    for (count = 0; count < nelem; count++)
    {
        /* Get the next element */
        
        
        if (numints > 1 ) {
            
            if (border[0] == 'b') {
                
                for (iint = numints; iint; iint--) {
                    
                    element[iint-1] = ((unsigned int *) unsigned_char_data)[numints-iint];
                    
                }
                
            } else {
                
                for (iint = 0; iint < numints; iint++) {
                    
                    element[iint] = ((unsigned int *) unsigned_char_data)[iint];
                    
                }
            }
            
        } else {
            
            if (elsize == sizeof (int))
                
                element[0] = *((unsigned int *) unsigned_char_data);
            
            else
                
                if (elsize == sizeof (short))
                    
                    element[0] = *((unsigned short *) unsigned_char_data);
            
                else
                    
                    element[0] = *unsigned_char_data;
            
        }
        
        unsigned_char_data += elsize;
        
        
        
        
        /* Make the element unsigned */
        
        element[numints-1] += unsign;
        
        
        /* Limit the value to 64 bits */
        
        if (element[numints-1] > limit) {
            
            if (elsign && (int) (element[numints-1] - unsign) < 0) {
                
                for (iint = 0; iint < numints; iint++) element[iint] = 0;
                
            } else {
                
                element[numints-1] = limit;
                
                for (iint = 0; iint < numints-1 ; iint++) element[iint] = ~0;
                
            }
        }
        
        /* Update the minimum and maximum values */
        
        if (numints == 1) {
            
            if (element[0] < minelement[0])
                
                minelement[0] = element[0];
            
            if (element[0] > maxelement[0])
                
                maxelement[0] = element[0];
            
            /* Calculate the offset to save */
            
            code[0] = element[0] - lastelement[0];
            
            code[1] = 0;
            
        } else {
            
            for (iint = 0; iint < numints; iint++) ((unsigned int *)code)[iint] = minelement[iint];
            
            code[numints] = 0;
            
            cbf_mpint_negate_acc((unsigned int *)code,numints+1);
            
            cbf_mpint_add_acc((unsigned int *)code,numints+1,element,numints);
            
            if (code[numints] < 0) 
                
                for (iint = 0; iint < numints; iint++) ((unsigned int *)minelement)[iint] = element[iint];
            
            for (iint = 0; iint < numints; iint++)  ((unsigned int *)code)[iint] = maxelement[iint];
            
            code[numints] = 0;
            
            cbf_mpint_negate_acc((unsigned int *)code,numints+1);
            
            cbf_mpint_add_acc((unsigned int *)code,numints+1,element,numints);
            
            if (code[numints] > 0) 
                
                for (iint = 0; iint < numints; iint++) ((unsigned int *)maxelement)[iint] = element[iint];
            
            for (iint = 0; iint < numints; iint++)  ((unsigned int *)code)[iint] = lastelement[iint];
            
            code[numints] = 0;
            
            cbf_mpint_negate_acc((unsigned int *)code,numints+1);
            
            cbf_mpint_add_acc((unsigned int *)code,numints+1,element,numints);
            
        }
        
        /*  code is the signed difference element - lastelement
         
         if code is negative and element is > lastelement as
         unsigned values, or code is non-negative and element
         is <- lastelement as unsigned values, we have an overflow
         
         */
        
        if ((numints==1 && ((element[0] < lastelement[0])^(code[0] < 0)))||
            (numints > 1 && ((code[numints]<0) ^ (code[numints-1]<0)))) {
            
            bitcount = numints * sizeof( int ) * CHAR_BIT;
            
            node [endcode + bitcount].count++;
            
        } else {
            
            /* Encode the offset */
            
            if (numints == 1) {
                
                m = (code[0] ^ (code[0] << 1));
                
                if ((m & -((int) endcode)) == 0) {
                    
                    /* Simple code */
                    
                    node [code[0] & (endcode - 1)].count++;
                    
                } else {
                    
                    /* Count the number of bits */
                    
                    bitcount = sizeof (int) * CHAR_BIT;
                    
                    while (((m >> (bitcount - 1)) & 1) == 0)
                        
                        bitcount--;
                    
                    node [endcode + bitcount].count++;
                    
                    
                }
                
            } else {
                
                size_t xbc;
                
                cbf_mpint_get_acc_bitlength((unsigned int *)code,numints,&xbc);
                
                if ( xbc >= sizeof (int) * CHAR_BIT ) {
                    
                    node [endcode + xbc].count++;                        
                    
                } else {
                    
                    m = (code[0] ^ (code[0] << 1));
                    
                    if ((m & -((int) endcode)) == 0) {
                        
                        /* Simple code */
                        
                        node [code[0] & (endcode - 1)].count++;
                        
                    } else {
                        
                        node [endcode + xbc].count++;
                        
                    }                        
                    
                }
                
            }
        }
        
        
        /* Update the previous element */
        
        for (iint=0; iint < numints; iint++) lastelement[iint] = element[iint];
    }
    
    
    /* Make the minimum and maxium signed? */
    
    minelement[numints-1] -= unsign;
    maxelement[numints-1] -= unsign;
    
    if (numints == 1) {
        int minsign, maxsign;
        minsign = -(((int)minelement[0])<0 && elsign);
        maxsign = -(((int)maxelement[0])<0 && elsign);
        minelement[1] = minelement[2] = minelement[3] = minsign;
        maxelement[1] = maxelement[2] = maxelement[3] = maxsign;
    }
    
    
    /* Save the minimum and maximum */
    
    if (nelem)
        for (iint=0; iint < 4; iint++) {
            minelem[iint] = (int) minelement[iint];
            maxelem[iint] = (int) maxelement[iint];
        }
    
    
    /* End code */
    
    node [endcode].count = 1;
    
    data->nextnode = endcode + data->maxbits + 1;
    
    
    /* Success */
    
    return 0;
}


    /* Compress an array */

int cbf_compress_canonical (void         *source,
                            size_t        elsize,
                            int           elsign,
                            size_t        nelem,
                            unsigned int  compression,
                            cbf_file     *file,
                            size_t       *binsize,
                            int          *storedbits,
                            int           realarray,
                            const char   *byteorder,
                            size_t        dimfast,
                            size_t        dimmid,
                            size_t        dimslow,
                            size_t        padding)
{
    int code[5], minelement[4], maxelement[4];
    
    unsigned int count, i, iint, element[4], lastelement[4], bits, unsign, sign, limit, endcode;
    
    unsigned long bitcount, expected_bitcount;
    
    unsigned char *unsigned_char_data;
    
    cbf_compress_node *node, *start;
    
    cbf_compress_data *data;
    
    int numints;
    
    char * border;
    
    char * rformat;
    
    CBF_UNUSED( compression );
    
    CBF_UNUSED( byteorder );
    
    CBF_UNUSED( dimfast );
    
    CBF_UNUSED( dimmid );
    
    CBF_UNUSED( dimslow );
    
    CBF_UNUSED( padding );

    CBF_UNUSED( node );

    CBF_UNUSED( endcode );
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char) &&
        elsize != 2*sizeof (int) &&
        elsize != 4*sizeof (int) )      
        
        return CBF_ARGUMENT;
    
    /* check for compatible real format */
    
    if ( realarray ) {
        
        cbf_failnez (cbf_get_local_real_format(&rformat) )
        
        if ( strncmp(rformat,"ieee",4) ) return CBF_ARGUMENT;
        
    }
    
    /* Get the local byte order */
    
    if (realarray) {
        
        cbf_get_local_real_byte_order(&border);
        
    } else {
        
        cbf_get_local_integer_byte_order(&border);
        
    }
    
    
    
    bits = elsize * CHAR_BIT;
    
    if (bits < 1 || bits > 64)
        
        return CBF_ARGUMENT;
    
    numints = (bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));
    
    
    
    /* Create and initialise the compression data */
    
    cbf_failnez (cbf_make_compressdata (&data, file))
    
    cbf_onfailnez (cbf_initialise_compressdata (data, 8, bits+1),
                   cbf_free_compressdata (data))
    
    
    /* Count the symbols */
    
    cbf_onfailnez (cbf_count_values (data, source, elsize, elsign, nelem,
                                     minelement, maxelement, border),
                   cbf_free_compressdata (data))
    
    
    /* Generate the code lengths */
    
    start = cbf_create_list (data);
    
    while (start->next)
        
        start = cbf_reduce_list (data, start);
    
    cbf_generate_codelengths (start, 0);
    
    
    /* Count the expected number of bits */
    
    expected_bitcount = cbf_count_bits (data);
    
    
    /* Write the number of elements (64 bits) */
    
    cbf_onfailnez (cbf_put_integer (file, nelem, 0, 64),
                   cbf_free_compressdata (data))
    
    
    /* Write the minimum element (64 bits) */
    
    cbf_onfailnez (cbf_put_bits (file, minelement, 64),
                   cbf_free_compressdata (data))
    
    
    /* Write the maximum element (64 bits) */
    
    cbf_onfailnez (cbf_put_bits (file, maxelement, 64),
                   cbf_free_compressdata (data))
    
    
    /* Write the reserved entry (64 bits) */
    
    cbf_onfailnez (cbf_put_integer (file, 0, 0, 64),
                   cbf_free_compressdata (data))
    
    bitcount = 4 * 64;
    
    
    /* Write the table */
    
    cbf_onfailnez (cbf_put_table (data, &bits), cbf_free_compressdata (data))
    
    bitcount += bits;
    
    
    /* Generate the canonical bitcodes */
    
    cbf_onfailnez (cbf_generate_canonicalcodes (data), \
                   cbf_free_compressdata (data))
    
    
    /* Initialise the pointers */
    
    unsigned_char_data = (unsigned char *) source;
    
    node = data->node;
    
    
    /* Maximum limit (unsigned) is 64 bits */
    
    sign = 1 << ((elsize-(numints-1)*sizeof(int))* CHAR_BIT - 1);
    
    if (elsize == sizeof (int) || elsize == numints*sizeof(int) )
        
        limit = ~0;
    
    else
        
        if (numints == 1) {
            
            limit = ~-(1 << (elsize * CHAR_BIT));
            
        } else {
            
            limit = ~-(1 << ((elsize-(numints-1)*sizeof(int)) * CHAR_BIT));
            
        }
    
    if (storedbits) 
        
        *storedbits = (numints > 1)?(numints*sizeof(int)*CHAR_BIT):(elsize*CHAR_BIT);
    
    /* Offset to make the value unsigned */
    
    if (elsign)
        
        unsign = sign;
    
    else
        
        unsign = 0;
    
    
    /* Start from 0 */
    
    for (i = 0; (int)i < numints-1; i++ ) lastelement[i] = 0;
    
    lastelement[numints-1] = unsign;
    
    endcode = 1 << data->bits;
    
    for (count = 0; count < nelem; count++)
    {
        /* Get the next element */
        
        if (numints > 1 ) {
            
            if (border[0] == 'b') {
                
                for (iint = numints; iint; iint--) {
                    
                    element[iint-1] = *((unsigned int *) unsigned_char_data);
                    
                    unsigned_char_data += sizeof (int);
                    
                }
                
            } else {
                
                for (iint = 0; (int)iint < numints; iint++) {
                    
                    element[iint] = *((unsigned int *) unsigned_char_data);
                    
                    unsigned_char_data += sizeof (int);
                }
            }
            
        } else {
            
            
            if (elsize == sizeof (int))
                
                element[0] = *((unsigned int *) unsigned_char_data);
            
            else
                
                if (elsize == sizeof (short))
                    
                    element[0] = *((unsigned short *) unsigned_char_data);
            
                else
                    
                    element[0] = *unsigned_char_data;
            
            unsigned_char_data += elsize;
        }
        
        
        /* Make the element unsigned */
        
        element[numints-1] += unsign;
        
        
        /* Limit the value to 64 bits */
        
        if (element[numints-1] > limit) {
            
            if (elsign && (int) (element[numints-1] - unsign) < 0)
                
            {
                for(i=0; (int)i < numints-1; i++) element[numints]=0;
                
                element[numints-1] = 0;
            }
            
            else
                
            {
                for(i=0; (int)i <numints-1; i++) element[numints]=0;
                
                element[numints-1] = limit;
            }
        }
        
        
        /* Calculate the offset to save */
        
        if (numints==1) {
            
            code[0] = element[0] - lastelement[0];
            
            
            
            /* Write the (overflowed?) code */
            
            cbf_onfailnez (cbf_put_code (data, code[0],
                                         (element[0] < lastelement[0]) ^ (code[0] < 0), &bits),
                           cbf_free_compressdata (data))
            
        } else {
            
            for (i=0; (int)i < numints; i++) code[i] = lastelement[i];
            
            code[numints] = 0;
            
            cbf_mpint_negate_acc((unsigned int *)code,numints+1);
            
            cbf_mpint_add_acc((unsigned int *)code,numints+1,(unsigned int *)element,numints);
            
            cbf_onfailnez (cbf_put_mpint_code (data, code,
                                               (code[numints] < 0) ^ (code[numints-1] < 0), 
                                               &bits, numints),
                           cbf_free_compressdata (data))
            
            
        }
        
        bitcount += bits;
        
        
        /* Update the previous element */
        
        for (i=0; (int)i < numints; i++) lastelement[i] = element[i];
    }
    
    
    /* End code */
    
    cbf_onfailnez (cbf_put_stopcode (data, &bits), cbf_free_compressdata (data))
    
    bitcount += bits;
    
    
    /* Free memory */
    
    cbf_free_compressdata (data);
    
    
    /* Does the actual bit count match the expected bit count? */
    
    if (bitcount != expected_bitcount) {
        
        return CBF_BITCOUNT;
        
    }         
    
    
    /* Calculate the number of characters written */
    
    if (binsize)
        
        *binsize = (bitcount + 7) / 8;
    
    
    /* Success */
    
    return 0;
}


    /* Decompress an array (from the start of the table) */

int cbf_decompress_canonical (void         *destination,
                              size_t        elsize,
                              int           elsign,
                              size_t        nelem,
                              size_t       *nelem_read,
                              size_t        compressedsize,
                              unsigned int  compression,
                              int           data_bits,
                              int           data_sign,
                              cbf_file     *file,
                              int           realarray,
                              const char   *byteorder,
                              size_t        dimover,
                              size_t        dimfast,
                              size_t        dimmid,
                              size_t        dimslow,
                              size_t        padding)
{
    unsigned int bits, element[4], sign, unsign, limit, count64, count;
    
    unsigned char *unsigned_char_data;
    
    cbf_compress_data *data;
    
    cbf_compress_node *start;
    
    unsigned int offset [4], last_element [4];
    
    size_t numints;
    
    int errorcode;
    
    int i, iint;
    
    char* rformat;
    
    char* border;
    
    CBF_UNUSED( compressedsize );
    
    CBF_UNUSED( compression );
    
    CBF_UNUSED( data_bits );
    
    CBF_UNUSED( data_sign );
    
    CBF_UNUSED( byteorder );
    
    CBF_UNUSED( dimover );
    
    CBF_UNUSED( dimfast );
    
    CBF_UNUSED( dimmid );
    
    CBF_UNUSED( dimslow );
    
    CBF_UNUSED( padding );

    CBF_UNUSED( count64 );
    
    /* Is the element size valid? */
    
    if (elsize != sizeof (int) &&
        elsize != sizeof (short) &&
        elsize != sizeof (char) &&
        elsize != 2*sizeof(unsigned int) &&
        elsize != 4*sizeof(unsigned int))
        
        return CBF_ARGUMENT;
    
    /* check for compatible real format */
    
    if ( realarray ) {
        
        cbf_failnez (cbf_get_local_real_format(&rformat) )
        
        if ( strncmp(rformat,"ieee",4) ) return CBF_ARGUMENT;
        
    }
    
    bits = elsize * CHAR_BIT;
    
    if (bits < 1 || bits > 64)
        
        return CBF_ARGUMENT;
    
    numints = (bits + CHAR_BIT*sizeof (int) -1)/(CHAR_BIT*sizeof (int));
    
    
    
    /* Discard the reserved entry (64 bits) */
    
    cbf_failnez (cbf_get_integer (file, NULL, 0, 64))
    
    
    /* Create and initialise the compression data */
    
    cbf_failnez (cbf_make_compressdata (&data, file))
    
    
    /* Read the compression table */
    
    cbf_onfailnez (cbf_get_table (data), cbf_free_compressdata (data))
    
    
    /* Set up the decode data */
    
    cbf_onfailnez (cbf_setup_decode (data, &start), cbf_free_compressdata (data))
    
    
    /* Initialise the pointer */
    
    unsigned_char_data = (unsigned char *) destination;
    
    
    /* Maximum limit (unsigned) is 64 bits */
    
    sign = 1 << ((elsize-(numints-1)*sizeof(int))* CHAR_BIT - 1);
    
    if (elsize == numints*sizeof(int) )
        
        limit = ~0;
    
    else
        
        limit = ~-(1 << ((elsize-(numints-1)*sizeof(int)) * CHAR_BIT));
    
    
    /* Offset to make the value unsigned */
    
    if (elsign)
        
        unsign = sign;
    
    else
        
        unsign = 0;
    
    /* Get the local byte order */
    
    if (realarray) {
        
        cbf_get_local_real_byte_order(&border);
        
    } else {
        
        cbf_get_local_integer_byte_order(&border);
        
    }
    
    
    /* How many ints do we need to hold 64 bits? */
    
    count64 = (64 + sizeof (int) * CHAR_BIT - 1) / (sizeof (int) * CHAR_BIT);
    
    
    /* Initialise the first element */
    
    for (iint = 0; iint < (int)(numints-1); iint++)
        
        last_element [iint] = 0;
    
    last_element [numints-1] = unsign;
    
    /* Read the elements */
    
    for (count = 0; count < nelem; count++)
    {
        /* Read the offset */
        
        
        errorcode = cbf_get_mpint_code (data, start, offset, &bits, numints);
        
        if (errorcode)
        {
            if (nelem_read)
                
                *nelem_read = count;
            
            cbf_free_compressdata (data);
            
            return errorcode;
        }
        
        
        /* Update the current element */
        
        if (numints == 1) {
            
            last_element [0] += offset [0];
            
            element[0] = last_element [0];
            
        } else {
            
            cbf_mpint_add_acc(last_element,numints,offset,(bits+sizeof(int)*CHAR_BIT-1)/(sizeof(int)*CHAR_BIT));
            
            for (i=0; i < (int)numints; i++) element[i] = last_element[i];
            
        }
        
        /* Limit the value to fit the element size */
        
        if (element[numints-1] > limit) {
            
            if (elsign && (int) (element[numints-1] - unsign) < 0)
                
                element[numints-1] = 0;
            
            else
                
                element[numints-1] = limit;
            
        }
        
        /* Make the element signed? */
        
        element[numints-1] -= unsign;
        
        
        /* Save the element */
        
        if (elsize == sizeof (int))
            
            *((unsigned int *) unsigned_char_data) = element[0];
        
        else
            
            if (elsize == sizeof (short))
                
                *((unsigned short *) unsigned_char_data) = element[0];
        
            else
                
                if (elsize == sizeof (char)) {
                    
                    *unsigned_char_data = element[0];
                    
                } else {
                    
                    if (border[0] == 'b') {
                        
                        for (iint = numints; iint; iint--) {
                            
                            ((unsigned int *) unsigned_char_data)[numints-iint] = element[iint-1];
                            
                            
                        }
                        
                    } else {
                        
                        for (iint = 0; iint < (int)numints; iint++) {
                            
                            ((unsigned int *) unsigned_char_data)[iint] = element[iint];
                            
                        }
                        
                    }
                    
                    
                }
        
        
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
