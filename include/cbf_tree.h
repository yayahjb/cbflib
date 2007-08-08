/**********************************************************************
 * cbf_tree.h                                                         *
 *                                                                    *
 * Version 0.7.6 14 July 2006                                         *
 *                                                                    *
 *                          Paul Ellis and                            *
 *         Herbert J. Bernstein (yaya@bernstein-plus-sons.com)        *
 *                                                                    *
 * (C) Copyright 2006 Herbert J. Bernstein                            *
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

#ifndef CBF_TREE_H
#define CBF_TREE_H

#ifdef __cplusplus

extern "C" {

#endif

#include "cbf_context.h"


  /* Node types */

typedef enum
{
  CBF_UNDEFINED,        /* Undefined */
  CBF_LINK,             /* Link      */
  CBF_ROOT,             /* Root      */
  CBF_DATABLOCK,        /* Datablock */
  CBF_SAVEFRAME,        /* Saveframe */
  CBF_CATEGORY,         /* Category  */
  CBF_COLUMN,           /* Column    */
  CBF_VALUE             /* Value     */  /* Not a visible node type */
}
CBF_NODETYPE;


  /* Node structure */

typedef struct cbf_node_struct
{
  CBF_NODETYPE type;

  cbf_context *context;

  const char *name;

  struct cbf_node_struct *parent;

  struct cbf_node_struct *link;

  unsigned int children;

  size_t child_size;

  struct cbf_node_struct **child;
}
cbf_node;


  /* Prototypes */

  /* Undo links and promote to the first non-link
     freeing the memory for the links */
     
int cbf_undo_links (cbf_node **node);

  /* These function will not trace a link */

  /* Free a node */

int cbf_free_node (cbf_node *node);


  /* Set the number of children */

int cbf_set_children (cbf_node *node, unsigned int children);


  /* Change a link */

int cbf_set_link (cbf_node *link, cbf_node *node);


  /* Add a child link */

int cbf_add_link (cbf_node *link, cbf_node *child);


  /* Set a link successively to each child link */

int cbf_shift_link (cbf_node *link);


  /* These function will trace a link */

  /* Trace a link */

cbf_node *cbf_get_link (const cbf_node *node);


  /* Find a child node */

int cbf_find_child (cbf_node **child, const cbf_node *node, 
                    const char *name);

  /* Find a child node by name and type */

int cbf_find_typed_child (cbf_node **child, const cbf_node *node, 
                                            const char *name, 
                                            CBF_NODETYPE type);

  /* Find a child node, accepting the last match  */

int cbf_find_last_child (cbf_node **child, const cbf_node *node, 
                         const char *name);


  /* Find a child node, accepting the last match  */

int cbf_find_last_typed_child (cbf_node **child, const cbf_node *node, 
                         const char *name, CBF_NODETYPE type);

  /* Find a parent node */

int cbf_find_parent (cbf_node **parent, const cbf_node *node, 
                     CBF_NODETYPE type);


  /* Count the number of children */

int cbf_count_children (unsigned int *children, const cbf_node *node);


  /* Count the number of children of a given type */

int cbf_count_typed_children (unsigned int *children, const cbf_node *node, CBF_NODETYPE type);


  /* Get the index of a child */

int cbf_child_index (unsigned int *index, const cbf_node *node);


  /* Get the specified child */

int cbf_get_child (cbf_node **child, const cbf_node *node, 
                   unsigned int index);


  /* Add a child to a node */

int cbf_add_child (cbf_node *node, cbf_node *child);


  /* Add a child to a node with duplicates allowed */

int cbf_add_new_child (cbf_node *node, cbf_node *child);


  /* Get the name of a node */

int cbf_get_name (const char **name, cbf_node *node);


  /* All of the following functions assume that the string arguments
     have been created using cbf_copy_string and that no pointers to
     the strings are retained by the calling functions */

  /* Name a node */

int cbf_name_node (cbf_node *node, const char *name);


  /* Name a node allowing for duplicates  */

int cbf_name_new_node (cbf_node *node, const char *name);


  /* Make a new node */

int cbf_make_node (cbf_node **node, CBF_NODETYPE type, 
                   cbf_context *context, const char *name);


  /* Make a new node allowing for duplicates */

int cbf_make_new_node (cbf_node **node, CBF_NODETYPE type, 
                       cbf_context *context, const char *name);


  /* Make a new child node */

int cbf_make_child (cbf_node **child, cbf_node *node, 
                    CBF_NODETYPE type, const char *name);


  /* Make a new child node, with duplicates allowed */

int cbf_make_new_child (cbf_node **child, cbf_node *node,
                        CBF_NODETYPE type, const char *name);


  /* Get the value of a row */

int cbf_get_columnrow (const char **value, const cbf_node *column, 
                       unsigned int row);


  /* Set the value of a row */

int cbf_set_columnrow (cbf_node *column, unsigned int row, 
                       const char *value, int free);


  /* Insert a value in a column */

int cbf_insert_columnrow (cbf_node *column, unsigned int row, 
                          const char *value);


  /* Delete a value from a column */

int cbf_delete_columnrow (cbf_node *column, unsigned int row);


  /* Add a value to a column */

int cbf_add_columnrow (cbf_node *column, const char *value);


  /* compute a hash code for a string */
  
int cbf_compute_hashcode(const char *string, unsigned int *hashcode);



#ifdef __cplusplus

}

#endif

#endif /* CBF_TREE_H */

