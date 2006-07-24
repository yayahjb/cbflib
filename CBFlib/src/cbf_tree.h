
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
  CBF_CATEGORY,         /* Category  */
  CBF_COLUMN            /* Column    */
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

int cbf_find_child (cbf_node **child, const cbf_node *node, const char *name);


  /* Find a parent node */

int cbf_find_parent (cbf_node **parent, const cbf_node *node, CBF_NODETYPE type);


  /* Count the number of children */

int cbf_count_children (unsigned int *children, const cbf_node *node);


  /* Get the index of a child */

int cbf_child_index (unsigned int *index, const cbf_node *node);


  /* Get the specified child */

int cbf_get_child (cbf_node **child, const cbf_node *node, unsigned int index);


  /* Add a child to a node */

int cbf_add_child (cbf_node *node, cbf_node *child);


  /* Get the name of a node */

int cbf_get_name (const char **name, cbf_node *node);


  /* All of the following functions assume that the string arguments
     have been created using cbf_copy_string and that no pointers to
     the strings are retained by the calling functions */

  /* Name a node */

int cbf_name_node (cbf_node *node, const char *name);


  /* Make a new node */

int cbf_make_node (cbf_node **node, CBF_NODETYPE type, cbf_context *context, const char *name);


  /* Make a new child node */

int cbf_make_child (cbf_node **child, cbf_node *node, CBF_NODETYPE type, const char *name);


  /* Get the value of a row */

int cbf_get_columnrow (const char **value, const cbf_node *column, unsigned int row);


  /* Set the value of a row */

int cbf_set_columnrow (cbf_node *column, unsigned int row, const char *value);


  /* Insert a value in a column */

int cbf_insert_columnrow (cbf_node *column, unsigned int row, const char *value);


  /* Delete a value from a column */

int cbf_delete_columnrow (cbf_node *column, unsigned int row);


  /* Add a value to a column */

int cbf_add_columnrow (cbf_node *column, const char *value);


#ifdef __cplusplus

}

#endif

#endif /* CBF_TREE_H */

