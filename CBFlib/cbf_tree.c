
#ifdef __cplusplus

extern "C" {

#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cbf.h"
#include "cbf_alloc.h"
#include "cbf_tree.h"
#include "cbf_context.h"
#include "cbf_binary.h"


  /* Make a new node */

int cbf_make_node (cbf_node **node, CBF_NODETYPE type, cbf_context *context, const char *name)
{
  int errorcode;
  
  if (!node)

    return CBF_ARGUMENT;


    /* Create the new node */

  cbf_failnez (cbf_alloc ((void **) node, NULL, sizeof (cbf_node), 1))

    
    /* Initialise the node */

  (*node)->type = type;

  (*node)->name = NULL;

  (*node)->link = NULL;

  (*node)->parent = NULL;
      
  (*node)->children = 0;

  (*node)->child_size = 0;

  (*node)->child = NULL;


    /* Add the context? */

  if (type == CBF_LINK)

    (*node)->context = NULL;

  else
  {
      /* Does the context exist? */

    if (context)

      (*node)->context = context;

    else
    
      (*node)->context = NULL;


      /* Add a context connection */

    cbf_onfailnez (cbf_add_contextconnection (&(*node)->context), 
               cbf_free ((void **) node, NULL))


      /* Name the node */

    errorcode = cbf_name_node (*node, name);

    if (errorcode)
    {
      errorcode |= cbf_free_context (&(*node)->context);
      
      return errorcode | cbf_free_node (*node);
    }
  }


    /* Success */

  return 0;
}

  /* Make a new node allowing for duplicates */

int cbf_make_new_node (cbf_node **node, CBF_NODETYPE type, cbf_context *context, const char *name)
{
  int errorcode;
  
  if (!node)

    return CBF_ARGUMENT;


    /* Create the new node */

  cbf_failnez (cbf_alloc ((void **) node, NULL, sizeof (cbf_node), 1))

    
    /* Initialise the node */

  (*node)->type = type;

  (*node)->name = NULL;

  (*node)->link = NULL;

  (*node)->parent = NULL;
      
  (*node)->children = 0;

  (*node)->child_size = 0;

  (*node)->child = NULL;


    /* Add the context? */

  if (type == CBF_LINK)

    (*node)->context = NULL;

  else
  {
      /* Does the context exist? */

    if (context)

      (*node)->context = context;

    else
    
      (*node)->context = NULL;


      /* Add a context connection */

    cbf_onfailnez (cbf_add_contextconnection (&(*node)->context), 
               cbf_free ((void **) node, NULL))


      /* Name the node */

    errorcode = cbf_name_new_node (*node, name);

    if (errorcode)
    {
      errorcode |= cbf_free_context (&(*node)->context);
      
      return errorcode | cbf_free_node (*node);
    }
  }


    /* Success */

  return 0;
}


  /* Free a node */

int cbf_free_node (cbf_node *node)
{
  unsigned int count;

  
    /* Check the arguments */
    
  if (!node)

    return CBF_ARGUMENT;


    /* Disconnect the node from its parent? */

  if (node->parent)

    for (count = 0; count < node->parent->children; count++)

      if (node->parent->child [count] == node)
      {
        node->parent->children--;

        if (node->parent->children == 0)

          cbf_failnez (cbf_free ((void **) &node->parent->child,
                                       &node->parent->child_size))

        else

          if (node->parent->children > count)
          
            memmove (node->parent->child + count, node->parent->child + count + 1,
                    (node->parent->children - count) * sizeof (cbf_node *));

        break;
      }


    /* Free the children */

  cbf_failnez (cbf_set_children (node, 0))


    /* Free the name */

  cbf_free_string (NULL, node->name);


    /* Free the context connection */

  cbf_failnez (cbf_delete_contextconnection (&node->context))


    /* Free the node */

  return cbf_free ((void **) &node, NULL);
}


  /* Set the number of children */

int cbf_set_children (cbf_node *node, unsigned int children)
{
  unsigned int count, new_size;

  int errorcode;


    /* Check the arguments */

  if (!node)

    return CBF_ARGUMENT;


    /* Is the current size correct? */

  if (children == node->children)

    return 0;
    

    /* Decrease the number of children? */

  if (children < node->children)
  {
    errorcode = 0;
    
    for (count = children; count < node->children; count++)

        /* Free the child */

      if (node->type == CBF_COLUMN)
    
        errorcode |= cbf_set_columnrow (node, count, NULL);

      else

        if (node->type != CBF_LINK)

          if (node->child [count])
          {
            node->child [count]->parent = NULL;
        
            errorcode |= cbf_free_node (node->child [count]);

            node->child [count] = NULL;
          }

    if (children == 0)

      errorcode = cbf_free ((void **) &node->child, &node->child_size);

    node->children = children;

    return errorcode;
  }


    /* Increase the number of children */

  if (children > node->child_size + 4)

    new_size = children + 4;

  else

    new_size = ((children + 2) * 3) / 2;

  cbf_failnez (cbf_realloc ((void **) &node->child, &node->child_size,
                                          sizeof (cbf_node *), new_size))

  node->children = children;


    /* Success */

  return 0;
}
  

  /* Trace a link */

cbf_node *cbf_get_link (const cbf_node *node)
{
  while (node)

    if (node->type == CBF_LINK)

      node = node->link;

    else

      return (cbf_node *) node;


    /* Fail */

  return NULL;
}


  /* Find a child node */

int cbf_find_child (cbf_node **child, const cbf_node *node, const char *name)
{
  unsigned int count;

  const char *namec, *nodenamec;


    /* Follow any links */

  node = cbf_get_link (node);
  

    /* Check the arguments */

  if (!node)

    return CBF_ARGUMENT;


    /* Is it a normal node? */

  if (node->type == CBF_COLUMN)

    return CBF_ARGUMENT;


    /* Search the children */

  for (count = 0; count < node->children; count++)

    if (name)
    {
      if (node->child [count]->name)
      {
        for (namec = name, nodenamec = node->child [count]->name;
            *namec && toupper (*nodenamec) == toupper (*namec);
             namec++, nodenamec++);

        if (!*namec && !*nodenamec)
        {
          if (child)
          
            *child = node->child [count];

          return 0;
        }
      }
    }
    else

      if (name == node->child [count]->name)
      {
        if (child)
        
          *child = node->child [count];

        return 0;
      }


    /* Fail */

  return CBF_NOTFOUND;
}

  /* Find a child node, accepting the last match  */

int cbf_find_last_child (cbf_node **child, const cbf_node *node, const char *name)
{
  unsigned int count;

  const char *namec, *nodenamec;


    /* Follow any links */

  node = cbf_get_link (node);
  

    /* Check the arguments */

  if (!node)

    return CBF_ARGUMENT;


    /* Is it a normal node? */

  if (node->type == CBF_COLUMN)

    return CBF_ARGUMENT;


    /* Search the children */

  for ( count = node->children; count > 0;  ) {

    count--;

    if (name)
    {
      if (node->child [count]->name)
      {
        for (namec = name, nodenamec = node->child [count]->name;
            *namec && toupper (*nodenamec) == toupper (*namec);
             namec++, nodenamec++);

        if (!*namec && !*nodenamec)
        {
          if (child)
          
            *child = node->child [count];

          return 0;
        }
      }
    }
    else

      if (name == node->child [count]->name)
      {
        if (child)
        
          *child = node->child [count];

        return 0;
      }

  }
    /* Fail */

  return CBF_NOTFOUND;
}


  /* Find a parent node */

int cbf_find_parent (cbf_node **parent, const cbf_node *node, CBF_NODETYPE type)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the arguments */

  if (!node)

    return CBF_ARGUMENT;


    /* Find the parent */
    
  while (node)
  {
    if (node->type == type)
    {
      if (parent)
      
        *parent = (cbf_node *) node;
      
      return 0;
    }

    node = node->parent;
  }


    /* Fail */

  return CBF_NOTFOUND;
}


  /* Count the number of children */

int cbf_count_children (unsigned int *children, const cbf_node *node)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the arguments */
    
  if (!children || !node)

    return CBF_ARGUMENT;


    /* Success */

  *children = node->children;
  
  return 0;
}


  /* Get the index of a child */

int cbf_child_index (unsigned int *index, const cbf_node *node)
{
  cbf_node *parent;

  unsigned int child;

  
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the arguments */

  if (!node)

    return CBF_ARGUMENT;


    /* Get the parent */

  parent = node->parent;

  if (!parent)

    return CBF_NOTFOUND;


    /* Find the child */

  for (child = 0; child < parent->children; child++)

    if (parent->child [child] == node)
    {
      if (index)

        *index = child;

      return 0;
    }


    /* Fail */

  return CBF_NOTFOUND;
}


  /* Get the specified child */

int cbf_get_child (cbf_node **child, const cbf_node *node, unsigned int index)
{
    /* Follow any links */

  node = cbf_get_link (node);
  

    /* Check the arguments */

  if (!node)

    return CBF_ARGUMENT;


    /* Is it a normal node? */

  if (node->type == CBF_COLUMN)

    return CBF_ARGUMENT;


    /* Does the child exists? */

  if (index < node->children)
  {
    if (child)
          
      *child = node->child [index];

    return 0;
  }


    /* Fail */

  return CBF_NOTFOUND;
}


  /* Get the name of a node */

int cbf_get_name (char **name, cbf_node *node)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the arguments */
      
  if (!node)

    return CBF_ARGUMENT;


    /* Set the name */

  if (name)

    *name = node->name;


    /* Success */

  return 0;
}


  /* All of the following functions assume that all of the strings have
     been created using cbf_copy_string and that no pointers to the
     strings are retained by the calling functions */

  /* Name a node */

int cbf_name_node (cbf_node *node, const char *name)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the arguments */
      
  if (!node)

    return CBF_ARGUMENT;
    

    /* Is there a sibling with this name? */

  if (node->parent)

    if (cbf_find_child (NULL, node->parent, name) == 0)

      return CBF_IDENTICAL;
      

    /* Replace the old name */

  cbf_free_string (NULL, node->name);

  node->name = (char *) name;


    /* Success */      

  return 0;
}
  /* Name a node allowing for duplicates  */

int cbf_name_new_node (cbf_node *node, const char *name)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the arguments */
      
  if (!node)

    return CBF_ARGUMENT;
    

    /* Replace the old name */

  cbf_free_string (NULL, node->name);

  node->name = (char *) name;


    /* Success */      

  return 0;
}


  /* Add a child to a node */

int cbf_add_child (cbf_node *node, cbf_node *child)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the first argument */
    
  if (!node)

    return CBF_ARGUMENT;


    /* Follow any links */

  child = cbf_get_link (child);


    /* Check the second argument */
    
  if (!child)

    return CBF_ARGUMENT;
    

    /* Is there already a child with this name? */
    
  if (cbf_find_child (NULL, node, child->name) == 0)

    return CBF_IDENTICAL;


    /* Add the child */

  cbf_failnez (cbf_set_children (node, node->children + 1))

  child->parent = node;

  node->child [node->children - 1] = child;


    /* Success */

  return 0;
}

  /* Add a child to a node with duplicates allowed */

int cbf_add_new_child (cbf_node *node, cbf_node *child)
{
    /* Follow any links */

  node = cbf_get_link (node);


    /* Check the first argument */
    
  if (!node)

    return CBF_ARGUMENT;


    /* Follow any links */

  child = cbf_get_link (child);


    /* Check the second argument */
    
  if (!child)

    return CBF_ARGUMENT;


    /* Add the child */

  cbf_failnez (cbf_set_children (node, node->children + 1))

  child->parent = node;

  node->child [node->children - 1] = child;


    /* Success */

  return 0;
}


  /* Make a new child node */

int cbf_make_child (cbf_node **child, cbf_node *node, CBF_NODETYPE type, const char *name)
{
  cbf_node *newchild;

  int errorcode;

  
    /* Check the type */

  if (type == CBF_LINK)

    return CBF_ARGUMENT;
    

    /* Follow any links */

  node = cbf_get_link (node);


    /* Does the child already exist? */

  errorcode = cbf_find_last_child (child, node, name);

  if (errorcode == 0)
  {
    cbf_free_string (NULL, name);
    
    return 0;
  }

  if (errorcode != CBF_NOTFOUND)

    return errorcode;


    /* Make a new node */

  cbf_failnez (cbf_make_node (&newchild, type, node->context, name))

  errorcode = cbf_add_child (node, newchild);

  if (errorcode)
  {
    newchild->name = NULL;

    cbf_free_node (newchild);
    
    return errorcode;
  }


    /* Success */

  if (child)

    *child = newchild;

  return 0;
}

  /* Make a new child node, with duplicates allowed */

int cbf_make_new_child (cbf_node **child, cbf_node *node,
    CBF_NODETYPE type, const char *name)
{
  cbf_node *newchild;

  int errorcode;

  
    /* Check the type */

  if (type == CBF_LINK)

    return CBF_ARGUMENT;
    

    /* Follow any links */

  node = cbf_get_link (node);



    /* Make a new node */

  cbf_failnez (cbf_make_new_node (&newchild, type, node->context, name))

  errorcode = cbf_add_new_child (node, newchild);

  if (errorcode)
  {
    newchild->name = NULL;

    cbf_free_node (newchild);
    
    return errorcode;
  }


    /* Success */

  if (child)

    *child = newchild;

  return 0;
}


  /* Change a link */

int cbf_set_link (cbf_node *link, cbf_node *node)
{
    /* Check the arguments */
    
  if (!link)

    return CBF_ARGUMENT;


    /* Check the type */

  if (link->type != CBF_LINK)

    return CBF_ARGUMENT;


    /* Change the link */

  link->link = node;


    /* Success */

  return 0;
}


  /* Add a child link */

int cbf_add_link (cbf_node *link, cbf_node *child)
{
    /* Check the arguments */
    
  if (!link)

    return CBF_ARGUMENT;


    /* Check the type */

  if (link->type != CBF_LINK)

    return CBF_ARGUMENT;


    /* Add the child */

  cbf_failnez (cbf_set_children (link, link->children + 1))

  link->child [link->children - 1] = child;
  

    /* Success */

  return 0;
}
                                                  

  /* Set a link successively to each child link */

int cbf_shift_link (cbf_node *link)
{
    /* Check the arguments */
    
  if (!link)

    return CBF_ARGUMENT;


    /* Check the type */

  if (link->type != CBF_LINK)

    return CBF_ARGUMENT;


    /* Do the children exist? */

  if (link->children == 0)

    return CBF_ARGUMENT;


    /* Change the link */

  link->link = link->child [0];


    /* Shift the children */

  memmove (link->child, link->child + 1, (link->children - 1) * sizeof (cbf_node *));

  link->child [link->children - 1] = link->link;


    /* Success */

  return 0;
}


  /* Set the value of a row */

int cbf_set_columnrow (cbf_node *column, unsigned int row, const char *value)
{
    /* Follow any links */

  column = cbf_get_link (column);


    /* Check the arguments */
    
  if (!column)

    return CBF_ARGUMENT;


    /* Check the node type */

  if (column->type != CBF_COLUMN)

    return CBF_ARGUMENT;


    /* Increase the column size? */

  if (row + 1 > column->children)

    cbf_failnez (cbf_set_children (column, row + 1))


    /* Set the value */

  cbf_failnez (cbf_free_value (column->context, (const char **) &(column->child [row])))

  column->child [row] = (cbf_node *) value;


    /* Success */

  return 0;
}


  /* Get the value of a row */

int cbf_get_columnrow (char **value, const cbf_node *column, unsigned int row)
{
    /* Follow any links */

  column = cbf_get_link (column);


    /* Check the arguments */
    
  if (!column)

    return CBF_ARGUMENT;


    /* Check the node type */

  if (column->type != CBF_COLUMN)

    return CBF_ARGUMENT;


    /* Is the value in the column? */

  if (row + 1 > column->children)

    return CBF_NOTFOUND;


    /* Success */

  if (value)

    *value = (char *) column->child [row];

  return 0;
}


  /* Inset a value into a column */

int cbf_insert_columnrow (cbf_node *column, unsigned int row, const char *value)
{
    /* Follow any links */

  column = cbf_get_link (column);


    /* Check the arguments */

  if (!column)

    return CBF_ARGUMENT;

  if (row > column->children)

    return CBF_NOTFOUND;


    /* Increase the column size */

  cbf_failnez (cbf_set_children (column, column->children + 1))


    /* Move any values further down the column */

  if (row < column->children - 1)

    memmove (column->child + row + 1, column->child + row,
               sizeof (cbf_node *) * (column->children - row - 1));
  

    /* Set the value */

  column->child [row] = (cbf_node *) value;


    /* Success */

  return 0;
}


  /* Delete a row from a column */

int cbf_delete_columnrow (cbf_node *column, unsigned int row)
{
    /* Follow any links */

  column = cbf_get_link (column);


    /* Check the arguments */

  if (!column)

    return CBF_ARGUMENT;

  if (row >= column->children)

    return CBF_NOTFOUND;


    /* Free the value */

  cbf_failnez (cbf_set_columnrow (column, row, NULL))


    /* Move any values further down the column */

  if (row < column->children - 1)

    memmove (column->child + row, column->child + row + 1,
               sizeof (cbf_node *) * (column->children - row - 1));

  column->child [column->children - 1] = NULL;


    /* Decrease the column size */

  return cbf_set_children (column, column->children - 1);
}


  /* Add a value to a column */

int cbf_add_columnrow (cbf_node *column, const char *value)
{
    /* Follow any links */

  column = cbf_get_link (column);


    /* Check the arguments */

  if (!column)

    return CBF_ARGUMENT;


    /* Add the value */

  return cbf_set_columnrow (column, column->children, value);
}


#ifdef __cplusplus

}

#endif

