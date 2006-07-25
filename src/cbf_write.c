/**********************************************************************
 * cbf_write -- write files                                           *
 *                                                                    *
 * Version 0.7.4 12 January 2004                                      *
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

#include "cbf.h"
#include "cbf_ascii.h"
#include "cbf_binary.h"
#include "cbf_compress.h"
#include "cbf_file.h"
#include "cbf_tree.h"
#include "cbf_write.h"
#include "cbf_write_binary.h"
#include "cbf_read_mime.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

int cbf_value_type (char *value);

static char wordtok[5] = "word";
static char texttok[5] = "text";
static char dblqtok[5] = "dblq";
static char sglqtok[5] = "sglq";
static char nulltok[5] = "null"; 

  /* Get the value type of an ascii string */

int cbf_get_value_type(const char *value, const char **value_type)
{

    /* Prepare an empty return */

  *value_type = NULL;

    /* Is the value missing? */

  if (!value)

    return 0;

    /* Is the value valid? */

  if ((*value & '\200') != '\200')

    return CBF_ARGUMENT;
    

    /* Has the value already been checked? */

  if ((value [0] & '\300') != '\300') {

    cbf_failnez(cbf_value_type((char *)value))

  }

  if (*value == CBF_TOKEN_WORD) {

    *value_type = wordtok;

    return 0; 

  }   

  if (*value == CBF_TOKEN_SQSTRING) {

    *value_type = sglqtok;

    return 0; 

  }   

  if (*value == CBF_TOKEN_DQSTRING) {

    *value_type = dblqtok;

    return 0; 

  }   

  if (*value == CBF_TOKEN_SCSTRING) {

    *value_type = texttok;

    return 0; 

  }   

  if (*value == CBF_TOKEN_NULL) {

    *value_type = nulltok;

    return 0; 

  }   

  return CBF_ARGUMENT;

}
  /* Set the value type of an ascii string */

int cbf_set_value_type(char *value, const char *value_type)
{

    /* Is the value type missing? */

  if (!value)

    return CBF_ARGUMENT;

    /* Is the value valid? */

  if ((*value & '\200') != '\200')

    return CBF_ARGUMENT;
    

    /* Has the value already been checked? */

  if ((value [0] & '\300') != '\300') {

    cbf_failnez(cbf_value_type(value))

  }

  if (strcmp(value_type,wordtok) == 0) {

    if ( strcmp(&value[1],".") == 0 ||

      strcmp(&value[1],"?") == 0 ||

      *value == CBF_TOKEN_WORD ) {

      *value = CBF_TOKEN_WORD;

      return 0;
    }

    return CBF_ARGUMENT; 

  }   

  if (strcmp(value_type,nulltok) == 0) {

    if ( strcmp(&value[1],".") == 0 ||

      strcmp(&value[1],"?") == 0) {

      *value = CBF_TOKEN_NULL;

      return 0;
    }

    return CBF_ARGUMENT; 

  }   

  if (strcmp(value_type,sglqtok) == 0) {

    if(strstr(&value[1],"' ") ||

      strstr(&value[1],"'\t") ||

      strstr(&value[1],"\n")) {

      return CBF_ARGUMENT;

    }

    *value = CBF_TOKEN_SQSTRING;

    return 0; 

  }   

  if (strcmp(value_type,dblqtok) == 0 ) {

    if(strstr(&value[1],"\" ") ||

      strstr(&value[1],"\"\t") ||

      strstr(&value[1],"\n")) {

      return CBF_ARGUMENT;

    }

    *value = CBF_TOKEN_DQSTRING;

    return 0; 

  }   

  if (strcmp(value_type,texttok) == 0 ) {

    if(strstr(&value[1],"\n;")) {

      return CBF_ARGUMENT;

    }

    *value = CBF_TOKEN_SCSTRING;

    return 0; 

  }   

  return CBF_ARGUMENT;

}


  /* Check the value type */

int cbf_value_type (char *value)
{
  int test [6], C, count;


    /* Is the value missing? */

  if (!value)

    return 0;


    /* Is the value valid? */

  if ((*value & '\200') != '\200')

    return CBF_ARGUMENT;
    

    /* Has the value already been checked? */

  if ((value [0] & '\300') == '\300')

    return 0;
    

    /* Properties */

  memset (test, 0, sizeof (test));

  for (count = 1; value [count]; count++)
  {
    C = toupper (value [count]);

    test [0] |= isspace (C);

    test [1] |= C == '\n';
    test [2] |= C == '\'';
    test [3] |= C == '"';

    if (count <= 5)
    {
      test [4] |= C != " DATA_" [count];
      test [5] |= C != " LOOP_" [count];

      if (count <= 1)

        test [0] |= C == '_' || C == '\'' || C == '"' || C == '#';
    }
  }

  test [0] |= strcmp (&value [1], "?") == 0;
  test [0] |= strcmp (&value [1], ".") == 0;


    /* Simple word? */

  if (!test [0] && test [4] && test [5])

    *value = CBF_TOKEN_WORD;

  else

      /* Single line? */

    if (!test [1] && (!test [2] || !test [3]))
    {
      if (!test [2])

        *value = CBF_TOKEN_SQSTRING;

      else

        *value = CBF_TOKEN_DQSTRING;
    }
    else

        /* Multiple lines */    

      *value = CBF_TOKEN_SCSTRING;


    /* Success */

  return 0;
}


  /* Write a datablock name to a file */

int cbf_write_datablockname (const cbf_node *datablock, cbf_file *file)
{
    /* Does the node exist? */

  if (!datablock)

    return CBF_ARGUMENT;


    /* Write the name */

  if (datablock->name)
  {
    cbf_failnez (cbf_write_string (file, "\ndata_"))

    cbf_failnez (cbf_write_string (file, datablock->name))

    cbf_failnez (cbf_write_character (file, '\n'))
  }
  else

    if (datablock->children)

      cbf_failnez (cbf_write_string (file, "\ndata_\n"))


    /* Success */

  return 0;
}


  /* Write an item name to a file */

int cbf_write_itemname (const cbf_node *column, cbf_file *file)
{
  cbf_node *category;


    /* Get the category */
      
  cbf_failnez (cbf_find_parent (&category, column, CBF_CATEGORY))


    /* Check that the name is valid */

  if (!category->name && !column->name)

    return CBF_ARGUMENT;


    /* Write the category name */

  cbf_failnez (cbf_write_character (file, '_'))

  if (category->name)
  {
    cbf_failnez (cbf_write_string (file, category->name))

    cbf_failnez (cbf_write_character (file, '.'))
  }


    /* Write the column name */

  if (column->name)

    cbf_failnez (cbf_write_string (file, column->name))


    /* Success */

  return 0;
}


  /* Write a value to a file */

int cbf_write_value (cbf_node *column, unsigned int row, 
                     cbf_file *file, int isbuffer)
{
  const char *text;


    /* Check the arguments */

  if (!column)

    return CBF_ARGUMENT;

  if (row >= column->children)

    return CBF_NOTFOUND;


    /* Get the value */

  cbf_failnez (cbf_get_columnrow (&text, column, row))


    /* Missing value? */
    
  if (!text)

    return cbf_write_ascii (text, file);


    /* Plain ASCII? */

  cbf_failnez (cbf_value_type ((char *) text))

  if (*text == CBF_TOKEN_WORD     ||
      *text == CBF_TOKEN_SQSTRING ||
      *text == CBF_TOKEN_DQSTRING ||
      *text == CBF_TOKEN_SCSTRING ||
      *text == CBF_TOKEN_NULL)

    return cbf_write_ascii (text, file);


    /* Plain binary? */

  if (*text == CBF_TOKEN_BIN || *text == CBF_TOKEN_TMP_BIN)

    return cbf_write_binary (column, row, file, isbuffer);


    /* Undecoded MIME? */
  
  if (*text == CBF_TOKEN_MIME_BIN)
  {
      /* Convert the value to a normal binary section */
      
    cbf_failnez (cbf_mime_temp (column, row))
  
    return cbf_write_binary (column, row, file, isbuffer);
  }


    /* Fail */

  return CBF_ARGUMENT;
}


  /* Write a category to a file */

int cbf_write_category (const cbf_node *category, cbf_file *file, int isbuffer)
{
  unsigned int count, first, last=0, column, columns, row;
  
  int loop;


    /* Check the arguments */

  if (!category)

    return CBF_ARGUMENT;

    
    /* Print out columns of the same length in loops */

  for (first = 0, loop = 1; first < category->children; first = last)
  {
    columns = 1;
    
    if (category->child [first])
    {
      for (last = first + 1; last < category->children; last++)

        if (category->child [last])
        {
          if (category->child [last]->children != 
            category->child [first]->children)

            break;
            
          columns++;
        }


        /* Make a loop? */

      if (columns > 1 || category->child [first]->children > 1)
      {
        cbf_failnez (cbf_write_string (file, "\nloop_\n"))

        loop = 1;
      }
      else
      {
        if (loop)

          cbf_failnez (cbf_write_character (file, '\n'))

        loop = 0;
      }


        /* Write the items */

      for (count = first; count < last; count++)
      {
        cbf_failnez (cbf_write_itemname (category->child [count], file))
          
        if (loop)

          cbf_failnez (cbf_write_character (file, '\n'))
      }


        /* Write the values */

      for (row = 0; row < category->child [first]->children; row++)
      {
        for (column = first; column < last; column++)

          cbf_failnez (cbf_write_value (category->child [column], row, 
                                                       file, isbuffer))

        cbf_failnez (cbf_get_filecoordinates (file, NULL, &column))

        if (column)

          cbf_failnez (cbf_write_character (file, '\n'))
      }
    }
  }


    /* Success */

  return 0;
}


  /* Write a node to a file */

int cbf_write_node (const cbf_node *node, cbf_file *file, int isbuffer)
{
  unsigned int count;
  

    /* Follow any links */

  node = cbf_get_link (node);


    /* Does the node exist? */

  if (!node)

    return CBF_ARGUMENT;


    /* Node type */

  switch (node->type)
  {
    case CBF_ROOT:

      cbf_failnez (cbf_write_string (file, "###CBF: VERSION 1.3.1\n"))
    
      if (file->write_encoding & ENC_NONE)

        cbf_failnez (cbf_write_string (file, 
                             "# CBF file written by cbflib v0.7.4\n"))
      else

        cbf_failnez (cbf_write_string (file, 
                             "# CIF file written by cbflib v0.7.4\n"))

      break;

    case CBF_DATABLOCK:

      cbf_failnez (cbf_write_datablockname (node, file))

      break;

    case CBF_CATEGORY:

      cbf_failnez (cbf_write_category (node, file, isbuffer))

      break;

    default:

      return CBF_ARGUMENT;
  }


    /* Write the children */

  if (node->type == CBF_ROOT || node->type == CBF_DATABLOCK)

    for (count = 0; count < node->children; count++)

      cbf_failnez (cbf_write_node (node->child [count], file, isbuffer))


    /* Flush the buffers */

  return cbf_flush_characters (file);
}


#ifdef __cplusplus

}

#endif
