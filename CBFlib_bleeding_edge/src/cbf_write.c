/**********************************************************************
 * cbf_write -- write files                                           *
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
#include "cbf_string.h"

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
static char tsqstok[5] = "tsqs";
static char tdqstok[5] = "tdqs";
static char prnstok[5] = "prns";
static char brcstok[5] = "brcs";
static char bktstok[5] = "bkts";

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

  if (*value == CBF_TOKEN_BKTSTRING) {

    *value_type = bktstok;

    return 0;

  }
  
  if (*value == CBF_TOKEN_BRCSTRING) {

    *value_type = brcstok;

    return 0;

  }
  if (*value == CBF_TOKEN_PRNSTRING) {

    *value_type = prnstok;

    return 0;

  }
  if (*value == CBF_TOKEN_TDQSTRING) {

    *value_type = tdqstok;

    return 0;

  }
  if (*value == CBF_TOKEN_TSQSTRING) {

    *value_type = tsqstok;

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

  char *cptr;
  
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
  
    cptr = &value[1];

    while (*cptr && (cptr=strstr(cptr,"\n;")) ) {
    
      if (isspace(cptr[2])) {

        cbf_warning("text field contains terminator, will be folded on output");
        
        break;

      }
      
      if (*cptr) cptr++;
    	
    }

    *value = CBF_TOKEN_SCSTRING;

    return 0;

  }


  if (strcmp(value_type,tsqstok) == 0 ) {
  
    cptr = &value[1];

    while (*cptr && (cptr=strstr(cptr,"'''")) ) {
    
      if (isspace(cptr[2])) {

        cbf_warning("triple singled-quoted field contains terminator, will be folded on output");
        
        break;

      }
      
      if (*cptr) cptr++;
    	
    }

    *value = CBF_TOKEN_TSQSTRING;

    return 0;

  }
  
    if (strcmp(value_type,tdqstok) == 0 ) {
  
    cptr = &value[1];

    while (*cptr && (cptr=strstr(cptr,"\"\"\"")) ) {
    
      if (isspace(cptr[3])) {

        cbf_warning("triple double-quoted field contains terminator, will be folded on output");
        
        break;

      }
      
      if (*cptr) cptr++;
    	
    }

    *value = CBF_TOKEN_TDQSTRING;

    return 0;

  }


  if (strcmp(value_type,prnstok) == 0 ) {
  
    *value = CBF_TOKEN_PRNSTRING;

    return 0;

  }
  

  if (strcmp(value_type,brcstok) == 0 ) {
  
    *value = CBF_TOKEN_BRCSTRING;

    return 0;

  }

  if (strcmp(value_type,bktstok) == 0 ) {
  
    *value = CBF_TOKEN_BKTSTRING;

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

  if (count <= 5) test[4]=test[5]=1;

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


  /* Write a save frame name to a file */

int cbf_write_saveframename (const cbf_node *saveframe, cbf_file *file)
{
    /* Does the node exist? */

  if (!saveframe)

    return CBF_ARGUMENT;


    /* Write the name */

  if (saveframe->name)
  {
    cbf_failnez (cbf_write_string (file, "\nsave_"))

    cbf_failnez (cbf_write_string (file, saveframe->name))

    cbf_failnez (cbf_write_character (file, '\n'))
  }
  else

    if (saveframe->children)

      cbf_failnez (cbf_write_string (file, "\nsave_(none)\n"))


    /* Success */

  return 0;
}

  /* Compose an item name from a category and column */
  
int cbf_compose_itemname (cbf_handle handle, const cbf_node *column, char * itemname, size_t limit) 
{

  cbf_node *category;
   
  char column_fill[1] = "\0";

  char * tempcat;

  char * tempcol;

  int ipos;
  
  itemname[0] = itemname[limit] = '\0';

    /* Get the category */

  cbf_failnez (cbf_find_parent (&category, column, CBF_CATEGORY))


    /* Check that the name is valid */

  if (!category->name && !column->name) {
  	
    strncpy (itemname, "_(null)", limit );

    return CBF_ARGUMENT;
    
  }

    /* construct the item name */

  if (column->name) {
  
    tempcol = (char *)column->name;

  } else {

    tempcol = column_fill;
  }

  if (!category->name ||
      !(category->name[0]) ||
      !cbf_cistrcmp("(none)",category->name) ||
      tempcol[0]=='_') {
      
    strncpy(itemname,tempcol,limit);

    if (strlen(tempcol) > limit) return CBF_ARGUMENT;


  } else {

      if(!category->name) return CBF_ARGUMENT;

      itemname[0] = '_';

      cbf_failnez( cbf_require_category_root(handle,category->name,(const char **)&tempcat))

      strncpy(itemname+1,tempcat,limit-1);

      if (strlen(tempcat) > 72 || strlen(tempcat) > limit-1) return CBF_ARGUMENT;

      ipos = strlen(itemname);

      if ( ipos < limit ) itemname[ipos++] = '.';

      if ( limit-ipos > 0) strncpy(itemname+ipos,tempcol,limit-ipos);

      if (strlen(tempcol)+ipos+2 > 75 || strlen(tempcol)+ipos+2 > limit) return CBF_ARGUMENT;


  }
  
  return 0;

}


  /* Write an item name to a file */

int cbf_write_itemname (cbf_handle handle, const cbf_node *column, cbf_file *file)
{
  char itemname[81];
  
  char buffer[255];

  char * temptag;

    /* Compose the tag and get the root alias */
  
  if ( cbf_compose_itemname (handle, column, itemname, (size_t )80)) {
  
    strcpy (itemname+77,"...");
  
    sprintf (buffer, "output line %u(%u) column name too long or invalid\n         converted to \"%s\"",
      1+file->line, 1+file->column,
      itemname);
      
    cbf_warning (buffer);
  
  }

  cbf_failnez( cbf_require_tag_root(handle,(const char *)itemname,(const char **)&temptag))



    /* Write the tag name */


  cbf_failnez (cbf_write_string (file, temptag))


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
      *text == CBF_TOKEN_TSQSTRING ||
      *text == CBF_TOKEN_TDQSTRING ||
      *text == CBF_TOKEN_PRNSTRING ||
      *text == CBF_TOKEN_BKTSTRING ||
      *text == CBF_TOKEN_BRCSTRING ||
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

int cbf_write_category (cbf_handle handle, const cbf_node *category, cbf_file *file, int isbuffer)
{
  unsigned int count, first, last=0, column, columns, row, maxrows, matrixcount;

  int loop, matrix, len;

  const char * column_name;


    /* Check the arguments */

  if (!category)

    return CBF_ARGUMENT;


    /* Print out columns of the same length in loops
       unless the number of rows is 1 */

  maxrows = 0;

  matrix = 0;

  for (first = 0, loop = 1; first < category->children; first = last)
  {
    columns = 1;

    if (category->child [first] &&
      category->child [first]->children > maxrows)
      maxrows = category->child [first]->children;

    if (category->child [first])
    {
      for (last = first + 1; last < category->children; last++)

        if (category->child [last])
        {
          if (category->child [last]->children !=
            category->child [first]->children) {

            if (category->child [last]->children > maxrows)
              maxrows = category->child [last]->children;

            break;
          }

          columns++;
        }

        /* check for a matrix        */

      matrix = 0;

      matrixcount = 0;

      for (count = first ; count < last; count++)
      {

        column_name = (category->child [count])->name;

        if (column_name) {
           len = strlen(column_name);
           if ((len > 0 && column_name[len-1]==']') ||
               (len > 4 && !cbf_cistrncmp("]_esd",(char *)column_name+len-5,5))) {
             matrixcount++;
             if (matrixcount > ((last-first+1)>>1)+1) {
               matrix = 1;
               break;
             }
           }
        }
      }

        /* Make a loop? */

      if ( matrix || (maxrows > 1 &&
        (columns > 1 || category->child [first]->children > 1)))
      {
        cbf_failnez (cbf_write_string (file, "\nloop_\n"))

        loop = 1;
      }
      else
      {

        cbf_failnez (cbf_write_character (file, '\n'))

        loop = 0;
      }

        /* Write the items for a loop */

      if (loop)
      for (count = first; count < last; count++)
      {
        cbf_failnez (cbf_write_itemname (handle, category->child [count], file))

        cbf_failnez (cbf_write_character (file, '\n'))
      }


        /* Write the values */

      for (row = 0; row < category->child [first]->children; row++)
      { unsigned int xcol;
      
        for (column = first; column < last; column++) {

          if (!loop) {

            cbf_failnez (cbf_write_itemname (handle, category->child [column], file))

          }

          if (matrix) {

            column_name = (category->child [column])->name;

            if (column_name) {

              len = strlen(column_name);

               if ((len > 2 && !cbf_cistrncmp("[1]",(char *)column_name+len-3,3)) ||

                (len > 6 && !cbf_cistrncmp("[1]_esd",(char *)column_name+len-7,7))) {

                cbf_failnez (cbf_write_character (file, '\n'))

               }

            }

          }

          cbf_failnez (cbf_write_value (category->child [column], row,
                                                       file, isbuffer))
          if (!loop) {

            cbf_failnez (cbf_write_character (file, '\n'))

          }
        }

        cbf_failnez (cbf_get_filecoordinates (file, NULL, &xcol))

        if (xcol)

          cbf_failnez (cbf_write_character (file, '\n'))
      }
    }
  }


    /* Success */

  return 0;
}


  /* Write a node to a file */

int cbf_write_node (cbf_handle handle, const cbf_node *node, cbf_file *file, int isbuffer)
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

      cbf_failnez (cbf_write_string (file, "###" CBF_DIC_VERSION "\n"))

      if (file->write_encoding & ENC_NONE)

        cbf_failnez (cbf_write_string (file,
                             "# CBF file written by " CBF_API_VERSION "\n"))
      else

        cbf_failnez (cbf_write_string (file,
                             "# CIF file written by " CBF_API_VERSION "\n"))

      break;

    case CBF_DATABLOCK:

      cbf_failnez (cbf_write_datablockname (node, file))

      break;

    case CBF_CATEGORY:

      cbf_failnez (cbf_write_category (handle, node, file, isbuffer))

      break;

    case CBF_SAVEFRAME:

      cbf_failnez (cbf_write_saveframename (node, file))

      break;


    default:

      return CBF_ARGUMENT;
  }


    /* Write the children */

  if (node->type == CBF_ROOT || node->type == CBF_DATABLOCK || node->type == CBF_SAVEFRAME)

    for (count = 0; count < node->children; count++)

      cbf_failnez (cbf_write_node (handle, node->child [count], file, isbuffer))

  if (node->type == CBF_SAVEFRAME )

      cbf_failnez (cbf_write_string (file, "\nsave_\n"))

    /* Flush the buffers */

  return cbf_flush_characters (file);



}


#ifdef __cplusplus

}

#endif
