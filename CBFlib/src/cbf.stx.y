%{

/**********************************************************************
 * cbf.stx -- cbf parser                                              *
 *                                                                    *
 * Version 0.7.7 19 February 2007                                     *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cbf.h"
#include "cbf_tree.h"
#include "cbf_alloc.h"
#include "cbf_context.h"

#define yyparse       cbf_parse
#define yylex         cbf_lex_wrapper
#define yyerror(x)    cbf_syntax_error(((cbf_handle)(((void **)context)[2])),(x))
#define YYLEX_PARAM   context
#define YYPARSE_PARAM context
typedef union
{
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} YYSTYPE;
#define YYSTYPE_IS_DECLARED


#ifdef alloca
#undef alloca
#endif

#define alloca(x) (NULL)

#define YYINITDEPTH 200
#define YYMAXDEPTH  200

int cbf_lex (cbf_handle handle, YYSTYPE *val ); 

int cbf_lex_wrapper (void *val, void *vcontext)
{
  int token;

  do {

    token = cbf_lex ((cbf_handle)((void **) vcontext) [2], (YYSTYPE *)val);
    
    if ( token == COMMENT && ((YYSTYPE *)val)->text ) {

      cbf_free_text(&(((YYSTYPE *)val)->text),NULL);

    }

  } while (token == COMMENT);

  return token;
}

int cbf_syntax_error (cbf_handle handle, const char *message)
{

  cbf_log( handle, message, CBF_LOGERROR|CBF_LOGSTARTLOC );
  return 0;
}

%}

%union
{
  int          errorcode;
  const char  *text;
  cbf_node    *node;
}

%token <text> DATA
%token <text> SAVE
%token        SAVEEND
%token        LOOP
%token <text> ITEM
%token <text> CATEGORY
%token <text> COLUMN
%token <text> STRING
%token <text> CBFWORD
%token <text> BINARY
%token <text> UNKNOWN
%token <text> COMMENT
%token <errorcode> ERROR

%type  <node> cbf
%type  <node> cbfstart
%type  <node> CbfThruDBName
%type  <node> ErrorCbfWODBName
%type  <node> CbfThruDBElement
%type  <node> CbfThruSaveFrame
%type  <node> CbfThruCategory
%type  <node> CbfThruColumn
%type  <node> CbfThruAssignment
%type  <node> ErrorCbfThruExtraValue
%type  <node> CbfThruLoopStart
%type  <node> CbfThruLoopCategory
%type  <node> CbfThruLoopColumn
%type  <node> CbfThruLoopAssignment
%type  <node> CbfThruSFElement
%type  <node> CbfThruSFCategory
%type  <node> CbfThruSFColumn
%type  <node> CbfThruSFAssignment
%type  <node> ErrorCbfThruExtraSFValue
%type  <node> CbfThruSFLoopStart
%type  <node> CbfThruSFLoopCategory
%type  <node> CbfThruSFLoopColumn
%type  <node> CbfThruSFLoopAssignment
%type  <text> DataBlockName
%type  <text> SaveFrameName
%type  <text> CategoryName
%type  <text> ColumnName
%type  <text> ItemName
%type  <text> Value

%pure_parser
%no_lines
%expect 0

%%

cbf:              cbfstart                      {
                                                  $$ = $1;  ((void **)context)[3] = NULL;
                                                }
                | CbfThruDBElement              {
                
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))
                                                  
                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_ROOT))
                                                }
                ;

cbfstart:                                       {
                                                  $$ = ((void **) context) [1];
                                                }
                ;

CbfThruDBName:   cbf DataBlockName             {
                
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_DATABLOCK,
                                                                                                                  (cbf_node *) NULL))
             
                                                  if (strlen($2)==0) {
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"empty data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  
                                                  if (!cbf_find_last_child(&($$),$1,$2) ){
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"duplicate data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_DATABLOCK, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                }
                | CbfThruSFElement DataBlockName { 
                                                  cbf_log((cbf_handle)(((void **)context)[2]),"prior save frame not terminated",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                      
                                                  cbf_failnez (cbf_find_parent (&($$), $1, CBF_ROOT))
                
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_DATABLOCK,
                                                                                                                  (cbf_node *) NULL))
             
                                                  if (strlen($2)==0) {
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"empty data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  
                                                  if (!cbf_find_last_child(&($$),$$,$2) ){
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"duplicate data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_DATABLOCK, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                }
                ;

ErrorCbfWODBName:
                cbfstart                       {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_DATABLOCK, NULL))

                                                  cbf_log((cbf_handle)(((void **)context)[2]),"no data block",
                                                    CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                 
                                                }
                ;


CbfThruDBElement: CbfThruDBName                 {
                                                  $$ = $1; ((void **)context)[3] = NULL;
                                                }
                | CbfThruAssignment             {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))
                                                                                                                   
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                }
                | CbfThruLoopAssignment         {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                }
                | CbfThruSaveFrame              {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                }
                | ErrorCbfThruExtraValue        {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                  NULL))
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                }
                ;


CbfThruSFElement: CbfThruDBElement SaveFrameName 
                                                {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_make_child (&($$), (cbf_node *) $1, CBF_SAVEFRAME, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                }
                | CbfThruSFElement SaveFrameName 
                                                { 
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_SAVEFRAME,
                                                                                                                   NULL))

                                                  cbf_log((cbf_handle)(((void **)context)[2]),"save frame not terminated",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                      
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_SAVEFRAME, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                }
                | ErrorCbfWODBName SaveFrameName 
                                                {

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_SAVEFRAME, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                }
                | CbfThruSFAssignment           {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))
                                                                                                                   
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))
                                                }
                | CbfThruSFLoopAssignment       {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))
                                                }
                | ErrorCbfThruExtraSFValue      {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))

                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))
                                                }
                ;

CbfThruSaveFrame: CbfThruSFElement SAVEEND      {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $1, CBF_SAVEFRAME,
                                                                                                                   NULL))

                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))


                                                }
                ;

CbfThruCategory: CbfThruDBElement CategoryName  {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                  
                                                }
                | ErrorCbfWODBName CategoryName  
                                                {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                  
                                                }
                | CbfThruCategory CategoryName  { cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",
                                                    CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                    
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                
                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                  
                                                }
                | CbfThruColumn  CategoryName   { cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",
                                                    CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                    
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                
                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                  
                                                }
                ;

CbfThruColumn:    CbfThruCategory ColumnName    {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_COLUMN, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                  
                                                }
                | CbfThruColumn ItemName        { cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))

                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_DATABLOCK))
                                                  
                                                  cbf_failnez (cbf_make_new_child (&($$), $$, CBF_CATEGORY, $2))                                                 
                                                   
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                  
                                                }
                | CbfThruDBElement ItemName     {
                                                  cbf_failnez (cbf_make_new_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                }
                | ErrorCbfWODBName ItemName     {
                                                  cbf_failnez (cbf_make_new_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                }

                ;

CbfThruAssignment:
                  CbfThruColumn Value           {
                                                  $$ = $1;

                                                  cbf_failnez (cbf_set_columnrow ($$, 0, $2, 1))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $2, CBF_VALUE,
                                                                                                                  (cbf_node *) $$))
                                                }

                ;

ErrorCbfThruExtraValue:
                 CbfThruAssignment Value        {
                                                  $$ = $1;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez(cbf_free_text(&($2),NULL))

                                                }
                                                
                | ErrorCbfThruExtraValue Value
                                                {
                                                  $$ = $1;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&($2),NULL))

                                                }
                | CbfThruLoopStart Value
                                                {
                                                  $$ = $1;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"loop value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&($2),NULL))

                                                }
                ;


CbfThruLoopStart: CbfThruDBElement Loop         {
                                                  cbf_failnez (cbf_make_node (&($$), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ($$, $1))
                                                }
                | ErrorCbfWODBName Loop       {
                                                  cbf_failnez (cbf_make_node (&($$), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ($$, $1))
                                                }
                | CbfThruLoopStart Loop       {
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"redundant \"loop_\" ",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_make_node (&($$), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ($$, $1))
                                                }

                ;

CbfThruLoopCategory:
                  CbfThruLoopStart CategoryName {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_set_link ($1, $$))
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  $$ = $1;

                                                }
                | CbfThruLoopColumn CategoryName {
                                                  cbf_failnez (cbf_find_parent (&($$), $1, CBF_DATABLOCK))

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_CATEGORY, $2))

                                                  cbf_failnez (cbf_set_link ($1, $$))
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  $$ = $1;
                                                }
                ;

CbfThruLoopColumn:
                  CbfThruLoopStart ItemName     {
                                                  cbf_failnez (cbf_make_new_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ($1, $$))

                                                  cbf_failnez (cbf_add_link ($1, $$))

                                                  $$ = $1;
                                                }
                | CbfThruLoopColumn ItemName    {
                                                  $$ = ((void **)context)[3];

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ($1, $$))

                                                  cbf_failnez (cbf_add_link ($1, $$))

                                                  $$ = $1;
                                                }
                | CbfThruLoopCategory ColumnName
                                                {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_COLUMN, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ($1, $$))

                                                  cbf_failnez (cbf_add_link ($1, $$))

                                                  $$ = $1;
                                                }
                ;

CbfThruLoopAssignment:  
                 CbfThruLoopColumn Value       {
                                                  $$ = $1;

                                                  cbf_failnez (cbf_shift_link ($$))

                                                  cbf_failnez (cbf_add_columnrow ($$, $2))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $2, CBF_VALUE,
                                                                                                                  (cbf_node *) $$))
                                                }
                | CbfThruLoopAssignment Value          {
                                                  $$ = $1;

                                                  cbf_failnez (cbf_shift_link ($$))

                                                  cbf_failnez (cbf_add_columnrow ($$, $2))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $2, CBF_VALUE,
                                                                                                                  (cbf_node *) $$))
                                                }
                ;

CbfThruSFCategory:  
                  CbfThruSFElement CategoryName {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                }
                | CbfThruSFCategory CategoryName   
                                                {  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))
                                                  
                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))
                                                  
                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                }

                | CbfThruSFColumn CategoryName  {  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))
                                                  
                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))
                                                  
                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  ((void **)context)[3] = (void *)$$;
                                                }
                 ;

CbfThruSFColumn: CbfThruSFCategory ColumnName   {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_COLUMN, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                }
                | CbfThruSFElement ItemName     {
                                                  cbf_failnez (cbf_make_new_child (&($$), $1, CBF_CATEGORY, $2))
                                                                                                    
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                }
                  
                |  CbfThruSFColumn ItemName
                                                {
                                                  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  $$ = $1; cbf_failnez (cbf_undo_links (&($$)))
                                                  
                                                  cbf_failnez (cbf_find_parent (&($$), $$, CBF_SAVEFRAME))
                                                   
                                                  cbf_failnez (cbf_make_new_child (&($$), $$, CBF_CATEGORY, $2))
                                                                                                    
                                                  ((void **)context)[3] = (void *)$$;

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                }
               ;

CbfThruSFAssignment: 
                  CbfThruSFColumn Value        {
                                                  $$ = $1;

                                                  cbf_failnez (cbf_set_columnrow ($$, 0, $2, 1))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $2, CBF_VALUE,
                                                                                                                  (cbf_node *) $$))
                                                }                                                
                ;


ErrorCbfThruExtraSFValue:

                 CbfThruSFAssignment Value        {
                                                  $$ = $1;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&($2), NULL))

                                                }
                                                
                | ErrorCbfThruExtraSFValue Value
                                                {
                                                  $$ = $1;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&($2), NULL))
                                                }
                | CbfThruSFLoopStart Value
                                                {
                                                  $$ = $1;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"loop value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&($2), NULL))

                                                }
                ;


CbfThruSFLoopStart:  
                  CbfThruSFElement Loop         {
                                                  cbf_failnez (cbf_make_node (&($$), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ($$, $1))
                                                }
                | CbfThruSFLoopStart Loop       {
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"redundant \"loop_\" ",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_make_node (&($$), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ($$, $1))
                                                }
                ;

CbfThruSFLoopCategory:
                  CbfThruSFLoopStart CategoryName  {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;

                                                  cbf_failnez (cbf_set_link ($1, $$))
                                                  
                                                  ((void **)context)[3] = (void *)$$;

                                                  $$ = $1;
                                                }
                | CbfThruSFLoopColumn CategoryName {
                                                  cbf_failnez (cbf_find_parent (&($$), $1, CBF_SAVEFRAME))

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_CATEGORY, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;

                                                  cbf_failnez (cbf_set_link ($1, $$))
                                                 
                                                  ((void **)context)[3] = (void *)$$;

                                                  $$ = $1;
                                                }
                ;

CbfThruSFLoopColumn:
                  CbfThruSFLoopStart ItemName     {
                                                  cbf_failnez (cbf_make_new_child (&($$), $1, CBF_CATEGORY, $2))
                                                  
                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, cbf_copy_string(NULL,$2,0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ($1, $$))

                                                  cbf_failnez (cbf_add_link ($1, $$))

                                                  $$ = $1;
                                                }
                | CbfThruSFLoopColumn ItemName  {
                                                  $$ = ((void **)context)[3];

                                                  cbf_failnez (cbf_make_child (&($$), $$, CBF_COLUMN, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ($1, $$))

                                                  cbf_failnez (cbf_add_link ($1, $$))

                                                  $$ = $1;
                                                }
                | CbfThruSFLoopCategory ColumnName       {
                                                  cbf_failnez (cbf_make_child (&($$), $1, CBF_COLUMN, $2))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)$$;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $$, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ($1, $$))

                                                  cbf_failnez (cbf_add_link ($1, $$))

                                                  $$ = $1;
                                                }
                ;

CbfThruSFLoopAssignment:
                  CbfThruSFLoopColumn Value       {
                                                  $$ = $1;

                                                  cbf_failnez (cbf_shift_link ($$))

                                                  cbf_failnez (cbf_add_columnrow ($$, $2))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $2, CBF_VALUE,
                                                                                                                  (cbf_node *) $$))
                                                }
                | CbfThruSFLoopAssignment Value    {
                                                  $$ = $1;

                                                  cbf_failnez (cbf_shift_link ($$))

                                                  cbf_failnez (cbf_add_columnrow ($$, $2))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) $2, CBF_VALUE,
                                                                                                                  (cbf_node *) $$))
                                                }
                ;

Loop:             LOOP
                ;

DataBlockName:    DATA                          {
                                                  $$ = $1;
                                                }
                ;

SaveFrameName:    SAVE                          {
                                                  $$ = $1;
                                                }
                ;
CategoryName:     CATEGORY                      {
                                                  $$ = $1;
                                                }
                ;

ColumnName:       COLUMN                        {
                                                  $$ = $1;
                                                }
                ;

ItemName:         ITEM                          {
                                                  $$ = $1;
                                                }
                ;

Value:            STRING                        {
                                                  $$ = $1;
                                                }
                | CBFWORD                          {
                                                  $$ = $1;
                                                }
                | BINARY                        {
                                                  $$ = $1;
                                                }
                ;

%%

#ifdef __cplusplus

}

#endif
