
/*  A Bison parser, made from ./src/cbf.stx.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	DATA	257
#define	SAVE	258
#define	SAVEEND	259
#define	LOOP	260
#define	ITEM	261
#define	CATEGORY	262
#define	COLUMN	263
#define	STRING	264
#define	CBFWORD	265
#define	BINARY	266
#define	UNKNOWN	267
#define	COMMENT	268
#define	ERROR	269

#line 1 "./src/cbf.stx.y"


/**********************************************************************
 * cbf.stx -- cbf parser                                              *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cbf.h"
#include "cbf_tree.h"

#define yyparse       cbf_parse
#define yylex         cbf_lex_wrapper
#define yyerror(x)    cbf_syntax_error(((cbf_handle)(((void **)context)[2])),(x))
#define YYLEX_PARAM   context
#define YYPARSE_PARAM context


#ifdef alloca
#undef alloca
#endif

#define alloca(x) (NULL)

#define YYINITDEPTH 200
#define YYMAXDEPTH  200

int cbf_lex (cbf_handle handle, union {
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} *val);

int cbf_lex_wrapper (void *val, void *context)
{
  int token;

  do

    token = cbf_lex ((cbf_handle)((void **) context) [2], val);

  while (token == COMMENT);

  return token;
}

int cbf_syntax_error (cbf_handle handle, const char *message)
{

  cbf_log( handle, message, CBF_LOGERROR|CBF_LOGSTARTLOC );
  return 0;
}


#line 309 "./src/cbf.stx.y"
typedef union
{
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		81
#define	YYFLAG		-32768
#define	YYNTBASE	16

#define YYTRANSLATE(x) ((unsigned)(x) <= 269 ? yytranslate[x] : 46)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     5,     8,    11,    13,    15,    17,    19,
    21,    23,    26,    29,    32,    34,    36,    38,    41,    44,
    47,    50,    53,    56,    59,    62,    65,    68,    71,    74,
    77,    80,    83,    86,    89,    92,    95,    98,   101,   104,
   107,   110,   113,   116,   119,   122,   125,   128,   131,   134,
   137,   140,   143,   146,   149,   152,   155,   158,   161,   164,
   166,   168,   170,   172,   174,   176,   178,   180
};

static const short yyrhs[] = {    17,
     0,    20,     0,     0,    16,    40,     0,    21,    40,     0,
    17,     0,    18,     0,    25,     0,    30,     0,    22,     0,
    26,     0,    20,    41,     0,    21,    41,     0,    19,    41,
     0,    33,     0,    38,     0,    34,     0,    21,     5,     0,
    20,    42,     0,    19,    42,     0,    23,    42,     0,    24,
    42,     0,    23,    43,     0,    24,    44,     0,    20,    44,
     0,    19,    44,     0,    24,    45,     0,    25,    45,     0,
    26,    45,     0,    27,    45,     0,    20,    39,     0,    19,
    39,     0,    27,    39,     0,    27,    42,     0,    29,    42,
     0,    27,    44,     0,    29,    44,     0,    28,    43,     0,
    29,    45,     0,    30,    45,     0,    21,    42,     0,    31,
    42,     0,    32,    42,     0,    31,    43,     0,    21,    44,
     0,    32,    44,     0,    32,    45,     0,    33,    45,     0,
    34,    45,     0,    35,    45,     0,    21,    39,     0,    35,
    39,     0,    35,    42,     0,    37,    42,     0,    35,    44,
     0,    37,    44,     0,    36,    43,     0,    37,    45,     0,
    38,    45,     0,     6,     0,     3,     0,     4,     0,     8,
     0,     9,     0,     7,     0,    10,     0,    11,     0,    12,
     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   366,   369,   374,   379,   403,   433,   444,   447,   457,   467,
   477,   490,   500,   513,   523,   529,   535,   543,   551,   559,
   568,   580,   594,   603,   619,   631,   646,   658,   666,   673,
   683,   688,   693,   703,   718,   731,   750,   766,   783,   794,
   806,   814,   826,   838,   846,   859,   878,   890,   899,   906,
   916,   922,   931,   943,   958,   975,   991,  1007,  1018,  1030,
  1033,  1038,  1042,  1047,  1052,  1057,  1060,  1063
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","DATA","SAVE",
"SAVEEND","LOOP","ITEM","CATEGORY","COLUMN","STRING","CBFWORD","BINARY","UNKNOWN",
"COMMENT","ERROR","cbf","cbfstart","CbfThruDBName","ErrorCbfWODBName","CbfThruDBElement",
"CbfThruSFElement","CbfThruSaveFrame","CbfThruCategory","CbfThruColumn","CbfThruAssignment",
"ErrorCbfThruExtraValue","CbfThruLoopStart","CbfThruLoopCategory","CbfThruLoopColumn",
"CbfThruLoopAssignment","CbfThruSFCategory","CbfThruSFColumn","CbfThruSFAssignment",
"ErrorCbfThruExtraSFValue","CbfThruSFLoopStart","CbfThruSFLoopCategory","CbfThruSFLoopColumn",
"CbfThruSFLoopAssignment","Loop","DataBlockName","SaveFrameName","CategoryName",
"ColumnName","ItemName","Value", NULL
};
#endif

static const short yyr1[] = {     0,
    16,    16,    17,    18,    18,    19,    20,    20,    20,    20,
    20,    21,    21,    21,    21,    21,    21,    22,    23,    23,
    23,    23,    24,    24,    24,    24,    25,    26,    26,    26,
    27,    27,    27,    28,    28,    29,    29,    29,    30,    30,
    31,    31,    31,    32,    32,    32,    33,    34,    34,    34,
    35,    35,    36,    36,    37,    37,    37,    38,    38,    39,
    40,    41,    42,    43,    44,    45,    45,    45
};

static const short yyr2[] = {     0,
     1,     1,     0,     2,     2,     1,     1,     1,     1,     1,
     1,     2,     2,     2,     1,     1,     1,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
     1,     1,     1,     1,     1,     1,     1,     1
};

static const short yydefact[] = {     3,
     0,     6,     7,     0,     2,     0,    10,     0,     0,     8,
    11,     0,     0,     0,     9,     0,     0,    15,    17,     0,
     0,     0,    16,    61,     4,    62,    60,    65,    63,    32,
    14,    20,    26,    31,    12,    19,    25,    18,    51,     5,
    13,    41,    45,    64,    21,    23,    66,    67,    68,    22,
    24,    27,    28,    29,    33,    34,    36,    30,    38,    35,
    37,    39,    40,    42,    44,    43,    46,    47,    48,    49,
    52,    53,    55,    50,    57,    54,    56,    58,    59,     0,
     0
};

static const short yydefgoto[] = {     1,
     2,     3,     4,     5,     6,     7,     8,     9,    10,    11,
    12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
    22,    23,    30,    25,    31,    32,    46,    33,    52
};

static const short yypact[] = {-32768,
    21,    23,-32768,    65,    65,    54,-32768,    69,    56,    64,
    64,    44,    -3,    56,    64,    69,    56,    64,    64,    44,
    -3,    56,    64,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,    16,
-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,     8,     2,    74,    -5,   -11,    13,    26
};


#define	YYLAST		80


static const short yytable[] = {    36,
    42,    59,    45,    50,    65,    44,    56,    40,    60,    75,
    64,    66,    34,    39,    72,    81,    76,    37,    43,    55,
    80,    51,    -1,    24,    57,    -1,    61,    71,     0,    67,
     0,     0,    73,     0,    77,    53,    54,    58,     0,    62,
    63,     0,    68,    69,    70,    74,     0,    78,    79,    27,
    28,    29,     0,    47,    48,    49,    24,    26,    38,    27,
    28,    29,    28,    29,     0,    47,    48,    49,    26,     0,
    27,    28,    29,    47,    48,    49,    29,    44,    35,    41
};

static const short yycheck[] = {     5,
     6,    13,     8,     9,    16,     9,    12,     6,    14,    21,
    16,    17,     5,     6,    20,     0,    22,     5,     6,    12,
     0,     9,     0,     3,    12,     3,    14,    20,    -1,    17,
    -1,    -1,    20,    -1,    22,    10,    11,    12,    -1,    14,
    15,    -1,    17,    18,    19,    20,    -1,    22,    23,     6,
     7,     8,    -1,    10,    11,    12,     3,     4,     5,     6,
     7,     8,     7,     8,    -1,    10,    11,    12,     4,    -1,
     6,     7,     8,    10,    11,    12,     8,     9,     5,     6
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */

/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif



/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
{
                                                  yyval.node = yyvsp[0].node;  ((void **)context)[3] = NULL;
                                                ;
    break;}
case 2:
{
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_ROOT))
                                                ;
    break;}
case 3:
{
                                                  yyval.node = ((void **) context) [1];
                                                ;
    break;}
case 4:
{
                
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[-1].node, CBF_DATABLOCK,
                                                                                                                  (cbf_node *) NULL))
             
                                                  if (strlen(yyvsp[0].text)==0) {
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"empty data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  
                                                  if (!cbf_find_last_child(&(yyval.node),yyvsp[-1].node,yyvsp[0].text) ){
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"duplicate data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                ;
    break;}
case 5:
{ 
                                                  cbf_log((cbf_handle)(((void **)context)[2]),"prior save frame not terminated",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                      
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_ROOT))
                
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[-1].node, CBF_DATABLOCK,
                                                                                                                  (cbf_node *) NULL))
             
                                                  if (strlen(yyvsp[0].text)==0) {
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"empty data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  
                                                  if (!cbf_find_last_child(&(yyval.node),yyval.node,yyvsp[0].text) ){
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"duplicate data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                ;
    break;}
case 6:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[0].node, CBF_DATABLOCK, NULL))

                                                  cbf_log((cbf_handle)(((void **)context)[2]),"no data block",
                                                    CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                 
                                                ;
    break;}
case 7:
{
                                                  yyval.node = yyvsp[0].node; ((void **)context)[3] = NULL;
                                                ;
    break;}
case 8:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;
    break;}
case 9:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;
    break;}
case 10:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;
    break;}
case 11:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                  NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;
    break;}
case 12:
{

                                                  cbf_failnez (cbf_make_child (&(yyval.node), (cbf_node *) yyvsp[-1].node, CBF_SAVEFRAME, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                ;
    break;}
case 13:
{ cbf_log((cbf_handle)(((void **)context)[2]),"save frame not terminated",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                      
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK))

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_SAVEFRAME, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                ;
    break;}
case 14:
{

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_SAVEFRAME, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                ;
    break;}
case 15:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_SAVEFRAME))
                                                ;
    break;}
case 16:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_SAVEFRAME))
                                                ;
    break;}
case 17:
{
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].node, CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[0].node, CBF_SAVEFRAME))
                                                ;
    break;}
case 18:
{

                                                  cbf_failnez (cbf_find_parent ( &(yyval.node), yyvsp[-1].node, CBF_SAVEFRAME ))


                                                ;
    break;}
case 19:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                  
                                                ;
    break;}
case 20:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                  
                                                ;
    break;}
case 21:
{ cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",
                                                    CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK))
                
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                  
                                                ;
    break;}
case 22:
{ cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",
                                                    CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK))
                
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                  
                                                ;
    break;}
case 23:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                  
                                                ;
    break;}
case 24:
{ cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK))
                                                  
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))                                                 
                                                   
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                  
                                                ;
    break;}
case 25:
{
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;
    break;}
case 26:
{
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;
    break;}
case 27:
{
                                                  yyval.node = yyvsp[-1].node;

                                                  cbf_failnez (cbf_set_columnrow (yyval.node, 0, yyvsp[0].text, 1))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].text, CBF_VALUE,
                                                                                                                  (cbf_node *) yyval.node))
                                                ;
    break;}
case 28:
{
                                                  yyval.node = yyvsp[-1].node;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                ;
    break;}
case 29:
{
                                                  yyval.node = yyvsp[-1].node;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                ;
    break;}
case 30:
{
                                                  yyval.node = yyvsp[-1].node;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"loop value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                ;
    break;}
case 31:
{
                                                  cbf_failnez (cbf_make_node (&(yyval.node), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link (yyval.node, yyvsp[-1].node))
                                                ;
    break;}
case 32:
{
                                                  cbf_failnez (cbf_make_node (&(yyval.node), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link (yyval.node, yyvsp[-1].node))
                                                ;
    break;}
case 33:
{
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"redundant \"loop_\" ",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_make_node (&(yyval.node), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link (yyval.node, yyvsp[-1].node))
                                                ;
    break;}
case 34:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  yyval.node = yyvsp[-1].node;

                                                ;
    break;}
case 35:
{
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_DATABLOCK))

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 36:
{
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))

                                                  cbf_failnez (cbf_add_link (yyvsp[-1].node, yyval.node))

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 37:
{
                                                  yyval.node = ((void **)context)[3];

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))

                                                  cbf_failnez (cbf_add_link (yyvsp[-1].node, yyval.node))

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 38:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))

                                                  cbf_failnez (cbf_add_link (yyvsp[-1].node, yyval.node))

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 39:
{
                                                  yyval.node = yyvsp[-1].node;

                                                  cbf_failnez (cbf_shift_link (yyval.node))

                                                  cbf_failnez (cbf_add_columnrow (yyval.node, yyvsp[0].text))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].text, CBF_VALUE,
                                                                                                                  (cbf_node *) yyval.node))
                                                ;
    break;}
case 40:
{
                                                  yyval.node = yyvsp[-1].node;

                                                  cbf_failnez (cbf_shift_link (yyval.node))

                                                  cbf_failnez (cbf_add_columnrow (yyval.node, yyvsp[0].text))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].text, CBF_VALUE,
                                                                                                                  (cbf_node *) yyval.node))
                                                ;
    break;}
case 41:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                ;
    break;}
case 42:
{  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_SAVEFRAME))
                                                  
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                ;
    break;}
case 43:
{  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_SAVEFRAME))
                                                  
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;
                                                ;
    break;}
case 44:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;
    break;}
case 45:
{
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                                                                    
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;
    break;}
case 46:
{
                                                  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_SAVEFRAME))
                                                   
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))
                                                                                                    
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;
    break;}
case 47:
{
                                                  yyval.node = yyvsp[-1].node;

                                                  cbf_failnez (cbf_set_columnrow (yyval.node, 0, yyvsp[0].text, 1))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].text, CBF_VALUE,
                                                                                                                  (cbf_node *) yyval.node))
                                                ;
    break;}
case 48:
{
                                                  yyval.node = yyvsp[-1].node;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                ;
    break;}
case 49:
{
                                                  yyval.node = yyvsp[-1].node;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                ;
    break;}
case 50:
{
                                                  yyval.node = yyvsp[-1].node;
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"loop value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                ;
    break;}
case 51:
{
                                                  cbf_failnez (cbf_make_node (&(yyval.node), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link (yyval.node, yyvsp[-1].node))
                                                ;
    break;}
case 52:
{
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"redundant \"loop_\" ",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_make_node (&(yyval.node), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link (yyval.node, yyvsp[-1].node))
                                                ;
    break;}
case 53:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))
                                                  
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 54:
{
                                                  cbf_failnez (cbf_find_parent (&(yyval.node), yyvsp[-1].node, CBF_SAVEFRAME))

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))
                                                 
                                                  ((void **)context)[3] = (void *)yyval.node;

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 55:
{
                                                  cbf_failnez (cbf_make_new_child (&(yyval.node), yyvsp[-1].node, CBF_CATEGORY, yyvsp[0].text))
                                                  
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))

                                                  cbf_failnez (cbf_add_link (yyvsp[-1].node, yyval.node))

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 56:
{
                                                  yyval.node = ((void **)context)[3];

                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyval.node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))

                                                  cbf_failnez (cbf_add_link (yyvsp[-1].node, yyval.node))

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 57:
{
                                                  cbf_failnez (cbf_make_child (&(yyval.node), yyvsp[-1].node, CBF_COLUMN, yyvsp[0].text))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)yyval.node;
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyval.node, CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link (yyvsp[-1].node, yyval.node))

                                                  cbf_failnez (cbf_add_link (yyvsp[-1].node, yyval.node))

                                                  yyval.node = yyvsp[-1].node;
                                                ;
    break;}
case 58:
{
                                                  yyval.node = yyvsp[-1].node;

                                                  cbf_failnez (cbf_shift_link (yyval.node))

                                                  cbf_failnez (cbf_add_columnrow (yyval.node, yyvsp[0].text))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].text, CBF_VALUE,
                                                                                                                  (cbf_node *) yyval.node))
                                                ;
    break;}
case 59:
{
                                                  yyval.node = yyvsp[-1].node;

                                                  cbf_failnez (cbf_shift_link (yyval.node))

                                                  cbf_failnez (cbf_add_columnrow (yyval.node, yyvsp[0].text))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) yyvsp[0].text, CBF_VALUE,
                                                                                                                  (cbf_node *) yyval.node))
                                                ;
    break;}
case 61:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 62:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 63:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 64:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 65:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 66:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 67:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
case 68:
{
                                                  yyval.text = yyvsp[0].text;
                                                ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */


  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}


#ifdef __cplusplus

}

#endif
