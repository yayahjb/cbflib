/* A Bison parser, made by GNU Bison 2.0.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     DATA = 258,
     SAVE = 259,
     SAVEEND = 260,
     LOOP = 261,
     ITEM = 262,
     CATEGORY = 263,
     COLUMN = 264,
     STRING = 265,
     CBFWORD = 266,
     BINARY = 267,
     UNKNOWN = 268,
     COMMENT = 269,
     ERROR = 270
   };
#endif
#define DATA 258
#define SAVE 259
#define SAVEEND 260
#define LOOP 261
#define ITEM 262
#define CATEGORY 263
#define COLUMN 264
#define STRING 265
#define CBFWORD 266
#define BINARY 267
#define UNKNOWN 268
#define COMMENT 269
#define ERROR 270




/* Copy the first part of user declarations.  */



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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)

typedef union YYSTYPE {
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} YYSTYPE;
/* Line 190 of yacc.c.  */

# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 213 of yacc.c.  */


#if ! defined (yyoverflow) || YYERROR_VERBOSE

# ifndef YYFREE
#  define YYFREE free
# endif
# ifndef YYMALLOC
#  define YYMALLOC malloc
# endif

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   else
#    define YYSTACK_ALLOC alloca
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
# endif
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short int yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short int) + sizeof (YYSTYPE))			\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short int yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  24
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   80

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  16
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  31
/* YYNRULES -- Number of rules. */
#define YYNRULES  69
/* YYNRULES -- Number of states. */
#define YYNSTATES  81

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   270

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned char yyprhs[] =
{
       0,     0,     3,     5,     7,     8,    11,    14,    16,    18,
      20,    22,    24,    26,    29,    32,    35,    37,    39,    41,
      44,    47,    50,    53,    56,    59,    62,    65,    68,    71,
      74,    77,    80,    83,    86,    89,    92,    95,    98,   101,
     104,   107,   110,   113,   116,   119,   122,   125,   128,   131,
     134,   137,   140,   143,   146,   149,   152,   155,   158,   161,
     164,   167,   169,   171,   173,   175,   177,   179,   181,   183
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      17,     0,    -1,    18,    -1,    21,    -1,    -1,    17,    41,
      -1,    22,    41,    -1,    18,    -1,    19,    -1,    26,    -1,
      31,    -1,    23,    -1,    27,    -1,    21,    42,    -1,    22,
      42,    -1,    20,    42,    -1,    34,    -1,    39,    -1,    35,
      -1,    22,     5,    -1,    21,    43,    -1,    20,    43,    -1,
      24,    43,    -1,    25,    43,    -1,    24,    44,    -1,    25,
      45,    -1,    21,    45,    -1,    20,    45,    -1,    25,    46,
      -1,    26,    46,    -1,    27,    46,    -1,    28,    46,    -1,
      21,    40,    -1,    20,    40,    -1,    28,    40,    -1,    28,
      43,    -1,    30,    43,    -1,    28,    45,    -1,    30,    45,
      -1,    29,    44,    -1,    30,    46,    -1,    31,    46,    -1,
      22,    43,    -1,    32,    43,    -1,    33,    43,    -1,    32,
      44,    -1,    22,    45,    -1,    33,    45,    -1,    33,    46,
      -1,    34,    46,    -1,    35,    46,    -1,    36,    46,    -1,
      22,    40,    -1,    36,    40,    -1,    36,    43,    -1,    38,
      43,    -1,    36,    45,    -1,    38,    45,    -1,    37,    44,
      -1,    38,    46,    -1,    39,    46,    -1,     6,    -1,     3,
      -1,     4,    -1,     8,    -1,     9,    -1,     7,    -1,    10,
      -1,    11,    -1,    12,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
       0,   377,   377,   380,   388,   393,   417,   448,   458,   461,
     473,   485,   496,   510,   522,   544,   557,   565,   573,   583,
     598,   606,   615,   629,   645,   654,   672,   684,   700,   712,
     721,   730,   742,   747,   752,   763,   777,   791,   809,   825,
     843,   853,   866,   873,   887,   901,   909,   922,   944,   957,
     966,   974,   987,   992,  1002,  1013,  1029,  1045,  1061,  1078,
    1088,  1100,  1103,  1108,  1112,  1117,  1122,  1127,  1130,  1133
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DATA", "SAVE", "SAVEEND", "LOOP",
  "ITEM", "CATEGORY", "COLUMN", "STRING", "CBFWORD", "BINARY", "UNKNOWN",
  "COMMENT", "ERROR", "$accept", "cbf", "cbfstart", "CbfThruDBName",
  "ErrorCbfWODBName", "CbfThruDBElement", "CbfThruSFElement",
  "CbfThruSaveFrame", "CbfThruCategory", "CbfThruColumn",
  "CbfThruAssignment", "ErrorCbfThruExtraValue", "CbfThruLoopStart",
  "CbfThruLoopCategory", "CbfThruLoopColumn", "CbfThruLoopAssignment",
  "CbfThruSFCategory", "CbfThruSFColumn", "CbfThruSFAssignment",
  "ErrorCbfThruExtraSFValue", "CbfThruSFLoopStart",
  "CbfThruSFLoopCategory", "CbfThruSFLoopColumn",
  "CbfThruSFLoopAssignment", "Loop", "DataBlockName", "SaveFrameName",
  "CategoryName", "ColumnName", "ItemName", "Value", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short int yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    16,    17,    17,    18,    19,    19,    20,    21,    21,
      21,    21,    21,    22,    22,    22,    22,    22,    22,    23,
      24,    24,    24,    24,    25,    25,    25,    25,    26,    27,
      27,    27,    28,    28,    28,    29,    29,    30,    30,    30,
      31,    31,    32,    32,    32,    33,    33,    33,    34,    35,
      35,    35,    36,    36,    37,    37,    38,    38,    38,    39,
      39,    40,    41,    42,    43,    44,    45,    46,    46,    46
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     1,     0,     2,     2,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     1,     1,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       4,     0,     7,     8,     0,     3,     0,    11,     0,     0,
       9,    12,     0,     0,     0,    10,     0,     0,    16,    18,
       0,     0,     0,    17,     1,    62,     5,    63,    61,    66,
      64,    33,    15,    21,    27,    32,    13,    20,    26,    19,
      52,     6,    14,    42,    46,    65,    22,    24,    67,    68,
      69,    23,    25,    28,    29,    30,    34,    35,    37,    31,
      39,    36,    38,    40,    41,    43,    45,    44,    47,    48,
      49,    50,    53,    54,    56,    51,    58,    55,    57,    59,
      60
};

/* YYDEFGOTO[NTERM-NUM]. */
static const yysigned_char yydefgoto[] =
{
      -1,     1,     2,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    31,    26,    32,    33,    47,    34,
      53
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -12
static const yysigned_char yypact[] =
{
     -12,    21,    23,   -12,    65,    65,    54,   -12,    69,    56,
      64,    64,    44,    -3,    56,    64,    69,    56,    64,    64,
      44,    -3,    56,    64,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,   -12,
     -12,   -12,   -12,   -12,     8,     2,    74,    -5,   -11,    13,
      26
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -3
static const yysigned_char yytable[] =
{
      37,    43,    60,    46,    51,    66,    45,    57,    41,    61,
      76,    65,    67,    35,    40,    73,     0,    77,    38,    44,
      56,    24,    52,    -2,    25,    58,    -2,    62,    72,     0,
      68,     0,     0,    74,     0,    78,    54,    55,    59,     0,
      63,    64,     0,    69,    70,    71,    75,     0,    79,    80,
      28,    29,    30,     0,    48,    49,    50,    25,    27,    39,
      28,    29,    30,    29,    30,     0,    48,    49,    50,    27,
       0,    28,    29,    30,    48,    49,    50,    30,    45,    36,
      42
};

static const yysigned_char yycheck[] =
{
       5,     6,    13,     8,     9,    16,     9,    12,     6,    14,
      21,    16,    17,     5,     6,    20,    -1,    22,     5,     6,
      12,     0,     9,     0,     3,    12,     3,    14,    20,    -1,
      17,    -1,    -1,    20,    -1,    22,    10,    11,    12,    -1,
      14,    15,    -1,    17,    18,    19,    20,    -1,    22,    23,
       6,     7,     8,    -1,    10,    11,    12,     3,     4,     5,
       6,     7,     8,     7,     8,    -1,    10,    11,    12,     4,
      -1,     6,     7,     8,    10,    11,    12,     8,     9,     5,
       6
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,     3,    41,     4,     6,     7,
       8,    40,    42,    43,    45,    40,    42,    43,    45,     5,
      40,    41,    42,    43,    45,     9,    43,    44,    10,    11,
      12,    43,    45,    46,    46,    46,    40,    43,    45,    46,
      44,    43,    45,    46,    46,    43,    44,    43,    45,    46,
      46,    46,    40,    43,    45,    46,    44,    43,    45,    46,
      46
};

#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (N)								\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (0)
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
              (Loc).first_line, (Loc).first_column,	\
              (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Type, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short int *bottom, short int *top)
#else
static void
yy_stack_print (bottom, top)
    short int *bottom;
    short int *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);


# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */






/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  /* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short int yyssa[YYINITDEPTH];
  short int *yyss = yyssa;
  register short int *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;


  yyvsp[0] = yylval;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short int *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short int *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

/* Do appropriate processing given the current state.  */
/* Read a look-ahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to look-ahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

    {
                                                  (yyval.node) = (yyvsp[0].node);  ((void **)context)[3] = NULL;
                                                ;}
    break;

  case 3:

    {
                
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))
                                                  
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_ROOT))
                                                ;}
    break;

  case 4:

    {
                                                  (yyval.node) = ((void **) context) [1];
                                                ;}
    break;

  case 5:

    {
                
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_DATABLOCK,
                                                                                                                  (cbf_node *) NULL))
             
                                                  if (strlen((yyvsp[0].text))==0) {
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"empty data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  
                                                  if (!cbf_find_last_child(&((yyval.node)),(yyvsp[-1].node),(yyvsp[0].text)) ){
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"duplicate data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_DATABLOCK, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                ;}
    break;

  case 6:

    { 
                                                  cbf_log((cbf_handle)(((void **)context)[2]),"prior save frame not terminated",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                      
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyvsp[-1].node), CBF_ROOT))
                
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_DATABLOCK,
                                                                                                                  (cbf_node *) NULL))
             
                                                  if (strlen((yyvsp[0].text))==0) {
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"empty data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  
                                                  if (!cbf_find_last_child(&((yyval.node)),(yyval.node),(yyvsp[0].text)) ){
                                                  
                                                    cbf_log((cbf_handle)(((void **)context)[2]),"duplicate data block name",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                  
                                                  }

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_DATABLOCK, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                ;}
    break;

  case 7:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[0].node), CBF_DATABLOCK, NULL))

                                                  cbf_log((cbf_handle)(((void **)context)[2]),"no data block",
                                                    CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                 
                                                ;}
    break;

  case 8:

    {
                                                  (yyval.node) = (yyvsp[0].node); ((void **)context)[3] = NULL;
                                                ;}
    break;

  case 9:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                   NULL))
                                                                                                                   
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;}
    break;

  case 10:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;}
    break;

  case 11:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                   NULL))
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;}
    break;

  case 12:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                  NULL))
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                ;}
    break;

  case 13:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (cbf_node *) (yyvsp[-1].node), CBF_SAVEFRAME, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                ;}
    break;

  case 14:

    { 
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_SAVEFRAME,
                                                                                                                   NULL))

                                                  cbf_log((cbf_handle)(((void **)context)[2]),"save frame not terminated",
                                                      CBF_LOGWARNING|CBF_LOGSTARTLOC);
                                                      
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_SAVEFRAME, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                ;}
    break;

  case 15:

    {

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_SAVEFRAME, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                   
                                                  ((void **)context)[3] = NULL;
                                                  
                                                ;}
    break;

  case 16:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                   NULL))
                                                                                                                   
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))
                                                ;}
    break;

  case 17:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                   NULL))
                                                  
                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))
                                                ;}
    break;

  case 18:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].node), CBF_CATEGORY,
                                                                                                                   NULL))

                                                  (yyval.node) = (yyvsp[0].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))
                                                ;}
    break;

  case 19:

    {
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_CATEGORY,
                                                                                                                   NULL))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[-1].node), CBF_SAVEFRAME,
                                                                                                                   NULL))

                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))


                                                ;}
    break;

  case 20:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                  
                                                ;}
    break;

  case 21:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                  
                                                ;}
    break;

  case 22:

    { cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",
                                                    CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                    
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                  
                                                ;}
    break;

  case 23:

    { cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",
                                                    CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                    
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                  
                                                ;}
    break;

  case 24:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_COLUMN, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                  
                                                ;}
    break;

  case 25:

    { cbf_log ((cbf_handle)(((void **)context)[2]),"data name with no value",CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))

                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_DATABLOCK))
                                                  
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))                                                 
                                                   
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                  
                                                ;}
    break;

  case 26:

    {
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;}
    break;

  case 27:

    {
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;}
    break;

  case 28:

    {
                                                  (yyval.node) = (yyvsp[-1].node);

                                                  cbf_failnez (cbf_set_columnrow ((yyval.node), 0, (yyvsp[0].text), 1))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].text), CBF_VALUE,
                                                                                                                  (cbf_node *) (yyval.node)))
                                                ;}
    break;

  case 29:

    {
                                                  (yyval.node) = (yyvsp[-1].node);
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez(cbf_free_text(&((yyvsp[0].text)),NULL))

                                                ;}
    break;

  case 30:

    {
                                                  (yyval.node) = (yyvsp[-1].node);
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&((yyvsp[0].text)),NULL))

                                                ;}
    break;

  case 31:

    {
                                                  (yyval.node) = (yyvsp[-1].node);
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"loop value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&((yyvsp[0].text)),NULL))

                                                ;}
    break;

  case 32:

    {
                                                  cbf_failnez (cbf_make_node (&((yyval.node)), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ((yyval.node), (yyvsp[-1].node)))
                                                ;}
    break;

  case 33:

    {
                                                  cbf_failnez (cbf_make_node (&((yyval.node)), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ((yyval.node), (yyvsp[-1].node)))
                                                ;}
    break;

  case 34:

    {
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"redundant \"loop_\" ",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_make_node (&((yyval.node)), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ((yyval.node), (yyvsp[-1].node)))
                                                ;}
    break;

  case 35:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  (yyval.node) = (yyvsp[-1].node);

                                                ;}
    break;

  case 36:

    {
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyvsp[-1].node), CBF_DATABLOCK))

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 37:

    {
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))

                                                  cbf_failnez (cbf_add_link ((yyvsp[-1].node), (yyval.node)))

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 38:

    {
                                                  (yyval.node) = ((void **)context)[3];

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))

                                                  cbf_failnez (cbf_add_link ((yyvsp[-1].node), (yyval.node)))

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 39:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_COLUMN, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))

                                                  cbf_failnez (cbf_add_link ((yyvsp[-1].node), (yyval.node)))

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 40:

    {
                                                  (yyval.node) = (yyvsp[-1].node);

                                                  cbf_failnez (cbf_shift_link ((yyval.node)))

                                                  cbf_failnez (cbf_add_columnrow ((yyval.node), (yyvsp[0].text)))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].text), CBF_VALUE,
                                                                                                                  (cbf_node *) (yyval.node)))
                                                ;}
    break;

  case 41:

    {
                                                  (yyval.node) = (yyvsp[-1].node);

                                                  cbf_failnez (cbf_shift_link ((yyval.node)))

                                                  cbf_failnez (cbf_add_columnrow ((yyval.node), (yyvsp[0].text)))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].text), CBF_VALUE,
                                                                                                                  (cbf_node *) (yyval.node)))
                                                ;}
    break;

  case 42:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                ;}
    break;

  case 43:

    {  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))
                                                  
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))
                                                  
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                ;}
    break;

  case 44:

    {  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))
                                                  
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))
                                                  
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);
                                                ;}
    break;

  case 45:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_COLUMN, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;}
    break;

  case 46:

    {
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                                                                    
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;}
    break;

  case 47:

    {
                                                  cbf_log((cbf_handle)(((void **)context)[2]), "data name with no value", CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  (yyval.node) = (yyvsp[-1].node); cbf_failnez (cbf_undo_links (&((yyval.node))))
                                                  
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyval.node), CBF_SAVEFRAME))
                                                   
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))
                                                                                                    
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))
                                                ;}
    break;

  case 48:

    {
                                                  (yyval.node) = (yyvsp[-1].node);

                                                  cbf_failnez (cbf_set_columnrow ((yyval.node), 0, (yyvsp[0].text), 1))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].text), CBF_VALUE,
                                                                                                                  (cbf_node *) (yyval.node)))
                                                ;}
    break;

  case 49:

    {
                                                  (yyval.node) = (yyvsp[-1].node);
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&((yyvsp[0].text)), NULL))

                                                ;}
    break;

  case 50:

    {
                                                  (yyval.node) = (yyvsp[-1].node);
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&((yyvsp[0].text)), NULL))
                                                ;}
    break;

  case 51:

    {
                                                  (yyval.node) = (yyvsp[-1].node);
                                                  
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"loop value without tag",CBF_LOGERROR|CBF_LOGSTARTLOC);

                                                  cbf_failnez(cbf_free_text(&((yyvsp[0].text)), NULL))

                                                ;}
    break;

  case 52:

    {
                                                  cbf_failnez (cbf_make_node (&((yyval.node)), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ((yyval.node), (yyvsp[-1].node)))
                                                ;}
    break;

  case 53:

    {
                                                  cbf_log ((cbf_handle)(((void **)context)[2]),"redundant \"loop_\" ",CBF_LOGERROR|CBF_LOGSTARTLOC);
                                                  
                                                  cbf_failnez (cbf_make_node (&((yyval.node)), CBF_LINK, NULL, NULL))

                                                  cbf_failnez (cbf_set_link ((yyval.node), (yyvsp[-1].node)))
                                                ;}
    break;

  case 54:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))
                                                  
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 55:

    {
                                                  cbf_failnez (cbf_find_parent (&((yyval.node)), (yyvsp[-1].node), CBF_SAVEFRAME))

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))
                                                 
                                                  ((void **)context)[3] = (void *)(yyval.node);

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 56:

    {
                                                  cbf_failnez (cbf_make_new_child (&((yyval.node)), (yyvsp[-1].node), CBF_CATEGORY, (yyvsp[0].text)))
                                                  
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, cbf_copy_string(NULL,(yyvsp[0].text),0)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))

                                                  cbf_failnez (cbf_add_link ((yyvsp[-1].node), (yyval.node)))

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 57:

    {
                                                  (yyval.node) = ((void **)context)[3];

                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyval.node), CBF_COLUMN, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))

                                                  cbf_failnez (cbf_add_link ((yyvsp[-1].node), (yyval.node)))

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 58:

    {
                                                  cbf_failnez (cbf_make_child (&((yyval.node)), (yyvsp[-1].node), CBF_COLUMN, (yyvsp[0].text)))
                                                  
                                                  ((cbf_handle)(((void **)context)[2]))->node=(cbf_node *)(yyval.node);
                                                  
                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyval.node), CBF_COLUMN,
                                                                                                                  (cbf_node *)(((void **)context)[3])))

                                                  cbf_failnez (cbf_set_link ((yyvsp[-1].node), (yyval.node)))

                                                  cbf_failnez (cbf_add_link ((yyvsp[-1].node), (yyval.node)))

                                                  (yyval.node) = (yyvsp[-1].node);
                                                ;}
    break;

  case 59:

    {
                                                  (yyval.node) = (yyvsp[-1].node);

                                                  cbf_failnez (cbf_shift_link ((yyval.node)))

                                                  cbf_failnez (cbf_add_columnrow ((yyval.node), (yyvsp[0].text)))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].text), CBF_VALUE,
                                                                                                                  (cbf_node *) (yyval.node)))
                                                ;}
    break;

  case 60:

    {
                                                  (yyval.node) = (yyvsp[-1].node);

                                                  cbf_failnez (cbf_shift_link ((yyval.node)))

                                                  cbf_failnez (cbf_add_columnrow ((yyval.node), (yyvsp[0].text)))

                                                  cbf_failnez (cbf_validate ((cbf_handle)(((void **)context)[2]), (cbf_node *) (yyvsp[0].text), CBF_VALUE,
                                                                                                                  (cbf_node *) (yyval.node)))
                                                ;}
    break;

  case 62:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 63:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 64:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 65:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 66:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 67:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 68:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;

  case 69:

    {
                                                  (yyval.text) = (yyvsp[0].text);
                                                ;}
    break;


    }

/* Line 1037 of yacc.c.  */


  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {

		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 yydestruct ("Error: popping",
                             yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  yydestruct ("Error: discarding", yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping", yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token. */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yydestruct ("Error: discarding lookahead",
              yytoken, &yylval);
  yychar = YYEMPTY;
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}





#ifdef __cplusplus

}

#endif

