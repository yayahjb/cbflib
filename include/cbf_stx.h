/* A Bison parser, made by GNU Bison 1.875d.  */

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




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)

typedef union YYSTYPE {
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} YYSTYPE;
/* Line 1285 of yacc.c.  */

# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif





