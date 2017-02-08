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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ARRAY = 258,
     BEGIN = 259,
     CASE = 260,
     CONST = 261,
     DO = 262,
     DOTS = 263,
     ELSE = 264,
     END = 265,
     FIL = 266,
     FAR = 267,
     FOR = 268,
     FUNCTION = 269,
     GOTO = 270,
     IDENT = 271,
     ICONST = 272,
     IF = 273,
     IMPLEMENTATION = 274,
     INTERFACE = 275,
     LABEL = 276,
     LOOPHOLE = 277,
     OBJECT = 278,
     OF = 279,
     ORIGIN = 280,
     OTHERWISE = 281,
     PACKED = 282,
     PROCEDURE = 283,
     PROGRAM = 284,
     RCONST = 285,
     READ = 286,
     RECORD = 287,
     REPEAT = 288,
     RETURN = 289,
     SET = 290,
     SCONST = 291,
     STRING = 292,
     THEN = 293,
     TO = 294,
     TYPE = 295,
     UNTIL = 296,
     UNIT = 297,
     UNIT_END = 298,
     VAR = 299,
     WHILE = 300,
     WITH = 301,
     WRITE = 302,
     SCOPE = 303,
     LETSHR = 304,
     LETSHL = 305,
     LETOR = 306,
     LETAND = 307,
     LETMUL = 308,
     LETDIV = 309,
     LETSUB = 310,
     LETADD = 311,
     LET = 312,
     IN = 313,
     GE = 314,
     GT = 315,
     LE = 316,
     LT = 317,
     NE = 318,
     EQ = 319,
     XOR = 320,
     OR = 321,
     MINUS = 322,
     PLUS = 323,
     SHL = 324,
     SHR = 325,
     AND = 326,
     MUL = 327,
     DIVR = 328,
     DIV = 329,
     MOD = 330,
     ADDRESS = 331,
     NOT = 332,
     UMINUS = 333,
     UPLUS = 334
   };
#endif
#define ARRAY 258
#define BEGIN 259
#define CASE 260
#define CONST 261
#define DO 262
#define DOTS 263
#define ELSE 264
#define END 265
#define FIL 266
#define FAR 267
#define FOR 268
#define FUNCTION 269
#define GOTO 270
#define IDENT 271
#define ICONST 272
#define IF 273
#define IMPLEMENTATION 274
#define INTERFACE 275
#define LABEL 276
#define LOOPHOLE 277
#define OBJECT 278
#define OF 279
#define ORIGIN 280
#define OTHERWISE 281
#define PACKED 282
#define PROCEDURE 283
#define PROGRAM 284
#define RCONST 285
#define READ 286
#define RECORD 287
#define REPEAT 288
#define RETURN 289
#define SET 290
#define SCONST 291
#define STRING 292
#define THEN 293
#define TO 294
#define TYPE 295
#define UNTIL 296
#define UNIT 297
#define UNIT_END 298
#define VAR 299
#define WHILE 300
#define WITH 301
#define WRITE 302
#define SCOPE 303
#define LETSHR 304
#define LETSHL 305
#define LETOR 306
#define LETAND 307
#define LETMUL 308
#define LETDIV 309
#define LETSUB 310
#define LETADD 311
#define LET 312
#define IN 313
#define GE 314
#define GT 315
#define LE 316
#define LT 317
#define NE 318
#define EQ 319
#define XOR 320
#define OR 321
#define MINUS 322
#define PLUS 323
#define SHL 324
#define SHR 325
#define AND 326
#define MUL 327
#define DIVR 328
#define DIV 329
#define MOD 330
#define ADDRESS 331
#define NOT 332
#define UMINUS 333
#define UPLUS 334




#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
///*** #line 27 "parser.y"
typedef union YYSTYPE {
    token                *tok;

    token_list           *toks; 

    node                 *n_node;
    tpd_node             *n_tpd;
    block_node           *n_block; 
    stmt_node            *n_stmt; 
    decl_node            *n_decl; 
    expr_node            *n_expr; 
    expr_group_node      *n_grp; 

    write_param_node     *n_wrtp; 
    write_list_node      *n_wrls; 
    case_node            *n_case; 
    set_item_node        *n_item; 

    const_def_node       *n_cdef; 
    type_def_node        *n_tdef; 
    var_decl_node        *n_vdcl; 

    field_init_node      *n_field;
    
    param_list_node      *n_plist; 
    idx_node             *n_idx; 

    field_list_node      *n_fldls;
    variant_part_node    *n_varp;
    selector_node        *n_sel; 
    variant_node         *n_vari;
    compound_node        *n_comp;
   
    import_list_node     *n_imp; 
} YYSTYPE;
/* Line 1318 of yacc.c.  */
///*** #line 231 "parser.tab.h"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE zzlval;



