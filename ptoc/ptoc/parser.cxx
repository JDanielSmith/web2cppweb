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
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse zzparse
#define yylex   zzlex
#define yyerror zzerror
#define yylval  zzlval
#define yychar  zzchar
#define yydebug zzdebug
#define yynerrs zznerrs


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




/* Copy the first part of user declarations.  */
///*** #line 1 "parser.y"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nmtbl.h"
#include "token.h"
#include "trnod.h"
#include "util.h"

static int zzcnv_table[] = {
#define DEF_TOKEN(mnem, cat, cls, yacc) yacc,
#include "token.dpp"
};

#define YYINITDEPTH 100000

void zzerror(char* text)
{
	error(curr_token, "syntax error: %s", text);
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
/* Line 190 of yacc.c.  */
///*** #line 303 "parser.tab.c"
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 213 of yacc.c.  */
///*** #line 315 "parser.tab.c"

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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1219

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  90
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  95
/* YYNRULES -- Number of rules. */
#define YYNRULES  270
/* YYNRULES -- Number of states. */
#define YYNSTATES  549

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   334

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
{
	0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	52,    53,     2,     2,    49,     2,    47,     2,     2,     2,
	2,     2,     2,     2,     2,     2,     2,     2,    50,    51,
	2,     2,     2,     2,    57,     2,     2,     2,     2,     2,
	2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	2,    54,     2,    55,    56,     2,     2,     2,     2,     2,
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
	15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
	25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
	35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
	45,    46,    48,    58,    59,    60,    61,    62,    63,    64,
	65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
	75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
	85,    86,    87,    88,    89
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
YYRHS.  */
static const unsigned short int yyprhs[] =
{
	0,     0,     3,     4,     7,     9,    11,    13,    16,    23,
	24,    26,    28,    35,    45,    55,    62,    69,    71,    72,
	76,    80,    82,    85,    86,    89,    91,    93,    95,    97,
	99,   101,   103,   110,   114,   115,   118,   120,   122,   124,
	126,   128,   130,   132,   133,   137,   141,   145,   149,   153,
	157,   161,   165,   169,   172,   175,   181,   186,   193,   202,
	207,   212,   215,   218,   220,   222,   227,   231,   235,   237,
	241,   243,   247,   248,   250,   251,   255,   257,   261,   263,
	265,   266,   268,   272,   276,   278,   282,   284,   288,   290,
	294,   298,   302,   306,   310,   314,   318,   322,   326,   330,
	334,   338,   342,   346,   350,   354,   358,   362,   366,   370,
	374,   378,   382,   386,   390,   394,   398,   400,   403,   406,
	409,   412,   415,   417,   421,   426,   430,   433,   438,   445,
	447,   449,   451,   453,   457,   459,   460,   462,   466,   468,
	472,   474,   478,   480,   484,   486,   487,   491,   493,   497,
	501,   505,   507,   511,   513,   517,   523,   527,   529,   533,
	535,   539,   542,   543,   547,   551,   557,   560,   561,   565,
	569,   572,   573,   575,   579,   585,   589,   593,   599,   603,
	609,   616,   625,   630,   637,   644,   653,   662,   673,   679,
	688,   699,   707,   710,   713,   715,   717,   718,   722,   724,
	728,   731,   733,   735,   739,   741,   743,   745,   747,   749,
	751,   753,   755,   757,   759,   761,   763,   765,   767,   769,
	771,   773,   775,   777,   782,   785,   790,   792,   794,   802,
	810,   818,   826,   830,   834,   837,   842,   847,   852,   856,
	863,   866,   868,   870,   872,   874,   877,   881,   883,   885,
	888,   891,   896,   897,   899,   901,   905,   911,   913,   917,
	919,   923,   926,   928,   930,   935,   939,   941,   943,   946,
	950
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const short int yyrhs[] =
{
	91,     0,    -1,    -1,    92,    93,    -1,    94,    -1,    96,
	-1,    97,    -1,   101,    47,    -1,    29,    16,    99,    51,
	101,    47,    -1,    -1,    47,    -1,   102,    -1,    29,    16,
	99,    51,   102,    95,    -1,    42,    16,    51,    20,   105,
	19,    98,    10,    47,    -1,    42,    16,    51,    20,   105,
	19,    98,   108,    47,    -1,    20,   105,    19,    98,    10,
	47,    -1,    20,   105,    19,    98,   108,    47,    -1,   102,
	-1,    -1,    52,   100,    53,    -1,    16,    49,   100,    -1,
	16,    -1,   102,   108,    -1,    -1,   103,   102,    -1,   133,
	-1,   135,    -1,   138,    -1,   141,    -1,   147,    -1,   145,
	-1,   104,    -1,    42,    16,    51,    20,   105,    43,    -1,
	20,   105,    43,    -1,    -1,   106,   105,    -1,   133,    -1,
	135,    -1,   138,    -1,   141,    -1,   146,    -1,   145,    -1,
	104,    -1,    -1,   120,    67,   118,    -1,   120,    66,   118,
	-1,   120,    65,   118,    -1,   120,    64,   118,    -1,   120,
	63,   118,    -1,   120,    62,   118,    -1,   120,    61,   118,
	-1,   120,    60,   118,    -1,   120,    59,   118,    -1,    15,
	17,    -1,    15,    16,    -1,     5,   118,    24,   112,    10,
	-1,    18,   118,    38,   107,    -1,    18,   118,    38,   107,
	9,   107,    -1,    13,    16,    67,   118,    39,   118,     7,
	107,    -1,    45,   118,     7,   107,    -1,    33,   109,    41,
	118,    -1,    48,   111,    -1,    31,   110,    -1,   120,    -1,
	34,    -1,    46,   124,     7,   107,    -1,    17,    50,   107,
	-1,    16,    50,   107,    -1,   108,    -1,     4,   109,    10,
	-1,   107,    -1,   107,    51,   109,    -1,    -1,   130,    -1,
	-1,    52,   131,    53,    -1,   114,    -1,   114,   113,   109,
	-1,    26,    -1,     9,    -1,    -1,   115,    -1,   115,    51,
	114,    -1,   116,    50,   107,    -1,   117,    -1,   117,    49,
	116,    -1,   118,    -1,   118,     8,   118,    -1,   119,    -1,
	118,    78,   118,    -1,   118,    77,   118,    -1,   118,    85,
	118,    -1,   118,    82,   118,    -1,   118,    84,   118,    -1,
	118,    83,   118,    -1,   120,    67,   118,    -1,   120,    66,
	118,    -1,   120,    65,   118,    -1,   120,    64,   118,    -1,
	120,    63,   118,    -1,   120,    60,   118,    -1,   120,    59,
	118,    -1,   120,    62,   118,    -1,   120,    61,   118,    -1,
	118,    81,   118,    -1,   118,    79,   118,    -1,   118,    80,
	118,    -1,   118,    76,   118,    -1,   118,    75,   118,    -1,
	118,    70,   118,    -1,   118,    72,   118,    -1,   118,    71,
	118,    -1,   118,    69,   118,    -1,   118,    74,   118,    -1,
	118,    73,   118,    -1,   118,    68,   118,    -1,   120,    -1,
	78,   119,    -1,    77,   119,    -1,    87,   119,    -1,    57,
	120,    -1,    81,   120,    -1,   121,    -1,    52,   124,    53,
	-1,   120,    52,   125,    53,    -1,   120,    47,    16,    -1,
	120,    56,    -1,   120,    54,   124,    55,    -1,    22,    52,
	154,    49,   118,    53,    -1,   127,    -1,    17,    -1,    30,
	-1,    36,    -1,    54,   122,    55,    -1,    16,    -1,    -1,
	123,    -1,   123,    49,   122,    -1,   118,    -1,   118,     8,
	118,    -1,   118,    -1,   118,    49,   124,    -1,   126,    -1,
	126,    49,   125,    -1,   118,    -1,    -1,    52,   128,    53,
	-1,   129,    -1,   129,    51,   128,    -1,    16,    50,   118,
	-1,    52,   124,    53,    -1,   132,    -1,   132,    49,   131,
	-1,   118,    -1,   118,    50,   118,    -1,   118,    50,   118,
	50,   118,    -1,    21,   134,    51,    -1,    17,    -1,    17,
	49,   134,    -1,    16,    -1,    16,    49,   134,    -1,     6,
	136,    -1,    -1,   137,    51,   136,    -1,    16,    74,   118,
	-1,    16,    50,   155,    74,   118,    -1,    40,   139,    -1,
	-1,   140,    51,   139,    -1,    16,    74,   154,    -1,    44,
	142,    -1,    -1,   143,    -1,   143,    51,    58,    -1,   143,
	51,    58,    51,   142,    -1,   143,    51,   142,    -1,   100,
	50,   154,    -1,    16,    25,   118,    50,   158,    -1,    28,
	16,   149,    -1,    14,    16,   149,    50,   154,    -1,    28,
	16,   149,    51,   148,    51,    -1,    14,    16,   149,    50,
	154,    51,   148,    51,    -1,    28,    16,   149,    51,    -1,
	14,    16,   149,    50,   154,    51,    -1,    28,    16,   149,
	51,   101,    51,    -1,    14,    16,   149,    50,   154,    51,
	101,    51,    -1,    28,    16,    47,    16,   149,    51,   101,
	51,    -1,    14,    16,    47,    16,   149,    50,   154,    51,
	101,    51,    -1,    14,    16,    51,   101,    51,    -1,    28,
	16,   149,    51,    12,    51,   101,    51,    -1,    14,    16,
	149,    50,   154,    51,    12,    51,   101,    51,    -1,    14,
	16,    51,    12,    51,   101,    51,    -1,    16,   148,    -1,
	58,   148,    -1,    16,    -1,    58,    -1,    -1,    52,   150,
	53,    -1,   151,    -1,   151,    51,   150,    -1,    44,   152,
	-1,   152,    -1,   144,    -1,   100,    50,   153,    -1,   100,
	-1,   158,    -1,   161,    -1,   158,    -1,   159,    -1,   167,
	-1,   168,    -1,   165,    -1,   173,    -1,   164,    -1,   162,
	-1,   163,    -1,   157,    -1,   156,    -1,   158,    -1,   160,
	-1,   167,    -1,   166,    -1,   157,    -1,    14,   149,    50,
	154,    -1,    28,   149,    -1,    37,    54,   118,    55,    -1,
	16,    -1,    37,    -1,   174,     3,    54,   177,    55,    24,
	154,    -1,   174,     3,    54,   177,    55,    24,   155,    -1,
	174,     3,    54,   175,    55,    24,   158,    -1,   174,     3,
	54,   175,    55,    24,   161,    -1,    52,   100,    53,    -1,
	118,     8,   118,    -1,    56,   154,    -1,   174,    35,    24,
	154,    -1,   174,    35,    24,   155,    -1,   174,    32,   179,
	10,    -1,    23,   169,    10,    -1,    23,    52,    16,    53,
	169,    10,    -1,   170,   172,    -1,   172,    -1,   170,    -1,
	171,    -1,   143,    -1,   143,    51,    -1,   143,    51,   171,
	-1,   145,    -1,   146,    -1,   145,   172,    -1,   146,   172,
	-1,   174,    11,    24,   154,    -1,    -1,    27,    -1,   176,
	-1,   176,    51,   175,    -1,    16,     8,    16,    50,   154,
	-1,   178,    -1,   178,    49,   177,    -1,   158,    -1,   118,
	8,   118,    -1,   180,   181,    -1,   180,    -1,   142,    -1,
	5,   182,    24,   183,    -1,    16,    50,   154,    -1,   154,
	-1,   184,    -1,   184,    51,    -1,   184,    51,   183,    -1,
	124,    50,    52,   179,    53,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short int yyrline[] =
{
	0,   261,   261,   261,   285,   285,   285,   287,   288,   293,
	293,   295,   296,   304,   306,   308,   310,   313,   316,   317,
	319,   320,   322,   327,   328,   330,   330,   330,   330,   331,
	331,   331,   333,   335,   339,   340,   342,   342,   342,   342,
	343,   343,   343,   372,   373,   374,   375,   376,   377,   378,
	379,   380,   381,   382,   383,   384,   385,   386,   388,   390,
	391,   392,   393,   394,   395,   396,   397,   398,   399,   401,
	403,   403,   405,   405,   407,   408,   410,   411,   423,   423,
	425,   426,   427,   429,   431,   432,   434,   435,   459,   460,
	461,   462,   463,   464,   465,   467,   468,   469,   470,   471,
	472,   473,   474,   475,   478,   479,   480,   481,   482,   484,
	485,   486,   487,   488,   489,   490,   492,   493,   495,   497,
	499,   500,   502,   503,   504,   505,   506,   507,   508,   510,
	511,   512,   513,   514,   515,   517,   518,   519,   521,   522,
	524,   524,   526,   527,   529,   529,   532,   536,   537,   539,
	542,   545,   545,   547,   548,   549,   591,   594,   595,   596,
	597,   599,   602,   603,   605,   606,   608,   611,   612,   614,
	616,   619,   620,   621,   626,   632,   634,   635,   639,   641,
	646,   648,   652,   654,   658,   660,   662,   664,   666,   668,
	670,   672,   675,   676,   677,   678,   681,   682,   684,   685,
	687,   688,   688,   690,   691,   693,   693,   699,   699,   699,
	699,   699,   699,   700,   700,   700,   700,   700,   702,   702,
	702,   702,   702,   704,   706,   708,   710,   711,   713,   716,
	719,   721,   724,   726,   729,   731,   733,   735,   738,   740,
	743,   745,   746,   748,   751,   752,   753,   755,   755,   756,
	757,   761,   763,   763,   765,   766,   768,   771,   771,   774,
	775,   779,   781,   784,   786,   791,   792,   795,   796,   797,
	799
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
	"$end", "error", "$undefined", "ARRAY", "BEGIN", "CASE", "CONST", "DO",
	"DOTS", "ELSE", "END", "FIL", "FAR", "FOR", "FUNCTION", "GOTO", "IDENT",
	"ICONST", "IF", "IMPLEMENTATION", "INTERFACE", "LABEL", "LOOPHOLE",
	"OBJECT", "OF", "ORIGIN", "OTHERWISE", "PACKED", "PROCEDURE", "PROGRAM",
	"RCONST", "READ", "RECORD", "REPEAT", "RETURN", "SET", "SCONST",
	"STRING", "THEN", "TO", "TYPE", "UNTIL", "UNIT", "UNIT_END", "VAR",
	"WHILE", "WITH", "'.'", "WRITE", "','", "':'", "';'", "'('", "')'",
	"'['", "']'", "'^'", "'@'", "SCOPE", "LETSHR", "LETSHL", "LETOR",
	"LETAND", "LETMUL", "LETDIV", "LETSUB", "LETADD", "LET", "IN", "GE",
	"GT", "LE", "LT", "NE", "EQ", "XOR", "OR", "MINUS", "PLUS", "SHL", "SHR",
	"AND", "MUL", "DIVR", "DIV", "MOD", "ADDRESS", "NOT", "UMINUS", "UPLUS",
	"$accept", "translation", "@1", "input_file", "program", "progend",
	"module", "unit", "unit_def_list", "prog_param_list", "ident_list",
	"block", "decl_part_list", "decl_part", "unit_spec", "unit_decl_list",
	"unit_decl", "statement", "compoundst", "sequence", "actual_params",
	"write_params", "case_list", "otherwise", "case_items", "case_item",
	"case_elem_list", "case_elem", "expr", "simple_expr", "primary",
	"constant", "set_elem_list", "set_elem", "expr_list", "act_param_list",
	"act_param", "record_constant", "field_init_list", "field_init_item",
	"expr_group", "write_list", "write_param", "label_decl_part",
	"label_list", "const_def_part", "const_def_list", "const_def",
	"type_def_part", "type_def_list", "type_def", "var_decl_part",
	"var_decl_list", "var_decl", "proc_decl", "proc_fwd_decl", "proc_spec",
	"proc_def", "qualifiers", "formal_params", "formal_param_list",
	"formal_param", "param_decl", "param_type", "type", "const_type",
	"fptr_type", "string_type", "simple_type", "array_type",
	"const_array_type", "conformant_array_type", "enum_type", "range_type",
	"pointer_type", "set_type", "const_set_type", "record_type",
	"object_type", "object_components", "object_fields", "field_decl_list",
	"object_methods", "file_type", "packed", "conformant_indices",
	"conformant_index", "indices", "index_spec", "field_list", "fixed_part",
	"variant_part", "selector", "variant_list", "variant", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
token YYLEX-NUM.  */
static const unsigned short int yytoknum[] =
{
	0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
	265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
	275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
	285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
	295,   296,   297,   298,   299,   300,   301,    46,   302,    44,
	58,    59,    40,    41,    91,    93,    94,    64,   303,   304,
	305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
	315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
	325,   326,   327,   328,   329,   330,   331,   332,   333,   334
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
	0,    90,    92,    91,    93,    93,    93,    94,    94,    95,
	95,    96,    96,    97,    97,    97,    97,    98,    99,    99,
	100,   100,   101,   102,   102,   103,   103,   103,   103,   103,
	103,   103,   104,   104,   105,   105,   106,   106,   106,   106,
	106,   106,   106,   107,   107,   107,   107,   107,   107,   107,
	107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
	107,   107,   107,   107,   107,   107,   107,   107,   107,   108,
	109,   109,   110,   110,   111,   111,   112,   112,   113,   113,
	114,   114,   114,   115,   116,   116,   117,   117,   118,   118,
	118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
	118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
	118,   118,   118,   118,   118,   118,   119,   119,   119,   119,
	119,   119,   120,   120,   120,   120,   120,   120,   120,   121,
	121,   121,   121,   121,   121,   122,   122,   122,   123,   123,
	124,   124,   125,   125,   126,   126,   127,   128,   128,   129,
	130,   131,   131,   132,   132,   132,   133,   134,   134,   134,
	134,   135,   136,   136,   137,   137,   138,   139,   139,   140,
	141,   142,   142,   142,   142,   142,   143,   143,   144,   144,
	145,   145,   146,   146,   147,   147,   147,   147,   147,   147,
	147,   147,   148,   148,   148,   148,   149,   149,   150,   150,
	151,   151,   151,   152,   152,   153,   153,   154,   154,   154,
	154,   154,   154,   154,   154,   154,   154,   154,   155,   155,
	155,   155,   155,   156,   156,   157,   158,   158,   159,   160,
	161,   161,   162,   163,   164,   165,   166,   167,   168,   168,
	169,   169,   169,   170,   171,   171,   171,   172,   172,   172,
	172,   173,   174,   174,   175,   175,   176,   177,   177,   178,
	178,   179,   179,   180,   181,   182,   182,   183,   183,   183,
	184
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
	0,     2,     0,     2,     1,     1,     1,     2,     6,     0,
	1,     1,     6,     9,     9,     6,     6,     1,     0,     3,
	3,     1,     2,     0,     2,     1,     1,     1,     1,     1,
	1,     1,     6,     3,     0,     2,     1,     1,     1,     1,
	1,     1,     1,     0,     3,     3,     3,     3,     3,     3,
	3,     3,     3,     2,     2,     5,     4,     6,     8,     4,
	4,     2,     2,     1,     1,     4,     3,     3,     1,     3,
	1,     3,     0,     1,     0,     3,     1,     3,     1,     1,
	0,     1,     3,     3,     1,     3,     1,     3,     1,     3,
	3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
	3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
	3,     3,     3,     3,     3,     3,     1,     2,     2,     2,
	2,     2,     1,     3,     4,     3,     2,     4,     6,     1,
	1,     1,     1,     3,     1,     0,     1,     3,     1,     3,
	1,     3,     1,     3,     1,     0,     3,     1,     3,     3,
	3,     1,     3,     1,     3,     5,     3,     1,     3,     1,
	3,     2,     0,     3,     3,     5,     2,     0,     3,     3,
	2,     0,     1,     3,     5,     3,     3,     5,     3,     5,
	6,     8,     4,     6,     6,     8,     8,    10,     5,     8,
	10,     7,     2,     2,     1,     1,     0,     3,     1,     3,
	2,     1,     1,     3,     1,     1,     1,     1,     1,     1,
	1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
	1,     1,     1,     4,     2,     4,     1,     1,     7,     7,
	7,     7,     3,     3,     2,     4,     4,     4,     3,     6,
	2,     1,     1,     1,     1,     2,     3,     1,     1,     2,
	2,     4,     0,     1,     1,     3,     5,     1,     3,     1,
	3,     2,     1,     1,     4,     3,     1,     1,     2,     3,
	5
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
means the default is an error.  */
static const unsigned short int yydefact[] =
{
	2,     0,    23,     1,   162,     0,    34,     0,     0,     0,
	167,     0,   171,     3,     4,     5,     6,     0,    11,    23,
	31,    25,    26,    27,    28,    30,    29,     0,   161,     0,
	196,     0,    34,     0,     0,    42,     0,    34,    36,    37,
	38,    39,    41,    40,   159,   157,     0,   196,    18,     0,
	166,     0,     0,    21,     0,   170,   172,     7,    43,    22,
	24,   252,     0,   162,     0,    23,     0,     0,   196,     0,
	196,     0,    23,    33,    35,     0,     0,   156,     0,     0,
	0,     0,   252,   167,     0,     0,     0,   252,   171,     0,
	0,     0,   134,   130,     0,     0,   131,    72,    43,    64,
	132,     0,     0,    74,     0,   135,    70,    68,     0,    63,
	122,   129,   226,   253,   227,     0,   222,   218,   219,   221,
	220,     0,   134,   130,     0,     0,     0,     0,     0,   164,
	88,   116,   163,   196,     0,     0,     0,     0,    21,     0,
	0,   204,   202,     0,   198,   201,   252,     0,     0,     0,
	0,    17,   160,   158,   196,    23,     0,    23,   196,   134,
	0,   196,     0,   252,     0,   169,   217,   216,   207,   208,
	214,   215,   213,   211,   209,   210,   212,     0,   168,    34,
	0,    20,   176,   173,   175,     0,     0,    54,    53,    43,
	43,     0,   252,     0,    62,    73,     0,     0,   140,     0,
	0,    61,   134,     0,     0,   147,   138,     0,   136,    43,
	69,     0,   145,     0,   126,     0,     0,     0,     0,     0,
	0,     0,     0,     0,     0,     0,     0,   171,     0,   120,
	118,   116,   117,   121,   119,     0,     0,     0,     0,     0,
	0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,     0,     0,    23,   188,   196,   196,   200,   252,   197,
	0,     0,   252,   182,    34,     0,     0,     0,     0,   194,
	195,     0,     0,    19,     0,     9,     0,     0,   244,   247,
	248,     0,   242,   243,   241,   224,   134,     0,   234,     0,
	0,     0,     0,     0,     0,   171,    80,     0,    67,    66,
	43,     0,     0,     0,    43,     0,    43,   153,     0,   151,
	0,   123,   146,     0,     0,   133,   135,    71,   125,   144,
	0,   142,     0,    52,    51,    50,    49,    48,    47,    46,
	45,    44,     0,   165,     0,   263,     0,   262,   252,   115,
	112,   109,   111,   110,   114,   113,   108,   107,    90,    89,
	105,   106,   104,    92,    94,    93,    91,   101,   100,   103,
	102,    99,    98,    97,    96,    95,   252,     0,     0,   178,
	227,   203,   205,   206,     0,   199,    23,     0,     0,    15,
	16,    23,    23,   192,   193,   184,   180,     8,    10,    12,
	252,     0,   245,   249,   250,   238,   240,   232,   233,     0,
	252,   252,    23,    32,   177,   174,     0,    76,    81,     0,
	84,    86,     0,    56,     0,   150,    60,    59,   141,    65,
	0,    75,     0,   149,     0,   148,   139,   137,   124,   145,
	127,   225,     0,   259,     0,   257,   237,   252,   261,   236,
	0,   191,   252,     0,     0,     0,     0,   183,     0,     0,
	223,     0,   246,     0,   251,   235,     0,    55,    79,    78,
	43,    80,    43,     0,     0,     0,    43,     0,   154,   152,
	143,     0,     0,     0,   134,   266,     0,    23,   179,     0,
	23,   185,   181,   186,   189,     0,     0,     0,     0,    77,
	82,    83,    85,    87,     0,    57,   128,     0,   260,   252,
	258,   252,     0,     0,     0,     0,   254,     0,   239,   252,
	13,    14,    43,   155,   229,   265,     0,   264,   267,   187,
	0,     0,     0,   190,   228,    58,     0,   268,     0,   252,
	255,   171,   269,   252,   230,   231,     0,   256,   270
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short int yydefgoto[] =
{
	-1,     1,     2,    13,    14,   399,    15,    16,   150,    81,
	54,    17,   136,    19,    20,    36,    37,   106,   107,   108,
	194,   201,   416,   470,   417,   418,   419,   420,   164,   130,
	131,   110,   207,   208,   203,   330,   331,   111,   204,   205,
	195,   318,   319,    21,    46,    22,    28,    29,    23,    50,
	51,    24,   345,    56,   142,    25,    43,    26,   282,    67,
	143,   144,   145,   381,   165,   115,   166,   167,   168,   169,
	118,   383,   170,   171,   172,   173,   119,   174,   175,   291,
	292,   293,   294,   176,   177,   515,   516,   444,   445,   346,
	347,   448,   486,   527,   528
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
STATE-NUM.  */
#define YYPACT_NINF -400
static const short int yypact[] =
{
	-400,    13,   235,  -400,     9,    16,   293,   245,    38,    42,
	54,    90,    93,  -400,  -400,  -400,  -400,    72,    57,   360,
	-400,  -400,  -400,  -400,  -400,  -400,  -400,   -31,  -400,    73,
	188,   117,   293,   121,   123,  -400,     5,   293,  -400,  -400,
	-400,  -400,  -400,  -400,    94,    98,   128,    29,   120,   140,
	-400,   177,   181,    10,   186,  -400,   187,  -400,   934,  -400,
	-400,    99,   717,     9,   226,   351,   106,   204,   205,   113,
	205,   208,   360,  -400,  -400,   245,   245,  -400,   232,   229,
	265,   231,   654,    54,   266,   717,   265,   654,    -2,   717,
	269,   250,   244,   246,   717,   248,  -400,   249,   934,  -400,
	-400,   717,   717,   251,   728,   717,   253,  -400,   287,   633,
	-400,  -400,  -400,  -400,   255,   224,  -400,  -400,  -400,  -400,
	-400,    30,  -400,  -400,    75,   717,   717,    75,   717,  1134,
	-400,  1133,  -400,   205,   259,   267,    57,   300,   268,   306,
	265,   273,  -400,   271,   283,  -400,   654,   286,   292,   318,
	12,  -400,  -400,  -400,   205,   890,   294,   360,   205,   873,
	88,   205,   767,   654,   478,  -400,  -400,  -400,  -400,  -400,
	-400,  -400,  -400,  -400,  -400,  -400,  -400,    64,  -400,   293,
	958,  -400,  -400,   299,  -400,   787,   284,  -400,  -400,   934,
	934,   527,   654,   717,  -400,  -400,   311,   426,   922,   346,
	717,  -400,   312,   314,   315,   313,   504,   321,   324,   934,
	-400,   354,   717,   717,  -400,   717,   717,   717,   717,   717,
	717,   717,   717,   717,   717,   717,   328,    93,   359,   175,
	-400,   175,  -400,   175,  -400,   717,   717,   717,   717,   717,
	717,   717,   717,   717,   717,   717,   717,   717,   717,   717,
	717,   717,   717,   717,   717,   717,   717,   717,   717,   717,
	717,   717,   335,   360,  -400,   205,   205,  -400,   196,  -400,
	106,   336,   654,    -1,   293,   339,   342,   341,   343,    -1,
	-1,   347,   348,  -400,   350,     8,   340,   387,   355,   104,
	104,   395,   104,  -400,  -400,  -400,   197,   358,  -400,   717,
	353,   384,   389,    82,     7,    93,   717,   717,  -400,  -400,
	934,   368,   371,   717,   934,   717,   934,   994,   372,   378,
	717,  -400,  -400,   405,   717,  -400,   717,  -400,  -400,  1134,
	382,   390,   376,  1134,  1134,  1134,  1134,  1134,  1134,  1134,
	1134,  1134,  1094,  1134,   393,  -400,   431,   437,    99,   438,
	438,   438,   438,   438,   438,   438,   247,   247,   247,   247,
	-400,  -400,  -400,  -400,  -400,  -400,  -400,  1134,  1134,  1134,
	1134,  1134,  1134,  1134,  1134,  1134,   654,   385,   394,  -400,
	-400,  -400,  -400,  -400,   443,  -400,   915,   403,   412,  -400,
	-400,   360,   360,  -400,  -400,  -400,  -400,  -400,  -400,  -400,
	654,   406,    93,  -400,  -400,  -400,  -400,  -400,  1134,   393,
	654,   654,   360,  -400,  -400,  -400,   448,   105,   410,   413,
	415,   556,   757,   453,   717,  -400,  1134,  -400,  -400,  -400,
	717,  -400,   717,  1134,   312,  -400,  1134,  -400,  -400,   717,
	-400,  -400,   582,  -400,   411,   416,  -400,   700,  -400,  -400,
	418,  -400,   654,   419,   421,   424,   425,    -1,   430,   432,
	-400,   114,  -400,   422,  -400,  -400,    69,  -400,  -400,  -400,
	934,   717,   934,   717,   717,   717,   934,  1063,  1030,  -400,
	-400,   717,   461,   393,     3,  -400,   463,   360,  -400,   472,
	360,  -400,  -400,  -400,  -400,   479,   466,   444,   446,  -400,
	-400,  -400,  -400,  1134,   460,  -400,  -400,   717,  1134,    99,
	-400,   654,   717,   473,   517,   471,   515,   516,  -400,   654,
	-400,  -400,   934,  1134,  -400,  -400,   518,  -400,   519,  -400,
	553,   547,   472,  -400,  -400,  -400,   539,   717,   542,   196,
	-400,    93,  -400,   654,  -400,  -400,   540,  -400,  -400
};

/* YYPGOTO[NTERM-NUM].  */
static const short int yypgoto[] =
{
	-400,  -400,  -400,  -400,  -400,  -400,  -400,  -400,   182,  -400,
	14,   -47,     2,  -400,    31,    -6,  -400,  -161,   -15,   -92,
	-400,  -400,  -400,  -400,   142,  -400,   141,  -400,   -55,    92,
	-38,  -400,   289,  -400,  -100,   178,  -400,  -400,   295,  -400,
	-400,   184,  -400,    32,   198,    34,   557,  -400,    45,   536,
	-400,    46,    -3,  -149,  -400,    66,  -115,  -400,  -238,    76,
	352,  -400,   481,  -400,   -51,  -331,  -400,   -56,   -61,  -400,
	-400,    84,  -400,  -400,  -400,  -400,  -400,   -53,  -400,   183,
	-400,   240,     1,  -400,   -60,   111,  -400,  -399,  -400,   107,
	-400,  -400,  -400,   108,  -400
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
positive, shift that token.  If negative, reduce the rule which
number is the opposite.  If zero, do what YYDEFACT says.
If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -227
static const short int yytable[] =
{
	117,   121,   199,    59,    18,   116,   196,   129,   120,    55,
	463,   288,    58,     3,    53,   279,    58,   449,   135,    61,
	109,    60,   275,   112,    72,    27,    69,  -226,   308,   309,
	180,    74,    30,   226,   185,    85,   182,    35,    38,   191,
	39,   393,   394,    62,   380,   290,   197,   198,    73,   198,
	206,    40,    41,   511,    47,   398,   183,   280,    48,    86,
	109,    58,   227,    35,    38,   228,    39,   300,    35,    38,
	49,    39,    42,    58,   151,   301,    78,    40,    41,   497,
	141,    66,    40,    41,   510,   184,   229,   231,   231,   233,
	231,   122,   123,   312,   156,   271,   227,    95,    42,   302,
	181,   412,    31,    42,    53,    96,    52,   198,   281,    53,
	284,   100,   298,   332,   468,   112,    33,   327,    31,    57,
	137,    59,   138,    79,    63,   413,   113,   104,    31,   105,
	53,   469,    33,    68,   139,   276,   114,    70,   198,    71,
	287,   311,    33,    75,   147,   317,   148,    76,   456,   423,
	140,   109,   109,   427,   141,   429,    73,   329,   198,   285,
	333,   334,   335,   336,   337,   338,   339,   340,   341,   342,
	343,   109,    80,   303,   290,   290,   297,   290,   524,    77,
	349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
	359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
	369,   370,   371,   372,   373,   374,   375,   382,   384,   262,
	35,    38,   112,    39,    82,   428,   377,   230,   232,   456,
	234,   387,   211,   113,    40,    41,   289,   212,    83,   213,
	277,   214,    84,   380,   286,    64,    87,   295,    88,    65,
	66,     4,   133,   414,   408,    42,    86,   320,   154,     5,
	-21,   421,   422,   288,   146,     6,     7,    66,   426,   149,
	198,    44,    45,     8,     9,   433,   187,   188,   388,   436,
	59,   206,   109,   152,   153,    10,   109,    11,   109,    12,
	155,   138,   157,   443,   141,   186,   179,   117,   121,   442,
	403,   404,   116,   406,   189,   120,   190,   210,   225,     4,
	192,   193,   415,   200,   209,    35,    38,    31,    39,   224,
	263,   501,   288,    32,     7,   505,   265,    86,   264,    40,
	41,    33,   266,   268,   269,   450,   246,   247,   248,   249,
	250,   251,   252,    10,   270,    34,   272,    12,   274,   455,
	42,   378,   379,   273,   458,   459,   290,   283,   443,   460,
	305,   307,   313,   316,   442,   289,   289,     4,   289,   464,
	465,   535,   320,   134,   323,     5,     4,   321,   322,   477,
	328,    32,     7,   326,     5,   478,   325,   317,   499,     8,
	32,     7,   344,   348,   329,   376,   389,   386,     8,   390,
	400,    10,   391,    34,   392,    12,   485,   397,   395,   396,
	10,   488,    34,   401,    12,   405,   402,   409,   410,   159,
	123,   407,   526,   411,   151,    95,   421,   424,   421,   503,
	504,   434,   443,    96,   425,   431,   508,   432,   442,   100,
	380,   440,   109,   314,   109,   438,   451,   526,   109,   439,
	513,   446,   447,   517,   452,   104,   453,   105,   117,   121,
	124,   498,   523,   116,   457,   413,   120,   198,   467,   461,
	525,   471,   476,   472,   473,   483,   482,   522,   534,   487,
	125,   126,   490,   489,   127,   491,   492,   496,   544,   384,
	128,   493,   198,   494,   109,   509,   299,   512,   514,   518,
	519,   520,   547,   521,   235,   236,   237,   238,   239,   240,
	241,   242,   243,   244,   245,   246,   247,   248,   249,   250,
	251,   252,   324,   242,   243,   244,   245,   246,   247,   248,
	249,   250,   251,   252,   529,   530,   531,   289,   235,   236,
	237,   238,   239,   240,   241,   242,   243,   244,   245,   246,
	247,   248,   249,   250,   251,   252,   235,   236,   237,   238,
	239,   240,   241,   242,   243,   244,   245,   246,   247,   248,
	249,   250,   251,   252,   474,   310,   532,   533,   536,   538,
	537,   539,   235,   236,   237,   238,   239,   240,   241,   242,
	243,   244,   245,   246,   247,   248,   249,   250,   251,   252,
	481,   541,   543,   548,   466,   235,   236,   237,   238,   239,
	240,   241,   242,   243,   244,   245,   246,   247,   248,   249,
	250,   251,   252,   500,   502,   437,   479,   480,   435,   178,
	132,   267,   385,   545,   235,   236,   237,   238,   239,   240,
	241,   242,   243,   244,   245,   246,   247,   248,   249,   250,
	251,   252,   462,   540,   495,   542,     0,     0,   546,     0,
	235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
	245,   246,   247,   248,   249,   250,   251,   252,   158,     0,
	159,   123,     0,     0,     0,     0,    95,   160,     0,     0,
	211,   113,   161,     0,    96,   212,     0,   213,     0,   214,
	100,   114,   215,   216,   217,   218,   219,   220,   221,   222,
	223,     0,     0,     0,     0,     0,   162,     0,   105,     0,
	163,   124,     0,     0,   158,     0,   484,   123,     0,     0,
	0,     0,    95,   160,     0,     0,     0,   113,   161,     0,
	96,   125,   126,   122,   123,   127,   100,   114,     0,    95,
	0,   128,     0,     0,   202,   123,     0,    96,     0,     0,
	95,     0,   162,   100,   105,     0,   163,   124,    96,     0,
	0,     0,     0,     0,   100,     0,     0,     0,     0,   104,
	0,   105,     0,     0,   124,     0,     0,   125,   126,     0,
	104,   127,   105,   296,   123,   124,     0,   128,     0,    95,
	0,     0,     0,     0,   125,   126,   475,    96,   127,     0,
	0,     0,     0,   100,   128,   125,   126,     0,     0,   127,
	0,   306,     0,     0,     0,   128,     0,     0,     0,   104,
	0,   105,     0,     0,   124,   235,   236,   237,   238,   239,
	240,   241,   242,   243,   244,   245,   246,   247,   248,   249,
	250,   251,   252,     0,   125,   126,     0,     0,   127,     0,
	0,     0,     0,     0,   128,   235,   236,   237,   238,   239,
	240,   241,   242,   243,   244,   245,   246,   247,   248,   249,
	250,   251,   252,  -226,     0,     0,     0,  -226,  -226,  -226,
	0,     0,     0,  -226,     0,     0,     0,  -226,     0,     0,
	0,     0,  -226,  -226,  -226,     0,     4,  -226,     0,     0,
	0,  -226,   278,     0,     5,     0,   279,     0,     0,     0,
	32,     7,     0,  -226,     0,  -226,  -226,  -226,     8,     0,
	0,     4,  -226,     0,  -226,     0,  -226,   454,  -226,     5,
	10,   279,    34,     0,    12,    32,     7,     0,    58,    89,
	0,     0,     0,     8,     0,     0,     0,    90,   280,    91,
	92,    93,    94,     0,     0,    10,    95,    34,     0,    12,
	0,     0,     0,     0,    96,    97,     0,    98,    99,     0,
	100,   315,     0,   280,     0,     0,     0,     0,     0,   101,
	102,     0,   103,     0,     0,     0,   104,     0,   105,     0,
	235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
	245,   246,   247,   248,   249,   250,   251,   252,   304,     0,
	0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,     0,     0,     0,     0,     0,   235,   236,   237,   238,
	239,   240,   241,   242,   243,   244,   245,   246,   247,   248,
	249,   250,   251,   252,   430,     0,     0,     0,     0,     0,
	0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,     0,   235,   236,   237,   238,   239,   240,   241,   242,
	243,   244,   245,   246,   247,   248,   249,   250,   251,   252,
	507,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,     0,     0,     0,     0,     0,     0,     0,   235,   236,
	237,   238,   239,   240,   241,   242,   243,   244,   245,   246,
	247,   248,   249,   250,   251,   252,   506,     0,     0,     0,
	0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,   235,   236,   237,   238,   239,   240,   241,   242,   243,
	244,   245,   246,   247,   248,   249,   250,   251,   252,   441,
	0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
	0,     0,   235,   236,   237,   238,   239,   240,   241,   242,
	243,   244,   245,   246,   247,   248,   249,   250,   251,   252,
	211,     0,     0,     0,     0,   212,     0,   213,     0,   214,
	0,     0,   253,   254,   255,   256,   257,   258,   259,   260,
	261,     0,   235,   236,   237,   238,   239,   240,   241,   242,
	243,   244,   245,   246,   247,   248,   249,   250,   251,   252
};

static const short int yycheck[] =
{
	61,    61,   102,    18,     2,    61,    98,    62,    61,    12,
	409,   160,     4,     0,    16,    16,     4,   348,    65,    50,
	58,    19,    10,    16,    19,    16,    32,    24,   189,   190,
	85,    37,    16,     3,    89,    25,    87,     6,     6,    94,
	6,   279,   280,    74,    37,   160,   101,   102,    43,   104,
	105,     6,     6,    50,    16,    47,    58,    58,    16,    49,
	98,     4,    32,    32,    32,    35,    32,     3,    37,    37,
	16,    37,     6,     4,    72,    11,    47,    32,    32,    10,
	66,    52,    37,    37,   483,    88,   124,   125,   126,   127,
	128,    16,    17,   193,    80,   146,    32,    22,    32,    35,
	86,    19,    14,    37,    16,    30,    16,   162,   155,    16,
	157,    36,   163,   213,     9,    16,    28,   209,    14,    47,
	14,   136,    16,    47,    51,    43,    27,    52,    14,    54,
	16,    26,    28,    16,    28,   150,    37,    16,   193,    16,
	52,   192,    28,    49,    68,   200,    70,    49,   386,   310,
	44,   189,   190,   314,   140,   316,    43,   212,   213,   157,
	215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
	225,   209,    52,   179,   289,   290,   162,   292,   509,    51,
	235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
	245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
	255,   256,   257,   258,   259,   260,   261,   268,   268,   133,
	179,   179,    16,   179,    74,   315,   263,   125,   126,   457,
	128,   272,    47,    27,   179,   179,   160,    52,    51,    54,
	154,    56,    51,    37,   158,    47,    50,   161,    51,    51,
	52,     6,    16,   304,   299,   179,    49,    50,    16,    14,
	53,   306,   307,   402,    50,    20,    21,    52,   313,    51,
	315,    16,    17,    28,    29,   320,    16,    17,   274,   324,
	285,   326,   310,    75,    76,    40,   314,    42,   316,    44,
	51,    16,    51,   344,   270,    16,    20,   348,   348,   344,
	289,   290,   348,   292,    50,   348,    50,    10,    74,     6,
	52,    52,   305,    52,    51,   274,   274,    14,   274,    54,
	51,   472,   461,    20,    21,   476,    16,    49,    51,   274,
	274,    28,    16,    50,    53,   376,    79,    80,    81,    82,
	83,    84,    85,    40,    51,    42,    50,    44,    20,   386,
	274,   265,   266,    51,   391,   392,   461,    53,   409,   400,
	51,    67,    41,     7,   409,   289,   290,     6,   292,   410,
	411,   522,    50,    12,    51,    14,     6,    53,    53,   424,
	16,    20,    21,    49,    14,   430,    55,   432,   470,    28,
	20,    21,    54,    24,   439,    50,    47,    51,    28,    47,
	50,    40,    51,    42,    51,    44,   447,    47,    51,    51,
	40,   452,    42,    16,    44,    10,    51,    54,    24,    16,
	17,    53,   512,    24,   412,    22,   471,    49,   473,   474,
	475,    16,   483,    30,    53,    53,   481,    49,   483,    36,
	37,    55,   470,     7,   472,    53,    51,   537,   476,    49,
	487,    10,     5,   490,    50,    52,     3,    54,   509,   509,
	57,   466,   507,   509,    51,    43,   509,   512,    10,    53,
	511,    51,     9,    50,    49,    49,    55,     7,   519,    51,
	77,    78,    51,    54,    81,    51,    51,    55,   539,   539,
	87,    51,   537,    51,   522,    24,     8,    24,    16,    10,
	24,    47,   543,    47,    68,    69,    70,    71,    72,    73,
	74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
	84,    85,     8,    75,    76,    77,    78,    79,    80,    81,
	82,    83,    84,    85,    51,     8,    55,   461,    68,    69,
	70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
	80,    81,    82,    83,    84,    85,    68,    69,    70,    71,
	72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
	82,    83,    84,    85,     8,    38,    51,    51,    50,    16,
	51,    24,    68,    69,    70,    71,    72,    73,    74,    75,
	76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
	8,    52,    50,    53,   412,    68,    69,    70,    71,    72,
	73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
	83,    84,    85,   471,   473,   326,   432,   439,   323,    83,
	63,   140,   270,   539,    68,    69,    70,    71,    72,    73,
	74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
	84,    85,   402,   532,   461,   537,    -1,    -1,   541,    -1,
	68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
	78,    79,    80,    81,    82,    83,    84,    85,    14,    -1,
	16,    17,    -1,    -1,    -1,    -1,    22,    23,    -1,    -1,
	47,    27,    28,    -1,    30,    52,    -1,    54,    -1,    56,
	36,    37,    59,    60,    61,    62,    63,    64,    65,    66,
	67,    -1,    -1,    -1,    -1,    -1,    52,    -1,    54,    -1,
	56,    57,    -1,    -1,    14,    -1,    16,    17,    -1,    -1,
	-1,    -1,    22,    23,    -1,    -1,    -1,    27,    28,    -1,
	30,    77,    78,    16,    17,    81,    36,    37,    -1,    22,
	-1,    87,    -1,    -1,    16,    17,    -1,    30,    -1,    -1,
	22,    -1,    52,    36,    54,    -1,    56,    57,    30,    -1,
	-1,    -1,    -1,    -1,    36,    -1,    -1,    -1,    -1,    52,
	-1,    54,    -1,    -1,    57,    -1,    -1,    77,    78,    -1,
	52,    81,    54,    16,    17,    57,    -1,    87,    -1,    22,
	-1,    -1,    -1,    -1,    77,    78,    39,    30,    81,    -1,
	-1,    -1,    -1,    36,    87,    77,    78,    -1,    -1,    81,
	-1,    24,    -1,    -1,    -1,    87,    -1,    -1,    -1,    52,
	-1,    54,    -1,    -1,    57,    68,    69,    70,    71,    72,
	73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
	83,    84,    85,    -1,    77,    78,    -1,    -1,    81,    -1,
	-1,    -1,    -1,    -1,    87,    68,    69,    70,    71,    72,
	73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
	83,    84,    85,     0,    -1,    -1,    -1,     4,     5,     6,
	-1,    -1,    -1,    10,    -1,    -1,    -1,    14,    -1,    -1,
	-1,    -1,    19,    20,    21,    -1,     6,    24,    -1,    -1,
	-1,    28,    12,    -1,    14,    -1,    16,    -1,    -1,    -1,
	20,    21,    -1,    40,    -1,    42,    43,    44,    28,    -1,
	-1,     6,    49,    -1,    51,    -1,    53,    12,    55,    14,
	40,    16,    42,    -1,    44,    20,    21,    -1,     4,     5,
	-1,    -1,    -1,    28,    -1,    -1,    -1,    13,    58,    15,
	16,    17,    18,    -1,    -1,    40,    22,    42,    -1,    44,
	-1,    -1,    -1,    -1,    30,    31,    -1,    33,    34,    -1,
	36,    49,    -1,    58,    -1,    -1,    -1,    -1,    -1,    45,
	46,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
	68,    69,    70,    71,    72,    73,    74,    75,    76,    77,
	78,    79,    80,    81,    82,    83,    84,    85,    50,    -1,
	-1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	-1,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,    71,
	72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
	82,    83,    84,    85,    50,    -1,    -1,    -1,    -1,    -1,
	-1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	-1,    -1,    68,    69,    70,    71,    72,    73,    74,    75,
	76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
	50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	-1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,
	70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
	80,    81,    82,    83,    84,    85,    53,    -1,    -1,    -1,
	-1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	-1,    68,    69,    70,    71,    72,    73,    74,    75,    76,
	77,    78,    79,    80,    81,    82,    83,    84,    85,    55,
	-1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
	-1,    -1,    68,    69,    70,    71,    72,    73,    74,    75,
	76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
	47,    -1,    -1,    -1,    -1,    52,    -1,    54,    -1,    56,
	-1,    -1,    59,    60,    61,    62,    63,    64,    65,    66,
	67,    -1,    68,    69,    70,    71,    72,    73,    74,    75,
	76,    77,    78,    79,    80,    81,    82,    83,    84,    85
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
	0,    91,    92,     0,     6,    14,    20,    21,    28,    29,
	40,    42,    44,    93,    94,    96,    97,   101,   102,   103,
	104,   133,   135,   138,   141,   145,   147,    16,   136,   137,
	16,    14,    20,    28,    42,   104,   105,   106,   133,   135,
	138,   141,   145,   146,    16,    17,   134,    16,    16,    16,
	139,   140,    16,    16,   100,   142,   143,    47,     4,   108,
	102,    50,    74,    51,    47,    51,    52,   149,    16,   105,
	16,    16,    19,    43,   105,    49,    49,    51,    47,   149,
	52,    99,    74,    51,    51,    25,    49,    50,    51,     5,
	13,    15,    16,    17,    18,    22,    30,    31,    33,    34,
	36,    45,    46,    48,    52,    54,   107,   108,   109,   120,
	121,   127,    16,    27,    37,   155,   157,   158,   160,   166,
	167,   174,    16,    17,    57,    77,    78,    81,    87,   118,
	119,   120,   136,    16,    12,   101,   102,    14,    16,    28,
	44,   100,   144,   150,   151,   152,    50,   149,   149,    51,
	98,   102,   134,   134,    16,    51,   100,    51,    14,    16,
	23,    28,    52,    56,   118,   154,   156,   157,   158,   159,
	162,   163,   164,   165,   167,   168,   173,   174,   139,    20,
	118,   100,   154,    58,   142,   118,    16,    16,    17,    50,
	50,   118,    52,    52,   110,   130,   109,   118,   118,   124,
	52,   111,    16,   124,   128,   129,   118,   122,   123,    51,
	10,    47,    52,    54,    56,    59,    60,    61,    62,    63,
	64,    65,    66,    67,    54,    74,     3,    32,    35,   120,
	119,   120,   119,   120,   119,    68,    69,    70,    71,    72,
	73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
	83,    84,    85,    59,    60,    61,    62,    63,    64,    65,
	66,    67,   149,    51,    51,    16,    16,   152,    50,    53,
	51,   154,    50,    51,    20,    10,   108,   149,    12,    16,
	58,   101,   148,    53,   101,   102,   149,    52,   143,   145,
	146,   169,   170,   171,   172,   149,    16,   100,   154,     8,
	3,    11,    35,   105,    50,    51,    24,    67,   107,   107,
	38,   154,   124,    41,     7,    49,     7,   118,   131,   132,
	50,    53,    53,    51,     8,    55,    49,   109,    16,   118,
	125,   126,   124,   118,   118,   118,   118,   118,   118,   118,
	118,   118,   118,   118,    54,   142,   179,   180,    24,   118,
	118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
	118,   118,   118,   118,   118,   118,   118,   118,   118,   118,
	118,   118,   118,   118,   118,   118,    50,   101,   149,   149,
	37,   153,   158,   161,   174,   150,    51,   154,   105,    47,
	47,    51,    51,   148,   148,    51,    51,    47,    47,    95,
	50,    16,    51,   172,   172,    10,   172,    53,   118,    54,
	24,    24,    19,    43,   158,   142,   112,   114,   115,   116,
	117,   118,   118,   107,    49,    53,   118,   107,   124,   107,
	50,    53,    49,   118,    16,   128,   118,   122,    53,    49,
	55,    55,   118,   158,   177,   178,    10,     5,   181,   155,
	154,    51,    50,     3,    12,   101,   148,    51,   101,   101,
	154,    53,   171,   177,   154,   154,    98,    10,     9,    26,
	113,    51,    50,    49,     8,    39,     9,   118,   118,   131,
	125,     8,    55,    49,    16,   154,   182,    51,   154,    54,
	51,    51,    51,    51,    51,   169,    55,    10,   108,   109,
	114,   107,   116,   118,   118,   107,    53,    50,   118,    24,
	177,    50,    24,   101,    16,   175,   176,   101,    10,    24,
	47,    47,     7,   118,   155,   154,   124,   183,   184,    51,
	8,    55,    51,    51,   154,   107,    50,    51,    16,    24,
	175,    52,   183,    50,   158,   161,   179,   154,    53
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
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
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
yy_stack_print(short int *bottom, short int *top)
#else
static void
yy_stack_print(bottom, top)
short int *bottom;
short int *top;
#endif
{
	YYFPRINTF(stderr, "Stack now");
	for (/* Nothing. */; bottom <= top; ++bottom)
		YYFPRINTF(stderr, " %d", *bottom);
	YYFPRINTF(stderr, "\n");
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
yy_reduce_print(int yyrule)
#else
static void
yy_reduce_print(yyrule)
int yyrule;
#endif
{
	int yyi;
	unsigned int yylno = yyrline[yyrule];
	YYFPRINTF(stderr, "Reducing stack by rule %d (line %u), ",
		yyrule - 1, yylno);
	/* Print the symbols being reduced, and their result.  */
	for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
		YYFPRINTF(stderr, "%s ", yytname[yyrhs[yyi]]);
	YYFPRINTF(stderr, "-> %s\n", yytname[yyr1[yyrule]]);
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
yystrlen(const char *yystr)
#   else
yystrlen(yystr)
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
yystpcpy(char *yydest, const char *yysrc)
#   else
yystpcpy(yydest, yysrc)
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
yysymprint(FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint(yyoutput, yytype, yyvaluep)
FILE *yyoutput;
int yytype;
YYSTYPE *yyvaluep;
#endif
{
	/* Pacify ``unused variable'' warnings.  */
	(void)yyvaluep;

	if (yytype < YYNTOKENS)
		YYFPRINTF(yyoutput, "token %s (", yytname[yytype]);
	else
		YYFPRINTF(yyoutput, "nterm %s (", yytname[yytype]);


# ifdef YYPRINT
	if (yytype < YYNTOKENS)
		YYPRINT(yyoutput, yytoknum[yytype], *yyvaluep);
# endif
	switch (yytype)
	{
	default:
		break;
	}
	YYFPRINTF(yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct(const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct(yymsg, yytype, yyvaluep)
const char *yymsg;
int yytype;
YYSTYPE *yyvaluep;
#endif
{
	/* Pacify ``unused variable'' warnings.  */
	(void)yyvaluep;

	if (!yymsg)
		yymsg = "Deleting";
	YY_SYMBOL_PRINT(yymsg, yytype, yyvaluep, yylocationp);

	switch (yytype)
	{

	default:
		break;
	}
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse(void *YYPARSE_PARAM);
# else
int yyparse();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse(void);
#else
int yyparse();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse(void *YYPARSE_PARAM)
# else
int yyparse(YYPARSE_PARAM)
void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse(void)
#else
int
yyparse()

#endif
#endif
{

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

	YYDPRINTF((stderr, "Starting parse\n"));

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
			yyoverflow("parser stack overflow",
				&yyss1, yysize * sizeof(*yyssp),
				&yyvs1, yysize * sizeof(*yyvsp),

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
				(union yyalloc *) YYSTACK_ALLOC(YYSTACK_BYTES(yystacksize));
			if (!yyptr)
				goto yyoverflowlab;
			YYSTACK_RELOCATE(yyss);
			YYSTACK_RELOCATE(yyvs);

#  undef YYSTACK_RELOCATE
			if (yyss1 != yyssa)
				YYSTACK_FREE(yyss1);
		}
# endif
#endif /* no yyoverflow */

		yyssp = yyss + yysize - 1;
		yyvsp = yyvs + yysize - 1;


		YYDPRINTF((stderr, "Stack size increased to %lu\n",
			(unsigned long int) yystacksize));

		if (yyss + yystacksize - 1 <= yyssp)
			YYABORT;
	}

	YYDPRINTF((stderr, "Entering state %d\n", yystate));

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
		YYDPRINTF((stderr, "Reading a token: "));
		yychar = YYLEX;
	}

	if (yychar <= YYEOF)
	{
		yychar = yytoken = YYEOF;
		YYDPRINTF((stderr, "Now at end of input.\n"));
	}
	else
	{
		yytoken = YYTRANSLATE(yychar);
		YY_SYMBOL_PRINT("Next token is", yytoken, &yylval, &yylloc);
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
	YY_SYMBOL_PRINT("Shifting", yytoken, &yylval, &yylloc);

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
	yyval = yyvsp[1 - yylen];


	YY_REDUCE_PRINT(yyn);
	switch (yyn)
	{
	case 2:
		///*** #line 261 "parser.y"
	{
		if (turbo_pascal) {
			zzcnv_table[TKN_STRING] = STRING;
			zzcnv_table[TKN_STR] = WRITE;
			zzcnv_table[TKN_SHL] = SHL;
			zzcnv_table[TKN_SHR] = SHR;
			zzcnv_table[TKN_XOR] = XOR;
			zzcnv_table[TKN_UNIT] = UNIT;
			zzcnv_table[TKN_FAR] = FAR;
			zzcnv_table[TKN_IMPLEMENTATION] = IMPLEMENTATION;
			zzcnv_table[TKN_INTERFACE] = INTERFACE;

			zzcnv_table[TKN_OBJECT] = OBJECT;
			zzcnv_table[TKN_CONSTRUCTOR] = PROCEDURE;
			zzcnv_table[TKN_DESTRUCTOR] = PROCEDURE;
		}
		else {
			zzcnv_table[TKN_ORIGIN] = ORIGIN;
		}
		; }
	break;

	case 3:
		///*** #line 280 "parser.y"
	{
		(yyvsp[0].n_node)->attrib(ctx_program);
		(yyvsp[0].n_node)->translate(ctx_program);
		; }
	break;

	case 7:
		///*** #line 287 "parser.y"
	{ (yyval.n_node) = new program_node(nullptr, nullptr, nullptr, nullptr, (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 8:
		///*** #line 289 "parser.y"
	{
		(yyval.n_node) = new program_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_imp), (yyvsp[-2].tok), (yyvsp[-1].n_block), (yyvsp[0].tok));
		; }
	break;

	case 9:
		///*** #line 293 "parser.y"
	{ (yyval.tok) = nullptr; ; }
	break;

	case 11:
		///*** #line 295 "parser.y"
	{ (yyval.n_node) = new module_node(nullptr, nullptr, nullptr, nullptr, (yyvsp[0].n_decl), nullptr); ; }
	break;

	case 12:
		///*** #line 297 "parser.y"
	{
		(yyval.n_node) = new module_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_imp), (yyvsp[-2].tok), (yyvsp[-1].n_decl), (yyvsp[0].tok));
		; }
	break;

	case 13:
		///*** #line 305 "parser.y"
	{ (yyval.n_node) = new unit_node((yyvsp[-8].tok), (yyvsp[-7].tok), (yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].n_decl), (yyvsp[-3].tok), (yyvsp[-2].n_decl), nullptr, (yyvsp[-1].tok), (yyvsp[0].tok)); ; }
	break;

	case 14:
		///*** #line 307 "parser.y"
	{ (yyval.n_node) = new unit_node((yyvsp[-8].tok), (yyvsp[-7].tok), (yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].n_decl), (yyvsp[-3].tok), (yyvsp[-2].n_decl), (yyvsp[-1].n_comp), nullptr, (yyvsp[0].tok)); ; }
	break;

	case 15:
		///*** #line 309 "parser.y"
	{ (yyval.n_node) = new unit_node(nullptr, nullptr, nullptr, (yyvsp[-5].tok), (yyvsp[-4].n_decl), (yyvsp[-3].tok), (yyvsp[-2].n_decl), nullptr, (yyvsp[-1].tok), (yyvsp[0].tok)); ; }
	break;

	case 16:
		///*** #line 311 "parser.y"
	{ (yyval.n_node) = new unit_node(nullptr, nullptr, nullptr, (yyvsp[-5].tok), (yyvsp[-4].n_decl), (yyvsp[-3].tok), (yyvsp[-2].n_decl), (yyvsp[-1].n_comp), nullptr, (yyvsp[0].tok)); ; }
	break;

	case 18:
		///*** #line 316 "parser.y"
	{ (yyval.n_imp) = nullptr; ; }
	break;

	case 19:
		///*** #line 317 "parser.y"
	{ (yyval.n_imp) = new import_list_node((yyvsp[-2].tok), (yyvsp[-1].toks), (yyvsp[0].tok)); ; }
	break;

	case 20:
		///*** #line 319 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[-2].tok), (yyvsp[0].toks)); ; }
	break;

	case 21:
		///*** #line 320 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[0].tok)); ; }
	break;

	case 22:
		///*** #line 323 "parser.y"
	{
		(yyval.n_block) = new block_node((yyvsp[-1].n_decl), (yyvsp[0].n_comp));
		; }
	break;

	case 23:
		///*** #line 327 "parser.y"
	{ (yyval.n_decl) = nullptr; ; }
	break;

	case 24:
		///*** #line 328 "parser.y"
	{ (yyvsp[-1].n_decl)->next = (yyvsp[0].n_decl); (yyval.n_decl) = (yyvsp[-1].n_decl); ; }
	break;

	case 32:
		///*** #line 334 "parser.y"
	{  (yyval.n_decl) = new unit_spec_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_decl)); ; }
	break;

	case 33:
		///*** #line 336 "parser.y"
	{  (yyval.n_decl) = new unit_spec_node(nullptr, nullptr, nullptr, (yyvsp[-2].tok), (yyvsp[-1].n_decl)); ; }
	break;

	case 34:
		///*** #line 339 "parser.y"
	{ (yyval.n_decl) = nullptr; ; }
	break;

	case 35:
		///*** #line 340 "parser.y"
	{ (yyvsp[-1].n_decl)->next = (yyvsp[0].n_decl); (yyval.n_decl) = (yyvsp[-1].n_decl); ; }
	break;

	case 43:
		///*** #line 372 "parser.y"
	{ (yyval.n_stmt) = new empty_node(curr_token->prev_relevant()); ; }
	break;

	case 44:
		///*** #line 373 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 45:
		///*** #line 374 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 46:
		///*** #line 375 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 47:
		///*** #line 376 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 48:
		///*** #line 377 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 49:
		///*** #line 378 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 50:
		///*** #line 379 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 51:
		///*** #line 380 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 52:
		///*** #line 381 "parser.y"
	{ (yyval.n_stmt) = new assign_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 53:
		///*** #line 382 "parser.y"
	{ (yyval.n_stmt) = new goto_node((yyvsp[-1].tok), (yyvsp[0].tok)); ; }
	break;

	case 54:
		///*** #line 383 "parser.y"
	{ (yyval.n_stmt) = new goto_node((yyvsp[-1].tok), (yyvsp[0].tok)); ; }
	break;

	case 55:
		///*** #line 384 "parser.y"
	{ (yyval.n_stmt) = new switch_node((yyvsp[-4].tok), (yyvsp[-3].n_expr), (yyvsp[-2].tok), (yyvsp[-1].n_case), (yyvsp[0].tok)); ; }
	break;

	case 56:
		///*** #line 385 "parser.y"
	{ (yyval.n_stmt) = new if_node((yyvsp[-3].tok), (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 57:
		///*** #line 387 "parser.y"
	{ (yyval.n_stmt) = new if_node((yyvsp[-5].tok), (yyvsp[-4].n_expr), (yyvsp[-3].tok), (yyvsp[-2].n_stmt), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 58:
		///*** #line 389 "parser.y"
	{ (yyval.n_stmt) = new for_node((yyvsp[-7].tok), (yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].n_expr), (yyvsp[-3].tok), (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 59:
		///*** #line 390 "parser.y"
	{ (yyval.n_stmt) = new while_node((yyvsp[-3].tok), (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 60:
		///*** #line 391 "parser.y"
	{ (yyval.n_stmt) = new repeat_node((yyvsp[-3].tok), (yyvsp[-2].n_stmt), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 61:
		///*** #line 392 "parser.y"
	{ (yyval.n_stmt) = new write_node((yyvsp[-1].tok), (yyvsp[0].n_wrls)); ; }
	break;

	case 62:
		///*** #line 393 "parser.y"
	{ (yyval.n_stmt) = new read_node((yyvsp[-1].tok), (yyvsp[0].n_grp)); ; }
	break;

	case 63:
		///*** #line 394 "parser.y"
	{ (yyval.n_stmt) = new pcall_node((yyvsp[0].n_expr)); ; }
	break;

	case 64:
		///*** #line 395 "parser.y"
	{ (yyval.n_stmt) = new return_node((yyvsp[0].tok)); ; }
	break;

	case 65:
		///*** #line 396 "parser.y"
	{ (yyval.n_stmt) = new with_node((yyvsp[-3].tok), (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 66:
		///*** #line 397 "parser.y"
	{ (yyval.n_stmt) = new label_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 67:
		///*** #line 398 "parser.y"
	{ (yyval.n_stmt) = new label_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 68:
		///*** #line 399 "parser.y"
	{ (yyval.n_stmt) = (yyvsp[0].n_comp); ; }
	break;

	case 69:
		///*** #line 401 "parser.y"
	{ (yyval.n_comp) = new compound_node((yyvsp[-2].tok), (yyvsp[-1].n_stmt), (yyvsp[0].tok)); ; }
	break;

	case 71:
		///*** #line 403 "parser.y"
	{ (yyvsp[-2].n_stmt)->next = (yyvsp[0].n_stmt); (yyval.n_stmt) = (yyvsp[-2].n_stmt); ; }
	break;

	case 72:
		///*** #line 405 "parser.y"
	{ (yyval.n_grp) = nullptr; ; }
	break;

	case 73:
		///*** #line 405 "parser.y"
	{ (yyval.n_grp) = (yyvsp[0].n_grp); ; }
	break;

	case 74:
		///*** #line 407 "parser.y"
	{ (yyval.n_wrls) = nullptr; ; }
	break;

	case 75:
		///*** #line 408 "parser.y"
	{ (yyval.n_wrls) = new write_list_node((yyvsp[-2].tok), (yyvsp[-1].n_wrtp), (yyvsp[0].tok)); ; }
	break;

	case 77:
		///*** #line 412 "parser.y"
	{
		if ((yyvsp[-2].n_case) != nullptr) {
			case_node** cpp;
			for (cpp = &(yyvsp[-2].n_case)->next; *cpp != nullptr; cpp = &(*cpp)->next);
			*cpp = new case_node(nullptr, (yyvsp[-1].tok), (yyvsp[0].n_stmt));
			(yyval.n_case) = (yyvsp[-2].n_case);
		}
		else {
			(yyval.n_case) = new case_node(nullptr, (yyvsp[-1].tok), (yyvsp[0].n_stmt));
		}
		; }
	break;

	case 80:
		///*** #line 425 "parser.y"
	{ (yyval.n_case) = nullptr; ; }
	break;

	case 82:
		///*** #line 427 "parser.y"
	{ (yyvsp[-2].n_case)->next = (yyvsp[0].n_case); (yyval.n_case) = (yyvsp[-2].n_case); ; }
	break;

	case 83:
		///*** #line 429 "parser.y"
	{ (yyval.n_case) = new case_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_stmt)); ; }
	break;

	case 85:
		///*** #line 432 "parser.y"
	{ (yyvsp[-2].n_expr)->next = (yyvsp[0].n_expr); (yyval.n_expr) = (yyvsp[-2].n_expr); ; }
	break;

	case 86:
		///*** #line 434 "parser.y"
	{ (yyval.n_expr) = (yyvsp[0].n_expr); ; }
	break;

	case 87:
		///*** #line 435 "parser.y"
	{ (yyval.n_expr) = new case_range_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 89:
		///*** #line 460 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_add, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 90:
		///*** #line 461 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_sub, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 91:
		///*** #line 462 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_mod, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 92:
		///*** #line 463 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_mul, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 93:
		///*** #line 464 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_div, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 94:
		///*** #line 465 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_divr, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 95:
		///*** #line 467 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_let, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 96:
		///*** #line 468 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letadd, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 97:
		///*** #line 469 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letsub, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 98:
		///*** #line 470 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letdiv, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 99:
		///*** #line 471 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letmul, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 100:
		///*** #line 472 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letshl, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 101:
		///*** #line 473 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letshr, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 102:
		///*** #line 474 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letand, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 103:
		///*** #line 475 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_letor, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 104:
		///*** #line 478 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_and, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 105:
		///*** #line 479 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_shl, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 106:
		///*** #line 480 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_shr, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 107:
		///*** #line 481 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_or, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 108:
		///*** #line 482 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_xor, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 109:
		///*** #line 484 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_gt, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 110:
		///*** #line 485 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_lt, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 111:
		///*** #line 486 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_le, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 112:
		///*** #line 487 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_ge, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 113:
		///*** #line 488 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_eq, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 114:
		///*** #line 489 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_ne, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 115:
		///*** #line 490 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_in, (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 117:
		///*** #line 493 "parser.y"
	{
		(yyval.n_expr) = new op_node(tn_plus, nullptr, (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 118:
		///*** #line 496 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_minus, nullptr, (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 119:
		///*** #line 498 "parser.y"
	{ (yyval.n_expr) = new op_node(tn_not, nullptr, (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 120:
		///*** #line 499 "parser.y"
	{ (yyval.n_expr) = new address_node((yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 121:
		///*** #line 500 "parser.y"
	{ (yyval.n_expr) = new address_node((yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 123:
		///*** #line 503 "parser.y"
	{ (yyval.n_expr) = new expr_group_node((yyvsp[-2].tok), (yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 124:
		///*** #line 504 "parser.y"
	{ (yyval.n_expr) = new fcall_node((yyvsp[-3].n_expr), (yyvsp[-2].tok), (yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 125:
		///*** #line 505 "parser.y"
	{ (yyval.n_expr) = new access_expr_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].tok)); ; }
	break;

	case 126:
		///*** #line 506 "parser.y"
	{ (yyval.n_expr) = new deref_expr_node((yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 127:
		///*** #line 507 "parser.y"
	{ (yyval.n_expr) = new idx_expr_node((yyvsp[-3].n_expr), (yyvsp[-2].tok), (yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 128:
		///*** #line 508 "parser.y"
	{ (yyval.n_expr) = new loophole_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_tpd), (yyvsp[-2].tok), (yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 130:
		///*** #line 511 "parser.y"
	{ (yyval.n_expr) = new integer_node((yyvsp[0].tok)); ; }
	break;

	case 131:
		///*** #line 512 "parser.y"
	{ (yyval.n_expr) = new real_node((yyvsp[0].tok)); ; }
	break;

	case 132:
		///*** #line 513 "parser.y"
	{ (yyval.n_expr) = new string_node((yyvsp[0].tok)); ; }
	break;

	case 133:
		///*** #line 514 "parser.y"
	{ (yyval.n_expr) = new set_node((yyvsp[-2].tok), (yyvsp[-1].n_item), (yyvsp[0].tok)); ; }
	break;

	case 134:
		///*** #line 515 "parser.y"
	{ (yyval.n_expr) = new atom_expr_node((yyvsp[0].tok)); ; }
	break;

	case 135:
		///*** #line 517 "parser.y"
	{ (yyval.n_item) = nullptr; ; }
	break;

	case 137:
		///*** #line 519 "parser.y"
	{ (yyvsp[-2].n_item)->next = (yyvsp[0].n_item); (yyval.n_item) = (yyvsp[-2].n_item); ; }
	break;

	case 138:
		///*** #line 521 "parser.y"
	{ (yyval.n_item) = new set_elem_node((yyvsp[0].n_expr)); ; }
	break;

	case 139:
		///*** #line 522 "parser.y"
	{ (yyval.n_item) = new set_range_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 141:
		///*** #line 524 "parser.y"
	{ (yyvsp[-2].n_expr)->next = (yyvsp[0].n_expr); (yyval.n_expr) = (yyvsp[-2].n_expr); ; }
	break;

	case 143:
		///*** #line 527 "parser.y"
	{ (yyvsp[-2].n_expr)->next = (yyvsp[0].n_expr); (yyval.n_expr) = (yyvsp[-2].n_expr); ; }
	break;

	case 145:
		///*** #line 529 "parser.y"
	{ (yyval.n_expr) = new skipped_node(curr_token->prev_relevant()); ; }
	break;

	case 146:
		///*** #line 532 "parser.y"
	{
		(yyval.n_expr) = new record_constant_node((yyvsp[-2].tok), (yyvsp[-1].n_field), (yyvsp[0].tok));
		; }
	break;

	case 147:
		///*** #line 536 "parser.y"
	{ (yyval.n_field) = (yyvsp[0].n_field); ; }
	break;

	case 148:
		///*** #line 537 "parser.y"
	{ (yyvsp[-2].n_field)->next = (yyvsp[0].n_field); (yyval.n_field) = (yyvsp[-2].n_field); ; }
	break;

	case 149:
		///*** #line 539 "parser.y"
	{ (yyval.n_field) = new field_init_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 150:
		///*** #line 542 "parser.y"
	{ (yyval.n_grp) = new expr_group_node((yyvsp[-2].tok), (yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 152:
		///*** #line 545 "parser.y"
	{ (yyvsp[-2].n_wrtp)->next = (yyvsp[0].n_wrtp); (yyval.n_wrtp) = (yyvsp[-2].n_wrtp); ; }
	break;

	case 153:
		///*** #line 547 "parser.y"
	{ (yyval.n_wrtp) = new write_param_node((yyvsp[0].n_expr)); ; }
	break;

	case 154:
		///*** #line 548 "parser.y"
	{ (yyval.n_wrtp) = new write_param_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 155:
		///*** #line 549 "parser.y"
	{ (yyval.n_wrtp) = new write_param_node((yyvsp[-4].n_expr), (yyvsp[-3].tok), (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 156:
		///*** #line 592 "parser.y"
	{ (yyval.n_decl) = new label_decl_part_node((yyvsp[-2].tok), (yyvsp[-1].toks), (yyvsp[0].tok)); ; }
	break;

	case 157:
		///*** #line 594 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[0].tok)); ; }
	break;

	case 158:
		///*** #line 595 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[-2].tok), (yyvsp[0].toks)); ; }
	break;

	case 159:
		///*** #line 596 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[0].tok)); ; }
	break;

	case 160:
		///*** #line 597 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[-2].tok), (yyvsp[0].toks)); ; }
	break;

	case 161:
		///*** #line 600 "parser.y"
	{ (yyval.n_decl) = new const_def_part_node((yyvsp[-1].tok), (yyvsp[0].n_cdef)); ; }
	break;

	case 162:
		///*** #line 602 "parser.y"
	{ (yyval.n_cdef) = nullptr; ; }
	break;

	case 163:
		///*** #line 603 "parser.y"
	{ (yyvsp[-2].n_cdef)->next = (yyvsp[0].n_cdef); (yyval.n_cdef) = (yyvsp[-2].n_cdef); ; }
	break;

	case 164:
		///*** #line 605 "parser.y"
	{ (yyval.n_cdef) = new const_def_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 165:
		///*** #line 606 "parser.y"
	{ (yyval.n_cdef) = new typed_const_def_node((yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].n_tpd), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 166:
		///*** #line 609 "parser.y"
	{ (yyval.n_decl) = new type_def_part_node((yyvsp[-1].tok), (yyvsp[0].n_tdef)); ; }
	break;

	case 167:
		///*** #line 611 "parser.y"
	{ (yyval.n_tdef) = nullptr; ; }
	break;

	case 168:
		///*** #line 612 "parser.y"
	{ (yyvsp[-2].n_tdef)->next = (yyvsp[0].n_tdef); (yyval.n_tdef) = (yyvsp[-2].n_tdef); ; }
	break;

	case 169:
		///*** #line 614 "parser.y"
	{ (yyval.n_tdef) = new type_def_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 170:
		///*** #line 617 "parser.y"
	{ (yyval.n_decl) = new var_decl_part_node((yyvsp[-1].tok), (yyvsp[0].n_vdcl)); ; }
	break;

	case 171:
		///*** #line 619 "parser.y"
	{ (yyval.n_vdcl) = nullptr; ; }
	break;

	case 173:
		///*** #line 622 "parser.y"
	{
		(yyvsp[-2].n_vdcl)->scope = (yyvsp[0].tok);
		(yyvsp[0].tok)->disable();
		; }
	break;

	case 174:
		///*** #line 627 "parser.y"
	{
		(yyvsp[-4].n_vdcl)->scope = (yyvsp[-2].tok);
		token::remove((yyvsp[-2].tok), (yyvsp[-1].tok));
		(yyvsp[-4].n_vdcl)->next = (yyvsp[0].n_vdcl); (yyval.n_vdcl) = (yyvsp[-4].n_vdcl);
		; }
	break;

	case 175:
		///*** #line 632 "parser.y"
	{ (yyvsp[-2].n_vdcl)->next = (yyvsp[0].n_vdcl); (yyval.n_vdcl) = (yyvsp[-2].n_vdcl); ; }
	break;

	case 176:
		///*** #line 634 "parser.y"
	{ (yyval.n_vdcl) = new var_decl_node((yyvsp[-2].toks), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 177:
		///*** #line 636 "parser.y"
	{ (yyval.n_vdcl) = (var_decl_node*)new var_origin_decl_node((yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 178:
		///*** #line 640 "parser.y"
	{ (yyval.n_decl) = new proc_decl_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_plist)); ; }
	break;

	case 179:
		///*** #line 642 "parser.y"
	{ (yyval.n_decl) = new proc_decl_node((yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].n_plist), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 180:
		///*** #line 647 "parser.y"
	{ (yyval.n_decl) = new proc_fwd_decl_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_plist), nullptr, nullptr, (yyvsp[-2].tok), (yyvsp[-1].toks), (yyvsp[0].tok)); ; }
	break;

	case 181:
		///*** #line 649 "parser.y"
	{ (yyval.n_decl) = new proc_fwd_decl_node((yyvsp[-7].tok), (yyvsp[-6].tok), (yyvsp[-5].n_plist), (yyvsp[-4].tok), (yyvsp[-3].n_tpd), (yyvsp[-2].tok), (yyvsp[-1].toks), (yyvsp[0].tok)); ; }
	break;

	case 182:
		///*** #line 653 "parser.y"
	{ (yyval.n_decl) = new proc_fwd_decl_node((yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_plist), nullptr, nullptr, (yyvsp[0].tok)); ; }
	break;

	case 183:
		///*** #line 655 "parser.y"
	{ (yyval.n_decl) = new proc_fwd_decl_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_plist), (yyvsp[-2].tok), (yyvsp[-1].n_tpd), (yyvsp[0].tok)); ; }
	break;

	case 184:
		///*** #line 659 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-5].tok), nullptr, nullptr, (yyvsp[-4].tok), (yyvsp[-3].n_plist), nullptr, nullptr, (yyvsp[-2].tok), nullptr, nullptr, (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 185:
		///*** #line 661 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-7].tok), nullptr, nullptr, (yyvsp[-6].tok), (yyvsp[-5].n_plist), (yyvsp[-4].tok), (yyvsp[-3].n_tpd), (yyvsp[-2].tok), nullptr, nullptr, (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 186:
		///*** #line 663 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-7].tok), (yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_plist), nullptr, nullptr, (yyvsp[-2].tok), nullptr, nullptr, (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 187:
		///*** #line 665 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-9].tok), (yyvsp[-8].tok), (yyvsp[-7].tok), (yyvsp[-6].tok), (yyvsp[-5].n_plist), (yyvsp[-4].tok), (yyvsp[-3].n_tpd), (yyvsp[-2].tok), nullptr, nullptr, (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 188:
		///*** #line 667 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-4].tok), nullptr, nullptr, (yyvsp[-3].tok), nullptr, nullptr, nullptr, (yyvsp[-2].tok), nullptr, nullptr, (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 189:
		///*** #line 669 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-7].tok), nullptr, nullptr, (yyvsp[-6].tok), (yyvsp[-5].n_plist), nullptr, nullptr, (yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 190:
		///*** #line 671 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-9].tok), nullptr, nullptr, (yyvsp[-8].tok), (yyvsp[-7].n_plist), (yyvsp[-6].tok), (yyvsp[-5].n_tpd), (yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 191:
		///*** #line 673 "parser.y"
	{ (yyval.n_decl) = new proc_def_node((yyvsp[-6].tok), nullptr, nullptr, (yyvsp[-5].tok), nullptr, nullptr, nullptr, (yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_block), (yyvsp[0].tok)); ; }
	break;

	case 192:
		///*** #line 675 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[-1].tok), (yyvsp[0].toks)); ; }
	break;

	case 193:
		///*** #line 676 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[-1].tok), (yyvsp[0].toks)); ; }
	break;

	case 194:
		///*** #line 677 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[0].tok)); ; }
	break;

	case 195:
		///*** #line 678 "parser.y"
	{ (yyval.toks) = new token_list((yyvsp[0].tok)); ; }
	break;

	case 196:
		///*** #line 681 "parser.y"
	{ (yyval.n_plist) = nullptr; ; }
	break;

	case 197:
		///*** #line 682 "parser.y"
	{ (yyval.n_plist) = new param_list_node((yyvsp[-2].tok), (yyvsp[-1].n_decl), (yyvsp[0].tok)); ; }
	break;

	case 199:
		///*** #line 685 "parser.y"
	{ (yyvsp[-2].n_decl)->next = (yyvsp[0].n_decl); (yyval.n_decl) = (yyvsp[-2].n_decl); ; }
	break;

	case 200:
		///*** #line 687 "parser.y"
	{ (yyval.n_decl) = new var_decl_part_node((yyvsp[-1].tok), (yyvsp[0].n_vdcl)); ; }
	break;

	case 201:
		///*** #line 688 "parser.y"
	{ (yyval.n_decl) = (yyvsp[0].n_vdcl); ; }
	break;

	case 203:
		///*** #line 690 "parser.y"
	{ (yyval.n_vdcl) = new var_decl_node((yyvsp[-2].toks), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 204:
		///*** #line 691 "parser.y"
	{ (yyval.n_vdcl) = new var_decl_node((yyvsp[0].toks), nullptr, nullptr); ; }
	break;

	case 223:
		///*** #line 705 "parser.y"
	{ (yyval.n_tpd) = new fptr_tpd_node((yyvsp[-3].tok), (yyvsp[-2].n_plist), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 224:
		///*** #line 706 "parser.y"
	{ (yyval.n_tpd) = new fptr_tpd_node((yyvsp[-1].tok), (yyvsp[0].n_plist)); ; }
	break;

	case 225:
		///*** #line 708 "parser.y"
	{ (yyval.n_tpd) = new varying_tpd_node((yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_expr), (yyvsp[0].tok)); ; }
	break;

	case 226:
		///*** #line 710 "parser.y"
	{ (yyval.n_tpd) = new simple_tpd_node((yyvsp[0].tok)); ; }
	break;

	case 227:
		///*** #line 711 "parser.y"
	{ (yyval.n_tpd) = new string_tpd_node((yyvsp[0].tok)); ; }
	break;

	case 228:
		///*** #line 714 "parser.y"
	{ (yyval.n_tpd) = new array_tpd_node((yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_idx), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 229:
		///*** #line 717 "parser.y"
	{ (yyval.n_tpd) = new array_tpd_node((yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_idx), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 230:
		///*** #line 720 "parser.y"
	{ (yyval.n_tpd) = new array_tpd_node((yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_idx), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 231:
		///*** #line 722 "parser.y"
	{ (yyval.n_tpd) = new array_tpd_node((yyvsp[-6].tok), (yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].n_idx), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 232:
		///*** #line 724 "parser.y"
	{ (yyval.n_tpd) = new enum_tpd_node((yyvsp[-2].tok), (yyvsp[-1].toks), (yyvsp[0].tok)); ; }
	break;

	case 233:
		///*** #line 727 "parser.y"
	{ (yyval.n_tpd) = new range_tpd_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 234:
		///*** #line 729 "parser.y"
	{ (yyval.n_tpd) = new ptr_tpd_node((yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 235:
		///*** #line 731 "parser.y"
	{ (yyval.n_tpd) = new set_tpd_node((yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 236:
		///*** #line 733 "parser.y"
	{ (yyval.n_tpd) = new set_tpd_node((yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 237:
		///*** #line 736 "parser.y"
	{ (yyval.n_tpd) = new record_tpd_node((yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_fldls), (yyvsp[0].tok)); ; }
	break;

	case 238:
		///*** #line 739 "parser.y"
	{ (yyval.n_tpd) = new object_tpd_node((yyvsp[-2].tok), nullptr, nullptr, nullptr, (yyvsp[-1].n_decl), (yyvsp[0].tok)); ; }
	break;

	case 239:
		///*** #line 741 "parser.y"
	{ (yyval.n_tpd) = new object_tpd_node((yyvsp[-5].tok), (yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_decl), (yyvsp[0].tok)); ; }
	break;

	case 240:
		///*** #line 744 "parser.y"
	{ (yyvsp[-1].n_decl)->next = (yyvsp[0].n_decl); (yyval.n_decl) = (yyvsp[-1].n_decl); ; }
	break;

	case 243:
		///*** #line 749 "parser.y"
	{ (yyval.n_decl) = new var_decl_part_node(nullptr, (yyvsp[0].n_vdcl)); ; }
	break;

	case 245:
		///*** #line 752 "parser.y"
	{ (yyval.n_vdcl) = (yyvsp[-1].n_vdcl); ; }
	break;

	case 246:
		///*** #line 753 "parser.y"
	{ (yyvsp[-2].n_vdcl)->next = (yyvsp[0].n_vdcl); (yyval.n_vdcl) = (yyvsp[-2].n_vdcl); ; }
	break;

	case 249:
		///*** #line 756 "parser.y"
	{ (yyvsp[-1].n_decl)->next = (yyvsp[0].n_decl); (yyval.n_decl) = (yyvsp[-1].n_decl); ; }
	break;

	case 250:
		///*** #line 757 "parser.y"
	{ (yyvsp[-1].n_decl)->next = (yyvsp[0].n_decl); (yyval.n_decl) = (yyvsp[-1].n_decl); ; }
	break;

	case 251:
		///*** #line 761 "parser.y"
	{ (yyval.n_tpd) = new file_tpd_node((yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 252:
		///*** #line 763 "parser.y"
	{ (yyval.tok) = nullptr; ; }
	break;

	case 255:
		///*** #line 766 "parser.y"
	{ (yyvsp[-2].n_idx)->next = (yyvsp[0].n_idx); (yyval.n_idx) = (yyvsp[-2].n_idx); ; }
	break;

	case 256:
		///*** #line 769 "parser.y"
	{ (yyval.n_idx) = new conformant_index_node((yyvsp[-4].tok), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 258:
		///*** #line 772 "parser.y"
	{ (yyvsp[-2].n_idx)->next = (yyvsp[0].n_idx); (yyval.n_idx) = (yyvsp[-2].n_idx); ; }
	break;

	case 259:
		///*** #line 774 "parser.y"
	{ (yyval.n_idx) = new type_index_node((yyvsp[0].n_tpd)); ; }
	break;

	case 260:
		///*** #line 775 "parser.y"
	{ (yyval.n_idx) = new range_index_node((yyvsp[-2].n_expr), (yyvsp[-1].tok), (yyvsp[0].n_expr)); ; }
	break;

	case 261:
		///*** #line 780 "parser.y"
	{ (yyval.n_fldls) = new field_list_node((yyvsp[-1].n_vdcl), (yyvsp[0].n_varp)); ; }
	break;

	case 262:
		///*** #line 782 "parser.y"
	{ (yyval.n_fldls) = new field_list_node((yyvsp[0].n_vdcl)); ; }
	break;

	case 264:
		///*** #line 787 "parser.y"
	{
		(yyval.n_varp) = new variant_part_node((yyvsp[-3].tok), (yyvsp[-2].n_sel), (yyvsp[-1].tok), (yyvsp[0].n_vari));
		; }
	break;

	case 265:
		///*** #line 791 "parser.y"
	{ (yyval.n_sel) = new selector_node((yyvsp[-2].tok), (yyvsp[-1].tok), (yyvsp[0].n_tpd)); ; }
	break;

	case 266:
		///*** #line 792 "parser.y"
	{ (yyval.n_sel) = new selector_node(nullptr, nullptr, (yyvsp[0].n_tpd)); ; }
	break;

	case 268:
		///*** #line 796 "parser.y"
	{ (yyval.n_vari) = (yyvsp[-1].n_vari); ; }
	break;

	case 269:
		///*** #line 797 "parser.y"
	{ (yyvsp[-2].n_vari)->next = (yyvsp[0].n_vari); (yyval.n_vari) = (yyvsp[-2].n_vari); ; }
	break;

	case 270:
		///*** #line 800 "parser.y"
	{
		(yyval.n_vari) = new variant_node((yyvsp[-4].n_expr), (yyvsp[-3].tok), (yyvsp[-2].tok), (yyvsp[-1].n_fldls), (yyvsp[0].tok));
		; }
	break;


	}

	/* Line 1037 of yacc.c.  */
	///*** #line 2950 "parser.tab.c"

	yyvsp -= yylen;
	yyssp -= yylen;


	YY_STACK_PRINT(yyss, yyssp);

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
			int yytype = YYTRANSLATE(yychar);
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
					yysize += yystrlen(yyprefix) + yystrlen(yytname[yyx]);
					yycount += 1;
					if (yycount == 5)
					{
						yysize = 0;
						break;
					}
				}
			yysize += (sizeof("syntax error, unexpected ")
				+ yystrlen(yytname[yytype]));
			yymsg = (char *)YYSTACK_ALLOC(yysize);
			if (yymsg != 0)
			{
				char *yyp = yystpcpy(yymsg, "syntax error, unexpected ");
				yyp = yystpcpy(yyp, yytname[yytype]);

				if (yycount < 5)
				{
					yyprefix = ", expecting ";
					for (yyx = yyxbegin; yyx < yyxend; ++yyx)
						if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
						{
							yyp = yystpcpy(yyp, yyprefix);
							yyp = yystpcpy(yyp, yytname[yyx]);
							yyprefix = " or ";
						}
				}
				yyerror(yymsg);
				YYSTACK_FREE(yymsg);
			}
			else
				yyerror("syntax error; also virtual memory exhausted");
		}
		else
#endif /* YYERROR_VERBOSE */
			yyerror("syntax error");
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
					yydestruct("Error: popping",
						yystos[*yyssp], yyvsp);
				}
		}
		else
		{
			yydestruct("Error: discarding", yytoken, &yylval);
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


		yydestruct("Error: popping", yystos[yystate], yyvsp);
		YYPOPSTACK;
		yystate = *yyssp;
		YY_STACK_PRINT(yyss, yyssp);
	}

	if (yyn == YYFINAL)
		YYACCEPT;

	*++yyvsp = yylval;


	/* Shift the error token. */
	YY_SYMBOL_PRINT("Shifting", yystos[yyn], yyvsp, yylsp);

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
	yydestruct("Error: discarding lookahead",
		yytoken, &yylval);
	yychar = YYEMPTY;
	yyresult = 1;
	goto yyreturn;

#ifndef yyoverflow
	/*----------------------------------------------.
	| yyoverflowlab -- parser overflow comes here.  |
	`----------------------------------------------*/
	yyoverflowlab :
				  yyerror("parser stack overflow");
				  yyresult = 2;
				  /* Fall through.  */
#endif

			  yyreturn:
#ifndef yyoverflow
				  if (yyss != yyssa)
					  YYSTACK_FREE(yyss);
#endif
				  return yyresult;
}


///*** #line 804 "parser.y"


int zzlex() {
	curr_token = curr_token ? curr_token->next_relevant()
		: token::first_relevant();
	if (curr_token->tag == TKN_SCONST) {
		while (curr_token->next != nullptr && curr_token->next->tag == TKN_SCONST)
		{
			curr_token->in_text = curr_token->out_text =
				dprintf("%s%s", curr_token->in_text, curr_token->next->in_text);
			curr_token->next->remove();
		}
	}
	else if (turbo_pascal && curr_token->tag == TKN_FILE
		&& curr_token->next_relevant()->tag != TKN_OF)
	{
		curr_token->tag = TKN_IDENT;
		curr_token->in_text = curr_token->out_text = "untyped_file";
		curr_token->name = nm_entry::find("untyped_file");
	}
	zzlval.tok = curr_token;
	return zzcnv_table[curr_token->tag];
}



