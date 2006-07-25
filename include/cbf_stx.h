typedef union
{
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} YYSTYPE;
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

