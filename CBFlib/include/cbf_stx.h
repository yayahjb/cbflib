typedef union
{
  int          errorcode;
  const char  *text;
  cbf_node    *node;
} YYSTYPE;
#define	DATA	257
#define	LOOP	258
#define	ITEM	259
#define	CATEGORY	260
#define	COLUMN	261
#define	STRING	262
#define	WORD	263
#define	BINARY	264
#define	UNKNOWN	265
#define	COMMENT	266
#define	ERROR	267

