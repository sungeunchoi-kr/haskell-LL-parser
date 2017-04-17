/*---------------------------------------------------------------------------------------*/
/*                                    SCANNER.H                                          */
/*---------------------------------------------------------------------------------------*/
#ifndef SCANNER_H
#define SCANNER_H

#include <stdio.h>

/* internal */
#define EPSILON             -1
#define SKIP                0

/* symbol type */
#define IDENTIFIER          1
#define CONSTANT            2
#define KEYWORD             3
#define SPECIAL             4

/* keywords */
#define KW_class            0x0fff 
#define KW_ssalc            0x1000 
#define KW_forall           0x1001 
#define KW_formyselfonly    0x1002 
#define KW_method           0x1003 
#define KW_dohtem           0x1004 
#define KW_number           0x1005 
#define KW_boolean          0x1006 
#define KW_see              0x1007 
#define KW_show             0x1008 
#define KW_if               0x1009 
#define KW_otherwise        0x100a 
#define KW_onlyif           0x100b 
#define KW_loop             0x100c 
#define KW_pool             0x100d 
#define KW_copy             0x100e 
#define KW_into             0x100f 
#define KW_exit             0x1010 
#define KW_ifnot            0x1011 
#define KW_nono             0x1012 
#define KW_notfalse         0x1013 
#define KW_nottrue          0x1014 
#define KW_and              0x1015 
#define KW_or               0x1016 
#define KW_less             0x1017 
#define KW_notequal         0x1018 
#define KW_add              0x1019 

/* 4 special symbols*/
#define SP_LPAREN           0x1020 
#define SP_RPAREN           0x1021
#define SP_SEMICOLON        0x1022
#define SP_STAR             0x1023 


#define STRBUF_MAXBUFSZ  1024

/*
 * strbuf_t: buffer kept while the dfa moves around, so we know
 * what it has read when it accepts.
 */
typedef struct {
    char data[STRBUF_MAXBUFSZ];
    int len;
} strbuf_t;

/*
 * parse_context_t: the currrent parsing context.
 * read this when a token is accepted to know what type/value the
 * token is.
 */
typedef struct {
    int type;      /* type of token recognized */
    int value;     /* the value of token (if keyword or special) */
    strbuf_t buf;  /* the buffer strbuf_t */
} parse_context_t;
parse_context_t context;

/* append a character to the buffer. */
void bufcat(strbuf_t* buffer, char c);
/* clear the buffer */
void strbuf_clear(strbuf_t* buffer);

extern int ACCEPT_HANDLER(void);
extern int ERROR_HANDLER(char c);
int SCANNER(FILE* file);

#endif

