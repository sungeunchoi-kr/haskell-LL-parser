/*---------------------------------------------------------------------------------------*/
/*                                  BOOKKEEPER.H                                         */
/*---------------------------------------------------------------------------------------*/
#ifndef BOOKKEEPER_H
#define BOOKKEEPER_H

#define BOOKKEEPER_TYPE_IDENTIFIER    0x01
#define BOOKKEEPER_TYPE_CONSTANT      0x02

#define MAX_ROWS  100

/* symtab_row_t: a single entry in the symbol table. */
typedef struct {
    int type;
    char* token;
    int line;
} symtab_row_t;

/* symtab_t: the symbol table. */
typedef struct {
    symtab_row_t symbols[MAX_ROWS];
    int size;
} symtab_t;

/* the below are the "methods" on the symbol table. */
void BOOKKEEPER_init(symtab_t* self);
void BOOKKEEPER_add_symbol(symtab_t* self, const char* symbol, int type, int line);
void BOOKKEEPER_print(symtab_t* self);

const char* BOOKKEEPER_TYPE_tostring(int id);

#endif

