/*      SC      A Table Calculator
 *              Common definitions
 *
 *              original by James Gosling, September 1982
 *              modified by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              R. Bond  12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

/*---------------- General system dependent definitions ----------------*/

#include <ctype.h>      /* for isxxx() */
#include <errno.h>      /* adjust for non conforming systems */
#include <limits.h>     /* for PATH_MAX */
#include <stdarg.h>     /* adjust for non conforming systems */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined (__unix__) || (defined (__APPLE__) && defined (__MACH__))
#include <unistd.h>
#endif

#include "compat.h"
#include "util.h"

extern const char *progname;

#ifndef CRYPT_PATH
#define NOCRYPT
#endif

/*---------------- Spreadsheet data ----------------*/

#define MINROWS 100     /* minimum size at startup */
#define MINCOLS 30
#define ABSMAXROWS 0xFFFFFF  /* maximum number of rows */
#define ABSMAXCOLS 702  /* maximum number of columns for A-ZZ (base 26) */

#define CRROWS 1
#define CRCOLS 2
#define RESROW 3 /* rows reserved for prompt, error, and column numbers */

/* formats for engformat() */
#define REFMTFIX        0
#define REFMTFLT        1
#define REFMTENG        2
#define REFMTDATE       3
#define REFMTLDATE      4

#define DEFWIDTH    10      /* Default column width */
#define DEFPREC      2      /* Default precision */
#define DEFREFMT  REFMTFIX  /* Make default format fixed point  THA 10/14/90 */

#define FKEYS            25   /* Number of function keys available (0..24) */
#define HISTLEN         100   /* Number of history entries for vi emulation */
#define CPAIRS            8   /* Number of color pairs available */
#define COLFORMATS       10   /* Number of custom column formats */

typedef unsigned char sc_bool_t;

#define FBUFLEN 1024    /* buffer size for a single field */
#define PATHLEN (PATH_MAX < 8192 ? 8192 : PATH_MAX) /* maximum path length */

#ifndef DFLT_PAGER
#define DFLT_PAGER "more"       /* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD  FBUFLEN      /* for ! command and commands that use the pager */

/* a cellref has a row, column, flags and sheet */
typedef struct cellref cellref_t;
struct cellref {
    int row;
    short col;
    unsigned char sheet;
    unsigned char vf;
};

#define FIX_ROW         001
#define FULL_ROW        002
#define INVALID_ROW     004
#define FIX_COL         010
#define FULL_COL        020
#define INVALID_COL     040
#define INVALID_REF     044

/* a rangeref has 2 cell references. The sheet should be identical */
typedef struct rangeref rangeref_t;
struct rangeref {
    struct cellref left, right;
};

/*
 * ent_ptr holds the row/col # and address type of a cell
 *
 * vf is the type of cell address, 0 non-fixed, or bitwise OR of FIX_ROW or
 *      FIX_COL
 * vp : we just use vp->row or vp->col, vp may be a new cell just for holding
 *      row/col (say in gram.y) or a pointer to an existing cell
 */
struct ent_ptr {
    int vf;
    struct ent *vp;
};

/* holds the beginning/ending cells of a range */
struct range_s {
    struct ent_ptr left, right;
};

typedef struct enode enode_t;

#define SC_EMPTY   0
#define SC_ERROR   1
#define SC_BOOLEAN 2
#define SC_NUMBER  3
#define SC_STRING  4
#define SC_RANGE   5

typedef long sclong_t;
typedef unsigned long sculong_t;
#define SCLONG_MAX   LONG_MAX
#define SCLONG_MIN   LONG_MIN
#define SCULONG_MAX  ULONG_MIN

typedef struct scvalue scvalue_t;
struct scvalue {
    union {
        double v;
        SCXMEM string_t *str;
        rangeref_t rr;
        int error;
    } u;
    int type;
};

typedef struct eval_context eval_ctx_t;
struct eval_context {
    struct sheet *sp;
    int gmyrow, gmycol;  /* globals used to implement @myrow, @mycol cmds */
    int rowoffset, coloffset;   /* row & col offsets for range functions */
};

/* info for each cell, only alloc'd when something is stored in a cell */
struct ent {
    // XXX: should use union
    double v;                   /* v && label are set in EvalAll() */
    SCXMEM string_t *label;     /* cell string value */
    SCXMEM enode_t *expr;       /* cell formula */
    SCXMEM string_t *format;    /* printf format for this cell */
    unsigned char cellerror;    /* error in a cell? (should pack with flags) */
    unsigned char type;         /* SC_xxx */
    int row;
    short col;                  /* the cell col/row */
    short flags;
    rangeref_t nrr;             /* nrr: link to note */ // XXX: should just use flag
    struct ent *next;           /* next deleted ent (pulled, deleted cells) */
};

/* expression node is the basic block of formulae */
struct enode {
    unsigned short op;
    unsigned short type;
#define OP_TYPE_FUNC    0
#define OP_TYPE_VAR     1
#define OP_TYPE_RANGE   2
#define OP_TYPE_DOUBLE  3
#define OP_TYPE_STRING  4
#define OP_TYPE_ERROR   5
    int nargs;
    union {
        int error;                  /* error number */
        double k;                   /* constant # */
        struct ent_ptr v;           /* ref. another cell */
        struct range_s r;           /* op is on a range */
        SCXMEM string_t *s;         /* op is a string constant */
        SCXMEM enode_t *args[1];    /* flexible array of arguments */
    } e;
};

/* named ranges */
struct nrange {
    struct ent_ptr r_left, r_right;
    SCXMEM string_t *r_name;            /* possible name for this range */
    struct nrange *r_next, *r_prev;     /* chained ranges */
    int r_is_range;
};

/* framed ranges */
struct frange {
    struct ent *or_left, *or_right;     /* outer range */
    struct ent *ir_left, *ir_right;     /* inner range */
    struct frange *r_next, *r_prev;     /* chained frame ranges */
};

/* color ranges */
struct crange {
    struct ent *r_left, *r_right;
    int r_color;
    struct crange *r_next, *r_prev;     /* chained color ranges */
};

struct colorpair {
    int fg;
    int bg;
    enode_t *expr;
};

/* stores an abbreviation and its expansion */
/* doubly linked list, sorted by name */
struct abbrev {
    SCXMEM string_t *name;
    SCXMEM string_t *exp;
    SCXMEM struct abbrev *next, *prev;
};

struct impexfilt {
    char ext[PATHLEN];
    char plugin[PATHLEN];
    char type;
    struct impexfilt *next;
};

/* Use this structure to save the last 'g' command */
struct go_save {
    int g_type;
    int stflag;
    double g_n;
    SCXMEM string_t *g_s;
    rangeref_t g_rr;
    cellref_t st;
};

/* g_type can be: */
#define G_NONE    0     /* Starting value - must be 0 */
#define G_CELL    1
#define G_NUM     2
#define G_ERROR   3
#define G_INVALID 4
#define G_STR     5
#define G_NSTR    6
#define G_XSTR    7

/* opcode symbols */
enum opcode {
#define OP(op,min,max,efun,arg,str,desc)  op,
#include "opcodes.h"
#undef OP
    OP_count,
};

typedef void (*scarg_t)(void);

struct opdef {
    const char *name;
    signed char min;
    signed char max;
    signed char priority;
    signed char signature;
    scvalue_t (*efun)(eval_ctx_t *cp, enode_t *e);
    scarg_t arg;
};

extern struct opdef const opdefs[];

/* flag values (8 bits) */
#define IS_LOCKED       0001  /* is protected from user modification */
#define IS_CHANGED      0002  /* set when modifying ent, tested and udated in update() for partial screen updates */
#define IS_DELETED      0004  /* set by free_ent, cleared in move_area, pullcells, tested in eval, decompile */
#define IS_CLEARED      0010  /* set by clearent, used in syncref */
#define MAY_SYNC        0020  /* set when deleting cells, used in syncref */
#define HAS_NOTE        0040

#define ALIGN_MASK      0300
#define ALIGN_DEFAULT   0000
#define ALIGN_LEFT      0100
#define ALIGN_CENTER    0200
#define ALIGN_RIGHT     0300
#define ALIGN_CLIP      0400  /* clip contents if longer than colwidth instead of displaying '*' */

/* error values */
#define ERROR_NULL  1  // #NULL!  Intersection of ranges produced zero cells.
#define ERROR_DIV0  2  // #DIV/0! Attempt to divide by zero, including division by an empty cell. 6.13.11
#define ERROR_VALUE 3  // #VALUE! Parameter is wrong type.
#define ERROR_REF   4  // #REF!   Reference to invalid cell (e.g., beyond the application's abilities).
#define ERROR_NAME  5  // #NAME?  Unrecognized/deleted name.
#define ERROR_NUM   6  // #NUM!   Failed to meet domain constraints (e.g., input was too large or too small).
#define ERROR_NA    7  // #N/A    Not available. ISNA() returns TRUE for this value. Used for Lookup functions which fail.
#define ERROR_MEM   8  // #MEM!   Memory allocation error.
#define ERROR_INT   9  // #INT!   Internal error.
#define ERROR_count 10

/* calculation order */
#define BYCOLS 1
#define BYROWS 2

/* values for showrange for ranges of rows or columns */
#define SHOWROWS 2
#define SHOWCOLS 4

/* tblprint style output for: */
#define TBL     1       /* 'tbl' */
#define LATEX   2       /* 'LaTeX' */
#define TEX     3       /* 'TeX' */
#define SLATEX  4       /* 'SLaTeX' (Scandinavian LaTeX) */
#define FRAME   5       /* tblprint style output for FrameMaker */

#define GROWAMT 30      /* default minimum amount to grow */

#define GROWNEW     1   /* first time table */
#define GROWROW     2   /* add rows */
#define GROWCOL     4   /* add columns */
#define GROWBOTH    6   /* grow both */

/*---------------- keyboard input stuff ----------------*/

/*
 * CG: since we cannot use iscntrl() with an argument outside the
 * range EOF..0xFF and `ctl(c)`, ESC and DEL are defined explicitly
 * let's define ISCTL(c) consistently and test for DEL explicitly
 * wherever it is required.
 */
#define ISCTL(c)    (!((c) & ~0x1F))
#define ISBYTE(c)   (!((c) & ~0xFF))

#define ctl(c) ((c)&037)
#define ESC 033
#define DEL 0177

/* special key codes, compatible with Unicode */
#define SC_KEY_DOWN         0xE402      /* down-arrow key */
#define SC_KEY_UP           0xE403      /* up-arrow key */
#define SC_KEY_LEFT         0xE404      /* left-arrow key */
#define SC_KEY_RIGHT        0xE405      /* right-arrow key */
#define SC_KEY_HOME         0xE406      /* home key */
#define SC_KEY_BACKSPACE    0xE407      /* backspace key */
#define SC_KEY_F0           0xE410      /* Function keys. */
#define SC_KEY_F(n)         (SC_KEY_F0+(n)) /* 10 Function keys. */
#define SC_KEY_DC           0xE512      /* delete-character key */
#define SC_KEY_IC           0xE513      /* insert-character key */
#define SC_KEY_NPAGE        0xE522      /* next-page key */
#define SC_KEY_PPAGE        0xE523      /* previous-page key */
#define SC_KEY_ENTER        0xE527      /* enter/send key */
#define SC_KEY_END          0xE550      /* end key */
#define SC_KEY_FIND         0xE552      /* find key */
#define SC_KEY_HELP         0xE553      /* help key */
#define SC_KEY_MOUSE        0xE631      /* Mouse event has occurred */
#define SC_KEY_RESIZE       0xE632      /* Terminal resize event */

#define SC_ALT(c)   ((c)|0xE8000)       /* can be combined with all keys */

struct screen_mouse_event { int x, y, bstate; };

/* 5 bits per mouse button, 5 mouse buttons */
#define SC_BUTTON_RELEASED(n)        (001 << (((n)-1)*5))
#define SC_BUTTON_PRESSED(n)         (002 << (((n)-1)*5))
#define SC_BUTTON_CLICKED(n)         (004 << (((n)-1)*5))
#define SC_BUTTON_DOUBLE_CLICKED(n)  (010 << (((n)-1)*5))
#define SC_BUTTON_CTRL               (001 << ((6-1)*5))
#define SC_BUTTON_SHIFT              (002 << ((6-1)*5))
#define SC_BUTTON_ALT                (004 << ((6-1)*5))

/*---------------- Context sensitive help ----------------*/

enum help_context {
    HELP_INTRO,
    HELP_TOGGLEOPTIONS,
    HELP_SETOPTIONS,
    HELP_CURSOR,
    HELP_CELL,
    HELP_VI,
    HELP_FILE,
    HELP_ROW,
    HELP_RANGE,
    HELP_MISC,
    HELP_VAR,
    HELP_RANGEF,
    HELP_NUMERICF,
    HELP_STRINGF,
    HELP_FINF,
    HELP_TIMEF,
    HELP_NB,
};

struct menu_item {
    const char *option;
    const char *desc;
    int (*func)(const void *arg, int n);
    const void *arg;
    int n;
};

/*---------------- Global data ----------------*/

/* The table data is organized as:
   `tbl`: a pointer to an array of `maxrows` pointers to rows
   each row pointer points to an array of `maxcols` pointers to cells
   these cell pointers can be NULL or point to an allocated `ent` structure
   This design is suboptimal in terms of memory space and implies much
   dreaded three star programming.
 */
typedef struct rowfmt {
    unsigned char hidden;
} rowfmt_t;
typedef struct colfmt {
    unsigned char hidden;
    unsigned char fwidth;
    unsigned char precision;
    unsigned char realfmt;
} colfmt_t;

typedef struct sheet {
    SCXMEM struct ent ***tbl;
    int maxrow, maxcol;
    int maxrows, maxcols;   /* # cells currently allocated */
    int currow, curcol;     /* current cell */
    int strow, stcol;       /* screen top-left cell */
    int showrange;          /* causes ranges to be highlighted */
    int showsc, showsr;     /* starting cell for highlighted range */
    int rescol;             /* screen columns reserved for row numbers */
    int modflg;             /* sheet modified indicator */
    SCXMEM colfmt_t *colfmt;
    SCXMEM rowfmt_t *rowfmt;
    SCXMEM short *row_size;
    SCXMEM string_t *mdir;
    SCXMEM string_t *autorun;
    SCXMEM string_t *fkey[FKEYS];
    // XXX: should allocate and reallocate this array
    SCXMEM string_t *colformat[COLFORMATS];
    SCXMEM struct abbrev *abbr_base, *abbr_tail;
    SCXMEM struct crange *crange_base, *crange_tail;
    SCXMEM struct nrange *nrange_base, *nrange_tail;
    SCXMEM struct frange *frange_base, *frange_tail;
    int autocalc;     /* 1 to calculate after each update */
    int autoinsert;    /* Causes rows to be inserted if craction is non-zero
                          and the last cell in a row/column of the scrolling
                          portion of a framed range has been filled      */
    int autowrap;      /* Causes cursor to move to next row/column if craction
                          is non-zero and the last cell in a row/column of
                          the scrolling portion of a framed range has been
                          filled */
    int cslop;
    int optimize;     /* Causes numeric expressions to be optimized */
    int rndtoeven;
    int propagation;   /* max number of times to try calculation */
    int calc_order;
    int numeric;
    double prescale;   /* Prescale for constants in let() */
    int extfunc;       /* Enable/disable external functions */
    int showtop;
    int tbl_style;     /* headers for T command output */
    int craction;      /* 1 for down, 2 for right */
    int pagesize;  /* If nonzero, use instead of 1/2 screen height */
    int rowlimit;
    int collimit;
    int color;
    int colorneg;     /* Increment color number for cells with negative numbers */
    int colorerr;     /* Color cells with errors with color 3 */
    char curfile[PATHLEN];
} sheet_t;

static inline int row_hidden(sheet_t *sp, int row) { return sp->rowfmt[row].hidden; }
static inline int col_hidden(sheet_t *sp, int col) { return sp->colfmt[col].hidden; }
static inline int col_fwidth(sheet_t *sp, int col) { return sp->colfmt[col].fwidth; }

extern sheet_t *sht;

extern cellref_t savedcr[37];
extern cellref_t savedst[37];
extern int FullUpdate;
extern int rowsinrange;         /* Number of rows in target range of a goto */
extern int colsinrange;         /* Number of cols in target range of a goto */

extern char line[FBUFLEN];
extern int linelim;
extern int changed;

/* temporary sheet fragments: stack of 4 work buffers and 36 named buffers (a-z,0-9) */
typedef struct subsheet {
    int minrow, mincol, maxrow, maxcol;
    int ncols, nrows;
    SCXMEM struct ent *ptr;  /* list of allocated cells */
    SCXMEM colfmt_t *colfmt;
    SCXMEM rowfmt_t *rowfmt;
} subsheet_t;

extern int macrofd;
extern int brokenpipe;          /* Set to true if SIGPIPE is received */
extern char dpoint;     /* country-dependent decimal point from locale */
extern char thsep;      /* country-dependent thousands separator from locale */
extern int lcols;
extern int lastendrow;          /* Last bottom row of screen */
extern struct frange *lastfr;   /* Last framed range we were in */
extern int framerows;           /* Rows in current frame */
extern int framecols;           /* Columns in current frame */
extern char search_ind;         /* Search indicator */
extern char mode_ind;           /* Mode indicator */
extern int seenerr;
extern int emacs_bindings;      /* use emacs-like bindings */
extern sc_bool_t sc_decimal;    /* Set if there was a decimal point in the number */
extern SCXMEM string_t *histfile;
extern SCXMEM string_t *scext;
extern SCXMEM string_t *ascext;
extern SCXMEM string_t *tbl0ext;
extern SCXMEM string_t *tblext;
extern SCXMEM string_t *latexext;
extern SCXMEM string_t *slatexext;
extern SCXMEM string_t *texext;
extern struct go_save gs;
extern const char * const error_name[];
#ifndef NOCRYPT
extern int Crypt;
#endif

extern int skipautorun;
extern int scrc;
extern int autolabel;
extern int showcell;
extern int color;               /* global setting for terminal handling */
extern int braille;
extern int braillealt;
extern int dobackups;
extern int loading;

extern char revmsg[80];
extern int showneed;   /* Causes cells needing values to be highlighted */
extern int showexpr;   /* Causes cell exprs to be displayed, highlighted */
extern int shownote;   /* Causes cells with attached notes to be highlighted */

/*---------------- spreadsheet data ----------------*/

static inline cellref_t cellref(int row, int col) {
    cellref_t cell = { row, col, 0, 0 };
    return cell;
}

static inline cellref_t cellref1(int row, int col, int vf) {
    cellref_t cell = { row, col, 0, vf };
    return cell;
}

static inline rangeref_t rangeref(int r1, int c1, int r2, int c2) {
    rangeref_t range = { { r1, c1, 0, 0 }, { r2, c2, 0, 0 } };
    return range;
}

static inline rangeref_t rangeref1(int r1, int c1, int vf1, int r2, int c2, int vf2) {
    rangeref_t range = { { r1, c1, 0, vf1 }, { r2, c2, 0, vf2 } };
    return range;
}

static inline rangeref_t rangeref2(cellref_t left, cellref_t right) {
    rangeref_t range;
    range.left = left;
    range.right = right;
    return range;
}

static inline cellref_t cellref_current(sheet_t *sp) {
    return cellref(sp->currow, sp->curcol);
}

static inline rangeref_t rangeref_curcell(sheet_t *sp) {
    return rangeref(sp->currow, sp->curcol, sp->currow, sp->curcol);
}

static inline rangeref_t rangeref_current(sheet_t *sp) {
    if (sp->showrange) {
        sp->showrange = 0;
        return rangeref(sp->showsr, sp->showsc, sp->currow, sp->curcol);
    } else {
        return rangeref_curcell(sp);
    }
}

static inline rangeref_t rangeref_total(sheet_t *sp) {
    return rangeref(0, 0, sp->maxrow, sp->maxcol);
}

static inline rangeref_t rangeref_empty(void) {
    return rangeref(0, 0, -1, -1);
}

static inline int cell_in_range(cellref_t cr, rangeref_t rr) {
    return (cr.row >= rr.left.row && cr.row <= rr.right.row &&
            cr.col >= rr.left.col && cr.col <= rr.right.col);
}

extern rangeref_t *range_normalize(rangeref_t *rr);
extern sheet_t *sheet_init(sheet_t *sp); /* initialize settings to default values */
extern void erasedb(sheet_t *sp);
extern int load_scrc(sheet_t *sp);

/* check if the cell at r,c has a value */
/* p must be defined as struct ent *p; */
#define VALID_CELL(sp, p, r, c) ((p = getcell(sp, r, c)) && (p->type != SC_EMPTY))

/* a linked list of free [struct ent]'s, uses .next as the pointer */
extern struct ent *lookat(sheet_t *sp, int row, int col);  /* allocates the cell */
extern struct ent *getcell(sheet_t *sp, int row, int col); /* does not allocate the cell */
extern int checkbounds(sheet_t *sp, int *rowp, int *colp);
extern void clearent(struct ent *v);

/*---------------- expressions ----------------*/

extern int parse_line(const char *buf);
extern void parse_error(const char *err, const char *src, const char *src_pos);
extern void yyerror(const char *err);
extern int parse_cellref(const char *p, cellref_t *cp, int *lenp);
extern int parse_rangeref(const char *p0, rangeref_t *rp, int *lenp);
extern int yylex(void);
extern SCXMEM enode_t *new_op0(int op, int nargs);
extern SCXMEM enode_t *new_op1(int op, SCXMEM enode_t *a1);
extern SCXMEM enode_t *new_op1x(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2);
extern SCXMEM enode_t *new_op2(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2);
extern SCXMEM enode_t *new_op3(int op, SCXMEM enode_t *a1,
                               SCXMEM enode_t *a2, SCXMEM enode_t *a3);
extern SCXMEM enode_t *new_const(double v);
extern SCXMEM enode_t *new_error(int error);
extern SCXMEM enode_t *new_range(sheet_t *sp, rangeref_t rr);
extern SCXMEM enode_t *new_str(SCXMEM string_t *s);
extern SCXMEM enode_t *new_var(sheet_t *sp, cellref_t cr);
extern enode_t *copye(sheet_t *sp, enode_t *e, int Rdelta, int Cdelta,
                      int r1, int c1, int r2, int c2, int transpose);
#define DCP_DEFAULT    0
#define DCP_NO_NAME    1
#define DCP_NO_LOCALE  2
#define DCP_NO_EXPR    4
extern int decompile(sheet_t *sp, char *dest, size_t size, enode_t *e, int dr, int dc, int dcp_flags);
extern int decompile_expr(sheet_t *sp, buf_t buf, enode_t *e, int dr, int dc, int flags);
extern void efree(SCXMEM enode_t *e);
extern int buf_putvalue(buf_t buf, scvalue_t a);
extern void free_enode_list(void);

extern void EvalAll(sheet_t *sp);
extern scvalue_t eval_at(sheet_t *sp, enode_t *e, int row, int col);
extern SCXMEM string_t *seval_at(sheet_t *sp, enode_t *se, int row, int col, int *errp);
extern double neval_at(sheet_t *sp, enode_t *e, int row, int col, int *errp);

/*---------------- format and cell attributes ----------------*/

extern int engformat(char *buf, size_t size, int fmt, int lprecision, double val, int *alignp);
extern int format(char *buf, size_t buflen, const char *fmt, int lprecision, double val, int *alignp);
extern void cmd_format(sheet_t *sp, int c1, int c2, int w, int p, int r);
extern void cmd_setformat(sheet_t *sp, int n, SCXMEM string_t *str);
extern void format_cells(sheet_t *sp, rangeref_t rr, SCXMEM string_t *s);
extern void dohide(sheet_t *sp);
extern void hidecols(sheet_t *sp, int c1, int c2);
extern void hiderows(sheet_t *sp, int c1, int c2);
extern int locked_cell(sheet_t *sp, int row, int col);
extern void lock_cells(sheet_t *sp, rangeref_t rr);
extern void unlock_cells(sheet_t *sp, rangeref_t rr);
extern void showcol(sheet_t *sp, int c1, int c2);
extern void showrow(sheet_t *sp, int r1, int r2);
extern void range_align(sheet_t *sp, rangeref_t rr, int align);
extern void note_add(sheet_t *sp, cellref_t cr, rangeref_t rr);
extern void note_delete(sheet_t *sp, cellref_t cr);
extern const char *coltoa(int col);
extern const char *v_name(sheet_t *sp, int row, int col);
extern const char *r_name(sheet_t *sp, int r1, int c1, int r2, int c2);
extern int atocol(const char *s, int *lenp);
extern int rows_height(sheet_t *sp, int r, int n);
extern int cols_width(sheet_t *sp, int c, int n);

/*---------------- spreadsheet editing ----------------*/

extern void deletecols(sheet_t *sp, int c1, int c2);
extern void deleterows(sheet_t *sp, int r1, int r2);
extern void copy_set_source_range(rangeref_t rr);
#define COPY_FROM_RANGE   0x01
#define COPY_FROM_QBUF    0x02
#define COPY_FROM_DEF     0x04
extern void copy_range(sheet_t *sp, int flags, rangeref_t drr, rangeref_t srr);
extern void copyent(sheet_t *sp, struct ent *n, struct ent *p,
                    int dr, int dc, int r1, int c1, int r2, int c2, int transpose);
extern int dupcol(sheet_t *sp, cellref_t cr);
extern int duprow(sheet_t *sp, cellref_t cr);
extern void cmd_select_qbuf(char c);
extern int edit_cell(sheet_t *sp, buf_t buf, int row, int col, struct ent *p, int dcp_flags, int c0);
extern void erase_range(sheet_t *sp, rangeref_t rr);
extern void fill_range(sheet_t *sp, rangeref_t rr, double start, double inc, int bycols);
extern void free_ent_list(void);
extern void let(sheet_t *sp, cellref_t cr, SCXMEM enode_t *e, int align);
extern void unlet(sheet_t *sp, cellref_t cr);
extern int insertcols(sheet_t *sp, cellref_t cr, int arg, int delta);
extern int insertrows(sheet_t *sp, cellref_t cr, int arg, int delta);
extern void move_area(sheet_t *sp, int dr, int dc, rangeref_t rr);
extern void move_range(sheet_t *sp, cellref_t cr, rangeref_t rr);
extern void moveto(sheet_t *sp, rangeref_t rr, cellref_t st);
extern void cmd_pullcells(sheet_t *sp, int cmd, int uarg);
extern void sort_range(sheet_t *sp, rangeref_t rr, SCXMEM string_t *criteria);
extern void valueize_area(sheet_t *sp, rangeref_t rr);
extern void sync_ranges(sheet_t *sp);
extern void sync_refs(sheet_t *sp);
extern void yankcols(sheet_t *sp, int c1, int c2);
extern void yankrows(sheet_t *sp, int r1, int r2);
extern void yank_range(sheet_t *sp, rangeref_t rr);
extern int growtbl(sheet_t *sp, int rowcol, int toprow, int topcol);

/*---------------- spreadsheet options ----------------*/

extern void setautocalc(sheet_t *sp, int i);
extern void setiterations(sheet_t *sp, int i);
extern void setcalcorder(sheet_t *sp, int i);
extern void set_mdir(sheet_t *sp, SCXMEM string_t *str);
extern void set_autorun(sheet_t *sp, SCXMEM string_t *str);
extern void set_fkey(sheet_t *sp, int n, SCXMEM string_t *str);

/*---------------- styles ----------------*/

#define SC_COLOR_BLACK     0
#define SC_COLOR_RED       1
#define SC_COLOR_GREEN     2
#define SC_COLOR_YELLOW    3
#define SC_COLOR_BLUE      4
#define SC_COLOR_MAGENTA   5
#define SC_COLOR_CYAN      6
#define SC_COLOR_WHITE     7

extern SCXMEM struct colorpair *cpairs[CPAIRS + 1];
extern void change_color(sheet_t *sp, int pair, enode_t *e);
extern void initcolor(sheet_t *sp, int colornum);
extern void sc_setcolor(int set);
#define STYLE_NONE      0
#define STYLE_CELL      1
#define STYLE_NEG       2
#define STYLE_ERROR     3
#define STYLE_NOTE      4
#define STYLE_FRAME     5
#define STYLE_FRAME_CUR 6
#define STYLE_RANGE     5
extern int init_style(int n, int fg, int bg, enode_t *expr);
extern void select_style(int style, int rev);
extern void free_styles(void);
extern void colors_write(sheet_t *sp, FILE *f, int indent);

/*---------------- color ranges ----------------*/

extern int crange_test(sheet_t *sp);
extern void crange_add(sheet_t *sp, rangeref_t rr, int pair);
extern void crange_clean(sheet_t *sp);
extern void crange_delete(sheet_t *sp, struct crange *r);
extern struct crange *crange_find(sheet_t *sp, int row, int col);
extern void crange_list(sheet_t *sp, FILE *f);
extern void crange_write(sheet_t *sp, FILE *f);
extern void crange_sync(sheet_t *sp);
extern void crange_fix(sheet_t *sp, int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);

/*---------------- abbreviations ----------------*/

extern int abbrev_test(sheet_t *sp);
extern void abbrev_add(sheet_t *sp, SCXMEM string_t *name, SCXMEM string_t *exp);
extern void abbrev_clean(sheet_t *sp);
extern void abbrev_delete(sheet_t *sp, SCXMEM string_t *abbrev);
extern struct abbrev *abbrev_find(sheet_t *sp, const char *abbrev, int len, struct abbrev **prev);
extern void abbrev_list(sheet_t *sp, FILE *f);
extern void abbrev_write(sheet_t *sp, FILE *f);

/*---------------- named ranges ----------------*/

extern int nrange_test(sheet_t *sp);
extern void nrange_add(sheet_t *sp, SCXMEM string_t *name, rangeref_t rr, int is_range);
extern void nrange_clean(sheet_t *sp);
extern void nrange_delete(sheet_t *sp, rangeref_t rr);
extern int nrange_find_name(sheet_t *sp, const char *name, int len, struct nrange **rng);
extern void nrange_list(sheet_t *sp, FILE *f);
extern struct nrange *nrange_find_coords(sheet_t *sp, rangeref_t rr);
extern void nrange_sync(sheet_t *sp);
extern void nrange_write(sheet_t *sp, FILE *f);
extern void nrange_fix(sheet_t *sp, int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);

/*---------------- frame ranges ----------------*/

extern int frange_test(sheet_t *sp);
#define FRANGE_DIRECT  0
#define FRANGE_FIND    1
#define FRANGE_INNER   2
extern struct frange *frange_find(sheet_t *sp, int row, int col);
extern void frange_add(sheet_t *sp, int flags, rangeref_t orr, rangeref_t irr,
                       int toprows, int bottomrows, int leftcols, int rightcols);
extern void frange_clean(sheet_t *sp);
extern void frange_delete(sheet_t *sp, struct frange *r);
extern void frange_list(sheet_t *sp, FILE *fp);
extern void frange_sync(sheet_t *sp);
extern void frange_write(sheet_t *sp, FILE *f);
static inline struct frange *frange_get_current(sheet_t *sp) {
    return frange_find(sp, sp->currow, sp->curcol);
}
extern void frange_fix(sheet_t *sp, int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);

/*---------------- file reading and writing ----------------*/

extern FILE *openfile(char *fname, size_t fnamesiz, int *rpid, int *rfd);
extern void closefile(FILE *f, int pid, int rfd);
extern char *findhome(char *fname, size_t fnamesiz);
extern int backup_file(const char *path);
extern int creadfile(sheet_t *sp, const char *fname, int eraseflg);
extern int cwritefile(sheet_t *sp, const char *fname, rangeref_t rr, int dcp_flags);
extern int modcheck(sheet_t *sp, const char *endstr);
extern int readfile(sheet_t *sp, const char *fname, int eraseflg);
extern int writefile(sheet_t *sp, const char *fname, rangeref_t rr, int dcp_flags);
extern void printfile(sheet_t *sp, SCXMEM string_t *fname, rangeref_t rr);
extern void tblprintfile(sheet_t *sp, SCXMEM string_t *fname, rangeref_t rr);
extern void write_cells(sheet_t *sp, FILE *f, rangeref_t rr, cellref_t cr, int dcp_flags);
extern void write_fd(sheet_t *sp, FILE *f, rangeref_t rr, int dcp_flags);

/*---------------- navigation ----------------*/

extern void backpage(sheet_t *sp, int arg);
extern void backcell(sheet_t *sp, int arg);
extern void backcol(sheet_t *sp, int arg);
extern void backrow(sheet_t *sp, int arg);
extern void doend(sheet_t *sp, int rowinc, int colinc);
extern void forwpage(sheet_t *sp, int arg);
extern void forwcell(sheet_t *sp, int arg);
extern void forwcol(sheet_t *sp, int arg);
extern void forwrow(sheet_t *sp, int arg);
extern void go_free(sheet_t *sp);
extern void go_last(sheet_t *sp);
extern int num_search(sheet_t *sp, int g_type, rangeref_t rr, double n);
extern int str_search(sheet_t *sp, int g_type, rangeref_t rr, SCXMEM string_t *str);
extern void remember(sheet_t *sp, int save);
extern void gotonote(sheet_t *sp);

/*---------------- interaction ----------------*/

extern void vi_interaction(sheet_t *sp);
extern int yn_ask(const char *msg);
extern void help(int ctx);
extern void lotus_menu(void);
extern void cmd_recalc(sheet_t *sp);
extern void cmd_redraw(sheet_t *sp);
extern void cmd_run(SCXMEM string_t *str);
extern void sc_cmd_put(sheet_t *sp, const char *arg, int vopt);
extern void sc_cmd_write(const char *arg);
extern int query(sheet_t *sp, char *dest, int destsize, const char *s, const char *data);
extern void free_hist(void);
extern void read_hist(void);
extern void write_hist(void);
extern void sc_set_locale(int set);
extern int set_line(const char *fmt, ...) sc__attr_printf(1,2);

extern void signals(void);
extern sigret_t doquit(int i);
extern sigret_t time_out(int signo);
extern sigret_t dump_me(int i);
extern sigret_t nopipe(int i);
#ifdef SIGWINCH
extern sigret_t winchg(int i);
#endif

/*---------------- plugins ----------------*/

extern int cmd_plugin(sheet_t *sp, SCXMEM string_t *str);
extern void plugin_add(SCXMEM string_t *ext, SCXMEM string_t *plugin, char type);
extern int plugin_exists(const char *name, int len, char *path, size_t size);
extern char *plugin_find(const char *ext, char type);

/*---------------- piping commands ----------------*/

extern void cmd_getnum(sheet_t *sp, rangeref_t rr, int fd);
extern void cmd_fgetnum(sheet_t *sp, rangeref_t rr, int fd);
extern void cmd_getstring(sheet_t *sp, rangeref_t rr, int fd);
extern void cmd_getexp(sheet_t *sp, rangeref_t rr, int fd);
extern void cmd_getformat(sheet_t *sp, int col, int fd);
extern void cmd_getfmt(sheet_t *sp, rangeref_t rr, int fd);
extern void cmd_getframe(sheet_t *sp, int fd);
extern void cmd_getrange(sheet_t *sp, SCXMEM string_t *name, int fd);
extern void cmd_eval(sheet_t *sp, SCXMEM enode_t *e, SCXMEM string_t *fmt, int row, int col, int fd);
extern void cmd_seval(sheet_t *sp, SCXMEM enode_t *e, int row, int col, int fd);
extern void cmd_query(sheet_t *sp, SCXMEM string_t *s, SCXMEM string_t *data, int fd);
extern void cmd_getkey(sheet_t *sp, int fd);
extern void cmd_status(sheet_t *sp, int fd);
extern void cmd_whereami(sheet_t *sp, int fd);

/*---------------- display ----------------*/

extern void error(const char *fmt, ...) sc__attr_printf(1,2);
extern void fatal(const char *str);
extern void update(sheet_t *sp, int anychanged);
extern void repaint_cursor(sheet_t *sp, int set);

/*---------------- screen and input stuff ----------------*/

extern int usecurses;
extern int screen_COLS, screen_LINES;
#ifdef VMS
extern int VMS_read_raw;   /*sigh*/
#endif

extern void initkbd(void);
extern void kbd_again(void);
extern void resetkbd(void);

extern int nmgetch(int clearline);
extern int nmgetch_savepos(int clearline);
extern int nmungetch(int c);

extern void startdisp(void);
extern void stopdisp(void);

extern void screen_resize(sheet_t *sp);
extern void screen_pause(void);
extern void screen_rebuild(void);
extern void screen_erase(void);
extern void screen_refresh(void);
extern void screen_move(int y, int x);
extern void screen_clear_line(int y);
extern void screen_draw_page(int y, int x, const char * const *screen);
extern void screen_draw_line(int y, int x, const char *str);
extern void screen_init_pair(int n, int fg, int bg);
extern int screen_get_keyname(char *buf, size_t size, int c);
extern void screen_mouseon(void);
extern void screen_mouseoff(void);
extern int screen_getmouse(struct screen_mouse_event *event);
extern void screen_hidecursor(void);
extern int screen_draw_menu(int y, int x, struct menu_item const *menu, int option);
extern void screen_deraw(int ClearLastLine);
extern void screen_goraw(void);
