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
#include <stdio.h>      /* would be included by compat for curses anyway */
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

#define ATBL(tbl, row, col)     (&tbl[row][col])

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
#define DELBUFSIZE       40   /* Number of named buffers + 4 */

typedef unsigned char sc_bool_t;

extern void error(const char *fmt, ...) sc__attr_printf(1,2);
extern void fatal(const char *str);

#define FBUFLEN 1024    /* buffer size for a single field */
#define PATHLEN (PATH_MAX < 8192 ? 8192 : PATH_MAX) /* maximum path length */

#ifndef DFLT_PAGER
#define DFLT_PAGER "more"       /* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160      /* for ! command and commands that use the pager */

/* a cellref has a row, column, flags and sheet */
typedef struct cellref cellref_t;
struct cellref {
    int row;
    short col;
    unsigned char sheet;
    unsigned char vf;
};

#define FIX_ROW 1
#define FIX_COL 2

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

/* stores type of operation this cell will perform */
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

/* stores a range (left, right) */
struct nrange {
    struct ent_ptr r_left, r_right;
    SCXMEM string_t *r_name;            /* possible name for this range */
    struct nrange *r_next, *r_prev;     /* chained ranges */
    int r_is_range;
};

/* stores a framed range (left, right) */
struct frange {
    struct ent *or_left, *or_right;     /* outer range */
    struct ent *ir_left, *ir_right;     /* inner range */
    struct frange *r_next, *r_prev;     /* chained frame ranges */
};

/* stores a color range (left, right) */
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
/* singly linked list, sorted by name */
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
    double g_n;
    SCXMEM string_t *g_s;
    rangeref_t g_rr;
    cellref_t st;
    int stflag;
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

/*---------------- curses stuff ----------------*/

#define CLEAR_LINE error("%s", "") /* suppress warning on NetBSD curses */

#ifndef A_CHARTEXT      /* Should be defined in curses.h */
#define A_CHARTEXT 0xff
#endif

#ifndef color_set
#define color_set(c, o)         attron(COLOR_PAIR(c))
#endif

#if !defined(HAVE_ATTR_T) && defined(_COMPAT_H) /* Not defined for psc */
typedef chtype attr_t;
#endif

#if !defined(HAVE_ATTR_GET) && !defined(NO_ATTR_GET)
#define attr_get(a, p, o)       ((void)((a) != 0 && (*(a) = stdscr->_attrs)), \
                                (void)((p) != 0 && \
                                (*(p) = PAIR_NUMBER(stdscr->_attrs))), OK)
#endif

#if defined BSD42 || defined SYSIII
# ifndef cbreak
# define cbreak      crmode
# define nocbreak    nocrmode
# endif
#endif

/*---------------- keyboard input stuff ----------------*/

#define ctl(c) ((c)&037)
#define ESC 033
#define DEL 0177

/*
 * there seems to be some question about what to do w/ the iscntrl
 * some BSD systems are reportedly broken as well...
 *
 * CG: to settle this issue once and for all, we cannot use
 * iscntrl() with an argument outside the range EOF..0xFF.
 * Furthermore `ctl(c)` and ESC and DEL are defined explicitly
 * so let's define ISCTL(c) consistently and test for DEL explicitly
 * wherever it is required.
 */
#define ISCTL(c)  (!((c) & ~0x1F))
#define ISBYTE(c) (!((c) & ~0xFF))

#define KEY_ALT(c)   ((c)|01000)
extern int nmgetch(int clearline);

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

extern void help(int ctx);

/*---------------- Global data ----------------*/

/* The table data is organized as:
   `tbl`: a pointer to an array of `maxrows` pointers to rows
   each row pointer points to an array of `maxcols` pointers to cells
   these cell pointers can be NULL or point to an allocated `ent` structure
   This design is suboptimal in terms of memory space and implies much
   dreaded three star programming.
 */
extern SCXMEM struct ent ***tbl;       /* data table ref. in vmtbl.c and ATBL() */

extern char curfile[PATHLEN];
extern int strow, stcol;
extern int currow, curcol;
extern int rescol;              /* columns reserved for row numbers */
extern cellref_t savedcr[37];
extern cellref_t savedst[37];
extern int FullUpdate;
extern int maxrow, maxcol;
extern int maxrows, maxcols;    /* # cells currently allocated */
extern int rowsinrange;         /* Number of rows in target range of a goto */
extern int colsinrange;         /* Number of cols in target range of a goto */
extern SCXMEM int *fwidth;
extern SCXMEM int *precision;
extern SCXMEM int *realfmt;
extern SCXMEM string_t *colformat[COLFORMATS];
extern SCXMEM unsigned char *col_hidden;
extern SCXMEM unsigned char *row_hidden;
extern char line[FBUFLEN];
extern int linelim;
extern int changed;
/* temporary sheet fragments: stack of 4 work buffers and 36 named buffers (a-z,0-9) */
extern SCXMEM struct ent *delbuf[DELBUFSIZE];
extern SCXMEM unsigned char *delbuffmt[DELBUFSIZE];
extern int dbidx;
extern int qbuf;                /* buffer no. specified by `"' command */
extern int showsc, showsr;
extern int showrange;           /* Causes ranges to be highlighted */
extern int macrofd;
extern int cslop;
extern int usecurses;
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

static inline cellref_t cellref_current(void) {
    return cellref(currow, curcol);
}

static inline rangeref_t rangeref_current(void) {
    if (showrange) {
        showrange = 0;
        return rangeref(showsr, showsc, currow, curcol);
    } else {
        return rangeref(currow, curcol, currow, curcol);
    }
}

static inline rangeref_t rangeref_total(void) {
    return rangeref(0, 0, maxrow, maxcol);
}

static inline rangeref_t rangeref_empty(void) {
    return rangeref(0, 0, -1, -1);
}

rangeref_t *range_normalize(rangeref_t *rr);

/* check if the cell at r,c has a value */
/* p must be defined as struct ent *p; */
#define VALID_CELL(p, r, c) ((p = *ATBL(tbl, r, c)) && (p->type != SC_EMPTY))

/* styles */
extern SCXMEM struct colorpair *cpairs[CPAIRS + 1];
extern int are_colors(void);
extern void change_color(int pair, enode_t *e);
extern void initcolor(int colornum);
extern void list_colors(FILE *f);
extern void write_colors(FILE *f, int indent);
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

extern FILE *openfile(char *fname, size_t fnamesiz, int *rpid, int *rfd);
extern char *findhome(char *fname, size_t fnamesiz);
extern char *findplugin(const char *ext, char type);
extern const char *coltoa(int col);
extern const char *v_name(int row, int col);
extern const char *r_name(int r1, int c1, int r2, int c2);
extern scvalue_t eval_at(enode_t *e, int row, int col);
extern SCXMEM string_t *seval_at(enode_t *se, int row, int col, int *errp);
extern double neval_at(enode_t *e, int row, int col, int *errp);
extern int are_frames(void);
extern int are_nranges(void);
extern int atocol(const char *s, int *lenp);
extern int creadfile(const char *fname, int eraseflg);
extern int cwritefile(const char *fname, rangeref_t rr, int dcp_flags);
extern int engformat(char *buf, size_t size, int fmt, int lprecision, double val, int *alignp);
extern int find_nrange_name(const char *name, int len, struct nrange **rng);
struct nrange *find_nrange_coords(rangeref_t rr);
extern int format(char *buf, size_t buflen, const char *fmt, int lprecision, double val, int *alignp);
extern int growtbl(int rowcol, int toprow, int topcol);
extern int locked_cell(int row, int col);
extern int modcheck(const char *endstr);
extern int plugin_exists(const char *name, int len, char *path, size_t size);
extern int readfile(const char *fname, int eraseflg);
extern int writefile(const char *fname, rangeref_t rr, int dcp_flags);
extern int yn_ask(const char *msg);
extern struct abbrev *find_abbr(const char *abbrev, int len, struct abbrev **prev);
extern enode_t *copye(enode_t *e, int Rdelta, int Cdelta,
                      int r1, int c1, int r2, int c2, int transpose);

extern SCXMEM enode_t *new_op0(int op, int nargs);
extern SCXMEM enode_t *new_op1(int op, SCXMEM enode_t *a1);
extern SCXMEM enode_t *new_op1x(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2);
extern SCXMEM enode_t *new_op2(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2);
extern SCXMEM enode_t *new_op3(int op, SCXMEM enode_t *a1,
                               SCXMEM enode_t *a2, SCXMEM enode_t *a3);
extern SCXMEM enode_t *new_const(double v);
extern SCXMEM enode_t *new_error(int error);
extern SCXMEM enode_t *new_range(rangeref_t rr);
extern SCXMEM enode_t *new_str(SCXMEM string_t *s);
extern SCXMEM enode_t *new_var(cellref_t cr);
/* a linked list of free [struct ent]'s, uses .next as the pointer */
extern struct ent *lookat(int row, int col);    /* allocates the cell */
extern struct ent *lookat_nc(int row, int col); /* does not allocate the cell */
extern struct crange *find_crange(int row, int col);
extern struct frange *find_frange(int row, int col);
extern void EvalAll(void);
extern void add_crange(rangeref_t rr, int pair);
extern void del_crange(struct crange *r);
#define FRANGE_DIRECT  0
#define FRANGE_FIND    1
#define FRANGE_INNER   2
extern void add_frange(int flags, rangeref_t orr, rangeref_t irr,
                       int toprows, int bottomrows, int leftcols, int rightcols);
extern void del_frange(struct frange *r);
extern void add_nrange(SCXMEM string_t *name, rangeref_t rr, int is_range);
extern void add_plugin(SCXMEM string_t *ext, SCXMEM string_t *plugin, char type);
extern void backpage(int arg);
extern void backcell(int arg);
extern void backcol(int arg);
extern void backrow(int arg);
extern void checkbounds(int *rowp, int *colp);
extern void clearent(struct ent *v);
extern void clean_crange(void);
extern void clean_frange(void);
extern void clean_nrange(void);
extern void deletecols(int c1, int c2);
extern void closefile(FILE *f, int pid, int rfd);
extern void closerow(int r, int numrow);
extern void deleterows(int r1, int r2);
extern void copy_set_source_range(int r1, int c1, int r2, int c2);
#define COPY_FROM_RANGE   0x01
#define COPY_FROM_QBUF    0x02
#define COPY_FROM_DEF     0x04
extern void copy(int flags, rangeref_t drr, rangeref_t srr);
extern void copyent(struct ent *n, struct ent *p,
                    int dr, int dc, int r1, int c1, int r2, int c2, int transpose);
// XXX: should pass a context with a cell reference
#define DCP_DEFAULT    0
#define DCP_NO_NAME    1
#define DCP_NO_LOCALE  2
#define DCP_NO_EXPR    4
extern int decompile(char *dest, size_t size, enode_t *e, int dr, int dc, int dcp_flags);
// XXX: should pass a context with a cell reference
extern int decompile_expr(buf_t buf, enode_t *e, int dr, int dc, int flags);
extern void del_nrange(rangeref_t rr);
extern void del_abbr(SCXMEM string_t *abbrev);
extern void deraw(int ClearLastLine);
extern void doend(int rowinc, int colinc);
extern void cmd_format(int c1, int c2, int w, int p, int r);
extern void cmd_setformat(int n, SCXMEM string_t *str);
extern void cmd_redraw(void);
extern void cmd_select_qbuf(char c);
extern void cmd_whereami(int fd);
extern int dupcol(cellref_t cr);
extern int duprow(cellref_t cr);
extern void cmd_query(SCXMEM string_t *s, SCXMEM string_t *data, int fd);
extern void cmd_status(int fd);
extern int cmd_plugin(SCXMEM string_t *str);
extern void set_mdir(SCXMEM string_t *str);
extern void set_autorun(SCXMEM string_t *str);
extern void set_fkey(int n, SCXMEM string_t *str);
extern void cmd_recalc(void);
extern void cmd_run(SCXMEM string_t *str);
extern int edits(buf_t buf, int row, int col, struct ent *p, int dcp_flags);
extern int editv(buf_t buf, int row, int col, struct ent *p, int dcp_flags);
extern void efree(SCXMEM enode_t *e);
extern void free_enode_list(void);
extern void erase_area(int idx, int sr, int sc, int er, int ec, int ignorelock);
extern void erasedb(int load_scrc);
extern void eraser(rangeref_t rr);
extern void fgetnum(rangeref_t rr, int fd);
extern void fillr(rangeref_t rr, double start, double inc, int bycols);
extern void fix_colors(int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);
extern void fix_frames(int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);
extern void fix_ranges(int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);
extern void free_ent_list(void);
extern int flush_saved(int idx);  /* free delbuf[idx] */
extern void format_cells(rangeref_t rr, SCXMEM string_t *s);
extern void forwpage(int arg);
extern void forwcell(int arg);
extern void forwcol(int arg);
extern void forwrow(int arg);
extern void getexp(rangeref_t rr, int fd);
extern void getfmt(rangeref_t rr, int fd);
extern void getformat(int col, int fd);
extern void getnum(rangeref_t rr, int fd);
extern void getstring(rangeref_t rr, int fd);
extern void go_last(void);
extern void goraw(void);
extern void dohide(void);
extern void hidecols(int c1, int c2);
extern void hiderows(int c1, int c2);
extern void initkbd(void);
extern int insertcol(cellref_t cr, int arg, int delta);
extern int insertrow(cellref_t cr, int arg, int delta);
extern void kbd_again(void);
extern void unlet(cellref_t cr);
extern void let(cellref_t cr, SCXMEM enode_t *e, int align);
extern void list_nranges(FILE *f);
extern void lock_cells(rangeref_t rr);
extern void move_area(int dr, int dc, rangeref_t rr);
extern void mover(cellref_t cr, rangeref_t rr);
extern void moveto(rangeref_t rr, cellref_t st);
extern int num_search(int g_type, rangeref_t rr, double n);
extern void printfile(SCXMEM string_t *fname, rangeref_t rr);
extern void pullcells(int to_insert, cellref_t cr);
extern int query(char *dest, int destsize, const char *s, const char *data);
extern void free_hist(void);
extern void read_hist(void);
extern void remember(int save);
extern void resetkbd(void);
extern void setautocalc(int i);
extern void setiterations(int i);
extern void setcalcorder(int i);

extern void showcol(int c1, int c2);
extern void showrow(int r1, int r2);
extern void signals(void);
extern void sortrange(rangeref_t rr, SCXMEM string_t *criteria);
extern void startdisp(void);
extern void stopdisp(void);
extern int str_search(int g_type, rangeref_t rr, SCXMEM string_t *str);
extern void sync_cranges(void);
extern void sync_franges(void);
extern void sync_nranges(void);
extern void sync_ranges(void);
extern void sync_refs(void);
extern void tblprintfile(SCXMEM string_t *fname, rangeref_t rr);
extern void unlock_cells(rangeref_t rr);
extern void update(int anychanged);
extern void valueize_area(rangeref_t rr);
extern void write_abbrevs(FILE *f);
extern void clean_abbrevs(void);
extern void write_cells(FILE *f, rangeref_t rr, cellref_t cr, int dcp_flags);
extern void write_cranges(FILE *f);
extern void write_fd(FILE *f, rangeref_t rr, int dcp_flags);
extern void write_franges(FILE *f);
extern void write_hist(void);
extern void write_nranges(FILE *f);
extern void yank_area(rangeref_t rr);
extern int parse_line(const char *buf);
extern void parse_error(const char *err, const char *src, int pos);
extern void yyerror(const char *err);
extern int parse_cellref(const char *p, cellref_t *cp, int *lenp);
extern int parse_rangeref(const char *p0, rangeref_t *rp, int *lenp);
extern int yylex(void);
extern int backup_file(const char *path);
extern void sc_set_locale(int set);
extern int set_line(const char *fmt, ...) sc__attr_printf(1,2);
extern int rows_height(int r, int n);
extern int cols_width(int c, int n);

static inline struct frange *get_current_frange(void) {
    return find_frange(currow, curcol);
}

extern int modflg;
#ifndef NOCRYPT
extern int Crypt;
#endif
extern SCXMEM string_t *mdir;
extern SCXMEM string_t *autorun;
extern int skipautorun;
extern SCXMEM string_t *fkey[FKEYS];
extern int scrc;
extern double prescale;
extern int extfunc;
extern int propagation;
extern int calc_order;
extern int autocalc;
extern int autolabel;
extern int autoinsert;
extern int autowrap;
extern int optimize;
extern int numeric;
extern int showcell;
extern int showtop;
extern int color;
extern int colorneg;
extern int colorerr;
extern int braille;
extern int braillealt;
extern int dobackups;
extern int loading;
extern int tbl_style;
extern int rndtoeven;
extern int craction;
extern int pagesize;  /* If nonzero, use instead of 1/2 screen height */
extern int rowlimit;
extern int collimit;

extern char revmsg[80];
extern int showneed;   /* Causes cells needing values to be highlighted */
extern int showexpr;   /* Causes cell exprs to be displayed, highlighted */
extern int shownote;   /* Causes cells with attached notes to be highlighted */
#ifdef VMS
extern int VMS_read_raw;   /*sigh*/
#endif

extern void gotonote(void);
extern void addnote(cellref_t cr, rangeref_t rr);
extern void delnote(cellref_t cr);
extern void range_align(rangeref_t rr, int align);
extern void yankcols(int c1, int c2);
extern void yankrows(int r1, int r2);
extern void list_frames(FILE *fp);
extern void yankr(rangeref_t rr);
extern void dogetkey(int fd);
extern void cmd_seval(SCXMEM enode_t *e, int row, int col, int fd);
extern void cmd_eval(SCXMEM enode_t *e, SCXMEM string_t *fmt, int row, int col, int fd);
extern void getrange(SCXMEM string_t *name, int fd);
extern void getframe(int fd);
extern void add_abbr(SCXMEM string_t *name, SCXMEM string_t *exp);
extern void repaint_cursor(int set);
extern sigret_t doquit(int i);
extern sigret_t time_out(int signo);
extern sigret_t dump_me(int i);
extern sigret_t nopipe(int i);
#ifdef SIGWINCH
extern sigret_t winchg(int i);
#endif
extern void mouseon(void);
extern void mouseoff(void);
extern void hidecursor(void);
extern void vi_interaction(void);
extern void sc_cmd_put(const char *arg, int vopt);
extern void sc_cmd_write(const char *arg);
extern void lotus_menu(void);
