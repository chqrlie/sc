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

#if (defined(__GNUC__) || defined(__TINYC__))
/* make sure that the keyword is not disabled by glibc (TINYC case) */
#define sc__attr_printf(a, b)  __attribute__((format(printf, a, b)))
#else
#define sc__attr_printf(a, b)
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(strrchr)
#define strrchr rindex
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(strchr)
#define strchr index
#endif

#ifdef SYSV4
size_t strlen(const char *s);
#endif

#ifndef FALSE
# define        FALSE   0
# define        TRUE    1
#endif /* !FALSE */

#ifdef SIGVOID
#define sigret_t void
#else
#define sigret_t int
#endif

#if defined(BSD42) || defined(BSD43) && !defined(ultrix)
#define memcpy(dest, source, len)       bcopy(source, dest, (unsigned int)len);
#define memset(dest, zero, len)         bzero((dest), (unsigned int)(len));
#else
#include <memory.h>
#endif

#ifndef HAVE_ISFINITE
#define isfinite(v)  finite(v)
#endif

extern const char *progname;

#ifndef CRYPT_PATH
#define NOCRYPT
#endif

/*---------------- Spreadsheet data ----------------*/

#define ATBL(tbl, row, col)     (&tbl[row][col])

#define MINROWS 100     /* minimum size at startup */
#define MINCOLS 30
#define ABSMAXCOLS 702  /* absolute cols: ZZ (base 26) */

#define CRROWS 1
#define CRCOLS 2
#define RESROW 3 /* rows reserved for prompt, error, and column numbers */

/* formats for engformat() */
#define REFMTFIX        0
#define REFMTFLT        1
#define REFMTENG        2
#define REFMTDATE       3
#define REFMTLDATE      4

#define DEFWIDTH 10     /* Default column width and precision */
#define DEFPREC   2
#define DEFREFMT  REFMTFIX /* Make default format fixed point  THA 10/14/90 */

#define FKEYS            24     /* Number of function keys available */
#define HISTLEN         100     /* Number of history entries for vi emulation */
#define CPAIRS            8     /* Number of color pairs available */
#define COLFORMATS       10     /* Number of custom column formats */
#define DELBUFSIZE       40     /* Number of named buffers + 4 */

extern void error(const char *fmt, ...) sc__attr_printf(1,2);
extern void fatal(const char *str);

#define FBUFLEN 1024    /* buffer size for a single field */
#define PATHLEN (PATH_MAX < 8192 ? 8192 : PATH_MAX) /* maximum path length */

#ifndef DFLT_PAGER
#define DFLT_PAGER "more"       /* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160      /* for ! command and commands that use the pager */

#define SCXMEM          /* flag allocated pointers with this */

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

/*
 * Some not too obvious things about the flags:
 *    IS_VALID means there is a valid number in v.
 *    IS_LOCKED means that the cell cannot be edited.
 *    IS_LABEL set means it points to a valid constant string.
 *    IS_STREXPR set means expr yields a string expression.
 *    If IS_STREXPR is not set, and expr points to an expression tree, the
 *        expression yields a numeric expression.
 *    So, either v or label can be set to a constant.
 *        Either (but not both at the same time) can be set from an expression.
 */

#define VALID_CELL(p, r, c) ((p = *ATBL(tbl, r, c)) && \
                             ((p->flags & IS_VALID) || p->label))

/* info for each cell, only alloc'd when something is stored in a cell */
struct ent {
    double v;                   /* v && label are set in EvalAll() */
    SCXMEM char *label;         /* cell string value */
    SCXMEM struct enode *expr;  /* cell formula */
    SCXMEM char *format;        /* printf format for this cell */
    char cellerror;             /* error in a cell? (should pack with flags) */
    short flags;
    short col, ncol, nlastcol;  /* the cell col/row */
    int row, nrow, nlastrow;    /* ncol/nrow:nlastcol/nlastrow link to note */
    struct ent *next;           /* next deleted ent (pulled, deleted cells) */
};

#define FIX_ROW 1
#define FIX_COL 2

/* stores type of operation this cell will perform */
struct enode {
    int op;
    union {
        int gram_match;         /* some compilers (hp9000ipc) need this */
        double k;               /* constant # */
        struct ent_ptr v;       /* ref. another cell */
        struct range_s r;       /* op is on a range */
        SCXMEM char *s;         /* op is a string constant */
        struct {                /* other cells use to eval()/seval() */
            SCXMEM struct enode *left, *right;
            SCXMEM char *s;     /* previous value of @ext function in case */
        } o;                    /*      external functions are turned off */
    } e;
};

/* stores a range (left, right) */
struct range {
    struct ent_ptr r_left, r_right;
    SCXMEM char *r_name;                /* possible name for this range */
    struct range *r_next, *r_prev;      /* chained ranges */
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
    struct enode *expr;
};

/* stores an abbreviation and its expansion */
/* singly linked list, sorted by name */
struct abbrev {
    SCXMEM char *name;
    SCXMEM char *exp;
    SCXMEM struct abbrev *next;
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
    SCXMEM char *g_s;
    int g_row;
    int g_col;
    int g_lastrow;
    int g_lastcol;
    int strow;
    int stcol;
    int stflag;
    int errsearch;
};

/* op values */
#define O_VAR 'v'
#define O_CONST 'k'
#define O_ECONST 'E'    /* constant cell w/ an error */
#define O_SCONST '$'
#define REDUCE 0200     /* Or'ed into OP if operand is a range */

#define OP_BASE 256
#define ACOS      (OP_BASE + 0)
#define ASIN      (OP_BASE + 1)
#define ATAN      (OP_BASE + 2)
#define CEIL      (OP_BASE + 3)
#define COS       (OP_BASE + 4)
#define EXP       (OP_BASE + 5)
#define FABS      (OP_BASE + 6)
#define FLOOR     (OP_BASE + 7)
#define HYPOT     (OP_BASE + 8)
#define LOG       (OP_BASE + 9)
#define LOG10     (OP_BASE + 10)
#define POW       (OP_BASE + 11)
#define SIN       (OP_BASE + 12)
#define SQRT      (OP_BASE + 13)
#define TAN       (OP_BASE + 14)
#define DTR       (OP_BASE + 15)
#define RTD       (OP_BASE + 16)
#define SUM       (OP_BASE + 17)
#define PROD      (OP_BASE + 18)
#define AVG       (OP_BASE + 19)
#define COUNT     (OP_BASE + 20)
#define STDDEV    (OP_BASE + 21)
#define MAX       (OP_BASE + 22)
#define MIN       (OP_BASE + 23)
#define RND       (OP_BASE + 24)
#define HOUR      (OP_BASE + 25)
#define MINUTE    (OP_BASE + 26)
#define SECOND    (OP_BASE + 27)
#define MONTH     (OP_BASE + 28)
#define DAY       (OP_BASE + 29)
#define YEAR      (OP_BASE + 30)
#define NOW       (OP_BASE + 31)
#define DATE      (OP_BASE + 32)
#define FMT       (OP_BASE + 33)
#define SUBSTR    (OP_BASE + 34)
#define STON      (OP_BASE + 35)
#define EQS       (OP_BASE + 36)
#define EXT       (OP_BASE + 37)
#define ELIST     (OP_BASE + 38)    /* List of expressions */
#define LMAX      (OP_BASE + 39)
#define LMIN      (OP_BASE + 40)
#define NVAL      (OP_BASE + 41)
#define SVAL      (OP_BASE + 42)
#define PV        (OP_BASE + 43)
#define FV        (OP_BASE + 44)
#define PMT       (OP_BASE + 45)
#define STINDEX   (OP_BASE + 46)
#define LOOKUP    (OP_BASE + 47)
#define ATAN2     (OP_BASE + 48)
#define INDEX     (OP_BASE + 49)
#define DTS       (OP_BASE + 50)
#define TTS       (OP_BASE + 51)
#define ABS       (OP_BASE + 52)
#define HLOOKUP   (OP_BASE + 53)
#define VLOOKUP   (OP_BASE + 54)
#define ROUND     (OP_BASE + 55)
#define IF        (OP_BASE + 56)
#define FILENAME  (OP_BASE + 57)
#define MYROW     (OP_BASE + 58)
#define MYCOL     (OP_BASE + 59)
#define LASTROW   (OP_BASE + 60)
#define LASTCOL   (OP_BASE + 61)
#define COLTOA    (OP_BASE + 62)
#define UPPER     (OP_BASE + 63)
#define LOWER     (OP_BASE + 64)
#define CAPITAL   (OP_BASE + 65)
#define NUMITER   (OP_BASE + 66)
#define ERR_      (OP_BASE + 67)
#define PI_       (OP_BASE + 68)
#define BLACK     (OP_BASE + 69)
#define RED       (OP_BASE + 70)
#define GREEN     (OP_BASE + 71)
#define YELLOW    (OP_BASE + 72)
#define BLUE      (OP_BASE + 73)
#define MAGENTA   (OP_BASE + 74)
#define CYAN      (OP_BASE + 75)
#define WHITE     (OP_BASE + 76)
#define RAND      (OP_BASE + 77)
#define RANDBETWEEN  (OP_BASE + 78)

/* flag values (9 bits) */
#define IS_VALID     0001
#define IS_CHANGED   0002
#define IS_STREXPR   0004
#define IS_LEFTFLUSH 0010
#define IS_DELETED   0020
#define IS_LOCKED    0040
#define IS_LABEL     0100
#define IS_CLEARED   0200
#define MAY_SYNC     0400

/* cell error (1st generation (ERROR) or 2nd+ (INVALID)) */
#define CELLOK          0
#define CELLERROR       1
#define CELLINVALID     2

/* calculation order */
#define BYCOLS 1
#define BYROWS 2

/* values for showrange for ranges of rows or columns */
#define SHOWROWS 2
#define SHOWCOLS 3

/* tblprint style output for: */
#define TBL     1               /* 'tbl' */
#define LATEX   2               /* 'LaTeX' */
#define TEX     3               /* 'TeX' */
#define SLATEX  4               /* 'SLaTeX' (Scandinavian LaTeX) */
#define FRAME   5               /* tblprint style output for FrameMaker */

/* Types for etype() */
#define NUM     1
#define STR     2

#define GROWAMT 30      /* default minimum amount to grow */

#define GROWNEW     1       /* first time table */
#define GROWROW     2       /* add rows */
#define GROWCOL     4       /* add columns */
#define GROWBOTH    6       /* grow both */

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

/* use this function to ensure consistency for string arguments */
static inline const char *s2c(char *p) { return p; }

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
extern struct ent ***tbl;       /* data table ref. in vmtbl.c and ATBL() */

extern char curfile[PATHLEN];
extern int strow, stcol;
extern int currow, curcol;
extern int gmyrow, gmycol;      /* globals used for @myrow, @mycol cmds */
extern int rescol;              /* columns reserved for row numbers */
extern int savedrow[37], savedcol[37];
extern int savedstrow[37], savedstcol[37];
extern int FullUpdate;
extern int maxrow, maxcol;
extern int maxrows, maxcols;    /* # cells currently allocated */
extern int rowsinrange;         /* Number of rows in target range of a goto */
extern int colsinrange;         /* Number of cols in target range of a goto */
extern SCXMEM int *fwidth;
extern SCXMEM int *precision;
extern SCXMEM int *realfmt;
extern SCXMEM char *colformat[10];
extern SCXMEM unsigned char *col_hidden;
extern SCXMEM unsigned char *row_hidden;
extern char line[FBUFLEN];
extern ssize_t linelim;
extern int changed;
extern SCXMEM struct ent *delbuf[DELBUFSIZE];
extern SCXMEM char *delbuffmt[DELBUFSIZE];
extern int dbidx;
extern int qbuf;                /* buffer no. specified by `"' command */
extern int showsc, showsr;
extern int showrange;           /* Causes ranges to be highlighted */
extern int cellassign;
extern int macrofd;
extern int cslop;
extern int usecurses;
extern int brokenpipe;          /* Set to true if SIGPIPE is received */
extern char dpoint;     /* country-dependent decimal point from locale */
extern char thsep;      /* country-dependent thousands separator from locale */
extern char histfile[PATHLEN];
extern int lcols;
extern int lastendrow;          /* Last bottom row of screen */
extern struct frange *lastfr;   /* Last framed range we were in */
extern int framerows;           /* Rows in current frame */
extern int framecols;           /* Columns in current frame */
extern char search_ind;         /* Search indicator */
extern char mode_ind;           /* Mode indicator */
extern int seenerr;
extern int emacs_bindings;      /* use emacs-like bindings */
/* a linked list of free [struct enodes]'s, uses .e.o.left as the next pointer */
extern SCXMEM struct enode *freeenodes;
extern bool sc_decimal;     /* Set if there was a decimal point in the number */
extern SCXMEM char *scext;
extern SCXMEM char *ascext;
extern SCXMEM char *tbl0ext;
extern SCXMEM char *tblext;
extern SCXMEM char *latexext;
extern SCXMEM char *slatexext;
extern SCXMEM char *texext;
extern int Vopt;
extern struct go_save gs;

/* memory allocation */
extern SCXMEM void *scxmalloc(size_t n);
extern SCXMEM void *scxrealloc(SCXMEM void *ptr, size_t n);
extern SCXMEM char *scxdup(const char *s);
extern void scxfree(SCXMEM void *p);
extern char *set_string(SCXMEM char **pp, SCXMEM char *s);
extern char *set_cstring(SCXMEM char **pp, const char *s);

/* styles */
extern struct SCXMEM colorpair *cpairs[CPAIRS + 1];
extern int are_colors(void);
extern void change_color(int pair, struct enode *e);
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
extern int init_style(int n, int fg, int bg, struct enode *expr);
extern void select_style(int style, int rev);

extern FILE *openfile(char *fname, size_t fnamesiz, int *rpid, int *rfd);
extern char *findhome(char *fname, size_t fnamesiz);
extern char *findplugin(const char *ext, char type);
extern char *coltoa(int col);
extern char *v_name(int row, int col);
extern char *r_name(int r1, int c1, int r2, int c2);
extern SCXMEM char *seval(struct enode *se);
extern double eval(struct enode *e);
extern int are_frames(void);
extern int are_ranges(void);
extern int atocol(const char *string, int len);
extern int creadfile(const char *save, int eraseflg);
extern int cwritefilec(const char *fname, int r0, int c0, int rn, int cn);
extern bool engformat(int fmt, int width, int lprecision, double val,
                      char *buf, int buflen);
extern int etype(struct enode *e);
extern int find_range_name(const char *name, int len, struct range **rng);
struct range *find_range_coords(const struct ent *lmatch, const struct ent *rmatch);
extern bool format(const char *fmt, int lprecision, double val, char *buf, size_t buflen);
extern int growtbl(int rowcol, int toprow, int topcol);
extern int locked_cell(int r, int c);
extern int modcheck(const char *endstr);
extern int plugin_exists(const char *name, int len, char *path, size_t size);
extern int readfile(const char *fname, int eraseflg);
extern int writefile(const char *fname, int r0, int c0, int rn, int cn);
extern int yn_ask(const char *msg);
extern struct abbrev *find_abbr(const char *abbrev, int len, struct abbrev **prev);
extern struct enode *copye(struct enode *e, int Rdelta, int Cdelta,
                           int r1, int c1, int r2, int c2, int transpose);

extern SCXMEM struct enode *new(int op, SCXMEM struct enode *a1, SCXMEM struct enode *a2);
extern SCXMEM struct enode *new_const(int op, double a1);
extern SCXMEM struct enode *new_range(int op, struct range_s a1);
extern SCXMEM struct enode *new_str(SCXMEM char *s);
extern SCXMEM struct enode *new_var(int op, struct ent_ptr a1);
/* a linked list of free [struct ent]'s, uses .next as the pointer */
extern SCXMEM struct ent *freeents;
extern struct ent *lookat(int row, int col);
extern struct crange *find_crange(int row, int col);
extern struct frange *find_frange(int row, int col);
extern void EvalAll(void);
extern void add_crange(struct ent *r_left, struct ent *r_right, int pair);
extern void add_frange(struct ent *or_left, struct ent *or_right,
                       struct ent *ir_left, struct ent *ir_right,
                       int toprows, int bottomrows,
                       int leftcols, int rightcols);
extern void add_range(const char *name, struct ent_ptr left, struct ent_ptr right,
                      int is_range);
extern void addplugin(const char *ext, const char *plugin, char type);
extern void backcol(int arg);
extern void backrow(int arg);
extern void checkbounds(int *rowp, int *colp);
extern void clearent(struct ent *v);
extern void clean_crange(void);
extern void clean_frange(void);
extern void clean_range(void);
extern void deletecols(int c1, int c2);
extern void closefile(FILE *f, int pid, int rfd);
extern void closerow(int r, int numrow);
extern void deleterows(int r1, int r2);
extern void copy(struct ent *dv1, struct ent *dv2, struct ent *v1, struct ent *v2);
extern void docopy(void);
extern void copyent(struct ent *n, struct ent *p,
                    int dr, int dc, int r1, int c1, int r2, int c2, int transpose);
extern void decompile(struct enode *e, int priority);
extern void del_range(struct ent *left, struct ent *right);
extern void del_abbr(const char *abbrev);
extern void deraw(int ClearLastLine);
extern void doend(int rowinc, int colinc);
extern void doformat(int c1, int c2, int w, int p, int r);
extern void doredraw(void);
extern void doselect(char c);
extern void dowhereami(int fd);
extern void dupcol(void);
extern void duprow(void);
extern void doquery(const char *s, const char *data, int fd);
extern void dostat(int fd);
extern int doplugin(const char *str);
extern void domdir(const char *str);
extern void doautorun(const char *str);
extern void dofkey(int n, const char *str);
extern void dohistfile(const char *str);
extern void dosetformat(int n, const char *str);
extern void dorun(const char *str);
extern void dodefine(const char *name);
extern void editfmt(int row, int col);
extern void edits(int row, int col);
extern void editv(int row, int col);
extern void efree(SCXMEM struct enode *e);
extern void erase_area(int sr, int sc, int er, int ec, int ignorelock);
extern void erasedb(void);
extern void eraser(struct ent *v1, struct ent *v2);
extern void fgetnum(int r0, int c0, int rn, int cn, int fd);
extern void fill(struct ent *v1, struct ent *v2, double start, double inc);
extern void fix_colors(int row1, int col1, int row2, int col2,
                       int delta1, int delta2);
extern void fix_frames(int row1, int col1, int row2, int col2,
                       int delta1, int delta2);
extern void fix_ranges(int row1, int col1, int row2, int col2,
                       int delta1, int delta2);
extern void flush_saved(void);
extern void format_cell(struct ent *v1, struct ent *v2, const char *s);
extern void forwcol(int arg);
extern void forwrow(int arg);
extern void free_ent(struct ent *p, int unlock);
extern void getexp(int r0, int c0, int rn, int cn, int fd);
extern void getfmt(int r0, int c0, int rn, int cn, int fd);
extern void getformat(int col, int fd);
extern void getnum(int r0, int c0, int rn, int cn, int fd);
extern void getstring(int r0, int c0, int rn, int cn, int fd);
extern void go_last(void);
extern void goraw(void);
extern void dohide(void);
extern void hidecols(int c1, int c2);
extern void hiderows(int c1, int c2);
extern void initkbd(void);
extern void insertcol(int arg, int delta);
extern void insertrow(int arg, int delta);
extern void kbd_again(void);
extern void label(struct ent *v, const char *s, int flushdir);
extern void unlet(struct ent *v);
extern void let(struct ent *v, struct enode *e);
extern void list_ranges(FILE *f);
extern void lock_cells(struct ent *v1, struct ent *v2);
extern void move_area(int dr, int dc, int sr, int sc, int er, int ec);
extern void mover(struct ent *d, struct ent *v1, struct ent *v2);
extern void moveto(int row, int col, int lastrow, int lastcol,
                   int cornrow, int corncol);
extern void num_search(double n, int firstrow, int firstcol, int lastrow,
                       int lastcol, int errsearch);
extern void printfile(const char *fname, int r0, int c0, int rn, int cn);
extern void pullcells(int to_insert);
extern void query(const char *s, const char *data);
extern void read_hist(void);
extern void remember(int save);
extern void resetkbd(void);
extern void setautocalc(int i);
extern void setiterations(int i);
extern void setcalcorder(int i);
extern void showcol(int c1, int c2);
extern void showrow(int r1, int r2);
extern void showstring(const char *string, int dirflush, int hasvalue, int row,
                       int col, int *nextcolp, int mxcol, int *fieldlenp, int r, int c,
                       struct frange *fr, int frightcols, int flcols, int frcols);
extern void signals(void);
extern void slet(struct ent *v, struct enode *se, int flushdir);
extern void sortrange(struct ent *left, struct ent *right, const char *criteria);
extern void startdisp(void);
extern void stopdisp(void);
extern void str_search(const char *s, int firstrow, int firstcol, int lastrow,
                       int lastcol, int num);
extern void sync_cranges(void);
extern void sync_franges(void);
extern void sync_ranges(void);
extern void sync_refs(void);
extern void tblprintfile(const char *fname, int r0, int c0, int rn, int cn);
extern void unlock_cells(struct ent *v1, struct ent *v2);
extern void update(int anychanged);
extern void valueize_area(int sr, int sc, int er, int ec);
extern void write_abbrevs(FILE *f);
extern void clean_abbrevs(void);
extern void write_cells(FILE *f, int r0, int c0, int rn, int cn,
                        int dr, int dc);
extern void write_cranges(FILE *f);
extern void write_fd(FILE *f, int r0, int c0, int rn, int cn);
extern void write_franges(FILE *f);
extern void write_hist(void);
extern void write_ranges(FILE *f);
extern void yank_area(int sr, int sc, int er, int ec);
extern void yyerror(const char *err);
extern int yylex(void);
// XXX: redundant declaration in y.tab.h
//extern int yyparse(void);
extern int backup_file(const char *path);
extern void sc_set_locale(int set);
extern int set_line(const char *fmt, ...) sc__attr_printf(1,2);

extern int modflg;
#ifndef NOCRYPT
extern int Crypt;
#endif
extern SCXMEM char *mdir;
extern SCXMEM char *autorun;
extern int skipautorun;
extern SCXMEM char *fkey[FKEYS];
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
extern void addnote(struct ent *p, int sr, int sc, int er, int ec);
extern void delnote(struct ent *p);
extern void center(int sr, int sc, int er, int ec);
extern void rjustify(int sr, int sc, int er, int ec);
extern void ljustify(int sr, int sc, int er, int ec);
extern void yankcols(int c1, int c2);
extern void yankrows(int r1, int r2);
extern void list_frames(FILE *fp);
extern void yankr(struct ent *v1, struct ent *v2);
extern void dogetkey(int fd);
extern void doseval(struct enode *e, int row, int col, int fd);
extern void doeval(struct enode *e, const char *fmt, int row, int col, int fd);
extern void getrange(const char *name, int fd);
extern void getframe(int fd);
extern void add_abbr(const char *string);
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
extern void vi_select_range(const char *arg);
extern void lotus_menu(void);

/* character class macros to avoid undefined behavior on negative chars */
#define isspacechar(c)   isspace((unsigned char)(c))
#define isdigitchar(c)   isdigit((unsigned char)(c))
#define isxdigitchar(c)  isxdigit((unsigned char)(c))
#define isalphachar(c)   isalpha((unsigned char)(c))
#define isalnumchar(c)   isalnum((unsigned char)(c))
#define islowerchar(c)   islower((unsigned char)(c))
#define isupperchar(c)   isupper((unsigned char)(c))
#define tolowerchar(c)   tolower((unsigned char)(c))
#define toupperchar(c)   toupper((unsigned char)(c))

static inline int isalphachar_(char c) { return isalphachar(c) || c == '_'; }
static inline int isalnumchar_(char c) { return isalnumchar(c) || c == '_'; }

/* char buffer utilities */

/* buf_t structure to collect bufferized output */
typedef struct buf_t {
    char *buf;
    size_t size, len;
} buf_t[1];

/* define a fixed size buf_t object */
#define buf_t(name, size) \
    char name##__buf[size]; \
    buf_t name = { name##__buf, sizeof(name##__buf), 0 }

/* initialize a buffer with explicit array and size */
static inline void buf_init(buf_t buf, char *p, size_t size) {
    buf->buf = p;
    buf->size = size;
    buf->len = 0;
}

/* clear the contents of a buffer */
static inline void buf_reset(buf_t buf) {
    buf->buf[buf->len = 0] = '\0';
}

/* write the contents of a buffer to a system file handle */
static inline ssize_t buf_write(buf_t buf, int fd) {
    ssize_t res = write(fd, buf->buf, buf->len);
    buf->len = 0;
    return res;
}

/* append a char to a buffer  */
int buf_putc(buf_t buf, int c);

/* append a block of bytes to a buffer */
size_t buf_put(buf_t buf, const char *s, size_t len);

/* append a string to a buffer */
size_t buf_puts(buf_t buf, const char *s);

/* append a formated string to a buffer */
size_t buf_printf(buf_t buf, const char *fmt, ...) sc__attr_printf(2,3);

/* set buffer contents to block of bytes */
size_t buf_set(buf_t buf, const char *s, size_t len);

/* set buffer contents to a string */
size_t buf_sets(buf_t buf, const char *s);

/* set buffer contents to a formated string */
size_t buf_setf(buf_t buf, const char *fmt, ...) sc__attr_printf(2,3);
