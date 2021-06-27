/*      SC      A Spreadsheet Calculator
 *              Expression interpreter and assorted support routines.
 *
 *              original by James Gosling, September 1982
 *              modified by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#ifdef aiws
#undef _C_func                  /* Fixes for undefined symbols on AIX */
#endif

#ifdef USE_IEEEFP_H
# include <ieeefp.h>
#endif

#include <math.h>
#include <signal.h>
#include <setjmp.h>
#include <time.h>
#ifdef REGCOMP
#include <regex.h>
#endif
#include "sc.h"
#ifdef RE_COMP
extern char *re_comp(char *s);
extern char *re_exec(char *s);
#endif
#ifdef REGCMP
char *regcmp();
char *regex();
#endif

/* Use this structure to save the last 'g' command */
struct go_save gs;

/* g_type can be: */
#define G_NONE 0        /* Starting value - must be 0 */
#define G_NUM 1
#define G_STR 2
#define G_NSTR 3
#define G_XSTR 4
#define G_CELL 5

#define ISVALID(r,c)    ((r)>=0 && (r)<maxrows && (c)>=0 && (c)<maxcols)

static jmp_buf fpe_save;
static int exprerr;        /* Set by eval() and seval() if expression errors */
double prescale = 1.0; /* Prescale for constants in let() */
int extfunc  = 0;   /* Enable/disable external functions */
int loading = 0;    /* Set when readfile() is active */
int gmyrow, gmycol; /* globals used to implement @myrow, @mycol cmds */
static int rowoffset = 0, coloffset = 0;   /* row & col offsets for range functions */
int propagation = 10;   /* max number of times to try calculation */
static int repct = 1;   /* Make repct a global variable so that the
                           function @numiter can access it */

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
struct enode *freeenodes = NULL;

static double dolookup(struct enode *val, int minr, int minc, int maxr,
                       int maxc, int offr, int offc);
static double fn1_eval(double (*fn)(double), double arg);
static double fn2_eval(double (*fn)(double, double), double arg1, double arg2);
static int RealEvalAll(void);
static int constant(struct enode *e);
static void RealEvalOne(struct ent *p, int i, int j, int *chgct);
static void copydbuf(int deltar, int deltac);

static double finfunc(int fun, double v1, double v2, double v3);
static SCXMEM char *dostindex(int minr, int minc, int maxr, int maxc, struct enode *val);
static double doindex(int minr, int minc, int maxr, int maxc, struct enode *val);
static double docount(int, int, int, int, struct enode *);
static double dosum(int, int, int, int, struct enode *);
static double doprod(int, int, int, int, struct enode *);
static double doavg(int, int, int, int, struct enode *);
static double dostddev(int, int, int, int, struct enode *);
static double domax(int, int, int, int, struct enode *);
static double domin(int, int, int, int, struct enode *);
static double dodts(int, int, int);
static double dotts(int, int, int);
static double dotime(int, double);
static double doston(SCXMEM char *);
static double doeqs(SCXMEM char *s1, SCXMEM char *s2);
static struct ent *getent(SCXMEM char *colstr, double row);
static double donval(SCXMEM char *colstr, double row);
static double dolmax(struct enode *);
static double dolmin(struct enode *);
static SCXMEM char *docat(SCXMEM char *s1, SCXMEM char *s2);
static SCXMEM char *dodate(time_t, const char *);
static SCXMEM char *dofmt(SCXMEM char *fmtstr, double v);
static SCXMEM char *doext(struct enode *);
static SCXMEM char *dosval(SCXMEM char *colstr, double row);
static char *docapital(char *s);  // in place
static char *docase(int acase, char *s);  // in place, UPPER or LOWER
static SCXMEM char *dosubstr(SCXMEM char *, int, int);

#ifdef RINT
double rint(double d);
#endif
static double rand_between(double aa, double bb);

static int cellerror = CELLOK;     /* is there an error in this cell */
static void g_free(void);
static sigret_t eval_fpe(int);

#ifndef M_PI
#define M_PI (double)3.14159265358979323846
#endif
#define dtr(x) ((x) * (M_PI / (double)180.0))
#define rtd(x) ((x) * (180.0 / (double)M_PI))

static double finfunc(int fun, double v1, double v2, double v3) {
    double answer, p;

    p = fn2_eval(pow, 1 + v2, v3);

    switch (fun) {
    case PV:
        if (v2) {
            answer = v1 * (1 - 1 / p) / v2;
        } else {
            cellerror = CELLERROR;
            answer = 0.0;
        }
        break;
    case FV:
        if (v2) {
            answer = v1 * (p - 1) / v2;
        } else {
            cellerror = CELLERROR;
            answer = 0.0;
        }
        break;
    case PMT:
        /* CHECK IF ~= 1 - 1/1 */
        if (p && p != 1.0) {
            answer = v1 * v2 / (1 - 1 / p);
        } else {
            cellerror = CELLERROR;
            answer = 0.0;
        }
        break;
    default:
        error("Unknown function in finfunc");
        cellerror = CELLERROR;
        answer = 0.0;
        break;
    }
    return answer;
}

static SCXMEM char *dostindex(int minr, int minc, int maxr, int maxc, struct enode *val) {
    int r, c;
    struct ent *p;
    SCXMEM char *pr;

    if (minr == maxr) {                 /* look along the row */
        r = minr;
        c = minc + (int)eval(val) - 1;
    } else if (minc == maxc) {          /* look down the column */
        r = minr + (int)eval(val) - 1;
        c = minc;
    } else {
        r = minr + (int)eval(val->e.o.left) - 1;
        c = minc + (int)eval(val->e.o.right) - 1;
    }
    p = NULL;
    if (c <= maxc && c >=minc && r <= maxr && r >=minr) {
        p = *ATBL(tbl, r, c);
        if (p && p->label) {
            pr = scxdup(p->label);
            if (p->cellerror)
                cellerror = CELLINVALID;
            return pr;
        }
    }
    return NULL;
}

static double doindex(int minr, int minc, int maxr, int maxc, struct enode *val) {
    int r, c;
    struct ent *p;

    if (val->op == ',') {               /* index by both row and column */
        r = minr + (int)eval(val->e.o.left) - 1;
        c = minc + (int)eval(val->e.o.right) - 1;
    } else if (minr == maxr) {          /* look along the row */
        r = minr;
        c = minc + (int)eval(val) - 1;
    } else if (minc == maxc) {          /* look down the column */
        r = minr + (int)eval(val) - 1;
        c = minc;
    } else {
        error("Improper indexing operation");
        return 0.0;
    }

    if (c <= maxc && c >= minc && r <= maxr && r >= minr &&
            (p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
        if (p->cellerror)
            cellerror = CELLINVALID;
        return p->v;
    } else
        return 0.0;
}

static double dolookup(struct enode * val, int minr, int minc, int maxr, int maxc, int offset, int vflag) {
    double v, ret = 0.0;
    int r, c;
    struct ent *p = NULL;
    int incr, incc, fndr, fndc;
    SCXMEM char *s = NULL;

    incr = vflag;
    incc = 1 - vflag;
    if (etype(val) == NUM) {
        cellerror = CELLOK;
        v = eval(val);
        for (r = minr, c = minc; r <= maxr && c <= maxc; r += incr, c += incc) {
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->v <= v) {
                    fndr = incc ? (minr + offset) : r;
                    fndc = incr ? (minc + offset) : c;
                    if (ISVALID(fndr, fndc))
                        p = *ATBL(tbl, fndr, fndc);
                    else {
                        error(" range specified to @[hv]lookup");
                        cellerror = CELLERROR;
                    }
                    // XXX: bufg if !ISVALID(fndr, fndc)
                    if (p && (p->flags & IS_VALID)) {
                        if (p->cellerror)
                            cellerror = CELLINVALID;
                        ret = p->v;
                    }
                } else
                    break;
            }
        }
    } else {
        cellerror = CELLOK;
        s = seval(val);
        for (r = minr, c = minc; r <= maxr && c <= maxc; r += incr, c += incc) {
            if ((p = *ATBL(tbl, r, c)) && p->label) {
                if (strcmp(p->label, s) == 0) {
                    fndr = incc ? (minr + offset) : r;
                    fndc = incr ? (minc + offset) : c;
                    if (ISVALID(fndr, fndc)) {
                        p = *ATBL(tbl, fndr, fndc);
                        if (p->cellerror)
                            cellerror = CELLINVALID;
                    } else {
                        error(" range specified to @[hv]lookup");
                        cellerror = CELLERROR;
                    }
                    break;
                }
            }
        }
        if (p && (p->flags & IS_VALID))
            ret = p->v;
        scxfree(s);
    }
    return ret;
}

static double docount(int minr, int minc, int maxr, int maxc, struct enode *e) {
    int v;
    int r, c;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;
                    v++;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return (double)v;
}

static double dosum(int minr, int minc, int maxr, int maxc, struct enode *e) {
    double v;
    int r, c;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;
                    v += p->v;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double doprod(int minr, int minc, int maxr, int maxc, struct enode *e) {
    double v;
    int r, c;
    int cellerr = CELLOK;
    struct ent *p;

    v = 1.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;
                    v *= p->v;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double doavg(int minr, int minc, int maxr, int maxc, struct enode *e) {
    double v;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;
                    v += p->v;
                    count++;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;

    if (count == 0)
        return 0.0;
    else
        return v / (double)count;
}

static double dostddev(int minr, int minc, int maxr, int maxc, struct enode *e) {
    double lp, rp, v, nd;
    int r, c;
    int n;
    int cellerr = CELLOK;
    struct ent *p;

    n = 0;
    lp = 0.0;
    rp = 0.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;
                    v = p->v;
                    lp += v * v;
                    rp += v;
                    n++;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;

    if (n <= 1)
        return 0.0;
    nd = (double)n;
    return sqrt((nd * lp - rp * rp) / (nd * (nd - 1)));
}

static double domax(int minr, int minc, int maxr, int maxc, struct enode *e) {
    double v = 0.0;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;
                    if (!count++) {
                        v = p->v;
                    } else
                    if (p->v > v)
                        v = p->v;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double domin(int minr, int minc, int maxr, int maxc, struct enode *e) {
    double v = 0.0;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e) {
                rowoffset = r - minr;
                coloffset = c - minc;
            }
            if (!e || eval(e)) {
                if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                    if (p->cellerror)
                        cellerr = CELLINVALID;

                    if (!count++) {
                        v = p->v;
                    } else
                    if (p->v < v)
                        v = p->v;
                }
            }
        }
    }
    cellerror = cellerr;
    rowoffset = coloffset = 0;
    return v;
}

static double dodts(int e1, int e2, int e3) {
    int mdays[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    int yr, mo, day;
    time_t secs;
    struct tm t;

    if (e2 > 12 || e3 > 31) {
        mo  = e1;
        day = e2;
        yr  = e3;
    } else {
        yr  = e1;
        mo  = e2;
        day = e3;
    }
    mdays[1] = 28 + (yr % 4 == 0) - (yr % 100 == 0) + (yr % 400 == 0);

    t.tm_hour = t.tm_min = t.tm_sec = 0;
    t.tm_mon = mo - 1;
    t.tm_mday = day;
    t.tm_year = yr - 1900;
    t.tm_isdst = -1;

    if (mo < 1 || mo > 12 || day < 1 || day > mdays[mo - 1] || (secs = mktime(&t)) == -1) {
        error("@dts: invalid argument or date out of range");
        cellerror = CELLERROR;
        return 0.0;
    }

    return (double)secs;
}

static double dotts(int hr, int min, int sec) {
    if (hr < 0 || hr > 23 || min < 0 || min > 59 || sec < 0 || sec > 59) {
        error ("@tts: Invalid argument");
        cellerror = CELLERROR;
        return 0.0;
    }
    return (double)(sec + min * 60 + hr * 3600);
}

static double dotime(int which, double when) {
    static time_t t_cache;
    static struct tm tm_cache;
    struct tm *tp;
    time_t tloc;

    if (which == NOW)
        return (double)time(NULL);

    tloc = (time_t)when;

    if (!t_cache || tloc != t_cache) {
        tp = localtime(&tloc);
        tm_cache = *tp;
        t_cache = tloc;
    }

    switch (which) {
    case HOUR:          return (double)tm_cache.tm_hour;
    case MINUTE:        return (double)tm_cache.tm_min;
    case SECOND:        return (double)tm_cache.tm_sec;
    case MONTH:         return (double)tm_cache.tm_mon + 1;
    case DAY:           return (double)tm_cache.tm_mday;
    case YEAR:          return (double)tm_cache.tm_year + 1900;
    }
    /* Safety net */
    cellerror = CELLERROR;
    return 0.0;
}

static double doston(SCXMEM char *s) {
    double v;

    if (!s)
        return 0.0;

    v = strtod(s, NULL);
    scxfree(s);
    return v;
}

static double doeqs(SCXMEM char *s1, SCXMEM char *s2) {
    double v;

    if (!s1 && !s2)
        return 1.0;

    if (!s1 || !s2)
        v = 0.0;
    else if (strcmp(s1, s2) == 0)
        v = 1.0;
    else
        v = 0.0;

    scxfree(s1);
    scxfree(s2);

    return v;
}

/*
 * Given a string representing a column name and a value which is a row
 * number, return a pointer to the selected cell's entry, if any, else NULL.
 * Use only the integer part of the column number.  Always free the string.
 */

static struct ent *getent(SCXMEM char *colstr, double rowdoub) {
    int collen;             /* length of string */
    int row, col;           /* integer values   */
    struct ent *p = NULL;   /* selected entry   */

    if (!colstr) {
        cellerror = CELLERROR;
        return NULL;
    }

    if (((row = (int)floor(rowdoub)) >= 0)
    &&  (row < maxrows)                          /* in range */
    &&  ((collen = strlen(colstr)) <= 2)         /* not too long */
    &&  ((col = atocol(colstr, collen)) >= 0)
    &&  (col < maxcols)) {                       /* in range */
        p = *ATBL(tbl, row, col);
        if (p && p->cellerror)
            cellerror = CELLINVALID;
    }
    scxfree(colstr);
    return p;
}

/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

static double donval(SCXMEM char *colstr, double rowdoub) {
    struct ent *ep = getent(colstr, rowdoub);

    return (ep && (ep->flags & IS_VALID)) ? ep->v : 0.0;
}


/*
 *      The list routines (e.g. dolmax) are called with an LMAX enode.
 *      The left pointer is a chain of ELIST nodes, the right pointer
 *      is a value.
 */
static double dolmax(struct enode *ep) {
    int count = 0;
    double maxval = 0.0;
    struct enode *p;

    cellerror = CELLOK;
    for (p = ep; p; p = p->e.o.left) {
        double v = eval(p->e.o.right);
        if (!count || v > maxval) {
            maxval = v;
            count++;
        }
    }
    return maxval;
}

static double dolmin(struct enode *ep) {
    int count = 0;
    double minval = 0.0;
    struct enode *p;

    cellerror = CELLOK;
    for (p = ep; p; p = p->e.o.left) {
        double v = eval(p->e.o.right);
        if (!count || v < minval) {
            minval = v;
            count++;
        }
    }
    return minval;
}

double eval(struct enode *e) {
    if (e == NULL) {
        cellerror = CELLINVALID;
        return 0.0;
    }
    switch (e->op) {
    case '+':       return eval(e->e.o.left) + eval(e->e.o.right);
    case '-':       {
                        double l = eval(e->e.o.left);
                        double r = eval(e->e.o.right);
                        return l - r;
                    }
    case '*':       return eval(e->e.o.left) * eval(e->e.o.right);
    case '/':       {
                        double num = eval(e->e.o.left);
                        double denom = eval(e->e.o.right);
                        if (cellerror) {
                            cellerror = CELLINVALID;
                            return 0.0;
                        } else
                        if (denom) {
                            return num / denom;
                        } else {
                            cellerror = CELLERROR;
                            return 0.0;
                        }
                    }
    case '%':       {
                        // XXX: this API is incorrect
                        double num = floor(eval(e->e.o.left));
                        double denom = floor(eval(e->e.o.right));
                        if (denom) {
                            return (num - floor(num / denom) * denom);
                        } else {
                            cellerror = CELLERROR;
                            return 0.0;
                        }
                    }
    case '^':       return fn2_eval(pow, eval(e->e.o.left), eval(e->e.o.right));
    case '<':       return eval(e->e.o.left) < eval(e->e.o.right);
    case '=':       {
                        double l = eval(e->e.o.left);
                        double r = eval(e->e.o.right);
                        return l == r;
                    }
    case '>':       return eval(e->e.o.left) >  eval(e->e.o.right);
    case '&':       return eval(e->e.o.left) && eval(e->e.o.right);
    case '|':       return eval(e->e.o.left) || eval(e->e.o.right);
    case IF:
    case '?':       return eval(e->e.o.left) ? eval(e->e.o.right->e.o.left)
                                             : eval(e->e.o.right->e.o.right);
    case 'm':       return -eval(e->e.o.left);
    case 'f':       {
                        int rtmp = rowoffset;
                        int ctmp = coloffset;
                        double ret;
                        rowoffset = coloffset = 0;
                        ret = eval(e->e.o.left);
                        rowoffset = rtmp;
                        coloffset = ctmp;
                        return ret;
                    }
    case 'F':       return eval(e->e.o.left);
    case '!':       return eval(e->e.o.left) == 0.0;
    case ';':       return ((int)eval(e->e.o.left) & 7) +
                            (((int)eval(e->e.o.right) & 7) << 3);
    case O_CONST:   if (!isfinite(e->e.k)) {
                        e->op = ERR_;
                        e->e.k = 0.0;
                        cellerror = CELLERROR;
                    }
                    return e->e.k;
    case O_VAR:     {
                        struct ent *vp = e->e.v.vp;
                        int row, col;
                        if (vp && (rowoffset || coloffset)) {
                            row = e->e.v.vf & FIX_ROW ?
                                vp->row : vp->row + rowoffset;
                            col = e->e.v.vf & FIX_COL ?
                                vp->col : vp->col + coloffset;
                            checkbounds(&row, &col);
                            vp = *ATBL(tbl, row, col);
                        }
                        if (!vp || (vp->flags & IS_DELETED)) {
                            cellerror = CELLERROR;
                            return 0.0;
                        }
                        if (vp->cellerror)
                            cellerror = CELLINVALID;
                        return vp->v;
                    }
    case SUM:
    case PROD:
    case AVG:
    case COUNT:
    case STDDEV:
    case MAX:
    case MIN:
    case INDEX:
    case LOOKUP:
    case HLOOKUP:
    case VLOOKUP: {
            int minr = e->e.o.left->e.r.left.vp->row;
            int minc = e->e.o.left->e.r.left.vp->col;
            int maxr = e->e.o.left->e.r.right.vp->row;
            int maxc = e->e.o.left->e.r.right.vp->col;
            if (minr > maxr) SWAPINT(minr, maxr);
            if (minc > maxc) SWAPINT(minc, maxc);
            switch (e->op) {
            case SUM:
                return dosum(minr, minc, maxr, maxc, e->e.o.right);
            case PROD:
                return doprod(minr, minc, maxr, maxc, e->e.o.right);
            case AVG:
                return doavg(minr, minc, maxr, maxc, e->e.o.right);
            case COUNT:
                return docount(minr, minc, maxr, maxc, e->e.o.right);
            case STDDEV:
                return dostddev(minr, minc, maxr, maxc, e->e.o.right);
            case MAX:
                return domax(minr, minc, maxr, maxc, e->e.o.right);
            case MIN:
                return domin(minr, minc, maxr, maxc, e->e.o.right);
            case LOOKUP:
                return dolookup(e->e.o.right, minr, minc, maxr, maxc, 1, minc == maxc);
            case HLOOKUP:
                return dolookup(e->e.o.right->e.o.left, minr, minc, maxr, maxc,
                                (int)eval(e->e.o.right->e.o.right), 0);
            case VLOOKUP:
                return dolookup(e->e.o.right->e.o.left, minr, minc, maxr, maxc,
                                (int)eval(e->e.o.right->e.o.right), 1);
            case INDEX:
                return doindex(minr, minc, maxr, maxc, e->e.o.right);
            }
        }
    case REDUCE | 'R': return abs(e->e.r.right.vp->row - e->e.r.left.vp->row) + 1;
    case REDUCE | 'C': return abs(e->e.r.right.vp->col - e->e.r.left.vp->col) + 1;

    case ABS:        return fn1_eval( fabs, eval(e->e.o.left));
    case ACOS:       return fn1_eval( acos, eval(e->e.o.left));
    case ASIN:       return fn1_eval( asin, eval(e->e.o.left));
    case ATAN:       return fn1_eval( atan, eval(e->e.o.left));
    case ATAN2:      return fn2_eval( atan2, eval(e->e.o.left), eval(e->e.o.right));
    case CEIL:       return fn1_eval( ceil, eval(e->e.o.left));
    case COS:        return fn1_eval( cos, eval(e->e.o.left));
    case EXP:        return fn1_eval( exp, eval(e->e.o.left));
    case FABS:       return fn1_eval( fabs, eval(e->e.o.left));
    case FLOOR:      return fn1_eval( floor, eval(e->e.o.left));
    case HYPOT:      return fn2_eval( hypot, eval(e->e.o.left), eval(e->e.o.right));
    case LOG:        return fn1_eval( log, eval(e->e.o.left));
    case LOG10:      return fn1_eval( log10, eval(e->e.o.left));
    case POW:        return fn2_eval( pow, eval(e->e.o.left), eval(e->e.o.right));
    case SIN:        return fn1_eval( sin, eval(e->e.o.left));
    case SQRT:       return fn1_eval( sqrt, eval(e->e.o.left));
    case TAN:        return fn1_eval( tan, eval(e->e.o.left));
    case DTR:        return dtr(eval(e->e.o.left));
    case RTD:        return rtd(eval(e->e.o.left));
    case RAND:       return (double)rand() / ((double)RAND_MAX + 1);
    case RANDBETWEEN: return (double)rand_between(eval(e->e.o.left), eval(e->e.o.right));
    case RND:
        if (rndtoeven) {
            return rint(eval(e->e.o.left));
        } else {
            double temp = eval(e->e.o.left);
            return (temp - floor(temp) < 0.5 ? floor(temp) : ceil(temp));
        }
    case ROUND:
        {   int prec = (int)eval(e->e.o.right);
            double scale = 1;
            if (0 < prec)
                do scale *= 10; while (0 < --prec);
            else if (prec < 0)
                do scale /= 10; while (++prec < 0);

            if (rndtoeven)
                return rint(eval(e->e.o.left) * scale) / scale;
            else {
                double temp = eval(e->e.o.left);
                temp *= scale;
                /* xxx */
                /*
                temp = (temp > 0.0 ? floor(temp + 0.5) : ceil(temp - 0.5));
                */
                temp = ((temp - floor(temp)) < 0.5 ?
                        floor(temp) : ceil(temp));
                return temp / scale;
            }
        }
    case FV:
    case PV:
    case PMT:       return finfunc(e->op, eval(e->e.o.left),
                                   eval(e->e.o.right->e.o.left),
                                   eval(e->e.o.right->e.o.right));
    case HOUR:      return dotime(HOUR, eval(e->e.o.left));
    case MINUTE:    return dotime(MINUTE, eval(e->e.o.left));
    case SECOND:    return dotime(SECOND, eval(e->e.o.left));
    case MONTH:     return dotime(MONTH, eval(e->e.o.left));
    case DAY:       return dotime(DAY, eval(e->e.o.left));
    case YEAR:      return dotime(YEAR, eval(e->e.o.left));
    case NOW:       return dotime(NOW, 0.0);
    case DTS:       return dodts((int)eval(e->e.o.left),
                                 (int)eval(e->e.o.right->e.o.left),
                                 (int)eval(e->e.o.right->e.o.right));
    case TTS:       return dotts((int)eval(e->e.o.left),
                                 (int)eval(e->e.o.right->e.o.left),
                                 (int)eval(e->e.o.right->e.o.right));
    case STON:      return doston(seval(e->e.o.left));
    case EQS:       return doeqs(seval(e->e.o.right), seval(e->e.o.left));
    case LMAX:      return dolmax(e);
    case LMIN:      return dolmin(e);
    case NVAL:      return donval(seval(e->e.o.left), eval(e->e.o.right));
    case MYROW:     return (double)(gmyrow + rowoffset);
    case MYCOL:     return (double)(gmycol + coloffset);
    case LASTROW:   return (double)maxrow;
    case LASTCOL:   return (double)maxcol;
    case NUMITER:   return (double)repct;
    case ERR_:      cellerror = CELLERROR;
                    return 0.0;
    case PI_:       return (double)M_PI;
    case BLACK:     return (double)COLOR_BLACK;
    case RED:       return (double)COLOR_RED;
    case GREEN:     return (double)COLOR_GREEN;
    case YELLOW:    return (double)COLOR_YELLOW;
    case BLUE:      return (double)COLOR_BLUE;
    case MAGENTA:   return (double)COLOR_MAGENTA;
    case CYAN:      return (double)COLOR_CYAN;
    case WHITE:     return (double)COLOR_WHITE;
    default:        error ("Illegal numeric expression");
                    exprerr = 1;
    }
    cellerror = CELLERROR;
    return 0.0;
}

static sigret_t eval_fpe(int i) { /* Trap for FPE errors in eval */
    (void)i;
#if defined(i386) && !defined(M_XENIX)
    asm("       fnclex");
    asm("       fwait");
#else
#ifdef IEEE_MATH
    fpsetsticky((fp_except)0);    /* Clear exception */
#endif /* IEEE_MATH */
#ifdef PC
    _fpreset();
#endif
#endif
    /* re-establish signal handler for next time */
    signal(SIGFPE, eval_fpe);
    longjmp(fpe_save, 1);
}

static double fn1_eval(double (*fn)(double), double a) {
    double res;
    errno = 0;
    res = (*fn)(a);
    if (errno)
        cellerror = CELLERROR;
    return res;
}

static double fn2_eval(double (*fn)(double, double), double arg1, double arg2) {
    double res;
    errno = 0;
    res = (*fn)(arg1, arg2);
    if (errno)
        cellerror = CELLERROR;
    return res;
}

/*
 * Rules for string functions:
 * Take string arguments which they scxfree.
 * All returned strings are assumed to be xalloced.
 */

static SCXMEM char *docat(SCXMEM char *s1, SCXMEM char *s2) {
    size_t len1, len2;
    char *p;

    if (!s1) return s2;
    if (!s2) return s1;
    len1 = strlen(s1);
    len2 = strlen(s2);
    p = scxmalloc(len1 + len2 + 1);
    memcpy(p, s1, len1);
    memcpy(p + len1, s2, len2 + 1);
    scxfree(s1);
    scxfree(s2);
    return p;
}

static SCXMEM char *dodate(time_t tloc, const char *fmtstr) {
    char buff[FBUFLEN];

    if (!fmtstr)
        fmtstr = "%a %b %d %H:%M:%S %Y";
    // XXX: should check format string
    ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
        (buff, sizeof buff, fmtstr, localtime(&tloc));
    return scxdup(buff);
}

static SCXMEM char *dofmt(SCXMEM char *fmtstr, double v) {
    char buff[FBUFLEN];

    if (!fmtstr)
        return NULL;
    // XXX: Achtung Minen! snprintf from user supplied format string
    // XXX: MUST validate format string for no or single arg of type double
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    ((int (*)(char *, size_t, const char *, ...))snprintf)(buff, FBUFLEN, fmtstr, v);
    scxfree(fmtstr);
    return scxdup(buff);
}


/*
 * Given a command name and a value, run the command with the given value and
 * read and return its first output line (only) as an allocated string, always
 * a copy of se->e.o.s, which is set appropriately first unless external
 * functions are disabled, in which case the previous value is used.  The
 * handling of se->e.o.s and freeing of command is tricky.  Returning an
 * allocated string in all cases, even if null, insures cell expressions are
 * written to files, etc.
 */

#ifdef NOEXTFUNCS
static SCXMEM char *doext(struct enode *se) {
    SCXMEM char *command = seval(se->e.o.left);
    double value = eval(se->e.o.right);

    error("Warning: External functions unavailable");
    cellerror = CELLERROR;      /* not sure if this should be a cellerror */
    scxfree(command);
    return scxdup("");
}

#else /* NOEXTFUNCS */

static SCXMEM char *doext(struct enode *se) {
    char buff[FBUFLEN];         /* command line/return, not permanently alloc */
    SCXMEM char *command;
    double value;

    command = seval(se->e.o.left);
    value = eval(se->e.o.right);
    if (!extfunc) {
        error("Warning: external functions disabled; using %s value",
              (se->e.o.s && *se->e.o.s) ? "previous" : "null");
        scxfree(command);
    } else {
        if (!command || !*command) {
            error("Warning: external function given null command name");
            cellerror = CELLERROR;
            scxfree(command);
        } else {
            FILE *pf;

            snprintf(buff, sizeof buff, "%s %.13g", command, value); /* build cmd line */
            scxfree(command);

            error("Running external function...");
            refresh();

            if ((pf = popen(buff, "r")) == NULL) {     /* run it */
                error("Warning: running \"%s\" failed", buff);
                cellerror = CELLERROR;
            } else {
                if (fgets(buff, sizeof(buff), pf) == NULL) {  /* one line */
                    error("Warning: external function returned nothing");
                } else {
                    size_t len = strlen(buff);
                    if (len && buff[len - 1] == '\n')   /* contains newline */
                        buff[--len] = '\0';             /* end string there */
                    if (!se->e.o.s || strcmp(se->e.o.s, buff)) {
                        se->e.o.s = scxrealloc(se->e.o.s, len + 1);
                        if (se->e.o.s)
                            memcpy(se->e.o.s, buff, len + 1);
                    }
                    error(" "); /* erase notice */
                }
                pclose(pf);
            }
        }
    }
    return scxdup(se->e.o.s ? se->e.o.s : "");
}

#endif  /* NOEXTFUNCS */


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's string value, if any.  Even if none,
 * still allocate and return a null string so the cell has a label value so
 * the expression is saved in a file, etc.
 */

static SCXMEM char *dosval(SCXMEM char *colstr, double rowdoub) {
    struct ent *ep = getent(colstr, rowdoub);
    const char *llabel = ep ? ep->label : "";
    return scxdup(llabel);
}


/*
 * Substring:  Note that v1 and v2 are one-based to users, but zero-based
 * when calling this routine.
 */

// XXX: should handle UTF-8
static SCXMEM char *dosubstr(SCXMEM char *s, int v1, int v2) {
    SCXMEM char *p;
    char *s1, *s2;

    if (!s)
        return NULL;

    if (v2 >= (ssize_t)strlen(s))       /* past end */
        v2 = strlen(s) - 1;             /* to end   */

    if (v1 < 0 || v1 > v2) {            /* out of range, return null string */
        scxfree(s);
        return scxdup("");
    }
    s2 = p = scxmalloc(v2 - v1 + 2);
    s1 = &s[v1];
    for (; v1 <= v2; v1++)
        *s2++ = *s1++;
    *s2 = '\0';
    scxfree(s);
    return p;
}

/*
 * character casing: make upper case, make lower case
 */

// XXX: should handle UTF-8 encoded UNICODE stuff
// XXX: string argument is modified in place?
static char *docase(int acase, char *s) {
    char *p = s;

    if (s == NULL)
        return NULL;

    if (acase == UPPER) {
        while (*p != '\0' ) {
            if (islowerchar(*p))
                *p = toupperchar(*p);
            p++;
        }
    } else
    if (acase == LOWER) {
        while (*p != '\0') {
            if (isupperchar(*p))
                *p = tolowerchar(*p);
            p++;
        }
    }
    return s;
}

/*
 * make proper capitals of every word in a string
 * if the string has mixed case we say the string is lower
 *      and we will upcase only first letters of words
 * if the string is all upper we will lower rest of words.
 */

static char *docapital(char *s) {
    char *p;
    int skip = 1;
    int AllUpper = 1;

    if (s == NULL)
        return NULL;

    for (p = s; *p; p++) {
        if (islowerchar(*p)) {
            AllUpper = 0;
            break;
        }
    }
    for (p = s; *p; p++) {
        if (!isalnumchar(*p))
            skip = 1;
        else
        if (skip == 1) {
            skip = 0;
            if (islowerchar(*p))
                *p = toupperchar(*p);
        } else    /* if the string was all upper before */
        if (isupperchar(*p) && AllUpper != 0)
            *p = tolowerchar(*p);
    }
    return s;
}

SCXMEM char *seval(struct enode *se) {
    if (se == NULL) return NULL;
    switch (se->op) {
    case O_SCONST:  return scxdup(se->e.s);
    case O_VAR:     {
                        struct ent *vp = se->e.v.vp;
                        int row, col;
                        if (vp && (rowoffset || coloffset)) {
                            row = se->e.v.vf & FIX_ROW ?
                                vp->row : vp->row + rowoffset;
                            col = se->e.v.vf & FIX_COL ?
                                vp->col : vp->col + coloffset;
                            checkbounds(&row, &col);
                            vp = *ATBL(tbl, row, col);
                        }
                        if (!vp || !vp->label)
                            return NULL;
                        return scxdup(vp->label);
                    }
    case '#':       return docat(seval(se->e.o.left), seval(se->e.o.right));
    case 'f':       {
                        int rtmp = rowoffset;
                        int ctmp = coloffset;
                        SCXMEM char *ret;
                        rowoffset = coloffset = 0;
                        ret = seval(se->e.o.left);
                        rowoffset = rtmp;
                        coloffset = ctmp;
                        return ret;
                    }
    case 'F':       return seval(se->e.o.left);
    case IF:
    case '?':       return eval(se->e.o.left) ? seval(se->e.o.right->e.o.left)
                                              : seval(se->e.o.right->e.o.right);
    case DATE:      return dodate((time_t)(eval(se->e.o.left)),
                                  seval(se->e.o.right));
    case FMT:       return dofmt(seval(se->e.o.left), eval(se->e.o.right));
    case UPPER:     return docase(UPPER, seval(se->e.o.left));
    case LOWER:     return docase(LOWER, seval(se->e.o.left));
    case CAPITAL:   return docapital(seval(se->e.o.left));
    case STINDEX:   {
                        int minr = se->e.o.left->e.r.left.vp->row;
                        int minc = se->e.o.left->e.r.left.vp->col;
                        int maxr = se->e.o.left->e.r.right.vp->row;
                        int maxc = se->e.o.left->e.r.right.vp->col;
                        if (minr > maxr) SWAPINT(minr, maxr);
                        if (minc > maxc) SWAPINT(minc, maxc);
                        return dostindex(minr, minc, maxr, maxc, se->e.o.right);
                    }
    case EXT:       return doext(se);
    case SVAL:      return dosval(seval(se->e.o.left), eval(se->e.o.right));
    case SUBSTR:    return dosubstr(seval(se->e.o.left),
                                    (int)eval(se->e.o.right->e.o.left) - 1,
                                    (int)eval(se->e.o.right->e.o.right) - 1);
    case COLTOA:    return scxdup(coltoa((int)eval(se->e.o.left)));
    case FILENAME:  {
                        int n = (int)eval(se->e.o.left);
                        const char *s = n ? curfile : get_basename(curfile);
                        return scxdup(s);
                    }
    default:
                    error("Illegal string expression");
                    exprerr = 1;
                    return NULL;
    }
}

/*
 * The graph formed by cell expressions which use other cells's values is not
 * evaluated "bottom up".  The whole table is merely re-evaluated cell by cell,
 * top to bottom, left to right, in RealEvalAll().  Each cell's expression uses
 * constants in other cells.  However, RealEvalAll() notices when a cell gets a
 * new numeric or string value, and reports if this happens for any cell.
 * EvalAll() repeats calling RealEvalAll() until there are no changes or the
 * evaluation count expires.
 */

void setiterations(int i) {
    if (i < 1) {
        error("iteration count must be at least 1");
        propagation = 1;
    } else
        propagation = i;
}

void EvalAll(void) {
    int lastcnt, pair, v;

    signal(SIGFPE, eval_fpe);

    for (repct = 1; (lastcnt = RealEvalAll()) && repct < propagation; repct++)
        continue;

    if (propagation > 1 && lastcnt > 0)
        error("Still changing after %d iterations", repct);

    if (usecurses && color && has_colors()) {
        for (pair = 1; pair <= CPAIRS; pair++) {
            cellerror = CELLOK;
            if (cpairs[pair] && cpairs[pair]->expr) {
                v = (int)eval(cpairs[pair]->expr);
                // XXX: should ignore value if cellerror
                init_style(pair, v & 7, (v >> 3) & 7, cpairs[pair]->expr);
            }
            /* Can't see to fix the problem if color 1 has an error, so
             * turn off color in that case.
             */
            if (pair == 1 && cellerror) {
                color = 0;
                attron(COLOR_PAIR(0));
                color_set(0, NULL);
                error("Error in color 1: color turned off");
            }
        }
    }
    signal(SIGFPE, doquit);
}

/*
 * Evaluate all cells which have expressions and alter their numeric or string
 * values.  Return the number of cells which changed.
 */

static int RealEvalAll(void) {
    int i, j;
    int chgct = 0;
    struct ent *p;

    if (calc_order == BYROWS) {
        for (i = 0; i <= maxrow; i++) {
            for (j = 0; j <= maxcol; j++) {
                if ((p = *ATBL(tbl, i, j)) && p->expr)
                    RealEvalOne(p, i, j, &chgct);
            }
        }
    } else
    if (calc_order == BYCOLS) {
        for (j = 0; j <= maxcol; j++) {
            for (i = 0; i <= maxrow; i++) {
                if ((p = *ATBL(tbl,i,j)) && p->expr)
                    RealEvalOne(p, i, j, &chgct);
            }
        }
    } else {
        // XXX: Should implement topological sort
        error("Internal error calc_order");
    }
    return chgct;
}

void RealEvalOne(struct ent *p, int i, int j, int *chgct) {
    gmyrow = i;
    gmycol = j;

    if (p->flags & IS_STREXPR) {
        SCXMEM char *v;
        if (setjmp(fpe_save)) {
            error("Floating point exception %s", v_name(i, j));
            cellerror = CELLERROR;
            v = scxdup("");
        } else {
            cellerror = CELLOK;
            v = seval(p->expr);
        }
        p->cellerror = cellerror;
        if (!v && !p->label) /* Everything's fine */
            return;
        if (!p->label || !v || strcmp(v, p->label) != 0 || cellerror) {
            (*chgct)++;
            p->flags |= IS_CHANGED;
            changed++;
        }
        set_string(&p->label, v);
    } else {
        double v;
        if (setjmp(fpe_save)) {
            error("Floating point exception %s", v_name(i, j));
            cellerror = CELLERROR;
            v = 0.0;
        } else {
            cellerror = CELLOK;
            v = eval(p->expr);
            if (cellerror == CELLOK && !isfinite(v))
                cellerror = CELLERROR;
        }
        if ((cellerror != p->cellerror) || (v != p->v)) {
            p->cellerror = cellerror;
            p->v = v;
            if (!cellerror)     /* don't keep eval'ing an error */
                (*chgct)++;
            p->flags |= IS_CHANGED | IS_VALID;
            changed++;
        }
    }
}

SCXMEM struct enode *new(int op, SCXMEM struct enode *a1, SCXMEM struct enode *a2) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = op;
    p->e.o.left = a1;
    p->e.o.right = a2;
    p->e.o.s = NULL;
    return p;
}

struct SCXMEM enode *new_var(int op, struct ent_ptr a1) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = op;
    p->e.v = a1;
    return p;
}

struct SCXMEM enode *new_range(int op, struct range_s a1) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = op;
    p->e.r = a1;
    return p;
}

struct SCXMEM enode *new_const(int op, double a1) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = op;
    p->e.k = a1;
    return p;
}

struct SCXMEM enode *new_str(SCXMEM char *s) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = O_SCONST;
    p->e.s = s;
    return p;
}

void copy(struct ent *dv1, struct ent *dv2, struct ent *v1, struct ent *v2) {
    struct ent *p;
    // XXX: get rid of this static mess
    static int minsr = -1, minsc = -1;
    static int maxsr = -1, maxsc = -1;
    int mindr, mindc;
    int maxdr, maxdc;
    int deltar, deltac;

    if (dv1) {
        mindr = dv1->row;
        mindc = dv1->col;
        maxdr = dv2->row;
        maxdc = dv2->col;
        if (mindr > maxdr) SWAPINT(mindr, maxdr);
        if (mindc > maxdc) SWAPINT(mindc, maxdc);
    } else {
        if (showrange) {
            showrange = 0;
            mindr = showsr < currow ? showsr : currow;
            mindc = showsc < curcol ? showsc : curcol;
            maxdr = showsr > currow ? showsr : currow;
            maxdc = showsc > curcol ? showsc : curcol;
        } else if (v1) {
            /* Set up the default source range for the "c." command. */
            minsr = maxsr = v1->row;
            minsc = maxsc = v1->col;
            return;
        } else {
            mindr = maxdr = currow;
            mindc = maxdc = curcol;
        }
    }

    if (v1) {
        minsr = v1->row;
        minsc = v1->col;
        maxsr = v2->row;
        maxsc = v2->col;
        if (minsr > maxsr) SWAPINT(minsr, maxsr);
        if (minsc > maxsc) SWAPINT(minsc, maxsc);
    } else if (dv1 == NULL || v2 != NULL) {
        if (qbuf && delbuf[qbuf]) {
            delbuf[++dbidx] = delbuf[qbuf];
            delbuffmt[dbidx] = delbuffmt[qbuf];
        } else if (dbidx < 0)
            return;
        minsr = maxrow;
        minsc = maxcol;
        maxsr = 0;
        maxsc = 0;
        for (p = delbuf[dbidx]; p; p = p->next) {
            if (p->row < minsr) minsr = p->row;
            if (p->row > maxsr) maxsr = p->row;
            if (p->col < minsc) minsc = p->col;
            if (p->col > maxsc) maxsc = p->col;
        }
    } else if (showrange &&
               !(showsr == currow && showsc == curcol &&
                 mindr == currow && mindc == curcol &&
                 maxdr == currow && maxdc == curcol)) {
        minsr = showsr < currow ? showsr : currow;
        minsc = showsc < curcol ? showsc : curcol;
        maxsr = showsr > currow ? showsr : currow;
        maxsc = showsc > curcol ? showsc : curcol;
    } else {
        // XXX: use static values: check if we can avoid it
        if (minsr == -1)
            return;
    }

    checkbounds(&maxdr, &maxdc);

    if (maxdr - mindr < maxsr - minsr) maxdr = mindr + (maxsr - minsr);
    if (maxdc - mindc < maxsc - minsc) maxdc = mindc + (maxsc - minsc);
    if (dv1 && (v1 || !v2))
        yank_area(minsr, minsc, maxsr, maxsc);
    erase_area(mindr, mindc, maxdr, maxdc, 0);
    sync_refs();
    flush_saved();

    error("Copying...");
    if (!loading)
        refresh();
    p = delbuf[dbidx];
    if (minsr == maxsr && minsc == maxsc) {
        /* Source is a single cell */
        for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++) {
            for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
                copydbuf(deltar, deltac);
        }
    } else if (minsr == maxsr) {
        /* Source is a single row */
        deltac = mindc - p->col;
        for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++)
            copydbuf(deltar, deltac);
    } else if (minsc == maxsc) {
        /* Source is a single column */
        deltar = mindr - p->row;
        for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
            copydbuf(deltar, deltac);
    } else {
        /* Everything else */
        deltar = mindr - p->row;
        deltac = mindc - p->col;
        copydbuf(deltar, deltac);
    }

    if (dv1 && (v1 || !v2)) {
        sync_refs();
        flush_saved();
    }

    if (dv1 == NULL) {
        if (qbuf && delbuf[qbuf]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx--] = NULL;
        }
        qbuf = 0;
    }
    error("Copy done.");
}

static void copydbuf(int deltar, int deltac) {
    int vr, vc;
    struct ent *p = delbuf[dbidx];
    struct ent *n;

    while (p) {
        vr = p->row + deltar;
        vc = p->col + deltac;
        n = lookat(vr, vc);
        if (n->flags & IS_LOCKED)
            continue;
        copyent(n, p, deltar, deltac, 0, 0, maxrow, maxcol, 0);
        p = p->next;
    }
}

/* ERASE a Range of cells */
void eraser(struct ent *v1, struct ent *v2) {
    int i;
    struct ent *obuf = NULL;

    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 1];
    delbuf[DELBUFSIZE - 1] = NULL;
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 1];
    delbuffmt[DELBUFSIZE - 1] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    flush_saved();
    if (qbuf) {
        if (dbidx < 0) dbidx++;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        flush_saved();
        obuf = delbuf[qbuf];    /* orig. contents of the del. buffer */
    }
    erase_area(v1->row, v1->col, v2->row, v2->col, 0);
    sync_refs();
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
    for (i = DELBUFSIZE - 1; i > DELBUFSIZE - 9; i--) {
        delbuf[i] = delbuf[i-1];
        delbuffmt[i] = delbuffmt[i-1];
    }
    delbuf[DELBUFSIZE - 9] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 9] = delbuffmt[dbidx];
    FullUpdate++;
    modflg++;
}

/* YANK a Range of cells */
void yankr(struct ent *v1, struct ent *v2) {
    int i, qtmp;
    struct ent *obuf = NULL;

    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
    delbuffmt[DELBUFSIZE - 10] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    flush_saved();
    if (qbuf) {
        if (dbidx < 0) dbidx++;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        flush_saved();
        obuf = delbuf[qbuf];    /* orig. contents of the del. buffer */
    }
    qtmp = qbuf;
    qbuf = 0;
    yank_area(v1->row, v1->col, v2->row, v2->col);
    qbuf = qtmp;
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
    delbuf[DELBUFSIZE - 10] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 10] = delbuffmt[dbidx];
}

/* MOVE a Range of cells */
void mover(struct ent *d, struct ent *v1, struct ent *v2) {
    move_area(d->row, d->col, v1->row, v1->col, v2->row, v2->col);
    sync_refs();
    FullUpdate++;
}

/* Goto subroutines */

static void g_free(void) {
    switch (gs.g_type) {
    case G_STR:
    case G_NSTR: set_string(&gs.g_s, NULL); break;
    default: break;
    }
    gs.g_type = G_NONE;
    gs.errsearch = 0;
}

/* repeat the last goto command */
void go_last(void) {
    int num = 0;

    switch (gs.g_type) {
    case G_NONE:
        error("Nothing to repeat");
        break;
    case G_NUM:
        num_search(gs.g_n, gs.g_row, gs.g_col,
                   gs.g_lastrow, gs.g_lastcol, gs.errsearch);
        break;
    case G_CELL:
        moveto(gs.g_row, gs.g_col, gs.g_lastrow, gs.g_lastcol, gs.strow, gs.stcol);
        break;
    case G_XSTR:
        num++;
        FALLTHROUGH;
    case G_NSTR:
        num++;
        FALLTHROUGH;
    case G_STR:
        str_search(gs.g_s, gs.g_row, gs.g_col, gs.g_lastrow, gs.g_lastcol, num);
        break;

    default:
        error("go_last: internal error");
        break;
    }
}

/* Place the cursor on a given cell.  If cornerrow >= 0, place the cell
 * at row cornerrow and column cornercol in the upper left corner of the
 * screen if possible.
 */
void moveto(int row, int col, int lastrow, int lastcol,
            int cornerrow, int cornercol)
{
    int i;

    if (!loading && row != -1 && (row != currow || col != curcol))
        remember(0);

    currow = row;
    curcol = col;
    g_free();
    gs.g_type = G_CELL;
    gs.g_row = currow;
    gs.g_col = curcol;
    gs.g_lastrow = lastrow;
    gs.g_lastcol = lastcol;
    gs.strow = cornerrow;
    gs.stcol = cornercol;
    if (cornerrow >= 0) {
        strow = cornerrow;
        stcol = cornercol;
        gs.stflag = 1;
    } else {
        gs.stflag = 0;
    }
    for (rowsinrange = 0, i = row; i <= lastrow; i++) {
        if (row_hidden[i])
            continue;
        rowsinrange++;
    }
    for (colsinrange = 0, i = col; i <= lastcol; i++) {
        if (col_hidden[i])
            continue;
        colsinrange += fwidth[i];
    }
    FullUpdate++;
    if (loading) {
        update(1);
        changed = 0;
    } else {
        remember(1);
    }
}

/*
 * 'goto' either a given number,'error', or 'invalid' starting at currow,curcol
 */
void num_search(double n, int firstrow, int firstcol, int lastrow,
                int lastcol, int errsearch)
{
    struct ent *p;
    int r,c;
    int endr, endc;

    if (!loading)
        remember(0);

    g_free();
    gs.g_type = G_NUM;
    gs.g_n = n;
    gs.g_row = firstrow;
    gs.g_col = firstcol;
    gs.g_lastrow = lastrow;
    gs.g_lastcol = lastcol;
    gs.errsearch = errsearch;

    if (currow >= firstrow && currow <= lastrow &&
            curcol >= firstcol && curcol <= lastcol) {
        endr = currow;
        endc = curcol;
    } else {
        endr = lastrow;
        endc = lastcol;
    }
    r = endr;
    c = endc;
    while (1) {
        if (c < lastcol) {
            c++;
        } else {
            if (r < lastrow) {
                while (++r < lastrow && row_hidden[r])
                    continue;
                c = firstcol;
            } else {
                r = firstrow;
                c = firstcol;
            }
        }
        p = *ATBL(tbl, r, c);
        if (!col_hidden[c] && p && (p->flags & IS_VALID) &&
                (errsearch || (p->v == n)) && (!errsearch ||
                (p->cellerror == errsearch)))   /* CELLERROR vs CELLINVALID */
            break;
        if (r == endr && c == endc) {
            if (errsearch) {
                error("no %s cell found", errsearch == CELLERROR ? "ERROR" :
                      "INVALID");
            } else {
                error("Number not found");
            }
            return;
        }
    }

    currow = r;
    curcol = c;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    if (loading) {
        update(1);
        changed = 0;
    } else {
        remember(1);
    }
}

/* 'goto' a cell containing a matching string */
void str_search(const char *s, int firstrow, int firstcol, int lastrow, int lastcol, int num) {
    char field[FBUFLEN];
    struct ent *p;
    int found = 0;
    int row, col, endr, endc;
#if defined(RE_COMP) || defined(REGCMP)
    char *tmp = NULL;
#endif
#if defined(REGCOMP)
    regex_t preg;
    int errcode;
#endif

#if defined(REGCOMP)
    if ((errcode = regcomp(&preg, s, REG_EXTENDED))) {
        char buf[160];
        regerror(errcode, &preg, buf, sizeof(buf));
        error("%s", buf);
        return;
    }
#endif
#if defined(RE_COMP)
    if ((tmp = re_comp(s)) != NULL) {
        error("%s", tmp);
        return;
    }
#endif
#if defined(REGCMP)
    if ((tmp = regcmp(s, NULL)) == NULL) {
        cellerror = CELLERROR;
        error("Invalid search string");
        return;
    }
#endif
    if (!loading)
        remember(0);

    g_free();
    gs.g_type = G_STR + num;
    gs.g_s = scxdup(s);
    gs.g_row = firstrow;
    gs.g_col = firstcol;
    gs.g_lastrow = lastrow;
    gs.g_lastcol = lastcol;
    if (currow >= firstrow && currow <= lastrow &&
            curcol >= firstcol && curcol <= lastcol) {
        endr = currow;
        endc = curcol;
    } else {
        endr = lastrow;
        endc = lastcol;
    }
    row = endr;
    col = endc;
    for (;;) {
        if (col < lastcol) {
            col++;
        } else {
            col = firstcol;
            if (row < lastrow) {
                while (++row < lastrow && row_hidden[row])
                    continue;
            } else {
                row = firstrow;
            }
        }
        if (!col_hidden[col] && (p = *ATBL(tbl, row, col))) {
            const char *str = field;
            int fieldlen = fwidth[col];
            *field = '\0';
            if (gs.g_type == G_NSTR) {
                if (p->cellerror) {
                    str = (p->cellerror == CELLERROR) ? "ERROR" : "INVALID";
                } else
                if (p->flags & IS_VALID) {
                    const char *cfmt = p->format;
                    // XXX: should use colformat[realfmt[col]]
                    if (cfmt) {
                        if (*cfmt == ctl('d')) {
                            time_t v = (time_t)(p->v);
                            // XXX: must check format string
                            ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
                                (field, sizeof(field), cfmt + 1, localtime(&v));
                        } else {
                            format(cfmt, precision[col], p->v, field, sizeof(field));
                        }
                    } else {
                        engformat(realfmt[col], fieldlen, precision[col], p->v, field, sizeof(field));
                    }
                }
            } else if (gs.g_type == G_XSTR) {
                if (p->expr) {
                    decompile(field, sizeof field, p->expr);
                    if (*field == '?')
                        *field = '\0';
                }
            }
            if (gs.g_type == G_STR) {
                str = p->label;
            }
            if (str && *str
#if defined(REGCOMP)
            &&  (regexec(&preg, str, 0, NULL, 0) == 0)
#else
#if defined(RE_COMP)
            &&  (re_exec(str) != 0)
#else
#if defined(REGCMP)
            &&  (regex(tmp, str) != NULL)
#else
            &&  (strcmp(s, str) == 0)
#endif
#endif
#endif
                ) {
                found = 1;
                break;
            }
        }
        if (row == endr && col == endc)
            break;
    }
#if defined(REGCOMP)
    regfree(&preg);
#endif
#if defined(REGCMP)
    free(tmp);
#endif
    if (found) {
        currow = row;
        curcol = col;
        rowsinrange = 1;
        colsinrange = fwidth[col];
        if (loading) {
            update(1);
            changed = 0;
        } else {
            remember(1);
        }
    } else {
        error("String not found");
    }
}

/* fill a range with constants */
void fill(struct ent *v1, struct ent *v2, double start, double inc) {
    int r, c;
    struct ent *n;
    int minr = v1->row;
    int minc = v1->col;
    int maxr = v2->row;
    int maxc = v2->col;
    if (minr > maxr) SWAPINT(minr, maxr);
    if (minc > maxc) SWAPINT(minc, maxc);
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    FullUpdate++;
    if (calc_order == BYROWS) {
        for (r = minr; r <= maxr; r++) {
            for (c = minc; c <= maxc; c++) {
                n = lookat(r, c);
                if (n->flags & IS_LOCKED) continue;
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->flags |= IS_CHANGED | IS_VALID;
                n->flags &= ~IS_CLEARED;
            }
        }
    } else
    if (calc_order == BYCOLS) {
        for (c = minc; c <= maxc; c++) {
            for (r = minr; r <= maxr; r++) {
                n = lookat(r, c);
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->flags |= IS_CHANGED | IS_VALID;
                n->flags &= ~IS_CLEARED;
            }
        }
    } else {
        error(" Internal error calc_order");
    }
    changed++;
}

/* lock a range of cells */

void lock_cells(struct ent *v1, struct ent *v2) {
    int r, c;
    struct ent *n;
    int minr = v1->row;
    int minc = v1->col;
    int maxr = v2->row;
    int maxc = v2->col;
    if (minr > maxr) SWAPINT(minr, maxr);
    if (minc > maxc) SWAPINT(minc, maxc);
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            n = lookat(r, c);
            n->flags |= IS_LOCKED;
        }
    }
}

/* unlock a range of cells */

void unlock_cells(struct ent *v1, struct ent *v2) {
    int r, c;
    struct ent *n;
    int minr = v1->row;
    int minc = v1->col;
    int maxr = v2->row;
    int maxc = v2->col;
    if (minr > maxr) SWAPINT(minr, maxr);
    if (minc > maxc) SWAPINT(minc, maxc);
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            n = lookat(r, c);
            n->flags &= ~IS_LOCKED;
        }
    }
}

/* clear the numeric part of a cell */
void unlet(struct ent *v) {
    v->v = 0.0;
    if (v->expr && !(v->flags & IS_STREXPR)) {
        efree(v->expr);
        v->expr = NULL;
    }
    v->cellerror = CELLOK;
    v->flags &= ~IS_VALID;
    v->flags |= IS_CHANGED;
    changed++;
    FullUpdate++;
    modflg++;
}

/* set the numeric part of a cell */
void let(struct ent *v, SCXMEM struct enode *e) {
    double val;
    // XXX: test for constant expression is potentially incorrect
    unsigned isconstant = constant(e);

    if (locked_cell(v->row, v->col))
        return;
    if (v->row == currow && v->col == curcol)
        cellassign = 1;

    val = 0.0;
    if (!loading || isconstant) {
        exprerr = 0;
        signal(SIGFPE, eval_fpe);
        if (setjmp(fpe_save)) {
            error("Floating point exception in cell %s",
                  v_name(v->row, v->col));
            cellerror = CELLERROR;
            val = 0.0;
        } else {
            cellerror = CELLOK;
            val = eval(e);
        }
        if (v->cellerror != cellerror) {
            v->cellerror = cellerror;
            v->flags |= IS_CHANGED;
            changed++;
            modflg++;
            FullUpdate++;
        }
        signal(SIGFPE, doquit);
        if (exprerr) {
            efree(e);
            return;
        }
    }

    if (isconstant) {
        /* prescale input unless it has a decimal */
        // XXX: sc_decimal is a horrible hack!
        if (!loading && !sc_decimal && (prescale < 0.9999999))
            val *= prescale;
        sc_decimal = FALSE;

        v->v = val;

        if (!(v->flags & IS_STREXPR)) {
            efree(v->expr);
            v->expr = NULL;
        }
        efree(e);
    } else {
        efree(v->expr);
        v->expr = e;
        v->flags &= ~IS_STREXPR;
    }

    changed++;
    modflg++;
    v->flags |= IS_CHANGED | IS_VALID;

    if (!loading) {
        int i;

        // XXX: why 28?
        for (i = 36; i > 28; i--) {
            savedrow[i] = savedrow[i-1];
            savedcol[i] = savedcol[i-1];
            savedstrow[i] = savedstrow[i-1];
            savedstcol[i] = savedstcol[i-1];
        }

        savedrow[28] = v->row;
        savedcol[28] = v->col;
        savedstrow[28] = savedstrow[27];
        savedstcol[28] = savedstcol[27];
    }
}

void slet(struct ent *v, SCXMEM struct enode *se, int align) {
    SCXMEM char *p;

    if (locked_cell(v->row, v->col))
        return;
    if (v->row == currow && v->col == curcol)
        cellassign = 1;
    exprerr = 0;
    signal(SIGFPE, eval_fpe);
    if (setjmp(fpe_save)) {
        // XXX: potential memory leak in the evaluator
        error("Floating point exception in cell %s",
              v_name(v->row, v->col));
        cellerror = CELLERROR;
        p = scxdup("");
    } else {
        cellerror = CELLOK;
        p = seval(se);
    }
    if (v->cellerror != cellerror) {
        v->cellerror = cellerror;
        v->flags |= IS_CHANGED;
        changed++;
        modflg++;
        FullUpdate++;
    }
    signal(SIGFPE, doquit);
    if (exprerr) {
        scxfree(p);
        efree(se);
        return;
    }
    if (!loading) {
        int i;

        // XXX: why 28?
        for (i = 36; i > 28; i--) {
            savedrow[i] = savedrow[i-1];
            savedcol[i] = savedcol[i-1];
            savedstrow[i] = savedstrow[i-1];
            savedstcol[i] = savedstcol[i-1];
        }

        savedrow[28] = v->row;
        savedcol[28] = v->col;
        savedstrow[28] = savedstrow[27];
        savedstcol[28] = savedstcol[27];
    }
    set_string(&v->label, p);
    v->flags &= ~ALIGN_MASK;
    v->flags |= IS_CHANGED | align;
    if (v->expr) {
        efree(v->expr);
        v->expr = NULL;
        v->flags &= ~IS_STREXPR;
    }
    // XXX: test for constant expression is potentially incorrect
    if (constant(se)) {
        efree(se);
    } else {
        v->expr = se;
        v->flags |= IS_STREXPR;
    }
    FullUpdate++;
    changed++;
    modflg++;
}

void format_cell(struct ent *v1, struct ent *v2, const char *s) {
    int r, c;
    struct ent *n;
    int minr = v1->row;
    int minc = v1->col;
    int maxr = v2->row;
    int maxc = v2->col;
    if (minr > maxr) SWAPINT(minr, maxr);
    if (minc > maxc) SWAPINT(minc, maxc);
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minc < 0) minc = 0;

    FullUpdate++;
    modflg++;

    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            n = lookat(r, c);
            if (locked_cell(n->row, n->col))
                continue;
            set_cstring(&n->format, s && *s ? s : NULL);
            n->flags |= IS_CHANGED;
        }
    }
}

void clearent(struct ent *v) {
    if (v) {
        v->v = 0.0;
        set_string(&v->label, NULL);
        efree(v->expr);
        v->expr = NULL;
        set_string(&v->format, NULL);
        v->cellerror = 0;
        v->flags = IS_CHANGED | IS_CLEARED;
        // XXX: should clear other fields?
        //      ncol, nrow, nlastcol, nlastrow
        //      next
        FullUpdate++;  // XXX: really?
        changed++;
        modflg++;
    }
}

/*
 * Say if an expression is a constant (return 1) or not.
 */
static int constant(struct enode *e) {
    return (e == NULL
        ||  e->op == O_CONST
        ||  e->op == O_SCONST
        ||  (e->op == 'm' && constant(e->e.o.left))
        ||  (e->op != O_VAR
        &&   !(e->op & REDUCE)
        &&   constant(e->e.o.left)
        &&   constant(e->e.o.right)
        &&   e->op != EXT     /* functions look like constants but aren't */
        &&   e->op != NVAL
        &&   e->op != SVAL
        &&   e->op != NOW
        &&   e->op != MYROW
        &&   e->op != MYCOL
        &&   e->op != LASTROW
        &&   e->op != LASTCOL
        &&   e->op != NUMITER
        &&   e->op != FILENAME
        &&   optimize));
}

void efree(SCXMEM struct enode *e) {
    if (e) {
        if (e->op != O_VAR && e->op != O_CONST && e->op != O_SCONST
                && !(e->op & REDUCE)) {
            efree(e->e.o.left);
            efree(e->e.o.right);
        }
        if (e->op == O_SCONST)
            scxfree(e->e.s);
        else
        if (e->op == EXT)
            scxfree(e->e.o.s);
        e->e.o.left = freeenodes;
        freeenodes = e;
    }
}

char *coltoa(int col) {
    static unsigned int bufn;
    static char buf[4][4];
    char *rname = buf[bufn++ & 3];
    char *p = rname;

    if (col > 25) {
        *p++ = col / 26 + 'A' - 1;
        col %= 26;
    }
    *p++ = col + 'A';
    *p = '\0';
    return rname;
}

/*---- decompile expressions ----*/

static void out_const(buf_t buf, double v) {
    buf_printf(buf, "%.15g", v);
}

static void out_sconst(buf_t buf, const char *s) {
    // XXX: potentially incorrect for embedded `"` and other control characters
    buf_putc(buf, '"');
    buf_puts(buf, s);
    buf_putc(buf, '"');
}

static void out_var(buf_t buf, struct ent_ptr v, int usename) {
    struct range *r;

    if (!v.vp || (v.vp->flags & IS_DELETED)) {
        buf_puts(buf, "@ERR");
    } else
    if (usename && (r = find_range_coords(v.vp, v.vp)) != NULL && !r->r_is_range) {
        buf_puts(buf, r->r_name);
    } else {
        buf_printf(buf, "%s%s%s%d",
                   (v.vf & FIX_COL) ? "$" : "", coltoa(v.vp->col),
                   (v.vf & FIX_ROW) ? "$" : "", v.vp->row);
    }
}

static void out_range(buf_t buf, struct enode *e) {
    struct range *r;

    if ((r = find_range_coords(e->e.r.left.vp, e->e.r.right.vp)) != NULL && r->r_is_range) {
        buf_puts(buf, r->r_name);
    } else {
        out_var(buf, e->e.r.left, 0);
        buf_putc(buf, ':');
        out_var(buf, e->e.r.right, 0);
    }
}

/*
 *      To make list elements come out in the same order
 *      they were entered, we must do a depth-first eval
 *      of the ELIST tree
 */
static void decompile_list(buf_t buf, struct enode *p) {
    if (p) {
        if (p->e.o.left) {
            decompile_list(buf, p->e.o.left);    /* depth first */
            buf_putc(buf, ',');
        }
        decompile_node(buf, p->e.o.right, 0);
    }
}

static void unary_arg(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    decompile_node(buf, e->e.o.left, 30);
}

static void one_arg(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    buf_putc(buf, '(');
    decompile_node(buf, e->e.o.left, 0);
    buf_putc(buf, ')');
}

static void two_arg(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    buf_putc(buf, '(');
    decompile_node(buf, e->e.o.left, 0);
    // XXX: should test e->e.o.right
    buf_putc(buf, ',');
    decompile_node(buf, e->e.o.right, 0);
    buf_putc(buf, ')');
}

static void three_arg(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    buf_putc(buf, '(');
    decompile_node(buf, e->e.o.left, 0);
    buf_putc(buf, ',');
    decompile_node(buf, e->e.o.right->e.o.left, 0);
    buf_putc(buf, ',');
    decompile_node(buf, e->e.o.right->e.o.right, 0);
    buf_putc(buf, ')');
}

static void range_arg(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    buf_putc(buf, '(');
    out_range(buf, e);
    buf_putc(buf, ')');
}

static void two_arg_index(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    buf_putc(buf, '(');
    out_range(buf, e->e.o.left);
    buf_putc(buf, ',');
    decompile_node(buf, e->e.o.right->e.o.left, 0);
    buf_putc(buf, ',');
    decompile_node(buf, e->e.o.right->e.o.right, 0);
    buf_putc(buf, ')');
}

static void index_arg(buf_t buf, const char *s, struct enode *e) {
    if (e->e.o.right && e->e.o.right->op == ',') {
        two_arg_index(buf, s, e);
        return;
    }
    buf_puts(buf, s);
    buf_putc(buf, '(');
    out_range(buf, e->e.o.left);
    if (e->e.o.right) {
        buf_putc(buf, ',');
        decompile_node(buf, e->e.o.right, 0);
    }
    buf_putc(buf, ')');
}

static void list_arg(buf_t buf, const char *s, struct enode *e) {
    buf_puts(buf, s);
    buf_putc(buf, '(');
    decompile_node(buf, e->e.o.right, 0);
    buf_putc(buf, ',');
    decompile_list(buf, e->e.o.left);
    buf_putc(buf, ')');
}

void decompile_node(buf_t buf, struct enode *e, int priority) {
    if (e) {
        int mypriority;
        switch (e->op) {
        default: mypriority = 99; break;
        case ';': mypriority = 1; break;
        case '?': mypriority = 2; break;
        case ':': mypriority = 3; break;
        case '|': mypriority = 4; break;
        case '&': mypriority = 5; break;
        case '<': case '=': case '>': mypriority = 6; break;
        case '+': case '-': case '#': mypriority = 8; break;
        case '*': case '/': case '%': mypriority = 10; break;
        case '^': mypriority = 12; break;
        }
        if (mypriority < priority) buf_putc(buf, '(');

        switch (e->op) {
        case 'f':       unary_arg(buf, "@fixed ", e); break;
        case 'F':       unary_arg(buf, "(@fixed)", e); break;
        case 'm':       unary_arg(buf, "-", e); break;
        case '!':       unary_arg(buf, "!", e); break;
        case O_VAR:     out_var(buf, e->e.v, 1); break;
        case O_CONST:   out_const(buf, e->e.k); break;
        case O_SCONST:  out_sconst(buf, e->e.s); break;

        case SUM:       index_arg(buf, "@sum", e); break;
        case PROD:      index_arg(buf, "@prod", e); break;
        case AVG:       index_arg(buf, "@avg", e); break;
        case COUNT:     index_arg(buf, "@count", e); break;
        case STDDEV:    index_arg(buf, "@stddev", e); break;
        case MAX:       index_arg(buf, "@max", e); break;
        case MIN:       index_arg(buf, "@min", e); break;
        case REDUCE | 'R': range_arg(buf, "@rows", e); break;
        case REDUCE | 'C': range_arg(buf, "@cols", e); break;

        case ABS:       one_arg(buf, "@abs", e); break;
        case ACOS:      one_arg(buf, "@acos", e); break;
        case ASIN:      one_arg(buf, "@asin", e); break;
        case ATAN:      one_arg(buf, "@atan", e); break;
        case ATAN2:     two_arg(buf, "@atan2", e); break;
        case CEIL:      one_arg(buf, "@ceil", e); break;
        case COS:       one_arg(buf, "@cos", e); break;
        case EXP:       one_arg(buf, "@exp", e); break;
        case FABS:      one_arg(buf, "@fabs", e); break;
        case FLOOR:     one_arg(buf, "@floor", e); break;
        case HYPOT:     two_arg(buf, "@hypot", e); break;
        case LOG:       one_arg(buf, "@ln", e); break;
        case LOG10:     one_arg(buf, "@log", e); break;
        case POW:       two_arg(buf, "@pow", e); break;
        case SIN:       one_arg(buf, "@sin", e); break;
        case SQRT:      one_arg(buf, "@sqrt", e); break;
        case TAN:       one_arg(buf, "@tan", e); break;
        case DTR:       one_arg(buf, "@dtr", e); break;
        case RTD:       one_arg(buf, "@rtd", e); break;
        case RND:       one_arg(buf, "@rnd", e); break;
        case ROUND:     two_arg(buf, "@round", e); break;
        case HOUR:      one_arg(buf, "@hour", e); break;
        case MINUTE:    one_arg(buf, "@minute", e); break;
        case SECOND:    one_arg(buf, "@second", e); break;
        case MONTH:     one_arg(buf, "@month", e); break;
        case DAY:       one_arg(buf, "@day", e); break;
        case YEAR:      one_arg(buf, "@year", e); break;
        case NOW:       buf_puts(buf, "@now"); break;
        case DATE:      if (e->e.o.right)
                            two_arg(buf, "@date", e);
                        else
                            one_arg(buf, "@date", e);
                        break;
        case FMT:       two_arg(buf, "@fmt", e); break;
        case UPPER:     one_arg(buf, "@upper", e); break;
        case LOWER:     one_arg(buf, "@lower", e); break;
        case CAPITAL:   one_arg(buf, "@capital", e); break;
        case DTS:       three_arg(buf, "@dts", e); break;
        case TTS:       three_arg(buf, "@tts", e); break;
        case STON:      one_arg(buf, "@ston", e); break;
        case EQS:       two_arg(buf, "@eqs", e); break;
        case LMAX:      list_arg(buf, "@max", e); break;
        case LMIN:      list_arg(buf, "@min", e); break;
        case FV:        three_arg(buf, "@fv", e); break;
        case PV:        three_arg(buf, "@pv", e); break;
        case PMT:       three_arg(buf, "@pmt", e); break;
        case NVAL:      two_arg(buf, "@nval", e); break;
        case SVAL:      two_arg(buf, "@sval", e); break;
        case EXT:       two_arg(buf, "@ext", e); break;
        case SUBSTR:    three_arg(buf, "@substr", e); break;
        case STINDEX:   index_arg(buf, "@stindex", e); break;
        case INDEX:     index_arg(buf, "@index", e); break;
        case LOOKUP:    index_arg(buf, "@lookup", e); break;
        case HLOOKUP:   two_arg_index(buf, "@hlookup", e); break;
        case VLOOKUP:   two_arg_index(buf, "@vlookup", e); break;
        case IF:        three_arg(buf, "@if", e); break;
        case MYROW:     buf_puts(buf, "@myrow"); break;
        case MYCOL:     buf_puts(buf, "@mycol"); break;
        case LASTROW:   buf_puts(buf, "@lastrow"); break;
        case LASTCOL:   buf_puts(buf, "@lastcol"); break;
        case COLTOA:    one_arg(buf, "@coltoa", e); break;
        case FILENAME:  one_arg(buf, "@filename", e); break;
        case NUMITER:   buf_puts(buf, "@numiter"); break;
        case ERR_:      buf_puts(buf, "@err"); break;
        case PI_:       buf_puts(buf, "@pi"); break;
        case BLACK:     buf_puts(buf, "@black"); break;
        case RED:       buf_puts(buf, "@red"); break;
        case GREEN:     buf_puts(buf, "@green"); break;
        case YELLOW:    buf_puts(buf, "@yellow"); break;
        case BLUE:      buf_puts(buf, "@blue"); break;
        case MAGENTA:   buf_puts(buf, "@magenta"); break;
        case CYAN:      buf_puts(buf, "@cyan"); break;
        case WHITE:     buf_puts(buf, "@white"); break;
        default:        // XXX: priority seems bogus
                        decompile_node(buf, e->e.o.left, mypriority);
                        buf_putc(buf, e->op);
                        decompile_node(buf, e->e.o.right, mypriority + 1);
                        break;
        }
        if (mypriority < priority) buf_putc(buf, ')');
    } else {
        buf_putc(buf, '?');
    }
}

int decompile(char *dest, size_t size, struct enode *e) {
    buf_t buf;
    buf_init(buf, dest, size);
    decompile_node(buf, e, 0);
    return buf->len;
}

void editfmt(buf_t buf, int row, int col) {
    struct ent *p;

    p = lookat(row, col);
    if (p->format) {
        buf_setf(buf, "fmt %s ", v_name(row, col));
        out_sconst(buf, p->format);
    } else {
        buf_reset(buf);
    }
}

void editv(buf_t buf, int row, int col) {
    struct ent *p = lookat(row, col);

    buf_setf(buf, "let %s = ", v_name(row, col));
    if (p->flags & IS_VALID) {
        if ((p->flags & IS_STREXPR) || p->expr == NULL) {
            out_const(buf, p->v);
        } else {
            decompile_node(buf, p->expr, 0);
        }
    }
}

void edits(buf_t buf, int row, int col) {
    struct ent *p = lookat(row, col);
    const char *command;

    switch (p->flags & ALIGN_MASK) {
    default:
    case ALIGN_LEFT:    command = "leftstring";  break;
    case ALIGN_CENTER:  command = "label";       break;
    case ALIGN_RIGHT:   command = "rightstring"; break;
    }
    buf_setf(buf, "%s %s = ", command, v_name(row, col));
    if ((p->flags & IS_STREXPR) && p->expr) {
        decompile_node(buf, p->expr, 0);
    } else if (p->label) {
        out_sconst(buf, p->label);
    } else {
        /* output a single `"` for the user to start entering the string */
        buf_putc(buf, '"');
    }
}

#ifdef RINT
/*      round-to-even, also known as ``banker's rounding''.
        With round-to-even, a number exactly halfway between two values is
        rounded to whichever is even; e.g. rnd(0.5)=0, rnd(1.5)=2,
        rnd(2.5)=2, rnd(3.5)=4.  This is the default rounding mode for
        IEEE floating point, for good reason: it has better numeric
        properties.  For example, if X+Y is an integer,
        then X+Y = rnd(X)+rnd(Y) with round-to-even,
        but not always with sc's rounding (which is
        round-to-positive-infinity).  I ran into this problem when trying to
        split interest in an account to two people fairly.
*/

double rint(double d) {
    /* as sent */
    double fl = floor(d), fr = d - fl;
    return fr < 0.5 || fr == 0.5 && fl == floor(fl / 2) * 2 ? fl : ceil(d);
}
#endif

static double rand_between(double aa, double bb) {
    long int a = (long int)aa;
    long int b = (long int)bb;
    if (a > b) {
        long int c = a;
        a = b;
        b = c;
    }
    if (a == b)
        return a;
    else
        return a + (long int)rand() * (double)(b - a + 1) / ((double)RAND_MAX + 1);
}
