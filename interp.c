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

#define ISVALID(r,c)    ((r)>=0 && (r)<maxrows && (c)>=0 && (c)<maxcols)

static jmp_buf fpe_save;
static int exprerr;     /* Set by eval() and seval() if expression errors */
double prescale = 1.0;  /* Prescale for constants in let() */
int extfunc = 0;        /* Enable/disable external functions */
int loading = 0;        /* Set when readfile() is active */
int gmyrow, gmycol;     /* globals used to implement @myrow, @mycol cmds */
static int rowoffset = 0, coloffset = 0;   /* row & col offsets for range functions */
int propagation = 10;   /* max number of times to try calculation */
static int repct = 1;   /* Make repct a global variable so that the
                           function @numiter can access it */

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
static struct enode *freeenodes = NULL;

static double dolookup(struct enode *val, int minr, int minc, int maxr,
                       int maxc, int offr, int offc);
static double fn1_eval(double (*fn)(double), double arg);
static double fn2_eval(double (*fn)(double, double), double arg1, double arg2);
static int RealEvalAll(void);
static int constant_expr(struct enode *e, int full);
static void RealEvalOne(struct ent *p, int i, int j, int *chgct);

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

static double donow(void) {
    // XXX: should use a more precise time value
    return (double)time(NULL);
}

static double dotime(int which, double when) {
    static time_t t_cache;
    static struct tm tm_cache;
    time_t tloc;

    tloc = (time_t)when;

    // XXX: this primitive cacheing system fails
    //      as soon as there are more than 1 time value
    //      and it will fail if the current TZ changes
    if (!t_cache || tloc != t_cache) {
        struct tm *tp = localtime(&tloc);
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
    &&  ((col = atocol(colstr, &collen)) >= 0)   /* has column */
    &&  (colstr[collen] == '\0')                 /* exact match */
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
    case NOW:       return donow();
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

static void RealEvalOne(struct ent *p, int i, int j, int *chgct) {
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

/* set the calculation order */
void setcalcorder(int i) {
    if (i == BYROWS || i == BYCOLS)
        calc_order = i;
}

void setautocalc(int i) {
    autocalc = i;
}

/*---------------- expression tree construction ----------------*/

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

struct SCXMEM enode *new_var(int op, cellref_t cr) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = op;
    p->e.v.vf = cr.vf;
    p->e.v.vp = lookat(cr.row, cr.col);
    return p;
}

struct SCXMEM enode *new_range(int op, rangeref_t rr) {
    SCXMEM struct enode *p;

    if (freeenodes) {
        p = freeenodes;
        freeenodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(struct enode));
    }
    p->op = op;
    p->e.r.left.vf = rr.left.vf;
    p->e.r.left.vp = lookat(rr.left.row, rr.left.col);
    p->e.r.right.vf = rr.right.vf;
    p->e.r.right.vp = lookat(rr.right.row, rr.right.col);
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

struct enode *copye(struct enode *e, int Rdelta, int Cdelta,
                    int r1, int c1, int r2, int c2, int transpose)
{
    struct enode *ret;
    // XXX: horrible hack to copy aggregate ops
    static struct enode *range = NULL;

    if (e == NULL)
        ret = NULL;
    else
    if (e->op & REDUCE) {
        int newrow, newcol;
        if (freeenodes) {
            ret = freeenodes;
            freeenodes = ret->e.o.left;
        } else {
            ret = scxmalloc(sizeof (struct enode));
        }
        ret->op = e->op;
        newrow = e->e.r.left.vf & FIX_ROW ||
                 e->e.r.left.vp->row < r1 || e->e.r.left.vp->row > r2 ||
                 e->e.r.left.vp->col < c1 || e->e.r.left.vp->col > c2 ?
                 e->e.r.left.vp->row :
                 transpose ? r1 + Rdelta + e->e.r.left.vp->col - c1 :
                 e->e.r.left.vp->row + Rdelta;
        newcol = e->e.r.left.vf & FIX_COL ||
                 e->e.r.left.vp->row < r1 || e->e.r.left.vp->row > r2 ||
                 e->e.r.left.vp->col < c1 || e->e.r.left.vp->col > c2 ?
                 e->e.r.left.vp->col :
                 transpose ? c1 + Cdelta + e->e.r.left.vp->row - r1 :
                 e->e.r.left.vp->col + Cdelta;
        ret->e.r.left.vp = lookat(newrow, newcol);
        ret->e.r.left.vf = e->e.r.left.vf;
        newrow = e->e.r.right.vf & FIX_ROW ||
                 e->e.r.right.vp->row < r1 || e->e.r.right.vp->row > r2 ||
                 e->e.r.right.vp->col < c1 || e->e.r.right.vp->col > c2 ?
                 e->e.r.right.vp->row :
                 transpose ? r1 + Rdelta + e->e.r.right.vp->col - c1 :
                 e->e.r.right.vp->row + Rdelta;
        newcol = e->e.r.right.vf & FIX_COL ||
                 e->e.r.right.vp->row < r1 || e->e.r.right.vp->row > r2 ||
                 e->e.r.right.vp->col < c1 || e->e.r.right.vp->col > c2 ?
                 e->e.r.right.vp->col :
                 transpose ? c1 + Cdelta + e->e.r.right.vp->row - r1 :
                 e->e.r.right.vp->col + Cdelta;
        ret->e.r.right.vp = lookat(newrow, newcol);
        ret->e.r.right.vf = e->e.r.right.vf;
    } else {
        struct enode *temprange = NULL;

        if (freeenodes) {
            ret = freeenodes;
            freeenodes = ret->e.o.left;
        } else
            ret = scxmalloc(sizeof (struct enode));
        ret->op = e->op;
        switch (ret->op) {
        case SUM:
        case PROD:
        case AVG:
        case COUNT:
        case STDDEV:
        case MAX:
        case MIN:
            temprange = range;
            range = e->e.o.left;
            r1 = 0;
            c1 = 0;
            r2 = maxrow;
            c2 = maxcol;
        }
        switch (ret->op) {
        case 'v': {
                int newrow, newcol;
                if (range && e->e.v.vp->row >= range->e.r.left.vp->row &&
                        e->e.v.vp->row <= range->e.r.right.vp->row &&
                        e->e.v.vp->col >= range->e.r.left.vp->col &&
                        e->e.v.vp->col <= range->e.r.right.vp->col) {
                    newrow = range->e.r.left.vf & FIX_ROW ?
                             e->e.v.vp->row : e->e.v.vp->row + Rdelta;
                    newcol = range->e.r.left.vf & FIX_COL ?
                             e->e.v.vp->col : e->e.v.vp->col + Cdelta;
                } else {
                    newrow = e->e.v.vf & FIX_ROW ||
                             e->e.v.vp->row < r1 || e->e.v.vp->row > r2 ||
                             e->e.v.vp->col < c1 || e->e.v.vp->col > c2 ?
                             e->e.v.vp->row :
                             transpose ? r1 + Rdelta + e->e.v.vp->col - c1 :
                             e->e.v.vp->row + Rdelta;
                    newcol = e->e.v.vf & FIX_COL ||
                             e->e.v.vp->row < r1 || e->e.v.vp->row > r2 ||
                             e->e.v.vp->col < c1 || e->e.v.vp->col > c2 ?
                             e->e.v.vp->col :
                             transpose ? c1 + Cdelta + e->e.v.vp->row - r1 :
                             e->e.v.vp->col + Cdelta;
                }
                ret->e.v.vp = lookat(newrow, newcol);
                ret->e.v.vf = e->e.v.vf;
                break;
            }
        case 'k':
            ret->e.k = e->e.k;
            break;
        case 'f':
        case 'F':
            if (( range && ret->op == 'F') ||
                (!range && ret->op == 'f')   )
                Rdelta = Cdelta = 0;
            ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
                                  r1, c1, r2, c2, transpose);
            ret->e.o.right = NULL;
            break;
        case '$':
        case EXT:
            ret->e.s = scxdup(e->e.s);
            if (e->op == '$')       /* Drop through if ret->op is EXT */
                break;
            FALLTHROUGH;
        default:
            ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
                                  r1, c1, r2, c2, transpose);
            ret->e.o.right = copye(e->e.o.right, Rdelta, Cdelta,
                                   r1, c1, r2, c2, transpose);
            break;
        }
        switch (ret->op) {
        case SUM:
        case PROD:
        case AVG:
        case COUNT:
        case STDDEV:
        case MAX:
        case MIN:
            range = temprange;
        }
    }
    return ret;
}

/*
 * Say if an expression is a constant (return 1) or not.
 */
static int constant_expr(struct enode *e, int full) {
    return (e == NULL
        ||  e->op == O_CONST
        ||  e->op == O_SCONST
        ||  (e->op == 'm' && constant_expr(e->e.o.left, 0)) /* unary minus */
        ||  (full
        &&   e->op != O_VAR
        &&   !(e->op & REDUCE)
        &&   constant_expr(e->e.o.left, full)
        &&   constant_expr(e->e.o.right, full)
        &&   e->op != EXT     /* functions look like constants but aren't */
        &&   e->op != NVAL
        &&   e->op != SVAL
        &&   e->op != NOW
        &&   e->op != MYROW
        &&   e->op != MYCOL
        &&   e->op != LASTROW
        &&   e->op != LASTCOL
        &&   e->op != NUMITER
        &&   e->op != FILENAME));
}

// XXX: all these should go to cmds.c

/* clear the numeric part of a cell */
void unlet(cellref_t cr) {
    struct ent *v = lookat_nc(cr.row, cr.col);
    if (v == NULL)
        return;
    v->v = 0.0;
    if (v->expr && !(v->flags & IS_STREXPR)) {
        efree(v->expr);
        v->expr = NULL;
    }
    v->cellerror = CELLOK;
    v->flags &= ~IS_VALID;
    v->flags |= IS_CHANGED;
    FullUpdate++;
    changed++;
    modflg++;
}

static void push_mark(int row, int col) {
    int i;

    /* shift saved places */
    for (i = 36; i > 28; i--) {
        savedcr[i] = savedcr[i-1];
        savedst[i] = savedst[i-1];
    }

    /* save current cell and screen position */
    savedcr[28] = cellref(row, col);
    savedst[28] = savedst[27];
}

/* set the numeric part of a cell */
void let(cellref_t cr, SCXMEM struct enode *e) {
    struct ent *v = lookat(cr.row, cr.col);
    double val;
    // XXX: test for constant expression is potentially incorrect
    unsigned isconstant = constant_expr(e, optimize);

    // XXX: locked cell checking is done in vi.c
    //      should just return silently?
    if (v == NULL || locked_cell(v))
        return;

    val = 0.0;
    if (!loading || isconstant) {
        exprerr = 0;
        signal(SIGFPE, eval_fpe);
        if (setjmp(fpe_save)) {
            error("Floating point exception in cell %s",
                  v_name(cr.row, cr.col));
            cellerror = CELLERROR;
            val = 0.0;
        } else {
            cellerror = CELLOK;
            val = eval(e);
        }
        signal(SIGFPE, doquit);
        if (v->cellerror != cellerror) {
            v->cellerror = cellerror;
            v->flags |= IS_CHANGED;
            FullUpdate++;
            changed++;
            modflg++;
        }
        if (exprerr) {
            efree(e);
            return;
        }
    }

    if (isconstant) {
        /* prescale input unless it has a decimal */
        // XXX: sc_decimal is a horrible hack!
        //      should use a flag in the expression node
        if (!loading && !sc_decimal && (prescale < 0.9999999))
            val *= prescale;
        sc_decimal = FALSE;

        v->v = val;

        // XXX: should replace string value with number value
        //      hence should free expr and clear IS_STREXPR
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

    if (!loading)
        push_mark(cr.row, cr.col);
}

void slet(cellref_t cr, SCXMEM struct enode *se, int align) {
    struct ent *v = lookat(cr.row, cr.col);
    SCXMEM char *p;

    // XXX: locked cell checking is done in vi.c
    //      should just return silently?
    if (v == NULL || locked_cell(v))
        return;

    exprerr = 0;
    signal(SIGFPE, eval_fpe);
    if (setjmp(fpe_save)) {
        // XXX: potential memory leak in the evaluator
        error("Floating point exception in cell %s",
              v_name(cr.row, cr.col));
        cellerror = CELLERROR;
        p = scxdup("");
    } else {
        cellerror = CELLOK;
        p = seval(se);
    }
    if (v->cellerror != cellerror) {
        v->cellerror = cellerror;
        v->flags |= IS_CHANGED;
        FullUpdate++;
        changed++;
        modflg++;
    }
    signal(SIGFPE, doquit);
    if (exprerr) {
        scxfree(p);
        efree(se);
        return;
    }
    if (!loading)
        push_mark(cr.row, cr.col);

    set_string(&v->label, p);
    v->flags &= ~ALIGN_MASK;
    v->flags |= IS_CHANGED | align;
    if (v->expr) {
        efree(v->expr);
        v->expr = NULL;
        v->flags &= ~IS_STREXPR;
    }
    // XXX: test for constant expression is potentially incorrect
    if (constant_expr(se, optimize)) {
        efree(se);
    } else {
        v->expr = se;
        v->flags |= IS_STREXPR;
    }
    FullUpdate++;
    changed++;
    modflg++;
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

    // XXX: use more than 2 letters?
    if (col > 25) {
        *p++ = col / 26 + 'A' - 1;
        col %= 26;
    }
    *p++ = col + 'A';
    *p = '\0';
    return rname;
}

/*---- expression decompiler ----*/

typedef struct decomp_t decomp_t;
struct decomp_t {
    struct buf_t *buf;
    int dr, dc, flags;
};

static void decompile_node(decomp_t *dcp, struct enode *e, int priority);

static void out_const(decomp_t *dcp, double v) {
    buf_printf(dcp->buf, "%.15g", v);
}

static void out_sconst(decomp_t *dcp, const char *s) {
    buf_quotestr(dcp->buf, '"', s, '"');
}

static void out_var(decomp_t *dcp, struct ent_ptr v, int usename) {
    struct nrange *r;

    if (!v.vp || (v.vp->flags & IS_DELETED)) {
        buf_puts(dcp->buf, "@ERR");
    } else
    if (usename && (r = find_nrange_coords(rangeref(v.vp->row, v.vp->col, v.vp->row, v.vp->col))) != NULL && !r->r_is_range) {
        // XXX: this is incorrect if the named range has different flags
        buf_puts(dcp->buf, r->r_name);
    } else {
        buf_printf(dcp->buf, "%s%s%s%d",
                   (v.vf & FIX_COL) ? "$" : "", coltoa(v.vp->col),
                   (v.vf & FIX_ROW) ? "$" : "", v.vp->row);
    }
}

static void out_range(decomp_t *dcp, struct enode *e) {
    struct nrange *r;

    if ((r = find_nrange_coords(rangeref(e->e.r.left.vp->row, e->e.r.left.vp->col,
                                         e->e.r.right.vp->row, e->e.r.right.vp->col))) != NULL && r->r_is_range)
    {
        // XXX: this is incorrect if the named range has different flags
        buf_puts(dcp->buf, r->r_name);
    } else {
        out_var(dcp, e->e.r.left, 0);
        buf_putc(dcp->buf, ':');
        out_var(dcp, e->e.r.right, 0);
    }
}

/*
 *      To make list elements come out in the same order
 *      they were entered, we must do a depth-first eval
 *      of the ELIST tree
 */
static void decompile_list(decomp_t *dcp, struct enode *p) {
    if (p) {
        if (p->e.o.left) {
            decompile_list(dcp, p->e.o.left);    /* depth first */
            buf_putc(dcp->buf, ',');
        }
        decompile_node(dcp, p->e.o.right, 0);
    }
}

static void unary_arg(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    decompile_node(dcp, e->e.o.left, 30);
}

static void one_arg(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.left, 0);
    buf_putc(dcp->buf, ')');
}

static void two_arg(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.left, 0);
    // XXX: should test e->e.o.right
    buf_putc(dcp->buf, ',');
    decompile_node(dcp, e->e.o.right, 0);
    buf_putc(dcp->buf, ')');
}

static void three_arg(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.left, 0);
    buf_putc(dcp->buf, ',');
    decompile_node(dcp, e->e.o.right->e.o.left, 0);
    buf_putc(dcp->buf, ',');
    decompile_node(dcp, e->e.o.right->e.o.right, 0);
    buf_putc(dcp->buf, ')');
}

static void range_arg(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    out_range(dcp, e);
    buf_putc(dcp->buf, ')');
}

static void two_arg_index(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    out_range(dcp, e->e.o.left);
    buf_putc(dcp->buf, ',');
    decompile_node(dcp, e->e.o.right->e.o.left, 0);
    buf_putc(dcp->buf, ',');
    decompile_node(dcp, e->e.o.right->e.o.right, 0);
    buf_putc(dcp->buf, ')');
}

static void index_arg(decomp_t *dcp, const char *s, struct enode *e) {
    if (e->e.o.right && e->e.o.right->op == ',') {
        two_arg_index(dcp, s, e);
        return;
    }
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    out_range(dcp, e->e.o.left);
    if (e->e.o.right) {
        buf_putc(dcp->buf, ',');
        decompile_node(dcp, e->e.o.right, 0);
    }
    buf_putc(dcp->buf, ')');
}

static void list_arg(decomp_t *dcp, const char *s, struct enode *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.right, 0);
    buf_putc(dcp->buf, ',');
    decompile_list(dcp, e->e.o.left);
    buf_putc(dcp->buf, ')');
}

static void decompile_node(decomp_t *dcp, struct enode *e, int priority) {
    if (e) {
        int mypriority;
        switch (e->op) {
        default: mypriority = 99; break;
        case ';': mypriority = 1; break;
        case '?': mypriority = 2; break;
        case ':': mypriority = 3; break;
        case '|': mypriority = 4; break;
        case '&': mypriority = 5; break;
        case '<':
        case '=':
        case '>': mypriority = 6; break;
        case '+':
        case '-':
        case '#': mypriority = 8; break;
        case '*':
        case '/':
        case '%': mypriority = 10; break;
        case '^': mypriority = 12; break;
        }
        if (mypriority < priority) buf_putc(dcp->buf, '(');

        switch (e->op) {
        case 'f':       unary_arg(dcp, "@fixed ", e); break;
        case 'F':       unary_arg(dcp, "(@fixed)", e); break;
        case 'm':       unary_arg(dcp, "-", e); break;
        case '!':       unary_arg(dcp, "!", e); break;
        case O_VAR:     out_var(dcp, e->e.v, 1); break;
        case O_CONST:   out_const(dcp, e->e.k); break;
        case O_SCONST:  out_sconst(dcp, e->e.s); break;

        case SUM:       index_arg(dcp, "@sum", e); break;
        case PROD:      index_arg(dcp, "@prod", e); break;
        case AVG:       index_arg(dcp, "@avg", e); break;
        case COUNT:     index_arg(dcp, "@count", e); break;
        case STDDEV:    index_arg(dcp, "@stddev", e); break;
        case MAX:       index_arg(dcp, "@max", e); break;
        case MIN:       index_arg(dcp, "@min", e); break;
        case REDUCE | 'R': range_arg(dcp, "@rows", e); break;
        case REDUCE | 'C': range_arg(dcp, "@cols", e); break;

        case ABS:       one_arg(dcp, "@abs", e); break;
        case ACOS:      one_arg(dcp, "@acos", e); break;
        case ASIN:      one_arg(dcp, "@asin", e); break;
        case ATAN:      one_arg(dcp, "@atan", e); break;
        case ATAN2:     two_arg(dcp, "@atan2", e); break;
        case CEIL:      one_arg(dcp, "@ceil", e); break;
        case COS:       one_arg(dcp, "@cos", e); break;
        case EXP:       one_arg(dcp, "@exp", e); break;
        case FABS:      one_arg(dcp, "@fabs", e); break;
        case FLOOR:     one_arg(dcp, "@floor", e); break;
        case HYPOT:     two_arg(dcp, "@hypot", e); break;
        case LOG:       one_arg(dcp, "@ln", e); break;
        case LOG10:     one_arg(dcp, "@log", e); break;
        case POW:       two_arg(dcp, "@pow", e); break;
        case SIN:       one_arg(dcp, "@sin", e); break;
        case SQRT:      one_arg(dcp, "@sqrt", e); break;
        case TAN:       one_arg(dcp, "@tan", e); break;
        case DTR:       one_arg(dcp, "@dtr", e); break;
        case RTD:       one_arg(dcp, "@rtd", e); break;
        case RND:       one_arg(dcp, "@rnd", e); break;
        case ROUND:     two_arg(dcp, "@round", e); break;
        case HOUR:      one_arg(dcp, "@hour", e); break;
        case MINUTE:    one_arg(dcp, "@minute", e); break;
        case SECOND:    one_arg(dcp, "@second", e); break;
        case MONTH:     one_arg(dcp, "@month", e); break;
        case DAY:       one_arg(dcp, "@day", e); break;
        case YEAR:      one_arg(dcp, "@year", e); break;
        case NOW:       buf_puts(dcp->buf, "@now"); break;
        case DATE:      if (e->e.o.right)
                            two_arg(dcp, "@date", e);
                        else
                            one_arg(dcp, "@date", e);
                        break;
        case FMT:       two_arg(dcp, "@fmt", e); break;
        case UPPER:     one_arg(dcp, "@upper", e); break;
        case LOWER:     one_arg(dcp, "@lower", e); break;
        case CAPITAL:   one_arg(dcp, "@capital", e); break;
        case DTS:       three_arg(dcp, "@dts", e); break;
        case TTS:       three_arg(dcp, "@tts", e); break;
        case STON:      one_arg(dcp, "@ston", e); break;
        case EQS:       two_arg(dcp, "@eqs", e); break;
        case LMAX:      list_arg(dcp, "@max", e); break;
        case LMIN:      list_arg(dcp, "@min", e); break;
        case FV:        three_arg(dcp, "@fv", e); break;
        case PV:        three_arg(dcp, "@pv", e); break;
        case PMT:       three_arg(dcp, "@pmt", e); break;
        case NVAL:      two_arg(dcp, "@nval", e); break;
        case SVAL:      two_arg(dcp, "@sval", e); break;
        case EXT:       two_arg(dcp, "@ext", e); break;
        case SUBSTR:    three_arg(dcp, "@substr", e); break;
        case STINDEX:   index_arg(dcp, "@stindex", e); break;
        case INDEX:     index_arg(dcp, "@index", e); break;
        case LOOKUP:    index_arg(dcp, "@lookup", e); break;
        case HLOOKUP:   two_arg_index(dcp, "@hlookup", e); break;
        case VLOOKUP:   two_arg_index(dcp, "@vlookup", e); break;
        case IF:        three_arg(dcp, "@if", e); break;
        case MYROW:     buf_puts(dcp->buf, "@myrow"); break;
        case MYCOL:     buf_puts(dcp->buf, "@mycol"); break;
        case LASTROW:   buf_puts(dcp->buf, "@lastrow"); break;
        case LASTCOL:   buf_puts(dcp->buf, "@lastcol"); break;
        case COLTOA:    one_arg(dcp, "@coltoa", e); break;
        case FILENAME:  one_arg(dcp, "@filename", e); break;
        case NUMITER:   buf_puts(dcp->buf, "@numiter"); break;
        case ERR_:      buf_puts(dcp->buf, "@err"); break;
        case PI_:       buf_puts(dcp->buf, "@pi"); break;
        case BLACK:     buf_puts(dcp->buf, "@black"); break;
        case RED:       buf_puts(dcp->buf, "@red"); break;
        case GREEN:     buf_puts(dcp->buf, "@green"); break;
        case YELLOW:    buf_puts(dcp->buf, "@yellow"); break;
        case BLUE:      buf_puts(dcp->buf, "@blue"); break;
        case MAGENTA:   buf_puts(dcp->buf, "@magenta"); break;
        case CYAN:      buf_puts(dcp->buf, "@cyan"); break;
        case WHITE:     buf_puts(dcp->buf, "@white"); break;
        default:        // XXX: priority seems bogus
                        decompile_node(dcp, e->e.o.left, mypriority);
                        buf_putc(dcp->buf, e->op);
                        decompile_node(dcp, e->e.o.right, mypriority + 1);
                        break;
        }
        if (mypriority < priority) buf_putc(dcp->buf, ')');
    } else {
        buf_putc(dcp->buf, '?');
    }
}

// XXX: should get a context with a cell reference
void decompile_expr(buf_t buf, struct enode *e, int dr, int dc, int flags) {
    decomp_t ctx = { buf, dr, dc, flags };
    decompile_node(&ctx, e, 0);
}

// XXX: should get a context with a cell reference
int decompile(char *dest, size_t size, struct enode *e,
              int dr, int dc, int flags)
{
    buf_t buf;
    buf_init(buf, dest, size);
    decompile_expr(buf, e, dr, dc, flags);
    return buf->len;
}

int etype(struct enode *e) {
    if (e == NULL)
        return NUM;
    switch (e->op) {
    case UPPER: case LOWER: case CAPITAL:
    case O_SCONST: case '#': case DATE: case FMT: case STINDEX:
    case EXT: case SVAL: case SUBSTR:
        return STR;

    case '?':
    case IF:
        return etype(e->e.o.right->e.o.left);

    case 'f':
        return etype(e->e.o.right);

    case O_VAR: {
            struct ent *p;
            p = e->e.v.vp;
            if (p->expr)
                return (p->flags & IS_STREXPR) ? STR : NUM;
            else if (p->label)
                return STR;
            else
                return NUM;
        }

    default:
        return NUM;
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

void cmd_recalc(void) {
    EvalAll();
    update(1);
    changed = 0;
}
