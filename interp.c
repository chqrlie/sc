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

#include "sc.h"

#define ISVALID(r,c)    ((r)>=0 && (r)<maxrows && (c)>=0 && (c)<maxcols)

static jmp_buf fpe_save;
static int exprerr;     /* Set by eval() and seval() if expression errors */
double prescale = 1.0;  /* Prescale for constants in let() */
int extfunc = 0;        /* Enable/disable external functions */
int loading = 0;        /* Set when readfile() is active */
int propagation = 10;   /* max number of times to try calculation */
static int repct = 1;   /* Make repct a global variable so that the
                           function @numiter can access it */
static int cellerror = CELLOK;     /* is there an error in this cell */

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
static enode_t *free_enodes = NULL;

static SCXMEM enode_t *new_node(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2);
extern scvalue_t eval_node(eval_ctx_t *cp, enode_t *e, int gv);
static int RealEvalAll(void);
static void RealEvalOne(struct ent *p, int i, int j, int *chgct);

#ifdef RINT
double rint(double d);
#endif
static sigret_t eval_fpe(int);

#ifndef M_PI
#define M_PI (double)3.14159265358979323846
#endif
#define dtr(x) ((x) * (M_PI / (double)180.0))
#define rtd(x) ((x) * (180.0 / (double)M_PI))

/*---------------- utility functions ----------------*/

static inline void scvalue_free(scvalue_t v) {
    if (v.type == SC_STRING)
        free_string(v.u.str);
}

static inline scvalue_t scvalue_empty(void) {
    scvalue_t res;
    res.type = SC_EMPTY;
    return res;
}

static inline scvalue_t scvalue_error(eval_ctx_t *cp, int error) {
    scvalue_t res;
    res.type = SC_ERROR;
    res.u.cellerror = cp->cellerror = error;
    return res;
}

static inline scvalue_t scvalue_bool(int t) {
    scvalue_t res;
    res.type = SC_NUMBER;
    res.u.v = t;
    return res;
}

static inline scvalue_t scvalue_number(double v) {
    scvalue_t res;
    res.type = SC_NUMBER;
    res.u.v = v;
    return res;
}

static inline scvalue_t scvalue_string(SCXMEM string_t *str) {
    scvalue_t res;
    res.type = SC_STRING;
    res.u.str = str;
    return res;
}

static double eval_num(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e, 1);
    if (res.type == SC_NUMBER)
        return res.u.v;
    // XXX: should convert string to number
    scvalue_free(res);
    cp->error = CELLERROR;
    return 0;
}

static SCXMEM string_t *eval_str(eval_ctx_t *cp, enode_t *e) {
    char buf[32];
    scvalue_t res = eval_node(cp, e, 1);
    if (res.type == SC_STRING)
        return res.u.str;
    if (res.type == SC_NUMBER) {
        snprintf(buf, sizeof buf, "%.15g", res.u.v);
        return new_string(buf);
    }
    cp->error = CELLERROR;
    return NULL;
}

static scvalue_t eval_div(eval_ctx_t *cp, enode_t *a, enode_t *b) {
    double num = eval_num(cp, a);
    double denom = eval_num(cp, b);
    if (cp->cellerror)
        return scvalue_error(cp, CELLINVALID);
    else
    if (denom)
        return scvalue_number(num / denom);
    else
        return scvalue_error(cp, CELLERROR);
}

static scvalue_t eval_mod(eval_ctx_t *cp, enode_t *a, enode_t *b) {
    // XXX: this API is incorrect
    double num = floor(eval_num(cp, a));
    double denom = floor(eval_num(cp, b));
    if (cp->cellerror)
        return scvalue_error(cp, CELLINVALID);
    else
    if (denom)
        return scvalue_number(num - floor(num / denom) * denom);
    else
        return scvalue_error(cp, CELLERROR);
}

/*---------------- financial functions ----------------*/

static double fin_pv(double v1, double v2, double v3) {
    if (v2) {
        double p;
        errno = 0;
        p = pow(1 + v2, v3);
        if (!errno)
            return v1 * (1 - 1 / p) / v2;
    } else {
        errno = ERANGE;
    }
    return 0.0;
}

static double fin_fv(double v1, double v2, double v3) {
    if (v2) {
        double p;
        errno = 0;
        p = pow(1 + v2, v3);
        if (!errno)
            return v1 * (p - 1) / v2;
    } else {
        errno = ERANGE;
    }
    return 0.0;
}

static double fin_pmt(double v1, double v2, double v3) {
    double p;
    errno = 0;
    p = pow(1 + v2, v3);
    /* CHECK IF ~= 1 - 1/1 */
    if (!errno && p && p != 1.0) {
        return v1 * v2 / (1 - 1 / p);
    } else {
        errno = ERANGE;
    }
    return 0.0;
}

/*---------------- range lookup functions ----------------*/

static scvalue_t scvalue_getcell(eval_ctx_t *cp, struct ent *p) {
    if (!p || (p->flags & IS_DELETED)) {
        return scvalue_error(cp, CELLERROR);
    }
    if (p->cellerror)
        return scvalue_error(cp, CELLINVALID);
    if (p->flags & IS_VALID)
        return scvalue_number(p->v);
    if (p->label)
        return scvalue_string(dup_string(p->label));
    return scvalue_empty();
}

static scvalue_t doindex(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *val) {
    int r, c;

    if (val->op == ',') {               /* index by both row and column */
        r = minr + (int)eval_num(cp, val->e.o.left) - 1;
        c = minc + (int)eval_num(cp, val->e.o.right) - 1;
    } else if (minr == maxr) {          /* look along the row */
        r = minr;
        c = minc + (int)eval_num(cp, val) - 1;
    } else if (minc == maxc) {          /* look down the column */
        r = minr + (int)eval_num(cp, val) - 1;
        c = minc;
    } else {
        error("Improper indexing operation");
        return scvalue_empty();
    }
    if (c >= minc && c <= maxc && r >= minr && r <= maxr) {
        struct ent *p = *ATBL(tbl, r, c);
        return scvalue_getcell(cp, p);
    }
    return scvalue_empty();
}

static scvalue_t dolookup(eval_ctx_t *cp, enode_t *val, int minr, int minc, int maxr, int maxc, int offset, int vflag) {
    scvalue_t a = eval_node(cp, val, 1);
    int r, c;
    struct ent *p = NULL;
    struct ent *vp = NULL;
    int incr, incc, fndr, fndc;

    incr = vflag;
    incc = 1 - vflag;
    if (a.type == SC_NUMBER) {
        for (r = minr, c = minc; r <= maxr && c <= maxc; r += incr, c += incc) {
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->v <= a.u.v) {
                    fndr = incc ? (minr + offset) : r;
                    fndc = incr ? (minc + offset) : c;
                    if (ISVALID(fndr, fndc)) {
                        vp = *ATBL(tbl, fndr, fndc);
                    } else {
                        vp = NULL;
                    }
                } else
                    break;
            }
        }
    } else
    if (a.type == SC_STRING) {
        const char *str = s2str(a.u.str);

        for (r = minr, c = minc; r <= maxr && c <= maxc; r += incr, c += incc) {
            if ((p = *ATBL(tbl, r, c)) && p->label) {
                if (strcmp(s2c(p->label), str) == 0) {
                    fndr = incc ? (minr + offset) : r;
                    fndc = incr ? (minc + offset) : c;
                    if (ISVALID(fndr, fndc)) {
                        vp = *ATBL(tbl, fndr, fndc);
                    } else {
                        vp = NULL;
                    }
                    break;
                }
            }
        }
        free_string(a.u.str);
    }
    return scvalue_getcell(cp, vp);
}

/*---------------- aggregate functions ----------------*/

static int eval_test(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e, 1);
    if (a.type == SC_NUMBER)
        return a.u.v != 0;
    if (a.type == SC_STRING) {
        int res = s2str(a.u.str)[0] != '\0';
        free_string(a.u.str);
        return res;
    }
    return 0;
}

static scvalue_t eval_offset(eval_ctx_t *cp, enode_t *e, int roffset, int coffset) {
    int save_rowoffset = cp->rowoffset;
    int save_coloffset = cp->coloffset;
    scvalue_t res;
    cp->rowoffset = roffset;
    cp->coloffset = coffset;
    res = eval_node(cp, e, 1);
    cp->rowoffset = save_rowoffset;
    cp->coloffset = save_coloffset;
    return res;
}

static int eval_test_offset(eval_ctx_t *cp, enode_t *e, int roffset, int coffset) {
    int save_rowoffset = cp->rowoffset;
    int save_coloffset = cp->coloffset;
    int res;
    cp->rowoffset = roffset;
    cp->coloffset = coffset;
    res = eval_test(cp, e);
    cp->rowoffset = save_rowoffset;
    cp->coloffset = save_coloffset;
    return res;
}

static scvalue_t docount(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                count++;
            }
        }
    }
    cellerror = cellerr;
    return scvalue_number(count);
}

static scvalue_t dosum(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                v += p->v;
            }
        }
    }
    cellerror = cellerr;
    return scvalue_number(v);
}

static scvalue_t doprod(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int cellerr = CELLOK;
    struct ent *p;

    v = 1.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                v *= p->v;
            }
        }
    }
    cellerror = cellerr;
    return scvalue_number(v);
}

static scvalue_t doavg(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                v += p->v;
                count++;
            }
        }
    }
    cellerror = cellerr;

    if (count == 0)
        return scvalue_number(0.0);
    else
        return scvalue_number(v / (double)count);
}

static scvalue_t dostddev(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double res, lp, rp, v, nd;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    res = 0.0;
    lp = 0.0;
    rp = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                v = p->v;
                lp += v * v;
                rp += v;
                count++;
            }
        }
    }
    cellerror = cellerr;

    if (count > 1) {
        nd = (double)count;
        res = sqrt((nd * lp - rp * rp) / (nd * (nd - 1)));
    }
    return scvalue_number(res);
}

static scvalue_t domax(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                if (!count++) {
                    v = p->v;
                } else
                if (p->v > v)
                    v = p->v;
            }
        }
    }
    cellerror = cellerr;
    return scvalue_number(v);
}

static scvalue_t domin(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int count;
    int cellerr = CELLOK;
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && (p->flags & IS_VALID)) {
                if (p->cellerror)  // XXX: unclear what to do on invalid cells
                    cellerr = CELLINVALID;
                if (!count++) {
                    v = p->v;
                } else
                if (p->v < v)
                    v = p->v;
            }
        }
    }
    cellerror = cellerr;
    return scvalue_number(v);
}

/*---------------- date and time functions ----------------*/

// XXX: should accept 6 or 7 arguments
// XXX: should use integral part for day and fraction for time.
//      which may be incorrect for DST time adjustments.
static scvalue_t dodts(eval_ctx_t *cp, int yr, int mo, int day) {
    time_t secs;
    struct tm t;

    if (yr >= 0 && yr < 100)
        yr += yr >= 50 ? 1900 : 2000;

    t.tm_hour = t.tm_min = t.tm_sec = 0;
    t.tm_mon = mo - 1;
    t.tm_mday = day;
    t.tm_year = yr - 1900;
    t.tm_isdst = -1;

    // XXX: should implement proleptic Gregorian calendar ourselves
    /* mktime() handles out of range values for tm_mon and tm_mday
     * as a number of months and/or days beyond the date determined
     * from the other fields.
     * Dates before 1901/12/31 seem to fail on OS/X.
     */
    if (mo < 1 || day < 1 || (secs = mktime(&t)) == -1) {
        error("@dts: invalid argument or date out of range");
        return scvalue_error(cp, CELLERROR);
    }
    return scvalue_number(secs);
}

static double dotts(double hr, double min, double sec) {
    int seconds = ((int)floor(sec) + (int)floor(min) * 60 + (int)floor(hr) * 3600) % 86400;
    return seconds < 0 ? 86400 + seconds : seconds;
}

static scvalue_t donow(void) {
    // XXX: should use a more precise time value
    return scvalue_number((double)time(NULL));
}

static scvalue_t dotime(eval_ctx_t *cp, int which, enode_t *e) {
    static time_t t_cache;
    static struct tm tm_cache;
    struct tm *tp = &tm_cache;
    time_t tloc = (time_t)eval_num(cp, e);

    // XXX: this primitive cacheing system fails
    //      as soon as there are more than 1 time value
    //      and it will fail if the current TZ changes
    if (!t_cache || tloc != t_cache) {
        tp = localtime(&tloc);
        if (tp) {
            t_cache = tloc;
            tm_cache = *tp;
        }
    }

    if (tp) {
        switch (which) {
        case HOUR:          return scvalue_number(tm_cache.tm_hour);
        case MINUTE:        return scvalue_number(tm_cache.tm_min);
        case SECOND:        return scvalue_number(tm_cache.tm_sec);
        case MONTH:         return scvalue_number(tm_cache.tm_mon + 1);
        case DAY:           return scvalue_number(tm_cache.tm_mday);
        case YEAR:          return scvalue_number(tm_cache.tm_year + 1900);
        }
    }
    return scvalue_error(cp, CELLERROR);
}

static scvalue_t doston(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e, 1);
    double v = 0;
    char *end;

    if (a.type == SC_NUMBER)
        return a;
    // XXX: is an empty string an error?
    // XXX: is a blank string an error?
    if (a.type == SC_STRING) {
        v = strtod(s2str(a.u.str), &end);
        free_string(a.u.str);
        if (*end) {
            // XXX: is this an error?
        }
        if (!isfinite(v)) {
            // XXX: is this an error?
        }
    }
    return scvalue_number(v);
}

#if 0
static double doeqs(SCXMEM string_t *s1, SCXMEM string_t *s2) {
    double v;

    if (!s1 && !s2)
        return 1.0;

    if (s1 && s2 && strcmp(s2c(s1), s2c(s2)) == 0)
        v = 1.0;
    else
        v = 0.0;

    free_string(s1);
    free_string(s2);

    return v;
}
#endif

/*
 * Given a string representing a column name and a value which is a row
 * number, return a pointer to the selected cell's entry, if any, else NULL.
 * Use only the integer part of the column number.  Always free the string.
 */

static struct ent *getent(SCXMEM string_t *colstr, double rowdoub) {
    int collen;             /* length of column name */
    int row, col;           /* integer values */
    struct ent *p = NULL;   /* selected entry */

    if (!colstr) {
        cellerror = CELLERROR;
        return NULL;
    }

    if (((row = (int)floor(rowdoub)) >= 0)
    &&  (row < maxrows)                          /* in range */
    &&  ((col = atocol(s2c(colstr), &collen)) >= 0)   /* has column */
    &&  (s2c(colstr)[collen] == '\0')                 /* exact match */
    &&  (col < maxcols)) {                       /* in range */
        p = *ATBL(tbl, row, col);
        if (p && p->cellerror)
            cellerror = CELLINVALID;
    }
    free_string(colstr);
    return p;
}

/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

static scvalue_t donval(eval_ctx_t *cp, SCXMEM string_t *colstr, double rowdoub) {
    struct ent *p = getent(colstr, rowdoub);
    scvalue_t res = scvalue_getcell(cp, p);
    if (res.type == SC_NUMBER)
        return res;
    // XXX: should convert string to number
    scvalue_free(res);
    cp->error = CELLERROR;
    return scvalue_error(cp, CELLERROR);
}

/*
 *      The list routines (e.g. dolmax) are called with an LMAX enode.
 *      The left pointer is a chain of ELIST nodes, the right pointer
 *      is a value.
 */
static scvalue_t dolmax(eval_ctx_t *cp, enode_t *ep) {
    // XXX: should handle ranges in list
    int count = 0;
    double maxval = 0.0;
    enode_t *e;

    cellerror = CELLOK;
    for (e = ep; e; e = e->e.o.right) {
        double v = eval_num(cp, e->e.o.left);
        if (!count || v > maxval) {
            maxval = v;
            count++;
        }
    }
    return scvalue_number(maxval);
}

static scvalue_t dolmin(eval_ctx_t *cp, enode_t *ep) {
    // XXX: should handle ranges in list
    int count = 0;
    double minval = 0.0;
    enode_t *e;

    cellerror = CELLOK;
    for (e = ep; e; e = e->e.o.right) {
        double v = eval_num(cp, e->e.o.left);
        if (!count || v < minval) {
            minval = v;
            count++;
        }
    }
    return scvalue_number(minval);
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

static scvalue_t fn1_eval(eval_ctx_t *cp, double (*fn)(double), enode_t *a) {
    double res;
    errno = 0;
    res = (*fn)(eval_num(cp, a));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static scvalue_t fn2_eval(eval_ctx_t *cp, double (*fn)(double, double),
                          enode_t *arg1, enode_t *arg2)
{
    double res;
    errno = 0;
    res = (*fn)(eval_num(cp, arg1), eval_num(cp, arg2));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static scvalue_t fn3_eval(eval_ctx_t *cp, double (*fn)(double, double, double),
                          enode_t *arg1, enode_t *arg2, enode_t *arg3)
{
    double res;
    errno = 0;
    res = (*fn)(eval_num(cp, arg1), eval_num(cp, arg2), eval_num(cp, arg3));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static double rand_between(double aa, double bb) {
    long int a = (long int)aa;
    long int b = (long int)bb;
    if (a > b) {
        long int c = a;
        a = b;
        b = c;
    }
    if (a == b) {
        return a;
    } else {
        /* return an integer */
        return a + rand() * (b - a + 1) / ((long int)RAND_MAX + 1);
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

static double dornd(double d) {
    if (rndtoeven) {
        return rint(d);
    } else {
        return (d - floor(d) < 0.5 ? floor(d) : ceil(d));
    }
}

static double doround(double a, double b) {
    int prec = (int)b;
    double scale = 1.0;

    while (prec > 0) {
        scale *= 10.0;
        prec--;
    }
    while (prec < 0) {
        scale /= 10.0;
        prec++;
    }
    return dornd(a * scale) / scale;
}

/*---------------- string functions ----------------*/

/*
 * Rules for string functions:
 * Take string arguments which they scxfree.
 * All returned strings are assumed to be xalloced.
 */

static SCXMEM string_t *dodate(time_t tloc, SCXMEM string_t *fmtstr) {
    char buff[FBUFLEN];
    const char *fmt = fmtstr ? s2c(fmtstr) : "%a %b %d %H:%M:%S %Y";
    // XXX: should check format string
    ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
        (buff, sizeof buff, fmt, localtime(&tloc));
    free_string(fmtstr);
    return new_string(buff);
}

static SCXMEM string_t *dofmt(SCXMEM string_t *fmtstr, double v) {
    char buff[FBUFLEN];

    if (!fmtstr)
        return NULL;
    // XXX: Achtung Minen! snprintf from user supplied format string
    // XXX: MUST validate format string for no or single arg of type double
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    ((int (*)(char *, size_t, const char *, ...))snprintf)(buff, FBUFLEN, s2c(fmtstr), v);
    free_string(fmtstr);
    return new_string(buff);
}


/*
 * Given a command name and a value, run the command with the given value and
 * read and return its first output line (only) as an allocated string, store
 * this as a third argument of @ext().
 */

static scvalue_t doext(eval_ctx_t *cp, enode_t *se) {
    char buff[FBUFLEN];     /* command line/output */
    SCXMEM string_t *command;
    enode_t *right = se->e.o.right;
    enode_t *args = right;
    enode_t *prev = NULL;
    int cellerr = 0;

    if (right && right->op == 0) {
        prev = right->e.o.left;
        args = right->e.o.right;
    }
    *buff = '\0';

#ifdef NOEXTFUNCS
    error("Warning: External functions unavailable");
    //cellerr = CELLERROR;      /* not sure if this should be a cellerror */
#else
    if (!extfunc) {
        error("Warning: external functions disabled; using %s value",
              prev ? "null" : "previous");
    } else {
        command = eval_str(cp, se->e.o.left);
        if (sempty(command)) {
            error("Warning: external function given null command name");
            cellerr = CELLERROR;
            free_string(command);
        } else {
            FILE *pf;
            SCXMEM string_t *s = eval_str(cp, args);

            /* build cmd line */
            // XXX: should accept argument list
            snprintf(buff, sizeof buff, "%s %s", s2c(command), s2str(s));
            free_string(s);
            free_string(command);

            error("Running external function...");
            refresh();

            if ((pf = popen(buff, "r")) == NULL) {     /* run it */
                error("Warning: running \"%s\" failed", buff);
                cellerr = CELLERROR;
            } else {
                if (fgets(buff, sizeof(buff), pf) == NULL) {  /* one line */
                    // XXX: should use the empty string?
                    error("Warning: external function returned nothing");
                    *buff = '\0';
                } else {
                    // XXX: should strip initial and triling spaces
                    size_t len = strlen(buff);
                    if (len && buff[len - 1] == '\n')   /* contains newline */
                        buff[--len] = '\0';             /* end string there */
                    error(" "); /* erase notice */
                }
                pclose(pf);
                if (args == right) {
                    prev = new_str(new_string(buff));
                    /* create dummy node to store previous value */
                    se->e.o.right = right = new_node(0, prev, args);
                } else
                if (prev && prev->op == O_SCONST)
                    set_string(&prev->e.s, new_string(buff));
            }
        }
    }
#endif  /* NOEXTFUNCS */
    if (prev)
        return eval_node(cp, prev, 1);
    if (cellerr)
        return scvalue_error(cp, cellerr);
    else
        return scvalue_string(new_string(buff));
}


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's string value, if any.  Even if none,
 * still allocate and return a null string so the cell has a label value so
 * the expression is saved in a file, etc.
 */

static scvalue_t dosval(eval_ctx_t *cp, SCXMEM string_t *colstr, double rowdoub) {
    char buf[32];
    struct ent *p = getent(colstr, rowdoub);
    scvalue_t res = scvalue_getcell(cp, p);
    if (res.type == SC_STRING)
        return res;
    if (res.type == SC_NUMBER) {
        snprintf(buf, sizeof buf, "%.15g", res.u.v);
        return scvalue_string(new_string(buf));
    }
    cp->error = CELLERROR;
    return scvalue_error(cp, CELLERROR);
}

/*
 * character casing: make upper case, make lower case
 */

// XXX: should handle UTF-8 encoded UNICODE stuff
static SCXMEM string_t *docase(int acase, SCXMEM string_t *s) {
    SCXMEM string_t *s2;
    char *p;

    if (sempty(s))
        return s;

    s2 = new_string_len(s->s, slen(s));
    if (!s2)
        return s2;
    free_string(s);
    if (acase == UPPER) {
        for (p = s2->s; *p; p++) {
            if (islowerchar(*p))
                *p = toupperchar(*p);
        }
    } else
    if (acase == LOWER) {
        for (p = s2->s; *p; p++) {
            if (isupperchar(*p))
                *p = tolowerchar(*p);
        }
    }
    return s2;
}

/*
 * make proper capitals of every word in a string
 * if the string has mixed case we say the string is lower
 *      and we will upcase only first letters of words
 * if the string is all upper we will lower rest of words.
 */

static SCXMEM string_t *docapital(SCXMEM string_t *s) {
    SCXMEM string_t *s2;
    char *p;
    int skip = 1;
    int AllUpper = 1;

    if (sempty(s))
        return s;

    s2 = new_string_len(s2c(s), slen(s));
    if (!s2)
        return s2;
    free_string(s);
    for (p = s2->s; *p; p++) {
        if (islowerchar(*p)) {
            AllUpper = 0;
            break;
        }
    }
    for (p = s2->s; *p; p++) {
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
    return s2;
}

static scvalue_t dofilename(eval_ctx_t *cp, enode_t *e) {
    int n = eval_test(cp, e);
    const char *s = n ? curfile : get_basename(curfile);
    return scvalue_string(new_string(s));
}

static scvalue_t eval_or(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e->e.o.left, 1);
    if (a.type == SC_NUMBER) {
        if (a.u.v != 0)
            return a;
    } else
    if (a.type == SC_STRING) {
        if (s2str(a.u.str)[0] != '\0')
            return a;
        free_string(a.u.str);
    }
    return eval_node(cp, e->e.o.right, 1);
}

static scvalue_t eval_cmp(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e->e.o.left, 1);
    scvalue_t b = eval_node(cp, e->e.o.right, 1);
    int cmp = 0;
    if (a.type == SC_NUMBER && b.type == SC_NUMBER) {
        cmp = (a.u.v > a.u.v) - (a.u.v < a.u.v);
    } else
    if (a.type == SC_STRING && b.type == SC_STRING) {
        cmp = strcmp(s2str(a.u.str), s2str(b.u.str));
        free_string(a.u.str);
        free_string(b.u.str);
    } else {
        cmp = -1;
        scvalue_free(a);
        scvalue_free(b);
    }
    switch (e->op) {
    case '<':   return scvalue_bool(cmp <  0);
    case OP_LE: return scvalue_bool(cmp <= 0);
    case EQS:
    case '=':   return scvalue_bool(cmp == 0);
    case OP_LG:
    case OP_NE: return scvalue_bool(cmp != 0);
    case '>':   return scvalue_bool(cmp >  0);
    case OP_GE: return scvalue_bool(cmp >= 0);
    }
    return scvalue_bool(0);
}

/*---------------- dynamic evaluator ----------------*/

scvalue_t eval_node(eval_ctx_t *cp, enode_t *e, int gv) {
    if (e == NULL)
        return scvalue_error(cp, CELLINVALID);

    switch (e->op) {
    case '+':       return scvalue_number(eval_num(cp, e->e.o.left) + eval_num(cp, e->e.o.right));
    case '-':       return scvalue_number(eval_num(cp, e->e.o.left) - eval_num(cp, e->e.o.right));
    case '*':       return scvalue_number(eval_num(cp, e->e.o.left) * eval_num(cp, e->e.o.right));
    case '/':       return eval_div(cp, e->e.o.left, e->e.o.right);
    case '%':       return eval_mod(cp, e->e.o.left, e->e.o.right);
    case '<':
    case OP_LE:
    case '=':
    case OP_LG:
    case OP_NE:
    case '>':
    case OP_GE:     return eval_cmp(cp, e);
                    // XXX: should have @and(list) and @or(list)
    case '&':       return eval_test(cp, e->e.o.left) ? eval_node(cp, e->e.o.right, 1) : scvalue_bool(0);
    case '|':       return eval_or(cp, e);
    case IF:
    case '?':       return eval_node(cp, eval_test(cp, e->e.o.left) ?
                                     e->e.o.right->e.o.left : e->e.o.right->e.o.right, gv);
    case 'm':       return scvalue_number(-eval_num(cp, e->e.o.left));
    case 'f':       return eval_offset(cp, e->e.o.left, 0, 0);
    case 'F':       return eval_node(cp, e->e.o.left, gv);
    case '!':       return scvalue_bool(!eval_test(cp, e->e.o.left));
    case ';':       return scvalue_number(((int)eval_num(cp, e->e.o.left) & 7) +
                                          (((int)eval_num(cp, e->e.o.right) & 7) << 3));
    case O_CONST:   return scvalue_number(e->e.k);
    case O_VAR:     {
                        struct ent *vp = e->e.v.vp;
                        if (vp && (cp->rowoffset || cp->coloffset)) {
                            int row = (e->e.v.vf & FIX_ROW) ?
                                vp->row : vp->row + cp->rowoffset;
                            int col = (e->e.v.vf & FIX_COL) ?
                                vp->col : vp->col + cp->coloffset;
                            // XXX: this is bogus: out of bounds cells should
                            //      evaluate to 0.0 or a cellerror
                            checkbounds(&row, &col);
                            vp = *ATBL(tbl, row, col);
                        }
                        return scvalue_getcell(cp, vp);
                    }
    case SUM:
    case PROD:
    case AVG:
    case COUNT:
    case STDDEV:
    case MAX:
    case MIN:
    case ROWS_:
    case COLS_:
    case STINDEX:
    case INDEX:
    case LOOKUP:
    case HLOOKUP:
    case VLOOKUP: {
            // XXX: this 2 argument API is not general enough
            enode_t *left = e->e.o.left;
            enode_t *right = e->e.o.right;
            int minr = left->e.r.left.vp->row;
            int minc = left->e.r.left.vp->col;
            int maxr = left->e.r.right.vp->row;
            int maxc = left->e.r.right.vp->col;
            if (minr > maxr) SWAPINT(minr, maxr);
            if (minc > maxc) SWAPINT(minc, maxc);
            switch (e->op) {
            case SUM:   return dosum(cp, minr, minc, maxr, maxc, right);
            case PROD:  return doprod(cp, minr, minc, maxr, maxc, right);
            case AVG:   return doavg(cp, minr, minc, maxr, maxc, right);
            case COUNT: return docount(cp, minr, minc, maxr, maxc, right);
            case STDDEV: return dostddev(cp, minr, minc, maxr, maxc, right);
            case MAX:   return domax(cp, minr, minc, maxr, maxc, right);
            case MIN:   return domin(cp, minr, minc, maxr, maxc, right);
            case ROWS_: return scvalue_number(maxr - minr + 1);
            case COLS_: return scvalue_number(maxc - minc + 1);
            case LOOKUP: return dolookup(cp, right, minr, minc, maxr, maxc, 1, minc == maxc);
            case HLOOKUP: return dolookup(cp, right->e.o.left, minr, minc, maxr, maxc,
                                          (int)eval_num(cp, right->e.o.right), 0);
            case VLOOKUP: return dolookup(cp, right->e.o.left, minr, minc, maxr, maxc,
                                          (int)eval_num(cp, right->e.o.right), 1);
            case STINDEX:
            case INDEX: return doindex(cp, minr, minc, maxr, maxc, right);
            }
        }

    case ABS:       return fn1_eval(cp, fabs, e->e.o.left);
    case ACOS:      return fn1_eval(cp, acos, e->e.o.left);
    case ASIN:      return fn1_eval(cp, asin, e->e.o.left);
    case ATAN:      return fn1_eval(cp, atan, e->e.o.left);
    case ATAN2:     return fn2_eval(cp, atan2, e->e.o.left, e->e.o.right);
    case CEIL:      return fn1_eval(cp, ceil, e->e.o.left);
    case COS:       return fn1_eval(cp, cos, e->e.o.left);
    case EXP:       return fn1_eval(cp, exp, e->e.o.left);
    case FABS:      return fn1_eval(cp, fabs, e->e.o.left);
    case FLOOR:     return fn1_eval(cp, floor, e->e.o.left);
    case HYPOT:     return fn2_eval(cp, hypot, e->e.o.left, e->e.o.right);
    case LOG:       return fn1_eval(cp, log, e->e.o.left);
    case LOG10:     return fn1_eval(cp, log10, e->e.o.left);
    case '^':
    case POW:       return fn2_eval(cp, pow, e->e.o.left, e->e.o.right);
    case SIN:       return fn1_eval(cp, sin, e->e.o.left);
    case SQRT:      return fn1_eval(cp, sqrt, e->e.o.left);
    case TAN:       return fn1_eval(cp, tan, e->e.o.left);
    case DTR:       return scvalue_number(dtr(eval_num(cp, e->e.o.left)));
    case RTD:       return scvalue_number(rtd(eval_num(cp, e->e.o.left)));
    case RAND:      return scvalue_number((double)rand() / ((double)RAND_MAX + 1));
    case RANDBETWEEN: return fn2_eval(cp, rand_between, e->e.o.left, e->e.o.right);
    case RND:       return fn1_eval(cp, dornd, e->e.o.left);
    case ROUND:     return fn2_eval(cp, doround, e->e.o.left, e->e.o.right);
    case FV:        return fn3_eval(cp, fin_fv, e->e.o.left,
                                    e->e.o.right->e.o.left,
                                    e->e.o.right->e.o.right);
    case PV:        return fn3_eval(cp, fin_pv, e->e.o.left,
                                    e->e.o.right->e.o.left,
                                    e->e.o.right->e.o.right);
    case PMT:       return fn3_eval(cp, fin_pmt, e->e.o.left,
                                    e->e.o.right->e.o.left,
                                    e->e.o.right->e.o.right);
    case HOUR:
    case MINUTE:
    case SECOND:
    case MONTH:
    case DAY:
    case YEAR:      return dotime(cp, e->op, e->e.o.left);
    case NOW:       return donow();
    case DTS:       return dodts(cp, (int)eval_num(cp, e->e.o.left),
                                 (int)eval_num(cp, e->e.o.right->e.o.left),
                                 (int)eval_num(cp, e->e.o.right->e.o.right));
    case TTS:       return fn3_eval(cp, dotts, e->e.o.left,
                                    e->e.o.right->e.o.left,
                                    e->e.o.right->e.o.right);
    case STON:      return doston(cp, e->e.o.left);
    case EQS:       return eval_cmp(cp, e);
    case LMAX:      return dolmax(cp, e);
    case LMIN:      return dolmin(cp, e);
    case NVAL:      return donval(cp, eval_str(cp, e->e.o.left), eval_num(cp, e->e.o.right));
    case MYROW:     return scvalue_number(cp->gmyrow + cp->rowoffset);
    case MYCOL:     return scvalue_number(cp->gmycol + cp->coloffset);
    case LASTROW:   return scvalue_number(maxrow);
    case LASTCOL:   return scvalue_number(maxcol);
    case NUMITER:   return scvalue_number(repct);
    case ERR_:      return scvalue_error(cp, CELLERROR);
    case PI_:       return scvalue_number(M_PI);
    case BLACK:     return scvalue_number(COLOR_BLACK);
    case RED:       return scvalue_number(COLOR_RED);
    case GREEN:     return scvalue_number(COLOR_GREEN);
    case YELLOW:    return scvalue_number(COLOR_YELLOW);
    case BLUE:      return scvalue_number(COLOR_BLUE);
    case MAGENTA:   return scvalue_number(COLOR_MAGENTA);
    case CYAN:      return scvalue_number(COLOR_CYAN);
    case WHITE:     return scvalue_number(COLOR_WHITE);
    case O_SCONST:  return scvalue_string(dup_string(e->e.s));
    case '#':       return scvalue_string(cat_strings(eval_str(cp, e->e.o.left), eval_str(cp, e->e.o.right)));
    case DATE:      return scvalue_string(dodate((time_t)eval_num(cp, e->e.o.left), eval_str(cp, e->e.o.right)));
    case FMT:       return scvalue_string(dofmt(eval_str(cp, e->e.o.left), eval_num(cp, e->e.o.right)));
    case UPPER:     return scvalue_string(docase(UPPER, eval_str(cp, e->e.o.left)));
    case LOWER:     return scvalue_string(docase(LOWER, eval_str(cp, e->e.o.left)));
    case CAPITAL:   return scvalue_string(docapital(eval_str(cp, e->e.o.left)));
    case EXT:       return doext(cp, e);
    case SVAL:      return dosval(cp, eval_str(cp, e->e.o.left), eval_num(cp, e->e.o.right));
    case SUBSTR:    /* Substring: Note that v1 and v2 are one-based and v2 is included */
                    return scvalue_string(sub_string(eval_str(cp, e->e.o.left),
                                                     (int)eval_num(cp, e->e.o.right->e.o.left) - 1,
                                                     (int)eval_num(cp, e->e.o.right->e.o.right)));
    case COLTOA:    return scvalue_string(new_string(coltoa((int)eval_num(cp, e->e.o.left))));
    case FILENAME:  return dofilename(cp, e->e.o.left);
    default:        error("Illegal expression");
                    exprerr = 1;
                    return scvalue_error(cp, CELLERROR);
    }
}

/*---------------- typed evaluators ----------------*/

double eval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ 0, row, col, 0, 0, 0 }};
    return eval_num(cp, e);
}

SCXMEM string_t *seval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ 0, row, col, 0, 0, 0 }};
    return eval_str(cp, e);
}

/*---------------- spreadsheet recalc ----------------*/

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
                v = (int)eval_at(cpairs[pair]->expr, 0, 0);
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


static void RealEvalOne(struct ent *p, int row, int col, int *chgct) {
    if (p->flags & IS_STREXPR) {
        SCXMEM string_t *v;
        if (setjmp(fpe_save)) {
            error("Floating point exception %s", v_name(row, col));
            cellerror = CELLERROR;
            v = new_string("");
        } else {
            cellerror = CELLOK;
            v = seval_at(p->expr, row, col);
        }
        p->cellerror = cellerror;
        if (!v && !p->label) /* Everything's fine */
            return;
        if (!p->label || !v || strcmp(s2c(v), s2c(p->label)) != 0 || cellerror) {
            (*chgct)++;
            p->flags |= IS_CHANGED;
            changed++;
        }
        set_string(&p->label, v);
    } else {
        double v;
        if (setjmp(fpe_save)) {
            error("Floating point exception %s", v_name(row, col));
            cellerror = CELLERROR;
            v = 0.0;
        } else {
            cellerror = CELLOK;
            v = eval_at(p->expr, row, col);
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

static SCXMEM enode_t *new_node(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2) {
    SCXMEM enode_t *p;

    if (free_enodes) {
        p = free_enodes;
        free_enodes = p->e.o.left;
    } else {
        p = scxmalloc(sizeof(enode_t));
        if (!p) {
            efree(a1);
            efree(a2);
            return NULL;
        }
    }
    p->op = op;
    p->type = OP_TYPE_NODES;
    p->e.o.left = a1;
    p->e.o.right = a2;
    return p;
}

SCXMEM enode_t *new_op0(int op) {
    return new_node(op, NULL, NULL);
}

SCXMEM enode_t *new_op1(int op, SCXMEM enode_t *a1) {
    if (!a1) return NULL;
    return new_node(op, a1, NULL);
}

SCXMEM enode_t *new_op2(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2) {
    if (!a1 || !a2) {
        efree(a1);
        efree(a2);
        return NULL;
    }
    return new_node(op, a1, a2);
}

SCXMEM enode_t *new_op3(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2,
                        SCXMEM enode_t *a3)
{
    if (!a1 || !a2 || !a3) {
        efree(a1);
        efree(a2);
        efree(a3);
        return NULL;
    }
    return new_node(op, a1, new_node(op == '?' ? ':' : ',', a2, a3));
}

SCXMEM enode_t *new_var(cellref_t cr) {
    SCXMEM enode_t *p;

    if ((p = new_node(O_VAR, NULL, NULL))) {
        p->type = OP_TYPE_VAR;
        p->e.v.vf = cr.vf;
        p->e.v.vp = lookat(cr.row, cr.col);
    }
    return p;
}

SCXMEM enode_t *new_range(rangeref_t rr) {
    SCXMEM enode_t *p;

    if ((p = new_node(RANGEARG, NULL, NULL))) {
        p->type = OP_TYPE_RANGE;
        p->e.r.left.vf = rr.left.vf;
        p->e.r.left.vp = lookat(rr.left.row, rr.left.col);
        p->e.r.right.vf = rr.right.vf;
        p->e.r.right.vp = lookat(rr.right.row, rr.right.col);
    }
    return p;
}

SCXMEM enode_t *new_const(double v) {
    SCXMEM enode_t *p;

    if ((p = new_node(O_CONST, NULL, NULL))) {
        p->type = OP_TYPE_DOUBLE;
        p->e.k = v;
        if (!isfinite(v))
            p->op = ERR_;
    }
    return p;
}

SCXMEM enode_t *new_str(SCXMEM string_t *s) {
    SCXMEM enode_t *p = NULL;

    if (s && (p = new_node(O_SCONST, NULL, NULL)) != NULL) {
        p->type = OP_TYPE_STRING;
        p->e.s = s;
    }
    return p;
}

enode_t *copye(enode_t *e, int Rdelta, int Cdelta,
               int r1, int c1, int r2, int c2, int transpose, enode_t *range)
{
    enode_t *ret;

    if (e == NULL)
        return NULL;

    if (!(ret = new_node(e->op, NULL, NULL)))
        return NULL;

    if (e->type == OP_TYPE_RANGE) {
        int newrow, newcol, row, col, vf;

        ret->type = OP_TYPE_RANGE;

        vf = e->e.r.left.vf;
        row = e->e.r.left.vp->row;
        col = e->e.r.left.vp->col;
        newrow = ((vf & FIX_ROW) || row < r1 || row > r2 || col < c1 || col > c2 ?
              row : transpose ? r1 + Rdelta + col - c1 : row + Rdelta);
        newcol = ((vf & FIX_COL) || row < r1 || row > r2 || col < c1 || col > c2 ?
              col : transpose ? c1 + Cdelta + row - r1 : col + Cdelta);
        ret->e.r.left.vf = vf;
        ret->e.r.left.vp = lookat(newrow, newcol);
        vf = e->e.r.right.vf;
        row = e->e.r.right.vp->row;
        col = e->e.r.right.vp->col;
        newrow = ((vf & FIX_ROW) || row < r1 || row > r2 || col < c1 || col > c2 ?
              row : transpose ? r1 + Rdelta + col - c1 : row + Rdelta);
        newcol = ((vf & FIX_COL) || row < r1 || row > r2 || col < c1 || col > c2 ?
              col : transpose ? c1 + Cdelta + row - r1 : col + Cdelta);
        ret->e.r.right.vf = vf;
        ret->e.r.right.vp = lookat(newrow, newcol);
    } else
    if (e->type == OP_TYPE_VAR) {
        int newrow, newcol, row, col, vf;

        ret->type = OP_TYPE_VAR;

        vf = e->e.v.vf;
        row = e->e.v.vp->row;
        col = e->e.v.vp->col;
        if (range
        &&  row >= range->e.r.left.vp->row
        &&  row <= range->e.r.right.vp->row
        &&  col >= range->e.r.left.vp->col
        &&  col <= range->e.r.right.vp->col) {
            newrow = ((range->e.r.left.vf & FIX_ROW) ? row : row + Rdelta);
            newcol = ((range->e.r.left.vf & FIX_COL) ? col : col + Cdelta);
        } else {
            newrow = ((vf & FIX_ROW) || row < r1 || row > r2 || col < c1 || col > c2 ?
                      row : transpose ? r1 + Rdelta + col - c1 : row + Rdelta);
            newcol = ((vf & FIX_COL) || row < r1 || row > r2 || col < c1 || col > c2 ?
                      col : transpose ? c1 + Cdelta + row - r1 : col + Cdelta);
        }
        ret->e.v.vp = lookat(newrow, newcol);
        ret->e.v.vf = vf;
    } else
    if (e->type == OP_TYPE_VAR) {
        ret->type = OP_TYPE_DOUBLE;
        ret->e.k = e->e.k;
    } else
    if (e->type == OP_TYPE_STRING) {
        ret->type = OP_TYPE_STRING;
        ret->e.s = dup_string(e->e.s);
    } else {
        switch (ret->op) {
        case SUM:
        case PROD:
        case AVG:
        case COUNT:
        case STDDEV:
        case MAX:
        case MIN:
            // XXX: this hack is for SUM.IF and similar
            range = e->e.o.left;
            r1 = 0;
            c1 = 0;
            r2 = maxrow;
            c2 = maxcol;
            break;
        case 'f':
            if (!range)
                Rdelta = Cdelta = 0;
            break;
        case 'F':
            if (range)
                Rdelta = Cdelta = 0;
            break;
        }
        if ((e->e.o.left && !(ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
                                                    r1, c1, r2, c2, transpose, range)))
        ||  (e->e.o.right && !(ret->e.o.right = copye(e->e.o.right, Rdelta, Cdelta,
                                                      r1, c1, r2, c2, transpose, range))))
        {
            efree(ret);
            return NULL;
        }
    }
    return ret;
}

/*
 * Say if an expression is a constant (return 1) or not.
 */
static int constant_expr(enode_t *e, int full) {
    return (e == NULL
        ||  e->op == O_CONST
        ||  e->op == O_SCONST
        ||  (e->op == 'm' && constant_expr(e->e.o.left, 0)) /* unary minus */
        ||  (full
        &&   e->type == OP_TYPE_NODES
        &&   constant_expr(e->e.o.left, full)
        &&   constant_expr(e->e.o.right, full)
        &&   e->op != RAND     /* non pure functions */
        &&   e->op != RANDBETWEEN
        &&   e->op != EXT
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
    // XXX: what if the cell is locked?
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
void let(cellref_t cr, SCXMEM enode_t *e) {
    struct ent *v = lookat(cr.row, cr.col);
    double val;
    // XXX: test for constant expression is potentially incorrect
    unsigned isconstant = constant_expr(e, optimize);

    // XXX: locked cell checking is done in vi.c
    //      should just return silently?
    if (v == NULL || (v->flags & IS_LOCKED))
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
            val = eval_at(e, cr.row, cr.col);
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

void slet(cellref_t cr, SCXMEM enode_t *se, int align) {
    struct ent *v = lookat(cr.row, cr.col);
    SCXMEM string_t *p;

    // XXX: locked cell checking is done in vi.c
    //      should just return silently?
    if (v == NULL || (v->flags & IS_LOCKED))
        return;

    exprerr = 0;
    signal(SIGFPE, eval_fpe);
    if (setjmp(fpe_save)) {
        // XXX: potential memory leak in the evaluator
        error("Floating point exception in cell %s",
              v_name(cr.row, cr.col));
        cellerror = CELLERROR;
        p = new_string("");
    } else {
        cellerror = CELLOK;
        p = seval_at(se, cr.row, cr.col);
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
        free_string(p);
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

void efree(SCXMEM enode_t *e) {
    if (e) {
        if (e->type == OP_TYPE_NODES) {
            efree(e->e.o.left);
            efree(e->e.o.right);
        } else
        if (e->type == OP_TYPE_STRING) {
            free_string(e->e.s);
        }
        e->e.o.left = free_enodes;
        free_enodes = e;
    }
}

void free_enode_list(void) {
    enode_t *e, *next;
    for (e = free_enodes, free_enodes = NULL; e; e = next) {
        next = e->e.o.left;
        scxfree(e);
    }
}

/*---- expression decompiler ----*/

typedef struct decomp_t decomp_t;
struct decomp_t {
    struct buf_t *buf;
    int dr, dc, flags;
};

static void decompile_node(decomp_t *dcp, enode_t *e, int priority);

static void out_const(decomp_t *dcp, double v) {
#if 0
    // XXX: this ugly hack will patch the value
    //      but only a single match in the formula
    //      which may not even be a number!
    //      should pass localisation context to
    //      conversion function
    if (dpoint != '.') {
        char *dpointptr = strchr(buf->buf, dpoint);
        if (dpointptr != NULL)
            *dpointptr = '.';
    }
#endif
    buf_printf(dcp->buf, "%.15g", v);
}

static void out_sconst(decomp_t *dcp, const char *s) {
    buf_quotestr(dcp->buf, '"', s, '"');
}

static void out_var(decomp_t *dcp, struct ent_ptr v, int usename) {
    int row, col;
    struct nrange *r;

    if (!v.vp || (v.vp->flags & IS_DELETED)
    ||  (row = v.vp->row + dcp->dr) < 0
    ||  (col = v.vp->col + dcp->dc) < 0) {
        buf_puts(dcp->buf, "@ERR");
    } else
    if (!(dcp->flags & DCP_NO_NAME) && usename
    &&  (r = find_nrange_coords(rangeref(v.vp->row, v.vp->col, v.vp->row, v.vp->col))) != NULL
    &&  !r->r_is_range) {
        // XXX: this is incorrect if the named range has different flags
        buf_puts(dcp->buf, s2c(r->r_name));
    } else {
        buf_printf(dcp->buf, "%s%s%s%d",
                   (v.vf & FIX_COL) ? "$" : "", coltoa(col),
                   (v.vf & FIX_ROW) ? "$" : "", row);
    }
}

static void out_range(decomp_t *dcp, enode_t *e) {
    struct nrange *r;

    if (!(dcp->flags & DCP_NO_NAME)
    &&  (r = find_nrange_coords(rangeref(e->e.r.left.vp->row, e->e.r.left.vp->col,
                                         e->e.r.right.vp->row, e->e.r.right.vp->col))) != NULL
    &&  r->r_is_range) {
        // XXX: this is incorrect if the named range has different flags
        buf_puts(dcp->buf, s2c(r->r_name));
    } else {
        out_var(dcp, e->e.r.left, 0);
        buf_putc(dcp->buf, ':');
        out_var(dcp, e->e.r.right, 0);
    }
}

static void out_unary(decomp_t *dcp, const char *s, enode_t *e) {
    buf_puts(dcp->buf, s);
    decompile_node(dcp, e->e.o.left, 30);
}

static void decompile_list(decomp_t *dcp, enode_t *p) {
    while (p) {
        if (p->op == 0) {   /* skip summy nodes (@EXP) */
            p = p->e.o.right;
            continue;
        }
        buf_putc(dcp->buf, ',');
        if (p->op == ',') {
            decompile_node(dcp, p->e.o.left, 0);
            p = p->e.o.right;
        } else {
            decompile_node(dcp, p, 0);
            break;
        }
    }
}

static void out_func(decomp_t *dcp, const char *s, enode_t *e) {
    buf_puts(dcp->buf, s);
    buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.left, 0);
    decompile_list(dcp, e->e.o.right);
    buf_putc(dcp->buf, ')');
}

static void out_infix(decomp_t *dcp, char op1, char op2, enode_t *e, int priority, int mypriority) {
    if (mypriority < priority)
        buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.left, mypriority);
    buf_putc(dcp->buf, op1);
    if (op2)
        buf_putc(dcp->buf, op2);
    // XXX: priority seems bogus
    decompile_node(dcp, e->e.o.right, mypriority + 1);
    if (mypriority < priority)
        buf_putc(dcp->buf, ')');
}

static void decompile_node(decomp_t *dcp, enode_t *e, int priority) {
    if (!e) {
        buf_putc(dcp->buf, '?');
        return;
    }
    switch (e->op) {
    case O_CONST:   out_const(dcp, e->e.k);         break;
    case O_SCONST:  out_sconst(dcp, s2c(e->e.s));   break;
    case O_VAR:     out_var(dcp, e->e.v, 1);        break;
    case RANGEARG:  out_range(dcp, e);              break;
    case 'f':       out_unary(dcp, "@fixed ", e);   break;
    case 'F':       out_unary(dcp, "(@fixed)", e);  break;
    case 'm':       out_unary(dcp, "-", e);         break;
    case '!':       out_unary(dcp, "!", e);         break;
    case SUM:       out_func(dcp, "@sum", e);       break;
    case PROD:      out_func(dcp, "@prod", e);      break;
    case AVG:       out_func(dcp, "@avg", e);       break;
    case COUNT:     out_func(dcp, "@count", e);     break;
    case STDDEV:    out_func(dcp, "@stddev", e);    break;
    case MAX:       out_func(dcp, "@max", e);       break;
    case MIN:       out_func(dcp, "@min", e);       break;
    case ROWS_:     out_func(dcp, "@rows", e);      break;
    case COLS_:     out_func(dcp, "@cols", e);      break;
    case ABS:       out_func(dcp, "@abs", e);       break;
    case ACOS:      out_func(dcp, "@acos", e);      break;
    case ASIN:      out_func(dcp, "@asin", e);      break;
    case ATAN:      out_func(dcp, "@atan", e);      break;
    case ATAN2:     out_func(dcp, "@atan2", e);     break;
    case CEIL:      out_func(dcp, "@ceil", e);      break;
    case COS:       out_func(dcp, "@cos", e);       break;
    case EXP:       out_func(dcp, "@exp", e);       break;
    case FABS:      out_func(dcp, "@fabs", e);      break;
    case FLOOR:     out_func(dcp, "@floor", e);     break;
    case HYPOT:     out_func(dcp, "@hypot", e);     break;
    case LOG:       out_func(dcp, "@ln", e);        break;
    case LOG10:     out_func(dcp, "@log", e);       break;
    case POW:       out_func(dcp, "@pow", e);       break;
    case SIN:       out_func(dcp, "@sin", e);       break;
    case SQRT:      out_func(dcp, "@sqrt", e);      break;
    case TAN:       out_func(dcp, "@tan", e);       break;
    case DTR:       out_func(dcp, "@dtr", e);       break;
    case RTD:       out_func(dcp, "@rtd", e);       break;
    case RND:       out_func(dcp, "@rnd", e);       break;
    case ROUND:     out_func(dcp, "@round", e);     break;
    case HOUR:      out_func(dcp, "@hour", e);      break;
    case MINUTE:    out_func(dcp, "@minute", e);    break;
    case SECOND:    out_func(dcp, "@second", e);    break;
    case MONTH:     out_func(dcp, "@month", e);     break;
    case DAY:       out_func(dcp, "@day", e);       break;
    case YEAR:      out_func(dcp, "@year", e);      break;
    case NOW:       buf_puts(dcp->buf, "@now");     break;
    case DATE:      out_func(dcp, "@date", e);      break;
    case FMT:       out_func(dcp, "@fmt", e);       break;
    case UPPER:     out_func(dcp, "@upper", e);     break;
    case LOWER:     out_func(dcp, "@lower", e);     break;
    case CAPITAL:   out_func(dcp, "@capital", e);   break;
    case DTS:       out_func(dcp, "@dts", e);       break;
    case TTS:       out_func(dcp, "@tts", e);       break;
    case STON:      out_func(dcp, "@ston", e);      break;
    case EQS:       out_func(dcp, "@eqs", e);       break;
    case LMAX:      out_func(dcp, "@max", e);       break;
    case LMIN:      out_func(dcp, "@min", e);       break;
    case FV:        out_func(dcp, "@fv", e);        break;
    case PV:        out_func(dcp, "@pv", e);        break;
    case PMT:       out_func(dcp, "@pmt", e);       break;
    case NVAL:      out_func(dcp, "@nval", e);      break;
    case SVAL:      out_func(dcp, "@sval", e);      break;
    case EXT:       out_func(dcp, "@ext", e);       break;
    case SUBSTR:    out_func(dcp, "@substr", e);    break;
    case STINDEX:   out_func(dcp, "@stindex", e);   break;
    case INDEX:     out_func(dcp, "@index", e);     break;
    case LOOKUP:    out_func(dcp, "@lookup", e);    break;
    case HLOOKUP:   out_func(dcp, "@hlookup", e);   break;
    case VLOOKUP:   out_func(dcp, "@vlookup", e);   break;
    case IF:        out_func(dcp, "@if", e);        break;
    case MYROW:     buf_puts(dcp->buf, "@myrow");   break;
    case MYCOL:     buf_puts(dcp->buf, "@mycol");   break;
    case LASTROW:   buf_puts(dcp->buf, "@lastrow"); break;
    case LASTCOL:   buf_puts(dcp->buf, "@lastcol"); break;
    case COLTOA:    out_func(dcp, "@coltoa", e);    break;
    case FILENAME:  out_func(dcp, "@filename", e);  break;
    case NUMITER:   buf_puts(dcp->buf, "@numiter"); break;
    case ERR_:      buf_puts(dcp->buf, "@err");     break;
    case PI_:       buf_puts(dcp->buf, "@pi");      break;
    case BLACK:     buf_puts(dcp->buf, "@black");   break;
    case RED:       buf_puts(dcp->buf, "@red");     break;
    case GREEN:     buf_puts(dcp->buf, "@green");   break;
    case YELLOW:    buf_puts(dcp->buf, "@yellow");  break;
    case BLUE:      buf_puts(dcp->buf, "@blue");    break;
    case MAGENTA:   buf_puts(dcp->buf, "@magenta"); break;
    case CYAN:      buf_puts(dcp->buf, "@cyan");    break;
    case WHITE:     buf_puts(dcp->buf, "@white");   break;
    case ';':       out_infix(dcp, e->op, 0, e, priority, 1); break;
    case '?':       out_infix(dcp, e->op, 0, e, priority, 2); break;
    case ':':       out_infix(dcp, e->op, 0, e, priority, 3); break;
    case '|':       out_infix(dcp, e->op, 0, e, priority, 4); break;
    case '&':       out_infix(dcp, e->op, 0, e, priority, 5); break;
    case OP_GE:     out_infix(dcp, '>', '=', e, priority, 6); break;
    case OP_LE:     out_infix(dcp, '<', '=', e, priority, 6); break;
    case OP_LG:     out_infix(dcp, '<', '>', e, priority, 6); break;
    case OP_NE:     out_infix(dcp, '!', '=', e, priority, 6); break;
    case '<':
    case '=':
    case '>':       out_infix(dcp, e->op, 0, e, priority, 6); break;
    case '+':
    case '-':
    case '#':       out_infix(dcp, e->op, 0, e, priority, 8); break;
    case '*':
    case '/':
    case '%':       out_infix(dcp, e->op, 0, e, priority, 10); break;
    case '^':       out_infix(dcp, e->op, 0, e, priority, 12); break;
        // XXX: handle ',' nodes?
    default:        buf_printf(dcp->buf, "@errnode(%d)", e->op); break;
    }
}

/* decompile an expression with an optional cell offset and options */
int decompile_expr(buf_t buf, enode_t *e, int dr, int dc, int flags) {
    decomp_t ctx = { buf, dr, dc, flags };
    decompile_node(&ctx, e, 0);
    return buf->len;
}

/* decompile an expression with an optional cell offset and options */
int decompile(char *dest, size_t size, enode_t *e, int dr, int dc, int flags) {
    buf_t buf;
    buf_init(buf, dest, size);
    return decompile_expr(buf, e, dr, dc, flags);
}

int etype(enode_t *e) {
    if (e == NULL)
        return NUM;

    switch (e->op) {
    case UPPER:
    case LOWER:
    case CAPITAL:
    case O_SCONST:
    case '#':
    case DATE:
    case FMT:
    case STINDEX:
    case EXT:
    case SVAL:
    case SUBSTR:
        return STR;

    case '?':
    case IF:
        return etype(e->e.o.right->e.o.left);

    case 'f':
    case 'F':
        return etype(e->e.o.right);

    case O_VAR: {
            struct ent *p = e->e.v.vp;
            if (p->expr)
                return (p->flags & IS_STREXPR) ? STR : NUM;
            else
                return p->label ? STR : NUM;
        }
    default:
        return NUM;
    }
}

void cmd_recalc(void) {
    EvalAll();
    update(1);
    changed = 0;
}
