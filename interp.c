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

/* a linked list of free [struct enodes]'s, uses .e.o.left as the pointer */
static enode_t *free_enodes = NULL;

static SCXMEM enode_t *new_node(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2);
extern scvalue_t eval_node(eval_ctx_t *cp, enode_t *e, int gv);
static int RealEvalAll(void);
static void RealEvalOne(struct ent *p, enode_t *e, int i, int j, int *chgct);

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
    res.type = SC_BOOLEAN;
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
    if (res.type == SC_NUMBER || res.type == SC_BOOLEAN)
        return res.u.v;
    if (res.type == SC_STRING) {
        char *end;
        double v = strtod(s2str(res.u.str), &end);
        free_string(res.u.str);
        if (!*end)
            return v;
    }
    if (res.type == SC_EMPTY)
        return 0.0;

    cp->cellerror = CELLERROR; /* invalid conversion */
    return 0.0;
}

static sclong_t eval_long(eval_ctx_t *cp, enode_t *e) {
    // XXX: simplify this if SC_LONG becomes a value type
    // XXX: check rounding issues
    return (sclong_t)eval_num(cp, e);
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
    if (res.type == SC_BOOLEAN)
        return new_string(res.u.v ? "TRUE" : "FALSE");
    if (res.type == SC_EMPTY)
        return dup_string(empty_string);

    cp->cellerror = CELLERROR;
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
    if (p) {
        if (p->flags & IS_DELETED)
            return scvalue_error(cp, CELLERROR);
        if (p->type == SC_ERROR)
            return scvalue_error(cp, CELLINVALID);
        if (p->type == SC_NUMBER)
            return scvalue_number(p->v);
        if (p->type == SC_STRING)
            return scvalue_string(dup_string(p->label));
        if (p->type == SC_BOOLEAN)
            return scvalue_bool(p->v);
    }
    return scvalue_empty();
}

static scvalue_t doindex(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *val) {
    int r, c;

    if (val->op == OP_COMMA) {               /* index by both row and column */
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

    // XXX: lookup algorithm is incorrect: should implement polymorphic comparison
    //      should use binary search
    incr = vflag;
    incc = 1 - vflag;
    if (a.type == SC_NUMBER) {
        for (r = minr, c = minc; r <= maxr && c <= maxc; r += incr, c += incc) {
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
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
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_STRING) {
                if (strcmp(s2str(p->label), str) == 0) {
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
    if (a.type == SC_NUMBER || a.type == SC_BOOLEAN)
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
    struct ent *p;

    /* Semantics: Counts the numbers in the list N. Only numbers in references
       are counted; all other types are ignored. Errors are not propagated.
       It is implementation-defined what happens if 0 parameters are passed,
       but it should be an Error or 0.
     */
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                count++;
            }
        }
    }
    return scvalue_number(count);
}

static scvalue_t dosum(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    struct ent *p;

    v = 0.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                v += p->v;
            }
        }
    }
    return scvalue_number(v);
}

static scvalue_t doprod(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    struct ent *p;

    v = 1.0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                v *= p->v;
            }
        }
    }
    return scvalue_number(v);
}

static scvalue_t doavg(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int count;
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                v += p->v;
                count++;
            }
        }
    }

    if (count == 0)
        return scvalue_number(0.0);
    else
        return scvalue_number(v / (double)count);
}

static scvalue_t dostddev(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double res, lp, rp, v, nd;
    int r, c;
    int count;
    struct ent *p;

    res = 0.0;
    lp = 0.0;
    rp = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                v = p->v;
                lp += v * v;
                rp += v;
                count++;
            }
        }
    }

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
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                if (!count++) {
                    v = p->v;
                } else
                if (p->v > v)
                    v = p->v;
            }
        }
    }
    return scvalue_number(v);
}

static scvalue_t domin(eval_ctx_t *cp, int minr, int minc, int maxr, int maxc, enode_t *e) {
    double v;
    int r, c;
    int count;
    struct ent *p;

    v = 0.0;
    count = 0;
    for (r = minr; r <= maxr; r++) {
        for (c = minc; c <= maxc; c++) {
            if (e && !eval_test_offset(cp, e, r - minr, c - minc))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                if (!count++) {
                    v = p->v;
                } else
                if (p->v < v)
                    v = p->v;
            }
        }
    }
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
        case OP_HOUR:   return scvalue_number(tm_cache.tm_hour);
        case OP_MINUTE: return scvalue_number(tm_cache.tm_min);
        case OP_SECOND: return scvalue_number(tm_cache.tm_sec);
        case OP_MONTH:  return scvalue_number(tm_cache.tm_mon + 1);
        case OP_DAY:    return scvalue_number(tm_cache.tm_mday);
        case OP_YEAR:   return scvalue_number(tm_cache.tm_year + 1900);
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
    if (a.type == SC_BOOLEAN)
        v = a.u.v;
    else
    if (a.type == SC_STRING) {
        // XXX: is an empty string an error?
        // XXX: is a blank string an error?
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

static struct ent *getent(eval_ctx_t *cp, SCXMEM string_t *colstr, double rowdoub) {
    int collen;             /* length of column name */
    int row, col;           /* integer values */
    struct ent *p = NULL;   /* selected entry */

    if (((row = (int)floor(rowdoub)) >= 0)
    &&  colstr
    &&  ((col = atocol(s2c(colstr), &collen)) >= 0)   /* has column */
    &&  (s2c(colstr)[collen] == '\0'))      /* exact match */
    {
        if (row < maxrows && col < maxcols) { /* in range */
            p = *ATBL(tbl, row, col);
            if (p && p->cellerror)
                cp->cellerror = CELLINVALID;
        }
    } else {
        cp->cellerror = CELLERROR;
    }
    free_string(colstr);
    return p;
}

/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

static scvalue_t donval(eval_ctx_t *cp, SCXMEM string_t *colstr, double rowdoub) {
    struct ent *p = getent(cp, colstr, rowdoub);
    scvalue_t res = scvalue_getcell(cp, p);
    if (res.type == SC_NUMBER)
        return res;
    if (res.type == SC_BOOLEAN)
        return scvalue_number(res.u.v);
    if (res.type == SC_STRING) {
        char *end;
        double v = strtod(s2str(res.u.str), &end);
        free_string(res.u.str);
        if (!*end)
            return scvalue_number(v);
    }
    if (res.type == SC_EMPTY)
        return scvalue_number(0.0);

    return scvalue_error(cp, CELLERROR);
}

/*
 * The list routines (e.g. dolmax) are called with an LMAX enode.
 * The left pointer is a value, the right pointer is a chain of OP_COMMA nodes
 */
static scvalue_t dolmax(eval_ctx_t *cp, enode_t *ep) {
    // XXX: should handle ranges in list
    int count = 0;
    double maxval = 0.0;
    enode_t *e;

    for (e = ep; e; e = e->e.o.right) {
        scvalue_t res = eval_node(cp, e->e.o.left, 1);
        if (res.type == SC_NUMBER) {
            if (!count++ || res.u.v > maxval)
                maxval = res.u.v;
        } else {
            scvalue_free(res);
        }
    }
    return scvalue_number(maxval);
}

static scvalue_t dolmin(eval_ctx_t *cp, enode_t *ep) {
    // XXX: should handle ranges in list
    int count = 0;
    double minval = 0.0;
    enode_t *e;

    for (e = ep; e; e = e->e.o.right) {
        scvalue_t res = eval_node(cp, e->e.o.left, 1);
        if (res.type == SC_NUMBER) {
            if (!count++ || res.u.v < minval)
                minval = res.u.v;
        } else {
            scvalue_free(res);
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

static scvalue_t fn1_eval(eval_ctx_t *cp, double (*fn)(double), enode_t *e) {
    double res;
    errno = 0;
    res = (*fn)(eval_num(cp, e->e.o.left));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static scvalue_t fn2_eval(eval_ctx_t *cp, double (*fn)(double, double), enode_t *e) {
    double res;
    errno = 0;
    res = (*fn)(eval_num(cp, e->e.o.left), eval_num(cp, e->e.o.right));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static scvalue_t fn3_eval(eval_ctx_t *cp, double (*fn)(double, double, double), enode_t *e) {
    double res;
    errno = 0;
    res = (*fn)(eval_num(cp, e->e.o.left),
                eval_num(cp, e->e.o.right->e.o.left),
                eval_num(cp, e->e.o.right->e.o.right));
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

#if 0
static scvalue_t fl1_eval(eval_ctx_t *cp, sclong_t (*fn)(sclong_t), enode_t *e) {
    sclong_t res;
    errno = 0;
    res = (*fn)(eval_long(cp, e->e.o.left));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}
#endif

static scvalue_t fl2_eval(eval_ctx_t *cp, sclong_t (*fn)(sclong_t, sclong_t), enode_t *e) {
    sclong_t res;
    errno = 0;
    res = (*fn)(eval_long(cp, e->e.o.left), eval_long(cp, e->e.o.right));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

#if 0
static scvalue_t fl3_eval(eval_ctx_t *cp, sclong_t (*fn)(sclong_t, sclong_t, sclong_t), enode_t *e) {
    sclong_t res;
    errno = 0;
    res = (*fn)(eval_long(cp, e->e.o.left),
                eval_long(cp, e->e.o.right->e.o.left),
                eval_long(cp, e->e.o.right->e.o.right));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}
#endif

static sclong_t bitand(sclong_t a, sclong_t b) {
    /* BITAND(value1, value2)    Bitwise boolean AND of two numbers. */
    return a & b;
}

static sclong_t bitlshift(sclong_t a, sclong_t b) {
    /* BITLSHIFT(value, shift_amount)    Shifts the bits of the input a certain number of places to the left. */
    return b < 0 ? a >> b : a << b;
}

static sclong_t bitor(sclong_t a, sclong_t b) {
    /* BITOR(value1, value2)    Bitwise boolean OR of 2 numbers. */
    return a | b;
}

static sclong_t bitrshift(sclong_t a, sclong_t b) {
    /* BITRSHIFT(value, shift_amount)    Shifts the bits of the input a certain number of places to the right. */
    return b < 0 ? a << b : a >> b;
}

static sclong_t bitxor(sclong_t a, sclong_t b) {
    /* BITXOR(value1, value2)    Bitwise XOR (exclusive OR) of 2 numbers. */
    return a ^ b;
}

/*---------------- string functions ----------------*/

/*
 * Rules for string functions:
 * Take string arguments which they scxfree.
 * All returned strings are assumed to be xalloced.
 */

static scvalue_t dodate(eval_ctx_t *cp, enode_t *e) {
    char buff[FBUFLEN];
    time_t tloc = (time_t)eval_num(cp, e->e.o.left);
    SCXMEM string_t *fmtstr = e->e.o.right ? eval_str(cp, e->e.o.right) : NULL;
    const char *fmt = fmtstr ? s2c(fmtstr) : "%a %b %d %H:%M:%S %Y";
    struct tm *tp = localtime(&tloc);
    if (tp) {
        // XXX: should check format string
        ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
            (buff, sizeof buff, fmt, tp);
        free_string(fmtstr);
        return scvalue_string(new_string(buff));
    } else {
        free_string(fmtstr);
        return scvalue_error(cp, CELLERROR);
    }
}

static scvalue_t dofmt(eval_ctx_t *cp, enode_t *e) {
    char buff[FBUFLEN];
    SCXMEM string_t *fmtstr = eval_str(cp, e->e.o.left);
    double v = eval_num(cp, e->e.o.right);

    if (cp->cellerror)
        return scvalue_error(cp, CELLINVALID);

    // XXX: Achtung Minen! snprintf from user supplied format string
    // XXX: MUST validate format string for no or single arg of type double
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    ((int (*)(char *, size_t, const char *, ...))snprintf)(buff, FBUFLEN, s2str(fmtstr), v);
    free_string(fmtstr);
    return scvalue_string(new_string(buff));
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
                if (prev && prev->op == OP_SCONST)
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
    struct ent *p = getent(cp, colstr, rowdoub);
    scvalue_t res = scvalue_getcell(cp, p);
    if (res.type == SC_STRING)
        return res;
    if (res.type == SC_BOOLEAN)
        return scvalue_string(new_string(res.u.v ? "TRUE" : "FALSE"));
    if (res.type == SC_NUMBER) {
        snprintf(buf, sizeof buf, "%.15g", res.u.v);
        return scvalue_string(new_string(buf));
    }
    if (res.type == SC_EMPTY)
        return scvalue_string(dup_string(empty_string));

    return scvalue_error(cp, CELLERROR);
}

/*
 * character casing: make upper case, make lower case
 */

// XXX: should handle UTF-8 encoded UNICODE stuff
static scvalue_t docase(eval_ctx_t *cp, int op, SCXMEM string_t *s) {
    SCXMEM string_t *s2;
    char *p;

    if (sempty(s))
        return scvalue_string(s);

    s2 = new_string_len(s2c(s), slen(s));
    if (!s2)
        return scvalue_error(cp, CELLERROR);

    free_string(s);
    if (op == OP_UPPER) {
        for (p = s2->s; *p; p++) {
            if (islowerchar(*p))
                *p = toupperchar(*p);
        }
    } else
    if (op == OP_LOWER) {
        for (p = s2->s; *p; p++) {
            if (isupperchar(*p))
                *p = tolowerchar(*p);
        }
    } else
    if (op == OP_CAPITAL) {
        int skip = 1;
        int AllUpper = 1;
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
            } else {  /* if the string was all upper before */
                if (isupperchar(*p) && AllUpper != 0)
                    *p = tolowerchar(*p);
            }
        }
    }
    return scvalue_string(s2);
}

static scvalue_t dofilename(eval_ctx_t *cp, enode_t *e) {
    int n = eval_test(cp, e);
    const char *s = n ? curfile : get_basename(curfile);
    return scvalue_string(new_string(s));
}

static scvalue_t eval_or(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e->e.o.left, 1);
    if (a.type == SC_NUMBER || a.type == SC_BOOLEAN) {
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

static int is_relative(int op) {
    return op == OP_LT || op == OP_GT || op == OP_LE || op == OP_GE;
}

static scvalue_t eval_cmp(eval_ctx_t *cp, int op, enode_t *e) {
    scvalue_t a = eval_node(cp, e->e.o.left, 1);
    scvalue_t b = eval_node(cp, e->e.o.right, 1);
    int cmp = 0;
    // XXX: mixed types should compare in this order:
    //  number < string < logical < error < empty
    // XXX: should stop error propagation
    if (a.type == SC_ERROR || b.type == SC_ERROR) {
        if (op == OP_EQ || op == OP_EQS)
            cmp = 1;  /* return false */
        else
            op = 0;  /* return error */
    } else
    if (a.type == SC_NUMBER || (a.type == SC_BOOLEAN && is_relative(op))) {
        if (b.type == SC_NUMBER || (b.type == SC_BOOLEAN && is_relative(op)))
            cmp = (a.u.v > a.u.v) - (a.u.v < a.u.v);
        else
            cmp = -1;
    } else
    if (b.type == SC_NUMBER || (b.type == SC_BOOLEAN && is_relative(op))) {
        cmp = 1;
    } else
    if (a.type == SC_STRING) {
        if (b.type == SC_STRING)
            cmp = strcmp(s2str(a.u.str), s2str(b.u.str));
        else
            cmp = -1;
    } else
    if (b.type == SC_STRING) {
        cmp = 1;
    } else
    if (a.type == SC_BOOLEAN) {
        if (b.type == SC_BOOLEAN)
            cmp = b.u.v - a.u.v;
        else
            cmp = -1;
    } else
    if (b.type == SC_BOOLEAN) {
        cmp = 1;
    } else {
        cmp = 0;
    }
    scvalue_free(a);
    scvalue_free(b);
    switch (op) {
    case OP_LT:     return scvalue_bool(cmp <  0);
    case OP_LE:     return scvalue_bool(cmp <= 0);
    case OP_EQS:
    case OP_EQ:     return scvalue_bool(cmp == 0);
    case OP_LG:
    case OP_NE:     return scvalue_bool(cmp != 0);
    case OP_GT:     return scvalue_bool(cmp >  0);
    case OP_GE:     return scvalue_bool(cmp >= 0);
    }
    return scvalue_error(cp, CELLERROR);
}

/*---------------- dynamic evaluator ----------------*/

scvalue_t eval_node(eval_ctx_t *cp, enode_t *e, int gv) {
    if (e == NULL)
        return scvalue_empty();

    switch (e->op) {
    case OP_PLUS:   return scvalue_number(eval_num(cp, e->e.o.left) + eval_num(cp, e->e.o.right));
    case OP_MINUS:  return scvalue_number(eval_num(cp, e->e.o.left) - eval_num(cp, e->e.o.right));
    case OP_STAR:   return scvalue_number(eval_num(cp, e->e.o.left) * eval_num(cp, e->e.o.right));
    case OP_SLASH:  return eval_div(cp, e->e.o.left, e->e.o.right);
    case OP_PERCENT: return eval_mod(cp, e->e.o.left, e->e.o.right);
    case OP_LT:
    case OP_LE:
    case OP_EQ:
    case OP_LG:
    case OP_NE:
    case OP_GT:
    case OP_GE:     return eval_cmp(cp, e->op, e);
                    // XXX: should have @and(list) and @or(list)
    case OP_AMPERSAND: return eval_test(cp, e->e.o.left) ? eval_node(cp, e->e.o.right, 1) : scvalue_bool(0);
    case OP_VBAR:   return eval_or(cp, e);
    case OP_IF:
    case OP_QMARK:  return eval_node(cp, eval_test(cp, e->e.o.left) ?
                                     e->e.o.right->e.o.left : e->e.o.right->e.o.right, gv);
    case OP_UMINUS: return scvalue_number(-eval_num(cp, e->e.o.left));
    case OP_UPLUS:  return eval_node(cp, e->e.o.left, gv);
    case OP_FIXED:  return eval_offset(cp, e->e.o.left, 0, 0);
    case OP_PFIXED: return eval_node(cp, e->e.o.left, gv);
    case OP_BANG:   return scvalue_bool(!eval_test(cp, e->e.o.left));
    case OP_SEMI:   return scvalue_number(((int)eval_num(cp, e->e.o.left) & 7) +
                                          (((int)eval_num(cp, e->e.o.right) & 7) << 3));
    case OP_CONST:  return scvalue_number(e->e.k);
    case OP_VAR:    {
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
    case OP_SUM:
    case OP_PROD:
    case OP_AVG:
    case OP_COUNT:
    case OP_STDDEV:
    case OP_MAX:
    case OP_MIN:
    case OP_ROWS:
    case OP_COLS:
    case OP_STINDEX:
    case OP_INDEX:
    case OP_LOOKUP:
    case OP_HLOOKUP:
    case OP_VLOOKUP: {
            // XXX: this 2 argument API is not general enough
            // XXX: use generic evaluator for these
            enode_t *left = e->e.o.left;
            enode_t *right = e->e.o.right;
            int minr = left->e.r.left.vp->row;
            int minc = left->e.r.left.vp->col;
            int maxr = left->e.r.right.vp->row;
            int maxc = left->e.r.right.vp->col;
            if (minr > maxr) SWAPINT(minr, maxr);
            if (minc > maxc) SWAPINT(minc, maxc);
            switch (e->op) {
            case OP_SUM:    return dosum(cp, minr, minc, maxr, maxc, right);
            case OP_PROD:   return doprod(cp, minr, minc, maxr, maxc, right);
            case OP_AVG:    return doavg(cp, minr, minc, maxr, maxc, right);
            case OP_COUNT:  return docount(cp, minr, minc, maxr, maxc, right);
            case OP_STDDEV: return dostddev(cp, minr, minc, maxr, maxc, right);
            case OP_MAX:    return domax(cp, minr, minc, maxr, maxc, right);
            case OP_MIN:    return domin(cp, minr, minc, maxr, maxc, right);
            case OP_ROWS:   return scvalue_number(maxr - minr + 1);
            case OP_COLS:   return scvalue_number(maxc - minc + 1);
            case OP_LOOKUP: return dolookup(cp, right, minr, minc, maxr, maxc, 1, minc == maxc);
            case OP_HLOOKUP: return dolookup(cp, right->e.o.left, minr, minc, maxr, maxc,
                                          (int)eval_num(cp, right->e.o.right), 0);
            case OP_VLOOKUP: return dolookup(cp, right->e.o.left, minr, minc, maxr, maxc,
                                          (int)eval_num(cp, right->e.o.right), 1);
            case OP_STINDEX:
            case OP_INDEX: return doindex(cp, minr, minc, maxr, maxc, right);
            }
        }

    case OP_ABS:    return fn1_eval(cp, fabs, e);
    case OP_ACOS:   return fn1_eval(cp, acos, e);
    case OP_ASIN:   return fn1_eval(cp, asin, e);
    case OP_ATAN:   return fn1_eval(cp, atan, e);
    case OP_ATAN2:  return fn2_eval(cp, atan2, e);
    case OP_CEIL:   return fn1_eval(cp, ceil, e);
    case OP_COS:    return fn1_eval(cp, cos, e);
    case OP_EXP:    return fn1_eval(cp, exp, e);
    case OP_FABS:   return fn1_eval(cp, fabs, e);
    case OP_FLOOR:  return fn1_eval(cp, floor, e);
    case OP_HYPOT:  return fn2_eval(cp, hypot, e);
    case OP_LOG:    return fn1_eval(cp, log, e);
    case OP_LOG10:  return fn1_eval(cp, log10, e);
    case OP_CARET:
    case OP_POW:    return fn2_eval(cp, pow, e);
    case OP_SIN:    return fn1_eval(cp, sin, e);
    case OP_SQRT:   return fn1_eval(cp, sqrt, e);
    case OP_TAN:    return fn1_eval(cp, tan, e);
    case OP_DTR:    return scvalue_number(dtr(eval_num(cp, e->e.o.left)));
    case OP_RTD:    return scvalue_number(rtd(eval_num(cp, e->e.o.left)));
    case OP_RAND:   return scvalue_number((double)rand() / ((double)RAND_MAX + 1));
    case OP_RANDBETWEEN: return fn2_eval(cp, rand_between, e);
    case OP_RND:    return fn1_eval(cp, dornd, e);
    case OP_ROUND:  return fn2_eval(cp, doround, e);
    case OP_FV:     return fn3_eval(cp, fin_fv, e);
    case OP_PV:     return fn3_eval(cp, fin_pv, e);
    case OP_PMT:    return fn3_eval(cp, fin_pmt, e);
    case OP_HOUR:
    case OP_MINUTE:
    case OP_SECOND:
    case OP_MONTH:
    case OP_DAY:
    case OP_YEAR:   return dotime(cp, e->op, e->e.o.left);
    case OP_NOW:    return donow();
    case OP_DTS:    return dodts(cp, (int)eval_num(cp, e->e.o.left),
                                 (int)eval_num(cp, e->e.o.right->e.o.left),
                                 (int)eval_num(cp, e->e.o.right->e.o.right));
    case OP_TTS:    return fn3_eval(cp, dotts, e);
    case OP_STON:   return doston(cp, e->e.o.left);
    case OP_EQS:    return eval_cmp(cp, e->op, e);
    case OP_LMAX:   return dolmax(cp, e);
    case OP_LMIN:   return dolmin(cp, e);
    case OP_NVAL:   return donval(cp, eval_str(cp, e->e.o.left), eval_num(cp, e->e.o.right));
    case OP_MYROW:  return scvalue_number(cp->gmyrow + cp->rowoffset);
    case OP_MYCOL:  return scvalue_number(cp->gmycol + cp->coloffset);
    case OP_LASTROW: return scvalue_number(maxrow);
    case OP_LASTCOL: return scvalue_number(maxcol);
    case OP_NUMITER: return scvalue_number(repct);
    case OP_ERR:    return scvalue_error(cp, CELLERROR);
    case OP_PI:     return scvalue_number(M_PI);
    case OP_BLACK:  return scvalue_number(COLOR_BLACK);
    case OP_RED:    return scvalue_number(COLOR_RED);
    case OP_GREEN:  return scvalue_number(COLOR_GREEN);
    case OP_YELLOW: return scvalue_number(COLOR_YELLOW);
    case OP_BLUE:   return scvalue_number(COLOR_BLUE);
    case OP_MAGENTA: return scvalue_number(COLOR_MAGENTA);
    case OP_CYAN:   return scvalue_number(COLOR_CYAN);
    case OP_WHITE:  return scvalue_number(COLOR_WHITE);
    case OP_SCONST: return scvalue_string(dup_string(e->e.s));
    case OP_SHARP:  return scvalue_string(cat_strings(eval_str(cp, e->e.o.left), eval_str(cp, e->e.o.right)));
    case OP_DATE:   return dodate(cp, e);
    case OP_FMT:    return dofmt(cp, e);
    case OP_CAPITAL:
    case OP_LOWER:
    case OP_UPPER:  return docase(cp, e->op, eval_str(cp, e->e.o.left));
    case OP_EXT:    return doext(cp, e);
    case OP_SVAL:   return dosval(cp, eval_str(cp, e->e.o.left), eval_num(cp, e->e.o.right));
    case OP_SUBSTR: /* Substring: Note that v1 and v2 are one-based and v2 is included */
                    return scvalue_string(sub_string(eval_str(cp, e->e.o.left),
                                                     (int)eval_num(cp, e->e.o.right->e.o.left) - 1,
                                                     (int)eval_num(cp, e->e.o.right->e.o.right)));
    case OP_COLTOA: return scvalue_string(new_string(coltoa((int)eval_num(cp, e->e.o.left))));
    case OP_FILENAME: return dofilename(cp, e->e.o.left);

    case OP_BITAND: return fl2_eval(cp, bitand, e);
    case OP_BITLSHIFT: return fl2_eval(cp, bitlshift, e);
    case OP_BITOR:  return fl2_eval(cp, bitor, e);
    case OP_BITRSHIFT: return fl2_eval(cp, bitrshift, e);
    case OP_BITXOR: return fl2_eval(cp, bitxor, e);

    default:        error("Illegal expression");
                    exprerr = 1;
                    return scvalue_error(cp, CELLERROR);
    }
}

/*---------------- typed evaluators ----------------*/

scvalue_t eval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0, 0 }};
    return eval_node(cp, e, 1);
}

double neval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0, 0 }};
    return eval_num(cp, e);
}

SCXMEM string_t *seval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0, 0 }};
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
            eval_ctx_t cp[1] = {{ 0, 0, 0, 0, 0 }};
            if (cpairs[pair] && cpairs[pair]->expr) {
                v = (int)eval_num(cp, cpairs[pair]->expr);
                // XXX: should ignore value if cellerror
                init_style(pair, v & 7, (v >> 3) & 7, cpairs[pair]->expr);
            }
            /* Can't see to fix the problem if color 1 has an error, so
             * turn off color in that case.
             */
            if (pair == 1 && cp->cellerror) {
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
                    RealEvalOne(p, p->expr, i, j, &chgct);
            }
        }
    } else
    if (calc_order == BYCOLS) {
        for (j = 0; j <= maxcol; j++) {
            for (i = 0; i <= maxrow; i++) {
                if ((p = *ATBL(tbl,i,j)) && p->expr)
                    RealEvalOne(p, p->expr, i, j, &chgct);
            }
        }
    } else {
        // XXX: Should implement topological sort
        error("Internal error calc_order");
    }
    return chgct;
}


static void RealEvalOne(struct ent *p, enode_t *e, int row, int col, int *chgct) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0, 0 }};
    scvalue_t res;

    if (setjmp(fpe_save)) {
        error("Floating point exception at %s", v_name(row, col));
        res = scvalue_error(cp, CELLERROR);
    } else {
        res = eval_node(cp, e, 1);
    }
    if ((res.type == SC_STRING && !res.u.str)
    ||  (res.type == SC_NUMBER && !isfinite(res.u.v))) {
        res = scvalue_error(cp, CELLERROR);
    }
    if (p->type == SC_STRING) {
        if (res.type == SC_STRING && !strcmp(s2c(res.u.str), s2c(p->label))) {
            free_string(res.u.str);
            p->cellerror = 0;
            return;
        } else {
            set_string(&p->label, NULL);
        }
    }
    if (res.type == SC_STRING) {
        (*chgct)++;
        p->flags |= IS_CHANGED;
        p->type = SC_STRING;
        changed++;
        set_string(&p->label, res.u.str);
        p->cellerror = 0;
    } else
    if (res.type == SC_NUMBER) {
        if (p->type != SC_NUMBER || p->v != res.u.v) {
            (*chgct)++;
            p->flags |= IS_CHANGED;
            p->type = SC_NUMBER;
            changed++;
        }
        p->v = res.u.v;
        p->cellerror = 0;
    } else
    if (res.type == SC_BOOLEAN) {
        if (p->type != SC_BOOLEAN || p->v != res.u.v) {
            (*chgct)++;
            p->flags |= IS_CHANGED;
            p->type = SC_BOOLEAN;
            changed++;
        }
        p->v = res.u.v;
        p->cellerror = 0;
    } else
    if (res.type == SC_ERROR) {
        if (p->type != SC_ERROR || p->cellerror != res.u.cellerror) {
            p->flags |= IS_CHANGED;
            p->type = SC_ERROR;
        }
        p->v = 0;
        p->cellerror = res.u.cellerror;
    } else
    if (res.type == SC_EMPTY) {
        if (p->type != SC_EMPTY) {
            p->flags |= IS_CHANGED;
            p->type = SC_EMPTY;
        }
        p->v = 0;
        p->cellerror = 0;
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
    return new_node(op, a1, new_node(op == OP_QMARK ? OP_COLON : OP_COMMA, a2, a3));
}

SCXMEM enode_t *new_var(cellref_t cr) {
    SCXMEM enode_t *p;

    if ((p = new_node(OP_VAR, NULL, NULL))) {
        p->type = OP_TYPE_VAR;
        p->e.v.vf = cr.vf;
        p->e.v.vp = lookat(cr.row, cr.col);
    }
    return p;
}

SCXMEM enode_t *new_range(rangeref_t rr) {
    SCXMEM enode_t *p;

    if ((p = new_node(OP_RANGEARG, NULL, NULL))) {
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

    if ((p = new_node(OP_CONST, NULL, NULL))) {
        p->type = OP_TYPE_DOUBLE;
        p->e.k = v;
        if (!isfinite(v))
            p->op = OP_ERR;
    }
    return p;
}

SCXMEM enode_t *new_str(SCXMEM string_t *s) {
    SCXMEM enode_t *p = NULL;

    if (s && (p = new_node(OP_SCONST, NULL, NULL)) != NULL) {
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
        case OP_SUM:
        case OP_PROD:
        case OP_AVG:
        case OP_COUNT:
        case OP_STDDEV:
        case OP_MAX:
        case OP_MIN:
            // XXX: this hack is for SUM.IF and similar
            range = e->e.o.left;
            r1 = 0;
            c1 = 0;
            r2 = maxrow;
            c2 = maxcol;
            break;
        case OP_FIXED:
            if (!range)
                Rdelta = Cdelta = 0;
            break;
        case OP_PFIXED:
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
        ||  e->op == OP_CONST
        ||  e->op == OP_SCONST
        ||  (e->op == OP_UMINUS && constant_expr(e->e.o.left, 0)) /* unary minus */
        ||  (full
        &&   e->type == OP_TYPE_NODES
        &&   constant_expr(e->e.o.left, full)
        &&   constant_expr(e->e.o.right, full)
        &&   e->op != OP_RAND     /* non pure functions */
        &&   e->op != OP_RANDBETWEEN
        &&   e->op != OP_EXT
        &&   e->op != OP_NVAL
        &&   e->op != OP_SVAL
        &&   e->op != OP_NOW
        &&   e->op != OP_MYROW
        &&   e->op != OP_MYCOL
        &&   e->op != OP_LASTROW
        &&   e->op != OP_LASTCOL
        &&   e->op != OP_NUMITER
        &&   e->op != OP_FILENAME));
}

// XXX: all these should go to cmds.c

/* clear the value and expression of a cell */
void unlet(cellref_t cr) {
    struct ent *v = lookat_nc(cr.row, cr.col);
    if (v && v->type != SC_EMPTY) {
        // XXX: what if the cell is locked?
        v->v = 0.0;
        efree(v->expr);
        v->expr = NULL;
        set_string(&v->label, NULL);
        v->cellerror = CELLOK;
        v->type = SC_EMPTY;
        v->flags |= IS_CHANGED;
        FullUpdate++;
        changed++;
        modflg++;
    }
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

/* set the expression and/or value part of a cell */
void let(cellref_t cr, SCXMEM enode_t *e, int align) {
    struct ent *v = lookat(cr.row, cr.col);
    int isconstant = constant_expr(e, optimize);

    /* prescale input unless it has a decimal */
    if (!loading) {
        // XXX: sc_decimal is a horrible hack!
        //      should use a flag in the expression node
        if (e->op == OP_CONST && !sc_decimal && prescale < 0.9999999)
            e->e.k *= prescale;
        sc_decimal = FALSE;
    }

    // XXX: locked cell checking is done in vi.c. just return silently
    if (v == NULL || (v->flags & IS_LOCKED)) {
        efree(e);
        return;
    }

    // XXX: test for constant expression is potentially incorrect
    if (!loading || isconstant) {
        int chgcnt = 0;
        exprerr = 0;
        signal(SIGFPE, eval_fpe);
        RealEvalOne(v, e, cr.row, cr.col, &chgcnt);
        signal(SIGFPE, doquit);
    }

    if (isconstant) {
        efree(e);
        e = NULL;
    }
    efree(v->expr);
    v->expr = e;
    v->flags |= IS_CHANGED;
    if (align >= 0) {
        v->flags &= ~ALIGN_MASK;
        v->flags |= IS_CHANGED | align;
    }

    changed++;
    modflg++;

    if (!loading)
        push_mark(cr.row, cr.col);
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
        if (p->op == 0) {   /* skip dummy nodes (@EXP) */
            p = p->e.o.right;
            continue;
        }
        buf_putc(dcp->buf, ',');
        if (p->op == OP_COMMA) {
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
    if (e) {
        buf_putc(dcp->buf, '(');
        decompile_node(dcp, e->e.o.left, 0);
        decompile_list(dcp, e->e.o.right);
        buf_putc(dcp->buf, ')');
    }
}

static void out_infix(decomp_t *dcp, const char *s, enode_t *e, int priority, int mypriority) {
    if (mypriority < priority)
        buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.o.left, mypriority);
    buf_puts(dcp->buf, s);
    // XXX: priority seems bogus
    decompile_node(dcp, e->e.o.right, mypriority + 1);
    if (mypriority < priority)
        buf_putc(dcp->buf, ')');
}

static const char * const opname[] = {
#define OP(op,str,min,max)  str,
#include "opcodes.h"
#undef OP
};
static signed char const opmin[] = {
#define OP(op,str,min,max)  min,
#include "opcodes.h"
#undef OP
};
#if 0
static signed char const opmax[] = {
#define OP(op,str,min,max)  max,
#include "opcodes.h"
#undef OP
};
#endif

static void decompile_node(decomp_t *dcp, enode_t *e, int priority) {
    if (!e) {
        buf_putc(dcp->buf, '?');
        return;
    }
    switch (e->op) {
    case OP_DUMMY:      decompile_node(dcp, e->e.o.right, priority); break;
    case OP_CONST:      out_const(dcp, e->e.k);         break;
    case OP_SCONST:     out_sconst(dcp, s2c(e->e.s));   break;
    case OP_VAR:        out_var(dcp, e->e.v, 1);        break;
    case OP_RANGEARG:   out_range(dcp, e);              break;
    case OP_FIXED:
    case OP_PFIXED:
    case OP_UMINUS:
    case OP_UPLUS:
    case OP_BANG:       out_unary(dcp, opname[e->op], e); break;
    case OP_SEMI:       out_infix(dcp, opname[e->op], e, priority, 1); break;
    case OP_QMARK:      out_infix(dcp, opname[e->op], e, priority, 2); break;
    case OP_COLON:      out_infix(dcp, opname[e->op], e, priority, 3); break;
    case OP_VBAR:       out_infix(dcp, opname[e->op], e, priority, 4); break;
    case OP_AMPERSAND:  out_infix(dcp, opname[e->op], e, priority, 5); break;
    case OP_EQ:
    case OP_NE:
    case OP_LG:
    case OP_LT:
    case OP_LE:
    case OP_GE:
    case OP_GT:         out_infix(dcp, opname[e->op], e, priority, 6); break;
    case OP_SHARP:      out_infix(dcp, opname[e->op], e, priority, 7); break;
    case OP_PLUS:
    case OP_MINUS:      out_infix(dcp, opname[e->op], e, priority, 8); break;
    case OP_STAR:
    case OP_SLASH:
    case OP_PERCENT:    out_infix(dcp, opname[e->op], e, priority, 10); break;
    case OP_CARET:      out_infix(dcp, opname[e->op], e, priority, 12); break;
    default:            if (e->op >= 0 && e->op < OP_count) {
                            out_func(dcp, opname[e->op], (opmin[e->op] < 0) ? NULL : e);
                        } else {
                            buf_printf(dcp->buf, "@errnode(%d)", e->op);
                        }
                        break;
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

void cmd_recalc(void) {
    EvalAll();
    update(1);
    changed = 0;
}
