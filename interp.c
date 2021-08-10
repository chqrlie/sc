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
extern scvalue_t eval_node(eval_ctx_t *cp, enode_t *e);
extern scvalue_t eval_node_value(eval_ctx_t *cp, enode_t *e);
static scvalue_t scvalue_getcell(eval_ctx_t *cp, int row, int col);
static int RealEvalAll(void);
static void RealEvalOne(struct ent *p, enode_t *e, int i, int j, int *chgct);

#ifdef RINT
double rint(double d);
#endif
static sigret_t eval_fpe(int);

#ifndef M_PI
#define M_PI (double)3.14159265358979323846
#endif

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

static inline scvalue_t scvalue_range(rangeref_t rr) {
    scvalue_t res;
    res.type = SC_RANGE;
    res.u.rr = rr;
    return res;
}

static double eval_num(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e);
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
    scvalue_t res = eval_node_value(cp, e);
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

static scvalue_t eval_colon(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e->e.o.left);
    scvalue_t b = eval_node(cp, e->e.o.right);
    if (a.type == SC_RANGE && b.type == SC_RANGE) {
        if (a.u.rr.left.col > b.u.rr.left.col)
            a.u.rr.left.col = b.u.rr.left.col;
        if (a.u.rr.left.row > b.u.rr.left.row)
            a.u.rr.left.row = b.u.rr.left.row;
        if (a.u.rr.right.col < b.u.rr.right.col)
            a.u.rr.right.col = b.u.rr.right.col;
        if (a.u.rr.right.row < b.u.rr.right.row)
            a.u.rr.right.row = b.u.rr.right.row;
        return a;
    }
    return scvalue_error(cp, CELLERROR);
}

static scvalue_t eval_add(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(eval_num(cp, e->e.o.left) + eval_num(cp, e->e.o.right));
}

static scvalue_t eval_sub(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(eval_num(cp, e->e.o.left) - eval_num(cp, e->e.o.right));
}

static scvalue_t eval_mul(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(eval_num(cp, e->e.o.left) * eval_num(cp, e->e.o.right));
}

static scvalue_t eval_neg(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(-eval_num(cp, e->e.o.left));
}

static scvalue_t eval_div(eval_ctx_t *cp, enode_t *e) {
    double num = eval_num(cp, e->e.o.left);
    double denom = eval_num(cp, e->e.o.right);
    if (cp->cellerror)
        return scvalue_error(cp, CELLINVALID);
    else
    if (denom)
        return scvalue_number(num / denom);
    else
        return scvalue_error(cp, CELLERROR);
}

static scvalue_t eval_mod(eval_ctx_t *cp, enode_t *e) {
    // XXX: this API is incorrect
    double num = floor(eval_num(cp, e->e.o.left));
    double denom = floor(eval_num(cp, e->e.o.right));
    if (cp->cellerror)
        return scvalue_error(cp, CELLINVALID);
    else
    if (denom)
        return scvalue_number(num - floor(num / denom) * denom);
    else
        return scvalue_error(cp, CELLERROR);
}

static scvalue_t eval_pi(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(M_PI);
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

static scvalue_t scvalue_getcell(eval_ctx_t *cp, int row, int col) {
    struct ent *p = lookat_nc(row, col);
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
    if (row < 0 || col < 0)
        return scvalue_error(cp, CELLERROR);

    return scvalue_empty();
}

static scvalue_t eval_vararg(eval_ctx_t *cp, enode_t *e) {
    struct ent *vp = e->e.v.vp;
    if (vp) {
        int row = (e->e.v.vf & FIX_ROW) ? vp->row : vp->row + cp->rowoffset;
        int col = (e->e.v.vf & FIX_COL) ? vp->col : vp->col + cp->coloffset;
        if (row >= 0 && col >= 0)
            return scvalue_range(rangeref(row, col, row, col));
    }
    return scvalue_error(cp, CELLERROR);
}

static scvalue_t eval_range(eval_ctx_t *cp, enode_t *e) {
    struct ent *v1 = e->e.r.left.vp;
    struct ent *v2 = e->e.r.right.vp;
    if (v1 && v2) {
        int minr = (e->e.r.left.vf & FIX_ROW) ? v1->row : v1->row + cp->rowoffset;
        int minc = (e->e.r.left.vf & FIX_COL) ? v1->col : v1->col + cp->coloffset;
        int maxr = (e->e.r.right.vf & FIX_ROW) ? v2->row : v2->row + cp->rowoffset;
        int maxc = (e->e.r.right.vf & FIX_COL) ? v2->col : v2->col + cp->coloffset;
        if (minr > maxr) SWAPINT(minr, maxr);
        if (minc > maxc) SWAPINT(minc, maxc);
        if (minr >= 0 && minc >= 0)
            return scvalue_range(rangeref(minr, minc, maxr, maxc));
    }
    return scvalue_error(cp, CELLERROR);
}

static scvalue_t eval_index(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e->e.o.left);
    enode_t *args = e->e.o.right;
    int r = 0, c = 0;

    if (res.type != SC_RANGE)
        return scvalue_error(cp, CELLERROR);

    if (args) {
        if (args->op == OP_COMMA) {     /* index by both row and column */
            r = (int)eval_num(cp, args->e.o.left);
            c = (int)eval_num(cp, args->e.o.right);
        } else if (res.u.rr.right.row == res.u.rr.left.row) {
            /* single row: argument is column index */
            c = (int)eval_num(cp, args);
        } else {
            r = (int)eval_num(cp, args);
        }
    }
    if (c < 0 || c > res.u.rr.right.col - res.u.rr.left.col + 1
    ||  r < 0 || r > res.u.rr.right.row - res.u.rr.left.row + 1)
        return scvalue_error(cp, CELLERROR);
    if (c > 0)
        res.u.rr.right.col = res.u.rr.left.col += c - 1;
    if (r > 0)
        res.u.rr.right.row = res.u.rr.left.row += r - 1;
    return res;
}

static scvalue_t eval_lookup(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a;
    scvalue_t rr = eval_node_value(cp, e->e.o.right->op == OP_COMMA ?
                                    e->e.o.right->e.o.left : e->e.o.right);
    scvalue_t dest;
    int r, c, incc = 0, incr = 0, dr = 0, dc = 0, offset, found = -1;

    if (rr.type != SC_RANGE || a.type == SC_ERROR) {
        scvalue_free(rr);
        return scvalue_error(cp, CELLERROR);
    }

    if (e->op == OP_LOOKUP) {
        if (rr.u.rr.right.row - rr.u.rr.left.row >= rr.u.rr.right.col - rr.u.rr.left.col) {
            incr = 1;
            offset = rr.u.rr.right.col - rr.u.rr.left.col;
        } else {
            incc = 1;
            offset = rr.u.rr.right.row - rr.u.rr.left.row;
        }
        if (e->e.o.right->op == OP_COMMA) {
            offset = 0;
            dest = eval_node(cp, e->e.o.right->e.o.right);
            if (dest.type != SC_RANGE
            ||  ((dr = dest.u.rr.right.row == dest.u.rr.left.row) != 0 &&
                 (dc = dest.u.rr.right.col == dest.u.rr.left.col) != 0)) {
                scvalue_free(dest);
                return scvalue_error(cp, CELLERROR);
            }
        }
    } else {
        offset = (int)eval_num(cp, (e->e.o.right->e.o.right->op == OP_COMMA ?
                                    e->e.o.right->e.o.right->e.o.left :
                                    e->e.o.right->e.o.right)) - 1;
        if (e->op == OP_VLOOKUP) {
            dr = incr = 1;
        } else {
            dc = incc = 1;
        }
        dest = rr;
    }

    a = eval_node_value(cp, e->e.o.left);
    // XXX: should implement binary search unless 4th argument is provided and false
    for (r = rr.u.rr.left.row, c = rr.u.rr.left.col; r <= rr.u.rr.right.row && c <= rr.u.rr.right.col; r += incr, c += incc) {
        struct ent *p = *ATBL(tbl, r, c);
        if (!p)
            continue;
        if (p->type == SC_NUMBER) {
            if (a.type == SC_NUMBER) {
                if (p->v > a.u.v) break;
                found = (r - dest.u.rr.left.row) + (c - dest.u.rr.left.col);
            }
        } else
        if (p->type == SC_STRING) {
            if (a.type == SC_STRING) {
                int cmp = strcmp(s2str(p->label), s2str(a.u.str));
                if (cmp > 0) break;
                found = (r - dest.u.rr.left.row) + (c - dest.u.rr.left.col);
            }
        }
    }
    scvalue_free(a);
    if (found >= 0) {
        r = dest.u.rr.left.row + (dr ? found : offset);
        c = dest.u.rr.left.col + (dc ? found : offset);
        return scvalue_range(rangeref(r, c, r, c));
    }
    return scvalue_error(cp, CELLERROR);    // ERROR_NA
}

/*---------------- aggregate functions ----------------*/

static int eval_test(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node_value(cp, e);
    if (a.type == SC_NUMBER || a.type == SC_BOOLEAN)
        return a.u.v != 0;
    if (a.type == SC_STRING) {
        int res = s2str(a.u.str)[0] != '\0';
        free_string(a.u.str);
        return res;
    }
    return 0;
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

static scvalue_t eval_rows_cols(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e->e.o.left);
    if (res.type == SC_RANGE) {
        return scvalue_number((e->op == OP_ROWS ?
                               res.u.rr.right.row - res.u.rr.left.row :
                               res.u.rr.right.col - res.u.rr.left.col));
    } else {
        return scvalue_number(0);
    }
}

struct agregatedata_t {
    int count;
    double v, v2;
};

static void aggregate_count(struct agregatedata_t *sp, double v) {
    sp->count++;
}
static void aggregate_max(struct agregatedata_t *sp, double v) {
    if (!sp->count++ || sp->v > v) sp->v = v;
}
static void aggregate_min(struct agregatedata_t *sp, double v) {
    if (!sp->count++ || sp->v < v) sp->v = v;
}
static void aggregate_product(struct agregatedata_t *sp, double v) {
    sp->v *= v;
    sp->count++;
}
static void aggregate_sum(struct agregatedata_t *sp, double v) {
    sp->v += v;
    sp->count++;
}
static void aggregate_sum2(struct agregatedata_t *sp, double v) {
    sp->v += v;
    sp->v2 += v * v;
    sp->count++;
}

static scvalue_t aggregate_average_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    return scvalue_number(ap->count ? ap->v / ap->count : ap->v);
}

static scvalue_t aggregate_count_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    return scvalue_number(ap->count);
}

static scvalue_t aggregate_v_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    return scvalue_number(ap->v);
}

static scvalue_t aggregate_v2_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    return scvalue_number(ap->v2);
}

static scvalue_t aggregate_stdev_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    double rp = 0.0;
    if (ap->count > 1) {
        double nd = (double)ap->count;
        rp = sqrt((nd * ap->v2 - ap->v * ap->v) / (nd * (nd - 1)));
    }
    return scvalue_number(rp);
}

static scvalue_t aggregate_stdevp_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    double rp = 0.0;
    if (ap->count > 0) {
        double nd = (double)ap->count;
        rp = sqrt((nd * ap->v2 - ap->v * ap->v) / (nd * nd));
    }
    return scvalue_number(rp);
}

static scvalue_t aggregate_var_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    double rp = 0.0;
    if (ap->count > 1) {
        double nd = (double)ap->count;
        rp = (nd * ap->v2 - ap->v * ap->v) / (nd * (nd - 1));
    }
    return scvalue_number(rp);
}

static scvalue_t aggregate_varp_ret(eval_ctx_t *cp, struct agregatedata_t *ap) {
    double rp = 0.0;
    if (ap->count > 0) {
        double nd = (double)ap->count;
        rp = (nd * ap->v2 - ap->v * ap->v) / (nd * nd);
    }
    return scvalue_number(rp);
}

/*
 * The list routines (e.g. eval_max) are called with a list of expressions.
 * The left pointer is an expression, the right pointer is a chain of OP_COMMA nodes
 */
static scvalue_t eval_aggregate(eval_ctx_t *cp, enode_t *ep,
                                void (*fun)(struct agregatedata_t *sp, double v),
                                scvalue_t (*retfun)(eval_ctx_t *cp, struct agregatedata_t *sp),
                                struct agregatedata_t *ap,
                                int allvalues)
{
    enode_t *e;

    for (e = ep; e; e = e->e.o.right) {
        scvalue_t res = eval_node(cp, e->e.o.left);
        switch (res.type) {
        case SC_RANGE: {
                int r, c;
                struct ent *p;
                for (r = res.u.rr.left.row; r <= res.u.rr.right.row; r++) {
                    for (c = res.u.rr.left.col; c <= res.u.rr.right.col; c++) {
                        if ((p = *ATBL(tbl, r, c))) {
                            switch (p->type) {
                            case SC_NUMBER:  fun(ap, p->v); break;
                            case SC_BOOLEAN: if (allvalues) fun(ap, p->v); break;
                            case SC_ERROR:
                            case SC_STRING:  if (allvalues) fun(ap, 0); break;
                            }
                        }
                    }
                }
                break;
            }
        case SC_NUMBER:     fun(ap, res.u.v); break;
        case SC_BOOLEAN:    if (allvalues) fun(ap, res.u.v); break;
        case SC_ERROR:
        case SC_STRING:     if (allvalues) fun(ap, 0);
                            scvalue_free(res);
                            break;
        }
    }
    return retfun(cp, ap);
}

static scvalue_t eval_aggregateif(eval_ctx_t *cp, enode_t *ep,
                                  void (*fun)(struct agregatedata_t *sp, double v),
                                  scvalue_t (*retfun)(eval_ctx_t *cp, struct agregatedata_t *sp),
                                  struct agregatedata_t *ap,
                                  int allvalues)
{
    scvalue_t res = eval_node(cp, ep->e.o.left);
    enode_t *test = ep->e.o.left;
    int r, c;
    struct ent *p;

    if (res.type != SC_RANGE) {
        scvalue_free(res);
        return scvalue_error(cp, CELLERROR);
    }
    // XXX: should implement simple comparisons as a string
    for (r = res.u.rr.left.row; r <= res.u.rr.right.row; r++) {
        for (c = res.u.rr.left.col; c <= res.u.rr.right.col; c++) {
            if (test && !eval_test_offset(cp, test, r - res.u.rr.left.row, c - res.u.rr.left.col))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                fun(ap, p->v);
            }
        }
    }
    return retfun(cp, ap);
}

static scvalue_t eval_average(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum, aggregate_average_ret, &pack, ep->op == OP_AVERAGEA);
}

static scvalue_t eval_count(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_count, aggregate_count_ret, &pack, ep->op == OP_COUNTA);
}

static scvalue_t eval_max(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_max, aggregate_v_ret, &pack, ep->op == OP_MAXA);
}

static scvalue_t eval_min(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_min, aggregate_v_ret, &pack, ep->op == OP_MINA);
}

static scvalue_t eval_product(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 1.0, 0 };
    return eval_aggregate(cp, ep, aggregate_product, aggregate_v_ret, &pack, FALSE);
}

static scvalue_t eval_sum(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum, aggregate_v_ret, &pack, FALSE);
}

static scvalue_t eval_sumsq(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum2, aggregate_v2_ret, &pack, FALSE);
}

static scvalue_t eval_stdev(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum2, aggregate_stdev_ret, &pack, ep->op == OP_STDEVA);
}

static scvalue_t eval_stdevp(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum2, aggregate_stdevp_ret, &pack, ep->op == OP_STDEVPA);
}

static scvalue_t eval_var(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum2, aggregate_var_ret, &pack, ep->op == OP_VARA);
}

static scvalue_t eval_varp(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregate(cp, ep, aggregate_sum2, aggregate_varp_ret, &pack, ep->op == OP_VARPA);
}

static scvalue_t eval_averageif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_sum, aggregate_average_ret, &pack, ep->op == OP_AVERAGEA);
}

static scvalue_t eval_countif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_count, aggregate_count_ret, &pack, ep->op == OP_COUNTA);
}

static scvalue_t eval_maxif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_max, aggregate_v_ret, &pack, ep->op == OP_MAXA);
}

static scvalue_t eval_minif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_min, aggregate_v_ret, &pack, ep->op == OP_MINA);
}

static scvalue_t eval_sumif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_sum, aggregate_v_ret, &pack, FALSE);
}

/*---------------- date and time functions ----------------*/

// XXX: should accept 6 or 7 arguments
// XXX: should use integral part for day and fraction for time.
//      which may be incorrect for DST time adjustments.
static scvalue_t eval_dts(eval_ctx_t *cp, enode_t *e) {
    int yr = (int)eval_num(cp, e->e.o.left);
    int mo = (int)eval_num(cp, e->e.o.right->e.o.left);
    int day = (int)eval_num(cp, e->e.o.right->e.o.right);
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

static scvalue_t eval_now(eval_ctx_t *cp, enode_t *e) {
    // XXX: should use a more precise time value
    return scvalue_number((double)time(NULL));
}

static scvalue_t eval_time(eval_ctx_t *cp, enode_t *e) {
    static time_t t_cache;
    static struct tm tm_cache;
    struct tm *tp = &tm_cache;
    int which = e->op;
    time_t tloc = (time_t)eval_num(cp, e->e.o.left);

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

static scvalue_t eval_ston(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node_value(cp, e->e.o.left);
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

static scvalue_t eval_getent(eval_ctx_t *cp, enode_t *e) {
    SCXMEM string_t *colstr = eval_str(cp, e->e.o.left);
    double rowdoub = eval_num(cp, e->e.o.right);
    int row = (int)floor(rowdoub);
    int col = atocol(s2str(colstr), NULL);

    free_string(colstr);
    // XXX: should return a reference?
    if (row >= 0 && col >= 0)
        return scvalue_getcell(cp, row, col);
    else
        return scvalue_error(cp, CELLERROR);
}

/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

static scvalue_t eval_nval(eval_ctx_t *cp, enode_t *e) {
    // XXX: should return an SC_RANGE and use eval_make_number()
    scvalue_t res = eval_getent(cp, e);
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

/*---------------- math functions ----------------*/

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

static double radians(double x) { return x * (M_PI / 180.0); }
static double degrees(double x) { return x * (180.0 / M_PI); }
static double sc_sign(double x) { return x < 0 ? -1 : x > 0; }

static scvalue_t eval_fn1(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    double res;
    errno = 0;
    res = ((double (*)(double))fun)(eval_num(cp, e->e.o.left));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static scvalue_t eval_fn2(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    double res;
    errno = 0;
    res = ((double (*)(double, double))fun)
        (eval_num(cp, e->e.o.left), eval_num(cp, e->e.o.right));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

static scvalue_t eval_fn3(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    double res;
    errno = 0;
    res = ((double (*)(double, double, double))fun)
        (eval_num(cp, e->e.o.left),
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

static scvalue_t eval_rand(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number((double)rand() / ((double)RAND_MAX + 1));
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
static scvalue_t eval_fl1(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    sclong_t res;
    errno = 0;
    res = ((sclong_t (*)(sclong_t))fun)(eval_long(cp, e->e.o.left));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}
#endif

static scvalue_t eval_fl2(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    sclong_t res;
    errno = 0;
    res = ((sclong_t (*)(sclong_t, sclong_t))fun)
        (eval_long(cp, e->e.o.left), eval_long(cp, e->e.o.right));
    if (errno)
        return scvalue_error(cp, CELLERROR);
    return scvalue_number(res);
}

#if 0
static scvalue_t eval_fl3(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    sclong_t res;
    errno = 0;
    res = ((sclong_t (*)(sclong_t, sclong_t, sclong_t))fun)
        (eval_long(cp, e->e.o.left),
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

static sclong_t makecolor(sclong_t a, sclong_t b) {
    return (a & 7) + ((b & 7) << 3);
}

/*---------------- string functions ----------------*/

/*
 * Rules for string functions:
 * Take string arguments which they scxfree.
 * All returned strings are assumed to be xalloced.
 */

static scvalue_t eval_date(eval_ctx_t *cp, enode_t *e) {
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

static scvalue_t eval_fmt(eval_ctx_t *cp, enode_t *e) {
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

static scvalue_t eval_ext(eval_ctx_t *cp, enode_t *se) {
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
        return eval_node(cp, prev);
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

static scvalue_t eval_sval(eval_ctx_t *cp, enode_t *e) {
    char buf[32];
    // XXX: should return an SC_RANGE and use eval_make_string()
    scvalue_t res = eval_getent(cp, e);
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
static scvalue_t eval_case(eval_ctx_t *cp, enode_t *e) {
    SCXMEM string_t *s = eval_str(cp, e->e.o.left);
    SCXMEM string_t *s2;
    char *p;

    if (sempty(s))
        return scvalue_string(s);

    s2 = new_string_len(s2c(s), slen(s));
    if (!s2)
        return scvalue_error(cp, CELLERROR);

    free_string(s);
    switch (e->op) {
    case OP_UPPER:
        for (p = s2->s; *p; p++) {
            if (islowerchar(*p))
                *p = toupperchar(*p);
        }
        break;
    case OP_LOWER:
        for (p = s2->s; *p; p++) {
            if (isupperchar(*p))
                *p = tolowerchar(*p);
        }
        break;
    case OP_CAPITAL: {
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
        break;
    }
    return scvalue_string(s2);
}

static scvalue_t eval_substr(eval_ctx_t *cp, enode_t *e) {
    /* Substring: Note that v1 and v2 are one-based and v2 is included */
    SCXMEM string_t *str = eval_str(cp, e->e.o.left);
    int v1 = (int)eval_num(cp, e->e.o.right->e.o.left);
    int v2 = (int)eval_num(cp, e->e.o.right->e.o.right);
    return scvalue_string(sub_string(str, v1 - 1, v2));
}

static scvalue_t eval_concat(eval_ctx_t *cp, enode_t *e) {
    // XXX: should accept expression list
    return scvalue_string(cat_strings(eval_str(cp, e->e.o.left), eval_str(cp, e->e.o.right)));
}

static scvalue_t eval_filename(eval_ctx_t *cp, enode_t *e) {
    int n = eval_test(cp, e->e.o.left);
    const char *s = n ? curfile : get_basename(curfile);
    return scvalue_string(new_string(s));
}

static scvalue_t eval_coltoa(eval_ctx_t *cp, enode_t *e) {
    return scvalue_string(new_string(coltoa((int)eval_num(cp, e->e.o.left))));
}

static scvalue_t eval_not(eval_ctx_t *cp, enode_t *e) {
    // XXX: handle errors?
    return scvalue_bool(!eval_test(cp, e->e.o.left));
}

static scvalue_t eval_and(eval_ctx_t *cp, enode_t *e) {
    return eval_test(cp, e->e.o.left) ? eval_node_value(cp, e->e.o.right) : scvalue_bool(0);
}

static scvalue_t eval_or(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node_value(cp, e->e.o.left);
    if (a.type == SC_NUMBER || a.type == SC_BOOLEAN) {
        if (a.u.v != 0)
            return a;
    } else
    if (a.type == SC_STRING) {
        if (s2str(a.u.str)[0] != '\0')
            return a;
        free_string(a.u.str);
    }
    return eval_node_value(cp, e->e.o.right);
}

static scvalue_t eval_if(eval_ctx_t *cp, enode_t *e) {
    return eval_node(cp, eval_test(cp, e->e.o.left) ?
                     e->e.o.right->e.o.left : e->e.o.right->e.o.right);
}

static int is_relative(int op) {
    return op == OP_LT || op == OP_GT || op == OP_LE || op == OP_GE;
}

static scvalue_t eval_cmp(eval_ctx_t *cp, enode_t *e) {
    int op = e->op;
    scvalue_t a = eval_node_value(cp, e->e.o.left);
    scvalue_t b = eval_node_value(cp, e->e.o.right);
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

static scvalue_t eval_const(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(e->e.k);
}

static scvalue_t eval_sconst(eval_ctx_t *cp, enode_t *e) {
    return scvalue_string(dup_string(e->e.s));
}

static scvalue_t eval_other(eval_ctx_t *cp, enode_t *e) {
    int val = 0;
    switch (e->op) {
    case OP_MYROW:      val = cp->gmyrow + cp->rowoffset; break;
    case OP_MYCOL:      val = cp->gmycol + cp->coloffset; break;
    case OP_LASTROW:    val = maxrow;           break;
    case OP_LASTCOL:    val = maxcol;           break;
    case OP_NUMITER:    val = repct;            break;
    case OP_BLACK:      val = COLOR_BLACK;      break;
    case OP_RED:        val = COLOR_RED;        break;
    case OP_GREEN:      val = COLOR_GREEN;      break;
    case OP_YELLOW:     val = COLOR_YELLOW;     break;
    case OP_BLUE:       val = COLOR_BLUE;       break;
    case OP_MAGENTA:    val = COLOR_MAGENTA;    break;
    case OP_CYAN:       val = COLOR_CYAN;       break;
    case OP_WHITE:      val = COLOR_WHITE;      break;
    case OP_FALSE:      return scvalue_bool(0); break;
    case OP_TRUE:       return scvalue_bool(1); break;
    case OP_UPLUS:      return eval_node_value(cp, e->e.o.left);
    default:            error("Illegal expression");
                        exprerr = 1;
                        FALLTHROUGH;
    case OP_ERR:        return scvalue_error(cp, CELLERROR);
    }
    return scvalue_number(val);
}

/*---------------- dynamic evaluator ----------------*/

/* opcode definitions, used for evaluator and decompiler */
// XXX: should use for parser and constant_node() too.
struct opdef const opdefs[] = {
#define OP(op,str,min,max,efun,arg)  { str, min, max, 0, 0, efun, (scarg_t)(arg) },
#include "opcodes.h"
#undef OP
};

scvalue_t eval_node(eval_ctx_t *cp, enode_t *e) {
    if (e == NULL)
        return scvalue_empty();

    if (e->op >= 0 && e->op < OP_count) {
        const struct opdef *opp = &opdefs[e->op];
        if (opp->efun)
            return opp->efun(cp, e);
    }
    return eval_other(cp, e);
}

/*---------------- typed evaluators ----------------*/

scvalue_t eval_node_value(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e);
    if (res.type == SC_RANGE) {
        /* reduce dimensions by intersecting with cell row and column */
        int row = res.u.rr.left.row;
        int col = res.u.rr.left.col;
        if ((row == res.u.rr.right.row || ((row = cp->gmyrow) >= res.u.rr.left.row && row <= res.u.rr.right.row))
        &&  (col == res.u.rr.right.col || ((col = cp->gmycol) >= res.u.rr.left.col && row <= res.u.rr.right.col))) {
            return scvalue_getcell(cp, row, col);
        }
        return scvalue_error(cp, CELLERROR);
    } else {
        return res;
    }
}

scvalue_t eval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0, 0 }};
    return eval_node_value(cp, e);
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
        res = eval_node_value(cp, e);
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
    return new_node(op, a1, new_node(OP_COMMA, a2, a3));
}

SCXMEM enode_t *new_var(cellref_t cr) {
    SCXMEM enode_t *p;

    if ((p = new_node(OP_VARARG, NULL, NULL))) {
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
               int r1, int c1, int r2, int c2, int transpose)
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
        newrow = ((vf & FIX_ROW) || row < r1 || row > r2 || col < c1 || col > c2 ?
              row : transpose ? r1 + Rdelta + col - c1 : row + Rdelta);
        newcol = ((vf & FIX_COL) || row < r1 || row > r2 || col < c1 || col > c2 ?
              col : transpose ? c1 + Cdelta + row - r1 : col + Cdelta);
        ret->e.v.vp = lookat(newrow, newcol);
        ret->e.v.vf = vf;
    } else
    if (e->type == OP_TYPE_DOUBLE) {
        ret->type = OP_TYPE_DOUBLE;
        ret->e.k = e->e.k;
    } else
    if (e->type == OP_TYPE_STRING) {
        ret->type = OP_TYPE_STRING;
        ret->e.s = dup_string(e->e.s);
    } else {
        if ((e->e.o.left && !(ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
                                                    r1, c1, r2, c2, transpose)))
        ||  (e->e.o.right && !(ret->e.o.right = copye(e->e.o.right, Rdelta, Cdelta,
                                                      r1, c1, r2, c2, transpose))))
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
    if (*s == '@') {
        buf_puts(dcp->buf, s);
    } else {
        buf_putc(dcp->buf, '@');
        while (*s && *s != '(')
            buf_putc(dcp->buf, tolowerchar(*s++));
    }
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

static void decompile_node(decomp_t *dcp, enode_t *e, int priority) {
    const struct opdef *opp;

    if (!e) {
        buf_putc(dcp->buf, '?');
        return;
    }
    opp = &opdefs[e->op];
    switch (e->op) {
    case OP_DUMMY:      decompile_node(dcp, e->e.o.right, priority); break;
    case OP_CONST:      out_const(dcp, e->e.k);         break;
    case OP_SCONST:     out_sconst(dcp, s2c(e->e.s));   break;
    case OP_VARARG:     out_var(dcp, e->e.v, 1);        break;
    case OP_RANGEARG:   out_range(dcp, e);              break;
    case OP_UMINUS:
    case OP_UPLUS:
    case OP_BANG:       out_unary(dcp, opp->name, e); break;
    case OP_SEMI:       out_infix(dcp, opp->name, e, priority, 1); break;
    case OP_VBAR:       out_infix(dcp, opp->name, e, priority, 4); break;
    case OP_AMPERSAND:  out_infix(dcp, opp->name, e, priority, 5); break;
    case OP_EQ:
    case OP_NE:
    case OP_LG:
    case OP_LT:
    case OP_LE:
    case OP_GE:
    case OP_GT:         out_infix(dcp, opp->name, e, priority, 6); break;
    case OP_SHARP:      out_infix(dcp, opp->name, e, priority, 7); break;
    case OP_PLUS:
    case OP_MINUS:      out_infix(dcp, opp->name, e, priority, 8); break;
    case OP_STAR:
    case OP_SLASH:
    case OP_PERCENT:    out_infix(dcp, opp->name, e, priority, 10); break;
    case OP_CARET:      out_infix(dcp, opp->name, e, priority, 12); break;
    case OP_COLON:      out_infix(dcp, opp->name, e, priority, 13); break;
    default:            if (e->op >= 0 && e->op < OP_count) {
                            out_func(dcp, opp->name, (opp->min < 0) ? NULL : e);
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
