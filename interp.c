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
double prescale = 1.0;  /* Prescale for constants in let() */
int extfunc = 0;        /* Enable/disable external functions */
int loading = 0;        /* Set when readfile() is active */
int propagation = 10;   /* max number of times to try calculation */
static int repct = 1;   /* Make repct a global variable so that the
                           function @numiter can access it */

const char * const error_name[] = {
    NULL,
    "#NULL!",   // ERROR_NULL: Intersection of ranges produced zero cells.
    "#DIV/0!",  // ERROR_DIV0: Attempt to divide by zero, including division by an empty cell. 6.13.11
    "VALUE!",   // ERROR_VALUE: Parameter is wrong type.
    "#REF!",    // ERROR_REF: Reference to invalid cell (e.g., beyond the application's abilities).
    "#NAME?",   // ERROR_NAME: Unrecognized/deleted name.
    "#NUM!",    // ERROR_NUM: Failed to meet domain constraints (e.g., input was too large or too small).
    "#N/A",     // ERROR_NA: Not available. ISNA() returns TRUE for this value. Used for Lookup functions which fail.
};

static SCXMEM enode_t *new_node(int op, int nargs);
extern scvalue_t eval_node(eval_ctx_t *cp, enode_t *e);
extern scvalue_t eval_node_value(eval_ctx_t *cp, enode_t *e);
static scvalue_t scvalue_getcell(eval_ctx_t *cp, int row, int col);
static int RealEvalAll(void);
static int RealEvalOne(struct ent *p, enode_t *e, int i, int j);

#ifdef RINT
double rint(double d);
#endif
static sigret_t eval_fpe(int);

#ifndef M_PI
#define M_PI (double)3.14159265358979323846
#endif

static double math_acot(double x) { return atan(1 / x); }
static double math_acoth(double x) { return log((x + 1) / (x - 1)) / 2; }
static double math_cot(double x) { return 1 / tan(x); }
static double math_coth(double x) { return 1 / tanh(x); }
static double math_csc(double x) { return 1 / sin(x); }
static double math_csch(double x) { return 1 / sinh(x); }
static double math_sec(double x) { return 1 / cos(x); }
static double math_sech(double x) { return 1 / cosh(x); }
static double math_sqrtpi(double x) { return sqrt(x * M_PI); }
static double math_trunc(double x) { return x < 0 ? ceil(x) : floor(x); }
static double math_grow(double x) { return x < 0 ? floor(x) : ceil(x); }
static double math_even(double v) { return 2 * math_grow(v / 2); }
static double math_odd(double v) { return v < 0 ? 2 * floor((v + 1) / 2) - 1 : 2 * ceil((v - 1) / 2) + 1; }
static double math_radians(double x) { return x * (M_PI / 180.0); }
static double math_degrees(double x) { return x * (180.0 / M_PI); }
static double math_sign(double x) { return x < 0 ? -1 : x > 0; }
static double math_percent(double x) { return x / 100; }

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

static inline scvalue_t scvalue_error(int error) {
    scvalue_t res;
    res.type = SC_ERROR;
    res.u.error = error;
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
    if (str) {
        res.type = SC_STRING;
        res.u.str = str;
    } else {
        res.type = SC_ERROR;
        res.u.error = ERROR_NULL;
    }
    return res;
}

static inline scvalue_t scvalue_range(rangeref_t rr) {
    scvalue_t res;
    res.type = SC_RANGE;
    res.u.rr = rr;
    return res;
}

static scvalue_t eval__number(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(e->e.k);
}

static scvalue_t eval__string(eval_ctx_t *cp, enode_t *e) {
    return scvalue_string(dup_string(e->e.s));
}

static double eval_num(eval_ctx_t *cp, enode_t *e, int *errp) {
    scvalue_t res = eval_node_value(cp, e);
    if (res.type == SC_NUMBER || res.type == SC_BOOLEAN)
        return res.u.v;
    if (res.type == SC_EMPTY)
        return 0.0;
    if (res.type == SC_STRING) {
        // XXX: should accept extended syntax, including trailing %
        char *end, c;
        double v = strtod(s2c(res.u.str), &end);
        c = *end;
        free_string(res.u.str);
        if (!c)
            return v;
        *errp = ERROR_VALUE; /* invalid conversion */
    } else {
        /* type is SC_ERROR */
        *errp = res.u.error;
    }
    return 0.0;
}

static sclong_t eval_long(eval_ctx_t *cp, enode_t *e, int *errp) {
    // XXX: simplify this if SC_LONG becomes a value type
    // XXX: check rounding issues
    return (sclong_t)eval_num(cp, e, errp);
}

static SCXMEM string_t *eval_str(eval_ctx_t *cp, enode_t *e, int *errp) {
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
    /* type is SC_ERROR */
    *errp = res.u.error;
    return NULL;
}

static scvalue_t eval_colon(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node(cp, e->e.args[0]);
    scvalue_t b = eval_node(cp, e->e.args[1]);
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
    scvalue_free(a);
    scvalue_free(b);
    return scvalue_error(ERROR_VALUE);
}

static scvalue_t eval_add(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double v = eval_num(cp, e->e.args[0], &err) + eval_num(cp, e->e.args[1], &err);
    return err ? scvalue_error(err) : scvalue_number(v);
}

static scvalue_t eval_sub(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double v = eval_num(cp, e->e.args[0], &err) - eval_num(cp, e->e.args[1], &err);
    return err ? scvalue_error(err) : scvalue_number(v);
}

static scvalue_t eval_mul(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double v = eval_num(cp, e->e.args[0], &err) * eval_num(cp, e->e.args[1], &err);
    return err ? scvalue_error(err) : scvalue_number(v);
}

static scvalue_t eval_neg(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double v = -eval_num(cp, e->e.args[0], &err);
    return err ? scvalue_error(err) : scvalue_number(v);
}

static scvalue_t eval_div(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double num = eval_num(cp, e->e.args[0], &err);
    double denom = eval_num(cp, e->e.args[1], &err);
    if (err) return scvalue_error(err);
    if (!denom) return scvalue_error(ERROR_DIV0);
    return scvalue_number(num / denom);
}

static scvalue_t eval_quotient(eval_ctx_t *cp, enode_t *e) {
    // XXX: should factorize with eval_div
    int err = 0;
    double num = eval_num(cp, e->e.args[0], &err);
    double denom = eval_num(cp, e->e.args[1], &err);

    if (err) return scvalue_error(err);
    if (!denom) return scvalue_error(ERROR_DIV0);
    return scvalue_number(math_trunc(num / denom));
}

static scvalue_t eval_mod(eval_ctx_t *cp, enode_t *e) {
    // XXX: should factorize with eval_div
    int err = 0;
    double num = eval_num(cp, e->e.args[0], &err);
    double denom = eval_num(cp, e->e.args[1], &err);

    if (err) return scvalue_error(err);
    if (!denom) return scvalue_error(ERROR_DIV0);
    return scvalue_number(num - math_trunc(num / denom) * denom);
}

static scvalue_t eval_pi(eval_ctx_t *cp, enode_t *e) {
    return scvalue_number(M_PI);
}

static scvalue_t eval_fact(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int num = (int)eval_num(cp, e->e.args[0], &err);
    double res;
    if (err) return scvalue_error(err);
    if (num < 0 || num > 200)
        return scvalue_error(ERROR_NUM);
    for (res = 1.0; num > 1; num -= 1 + (e->op == OP_FACTDOUBLE))
        res *= num;
    return scvalue_number(res);
}

static scvalue_t eval_combin(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int n = (int)eval_num(cp, e->e.args[0], &err);
    int r = (int)eval_num(cp, e->e.args[1], &err);
    double res;
    if (err) return scvalue_error(err);
    if (n >= 0 && r >= 0 && n >= r) {
        if (e->op == OP_COMBINA) {
            n = n + r - 1;
            r = n - r;
        }
        if (r > n - r) {
            r = n - r;
        }
        res = 1;
        while (r > 0) {
            res = res * n-- / r--;
        }
        return scvalue_number(floor(res + 0.5));
    }
    return scvalue_error(ERROR_NUM);
}

static scvalue_t eval_permut(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int n = (int)eval_num(cp, e->e.args[0], &err);
    int k = (int)eval_num(cp, e->e.args[1], &err);
    double res;
    if (err) return scvalue_error(err);
    if (e->op == OP_PERMUTATIONA) {
        if (n >= 0 && k >= 0) {
            scvalue_number(pow(n, k));
        }
    } else {
        if (n >= 0 && k >= 0 && n >= k) {
            for (res = 1; n > k; n--)
                res *= n;
            return scvalue_number(res);
        }
    }
    return scvalue_error(ERROR_NUM);
}

static sculong_t gcd_ulong(sculong_t u, sculong_t v) {
    int d = 0, ub, vb;
    while (u != v) {
        if (u < v) {
            sculong_t t = u;
            u = v;
            v = t;
        }
        if (u & v & 1) { // u and v are both odd
            u = (u - v) >> 1;
        } else {
            if (v == 0) break;
            ub = (u & 1) ^ 1;
            vb = (v & 1) ^ 1;
            u >>= ub;
            v >>= vb;
            d += ub & vb;
        }
    }
    return u << d;
}

static scvalue_t eval_gcd_lcm(eval_ctx_t *cp, enode_t *e) {
    int i, err = 0;
    sclong_t gcd = 0;
    double lcm = 1;
    for (i = 0; i < e->nargs; i++) {
        sclong_t b = (sclong_t)eval_num(cp, e->e.args[i], &err);
        if (err) return scvalue_error(err);
        if (b < 0) return scvalue_error(ERROR_NUM);
        if (b > 0) {
            gcd = gcd_ulong(gcd, b);
            b = b / gcd;
        }
        lcm = lcm * b;
    }
    return scvalue_number(e->op == OP_LCM ? lcm : (double)gcd);
}

static scvalue_t eval_mround(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double n = eval_num(cp, e->e.args[0], &err);
    double significance = 1;
    double mode = 0;
    double adjust = (e->op == OP_MROUND) ? 0.5 : 0;
    double res = 0;
    /* silently accept significance and n with opposite signs */
    if (e->nargs > 1)
        significance = fabs(eval_num(cp, e->e.args[1], &err));
    if (e->nargs > 2)
        mode = eval_num(cp, e->e.args[2], &err);
    if (err) return scvalue_error(err);
    if (n != 0 && significance != 0) {
        if (mode) {
            significance = math_sign(n) * significance;
        }
        if (e->op == OP_CEILING)
            res = ceil(n / significance) * significance;
        else
            res = (floor(n / significance) + adjust) * significance;
    }
    return scvalue_number(res);
}

static scvalue_t eval_round(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double x = eval_num(cp, e->e.args[0], &err);
    int digits = 0;
    double scale = 1.0;
    double adjust = (e->op == OP_ROUND) ? 0.5 : 0;
    double res = 0;
    if (e->nargs > 1)
        digits = (int)eval_num(cp, e->e.args[1], &err);
    if (err) return scvalue_error(err);
    while (digits > 0) {
        scale /= 10.0;
        digits--;
    }
    while (digits < 0) {
        scale *= 10.0;
        digits++;
    }
    if (e->op == OP_ROUNDUP) {
        res = math_grow(x / scale) * scale;
    } else {
        res = math_trunc(x / scale + adjust) * scale;
    }
    return scvalue_number(res);
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
            return scvalue_error(ERROR_REF);
        if (p->type == SC_ERROR)
            return scvalue_error(p->cellerror);
        if (p->type == SC_NUMBER)
            return scvalue_number(p->v);
        if (p->type == SC_STRING)
            return scvalue_string(dup_string(p->label));
        if (p->type == SC_BOOLEAN)
            return scvalue_bool(p->v);
    }
    if (row < 0 || col < 0)
        return scvalue_error(ERROR_REF);

    return scvalue_empty();
}

static scvalue_t eval__var(eval_ctx_t *cp, enode_t *e) {
    struct ent *vp = e->e.v.vp;
    if (vp) {
        int row = (e->e.v.vf & FIX_ROW) ? vp->row : vp->row + cp->rowoffset;
        int col = (e->e.v.vf & FIX_COL) ? vp->col : vp->col + cp->coloffset;
        if (row >= 0 && col >= 0)
            return scvalue_range(rangeref(row, col, row, col));
    }
    return scvalue_error(ERROR_REF);
}

static scvalue_t eval__range(eval_ctx_t *cp, enode_t *e) {
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
    return scvalue_error(ERROR_REF);
}

static scvalue_t eval_address(eval_ctx_t *cp, enode_t *e) {
    char buff[32];
    int err = 0;
    int row = (int)eval_num(cp, e->e.args[0], &err);
    int col = (int)eval_num(cp, e->e.args[1], &err);
    int rel = e->nargs > 2 ? (int)eval_num(cp, e->e.args[2], &err) : 1;
    if (err) return scvalue_error(err);
    if (row < 0 || row > ABSMAXCOLS || col < 0 || col > ABSMAXCOLS || rel < 1 || rel > 4)
        return scvalue_error(ERROR_NUM);
    snprintf(buff, sizeof buff, "%s%s%s%d", &"$"[(rel & 1) ^ 1], coltoa(col), &"$"[rel > 2], row);
    return scvalue_string(new_string(buff));
}

static scvalue_t eval_indirect(eval_ctx_t *cp, enode_t *e) {
    int err = 0, len;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    const char *s;
    struct nrange *r;
    rangeref_t rr;
    int minc = 0, minr = 0, maxc = 0, maxr = 0;

    if (!err) {
        s = s2c(str);
        len = slen(str);
        if (parse_rangeref(s, &rr, NULL)) {
            minr = rr.left.row;
            minc = rr.left.col;
            maxr = rr.right.row;
            maxc = rr.right.col;
        } else
        if (find_nrange_name(s, len, &r)) {
            minr = r->r_left.vp->row;
            minc = r->r_left.vp->col;
            maxr = r->r_right.vp->row;
            maxc = r->r_right.vp->col;
        } else {
            err = ERROR_REF;
        }
        free_string(str);
    }
    if (err) {
        return scvalue_error(err);
    } else {
        if (minr > maxr) SWAPINT(minr, maxr);
        if (minc > maxc) SWAPINT(minc, maxc);
        return scvalue_range(rangeref(minr, minc, maxr, maxc));
    }
}

static scvalue_t eval_choose(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int index = (int)eval_num(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    if (index < 1 || index > e->nargs) return scvalue_error(ERROR_VALUE);
    return eval_node(cp, e->e.args[index]);
}

static scvalue_t eval_index(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e->e.args[0]);
    int err = 0, r = 0, c = 0;

    if (res.type != SC_RANGE) {
        scvalue_free(res);
        return scvalue_error(ERROR_VALUE);
    }
    if (e->nargs > 1) {
        if (e->nargs > 2) {     /* index by both row and column */
            r = (int)eval_num(cp, e->e.args[1], &err);
            c = (int)eval_num(cp, e->e.args[2], &err);
        } else if (res.u.rr.right.row == res.u.rr.left.row) {
            /* single row: argument is column index */
            c = (int)eval_num(cp, e->e.args[1], &err);
        } else {
            r = (int)eval_num(cp, e->e.args[1], &err);
        }
        if (err) return scvalue_error(err);
    }
    if (c < 0 || c > res.u.rr.right.col - res.u.rr.left.col + 1
    ||  r < 0 || r > res.u.rr.right.row - res.u.rr.left.row + 1)
        return scvalue_error(ERROR_REF);
    if (c > 0)
        res.u.rr.right.col = res.u.rr.left.col += c - 1;
    if (r > 0)
        res.u.rr.right.row = res.u.rr.left.row += r - 1;
    return res;
}

static scvalue_t eval_lookup(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node_value(cp, e->e.args[0]);
    scvalue_t rr, dest;
    int r, c, incc = 0, incr = 0, dr = 0, dc = 0, ncols, nrows;
    int i, count, found = -1, sorted = 1, offset = 0, err = 0;

    if (a.type == SC_ERROR)
        return a;

    for (;;) {
        rr = eval_node(cp, e->e.args[1]);
        if (rr.type != SC_RANGE) {
            scvalue_free(rr);
            err = ERROR_VALUE;
            break;
        }
        ncols = rr.u.rr.right.col - rr.u.rr.left.col + 1;
        nrows = rr.u.rr.right.row - rr.u.rr.left.row + 1;
        if (e->op == OP_MATCH) {
            sorted = (int)eval_num(cp, e->e.args[2], &err);
            if (err) break;
            if (sorted < -1 || sorted > 1) {
                err = ERROR_NUM;
                break;
            }
            if (ncols == 1) {
                incr = 1;
            } else
            if (nrows == 1) {
                incc = 1;
            } else {
                err = ERROR_VALUE;
                break;
            }
        } else
        if (e->op == OP_LOOKUP) {
            if (nrows >= ncols) {
                incr = 1;
                offset = ncols - 1;
            } else {
                incc = 1;
                offset = nrows - 1;
            }
            if (e->nargs > 2) {
                dest = eval_node(cp, e->e.args[2]);
                if (dest.type != SC_RANGE) {
                    scvalue_free(dest);
                    err = ERROR_VALUE;
                    break;
                }
                dr = (dest.u.rr.left.row == dest.u.rr.right.row);
                dc = (dest.u.rr.left.col == dest.u.rr.right.col);
            }
        } else {
            /* op is OP_HLOOKUP or OP_VLOOKUP */
            dest = rr;
            offset = (int)eval_num(cp, e->e.args[2], &err) - 1;
            if (e->nargs > 3 && !eval_num(cp, e->e.args[3], &err))
                sorted = 0;
            if (err) break;
            if (e->op == OP_VLOOKUP) {
                dr = incr = 1;
            } else {
                dc = incc = 1;
            }
        }

        // XXX: should implement binary search if sorted
        count = ncols * incc + nrows * incr;
        for (i = 0; i < count; i++) {
            struct ent *p;
            int cmp;

            r = rr.u.rr.left.row + i * incr;
            c = rr.u.rr.left.col + i * incc;
            p = *ATBL(tbl, r, c);
            if (!p || p->type == SC_EMPTY) {
                cmp = (a.type == SC_EMPTY) ? 0 : 1;
            } else
            if (p->type == a.type) {
                if (a.type == SC_NUMBER || a.type == SC_BOOLEAN) {
                    cmp = (p->v > a.u.v) - (p->v < a.u.v);
                } else
                if (a.type == SC_STRING) {
                    cmp = strcmp(s2str(p->label), s2str(a.u.str));
                } else {
                    cmp = p->cellerror - a.u.error;
                }
            } else {
                cmp = p->type - a.type;
            }
            if (sorted > 0 && cmp > 0) break;
            if (sorted < 0 && cmp < 0) break;
            if (sorted || cmp == 0) {
                found = i;
            }
        }
        if (found >= 0) {
            scvalue_free(a);
            if (e->op == OP_MATCH) return scvalue_number(found + 1);
            r = dest.u.rr.left.row + (dr ? found : offset);
            c = dest.u.rr.left.col + (dc ? found : offset);
            return scvalue_range(rangeref(r, c, r, c));
        }
        err = ERROR_NA;
        break;
    }
    scvalue_free(a);
    return scvalue_error(err);
}

/*---------------- aggregate functions ----------------*/

static int eval_test(eval_ctx_t *cp, enode_t *e, int *errp) {
    scvalue_t a = eval_node_value(cp, e);
    if (a.type == SC_NUMBER || a.type == SC_BOOLEAN)
        return a.u.v != 0;
    if (a.type == SC_STRING) {
        int res = s2str(a.u.str)[0] != '\0';
        free_string(a.u.str);
        return res;
    }
    *errp = a.u.error;
    return 0;
}

static int eval_test_offset(eval_ctx_t *cp, enode_t *e, int roffset, int coffset, int *errp) {
    int save_rowoffset = cp->rowoffset;
    int save_coloffset = cp->coloffset;
    int res;
    cp->rowoffset = roffset;
    cp->coloffset = coffset;
    res = eval_test(cp, e, errp);
    cp->rowoffset = save_rowoffset;
    cp->coloffset = save_coloffset;
    return res;
}

static scvalue_t eval_error_type(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    if (res.type == SC_ERROR) return scvalue_number(res.u.error);
    scvalue_free(res);
    return scvalue_error(ERROR_VALUE);
}

static scvalue_t eval_isformula(eval_ctx_t *cp, enode_t *e) {
    int t = FALSE;
    scvalue_t res = eval_node(cp, e->e.args[0]);
    if (res.type == SC_RANGE) {
        /* reduce dimensions by intersecting with cell row and column */
        int row = res.u.rr.left.row;
        int col = res.u.rr.left.col;
        if ((row == res.u.rr.right.row || ((row = cp->gmyrow) >= res.u.rr.left.row && row <= res.u.rr.right.row))
        &&  (col == res.u.rr.right.col || ((col = cp->gmycol) >= res.u.rr.left.col && col <= res.u.rr.right.col))) {
            struct ent *p = lookat_nc(row, col);
            t = (p && p->expr);
        }
    }
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_formula(eval_ctx_t *cp, enode_t *e) {
    char buff[FBUFLEN];
    scvalue_t res = eval_node(cp, e->e.args[0]);
    if (res.type == SC_RANGE) {
        /* reduce dimensions by intersecting with cell row and column */
        int row = res.u.rr.left.row;
        int col = res.u.rr.left.col;
        if ((row == res.u.rr.right.row || ((row = cp->gmyrow) >= res.u.rr.left.row && row <= res.u.rr.right.row))
        &&  (col == res.u.rr.right.col || ((col = cp->gmycol) >= res.u.rr.left.col && col <= res.u.rr.right.col))) {
            struct ent *p = lookat_nc(row, col);
            if (p && p->expr) {
                decompile(buff, sizeof buff, p->expr, 0, 0, DCP_DEFAULT);
                return scvalue_string(new_string(buff));
            }
        }
    }
    scvalue_free(res);
    return scvalue_error(ERROR_VALUE);
}

static scvalue_t eval_iseven_odd(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    double v = eval_num(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    return scvalue_bool(((long)math_trunc(v) & 1) == (e->op == OP_ISODD));
}

static scvalue_t eval_isblank(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    int t = (res.type == SC_EMPTY);
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_iserr(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    int t = FALSE;
    if (res.type == SC_ERROR) {
        if (e->op == OP_ISERR) t = (res.u.error != ERROR_NA);
        else if (e->op == OP_ISNA) t = (res.u.error == ERROR_NA);
        else t = TRUE;
    }
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_islogical(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    // XXX: iserr should check that the error is not #N/A
    int t = (res.type == SC_BOOLEAN);
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_isnontext(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    int t = (res.type != SC_STRING);
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_isnumber(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    int t = (res.type == SC_NUMBER);
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_isref(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e->e.args[0]);
    int t = (res.type == SC_RANGE);
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_istext(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    int t = (res.type == SC_STRING);
    scvalue_free(res);
    return scvalue_bool(t);
}

static scvalue_t eval_row_col(eval_ctx_t *cp, enode_t *e) {
    if (e->nargs > 0) {
        scvalue_t res = eval_node(cp, e->e.args[0]);
        if (res.type == SC_RANGE) {
            return scvalue_number(e->op == OP_ROW ? res.u.rr.left.row : res.u.rr.left.col);
        } else {
            scvalue_free(res);
            return scvalue_error(ERROR_VALUE);
        }
    } else {
        return scvalue_number(e->op == OP_ROW ? cp->gmyrow + cp->rowoffset : cp->gmycol + cp->coloffset);
    }
}

static scvalue_t eval_rows_cols(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node(cp, e->e.args[0]);
    if (res.type == SC_RANGE) {
        return scvalue_number((e->op == OP_ROWS ?
                               res.u.rr.right.row - res.u.rr.left.row :
                               res.u.rr.right.col - res.u.rr.left.col));
    } else {
        scvalue_free(res);
        return scvalue_error(ERROR_VALUE);
    }
}

static scvalue_t eval_type(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    int type;
    switch (res.type) {
    case SC_EMPTY:      type = 0; break;
    case SC_NUMBER:     type = 1; break;
    case SC_STRING:     type = 2; break;
    case SC_BOOLEAN:    type = 4; break;
    default:
    case SC_ERROR:      type = 16; break;
    }
    return scvalue_number(type);
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
                                struct agregatedata_t *ap, int allvalues)
{
    int i;

    for (i = 0; i < ep->nargs; i++) {
        scvalue_t res = eval_node(cp, ep->e.args[i]);
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
                            case SC_STRING:
                            case SC_ERROR:   if (allvalues) fun(ap, 0); break;
                            }
                        }
                    }
                }
                break;
            }
        case SC_NUMBER:     fun(ap, res.u.v); break;
        case SC_BOOLEAN:    if (allvalues) fun(ap, res.u.v); break;
        case SC_STRING:     free_string(res.u.str); FALLTHROUGH;
        case SC_ERROR:      if (allvalues) fun(ap, 0); break;
        }
    }
    return retfun(cp, ap);
}

static scvalue_t eval_aggregateif(eval_ctx_t *cp, enode_t *ep,
                                  void (*fun)(struct agregatedata_t *sp, double v),
                                  scvalue_t (*retfun)(eval_ctx_t *cp, struct agregatedata_t *sp),
                                  struct agregatedata_t *ap, int ifs)
{
    scvalue_t res = eval_node(cp, ep->e.args[0]);
    enode_t *test = ep->e.args[1];
    int r, c, err = 0;
    struct ent *p;

    if (res.type != SC_RANGE) {
        scvalue_free(res);
        return scvalue_error(ERROR_VALUE);
    }
    // XXX: should implement IFS
    // XXX: should implement simple comparisons as a string
    for (r = res.u.rr.left.row; r <= res.u.rr.right.row; r++) {
        for (c = res.u.rr.left.col; c <= res.u.rr.right.col; c++) {
            // XXX: error propagation?
            if (test && !eval_test_offset(cp, test, r - res.u.rr.left.row, c - res.u.rr.left.col, &err))
                continue;
            if ((p = *ATBL(tbl, r, c)) && p->type == SC_NUMBER) {
                fun(ap, p->v);
            }
        }
    }
    return retfun(cp, ap);
}

static scvalue_t eval_countblank(eval_ctx_t *cp, enode_t *ep) {
    sclong_t count = 0;
    int i;

    for (i = 0; i < ep->nargs; i++) {
        scvalue_t res = eval_node(cp, ep->e.args[i]);
        switch (res.type) {
        case SC_RANGE: {
                int r, c;
                struct ent *p;
                for (r = res.u.rr.left.row; r <= res.u.rr.right.row; r++) {
                    for (c = res.u.rr.left.col; c <= res.u.rr.right.col; c++) {
                        if (!(p = *ATBL(tbl, r, c)) || p->type == SC_EMPTY)
                            count++;
                    }
                }
                break;
            }
        case SC_EMPTY:      count++; break;
        case SC_STRING:     free_string(res.u.str); break;
        }
    }
    return scvalue_number(count);
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
    return eval_aggregateif(cp, ep, aggregate_sum, aggregate_average_ret, &pack, ep->op == OP_AVERAGEIFS);
}

static scvalue_t eval_countif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_count, aggregate_count_ret, &pack, ep->op == OP_COUNTIFS);
}

static scvalue_t eval_maxif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_max, aggregate_v_ret, &pack, ep->op == OP_MAXIFS);
}

static scvalue_t eval_minif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_min, aggregate_v_ret, &pack, ep->op == OP_MINIFS);
}

static scvalue_t eval_sumif(eval_ctx_t *cp, enode_t *ep) {
    struct agregatedata_t pack = { 0, 0, 0 };
    return eval_aggregateif(cp, ep, aggregate_sum, aggregate_v_ret, &pack, ep->op == OP_SUMIFS);
}

/*---------------- date and time functions ----------------*/

// XXX: should accept 6 or 7 arguments
// XXX: should use integral part for day and fraction for time.
//      which may be incorrect for DST time adjustments.
static scvalue_t eval_date(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int yr = (int)eval_num(cp, e->e.args[0], &err);
    int mo = (int)eval_num(cp, e->e.args[1], &err);
    int day = (int)eval_num(cp, e->e.args[2], &err);
    time_t secs;
    struct tm t;

    if (err) return scvalue_error(err);

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
        error("@date: invalid argument or date out of range");
        return scvalue_error(ERROR_NUM);
    }
    return scvalue_number(secs);
}

static double time3(double hr, double min, double sec) {
    int seconds = ((int)floor(sec) + (int)floor(min) * 60 + (int)floor(hr) * 3600) % 86400;
    return seconds < 0 ? 86400 + seconds : seconds;
}

static scvalue_t eval_now(eval_ctx_t *cp, enode_t *e) {
    // XXX: should use a more precise time value
    // XXX: this is completely incorrect. Should return composite time
    return scvalue_number((double)time(NULL));
}

static scvalue_t eval_tc(eval_ctx_t *cp, enode_t *e) {
    static time_t t_cache;
    static struct tm tm_cache;
    struct tm *tp = &tm_cache;
    int which = e->op;
    int err = 0;
    time_t tloc = (time_t)eval_num(cp, e->e.args[0], &err);

    if (err) return scvalue_error(err);

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
    return scvalue_error(ERROR_NUM);
}

static scvalue_t eval_ston(eval_ctx_t *cp, enode_t *e) {
    scvalue_t a = eval_node_value(cp, e->e.args[0]);
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

static scvalue_t eval_exact(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *s1 = eval_str(cp, e->e.args[0], &err);
    SCXMEM string_t *s2 = eval_str(cp, e->e.args[1], &err);
    int res = -1;

    if (!err)
        res = !strcmp(s2c(s1), s2c(s2));

    free_string(s1);
    free_string(s2);

    return err ? scvalue_error(err) : scvalue_number(res);
}

/*
 * Given a string representing a column name and a value which is a row
 * number, return a pointer to the selected cell's entry, if any, else NULL.
 * Use only the integer part of the column number.  Always free the string.
 */

static scvalue_t eval_getent(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *colstr = eval_str(cp, e->e.args[0], &err);
    double rowdoub = eval_num(cp, e->e.args[1], &err);
    if (!err) {
        int row = (int)floor(rowdoub);
        int col = atocol(s2c(colstr), NULL);
        free_string(colstr);
        // XXX: should return a reference?
        if (row >= 0 && col >= 0)
            return scvalue_getcell(cp, row, col);
        err = ERROR_REF;
    }
    return scvalue_error(err);
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

    return res;
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

static scvalue_t eval_fn1(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    int err = 0;
    double v = eval_num(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    errno = 0;
    v = ((double (*)(double))fun)(v);
    if (errno) return scvalue_error(ERROR_NUM);
    return scvalue_number(v);
}

static scvalue_t eval_fn2(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    int err = 0;
    double a0 = eval_num(cp, e->e.args[0], &err);
    double a1 = eval_num(cp, e->e.args[1], &err);
    double res;
    if (err) return scvalue_error(err);
    errno = 0;
    res = ((double (*)(double, double))fun)(a0, a1);
    if (errno) return scvalue_error(ERROR_NUM);
    return scvalue_number(res);
}

static scvalue_t eval_fn3(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    int err = 0;
    double a0 = eval_num(cp, e->e.args[0], &err);
    double a1 = eval_num(cp, e->e.args[1], &err);
    double a2 = eval_num(cp, e->e.args[2], &err);
    double res;
    if (err) return scvalue_error(err);
    errno = 0;
    res = ((double (*)(double, double, double))fun)(a0, a1, a2);
    if (errno) return scvalue_error(ERROR_NUM);
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

#if 0
static scvalue_t eval_fl1(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    int err = 0;
    sclong_t v = eval_long(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    errno = 0;
    v = ((sclong_t (*)(sclong_t))fun)(v);
    if (errno) return scvalue_error(ERROR_NUM);
    return scvalue_number(v);
}
#endif

static scvalue_t eval_fl2(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    int err = 0;
    sclong_t a0 = eval_long(cp, e->e.args[0], &err);
    sclong_t a1 = eval_long(cp, e->e.args[1], &err);
    sclong_t res;
    if (err) return scvalue_error(err);
    errno = 0;
    res = ((sclong_t (*)(sclong_t, sclong_t))fun)(a0, a1);
    if (errno) return scvalue_error(ERROR_NUM);
    return scvalue_number(res);
}

#if 0
static scvalue_t eval_fl3(eval_ctx_t *cp, enode_t *e) {
    scarg_t fun = opdefs[e->op].arg;
    int err = 0;
    sclong_t a0 = eval_long(cp, e->e.args[0], &err);
    sclong_t a1 = eval_long(cp, e->e.args[1], &err);
    sclong_t a2 = eval_long(cp, e->e.args[2], &err);
    if (err) return scvalue_error(err);
    sclong_t res;
    errno = 0;
    res = ((sclong_t (*)(sclong_t, sclong_t, sclong_t))fun)(a0, a1, a2);
    if (errno) return scvalue_error(ERROR_NUM);
    return scvalue_number(res);
}
#endif

static sclong_t bitand(sclong_t a, sclong_t b) {
    return a & b;
}

static sclong_t bitlshift(sclong_t a, sclong_t b) {
    return b < 0 ? a >> b : a << b;
}

static sclong_t bitor(sclong_t a, sclong_t b) {
    return a | b;
}

static sclong_t bitrshift(sclong_t a, sclong_t b) {
    return b < 0 ? a << b : a >> b;
}

static sclong_t bitxor(sclong_t a, sclong_t b) {
    return a ^ b;
}

static sclong_t makecolor(sclong_t a, sclong_t b) {
    return (a & 7) + ((b & 7) << 3);
}

/*---------------- formating functions ----------------*/

/*
 * Rules for string functions:
 * Take string arguments which they scxfree.
 * All returned strings are assumed to be xalloced.
 */

static scvalue_t eval_datefmt(eval_ctx_t *cp, enode_t *e) {
    char buff[FBUFLEN];
    int err = 0;
    time_t tloc = (time_t)eval_num(cp, e->e.args[0], &err);
    SCXMEM string_t *fmtstr = (e->nargs > 1) ? eval_str(cp, e->e.args[1], &err) : NULL;
    const char *fmt = fmtstr ? s2c(fmtstr) : "%a %b %d %H:%M:%S %Y";
    struct tm *tp = localtime(&tloc);
    if (!err && !tp) {
        err = ERROR_NUM;
    }
    if (!err) {
        // XXX: should check format string
        ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
        (buff, sizeof buff, fmt, tp);
        free_string(fmtstr);
        return scvalue_string(new_string(buff));
    } else {
        free_string(fmtstr);
        return scvalue_error(err);
    }
}

static scvalue_t eval_fmt(eval_ctx_t *cp, enode_t *e) {
    char buff[FBUFLEN];
    int err = 0;
    SCXMEM string_t *fmtstr = eval_str(cp, e->e.args[0], &err);
    double v = eval_num(cp, e->e.args[1], &err);

    if (err) return scvalue_error(err);

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

static scvalue_t eval_ext(eval_ctx_t *cp, enode_t *e) {
    int err = 0;

#ifdef NOEXTFUNCS
    error("Warning: External functions unavailable");
    err = ERROR_NA;      /* not sure if this should be an error */
#else
    for (;;) {
        char buff[FBUFLEN];
        buf_t buf;
        enode_t *left = e->e.args[0];
        enode_t *cmd = left;
        enode_t *prev = NULL;
        SCXMEM string_t *str;
        int len, i;
        FILE *pf;

        if (left && left->op == OP_DUMMY) {
            prev = left->e.args[0];
            cmd = left->e.args[1];
        }

        if (!extfunc) {
            // XXX: should probably be N/A if no previous value
            error("Warning: external functions disabled; using %s value",
                  prev ? "previous" : "null");
            if (prev) return eval_node(cp, prev);
            return scvalue_string(dup_string(empty_string));
        }
        str = eval_str(cp, cmd, &err);
        if (err) break;
        buf_init(buf, buff, sizeof buff);
        len = buf_puts(buf, s2c(str));
        free_string(str);
        if (len == 0) {
            error("Warning: external function given null command name");
            err = ERROR_VALUE;
            break;
        }
        for (i = 1; i < e->nargs; i++) {
            str = eval_str(cp, e->e.args[i], &err);
            if (err) break;
            buf_printf(buf, " %s", s2c(str));
            free_string(str);
        }
        if (err) break;

        error("Running external function...");
        refresh();

        if ((pf = popen(buff, "r")) == NULL) {     /* run it */
            error("Warning: running \"%s\" failed", buff);
            err = ERROR_NA;
            break;
        }
        if (fgets(buff, sizeof buff, pf) == NULL) {  /* one line */
            // XXX: should use the empty string?
            error("Warning: external function returned nothing");
            *buff = '\0';
        } else {
            strtrim(buff);
            error(" "); /* erase notice */
        }
        pclose(pf);
        str = new_string(buff);
        if (!str) {
            err = ERROR_NULL;
            break;
        }
        if (cmd == left) {
            prev = new_str(dup_string(str));
            /* create dummy node to store previous value */
            // XXX: potential memory leak on malloc failure
            e->e.args[0] = left = new_op2(OP_DUMMY, prev, cmd);
        } else
        if (prev && prev->op == OP__STRING) {
            /* set updated previous value in dummy node */
            set_string(&prev->e.s, dup_string(str));
        }
        return scvalue_string(str);
    }
#endif  /* NOEXTFUNCS */
    return scvalue_error(err);
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

    return res; /* type is SC_ERROR */
}

/*---------------- string functions ----------------*/

// XXX: should handle UTF-8 encoded UNICODE stuff
static scvalue_t eval_case(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *s = eval_str(cp, e->e.args[0], &err);
    SCXMEM string_t *s2;
    char *p;

    if (err) scvalue_error(err);

    if (sempty(s))
        return scvalue_string(s);

    s2 = new_string_len(s2c(s), slen(s));
    free_string(s);
    if (!s2)
        return scvalue_error(ERROR_NULL);

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
    case OP_PROPER: {
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

static scvalue_t eval_char(eval_ctx_t *cp, enode_t *e) {
    int len, err = 0;
    int code = (int)eval_num(cp, e->e.args[0], &err);
    char buf[10];
    if (err) return scvalue_error(err);
    len = 0;
    buf[len++] = code;
    // XXX: should support UNICODE via UTF-8 encoding
    return scvalue_string(new_string_len(buf, len));
}

static scvalue_t eval_code(eval_ctx_t *cp, enode_t *e) {
    int err = 0, code;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    // XXX: should support UNICODE via UTF-8 encoding
    code = *s2c(str);
    free_string(str);
    return scvalue_number(code);
}

static scvalue_t eval_clean(eval_ctx_t *cp, enode_t *e) {
    int err = 0, i, j, len, count;
    SCXMEM string_t *text = eval_str(cp, e->e.args[0], &err);
    SCXMEM string_t *str = text;
    const char *p;
    char *q;
    if (err) return scvalue_error(err);
    p = s2c(text);
    len = slen(text);
    for (i = count = 0; i < len; i++) {
        unsigned char c = p[i];
        count += (c < 32 || c == 127);
    }
    if (count) {
        str = new_string_len(p, len - count);
        if (str) {
            q = str->s;
            for (i = j = 0; i < len; i++) {
                unsigned char c = p[i];
                if (!(c < 32 || c == 127))
                    q[j++] = c;
            }
        }
        free_string(text);
    }
    return scvalue_string(str);
}

static scvalue_t eval_len(eval_ctx_t *cp, enode_t *e) {
    int err = 0, len;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    len = slen(str);
    free_string(str);
    return scvalue_number(len);
}

static scvalue_t eval_t(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    if (res.type == SC_STRING) return res;
    return scvalue_string(dup_string(empty_string));
}

static scvalue_t eval_find(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *search = eval_str(cp, e->e.args[0], &err);
    SCXMEM string_t *t = eval_str(cp, e->e.args[1], &err);
    int start = e->nargs > 2 ? (int)eval_num(cp, e->e.args[2], &err) : 1;

    if (!err) {
        if (start < 1) {
            err = ERROR_NUM;    /* invalid argument */
        } else
        if (start <= slen(search)) {
            const char *s1 = s2c(search);
            const char *p = (e->op == OP_SEARCH || e->op == OP_SEARCHB) ?
                str_case_str(s1 + start - 1, s2c(t)) : strstr(s1 + start - 1, s2c(t));
            if (p != NULL) {
                start = p - s1 + 1;
            } else {
                err = ERROR_NA;
            }
        } else {
            err = ERROR_NA;
        }
    }
    free_string(search);
    free_string(t);
    if (err)
        return scvalue_error(err);
    else
        return scvalue_number(start);
}

static scvalue_t eval_substitute(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *text = eval_str(cp, e->e.args[0], &err);
    SCXMEM string_t *oldtext = eval_str(cp, e->e.args[1], &err);
    SCXMEM string_t *newtext = eval_str(cp, e->e.args[2], &err);
    SCXMEM string_t *str = NULL;
    int which = 0, len, oldlen, newlen, count, len2;
    const char *s, *s0;

    for (;;) {
        if (err)
            break;
        if (e->nargs > 3) {
            which = (int)eval_num(cp, e->e.args[3], &err);
            if (err)
                break;
            if (which < 1) {
                err = ERROR_NUM;
                break;
            }
        }
        str = dup_string(text);
        if (!slen(oldtext))
            break;
        s = s2c(text);
        len = slen(text);
        oldlen = slen(oldtext);
        newlen = slen(newtext);
        count = 0;
        while ((s = strstr(s, s2c(oldtext))) != NULL) {
            count++;
            if (count == which)
                break;
            s += oldlen;
        }
        if (which)
            count = (count >= which);

        if (count == 0)
            break;

        len2 = len + count * (newlen - oldlen);
        free_string(str);
        // XXX: handle max string len
        if ((str = new_string_len(NULL, len2))) {
            char *p = str->s;
            s = s0 = s2c(text);
            while ((s = strstr(s, s2c(oldtext))) != NULL) {
                if (++count < which)
                    continue;
                memcpy(p, s0, s - s0);
                p += s - s0;
                memcpy(p, s2c(newtext), newlen);
                p += newlen;
                s0 = s += oldlen;
                if (which)
                    break;
            }
            strcpy(p, s0);
        }
        break;
    }
    free_string(text);
    free_string(oldtext);
    free_string(newtext);
    if (err)
        return scvalue_error(err);
    else
        return scvalue_string(str);
}

static scvalue_t eval_replace(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *text = eval_str(cp, e->e.args[0], &err);
    int start = (int)eval_num(cp, e->e.args[1], &err) - 1;
    int count = (int)eval_num(cp, e->e.args[2], &err);
    SCXMEM string_t *newtext = eval_str(cp, e->e.args[3], &err);
    SCXMEM string_t *str = dup_string(text);

    if (!err) {
        if (start >= 0 && count >= 0) {
            int len = slen(text);
            int len2 = slen(newtext);
            if (start > len) start = len;
            if (count > len - start) count = len - start;
            free_string(str);
            if ((str = new_string_len(NULL, len - count + len2))) {
                char *p = str->s;
                memcpy(p, s2c(text), start);
                memcpy(p + start, s2c(newtext), len2);
                memcpy(p + start + len2, s2c(text) + start + count,
                       len - start - count);
            }
        } else {
            err = ERROR_NUM;
        }
    }
    free_string(text);
    free_string(newtext);
    if (err)
        return scvalue_error(err);
    else
        return scvalue_string(str);
}

static scvalue_t eval_rept(eval_ctx_t *cp, enode_t *e) {
    int len, len2, err = 0;
    SCXMEM string_t *text = eval_str(cp, e->e.args[0], &err);
    int count = (int)eval_num(cp, e->e.args[1], &err);
    SCXMEM string_t *str;

    if (err || count < 0) {
        free_string(text);
        return scvalue_error(err ? err : ERROR_NUM);
    }
    len = slen(text);
    if (count != 0 && FBUFLEN / count < len)
        count = FBUFLEN / len;
    if ((len2 = len * count) == 0) {
        str = dup_string(empty_string);
    } else
    if ((str = new_string_len(NULL, len2))) {
        char *p = str->s;
        for (; count --> 0; p += len)
            memcpy(p, s2c(text), len);
    }
    free_string(text);
    return scvalue_string(str);
}

static scvalue_t eval_left(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    int n = (int)eval_num(cp, e->e.args[1], &err);
    if (err) {
        free_string(str);
        return scvalue_error(err);
    }
    return scvalue_string(sub_string(str, 0, n));
}

static scvalue_t eval_right(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    int n = (int)eval_num(cp, e->e.args[1], &err);
    if (err) {
        free_string(str);
        return scvalue_error(err);
    }
    return scvalue_string(sub_string(str, slen(str) - n, n));
}

static scvalue_t eval_mid(eval_ctx_t *cp, enode_t *e) {
    /* OP_MID: v1 is one-based character offset, v2 is a number of characters */
    /* OP_MIDB: v1 is one-based byte offset, v2 is a number of bytes */
    /* OP_SUBSTR: v1 and v2 are one-based character offsets, v2 is included */
    int err = 0;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    int v1 = (int)eval_num(cp, e->e.args[1], &err);
    int v2 = (int)eval_num(cp, e->e.args[2], &err);
    if (err) {
        free_string(str);
        return scvalue_error(err);
    }
    return scvalue_string(sub_string(str, v1 - 1, e->op == OP_SUBSTR ? v2 - v1 + 1 : v2));
}

static scvalue_t eval_trim(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    return scvalue_string(trim_string(str));
}

static scvalue_t eval_concat(eval_ctx_t *cp, enode_t *e) {
    int i, err = 0;
    SCXMEM string_t *str = NULL;
    for (i = 0; i < e->nargs; i++) {
        SCXMEM string_t *str2 = eval_str(cp, e->e.args[0], &err);
        if (err) {
            free_string(str);
            return scvalue_error(err);
        }
        str = cat_strings(str, str2);
        if (!str) return scvalue_error(ERROR_NUM);
    }
    return scvalue_string(str);
}

static scvalue_t eval_filename(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int n = eval_test(cp, e->e.args[0], &err);
    if (err) scvalue_error(err);
    return scvalue_string(new_string(n ? curfile : get_basename(curfile)));
}

static scvalue_t eval_coltoa(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int col = (int)eval_num(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    return scvalue_string(new_string(coltoa(col)));
}

static scvalue_t eval_not(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int res = !eval_test(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    return scvalue_bool(res);
}

static scvalue_t eval_and(eval_ctx_t *cp, enode_t *e) {
    int err = 0, i, res = TRUE;
    for (i = 0; res && i < e->nargs; i++) {
        res &= eval_test(cp, e->e.args[i], &err);
        if (err) return scvalue_error(err);
    }
    return scvalue_bool(res);
}

static scvalue_t eval_or(eval_ctx_t *cp, enode_t *e) {
    int err = 0, i, res = FALSE;
    for (i = 0; !res && i < e->nargs; i++) {
        res |= eval_test(cp, e->e.args[i], &err);
        if (err) return scvalue_error(err);
    }
    return scvalue_bool(res);
}

static scvalue_t eval_xor(eval_ctx_t *cp, enode_t *e) {
    int err = 0, i, res = FALSE;
    for (i = 0; i < e->nargs; i++) {
        res ^= eval_test(cp, e->e.args[i], &err);
        if (err) return scvalue_error(err);
    }
    return scvalue_bool(res);
}

static scvalue_t eval_if(eval_ctx_t *cp, enode_t *e) {
    int err = 0;
    int t = eval_test(cp, e->e.args[0], &err);
    if (err) return scvalue_error(err);
    if (e->nargs > 1) {
        if (t) return eval_node(cp, e->e.args[1]);
        if (e->nargs > 2) return eval_node(cp, e->e.args[2]);
    }
    return scvalue_bool(t);
}

static scvalue_t eval_iferror(eval_ctx_t *cp, enode_t *e) {
    scvalue_t res = eval_node_value(cp, e->e.args[0]);
    if (res.type == SC_ERROR) {
        if (e->op == OP_IFERROR || res.u.error == ERROR_NA) {
            if (e->nargs > 1)
                return eval_node(cp, e->e.args[1]);
            else
                return scvalue_empty();
        }
    }
    return res;
}

static int is_relative(int op) {
    return op == OP_LT || op == OP_GT || op == OP_LE || op == OP_GE;
}

static scvalue_t eval_cmp(eval_ctx_t *cp, enode_t *e) {
    int op = e->op;
    scvalue_t a = eval_node_value(cp, e->e.args[0]);
    scvalue_t b = eval_node_value(cp, e->e.args[1]);
    int cmp = 0;
    // XXX: mixed types should compare in this order:
    //  number < string < logical < error < empty
    // XXX: should stop error propagation
    if (a.type == SC_ERROR || b.type == SC_ERROR) {
        if (op == OP_EQ)
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
    case OP_EQ:     return scvalue_bool(cmp == 0);
    case OP_LG:
    case OP_NE:     return scvalue_bool(cmp != 0);
    case OP_GT:     return scvalue_bool(cmp >  0);
    case OP_GE:     return scvalue_bool(cmp >= 0);
    }
    return scvalue_error(ERROR_NULL);
}

static scvalue_t eval_to_base(eval_ctx_t *cp, enode_t *e, int err, int from_base, int base, int argdig) {
    char buf[300];
    char *p = buf + sizeof(buf);
    long n0;
    unsigned long n;
    int mindigits = 0;
    SCXMEM string_t *str = NULL;
    const char *s;
    char *endp;

    for (;;) {
        if (err) break;

        if (from_base < 2 || from_base > 36 || base < 2 || base > 36) {
            err = ERROR_NUM;
            break;
        }

        if (from_base == 10) {
            n0 = eval_num(cp, e->e.args[0], &err);
            if (err) break;
        } else {
            str = eval_str(cp, e->e.args[0], &err);
            if (err) break;

            errno = 0;
            n0 = strtol(s = s2c(str), &endp, from_base);
            if (errno || endp == s) {
                err = ERROR_VALUE;
                break;
            }
            if (base == 2 && toupperchar(*endp) == 'B')
                endp++;
            if (base == 16 && toupperchar(*endp) == 'H')
                endp++;
            if (*endp != '\0') {
                err = ERROR_VALUE;
                break;
            }
        }
        if (e->nargs > argdig) {
            mindigits = (int)eval_num(cp, e->e.args[argdig], &err);
            if (err) break;
        }
        n = n0 < 0 ? 0UL - n0 : 0UL + n0;
        do {
            unsigned int d = n % base;
            n /= base;
            /* Assuming ASCII where all letters are consecutive */
            *--p = d + (d < 10 ? '0' : 'A' - 10);
        } while (n > 0);

        while (p > buf + 1 && mindigits > buf + sizeof(buf) - p)
            *--p = '0';
        if (n0 < 0)
            *--p = '-';
        free_string(str);
        return scvalue_string(new_string_len(p, buf + sizeof(buf) - p));
    }
    free_string(str);
    return scvalue_error(err);
}

static scvalue_t eval_base(eval_ctx_t *cp, enode_t *e) {
    int err = 0, from_base = (int)eval_num(cp, e->e.args[1], &err);
    return eval_to_base(cp, e, err, 10, from_base, 2);
}

static scvalue_t eval_decimal(eval_ctx_t *cp, enode_t *e) {
    int err = 0, base = (int)eval_num(cp, e->e.args[1], &err);
    return eval_to_base(cp, e, err, base, 10, 2);
}

static scvalue_t eval_bin2dec(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 2, 10, 1);
}

static scvalue_t eval_bin2hex(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 2, 16, 1);
}

static scvalue_t eval_bin2oct(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 2, 8, 1);
}

static scvalue_t eval_dec2bin(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 10, 2, 1);
}

static scvalue_t eval_dec2hex(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 10, 16, 1);
}

static scvalue_t eval_dec2oct(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 10, 8, 1);
}

static scvalue_t eval_hex2bin(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 16, 2, 1);
}

static scvalue_t eval_hex2dec(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 16, 10, 1);
}

static scvalue_t eval_hex2oct(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 16, 8, 1);
}

static scvalue_t eval_oct2bin(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 8, 2, 1);
}

static scvalue_t eval_oct2dec(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 8, 10, 1);
}

static scvalue_t eval_oct2hex(eval_ctx_t *cp, enode_t *e) {
    return eval_to_base(cp, e, 0, 8, 16, 1);
}

static int roman_value(char c) {
    switch (toupperchar(c)) {
    case '\0':  return 0;
    case 'I':   return 1;
    case 'V':   return 5;
    case 'X':   return 10;
    case 'L':   return 50;
    case 'C':   return 100;
    case 'D':   return 500;
    case 'M':   return 1000;
    default:    return -1;
    }
}

static scvalue_t eval_arabic(eval_ctx_t *cp, enode_t *e) {
    int err = 0, v, n = 0;
    SCXMEM string_t *str = eval_str(cp, e->e.args[0], &err);

    if (!err) {
        const char *s = s2c(str);
        while (*s) {
            v = roman_value(*s++);
            if (v < 0) {
                err = ERROR_VALUE;
                break;
            }
            n += (v < roman_value(*s)) ? -v : v;
        }
    }
    free_string(str);
    return err ? scvalue_error(err) : scvalue_number(n);
}

static scvalue_t eval_roman(eval_ctx_t *cp, enode_t *e) {
    char buf[16];
    char *q = buf + countof(buf);
    char const digits[] = "IVXLCDM";
    const char *p = digits;
    int err = 0, n = (int)eval_num(cp, e->e.args[0], &err);

    if (err) return scvalue_error(err);
    if (n <= 0 || n >= 4000) {
        return scvalue_error(ERROR_NUM);
    }

    // XXX: incorrect algorithms, should support 2nd argument
    while (n > 0) {
#if 1
        /* Potentially shorter code :-) */
#define R(a,b,c,d)  (((a)<<6)|((b)<<4)|((c)<<2)|((d)<<0))
        int n10;
        unsigned char const pat[10] = { /* 0, 1, 5, 21, 6, 2, 9, 37, 85, 7 */
            R(0,0,0,0), R(0,0,0,1), R(0,0,1,1), R(0,1,1,1), R(0,0,1,2),
            R(0,0,0,2), R(0,0,2,1), R(0,2,1,1), R(2,1,1,1), R(0,0,1,3),
        };
        for (n10 = pat[n % 10]; n10; n10 >>= 2) {
            *--q = p[(n10 & 3) - 1];
        }
#else
        int n10 = n % 10;
        int n1 = n10 % 5;
        if (n1 == 4) {
            *--q = p[1 + (n10 == 9)];
            *--q = p[0];
        } else {
            while (n1--)
                *--q = p[0];
            if (n10 >= 5)
                *--q = p[1];
        }
#endif
        n /= 10;
        p += 2;
    }
    return scvalue_string(new_string_len(q, buf + sizeof(buf) - q));
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
    case OP_NA:         return scvalue_error(ERROR_NA);
    case OP_FALSE:      return scvalue_bool(0); break;
    case OP_TRUE:       return scvalue_bool(1); break;
    case OP_UPLUS:      return eval_node_value(cp, e->e.args[0]);
    case OP_ERR:
    case OP_ERRNUM:     return scvalue_error(ERROR_NUM);
    case OP_ERRREF:     return scvalue_error(ERROR_REF);
    default:            error("Illegal expression");
                        return scvalue_error(ERROR_NULL);
    }
    return scvalue_number(val);
}

/*---------------- dynamic evaluator ----------------*/

/* opcode definitions, used for evaluator and decompiler */
// XXX: should use for parser and constant_node() too.
struct opdef const opdefs[] = {
#define OP(op,min,max,efun,arg,str,desc)  { str, min, max, 0, 0, efun, (scarg_t)(arg) },
#include "opcodes.h"
#undef OP
};

scvalue_t eval_node(eval_ctx_t *cp, enode_t *e) {
    if (e == NULL)
        return scvalue_empty();

    if (e->op < OP_count) {
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
        &&  (col == res.u.rr.right.col || ((col = cp->gmycol) >= res.u.rr.left.col && col <= res.u.rr.right.col))) {
            return scvalue_getcell(cp, row, col);
        }
        return scvalue_error(ERROR_REF);
    } else {
        return res;
    }
}

// XXX: unused?
scvalue_t eval_at(enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0 }};
    return eval_node_value(cp, e);
}

double neval_at(enode_t *e, int row, int col, int *errp) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0 }};
    return eval_num(cp, e, errp);
}

SCXMEM string_t *seval_at(enode_t *e, int row, int col, int *errp) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0 }};
    return eval_str(cp, e, errp);
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
    int lastcnt, pair, v, err = 0;

    signal(SIGFPE, eval_fpe);

    for (repct = 1; (lastcnt = RealEvalAll()) && repct < propagation; repct++)
        continue;

    if (propagation > 1 && lastcnt > 0)
        error("Still changing after %d iterations", repct);

    if (usecurses && color && has_colors()) {
        for (pair = 1; pair <= CPAIRS; pair++) {
            if (cpairs[pair] && cpairs[pair]->expr) {
                v = (int)neval_at(cpairs[pair]->expr, 0, 0, &err);
                if (!err) {
                    /* ignore value if expression error */
                    init_style(pair, v & 7, (v >> 3) & 7, cpairs[pair]->expr);
                }
            }
            /* Can't see to fix the problem if color 1 has an error, so
             * turn off color in that case.
             */
            if (pair == 1 && err) {
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
                    chgct += RealEvalOne(p, p->expr, i, j);
            }
        }
    } else
    if (calc_order == BYCOLS) {
        for (j = 0; j <= maxcol; j++) {
            for (i = 0; i <= maxrow; i++) {
                if ((p = *ATBL(tbl,i,j)) && p->expr)
                    chgct += RealEvalOne(p, p->expr, i, j);
            }
        }
    } else {
        // XXX: Should implement topological sort
        error("Internal error calc_order");
    }
    return chgct;
}

static int RealEvalOne(struct ent *p, enode_t *e, int row, int col) {
    eval_ctx_t cp[1] = {{ row, col, 0, 0 }};
    scvalue_t res;

    if (setjmp(fpe_save)) {
        error("Floating point exception at %s", v_name(row, col));
        res = scvalue_error(ERROR_NUM);
    } else {
        res = eval_node_value(cp, e);
    }
    if (res.type == SC_NUMBER && !isfinite(res.u.v)) {
        res = scvalue_error(ERROR_NUM);
    }
    if (p->type == res.type) {
        if (res.type == SC_STRING) {
            if (!strcmp(s2c(res.u.str), s2c(p->label))) {
                free_string(res.u.str);
                return 0;
            }
        } else
        if (res.type == SC_NUMBER || res.type == SC_BOOLEAN) {
            if (res.u.v == p->v)
                return 0;
        } else
        if (res.type == SC_ERROR) {
            if (res.u.error == p->cellerror)
                return 0;
        } else {
            /* res.type is SC_EMPTY */
            return 0;
        }
    }
    // XXX: cell value changes, should store undo record?
    if (p->type == SC_STRING) {
        set_string(&p->label, NULL); /* free the previous label */
    }
    p->type = res.type;
    p->cellerror = 0;
    p->flags |= IS_CHANGED;
    p->v = 0;
    changed++;
    if (res.type == SC_STRING) {
        set_string(&p->label, res.u.str);
    } else
    if (res.type == SC_NUMBER || res.type == SC_BOOLEAN) {
        p->v = res.u.v;
    } else
    if (res.type == SC_ERROR) {
        p->cellerror = res.u.error;
    }
    return 1;
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

static SCXMEM enode_t *new_node(int op, int nargs) {
    SCXMEM enode_t *p;
    size_t size = offsetof(enode_t, e) + sizeof(p->e.args) * nargs;
    int i;

    p = scxmalloc(size > sizeof(enode_t) ? size : sizeof(enode_t));
    if (p) {
        p->op = op;
        p->type = OP_TYPE_FUNC;
        p->nargs = nargs;
        for (i = 0; i < nargs; i++) {
            p->e.args[i] = NULL;
        }
    }
    return p;
}

SCXMEM enode_t *new_op0(int op) {
    return new_node(op, 0);
}

SCXMEM enode_t *new_op1(int op, SCXMEM enode_t *a1) {
    SCXMEM enode_t *e = new_node(op, 1);
    if (!e || !a1) {
        efree(a1);
        scxfree(e);
        return NULL;
    }
    e->e.args[0] = a1;
    return e;
}

SCXMEM enode_t *new_op2(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2) {
    SCXMEM enode_t *e = new_node(op, 2);
    if (!e || !a1 || !a2) {
        efree(a1);
        efree(a2);
        scxfree(e);
        return NULL;
    }
    e->e.args[0] = a1;
    e->e.args[1] = a2;
    return e;
}

SCXMEM enode_t *new_op1x(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2) {
    int i, j, nargs = 1;
    SCXMEM enode_t *e;
    SCXMEM enode_t *p;

    for (p = a2; p && p->op == OP_COMMA;) {
        nargs += p->nargs - 1;
        p = p->e.args[p->nargs - 1];
    }
    if (p) {
        nargs++;
    }
    e = new_node(op, nargs);
    if (!e || !a1 || !a2) {
        efree(a1);
        efree(a2);
        return NULL;
    }
    i = 0;
    e->e.args[i++] = a1;
    for (p = a2; p && p->op == OP_COMMA;) {
        for (j = 0; j < p->nargs; j++)
            e->e.args[i++] = p->e.args[j];
        scxfree(p);
        p = e->e.args[--i];
    }
    if (p) {
        e->e.args[i++] = p;
    }
    return e;
}

SCXMEM enode_t *new_op3(int op, SCXMEM enode_t *a1, SCXMEM enode_t *a2,
                        SCXMEM enode_t *a3)
{
    SCXMEM enode_t *e = new_node(op, 3);

    if (!e || !a1 || !a2 || !a3) {
        efree(a1);
        efree(a2);
        efree(a3);
        scxfree(e);
        return NULL;
    }
    e->e.args[0] = a1;
    e->e.args[1] = a2;
    e->e.args[2] = a3;
    return e;
}

SCXMEM enode_t *new_var(cellref_t cr) {
    SCXMEM enode_t *p = scxmalloc(sizeof(enode_t));
    if (p) {
        p->op = OP__VAR;
        p->type = OP_TYPE_VAR;
        p->nargs = 0;
        p->e.v.vf = cr.vf;
        p->e.v.vp = lookat(cr.row, cr.col);
    }
    return p;
}

SCXMEM enode_t *new_range(rangeref_t rr) {
    SCXMEM enode_t *p = scxmalloc(sizeof(enode_t));
    if (p) {
        p->op = OP__RANGE;
        p->type = OP_TYPE_RANGE;
        p->nargs = 0;
        p->e.r.left.vf = rr.left.vf;
        p->e.r.left.vp = lookat(rr.left.row, rr.left.col);
        p->e.r.right.vf = rr.right.vf;
        p->e.r.right.vp = lookat(rr.right.row, rr.right.col);
    }
    return p;
}

SCXMEM enode_t *new_const(double v) {
    SCXMEM enode_t *p = scxmalloc(sizeof(enode_t));
    if (p) {
        p->op = OP__NUMBER;
        p->type = OP_TYPE_DOUBLE;
        p->nargs = 0;
        p->e.k = v;
        if (!isfinite(v)) {
            p->op = OP_ERRNUM;
            p->type = OP_TYPE_FUNC;
        }
    }
    return p;
}

SCXMEM enode_t *new_str(SCXMEM string_t *s) {
    SCXMEM enode_t *p = scxmalloc(sizeof(enode_t));
    if (p) {
        p->op = OP__STRING;
        p->type = OP_TYPE_STRING;
        p->nargs = 0;
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

    if (!(ret = new_node(e->op, e->nargs)))
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
    } else
    if (e->type == OP_TYPE_FUNC) {
        int i;
        for (i = 0; i < e->nargs; i++) {
            if (!(ret->e.args[i] = copye(e->e.args[i], Rdelta, Cdelta, r1, c1, r2, c2, transpose))) {
                efree(ret);
                return NULL;
            }
        }
    }
    return ret;
}

/*
 * Say if an expression is a constant (return 1) or not.
 */
static int constant_expr(enode_t *e, int full) {
    int i;

    if (e == NULL
    ||  e->op == OP__NUMBER
    ||  e->op == OP__STRING
    ||  (e->op == OP_UMINUS && constant_expr(e->e.args[0], 0))) /* unary minus */
        return TRUE;

    if (!full
    ||  e->type != OP_TYPE_FUNC
    ||  e->op == OP_RAND     /* non pure functions */
    ||  e->op == OP_RANDBETWEEN
    ||  e->op == OP_EXT
    ||  e->op == OP_NVAL
    ||  e->op == OP_SVAL
    ||  e->op == OP_NOW
    ||  e->op == OP_MYROW
    ||  e->op == OP_MYCOL
    ||  e->op == OP_LASTROW
    ||  e->op == OP_LASTCOL
    ||  e->op == OP_NUMITER
    ||  e->op == OP_FILENAME)
        return FALSE;

    for (i = 0; i < e->nargs; i++) {
        if (!constant_expr(e->e.args[i], full))
            return FALSE;
    }
    return TRUE;
}

// XXX: all these should go to cmds.c

/* clear the value and expression of a cell */
void unlet(cellref_t cr) {
    struct ent *p = lookat_nc(cr.row, cr.col);
    if (p && p->type != SC_EMPTY) {
        // XXX: what if the cell is locked?
        set_string(&p->label, NULL);
        efree(p->expr);
        p->expr = NULL;
        p->type = SC_EMPTY;
        p->cellerror = 0;
        p->v = 0.0;
        p->flags |= IS_CHANGED;
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
        if (e->op == OP__NUMBER && !sc_decimal && prescale < 0.9999999)
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
        signal(SIGFPE, eval_fpe);
        RealEvalOne(v, e, cr.row, cr.col);
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
        if (e->type == OP_TYPE_FUNC) {
            int i;
            for (i = 0; i < e->nargs; i++)
                efree(e->e.args[i]);
        } else
        if (e->type == OP_TYPE_STRING) {
            free_string(e->e.s);
        }
        scxfree(e);
    }
}

void free_enode_list(void) {
}

/*---- expression decompiler ----*/

typedef struct decomp_t decomp_t;
struct decomp_t {
    struct buf_t *buf;
    int dr, dc, flags;
};

static void decompile_node(decomp_t *dcp, enode_t *e, int priority);

static void out_number(decomp_t *dcp, double v) {
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

static void out_string(decomp_t *dcp, const char *s) {
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

static void out_prefix(decomp_t *dcp, const char *s, enode_t *e) {
    buf_puts(dcp->buf, s);
    decompile_node(dcp, e->e.args[0], 30);
}

static void out_postfix(decomp_t *dcp, const char *s, enode_t *e) {
    decompile_node(dcp, e->e.args[0], 30);
    buf_puts(dcp->buf, s);
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
        int i, sep = '(';
        for (i = 0; i < e->nargs; i++, sep = ',') {
            buf_putc(dcp->buf, sep);
            decompile_node(dcp, e->e.args[i], 0);
        }
        buf_putc(dcp->buf, ')');
    }
}

static void out_infix(decomp_t *dcp, const char *s, enode_t *e, int priority, int mypriority) {
    if (mypriority < priority)
        buf_putc(dcp->buf, '(');
    decompile_node(dcp, e->e.args[0], mypriority);
    buf_puts(dcp->buf, s);
    // XXX: priority seems bogus
    decompile_node(dcp, e->e.args[1], mypriority + 1);
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
    case OP_DUMMY:      decompile_node(dcp, e->e.args[1], priority); break;
    case OP__NUMBER:    out_number(dcp, e->e.k);        break;
    case OP__STRING:    out_string(dcp, s2c(e->e.s));   break;
    case OP__VAR:       out_var(dcp, e->e.v, 1);        break;
    case OP__RANGE:     out_range(dcp, e);              break;
    case OP_UMINUS:
    case OP_UPLUS:      out_prefix(dcp, opp->name, e); break;
    case OP_SEMI:       out_infix(dcp, opp->name, e, priority, 1); break;
    case OP_EQ:
    case OP_NE:
    case OP_LG:
    case OP_LT:
    case OP_LE:
    case OP_GE:
    case OP_GT:         out_infix(dcp, opp->name, e, priority, 6); break;
    case OP_SHARP:
    case OP_AMPERSAND:  out_infix(dcp, opp->name, e, priority, 7); break;
    case OP_PLUS:
    case OP_MINUS:      out_infix(dcp, opp->name, e, priority, 8); break;
    case OP_STAR:
    case OP_SLASH:      out_infix(dcp, opp->name, e, priority, 10); break;
    case OP_PERCENT:    out_postfix(dcp, opp->name, e); break;
    case OP_CARET:      out_infix(dcp, opp->name, e, priority, 12); break;
    case OP_COLON:      out_infix(dcp, opp->name, e, priority, 13); break;
    default:            if (e->op < OP_count) {
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
