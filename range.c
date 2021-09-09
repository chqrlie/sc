/*      SC      A Spreadsheet Calculator
 *              Range Manipulations
 *
 *              Robert Bond, 4/87
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

static struct nrange *rng_base;
static struct nrange *rng_tail;

static void fix_enode(struct enode *e, int row1, int col1, int row2, int col2,
                      int delta1, int delta2, struct frange *fr);

int are_nranges(void) {
    return rng_base != NULL;
}

void add_nrange(SCXMEM string_t *name, rangeref_t rr, int is_range) {
    struct nrange *r;
    const char *p, *p0;
    struct nrange *prev = NULL;
    struct nrange *next;

    if (!name)
        return;

    range_normalize(&rr);

    if (!find_nrange_name(s2c(name), slen(name), &prev)) {
        error("Error: range name \"%s\" already defined", s2c(name));
        string_free(name);
        return;
    }

    /* Range name: must contain only letters, digits and _ */
    for (p = s2c(name); *p; p++) {
        if (!isalnumchar_(*p)) {
            error("Invalid range name \"%s\" - illegal combination", s2c(name));
            string_free(name);
            return;
        }
    }

    /* Range name must not be cell name nor a valid number */
    p = p0 = s2c(name);
    if (isdigitchar(*p) ||
        (isalphachar(*p++) && (isdigitchar(*p) ||
                               (isalphachar(*p++) && isdigitchar(*p))))) {
        if (*p0 == '0' && (p0[1] == 'x' || p0[1] == 'X')) {
            ++p;
            while (isxdigitchar(*++p))
                continue;
            if (*p == 'p' || *p == 'P') {
                while (isdigitchar(*++p))
                    continue;
            }
        } else {
            while (isdigitchar(*++p))
                continue;
            if (isdigitchar(*p0) && (*p == 'e' || *p == 'E')) {
                while (isdigitchar(*++p))
                    continue;
            }
        }
        if (!*p) {
            error("Invalid range name \"%s\" - ambiguous", s2c(name));
            string_free(name);
            return;
        }
    }

    if (is_range < 0) {
        is_range = (rr.left.row != rr.right.row || rr.left.col != rr.right.col);
    }

    if (autolabel && rr.left.col > 0 && !is_range) {
        struct ent *cp = lookat(sht, rr.left.row, rr.left.col - 1);
        if (!cp->type && !cp->expr) {
            /* empty cell to the left of the defined cell:
               set the cell label to the name.
             */
            string_set(&cp->label, name);
            cp->type = SC_STRING;
            cp->flags &= ~ALIGN_MASK;
            cp->flags |= ALIGN_DEFAULT;
            FullUpdate++;
            modflg++;
        }
    }

    r = scxmalloc(sizeof(struct nrange));
    r->r_name = name;
    r->r_left.vp = lookat(sht, rr.left.row, rr.left.col);
    r->r_left.vf = rr.left.vf;
    r->r_right.vp = lookat(sht, rr.right.row, rr.right.col);
    r->r_right.vf = rr.right.vf;
    r->r_is_range = is_range;
    // link in doubly linked list
    if (prev) {
        next = prev->r_next;
        prev->r_next = r;
    } else {
        next = rng_base;
        rng_base = r;
    }
    r->r_prev = prev;
    r->r_next = next;
    if (next)
        next->r_prev = r;
    else
        rng_tail = r;
    modflg++;
}

void del_nrange(rangeref_t rr) {
    struct nrange *r;

    range_normalize(&rr);
    r = find_nrange_coords(rr);
    if (!r)
        return;

    if (r->r_next)
        r->r_next->r_prev = r->r_prev;
    else
        rng_tail = r->r_prev;
    if (r->r_prev)
        r->r_prev->r_next = r->r_next;
    else
        rng_base = r->r_next;
    string_free(r->r_name);
    scxfree(r);
    modflg++;
}

void clean_nrange(void) {
    struct nrange *r;
    struct nrange *nextr;

    r = rng_base;
    rng_base = rng_tail = NULL;

    while (r) {
        nextr = r->r_next;
        string_free(r->r_name);
        scxfree(r);
        r = nextr;
    }
}

/* Match on name or lmatch, rmatch */

int find_nrange_name(const char *name, int len, struct nrange **rng) {
    struct nrange *r;
    int cmp;
    int exact = TRUE;

    if (len < 0) {
        exact = FALSE;
        len = -len;
    }
    *rng = NULL;
    for (r = rng_base; r; r = r->r_next) {
        const char *r_name = s2c(r->r_name);
        if ((cmp = strncmp(name, r_name, len)) > 0)
            return cmp;
        *rng = r;
        if (cmp == 0) {
            // XXX: should return cmp if len > strlen(r_name)
            if (!exact || r_name[len] == '\0')
                return cmp;
        }
    }
    return -1;
}

// XXX: should take a boolean to check flags
struct nrange *find_nrange_coords(rangeref_t rr) {
    struct nrange *r;

    for (r = rng_base; r; r = r->r_next) {
        if (r->r_left.vp->row == rr.left.row && r->r_left.vp->col == rr.left.col
        &&  r->r_right.vp->row == rr.right.row && r->r_right.vp->col == rr.right.col) {
            break;
        }
    }
    return r;
}

void sync_nranges(void) {
    struct nrange *r;

    for (r = rng_base; r; r = r->r_next) {
        r->r_left.vp = lookat(sht, r->r_left.vp->row, r->r_left.vp->col);
        r->r_right.vp = lookat(sht, r->r_right.vp->row, r->r_right.vp->col);
    }
}

// XXX: this duplicates sync_expr() ?
static void sync_enode(struct enode *e) {
    if (e) {
        // XXX: should sync 'v' nodes too?
        if (e->type == OP_TYPE_RANGE) {
            e->e.r.left.vp = lookat(sht, e->e.r.left.vp->row, e->e.r.left.vp->col);
            e->e.r.right.vp = lookat(sht, e->e.r.right.vp->row, e->e.r.right.vp->col);
        } else
        if (e->type == OP_TYPE_FUNC) {
            int i;
            for (i = 0; i < e->nargs; i++)
                sync_enode(e->e.args[i]);
        }
    }
}

void sync_ranges(void) {
    int i, j;
    struct ent *p;

    sync_nranges();
    for (i = 0; i <= maxrow; i++) {
        for (j = 0; j <= maxcol; j++) {
            if ((p = getcell(sht, i, j)) && p->expr)
                sync_enode(p->expr);
        }
    }
    sync_franges();
    sync_cranges();
}

void write_nranges(FILE *f) {
    struct nrange *r;

    for (r = rng_tail; r; r = r->r_prev) {
        fprintf(f, "define \"%s\" %s%s%s%d",
                s2c(r->r_name),
                r->r_left.vf & FIX_COL ? "$" : "",
                coltoa(r->r_left.vp->col),
                r->r_left.vf & FIX_ROW ? "$" : "",
                r->r_left.vp->row);
        if (r->r_is_range) {
            fprintf(f, ":%s%s%s%d",
                    r->r_right.vf & FIX_COL ? "$" : "",
                    coltoa(r->r_right.vp->col),
                    r->r_right.vf & FIX_ROW ? "$" : "",
                    r->r_right.vp->row);
        }
        fprintf(f, "\n");
    }
}

void list_nranges(FILE *f) {
    struct nrange *r;

    if (!are_nranges()) {
        fprintf(f, "  No ranges defined");
        return;
    }

    fprintf(f, "  %-30s %s\n","Name","Definition");
    if (!brokenpipe) fprintf(f, "  %-30s %s\n","----","----------");

    for (r = rng_tail; r; r = r->r_prev) {
        fprintf(f, "  %-30s %s%s%s%d",
                s2c(r->r_name),
                r->r_left.vf & FIX_COL ? "$" : "",
                coltoa(r->r_left.vp->col),
                r->r_left.vf & FIX_ROW ? "$" : "",
                r->r_left.vp->row);
        if (brokenpipe) return;
        if (r->r_is_range) {
            fprintf(f, ":%s%s%s%d",
                    r->r_right.vf & FIX_COL ? "$" : "",
                    coltoa(r->r_right.vp->col),
                    r->r_right.vf & FIX_ROW ? "$" : "",
                    r->r_right.vp->row);
        }
        fprintf(f, "\n");
        if (brokenpipe) return;
    }
}

const char *coltoa(int col) {
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

// XXX: should take cellref_t and a boolean to check and/or print flags
//      and/or print named cells
const char *v_name(int row, int col) {
    struct nrange *r;
    static unsigned int bufn;
    static char buf[4][20];

    // XXX: should we test the is_range flag?
    if ((r = find_nrange_coords(rangeref(row, col, row, col)))) {
        return s2c(r->r_name);
    } else {
        char *vname = buf[bufn++ & 3];
        snprintf(vname, sizeof buf[0], "%s%d", coltoa(col), row);
        return vname;
    }
}

// XXX: should take rangeref_t and a boolean to check and/or print flags
//      and/or print named cells
const char *r_name(int r1, int c1, int r2, int c2) {
    struct nrange *r;
    static unsigned int bufn;
    static char buf[2][100];

    if ((r = find_nrange_coords(rangeref(r1, c1, r2, c2)))) {
        return s2c(r->r_name);
    } else {
        char *rname = buf[bufn++ & 1];
        snprintf(rname, sizeof buf[0], "%s%d:%s%d",
                 coltoa(c1), r1, coltoa(c2), r2);
        return rname;
    }
}

void fix_ranges(int row1, int col1, int row2, int col2,
                int delta1, int delta2, struct frange *fr)
{
    int r1, c1, r2, c2, i, j;
    struct nrange *r;

    /* First we fix all of the named ranges. */
    for (r = rng_base; r; r = r->r_next) {
        r1 = r->r_left.vp->row;
        c1 = r->r_left.vp->col;
        r2 = r->r_right.vp->row;
        c2 = r->r_right.vp->col;

        if (!fr || (c1 >= fr->or_left->col && c1 <= fr->or_right->col)) {
            if (r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
            if (c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
        }

        if (!fr || (c2 >= fr->or_left->col && c2 <= fr->or_right->col)) {
            if (r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
            if (c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
        }
        r->r_left.vp = lookat(sht, r1, c1);
        r->r_right.vp = lookat(sht, r2, c2);
    }

    /* Next, we go through all valid cells with expressions and fix any ranges
     * that need fixing.
     */
    for (i = 0; i <= maxrow; i++) {
        for (j = 0; j <= maxcol; j++) {
            struct ent *p = getcell(sht, i, j);
            if (p && p->expr)
                fix_enode(p->expr, row1, col2, row2, col2, delta1, delta2, fr);
        }
    }
    fix_frames(row1, col1, row2, col2, delta1, delta2, fr);
    fix_colors(row1, col1, row2, col2, delta1, delta2, fr);
}

static void fix_enode(struct enode *e, int row1, int col1, int row2, int col2,
                      int delta1, int delta2, struct frange *fr)
{
    if (e) {
        // XXX: should fix 'v' nodes too?
        if (e->type == OP_TYPE_RANGE) {
            int r1 = e->e.r.left.vp->row;
            int c1 = e->e.r.left.vp->col;
            int r2 = e->e.r.right.vp->row;
            int c2 = e->e.r.right.vp->col;
            if (r1 > r2) SWAPINT(r1, r2);
            if (c1 > c2) SWAPINT(c1, c2);

            if (!(fr && (c1 < fr->or_left->col || c1 > fr->or_right->col))) {
                if (r1 != r2 && r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
                if (c1 != c2 && c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
            }

            if (!(fr && (c2 < fr->or_left->col || c2 > fr->or_right->col))) {
                if (r1 != r2 && r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
                if (c1 != c2 && c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
            }
            e->e.r.left.vp = lookat(sht, r1, c1);
            e->e.r.right.vp = lookat(sht, r2, c2);
        } else
        if (e->type == OP_TYPE_FUNC) {
            int i;
            for (i = 0; i < e->nargs; i++)
                fix_enode(e->e.args[i], row1, col1, row2, col2, delta1, delta2, fr);
        }
    }
}
