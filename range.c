/*      SC      A Spreadsheet Calculator
 *              Range Manipulations
 *
 *              Robert Bond, 4/87
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

static void sync_enode(struct enode *e);
static void fix_enode(struct enode *e, int row1, int col1, int row2, int col2,
               int delta1, int delta2);

static struct range *rng_base;
static struct range *rng_tail;

int are_ranges(void) {
    return rng_base != NULL;
}

void add_range(char *name, struct ent_ptr left, struct ent_ptr right, int is_range) {
    struct range *r;
    char *p;
    int minr, minc, maxr, maxc;
    int minrf, mincf, maxrf, maxcf;
    struct ent *rcp;
    struct range *prev = NULL;
    struct range *next;

    if (left.vp->row < right.vp->row) {
        minr = left.vp->row; minrf = left.vf & FIX_ROW;
        maxr = right.vp->row; maxrf = right.vf & FIX_ROW;
    } else {
        minr = right.vp->row; minrf = right.vf & FIX_ROW;
        maxr = left.vp->row; maxrf = right.vf & FIX_ROW;
    }

    if (left.vp->col < right.vp->col) {
        minc = left.vp->col; mincf = left.vf & FIX_COL;
        maxc = right.vp->col; maxcf = right.vf & FIX_COL;
    } else {
        minc = right.vp->col; mincf = right.vf & FIX_COL;
        maxc = left.vp->col; maxcf = left.vf & FIX_COL;
    }

    left.vp = lookat(minr, minc);
    left.vf = minrf | mincf;
    right.vp = lookat(maxr, maxc);
    right.vf = maxrf | maxcf;

    if (!find_range_name(name, strlen(name), &prev)) {
        error("Error: range name \"%s\" already defined", name);
        scxfree(name);
        return;
    }

    for (p = name; *p; p++) {
        if (!isalnumchar_(*p)) {
            error("Invalid range name \"%s\" - illegal combination", name);
            scxfree(name);
            return;
        }
    }

    p = name;
    // cell names and numbers:
    // 1 and 2 letter column names
    // integers, hex integers, floats and hex floats
    if (isdigitchar(*p) ||
        (isalphachar(*p++) && (isdigitchar(*p) ||
                               (isalphachar(*p++) && isdigitchar(*p))))) {
        if (*name == '0' && (name[1] == 'x' || name[1] == 'X')) {
            ++p;
            while (isxdigitchar(*++p))
                continue;
            if (*p == 'p' || *p == 'P') {
                while (isxdigitchar(*++p))
                    continue;
            }
        } else {
            while (isdigitchar(*++p))
                continue;
            if (isdigitchar(*name) && (*p == 'e' || *p == 'E')) {
                while (isdigitchar(*++p))
                    continue;
            }
        }
        if (!(*p)) {
            error("Invalid range name \"%s\" - ambiguous", name);
            scxfree(name);
            return;
        }
    }

    if (autolabel && minc > 0 && !is_range) {
        rcp = lookat(minr, minc - 1);
        if (rcp->label == 0 && rcp->expr == 0 && rcp->v == 0)
            label(rcp, name, 0);
    }

    r = scxmalloc(sizeof(struct range));
    r->r_name = name;
    r->r_left = left;
    r->r_right = right;
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

void del_range(struct ent *left, struct ent *right) {
    struct range *r;
    int minr, minc, maxr, maxc;

    minr = left->row < right->row ? left->row : right->row;
    minc = left->col < right->col ? left->col : right->col;
    maxr = left->row > right->row ? left->row : right->row;
    maxc = left->col > right->col ? left->col : right->col;

    left = lookat(minr, minc);
    right = lookat(maxr, maxc);
    r = find_range_coords(left, right);
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
    scxfree(r->r_name);
    scxfree(r);
    modflg++;
}

void clean_range(void) {
    struct range *r;
    struct range *nextr;

    r = rng_base;
    rng_base = rng_tail = NULL;

    while (r) {
        nextr = r->r_next;
        scxfree(r->r_name);
        scxfree(r);
        r = nextr;
    }
}

/* Match on name or lmatch, rmatch */

int find_range_name(const char *name, int len, struct range **rng) {
    struct range *r;
    int cmp;
    int exact = TRUE;

    if (len < 0) {
        exact = FALSE;
        len = -len;
    }
    *rng = NULL;
    for (r = rng_base; r; r = r->r_next) {
        if ((cmp = strncmp(name, r->r_name, len)) > 0)
            return cmp;
        *rng = r;
        if (cmp == 0) {
            if (!exact || r->r_name[len] == '\0')
                return cmp;
        }
    }
    return -1;
}

struct range *find_range_coords(struct ent *lmatch, struct ent *rmatch) {
    struct range *r;

    for (r = rng_base; r; r = r->r_next) {
        if ((lmatch == r->r_left.vp) && (rmatch == r->r_right.vp)) {
            break;
        }
    }
    return r;
}

void sync_ranges(void) {
    int i, j;
    struct range *r;
    struct ent *p;

    for (r = rng_base; r; r = r->r_next) {
        r->r_left.vp = lookat(r->r_left.vp->row, r->r_left.vp->col);
        r->r_right.vp = lookat(r->r_right.vp->row, r->r_right.vp->col);
    }
    for (i = 0; i <= maxrow; i++) {
        for (j = 0; j <= maxcol; j++) {
            if ((p = *ATBL(tbl,i,j)) && p->expr)
                sync_enode(p->expr);
        }
    }
    sync_franges();
    sync_cranges();
}

static void sync_enode(struct enode *e) {
    if (e) {
        if ((e->op & REDUCE)) {
            e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
            e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
        } else if (e->op != O_VAR && e->op != O_CONST && e->op != O_SCONST) {
            sync_enode(e->e.o.left);
            sync_enode(e->e.o.right);
        }
    }
}

void write_ranges(FILE *f) {
    struct range *r;

    for (r = rng_tail; r; r = r->r_prev) {
        fprintf(f, "define \"%s\" %s%s%s%d",
                r->r_name,
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

void list_ranges(FILE *f) {
    struct range *r;

    if (!are_ranges()) {
        fprintf(f, "  No ranges defined");
        return;
    }

    fprintf(f, "  %-30s %s\n","Name","Definition");
    if (!brokenpipe) fprintf(f, "  %-30s %s\n","----","----------");

    for (r = rng_tail; r; r = r->r_prev) {
        fprintf(f, "  %-30s %s%s%s%d",
                r->r_name,
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

char *v_name(int row, int col) {
    struct ent *v;
    struct range *r;
    static unsigned int bufn;
    static char buf[4][20];

    v = lookat(row, col);
    r = find_range_coords(v, v);
    if (r) {
        return r->r_name;
    } else {
        char *vname = buf[bufn++ & 3];
        snprintf(vname, sizeof buf[0], "%s%d", coltoa(col), row);
        return vname;
    }
}

char *r_name(int r1, int c1, int r2, int c2) {
    struct ent *v1, *v2;
    struct range *r;
    static char buf[100];

    v1 = lookat(r1, c1);
    v2 = lookat(r2, c2);
    r = find_range_coords(v1, v2);
    if (r) {
        return r->r_name;
    } else {
        // XXX: should not accept partial range names?
        snprintf(buf, sizeof buf, "%s:%s", v_name(r1, c1), v_name(r2, c2));
        return buf;
    }
}

void fix_ranges(int row1, int col1, int row2, int col2, int delta1, int delta2) {
    int r1, r2, c1, c2, i, j;
    struct range *r;
    struct frange *fr;
    struct ent *p;

    fr = find_frange(currow, curcol);

    /* First we fix all of the named ranges. */
    for (r = rng_base; r; r = r->r_next) {
        r1 = r->r_left.vp->row;
        c1 = r->r_left.vp->col;
        r2 = r->r_right.vp->row;
        c2 = r->r_right.vp->col;

        if (!(fr && (c1 < fr->or_left->col || c1 > fr->or_right->col))) {
            if (r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
            if (c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
        }

        if (!(fr && (c2 < fr->or_left->col || c2 > fr->or_right->col))) {
            if (r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
            if (c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
        }
        r->r_left.vp = lookat(r1, c1);
        r->r_right.vp = lookat(r2, c2);
    }

    /* Next, we go through all valid cells with expressions and fix any ranges
     * that need fixing.
     */
    for (i = 0; i <= maxrow; i++) {
        for (j = 0; j <= maxcol; j++) {
            if ((p = *ATBL(tbl,i,j)) && p->expr)
                fix_enode(p->expr, row1, col2, row2, col2, delta1, delta2);
        }
    }
    fix_frames(row1, col1, row2, col2, delta1, delta2);
    fix_colors(row1, col1, row2, col2, delta1, delta2);
}

static void fix_enode(struct enode *e, int row1, int col1, int row2, int col2,
                      int delta1, int delta2)
{
    if (e) {
        if ((e->op & REDUCE)) {
            int r, c;
            int r1, c1, r2, c2;
            struct frange *fr;

            fr = find_frange(currow, curcol);
            r1 = e->e.r.left.vp->row;
            c1 = e->e.r.left.vp->col;
            r2 = e->e.r.right.vp->row;
            c2 = e->e.r.right.vp->col;
            if (r1 > r2) { r = r2; r2 = r1; r1 = r; }
            if (c1 > c2) { c = c2; c2 = c1; c1 = c; }

            if (!(fr && (c1 < fr->or_left->col || c1 > fr->or_right->col))) {
                if (r1 != r2 && r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
                if (c1 != c2 && c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
            }

            if (!(fr && (c2 < fr->or_left->col || c2 > fr->or_right->col))) {
                if (r1 != r2 && r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
                if (c1 != c2 && c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
            }
            e->e.r.left.vp = lookat(r1, c1);
            e->e.r.right.vp = lookat(r2, c2);

        } else if (e->op != O_VAR && e->op !=O_CONST && e->op != O_SCONST) {
            fix_enode(e->e.o.left, row1, col1, row2, col2, delta1, delta2);
            fix_enode(e->e.o.right, row1, col1, row2, col2, delta1, delta2);
        }
    }
}
