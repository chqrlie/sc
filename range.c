/*      SC      A Spreadsheet Calculator
 *              Range Manipulations
 *
 *              Robert Bond, 4/87
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

int nrange_test(sheet_t *sp) {
    return sp->nrange_base != NULL;
}

void nrange_add(sheet_t *sp, SCXMEM string_t *name, rangeref_t rr, int is_range) {
    struct nrange *r;
    const char *p, *p0;
    struct nrange *prev = NULL;
    struct nrange *next;

    if (!name)
        return;

    range_normalize(&rr);

    // XXX: should just redefine existing named range
    if (!nrange_find_name(sp, s2c(name), slen(name), &prev)) {
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
        struct ent *cp = lookat(sp, rr.left.row, rr.left.col - 1);
        if (cp->type == SC_EMPTY && !cp->expr && !(cp->flags & IS_LOCKED)) {
            /* empty cell to the left of the defined cell:
               set the cell label to the name.
             */
            string_set(&cp->label, name);
            cp->type = SC_STRING;
            sp->modflg++;   // XXX: redundant
            FullUpdate++;   // XXX: redundant?
        }
    }

    r = scxmalloc(sizeof(struct nrange));
    r->r_name = name;
    r->r_left.vp = lookat(sp, rr.left.row, rr.left.col);
    r->r_left.vf = rr.left.vf;
    r->r_right.vp = lookat(sp, rr.right.row, rr.right.col);
    r->r_right.vf = rr.right.vf;
    r->r_is_range = is_range;
    // link in doubly linked list
    if (prev) {
        next = prev->r_next;
        prev->r_next = r;
    } else {
        next = sp->nrange_base;
        sp->nrange_base = r;
    }
    r->r_prev = prev;
    r->r_next = next;
    if (next)
        next->r_prev = r;
    else
        sp->nrange_tail = r;
    sp->modflg++;
}

void nrange_delete(sheet_t *sp, rangeref_t rr) {
    struct nrange *r;

    range_normalize(&rr);
    r = nrange_find_coords(sp, rr);
    if (!r)
        return;

    if (r->r_next)
        r->r_next->r_prev = r->r_prev;
    else
        sp->nrange_tail = r->r_prev;
    if (r->r_prev)
        r->r_prev->r_next = r->r_next;
    else
        sp->nrange_base = r->r_next;
    string_free(r->r_name);
    scxfree(r);
    sp->modflg++;
}

void nrange_clean(sheet_t *sp) {
    struct nrange *r;
    struct nrange *nextr;

    r = sp->nrange_base;
    sp->nrange_base = sp->nrange_tail = NULL;

    while (r) {
        nextr = r->r_next;
        string_free(r->r_name);
        scxfree(r);
        r = nextr;
    }
}

/* Match on name or lmatch, rmatch */
int nrange_find_name(sheet_t *sp, const char *name, int len, struct nrange **rng) {
    struct nrange *r;
    int cmp;
    int exact = TRUE;

    if (len < 0) {
        exact = FALSE;
        len = -len;
    }
    *rng = NULL;
    for (r = sp->nrange_base; r; r = r->r_next) {
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
struct nrange *nrange_find_coords(sheet_t *sp, rangeref_t rr) {
    struct nrange *r;

    for (r = sp->nrange_base; r; r = r->r_next) {
        if (r->r_left.vp->row == rr.left.row && r->r_left.vp->col == rr.left.col
        &&  r->r_right.vp->row == rr.right.row && r->r_right.vp->col == rr.right.col) {
            break;
        }
    }
    return r;
}

void nrange_sync(sheet_t *sp) {
    struct nrange *r;

    for (r = sp->nrange_base; r; r = r->r_next) {
        r->r_left.vp = lookat(sp, r->r_left.vp->row, r->r_left.vp->col);
        r->r_right.vp = lookat(sp, r->r_right.vp->row, r->r_right.vp->col);
    }
}

void nrange_write(sheet_t *sp, FILE *f) {
    struct nrange *r;

    for (r = sp->nrange_tail; r; r = r->r_prev) {
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

void nrange_list(sheet_t *sp, FILE *f) {
    struct nrange *r;

    if (!nrange_test(sp)) {
        fprintf(f, "  No ranges defined");
        return;
    }

    fprintf(f, "  %-30s %s\n","Name","Definition");
    if (!brokenpipe) fprintf(f, "  %-30s %s\n","----","----------");

    for (r = sp->nrange_tail; r; r = r->r_prev) {
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
const char *v_name(sheet_t *sp, int row, int col) {
    struct nrange *r;
    static unsigned int bufn;
    static char buf[4][20];

    // XXX: should we test the is_range flag?
    if ((r = nrange_find_coords(sp, rangeref(row, col, row, col)))) {
        return s2c(r->r_name);
    } else {
        char *vname = buf[bufn++ & 3];
        snprintf(vname, sizeof buf[0], "%s%d", coltoa(col), row);
        return vname;
    }
}

// XXX: should take rangeref_t and a boolean to check and/or print flags
//      and/or print named cells
const char *r_name(sheet_t *sp, int r1, int c1, int r2, int c2) {
    struct nrange *r;
    static unsigned int bufn;
    static char buf[2][100];

    if ((r = nrange_find_coords(sp, rangeref(r1, c1, r2, c2)))) {
        return s2c(r->r_name);
    } else {
        char *rname = buf[bufn++ & 1];
        snprintf(rname, sizeof buf[0], "%s%d:%s%d",
                 coltoa(c1), r1, coltoa(c2), r2);
        return rname;
    }
}

void nrange_fix(sheet_t *sp, int row1, int col1, int row2, int col2,
                int delta1, int delta2, struct frange *fr)
{
    int r1, c1, r2, c2;
    struct nrange *r;

    /* We fix all of the named ranges. */
    for (r = sp->nrange_base; r; r = r->r_next) {
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
        // XXX: should check if range disappeared
        r->r_left.vp = lookat(sp, r1, c1);
        r->r_right.vp = lookat(sp, r2, c2);
    }
}
