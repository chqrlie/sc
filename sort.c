/*      SC      A Spreadsheet Calculator
 *              Sorting routines
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Originally created:  April, 2001
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

struct sortcrit {
    int direction, type, column;
};

typedef struct sort_context {
    sheet_t *sp;
    int minr, minc, maxr, maxc;
    SCXMEM struct sortcrit *crit;
    int ncrit;
} sort_ctx_t;

static sort_ctx_t *sort_context;

static int compare(const void *a1, const void *a2) {
    sort_ctx_t *sc = sort_context;
    int row1 = *(const int *)a1;
    int row2 = *(const int *)a2;
    struct ent *p1;
    struct ent *p2;
    int result = 0;
    int i;

    for (i = 0; !result; i++) {
        if (i >= sc->ncrit) {
            /* stable sort: rows that compare equal stay in the same order */
            return (row1 > row2) - (row1 < row2);
        }
        p1 = getcell(sc->sp, row1, sc->crit[i].column);
        p2 = getcell(sc->sp, row2, sc->crit[i].column);

        // XXX: comparison algorithm should be the same as for expressions
        /* mixed types are sorted in this order:
           numbers < text < logical < error < empty
         */
        /* empty cells always compare larger */
        if (!p1 || p1->type == SC_EMPTY) {
            result = (p2 || p2->type == SC_EMPTY) ? 1 : 0;
            continue;
        }
        if (!p2 || p2->type == SC_EMPTY) {
            result = -1;
            continue;
        }
        // XXX: ignore sc->crit[i].type
        if (p1->type == SC_NUMBER) {
            if (p2->type == SC_NUMBER)
                result = (p1->v > p2->v) - (p1->v < p2->v);
            else
                result = -1;
        } else
        if (p2->type == SC_NUMBER) {
            result = 1;
        } else
        if (p1->type == SC_STRING) {
            if (p2->type == SC_STRING)
                result = strcmp(s2c(p1->label), s2c(p2->label));
            else
                result = -1;
        } else
        if (p2->type == SC_STRING) {
            result = 1;
        } else
        if (p1->type == SC_BOOLEAN) {
            if (p2->type == SC_BOOLEAN)
                result = p2->v - p1->v;
            else
                result = -1;
        } else
        if (p2->type == SC_BOOLEAN) {
            result = 1;
        } else {
            result = p2->cellerror - p1->cellerror;
        }
        result *= sc->crit[i].direction;
    }
    return result;
}

void sortrange(sheet_t *sp, rangeref_t rr, SCXMEM string_t *criteria) {
    sort_ctx_t sc[1];
    struct sortcrit *crit;
    SCXMEM int *rows;
    SCXMEM int *destrows;
    int i, r, nrows, col, len;
    const char *cp;
    struct ent *p;

    range_normalize(&rr);
    sc->sp = sp;
    sc->minr = rr.left.row;
    sc->minc = rr.left.col;
    sc->maxr = rr.right.row;
    sc->maxc = rr.right.col;
    nrows = sc->maxr - sc->minr + 1;
    if (nrows <= 1)
        return;

    /* allocate sort criteria: at type 0, type 1 col=minc */
    sc->crit = scxmalloc(sizeof(*sc->crit) * 2);
    /* allocate array of row numbers that will be sorted */
    rows = scxmalloc(sizeof(*rows) * nrows);
    destrows = scxmalloc(sizeof(*rows) * nrows);
    if (!sc->crit || !rows || !destrows)
        goto fail;

    for (i = 0, r = sc->minr; r <= sc->maxr; r++, i++)
        rows[i] = destrows[i] = r;

    if (sempty(criteria)) {
        sc->crit[0].direction = 1;
        sc->crit[0].type = 1;
        sc->crit[0].column = sc->minc;
        sc->crit[1].direction = 1;
        sc->crit[1].type = 0;
        sc->crit[1].column = sc->minc;
        sc->ncrit = 2;
    } else {
        for (sc->ncrit = 0, cp = s2c(criteria); *cp; sc->ncrit++) {
            if (sc->ncrit > 1) {
                /* allocate new sorting criterion */
                void *new_crit = scxrealloc(sc->crit, (sc->ncrit + 1) * (sizeof(*sc->crit)));
                if (!new_crit)
                    goto fail;
                sc->crit = new_crit;
            }
            crit = &sc->crit[sc->ncrit];
            switch (*cp++) {
            case '+':
                crit->direction = 1;
                break;
            case '-':
                crit->direction = -1;
                break;
            default:
                crit->direction = 1;
                cp--;
            }
            switch (*cp++) {
            case '#':
                crit->type = 0;
                break;
            case '$':
                crit->type = 1;
                break;
            default:
                crit->type = 0;
                cp--;
            }
            if ((col = atocol(cp, &len)) >= 0) {
                cp += len;
                crit->column = col;
                if (col >= sc->minc && col <= sc->maxc)
                    continue;
            }
            error("Invalid sort criteria");
            goto fail;
        }
    }

    /* we should use qsort_r() but it might not be available */
    sort_context = sc;
    qsort(rows, nrows, sizeof(*rows), compare);
    sort_context = NULL;
    for (i = 0; i < nrows; i++)
        destrows[rows[i] - sc->minr] = sc->minr + i;

    // XXX: this is bogus:
    // - cell values and formats should be moved
    // - cell borders should be left unchanged?
    // - ranges should be adjusted if completely included
    //   in a single row of the sorting range
    /* move cell range to subsheet delbuf[++dbidx] */
    dbidx = 0;  /* avoid clobbering delbuf[0] */
    erase_area(sp, ++dbidx, sc->minr, sc->minc, sc->maxr, sc->maxc, 1);
    // XXX: this seems bogus too
    // XXX: make formulas that refer to the sort range
    //      point to empty cells
    // XXX: should we use sync_refs() instead?
    sync_ranges(sp);
    for (i = 0, p = delbuf[dbidx].ptr; p; p = p->next) {
        if (p->row < sc->minr || p->row > sc->maxr) {
            /* cannot find row in sort range */
            error("sort error");
            break;
        }
        p->row = destrows[p->row - sc->minr];
    }
    // XXX: should use destrows to update formulae and ranges
    pullcells(sp, dbidx, 'm', cellref(sc->minr, sc->minc));    /* PULLMERGE */
    delbuf_free(dbidx--);

fail:
    string_free(criteria);
    scxfree(sc->crit);
    scxfree(rows);
    scxfree(destrows);
}
