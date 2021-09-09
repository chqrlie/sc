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

int compare(const void *row1, const void *row2);

struct sortcrit {
    int direction, type, column;
};

static SCXMEM struct sortcrit *sort;
static int howmany;

void sortrange(rangeref_t rr, SCXMEM string_t *criteria) {
    SCXMEM int *rows;
    int minr, minc, maxr, maxc;
    int i, r, nrows, col, len, qtmp;
    const char *cp;
    struct ent *p;

    range_normalize(&rr);
    minr = rr.left.row;
    minc = rr.left.col;
    maxr = rr.right.row;
    maxc = rr.right.col;
    nrows = maxr - minr + 1;

    sort = scxmalloc(sizeof(*sort) * 2);
    rows = scxmalloc(sizeof(*rows) * nrows);
    if (!sort || !rows)
        goto fail;

    for (i = 0, r = minr; r <= maxr; r++, i++)
        rows[i] = r;

    if (sempty(criteria)) {
        sort[0].direction = 1;
        sort[0].type = 1;
        sort[0].column = minc;
        sort[1].direction = 1;
        sort[1].type = 0;
        sort[1].column = minc;
        howmany = 2;
    } else {
        for (howmany = 0, cp = s2c(criteria); *cp; howmany++) {
            if (howmany > 1) {
                void *new_sort = scxrealloc(sort, (howmany + 1) * (sizeof(struct sortcrit)));
                if (!new_sort)
                    goto fail;
                sort = new_sort;
            }
            switch (*cp++) {
            case '+':
                sort[howmany].direction = 1;
                break;
            case '-':
                sort[howmany].direction = -1;
                break;
            default:
                sort[howmany].direction = 1;
                cp--;
            }
            switch (*cp++) {
            case '#':
                sort[howmany].type = 0;
                break;
            case '$':
                sort[howmany].type = 1;
                break;
            default:
                sort[howmany].type = 0;
                cp--;
            }
            if ((col = atocol(cp, &len)) >= 0) {
                cp += len;
                sort[howmany].column = col;
                if (col >= minc && col <= maxc)
                    continue;
            }
            error("Invalid sort criteria");
            goto fail;
        }
    }

    qsort(rows, nrows, sizeof(int), compare);
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, minr, minc, maxr, maxc, 1);
    // XXX: make formulas that refer to the sort range
    //      point to empty cells
    // XXX: should we use sync_refs() instead?
    sync_ranges();
    for (i = 0, p = delbuf[dbidx]; p; p = p->next) {
        if (rows[i] != p->row) {
            /* find destination row */
            for (i = 0; i < nrows && rows[i] != p->row; i++)
                continue;
            if (i >= nrows) {
                /* cannot find row in sort range */
                error("sort error");
                // XXX: memory leak
                return;
            }
        }
        p->row = minr + i; // XXX: sync formulas ?
    }
    // XXX: Achtung! pullcells uses qbuf if set
    qtmp = qbuf;
    qbuf = 0;
    pullcells('m', cellref(minr, minc));    /* PULLMERGE */
    qbuf = qtmp;
    /* free delbuf[dbidx--] */
    flush_saved(dbidx--);

    // XXX: should actually move to the new position of the same cell

fail:
    string_free(criteria);
    scxfree(sort);
    sort = NULL;
    howmany = 0;
    scxfree(rows);
}

int compare(const void *a1, const void *a2) {
    int row1 = *(const int *)a1;
    int row2 = *(const int *)a2;
    struct ent *p1;
    struct ent *p2;
    int result = 0;
    int i;

    for (i = 0; !result; i++) {
        if (i >= howmany)
            return (row1 > row2) - (row1 < row2);

        p1 = getcell(sht, row1, sort[i].column);
        p2 = getcell(sht, row2, sort[i].column);

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
        // XXX: ignore sort[i].type
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
        result *= sort[i].direction;
    }
    return result;
}
