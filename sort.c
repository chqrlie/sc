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

void sortrange(struct ent *left, struct ent *right, const char *criteria) {
    int r, c, i;
    SCXMEM int *rows;
    int nrows, col = 0;
    const char *cp = criteria;
    struct ent *p;
    int minr = left->row;
    int minc = left->col;
    int maxr = right->row;
    int maxc = right->col;
    if (minr > maxr) SWAPINT(minr, maxr);
    if (minc > maxc) SWAPINT(minc, maxc);
    nrows = (maxr - minr + 1);

    sort = scxmalloc(2 * sizeof(struct sortcrit));
    rows = scxmalloc(nrows * sizeof(int));
    for (i = 0, r = minr; r <= maxr; r++, i++)
        rows[i] = r;

    if (!criteria) {
        sort[0].direction = 1;
        sort[0].type = 1;
        sort[0].column = minc;
        sort[1].direction = 1;
        sort[1].type = 0;
        sort[1].column = minc;
        howmany = 2;
    } else {
        for (howmany = 0; *cp; howmany++) {
            if (howmany > 1)
                sort = scxrealloc(sort, (howmany + 1) * (sizeof(struct sortcrit)));
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
            if (isalphachar(*cp)) {
                col = toupperchar(*cp++) - 'A';
                if (isalphachar(*cp))
                    col = (col + 1) * 26 + toupperchar(*cp++) - 'A';
                sort[howmany].column = col;
                if (col < minc || col > maxc) {
                    error("Invalid sort criteria");
                    goto fail;
                }
            } else {
                error("Invalid sort criteria");
                goto fail;
            }
        }
    }

    qsort(rows, nrows, sizeof(int), compare);
    erase_area(minr, minc, maxr, maxc, 1);
    sync_ranges();
    for (i = 0, p = delbuf[dbidx]; p; p = p->next) {
        if (rows[i] != p->row) {
            for (i = 0; i < nrows && rows[i] != p->row; i++)
                continue;
            if (i >= nrows) {
                /* cannot find row in sort range */
                error("sort error");
                // XXX: memory leak
                return;
            }
        }
        p->row = minr + i;
    }
    r = currow;
    c = curcol;
    currow = minr;
    curcol = minc;

    // XXX: should use copy semantics, not move
    pullcells('m');
    flush_saved();

    /* Restore current cell. */
    currow = r;
    curcol = c;

fail:
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

    for (i = 0; !result && i < howmany; i++) {
        p1 = *ATBL(tbl, row1, sort[i].column);
        p2 = *ATBL(tbl, row2, sort[i].column);

        // XXX: comparison algorithm should be the same as for expressions
        if (sort[i].type) {
            if (p1 && p1->label) {
                if (p2 && p2->label)
                    result = strcmp(p1->label, p2->label);
                else
                    result = -1;
            } else
            if (p2 && p2->label)
                result = 1;
        } else
            if (p1 && p2 && (p1->flags & IS_VALID) && (p2->flags & IS_VALID)) {
                result = (p1->v > p2->v) - (p1->v < p2->v);
            } else
            if (p1 && (p1->flags & IS_VALID))
                result = -1;
            else
            if (p2 && (p2->flags & IS_VALID))
                result = 1;

        result *= sort[i].direction;
    }

    if (!result)
        result = (row1 > row2) - (row1 < row2);

    return result;
}
