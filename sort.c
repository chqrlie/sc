/*      SC      A Spreadsheet Calculator
 *              Sorting routines
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Originally created:  April, 2001
 *
 *              $Revision: 7.16 $
 */

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include "compat.h"
#include "sc.h"

int compare(const void *row1, const void *row2);

static struct sortcrit {
    int direction, type, column;
} *sort;

static int howmany;

void sortrange(struct ent *left, struct ent *right, char *criteria)
{
    int minr, minc, maxr, maxc, r, c;
    int *rows, col = 0;
    int cp = 0;
    struct ent *p;

    minr = left->row < right->row ? left->row : right->row;
    minc = left->col < right->col ? left->col : right->col;
    maxr = left->row > right->row ? left->row : right->row;
    maxc = left->col > right->col ? left->col : right->col;

    sort = scxmalloc((2 * sizeof(struct sortcrit)));
    rows = scxmalloc((maxr - minr + 1) * sizeof(int));
    for (r = minr, c = 0; r <= maxr; r++, c++)
        rows[c] = r;

    if (!criteria) {
        sort[0].direction = 1;
        sort[0].type = 1;
        sort[0].column = minc;
        sort[1].direction = 1;
        sort[1].type = 0;
        sort[1].column = minc;
        howmany = 2;
    } else
        for (howmany = 0; criteria[cp]; howmany++) {
            if (howmany > 1)
                sort = scxrealloc(sort,
                        (howmany + 1) * (sizeof(struct sortcrit)));
            switch (criteria[cp++]) {
                case '+':
                    sort[howmany].direction = 1;
                    break;
                case '-':
                    sort[howmany].direction = -1;
                    break;
                default:
                    error("Invalid sort criteria");
                    return;
            }
            switch (criteria[cp++]) {
                case '#':
                    sort[howmany].type = 0;
                    break;
                case '$':
                    sort[howmany].type = 1;
                    break;
                default:
                    error("Invalid sort criteria");
                    return;
            }
            if (criteria[cp])
                col = toupper((int)criteria[cp++]) - 'A';
            else {
                error("Invalid sort criteria");
                return;
            }
            if (criteria[cp] && criteria[cp] != '+' && criteria[cp] != '-')
                col = (col + 1) * 26 + toupper((int)criteria[cp++]) - 'A';
            sort[howmany].column = col;
            if (col < minc || col > maxc) {
                error("Invalid sort criteria");
                return;
            }
        }

    qsort(rows, maxr - minr + 1, sizeof(int), compare);
    erase_area(minr, minc, maxr, maxc, 1);
    sync_ranges();
    for (c = 0, p = delbuf[dbidx]; p; p = p->next) {
        if (rows[c] != p->row) {
            for (c = 0; c <= maxr - minr && rows[c] != p->row; c++) ;
            if (c > maxr - minr) {
                error("sort error");
                return;
            }
        }
        p->row = minr + c;
    }
    scxfree(sort);
    scxfree(rows);
    if (criteria) scxfree(criteria);

    r = currow;
    c = curcol;
    currow = minr;
    curcol = minc;

    pullcells('m');
    flush_saved();

    currow = r;
    curcol = c;
}

int compare(const void *row1, const void *row2)
{
    struct ent *p1;
    struct ent *p2;
    double diff;
    int result = 0;
    int i;

    for (i = 0; !result && i < howmany; i++) {
        p1 = *ATBL(tbl, *((const int *) row1), sort[i].column);
        p2 = *ATBL(tbl, *((const int *) row2), sort[i].column);

        if (sort[i].type) {
            if (p1 && p1->label)
                if (p2 && p2->label)
                    result = strcmp(p1->label, p2->label);
                else
                    result = -1;
            else if (p2 && p2->label)
                result = 1;
        } else
            if (p1 && p2 && p1->flags & IS_VALID && p2->flags & IS_VALID) {
                diff = (p1->v - p2->v);
                result = (diff > 0 ? 1 : diff < 0 ? -1 : 0);
            }
            else if (p1 && p1->flags & IS_VALID)
                result = -1;
            else if (p2 && p2->flags & IS_VALID)
                result = 1;

        result *= sort[i].direction;
    }

    if (!result)
        result = (*((const int *) row1) - *((const int *) row2));

    return (result);
}
