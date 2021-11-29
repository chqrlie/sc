/*      SC      A Spreadsheet Calculator
 *              Spreadsheet 'tbl' creation
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#include "sc.h"

/* scxrealloc will just scxmalloc if oldptr is == NULL */
#define GROWALLOC(ptr, curcount, nelem, msg, def) do {             \
        size_t i__ = (curcount), n__ = (nelem);                    \
        void *newptr__ = scxrealloc(ptr, n__ * sizeof(*(ptr)));    \
        if (newptr__ == NULL) {                                    \
           error(msg);                                             \
           return FALSE;                                           \
       }                                                           \
       ptr = newptr__;                                             \
       while (i__ < n__) { (ptr)[i__++] = def; }                   \
    } while (0)

static const char nolonger[] = "The table cannot be any longer";
static const char nowider[] = "The table cannot be any wider";

/*
 * grow the main && auxiliary tables (update maxrows/maxcols as needed)
 * toprow &&/|| topcol tell us a better guess of how big to become.
 * we return TRUE if we could grow, FALSE if not....
 */
static int growtbl(sheet_t *sp, int toprow, int topcol) {
    int row, curcols, currows, newrows, newcols;

    newrows = currows = sp->maxrows;
    newcols = curcols = sp->maxcols;

    if (topcol >= curcols) {
        if (topcol > ABSMAXCOLS) {
            error(nowider);
            return FALSE;
        }
        newcols = (topcol / GROWAMT + 1) * GROWAMT;
        if (newcols > ABSMAXCOLS)
            newcols = ABSMAXCOLS;
    }
    if (toprow >= currows) {
        if (toprow > ABSMAXROWS) {
            error(nolonger);
            return FALSE;
        }
        newrows = (toprow / GROWAMT + 1) * GROWAMT;
        if (newrows > ABSMAXROWS)
            newrows = ABSMAXROWS;
    }
    if (newcols > curcols) {
        colfmt_t def_colfmt = { FALSE, DEFWIDTH, DEFPREC, DEFREFMT };
        GROWALLOC(sp->colfmt, curcols, newcols, nowider, def_colfmt);
        for (row = 0; row < currows; row++) {
            GROWALLOC(sp->tbl[row].cp, curcols, newcols, nowider, NULL);
            sp->tbl[row].maxcol = newcols - 1;
        }
    }

    if (newrows > currows) {
        rowfmt_t def_rowfmt = { FALSE };
        rowptr_t def_rowptr = { 0, -1, NULL };
        GROWALLOC(sp->rowfmt, currows, newrows, nolonger, def_rowfmt);
        GROWALLOC(sp->tbl, currows, newrows, nolonger, def_rowptr);
        for (row = currows; row < newrows; row++) {
            GROWALLOC(sp->tbl[row].cp, 0, newcols, nolonger, NULL);
            sp->tbl[row].maxcol = newcols - 1;
        }
    }
    sp->maxrows = newrows;
    sp->maxcols = newcols;

    for (sp->rescol = 4; newrows >= 1000; sp->rescol++) {
        newrows /= 10;
    }
    return TRUE;
}

/*
 * check to see if row && col are currently allocated, if not expand the
 * current size if we can.
 */

/* return value:
   0 if not resizing was needed
   1 if resizing succeeded
   -1 if resizing failed
 */
int checkbounds(sheet_t *sp, int row, int col) {
    if (row >= sp->maxrows || col >= sp->maxcols)
        return growtbl(sp, row, col) ? 1 : -1;
    else
        return 0;
}
