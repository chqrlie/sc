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
 *              $Revision: 8.1 $
 */

#include "sc.h"

/*
 * check to see if *rowp && *colp are currently allocated, if not expand the
 * current size if we can.
 */

int checkbounds(sheet_t *sp, int *rowp, int *colp) {
    if (*rowp < 0)
        *rowp = 0;
    else
    if (*rowp >= sp->maxrows) {
        if (*colp >= sp->maxcols) {
            if (!growtbl(sp, GROWBOTH, *rowp, *colp)) {
                *rowp = sp->maxrows - 1;
                *colp = sp->maxcols - 1;
            }
            return 0;
        } else {
            if (!growtbl(sp, GROWROW, *rowp, 0))
                *rowp = sp->maxrows - 1;
            return 0;
        }
    }
    if (*colp < 0)
        *colp = 0;
    else
    if (*colp >= sp->maxcols) {
        if (!growtbl(sp, GROWCOL, 0, *colp))
            *colp = sp->maxcols - 1;
    }
    return 0;
}

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
int growtbl(sheet_t *sp, int mode, int toprow, int topcol) {
    int row, curcols, currows, newrows, newcols;

    newrows = currows = sp->maxrows;
    newcols = curcols = sp->maxcols;

    if (mode == GROWNEW) {
        /* when we first start up, fill the screen w/ cells */
        newrows = screen_LINES - RESROW;
        if (newrows < MINROWS) newrows = MINROWS;
        newcols = (screen_COLS - sp->rescol) / DEFWIDTH;
        if (newcols < MINCOLS) newcols = MINCOLS;
        currows = toprow = 0;
        curcols = topcol = 0;
    }

    /* set how much to grow */
    if (mode & GROWROW) {
        if (toprow >= sp->maxrows)
            newrows = toprow + GROWAMT;
        else
            newrows += GROWAMT;
    }

    if (mode & GROWCOL) {
        if ((mode == GROWCOL) && ((sp->maxcols == ABSMAXCOLS) || (topcol >= ABSMAXCOLS))) {
            error(nowider);
            return FALSE;
        }

        if (topcol >= sp->maxcols)
            newcols = topcol + GROWAMT;
        else
            newcols += GROWAMT;

        if (newcols > ABSMAXCOLS)
            newcols = ABSMAXCOLS;
    }

    if (newcols > curcols) {
        colfmt_t def_colfmt = { FALSE, DEFWIDTH, DEFPREC, DEFREFMT };
        GROWALLOC(sp->colfmt, curcols, newcols, nowider, def_colfmt);
        for (row = 0; row < currows; row++) {
            GROWALLOC(sp->tbl[row], curcols, newcols, nowider, NULL);
        }
    }

    if (newrows > currows) {
        rowfmt_t def_rowfmt = { FALSE };
        GROWALLOC(sp->rowfmt, currows, newrows, nolonger, def_rowfmt);
        GROWALLOC(sp->row_size, currows, newrows, nolonger, 0);
        GROWALLOC(sp->tbl, currows, newrows, nolonger, NULL);
        for (row = currows; row < newrows; row++) {
            GROWALLOC(sp->tbl[row], 0, newcols, nolonger, NULL);
        }
    }
    FullUpdate++;
    sp->maxrows = newrows;
    sp->maxcols = newcols;

    for (sp->rescol = 4; newrows >= 1000; sp->rescol++) {
        newrows /= 10;
    }
    return TRUE;
}
