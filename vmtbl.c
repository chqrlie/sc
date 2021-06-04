/*      SC      A Spreadsheet Calculator
 *              Spreadsheet 'tbl' creation
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              $Revision: 7.16 $
 *
 */

#include <unistd.h>
#include <limits.h>
#include "compat.h"
#include "sc.h"

/*
 * check to see if *rowp && *colp are currently allocated, if not expand the
 * current size if we can.
 */

void checkbounds(int *rowp, int *colp)
{
    if (*rowp < 0)
        *rowp = 0;
    else if (*rowp >= maxrows) {
        if (*colp >= maxcols) {
            if (!growtbl(GROWBOTH, *rowp, *colp)) {
                *rowp = maxrows - 1;
                *colp = maxcols - 1;
            }
            return;
        } else {
            if (!growtbl(GROWROW, *rowp, 0))
                *rowp = maxrows - 1;
            return;
        }
    }
    if (*colp < 0)
        *colp = 0;
    else if (*colp >= maxcols) {
        if (!growtbl(GROWCOL, 0, *colp))
            *colp = maxcols - 1;
    }
}

/* scxrealloc will just scxmalloc if oldptr is == NULL */
#define GROWALLOC(newptr, oldptr, nelem, type, msg)                 \
    do { type *newptr = scxrealloc(oldptr, (nelem) * sizeof(type)); \
       if (newptr == NULL) {                                        \
           error(msg);                                              \
           return FALSE;                                            \
       }                                                            \
       oldptr = newptr;                                             \
    } while (0)

static const char nolonger[] = "The table can't be any longer";
static const char nowider[] = "The table can't be any wider";

/*
 * grow the main && auxiliary tables (reset maxrows/maxcols as needed)
 * toprow &&/|| topcol tell us a better guess of how big to become.
 * we return TRUE if we could grow, FALSE if not....
 */
int growtbl(int rowcol, int toprow, int topcol)
{
    int newrows, newcols, row, col;
    struct ent **rowptr;

    newrows = maxrows;
    newcols = maxcols;

    if (rowcol == GROWNEW) {
        maxrows = toprow = 0;
        /* when we first start up, fill the screen w/ cells */
        {   int startval;
            startval = LINES - RESROW;
            newrows = startval > MINROWS ? startval : MINROWS;
            startval = ((COLS) - rescol) / DEFWIDTH;
            newcols = startval > MINCOLS ? startval : MINCOLS;
        }
        maxcols = topcol = 0;
    }

    /* set how much to grow */
    if (rowcol & GROWROW) {
        if (toprow > maxrows)
            newrows = GROWAMT + toprow;
        else
            newrows += GROWAMT;
    }

    if (rowcol & GROWCOL) {
        if ((rowcol == GROWCOL) && ((maxcols == ABSMAXCOLS) || (topcol >= ABSMAXCOLS))) {
            error(nowider);
            return FALSE;
        }

        if (topcol > maxcols)
            newcols = GROWAMT + topcol;
        else
            newcols += GROWAMT;

        if (newcols > ABSMAXCOLS)
            newcols = ABSMAXCOLS;
    }

    if (newrows > maxrows) {
        GROWALLOC(row_hidden2, row_hidden, newrows, char, nolonger);
        memset(row_hidden + maxrows, 0, (newrows - maxrows) * sizeof(*row_hidden));
        GROWALLOC(tbl2, tbl, newrows, struct ent **, nolonger);
        for (row = maxrows; row < newrows; row++) {
            tbl[row] = NULL;
        }
    }

    if (newcols > maxcols) {
        GROWALLOC(fwidth2, fwidth, newcols, int, nowider);
        GROWALLOC(precision2, precision, newcols, int, nowider);
        GROWALLOC(realfmt2, realfmt, newcols, int, nowider);
        GROWALLOC(col_hidden2, col_hidden, newcols, char, nowider);
        for (col = maxcols; col < newcols; col++) {
            col_hidden[col] = 0;
            fwidth[col] = DEFWIDTH;
            precision[col] = DEFPREC;
            realfmt[col] = DEFREFMT;
        }

        /* [re]alloc the space for each row */
        for (row = 0; row < maxrows; row++) {
            if ((tbl[row] = scxrealloc(tbl[row], sizeof(*tbl[row]) * newcols)) == NULL) {
                error(nowider);
                return FALSE;
            }
            for (rowptr = tbl[row], col = maxcols; col < maxcols; col++)
                rowptr[col] = NULL;
        }
    }

    /* fill in the bottom of the table */
    for (row = maxrows; row < newrows; row++) {
        if ((tbl[row] = scxmalloc((sizeof(*tbl[row]) * newcols))) == NULL) {
            error(nowider);
            return FALSE;
        }
        for (rowptr = tbl[row], col = 0; col < newcols; col++)
            rowptr[col] = NULL;
    }

    FullUpdate++;
    maxrows = newrows;
    maxcols = newcols;

    if (maxrows > 1000) rescol = 5;
    if (maxrows > 10000) rescol = 6;

    return TRUE;
}
