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

#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include "compat.h"
#include "sc.h"

/* scxrealloc will just scxmalloc if oldptr is == NULL */
#define GROWALLOC(newptr, oldptr, nelem, type, msg) \
    do { type *newptr = scxrealloc(oldptr, (nelem) * sizeof(type)); \
       if (newptr == NULL) { \
           error(msg); \
           return FALSE; \
       } \
       oldptr = newptr; \
    } while (0)

static const char nowider[] = "The table can't be any wider";

/*
 * grow the main && auxiliary tables (reset maxrows/maxcols as needed)
 * toprow &&/|| topcol tell us a better guess of how big to become.
 * we return TRUE if we could grow, FALSE if not....
 */
int growtbl(int rowcol, int toprow, int topcol)
{
    int newcols;

    (void)toprow; /* unused */
    newcols = maxcols;
    if (rowcol == GROWNEW) {
        newcols = MINCOLS;
        maxcols = topcol = 0;
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

    if ((rowcol == GROWCOL) || (rowcol == GROWBOTH) || (rowcol == GROWNEW)) {
        GROWALLOC(fwidth2, fwidth, newcols, int, nowider);
        GROWALLOC(precision2, precision, newcols, int, nowider);
        GROWALLOC(realfmt2, realfmt, newcols, int, nowider);

        memset(fwidth + maxcols, 0, (newcols - maxcols) * sizeof(*fwidth));
        memset(precision + maxcols, 0, (newcols - maxcols) * sizeof(*precision));
        memset(realfmt + maxcols, 0, (newcols - maxcols) * sizeof(*realfmt));
    }

    maxcols = newcols;
    return TRUE;
}
