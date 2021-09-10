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
#define GROWALLOC(ptr, nelem, type, msg)                           \
    do { type *newptr__ = scxrealloc(ptr, (nelem) * sizeof(type)); \
       if (newptr__ == NULL) {                                     \
           error(msg);                                             \
           return FALSE;                                           \
       }                                                           \
       ptr = newptr__;                                             \
    } while (0)

static const char nolonger[] = "The table cannot be any longer";
static const char nowider[] = "The table cannot be any wider";

/*
 * grow the main && auxiliary tables (update maxrows/maxcols as needed)
 * toprow &&/|| topcol tell us a better guess of how big to become.
 * we return TRUE if we could grow, FALSE if not....
 */
int growtbl(sheet_t *sp, int mode, int toprow, int topcol) {
    int row, col, curcols, currows, newrows, newcols;

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

    if (newrows > currows) {
        GROWALLOC(sp->row_hidden, newrows, unsigned char, nolonger);
        memzero(sp->row_hidden + currows, (newrows - currows) * sizeof(*sp->row_hidden));
        GROWALLOC(sp->tbl, newrows, struct ent **, nolonger);
        for (row = currows; row < newrows; row++) {
            sp->tbl[row] = NULL;
        }
    }

    if (newcols > curcols) {
        GROWALLOC(sp->fwidth, newcols, int, nowider);
        GROWALLOC(sp->precision, newcols, int, nowider);
        GROWALLOC(sp->realfmt, newcols, int, nowider);
        GROWALLOC(sp->col_hidden, newcols, unsigned char, nowider);
        for (col = curcols; col < newcols; col++) {
            sp->fwidth[col] = DEFWIDTH;
            sp->precision[col] = DEFPREC;
            sp->realfmt[col] = DEFREFMT;
            sp->col_hidden[col] = 0;
        }

        /* [re]alloc the space for each row */
        for (row = 0; row < currows; row++) {
            struct ent **rowptr = scxrealloc(sp->tbl[row], sizeof(*sp->tbl[row]) * newcols);
            if (rowptr == NULL) {
                error(nowider);
                return FALSE;
            }
            sp->tbl[row] = rowptr;
            for (col = curcols; col < newcols; col++)
                rowptr[col] = NULL;
        }
    }

    /* fill in the bottom of the table */
    for (row = currows; row < newrows; row++) {
        struct ent **rowptr = scxmalloc(sizeof(*sp->tbl[row]) * newcols);
        if (rowptr == NULL) {
            error(nolonger);
            return FALSE;
        }
        sp->tbl[row] = rowptr;
        for (col = 0; col < newcols; col++)
            rowptr[col] = NULL;
    }

    FullUpdate++;
    sp->maxrows = newrows;
    sp->maxcols = newcols;

    for (sp->rescol = 4; newrows >= 1000; sp->rescol++) {
        newrows /= 10;
    }
    return TRUE;
}
