/*      SC      A Spreadsheet Calculator
 *              Command routines
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <sys/wait.h>
#include <time.h>
#include <utime.h>
#include <sys/file.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <signal.h>
#include "sc.h"

static void syncref(struct enode *e);
static void unspecial(FILE *f, char *str, int delim);
static struct ent *deldata1(void);
static void deldata2(struct ent *obuf);
static int any_locked_cells(int r1, int c1, int r2, int c2);

#define DEFCOLDELIM ':'

int macrofd;
int cslop;
static struct impexfilt *filt = NULL; /* root of list of impex filters */

/* return a pointer to a cell's [struct ent *], creating if needed */
struct ent *lookat(int row, int col) {
    struct ent **pp;
    struct ent *p;

    checkbounds(&row, &col);
    pp = ATBL(tbl, row, col);
    if (*pp == NULL) {
        if ((p = freeents) != NULL) {
            freeents = p->next;
        } else {
            p = scxmalloc(sizeof(struct ent));
        }
        if (row > maxrow) maxrow = row;
        if (col > maxcol) maxcol = col;
        p->v = 0.0;
        p->label = NULL;
        p->expr = NULL;
        p->format = NULL;
        p->cellerror = CELLOK;
        p->flags = MAY_SYNC;
        p->row = row;
        p->col = col;
        p->nrow = -1;
        p->ncol = -1;
        p->nlastrow = -1;
        p->nlastcol = -1;
        p->next = NULL;
        *pp = p;
    }
    return *pp;
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
void free_ent(struct ent *p, int unlock) {
    p->next = delbuf[dbidx];
    delbuf[dbidx] = p;
    p->flags |= IS_DELETED;
    if (unlock)
        p->flags &= ~IS_LOCKED;
}

/* free deleted cells */
void flush_saved(void) {
    struct ent *p;
    struct ent *q;

    if (dbidx < 0)
        return;

    if ((p = delbuf[dbidx])) {
        scxfree(delbuffmt[dbidx]);
        delbuffmt[dbidx] = NULL;
    }
    while (p) {
        clearent(p);
        q = p->next;
        p->next = freeents;     /* put this ent on the front of freeents */
        freeents = p;
        p = q;
    }
    delbuf[dbidx--] = NULL;
}

/* copy the current row (currow) and place the cursor in the new row */
void duprow(void) {
    int c1, c2, coltmp = curcol;
    struct frange *fr;

    fr = find_frange(currow, curcol);
    c1 = fr ? fr->or_left->col : 0;
    c2 = fr ? fr->or_right->col : maxcol;

    if (currow >= maxrows - 1 || (!fr && maxrow >= maxrows - 1) ||
        (fr && fr->or_right->row >= maxrows - 1)) {
        if (!growtbl(GROWROW, 0, 0))
            return;
    }
    modflg++;
    insertrow(1, 1);
    for (curcol = c1; curcol <= c2; curcol++) {
        struct ent *p = *ATBL(tbl, currow - 1, curcol);
        if (p) {
            struct ent *n = lookat(currow, curcol);
            copyent(n, p, 1, 0, 0, 0, maxrow, maxcol, 0);
        }
    }
    curcol = coltmp;
}

/* copy the current column (curcol) and place the cursor in the new column */
void dupcol(void) {
    int rowtmp = currow;

    if (curcol >= maxcols - 1 || maxcol >= maxcols - 1) {
        if (!growtbl(GROWCOL, 0, 0))
            return;
    }
    modflg++;
    insertcol(1, 1);
    for (currow = 0; currow <= maxrow; currow++) {
        struct ent *p = *ATBL(tbl, currow, curcol - 1);
        if (p) {
            struct ent *n = lookat(currow, curcol);
            copyent(n, p, 0, 1, 0, 0, maxrow, maxcol, 0);
        }
    }
    currow = rowtmp;
}

/* Insert 'arg' rows.  The row(s) will be inserted before currow if delta
 * is 0; after if it is 1.
 */
void insertrow(int arg, int delta) {
    int r, c, i;
    struct ent **tmprow;
    int lim = maxrow - currow + 1;
    struct frange *fr;

    if (currow > maxrow)
        maxrow = currow;
    maxrow += arg;
    lim = maxrow - lim + delta;

    if ((maxrow >= maxrows) && !growtbl(GROWROW, maxrow, 0))
        return;

    if ((fr = find_frange(currow + delta, curcol))) {
        move_area(currow + arg + delta, fr->or_left->col, currow + delta,
                  fr->or_left->col, fr->or_right->row, fr->or_right->col);
        if (!delta && fr->ir_left->row == currow + arg)
            fr->ir_left = lookat(fr->ir_left->row - arg, fr->ir_left->col);
        if (delta && fr->ir_right->row == currow)
            fr->ir_right = lookat(fr->ir_right->row + arg, fr->ir_right->col);

        for (i = 0; i < 37; i++) {      /* update all marked cells */
            if (savedrow[i] >= currow + delta &&
                savedcol[i] >= fr->or_left->col &&
                savedcol[i] <= fr->or_right->col)
                savedrow[i] += arg;
            if (savedstrow[i] >= currow + delta &&
                savedstcol[i] >= fr->or_left->col &&
                savedstcol[i] <= fr->or_right->col)
                savedstrow[i] += arg;
        }
        if (gs.g_row >= currow + delta &&
            gs.g_col >= fr->or_left->col &&
            gs.g_col <= fr->or_right->col)
            gs.g_row += arg;
        if (gs.g_lastrow >= currow + delta &&
            gs.g_lastcol >= fr->or_left->col &&
            gs.g_lastcol <= fr->or_right->col)
            gs.g_lastrow += arg;
        if (gs.strow >= currow + delta &&
            gs.stcol >= fr->or_left->col &&
            gs.stcol <= fr->or_right->col)
            gs.strow += arg;
        for (r = 0; r <= maxrow; r++) {
            for (c = 0; c <= maxcol; c++) {
                struct ent *p = *ATBL(tbl, r, c);
                if (p) {
                    if (p->nrow >= currow + delta &&
                        p->ncol >= fr->or_left->col &&
                        p->ncol <= fr->or_right->col)
                        p->nrow += arg;
                    if (p->nlastrow >= currow + delta &&
                        p->nlastcol >= fr->or_left->col &&
                        p->nlastcol <= fr->or_right->col)
                        p->nlastrow += arg;
                }
            }
        }
    } else {
        /*
         * save the last active row+1, shift the rows downward, put the last
         * row in place of the first
         */
        tmprow = tbl[maxrow];
        for (r = maxrow; r > lim; r--) {
            row_hidden[r] = row_hidden[r-arg];
            row_hidden[r-arg] = row_hidden[r-1];
            tbl[r] = tbl[r-arg];
            tbl[r-arg] = tbl[r-1];
            for (c = 0; c < maxcols; c++) {
                struct ent *p = *ATBL(tbl, r, c);
                if (p)
                    p->row = r;
            }
        }
        tbl[r] = tmprow;                /* the last row was never used.... */

        for (i = 0; i < 37; i++) {      /* update all marked cells */
            if (savedrow[i] >= currow + delta)
                savedrow[i] += arg;
            if (savedstrow[i] >= currow + delta)
                savedstrow[i] += arg;
        }
        if (gs.g_row >= currow + delta)
            gs.g_row += arg;
        if (gs.g_lastrow >= currow + delta)
            gs.g_lastrow += arg;
        if (gs.strow >= currow + delta)
            gs.strow += arg;
        for (r = 0; r <= maxrow; r++) {
            for (c = 0; c <= maxcol; c++) {
                struct ent *p = *ATBL(tbl, r, c);
                if (p) {
                    if (p->nrow >= currow + delta)
                        p->nrow += arg;
                    if (p->nlastrow >= currow + delta)
                        p->nlastrow += arg;
                }
            }
        }
    }
    fix_ranges(currow + arg * (1 - delta), -1, currow + arg * (1 - delta), -1,
               delta ? 0 : arg, delta ? arg : 0);
    currow += delta;
    FullUpdate++;
    modflg++;
}

/* Insert 'arg' cols.  The col(s) will be inserted before curcol if delta
 * is 0; after if it is 1.
 */
void insertcol(int arg, int delta) {
    int r, c;
    struct ent **pp;
    int lim = maxcol - curcol - delta + 1;
    struct frange *fr;

    if (curcol + delta > maxcol)
        maxcol = curcol + delta;
    maxcol += arg;

    if ((maxcol >= maxcols) && !growtbl(GROWCOL, 0, maxcol))
        return;

    for (c = maxcol; c >= curcol + delta + arg; c--) {
        fwidth[c] = fwidth[c-arg];
        precision[c] = precision[c-arg];
        realfmt[c] = realfmt[c-arg];
        col_hidden[c] = col_hidden[c-arg];
    }
    for (c = curcol + delta; c - curcol - delta < arg; c++) {
        fwidth[c] = DEFWIDTH;
        precision[c] =  DEFPREC;
        realfmt[c] = DEFREFMT;
        col_hidden[c] = FALSE;
    }

    // XXX: this loop is confusing
    for (r = 0; r <= maxrow; r++) {
        pp = ATBL(tbl, r, maxcol);
        for (c = lim; --c >= 0; pp--) {
            if ((*pp = pp[-arg]))
                (*pp)->col += arg;
        }
        pp = ATBL(tbl, r, curcol + delta);
        for (c = curcol + delta; c - curcol - delta < arg; c++, pp++)
            *pp = NULL;
    }

    /* Update all marked cells. */
    for (c = 0; c < 37; c++) {
        if (savedcol[c] >= curcol + delta)
            savedcol[c] += arg;
        if (savedstcol[c] >= curcol + delta)
            savedstcol[c] += arg;
    }
    if (gs.g_col >= curcol + delta)
        gs.g_col += arg;
    if (gs.g_lastcol >= curcol + delta)
        gs.g_lastcol += arg;
    if (gs.stcol >= curcol + delta)
        gs.stcol += arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p) {
                if (p->ncol >= curcol + delta)
                    p->ncol += arg;
                if (p->nlastcol >= curcol + delta)
                    p->nlastcol += arg;
            }
        }
    }
    if (!delta && (fr = find_frange(currow, curcol)) &&
            fr->ir_left->col == curcol + arg)
        fr->ir_left = lookat(fr->ir_left->row, fr->ir_left->col - arg);
    if (delta && (fr = find_frange(currow, curcol)) &&
            fr->ir_right->col == curcol)
        fr->ir_right = lookat(fr->ir_right->row, fr->ir_right->col + arg);
    fix_ranges(-1, curcol + arg * (1 - delta), -1, curcol + arg * (1 - delta),
               delta ? 0 : arg, delta ? arg : 0);
    curcol += delta;
    FullUpdate++;
    modflg++;
}

/* delete rows starting at r1 up to and including r2 */
void deleterows(int r1, int r2) {
    int nrows, i, save = currow;
    struct frange *fr;
    struct ent *obuf = NULL;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r2 > maxrow)
        r2 = maxrow;
    if (r1 > maxrow) {
        /* deleting rows beyond the active area: nothing to do */
        return;
    }

    currow = r1;
    nrows = r2 - r1 + 1;

    if ((fr = find_frange(r1, curcol))) {
        if (any_locked_cells(r1, fr->or_left->col,
                             r2, fr->or_right->col)) {
            error("Locked cells encountered. Nothing changed");
        } else {
            FullUpdate++;
            modflg++;
            obuf = deldata1();
            erase_area(r1, fr->or_left->col, r2, fr->or_right->col, 0);
            fix_ranges(r1, -1, r2, -1, -1, -1);
            deldata2(obuf);
            if (r1 + nrows > fr->ir_right->row && fr->ir_right->row >= r1)
                fr->ir_right = lookat(r1 - 1, fr->ir_right->col);
            if (r1 + nrows > fr->or_right->row)
                fr->or_right = lookat(r1 - 1, fr->or_right->col);
            else
                move_area(r1, fr->or_left->col,
                          r1 + nrows, fr->or_left->col,
                          fr->or_right->row, fr->or_right->col);
            if (fr->ir_left->row > fr->ir_right->row)
                add_frange(fr->or_left, fr->or_right, NULL, NULL, 0, 0, 0, 0);

            /* Update all marked cells. */
            for (i = 0; i < 37; i++) {
                if (savedcol[i] >= fr->or_left->col &&
                    savedcol[i] <= fr->or_right->col) {
                    if (savedrow[i] >= r1 && savedrow[i] <= r2)
                        savedrow[i] = savedcol[i] = -1;
                    if (savedrow[i] > r2)
                        savedrow[i] -= nrows;
                }
                if (savedstcol[i] >= fr->or_left->col &&
                    savedstcol[i] <= fr->or_right->col) {
                    if (savedstrow[i] >= r1 && savedstrow[i] <= r2)
                        savedstrow[i] = r1;
                    if (savedstrow[i] > r2)
                        savedstrow[i] -= nrows;
                }
            }
            if (gs.g_col >= fr->or_left->col && gs.g_col <= fr->or_right->col) {
                if (gs.g_row >= r1 && gs.g_row <= r2)
                    gs.g_row = r1;
                if (gs.g_row > r2)
                    gs.g_row -= nrows;
            }
            if (gs.g_lastcol >= fr->or_left->col && gs.g_lastcol <= fr->or_right->col) {
                if (gs.g_lastrow >= r1 && gs.g_lastrow <= r2)
                    gs.g_lastrow = r1 - 1;
                if (gs.g_lastrow > r2)
                    gs.g_lastrow -= nrows;
            }
            if (gs.g_row > gs.g_lastrow)
                gs.g_row = gs.g_col = -1;
            if (gs.stcol >= fr->or_left->col && gs.stcol <= fr->or_right->col) {
                if (gs.strow >= r1 && gs.strow <= r2)
                    gs.strow = r1;
                if (gs.strow > r2)
                    gs.strow -= nrows;
            }
        }
    } else {
        if (any_locked_cells(r1, 0, r2, maxcol)) {
            error("Locked cells encountered. Nothing changed");
        } else {
            obuf = deldata1();
            erase_area(r1, 0, r2, maxcol, 0);
            fix_ranges(r1, -1, r2, -1, -1, -1);
            closerow(r1, nrows);
            deldata2(obuf);
        }
    }
    currow = save < r1 ? save : (save <= r2) ? r1 : save - nrows;
}

static struct ent *deldata1(void) {
    int i;
    struct ent *obuf = NULL;
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 1];
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 1];
    delbuf[DELBUFSIZE - 1] = NULL;
    delbuffmt[DELBUFSIZE - 1] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    flush_saved();
    if (qbuf) {
        if (dbidx < 0) dbidx++;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        flush_saved();
        obuf = delbuf[qbuf];    /* orig. contents of the del. buffer */
    }
    sync_refs();
    return obuf;
}

static void deldata2(struct ent *obuf) {
    int i;
    struct ent *p;
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
    for (i = DELBUFSIZE - 1; i > DELBUFSIZE - 9; i--) {
        delbuf[i] = delbuf[i-1];
        delbuffmt[i] = delbuffmt[i-1];
    }
    delbuf[DELBUFSIZE - 9] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 9] = delbuffmt[dbidx];
    for (p = delbuf[dbidx]; p; p = p->next)
        p->flags &= ~MAY_SYNC;
}

static void yankrow(int arg) {
    int rs = maxrow - currow + 1;
    int i, qtmp;
    struct frange *fr;
    struct ent *obuf = NULL;

    if ((fr = find_frange(currow, curcol)))
        rs = fr->or_right->row - currow + 1;
    if (rs - arg < 0) {
        rs = rs > 0 ? rs : 0;
        error("Can't yank %d row%s %d row%s left", arg,
              (arg != 1 ? "s," : ","), rs, (rs != 1 ? "s" : ""));
        return;
    }
    sync_refs();
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[DELBUFSIZE - 10] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    flush_saved();
    if (qbuf) {
        if (dbidx < 0) dbidx++;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        flush_saved();
        obuf = delbuf[qbuf];    /* orig. contents of the del. buffer */
    }
    qtmp = qbuf;
    qbuf = 0;
    if (fr) {
        yank_area(currow, fr->or_left->col, currow + arg - 1,
                  fr->or_right->col);
    } else {
        yank_area(currow, 0, currow + arg - 1, maxcol);
    }
    qbuf = qtmp;
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
    delbuf[DELBUFSIZE - 10] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 10] = delbuffmt[dbidx];
}

void yankrows(int r1, int r2) {
    int r = currow, a;
    if (r1 < r2) {
        currow = r1;
        a = r2 - r1 + 1;
    } else {
        currow = r2;
        a = r1 - r2 + 1;
    }
    yankrow(a);
    currow = r;
}

static void yankcol(int arg) {
    int cs = maxcol - curcol + 1;
    int i, qtmp;
    struct ent *obuf = NULL;

    if (cs - arg < 0) {
        cs = cs > 0 ? cs : 0;
        error("Can't yank %d column%s %d column%s left",
              arg, (arg != 1 ? "s," : ","), cs, (cs != 1 ? "s" : ""));
        return;
    }
    sync_refs();
    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[DELBUFSIZE - 10] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    flush_saved();
    if (qbuf) {
        if (dbidx < 0) dbidx++;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        flush_saved();
        obuf = delbuf[qbuf];    /* orig. contents of the del. buffer */
    }
    qtmp = qbuf;
    qbuf = 0;
    yank_area(0, curcol, maxrow, curcol + arg - 1);
    qbuf = qtmp;
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
    delbuf[DELBUFSIZE - 10] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 10] = delbuffmt[dbidx];
}

void yankcols(int c1, int c2) {
    int c = curcol, a;
    if (c1 < c2) {
        curcol = c1;
        a = c2 - c1 + 1;
    } else {
        curcol = c2;
        a = c1 - c2 + 1;
    }
    yankcol(a);
    curcol = c;
}

/* ignorelock is used when sorting so that locked cells can still be sorted */

void erase_area(int sr, int sc, int er, int ec, int ignorelock) {
    int r, c;
    struct ent **pp;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    if (sr < 0) sr = 0;
    if (sc < 0) sc = 0;
    checkbounds(&er, &ec);

    /* Do a lookat() for the upper left and lower right cells of the range
     * being erased to make sure they are included in the delete buffer so
     * that pulling cells always works correctly even if the cells at one
     * or more edges of the range are all empty.
     */
    lookat(sr, sc);
    lookat(er, ec);

    delbuffmt[++dbidx] = scxmalloc((4*(ec-sc+1)+(er-sr+1))*sizeof(char));
    for (c = sc; c <= ec; c++) {
        delbuffmt[dbidx][4*(c-sc)+0] = (char)fwidth[c];
        delbuffmt[dbidx][4*(c-sc)+1] = (char)precision[c];
        delbuffmt[dbidx][4*(c-sc)+2] = (char)realfmt[c];
        delbuffmt[dbidx][4*(c-sc)+3] = (char)col_hidden[c];
    }
    for (r = sr; r <= er; r++) {
        for (c = sc; c <= ec; c++) {
            pp = ATBL(tbl, r, c);
            if (*pp && (!((*pp)->flags & IS_LOCKED) || ignorelock)) {
                free_ent(*pp, 0);
                *pp = NULL;
            }
        }
        delbuffmt[dbidx][4*(ec-sc+1)+(r-sr)] = (char)row_hidden[r];
    }
}

void yank_area(int sr, int sc, int er, int ec) {
    int r, c;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    if (sr < 0) sr = 0;
    if (sc < 0) sc = 0;
    checkbounds(&er, &ec);

    r = currow;
    currow = sr;
    c = curcol;
    curcol = sc;

    erase_area(sr, sc, er, ec, 0);
    pullcells('p');

    currow = r;
    curcol = c;
}

void move_area(int dr, int dc, int sr, int sc, int er, int ec) {
    struct ent *p;
    struct ent **pp;
    int deltar, deltac;
    int r, c;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    if (sr < 0) sr = 0;
    if (sc < 0) sc = 0;
    checkbounds(&er, &ec);

    r = currow;
    currow = sr;
    c = curcol;
    curcol = sc;

    /* First we erase the source range, which puts the cells on the delete
     * buffer stack.
     */
    erase_area(sr, sc, er, ec, 0);

    currow = r;
    curcol = c;
    deltar = dr - sr;
    deltac = dc - sc;

    /* Now we erase the destination range, which adds it to the delete buffer
     * stack, but then we flush it off.  We then move the original source
     * range from the stack to the destination range, adjusting the addresses
     * as we go, leaving the stack in its original state.
     */
    erase_area(dr, dc, er + deltar, ec + deltac, 0);
    flush_saved();
    for (p = delbuf[dbidx]; p; p = p->next) {
        pp = ATBL(tbl, p->row + deltar, p->col + deltac);
        *pp = p;
        p->row += deltar;
        p->col += deltac;
        p->flags &= ~IS_DELETED;
    }
    delbuf[dbidx] = NULL;
    delbuffmt[dbidx--] = NULL;
}

/*
 * deletes the expression associated w/ a cell and turns it into a constant
 * containing whatever was on the screen
 */
void valueize_area(int sr, int sc, int er, int ec) {
    int r, c;
    struct ent *p;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    if (sr < 0) sr = 0;
    if (sc < 0) sc = 0;
    checkbounds(&er, &ec);

    for (r = sr; r <= er; r++) {
        for (c = sc; c <= ec; c++) {
            p = *ATBL(tbl, r, c);
            if (p) {
                if (p->flags & IS_LOCKED) {
                    error(" Cell %s%d is locked", coltoa(c), r);
                    continue;
                }
                if (p->expr) {
                    efree(p->expr);
                    p->expr = NULL;
                    p->flags &= ~IS_STREXPR;
                }
            }
        }
    }
    modflg++;  // XXX: should be done only upon modification
}

void pullcells(int to_insert) {
    struct ent *obuf;
    struct ent *p, *n;
    struct ent **pp;
    int deltar, deltac;
    int minrow, mincol;
    int mxrow, mxcol;
    int numrows, numcols;
    int i;
    struct frange *fr;

    if (qbuf && delbuf[qbuf]) {
        delbuf[++dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
    }
    obuf = delbuf[dbidx];       /* orig. contents of the del. buffer */

    if ((qbuf && !delbuf[qbuf]) || dbidx < 0) {
        error("No data to pull");
        qbuf = 0;
        return;
    }

    minrow = maxrows;
    mincol = maxcols;
    mxrow = 0;
    mxcol = 0;

    for (p = delbuf[dbidx]; p; p = p->next) {
        if (p->row < minrow)
            minrow = p->row;
        if (p->row > mxrow)
            mxrow = p->row;
        if (p->col < mincol)
            mincol = p->col;
        if (p->col > mxcol)
            mxcol = p->col;
        p->flags |= MAY_SYNC;
    }

    numrows = mxrow - minrow + 1;
    numcols = mxcol - mincol + 1;
    deltar = currow - minrow;
    deltac = curcol - mincol;

    if (to_insert == 'C') {
        minrow = 0;
        mincol = 0;
        mxrow = maxrows;
        mxcol = maxcols;
    }

    if (to_insert == 'r') {
        insertrow(numrows, 0);
        if ((fr = find_frange(currow, curcol))) {
            deltac = fr->or_left->col - mincol;
        } else {
            for (i = 0; i < numrows; i++)
                row_hidden[currow+i] = delbuffmt[dbidx][4*numcols+i];
            deltac = 0;
        }
    } else
    if (to_insert == 'c') {
        insertcol(numcols, 0);
        for (i = 0; i < numcols; i++) {
            fwidth[curcol+i] = delbuffmt[dbidx][4*i];
            precision[curcol+i] = delbuffmt[dbidx][4*i+1];
            realfmt[curcol+i] = delbuffmt[dbidx][4*i+2];
            col_hidden[curcol+i] = delbuffmt[dbidx][4*i+3];
        }
        deltar = 0;
    } else
    if (to_insert == 'x') {      /* Do an exchange. */
        struct ent *tmpbuf;
        char *tmpfmt;

        /* Save the original contents of the destination range on the
         * delete buffer stack in preparation for the exchange, then swap
         * the top two pointers on the stack, so that the original cells
         * to be pulled are still on top.
         */
        erase_area(minrow + deltar, mincol + deltac,
                   mxrow + deltar, mxcol + deltac, 0);
        tmpbuf = delbuf[dbidx];
        delbuf[dbidx] = delbuf[dbidx - 1];
        delbuf[dbidx - 1] = tmpbuf;
        tmpfmt = delbuffmt[dbidx];
        delbuffmt[dbidx] = delbuffmt[dbidx - 1];
        delbuffmt[dbidx - 1] = tmpfmt;
    } else
    if (to_insert == 'p') {
        erase_area(minrow + deltar, mincol + deltac,
                   mxrow + deltar, mxcol + deltac, 0);
        sync_refs();
        flush_saved();
    } else
    if (to_insert == 't') {
        erase_area(minrow + deltar, mincol + deltac,
                   minrow + deltar + mxcol - mincol,
                   mincol + deltac + mxrow - minrow, 0);
        sync_refs();
        flush_saved();
    }

    FullUpdate++;
    modflg++;

    /* At this point, we copy the cells from the delete buffer into the
     * destination range.
     */
    for (p = delbuf[dbidx]; p; p = p->next) {
        if (to_insert == 't') {   /* Transpose rows and columns while pulling. */
            n = lookat(minrow + deltar + p->col - mincol,
                       mincol + deltac + p->row - minrow);
        } else {
            n = lookat(p->row + deltar, p->col + deltac);
        }
        copyent(n, p, deltar, deltac, minrow, mincol, mxrow, mxcol, to_insert);
    }

    /* Now exchange them so that the original cells from the delete buffer
     * are in the destination range instead of the copies.  When doing a
     * "pull exchange" ("px" or "pullxchg"), exchange the original contents
     * of the destination range with the contents of the delete buffer
     * instead.  Don't do this if transposing or merging (including merging
     * cell formats), or if the expressions in the destination cells have
     * been adjusted during a copy.
     */
    if (to_insert != 't' && to_insert != 'm' && to_insert != 'f' && to_insert != 'C') {
        if (to_insert == 'x') {
            struct ent *tmpbuf = delbuf[dbidx];
            char *tmpfmt = delbuffmt[dbidx];

            delbuf[dbidx] = delbuf[dbidx - 1];
            delbuf[dbidx - 1] = tmpbuf;
            delbuffmt[dbidx] = delbuffmt[dbidx - 1];
            delbuffmt[dbidx - 1] = tmpfmt;
        } else {
            for (p = delbuf[dbidx++]; p; p = p->next) {
                pp = ATBL(tbl, p->row + deltar, p->col + deltac);
                if (*pp && !((*pp)->flags & IS_LOCKED)) {
                    free_ent(*pp, 1);
                    *pp = NULL;
                }
            }
        }
        for (p = delbuf[dbidx - 1]; p; p = p->next) {
            pp = ATBL(tbl, p->row + deltar, p->col + deltac);
            // XXX: bug if (*pp && !((*pp)->flags & IS_LOCKED))
            *pp = p;
            p->row += deltar;
            p->col += deltac;
            p->flags &= ~IS_DELETED;
        }
        delbuf[dbidx - 1] = delbuf[dbidx];
        delbuf[dbidx--] = NULL;

        sync_refs();
        /*
         * Now change the cell addresses in the delete buffer to match
         * where the original cells came from.
         */
        for (p = delbuf[dbidx]; p; p = p->next) {
            p->row -= deltar;
            p->col -= deltac;
        }
    } else {
        sync_refs();
    }

    /* Now make sure all references to the pulled cells in all named buffers
     * point to the new set of cells in the delete buffer.
     */
    for (i = 0; i < DELBUFSIZE; i++) {
        if (delbuf[i] == obuf) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    if (qbuf && delbuf[qbuf]) {
        delbuf[dbidx] = NULL;
        delbuffmt[dbidx--] = NULL;
    }
    qbuf = 0;
}

/* delete numrow rows, starting with rs */
void closerow(int rs, int numrow) {
    struct ent **pp;
    int r, c, i;
    struct ent **tmprow;

    if (rs + numrow - 1 > maxrow) return;
    r = rs;

    /*
     * Rows are dealt with in numrow groups, each group of rows spaced numrow
     * rows apart.
     */
    for (i = 0; i < numrow; i++) {
        r = rs + i;

        /* save the first row of the group and empty it out */
        tmprow = tbl[r];
        pp = ATBL(tbl, r, 0);
        for (c = maxcol + 1; c --> 0; pp++) {
            if (*pp) {
                free_ent(*pp, 1);
                *pp = NULL;
            }
        }

        /* move the rows, put the deleted, but now empty, row at the end */
        for (; r + numrow < maxrows - 1; r += numrow) {
            row_hidden[r] = row_hidden[r + numrow];
            tbl[r] = tbl[r + numrow];
            for (c = 0; c < maxcols; c++) {
                struct ent *p = *ATBL(tbl, r, c);
                if (p)
                    p->row = r;
            }
        }
        tbl[r] = tmprow;
    }

    /* Update all marked cells. */
    for (i = 0; i < 37; i++) {
        if (savedrow[i] >= rs && savedrow[i] < rs + numrow)
            savedrow[i] = savedcol[i] = -1;
        if (savedrow[i] >= rs + numrow)
            savedrow[i] -= numrow;
        if (savedstrow[i] >= rs && savedstrow[i] < rs + numrow)
            savedstrow[i] = rs;
        if (savedstrow[i] >= rs + numrow)
            savedstrow[i] -= numrow;
    }
    if (gs.g_row >= rs && gs.g_row < rs + numrow)
        gs.g_row = rs;
    if (gs.g_row >= rs + numrow)
        gs.g_row -= numrow;
    if (gs.g_lastrow >= rs && gs.g_lastrow < rs + numrow)
        gs.g_lastrow = rs - 1;
    if (gs.g_lastrow >= rs + numrow)
        gs.g_lastrow -= numrow;
    if (gs.g_row > gs.g_lastrow)
        gs.g_row = gs.g_col = -1;
    if (gs.strow >= rs && gs.strow < rs + numrow)
        gs.strow = rs;
    if (gs.strow >= rs + numrow)
        gs.strow -= numrow;

    maxrow -= numrow;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p) {
                if (p->nrow >= rs && p->nrow < rs + numrow)
                    p->nrow = rs;
                if (p->nrow >= rs + numrow)
                    p->nrow -= numrow;
                if (p->nlastrow >= rs && p->nlastrow < rs + numrow)
                    p->nlastrow = rs - 1;
                if (p->nlastrow >= rs + numrow)
                    p->nlastrow -= numrow;
                if (p->nlastrow < p->nrow)
                    p->nrow = p->ncol = -1;
            }
        }
    }
    FullUpdate++;
    modflg++;
}

/* delete group of columns (1 or more) */
void deletecols(int c1, int c2) {
    int r, c, ncols, i, save = curcol;
    struct ent **pp;
    struct ent *p;
    struct ent *obuf = NULL;

    if (c1 > c2) SWAPINT(c1, c2);
    if (c2 > maxcol)
        c2 = maxcol;
    if (c1 > maxcol) {
        /* deleting columns beyond the active area: nothing to do */
        return;
    }

    if (any_locked_cells(0, c1, maxrow, c2)) {
        error("Locked cells encountered. Nothing changed");
        return;
    }
    ncols = c2 - c1 + 1;
    curcol = c1;

    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 1];
    delbuf[DELBUFSIZE - 1] = NULL;
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 1];
    delbuffmt[DELBUFSIZE - 1] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    flush_saved();
    if (qbuf) {
        if (dbidx < 0) dbidx++;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        flush_saved();
        obuf = delbuf[qbuf];    /* orig. contents of the del. buffer */
    }
    sync_refs();
    erase_area(0, c1, maxrow, c2, 0);
    fix_ranges(-1, c1, -1, c2, -1, -1);
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((obuf && delbuf[i] == obuf) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
    for (i = DELBUFSIZE - 1; i > DELBUFSIZE - 9; i--) {
        delbuf[i] = delbuf[i-1];
        delbuffmt[i] = delbuffmt[i-1];
    }
    delbuf[DELBUFSIZE - 9] = delbuf[dbidx];
    delbuffmt[DELBUFSIZE - 9] = delbuffmt[dbidx];
    for (p = delbuf[dbidx]; p; p = p->next)
        p->flags &= ~MAY_SYNC;

    /* clear then copy the block left */
    for (r = 0; r <= maxrow; r++) {
        for (c = c1; c <= c2; c++) {
            pp = ATBL(tbl, r, c);
            if (*pp) {
                free_ent(*pp, 1);
                *pp = NULL;
            }
        }
        for (c = c2; c <= maxcol; c++) {
            pp = ATBL(tbl, r, c);
            if ((*pp = pp[ncols])) {
                (*pp)->col -= ncols;
                pp[ncols] = NULL;
            }
        }
    }

    for (c = c1; c <= c2; c++) {
        fwidth[c] = fwidth[c + ncols];
        precision[c] = precision[c + ncols];
        realfmt[c] = realfmt[c + ncols];
        col_hidden[c] = col_hidden[c + ncols];
    }
    for (; c <= maxcols; c++) {
        fwidth[c] = DEFWIDTH;
        precision[c] = DEFPREC;
        realfmt[c] = DEFREFMT;
        col_hidden[c] = FALSE;
    }

    /* Update all marked cells. */
    for (i = 0; i < 37; i++) {
        if (savedcol[i] >= c1 && savedcol[i] <= c2)
            savedrow[i] = savedcol[i] = -1;
        if (savedcol[i] > c2)
            savedcol[i] -= ncols;
        if (savedstcol[i] >= c1 && savedstcol[i] <= c2)
            savedstcol[i] = c1;
        if (savedstcol[i] > c2)
            savedstcol[i] -= ncols;
    }
    if (gs.g_col >= c1 && gs.g_col <= c2)
        gs.g_col = c1;
    if (gs.g_col > c2)
        gs.g_col -= ncols;
    if (gs.g_lastcol >= c1 && gs.g_lastcol <= c2)
        gs.g_lastcol = c1 - 1;
    if (gs.g_lastcol > c2)
        gs.g_lastcol -= ncols;
    if (gs.g_col > gs.g_lastcol)
        gs.g_row = gs.g_col = -1;
    if (gs.stcol >= c1 && gs.stcol <= c2)
        gs.stcol = c1;
    if (gs.stcol > c2)
        gs.stcol -= ncols;

    maxcol -= ncols;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            p = *ATBL(tbl, r, c);
            if (p) {
                if (p->ncol >= c1 && p->ncol <= c2)
                    p->ncol = c1;
                if (p->ncol > c2)
                    p->ncol -= ncols;
                if (p->nlastcol >= c1 && p->nlastcol <= c2)
                    p->nlastcol = c1 - 1;
                if (p->nlastcol > c2)
                    p->nlastcol -= ncols;
                if (p->nlastcol < p->ncol)
                    p->nrow = p->ncol = -1;
            }
        }
    }
    rowsinrange = 1;
    colsinrange = fwidth[c1];

    FullUpdate++;
    modflg++;
    curcol = save < c1 ? save : (save <= c2) ? c1 : save - ncols;
}

void doend(int rowinc, int colinc) {
    struct ent *p;
    int r, c;

    if (!loading)
        remember(0);

    if (VALID_CELL(p, currow, curcol)) {
        r = currow + rowinc;
        c = curcol + colinc;
        if (r >= 0 && r < maxrows &&
            c >= 0 && c < maxcols &&
            !VALID_CELL(p, r, c)) {
                currow = r;
                curcol = c;
        }
    }

    if (!VALID_CELL(p, currow, curcol)) {
        switch (rowinc) {
        case -1:
            while (!VALID_CELL(p, currow, curcol) && currow > 0)
                currow--;
            break;
        case  1:
            while (!VALID_CELL(p, currow, curcol) && currow < maxrows - 1)
                currow++;
            break;
        case  0:
            switch (colinc) {
            case -1:
                while (!VALID_CELL(p, currow, curcol) && curcol > 0)
                    curcol--;
                break;
            case  1:
                while (!VALID_CELL(p, currow, curcol) && curcol < maxcols - 1)
                    curcol++;
                break;
            }
            break;
        }
        rowsinrange = 1;
        colsinrange = fwidth[curcol];
        if (!loading)
            remember(1);
        return;
    }

    switch (rowinc) {
    case -1:
        while (VALID_CELL(p, currow, curcol) && currow > 0)
            currow--;
        break;
    case  1:
        while (VALID_CELL(p, currow, curcol) && currow < maxrows - 1)
            currow++;
        break;
    case  0:
        switch (colinc) {
        case -1:
            while (VALID_CELL(p, currow, curcol) && curcol > 0)
                curcol--;
            break;
        case  1:
            while (VALID_CELL(p, currow, curcol) && curcol < maxcols - 1)
                curcol++;
            break;
        }
        break;
    }
    if (!VALID_CELL(p, currow, curcol)) {
        currow -= rowinc;
        curcol -= colinc;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* Modified 9/17/90 THA to handle more formats */
void cmd_format(int c1, int c2, int w, int p, int r) {
    int i;
    int crows = 0;
    int ccols = c2;

    if (c1 >= maxcols && !growtbl(GROWCOL, 0, c1)) c1 = maxcols-1;
    if (c2 >= maxcols && !growtbl(GROWCOL, 0, c2)) c2 = maxcols-1;

    if (w == 0) {
        error("Width too small - setting to 1");
        w = 1;
    }

    if (usecurses && w > COLS - rescol - 2) {
        error("Width too large - Maximum = %d", COLS - rescol - 2);
        w = COLS - rescol - 2;
    }

    if (p > w) {
        error("Precision too large");
        p = w;
    }

    checkbounds(&crows, &ccols);
    if (ccols < c2) {
        error("Format statement failed to create implied column %d", c2);
        return;
    }

    for (i = c1; i <= c2; i++) {
        fwidth[i] = w;
        precision[i] = p;
        realfmt[i] = r;
    }

    rowsinrange = 1;
    colsinrange = fwidth[curcol];

    FullUpdate++;
    modflg++;
}

void range_align(int sr, int sc, int er, int ec, int align) {
    struct ent *p;
    int i, j;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    for (i = sr; i <= er; i++) {
        for (j = sc; j <= ec; j++) {
            p = *ATBL(tbl, i, j);
            if (p) {
                p->flags &= ~ALIGN_MASK;
                p->flags |= IS_CHANGED | align;
                changed++;
                modflg++;
            }
        }
    }
}

static void print_options(FILE *f) {
    if (autocalc &&
        !autoinsert &&
        !autowrap &&
        !cslop &&
        !optimize &&
        !rndtoeven &&
        propagation == 10 &&
        calc_order == BYROWS &&
        !numeric &&
        prescale == 1.0 &&
        !extfunc &&
        showtop &&
        tbl_style == 0 &&
        craction == 0 &&
        pagesize == 0 &&
        rowlimit == -1 &&
        collimit == -1 &&
        !color &&
        !colorneg &&
        !colorerr
       )
        return;         /* No reason to do this */

    fprintf(f, "set");
    if (!autocalc)
        fprintf(f," !autocalc");
    if (autoinsert)
        fprintf(f," autoinsert");
    if (autowrap)
        fprintf(f," autowrap");
    if (cslop)
        fprintf(f," cslop");
    if (optimize)
        fprintf(f," optimize");
    if (rndtoeven)
        fprintf(f, " rndtoeven");
    if (propagation != 10)
        fprintf(f, " iterations = %d", propagation);
    if (calc_order != BYROWS )
        fprintf(f, " bycols");
    if (numeric)
        fprintf(f, " numeric");
    if (prescale != 1.0)
        fprintf(f, " prescale");
    if (extfunc)
        fprintf(f, " extfun");
    if (!showtop)
        fprintf(f, " !toprow");
    if (tbl_style) {
        fprintf(f, " tblstyle = %s",
                tbl_style == TBL ? "tbl" :
                tbl_style == LATEX ? "latex" :
                tbl_style == SLATEX ? "slatex" :
                tbl_style == TEX ? "tex" :
                tbl_style == FRAME ? "frame" : "0" );
    }
    if (craction)
        fprintf(f, " craction = %d", craction);
    if (pagesize)
        fprintf(f, " pagesize = %d", pagesize);
    if (rowlimit >= 0)
        fprintf(f, " rowlimit = %d", rowlimit);
    if (collimit >= 0)
        fprintf(f, " collimit = %d", collimit);
    if (color)
        fprintf(f," color");
    if (colorneg)
        fprintf(f," colorneg");
    if (colorerr)
        fprintf(f," colorerr");
    fprintf(f, "\n");
}

void printfile(const char *fname, int r0, int c0, int rn, int cn) {
    char field[FBUFLEN];
    buf_t(buf, FBUFLEN);
    FILE *f;
    int pid = -1;
    int fieldlen, nextcol;
    int row, col;
    char path[PATHLEN];
    char *ext;

    if (fname) {
        /* printfile will be the [path/]file ---> [path/]file.out */
        if (*fname == '\0') {
            strlcpy(path, curfile, sizeof path);
            ext = get_extension(path);

            /* keep the extension unless .sc or scext */
            if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, scext)))
                ext += strlen(ext);

            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     ascext ? ascext : "asc");
        } else {
            /* strarg in gram.y, always size of \0 terminated string. */
            strlcpy(path, fname, sizeof path);
        }
        if (!strcmp(path, curfile) &&
            !yn_ask("Confirm that you want to destroy the data base: (y,n)")) {
            return;
        }

        if ((f = openfile(path, sizeof path, &pid, NULL)) == NULL) {
            error("Can't create file \"%s\"", path);
            return;
        }
    } else {
        f = stdout;
    }

    for (row = r0; row <= rn; row++) {
        int w = 0;

        if (row_hidden[row])
            continue;

        buf_reset(buf);
        for (col = c0; col <= cn; col = nextcol, w += fieldlen) {
            struct ent *p = *ATBL(tbl, row, col);
            char *s;

            fieldlen = 0;
            nextcol = col + 1;
            if (col_hidden[col]) {
                continue;
            }

            // XXX: should handle cell fusion
            fieldlen = fwidth[col];
            if (!p)
                continue;

            if (buf_extend(buf, w + fieldlen + 2, FBUFLEN))
                goto malloc_error;

            // XXX: alignment should be determined from cell format
            //      ALIGN_AUTO, ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT

            if (p->flags & IS_VALID) {
                int align = p->flags & ALIGN_MASK;
                int len;

                if (p->cellerror) {
                    // XXX: append a space for cell alignment
                    len = strlcpy(field, (p->cellerror == CELLERROR ?
                                          "ERROR " : "INVALID "), sizeof field);
                    align |= ALIGN_CLIP;
                } else {
                    const char *cfmt = p->format;
                    if (cfmt) {
                        len = format(field, sizeof field, cfmt, precision[col], p->v, &align);
                    } else {
                        len = engformat(field, sizeof field, realfmt[col], precision[col], p->v, &align);
                    }
                }
                if ((int)buf->len < w) {
                    buf_repc(buf, ' ', w - buf->len);
                } else {
                    buf->buf[buf->len = w] = '\0';
                }
                if (align & ALIGN_CLIP) {
                    if (len < 0)
                        len = 0;
                    if (len > fieldlen)
                        len = fieldlen;
                    field[len] = '\0';
                    align &= ~ALIGN_CLIP;
                }
                if (len < 0 || len > fieldlen) {
                    buf_repc(buf, '*', fieldlen);
                } else
                if (align == ALIGN_LEFT) {
                    buf_printf(buf, "%-*s", fieldlen, field);
                } else
                if (align == ALIGN_CENTER) {
                    int half = (fieldlen - len) / 2;
                    buf_printf(buf, "%*s%*s", half, field, len - half, "");
                } else {
                    buf_printf(buf, "%*s", fieldlen, field);
                }
            } else
            if ((s = p->label)) {
                int slen = strlen(s);
                int pad = 0;
                int soff = 0;

                if (*s == '\\' && slen > 1) {
                    /* A string starting with a backslash is repeated across
                       the column width. */
                    int remain = fieldlen;
                    slen -= 1;  /* skip the '\' */
                    s += 1;
                    while (remain > 0) {
                        int chunk = slen <= remain ? slen : remain;
                        buf_put(buf, s, chunk);
                        remain -= chunk;
                    }
                    continue;
                }
                /* Figure out if the label slops over to a blank field. */
                while (slen > fieldlen && nextcol <= cn) {
                    if (!col_hidden[nextcol]) {
                        struct ent *nc = lookat(row, nextcol);
                        if ((nc->flags & IS_VALID) || nc->label)
                            break;
                        fieldlen += fwidth[nextcol];
                    }
                    nextcol++;
                }
                switch (p->flags & ALIGN_MASK) {
                default:
                case ALIGN_LEFT:
                    pad = w - buf->len;
                    if (slen > fieldlen)
                        slen = fieldlen;
                    break;
                case ALIGN_CENTER:
                    pad = w - buf->len + (fwidth[nextcol] - slen) / 2;
                    if (pad < 0) {
                        soff = -pad;
                        slen -= soff;
                    }
                    if ((int)buf->len + pad + slen > w + fieldlen)
                        slen = w + fieldlen - buf->len - pad;
                    break;
                case ALIGN_RIGHT:
                    pad = w - buf->len + fieldlen - slen;
                    if (pad < 0) {
                        soff = -pad;
                        slen -= soff;
                        pad = 0;
                    }
                    break;
                }

                if (buf_extend(buf, w + fieldlen + 2, FBUFLEN))
                    goto malloc_error;

                buf_repc(buf, ' ', pad);
                buf_put(buf, s + soff, slen);
                if (nextcol <= cn)
                    buf_repc(buf, ' ', w + fieldlen - buf->len);
            }
        }
        buf_putc(buf, '\n');
        fputs(buf->buf, f);
    }
    if (0) {
    malloc_error:
        error("Realloc failed in printfile()");
    }
    buf_free(buf);
    if (fname) closefile(f, pid, 0);
}

void tblprintfile(const char *fname, int r0, int c0, int rn, int cn) {
    FILE *f;
    int pid;
    int row, col;
    char coldelim = DEFCOLDELIM;
    char path[PATHLEN];
    char *ext;

    /* tblprintfile will be the [path/]file ---> [path/]file.out */
    if (*fname == '\0') {
        strlcpy(path, curfile, sizeof path);
        ext = get_extension(path);

        /* keep the extension unless .sc or scext */
        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, scext)))
            ext += strlen(ext);

        if (tbl_style == 0) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     tbl0ext ? tbl0ext : "cln");
        } else
        if (tbl_style == TBL) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     tblext ? tblext : "tbl");
        } else
        if (tbl_style == LATEX) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     latexext ? latexext : "lat");
        } else
        if (tbl_style == SLATEX) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     slatexext ? slatexext : "stx");
        } else
        if (tbl_style == TEX) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     texext ? texext : "tex");
        }
    } else {
        strlcpy(path, fname, sizeof path);
    }
    if (!strcmp(path, curfile) &&
        !yn_ask("Confirm that you want to destroy the data base: (y,n)"))
        return;

    if ((f = openfile(path, sizeof path, &pid, NULL)) == NULL) {
        error("Can't create file \"%s\"", path);
        return;
    }

    if (tbl_style == TBL) {
        fprintf(f, ".\\\" ** %s spreadsheet output \n.TS\n", progname);
        fprintf(f, "tab(%c);\n", coldelim);
        for (col = c0; col <= cn; col++)
            fprintf(f, " n");
        fprintf(f, ".\n");
    } else
    if (tbl_style == LATEX) {
        fprintf(f, "%% ** %s spreadsheet output\n\\begin{tabular}{", progname);
        for (col = c0; col <= cn; col++)
            fprintf(f, "c");
        fprintf(f, "}\n");
        coldelim = '&';
    } else
    if (tbl_style == SLATEX) {
        fprintf(f, "%% ** %s spreadsheet output\n!begin<tabular><", progname);
        for (col = c0; col <= cn; col++)
            fprintf(f, "c");
        fprintf(f, ">\n");
        coldelim = '&';
    } else
    if (tbl_style == TEX) {
        fprintf(f, "{\t%% ** %s spreadsheet output\n\\settabs %d \\columns\n",
                progname, cn - c0 + 1);
        coldelim = '&';
    } else
    if (tbl_style == FRAME) {
        fprintf(f, "<MIFFile 3.00> # generated by the sc spreadsheet calculator\n");
        fprintf(f, "<Tbls\n");
        fprintf(f, " <Tbl \n");
        fprintf(f, "  <TblID 1> # This table's ID is 1\n");
        fprintf(f, "  <TblFormat \n");
        fprintf(f, "   <TblTag `Format A'> # Table Format Catalog\n");
        fprintf(f, "  > # end of TblFormat\n");
        fprintf(f, "  <TblNumColumns %d> # Has %d columns\n", cn-c0+1, cn-c0+1);
        fprintf(f, "  <TblTitleContent\n");
        fprintf(f, "   <Para\n");
        fprintf(f, "    <PgfTag `TableTitle'> # Forces lookup in Paragraph Format Catalog\n");
        fprintf(f, "    <ParaLine\n");
        fprintf(f, "     <String `%s'>\n", fname);
        fprintf(f, "    > # end of ParaLine\n");
        fprintf(f, "   > # end of Para\n");
        fprintf(f, "  > # end of TblTitleContent\n");
        fprintf(f, "  <TblH # The heading\n");
        fprintf(f, "   <Row # The heading row\n");
        for (col = c0; col <= cn; col++) {
            fprintf(f, "    <Cell <CellContent <Para # Cell in column \n");
            fprintf(f, "       <PgfTag `CellHeading'> # in Paragraph Format Catalog\n");
            fprintf(f, "       <ParaLine <String `%c'>>\n", 'A'+col);
            fprintf(f, "    >>> # end of Cell\n");
        }
        fprintf(f, "   > # end of Row\n");
        fprintf(f, "  > # end of TblH\n");
        fprintf(f, "  <TblBody # The body\n");
    }

    for (row = r0; row <= rn; row++) {
        int fieldlen;

        // XXX: print hidden rows?

        if (tbl_style == TEX)
            fprintf(f, "\\+");
        else
        if (tbl_style == FRAME) {
            fprintf(f, "   <Row # The next body row\n");
        }

        for (col = c0; col <= cn; col++) {
            struct ent *p = *ATBL(tbl, row, col);
            char *s;

            // XXX: print hidden columns?

            // XXX: should handle cell fusion
            fieldlen = fwidth[col];

            if (tbl_style == FRAME) {
                fprintf(f, "    <Cell <CellContent <Para\n");
                fprintf(f, "       <PgfTag `CellBody'> # in Paragraph Format Catalog\n");
                fprintf(f, "       <ParaLine <String `");
            }
            if (p) {
                char field[FBUFLEN];
                int align = p->flags & ALIGN_MASK;
                int len;

                if (p->flags & IS_VALID) {
                    /* convert cell contents, do not test width, do not align with spaces */
                    // XXX: should implement alignment in output format
                    if (p->cellerror) {
                        len = strlcpy(field, (p->cellerror == CELLERROR ?
                                              "ERROR" : "INVALID"), sizeof field);
                        align |= ALIGN_CLIP;
                    } else {
                        const char *cfmt = p->format;
                        if (cfmt) {
                            len = format(field, sizeof field, cfmt, precision[col], p->v, &align);
                        } else {
                            len = engformat(field, sizeof field, realfmt[col], precision[col], p->v, &align);
                        }
                    }
                    // XXX: should fill fieldlen with * if too long
                    unspecial(f, field, coldelim);
                }
                if ((s = p->label)) {
                    // XXX: should handle repeated pattern starting with '\'
                    unspecial(f, s, coldelim);
                }
            }
            if (tbl_style == FRAME) {
                fprintf(f, "'>>\n");
                fprintf(f, "    >>> # end of Cell\n");
            }
            if (col < cn) {
                if (tbl_style != FRAME)
                    fprintf(f, "%c", coldelim);
            }
        }
        if (tbl_style == LATEX) {
            if (row < rn) fprintf (f, "\\\\");
        } else
        if (tbl_style == SLATEX) {
            if (row < rn) fprintf(f, "!!");
        } else
        if (tbl_style == TEX) {
            fprintf (f, "\\cr");
        } else
        if (tbl_style == FRAME) {
            fprintf(f, "   > # end of Row\n");
        }
        fprintf(f, "\n");
    }

    if (tbl_style == TBL)
        fprintf (f,".TE\n.\\\" ** end of %s spreadsheet output\n", progname);
    else
    if (tbl_style == LATEX)
        fprintf(f, "\\end{tabular}\n%% ** end of %s spreadsheet output\n", progname);
    else
    if (tbl_style == SLATEX)
        fprintf (f,"!end<tabular>\n%% ** end of %s spreadsheet output\n", progname);
    else
    if (tbl_style == TEX)
        fprintf (f,"}\n%% ** end of %s spreadsheet output\n", progname);
    else
    if (tbl_style == FRAME) {
        fprintf(f, "  > # end of TblBody\n");
        fprintf(f, " ># end of Tbl\n");
        fprintf(f, "> # end of Tbls\n");
        fprintf(f, "<TextFlow <Para \n");
        fprintf(f, "  <PgfTag Body> \n");
        fprintf(f, "  <ParaLine <ATbl 1>> # Reference to table ID 1\n");
        fprintf(f, ">>\n");
    }

    closefile(f, pid, 0);
}

/* unspecial (backquote) things that are special chars in a table */
static void unspecial(FILE *f, char *str, int delim) {
    if (*str == '\\') str++; /* delete wheeling string operator, OK? */
    while (*str) {
        if (((tbl_style == LATEX) || (tbl_style == SLATEX) ||
             (tbl_style == TEX)) &&
            ((*str == delim) || (*str == '$') || (*str == '#') ||
             (*str == '%') || (*str == '{') || (*str == '}') ||
             (*str == '&')))
            putc('\\', f);
        putc(*str, f);
        str++;
    }
}

struct enode *copye(struct enode *e, int Rdelta, int Cdelta,
                    int r1, int c1, int r2, int c2, int transpose)
{
    struct enode *ret;
    static struct enode *range = NULL;

    if (e == NULL)
        ret = NULL;
    else if (e->op & REDUCE) {
        int newrow, newcol;
        if (freeenodes) {
            ret = freeenodes;
            freeenodes = ret->e.o.left;
        } else
            ret = scxmalloc(sizeof (struct enode));
        ret->op = e->op;
        newrow = e->e.r.left.vf & FIX_ROW ||
                 e->e.r.left.vp->row < r1 || e->e.r.left.vp->row > r2 ||
                 e->e.r.left.vp->col < c1 || e->e.r.left.vp->col > c2 ?
                 e->e.r.left.vp->row :
                 transpose ? r1 + Rdelta + e->e.r.left.vp->col - c1 :
                 e->e.r.left.vp->row + Rdelta;
        newcol = e->e.r.left.vf & FIX_COL ||
                 e->e.r.left.vp->row < r1 || e->e.r.left.vp->row > r2 ||
                 e->e.r.left.vp->col < c1 || e->e.r.left.vp->col > c2 ?
                 e->e.r.left.vp->col :
                 transpose ? c1 + Cdelta + e->e.r.left.vp->row - r1 :
                 e->e.r.left.vp->col + Cdelta;
        ret->e.r.left.vp = lookat(newrow, newcol);
        ret->e.r.left.vf = e->e.r.left.vf;
        newrow = e->e.r.right.vf & FIX_ROW ||
                 e->e.r.right.vp->row < r1 || e->e.r.right.vp->row > r2 ||
                 e->e.r.right.vp->col < c1 || e->e.r.right.vp->col > c2 ?
                 e->e.r.right.vp->row :
                 transpose ? r1 + Rdelta + e->e.r.right.vp->col - c1 :
                 e->e.r.right.vp->row + Rdelta;
        newcol = e->e.r.right.vf & FIX_COL ||
                 e->e.r.right.vp->row < r1 || e->e.r.right.vp->row > r2 ||
                 e->e.r.right.vp->col < c1 || e->e.r.right.vp->col > c2 ?
                 e->e.r.right.vp->col :
                 transpose ? c1 + Cdelta + e->e.r.right.vp->row - r1 :
                 e->e.r.right.vp->col + Cdelta;
        ret->e.r.right.vp = lookat(newrow, newcol);
        ret->e.r.right.vf = e->e.r.right.vf;
    } else {
        struct enode *temprange = NULL;

        if (freeenodes) {
            ret = freeenodes;
            freeenodes = ret->e.o.left;
        } else
            ret = scxmalloc(sizeof (struct enode));
        ret->op = e->op;
        switch (ret->op) {
            case SUM:
            case PROD:
            case AVG:
            case COUNT:
            case STDDEV:
            case MAX:
            case MIN:
                temprange = range;
                range = e->e.o.left;
                r1 = 0;
                c1 = 0;
                r2 = maxrow;
                c2 = maxcol;
        }
        switch (ret->op) {
            case 'v':
                {
                    int newrow, newcol;
                    if (range && e->e.v.vp->row >= range->e.r.left.vp->row &&
                                 e->e.v.vp->row <= range->e.r.right.vp->row &&
                                 e->e.v.vp->col >= range->e.r.left.vp->col &&
                                 e->e.v.vp->col <= range->e.r.right.vp->col) {
                        newrow = range->e.r.left.vf & FIX_ROW ?
                                e->e.v.vp->row : e->e.v.vp->row + Rdelta;
                        newcol = range->e.r.left.vf & FIX_COL ?
                                e->e.v.vp->col : e->e.v.vp->col + Cdelta;
                    } else {
                        newrow = e->e.v.vf & FIX_ROW ||
                                 e->e.v.vp->row < r1 || e->e.v.vp->row > r2 ||
                                 e->e.v.vp->col < c1 || e->e.v.vp->col > c2 ?
                                 e->e.v.vp->row :
                                 transpose ? r1 + Rdelta + e->e.v.vp->col - c1 :
                                 e->e.v.vp->row + Rdelta;
                        newcol = e->e.v.vf & FIX_COL ||
                                 e->e.v.vp->row < r1 || e->e.v.vp->row > r2 ||
                                 e->e.v.vp->col < c1 || e->e.v.vp->col > c2 ?
                                 e->e.v.vp->col :
                                 transpose ? c1 + Cdelta + e->e.v.vp->row - r1 :
                                 e->e.v.vp->col + Cdelta;
                    }
                    ret->e.v.vp = lookat(newrow, newcol);
                    ret->e.v.vf = e->e.v.vf;
                    break;
                }
            case 'k':
                ret->e.k = e->e.k;
                break;
            case 'f':
            case 'F':
                if (( range && ret->op == 'F') ||
                    (!range && ret->op == 'f')   )
                    Rdelta = Cdelta = 0;
                ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
                                      r1, c1, r2, c2, transpose);
                ret->e.o.right = NULL;
                break;
            case '$':
            case EXT:
                ret->e.s = scxdup(e->e.s);
                if (e->op == '$')       /* Drop through if ret->op is EXT */
                    break;
                FALLTHROUGH;
            default:
                ret->e.o.left = copye(e->e.o.left, Rdelta, Cdelta,
                                      r1, c1, r2, c2, transpose);
                ret->e.o.right = copye(e->e.o.right, Rdelta, Cdelta,
                                       r1, c1, r2, c2, transpose);
                break;
        }
        switch (ret->op) {
        case SUM:
        case PROD:
        case AVG:
        case COUNT:
        case STDDEV:
        case MAX:
        case MIN:
            range = temprange;
        }
    }
    return ret;
}

void docopy(void) {
    if (showrange) {
        showrange = 0;
        copy(lookat(showsr, showsc), lookat(currow, curcol), NULL, NULL);
    } else {
        copy(lookat(currow, curcol), lookat(currow, curcol), NULL, NULL);
    }
}

/*
 * sync_refs and syncref are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
void sync_refs(void) {
    int i, j;
    struct ent *p;
    sync_ranges();
    for (i = 0; i <= maxrow; i++) {
        for (j = 0; j <= maxcol; j++) {
            if ((p = *ATBL(tbl, i, j)) && p->expr)
                syncref(p->expr);
        }
    }
    for (i = 0; i < DELBUFSIZE; i++) {
        for (p = delbuf[i]; p; p = p->next) {
            if (p->expr)
                syncref(p->expr);
        }
    }
}

static void syncref(struct enode *e) {
    if (e == NULL)
        return;
    else if (e->op & REDUCE) {
        e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
        e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
    } else {
        switch (e->op) {
        case 'v':
            if (e->e.v.vp->flags & IS_CLEARED) {
                e->op = ERR_;
                e->e.o.left = NULL;
                e->e.o.right = NULL;
            } else if (e->e.v.vp->flags & MAY_SYNC) {
                e->e.v.vp = lookat(e->e.v.vp->row, e->e.v.vp->col);
            }
            break;
        case 'k':
            break;
        case '$':
            break;
        default:
            syncref(e->e.o.right);
            syncref(e->e.o.left);
            break;
        }
    }
}

/* mark rows as hidden */
void hiderows(int r1, int r2) {
    int r, a;
    if (r1 > r2) SWAPINT(r1, r2);
    if (r1 < 0) {
        error("Invalid range");
        return;
    }
    a = r2 - r1 + 1;
    if (r2 > maxrows) {
        if (!growtbl(GROWROW, a + 1, 0)) {
            error("You can't hide the last row");
            return;
        }
    }
    for (r = r1; r <= r2; r++)
        row_hidden[r] = 1;

    if (currow >= r1)
        currow = (currow <= r2) ? r2 + 1 : currow - a;

    FullUpdate++;
    modflg++;
}

/* mark columns as hidden */
void hidecols(int c1, int c2) {
    int c, a;
    if (c1 > c2) SWAPINT(c1, c2);
    a = c2 - c1 + 1;
    if (c1 < 0) {
        error("Invalid range");
        return;
    }
    if (c2 >= maxcols - 1) {
        if ((a >= ABSMAXCOLS - 1) || !growtbl(GROWCOL, 0, a + 1)) {
            error("You can't hide the last col");
            return;
        }
    }
    for (c = c1; c <= c2; c++)
        col_hidden[c] = TRUE;

    if (curcol >= c1)
        curcol = (curcol <= c2) ? c2 + 1 : curcol - a;

    FullUpdate++;
    modflg++;
}

void dohide(void) {
    if (showrange == SHOWROWS) {
        hiderows(currow, showsr);
    } else
    if (showrange == SHOWCOLS) {
        hidecols(curcol, showsc);
    }
}

/* mark a row as not-hidden */
void showrow(int r1, int r2) {
    if (r1 < 0 || r1 > r2) {
        error ("Invalid range");
        return;
    }
    if (r2 > maxrows - 1) {
        r2 = maxrows - 1;
    }
    FullUpdate++;
    modflg++;
    while (r1 <= r2)
        row_hidden[r1++] = 0;
}

/* mark a column as not-hidden */
void showcol(int c1, int c2) {
    if (c1 < 0 || c1 > c2) {
        error ("Invalid range");
        return;
    }
    if (c2 > maxcols - 1) {
        c2 = maxcols - 1;
    }
    FullUpdate++;
    modflg++;
    while (c1 <= c2)
        col_hidden[c1++] = FALSE;
}

/* Open the input or output file, setting up a pipe if needed */
FILE *openfile(char *fname, size_t fnamesiz, int *rpid, int *rfd) {
    int pipefd[4];
    int pid;
    FILE *f;
    char *efname;

    while (*fname == ' ') { /* Skip leading blanks */
        fname++;
        fnamesiz--;
    }

    if (*fname != '|') {                /* Open file if not pipe */
        *rpid = 0;
        if (rfd != NULL)
            *rfd = 1;                   /* Set to stdout just in case */

        efname = findhome(fname, fnamesiz);
        if (dobackups && rfd == NULL && !backup_file(efname) &&
            (yn_ask("Could not create backup copy.  Save anyway?: (y,n)") != 1))
            return 0;
        return fopen(efname, rfd == NULL ? "w" : "r");
    }

#ifdef NOPIPES
    error("Piping not available\n");
    return 0;
#else
    fname++;                            /* Skip | */
    fnamesiz--;
    efname = findhome(fname, fnamesiz);
    if (pipe(pipefd) < 0 || (rfd != NULL && pipe(pipefd+2) < 0)) {
        error("Can't make pipe to child");
        *rpid = 0;
        return 0;
    }

    deraw(rfd == NULL);
#ifdef VMS
    fprintf(stderr, "No son tasks available yet under VMS--sorry\n");
    return f;
#else /* VMS */
    if ((pid = fork()) == 0) {   /* if child */
        close(0);                /* close stdin */
        close(pipefd[1]);
        dup(pipefd[0]);          /* connect to first pipe */
        if (rfd != NULL) {       /* if opening for read */
            close(1);            /* close stdout */
            close(pipefd[2]);
            dup(pipefd[3]);      /* connect to second pipe */
        }
        signal(SIGINT, SIG_DFL); /* reset */
        execl("/bin/sh", "sh", "-c", efname, (char *)NULL);
        exit (-127);
    } else {                     /* else parent */
        *rpid = pid;
        if ((f = fdopen(pipefd[(rfd == NULL ? 1 : 2)], rfd == NULL ? "w" : "r")) == NULL) {
            kill(pid, 9);
            error("Cannot fdopen for %s", rfd == NULL ? "output" : "input");
            close(pipefd[1]);
            if (rfd != NULL)
                close(pipefd[3]);
            *rpid = 0;
            return 0;
        }
    }
    close(pipefd[0]);
    if (rfd != NULL) {
        close(pipefd[3]);
        *rfd = pipefd[1];
    }
    return f;
#endif /* VMS */
#endif /* NOPIPES */
}

/* close a file opened by openfile(), if process wait for return */
void closefile(FILE *f, int pid, int rfd) {
    int temp;

    if (fclose(f) == EOF) {
        error("fclose(): %s", strerror(errno));
    }
#ifndef NOPIPES
    if (pid) {
        while (pid != wait(&temp))
            continue;
        if (rfd == 0) {
            printf("Press any key to continue ");
            fflush(stdout);
            cbreak();
            nmgetch(0);
            goraw();
            clear();
        } else {
            close(rfd);
            if (usecurses) {
# ifdef VMS
                VMS_read_raw = 1;
# else /* VMS */
#  ifdef HAVE_FIXTERM
                fixterm();
#  else
                cbreak();
                nonl();
                noecho();
#  endif
                kbd_again();
# endif /* VMS */
                if (color && has_colors())
                    bkgdset(COLOR_PAIR(1) | ' ');
            }
        }
    }
#endif /* NOPIPES */
    if (brokenpipe) {
        error("Broken pipe");
        brokenpipe = FALSE;
    }
}

/* Copy a cell (struct ent).  "special" indicates special treatment when
 * merging two cells for the "pm" command, merging formats only for the
 * "pf" command, or for adjusting cell references when transposing with
 * the "pt" command.  r1, c1, r2, and c2 define the range in which the dr
 * and dc values should be used.
 */
void copyent(struct ent *n, struct ent *p, int dr, int dc,
             int r1, int c1, int r2, int c2, int special)
{
    if (!n || !p) {
        error("internal error");
        return;
    }
    if (special != 'f') {
        if (special != 'm' || (p->flags & IS_VALID)) {
            n->v = p->v;
            n->flags |= p->flags & IS_VALID;
        }
        if (special != 'm' || p->expr) {
            n->expr = copye(p->expr, dr, dc, r1, c1, r2, c2, special == 't');
            if (p->flags & IS_STREXPR)
                n->flags |= IS_STREXPR;
            else
                n->flags &= ~IS_STREXPR;
        }
        if (p->label) {
            set_cstring(&n->label, p->label);
        } else if (special != 'm') {
            set_cstring(&n->label, NULL);
        }
        n->flags &= ~ALIGN_MASK;
        n->flags |= p->flags & ALIGN_MASK;
        n->flags |= p->flags & IS_LOCKED;
    }
    if (p->format) {
        set_cstring(&n->format, p->format);
    } else if (special != 'm' && special != 'f') {
        set_cstring(&n->format, NULL);
    }
    n->flags |= IS_CHANGED;
}

#ifndef NOPLUGINS
/* add a plugin/mapping pair to the end of the filter list. type is
 * r(ead) or w(rite)
 */

void addplugin(const char *ext, const char *plugin, char type) {
    struct impexfilt *fp;
    char mesg[PATHLEN];

    if (!plugin_exists(plugin, -1, mesg, sizeof mesg)) {
        error("Cannot find plugin %s", plugin);
        return;
    }
    if (filt == NULL) {
        filt = scxmalloc(sizeof(struct impexfilt));
        fp = filt;
    } else {
        fp = filt;
        while (fp->next != NULL)
            fp = fp->next;
        fp->next = scxmalloc(sizeof(struct impexfilt));
        fp = fp->next;
    }
    strlcpy(fp->plugin, plugin, PATHLEN);
    strlcpy(fp->ext, ext, PATHLEN);
    fp->type = type;
    fp->next = NULL;
}

char *findplugin(const char *ext, char type) {
    struct impexfilt *fp;

    fp = filt;
    if (fp == NULL)
        return NULL;
    if ((!strcmp(fp->ext, ext)) && (fp->type == type))
        return fp->plugin;
    while (fp->next != NULL) {
        fp = fp->next;
        if ((!strcmp(fp->ext, ext)) && (fp->type == type))
            return fp->plugin;
    }

    return NULL;
}
#endif /* NOPLUGINS */

void write_fd(FILE *f, int r0, int c0, int rn, int cn) {
    int r, c;

    fprintf(f, "# This data file was generated by the Spreadsheet ");
    fprintf(f, "Calculator.\n");
    fprintf(f, "# You almost certainly shouldn't edit it.\n\n");
    print_options(f);
    write_abbrevs(f);
    for (c = 0; c < COLFORMATS; c++) {
        if (colformat[c])
            fprintf(f, "format %d = \"%s\"\n", c, colformat[c]);
    }
    for (c = c0; c <= cn; c++) {
        if (fwidth[c] != DEFWIDTH || precision[c] != DEFPREC || realfmt[c] != DEFREFMT) {
            fprintf(f, "format %s %d %d %d\n",
                    coltoa(c), fwidth[c], precision[c], realfmt[c]);
        }
    }
    for (c = c0; c <= cn; c++) {
        if (col_hidden[c])
            fprintf(f, "hide %s\n", coltoa(c));
    }
    for (r = r0; r <= rn; r++) {
        if (row_hidden[r])
            fprintf(f, "hide %d\n", r);
    }
    write_ranges(f);
    write_franges(f);
    write_colors(f, 0);
    write_cranges(f);

    if (mdir)
        fprintf(f, "mdir \"%s\"\n", mdir);
    if (autorun)
        fprintf(f, "autorun \"%s\"\n", autorun);
    for (c = 0; c < FKEYS; c++) {
        if (fkey[c])
            fprintf(f, "fkey %d = \"%s\"\n", c + 1, fkey[c]);
    }
    write_cells(f, r0, c0, rn, cn, r0, c0);
    for (r = r0; r <= rn; r++) {
        for (c = c0; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p) {
                if (p->flags & IS_LOCKED) {
                    fprintf(f, "lock %s%d\n", coltoa(p->col), p->row);
                }
                if (p->nrow >= 0) {
                    fprintf(f, "addnote %s %s\n",
                            v_name(p->row, p->col),
                            r_name(p->nrow, p->ncol,
                                   p->nlastrow, p->nlastcol));
                }
            }
        }
    }
    fprintf(f, "goto %s %s\n", v_name(currow, curcol), v_name(strow, stcol));
}

void write_cells(FILE *f, int r0, int c0, int rn, int cn, int dr, int dc) {
    buf_t(buf, FBUFLEN);
    int r, c, mf;
    int rs = 0;
    int cs = 0;

    mf = modflg;
    if (dr != r0 || dc != c0) {
        yank_area(r0, c0, rn, cn);
        rn += dr - r0;
        cn += dc - c0;
        rs = currow;
        cs = curcol;
        currow = dr;
        curcol = dc;
        pullcells('x');
    }
    if (Vopt) valueize_area(dr, dc, rn, cn);
    for (r = dr; r <= rn; r++) {
        for (c = dc; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p) {
                if (p->label || (p->flags & IS_STREXPR)) {
                    edits(buf, r, c);
                    fprintf(f, "%s\n", buf->buf);
                }
                if (p->flags & IS_VALID) {
                    editv(buf, r, c);
                    if (dpoint != '.') {
                        char *dpointptr = strchr(buf->buf, dpoint);
                        if (dpointptr != NULL)
                            *dpointptr = '.';
                    }
                    fprintf(f, "%s\n", buf->buf);
                }
                if (p->format) {
                    editfmt(buf, r, c);
                    fprintf(f, "%s\n", buf->buf);
                }
            }
        }
    }
    if (dr != r0 || dc != c0) {
        pullcells('x');
        currow = rs;
        curcol = cs;
        flush_saved();
    }
    modflg = mf;
}

int writefile(const char *fname, int r0, int c0, int rn, int cn) {
    FILE *f;
    char save[PATHLEN];
    char tfname[PATHLEN];
    char *tpp;
    const char *p;
    char *ext;
    char *plugin;
    int pid;

#ifndef NOPLUGINS
    /* find the extension and mapped plugin if exists */
    p = get_extension(fname);
    if (*p) {
        if ((plugin = findplugin(p + 1, 'w')) != NULL) {
            size_t len;
            if (!plugin_exists(plugin, -1, save + 1, sizeof save - 1)) {
                error("plugin not found");
                return -1;
            }
            *save = '|';
            len = strlen(save);
            if (snprintf(save + len, sizeof(save) - len, " %s%d:%s%d \"%s\"",
                         coltoa(c0), r0, coltoa(cn), rn, fname) >= PATHLEN) {
                error("Path too long");
                return -1;
            }
            /* pass it to readfile as an advanced macro */
            // XXX: does writefile pass to readfile?
            readfile(save, 0);
            return 0;
        }
    }
#endif

#ifndef NOCRYPT
    if (Crypt) {
        return cwritefile(fname, r0, c0, rn, cn);
    }
#endif /* NOCRYPT */

    if (*fname == '\0') {
        if (isatty(STDOUT_FILENO) || *curfile != '\0') {
            fname = curfile;
        } else {
            write_fd(stdout, r0, c0, rn, cn);
            return 0;
        }
    }

    /* copy the string, strip the \ in front of " */
    for (tpp = tfname, p = fname; *p; p++) {
        if (*p == '\\' && p[1] == '"')
            p++;
        *tpp++ = *p;
    }
    *tpp = '\0';
    ext = get_extension(tfname);
    if (scext != NULL) {
        if (!strcmp(ext, ".sc") || (scext && !strcmp(ext, scext)))
            *ext = '\0';
        strlcat(tfname, ".", sizeof tfname);
        strlcat(tfname, scext, sizeof tfname);
    }

    strlcpy(save, tfname, sizeof save);
    for (tpp = save; *tpp != '\0'; tpp++) {
        if (*tpp == '"') {
            strsplice(save, sizeof save, tpp - save, 0, "\\", 1);
            tpp++;
        }
    }
    if ((f = openfile(tfname, sizeof tfname, &pid, NULL)) == NULL) {
        error("Can't create file \"%s\"", save);
        return -1;
    }

    if (usecurses) {
        error("Writing file \"%s\"...", save);
        refresh();
    }
    write_fd(f, r0, c0, rn, cn);

    closefile(f, pid, 0);

    if (!pid) {
        strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
        FullUpdate++;
        if (usecurses) {
            error("File \"%s\" written", curfile);
        } else
            fprintf(stderr, "\nFile \"%s\" written", curfile);
    }

    return 0;
}

int readfile(const char *fname, int eraseflg) {
    FILE *f;
    char save[PATHLEN];
    char buf[FBUFLEN];
    int tempautolabel;
    char *p;
    char *plugin;
    int pid = 0;
    int rfd = STDOUT_FILENO, savefd;

    tempautolabel = autolabel;          /* turn off auto label when */
    autolabel = 0;                      /* reading a file */

    if (*fname == '*' && mdir) {
        strlcpy(save, mdir, sizeof save);
        strlcat(save, fname, sizeof save);
    } else {
        if (*fname == '\0')
            fname = curfile;
        strlcpy(save, fname, sizeof save);
    }

#ifndef NOPLUGINS
    if ((p = strrchr(fname, '.')) && (fname[0] != '|')) {  /* exclude macros */
        if ((plugin = findplugin(p+1, 'r')) != NULL) {
            size_t l;
            if (!plugin_exists(plugin, -1, save + 1, sizeof save - 1)) {
                error("plugin not found");
                return 0;
            }
            *save = '|';
            if ((strlen(save) + strlen(fname) + 2) > PATHLEN) {
                error("Path too long");
                return 0;
            }
            l = strlen(save);
            snprintf(save + l, sizeof(save) - l, " \"%s\"", fname);
            eraseflg = 0;
            /* get filename: could be preceded by params if this is a save */
            while (p > fname) {
                if (*p == ' ') {
                    p++;
                    break;
                }
                p--;
            }
            strlcpy(curfile, p, sizeof curfile);
        }
    }
#endif

#ifndef NOCRYPT
    if (Crypt) {
        int ret = 0;
        if (*save == '-' && strlen(fname) == 1)
            error("Can't use encryption in a pipeline.");
        else
        if (*save == '|')
            error("Can't use encryption with advanced macros.");
        else
            ret = creadfile(save, eraseflg);
        autolabel = tempautolabel;
        return ret;
    }
#endif /* NOCRYPT */

    if (eraseflg && strcmp(fname, curfile) && modcheck(" first"))
        return 0;

    if (fname[0] == '-' && fname[1] == '\0') {
        f = stdin;
        *save = '\0';
    } else {
        if ((f = openfile(save, sizeof save, &pid, &rfd)) == NULL) {
            error("Can't read file \"%s\"", save);
            autolabel = tempautolabel;
            return 0;
        }
    }
    if (*fname == '|')
        *save = '\0';

    if (eraseflg) {
        if (*save) {
            if (usecurses) {
                error("Reading file \"%s\"", save);
                refresh();
            } else
                fprintf(stderr, "Reading file \"%s\"\n", save);
        }
        erasedb();
    }

    remember(0);
    loading++;
    savefd = macrofd;
    macrofd = rfd;
    // XXX: should use a local buffer
    while (!brokenpipe && fgets(buf, sizeof(buf), f)) {
        p = buf;
        if (*p == '|' && pid != 0) {
            *p = ' ';
        } else {
            while (*p == ' ') {
                /* skip initial blanks */
                p++;
            }
            if (*p == '#' || *p == '\0' || *p == '\n') {
                /* ignore comments and blank lines */
                continue;
            }
        }
        parse_line(buf);
    }
    macrofd = savefd;
    --loading;
    remember(1);

    closefile(f, pid, rfd);
    if (f == stdin) {
        freopen("/dev/tty", "r", stdin);
        goraw();
    }
    //linelim = -1;
    if (eraseflg) {
        strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
        cellassign = 0;
        if (autorun && !skipautorun) readfile(autorun, 0);
        skipautorun = 0;
        EvalAll();
        if (*save) {
            if (usecurses) {
                error("File \"%s\" loaded.", save);
                refresh();
            } else
                fprintf(stderr, "File \"%s\" loaded.\n", save);
        }
    }
    autolabel = tempautolabel;
    FullUpdate++;
    return 1;
}

/* erase the database (tbl, etc.) */
void erasedb(void) {
    int r, c, fd;
    char *home;

    for (c = 0; c <= maxcol; c++) {
        fwidth[c] = DEFWIDTH;
        precision[c] = DEFPREC;
        realfmt[c] = DEFREFMT;
    }

    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent **pp = ATBL(tbl, r, c);
            if (*pp) {
                efree((*pp)->expr);
                (*pp)->expr = NULL;
                set_string(&(*pp)->label, NULL);
                set_string(&(*pp)->format, NULL);
                (*pp)->next = freeents; /* save [struct ent] for reuse */
                freeents = *pp;
                *pp = NULL;
            }
        }
    }

    for (c = 0; c < COLFORMATS; c++)
        set_cstring(&colformat[c], NULL);

    maxrow = 0;
    maxcol = 0;
    clean_range();
    clean_frange();
    clean_crange();
    clean_abbrevs();

    propagation = 10;
    calc_order = BYROWS;
    prescale = 1.0;
    tbl_style = 0;
    craction = 0;
    rowlimit = collimit = -1;
    qbuf = 0;

    autocalc = showcell = showtop = color = colorneg = colorerr = 1;
    autoinsert = autowrap = optimize = numeric = extfunc = cslop = 0;
    currow = curcol = strow = stcol = 0;
    if (usecurses)
        select_style(STYLE_NONE, 0);
    /* unset all marks */
    for (c = 0; c < 37; c++)
        savedrow[c] = savedcol[c] = savedstrow[c] = savedstcol[c] = -1;

    set_cstring(&mdir, NULL);
    set_cstring(&autorun, NULL);

    for (c = 0; c < FKEYS; c++)
        set_cstring(&fkey[c], NULL);

    // XXX: this should be in a different function
    /*
     * Load $HOME/.scrc if present.
     */
    if ((home = getenv("HOME"))) {
        snprintf(curfile, sizeof curfile, "%s/.scrc", home);
        if ((fd = open(curfile, O_RDONLY)) >= 0) {
            close(fd);
            readfile(curfile, 0);
        }
    }

    /*
     * Load ./.scrc if present and $HOME/.scrc contained `set scrc'.
     */
    if (scrc && strcmp(home, getcwd(curfile, PATHLEN)) &&
            (fd = open(".scrc", O_RDONLY)) >= 0) {
        close(fd);
        readfile(".scrc", 0);
    }

    *curfile = '\0';
    FullUpdate++;
}

/* moves curcol back one displayed column */
void backcol(int arg) {
    while (--arg >= 0) {
        if (curcol)
            curcol--;
        else
            {error ("At column A"); break;}
        while (col_hidden[curcol] && curcol)
            curcol--;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves curcol forward one displayed column */
void forwcol(int arg) {
    while (--arg >= 0) {
        if (curcol < maxcols - 1)
            curcol++;
        else
        if (!growtbl(GROWCOL, 0, arg))  /* get as much as needed */
            break;
        else
            curcol++;
        while (col_hidden[curcol] && (curcol < maxcols - 1))
            curcol++;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves currow forward one displayed row */
void forwrow(int arg) {
    while (--arg >= 0) {
        if (currow < maxrows - 1)
            currow++;
        else
        if (!growtbl(GROWROW, arg, 0))  /* get as much as needed */
            break;
        else
            currow++;
        while (row_hidden[currow] && (currow < maxrows-1))
            currow++;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

/* moves currow backward one displayed row */
void backrow(int arg) {
    while (--arg>=0) {
        if (currow)
            currow--;
        else {
            error("At row zero");
            break;
        }
        while (row_hidden[currow] && currow)
            currow--;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
}

void gotonote(void) {
    struct ent *p;

    p = lookat(currow, curcol);
    if (p->nrow == -1) {
        error("No note attached");
    } else {
        moveto(p->nrow, p->ncol, p->nlastrow, p->nlastcol, -1, -1);
    }
}

/*
 * Show a cell's label string or expression value.  May overwrite value if
 * there is one already displayed in the cell.  Created from old code in
 * update(), copied with minimal changes.
 */

void showstring(const char *string,         /* to display */
                int align,                  /* ALIGN_xxx */
                int hasvalue,               /* is there a numeric value? */
                int row, int col,           /* spreadsheet location */
                int *nextcolp,              /* value returned through it */
                int mxcol,                  /* last column displayed? */
                int *fieldlenp,             /* value returned through it */
                int r, int c,               /* screen row and column */
                struct frange *fr,          /* frame range we're currently in, if any */
                int frightcols,             /* number of frame columns to the right */
                int flcols, int frcols)     /* width of left and right sides of frame */
{
    int nextcol  = *nextcolp;
    int fieldlen = *fieldlenp;
    char field[FBUFLEN];
    int slen;
    char *start, *last, *fp;
    const char *sp;
    struct ent *nc;
    struct crange *cr;

    cr = find_crange(row, col);

    /* This figures out if the label is allowed to
       slop over into the next blank field */

    slen = strlen(string);
    for (sp = string; *sp != '\0'; sp++) {
        if (*sp == '\\' && sp[1] == '"')
            slen--;
    }
    if (*string == '\\' && string[1] != '\0' && string[1] != '"')
        slen = fwidth[col];
    if (c + fieldlen == rescol + flcols && nextcol < stcol)
        nextcol = stcol;
    if (frightcols &&
            c + fieldlen + fwidth[nextcol] >= COLS - 1 - frcols &&
            nextcol < fr->or_right->col - frightcols + 1)
        nextcol = fr->or_right->col - frightcols + 1;
    while ((slen > fieldlen) && (nextcol <= mxcol) &&
            !((nc = lookat(row, nextcol))->flags & IS_VALID) && !(nc->label) &&
            (cslop || find_crange(row, nextcol) == cr)) {

        if (!col_hidden[nextcol])
            fieldlen += fwidth[nextcol];
        nextcol++;
        if (c + fieldlen == rescol + flcols && nextcol < stcol)
            nextcol = stcol;
        if (frightcols &&
                c + fieldlen + fwidth[nextcol] >= COLS - 1 - frcols &&
                nextcol < fr->or_right->col - frightcols + 1)
            nextcol = fr->or_right->col - frightcols + 1;
    }
    if (slen > fieldlen)
        slen = fieldlen;

    /* Now justify and print */
    // XXX: should allow center and right to bleed to the left over empty cells
    if (align == ALIGN_CENTER)
        start = field + ((slen < fwidth[col]) ? (fieldlen - slen) / 2 : 0);
    else
        start = (align == ALIGN_RIGHT) ? field + fieldlen - slen : field;
    last = field + fieldlen;
    fp = field;
    if (slen) {
        while (fp < start)
            *fp++ = ' ';
    }
    if (*string == '\\' && string[1] != '\0' && string[1] != '"') {
        const char *strt = ++string;

        while (slen--) {
            if (*string == '\\' && string[1] == '"') {
                slen++;
                string++;
            } else
                *fp++ = *string++;
            if (*string == '\0')
                string = strt;
        }
    } else {
        while (slen--) {
            if (*string == '\\' && string[1] == '"') {
                slen++;
                string++;
            } else
                *fp++ = *string++;
        }
    }

    if (!hasvalue || fieldlen != fwidth[col]) {
        while (fp < last)
            *fp++ = ' ';
    }
    *fp = '\0';
    for (fp = field; *fp != '\0'; fp++) {
        if (*fp == '\\' && fp[1] == '"')
            strsplice(field, sizeof field, fp - field, 1, NULL, 0);
    }
    mvaddstr(r, c, field);

    *nextcolp  = nextcol;
    *fieldlenp = fieldlen;
}

int etype(struct enode *e) {
    if (e == NULL)
        return NUM;
    switch (e->op) {
    case UPPER: case LOWER: case CAPITAL:
    case O_SCONST: case '#': case DATE: case FMT: case STINDEX:
    case EXT: case SVAL: case SUBSTR:
        return STR;

    case '?':
    case IF:
        return etype(e->e.o.right->e.o.left);

    case 'f':
        return etype(e->e.o.right);

    case O_VAR: {
            struct ent *p;
            p = e->e.v.vp;
            if (p->expr)
                return (p->flags & IS_STREXPR) ? STR : NUM;
            else if (p->label)
                return STR;
            else
                return NUM;
        }

    default:
        return NUM;
    }
}

/* return 1 if yes given, 0 otherwise */
int yn_ask(const char *msg) {
    move(0, 0);
    clrtoeol();
    addstr(msg);
    refresh();
    // should clear screen row 0 upon returning
    for (;;) {
        switch (nmgetch(0)) {
        case 'y':
        case 'Y':
            return 1;
        case 'n':
        case 'N':
            return 0;
        case ctl('g'):
        case ESC:
        case EOF:
            return -1;
        }
    }
}

/* expand a ~ in a path to your home directory */
#ifndef VMS
#include <pwd.h>
#endif
char *findhome(char *path, size_t pathsiz) {
    static const char *HomeDir = NULL;

    if (*path == '~') {
        char *pathptr;
        char tmppath[PATHLEN];

        if (HomeDir == NULL) {
            HomeDir = getenv("HOME");
            if (HomeDir == NULL)
                HomeDir = "/";
        }
        pathptr = path + 1;
        if ((*pathptr == '/') || (*pathptr == '\0'))
            strlcpy(tmppath, HomeDir, sizeof tmppath);
#ifndef VMS
        else {
            struct passwd *pwent;
            char *namep;
            char name[50];

            namep = name;
            while ((*pathptr != '\0') && (*pathptr != '/'))
                    *namep++ = *pathptr++;
            *namep = '\0';
            if ((pwent = getpwnam(name)) == NULL) {
                error("Can't find user %s", name);
                return NULL;
            }
            strlcpy(tmppath, pwent->pw_dir, sizeof tmppath);
        }
#endif
        strlcat(tmppath, pathptr, sizeof tmppath);
        strlcpy(path, tmppath, pathsiz);
    }
    return path;
}

/*
 * make a backup copy of a file, use the same mode and name in the format
 * [path/]file~
 * return 1 if we were successful, 0 otherwise
 */
int backup_file(const char *path) {
    struct stat statbuf;
    struct utimbuf timebuf;
    char tpath[PATHLEN];
    char sbuf[BUFSIZ];
    char *buf = sbuf;
    size_t buflen = sizeof buf;
    int infd, outfd, rc = 1;
    int count, wpos, wc;
    mode_t oldumask;

    /* tpath will be the [path/]file ---> [path/]file~ */
    if (snprintf(tpath, sizeof tpath, "%s~", path) >= (int)(sizeof tpath))
        return 0;

    if (stat(path, &statbuf))
        return (errno == ENOENT);

    if ((infd = open(path, O_RDONLY, 0)) < 0)
        return 0;

    // XXX: if path is read-only, open for writing might fail
    oldumask = umask(0);
    outfd = open(tpath, O_TRUNC | O_WRONLY | O_CREAT, statbuf.st_mode);
    umask(oldumask);
    if (outfd < 0) {
        close(infd);
        return 0;
    }
    /* if we know the optimum block size, use it */
    if ((int)buflen < (int)statbuf.st_blksize) {
        buflen = (int)statbuf.st_blksize;
        if ((buf = scxmalloc(buflen)) == NULL) {
            buf = sbuf;
            buflen = sizeof sbuf;
        }
    }
    chown(tpath, statbuf.st_uid, statbuf.st_gid);

    rc = 1;
    while (rc) {
        count = read(infd, buf, buflen);
        if (count <= 0) {
            if (count < 0) {
                if (errno == EINTR)
                    continue;
                rc = 0;
            }
            break;
        }
        for (wpos = 0; wpos < count; wpos += wc) {
            wc = write(outfd, buf + wpos, count - wpos);
            if (wc <= 0) {
                if (wc < 0) {
                    wc = 0;
                    if (errno == EINTR)
                        continue;
                }
                rc = 0;
                break;
            }
        }
    }
    if (buf != sbuf)
        scxfree(buf);
    close(infd);
    close(outfd);
    if (rc) {
        /* copy access and modification times from original file */
        timebuf.actime = statbuf.st_atime;
        timebuf.modtime = statbuf.st_mtime;
        utime(tpath, &timebuf);
    } else {
        unlink(tpath);
    }
    return rc;
}

/* set the calculation order */
void setcalcorder(int i) {
    if (i == BYROWS || i == BYCOLS)
        calc_order = i;
}

void setautocalc(int i) {
    autocalc = i;
}

/* check if tbl was modified and ask to save */
int modcheck(const char *endstr) {
    int yn_ans;

    if (modflg && curfile[0]) {
        char lin[100];

        snprintf(lin, sizeof lin, "File \"%s\" is modified, save%s? ", curfile, endstr);
        if ((yn_ans = yn_ask(lin)) < 0)
            return 1;
        else
        if (yn_ans == 1) {
            if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
                return 1;
        }
    } else if (modflg) {
        if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
            return 1;
        else
            return yn_ans;
    }
    return 0;
}

/* Returns 1 if cell is locked, 0 otherwise */
int locked_cell(int r, int c) {
    struct ent *p = *ATBL(tbl, r, c);
    if (p && (p->flags & IS_LOCKED)) {
        error("Cell %s%d is locked", coltoa(c), r) ;
        return 1;
    }
    return 0;
}

/* Check if area contains locked cells */
int any_locked_cells(int r1, int c1, int r2, int c2) {
    int r, c;
    struct ent *p;

    for (r = r1; r <= r2; r++) {
        for (c = c1; c <= c2; c++) {
            p = *ATBL(tbl, r, c);
            if (p && (p->flags & IS_LOCKED))
                return 1;
        }
    }
    return 0;
}

void sc_set_locale(int set) {
    dpoint = '.';
    thsep = ',';
    FullUpdate++;

    if (set) {
#ifdef USELOCALE
        struct lconv *locstruct;
        char *loc = setlocale(LC_ALL, "");
        if (loc != NULL) {
            locstruct = localeconv();
            dpoint = locstruct->decimal_point[0];
            thsep = locstruct->thousands_sep[0];
        }
#else
        error("Locale support not available");
#endif
    }
}

int cmd_plugin(const char *str) {
    char buf[PATHLEN];
    snprintf(buf, sizeof buf, "|%s", str);
    return readfile(buf, 0);
}

void set_mdir(const char *str) {
    set_cstring(&mdir, str && *str ? str : NULL);
    modflg++;
}

void set_autorun(const char *str) {
    set_cstring(&autorun, str && *str ? str : NULL);
    modflg++;
}

void set_fkey(int n, const char *str) {
    if (n > 0 && n <= FKEYS) {
        set_cstring(&fkey[n - 1], str && *str ? str : NULL);
        modflg++;
    } else {
        error("Invalid function key");
    }
}

void set_histfile(const char *str) {
    strlcpy(histfile, str, sizeof histfile);
}

void cmd_select_qbuf(char c) {
    if (c >= '0' && c <= '9') {
        qbuf = c - '0' + (DELBUFSIZE - 10);
    } else if (c >= 'a' && c <= 'z') {
        qbuf = c - 'a' + (DELBUFSIZE - 36);
    } else if (c == '"') {
        qbuf = 0;
    } else {
        error("Invalid buffer");
    }
}

void cmd_setformat(int n, const char *str) {
    if (n >= 0 && n < 10) {
        set_cstring(&colformat[n], str && *str ? str : NULL);
        FullUpdate++;
        modflg++;
    } else {
        error("Invalid format number");
    }
}

void cmd_redraw(void) {
    if (usecurses) {
        clearok(stdscr, TRUE);
        //linelim = -1;
        update(1);
        refresh();
        changed = 0;
    }
}

void cmd_run(const char *str) {
    deraw(1);
    system(str);
    if (*str && str[strlen(str) - 1] != '&') {
        printf("Press any key to continue ");
        fflush(stdout);
        cbreak();
        nmgetch(0);
    }
    goraw();
}

void cmd_whereami(int fd) {
    char buf[64];
    snprintf(buf, sizeof buf, "%s%d %s%d\n",
             coltoa(curcol), currow, coltoa(stcol), strow);
    write(fd, buf, strlen(buf));
}

void cmd_define(const char *name) {
    struct ent_ptr arg1, arg2;
    arg1.vp = lookat(showsr, showsc);
    arg1.vf = 0;
    arg2.vp = lookat(currow, curcol);
    arg2.vf = 0;
    if (showrange && arg1.vp != arg2.vp)
        add_range(name, arg1, arg2, 1);
    else
        add_range(name, arg2, arg2, 0);
}

void addnote(struct ent *p, int sr, int sc, int er, int ec) {
    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    p->nrow = sr;
    p->ncol = sc;
    p->nlastrow = er;
    p->nlastcol = ec;
    p->flags |= IS_CHANGED;
    FullUpdate++;
    modflg++;
}

void delnote(struct ent *p) {
    if (p) {
        p->nrow = p->ncol = -1;
        p->flags |= IS_CHANGED;
        modflg++;
    }
}

char *set_string(SCXMEM char **pp, SCXMEM char *s) {
    scxfree(*pp);
    return *pp = s;
}

char *set_cstring(SCXMEM char **pp, const char *s) {
    scxfree(*pp);
    return *pp = s ? scxdup(s) : NULL;
}

void cmd_recalc(void) {
    EvalAll();
    update(1);
    changed = 0;
}
