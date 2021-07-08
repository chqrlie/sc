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

#include <fcntl.h>      // because erasedb reads .scrc files
#include "sc.h"

/* a linked list of free [struct ent]'s, uses .next as the pointer */
static struct ent *freeents = NULL;

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
SCXMEM struct ent *delbuf[DELBUFSIZE];
SCXMEM char *delbuffmt[DELBUFSIZE];
int dbidx = -1;

static struct ent *deldata1(void);
static void deldata2(struct ent *obuf);
static int any_locked_cells(int r1, int c1, int r2, int c2);

/* move a cell to the delbuf list */
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

    p = delbuf[dbidx];
    delbuf[dbidx] = NULL;
    while (p) {
        // XXX: entries are added to freeents in reverse order
        q = p->next;
        clearent(p);
        p->next = freeents;     /* put this ent on the front of freeents */
        freeents = p;
        p = q;
    }
    scxfree(delbuffmt[dbidx]);
    delbuffmt[dbidx] = NULL;
    dbidx--;
}

void clearent(struct ent *v) {
    if (v) {
        v->v = 0.0;
        set_string(&v->label, NULL);
        efree(v->expr);
        v->expr = NULL;
        set_string(&v->format, NULL);
        v->cellerror = 0;
        v->flags = IS_CHANGED | IS_CLEARED;
        v->nrr = rangeref_empty();
        // XXX: should clear other fields?
        //      next
        FullUpdate++;  // XXX: really?
        changed++;
        modflg++;
    }
}

struct ent *lookat_nc(int row, int col) {
    if (row >= 0 && row <= maxrow && col >= 0 && col <= maxcol)
        return *ATBL(tbl, row, col);
    else
        return NULL;
}

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
        p->nrr = rangeref_empty();
        p->next = NULL;
        *pp = p;
    }
    return *pp;
}

void range_normalize(rangeref_t *rr) {
    if (rr->left.row > rr->right.row) {
        int row = rr->left.row;
        rr->left.row = rr->right.row;
        rr->right.row = row;
        rr->left.vf ^= rr->right.vf & FIX_ROW;
        rr->right.vf ^= rr->left.vf & FIX_ROW;
        rr->left.vf ^= rr->right.vf & FIX_ROW;
    }
    if (rr->left.col > rr->right.col) {
        int col = rr->left.col;
        rr->left.col = rr->right.col;
        rr->right.col = col;
        rr->left.vf ^= rr->right.vf & FIX_COL;
        rr->right.vf ^= rr->left.vf & FIX_COL;
        rr->left.vf ^= rr->right.vf & FIX_COL;
    }
}

/* copy the current row (currow) and place the cursor in the new row */
void duprow(void) {
    int row = currow, col = curcol;
    int c1 = 0;
    int c2 = maxcol;
    struct frange *fr;

    if ((fr = find_frange(row, col))) {
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
    }

    // XXX: get rid of this test and check insertrow return value
    if (row + 1 >= maxrows
    ||  (!fr && maxrow + 1 >= maxrows)
    ||  (fr && fr->or_right->row + 1 >= maxrows)) {
        if (!growtbl(GROWROW, 0, 0))
            return;
    }
    insertrow(1, 1);
    modflg++;
    // XXX: should use copy(row + 1, c1, row + 1, c2, row, c1, row, c2)
    for (col = c1; col <= c2; col++) {
        struct ent *p = *ATBL(tbl, row, col);
        if (p) {
            struct ent *n = lookat(row + 1, col);
            copyent(n, p, 1, 0, 0, 0, maxrow, maxcol, 0);
        }
    }
    currow = row + 1;
}

/* copy the current column (curcol) and place the cursor in the new column */
void dupcol(void) {
    int row, col = curcol;

    // XXX: get rid of this test and check insertcol return value
    if (col + 1 >= maxcols || maxcol + 1 >= maxcols) {
        if (!growtbl(GROWCOL, 0, 0))
            return;
    }
    modflg++;
    insertcol(1, 1);
    // XXX: should use copy(0, col + 1, maxrow, col + 1,
    //                      0, col, maxrow, col)
    for (row = 0; row <= maxrow; row++) {
        struct ent *p = *ATBL(tbl, row, col);
        if (p) {
            struct ent *n = lookat(row, col + 1);
            copyent(n, p, 0, 1, 0, 0, maxrow, maxcol, 0);
        }
    }
    curcol = col + 1;
}

#if 0
static void fix_cellref(cellref_t *rp, rangeref_t rr, int dr, int dc) {
    /* if a cell reference is inside the target range, shift it */
    if (rp->row >= rr.left.row && rp->row <= rr.right.row
    &&  rp->col >= rr.left.col && rp->col <= rr.right.col) {
        rp->row += dr;
        rp->col += dc;
    }
}
#endif

/* Insert 'arg' rows.  The row(s) will be inserted before currow if delta
 * is 0; after if it is 1.
 */
void insertrow(int arg, int delta) {
    int r, c, i;
    struct ent **tmprow;
    int lim = maxrow - currow + 1;
    rangeref_t rr = rangeref(currow + delta, 0, maxrow, maxcol);
    struct frange *fr;

    // XXX: this is bogus! maxrow should never be >= maxrows
    if (currow > maxrow)
        maxrow = currow;
    maxrow += arg;
    lim = maxrow - lim + delta;

    if ((maxrow >= maxrows) && !growtbl(GROWROW, maxrow, 0))
        return;

    if ((fr = find_frange(currow + delta, curcol))) {
        rr = rangeref(currow + delta, fr->or_left->col,
                      fr->or_right->row, fr->or_right->col);
        move_area(rr.left.row + arg, rr.left.col,
                  rr.left.row, rr.left.col, rr.right.row, rr.right.col);
        if (!delta && fr->ir_left->row == currow + arg)
            fr->ir_left = lookat(fr->ir_left->row - arg, fr->ir_left->col);
        if (delta && fr->ir_right->row == currow)
            fr->ir_right = lookat(fr->ir_right->row + arg, fr->ir_right->col);

        for (i = 0; i < 37; i++) {      /* update all marked cells */
            if (savedrow[i] >= rr.left.row &&
                savedcol[i] >= rr.left.col &&
                savedcol[i] <= rr.right.col)
                savedrow[i] += arg;
            if (savedstrow[i] >= rr.left.row &&
                savedstcol[i] >= rr.left.col &&
                savedstcol[i] <= rr.right.col)
                savedstrow[i] += arg;
        }
        if (gs.g_rr.left.row >= rr.left.row &&
            gs.g_rr.left.col >= rr.left.col &&
            gs.g_rr.left.col <= rr.right.col)
            gs.g_rr.left.row += arg;
        if (gs.g_rr.right.row >= rr.left.row &&
            gs.g_rr.right.col >= rr.left.col &&
            gs.g_rr.right.col <= rr.right.col)
            gs.g_rr.right.row += arg;
        if (gs.strow >= rr.left.row &&
            gs.stcol >= rr.left.col &&
            gs.stcol <= rr.right.col)
            gs.strow += arg;
        for (r = 0; r <= maxrow; r++) {
            for (c = 0; c <= maxcol; c++) {
                struct ent *p = *ATBL(tbl, r, c);
                if (p && (p->flags & HAS_NOTE)) {
                    if (p->nrr.left.row >= rr.left.row &&
                        p->nrr.left.col >= rr.left.col &&
                        p->nrr.left.col <= rr.right.col)
                        p->nrr.left.row += arg;
                    if (p->nrr.right.row >= rr.left.row &&
                        p->nrr.right.col >= rr.left.col &&
                        p->nrr.right.col <= rr.right.col)
                        p->nrr.right.row += arg;
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
            if (savedrow[i] >= rr.left.row)
                savedrow[i] += arg;
            if (savedstrow[i] >= rr.left.row)
                savedstrow[i] += arg;
        }
        if (gs.g_rr.left.row >= rr.left.row)
            gs.g_rr.left.row += arg;
        if (gs.g_rr.right.row >= rr.left.row)
            gs.g_rr.right.row += arg;
        if (gs.strow >= rr.left.row)
            gs.strow += arg;
        for (r = 0; r <= maxrow; r++) {
            for (c = 0; c <= maxcol; c++) {
                struct ent *p = *ATBL(tbl, r, c);
                if (p && (p->flags & HAS_NOTE)) {
                    if (p->nrr.left.row >= rr.left.row)
                        p->nrr.left.row += arg;
                    if (p->nrr.right.row >= rr.left.row)
                        p->nrr.right.row += arg;
                }
            }
        }
    }
    // XXX: cell coordinates have been updated
    fr = find_frange(currow, curcol);
    if (delta) {
        fix_ranges(currow, -1, currow, -1, 0, arg, fr);
        currow += delta;
    } else {
        fix_ranges(currow + arg, -1, currow + arg, -1, arg, 0, fr);
    }
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
    if (gs.g_rr.left.col >= curcol + delta)
        gs.g_rr.left.col += arg;
    if (gs.g_rr.right.col >= curcol + delta)
        gs.g_rr.right.col += arg;
    if (gs.stcol >= curcol + delta)
        gs.stcol += arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.col >= curcol + delta)
                    p->nrr.left.col += arg;
                if (p->nrr.right.col >= curcol + delta)
                    p->nrr.right.col += arg;
            }
        }
    }

    // XXX: cell coordinates have been updated
    fr = find_frange(currow, curcol);
    if (delta) {
        if (fr && fr->ir_right->col == curcol)
            fr->ir_right = lookat(fr->ir_right->row, fr->ir_right->col + arg);
        fix_ranges(-1, curcol, -1, curcol, 0, arg, fr);
        curcol += delta;
    } else {
        if (fr && fr->ir_left->col == curcol + arg)
            fr->ir_left = lookat(fr->ir_left->row, fr->ir_left->col - arg);
        fix_ranges(-1, curcol + arg, -1, curcol + arg, arg, 0, fr);
    }
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
            fix_ranges(r1, -1, r2, -1, -1, -1, fr);
            deldata2(obuf);
            if (r1 + nrows > fr->ir_right->row && fr->ir_right->row >= r1)
                fr->ir_right = lookat(r1 - 1, fr->ir_right->col);
            if (r1 + nrows > fr->or_right->row) {
                fr->or_right = lookat(r1 - 1, fr->or_right->col);
            } else {
                move_area(r1, fr->or_left->col,
                          r1 + nrows, fr->or_left->col,
                          fr->or_right->row, fr->or_right->col);
            }
            if (fr->ir_left->row > fr->ir_right->row) {
                del_frange(fr);
                fr = NULL;
            }
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
            if (gs.g_rr.left.col >= fr->or_left->col && gs.g_rr.left.col <= fr->or_right->col) {
                if (gs.g_rr.left.row >= r1 && gs.g_rr.left.row <= r2)
                    gs.g_rr.left.row = r1;
                if (gs.g_rr.left.row > r2)
                    gs.g_rr.left.row -= nrows;
            }
            if (gs.g_rr.right.col >= fr->or_left->col && gs.g_rr.right.col <= fr->or_right->col) {
                if (gs.g_rr.right.row >= r1 && gs.g_rr.right.row <= r2)
                    gs.g_rr.right.row = r1 - 1;
                if (gs.g_rr.right.row > r2)
                    gs.g_rr.right.row -= nrows;
            }
            if (gs.g_rr.left.row > gs.g_rr.right.row)
                gs.g_rr.left.row = gs.g_rr.left.col = -1;
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
            fix_ranges(r1, -1, r2, -1, -1, -1, NULL);
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
    // XXX: not freeing delbuf[dbidx], delbuffmt[dbidx]
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

    ++dbidx;
    // XXX: not setting delbuf[dbidx]?
    delbuffmt[dbidx] = scxmalloc((4*(ec-sc+1)+(er-sr+1))*sizeof(char));
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
    delbuffmt[dbidx] = NULL;
    dbidx--;
}

/*
 * deletes the expression associated w/ a cell and turns it into a constant
 * containing whatever was on the screen
 */
void valueize_area(rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p && p->expr) {
                if (p->flags & IS_LOCKED) {
                    error(" Cell %s%d is locked", coltoa(c), r);
                    continue;
                }
                efree(p->expr);
                p->expr = NULL;
                p->flags &= ~IS_STREXPR;
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
        ++dbidx;
        delbuf[dbidx] = delbuf[qbuf];
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
        delbuffmt[dbidx] = NULL;
        dbidx--;
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
    if (gs.g_rr.left.row >= rs && gs.g_rr.left.row < rs + numrow)
        gs.g_rr.left.row = rs;
    if (gs.g_rr.left.row >= rs + numrow)
        gs.g_rr.left.row -= numrow;
    if (gs.g_rr.right.row >= rs && gs.g_rr.right.row < rs + numrow)
        gs.g_rr.right.row = rs - 1;
    if (gs.g_rr.right.row >= rs + numrow)
        gs.g_rr.right.row -= numrow;
    if (gs.g_rr.left.row > gs.g_rr.right.row)
        gs.g_rr.left.row = gs.g_rr.left.col = -1;
    if (gs.strow >= rs && gs.strow < rs + numrow)
        gs.strow = rs;
    if (gs.strow >= rs + numrow)
        gs.strow -= numrow;

    maxrow -= numrow;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.row >= rs && p->nrr.left.row < rs + numrow)
                    p->nrr.left.row = rs;
                if (p->nrr.left.row >= rs + numrow)
                    p->nrr.left.row -= numrow;
                if (p->nrr.right.row >= rs && p->nrr.right.row < rs + numrow)
                    p->nrr.right.row = rs - 1;
                if (p->nrr.right.row >= rs + numrow)
                    p->nrr.right.row -= numrow;
                if (p->nrr.right.row < p->nrr.left.row)
                    p->nrr.left.row = p->nrr.left.col = -1;
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
    fix_ranges(-1, c1, -1, c2, -1, -1, find_frange(currow, curcol));
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
    if (gs.g_rr.left.col >= c1 && gs.g_rr.left.col <= c2)
        gs.g_rr.left.col = c1;
    if (gs.g_rr.left.col > c2)
        gs.g_rr.left.col -= ncols;
    if (gs.g_rr.right.col >= c1 && gs.g_rr.right.col <= c2)
        gs.g_rr.right.col = c1 - 1;
    if (gs.g_rr.right.col > c2)
        gs.g_rr.right.col -= ncols;
    if (gs.g_rr.left.col > gs.g_rr.right.col)
        gs.g_rr.left.row = gs.g_rr.left.col = -1;
    if (gs.stcol >= c1 && gs.stcol <= c2)
        gs.stcol = c1;
    if (gs.stcol > c2)
        gs.stcol -= ncols;

    maxcol -= ncols;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            p = *ATBL(tbl, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.col >= c1 && p->nrr.left.col <= c2)
                    p->nrr.left.col = c1;
                if (p->nrr.left.col > c2)
                    p->nrr.left.col -= ncols;
                if (p->nrr.right.col >= c1 && p->nrr.right.col <= c2)
                    p->nrr.right.col = c1 - 1;
                if (p->nrr.right.col > c2)
                    p->nrr.right.col -= ncols;
                if (p->nrr.right.col < p->nrr.left.col)
                    p->nrr.right.row = p->nrr.left.col = -1;
            }
        }
    }
    rowsinrange = 1;
    colsinrange = fwidth[c1];

    FullUpdate++;
    modflg++;
    curcol = save < c1 ? save : (save <= c2) ? c1 : save - ncols;
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

void range_align(rangeref_t rr, int align) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p && (p->flags & ALIGN_MASK) != align) {
                p->flags &= ~ALIGN_MASK;
                p->flags |= IS_CHANGED | align;
                changed++;
                modflg++;
            }
        }
    }
}

/*---------------- copying and moving ----------------*/

static void copydbuf(int deltar, int deltac) {
    int vr, vc;
    struct ent *p = delbuf[dbidx];
    struct ent *n;

    while (p) {
        vr = p->row + deltar;
        vc = p->col + deltac;
        n = lookat(vr, vc);
        if (n->flags & IS_LOCKED)
            continue;
        copyent(n, p, deltar, deltac, 0, 0, maxrow, maxcol, 0);
        p = p->next;
    }
}

// XXX: get rid of this static mess
static int copy_minsr = -1, copy_minsc = -1;
static int copy_maxsr = -1, copy_maxsc = -1;

void copy_set_source_range(int r1, int c1, int r2, int c2) {
    copy_minsr = r1;
    copy_minsc = c1;
    copy_maxsr = r2;
    copy_maxsc = c2;
}

void copy(int flags, rangeref_t drr, rangeref_t srr) {
    struct ent *p;
    int mindr, mindc, maxdr, maxdc;
    int minsr, minsc, maxsr, maxsc;
    int deltar, deltac;

    range_normalize(&drr);
    mindr = drr.left.row;
    mindc = drr.left.col;
    maxdr = drr.right.row;
    maxdc = drr.right.col;

    if (flags & COPY_FROM_RANGE) {
        range_normalize(&drr);
        minsr = srr.left.row;
        minsc = srr.left.col;
        maxsr = srr.right.row;
        maxsc = srr.right.col;
    } else
    if (flags & COPY_FROM_QBUF) {
        if (qbuf && delbuf[qbuf]) {
            ++dbidx;
            delbuf[dbidx] = delbuf[qbuf];
            delbuffmt[dbidx] = delbuffmt[qbuf];
        } else if (dbidx < 0)
            return;
        minsr = maxrow;
        minsc = maxcol;
        maxsr = 0;
        maxsc = 0;
        // XXX: delbufs should behave like worksheets
        //      there should be a defined range,
        //      col and row properties and
        //      a sparse cell array.
        for (p = delbuf[dbidx]; p; p = p->next) {
            if (p->row < minsr) minsr = p->row;
            if (p->row > maxsr) maxsr = p->row;
            if (p->col < minsc) minsc = p->col;
            if (p->col > maxsc) maxsc = p->col;
        }
    } else
    if (flags & COPY_FROM_DEF) {
        // XXX: use static values: check if we can avoid it
        minsr = copy_minsr;
        minsc = copy_minsc;
        maxsr = copy_maxsr;
        maxsc = copy_maxsc;
        if (minsr == -1)
            return;
    } else {
        return;
    }

    checkbounds(&maxdr, &maxdc);

    if (maxdr - mindr < maxsr - minsr) maxdr = mindr + (maxsr - minsr);
    if (maxdc - mindc < maxsc - minsc) maxdc = mindc + (maxsc - minsc);
    if (!(flags & COPY_FROM_QBUF)) {
        /* copy source cells to qbuf */
        // XXX: should not be necessary if copy order is well chosen
        //      or if source and destination do not overlap
        yank_area(minsr, minsc, maxsr, maxsc);
    }

    erase_area(mindr, mindc, maxdr, maxdc, 0);

    sync_refs();
    flush_saved();

    error("Copying...");
    if (!loading)
        refresh();
    p = delbuf[dbidx];
    if (minsr == maxsr && minsc == maxsc) {
        /* Source is a single cell */
        for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++) {
            for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
                copydbuf(deltar, deltac);
        }
    } else
    if (minsr == maxsr) {
        /* Source is a single row */
        deltac = mindc - p->col;
        for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++)
            copydbuf(deltar, deltac);
    } else
    if (minsc == maxsc) {
        /* Source is a single column */
        deltar = mindr - p->row;
        for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
            copydbuf(deltar, deltac);
    } else {
        /* Everything else */
        deltar = mindr - p->row;
        deltac = mindc - p->col;
        copydbuf(deltar, deltac);
    }

    if (!(flags & COPY_FROM_QBUF)) {
        sync_refs();
        flush_saved();
    }

    if (flags & COPY_FROM_QBUF) {
        if (qbuf && delbuf[qbuf]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            dbidx--;
        }
        qbuf = 0;
    }
    error("Copy done.");
}

/* ERASE a Range of cells */
void eraser(rangeref_t rr) {
    int i;
    struct ent *obuf = NULL;

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
    erase_area(rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);
    sync_refs();
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
    FullUpdate++;
    modflg++;
}

/* YANK a Range of cells */
void yankr(rangeref_t rr) {
    int i, qtmp;
    struct ent *obuf = NULL;

    if (dbidx < 0) dbidx++;
    delbuf[dbidx] = delbuf[DELBUFSIZE - 10];
    delbuf[DELBUFSIZE - 10] = NULL;
    delbuffmt[dbidx] = delbuffmt[DELBUFSIZE - 10];
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
    yank_area(rr.left.row, rr.left.col, rr.right.row, rr.right.col);
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

/* MOVE a Range of cells */
void mover(cellref_t cr, rangeref_t rr) {
    move_area(cr.row, cr.col,
              rr.left.row, rr.left.col, rr.right.row, rr.right.col);
    sync_refs();
    FullUpdate++;
}

/* fill a range with constants */
void fillr(rangeref_t rr, double start, double inc) {
    int r, c;

    range_normalize(&rr);

    FullUpdate++;
    if (calc_order == BYROWS) {
        for (r = rr.left.row; r <= rr.right.row; r++) {
            for (c = rr.left.col; c <= rr.right.col; c++) {
                struct ent *n = lookat(r, c);
                if (n->flags & IS_LOCKED) continue;
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->flags |= IS_CHANGED | IS_VALID;
                n->flags &= ~IS_CLEARED;
            }
        }
    } else
    if (calc_order == BYCOLS) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            for (r = rr.left.row; r <= rr.right.row; r++) {
                struct ent *n = lookat(r, c);
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->flags |= IS_CHANGED | IS_VALID;
                n->flags &= ~IS_CLEARED;
            }
        }
    } else {
        error(" Internal error calc_order");
    }
    changed++;
}

/* lock a range of cells */

void lock_cells(rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *n = lookat(r, c);
            // XXX: update IS_CHANGED?
            n->flags |= IS_LOCKED;
        }
    }
}

/* unlock a range of cells */

void unlock_cells(rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *n = lookat_nc(r, c);
            if (n) {
                // XXX: update IS_CHANGED?
                n->flags &= ~IS_LOCKED;
            }
        }
    }
}

void format_cells(rangeref_t rr, const char *s) {
    int r, c;

    FullUpdate++;
    modflg++;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *n = lookat(r, c);
            if (locked_cell(n))
                continue;
            set_cstring(&n->format, s && *s ? s : NULL);
            n->flags |= IS_CHANGED;
        }
    }
}

/*
 * sync_refs and syncref are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
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
            set_string(&n->label, NULL);
        }
        n->flags &= ~ALIGN_MASK;
        n->flags |= p->flags & ALIGN_MASK;
        n->flags |= p->flags & IS_LOCKED;
    }
    if (p->format) {
        set_cstring(&n->format, p->format);
    } else if (special != 'm' && special != 'f') {
        set_string(&n->format, NULL);
    }
    n->flags |= IS_CHANGED;
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
    clean_nrange();
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

/* Returns 1 if cell is locked, 0 otherwise */
int locked_cell(struct ent *p) {
    if (p && (p->flags & IS_LOCKED)) {
        error("Cell %s%d is locked", coltoa(p->col), p->row);
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

void addnote(cellref_t cr, rangeref_t rr) {
    struct ent *p = lookat(cr.row, cr.col);
    if (p) {
        range_normalize(&rr);
        p->nrr = rr;
        p->flags |= HAS_NOTE | IS_CHANGED;
        FullUpdate++;
        modflg++;
    }
}

void delnote(cellref_t cr) {
    struct ent *p = lookat_nc(cr.row, cr.col);
    if (p && (p->flags & HAS_NOTE)) {
        p->nrr = rangeref_empty();
        p->flags ^= HAS_NOTE;
        p->flags |= IS_CHANGED;
        modflg++;
    }
}
