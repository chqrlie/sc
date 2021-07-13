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
/* temporary sheet fragments: 4 work buffers and 36 named buffers (a-z,0-9) */
SCXMEM struct ent *delbuf[DELBUFSIZE];
SCXMEM unsigned char *delbuffmt[DELBUFSIZE];
int dbidx = -1;

static struct ent *qbuf_was_here;  /* dummy cell structure for qbuf patching */

static int any_locked_cells(int r1, int c1, int r2, int c2);

/* move a cell to the delbuf list (reverse order) */
static void free_ent(int idx, struct ent *p, int unlock) {
    p->next = delbuf[idx];
    delbuf[idx] = p;
    p->flags |= IS_DELETED;
    if (unlock)
        p->flags &= ~IS_LOCKED;
}

/* swap 2 entries in the delbuf array */
static void deldata_swap(int idx1, int idx2) {
    struct ent *tmpbuf;
    unsigned char *tmpfmt;

    tmpbuf = delbuf[idx1];
    delbuf[idx1] = delbuf[idx2];
    delbuf[idx2] = tmpbuf;
    tmpfmt = delbuffmt[idx1];
    delbuffmt[idx1] = delbuffmt[idx2];
    delbuffmt[idx2] = tmpfmt;
}

/* rotate a range of entries in the delbuf array */
static void deldata_rotate(int idx1, int idx2) {
    while (idx2 --> idx1) {
        deldata_swap(idx2 + 1, idx2);
    }
}

/* discard a named buffer unless it is duplicated */
// XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
static void deldata_discard_buf(int idx) {
    int i;

    if (dbidx < 0) dbidx = 0;
    // XXX: assuming delbuf[dbidx], delbuffmt[dbidx] are NULL
    //      or can be overwritten without leakage
    //      should check if delbuf[idx] is unique and flush it
    delbuf[dbidx] = delbuf[idx];
    delbuffmt[dbidx] = delbuffmt[idx];
    delbuf[idx] = NULL;
    delbuffmt[idx] = NULL;
    for (i = dbidx + 1; i < DELBUFSIZE; i++) {
        if (delbuf[i] == delbuf[dbidx]) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            break;
        }
    }
    /* free delbuf[dbidx--] */
    flush_saved(dbidx--);
}

/* discard delbuf[qbuf], set qbuf_was_here on all duplicates */
// XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
static void deldata_discard_qbuf(void) {
    if (qbuf) {
        // XXX: delbuf[qbuf] is set to qbuf_was_here
        // XXX: should set duplicates to qbuf_was_here and flush delbuf[idx]
        struct ent *obuf = delbuf[qbuf];
        if (dbidx < 0) dbidx = 0;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
        /* free delbuf[dbidx--] */
        flush_saved(dbidx--);
        if (obuf) {
            int i;
            for (i = 0; i < DELBUFSIZE; i++) {
                if (delbuf[i] == obuf) {
                    delbuf[i] = qbuf_was_here;
                    delbuffmt[i] = NULL;
                }
            }
        }
    }
}

/* store delbuf[dbidx] to delbuf[qbuf] and its copies, clear qbuf */
// XXX: this is confusing. Should use a different approach.
static void deldata_store_qbuf(void) {
    int i;
    for (i = 0; i < DELBUFSIZE; i++) {
        if ((delbuf[i] == qbuf_was_here) || (qbuf && i == qbuf)) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    qbuf = 0;
}

// XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
/* discard named buffer delbuf[idx] if unique and delbuf[qbuf] */
static void deldata_discard(int idx) {
    deldata_discard_buf(idx);
    deldata_discard_qbuf();
}

#define DD_UNSYNC  1

/* store delbuf[dbidx--] to idx1 after rotation and qbuf if any */
static void deldata_store(int idx1, int idx2, int flags) {
    struct ent *p;

    deldata_store_qbuf();
    if (idx1 != idx2) {
        /* shift named buffers 1-8 to 2-9 */
        // XXX: this fails if qbuf was one of the named buffers 1-9
        deldata_rotate(idx1, idx2);
    }
    /* set top of stack in named buffer '1' */
    // XXX: assuming named buffer '1' is NULL
    delbuf[idx1] = delbuf[dbidx];
    delbuffmt[idx1] = delbuffmt[dbidx];
    // XXX: document this
    if (flags & DD_UNSYNC) {
        for (p = delbuf[dbidx]; p; p = p->next)
            p->flags &= ~MAY_SYNC;
    }
}

/* free deleted cells */
int flush_saved(int idx) {
    struct ent *p, *q;

    if (idx < 0)
        return 0;

    p = delbuf[idx];
    delbuf[idx] = NULL;
    while (p) {
        // XXX: entries are added to freeents in reverse order
        //      but they were added to the delbuf in reverse order too
        q = p->next;
        clearent(p);
        p->next = freeents;     /* put this ent on the front of freeents */
        freeents = p;
        p = q;
    }
    scxfree(delbuffmt[idx]);
    delbuffmt[idx] = NULL;
    return 1;
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

rangeref_t *range_normalize(rangeref_t *rr) {
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
    return rr;
}

static rangeref_t *range_clip(rangeref_t *rr) {
    if (rr->left.row < 0) rr->left.row = 0;
    if (rr->left.col < 0) rr->left.col = 0;
    if (rr->right.row > maxrow) rr->right.row = maxrow;
    if (rr->right.col > maxcol) rr->right.col = maxcol;
    return rr;
}

/* duplicate the row at `cr.row` below it into a new row */
int duprow(cellref_t cr) {
    int row = cr.row, col;
    int c1 = 0;
    int c2 = maxcol;
    struct frange *fr;

    if ((fr = find_frange(cr.row, cr.col))) {
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
    }

    if (!insertrow(cr, 1, 1))
        return 0;

    modflg++;
    // XXX: should use copy_area(row + 1, c1, row + 1, c2, row, c1, row, c2)
    for (col = c1; col <= c2; col++) {
        struct ent *p = *ATBL(tbl, row, col);
        if (p) {
            struct ent *n = lookat(row + 1, col);
            copyent(n, p, 1, 0, 0, 0, maxrow, maxcol, 0);
        }
    }
    return 1;
}

/* duplicate the column at `cr.col` to the right it into a new column */
int dupcol(cellref_t cr) {
    int row, col = cr.col;

    if (!insertcol(cr, 1, 1))
        return 0;

    modflg++;
    // XXX: should use copy_area(0, col + 1, maxrow, col + 1,
    //                           0, col, maxrow, col)
    for (row = 0; row <= maxrow; row++) {
        struct ent *p = *ATBL(tbl, row, col);
        if (p) {
            struct ent *n = lookat(row, col + 1);
            copyent(n, p, 0, 1, 0, 0, maxrow, maxcol, 0);
        }
    }
    return 1;
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

/* Insert 'arg' rows.  The row(s) will be inserted before cr.row if delta
 * is 0; after if it is 1.
 * return 0 on failure, 1 on success
 */
int insertrow(cellref_t cr, int arg, int delta) {
    int r, c, i, lim;
    rangeref_t rr = rangeref(cr.row + delta, 0, maxrow, maxcol);
    struct frange *fr;

    if ((maxrow + arg >= maxrows) && !growtbl(GROWROW, maxrow + arg, 0))
        return 0;

    // XXX: should clip cr reference
    lim = cr.row + delta + arg - 1;  /* last inserted row */
    maxrow += arg;

    if ((fr = find_frange(cr.row, cr.col))) {
        // XXX: should verify if cr.row + delta is inside the frange
        // XXX: inconsistent source range: marks are updated beyond rr.right.row
        rr = rangeref(cr.row + delta, fr->or_left->col,
                      fr->or_right->row, fr->or_right->col);
        move_area(rr.left.row + arg, rr.left.col, rr);
        if (!delta && fr->ir_left->row == cr.row + arg)
            fr->ir_left = lookat(fr->ir_left->row - arg, fr->ir_left->col);
        if (delta && fr->ir_right->row == cr.row)
            fr->ir_right = lookat(fr->ir_right->row + arg, fr->ir_right->col);

        for (i = 0; i < 37; i++) {      /* update all marked cells */
            if (savedcr[i].row >= rr.left.row &&
                savedcr[i].col >= rr.left.col &&
                savedcr[i].col <= rr.right.col)
                savedcr[i].row += arg;
            if (savedst[i].row >= rr.left.row &&
                savedst[i].col >= rr.left.col &&
                savedst[i].col <= rr.right.col)
                savedst[i].row += arg;
        }
        if (gs.g_rr.left.row >= rr.left.row &&
            gs.g_rr.left.col >= rr.left.col &&
            gs.g_rr.left.col <= rr.right.col)
            gs.g_rr.left.row += arg;
        if (gs.g_rr.right.row >= rr.left.row &&
            gs.g_rr.right.col >= rr.left.col &&
            gs.g_rr.right.col <= rr.right.col)
            gs.g_rr.right.row += arg;
        if (gs.st.row >= rr.left.row &&
            gs.st.col >= rr.left.col &&
            gs.st.col <= rr.right.col)
            gs.st.row += arg;
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
        struct ent **tmp_row = tbl[maxrow];
        unsigned char tmp_hidden = row_hidden[maxrow];

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
        row_hidden[r] = tmp_hidden;
        tbl[r] = tmp_row;               /* the last row was never used.... */

        for (i = 0; i < 37; i++) {      /* update all marked cells */
            if (savedcr[i].row >= rr.left.row)
                savedcr[i].row += arg;
            if (savedst[i].row >= rr.left.row)
                savedst[i].row += arg;
        }
        if (gs.g_rr.left.row >= rr.left.row)
            gs.g_rr.left.row += arg;
        if (gs.g_rr.right.row >= rr.left.row)
            gs.g_rr.right.row += arg;
        if (gs.st.row >= rr.left.row)
            gs.st.row += arg;
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
    fr = find_frange(cr.row, cr.col);
    if (delta) {
        fix_ranges(cr.row, -1, cr.row, -1, 0, arg, fr);
    } else {
        fix_ranges(cr.row + arg, -1, cr.row + arg, -1, arg, 0, fr);
    }
    FullUpdate++;
    modflg++;
    return 1;
}

/* Insert 'arg' cols.  The col(s) will be inserted before cr.col if delta
 * is 0; after if it is 1.
 * return 0 on failure, 1 on success
 */
int insertcol(cellref_t cr, int arg, int delta) {
    int r, c;
    struct ent **pp;
    /* cols are moved from sc1:sc2 to dc1:dc2 */
    int sc1 = cr.col + delta;
    int sc2 = maxcol;
    int dc1 = sc1 + arg;
    int dc2 = sc2 + arg;
    struct frange *fr;

    if ((maxcol + arg >= maxcols) && !growtbl(GROWCOL, 0, maxcol + arg))
        return 0;

    maxcol += arg;

    for (c = dc2; c >= dc1; c--) {
        fwidth[c] = fwidth[c-arg];
        precision[c] = precision[c-arg];
        realfmt[c] = realfmt[c-arg];
        col_hidden[c] = col_hidden[c-arg];
    }
    for (c = sc1; c < dc1; c++) {
        fwidth[c] = DEFWIDTH;
        precision[c] = DEFPREC;
        realfmt[c] = DEFREFMT;
        col_hidden[c] = FALSE;
    }

    for (r = 0; r <= maxrow; r++) {
        pp = ATBL(tbl, r, 0);
        for (c = dc2; c >= dc1; c--) {
            if ((pp[c] = pp[c-arg]))
                pp[c]->col += arg;
        }
        for (c = sc1; c < dc1; c++)
            pp[c] = NULL;
    }

    /* Update all marked cells. */
    for (c = 0; c < 37; c++) {
        if (savedcr[c].col >= sc1)
            savedcr[c].col += arg;
        if (savedst[c].col >= sc1)
            savedst[c].col += arg;
    }
    if (gs.g_rr.left.col >= sc1)
        gs.g_rr.left.col += arg;
    if (gs.g_rr.right.col >= sc1)
        gs.g_rr.right.col += arg;
    if (gs.st.col >= sc1)
        gs.st.col += arg;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.col >= sc1)
                    p->nrr.left.col += arg;
                if (p->nrr.right.col >= sc1)
                    p->nrr.right.col += arg;
            }
        }
    }

    // XXX: cell coordinates have been updated
    fr = find_frange(cr.row, cr.col);
    if (delta) {
        if (fr && fr->ir_right->col == cr.col)
            fr->ir_right = lookat(fr->ir_right->row, fr->ir_right->col + arg);
        fix_ranges(-1, cr.col, -1, cr.col, 0, arg, fr);
    } else {
        if (fr && fr->ir_left->col == cr.col + arg)
            fr->ir_left = lookat(fr->ir_left->row, fr->ir_left->col - arg);
        fix_ranges(-1, cr.col + arg, -1, cr.col + arg, arg, 0, fr);
    }
    FullUpdate++;
    modflg++;
    return 1;
}

/* delete rows starting at r1 up to and including r2 */
void deleterows(int r1, int r2) {
    int nrows, i;
    int c1 = 0;
    int c2 = maxcol;
    struct frange *fr;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r2 > maxrow)
        r2 = maxrow;
    if (r1 > maxrow) {
        /* deleting rows beyond the active area: nothing to do */
        return;
    }

    nrows = r2 - r1 + 1;

    if (currow == r1 && (fr = get_current_frange())) {
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
        if (any_locked_cells(r1, c1, r2, c2)) {
            error("Locked cells encountered. Nothing changed");
        } else {
            FullUpdate++;
            modflg++;

            /* discard named buffer '9' and qbuf if any */
            deldata_discard(DELBUFSIZE - 1);
            sync_refs();
            /* move cell range to subsheet delbuf[++dbidx] */
            // XXX: this is necessary for synching the cell references
            erase_area(++dbidx, r1, c1, r2, c2, 0);
            fix_ranges(r1, -1, r2, -1, -1, -1, fr);
            /* store delbuf[dbidx--] to named buffer '1' after rotation and qbuf if any */
            deldata_store(DELBUFSIZE - 9, DELBUFSIZE - 1, DD_UNSYNC);
            if (r1 + nrows > fr->ir_right->row && fr->ir_right->row >= r1)
                fr->ir_right = lookat(r1 - 1, fr->ir_right->col);
            if (r1 + nrows > fr->or_right->row) {
                fr->or_right = lookat(r1 - 1, fr->or_right->col);
            } else {
                move_area(r1, c1, rangeref(r1 + nrows, c1, fr->or_right->row, c2));
            }
            if (fr->ir_left->row > fr->ir_right->row) {
                del_frange(fr);
                fr = NULL;
            }
            /* Update all marked cells. */
            for (i = 0; i < 37; i++) {
                if (savedcr[i].col >= c1 && savedcr[i].col <= c2) {
                    if (savedcr[i].row >= r1 && savedcr[i].row <= r2)
                        savedcr[i].row = savedcr[i].col = -1;
                    else if (savedcr[i].row > r2)
                        savedcr[i].row -= nrows;
                }
                if (savedst[i].col >= c1 && savedst[i].col <= c2) {
                    if (savedst[i].row >= r1 && savedst[i].row <= r2)
                        savedst[i].row = r1;
                    else if (savedst[i].row > r2)
                        savedst[i].row -= nrows;
                }
            }
            if (gs.g_rr.left.col >= c1 && gs.g_rr.left.col <= c2) {
                if (gs.g_rr.left.row >= r1 && gs.g_rr.left.row <= r2)
                    gs.g_rr.left.row = r1;
                else if (gs.g_rr.left.row > r2)
                    gs.g_rr.left.row -= nrows;
            }
            if (gs.g_rr.right.col >= c1 && gs.g_rr.right.col <= c2) {
                if (gs.g_rr.right.row >= r1 && gs.g_rr.right.row <= r2)
                    gs.g_rr.right.row = r1 - 1;
                else if (gs.g_rr.right.row > r2)
                    gs.g_rr.right.row -= nrows;
            }
            if (gs.g_rr.left.row > gs.g_rr.right.row)
                gs.g_rr.left.row = gs.g_rr.left.col = -1;
            if (gs.st.col >= c1 && gs.st.col <= c2) {
                if (gs.st.row >= r1 && gs.st.row <= r2)
                    gs.st.row = r1;
                else if (gs.st.row > r2)
                    gs.st.row -= nrows;
            }
        }
    } else {
        if (any_locked_cells(r1, c1, r2, c2)) {
            error("Locked cells encountered. Nothing changed");
        } else {
            /* discard named buffer '9' and qbuf if any */
            deldata_discard(DELBUFSIZE - 1);
            sync_refs();
            /* move cell range to subsheet delbuf[++dbidx] */
            // XXX: this is necessary for synching the cell references
            erase_area(++dbidx, r1, c1, r2, c2, 0);
            fix_ranges(r1, -1, r2, -1, -1, -1, NULL);
            closerow(r1, nrows);
            /* store delbuf[dbidx--] to named buffer '1' after rotation and qbuf if any */
            deldata_store(DELBUFSIZE - 9, DELBUFSIZE - 1, DD_UNSYNC);
        }
    }
    /* currow will not become > maxrow because maxrow was not decremented */
    if (currow > r1)
        currow = (currow <= r2) ? r1 : currow - nrows;
}

void yankrows(int r1, int r2) {
    int arg, nrows;
    int c1 = 0, c2 = maxcol;
    struct frange *fr;

    if (r1 > r2) SWAPINT(r1, r2);
    arg = r2 - r1 + 1;
    nrows = maxrow - r1 + 1;

    // XXX: should check if r1 and r2 are inside current frange
    if (r1 == currow && (fr = get_current_frange())) {
        nrows = fr->or_right->row - r1 + 1;
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
    }
    if (arg > nrows) {
        error("Cannot yank %d row%s, %d row%s left",
              arg, (arg != 1 ? "s" : ""), nrows, (nrows != 1 ? "s" : ""));
        return;
    }
    sync_refs();

    // XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
    /* discard named buffer '0' and qbuf if any */
    deldata_discard(DELBUFSIZE - 10);
    yank_area(rangeref(r1, c1, r1 + arg - 1, c2));
    /* set yanked data in named buffer '0' and qbuf if any */
    deldata_store(DELBUFSIZE - 10, DELBUFSIZE - 10, 0);
}

void yankcols(int c1, int c2) {
    int arg, ncols;

    if (c1 > c2) SWAPINT(c1, c2);
    arg = c2 - c1 + 1;
    ncols = maxcol - c1 + 1;

    if (arg > ncols) {
        error("Can't yank %d column%s, %d column%s left",
              arg, (arg != 1 ? "s" : ""), ncols, (ncols != 1 ? "s" : ""));
        return;
    }
    sync_refs();
    // XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
    /* discard named buffer '0' and qbuf if any */
    deldata_discard(DELBUFSIZE - 10);
    yank_area(rangeref(0, c1, maxrow, c1 + arg - 1));
    /* set yanked data in named buffer '0' and qbuf if any */
    deldata_store(DELBUFSIZE - 10, DELBUFSIZE - 10, 0);
}

/* ignorelock is used when sorting so that locked cells can still be sorted */

/* move cell range to subsheet delbuf[idx] */
void erase_area(int idx, int sr, int sc, int er, int ec, int ignorelock) {
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

    // XXX: assuming delbuffmt[idx] is NULL
    delbuffmt[idx] = scxmalloc((4*(ec-sc+1)+(er-sr+1))*sizeof(char));
    for (c = sc; c <= ec; c++) {
        delbuffmt[idx][4*(c-sc)+0] = (unsigned char)fwidth[c];
        delbuffmt[idx][4*(c-sc)+1] = (unsigned char)precision[c];
        delbuffmt[idx][4*(c-sc)+2] = (unsigned char)realfmt[c];
        delbuffmt[idx][4*(c-sc)+3] = (unsigned char)col_hidden[c];
    }
    for (r = sr; r <= er; r++) {
        for (c = sc; c <= ec; c++) {
            pp = ATBL(tbl, r, c);
            if (*pp && (!((*pp)->flags & IS_LOCKED) || ignorelock)) {
                free_ent(idx, *pp, 0);
                *pp = NULL;
            }
        }
        delbuffmt[idx][4*(ec-sc+1)+(r-sr)] = (unsigned char)row_hidden[r];
    }
}

/* copy a range of cells to delbuf[++dbidx] */
void yank_area(rangeref_t rr) {
    int qtmp;
    range_clip(range_normalize(&rr));
    // XXX: should copy area to delbuf[++dbidx] instead of move/copy/xchg
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);
    /* copy and xchg cell range from delbuf[dbidx] back to original position */
    // XXX: pullcells uses qbuf (!) if set
    qtmp = qbuf;
    qbuf = 0;
    pullcells('p', rr.left);
    qbuf = qtmp;
}

void move_area(int dr, int dc, rangeref_t rr) {
    struct ent *p;
    struct ent **pp;
    int deltar, deltac;

    range_clip(range_normalize(&rr));

    /* First we erase the source range, which puts the cells on the delete
     * buffer stack.
     */
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);

    deltar = dr - rr.left.row;
    deltac = dc - rr.left.col;

    /* Now we erase the destination range, which adds it to the delete buffer
     * stack, but then we flush it off.  We then move the original source
     * range from the stack to the destination range, adjusting the addresses
     * as we go, leaving the stack in its original state.
     */
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, dr, dc, rr.right.row + deltar, rr.right.col + deltac, 0);
    // XXX: why not sync_refs() ??? most likely a bug
    /* free delbuf[dbidx--] */
    flush_saved(dbidx--);
    // XXX: if moving entire columns or rows, the column or row flags should be copied
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

/* copy/move cell range from subsheet delbuf[qbuf] via delbuf[++dbidx] or delbuf[dbidx] */
void pullcells(int to_insert, cellref_t cr) {
    struct ent *obuf;
    struct ent *p, *n;
    struct ent **pp;
    int deltar, deltac;
    int minrow, mincol;
    int mxrow, mxcol;
    int numrows, numcols;
    int i, c;
    struct frange *fr;

    if ((qbuf && !delbuf[qbuf]) || dbidx < 0) {
        error("No data to pull");
        qbuf = 0;
        return;
    }
    if (qbuf) {
        /* push qbuf on delbuf stack */
        ++dbidx;
        delbuf[dbidx] = delbuf[qbuf];
        delbuffmt[dbidx] = delbuffmt[qbuf];
    }
    obuf = delbuf[dbidx];  /* orig. contents of the del. buffer */

    /* compute delbuf range */
    minrow = maxrows;
    mincol = maxcols;
    mxrow = 0;
    mxcol = 0;

    for (p = delbuf[dbidx]; p; p = p->next) {
        if (p->row < minrow) minrow = p->row;
        if (p->row > mxrow) mxrow = p->row;
        if (p->col < mincol) mincol = p->col;
        if (p->col > mxcol) mxcol = p->col;
        p->flags |= MAY_SYNC;
    }

    numrows = mxrow - minrow + 1;
    numcols = mxcol - mincol + 1;
    deltar = cr.row - minrow;
    deltac = cr.col - mincol;

    /* pull commands:
       'r' -> PULLROWS
       'c' -> PULLCOLS
       'p' -> PULL (paste+pop)
       'm' -> PULLMERGE
       'x' -> PULLXCHG
       't' -> PULLTP (transpose)
       'f' -> PULLFMT
       'C' -> pull clear?
       '.' -> pull copy? but implemented as copy(COPY_FROM_QBUF)
     */
    if (to_insert == 'C') {     /* PULL full? */
        minrow = 0;
        mincol = 0;
        mxrow = maxrows;
        mxcol = maxcols;
    } else
    if (to_insert == 'r') {     /* PULLROWS */
        if (!insertrow(cr, numrows, 0))
            return;
        if ((fr = find_frange(cr.row, cr.col))) {
            deltac = fr->or_left->col - mincol;
        } else {
            for (i = 0; i < numrows; i++) {
                row_hidden[cr.row + i] = delbuffmt[dbidx][4*numcols+i];
            }
            deltac = 0;
        }
    } else
    if (to_insert == 'c') {     /* PULLCOLS */
        if (!insertcol(cr, numcols, 0))
            return;
        for (i = 0, c = cr.col; i < numcols; i++, c++) {
            fwidth[c] = delbuffmt[dbidx][4*i];
            precision[c] = delbuffmt[dbidx][4*i+1];
            realfmt[c] = delbuffmt[dbidx][4*i+2];
            col_hidden[c] = delbuffmt[dbidx][4*i+3];
        }
        deltar = 0;
    } else
    if (to_insert == 'x') {     /* PULLXCHG */

        /* Save the original contents of the destination range on the
         * delete buffer stack in preparation for the exchange, then swap
         * the top two elements on the stack, so that the original cells
         * to be pulled are still on top.
         */
        /* move cell range to subsheet delbuf[++dbidx] */
        erase_area(++dbidx, minrow + deltar, mincol + deltac,
                   mxrow + deltar, mxcol + deltac, 0);
        /* swap temp buffers at top of stack */
        deldata_swap(dbidx, dbidx - 1);
    } else
    if (to_insert == 'p') {     /* PULL */
        /* move cell range to subsheet delbuf[++dbidx] */
        erase_area(++dbidx, minrow + deltar, mincol + deltac,
                   mxrow + deltar, mxcol + deltac, 0);
        sync_refs();
        /* free delbuf[dbidx--] */
        flush_saved(dbidx--);
    } else
    if (to_insert == 't') {     /* PULLTP (transpose) */
        /* move cell range to subsheet delbuf[++dbidx] */
        erase_area(++dbidx, minrow + deltar, mincol + deltac,
                   minrow + deltar + mxcol - mincol,
                   mincol + deltac + mxrow - minrow, 0);
        sync_refs();
        /* free delbuf[dbidx--] */
        flush_saved(dbidx--);
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
        /* to_insert is in PULL, PULLROWS, PULLCOLS, PULLXCHG */
        if (to_insert == 'x') {     /* PULLXCHG */
            /* swap temp buffers at top of stack */
            deldata_swap(dbidx, dbidx - 1);
        } else {
            // XXX: very confusing!
            /* move the unlocked cells from destination range to delbuf[dbidx+1] */
            // XXX: this does nothing for PULLROWS, PULLCOLS as the range is empty
            for (p = delbuf[dbidx++]; p; p = p->next) {
                pp = ATBL(tbl, p->row + deltar, p->col + deltac);
                if (*pp && !((*pp)->flags & IS_LOCKED)) {
                    free_ent(dbidx, *pp, 1);
                    *pp = NULL;
                }
            }
        }
        /* move the cells from delbuf to destination */
        for (p = delbuf[dbidx - 1]; p; p = p->next) {
            pp = ATBL(tbl, p->row + deltar, p->col + deltac);
            // XXX: leak if (*pp && !((*pp)->flags & IS_LOCKED)) ?
            *pp = p;
            p->row += deltar;
            p->col += deltac;
            p->flags &= ~IS_DELETED;
        }
        /* leave original cells at top of stack */
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
    // XXX: should use deldata_store_qbuf?
    // XXX: obuf could be a stale pointer
    for (i = 0; i < DELBUFSIZE; i++) {
        if (delbuf[i] == obuf) {
            delbuf[i] = delbuf[dbidx];
            delbuffmt[i] = delbuffmt[dbidx];
        }
    }
    if (qbuf) {
        delbuf[dbidx] = NULL;
        delbuffmt[dbidx] = NULL;
        dbidx--;
        qbuf = 0;
    }
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
                free_ent(dbidx, *pp, 1);
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
        if (savedcr[i].row >= rs && savedcr[i].row < rs + numrow)
            savedcr[i].row = savedcr[i].col = -1;
        else if (savedcr[i].row >= rs + numrow)
            savedcr[i].row -= numrow;
        if (savedst[i].row >= rs && savedst[i].row < rs + numrow)
            savedst[i].row = rs;
        else if (savedst[i].row >= rs + numrow)
            savedst[i].row -= numrow;
    }
    if (gs.g_rr.left.row >= rs && gs.g_rr.left.row < rs + numrow)
        gs.g_rr.left.row = rs;
    else if (gs.g_rr.left.row >= rs + numrow)
        gs.g_rr.left.row -= numrow;
    if (gs.g_rr.right.row >= rs && gs.g_rr.right.row < rs + numrow)
        gs.g_rr.right.row = rs - 1;
    else if (gs.g_rr.right.row >= rs + numrow)
        gs.g_rr.right.row -= numrow;
    if (gs.g_rr.left.row > gs.g_rr.right.row)
        gs.g_rr.left.row = gs.g_rr.left.col = -1;
    if (gs.st.row >= rs && gs.st.row < rs + numrow)
        gs.st.row = rs;
    else if (gs.st.row >= rs + numrow)
        gs.st.row -= numrow;

    maxrow -= numrow;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.row >= rs && p->nrr.left.row < rs + numrow)
                    p->nrr.left.row = rs;
                else if (p->nrr.left.row >= rs + numrow)
                    p->nrr.left.row -= numrow;
                if (p->nrr.right.row >= rs && p->nrr.right.row < rs + numrow)
                    p->nrr.right.row = rs - 1;
                else if (p->nrr.right.row >= rs + numrow)
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
    // XXX: probably useless and counterproductive
    //      this is for get_current_frange() which is questionable
    curcol = c1;

    /* discard named buffer '9' and qbuf if any */
    deldata_discard(DELBUFSIZE - 1);
    sync_refs();
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, 0, c1, maxrow, c2, 0);
    // XXX: should use sync_refs() to flag invalid references
    fix_ranges(-1, c1, -1, c2, -1, -1, get_current_frange());
    /* store delbuf[dbidx--] to named buffer '1' after rotation and qbuf if any */
    deldata_store(DELBUFSIZE - 9, DELBUFSIZE - 1, DD_UNSYNC);

    /* clear then copy the block left */
    for (r = 0; r <= maxrow; r++) {
#if 0
        // XXX: all cells should have been erased already
        for (c = c1; c <= c2; c++) {
            pp = ATBL(tbl, r, c);
            if (*pp) {
                free_ent(dbidx, *pp, 1);
                *pp = NULL;
            }
        }
#endif
        for (c = c1; c <= maxcol - ncols; c++) {
            pp = ATBL(tbl, r, c);
            if ((*pp = pp[ncols])) {
                (*pp)->col -= ncols;
                pp[ncols] = NULL;
            }
        }
    }

    for (c = c1; c <= maxcol - ncols; c++) {
        fwidth[c] = fwidth[c + ncols];
        precision[c] = precision[c + ncols];
        realfmt[c] = realfmt[c + ncols];
        col_hidden[c] = col_hidden[c + ncols];
    }
    for (; c <= maxcol; c++) {
        fwidth[c] = DEFWIDTH;
        precision[c] = DEFPREC;
        realfmt[c] = DEFREFMT;
        col_hidden[c] = FALSE;
    }

    /* Update all marked cells. */
    for (i = 0; i < 37; i++) {
        if (savedcr[i].col >= c1 && savedcr[i].col <= c2)
            savedcr[i].row = savedcr[i].col = -1;
        else if (savedcr[i].col > c2)
            savedcr[i].col -= ncols;
        if (savedst[i].col >= c1 && savedst[i].col <= c2)
            savedst[i].col = c1;
        else if (savedst[i].col > c2)
            savedst[i].col -= ncols;
    }
    if (gs.g_rr.left.col >= c1 && gs.g_rr.left.col <= c2)
        gs.g_rr.left.col = c1;
    else if (gs.g_rr.left.col > c2)
        gs.g_rr.left.col -= ncols;
    if (gs.g_rr.right.col >= c1 && gs.g_rr.right.col <= c2)
        gs.g_rr.right.col = c1 - 1;
    else if (gs.g_rr.right.col > c2)
        gs.g_rr.right.col -= ncols;
    if (gs.g_rr.left.col > gs.g_rr.right.col)
        gs.g_rr.left.row = gs.g_rr.left.col = -1;
    if (gs.st.col > c1 && gs.st.col <= c2)
        gs.st.col = c1;
    else if (gs.st.col > c2)
        gs.st.col -= ncols;

    maxcol -= ncols;

    /* Update note links. */
    for (r = 0; r <= maxrow; r++) {
        for (c = 0; c <= maxcol; c++) {
            p = *ATBL(tbl, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.col >= c1 && p->nrr.left.col <= c2)
                    p->nrr.left.col = c1;
                else if (p->nrr.left.col > c2)
                    p->nrr.left.col -= ncols;
                if (p->nrr.right.col >= c1 && p->nrr.right.col <= c2)
                    p->nrr.right.col = c1 - 1;
                else if (p->nrr.right.col > c2)
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
        if ((qbuf && !delbuf[qbuf]) || dbidx < 0)
            return;
        if (qbuf) {
            ++dbidx;
            delbuf[dbidx] = delbuf[qbuf];
            delbuffmt[dbidx] = delbuffmt[qbuf];
        }
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
        /* copy source cells to delbuf[++dbidx] */
        // XXX: should not be necessary if copy order is well chosen
        //      or if source and destination do not overlap
        yank_area(rangeref(minsr, minsc, maxsr, maxsc));
    }

    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, mindr, mindc, maxdr, maxdc, 0);
    sync_refs();
    /* free delbuf[dbidx--] */
    flush_saved(dbidx--);

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
        /* free delbuf[dbidx--] */
        flush_saved(dbidx--);
    }

    if (flags & COPY_FROM_QBUF) {
        if (qbuf) {
            delbuf[dbidx] = NULL;
            delbuffmt[dbidx] = NULL;
            dbidx--;
            qbuf = 0;
        }
    }
    error("Copy done.");
}

/* ERASE a Range of cells */
void eraser(rangeref_t rr) {
    // XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
    /* discard named buffer '9' and qbuf if any */
    deldata_discard(DELBUFSIZE - 1);
    // XXX: missing sync_refs() ???
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(++dbidx, rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);
    sync_refs();
    /* store delbuf[dbidx--] to named buffer '1' after rotation and qbuf if any */
    // XXX: missing DD_UNSYNC ???
    deldata_store(DELBUFSIZE - 9, DELBUFSIZE - 1, 0);

    FullUpdate++;
    modflg++;
}

/* YANK a Range of cells */
void yankr(rangeref_t rr) {
    // XXX: delbuf[dbidx] is overwritten and dbidx decremented if >= 0
    /* discard named buffer '0' and qbuf if any */
    deldata_discard(DELBUFSIZE - 10);
    yank_area(rr);
    /* set yanked data in named buffer '0' and qbuf if any */
    deldata_store(DELBUFSIZE - 10, DELBUFSIZE - 10, 0);
}

/* MOVE a Range of cells */
void mover(cellref_t cr, rangeref_t rr) {
    move_area(cr.row, cr.col, rr);
    sync_refs();
    FullUpdate++;
}

/* fill a range with constants */
void fillr(rangeref_t rr, double start, double inc, int bycols) {
    int r, c;

    range_normalize(&rr);

    if (bycols) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            for (r = rr.left.row; r <= rr.right.row; r++) {
                struct ent *n = lookat(r, c);
                if (n->flags & IS_LOCKED)
                    continue;
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->flags |= IS_CHANGED | IS_VALID;
                n->flags &= ~IS_CLEARED;
            }
        }
    } else {
        for (r = rr.left.row; r <= rr.right.row; r++) {
            for (c = rr.left.col; c <= rr.right.col; c++) {
                struct ent *n = lookat(r, c);
                if (n->flags & IS_LOCKED)
                    continue;
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->flags |= IS_CHANGED | IS_VALID;
                n->flags &= ~IS_CLEARED;
            }
        }
    }
    FullUpdate++;
    changed++;
    modflg++;
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
    modflg++;
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
    modflg++;
}

void format_cells(rangeref_t rr, SCXMEM string_t *s) {
    int r, c;

    if (s && !*s2c(s)) {
        free_string(s);
        s = NULL;
    }

    // XXX: should be skip locked cells silently
    //      or should be fail with an error
    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = lookat(r, c);
            if (p->flags & IS_LOCKED)
                continue;
            set_string(&p->format, dup_string(s));
            p->flags |= IS_CHANGED;
        }
    }
    free_string(s);
    FullUpdate++;
    modflg++;
}

/*
 * sync_refs and sync_expr are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
static void sync_expr(struct enode *e) {
    if (e == NULL)
        return;

    if (e->op & REDUCE) {
        e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
        e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
    } else {
        switch (e->op) {
        case 'v':
            if (e->e.v.vp->flags & IS_CLEARED) {
                e->op = ERR_;
                e->e.o.left = NULL;
                e->e.o.right = NULL;
            } else
            if (e->e.v.vp->flags & MAY_SYNC) {
                e->e.v.vp = lookat(e->e.v.vp->row, e->e.v.vp->col);
            }
            break;
        case 'k':
        case '$':
            break;
        default:
            sync_expr(e->e.o.right);
            sync_expr(e->e.o.left);
            break;
        }
    }
}

void sync_refs(void) {
    int i, j;
    struct ent *p;

    sync_ranges();

    // XXX: sync_ranges() already does sync_enode(p->expr)
    for (i = 0; i <= maxrow; i++) {
        for (j = 0; j <= maxcol; j++) {
            if ((p = *ATBL(tbl, i, j)) && p->expr)
                sync_expr(p->expr);
        }
    }
    for (i = 0; i < DELBUFSIZE; i++) {
        for (p = delbuf[i]; p; p = p->next) {
            if (p->expr)
                sync_expr(p->expr);
        }
    }
}

/* mark rows as hidden */
void hiderows(int r1, int r2) {
    int r;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r1 < 0) {
        error("Invalid range");
        return;
    }
    if (r2 + 1 >= maxrows) {
        if (!growtbl(GROWROW, r2 + 1, 0)) {
            // XXX: should remove this restriction
            error("You cannot hide the last row");
            return;
        }
    }
    if (r2 + 1 > maxrow)
        maxrow = r2 + 1;

    for (r = r1; r <= r2; r++)
        row_hidden[r] = 1;

    if (currow >= r1)
        currow = (currow <= r2) ? r2 + 1 : currow - (r2 - r1 + 1);

    FullUpdate++;
    modflg++;
}

/* mark columns as hidden */
void hidecols(int c1, int c2) {
    int c;

    if (c1 > c2) SWAPINT(c1, c2);
    if (c1 < 0) {
        error("Invalid range");
        return;
    }
    if (c2 + 1 >= maxcols) {
        if (!growtbl(GROWCOL, 0, c2 + 1)) {
            // XXX: should remove this restriction
            error("You cannot hide the last column");
            return;
        }
    }
    if (c2 + 1 > maxcol)
        maxcol = c2 + 1;

    for (c = c1; c <= c2; c++)
        col_hidden[c] = TRUE;

    if (curcol >= c1)
        curcol = (curcol <= c2) ? c2 + 1 : curcol - (c2 - c1 + 1);

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
    if (r1 > r2) SWAPINT(r1, r2);
    if (r1 < 0) {
        error("Invalid range");
        return;
    }
    if (r2 > maxrow)
        r2 = maxrow;

    FullUpdate++;
    modflg++;
    while (r1 <= r2)
        row_hidden[r1++] = 0;
}

/* mark a column as not-hidden */
void showcol(int c1, int c2) {
    if (c1 > c2) SWAPINT(c1, c2);
    if (c1 < 0) {
        error("Invalid range");
        return;
    }
    if (c2 > maxcol)
        c2 = maxcol;

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
            set_string(&n->label, dup_string(p->label));
        } else if (special != 'm') {
            set_string(&n->label, NULL);
        }
        n->flags &= ~ALIGN_MASK;
        n->flags |= p->flags & ALIGN_MASK;
        n->flags |= p->flags & IS_LOCKED;
    }
    if (p->format) {
        set_string(&n->format, dup_string(p->format));
    } else
    if (special != 'm' && special != 'f') {
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
        set_string(&colformat[c], NULL);

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
    for (c = 0; c < 37; c++) {
        savedcr[c] = cellref(-1, -1);
        savedst[c] = cellref(-1, -1);
    }

    set_string(&mdir, NULL);
    set_string(&autorun, NULL);

    for (c = 0; c < FKEYS; c++)
        set_string(&fkey[c], NULL);

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
int locked_cell(int row, int col) {
    struct ent *p = *ATBL(tbl, row, col);
    if (p && (p->flags & IS_LOCKED)) {
        error("Cell %s%d is locked", coltoa(col), row);
        return 1;
    }
    return 0;
}

/* Check if area contains locked cells */
static int any_locked_cells(int r1, int c1, int r2, int c2) {
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

void set_histfile(SCXMEM string_t *str) {
    strlcpy(histfile, str ? s2c(str) : "", sizeof histfile);
    free_string(str);
}

void set_mdir(SCXMEM string_t *str) {
    set_string(&mdir, str);
    modflg++;
}

void set_autorun(SCXMEM string_t *str) {
    set_string(&autorun, str);
    modflg++;
}

void set_fkey(int n, SCXMEM string_t *str) {
    if (n >= 0 && n < FKEYS) {
        set_string(&fkey[n], str);
        modflg++;
    } else {
        free_string(str);
        error("Invalid function key");
    }
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

void cmd_setformat(int n, SCXMEM string_t *str) {
    if (n >= 0 && n < 10) {
        if (str && !*s2c(str)) {
            free_string(str);
            str = NULL;
        }
        set_string(&colformat[n], str);
        FullUpdate++;
        modflg++;
    } else {
        error("Invalid format number");
        free_string(str);
    }
}

void cmd_run(SCXMEM string_t *str) {
    const char *cmd = str ? s2c(str) : "";
    deraw(1);
    system(cmd);
    if (*cmd && cmd[strlen(cmd) - 1] != '&') {
        printf("Press any key to continue ");
        fflush(stdout);
        cbreak();
        nmgetch(0);
    }
    goraw();
    free_string(str);
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
