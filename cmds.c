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

#include "sc.h"

#define ATBL(sp, row, col)     (&(sp)->tbl[row][col])

/* a linked list of free [struct ent]'s, uses .next as the pointer */
static struct ent *freeents = NULL;

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */
/* temporary sheet fragments: 4 work buffers and 36 named buffers (a-z,0-9) */
subsheet_t delbuf[DELBUFSIZE];
int dbidx = 0;

static int any_locked_cells(sheet_t *sp, int r1, int c1, int r2, int c2);
static void yank_area(sheet_t *sp, int idx, rangeref_t rr);
static void closerow(sheet_t *sp, int idx, int r, int numrow);

/* swap 2 entries in the delbuf array */
static void delbuf_swap(int idx1, int idx2) {
    subsheet_t tmpbuf = delbuf[idx1];
    delbuf[idx1] = delbuf[idx2];
    delbuf[idx2] = tmpbuf;
}

/* rotate a range of entries in the delbuf array */
static void delbuf_rotate(int idx1, int idx2) {
    while (idx2 --> idx1) {
        delbuf_swap(idx2 + 1, idx2);
    }
}

static void delbuf_clear(int idx) {
    subsheet_t *db = &delbuf[idx];
    db->minrow = db->mincol = 0;
    db->maxrow = db->maxcol = -1;
    db->qbuf_was_here = 0;
    db->ncols = db->nrows = 0;
    db->ptr = NULL;
    db->colfmt = NULL;
    db->rowfmt = NULL;
}

static void delbuf_copy(int dest, int src) {
    if (dest != src) {
        delbuf_free(dest);
        delbuf[dest] = delbuf[src];
    }
}

/* discard delbuf[qbuf], set qbuf_was_here on all duplicates */
static void deldata_discard_qbuf(void) {
    if (qbuf) { // XXX: this test may be incorrect
        int i;
        struct ent *obuf = delbuf[qbuf].ptr;
        delbuf[qbuf].qbuf_was_here = 1;
        /* set qbuf_was_here on all duplicates of qbuf */
        if (obuf) {
            for (i = 0; i < DELBUFSIZE; i++) {
                if (i != qbuf && delbuf[i].ptr == obuf) {
                    delbuf[i].qbuf_was_here = 1;
                    delbuf[i].ncols = delbuf[i].nrows = 0;
                    delbuf[i].ptr = NULL;
                    delbuf[i].colfmt = NULL;
                    delbuf[i].rowfmt = NULL;
                }
           }
        }
        delbuf_free(qbuf);
    }
}

/* store delbuf[idx] to delbuf[qbuf] and its copies, clear qbuf */
// XXX: this is confusing. Should use a different approach.
static void deldata_store_qbuf(int idx) {
    int i;
    for (i = 0; i < DELBUFSIZE; i++) {
        if (delbuf[i].qbuf_was_here || (qbuf && i == qbuf)) {
            delbuf_copy(i, idx);
        }
    }
    qbuf = 0;
}

/* discard named buffer delbuf[idx] if unique and delbuf[qbuf] */
static void deldata_discard(int idx) {
    delbuf_free(idx);
    deldata_discard_qbuf();
}

#define DD_UNSYNC  1

/* store delbuf[idx] to idx1 after rotation and qbuf if any */
static void deldata_store(int idx, int idx1, int idx2, int flags) {
    deldata_store_qbuf(idx);
    if (idx1 != idx2) {
        /* shift named buffers 1-8 to 2-9 */
        // XXX: this fails if qbuf was one of the named buffers 1-9
        delbuf_rotate(idx1, idx2);
    }
    /* duplicate delbuf[idx] to named buffer idx1 */
    delbuf_copy(idx1, idx);
    // XXX: document this
    if (flags & DD_UNSYNC) {
        struct ent *p;
        for (p = delbuf[idx1].ptr; p; p = p->next)
            p->flags &= ~MAY_SYNC;
    }
}

void free_ent_list(void) {
    struct ent *p, *next;
    int i;

    for (i = 0; i < DELBUFSIZE; i++) {
        delbuf_free(i);
    }
    for (p = freeents, freeents = NULL; p; p = next) {
        next = p->next;
        scxfree(p);
    }
}

/* free deleted cells */
int delbuf_free(int idx) {
    subsheet_t *db;
    struct ent *p, *next;
    int i;

    if (idx < 0)
        return 0;

    db = &delbuf[idx];
    if (!db->ptr && !db->colfmt && !db->rowfmt)
        return 0;

    /* detect duplicate and clear pointers */
    for (i = 0; i < DELBUFSIZE; i++) {
        if (i == idx)
            continue;
        if (db->ptr == delbuf[i].ptr)
            db->ptr = NULL;
        if (db->colfmt == delbuf[i].colfmt)
            db->colfmt = NULL;
        if (db->rowfmt == delbuf[i].rowfmt)
            db->rowfmt = NULL;
    }

    for (p = db->ptr; p; p = next) {
        // XXX: entries are added to freeents in reverse order
        //      but they were added to the delbuf in reverse order too
        next = p->next;
        clearent(p);
        p->next = freeents;     /* put this ent on the front of freeents */
        freeents = p;
    }
    db->ptr = NULL;
    scxfree(db->rowfmt);
    db->rowfmt = NULL;
    scxfree(db->colfmt);
    db->colfmt = NULL;
    db->ncols = db->nrows = 0;
    return 1;
}

void clearent(struct ent *p) {
    if (p) {
        string_set(&p->label, NULL);
        efree(p->expr);
        p->expr = NULL;
        string_set(&p->format, NULL);
        p->v = 0.0;
        p->cellerror = 0;
        p->type = SC_EMPTY;
        p->flags = IS_CHANGED | IS_CLEARED;
        p->nrr = rangeref_empty();
        // XXX: should clear other fields?
        //      next
        FullUpdate++;  // XXX: really?
        changed++;
        //sp->modflg++;
    }
}

struct ent *getcell(sheet_t *sp, int row, int col) {
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol)
        return *ATBL(sp, row, col);
    else
        return NULL;
}

/* move a cell to the delbuf list (reverse order) */
static void killcell(sheet_t *sp, int row, int col,
                     int idx, int ignorelock, int unlock)
{
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol) {
        struct ent **pp = ATBL(sp, row, col);
        struct ent *p;
        if ((p = *pp) && (!(p->flags & IS_LOCKED) || ignorelock)) {
            p->next = delbuf[idx].ptr;
            delbuf[idx].ptr = p;
            p->flags |= IS_DELETED;
            if (unlock)
                p->flags &= ~IS_LOCKED;
            *pp = NULL;
        }
    }
}

static int setcell(sheet_t *sp, int row, int col, struct ent *p) {
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol) {
        struct ent **pp = ATBL(sp, row, col);
        // XXX: should reclaim destination cell
        *pp = p;
        return 1;
    }
    return 0;
}

/* return a pointer to a cell's [struct ent *], creating if needed */
struct ent *lookat(sheet_t *sp, int row, int col) {
    struct ent **pp;
    struct ent *p;

    checkbounds(sp, &row, &col);
    pp = ATBL(sp, row, col);
    if (*pp == NULL) {
        if ((p = freeents) != NULL) {
            freeents = p->next;
        } else {
            p = scxmalloc(sizeof(struct ent));
        }
        if (row > sp->maxrow) sp->maxrow = row;
        if (col > sp->maxcol) sp->maxcol = col;
        p->v = 0.0;
        p->label = NULL;
        p->expr = NULL;
        p->format = NULL;
        p->cellerror = 0;
        p->type = SC_EMPTY;
        p->row = row;
        p->col = col;
        p->flags = MAY_SYNC;
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

static rangeref_t *range_clip(sheet_t *sp, rangeref_t *rr) {
    if (rr->left.row < 0) rr->left.row = 0;
    if (rr->left.col < 0) rr->left.col = 0;
    if (rr->right.row > sp->maxrow) rr->right.row = sp->maxrow;
    if (rr->right.col > sp->maxcol) rr->right.col = sp->maxcol;
    return rr;
}

/* duplicate the row at `cr.row` below it into a new row */
int duprow(sheet_t *sp, cellref_t cr) {
    int row = cr.row, col;
    int c1 = 0;
    int c2 = sp->maxcol;
    struct frange *fr;

    if ((fr = frange_find(sp, cr.row, cr.col))) {
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
    }

    if (!insertrows(sp, cr, 1, 1))
        return 0;

    sp->modflg++;
    // XXX: should use copy_area(row + 1, c1, row + 1, c2, row, c1, row, c2)
    for (col = c1; col <= c2; col++) {
        struct ent *p = getcell(sp, row, col);
        if (p) {
            struct ent *n = lookat(sp, row + 1, col);
            copyent(sp, n, p, 1, 0, 0, 0, sp->maxrow, sp->maxcol, 0);
        }
    }
    return 1;
}

/* duplicate the column at `cr.col` to the right it into a new column */
int dupcol(sheet_t *sp, cellref_t cr) {
    int row, col = cr.col;

    if (!insertcols(sp, cr, 1, 1))
        return 0;

    sp->modflg++;
    // XXX: should use copy_area(0, col + 1, maxrow, col + 1,
    //                           0, col, maxrow, col)
    for (row = 0; row <= sp->maxrow; row++) {
        struct ent *p = getcell(sp, row, col);
        if (p) {
            struct ent *n = lookat(sp, row, col + 1);
            copyent(sp, n, p, 0, 1, 0, 0, sp->maxrow, sp->maxcol, 0);
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
int insertrows(sheet_t *sp, cellref_t cr, int arg, int delta) {
    int r, c, i, lim;
    rangeref_t rr = rangeref(cr.row + delta, 0, sp->maxrow, sp->maxcol);
    struct frange *fr;

    if ((sp->maxrow + arg >= sp->maxrows) && !growtbl(sp, GROWROW, sp->maxrow + arg, 0))
        return 0;

    // XXX: should clip cr reference
    lim = cr.row + delta + arg - 1;  /* last inserted row */
    sp->maxrow += arg;

    if ((fr = frange_find(sp, cr.row, cr.col))) {
        // XXX: should verify if cr.row + delta is inside the frange
        // XXX: inconsistent source range: marks are updated beyond rr.right.row
        rr = rangeref(cr.row + delta, fr->or_left->col,
                      fr->or_right->row, fr->or_right->col);
        move_area(sp, rr.left.row + arg, rr.left.col, rr);
        if (!delta && fr->ir_left->row == cr.row + arg)
            fr->ir_left = lookat(sp, fr->ir_left->row - arg, fr->ir_left->col);
        if (delta && fr->ir_right->row == cr.row)
            fr->ir_right = lookat(sp, fr->ir_right->row + arg, fr->ir_right->col);

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
        for (r = 0; r <= sp->maxrow; r++) {
            for (c = 0; c <= sp->maxcol; c++) {
                struct ent *p = getcell(sp, r, c);
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
        struct ent **tmp_row = sp->tbl[sp->maxrow];
        rowfmt_t tmp_fmt = sp->rowfmt[sp->maxrow];

        for (r = sp->maxrow; r > lim; r--) {
            sp->rowfmt[r] = sp->rowfmt[r-arg];
            sp->rowfmt[r-arg] = sp->rowfmt[r-1];
            sp->tbl[r] = sp->tbl[r-arg];
            sp->tbl[r-arg] = sp->tbl[r-1];
            for (c = 0; c < sp->maxcols; c++) {
                struct ent *p = getcell(sp, r, c);
                if (p)
                    p->row = r;
            }
        }
        sp->rowfmt[r] = tmp_fmt;
        sp->tbl[r] = tmp_row;               /* the last row was never used.... */

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
        for (r = 0; r <= sp->maxrow; r++) {
            for (c = 0; c <= sp->maxcol; c++) {
                struct ent *p = getcell(sp, r, c);
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
    fr = frange_find(sp, cr.row, cr.col);
    if (delta) {
        fix_ranges(sp, cr.row, -1, cr.row, -1, 0, arg, fr);
    } else {
        fix_ranges(sp, cr.row + arg, -1, cr.row + arg, -1, arg, 0, fr);
    }
    FullUpdate++;
    sp->modflg++;
    return 1;
}

/* Insert 'arg' cols.  The col(s) will be inserted before cr.col if delta
 * is 0; after if it is 1.
 * return 0 on failure, 1 on success
 */
int insertcols(sheet_t *sp, cellref_t cr, int arg, int delta) {
    int r, c;
    struct ent **pp;
    /* cols are moved from sc1:sc2 to dc1:dc2 */
    int sc1 = cr.col + delta;
    int sc2 = sp->maxcol;
    int dc1 = sc1 + arg;
    int dc2 = sc2 + arg;
    struct frange *fr;
    colfmt_t def_colfmt = { FALSE, DEFWIDTH, DEFPREC, DEFREFMT };

    if ((sp->maxcol + arg >= sp->maxcols) && !growtbl(sp, GROWCOL, 0, sp->maxcol + arg))
        return 0;

    sp->maxcol += arg;

    for (c = dc2; c >= dc1; c--) {
        sp->colfmt[c] = sp->colfmt[c - arg];
    }
    for (c = sc1; c < dc1; c++) {
        sp->colfmt[c] = def_colfmt;
    }

    for (r = 0; r <= sp->maxrow; r++) {
        // XXX: should factorize as movecell()
        pp = ATBL(sp, r, 0);
        for (c = dc2; c >= dc1; c--) {
            if ((pp[c] = pp[c - arg])) {
                pp[c]->col += arg;
                pp[c - arg] = NULL;
            }
        }
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
    for (r = 0; r <= sp->maxrow; r++) {
        for (c = 0; c <= sp->maxcol; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p && (p->flags & HAS_NOTE)) {
                if (p->nrr.left.col >= sc1)
                    p->nrr.left.col += arg;
                if (p->nrr.right.col >= sc1)
                    p->nrr.right.col += arg;
            }
        }
    }

    // XXX: cell coordinates have been updated
    fr = frange_find(sp, cr.row, cr.col);
    if (delta) {
        if (fr && fr->ir_right->col == cr.col)
            fr->ir_right = lookat(sp, fr->ir_right->row, fr->ir_right->col + arg);
        fix_ranges(sp, -1, cr.col, -1, cr.col, 0, arg, fr);
    } else {
        if (fr && fr->ir_left->col == cr.col + arg)
            fr->ir_left = lookat(sp, fr->ir_left->row, fr->ir_left->col - arg);
        fix_ranges(sp, -1, cr.col + arg, -1, cr.col + arg, arg, 0, fr);
    }
    FullUpdate++;
    sp->modflg++;
    return 1;
}

/* delete rows starting at r1 up to and including r2 */
void deleterows(sheet_t *sp, int r1, int r2) {
    int nrows, i;
    int c1 = 0;
    int c2 = sp->maxcol;
    struct frange *fr;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r2 > sp->maxrow)
        r2 = sp->maxrow;
    if (r1 > sp->maxrow) {
        /* deleting rows beyond the active area: nothing to do */
        return;
    }

    nrows = r2 - r1 + 1;

    if (sp->currow == r1 && (fr = frange_get_current(sp))) {
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
        if (any_locked_cells(sp, r1, c1, r2, c2)) {
            error("Locked cells encountered. Nothing changed");
        } else {
            FullUpdate++;
            sp->modflg++;

            /* discard named buffer '9' and qbuf if any */
            deldata_discard(DELBUF_9);
            sync_refs(sp);
            /* move cell range to subsheet delbuf[0] */
            // XXX: this is necessary for synching the cell references
            erase_area(sp, dbidx = 0, r1, c1, r2, c2, 0);
            fix_ranges(sp, r1, -1, r2, -1, -1, -1, fr);
            /* store delbuf[dbidx] to named buffer '1' after rotation and qbuf if any */
            deldata_store(dbidx, DELBUF_1, DELBUF_9, DD_UNSYNC);
            /* purposely leave dbidx == 0 so move_area does not clobber it */
            if (r1 + nrows > fr->ir_right->row && fr->ir_right->row >= r1)
                fr->ir_right = lookat(sp, r1 - 1, fr->ir_right->col);
            if (r1 + nrows > fr->or_right->row) {
                fr->or_right = lookat(sp, r1 - 1, fr->or_right->col);
            } else {
                move_area(sp, r1, c1, rangeref(r1 + nrows, c1, fr->or_right->row, c2));
            }
            if (fr->ir_left->row > fr->ir_right->row) {
                frange_delete(sp, fr);
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
        if (any_locked_cells(sp, r1, c1, r2, c2)) {
            error("Locked cells encountered. Nothing changed");
        } else {
            /* discard named buffer '9' and qbuf if any */
            deldata_discard(DELBUF_9);
            sync_refs(sp);
            /* move cell range to subsheet delbuf[0] */
            // XXX: this is necessary for synching the cell references
            erase_area(sp, dbidx = 0, r1, c1, r2, c2, 0);
            fix_ranges(sp, r1, -1, r2, -1, -1, -1, NULL);
            closerow(sp, dbidx, r1, nrows);
            /* store delbuf[dbidx] to named buffer '1' after rotation and qbuf if any */
            deldata_store(dbidx, DELBUF_1, DELBUF_9, DD_UNSYNC);
            /* purposely leave dbidx == 0 so other commands do not clobber it */
        }
    }
    /* sp->currow will not become > maxrow because maxrow was not decremented */
    if (sp->currow > r1)
        sp->currow = (sp->currow <= r2) ? r1 : sp->currow - nrows;
}

void yankrows(sheet_t *sp, int r1, int r2) {
    int arg, nrows;
    int c1 = 0, c2 = sp->maxcol;
    struct frange *fr;

    if (r1 > r2) SWAPINT(r1, r2);
    arg = r2 - r1 + 1;
    nrows = sp->maxrow - r1 + 1;

    // XXX: should check if r1 and r2 are inside current frange
    if (r1 == sp->currow && (fr = frange_get_current(sp))) {
        nrows = fr->or_right->row - r1 + 1;
        c1 = fr->or_left->col;
        c2 = fr->or_right->col;
    }
    if (arg > nrows) {
        error("Cannot yank %d row%s, %d row%s left",
              arg, (arg != 1 ? "s" : ""), nrows, (nrows != 1 ? "s" : ""));
        return;
    }
    sync_refs(sp);

    /* discard named buffer '0' and qbuf if any */
    deldata_discard(DELBUF_0);
    /* copy source cells to delbuf[0] */
    yank_area(sp, dbidx = 0, rangeref(r1, c1, r1 + arg - 1, c2));
    /* set yanked data in named buffer '0' and qbuf if any */
    deldata_store(dbidx, DELBUF_0, DELBUF_0, 0);
    /* purposely leave dbidx == 0 so other commands do not clobber it */
}

void yankcols(sheet_t *sp, int c1, int c2) {
    int arg, ncols;

    if (c1 > c2) SWAPINT(c1, c2);
    arg = c2 - c1 + 1;
    ncols = sp->maxcol - c1 + 1;

    if (arg > ncols) {
        error("Cannot yank %d column%s, %d column%s left",
              arg, (arg != 1 ? "s" : ""), ncols, (ncols != 1 ? "s" : ""));
        return;
    }
    sync_refs(sp);

    /* discard named buffer '0' and qbuf if any */
    deldata_discard(DELBUF_0);
    /* copy source cells to delbuf[0] */
    yank_area(sp, dbidx = 0, rangeref(0, c1, sp->maxrow, c1 + arg - 1));
    /* set yanked data in named buffer '0' and qbuf if any */
    deldata_store(dbidx, DELBUF_0, DELBUF_0, 0);
    /* purposely leave dbidx == 0 so other commands do not clobber it */
}

/* ignorelock is used when sorting so that locked cells can still be sorted */

/* move cell range to subsheet delbuf[idx] */
// XXX: should not modify sheet boundaries
void erase_area(sheet_t *sp, int idx, int sr, int sc, int er, int ec, int ignorelock) {
    subsheet_t *db = &delbuf[idx];
    int r, c;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    if (sr < 0) sr = 0;
    if (sc < 0) sc = 0;
    checkbounds(sp, &er, &ec);

    /* Do a lookat() for the upper left and lower right cells of the range
     * being erased to make sure they are included in the delete buffer so
     * that pulling cells always works correctly even if the cells at one
     * or more edges of the range are all empty.
     */
    lookat(sp, sr, sc);
    lookat(sp, er, ec);

    delbuf_free(idx);
    db->minrow = sr;
    db->mincol = sc;
    db->maxrow = er;
    db->maxcol = ec;
    db->qbuf_was_here = 0;
    db->ncols = ec - sc + 1;
    db->nrows = er - sr + 1;
    db->colfmt = scxmalloc(db->ncols * sizeof(colfmt_t));
    for (c = sc; c <= ec; c++) {
        db->colfmt[c - sc] = sp->colfmt[c];
    }
    db->rowfmt = scxmalloc(db->nrows * sizeof(rowfmt_t));
    for (r = sr; r <= er; r++) {
        db->rowfmt[r - sr] = sp->rowfmt[r];
    }
    // XXX: should have range_kill()
    for (r = sr; r <= er; r++) {
        for (c = sc; c <= ec; c++) {
            killcell(sp, r, c, idx, ignorelock, 0);
        }
    }
}

/* copy a range of cells to delbuf[dbidx] */
static void yank_area(sheet_t *sp, int idx, rangeref_t rr) {
    range_clip(sp, range_normalize(&rr));
    // XXX: should copy area to delbuf[dbidx] instead of move/copy/xchg
    /* move cell range to subsheet delbuf[dbidx] */
    erase_area(sp, idx, rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);
    /* copy and xchg cell range from delbuf[dbidx] back to original position */
    pullcells(sp, idx, 'p', rr.left);
}

/* uses 2 working slots delbuf[dbidx+1] and delbuf[dbidx+2] */
void move_area(sheet_t *sp, int dr, int dc, rangeref_t rr) {
    struct ent *p, *next;
    int deltar, deltac;

    range_clip(sp, range_normalize(&rr));

    /* First we erase the source range, which puts the cells on the delete
     * buffer stack.
     */
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(sp, ++dbidx, rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);

    deltar = dr - rr.left.row;
    deltac = dc - rr.left.col;

    /* Now we erase the destination range, which adds it to the delete buffer
     * stack, but then we flush it off.  We then move the original source
     * range from the stack to the destination range, adjusting the addresses
     * as we go, leaving the stack in its original state.
     */
    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(sp, ++dbidx, dr, dc, rr.right.row + deltar, rr.right.col + deltac, 0);
    // XXX: why not sync_refs() ??? most likely a bug
    delbuf_free(dbidx--);
    // XXX: if moving entire columns or rows, the column or row flags should be copied

    for (p = delbuf[dbidx].ptr; p; p = next) {
        next = p->next;
        p->row += deltar;
        p->col += deltac;
        p->flags &= ~IS_DELETED;
        setcell(sp, p->row, p->col, p);
    }
    delbuf[dbidx].ptr = NULL;
    delbuf_free(dbidx--);
}

/*
 * deletes the expression associated w/ a cell and turns it into a constant
 * containing whatever was on the screen
 */
void valueize_area(sheet_t *sp, rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p && p->expr) {
                if (p->flags & IS_LOCKED) {
                    error(" Cell %s%d is locked", coltoa(c), r);
                    continue;
                }
                efree(p->expr);
                p->expr = NULL;
            }
        }
    }
    sp->modflg++;  // XXX: should be done only upon modification
}

void cmd_pullcells(sheet_t *sp, int cmd) {
    pullcells(sp, qbuf, cmd, cellref_current(sp));
    qbuf = 0;
}

/* copy/move cell range from subsheet delbuf[src]
   using 2 slots delbuf[dbidx+1] and  delbuf[dbidx+2] */
void pullcells(sheet_t *sp, int src, int cmd, cellref_t cr) {
    struct ent *obuf;
    struct ent *p, *n, *next;
    int deltar, deltac;
    int minrow, mincol;
    int maxrow, maxcol;
    int numrows, numcols;
    int i;
    struct frange *fr;

    if (!delbuf[src].ptr) {
        error("No data to pull");
        return;
    }
    /* push qbuf on delbuf stack */
    obuf = delbuf[src].ptr;  /* orig. contents of the del. buffer */
    delbuf_copy(++dbidx, src);

    /* compute delbuf range */
    minrow = sp->maxrows;
    mincol = sp->maxcols;
    maxrow = 0;
    maxcol = 0;

    for (p = delbuf[dbidx].ptr; p; p = p->next) {
        if (p->row < minrow) minrow = p->row;
        if (p->row > maxrow) maxrow = p->row;
        if (p->col < mincol) mincol = p->col;
        if (p->col > maxcol) maxcol = p->col;
        p->flags |= MAY_SYNC;
    }

    // XXX: should check consistency with delbuf[dbidx].ncols/nrows
    numrows = maxrow - minrow + 1;
    numcols = maxcol - mincol + 1;
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
    if (cmd == 'C') {     /* PULL full? */
        minrow = 0;
        mincol = 0;
        maxrow = sp->maxrows;
        maxcol = sp->maxcols;
    } else
    if (cmd == 'r') {     /* PULLROWS */
        if (!insertrows(sp, cr, numrows, 0))
            return; // XXX: should update dbidx
        if ((fr = frange_find(sp, cr.row, cr.col))) {
            deltac = fr->or_left->col - mincol;
        } else {
            if (delbuf[dbidx].rowfmt) {
                // XXX: incorrect if destination range has more rows than delbuf
                for (i = 0; i < numrows; i++) {
                    sp->rowfmt[cr.row + i] = delbuf[dbidx].rowfmt[i];
                }
            }
            deltac = 0;
        }
    } else
    if (cmd == 'c') {     /* PULLCOLS */
        if (!insertcols(sp, cr, numcols, 0))
            return; // XXX: should update dbidx
        if (delbuf[dbidx].colfmt) {
            // XXX: incorrect if destination range has more columns than delbuf
            for (i = 0; i < numcols; i++) {
                sp->colfmt[cr.col + i] = delbuf[dbidx].colfmt[i];
            }
        }
        deltar = 0;
    } else
    if (cmd == 'x') {     /* PULLXCHG */
        /* Save the original contents of the destination range on the
         * delete buffer stack in preparation for the exchange, then swap
         * the top two elements on the stack, so that the original cells
         * to be pulled are still on top.
         */
        /* move cell range to subsheet delbuf[++dbidx] */
        erase_area(sp, ++dbidx, minrow + deltar, mincol + deltac,
                   maxrow + deltar, maxcol + deltac, 0);
        /* swap temp buffers at top of stack */
        delbuf_swap(dbidx, dbidx - 1);
    } else
    if (cmd == 'p') {     /* PULL */
        /* move cell range to subsheet delbuf[++dbidx] */
        erase_area(sp, ++dbidx, minrow + deltar, mincol + deltac,
                   maxrow + deltar, maxcol + deltac, 0);
        sync_refs(sp);
        delbuf_free(dbidx--);
    } else
    if (cmd == 't') {     /* PULLTP (transpose) */
        /* move cell range to subsheet delbuf[++dbidx] */
        erase_area(sp, ++dbidx, minrow + deltar, mincol + deltac,
                   minrow + deltar + maxcol - mincol,
                   mincol + deltac + maxrow - minrow, 0);
        sync_refs(sp);
        delbuf_free(dbidx--);
    }

    FullUpdate++;
    sp->modflg++;

    /* At this point, we copy the cells from the delete buffer into the
     * destination range.
     */
    for (p = delbuf[dbidx].ptr; p; p = p->next) {
        if (cmd == 't') {   /* Transpose rows and columns while pulling. */
            n = lookat(sp, minrow + deltar + p->col - mincol,
                       mincol + deltac + p->row - minrow);
        } else {
            n = lookat(sp, p->row + deltar, p->col + deltac);
        }
        copyent(sp, n, p, deltar, deltac, minrow, mincol, maxrow, maxcol, cmd);
    }

    /* Now exchange them so that the original cells from the delete buffer
     * are in the destination range instead of the copies.  When doing a
     * "pull exchange" ("px" or "pullxchg"), exchange the original contents
     * of the destination range with the contents of the delete buffer
     * instead.  Don't do this if transposing or merging (including merging
     * cell formats), or if the expressions in the destination cells have
     * been adjusted during a copy.
     */
    if (cmd != 't' && cmd != 'm' && cmd != 'f' && cmd != 'C') {
        /* cmd is in PULL, PULLROWS, PULLCOLS, PULLXCHG */
        if (cmd == 'x') {     /* PULLXCHG */
            /* swap temp buffers at top of stack */
            delbuf_swap(dbidx, dbidx - 1);
        } else {
            // XXX: very confusing!
            /* move the unlocked cells from destination range to delbuf[dbidx+1] */
            // XXX: this does nothing for PULLROWS, PULLCOLS as the range is empty
            // XXX: should use erase_area() ?
            for (p = delbuf[dbidx++].ptr; p; p = p->next) {
                // XXX: delbuf[dxidx].colfmt and delbuf[dxidx].rowfmt are NULL
                killcell(sp, p->row + deltar, p->col + deltac, dbidx, 0, 1);
            }
        }
        /* move the cells from delbuf to destination */
        delbuf_swap(dbidx, dbidx - 1);
        for (p = delbuf[dbidx].ptr; p; p = next) {
            next = p->next;
            // XXX: leak if destination is locked ?
            p->row += deltar;
            p->col += deltac;
            p->flags &= ~IS_DELETED;
            setcell(sp, p->row, p->col, p);
        }
        delbuf[dbidx].ptr = NULL;
        delbuf_free(dbidx--);
        /* leave original cells at top of stack */
        sync_refs(sp);
        /*
         * Now change the cell addresses in the delete buffer to match
         * where the original cells came from.
         */
        for (p = delbuf[dbidx].ptr; p; p = p->next) {
            p->row -= deltar;
            p->col -= deltac;
        }
    } else {
        sync_refs(sp);
    }

    /* Now make sure all references to the pulled cells in all named buffers
     * point to the new set of cells in the delete buffer.
     */
    // XXX: should use deldata_store_qbuf?
    // XXX: obuf could be a stale pointer
    // XXX: delbuf[i].colfmt and delbuf[i].rowfmt could be lost?
    for (i = 0; i < DELBUFSIZE; i++) {
        if (delbuf[i].ptr == obuf) {
            delbuf[i].ptr = delbuf[dbidx].ptr;
        }
    }
    delbuf_free(dbidx--);
}

/* delete numrow rows, starting with rs */
static void closerow(sheet_t *sp, int idx, int rs, int numrow) {
    int r, c, i;
    struct ent **tmprow;
    rowfmt_t def_rowfmt = { FALSE };

    if (rs + numrow - 1 > sp->maxrow) return;
    r = rs;

    /*
     * Rows are dealt with in numrow groups, each group of rows spaced numrow
     * rows apart.
     */
    for (i = 0; i < numrow; i++) {
        r = rs + i;

        /* empty the first row of the group */
        for (c = 0; c < sp->maxcol; c++) {
            killcell(sp, r, c, idx, 1, 1);
        }

        /* move the rows, put the deleted, but now empty, row at the end */
        tmprow = sp->tbl[r];
        for (; r + numrow < sp->maxrows - 1; r += numrow) {
            sp->rowfmt[r] = sp->rowfmt[r + numrow];
            sp->tbl[r] = sp->tbl[r + numrow];
            for (c = 0; c < sp->maxcols; c++) {
                struct ent *p = getcell(sp, r, c);
                if (p)
                    p->row = r;
            }
        }
        sp->tbl[r] = tmprow;
        sp->rowfmt[r] = def_rowfmt;
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

    sp->maxrow -= numrow;

    /* Update note links. */
    for (r = 0; r <= sp->maxrow; r++) {
        for (c = 0; c <= sp->maxcol; c++) {
            struct ent *p = getcell(sp, r, c);
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
    sp->modflg++;
}

/* delete group of columns (1 or more) */
void deletecols(sheet_t *sp, int c1, int c2) {
    int r, c, ncols, i, save = sp->curcol;
    struct ent **pp;
    struct ent *p;
    colfmt_t def_colfmt = { FALSE, DEFWIDTH, DEFPREC, DEFREFMT };

    if (c1 > c2) SWAPINT(c1, c2);
    if (c2 > sp->maxcol)
        c2 = sp->maxcol;
    if (c1 > sp->maxcol) {
        /* deleting columns beyond the active area: nothing to do */
        return;
    }

    if (any_locked_cells(sp, 0, c1, sp->maxrow, c2)) {
        error("Locked cells encountered. Nothing changed");
        return;
    }
    ncols = c2 - c1 + 1;
    // XXX: probably useless and counterproductive
    //      this is for frange_get_current(sp) which is questionable
    sp->curcol = c1;

    /* discard named buffer '9' and qbuf if any */
    deldata_discard(DELBUF_9);
    sync_refs(sp);
    /* move cell range to subsheet delbuf[0] */
    erase_area(sp, dbidx = 0, 0, c1, sp->maxrow, c2, 0);
    // XXX: should use sync_refs() to flag invalid references
    fix_ranges(sp, -1, c1, -1, c2, -1, -1, frange_get_current(sp));
    /* store delbuf[dbidx] to named buffer '1' after rotation and qbuf if any */
    deldata_store(dbidx, DELBUF_1, DELBUF_9, DD_UNSYNC);
    /* purposely leave dbidx == 0 so other commands do not clobber it */

    /* clear then copy the block left */
    for (r = 0; r <= sp->maxrow; r++) {
        for (c = c1; c <= sp->maxcol - ncols; c++) {
            // XXX: should factorize as movecell()
            pp = ATBL(sp, r, c);
            if ((*pp = pp[ncols])) {
                (*pp)->col -= ncols;
                pp[ncols] = NULL;
            }
        }
    }

    for (c = c1; c <= sp->maxcol - ncols; c++) {
        sp->colfmt[c] = sp->colfmt[c + ncols];
    }
    for (; c <= sp->maxcol; c++) {
        sp->colfmt[c] = def_colfmt;
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

    sp->maxcol -= ncols;

    /* Update note links. */
    for (r = 0; r <= sp->maxrow; r++) {
        for (c = 0; c <= sp->maxcol; c++) {
            p = getcell(sp, r, c);
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
    FullUpdate++;
    sp->modflg++;
    sp->curcol = save < c1 ? save : (save <= c2) ? c1 : save - ncols;
}

/* Modified 9/17/90 THA to handle more formats */
void cmd_format(sheet_t *sp, int c1, int c2, int w, int p, int r) {
    int i;
    int crows = 0;
    int ccols = c2;

    if (c1 >= sp->maxcols && !growtbl(sp, GROWCOL, 0, c1)) c1 = sp->maxcols-1;
    if (c2 >= sp->maxcols && !growtbl(sp, GROWCOL, 0, c2)) c2 = sp->maxcols-1;

    if (w <= 0) {
        error("Width too small - setting to 1");
        w = 1;
    }

    if (usecurses) {
        int maxwidth = screen_COLS - sp->rescol - 2;
        if (w > maxwidth) {
            error("Width too large - Maximum = %d", maxwidth);
            w = maxwidth;
        }
    }

    if (p < 0)
        p = 0;
    if (p > w) {
        error("Precision too large");
        p = w;
    }

    if (r < 0)
        r = 0;
    if (r > 255)
        r = 255;
    checkbounds(sp, &crows, &ccols);
    if (ccols < c2) {
        error("Format statement failed to create implied column %d", c2);
        return;
    }

    for (i = c1; i <= c2; i++) {
        sp->colfmt[i].fwidth = w;
        sp->colfmt[i].precision = p;
        sp->colfmt[i].realfmt = r;
    }
    FullUpdate++;
    sp->modflg++;
}

void range_align(sheet_t *sp, rangeref_t rr, int align) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p && (p->flags & ALIGN_MASK) != align) {
                p->flags &= ~ALIGN_MASK;
                p->flags |= IS_CHANGED | align;
                changed++;
                sp->modflg++;
            }
        }
    }
}

/*---------------- copying and moving ----------------*/

static void copydbuf(sheet_t *sp, int idx, int deltar, int deltac) {
    struct ent *p = delbuf[idx].ptr;

    while (p) {
        int vr = p->row + deltar;
        int vc = p->col + deltac;
        struct ent *n = lookat(sp, vr, vc);
        if (n->flags & IS_LOCKED)
            continue;
        copyent(sp, n, p, deltar, deltac, 0, 0, sp->maxrow, sp->maxcol, 0);
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

void copy(sheet_t *sp, int flags, rangeref_t drr, rangeref_t srr) {
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
        // XXX: should just use dbidx=1
        if (!delbuf[qbuf].ptr) {
            // nothing to copy
            return;
        }
        delbuf_copy(++dbidx, qbuf);
        minsr = sp->maxrow;
        minsc = sp->maxcol;
        maxsr = 0;
        maxsc = 0;
        // XXX: delbufs should behave like worksheets
        //      there should be a defined range,
        //      col and row properties and
        //      a sparse cell array.
        for (p = delbuf[dbidx].ptr; p; p = p->next) {
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

    checkbounds(sp, &maxdr, &maxdc);

    if (maxdr - mindr < maxsr - minsr) maxdr = mindr + (maxsr - minsr);
    if (maxdc - mindc < maxsc - minsc) maxdc = mindc + (maxsc - minsc);

    if (!(flags & COPY_FROM_QBUF)) {
        /* copy source cells to delbuf[++dbidx] */
        // XXX: should not be necessary if copy order is well chosen
        //      or if source and destination do not overlap
        // XXX: should set dbidx=1
        yank_area(sp, ++dbidx, rangeref(minsr, minsc, maxsr, maxsc));
    }

    /* move cell range to subsheet delbuf[++dbidx] */
    erase_area(sp, ++dbidx, mindr, mindc, maxdr, maxdc, 0);
    sync_refs(sp);
    delbuf_free(dbidx--);

    error("Copying...");
    if (!loading)
        screen_refresh();
    p = delbuf[dbidx].ptr;
    if (minsr == maxsr && minsc == maxsc) {
        /* Source is a single cell */
        for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++) {
            for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
                copydbuf(sp, dbidx, deltar, deltac);
        }
    } else
    if (minsr == maxsr) {
        /* Source is a single row */
        deltac = mindc - p->col;
        for (deltar = mindr - p->row; deltar <= maxdr - p->row; deltar++)
            copydbuf(sp, dbidx, deltar, deltac);
    } else
    if (minsc == maxsc) {
        /* Source is a single column */
        deltar = mindr - p->row;
        for (deltac = mindc - p->col; deltac <= maxdc - p->col; deltac++)
            copydbuf(sp, dbidx, deltar, deltac);
    } else {
        /* Everything else */
        deltar = mindr - p->row;
        deltac = mindc - p->col;
        copydbuf(sp, dbidx, deltar, deltac);
    }

    if (!(flags & COPY_FROM_QBUF)) {
        sync_refs(sp);
        delbuf_free(dbidx--);
    }

    if (flags & COPY_FROM_QBUF) {
        delbuf_clear(dbidx--);
        qbuf = 0;
    }
    error("Copy done.");
}

/* ERASE a Range of cells */
void eraser(sheet_t *sp, rangeref_t rr) {
    /* discard named buffer '9' and qbuf if any */
    deldata_discard(DELBUF_9);
    // XXX: missing sync_refs() ???
    /* move cell range to subsheet delbuf[0] */
    erase_area(sp, dbidx = 0, rr.left.row, rr.left.col, rr.right.row, rr.right.col, 0);
    sync_refs(sp);
    /* store delbuf[dbidx] to named buffer '1' after rotation and qbuf if any */
    // XXX: missing DD_UNSYNC ???
    deldata_store(dbidx, DELBUF_1, DELBUF_9, 0);
    /* purposely leave dbidx == 0 so other commands do not clobber it */

    FullUpdate++;
    sp->modflg++;
}

/* YANK a Range of cells */
void yankr(sheet_t *sp, rangeref_t rr) {
    /* discard named buffer '0' and qbuf if any */
    deldata_discard(DELBUF_0);
    /* copy source cells to delbuf[0] */
    yank_area(sp, dbidx = 0, rr);
    /* set yanked data in named buffer '0' and qbuf if any */
    deldata_store(dbidx, DELBUF_0, DELBUF_0, 0);
    /* purposely leave dbidx == 0 so other commands do not clobber it */
}

/* MOVE a Range of cells */
void mover(sheet_t *sp, cellref_t cr, rangeref_t rr) {
    dbidx = 0;  /* do not clobber delbuf[0] */
    move_area(sp, cr.row, cr.col, rr);
    sync_refs(sp);
    FullUpdate++;
}

/* fill a range with constants */
void fillr(sheet_t *sp, rangeref_t rr, double start, double inc, int bycols) {
    int r, c;

    range_normalize(&rr);

    if (bycols) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            for (r = rr.left.row; r <= rr.right.row; r++) {
                struct ent *n = lookat(sp, r, c);
                if (n->flags & IS_LOCKED)
                    continue;
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->type = SC_NUMBER;
                n->flags &= ~IS_CLEARED;
                n->flags |= IS_CHANGED;
            }
        }
    } else {
        for (r = rr.left.row; r <= rr.right.row; r++) {
            for (c = rr.left.col; c <= rr.right.col; c++) {
                struct ent *n = lookat(sp, r, c);
                if (n->flags & IS_LOCKED)
                    continue;
                // XXX: why clear the format and alignment?
                clearent(n);
                n->v = start;
                start += inc;
                n->type = SC_NUMBER;
                n->flags &= ~IS_CLEARED;
                n->flags |= IS_CHANGED;
            }
        }
    }
    FullUpdate++;
    changed++;
    sp->modflg++;
}

/* lock a range of cells */
void lock_cells(sheet_t *sp, rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *n = lookat(sp, r, c);
            // XXX: update IS_CHANGED?
            n->flags |= IS_LOCKED;
        }
    }
    sp->modflg++;
}

/* unlock a range of cells */
void unlock_cells(sheet_t *sp, rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *n = getcell(sp, r, c);
            if (n) {
                // XXX: update IS_CHANGED?
                n->flags &= ~IS_LOCKED;
            }
        }
    }
    sp->modflg++;
}

void format_cells(sheet_t *sp, rangeref_t rr, SCXMEM string_t *s) {
    int r, c;

    if (s && !*s2c(s)) {
        string_free(s);
        s = NULL;
    }

    // XXX: should be skip locked cells silently
    //      or should be fail with an error
    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = lookat(sp, r, c);
            if (p->flags & IS_LOCKED)
                continue;
            string_set(&p->format, string_dup(s));
            p->flags |= IS_CHANGED;
        }
    }
    string_free(s);
    FullUpdate++;
    sp->modflg++;
}

/*
 * sync_refs and sync_expr are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
static void sync_expr(sheet_t *sp, enode_t *e) {
    if (e == NULL)
        return;

    switch (e->type) {
    case OP_TYPE_RANGE:
        e->e.r.left.vp = lookat(sp, e->e.r.left.vp->row, e->e.r.left.vp->col);
        e->e.r.right.vp = lookat(sp, e->e.r.right.vp->row, e->e.r.right.vp->col);
        break;
    case OP_TYPE_VAR:
        if (e->e.v.vp->flags & IS_CLEARED) {
            e->op = OP__ERROR;
            e->type = OP_TYPE_ERROR;
            e->e.error = ERROR_REF;
        } else
        if (e->e.v.vp->flags & MAY_SYNC) {
            e->e.v.vp = lookat(sp, e->e.v.vp->row, e->e.v.vp->col);
        }
        break;
    case OP_TYPE_FUNC: {
            int i;
            for (i = 0; i < e->nargs; i++)
                sync_expr(sp, e->e.args[i]);
        }
        break;
    }
}

void sync_refs(sheet_t *sp) {
    int i, j;
    struct ent *p;

    sync_ranges(sp);

    // XXX: sync_ranges() already does sync_enode(p->expr)
    for (i = 0; i <= sp->maxrow; i++) {
        for (j = 0; j <= sp->maxcol; j++) {
            if ((p = getcell(sp, i, j)) && p->expr)
                sync_expr(sp, p->expr);
        }
    }
    for (i = 0; i < DELBUFSIZE; i++) {
        for (p = delbuf[i].ptr; p; p = p->next) {
            if (p->expr)
                sync_expr(sp, p->expr);
        }
    }
}

/* mark rows as hidden */
void hiderows(sheet_t *sp, int r1, int r2) {
    int r;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r1 < 0) {
        error("Invalid range");
        return;
    }
    if (r2 + 1 >= sp->maxrows) {
        if (!growtbl(sp, GROWROW, r2 + 1, 0)) {
            // XXX: should remove this restriction
            error("You cannot hide the last row");
            return;
        }
    }
    if (r2 + 1 > sp->maxrow)
        sp->maxrow = r2 + 1;

    for (r = r1; r <= r2; r++)
        sp->rowfmt[r].hidden = TRUE;

    if (sp->currow >= r1)
        sp->currow = (sp->currow <= r2) ? r2 + 1 : sp->currow - (r2 - r1 + 1);

    FullUpdate++;
    sp->modflg++;
}

/* mark columns as hidden */
void hidecols(sheet_t *sp, int c1, int c2) {
    int c;

    if (c1 > c2) SWAPINT(c1, c2);
    if (c1 < 0) {
        error("Invalid range");
        return;
    }
    if (c2 + 1 >= sp->maxcols) {
        if (!growtbl(sp, GROWCOL, 0, c2 + 1)) {
            // XXX: should remove this restriction
            error("You cannot hide the last column");
            return;
        }
    }
    if (c2 + 1 > sp->maxcol)
        sp->maxcol = c2 + 1;

    for (c = c1; c <= c2; c++)
        sp->colfmt[c].hidden = TRUE;

    if (sp->curcol >= c1)
        sp->curcol = (sp->curcol <= c2) ? c2 + 1 : sp->curcol - (c2 - c1 + 1);

    FullUpdate++;
    sp->modflg++;
}

void dohide(sheet_t *sp) {
    if (sp->showrange == SHOWROWS) {
        hiderows(sp, sp->currow, sht->showsr);
    } else
    if (sp->showrange == SHOWCOLS) {
        hidecols(sp, sp->curcol, sht->showsc);
    }
}

/* mark a row as not-hidden */
void showrow(sheet_t *sp, int r1, int r2) {
    if (r1 > r2) SWAPINT(r1, r2);
    if (r1 < 0) {
        error("Invalid range");
        return;
    }
    if (r2 > sp->maxrow)
        r2 = sp->maxrow;

    FullUpdate++;
    sp->modflg++;
    while (r1 <= r2) {
        sp->rowfmt[r1++].hidden = FALSE;
    }
}

/* mark a column as not-hidden */
void showcol(sheet_t *sp, int c1, int c2) {
    if (c1 > c2) SWAPINT(c1, c2);
    if (c1 < 0) {
        error("Invalid range");
        return;
    }
    if (c2 > sp->maxcol)
        c2 = sp->maxcol;

    FullUpdate++;
    sp->modflg++;
    while (c1 <= c2)
        sp->colfmt[c1++].hidden = FALSE;
}

/* Copy a cell (struct ent).  "special" indicates special treatment when
 * merging two cells for the "pm" command, merging formats only for the
 * "pf" command, or for adjusting cell references when transposing with
 * the "pt" command.  r1, c1, r2, and c2 define the range in which the dr
 * and dc values should be used.
 */
void copyent(sheet_t *sp, struct ent *n, struct ent *p, int dr, int dc,
             int r1, int c1, int r2, int c2, int special)
{
    if (!n || !p) {
        error("internal error");
        return;
    }
    if (special != 'f') {
        /* transfer value unless merging and no value */
        if (special != 'm' || p->type != SC_EMPTY) {
            n->type = p->type;
            n->cellerror = p->cellerror;
            n->v = p->v;
            string_set(&n->label, string_dup(p->label));
        }
        if (special != 'm' || p->expr) {
            efree(n->expr);
            n->expr = copye(sp, p->expr, dr, dc, r1, c1, r2, c2, special == 't');
        }
        /* transfer alignment and LOCKED flag */
        n->flags &= ~ALIGN_MASK;
        n->flags |= p->flags & (ALIGN_MASK | IS_LOCKED);
    }
    if (p->format) {
        string_set(&n->format, string_dup(p->format));
    } else
    if (special != 'm' && special != 'f') {
        string_set(&n->format, NULL);
    }
    n->flags |= IS_CHANGED;
}

/* erase the database (sheet data, etc.) */
void erasedb(sheet_t *sp) {
    int r, c;

    for (r = 0; r <= sp->maxrow; r++) {
        for (c = 0; c <= sp->maxcol; c++) {
            // XXX: should factorize as erasecell()
            struct ent **pp = ATBL(sp, r, c);
            struct ent *p;
            if ((p = *pp)) {
                *pp = NULL;
                efree(p->expr);
                p->expr = NULL;
                string_set(&p->label, NULL);
                string_set(&p->format, NULL);
                p->next = freeents; /* save [struct ent] for reuse */
                freeents = p;
            }
        }
    }
    /* free all sheet data */
    for (r = 0; r < sp->maxrows; r++) {
        scxfree(sp->tbl[r]);
    }
    scxfree(sp->tbl);
    scxfree(sp->rowfmt);
    scxfree(sp->row_size);
    scxfree(sp->colfmt);
    sp->tbl = NULL;
    sp->rowfmt = NULL;
    sp->row_size = NULL;
    sp->colfmt = NULL;

    for (c = 0; c < COLFORMATS; c++) {
        string_set(&sp->colformat[c], NULL);
    }
    nrange_clean(sp);
    frange_clean(sp);
    crange_clean(sp);
    abbrev_clean(sp);

    string_set(&sp->mdir, NULL);
    string_set(&sp->autorun, NULL);
    for (c = 0; c < FKEYS; c++) {
        string_set(&sp->fkey[c], NULL);
    }
    /* unset all marks */
    for (c = 0; c < 37; c++) {
        savedcr[c] = cellref(-1, -1);
        savedst[c] = cellref(-1, -1);
    }
    qbuf = 0;

    sheet_init(sp);

    if (usecurses)
        select_style(STYLE_NONE, 0);

    sp->curfile[0] = '\0';
    FullUpdate++;
}

/* Returns 1 if cell is locked, 0 otherwise */
int locked_cell(sheet_t *sp, int row, int col) {
    struct ent *p = getcell(sp, row, col);
    if (p && (p->flags & IS_LOCKED)) {
        error("Cell %s%d is locked", coltoa(col), row);
        return 1;
    }
    return 0;
}

/* Check if area contains locked cells */
static int any_locked_cells(sheet_t *sp, int r1, int c1, int r2, int c2) {
    int r, c;
    struct ent *p;

    for (r = r1; r <= r2; r++) {
        for (c = c1; c <= c2; c++) {
            p = getcell(sp, r, c);
            if (p && (p->flags & IS_LOCKED))
                return 1;
        }
    }
    return 0;
}

void set_mdir(sheet_t *sp, SCXMEM string_t *str) {
    string_set(&sp->mdir, str);
    sp->modflg++;
}

void set_autorun(sheet_t *sp, SCXMEM string_t *str) {
    string_set(&sp->autorun, str);
    sp->modflg++;
}

void set_fkey(sheet_t *sp, int n, SCXMEM string_t *str) {
    if (n >= 0 && n < FKEYS) {
        string_set(&sp->fkey[n], str);
        sp->modflg++;
    } else {
        string_free(str);
        error("Invalid function key");
    }
}

void cmd_select_qbuf(char c) {
    if (c >= '0' && c <= '9') {
        qbuf = c - '0' + DELBUF_0;
    } else if (c >= 'a' && c <= 'z') {
        qbuf = c - 'a' + DELBUF_A;
    } else if (c == '"') {
        qbuf = 0;
    } else {
        error("Invalid buffer");
    }
}

void cmd_setformat(sheet_t *sp, int n, SCXMEM string_t *str) {
    if (n >= 0 && n < 10) {
        if (str && !*s2c(str)) {
            string_free(str);
            str = NULL;
        }
        string_set(&sp->colformat[n], str);
        FullUpdate++;
        sp->modflg++;
    } else {
        error("Invalid format number");
        string_free(str);
    }
}

void cmd_run(SCXMEM string_t *str) {
    const char *cmd = s2str(str);
    screen_deraw(1);
    system(cmd);
    if (*cmd && cmd[strlen(cmd) - 1] != '&') {
        screen_pause();
    }
    screen_goraw();
    string_free(str);
}

void note_add(sheet_t *sp, cellref_t cr, rangeref_t rr) {
    struct ent *p = lookat(sp, cr.row, cr.col);
    if (p) {
        range_normalize(&rr);
        p->nrr = rr;
        p->flags |= HAS_NOTE | IS_CHANGED;
        FullUpdate++;
        sp->modflg++;
    }
}

void note_delete(sheet_t *sp, cellref_t cr) {
    struct ent *p = getcell(sp, cr.row, cr.col);
    if (p && (p->flags & HAS_NOTE)) {
        p->nrr = rangeref_empty();
        p->flags ^= HAS_NOTE;
        p->flags |= IS_CHANGED;
        sp->modflg++;
    }
}
