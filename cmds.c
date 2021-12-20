/*      SC      A Spreadsheet Calculator
 *              Command routines
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#include "sc.h"

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We use it for 36 named registers (a-z,0-9), 1 unnamed register ("),
 * and 2 temporary registers.
 */

typedef struct subsheet {
    int minrow, mincol, maxrow, maxcol;
    int ncols, nrows, num, refs;
    SCXMEM rowptr_t *tbl;
    SCXMEM colfmt_t *colfmt;
    SCXMEM rowfmt_t *rowfmt;
} subsheet_t;

#define DELBUF_COUNT  40   /* Number of named buffers + 4 */
#define DELBUF_DEF   0
#define DELBUF_TMP1  1
#define DELBUF_TMP2  2
#define DELBUF_A    (DELBUF_COUNT - 26)
#define DELBUF_0    (DELBUF_COUNT - 36)
#define DELBUF_1    (DELBUF_0 + 1)
#define DELBUF_9    (DELBUF_0 + 9)

static subsheet_t delbuf_array[DELBUF_COUNT];
static subsheet_t *delbuf[DELBUF_COUNT];
static int qbuf;       /* register no. specified by " command */

/* a linked list of free [struct ent]'s, uses .next as the pointer */
static struct ent *free_ents;

#define ATBL(sp, row, col)     (&(sp)->tbl[row].cp[col])

static int any_locked_cells(sheet_t *sp, rangeref_t rr);
static void move_area(sheet_t *sp, int dr, int dc, rangeref_t rr);
static void yank_area(sheet_t *sp, int idx, rangeref_t rr);
#define KA_NO_FMT   1
#define KA_COPY     2
#define KA_MOVE     4
static void kill_area(sheet_t *sp, int idx, rangeref_t rr, int flags);
// XXX: should use rangeref_t for adjust range
static void ent_copy(sheet_t *sp, struct ent *n, struct ent *p,
                     int dr, int dc, int r1, int c1, int r2, int c2, int transpose);

#define fix_ranges(sp, row1, col1, row2, col2, delta1, delta2, fr)
#define sync_refs(sp)
#define sync_ranges(sp)

static void ent_free(struct ent *p) {
    if (p) {
        efree(p->expr);
        p->expr = NULL;
        string_set(&p->label, NULL);
        string_set(&p->format, NULL);
        p->v = 0.0;
        p->cellerror = 0;
        p->type = SC_EMPTY;
        p->flags = IS_CHANGED | IS_CLEARED;
        p->next = free_ents;     /* put this ent on the front of free_ents */
        free_ents = p;
    }
}

void delbuf_init(void) {
    int i;
    subsheet_t *db;
    for (i = 0, db = delbuf_array; i < DELBUF_COUNT; i++, db++) {
        db->num = i + 1;
        db->maxrow = db->maxcol = -1;
    }
}

/* swap 2 entries in the delbuf_ptr array */
static void delbuf_swap(int idx1, int idx2) {
    subsheet_t *tmpbuf = delbuf[idx1];
    delbuf[idx1] = delbuf[idx2];
    delbuf[idx2] = tmpbuf;
}

/* rotate a range of entries in the delbuf array */
static void delbuf_rotate(int idx1, int idx2) {
    while (idx2 --> idx1) {
        delbuf_swap(idx2 + 1, idx2);
    }
}

static void delbuf_clear(subsheet_t *db) {
    if (db) {
        db->minrow = db->mincol = 0;
        db->maxrow = db->maxcol = -1;
        db->ncols = db->nrows = 0;
        db->tbl = NULL;
        db->colfmt = NULL;
        db->rowfmt = NULL;
    }
}

/* unlink subsheet data, preserve duplicates if any */
static void delbuf_free(int idx) {
    subsheet_t *db;

    if (idx < 0 || !(db = delbuf[idx]))
        return;

    delbuf[idx] = NULL;
    if (--db->refs == 0) {
        /* free subsheet data if no duplicate found */
        // XXX: should use fast cell iterator
        int r, c;
        for (r = 0; r < db->nrows; r++) {
            for (c = 0; c < db->ncols; c++) {
                struct ent *p = db->tbl[r].cp[c];
                if (p) {
                    ent_free(p);
                    db->tbl[r].cp[c] = NULL;
                }
            }
            scxfree(db->tbl[r].cp);
            db->tbl[r].cp = NULL;
        }
        scxfree(db->tbl);
        db->tbl = NULL;
        scxfree(db->rowfmt);
        db->rowfmt = NULL;
        scxfree(db->colfmt);
        db->colfmt = NULL;
        delbuf_clear(db);
    }
}

static subsheet_t *delbuf_find(int idx) {
    int i;
    subsheet_t *db = &delbuf_array[idx];
    if (db->refs > 0) {
        for (i = 0, db = delbuf_array; i < DELBUF_COUNT; i++, db++) {
            if (db->refs == 0)
                break;
        }
    }
    db->refs++;
    return delbuf[idx] = db;
}

static void delbuf_copy(int src, int dest) {
    if (dest != src) {
        subsheet_t *db = delbuf[src];
        if (db) db->refs++;
        delbuf_free(dest);
        delbuf[dest] = db;
    }
}

// XXX: do we need this?
static void delbuf_unsync(int idx) {
    subsheet_t *db = delbuf[idx];
    int r, c;
    if (db) {
        // XXX: should use fast cell iterator
        for (r = 0; r < db->nrows; r++) {
            for (c = 0; c < db->ncols; c++) {
                struct ent *p = db->tbl[r].cp[c];
                if (p)
                    p->flags &= ~MAY_SYNC;
            }
        }
    }
}

void delbuf_list(sheet_t *sp, FILE *f) {
    int i, n = 0;
    for (i = 0; i < DELBUF_COUNT; i++) {
        int c = "\"@#$0123456789abcdefghijklmnopqrstuvwxyz"[i];
        subsheet_t *db = delbuf[i];
        if (db) {
            fprintf(f, "  \"%c   %2d  %2d  %s\n", c, db->num, db->refs,
                    range_addr(sp, rangeref(db->minrow, db->mincol, db->maxrow, db->maxcol)));
            if (brokenpipe) return;
            n++;
        }
    }
    if (!n) fprintf(f, "  No active registers\n");
}

void delbuf_clean(void) {
    struct ent *p, *next;
    int i;

    for (i = 0; i < DELBUF_COUNT; i++) {
        delbuf_free(i);
    }
    for (p = free_ents, free_ents = NULL; p; p = next) {
        next = p->next;
        scxfree(p);
    }
}

static void delbuf_setcell(subsheet_t *db, int row, int col, struct ent *p) {
    if (db && row >= db->minrow && row <= db->maxrow && col >= db->mincol && col <= db->maxcol) {
        db->tbl[row - db->minrow].cp[col - db->mincol] = p;
    } else {
        ent_free(p);
    }
}

struct ent *getcell(sheet_t *sp, int row, int col) {
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol)
        return *ATBL(sp, row, col);
    else
        return NULL;
}

int valid_cell(sheet_t *sp, int row, int col) {
    struct ent *p = getcell(sp, row, col);
    return (p != NULL && (p->expr || p->type != SC_EMPTY));
}

// XXX: should extend sheet if required and p != NULL
static int setcell(sheet_t *sp, int row, int col, struct ent *p) {
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol) {
        struct ent **pp = ATBL(sp, row, col);
        if (*pp && *pp != p) {
            ent_free(*pp);
        }
        *pp = p;
        FullUpdate++;  // XXX: really?
        changed++;
        sp->modflg++;
        return 1;
    }
    return 0;
}

static struct ent *ent_alloc(sheet_t *sp) {
    struct ent *p;
    if ((p = free_ents) != NULL) {
        free_ents = p->next;
    } else {
        p = scxmalloc(sizeof(struct ent));
        if (!p)
            return NULL;
    }
    p->v = 0.0;
    p->label = NULL;
    p->expr = NULL;
    p->format = NULL;
    p->cellerror = 0;
    p->type = SC_EMPTY;
    p->flags = MAY_SYNC;
    p->next = NULL;
    return p;
}

/* return a pointer to a cell structure [struct ent *],
   extending the sheet and allocating the structure if needed
 */
struct ent *lookat(sheet_t *sp, int row, int col) {
    struct ent **pp;

    if (row < 0 || col < 0)
        return NULL;
    if (row > sp->maxrow || col > sp->maxcol) {
        if (checkbounds(sp, row, col) < 0)
            return NULL;
        if (sp->maxrow < row) sp->maxrow = row;
        if (sp->maxcol < col) sp->maxcol = col;
    }
    pp = ATBL(sp, row, col);
    if (*pp == NULL) {
        *pp = ent_alloc(sp);
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
int dup_row(sheet_t *sp, cellref_t cr) {
    int row = cr.row, col;
    int c1 = 0;
    int c2 = sp->maxcol;
    struct frange *fr;

    if ((fr = frange_find(sp, cr.row, cr.col))) {
        c1 = fr->orr.left.col;
        c2 = fr->orr.right.col;
    }

    if (!insert_rows(sp, cr, 1, 1))
        return 0;

    sp->modflg++;
    // XXX: should use copy_area(row + 1, c1, row + 1, c2, row, c1, row, c2)
    for (col = c1; col <= c2; col++) {
        // XXX: should pass coordinates to ent_copy ?
        struct ent *p = getcell(sp, row, col);
        if (p) {
            struct ent *n = lookat(sp, row + 1, col);
            if (n)
                ent_copy(sp, n, p, 1, 0, 0, 0, sp->maxrow, sp->maxcol, 0);
        }
    }
    return 1;
}

/* duplicate the column at `cr.col` to the right it into a new column */
int dup_col(sheet_t *sp, cellref_t cr) {
    int row, col = cr.col;

    if (!insert_cols(sp, cr, 1, 1))
        return 0;

    sp->modflg++;
    // XXX: should use copy_area(0, col + 1, maxrow, col + 1, 0, col, maxrow, col)
    for (row = 0; row <= sp->maxrow; row++) {
        // XXX: should pass coordinates to ent_copy
        struct ent *p = getcell(sp, row, col);
        if (p) {
            struct ent *n = lookat(sp, row, col + 1);
            if (n)
                ent_copy(sp, n, p, 0, 1, 0, 0, sp->maxrow, sp->maxcol, 0);
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
int insert_rows(sheet_t *sp, cellref_t cr, int arg, int delta) {
    int r, lim;
    rangeref_t rr = rangeref(cr.row + delta, 0, sp->maxrow, sp->maxcol);
    adjust_ctx_t adjust_ctx;
    struct frange *fr;

    // XXX: no check for locked cells?

    if (checkbounds(sp, sp->maxrow + arg, 0) < 0)
        return 0;

    // XXX: should clip cr reference
    lim = cr.row + delta + arg - 1;  /* last inserted row */
    sp->maxrow += arg;

    if ((fr = frange_find(sp, cr.row, cr.col))) {
        // XXX: should verify if cr.row + delta is inside the frange
        // XXX: inconsistent source range: marks are updated beyond rr.right.row
        rr = rangeref(cr.row + delta, fr->orr.left.col,
                      fr->orr.right.row, fr->orr.right.col);
        move_area(sp, rr.left.row + arg, rr.left.col, rr);
#if 0
        if (!delta && fr->irr.left.row == cr.row + arg) {
            //fr->ir_left = lookat(sp, fr->irr.left.row - arg, fr->irr.left.col);
        }
        if (delta && fr->irr.right.row == cr.row) {
            //fr->ir_right = lookat(sp, fr->irr.right.row + arg, fr->irr.right.col);
        }
#endif
    } else {
        /*
         * save the last active row+1, shift the rows downward, put the last
         * row in place of the first
         */
        // XXX: this seems bogus, should implement a rotation
        rowptr_t tmp_row = sp->tbl[sp->maxrow];
        rowfmt_t tmp_fmt = sp->rowfmt[sp->maxrow];

        for (r = sp->maxrow; r > lim; r--) {
            sp->rowfmt[r] = sp->rowfmt[r-arg];
            sp->rowfmt[r-arg] = sp->rowfmt[r-1];
            sp->tbl[r] = sp->tbl[r-arg];
            sp->tbl[r-arg] = sp->tbl[r-1];
        }
        sp->rowfmt[r] = tmp_fmt;
        sp->tbl[r] = tmp_row;               /* the last row was never used.... */
    }
    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(-1, -1, -1, -1);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = -1;
    adjust_ctx.move_rr = rr;
    adjust_ctx.move_dr = arg;
    adjust_ctx.move_dc = 0;
    adjust_refs(&adjust_ctx);

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
int insert_cols(sheet_t *sp, cellref_t cr, int arg, int delta) {
    int r, c;
    struct ent **pp;
    /* cols are moved from sc1:sc2 to dc1:dc2 */
    int sc1 = cr.col + delta;
    int sc2 = sp->maxcol;
    int dc1 = sc1 + arg;
    int dc2 = sc2 + arg;
    struct frange *fr;
    colfmt_t def_colfmt = { FALSE, DEFWIDTH, DEFPREC, DEFREFMT };
    adjust_ctx_t adjust_ctx;

    if (checkbounds(sp, 0, sp->maxcol + arg) < 0)
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
                pp[c - arg] = NULL;
            }
        }
    }

    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(-1, -1, -1, -1);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = -1;
    adjust_ctx.move_rr = rangeref(0, sc1, sp->maxrow, sc2);
    adjust_ctx.move_dr = 0;
    adjust_ctx.move_dc = arg;
    adjust_refs(&adjust_ctx);

    // XXX: cell coordinates have been updated
    fr = frange_find(sp, cr.row, cr.col);
    if (delta) {
        // XXX: why not always fix frame limits?
        if (fr && fr->irr.right.col == cr.col) {
            //fr->ir_right = lookat(sp, fr->irr.right.row, fr->irr.right.col + arg);
        }
        fix_ranges(sp, -1, cr.col, -1, cr.col, 0, arg, fr);
    } else {
        if (fr && fr->irr.left.col == cr.col + arg) {
            //fr->ir_left = lookat(sp, fr->irr.left.row, fr->irr.left.col - arg);
        }
        fix_ranges(sp, -1, cr.col + arg, -1, cr.col + arg, arg, 0, fr);
    }
    FullUpdate++;
    sp->modflg++;
    return 1;
}

/* delete rows starting at r1 up to and including r2 */
void delete_rows(sheet_t *sp, int r1, int r2) {
    int nrows;
    int c1 = 0;
    int c2 = sp->maxcol;
    struct frange *fr = NULL;
    adjust_ctx_t adjust_ctx;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r2 > sp->maxrow)
        r2 = sp->maxrow;
    if (r1 > sp->maxrow) {
        /* deleting rows beyond the active area: nothing to do */
        // XXX: should still adjust references and set kill area?
        return;
    }

    nrows = r2 - r1 + 1;

    if (sp->currow == r1 && (fr = frange_get_current(sp))) {
        c1 = fr->orr.left.col;
        c2 = fr->orr.right.col;
    }
    if (any_locked_cells(sp, rangeref(r1, c1, r2, c2)))
        return;

    delbuf_free(DELBUF_9);
    delbuf_free(qbuf);
    sync_refs(sp);
    kill_area(sp, DELBUF_DEF, rangeref(r1, c1, r2, c2), KA_MOVE);
    // XXX: should delay until after area has moved
    fix_ranges(sp, r1, -1, r2, -1, -1, -1, fr);
    delbuf_copy(DELBUF_DEF, qbuf);
    qbuf = 0;
    delbuf_rotate(DELBUF_1, DELBUF_9);
    delbuf_copy(DELBUF_DEF, DELBUF_1);
    delbuf_unsync(DELBUF_DEF);

    if (fr) {
        // XXX: update current frame, possibly redundant with fix_ranges()
        //      should frame bottom rows should move up or not?
        //      the code is inconsistent
        if (r1 + nrows > fr->irr.right.row && fr->irr.right.row >= r1) {
            //fr->ir_right = lookat(sp, r1 - 1, fr->irr.right.col);
        }
        if (r1 + nrows > fr->orr.right.row) {
            //fr->or_right = lookat(sp, r1 - 1, fr->orr.right.col);
        } else {
            move_area(sp, r1, c1, rangeref(r1 + nrows, c1, fr->orr.right.row, c2));
        }
        if (fr->irr.left.row > fr->irr.right.row) {
            frange_delete(sp, fr);
            fr = NULL;
        }
    } else {
        /* moving whole rows by swapping row pointers */
        int r, dr;
        rowfmt_t def_rowfmt = { FALSE };

        /* reset row formats in target range */
        for (r = r1; r <= r2; r++) {
            sp->rowfmt[r] = def_rowfmt;
        }

        /* rotate row pointers and row formats */
        for (dr = r1; r <= sp->maxrow; r++, dr++) {
            /* swap rows and fix cell entries */
            rowptr_t tmprow = sp->tbl[dr];
            rowfmt_t tmpfmt = sp->rowfmt[dr];
            sp->tbl[dr] = sp->tbl[r];
            sp->rowfmt[dr] = sp->rowfmt[r];
            sp->tbl[r] = tmprow;
            sp->rowfmt[r] = tmpfmt;
        }
    }
    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(r1, c1, r2, c2);
    adjust_ctx.clamp_newr = r1;
    adjust_ctx.clamp_newc = -1;
    adjust_ctx.move_rr = rangeref(r2 + 1, c1, sp->maxrow, c2);
    adjust_ctx.move_dr = -nrows;
    adjust_ctx.move_dc = 0;
    adjust_refs(&adjust_ctx);
    if (!fr) {
        sp->maxrow -= nrows;
    }
    // XXX: missing fix_ranges() ?
    FullUpdate++;
    sp->modflg++;

    /* sp->currow will not become > maxrow because maxrow was not decremented */
    if (sp->currow > r1)
        sp->currow = (sp->currow <= r2) ? r1 : sp->currow - nrows;
}

void yank_rows(sheet_t *sp, int r1, int r2) {
    int arg, nrows;
    int c1 = 0, c2 = sp->maxcol;
    struct frange *fr;

    if (r1 > r2) SWAPINT(r1, r2);
    arg = r2 - r1 + 1;
    nrows = sp->maxrow - r1 + 1;

    // XXX: should check if r1 and r2 are inside current frange
    if (r1 == sp->currow && (fr = frange_get_current(sp))) {
        nrows = fr->orr.right.row - r1 + 1;
        c1 = fr->orr.left.col;
        c2 = fr->orr.right.col;
    }
    if (arg > nrows) {
        // XXX: should grow table or clip args
        error("Cannot yank %d %s, %d %s left",
              arg, (arg == 1 ? "row" : "rows"),
              nrows, (nrows == 1 ? "row" : "rows"));
        return;
    }
    sync_refs(sp); // XXX: why here?

    /* copy source cells to delbuf[0] */
    yank_area(sp, DELBUF_DEF, rangeref(r1, c1, r1 + arg - 1, c2));
    /* set yanked data in named buffer '0' or qbuf if any */
    delbuf_copy(DELBUF_DEF, qbuf ? qbuf : DELBUF_0);
    qbuf = 0;
}

void yank_cols(sheet_t *sp, int c1, int c2) {
    int arg, ncols;

    if (c1 > c2) SWAPINT(c1, c2);
    arg = c2 - c1 + 1;
    ncols = sp->maxcol - c1 + 1;

    if (arg > ncols) {
        // XXX: should grow table or clip args
        error("Cannot yank %d %s, %d %s left",
              arg, (arg == 1 ? "column" : "columns"),
              ncols, (ncols == 1 ? "column" : "columns"));
        return;
    }
    sync_refs(sp);

    /* copy source cells to delbuf[0] */
    yank_area(sp, DELBUF_DEF, rangeref(0, c1, sp->maxrow, c1 + arg - 1));
    /* set yanked data in named buffer '0' or qbuf if any */
    delbuf_copy(DELBUF_DEF, qbuf ? qbuf : DELBUF_0);
    qbuf = 0;
}

/* move cell range to subsheet delbuf[idx] */
static void kill_area(sheet_t *sp, int idx, rangeref_t rr, int flags) {
    subsheet_t *db;
    int r, c;
    int sr = rr.left.row;
    int sc = rr.left.col;
    int er = rr.right.row;
    int ec = rr.right.col;

    if (sr > er) SWAPINT(sr, er);
    if (sc > ec) SWAPINT(sc, ec);
    if (sr < 0) sr = 0;
    if (sc < 0) sc = 0;
    if (er > sp->maxrow) er = sp->maxrow;
    if (ec > sp->maxcol) ec = sp->maxcol;

    if (idx < 0) {
        /* just free the allocated cells */
        for (r = sr; r <= er; r++) {
            for (c = sc; c <= ec; c++) {
                struct ent **pp = ATBL(sp, r, c);
                struct ent *p = *pp;
                if (p) {
                    *pp = NULL;
                    ent_free(p);
                    FullUpdate++;  // XXX: really?
                    changed++;
                    sp->modflg++;
                }
            }
        }
        return;
    }

    delbuf_free(idx);
    db = delbuf_find(idx);
    if (!db)
        return;
    db->minrow = sr;
    db->mincol = sc;
    db->maxrow = er;
    db->maxcol = ec;
    db->ncols = ec - sc + 1;
    db->nrows = er - sr + 1;
    if (db->ncols < 0 || db->nrows < 0)
        return;

    /* Allocate cell pointer matrix */
    db->tbl = scxmalloc(db->nrows * sizeof(*db->tbl));
    for (r = 0; r < db->nrows; r++) {
        struct ent **pp = scxmalloc(db->ncols * sizeof(*pp));
        for (c = 0; c < db->ncols; c++) {
            pp[c] = NULL;
        }
        db->tbl[r].cp = pp;
        db->tbl[r].maxcol = ec;
    }
    if (!(flags & KA_NO_FMT)) {
        db->colfmt = scxmalloc(db->ncols * sizeof(*db->colfmt));
        for (c = sc; c <= ec; c++) {
            db->colfmt[c - sc] = sp->colfmt[c];
        }
        db->rowfmt = scxmalloc(db->nrows * sizeof(*db->rowfmt));
        for (r = sr; r <= er; r++) {
            db->rowfmt[r - sr] = sp->rowfmt[r];
        }
    }
    if (flags & KA_COPY) {
        for (r = sr; r <= er; r++) {
            for (c = sc; c <= ec; c++) {
                struct ent *p = getcell(sp, r, c);
                if (p) {
                    struct ent *n = ent_alloc(sp);
                    ent_copy(sp, n, p, 0, 0, 0, 0, sp->maxrow, sp->maxcol, 0);
                    //db->tbl[r - sr].cp[c - sc] = n;
                    delbuf_setcell(db, r, c, n);
                }
            }
        }
    } else
    if (flags & KA_MOVE) {
        for (r = sr; r <= er; r++) {
            for (c = sc; c <= ec; c++) {
                /* move a cell to the delbuf subsheet */
                struct ent **pp = ATBL(sp, r, c);
                struct ent *p = *pp;
                if (p) {
                    *pp = NULL;
                    p->flags |= IS_DELETED;
                    delbuf_setcell(db, r, c, p);
                    FullUpdate++;  // XXX: really?
                    changed++;
                    sp->modflg++;
                }
            }
        }
    }
}

/* copy a range of cells to delbuf[idx] */
static void yank_area(sheet_t *sp, int idx, rangeref_t rr) {
    range_clip(sp, range_normalize(&rr));
    /* copy cell range to subsheet delbuf[idx] */
    kill_area(sp, idx, rr, KA_COPY);
}

/* uses temporary subsheet DELBUF_TMP1 */
static void move_area(sheet_t *sp, int dr, int dc, rangeref_t rr) {
    struct ent *p;
    subsheet_t *db;
    int r, c, deltar, deltac;

    range_clip(sp, range_normalize(&rr));

    /* First we kill the source range to temporary subsheet TMP1 */
    kill_area(sp, DELBUF_TMP1, rr, KA_MOVE);

    deltar = dr - rr.left.row;
    deltac = dc - rr.left.col;

    /* Now we erase the destination range. We then move the original source
     * range from TMP1 to the destination range, adjusting the addresses as we go.
     */
    /* discard cell range to black hole */
    kill_area(sp, -1, rangeref(dr, dc, rr.right.row + deltar, rr.right.col + deltac), KA_NO_FMT);
    // XXX: if moving entire columns or rows, the column or row flags should be copied
    // XXX: this could be addressed with flags
    // XXX: references are updated by the caller

    db = delbuf[DELBUF_TMP1];
    if (db) {
        for (r = 0; r < db->nrows; r++) {
            for (c = 0; c < db->ncols; c++) {
                p = db->tbl[r].cp[c];
                if (p) {
                    db->tbl[r].cp[c] = NULL;
                    p->flags &= ~IS_DELETED;
                    setcell(sp, dr + r, dc + c, p);
                }
            }
        }
    }
    delbuf_free(DELBUF_TMP1);
}

/*
 * deletes the expression associated w/ a cell and turns it into a constant
 * containing whatever was on the screen
 */
void valueize_area(sheet_t *sp, rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    if (any_locked_cells(sp, rr))
        return;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p && p->expr) {
                efree(p->expr);
                p->expr = NULL;
                sp->modflg++;
            }
        }
    }
}

/* copy/move cell range from subsheet delbuf[src]
   using temporary subsheets DELBUF_TMP1 and DELBUF_TMP2 */
// XXX: should take destination range, instead of just corner cell?
static void pullcells(sheet_t *sp, int src, int cmd, cellref_t cr) {
    struct ent *p, *n;
    int minrow, mincol, maxrow, maxcol;
    int numrows, numcols, deltar, deltac;
    int i, row, col, r, c;
    struct frange *fr;
    subsheet_t *db = delbuf[src];
    subsheet_t *db1;

    if (!db || !db->nrows || !db->ncols) {
        error("No data to pull");
        return;
    }
    if (cmd == 'C') {
        /* pullcells is called with qbuf == idx when cmd == 'C' */
        copy_range(sp, COPY_FROM_QBUF, rangeref_current(sp), rangeref_empty());
        return;
    }
    /* compute delbuf range */
    minrow = db->minrow;
    maxrow = db->maxrow;
    mincol = db->mincol;
    maxcol = db->maxcol;

    // XXX: should check consistency with delbuf[src].ncols/nrows
    numrows = maxrow - minrow + 1;
    numcols = maxcol - mincol + 1;
    deltar = cr.row - minrow;
    deltac = cr.col - mincol;

    /* pull commands:
       'pr' -> PULLROWS
       'pc' -> PULLCOLS
       'pp' -> PULL (paste+pop)
       'pm' -> PULLMERGE
       'px' -> PULLXCHG
       'pt' -> PULLTP (transpose)
       'pf' -> PULLFMT
       'pC' -> PULLCOPY implemented above as copy(COPY_FROM_QBUF)
       'p.' -> does not get here, redirected to PULLCOPY
    // XXX: should have 'pv' -> PULLVALUES
     */

    if (cmd == 'r') {     /* PULLROWS */
        if (!insert_rows(sp, cr, numrows, 0))
            return;
        if ((fr = frange_find(sp, cr.row, cr.col))) {
            deltac = fr->orr.left.col - mincol;
        } else {
            if (db->rowfmt && db->nrows >= numrows) {
                // XXX: incorrect if destination range has more rows than src
                for (i = 0; i < numrows; i++) {
                    sp->rowfmt[cr.row + i] = db->rowfmt[i];
                }
            }
            deltac = 0;
        }
    } else
    if (cmd == 'c') {     /* PULLCOLS */
        if (!insert_cols(sp, cr, numcols, 0))
            return;
        if (db->colfmt && db->ncols >= numcols) {
            // XXX: incorrect if destination range has more columns than src
            for (i = 0; i < numcols; i++) {
                sp->colfmt[cr.col + i] = db->colfmt[i];
            }
        }
        deltar = 0;
    } else
    if (cmd == 'x') {     /* PULLXCHG */
        /* Save the original contents of the destination range on the
         * delete buffer stack in preparation for the exchange.
         */
        /* move cell range to temporary subsheet */
        kill_area(sp, DELBUF_TMP1, rangeref(minrow + deltar, mincol + deltac,
                                            maxrow + deltar, maxcol + deltac), KA_MOVE);
    } else
    if (cmd == 'p') {     /* PULL */
        // XXX: should just clear area (free cells) and sync the references
        kill_area(sp, DELBUF_TMP1, rangeref(minrow + deltar, mincol + deltac,
                                            maxrow + deltar, maxcol + deltac), KA_MOVE | KA_NO_FMT);
        sync_refs(sp);
        delbuf_free(DELBUF_TMP1);
    } else
    if (cmd == 't') {     /* PULLTP (transpose) */
        // XXX: named ranges and range references may not be updated properly
        // XXX: should just clear area (free cells) and sync the references
        kill_area(sp, DELBUF_TMP1, rangeref(minrow + deltar, mincol + deltac,
                                            minrow + deltar + maxcol - mincol,
                                            mincol + deltac + maxrow - minrow), KA_MOVE | KA_NO_FMT);
        sync_refs(sp);
        delbuf_free(DELBUF_TMP1);
    }
    if (cmd != 'x') {     /* PULLXCHG */
        /* At this point, we copy the cells from the delete buffer into the
         * destination range.
         */
        for (r = 0; r < db->nrows; r++) {
            for (c = 0; c < db->ncols; c++) {
                p = db->tbl[r].cp[c];
                if (p) {
                    // XXX: should pass coordinates to ent_copy
                    if (cmd == 't') {   /* Transpose rows and columns while pulling. */
                        n = lookat(sp, minrow + deltar + c, mincol + deltac + r);
                    } else {
                        n = lookat(sp, minrow + deltar + r, mincol + deltac + c);
                    }
                    if (n)
                        ent_copy(sp, n, p, deltar, deltac, minrow, mincol, maxrow, maxcol, cmd);
                }
            }
        }
    }
    /* Now exchange them so that the original cells from the delete buffer
     * are in the destination range instead of the copies.  When doing a
     * "pull exchange" ("px" or "pullxchg"), exchange the original contents
     * of the destination range with the contents of the delete buffer
     * instead.  Don't do this if transposing or merging (including merging
     * cell formats), or if the expressions in the destination cells have
     * been adjusted during a copy.
     */
    if (cmd != 't' && cmd != 'm' && cmd != 'f') {
        /* cmd is in PULL, PULLROWS, PULLCOLS, PULLXCHG */
        if (cmd != 'x') {     /* PULLXCHG */
            /* cmd is in PULL, PULLROWS, PULLCOLS */
            /* allocate cell pointer matrix in TMP1 */
            kill_area(sp, DELBUF_TMP1, rangeref(minrow + deltar, mincol + deltac,
                                                maxrow + deltar, maxcol + deltac), KA_NO_FMT);
            // XXX: db1->colfmt and db1->rowfmt are NULL
            // XXX: move the copied cells from destination range to temporary subsheet? @@@
        }
        /* move the cells from delbuf to destination */
        for (r = 0; r < db->nrows; r++) {
            for (c = 0; c < db->ncols; c++) {
                p = db->tbl[r].cp[c];
                if (p) {
                    db->tbl[r].cp[c] = NULL;
                    // XXX: what if destination is locked ?
                    row = db->minrow + r + deltar;
                    col = db->mincol + c + deltac;
                    p->flags &= ~IS_DELETED;
                    setcell(sp, row, col, p);
                }
            }
        }
        /* Replace the pulled cells in the source register with the
         * new set of cells.
         */
        db1 = delbuf[DELBUF_TMP1];
        {
            SCXMEM rowptr_t *tbl = db1->tbl;
            db1->tbl = db->tbl;
            db->tbl = tbl;
        }
        delbuf_free(DELBUF_TMP1);
    }
    sync_refs(sp);
    FullUpdate++;
    sp->modflg++;
}

void cmd_pullcells(sheet_t *sp, int cmd, int uarg) {
    while (uarg--) {
        // XXX: repeating pullcells makes sense for
        //      'pc' and 'pr' insert multiple copies
        //      'px' and 'pt' for performance tests
        //      'pp' to paste/pop multiple levels?
        //      'pc' and 'pr' should handle ranges specifically
        //      so multiple elements are inside range boundaries
        pullcells(sp, qbuf, cmd, cellref_current(sp));
    }
    qbuf = 0;
}

/* delete group of columns (1 or more) */
void delete_cols(sheet_t *sp, int c1, int c2) {
    int r, c, ncols, save = sp->curcol;
    colfmt_t def_colfmt = { FALSE, DEFWIDTH, DEFPREC, DEFREFMT };
    adjust_ctx_t adjust_ctx;

    if (c1 > c2) SWAPINT(c1, c2);
    if (c2 > sp->maxcol)
        c2 = sp->maxcol;
    if (c1 > sp->maxcol) {
        /* deleting columns beyond the active area: nothing to do */
        return;
    }

    if (any_locked_cells(sp, rangeref(0, c1, sp->maxrow, c2)))
        return;

    ncols = c2 - c1 + 1;
    // XXX: probably useless and counterproductive
    //      this is for frange_get_current(sp) which is questionable
    sp->curcol = c1;

    delbuf_free(DELBUF_9);
    delbuf_free(qbuf);
    sync_refs(sp);
    /* move cell range to subsheet delbuf[0] */
    kill_area(sp, DELBUF_DEF, rangeref(0, c1, sp->maxrow, c2), KA_MOVE);
    // XXX: should use sync_refs() to flag invalid references
    fix_ranges(sp, -1, c1, -1, c2, -1, -1, frange_get_current(sp));
    delbuf_copy(DELBUF_DEF, qbuf);
    qbuf = 0;
    delbuf_rotate(DELBUF_1, DELBUF_9);
    delbuf_copy(DELBUF_DEF, DELBUF_1);
    delbuf_unsync(DELBUF_DEF);

    /* clear then copy the block left */
    for (r = 0; r <= sp->maxrow; r++) {
        for (c = c1; c <= sp->maxcol - ncols; c++) {
            // XXX: should factorize as movecell()
            struct ent **pp = ATBL(sp, r, c);
            if ((*pp = pp[ncols])) {
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

    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(0, c1, sp->maxrow, c2);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = c1;
    adjust_ctx.move_rr = rangeref(0, c2 + 1, sp->maxrow, sp->maxcol);
    adjust_ctx.move_dr = 0;
    adjust_ctx.move_dc = -ncols;
    adjust_refs(&adjust_ctx);

    sp->maxcol -= ncols;

    FullUpdate++;
    sp->modflg++;
    sp->curcol = save < c1 ? save : (save <= c2) ? c1 : save - ncols;
}

/* Modified 9/17/90 THA to handle more formats */
void cmd_format(sheet_t *sp, int c1, int c2, int w, int p, int r) {
    int i;

    if (c1 > c2) SWAPINT(c1, c2);

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

    if (checkbounds(sp, 0, c2) < 0) {
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

// XXX: get rid of this static mess
static rangeref_t copy_rr;

void copy_set_source_range(rangeref_t rr) {
    copy_rr = rr;
}

/* using temporary subsheets DELBUF_TMP1 and DELBUF_TMP2 */
void copy_range(sheet_t *sp, int flags, rangeref_t drr, rangeref_t srr) {
    struct ent *p;
    int mindr, mindc, maxdr, maxdc;
    int minsr, minsc, maxsr, maxsc;
    int dr, dc, sr, sc;
    subsheet_t *db;

    range_normalize(&drr);
    mindr = drr.left.row;
    mindc = drr.left.col;
    maxdr = drr.right.row;
    maxdc = drr.right.col;

    if (flags & COPY_FROM_RANGE) {
        range_normalize(&srr);
        minsr = srr.left.row;
        minsc = srr.left.col;
        maxsr = srr.right.row;
        maxsc = srr.right.col;
    } else
    if (flags & COPY_FROM_QBUF) {
        db = delbuf[qbuf];
        if (!db || !db->nrows || !db->ncols) {
            // nothing to copy
            return;
        }
        minsr = db->minrow;
        minsc = db->mincol;
        maxsr = db->maxrow;
        maxsc = db->maxcol;
    } else
    if (flags & COPY_FROM_DEF) {
        // XXX: use static values: check if we can avoid it
        minsr = copy_rr.left.row;
        minsc = copy_rr.left.col;
        maxsr = copy_rr.right.row;
        maxsc = copy_rr.right.col;
    } else {
        return;
    }

    if (minsr < 0 || minsc < 0)
        return;

    /* compute actual destination range */
    if (maxdr - mindr < maxsr - minsr) {
        if (maxdr == mindr)
            maxdr = mindr + (maxsr - minsr);
        else
            maxsr = minsr + (maxdr - mindr);
    }
    if (maxdc - mindc < maxsc - minsc) {
        if (maxdc == mindc)
            maxdc = mindc + (maxsc - minsc);
        else
            maxsc = minsc + (maxdc - mindc);
    }

    /* fail if destination area exceeds maximum boundaries */
    if (checkbounds(sp, maxdr, maxdc) < 0) {
        error("cannot copy beyond maximum boundaries");
        return;
    }

    if (any_locked_cells(sp, rangeref(mindr, mindc, maxdr, maxdc)))
        return;

    if (flags & COPY_FROM_QBUF) {
        delbuf_copy(qbuf, DELBUF_TMP1);
    } else {
        // XXX: should not be necessary if copy order is well chosen
        //      or if source and destination do not overlap
        yank_area(sp, DELBUF_TMP1, rangeref(minsr, minsc, maxsr, maxsc));
    }

    /* move cell range to other temporary subsheet */
    kill_area(sp, DELBUF_TMP2, rangeref(mindr, mindc, maxdr, maxdc), KA_MOVE | KA_NO_FMT);
    sync_refs(sp);
    delbuf_free(DELBUF_TMP2);

    error("Copying...");
    if (!loading)
        screen_refresh();

    /* copy source range, replicating to fill destination range */
    db = delbuf[DELBUF_TMP1];
    for (dr = mindr; dr <= maxdr; dr += maxsr - minsr + 1) {
        for (dc = mindc; dc <= maxdc; dc += maxsc - minsc + 1) {
            int deltar = dr - minsr;
            int deltac = dc - minsc;
            for (sr = minsr; sr <= maxsr; sr++) {
                for (sc = minsc; sc <= maxsc; sc++) {
                    p = db->tbl[sr].cp[sc];
                    if (p) {
                        int vr = sr + deltar;
                        int vc = sc + deltac;
                        if (vr <= maxdr && vc <= maxdc) {
                            // XXX: should pass coordinates to ent_copy
                            struct ent *n = lookat(sp, vr, vc);
                            if (n)
                                ent_copy(sp, n, p, deltar, deltac, 0, 0, sp->maxrow, sp->maxcol, 0);
                        }
                    }
                }
            }
        }
    }

    if (flags & COPY_FROM_QBUF) {
        delbuf_free(DELBUF_TMP1);
        qbuf = 0;
    } else {
        sync_refs(sp);
        delbuf_free(DELBUF_TMP1);
    }

    error("Copy done.");
}

/* ERASE a Range of cells */
void erase_range(sheet_t *sp, rangeref_t rr) {
    delbuf_free(DELBUF_9);
    delbuf_free(qbuf);
    // XXX: missing sync_refs() ???
    /* move cell range to subsheet delbuf[0] */
    kill_area(sp, DELBUF_DEF, rr, KA_MOVE);
    sync_refs(sp);
    delbuf_copy(DELBUF_DEF, qbuf);
    qbuf = 0;
    delbuf_rotate(DELBUF_1, DELBUF_9);
    delbuf_copy(DELBUF_DEF, DELBUF_1);
    // XXX: missing delbuf_unsync(DELBUF_DEF); ?

    FullUpdate++;
    sp->modflg++;
}

/* YANK a Range of cells */
void yank_range(sheet_t *sp, rangeref_t rr) {
    /* copy source cells to delbuf[0] */
    yank_area(sp, DELBUF_DEF, rr);
    /* set yanked data in named buffer '0' or qbuf if any */
    delbuf_copy(DELBUF_DEF, qbuf ? qbuf : DELBUF_0);
    qbuf = 0;
}

/* MOVE a Range of cells */
void move_range(sheet_t *sp, cellref_t cr, rangeref_t rr) {
    move_area(sp, cr.row, cr.col, rr);
    sync_refs(sp);
    FullUpdate++;
}

/* fill a range with constants */
void fill_range(sheet_t *sp, rangeref_t rr, double start, double inc, int bycols) {
    int r, c;
    int dr = bycols == 0;
    int dc = bycols != 0;

    range_normalize(&rr);
    if (any_locked_cells(sp, rr))
        return;

    // XXX: should use set_cell_value with an scvalue_t
    for (c = rr.left.col, r = rr.left.row;;) {
        struct ent *p = lookat(sp, r, c);
        if (!p)
            break;
        if (p->type == SC_STRING) {
            string_set(&p->label, NULL); /* free the previous label */
        }
        p->type = SC_NUMBER;
        p->cellerror = 0;
        p->flags |= IS_CHANGED;
        p->v = start;
        start += inc;

        if ((c += dc) > rr.right.col) {
            c = rr.left.col;
            if (++r > rr.right.row)
                break;
        } else
        if ((r += dr) > rr.right.row) {
            r = rr.left.row;
            if (++c > rr.right.col)
                break;
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
    if (any_locked_cells(sp, rr))
        return;

    // XXX: should use set_cell_flags() with mask and bits
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = lookat(sp, r, c);
            if (p) {
                // XXX: update IS_CHANGED?
                p->flags |= IS_LOCKED;
            }
        }
    }
    sp->modflg++;
}

/* unlock a range of cells */
void unlock_cells(sheet_t *sp, rangeref_t rr) {
    int r, c;

    range_normalize(&rr);
    if (any_locked_cells(sp, rr))
        return;

    // XXX: should use set_cell_flags() with mask and bits
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p) {
                // XXX: update IS_CHANGED?
                p->flags &= ~IS_LOCKED;
            }
        }
    }
    sp->modflg++;
}

void format_cells(sheet_t *sp, rangeref_t rr, SCXMEM string_t *str) {
    int r, c;

    if (str && !*s2c(str)) {
        string_free(str);
        str = NULL;
    }

    range_normalize(&rr);
    if (any_locked_cells(sp, rr))
        return;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            if (str) {
                struct ent *p = lookat(sp, r, c);
                if (p) {
                    string_set(&p->format, string_dup(str));
                    p->flags |= IS_CHANGED;
                }
            } else {
                struct ent *p = getcell(sp, r, c);
                if (p && p->format) {
                    string_set(&p->format, NULL);
                    p->flags |= IS_CHANGED;
                }
            }
        }
    }
    string_free(str);
    FullUpdate++;
    sp->modflg++;
}

/*
 * sync_refs and sync_expr are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the free_ent calls in sc.c
 */
// XXX: fix this mess
#if 0
static void sync_expr(sheet_t *sp, enode_t *e) {
    if (e == NULL)
        return;

    switch (e->type) {
    case OP_TYPE_RANGE:
        // XXX: should use same logic as for OP_TYPE_VAR
        //e->e.r.left.vp = lookat(sp, e->e.r.left.vp->row, e->e.r.left.vp->col);
        //e->e.r.right.vp = lookat(sp, e->e.r.right.vp->row, e->e.r.right.vp->col);
        break;
    case OP_TYPE_VAR:
        if (e->e.v.vp->flags & IS_CLEARED) {
            /* cell was moved to the free list */
            e->op = OP__ERROR;
            e->type = OP_TYPE_ERROR;
            e->e.error = ERROR_REF;
        } else
        if (e->e.v.vp->flags & MAY_SYNC) {
            //e->e.v.vp = lookat(sp, e->e.v.vp->row, e->e.v.vp->col);
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

// XXX: this duplicates sync_expr() ?
/* make cell references point to current sheet cells */
static void sync_enode(sheet_t *sp, struct enode *e) {
    if (e) {
        if (e->type == OP_TYPE_RANGE) {
            //e->e.r.left.vp = lookat(sp, e->e.r.left.vp->row, e->e.r.left.vp->col);
            //e->e.r.right.vp = lookat(sp, e->e.r.right.vp->row, e->e.r.right.vp->col);
        } else
        if (e->type == OP_TYPE_VAR) {
            //e->e.v.vp = lookat(sp, e->e.v.vp->row, e->e.v.vp->col);
        } else
        if (e->type == OP_TYPE_FUNC) {
            int i;
            for (i = 0; i < e->nargs; i++)
                sync_enode(sp, e->e.args[i]);
        }
    }
}

static void sync_ranges(sheet_t *sp) {
    int row, col;
    struct ent *p;

    // XXX: should do this for delbuf cells too?
    for (row = 0; row <= sp->maxrow; row++) {
        for (col = 0; col <= sp->maxcol; col++) {
            if ((p = getcell(sp, row, col)) && p->expr)
                sync_enode(sp, p->expr);
        }
    }
}

static void sync_refs(sheet_t *sp) {
    int i, row, col;
    struct ent *p;

    sync_ranges(sp);

    // XXX: sync_ranges() already does sync_enode(p->expr)
    for (row = 0; row <= sp->maxrow; row++) {
        for (col = 0; col <= sp->maxcol; col++) {
            if ((p = getcell(sp, row, col)) && p->expr)
                sync_expr(sp, p->expr);
        }
    }
    for (i = 0; i < DELBUF_COUNT; i++) {
        for (p = delbuf_array[i].ptr; p; p = p->next) {
            if (p->expr)
                sync_expr(sp, p->expr);
        }
    }
}

static void enode_fix(sheet_t *sp, struct enode *e,
                      int row1, int col1, int row2, int col2,
                      int delta1, int delta2, struct frange *fr)
{
    /* Fix range references: single cells are not changed.
       top-left cell is set to col2/row2 - delta1
       bottom-right cell is set to col1/row1 + delta2
     */
    if (e) {
        // XXX: why not fix OP_TYPE_VAR nodes?
        if (e->type == OP_TYPE_RANGE) {
            int r1 = e->e.r.left.vp->row;
            int c1 = e->e.r.left.vp->col;
            int r2 = e->e.r.right.vp->row;
            int c2 = e->e.r.right.vp->col;
            if (r1 > r2) SWAPINT(r1, r2);
            if (c1 > c2) SWAPINT(c1, c2);

            if (!fr || (c1 >= fr->orr.left.col && c1 <= fr->orr.right.col)) {
                // XXX: why special case r1==r2 or c1==c2 ?
                if (r1 != r2 && r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
                if (c1 != c2 && c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
            }

            if (!fr || (c2 >= fr->orr.left.col && c2 <= fr->orr.right.col)) {
                if (r1 != r2 && r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
                if (c1 != c2 && c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
            }
            // XXX: what if reference becomes invalid?
            //e->e.r.left.vp = lookat(sp, r1, c1);
            //e->e.r.right.vp = lookat(sp, r2, c2);
        } else
        if (e->type == OP_TYPE_FUNC) {
            int i;
            for (i = 0; i < e->nargs; i++)
                enode_fix(sp, e->e.args[i], row1, col1, row2, col2, delta1, delta2, fr);
        }
    }
}

static void fix_ranges(sheet_t *sp, int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr)
{
    int row, col;

    /* Next, we go through all valid cells with expressions and fix any ranges
     * that need fixing.
     */
    for (row = 0; row <= sp->maxrow; row++) {
        for (col = 0; col <= sp->maxcol; col++) {
            struct ent *p = getcell(sp, row, col);
            if (p && p->expr)
                enode_fix(sp, p->expr, row1, col1, row2, col2, delta1, delta2, fr);
        }
    }
}
#endif

void cell_adjust(adjust_ctx_t *ap, cellref_t *cp) {
    if (cell_in_range(*cp, ap->clamp_rr)) {
        if (ap->clamp_newr >= 0) cp->row = ap->clamp_newr;
        if (ap->clamp_newc >= 0) cp->col = ap->clamp_newc;
    } else
    if (cell_in_range(*cp, ap->move_rr)) {
        cp->row += ap->move_dr;
        cp->col += ap->move_dc;
    }
}

void range_adjust(adjust_ctx_t *ap, rangeref_t *rp) {
    if (cell_in_range(rp->left, ap->clamp_rr)) {
        if (ap->clamp_newr >= 0) rp->left.row = ap->clamp_newr;
        if (ap->clamp_newc >= 0) rp->left.col = ap->clamp_newc;
    } else
    if (cell_in_range(rp->left, ap->move_rr)) {
        rp->left.row += ap->move_dr;
        rp->left.col += ap->move_dc;
    }
    if (cell_in_range(rp->right, ap->clamp_rr)) {
        if (ap->clamp_newr >= 0) rp->right.row = ap->clamp_newr - 1;
        if (ap->clamp_newc >= 0) rp->right.col = ap->clamp_newc - 1;
    } else
    if (cell_in_range(rp->right, ap->move_rr)) {
        rp->right.row += ap->move_dr;
        rp->right.col += ap->move_dc;
    }
}

static void enode_adjust(adjust_ctx_t *ap, struct enode *e) {
    if (e == NULL)
        return;

    switch (e->type) {
    case OP_TYPE_RANGE:
        range_adjust(ap, &e->e.rr);
        break;
    case OP_TYPE_VAR:
        cell_adjust(ap, &e->e.cr);
        break;
    case OP_TYPE_FUNC: {
            int i;
            for (i = 0; i < e->nargs; i++)
                enode_adjust(ap, e->e.args[i]);
        }
        break;
    }
}

void adjust_refs(adjust_ctx_t *ap) {
    int i, row, col, r, c;
    struct ent *p;
    sheet_t *sp;

    /* Update all marked cells. */
    for (i = 0; i < MARK_COUNT; i++) {
        cell_adjust(ap, &ap->sp->savedcr[i]);
        cell_adjust(ap, &ap->sp->savedst[i]);
    }
    /* Update goto targets. */
    range_adjust(ap, &gs.g_rr);
    cell_adjust(ap, &gs.st);
    crange_adjust(ap);  /* Update color ranges. */
    nrange_adjust(ap);  /* Update named ranges. */
    frange_adjust(ap);  /* Update frames. */
    note_adjust(ap);    /* Update note targets. */

    sp = ap->sp;
    for (row = 0; row <= sp->maxrow; row++) {
        for (col = 0; col <= sp->maxcol; col++) {
            if ((p = getcell(sp, row, col)) && p->expr)
                enode_adjust(ap, p->expr);
        }
    }
    /* Enumerate delbuf_array with sheet number */
    for (i = 0; i < DELBUF_COUNT; i++) {
        subsheet_t *db = delbuf_array + i;
        if (!db->refs) continue;
        for (r = 0; r < db->nrows; r++) {
            for (c = 0; c < db->ncols; c++) {
                p = db->tbl[r].cp[c];
                if (p && p->expr)
                    enode_adjust(ap, p->expr);
            }
        }
    }
}

/*---------------- hide/show commands ----------------*/

/* mark rows as hidden */
void hiderows(sheet_t *sp, int r1, int r2) {
    int r;

    if (r1 > r2) SWAPINT(r1, r2);
    if (r1 < 0) {
        error("Invalid range");
        return;
    }
    if (checkbounds(sp, r2 + 1, 0) < 0) {
        // XXX: should remove this restriction
        error("You cannot hide the last row");
        return;
    }
    if (sp->maxrow < r2 + 1)
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
    if (checkbounds(sp, 0, c2 + 1) < 0) {
        // XXX: should remove this restriction
        error("You cannot hide the last column");
        return;
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
// XXX: check if maxrow/maxcol is OK for full range
static void ent_copy(sheet_t *sp, struct ent *n, struct ent *p, int dr, int dc,
                     int r1, int c1, int r2, int c2, int special)
{
    int hasvalue;

    if (!n || !p) {
        error("internal error");
        return;
    }
    hasvalue = p->type != SC_EMPTY || p->expr;
    if (special != 'f') {
        /* transfer value unless merging and no value */
        if (special != 'm' || hasvalue) {
            n->type = p->type;
            n->cellerror = p->cellerror;
            n->v = p->v;
            string_set(&n->label, string_dup(p->label));
            efree(n->expr);
            n->expr = copye(sp, p->expr, dr, dc, r1, c1, r2, c2, special == 't');
            n->flags |= IS_CHANGED;
        }
    }
    if (special == 'f' || hasvalue) {
        /* transfer alignment and LOCKED flag */
        n->flags &= ~(ALIGN_MASK | IS_LOCKED);
        n->flags |= p->flags & (ALIGN_MASK | IS_LOCKED);
        if (p->format) {
            string_set(&n->format, string_dup(p->format));
        } else
        if (special != 'm' && special != 'f') {
            // XXX: why not reset format if special == 'f' ?
            string_set(&n->format, NULL);
        }
        n->flags |= IS_CHANGED;
    }
}

/* erase the database (sheet data, etc.) */
void erasedb(sheet_t *sp) {
    int r, c;

    for (r = 0; r <= sp->maxrow; r++) {
        for (c = 0; c <= sp->maxcol; c++) {
            // XXX: should factorize as erasecell()
            struct ent **pp = ATBL(sp, r, c);
            struct ent *p = *pp;
            if (p) {
                *pp = NULL;
                efree(p->expr);
                p->expr = NULL;
                string_set(&p->label, NULL);
                string_set(&p->format, NULL);
                p->next = free_ents; /* save [struct ent] for reuse */
                free_ents = p;
            }
        }
    }
    /* free all sheet data */
    for (r = 0; r < sp->maxrows; r++) {
        scxfree(sp->tbl[r].cp);
    }
    scxfree(sp->tbl);
    scxfree(sp->rowfmt);
    scxfree(sp->colfmt);
    sp->tbl = NULL;
    sp->rowfmt = NULL;
    sp->colfmt = NULL;

    for (c = 0; c < COLFORMATS; c++) {
        string_set(&sp->colformat[c], NULL);
    }
    nrange_clean(sp);
    frange_clean(sp);
    crange_clean(sp);
    abbrev_clean(sp);
    note_clean(sp);

    string_set(&sp->mdir, NULL);
    string_set(&sp->autorun, NULL);
    for (c = 0; c < FKEYS; c++) {
        string_set(&sp->fkey[c], NULL);
    }
    /* unset all marks */
    for (c = 0; c < MARK_COUNT; c++) {
        sp->savedcr[c] = cellref(-1, -1);
        sp->savedst[c] = cellref(-1, -1);
    }
    qbuf = 0;

    sheet_init(sp);

    FullUpdate++;
}

/* Returns 1 if cell is locked, 0 otherwise */
int locked_cell(sheet_t *sp, int row, int col) {
    if (sp->protect) {
        struct ent *p = getcell(sp, row, col);
        if (p && (p->flags & IS_LOCKED)) {
            error("Cell %s is locked", cell_addr(sp, cellref(row, col)));
            return 1;
        }
    }
    return 0;
}

/* Check if area contains locked cells */
static int any_locked_cells(sheet_t *sp, rangeref_t rr) {
    if (sp->protect) {
        int r, c;
        struct ent *p;

        for (r = rr.left.row; r <= rr.right.row; r++) {
            for (c = rr.left.col; c <= rr.right.col; c++) {
                p = getcell(sp, r, c);
                if (p && (p->flags & IS_LOCKED)) {
                    error("Locked cells encountered. Nothing changed");
                    return 1;
                }
            }
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

void select_register(char c) {
    if (c >= '0' && c <= '9') {
        qbuf = DELBUF_0 + (c - '0');
    } else if (c >= 'a' && c <= 'z') {
        qbuf = DELBUF_A + (c - 'a');
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

/*---------------- cell annotations ----------------*/

int note_test(sheet_t *sp) {
    return sp->note_base != NULL;
}

/* unlink and free an annotation */
static void note_free(sheet_t *sp, SCXMEM struct note *a) {
    if (a) {
        if (a->next)
            a->next->prev = a->prev;
        else
            sp->note_tail = a->prev;
        if (a->prev)
            a->prev->next = a->next;
        else
            sp->note_base = a->next;
        string_free(a->str);
        scxfree(a);
    }
}

struct note *note_find(sheet_t *sp, cellref_t cr) {
    struct note *a;
    for (a = sp->note_base; a; a = a->next) {
        if (a->cr.row == cr.row && a->cr.col == cr.col)
            break;
    }
    return a;
}

void note_clean(sheet_t *sp) {
    struct note *a, *next;

    a = sp->note_base;
    sp->note_base = sp->note_tail = NULL;
    while (a) {
        next = a->next;
        string_free(a->str);
        scxfree(a);
        a = next;
    }
}

void note_add(sheet_t *sp, cellref_t cr, rangeref_t rr, SCXMEM string_t *str) {
    struct note *a, *next;
    // XXX: should use set_cell_format with mask and bits
    struct ent *p = lookat(sp, cr.row, cr.col);
    if (!p) {
        string_free(str);
        return;
    }
    a = note_find(sp, cr);
    if (!a) {
        a = scxmalloc(sizeof(*a));
        if (!a) {
            string_free(str);
            return;
        }
        a->str = NULL;
        a->cr = cr;
        /* link note at head of list */
        next = sp->note_base;
        sp->note_base = a;
        a->prev = NULL;
        a->next = next;
        if (next)
            next->prev = a;
        else
            sp->note_tail = a;
    }
    string_set(&a->str, str);
    range_normalize(&rr);
    a->rr = rr;
    p->flags |= HAS_NOTE | IS_CHANGED;
    FullUpdate++;
    sp->modflg++;
}

void note_delete(sheet_t *sp, cellref_t cr) {
    struct ent *p = getcell(sp, cr.row, cr.col);
    if (p && (p->flags & HAS_NOTE)) {
        p->flags ^= HAS_NOTE;
        p->flags |= IS_CHANGED;
        sp->modflg++;
        note_free(sp, note_find(sp, cr));
    }
}

void note_adjust(adjust_ctx_t *ap) {
    struct note *a;

    for (a = ap->sp->note_base; a; a = a->next) {
        cell_adjust(ap, &a->cr);
        range_adjust(ap, &a->rr);
    }
}

void note_write(sheet_t *sp, FILE *f) {
    struct note *a;

    for (a = sp->note_tail; a; a = a->prev) {
        // XXX: should clip write range
        if (a->str) {
            // XXX: should quote note string
            fprintf(f, "addnote %s \"%s\"\n",
                    cell_addr(sp, a->cr), s2c(a->str));
        } else {
            fprintf(f, "addnote %s %s\n",
                    cell_addr(sp, a->cr), range_addr(sp, a->rr));
        }
    }
}

/*---------------- range sorting ----------------*/

struct sortcrit {
    int direction, type, column;
};

typedef struct sort_context {
    sheet_t *sp;
    rangeref_t rr;
    SCXMEM struct sortcrit *crit;
    int ncrit;
} sort_ctx_t;

static sort_ctx_t *sort_context;

static int compare(const void *a1, const void *a2) {
    sort_ctx_t *sc = sort_context;
    int row1 = *(const int *)a1;
    int row2 = *(const int *)a2;
    struct ent *p1;
    struct ent *p2;
    int result = 0;
    int i;

    for (i = 0; !result; i++) {
        if (i >= sc->ncrit) {
            /* stable sort: rows that compare equal stay in the same order */
            return (row1 > row2) - (row1 < row2);
        }
        p1 = getcell(sc->sp, row1, sc->crit[i].column);
        p2 = getcell(sc->sp, row2, sc->crit[i].column);

        // XXX: comparison algorithm should be the same as for expressions
        /* mixed types are sorted in this order:
           numbers < text < logical < error < empty
         */
        /* empty cells always compare larger */
        if (!p1 || p1->type == SC_EMPTY) {
            result = (p2 || p2->type == SC_EMPTY) ? 1 : 0;
            continue;
        }
        if (!p2 || p2->type == SC_EMPTY) {
            result = -1;
            continue;
        }
        // XXX: ignore sc->crit[i].type
        if (p1->type == SC_NUMBER) {
            if (p2->type == SC_NUMBER)
                result = (p1->v > p2->v) - (p1->v < p2->v);
            else
                result = -1;
        } else
        if (p2->type == SC_NUMBER) {
            result = 1;
        } else
        if (p1->type == SC_STRING) {
            if (p2->type == SC_STRING)
                result = strcmp(s2c(p1->label), s2c(p2->label));
            else
                result = -1;
        } else
        if (p2->type == SC_STRING) {
            result = 1;
        } else
        if (p1->type == SC_BOOLEAN) {
            if (p2->type == SC_BOOLEAN)
                result = p2->v - p1->v;
            else
                result = -1;
        } else
        if (p2->type == SC_BOOLEAN) {
            result = 1;
        } else {
            result = p2->cellerror - p1->cellerror;
        }
        result *= sc->crit[i].direction;
    }
    return result;
}

/* uses temporary subsheet DELBUF_TMP1 */
void sort_range(sheet_t *sp, rangeref_t rr, SCXMEM string_t *criteria) {
    sort_ctx_t sc[1];
    struct sortcrit *crit;
    SCXMEM int *rows;
    SCXMEM int *destrows;
    int i, r, c, nrows, row, col, len;
    const char *cp;
    struct ent *p, *n;
    subsheet_t *db;

    range_normalize(&rr);
    sc->sp = sp;
    sc->rr = rr;
    nrows = rr.right.row - rr.left.row + 1;
    if (nrows <= 1)
        goto done;

    if (any_locked_cells(sp, rr))
        goto done;

    /* allocate sort criteria: at type 0, type 1 col=minc */
    sc->crit = scxmalloc(sizeof(*sc->crit) * 2);
    /* allocate array of row numbers that will be sorted */
    rows = scxmalloc(sizeof(*rows) * nrows);
    destrows = scxmalloc(sizeof(*rows) * nrows);
    if (!sc->crit || !rows || !destrows)
        goto fail;

    for (i = 0; i < nrows; i++)
        rows[i] = destrows[i] = rr.left.row + i;

    if (sempty(criteria)) {
        sc->crit[0].direction = 1;
        sc->crit[0].type = 1;
        sc->crit[0].column = rr.left.col;
        sc->crit[1].direction = 1;
        sc->crit[1].type = 0;
        sc->crit[1].column = rr.left.col;
        sc->ncrit = 2;
    } else {
        for (sc->ncrit = 0, cp = s2c(criteria); *cp; sc->ncrit++) {
            if (sc->ncrit > 1) {
                /* allocate new sorting criterion */
                void *new_crit = scxrealloc(sc->crit, (sc->ncrit + 1) * (sizeof(*sc->crit)));
                if (!new_crit)
                    goto fail;
                sc->crit = new_crit;
            }
            crit = &sc->crit[sc->ncrit];
            switch (*cp++) {
            case '+':
                crit->direction = 1;
                break;
            case '-':
                crit->direction = -1;
                break;
            default:
                crit->direction = 1;
                cp--;
            }
            switch (*cp++) {
            case '#':
                crit->type = 0;
                break;
            case '$':
                crit->type = 1;
                break;
            default:
                crit->type = 0;
                cp--;
            }
            if ((col = atocol(cp, &len)) >= 0) {
                cp += len;
                crit->column = col;
                if (col >= sc->rr.left.col && col <= sc->rr.right.col)
                    continue;
            }
            error("Invalid sort criteria");
            goto fail;
        }
    }

    /* we should use qsort_r() but it might not be available */
    sort_context = sc;
    qsort(rows, nrows, sizeof(*rows), compare);
    sort_context = NULL;
    for (i = 0; i < nrows; i++)
        destrows[rows[i] - rr.left.row] = rr.left.row + i;

    kill_area(sp, DELBUF_TMP1, rr, KA_MOVE | KA_NO_FMT);
    sync_ranges(sp);

    db = delbuf[DELBUF_TMP1];
    for (r = 0; r < db->nrows; r++) {
        for (c = 0; c < db->ncols; c++) {
            p = db->tbl[r].cp[c];
            if (p) {
                row = destrows[r];
                // XXX: should just fixup cell references and move the cell?
                //setcell(sp, row, c, p);
                // XXX: Should copy cell values, expressions and formats.
                // XXX: but leave borders unchanged.
                if (row == rr.left.row + r) {
                    db->tbl[r].cp[c] = NULL;
                    setcell(sp, row, c + rr.left.col, p);
                } else {
                    // XXX: should pass coordinates to ent_copy
                    n = lookat(sp, row, c + rr.left.col);
                    if (n) {
                        ent_copy(sp, n, p, row - (r + rr.left.row), 0, 0, 0, sp->maxrow, sp->maxcol, 0);
                    }
                }
            }
        }
    }
    delbuf_free(DELBUF_TMP1);

    /* Do not adjust range references. */

fail:
    scxfree(sc->crit);
    scxfree(rows);
    scxfree(destrows);
done:
    string_free(criteria);
}
