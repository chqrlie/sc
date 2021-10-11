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
    SCXMEM struct ent *ptr;  /* list of allocated cells */
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

#define ATBL(sp, row, col)     (&(sp)->tbl[row][col])

static int any_locked_cells(sheet_t *sp, int r1, int c1, int r2, int c2);
static void move_area(sheet_t *sp, int dr, int dc, rangeref_t rr);
static void yank_area(sheet_t *sp, int idx, rangeref_t rr);
#define KA_DEFAULT      0
#define KA_IGNORE_LOCK  1
#define KA_NO_FMT       2
#define KA_COPY         4
static void kill_area(sheet_t *sp, int idx, int sr, int sc, int er, int ec, int flags);
static void fix_ranges(sheet_t *sp, int row1, int col1, int row2, int col2,
                       int delta1, int delta2, struct frange *fr);
static void sync_refs(sheet_t *sp);
static void ent_copy(sheet_t *sp, struct ent *n, struct ent *p,
                     int dr, int dc, int r1, int c1, int r2, int c2, int transpose);

static void ent_clear(struct ent *p) {
    if (p) {
        string_set(&p->label, NULL);
        efree(p->expr);
        p->expr = NULL;
        string_set(&p->format, NULL);
        p->v = 0.0;
        p->cellerror = 0;
        p->type = SC_EMPTY;
        p->flags = IS_CHANGED | IS_CLEARED;
        // XXX: should clear other fields?
        //      next
        FullUpdate++;  // XXX: really?
        changed++;
        //sp->modflg++;
    }
}

static void ent_free(struct ent *p) {
    if (p) {
        ent_clear(p);
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
        db->ptr = NULL;
        db->colfmt = NULL;
        db->rowfmt = NULL;
    }
}

/* unlink subsheet data, preserve duplicates if any */
static void delbuf_free(int idx) {
    subsheet_t *db;
    struct ent *p, *next;

    if (idx < 0 || !(db = delbuf[idx]))
        return;

    delbuf[idx] = NULL;
    if (--db->refs == 0) {
        /* free subsheet data if no duplicate found */
        for (p = db->ptr; p; p = next) {
            // XXX: entries are added to free_ents in reverse order
            //      but they were added to the delbuf in reverse order too
            next = p->next;
            ent_free(p);
        }
        db->ptr = NULL;
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

static void delbuf_unsync(int idx) {
    subsheet_t *db = delbuf[idx];
    if (db) {
        struct ent *p;
        for (p = db->ptr; p; p = p->next)
            p->flags &= ~MAY_SYNC;
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

/* move a cell to the delbuf list (reverse order) */
static void killcell(sheet_t *sp, int row, int col,
                     subsheet_t *db, int ignorelock, int unlock)
{
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol) {
        struct ent **pp = ATBL(sp, row, col);
        struct ent *p;
        if ((p = *pp) && (!(p->flags & IS_LOCKED) || ignorelock)) {
            p->flags |= IS_DELETED;
            if (unlock)
                p->flags &= ~IS_LOCKED;
            *pp = NULL;
            if (db) {
                p->next = db->ptr;
                db->ptr = p;
                // XXX: should update subsheet range?
            } else {
                ent_free(p);
            }
        }
    }
}

static int setcell(sheet_t *sp, int row, int col, struct ent *p) {
    if (row >= 0 && row <= sp->maxrow && col >= 0 && col <= sp->maxcol) {
        struct ent **pp = ATBL(sp, row, col);
        if (*pp && *pp != p) {
            ent_free(*pp);
        }
        *pp = p;
        return 1;
    }
    return 0;
}

static struct ent *ent_alloc(sheet_t *sp, int row, int col) {
    struct ent *p;
    if ((p = free_ents) != NULL) {
        free_ents = p->next;
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
    p->next = NULL;
    return p;
}

/* return a pointer to a cell's [struct ent *], creating if needed */
struct ent *lookat(sheet_t *sp, int row, int col) {
    struct ent **pp;

    checkbounds(sp, &row, &col);
    pp = ATBL(sp, row, col);
    if (*pp == NULL) {
        *pp = ent_alloc(sp, row, col);
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
        struct ent *p = getcell(sp, row, col);
        if (p) {
            struct ent *n = lookat(sp, row + 1, col);
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
    // XXX: should use copy_area(0, col + 1, maxrow, col + 1,
    //                           0, col, maxrow, col)
    for (row = 0; row <= sp->maxrow; row++) {
        struct ent *p = getcell(sp, row, col);
        if (p) {
            struct ent *n = lookat(sp, row, col + 1);
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
    int r, c, lim;
    rangeref_t rr = rangeref(cr.row + delta, 0, sp->maxrow, sp->maxcol);
    adjust_ctx_t adjust_ctx;
    struct frange *fr;

    // XXX: no check for locked cells?

    if ((sp->maxrow + arg >= sp->maxrows) && !growtbl(sp, GROWROW, sp->maxrow + arg, 0))
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
        if (!delta && fr->irr.left.row == cr.row + arg)
            fr->ir_left = lookat(sp, fr->irr.left.row - arg, fr->irr.left.col);
        if (delta && fr->irr.right.row == cr.row)
            fr->ir_right = lookat(sp, fr->irr.right.row + arg, fr->irr.right.col);
#endif
    } else {
        /*
         * save the last active row+1, shift the rows downward, put the last
         * row in place of the first
         */
        // XXX: this seems bogus, should implement a rotation
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
    }
    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(-1, -1, -1, -1);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = -1;
    adjust_ctx.move_rr = rr;
    adjust_ctx.move_dr = arg;
    adjust_ctx.move_dc = 0;
    adjust_ctx.destrows = NULL;
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

    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(-1, -1, -1, -1);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = -1;
    adjust_ctx.move_rr = rangeref(0, sc1, sp->maxrow, sc2);
    adjust_ctx.move_dr = 0;
    adjust_ctx.move_dc = arg;
    adjust_ctx.destrows = NULL;
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
    if (any_locked_cells(sp, r1, c1, r2, c2)) {
        error("Locked cells encountered. Nothing changed");
        return;
    }
    delbuf_free(DELBUF_9);
    delbuf_free(qbuf);
    sync_refs(sp);
    kill_area(sp, DELBUF_DEF, r1, c1, r2, c2, KA_DEFAULT);
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
        int r, c, dr;
        rowfmt_t def_rowfmt = { FALSE };

        /* reset row formats in target range */
        for (r = r1; r <= r2; r++) {
            sp->rowfmt[r] = def_rowfmt;
        }

        /* rotate row pointers and row formats */
        for (r = r2 + 1, dr = r1; r <= sp->maxrow; r++, dr++) {
            /* swap rows and fix cell entries */
            struct ent **tmprow = sp->tbl[dr];
            rowfmt_t tmpfmt = sp->rowfmt[dr];
            sp->rowfmt[dr] = sp->rowfmt[r];
            sp->tbl[dr] = sp->tbl[r];
            sp->tbl[r] = tmprow;
            sp->rowfmt[r] = tmpfmt;
            for (c = 0; c < sp->maxcols; c++) {
                struct ent *p = getcell(sp, r, c);
                if (p)
                    p->row = r;
            }
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
    adjust_ctx.destrows = NULL;
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

/* ignorelock is used when sorting so that locked cells can still be sorted */

/* move cell range to subsheet delbuf[idx] */
// XXX: should not modify sheet boundaries
static void kill_area(sheet_t *sp, int idx, int sr, int sc, int er, int ec, int flags) {
    subsheet_t *db;
    int r, c;

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
                struct ent *p;
                if ((p = *pp) && (!(p->flags & IS_LOCKED) || (flags & KA_IGNORE_LOCK))) {
                    p->flags |= IS_DELETED;
                    *pp = NULL;
                    ent_free(p);
                }
            }
        }
        return;
    }

    /* Do a lookat() for the upper left and lower right cells of the range
     * being erased to make sure they are included in the delete buffer so
     * that pulling cells always works correctly even if the cells at one
     * or more edges of the range are all empty.
     */
    lookat(sp, sr, sc);
    lookat(sp, er, ec);

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
    if (!(flags & KA_NO_FMT)) {
        db->colfmt = scxmalloc(db->ncols * sizeof(colfmt_t));
        for (c = sc; c <= ec; c++) {
            db->colfmt[c - sc] = sp->colfmt[c];
        }
        db->rowfmt = scxmalloc(db->nrows * sizeof(rowfmt_t));
        for (r = sr; r <= er; r++) {
            db->rowfmt[r - sr] = sp->rowfmt[r];
        }
    }
    if (flags & KA_COPY) {
        for (r = sr; r <= er; r++) {
            for (c = sc; c <= ec; c++) {
                struct ent *p = getcell(sp, r, c);
                if (p) {
                    struct ent *n = ent_alloc(sp, r, c);
                    ent_copy(sp, n, p, 0, 0, 0, 0, sp->maxrow, sp->maxcol, 0);
                    n->next = db->ptr;
                    db->ptr = n;
                }
            }
        }
    } else {
        for (r = sr; r <= er; r++) {
            for (c = sc; c <= ec; c++) {
                killcell(sp, r, c, db, flags & KA_IGNORE_LOCK, 0);
            }
        }
    }
}

/* copy a range of cells to delbuf[idx] */
static void yank_area(sheet_t *sp, int idx, rangeref_t rr) {
    range_clip(sp, range_normalize(&rr));
    /* copy cell range to subsheet delbuf[idx] */
    kill_area(sp, idx, rr.left.row, rr.left.col, rr.right.row, rr.right.col, KA_COPY);
}

/* uses temporary subsheet DELBUF_TMP1 */
static void move_area(sheet_t *sp, int dr, int dc, rangeref_t rr) {
    struct ent *p, *next;
    subsheet_t *db;
    int deltar, deltac;

    range_clip(sp, range_normalize(&rr));

    /* First we erase the source range, which puts the cells on the delete
     * buffer stack.
     */
    /* move cell range to temporary subsheet */
    kill_area(sp, DELBUF_TMP1, rr.left.row, rr.left.col, rr.right.row, rr.right.col, KA_DEFAULT);

    deltar = dr - rr.left.row;
    deltac = dc - rr.left.col;

    /* Now we erase the destination range. We then move the original source
     * range from the stack to the destination range, adjusting the addresses
     * as we go, leaving the stack in its original state.
     */
    /* discard cell range to black hole */
    kill_area(sp, -1, dr, dc, rr.right.row + deltar, rr.right.col + deltac, KA_NO_FMT);
    // XXX: why not sync_refs() ??? most likely a bug
    // XXX: if moving entire columns or rows, the column or row flags should be copied
    // XXX: this could be addressed with flags

    db = delbuf[DELBUF_TMP1];
    if (db) {
        for (p = db->ptr; p; p = next) {
            next = p->next;
            p->row += deltar;
            p->col += deltac;
            p->flags &= ~IS_DELETED;
            setcell(sp, p->row, p->col, p);
        }
        db->ptr = NULL;
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
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p && p->expr) {
                if (p->flags & IS_LOCKED) {
                    error(" Cell %s is locked", cell_addr(sp, cellref(r, c)));
                    continue;
                }
                efree(p->expr);
                p->expr = NULL;
            }
        }
    }
    sp->modflg++;  // XXX: should be done only upon modification
}

/* copy/move cell range from subsheet delbuf[src]
   using temporary subsheets DELBUF_TMP1 and DELBUF_TMP2 */
// XXX: should take destination range, instead of just corner cell?
static void pullcells(sheet_t *sp, int src, int cmd, cellref_t cr) {
    struct ent *p, *n, *next;
    int minrow, mincol, maxrow, maxcol;
    int numrows, numcols, deltar, deltac;
    int i;
    struct frange *fr;
    subsheet_t *db;

    if (!delbuf[src] || !delbuf[src]->ptr) {
        error("No data to pull");
        return;
    }
    if (cmd == 'C') {
        /* pullcells is called with qbuf == idx when cmd == 'C' */
        copy_range(sp, COPY_FROM_QBUF, rangeref_current(sp), rangeref_empty());
        return;
    }
    /* compute delbuf range */
    minrow = sp->maxrows;
    mincol = sp->maxcols;
    maxrow = 0;
    maxcol = 0;

    for (p = delbuf[src]->ptr; p; p = p->next) {
        if (p->row < minrow) minrow = p->row;
        if (p->row > maxrow) maxrow = p->row;
        if (p->col < mincol) mincol = p->col;
        if (p->col > maxcol) maxcol = p->col;
        p->flags |= MAY_SYNC;
    }

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
            db = delbuf[src];
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
        db = delbuf[src];
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
        kill_area(sp, DELBUF_TMP1, minrow + deltar, mincol + deltac,
                   maxrow + deltar, maxcol + deltac, KA_DEFAULT);
    } else
    if (cmd == 'p') {     /* PULL */
        // XXX: should just clear area (free cells) and sync the references
        kill_area(sp, DELBUF_TMP1, minrow + deltar, mincol + deltac,
                   maxrow + deltar, maxcol + deltac, KA_NO_FMT);
        sync_refs(sp);
        delbuf_free(DELBUF_TMP1);
    } else
    if (cmd == 't') {     /* PULLTP (transpose) */
        // XXX: named ranges and range references may not be updated properly
        // XXX: should just clear area (free cells) and sync the references
        kill_area(sp, DELBUF_TMP1, minrow + deltar, mincol + deltac,
                   minrow + deltar + maxcol - mincol,
                   mincol + deltac + maxrow - minrow, KA_NO_FMT);
        sync_refs(sp);
        delbuf_free(DELBUF_TMP1);
    }

    if (cmd != 'x') {     /* PULLXCHG */
        /* At this point, we copy the cells from the delete buffer into the
         * destination range.
         */
        for (p = delbuf[src]->ptr; p; p = p->next) {
            if (cmd == 't') {   /* Transpose rows and columns while pulling. */
                n = lookat(sp, minrow + deltar + p->col - mincol,
                           mincol + deltac + p->row - minrow);
            } else {
                n = lookat(sp, p->row + deltar, p->col + deltac);
            }
            ent_copy(sp, n, p, deltar, deltac, minrow, mincol, maxrow, maxcol, cmd);
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
            /* move the copied cells from destination range to temprary subsheet */
            delbuf_free(DELBUF_TMP1);
            db = delbuf_find(DELBUF_TMP1);
            for (p = delbuf[src]->ptr; p; p = p->next) {
                // XXX: db->colfmt and db->rowfmt are NULL
                killcell(sp, p->row + deltar, p->col + deltac, db, 0, 1);
            }
        }
        /* move the cells from delbuf to destination */
        for (p = delbuf[src]->ptr; p; p = next) {
            next = p->next;
            // XXX: what if destination is locked ?
            p->row += deltar;
            p->col += deltac;
            p->flags &= ~IS_DELETED;
            setcell(sp, p->row, p->col, p);
        }
        /* Replace the pulled cells in the source register with the
         * new set of cells.
         */
        delbuf[src]->ptr = delbuf[DELBUF_TMP1]->ptr;
        delbuf[DELBUF_TMP1]->ptr = NULL;
        /*
         * Now change the cell addresses in the delete buffer to match
         * where the original cells came from.
         */
        for (p = delbuf[src]->ptr; p; p = p->next) {
            p->row -= deltar;
            p->col -= deltac;
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

    if (any_locked_cells(sp, 0, c1, sp->maxrow, c2)) {
        error("Locked cells encountered. Nothing changed");
        return;
    }
    ncols = c2 - c1 + 1;
    // XXX: probably useless and counterproductive
    //      this is for frange_get_current(sp) which is questionable
    sp->curcol = c1;

    delbuf_free(DELBUF_9);
    delbuf_free(qbuf);
    sync_refs(sp);
    /* move cell range to subsheet delbuf[0] */
    kill_area(sp, DELBUF_DEF, 0, c1, sp->maxrow, c2, KA_DEFAULT);
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

    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(0, c1, sp->maxrow, c2);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = c1;
    adjust_ctx.move_rr = rangeref(0, c2 + 1, sp->maxrow, sp->maxcol);
    adjust_ctx.move_dr = 0;
    adjust_ctx.move_dc = -ncols;
    adjust_ctx.destrows = NULL;
    adjust_refs(&adjust_ctx);

    sp->maxcol -= ncols;

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
    int r, c;

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
        if (!delbuf[qbuf] || !delbuf[qbuf]->ptr) {
            // nothing to copy
            return;
        }
        // XXX: should use subsheet range
        minsr = sp->maxrow;
        minsc = sp->maxcol;
        maxsr = 0;
        maxsc = 0;
        for (p = delbuf[qbuf]->ptr; p; p = p->next) {
            if (p->row < minsr) minsr = p->row;
            if (p->row > maxsr) maxsr = p->row;
            if (p->col < minsc) minsc = p->col;
            if (p->col > maxsc) maxsc = p->col;
        }
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
    if (checkbounds(sp, &maxdr, &maxdc) < 0) {
        // XXX: should extend sheet
        error("cannot copy beyond boundaries");
        return;
    }

    if (flags & COPY_FROM_QBUF) {
        delbuf_copy(qbuf, DELBUF_TMP1);
    } else {
        // XXX: should not be necessary if copy order is well chosen
        //      or if source and destination do not overlap
        yank_area(sp, DELBUF_TMP1, rangeref(minsr, minsc, maxsr, maxsc));
    }

    /* move cell range to other temporary subsheet */
    kill_area(sp, DELBUF_TMP2, mindr, mindc, maxdr, maxdc, KA_NO_FMT);
    sync_refs(sp);
    delbuf_free(DELBUF_TMP2);

    error("Copying...");
    if (!loading)
        screen_refresh();

    /* copy source range, replicating to fill destination range */
    for (r = mindr; r <= maxdr; r++) {
        for (c = mindc; c <= maxdc; c++) {
            int deltar = r - minsr;
            int deltac = c - minsc;
            for (p = delbuf[DELBUF_TMP1]->ptr; p; p = p->next) {
                int vr = p->row + deltar;
                int vc = p->col + deltac;
                if (vr <= maxdr && vc <= maxdc) {
                    struct ent *n = lookat(sp, vr, vc);
                    if (n->flags & IS_LOCKED)
                        continue;
                    ent_copy(sp, n, p, deltar, deltac, 0, 0, sp->maxrow, sp->maxcol, 0);
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
    kill_area(sp, DELBUF_DEF, rr.left.row, rr.left.col, rr.right.row, rr.right.col, KA_DEFAULT);
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

    for (c = rr.left.col, r = rr.left.row;;) {
        struct ent *p = lookat(sp, r, c);
        if (!(p->flags & IS_LOCKED)) {
            if (p->type == SC_STRING) {
                string_set(&p->label, NULL); /* free the previous label */
            }
            p->type = SC_NUMBER;
            p->cellerror = 0;
            p->flags |= IS_CHANGED;
            p->v = start;
            start += inc;
        }
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
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = lookat(sp, r, c);
            // XXX: update IS_CHANGED?
            p->flags |= IS_LOCKED;
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

    // XXX: should be skip locked cells silently
    //      or should be fail with an error
    range_normalize(&rr);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            if (str) {
                struct ent *p = lookat(sp, r, c);
                if (p->flags & IS_LOCKED)
                    continue;
                string_set(&p->format, string_dup(str));
                p->flags |= IS_CHANGED;
            } else {
                struct ent *p = getcell(sp, r, c);
                if (!p || (p->flags & IS_LOCKED))
                    continue;
                if (p->format) {
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
static void sync_expr(sheet_t *sp, enode_t *e) {
    if (e == NULL)
        return;

    switch (e->type) {
    case OP_TYPE_RANGE:
        // XXX: should use same logic as for OP_TYPE_VAR
        e->e.r.left.vp = lookat(sp, e->e.r.left.vp->row, e->e.r.left.vp->col);
        e->e.r.right.vp = lookat(sp, e->e.r.right.vp->row, e->e.r.right.vp->col);
        break;
    case OP_TYPE_VAR:
        if (e->e.v.vp->flags & IS_CLEARED) {
            /* cell was moved to the free list */
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

// XXX: this duplicates sync_expr() ?
/* make cell references point to current sheet cells */
static void sync_enode(sheet_t *sp, struct enode *e) {
    if (e) {
        if (e->type == OP_TYPE_RANGE) {
            e->e.r.left.vp = lookat(sp, e->e.r.left.vp->row, e->e.r.left.vp->col);
            e->e.r.right.vp = lookat(sp, e->e.r.right.vp->row, e->e.r.right.vp->col);
        } else
        if (e->type == OP_TYPE_VAR) {
            e->e.v.vp = lookat(sp, e->e.v.vp->row, e->e.v.vp->col);
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
            e->e.r.left.vp = lookat(sp, r1, c1);
            e->e.r.right.vp = lookat(sp, r2, c2);
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

void cell_adjust(adjust_ctx_t *ap, cellref_t *cp) {
    if (cell_in_range(*cp, ap->clamp_rr)) {
        if (ap->clamp_newr >= 0) cp->row = ap->clamp_newr;
        if (ap->clamp_newc >= 0) cp->col = ap->clamp_newc;
    } else
    if (cell_in_range(*cp, ap->move_rr)) {
        if (ap->destrows) {
            cp->row = ap->destrows[cp->row - ap->move_rr.left.row];
        } else {
            cp->row += ap->move_dr;
            cp->col += ap->move_dc;
        }
    }
}

void range_adjust(adjust_ctx_t *ap, rangeref_t *rp) {
    if (ap->destrows) {
        if (rp->left.row == rp->right.row
        &&  cell_in_range(rp->left, ap->clamp_rr)
        &&  cell_in_range(rp->right, ap->clamp_rr))
            rp->left.row = rp->right.row = ap->destrows[rp->left.row - ap->move_rr.left.row];
        return;
    }
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

void adjust_refs(adjust_ctx_t *ap) {
    int i;
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
        n->flags &= ~ALIGN_MASK;
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
            struct ent *p;
            if ((p = *pp)) {
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
    struct ent *p = getcell(sp, row, col);
    if (p && (p->flags & IS_LOCKED)) {
        error("Cell %s is locked", cell_addr(sp, cellref(row, col)));
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
    int minr, minc, maxr, maxc;
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
    int i, r, nrows, col, len;
    const char *cp;
    struct ent *p;
    adjust_ctx_t adjust_ctx;

    range_normalize(&rr);
    sc->sp = sp;
    sc->minr = rr.left.row;
    sc->minc = rr.left.col;
    sc->maxr = rr.right.row;
    sc->maxc = rr.right.col;
    nrows = sc->maxr - sc->minr + 1;
    if (nrows <= 1)
        goto done;

    if (any_locked_cells(sp, sc->minr, sc->minc, sc->maxr, sc->maxc)) {
        error("Locked cells encountered. Nothing changed");
        goto done;
    }

    /* allocate sort criteria: at type 0, type 1 col=minc */
    sc->crit = scxmalloc(sizeof(*sc->crit) * 2);
    /* allocate array of row numbers that will be sorted */
    rows = scxmalloc(sizeof(*rows) * nrows);
    destrows = scxmalloc(sizeof(*rows) * nrows);
    if (!sc->crit || !rows || !destrows)
        goto fail;

    for (i = 0, r = sc->minr; r <= sc->maxr; r++, i++)
        rows[i] = destrows[i] = r;

    if (sempty(criteria)) {
        sc->crit[0].direction = 1;
        sc->crit[0].type = 1;
        sc->crit[0].column = sc->minc;
        sc->crit[1].direction = 1;
        sc->crit[1].type = 0;
        sc->crit[1].column = sc->minc;
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
                if (col >= sc->minc && col <= sc->maxc)
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
        destrows[rows[i] - sc->minr] = sc->minr + i;

    // XXX: this is bogus:
    // - cell values and formats should be moved
    // - cell borders should be left unchanged?
    // - ranges should be adjusted if completely included
    //   in a single row of the sorting range
    /* move cell range to temporary subsheet */
    kill_area(sp, DELBUF_TMP1, sc->minr, sc->minc, sc->maxr, sc->maxc, KA_NO_FMT | KA_IGNORE_LOCK);
    // XXX: this seems bogus too
    // XXX: make formulas that refer to the sort range
    //      point to empty cells
    // XXX: should we use sync_refs() instead?
    // XXX: should use destrows to update formulae and ranges
    //      the current model just moves all cells and updates all ranges
    sync_ranges(sp);
    for (p = delbuf[DELBUF_TMP1]->ptr; p; p = p->next) {
        if (p->row < sc->minr || p->row > sc->maxr) {
            /* cannot find row in sort range */
            error("sort error");
            break;
        }
        p->row = destrows[p->row - sc->minr];
        p->flags &= ~IS_DELETED;
        setcell(sp, p->row, p->col, p);
    }
    delbuf[DELBUF_TMP1]->ptr = NULL;
    delbuf_free(DELBUF_TMP1);

    /* Adjust range references. */
    adjust_ctx.sp = sp;
    adjust_ctx.clamp_rr = rangeref(-1, -1, -1, -1);
    adjust_ctx.clamp_newr = -1;
    adjust_ctx.clamp_newc = -1;
    adjust_ctx.move_rr = rr;
    adjust_ctx.move_dr = 0;
    adjust_ctx.move_dc = 0;
    adjust_ctx.destrows = destrows;
    adjust_refs(&adjust_ctx);

fail:
    scxfree(sc->crit);
    scxfree(rows);
    scxfree(destrows);
done:
    string_free(criteria);
}
