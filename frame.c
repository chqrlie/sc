/*      SC      A Spreadsheet Calculator
 *              Framed range manipulation
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Originally created:  December, 2000
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

static SCXMEM struct frange *frame_base;
static struct frange *frame_tail;

int are_frames(void) {
    return frame_base != NULL;
}

void del_frange(struct frange *r) {
    if (r) {
        if (r->r_next)
            r->r_next->r_prev = r->r_prev;
        else
            frame_tail = r->r_prev;
        if (r->r_prev)
            r->r_prev->r_next = r->r_next;
        else
            frame_base = r->r_next;
        scxfree(r);
        if (lastfr == r)
            lastfr = NULL;
    }
}

void add_frange(sheet_t *sp, int flags, rangeref_t orr, rangeref_t irr,
                int toprows, int bottomrows, int leftcols, int rightcols)
{
    struct ent *or_left, *or_right, *ir_left = NULL, *ir_right = NULL;
    struct frange *r;

    if (flags & FRANGE_FIND) {
        r = find_frange(orr.left.row, orr.left.col);
        if (!r)
            return;
        orr.left.row = r->or_left->row;
        orr.left.col = r->or_left->col;
        orr.right.row = r->or_right->row;
        orr.right.col = r->or_right->col;
    }
    range_normalize(&orr);

    or_left = lookat(sp, orr.left.row, orr.left.col);
    or_right = lookat(sp, orr.right.row, orr.right.col);

    if (flags & FRANGE_INNER) {
        range_normalize(&irr);

        ir_left = lookat(sp, irr.left.row, irr.left.col);
        ir_right = lookat(sp, irr.right.row, irr.right.col);

        if (irr.left.row < orr.left.row || irr.left.col < orr.left.col
        ||  irr.right.row > orr.right.row || irr.right.col > orr.right.col) {
            // XXX: should also detect if irr is strictly included
            //      and not empty
            error("Invalid parameters");
            return;
        }
    }

    /*
     * Has this frange already been created?  If so, any negative
     * parameters mean "don't change this value."
     */
    for (r = frame_base; r; r = r->r_next) {
        if ((r->or_left == or_left) && (r->or_right == or_right)) {
            if (ir_left) {
                r->ir_left = ir_left;
                r->ir_right = ir_right;
            } else {
                if (toprows < 0)
                    toprows = r->ir_left->row - r->or_left->row;
                if (bottomrows < 0)
                    bottomrows = r->or_right->row - r->ir_right->row;
                if (leftcols < 0)
                    leftcols = r->ir_left->col - r->or_left->col;
                if (rightcols < 0)
                    rightcols = r->or_right->col - r->ir_right->col;
                r->ir_left = lookat(sp, r->or_left->row + toprows,
                                    r->or_left->col + leftcols);
                r->ir_right = lookat(sp, r->or_right->row - bottomrows,
                                     r->or_right->col - rightcols);
            }

            /* If all frame sides are 0, delete the frange */
            if (r->ir_left == r->or_left && r->ir_right == r->or_right) {
                del_frange(r);
            }
            modflg++;
            FullUpdate++;
            return;
        }
    }
    /*
     * See if the specified range overlaps any previously created frange.
     */
    for (r = frame_base; r; r = r->r_next) {
        if (!(r->or_left->row > orr.right.row || r->or_right->row < orr.left.row ||
              r->or_left->col > orr.right.col || r->or_right->col < orr.left.col))
        {
            error("Framed ranges may not be nested or overlapping");
            return;
        }
    }

    if (ir_left != or_left || ir_right != or_right) {
        r = scxmalloc(sizeof(struct frange));
        r->or_left = or_left;
        r->or_right = or_right;

        if (ir_left) {
            r->ir_left  = ir_left;
            r->ir_right = ir_right;
        } else {
            if (toprows    < 0) toprows    = 0;
            if (bottomrows < 0) bottomrows = 0;
            if (leftcols   < 0) leftcols   = 0;
            if (rightcols  < 0) rightcols  = 0;
            r->ir_left = lookat(sp, r->or_left->row + toprows,
                                r->or_left->col + leftcols);
            r->ir_right = lookat(sp, r->or_right->row - bottomrows,
                                 r->or_right->col - rightcols);
        }

        r->r_next = frame_base;
        r->r_prev = NULL;
        if (frame_base)
            frame_base->r_prev = r;
        else
            frame_tail = r;
        frame_base = r;

        modflg++;
        FullUpdate++;
    }
}

void clean_frange(void) {
    struct frange *fr;

    fr = frame_base;
    frame_base = frame_tail = NULL;

    while (fr) {
        struct frange *nextfr = fr->r_next;
        scxfree(fr);
        fr = nextfr;
    }
    lastfr = NULL;
}

struct frange *find_frange(int row, int col) {
    struct frange *r;

    for (r = frame_base; r; r = r->r_next) {
        if ((r->or_left->row <= row) && (r->or_left->col <= col) &&
            (r->or_right->row >= row) && (r->or_right->col >= col))
            return r;
    }
    return 0;
}

void sync_franges(sheet_t *sp) {
    struct frange *fr;

    for (fr = frame_base; fr; fr = fr->r_next) {
        fr->or_left  = lookat(sp, fr->or_left->row,  fr->or_left->col);
        fr->or_right = lookat(sp, fr->or_right->row, fr->or_right->col);
        fr->ir_left  = lookat(sp, fr->ir_left->row,  fr->ir_left->col);
        fr->ir_right = lookat(sp, fr->ir_right->row, fr->ir_right->col);
    }
}

void write_franges(sheet_t *sp, FILE *f) {
    struct frange *r;

    for (r = frame_tail; r; r = r->r_prev) {
        fprintf(f, "frame %s %s\n",
                r_name(r->or_left->row, r->or_left->col,
                       r->or_right->row, r->or_right->col),
                r_name(r->ir_left->row, r->ir_left->col,
                       r->ir_right->row, r->ir_right->col));
    }
}

void list_frames(FILE *f) {
    struct frange *r;

    if (!are_frames()) {
        fprintf(f, "  No frames");
        return;
    }

    fprintf(f, "  %-30s %s\n", "Outer Range", "Inner Range");
    if (!brokenpipe)
        fprintf(f, "  %-30s %s\n", "-----------", "-----------");

    for (r = frame_tail; r; r = r->r_prev) {
        fprintf(f, "  %-30s %s\n",
                r_name(r->or_left->row, r->or_left->col,
                       r->or_right->row, r->or_right->col),
                r_name(r->ir_left->row, r->ir_left->col,
                       r->ir_right->row, r->ir_right->col));
        if (brokenpipe) return;
    }
}

void fix_frames(sheet_t *sp, int row1, int col1, int row2, int col2,
                int delta1, int delta2, struct frange *fr)
{
    int r1, c1, r2, c2;
    struct frange *r;

    for (r = frame_base; r; r = r->r_next) {
        r1 = r->or_left->row;
        c1 = r->or_left->col;
        r2 = r->or_right->row;
        c2 = r->or_right->col;

        if (!fr || (c1 >= fr->or_left->col && c1 <= fr->or_right->col)) {
            if (r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
            if (c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
        }

        if (!fr || (c2 >= fr->or_left->col && c2 <= fr->or_right->col)) {
            if (r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
            if (c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
        }

        r->or_left = lookat(sp, r1, c1);
        r->or_right = lookat(sp, r2, c2);

        r1 = r->ir_left->row;
        c1 = r->ir_left->col;
        r2 = r->ir_right->row;
        c2 = r->ir_right->col;

        if (!fr || (c1 >= fr->or_left->col && c1 <= fr->or_right->col)) {
            if (r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
            if (c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
        }

        if (!fr || (c2 >= fr->or_left->col && c2 <= fr->or_right->col)) {
            if (r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
            if (c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
        }

        r->ir_left = lookat(sp, r1, c1);
        r->ir_right = lookat(sp, r2, c2);
    }
}
