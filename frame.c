/*      SC      A Spreadsheet Calculator
 *              Framed range manipulation
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Originally created:  December, 2000
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#include "sc.h"

int frange_test(sheet_t *sp) {
    return sp->frange_base != NULL;
}

void frange_delete(sheet_t *sp, struct frange *r) {
    if (r) {
        if (r->next)
            r->next->prev = r->prev;
        else
            sp->frange_tail = r->prev;
        if (r->prev)
            r->prev->next = r->next;
        else
            sp->frange_base = r->next;
        scxfree(r);
        if (lastfr == r)
            lastfr = NULL;
    }
}

void frange_unframe(sheet_t *sp, int flags, rangeref_t rr) {
    struct frange *r, *next;

    if (flags & FRANGE_FIND) {
        r = frange_find(sp, rr.left.row, rr.left.col);
        if (!r) {
            error("Frame not found");
            return;
        }
        frange_delete(sp, r);
    } else {
        for (r = sp->frange_base; r; r = next) {
            next = r->next;
            if (range_overlap(r->orr, rr))
                frange_delete(sp, r);
        }
    }
}

void frange_add(sheet_t *sp, int flags, rangeref_t orr, rangeref_t irr,
                int toprows, int bottomrows, int leftcols, int rightcols)
{
    struct frange *r;

    if (flags & FRANGE_FIND) {
        r = frange_find(sp, orr.left.row, orr.left.col);
        if (!r) {
            error("Frame not found");
            return;
        }
        orr = r->orr;
    } else {
        /* create or modify an existing frame */
        range_normalize(&orr);
        /* locate existing frame and test for potential overlaps */
        for (r = sp->frange_base; r; r = r->next) {
            if (range_same(r->orr, orr))
                break;
            if (range_overlap(r->orr, orr)) {
                error("Framed ranges may not overlap");
                return;
            }
        }
    }

    if (flags & FRANGE_INNER) {
        /* inner range was specified */
        range_normalize(&irr);
    } else {
        irr = r ? r->irr : orr;
        /*
         * Has this frange already been created?  If so, any negative
         * parameters mean "don't change this value."
         */
        if (toprows >= 0)
            irr.left.row = orr.left.row + toprows;
        if (leftcols >= 0)
            irr.left.col = orr.left.col + leftcols;
        if (bottomrows >= 0)
            irr.right.row = orr.right.row - toprows;
        if (rightcols >= 0)
            irr.right.col = orr.right.col + rightcols;
    }
    /* If all frame sides are 0 or if inner range is not included
       in outer range, report error.
     */
    if (range_same(irr, orr) || !range_in_range(irr, orr)) {
        // XXX: should also detect if irr is valid and not empty
        error("Invalid inner frame");
        return;
    }
    if (r != NULL) {
        /* update existing frame */
        r->irr = irr;
    } else {
        /* allocate and link new frame */
        r = scxmalloc(sizeof(struct frange));
        r->orr = orr;
        r->irr = irr;
        r->next = sp->frange_base;
        r->prev = NULL;
        if (sp->frange_base)
            sp->frange_base->prev = r;
        else
            sp->frange_tail = r;
        sp->frange_base = r;
    }
    sp->modflg++;
    FullUpdate++;
}

void frange_clean(sheet_t *sp) {
    struct frange *fr;

    fr = sp->frange_base;
    sp->frange_base = sp->frange_tail = NULL;

    while (fr) {
        struct frange *nextfr = fr->next;
        scxfree(fr);
        fr = nextfr;
    }
    lastfr = NULL;
}

struct frange *frange_find(sheet_t *sp, int row, int col) {
    struct frange *r;

    for (r = sp->frange_base; r; r = r->next) {
        if (cell_in_range(cellref(row, col), r->orr))
            return r;
    }
    return 0;
}

void frange_adjust(adjust_ctx_t *ap) {
    struct frange *a;

    for (a = ap->sp->frange_base; a; a = a->next) {
        range_adjust(ap, &a->orr);
        range_adjust(ap, &a->irr);
    }
}

void frange_write(sheet_t *sp, FILE *f) {
    struct frange *r;

    for (r = sp->frange_tail; r; r = r->prev) {
        fprintf(f, "frame %s %s\n", range_addr(sp, r->orr), range_addr(sp, r->irr));
    }
}

void frange_list(sheet_t *sp, FILE *f) {
    struct frange *r;

    if (!frange_test(sp)) {
        fprintf(f, "  No frames\n");
        return;
    }

    fprintf(f, "  %-30s %s\n", "Outer Range", "Inner Range");
    if (!brokenpipe)
        fprintf(f, "  %-30s %s\n", "-----------", "-----------");

    for (r = sp->frange_tail; r; r = r->prev) {
        fprintf(f, "  %-30s %s\n", range_addr(sp, r->orr), range_addr(sp, r->irr));
        if (brokenpipe) return;
    }
}
