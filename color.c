/*      SC      A Spreadsheet Calculator
 *              Color manipulation routines
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Original Version Created:  January, 2001
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

struct SCXMEM colorpair *cpairs[1 + CPAIRS];

struct rgb_color {
    unsigned char r, g, b, a;
};

struct sc_style {
    unsigned short fg, bg;
    unsigned char so, i, b, u;
    struct rgb_color fg_rgb, bg_rgb;
#define STYLE_DEF(fg, bg) { fg, bg, 0, 0, 0, 0, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }}
};

static struct sc_style const default_style[1+CPAIRS] = {
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_BLACK),  /* 0: unused */
#if 0  // default colors from SC */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_BLUE),    /* 1: default cell color */
    STYLE_DEF(SC_COLOR_RED, SC_COLOR_WHITE),     /* 2: negative numbers */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_RED),     /* 3: cells with errors */
    STYLE_DEF(SC_COLOR_BLACK, SC_COLOR_YELLOW),  /* 4: '*' marking cells with attached notes */
    STYLE_DEF(SC_COLOR_BLACK, SC_COLOR_CYAN),    /* 5: the row and column number frame */
    STYLE_DEF(SC_COLOR_RED, SC_COLOR_CYAN),      /* 6: the current col and row frame */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_BLACK),
    STYLE_DEF(SC_COLOR_RED, SC_COLOR_BLACK),
#else  // default colors from 1-2-3 */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_BLACK),   /* 1: default cell color */
    STYLE_DEF(SC_COLOR_RED, SC_COLOR_BLACK),     /* 2: negative numbers */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_RED),     /* 3: cells with errors */
    STYLE_DEF(SC_COLOR_YELLOW, SC_COLOR_BLACK),  /* 4: '*' marking cells with attached notes */
    STYLE_DEF(SC_COLOR_BLACK, SC_COLOR_CYAN),    /* 5: the row and column number frame */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_BLUE),    /* 6: the current col and row frame */
    STYLE_DEF(SC_COLOR_WHITE, SC_COLOR_BLACK),
    STYLE_DEF(SC_COLOR_RED, SC_COLOR_BLACK),
#endif
};

int init_style(int n, int fg, int bg, enode_t *expr) {
    if (n < 1 || n > CPAIRS)
        return -1;
    if (!cpairs[n])
        cpairs[n] = scxmalloc(sizeof(struct colorpair));
    cpairs[n]->fg = fg;
    cpairs[n]->bg = bg;
    cpairs[n]->expr = expr;
    screen_init_pair(n, fg, bg);
    return 0;
}

void free_styles(void) {
    int i;
    for (i = 0; i <= CPAIRS; i++) {
        scxfree(cpairs[i]);
        cpairs[i] = NULL;
    }
}

void initcolor(sheet_t *sp, int colornum) {
    int i;
    if (colornum < 0 || colornum > CPAIRS) {
        error("Invalid color number %d", colornum);
        return;
    }
    for (i = 1; i <= CPAIRS; i++) {
        if (!colornum || i == colornum)
            init_style(i, default_style[i].fg, default_style[i].bg, NULL);
    }
    select_style(STYLE_CELL, 0);
}

void change_color(sheet_t *sp, int pair, enode_t *e) {
    int v, err = 0;

    if (pair < 1 || pair > CPAIRS) {
        error("Invalid color number %d", pair);
        return;
    }

    v = (int)neval_at(sp, e, 0, 0, &err);
    if (!err) {
        init_style(pair, v & 7, (v >> 3) & 7, e);
        sp->modflg++;
        FullUpdate++;
    }
}

void colors_write(sheet_t *sp, FILE *f, int indent) {
    int i, count = 0;
    buf_t(buf, FBUFLEN);

    for (i = 1; i <= CPAIRS; i++) {
        if (cpairs[i] && cpairs[i]->expr) {
            buf_setf(buf, "color %d = ", i);
            // XXX: what is the current cell?
            decompile_expr(sp, buf, cpairs[i]->expr, 0, 0, DCP_NO_LOCALE);
            fprintf(f, "%*s%s\n", indent, "", buf->buf);
            if (brokenpipe) return;
            count++;
        }
    }
    if (indent && count) fprintf(f, "\n");
}

int crange_test(sheet_t *sp) {
    return sp->crange_base != NULL;
}

void crange_delete(sheet_t *sp, struct crange *r) {
    if (r) {
        if (r->r_next)
            r->r_next->r_prev = r->r_prev;
        else
            sp->crange_tail = r->r_prev;
        if (r->r_prev)
            r->r_prev->r_next = r->r_next;
        else
            sp->crange_base = r->r_next;
        scxfree(r);
    }
}

void crange_add(sheet_t *sp, rangeref_t rr, int pair) {
    struct crange *r;

    range_normalize(&rr);

    if (!pair) {
        /* remove color range */
        for (r = sp->crange_base; r; r = r->r_next) {
            if ((r->r_left->row == rr.left.row) && (r->r_left->col == rr.left.col)
            &&  (r->r_right->row == rr.right.row) && (r->r_right->col == rr.right.col))
            {
                crange_delete(sp, r);
                sp->modflg++;
                FullUpdate++;
                return;
            }
        }
        error("Color range not defined");
        return;
    }

    r = scxmalloc(sizeof(struct crange));
    r->r_left = lookat(sp, rr.left.row, rr.left.col);
    r->r_right = lookat(sp, rr.right.row, rr.right.col);
    r->r_color = pair;

    r->r_prev = NULL;
    r->r_next = sp->crange_base;
    sp->crange_base = r;
    if (r->r_next)
        r->r_next->r_prev = r;
    else
        sp->crange_tail = r;

    sp->modflg++;
    FullUpdate++;
}

void crange_clean(sheet_t *sp) {
    struct crange *cr;

    cr = sp->crange_base;
    sp->crange_base = sp->crange_tail = NULL;
    while (cr) {
        struct crange *nextcr = cr->r_next;
        scxfree(cr);
        cr = nextcr;
    }
}

struct crange *crange_find(sheet_t *sp, int row, int col) {
    struct crange *r;

    for (r = sp->crange_base; r; r = r->r_next) {
        if ((r->r_left->row <= row) && (r->r_left->col <= col) &&
            (r->r_right->row >= row) && (r->r_right->col >= col))
            break;
    }
    return r;
}

void crange_sync(sheet_t *sp) {
    struct crange *cr;

    for (cr = sp->crange_base; cr; cr = cr->r_next) {
        cr->r_left = lookat(sp, cr->r_left->row, cr->r_left->col);
        cr->r_right = lookat(sp, cr->r_right->row, cr->r_right->col);
    }
}

void crange_write(sheet_t *sp, FILE *f) {
    struct crange *r;

    for (r = sp->crange_tail; r; r = r->r_prev) {
        fprintf(f, "color %s %d\n",
                r_name(sp, r->r_left->row, r->r_left->col,
                       r->r_right->row, r->r_right->col),
                r->r_color);
    }
}

void crange_list(sheet_t *sp, FILE *f) {
    struct crange *r;

    colors_write(sp, f, 2); // XXX: inconsistent format
    if (brokenpipe) return;

    if (!crange_test(sp)) {
        fprintf(f, "  No color ranges");
        return;
    }

    fprintf(f, "  %-30s %s\n", "Range", "Color");
    if (!brokenpipe) fprintf(f, "  %-30s %s\n", "-----", "-----");

    for (r = sp->crange_tail; r; r = r->r_prev) {
        fprintf(f, "  %-32s %d\n",
                r_name(sp, r->r_left->row, r->r_left->col,
                       r->r_right->row, r->r_right->col),
                r->r_color);
        if (brokenpipe) return;
    }
}

void crange_fix(sheet_t *sp, int row1, int col1, int row2, int col2,
                int delta1, int delta2, struct frange *fr)
{
    int r1, c1, r2, c2;
    struct crange *cr, *ncr;

    for (cr = sp->crange_base; cr; cr = ncr) {
        ncr = cr->r_next;
        r1 = cr->r_left->row;
        c1 = cr->r_left->col;
        r2 = cr->r_right->row;
        c2 = cr->r_right->col;

        if (!fr || (c1 >= fr->or_left->col && c1 <= fr->or_right->col)) {
            if (r1 != r2 && r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
            if (c1 != c2 && c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
        }

        if (!fr || (c2 >= fr->or_left->col && c2 <= fr->or_right->col)) {
            if (r1 != r2 && r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
            if (c1 != c2 && c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
        }

        if (r1 > r2 || c1 > c2 ||
            (row1 >= 0 && row2 >= 0 && r1 >= row1 && r2 <= row2) ||
            (col1 >= 0 && col2 >= 0 && c1 >= col1 && c2 <= col2))
        {
            crange_delete(sp, cr);
        } else {
            cr->r_left = lookat(sp, r1, c1);
            cr->r_right = lookat(sp, r2, c2);
        }
    }
}
