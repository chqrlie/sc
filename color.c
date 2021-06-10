/*      SC      A Spreadsheet Calculator
 *              Color manipulation routines
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Original Version Created:  January, 2001
 *
 *              $Revision: 7.16 $
 */

#include <sys/types.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <limits.h>
#include "sc.h"

struct colorpair *cpairs[1 + CPAIRS];

static struct crange *color_base;

int are_colors(void) {
    return color_base != NULL;
}

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
    STYLE_DEF(COLOR_WHITE, COLOR_BLACK),  /* 0: unused */
#if 0  // default colors from SC */
    STYLE_DEF(COLOR_WHITE, COLOR_BLUE),    /* 1: default cell color */
    STYLE_DEF(COLOR_RED, COLOR_WHITE),     /* 2: negative numbers */
    STYLE_DEF(COLOR_WHITE, COLOR_RED),     /* 3: cells with errors */
    STYLE_DEF(COLOR_BLACK, COLOR_YELLOW),  /* 4: '*' marking cells with attached notes */
    STYLE_DEF(COLOR_BLACK, COLOR_CYAN),    /* 5: the row and column number frame */
    STYLE_DEF(COLOR_RED, COLOR_CYAN),      /* 6: the current col and row frame */
    STYLE_DEF(COLOR_WHITE, COLOR_BLACK),
    STYLE_DEF(COLOR_RED, COLOR_BLACK),
#else  // default colors from 1-2-3 */
    STYLE_DEF(COLOR_WHITE, COLOR_BLACK),   /* 1: default cell color */
    STYLE_DEF(COLOR_RED, COLOR_BLACK),     /* 2: negative numbers */
    STYLE_DEF(COLOR_WHITE, COLOR_RED),     /* 3: cells with errors */
    STYLE_DEF(COLOR_YELLOW, COLOR_BLACK),  /* 4: '*' marking cells with attached notes */
    STYLE_DEF(COLOR_BLACK, COLOR_CYAN),    /* 5: the row and column number frame */
    STYLE_DEF(COLOR_WHITE, COLOR_BLUE),    /* 6: the current col and row frame */
    STYLE_DEF(COLOR_WHITE, COLOR_BLACK),
    STYLE_DEF(COLOR_RED, COLOR_BLACK),
#endif
};

int init_style(int n, int fg, int bg, struct enode *expr) {
    if (n < 1 || n > CPAIRS)
        return -1;
    if (!cpairs[n])
        cpairs[n] = scxmalloc(sizeof(struct colorpair));
    cpairs[n]->fg = fg;
    cpairs[n]->bg = bg;
    cpairs[n]->expr = expr;
    if (color && has_colors())
        init_pair(n, fg, bg);
    return 0;
}

void initcolor(int colornum) {
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

void change_color(int pair, struct enode *e) {
    int v;

    if (pair < 1 || pair > CPAIRS) {
        error("Invalid color number %d", pair);
        return;
    }

    v = (int)eval(e);
    init_style(pair, v & 7, (v >> 3) & 7, e);
    modflg++;
    FullUpdate++;
}

void add_crange(struct ent *r_left, struct ent *r_right, int pair) {
    struct crange *r;
    int minr, minc, maxr, maxc;

    minr = r_left->row < r_right->row ? r_left->row : r_right->row;
    minc = r_left->col < r_right->col ? r_left->col : r_right->col;
    maxr = r_left->row > r_right->row ? r_left->row : r_right->row;
    maxc = r_left->col > r_right->col ? r_left->col : r_right->col;

    if (!pair) {
        for (r = color_base; r; r = r->r_next) {
            if ((r->r_left->row == r_left->row) &&
                (r->r_left->col == r_left->col) &&
                (r->r_right->row == r_right->row) &&
                (r->r_right->col == r_right->col))
            {
                if (r->r_next)
                    r->r_next->r_prev = r->r_prev;
                if (r->r_prev)
                    r->r_prev->r_next = r->r_next;
                else
                    color_base = r->r_next;
                scxfree(r);
                modflg++;
                FullUpdate++;
                return;
            }
        }
        error("Color range not defined");
        return;
    }

    r = scxmalloc(sizeof(struct crange));
    r->r_left = lookat(minr, minc);
    r->r_right = lookat(maxr, maxc);
    r->r_color = pair;

    r->r_next = color_base;
    r->r_prev = NULL;
    if (r->r_next)
        r->r_next->r_prev = r;
    color_base = r;

    modflg++;
    FullUpdate++;
}

void clean_crange(void) {
    struct crange *cr;

    cr = color_base;
    color_base = NULL;
    while (cr) {
        struct crange *nextcr = cr->r_next;
        scxfree(cr);
        cr = nextcr;
    }
}

struct crange *find_crange(int row, int col) {
    struct crange *r;

    for (r = color_base; r; r = r->r_next) {
        if ((r->r_left->row <= row) && (r->r_left->col <= col) &&
            (r->r_right->row >= row) && (r->r_right->col >= col))
            break;
    }
    return r;
}

void sync_cranges(void) {
    struct crange *cr;

    cr = color_base;
    while (cr) {
        cr->r_left = lookat(cr->r_left->row, cr->r_left->col);
        cr->r_right = lookat(cr->r_right->row, cr->r_right->col);
        cr = cr->r_next;
    }
}

void write_cranges(FILE *f) {
    struct crange *r;
    struct crange *nextr;

    for (r = nextr = color_base; nextr; r = nextr, nextr = r->r_next)
        continue;
    while (r) {
        fprintf(f, "color %s:%s %d\n",
                v_name(r->r_left->row, r->r_left->col),
                v_name(r->r_right->row, r->r_right->col),
                r->r_color);
        r = r->r_prev;
    }
}

void write_colors(FILE *f, int indent) {
    int i, count = 0;

    for (i = 1; i <= CPAIRS; i++) {
        if (cpairs[i] && cpairs[i]->expr) {
            snprintf(line, sizeof line, "color %d = ", i);
            linelim = strlen(line);
            decompile(cpairs[i]->expr, 0);
            fprintf(f, "%*s%s\n", indent, "", line);
            if (brokenpipe) return;
            count++;
        }
    }
    if (indent && count) fprintf(f, "\n");
}

void list_colors(FILE *f) {
    struct crange *r;
    struct crange *nextr;

    write_colors(f, 2);
    linelim = -1;
    if (brokenpipe) return;

    if (!are_colors()) {
        fprintf(f, "  No color ranges");
        return;
    }

    fprintf(f, "  %-30s %s\n", "Range", "Color");
    if (!brokenpipe) fprintf(f, "  %-30s %s\n", "-----", "-----");

    for (r = nextr = color_base; nextr; r = nextr, nextr = r->r_next)
        continue;
    while (r) {
        fprintf(f, "  %-32s %d\n", r_name(r->r_left->row, r->r_left->col,
                                          r->r_right->row, r->r_right->col), r->r_color);
        if (brokenpipe) return;
        r = r->r_prev;
    }
}

void fix_colors(int row1, int col1, int row2, int col2, int delta1, int delta2) {
    int r1, c1, r2, c2;
    struct crange *cr, *ncr;
    struct frange *fr;

    fr = find_frange(currow, curcol);

    if (color_base) {
        for (cr = color_base; cr; cr = ncr) {
            ncr = cr->r_next;
            r1 = cr->r_left->row;
            c1 = cr->r_left->col;
            r2 = cr->r_right->row;
            c2 = cr->r_right->col;

            if (!(fr && (c1 < fr->or_left->col || c1 > fr->or_right->col))) {
                if (r1 != r2 && r1 >= row1 && r1 <= row2) r1 = row2 - delta1;
                if (c1 != c2 && c1 >= col1 && c1 <= col2) c1 = col2 - delta1;
            }

            if (!(fr && (c2 < fr->or_left->col || c2 > fr->or_right->col))) {
                if (r1 != r2 && r2 >= row1 && r2 <= row2) r2 = row1 + delta2;
                if (c1 != c2 && c2 >= col1 && c2 <= col2) c2 = col1 + delta2;
            }

            if (r1 > r2 || c1 > c2 ||
                (row1 >= 0 && row2 >= 0 && row1 <= r1 && row2 >= r2) ||
                (col1 >= 0 && col2 >= 0 && col1 <= c1 && col2 >= c2)) {
                /* the 0 means delete color range */
                add_crange(cr->r_left, cr->r_right, 0);
            } else {
                cr->r_left = lookat(r1, c1);
                cr->r_right = lookat(r2, c2);
            }
        }
    }
}
