
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

void initcolor(int colornum) {
    if (!colornum) {
        int i;

        // XXX: why allocate these instead of using an array of structs?
        for (i = 1; i <= CPAIRS; i++)
            cpairs[i] = scxmalloc(sizeof(struct colorpair));
    }

    /* default colors */
    if (!colornum || colornum == 1) {
        cpairs[1]->fg = COLOR_WHITE;
        cpairs[1]->bg = COLOR_BLUE;
        cpairs[1]->expr = NULL;
        init_pair(1, cpairs[1]->fg, cpairs[1]->bg);
    }

    /* default for negative numbers */
    if (!colornum || colornum == 2) {
        cpairs[2]->fg = COLOR_RED;
        cpairs[2]->bg = COLOR_WHITE;
        cpairs[2]->expr = NULL;
        init_pair(2, cpairs[2]->fg, cpairs[2]->bg);
    }

    /* default for cells with errors */
    if (!colornum || colornum == 3) {
        cpairs[3]->fg = COLOR_WHITE;
        cpairs[3]->bg = COLOR_RED;
        cpairs[3]->expr = NULL;
        init_pair(3, cpairs[3]->fg, cpairs[3]->bg);
    }

    /* default for '*' marking cells with attached notes */
    if (!colornum || colornum == 4) {
        cpairs[4]->fg = COLOR_BLACK;
        cpairs[4]->bg = COLOR_YELLOW;
        cpairs[4]->expr = NULL;
        init_pair(4, cpairs[4]->fg, cpairs[4]->bg);
    }

    if (!colornum || colornum == 5) {
        cpairs[5]->fg = COLOR_BLACK;
        cpairs[5]->bg = COLOR_CYAN;
        cpairs[5]->expr = NULL;
        init_pair(5, cpairs[5]->fg, cpairs[5]->bg);
    }

    if (!colornum || colornum == 6) {
        cpairs[6]->fg = COLOR_RED;
        cpairs[6]->bg = COLOR_CYAN;
        cpairs[6]->expr = NULL;
        init_pair(6, cpairs[6]->fg, cpairs[6]->bg);
    }

    if (!colornum || colornum == 7) {
        cpairs[7]->fg = COLOR_WHITE;
        cpairs[7]->bg = COLOR_BLACK;
        cpairs[7]->expr = NULL;
        init_pair(7, cpairs[7]->fg, cpairs[7]->bg);
    }

    if (!colornum || colornum == 8) {
        cpairs[8]->fg = COLOR_RED;
        cpairs[8]->bg = COLOR_BLACK;
        cpairs[8]->expr = NULL;
        init_pair(8, cpairs[8]->fg, cpairs[8]->bg);
    }

    if (color && has_colors())
        color_set(1, NULL);
}

void change_color(int pair, struct enode *e) {
    int v;

    if (pair < 1 || pair > CPAIRS) {
        error("Invalid color number %d", pair);
        return;
    }

    v = (int)eval(e);

    if (!cpairs[pair])
        cpairs[pair] = scxmalloc(sizeof(struct colorpair));
    cpairs[pair]->fg = v & 7;
    cpairs[pair]->bg = (v >> 3) & 7;
    cpairs[pair]->expr = e;
    if (color && has_colors())
        init_pair(pair, cpairs[pair]->fg, cpairs[pair]->bg);

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

void sc_setcolor(int set) {
    color = set;
    if (usecurses && has_colors()) {
        if (set) {
            color_set(1, NULL);
            bkgd(COLOR_PAIR(1) | ' ');
        } else {
            color_set(0, NULL);
            bkgd(COLOR_PAIR(0) | ' ');
        }
        FullUpdate++;
    }
}
