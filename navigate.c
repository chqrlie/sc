/*      SC      A Spreadsheet Calculator
 *              Navigation routines
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#if defined REGCOMP
#include <regex.h>
#elif defined RE_COMP
extern char *re_comp(char *s);
extern char *re_exec(char *s);
#elif defined REGCMP
char *regcmp();
char *regex();
#else
#endif

#include "sc.h"

/* Use this structure to save the last 'g' command */
// XXX: this duplicates the search_ctx_t feature
struct go_save gs;

/* Goto subroutines */

void go_free(sheet_t *sp) {
    gs.g_type = G_NONE;
    string_set(&gs.g_s, NULL);
}

/* repeat the last goto command */
void go_last(sheet_t *sp) {
    switch (gs.g_type) {
    case G_CELL:
        moveto(sp, gs.g_rr, gs.st);
        break;
    case G_NUM:
    case G_ERROR:
    case G_INVALID:
    case G_STR:
    case G_NSTR:
    case G_XSTR:
        do_search(sp, gs.g_type, gs.g_rr, gs.g_n, string_dup(gs.g_s));
        break;
    default:
        error("Nothing to repeat");
        break;
    }
}

/* Place the cursor on a given cell.  If st.row >= 0, place the cell
 * at row st.row and column st.col in the upper left corner of the
 * screen if possible.
 */
void moveto(sheet_t *sp, rangeref_t rr, cellref_t st) {
    remember(sp, 0);

    sp->currow = rr.left.row;
    sp->curcol = rr.left.col;
    go_free(sp);
    gs.g_type = G_CELL;
    gs.g_rr = rr;
    gs.st = st;
    if (st.row >= 0) {
        sp->strow = st.row;
        sp->stcol = st.col;
        gs.stflag = 1;
    } else {
        gs.stflag = 0;
    }
    rowsinrange = rows_height(sp, rr.left.row, rr.right.row - rr.left.row + 1);
    colsinrange = cols_width(sp, rr.left.col, rr.right.col - rr.left.col + 1);

    FullUpdate++;
    if (loading) {
        // XXX: shy update the screen now?
        update(sp, 1);
        changed = 0;
    } else {
        remember(sp, 1);
    }
}

// XXX: this duplicates the go_save structure
typedef struct search_context {
    sheet_t *sp;
    int g_type;
    int errsearch;
    double n;
    const char *s;
#if defined REGCOMP
    regex_t preg;
    int errcode;
#elif defined RE_COMP
    char *tmp;
#elif defined REGCMP
    char *tmp;
#else
#endif
} search_ctx_t;

static int search_init(search_ctx_t *sc, sheet_t *sp, int g_type, rangeref_t rr, double n, SCXMEM string_t *str) {
    sc->sp = sp;

    go_free(sp);
    gs.g_type = g_type;
    gs.g_rr = rr;
    gs.g_n = n;
    string_set(&gs.g_s, str);

    switch (sc->g_type = g_type) {
    case G_ERROR:
    case G_INVALID:
        // XXX: refine this, find all errors for now
        sc->errsearch = -2;
        break;
    case G_NUM:
        sc->n = n;
        break;
    case G_STR:
    case G_NSTR:
    case G_XSTR:
        if (!str)
            return -1;

        sc->s = s2c(str);

#if defined REGCOMP
        if ((sc->errcode = regcomp(&sc->preg, sc->s, REG_EXTENDED))) {
            char buf[160];
            regerror(sc->errcode, &sc->preg, buf, sizeof(buf));
            error("%s", buf);
            return -1;
        }
#elif defined RE_COMP
        if ((sc->tmp = re_comp(sc->s)) != NULL) {
            error("%s", sc->tmp);
            return -1;
        }
#elif defined REGCMP
        if ((sc->tmp = regcmp(sc->s, NULL)) == NULL) {
            error("Invalid search string");
            return -1;
        }
#else
        /* otherwise nothing to do, will just use strcmp() */
#endif
        break;
    }
    return 0;
}
static int search_match(search_ctx_t *sc, int row, int col, struct ent *p) {
    switch (sc->g_type) {
    case G_ERROR:
    case G_INVALID:
        if (sc->errsearch & (1 << p->cellerror))
            return 1;
        break;
    case G_NUM:
        if (p->type == SC_NUMBER && p->v == sc->n)
            return 1;
        break;
    case G_STR:
    case G_NSTR:
    case G_XSTR: {
            /* convert cell contents, do not test width, ignore alignment */
            char field[FBUFLEN];
            const char *s1 = field;
            int align = ALIGN_DEFAULT;
            sheet_t *sp = sc->sp;

            *field = '\0';
            if (sc->g_type == G_NSTR) {
                /* match regex on formated number, error or boolean */
                if (p->cellerror) {
                    s1 = error_name[p->cellerror];
                } else
                if (p->type == SC_BOOLEAN) {
                    s1 = boolean_name[!!p->v];
                } else
                if (p->type == SC_NUMBER) {
                    if (p->format) {
                        format(field, sizeof field, s2c(p->format), sp->colfmt[col].precision, p->v, &align);
                    } else {
                        engformat(field, sizeof field, sp->colfmt[col].realfmt, sp->colfmt[col].precision, p->v, &align);
                    }
                }
                // XXX: should other types be matched too?
            } else if (sc->g_type == G_XSTR) {
                /* match regex on expression source code only */
                if (p->expr) {
                    // XXX: should pass row, col as the cell reference
                    decompile(sp, field, sizeof field, p->expr, 0, 0, DCP_DEFAULT);
                    if (*field == '?')
                        *field = '\0';
                }
            } else if (sc->g_type == G_STR) {
                if (p->type == SC_STRING) {
                    s1 = s2str(p->label);
                }
            }
            if (s1 && *s1
#if defined REGCOMP
            &&  (regexec(&sc->preg, s1, 0, NULL, 0) == 0)
#elif defined RE_COMP
            &&  (re_exec(s1) != 0)
#elif defined REGCMP
            &&  (regex(sc->tmp, s1) != NULL)
#else
            &&  (strcmp(sc->s, s1) == 0) // case sensitive
#endif
                ) {
                return 1;
            }
            break;
        }
    }
    return 0;
}

static void search_close(search_ctx_t *sc, int found) {
    switch (sc->g_type) {
    case G_ERROR:
    case G_INVALID:
        if (!found)
            error("no ERROR cell found");
        break;
    case G_NUM:
        if (!found)
            error("Number not found");
        break;
    case G_STR:
    case G_NSTR:
    case G_XSTR:
#if defined REGCOMP
        regfree(&sc->preg);
#elif defined RE_COMP
#elif defined REGCMP
        free(sc->tmp);
#else
#endif
        if (!found)
            error("String not found");
        break;
    }
}

/*
 * 'goto' either a given number, string, regex, 'error', or 'invalid' starting at currow/curcol
 */
int do_search(sheet_t *sp, int g_type, rangeref_t rr, double n, SCXMEM string_t *str) {
    search_ctx_t sc[1];
    struct ent *p;
    int firstrow, firstcol, lastrow, lastcol;
    int row, col, endr, endc;
    int found = 0;

    if (search_init(sc, sp, g_type, rr, n, str))
        return -1;

    remember(sp, 0);

    firstrow = rr.left.row;
    firstcol = rr.left.col;
    lastrow = rr.right.row;
    lastcol = rr.right.col;

    // XXX: should clip search area to active area
    row = sp->currow;
    col = sp->curcol;
    if (row >= firstrow && row <= lastrow && col >= firstcol && col <= lastcol) {
        endr = row;
        endc = col;
    } else {
        endr = row = lastrow;
        endc = col = lastcol;
    }

    for (;;) {
        if (col++ >= lastcol) {
            col = firstcol;
            if (col > lastcol)
                break;
            if (row++ >= lastrow) {
                row = firstrow;
                if (row > lastrow)
                    break;
            }
        }
        // XXX: should skip hidden rows
        if (!row_hidden(sp, row) && !col_hidden(sp, col) && (p = getcell(sp, row, col))) {
            if (search_match(sc, row, col, p)) {
                found = 1;
                break;
            }
        }
        if (row == endr && col == endc)
            break;
    }

    search_close(sc, found);

    if (found) {
        sp->currow = row;
        sp->curcol = col;
        if (loading) {
            update(sp, 1);
            changed = 0;
        } else {
            remember(sp, 1);
        }
    }
    return found;
}

/* spreadsheet navigation primitives */

void doend(sheet_t *sp, int rowinc, int colinc) {
    int r, c;

    remember(sp, 0);

    if (valid_cell(sp, sp->currow, sp->curcol)) {
        r = sp->currow + rowinc;
        c = sp->curcol + colinc;
        if (r >= 0 && r < sp->maxrows &&
            c >= 0 && c < sp->maxcols &&
            !valid_cell(sp, r, c)) {
                sp->currow = r;
                sp->curcol = c;
        }
    }

    // XXX: skip hidden cells?
    if (!valid_cell(sp, sp->currow, sp->curcol)) {
        switch (rowinc) {
        case -1:
            while (!valid_cell(sp, sp->currow, sp->curcol) && sp->currow > 0)
                sp->currow--;
            break;
        case  1:
            while (!valid_cell(sp, sp->currow, sp->curcol) && sp->currow < sp->maxrows - 1)
                sp->currow++;
            break;
        case  0:
            switch (colinc) {
            case -1:
                while (!valid_cell(sp, sp->currow, sp->curcol) && sp->curcol > 0)
                    sp->curcol--;
                break;
            case  1:
                while (!valid_cell(sp, sp->currow, sp->curcol) && sp->curcol < sp->maxcols - 1)
                    sp->curcol++;
                break;
            }
            break;
        }
        remember(sp, 1);
        return;
    }

    switch (rowinc) {
    case -1:
        while (valid_cell(sp, sp->currow, sp->curcol) && sp->currow > 0)
            sp->currow--;
        break;
    case  1:
        while (valid_cell(sp, sp->currow, sp->curcol) && sp->currow < sp->maxrows - 1)
            sp->currow++;
        break;
    case  0:
        switch (colinc) {
        case -1:
            while (valid_cell(sp, sp->currow, sp->curcol) && sp->curcol > 0)
                sp->curcol--;
            break;
        case  1:
            while (valid_cell(sp, sp->currow, sp->curcol) && sp->curcol < sp->maxcols - 1)
                sp->curcol++;
            break;
        }
        break;
    }
    if (!valid_cell(sp, sp->currow, sp->curcol)) {
        // XXX: this is bogus if already on maxcol or maxrow
        sp->currow -= rowinc;
        sp->curcol -= colinc;
    }
}

/* moves currow down one page */
// XXX: hidden row issue
void forwpage(sheet_t *sp, int arg) {
    int ps = sp->pagesize ? sp->pagesize : (screen_LINES - RESROW - framerows) / 2;
    forwrow(sp, arg * ps);
    sp->strow = sp->strow + arg * ps;
    FullUpdate++;
}

/* moves currow up one page */
// XXX: hidden row issue
void backpage(sheet_t *sp, int arg) {
    int ps = sp->pagesize ? sp->pagesize : (screen_LINES - RESROW - framerows) / 2;
    backrow(sp, arg * ps);
    sp->strow = sp->strow - arg * ps;
    if (sp->strow < 0) sp->strow = 0;
    FullUpdate++;
}

/* moves curcol forward to the next cell, wrapping at maxcols - 1 */
void forwcell(sheet_t *sp, int arg) {
    while (arg --> 0) {
        do {
            if (sp->curcol < sp->maxcols - 1) {
                sp->curcol++;
            } else
            if (sp->currow < sp->maxrows - 1) {
                sp->curcol = 0;
                while (++sp->currow < sp->maxrows - 1 && row_hidden(sp, sp->currow))
                    continue;
            } else {
                error("At end of table");
                arg = 0;
                break;
            }
        } while (col_hidden(sp, sp->curcol) || !valid_cell(sp, sp->currow, sp->curcol));
    }
}

/* moves curcol backward to the previous cell, wrapping at 0 */
void backcell(sheet_t *sp, int arg) {
    while (arg --> 0) {
        do {
            if (sp->curcol) {
                sp->curcol--;
            } else
            if (sp->currow) {
                sp->curcol = sp->maxcols - 1;
                while (--sp->currow && row_hidden(sp, sp->currow))
                    continue;
            } else {
                error("At start of table");
                arg = 0;
                break;
            }
        } while (col_hidden(sp, sp->curcol) || !valid_cell(sp, sp->currow, sp->curcol));
    }
}

/* moves curcol forward arg displayed columns */
void forwcol(sheet_t *sp, int arg) {
    int col = sp->curcol;
    while (arg > 0) {
        if (col >= ABSMAXCOLS) {
            error("At max col");
            return;
        }
        col++;
        if (!col_hidden(sp, col)) {
            sp->curcol = col;
            arg--;
        }
    }
}

/* moves curcol back arg displayed columns */
void backcol(sheet_t *sp, int arg) {
    int col = sp->curcol;
    while (arg > 0) {
        if (col <= 0) {
            error("At column A");
            break;
        }
        col--;
        if (!col_hidden(sp, col)) {
            sp->curcol = col;
            arg--;
        }
    }
}

/* moves currow forward arg displayed rows */
void forwrow(sheet_t *sp, int arg) {
    int row = sp->currow;
    while (arg > 0) {
        if (row >= ABSMAXROWS) {
            error("At max row");
            return;
        }
        row++;
        if (!row_hidden(sp, row)) {
            sp->currow = row;
            arg--;
        }
    }
}

/* moves currow backward arg displayed rows */
void backrow(sheet_t *sp, int arg) {
    int row = sp->currow;
    while (arg > 0) {
        if (row <= 0) {
            error("At min row");
            break;
        }
        row--;
        if (!row_hidden(sp, row)) {
            sp->currow = row;
            arg--;
        }
    }
}

void gotonote(sheet_t *sp) {
    struct ent *p = getcell(sp, sp->currow, sp->curcol);
    if (p && (p->flags & HAS_NOTE)) {
        struct note *n = note_find(sp, cellref(sp->currow, sp->curcol));
        if (n) {
            if (!n->str) {
                // XXX: what if target is hidden?
                moveto(sp, n->rr, cellref(-1, -1));
            } else {
                error("No note target range");
            }
        } else {
            error("Note not found");
        }
    } else {
        error("No note attached");
    }
}

/* If save is 0, remember the current position.  Otherwise, if the current
 * cell has changed since the last remember(sp, 0), save the remembered location
 * for the `, ', and c comands.
 */
// XXX: move out of vi.c (maybe navigate.c ?)
void remember(sheet_t *sp, int save) {
    if (loading)
        return;
    if (save) {
        if (sp->currow != sp->remrow || sp->curcol != sp->remcol
        ||  sp->strow != sp->remstrow || sp->stcol != sp->remstcol) {
            sp->savedcr[0] = cellref(sp->remrow, sp->remcol);
            sp->savedst[0] = cellref(sp->remstrow, sp->remstcol);
        }
    } else {
        sp->remrow = sp->currow;
        sp->remcol = sp->curcol;
        sp->remstrow = sp->strow;
        sp->remstcol = sp->stcol;
    }
}

void gohome(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (cell_in_range(cellref(sp->currow, sp->curcol), fr->irr)
        &&  (sp->currow > fr->irr.left.row || sp->curcol > fr->irr.left.col)) {
            sp->currow = fr->irr.left.row;
            sp->curcol = fr->irr.left.col;
        } else
        if (sp->currow > fr->orr.left.row || sp->curcol > fr->orr.left.col) {
            sp->currow = fr->orr.left.row;
            sp->curcol = fr->orr.left.col;
        } else {
            sp->currow = 0;
            sp->curcol = 0;
        }
    } else {
        sp->currow = 0;
        sp->curcol = 0;
    }
    remember(sp, 1);
    FullUpdate++;
}

void leftlimit(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->currow >= fr->irr.left.row && sp->currow <= fr->irr.right.row &&
            sp->curcol > fr->irr.left.col && sp->curcol <= fr->irr.right.col)
            sp->curcol = fr->irr.left.col;
        else
        if (sp->curcol > fr->orr.left.col)
            sp->curcol = fr->orr.left.col;
        else
            sp->curcol = 0;
    } else {
        sp->curcol = 0;
    }
    remember(sp, 1);
}

void rightlimit(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->currow >= fr->irr.left.row && sp->currow <= fr->irr.right.row &&
            sp->curcol >= fr->irr.left.col && sp->curcol < fr->irr.right.col)
            sp->curcol = fr->irr.right.col;
        else
        if (sp->curcol >= fr->orr.left.col && sp->curcol < fr->orr.right.col)
            sp->curcol = fr->orr.right.col;
        else {
            sp->curcol = sp->maxcol;
            while (!valid_cell(sp, sp->currow, sp->curcol) && sp->curcol > fr->orr.right.col)
                sp->curcol--;
            if ((fr = frange_get_current(sp)))
                sp->curcol = fr->orr.right.col;
        }
    } else {
        sp->curcol = sp->maxcol;
        while (!valid_cell(sp, sp->currow, sp->curcol) && sp->curcol > 0)
            sp->curcol--;
        if ((fr = frange_get_current(sp)))
            sp->curcol = fr->orr.right.col;
    }
    remember(sp, 1);
}

void gototop(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->curcol >= fr->irr.left.col && sp->curcol <= fr->irr.right.col &&
            sp->currow > fr->irr.left.row && sp->currow <= fr->irr.right.row)
            sp->currow = fr->irr.left.row;
        else
        if (sp->currow > fr->orr.left.row)
            sp->currow = fr->orr.left.row;
        else
            sp->currow = 0;
    } else {
        sp->currow = 0;
    }
    remember(sp, 1);
}

void gotobottom(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->curcol >= fr->irr.left.col && sp->curcol <= fr->irr.right.col &&
            sp->currow >= fr->irr.left.row && sp->currow < fr->irr.right.row)
            sp->currow = fr->irr.right.row;
        else
        if (sp->currow < fr->orr.right.row)
            sp->currow = fr->orr.right.row;
        else {
            sp->currow = sp->maxrow;
            while (!valid_cell(sp, sp->currow, sp->curcol) && sp->currow > fr->orr.right.row)
                sp->currow--;
            if ((fr = frange_get_current(sp)))
                sp->currow = fr->orr.right.row;
        }
    } else {
        sp->currow = sp->maxrow;
        while (!valid_cell(sp, sp->currow, sp->curcol) && sp->currow > 0)
            sp->currow--;
        if ((fr = frange_get_current(sp)))
            sp->currow = fr->orr.right.row;
    }
    remember(sp, 1);
}

void scroll_down(sheet_t *sp) {
    sp->strow++;
    // XXX: check maximum row?
    while (row_hidden(sp, sp->strow))
        sp->strow++;
    if (sp->currow < sp->strow)
        sp->currow = sp->strow;
}

void scroll_up(sheet_t *sp, int x) {
    if (sp->strow) {
        sp->strow--;
        while (sp->strow && row_hidden(sp, sp->strow))
            sp->strow--;
    }
    forwrow(sp, x);
    if (sp->currow >= lastendrow)
        backrow(sp, 1);
    backrow(sp, x);
}
