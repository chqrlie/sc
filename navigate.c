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
        num_search(sp, gs.g_type, gs.g_rr, gs.g_n);
        break;
    case G_STR:
    case G_NSTR:
    case G_XSTR:
        str_search(sp, gs.g_type, gs.g_rr, string_dup(gs.g_s));
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
    if (!loading && rr.left.row != -1 && (rr.left.row != sp->currow || rr.left.col != sp->curcol))
        remember(sp, 0);

    //lookat(sp, rr.left.row, rr.left.col);
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

/*
 * 'goto' either a given number, 'error', or 'invalid' starting at currow/curcol
 */
//int do_search(int g_type, rangeref_t rr, ... {double n or SCXMEM char *str})

int num_search(sheet_t *sp, int g_type, rangeref_t rr, double n) {
    struct ent *p;
    int firstrow, firstcol, lastrow, lastcol;
    int row, col, endr, endc;
    int found = 0;
    int errsearch = 0;

    if (!loading)
        remember(sp, 0);

    // XXX: refine this, find all errors for now
    if (g_type == G_ERROR || g_type == G_INVALID)
        errsearch = -1;

    go_free(sp);
    gs.g_type = g_type;
    gs.g_rr = rr;
    gs.g_n = n;

    firstrow = rr.left.row;
    firstcol = rr.left.col;
    lastrow = rr.right.row;
    lastcol = rr.right.col;

    if (sp->currow >= firstrow && sp->currow <= lastrow &&
        sp->curcol >= firstcol && sp->curcol <= lastcol) {
        endr = sp->currow;
        endc = sp->curcol;
    } else {
        endr = lastrow;
        endc = lastcol;
    }
    row = endr;
    col = endc;
    for (;;) {
        if (col++ >= lastcol) {
            col = firstcol;
            if (row++ >= lastrow) {
                row = firstrow;
            }
        }
        if (!row_hidden(sp, row) && !col_hidden(sp, col) && (p = getcell(sp, row, col))) {
            if (errsearch) {
                if (errsearch & (1 << p->cellerror)) {
                    found = 1;
                    break;
                }
            } else
            if (p->type == SC_NUMBER && p->v == n) {
                found = 1;
                break;
            }
        }
        if (row == endr && col == endc)
            break;
    }

    if (found) {
        sp->currow = row;
        sp->curcol = col;
        if (loading) {
            update(sp, 1);
            changed = 0;
        } else {
            remember(sp, 1);
        }
    } else {
        if (errsearch) {
            error("no ERROR cell found");
        } else {
            error("Number not found");
        }
    }
    return found;
}

/* 'goto' a cell containing a matching string */
int str_search(sheet_t *sp, int g_type, rangeref_t rr, SCXMEM string_t *str) {
    char field[FBUFLEN];
    struct ent *p;
    int firstrow, firstcol, lastrow, lastcol;
    int row, col, endr, endc;
    int found = 0;
    const char *s;
#if defined REGCOMP
    regex_t preg;
    int errcode;
#elif defined RE_COMP
    char *tmp = NULL;
#elif defined REGCMP
    char *tmp = NULL;
#else
#endif

    if (!str)
        return -1;

    s = s2c(str);

#if defined REGCOMP
    if ((errcode = regcomp(&preg, s, REG_EXTENDED))) {
        char buf[160];
        regerror(errcode, &preg, buf, sizeof(buf));
        error("%s", buf);
        string_free(str);
        return -1;
    }
#elif defined RE_COMP
    if ((tmp = re_comp(s)) != NULL) {
        error("%s", tmp);
        string_free(str);
        return -1;
    }
#elif defined REGCMP
    if ((tmp = regcmp(s, NULL)) == NULL) {
        error("Invalid search string");
        string_free(str);
        return -1;
    }
#else
    /* otherwise nothing to do, will just use strcmp() */
#endif
    if (!loading)
        remember(sp, 0);

    go_free(sp);
    gs.g_type = g_type;
    gs.g_rr = rr;
    string_set(&gs.g_s, str);

    firstrow = rr.left.row;
    firstcol = rr.left.col;
    lastrow = rr.right.row;
    lastcol = rr.right.col;

    if (sp->currow >= firstrow && sp->currow <= lastrow && sp->curcol >= firstcol && sp->curcol <= lastcol) {
        endr = sp->currow;
        endc = sp->curcol;
    } else {
        endr = lastrow;
        endc = lastcol;
    }
    row = endr;
    col = endc;
    // XXX: incorrect if firstrow or lastrow is hidden
    for (;;) {
        if (col++ >= lastcol) {
            col = firstcol;
            if (row++ >= lastrow) {
                row = firstrow;
            }
        }
        if (!row_hidden(sp, row) && !col_hidden(sp, col) && (p = getcell(sp, row, col))) {
            /* convert cell contents, do not test width, ignore alignment */
            const char *s1 = field;
            int align = ALIGN_DEFAULT;

            *field = '\0';
            if (gs.g_type == G_NSTR) {
                if (p->cellerror) {
                    s1 = error_name[p->cellerror];
                } else
                if (p->type == SC_NUMBER) {
                    if (p->format) {
                        format(field, sizeof field, s2c(p->format), sp->colfmt[col].precision, p->v, &align);
                    } else {
                        engformat(field, sizeof field, sp->colfmt[col].realfmt, sp->colfmt[col].precision, p->v, &align);
                    }
                }
            } else if (gs.g_type == G_XSTR) {
                if (p->expr) {
                    // XXX: should pass row, col as the cell reference
                    decompile(sp, field, sizeof field, p->expr, 0, 0, DCP_DEFAULT);
                    if (*field == '?')
                        *field = '\0';
                }
            }
            if (gs.g_type == G_STR && p->type == SC_STRING) {
                s1 = s2str(p->label);
            }
            if (s1 && *s1
#if defined REGCOMP
            &&  (regexec(&preg, s1, 0, NULL, 0) == 0)
#elif defined RE_COMP
            &&  (re_exec(s1) != 0)
#elif defined REGCMP
            &&  (regex(tmp, s1) != NULL)
#else
            &&  (strcmp(s, s1) == 0)
#endif
                ) {
                found = 1;
                break;
            }
        }
        if (row == endr && col == endc)
            break;
    }
#if defined REGCOMP
    regfree(&preg);
#elif defined RE_COMP
#elif defined REGCMP
    free(tmp);
#else
#endif
    if (found) {
        sp->currow = row;
        sp->curcol = col;
        if (loading) {
            update(sp, 1);
            changed = 0;
        } else {
            remember(sp, 1);
        }
    } else {
        error("String not found");
    }
    return found;
}

void doend(sheet_t *sp, int rowinc, int colinc) {
    int r, c;

    if (!loading)
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
        if (!loading)
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
void forwpage(sheet_t *sp, int arg) {
    int ps = sp->pagesize ? sp->pagesize : (screen_LINES - RESROW - framerows) / 2;
    forwrow(sp, arg * ps);
    // XXX: hidden row issue
    sp->strow = sp->strow + arg * ps;
    FullUpdate++;
}

/* moves currow up one page */
void backpage(sheet_t *sp, int arg) {
    int ps = sp->pagesize ? sp->pagesize : (screen_LINES - RESROW - framerows) / 2;
    backrow(sp, arg * ps);
    // XXX: hidden row issue
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

/* moves curcol forward one displayed column */
void forwcol(sheet_t *sp, int arg) {
    while (arg --> 0) {
        if (sp->curcol < sp->maxcols - 1)
            sp->curcol++;
        else
        if (!growtbl(sp, GROWCOL, 0, arg))  /* get as much as needed */
            break;
        else
            sp->curcol++;
        while (col_hidden(sp, sp->curcol) && (sp->curcol < sp->maxcols - 1))
            sp->curcol++;
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

/* moves curcol back one displayed column */
void backcol(sheet_t *sp, int arg) {
    while (arg --> 0) {
        if (!sp->curcol) {
            error("At column A");
            break;
        }
        sp->curcol--;
        while (sp->curcol && col_hidden(sp, sp->curcol))
            sp->curcol--;
    }
}

/* moves currow forward one displayed row */
void forwrow(sheet_t *sp, int arg) {
    while (arg --> 0) {
        if (sp->currow < sp->maxrows - 1)
            sp->currow++;
        else
        if (!growtbl(sp, GROWROW, sp->maxrows + arg, 0))  /* get as much as needed */
            break;
        else
            sp->currow++;
        while (row_hidden(sp, sp->currow) && (sp->currow < sp->maxrows - 1))
            sp->currow++;
    }
}

/* moves currow backward one displayed row */
void backrow(sheet_t *sp, int arg) {
    while (arg --> 0) {
        if (!sp->currow) {
            error("At row zero");
            break;
        }
        sp->currow--;
        while (sp->currow && row_hidden(sp, sp->currow))
            sp->currow--;
    }
}

void gotonote(sheet_t *sp) {
    struct ent *p = getcell(sp, sp->currow, sp->curcol);
    if (p && (p->flags & HAS_NOTE)) {
        struct note *n = note_find(sp, cellref(sp->currow, sp->curcol));
        if (n) {
            if (!n->str) {
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
