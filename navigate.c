/*      SC      A Spreadsheet Calculator
 *              Navigation routines
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

/* Use this structure to save the last 'g' command */
struct go_save gs;

/* g_type can be: */
#define G_NONE 0        /* Starting value - must be 0 */
#define G_NUM 1
#define G_STR 2
#define G_NSTR 3
#define G_XSTR 4
#define G_CELL 5

/* Goto subroutines */

static void g_free(void) {
    switch (gs.g_type) {
    case G_STR:
    case G_NSTR: set_string(&gs.g_s, NULL); break;
    default: break;
    }
    gs.g_type = G_NONE;
    gs.errsearch = 0;
}

/* repeat the last goto command */
void go_last(void) {
    int num = 0;

    switch (gs.g_type) {
    case G_NONE:
        error("Nothing to repeat");
        break;
    case G_NUM:
        num_search(gs.g_n, gs.g_rr, gs.errsearch);
        break;
    case G_CELL:
        moveto(gs.g_rr, gs.st);
        break;
    case G_XSTR:
        num++;
        FALLTHROUGH;
    case G_NSTR:
        num++;
        FALLTHROUGH;
    case G_STR:
        str_search(dup_string(gs.g_s), gs.g_rr, num);
        break;

    default:
        error("go_last: internal error");
        break;
    }
}

/* Place the cursor on a given cell.  If st.row >= 0, place the cell
 * at row st.row and column st.col in the upper left corner of the
 * screen if possible.
 */
void moveto(rangeref_t rr, cellref_t st) {
    if (!loading && rr.left.row != -1 && (rr.left.row != currow || rr.left.col != curcol))
        remember(0);

    lookat(rr.left.row, rr.left.col);
    currow = rr.left.row;
    curcol = rr.left.col;
    g_free();
    gs.g_type = G_CELL;
    gs.g_rr = rr;
    gs.st = st;
    if (st.row >= 0) {
        strow = st.row;
        stcol = st.col;
        gs.stflag = 1;
    } else {
        gs.stflag = 0;
    }
    rowsinrange = rows_height(rr.left.row, rr.right.row - rr.left.row + 1);
    colsinrange = cols_width(rr.left.col, rr.right.col - rr.left.col + 1);

    FullUpdate++;
    if (loading) {
        update(1);
        changed = 0;
    } else {
        remember(1);
    }
}

/*
 * 'goto' either a given number,'error', or 'invalid' starting at currow,curcol
 */
void num_search(double n, rangeref_t rr, int errsearch) {
    struct ent *p;
    int firstrow, firstcol, lastrow, lastcol;
    int r, c, endr, endc;

    if (!loading)
        remember(0);

    g_free();
    gs.g_type = G_NUM;
    gs.g_n = n;
    gs.g_rr = rr;
    gs.errsearch = errsearch;

    firstrow = rr.left.row;
    firstcol = rr.left.col;
    lastrow = rr.right.row;
    lastcol = rr.right.col;

    if (currow >= firstrow && currow <= lastrow &&
            curcol >= firstcol && curcol <= lastcol) {
        endr = currow;
        endc = curcol;
    } else {
        endr = lastrow;
        endc = lastcol;
    }
    r = endr;
    c = endc;
    while (1) {
        if (c < lastcol) {
            c++;
        } else {
            if (r < lastrow) {
                while (++r < lastrow && row_hidden[r])
                    continue;
                c = firstcol;
            } else {
                r = firstrow;
                c = firstcol;
            }
        }
        p = *ATBL(tbl, r, c);
        if (!col_hidden[c] && p && (p->flags & IS_VALID) &&
                (errsearch || (p->v == n)) && (!errsearch ||
                (p->cellerror == errsearch)))   /* CELLERROR vs CELLINVALID */
            break;
        if (r == endr && c == endc) {
            if (errsearch) {
                error("no %s cell found", errsearch == CELLERROR ? "ERROR" :
                      "INVALID");
            } else {
                error("Number not found");
            }
            return;
        }
    }

    currow = r;
    curcol = c;
    if (loading) {
        update(1);
        changed = 0;
    } else {
        remember(1);
    }
}

/* 'goto' a cell containing a matching string */
void str_search(SCXMEM string_t *str, rangeref_t rr, int num) {
    char field[FBUFLEN];
    struct ent *p;
    int found = 0;
    int firstrow, firstcol, lastrow, lastcol;
    int row, col, endr, endc;
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
        return;

    s = s2c(str);

#if defined REGCOMP
    if ((errcode = regcomp(&preg, s, REG_EXTENDED))) {
        char buf[160];
        regerror(errcode, &preg, buf, sizeof(buf));
        error("%s", buf);
        free_string(str);
        return;
    }
#elif defined RE_COMP
    if ((tmp = re_comp(s)) != NULL) {
        error("%s", tmp);
        free_string(str);
        return;
    }
#elif defined REGCMP
    if ((tmp = regcmp(s, NULL)) == NULL) {
        cellerror = CELLERROR;
        error("Invalid search string");
        free_string(str);
        return;
    }
#else
    /* otherwise nothing to do, will just use strcmp() */
#endif
    if (!loading)
        remember(0);

    g_free();
    gs.g_type = G_STR + num;
    set_string(&gs.g_s, str);
    gs.g_rr = rr;
    firstrow = rr.left.row;
    firstcol = rr.left.col;
    lastrow = rr.right.row;
    lastcol = rr.right.col;
    if (currow >= firstrow && currow <= lastrow &&
            curcol >= firstcol && curcol <= lastcol) {
        endr = currow;
        endc = curcol;
    } else {
        endr = lastrow;
        endc = lastcol;
    }
    row = endr;
    col = endc;
    for (;;) {
        if (col < lastcol) {
            col++;
        } else {
            col = firstcol;
            if (row < lastrow) {
                while (++row < lastrow && row_hidden[row])
                    continue;
            } else {
                row = firstrow;
            }
        }
        if (!col_hidden[col] && (p = *ATBL(tbl, row, col))) {
            /* convert cell contents, do not test width, ignore alignment */
            const char *s1 = field;
            int align = ALIGN_DEFAULT;

            *field = '\0';
            if (gs.g_type == G_NSTR) {
                if (p->cellerror) {
                    s1 = (p->cellerror == CELLERROR) ? "ERROR" : "INVALID";
                } else
                if (p->flags & IS_VALID) {
                    if (p->format) {
                        format(field, sizeof field, s2c(p->format), precision[col], p->v, &align);
                    } else {
                        engformat(field, sizeof field, realfmt[col], precision[col], p->v, &align);
                    }
                }
            } else if (gs.g_type == G_XSTR) {
                if (p->expr) {
                    // XXX: should pass row, col as the cell reference
                    decompile(field, sizeof field, p->expr, 0, 0, DCP_DEFAULT);
                    if (*field == '?')
                        *field = '\0';
                }
            }
            if (gs.g_type == G_STR) {
                s1 = s2c(p->label);
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
        currow = row;
        curcol = col;
        if (loading) {
            update(1);
            changed = 0;
        } else {
            remember(1);
        }
    } else {
        error("String not found");
    }
}

void doend(int rowinc, int colinc) {
    struct ent *p;
    int r, c;

    if (!loading)
        remember(0);

    if (VALID_CELL(p, currow, curcol)) {
        r = currow + rowinc;
        c = curcol + colinc;
        if (r >= 0 && r < maxrows &&
            c >= 0 && c < maxcols &&
            !VALID_CELL(p, r, c)) {
                currow = r;
                curcol = c;
        }
    }

    if (!VALID_CELL(p, currow, curcol)) {
        switch (rowinc) {
        case -1:
            while (!VALID_CELL(p, currow, curcol) && currow > 0)
                currow--;
            break;
        case  1:
            while (!VALID_CELL(p, currow, curcol) && currow < maxrows - 1)
                currow++;
            break;
        case  0:
            switch (colinc) {
            case -1:
                while (!VALID_CELL(p, currow, curcol) && curcol > 0)
                    curcol--;
                break;
            case  1:
                while (!VALID_CELL(p, currow, curcol) && curcol < maxcols - 1)
                    curcol++;
                break;
            }
            break;
        }
        if (!loading)
            remember(1);
        return;
    }

    switch (rowinc) {
    case -1:
        while (VALID_CELL(p, currow, curcol) && currow > 0)
            currow--;
        break;
    case  1:
        while (VALID_CELL(p, currow, curcol) && currow < maxrows - 1)
            currow++;
        break;
    case  0:
        switch (colinc) {
        case -1:
            while (VALID_CELL(p, currow, curcol) && curcol > 0)
                curcol--;
            break;
        case  1:
            while (VALID_CELL(p, currow, curcol) && curcol < maxcols - 1)
                curcol++;
            break;
        }
        break;
    }
    if (!VALID_CELL(p, currow, curcol)) {
        // XXX: this is bogus if already on maxcol or maxrow
        currow -= rowinc;
        curcol -= colinc;
    }
}

/* moves currow down one page */
void forwpage(int arg) {
    int ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
    forwrow(arg * ps);
    // XXX: hidden row issue
    strow = strow + arg * ps;
    FullUpdate++;
}

/* moves currow up one page */
void backpage(int arg) {
    int ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
    backrow(arg * ps);
    // XXX: hidden row issue
    strow = strow - arg * ps;
    if (strow < 0) strow = 0;
    FullUpdate++;
}

/* moves curcol forward to the next cell, wrapping at maxcols - 1 */
void forwcell(int arg) {
    struct ent *p;
    while (arg --> 0) {
        do {
            if (curcol < maxcols - 1) {
                curcol++;
            } else
            if (currow < maxrows - 1) {
                curcol = 0;
                while (++currow < maxrows - 1 && row_hidden[currow])
                    continue;
            } else {
                error("At end of table");
                arg = 0;
                break;
            }
        } while (col_hidden[curcol] || !VALID_CELL(p, currow, curcol));
    }
}

/* moves curcol forward one displayed column */
void forwcol(int arg) {
    while (arg --> 0) {
        if (curcol < maxcols - 1)
            curcol++;
        else
        if (!growtbl(GROWCOL, 0, arg))  /* get as much as needed */
            break;
        else
            curcol++;
        while (col_hidden[curcol] && (curcol < maxcols - 1))
            curcol++;
    }
}

/* moves curcol backward to the previous cell, wrapping at 0 */
void backcell(int arg) {
    struct ent *p;
    while (arg --> 0) {
        do {
            if (curcol) {
                curcol--;
            } else
            if (currow) {
                curcol = maxcols - 1;
                while (--currow && row_hidden[currow])
                    continue;
            } else {
                error("At start of table");
                arg = 0;
                break;
            }
        } while (col_hidden[curcol] || !VALID_CELL(p, currow, curcol));
    }
}

/* moves curcol back one displayed column */
void backcol(int arg) {
    while (arg --> 0) {
        if (!curcol) {
            error("At column A");
            break;
        }
        curcol--;
        while (curcol && col_hidden[curcol])
            curcol--;
    }
}

/* moves currow forward one displayed row */
void forwrow(int arg) {
    while (arg --> 0) {
        if (currow < maxrows - 1)
            currow++;
        else
        if (!growtbl(GROWROW, maxrows + arg, 0))  /* get as much as needed */
            break;
        else
            currow++;
        while (row_hidden[currow] && (currow < maxrows - 1))
            currow++;
    }
}

/* moves currow backward one displayed row */
void backrow(int arg) {
    while (arg --> 0) {
        if (!currow) {
            error("At row zero");
            break;
        }
        currow--;
        while (currow && row_hidden[currow])
            currow--;
    }
}

void gotonote(void) {
    struct ent *p;

    p = lookat(currow, curcol);
    if (p->flags & HAS_NOTE) {
        moveto(p->nrr, cellref(-1, -1));
    } else {
        error("No note attached");
    }
}
