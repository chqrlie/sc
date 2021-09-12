/*      SC      A Spreadsheet Calculator
 *              Curses based Screen driver
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <time.h>
#include "icurses.h"
#include "sc.h"

static char under_cursor = ' '; /* Data under the < cursor */
char mode_ind = 'i';
char search_ind = ' ';

static int lines, cols, rows;
int lcols;
static int lastmx, lastmy; /* Screen address of the cursor */
static int sc_lastrow, sc_lastcol; /* Spreadsheet row/col the cursor was in last */
int lastendrow = -1;        /* Last bottom row of screen */
static int lastftoprows = 0;       /* Rows in top of frame cursor was in last */
static int lastfbottomrows = 0;    /* Rows in bottom of frame cursor was in last */
static int lastfleftcols = 0;      /* Columns in left side of frame cursor was
                                    in last */
static int lastfrightcols = 0;
struct frange *lastfr = 0;     /* Last framed range we were in */
static sc_bool_t frTooLarge = 0; /* If set, either too many rows or too many columns
                        exist in frame to allow room for the scrolling
                        portion of the framed range */
#ifdef RIGHT_CBUG
static int wasforw = FALSE;  /* Causes screen to be redisplay if on lastcol */
#endif
int framerows;      /* Rows in current frame */
int framecols;      /* Columns in current frame */
int screen_COLS = 80, screen_LINES = 25;
int seenerr;
static int standlast = FALSE;
#ifdef XENIX2_3
struct termio tmio;
#endif

static void repaint(int x, int y, int len, int attron, int attroff);
static void showstring(sheet_t *sp, const char *string, int align, int hasvalue, int row,
                       int col, int *nextcolp, int mxcol, int *fieldlenp, int r, int c,
                       struct frange *fr, int frightcols, int flcols, int frcols);

static int addustr(const char *s) {
    int i, pos, res = OK, wc;
    for (i = pos = 0;;) {
        unsigned char c = s[i];
        if (c > 0 && !(c & 0x80)) {
            i++;
        } else {
            res |= addnstr(s + pos, i - pos);
            if (!c)
                break;
            pos = i += utf8_decode(s + pos, &wc);
            res |= addnwstr(&wc, 1);
        }
    }
    return res;
}

static int addqstr(const char *s) {
    int i, pos, res = OK, wc;
    for (i = pos = 0;;) {
        unsigned char c = s[i];
        if (c > 0 && !(c & 0x80) && c != '"' && c != '\\') {
            i++;
        } else {
            res |= addnstr(s + pos, i - pos);
            if (!c)
                break;
            if (c < 0x80) {
                pos = i++;
                /* only escape `"`, `\"` and `\\` */
                if (c != '\\' || s[i] == '\\' || s[i] == '"')
                    res |= addch('\\');
            } else {
                pos = i += utf8_decode(s + pos, &wc);
                res |= addnwstr(&wc, 1);
            }
        }
    }
    return res;
}

void error(const char *fmt, ...) {
    char buf[256];
    va_list ap;

    va_start(ap, fmt);
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    ((int (*)(char *, size_t, const char *, va_list))vsnprintf)(buf, sizeof buf, fmt, ap);
    va_end(ap);

    if (usecurses) {
        /* seenerr is cleared in vi_interaction().
           show only a single error per input cycle. */
        if (seenerr) return;
        seenerr++;

        select_style(STYLE_CELL, 0);
        screen_clear_line(1);
        // XXX: should clip message to screen width
        addustr(buf);
    } else {
        if (*buf) fprintf(stderr, "%s\n", buf);
    }
}

void select_style(int n, int rev) {
    if (usecurses) {
        if (rev) {
            if (rev > 0)
                standout();
            else
                standend();
        }
        if (color && has_colors()) {
            attron(COLOR_PAIR(n));
        }
    }
}

int rows_height(sheet_t *sp, int r, int n) {
    int height = 0;
    while (n --> 0 && r < sp->maxrows) {
        if (!row_hidden(sp, r))
            height += 1;
        r++;
    }
    return height;
}

int cols_width(sheet_t *sp, int c, int n) {
    int width = 0;
    while (n --> 0 && c < sp->maxcols) {
        if (!col_hidden(sp, c))
            width += col_fwidth(sp, c);
        c++;
    }
    return width;
}

/* update() does general screen update */
void update(sheet_t *sp, int anychanged) {          /* did any cell really change in value? */
    char field[FBUFLEN];
    int row, col;
    struct ent *p;
    int mxrow, mxcol;
    int minsr = 0, minsc = 0, maxsr = 0, maxsc = 0;
    int r, i;
    struct frange *fr;
    struct crange *cr;
    int ftoprows, fbottomrows, fleftcols, frightcols;
    int ftrows, fbrows, flcols, frcols;
    sc_bool_t message;

    /*
     * If receiving input from a pipeline, don't display spreadsheet data
     * on screen.
     */
    if (!usecurses) return;

    // XXX: this code is a mess. Should at least document if not simplify
    getmaxyx(stdscr, lines, cols);
    fr = lastfr;
    if (!(fr && fr->or_left->row <= sp->currow &&   /* If we've left the        */
          fr->or_left->col <= sp->curcol &&       /* previous framed range... */
          fr->or_right->row >= sp->currow &&
          fr->or_right->col >= sp->curcol)) {
        fr = frange_get_current(sp);
        if (fr != lastfr)
            FullUpdate++;
    }
    ftoprows = fbottomrows = fleftcols = frightcols = 0;
    ftrows = fbrows = flcols = frcols = 0;
    framerows = framecols = 0;
    if (fr) {
        ftoprows = fr->ir_left->row - fr->or_left->row;
        ftrows = rows_height(sp, fr->or_left->row, ftoprows);
        fbottomrows = fr->or_right->row - fr->ir_right->row;
        fbrows = rows_height(sp, fr->ir_right->row + 1, fbottomrows);
        fleftcols = fr->ir_left->col - fr->or_left->col;
        flcols = cols_width(sp, fr->or_left->col, fleftcols);
        frightcols = fr->or_right->col - fr->ir_right->col;
        frcols = cols_width(sp, fr->ir_right->col + 1, frightcols);
        framerows = RESROW + ftrows + fbrows;
        framecols = sp->rescol + flcols + frcols;
        if (framerows >= lines || framecols >= cols) {
            frTooLarge = TRUE;
            if (FullUpdate) {
                error("Frame too large for screen size - ignoring");
            }
            ftoprows = fbottomrows = fleftcols = frightcols = 0;
            ftrows = fbrows = flcols = frcols = 0;
            framerows = framecols = 0;
            sp->strow -= lastftoprows;
            sp->stcol -= lastfleftcols;
        } else {
            frTooLarge = FALSE;
            if (sp->strow >= fr->or_left->row) {
                if (fr == lastfr && sp->strow < fr->or_left->row + ftoprows)
                    sp->strow = fr->or_left->row;
                else if (sp->strow > fr->ir_right->row) {
                    sp->strow = fr->ir_right->row;
                    FullUpdate++;
                }
            }
            if (sp->stcol >= fr->or_left->col) {
                if (sp->stcol < fr->or_left->col + fleftcols)
                    sp->stcol = fr->or_left->col;
                else if (sp->stcol > fr->ir_right->col) {
                    sp->stcol = fr->ir_right->col;
                    FullUpdate++;
                }
            }
            if (fr == lastfr && sp->currow == sc_lastrow)
                fbottomrows = lastfbottomrows;
            else if (sp->currow < fr->ir_right->row)
                fbottomrows = fr->or_right->row - fr->ir_right->row;
            else
                fbottomrows = fr->or_right->row - sp->currow;
            if (fr == lastfr && sp->curcol == sc_lastcol)
                frightcols = lastfrightcols;
            else if (sp->curcol < fr->ir_right->col)
                frightcols = fr->or_right->col - fr->ir_right->col;
            else
                frightcols = fr->or_right->col - sp->curcol;
        }
        fbrows = rows_height(sp, fr->ir_right->row + 1, fbottomrows);
        frcols = cols_width(sp, fr->ir_right->col + 1, frightcols);
    }
    if (fr != lastfr && !gs.stflag && lastfr) {
        if (sp->strow >= lastfr->ir_left->row)
            sp->strow -= lastftoprows;
        if (sp->stcol >= lastfr->ir_left->col)
            sp->stcol -= lastfleftcols;
    }

    /*
     * Place the cursor on the screen.  Set col, sp->curcol, stcol, sc_lastcol as
     * needed.  If strow and stcol are negative, centering is forced.
     */
    if ((sp->curcol != sc_lastcol) || FullUpdate) {
        // XXX: update should not have a side effect on sp->currow/sp->curcol
        while (col_hidden(sp, sp->curcol))   /* You can't hide the last row or col */
            sp->curcol++;
        if (col_fwidth(sp, sp->curcol) > cols - sp->rescol - 2) {
            // XXX: update should not have a side effect on the database
            error("column %s too wide - resizing", coltoa(sp->curcol));
            cmd_format(sp, sp->curcol, sp->curcol, cols - sp->rescol - 2,
                       sp->colfmt[sp->curcol].precision,
                       sp->colfmt[sp->curcol].realfmt);
        }

        /* First see if the last display still covers sp->curcol */
        if (sp->stcol >= 0 && sp->stcol <= sp->curcol) {
            int c = 0;

            if (fr) {
                if (fr != lastfr) {
                    if (sp->stcol == fr->or_left->col)
                        sp->stcol += fleftcols;
                    else if (sp->stcol >= fr->or_left->col && !gs.stflag) {
                        sp->stcol += fleftcols;
                        if (sp->stcol > fr->ir_right->col)
                            sp->stcol = fr->ir_right->col + 1;
                    }
                } else if (sp->stcol == fr->or_left->col)
                    sp->stcol += fleftcols;
            }
            i = sp->stcol;
            lcols = 0;
            col = sp->rescol + frcols;
            if (fr && sp->stcol >= fr->or_left->col) {
                if (sp->stcol < fr->ir_left->col)
                    i = fr->or_left->col;
                else
                    col += flcols;
            }
            for (; i < sp->maxcols &&
                    (col + col_fwidth(sp, i) < cols - 1 || col_hidden(sp, i) || i < sp->curcol);
                 i++) {
                lcols++;
                if (fr && i == fr->ir_right->col + 1) {
                    col -= frcols;
                    frcols = frightcols = 0;
                }
                if (col_hidden(sp, i))
                    continue;

                /* If there isn't room for more columns, and we haven't yet
                 * reached the current column, start removing columns from
                 * the left.
                 */
                while (col + col_fwidth(sp, i) > cols - 2) {
                    lcols--;
                    col -= col_fwidth(sp, sp->stcol);
                    while (col_hidden(sp, ++sp->stcol))
                        continue;
                    FullUpdate++;
                    c++;
                }
                col += col_fwidth(sp, i);
            }
            if (!frTooLarge && fr && sp->curcol <= sp->stcol + lcols &&
                    fr->ir_left->col >= sp->stcol + lcols) {
                while (sp->stcol + lcols < fr->ir_left->col) {
                    col -= col_fwidth(sp, sp->stcol);
                    lcols--;
                    while (col_hidden(sp, ++sp->stcol))
                        lcols--;
                    while (col + col_fwidth(sp, sp->stcol + lcols) < cols - 1) {
                        col += col_fwidth(sp, sp->stcol + lcols);
                        lcols++;
                    }
                }
            } else if (c)
                sp->stcol = -1;
        }

        if (!rowsinrange) rowsinrange = 1;
        if (!colsinrange) colsinrange = col_fwidth(sp, sp->curcol);

        // XXX: should compute goto target area and shift screen?
        while (sp->stcol < 0 || sp->curcol < sp->stcol || sp->stcol + lcols - 1 < sp->curcol ||
                (colsinrange != col_fwidth(sp, sp->curcol) && sp->stcol != sp->curcol &&
                sp->stcol + lcols - 1 < gs.g_rr.right.col)) {

            FullUpdate++;

                /* How about back one? */
            if (sp->stcol - 1 == sp->curcol) {
                sp->stcol--;
                /* Forward one? */
            } else if (sp->stcol >= 0 && sp->stcol + lcols == sp->curcol) {
                sp->stcol++;
            } else if (sp->stcol >= 0 && fr && sp->curcol >= fr->or_left->col &&
                       sp->curcol <= fr->ir_left->col && sp->stcol < sp->curcol &&
                       sp->curcol <= sp->stcol + lcols + fr->ir_left->col -
                            fr->or_left->col) {
                while ((sp->stcol + lcols < fr->ir_left->col && !frTooLarge) ||
                        (colsinrange != col_fwidth(sp, sp->curcol) && sp->stcol != sp->curcol &&
                        sp->stcol + lcols - 1 < gs.g_rr.right.col)) {
                    if (col_hidden(sp, ++sp->stcol)) lcols--;
                }
            } else {
                /* Try to put the cursor in the center of the screen.
                 * If we've just jumped to a range using the goto command,
                 * center the range instead.
                 */
                if (colsinrange > cols - sp->rescol - flcols - frcols - 2)
                    colsinrange = cols - sp->rescol - flcols - frcols - 2;
                col = (cols - sp->rescol - flcols - frcols - colsinrange) / 2;
                sp->stcol = sp->curcol;
                for (i = sp->curcol - 1;
                     i >= (fr ? fr->or_left->col + fleftcols : 0) &&
                         (col - col_fwidth(sp, i) > 0 || col_hidden(sp, i));
                     i--) {
                    sp->stcol--;
                    if (col_hidden(sp, i))
                        continue;
                    col -= col_fwidth(sp, i);
                }
                if (fr && sp->stcol < fr->or_left->col + fleftcols) {
                    sp->stcol = fr->or_left->col + fleftcols;
                    if (sp->curcol < sp->stcol)
                        sp->stcol = sp->curcol;
                }
            }
            /* Now pick up the counts again */
            i = sp->stcol;
            lcols = 0;
            col = sp->rescol + frcols;
            if (fr && sp->stcol >= fr->or_left->col) {
                if (sp->stcol < fr->ir_left->col)
                    i = fr->or_left->col;
                else
                    col += flcols;
            }
            for (; i < sp->maxcols &&
                    (col + col_fwidth(sp, i) < cols-1 || col_hidden(sp, i) || i < sp->curcol);
                    i++) {
                lcols++;
                if (fr && i == fr->ir_right->col + 1) {
                    col -= frcols;
                    frcols = frightcols = 0;
                }
                if (col_hidden(sp, i))
                    continue;

                col += col_fwidth(sp, i);
            }
        }
    }
    if (fleftcols && sp->stcol >= fr->or_left->col && sp->stcol < fr->or_left->col + fleftcols) {
        lcols += (fr->or_left->col - sp->stcol);
        sp->stcol = fr->or_left->col + fleftcols;
        if (sp->curcol < sp->stcol)
            sp->stcol = sp->curcol;
    }

    /* Now - same process on the rows as the columns */
    if ((sp->currow != sc_lastrow) || FullUpdate) {
        // XXX: update should not have a side effect on sp->currow/sp->curcol
        while (row_hidden(sp, sp->currow))   /* You can't hide the last row or col */
            sp->currow++;
        if (sp->strow >= 0 && sp->strow <= sp->currow) {
            int c = 0;

            if (fr) {
                if (fr != lastfr) {
                    if (sp->strow == fr->or_left->row)
                        sp->strow += ftoprows;
                    else if (sp->strow >= fr->or_left->row && !gs.stflag) {
                        sp->strow += ftoprows;
                        if (sp->strow > fr->ir_right->row)
                            sp->strow = fr->ir_right->row + 1;
                    }
                } else if (sp->strow == fr->or_left->row)
                    sp->strow += ftoprows;
            }
            i = sp->strow;
            rows = 0;
            row = RESROW + fbrows;
            if (fr && sp->strow >= fr->or_left->row) {
                if (sp->strow < fr->ir_left->row)
                    i = fr->or_left->row;
                else
                    row += ftrows;
            }
            for (; (row < lines || row_hidden(sp, i) || i < sp->currow) && i < sp->maxrows; i++) {
                rows++;
                if (fr && i == fr->ir_right->row + 1) {
                    row -= fbrows;
                    fbrows = fbottomrows = 0;
                }
                if (row_hidden(sp, i))
                    continue;

                /* If there isn't room for more rows, and we haven't yet
                 * reached the current row, start removing rows from the
                 * top.
                 */
                if (row >= lines) {
                    rows--;
                    row--;
                    while (row_hidden(sp, ++sp->strow))
                        continue;
                    FullUpdate++;
                    c++;
                }
                row++;
            }
            if (!frTooLarge && fr && sp->currow <= sp->strow + rows &&
                fr->ir_left->row >= sp->strow + rows) {
                while (sp->strow + rows < fr->ir_left->row) {
                    while (row_hidden(sp, ++sp->strow))
                        continue;
                }
            } else if (c && sp->currow > lastendrow)
                sp->strow = -1;
        }

        // XXX: this code is bogus
        while (sp->strow < 0 || sp->currow < sp->strow || sp->strow + rows - 1 < sp->currow ||
               sp->strow + rows < sp->currow + rowsinrange) {

            FullUpdate++;

            /* How about up one? */
            if (sp->strow - 1 == sp->currow) {
                sp->strow--;
                /* Down one? */
            } else if (sp->strow >= 0 && sp->strow + rows == sp->currow) {
                sp->strow++;
            } else if (sp->strow >= 0 && fr && sp->currow >= fr->or_left->row &&
                       sp->currow <= fr->ir_left->row && sp->strow < sp->currow &&
                       sp->currow <= sp->strow + rows + fr->ir_left->row - fr->or_left->row) {
                while ((sp->strow + rows < fr->ir_left->row && !frTooLarge) ||
                       (rowsinrange > 1 && sp->strow != sp->currow &&
                        sp->strow + rows - 1 < gs.g_rr.right.row)) {
                    if (row_hidden(sp, ++sp->strow)) rows--;
                }
            } else {
                /* Try to put the cursor in the center of the screen.
                 * If we've just jumped to a range using the goto command,
                 * center the range instead.
                 */
                rowsinrange = (rowsinrange > lines - RESROW - ftrows - fbrows ?
                               lines - RESROW - ftrows - fbrows : rowsinrange);
                row = (lines - RESROW - ftrows - fbrows - rowsinrange) / 2;
                sp->strow = sp->currow;
                for (i = sp->currow - 1;
                     i >= (fr ? fr->or_left->row + ftoprows : 0) &&
                         (row > 0 || row_hidden(sp, i)); i--) {
                    sp->strow--;
                    if (row_hidden(sp, i))
                        continue;
                    row--;
                }
                if (fr && sp->strow < fr->or_left->row + ftoprows) {
                    sp->strow = fr->or_left->row + ftoprows;
                    if (sp->currow < sp->strow)
                        sp->strow = sp->currow;
                }
            }
            /* Now pick up the counts again */
            i = sp->strow;
            rows = 0;
            row = RESROW + fbrows;
            if (fr && sp->strow >= fr->or_left->row) {
                if (sp->strow < fr->ir_left->row)
                    i = fr->or_left->row;
                else
                    row += ftrows;
            }
            for (; (row < lines || row_hidden(sp, i) || i < sp->currow) && i < sp->maxrows; i++) {
                rows++;
                if (fr && i == fr->ir_right->row + 1) {
                    row -= fbrows;
                    fbrows = fbottomrows = 0;
                }
                if (row_hidden(sp, i))
                    continue;
                row++;
            }
        }
    }
    rowsinrange = colsinrange = 0;

    if (ftoprows && sp->strow >= fr->or_left->row && sp->strow < fr->or_left->row + ftoprows) {
        rows += (fr->or_left->row - sp->strow);
        sp->strow = fr->or_left->row + ftoprows;
        if (sp->currow < sp->strow)
            sp->strow = sp->currow;
    }

    mxcol = frightcols ? fr->or_right->col : sp->stcol + lcols - 1;
    mxrow = fbottomrows ? fr->or_right->row : sp->strow + rows - 1;
    gs.stflag = 0;
    lastfr = fr;
    lastftoprows = ftoprows;
    lastfbottomrows = fbottomrows;
    lastfleftcols = fleftcols;
    lastfrightcols = frightcols;

    /* Get rid of cursor standout on the cell at previous cursor position */
    if (!FullUpdate) {
        repaint_cursor(sp, -showcell);

        // XXX: check this stuff
        move(lastmy, lastmx + col_fwidth(sp, sc_lastcol));
        if ((inch() & A_CHARTEXT) == '<')
            addch(under_cursor | (inch() & A_ATTRIBUTES));

        /* remove the frame cursor */
        select_style(STYLE_FRAME, 0);
        repaint(lastmx, RESROW - 1, col_fwidth(sp, sc_lastcol), 0/*A_STANDOUT*/, A_COLOR);
        repaint(0, lastmy, sp->rescol - 1, 0/*A_STANDOUT*/, A_COLOR);
        select_style(STYLE_CELL, 0);
    }
    sc_lastrow = sp->currow;
    sc_lastcol = sp->curcol;
    lastendrow = sp->strow + rows;

    /* where is the the cursor now? */
    lastmy = RESROW;
    row = sp->strow;
    if (fr && row >= fr->or_left->row) {
        if (row < fr->ir_left->row)
            row = fr->or_left->row;
        else
            lastmy += ftrows;
    }
    lastmy += rows_height(sp, row, sp->currow - row);
    lastmx = sp->rescol;
    col = sp->stcol;
    if (fr && col >= fr->or_left->col) {
        if (col < fr->ir_left->col)
            col = fr->or_left->col;
        else
            lastmx += flcols;
    }
    lastmx += cols_width(sp, col, sp->curcol - col);
    select_style(STYLE_CELL, 0);

    if (FullUpdate || standlast) {
        move(2, 0);
        clrtobot();

        /* display the row numbers */
        // XXX: this loop is a mess
        for (row = RESROW, i = (ftoprows && sp->strow >= fr->or_left->row ?
                fr->or_left->row : sp->strow);
                i <= mxrow; i++) {
            if (ftoprows && sp->strow >= fr->or_left->row && row == RESROW + ftrows)
                i = sp->strow < i ? i : sp->strow;
            if (fbottomrows && row == lines - fbrows)
                i = fr->or_right->row - fbottomrows + 1;
            if (row_hidden(sp, i))
                continue;
            move(row, 0);
            if (i == sp->currow)
                select_style(STYLE_FRAME_CUR, 0);
            else
                select_style(STYLE_FRAME, 0);
            printw("%-*d", sp->rescol - 1, i);
            row++;
        }
#ifdef RIGHT_CBUG
        if (wasforw) {
            select_style(STYLE_CELL, 0);
            screen_rebuild();
            wasforw = 0;
        }
#endif
        /* display the column headings */
        select_style(STYLE_FRAME, 0);
        move(2, 0);
        printw("%*s", sp->rescol, "");

        col = sp->rescol;
        for (i = (fleftcols && sp->stcol >= fr->or_left->col) ? fr->or_left->col : sp->stcol;
             i <= mxcol;
             i++)
        {
            const char *colname;
            int len, width;

            if (fleftcols && sp->stcol >= fr->or_left->col && col == sp->rescol + flcols)
                i = sp->stcol < i ? i : sp->stcol;
            if (frightcols && col + col_fwidth(sp, i) >= cols - 1 - frcols && i < fr->or_right->col - frightcols + 1)
                i = fr->or_right->col - frightcols + 1;
            if (col_hidden(sp, i))
                continue;
            if (i == sp->curcol)
                select_style(STYLE_FRAME_CUR, 0);
            else
                select_style(STYLE_FRAME, 0);
            move(2, col);
            colname = coltoa(i);
            len = strlen(colname);
            width = col_fwidth(sp, i);
            if (width <= len) {
                printw("%s", colname + len - col_fwidth(sp, i));
            } else
            if (braille) {
                printw("%-*s", width, colname);
            } else {
                int k = (width - len) / 2;
                printw("%*s%-*s", k, "", width - k, colname);
            }
            col += width;
        }
        select_style(STYLE_CELL, 0);
    }

    move(1, 0);
    message = (inch() & A_CHARTEXT) != ' ';
    if (sp->showrange) {
        if (sp->showrange == SHOWROWS) {
            minsr = sp->showsr < sp->currow ? sp->showsr : sp->currow;
            minsc = fr ? fr->or_left->col : 0;
            maxsr = sp->showsr > sp->currow ? sp->showsr : sp->currow;
            maxsc = fr ? fr->or_right->col : sp->maxcols;

            if (sp->showtop && !message) {
                clrtoeol();
                printw("Default range:  %d:%d", minsr, maxsr);
            }
        } else if (sp->showrange == SHOWCOLS) {
            minsr = 0;
            minsc = sp->showsc < sp->curcol ? sp->showsc : sp->curcol;
            maxsr = sp->maxrows;
            maxsc = sp->showsc > sp->curcol ? sp->showsc : sp->curcol;

            if (sp->showtop && !message) {
                clrtoeol();
                printw("Default range:  %s:%s", coltoa(minsc), coltoa(maxsc));
            }
        } else {
            minsr = sp->showsr < sp->currow ? sp->showsr : sp->currow;
            minsc = sp->showsc < sp->curcol ? sp->showsc : sp->curcol;
            maxsr = sp->showsr > sp->currow ? sp->showsr : sp->currow;
            maxsc = sp->showsc > sp->curcol ? sp->showsc : sp->curcol;

            if (sp->showtop && !message) {
                clrtoeol();
                printw("Default range:  %s", r_name(sp, minsr, minsc, maxsr, maxsc));
            }
        }
    } else if (braille && braillealt && !message && mode_ind == 'v') {
        clrtoeol();
        printw("Current cell:   %s%d ", coltoa(sp->curcol), sp->currow);
    }

    /* Repaint the visible screen */
    if (sp->showrange || anychanged || FullUpdate || standlast) {
        /* may be reset in loop, if not next time we will do a FullUpdate */
        if (standlast) {
            FullUpdate = TRUE;
            standlast = FALSE;
        }

        for (row = (ftoprows && sp->strow >= fr->or_left->row ?
                    fr->or_left->row : sp->strow), r = RESROW;
             row <= mxrow;
             row++)
        {
            int c = sp->rescol;
            int do_stand = 0;
            int fieldlen;
            int nextcol;
            int len;

#if 0
            if (row < 0 || row >= sp->maxrows) {
                fprintf(stderr, "invalid row: %d\n", row);
                continue;
            }
#endif
            if (row_hidden(sp, row))
                continue;
            if (ftoprows && sp->strow >= fr->or_left->row && r == RESROW + ftrows)
                row = (sp->strow < row ? row : sp->strow);
            if (fbottomrows && r == lines - fbrows)
                row = fr->or_right->row - fbottomrows + 1;
            for (col = (fleftcols && sp->stcol >= fr->or_left->col ? fr->or_left->col : sp->stcol);
                 col <= mxcol;
                 col = nextcol, c += fieldlen)
            {
                if (fleftcols && sp->stcol >= fr->or_left->col && c == sp->rescol + flcols) {
                    col = (sp->stcol < col ? col : sp->stcol);
                }
                if (frightcols && c + col_fwidth(sp, col) >= cols - 1 - frcols && col < fr->or_right->col - frightcols + 1) {
                    col = fr->or_right->col - frightcols + 1;
                }
                nextcol = col + 1;
                fieldlen = 0;
#if 0
                if (col < 0 || col >= sp->maxcols) {
                    fprintf(stderr, "invalid cell: %d.%d\n", col, row);
                    continue;
                }
#endif
                if (col_hidden(sp, col))
                    continue;
                fieldlen = col_fwidth(sp, col);

                p = getcell(sp, row, col);

                select_style(STYLE_CELL, 0);
                /*
                 * Set standout if:
                 *
                 * - showing ranges, and not showing cells which need to be filled
                 *   in, and not showing cell expressions, and in a range, OR
                 *
                 * - showing cells which need to be filled in and this one is
                 *   of that type (has a value and doesn't have an expression,
                 *   or it is a string expression), OR
                 *
                 * - showing cells which have expressions and this one does.
                 */
                if ((sp->showrange && !showneed && !showexpr
                        && (row >= minsr) && (row <= maxsr)
                        && (col >= minsc) && (col <= maxsc))
                ||  (showneed && p && !p->expr && (p->type != SC_EMPTY))
                ||  (showexpr && p && p->expr)
                ||  (shownote && p && (p->flags & HAS_NOTE)))
                {
                    move(r, c);
                    //standout();
                    if ((cr = crange_find(sp, row, col)))
                        select_style(cr->r_color, 0);
                    else
                        select_style(STYLE_RANGE, 0);
                    standlast++;
                    if (!p) {     /* no cell, but standing out */
                        printw("%*s", fieldlen, "");
                        select_style(STYLE_CELL, -1);
                        continue;
                    } else
                        do_stand = 1;
                } else {
                    do_stand = 0;
                }

                if ((cr = crange_find(sp, row, col)))
                    select_style(cr->r_color, 0);

                if (p && ((p->flags & IS_CHANGED) || FullUpdate || do_stand)) {
                    if (do_stand) {
                        p->flags |= IS_CHANGED;
                    } else {
                        move(r, c);
                        p->flags &= ~IS_CHANGED;
                    }

                    if (p->type == SC_ERROR) {
                        // XXX: right alignment is ignored
                        if (sp->colorerr)
                            select_style(STYLE_ERROR, 0);
                        printw("%*.*s", fieldlen, fieldlen, error_name[p->cellerror]);
                    } else
                    if (p->expr && showexpr) {
                        /* Show expression; takes priority over other displays */
                        decompile(sp, field, sizeof field, p->expr, 0, 0, DCP_DEFAULT);
                        showstring(sp, field, ALIGN_LEFT, /* hasvalue = */ 0,
                                   row, col, &nextcol, mxcol, &fieldlen, r, c,
                                   fr, frightcols, flcols, frcols);
                    } else
                    if (p->type == SC_NUMBER || p->type == SC_BOOLEAN) {
                        /* Show cell's numeric value: */
                        int note = (p->flags & HAS_NOTE) != 0;
                        int align = p->flags & ALIGN_MASK;

                        if (p->type == SC_BOOLEAN) {
                            len = pstrcpy(field, sizeof field, p->v ? "TRUE" : "FALSE");
                            if (!align)
                                align = ALIGN_CENTER;
                        } else {
                            if (sp->colorneg && p->v < 0) {
                                if (cr)
                                    select_style(((cr->r_color) % CPAIRS) + 1, 0);
                                else
                                    select_style(STYLE_NEG, 0);
                            }
                            /* convert cell contents, do not test width, should not align */
                            *field = '\0';
                            if (p->format) {
                                len = format(field, sizeof field, s2c(p->format), sp->colfmt[col].precision, p->v, &align);
                            } else {
                                len = engformat(field, sizeof field, sp->colfmt[col].realfmt, sp->colfmt[col].precision, p->v, &align);
                            }
                        }
                        if (align & ALIGN_CLIP) {
                            align &= ~ALIGN_CLIP;
                            if (len < 0)
                                field[len = 0] = '\0';
                            // XXX: handle ALIGN_CENTER?
                            if (len > fieldlen)
                                field[len = fieldlen] = '\0';
                        }
                        if (len < 0 || len > fieldlen) {
                            /* fill column with stars, set the color of
                               the first one according to note presence */
                            if (note) {
#ifndef NO_ATTR_GET
                                attr_t attr;
                                short curcolor = 0;
                                if (color && has_colors()) {
                                    /* silence warning */
                                    attr_t *attrp = &attr;
                                    short *curcolorp = &curcolor;
                                    attr_get(attrp, curcolorp, NULL);
                                    select_style(STYLE_NOTE, 0);
                                }
#endif
                                addch('*');
#ifndef NO_ATTR_GET
                                select_style(curcolor, 0);
#endif
                            }
                            for (i = note; i < fieldlen; i++) {
                                addch('*');
                            }
                        } else {
                            int lpad = 0;
                            int rpad = 0;

                            if (align == ALIGN_LEFT) {
                                rpad = fieldlen - len;
                            } else
                            if (align == ALIGN_CENTER) {
                                lpad = (fieldlen - len) / 2;
                                rpad = fieldlen - len - lpad;
                            } else {
                                lpad = fieldlen - len;
                            }
                            if (note) {
                                if (lpad)
                                    lpad--;
                                else
                                if (rpad)
                                    rpad--;
                                else
                                    note = 0;
                            }
                            for (i = 0; i < lpad; i++)
                                addch(' ');
                            if (note) {
#ifndef NO_ATTR_GET
                                attr_t attr;
                                short curcolor = 0;
                                if (color && has_colors()) {
                                    /* silence warning */
                                    attr_t *attrp = &attr;
                                    short *curcolorp = &curcolor;
                                    attr_get(attrp, curcolorp, NULL);
                                    select_style(STYLE_NOTE, 0);
                                }
#endif
                                addch('*');
#ifndef NO_ATTR_GET
                                select_style(curcolor, 0);
#endif
                            }
                            addustr(field);
                            for (i = 0; i < rpad; i++)
                                addch(' ');
                        }
                    } else
                    if (p->type == SC_STRING) {
                        /* Show cell's label string: */
                        // XXX: should handle notes too */
                        showstring(sp, s2str(p->label),
                                   p->flags & ALIGN_MASK,
                                   /* hasvalue = */ 0,
                                   row, col, &nextcol, mxcol, &fieldlen,
                                   r, c, fr, frightcols, flcols, frcols);
                    } else      /* repaint a blank cell: XXX: too complicated */
                    if ((((do_stand || !FullUpdate) && (p->flags & IS_CHANGED)) ||
                         (cr && cr->r_color != 1)))
                    {
                        printw("%*s", fieldlen, "");
                    }
                } else
                if (!p && cr && cr->r_color != 1) {
                    move(r, c);
                    select_style(cr->r_color, 0);
                    printw("%*s", fieldlen, "");
                }
                select_style(STYLE_CELL, 0);
                if (do_stand) {
                    //standend();
                    do_stand = 0;
                }
            }
            r++;
        }
    }

    /* place 'cursor marker' */
    if (showcell && !showneed && !showexpr && !shownote) {
        repaint_cursor(sp, showcell);
    }

    /* highlite the frame cursor */
    select_style(STYLE_FRAME_CUR, 0);
    repaint(lastmx, RESROW - 1, col_fwidth(sp, sc_lastcol), 0, A_COLOR | A_STANDOUT);
    repaint(0, lastmy, sp->rescol - 1, 0, A_COLOR | A_STANDOUT);
    select_style(STYLE_CELL, 0);

    move(lastmy, lastmx + col_fwidth(sp, sc_lastcol));
    under_cursor = (inch() & A_CHARTEXT);
    if (!showcell)
        addch('<' | (inch() & A_ATTRIBUTES));

    move(0, 0);
    clrtoeol();

    if (linelim >= 0) {
        // XXX: display the current edit line
        // XXX: should scroll line to make cursor position visible
        // XXX: should clip the line to the visible portion
        int ctlchars = 0;
        for (i = 0; i < linelim; i++) {
            if ((unsigned char)line[i] < ' ')
                ctlchars++;
        }
        addch(mode_ind);
        addch('>');
        addch(search_ind);
        addustr(line);
        if (!braille || (!message && mode_ind != 'v'))
            move((linelim + 3 + ctlchars) / cols, (linelim + 3 + ctlchars) % cols);
        else if (message)
            move(1, 0);
        else if (braillealt)
            move(1, 16);
        else
            move(lastmy, lastmx);
    } else {
        if (sp->showtop) {                  /* show top line */
            printw("%s%d: ", coltoa(sp->curcol), sp->currow);

            p = getcell(sp, sp->currow, sp->curcol);

            /* show the current cell format */
            if (p && p->format) {
                printw("(%s) ", s2c(p->format));
            } else {
                printw("(%d %d %d) ", col_fwidth(sp, sp->curcol),
                       sp->colfmt[sp->curcol].precision,
                       sp->colfmt[sp->curcol].realfmt);
            }
            if (p) {
                /* display cell notes */
                if (p->flags & HAS_NOTE) {
                    /* Show the cell note range */
                    // XXX: should show the note instead?
                    printw("{*%s} ", r_name(sp, p->nrr.left.row, p->nrr.left.col,
                                            p->nrr.right.row, p->nrr.right.col));
                }
                /* display cell alignment */
                switch (p->flags & ALIGN_MASK) {
                case ALIGN_LEFT:    addch('<'); break;
                case ALIGN_CENTER:  addch('|'); break;
                case ALIGN_RIGHT:   addch('>'); break;
                }
                if (p->expr) {
                    // XXX: should pass sp->currow, sp->curcol as the cell reference
                    decompile(sp, field, sizeof field, p->expr, 0, 0, DCP_DEFAULT);
                    addch('[');
                    addustr(field);
                    addch(']');
                    addch(' ');
                }
                if (p->type == SC_NUMBER) { /* value is a number */
                    snprintf(field, sizeof field, "%.15g", p->v);
                    addstr(field);
                    addch(' ');
                } else
                if (p->type == SC_BOOLEAN) { /* value is logical */
                    addstr(p->v ? "TRUE" : "FALSE");
                    addch(' ');
                } else
                if (p->type == SC_STRING) { /* value is a string */
                    addch('\"');
                    addqstr(s2str(p->label));
                    addch('\"');
                    addch(' ');
                } else
                if (p->type == SC_ERROR) { /* value is an error */
                    addstr(error_name[p->cellerror]);
                }
                /* Display if cell is locked */
                if (p->flags & IS_LOCKED)
                    addstr("locked ");
            }
        }
        if (braille) {
            if (message)
                move(1, 0);
            else if (braillealt)
                move(0, 0);
            else
                move(lastmy, lastmx);
        } else if (showcell)
            move(lines - 1, cols - 1);
        else
            move(lastmy, lastmx + col_fwidth(sp, sc_lastcol));
    }

    select_style(STYLE_CELL, 0);

    if (revmsg[0]) {
        move(0, 0);
        clrtoeol();      /* get rid of topline display */
        printw(revmsg);
        *revmsg = '\0';         /* don't show it again */
        if (braille) {
            if (message)
                move(1, 0);
            else if (braillealt)
                move(0, 0);
            else
                move(lastmy, lastmx);
        } else if (showcell)
            move(lines - 1, cols - 1);
        else
            move(lastmy, lastmx + col_fwidth(sp, sc_lastcol));
    }

    FullUpdate = FALSE;
}

/* redraw what is under the cursor from curses' idea of the screen */
static void repaint(int x, int y, int len, int attron, int attroff) {
    while (len-- > 0) {
        move(y, x);
        addch((inch() & ~attroff) | attron);
        x++;
    }
}

void repaint_cursor(sheet_t *sp, int set) {
    int col = sc_lastcol;
    int width = col_fwidth(sp, col);

    if (!usecurses) return;

    if (set) {
#if 0
        struct ent *p;
        struct crange *cr;
        int style = STYLE_CELL;
        int row = sc_lastrow;

        if ((cr = crange_find(row, col)))
            style = cr->r_color;
        p = getcell(sp, row, col);
        if (p) {
            if (colorneg && (p->type == SC_NUMBER) && p->v < 0) {
                if (cr)
                    style = cr->r_color % CPAIRS + 1;
                else
                    style = STYLE_NEG;
            } else
            if (colorerr && p->cellerror)
                style = STYLE_ERROR;
        }
        select_style(style, 0);
#endif
        if (set < 0)
            repaint(lastmx, lastmy, width, 0, A_STANDOUT);
        else
            repaint(lastmx, lastmy, width, A_STANDOUT, 0);
    }
}

/* error routine for yacc (gram.y) */
void parse_error(const char *err, const char *src, const char *src_pos) {
    if (src_pos) {
        int pos = src_pos - src;
        error("%s: %.*s>%s", err, pos, src, src_pos);
    } else {
        error("%s: %s", err, src);
    }
}

void startdisp(void) {
    if (usecurses) {
#ifdef sun
        int fd = dup(0);
#endif
        int i;
#ifdef TIOCGSIZE
        {
            struct ttysize size;
            if (ioctl(0, TIOCGSIZE, &size) == 0) {
                lines = size.ts_lines;
                cols = size.ts_cols;
            }
        }
#endif

#ifdef XENIX2_3
        ioctl(fileno(stdin), TCGETA, &tmio);
#endif
        initscr();
        screen_LINES = LINES;
        screen_COLS = COLS;
        start_color();
        for (i = 1; i <= CPAIRS; i++) {
            if (cpairs[i])
                init_pair(i, cpairs[i]->fg, cpairs[i]->bg);
        }
        if (color && has_colors())
            bkgdset(COLOR_PAIR(1) | ' ');
#ifdef sun
        close(0);
        dup(fd);
        close(fd);
#endif
        clear();
#ifdef VMS
        VMS_read_raw = 1;
#else
        nonl();
        noecho();
        cbreak();
#endif
        initkbd();
        scrollok(stdscr, 1);

#if defined(SYSV3) && !defined(NOIDLOK)
# ifndef IDLOKBAD
        /*
         * turn hardware insert/delete on, if possible.
         * turn on scrolling for systems with SYSVr3.{1,2} (SYSVr3.0 has
         * this set as the default)
         */
        idlok(stdscr, TRUE);
# else  /*
         * This seems to fix (with an empty spreadsheet):
         *      a) Redrawing the bottom half of the screen when you
         *              move between row 9 <-> 10
         *      b) the highlighted row labels being trash when you
         *              move between row 9 <-> 10
         *      c) On an xterm on Esix Rev. D+ from eating lines
         *       -goto (or move) a few lines (or more) past the bottom
         *       of the screen, goto (or move) to the top line on the
         *       screen, move upward and the current line is deleted, the
         *       others move up even when they should not, check by
         *       noticing the rows become 2, 3, 40, 41, 42... (etc).
         */
        idlok(stdscr, FALSE);
# endif
#endif

        FullUpdate++;
    }
}

void stopdisp(void) {
    if (usecurses) {
        screen_deraw(1);
        resetkbd();
        endwin();
#ifdef XENIX2_3
        ioctl(fileno(stdin), TCSETAW, & tmio);
#endif
    }
}

/* init curses */
void screen_goraw(void) {
    if (usecurses) {
#ifdef VMS
        VMS_read_raw = 1;
#else /* VMS */
# ifdef HAVE_FIXTERM
        fixterm();
# else
        cbreak();
        nonl();
        noecho();
# endif
        kbd_again();
#endif /* VMS */
        if (color && has_colors())
            bkgdset(COLOR_PAIR(1) | ' ');
        FullUpdate++;
    }
}

/* clean up curses */
void screen_deraw(int ClearLastLine) {
    if (usecurses) {
        if (ClearLastLine) {
            if (color && has_colors())
                bkgdset(COLOR_PAIR(0) | ' ');
            move(lines - 1, 0);
            clrtoeol();
            refresh();
        }
#ifdef VMS
        VMS_read_raw = 0;
#else /* VMS */
# ifdef HAVE_RESETTERM
        resetterm();
# else
        nocbreak();
        nl();
        echo();
# endif
        resetkbd();
#endif /* VMS */
    }
}

/* called to let the user see external command output */
void screen_pause(void) {
    printf("Press any key to continue ");
    fflush(stdout);
    cbreak();
    nmgetch(0);
}

void screen_mouseon(void) {
    if (usecurses) {
#ifdef NCURSES_MOUSE_VERSION
        mousemask(BUTTON1_CLICKED
# if NCURSES_MOUSE_VERSION >= 2
                  | BUTTON4_PRESSED | BUTTON5_PRESSED
# endif
                  , NULL);
# if NCURSES_MOUSE_VERSION < 2
        error("Warning: NCURSES_MOUSE_VERSION < 2");
# endif
#else
        error("Error: NCURSES_MOUSE_VERSION undefined");
#endif
    }
}

void screen_mouseoff(void) {
    if (usecurses) {
#if NCURSES_MOUSE_VERSION >= 2
        mousemask(0, NULL);
#endif
    }
}

int screen_getmouse(struct screen_mouse_event *event) {
    MEVENT cevent;
    int bits = 0;

    if (!usecurses || getmouse(&cevent) != OK)
        return 0;

    event->x = cevent.x;
    event->y = cevent.y;
    //event->bstate = cevent.bstate;
    /* map mouse events */
    if (cevent.bstate & BUTTON1_RELEASED)       bits |= SC_BUTTON_RELEASED(1);
    if (cevent.bstate & BUTTON1_PRESSED)        bits |= SC_BUTTON_PRESSED(1);
    if (cevent.bstate & BUTTON1_CLICKED)        bits |= SC_BUTTON_CLICKED(1);
    if (cevent.bstate & BUTTON1_DOUBLE_CLICKED) bits |= SC_BUTTON_DOUBLE_CLICKED(1);
    if (cevent.bstate & BUTTON2_RELEASED)       bits |= SC_BUTTON_RELEASED(2);
    if (cevent.bstate & BUTTON2_PRESSED)        bits |= SC_BUTTON_PRESSED(2);
    if (cevent.bstate & BUTTON2_CLICKED)        bits |= SC_BUTTON_CLICKED(2);
    if (cevent.bstate & BUTTON2_DOUBLE_CLICKED) bits |= SC_BUTTON_DOUBLE_CLICKED(2);
    if (cevent.bstate & BUTTON3_RELEASED)       bits |= SC_BUTTON_RELEASED(3);
    if (cevent.bstate & BUTTON3_PRESSED)        bits |= SC_BUTTON_PRESSED(3);
    if (cevent.bstate & BUTTON3_CLICKED)        bits |= SC_BUTTON_CLICKED(3);
    if (cevent.bstate & BUTTON3_DOUBLE_CLICKED) bits |= SC_BUTTON_DOUBLE_CLICKED(3);
    if (cevent.bstate & BUTTON_CTRL)            bits |= SC_BUTTON_CTRL;
    if (cevent.bstate & BUTTON_SHIFT)           bits |= SC_BUTTON_SHIFT;
    if (cevent.bstate & BUTTON_ALT)             bits |= SC_BUTTON_ALT;
    event->bstate = bits;
    return 1;
}

void screen_init_pair(int n, int fg, int bg) {
    if (usecurses && color && has_colors()) {
        // XXX: translate SC_COLOR_xxx to COLOR_xxx
        init_pair(n, fg, bg);
    }
}

void sc_setcolor(int set) {
    color = set;
    if (usecurses && has_colors()) {
        if (set) {
            //select_style(STYLE_CELL, 0);
            attron(COLOR_PAIR(1));
            bkgd(COLOR_PAIR(1) | ' ');
        } else {
            //select_style(STYLE_NONE, 0);
            attron(COLOR_PAIR(0));
            bkgd(COLOR_PAIR(0) | ' ');
        }
        FullUpdate++;
    }
}

void screen_hidecursor(void) {
    if (usecurses)
        move(lines - 1, cols - 1);
}

/*
 * Show a cell's label string or expression value.  May overwrite value if
 * there is one already displayed in the cell.  Created from old code in
 * update(), copied with minimal changes.
 */

static
void showstring(sheet_t *sp,
                const char *string,         /* to display */
                int align,                  /* ALIGN_xxx */
                int hasvalue,               /* is there a numeric value? */
                int row, int col,           /* spreadsheet location */
                int *nextcolp,              /* value returned through it */
                int mxcol,                  /* last column displayed? */
                int *fieldlenp,             /* value returned through it */
                int r, int c,               /* screen row and column */
                struct frange *fr,          /* frame range we're currently in, if any */
                int frightcols,             /* number of frame columns to the right */
                int flcols, int frcols)     /* width of left and right sides of frame */
{
    int nextcol  = *nextcolp;
    int fieldlen = *fieldlenp;
    char field[FBUFLEN];
    int slen;
    char *start, *last, *fp;
    struct ent *nc;
    struct crange *cr;

    cr = crange_find(sp, row, col);

    /* This figures out if the label is allowed to
       slop over into the next blank field */

    slen = strlen(string);
    /* handle repeating patterns */
    if (*string == '\\' && string[1] != '\0')
        slen = col_fwidth(sp, col);
    if (c + fieldlen == sp->rescol + flcols && nextcol < sp->stcol)
        nextcol = sp->stcol;
    if (frightcols &&
            c + fieldlen + col_fwidth(sp, nextcol) >= cols - 1 - frcols &&
            nextcol < fr->or_right->col - frightcols + 1)
        nextcol = fr->or_right->col - frightcols + 1;

    while (slen > fieldlen && nextcol <= mxcol && !VALID_CELL(sp, nc, row, nextcol)
       &&  (sp->cslop || crange_find(sp, row, nextcol) == cr)) {
        if (!col_hidden(sp, nextcol))
            fieldlen += col_fwidth(sp, nextcol);
        nextcol++;
        if (c + fieldlen == sp->rescol + flcols && nextcol < sp->stcol)
            nextcol = sp->stcol;
        if (frightcols &&
                c + fieldlen + col_fwidth(sp, nextcol) >= cols - 1 - frcols &&
                nextcol < fr->or_right->col - frightcols + 1)
            nextcol = fr->or_right->col - frightcols + 1;
    }
    if (slen > fieldlen)
        slen = fieldlen;

    /* Now justify and print */
    // XXX: should allow center and right to bleed to the left over empty cells
    if (align == ALIGN_CENTER)
        start = field + ((slen < col_fwidth(sp, col)) ? (fieldlen - slen) / 2 : 0);
    else
        start = (align == ALIGN_RIGHT) ? field + fieldlen - slen : field;
    last = field + fieldlen;
    fp = field;
    if (slen) {
        while (fp < start)
            *fp++ = ' ';
    }
    if (*string == '\\' && string[1] != '\0') {
        const char *strt = ++string;

        while (slen--) {
            *fp++ = *string++;
            if (*string == '\0')
                string = strt;
        }
    } else {
        while (slen--) {
            *fp++ = *string++;
        }
    }

    if (!hasvalue || fieldlen != col_fwidth(sp, col)) {
        while (fp < last)
            *fp++ = ' ';
    }
    *fp = '\0';
    move(r, c);
    addustr(field);

    *nextcolp  = nextcol;
    *fieldlenp = fieldlen;
}

void cmd_redraw(sheet_t *sp) {
    if (usecurses) {
        screen_rebuild();
        update(sp, 1);
        screen_refresh();
        changed = 0;
    }
}

/*---------------- keyboard input ----------------*/

// XXX: should move the keyboard stuff to the curses module

#ifdef SIMPLE

void initkbd(void) {}
void kbd_again(void) {}
void resetkbd(void) {}

# ifndef VMS

int nmgetch(int clearline) {
    int c = getchar();
    if (clearline) screen_clear_line(1);
    return c;
}

# else /* VMS */

/*
   This is not perfect, it doesn't move the cursor when goraw changes
   over to deraw, but it works well enough since the whole sc package
   is incredibly stable (loop constantly positions cursor).

   Question, why didn't the VMS people just implement cbreak?

   NOTE: During testing it was discovered that the DEBUGGER and curses
   and this method of reading would collide (the screen was not updated
   when continuing from screen mode in the debugger).
*/
int nmgetch(int clearline) {
    short c;
    static int key_id = 0;
    int status;
#  define VMScheck(a) {if (~(status = (a)) & 1) VMS_MSG (status);}

    if (VMS_read_raw) {
        VMScheck(smg$read_keystroke(&stdkb->_id, &c, 0, 0, 0));
    } else {
        c = getchar();
    }

    switch (c) {
    case SMG$K_TRM_LEFT:  c = SC_KEY_LEFT;  break;
    case SMG$K_TRM_RIGHT: c = SC_KEY_RIGHT; break;
    case SMG$K_TRM_UP:    c = ctl('p');  break;
    case SMG$K_TRM_DOWN:  c = ctl('n');  break;
    default:   c = c & A_CHARTEXT; break;
    }
    if (clearline) screen_clear_line(1);
    return c;
}

VMS_MSG(int status) {
    /* Routine to put out the VMS operating system error (if one occurs). */
#  include <descrip.h>
   char errstr[81], buf[120];
   $DESCRIPTOR(errdesc, errstr);
   short length;
#  define err_out(msg) fprintf (stderr,msg)

   /* Check for no error or standard error */

    if (~status & 1) {
        status = status & 0x8000 ? status & 0xFFFFFFF : status & 0xFFFF;
        if (SYS$GETMSG(status, &length, &errdesc, 1, 0) == SS$_NORMAL) {
            errstr[length] = '\0';
            snprintf(buf, sizeof buf, "<0x%x> %s", status,
                     errdesc.dsc$a_pointer);
            err_out(buf);
        } else {
            err_out("System error");
        }
    }
}
# endif /* VMS */

#else /* SIMPLE */

# if defined(BSD42) || defined (SYSIII) || defined(BSD43)

#  define N_KEY 4

struct key_map {
    char *k_str;
    int k_val;
    char k_index;
};

struct key_map km[N_KEY];

char keyarea[N_KEY*30];

char *ks;
char ks_buf[20];
char *ke;
char ke_buf[20];

#  ifdef TIOCSLTC
struct ltchars old_chars, new_chars;
#  endif

char dont_use[] = {
    ctl('['), ctl('a'), ctl('b'), ctl('c'), ctl('e'), ctl('f'), ctl('g'),
    ctl('h'), ctl('i'), ctl('j'), ctl('l'), ctl('m'), ctl('n'), ctl('p'),
    ctl('q'), ctl('r'), ctl('s'), ctl('t'), ctl('u'), ctl('v'), ctl('w'),
    ctl('x'), ctl('z'), 0
};

int charout(int c) {
    return putchar(c);
}

void initkbd(void) {
    struct key_map *kp;
    int i, j;
    char *p = keyarea;
    char *ktmp;
    static char buf[1024]; /* Why do I have to do this again? */

    // XXX: should have a hard coded VT100 terminal version for !usecurses

    if (!(ktmp = getenv("TERM"))) {
        fprintf(stderr, "TERM environment variable not set\n");
        exit(1);
    }
    if (tgetent(buf, ktmp) <= 0)
        return;

    km[0].k_str = tgetstr("kl", &p); km[0].k_val = KEY_LEFT;
    km[1].k_str = tgetstr("kr", &p); km[1].k_val = KEY_RIGHT;
    km[2].k_str = tgetstr("ku", &p); km[2].k_val = ctl('p');
    km[3].k_str = tgetstr("kd", &p); km[3].k_val = ctl('n');

    ktmp = tgetstr("ks", &p);
    if (ktmp) {
        pstrcpy(ks_buf, sizeof ks_buf, ktmp);
        ks = ks_buf;
        tputs(ks, 1, charout);
    }
    ktmp = tgetstr("ke", &p);
    if (ktmp) {
        pstrcpy(ke_buf, sizeof ke_buf, ktmp);
        ke = ke_buf;
    }

    /* Unmap arrow keys which conflict with our ctl keys   */
    /* Ignore unset, longer than length 1, and 1-1 mapped keys */

    for (i = 0; i < N_KEY; i++) {
        kp = &km[i];
        if (kp->k_str && (kp->k_str[1] == 0) && (kp->k_str[0] != kp->k_val)) {
            for (j = 0; dont_use[j] != 0; j++) {
                if (kp->k_str[0] == dont_use[j]) {
                     kp->k_str = NULL;
                     break;
                }
            }
        }
    }

#  ifdef TIOCSLTC
    ioctl(fileno(stdin), TIOCGLTC, (char *)&old_chars);
    new_chars = old_chars;
    if (old_chars.t_lnextc == ctl('v'))
        new_chars.t_lnextc = -1;
    if (old_chars.t_rprntc == ctl('r'))
        new_chars.t_rprntc = -1;
    ioctl(fileno(stdin), TIOCSLTC, (char *)&new_chars);
#  endif
}

void kbd_again(void) {
    if (ks)
        tputs(ks, 1, charout);

#  ifdef TIOCSLTC
    ioctl(fileno(stdin), TIOCSLTC, (char *)&new_chars);
#  endif
}

void resetkbd(void) {
    if (ke)
        tputs(ke, 1, charout);

#  ifdef TIOCSLTC
    ioctl(fileno(stdin), TIOCSLTC, (char *)&old_chars);
#  endif
}

int nmgetch(int clearline) {
    int c;
    struct key_map *kp;
    struct key_map *biggest;
    int i;
    int almost;
    int maybe;

    static char dumpbuf[10];
    static char *dumpindex;

    if (dumpindex && *dumpindex) {
        if (clearline) screen_clear_line(1);
        return *dumpindex++;
    }

    c = getchar();
    biggest = 0;
    almost = 0;

    for (kp = &km[0]; kp < &km[N_KEY]; kp++) {
        if (!kp->k_str)
            continue;
        if (c == kp->k_str[kp->k_index]) {
            almost = 1;
            kp->k_index++;
            if (kp->k_str[kp->k_index] == 0) {
                c = kp->k_val;
                for (kp = &km[0]; kp < &km[N_KEY]; kp++)
                    kp->k_index = 0;
                return c;
            }
        }
        if (!biggest && kp->k_index)
            biggest = kp;
        else if (kp->k_index && biggest->k_index < kp->k_index)
            biggest = kp;
    }

    if (almost) {
        signal(SIGALRM, time_out);
        alarm(1);

        if (setjmp(wakeup) == 0) {
            maybe = nmgetch(clearline);
            alarm(0);
            return maybe;
        }
    }

    if (biggest) {
        for (i = 0; i < biggest->k_index; i++)
            dumpbuf[i] = biggest->k_str[i];
        if (!almost)
            dumpbuf[i++] = c;
        dumpbuf[i] = '\0';
        dumpindex = &dumpbuf[1];
        for (kp = &km[0]; kp < &km[N_KEY]; kp++)
            kp->k_index = 0;
        return dumpbuf[0];
    }

    if (clearline) screen_clear_line(1);
    return c;
}

# endif /* if defined(BSD42) || defined (SYSIII) || defined(BSD43) */

void initkbd(void) {
#ifdef HAVE_ESCDELAY
    /*
     * If ncurses exports the ESCDELAY variable, it should be set to
     * a low value, or you'll experience a delay in processing escape
     * sequences that are recognized by mc (e.g. Esc-Esc).  On the other
     * hand, making ESCDELAY too small can result in some sequences
     * (e.g. cursor arrows) being reported as separate keys under heavy
     * processor load, and this can be a problem if mc hasn't learned
     * them in the "Learn Keys" dialog.  The value is in milliseconds.
     */
    ESCDELAY = 100;
#endif /* HAVE_ESCDELAY */
    keypad(stdscr, TRUE);
#ifndef NONOTIMEOUT
    notimeout(stdscr,TRUE);
#endif
}

void kbd_again(void) {
    keypad(stdscr, TRUE);
#ifndef NONOTIMEOUT
    notimeout(stdscr,TRUE);
#endif
}

void resetkbd(void) {
    keypad(stdscr, FALSE);
#ifndef NONOTIMEOUT
    notimeout(stdscr, FALSE);
#endif
}

static int kbhit(void) {
#if 0
    /* this does not work because getch() already read pending input */
    struct timeval tv;
    fd_set fds;
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds); //STDIN_FILENO is 0
    return (select(STDIN_FILENO+1, &fds, NULL, NULL, &tv) == 1 &&
            FD_ISSET(STDIN_FILENO, &fds));
#else
    return 1;
#endif
}

int nmgetch(int clearline) {
    int c;

    c = getch();
    switch (c) {
# ifdef KEY_SELECT
    case KEY_SELECT:    c = 'm'; break;
# endif
# ifdef KEY_C1
/* This stuff works for a wyse wy75 in ANSI mode under 5.3.  Good luck. */
/* It is supposed to map the curses keypad back to the numeric equiv. */

/* I had to disable this to make programmable function keys work.  I'm
 * not familiar with the wyse wy75 terminal.  Does anyone know how to
 * make this work without causing problems with programmable function
 * keys on everything else?  - CRM

    case KEY_C1:    c = '0'; break;
    case KEY_A1:    c = '1'; break;
    case KEY_B2:    c = '2'; break;
    case KEY_A3:    c = '3'; break;
    case KEY_F(5):  c = '4'; break;
    case KEY_F(6):  c = '5'; break;
    case KEY_F(7):  c = '6'; break;
    case KEY_F(9):  c = '7'; break;
    case KEY_F(10): c = '8'; break;
    case KEY_F0:    c = '9'; break;
    case KEY_C3:    c = '.'; break;
    case KEY_ENTER: c = ctl('m'); break;
 *
 */
# endif
    case KEY_DOWN:      c = SC_KEY_DOWN; break; /* down-arrow key */
    case KEY_UP:        c = SC_KEY_UP; break; /* up-arrow key */
    case KEY_LEFT:      c = SC_KEY_LEFT; break; /* left-arrow key */
    case KEY_RIGHT:     c = SC_KEY_RIGHT; break; /* right-arrow key */
    case KEY_HOME:      c = SC_KEY_HOME; break; /* home key */
    case KEY_BACKSPACE: c = SC_KEY_BACKSPACE; break; /* backspace key */
    case KEY_F0:        c = SC_KEY_F0; break; /* Function keys. */
    case KEY_F(1):      c = SC_KEY_F(1); break; /* Value of function key n */
    case KEY_F(2):      c = SC_KEY_F(2); break;
    case KEY_F(3):      c = SC_KEY_F(3); break;
    case KEY_F(4):      c = SC_KEY_F(4); break;
    case KEY_F(5):      c = SC_KEY_F(5); break;
    case KEY_F(6):      c = SC_KEY_F(6); break;
    case KEY_F(7):      c = SC_KEY_F(7); break;
    case KEY_F(8):      c = SC_KEY_F(8); break;
    case KEY_F(9):      c = SC_KEY_F(9); break;
    case KEY_F(10):     c = SC_KEY_F(10); break;
    case KEY_DC:        c = SC_KEY_DC; break; /* delete-character key */
    case KEY_IC:        c = SC_KEY_IC; break; /* insert-character key */
    case KEY_NPAGE:     c = SC_KEY_NPAGE; break; /* next-page key */
    case KEY_PPAGE:     c = SC_KEY_PPAGE; break; /* previous-page key */
    case KEY_ENTER:     c = SC_KEY_ENTER; break; /* enter/send key */
    case KEY_END:       c = SC_KEY_END; break; /* end key */
    case KEY_FIND:      c = SC_KEY_FIND; break; /* find key */
    case KEY_HELP:      c = SC_KEY_HELP; break; /* help key */
    case KEY_MOUSE:     c = SC_KEY_MOUSE; break; /* Mouse event has occurred */
    case KEY_RESIZE:    c = SC_KEY_RESIZE; break; /* Terminal resize event */

    case 27:
        // XXX: should check for available input from curses to distinguish
        //      ESC hit by the user from alt and special keys
        if (kbhit()) {
            c = getch();
            if (c != 27)
                c = SC_ALT(c);
        }
        break;
    default:
        break;
    }
    if (clearline) screen_clear_line(1);
    return c;
}

#endif /* SIMPLE */

int nmgetch_savepos(int clearline) {
    int c, tempx = 0, tempy = 0;
    if (usecurses) getyx(stdscr, tempy, tempx);
    c = nmgetch(clearline);
    if (usecurses) move(tempy, tempx);
    return c;
}

int nmungetch(int c) {
    return ungetch(c);
}

/* called upon receiving the SIGWINCH signal */
void screen_resize(sheet_t *sp) {
    if (usecurses) {
        stopdisp();
        startdisp();
        /*
         * I'm not sure why a screen_refresh() needs to be done both before and after
         * the screen_rebuild() and update(), but without doing it this way, a screen
         * (or window) that grows bigger will leave the added space blank. - CRM
         */
        screen_refresh();
        FullUpdate++;
        screen_rebuild();
        update(sp, 1);
        screen_refresh();
    }
}

void screen_rebuild(void) {
    if (usecurses) clearok(stdscr, 1);
}

void screen_erase(void) {
    if (usecurses) clear();
}

void screen_refresh(void) {
    if (usecurses) refresh();
}

void screen_move(int y, int x) {
    if (usecurses) move(y, x);
}

void screen_clear_line(int y) {
    if (usecurses) { move(y, 0); clrtoeol(); }
}

void screen_draw_page(int y, int x, const char * const *screen) {
    if (usecurses) {
        int i;
        for (i = 0; screen[i]; i++) {
            move(y + i, x);
            addustr(screen[i]);
            clrtoeol();
        }
    }
}

void screen_draw_line(int y, int x, const char *str) {
    if (usecurses) {
        move(y, x);
        addustr(str);
        clrtoeol();
    }
}

int screen_get_keyname(char *buf, size_t size, int c) {
    const char *name;

    if (c < 256) {
        // XXX: should handle control keys, DEL, and UNICODE
        buf[0] = c;
        buf[1] = '\0';
        return 1;
    }
#ifdef HAVE_CURSES_KEYNAME
    if (c >= KEY_MIN && c <= KEY_MAX && (name = keyname(c)) != NULL) {
        // XXX: this is bogus for function keys
        int i, len;
        buf[0] = '\0';
        pstrcpy(buf + 1, size - 1, name);
        /* strip `KEY_` and parentheses */
        for (len = 1, i = 5; buf[i]; i++) {
            if (buf[i] != '(' && buf[i] != ')')
                buf[len++] = buf[i];
        }
        buf[len] = '\0';
        return len;
    }
#endif
    buf[0] = '\0';
    return 1 + pstrcpy(buf + 1, size - 1, "UNKNOWN KEY");
}

int screen_draw_menu(int y, int x, struct menu_item const *menu, int option) {
    int e;
    move(y, x);
    clrtoeol();
    for (e = 0; menu[e].option; e++) {
        if (e == option)
            select_style(STYLE_FRAME, 0);
        addustr(menu[e].option);
        select_style(STYLE_CELL, 0);
        addstr("  ");
    }
    return e;
}
