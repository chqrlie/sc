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
#include "sc.h"

#ifdef BROKENCURSES    /* nl/nonl bug fix */
#undef nl
#undef nonl
#define nl()     (_tty.sg_flags |= CRMOD, _pfast = _rawmode, stty(_tty_ch, &_tty))
#define nonl()   (_tty.sg_flags &= ~CRMOD, _pfast = TRUE, stty(_tty_ch, &_tty))
#endif

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
static bool frTooLarge = 0; /* If set, either too many rows or too many columns
                        exist in frame to allow room for the scrolling
                        portion of the framed range */
#ifdef RIGHT_CBUG
static int wasforw = FALSE;  /* Causes screen to be redisplay if on lastcol */
#endif
int framerows;      /* Rows in current frame */
int framecols;      /* Columns in current frame */
int rescol = 4;     /* Columns reserved for row numbers */

static void repaint(int x, int y, int len, int attron, int attroff);

void error(const char *fmt, ...) {
    char buf[256];
    va_list ap;

    select_style(STYLE_CELL, 0);
    if (isatty(fileno(stdout)) && !move(1, 0) && !clrtoeol()) {
        va_start(ap, fmt);
        // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
        ((int (*)(char *, size_t, const char *, va_list))vsnprintf)(buf, sizeof buf, fmt, ap);
        va_end(ap);
        addstr(buf);
    }
}

void select_style(int n, int rev) {
    if (rev) {
        if (rev > 0)
            standout();
        else
            standend();
    }
    if (color && has_colors()) {
        color_set(n, NULL);
    }
}

/*
 * update() does general screen update
 *
 * standout last time in update()?
 *      At this point we will let curses do work
 */
static int standlast = FALSE;

void update(int anychanged) {          /* did any cell really change in value? */
    buf_t(buf, FBUFLEN);
    int row, col;
    struct ent *p;
    int mxrow, mxcol;
    int minsr = 0, minsc = 0, maxsr = 0, maxsc = 0;
    int r, i;
    struct frange *fr;
    struct crange *cr;
    int ftoprows, fbottomrows, fleftcols, frightcols;
    int ftrows, fbrows, flcols, frcols;
    bool message;

    /*
     * If receiving input from a pipeline, don't display spreadsheet data
     * on screen.
     */
    if (!usecurses) return;

    getmaxyx(stdscr, lines, cols);
    fr = lastfr;
    if (!(fr && fr->or_left->row <= currow &&   /* If we've left the        */
            fr->or_left->col <= curcol &&       /* previous framed range... */
            fr->or_right->row >= currow &&
            fr->or_right->col >= curcol)) {
        fr = find_frange(currow, curcol);
        if (fr != lastfr)
            FullUpdate++;
    }
    if (fr) {
        ftoprows = fr->ir_left->row - fr->or_left->row;
        fbottomrows = fr->or_right->row - fr->ir_right->row;
        fleftcols = fr->ir_left->col - fr->or_left->col;
        frightcols = fr->or_right->col - fr->ir_right->col;
        framerows = RESROW + ftoprows + fbottomrows;
        framecols = rescol;
        for (r = fr->or_left->row - 1, i = ftoprows; i; i--)
            if (row_hidden[r+i])
                framerows--;
        for (r = fr->or_right->row - 1, i = fbottomrows; i; i--)
            if (row_hidden[r-i])
                framerows--;
        for (r = fr->or_left->col - 1, i = fleftcols; i; i--) {
            if (col_hidden[r+i])
                continue;
            framecols += fwidth[r+i];
        }
        for (r = fr->or_right->col - 1, i = frightcols; i; i--) {
            if (col_hidden[r-i])
                continue;
            framecols += fwidth[r-i];
        }
        if (framerows >= lines || framecols >= cols) {
            frTooLarge = TRUE;
            if (FullUpdate) {
                error("Frame too large for screen size - ignoring");
            }
            ftoprows = fbottomrows = fleftcols = frightcols = 0;
            strow -= lastftoprows;
            stcol -= lastfleftcols;
        } else {
            frTooLarge = FALSE;
            if (strow >= fr->or_left->row) {
                if (fr == lastfr && strow < fr->or_left->row + ftoprows)
                    strow = fr->or_left->row;
                else if (strow > fr->ir_right->row) {
                    strow = fr->ir_right->row;
                    FullUpdate++;
                }
            }
            if (stcol >= fr->or_left->col) {
                if (stcol < fr->or_left->col + fleftcols)
                    stcol = fr->or_left->col;
                else if (stcol > fr->ir_right->col) {
                    stcol = fr->ir_right->col;
                    FullUpdate++;
                }
            }
            if (fr == lastfr && currow == sc_lastrow)
                fbottomrows = lastfbottomrows;
            else if (currow < fr->ir_right->row)
                fbottomrows = fr->or_right->row - fr->ir_right->row;
            else
                fbottomrows = fr->or_right->row - currow;
            if (fr == lastfr && curcol == sc_lastcol)
                frightcols = lastfrightcols;
            else if (curcol < fr->ir_right->col)
                frightcols = fr->or_right->col - fr->ir_right->col;
            else
                frightcols = fr->or_right->col - curcol;
        }
    } else {
        ftoprows = fbottomrows = fleftcols = frightcols = 0;
        framerows = framecols = 0;
    }
    if (fr != lastfr && !gs.stflag && lastfr) {
        if (strow >= lastfr->ir_left->row)
            strow -= lastftoprows;
        if (stcol >= lastfr->ir_left->col)
            stcol -= lastfleftcols;
    }

    ftrows = ftoprows;
    fbrows = fbottomrows;
    flcols = frcols = 0;
    if (fr) {
        for (r = fr->or_left->row - 1, i = ftrows; i; i--)
            if (row_hidden[r+i])
                ftrows--;
        for (r = fr->or_right->row + 1, i = fbrows; i; i--)
            if (row_hidden[r-i])
                fbrows--;
        for (r = fr->or_left->col - 1, i = fleftcols; i; i--) {
            if (col_hidden[r+i])
                continue;
            flcols += fwidth[r+i];
        }
        for (r = fr->or_right->col + 1, i = frightcols; i; i--) {
            if (col_hidden[r-i])
                continue;
            frcols += fwidth[r-i];
        }
    }

    /*
     * Place the cursor on the screen.  Set col, curcol, stcol, sc_lastcol as
     * needed.  If strow and stcol are negative, centering is forced.
     */
    if ((curcol != sc_lastcol) || FullUpdate) {
        while (col_hidden[curcol])   /* You can't hide the last row or col */
            curcol++;
        if (fwidth[curcol] > cols - rescol - 2) {
            error("column %s too wide - resizing", coltoa(curcol));
            doformat(curcol, curcol, cols - rescol - 2,
                    precision[curcol], realfmt[curcol]);
        }

        /* First see if the last display still covers curcol */
        if (stcol >= 0 && stcol <= curcol) {
            int c = 0;

            if (fr) {
                if (fr != lastfr) {
                    if (stcol == fr->or_left->col)
                        stcol += fleftcols;
                    else if (stcol >= fr->or_left->col && !gs.stflag) {
                        stcol += fleftcols;
                        if (stcol > fr->ir_right->col)
                            stcol = fr->ir_right->col + 1;
                    }
                } else if (stcol == fr->or_left->col)
                    stcol += fleftcols;
            }
            i = stcol;
            lcols = 0;
            col = rescol + frcols;
            if (fr && stcol >= fr->or_left->col) {
                if (stcol < fr->ir_left->col)
                    i = fr->or_left->col;
                else
                    col += flcols;
            }
            for (; i < maxcols &&
                    (col + fwidth[i] < cols-1 || col_hidden[i] || i < curcol);
                    i++) {
                lcols++;
                if (fr && i == fr->ir_right->col + 1) {
                    col -= frcols;
                    frcols = frightcols = 0;
                }
                if (col_hidden[i])
                    continue;

                /* If there isn't room for more columns, and we haven't yet
                 * reached the current column, start removing columns from
                 * the left.
                 */
                while (col + fwidth[i] > cols - 2) {
                    lcols--;
                    col -= fwidth[stcol];
                    while (col_hidden[++stcol])
                        continue;
                    FullUpdate++;
                    c++;
                }
                col += fwidth[i];
            }
            if (!frTooLarge && fr && curcol <= stcol + lcols &&
                    fr->ir_left->col >= stcol + lcols) {
                while (stcol + lcols < fr->ir_left->col) {
                    col -= fwidth[stcol];
                    lcols--;
                    while (col_hidden[++stcol])
                        lcols--;
                    while (col + fwidth[stcol + lcols] < cols - 1) {
                        col += fwidth[stcol + lcols];
                        lcols++;
                    }
                }
            } else if (c)
                stcol = -1;
        }

        while (stcol < 0 || curcol < stcol || stcol + lcols - 1 < curcol ||
                (colsinrange != fwidth[curcol] && stcol != curcol &&
                stcol + lcols - 1 < gs.g_lastcol)) {

            FullUpdate++;

                /* How about back one? */
            if (stcol - 1 == curcol) {
                stcol--;
                /* Forward one? */
            } else if (stcol >= 0 && stcol + lcols == curcol) {
                stcol++;
            } else if (stcol >= 0 && fr && curcol >= fr->or_left->col &&
                    curcol <= fr->ir_left->col && stcol < curcol &&
                    curcol <= stcol + lcols + fr->ir_left->col -
                            fr->or_left->col) {
                while ((stcol + lcols < fr->ir_left->col && !frTooLarge) ||
                        (colsinrange != fwidth[curcol] && stcol != curcol &&
                        stcol + lcols - 1 < gs.g_lastcol)) {
                    if (col_hidden[++stcol]) lcols--;
                }
            } else {
                /* Try to put the cursor in the center of the screen.
                 * If we've just jumped to a range using the goto command,
                 * center the range instead.
                 */
                colsinrange = (colsinrange > cols - rescol -
                        flcols - frcols - 2 ?
                        cols - rescol - flcols - frcols - 2 : colsinrange);
                col = (cols - rescol - flcols - frcols - colsinrange)/2;
                stcol = curcol;
                for (i = curcol - 1;
                        i >= (fr ? fr->or_left->col + fleftcols : 0) &&
                        (col - fwidth[i] > 0 || col_hidden[i]);
                        i--) {
                    stcol--;
                    if (col_hidden[i])
                        continue;
                    col -= fwidth[i];
                }
                if (fr && stcol < fr->or_left->col + fleftcols) {
                    stcol = fr->or_left->col + fleftcols;
                    if (curcol < stcol)
                        stcol = curcol;
                }
            }
            /* Now pick up the counts again */
            i = stcol;
            lcols = 0;
            col = rescol + frcols;
            if (fr && stcol >= fr->or_left->col) {
                if (stcol < fr->ir_left->col)
                    i = fr->or_left->col;
                else
                    col += flcols;
            }
            for (; i < maxcols &&
                    (col + fwidth[i] < cols-1 || col_hidden[i] || i < curcol);
                    i++) {
                lcols++;
                if (fr && i == fr->ir_right->col + 1) {
                    col -= frcols;
                    frcols = frightcols = 0;
                }
                if (col_hidden[i])
                    continue;

                col += fwidth[i];
            }
        }
    }
    if (fleftcols && stcol >= fr->or_left->col &&
            stcol < fr->or_left->col + fleftcols) {
        lcols += (fr->or_left->col - stcol);
        stcol = fr->or_left->col + fleftcols;
        if (curcol < stcol)
            stcol = curcol;
    }

    /* Now - same process on the rows as the columns */
    if ((currow != sc_lastrow) || FullUpdate) {
        while (row_hidden[currow])   /* You can't hide the last row or col */
            currow++;
        if (strow >= 0 && strow <= currow) {
            int c = 0;

            if (fr) {
                if (fr != lastfr) {
                    if (strow == fr->or_left->row)
                        strow += ftoprows;
                    else if (strow >= fr->or_left->row && !gs.stflag) {
                        strow += ftoprows;
                        if (strow > fr->ir_right->row)
                            strow = fr->ir_right->row + 1;
                    }
                } else if (strow == fr->or_left->row)
                    strow += ftoprows;
            }
            i = strow;
            rows = 0;
            row = RESROW + fbrows;
            if (fr && strow >= fr->or_left->row) {
                if (strow < fr->ir_left->row)
                    i = fr->or_left->row;
                else
                    row += ftrows;
            }
            for (; (row < lines || row_hidden[i] || i < currow) && i < maxrows;
                    i++) {
                rows++;
                if (fr && i == fr->ir_right->row + 1) {
                    row -= fbrows;
                    fbrows = fbottomrows = 0;
                }
                if (row_hidden[i])
                    continue;

                /* If there isn't room for more rows, and we haven't yet
                 * reached the current row, start removing rows from the
                 * top.
                 */
                if (row >= lines) {
                    rows--;
                    row--;
                    while (row_hidden[++strow])
                        continue;
                    FullUpdate++;
                    c++;
                }
                row++;
            }
            if (!frTooLarge && fr && currow <= strow + rows &&
                    fr->ir_left->row >= strow + rows) {
                while (strow + rows < fr->ir_left->row) {
                    while (row_hidden[++strow])
                        continue;
                }
            } else if (c && currow > lastendrow)
                strow = -1;
        }

        while (strow < 0 || currow < strow || strow + rows - 1 < currow ||
                strow + rows < currow + rowsinrange) {

            FullUpdate++;

                /* How about up one? */
            if (strow - 1 == currow) {
                strow--;
                /* Down one? */
            } else if (strow >= 0 && strow + rows == currow) {
                strow++;
            } else if (strow >= 0 && fr && currow >= fr->or_left->row &&
                    currow <= fr->ir_left->row && strow < currow &&
                    currow <= strow + rows + fr->ir_left->row -
                            fr->or_left->row) {
                while ((strow + rows < fr->ir_left->row && !frTooLarge) ||
                        (rowsinrange > 1 && strow != currow &&
                        strow + rows - 1 < gs.g_lastrow)) {
                    if (row_hidden[++strow]) rows--;
                }
            } else {
                /* Try to put the cursor in the center of the screen.
                 * If we've just jumped to a range using the goto command,
                 * center the range instead.
                 */
                rowsinrange = (rowsinrange > lines - RESROW - ftrows - fbrows ?
                        lines - RESROW - ftrows - fbrows : rowsinrange);
                row = (lines - RESROW - ftrows - fbrows - rowsinrange) / 2;
                strow = currow;
                for (i = currow - 1;
                        i >= (fr ? fr->or_left->row + ftoprows : 0) &&
                        (row > 0 || row_hidden[i]); i--) {
                    strow--;
                    if (row_hidden[i])
                        continue;
                    row--;
                }
                if (fr && strow < fr->or_left->row + ftoprows) {
                    strow = fr->or_left->row + ftoprows;
                    if (currow < strow)
                        strow = currow;
                }
            }
            /* Now pick up the counts again */
            i = strow;
            rows = 0;
            row = RESROW + fbrows;
            if (fr && strow >= fr->or_left->row) {
                if (strow < fr->ir_left->row)
                    i = fr->or_left->row;
                else
                    row += ftrows;
            }
            for (; (row < lines || row_hidden[i] || i < currow) && i < maxrows;
                    i++) {
                rows++;
                if (fr && i == fr->ir_right->row + 1) {
                    row -= fbrows;
                    fbrows = fbottomrows = 0;
                }
                if (row_hidden[i])
                    continue;

                row++;
            }
        }
    }
    if (ftoprows && strow >= fr->or_left->row &&
            strow < fr->or_left->row + ftoprows) {
        rows += (fr->or_left->row - strow);
        strow = fr->or_left->row + ftoprows;
        if (currow < strow)
            strow = currow;
    }

    mxcol = frightcols ? fr->or_right->col : stcol + lcols - 1;
    mxrow = fbottomrows ? fr->or_right->row : strow + rows - 1;
    gs.stflag = 0;
    lastfr = fr;
    lastftoprows = ftoprows;
    lastfbottomrows = fbottomrows;
    lastfleftcols = fleftcols;
    lastfrightcols = frightcols;

    /* Get rid of cursor standout on the cell at previous cursor position */
    if (!FullUpdate) {
        repaint_cursor(-showcell);

        // XXX: check this stuff
        move(lastmy, lastmx + fwidth[sc_lastcol]);
        if ((inch() & A_CHARTEXT) == '<')
            addch(under_cursor | (inch() & A_ATTRIBUTES));

        /* remove the frame cursor */
        select_style(STYLE_FRAME, 0);
        repaint(lastmx, RESROW - 1, fwidth[sc_lastcol], 0/*A_STANDOUT*/, A_COLOR);
        repaint(0, lastmy, rescol - 1, 0/*A_STANDOUT*/, A_COLOR);
        select_style(STYLE_CELL, 0);
    }
    sc_lastrow = currow;
    sc_lastcol = curcol;
    lastendrow = strow + rows;

    /* where is the the cursor now? */
    lastmy = RESROW;
    if (fr && strow >= fr->or_left->row) {
        if (strow < fr->ir_left->row)
            row = fr->or_left->row;
        else {
            row = strow;
            lastmy += ftrows;
        }
    } else
        row = strow;
    for (; row < currow; row++) {
        if (!row_hidden[row])
            lastmy++;
    }
    lastmx = rescol;
    if (fr && stcol >= fr->or_left->col) {
        if (stcol < fr->ir_left->col)
            col = fr->or_left->col;
        else {
            col = stcol;
            lastmx += flcols;
        }
    } else
        col = stcol;
    for (; col < curcol; col++) {
        if (!col_hidden[col])
            lastmx += fwidth[col];
    }
    select_style(STYLE_CELL, 0);

    if (FullUpdate || standlast) {
        move(2, 0);
        clrtobot();

        /* display the row numbers */
        // XXX: this loop is a mess
        for (row = RESROW, i = (ftoprows && strow >= fr->or_left->row ?
                fr->or_left->row : strow);
                i <= mxrow; i++) {
            if (ftoprows && strow >= fr->or_left->row && row == RESROW + ftrows)
                i = strow < i ? i : strow;
            if (fbottomrows && row == lines - fbrows)
                i = fr->or_right->row - fbottomrows + 1;
            if (row_hidden[i])
                continue;
            move(row, 0);
            if (i == currow)
                select_style(STYLE_FRAME_CUR, 0);
            else
                select_style(STYLE_FRAME, 0);
            printw("%-*d", rescol - 1, i);
            row++;
        }
#ifdef RIGHT_CBUG
        if (wasforw) {
            select_style(STYLE_CELL, 0);
            clearok(stdscr, TRUE);
            wasforw = 0;
        }
#endif
        /* display the column headings */
        select_style(STYLE_FRAME, 0);
        move(2, 0);
        printw("%*s", rescol, "");

        col = rescol;
        for (i = (fleftcols && stcol >= fr->or_left->col) ? fr->or_left->col : stcol;
             i <= mxcol;
             i++)
        {
            char *colname;
            int len, width;

            if (fleftcols && stcol >= fr->or_left->col && col == rescol + flcols)
                i = stcol < i ? i : stcol;
            if (frightcols && col + fwidth[i] >= cols - 1 - frcols && i < fr->or_right->col - frightcols + 1)
                i = fr->or_right->col - frightcols + 1;
            if (col_hidden[i])
                continue;
            if (i == curcol)
                select_style(STYLE_FRAME_CUR, 0);
            else
                select_style(STYLE_FRAME, 0);
            move(2, col);
            colname = coltoa(i);
            len = strlen(colname);
            width = fwidth[i];
            if (width <= len) {
                printw("%s", colname + len - fwidth[i]);
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
    if (showrange) {
        if (showrange == SHOWROWS) {
            minsr = showsr < currow ? showsr : currow;
            minsc = fr ? fr->or_left->col : 0;
            maxsr = showsr > currow ? showsr : currow;
            maxsc = fr ? fr->or_right->col : maxcols;

            if (showtop && !message) {
                clrtoeol();
                printw("Default range:  %d:%d", minsr, maxsr);
            }
        } else if (showrange == SHOWCOLS) {
            minsr = 0;
            minsc = showsc < curcol ? showsc : curcol;
            maxsr = maxrows;
            maxsc = showsc > curcol ? showsc : curcol;

            if (showtop && !message) {
                clrtoeol();
                printw("Default range:  %s:%s", coltoa(minsc), coltoa(maxsc));
            }
        } else {
            minsr = showsr < currow ? showsr : currow;
            minsc = showsc < curcol ? showsc : curcol;
            maxsr = showsr > currow ? showsr : currow;
            maxsc = showsc > curcol ? showsc : curcol;

            if (showtop && !message) {
                clrtoeol();
                printw("Default range:  %s", r_name(minsr, minsc, maxsr, maxsc));
            }
        }
    } else if (braille && braillealt && !message && mode_ind == 'v') {
        clrtoeol();
        printw("Current cell:   %s%d ", coltoa(curcol), currow);
    }

    /* Repaint the visible screen */
    if (showrange || anychanged || FullUpdate || standlast) {
        /* may be reset in loop, if not next time we will do a FullUpdate */
        if (standlast) {
            FullUpdate = TRUE;
            standlast = FALSE;
        }

        for (row = (ftoprows && strow >= fr->or_left->row ?
                    fr->or_left->row : strow), r = RESROW;
             row <= mxrow;
             row++)
        {
            int c = rescol;
            int do_stand = 0;
            int fieldlen;
            int nextcol;

#if 0
            if (row < 0 || row >= maxrows) {
                fprintf(stderr, "invalid row: %d\n", row);
                continue;
            }
#endif
            if (row_hidden[row])
                continue;
            if (ftoprows && strow >= fr->or_left->row && r == RESROW + ftrows)
                row = (strow < row ? row : strow);
            if (fbottomrows && r == lines - fbrows)
                row = fr->or_right->row - fbottomrows + 1;
            for (col = (fleftcols && stcol >= fr->or_left->col ? fr->or_left->col : stcol);
                 col <= mxcol;
                 col = nextcol, c += fieldlen)
            {
                if (fleftcols && stcol >= fr->or_left->col && c == rescol + flcols) {
                    col = (stcol < col ? col : stcol);
                }
                if (frightcols && c + fwidth[col] >= cols - 1 - frcols && col < fr->or_right->col - frightcols + 1) {
                    col = fr->or_right->col - frightcols + 1;
                }
                nextcol = col + 1;
                fieldlen = 0;
#if 0
                if (col < 0 || col >= maxcols) {
                    fprintf(stderr, "invalid cell: %d.%d\n", col, row);
                    continue;
                }
#endif
                if (col_hidden[col])
                    continue;
                fieldlen = fwidth[col];

                p = *ATBL(tbl, row, col);

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
                if ((showrange && !showneed && !showexpr
                        && (row >= minsr) && (row <= maxsr)
                        && (col >= minsc) && (col <= maxsc))
                    || (showneed && p && (p->flags & IS_VALID) &&
                        ((p->flags & IS_STREXPR) || !p->expr))
                    || (showexpr && p && p->expr)
                    || (shownote && p && (p->nrow >= 0)))
                {
                    move(r, c);
                    //standout();
                    if ((cr = find_crange(row, col)))
                        select_style(cr->r_color, 0);
                    else
                        select_style(STYLE_RANGE, 0);
                    standlast++;
                    if (!p) {     /* no cell, but standing out */
                        printw("%*s", fwidth[col], "");
                        select_style(STYLE_CELL, -1);
                        continue;
                    } else
                        do_stand = 1;
                } else
                    do_stand = 0;

                if ((cr = find_crange(row, col)))
                    select_style(cr->r_color, 0);

                if (p && ((p->flags & IS_CHANGED) || FullUpdate || do_stand)) {
                    if (do_stand) {
                        p->flags |= IS_CHANGED;
                    } else {
                        move(r, c);
                        p->flags &= ~IS_CHANGED;
                    }

                    /*
                     * Show expression; takes priority over other displays:
                     */

                    if (p->cellerror) {
                        if (colorerr)
                            select_style(STYLE_ERROR, 0);
                        printw("%*.*s", fwidth[col], fwidth[col],
                               p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                    } else
                    if (showexpr && p->expr) {
                        buf_reset(buf);
                        decompile(buf, p->expr);
                        showstring(buf->buf, /* leftflush = */ 1, /* hasvalue = */ 0,
                                   row, col, &nextcol, mxcol, &fieldlen, r, c,
                                   fr, frightcols, flcols, frcols);
                    } else {
                        /*
                         * Show cell's numeric value:
                         */

                        if (p->flags & IS_VALID) {
                            char field[FBUFLEN];
                            char *cfmt;
                            int note;

                            *field = '\0';
                            note = p->nrow >= 0 ? 1 : 0;
                            cfmt = p->format ? p->format :
                                (realfmt[col] >= 0 && realfmt[col] < COLFORMATS &&
                                 colformat[realfmt[col]]) ?
                                colformat[realfmt[col]] : NULL;
                            if (colorneg && p->v < 0) {
                                if (cr)
                                    select_style(((cr->r_color) % CPAIRS) + 1, 0);
                                else
                                    select_style(STYLE_NEG, 0);
                            }
                            if (cfmt) {
                                if (*cfmt == ctl('d')) {
                                    time_t v = (time_t)(p->v);
                                    // XXX: should check format string
                                    ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
                                        (field, sizeof(field), cfmt + 1, localtime(&v));
                                } else
                                    format(cfmt, precision[col], p->v,
                                           field, sizeof(field));
                            } else {
                                engformat(realfmt[col], fwidth[col] - note,
                                          precision[col], p->v,
                                          field, sizeof(field));
                            }
                            if ((ssize_t)strlen(field) > fwidth[col]) {
                                for (i = 0; i < fwidth[col]; i++) {
                                    if (note) {
#ifndef NO_ATTR_GET
                                        attr_t attr;
                                        short curcolor = 0;
                                        if (!i && color && has_colors()) {
                                            /* silence warning */
                                            attr_t *attrp = &attr;
                                            short *curcolorp = &curcolor;
                                            attr_get(attrp, curcolorp, NULL);
                                            select_style(STYLE_NOTE, 0);
                                        }
#endif
                                        addch('*');
                                        i++;
#ifndef NO_ATTR_GET
                                        if (!i)
                                            select_style(curcolor, 0);
#endif
                                    }
                                    addch('*');
                                }
                            } else {
                                if (cfmt && *cfmt != ctl('d')) {
                                    for (i = 0; i < fwidth[col] - (ssize_t)strlen(field) - note; i++)
                                        addch(' ');
                                }
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
                                addstr(field);
                                if (cfmt && *cfmt == ctl('d')) {
                                    for (i = 0; i < fwidth[col] - (ssize_t)strlen(field) - note; i++)
                                        addch(' ');
                                }
                            }
                        }

                        /*
                         * Show cell's label string:
                         */

                        if (p->label) {
                            showstring(p->label,
                                       p->flags & (IS_LEFTFLUSH | IS_LABEL),
                                       p->flags & IS_VALID,
                                       row, col, &nextcol, mxcol, &fieldlen,
                                       r, c, fr, frightcols, flcols, frcols);
                        } else      /* repaint a blank cell: */
                        if ((((do_stand || !FullUpdate) && (p->flags & IS_CHANGED)) ||
                             (cr && cr->r_color != 1)) &&
                            !(p->flags & IS_VALID) && !p->label)
                        {
                            printw("%*s", fwidth[col], "");
                        }
                    } /* else */
                } else
                if (!p && cr && cr->r_color != 1) {
                    move(r, c);
                    select_style(cr->r_color, 0);
                    printw("%*s", fwidth[col], "");
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
        repaint_cursor(showcell);
    }

    /* highlite the frame cursor */
    select_style(STYLE_FRAME_CUR, 0);
    repaint(lastmx, RESROW - 1, fwidth[sc_lastcol], 0, A_COLOR | A_STANDOUT);
    repaint(0, lastmy, rescol - 1, 0, A_COLOR | A_STANDOUT);
    select_style(STYLE_CELL, 0);

    move(lastmy, lastmx + fwidth[sc_lastcol]);
    under_cursor = (inch() & A_CHARTEXT);
    if (!showcell)
        addch('<' | (inch() & A_ATTRIBUTES));

    move(0, 0);
    clrtoeol();

    if (linelim >= 0) {
        // XXX: display the current edit line
        // XXX: should scroll line to make cursor position visible
        int ctlchars = 0;
        for (i = 0; i < linelim; i++) {
            if ((unsigned char)line[i] < ' ')
                ctlchars++;
        }
        addch(mode_ind);
        addch('>');
        addch(search_ind);
        addstr(line);
        if (!braille || (!message && mode_ind != 'v'))
            move((linelim + 3 + ctlchars) / cols, (linelim + 3 + ctlchars) % cols);
        else if (message)
            move(1, 0);
        else if (braillealt)
            move(1, 16);
        else
            move(lastmy, lastmx);
    } else {
        if (showtop) {                  /* show top line */
            struct ent *p1;
            int printed = 0;            /* printed something? */

            printw("%s%d ", coltoa(curcol), currow);

            if ((p1 = *ATBL(tbl, currow, curcol)) && p1->nrow > -1) {
                printw("{*%s} ", r_name(p1->nrow, p1->ncol,
                                        p1->nlastrow, p1->nlastcol));
            }

            /* show the current cell's format */
            if (p1 && p1->format) {
                printw("(%s) ", p1->format);
            } else {
                printw("(%d %d %d) ", fwidth[curcol], precision[curcol],
                                      realfmt[curcol]);
            }
            if (p1) {
                buf_reset(buf);
                if (p1->expr) {
                    /* has expr of some type */
                    decompile(buf, p1->expr);
                }

                /*
                 * Display string part of cell:
                 */

                if (p1->expr && (p1->flags & IS_STREXPR)) {
                    if (p1->flags & IS_LABEL)
                        addstr("|{");
                    else
                        addstr((p1->flags & IS_LEFTFLUSH) ? "<{" : ">{");
                    addstr(buf->buf);
                    addstr("} ");        /* and this '}' is for vi % */
                    printed = 1;
                } else
                if (p1->label) {
                    /* has constant label only */
                    if (p1->flags & IS_LABEL)
                        addstr("|\"");
                    else
                        addstr((p1->flags & IS_LEFTFLUSH) ? "<\"" : ">\"");
                    addstr(p1->label);
                    addstr("\" ");
                    printed = 1;
                }

                /*
                 * Display value part of cell:
                 */

                if (p1->flags & IS_VALID) {
                    /* has value or num expr */
                    if (!(p1->expr) || (p1->flags & IS_STREXPR))
                        buf_setf(buf, "%.15g", p1->v);

                    addch('[');
                    addstr(buf->buf);
                    addch(']');
                    printed = 1;
                }
            }
            if (!printed)
                addstr("[]");
            /* Display if cell is locked */
            if (p1 && (p1->flags & IS_LOCKED))
                addstr(" locked");
        }
        if (braille)
            if (message)
                move(1, 0);
            else if (braillealt)
                move(0, 0);
            else
                move(lastmy, lastmx);
        else if (showcell)
            move(lines - 1, cols - 1);
        else
            move(lastmy, lastmx + fwidth[sc_lastcol]);
    }

    select_style(STYLE_CELL, 0);

    if (revmsg[0]) {
        move(0, 0);
        clrtoeol();      /* get rid of topline display */
        printw(revmsg);
        *revmsg = '\0';         /* don't show it again */
        if (braille)
            if (message)
                move(1, 0);
            else if (braillealt)
                move(0, 0);
            else
                move(lastmy, lastmx);
        else if (showcell)
            move(lines - 1, cols - 1);
        else
            move(lastmy, lastmx + fwidth[sc_lastcol]);
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

void repaint_cursor(int set) {
    int row = sc_lastrow;
    int col = sc_lastcol;
    int width = fwidth[col];
    struct ent *p;
    struct crange *cr;
    int style = STYLE_CELL;

    if (set) {
        p = *ATBL(tbl, row, col);
        if ((cr = find_crange(row, col)))
            style = cr->r_color;
        if (p) {
            if (colorneg && (p->flags & IS_VALID) && p->v < 0) {
                if (cr)
                    style = cr->r_color % CPAIRS + 1;
                else
                    style = STYLE_NEG;
            } else
            if (colorerr && p->cellerror)
                style = STYLE_ERROR;
        }
        select_style(style, 0);

        if (set < 0)
            repaint(lastmx, lastmy, width, 0, A_STANDOUT);
        else
            repaint(lastmx, lastmy, width, A_STANDOUT, 0);
    }
}

int seenerr;

/* error routine for yacc (gram.y) */
void parse_error(const char *err, const char *base, const char *pos) {
    if (usecurses) {
        if (seenerr) return;
        seenerr++;
        move(1, 0);
        clrtoeol();
        // XXX: should print line portion around error if too long
        printw("%s: %.*s<=%s", err, (int)(pos - base), base, pos);
    } else {
        fprintf(stderr, "%s: %.*s<=%s\n", err, (int)(pos - base), base, pos);
    }
}

#ifdef XENIX2_3
struct termio tmio;
#endif

void startdisp(void) {
#ifdef sun
    int fd = dup(0);
#endif
    if (usecurses) {
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
        deraw(1);
        resetkbd();
        endwin();
#ifdef XENIX2_3
        ioctl(fileno(stdin), TCSETAW, & tmio);
#endif
    }
}

/* init curses */
#ifdef VMS

void goraw(void) {
    if (usecurses) {
        VMS_read_raw = 1;
        if (color && has_colors())
            bkgdset(COLOR_PAIR(1) | ' ');
        FullUpdate++;
    }
}

void deraw(int ClearLastLine) {
    if (usecurses) {
        if (ClearLastLine) {
            if (color && has_colors())
                bkgdset(COLOR_PAIR(0) | ' ');
            move(lines - 1, 0);
            clrtoeol();
            refresh();
        }
        VMS_read_raw = 0;
    }
}

#else /* VMS */
void goraw(void) {
    if (usecurses) {
#ifdef HAVE_FIXTERM
        fixterm();
#else
        cbreak();
        nonl();
        noecho();
#endif
        kbd_again();
        if (color && has_colors())
            bkgdset(COLOR_PAIR(1) | ' ');
        FullUpdate++;
    }
}

/* clean up curses */
void deraw(int ClearLastLine) {
    if (usecurses) {
        if (ClearLastLine) {
            if (color && has_colors())
                bkgdset(COLOR_PAIR(0) | ' ');
            move(lines - 1, 0);
            clrtoeol();
            refresh();
        }
#ifdef HAVE_RESETTERM
        resetterm();
#else
        nocbreak();
        nl();
        echo();
#endif
        resetkbd();
    }
}

#endif /* VMS */

void mouseon(void) {
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

void mouseoff(void) {
#if NCURSES_MOUSE_VERSION >= 2
    mousemask(0, NULL);
#endif
}

void sc_setcolor(int set) {
    color = set;
    if (usecurses && has_colors()) {
        if (set) {
            select_style(STYLE_CELL, 0);
            bkgd(COLOR_PAIR(1) | ' ');
        } else {
            //select_style(STYLE_NONE, 0);
            color_set(0, NULL);
            bkgd(COLOR_PAIR(0) | ' ');
        }
        FullUpdate++;
    }
}

void hidecursor(void) {
    move(lines - 1, cols - 1);
}
