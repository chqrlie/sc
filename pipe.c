/*      SC      A Spreadsheet Calculator
 *              Routines for piping data to and from an external macro program
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Original Version Created:  June, 2000
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <time.h>
#include "sc.h"

void getnum(rangeref_t rr, int fd) {
    char buf[32];
    int r, c;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p) {
                if (p->cellerror)
                    snprintf(buf, sizeof buf - 1, "%s",
                             p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                else if (p->flags & IS_VALID)
                    snprintf(buf, sizeof buf - 1, "%.15g", p->v);
            }
            strlcat(buf, (c < rr.right.col) ? "\t" : "\n", sizeof buf);
            write(fd, buf, strlen(buf));
            if (brokenpipe)
                return;
        }
    }
}

void fgetnum(rangeref_t rr, int fd) {
    char field[FBUFLEN+1];
    int row, col;

    for (row = rr.left.row; row <= rr.right.row; row++) {
        for (col = rr.left.col; col <= rr.right.col; col++) {
            /* convert cell contents, but ignore alignment and width test */
            struct ent *p = *ATBL(tbl, row, col);
            int align = ALIGN_DEFAULT;

            *field = '\0';
            if (p) {
                if (p->cellerror) {
                    snprintf(field, sizeof field - 1, "%s",
                             p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                } else
                if (p->flags & IS_VALID) {
                    if (p->format) {
                        format(field, sizeof(field) - 1, p->format->s, precision[col], p->v, &align);
                    } else {
                        engformat(field, sizeof(field) - 1, realfmt[col], precision[col], p->v, &align);
                    }
                }
            }
            strlcat(field, (col < rr.right.col) ? "\t" : "\n", sizeof field);
            write(fd, field, strlen(field));
            if (brokenpipe)
                return;
        }
    }
}

void getstring(rangeref_t rr, int fd) {
    char buf[FBUFLEN];  /* for very long labels */
    int r, c;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p && p->label)
                snprintf(buf, sizeof buf - 1, "%s", p->label->s);
            strlcat(buf, (c < rr.right.col) ? "\t" : "\n", sizeof buf);
            write(fd, buf, strlen(buf));
            if (brokenpipe)
                return;
        }
    }
}

void getexp(rangeref_t rr, int fd) {
    buf_t(buf, FBUFLEN);
    int r, c;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            buf_reset(buf);
            if (p && p->expr) {
                // XXX: should pass r, c as the current cell
                decompile_expr(buf, p->expr, 0, 0, DCP_NO_LOCALE);
                if (*buf->buf == '?')
                    buf_reset(buf);
            }
            // XXX: should force separator output even if buffer is full
            buf_putc(buf, (c < rr.right.col) ? '\t' : '\n');
            buf_write(buf, fd);
            if (brokenpipe)
                return;
        }
    }
}

void getformat(int col, int fd) {
    char buf[32];
    snprintf(buf, sizeof buf, "%d %d %d\n", fwidth[col], precision[col], realfmt[col]);
    write(fd, buf, strlen(buf));
}

void getfmt(rangeref_t rr, int fd) {
    char buf[FBUFLEN];  /* for very long format strings */
    int r, c;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p && p->format)
                snprintf(buf, sizeof buf - 1, "%s", p->format->s);
            strlcat(buf, (c < rr.right.col) ? "\t" : "\n", sizeof buf);
            write(fd, buf, strlen(buf));
            if (brokenpipe)
                return;
        }
    }
}

void getframe(int fd) {
    char buf[100];
    struct frange *fr;

    *buf = '\0';
    if ((fr = get_current_frange())) {
        snprintf(buf, sizeof buf - 1, "%s %s",
                 r_name(fr->or_left->row, fr->or_left->col,
                        fr->or_right->row, fr->or_right->col),
                 r_name(fr->ir_left->row, fr->ir_left->col,
                        fr->ir_right->row, fr->ir_right->col));
    }
    strlcat(buf, "\n", sizeof buf);
    write(fd, buf, strlen(buf));
}

void getrange(const char *name, int fd) {
    char buf[100];
    struct nrange *r;

    *buf = '\0';
    if (!find_nrange_name(name, strlen(name), &r)) {
        snprintf(buf, sizeof buf - 1, "%s%s%s%d",
                r->r_left.vf & FIX_COL ? "$" : "",
                coltoa(r->r_left.vp->col),
                r->r_left.vf & FIX_ROW ? "$" : "",
                r->r_left.vp->row);
        if (r->r_is_range) {
            int len = strlen(buf);
            snprintf(buf + len, sizeof(buf) - 1 - len, ":%s%s%s%d",
                     r->r_right.vf & FIX_COL ? "$" : "",
                     coltoa(r->r_right.vp->col),
                     r->r_right.vf & FIX_ROW ? "$" : "",
                     r->r_right.vp->row);
        }
    }
    strlcat(buf, "\n", sizeof buf);
    write(fd, buf, strlen(buf));
}

void cmd_eval(struct enode *e, const char *fmt, int row, int col, int fd) {
    char buf[FBUFLEN];
    int align = ALIGN_DEFAULT;
    double v;

    gmyrow = row;
    gmycol = col;

    v = eval(e);
    if (fmt) {
        /* convert cell contents, do not test width, should not align */
        format(buf, sizeof buf - 1, fmt, precision[col], v, &align);
    } else {
        snprintf(buf, sizeof buf - 1, "%.15g", v);
    }
    strlcat(buf, "\n", sizeof buf);
    write(fd, buf, strlen(buf));
}

void cmd_seval(struct enode *e, int row, int col, int fd) {
    SCXMEM string_t *str;

    gmyrow = row;
    gmycol = col;

    str = seval(e);
    if (str)
        write(fd, str->s, str->len);
    write(fd, "\n", 1);
    free_string(str);
}

// XXX: this is an ugly hack
void cmd_query(const char *s, const char *data, int fd) {
    goraw();
    // XXX: should provide destination buffer
    query(s, data);
    deraw(0);
    if (linelim >= 0) {
        write(fd, line, strlen(line));
        write(fd, "\n", 1);
    }

    line[0] = '\0';
    linelim = -1;
    //CLEAR_LINE; // XXX: why this?
    update(0);
}

void dogetkey(int fd) {
    char buf[32];
    int c, len;

    goraw();
    c = nmgetch(0);
    deraw(0);

    // XXX: this is bogus for function keys
    if (c < 256) {
        buf[0] = c;
        len = 1;
#ifdef HAVE_CURSES_KEYNAME
    } else if (c >= KEY_MIN && c <= KEY_MAX) {
        int i, j;
        buf[0] = '\0';
        snprintf(buf + 1, sizeof buf - 1, "%s\n", keyname(c));
        /* strip `KEY_` and parentheses */
        for (i = 1, j = 5; buf[j-1]; j++) {
            if (buf[j] != '(' && buf[j] != ')')
                buf[i++] = buf[j];
        }
        len = 1 + strlen(buf + 1);
#endif
    } else {
        buf[0] = '0';
        snprintf(buf + 1, sizeof buf - 1, "%s\n", "UNKNOWN KEY");
        len = 1 + strlen(buf + 1);
    }

    write(fd, buf, len);
}

void cmd_status(int fd) {
    char buf[8];
    char *p = buf;
    if (modflg)                 *p++ = 'm';
    if (isatty(STDIN_FILENO))   *p++ = 'i';
    if (isatty(STDOUT_FILENO))  *p++ = 'o';
    *p++ = '\n';
    *p = '\0';
    write(fd, buf, p - buf);
}

void cmd_whereami(int fd) {
    char buf[64];
    snprintf(buf, sizeof buf, "%s%d %s%d\n",
             coltoa(curcol), currow, coltoa(stcol), strow);
    write(fd, buf, strlen(buf));
}
