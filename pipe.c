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

void getnum(int r0, int c0, int rn, int cn, int fd) {
    char buf[32];
    int r, c;

    for (r = r0; r <= rn; r++) {
        for (c = c0; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p) {
                if (p->cellerror)
                    snprintf(buf, sizeof buf - 1, "%s",
                             p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                else if (p->flags & IS_VALID)
                    snprintf(buf, sizeof buf - 1, "%.15g", p->v);
            }
            strlcat(buf, (c < cn) ? "\t" : "\n", sizeof buf);
            write(fd, buf, strlen(buf));
            if (brokenpipe)
                return;
        }
    }
}

void fgetnum(int r0, int c0, int rn, int cn, int fd) {
    char field[FBUFLEN+1];  /* for very long format strings */
    int row, col;

    for (row = r0; row <= rn; row++) {
        for (col = c0; col <= cn; col++) {
            struct ent *p = *ATBL(tbl, row, col);
            int fieldlen = fwidth[col];

            *field = '\0';
            if (p) {
                if (p->cellerror) {
                    snprintf(field, sizeof field - 1, "%s",
                             p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                } else
                if (p->flags & IS_VALID) {
                    const char *cfmt = p->format;
                    if (cfmt) {
                        if (*cfmt == ctl('d')) {
                            time_t v = (time_t)(p->v);
                            // XXX: must check format string
                            ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
                                (field, sizeof(field) - 1, cfmt + 1, localtime(&v));
                        } else {
                            format(cfmt, precision[col], p->v, field, sizeof(field) - 1);
                        }
                    } else {
                        engformat(realfmt[col], fieldlen, precision[col], p->v, field, sizeof(field) - 1);
                    }
                }
            }
            strlcat(field, (col < cn) ? "\t" : "\n", sizeof field);
            write(fd, field, strlen(field));
            if (brokenpipe)
                return;
        }
    }
}

void getstring(int r0, int c0, int rn, int cn, int fd) {
    char buf[FBUFLEN];  /* for very long labels */
    int r, c;

    for (r = r0; r <= rn; r++) {
        for (c = c0; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p && p->label)
                snprintf(buf, sizeof buf - 1, "%s", p->label);
            strlcat(buf, (c < cn) ? "\t" : "\n", sizeof buf);
            write(fd, buf, strlen(buf));
            if (brokenpipe)
                return;
        }
    }
}

void getexp(int r0, int c0, int rn, int cn, int fd) {
    buf_t(buf, FBUFLEN);
    int r, c;

    for (r = r0; r <= rn; r++) {
        for (c = c0; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            buf_reset(buf);
            if (p && p->expr) {
                decompile_node(buf, p->expr, 0);
                if (*buf->buf == '?')
                    buf_reset(buf);
            }
            // XXX: should force separator output even if buffer is full
            buf_putc(buf, (c < cn) ? '\t' : '\n');
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

void getfmt(int r0, int c0, int rn, int cn, int fd) {
    char buf[FBUFLEN];  /* for very long format strings */
    int r, c;

    for (r = r0; r <= rn; r++) {
        for (c = c0; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p && p->format)
                snprintf(buf, sizeof buf - 1, "%s", p->format);
            strlcat(buf, (c < cn) ? "\t" : "\n", sizeof buf);
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
    if ((fr = find_frange(currow, curcol))) {
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
    struct range *r;

    *buf = '\0';
    if (!find_range_name(name, strlen(name), &r)) {
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
    double v;

    gmyrow = row;
    gmycol = col;

    v = eval(e);
    if (fmt) {
        if (*fmt == ctl('d')) {
            time_t tv = v;
            // XXX: should check format string
            ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
                (buf, sizeof buf - 1, fmt + 1, localtime(&tv));
        } else {
            format(fmt, precision[col], v, buf, sizeof buf - 1);
        }
    } else {
        snprintf(buf, sizeof buf - 1, "%.15g", v);
    }
    strlcat(buf, "\n", sizeof buf);
    write(fd, buf, strlen(buf));
}

void cmd_seval(struct enode *e, int row, int col, int fd) {
    SCXMEM char *s;

    gmyrow = row;
    gmycol = col;

    s = seval(e);
    if (s)
        write(fd, s, strlen(s));
    write(fd, "\n", 1);
    scxfree(s);
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
