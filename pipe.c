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
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);

            *buf = '\0';
            if (p) {
                switch (p->type) {
                case SC_NUMBER:
                    snprintf(buf, sizeof buf - 1, "%.15g", p->v);
                    break;
                case SC_BOOLEAN:
                    snprintf(buf, sizeof buf - 1, "%s", p->v ? "TRUE" : "FALSE");
                    break;
                case SC_ERROR:
                    snprintf(buf, sizeof buf - 1, "%s",
                             p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                    break;
                }
            }
            len = pstrcat(buf, sizeof buf, (c < rr.right.col) ? "\t" : "\n");
            write(fd, buf, len);
            if (brokenpipe)
                return;
        }
    }
}

void fgetnum(rangeref_t rr, int fd) {
    char buf[FBUFLEN+1];
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            /* convert cell contents, but ignore alignment and width test */
            struct ent *p = *ATBL(tbl, r, c);
            int align = ALIGN_DEFAULT;

            *buf = '\0';
            if (p) {
                switch (p->type) {
                case SC_NUMBER:
                    if (p->format) {
                        format(buf, sizeof(buf) - 1, s2c(p->format), precision[c], p->v, &align);
                    } else {
                        engformat(buf, sizeof(buf) - 1, realfmt[c], precision[c], p->v, &align);
                    }
                    break;
                case SC_BOOLEAN:
                    snprintf(buf, sizeof buf - 1, "%s", p->v ? "TRUE" : "FALSE");
                    break;
                case SC_ERROR:
                    snprintf(buf, sizeof buf - 1, "%s",
                             p->cellerror == CELLERROR ? "ERROR" : "INVALID");
                    break;
                }
            }
            len = pstrcat(buf, sizeof buf, (c < rr.right.col) ? "\t" : "\n");
            write(fd, buf, len);
            if (brokenpipe)
                return;
        }
    }
}

void getstring(rangeref_t rr, int fd) {
    char buf[FBUFLEN];  /* for very long labels */
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p && p->type == SC_STRING)
                snprintf(buf, sizeof buf - 1, "%s", s2str(p->label));
            len = pstrcat(buf, sizeof buf, (c < rr.right.col) ? "\t" : "\n");
            write(fd, buf, len);
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
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            *buf = '\0';
            if (p && p->format)
                snprintf(buf, sizeof buf - 1, "%s", s2c(p->format));
            len = pstrcat(buf, sizeof buf, (c < rr.right.col) ? "\t" : "\n");
            write(fd, buf, len);
            if (brokenpipe)
                return;
        }
    }
}

void getframe(int fd) {
    char buf[100];
    struct frange *fr;
    int len;

    *buf = '\0';
    if ((fr = get_current_frange())) {
        snprintf(buf, sizeof buf - 1, "%s %s",
                 r_name(fr->or_left->row, fr->or_left->col,
                        fr->or_right->row, fr->or_right->col),
                 r_name(fr->ir_left->row, fr->ir_left->col,
                        fr->ir_right->row, fr->ir_right->col));
    }
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
}

void getrange(SCXMEM string_t *name, int fd) {
    char buf[100];
    struct nrange *r;
    int len;

    *buf = '\0';
    if (name && !find_nrange_name(s2c(name), slen(name), &r)) {
        snprintf(buf, sizeof buf - 1, "%s%s%s%d",
                r->r_left.vf & FIX_COL ? "$" : "",
                coltoa(r->r_left.vp->col),
                r->r_left.vf & FIX_ROW ? "$" : "",
                r->r_left.vp->row);
        if (r->r_is_range) {
            len = strlen(buf);
            snprintf(buf + len, sizeof(buf) - 1 - len, ":%s%s%s%d",
                     r->r_right.vf & FIX_COL ? "$" : "",
                     coltoa(r->r_right.vp->col),
                     r->r_right.vf & FIX_ROW ? "$" : "",
                     r->r_right.vp->row);
        }
    }
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
    free_string(name);
}

void cmd_eval(SCXMEM enode_t *e, SCXMEM string_t *fmt, int row, int col, int fd) {
    char buf[FBUFLEN];
    int align = ALIGN_DEFAULT, len, err = 0;
    double v;

    // XXX: should output parseable value: number or string
    v = neval_at(e, row, col, &err);
    if (err) {
        snprintf(buf, sizeof buf - 1, "ERROR");
    } else
    if (!sempty(fmt)) {
        /* convert cell contents, do not test width, should not align */
        format(buf, sizeof buf - 1, s2c(fmt), precision[col], v, &align);
    } else {
        snprintf(buf, sizeof buf - 1, "%.15g", v);
    }
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
    free_string(fmt);
    efree(e);
}

void cmd_seval(SCXMEM enode_t *e, int row, int col, int fd) {
    int err = 0;
    SCXMEM string_t *str;

    str = seval_at(e, row, col, &err);
    if (!err) write(fd, s2c(str), slen(str));
    write(fd, "\n", 1);
    free_string(str);
    efree(e);
}

void cmd_query(SCXMEM string_t *s, SCXMEM string_t *data, int fd) {
    char buf[FBUFLEN];
    int len;

    goraw();
    len = query(buf, sizeof buf, s ? s2c(s) : NULL, data ? s2c(data) : NULL);
    deraw(0);
    if (len >= 0) {
        write(fd, buf, len);
        write(fd, "\n", 1);
    }

    free_string(s);
    free_string(data);
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
