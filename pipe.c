/*      SC      A Spreadsheet Calculator
 *              Routines for piping data to and from an external macro program
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Original Version Created:  June, 2000
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#include "sc.h"

void cmd_getnum(sheet_t *sp, rangeref_t rr, int fd) {
    char buf[32];
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);

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
                    snprintf(buf, sizeof buf - 1, "%s", error_name[p->cellerror]);
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

void cmd_fgetnum(sheet_t *sp, rangeref_t rr, int fd) {
    char buf[FBUFLEN+1];
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            /* convert cell contents, but ignore alignment and width test */
            struct ent *p = getcell(sp, r, c);
            int align = ALIGN_DEFAULT;

            *buf = '\0';
            if (p) {
                switch (p->type) {
                case SC_NUMBER:
                    if (p->format) {
                        format(buf, sizeof(buf) - 1, s2c(p->format), sp->colfmt[c].precision, p->v, &align);
                    } else {
                        engformat(buf, sizeof(buf) - 1, sp->colfmt[c].realfmt, sp->colfmt[c].precision, p->v, &align);
                    }
                    break;
                case SC_BOOLEAN:
                    snprintf(buf, sizeof buf - 1, "%s", p->v ? "TRUE" : "FALSE");
                    break;
                case SC_ERROR:
                    snprintf(buf, sizeof buf - 1, "%s", error_name[p->cellerror]);
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

void cmd_getstring(sheet_t *sp, rangeref_t rr, int fd) {
    char buf[FBUFLEN];  /* for very long labels */
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
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

void cmd_getexp(sheet_t *sp, rangeref_t rr, int fd) {
    buf_t(buf, FBUFLEN);
    int r, c;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            buf_reset(buf);
            if (p && p->expr) {
                // XXX: should pass r, c as the current cell
                decompile_expr(sp, buf, p->expr, 0, 0, DCP_NO_LOCALE);
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

void cmd_getformat(sheet_t *sp, int col, int fd) {
    char buf[32];
    snprintf(buf, sizeof buf, "%d %d %d\n",
             sp->colfmt[col].fwidth,
             sp->colfmt[col].precision,
             sp->colfmt[col].realfmt);
    write(fd, buf, strlen(buf));
}

void cmd_getfmt(sheet_t *sp, rangeref_t rr, int fd) {
    char buf[FBUFLEN];  /* for very long format strings */
    int r, c, len;

    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
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

void cmd_getframe(sheet_t *sp, int fd) {
    char buf[100];
    struct frange *fr;
    int len;

    *buf = '\0';
    if ((fr = frange_get_current(sp))) {
        snprintf(buf, sizeof buf - 1, "%s %s",
                 range_addr(sp, fr->orr), range_addr(sp, fr->irr));
    }
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
}

void cmd_getrange(sheet_t *sp, SCXMEM string_t *name, int fd) {
    char buf[100];
    struct nrange *r;
    int len;

    *buf = '\0';
    if (name && !nrange_find_name(sp, s2c(name), slen(name), &r)) {
        if (r->is_range) {
            snprintf(buf, sizeof(buf) - 1, "%s", range_addr(sp, r->rr));
        } else {
            snprintf(buf, sizeof(buf) - 1, "%s", cell_addr(sp, r->rr.left));
        }
    }
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
    string_free(name);
}

void cmd_eval(sheet_t *sp, SCXMEM enode_t *e, SCXMEM string_t *fmt, int row, int col, int fd) {
    char buf[FBUFLEN];
    int align = ALIGN_DEFAULT, len, err = 0;
    double v;

    // XXX: should output parseable value: number or string
    v = neval_at(sp, e, row, col, &err);
    if (err) {
        snprintf(buf, sizeof buf - 1, "ERROR");
    } else
    if (!sempty(fmt)) {
        /* convert cell contents, do not test width, should not align */
        format(buf, sizeof buf - 1, s2c(fmt), sp->colfmt[col].precision, v, &align);
    } else {
        snprintf(buf, sizeof buf - 1, "%.15g", v);
    }
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
    string_free(fmt);
    efree(e);
}

void cmd_seval(sheet_t *sp, SCXMEM enode_t *e, int row, int col, int fd) {
    int err = 0;
    SCXMEM string_t *str;

    str = seval_at(sp, e, row, col, &err);
    if (!err) write(fd, s2c(str), slen(str));
    write(fd, "\n", 1);
    string_free(str);
    efree(e);
}

void cmd_query(sheet_t *sp, SCXMEM string_t *s, SCXMEM string_t *data, int fd) {
    char buf[FBUFLEN];
    int len;

    screen_goraw();
    len = query(sp, buf, sizeof buf, s ? s2c(s) : NULL, data ? s2c(data) : NULL);
    screen_deraw(0);
    if (len >= 0) {
        write(fd, buf, len);
        write(fd, "\n", 1);
    }

    string_free(s);
    string_free(data);
}

void cmd_getkey(sheet_t *sp, int fd) {
    char buf[32];
    int c, len;

    screen_goraw();
    c = nmgetch(0);
    screen_deraw(0);
    screen_get_keyname(buf, sizeof(buf) - 1, c);
    len = pstrcat(buf, sizeof buf, "\n");
    write(fd, buf, len);
}

void cmd_status(sheet_t *sp, int fd) {
    char buf[8];
    char *p = buf;
    if (sp->modflg)             *p++ = 'm';
    if (isatty(STDIN_FILENO))   *p++ = 'i';
    if (isatty(STDOUT_FILENO))  *p++ = 'o';
    *p++ = '\n';
    *p = '\0';
    write(fd, buf, p - buf);
}

void cmd_whereami(sheet_t *sp, int fd) {
    char buf[64];
    snprintf(buf, sizeof buf, "%s %s\n",
             cell_addr(sp, cellref_current(sp)),
             cell_addr(sp, cellref(sp->strow, sp->stcol)));
    write(fd, buf, strlen(buf));
}
