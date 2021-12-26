/*      SC      A Spreadsheet Calculator
 *              Printing routines
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#include "sc.h"

#define DEFCOLDELIM ':'

void printfile(sheet_t *sp, SCXMEM string_t *str, rangeref_t rr) {
    char field[FBUFLEN];
    buf_t(buf, FBUFLEN);
    FILE *f;
    int pid = -1;
    int fieldlen, nextcol;
    int row, col;
    char path[PATHLEN];
    char *ext;
    const char *fname = str ? s2c(str) : NULL;

    if (fname) {
        /* printfile will be the [path/]file ---> [path/]file.out */
        if (*fname == '\0') {
            pstrcpy(path, sizeof path, sp->curfile);
            ext = get_extension(path);

            /* keep the extension unless .sc or scext */
            if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, s2c(scext))))
                ext += strlen(ext);

            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     ascext ? s2c(ascext) : "asc");
        } else {
            /* strarg in gram.y, always size of \0 terminated string. */
            pstrcpy(path, sizeof path, fname);
        }
        if (!strcmp(path, sp->curfile)
        &&  !yn_ask("Confirm that you want to destroy the data base: (y,n)")) {
            string_free(str);
            return;
        }

        if ((f = openfile(path, sizeof path, &pid, NULL)) == NULL) {
            error("Cannot create file \"%s\"", path);
            string_free(str);
            return;
        }
    } else {
        f = stdout;
    }

    for (row = rr.left.row; row <= rr.right.row; row++) {
        int w = 0;

        if (row_hidden(sp, row))
            continue;

        buf_reset(buf);
        for (col = rr.left.col; col <= rr.right.col; col = nextcol, w += fieldlen) {
            struct ent *p = getcell(sp, row, col);
            const char *s;
            int align, len;

            fieldlen = 0;
            nextcol = col + 1;
            if (col_hidden(sp, col)) {
                continue;
            }

            // XXX: should handle cell fusion
            fieldlen = col_fwidth(sp, col);
            if (!p)
                continue;

            if (buf_extend(buf, w + fieldlen + 2, FBUFLEN))
                goto malloc_error;

            // XXX: alignment should be determined from cell format
            //      ALIGN_AUTO, ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT

            align = p->flags & ALIGN_MASK;

            if (p->type == SC_NUMBER || p->type == SC_ERROR || p->type == SC_BOOLEAN) {
                if (p->type == SC_ERROR) {
                    // XXX: append a space for cell alignment
                    len = pstrcpy(field, sizeof field, error_name[p->cellerror]);
                    align |= ALIGN_CLIP;
                } else
                if (p->type == SC_BOOLEAN) {
                    len = pstrcpy(field, sizeof field, boolean_name[!!p->v]);
                    if (!align)
                        align = ALIGN_CENTER;
                } else {
                    if (p->format) {
                        len = format(field, sizeof field, s2c(p->format), sp->colfmt[col].precision, p->v, &align);
                    } else {
                        len = engformat(field, sizeof field, sp->colfmt[col].realfmt, sp->colfmt[col].precision, p->v, &align);
                    }
                }
                if ((int)buf->len < w) {
                    buf_repc(buf, ' ', w - buf->len);
                } else {
                    buf->buf[buf->len = w] = '\0';
                }
                if (align & ALIGN_CLIP) {
                    if (len < 0)
                        len = 0;
                    if (len > fieldlen)
                        len = fieldlen;
                    field[len] = '\0';
                    align &= ~ALIGN_CLIP;
                }
                if (len < 0 || len > fieldlen) {
                    buf_repc(buf, '*', fieldlen);
                } else
                if (align == ALIGN_LEFT) {
                    buf_printf(buf, "%-*s", fieldlen, field);
                } else
                if (align == ALIGN_CENTER) {
                    int half = (fieldlen - len) / 2;
                    buf_printf(buf, "%*s%*s", half, field, len - half, "");
                } else {
                    buf_printf(buf, "%*s", fieldlen, field);
                }
            } else
            if (p->type == SC_STRING) {
                int slen = strlen(s = s2str(p->label));
                int pad = 0;
                int soff = 0;

                if (*s == '\\' && slen > 1) {
                    /* A string starting with a backslash is repeated across
                       the column width. */
                    int remain = fieldlen;
                    slen -= 1;  /* skip the '\' */
                    s += 1;
                    while (remain > 0) {
                        int chunk = slen <= remain ? slen : remain;
                        buf_put(buf, s, chunk);
                        remain -= chunk;
                    }
                    continue;
                }
                /* Figure out if the label slops over to a blank field. */
                while (slen > fieldlen && nextcol <= rr.right.col) {
                    if (!col_hidden(sp, nextcol)) {
                        struct ent *nc = getcell(sp, row, nextcol);
                        if (nc && (nc->type || nc->expr))
                            break;
                        fieldlen += col_fwidth(sp, nextcol);
                    }
                    nextcol++;
                }
                switch (p->flags & ALIGN_MASK) {
                default:
                case ALIGN_LEFT:
                    pad = w - buf->len;
                    if (slen > fieldlen)
                        slen = fieldlen;
                    break;
                case ALIGN_CENTER:
                    pad = w - buf->len + (col_fwidth(sp, nextcol) - slen) / 2;
                    if (pad < 0) {
                        soff = -pad;
                        slen -= soff;
                    }
                    if ((int)buf->len + pad + slen > w + fieldlen)
                        slen = w + fieldlen - buf->len - pad;
                    break;
                case ALIGN_RIGHT:
                    pad = w - buf->len + fieldlen - slen;
                    if (pad < 0) {
                        soff = -pad;
                        slen -= soff;
                        pad = 0;
                    }
                    break;
                }

                if (buf_extend(buf, w + fieldlen + 2, FBUFLEN))
                    goto malloc_error;

                buf_repc(buf, ' ', pad);
                buf_put(buf, s + soff, slen);
                if (nextcol <= rr.right.col)
                    buf_repc(buf, ' ', w + fieldlen - buf->len);
            }
        }
        buf_putc(buf, '\n');
        fputs(buf->buf, f);
    }
    if (0) {
    malloc_error:
        error("Realloc failed in printfile()");
    }
    buf_free(buf);
    if (fname) closefile(f, pid, 0);
    string_free(str);
}

/* unspecial (backquote) things that are special chars in a table */
static void unspecial(sheet_t *sp, FILE *f, const char *str, int delim) {
    if (*str == '\\') str++; /* delete wheeling string operator, OK? */
    while (*str) {
        if (((sp->tbl_style == LATEX) || (sp->tbl_style == SLATEX) ||
             (sp->tbl_style == TEX)) &&
            ((*str == delim) || (*str == '$') || (*str == '#') ||
             (*str == '%') || (*str == '{') || (*str == '}') ||
             (*str == '&')))
            putc('\\', f);
        putc(*str, f);
        str++;
    }
}

void tblprintfile(sheet_t *sp, SCXMEM string_t *str, rangeref_t rr) {
    FILE *f;
    int pid;
    int row, col, ncols = rr.right.col - rr.left.col + 1;
    char coldelim = DEFCOLDELIM;
    char path[PATHLEN];
    char *ext;
    const char *fname = s2str(str);

    /* tblprintfile will be the [path/]file ---> [path/]file.out */
    if (*fname == '\0') {
        pstrcpy(path, sizeof path, sp->curfile);
        ext = get_extension(path);

        /* keep the extension unless .sc or scext */
        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, s2c(scext))))
            ext += strlen(ext);

        if (sp->tbl_style == 0) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     tbl0ext ? s2c(tbl0ext) : "cln");
        } else
        if (sp->tbl_style == TBL) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     tblext ? s2c(tblext) : "tbl");
        } else
        if (sp->tbl_style == LATEX) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     latexext ? s2c(latexext) : "lat");
        } else
        if (sp->tbl_style == SLATEX) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     slatexext ? s2c(slatexext) : "stx");
        } else
        if (sp->tbl_style == TEX) {
            snprintf(ext, path + sizeof(path) - ext, ".%s",
                     texext ? s2c(texext) : "tex");
        }
    } else {
        pstrcpy(path, sizeof path, fname);
    }
    if (!strcmp(path, sp->curfile)
    &&  !yn_ask("Confirm that you want to destroy the data base: (y,n)")) {
        string_free(str);
        return;
    }

    if ((f = openfile(path, sizeof path, &pid, NULL)) == NULL) {
        error("Cannot create file \"%s\"", path);
        string_free(str);
        return;
    }

    if (sp->tbl_style == TBL) {
        fprintf(f, ".\\\" ** %s spreadsheet output \n.TS\n", progname);
        fprintf(f, "tab(%c);\n", coldelim);
        for (col = rr.left.col; col <= rr.right.col; col++)
            fprintf(f, " n");
        fprintf(f, ".\n");
    } else
    if (sp->tbl_style == LATEX) {
        fprintf(f, "%% ** %s spreadsheet output\n\\begin{tabular}{", progname);
        for (col = rr.left.col; col <= rr.right.col; col++)
            fprintf(f, "c");
        fprintf(f, "}\n");
        coldelim = '&';
    } else
    if (sp->tbl_style == SLATEX) {
        fprintf(f, "%% ** %s spreadsheet output\n!begin<tabular><", progname);
        for (col = rr.left.col; col <= rr.right.col; col++)
            fprintf(f, "c");
        fprintf(f, ">\n");
        coldelim = '&';
    } else
    if (sp->tbl_style == TEX) {
        fprintf(f, "{\t%% ** %s spreadsheet output\n\\settabs %d \\columns\n",
                progname, ncols);
        coldelim = '&';
    } else
    if (sp->tbl_style == FRAME) {
        fprintf(f, "<MIFFile 3.00> # generated by the sc spreadsheet calculator\n");
        fprintf(f, "<Tbls\n");
        fprintf(f, " <Tbl \n");
        fprintf(f, "  <TblID 1> # This table's ID is 1\n");
        fprintf(f, "  <TblFormat \n");
        fprintf(f, "   <TblTag `Format A'> # Table Format Catalog\n");
        fprintf(f, "  > # end of TblFormat\n");
        fprintf(f, "  <TblNumColumns %d> # Has %d columns\n", ncols, ncols);
        fprintf(f, "  <TblTitleContent\n");
        fprintf(f, "   <Para\n");
        fprintf(f, "    <PgfTag `TableTitle'> # Forces lookup in Paragraph Format Catalog\n");
        fprintf(f, "    <ParaLine\n");
        fprintf(f, "     <String `%s'>\n", fname);
        fprintf(f, "    > # end of ParaLine\n");
        fprintf(f, "   > # end of Para\n");
        fprintf(f, "  > # end of TblTitleContent\n");
        fprintf(f, "  <TblH # The heading\n");
        fprintf(f, "   <Row # The heading row\n");
        for (col = rr.left.col; col <= rr.right.col; col++) {
            fprintf(f, "    <Cell <CellContent <Para # Cell in column \n");
            fprintf(f, "       <PgfTag `CellHeading'> # in Paragraph Format Catalog\n");
            // XXX: incorrect for columns beyond 25
            fprintf(f, "       <ParaLine <String `%c'>>\n", 'A'+col);
            fprintf(f, "    >>> # end of Cell\n");
        }
        fprintf(f, "   > # end of Row\n");
        fprintf(f, "  > # end of TblH\n");
        fprintf(f, "  <TblBody # The body\n");
    }

    for (row = rr.left.row; row <= rr.right.row; row++) {
        // XXX: print hidden rows?

        if (sp->tbl_style == TEX)
            fprintf(f, "\\+");
        else
        if (sp->tbl_style == FRAME) {
            fprintf(f, "   <Row # The next body row\n");
        }

        for (col = rr.left.col; col <= rr.right.col; col++) {
            struct ent *p = getcell(sp, row, col);

            // XXX: print hidden columns?
            // XXX: should handle cell fusion

            if (sp->tbl_style == FRAME) {
                fprintf(f, "    <Cell <CellContent <Para\n");
                fprintf(f, "       <PgfTag `CellBody'> # in Paragraph Format Catalog\n");
                fprintf(f, "       <ParaLine <String `");
            }
            if (p) {
                char field[FBUFLEN];
                int align = p->flags & ALIGN_MASK;

                if (p->type == SC_NUMBER || p->type == SC_ERROR || p->type == SC_BOOLEAN) {
                    /* convert cell contents, do not test width, do not align with spaces */
                    // XXX: should implement alignment in output format
                    if (p->type == SC_ERROR) {
                        pstrcpy(field, sizeof field, error_name[p->cellerror]);
                        align |= ALIGN_CLIP;
                    } else
                    if (p->type == SC_BOOLEAN) {
                        pstrcpy(field, sizeof field, boolean_name[!!p->v]);
                        if (!align)
                            align = ALIGN_CENTER;
                    } else {
                        if (p->format) {
                            format(field, sizeof field, s2c(p->format), sp->colfmt[col].precision, p->v, &align);
                        } else {
                            engformat(field, sizeof field, sp->colfmt[col].realfmt, sp->colfmt[col].precision, p->v, &align);
                        }
                    }
                    // XXX: should fill fieldlen with * if too long
                    unspecial(sp, f, field, coldelim);
                } else
                if (p->type == SC_STRING) {
                    // XXX: should handle repeated pattern starting with '\'
                    unspecial(sp, f, s2str(p->label), coldelim);
                }
            }
            if (sp->tbl_style == FRAME) {
                fprintf(f, "'>>\n");
                fprintf(f, "    >>> # end of Cell\n");
            }
            if (col < rr.right.col) {
                if (sp->tbl_style != FRAME)
                    fprintf(f, "%c", coldelim);
            }
        }
        if (sp->tbl_style == LATEX) {
            if (row < rr.right.row) fprintf (f, "\\\\");
        } else
        if (sp->tbl_style == SLATEX) {
            if (row < rr.right.row) fprintf(f, "!!");
        } else
        if (sp->tbl_style == TEX) {
            fprintf (f, "\\cr");
        } else
        if (sp->tbl_style == FRAME) {
            fprintf(f, "   > # end of Row\n");
        }
        fprintf(f, "\n");
    }

    if (sp->tbl_style == TBL)
        fprintf (f,".TE\n.\\\" ** end of %s spreadsheet output\n", progname);
    else
    if (sp->tbl_style == LATEX)
        fprintf(f, "\\end{tabular}\n%% ** end of %s spreadsheet output\n", progname);
    else
    if (sp->tbl_style == SLATEX)
        fprintf (f,"!end<tabular>\n%% ** end of %s spreadsheet output\n", progname);
    else
    if (sp->tbl_style == TEX)
        fprintf (f,"}\n%% ** end of %s spreadsheet output\n", progname);
    else
    if (sp->tbl_style == FRAME) {
        fprintf(f, "  > # end of TblBody\n");
        fprintf(f, " ># end of Tbl\n");
        fprintf(f, "> # end of Tbls\n");
        fprintf(f, "<TextFlow <Para \n");
        fprintf(f, "  <PgfTag Body> \n");
        fprintf(f, "  <ParaLine <ATbl 1>> # Reference to table ID 1\n");
        fprintf(f, ">>\n");
    }

    closefile(f, pid, 0);
    string_free(str);
}
