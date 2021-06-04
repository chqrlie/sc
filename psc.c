/* Sc parse routine
 *
 * usage psc options
 * options:
 *   -L         Left justify strings.  Default is right justify.
 *   -r         Assemble data into rows first, not columns.
 *   -R n       Increment by n between rows
 *   -C n       Increment by n between columns
 *   -n n       Length of the row (column) should be n.
 *   -s v       Top left location in the spreadsheet should be v; eg, k5
 *   -d c       Use c as the delimiter between the fields.
 *   -k         Keep all delimiters - Default is strip multiple delimiters to 1.
 *   -f         suppress 'format' lines in output
 *   -S         Use strings vs numbers for numbers
 *   -P         Use numbers only when there is no [-+eE] (plain numbers only)
 *
 *  Author: Robert Bond
 *  Adjustments: Jeff Buhrt, Eric Putz and Chuck Martin
 */

#include "config.h"
#include "version.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#ifdef HAVE_STDBOOL_H
# include <stdbool.h>
#else
# if !HAVE__BOOL
#  define _Bool int
# endif
# define bool _Bool
# define true 1
# define false 0
#endif
#include <limits.h>
#include <string.h>
#include <errno.h>
#include "sc.h"

#define END     0
#define NUM     1
#define ALPHA   2
#define SPACE   3
#define EOL     4

#define PRINTF_CMD_ERR(x) ": Writing command \"" x "\": %s\n"

char *coltoa(int col);
const char *progname;
int getrow(char *p);
int getcol(char *p);
static int scan(void);

int *fwidth;
int *precision;
int maxcols;
int *realfmt;

static int curlen;
int curcol;
static int coff;
int currow;
static int roff;
static int first;
static int effr, effc;
static int exit_status = EXIT_SUCCESS;

/* option flags reset */
static int colfirst = FALSE;
static int leftadj = FALSE;
static int r0 = 0;
static int c0 = 0;
static int rinc = 1;
static int cinc = 1;
static int len = 20000;
static char delim1 = ' ';
static char delim2 = '\t';
static int strip_delim = TRUE;
static int drop_format = FALSE;
static int strnums      = FALSE;
static int plainnums    = FALSE;

static char token[1000];

int main(int argc, char **argv)
{
    int c;
    int i, j;
    register char *p;

    progname = argv[0];
    while ((c = getopt(argc, argv, "rfLks:R:C:n:d:SPv")) != EOF) {
        switch (c) {
        case 'r':
            colfirst = TRUE;
            break;
        case 'L':
            leftadj = TRUE;
            break;
        case 's':
            c0 = getcol(optarg);
            r0 = getrow(optarg);
            break;
        case 'R':
            rinc = atoi(optarg);
            break;
        case 'C':
            cinc = atoi(optarg);
            break;
        case 'n':
            len = atoi(optarg);
            break;
        case 'd':
            delim1 = optarg[0];
            delim2 = '\0';
            break;
        case 'k':
            strip_delim = FALSE;
            break;
        case 'f':
            drop_format = TRUE;
            break;
        case 'S':
            strnums = TRUE;
            break;
        case 'P':
            plainnums = TRUE;
            break;
        case 'v':
            fprintf(stderr,"%s: %s\n", progname, rev);
            return 0;
        default:
            exit(EXIT_FAILURE);
        }
    }

    if (optind < argc) {
            fprintf(stderr, "%s: %d more argument(s) than expected\n",
                progname, argc - optind);
            exit(EXIT_FAILURE);
    }

        /* setup the spreadsheet arrays */
    if (!growtbl(GROWNEW, 0, 0))
        exit(EXIT_FAILURE);

    curlen = 0;
    curcol = c0; coff = 0;
    currow = r0; roff = 0;
    first = TRUE;

    while (1) {

        effr = currow+roff;
        effc = curcol+coff;

        switch (scan()) {
        case END:
                if (drop_format) exit(exit_status);

                for (i = 0; i<maxcols; i++) {
                        if (fwidth[i]) {
                                if (printf("format %s %d %d %d\n", coltoa(i),
                                                fwidth[i]+1, precision[i], REFMTFIX) < 0)
                                {
                                        fprintf(stderr, "%s: Column %d" PRINTF_CMD_ERR("format"),
                                                progname, i, strerror(errno));
                                        exit(EXIT_FAILURE);
                                }
                        }
                }

                exit(exit_status);
        case NUM:
                first = FALSE;

                if (printf("let %s%d = %s\n", coltoa(effc), effr, token) < 0) {
                        fprintf(stderr, "%s" PRINTF_CMD_ERR("let"), progname,
                                strerror(errno));
                        exit_status = EXIT_FAILURE;
                }

                if (effc >= maxcols - 1) {
                        if (!growtbl(GROWCOL, 0, effc)) {
                                fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
                                exit_status = EXIT_FAILURE;
                                continue;
                        }
                }

                i = 0;
                j = 0;
                p = token;

                while (*p && *p != '.') {
                        p++; i++;
                }

                if (*p) {
                        p++; i++;
                }

                while (*p) {
                        p++; i++; j++;
                }

                {
                        int     ow, nw;

                        ow = fwidth[effc] - precision[effc];

                        if (precision[effc] < j)
                                precision[effc] = j;

                        if (fwidth[effc] < i)
                                fwidth[effc] = i;

                        /* now make sure:
                        *       1234.567890 (format 11 6)
                        *       1234567.890 (format 11 3)
                        *       both show (format 14 6)
                        *               (really it uses 15 6 to separate columns)
                        */
                        if ((nw = i - j) > ow)
                                fwidth[effc] += nw - (fwidth[effc] - precision[effc]);
                }
                break;
        case ALPHA:
                first = FALSE;

                {
                        const char *cmd = leftadj ? "leftstring" : "rightstring";

                        if (printf("%s %s%d = \"%s\"\n", cmd, coltoa(effc),effr,token) < 0) {
                                fprintf(stderr, "%s: Writing command \"%s\": %s\n", progname,
                                        cmd, strerror(errno));
                                exit_status = EXIT_FAILURE;
                        }
                }

                if (effc >= maxcols - 1 && !growtbl(GROWCOL, 0, effc)) {
                        fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
                        exit_status = EXIT_FAILURE;
                        continue;
                }

                i = strlen(token);

                if (i > fwidth[effc]) {
                        fwidth[effc] = i;
                }

                break;
        case SPACE:
            if (first && strip_delim)
                break;
            if (colfirst)
                roff++;
            else
                coff++;
            break;
        case EOL:
            curlen++;
            roff = 0;
            coff = 0;
            first = TRUE;
            if (colfirst) {
                if (curlen >= len) {
                    curcol = c0;
                    currow += rinc;
                    curlen = 0;
                } else {
                    curcol += cinc;
                }
            } else {
                if (curlen >= len) {
                    currow = r0;
                    curcol += cinc;
                    curlen = 0;
                } else {
                    currow += rinc;
                }
            }
            break;
        }
    }
}

static int scan(void) {
    register int c;
    register char *p;
    register int founddigit;

    p = token;
    c = getchar();

    if (c == EOF)
        return END;

    if (c == '\n')
        return EOL;

    if (c == delim1 || c == delim2) {
        if (strip_delim) {
            while ((c = getchar()) != EOF && (c == delim1 || c == delim2))
                continue;
            (void)ungetc(c, stdin);
        }
        return SPACE;
    }

    if (c == '\"') {
        while ((c = getchar()) != EOF && c != '\"' && c != '\n')
            *p++ = c;
        if (c != '\"')
            (void)ungetc(c, stdin);
        *p = '\0';
        return ALPHA;
    }

    while (c != delim1 && c != delim2 && c != '\n' && c != EOF) {
        *p++ = c;
        c = getchar();
    }
    *p = '\0';
    (void)ungetc(c, stdin);

    p = token;
    c = (unsigned char)*p;
    founddigit = FALSE;
    /*
     * str_nums always returns numbers as strings
     * plainnums returns 'numbers' with [-+eE] in them as strings
     * lastprtnum makes sure a number ends in one of [0-9eE.]
     */
    if (!strnums && (isdigit(c) || c == '.' || c == '-' || c == '+')) {
        int lastprtnum = FALSE;

        while (isdigit(c) || c == '.' ||
               (!plainnums && (c == '-' || c == '+' || c == 'e' || c == 'E')))
        {
            if (isdigit(c))
                lastprtnum = founddigit = TRUE;
            else
            if (!(c == '.' || c == 'e' || c == 'E'))
                lastprtnum = FALSE;
            c = (unsigned char)*p++;
        }
        if (c == '\0' && founddigit && lastprtnum)
            return NUM;
        else
            return ALPHA;
    }

    return ALPHA;
}

/* turns [A-Z][A-Z] into a number */
int getcol(char *p)
{
    int col;

    col = 0;
    if (!p)
        return 0;
    while (*p && !isalphachar(*p))
        p++;
    if (!*p)
        return 0;
    col = toupperchar(*p) - 'A';
    if (isalphachar(*++p))
        col = (col + 1) * 26 + (toupperchar(*p) - 'A');
    return col;
}

/* given a string turn it into a row number */
int getrow(char *p)
{
    int row = 0;
    if (!p)
        return 0;
    while (*p && !isdigitchar(*p))
        p++;
    while (isdigitchar(*p)) {
        row = row * 10 + *p - '0';
        p++;
    }
    return row;
}

/* turns a column number into [A-Z][A-Z] */
char *coltoa(int col)
{
    static unsigned int bufn;
    static char buf[4][4];
    char *rname = buf[bufn++ & 3];
    char *p = rname;

    if (col < 0 || col > 27 * 26) { /* A-Z, AA-ZZ */
        fprintf(stderr,"coltoa: invalid col: %d", col);
        exit_status = EXIT_FAILURE;
    }

    if (col > 25) {
        *p++ = col / 26 + 'A' - 1;
        col %= 26;
    }

    *p++ = col + 'A';
    *p = '\0';
    return rname;
}
