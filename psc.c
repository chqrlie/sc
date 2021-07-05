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

#include "sc.h"
#include "version.h"

#define END     0
#define NUM     1
#define ALPHA   2
#define SPACE   3
#define EOL     4

#define PRINTF_CMD_ERR(x) ": Writing command \"" x "\": %s\n"

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

const char *progname;

struct ent ***tbl;
int *fwidth;
int *precision;
int *realfmt;
int maxcols;
int curcol;
int currow;

static int scan(void);
static int getrow(const char *p);
static int getcol(const char *p);

//int growtbl(int rowcol, int toprow, int topcol);
//char *coltoa(int col);

static int curlen;
static int coff, roff;
static int first;
static int effr, effc;

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
static int strnums = FALSE;
static int plainnums = FALSE;
static char token[1000];

void fatal(const char *str) {
    fprintf(stderr, "psc: %s\n", str);
    exit(1);
}

void error(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    ((int (*)(FILE *, const char *, va_list))vfprintf)(stderr, fmt, ap);
    va_end(ap);
}

int main(int argc, char **argv) {
    int exit_status = EXIT_SUCCESS;
    int c;
    int i, j;
    char *p;

    progname = argv[0];
    while ((c = getopt(argc, argv, "rfLks:R:C:n:d:SPvh?")) != EOF) {
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
        case 'h':
        case '?':
            printf("usage: psc options\n"
                   "options:\n"
                   "  -L         Left justify strings.  Default is right justify.\n"
                   "  -r         Assemble data into rows first, not columns.\n"
                   "  -R n       Increment by n between rows\n"
                   "  -C n       Increment by n between columns\n"
                   "  -n n       Length of the row (column) should be n.\n"
                   "  -s v       Top left location in the spreadsheet should be v; eg, k5\n"
                   "  -d c       Use c as the delimiter between the fields.\n"
                   "  -k         Keep all delimiters - Default is strip multiple delimiters to 1.\n"
                   "  -f         suppress 'format' lines in output\n"
                   "  -S         Use strings vs numbers for numbers\n"
                   "  -P         Use numbers only when there is no [-+eE] (plain numbers only)\n");
            return 2;
        default:
            return EXIT_FAILURE;
        }
    }

    if (optind < argc) {
        fprintf(stderr, "%s: %d more argument(s) than expected\n",
                progname, argc - optind);
        return EXIT_FAILURE;
    }

    /* setup the spreadsheet arrays */
    if (!growtbl(GROWNEW, 0, 0))
        return EXIT_FAILURE;

    curlen = 0;
    curcol = c0; coff = 0;
    currow = r0; roff = 0;
    first = TRUE;

    for (;;) {

        effr = currow + roff;
        effc = curcol + coff;

        switch (scan()) {
        case END:
            if (drop_format) exit(exit_status);

            for (i = 0; i < maxcols; i++) {
                if (fwidth[i]) {
                    if (printf("format %s %d %d %d\n", coltoa(i),
                               fwidth[i] + 1, precision[i], REFMTFIX) < 0)
                    {
                        fprintf(stderr, "%s: Column %d" PRINTF_CMD_ERR("format"),
                                progname, i, strerror(errno));
                        exit_status = EXIT_FAILURE;
                        break;
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
                int ow, nw;

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

                if (printf("%s %s%d = \"%s\"\n", cmd, coltoa(effc), effr, token) < 0) {
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
    int c;
    char *p;
    int founddigit;

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
            ungetc(c, stdin);
        }
        return SPACE;
    }

    if (c == '\"') {
        while ((c = getchar()) != EOF && c != '\"' && c != '\n')
            *p++ = c;
        if (c != '\"')
            ungetc(c, stdin);
        *p = '\0';
        return ALPHA;
    }

    while (c != delim1 && c != delim2 && c != '\n' && c != EOF) {
        *p++ = c;
        c = getchar();
    }
    *p = '\0';
    ungetc(c, stdin);

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
static int getcol(const char *p) {
    int col = 0;
    if (!p)
        return 0;
    while (*p && !isalphachar(*p))
        p++;
    if (!*p)
        return 0;
    col = toupperchar(*p++) - 'A';
    if (isalphachar(*p))
        col = (col + 1) * 26 + (toupperchar(*p++) - 'A');
    return col;
}

/* given a string turn it into a row number */
static int getrow(const char *p) {
    int row = 0;
    if (!p)
        return 0;
    while (*p && !isdigitchar(*p))
        p++;
    while (isdigitchar(*p))
        row = row * 10 + *p++ - '0';
    return row;
}

/* turns a column number into [A-Z][A-Z] */
char *coltoa(int col) {
    static char rname[8];
    char *p = rname;

    if (col > 25) {
        *p++ = col / 26 + 'A' - 1;
        col %= 26;
    }
    *p++ = col + 'A';
    *p = '\0';
    return rname;
}

#define GROWALLOC(ptr, nelem, type, msg) \
    do { type *newptr__ = realloc(ptr, (nelem) * sizeof(type)); \
       if (newptr__ == NULL) { \
           error(msg); \
           return FALSE; \
       } \
       ptr = newptr__; \
    } while (0)

static const char nowider[] = "The table can't be any wider";

/*
 * grow the main && auxiliary tables (reset maxrows/maxcols as needed)
 * toprow &&/|| topcol tell us a better guess of how big to become.
 * we return TRUE if we could grow, FALSE if not....
 */
int growtbl(int rowcol, int toprow, int topcol) {
    int newcols;

    (void)toprow; /* unused */
    newcols = maxcols;
    if (rowcol == GROWNEW) {
        newcols = MINCOLS;
        maxcols = topcol = 0;
    }
    if (rowcol & GROWCOL) {
        if ((rowcol == GROWCOL) && ((maxcols == ABSMAXCOLS) || (topcol >= ABSMAXCOLS))) {
            error(nowider);
            return FALSE;
        }

        if (topcol > maxcols)
            newcols = GROWAMT + topcol;
        else
            newcols += GROWAMT;

        if (newcols > ABSMAXCOLS)
            newcols = ABSMAXCOLS;
    }

    if ((rowcol == GROWCOL) || (rowcol == GROWBOTH) || (rowcol == GROWNEW)) {
        GROWALLOC(fwidth, newcols, int, nowider);
        GROWALLOC(precision, newcols, int, nowider);
        GROWALLOC(realfmt, newcols, int, nowider);

        memzero(fwidth + maxcols, (newcols - maxcols) * sizeof(*fwidth));
        memzero(precision + maxcols, (newcols - maxcols) * sizeof(*precision));
        memzero(realfmt + maxcols, (newcols - maxcols) * sizeof(*realfmt));
    }

    maxcols = newcols;
    return TRUE;
}
