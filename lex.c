/*      SC      A Spreadsheet Calculator
 *              Lexical analyser
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              More mods by Alan Silverstein, 3/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <sys/stat.h>
#include <math.h>

#if defined(BSD42) || defined(BSD43)
# include <sys/ioctl.h>
#endif

#ifdef USE_IEEEFP_H
# include <ieeefp.h>
#endif

#include <signal.h>
#include <setjmp.h>
#include "sc.h"

static sigret_t fpe_trap(int);

#ifdef VMS
# include "gram_tab.h"
typedef union {
    int ival;
    double fval;
    SCXMEM string_t *sval;
    SCXMEM enode_t *enode;
    struct cellref cval;
    struct rangeref rval;
} YYSTYPE;
extern YYSTYPE yylval;
#else   /* VMS */
# include "y.tab.h"
#endif /* VMS */

#ifdef hpux
extern YYSTYPE yylval;
#endif /* hpux */

#if defined OPENBSD || defined FREEBSD
/* should be declared in y.tab.h */
extern int yyparse(void);
#endif

static jmp_buf wakeup;
static jmp_buf fpe_buf;

sc_bool_t sc_decimal = FALSE;

static sigret_t fpe_trap(int signo) {
    (void)signo;
#if defined(i386) && !defined(M_XENIX)
    asm("       fnclex");
    asm("       fwait");
#else
# ifdef IEEE_MATH
    fpsetsticky((fp_except)0);    /* Clear exception */
# endif /* IEEE_MATH */
# ifdef PC
    _fpreset();
# endif
#endif
    longjmp(fpe_buf, 1);
}

sigret_t time_out(int signo) {
    (void)signo;
    longjmp(wakeup, 1);
}

struct key {
    const char *key;
    int val;
};

#include "tokens.h"

/* Lexer context */
// XXX: should warp in a structure for recursive calls to parse_line()
static const char *src_line;
static const char *src_pos;
static sc_bool_t isgoto;
static sc_bool_t issetting;
static sc_bool_t colstate;
static int dateflag;

int parse_line(const char *buf) {
    int ret;
    while (isspacechar(*buf))
        buf++;
    src_pos = src_line = buf;
    isgoto = 0;
    issetting = 0;
    colstate = 0;
    dateflag = 0;
    ret = yyparse();
    src_pos = src_line = "";
    return ret;
}

void yyerror(const char *err) {
    parse_error(err, src_line, src_pos - src_line);
}

static int parse_cellref(const char *p, cellref_t *cp, int *lenp) {
    int i = 0, len, row, col, vf = 0;
    if (p[i] == '$') {
        i++;
        vf |= FIX_COL;
    }
    if ((col = atocol(p + i, &len)) < 0)
        return 0;
    i += len;
    if (p[i] == '$') {
        i++;
        vf |= FIX_ROW;
    }
    if (!isdigitchar(p[i]))
        return 0;
    row = p[i++] - '0';
    while (isdigitchar(p[i])) {
        row = row * 10 + (p[i++] - '0');
    }
    // XXX: should check for maximum row?
    *cp = cellref1(row, col, vf);
    if (lenp)
        *lenp = i;
    return 1;
}

static int lookup_name(struct key *tblp, const char *p, int len) {
    for (; tblp->key; tblp++) {
        // XXX: ugly hard coded case mapped comparison
        // XXX: the length test is bogus, accesses beyond the end of the string
        if (((tblp->key[0] ^ p[0]) & 0x5F) == 0 && tblp->key[len] == '\0') {
            /* Commenting the following line makes the search slower */
            /* but avoids access outside valid memory. A binary lookup would   */
            /* be the better alternative. */
            int i;
            for (i = 1; i < len && ((p[i] ^ tblp->key[i]) & 0x5F) == 0; i++)
                continue;
            if (i == len)
                return tblp->val;
        }
    }
    return -1;
}

int yylex(void) {
    char path[PATHLEN];
    const char *p = src_pos;
    const char *p0;
    int ret = -1, len;
    sc_bool_t isfunc = 0;
    struct nrange *r;

    for (;;) {
        if (isspacechar(*p)) {
            p++;
            continue;
        }
        if (*p == '[') {    /* syntax hint comment */
            while (*p && *p++ != ']')
                continue;
            continue;
        }
        if (*p == '\0') {
            ret = -1;
            break;
        }
        if (*p == '@' && isalphachar_(p[1])) {
            isfunc = 1;
            p++;
        }
        src_pos = p0 = p;
        if (isalphachar_(*p) || *p == '$') {
            // XXX: should only accept '$' in cell references
            for (p += 1; isalnumchar_(*p) || *p == '$'; p++)
                continue;

            if (p0 == src_line) {
                /* look up command name */
                if ((ret = lookup_name(cmdres, p0, p - p0)) >= 0) {
                    yylval.ival = ret;
                    /* set context for specific keywords */
                    /* accept column names for some commands */
                    colstate = (ret <= S_FORMAT);
                    if (ret == S_GOTO) isgoto = 1;
                    if (ret == S_SET) issetting = 1;
                    break;
                }
            }
            if (isfunc) {
                isfunc = 0;
                if ((yylval.ival = lookup_name(funcres, p0, p - p0)) >= 0) {
                    ret = yylval.ival;
                    break;
                }
                yylval.ival = ret = '@'; // unknown function name, return single '@'
                p = p0;
                break;
            }
            if (parse_cellref(p0, &yylval.cval, &len) && len == p - p0) {
                ret = VAR;
                break;
            }
            if (colstate && (yylval.ival = atocol(p0, &len)) >= 0 && len == p - p0) {
                ret = COL;
                break;
            }
            if (isgoto) {
                if ((yylval.ival = lookup_name(gotores, p0, p - p0)) >= 0) {
                    isgoto = 0;
                    ret = yylval.ival;
                    break;
                }
            }
            if (issetting) {
                if ((yylval.ival = lookup_name(settingres, p0, p - p0)) >= 0) {
                    ret = yylval.ival;
                    break;
                }
            }
            if (!find_nrange_name(p0, p - p0, &r)) {
                if (r->r_is_range) {
                    yylval.rval = rangeref(r->r_left.vp->row, r->r_left.vp->col,
                                           r->r_right.vp->row, r->r_right.vp->col);
                    ret = RANGE;
                    break;
                } else {
                    yylval.cval = cellref(r->r_left.vp->row, r->r_left.vp->col);
                    ret = VAR;
                    break;
                }
            } else
            if (plugin_exists(p0, p - p0, path, PATHLEN)) {
                // XXX: really catenate the rest of the input line?
                pstrcat(path, PATHLEN, p);
                yylval.sval = new_string(path);
                ret = PLUGIN;
                break;
            } else {
                yyerror("Unintelligible word");
                ret = WORD;
                break;
            }
        } else
        if ((*p == '.' && isdigitchar(p[1])) || isdigitchar(*p)) {
            sigret_t (*sig_save)(int);
            volatile double v = 0.0;
            int temp;
            const char *nstart = p;

            sig_save = signal(SIGFPE, fpe_trap);
            if (setjmp(fpe_buf)) {
                signal(SIGFPE, sig_save);
                // XXX: was: yylval.fval = v; but gcc complains about v getting clobbered
                yylval.fval = 0.0;
                error("Floating point exception\n");
                return FNUMBER;
            }

            if (*p == '.' && dateflag) {  /* .'s in dates are returned as tokens. */
                ret = *p++;
                dateflag--;
            } else {
                if (*p != '.') {
                    p0 = p;
                    do {
                        v = v * 10.0 + (double)((unsigned)*p++ - '0');
                    } while (isdigitchar(*p));
                    if (dateflag) {
                        ret = NUMBER;
                        yylval.ival = (int)v;
                        /*
                         *  If a string of digits is followed by two .'s separated by
                         *  one or two digits, assume this is a date and return the
                         *  .'s as tokens instead of interpreting them as decimal
                         *  points.  dateflag counts the .'s as they're returned.
                         */
                        // XXX: should parse date and return as such
                        // XXX: same for xx/xx/xx and xx:xx xx:xx:xx
                    } else
                    if (*p == '.' && isdigitchar(p[1]) &&
                        (p[2] == '.' || (isdigitchar(p[2]) && p[3] == '.'))) {
                        ret = NUMBER;
                        yylval.ival = (int)v;
                        dateflag = 2;
                    } else
                    if (*p == 'e' || *p == 'E') {
                        while (isdigitchar(*++p))
                            continue;
                        if (isalphachar_(*p)) {
                            // XXX: the whole word should be returned as a word.
                            src_pos = p;
                            return yylex();     // XXX: why a recursive call?
                        } else
                            ret = FNUMBER;
                    } else
                    if (isalphachar_(*p)) {
                        // XXX: the whole word should be returned as a word.
                        src_pos = p;
                        return yylex();     // XXX: why a recursive call?
                    }
                }
                if ((!dateflag && *p == '.') || ret == FNUMBER) {
                    char *endp;
                    ret = FNUMBER;
                    yylval.fval = strtod(nstart, &endp);
                    p = endp;
                    if (!isfinite(yylval.fval))
                        ret = F_ERR;
                    else
                        sc_decimal = TRUE;
                } else {
                    temp = (int)v;
                    if ((double)temp == v) {
                        /* A NUMBER is an integer in the range of `int`. */
                        /* it can be used for row numbers */
                        ret = NUMBER;
                        yylval.ival = temp;
                    } else {
                        ret = FNUMBER;
                        yylval.fval = v;
                    }
                }
            }
            signal(SIGFPE, sig_save);
            break;
        } else
        if (*p == '"') {
            const char *p1;
            char *ptr;
            p++;  /* skip the '"' */
            /* "string" or "string\"quoted\"" */
            for (p1 = p, len = 0; *p1 && *p1 != '"' && *p1 != '\n'; p1++) {
                if (*p1 == '\\' && (p1[1] == '"' || p1[1] == '\\'))
                    p1++;
                len++;
            }
            yylval.sval = new_string_len(NULL, len);
            ptr = yylval.sval->s;
            while (*p && *p != '"' && *p != '\n') {
                if (*p == '\\' && (p[1] == '"' || p[1] == '\\'))
                    p++;
                *ptr++ = *p++;
            }
            *ptr = '\0';
            if (*p == '"')
                p++;
            ret = STRING;
        } else {
            yylval.ival = ret = *p++;
        }
        break;
    }
    src_pos = p;
    return ret;
}

/*
* This is a very simpleminded test for plugins:  does the file merely exist
* in the plugin directories.  Perhaps should test for it being executable
*/

int plugin_exists(const char *name, int len, char *path, size_t size) {
#ifndef NOPLUGINS
    struct stat sb;
    char *homedir;

    if ((homedir = getenv("HOME"))) {
        if (snprintf(path, size, "%s/.sc/plugins/%.*s", homedir, len, name) >= (int)size)
            return 0;
    } else {
        if (snprintf(path, size, "%s/plugins/%.*s", LIBDIR, len, name) >= (int)size)
            return 0;
    }
    if (!stat(path, &sb))
        return 1;
#endif
    return 0;
}

/*
 * Given a token string starting with a symbolic column name and its valid
 * length, convert column name ("A"-"Z" or "AA"-"ZZ") to a column number (0-N).
 * Never mind if the column number is illegal (too high).  The procedure's name
 * and function are the inverse of coltoa().
 */

int atocol(const char *s, int *lenp) {
    int col, i = 0;

    if (!isalphachar(*s))
        return -1;

    // XXX: use more than 2 letters?
    col = toupperchar(s[i++]) - 'A';
    if (isalphachar(s[i]))
        col = (col + 1) * 26 + (toupperchar(s[i++]) - 'A');
    if (lenp)
        *lenp = i;
    return col;
}
