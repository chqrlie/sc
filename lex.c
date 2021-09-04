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
    short val, op;
};

#include "tokens.h"

/* Lexer context */
// XXX: should warp in a structure for recursive calls to parse_line()
static const char *src_line;
static const char *src_pos;
static sc_bool_t isexpr;
static sc_bool_t isgoto;
static sc_bool_t issetting;
static sc_bool_t colstate;
static int dateflag;

int parse_line(const char *buf) {
    int ret;
    while (isspacechar(*buf))
        buf++;
    src_pos = src_line = buf;
    isexpr = 0;
    isgoto = 0;
    issetting = 0;
    colstate = 0;
    dateflag = 0;
    ret = yyparse();
    src_pos = src_line = "";
    return ret;
}

void yyerror(const char *err) {
    parse_error(err, src_line, src_pos);
}

int parse_cellref(const char *p, cellref_t *cp, int *lenp) {
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
    // XXX: should reject if isalnumchar_(p[i]) ?
    return 1;
}

int parse_rangeref(const char *p, rangeref_t *rp, int *lenp) {
    int len, len2, res = 1;
    if (parse_cellref(p, &rp->left, &len)) {
        rp->right = rp->left;
        if (p[len] == ':'
        &&  parse_cellref(p + len + 1, &rp->right, &len2)
        &&  !isalnumchar_(p[len += 1 + len2])) {
            res = 2;
        }
        if (lenp) {
            *lenp = len;
            return res;
        } else
        if (p[len] == '\0') {
            return res;
        }
    }
    return 0;
}

static int parse_int(const char *p, const char **endp, int *vp) {
    if (isdigitchar(*p)) {
        int val = *p++ - '0';
        while (isdigitchar(*p)) {
            val = val * 10 + (*p++ - '0');
        }
        *vp = val;
        *endp = p;
        return 1;
    }
    return 0;
}

static int parse_float(const char *p, const char **endp, double *vp) {
    // XXX: should ignore exponent
    char *end;
    double val = strtod(p, &end);
    if (end > p) {
        *endp = end;
        *vp = val;
        return 1;
    }
    return 0;
}

static int parse_time(const char *p, const char **endp, double *vp) {
    int h, m, s = 0;
    double ms = 0;
    if (parse_int(p, &p, &h) && *p == ':' && parse_int(p + 1, &p, &m)) {
        if (*p == ':' && parse_int(p + 1, &p, &s)) {
            if (*p == '.')
                parse_float(p, &p, &ms);
        }
        *endp = p;
        *vp = (ms + s + m * 60 + h * 3600) / 86400.0;
    }
    return 0;
}

static int parse_date(const char *s, const char **endp, double *vp) {
    static int const days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    const char *p;
    int d, m, y;
    double t = 0;
    // XXX: should swap d and m if current locale has m/d/y
    if ((parse_int(s, &p, &d) && *p == '/' && parse_int(p + 1, &p, &m) && *p == '/' && parse_int(p + 1, &p, &y))
    ||  (parse_int(s, &p, &y) && *p == '.' && parse_int(p + 1, &p, &m) && *p == '.' && parse_int(p + 1, &p, &d)))
    {
        if (*p == ' ') {
            parse_time(p + 1, &p, &t);
        }
        if (m > 0 && m <= 12 && d > 0 && d <= days[m - 1] + (!(y % 4) && ((y % 100) || !(y % 400)))) {
            int mon, year = y - 1968;    /* base 1968 */
            if ((mon = m + 1) < 4) {
                year -= 1;
                mon += 12;
            }
            /* compute day number from 1968-3-1 and offset back to 1970-1-1 */
            /* 794 = 123 + (366 + 365 - 60) */
            // XXX: should use 1904-1-1 for XL compatibility
            *vp = t + year * 1461 / 4 + mon * 153 / 5 + d - 794 /* + 24107 */;
            *endp = p;
            return 1;
        }
    }
    return 0;
}

static int lookup_fname(const char *p, int len, int *pop) {
    const struct opdef *opp = opdefs;
    const char *fname;
    int op;

    for (op = 0; op < OP_count; op++, opp++) {
        if (!(fname = opp->name))
            continue;
        if (*fname == '@')
            fname++;
        if (!sc_strncasecmp(fname, p, len) && (fname[len] == '\0' || fname[len] == '(')) {
            *pop = op;
            switch (opp->min) {
            case 0:     if (opp->max == 0) return FUNC0;
                        if (opp->max == 1) return FUNC01;
                        break;
            case 1:     if (opp->max == 1) return FUNC1;
                        if (opp->max == 2) return FUNC12;
                        if (opp->max == 3) return FUNC13;
                        if (opp->max == -1) return FUNC1x;
                        break;
            case 2:     if (opp->max == 2) return FUNC2;
                        if (opp->max == 3) return FUNC23;
                        if (opp->max == -1) return FUNC2x;
                        break;
            case 3:     if (opp->max == 3) return FUNC3;
                        if (opp->max == 4) return FUNC34;
                        if (opp->max == 5) return FUNC35;
                        break;
                        // XXX: other combinations: 1/4, 2/4, 4/4, 5/5
            }
            return -1;
        }
    }
    return -1;
}

#if 1
static int compare_name(const char *p, int len, const char *str) {
    while (len --> 0) {
        int cmp = toupperchar(*p++) - *str++;
        if (cmp != 0)
            return cmp;
    }
    return (*str == '\0') ? 0 : -1;
}

/* Use binary sort, assuming keywords are all uppercase and sorted */
static int lookup_name(const struct key *tblp, size_t count, const char *p, int len, int *pop) {
    if (len --> 0) {
        size_t a = 0, b = count;
        char c = toupperchar(*p++);
        int cmp;
        while (a < b) {
            size_t n = (a + b) >> 1;
            const struct key *tp = tblp + n;
            if (!(cmp = c - tp->key[0]) && !(cmp = compare_name(p, len, tp->key + 1))) {
                *pop = tp->op;
                return tp->val;
            }
            if (cmp < 0) {
                b = n;
            } else {
                a = n + 1;
            }
        }
    }
    return -1;
}
#else
static int lookup_name(const struct key *tblp, size_t count, const char *p, int len, int *pop) {
    // XXX: should use binary search
    for (; count --> 0; tblp++) {
        // XXX: ugly hard coded case mapped comparison
        // XXX: the length test is bogus, accesses beyond the end of the string
        if (((tblp->key[0] ^ p[0]) & 0x5F) == 0 && tblp->key[len] == '\0') {
            /* Commenting the following line makes the search slower */
            /* but avoids access outside valid memory. A binary lookup would   */
            /* be the better alternative. */
            int i;
            for (i = 1; i < len && ((p[i] ^ tblp->key[i]) & 0x5F) == 0; i++)
                continue;
            if (i == len) {
                *pop = tblp->op;
                return tblp->val;
            }
        }
    }
    return -1;
}
#endif

int yylex(void) {
    char path[PATHLEN];
    const char *p = src_pos;
    const char *p0;
    int ret = -1, len, op;
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
        if (*p == '=')
            isexpr = 1;

        if (*p == '@' && isalphachar_(p[1])) {
            isfunc = 1;
            p++;
        }
        src_pos = p0 = p;
        if (isalphachar_(*p) || *p == '$') {
            // XXX: should only accept '$' in cell references
            for (p += 1; isalnumchar_(*p) || *p == '$' || *p == '.'; p++)
                continue;

            if (p0 == src_line) {
                /* look up command name */
                if ((ret = lookup_name(cmdres, countof(cmdres), p0, p - p0, &yylval.ival)) >= 0) {
                    /* set context for specific keywords */
                    /* accept column names for some commands */
                    colstate = (ret <= S_FORMAT);
                    if (ret == S_GOTO) isgoto = 1;
                    if (ret == S_SET) issetting = 1;
                    if (ret == S_EVAL || ret == S_SEVAL) isexpr = 1;
                    break;
                }
                // XXX: otherwise unknown command?
            }
            if (isexpr) {
                if ((ret = lookup_fname(p0, p - p0, &op)) >= 0) {
                    if (isfunc || *p == '(' || op == OP_TRUE || op == OP_FALSE) {
                        yylval.ival = op;
                        isfunc = 0;
                        break;
                    }
                } else {
                    // XXX: should accept unknown function name and create node
                    //      for later re-editing the formula and/or saving it.
                    if (isfunc) {
                        isfunc = 0;
                        yylval.ival = ret = '@'; // unknown function name, return single '@'
                        p = p0 + 1;
                        break;
                    }
                }
            }
            if (parse_cellref(p0, &yylval.cval, &len) && len == p - p0) {
                cellref_t c2;
                if (*p == ':' && parse_cellref(p + 1, &c2, &len) && !isalnumchar_(p[len+1])) {
                    yylval.rval.left = yylval.cval;
                    yylval.rval.right = c2;
                    p += 1 + len;
                    ret = RANGE;
                    break;
                } else {
                    ret = VAR;
                    break;
                }
            }
            if (colstate && (yylval.ival = atocol(p0, &len)) >= 0 && len == p - p0) {
                ret = COL;
                break;
            }
            if (isgoto) {
                if ((ret = lookup_name(gotores, countof(gotores), p0, p - p0, &yylval.ival)) >= 0) {
                    isgoto = 0;
                    break;
                }
            }
            if (issetting) {
                if ((ret = lookup_name(settingres, countof(settingres), p0, p - p0, &yylval.ival)) >= 0)
                    break;
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
                yylval.sval = string_new(path);
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

            if (p == src_line && parse_time(p, &p, &yylval.fval)) {
                ret = FNUMBER;
                break;
            }
            if (parse_date(p, &p, &yylval.fval)) {
                ret = FNUMBER;
                break;
            }
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
                    if (!isfinite(yylval.fval)) {
                        yylval.ival = ERROR_NUM;
                        ret = T_ERROR;
                    } else {
                        sc_decimal = TRUE;
                    }
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
        if (*p == '#') {
            int i;
            for (i = 1; i < ERROR_count; i++) {
                len = strlen(error_name[i]);
                if (!sc_strncasecmp(p, error_name[i], len)) {
                    p += len;
                    yylval.ival = i;
                    ret = T_ERROR;
                    break;
                }
            }
            if (i == ERROR_count) {
                yylval.ival = ret = *p++;
            }
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
            yylval.sval = string_new_len(NULL, len);
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
            if (ret == '<' && *p == '=') {
                ret = T_LTE;
                p++;
            } else
            if (ret == '<' && *p == '>') {
                ret = T_NE;
                p++;
            } else
            if (ret == '!' && *p == '=') {
                ret = T_NE2;
                p++;
            } else
            if (ret == '>' && *p == '=') {
                ret = T_GTE;
                p++;
            }
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
 * and function are the inverse of coltoa(). Pass NULL lenp for exact match.
 */

int atocol(const char *s, int *lenp) {
    int col, i = 0;

    if (!isalphachar(*s))
        return -1;

    // XXX: use more than 2 letters?
    col = toupperchar(s[i++]) - 'A';
    if (isalphachar(s[i])) {
        col = (col + 1) * 26 + (toupperchar(s[i++]) - 'A');
    }
    if (lenp) {
        *lenp = i;
    } else {
        if (s[i] != '\0')
            col = -1;
    }
    return col;
}
