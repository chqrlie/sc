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
    struct ent *ent;
    struct enode *enode;
    char *sval;
    struct range_s rval;
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

static struct key experres[] = {
#include "experres.h"
    { 0, 0 }
};

static struct key statres[] = {
#include "statres.h"
    { 0, 0 }
};

static const char *src_line;
static const char *src_pos;

int parse_line(const char *buf) {
    int ret;
    while (isspacechar(*buf))
        buf++;
    src_pos = src_line = buf;
    ret = yyparse();
    src_pos = src_line = "";
    return ret;
}

void yyerror(const char *err) {
    parse_error(err, src_line, src_pos);
}

int yylex(void) {
    char path[PATHLEN];
    const char *p = src_pos;
    int ret = -1;

    // XXX: static data in the lexer is toxic
    static int isfunc = 0;
    static sc_bool_t isgoto = 0;
    static sc_bool_t colstate = 0;
    static int dateflag;
    static const char *tokenst = NULL;
    static size_t tokenl;

    for (;;) {
        if (isspacechar(*p)) {
            p++;
            continue;
        }
        if (*p == '[') {    /* syntax hint comment */
            while (*p && *p++ != ']')
                continue;
            src_pos = p;    // XXX: probably useless
            tokenst = NULL; // XXX: probably useless
            continue;
        }
        if (*p == '\0') {
            isfunc = isgoto = 0;
            ret = -1;
        } else
        if (isalphachar_(*p)) {
            const char *la;      /* lookahead pointer */
            struct key *tblp;

            if (!tokenst) {
                tokenst = p;
                tokenl = 0;
            }
            /*
             *  This picks up either 1 or 2 alpha characters (a column) or
             *  tokens made up of alphanumeric chars and '_' (a function or
             *  token or command or a range name)
             */
            while (isalphachar(*p)) {
                p++;
                tokenl++;
            }
            la = p;
            while (isdigitchar(*la) || *la == '$')
                la++;
            /*
             * A COL is 1 or 2 char alpha with nothing but digits following
             * (no alpha or '_')
             */
            if (!isdigitchar(*tokenst) && tokenl && tokenl <= 2 &&
                (colstate || (isdigitchar(la[-1]) && !(isalphachar_(*la))))) {
                ret = COL;
                yylval.ival = atocol(tokenst, tokenl);
            } else {
                while (isalnumchar_(*p)) {
                    p++;
                    tokenl++;
                }
                ret = WORD;
                if (src_pos == src_line || isfunc) {
                    if (isfunc) isfunc--;
                    /* initial blanks are ignored, so the test on src_pos is incorrect */
                    for (tblp = src_pos > src_line ? experres : statres; tblp->key; tblp++) {
                        // XXX: ugly hard coded case mapped comparison
                        // XXX: the length test is bogus, accesses beyond the end of the string
                        if (((tblp->key[0] ^ tokenst[0]) & 0x5F) == 0 && tblp->key[tokenl] == 0) {
                            /* Commenting the following line makes the search slower */
                            /* but avoids access outside valid memory. A BST would   */
                            /* be the better alternative. */
                            unsigned int i = 1;
                            while (i < tokenl && ((tokenst[i] ^ tblp->key[i]) & 0x5F) == 0)
                                i++;
                            if (i >= tokenl) {
                                ret = tblp->val;
                                yylval.ival = ret;
                                colstate = (ret <= S_FORMAT);
                                if (isgoto) {
                                    isfunc = isgoto = 0;
                                    if (ret != K_ERROR && ret != K_INVALID)
                                        ret = WORD;
                                }
                                break;
                            }
                        }
                    }
                }
                if (ret == WORD) {
                    struct range *r;
                    if (!find_range_name(tokenst, tokenl, &r)) {
                        yylval.rval.left = r->r_left;
                        yylval.rval.right = r->r_right;
                        if (r->r_is_range)
                            ret = RANGE;
                        else
                            ret = VAR;
                    } else
                    if (plugin_exists(tokenst, tokenl, path, PATHLEN)) {
                        // XXX: really catenate the rest of the input line?
                        strlcat(path, p, PATHLEN);
                        yylval.sval = scxdup(path);
                        ret = PLUGIN;
                    } else {
                        src_pos = p;
                        yyerror("Unintelligible word");
                    }
                }
            }
        } else
        if ((*p == '.') || isdigitchar(*p)) {
            sigret_t (*sig_save)(int);
            volatile double v = 0.0;
            int temp;
            const char *nstart = p;

            sig_save = signal(SIGFPE, fpe_trap);
            if (setjmp(fpe_buf)) {
                signal(SIGFPE, sig_save);
                // XXX: was: yylval.fval = v; but gcc complains about v getting clobbered
                yylval.fval = v;
                error("Floating point exception\n");
                isfunc = isgoto = 0;
                tokenst = NULL;
                return FNUMBER;
            }

            if (*p == '.' && dateflag) {  /* .'s in dates are returned as tokens. */
                ret = *p++;
                dateflag--;
            } else {
                if (*p != '.') {
                    tokenst = p;
                    tokenl = 0;
                    do {
                        v = v * 10.0 + (double)((unsigned) *p - '0');
                        tokenl++;
                    } while (isdigitchar(*++p));
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
                        ret = K_ERR;
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
        } else
        if (*p == '"') {
            const char *p1;
            size_t size = 1;
            char *ptr;
            p++;  /* skip the '"' */
            /* "string" or "string\"quoted\"" */
            for (p1 = p; *p1 && *p1 != '"' && *p1 != '\n'; p1++) {
                if (*p1 == '\\' && (p1[1] == '"' || p1[1] == '\\'))
                    p1++;
                size++;
            }
            ptr = scxmalloc(size);
            yylval.sval = ptr;
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
    if (!isfunc) isfunc = ((ret == '@') + (ret == S_GOTO) - (ret == S_SET));
    if (ret == S_GOTO) isgoto = TRUE;
    tokenst = NULL;
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

int atocol(const char *string, int len) {
    int col;

    col = toupperchar(string[0]) - 'A';
    if (len == 2)               /* has second char */
        col = (col + 1) * 26 + (toupperchar(string[1]) - 'A');

    return col;
}
