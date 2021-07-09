/*      SC      A Spreadsheet Calculator
 *              Main driver
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              More mods by Alan Silverstein, 3-4/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <sys/wait.h>
#include <signal.h>
#include <sys/file.h>
#include <fcntl.h>
#ifdef USELOCALE
#include <locale.h>
#endif
#include <termios.h>
#include "sc.h"
#include "version.h"

#ifndef SAVENAME
#define SAVENAME "SC.SAVE" /* file name to use for emergency saves */
#endif /* SAVENAME */

static void settcattr(void);

/* Globals declared in sc.h */

SCXMEM struct ent ***tbl;
int strow = 0, stcol = 0;
int currow = 0, curcol = 0;
int savedrow[37], savedcol[37];     /* stack of marked cells */
int savedstrow[37], savedstcol[37];
int FullUpdate = 0;
int maxrow, maxcol;
int maxrows, maxcols;
SCXMEM int *fwidth;
SCXMEM int *precision;
SCXMEM int *realfmt;
SCXMEM unsigned char *col_hidden;
SCXMEM unsigned char *row_hidden;
int changed;
int cslop;
int qbuf;       /* buffer no. specified by " command */
int modflg;
int numeric;
SCXMEM char *mdir;
SCXMEM char *autorun;
int skipautorun;
SCXMEM char *fkey[FKEYS];
SCXMEM char *scext;
SCXMEM char *ascext;
SCXMEM char *tbl0ext;
SCXMEM char *tblext;
SCXMEM char *latexext;
SCXMEM char *slatexext;
SCXMEM char *texext;
int scrc = 0;
int showsc, showsr;     /* Starting cell for highlighted range */
int usecurses = TRUE;   /* Use curses unless piping/redirection or using -q */
int brokenpipe = FALSE; /* Set to true if SIGPIPE is received */

char curfile[PATHLEN];
char revmsg[80];

/* numeric separators, country-dependent if locale support enabled: */
char dpoint = '.';   /* decimal point */
char thsep = ',';    /* thousands separator */

int showtop   = 1;     /* Causes current cell value display in top line  */
int showcell  = 1;     /* Causes current cell to be highlighted          */
int showrange = 0;     /* Causes ranges to be highlighted                */
int showneed  = 0;     /* Causes cells needing values to be highlighted  */
int showexpr  = 0;     /* Causes cell exprs to be displayed, highlighted */
int shownote  = 0;     /* Causes cells with attached notes to be
                          highlighted                                    */
int braille   = 0;     /* Be nice to users of braille displays           */
int braillealt = 0;    /* Alternate mode for braille users               */

int autocalc  = 1;     /* 1 to calculate after each update */
int autolabel = 1;     /* If room, causes label to be created after a define */
int autoinsert = 0;    /* Causes rows to be inserted if craction is non-zero
                          and the last cell in a row/column of the scrolling
                          portion of a framed range has been filled      */
int autowrap = 0;      /* Causes cursor to move to next row/column if craction
                          is non-zero and the last cell in a row/column of
                          the scrolling portion of a framed range has been
                          filled */
int calc_order = BYROWS;
int optimize  = 0;     /* Causes numeric expressions to be optimized */
int tbl_style = 0;     /* headers for T command output */
int rndtoeven = 0;
int color     = 1;     /* Use color */ // XXX: should rename as use_color
int colorneg  = 1;     /* Increment color number for cells with negative numbers */
int colorerr  = 1;     /* Color cells with errors with color 3 */
int craction = 0;      /* 1 for down, 2 for right */
int pagesize = 0;      /* If nonzero, use instead of 1/2 screen height */
int dobackups;         /* Copy current database file to backup file      */
                       /* before overwriting                             */
int rowlimit = -1;
int collimit = -1;
int rowsinrange = 1;
int colsinrange = DEFWIDTH;
int emacs_bindings = 1;      /* use emacs-like bindings */

#ifdef VMS
int VMS_read_raw = 0;
#endif

const char *progname;
int Vopt;
#ifdef TRACE
static FILE *ftrace;
#endif

int main(int argc, char **argv) {
    int c;
    const char *revi;

    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */

    int mopt = 0;
    int oopt = 0;
    int nopt = 0;
    int copt = 0;
    int ropt = 0;
    int Copt = 0;
    int Ropt = 0;
    int eopt = 0;
    int popt = 0;
    int qopt = 0;
    int Mopt = 0;

    Vopt = 0;

#ifdef USELOCALE
    setlocale(LC_ALL, "");
#endif
    progname = get_basename(argv[0]);

#ifdef TRACE
    if (!(ftrace = fopen(TRACE, "w"))) {
        fprintf(stderr, "%s: fopen(%s, 'w') failed: %s\n",
                progname, TRACE, strerror(errno));
        exit(1);
    }
#endif

    while ((c = getopt(argc, argv, "axmoncrCReP:W:vqMh?")) != EOF) {
        switch (c) {
        case 'a':
            skipautorun = 1;
            break;
        case 'x':
#ifdef NOCRYPT
            fprintf(stderr, "Crypt not available\n");
            exit(1);
#else
            Crypt = 1;
#endif
            break;
        case 'm':
            mopt = 1;
            break;
        case 'o':
            oopt = 1;
            break;
        case 'n':
            nopt = 1;
            break;
        case 'c':
            copt = 1;
            break;
        case 'r':
            ropt = 1;
            break;
        case 'C':
            Copt = 1;
            craction = CRCOLS;
            break;
        case 'R':
            Ropt = 1;
            craction = CRROWS;
            break;
        case 'e':
            rndtoeven = 1;
            eopt = 1;
            break;
        case 'P':
        case 'W':
            popt = 1;
            break;
        case 'v':
            break;
        case 'q':
            qopt = 1;
            break;
        case 'M':
            Mopt = 1;
            break;
        default:
            printf("usage: sc [-acemnoqrvxCMR] [-P RANGE/ADDRESS] [-W RANGE]\n"
                   "options:\n"
                   "  -a   Do not run the autorun macro, if present in the file.\n"
                   "  -c   Set recalculation in column order.\n"
                   "  -e   Enable round-to-even (banker's rounding).\n"
                   "  -m   Disable automatic recalculation.\n"
                   "  -n   Enable quick numeric entry mode.\n"
                   "  -o   Enable automatic optimization of expressions.\n"
                   "  -q   Quit after loading all files.\n"
                   "  -r   Set recalculation in row order (default option).\n"
                   "  -v   Output expression values when piping data out via -P option.\n"
                   "  -x   Use crypt to encrypt and decrypt data files.\n"
                   "  -C   Set automatic newline action to increment the column.\n"
                   "  -M   Process mouse events.\n"
                   "  -R   Set automatic newline action to increment the row.\n"
                   "  -P   Pipe a range to standard output.\n"
                   "  -W   Write a range to standard output.\n");
            return 1;
        }
    }

    if (!isatty(STDOUT_FILENO) || popt || qopt) usecurses = FALSE;
    startdisp();
    signals();
    settcattr();
    read_hist();

    /* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(GROWNEW, 0, 0)) {
        stopdisp();
        exit(1);
    }

    /*
     * Build revision message for later use:
     */

    if (popt) {
        *revmsg = '\0';
    } else {
        /* extract the revision string defined in version.c */
        /* skip up to and including the colon */
        for (revi = rev; *revi && *revi++ != ':'; )
            continue;
        snprintf(revmsg, sizeof revmsg, "%s%.*s:  Type '?' for help.",
                 progname, (int)strlen(revi) - 2, revi);
    }

    if (optind < argc && !strcmp(argv[optind], "--"))
        optind++;
    if (optind < argc && argv[optind][0] != '|' && strcmp(argv[optind], "-"))
        strlcpy(curfile, argv[optind], sizeof curfile);

    if (usecurses && has_colors())
        initcolor(0);

    if (optind < argc) {
        if (!readfile(argv[optind], 1) && (optind == argc - 1))
            error("New file: \"%s\"", curfile);
        EvalAll();
        optind++;
    } else {
        erasedb();
    }

    while (optind < argc) {
        /* merge other files into the current db */
        readfile(argv[optind], 0);
        optind++;
    }

    savedrow[0] = currow;
    savedcol[0] = curcol;
    savedstrow[0] = strow;
    savedstcol[0] = stcol;
    // XXX: potentially redundant
    EvalAll();

    if (!(popt || isatty(STDIN_FILENO)))
        readfile("-", 0);

    if (qopt) {
        stopdisp();
        exit(0);
    }

    if (usecurses)
        clearok(stdscr, TRUE);

    // XXX: potentially redundant
    EvalAll();

    if (mopt) autocalc = 0;
    if (oopt) optimize = 1;
    if (nopt) numeric = 1;
    if (copt) calc_order = BYCOLS;
    if (ropt) calc_order = BYROWS;
    if (Copt) craction = CRCOLS;
    if (Ropt) craction = CRROWS;
    if (eopt) rndtoeven = 1;
    if (Mopt) mouseon();
    if (popt) {
        const char *redraw = NULL;
        int o;

#ifdef BSD43
        optreset = 1;
#endif
        /* reparse command line arguments */
        optind = 1;
        stopdisp();
        while ((o = getopt(argc, argv, "axmoncrCReP:W:vq")) != EOF) {
            switch (o) {
            case 'v':
                Vopt = 1;
                break;
            case 'P':
                sc_cmd_put(optarg);
                if (*optarg == '/')
                    redraw = "recalc\nredraw\n";
                Vopt = 0;
                break;
            case 'W':
                sc_cmd_write(optarg);
                break;
            default:
                break;
            }
        }
        if (redraw) fputs(redraw, stdout);
        return EXIT_SUCCESS;
    }

    if (!isatty(STDOUT_FILENO)) {
        stopdisp();
        write_fd(stdout, rangeref_total());
        return EXIT_SUCCESS;
    }

    vi_interaction();
    stopdisp();
    write_hist();
    return EXIT_SUCCESS;
}

/* try to save the current spreadsheet if we can */
static void diesave(void) {
    char path[PATHLEN];

    if (modcheck(" before Spreadsheet dies") == 1) {
        snprintf(path, sizeof path, "~/%s", SAVENAME);
        if (writefile(path, rangeref_total()) < 0) {
            snprintf(path, sizeof path, "/tmp/%s", SAVENAME);
            if (writefile(path, rangeref_total()) < 0)
                error("Couldn't save current spreadsheet, Sorry");
        }
    }
}

void signals(void) {
    signal(SIGINT, doquit);
    signal(SIGQUIT, dump_me);
    signal(SIGPIPE, nopipe);
    signal(SIGALRM, time_out);
#ifndef __DJGPP__
    signal(SIGBUS, doquit);
#endif
    signal(SIGTERM, doquit);
    signal(SIGFPE, doquit);
#ifdef SIGWINCH
    signal(SIGWINCH, winchg);
#endif
}

sigret_t nopipe(int i) {
    (void)i;
    brokenpipe = TRUE;
}

sigret_t winchg(int i) {
    (void)i;
    stopdisp();
    startdisp();
    /*
     * I'm not sure why a refresh() needs to be done both before and after
     * the clearok() and update(), but without doing it this way, a screen
     * (or window) that grows bigger will leave the added space blank. - CRM
     */
    refresh();
    FullUpdate++;
    clearok(stdscr, TRUE);
    update(1);
    refresh();
#ifdef SIGWINCH
    signal(SIGWINCH, winchg);
#endif
}

sigret_t doquit(int i) {
    (void)i;
    if (usecurses) {
        diesave();
        stopdisp();
    }
    write_hist();
    exit(1);
}

sigret_t dump_me(int i) {
    (void)i;
    if (usecurses)
        diesave();
    deraw(1);
    abort();
}

static void settcattr(void) {
    struct termios tty;
    long vdis = 255;

#ifdef _PC_VDISABLE
    if ((vdis = fpathconf(STDIN_FILENO, _PC_VDISABLE)) == -1) {
        fprintf(stderr,
                "fpathconf(STDIN, _PC_VDISABLE) failed: %s\n",
                strerror(errno));
        vdis = 255;
    }
#endif
    if (tcgetattr(STDIN_FILENO, &tty) == -1) {
        fprintf(stderr, "tcgetattr STDIN failed: %s\n",
                strerror(errno));
        return;
    }
    //VINTR
    tty.c_cc[VQUIT] = vdis;
    tty.c_cc[VSTART] = vdis;
    tty.c_cc[VSTOP] = vdis;
#ifdef VLNEXT
    tty.c_cc[VLNEXT] = vdis;
#endif
#ifdef VDISCARD
    tty.c_cc[VDISCARD] = vdis;
#endif
#ifdef VSTATUS
    tty.c_cc[VSTATUS] = vdis;
#endif
    tty.c_cc[VSUSP] = vdis;
#ifdef VDSUSP
    tty.c_cc[VDSUSP] = vdis;
#endif
    if (tcsetattr(STDIN_FILENO, TCSADRAIN, &tty) == -1) {
        fprintf(stderr, "tcsetattr STDIN failed: %s\n",
                strerror(errno));
        return;
    }
}

void fatal(const char *str) {
    deraw(1);
    fprintf(stderr, "%s\n", str);
    diesave();
    exit(1);
}
