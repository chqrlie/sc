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

static sheet_t cur_sheet;
sheet_t *sht = &cur_sheet;

cellref_t savedcr[MARK_COUNT];     /* stack of marked cells */
cellref_t savedst[MARK_COUNT];
int FullUpdate;
int changed;
int skipautorun;
SCXMEM string_t *histfile;
SCXMEM string_t *scext;
SCXMEM string_t *ascext;
SCXMEM string_t *tbl0ext;
SCXMEM string_t *tblext;
SCXMEM string_t *latexext;
SCXMEM string_t *slatexext;
SCXMEM string_t *texext;
int scrc = 0;
int usecurses = TRUE;   /* Use curses unless piping/redirection or using -q */
int brokenpipe = FALSE; /* Set to true if SIGPIPE is received */

char revmsg[80];

/* numeric separators, country-dependent if locale support enabled: */
char dpoint = '.';   /* decimal point */
char thsep = ',';    /* thousands separator */

//int showtop   = 1;     /* Causes current cell value display in top line  */
int showcell  = 1;     /* Causes current cell to be highlighted          */
int showneed  = 0;     /* Causes cells needing values to be highlighted  */
int showexpr  = 0;     /* Causes cell exprs to be displayed, highlighted */
int shownote  = 0;     /* Causes cells with attached notes to be
                          highlighted                                    */
int braille   = 0;     /* Be nice to users of braille displays           */
int braillealt = 0;    /* Alternate mode for braille users               */

int autolabel = 1;     /* If room, causes label to be created after a define */
int color     = 1;     /* Use color */ // XXX: should rename as use_color
int dobackups;         /* Copy current database file to backup file      */
                       /* before overwriting                             */
int rowsinrange;
int colsinrange;
int emacs_bindings = 1;      /* use emacs-like bindings */

const char *progname;
#ifdef TRACE
static FILE *ftrace;
#endif

int main(int argc, char **argv) {
    sheet_t *sp = sht;
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
    int Dopt = 0;

    string_init();
    histfile = string_new("~/.sc_history");

#ifdef USELOCALE
    //setlocale(LC_ALL, "");
    setlocale(LC_ALL, "en_US.UTF-8");
#endif
    progname = get_basename(argv[0]);

#ifdef TRACE
    if (!(ftrace = fopen(TRACE, "w"))) {
        fprintf(stderr, "%s: fopen(%s, 'w') failed: %s\n",
                progname, TRACE, strerror(errno));
        exit(1);
    }
#endif

    while ((c = getopt(argc, argv, "aCcDeMmnoqRrvxP:W:h?")) != EOF) {
        switch (c) {
        case 'a':   skipautorun = 1;                break;
        case 'c':   copt = 1;                       break;
        case 'C':   Copt = 1; sp->craction = CRCOLS;  break;
        case 'D':   Dopt = 1;                       break;
        case 'e':   eopt = 1; sp->rndtoeven = 1;    break;
        case 'M':   Mopt = 1;                       break;
        case 'm':   mopt = 1;                       break;
        case 'n':   nopt = 1;                       break;
        case 'o':   oopt = 1;                       break;
        case 'q':   qopt++;                         break;
        case 'R':   Ropt = 1; sp->craction = CRROWS;  break;
        case 'r':   ropt = 1;                       break;
        case 'v':                                   break;
        case 'x':
#ifdef NOCRYPT
            fprintf(stderr, "Crypt not available\n");
            exit(1);
#else
            Crypt = 1;
#endif
            break;
        case 'P':
        case 'W':   popt = 1;                       break;
        default:
            printf("usage: sc [-aCcDeMmnoqRrvx] [-P RANGE/ADDRESS] [-W RANGE]\n"
                   "options:\n"
                   "  -a   Do not run the autorun macro, if present in the file.\n"
                   "  -C   Set automatic newline action to increment the column.\n"
                   "  -c   Set recalculation in column order.\n"
                   "  -D   Enable debug output.\n"
                   "  -e   Enable round-to-even (banker's rounding).\n"
                   "  -M   Process mouse events.\n"
                   "  -m   Disable automatic recalculation.\n"
                   "  -n   Enable quick numeric entry mode.\n"
                   "  -o   Enable automatic optimization of expressions.\n"
                   "  -q   Quit after loading all files.\n"
                   "  -R   Set automatic newline action to increment the row.\n"
                   "  -r   Set recalculation in row order (default option).\n"
                   "  -v   Output expression values when piping data out via -P option.\n"
                   "  -x   Use crypt to encrypt and decrypt data files.\n"
                   "  -P   Pipe a range to standard output.\n"
                   "  -W   Write a range to standard output.\n");
            return 1;
        }
    }

    delbuf_init();
    sheet_init(sp);

    if (!isatty(STDOUT_FILENO) || popt || qopt == 1) usecurses = FALSE;
    startdisp();
    signals();
    settcattr();
    read_hist(string_dup(histfile));

    /* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(sp, GROWNEW, 0, 0)) {
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
        pstrcpy(sp->curfile, sizeof sp->curfile, argv[optind]);

    if (usecurses)
        initcolor(sp, 0);

    if (optind < argc) {
        if (!readfile(sp, argv[optind], 1) && (optind == argc - 1))
            error("New file: \"%s\"", sp->curfile);
        EvalAll(sp); // XXX: should delay until after all files have been loaded
        optind++;
    } else {
        erasedb(sp);
        load_scrc(sp);
    }

    while (optind < argc) {
        /* merge other files into the current db */
        readfile(sp, argv[optind], 0);
        optind++;
    }

    savedcr[0] = cellref(sp->currow, sp->curcol);
    savedst[0] = cellref(sp->strow, sp->stcol);
    // XXX: potentially redundant
    // XXX: should check for autocalc
    EvalAll(sp);

    if (!(popt || isatty(STDIN_FILENO)))
        readfile(sp, "-", 0);

    if (qopt == 1) {
        stopdisp();
        exit(0);
    }

    screen_rebuild();

    // XXX: potentially redundant
    EvalAll(sp);

    if (mopt) sp->autocalc = 0;
    if (oopt) sp->optimize = 1;
    if (nopt) sp->numeric = 1;
    if (copt) sp->calc_order = BYCOLS;
    if (ropt) sp->calc_order = BYROWS;
    if (Copt) sp->craction = CRCOLS;
    if (Ropt) sp->craction = CRROWS;
    if (eopt) sp->rndtoeven = 1;
    if (Mopt) screen_mouseon();
    if (popt) {
        int Vopt = 0;
        const char *redraw = NULL;

#ifdef BSD43
        optreset = 1;
#endif
        /* reparse command line arguments */
        optind = 1;
        stopdisp();
        while ((c = getopt(argc, argv, "axmoncrCDReP:W:vqM")) != EOF) {
            switch (c) {
            case 'v':
                Vopt = 1;
                break;
            case 'P':
                sc_cmd_put(sp, optarg, Vopt);
                if (optarg && *optarg == '/')
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
        write_fd(sp, stdout, rangeref_total(sp), DCP_DEFAULT);
        return EXIT_SUCCESS;
    }

#ifdef VENIX
    setbuf(stdin, NULL);
#endif
    if (!qopt)
        vi_interaction(sp);
    stopdisp();
    write_hist(string_dup(histfile));

    if (Dopt) {
        /* free all memory and check for remaining blocks */
        erasedb(sp);
        go_free(sp);
        delbuf_clean();
        free_enode_list();
        free_styles();
        free_hist();
        string_set(&histfile, NULL);
        string_set(&scext, NULL);
        string_set(&ascext, NULL);
        string_set(&tbl0ext, NULL);
        string_set(&tblext, NULL);
        string_set(&latexext, NULL);
        string_set(&slatexext, NULL);
        string_set(&texext, NULL);
        string_exit();
        scxmemdump();
    }

    return EXIT_SUCCESS;
}

/* try to save the current spreadsheet if we can */
static void diesave(void) {
    sheet_t *sp = sht;
    char path[PATHLEN];

    if (modcheck(sp, " before Spreadsheet dies") == 1) {
        snprintf(path, sizeof path, "~/%s", SAVENAME);
        if (writefile(sp, path, rangeref_total(sp), DCP_DEFAULT) < 0) {
            snprintf(path, sizeof path, "/tmp/%s", SAVENAME);
            if (writefile(sp, path, rangeref_total(sp), DCP_DEFAULT) < 0)
                error("Could not save current spreadsheet, Sorry");
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
    sheet_t *sp = sht;

    (void)i;
    screen_resize(sp);
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
    write_hist(string_dup(histfile));
    exit(1);
}

sigret_t dump_me(int i) {
    (void)i;
    if (usecurses)
        diesave();
    screen_deraw(1);
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
    screen_deraw(1);
    fprintf(stderr, "%s\n", str);
    diesave();
    exit(1);
}
