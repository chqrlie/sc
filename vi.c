/*      SC      A Spreadsheet Calculator
 *              One line vi emulation
 *
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <sys/wait.h>
#include <signal.h>
#if defined(REGCOMP)
#include <regex.h>
#endif
#include "sc.h"
#include "y.tab.h"

#if defined(RE_COMP)
extern char *re_comp(char *s);
extern char *re_exec(char *s);
#endif
#if defined(REGCMP)
char *regcmp();
char *regex();
#endif

static inline int iswordchar(char c) { return isalnumchar(c) || c == '_'; }

static void write_line(int c);
static void append_line(void);
static void back_hist(void);
static int back_line(int arg);
static int back_word(int arg, int big_word);
static void back_space(void);
static void change_case(int arg);
static void col_0(void);
static void cr_line(void);
static void del_in_line(int arg, int back_null);
static void del_to_end(void);
static void ins_string(const char *s);
static void ins_in_line(int c);
static void doabbrev(void);
static void dogoto(void);
static void dotab(void);
static void dotcmd(void);
static void doshell(void);
static int find_char(int arg, int n);
static void forw_hist(void);
static int forw_line(int arg, int stop_null);
static int forw_word(int arg, int end_word, int big_word, int stop_null);
static int istart;
static void last_col(void);
static void match_paren(void);
static void rep_char(void);
static void replace_in_line(int c);
static void replace_mode(void);
static void restore_it(void);
static void savedot(int c);
static void save_hist(void);
static void search_again(bool reverse);
static void search_hist(void);
static void search_mode(char sind);
static void stop_edit(void);
static int to_char(int arg, int n);
static void u_save(int c);
static void yank_cmd(int delete, int change);
static void yank_chars(int first, int last, int delete);
static int get_motion(int change);
static int vigetch(void);
static void scroll_down(void);
static void scroll_up(int);
static void colshow_op(void);
static void rowshow_op(void);
static int checkmark(int c);
static void markcell(void);
static void dotick(int tick);
static int get_rcqual(int ch);
static void formatcol(int arg);
static void edit_mode(void);
static void insert_mode(void);
static void toggle_navigate_mode(void);
static void startshow(void);
static void showdr(void);
static void gotobottom(void);
static void gototop(void);
static void gohome(void);
static void leftlimit(void);
static void rightlimit(void);
static void list_all(void);

static int uarg = 1;        /* universal numeric prefix argument */

static char *completethis = NULL;
static int search_dir;      /* Search direction:  forward = 0; back = 1 */
char histfile[PATHLEN] = "~/.sc_history";

/* values for mode below */

#define INSERT_MODE     0   /* Insert mode */
#define EDIT_MODE       1   /* Edit mode */
#define REP_MODE        2   /* Replace mode */
#define SEARCH_MODE     3   /* Get arguments for '/' command */
#define NAVIGATE_MODE   4   /* Navigate the spreadsheet while editing a line */

#define DOTLEN          200

static int mode = INSERT_MODE;
static struct hist {
    unsigned int len;
    char *histline;
} history[HISTLEN + 1];

static int histp = 0;
static int lasthist = 0;
static int endhist = -1;
static int histsessionstart = 0;
static int histsessionnew = 0;

#ifdef REGCOMP
static regex_t preg;
static regex_t *last_search = NULL;
static int errcode;
#else
#ifndef RE_COMP
static char *last_search = NULL;
#endif
#endif
static char *undo_line = NULL;
static unsigned undolen = 0;
static int undo_lim;
static char dotb[DOTLEN];
static int doti = 0;
static int do_dot = 0;
static int nosavedot = 1;
static int dotarg = 1;
static char putbuf[FBUFLEN];
static int findfunc = '\0';
static int findchar = 1;
static int finddir = 0;
static int numeric_field = 0; /* Started the line editing with a number */

#ifdef NCURSES_MOUSE_VERSION
static MEVENT mevent;
static void mouse_set_pos(int);
static int mouse_sel_cell(int);
#endif

int set_line(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    linelim = ((int (*)(char *, size_t, const char *, va_list))vsnprintf)
        (line, sizeof line, fmt, ap);
    va_end(ap);
    if (linelim >= (int)(sizeof line))
        linelim = strlen(line);
    return linelim;
}

void vi_interaction(void) {
    int inloop = 1;
    int c, rcqual;
    int narg;
    int edistate = -1;
    int nedistate;
    int running;
    int anychanged = FALSE;
    int tempx, tempy;       /* Temp versions of curx, cury */
    char *ext;
    struct ent *p;
    int ps;

    modflg = 0;
    cellassign = 0;
#ifdef VENIX
    setbuf(stdin, NULL);
#endif

    uarg = 1;
    while (inloop) {
        running = 1;
        while (running) {
            nedistate = -1;
            narg = 1;
            if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate)) {
                EvalAll();
                if (changed)    /* if EvalAll changed or was before */
                    anychanged = TRUE;
                changed = 0;
            } else {            /* any cells change? */
                if (changed)
                    anychanged = TRUE;
                //changed = 0;  // XXX: should clear changed
            }

            update(anychanged);
            anychanged = FALSE;
#ifndef SYSV3   /* HP/Ux 3.1 this may not be wanted */
            refresh(); /* 5.3 does a refresh in getch */
#endif
            // XXX: should use CLEAR_LINE
            c = nmgetch(0);
            getyx(stdscr, tempy, tempx);
            move(1, 0);
            clrtoeol();
            move(tempy, tempx);
            seenerr = 0;
            showneed = 0;   /* reset after each update */
            showexpr = 0;
            shownote = 0;

            if (ISCTL(c) || c == DEL || c == KEY_END || c == KEY_BACKSPACE) {
                switch (c) {
#ifdef SIGTSTP
                case ctl('z'):
                    deraw(1);
                    kill(0, SIGTSTP); /* Nail process group */
                    /* the pc stops here */
                    goraw();
                    break;
#endif
                case ctl('r'):
                    showneed = 1;
                case ctl('l'):
                    FullUpdate++;
                    clearok(stdscr, 1);
                    break;
                case ctl('x'):
                    FullUpdate++;
                    showexpr = 1;
                    clearok(stdscr, 1);
                    break;
                case ctl('b'):
                    if (emacs_bindings) {
                        backcol(uarg);
                        break;
                    }
                    ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                    backrow(uarg * ps);
                    // XXX: hidden row issue
                    strow = strow - uarg * ps;
                    if (strow < 0) strow = 0;
                    FullUpdate++;
                    break;
                case ctl('c'):
                    running = 0;
                    break;

                case KEY_END:
                case ctl('e'):
                    if (linelim < 0 || mode_ind == 'v') {
                        switch (c = nmgetch(1)) {
                        case KEY_UP:
                        case ctl('p'): case 'k':    doend(-1, 0);   break;

                        case KEY_DOWN:
                        case ctl('n'): case 'j':    doend( 1, 0);   break;

                        case KEY_LEFT:
                        case KEY_BACKSPACE:
                        case ctl('h'): case 'h':    doend( 0,-1);   break;

                        case KEY_RIGHT:
                        case ' ':
                        case ctl('i'): case 'l':    doend( 0, 1);   break;

                        case ctl('e'):
                        case ctl('y'):
                            while (c == ctl('e') || c == ctl('y')) {
                                int x = uarg;

                                while (uarg) {
                                    if (c == ctl('e')) {
                                        scroll_down();
                                    } else {
                                        // XXX: Passing x seems incorrect
                                        scroll_up(x);
                                    }
                                    uarg--;
                                }
                                FullUpdate++;
                                update(0);
                                uarg = 1;
                                c = nmgetch(0);
                            }
                            ungetch(c);
                            break;

                        case ESC:
                        case ctl('g'):
                            break;

                        default:
                            error("Invalid ^E command");
                            break;
                        }
                    } else {
                        write_line(ctl('e'));
                    }
                    break;

                case ctl('y'):
                    while (c == ctl('e') || c == ctl('y')) {
                        int x = uarg;

                        while (uarg) {
                            if (c == ctl('e')) {
                                scroll_down();
                            } else {
                                // XXX: Passing x seems incorrect
                                scroll_up(x);
                            }
                            uarg--;
                        }
                        FullUpdate++;
                        update(0);
                        uarg = 1;
                        c = nmgetch(0);
                    }
                    ungetch(c);
                    break;

                case ctl('f'):
                    if (emacs_bindings) {
                        forwcol(uarg);
                        break;
                    }
                    ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                    forwrow(uarg * ps);
                    // XXX: hidden row issue
                    strow = strow + (uarg * ps);
                    FullUpdate++;
                    break;

                case ctl('g'):
                    showrange = 0;
                    linelim = -1;
                    move(1, 0);
                    clrtoeol();
                    break;

                case ESC:       /* ctl('[') */
                    write_line(ESC);
                    break;

                case ctl('d'):
                    write_line(ctl('d'));
                    break;

                case KEY_BACKSPACE:
                case DEL:
                case ctl('h'):
                    if (linelim < 0) {  /* not editing line */
                        backcol(uarg);   /* treat like ^B    */
                        break;
                    }
                    write_line(ctl('h'));
                    break;

                case ctl('i'):          /* tab */
                    if (linelim < 0) {  /* not editing line */
                        forwcol(uarg);
                        break;
                    }
                    write_line(ctl('i'));
                    break;

                case ctl('m'):
                case ctl('j'):
                    write_line(ctl('m'));
                    break;

                case ctl('n'):
                    c = craction;
                    if (numeric_field) {
                        craction = 0;
                        write_line(ctl('m'));
                        numeric_field = 0;
                    }
                    craction = c;
                    if (linelim < 0) {
                        forwrow(uarg);
                        break;
                    }
                    write_line(ctl('n'));
                    break;

                case ctl('p'):
                    c = craction;
                    if (numeric_field) {
                        craction = 0;
                        write_line(ctl('m'));
                        numeric_field = 0;
                    }
                    craction = c;
                    if (linelim < 0) {
                        backrow(uarg);
                        break;
                    }
                    write_line(ctl('p'));
                    break;

                case ctl('q'):
                    if (emacs_bindings) {
                        // XXX: quoted insert: just a test for function keys
                        error("Quote: ");
                        for (;;) {
                            c = nmgetch(1);
                            if (c == ctl('q') || c == ctl('m'))
                                break;
                            error("Quote: %d (%#x)\n", c, c);
                        }
                        break;
                    }
                    break;      /* ignore flow control */

                case ctl('s'):
                    if (emacs_bindings) {
                        // XXX: search
                        break;
                    }
                    break;      /* ignore flow control */

                case ctl('t'):
                    error("Toggle: a:auto,c:cell,e:ext funcs,n:numeric,t:top,"
#ifndef NOCRYPT
                          "x:encrypt,"
#endif
                          "$:pre-scale,<MORE>");
                    if (braille) move(1, 0);
                    refresh();

                    switch (nmgetch(1)) {
                    case 'a': case 'A':
                    case 'm': case 'M':
                        autocalc ^= 1;
                        error("Automatic recalculation %s.",
                              autocalc ? "enabled" : "disabled");
                        break;
                    case 'b':
                        braille ^= 1;
                        error("Braille enhancement %s.",
                              braille ? "enabled" : "disabled");
                        --modflg;   /* negate the modflg++ */
                        break;
                    case 'c':
                        repaint_cursor(-showcell);
                        showcell = !showcell;
                        repaint_cursor(showcell);
                        error("Cell highlighting %s.",
                              showcell ? "enabled" : "disabled");
                        --modflg;   /* negate the modflg++ */
                        break;
                    case 'C':
                        color = !color;
                        if (has_colors()) {
                            if (color) {
                                attron(COLOR_PAIR(1));
                                bkgd(COLOR_PAIR(1) | ' ');
                            } else {
                                attron(COLOR_PAIR(0));
                                bkgd(COLOR_PAIR(0) | ' ');
                            }
                        }
                        error("Color %s.", color ? "enabled" : "disabled");
                        break;
                    case 'e':
                        extfunc = !extfunc;
                        error("External functions %s.",
                              extfunc? "enabled" : "disabled");
                        break;
                    case 'E':
                        colorerr = !colorerr;
                        error("Color changing of cells with errors %s.",
                              colorerr ? "enabled" : "disabled");
                        break;
                    case 'i': case 'I':
                        autoinsert = !autoinsert;
                        error("Autoinsert %s.",
                              autoinsert? "enabled" : "disabled");
                        break;
                    case 'l': case 'L':
                        autolabel = !autolabel;
                        error("Autolabel %s.",
                              autolabel ? "enabled" : "disabled");
                        break;
                    case 'n':
                        numeric = !numeric;
                        error("Numeric input %s.",
                              numeric ? "enabled" : "disabled");
                        break;
                    case 'N':
                        colorneg = !colorneg;
                        error("Color changing of negative numbers %s.",
                              colorneg ? "enabled" : "disabled");
                        break;
                    case 'o': case 'O':
                        optimize ^= 1;
                        error("%s expressions upon entry.",
                              optimize ? "Optimize" : "Do not optimize");
                        break;
                    case 'r': case 'R':
                        error("Which direction after return key?");
                        switch (nmgetch(1)) {
                        case ctl('m'):
                            craction = 0;
                            error("No action after new line");
                            break;
                        case 'j':
                        case ctl('n'):
                        case KEY_DOWN:
                            craction = CRROWS;
                            error("Down row after new line");
                            break;
                        case 'l':
                        case ' ':
                        case KEY_RIGHT:
                            craction = CRCOLS;
                            error("Right column after new line");
                            break;
                        case ESC:
                        case ctl('g'):
                            break;
                        default:
                            error("Not a valid direction");
                        }
                        break;
                    case 's':
                        cslop ^= 1;
                        error("Color slop %s.",
                              cslop ? "enabled" : "disabled");
                        break;
                    case 't': case 'T':
                        showtop = !showtop;
                        error("Top line %s.",
                              showtop ? "enabled" : "disabled");
                        break;
                    case 'v':
                        emacs_bindings = !emacs_bindings;
                        error("Emacs %s.",
                              emacs_bindings ? "enabled" : "disabled");
                        break;
                    case 'w': case 'W':
                        autowrap = !autowrap;
                        error("Autowrap %s.",
                              autowrap? "enabled" : "disabled");
                        break;
                    case 'x': case 'X':
#ifdef NOCRYPT
                        error("Encryption not available.");
#else
                        Crypt = !Crypt;
                        error("Encryption %s.", Crypt ? "enabled" : "disabled");
#endif
                        break;
                    case 'z': case 'Z':
                        rowlimit = currow;
                        collimit = curcol;
                        error("Row and column limits set");
                        break;
                    case '$':
                        if (prescale == 1.0) {
                            error("Prescale enabled.");
                            prescale = 0.01;
                        } else {
                            prescale = 1.0;
                            error("Prescale disabled.");
                        }
                        break;
                    case ESC:
                    case ctl('g'):
                        --modflg;   /* negate the modflg++ */
                        break;
                    default:
                        error("Invalid toggle command");
                        --modflg;   /* negate the modflg++ */
                    }
                    FullUpdate++;
                    modflg++;
                    break;

                case ctl('u'):
                    narg = uarg * 4;
                    nedistate = 1;
                    break;

                case ctl('v'):  /* switch to navigate mode, or if already *
                                 * in navigate mode, insert variable name */
                    if (emacs_bindings) {
                        ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                        forwrow(uarg * ps);
                        strow = strow + uarg * ps;
                        FullUpdate++;
                        break;
                    }
                    if (linelim >= 0)
                        write_line(ctl('v'));
                    break;

                case ctl('w'):  /* insert variable expression */
                    if (linelim >= 0) {
                        struct ent *p = lookat(currow, curcol);
                        decompile(p->expr, 0);
                    }
                    break;

                case ctl('a'):
                    if (emacs_bindings) {
                        // XXX: goto beginning of row.
                        //      repeated: goto A0
                        break;
                    }
                    if (linelim >= 0) {
                        write_line(c);
                    } else {
                        remember(0);
                        currow = 0;
                        curcol = 0;
                        rowsinrange = 1;
                        colsinrange = fwidth[curcol];
                        remember(1);
                        FullUpdate++;
                    }
                    break;
                case '\035':    /* ^] */
                    if (linelim >= 0)
                        write_line(c);
                    break;
                default:
                    error("No such command (^%c)", c + 0100);
                    break;
                } /* End of the control char switch stmt */
            } else
            if (ISBYTE(c) && isdigit(c) &&
                ((!numeric && linelim < 0) ||
                 (linelim >= 0 && (mode_ind == 'e' || mode_ind == 'v')) ||
                 edistate >= 0))
            {
                /* we got a leading number */
                if (edistate != 0) {
                    /* First char of the count */
                    if (c == '0') {    /* just a '0' goes to left col */
                        if (linelim >= 0)
                            write_line(c);
                        else
                            leftlimit();
                    } else {
                        nedistate = 0;
                        narg = c - '0';
                    }
                } else {
                    /* Succeeding count chars */
                    nedistate = 0;
                    narg = uarg * 10 + (c - '0');
                }
            } else
            if (c == KEY_F(1) && !fkey[c - KEY_F0 - 1]) {
                deraw(1);
                system("man sc");
                goraw();
                clear();
            } else
            if (linelim >= 0) {
                /* Editing line */
                switch (c) {
                case ')':
                case ',':
                    if (showrange)
                        showdr();
                    break;
                default:
                    break;
                }
                write_line(c);
            } else
            if (!numeric && (c == '+' || c == '-')) {
                /* increment/decrement ops */
                p = *ATBL(tbl, currow, curcol);
                if (!p || !(p->flags & IS_VALID)) {
                    if (c == '+') {
                        editv(currow, curcol);
                        insert_mode();
                        write_line(ctl('v'));
                    }
                    continue;
                }
                if (p->expr && !(p->flags & IS_STREXPR)) {
                    error("Can't increment/decrement a formula\n");
                    continue;
                }
                FullUpdate++;
                modflg++;
                if (c == '+')
                    p->v += (double)uarg;
                else
                    p->v -= (double)uarg;
            } else
            if (c > KEY_F0 && c <= KEY_F(FKEYS)) {
                /* a function key was pressed */
                if (fkey[c - KEY_F0 - 1]) {
                    char *tpp;

                    insert_mode();
                    strlcpy(line, fkey[c - KEY_F0 - 1], sizeof line);
                    linelim = 0;
                    for (tpp = line; *tpp != '\0'; tpp++) {
                        if (*tpp == '\\' && tpp[1] == '"') {
                            strsplice(line, sizeof line, tpp - line, 1, NULL, 0);
                        } else
                        if (*tpp == '$' && tpp[1] == '$') {
                            char mycell[16];
                            size_t len;
                            len = snprintf(mycell, sizeof(mycell), "%s%d", coltoa(curcol), currow);
                            strsplice(line, sizeof line, tpp - line, 2, mycell, len);
                            tpp += len - 1;
                        }
                    }
                    write_line(ctl('m'));
                }
            } else {
                /* switch on a normal command character */
                switch (c) {
                case '/':
                    lotus_menu();
                    break;
                case ':':
                    if (linelim >= 0)
                        write_line(':');
                    break;      /* Be nice to vi users */

                case '@':
                    EvalAll();
                    changed = 0;
                    anychanged = TRUE;
                    break;

                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                case '.':
                    if (locked_cell(currow, curcol))
                        break;
                    /* set mark 0 */
                    savedrow[27] = currow;
                    savedcol[27] = curcol;
                    savedstrow[27] = strow;
                    savedstcol[27] = stcol;

                    numeric_field = 1;
                    set_line("let %s = %c", v_name(currow, curcol), c);
                    insert_mode();
                    break;

                case '+':
                case '-':
                    if (locked_cell(currow, curcol))
                        break;
                    p = lookat(currow, curcol);
                    /* set mark 0 */
                    savedrow[27] = currow;
                    savedcol[27] = curcol;
                    savedstrow[27] = strow;
                    savedstcol[27] = stcol;

                    numeric_field = 1;
                    editv(currow, curcol);
                    insert_mode();
                    if (c == '-' || (p->flags & IS_VALID))
                        write_line(c);
                    else
                        write_line(ctl('v'));
                    break;

                case '=':
                    if (locked_cell(currow, curcol))
                        break;
                    /* set mark 0 */
                    savedrow[27] = currow;
                    savedcol[27] = curcol;
                    savedstrow[27] = strow;
                    savedstcol[27] = stcol;

                    set_line("let %s = ", v_name(currow, curcol));
                    insert_mode();
                    break;

                case '!':
                    doshell();
                    break;

                /*
                 * Range commands:
                 */

                case 'r':
                    error("Range: x:erase v:value c:copy f:fill d:def l:lock U:unlock S:show u:undef F:fmt");
                    if (braille) move(1, 0);
                    refresh();

                    switch (c = nmgetch(1)) {
                    case 'l':
                        set_line("lock [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'U':
                        set_line("unlock [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'c':
                        set_line("copy [dest_range src_range] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'm':
                        set_line("move [destination src_range] %s ", v_name(currow, curcol));
                        insert_mode();
                        write_line(ctl('v'));
                        break;
                    case 'x':
                        set_line("erase [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'y':
                        set_line("yank [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'v':
                        set_line("value [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'f':
                        set_line("fill [range start inc] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'd':
                        set_line("define [string range] \"");
                        insert_mode();
                        break;
                    case 'u':
                        set_line("undefine [range] ");
                        insert_mode();
                        break;
                    case 'r':
                        error("frame (top/bottom/left/right/all/unframe)");
                        if (braille) move(1, 0);
                        refresh();
                        switch (c = nmgetch(1)) {
                        case 't':
                            set_line("frametop [<outrange> rows] ");
                            insert_mode();
                            break;
                        case 'b':
                            set_line("framebottom [<outrange> rows] ");
                            insert_mode();
                            break;
                        case 'l':
                            set_line("frameleft [<outrange> cols] ");
                            insert_mode();
                            break;
                        case 'r':
                            set_line("frameright [<outrange> cols] ");
                            insert_mode();
                            break;
                        case 'a':
                            set_line("frame [<outrange> inrange] ");
                            insert_mode();
                            startshow();
                            break;
                        case 'u':
                            set_line("unframe [<range>] ");
                            insert_mode();
                            startshow();
                            break;
                        case ESC:
                        case ctl('g'):
                            linelim = -1;
                            break;
                        default:
                            error("Invalid frame command");
                            linelim = -1;
                            break;
                        }
                        break;
                    case 's':
                        set_line("sort [range \"criteria\"] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'C':
                        set_line("color [range color#] ");
                        insert_mode();
                        startshow();
                        break;
                    case 'S':
                        list_all();
                        break;
                    case 'F':
                        set_line("fmt [range \"format\"] ");
                        insert_mode();
                        startshow();
                        break;
                    case '{':
                        set_line("leftjustify [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case '}':
                        set_line("rightjustify [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case '|':
                        set_line("center [range] ");
                        insert_mode();
                        startshow();
                        break;
                    case ESC:
                    case ctl('g'):
                        break;
                    default:
                        error("Invalid region command");
                        break;
                    }
                    break;

                case '~':
                    set_line("abbrev \"");
                    insert_mode();
                    break;

                case '"':
                    error("Select buffer (a-z or 0-9):");
                    c = nmgetch(1);
                    if (c == ESC || c == ctl('g')) {
                        break;
                    } else if (c >= '0' && c <= '9') {
                        qbuf = c - '0' + (DELBUFSIZE - 10);
                    } else if (c >= 'a' && c <= 'z') {
                        qbuf = c - 'a' + (DELBUFSIZE - 36);
                    } else if (c == '"') {
                        qbuf = 0;
                    } else {
                        error("Invalid buffer");
                    }
                    break;

                    /*
                     * Row/column commands:
                     */

                case KEY_IC:
                case 'i':
                case 'o':
                case 'a':
                case 'd':
                case 'y':
                case 'p':
                case 'v':
                case 's':
                case 'Z':
                    {
                        if (!(rcqual = get_rcqual(c))) {
                            error("Invalid row/column command");
                            break;
                        }

                        if (rcqual == ESC || rcqual == ctl('g'))
                            break;

                        switch (c) {
                        case 'i':
                            if (rcqual == 'r')      insertrow(uarg, 0);
                            else                    insertcol(uarg, 0);
                            break;

                        case 'o':
                            if (rcqual == 'r')      insertrow(uarg, 1);
                            else                    insertcol(uarg, 1);
                            break;

                        case 'a':
                            if (rcqual == 'r')      while (uarg--) duprow();
                            else                    while (uarg--) dupcol();
                            break;

                        case 'd':
                            if (rcqual == 'r')      deleterows(currow, currow + uarg - 1);
                            else                    deletecols(curcol, curcol + uarg - 1);
                            break;

                        case 'y':
                            if (rcqual == 'r')      yankrows(currow, currow + uarg - 1);
                            else                    yankcols(curcol, curcol + uarg - 1);
                            break;

                        case 'p':
                            if (rcqual == '.') {
                                set_line("pullcopy ");
                                insert_mode();
                                startshow();
                                break;
                            }
                            while (uarg--)
                                pullcells(rcqual);
                            break;

                            /*
                             * turn an area starting at currow/curcol into
                             * constants vs expressions - not reversable
                             */
                        case 'v':
                            if (rcqual == 'r') {
                                struct frange *fr;

                                if ((fr = find_frange(currow, curcol)))
                                    valueize_area(currow, fr->or_left->col,
                                                  currow + uarg - 1, fr->or_right->col);
                                else
                                    valueize_area(currow, 0, currow + uarg - 1, maxcol);
                            } else
                                valueize_area(0, curcol, maxrow, curcol + uarg - 1);
                            break;

                        case 'Z':
                            switch (rcqual) {
                            case 'r':   hiderows(currow, currow + uarg - 1); break;
                            case 'c':   hidecols(curcol, curcol + uarg - 1); break;
                            case 'Z':   if (modflg && curfile[0]) {
                                            writefile(curfile, 0, 0, maxrow, maxcol);
                                            running = 0;
                                        } else if (modflg) {
                                            error("No file name.");
                                        } else
                                            running = 0;
                                        break;
                            }
                            break;

                        case 's':
                            /* special case; no repeat count */
                            if (rcqual == 'r')      rowshow_op();
                            else                    colshow_op();
                            break;
                        }
                        break;
                    }

                case '$':
                    rightlimit();
                    break;
                case '#':
                    gotobottom();
                    break;
                case 'w':
                    while (--uarg >= 0) {
                        do {
                            if (curcol < maxcols - 1)
                                curcol++;
                            else {
                                if (currow < maxrows - 1) {
                                    while (++currow < maxrows - 1 && row_hidden[currow])
                                        continue;
                                    curcol = 0;
                                } else {
                                    error("At end of table");
                                    break;
                                }
                            }
                        } while (col_hidden[curcol] || !VALID_CELL(p, currow, curcol));
                    }
                    rowsinrange = 1;
                    colsinrange = fwidth[curcol];
                    break;
                case 'b':
                    while (--uarg >= 0) {
                        do {
                            if (curcol)
                                curcol--;
                            else {
                                if (currow) {
                                    while (--currow && row_hidden[currow])
                                        continue;
                                    curcol = maxcols - 1;
                                } else {
                                    error("At start of table");
                                    break;
                                }
                            }
                        } while (col_hidden[curcol] || !VALID_CELL(p, currow, curcol));
                    }
                    rowsinrange = 1;
                    colsinrange = fwidth[curcol];
                    break;
                case '^':
                    gototop();
                    break;
#ifdef KEY_HELP
                case KEY_HELP:
#endif
                case '?':
                    help(HELP_INTRO);
                    break;
                case '\\':
                    if (!locked_cell(currow, curcol)) {
                        /* set mark 0 */
                        savedrow[27] = currow;
                        savedcol[27] = curcol;
                        savedstrow[27] = strow;
                        savedstcol[27] = stcol;

                        set_line("label %s = \"", v_name(currow, curcol));
                        insert_mode();
                    }
                    break;

                case '<':
                    if (!locked_cell(currow, curcol)) {
                        /* set mark 0 */
                        savedrow[27] = currow;
                        savedcol[27] = curcol;
                        savedstrow[27] = strow;
                        savedstcol[27] = stcol;

                        set_line("leftstring %s = \"", v_name(currow, curcol));
                        insert_mode();
                    }
                    break;

                case '>':
                    if (!locked_cell(currow, curcol)) {
                        /* set mark 0 */
                        savedrow[27] = currow;
                        savedcol[27] = curcol;
                        savedstrow[27] = strow;
                        savedstcol[27] = stcol;

                       set_line("rightstring %s = \"", v_name(currow, curcol));
                       insert_mode();
                    }
                    break;
                case '{':
                    p = *ATBL(tbl, currow, curcol);
                    if (p && p->label)
                        ljustify(currow, curcol, currow, curcol);
                    else
                        error("Nothing to justify");
                    break;
                case '}':
                    p = *ATBL(tbl, currow, curcol);
                    if (p && p->label)
                        rjustify(currow, curcol, currow, curcol);
                    else
                        error("Nothing to justify");
                    break;
                case '|':
                    p = *ATBL(tbl, currow, curcol);
                    if (p && p->label)
                        center(currow, curcol, currow, curcol);
                    else
                        error("Nothing to center");
                    break;
                case 'e':
                    if (!locked_cell(currow, curcol)) {
                        p = lookat(currow, curcol);

                        /* set mark 0 */
                        savedrow[27] = currow;
                        savedcol[27] = curcol;
                        savedstrow[27] = strow;
                        savedstcol[27] = stcol;

                        editv(currow, curcol);
                        if (!(p->flags & IS_VALID)) {
                            insert_mode();
                        } else
                            edit_mode();
                    }
                    break;
                case 'E':
                    if (!locked_cell(currow, curcol)) {
                        /* set mark 0 */
                        savedrow[27] = currow;
                        savedcol[27] = curcol;
                        savedstrow[27] = strow;
                        savedstcol[27] = stcol;

                        edits(currow, curcol);
                        edit_mode();
                    }
                    break;
                case 'f':
                    formatcol(uarg);
                    break;
                case 'F':
                    p = *ATBL(tbl, currow, curcol);
                    if (p && p->format) {
                        set_line("fmt [format] %s \"%s", v_name(currow, curcol), p->format);
                        edit_mode();
                    } else {
                        set_line("fmt [format] %s \"", v_name(currow, curcol));
                        insert_mode();
                    }
                    break;
                case 'C': {
                    if (braille) {
                        braillealt ^= 1;
                        break;
                    }
                    error("Color number to set (1-8)?");
                    c = nmgetch(1);
                    if (c == ESC || c == ctl('g')) {
                        break;
                    }
                    if ((c -= '0') < 1 || c > CPAIRS) {
                        error("Invalid color number.");
                        break;
                    }
                    set_line("color %d = ", c);
                    if (cpairs[c] && cpairs[c]->expr) {
                        decompile(cpairs[c]->expr, 0);
                        edit_mode();
                    } else {
                        insert_mode();
                    }
                    break;
                }
#ifdef KEY_FIND
                case KEY_FIND:
#endif
                case 'g':
                    set_line("goto [v] ");
                    insert_mode();
                    break;
                case 'n':
                    go_last();
                    break;
                case 'P':
                    set_line("put [\"dest\" range] \"");
                    if (*curfile) {
                        ext = get_extension(curfile);
                        /* keep the extension unless .sc or scext */
                        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, scext)))
                            ext += strlen(ext);
                        error("Default path is \"%.*s.%s\"",
                              (int)(ext - curfile), curfile,
                              scext ? scext : "sc");
                    }
                    insert_mode();
                    break;
                case 'M':
                    set_line("merge [\"source\"] \"");
                    insert_mode();
                    break;
                case 'R':
                    if (mdir)
                        set_line("merge [\"macro_file\"] \"%s", mdir);
                    else
                        set_line("merge [\"macro_file\"] \"");
                    insert_mode();
                    break;
                case 'D':
                    set_line("mdir [\"macro_directory\"] \"");
                    insert_mode();
                    break;
                case 'A':
                    if (autorun)
                        set_line("autorun [\"macro_file\"] \"%s", autorun);
                    else
                        set_line("autorun [\"macro_file\"] \"");
                    insert_mode();
                    break;
                case 'G':
                    set_line("get [\"source\"] \"");
                    if (*curfile)
                        error("Default file is \"%s\"", curfile);
                    insert_mode();
                    break;
                case 'W':
                    set_line("write [\"dest\" range] \"");
                    if (*curfile) {
                        ext = get_extension(curfile);
                        /* keep the extension unless .sc or scext */
                        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, scext)))
                            ext += strlen(ext);
                        error("Default file is \"%.*s.%s\"",
                              (int)(ext - curfile), curfile,
                              ascext ? ascext : "asc");
                    }
                    insert_mode();
                    break;
                case 'S':       /* set options */
                    set_line("set ");
                    error("Options:byrows,bycols,iterations=n,tblstyle=(0|tbl|latex|slatex|tex|frame),<MORE>");
                    insert_mode();
                    break;
                case 'T':       /* tbl output */
                    set_line("tbl [\"dest\" range] \"");
                    if (*curfile) {
                        ext = get_extension(curfile);
                        /* keep the extension unless .sc or scext */
                        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, scext)))
                            ext += strlen(ext);
                        if (tbl_style == 0) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - curfile), curfile,
                                  tbl0ext ? tbl0ext : "cln");
                        } else if (tbl_style == TBL) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - curfile), curfile,
                                  tblext ? tblext : "tbl");
                        } else if (tbl_style == LATEX) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - curfile), curfile,
                                  latexext ? latexext : "lat");
                        } else if (tbl_style == SLATEX) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - curfile), curfile,
                                  slatexext ? slatexext : "stx");
                        } else if (tbl_style == TEX) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - curfile), curfile,
                                  texext ? texext : "tex");
                        }
                    }
                    insert_mode();
                    break;
#ifdef KEY_DC
                case KEY_DC:
#endif
                case 'x':
                    if (calc_order == BYROWS) {
                        eraser(lookat(currow, curcol),
                                lookat(currow, curcol + uarg - 1));
                    } else {
                        eraser(lookat(currow, curcol),
                               lookat(currow + uarg - 1, curcol));
                    }
                    break;
                case 'Q':
                case 'q':
                    running = 0;
                    break;
                case KEY_LEFT:
                case 'h':
                    backcol(uarg);
                    break;
                case KEY_DOWN:
                case 'j':
                    forwrow(uarg);
                    break;
                case KEY_UP:
                case 'k':
                    backrow(uarg);
                    break;
                case 'H':
                    backcol(curcol - stcol + 2);
                    break;
#ifdef KEY_NPAGE
                case KEY_NPAGE:                 /* next page */
#endif
                case 'J':
                    ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                    forwrow(uarg * ps);
                    strow = strow + uarg * ps;
                    FullUpdate++;
                    break;
#ifdef  KEY_PPAGE
                case KEY_PPAGE:                 /* previous page */
#endif
                case KEY_ALT('v'):
                case 'K':
                    ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                    backrow(uarg * ps);
                    strow = strow - uarg * ps;
                    if (strow < 0) strow = 0;
                    FullUpdate++;
                    break;
#ifdef KEY_HOME
                case KEY_HOME:
                    gohome();
                    break;
#endif
                case 'L':
                    forwcol(lcols - (curcol - stcol) + 1);
                    break;
                case KEY_RIGHT:
                case ' ':
                case 'l':
                    forwcol(uarg);
                    break;
                case 'm':
                    markcell();
                    break;
                case 'c':
                    error("Copy marked cell:");
                    c = nmgetch(1);
                    if (c == ESC || c == ctl('g')) {
                        break;
                    }
                    if (c == '.') {
                        copy(NULL, NULL, lookat(currow, curcol), NULL);
                        set_line("copy [dest_range src_range] ");
                        insert_mode();
                        startshow();
                        break;
                    }
                    if ((c = checkmark(c)) < 0) {
                        error("Invalid mark (must be a-z, 0-9, ` or \')");
                        break;
                    }
                    if (savedrow[c] == -1) {
                        error("Mark not set");
                        break;
                    }
                    {
                        int c1;
                        struct ent *n;

                        p = *ATBL(tbl, savedrow[c], savedcol[c]);
                        for (c1 = curcol; uarg-- && c1 < maxcols; c1++) {
                            if ((n = *ATBL(tbl, currow, c1))) {
                                if (n->flags & IS_LOCKED)
                                    continue;
                                if (!p) {
                                    clearent(n);
                                    continue;
                                }
                            } else {
                                if (!p) break;
                                n = lookat(currow, c1);
                            }
                            copyent(n, p, currow - savedrow[c], c1 - savedcol[c],
                                    0, 0, maxrow, maxcol, 0);
                            n->flags |= IS_CHANGED;
                        }

                        FullUpdate++;
                        modflg++;
                        break;
                    }
                case '`':
                case '\'':
                    dotick(c);
                    break;
                case '*':
                    error("Note: Add/Delete/Show/*(go to note)?");
                    c = nmgetch(1);
                    if (c == ESC || c == ctl('g')) {
                        break;
                    }
                    if (c == 'a' || c == 'A') {
                        set_line("addnote [target range] %s ", v_name(currow, curcol));
                        insert_mode();
                        write_line(ctl('v'));
                        FullUpdate++;
                        break;
                    }
                    if (c == 'd' || c == 'D') {
                        p = lookat(currow, curcol);
                        p->nrow = p->ncol = -1;
                        p->flags |= IS_CHANGED;
                        modflg++;
                        FullUpdate++;
                        break;
                    }
                    if (c == 's' || c == 'S') {
                        FullUpdate++;
                        shownote = 1;
                        clearok(stdscr,1);
                        error("Highlighted cells have attached notes.");
                        break;
                    }
                    if (c == '*') {
                        gotonote();
                        break;
                    }
                    error("Invalid command");
                    break;
                case 'z':
                    switch (c = nmgetch(1)) {
                    case ctl('m'):
                        strow = currow;
                        FullUpdate++;
                        clearok(stdscr,1);
                        break;
                    case '.':
                        strow = -1;
                        FullUpdate++;
                        clearok(stdscr,1);
                        break;
                    case '|':
                        stcol = -1;
                        FullUpdate++;
                        clearok(stdscr,1);
                        break;
                    case 'c':
                        /* Force centering of current cell (or range, if
                         * we've just jumped to a new range with the goto
                         * command).
                         */
                        strow = -1;
                        stcol = -1;
                        FullUpdate++;
                        clearok(stdscr,1);
                        break;
                    default:
                        break;
                    }
                    break;
#ifdef KEY_RESIZE
                case KEY_RESIZE:
#ifndef SIGWINCH
                    winchg();
#endif
                    break;
#endif
#ifdef NCURSES_MOUSE_VERSION
                case KEY_MOUSE:
                    if (getmouse(&mevent) != OK)
                        break;
                    if (mevent.bstate & BUTTON1_CLICKED) {
                        mouse_sel_cell(0);
                        update(0);
                    } else if (mevent.bstate & BUTTON1_PRESSED) {
                        mouse_sel_cell(1);
                    } else if (mevent.bstate & BUTTON1_RELEASED) {
                        if (!mouse_sel_cell(2))
                            update(0);
                    }
# if NCURSES_MOUSE_VERSION >= 2
                    else if (mevent.bstate & BUTTON4_PRESSED) {
                        scroll_up(1);
                        FullUpdate++;
                        update(0);
                    } else if (mevent.bstate & BUTTON5_PRESSED) {
                        scroll_down();
                        FullUpdate++;
                        update(0);
                    }
# endif
                    break;
#endif
                default:
                    if (c < 32 || c >= 127) {
                        error("Unhandled key: %d (%#x)\n", c, c);
                    } else {
                        error("No such command (%c)", c);
                    }
                    break;
                }
            }
            edistate = nedistate;
            uarg = narg;
        }                           /* while (running) */
        inloop = modcheck(" before exiting");
    }                           /*  while (inloop) */
}

static void scroll_down(void) {
    strow++;
    // XXX: check maximum row?
    while (row_hidden[strow])
        strow++;
    if (currow < strow)
        currow = strow;
}

static void scroll_up(int x) {
    if (strow)
        strow--;
    while (strow && row_hidden[strow])
        strow--;
    forwrow(x);
    if (currow >= lastendrow)
        backrow(1);
    backrow(x);
}

static void colshow_op(void) {
    int i, j;
    for (i = 0; i < maxcols; i++) {
        if (col_hidden[i])
            break;
    }
    for (j = i; j < maxcols; j++) {
        if (!col_hidden[j])
            break;
    }
    j--;
    if (i >= maxcols) {
        error("No hidden columns to show");
    } else {
        set_line("show %s:%s", coltoa(i), coltoa(j));
    }
}

static void rowshow_op(void) {
    int i, j;
    for (i = 0; i < maxrows; i++) {
        if (row_hidden[i])
            break;
    }
    for (j = i; j < maxrows; j++) {
        if (!row_hidden[j])
            break;
    }
    j--;
    if (i >= maxrows) {
        error("No hidden rows to show");
    } else {
        set_line("show %d:%d", i, j);
    }
}

static int checkmark(int c) {
    if (c == '`' || c == '\'')
        return 0;
    else if (c >= 'a' && c <= 'z')
        return c - 'a' + 1;
    else if (c >= '0' && c <= '9')
        return c - '0' + 1 + 26;
    else
        return -1;
}

static void markcell(void) {
    int c;

    error("Mark cell:");
    c = nmgetch(1);
    if (c == ESC || c == ctl('g')) {
        return;
    }
    if ((c = checkmark(c)) < 0) {
        error("Invalid mark (must be letter, digit, ` or ')");
        return;
    }
    savedrow[c] = currow;
    savedcol[c] = curcol;
    savedstrow[c] = strow;
    savedstcol[c] = stcol;
}

static void dotick(int tick) {
    int c;

    remember(0);

    error("Go to marked cell:");
    c = nmgetch(1);
    if (c == ESC || c == ctl('g')) {
        return;
    }
    if ((c = checkmark(c)) < 0) {
        error("Invalid mark (must be letter, digit, ` or ')");
        return;
    }
    if (savedrow[c] == -1) {
        error("Mark not set");
        return;
    }
    currow = savedrow[c];
    curcol = savedcol[c];
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    if (tick == '\'') {
        strow = savedstrow[c];
        stcol = savedstcol[c];
        gs.stflag = 1;
    } else {
        gs.stflag = 0;
    }
    remember(1);

    FullUpdate++;
}

static void write_line(int c) {
    struct frange *fr;
    struct crange *cr;
    struct ent *p;
    int ps;

    CLEAR_LINE;  // XXX: possibly redundant
    if (c != ctl('i'))
        completethis = NULL;
    if (mode == EDIT_MODE) {
        nosavedot = 0;
        switch (c) {
        case KEY_BACKSPACE:
        case ctl('h'):  linelim = back_line(uarg);                      break;
        case ctl('i'):  dotab();                                        break;
        case ctl('m'):  if (search_ind != ' ')
                            search_hist();
                        else
                            cr_line();
                        break;
        case 'v':
        case ctl('v'):  toggle_navigate_mode();                         break;
        case ESC:       stop_edit();                                    break;
        case '+':       forw_hist();                                    break;
        case '-':       back_hist();                                    break;
        case KEY_END:
        case ctl('e'):
        case '$':       last_col();                                     break;
        case '.':       dotcmd();                                       break;
        case '!':       doshell();                                      break;
        case ';':       if (findfunc)
                            ungetch(findchar);
                        else
                            break;
                        findchar = 0;
                        if (findfunc == 'f')
                            linelim = find_char(uarg, finddir);
                        else
                            linelim = to_char(uarg, finddir);
                                                                       break;
        case ',':       if (findfunc)
                            ungetch(findchar);
                        else
                            break;
                        findchar = 0;
                        if (findfunc == 'f')
                            linelim = find_char(uarg, -finddir);
                        else
                            linelim = to_char(uarg, -finddir);
                                                                        break;
        case '~':       u_save(c); change_case(uarg);                   break;
        case '%':       match_paren();                                  break;
#ifdef KEY_FIND
        case KEY_FIND:
#endif
        case '?':
        case '/':       search_mode(c);                                 break;
        case KEY_HOME:
        case ctl('a'):
        case '0':       col_0();                                        break;
        case 'A':       u_save(c); last_col(); append_line();           break;
        case 'B':       linelim = back_word(uarg, 1);                   break;
        case 'C':       u_save(c); del_to_end(); append_line();         break;
        case 'D':       u_save(c); del_to_end();                        break;
        case 'E':       linelim = forw_word(uarg, 1, 1, 0);             break;
        case 'F':       linelim = find_char(uarg, -1);                  break;
        case 'G':       if (histp > 0) histp = lasthist; forw_hist();   break;
        case 'I':       u_save(c); col_0(); insert_mode();              break;
        case 'N':       search_again(true);                             break;
        case 'P':       u_save(c);
                        ins_string(putbuf);
                        linelim = back_line(1);                         break;
        case 'R':       u_save(c); replace_mode();                      break;
        case 'T':       linelim = to_char(uarg, -1);                    break;
        case 'W':       linelim = forw_word(uarg, 0, 1, 0);             break;
        case 'X':       u_save(c); back_space();                        break;
        case 'Y':       yank_chars(linelim, strlen(line), 0);           break;
        case 'a':       u_save(c); append_line();                       break;
        case 'b':       linelim = back_word(uarg, 0);                   break;
        case 'c':       u_save(c); yank_cmd(1, 1); insert_mode();       break;
        case 'd':       u_save(c); yank_cmd(1, 0);                      break;
        case 'e':       linelim = forw_word(uarg, 1, 0, 0);             break;
        case 'f':       linelim = find_char(uarg, 1);                   break;
        case KEY_LEFT:
        case ctl('b'):
        case 'h':       linelim = back_line(uarg);                      break;
        case KEY_IC:
        case 'i':       u_save(c); insert_mode();                       break;
        case KEY_DOWN:
        case 'j':       forw_hist();                                    break;
        case KEY_UP:
        case 'k':       back_hist();                                    break;
        case KEY_RIGHT:
        case ctl('f'):
        case ' ':
        case 'l':       linelim = forw_line(uarg, 0);                   break;
        case 'n':       search_again(false);                            break;
        case 'p':       u_save(c);
                        linelim = forw_line(1, 1);
                        ins_string(putbuf);
                        linelim = back_line(1);                         break;
        case 'q':       stop_edit();                                    break;
        case 'r':       u_save(c); rep_char();                          break;
        case 's':       u_save(c); del_in_line(uarg, 0); insert_mode(); break;
        case 't':       linelim = to_char(uarg, 1);                     break;
        case 'u':       restore_it();                                   break;
        case 'w':       linelim = forw_word(uarg, 0, 0, 0);             break;
        case KEY_DC:
        case 'x':       u_save(c); del_in_line(uarg, 1);                break;
        case 'y':       yank_cmd(0, 0);                                 break;
#ifdef NCURSES_MOUSE_VERSION
        case KEY_MOUSE:
            if (getmouse(&mevent) != OK || mevent.y)
                break;
            if (mevent.bstate & BUTTON1_CLICKED)
                mouse_set_pos(0);
            else if (mevent.bstate & BUTTON1_PRESSED)
                mouse_set_pos(1);
            else if (mevent.bstate & BUTTON1_RELEASED)
                mouse_set_pos(2);
            break;
#endif
        default:
            break;
        }
    } else if (mode == INSERT_MODE) {
        if (c == ctl('m'))
            savedot(ESC);
        else
            savedot(c);
        switch (c) {
        case KEY_BACKSPACE:
        case ctl('h'):        back_space();                           break;
        case ctl('i'):        dotab();                                break;
        case ctl('m'):        cr_line();                              break;
        case ctl('v'):        toggle_navigate_mode();                 break;
        case KEY_LEFT:
        case ctl('b'):        if (numeric_field) {
                                    if (linelim > 0 &&
                                            (size_t)linelim == strlen(line) &&
                                            (line[linelim - 1] == '+' ||
                                             line[linelim - 1] == '-')) {
                                        toggle_navigate_mode();
                                        backcol(1);
                                    } else {
                                        c = craction;
                                        craction = 0;
                                        cr_line();
                                        craction = c;
                                        backcol(1);
                                    }
                                } else {
                                    linelim = back_line(uarg);
                                    istart = linelim;
                                }   break;
        case KEY_RIGHT:
        case ctl('f'):          if (numeric_field) {
                                    if (linelim > 0 &&
                                            (size_t)linelim == strlen(line) &&
                                            (line[linelim - 1] == '+' ||
                                             line[linelim - 1] == '-')) {
                                        toggle_navigate_mode();
                                        forwcol(1);
                                    } else {
                                        c = craction;
                                        craction = 0;
                                        cr_line();
                                        craction = c;
                                        forwcol(1);
                                    }
                                } else {
                                    linelim = forw_line(uarg, 1);
                                    istart = linelim;
                                }   break;
        case KEY_DOWN:
        case ctl('n'):          if (numeric_field) {
                                    if (linelim > 0 &&
                                            (size_t)linelim == strlen(line) &&
                                            (line[linelim - 1] == '+' ||
                                             line[linelim - 1] == '-')) {
                                        toggle_navigate_mode();
                                        forwrow(1);
                                    } else {
                                        c = craction;
                                        craction = 0;
                                        cr_line();
                                        craction = c;
                                        forwrow(1);
                                    }
                                } else {
                                    forw_hist();
                                    istart = linelim;
                                }   break;
        case KEY_UP:
        case ctl('p'):          if (numeric_field) {
                                    if (linelim > 0 &&
                                            (size_t)linelim == strlen(line) &&
                                            (line[linelim - 1] == '+' ||
                                             line[linelim - 1] == '-')) {
                                        toggle_navigate_mode();
                                        backrow(1);
                                    } else {
                                        c = craction;
                                        craction = 0;
                                        cr_line();
                                        craction = c;
                                        backrow(1);
                                    }
                                } else {
                                    back_hist();
                                    istart = linelim;
                                }   break;
        case KEY_HOME:
        case ctl('a'):          col_0();                                break;
        case KEY_END:
        case ctl('e'):          last_col();                             break;
        case ESC:               ins_in_line(0);
                                edit_mode();                            break;
        /* '\035' is ^], which expands abbreviations without inserting another
         * character in the line
         */
        case '\035':            if (linelim > 0) doabbrev();            break;
#ifdef NCURSES_MOUSE_VERSION
        case KEY_MOUSE:
            if (getmouse(&mevent) != OK || mevent.y)
                break;
            if (mevent.bstate & BUTTON1_CLICKED)
                mouse_set_pos(0);
            else if (mevent.bstate & BUTTON1_PRESSED)
                mouse_set_pos(1);
            else if (mevent.bstate & BUTTON1_RELEASED)
                mouse_set_pos(2);
            break;
#endif
        default:                ins_in_line(c);                         break;
        }
    } else if (mode == SEARCH_MODE) {
        switch (c) {
        case KEY_BACKSPACE:
        case ctl('h'):          back_space();                           break;
        case ctl('m'):          search_hist();                          break;
        case ESC:               ins_in_line(0);
                                edit_mode();                            break;
        /* '\035' is ^], which expands abbreviations without inserting another
         * character in the line
         */
        case '\035':            if (linelim > 0) doabbrev();            break;
        default:                ins_in_line(c);                         break;
        }
    } else if (mode == REP_MODE) {
        savedot(c);
        switch (c) {
        case KEY_BACKSPACE:
        case ctl('h'):          if (linelim >= 0 &&
                                    (size_t)linelim > strlen(undo_line))
                                    back_space();
                                else {
                                    linelim = back_line(1);
                                    line[linelim] = undo_line[linelim];
                                }                                       break;
        case ctl('m'):          cr_line();                              break;
        case ESC:               edit_mode();                            break;
        default:                replace_in_line(c);                     break;
        }
    } else if (mode == NAVIGATE_MODE) {
        switch (c) {
        case '.':
        case ':':
        case ctl('i'):          if (!showrange) {
                                    toggle_navigate_mode();
                                    startshow();
                                } else if ((size_t)linelim == strlen(line) &&
                                           ((linelim > 0 && (line[linelim - 1] == '+' || line[linelim - 1] == '-')) ||
                                            (linelim > 1 && (line[linelim - 1] == ' ' && line[linelim - 2] == '='))))
                                {
                                    ins_string("@sum(");
                                    showdr();
                                    ins_in_line(')');
                                } else {
                                    showdr();
                                    ins_in_line(' ');
                                }                                       break;
        case ' ':               if (showrange) {
                                    showdr();
                                    ins_in_line(' ');
                                    toggle_navigate_mode();
                                } else {
                                    forwcol(uarg);
                                }                                       break;
        case '+':
        case '-':               if (!showrange) {
                                    ins_string(v_name(currow, curcol));
                                    ins_in_line(c);
                                } else if ((size_t)linelim == strlen(line) &&
                                           ((linelim > 0 && (line[linelim - 1] == '+' || line[linelim - 1] == '-')) ||
                                            (linelim > 1 && (line[linelim - 1] == ' ' && line[linelim - 2] == '='))))
                                {
                                    ins_string("@sum(");
                                    showdr();
                                    ins_in_line(')');
                                    ins_in_line(c);
                                    toggle_navigate_mode();
                                } else {
                                    showdr();
                                    ins_in_line(')');
                                    ins_in_line(c);
                                }                                       break;
        case ctl('m'):          if (!showrange) {
                                    ins_string(v_name(currow, curcol));
                                    toggle_navigate_mode();
                                } else {
                                    toggle_navigate_mode();
                                    cr_line();
                                }                                       break;
        case 'v':               {   /* insert variable value */
                                    char temp[100];

                                    p = *ATBL(tbl, currow, curcol);
                                    if (p && (p->flags & IS_VALID)) {
                                        snprintf(temp, sizeof temp, "%.*f",
                                                 precision[curcol], p->v);
                                        ins_string(temp);
                                        toggle_navigate_mode();
                                    }
                                }                                       break;
        case 'c':               if ((cr = find_crange(currow, curcol))) {
                                    ins_string(r_name(cr->r_left->row,
                                                      cr->r_left->col,
                                                      cr->r_right->row,
                                                      cr->r_right->col));
                                    toggle_navigate_mode();
                                    ins_in_line(' ');
                                    showrange = 0;
                                }                                       break;
        case 'f':               if ((fr = find_frange(currow, curcol))) {
                                    ins_string(r_name(fr->or_left->row,
                                                      fr->or_left->col,
                                                      fr->or_right->row,
                                                      fr->or_right->col));
                                    toggle_navigate_mode();
                                    ins_in_line(' ');
                                    showrange = 0;
                                }                                       break;
        case 'r':               if ((fr = find_frange(currow, curcol))) {
                                    ins_string(r_name(fr->ir_left->row,
                                                      fr->ir_left->col,
                                                      fr->ir_right->row,
                                                      fr->ir_right->col));
                                    toggle_navigate_mode();
                                    ins_in_line(' ');
                                    showrange = 0;
                                }                                       break;
        case KEY_LEFT:
        case 'h':               backcol(uarg);                          break;
        case KEY_RIGHT:
        case 'l':               forwcol(uarg);                          break;
        case KEY_DOWN:
        case ctl('n'):
        case 'j':               forwrow(uarg);                          break;
        case KEY_UP:
        case ctl('p'):
        case 'k':               backrow(uarg);                          break;
        case 'q':
        case ctl('g'):
        case ctl('v'):
        case ESC:               toggle_navigate_mode();
                                showrange = 0;                          break;
        case 'H':               backcol(curcol - stcol + 2);
                                                                        break;
        case KEY_NPAGE:         /* next page */
        case ctl('f'):
        case 'J':               ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                                forwrow(uarg * ps);
                                strow = strow + (uarg * ps);
                                FullUpdate++;
                                                                        break;
        case KEY_PPAGE:         /* previous page */
        case ctl('b'):
        case 'K':               ps = pagesize ? pagesize : (LINES - RESROW - framerows) / 2;
                                backrow(uarg * ps);
                                strow = strow - (uarg * ps);
                                if (strow < 0) strow = 0;
                                FullUpdate++;
                                                                        break;
        case 'L':               forwcol(lcols - (curcol - stcol) + 1);  break;
        case ctl('a'):
        case KEY_HOME:          gohome();                               break;
        case '0':               leftlimit();                            break;
        case '$':               rightlimit();                           break;
        case '^':               gototop();                              break;
        case '#':               gotobottom();                           break;
        case 'o':               if (showrange) {
                                    int r = currow;
                                    int cc = curcol;
                                    currow = showsr;
                                    showsr = r;
                                    curcol = showsc;
                                    showsc = cc;
                                    rowsinrange = 1;
                                    colsinrange = fwidth[curcol];
                                }                                       break;
        case 'm':               markcell();                             break;
        case '`': case '\'':    dotick(c);                              break;
        case '*':               if (nmgetch(0) == '*') gotonote();       break;
        case 'g':               dogoto();                               break;
        case 'n':               go_last();                              break;

        case 'w':               while (--uarg >= 0) {
                                    do {
                                        if (curcol < maxcols - 1)
                                            curcol++;
                                        else {
                                            if (currow < maxrows - 1) {
                                                while (++currow < maxrows - 1 && row_hidden[currow])
                                                    continue;
                                                curcol = 0;
                                            } else {
                                                // XXX: current cell was updated
                                                error("At end of table");
                                                break;
                                            }
                                        }
                                    } while (col_hidden[curcol] || !VALID_CELL(p, currow, curcol));
                                }
                                rowsinrange = 1;
                                colsinrange = fwidth[curcol];
                                break;

        case 'b':               while (--uarg >= 0) {
                                    do {
                                        if (curcol)
                                            curcol--;
                                        else {
                                            if (currow) {
                                                while (--currow && row_hidden[currow])
                                                    continue;
                                                curcol = maxcols - 1;
                                            } else {
                                                // XXX: current cell was updated
                                                error("At start of table");
                                                break;
                                            }
                                        }
                                    } while (col_hidden[curcol] || !VALID_CELL(p, currow, curcol));
                                }
                                rowsinrange = 1;
                                colsinrange = fwidth[curcol];
                                break;

        case 'C':               if (braille)
                                    braillealt ^= 1;
                                break;
        }
    }
}

static void edit_mode(void) {
    mode_ind = 'e';
    mode = EDIT_MODE;
    if (linelim < 0)    /* -1 says stop editing, ...so we still aren't */
        return;
    numeric_field = 0;
    linelim = back_line(1);
}

static void insert_mode(void) {
    mode_ind = 'i';
    mode = INSERT_MODE;
    istart = linelim;
}

static void search_mode(char sind) {
    if (search_ind == ' ') {
        /*
         * This back and forth movement through the history is just a quick
         * way to force the current command to be saved in history[0].histline,
         * allocating space for it if necessary.  The command will be copied
         * back into line by search_hist() before the search is done. - CRM
         */
        back_hist();
        forw_hist();
        line[0] = '\0';
        linelim = 0;
        mode_ind = 'i';
        search_ind = sind;
        search_dir = sind == '/' ? 1 : 0;
        mode = SEARCH_MODE;
        istart = linelim;
    }
}

static void replace_mode(void) {
    mode_ind = 'R';
    mode = REP_MODE;
}

static void toggle_navigate_mode(void) {
    static char prev_mode = NAVIGATE_MODE;
    int limtmp;

    switch (prev_mode) {
    case INSERT_MODE:
        if (mode == NAVIGATE_MODE) {
            prev_mode = NAVIGATE_MODE;
            insert_mode();
            break;
        } else
            return;
    case EDIT_MODE:
        if (mode == NAVIGATE_MODE) {
            prev_mode = NAVIGATE_MODE;
            limtmp = linelim;
            edit_mode();
            linelim = limtmp;
            break;
        } else
            return;
    case NAVIGATE_MODE:
        prev_mode = mode;
        mode_ind = 'v';
        mode = NAVIGATE_MODE;
        break;
    default:
        prev_mode = NAVIGATE_MODE;
        break;
    }
}

static void dotab(void) {
    static struct range *firstmatch;
    static struct range *lastmatch;
    static struct range *nextmatch;
    int len;

    if ((linelim > 0 && isalnumchar_(line[linelim-1])) ||
            (completethis && line[linelim-1] == ' ')) {
        if (!completethis) {
            for (completethis = line + linelim - 1;
                 isalnumchar_(*completethis);
                 completethis--)
                continue;
            completethis++;
            len = line + linelim - completethis;
            if (!find_range_name(completethis, -len, &lastmatch)) {
                firstmatch = lastmatch;
                while (firstmatch->r_next &&
                       !strncmp(completethis, firstmatch->r_next->r_name, len))
                    firstmatch = firstmatch->r_next;
                nextmatch = firstmatch;
            } else
                nextmatch = NULL;
        }
        if (nextmatch) {
            len = line + linelim - completethis;
            strsplice(line, sizeof line, completethis - line, len, NULL, 0);
            linelim -= len;
            ins_string(nextmatch->r_name);
            if (completethis[-1] == ' ' && line[linelim] != ' ')
                ins_in_line(' ');
            if (nextmatch == lastmatch)
                nextmatch = firstmatch;
            else
                nextmatch = nextmatch->r_prev;
        }
    } else
        startshow();
}

/* show the current range (see ^I), we are moving around to define a range */
static void startshow(void) {
    showrange = 1;
    showsr = currow;
    showsc = curcol;
    toggle_navigate_mode();
}

/* insert the range we defined by moving around the screen, see startshow() */
static void showdr(void) {
    char r[32];
    int minsr = showsr < currow ? showsr : currow;
    int minsc = showsc < curcol ? showsc : curcol;
    int maxsr = showsr > currow ? showsr : currow;
    int maxsc = showsc > curcol ? showsc : curcol;

    if (showrange == SHOWROWS) {
        snprintf(r, sizeof r, "%d:%d", minsr, maxsr);
        ins_string(r);
    } else if (showrange == SHOWCOLS) {
        snprintf(r, sizeof r, "%s:%s", coltoa(minsc), coltoa(maxsc));
        ins_string(r);
    } else {
        ins_string(r_name(minsr, minsc, maxsr, maxsc));
    }
    toggle_navigate_mode();
    showrange = 0;
}

/* dot command functions.  Saves info so we can redo on a '.' command */
static void savedot(int c) {
    if (do_dot || nosavedot || (c == '\n'))
        return;

    if (doti == 0) dotarg = uarg;
    if (doti < DOTLEN - 1) {
        if (c > 255) {
            if (doti < DOTLEN - 2) {
                dotb[doti++] = c / 256;
                c %= 256;
            } else
                return;
        }
        dotb[doti++] = c;
        dotb[doti] = '\0';
    }
}

static void dotcmd(void) {
    static int dotcalled = 0;
    int c;

    if (dotcalled)      /* stop recursive calling of dotcmd() */
        return;
    do_dot = 1;
    doti = 0;
    if (uarg == 1)
        uarg = dotarg;
    else
        dotarg = uarg;
    while (dotb[doti] != '\0') {
        if ((c = dotb[doti++]) < 4)
            c = c * 256 + dotb[doti++];
        dotcalled = 1;
        write_line(c);
    }
    do_dot = 0;
    doti = 0;
    dotcalled = 0;
}

static int vigetch(void) {
    if (do_dot) {
        if (dotb[doti] != '\0') {
            return dotb[doti++];
        } else {
            do_dot = 0;
            doti = 0;
            return nmgetch(0);
        }
    }
    update(1);
    return nmgetch(0);
}

/* saves the current line for possible use by an undo cmd */
static void u_save(int c) {
    if (strlen(line) + 1 > undolen) {
        undolen = strlen(line) + 40;
        undo_line = scxrealloc(undo_line, undolen);
    }
    strlcpy(undo_line, line, undolen);

    undo_lim = linelim;

    /* reset dot command if not processing it. */
    if (!do_dot) {
        doti = 0;
        savedot(c);
    }
}

/* Restores the current line saved by u_save() */
static void restore_it(void) {
    // XXX: never freed
    static char *tempc = NULL;
    static unsigned templen = 0;
    int tempi;

    if ((undo_line == NULL) || (*undo_line == '\0'))
        return;

    if (strlen(line) + 1 > templen) {
        templen = strlen(line) + 40;
        tempc = scxrealloc(tempc, templen);
    }

    strlcpy(tempc, line, templen);
    tempi = linelim;
    strlcpy(line, undo_line, sizeof line);
    linelim = undo_lim;
    strlcpy(undo_line, tempc, undolen);
    undo_lim = tempi;
}

/* This command stops the editing process. */
static void stop_edit(void) {
    if (search_ind != ' ') {
        search_ind = ' ';
        strlcpy(line, history[0].histline, sizeof line);
        write_line('G');
    } else {
        showrange = 0;
        numeric_field = 0;
        linelim = -1;
        move(1, 0);
        clrtoeol();
    }
}

/*
 * Motion commands.  Forward motion commands take an argument
 * which, when set, cause the forward motion to continue onto
 * the null at the end of the line instead of stopping at the
 * the last character of the line.
 */
static int forw_line(int a, int stop_null) {
    ssize_t cpos = linelim;

    if (linelim < 0)
        return linelim;
    else
    if (a >= 0 && (size_t)(linelim + a) <= strlen(line))
        cpos += a;
    else
        cpos = strlen(line);

    if (cpos > 0 && (size_t)cpos == strlen(line) && !stop_null)
        return cpos - 1;
    else
        return cpos;
}

/* If end_word is non-zero, go to next end-of-word.  Otherwise, go to next
 * beginning-of-word.
 */

static int forw_word(int a, int end_word, int big_word, int stop_null) {
    int c;
    ssize_t cpos;

    cpos = linelim;

    while (cpos >= 0 && (size_t)cpos < strlen(line) && a--) {
        if (end_word)
            cpos++;

        if (line[cpos] == ' ') {
            while (line[cpos] == ' ')
                cpos++;
            if (cpos > 0 && line[cpos] == '\0')
                --cpos;
            if (!end_word)
                continue;
        }

        if (big_word)
            while ((c = line[cpos]) && c != ' ')
                cpos++;
        else if (iswordchar(line[cpos])) {
            while (iswordchar(line[cpos]))
                cpos++;
        } else {
            while ((c = line[cpos]) != '\0' && !iswordchar(c) && c != ' ')
                cpos++;
        }

        if (end_word)
            cpos--;
        else while (line[cpos] == ' ')
            cpos++;

        if (cpos > 0 && line[cpos] == '\0' && !stop_null)
            --cpos;
    }

    return cpos;
}

static int back_line(int a) {
    if (linelim > a)
        return linelim - a;
    else
        return 0;
}

static int back_word(int a, int big_word) {
    int c;
    int cpos;

    cpos = linelim;

    while (cpos > 0 && a--) {
        if (line[cpos] == ' ') {
            /* Skip white space */
            while (cpos > 0 && line[cpos] == ' ')
                --cpos;
        } else
        if (cpos > 0 && (line[cpos-1] == ' '
                     || ( iswordchar(line[cpos]) && !iswordchar(line[cpos-1]))
                     || (!iswordchar(line[cpos]) &&  iswordchar(line[cpos-1])))) {
            /* Started on the first char of a word - back up to prev. word */
            --cpos;
            while (cpos > 0 && line[cpos] == ' ')
                --cpos;
        }

        /* Skip across the word - goes 1 too far */
        if (big_word)
            while (cpos > 0 && (c = line[cpos]) && c != ' ')
                --cpos;
        else if (iswordchar(line[cpos])) {
            while (cpos > 0 && iswordchar(line[cpos]))
                --cpos;
        } else {
            while (cpos > 0 && (c = line[cpos]) != '\0' && !iswordchar(c) && c != ' ')
                --cpos;
        }

        /* We are done - fix up the one too far */
        if (cpos > 0 && line[cpos] && line[cpos+1])
            cpos++;
    }

    return cpos;
}

/* Text manipulation commands */

/* If back_null is set, back up if the deletion leaves you at the null
 * line terminator.  Otherwise, don't.
 */
static void del_in_line(int a, int back_null) {
    int len, i;

    if (linelim >= 0) {
        len = strlen(line);
        if (a > len - linelim)
            a = len - linelim;
        if (linelim == len && linelim > 0)
            linelim--;
        strlcpy(putbuf, line + linelim, a);
        putbuf[a] = '\0';
        for (i = linelim; i < len; i++)
            line[i] = line[i+a];
    }
    if (back_null && linelim > 0 && line[linelim] == '\0')
        --linelim;
}

static void ins_in_line(int c) {
    static int inabbr;

    if (c < 256) {
        if (linelim < 0 && c > 0) {
            *line = '\0';
            linelim = 0;
        }
        if (!inabbr && linelim > 0 && !(isalnum(c) || c == '_')) {
            inabbr++;
            doabbrev();
            inabbr--;
        }
        if (c > 0) {
            char buf[1];
            *buf = c;
            if (strsplice(line, sizeof line, linelim, 0, buf, 1) < (int)(sizeof line))
                linelim++;
        }
    }
}

static void ins_string(const char *s) {
    while (*s)
        ins_in_line(*s++);
}

static void doabbrev(void) {
    int len, pos;
    struct abbrev *a;
    struct abbrev *prev;

    if (istart < 0 || linelim < 2)
        return;

    if (!isalnumchar_(line[linelim - 1]) ||
        !(mode == INSERT_MODE || mode == SEARCH_MODE) || istart >= linelim)
        return;

    pos = linelim - 2;
    if (isalnumchar_(line[pos])) {
        for (; pos >= istart; pos--) {
            if (!isalnumchar_(line[pos]))
                break;
        }
    } else
    if (line[pos] != ' ') {
        for (; pos >= istart; pos--) {
            if (isalnumchar_(line[pos]) || line[pos] == ' ')
                break;
        }
    }
    pos++;

    if (istart && pos == istart) {
        if (isalnumchar_(line[pos])) {
            if (isalnumchar_(line[--pos]))
                return;
        } else {
            if (!(isalnumchar_(line[--pos]) || line[pos] == ' '))
                return;
        }
        pos++;
    }

    len = linelim - pos;
    if (len && (a = find_abbr(line + pos, len, &prev)) != NULL) {
        if (len > 1 || pos == 0 || line[pos-1] == ' ') {
            linelim = pos;
            del_in_line(len, 0);
            ins_string(a->exp);
        }
    }
}

static void append_line(void) {
    int i;

    i = linelim;
    if (i >= 0 && line[i])
        linelim++;
    insert_mode();
}

static void change_case(int a) {
    if (linelim < 0) {
        linelim = 0;
        *line = '\0';
    }
    while (a--) {
        if (islowerchar(line[linelim]))
            line[linelim] = toupperchar(line[linelim]);
        else if (isupperchar(line[linelim]))
            line[linelim] = tolowerchar(line[linelim]);
        linelim = forw_line(1, 0);
    }
}

static void rep_char(void) {
    int c;

    if (linelim < 0) {
        linelim = 0;
        *line = '\0';
    }
    c = vigetch();
    savedot(c);
    if (c < 256 && c != ESC && c != ctl('g')) {
        if (line[linelim] == '\0')
            line[linelim+1] = '\0';
        line[linelim] = c;
    }
}

static void replace_in_line(int c) {
    int len;

    if (c < 256) {
        if (linelim < 0) {
            linelim = 0;
            *line = '\0';
        }
        len = strlen(line);
        line[linelim++] = c;
        if (linelim > len)
            line[linelim] = '\0';
    }
}

static void back_space(void) {
    if (linelim == 0)
        return;

    if (line[linelim] == '\0') {
        linelim = back_line(1);
        del_in_line(1, 1);
        linelim = strlen(line);
    } else {
        linelim = back_line(1);
        del_in_line(1, 1);
    }
    if (linelim < istart)
        istart = linelim;
}

/* Setting change to 1 makes `w' act like `e' so that `cw' will act like
 * `ce', just as in vi.  Setting change to 0 causes `w' to act as usual.
 */

static int get_motion(int change) {
    int c;
    int arg2 = 0;

    c = vigetch();
    if (c == '0') {
        savedot(c);
        return 0;
    }
    while (c >= '0' && c <= '9') {
        arg2 = 10 * arg2 + c - '0';
        c = vigetch();
    }
    if (!arg2)
        arg2++;
    uarg *= arg2;
    if (!nosavedot) {
        savedot(c);
        dotarg = uarg;
    }
    switch (c) {
    case '$':   return strlen(line);
    case 'b':   return back_word(uarg, 0);
    case 'B':   return back_word(uarg, 1);
    case 'c':   return change ? -1 : linelim;
    case 'd':   return !change ? -1 : linelim;
    case 'e':   return forw_word(uarg, 1, 0, 1) + 1;
    case 'E':   return forw_word(uarg, 1, 1, 1) + 1;
    case 'f':   return ((c = find_char(uarg, 1)) == linelim) ? c : c + 1;
    case 'F':   return find_char(uarg, -1);
    case 'h':   return back_line(uarg);
    case 'l':   return forw_line(uarg, 1);
    case 't':   return ((c = to_char(uarg, 1)) == linelim) ? c : c + 1;
    case 'T':   return to_char(uarg, -1);
    case 'w':   return forw_word(uarg, change, 0, 1) + change;
    case 'W':   return forw_word(uarg, change, 1, 1) + change;
    default:    return linelim;
    }
}

static void yank_cmd(int delete, int change) {
    int cpos;

    if ((cpos = get_motion(change)) == -1) {
        cpos++;
        linelim = strlen(line);
    }
    yank_chars(cpos, linelim, delete);
}

static void yank_chars(int first, int last, int delete) {
    if (first == last)
        return;
    if (last < first) {
        int temp = last;
        last = first;
        first = temp;
    }
    linelim = first;
    *putbuf = '\0';
    strsplice(putbuf, sizeof putbuf, 0, 0, line + first, last - first);
    if (delete)
        strsplice(line, sizeof line, first, last - first, NULL, 0);
}

static void del_to_end(void) {
    if (linelim < 0)
        return;
    strlcpy(putbuf, line + linelim, sizeof putbuf);
    line[linelim] = '\0';
    linelim = back_line(1);
}

static void cr_line(void) {
    struct frange *fr;

    ins_in_line(0);
    insert_mode();
    numeric_field = 0;
    if (linelim == -1) {        /* '\n' alone will put you into insert mode */
        *line = '\0';           /* unless numeric and craction are both set */
        linelim = 0;
        if (numeric && craction)
            cellassign = 1;
        else
            return;
    }
    save_hist();
    nosavedot = 1;
    linelim = 0;
    yyparse();
    showrange = 0;
    linelim = -1;
    if (cellassign) {
        cellassign = 0;
        switch (craction) {
        case CRROWS:
            if ((rowlimit >= 0) && (currow >= rowlimit)) {
                forwcol(1);
                currow = 0;
            } else {
                if ((fr = find_frange(currow, curcol))) {
                    forwrow(1);
                    if (currow > fr->ir_right->row) {
                        backrow(1);
                        if (autowrap) {
                            forwcol(1);
                            currow = fr->ir_left->row;
                            if (row_hidden[currow])
                                forwrow(1);
                            if (curcol > fr->ir_right->col) {
                                backcol(1);
                                if (autoinsert)
                                    insertcol(1, 1);
                                else {
                                    currow = fr->ir_right->row;
                                    if (row_hidden[currow])
                                        backrow(1);
                                }
                            }
                        } else if (autoinsert)
                            insertrow(1, 1);
                    }
                } else
                    forwrow(1);
            }
            break;
        case CRCOLS:
            if ((collimit >= 0) && (curcol >= collimit)) {
                forwrow(1);
                curcol = 0;
            } else {
                if ((fr = find_frange(currow, curcol))) {
                    forwcol(1);
                    if (curcol > fr->ir_right->col) {
                        backcol(1);
                        if (autowrap) {
                            forwrow(1);
                            curcol = fr->ir_left->col;
                            if (col_hidden[curcol])
                                forwcol(1);
                            if (currow > fr->ir_right->row) {
                                backrow(1);
                                if (autoinsert)
                                    insertrow(1, 1);
                                else {
                                    curcol = fr->ir_right->col;
                                    if (col_hidden[curcol])
                                        backcol(1);
                                }
                            }
                        } else if (autoinsert)
                            insertcol(1, 1);
                    }
                } else
                    forwcol(1);
            }
            break;
        default:
            break;
        }
    }
}

static void doshell(void) {
    /*
    *  "! command"  executes command
    *  "!"      forks a shell
    *  "!!" repeats last command
    */
#ifdef NOSHELL
    error("Shell not available");
#else /* NOSHELL */
    const char *shl;
    int pid, temp, len;
    char cmd[MAXCMD];
    static char lastcmd[MAXCMD];

    if (!(shl = getenv("SHELL")))
        shl = "/bin/sh";

    deraw(1);
    fputs("! ", stdout);
    fflush(stdout);
    if (!fgets(cmd, MAXCMD, stdin))
        *cmd = '\0';
    len = strlen(cmd);
    if (len && cmd[len - 1] == '\n')
        cmd[--len] = '\0';        /* clobber \n */
    if (strcmp(cmd, "!") == 0)           /* repeat? */
        strlcpy(cmd, lastcmd, sizeof cmd);
    else
        strlcpy(lastcmd, cmd, sizeof lastcmd);

    if (modflg) {
        puts("[No write since last change]");
        fflush(stdout);
    }

    if (!(pid = fork())) {
        signal(SIGINT, SIG_DFL);  /* reset */
        if (strlen(cmd))
            execl(shl, shl, "-c", cmd, (char *)NULL);
        else
            execl(shl, shl, (char *)NULL);
        exit(-127);
    }

    while (pid != wait(&temp))
        continue;

    printf("Press any key to continue ");
    fflush(stdout);
    cbreak();
    getch();
    goraw();
    clear();
#endif /* NOSHELL */
}

static void list_all(void) {
    char px[MAXCMD];
    const char *pager;
    FILE *f;
    int pid;

    /* Show color definitions and various types of ranges */
    // XXX: deal with raw mode switch?
    if (!are_ranges() && !are_frames() && !are_colors()) {
        error("Nothing to show");
        return;
    }
    if (!(pager = getenv("PAGER")))
        pager = DFLT_PAGER;
    snprintf(px, sizeof px, "| %s", pager);
    f = openfile(px, sizeof px, &pid, NULL);
    if (!f) {
        error("Can't open pipe to %s", pager);
        return;
    }
    if (!brokenpipe) fprintf(f, "Named Ranges:\n=============\n\n");
    if (!brokenpipe) list_ranges(f);
    if (!brokenpipe) fprintf(f, "\n\nFrames:\n=======\n\n");
    if (!brokenpipe) list_frames(f);
    if (!brokenpipe) fprintf(f, "\n\nColors:\n=======\n\n");
    if (!brokenpipe) list_colors(f);
    closefile(f, pid, 0);
}

/* History functions */

static void save_hist(void) {
    if (!lasthist || strcmp(history[lasthist].histline, line)) {
        if (lasthist < 0)
            lasthist = 1;
        else
            lasthist = lasthist % HISTLEN + 1;

        if (lasthist > endhist)
            endhist = lasthist;

        if (history[lasthist].len < strlen(line) + 1) {
            history[lasthist].len = strlen(line) + 40;
            history[lasthist].histline = scxrealloc(history[lasthist].histline,
                                                    history[lasthist].len);
        }
        strlcpy(history[lasthist].histline, line, history[lasthist].len);
        histsessionnew++;
    }
    set_string(&history[0].histline, NULL);
    history[0].len = 0;
    histp = 0;
}

static void forw_hist(void) {
    if (histp == 0) {
        last_col();
        return;
    }

    if (histp == lasthist)
        histp = 0;
    else
        histp = histp % endhist + 1;

    if (lasthist >= 0) {
        strlcpy(line, history[histp].histline, sizeof line);
        last_col();
    }
    if (histp) {
        error("History line %d", endhist - lasthist + histp);
    } else {
        CLEAR_LINE;  // XXX: should not be necessary
    }
}

static void back_hist(void) {
    if (histp == 0) {
        if (history[0].len < strlen(line) + 1) {
            history[0].len = strlen(line) + 40;
            history[0].histline = scxrealloc(history[0].histline,
                    history[0].len);
        }
        strlcpy(history[0].histline, line, history[0].len);

        if (lasthist >= 0)
            histp = lasthist;
    } else if (histp == 1) {
        if (endhist != lasthist)
            histp = endhist;
    } else if (histp != ((lasthist + 1) % (endhist + 1)))
        histp--;

    if (lasthist >= 0) {
        strlcpy(line, history[histp].histline, sizeof line);
        last_col();
    }
    if (histp) {
        error("History line %d", endhist - lasthist + histp);
    } else {
        CLEAR_LINE;  // XXX: should not be necessary
    }
}

static void search_hist(void) {
#ifdef RECOMP
    char *tmp = NULL;
#endif
#if !defined(REGCOMP) && !defined(RE_COMP) && !defined(REGCMP)
    static unsigned lastsrchlen = 0;
#endif

    if (linelim < 1) {
        linelim = 0;
        edit_mode();
        return;
    }

#if defined REGCOMP
    if (last_search)
        regfree(last_search);
    else
        last_search = &preg;
    if ((errcode = regcomp(last_search, line, REG_EXTENDED))) {
        char buf[160];
        regerror(errcode, last_search, buf, sizeof buf);
        error("%s", buf);
        return;
    }
#elif defined RE_COMP
    if ((tmp = re_comp(line)) != NULL) {
        error("%s", tmp);
        return;
    }
#elif defined REGCMP
    free(last_search);
    if ((last_search = regcmp(line, NULL)) == NULL) {
        error("Invalid search string");
        return;
    }
#else
    if (strlen(line) + 1 > lastsrchlen) {
        lastsrchlen = strlen(line) + 40;
        last_search = scxrealloc(last_search, lastsrchlen);
    }
    strlcpy(last_search, line, lastsrchlen);
#endif
    strlcpy(line, history[0].histline, sizeof line);
    search_again(false);
    if (mode != EDIT_MODE) edit_mode();
    search_ind = ' ';
}

static void search_again(bool reverse) {
    int prev_match;
    int found_it = 0;
#if !defined(REGCOMP) && !defined(RE_COMP) && !defined(REGCMP)
    char *look_here;
    int do_next;
#endif

#if defined REGCOMP
    if (last_search == NULL)
        return;
#elif !defined(RE_COMP)
    if (last_search == NULL || *last_search == '\0')
        return;
#endif
    prev_match = histp > 0 ? histp : 0;
    CLEAR_LINE;

    do {
        if (lasthist > 0) {
            if (!(search_dir ^ reverse) && histp != lasthist)
                if (histp <= 0) {
                    histp = ((lasthist + 1) % endhist);
                    strlcpy(line, history[histp].histline, sizeof line);
                } else
                    forw_hist();
            else if ((search_dir ^ reverse) && histp != ((lasthist + 1) % endhist))
                back_hist();
            else {
                histp = 0;
                strlcpy(line, history[0].histline, sizeof line);
                last_col();
            }
        } else
            break;
        if (histp == prev_match) {
            if (histp <= 0) {
                error("No matches found");
                break;
            }
        }
        if (histp <= 0) {
            if (search_dir ^ reverse)
                back_hist();
            else {
                histp = ((lasthist + 1) % endhist);
                strlcpy(line, history[histp].histline, sizeof line);
            }
        }
        found_it = 0;
#if defined REGCOMP
        if (regexec(last_search, line, 0, NULL, 0) == 0)
            found_it++;
#elif defined RE_COMP
        if (re_exec(line) != 0)
            found_it++;
#elif defined REGCMP
        if (regex(last_search, line) != NULL)
            found_it++;
#else
        look_here = line;
        do_next = 0;
        while ((look_here = (char *)strchr(look_here, *last_search)) != NULL &&
                !found_it && !do_next) {

            if (strncmp(look_here, last_search, strlen(last_search)) == 0)
                found_it++;
            else if (look_here < line + strlen(line) - 1)
                look_here++;
            else
                do_next++;
        }
#endif
        if (histp == prev_match)
            break;
    } while (!found_it);
    if (found_it) {
        error("History line %d", endhist - lasthist + histp);
    } else {
        error("No matches found");
    }
    edit_mode();
    linelim = strlen(line) - 1;
}

static void readhistfile(FILE *fp) {
    if (!*histfile)
        return;
    while (fgets(line, FBUFLEN, fp)) {
        size_t len = strlen(line);
        if (len && line[len - 1] == '\n') {
            line[--len] = '\0'; /* chop the \n */
        }
        save_hist();
    }
}

// XXX: move out of vi.c
void write_hist(void) {
    int i;
    FILE *fp, *tmpfp = NULL;

    if (!*histfile)
        return;
    if (histsessionnew < HISTLEN) {
        /* write the new history for this session to a tmp file */
        tmpfp = tmpfile();
        for (i = 1; i <= histsessionnew; i++) {
            histsessionstart = histsessionstart % endhist + 1;
            if (history[histsessionstart].len > 40)
                fprintf(tmpfp, "%s\n", history[histsessionstart].histline);
        }
        fseek(tmpfp, 0, SEEK_SET);

        /* re-read the main history, then read back in the saved session hist*/
        histp = 0;
        lasthist = 0;
        endhist = -1;
        read_hist();
        readhistfile(tmpfp);

        if (fclose(tmpfp) == EOF) {
            error("fclose(tmpfile()): %s", strerror(errno));
        }
    }

    /* now write to whole lot out to the proper save file */
    if (findhome(histfile, sizeof histfile) && (fp = fopen(histfile, "w")) != NULL) {
        for (i = 1; i <= endhist; i++) {
            lasthist = lasthist % endhist + 1;
            if (history[lasthist].len > 40)
                fprintf(fp, "%s\n", history[lasthist].histline);
        }

        if (fclose(fp) == EOF) {
            error("fclose(%s): %s", histfile, strerror(errno));
        }
    }
}

void read_hist(void) {
    FILE *fp;

    if (!*histfile)
        return;

    if (findhome(histfile, sizeof histfile) && (fp = fopen(histfile, "r")) != NULL) {
        readhistfile(fp);

        if (fclose(fp) == EOF) {
            error("fclose(%s): %s", histfile, strerror(errno));
        }
    }

    histsessionstart = lasthist;
    histsessionnew = 0;
}

static void col_0(void) {
    linelim = 0;
}

static void last_col(void) {
    linelim = strlen(line);
    if (linelim > 0 && mode_ind == 'e')
        --linelim;
}

static int find_char(int a, int n) {
    int i;

    if (findchar)
        finddir = n;
    findchar = vigetch();
    if (doti > 0)
        switch (dotb[doti - 1]) {
        case 'f': case 'F': case 't': case 'T':
            savedot(findchar);
        default:
            break;
        }
    i = linelim;
    while (a--) {
        i += n;
        while (i >= 0 && line[i] && line[i] != findchar)
            i += n;
        if (i < 0 || !line[i]) {
            i = linelim;
            break;
        }
    }
    findfunc = 'f';
    return i;
}

static int to_char(int a, int n) {
    int i;
    int tmp = linelim;

    if (linelim + n >= 0 && (size_t)(linelim + n) < strlen(line))
        linelim += n;
    i = find_char(a, n);
    if (i != linelim)
        i -= n;
    linelim = tmp;
    findfunc = 't';

    return i;
}

static void match_paren(void) {
    int nest = 1;
    int tmp = linelim;

    if (line[linelim] == '(') {
        while (nest && ++linelim >= 0 && line[linelim]) {
            if (line[linelim] == '(')
                nest++;
            else if (line[linelim] == ')')
                nest--;
        }
        if (line[linelim] != ')')
            linelim = tmp;
    }
    else if (line[linelim] == ')') {
        while (nest && --linelim >= 0 && line[linelim]) {
            if (line[linelim] == ')')
                nest++;
            else if (line[linelim] == '(')
                nest--;
        }
        if (line[linelim] != '(')
            linelim = tmp;
    }
}

/* If save is 0, remember the current position.  Otherwise, if the current
 * cell has changed since the last remember(0), save the remembered location
 * for the `, ', and c comands.
 */
// XXX: move out of vi.c
void remember(int save) {
    static int remrow, remcol, remstrow, remstcol;

    if (save && (currow != remrow || curcol != remcol ||
                 strow != remstrow || stcol != remstcol)) {
        savedrow[0] = remrow;
        savedcol[0] = remcol;
        savedstrow[0] = remstrow;
        savedstcol[0] = remstcol;
    } else {
        remrow = currow;
        remcol = curcol;
        remstrow = strow;
        remstcol = stcol;
    }
}

static void gohome(void) {
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
        if (currow >= fr->ir_left->row &&
            currow <= fr->ir_right->row &&
            curcol >= fr->ir_left->col &&
            curcol <= fr->ir_right->col &&
            (currow > fr->ir_left->row ||
             curcol > fr->ir_left->col)) {
            currow = fr->ir_left->row;
            curcol = fr->ir_left->col;
        } else if (currow > fr->or_left->row ||
                   curcol > fr->or_left->col) {
            currow = fr->or_left->row;
            curcol = fr->or_left->col;
        } else {
            currow = 0;
            curcol = 0;
        }
    } else {
        currow = 0;
        curcol = 0;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
    FullUpdate++;
}

static void leftlimit(void) {
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
        if (currow >= fr->ir_left->row &&
            currow <= fr->ir_right->row &&
            curcol > fr->ir_left->col &&
            curcol <= fr->ir_right->col)
            curcol = fr->ir_left->col;
        else if (curcol > fr->or_left->col &&
                 curcol <= fr->or_right->col)
            curcol = fr->or_left->col;
        else
            curcol = 0;
    } else
        curcol = 0;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

static void rightlimit(void) {
    struct ent *p;
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
        if (currow >= fr->ir_left->row &&
            currow <= fr->ir_right->row &&
            curcol >= fr->ir_left->col &&
            curcol < fr->ir_right->col)
            curcol = fr->ir_right->col;
        else if (curcol >= fr->or_left->col &&
                 curcol < fr->or_right->col)
            curcol = fr->or_right->col;
        else {
            curcol = maxcols - 1;
            while (!VALID_CELL(p, currow, curcol) &&
                   curcol > fr->or_right->col)
                curcol--;
            if ((fr = find_frange(currow, curcol)))
                curcol = fr->or_right->col;
        }
    } else {
        curcol = maxcols - 1;
        while (!VALID_CELL(p, currow, curcol) && curcol > 0)
            curcol--;
        if ((fr = find_frange(currow, curcol)))
            curcol = fr->or_right->col;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

static void gototop(void) {
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
        if (curcol >= fr->ir_left->col &&
            curcol <= fr->ir_right->col &&
            currow > fr->ir_left->row &&
            currow <= fr->ir_right->row)
            currow = fr->ir_left->row;
        else if (currow > fr->or_left->row &&
                 currow <= fr->or_right->row)
            currow = fr->or_left->row;
        else
            currow = 0;
    } else
        currow = 0;
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

static void gotobottom(void) {
    struct ent *p;
    struct frange *fr;

    remember(0);
    if ((fr = find_frange(currow, curcol))) {
        if (curcol >= fr->ir_left->col &&
            curcol <= fr->ir_right->col &&
            currow >= fr->ir_left->row &&
            currow < fr->ir_right->row)
            currow = fr->ir_right->row;
        else if (currow >= fr->or_left->row &&
                 currow < fr->or_right->row)
            currow = fr->or_right->row;
        else {
            currow = maxrows - 1;
            while (!VALID_CELL(p, currow, curcol) &&
                   currow > fr->or_right->row)
                currow--;
            if ((fr = find_frange(currow, curcol)))
                currow = fr->or_right->row;
        }
    } else {
        currow = maxrows - 1;
        while (!VALID_CELL(p, currow, curcol) && currow > 0)
            currow--;
        if ((fr = find_frange(currow, curcol)))
            currow = fr->or_right->row;
    }
    rowsinrange = 1;
    colsinrange = fwidth[curcol];
    remember(1);
}

static void dogoto(void) {
    static char *tempc = NULL;
    static unsigned templen = 0;
    int tempi;

    if (strlen(line) + 1 > templen) {
        templen = strlen(line) + 40;
        tempc = scxrealloc(tempc, templen);
    }

    strlcpy(tempc, line, templen);
    tempi = linelim;

    /* Can't switch back to navigate mode if insert_mode() is used here
     * instead of toggle_navigate_mode(), which is what we want when doing
     * a goto from within navigate mode.
     */
    insert_mode();
    /* Tempted as I was, I had to resist making this "Where would you like
     * to go today?" - CRM :)
     */
    query("goto where?", NULL);
    if (linelim >= 0) {
        strsplice(line, sizeof line, 0, 0, "goto ", 5);
        linelim = 0;
        yyparse();
    }

    strlcpy(line, tempc, sizeof line);
    linelim = tempi;
    /* Now we need to change back to navigate mode ourselves so that
     * toggle_navigate_mode() will work properly again.
     */
    mode_ind = 'v';
    mode = NAVIGATE_MODE;
    if (!showrange)
        toggle_navigate_mode();
}

void query(const char *s, const char *data) {
    int c;

    insert_mode();
    strlcpy(line, data ? data : "", sizeof line);
    linelim = strlen(line);
    if (s != NULL)
        error("%s", s);

    while (linelim >= 0) {
        update(0);
        switch (c = nmgetch(1)) {
        case ctl('m'):
            return;
        case ctl('g'):
            line[0] = '\0';
            linelim = -1;
            update(0);
            return;
        case ctl('l'):
            FullUpdate++;
            clearok(stdscr, 1);
            update(1);
            break;
        default:
            write_line(c);
            break;
        }
    }
}

#ifdef NCURSES_MOUSE_VERSION
static int mouse_sel_cell(int mmode) { /* 0: set, 1: save, 2: cmp and set */
    int i, y, x, tx, ty;
    static int x1, y1;
    if ((y = mevent.y - RESROW) < 0 || (x = mevent.x - rescol) < 0)
        return 1;
    for (ty = strow, i = y; ; ty++) {
        if (row_hidden[ty])
            continue;
        if (--i < 0)
            break;
    }
    for (tx = stcol, i = x; ; tx++) {
        if (col_hidden[tx])
            continue;
        if ((i -= fwidth[tx]) < 0)
            break;
    }
    switch (mmode) {
    case 1:
        y1 = ty; x1 = tx;
        break;
    case 2:
        if (y1 != ty || x1 != tx)
            break;
    default:
        currow = ty; curcol = tx;
        return 0;
    }
    return 1;
}

static void mouse_set_pos(int m) {
    static int x0;
    switch (m) {
    case 1:
        x0 = mevent.x;
        break;
    case 2:
        if (x0 != mevent.x)
            break;
    default:
        linelim = mevent.x - 3;
    }
}
#endif

/*
 * Given a row/column command letter, emit a small menu, then read a qualifier
 * character for a row/column command and convert it to 'r' (row), 'c'
 * (column), or 0 (unknown).  If ch is 'p', three extra qualifiers, 'm', 'x',
 * and 't', are allowed.  If ch is 'Z', an extra qualifier 'Z' is allowed.
 */

static int get_rcqual(int ch) {
    int c;

    error("%s row/column:  r: row  c: column%s",
          (ch == KEY_IC)  ? "Insert" :
          (ch == 'i')     ? "Insert" :
          (ch == 'o')     ? "Open" :
          (ch == 'a')     ? "Append" :
          (ch == 'd')     ? "Delete" :
          (ch == 'y')     ? "Yank" :
          (ch == 'p')     ? "Pull" :
          (ch == 'v')     ? "Values" :
          (ch == 's')     ? "Show" :
          (ch == 'Z')     ? "Zap" :
                            "Select",
          (ch == 'p')     ? "  p: paste  m: merge  x: xchg  <MORE>" :
          (ch == 'Z')     ? "  Z: save/exit" :
                            "");

    refresh();

    switch (c = nmgetch(1)) {
    case 'r':       return 'r';
    case 'c':       return 'c';
    case 'p':       return (ch == 'p') ? 'p' : 0;
    case 'm':       return (ch == 'p') ? 'm' : 0;
    case 'x':       return (ch == 'p') ? 'x' : 0;
    case 't':       return (ch == 'p') ? 't' : 0;
    case 'f':       return (ch == 'p') ? 'f' : 0;
    case 'C':       return (ch == 'p') ? 'C' : 0;
    case '.':       return (ch == 'p') ? '.' : 0;
    case 'Z':       return (ch == 'Z') ? 'Z' : 0;
    case ESC:
    case ctl('g'):  return ESC;

    case 'd':       if (ch == 'd') {
                        ungetch('x');
                        return ESC;
                    } else
                        return 0;

    case 'y':       if (ch == 'y') {
                        yankr(lookat(currow, curcol),
                              lookat(currow, curcol));
                        return ESC;
                    } else
                        return 0;

    case 'v':       if (ch == 'v') {
                        valueize_area(currow, curcol, currow, curcol);
                        return ESC;
                    } else
                        return 0;

    case KEY_UP:
    case KEY_DOWN:
    case KEY_PPAGE:
    case KEY_NPAGE:
    case 'j':
    case 'k':
    case 'J':
    case 'K':
    case ctl('f'):
    case ctl('b'):
    case ctl('n'):
    case ctl('p'):  if (ch == 'd')
                        set_line("deleterow [range] ");
                    else if (ch == 'y')
                        set_line("yankrow [range] ");
                    else if (ch == 'Z')
                        set_line("hide [range] ");
                    else
                        return 0;
                    edit_mode();
                    write_line('A');
                    startshow();
                    showrange = SHOWROWS;
                    showsr = currow;
                    ungetch(c);
                    return ESC;

    case KEY_BACKSPACE:
    case KEY_LEFT:
    case KEY_RIGHT:
    case ' ':
    case 'h':
    case 'l':
    case 'H':
    case 'L':       if (ch == 'd')
                        set_line("deletecol [range] ");
                    else if (ch == 'y')
                        set_line("yankcol [range] ");
                    else if (ch == 'Z')
                        set_line("hide [range] ");
                    else
                        return 0;
                    edit_mode();
                    write_line('A');
                    startshow();
                    showrange = SHOWCOLS;
                    showsc = curcol;
                    ungetch(c);
                    return ESC;

    default:        return 0;
    }
    /*NOTREACHED*/
}

static void formatcol(int arg) {
    int c, i;
    int mf = modflg;
    int *oldformat;

    error("Current format is %d %d %d",
          fwidth[curcol], precision[curcol],
          realfmt[curcol]);
    refresh();
    oldformat = scxmalloc(arg * 3 * sizeof(int));
    for (i = 0; i < arg; i++) {
        oldformat[i * 3 + 0] = fwidth[i + curcol];
        oldformat[i * 3 + 1] = precision[i + curcol];
        oldformat[i * 3 + 2] = realfmt[i + curcol];
    }
    c = nmgetch(0);
    //CLEAR_LINE;     // XXX: clear line?
    while (c >= 0 && c != ctl('m') && c != 'q' && c != ESC &&
           c != ctl('g') && linelim < 0) {
        if (c >= '0' && c <= '9') {
            for (i = curcol; i < curcol + arg; i++)
                realfmt[i] = c - '0';
        } else {
            switch (c) {
            case KEY_LEFT:
            case '<':
            case 'h':
                for (i = curcol; i < curcol + arg; i++) {
                    fwidth[i]--;
                    if (fwidth[i] < 1)
                        fwidth[i] = 1;
                }
                rowsinrange = 1;
                colsinrange = fwidth[curcol];
                modflg++;
                break;
            case KEY_RIGHT:
            case '>':
            case 'l':
                for (i = curcol; i < curcol + arg; i++) {
                    fwidth[i]++;
                    if (fwidth[i] > COLS - rescol - 2)
                        fwidth[i] = COLS - rescol - 2;
                }
                rowsinrange = 1;
                colsinrange = fwidth[curcol];
                modflg++;
                break;
            case KEY_DOWN:
            case '-':
            case 'j':
                for (i = curcol; i < curcol + arg; i++) {
                    precision[i]--;
                    if (precision[i] < 0)
                        precision[i] = 0;
                }
                modflg++;
                break;
            case KEY_UP:
            case '+':
            case 'k':
                for (i = curcol; i < curcol + arg; i++)
                    precision[i]++;
                modflg++;
                break;
            case ' ':
                if (arg == 1) {
                    set_line("format [for column] %s ", coltoa(curcol));
                } else {
                    set_line("format [for columns] %s:%s ",
                             coltoa(curcol), coltoa(curcol+arg-1));
                }
                insert_mode();
                error("Current format is %d %d %d",
                      fwidth[curcol], precision[curcol], realfmt[curcol]);
                continue;
            case '=':
                error("Define format type (0-9):");
                refresh();
                c = nmgetch(1);
                if (c >= '0' && c <= '9') {
                    if (colformat[c - '0']) {
                        set_line("format %c = \"%s\"", c, colformat[c - '0']);
                        edit_mode();
                    } else {
                        set_line("format %c = \"", c);
                        insert_mode();
                    }
                } else {
                    error("Invalid format type");
                    c = -1;
                }
                continue;
            case ctl('l'):
                FullUpdate++;
                clearok(stdscr, 1);
                break;
            default:
                break;
            }
        }
        error("Current format is %d %d %d",
              fwidth[curcol], precision[curcol],
              realfmt[curcol]);
        FullUpdate++;
        update(1);
        refresh();
        if (linelim < 0) {
            c = nmgetch(0);
            if (c == ESC || c == ctl('g') || c == 'q') {
                for (i = 0; i < arg; i++) {
                    fwidth[i + curcol] = oldformat[i * 3 + 0];
                    precision[i + curcol] = oldformat[i * 3 + 1];
                    realfmt[i + curcol] = oldformat[i * 3 + 2];
                }
                modflg = mf;
                FullUpdate++;
                update(1);
            }
        }
    }
    scxfree(oldformat);
    if (c >= 0)
        CLEAR_LINE;     // XXX: should get rid of this hack
}

/* called from main() for -P/ option */
void vi_select_range(const char *arg) {
    int c;

    linelim = 0;
    *line = '\0';
    if (mode_ind != 'v')
        write_line(ctl('v'));

    error("Select range:");
    update(1);
    while (!linelim) {
        c = nmgetch(0);
        //CLEAR_LINE;   // XXX: why delay?
        switch (c) {
        case '.':
        case ':':
        case ctl('i'):
            if (!showrange) {
                write_line(c);
                break;
            }
            /* else drop through */
        case ctl('m'):
            set_line("put ");
            write_line('.');
            if (showrange)
                write_line('.');
            strlcat(line, arg, sizeof line);
            linelim = strlen(line);
            break;
        case ESC:
        case ctl('g'):
        case 'q':
            linelim = -1;
            break;
        case ctl('l'):
            FullUpdate++;
            clearok(stdscr, 1);
            break;
        default:
            write_line(c);
            break;
        }
        /* goto switches to insert mode when done, so we
         * have to switch back.
         */
        if (mode_ind == 'i')
            write_line(ctl('v'));
        CLEAR_LINE;
        update(1);
    }
}
