/*      SC      A Spreadsheet Calculator
 *              One line vi emulation
 *
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <sys/wait.h>
#include <signal.h>

#if defined REGCOMP
#include <regex.h>
#elif defined RE_COMP
extern char *re_comp(char *s);
extern char *re_exec(char *s);
#elif defined REGCMP
char *regcmp();
char *regex();
#else
#endif

#include "sc.h"

static inline int iswordchar(char c) { return isalnumchar(c) || c == '_'; }

static void write_line(sheet_t *sp, int c);
static void append_line(void);
static void back_hist(void);
static int back_line(int arg);
static int back_word(int arg, int big_word);
static void back_space(void);
static void change_case(int arg);
static void col_0(void);
static void cr_line(sheet_t *sp, int action);
static void del_in_line(int arg, int back_null);
static void del_to_end(void);
static void ins_string(sheet_t *sp, const char *s);
static void ins_in_line(sheet_t *sp, int c);
static void doabbrev(sheet_t *sp);
static void dogoto(sheet_t *sp);
static void dotab(sheet_t *sp);
static void dotcmd(sheet_t *sp);
static void doshell(sheet_t *sp);
static int find_char(sheet_t *sp, int start, int arg, int dir);
static void find_char2(sheet_t *sp, int arg, int dir);
static void forw_hist(void);
static int forw_line(int arg, int stop_null);
static int forw_word(int arg, int end_word, int big_word, int stop_null);
static int istart;
static void last_col(void);
static void match_paren(void);
static void rep_char(sheet_t *sp);
static void replace_in_line(int c);
static void replace_mode(void);
static void restore_it(void);
static void savedot(int c);
static void save_hist(void);
static void search_again(sc_bool_t reverse);
static void search_hist(void);
static void search_mode(char sind);
static void stop_edit(sheet_t *sp);
static int to_char(sheet_t *sp, int start, int arg, int dir);
static void u_save(int c);
static void yank_cmd(sheet_t *sp, int delete, int change);
static void yank_chars(int first, int last, int delete);
static int get_motion(sheet_t *sp, int change);
static int vigetch(sheet_t *sp);
static void scroll_down(sheet_t *sp);
static void scroll_up(sheet_t *sp, int x);
static void colshow_op(sheet_t *sp);
static void rowshow_op(sheet_t *sp);
static int setmark(sheet_t *sp, int c);  /* convert and set the mark and return index or complain and return -1 */
static int checkmark(int c);  /* convert mark char to index or complain and return -1 */
static void markcell(sheet_t *sp);
static void dotick(sheet_t *sp, int tick);
static int get_rcqual(sheet_t *sp, int ch);
static void formatcol(sheet_t *sp, int arg);
static void edit_mode(void);
static void insert_mode(void);
static void toggle_navigate_mode(void);
static void startshow(sheet_t *sp);
static void showdr(sheet_t *sp);
static void gotobottom(sheet_t *sp);
static void gototop(sheet_t *sp);
static void gohome(sheet_t *sp);
static void leftlimit(sheet_t *sp);
static void rightlimit(sheet_t *sp);
static void list_all(sheet_t *sp);

/* used in update() in screen.c */
char line[FBUFLEN];
int linelim = -1;  /* position in line for writing and parsing */

static int linelen = 0;
static int uarg = 1;        /* universal numeric prefix argument */

static char *completethis = NULL;
static int search_dir;      /* Search direction:  forward = 0; back = 1 */

/* values for mode below */

#define INSERT_MODE     0   /* Insert mode */
#define EDIT_MODE       1   /* Edit mode */
#define REP_MODE        2   /* Replace mode */
#define SEARCH_MODE     3   /* Get arguments for '/' command */
#define NAVIGATE_MODE   4   /* Navigate the spreadsheet while editing a line */

#define DOTLEN          200

static int mode = INSERT_MODE;

static SCXMEM string_t *history[HISTLEN + 1];
static int histp = 0;
static int lasthist = 0;
static int endhist = -1;
static int histsessionstart = 0;
static int histsessionnew = 0;

#if defined REGCOMP
static regex_t preg;
static regex_t *last_search = NULL;
static int errcode;
#elif defined RE_COMP
#elif defined REGCMP
static char *last_search = NULL;  /* allocated with malloc() */
#else
static SCXMEM string_t *last_search = NULL;
#endif
static char undo_line[FBUFLEN];
static int undo_len;
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
static int cellassign;

static void mouse_set_pos(void);
static int mouse_sel_cell(sheet_t *sp);

int set_line(const char *fmt, ...) {
    size_t len;
    va_list ap;
    va_start(ap, fmt);
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    len = ((int (*)(char *, size_t, const char *, va_list))vsnprintf)
        (line, sizeof line, fmt, ap);
    va_end(ap);
    if (len >= sizeof line)
        len = strlen(line);
    return linelim = linelen = len;
}

static void init_line(void) {
    line[0] = '\0';
    linelim = linelen = 0;
}

void vi_interaction(sheet_t *sp) {
    int inloop = 1;
    int c, ch2;
    int narg;
    int edistate = -1;
    int nedistate;
    int running;
    int anychanged = FALSE;
    char *ext;
    struct ent *p;
    buf_t buf;

    sp->modflg = 0;
    if (linelim < 0)
        cellassign = 0;

    uarg = 1;
    while (inloop) {
        running = 1;
        while (running) {
            sp = sht;
            nedistate = -1;
            narg = 1;
            if (edistate < 0 && linelim < 0 && sp->autocalc && (changed || FullUpdate)) {
                EvalAll(sp);
                if (changed)    /* if EvalAll changed or was before */
                    anychanged = TRUE;
                changed = 0;
            } else {            /* any cells change? */
                if (changed)
                    anychanged = TRUE;
                //changed = 0;  // XXX: should clear changed
            }

            update(sp, anychanged);
            anychanged = FALSE;
#ifndef SYSV3   /* HP/Ux 3.1 this may not be wanted */
            screen_refresh(); /* 5.3 does a refresh in getch */
#endif
            c = nmgetch_savepos(1);
            seenerr = 0;
            showneed = 0;   /* reset after each update */
            showexpr = 0;
            shownote = 0;

            if (ISCTL(c) || c == DEL || c == SC_KEY_END || c == SC_KEY_BACKSPACE) {
                switch (c) {
#ifdef SIGTSTP
                case ctl('z'):
                    screen_deraw(1);
                    kill(0, SIGTSTP); /* Nail process group */
                    /* the pc stops here */
                    screen_goraw();
                    break;
#endif
                case ctl('r'):
                    showneed = 1;
                    FALLTHROUGH;
                case ctl('l'):
                    FullUpdate++;
                    screen_rebuild();
                    break;
                case ctl('x'):
                    FullUpdate++;
                    showexpr = 1;
                    screen_rebuild();
                    break;
                case ctl('b'):
                    if (emacs_bindings)
                        backcol(sp, uarg);
                    else
                        backpage(sp, uarg);
                    break;
                case ctl('c'):
                    running = 0;
                    break;

                case SC_KEY_END:
                case ctl('e'):
                    if (linelim < 0 || mode_ind == 'v') {
                        switch (c = nmgetch(1)) {
                        case SC_KEY_UP:
                        case ctl('p'):
                        case 'k':       doend(sp, -1, 0);  break;

                        case SC_KEY_DOWN:
                        case ctl('n'):
                        case 'j':       doend(sp, 1, 0);   break;

                        case SC_KEY_LEFT:
                        case SC_KEY_BACKSPACE:
                        case ctl('h'):
                        case 'h':       doend(sp, 0, -1);  break;

                        case SC_KEY_RIGHT:
                        case ' ':
                        case ctl('i'):
                        case 'l':       doend(sp, 0, 1);   break;

                        case ctl('e'):
                        case ctl('y'):
                            while (c == ctl('e') || c == ctl('y')) {
                                int x = uarg;

                                while (uarg) {
                                    if (c == ctl('e')) {
                                        scroll_down(sp);
                                    } else {
                                        // XXX: Passing x seems incorrect
                                        scroll_up(sp, x);
                                    }
                                    uarg--;
                                }
                                FullUpdate++;
                                update(sp, 0);
                                uarg = 1;
                                c = nmgetch(0);
                            }
                            nmungetch(c);
                            break;

                        case ESC:
                        case ctl('g'):
                            break;

                        default:
                            error("Invalid ^E command");
                            break;
                        }
                    } else {
                        write_line(sp, ctl('e'));
                    }
                    break;

                case ctl('y'):
                    while (c == ctl('e') || c == ctl('y')) {
                        int x = uarg;

                        while (uarg) {
                            if (c == ctl('e')) {
                                scroll_down(sp);
                            } else {
                                // XXX: Passing x seems incorrect
                                scroll_up(sp, x);
                            }
                            uarg--;
                        }
                        FullUpdate++;
                        update(sp, 0);
                        uarg = 1;
                        c = nmgetch(0);
                    }
                    nmungetch(c);
                    break;

                case ctl('f'):
                    if (emacs_bindings)
                        forwcol(sp, uarg);
                    else
                        forwpage(sp, uarg);
                    break;

                case ctl('g'):
                    sp->showrange = 0;
                    linelim = -1;
                    screen_clear_line(1);
                    break;

                case ESC:       /* ctl('[') */
                    write_line(sp, ESC);
                    break;

                case ctl('d'):
                    write_line(sp, ctl('d'));
                    break;

                case SC_KEY_BACKSPACE:
                case DEL:
                case ctl('h'):
                    if (linelim < 0)  /* not editing */
                        backcol(sp, uarg);   /* treat like ^B    */
                    else
                        write_line(sp, ctl('h'));
                    break;

                case ctl('i'):          /* tab */
                    if (linelim < 0)  /* not editing */
                        forwcol(sp, uarg);
                    else
                        write_line(sp, ctl('i'));
                    break;

                case ctl('m'):
                case ctl('j'):
                    write_line(sp, ctl('m'));
                    break;

                case ctl('n'):
                    if (numeric_field) {
                        // XXX: should avoid global variable hacking
                        c = sp->craction;
                        sp->craction = 0;
                        write_line(sp, ctl('m'));
                        sp->craction = c;
                        numeric_field = 0;
                    }
                    if (linelim < 0)
                        forwrow(sp, uarg);
                    else
                        write_line(sp, ctl('n'));
                    break;

                case ctl('p'):
                    if (numeric_field) {
                        // XXX: should avoid global variable hacking
                        c = sp->craction;
                        sp->craction = 0;
                        write_line(sp, ctl('m'));
                        sp->craction = c;
                        numeric_field = 0;
                    }
                    if (linelim < 0)
                        backrow(sp, uarg);
                    else
                        write_line(sp, ctl('p'));
                    break;

                case ctl('q'):
                    if (emacs_bindings) {
                        // XXX: just a test for function keys
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
                    error("Toggle: a:auto,c:cell,e:ext funcs,n:numeric,p:protection,t:top,"
#ifndef NOCRYPT
                          "x:encrypt,"
#endif
                          "$:pre-scale,<MORE>");
                    if (braille) screen_move(1, 0);
                    screen_refresh();
                    switch (nmgetch(1)) {
                    case 'a': case 'A':
                    case 'm': case 'M':
                        sp->autocalc ^= 1;
                        error("Automatic recalculation %s.",
                              sp->autocalc ? "enabled" : "disabled");
                        break;
                    case 'b':
                        braille ^= 1;
                        error("Braille enhancement %s.",
                              braille ? "enabled" : "disabled");
                        --sp->modflg;   /* negate the sp->modflg++ */
                        break;
                    case 'c':
                        repaint_cursor(sp, -showcell);
                        showcell = !showcell;
                        repaint_cursor(sp, showcell);
                        error("Cell highlighting %s.",
                              showcell ? "enabled" : "disabled");
                        --sp->modflg;   /* negate the sp->modflg++ */
                        break;
                    case 'C':
                        sc_setcolor(!color);
                        error("Color %s.", color ? "enabled" : "disabled");
                        break;
                    case 'e':
                        sp->extfunc = !sp->extfunc;
                        error("External functions %s.",
                              sp->extfunc? "enabled" : "disabled");
                        break;
                    case 'E':
                        sp->colorerr = !sp->colorerr;
                        error("Color changing of cells with errors %s.",
                              sp->colorerr ? "enabled" : "disabled");
                        break;
                    case 'i': case 'I':
                        sp->autoinsert = !sp->autoinsert;
                        error("Autoinsert %s.",
                              sp->autoinsert? "enabled" : "disabled");
                        break;
                    case 'l': case 'L':
                        autolabel = !autolabel;
                        error("Autolabel %s.",
                              autolabel ? "enabled" : "disabled");
                        break;
                    case 'p':
                        sp->protect = !sp->protect;
                        error("Protect mode %s.",
                              sp->protect ? "enabled" : "disabled");
                        break;
                    case 'n':
                        sp->numeric = !sp->numeric;
                        error("Numeric input %s.",
                              sp->numeric ? "enabled" : "disabled");
                        break;
                    case 'N':
                        sp->colorneg = !sp->colorneg;
                        error("Color changing of negative numbers %s.",
                              sp->colorneg ? "enabled" : "disabled");
                        break;
                    case 'o': case 'O':
                        sp->optimize ^= 1;
                        error("%s expressions upon entry.",
                              sp->optimize ? "Optimize" : "Do not optimize");
                        break;
                    case 'r': case 'R':
                        error("Which direction after return key?");
                        switch (nmgetch(1)) {
                        case ctl('m'):
                            sp->craction = 0;
                            error("No action after new line");
                            break;
                        case 'j':
                        case ctl('n'):
                        case SC_KEY_DOWN:
                            sp->craction = CRROWS;
                            error("Down row after new line");
                            break;
                        case 'l':
                        case ' ':
                        case SC_KEY_RIGHT:
                            sp->craction = CRCOLS;
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
                        sp->cslop ^= 1;
                        error("Color slop %s.",
                              sp->cslop ? "enabled" : "disabled");
                        break;
                    case 't': case 'T':
                        sp->showtop = !sp->showtop;
                        error("Top line %s.",
                              sp->showtop ? "enabled" : "disabled");
                        break;
                    case 'v':
                        emacs_bindings = !emacs_bindings;
                        error("Emacs %s.",
                              emacs_bindings ? "enabled" : "disabled");
                        break;
                    case 'w': case 'W':
                        sp->autowrap = !sp->autowrap;
                        error("Autowrap %s.",
                              sp->autowrap? "enabled" : "disabled");
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
                        sp->rowlimit = sp->currow;
                        sp->collimit = sp->curcol;
                        error("Row and column limits set");
                        break;
                    case '$':
                        if (sp->prescale == 1.0) {
                            error("Prescale enabled.");
                            sp->prescale = 0.01;
                        } else {
                            sp->prescale = 1.0;
                            error("Prescale disabled.");
                        }
                        break;
                    case ESC:
                    case ctl('g'):
                        --sp->modflg;   /* negate the sp->modflg++ */
                        break;
                    default:
                        error("Invalid toggle command");
                        --sp->modflg;   /* negate the sp->modflg++ */
                    }
                    FullUpdate++;
                    sp->modflg++;
                    break;

                case ctl('u'):
                    narg = uarg * 4;
                    nedistate = 1;
                    break;

                case ctl('v'):  /* switch to navigate mode, or if already *
                                 * in navigate mode, insert variable name */
                    if (linelim >= 0)
                        write_line(sp, ctl('v'));
                    else
                    if (emacs_bindings)
                        forwpage(sp, uarg);
                    break;

                case ctl('w'):  /* insert variable expression */
                    if (linelim >= 0) {
                        buf_init2(buf, line, sizeof line, linelen);
                        p = getcell(sp, sp->currow, sp->curcol);
                        /* decompile expression into line array */
                        // XXX: insert expression instead of appending?
                        // XXX: should pass sp->currow, sp->curcol as the cell reference
                        if (p && p->expr)
                            decompile_expr(sp, buf, p->expr, 0, 0, DCP_DEFAULT);
                        linelim = linelen = buf->len;
                    }
                    break;

                case ctl('a'):
                    if (emacs_bindings) {
                        // XXX: goto beginning of row.
                        //      repeated: goto A0
                        break;
                    }
                    if (linelim >= 0) {
                        write_line(sp, c);
                    } else {
                        remember(sp, 0);
                        sp->currow = 0;
                        sp->curcol = 0;
                        remember(sp, 1);
                        FullUpdate++;
                    }
                    break;
                case '\035':    /* ^] */
                    if (linelim >= 0)
                        write_line(sp, c);
                    break;
                default:
                    error("No such command (^%c)", c + 0100);
                    break;
                } /* End of the control char switch stmt */
            } else
            if (ISBYTE(c) && isdigit(c) &&
                ((!sp->numeric && linelim < 0) ||
                 (linelim >= 0 && (mode_ind == 'e' || mode_ind == 'v')) ||
                 edistate >= 0))
            {
                /* we got a leading number */
                if (edistate != 0) {
                    /* First char of the count */
                    if (c == '0') {    /* just a '0' goes to left col */
                        if (linelim >= 0)
                            write_line(sp, c);
                        else
                            leftlimit(sp);
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
            if (c == SC_KEY_F(1) && sempty(sp->fkey[c - SC_KEY_F0])) {
                screen_deraw(1);
                system("man sc");
                screen_goraw();
                screen_erase();
            } else
            if (linelim >= 0) {
                /* Editing line */
                switch (c) {
                case ')':
                case ',':
                    if (sp->showrange)
                        showdr(sp);
                    break;
                default:
                    break;
                }
                write_line(sp, c);
            } else
            if (c >= SC_KEY_F0 && c <= SC_KEY_F(FKEYS-1)) {
                /* a function key was pressed */
                if (!sempty(sp->fkey[c - SC_KEY_F0])) {
                    int i;

                    pstrcpy(line, sizeof line, s2c(sp->fkey[c - SC_KEY_F0]));
                    for (i = 0; line[i]; i++) {
                        // XXX: string should have been unescaped already
                        if (line[i] == '\\' && line[i+1] == '"') {
                            strsplice(line, sizeof line, i, 1, NULL, 0);
                            /* i++ will skip the '"' */
                        } else
                        if (line[i] == '$' && line[i+1] == '$') {
                            const char *s = cell_addr(sp, cellref_current(sp));
                            size_t len = strlen(s);
                            strsplice(line, sizeof line, i, 2, s, len);
                            /* i++ will skip the replacement string */
                            i += len - 1;
                        }
                    }
                    linelen = i;
                    linelim = 0;
                    insert_mode();
                    write_line(sp, ctl('m'));
                }
            } else {
                /* switch on a normal command character */
                switch (c) {
                case '/':
                    if (linelim >= 0)
                        write_line(sp, c);
                    else
                        lotus_menu();
                    break;
                case ':':
                    if (linelim >= 0)
                        write_line(sp, c);
                    break;      /* Be nice to vi users */

                case '@':
                    EvalAll(sp);
                    changed = 0;    // XXX: questionable
                    anychanged = TRUE;
                    break;

                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                case '.':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        set_line("let %s = %c", cell_addr(sp, cellref_current(sp)), c);
                        setmark(sp, '0');
                        numeric_field = 1;
                        cellassign = 1;
                        insert_mode();
                    }
                    break;

                case '+':
                case '-':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        p = getcell(sp, sp->currow, sp->curcol);
                        if (!sp->numeric && p && p->type == SC_NUMBER) {
                            /* increment/decrement numeric cell by uarg */
                            if (c == '+')
                                p->v += (double)uarg;
                            else
                                p->v -= (double)uarg;
                            FullUpdate++;
                            sp->modflg++;
                            continue;
                        }
                        /* copy cell contents into line array */
                        buf_init(buf, line, sizeof line);
                        // XXX: the conversion should be localized
                        linelim = linelen = edit_cell(sp, buf, sp->currow, sp->curcol,
                                                      p, 0, 0, DCP_DEFAULT, 0);
                        setmark(sp, '0');
                        numeric_field = 1;
                        cellassign = 1;
                        insert_mode();
                        if (c == '-' || (p && p->flags == SC_NUMBER) || (p && p->expr))
                            write_line(sp, c);
                        else
                            write_line(sp, ctl('v'));
                    }
                    break;

                case '=':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        set_line("let %s = ", cell_addr(sp, cellref_current(sp)));
                        setmark(sp, '0');
                        cellassign = 1;
                        insert_mode();
                    }
                    break;

                case '!':
                    doshell(sp);
                    break;

                /*
                 * Range commands:
                 */

                case 'r':
                    error("Range: x:erase v:value c:copy f:fill d:def l:lock U:unlock S:show u:undef F:fmt");
                    if (braille) screen_move(1, 0);
                    screen_refresh();
                    switch (c = nmgetch(1)) {
                    case 'l':
                        set_line("lock [range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'U':
                        set_line("unlock [range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'c':
                        set_line("copy [dest_range src_range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'm':
                        set_line("move [destination src_range] %s ", cell_addr(sp, cellref_current(sp)));
                        insert_mode();
                        write_line(sp, ctl('v'));
                        break;
                    case 'x':
                        set_line("erase [range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'y':
                        set_line("yank [range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'v':
                        set_line("value [range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'f':
                        set_line("fill [range start inc] ");
                        insert_mode();
                        startshow(sp);
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
                        if (braille) screen_move(1, 0);
                        screen_refresh();
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
                            startshow(sp);
                            break;
                        case 'u':
                            set_line("unframe [<range>] ");
                            insert_mode();
                            startshow(sp);
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
                        startshow(sp);
                        break;
                    case 'C':
                        set_line("color [range color#] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case 'S':
                        list_all(sp);
                        break;
                    case 'F':
                        set_line("fmt [range \"format\"] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case '{':
                        set_line("leftjustify [range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    case '}':
                        set_line("rightjustify [range] ");
                        cellassign = 1;
                        insert_mode();
                        startshow(sp);
                        break;
                    case '|':
                        set_line("center [range] ");
                        cellassign = 1;
                        insert_mode();
                        startshow(sp);
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
                    } else {
                        select_register(c);
                    }
                    break;

                    /*
                     * Row/column commands:
                     */

                case SC_KEY_IC:
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
                        if (!(ch2 = get_rcqual(sp, c))) {
                            error("Invalid row/column command");
                            break;
                        }

                        if (ch2 == ESC || ch2 == ctl('g'))
                            break;

                        switch (c) {
                        case 'i':
                            if (ch2 == 'r') insert_rows(sp, cellref_current(sp), uarg, 0);
                            else            insert_cols(sp, cellref_current(sp), uarg, 0);
                            break;

                        case 'o':
                            if (ch2 == 'r') sp->currow += insert_rows(sp, cellref_current(sp), uarg, 1);
                            else            sp->curcol += insert_cols(sp, cellref_current(sp), uarg, 1);
                            break;

                        case 'a':
                            if (ch2 == 'r') while (uarg --> 0 && dup_row(sp, cellref_current(sp)))
                                                sp->currow++;
                            else            while (uarg --> 0 && dup_col(sp, cellref_current(sp)))
                                                sp->curcol++;
                            break;

                        case 'd':
                            if (ch2 == 'r') delete_rows(sp, sp->currow, sp->currow + uarg - 1);
                            else            delete_cols(sp, sp->curcol, sp->curcol + uarg - 1);
                            break;

                        case 'y':
                            if (ch2 == 'r') yank_rows(sp, sp->currow, sp->currow + uarg - 1);
                            else            yank_cols(sp, sp->curcol, sp->curcol + uarg - 1);
                            break;

                        case 'p':
                            if (ch2 == '.') {
                                // XXX: should handle uarg
                                set_line("pullcopy [range] ");
                                insert_mode();
                                startshow(sp);
                                break;
                            }
                            cmd_pullcells(sp, ch2, uarg);
                            break;

                            /*
                             * turn an area starting at sp->currow/sp->curcol into
                             * constants vs expressions - non reversible
                             */
                        case 'v':
                            // XXX: 'v.' should get a range for the "value" cmd
                            if (ch2 == 'r') {
                                int c1 = 0, c2 = sp->maxcol;
                                struct frange *fr;
                                if ((fr = frange_get_current(sp))) {
                                    c1 = fr->orr.left.col;
                                    c2 = fr->orr.right.col;
                                }
                                valueize_area(sp, rangeref(sp->currow, c1, sp->currow + uarg - 1, c2));
                            } else {
                                valueize_area(sp, rangeref(0, sp->curcol, sp->maxrow, sp->curcol + uarg - 1));
                            }
                            break;

                        case 'Z':
                            switch (ch2) {
                            case 'r':   hiderows(sp, sp->currow, sp->currow + uarg - 1); break;
                            case 'c':   hidecols(sp, sp->curcol, sp->curcol + uarg - 1); break;
                            case 'Z':   if (sp->modflg && sp->curfile[0]) {
                                            writefile(sp, sp->curfile, rangeref_total(sp), DCP_DEFAULT);
                                            running = 0;
                                        } else if (sp->modflg) {
                                            error("No file name.");
                                        } else
                                            running = 0;
                                        break;
                            }
                            break;

                        case 's':
                            /* special case; no repeat count */
                            if (ch2 == 'r') rowshow_op(sp);
                            else            colshow_op(sp);
                            break;
                        }
                        break;
                    }

                case '$':
                    rightlimit(sp);
                    break;
                case '#':
                    gotobottom(sp);
                    break;
                case 'w':
                    forwcell(sp, uarg);
                    break;
                case 'b':
                    backcell(sp, uarg);
                    break;
                case '^':
                    gototop(sp);
                    break;
                case SC_KEY_HELP:
                case '?':
                    help(HELP_INTRO);
                    break;
                case SC_ALT('?'):
                    error("count=%zu  requested=%zu  allocated=%zu  overhead=%zu",
                          scxmem_count, scxmem_requested, scxmem_allocated, scxmem_overhead);
                    //error("maxrow=%d  maxcol=%d  maxrows=%d  maxcols=%d  rescol=%d",
                    //      sp->maxrow, sp->maxcol, sp->maxrows, sp->maxcols, rescol);
                    break;
                case '\\':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        set_line("label %s = \"\\", cell_addr(sp, cellref_current(sp)));
                        setmark(sp, '0');
                        cellassign = 1;
                        insert_mode();
                    }
                    break;

                case '<':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        set_line("leftstring %s = \"", cell_addr(sp, cellref_current(sp)));
                        setmark(sp, '0');
                        cellassign = 1;
                        insert_mode();
                    }
                    break;

                case '>':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        set_line("rightstring %s = \"", cell_addr(sp, cellref_current(sp)));
                        setmark(sp, '0');
                        cellassign = 1;
                        insert_mode();
                    }
                    break;
                case '{':
                    range_align(sp, rangeref_current(sp), ALIGN_LEFT);
                    break;
                case '}':
                    range_align(sp, rangeref_current(sp), ALIGN_RIGHT);
                    break;
                case '|':
                    range_align(sp, rangeref_current(sp), ALIGN_CENTER);
                    break;
                case 'e':
                case 'E':
                    if (!locked_cell(sp, sp->currow, sp->curcol)) {
                        buf_init(buf, line, sizeof line);
                        p = getcell(sp, sp->currow, sp->curcol);
                        /* copy cell contents into line array */
                        // XXX: the conversion should be localized
                        linelim = linelen = edit_cell(sp, buf, sp->currow, sp->curcol,
                                                      p, 0, 0, DCP_DEFAULT, '"');
                        setmark(sp, '0');
                        cellassign = 1;
                        if (c == 'e' && (p->type != SC_NUMBER)) {
                            insert_mode();
                        } else {
                            edit_mode();
                        }
                    }
                    break;
                case 'f':
                    formatcol(sp, uarg);
                    break;
                case 'F':
                    p = getcell(sp, sp->currow, sp->curcol);
                    if (p && p->format) {
                        buf_init(buf, line, sizeof line);
                        buf_setf(buf, "fmt [format] %s \"", cell_addr(sp, cellref_current(sp)));
                        buf_quotestr(buf, 0, s2c(p->format), 0);
                        linelim = linelen = buf->len;
                        edit_mode();
                    } else {
                        set_line("fmt [format] %s \"", cell_addr(sp, cellref_current(sp)));
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
                        /* copy color expression into line array */
                        // XXX: should pass -1, -1 as the cell reference?
                        buf_init2(buf, line, sizeof line, linelen);
                        linelim = linelen = decompile_expr(sp, buf, cpairs[c]->expr, 0, 0, DCP_DEFAULT);
                        edit_mode();
                    } else {
                        insert_mode();
                    }
                    break;
                }
                case SC_KEY_FIND:
                case 'g':
                    set_line("goto [v] ");
                    insert_mode();
                    break;
                case 'n':
                    go_last(sp);
                    break;
                case 'P':
                    set_line("put [\"dest\" range] \"");
                    if (sp->curfile[0]) {
                        ext = get_extension(sp->curfile);
                        /* keep the extension unless .sc or scext */
                        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, s2c(scext))))
                            ext += strlen(ext);
                        error("Default path is \"%.*s.%s\"",
                              (int)(ext - sp->curfile), sp->curfile,
                              scext ? s2c(scext) : "sc");
                    }
                    insert_mode();
                    break;
                case 'M':
                    set_line("merge [\"source\"] \"");
                    insert_mode();
                    break;
                case 'R':
                    if (!sempty(sp->mdir))
                        set_line("merge [\"macro_file\"] \"%s", s2c(sp->mdir));
                    else
                        set_line("merge [\"macro_file\"] \"");
                    insert_mode();
                    break;
                case 'D':
                    set_line("mdir [\"macro_directory\"] \"");
                    insert_mode();
                    break;
                case 'A':
                    if (!sempty(sp->autorun))
                        set_line("autorun [\"macro_file\"] \"%s", s2c(sp->autorun));
                    else
                        set_line("autorun [\"macro_file\"] \"");
                    insert_mode();
                    break;
                case 'G':
                    set_line("get [\"source\"] \"");
                    if (sp->curfile[0])
                        error("Default file is \"%s\"", sp->curfile);
                    insert_mode();
                    break;
                case 'W':
                    set_line("write [\"dest\" range] \"");
                    if (sp->curfile[0]) {
                        ext = get_extension(sp->curfile);
                        /* keep the extension unless .sc or scext */
                        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, s2c(scext))))
                            ext += strlen(ext);
                        error("Default file is \"%.*s.%s\"",
                              (int)(ext - sp->curfile), sp->curfile,
                              ascext ? s2c(ascext) : "asc");
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
                    if (sp->curfile[0]) {
                        ext = get_extension(sp->curfile);
                        /* keep the extension unless .sc or scext */
                        if (strcmp(ext, ".sc") && !(scext && !strcmp(ext, s2c(scext))))
                            ext += strlen(ext);
                        if (sp->tbl_style == 0) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - sp->curfile), sp->curfile,
                                  tbl0ext ? s2c(tbl0ext) : "cln");
                        } else if (sp->tbl_style == TBL) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - sp->curfile), sp->curfile,
                                  tblext ? s2c(tblext) : "tbl");
                        } else if (sp->tbl_style == LATEX) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - sp->curfile), sp->curfile,
                                  latexext ? s2c(latexext) : "lat");
                        } else if (sp->tbl_style == SLATEX) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - sp->curfile), sp->curfile,
                                  slatexext ? s2c(slatexext) : "stx");
                        } else if (sp->tbl_style == TEX) {
                            error("Default file is \"%.*s.%s\"",
                                  (int)(ext - sp->curfile), sp->curfile,
                                  texext ? s2c(texext) : "tex");
                        }
                    }
                    insert_mode();
                    break;
                case SC_KEY_DC:
                case 'x':
                    if (sp->calc_order == BYROWS) {
                        erase_range(sp, rangeref(sp->currow, sp->curcol, sp->currow, sp->curcol + uarg - 1));
                    } else {
                        erase_range(sp, rangeref(sp->currow, sp->curcol, sp->currow + uarg - 1, sp->curcol));
                    }
                    break;
                case 'Q':
                case 'q':
                    running = 0;
                    break;
                case SC_KEY_LEFT:
                case 'h':
                    backcol(sp, uarg);
                    break;
                case SC_KEY_DOWN:
                case 'j':
                    forwrow(sp, uarg);
                    break;
                case SC_KEY_UP:
                case 'k':
                    backrow(sp, uarg);
                    break;
                case 'H':
                    backcol(sp, sp->curcol - sp->stcol + 2);
                    break;
                case SC_KEY_NPAGE:                 /* next page */
                case 'J':
                    forwpage(sp, uarg);
                    break;
                case SC_KEY_PPAGE:                 /* previous page */
                case SC_ALT('v'):
                case 'K':
                    backpage(sp, uarg);
                    break;
                case SC_KEY_HOME:
                    gohome(sp);
                    break;
                case 'L':
                    forwcol(sp, lcols - (sp->curcol - sp->stcol) + 1);
                    break;
                case SC_KEY_RIGHT:
                case ' ':
                case 'l':
                    forwcol(sp, uarg);
                    break;
                case 'm':
                    markcell(sp);
                    break;
                case 'c':
                    error("Copy marked cell:");
                    c = nmgetch(1);
                    if (c == ESC || c == ctl('g')) {
                        break;
                    }
                    if (c == '.') {
                        // XXX: horrible hack to set the copy()'s internal
                        // static variables for the default source range
                        // should get rid of this
                        copy_set_source_range(rangeref_current(sp));
                        set_line("copy [dest_range src_range] ");
                        insert_mode();
                        startshow(sp);
                        break;
                    }
                    if ((c = checkmark(c)) < 0)
                        break;
                    if (sp->savedcr[c].row == -1) {
                        error("Mark not set");
                        break;
                    }
                    copy_range(sp, COPY_FROM_RANGE,
                               rangeref(sp->currow, sp->curcol, sp->currow, sp->curcol + uarg - 1),
                               rangeref2(sp->savedcr[c], sp->savedcr[c]));
                    break;
                case '`':
                case '\'':
                    dotick(sp, c);
                    break;
                case '*':
                    error("Note: Add/Delete/Show/*(go to note)?");
                    c = nmgetch(1);
                    if (c == ESC || c == ctl('g')) {
                        break;
                    }
                    if (c == 'a' || c == 'A') {
                        set_line("addnote [target range] %s ", cell_addr(sp, cellref_current(sp)));
                        // XXX: should append note string if present
                        insert_mode();
                        write_line(sp, ctl('v'));
                        FullUpdate++;
                        break;
                    }
                    if (c == 'd' || c == 'D') {
                        p = getcell(sp, sp->currow, sp->curcol);
                        if (p && (p->flags & HAS_NOTE)) {
                            p->flags ^= HAS_NOTE;
                            p->flags |= IS_CHANGED;
                            sp->modflg++;
                            FullUpdate++;
                        }
                        break;
                    }
                    if (c == 's' || c == 'S') {
                        FullUpdate++;
                        shownote = 1;
                        screen_rebuild();
                        error("Highlighted cells have attached notes.");
                        break;
                    }
                    if (c == '*') {
                        gotonote(sp);
                        break;
                    }
                    error("Invalid command");
                    break;
                case 'z':
                    switch (c = nmgetch(1)) {
                    case ctl('m'):
                        sp->strow = sp->currow;
                        FullUpdate++;
                        screen_rebuild();
                        break;
                    case '.':
                        sp->strow = -1;
                        FullUpdate++;
                        screen_rebuild();
                        break;
                    case '|':
                        sp->stcol = -1;
                        FullUpdate++;
                        screen_rebuild();
                        break;
                    case 'c':
                        /* Force centering of current cell (or range, if
                         * we've just jumped to a new range with the goto
                         * command).
                         */
                        sp->strow = -1;
                        sp->stcol = -1;
                        FullUpdate++;
                        screen_rebuild();
                        break;
                    default:
                        break;
                    }
                    break;
                case SC_KEY_RESIZE:
#ifndef SIGWINCH
                    winchg();
#endif
                    break;
                case SC_KEY_MOUSE:
                    mouse_sel_cell(sp);
                    break;
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
        inloop = modcheck(sp, " before exiting");
    }                           /*  while (inloop) */
}

static void scroll_down(sheet_t *sp) {
    sp->strow++;
    // XXX: check maximum row?
    while (row_hidden(sp, sp->strow))
        sp->strow++;
    if (sp->currow < sp->strow)
        sp->currow = sp->strow;
}

static void scroll_up(sheet_t *sp, int x) {
    if (sp->strow) {
        sp->strow--;
        while (sp->strow && row_hidden(sp, sp->strow))
            sp->strow--;
    }
    forwrow(sp, x);
    if (sp->currow >= lastendrow)
        backrow(sp, 1);
    backrow(sp, x);
}

static void colshow_op(sheet_t *sp) {
    int i, j;
    for (i = 0; i < sp->maxcols; i++) {
        if (col_hidden(sp, i))
            break;
    }
    for (j = i; j < sp->maxcols; j++) {
        if (!col_hidden(sp, j))
            break;
    }
    j--;
    if (i >= sp->maxcols) {
        error("No hidden columns to show");
    } else {
        set_line("show %s:%s", coltoa(i), coltoa(j));
    }
}

static void rowshow_op(sheet_t *sp) {
    int i, j;
    for (i = 0; i < sp->maxrows; i++) {
        if (row_hidden(sp, i))
            break;
    }
    for (j = i; j < sp->maxrows; j++) {
        if (!row_hidden(sp, j))
            break;
    }
    j--;
    if (i >= sp->maxrows) {
        error("No hidden rows to show");
    } else {
        set_line("show %d:%d", i, j);
    }
}

/* convert mark char to index or complain and return -1 */
static int checkmark(int c) {
    if (c == '`' || c == '\'')
        return 0;
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 1;
    if (c >= '0' && c <= '9')
        return c - '0' + 1 + 26;
    error("Invalid mark %c (must be letter, digit, ` or ')", c);
    return -1;
}

static int setmark(sheet_t *sp, int c) {
    int n = checkmark(c);
    if (n >= 0) {
        sp->savedcr[n] = cellref(sp->currow, sp->curcol);
        sp->savedst[n] = cellref(sp->strow, sp->stcol);
    }
    return n;
}

static void markcell(sheet_t *sp) {
    int c;

    error("Mark cell:");
    c = nmgetch(1);
    if (c == ESC || c == ctl('g'))
        return;
    setmark(sp, c);
}

static void dotick(sheet_t *sp, int tick) {
    int c;

    remember(sp, 0);

    error("Go to marked cell:");
    c = nmgetch(1);
    if (c == ESC || c == ctl('g')) {
        return;
    }
    if ((c = checkmark(c)) < 0)
        return;

    if (sp->savedcr[c].row == -1) {
        error("Mark not set");
        return;
    }
    sp->currow = sp->savedcr[c].row;
    sp->curcol = sp->savedcr[c].col;
    if (tick == '\'') {
        sp->strow = sp->savedst[c].row;
        sp->stcol = sp->savedst[c].col;
        gs.stflag = 1;
    } else {
        gs.stflag = 0;
    }
    remember(sp, 1);

    FullUpdate++;
}

static int prev_line_char(int n) {
    return (linelim >= n) ? line[linelim - n] : 0;
}

static void check_navigate(sheet_t *sp) {
    int c;
    if (linelim == linelen && ((c = prev_line_char(1)) == '+' || c == '-')) {
        toggle_navigate_mode();
    } else {
        cr_line(sp, 0);
    }
}

/* edit the line buffer vi-style */
static void write_line(sheet_t *sp, int c) {
    struct frange *fr;
    struct crange *cr;
    struct ent *p;
    int c1;

    screen_clear_line(1);  // XXX: possibly redundant
    if (c != ctl('i'))
        completethis = NULL;
    if (mode == EDIT_MODE) {
        nosavedot = 0;
        switch (c) {
        case SC_KEY_BACKSPACE:
        case ctl('h'):  linelim = back_line(uarg);                      break;
        case ctl('i'):  dotab(sp);                                      break;
        case ctl('m'):  if (search_ind == ' ')
                            cr_line(sp, sp->craction);
                        else
                            search_hist();
                        break;
        case 'v':
        case ctl('v'):  toggle_navigate_mode();                         break;
        case ESC:       stop_edit(sp);                                  break;
        case '+':       forw_hist();                                    break;
        case '-':       back_hist();                                    break;
        case SC_KEY_END:
        case ctl('e'):
        case '$':       last_col();                                     break;
        case '.':       dotcmd(sp);                                     break;
        case '!':       doshell(sp);                                    break;
        case ';':       find_char2(sp, uarg, finddir);                  break;
        case ',':       find_char2(sp, uarg, -finddir);                 break;
        case '~':       u_save(c); change_case(uarg);                   break;
        case '%':       match_paren();                                  break;
        case SC_KEY_FIND:
        case '?':
        case '/':       search_mode(c);                                 break;
        case SC_KEY_HOME:
        case ctl('a'):
        case '0':       col_0();                                        break;
        case 'G':       if (histp > 0) histp = lasthist; forw_hist();   break;
        case 'R':       u_save(c); replace_mode();                      break;
        case 'a':       u_save(c); append_line();                       break;
        case 'A':       u_save(c); last_col(); append_line();           break;
        case 'b':       linelim = back_word(uarg, 0);                   break;
        case 'B':       linelim = back_word(uarg, 1);                   break;
        case 'c':       u_save(c); yank_cmd(sp, 1, 1); insert_mode();   break;
        case 'C':       u_save(c); del_to_end(); append_line();         break;
        case 'd':       u_save(c); yank_cmd(sp, 1, 0);                  break;
        case 'D':       u_save(c); del_to_end();                        break;
        case 'e':       linelim = forw_word(uarg, 1, 0, 0);             break;
        case 'E':       linelim = forw_word(uarg, 1, 1, 0);             break;
        case 'f':       linelim = find_char(sp, linelim, uarg, 1);      break;
        case 'F':       linelim = find_char(sp, linelim, uarg, -1);     break;
        case SC_KEY_LEFT:
        case ctl('b'):
        case 'h':       linelim = back_line(uarg);                      break;
        case SC_KEY_IC:
        case 'i':       u_save(c); insert_mode();                       break;
        case 'I':       u_save(c); col_0(); insert_mode();              break;
        case SC_KEY_DOWN:
        case 'j':       forw_hist();                                    break;
        case SC_KEY_UP:
        case 'k':       back_hist();                                    break;
        case SC_KEY_RIGHT:
        case ctl('f'):
        case ' ':
        case 'l':       linelim = forw_line(uarg, 0);                   break;
        case 'n':       search_again(FALSE);                            break;
        case 'N':       search_again(TRUE);                             break;
        case 'p':       u_save(c);
                        linelim = forw_line(1, 1);
                        ins_string(sp, putbuf);
                        linelim = back_line(1);                         break;
        case 'P':       u_save(c);
                        ins_string(sp, putbuf);
                        linelim = back_line(1);                         break;
        case 'q':       stop_edit(sp);                                  break;
        case 'r':       u_save(c); rep_char(sp);                        break;
        case 's':       u_save(c); del_in_line(uarg, 0); insert_mode(); break;
        case 't':       linelim = to_char(sp, linelim, uarg, 1);        break;
        case 'T':       linelim = to_char(sp, linelim, uarg, -1);       break;
        case 'u':       restore_it();                                   break;
        case 'w':       linelim = forw_word(uarg, 0, 0, 0);             break;
        case 'W':       linelim = forw_word(uarg, 0, 1, 0);             break;
        case SC_KEY_DC:
        case 'x':       u_save(c); del_in_line(uarg, 1);                break;
        case 'X':       u_save(c); back_space();                        break;
        case 'y':       yank_cmd(sp, 0, 0);                             break;
        case 'Y':       yank_chars(linelim, linelen, 0);                break;
        case SC_KEY_MOUSE:  mouse_set_pos();                            break;
        default:        break;
        }
    } else if (mode == INSERT_MODE) {
        if (c == ctl('m'))
            savedot(ESC);
        else
            savedot(c);
        switch (c) {
        case SC_KEY_BACKSPACE:
        case ctl('h'):      back_space();                           break;
        case ctl('i'):      dotab(sp);                              break;
        case ctl('m'):      cr_line(sp, sp->craction);                  break;
        case ctl('v'):      toggle_navigate_mode();                 break;
        case SC_KEY_LEFT:
        case ctl('b'):      if (numeric_field) {
                                check_navigate(sp);
                                backcol(sp, 1);
                            } else {
                                istart = linelim = back_line(uarg);
                            }   break;
        case SC_KEY_RIGHT:
        case ctl('f'):      if (numeric_field) {
                                check_navigate(sp);
                                forwcol(sp, 1);
                            } else {
                                istart = linelim = forw_line(uarg, 1);
                            }   break;
        case SC_KEY_DOWN:
        case ctl('n'):      if (numeric_field) {
                                check_navigate(sp);
                                forwrow(sp, 1);
                            } else {
                                forw_hist();
                                istart = linelim;
                            }   break;
        case SC_KEY_UP:
        case ctl('p'):      if (numeric_field) {
                                check_navigate(sp);
                                backrow(sp, 1);
                            } else {
                                back_hist();
                                istart = linelim;
                            }   break;
        case SC_KEY_HOME:
        case ctl('a'):      col_0();                                break;
        case SC_KEY_END:
        case ctl('e'):      last_col();                             break;
        case ESC:           ins_in_line(sp, 0);
                            edit_mode();                            break;
        /* '\035' is ^], which expands abbreviations without inserting another
         * character in the line
         */
        case '\035':        ins_in_line(sp, 0);                     break;
        case SC_KEY_MOUSE:  mouse_set_pos();                        break;
        default:            ins_in_line(sp, c);                     break;
        }
    } else if (mode == SEARCH_MODE) {
        switch (c) {
        case SC_KEY_BACKSPACE:
        case ctl('h'):      back_space();                           break;
        case ctl('m'):      search_hist();                          break;
        case ESC:           ins_in_line(sp, 0);
                            edit_mode();                            break;
        /* '\035' is ^], which expands abbreviations without inserting another
         * character in the line
         */
        case '\035':        ins_in_line(sp, 0);                     break;
        default:            ins_in_line(sp, c);                     break;
        }
    } else if (mode == REP_MODE) {
        savedot(c);
        switch (c) {
        case SC_KEY_BACKSPACE:
        case ctl('h'):      if (linelim >= 0 && linelim > undo_len) {
                                back_space();
                            } else {
                                linelim = back_line(1);
                                line[linelim] = undo_line[linelim];
                            }                                       break;
        case ctl('m'):      cr_line(sp, sp->craction);                  break;
        case ESC:           edit_mode();                            break;
        default:            replace_in_line(c);                     break;
        }
    } else if (mode == NAVIGATE_MODE) {
        switch (c) {
        case '.':
        case ':':
        case ctl('i'):      if (!sp->showrange) {
                                toggle_navigate_mode();
                                startshow(sp);
                            } else
                            if (linelim == linelen
                            &&  ((c1 = prev_line_char(1)) == '+' || c1 == '-' ||
                                 (c1 == ' ' && prev_line_char(2) == '=')))
                            {
                                ins_string(sp, "@sum(");
                                showdr(sp);
                                ins_in_line(sp, ')');
                            } else {
                                showdr(sp);
                                ins_in_line(sp, ' ');
                            }                                       break;
        case ' ':           if (sp->showrange) {
                                showdr(sp);
                                ins_in_line(sp, ' ');
                                toggle_navigate_mode();
                            } else {
                                forwcol(sp, uarg);
                            }                                       break;
        case '+':
        case '-':           if (!sp->showrange) {
                                ins_string(sp, cell_addr(sp, cellref_current(sp)));
                                ins_in_line(sp, c);
                            } else
                            if (linelim == linelen
                            &&  ((c1 = prev_line_char(1)) == '+' || c1 == '-' ||
                                 (c1 == ' ' && prev_line_char(2) == '=')))
                            {
                                ins_string(sp, "@sum(");
                                showdr(sp);
                                ins_in_line(sp, ')');
                                ins_in_line(sp, c);
                                toggle_navigate_mode();
                            } else {
                                showdr(sp);
                                ins_in_line(sp, ')');
                                ins_in_line(sp, c);
                            }                                       break;
        case ctl('m'):      if (!sp->showrange) {
                                ins_string(sp, cell_addr(sp, cellref_current(sp)));
                                toggle_navigate_mode();
                            } else {
                                toggle_navigate_mode();
                                cr_line(sp, sp->craction);
                            }                                       break;
        case 'v':           {   /* insert variable value */
                                char temp[100];

                                p = getcell(sp, sp->currow, sp->curcol);
                                if (p && (p->flags == SC_NUMBER)) {
                                    snprintf(temp, sizeof temp, "%.*f",
                                             sp->colfmt[sp->curcol].precision, p->v);
                                    ins_string(sp, temp);
                                    toggle_navigate_mode();
                                }
                            }                                       break;
        case 'c':           if ((cr = crange_find(sp, sp->currow, sp->curcol))) {
                                ins_string(sp, range_addr(sp, cr->rr));
                                toggle_navigate_mode();
                                ins_in_line(sp, ' ');
                                sp->showrange = 0;
                            }                                       break;
        case 'f':           if ((fr = frange_get_current(sp))) {
                                ins_string(sp, range_addr(sp, fr->orr));
                                toggle_navigate_mode();
                                ins_in_line(sp, ' ');
                                sp->showrange = 0;
                            }                                       break;
        case 'r':           if ((fr = frange_get_current(sp))) {
                                ins_string(sp, range_addr(sp, fr->irr));
                                toggle_navigate_mode();
                                ins_in_line(sp, ' ');
                                sp->showrange = 0;
                            }                                       break;
        case SC_KEY_LEFT:
        case 'h':           backcol(sp, uarg);                      break;
        case SC_KEY_RIGHT:
        case 'l':           forwcol(sp, uarg);                      break;
        case SC_KEY_DOWN:
        case ctl('n'):
        case 'j':           forwrow(sp, uarg);                      break;
        case SC_KEY_UP:
        case ctl('p'):
        case 'k':           backrow(sp, uarg);                      break;
        case 'q':
        case ctl('g'):
        case ctl('v'):
        case ESC:           toggle_navigate_mode();
                            sp->showrange = 0;                      break;
        case 'H':           backcol(sp, sp->curcol - sp->stcol + 2);
                                                                    break;
        case SC_KEY_NPAGE:     /* next page */
        case ctl('f'):
        case 'J':           forwpage(sp, uarg);                     break;
        case SC_KEY_PPAGE:     /* previous page */
        case ctl('b'):
        case 'K':           backpage(sp, uarg);                     break;
        case 'L':           forwcol(sp, lcols - (sp->curcol - sp->stcol) + 1);  break;
        case ctl('a'):
        case SC_KEY_HOME:   gohome(sp);                             break;
        case '0':           leftlimit(sp);                          break;
        case '$':           rightlimit(sp);                         break;
        case '^':           gototop(sp);                            break;
        case '#':           gotobottom(sp);                         break;
        case 'o':           if (sp->showrange) {
                                SWAPINT(sp->currow, sp->showsr);
                                SWAPINT(sp->curcol, sp->showsc);
                            }                                       break;
        case 'm':           markcell(sp);                           break;
        case '`':
        case '\'':          dotick(sp, c);                          break;
        case '*':           if (nmgetch(0) == '*') gotonote(sp);    break;
        case 'g':           dogoto(sp);                             break;
        case 'n':           go_last(sp);                            break;
        case 'w':           forwcell(sp, uarg);                     break;
        case 'b':           backcell(sp, uarg);                     break;
        case 'C':           if (braille) braillealt ^= 1;           break;
        }
    }
}

static void edit_mode(void) {
#if 0
    if (emacs_bindings) {
        insert_mode();
        return;
    }
#endif
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
        init_line();
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

static void dotab(sheet_t *sp) {
    // XXX: using static variables for this is bogus as named range may be deleted
    static struct nrange *firstmatch;
    static struct nrange *lastmatch;
    static struct nrange *nextmatch;
    int i, len;

    if (linelim > 0 && (isalnumchar_(line[linelim-1]) ||
                        (completethis && line[linelim-1] == ' '))) {
        if (!completethis) {
            for (i = linelim - 1; i > 0 && isalnumchar_(line[i-1]); i--)
                continue;
            completethis = line + i;
            len = linelim - i;
            if (!nrange_find_name(sp, completethis, -len, &lastmatch)) {
                firstmatch = lastmatch;
                while (firstmatch->next &&
                       !strncmp(completethis, s2c(firstmatch->next->name), len))
                    firstmatch = firstmatch->next;
                nextmatch = firstmatch;
            } else {
                nextmatch = NULL;
            }
        }
        if (nextmatch) {
            len = line + linelim - completethis;
            strsplice(line, sizeof line, completethis - line, len, NULL, 0);
            linelim -= len;
            linelen -= len;
            ins_string(sp, s2c(nextmatch->name));
            if (completethis > line && completethis[-1] == ' ' && line[linelim] != ' ')
                ins_in_line(sp, ' ');
            if (nextmatch == lastmatch)
                nextmatch = firstmatch;
            else
                nextmatch = nextmatch->prev;
        }
    } else
        startshow(sp);
}

/* show the current range (see ^I), we are moving around to define a range */
static void startshow(sheet_t *sp) {
    sp->showrange = 1;
    sp->showsr = sp->currow;
    sp->showsc = sp->curcol;
    toggle_navigate_mode();
}

/* insert the range we defined by moving around the screen, see startshow(sp) */
static void showdr(sheet_t *sp) {
    char buf[32];
    const char *s = buf;
    int minsr = sp->showsr < sp->currow ? sp->showsr : sp->currow;
    int minsc = sp->showsc < sp->curcol ? sp->showsc : sp->curcol;
    int maxsr = sp->showsr > sp->currow ? sp->showsr : sp->currow;
    int maxsc = sp->showsc > sp->curcol ? sp->showsc : sp->curcol;

    if (sp->showrange == SHOWROWS) {
        snprintf(buf, sizeof buf, "%d:%d", minsr, maxsr);
    } else if (sp->showrange == SHOWCOLS) {
        snprintf(buf, sizeof buf, "%s:%s", coltoa(minsc), coltoa(maxsc));
    } else {
        s = range_addr(sp, rangeref(minsr, minsc, maxsr, maxsc));
    }
    ins_string(sp, s);
    toggle_navigate_mode();
    sp->showrange = 0;
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

static void dotcmd(sheet_t *sp) {
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
        write_line(sp, c);
    }
    do_dot = 0;
    doti = 0;
    dotcalled = 0;
}

static int vigetch(sheet_t *sp) {
    if (do_dot) {
        if (dotb[doti] != '\0') {
            return dotb[doti++];
        } else {
            do_dot = 0;
            doti = 0;
            return nmgetch(0);
        }
    }
    update(sp, 1);
    return nmgetch(0);
}

/* saves the current line for possible use by an undo cmd */
static void u_save(int c) {
    undo_len = pstrcpy(undo_line, sizeof undo_line, line);
    undo_lim = linelim;

    /* reset dot command if not processing it. */
    if (!do_dot) {
        doti = 0;
        savedot(c);
    }
}

/* Restores the current line saved by u_save() */
/* swap line and undo_line, linelim and undo_lim */
static void restore_it(void) {
    int i, len;

    len = linelen;
    if (len < undo_len)
        len = undo_len;
    for (i = 0; i <= len; i++) {
        char c = line[i];
        line[i] = undo_line[i];
        undo_line[i] = c;
    }
    SWAPINT(linelim, undo_lim);
    SWAPINT(linelen, undo_len);
}

/* This command stops the editing process. */
static void stop_edit(sheet_t *sp) {
    if (search_ind != ' ') {
        search_ind = ' ';
        linelen = pstrcpy(line, sizeof line, s2str(history[0]));
        // XXX: update linelim?
        write_line(sp, 'G');
    } else {
        sp->showrange = 0;
        numeric_field = 0;
        linelim = -1;
        screen_clear_line(1);
    }
}

/*
 * Motion commands.  Forward motion commands take an argument
 * which, when set, cause the forward motion to continue onto
 * the null at the end of the line instead of stopping at the
 * the last character of the line.
 */
static int forw_line(int a, int stop_null) {
    int cpos = linelim;

    if (cpos < 0)
        return cpos;

    if (a >= 0 && cpos + a <= linelen)
        cpos += a;
    else
        cpos = linelen;

    if (cpos > 0 && cpos == linelen && !stop_null)
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

    while (cpos >= 0 && cpos < linelen && a--) {
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

        if (big_word) {
            while ((c = line[cpos]) != '\0' && c != ' ')
                cpos++;
        } else
        if (iswordchar(line[cpos])) {
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
            while (cpos > 0 && (c = line[cpos]) != '\0' && c != ' ')
                --cpos;
        else if (iswordchar(line[cpos])) {
            while (cpos > 0 && iswordchar(line[cpos]))
                --cpos;
        } else {
            while (cpos > 0 && (c = line[cpos]) != '\0' && !iswordchar(c) && c != ' ')
                --cpos;
        }

        /* We are done - fix up the one too far */
        if (cpos > 0 && line[cpos] != '\0' && line[cpos+1] != '\0')
            cpos++;
    }

    return cpos;
}

/* Text manipulation commands */

/* If back_null is set, back up if the deletion leaves you at the null
 * line terminator.  Otherwise, don't.
 */
static void del_in_line(int a, int back_null) {
    int i, lim = linelim, len = linelen;

    if (lim >= 0) {
        if (a > len - lim)
            a = len - lim;
        pstrncpy(putbuf, sizeof putbuf, line + lim, a);
        for (i = lim; i <= len - a; i++)
            line[i] = line[i+a];
        linelen -= a;
    }
    if (back_null && lim > 0 && line[lim] == '\0')
        --linelim;
}

static void ins_in_line(sheet_t *sp, int c) {
    static int inabbr;

    if (c < 256) {
        if (linelim < 0 && c > 0) {
            init_line();
        }
        if (!inabbr && linelim > 0 && !isalnumchar_(c)) {
            inabbr++;
            doabbrev(sp);
            inabbr--;
        }
        if (c > 0) {
            char buf[1];
            *buf = c;
            if (strsplice(line, sizeof line, linelim, 0, buf, 1) < (int)(sizeof line)) {
                linelen++;
                linelim++;
            }
        }
    }
}

// XXX: should not try and expand abbrevs one byte at a time
static void ins_string(sheet_t *sp, const char *s) {
    while (*s)
        ins_in_line(sp, *s++);
}

static void doabbrev(sheet_t *sp) {
    int len, pos, lim = linelim;
    struct abbrev *a;
    struct abbrev *prev;

    if (istart < 0 || lim < 2)
        return;

    if (!isalnumchar_(line[lim - 1]) ||
        !(mode == INSERT_MODE || mode == SEARCH_MODE) || istart >= lim)
        return;

    pos = lim - 2;
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

    len = lim - pos;
    if (len && (a = abbrev_find(sp, line + pos, len, &prev)) != NULL) {
        if (len > 1 || pos == 0 || line[pos-1] == ' ') {
            linelim = pos;
            del_in_line(len, 0);
            ins_string(sp, s2c(a->exp));
        }
    }
}

static void append_line(void) {
    if (linelim >= 0 && linelim < linelen)
        linelim++;
    insert_mode();
}

static void change_case(int a) {
    if (linelim < 0) {
        init_line();
    }
    // XXX: bogus! should stop at end of string
    if (a > linelen - linelim)
        a = linelen - linelim;
    while (a --> 0) {
        if (islowerchar(line[linelim]))
            line[linelim] = toupperchar(line[linelim]);
        else if (isupperchar(line[linelim]))
            line[linelim] = tolowerchar(line[linelim]);
        linelim = forw_line(1, 0);
    }
}

static void rep_char(sheet_t *sp) {
    int c;

    if (linelim < 0) {
        init_line();
    }
    c = vigetch(sp);
    savedot(c);
    if (c < 256 && c != ESC && c != ctl('g')) {
        if (linelim == linelen) {
            if (linelen == (int)(sizeof(line) - 1))
                return;
            line[++linelen] = '\0';
        }
        line[linelim] = c;
    }
}

static void replace_in_line(int c) {
    if (c < 256) {
        if (linelim < 0) {
            init_line();
        }
        if (linelim == linelen) {
            if (linelen == (int)(sizeof(line) - 1))
                return;
            line[++linelen] = '\0';
        }
        line[linelim++] = c;
    }
}

static void back_space(void) {
    if (linelim == 0)
        return;

    linelim = back_line(1);
    del_in_line(1, 0);
    if (linelim < istart)
        istart = linelim;
}

/* Setting change to 1 makes `w' act like `e' so that `cw' will act like
 * `ce', just as in vi.  Setting change to 0 causes `w' to act as usual.
 */

static int get_motion(sheet_t *sp, int change) {
    int c, lim;
    int arg2 = 0;

    c = vigetch(sp);
    if (c == '0') {
        savedot(c);
        return 0;
    }
    while (c >= '0' && c <= '9') {
        arg2 = 10 * arg2 + c - '0';
        c = vigetch(sp);
    }
    if (!arg2)
        arg2++;
    uarg *= arg2;
    if (!nosavedot) {
        savedot(c);
        dotarg = uarg;
    }
    switch (c) {
    case '$':   return linelen;
    case 'b':   return back_word(uarg, 0);
    case 'B':   return back_word(uarg, 1);
    case 'c':   return change ? -1 : linelim;
    case 'd':   return !change ? -1 : linelim;
    case 'e':   return forw_word(uarg, 1, 0, 1) + 1;
    case 'E':   return forw_word(uarg, 1, 1, 1) + 1;
    case 'f':   return ((lim = find_char(sp, linelim, uarg, 1)) == linelim) ? lim : lim + 1;
    case 'F':   return find_char(sp, linelim, uarg, -1);
    case 'h':   return back_line(uarg);
    case 'l':   return forw_line(uarg, 1);
    case 't':   return ((lim = to_char(sp, linelim, uarg, 1)) == linelim) ? lim : lim + 1;
    case 'T':   return to_char(sp, linelim, uarg, -1);
    case 'w':   return forw_word(uarg, change, 0, 1) + change;
    case 'W':   return forw_word(uarg, change, 1, 1) + change;
    default:    return linelim;
    }
}

static void yank_cmd(sheet_t *sp, int delete, int change) {
    int cpos;

    if ((cpos = get_motion(sp, change)) == -1) {
        cpos++;
        linelim = linelen;
    }
    yank_chars(cpos, linelim, delete);
}

static void yank_chars(int first, int last, int delete) {
    if (first == last)
        return;
    if (last < first) SWAPINT(last, first);
    linelim = first;
    *putbuf = '\0';
    strsplice(putbuf, sizeof putbuf, 0, 0, line + first, last - first);
    if (delete) {
        strsplice(line, sizeof line, first, last - first, NULL, 0);
        linelen -= last - first;
    }
}

static void del_to_end(void) {
    if (linelim < 0)
        return;
    pstrcpy(putbuf, sizeof putbuf, line + linelim);
    line[linelen = linelim] = '\0';
    linelim = back_line(1);
}

static void cr_line(sheet_t *sp, int action) {
    struct frange *fr;

    ins_in_line(sp, 0);
    insert_mode();
    numeric_field = 0;
    if (linelim == -1) {    /* '\n' alone will put you into insert mode */
        init_line();        /* unless numeric and action are both set */
        if (sp->numeric && action)
            cellassign = 1;
        else
            return;
    }
    save_hist();
    nosavedot = 1;
    parse_line(line);
    sp->showrange = 0;
    linelim = -1;
    if (cellassign) {
        cellassign = 0;
        switch (action) {
        case CRROWS:
            if ((sp->rowlimit >= 0) && (sp->currow >= sp->rowlimit)) {
                forwcol(sp, 1);
                sp->currow = 0;
            } else {
                if ((fr = frange_get_current(sp))) {
                    forwrow(sp, 1);
                    if (sp->currow > fr->irr.right.row) {
                        backrow(sp, 1);
                        if (sp->autowrap) {
                            forwcol(sp, 1);
                            sp->currow = fr->irr.left.row;
                            if (row_hidden(sp, sp->currow))
                                forwrow(sp, 1);
                            if (sp->curcol > fr->irr.right.col) {
                                backcol(sp, 1);
                                if (sp->autoinsert)
                                    sp->curcol += insert_cols(sp, cellref_current(sp), 1, 1);
                                else {
                                    sp->currow = fr->irr.right.row;
                                    if (row_hidden(sp, sp->currow))
                                        backrow(sp, 1);
                                }
                            }
                        } else if (sp->autoinsert)
                            sp->currow += insert_rows(sp, cellref_current(sp), 1, 1);
                    }
                } else
                    forwrow(sp, 1);
            }
            break;
        case CRCOLS:
            if ((sp->collimit >= 0) && (sp->curcol >= sp->collimit)) {
                forwrow(sp, 1);
                sp->curcol = 0;
            } else {
                if ((fr = frange_get_current(sp))) {
                    forwcol(sp, 1);
                    if (sp->curcol > fr->irr.right.col) {
                        backcol(sp, 1);
                        if (sp->autowrap) {
                            forwrow(sp, 1);
                            sp->curcol = fr->irr.left.col;
                            if (col_hidden(sp, sp->curcol))
                                forwcol(sp, 1);
                            if (sp->currow > fr->irr.right.row) {
                                backrow(sp, 1);
                                if (sp->autoinsert)
                                    sp->currow += insert_rows(sp, cellref_current(sp), 1, 1);
                                else {
                                    sp->curcol = fr->irr.right.col;
                                    if (col_hidden(sp, sp->curcol))
                                        backcol(sp, 1);
                                }
                            }
                        } else if (sp->autoinsert)
                            sp->curcol += insert_cols(sp, cellref_current(sp), 1, 1);
                    }
                } else
                    forwcol(sp, 1);
            }
            break;
        default:
            break;
        }
    }
}

static void doshell(sheet_t *sp) {
    /*
    *  "! command"  executes command
    *  "!"      forks a shell
    *  "!!" repeats last command
    */
#ifdef NOSHELL
    error("Shell not available");
#else /* NOSHELL */
    const char *shl;
    int pid, temp;
    char cmdbuf[MAXCMD];
    char *cmd;
    static char lastcmd[MAXCMD];

    if (!(shl = getenv("SHELL")))
        shl = "/bin/sh";

    screen_deraw(1);
    fputs("! ", stdout);
    fflush(stdout);
    if (!fgets(cmdbuf, MAXCMD, stdin))
        *cmdbuf = '\0';
    cmd = strtrim(cmdbuf);   /* strip the trailing newline */
    if (strcmp(cmd, "!") == 0)           /* repeat? */
        pstrcpy(cmd = cmdbuf, sizeof cmdbuf, lastcmd);
    else
        pstrcpy(lastcmd, sizeof lastcmd, cmd);

    if (sp->modflg) {
        puts("[No write since last change]");
        fflush(stdout);
    }

    if (!(pid = fork())) {
        signal(SIGINT, SIG_DFL);  /* reset */
        if (*cmd)
            execl(shl, shl, "-c", cmd, (char *)NULL);
        else
            execl(shl, shl, (char *)NULL);
        exit(-127);
    }

    while (pid != wait(&temp))
        continue;

    screen_pause();
    screen_goraw();
    screen_erase();
#endif /* NOSHELL */
}

static void list_all(sheet_t *sp) {
    char px[MAXCMD];
    const char *pager;
    FILE *f;
    int pid;

    /* Show color definitions and various types of ranges */
    // XXX: deal with raw mode switch?
    if (!nrange_test(sp) && !frange_test(sp) && !crange_test(sp)) {
        error("Nothing to show");
        return;
    }
    if (!(pager = getenv("PAGER")))
        pager = DFLT_PAGER;
    snprintf(px, sizeof px, "| %s", pager);
    f = openfile(px, sizeof px, &pid, NULL);
    if (!f) {
        error("Cannot open pipe to %s", pager);
        return;
    }
    if (!brokenpipe) fprintf(f, "Named Ranges:\n=============\n\n");
    if (!brokenpipe) nrange_list(sp, f);
    if (!brokenpipe) fprintf(f, "\nFrames:\n=======\n\n");
    if (!brokenpipe) frange_list(sp, f);
    if (!brokenpipe) fprintf(f, "\nColors:\n=======\n\n");
    if (!brokenpipe) crange_list(sp, f);
    if (!brokenpipe) fprintf(f, "\nRegisters:\n==========\n\n");
    if (!brokenpipe) delbuf_list(sp, f);
    if (!brokenpipe) fprintf(f, "\n");
    closefile(f, pid, 0);
}

/* History functions */

void free_hist(void) {
    int i;
    for (i = 0; i <= HISTLEN; i++) {
        string_free(history[i]);
        history[i] = NULL;
    }
#if defined REGCOMP
#elif defined RE_COMP
#elif defined REGCMP
    free(last_search);
    last_search = NULL;
#else
    string_free(last_search);
    last_search = NULL;
#endif
}

static void save_hist(void) {
    if (!lasthist || strcmp(s2str(history[lasthist]), line)) {
        if (lasthist < 0)
            lasthist = 1;
        else
            lasthist = lasthist % HISTLEN + 1;

        if (lasthist > endhist)
            endhist = lasthist;

        string_set(&history[lasthist], string_new(line));
        histsessionnew++;
    }
    string_free(history[0]);
    history[0] = NULL;
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
        linelen = pstrcpy(line, sizeof line, s2str(history[histp]));
        last_col();
    }
    if (histp) {
        error("History line %d", endhist - lasthist + histp);
    } else {
        screen_clear_line(1);  // XXX: should not be necessary
    }
}

static void back_hist(void) {
    if (histp == 0) {
        string_set(&history[0], string_new(line));
        if (lasthist >= 0)
            histp = lasthist;
    } else if (histp == 1) {
        if (endhist != lasthist)
            histp = endhist;
    } else if (histp != ((lasthist + 1) % (endhist + 1)))
        histp--;

    if (lasthist >= 0) {
        linelen = pstrcpy(line, sizeof line, s2str(history[histp]));
        last_col();
    }
    if (histp) {
        error("History line %d", endhist - lasthist + histp);
    } else {
        screen_clear_line(1);  // XXX: should not be necessary
    }
}

static void search_hist(void) {
#if defined REGCOMP
#elif defined RE_COMP
    char *tmp;
#elif defined REGCMP
#else
#endif

    if (linelim < 1) {
        init_line();
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
    string_set(&last_search, string_new(line));
#endif
    linelen = pstrcpy(line, sizeof line, s2str(history[0]));
    //last_col();
    search_again(FALSE);
    if (mode != EDIT_MODE) edit_mode();
    search_ind = ' ';
}

static void search_again(sc_bool_t reverse) {
    int prev_match;
    int found_it = 0;
#if defined REGCOMP
#elif defined RE_COMP
#elif defined REGCMP
#else
    char *look_here;
    int do_next;
#endif

#if defined REGCOMP
    if (last_search == NULL)
        return;
#elif defined RE_COMP
#elif defined REGCMP
    if (last_search == NULL || *last_search == '\0')
        return;
#else
    if (sempty(last_search))
        return;
#endif
    prev_match = histp > 0 ? histp : 0;
    screen_clear_line(1);

    do {
        if (lasthist > 0) {
            if (!(search_dir ^ reverse) && histp != lasthist)
                if (histp <= 0) {
                    histp = ((lasthist + 1) % endhist);
                    linelen = pstrcpy(line, sizeof line, s2str(history[histp]));
                } else
                    forw_hist();
            else if ((search_dir ^ reverse) && histp != ((lasthist + 1) % endhist))
                back_hist();
            else {
                histp = 0;
                linelen = pstrcpy(line, sizeof line, s2str(history[0]));
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
                linelen = pstrcpy(line, sizeof line, s2str(history[histp]));
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
        // XXX: should wrap this into a common emulation
        look_here = line;
        do_next = 0;
        while ((look_here = strchr(look_here, *s2c(last_search))) != NULL &&
                !found_it && !do_next) {

            if (strncmp(look_here, s2c(last_search), slen(last_search)) == 0)
                found_it++;
            else if (look_here < line + linelen - 1)
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
    linelim = linelen - 1;
}

static void readhistfile(FILE *fp) {
    if (!fp)
        return;
    while (fgets(line, FBUFLEN, fp)) {
        int len = strlen(line);
        if (len && line[len - 1] == '\n') {
            line[--len] = '\0'; /* chop the \n */
        }
        linelen = len;
        save_hist();
    }
    init_line();
    linelim = -1;
}

// XXX: move out of vi.c ?
void write_hist(SCXMEM string_t *filename) {
    char path[FBUFLEN];
    int i;
    FILE *fp, *tmpfp = NULL;

    if (sempty(filename)) {
        string_free(filename);
        return;
    }

    /* merge the history file with the new elements from the current session */
    /* should just append the new commands? */
    if (histsessionnew < HISTLEN) {
        /* write the new history for this session to a tmp file */
        tmpfp = tmpfile();
        for (i = 1; i <= histsessionnew; i++) {
            histsessionstart = histsessionstart % endhist + 1;
            if (history[histsessionstart])
                fprintf(tmpfp, "%s\n", s2c(history[histsessionstart]));
        }
        fseek(tmpfp, 0, SEEK_SET);

        /* re-read the main history, then read back in the saved session hist */
        histp = 0;
        lasthist = 0;
        endhist = -1;
        read_hist(string_dup(filename));
        readhistfile(tmpfp);

        if (fclose(tmpfp) == EOF) {
            error("fclose(tmpfile()): %s", strerror(errno));
        }
    }

    /* now write to whole lot out to the proper save file */
    pstrcpy(path, sizeof path, s2c(filename));
    if (findhome(path, sizeof path) && (fp = fopen(path, "w")) != NULL) {
        for (i = 1; i <= endhist; i++) {
            lasthist = lasthist % endhist + 1;
            if (history[lasthist])
                fprintf(fp, "%s\n", s2c(history[lasthist]));
        }
        if (fclose(fp) == EOF) {
            error("fclose(%s): %s", path, strerror(errno));
        }
    }
    string_free(filename);
}

void read_hist(SCXMEM string_t *filename) {
    char path[FBUFLEN];
    FILE *fp;

    if (sempty(filename)) {
        string_free(filename);
        return;
    }

    pstrcpy(path, sizeof path, s2c(filename));
    if (findhome(path, sizeof path) && (fp = fopen(path, "r")) != NULL) {
        readhistfile(fp);
        if (fclose(fp) == EOF) {
            error("fclose(%s): %s", path, strerror(errno));
        }
    }

    histsessionstart = lasthist;
    histsessionnew = 0;
    string_free(filename);
}

static void col_0(void) {
    linelim = 0;
}

static void last_col(void) {
    linelim = linelen;
    if (linelim > 0 && mode_ind == 'e')
        --linelim;
}

static int find_char(sheet_t *sp, int start, int arg, int dir) {
    int lim = start;
    if (findchar)
        finddir = dir;
    findchar = vigetch(sp);
    if (doti > 0) {
        switch (dotb[doti - 1]) {
        case 'f': case 'F': case 't': case 'T':
            savedot(findchar);
            break;
        default:
            break;
        }
    }
    while (arg --> 0) {
        lim += dir;
        while (lim >= 0 && line[lim] != '\0' && line[lim] != findchar)
            lim += dir;
        if (lim < 0 || line[lim] == '\0') {
            lim = start;
            break;
        }
    }
    findfunc = 'f';
    return lim;
}

static void find_char2(sheet_t *sp, int arg, int dir) {
    if (findchar) {
        nmungetch(findchar);
        findchar = 0;
        if (findfunc == 'f')
            linelim = find_char(sp, linelim, arg, dir);
        else
            linelim = to_char(sp, linelim, arg, dir);
    }
}

static int to_char(sheet_t *sp, int start, int arg, int dir) {
    int lim = start + dir;
    if (lim >= 0 && lim < linelen)
        start = lim;
    lim = find_char(sp, start, arg, dir);
    if (lim != start)
        lim -= dir;
    findfunc = 't';
    return lim;
}

static void match_paren(void) {
    int nest = 1;
    int tmp = linelim;

    if (line[linelim] == '(') {
        while (nest && ++linelim >= 0 && line[linelim] != '\0') {
            if (line[linelim] == '(')
                nest++;
            else if (line[linelim] == ')')
                nest--;
        }
        if (line[linelim] != ')')
            linelim = tmp;
    }
    else if (line[linelim] == ')') {
        while (nest && --linelim >= 0 && line[linelim] != '\0') {
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
 * cell has changed since the last remember(sp, 0), save the remembered location
 * for the `, ', and c comands.
 */
// XXX: move out of vi.c (maybe navigate.c ?)
void remember(sheet_t *sp, int save) {
    static int remrow, remcol, remstrow, remstcol;

    if (save && (sp->currow != remrow || sp->curcol != remcol ||
                 sp->strow != remstrow || sp->stcol != remstcol))
    {
        sp->savedcr[0] = cellref(remrow, remcol);
        sp->savedst[0] = cellref(remstrow, remstcol);
    } else {
        remrow = sp->currow;
        remcol = sp->curcol;
        remstrow = sp->strow;
        remstcol = sp->stcol;
    }
}

static void gohome(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (cell_in_range(cellref(sp->currow, sp->curcol), fr->irr)
        &&  (sp->currow > fr->irr.left.row || sp->curcol > fr->irr.left.col)) {
            sp->currow = fr->irr.left.row;
            sp->curcol = fr->irr.left.col;
        } else
        if (sp->currow > fr->orr.left.row || sp->curcol > fr->orr.left.col) {
            sp->currow = fr->orr.left.row;
            sp->curcol = fr->orr.left.col;
        } else {
            sp->currow = 0;
            sp->curcol = 0;
        }
    } else {
        sp->currow = 0;
        sp->curcol = 0;
    }
    remember(sp, 1);
    FullUpdate++;
}

static void leftlimit(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->currow >= fr->irr.left.row && sp->currow <= fr->irr.right.row &&
            sp->curcol > fr->irr.left.col && sp->curcol <= fr->irr.right.col)
            sp->curcol = fr->irr.left.col;
        else
        if (sp->curcol > fr->orr.left.col)
            sp->curcol = fr->orr.left.col;
        else
            sp->curcol = 0;
    } else {
        sp->curcol = 0;
    }
    remember(sp, 1);
}

static void rightlimit(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->currow >= fr->irr.left.row && sp->currow <= fr->irr.right.row &&
            sp->curcol >= fr->irr.left.col && sp->curcol < fr->irr.right.col)
            sp->curcol = fr->irr.right.col;
        else
        if (sp->curcol >= fr->orr.left.col && sp->curcol < fr->orr.right.col)
            sp->curcol = fr->orr.right.col;
        else {
            sp->curcol = sp->maxcol;
            while (!valid_cell(sp, sp->currow, sp->curcol) && sp->curcol > fr->orr.right.col)
                sp->curcol--;
            if ((fr = frange_get_current(sp)))
                sp->curcol = fr->orr.right.col;
        }
    } else {
        sp->curcol = sp->maxcol;
        while (!valid_cell(sp, sp->currow, sp->curcol) && sp->curcol > 0)
            sp->curcol--;
        if ((fr = frange_get_current(sp)))
            sp->curcol = fr->orr.right.col;
    }
    remember(sp, 1);
}

static void gototop(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->curcol >= fr->irr.left.col && sp->curcol <= fr->irr.right.col &&
            sp->currow > fr->irr.left.row && sp->currow <= fr->irr.right.row)
            sp->currow = fr->irr.left.row;
        else
        if (sp->currow > fr->orr.left.row)
            sp->currow = fr->orr.left.row;
        else
            sp->currow = 0;
    } else {
        sp->currow = 0;
    }
    remember(sp, 1);
}

static void gotobottom(sheet_t *sp) {
    struct frange *fr;

    remember(sp, 0);
    if ((fr = frange_get_current(sp))) {
        if (sp->curcol >= fr->irr.left.col && sp->curcol <= fr->irr.right.col &&
            sp->currow >= fr->irr.left.row && sp->currow < fr->irr.right.row)
            sp->currow = fr->irr.right.row;
        else
        if (sp->currow < fr->orr.right.row)
            sp->currow = fr->orr.right.row;
        else {
            sp->currow = sp->maxrow;
            while (!valid_cell(sp, sp->currow, sp->curcol) && sp->currow > fr->orr.right.row)
                sp->currow--;
            if ((fr = frange_get_current(sp)))
                sp->currow = fr->orr.right.row;
        }
    } else {
        sp->currow = sp->maxrow;
        while (!valid_cell(sp, sp->currow, sp->curcol) && sp->currow > 0)
            sp->currow--;
        if ((fr = frange_get_current(sp)))
            sp->currow = fr->orr.right.row;
    }
    remember(sp, 1);
}

static void dogoto(sheet_t *sp) {
    char buf[80];
    int len;
    SCXMEM string_t *save_line = string_new(line);
    int save_lim = linelim;

    /* Cannot switch back to navigate mode if insert_mode() is used here
     * instead of toggle_navigate_mode(), which is what we want when doing
     * a goto from within navigate mode.
     */
    insert_mode();
    /* Tempted as I was, I had to resist making this "Where would you like
     * to go today?" - CRM :)
     */
    len = query(sp, buf, sizeof buf, "goto where?", NULL);
    if (len >= 0) {
        strsplice(buf, sizeof buf, 0, 0, "goto ", 5);
        parse_line(buf);
    }
    if (save_line) {
        linelen = pstrcpy(line, sizeof line, s2c(save_line));
        linelim = save_lim;
        string_free(save_line);
    }
    update(sp, 0);

    /* Now we need to change back to navigate mode ourselves so that
     * toggle_navigate_mode() will work properly again.
     */
    mode_ind = 'v';
    mode = NAVIGATE_MODE;
    if (!sp->showrange)
        toggle_navigate_mode();
}

int query(sheet_t *sp, char *dest, int destsize, const char *s, const char *data) {
    int c, len;

    insert_mode();
    linelim = linelen = pstrcpy(line, sizeof line, data ? data : "");
    if (s != NULL)
        error("%s", s);

    while (linelim >= 0) {
        update(sp, 0);
        switch (c = nmgetch(1)) {
        case ctl('m'):
            break;
        case ctl('g'):
            init_line();
            linelim = -1;
            break;
        case ctl('l'):
            FullUpdate++;
            screen_rebuild();
            update(sp, 1);
            continue;
        default:
            write_line(sp, c);
            continue;
        }
        break;
    }
    len = pstrcpy(dest, destsize, line);
    init_line();
    linelim = -1;
    update(sp, 0);
    return len;
}

static int mouse_sel_cell(sheet_t *sp) { /* 0: set, 1: save, 2: cmp and set */
    static int x1, y1;
    int mmode = -1, x, y, i, tx, ty, res = 1;
    struct screen_mouse_event mevent;

    if (!screen_getmouse(&mevent))
        return 1;
    if (mevent.bstate & SC_BUTTON_CLICKED(1)) {
        mmode = 0;
    } else
    if (mevent.bstate & SC_BUTTON_PRESSED(1)) {
        mmode = 1;
    } else
    if (mevent.bstate & SC_BUTTON_RELEASED(1)) {
        mmode = 2;
    } else
    if (mevent.bstate & SC_BUTTON_PRESSED(4)) {
        scroll_up(sp, 1);
        FullUpdate++;
        update(sp, 0);
        return 1;
    } else
    if (mevent.bstate & SC_BUTTON_PRESSED(5)) {
        scroll_down(sp);
        FullUpdate++;
        update(sp, 0);
        return 1;
    }
    if ((y = mevent.y - RESROW) < 0 || (x = mevent.x - sp->rescol) < 0)
        return 1;
    for (ty = sp->strow, i = y;; ty++) {
        if (row_hidden(sp, ty))
            continue;
        if (--i < 0)
            break;
    }
    for (tx = sp->stcol, i = x;; tx++) {
        if (col_hidden(sp, tx))
            continue;
        if ((i -= sp->colfmt[tx].fwidth) < 0)
            break;
    }
    switch (mmode) {
    case 1:
        y1 = ty;
        x1 = tx;
        break;
    case 2:
        if (y1 != ty || x1 != tx)
            break;
        FALLTHROUGH;
    default:
        sp->currow = ty;
        sp->curcol = tx;
        res = 0;
        break;
    }
    update(sp, 0);
    return res;
}

static void mouse_set_pos(void) {
    static int x0;
    struct screen_mouse_event mevent;

    if (!screen_getmouse(&mevent) || mevent.y)
        return;
    if (mevent.bstate & SC_BUTTON_CLICKED(1)) {
        linelim = mevent.x - 3;
    } else
    if (mevent.bstate & SC_BUTTON_PRESSED(1)) {
        x0 = mevent.x;
    } else
    if (mevent.bstate & SC_BUTTON_RELEASED(1)) {
        if (x0 == mevent.x)
            linelim = mevent.x - 3;
    }
}

/*
 * Given a row/column command letter, emit a small menu, then read a qualifier
 * character for a row/column command and convert it to 'r' (row), 'c'
 * (column), or 0 (unknown).  If ch is 'p', three extra qualifiers, 'm', 'x',
 * and 't', are allowed.  If ch is 'Z', an extra qualifier 'Z' is allowed.
 */

static int get_rcqual(sheet_t *sp, int ch) {
    int c;

    error("%s row/column:  r: row  c: column%s",
          (ch == SC_KEY_IC)  ? "Insert" :
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

    screen_refresh();

    switch (c = nmgetch(1)) {
    case 'r':
    case 'c':       return c;
    case 'p':
    case 'm':
    case 'x':
    case 't':
    case 'f':
    case 'C':
    case '.':       return (ch == 'p') ? c : 0;
    case 'Z':       return (ch == 'Z') ? c : 0;
    case ESC:
    case ctl('g'):  return ESC;

    case 'd':       if (ch == 'd') {
                        nmungetch('x');
                        return ESC;
                    } else
                        return 0;

    case 'y':       if (ch == 'y') {
                        yank_range(sp, rangeref_current(sp));
                        return ESC;
                    } else
                        return 0;

    case 'v':       if (ch == 'v') {
                        valueize_area(sp, rangeref_current(sp));
                        return ESC;
                    } else
                        return 0;

    case SC_KEY_UP:
    case SC_KEY_DOWN:
    case SC_KEY_PPAGE:
    case SC_KEY_NPAGE:
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
                    write_line(sp, 'A');
                    startshow(sp);
                    sp->showrange = SHOWROWS;
                    sp->showsr = sp->currow;
                    nmungetch(c);
                    return ESC;

    case SC_KEY_BACKSPACE:
    case SC_KEY_LEFT:
    case SC_KEY_RIGHT:
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
                    write_line(sp, 'A');
                    startshow(sp);
                    sp->showrange = SHOWCOLS;
                    sp->showsc = sp->curcol;
                    nmungetch(c);
                    return ESC;

    default:        return 0;
    }
    /*NOTREACHED*/
}

static void formatcol(sheet_t *sp, int arg) {
    int c, i;
    int mf = sp->modflg;
    colfmt_t *oldformat;
    buf_t buf;

    if (arg < 0)
        arg = 0;
    else
    if (arg > sp->maxcol - sp->curcol + 1)
        arg = sp->maxcol - sp->curcol + 1;

    /* save column widths and formats */
    oldformat = scxmalloc(sizeof(*oldformat) * arg);
    for (i = 0; i < arg; i++) {
        oldformat[i] = sp->colfmt[i + sp->curcol];
    }
    error("Current format is %d %d %d",
          sp->colfmt[sp->curcol].fwidth,
          sp->colfmt[sp->curcol].precision,
          sp->colfmt[sp->curcol].realfmt);
    screen_refresh();
    c = nmgetch(0);
    //screen_clear_line(1);     // XXX: clear line?
    for (;;) {
        if (c < 0 || c == ctl('m') || c == 'q' || c == ESC ||
            c == ctl('g') || linelim >= 0)
            break;
        if (c >= '0' && c <= '9') {
            for (i = sp->curcol; i < sp->curcol + arg; i++)
                sp->colfmt[i].realfmt = c - '0';
        } else {
            switch (c) {
            case SC_KEY_LEFT:
            case '<':
            case 'h':
                for (i = sp->curcol; i < sp->curcol + arg; i++) {
                    if (sp->colfmt[i].fwidth > 1) {
                        sp->colfmt[i].fwidth--;
                        sp->modflg++;
                    }
                }
                break;
            case SC_KEY_RIGHT:
            case '>':
            case 'l':
                for (i = sp->curcol; i < sp->curcol + arg; i++) {
                    if (sp->colfmt[i].fwidth < screen_COLS - sp->rescol - 2) {
                        sp->colfmt[i].fwidth++;
                        sp->modflg++;
                    }
                }
                break;
            case SC_KEY_DOWN:
            case '-':
            case 'j':
                for (i = sp->curcol; i < sp->curcol + arg; i++) {
                    if (sp->colfmt[i].precision > 0) {
                        sp->colfmt[i].precision--;
                        sp->modflg++;
                    }
                }
                break;
            case SC_KEY_UP:
            case '+':
            case 'k':
                for (i = sp->curcol; i < sp->curcol + arg; i++) {
                    if (sp->colfmt[i].precision < 255) {
                        sp->colfmt[i].precision++;
                        sp->modflg++;
                    }
                }
                break;
            case ' ':
                // XXX: should use current format?
                if (arg == 1) {
                    set_line("format [for column] %s ", coltoa(sp->curcol));
                } else {
                    set_line("format [for columns] %s:%s ",
                             coltoa(sp->curcol), coltoa(sp->curcol + arg - 1));
                }
                insert_mode();
                error("Current format is %d %d %d",
                      sp->colfmt[sp->curcol].fwidth,
                      sp->colfmt[sp->curcol].precision,
                      sp->colfmt[sp->curcol].realfmt);
                continue;
            case '=':
                error("Define format type (0-9):");
                screen_refresh();
                c = nmgetch(1);
                if (c >= '0' && c <= '9') {
                    if (sp->colformat[c - '0']) {
                        buf_init(buf, line, sizeof line);
                        buf_setf(buf, "format %c = \"", c);
                        buf_quotestr(buf, 0, s2c(sp->colformat[c - '0']), 0);
                        linelim = linelen = buf->len;
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
                screen_rebuild();
                break;
            default:
                break;
            }
        }
        error("Current format is %d %d %d",
              sp->colfmt[sp->curcol].fwidth,
              sp->colfmt[sp->curcol].precision,
              sp->colfmt[sp->curcol].realfmt);
        FullUpdate++;
        update(sp, 1);
        screen_refresh();
        if (linelim < 0) {
            c = nmgetch(0);
            if (c == ESC || c == ctl('g') || c == 'q') {
                /* restore column widths and formats */
                for (i = 0; i < arg; i++) {
                    sp->colfmt[i + sp->curcol].fwidth = oldformat[i].fwidth;
                    sp->colfmt[i + sp->curcol].precision = oldformat[i].precision;
                    sp->colfmt[i + sp->curcol].realfmt = oldformat[i].realfmt;
                }
                sp->modflg = mf;
                FullUpdate++;
                update(sp, 1);
            }
        }
    }
    scxfree(oldformat);
    if (c >= 0)
        screen_clear_line(1);     // XXX: should get rid of this hack
}

/* called from main() for -P/ option, via sc_cmd_put() */
static int vi_select_range(sheet_t *sp, const char *cmd, const char *arg) {
    int c;

    init_line();
    if (mode_ind != 'v')
        write_line(sp, ctl('v'));

    error("Select range:");
    update(sp, 1);
    while (!linelim) {
        c = nmgetch(0);
        //screen_clear_line(1);   // XXX: why delay?
        switch (c) {
        case '.':
        case ':':
        case ctl('i'):
            if (!sp->showrange) {
                write_line(sp, c);
                break;
            }
            /* else drop through */
            FALLTHROUGH;
        case ctl('m'):
            set_line("%s ", cmd);
            write_line(sp, '.');
            if (sp->showrange)
                write_line(sp, '.');
            if (arg) {
                linelim = linelen = pstrcat(line, sizeof line, arg);
            }
            break;
        case ESC:
        case ctl('g'):
        case 'q':
            linelim = -1;
            break;
        case ctl('l'):
            FullUpdate++;
            screen_rebuild();
            break;
        default:
            write_line(sp, c);
            break;
        }
        /* goto switches to insert mode when done, so we
         * have to switch back.
         */
        if (mode_ind == 'i')
            write_line(sp, ctl('v'));
        screen_clear_line(1);
        update(sp, 1);
    }
    return linelim;
}

// XXX: get rid of this hack
void sc_cmd_put(sheet_t *sp, const char *arg, int vopt) {
    if (*optarg == '/') {
        int in = dup(STDIN_FILENO);
        int out = dup(STDOUT_FILENO);
        freopen("/dev/tty", "r", stdin);
        freopen("/dev/tty", "w", stdout);
        usecurses = TRUE;
        startdisp();
        screen_rebuild();
        FullUpdate++;

        // XXX: should not use global line array
        //      another ugly hack
        vi_select_range(sp, "put", optarg); // sets linelim
        stopdisp();
        dup2(in, STDIN_FILENO);
        dup2(out, STDOUT_FILENO);
        close(in);
        close(out);
    } else {
        set_line("put %s", optarg ? optarg : "");
    }
    if (linelim > 0) {
        if (vopt) {
            linelen = pstrcat(line, sizeof line, " *");
        }
        parse_line(line);
    }
    linelim = -1;
}

void sc_cmd_write(const char *arg) {
    char buf[FBUFLEN];
    snprintf(buf, sizeof buf, "write %s", optarg);
    parse_line(buf);
}

/* return 1 if yes given, 0 otherwise */
int yn_ask(const char *msg) {
    screen_draw_line(0, 0, msg);
    screen_refresh();
    // should clear screen row 0 upon returning
    for (;;) {
        switch (nmgetch(0)) {
        case 'y':
        case 'Y':
            return 1;
        case 'n':
        case 'N':
            return 0;
        case ctl('g'):
        case ESC:
        case EOF:
            return -1;
        }
    }
}

/* check if tbl was modified and ask to save */
int modcheck(sheet_t *sp, const char *endstr) {
    int yn_ans;

    if (sp->modflg && sp->curfile[0]) {
        char lin[100];

        snprintf(lin, sizeof lin, "File \"%s\" is modified, save%s? ", sp->curfile, endstr);
        if ((yn_ans = yn_ask(lin)) < 0)
            return 1;
        else
        if (yn_ans == 1) {
            if (writefile(sp, sp->curfile, rangeref_total(sp), DCP_DEFAULT) < 0)
                return 1;
        }
    } else if (sp->modflg) {
        if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
            return 1;
        else
            return yn_ans;
    }
    return 0;
}

int edit_cell(sheet_t *sp, buf_t buf, int row, int col, struct ent *p,
              int deltar, int deltac, int dcp_flags, int c0)
{
    int align = p ? (p->flags & ALIGN_MASK) : ALIGN_DEFAULT;
    size_t len;
    const char *command;

    switch (align) {
    default:            command = "let";         break;
    case ALIGN_LEFT:    command = "leftstring";  break;
    case ALIGN_CENTER:  command = "label";       break;
    case ALIGN_RIGHT:   command = "rightstring"; break;
    }
    len = buf_setf(buf, "%s %s = ", command, cell_addr(sp, cellref(row, col)));
    if (p) {
        if (p->expr && !(dcp_flags & DCP_NO_EXPR)) {
            // XXX: should pass row, col as a cell reference
            decompile_expr(sp, buf, p->expr, deltar, deltac, dcp_flags);
        } else
        if (p->type == SC_NUMBER) {
            // XXX: should convert to locale: use out_number()?
            buf_printf(buf, "%.15g", p->v);
        } else
        if (p->type == SC_BOOLEAN) {
            // XXX: should translate?
            buf_puts(buf, p->v ? "TRUE" : "FALSE");
        } else
        if (p->type == SC_STRING) {
            buf_quotestr(buf, '"', s2str(p->label), '"');
        } else
        if (p->type == SC_ERROR) {
            buf_puts(buf, error_name[p->cellerror]);
        }
    }
    if (len == buf->len && c0) {
        /* output a single `"` for the user to start entering the string */
        buf_putc(buf, c0);
    }
    return buf->len;
}
