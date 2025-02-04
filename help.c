/*      SC      A Table Calculator
 *              Help data
 *
 *              R. Bond, 1988
 *              J. Buhrt 1990
 *              Charlie Gordon 2021
 *
 *              $Revision: 9.1 $
 */

#ifdef QREF
#include <stdio.h>
static const char * const header = " Quick Reference";
static const char * const revision = "$Revision: 9.1 $";
#else
#include "sc.h"
#endif /* QREF */

static const char * const intro[] = {
" Overview:",
" ",
" A:   This overview",
" B:   Toggle Options",
" C:   Set Options",
" D:   Cursor movement commands",
" E:   Cell entry and editing commands",
" F:   Line Editing",
" G:   File commands",
" H:   Row and column commands",
" I:   Range commands",
" J:   Miscellaneous commands",
" K:   Variable names/Expressions",
" L:   Range functions",
" M:   Numeric functions",
" N:   String functions",
" O:   Financial functions",
" P:   Time and date functions",
" ",
" Q:   Return to main spreadsheet",
NULL
};

static const char * const toggleoptions[] = {
" B: Toggle Options",
" ",
"     ^Tx  Toggle options. Toggle one option selected by x:",
"       a    Recalculate automatically or on ``@'' commands.",
"       o    Optimize expressions upon entry if enabled.",
"       c    Current cell highlighting enable/disable.",
"       e    External function execution enable/disable.",
"       l    Autolabeling defined cells enable/disable.",
"       n    If enabled, a digit starts a numeric value.",
"       p    Spreadsheet protection mode.",
"       t    Top line display enable/disable.",
#ifndef NOCRYPT
"       x    Encrypt/decrypt database and listing files.",
#else
"       x    Encrypt/decrypt database and listing files (Not available).",
#endif
"       $    Dollar prescale.  If enabled, all numeric constants",
"            (not expressions) entered are multipled by 0.01.",
"       r    Newline action.  Toggle between no action, move down",
"            after entry and move right after entry.",
"       z    Set the newline action limits to the current row and column",
"            (for r && z see also set rowlimit=n, collimit=n)",
NULL
};

static const char * const setoptions[] = {
" C: Set Options",
" ",
"     S  Set options.  Options include:",
"          autocalc      Automatic recalculation.",
"          byrows        Recalculate in row order. (default)",
"          bycols        Recalculate in column order.",
"          optimize      Optimize expressions upon entry. (default off)",
"          iterations=n  Set the number of iterations allowed. (10)",
"          tblstyle=xx   Set ``T'' output style to:",
"                        0 (none), tex, latex, slatex, or tbl.",
"          rndtoeven     Round *.5 to nearest even number instead of",
"                        always up.",
"          rowlimit=n    Set the remembered row limit for newline action.",
"          collimit=n    Set the remembered column limit for newline action.",
"                        (rowlimit and collimit can both be set by ^Tz)",
NULL
};

static const char * const cursor[] = {
" D: Cell cursor movement (always OK):",
" ",
"     ^N ^P      Down, up",
"     j k l h    Down, up, right, left",
"     Arrow keys (if the terminal and termcap support them.)",
"     ^Ed        Go to end of range.  Follow ^E by a direction indicator",
"                such as ^P or j.",
" ",
"     J K L H    Down, up, right, left by 1/2 pages (or 'pagesize' rows)",
"     SPACE      Forward",
"     ^H         Back",
"     TAB        Forward, otherwise starts/ends a range",
"     ^          Up to row 0 of the current column.",
"     #          Down to the last valid row of the current column.",
"     0          Back to column A.  Preface with ^U if numeric mode.",
"     $          Forward to the last valid column of the current row.",
"     b          Back then up to the previous valid cell.",
"     w          Forward then down to the next valid cell.",
"     g          Go to a cell.  Cell name, range name, quoted string,",
"                a number, 'error', or 'invalid' to specify which cell.",
"     ` '        Go to a marked cell (see help screen e for more info.",
NULL
};

static const char * const cell[] = {
" E: Cell entry and editing commands:",
" ",
"     =    Enter a numeric constant or expression.",
"     <    Enter a left justified string or string expression.",
"     \\    Enter a centered label.",
"     >    Enter a right justified string or string expression.",
"     e    Edit the current cell's numeric value.",
"     E    Edit the current cell's string part.",
"     F    Assign a format to the current cell's numeric value.",
"     x    Clear the current cell.",
"     m    Followed by any lowercase letter, marks the current cell",
"          with that letter.",
"     c    Copy a marked cell to the current cell.",
"     +    Increment numeric value",
"     -    Decrement numeric value",
"  RETURN  Enter insert mode if the input line was empty (ESC to edit)",
" ",
"     In numeric mode, a decimal digit, ``+'', ``-'', and ``.'' all start",
"     a new numeric constant or expression.",
NULL
};

static const char * const vi[] = {
" F: Line Editor",
" ",
"     Hitting the ESC key while entering any command on the top line",
"     will start a one-line vi-style editor.  Supported commands:",
" ",
"     ESC q        Abort command entry.",
"     h l          Move cursor forward, backward.",
"     0 $          Move cursor to the beginning, end of the line.",
"     b w          Move cursor forward/back one word.",
"     e            Move cursor to next end-of-word.",
"     fc           Move cursor to character c.",
"     tc           Move the cursor to the character before c.",
"     i a          Enter insert mode before/after the cursor.",
"     I A          Move to column 0/end of line and enter insert mode.",
"     x X          Delete the character under/before the cursor.",
"     rc           Replace the character under the cursor with c.",
"     cm dm        Change/Delete - m = b,e,f,h,l,t or w.",
"     R            Enter replace (overstrike) mode.",
"     s            Delete character under cursor and enter insert mode.",
"     + j - k / ?  Forward/backward/search the command history.",
"     n N          Repeat last history search (N = opposite direction).",
"     . u          Repeat/undo the last command.",
"     ^V           Enter navigate mode.  Another ^V enters current cell address.",
"     ^W           Type, in the command line, the current cell's expression.",
"     ^A           Type, in the command line, the current cell's numeric value.",
NULL
};

static const char * const file[] = {
" G: File commands:",
" ",
"     G    Get a new database from a file. ",
"     M    Merge a new file into the current database.",
"     P    Put the current database into a file.",
"     W    Write a listing of the current database into a file in",
"          a form that matches its appearance on the screen.",
"     T    Write a listing of the current database to a file, but",
"          put delimiters between each pair of fields.",
"          Optionally brackets output with control lines for ``tbl'',",
"          ``LaTeX'', ``SLaTex'', or ``TeX''.",
" ",
#ifndef NOCRYPT
"     If encryption mode is set, file I/O will be encrypted/decrypted.",
"     ``\"| program\"'' for a file name will pipe (unencrypted) output to",
#else
"     ``\"| program\"'' for a file name will pipe output to",
#endif
"     a program for Put, Write and Table.  If a cell name is used",
"     as the file name, the cell's string part will be used as the",
"     file name.",
NULL
};

static const char * const row[] = {
" H: Row and column commands:",
" ",
"     ir, ic      Insert a new, empty row (column)",
"     ar, ac      Append a new copy of the current row (column)",
"     dr, dc      Delete the current row (column)",
"     pr, pc, pm  Pull deleted cells back into the spreadsheet",
"                 Insert rows, columns or merge the cells.",
"     vr, vc      Remove expressions from the affected rows (columns),",
"                 leaving only the values.",
"     Zr, Zc      Hide (``zap'') the current row (column)",
"     sr, sc      Show hidden rows (columns)",
"     f           Set the output format to be used with the values of",
"                 each cell in this column.  Enter field width and",
"                 number of fractional digits.  A preceding count can be",
"                 used to change more than one column.",
" ",
"     Commands which move or copy cells also modify the row and column ",
"     references in the new cell expressions.  Use the ``$'' style cell ",
"     reference to suppress the change.",
" ",
"     @myrow, @mycol        return the row or column of the current cell",
"     @lastrow, @lastcol    return the row or column of the current cell",
NULL
};

static const char * const range[] = {
" I: Range commands:",
" ",
"     rx   Clear a range. ",
"     rv   Remove the expressions from a range of cells, leaving ",
"          just the values.",
"     rc   Copy a source range to a destination range.",
"     rf   Fill a range with constant values starting with a given",
"          value and increasing by a given increment.",
"     rd   Assign a name to a cell or a range of cells.  Give the",
"          the name, surrounded by quotes, and either a cell name such",
"          as ``A10'' or a range such as ``a1:b20''.",
"     rl   Locks a cell or a range of cells, i.e makes it unchangeable.",
"     rU   Unlocks a locked cell, i.e makes it changeable.",
"     rs   Shows the currently defined range names.  Pipe output to",
"          sort, then to less.",
"     ru   Use this command to undefine a previously defined range name.",
"     rF   Assign a format string to a range of cells.",
" ",
"     Range operations affect a rectangular region on the screen",
"     defined by the upper left and lower right cells in the region.",
"     A range is specified by giving the cell names separated by ``:'',",
"     such as ``a20:k52''.  Another way to refer to a range is to use",
"     a name previously defined using ``rd''.",
NULL
};

static const char * const misc[] = {
" J: Miscellaneous commands:",
" ",
"     Q q ^C   Exit from the program.",
"     ^G ESC   Abort entry of the current command.",
"     ?        Help",
"     !        Shell escape.  Enter a command to run.  ``!!'' repeats",
"              the last command.  Just ``!'' starts an interactive shell.",
"     ^L       Redraw the screen.",
"     ^R       Redraw the screen.  Highlight cells with values but no",
"              expressions.",
"     ^X       Redraw the screen.  Show formulas, not values.",
"     C        Redraw the screen with the row containing the current cell",
"              centered.",
"     @        Recalculate the spreadsheet.",
"     TAB      When the character cursor is on the top line TAB can be used",
"              to start or stop the display of the default range.",
NULL
};

static const char * const var[] = {
" K: Variable names:",
" ",
"     K20    Row and column can vary on copies.",
"     $K$20  Row and column stay fixed on copies.",
"     $K20   Row can vary; column stays fixed on copies.",
"     K$20   Row stays fixed; column can vary on copies.",
"     Cells and ranges can be given a symbolic name via ``rd''.",
" ",
" Expressions:",
"     -e      Negation                e<=e  Less than or equal",
"     e+e     Addition                e=e   Equal",
"     e-e     Subtraction             e!=e  Not Equal",
"     e*e     Multiplication          e>=e  Greater than or equal",
"     e/e     Division                e>e  Greater than",
"     e%e     Modulo                  e<e  Less than",
"     e^e     Exponentiation          e&e  Boolean operator AND.",
"     ~e      Boolean operator NOT    e|e  Boolean operator OR",
"     e?e1:e2  or @if(e,e1,e2)",
"             Conditional: If e is non zero then then e1, else e2.",
"     Terms may be constants, variables, and parenthesized expressions.",
NULL
};

static const char * const rangef[] = {
" L: Range functions:",
" ",
"     @sum(r)           Sum all valid cells in the range.",
"     @product(r)       Multiply together all valid cells in the range.",
"     @avg(r)           Average all valid cells in the range.",
"     @count(r)         Count all valid cells in the range.",
"     @max(r)           Return the maximum value in the range.",
"     @min(r)           Return the minimum value in the range.",
"     @stdev(r)         Return the sample standard deviation of ",
"                       the cells in the range.",
"     @index(e,r) @stindex(e,r)",
"                       Return the numeric (string) value of the cell at",
"                       index e into range r.",
"     @lookup(e,r) @hlookup(e,r,n) @vlookup(e,r,n)",
"                       Search through the range r for a value that",
"                       matches e.  If e is numeric, the last value <= e",
"                       matches; if string, an exact match is required.",
"                       @lookup searches a single row (column) and returns",
"                       the value from the next column (row); @hlookup",
"                       (@vlookup) searches the first row (column) in r and",
"                       returns the value n columns (rows) from the match.",
NULL
};

static const char * const numericf[] = {
" M: Numeric functions:",
" ",
"     @atan2(e1,e2)     Arc tangent of e1/e2.",
"     @ceiling(e)       Smallest integer not less than e.",
"     @exp(e)           Exponential function of e.",
"     @abs(e)           Absolute value of e.",
"     @floor(e)         The largest integer not greater than e.",
"     @hypot(x,y)       Sqrt(x*x+y*y).",
"     @max(e1,e2,...)   The maximum of the values of the e's.",
"     @min(e1,e2,...)   The minimum of the values of the e's",
"     @nval(se,e)       The numeric value of a named cell.",
"     @pi               A constant quite close to pi.",
"     @power(e1,e2)     e1 raised to the power of e2.",
"     @rnd(e)           Round e to the nearest integer.",
"     @round(e,n)       Round e to n decimal places.",
"     @sqrt(e)          Square root of e.",
"     @ston(se)         Convert string expr se to a numeric",
"     @ln(e)   @log(e)           Natural/base 10 logarithm of e.",
"     @radians(e)  @degrees(e)   Convert degrees to/from radians.",
"     @cos(e)  @sin(e)  @tan(e)  Trig functions of radian arguments.",
"     @asin(e) @acos(e) @atan(e) Inverse trig function.",
NULL
};

static const char * const stringf[] = {
" N: String functions:",
" ",
"     #                 Concatenate strings.  For example, the",
"                       string expression ``A0 # \"zy dog\"'' yields",
"                       ``the lazy dog'' if A0 is ``the la''.",
"     @substr(se,e1,e2) Extract characters e1 through e2 from the",
"                       string expression se.  For example,",
"                       ``@substr(\"Nice jacket\" 4, 7)'' yields ",
"                       ``e ja''.",
"     @fmt(se,e)        Convert a number to a string using sprintf(3).",
"                       For example,  ``@fmt(\"*%6.3f*\",10.5)'' yields",
"                       ``*10.500*''.  Use formats are e, E, f, g, and G.",
"     @sval(se,e)       Return the string value of a cell selected by name.",
"     @ext(se,e)        Call an external function (program or",
"                       script).  Convert e to a string and append it",
"                       to the command line as an argument.  @ext yields",
"                       a string: the first line printed to standard",
"                       output by the command.",
"     @coltoa(e)        Return the column letter(s) from the passed number",
"     @upper(e) @lower(e)   Return the string in upper/lower case",
"     @proper(e)        Return the string with words in upper case",
"     String expressions are made up of constant strings (characters",
"     surrounded by quotes), variables, and string functions.",
NULL
};

static const char * const finf[] = {
" O: Financial functions:",
" ",
"     @pmt(e1,e2,e3)    @pmt(60000,.01,360) computes the monthly",
"                       payments for a $60000 mortgage at 12%",
"                       annual interest (.01 per month) for 30",
"                       years (360 months).",
" ",
"     @fv(e1,e2,e3)     @fv(100,.005,36) computes the future value",
"                       of 36 monthly payments of $100 at 6%",
"                       interest (.005 per month).  It answers the",
"                       question:  ``How much will I have in 36",
"                       months if I deposit $100 per month in a",
"                       savings account paying 6% interest com-",
"                       pounded monthly?''",
" ",
"     @pv(e1,e2,e3)     @pv(1000,.015,36) computes the present",
"                       value of an ordinary annuity of 36",
"                       monthly payments of $1000 at 18% annual",
"                       interest.  It answers the question: ``How",
"                       much can I borrow at 18% for 30 years if I",
"                       pay $1000 per month?''",
NULL
};

static const char * const timef[] = {
" P: Time and date functions:",
" ",
"     @now              Return the time encoded in seconds since 1970.",
"     @date(e1,e2,e3)   Return e1/e2/e3 encoded in seconds since 1970.",
"                       Either m/d/y or y/m/d may be used (year must",
"                       include the century), although y/m/d is preferred.",
"     @time(h,m,s)      Return h:m:s encoded in seconds since midnight.",
" ",
"     All of the following take an argument expressed in seconds:",
" ",
"     @datefmt(e)       Convert the time in seconds to a date",
"                       string 24 characters long in the following",
"                       form: ``Sun Sep 16 01:03:52 1973''.  Note",
"                       that you can extract pieces of this fixed format",
"                       string with @substr.",
"     @year(e)          Return the year.  Valid years begin with 1970.",
"     @month(e)         Return the month: 1 (Jan) to 12 (Dec).",
"     @day(e)           Return the day of the month: 1 to 31.",
"     @hour(e)          Return the number of hours since midnight: 0 to 23.",
"     @minute(e)        Return the number of minutes since the",
"                       last full hour: 0 to 59.",
"     @second(e)        Return the number of seconds since the",
"                       last full minute: 0 to 59.",
NULL
};

static const char * const * const pages[] = {
    intro, toggleoptions, setoptions, cursor, cell, vi,
    file, row, range, misc, var, rangef, numericf, stringf,
    finf, timef, NULL
};

#ifndef QREF

void help(int ctx) {
    int history[32];
    int pos = 0;
    int page = ctx;

    for (;;) {
        if (page < 0 || page >= HELP_NB)
            page = HELP_INTRO;
        history[pos] = page;
        screen_erase();
        screen_draw_page(2, 4, pages[page]);
        screen_draw_line(0, 0, "Which Screen? [a-p, q]");
        screen_refresh();
        switch (nmgetch(0)) {
        case SC_KEY_F(1):
        case SC_KEY_HELP:
        case '?':
        case 'a': case 'A': page = HELP_INTRO; break;
        case 'b': case 'B': page = HELP_TOGGLEOPTIONS; break;
        case 'c': case 'C': page = HELP_SETOPTIONS; break;
        case 'd': case 'D': page = HELP_CURSOR; break;
        case 'e': case 'E': page = HELP_CELL; break;
        case 'f': case 'F': page = HELP_VI; break;
        case 'g': case 'G': page = HELP_FILE; break;
        case 'h': case 'H': page = HELP_ROW; break;
        case 'i': case 'I': page = HELP_RANGE; break;
        case 'j': case 'J': page = HELP_MISC; break;
        case 'k': case 'K': page = HELP_VAR; break;
        case 'l': case 'L': page = HELP_RANGEF; break;
        case 'm': case 'M': page = HELP_NUMERICF; break;
        case 'n': case 'N': page = HELP_STRINGF; break;
        case 'o': case 'O': page = HELP_FINF; break;
        case 'p': case 'P': page = HELP_TIMEF; break;

        case ESC:
        case ctl('g'):
        case 'q': case 'Q': page = -1; break;

        case ' ':
        case ctl('f'):
        case ctl('n'):
        case SC_KEY_RIGHT:
        case SC_KEY_DOWN:   if (page < HELP_NB) page++; break;

        case ctl('h'):
        case DEL:
        case ctl('b'):
        case ctl('p'):
        case SC_KEY_LEFT:
        case SC_KEY_UP:     if (pos > 0) page = history[--pos];
                            else page = HELP_INTRO; break;

        default:            break;
        }
        if (page < 0)
            break;
        if (page != history[pos]) {
            if (pos == (int)(sizeof(history) / sizeof(*history)) - 1)
                memmove(history, history + 1, sizeof(history) - sizeof(*history));
            else
                pos++;
        }
    }
    FullUpdate++;
    screen_erase();
}

#else /* QREF */

int main(void) {
#ifdef TROFF
    const char * const *page;
    const char *p;
    int lineno, pageno = 0;

    puts(".nr PS 12");
    puts(".nr VS 14");
    puts(".nr HM 1i");
    puts(".nr FM 1i");
    puts(".nr PO 0.5i");
    printf(".EH '%s%s''%s'\n", SCNAME, header, revision);
    printf(".OH '%s%s''%s'\n", SCNAME, header, revision);
    puts(".EF ''%''");
    puts(".OF ''%''");
    puts(".de Lp");
    puts(".LP");
    puts(".ft CW");
    puts(".na");
    puts(".nf");
    puts("..");
    puts(".P1");
    puts(".LP");

    while ((page = pages[pageno++])) {
        puts(" ");
        puts(".SH");
        for (lineno = 0; page[lineno]; lineno++) {
            if (lineno == 2)
                puts(".Lp");
            /* output contents, quoting backslashes */
            for (p = page[lineno]; *p; p++) {
                if (*p == '\\')
                    putchar('\\');
                putchar(*p);
            }
            putchar('\n');
        }
        if (!(pageno % 2))
            puts(".bp");
    }
#else /* TROFF */
    const char * const *page;
    int lineno, pageno = 0;

    while ((page = pages[pageno++])) {
        fputs(SCNAME, stdout);
        fputs(header, stdout);
        puts("");
        puts(revision);
        puts("");

        for (lineno = 0; page[lineno]; lineno++) {
            puts(page[lineno]);
        }
        putchar('\f');
    }
#endif /* TROFF */
    return 0;
}
#endif /* QREF */
