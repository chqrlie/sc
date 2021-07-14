/*      SC      A Spreadsheet Calculator
 *              Command and expression parser
 *
 *              original by James Gosling, September 1982
 *              modified by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              more mods Robert Bond 12/86
 *              More mods by Alan Silverstein, 3/88, see list of changes.
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

%{

#include "sc.h"

#if defined FREEBSD || defined DARWIN
extern int yydebug;
extern int yynerrs;
extern int yyerrflag;
extern int yychar;
//extern union YYSTYPE yyval;
//extern union YYSTYPE yylval;
#endif

static void doerror(SCXMEM string_t *s) {
    error("%s", s2c(s));
    free_string(s);
}

static int doreadfile(SCXMEM string_t *fname, int eraseflg) {
    int ret = readfile(s2c(fname), eraseflg);
    free_string(fname);
    return ret;
}

static int dowritefile(SCXMEM string_t *fname, rangeref_t rr, int dcp_flags) {
    int ret = writefile(s2c(fname), rr, dcp_flags);
    free_string(fname);
    return ret;
}

static int string_to_char(SCXMEM string_t *str) {
    int c = -1;
    if (str) {
        c = *s2c(str);
        free_string(str);
    }
    return c;
}

static SCXMEM string_t *get_strarg(cellref_t cr) {
    struct ent *p = lookat_nc(cr.row, cr.col);
    if (p && p->label) {
        return dup_string(p->label);
    } else {
        // XXX: should convert numeric value to string according to format?
        return new_string("NULL_STRING");
    }
}

%}

%union {
    int ival;
    double fval;
    SCXMEM string_t *sval;
    SCXMEM struct enode *enode;
    struct cellref cval;
    struct rangeref rval;
}

%type <cval> var
%type <fval> num
%type <rval> range
%type <rval> var_or_range
%type <sval> strarg
%type <enode> e term expr_list
%type <ival> noval
%token <sval> STRING
%token <ival> NUMBER
%token <fval> FNUMBER
%token <rval> RANGE
%token <cval> VAR
%token <sval> WORD
%token <sval> PLUGIN
%token <ival> COL

/*
 *  When adding new commands, make sure that any commands that may take
 *  COL as an argument precede S_FORMAT in the %token list.  All other
 *  commands must come after S_FORMAT.  This is necessary so that range
 *  names can be less than three letters without being parsed as column
 *  names.
 */

%token S_SHOW
%token S_HIDE
%token S_INSERTCOL
%token S_OPENCOL
%token S_DELETECOL
%token S_YANKCOL
%token S_GETFORMAT
%token S_FORMAT

%token S_FMT
%token S_LET
%token S_LABEL
%token S_LEFTSTRING
%token S_RIGHTSTRING
%token S_LEFTJUSTIFY
%token S_RIGHTJUSTIFY
%token S_CENTER
%token S_COLOR
%token S_ADDNOTE
%token S_DELNOTE
%token S_GET
%token S_PUT
%token S_MERGE
%token S_WRITE
%token S_TBL
%token S_COPY
%token S_MOVE
%token S_ERASE
%token S_YANK
%token S_FILL
%token S_SORT
%token S_LOCK
%token S_UNLOCK
%token S_GOTO
%token S_DEFINE
%token S_UNDEFINE
%token S_ABBREV
%token S_UNABBREV
%token S_FRAME
%token S_FRAMETOP
%token S_FRAMEBOTTOM
%token S_FRAMELEFT
%token S_FRAMERIGHT
%token S_UNFRAME
%token S_VALUE
%token S_MDIR
%token S_AUTORUN
%token S_FKEY
%token S_HISTFILE
%token S_SCEXT
%token S_ASCEXT
%token S_TBL0EXT
%token S_TBLEXT
%token S_LATEXEXT
%token S_SLATEXEXT
%token S_TEXEXT
%token S_SET
%token S_UP
%token S_DOWN
%token S_LEFT
%token S_RIGHT
%token S_ENDUP
%token S_ENDDOWN
%token S_ENDLEFT
%token S_ENDRIGHT
%token S_SELECT
%token S_INSERTROW
%token S_OPENROW
%token S_DELETEROW
%token S_YANKROW
%token S_PULL
%token S_PULLMERGE
%token S_PULLROWS
%token S_PULLCOLS
%token S_PULLXCHG
%token S_PULLTP
%token S_PULLFMT
%token S_PULLCOPY
%token S_WHEREAMI
%token S_GETNUM
%token S_FGETNUM
%token S_GETSTRING
%token S_GETEXP
%token S_GETFMT
%token S_GETFRAME
%token S_GETRANGE
%token S_EVAL
%token S_SEVAL
%token S_QUERY
%token S_GETKEY
%token S_ERROR
%token S_RECALC
%token S_REDRAW
%token S_QUIT
%token S_STATUS
%token S_RUN
%token S_PLUGIN
%token S_PLUGOUT

/* goto keywords */
%token G_ERROR
%token G_INVALID

/* function names */
%token F_FIXED
%token F_SUM
%token F_PROD
%token F_AVG
%token F_STDDEV
%token F_COUNT
%token F_ROWS
%token F_COLS
%token F_ABS
%token F_ACOS
%token F_ASIN
%token F_ATAN
%token F_ATAN2
%token F_CEIL
%token F_COS
%token F_EXP
%token F_FABS
%token F_FLOOR
%token F_HYPOT
%token F_LN
%token F_LOG
%token F_PI
%token F_POW
%token F_SIN
%token F_SQRT
%token F_TAN
%token F_DTR
%token F_RTD
%token F_MAX
%token F_MIN
%token F_RAND
%token F_RANDBETWEEN
%token F_RND
%token F_ROUND
%token F_IF

%token F_PV
%token F_FV
%token F_PMT

%token F_HOUR
%token F_MINUTE
%token F_SECOND
%token F_MONTH
%token F_DAY
%token F_YEAR
%token F_NOW
%token F_DATE
%token F_DTS
%token F_TTS
%token F_FMT
%token F_SUBSTR
%token F_UPPER
%token F_LOWER
%token F_CAPITAL
%token F_STON
%token F_EQS
%token F_EXT
%token F_NVAL
%token F_SVAL
%token F_LOOKUP
%token F_HLOOKUP
%token F_VLOOKUP
%token F_INDEX
%token F_STINDEX
%token F_BLACK
%token F_RED
%token F_GREEN
%token F_YELLOW
%token F_BLUE
%token F_MAGENTA
%token F_CYAN
%token F_WHITE
%token F_FILENAME
%token F_MYROW
%token F_MYCOL
%token F_LASTROW
%token F_LASTCOL
%token F_COLTOA
%token F_NUMITER
%token F_ERR

/* setting names */
%token K_AUTO
%token K_AUTOCALC
%token K_AUTOINSERT
%token K_AUTOWRAP
%token K_CSLOP
%token K_BYROWS
%token K_BYCOLS
%token K_OPTIMIZE
%token K_ITERATIONS
%token K_NUMERIC
%token K_PRESCALE
%token K_EXTFUN
%token K_CELLCUR
%token K_TOPROW
%token K_COLOR
%token K_COLORNEG
%token K_COLORERR
%token K_BRAILLE
%token K_BACKUP
%token K_MOUSE
%token K_TBLSTYLE
%token K_TBL
%token K_LATEX
%token K_SLATEX
%token K_TEX
%token K_FRAME
%token K_RNDTOEVEN
%token K_CRACTION
%token K_CRROW
%token K_CRCOL
%token K_ROWLIMIT
%token K_COLLIMIT
%token K_PAGESIZE
%token K_NUMITER
%token K_SCRC
%token K_LOCALE
%token K_EMACS

%token <ival> '+' '-' '*' '/' '%' '^'

%right ';'
%left '?' ':'
%left '|'
%left '&'
%nonassoc '<' '=' '>' '!'
%left '+' '-' '#'
%left '*' '/' '%'
%left '^'

%%

command:  S_LET var_or_range '=' e      { let($2.left, $4); }
        | S_LET var_or_range '='        { unlet($2.left); }
        | S_LABEL var_or_range '=' e    { slet($2.left, $4, ALIGN_CENTER); }
        | S_LEFTSTRING var_or_range '=' e   { slet($2.left, $4, ALIGN_LEFT); }
        | S_RIGHTSTRING var_or_range '=' e  { slet($2.left, $4, ALIGN_RIGHT); }
        | S_LEFTJUSTIFY var_or_range    { range_align($2, ALIGN_LEFT); }
        | S_LEFTJUSTIFY                 { range_align(rangeref_current(), ALIGN_LEFT); }
        | S_RIGHTJUSTIFY var_or_range   { range_align($2, ALIGN_RIGHT); }
        | S_RIGHTJUSTIFY                { range_align(rangeref_current(), ALIGN_RIGHT); }
        | S_CENTER var_or_range         { range_align($2, ALIGN_CENTER); }
        | S_CENTER                      { range_align(rangeref_current(), ALIGN_CENTER); }
        | S_ADDNOTE var                 { addnote($2, rangeref_current()); }
        | S_ADDNOTE var var_or_range    { addnote($2, $3); }
        | S_DELNOTE var                 { delnote($2); }
        | S_DELNOTE                     { delnote(cellref_current()); }
        | S_FORMAT COL ':' COL NUMBER NUMBER NUMBER  { cmd_format($2, $4, $5, $6, $7); }
        | S_FORMAT COL NUMBER NUMBER NUMBER  { cmd_format($2, $2, $3, $4, $5); }
        | S_FORMAT COL ':' COL NUMBER NUMBER  { cmd_format($2, $4, $5, $6, REFMTFIX); }
        | S_FORMAT COL NUMBER NUMBER    { cmd_format($2, $2, $3, $4, REFMTFIX); }
        | S_FORMAT NUMBER '=' STRING    { cmd_setformat($2, $4); }
        | S_GET strarg                  { doreadfile($2, 1); }
        | S_MERGE strarg                { doreadfile($2, 0); }
        | S_MDIR strarg                 { set_mdir($2); }
        | S_AUTORUN strarg              { set_autorun($2); }
        | S_FKEY NUMBER '=' strarg      { set_fkey($2, $4); }
        | S_HISTFILE strarg             { set_histfile($2); }
        | S_SCEXT strarg                { set_string(&scext, $2); }
        | S_ASCEXT strarg               { set_string(&ascext, $2); }
        | S_TBL0EXT strarg              { set_string(&tbl0ext, $2); }
        | S_TBLEXT strarg               { set_string(&tblext, $2); }
        | S_LATEXEXT strarg             { set_string(&latexext, $2); }
        | S_SLATEXEXT strarg            { set_string(&slatexext, $2); }
        | S_TEXEXT strarg               { set_string(&texext, $2); }
        | S_PUT strarg range noval      { dowritefile($2, $3, $4); }
        | S_PUT strarg noval            { dowritefile($2, rangeref_total(), $3); }
        | S_PUT range noval             { write_cells(stdout, $2, $2.left, $3 | DCP_NO_NAME); }
        | S_PUT range '/' var noval     { write_cells(stdout, $2, $4, $5 | DCP_NO_NAME); }
        | S_PUT '%' '/' var noval       { write_cells(stdout, rangeref_total(), $4, $5 | DCP_NO_NAME); }
        | S_PUT '/' var noval           { write_cells(stdout, rangeref_current(), $3, $4 | DCP_NO_NAME); }
        | S_PUT '%' noval               { write_cells(stdout, rangeref_total(), cellref(0, 0), $3 | DCP_NO_NAME); }
        | S_PUT noval                   { write_cells(stdout, rangeref_total(), cellref(0, 0), $2 | DCP_NO_NAME); }
        | S_WRITE strarg range          { printfile($2, $3); }
        | S_WRITE strarg                { printfile($2, rangeref_total()); }
        | S_WRITE range                 { printfile(NULL, $2); }
        | S_WRITE '%'                   { printfile(NULL, rangeref_total()); }
        | S_WRITE                       { printfile(NULL, rangeref_total()); }
        | S_TBL strarg range            { tblprintfile($2, $3); }
        | S_TBL strarg                  { tblprintfile($2, rangeref_total()); }
        | S_SHOW COL ':' COL            { showcol($2, $4); }
        | S_SHOW NUMBER ':' NUMBER      { showrow($2, $4); }
        | S_HIDE                        { dohide(); }
        | S_HIDE COL                    { hidecols($2, $2); }
        | S_HIDE COL ':' COL            { hidecols($2, $4); }
        | S_HIDE NUMBER                 { hiderows($2, $2); }
        | S_HIDE NUMBER ':' NUMBER      { hiderows($2, $4); }
        | S_COPY                        { copy(COPY_FROM_DEF, rangeref_current(), rangeref_empty()); }
        | S_COPY range                  { copy(COPY_FROM_DEF, $2, rangeref_empty()); }
        | S_COPY range var_or_range     { copy(COPY_FROM_RANGE, $2, $3); }
        | S_MOVE var                    { mover($2, rangeref_current()); }
        | S_MOVE var var_or_range       { mover($2, $3); }
        | S_ERASE                       { eraser(rangeref_current()); }
        | S_ERASE var_or_range          { eraser($2); }
        | S_YANK                        { yankr(rangeref_current()); }
        | S_YANK var_or_range           { yankr($2); }
        | S_VALUE                       { valueize_area(rangeref_current()); }
        | S_VALUE var_or_range          { valueize_area($2); }
        | S_FILL var_or_range num num   { fillr($2, $3, $4, calc_order == BYCOLS); }
        | S_SORT                        { sortrange(rangeref_current(), NULL); }
        | S_SORT range                  { sortrange($2, NULL); }
        | S_SORT range strarg           { sortrange($2, $3); }
        | S_FMT var_or_range STRING     { format_cells($2, $3); }
        | S_LOCK                        { lock_cells(rangeref_current()); }
        | S_LOCK var_or_range           { lock_cells($2); }
        | S_UNLOCK                      { unlock_cells(rangeref_current()); }
        | S_UNLOCK var_or_range         { unlock_cells($2); }
        | S_GOTO var_or_range var_or_range { moveto($2, $3.left); }
        | S_GOTO var_or_range           { moveto($2, cellref(-1, -1)); }
        | S_GOTO num range              { num_search($2, $3, 0); }
        | S_GOTO num                    { num_search($2, rangeref_total(), 0); }
        | S_GOTO errlist                { /* code is executed in errlist rules */ }
        | S_GOTO STRING range           { str_search($2, $3, 0); }
        | S_GOTO '#' STRING range       { str_search($3, $4, 1); }
        | S_GOTO '%' STRING range       { str_search($3, $4, 2); }
        | S_GOTO STRING                 { str_search($2, rangeref_total(), 0); }
        | S_GOTO '#' STRING             { str_search($3, rangeref_total(), 1); }
        | S_GOTO '%' STRING             { str_search($3, rangeref_total(), 2); }
        | S_GOTO                        { go_last(); }
        | S_GOTO WORD                   { /* don't repeat last goto on "unintelligible word" */ }
        | S_DEFINE strarg               { add_nrange($2, rangeref_current(), -1); }
        | S_DEFINE strarg range         { add_nrange($2, $3, 1); }  // XXX: why distinguish this way?
        | S_DEFINE strarg var           { add_nrange($2, rangeref2($3, $3), 0); }
        | S_UNDEFINE var_or_range       { del_nrange($2); }
        | S_ABBREV STRING STRING        { add_abbr($2, $3); }
        | S_ABBREV STRING               { add_abbr($2, NULL); }
        | S_ABBREV                      { add_abbr(NULL, NULL); }
        | S_UNABBREV STRING             { del_abbr($2); }
        | S_FRAME range range           { add_frange(FRANGE_DIRECT | FRANGE_INNER,
                                                     $2, $3, 0, 0, 0, 0); }
        | S_FRAME range                 { if (showrange) {
                                            add_frange(FRANGE_DIRECT | FRANGE_INNER,
                                                       $2, rangeref_current(), 0, 0, 0, 0);
                                          } else {
                                            add_frange(FRANGE_FIND | FRANGE_INNER,
                                                       rangeref_current(),
                                                       $2, 0, 0, 0, 0);
                                          }
                                        }
        | S_FRAME                       { if (showrange && get_current_frange()) {
                                            add_frange(FRANGE_FIND | FRANGE_INNER,
                                                       rangeref(currow, curcol, currow, curcol),
                                                       rangeref_current(), 0, 0, 0, 0);
                                          } else {
                                              error("Need both outer and inner"
                                                    " ranges to create frame");
                                          }
                                        }
        | S_FRAMETOP range NUMBER       { add_frange(FRANGE_DIRECT,
                                                     $2, rangeref_empty(), $3, -1, -1, -1); }
        | S_FRAMETOP NUMBER             { add_frange(FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), $2, -1, -1, -1); }
        | S_FRAMEBOTTOM range NUMBER    { add_frange(FRANGE_DIRECT, $2,
                                                     rangeref_empty(), -1, $3, -1, -1); }
        | S_FRAMEBOTTOM NUMBER          { add_frange(FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), -1, $2, -1, -1); }
        | S_FRAMELEFT range NUMBER      { add_frange(FRANGE_DIRECT, $2,
                                                     rangeref_empty(), -1, -1, $3, -1); }
        | S_FRAMELEFT NUMBER            { add_frange(FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), -1, -1, $2, -1); }
        | S_FRAMERIGHT range NUMBER     { add_frange(FRANGE_DIRECT, $2,
                                                     rangeref_empty(), -1, -1, -1, $3); }
        | S_FRAMERIGHT NUMBER           { add_frange(FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), -1, -1, -1, $2); }
        | S_UNFRAME range               { add_frange(FRANGE_DIRECT, $2,
                                                     rangeref_empty(), 0, 0, 0, 0); }
        | S_UNFRAME                     { add_frange(FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), 0, 0, 0, 0); }
        | S_COLOR NUMBER '='            { initcolor($2); }
        | S_COLOR NUMBER '=' e          { change_color($2, $4); }
        | S_COLOR range NUMBER          { add_crange($2, $3); }
        | S_SET setlist                 { modflg++; }
        | S_UP                          { backrow(1); }
        | S_UP NUMBER                   { backrow($2); }
        | S_DOWN                        { forwrow(1); }
        | S_DOWN NUMBER                 { forwrow($2); }
        | S_LEFT                        { backcol(1); }
        | S_LEFT NUMBER                 { backcol($2); }
        | S_RIGHT                       { forwcol(1); }
        | S_RIGHT NUMBER                { forwcol($2); }
        | S_ENDUP                       { doend(-1,  0); }
        | S_ENDDOWN                     { doend( 1,  0); }
        | S_ENDLEFT                     { doend( 0, -1); }
        | S_ENDRIGHT                    { doend( 0,  1); }
        | S_SELECT STRING               { cmd_select_qbuf(string_to_char($2)); }
        | S_INSERTROW                   { insertrow(cellref_current(),  1, 0); }
        | S_INSERTROW '*' NUMBER        { insertrow(cellref_current(), $3, 0); }
        | S_INSERTCOL                   { insertcol(cellref_current(),  1, 0); }
        | S_INSERTCOL '*' NUMBER        { insertcol(cellref_current(), $3, 0); }
        | S_OPENROW                     { currow += insertrow(cellref_current(),  1, 1); }
        | S_OPENROW '*' NUMBER          { currow += insertrow(cellref_current(), $3, 1); }
        | S_OPENCOL                     { curcol += insertcol(cellref_current(),  1, 1); }
        | S_OPENCOL '*' NUMBER          { curcol += insertcol(cellref_current(), $3, 1); }
        | S_DELETEROW                   { if (showrange == SHOWROWS)
                                            deletecols(showsr, currow);
                                          else
                                            deletecols(currow, currow);
                                        }
        | S_DELETEROW '*' NUMBER        { deleterows(currow, currow + $3 - 1); }
        | S_DELETEROW NUMBER            { deleterows($2, $2); }
        | S_DELETEROW NUMBER ':' NUMBER { deleterows($2, $4); }
        | S_DELETECOL                   { if (showrange == SHOWCOLS)
                                            deletecols(showsc, curcol);
                                          else
                                            deletecols(curcol, curcol);
                                        }
        | S_DELETECOL COL               { deletecols($2, $2); }
        | S_DELETECOL '*' NUMBER        { deletecols(curcol, curcol + $3 - 1); }
        | S_DELETECOL COL ':' COL       { deletecols($2, $4); }
        | S_YANKROW                     { if (showrange == SHOWROWS)
                                            yankrows(showsr, currow);
                                          else
                                            yankrows(currow, currow);
                                        }
        | S_YANKROW '*' NUMBER          { yankrows(currow, currow + $3 - 1); }
        | S_YANKROW NUMBER              { yankrows($2, $2); }
        | S_YANKROW NUMBER ':' NUMBER   { yankrows($2, $4); }
        | S_YANKCOL                     { if (showrange == SHOWCOLS)
                                            yankcols(showsc, curcol);
                                          else
                                            yankcols(curcol, curcol);
                                        }
        | S_YANKCOL NUMBER              { yankcols($2, $2); }
        | S_YANKCOL '*' NUMBER          { yankcols(curcol, curcol + $3 - 1); }
        | S_YANKCOL COL ':' COL         { yankcols($2, $4); }
        | S_PULL                        { pullcells('p', cellref_current()); }
        | S_PULLMERGE                   { pullcells('m', cellref_current()); }
        | S_PULLROWS                    { pullcells('r', cellref_current()); }
        | S_PULLCOLS                    { pullcells('c', cellref_current()); }
        | S_PULLXCHG                    { pullcells('x', cellref_current()); }
        | S_PULLTP                      { pullcells('t', cellref_current()); }
        | S_PULLFMT                     { pullcells('f', cellref_current()); }
        | S_PULLCOPY                    { copy(COPY_FROM_QBUF, rangeref_current(), rangeref_empty()); }
        | S_PULLCOPY var_or_range       { copy(COPY_FROM_QBUF, $2, rangeref_empty()); }
        | S_WHEREAMI                    { cmd_whereami(macrofd); }
        | S_WHEREAMI '|' NUMBER         { cmd_whereami($3); }
        | S_GETNUM var_or_range         { getnum($2, macrofd); }
        | S_GETNUM var_or_range '|' NUMBER  { getnum($2, $4); }
        | S_GETNUM                      { getnum(rangeref_current(), macrofd); }
        | S_GETNUM '|' NUMBER           { getnum(rangeref_current(), $3); }
        | S_FGETNUM var_or_range        { fgetnum($2, macrofd); }
        | S_FGETNUM var_or_range '|' NUMBER  { fgetnum($2, $4); }
        | S_FGETNUM                     { fgetnum(rangeref_current(), macrofd); }
        | S_FGETNUM '|' NUMBER          { fgetnum(rangeref_current(), $3); }
        | S_GETSTRING var_or_range      { getstring($2, macrofd); }
        | S_GETSTRING var_or_range '|' NUMBER  { getstring($2, $4); }
        | S_GETSTRING                   { getstring(rangeref_current(), macrofd); }
        | S_GETSTRING '|' NUMBER        { getstring(rangeref_current(), $3); }
        | S_GETEXP var_or_range         { getexp($2, macrofd); }
        | S_GETEXP var_or_range '|' NUMBER  { getexp($2, $4); }
        | S_GETEXP                      { getexp(rangeref_current(), macrofd); }
        | S_GETEXP '|' NUMBER           { getexp(rangeref_current(), $3); }
        | S_GETFORMAT COL               { getformat($2, macrofd); }
        | S_GETFORMAT COL '|' NUMBER    { getformat($2, $4); }
        | S_GETFORMAT                   { getformat(curcol, macrofd); }
        | S_GETFORMAT '|' NUMBER        { getformat(curcol, $3); }
        | S_GETFMT var_or_range         { getfmt($2, macrofd); }
        | S_GETFMT var_or_range '|' NUMBER  { getfmt($2, $4); }
        | S_GETFMT                      { getfmt(rangeref_current(), macrofd); }
        | S_GETFMT '|' NUMBER           { getfmt(rangeref_current(), $3); }
        | S_GETFRAME                    { getframe(macrofd); }
        | S_GETFRAME '|' NUMBER         { getframe($3); }
        | S_GETRANGE STRING             { getrange($2, macrofd); }
        | S_GETRANGE STRING '|' NUMBER  { getrange($2, $4); }
        | S_EVAL e                      { cmd_eval($2, NULL, currow, curcol, macrofd); }
        | S_EVAL e STRING               { cmd_eval($2, $3, currow, curcol, macrofd); }
        | S_EVAL e STRING '|' NUMBER    { cmd_eval($2, $3, currow, curcol, $5); }
        | S_SEVAL e                     { cmd_seval($2, currow, curcol, macrofd); }
        | S_QUERY STRING STRING         { cmd_query($2, $3, macrofd); }
        | S_QUERY STRING STRING '|' NUMBER  { cmd_query($2, $3, $5); }
        | S_QUERY STRING                { cmd_query($2, NULL, macrofd); }
        | S_QUERY STRING '|' NUMBER     { cmd_query($2, NULL, $4); }
        | S_QUERY                       { cmd_query(NULL, NULL, macrofd); }
        | S_QUERY '|' NUMBER            { cmd_query(NULL, NULL, $3); }
        | S_GETKEY                      { dogetkey(macrofd); }
        | S_ERROR STRING                { doerror($2); }
        | S_STATUS                      { cmd_status(macrofd); }
        | S_STATUS '|' NUMBER           { cmd_status($3); }
        | S_RECALC                      { cmd_recalc(); }
        | S_REDRAW                      { cmd_redraw(); }
        | S_QUIT                        { stopdisp(); exit(0); }
        | S_RUN STRING                  { cmd_run($2); }
        | S_PLUGIN STRING '=' STRING    { add_plugin($2, $4, 'r'); }
        | S_PLUGOUT STRING '=' STRING   { add_plugin($2, $4, 'w'); }
        | PLUGIN                        { cmd_plugin($1); }
        | /* nothing */
        | error
        ;

noval:                                  { $$ = DCP_DEFAULT; }
        | '*'                           { $$ = DCP_NO_EXPR; }
        ;

term:     var                           { $$ = new_var(O_VAR, $1); }
        | F_FIXED term                  { $$ = new_node('f', $2, NULL); }
        | '(' F_FIXED ')' term          { $$ = new_node('F', $4, NULL); }
        | F_SUM '(' var_or_range ')'    { $$ = new_range(SUM, $3, NULL); }
        | F_SUM '(' range ',' e ')'     { $$ = new_range(SUM, $3, $5); }
        | F_PROD '(' var_or_range ')'   { $$ = new_range(PROD, $3, NULL); }
        | F_PROD '(' range ',' e ')'    { $$ = new_range(PROD, $3, $5); }
        | F_AVG '(' var_or_range ')'    { $$ = new_range(AVG, $3, NULL); }
        | F_AVG '(' range ',' e ')'     { $$ = new_range(AVG, $3, $5); }
        | F_STDDEV '(' var_or_range ')' { $$ = new_range(STDDEV, $3, NULL); }
        | F_STDDEV '(' range ',' e ')'  { $$ = new_range(STDDEV, $3, $5); }
        | F_COUNT '(' var_or_range ')'  { $$ = new_range(COUNT, $3, NULL); }
        | F_COUNT '(' range ',' e ')'   { $$ = new_range(COUNT, $3, $5); }
        | F_MAX '(' var_or_range ')'    { $$ = new_range(MAX, $3, NULL); }
        | F_MAX '(' range ',' e ')'     { $$ = new_range(MAX, $3, $5); }
        | F_MAX '(' e ',' expr_list ')' { $$ = new_node(LMAX, $5, $3); }
        | F_MIN '(' var_or_range ')'    { $$ = new_range(MIN, $3, NULL); }
        | F_MIN '(' range ',' e ')'     { $$ = new_range(MIN, $3, $5); }
        | F_MIN '(' e ',' expr_list ')' { $$ = new_node(LMIN, $5, $3); }
        | F_ROWS '(' var_or_range ')'   { $$ = new_range('R', $3, NULL); }
        | F_COLS '(' var_or_range ')'   { $$ = new_range('C', $3, NULL); }

        | F_ABS '(' e ')'               { $$ = new_node(ABS, $3, NULL); }
        | F_ACOS '(' e ')'              { $$ = new_node(ACOS, $3, NULL); }
        | F_ASIN '(' e ')'              { $$ = new_node(ASIN, $3, NULL); }
        | F_ATAN '(' e ')'              { $$ = new_node(ATAN, $3, NULL); }
        | F_ATAN2 '(' e ',' e ')'       { $$ = new_node(ATAN2, $3, $5); }
        | F_CEIL '(' e ')'              { $$ = new_node(CEIL, $3, NULL); }
        | F_COS '(' e ')'               { $$ = new_node(COS, $3, NULL); }
        | F_EXP '(' e ')'               { $$ = new_node(EXP, $3, NULL); }
        | F_FABS '(' e ')'              { $$ = new_node(FABS, $3, NULL); }
        | F_FLOOR '(' e ')'             { $$ = new_node(FLOOR, $3, NULL); }
        | F_HYPOT '(' e ',' e ')'       { $$ = new_node(HYPOT, $3, $5); }
        | F_LN '(' e ')'                { $$ = new_node(LOG, $3, NULL); }
        | F_LOG '(' e ')'               { $$ = new_node(LOG10, $3, NULL); }
        | F_POW '(' e ',' e ')'         { $$ = new_node(POW, $3, $5); }
        | F_SIN '(' e ')'               { $$ = new_node(SIN, $3, NULL); }
        | F_SQRT '(' e ')'              { $$ = new_node(SQRT, $3, NULL); }
        | F_TAN '(' e ')'               { $$ = new_node(TAN, $3, NULL); }
        | F_DTR '(' e ')'               { $$ = new_node(DTR, $3, NULL); }
        | F_RTD '(' e ')'               { $$ = new_node(RTD, $3, NULL); }
        | F_RAND '(' ')'                { $$ = new_node(RAND, NULL, NULL); }
        | F_RANDBETWEEN '(' e ',' e ')' { $$ = new_node(RANDBETWEEN, $3, $5); }
        | F_RND '(' e ')'               { $$ = new_node(RND, $3, NULL); }
        | F_ROUND '(' e ',' e ')'       { $$ = new_node(ROUND, $3, $5); }

        | F_IF '(' e ',' e ',' e ')'    { $$ = new_node(IF, $3, new_node(',', $5, $7)); }

        | F_PV '(' e ',' e ',' e ')'    { $$ = new_node(PV, $3, new_node(':', $5, $7)); }
        | F_FV '(' e ',' e ',' e ')'    { $$ = new_node(FV, $3, new_node(':', $5, $7)); }
        | F_PMT '(' e ',' e ',' e ')'   { $$ = new_node(PMT, $3, new_node(':', $5, $7)); }

        | F_HOUR '(' e ')'              { $$ = new_node(HOUR, $3, NULL); }
        | F_MINUTE '(' e ')'            { $$ = new_node(MINUTE, $3, NULL); }
        | F_SECOND '(' e ')'            { $$ = new_node(SECOND, $3, NULL); }
        | F_MONTH '(' e ')'             { $$ = new_node(MONTH, $3, NULL); }
        | F_DAY '(' e ')'               { $$ = new_node(DAY, $3, NULL); }
        | F_YEAR '(' e ')'              { $$ = new_node(YEAR, $3, NULL); }
        | F_NOW                         { $$ = new_node(NOW, NULL, NULL); }
        | F_DTS '(' e ',' e ',' e ')'   { $$ = new_node(DTS, $3, new_node(',', $5, $7)); }
                                        // XXX: should return a specific token, same for x/x/x and x:x and x:x:x
        | NUMBER '.' NUMBER '.' NUMBER  { $$ = new_node(DTS,
                                                        new_const(O_CONST, (double)$1),
                                                        new_node(',', new_const(O_CONST, (double)$3),
                                                                 new_const(O_CONST, (double)$5))); }
        | F_TTS '(' e ',' e ',' e ')'   { $$ = new_node(TTS, $3, new_node(',', $5, $7)); }
        | F_STON '(' e ')'              { $$ = new_node(STON, $3, NULL); }
        | F_EQS '(' e ',' e ')'         { $$ = new_node(EQS, $3, $5); }
        | F_DATE '(' e ')'              { $$ = new_node(DATE, $3, NULL); }
        | F_DATE '(' e ',' e ')'        { $$ = new_node(DATE, $3, $5); }
        | F_FMT '(' e ',' e ')'         { $$ = new_node(FMT, $3, $5); }
        | F_UPPER '(' e ')'             { $$ = new_node(UPPER, $3, NULL); }
        | F_LOWER '(' e ')'             { $$ = new_node(LOWER, $3, NULL); }
        | F_CAPITAL '(' e ')'           { $$ = new_node(CAPITAL, $3, NULL); }
        | F_INDEX '(' range ',' e ')'   { $$ = new_range(INDEX, $3, $5); }
        | F_INDEX '(' e ',' range ')'   { $$ = new_range(INDEX, $5, $3); }
        | F_INDEX '(' range ',' e ',' e ')'  { $$ = new_range(INDEX, $3, new_node(',', $5, $7)); }
        | F_LOOKUP '(' range ',' e ')'  { $$ = new_range(LOOKUP, $3, $5); }
        | F_LOOKUP '(' e ',' range ')'  { $$ = new_range(LOOKUP, $5, $3); }
        | F_HLOOKUP '(' range ',' e ',' e ')'  { $$ = new_range(HLOOKUP, $3, new_node(',', $5, $7)); }
        | F_HLOOKUP '(' e ',' range ',' e ')'  { $$ = new_range(HLOOKUP, $5, new_node(',', $3, $7)); }
        | F_VLOOKUP '(' range ',' e ',' e ')'  { $$ = new_range(VLOOKUP, $3, new_node(',', $5, $7)); }
        | F_VLOOKUP '(' e ',' range ',' e ')'  { $$ = new_range(VLOOKUP, $5, new_node(',', $3, $7)); }
        | F_STINDEX '(' range ',' e ')' { $$ = new_range(STINDEX, $3, $5); }
        | F_STINDEX '(' e ',' range ')' { $$ = new_range(STINDEX, $5, $3); }
        | F_STINDEX '(' range ',' e ',' e ')'  { $$ = new_range(STINDEX, $3, new_node(',', $5, $7)); }
        | F_EXT '(' e ',' e ')'         { $$ = new_node(EXT, $3, $5); }
        | F_NVAL '(' e ',' e ')'        { $$ = new_node(NVAL, $3, $5); }
        | F_SVAL '(' e ',' e ')'        { $$ = new_node(SVAL, $3, $5); }
        | F_SUBSTR '(' e ',' e ',' e ')'  { $$ = new_node(SUBSTR, $3, new_node(',', $5, $7)); }
        |     '(' e ')'                 { $$ = $2; }
        |     '+' term                  { $$ = $2; }
        |     '-' term                  { $$ = new_node('m', $2, NULL); }
        |     NUMBER                    { $$ = new_const(O_CONST, (double)$1); }
        |     FNUMBER                   { $$ = new_const(O_CONST, $1); }
        | F_PI                          { $$ = new_node(PI_, NULL, NULL); }
        |     STRING                    { $$ = new_str($1); }
        |     '~' term                  { $$ = new_node('!', $2, NULL); }
        |     '!' term                  { $$ = new_node('!', $2, NULL); }
        | F_FILENAME '(' e ')'          { $$ = new_node(FILENAME, $3, NULL); }
        | F_MYROW                       { $$ = new_node(MYROW, NULL, NULL); }
        | F_MYCOL                       { $$ = new_node(MYCOL, NULL, NULL); }
        | F_LASTROW                     { $$ = new_node(LASTROW, NULL, NULL); }
        | F_LASTCOL                     { $$ = new_node(LASTCOL, NULL, NULL); }
        | F_COLTOA '(' e ')'            { $$ = new_node(COLTOA, $3, NULL); }
        | F_NUMITER                     { $$ = new_node(NUMITER, NULL, NULL); }
        | F_ERR                         { $$ = new_node(ERR_, NULL, NULL); } // XXX: should handle errors differently
        | F_BLACK                       { $$ = new_node(BLACK, NULL, NULL); }
        | F_RED                         { $$ = new_node(RED, NULL, NULL); }
        | F_GREEN                       { $$ = new_node(GREEN, NULL, NULL); }
        | F_YELLOW                      { $$ = new_node(YELLOW, NULL, NULL); }
        | F_BLUE                        { $$ = new_node(BLUE, NULL, NULL); }
        | F_MAGENTA                     { $$ = new_node(MAGENTA, NULL, NULL); }
        | F_CYAN                        { $$ = new_node(CYAN, NULL, NULL); }
        | F_WHITE                       { $$ = new_node(WHITE, NULL, NULL); }
        ;

/* expressions */
e:        e '+' e                   { $$ = new_node('+', $1, $3); }
        | e '-' e                   { $$ = new_node('-', $1, $3); }
        | e '*' e                   { $$ = new_node('*', $1, $3); }
        | e '/' e                   { $$ = new_node('/', $1, $3); }
        | e '%' e                   { $$ = new_node('%', $1, $3); }
        | e '^' e                   { $$ = new_node('^', $1, $3); }
        | term
        | e '?' e ':' e             { $$ = new_node('?', $1, new_node(':', $3, $5)); }
        | e ';' e                   { $$ = new_node(';', $1, $3); }
        | e '<' e                   { $$ = new_node('<', $1, $3); }
        | e '=' e                   { $$ = new_node('=', $1, $3); }
        | e '>' e                   { $$ = new_node('>', $1, $3); }
        | e '&' e                   { $$ = new_node('&', $1, $3); }
        | e '|' e                   { $$ = new_node('|', $1, $3); }
        | e '<' '=' e               { $$ = new_node('!', new_node('>', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '!' '=' e               { $$ = new_node('!', new_node('=', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '<' '>' e               { $$ = new_node('!', new_node('=', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '>' '=' e               { $$ = new_node('!', new_node('<', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '#' e                   { $$ = new_node('#', $1, $3); }
        ;

expr_list: e                        { $$ = new_node(ELIST, NULL, $1); }
        | expr_list ',' e           { $$ = new_node(ELIST, $1, $3); }
        ;

range:    VAR ':' VAR               { $$.left = $1; $$.right = $3; }
        | RANGE                     { $$ = $1; }
        ;

var:    VAR                         { $$ = $1; }
        ;

var_or_range: range                 { $$ = $1; }
        |     var                   { $$.left = $1; $$.right = $1; }
        ;

not:      '!' | '~'
        ;

num:      NUMBER                    { $$ = (double)$1; }
        | FNUMBER                   { $$ = $1; }
        | '-' num                   { $$ = -$2; }
        | '+' num                   { $$ = $2; }
        ;

strarg:   STRING                    { $$ = $1; }
        | var                       { $$ = get_strarg($1); }
        ;

/* allows >=1 'setitem's to be listed in the same 'set' command */
setlist :
        | setlist setitem
        ;

/* things that you can 'set' */
setitem : K_AUTO                    { setautocalc(1); }
        | K_AUTOCALC                { setautocalc(1); }
        | not K_AUTO                { setautocalc(0); }
        | not K_AUTOCALC            { setautocalc(0); }
        | K_BYCOLS                  { setcalcorder(BYCOLS); }
        | K_BYROWS                  { setcalcorder(BYROWS); }
        | K_OPTIMIZE                { optimize = 1; }
        | not K_OPTIMIZE            { optimize = 0; }
        | K_NUMERIC                 { numeric = 1; }
        | not K_NUMERIC             { numeric = 0; }
        | K_PRESCALE                { prescale = 0.01; } // XXX: should use 100.0
        | not K_PRESCALE            { prescale = 1.0; }
        | K_EXTFUN                  { extfunc = 1; }
        | not K_EXTFUN              { extfunc = 0; }
        | K_CELLCUR                 { showcell = 1; }
        | not K_CELLCUR             { showcell = 0; }
        | K_TOPROW                  { showtop = 1; }
        | not K_TOPROW              { showtop = 0; }
        | K_AUTOINSERT              { autoinsert = 1; }
        | not K_AUTOINSERT          { autoinsert = 0; }
        | K_AUTOWRAP                { autowrap = 1; }
        | not K_AUTOWRAP            { autowrap = 0; }
        | K_CSLOP                   { cslop = 1; FullUpdate++; }
        | not K_CSLOP               { cslop = 0; FullUpdate++; }
        | K_COLOR                   { sc_setcolor(1); }
        | not K_COLOR               { sc_setcolor(0); }
        | K_COLORNEG                { colorneg = 1; }
        | not K_COLORNEG            { colorneg = 0; }
        | K_COLORERR                { colorerr = 1; }
        | not K_COLORERR            { colorerr = 0; }
        | K_BRAILLE                 { braille = 1; }
        | not K_BRAILLE             { braille = 0; }
        | K_BACKUP                  { dobackups = 1; }
        | not K_BACKUP              { dobackups = 0; }
        | K_MOUSE                   { mouseon(); }
        | not K_MOUSE               { mouseoff(); }
        | K_ITERATIONS '=' NUMBER   { setiterations($3); }
        | K_TBLSTYLE '=' NUMBER     { tbl_style = $3; }
        | K_TBLSTYLE '=' K_TBL      { tbl_style = TBL; }
        | K_TBLSTYLE '=' K_LATEX    { tbl_style = LATEX; }
        | K_TBLSTYLE '=' K_SLATEX   { tbl_style = SLATEX; }
        | K_TBLSTYLE '=' K_TEX      { tbl_style = TEX; }
        | K_TBLSTYLE '=' K_FRAME    { tbl_style = FRAME; }
        | K_RNDTOEVEN               { rndtoeven = 1; FullUpdate++; }
        | not K_RNDTOEVEN           { rndtoeven = 0; FullUpdate++; }
        | K_CRACTION '=' NUMBER     { craction = $3; }
        | K_ROWLIMIT '=' NUMBER     { rowlimit = $3; }
        | K_COLLIMIT '=' NUMBER     { collimit = $3; }
        | K_PAGESIZE '=' NUMBER     { pagesize = $3; }
        | K_SCRC                    { scrc++; }
        | K_LOCALE                  { sc_set_locale(1); }
        | not K_LOCALE              { sc_set_locale(0); }
        | K_EMACS                   { emacs_bindings = 1; }
        | not K_EMACS               { emacs_bindings = 0; }
        ;

/* types of errors, to 'goto' */
errlist:  G_ERROR range             { num_search(0.0, $2, CELLERROR); }
        | G_ERROR                   { num_search(0.0, rangeref_total(), CELLERROR); }
        | G_INVALID range           { num_search(0.0, $2, CELLINVALID); }
        | G_INVALID                 { num_search(0.0, rangeref_total(), CELLINVALID); }
        ;
