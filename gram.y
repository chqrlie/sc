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

%token K_ERROR
%token K_INVALID
%token K_FIXED
%token K_SUM
%token K_PROD
%token K_AVG
%token K_STDDEV
%token K_COUNT
%token K_ROWS
%token K_COLS
%token K_ABS
%token K_ACOS
%token K_ASIN
%token K_ATAN
%token K_ATAN2
%token K_CEIL
%token K_COS
%token K_EXP
%token K_FABS
%token K_FLOOR
%token K_HYPOT
%token K_LN
%token K_LOG
%token K_PI
%token K_POW
%token K_SIN
%token K_SQRT
%token K_TAN
%token K_DTR
%token K_RTD
%token K_MAX
%token K_MIN
%token K_RAND
%token K_RANDBETWEEN
%token K_RND
%token K_ROUND
%token K_IF

%token K_PV
%token K_FV
%token K_PMT

%token K_HOUR
%token K_MINUTE
%token K_SECOND
%token K_MONTH
%token K_DAY
%token K_YEAR
%token K_NOW
%token K_DATE
%token K_DTS
%token K_TTS
%token K_FMT
%token K_SUBSTR
%token K_UPPER
%token K_LOWER
%token K_CAPITAL
%token K_STON
%token K_EQS
%token K_EXT
%token K_NVAL
%token K_SVAL
%token K_LOOKUP
%token K_HLOOKUP
%token K_VLOOKUP
%token K_INDEX
%token K_STINDEX
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
%token K_BLACK
%token K_RED
%token K_GREEN
%token K_YELLOW
%token K_BLUE
%token K_MAGENTA
%token K_CYAN
%token K_WHITE
%token K_TBLSTYLE
%token K_TBL
%token K_LATEX
%token K_SLATEX
%token K_TEX
%token K_FRAME
%token K_RNDTOEVEN
%token K_FILENAME
%token K_MYROW
%token K_MYCOL
%token K_LASTROW
%token K_LASTCOL
%token K_COLTOA
%token K_CRACTION
%token K_CRROW
%token K_CRCOL
%token K_ROWLIMIT
%token K_COLLIMIT
%token K_PAGESIZE
%token K_NUMITER
%token K_ERR
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
        | '@' K_FIXED term              { $$ = new_node('f', $3, NULL); }
        | '(' '@' K_FIXED ')' term      { $$ = new_node('F', $5, NULL); }
        | '@' K_SUM '(' var_or_range ')'  { $$ = new_range(SUM, $4, NULL); }
        | '@' K_SUM '(' range ',' e ')'  { $$ = new_range(SUM, $4, $6); }
        | '@' K_PROD '(' var_or_range ')'  { $$ = new_range(PROD, $4, NULL); }
        | '@' K_PROD '(' range ',' e ')'  { $$ = new_range(PROD, $4, $6); }
        | '@' K_AVG '(' var_or_range ')'  { $$ = new_range(AVG, $4, NULL); }
        | '@' K_AVG '(' range ',' e ')'  { $$ = new_range(AVG, $4, $6); }
        | '@' K_STDDEV '(' var_or_range ')'  { $$ = new_range(STDDEV, $4, NULL); }
        | '@' K_STDDEV '(' range ',' e ')'  { $$ = new_range(STDDEV, $4, $6); }
        | '@' K_COUNT '(' var_or_range ')'  { $$ = new_range(COUNT, $4, NULL); }
        | '@' K_COUNT '(' range ',' e ')'  { $$ = new_range(COUNT, $4, $6); }
        | '@' K_MAX '(' var_or_range ')'  { $$ = new_range(MAX, $4, NULL); }
        | '@' K_MAX '(' range ',' e ')'  { $$ = new_range(MAX, $4, $6); }
        | '@' K_MAX '(' e ',' expr_list ')'  { $$ = new_node(LMAX, $6, $4); }
        | '@' K_MIN '(' var_or_range ')'  { $$ = new_range(MIN, $4, NULL); }
        | '@' K_MIN '(' range ',' e ')'  { $$ = new_range(MIN, $4, $6); }
        | '@' K_MIN '(' e ',' expr_list ')'  { $$ = new_node(LMIN, $6, $4); }
        | '@' K_ROWS '(' var_or_range ')'  { $$ = new_range('R', $4, NULL); }
        | '@' K_COLS '(' var_or_range ')'  { $$ = new_range('C', $4, NULL); }

        | '@' K_ABS '(' e ')'           { $$ = new_node(ABS, $4, NULL); }
        | '@' K_ACOS '(' e ')'          { $$ = new_node(ACOS, $4, NULL); }
        | '@' K_ASIN '(' e ')'          { $$ = new_node(ASIN, $4, NULL); }
        | '@' K_ATAN '(' e ')'          { $$ = new_node(ATAN, $4, NULL); }
        | '@' K_ATAN2 '(' e ',' e ')'   { $$ = new_node(ATAN2, $4, $6); }
        | '@' K_CEIL '(' e ')'          { $$ = new_node(CEIL, $4, NULL); }
        | '@' K_COS '(' e ')'           { $$ = new_node(COS, $4, NULL); }
        | '@' K_EXP '(' e ')'           { $$ = new_node(EXP, $4, NULL); }
        | '@' K_FABS '(' e ')'          { $$ = new_node(FABS, $4, NULL); }
        | '@' K_FLOOR '(' e ')'         { $$ = new_node(FLOOR, $4, NULL); }
        | '@' K_HYPOT '(' e ',' e ')'   { $$ = new_node(HYPOT, $4, $6); }
        | '@' K_LN '(' e ')'            { $$ = new_node(LOG, $4, NULL); }
        | '@' K_LOG '(' e ')'           { $$ = new_node(LOG10, $4, NULL); }
        | '@' K_POW '(' e ',' e ')'     { $$ = new_node(POW, $4, $6); }
        | '@' K_SIN '(' e ')'           { $$ = new_node(SIN, $4, NULL); }
        | '@' K_SQRT '(' e ')'          { $$ = new_node(SQRT, $4, NULL); }
        | '@' K_TAN '(' e ')'           { $$ = new_node(TAN, $4, NULL); }
        | '@' K_DTR '(' e ')'           { $$ = new_node(DTR, $4, NULL); }
        | '@' K_RTD '(' e ')'           { $$ = new_node(RTD, $4, NULL); }
        | '@' K_RAND '(' ')'            { $$ = new_node(RAND, NULL, NULL); }
        | '@' K_RANDBETWEEN '(' e ',' e ')'  { $$ = new_node(RANDBETWEEN, $4, $6); }
        | '@' K_RND '(' e ')'           { $$ = new_node(RND, $4, NULL); }
        | '@' K_ROUND '(' e ',' e ')'   { $$ = new_node(ROUND, $4, $6); }

        | '@' K_IF '(' e ',' e ',' e ')' { $$ = new_node(IF, $4, new_node(',', $6, $8)); }

        | '@' K_PV '(' e ',' e ',' e ')' { $$ = new_node(PV, $4, new_node(':', $6, $8)); }
        | '@' K_FV '(' e ',' e ',' e ')' { $$ = new_node(FV, $4, new_node(':', $6, $8)); }
        | '@' K_PMT '(' e ',' e ',' e ')' { $$ = new_node(PMT, $4, new_node(':', $6, $8)); }

        | '@' K_HOUR '(' e ')'          { $$ = new_node(HOUR, $4, NULL); }
        | '@' K_MINUTE '(' e ')'        { $$ = new_node(MINUTE, $4, NULL); }
        | '@' K_SECOND '(' e ')'        { $$ = new_node(SECOND, $4, NULL); }
        | '@' K_MONTH '(' e ')'         { $$ = new_node(MONTH, $4, NULL); }
        | '@' K_DAY '(' e ')'           { $$ = new_node(DAY, $4, NULL); }
        | '@' K_YEAR '(' e ')'          { $$ = new_node(YEAR, $4, NULL); }
        | '@' K_NOW                     { $$ = new_node(NOW, NULL, NULL); }
        | '@' K_DTS '(' e ',' e ',' e ')'  { $$ = new_node(DTS, $4, new_node(',', $6, $8)); }
                                        // XXX: should return a specific token, same for x/x/x and x:x and x:x:x
        | NUMBER '.' NUMBER '.' NUMBER  { $$ = new_node(DTS,
                                                   new_const(O_CONST, (double)$1),
                                                   new_node(',', new_const(O_CONST, (double)$3),
                                                       new_const(O_CONST, (double)$5))); }
        | '@' K_TTS '(' e ',' e ',' e ')'  { $$ = new_node(TTS, $4, new_node(',', $6, $8)); }
        | '@' K_STON '(' e ')'          { $$ = new_node(STON, $4, NULL); }
        | '@' K_EQS '(' e ',' e ')'     { $$ = new_node(EQS, $4, $6); }
        | '@' K_DATE '(' e ')'          { $$ = new_node(DATE, $4, NULL); }
        | '@' K_DATE '(' e ',' e ')'    { $$ = new_node(DATE, $4, $6); }
        | '@' K_FMT '(' e ',' e ')'     { $$ = new_node(FMT, $4, $6); }
        | '@' K_UPPER '(' e ')'         { $$ = new_node(UPPER, $4, NULL); }
        | '@' K_LOWER '(' e ')'         { $$ = new_node(LOWER, $4, NULL); }
        | '@' K_CAPITAL '(' e ')'       { $$ = new_node(CAPITAL, $4, NULL); }
        | '@' K_INDEX '(' range ',' e ')'  { $$ = new_range(INDEX, $4, $6); }
        | '@' K_INDEX '(' e ',' range ')'  { $$ = new_range(INDEX, $6, $4); }
        | '@' K_INDEX '(' range ',' e ',' e ')'  { $$ = new_range(INDEX, $4, new_node(',', $6, $8)); }
        | '@' K_LOOKUP '(' range ',' e ')'  { $$ = new_range(LOOKUP, $4, $6); }
        | '@' K_LOOKUP '(' e ',' range ')'  { $$ = new_range(LOOKUP, $6, $4); }
        | '@' K_HLOOKUP '(' range ',' e ',' e ')'  { $$ = new_range(HLOOKUP, $4, new_node(',', $6, $8)); }
        | '@' K_HLOOKUP '(' e ',' range ',' e ')'  { $$ = new_range(HLOOKUP, $6, new_node(',', $4, $8)); }
        | '@' K_VLOOKUP '(' range ',' e ',' e ')'  { $$ = new_range(VLOOKUP, $4, new_node(',', $6, $8)); }
        | '@' K_VLOOKUP '(' e ',' range ',' e ')'  { $$ = new_range(VLOOKUP, $6, new_node(',', $4, $8)); }
        | '@' K_STINDEX '(' range ',' e ')'  { $$ = new_range(STINDEX, $4, $6); }
        | '@' K_STINDEX '(' e ',' range ')'  { $$ = new_range(STINDEX, $6, $4); }
        | '@' K_STINDEX '(' range ',' e ',' e ')'  { $$ = new_range(STINDEX, $4, new_node(',', $6, $8)); }
        | '@' K_EXT '(' e ',' e ')'     { $$ = new_node(EXT, $4, $6); }
        | '@' K_NVAL '(' e ',' e ')'    { $$ = new_node(NVAL, $4, $6); }
        | '@' K_SVAL '(' e ',' e ')'    { $$ = new_node(SVAL, $4, $6); }
        | '@' K_SUBSTR '(' e ',' e ',' e ')'  { $$ = new_node(SUBSTR, $4, new_node(',', $6, $8)); }
        |     '(' e ')'                 { $$ = $2; }
        |     '+' term                  { $$ = $2; }
        |     '-' term                  { $$ = new_node('m', $2, NULL); }
        |     NUMBER                    { $$ = new_const(O_CONST, (double)$1); }
        |     FNUMBER                   { $$ = new_const(O_CONST, $1); }
        | '@' K_PI                      { $$ = new_node(PI_, NULL, NULL); }
        |     STRING                    { $$ = new_str($1); }
        |     '~' term                  { $$ = new_node('!', $2, NULL); }
        |     '!' term                  { $$ = new_node('!', $2, NULL); }
        | '@' K_FILENAME '(' e ')'      { $$ = new_node(FILENAME, $4, NULL); }
        | '@' K_MYROW                   { $$ = new_node(MYROW, NULL, NULL); }
        | '@' K_MYCOL                   { $$ = new_node(MYCOL, NULL, NULL); }
        | '@' K_LASTROW                 { $$ = new_node(LASTROW, NULL, NULL); }
        | '@' K_LASTCOL                 { $$ = new_node(LASTCOL, NULL, NULL); }
        | '@' K_COLTOA '(' e ')'        { $$ = new_node(COLTOA, $4, NULL); }
        | '@' K_NUMITER                 { $$ = new_node(NUMITER, NULL, NULL); }
        | '@' K_ERR                     { $$ = new_node(ERR_, NULL, NULL); }
        |     K_ERR                     { $$ = new_node(ERR_, NULL, NULL); }
        | '@' K_BLACK                   { $$ = new_node(BLACK, NULL, NULL); }
        | '@' K_RED                     { $$ = new_node(RED, NULL, NULL); }
        | '@' K_GREEN                   { $$ = new_node(GREEN, NULL, NULL); }
        | '@' K_YELLOW                  { $$ = new_node(YELLOW, NULL, NULL); }
        | '@' K_BLUE                    { $$ = new_node(BLUE, NULL, NULL); }
        | '@' K_MAGENTA                 { $$ = new_node(MAGENTA, NULL, NULL); }
        | '@' K_CYAN                    { $$ = new_node(CYAN, NULL, NULL); }
        | '@' K_WHITE                   { $$ = new_node(WHITE, NULL, NULL); }
        ;

/* expressions */
e:        e '+' e           { $$ = new_node('+', $1, $3); }
        | e '-' e           { $$ = new_node('-', $1, $3); }
        | e '*' e           { $$ = new_node('*', $1, $3); }
        | e '/' e           { $$ = new_node('/', $1, $3); }
        | e '%' e           { $$ = new_node('%', $1, $3); }
        | e '^' e           { $$ = new_node('^', $1, $3); }
        | term
        | e '?' e ':' e     { $$ = new_node('?', $1, new_node(':', $3, $5)); }
        | e ';' e           { $$ = new_node(';', $1, $3); }
        | e '<' e           { $$ = new_node('<', $1, $3); }
        | e '=' e           { $$ = new_node('=', $1, $3); }
        | e '>' e           { $$ = new_node('>', $1, $3); }
        | e '&' e           { $$ = new_node('&', $1, $3); }
        | e '|' e           { $$ = new_node('|', $1, $3); }
        | e '<' '=' e       { $$ = new_node('!', new_node('>', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '!' '=' e       { $$ = new_node('!', new_node('=', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '<' '>' e       { $$ = new_node('!', new_node('=', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '>' '=' e       { $$ = new_node('!', new_node('<', $1, $4), NULL); } // XXX: incorrect for NaN
        | e '#' e           { $$ = new_node('#', $1, $3); }
        ;

expr_list: e                { $$ = new_node(ELIST, NULL, $1); }
        | expr_list ',' e   { $$ = new_node(ELIST, $1, $3); }
        ;

range:    VAR ':' VAR       { $$.left = $1; $$.right = $3; }
        | RANGE             { $$ = $1; }
        ;

var:    VAR                 { $$ = $1; }
        ;

var_or_range: range         { $$ = $1; }
        |     var           { $$.left = $1; $$.right = $1; }
        ;

not:      '!' | '~'
        ;

num:      NUMBER            { $$ = (double)$1; }
        | FNUMBER           { $$ = $1; }
        | '-' num           { $$ = -$2; }
        | '+' num           { $$ = $2; }
        ;

strarg:   STRING            { $$ = $1; }
        | var               { $$ = get_strarg($1); }
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
errlist:  K_ERROR range             { num_search(0.0, $2, CELLERROR); }
        | K_ERROR                   { num_search(0.0, rangeref_total(), CELLERROR); }
        | K_INVALID range           { num_search(0.0, $2, CELLINVALID); }
        | K_INVALID                 { num_search(0.0, rangeref_total(), CELLINVALID); }
        ;
