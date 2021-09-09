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

static void cmd_error(SCXMEM string_t *s) {
    if (s) {
        error("%s", s2c(s));
        string_free(s);
    }
}

static void cmd_quit(void) {
    stopdisp();
    exit(0);
}

static int cmd_readfile(sheet_t *sp, SCXMEM string_t *fname, int eraseflg) {
    int ret = -1;
    if (fname) {
        ret = readfile(sp, s2c(fname), eraseflg);
        string_free(fname);
    }
    return ret;
}

static int cmd_writefile(sheet_t *sp, SCXMEM string_t *fname, rangeref_t rr, int dcp_flags) {
    int ret = -1;
    if (fname) {
        ret = writefile(sp, s2c(fname), rr, dcp_flags);
        string_free(fname);
    }
    return ret;
}

static int string_to_char(SCXMEM string_t *str) {
    int c = -1;
    if (str) {
        c = *s2c(str);
        string_free(str);
    }
    return c;
}

static SCXMEM string_t *get_strarg(sheet_t *sp, cellref_t cr) {
    struct ent *p = getcell(sp, cr.row, cr.col);
    if (p && p->type == SC_STRING) {
        return string_dup(p->label);
    } else {
        // XXX: should convert numeric value to string according to format?
        return string_new("NULL_STRING");
    }
}

%}

%union {
    int ival;
    double fval;
    SCXMEM string_t *sval;
    SCXMEM enode_t *enode;
    struct cellref cval;
    struct rangeref rval;
}

%type <cval> var
%type <fval> num
%type <rval> range var_or_range
%type <sval> strarg
%type <enode> e term expr_list
%type <ival> outfd noval
%token <sval> STRING BADFUNC BADNAME
%token <ival> NUMBER T_ERROR
%token <fval> FNUMBER
%token <rval> RANGE
%token <cval> VAR
%token <sval> WORD PLUGIN
%token <ival> COL
%token <ival> FUNC0 FUNC01 FUNC1 FUNC12 FUNC13 FUNC1x FUNC2 FUNC2x FUNC23 FUNC3 FUNC34 FUNC35

/* command names (one per line for automatic generation of tokens.h) */

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
%token GO_ERROR
%token GO_INVALID

/* setting names (one per line for automatic generation of tokens.h) */
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

//%token <ival> T_GTE T_LTE T_NE T_NE2
//%token <ival> '+' '-' '*' '/' '%' '^'

%right ';'
%left '&'
%nonassoc '<' '=' '>' T_LTE T_GTE T_NE T_NE2
%left '+' '-'
%left '*' '/' '%'
%left '^'
%left '!'
%left ':'

%%

command:  S_LET var_or_range '=' e      { let(sht, $2.left, $4, -1); }
        | S_LET var_or_range '='        { unlet(sht, $2.left); }
        | S_LABEL var_or_range '=' e    { let(sht, $2.left, $4, ALIGN_CENTER); }
        | S_LEFTSTRING var_or_range '=' e  { let(sht, $2.left, $4, ALIGN_LEFT); }
        | S_RIGHTSTRING var_or_range '=' e  { let(sht, $2.left, $4, ALIGN_RIGHT); }
        | S_LEFTJUSTIFY var_or_range    { range_align(sht, $2, ALIGN_LEFT); }
        | S_LEFTJUSTIFY                 { range_align(sht, rangeref_current(), ALIGN_LEFT); }
        | S_RIGHTJUSTIFY var_or_range   { range_align(sht, $2, ALIGN_RIGHT); }
        | S_RIGHTJUSTIFY                { range_align(sht, rangeref_current(), ALIGN_RIGHT); }
        | S_CENTER var_or_range         { range_align(sht, $2, ALIGN_CENTER); }
        | S_CENTER                      { range_align(sht, rangeref_current(), ALIGN_CENTER); }
        | S_ADDNOTE var                 { addnote(sht, $2, rangeref_current()); }
        | S_ADDNOTE var var_or_range    { addnote(sht, $2, $3); }
        | S_DELNOTE var                 { delnote(sht, $2); }
        | S_DELNOTE                     { delnote(sht, cellref_current()); }
        | S_FORMAT COL ':' COL NUMBER NUMBER NUMBER  { cmd_format(sht, $2, $4, $5, $6, $7); }
        | S_FORMAT COL NUMBER NUMBER NUMBER  { cmd_format(sht, $2, $2, $3, $4, $5); }
        | S_FORMAT COL ':' COL NUMBER NUMBER  { cmd_format(sht, $2, $4, $5, $6, REFMTFIX); }
        | S_FORMAT COL NUMBER NUMBER    { cmd_format(sht, $2, $2, $3, $4, REFMTFIX); }
        | S_FORMAT NUMBER '=' STRING    { cmd_setformat(sht, $2, $4); }
        | S_GET strarg                  { cmd_readfile(sht, $2, 1); }
        | S_MERGE strarg                { cmd_readfile(sht, $2, 0); }
        | S_MDIR strarg                 { set_mdir($2); }
        | S_AUTORUN strarg              { set_autorun($2); }
        | S_FKEY NUMBER '=' strarg      { set_fkey($2, $4); }
        | S_HISTFILE strarg             { string_set(&histfile, $2); }
        | S_SCEXT strarg                { string_set(&scext, $2); }
        | S_ASCEXT strarg               { string_set(&ascext, $2); }
        | S_TBL0EXT strarg              { string_set(&tbl0ext, $2); }
        | S_TBLEXT strarg               { string_set(&tblext, $2); }
        | S_LATEXEXT strarg             { string_set(&latexext, $2); }
        | S_SLATEXEXT strarg            { string_set(&slatexext, $2); }
        | S_TEXEXT strarg               { string_set(&texext, $2); }
        | S_PUT strarg range noval      { cmd_writefile(sht, $2, $3, $4); }
        | S_PUT strarg noval            { cmd_writefile(sht, $2, rangeref_total(sht), $3); }
        | S_PUT range noval             { write_cells(sht, stdout, $2, $2.left, $3 | DCP_NO_NAME); }
        | S_PUT range '/' var noval     { write_cells(sht, stdout, $2, $4, $5 | DCP_NO_NAME); }
        | S_PUT '%' '/' var noval       { write_cells(sht, stdout, rangeref_total(sht), $4, $5 | DCP_NO_NAME); }
        | S_PUT '/' var noval           { write_cells(sht, stdout, rangeref_current(), $3, $4 | DCP_NO_NAME); }
        | S_PUT '%' noval               { write_cells(sht, stdout, rangeref_total(sht), cellref(0, 0), $3 | DCP_NO_NAME); }
        | S_PUT noval                   { write_cells(sht, stdout, rangeref_total(sht), cellref(0, 0), $2 | DCP_NO_NAME); }
        | S_WRITE strarg range          { printfile(sht, $2, $3); }
        | S_WRITE strarg                { printfile(sht, $2, rangeref_total(sht)); }
        | S_WRITE range                 { printfile(sht, NULL, $2); }
        | S_WRITE '%'                   { printfile(sht, NULL, rangeref_total(sht)); }
        | S_WRITE                       { printfile(sht, NULL, rangeref_total(sht)); }
        | S_TBL strarg range            { tblprintfile(sht, $2, $3); }
        | S_TBL strarg                  { tblprintfile(sht, $2, rangeref_total(sht)); }
        | S_SHOW COL ':' COL            { showcol(sht, $2, $4); }
        | S_SHOW NUMBER ':' NUMBER      { showrow(sht, $2, $4); }
        | S_HIDE                        { dohide(sht); }
        | S_HIDE COL                    { hidecols(sht, $2, $2); }
        | S_HIDE COL ':' COL            { hidecols(sht, $2, $4); }
        | S_HIDE NUMBER                 { hiderows(sht, $2, $2); }
        | S_HIDE NUMBER ':' NUMBER      { hiderows(sht, $2, $4); }
        | S_COPY                        { copy(sht, COPY_FROM_DEF, rangeref_current(), rangeref_empty()); }
        | S_COPY range                  { copy(sht, COPY_FROM_DEF, $2, rangeref_empty()); }
        | S_COPY range var_or_range     { copy(sht, COPY_FROM_RANGE, $2, $3); }
        | S_MOVE var                    { mover(sht, $2, rangeref_current()); }
        | S_MOVE var var_or_range       { mover(sht, $2, $3); }
        | S_ERASE                       { eraser(sht, rangeref_current()); }
        | S_ERASE var_or_range          { eraser(sht, $2); }
        | S_YANK                        { yankr(sht, rangeref_current()); }
        | S_YANK var_or_range           { yankr(sht, $2); }
        | S_VALUE                       { valueize_area(sht, rangeref_current()); }
        | S_VALUE var_or_range          { valueize_area(sht, $2); }
        | S_FILL var_or_range num num   { fillr(sht, $2, $3, $4, calc_order == BYCOLS); }
        | S_FILL var_or_range num       { fillr(sht, $2, $3, 0, calc_order == BYCOLS); }
        | S_FILL var_or_range           { fillr(sht, $2, 0, 0, calc_order == BYCOLS); }
        | S_SORT                        { sortrange(sht, rangeref_current(), NULL); }
        | S_SORT range                  { sortrange(sht, $2, NULL); }
        | S_SORT range strarg           { sortrange(sht, $2, $3); }
        | S_FMT var_or_range STRING     { format_cells(sht, $2, $3); }
        | S_LOCK                        { lock_cells(sht, rangeref_current()); }
        | S_LOCK var_or_range           { lock_cells(sht, $2); }
        | S_UNLOCK                      { unlock_cells(sht, rangeref_current()); }
        | S_UNLOCK var_or_range         { unlock_cells(sht, $2); }
        | S_GOTO var_or_range var_or_range { moveto(sht, $2, $3.left); }
        | S_GOTO var_or_range           { moveto(sht, $2, cellref(-1, -1)); }
        | S_GOTO num range              { num_search(sht, G_NUM, $3, $2); }
        | S_GOTO num                    { num_search(sht, G_NUM, rangeref_total(sht), $2); }
        | S_GOTO GO_ERROR range         { num_search(sht, G_ERROR, $3, 0.0); }
        | S_GOTO GO_ERROR               { num_search(sht, G_ERROR, rangeref_total(sht), 0.0); }
        | S_GOTO GO_INVALID range       { num_search(sht, G_INVALID, $3, 0.0); }
        | S_GOTO GO_INVALID             { num_search(sht, G_INVALID, rangeref_total(sht), 0.0); }
        | S_GOTO STRING range           { str_search(sht, G_STR, $3, $2); }
        | S_GOTO '#' STRING range       { str_search(sht, G_NSTR, $4, $3); }
        | S_GOTO '%' STRING range       { str_search(sht, G_XSTR, $4, $3); }
        | S_GOTO STRING                 { str_search(sht, G_STR, rangeref_total(sht), $2); }
        | S_GOTO '#' STRING             { str_search(sht, G_NSTR, rangeref_total(sht), $3); }
        | S_GOTO '%' STRING             { str_search(sht, G_XSTR, rangeref_total(sht), $3); }
        | S_GOTO                        { go_last(sht); }
        | S_GOTO WORD                   { /* don't repeat last goto on "unintelligible word" */ }
        | S_DEFINE strarg               { add_nrange(sht, $2, rangeref_current(), -1); }
        | S_DEFINE strarg range         { add_nrange(sht, $2, $3, 1); }  // XXX: why distinguish this way?
        | S_DEFINE strarg var           { add_nrange(sht, $2, rangeref2($3, $3), 0); }
        | S_UNDEFINE var_or_range       { del_nrange(sht, $2); }
        | S_ABBREV STRING STRING        { add_abbr(sht, $2, $3); }
        | S_ABBREV STRING               { add_abbr(sht, $2, NULL); }
        | S_ABBREV                      { add_abbr(sht, NULL, NULL); }
        | S_UNABBREV STRING             { del_abbr(sht, $2); }
        | S_FRAME range range           { add_frange(sht, FRANGE_DIRECT | FRANGE_INNER,
                                                     $2, $3, 0, 0, 0, 0); }
        | S_FRAME range                 { if (showrange) {
                                            add_frange(sht, FRANGE_DIRECT | FRANGE_INNER,
                                                       $2, rangeref_current(), 0, 0, 0, 0);
                                          } else {
                                            add_frange(sht, FRANGE_FIND | FRANGE_INNER,
                                                       rangeref_current(),
                                                       $2, 0, 0, 0, 0);
                                          }
                                        }
        | S_FRAME                       { if (showrange && get_current_frange()) {
                                            add_frange(sht, FRANGE_FIND | FRANGE_INNER,
                                                       rangeref(currow, curcol, currow, curcol),
                                                       rangeref_current(), 0, 0, 0, 0);
                                          } else {
                                              error("Need both outer and inner"
                                                    " ranges to create frame");
                                          }
                                        }
        | S_FRAMETOP range NUMBER       { add_frange(sht, FRANGE_DIRECT,
                                                     $2, rangeref_empty(), $3, -1, -1, -1); }
        | S_FRAMETOP NUMBER             { add_frange(sht, FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), $2, -1, -1, -1); }
        | S_FRAMEBOTTOM range NUMBER    { add_frange(sht, FRANGE_DIRECT, $2,
                                                     rangeref_empty(), -1, $3, -1, -1); }
        | S_FRAMEBOTTOM NUMBER          { add_frange(sht, FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), -1, $2, -1, -1); }
        | S_FRAMELEFT range NUMBER      { add_frange(sht, FRANGE_DIRECT, $2,
                                                     rangeref_empty(), -1, -1, $3, -1); }
        | S_FRAMELEFT NUMBER            { add_frange(sht, FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), -1, -1, $2, -1); }
        | S_FRAMERIGHT range NUMBER     { add_frange(sht, FRANGE_DIRECT, $2,
                                                     rangeref_empty(), -1, -1, -1, $3); }
        | S_FRAMERIGHT NUMBER           { add_frange(sht, FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), -1, -1, -1, $2); }
        | S_UNFRAME range               { add_frange(sht, FRANGE_DIRECT, $2,
                                                     rangeref_empty(), 0, 0, 0, 0); }
        | S_UNFRAME                     { add_frange(sht, FRANGE_FIND,
                                                     rangeref(currow, curcol, currow, curcol),
                                                     rangeref_empty(), 0, 0, 0, 0); }
        | S_COLOR NUMBER '='            { initcolor(sht, $2); }
        | S_COLOR NUMBER '=' e          { change_color(sht, $2, $4); }
        | S_COLOR range NUMBER          { add_crange(sht, $2, $3); }
        | S_SET setlist                 { modflg++; }
        | S_UP                          { backrow(sht, 1); }
        | S_UP NUMBER                   { backrow(sht, $2); }
        | S_DOWN                        { forwrow(sht, 1); }
        | S_DOWN NUMBER                 { forwrow(sht, $2); }
        | S_LEFT                        { backcol(sht, 1); }
        | S_LEFT NUMBER                 { backcol(sht, $2); }
        | S_RIGHT                       { forwcol(sht, 1); }
        | S_RIGHT NUMBER                { forwcol(sht, $2); }
        | S_ENDUP                       { doend(sht, -1,  0); }
        | S_ENDDOWN                     { doend(sht,  1,  0); }
        | S_ENDLEFT                     { doend(sht,  0, -1); }
        | S_ENDRIGHT                    { doend(sht,  0,  1); }
        | S_SELECT STRING               { cmd_select_qbuf(string_to_char($2)); }
        | S_INSERTROW                   { insertrow(sht, cellref_current(),  1, 0); }
        | S_INSERTROW '*' NUMBER        { insertrow(sht, cellref_current(), $3, 0); }
        | S_INSERTCOL                   { insertcol(sht, cellref_current(),  1, 0); }
        | S_INSERTCOL '*' NUMBER        { insertcol(sht, cellref_current(), $3, 0); }
        | S_OPENROW                     { currow += insertrow(sht, cellref_current(),  1, 1); }
        | S_OPENROW '*' NUMBER          { currow += insertrow(sht, cellref_current(), $3, 1); }
        | S_OPENCOL                     { curcol += insertcol(sht, cellref_current(),  1, 1); }
        | S_OPENCOL '*' NUMBER          { curcol += insertcol(sht, cellref_current(), $3, 1); }
        | S_DELETEROW                   { if (showrange == SHOWROWS)
                                            deletecols(sht, showsr, currow);
                                          else
                                            deletecols(sht, currow, currow);
                                        }
        | S_DELETEROW '*' NUMBER        { deleterows(sht, currow, currow + $3 - 1); }
        | S_DELETEROW NUMBER            { deleterows(sht, $2, $2); }
        | S_DELETEROW NUMBER ':' NUMBER { deleterows(sht, $2, $4); }
        | S_DELETECOL                   { if (showrange == SHOWCOLS)
                                            deletecols(sht, showsc, curcol);
                                          else
                                            deletecols(sht, curcol, curcol);
                                        }
        | S_DELETECOL COL               { deletecols(sht, $2, $2); }
        | S_DELETECOL '*' NUMBER        { deletecols(sht, curcol, curcol + $3 - 1); }
        | S_DELETECOL COL ':' COL       { deletecols(sht, $2, $4); }
        | S_YANKROW                     { if (showrange == SHOWROWS)
                                            yankrows(sht, showsr, currow);
                                          else
                                            yankrows(sht, currow, currow);
                                        }
        | S_YANKROW '*' NUMBER          { yankrows(sht, currow, currow + $3 - 1); }
        | S_YANKROW NUMBER              { yankrows(sht, $2, $2); }
        | S_YANKROW NUMBER ':' NUMBER   { yankrows(sht, $2, $4); }
        | S_YANKCOL                     { if (showrange == SHOWCOLS)
                                            yankcols(sht, showsc, curcol);
                                          else
                                            yankcols(sht, curcol, curcol);
                                        }
        | S_YANKCOL NUMBER              { yankcols(sht, $2, $2); }
        | S_YANKCOL '*' NUMBER          { yankcols(sht, curcol, curcol + $3 - 1); }
        | S_YANKCOL COL ':' COL         { yankcols(sht, $2, $4); }
        | S_PULL                        { pullcells(sht, 'p', cellref_current()); }
        | S_PULLMERGE                   { pullcells(sht, 'm', cellref_current()); }
        | S_PULLROWS                    { pullcells(sht, 'r', cellref_current()); }
        | S_PULLCOLS                    { pullcells(sht, 'c', cellref_current()); }
        | S_PULLXCHG                    { pullcells(sht, 'x', cellref_current()); }
        | S_PULLTP                      { pullcells(sht, 't', cellref_current()); }
        | S_PULLFMT                     { pullcells(sht, 'f', cellref_current()); }
        | S_PULLCOPY                    { copy(sht, COPY_FROM_QBUF, rangeref_current(), rangeref_empty()); }
        | S_PULLCOPY var_or_range       { copy(sht, COPY_FROM_QBUF, $2, rangeref_empty()); }

        | S_WHEREAMI outfd              { cmd_whereami(sht, $2); }
        | S_GETNUM var_or_range outfd   { cmd_getnum(sht, $2, $3); }
        | S_GETNUM outfd                { cmd_getnum(sht, rangeref_current(), $2); }
        | S_FGETNUM var_or_range outfd  { cmd_fgetnum(sht, $2, $3); }
        | S_FGETNUM outfd               { cmd_fgetnum(sht, rangeref_current(), $2); }
        | S_GETSTRING var_or_range outfd  { cmd_getstring(sht, $2, $3); }
        | S_GETSTRING outfd             { cmd_getstring(sht, rangeref_current(), $2); }
        | S_GETEXP var_or_range outfd   { cmd_getexp(sht, $2, $3); }
        | S_GETEXP outfd                { cmd_getexp(sht, rangeref_current(), $2); }
        | S_GETFORMAT COL outfd         { cmd_getformat(sht, $2, $3); }
        | S_GETFORMAT outfd             { cmd_getformat(sht, curcol, $2); }
        | S_GETFMT var_or_range outfd   { cmd_getfmt(sht, $2, $3); }
        | S_GETFMT outfd                { cmd_getfmt(sht, rangeref_current(), $2); }
        | S_GETFRAME outfd              { cmd_getframe(sht, $2); }
        | S_GETRANGE STRING outfd       { cmd_getrange(sht, $2, $3); }
        | S_EVAL e outfd                { cmd_eval(sht, $2, NULL, currow, curcol, $3); }
        | S_EVAL e STRING outfd         { cmd_eval(sht, $2, $3, currow, curcol, $4); }
        | S_SEVAL e outfd               { cmd_seval(sht, $2, currow, curcol, $3); }
        | S_QUERY STRING STRING outfd   { cmd_query(sht, $2, $3, $4); }
        | S_QUERY STRING outfd          { cmd_query(sht, $2, NULL, $3); }
        | S_QUERY outfd                 { cmd_query(sht, NULL, NULL, $2); }
        | S_GETKEY outfd                { cmd_getkey($2); }
        | S_STATUS outfd                { cmd_status($2); }

        | S_RECALC                      { cmd_recalc(sht); }
        | S_REDRAW                      { cmd_redraw(sht); }
        | S_ERROR STRING                { cmd_error($2); }
        | S_QUIT                        { cmd_quit(); }
        | S_RUN STRING                  { cmd_run($2); }
        | S_PLUGIN STRING '=' STRING    { add_plugin($2, $4, 'r'); }
        | S_PLUGOUT STRING '=' STRING   { add_plugin($2, $4, 'w'); }
        | PLUGIN                        { cmd_plugin(sht, $1); }
        | /* nothing */
        | error
        ;

outfd:                                  { $$ = macrofd; }
        | '|' NUMBER                    { $$ = $2; }
        ;

noval:                                  { $$ = DCP_DEFAULT; }
        | '*'                           { $$ = DCP_NO_EXPR; }
        ;

term:         VAR                       { $$ = new_var(sht, $1); }
        |     RANGE                     { $$ = new_range(sht, $1); }
        |     NUMBER                    { $$ = new_const((double)$1); }
        |     FNUMBER                   { $$ = new_const($1); }
        |     STRING                    { $$ = new_str($1); }
        |     T_ERROR                   { $$ = new_error($1); }
        |     '(' e ')'                 { $$ = $2; }
        |     '+' term                  { $$ = new_op1(OP_UPLUS_, $2); }
        |     '-' term                  { $$ = new_op1(OP_UMINUS_, $2); }
        |     term '%'                  { $$ = new_op1(OP_PERCENT_, $1); }
        | FUNC0 '(' ')'                 { $$ = new_op0($1, 0); }
        | FUNC0                         { $$ = new_op0($1, -1); }
        | FUNC01 '(' ')'                { $$ = new_op0($1, 0); }
        | FUNC01                        { $$ = new_op0($1, -1); }
        | FUNC01 '(' e ')'              { $$ = new_op1($1, $3); }
        | FUNC1 '(' e ')'               { $$ = new_op1($1, $3); }
        | FUNC12 '(' e ')'              { $$ = new_op1($1, $3); }
        | FUNC12 '(' e ',' e ')'        { $$ = new_op2($1, $3, $5); }
        | FUNC13 '(' e ')'              { $$ = new_op1($1, $3); }
        | FUNC13 '(' e ',' e ')'        { $$ = new_op2($1, $3, $5); }
        | FUNC13 '(' e ',' e ',' e ')'  { $$ = new_op3($1, $3, $5, $7); }
        | FUNC1x '(' e ')'              { $$ = new_op1($1, $3); }
        | FUNC1x '(' e ',' expr_list ')' { $$ = new_op1x($1, $3, $5); }
        | FUNC2 '(' e ',' e ')'         { $$ = new_op2($1, $3, $5); }
        | FUNC23 '(' e ',' e ')'        { $$ = new_op2($1, $3, $5); }
        | FUNC23 '(' e ',' e ',' e ')'  { $$ = new_op3($1, $3, $5, $7); }
        | FUNC2x '(' e ',' expr_list ')' { $$ = new_op1x($1, $3, $5); }
        | FUNC3 '(' e ',' e ',' e ')'   { $$ = new_op3($1, $3, $5, $7); }
        | FUNC34 '(' e ',' e ',' e ')'  { $$ = new_op3($1, $3, $5, $7); }
        | FUNC34 '(' e ',' e ',' e ',' e ')' { $$ = new_op1x($1, $3, new_op3(OP_COMMA_, $5, $7, $9)); }
        | FUNC35 '(' e ',' e ',' e ')'  { $$ = new_op3($1, $3, $5, $7); } /* XXX: hack for FV, PMT, PV */
        | FUNC35 '(' e ',' e ',' e ',' e ')' { $$ = new_op1x($1, $3, new_op3(OP_COMMA_, $5, $7, $9)); }
        | FUNC35 '(' e ',' e ',' e ',' e ',' e ')' { $$ = new_op1x($1, $3, new_op2(OP_COMMA_, $5,
                                                                                   new_op3(OP_COMMA_, $7, $9, $11))); }
        | BADFUNC '(' ')'               { $$ = new_op1(OP__BADFUNC, new_str($1)); }
        | BADFUNC '(' expr_list ')'     { $$ = new_op1x(OP__BADFUNC, new_str($1), $3); }
        | BADNAME                       { $$ = new_op1(OP__BADNAME, new_str($1)); }
        ;

/* expressions */
e:        e '+' e                   { $$ = new_op2(OP_ADD_, $1, $3); }
        | e '-' e                   { $$ = new_op2(OP_MINUS_, $1, $3); }
        | e '*' e                   { $$ = new_op2(OP_MULTIPLY_, $1, $3); }
        | e '/' e                   { $$ = new_op2(OP_DIVIDE_, $1, $3); }
        | e '^' e                   { $$ = new_op2(OP_POW_, $1, $3); }
        | e '!' e                   { $$ = new_op2(OP_BANG_, $1, $3); }
        | e ':' e                   { $$ = new_op2(OP_COLON_, $1, $3); }
        | term
        | e '&' e                   { $$ = new_op2(OP_CONCAT_, $1, $3); }
        | e ';' e                   { $$ = new_op2(OP_SEMI_, $1, $3); }
        | e '<' e                   { $$ = new_op2(OP_LT_, $1, $3); }
        | e '=' e                   { $$ = new_op2(OP_EQ_, $1, $3); }
        | e '>' e                   { $$ = new_op2(OP_GT_, $1, $3); }
        | e T_GTE e                 { $$ = new_op2(OP_GTE_, $1, $3); }
        | e T_LTE e                 { $$ = new_op2(OP_LTE_, $1, $3); }
        | e T_NE e                  { $$ = new_op2(OP_NE_, $1, $3); }
        | e T_NE2 e                 { $$ = new_op2(OP_NE2_, $1, $3); }
        ;

expr_list: e                        { $$ = $1; }
        | e ',' expr_list           { $$ = new_op2(OP_COMMA_, $1, $3); }
        ;

range:    RANGE                     { $$ = $1; }
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
        | VAR                       { $$ = get_strarg(sht, $1); }
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
        | K_MOUSE                   { screen_mouseon(); }
        | not K_MOUSE               { screen_mouseoff(); }
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
