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

static void doerror(SCXMEM char *s) {
    error("%s", s2c(s));
    scxfree(s);
}

static int doreadfile(SCXMEM char *fname, int eraseflg) {
    int ret = readfile(s2c(fname), eraseflg);
    scxfree(fname);
    return ret;
}

static int dowritefile(SCXMEM char *fname, int r0, int c0, int rn, int cn) {
    int ret = writefile(s2c(fname), r0, c0, rn, cn);
    scxfree(fname);
    return ret;
}

static void doprintfile(SCXMEM char *fname, int r0, int c0, int rn, int cn) {
    printfile(s2c(fname), r0, c0, rn, cn);
    scxfree(fname);
}

static void dotblprintfile(SCXMEM char *fname, int r0, int c0, int rn, int cn) {
    tblprintfile(s2c(fname), r0, c0, rn, cn);
    scxfree(fname);
}

static void dosortrange(struct ent *left, struct ent *right, SCXMEM char *criteria) {
    sortrange(left, right, s2c(criteria));
    scxfree(criteria);
}

static void doformat_cell(struct ent *v1, struct ent *v2, SCXMEM char *s) {
    format_cell(v1, v2, s2c(s));
    scxfree(s);
}

static void dostr_search(SCXMEM char *s, int firstrow, int firstcol, int lastrow,
                         int lastcol, int num)
{
    str_search(s2c(s), firstrow, firstcol, lastrow, lastcol, num);
    scxfree(s);
}

static void dosetformat(int n, SCXMEM char *s) {
    cmd_setformat(n, s2c(s));
    scxfree(s);
}

static void doadd_range(SCXMEM char *name,
                        struct ent_ptr left, struct ent_ptr right,
                        int is_range)
{
    add_range(s2c(name), left, right, is_range);
    scxfree(name);
}

static void dodefine(SCXMEM char *name) {
    cmd_define(s2c(name));
    scxfree(name);
}

static void doadd_abbr(SCXMEM char *name) {
    add_abbr(s2c(name));
    scxfree(name);
}

static void dodel_abbr(SCXMEM char *name) {
    del_abbr(s2c(name));
    scxfree(name);
}

static void doselect(SCXMEM char *str) {
    cmd_select_qbuf(*s2c(str));
    scxfree(str);
}

static void dogetrange(SCXMEM char *name, int fd) {
    getrange(s2c(name), fd);
    scxfree(name);
}

static void doaddplugin(SCXMEM char *ext, SCXMEM char *plugin, char type) {
    addplugin(s2c(ext), s2c(plugin), type);
    scxfree(ext);
    scxfree(plugin);
}

static void dorun(SCXMEM char *str) {
    cmd_run(s2c(str));
    scxfree(str);
}

static int doplugin(SCXMEM char *str) {
    int ret = cmd_plugin(s2c(str));
    scxfree(str);
    return ret;
}

static void doeval(struct enode *e, SCXMEM char *fmt, int row, int col, int fd) {
    cmd_eval(e, s2c(fmt), row, col, fd);
    scxfree(fmt);
    efree(e);
}

static void doseval(struct enode *e, int row, int col, int fd) {
    cmd_seval(e, row, col, fd);
    efree(e);
}

static void doquery(SCXMEM char *s, SCXMEM char *data, int fd) {
    cmd_query(s2c(s), s2c(data), fd);
    scxfree(s);
    scxfree(data);
}

static void domdir(SCXMEM char *str) {
    set_mdir(s2c(str));
    scxfree(str);
}

static void doautorun(SCXMEM char *str) {
    set_autorun(s2c(str));
    scxfree(str);
}

static void dofkey(int n, SCXMEM char *str) {
    set_fkey(n, s2c(str));
    scxfree(str);
}

static void dohistfile(SCXMEM char *str) {
    set_histfile(s2c(str));
    scxfree(str);
}

%}

%union {
    int ival;
    double fval;
    struct ent_ptr ent;
    struct enode *enode;
    char *sval;
    struct range_s rval;
}

%type <ent> var
%type <fval> num
%type <rval> range
%type <rval> var_or_range
%type <sval> strarg
%type <enode> e term expr_list
%token <sval> STRING
%token <ival> NUMBER
%token <fval> FNUMBER
%token <rval> RANGE
%token <rval> VAR
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

command:  S_LET var_or_range '=' e
                                { let($2.left.vp, $4); }
        | S_LET var_or_range '=' { unlet($2.left.vp); }
        | S_LABEL var_or_range '=' e
                                { slet($2.left.vp, $4, ALIGN_CENTER); }
        | S_LEFTSTRING var_or_range '=' e
                                { slet($2.left.vp, $4, ALIGN_LEFT); }
        | S_RIGHTSTRING var_or_range '=' e
                                { slet($2.left.vp, $4, ALIGN_RIGHT); }
        | S_LEFTJUSTIFY var_or_range
                                { range_align($2.left.vp->row, $2.left.vp->col,
                                              $2.right.vp->row, $2.right.vp->col,
                                              ALIGN_LEFT); }
        | S_LEFTJUSTIFY         { if (showrange)
                                    range_align(showsr, showsc, currow, curcol, ALIGN_LEFT); }
        | S_RIGHTJUSTIFY var_or_range
                                { range_align($2.left.vp->row, $2.left.vp->col,
                                              $2.right.vp->row, $2.right.vp->col,
                                              ALIGN_RIGHT); }
        | S_RIGHTJUSTIFY        { if (showrange)
                                    range_align(showsr, showsc, currow, curcol, ALIGN_RIGHT); }
        | S_CENTER var_or_range
                                { range_align($2.left.vp->row, $2.left.vp->col,
                                              $2.right.vp->row, $2.right.vp->col, ALIGN_CENTER); }
        | S_CENTER              { if (showrange)
                                    range_align(showsr, showsc, currow, curcol, ALIGN_CENTER); }
        | S_ADDNOTE var         { if (showrange)
                                    addnote($2.vp, currow, curcol, showsr, showsc);
                                  else
                                    addnote($2.vp, currow, curcol, currow, curcol); }
        | S_ADDNOTE var var_or_range
                                { addnote($2.vp, $3.left.vp->row, $3.left.vp->col,
                                          $3.right.vp->row, $3.right.vp->col); }
        | S_DELNOTE var         { delnote($2.vp); }
        | S_DELNOTE             { delnote(lookat(currow, curcol)); }
        | S_FORMAT COL ':' COL NUMBER NUMBER NUMBER
                                { cmd_format($2, $4, $5, $6, $7); }
        | S_FORMAT COL NUMBER NUMBER NUMBER
                                { cmd_format($2, $2, $3, $4, $5); }
        | S_FORMAT COL ':' COL NUMBER NUMBER
                                { cmd_format($2, $4, $5, $6, REFMTFIX); }
        | S_FORMAT COL NUMBER NUMBER
                                { cmd_format($2, $2, $3, $4, REFMTFIX); }
        | S_FORMAT NUMBER '=' STRING
                                { dosetformat($2, $4); }
        | S_GET strarg          { doreadfile($2, 1); }
        | S_MERGE strarg        { doreadfile($2, 0); }
        | S_MDIR strarg         { domdir($2); }
        | S_AUTORUN strarg      { doautorun($2); }
        | S_FKEY NUMBER '=' strarg { dofkey($2, $4); }
        | S_HISTFILE strarg     { dohistfile($2); }
        | S_SCEXT strarg        { set_string(&scext, $2); }
        | S_ASCEXT strarg       { set_string(&ascext, $2); }
        | S_TBL0EXT strarg      { set_string(&tbl0ext, $2); }
        | S_TBLEXT strarg       { set_string(&tblext, $2); }
        | S_LATEXEXT strarg     { set_string(&latexext, $2); }
        | S_SLATEXEXT strarg    { set_string(&slatexext, $2); }
        | S_TEXEXT strarg       { set_string(&texext, $2); }
        | S_PUT strarg range    { dowritefile($2, $3.left.vp->row, $3.left.vp->col,
                                              $3.right.vp->row, $3.right.vp->col); }
        | S_PUT strarg          { dowritefile($2, 0, 0, maxrow, maxcol); }
        | S_PUT range           { write_cells(stdout,
                                              $2.left.vp->row, $2.left.vp->col,
                                              $2.right.vp->row, $2.right.vp->col,
                                              $2.left.vp->row, $2.left.vp->col); }
        | S_PUT range '/' var_or_range
                                { write_cells(stdout,
                                              $2.left.vp->row, $2.left.vp->col,
                                              $2.right.vp->row, $2.right.vp->col,
                                              $4.left.vp->row, $4.left.vp->col); }
        | S_PUT '%' '/' var_or_range
                                { write_cells(stdout, 0, 0, maxrow, maxcol,
                                              $4.left.vp->row, $4.left.vp->col); }
        | S_PUT '/' var_or_range
                                { write_cells(stdout,
                                              showsr, showsc, currow, curcol,
                                              $3.left.vp->row, $3.left.vp->col); }
        | S_PUT '%'             { write_cells(stdout, 0, 0, maxrow, maxcol, 0, 0); }
        | S_PUT                 { write_cells(stdout, 0, 0, maxrow, maxcol, 0, 0); }
        | S_WRITE strarg range  { doprintfile($2, $3.left.vp->row, $3.left.vp->col,
                                              $3.right.vp->row, $3.right.vp->col); }
        | S_WRITE strarg        { doprintfile($2, 0, 0, maxrow, maxcol); }
        | S_WRITE range         { doprintfile(NULL,
                                              $2.left.vp->row, $2.left.vp->col,
                                              $2.right.vp->row, $2.right.vp->col); }
        | S_WRITE '%'           { doprintfile(NULL, 0, 0, maxrow, maxcol); }
        | S_WRITE               { doprintfile(NULL, 0, 0, maxrow, maxcol); }
        | S_TBL strarg range    { dotblprintfile($2, $3.left.vp->row, $3.left.vp->col,
                                                 $3.right.vp->row, $3.right.vp->col); }
        | S_TBL strarg          { dotblprintfile($2, 0, 0, maxrow, maxcol); }
        | S_SHOW COL ':' COL    { showcol($2, $4); }
        | S_SHOW NUMBER ':' NUMBER
                                { showrow($2, $4); }
        | S_HIDE                { dohide(); }
        | S_HIDE COL            { hidecols($2, $2); }
        | S_HIDE COL ':' COL    { hidecols($2, $4); }
        | S_HIDE NUMBER         { hiderows($2, $2); }
        | S_HIDE NUMBER ':' NUMBER { hiderows($2, $4); }
        | S_COPY                { docopy(); }
        | S_COPY range          { copy($2.left.vp, $2.right.vp, NULL, NULL); }
        | S_COPY range var_or_range
                                { copy($2.left.vp, $2.right.vp,
                                       $3.left.vp, $3.right.vp); }
        | S_MOVE var            { mover($2.vp, lookat(showsr, showsc),
                                        lookat(currow, curcol)); }
        | S_MOVE var var_or_range { mover($2.vp, $3.left.vp, $3.right.vp); }
        | S_ERASE               { eraser(lookat(showsr, showsc),
                                         lookat(currow, curcol)); }
        | S_ERASE var_or_range  { eraser($2.left.vp, $2.right.vp); }
        | S_YANK                { yankr(lookat(showsr, showsc),
                                        lookat(currow, curcol)); }
        | S_YANK var_or_range   { yankr($2.left.vp, $2.right.vp); }
        | S_VALUE               { valueize_area(showsr, showsc, currow, curcol); }
        | S_VALUE var_or_range  { valueize_area($2.left.vp->row, $2.left.vp->col,
                                                $2.right.vp->row, $2.right.vp->col); }
        | S_FILL var_or_range num num
                                { fill($2.left.vp, $2.right.vp, $3, $4); }
        | S_SORT                { dosortrange(lookat(showsr, showsc),
                                              lookat(currow, curcol), NULL); }
        | S_SORT range          { dosortrange($2.left.vp, $2.right.vp, NULL); }
        | S_SORT range strarg   { dosortrange($2.left.vp, $2.right.vp, $3); }
        | S_FMT var_or_range STRING
                                { doformat_cell($2.left.vp, $2.right.vp, $3); }
        | S_LOCK                { lock_cells(lookat(showsr, showsc),
                                             lookat(currow, curcol)); }
        | S_LOCK var_or_range   { lock_cells($2.left.vp, $2.right.vp); }
        | S_UNLOCK              { unlock_cells(lookat(showsr, showsc),
                                               lookat(currow, curcol)); }
        | S_UNLOCK var_or_range { unlock_cells($2.left.vp, $2.right.vp); }
        | S_GOTO var_or_range var_or_range
                                { moveto($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col,
                                         $3.left.vp->row, $3.left.vp->col); }
        | S_GOTO var_or_range   { moveto($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col,
                                         -1, -1); }
        | S_GOTO num range      { num_search($2, $3.left.vp->row, $3.left.vp->col,
                                             $3.right.vp->row, $3.right.vp->col, 0); }
        | S_GOTO num            { num_search($2, 0, 0, maxrow, maxcol, 0); }
        | S_GOTO errlist        { /* code is executed in errlist rules */ }
        | S_GOTO STRING range   { dostr_search($2, $3.left.vp->row, $3.left.vp->col,
                                               $3.right.vp->row, $3.right.vp->col, 0); }
        | S_GOTO '#' STRING range
                                { dostr_search($3, $4.left.vp->row, $4.left.vp->col,
                                               $4.right.vp->row, $4.right.vp->col, 1); }
        | S_GOTO '%' STRING range
                                { dostr_search($3, $4.left.vp->row, $4.left.vp->col,
                                               $4.right.vp->row, $4.right.vp->col, 2); }
        | S_GOTO STRING         { dostr_search($2, 0, 0, maxrow, maxcol, 0); }
        | S_GOTO '#' STRING     { dostr_search($3, 0, 0, maxrow, maxcol, 1); }
        | S_GOTO '%' STRING     { dostr_search($3, 0, 0, maxrow, maxcol, 2); }
        | S_GOTO                { go_last(); }
        | S_GOTO WORD           { /* don't repeat last goto on "unintelligible word" */ }
        | S_DEFINE strarg       { dodefine($2); }
        | S_DEFINE strarg range { doadd_range($2, $3.left, $3.right, 1); }
        | S_DEFINE strarg var   { doadd_range($2, $3, $3, 0); }
        | S_UNDEFINE var_or_range
                                { del_range($2.left.vp, $2.right.vp); }
        | S_ABBREV STRING       { doadd_abbr($2); }
        | S_ABBREV              { doadd_abbr(NULL); }
        | S_UNABBREV STRING     { dodel_abbr($2); }
        | S_FRAME range range   { add_frange($2.left.vp, $2.right.vp,
                                             $3.left.vp, $3.right.vp,
                                             0, 0, 0, 0); }
        | S_FRAME range         { if (showrange) {
                                    showrange = 0;
                                    add_frange($2.left.vp, $2.right.vp,
                                               lookat(showsr, showsc),
                                               lookat(currow, curcol),
                                               0, 0, 0, 0);
                                  } else {
                                    struct frange *cfr = find_frange(currow, curcol);
                                    if (cfr) {
                                        add_frange(cfr->or_left, cfr->or_right,
                                                   $2.left.vp, $2.right.vp,
                                                   0, 0, 0, 0);
                                    }
                                  }
                                }
        | S_FRAME               { struct frange *cfr = find_frange(currow, curcol);
                                  if (showrange && cfr) {
                                      showrange = 0;
                                      add_frange(cfr->or_left, cfr->or_right,
                                                 lookat(showsr, showsc),
                                                 lookat(currow, curcol),
                                                 0, 0, 0, 0);
                                  } else {
                                      error("Need both outer and inner"
                                            " ranges to create frame");
                                  }
                                }
        | S_FRAMETOP range NUMBER
                                { add_frange($2.left.vp, $2.right.vp,
                                             NULL, NULL, $3, -1, -1, -1); }
        | S_FRAMETOP NUMBER     { struct frange *cfr = find_frange(currow, curcol);
                                  if (cfr)
                                      add_frange(cfr->or_left, cfr->or_right,
                                                 NULL, NULL, $2, -1, -1, -1); }
        | S_FRAMEBOTTOM range NUMBER
                                { add_frange($2.left.vp, $2.right.vp,
                                             NULL, NULL, -1, $3, -1, -1); }
        | S_FRAMEBOTTOM NUMBER  { struct frange *cfr = find_frange(currow, curcol);
                                  if (cfr)
                                      add_frange(cfr->or_left, cfr->or_right,
                                                 NULL, NULL, -1, $2, -1, -1); }
        | S_FRAMELEFT range NUMBER
                                { add_frange($2.left.vp, $2.right.vp,
                                             NULL, NULL, -1, -1, $3, -1); }
        | S_FRAMELEFT NUMBER    { struct frange *cfr = find_frange(currow, curcol);
                                  if (cfr)
                                      add_frange(cfr->or_left, cfr->or_right,
                                                 NULL, NULL, -1, -1, $2, -1); }
        | S_FRAMERIGHT range NUMBER
                                { add_frange($2.left.vp, $2.right.vp,
                                             NULL, NULL, -1, -1, -1, $3); }
        | S_FRAMERIGHT NUMBER   { struct frange *cfr = find_frange(currow, curcol);
                                  if (cfr)
                                      add_frange(cfr->or_left, cfr->or_right,
                                                 NULL, NULL, -1, -1, -1, $2); }
        | S_UNFRAME range       { add_frange($2.left.vp, $2.right.vp,
                                             NULL, NULL, 0, 0, 0, 0); }
        | S_UNFRAME             { struct frange *cfr = find_frange(currow, curcol);
                                  if (cfr)
                                      add_frange(cfr->or_left, cfr->or_right,
                                                 NULL, NULL, 0, 0, 0, 0); }
        | S_COLOR NUMBER '='    { initcolor($2); }
        | S_COLOR NUMBER '=' e  { change_color($2, $4); }
        | S_COLOR range NUMBER  { add_crange($2.left.vp, $2.right.vp, $3); }
        | S_SET setlist         { modflg++; }
        | S_UP                  { backrow(1); }
        | S_UP NUMBER           { backrow($2); }
        | S_DOWN                { forwrow(1); }
        | S_DOWN NUMBER         { forwrow($2); }
        | S_LEFT                { backcol(1); }
        | S_LEFT NUMBER         { backcol($2); }
        | S_RIGHT               { forwcol(1); }
        | S_RIGHT NUMBER        { forwcol($2); }
        | S_ENDUP               { doend(-1,  0); }
        | S_ENDDOWN             { doend( 1,  0); }
        | S_ENDLEFT             { doend( 0, -1); }
        | S_ENDRIGHT            { doend( 0,  1); }
        | S_SELECT STRING       { doselect($2); }
        | S_INSERTROW           { insertrow( 1, 0); }
        | S_INSERTROW '*' NUMBER { insertrow($3, 0); }
        | S_OPENROW             { insertrow( 1, 1); }
        | S_OPENROW '*' NUMBER  { insertrow($3, 1); }
        | S_INSERTCOL           { insertcol( 1, 0); }
        | S_INSERTCOL '*' NUMBER { insertcol($3, 0); }
        | S_OPENCOL             { insertcol( 1, 1); }
        | S_OPENCOL '*' NUMBER  { insertcol($3, 1); }
        | S_DELETEROW           { if (showrange == SHOWROWS)
                                      deletecols(showsr, currow);
                                  else
                                      deletecols(currow, currow);
                                }
        | S_DELETEROW '*' NUMBER { deleterows(currow, currow + $3 - 1); }
        | S_DELETEROW NUMBER    { deleterows($2, $2); }
        | S_DELETEROW NUMBER ':' NUMBER { deleterows($2, $4); }
        | S_DELETECOL           { if (showrange == SHOWCOLS)
                                      deletecols(showsc, curcol);
                                  else
                                      deletecols(curcol, curcol);
                                }
        | S_DELETECOL COL       { deletecols($2, $2); }
        | S_DELETECOL '*' NUMBER { deletecols(curcol, curcol + $3 - 1); }
        | S_DELETECOL COL ':' COL { deletecols($2, $4); }
        | S_YANKROW             { if (showrange == SHOWROWS)
                                      yankrows(showsr, currow);
                                  else
                                      yankrows(currow, currow);
                                }
        | S_YANKROW '*' NUMBER  { yankrows(currow, currow + $3 - 1); }
        | S_YANKROW NUMBER      { yankrows($2, $2); }
        | S_YANKROW NUMBER ':' NUMBER { yankrows($2, $4); }
        | S_YANKCOL             { if (showrange == SHOWCOLS)
                                      yankcols(showsc, curcol);
                                  else
                                      yankcols(curcol, curcol);
                                }
        | S_YANKCOL NUMBER      { yankcols($2, $2); }
        | S_YANKCOL '*' NUMBER  { yankcols(curcol, curcol + $3 - 1); }
        | S_YANKCOL COL ':' COL { yankcols($2, $4); }
        | S_PULL                { pullcells('p'); }
        | S_PULLMERGE           { pullcells('m'); }
        | S_PULLROWS            { pullcells('r'); }
        | S_PULLCOLS            { pullcells('c'); }
        | S_PULLXCHG            { pullcells('x'); }
        | S_PULLTP              { pullcells('t'); }
        | S_PULLFMT             { pullcells('f'); }
        | S_PULLCOPY            { copy(NULL, NULL, NULL, NULL); }
        | S_PULLCOPY var_or_range { // XXX: fix this ugly hack
                                    copy($2.left.vp, $2.right.vp, NULL, (struct ent *)1); }
        | S_WHEREAMI            { cmd_whereami(macrofd); }
        | S_WHEREAMI '|' NUMBER { cmd_whereami($3); }
        | S_GETNUM var_or_range { getnum($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col, macrofd); }
        | S_GETNUM var_or_range '|' NUMBER
                                { getnum($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col, $4); }
        | S_GETNUM              { getnum(currow, curcol, currow, curcol, macrofd); }
        | S_GETNUM '|' NUMBER   { getnum(currow, curcol, currow, curcol, $3); }
        | S_FGETNUM var_or_range { fgetnum($2.left.vp->row, $2.left.vp->col,
                                           $2.right.vp->row, $2.right.vp->col, macrofd); }
        | S_FGETNUM var_or_range '|' NUMBER
                                { fgetnum($2.left.vp->row, $2.left.vp->col,
                                          $2.right.vp->row, $2.right.vp->col, $4); }
        | S_FGETNUM             { fgetnum(currow, curcol, currow, curcol, macrofd); }
        | S_FGETNUM '|' NUMBER  { fgetnum(currow, curcol, currow, curcol, $3); }
        | S_GETSTRING var_or_range
                                { getstring($2.left.vp->row, $2.left.vp->col,
                                            $2.right.vp->row, $2.right.vp->col, macrofd); }
        | S_GETSTRING var_or_range '|' NUMBER
                                { getstring($2.left.vp->row, $2.left.vp->col,
                                            $2.right.vp->row, $2.right.vp->col, $4); }
        | S_GETSTRING           { getstring(currow, curcol, currow, curcol, macrofd); }
        | S_GETSTRING '|' NUMBER { getstring(currow, curcol, currow, curcol, $3); }
        | S_GETEXP var_or_range { getexp($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col, macrofd); }
        | S_GETEXP var_or_range '|' NUMBER
                                { getexp($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col, $4); }
        | S_GETEXP              { getexp(currow, curcol, currow, curcol, macrofd); }
        | S_GETEXP '|' NUMBER   { getexp(currow, curcol, currow, curcol, $3); }
        | S_GETFORMAT COL       { getformat($2, macrofd); }
        | S_GETFORMAT COL '|' NUMBER
                                { getformat($2, $4); }
        | S_GETFORMAT           { getformat(curcol, macrofd); }
        | S_GETFORMAT '|' NUMBER { getformat(curcol, $3); }
        | S_GETFMT var_or_range { getfmt($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col, macrofd); }
        | S_GETFMT var_or_range '|' NUMBER
                                { getfmt($2.left.vp->row, $2.left.vp->col,
                                         $2.right.vp->row, $2.right.vp->col, $4); }
        | S_GETFMT              { getfmt(currow, curcol, currow, curcol, macrofd); }
        | S_GETFMT '|' NUMBER   { getfmt(currow, curcol, currow, curcol, $3); }
        | S_GETFRAME            { getframe(macrofd); }
        | S_GETFRAME '|' NUMBER { getframe($3); }
        | S_GETRANGE STRING     { dogetrange($2, macrofd); }
        | S_GETRANGE STRING '|' NUMBER
                                { dogetrange($2, $4); }
        | S_EVAL e              { doeval($2, NULL, currow, curcol, macrofd); }
        | S_EVAL e STRING       { doeval($2, $3, currow, curcol, macrofd); }
        | S_EVAL e STRING '|' NUMBER
                                { doeval($2, $3, currow, curcol, $5); }
        | S_SEVAL e             { doseval($2, currow, curcol, macrofd); }
        | S_QUERY STRING STRING { doquery($2, $3, macrofd); }
        | S_QUERY STRING STRING '|' NUMBER
                                { doquery($2, $3, $5); }
        | S_QUERY STRING        { doquery($2, NULL, macrofd); }
        | S_QUERY STRING '|' NUMBER
                                { doquery($2, NULL, $4); }
        | S_QUERY               { doquery(NULL, NULL, macrofd); }
        | S_QUERY '|' NUMBER    { doquery(NULL, NULL, $3); }
        | S_GETKEY              { dogetkey(macrofd); }
        | S_ERROR STRING        { doerror($2); }
        | S_STATUS              { cmd_status(macrofd); }
        | S_STATUS '|' NUMBER   { cmd_status($3); }
        | S_RECALC              { cmd_recalc(); }
        | S_REDRAW              { cmd_redraw(); }
        | S_QUIT                { stopdisp(); exit(0); }
        | S_RUN STRING          { dorun($2); }
        | S_PLUGIN STRING '=' STRING
                                { doaddplugin($2, $4, 'r'); }
        | S_PLUGOUT STRING '=' STRING
                                { doaddplugin($2, $4, 'w'); }
        | PLUGIN                { doplugin($1); }
        | /* nothing */
        | error;

term:     var                   { $$ = new_var(O_VAR, $1); }
        | '@' K_FIXED term      { $$ = new('f', $3, NULL); }
        | '(' '@' K_FIXED ')' term
                                { $$ = new('F', $5, NULL); }
        | '@' K_SUM '(' var_or_range ')'
                                { $$ = new(SUM, new_range(REDUCE | SUM, $4), NULL); }
        | '@' K_SUM '(' range ',' e ')'
                                { $$ = new(SUM, new_range(REDUCE | SUM, $4), $6); }
        | '@' K_PROD '(' var_or_range ')'
                                { $$ = new(PROD, new_range(REDUCE | PROD, $4), NULL); }
        | '@' K_PROD '(' range ',' e ')'
                                { $$ = new(PROD, new_range(REDUCE | PROD, $4), $6); }
        | '@' K_AVG '(' var_or_range ')'
                                { $$ = new(AVG, new_range(REDUCE | AVG, $4), NULL); }
        | '@' K_AVG '(' range ',' e ')'
                                { $$ = new(AVG, new_range(REDUCE | AVG, $4), $6); }
        | '@' K_STDDEV '(' var_or_range ')'
                                { $$ = new(STDDEV, new_range(REDUCE | STDDEV, $4), NULL); }
        | '@' K_STDDEV '(' range ',' e ')'
                                { $$ = new(STDDEV, new_range(REDUCE | STDDEV, $4), $6); }
        | '@' K_COUNT '(' var_or_range ')'
                                { $$ = new(COUNT, new_range(REDUCE | COUNT, $4), NULL); }
        | '@' K_COUNT '(' range ',' e ')'
                                { $$ = new(COUNT, new_range(REDUCE | COUNT, $4), $6); }
        | '@' K_MAX '(' var_or_range ')'
                                { $$ = new(MAX, new_range(REDUCE | MAX, $4), NULL); }
        | '@' K_MAX '(' range ',' e ')'
                                { $$ = new(MAX, new_range(REDUCE | MAX, $4), $6); }
        | '@' K_MAX '(' e ',' expr_list ')'
                                { $$ = new(LMAX, $6, $4); }
        | '@' K_MIN '(' var_or_range ')'
                                { $$ = new(MIN, new_range(REDUCE | MIN, $4), NULL); }
        | '@' K_MIN '(' range ',' e ')'
                                { $$ = new(MIN, new_range(REDUCE | MIN, $4), $6); }
        | '@' K_MIN '(' e ',' expr_list ')'
                                { $$ = new(LMIN, $6, $4); }
        | '@' K_ROWS '(' var_or_range ')'
                                { $$ = new_range(REDUCE | 'R', $4); }
        | '@' K_COLS '(' var_or_range ')'
                                { $$ = new_range(REDUCE | 'C', $4); }

        | '@' K_ABS '(' e ')'           { $$ = new(ABS, $4, NULL); }
        | '@' K_ACOS '(' e ')'          { $$ = new(ACOS, $4, NULL); }
        | '@' K_ASIN '(' e ')'          { $$ = new(ASIN, $4, NULL); }
        | '@' K_ATAN '(' e ')'          { $$ = new(ATAN, $4, NULL); }
        | '@' K_ATAN2 '(' e ',' e ')'   { $$ = new(ATAN2, $4, $6); }
        | '@' K_CEIL '(' e ')'          { $$ = new(CEIL, $4, NULL); }
        | '@' K_COS '(' e ')'           { $$ = new(COS, $4, NULL); }
        | '@' K_EXP '(' e ')'           { $$ = new(EXP, $4, NULL); }
        | '@' K_FABS '(' e ')'          { $$ = new(FABS, $4, NULL); }
        | '@' K_FLOOR '(' e ')'         { $$ = new(FLOOR, $4, NULL); }
        | '@' K_HYPOT '(' e ',' e ')'   { $$ = new(HYPOT, $4, $6); }
        | '@' K_LN '(' e ')'            { $$ = new(LOG, $4, NULL); }
        | '@' K_LOG '(' e ')'           { $$ = new(LOG10, $4, NULL); }
        | '@' K_POW '(' e ',' e ')'     { $$ = new(POW, $4, $6); }
        | '@' K_SIN '(' e ')'           { $$ = new(SIN, $4, NULL); }
        | '@' K_SQRT '(' e ')'          { $$ = new(SQRT, $4, NULL); }
        | '@' K_TAN '(' e ')'           { $$ = new(TAN, $4, NULL); }
        | '@' K_DTR '(' e ')'           { $$ = new(DTR, $4, NULL); }
        | '@' K_RTD '(' e ')'           { $$ = new(RTD, $4, NULL); }
        | '@' K_RAND '(' ')'            { $$ = new(RAND, NULL, NULL); }
        | '@' K_RANDBETWEEN '(' e ',' e ')'  { $$ = new(RANDBETWEEN, $4, $6); }
        | '@' K_RND '(' e ')'           { $$ = new(RND, $4, NULL); }
        | '@' K_ROUND '(' e ',' e ')'   { $$ = new(ROUND, $4, $6); }

        | '@' K_IF '(' e ',' e ',' e ')' { $$ = new(IF, $4, new(',', $6, $8)); }

        | '@' K_PV '(' e ',' e ',' e ')' { $$ = new(PV, $4, new(':', $6, $8)); }
        | '@' K_FV '(' e ',' e ',' e ')' { $$ = new(FV, $4, new(':', $6, $8)); }
        | '@' K_PMT '(' e ',' e ',' e ')' { $$ = new(PMT, $4, new(':', $6, $8)); }

        | '@' K_HOUR '(' e ')'          { $$ = new(HOUR, $4, NULL); }
        | '@' K_MINUTE '(' e ')'        { $$ = new(MINUTE, $4, NULL); }
        | '@' K_SECOND '(' e ')'        { $$ = new(SECOND, $4, NULL); }
        | '@' K_MONTH '(' e ')'         { $$ = new(MONTH, $4, NULL); }
        | '@' K_DAY '(' e ')'           { $$ = new(DAY, $4, NULL); }
        | '@' K_YEAR '(' e ')'          { $$ = new(YEAR, $4, NULL); }
        | '@' K_NOW                     { $$ = new(NOW, NULL, NULL); }
        | '@' K_DTS '(' e ',' e ',' e ')'
                                        { $$ = new(DTS, $4, new(',', $6, $8)); }
        | NUMBER '.' NUMBER '.' NUMBER  { $$ = new(DTS,
                                                   new_const(O_CONST, (double)$1),
                                                   new(',', new_const(O_CONST, (double)$3),
                                                       new_const(O_CONST, (double)$5))); }
        | '@' K_TTS '(' e ',' e ',' e ')'
                                        { $$ = new(TTS, $4, new(',', $6, $8)); }
        | '@' K_STON '(' e ')'          { $$ = new(STON, $4, NULL); }
        | '@' K_EQS '(' e ',' e ')'     { $$ = new(EQS, $4, $6); }
        | '@' K_DATE '(' e ')'          { $$ = new(DATE, $4, NULL); }
        | '@' K_DATE '(' e ',' e ')'    { $$ = new(DATE, $4, $6); }
        | '@' K_FMT '(' e ',' e ')'     { $$ = new(FMT, $4, $6); }
        | '@' K_UPPER '(' e ')'         { $$ = new(UPPER, $4, NULL); }
        | '@' K_LOWER '(' e ')'         { $$ = new(LOWER, $4, NULL); }
        | '@' K_CAPITAL '(' e ')'       { $$ = new(CAPITAL, $4, NULL); }
        | '@' K_INDEX '(' range ',' e ')'
                { $$ = new(INDEX, new_range(REDUCE | INDEX, $4), $6); }
        | '@' K_INDEX '(' e ',' range ')'
                { $$ = new(INDEX, new_range(REDUCE | INDEX, $6), $4); }
        | '@' K_INDEX '(' range ',' e ',' e ')'
                { $$ = new(INDEX, new_range(REDUCE | INDEX, $4), new(',', $6, $8)); }
        | '@' K_LOOKUP '(' range ',' e ')'
                { $$ = new(LOOKUP, new_range(REDUCE | LOOKUP, $4), $6); }
        | '@' K_LOOKUP '(' e ',' range ')'
                { $$ = new(LOOKUP, new_range(REDUCE | LOOKUP, $6), $4); }
        | '@' K_HLOOKUP '(' range ',' e ',' e ')'
                { $$ = new(HLOOKUP, new_range(REDUCE | HLOOKUP, $4), new(',', $6, $8)); }
        | '@' K_HLOOKUP '(' e ',' range ',' e ')'
                { $$ = new(HLOOKUP, new_range(REDUCE | HLOOKUP, $6), new(',', $4, $8)); }
        | '@' K_VLOOKUP '(' range ',' e ',' e ')'
                { $$ = new(VLOOKUP, new_range(REDUCE | VLOOKUP, $4), new(',', $6, $8)); }
        | '@' K_VLOOKUP '(' e ',' range ',' e ')'
                { $$ = new(VLOOKUP, new_range(REDUCE | VLOOKUP, $6), new(',', $4, $8)); }
        | '@' K_STINDEX '(' range ',' e ')'
                { $$ = new(STINDEX, new_range(REDUCE | STINDEX, $4), $6); }
        | '@' K_STINDEX '(' e ',' range ')'
                { $$ = new(STINDEX, new_range(REDUCE | STINDEX, $6), $4); }
        | '@' K_STINDEX '(' range ',' e ',' e ')'
                { $$ = new(STINDEX, new_range(REDUCE | STINDEX, $4), new(',', $6, $8)); }
        | '@' K_EXT '(' e ',' e ')'     { $$ = new(EXT, $4, $6); }
        | '@' K_NVAL '(' e ',' e ')'    { $$ = new(NVAL, $4, $6); }
        | '@' K_SVAL '(' e ',' e ')'    { $$ = new(SVAL, $4, $6); }
        | '@' K_SUBSTR '(' e ',' e ',' e ')'
                                        { $$ = new(SUBSTR, $4, new(',', $6, $8)); }
        |     '(' e ')'                 { $$ = $2; }
        |     '+' term                  { $$ = $2; }
        |     '-' term                  { $$ = new('m', $2, NULL); }
        |     NUMBER                    { $$ = new_const(O_CONST, (double)$1); }
        |     FNUMBER                   { $$ = new_const(O_CONST, $1); }
        | '@' K_PI                      { $$ = new(PI_, NULL, NULL); }
        |     STRING                    { $$ = new_str($1); }
        |     '~' term                  { $$ = new('!', $2, NULL); }
        |     '!' term                  { $$ = new('!', $2, NULL); }
        | '@' K_FILENAME '(' e ')'      { $$ = new(FILENAME, $4, NULL); }
        | '@' K_MYROW                   { $$ = new(MYROW, NULL, NULL); }
        | '@' K_MYCOL                   { $$ = new(MYCOL, NULL, NULL); }
        | '@' K_LASTROW                 { $$ = new(LASTROW, NULL, NULL); }
        | '@' K_LASTCOL                 { $$ = new(LASTCOL, NULL, NULL); }
        | '@' K_COLTOA '(' e ')'        { $$ = new(COLTOA, $4, NULL); }
        | '@' K_NUMITER                 { $$ = new(NUMITER, NULL, NULL); }
        | '@' K_ERR                     { $$ = new(ERR_, NULL, NULL); }
        |     K_ERR                     { $$ = new(ERR_, NULL, NULL); }
        | '@' K_BLACK                   { $$ = new(BLACK, NULL, NULL); }
        | '@' K_RED                     { $$ = new(RED, NULL, NULL); }
        | '@' K_GREEN                   { $$ = new(GREEN, NULL, NULL); }
        | '@' K_YELLOW                  { $$ = new(YELLOW, NULL, NULL); }
        | '@' K_BLUE                    { $$ = new(BLUE, NULL, NULL); }
        | '@' K_MAGENTA                 { $$ = new(MAGENTA, NULL, NULL); }
        | '@' K_CYAN                    { $$ = new(CYAN, NULL, NULL); }
        | '@' K_WHITE                   { $$ = new(WHITE, NULL, NULL); }
        ;

/* expressions */
e:        e '+' e         { $$ = new('+', $1, $3); }
        | e '-' e         { $$ = new('-', $1, $3); }
        | e '*' e         { $$ = new('*', $1, $3); }
        | e '/' e         { $$ = new('/', $1, $3); }
        | e '%' e         { $$ = new('%', $1, $3); }
        | e '^' e         { $$ = new('^', $1, $3); }
        | term
        | e '?' e ':' e   { $$ = new('?', $1, new(':', $3, $5)); }
        | e ';' e         { $$ = new(';', $1, $3); }
        | e '<' e         { $$ = new('<', $1, $3); }
        | e '=' e         { $$ = new('=', $1, $3); }
        | e '>' e         { $$ = new('>', $1, $3); }
        | e '&' e         { $$ = new('&', $1, $3); }
        | e '|' e         { $$ = new('|', $1, $3); }
        | e '<' '=' e     { $$ = new('!', new('>', $1, $4), NULL); }
        | e '!' '=' e     { $$ = new('!', new('=', $1, $4), NULL); }
        | e '<' '>' e     { $$ = new('!', new('=', $1, $4), NULL); }
        | e '>' '=' e     { $$ = new('!', new('<', $1, $4), NULL); }
        | e '#' e         { $$ = new('#', $1, $3); }
        ;

expr_list: e              { $$ = new(ELIST, NULL, $1); }
        | expr_list ',' e { $$ = new(ELIST, $1, $3); }
        ;

range:    var ':' var     { $$.left = $1; $$.right = $3; }
        | RANGE           { $$ = $1; }
        ;

var:      COL NUMBER        { $$.vp = lookat($2, $1); $$.vf = 0; }
        | '$' COL NUMBER    { $$.vp = lookat($3, $2); $$.vf = FIX_COL; }
        | COL '$' NUMBER    { $$.vp = lookat($3, $1); $$.vf = FIX_ROW; }
        | '$' COL '$' NUMBER { $$.vp = lookat($4, $2); $$.vf = FIX_ROW | FIX_COL; }
        | VAR               { $$ = $1.left; }
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
        | var               { // XXX: should convert numeric value to string according to format
                              $$ = scxdup($1.vp->label ? $1.vp->label : "NULL_STRING"); }
        ;

/* allows >=1 'setitem's to be listed in the same 'set' command */
setlist :
        | setlist setitem
        ;

/* things that you can 'set' */
setitem : K_AUTO                { setautocalc(1); }
        | K_AUTOCALC            { setautocalc(1); }
        | not K_AUTO            { setautocalc(0); }
        | not K_AUTOCALC        { setautocalc(0); }
        | K_BYCOLS              { setcalcorder(BYCOLS); }
        | K_BYROWS              { setcalcorder(BYROWS); }
        | K_OPTIMIZE            { optimize = 1; }
        | not K_OPTIMIZE        { optimize = 0; }
        | K_NUMERIC             { numeric = 1; }
        | not K_NUMERIC         { numeric = 0; }
        | K_PRESCALE            { prescale = 0.01; } // XXX: should use 100.0
        | not K_PRESCALE        { prescale = 1.0; }
        | K_EXTFUN              { extfunc = 1; }
        | not K_EXTFUN          { extfunc = 0; }
        | K_CELLCUR             { showcell = 1; }
        | not K_CELLCUR         { showcell = 0; }
        | K_TOPROW              { showtop = 1; }
        | not K_TOPROW          { showtop = 0; }
        | K_AUTOINSERT          { autoinsert = 1; }
        | not K_AUTOINSERT      { autoinsert = 0; }
        | K_AUTOWRAP            { autowrap = 1; }
        | not K_AUTOWRAP        { autowrap = 0; }
        | K_CSLOP               { cslop = 1; FullUpdate++; }
        | not K_CSLOP           { cslop = 0; FullUpdate++; }
        | K_COLOR               { sc_setcolor(1); }
        | not K_COLOR           { sc_setcolor(0); }
        | K_COLORNEG            { colorneg = 1; }
        | not K_COLORNEG        { colorneg = 0; }
        | K_COLORERR            { colorerr = 1; }
        | not K_COLORERR        { colorerr = 0; }
        | K_BRAILLE             { braille = 1; }
        | not K_BRAILLE         { braille = 0; }
        | K_BACKUP              { dobackups = 1; }
        | not K_BACKUP          { dobackups = 0; }
        | K_MOUSE               { mouseon(); }
        | not K_MOUSE           { mouseoff(); }
        | K_ITERATIONS '=' NUMBER { setiterations($3); }
        | K_TBLSTYLE '=' NUMBER { tbl_style = $3; }
        | K_TBLSTYLE '=' K_TBL  { tbl_style = TBL; }
        | K_TBLSTYLE '=' K_LATEX { tbl_style = LATEX; }
        | K_TBLSTYLE '=' K_SLATEX { tbl_style = SLATEX; }
        | K_TBLSTYLE '=' K_TEX  { tbl_style = TEX; }
        | K_TBLSTYLE '=' K_FRAME  { tbl_style = FRAME; }
        | K_RNDTOEVEN           { rndtoeven = 1; FullUpdate++; }
        | not K_RNDTOEVEN       { rndtoeven = 0; FullUpdate++; }
        | K_CRACTION '=' NUMBER { craction = $3; }
        | K_ROWLIMIT '=' NUMBER { rowlimit = $3; }
        | K_COLLIMIT '=' NUMBER { collimit = $3; }
        | K_PAGESIZE '=' NUMBER { pagesize = $3; }
        | K_SCRC                { scrc++; }
        | K_LOCALE              { sc_set_locale(1); }
        | not K_LOCALE          { sc_set_locale(0); }
        | K_EMACS               { emacs_bindings = 1; }
        | not K_EMACS           { emacs_bindings = 0; }
        ;

/* types of errors, to 'goto' */
errlist:  K_ERROR range         { num_search(0.0, $2.left.vp->row, $2.left.vp->col,
                                             $2.right.vp->row, $2.right.vp->col,
                                             CELLERROR); }
        | K_ERROR               { num_search(0.0, 0, 0,
                                             maxrow, maxcol, CELLERROR); }
        | K_INVALID range       { num_search(0.0, $2.left.vp->row, $2.left.vp->col,
                                             $2.right.vp->row, $2.right.vp->col,
                                             CELLINVALID); }
        | K_INVALID             { num_search(0.0, 0, 0,
                                             maxrow, maxcol, CELLINVALID); }
        ;
