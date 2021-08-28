/*      SC      A Spreadsheet Calculator
 *              Lotus 1-2-3 menu
 *
 *              by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

struct menu_item {
    const char *option;
    const char *desc;
    int (*func)(const void *arg, int n);
    const void *arg;
    int n;
};

static int run_menu(const void *arg, int n);
static int run_copy(const void *arg, int n);
static int run_move(const void *arg, int n);
static int run_abort(const void *arg, int n);
static int run_system(const void *arg, int n);
static int run_quit(const void *arg, int n);

static struct menu_item const top_menu[];
static struct menu_item const worksheet_menu[];
static struct menu_item const range_menu[];
static struct menu_item const file_menu[];
static struct menu_item const print_menu[];
static struct menu_item const graph_menu[];
static struct menu_item const data_menu[];
static struct menu_item const addin_menu[];
static struct menu_item const quit_menu[];

void lotus_menu(void) {
    run_menu(top_menu, 0);
}

#define MENU(o,f,a,n,d) { o, d, f, a, n }
#define MEND() { NULL, NULL, NULL, NULL, 0 }

static struct menu_item const top_menu[] = {
    MENU("Worksheet", run_menu, worksheet_menu, 0,
         "Global  Insert  Delete  Column  Erase  Titles  Window  Status  Page  Learn"),
    MENU("Range", run_menu, range_menu, 0,
         "Format  Label  Erase  Name  Justify  Prot  Unprot  Input  Value  Trans  Search"),
    MENU("Copy", run_copy, NULL, 0,
         "Copy a cell or range of cells"),
    MENU("Move", run_move, NULL, 0,
         "Move a cell or range of cells"),
    MENU("File", run_menu, file_menu, 0,
         "Retrieve  Save  Combine  Xtract  Erase  List  Import  Directory  Admin"),
    MENU("Print", run_menu, print_menu, 0,
         "Print a range on a printer or to a print file"),
    MENU("Graph", run_menu, graph_menu, 0,
         "Type  X  A  B  C  D  E  F  Reset  View  Save  Options  Name  Group  Quit"),
    MENU("Data", run_menu, data_menu, 0,
         "Fill  Table  Sort  Query  Distribution  Matrix  Regression  Parse"),
    MENU("System", run_system, NULL, 0,
         "Leave 1-2-3 temporarily and use operating system"),
    MENU("Add-In", run_menu, addin_menu, 0,
         "Attach, Detach, Invoke or Clear 1-2-3 add-in programs"),
    MENU("Quit", run_menu, quit_menu, 0,
         "End the 1-2-3 session"),
    MEND()
};

static struct menu_item const wgfdt_menu[] = {
    MENU("1 (HH:MM:SS AM/PM)", NULL, NULL, 0,
         ""),
    MENU("2 (HH:MM AM/PM)", NULL, NULL, 0,
         ""),
    MENU("3 (Long Intn'l)", NULL, NULL, 0,
         ""),
    //"Currently configured: HH:MM:SS (24 hour)"
    //"Currently configured: HH.MM.SS (24 hour)"
    //"Currently configured: HH,MM,SS (24 hour)"
    //"Currently configured: HHhMMmSSs (24 hour)"
    MENU("4 (Short Intn'l)", NULL, NULL, 0,
         ""),
    //"Currently configured: HH:MM (24 hour)"
    //"Currently configured: HH.MM (24 hour)"
    //"Currently configured: HH,MM (24 hour)"
    //"Currently configured: HHhMMm (24 hour)"
    MEND()
};

static struct menu_item const wgfd_menu[] = {
    MENU("1 (DD-MMM-YY)", NULL, NULL, 0,
         "Lotus standard long form"),
    MENU("2 (DD-MMM)", NULL, NULL, 0,
         "Lotus standard short form"),
    MENU("3 (MMM-YY)", NULL, NULL, 0,
         ""),
    MENU("4 (Long Intn'l)", NULL, NULL, 0,
         ""),
    //"Currently configured: MM/DD/YY"
    //"Currently configured: DD/MM/YY"
    //"Currently configured: DD.MM.YY"
    //"Currently configured: YY-MM-DD"
    MENU("5 (Short Intn'l)", NULL, NULL, 0,
         ""),
    //"Currently configured: MM/DD"
    //"Currently configured: DD/MM"
    //"Currently configured: DD.MM"
    //"Currently configured: MM-DD"
    MENU("Time", run_menu, wgfdt_menu, 0,
         "Time formats"),
    MEND()
};

static struct menu_item const wgf_menu[] = {
    MENU("Fixed", NULL, NULL, 0,
         "Fixed number of decimal places (x.xx)"),
    //"Enter number of decimal places (0..15): "
    //"Illegal number of decimal places"
    MENU("Sci", NULL, NULL, 0,
         "Exponential format (x.xxE+xx)"),
    MENU("Currency", NULL, NULL, 0,
         "Currency format ($x,xxx.xx)"),
    MENU(",", NULL, NULL, 0,
         "Comma format (x,xxx.xx)"),
    MENU("General", NULL, NULL, 0,
         "Standard format (x.xx or x.xxExx)"),
    MENU("+/-", NULL, NULL, 0,
         "Horizontal bar graph format (+++ or ---)"),
    MENU("Percent", NULL, NULL, 0,
         "Percent format (x.xx%)"),
    MENU("Date", run_menu, wgfd_menu, 0,
         "Date and time formats"),
    MENU("Text", NULL, NULL, 0,
         "Display formula instead of value"),
    MENU("Hidden", NULL, NULL, 0,
         "Do not display cell contents"),
    MEND()
};

static struct menu_item const wgr_menu[] = {
    MENU("Natural", NULL, NULL, 0,
         "Recalculate in natural order"),
    MENU("Columnwise", NULL, NULL, 0,
         "Recalculate column by column"),
    MENU("Rowwise", NULL, NULL, 0,
         "Recalculate row by row"),
    MENU("Automatic", NULL, NULL, 0,
         "Recalculate every time entries change"),
    MENU("Manual", NULL, NULL, 0,
         "Press CALC (F9) to recalculate formulas"),
    MENU("Iteration", NULL, NULL, 0,
         "Specify number of recalculation passes"),
    //"Enter iteration count (1..50): "
    //"Illegal iteration count"
    MEND()
};

static struct menu_item const wgdpi_menu[] = {
    MENU("1", NULL, NULL, 0,
         "110 baud"),
    MENU("2", NULL, NULL, 0,
         "150 baud"),
    MENU("3", NULL, NULL, 0,
         "300 baud"),
    MENU("4", NULL, NULL, 0,
         "600 baud"),
    MENU("5", NULL, NULL, 0,
         "1200 baud"),
    MENU("6", NULL, NULL, 0,
         "2400 baud"),
    MENU("7", NULL, NULL, 0,
         "4800 baud"),
    MENU("8", NULL, NULL, 0,
         "9600 baud"),
    MENU("9", NULL, NULL, 0,
         "19200 baud"),
    MEND()
};

static struct menu_item const wgdpa_menu[] = {
    MENU("Yes", NULL, NULL, 0,
         "Printer automatically issues a line feed after a carriage return"),
    MENU("No", NULL, NULL, 0,
         "Printer does not automatically issue a line feed after a carriage return"),
    MEND()
};

static struct menu_item const wgdpw_menu[] = {
    MENU("Yes", NULL, NULL, 0,
         "Do not wait at the end of a page for paper change"),
    MENU("No", NULL, NULL, 0,
         "Wait at the end of a page for paper change"),
    MEND()
};

static struct menu_item const wgdp_menu[] = {
    MENU("Interface", run_menu, wgdpi_menu, 0,
         "Specify printer interface"),
    MENU("AutoLF", run_menu, wgdpa_menu, 0,
         "Automatic linefeed at end of line"),
    MENU("Left", NULL, NULL, 0,
         "Set default left margin"),
    //"Enter left margin (0..240): "
    //"Illegal left margin"
    MENU("Right", NULL, NULL, 0,
         "Set default right margin"),
    //"Enter right margin (0..240): "
    //"Illegal right margin"
    MENU("Top", NULL, NULL, 0,
         "Set default top margin"),
    //"Enter top margin (0..32): "
    //"Illegal top margin"
    MENU("Bot", NULL, NULL, 0,
         "Set default bottom margin"),
    //"Enter bottom margin (0..32): "
    //"Illegal bottom margin"
    MENU("Pg-Length", NULL, NULL, 0,
         "Set default number of lines per page"),
    //"Enter lines per page (1..100): "
    //"Illegal page length"
    MENU("Wait", run_menu, wgdpw_menu, 0,
         "Wait for paper change at end of each page"),
    MENU("Setup", NULL, NULL, 0,
         "Specify default setup string"),
    //"Enter setup string: "
    MENU("Name", NULL, NULL, 0,
         "Change current printer device"),
    MENU("Quit", NULL, NULL, 0,
         "Return to previous menu"),
    MEND()
};

static struct menu_item const wgdoip_menu[] = {
    MENU("A (.,,)", NULL, NULL, 0,
         "Decimal: Period   Argument separator: Comma       Thousands separator: Comma"),
    MENU("B (,..)", NULL, NULL, 0,
         "Decimal: Comma    Argument separator: Period      Thousands separator: Period"),
    MENU("C (.;,)", NULL, NULL, 0,
         "Decimal: Period   Argument separator: Semicolon   Thousands separator: Comma"),
    MENU("D (,;.)", NULL, NULL, 0,
         "Decimal: Comma    Argument separator: Semicolon   Thousands separator: Period"),
    MENU("E (., )", NULL, NULL, 0,
         "Decimal: Period   Argument separator: Comma       Thousands separator: Space"),
    MENU("F (,. )", NULL, NULL, 0,
         "Decimal: Comma    Argument separator: Period      Thousands separator: Space"),
    MENU("G (.; )", NULL, NULL, 0,
         "Decimal: Period   Argument separator: Semicolon   Thousands separator: Space"),
    MENU("H (,; )", NULL, NULL, 0,
         "Decimal: Comma    Argument separator: Semicolon   Thousands separator: Space"),
    MEND()
};

static struct menu_item const wgdoic_menu[] = {
    MENU("Prefix", NULL, NULL, 0,
         "Currency sign precedes value"),
    MENU("Suffix", NULL, NULL, 0,
         "Currency sign follows value"),
    MEND()
};

static struct menu_item const wgdoid_menu[] = {
    MENU("A (MM/DD/YY)", NULL, NULL, 0,
         "Format D4 will be MM/DD/YY     Format D5 will be MM/DD"),
    MENU("B (DD/MM/YY)", NULL, NULL, 0,
         "Format D4 will be DD/MM/YY     Format D5 will be DD/MM"),
    MENU("C (DD.MM.YY)", NULL, NULL, 0,
         "Format D4 will be DD.MM.YY     Format D5 will be DD.MM"),
    MENU("D (YY-MM-DD)", NULL, NULL, 0,
         "Format D4 will be YY-MM-DD     Format D5 will be MM-DD"),
    MEND()
};

static struct menu_item const wgdoit_menu[] = {
    MENU("A (HH:MM:SS)", NULL, NULL, 0,
         "Format D8 will be HH:MM:SS     Format D9 will be HH:MM"),
    MENU("B (HH.MM.SS)", NULL, NULL, 0,
         "Format D8 will be HH.MM.SS     Format D9 will be HH.MM"),
    MENU("C (HH,MM,SS)", NULL, NULL, 0,
         "Format D8 will be HH,MM,SS     Format D9 will be HH,MM"),
    MENU("D (HHhMMmSSs)", NULL, NULL, 0,
         "Format D8 will be HHhMMmSSs    Format D9 will be HHhMMm"),
    MEND()
};

static struct menu_item const wgdoin_menu[] = {
    MENU("Parentheses", NULL, NULL, 0,
         "Display parentheses around negative numbers"),
    MENU("Sign", NULL, NULL, 0,
         "Display minus sign in front of negative numbers"),
    MEND()
};

static struct menu_item const wgdoi_menu[] = {
    MENU("Punctuation", run_menu, wgdoip_menu, 0,
         "Specify numeric punctuation"),
    MENU("Currency", run_menu, wgdoic_menu, 0,
         "Specify currency sign"),
    //"Enter currency sign: "
    MENU("Date", run_menu, wgdoid_menu, 0,
         "Specify date formats D4 and D5"),
    MENU("Time", run_menu, wgdoit_menu, 0,
         "Specify time formats D8 and D9"),
    MENU("Negative", run_menu, wgdoin_menu, 0,
         "Specify display of negative numbers in , (Comma) and Currency formats"),
    MENU("Quit", NULL, NULL, 0,
         "Return to /Worksheet Global Default menu"),
    MEND()
};

static struct menu_item const wgdoh_menu[] = {
    MENU("Instant", NULL, NULL, 0,
         "Instant access; do not remove Help Disk"),
    MENU("Removable", NULL, NULL, 0,
         "Help Disk can be removed when Help is not in use"),
    MEND()
};

static struct menu_item const wgdoc_menu[] = {
    MENU("Standard", NULL, NULL, 0,
         "Use Lotus standard date and time: DD-MMM-YY  HH:MM AM/PM"),
    MENU("International", NULL, NULL, 0,
         "Use current International settings for date (D4) and time (D9)"),
    MENU("None", NULL, NULL, 0,
         "Hide date-and-time indicator"),
    MENU("Clock", NULL, NULL, 0,
         "Display the date-and-time indicator"),
    MENU("Filename", NULL, NULL, 0,
         "Display current file name instead of date-and-time indicator"),
    MEND()
};

static struct menu_item const wgdou_menu[] = {
    MENU("Yes", NULL, NULL, 0,
         "Turn undo on"),
    MENU("No", NULL, NULL, 0,
         "Turn undo off"),
    MEND()
};

static struct menu_item const wgdob_menu[] = {
    MENU("Yes", NULL, NULL, 0,
         "Computer bell will sound when an error occurs"),
    MENU("No", NULL, NULL, 0,
         "Computer bell will not sound when an error occurs"),
    MEND()
};

static struct menu_item const wgdo_menu[] = {
    MENU("International", run_menu, wgdoi_menu, 0,
         "Punctuation  Currency  Date  Time  Negative  Quit"),
    MENU("Help", run_menu, wgdoh_menu, 0,
         "Select Help access method"),
    MENU("Clock", run_menu, wgdoc_menu, 0,
         "Standard  International  None  Clock  Filename"),
    MENU("Undo", run_menu, wgdou_menu, 0,
         "Turn undo feature on or off"),
    MENU("Beep", run_menu, wgdob_menu, 0,
         "Determine if computer bell will sound when error occurs"),
    MENU("Add-In", NULL, NULL, 0,
         "Specify add-ins to be loaded automatically whenever you start 1-2-3"),
    MEND()
};

static struct menu_item const wgda_menu[] = {
    MENU("Yes", NULL, NULL, 0,
         "Run autoexecute macro when file is retrieved"),
    MENU("No", NULL, NULL, 0,
         "Do not run autoexecute macro when file is retrieved"),
    MEND()
};

static struct menu_item const wgd_menu[] = {
    MENU("Printer", run_menu, wgdp_menu, 0,
         "Specify printer interface and default settings"),
    MENU("Directory", NULL, NULL, 0,
         "Specify default directory"),
    //"Enter default directory: "
    MENU("Status", NULL, NULL, 0,
         "Display all default settings"),
    MENU("Update", NULL, NULL, 0,
         "Save new default settings in configuration file"),
    MENU("Other", run_menu, wgdo_menu, 0,
         "International  Help  Clock  Undo  Beep  Add-In"),
    MENU("Autoexec", run_menu, wgda_menu, 0,
         "Run autoexecute macros when files are retrieved"),
    MENU("Quit", NULL, NULL, 0,
         "Return to READY mode"),
    MEND()
};

static struct menu_item const wgp_menu[] = {
    MENU("Enable", NULL, NULL, 0,
         "Turn worksheet protection on"),
    MENU("Disable", NULL, NULL, 0,
         "Turn worksheet protection off"),
    MEND()
};

static struct menu_item const wgl_menu[] = {
    MENU("Left", NULL, NULL, 0,
         "Left-align labels in cells"),
    MENU("Right", NULL, NULL, 0,
         "Right-align labels in cells"),
    MENU("Center", NULL, NULL, 0,
         "Center labels in cells"),
    MEND()
};

static struct menu_item const wgz_menu[] = {
    MENU("Blanks", NULL, NULL, 0,
         "Display blanks for zero values"),
    MENU("Zero", NULL, NULL, 0,
         "Display zero values"),
    MENU("Label", NULL, NULL, 0,
         "Replace zeros with a specified label"),
    //"Enter label (can include label prefix): "
    MEND()
};

static struct menu_item const wg_menu[] = {
    MENU("Format", run_menu, wgf_menu, 0,
         "Fixed  Sci  Currency  ,  General  +/-  Percent  Date  Text  Hidden"),
    MENU("Label-Prefix", run_menu, wgl_menu, 0,
         "Set global label alignment"),
    MENU("Column-Width", NULL, NULL, 0,
         "Set global column width"),
    //"Enter global column width (1..240): "
    MENU("Recalculation", run_menu, wgr_menu, 0,
         "Natural  Columnwise  Rowwise  Automatic  Manual  Iteration"),
    MENU("Protection", run_menu, wgp_menu, 0,
         "Turn worksheet protection on or off"),
    MENU("Default", run_menu, wgd_menu, 0,
         "Printer  Directory  Status  Update  Other  Autoexec  Quit"),
    MENU("Zero", run_menu, wgz_menu, 0,
         "Change the way cells with a value of zero appear on the screen"),
    MEND()
};

static struct menu_item const wc_menu[] = {
    MENU("Set-Width", NULL, NULL, 0,
         "Specify width for current column"),
    //"Enter column width (1..240): "
    MENU("Reset-Width", NULL, NULL, 0,
         "Return current column to global column width"),
    MENU("Hide", NULL, NULL, 0,
         "Hide a range of columns"),
    //"Specify column to hide: "
    MENU("Display", NULL, NULL, 0,
         "Display a range of hidden columns"),
    //"Specify hidden columns to redisplay: "
    MENU("Column-Range", NULL, NULL, 0,
         "Change the width of a range of columns"),
    //"Specify the width of a range of columns"
    //"Enter range for column width change: "
    //"Return a range of columns to global column width"
    //"Enter range of columns to reset: "
    MEND()
};

static struct menu_item const we_menu[] = {
    MENU("No", NULL, NULL, 0,
         "Do not erase the worksheet; return to READY mode"),
    MENU("Yes", NULL, NULL, 0,
         "Erase the worksheet; return to READY mode"),
    MEND()
};

static struct menu_item const wi_menu[] = {
    MENU("Column", NULL, NULL, 0,
         "Insert one or more blank columns to the left of the cell pointer"),
    //"Enter column insert range: "
    MENU("Row", NULL, NULL, 0,
         "Insert one or more blank rows above the cell pointer"),
    //"Enter row insert range: "
    MEND()
};

static struct menu_item const wd_menu[] = {
    MENU("Column", NULL, NULL, 0,
         "Delete one or more columns"),
    //"Enter range of columns to delete: "
    MENU("Row", NULL, NULL, 0,
         "Delete one or more rows"),
    //"Enter range of rows to delete: "
    MEND()
};

static struct menu_item const wt_menu[] = {
    MENU("Both", NULL, NULL, 0,
         "Freeze all rows and columns above and to the left of the cell pointer"),
    MENU("Horizontal", NULL, NULL, 0,
         "Freeze all rows above the cell pointer"),
    MENU("Vertical", NULL, NULL, 0,
         "Freeze all columns to the left of the cell pointer"),
    MENU("Clear", NULL, NULL, 0,
         "Unfreeze all title columns and rows"),
    MEND()
};

static struct menu_item const ww_menu[] = {
    MENU("Horizontal", NULL, NULL, 0,
         "Split the screen horizontally at the current row"),
    MENU("Vertical", NULL, NULL, 0,
         "Split the screen vertically at the current column"),
    MENU("Sync", NULL, NULL, 0,
         "Synchronize scrolling in windows"),
    MENU("Unsync", NULL, NULL, 0,
         "Scroll windows independently"),
    MENU("Clear", NULL, NULL, 0,
         "Return to full-screen display"),
    MEND()
};

static struct menu_item const wl_menu[] = {
    MENU("Range", NULL, NULL, 0,
         "Specify a learn range in which to store keystrokes"),
    MENU("Cancel", NULL, NULL, 0,
         "Cancel the currently specified learn range"),
    MENU("Erase", NULL, NULL, 0,
         "Erase the contents of the learn range"),
    //"Erase entire contents of the learn range?"
    //"Enter learn range: "
    MEND()
};

static struct menu_item const worksheet_menu[] = {
    MENU("Global", run_menu, wg_menu, 0,
         "Format  Label-Prefix  Column-Width  Recalculation  Protection  Default  Zero"),
    MENU("Insert", run_menu, wi_menu, 0,
         "Insert blank column(s) or row(s)"),
    MENU("Delete", run_menu, wd_menu, 0,
         "Delete entire column(s) or row(s)"),
    MENU("Column", run_menu, wc_menu, 0,
         "Set-Width  Reset-Width  Hide  Display  Column-Range"),
    MENU("Erase", run_menu, we_menu, 0,
         "Erase the entire worksheet from memory"),
    MENU("Titles", run_menu, wt_menu, 0,
         "Set horizontal or vertical titles"),
    MENU("Window", run_menu, ww_menu, 0,
         "Set split screen and synchronized scrolling"),
    MENU("Status", NULL, NULL, 0,
         "Display worksheet settings"),
    MENU("Page", NULL, NULL, 0,
         "Insert a row containing a page-break symbol above the cell pointer"),
    MENU("Learn", run_menu, wl_menu, 0,
         "Record keystrokes in the worksheet"),
    MEND()
};

static struct menu_item const rf_menu[] = {
    MENU("Fixed", NULL, NULL, 0,
         "Fixed number of decimal places (x.xx)"),
    //"Enter number of decimal places (0..15): "
    //"Illegal number of decimal places"
    MENU("Sci", NULL, NULL, 0,
         "Exponential format (x.xxE+xx)"),
    MENU("Currency", NULL, NULL, 0,
         "Currency format ($x,xxx.xx)"),
    MENU(",", NULL, NULL, 0,
         "Comma format (x,xxx.xx)"),
    MENU("General", NULL, NULL, 0,
         "Standard format (x.xx or x.xxExx)"),
    MENU("+/-", NULL, NULL, 0,
         "Horizontal bar graph format (+++ or ---)"),
    MENU("Percent", NULL, NULL, 0,
         "Percent format (x.xx%)"),
    MENU("Date", run_menu, wgfd_menu, 0,
         "Date and time formats"),
    MENU("Text", NULL, NULL, 0,
         "Display formula instead of value"),
    MENU("Hidden", NULL, NULL, 0,
         "Do not display cell contents"),
    MENU("Reset", NULL, NULL, 0,
         "Return specified cells to global cell format"),
    MEND()
};

static struct menu_item const rnl_menu[] = {
    MENU("Right", NULL, NULL, 0,
         "Each label in range names cell to its right"),
    MENU("Down", NULL, NULL, 0,
         "Each label in range names cell below it"),
    MENU("Left", NULL, NULL, 0,
         "Each label in range names cell to its left"),
    MENU("Up", NULL, NULL, 0,
         "Each label in range names cell above it"),
    MEND()
};

static struct menu_item const rn_menu[] = {
    MENU("Create", NULL, NULL, 0,
         "Create or modify a range name"),
    //"Enter name: "
    //"Enter range: "
    MENU("Delete", NULL, NULL, 0,
         "Delete a range name"),
    //"Enter name to delete: "
    MENU("Labels", run_menu, rnl_menu, 0,
         "Create range names from a range of labels"),
    //"Enter label range: "
    MENU("Reset", NULL, NULL, 0,
         "Delete all range names"),
    MENU("Table", NULL, NULL, 0,
         "Create a table of range names"),
    //"Enter range for table: "
    MEND()
};

static struct menu_item const rl_menu[] = {
    MENU("Left", NULL, NULL, 0,
         "Left-align labels in cells"),
    MENU("Right", NULL, NULL, 0,
         "Right-align labels in cells"),
    MENU("Center", NULL, NULL, 0,
         "Center labels in cells"),
    MEND()
};

static struct menu_item const rsr_menu[] = {
    MENU("Replace", NULL, NULL, 0,
         "Replace string and proceed to next matching string in range"),
    MENU("All", NULL, NULL, 0,
         "Replace all matching strings in range with replacement string"),
    MENU("Next", NULL, NULL, 0,
         "Find next matching string without replacing current string"),
    MENU("Quit", NULL, NULL, 0,
         "Do not replace current string; return to READY mode"),
    MEND()
};

static struct menu_item const rs_menu[] = {
    MENU("Formulas", NULL, NULL, 0,
         "Search for string in formulas only"),
    MENU("Labels", NULL, NULL, 0,
         "Search for string in labels only"),
    MENU("Both", NULL, NULL, 0,
         "Search for string in formulas and labels"),
    MENU("Find", NULL, NULL, 0,
         "Highlight search string in the search range"),
    MENU("Replace", run_menu, rsr_menu, 0,
         "Replace each occurrence of search string with specified text"),
    MENU("Next", NULL, NULL, 0,
         "Find next matching string"),
    MENU("Quit", NULL, NULL, 0,
         "Return to READY mode"),
    MEND()
};

static struct menu_item const range_menu[] = {
    MENU("Format", run_menu, rf_menu, 0,
         "Fixed  Sci  Currency  ,  General  +/-  Percent  Date  Text  Hidden  Reset"),
    MENU("Label", run_menu, rl_menu, 0,
         "Select alignment for a label or range of labels"),
    MENU("Erase", NULL, NULL, 0,
         "Erase a cell or range of cells"),
    MENU("Name", run_menu, rn_menu, 0,
         "Create  Delete  Labels  Reset  Table"),
    MENU("Justify", NULL, NULL, 0,
         "Adjust a column of labels to a specified width"),
    MENU("Prot", NULL, NULL, 0,
         "Prevent changes to a range if global protection is on"),
    MENU("Unprot", NULL, NULL, 0,
         "Allow change to a range when global protection is on"),
    MENU("Input", NULL, NULL, 0,
         "Restrict data entry to unprotected cells"),
    MENU("Value", NULL, NULL, 0,
         "Copy a range, converting formulas to values"),
    MENU("Trans", NULL, NULL, 0,
         "Copy a range, switching columns and rows"),
    MENU("Search", run_menu, rs_menu, 0,
         "Find or replace a specified string in a range"),
#if 0
"Enter range to format: "
"Enter range of labels: "
"Enter range to erase: "
"Enter justify range: "
"Enter range to protect: "
"Enter range to unprotect: "
"Enter data input range: "
"Enter range to search: "
"Enter string to search for: "
"Enter replacement string: "
#endif
    MEND()
};

static struct menu_item const far_menu[] = {
    MENU("Get", NULL, NULL, 0,
         "Secure a reservation for saving the file"),
    MENU("Release", NULL, NULL, 0,
         "Release a reservation for saving the file"),
    MEND()
};

static struct menu_item const fa_menu[] = {
    MENU("Reservation", run_menu, far_menu, 0,
         "Get or release the current file's reservation"),
    MENU("Table", NULL, NULL, 0,
         "Enter a table of file information in the worksheet"),
    MENU("Link-Refresh", NULL, NULL, 0,
         "Update linked cells"),
    MEND()
};

static struct menu_item const fe_menu[] = {
    MENU("Worksheet", NULL, NULL, 0,
         "Erase a worksheet file"),
    MENU("Print", NULL, NULL, 0,
         "Erase a print file"),
    MENU("Graph", NULL, NULL, 0,
         "Erase a graph file"),
    MENU("Other", NULL, NULL, 0,
         "Erase any file"),
    //"Cancel command -- Do not erase the file"
    //"Erase the file"
    MEND()
};

static struct menu_item const fc_menu[] = {
    MENU("Copy", NULL, NULL, 0,
         "Copy data from a file on disk to the worksheet"),
    MENU("Add", NULL, NULL, 0,
         "Add values from a file on disk to values in the worksheet"),
    MENU("Subtract", NULL, NULL, 0,
         "Subtract values from a file on disk from values in the worksheet"),
    MENU("Entire-File", NULL, NULL, 0,
         "Incorporate entire file into worksheet"),
    MENU("Named/Specified-Range", NULL, NULL, 0,
         "Incorporate a range from a file into the worksheet"),
    //"Enter range name or address: "
    MEND()
};

static struct menu_item const fx_menu[] = {
    MENU("Formulas", NULL, NULL, 0,
         "Save data including formulas"),
    MENU("Values", NULL, NULL, 0,
         "Save current values and labels"),
    MEND()
};

static struct menu_item const fl_menu[] = {
    MENU("Worksheet", NULL, NULL, 0,
         "List worksheet files"),
    MENU("Print", NULL, NULL, 0,
         "List print files"),
    MENU("Graph", NULL, NULL, 0,
         "List graph files"),
    MENU("Other", NULL, NULL, 0,
         "List all files"),
    MENU("Linked", NULL, NULL, 0,
         "List all files linked to worksheet"),
    MEND()
};

static struct menu_item const fi_menu[] = {
    MENU("Text", NULL, NULL, 0,
         "Import each line of data as a single label"),
    MENU("Numbers", NULL, NULL, 0,
         "Import numbers and quoted text into separate columns"),
    MEND()
};

static struct menu_item const file_menu[] = {
    MENU("Retrieve", NULL, NULL, 0,
         "Erase the current worksheet from memory and display the selected worksheet"),
    //"Name of file to retrieve: "
    MENU("Save", NULL, NULL, 0,
         "Store the entire worksheet in a worksheet file"),
    //"Enter name of file to save: "
    //"Verify password: "
    MENU("Combine", run_menu, fc_menu, 0,
         "Incorporate all or part of a worksheet file into the current worksheet"),
    //"Enter name of file to combine: "
    MENU("Xtract", run_menu, fx_menu, 0,
         "Save a specified range in a worksheet file"),
    //"Enter name of file to extract to: "
    //"Enter extract range: "
    MENU("Erase", run_menu, fe_menu, 0,
         "Erase a file from disk"),
    //"Enter name of file to erase: "
    MENU("List", run_menu, fl_menu, 0,
         "Display the names of files in the current directory"),
    //"Enter extension of files to list: "
    //"Highlight linked file to list: "
    MENU("Import", run_menu, fi_menu, 0,
         "Read text or numbers from a text file into the worksheet"),
    //"Enter name of file to import: "
    MENU("Directory", NULL, NULL, 0,
         "Display and/or change the current directory"),
    //"Enter current directory: "
    MENU("Admin", run_menu, fa_menu, 0,
         "Reservation  Table  Link-Refresh"),
    MEND()
};

static struct menu_item const pom_menu[] = {
    MENU("Left", NULL, NULL, 0,
         "Set left margin"),
    //"Enter left margin (0..240): "
    //"Illegal left margin"
    MENU("Right", NULL, NULL, 0,
         "Set right margin"),
    //"Enter right margin (0..240): "
    //"Illegal right margin"
    MENU("Top", NULL, NULL, 0,
         "Set top margin"),
    //"Enter top margin (0..32): "
    //"Illegal top margin"
    MENU("Bottom", NULL, NULL, 0,
         "Set bottom margin"),
    //"Enter bottom margin (0..32): "
    //"Illegal bottom margin"
    MENU("None", NULL, NULL, 0,
         "Clear all margin settings"),
    MENU("Columns", NULL, NULL, 0,
         "Print border columns to the left of each print range"),
    //"Enter range for border columns: "
    MENU("Rows", NULL, NULL, 0,
         "Print border rows above each print range"),
    //"Enter range for border rows: "
    MEND()
};

static struct menu_item const poo_menu[] = {
    MENU("As-Displayed", NULL, NULL, 0,
         "Print range as displayed"),
    MENU("Cell-Formulas", NULL, NULL, 0,
         "List entries, one per line"),
    MENU("Formatted", NULL, NULL, 0,
         "Print headers, footers, and page breaks"),
    MENU("Unformatted", NULL, NULL, 0,
         "Do not print headers, footers, and page breaks"),
    MEND()
};

static struct menu_item const po_menu[] = {
    MENU("Header", NULL, NULL, 0,
         "Create a header"),
    //"Enter header: "
    MENU("Footer", NULL, NULL, 0,
         "Create a footer"),
    //"Enter footer: "
    MENU("Margins", run_menu, pom_menu, 0,
         "Left  Right  Top  Bottom  None"),
    MENU("Borders", NULL, NULL, 0,
         "Print border columns and/or rows"),
    MENU("Setup", NULL, NULL, 0,
         "Enter printer setup string"),
    //"Enter setup string: "
    MENU("Pg-Length", NULL, NULL, 0,
         "Specify number of lines per page"),
    //"Enter lines per page (1..100): "
    //"Illegal page length"
    MENU("Other", run_menu, poo_menu, 0,
         "As-Displayed  Cell-Formulas  Formatted  Unformatted"),
    MENU("Quit", NULL, NULL, 0,
         "Return to previous menu"),
    MEND()
};

static struct menu_item const pc_menu[] = {
    MENU("All", NULL, NULL, 0,
         "Return all print settings to defaults"),
    MENU("Range", NULL, NULL, 0,
         "Clear current print range"),
    MENU("Borders", NULL, NULL, 0,
         "Clear border column and row ranges"),
    MENU("Format", NULL, NULL, 0,
         "Return margins, page length, and setup string to defaults"),
    MEND()
};

static struct menu_item const print_menu[] = {
    MENU("Printer", NULL, NULL, 0,
         "Send print output directly to a printer"),
    MENU("File", NULL, NULL, 0,
         "Send print output to a text file"),
    //"Enter name of text file: "
    MENU("Range", NULL, NULL, 0,
         "Specify a range to print"),
    //"Enter print range: "
    MENU("Line", NULL, NULL, 0,
         "Advance paper one line"),
    MENU("Page", NULL, NULL, 0,
         "Advance paper to top of next page"),
    MENU("Options", run_menu, po_menu, 0,
         "Header  Footer  Margins  Borders  Setup  Pg-Length  Other  Quit"),
    MENU("Clear", run_menu, pc_menu, 0,
         "All  Range  Borders  Format"),
    MENU("Align", NULL, NULL, 0,
         "Reset to top of page (after adjusting paper)"),
    MENU("Go", NULL, NULL, 0,
         "Print the specified range"),
    MENU("Quit", NULL, NULL, 0,
         "Return to READY mode"),
    MEND()
};

static struct menu_item const gt_menu[] = {
    MENU("Line", NULL, NULL, 0,
         "Line graph"),
    MENU("Bar", NULL, NULL, 0,
         "Bar graph"),
    MENU("XY", NULL, NULL, 0,
         "XY graph"),
    MENU("Stack-Bar", NULL, NULL, 0,
         "Stacked bar graph"),
    MENU("Pie", NULL, NULL, 0,
         "Pie chart"),
    MEND()
};

static struct menu_item const gol_menu[] = {
    MENU("A", NULL, NULL, 0,
         "Assign legend for first data range"),
    //"Enter legend for first data range: "
    MENU("B", NULL, NULL, 0,
         "Assign legend for second data range"),
    //"Enter legend for second data range: "
    MENU("C", NULL, NULL, 0,
         "Assign legend for third data range"),
    //"Enter legend for third data range: "
    MENU("D", NULL, NULL, 0,
         "Assign legend for fourth data range"),
    //"Enter legend for fourth data range: "
    MENU("E", NULL, NULL, 0,
         "Assign legend for fifth data range"),
    //"Enter legend for fifth data range: "
    MENU("F", NULL, NULL, 0,
         "Assign legend for sixth data range"),
    //"Enter legend for sixth data range: "
    MENU("Range", NULL, NULL, 0,
         "Specify the range that contains legends for all data ranges"),
    //"Enter legend range: "
    MEND()
};

static struct menu_item const gr_menu[] = {
    MENU("Graph", NULL, NULL, 0,
         "Clear all current graph settings"),
    MENU("X", NULL, NULL, 0,
         "Clear X data range"),
    MENU("A", NULL, NULL, 0,
         "Clear first data range"),
    MENU("B", NULL, NULL, 0,
         "Clear second data range"),
    MENU("C", NULL, NULL, 0,
         "Clear third data range"),
    MENU("D", NULL, NULL, 0,
         "Clear fourth data range"),
    MENU("E", NULL, NULL, 0,
         "Clear fifth data range"),
    MENU("F", NULL, NULL, 0,
         "Clear sixth data range"),
    MENU("Ranges", NULL, NULL, 0,
         "Clear A-F, X, and group ranges"),
    MENU("Options", NULL, NULL, 0,
         "Cancel all /Graph Options settings"),
    MENU("Quit", NULL, NULL, 0,
         "Return to previous menu"),
    MEND()
};

static struct menu_item const gof_menu[] = {
    MENU("Lines", NULL, NULL, 0,
         "Connect data points with lines"),
    MENU("Symbols", NULL, NULL, 0,
         "Display a symbol at each data point"),
    MENU("Both", NULL, NULL, 0,
         "Display a symbol at each data point and connect data points with lines"),
    MENU("Neither", NULL, NULL, 0,
         "Display neither symbols nor lines"),
    MEND()
};

static struct menu_item const got_menu[] = {
    MENU("First", NULL, NULL, 0,
         "Assign first line of graph title"),
    //"Enter first line of graph title: "
    MENU("Second", NULL, NULL, 0,
         "Assign second line of graph title"),
    //"Enter second line of graph title: "
    MENU("X-Axis", NULL, NULL, 0,
         "Assign x-axis title"),
    //"Enter x-axis title: "
    MENU("Y-Axis", NULL, NULL, 0,
         "Assign y-axis title"),
    //"Enter y-axis title: "
    MEND()
};

static struct menu_item const gog_menu[] = {
    MENU("Horizontal", NULL, NULL, 0,
         "Draw grid lines across the graph"),
    MENU("Vertical", NULL, NULL, 0,
         "Draw grid lines up the graph"),
    MENU("Both", NULL, NULL, 0,
         "Draw grid lines both across and up the graph"),
    MENU("Clear", NULL, NULL, 0,
         "Clear all grid lines"),
    MEND()
};

static struct menu_item const gosi_menu[] = {
    MENU("Yes", NULL, NULL, 0,
         "Display scale indicator"),
    MENU("No", NULL, NULL, 0,
         "Hide scale indicator"),
    MEND()
};

static struct menu_item const gos_menu[] = {
    MENU("Y-Scale", NULL, NULL, 0,
         "Set y-axis scaling"),
    MENU("X-Scale", NULL, NULL, 0,
         "Set x-axis scaling"),
    MENU("Skip", NULL, NULL, 0,
         "Display every nth cell in X range"),
    //"Enter skip factor (1..8192): "
    //"Skip factor not within limits"
    MENU("Automatic", NULL, NULL, 0,
         "Scale automatically based on data ranges"),
    MENU("Manual", NULL, NULL, 0,
         "Scale according to specified lower and upper limits"),
    MENU("Lower", NULL, NULL, 0,
         "Specify lower scale limit"),
    //"Enter lower limit: "
    MENU("Upper", NULL, NULL, 0,
         "Specify upper scale limit"),
    //"Enter upper limit: "
    //"Select cell format for scale numbers"
    MENU("Indicator", run_menu, gosi_menu, 0,
         "Display or hide scale indicator"),
    MEND()
};

static struct menu_item const god_menu[] = {
    MENU("A", NULL, NULL, 0,
         "Assign first data-range data labels"),
    //"Enter data-label range for first data range: "
    MENU("B", NULL, NULL, 0,
         "Assign second data-range data labels"),
    //"Enter data-label range for second data range: "
    MENU("C", NULL, NULL, 0,
         "Assign third data-range data labels"),
    //"Enter data-label range for third data range: "
    MENU("D", NULL, NULL, 0,
         "Assign fourth data-range data labels"),
    //"Enter data-label range for fourth data range: "
    MENU("E", NULL, NULL, 0,
         "Assign fifth data-range data labels"),
    //"Enter data-label range for fifth data range: "
    MENU("F", NULL, NULL, 0,
         "Assign sixth data-range data labels"),
    //"Enter data-label range for sixth data range: "
    MENU("X", NULL, NULL, 0,
         "Assign all data labels"),
    //"Enter data-label range for all data ranges: "
    MEND()
};

static struct menu_item const go_menu[] = {
    MENU("Legend", run_menu, gol_menu, 0,
         "Create legends for data ranges"),
    MENU("Format", run_menu, gof_menu, 0,
         "Draw lines or symbols in line or XY graphs"),
    MENU("Titles", run_menu, got_menu, 0,
         "Add graph titles or axis titles to graph"),
    MENU("Grid", run_menu, gog_menu, 0,
         "Set horizontal and/or vertical grid lines"),
    MENU("Scale", run_menu, gos_menu, 0,
         "Select scaling options"),
    MENU("Color", NULL, NULL, 0,
         "Display graph in color"),
    MENU("B&W", NULL, NULL, 0,
         "Display graph in black and white"),
    MENU("Data-Labels", run_menu, god_menu, 0,
         "Label data points in a data range"),
    MEND()
};

static struct menu_item const gn_menu[] = {
    MENU("Use", NULL, NULL, 0,
         "Make a named graph current"),
    //"Enter name of graph to make current: "
    MENU("Create", NULL, NULL, 0,
         "Name the current graph"),
    //"Enter graph name: "
    MENU("Delete", NULL, NULL, 0,
         "Delete a named graph"),
    //"Enter name of graph to delete: "
    MENU("Delete all named graphs", NULL, NULL, 0,
         "Table"),
    MENU("Table", NULL, NULL, 0,
         "Create a table of named graphs"),
    //"Enter range for table: "
    MEND()
};

static struct menu_item const gg_menu[] = {
    MENU("Columnwise", NULL, NULL, 0,
         "Use columns as data ranges"),
    MENU("Rowwise", NULL, NULL, 0,
         "Use rows as data ranges"),
    MEND()
};

static struct menu_item const graph_menu[] = {
    MENU("Type", run_menu, gt_menu, 0,
         "Line  Bar  XY  Stack-Bar  Pie"),
    MENU("X", NULL, NULL, 0,
         "Set X data range"),
    //"Enter x-axis range: "
    MENU("A", NULL, NULL, 0,
         "Set first data range"),
    //"Enter first data range: "
    MENU("B", NULL, NULL, 0,
         "Set second data range"),
    //"Enter second data range: "
    MENU("C", NULL, NULL, 0,
         "Set third data range"),
    //"Enter third data range: "
    MENU("D", NULL, NULL, 0,
         "Set fourth data range"),
    //"Enter fourth data range: "
    MENU("E", NULL, NULL, 0,
         "Set fifth data range"),
    //"Enter fifth data range: "
    MENU("F", NULL, NULL, 0,
         "Set sixth data range"),
    //"Enter sixth data range: "
    MENU("Reset", run_menu, gr_menu, 0,
         "Graph  X  A  B  C  D  E  F  Ranges  Options  Quit"),
    MENU("View", NULL, NULL, 0,
         "View the current graph"),
    MENU("Save", NULL, NULL, 0,
         "Save the current graph in a file for printing"),
    //"Enter graph file name: "
    MENU("Options", run_menu, go_menu, 0,
         "Legend  Format  Titles  Grid  Scale  Color  B&W  Data-Labels  Quit"),
    MENU("Name", run_menu, gn_menu, 0,
         "Use  Create  Delete  Reset  Table"),
    MENU("Group", run_menu, gg_menu, 0,
         "Set all data ranges at once"),
    //"Enter group range: "
    MENU("Quit", NULL, NULL, 0,
         "Return to READY mode"),
    MEND()
};

static struct menu_item const dt_menu[] = {
    MENU("1", NULL, NULL, 0,
         "One input cell, one or more dependent formulas"),
    //"Enter table range: "
    //"Enter input cell 1: "
    MENU("2", NULL, NULL, 0,
         "Two input cells, one dependent formula"),
    //"Enter input cell 2: "
    MENU("Reset", NULL, NULL, 0,
         "Clear table ranges and input cells for all data tables"),
    MEND()
};

static struct menu_item const ds_menu[] = {
    MENU("Data-Range", NULL, NULL, 0,
         "Select records to be sorted"),
    //"Enter data range: "
    MENU("Primary-Key", NULL, NULL, 0,
         "Specify primary order for records"),
    //"Primary sort key: "
    //"Sort order (A or D): "
    //"A"
    //"D"
    MENU("Secondary-Key", NULL, NULL, 0,
         "Specify order for records with same primary key"),
    //"Secondary sort key: "
    MENU("Clear", NULL, NULL, 0,
         "Clear data range and sort keys"),
    MENU("Go", NULL, NULL, 0,
         "Sort data and return to READY mode"),
    MENU("Quit", NULL, NULL, 0,
         "Return to READY mode"),
    MEND()
};

static struct menu_item const dq_menu[] = {
    MENU("Input", NULL, NULL, 0,
         "Specify range that contains records to search"),
    //"Enter input range: "
    MENU("Criteria", NULL, NULL, 0,
         "Specify the range that contains criteria"),
    //"Enter criteria range: "
    MENU("Output", NULL, NULL, 0,
         "Specify the range to which extracted records are copied"),
    MENU("Find", NULL, NULL, 0,
         "Highlight each record that matches criteria"),
    MENU("Extract", NULL, NULL, 0,
         "Copy all records that match criteria to output range"),
    MENU("Unique", NULL, NULL, 0,
         "Copy records that match criteria to output range, eliminating duplicates"),
    MENU("Delete", NULL, NULL, 0,
         "Delete all records that match criteria"),
    //"Clear input, criteria, and output ranges"
    //"Cancel"
    //"Do not delete any records; return to previous menu"
    //"Delete records and close up spaces in input range"
    MEND()
};

static struct menu_item const dm_menu[] = {
    MENU("Invert", NULL, NULL, 0,
         "Create the inverse of a matrix"),
    //"Enter range to invert: "
    MENU("Multiply", NULL, NULL, 0,
         "Multiply two ranges as matrices"),
    //"Enter first range to multiply: "
    //"Enter second range to multiply: "
    MEND()
};

static struct menu_item const dri_menu[] = {
    MENU("Compute", NULL, NULL, 0,
         "Calculate the y-axis intercept automatically"),
    MENU("Zero", NULL, NULL, 0,
         "Use zero as the y-axis intercept"),
    MEND()
};

static struct menu_item const dr_menu[] = {
    MENU("X-Range", NULL, NULL, 0,
         "Specify independent variables (X range)"),
    //"Enter independent variables, or X range: "
    MENU("Y-Range", NULL, NULL, 0,
         "Specify dependent variable (Y range)"),
    //"Enter dependent variable, or Y range: "
    MENU("Output-Range", NULL, NULL, 0,
         "Specify the output range"),
    MENU("Intercept", run_menu, dri_menu, 0,
         "Compute  Zero"),
    MENU("Clear", NULL, NULL, 0,
         "Clear the X range, Y range, output range, and reset Intercept to Compute"),
    MENU("Go", NULL, NULL, 0,
         "Calculate a data regression on specified ranges"),
    MEND()
};

static struct menu_item const dpf_menu[] = {
    MENU("Create", NULL, NULL, 0,
         "Create a format line at the current cell"),
    MENU("Edit", NULL, NULL, 0,
         "Edit a format line at the current cell"),
    MEND()
};

static struct menu_item const dp_menu[] = {
    MENU("Format-Line", run_menu, dpf_menu, 0,
         "Create or edit a format line at the current cell"),
    MENU("Input-Column", NULL, NULL, 0,
         "Specify a column of labels to parse"),
    //"Enter column of labels to parse: "
    MENU("Range", NULL, NULL, 0,
         "Specify the range in which parsed data is placed"),
    MENU("Clear", NULL, NULL, 0,
         "Clear input column and output range"),
    MENU("Go", NULL, NULL, 0,
         "Parse labels in the input column and place them in the output range"),
    MEND()
};

static struct menu_item const data_menu[] = {
    MENU("Fill", NULL, NULL, 0,
         "Fill a range with a sequence of values"),
    //"Enter fill range: "
    //"Start: "
    //"Step: "
    //"Stop: "
    MENU("Table", run_menu, dt_menu, 0,
         "Create a table of values"),
    MENU("Sort", run_menu, ds_menu, 0,
         "Sort database records"),
    MENU("Query", run_menu, dq_menu, 0,
         "Find all records that satisfy given criteria"),
    MENU("Distribution", NULL, NULL, 0,
         "Calculate frequency distribution of the values in a range"),
    //"Enter values range: "
    //"Enter bin range: "
    MENU("Matrix", run_menu, dm_menu, 0,
         "Multiply and invert matrices"),
    //"Enter output range: "
    MENU("Regression", run_menu, dr_menu, 0,
         "Calculate linear regression"),
    MENU("Parse", run_menu, dp_menu, 0,
         "Convert a column of long labels into a range of labels or numbers"),
    MEND()
};

static struct menu_item const addin_menu[] = {
    MENU("Attach", NULL, NULL, 0,
         "Load an add-in program into memory"),
    MENU("Detach", NULL, NULL, 0,
         "Remove an attached add-in program from memory"),
    MENU("Invoke", NULL, NULL, 0,
         "Activate an attached add-in program"),
    MENU("Clear", NULL, NULL, 0,
         "Remove all attached add-in programs from memory"),
    MEND()
};

#if 0
"Enter add-in to attach: "
"Enter add-in to detach: "
"Enter add-in to invoke: "
"Return to READY mode"
"No-Key"
"Do not assign add-in to a key"
"7"
"Use APP1 (ALT-F7) to invoke the add-in"
"8"
"Use APP2 (ALT-F8) to invoke the add-in"
"9"
"Use APP3 (ALT-F9) to invoke the add-in"
"10"
"Use APP4 (ALT-F10) to invoke the add-in"
#endif

static struct menu_item const quit_menu[] = {
    MENU("No", run_abort, NULL, 0,
         "Do not end 1-2-3 session; return to READY mode"),
    MENU("Yes", run_quit, NULL, 0,
         "End 1-2-3 session (Remember to save your worksheet first)"),
    MEND()
};

static int run_menu(const void *arg, int n) {
    struct menu_item const *menu = arg;
    int option = 0, i, e, c;
    for (;;) {
        move(0, 0);
        clrtoeol();
        for (e = 0; menu[e].option; e++) {
            if (e == option)
                select_style(STYLE_FRAME, 0);
            addstr(menu[e].option);
            select_style(STYLE_CELL, 0);
            addstr("  ");
        }
        screen_draw_line(1, 0, menu[option].desc);
        screen_hidecursor();
        screen_refresh();
        switch (c = nmgetch(0)) {
        case KEY_HOME:
        case ctl('a'):
            option = 0;
            continue;
        case KEY_END:
        case ctl('e'):
            option = e - 1;
            continue;
        case KEY_LEFT:
        case DEL:
        case KEY_BACKSPACE:
        case ctl('b'):
            if (option > 0)
                option--;
            continue;
        case KEY_RIGHT:
        case ctl('f'):
            if (option < e - 1)
                option++;
            continue;
        case ESC:
        case KEY_UP:
        case ctl('p'):
            screen_clear_line(0);
            screen_clear_line(1);
            return ESC;
        case ctl('g'):
        case EOF:
            screen_clear_line(0);
            screen_clear_line(1);
            return c;
        case KEY_ENTER:
        case KEY_DOWN:
        case ctl('j'):
        case ctl('m'):
        case ctl('n'):
            break;
        default:
            for (i = 0; i < e; i++) {
                if (toupperchar(menu[i].option[0]) == toupperchar(c)) {
                    option = i;
                    break;
                }
            }
            if (i < e)
                break;
            continue;
        }
        screen_clear_line(0);
        screen_clear_line(1);
        if (menu[option].func) {
            c = menu[option].func(menu[option].arg, menu[option].n);
            if (c == ESC)
                continue;
            return c;
        }
    }
}

static int run_abort(const void *arg, int n) {
    return 0;
}

static int run_copy(const void *arg, int n) {
    //get_range("Enter range to copy FROM: ");
    //get_range("Enter range to copy TO: ");
    return 0;
}

static int run_move(const void *arg, int n) {
    //get_range("Enter range to move FROM: ");
    //get_range("Enter range to move TO: ");
    return 0;
}

static int run_system(const void *arg, int n) {
    return 0;
}

static int run_quit(const void *arg, int n) {
    return 0;
}
