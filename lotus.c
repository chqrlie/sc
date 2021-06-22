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

#define MENU(o,d,f,a,n) { o, d, f, a, n }
#define MEND() { NULL, NULL, NULL, NULL, 0 }

static struct menu_item const top_menu[] = {
    MENU("Worksheet",
         "Global  Insert  Delete  Column  Erase  Titles  Window  Status  Page  Learn",
         run_menu, worksheet_menu, 0),
    MENU("Range",
         "Format  Label  Erase  Name  Justify  Prot  Unprot  Input  Value  Trans  Search",
         run_menu, range_menu, 0),
    MENU("Copy",
         "Copy a cell or range of cells",
         run_copy, NULL, 0),
    MENU("Move",
         "Move a cell or range of cells",
         run_move, NULL, 0),
    MENU("File",
         "Retrieve  Save  Combine  Xtract  Erase  List  Import  Directory  Admin",
         run_menu, file_menu, 0),
    MENU("Print",
         "Print a range on a printer or to a print file",
         run_menu, print_menu, 0),
    MENU("Graph",
         "Type  X  A  B  C  D  E  F  Reset  View  Save  Options  Name  Group  Quit",
         run_menu, graph_menu, 0),
    MENU("Data",
         "Fill  Table  Sort  Query  Distribution  Matrix  Regression  Parse",
         run_menu, data_menu, 0),
    MENU("System",
         "Leave 1-2-3 temporarily and use operating system",
         run_system, NULL, 0),
    MENU("Add-In",
         "Attach, Detach, Invoke or Clear 1-2-3 add-in programs",
         run_menu, addin_menu, 0),
    MENU("Quit",
         "End the 1-2-3 session",
         run_menu, quit_menu, 0),
    MEND()
};

static struct menu_item const wgfdt_menu[] = {
    MENU("1 (HH:MM:SS AM/PM)",
         "",
         NULL, NULL, 0),
    MENU("2 (HH:MM AM/PM)",
         "",
         NULL, NULL, 0),
    MENU("3 (Long Intn'l)",
         "",
         NULL, NULL, 0),
    //"Currently configured: HH:MM:SS (24 hour)"
    //"Currently configured: HH.MM.SS (24 hour)"
    //"Currently configured: HH,MM,SS (24 hour)"
    //"Currently configured: HHhMMmSSs (24 hour)"
    MENU("4 (Short Intn'l)",
         "",
         NULL, NULL, 0),
    //"Currently configured: HH:MM (24 hour)"
    //"Currently configured: HH.MM (24 hour)"
    //"Currently configured: HH,MM (24 hour)"
    //"Currently configured: HHhMMm (24 hour)"
    MEND()
};

static struct menu_item const wgfd_menu[] = {
    MENU("1 (DD-MMM-YY)",
         "Lotus standard long form",
         NULL, NULL, 0),
    MENU("2 (DD-MMM)",
         "Lotus standard short form",
         NULL, NULL, 0),
    MENU("3 (MMM-YY)",
         "",
         NULL, NULL, 0),
    MENU("4 (Long Intn'l)",
         "",
         NULL, NULL, 0),
    //"Currently configured: MM/DD/YY"
    //"Currently configured: DD/MM/YY"
    //"Currently configured: DD.MM.YY"
    //"Currently configured: YY-MM-DD"
    MENU("5 (Short Intn'l)",
         "",
         NULL, NULL, 0),
    //"Currently configured: MM/DD"
    //"Currently configured: DD/MM"
    //"Currently configured: DD.MM"
    //"Currently configured: MM-DD"
    MENU("Time",
         "Time formats",
         run_menu, wgfdt_menu, 0),
    MEND()
};

static struct menu_item const wgf_menu[] = {
    MENU("Fixed",
         "Fixed number of decimal places (x.xx)",
         NULL, NULL, 0),
    //"Enter number of decimal places (0..15): "
    //"Illegal number of decimal places"
    MENU("Sci",
         "Exponential format (x.xxE+xx)",
         NULL, NULL, 0),
    MENU("Currency",
         "Currency format ($x,xxx.xx)",
         NULL, NULL, 0),
    MENU(",",
         "Comma format (x,xxx.xx)",
         NULL, NULL, 0),
    MENU("General",
         "Standard format (x.xx or x.xxExx)",
         NULL, NULL, 0),
    MENU("+/-",
         "Horizontal bar graph format (+++ or ---)",
         NULL, NULL, 0),
    MENU("Percent",
         "Percent format (x.xx%)",
         NULL, NULL, 0),
    MENU("Date",
         "Date and time formats",
         run_menu, wgfd_menu, 0),
    MENU("Text",
         "Display formula instead of value",
         NULL, NULL, 0),
    MENU("Hidden",
         "Do not display cell contents",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgr_menu[] = {
    MENU("Natural",
         "Recalculate in natural order",
         NULL, NULL, 0),
    MENU("Columnwise",
         "Recalculate column by column",
         NULL, NULL, 0),
    MENU("Rowwise",
         "Recalculate row by row",
         NULL, NULL, 0),
    MENU("Automatic",
         "Recalculate every time entries change",
         NULL, NULL, 0),
    MENU("Manual",
         "Press CALC (F9) to recalculate formulas",
         NULL, NULL, 0),
    MENU("Iteration",
         "Specify number of recalculation passes",
         NULL, NULL, 0),
    //"Enter iteration count (1..50): "
    //"Illegal iteration count"
    MEND()
};

static struct menu_item const wgdpi_menu[] = {
    MENU("1",
         "110 baud",
         NULL, NULL, 0),
    MENU("2",
         "150 baud",
         NULL, NULL, 0),
    MENU("3",
         "300 baud",
         NULL, NULL, 0),
    MENU("4",
         "600 baud",
         NULL, NULL, 0),
    MENU("5",
         "1200 baud",
         NULL, NULL, 0),
    MENU("6",
         "2400 baud",
         NULL, NULL, 0),
    MENU("7",
         "4800 baud",
         NULL, NULL, 0),
    MENU("8",
         "9600 baud",
         NULL, NULL, 0),
    MENU("9",
         "19200 baud",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdpa_menu[] = {
    MENU("Yes",
         "Printer automatically issues a line feed after a carriage return",
         NULL, NULL, 0),
    MENU("No",
         "Printer does not automatically issue a line feed after a carriage return",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdpw_menu[] = {
    MENU("Yes",
         "Do not wait at the end of a page for paper change",
         NULL, NULL, 0),
    MENU("No",
         "Wait at the end of a page for paper change",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdp_menu[] = {
    MENU("Interface",
         "Specify printer interface",
         run_menu, wgdpi_menu, 0),
    MENU("AutoLF",
         "Automatic linefeed at end of line",
         run_menu, wgdpa_menu, 0),
    MENU("Left",
         "Set default left margin",
         NULL, NULL, 0),
    //"Enter left margin (0..240): "
    //"Illegal left margin"
    MENU("Right",
         "Set default right margin",
         NULL, NULL, 0),
    //"Enter right margin (0..240): "
    //"Illegal right margin"
    MENU("Top",
         "Set default top margin",
         NULL, NULL, 0),
    //"Enter top margin (0..32): "
    //"Illegal top margin"
    MENU("Bot",
         "Set default bottom margin",
         NULL, NULL, 0),
    //"Enter bottom margin (0..32): "
    //"Illegal bottom margin"
    MENU("Pg-Length",
         "Set default number of lines per page",
         NULL, NULL, 0),
    //"Enter lines per page (1..100): "
    //"Illegal page length"
    MENU("Wait",
         "Wait for paper change at end of each page",
         run_menu, wgdpw_menu, 0),
    MENU("Setup",
         "Specify default setup string",
         NULL, NULL, 0),
    //"Enter setup string: "
    MENU("Name",
         "Change current printer device",
         NULL, NULL, 0),
    MENU("Quit",
         "Return to previous menu",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoip_menu[] = {
    MENU("A (.,,)",
         "Decimal: Period   Argument separator: Comma       Thousands separator: Comma",
         NULL, NULL, 0),
    MENU("B (,..)",
         "Decimal: Comma    Argument separator: Period      Thousands separator: Period",
         NULL, NULL, 0),
    MENU("C (.;,)",
         "Decimal: Period   Argument separator: Semicolon   Thousands separator: Comma",
         NULL, NULL, 0),
    MENU("D (,;.)",
         "Decimal: Comma    Argument separator: Semicolon   Thousands separator: Period",
         NULL, NULL, 0),
    MENU("E (., )",
         "Decimal: Period   Argument separator: Comma       Thousands separator: Space",
         NULL, NULL, 0),
    MENU("F (,. )",
         "Decimal: Comma    Argument separator: Period      Thousands separator: Space",
         NULL, NULL, 0),
    MENU("G (.; )",
         "Decimal: Period   Argument separator: Semicolon   Thousands separator: Space",
         NULL, NULL, 0),
    MENU("H (,; )",
         "Decimal: Comma    Argument separator: Semicolon   Thousands separator: Space",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoic_menu[] = {
    MENU("Prefix",
         "Currency sign precedes value",
         NULL, NULL, 0),
    MENU("Suffix",
         "Currency sign follows value",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoid_menu[] = {
    MENU("A (MM/DD/YY)",
         "Format D4 will be MM/DD/YY     Format D5 will be MM/DD",
         NULL, NULL, 0),
    MENU("B (DD/MM/YY)",
         "Format D4 will be DD/MM/YY     Format D5 will be DD/MM",
         NULL, NULL, 0),
    MENU("C (DD.MM.YY)",
         "Format D4 will be DD.MM.YY     Format D5 will be DD.MM",
         NULL, NULL, 0),
    MENU("D (YY-MM-DD)",
         "Format D4 will be YY-MM-DD     Format D5 will be MM-DD",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoit_menu[] = {
    MENU("A (HH:MM:SS)",
         "Format D8 will be HH:MM:SS     Format D9 will be HH:MM",
         NULL, NULL, 0),
    MENU("B (HH.MM.SS)",
         "Format D8 will be HH.MM.SS     Format D9 will be HH.MM",
         NULL, NULL, 0),
    MENU("C (HH,MM,SS)",
         "Format D8 will be HH,MM,SS     Format D9 will be HH,MM",
         NULL, NULL, 0),
    MENU("D (HHhMMmSSs)",
         "Format D8 will be HHhMMmSSs    Format D9 will be HHhMMm",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoin_menu[] = {
    MENU("Parentheses",
         "Display parentheses around negative numbers",
         NULL, NULL, 0),
    MENU("Sign",
         "Display minus sign in front of negative numbers",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoi_menu[] = {
    MENU("Punctuation",
         "Specify numeric punctuation",
         run_menu, wgdoip_menu, 0),
    MENU("Currency",
         "Specify currency sign",
         run_menu, wgdoic_menu, 0),
    //"Enter currency sign: "
    MENU("Date",
         "Specify date formats D4 and D5",
         run_menu, wgdoid_menu, 0),
    MENU("Time",
         "Specify time formats D8 and D9",
         run_menu, wgdoit_menu, 0),
    MENU("Negative",
         "Specify display of negative numbers in , (Comma) and Currency formats",
         run_menu, wgdoin_menu, 0),
    MENU("Quit",
         "Return to /Worksheet Global Default menu",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoh_menu[] = {
    MENU("Instant",
         "Instant access; do not remove Help Disk",
         NULL, NULL, 0),
    MENU("Removable",
         "Help Disk can be removed when Help is not in use",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdoc_menu[] = {
    MENU("Standard",
         "Use Lotus standard date and time: DD-MMM-YY  HH:MM AM/PM",
         NULL, NULL, 0),
    MENU("International",
         "Use current International settings for date (D4) and time (D9)",
         NULL, NULL, 0),
    MENU("None",
         "Hide date-and-time indicator",
         NULL, NULL, 0),
    MENU("Clock",
         "Display the date-and-time indicator",
         NULL, NULL, 0),
    MENU("Filename",
         "Display current file name instead of date-and-time indicator",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdou_menu[] = {
    MENU("Yes",
         "Turn undo on",
         NULL, NULL, 0),
    MENU("No",
         "Turn undo off",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdob_menu[] = {
    MENU("Yes",
         "Computer bell will sound when an error occurs",
         NULL, NULL, 0),
    MENU("No",
         "Computer bell will not sound when an error occurs",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgdo_menu[] = {
    MENU("International",
         "Punctuation  Currency  Date  Time  Negative  Quit",
         run_menu, wgdoi_menu, 0),
    MENU("Help",
         "Select Help access method",
         run_menu, wgdoh_menu, 0),
    MENU("Clock",
         "Standard  International  None  Clock  Filename",
         run_menu, wgdoc_menu, 0),
    MENU("Undo",
         "Turn undo feature on or off",
         run_menu, wgdou_menu, 0),
    MENU("Beep",
         "Determine if computer bell will sound when error occurs",
         run_menu, wgdob_menu, 0),
    MENU("Add-In",
         "Specify add-ins to be loaded automatically whenever you start 1-2-3",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgda_menu[] = {
    MENU("Yes",
         "Run autoexecute macro when file is retrieved",
         NULL, NULL, 0),
    MENU("No",
         "Do not run autoexecute macro when file is retrieved",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgd_menu[] = {
    MENU("Printer",
         "Specify printer interface and default settings",
         run_menu, wgdp_menu, 0),
    MENU("Directory",
         "Specify default directory",
         NULL, NULL, 0),
    //"Enter default directory: "
    MENU("Status",
         "Display all default settings",
         NULL, NULL, 0),
    MENU("Update",
         "Save new default settings in configuration file",
         NULL, NULL, 0),
    MENU("Other",
         "International  Help  Clock  Undo  Beep  Add-In",
         run_menu, wgdo_menu, 0),
    MENU("Autoexec",
         "Run autoexecute macros when files are retrieved",
         run_menu, wgda_menu, 0),
    MENU("Quit",
         "Return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgp_menu[] = {
    MENU("Enable",
         "Turn worksheet protection on",
         NULL, NULL, 0),
    MENU("Disable",
         "Turn worksheet protection off",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgl_menu[] = {
    MENU("Left",
         "Left-align labels in cells",
         NULL, NULL, 0),
    MENU("Right",
         "Right-align labels in cells",
         NULL, NULL, 0),
    MENU("Center",
         "Center labels in cells",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wgz_menu[] = {
    MENU("Blanks",
         "Display blanks for zero values",
         NULL, NULL, 0),
    MENU("Zero",
         "Display zero values",
         NULL, NULL, 0),
    MENU("Label",
         "Replace zeros with a specified label",
         NULL, NULL, 0),
    //"Enter label (can include label prefix): "
    MEND()
};

static struct menu_item const wg_menu[] = {
    MENU("Format",
         "Fixed  Sci  Currency  ,  General  +/-  Percent  Date  Text  Hidden",
         run_menu, wgf_menu, 0),
    MENU("Label-Prefix",
         "Set global label alignment",
         run_menu, wgl_menu, 0),
    MENU("Column-Width",
         "Set global column width",
         NULL, NULL, 0),
    //"Enter global column width (1..240): "
    MENU("Recalculation",
         "Natural  Columnwise  Rowwise  Automatic  Manual  Iteration",
         run_menu, wgr_menu, 0),
    MENU("Protection",
         "Turn worksheet protection on or off",
         run_menu, wgp_menu, 0),
    MENU("Default",
         "Printer  Directory  Status  Update  Other  Autoexec  Quit",
         run_menu, wgd_menu, 0),
    MENU("Zero",
         "Change the way cells with a value of zero appear on the screen",
         run_menu, wgz_menu, 0),
    MEND()
};

static struct menu_item const wc_menu[] = {
    MENU("Set-Width",
         "Specify width for current column",
         NULL, NULL, 0),
    //"Enter column width (1..240): "
    MENU("Reset-Width",
         "Return current column to global column width",
         NULL, NULL, 0),
    MENU("Hide",
         "Hide a range of columns",
         NULL, NULL, 0),
    //"Specify column to hide: "
    MENU("Display",
         "Display a range of hidden columns",
         NULL, NULL, 0),
    //"Specify hidden columns to redisplay: "
    MENU("Column-Range",
         "Change the width of a range of columns",
         NULL, NULL, 0),
    //"Specify the width of a range of columns"
    //"Enter range for column width change: "
    //"Return a range of columns to global column width"
    //"Enter range of columns to reset: "
    MEND()
};

static struct menu_item const we_menu[] = {
    MENU("No",
         "Do not erase the worksheet; return to READY mode",
         NULL, NULL, 0),
    MENU("Yes",
         "Erase the worksheet; return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wi_menu[] = {
    MENU("Column",
         "Insert one or more blank columns to the left of the cell pointer",
         NULL, NULL, 0),
    //"Enter column insert range: "
    MENU("Row",
         "Insert one or more blank rows above the cell pointer",
         NULL, NULL, 0),
    //"Enter row insert range: "
    MEND()
};

static struct menu_item const wd_menu[] = {
    MENU("Column",
         "Delete one or more columns",
         NULL, NULL, 0),
    //"Enter range of columns to delete: "
    MENU("Row",
         "Delete one or more rows",
         NULL, NULL, 0),
    //"Enter range of rows to delete: "
    MEND()
};

static struct menu_item const wt_menu[] = {
    MENU("Both",
         "Freeze all rows and columns above and to the left of the cell pointer",
         NULL, NULL, 0),
    MENU("Horizontal",
         "Freeze all rows above the cell pointer",
         NULL, NULL, 0),
    MENU("Vertical",
         "Freeze all columns to the left of the cell pointer",
         NULL, NULL, 0),
    MENU("Clear",
         "Unfreeze all title columns and rows",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const ww_menu[] = {
    MENU("Horizontal",
         "Split the screen horizontally at the current row",
         NULL, NULL, 0),
    MENU("Vertical",
         "Split the screen vertically at the current column",
         NULL, NULL, 0),
    MENU("Sync",
         "Synchronize scrolling in windows",
         NULL, NULL, 0),
    MENU("Unsync",
         "Scroll windows independently",
         NULL, NULL, 0),
    MENU("Clear",
         "Return to full-screen display",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const wl_menu[] = {
    MENU("Range",
         "Specify a learn range in which to store keystrokes",
         NULL, NULL, 0),
    MENU("Cancel",
         "Cancel the currently specified learn range",
         NULL, NULL, 0),
    MENU("Erase",
         "Erase the contents of the learn range",
         NULL, NULL, 0),
    //"Erase entire contents of the learn range?"
    //"Enter learn range: "
    MEND()
};

static struct menu_item const worksheet_menu[] = {
    MENU("Global",
         "Format  Label-Prefix  Column-Width  Recalculation  Protection  Default  Zero",
         run_menu, wg_menu, 0),
    MENU("Insert",
         "Insert blank column(s) or row(s)",
         run_menu, wi_menu, 0),
    MENU("Delete",
         "Delete entire column(s) or row(s)",
         run_menu, wd_menu, 0),
    MENU("Column",
         "Set-Width  Reset-Width  Hide  Display  Column-Range",
         run_menu, wc_menu, 0),
    MENU("Erase",
         "Erase the entire worksheet from memory",
         run_menu, we_menu, 0),
    MENU("Titles",
         "Set horizontal or vertical titles",
         run_menu, wt_menu, 0),
    MENU("Window",
         "Set split screen and synchronized scrolling",
         run_menu, ww_menu, 0),
    MENU("Status",
         "Display worksheet settings",
         NULL, NULL, 0),
    MENU("Page",
         "Insert a row containing a page-break symbol above the cell pointer",
         NULL, NULL, 0),
    MENU("Learn",
         "Record keystrokes in the worksheet",
         run_menu, wl_menu, 0),
    MEND()
};

static struct menu_item const rf_menu[] = {
    MENU("Fixed",
         "Fixed number of decimal places (x.xx)",
         NULL, NULL, 0),
    //"Enter number of decimal places (0..15): "
    //"Illegal number of decimal places"
    MENU("Sci",
         "Exponential format (x.xxE+xx)",
         NULL, NULL, 0),
    MENU("Currency",
         "Currency format ($x,xxx.xx)",
         NULL, NULL, 0),
    MENU(",",
         "Comma format (x,xxx.xx)",
         NULL, NULL, 0),
    MENU("General",
         "Standard format (x.xx or x.xxExx)",
         NULL, NULL, 0),
    MENU("+/-",
         "Horizontal bar graph format (+++ or ---)",
         NULL, NULL, 0),
    MENU("Percent",
         "Percent format (x.xx%)",
         NULL, NULL, 0),
    MENU("Date",
         "Date and time formats",
         run_menu, wgfd_menu, 0),
    MENU("Text",
         "Display formula instead of value",
         NULL, NULL, 0),
    MENU("Hidden",
         "Do not display cell contents",
         NULL, NULL, 0),
    MENU("Reset",
         "Return specified cells to global cell format",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const rnl_menu[] = {
    MENU("Right",
         "Each label in range names cell to its right",
         NULL, NULL, 0),
    MENU("Down",
         "Each label in range names cell below it",
         NULL, NULL, 0),
    MENU("Left",
         "Each label in range names cell to its left",
         NULL, NULL, 0),
    MENU("Up",
         "Each label in range names cell above it",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const rn_menu[] = {
    MENU("Create",
         "Create or modify a range name",
         NULL, NULL, 0),
    //"Enter name: "
    //"Enter range: "
    MENU("Delete",
         "Delete a range name",
         NULL, NULL, 0),
    //"Enter name to delete: "
    MENU("Labels",
         "Create range names from a range of labels",
         run_menu, rnl_menu, 0),
    //"Enter label range: "
    MENU("Reset",
         "Delete all range names",
         NULL, NULL, 0),
    MENU("Table",
         "Create a table of range names",
         NULL, NULL, 0),
    //"Enter range for table: "
    MEND()
};

static struct menu_item const rl_menu[] = {
    MENU("Left",
         "Left-align labels in cells",
         NULL, NULL, 0),
    MENU("Right",
         "Right-align labels in cells",
         NULL, NULL, 0),
    MENU("Center",
         "Center labels in cells",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const rsr_menu[] = {
    MENU("Replace",
         "Replace string and proceed to next matching string in range",
         NULL, NULL, 0),
    MENU("All",
         "Replace all matching strings in range with replacement string",
         NULL, NULL, 0),
    MENU("Next",
         "Find next matching string without replacing current string",
         NULL, NULL, 0),
    MENU("Quit",
         "Do not replace current string; return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const rs_menu[] = {
    MENU("Formulas",
         "Search for string in formulas only",
         NULL, NULL, 0),
    MENU("Labels",
         "Search for string in labels only",
         NULL, NULL, 0),
    MENU("Both",
         "Search for string in formulas and labels",
         NULL, NULL, 0),
    MENU("Find",
         "Highlight search string in the search range",
         NULL, NULL, 0),
    MENU("Replace",
         "Replace each occurrence of search string with specified text",
         run_menu, rsr_menu, 0),
    MENU("Next",
         "Find next matching string",
         NULL, NULL, 0),
    MENU("Quit",
         "Return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const range_menu[] = {
    MENU("Format",
         "Fixed  Sci  Currency  ,  General  +/-  Percent  Date  Text  Hidden  Reset",
         run_menu, rf_menu, 0),
    MENU("Label",
         "Select alignment for a label or range of labels",
         run_menu, rl_menu, 0),
    MENU("Erase",
         "Erase a cell or range of cells",
         NULL, NULL, 0),
    MENU("Name",
         "Create  Delete  Labels  Reset  Table",
         run_menu, rn_menu, 0),
    MENU("Justify",
         "Adjust a column of labels to a specified width",
         NULL, NULL, 0),
    MENU("Prot",
         "Prevent changes to a range if global protection is on",
         NULL, NULL, 0),
    MENU("Unprot",
         "Allow change to a range when global protection is on",
         NULL, NULL, 0),
    MENU("Input",
         "Restrict data entry to unprotected cells",
         NULL, NULL, 0),
    MENU("Value",
         "Copy a range, converting formulas to values",
         NULL, NULL, 0),
    MENU("Trans",
         "Copy a range, switching columns and rows",
         NULL, NULL, 0),
    MENU("Search",
         "Find or replace a specified string in a range",
         run_menu, rs_menu, 0),
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
    MENU("Get",
         "Secure a reservation for saving the file",
         NULL, NULL, 0),
    MENU("Release",
         "Release a reservation for saving the file",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const fa_menu[] = {
    MENU("Reservation",
         "Get or release the current file's reservation",
         run_menu, far_menu, 0),
    MENU("Table",
         "Enter a table of file information in the worksheet",
         NULL, NULL, 0),
    MENU("Link-Refresh",
         "Update linked cells",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const fe_menu[] = {
    MENU("Worksheet",
         "Erase a worksheet file",
         NULL, NULL, 0),
    MENU("Print",
         "Erase a print file",
         NULL, NULL, 0),
    MENU("Graph",
         "Erase a graph file",
         NULL, NULL, 0),
    MENU("Other",
         "Erase any file",
         NULL, NULL, 0),
    //"Cancel command -- Do not erase the file"
    //"Erase the file"
    MEND()
};

static struct menu_item const fc_menu[] = {
    MENU("Copy",
         "Copy data from a file on disk to the worksheet",
         NULL, NULL, 0),
    MENU("Add",
         "Add values from a file on disk to values in the worksheet",
         NULL, NULL, 0),
    MENU("Subtract",
         "Subtract values from a file on disk from values in the worksheet",
         NULL, NULL, 0),
    MENU("Entire-File",
         "Incorporate entire file into worksheet",
         NULL, NULL, 0),
    MENU("Named/Specified-Range",
         "Incorporate a range from a file into the worksheet",
         NULL, NULL, 0),
    //"Enter range name or address: "
    MEND()
};

static struct menu_item const fx_menu[] = {
    MENU("Formulas",
         "Save data including formulas",
         NULL, NULL, 0),
    MENU("Values",
         "Save current values and labels",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const fl_menu[] = {
    MENU("Worksheet",
         "List worksheet files",
         NULL, NULL, 0),
    MENU("Print",
         "List print files",
         NULL, NULL, 0),
    MENU("Graph",
         "List graph files",
         NULL, NULL, 0),
    MENU("Other",
         "List all files",
         NULL, NULL, 0),
    MENU("Linked",
         "List all files linked to worksheet",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const fi_menu[] = {
    MENU("Text",
         "Import each line of data as a single label",
         NULL, NULL, 0),
    MENU("Numbers",
         "Import numbers and quoted text into separate columns",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const file_menu[] = {
    MENU("Retrieve",
         "Erase the current worksheet from memory and display the selected worksheet",
         NULL, NULL, 0),
    //"Name of file to retrieve: "
    MENU("Save",
         "Store the entire worksheet in a worksheet file",
         NULL, NULL, 0),
    //"Enter name of file to save: "
    //"Verify password: "
    MENU("Combine",
         "Incorporate all or part of a worksheet file into the current worksheet",
         run_menu, fc_menu, 0),
    //"Enter name of file to combine: "
    MENU("Xtract",
         "Save a specified range in a worksheet file",
         run_menu, fx_menu, 0),
    //"Enter name of file to extract to: "
    //"Enter extract range: "
    MENU("Erase",
         "Erase a file from disk",
         run_menu, fe_menu, 0),
    //"Enter name of file to erase: "
    MENU("List",
         "Display the names of files in the current directory",
         run_menu, fl_menu, 0),
    //"Enter extension of files to list: "
    //"Highlight linked file to list: "
    MENU("Import",
         "Read text or numbers from a text file into the worksheet",
         run_menu, fi_menu, 0),
    //"Enter name of file to import: "
    MENU("Directory",
         "Display and/or change the current directory",
         NULL, NULL, 0),
    //"Enter current directory: "
    MENU("Admin",
         "Reservation  Table  Link-Refresh",
         run_menu, fa_menu, 0),
    MEND()
};

static struct menu_item const pom_menu[] = {
    MENU("Left",
         "Set left margin",
         NULL, NULL, 0),
    //"Enter left margin (0..240): "
    //"Illegal left margin"
    MENU("Right",
         "Set right margin",
         NULL, NULL, 0),
    //"Enter right margin (0..240): "
    //"Illegal right margin"
    MENU("Top",
         "Set top margin",
         NULL, NULL, 0),
    //"Enter top margin (0..32): "
    //"Illegal top margin"
    MENU("Bottom",
         "Set bottom margin",
         NULL, NULL, 0),
    //"Enter bottom margin (0..32): "
    //"Illegal bottom margin"
    MENU("None",
         "Clear all margin settings",
         NULL, NULL, 0),
    MENU("Columns",
         "Print border columns to the left of each print range",
         NULL, NULL, 0),
    //"Enter range for border columns: "
    MENU("Rows",
         "Print border rows above each print range",
         NULL, NULL, 0),
    //"Enter range for border rows: "
    MEND()
};

static struct menu_item const poo_menu[] = {
    MENU("As-Displayed",
         "Print range as displayed",
         NULL, NULL, 0),
    MENU("Cell-Formulas",
         "List entries, one per line",
         NULL, NULL, 0),
    MENU("Formatted",
         "Print headers, footers, and page breaks",
         NULL, NULL, 0),
    MENU("Unformatted",
         "Do not print headers, footers, and page breaks",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const po_menu[] = {
    MENU("Header",
         "Create a header",
         NULL, NULL, 0),
    //"Enter header: "
    MENU("Footer",
         "Create a footer",
         NULL, NULL, 0),
    //"Enter footer: "
    MENU("Margins",
         "Left  Right  Top  Bottom  None",
         run_menu, pom_menu, 0),
    MENU("Borders",
         "Print border columns and/or rows",
         NULL, NULL, 0),
    MENU("Setup",
         "Enter printer setup string",
         NULL, NULL, 0),
    //"Enter setup string: "
    MENU("Pg-Length",
         "Specify number of lines per page",
         NULL, NULL, 0),
    //"Enter lines per page (1..100): "
    //"Illegal page length"
    MENU("Other",
         "As-Displayed  Cell-Formulas  Formatted  Unformatted",
         run_menu, poo_menu, 0),
    MENU("Quit",
         "Return to previous menu",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const pc_menu[] = {
    MENU("All",
         "Return all print settings to defaults",
         NULL, NULL, 0),
    MENU("Range",
         "Clear current print range",
         NULL, NULL, 0),
    MENU("Borders",
         "Clear border column and row ranges",
         NULL, NULL, 0),
    MENU("Format",
         "Return margins, page length, and setup string to defaults",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const print_menu[] = {
    MENU("Printer",
         "Send print output directly to a printer",
         NULL, NULL, 0),
    MENU("File",
         "Send print output to a text file",
         NULL, NULL, 0),
    //"Enter name of text file: "
    MENU("Range",
         "Specify a range to print",
         NULL, NULL, 0),
    //"Enter print range: "
    MENU("Line",
         "Advance paper one line",
         NULL, NULL, 0),
    MENU("Page",
         "Advance paper to top of next page",
         NULL, NULL, 0),
    MENU("Options",
         "Header  Footer  Margins  Borders  Setup  Pg-Length  Other  Quit",
         run_menu, po_menu, 0),
    MENU("Clear",
         "All  Range  Borders  Format",
         run_menu, pc_menu, 0),
    MENU("Align",
         "Reset to top of page (after adjusting paper)",
         NULL, NULL, 0),
    MENU("Go",
         "Print the specified range",
         NULL, NULL, 0),
    MENU("Quit",
         "Return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const gt_menu[] = {
    MENU("Line",
         "Line graph",
         NULL, NULL, 0),
    MENU("Bar",
         "Bar graph",
         NULL, NULL, 0),
    MENU("XY",
         "XY graph",
         NULL, NULL, 0),
    MENU("Stack-Bar",
         "Stacked bar graph",
         NULL, NULL, 0),
    MENU("Pie",
         "Pie chart",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const gol_menu[] = {
    MENU("A",
         "Assign legend for first data range",
         NULL, NULL, 0),
    //"Enter legend for first data range: "
    MENU("B",
         "Assign legend for second data range",
         NULL, NULL, 0),
    //"Enter legend for second data range: "
    MENU("C",
         "Assign legend for third data range",
         NULL, NULL, 0),
    //"Enter legend for third data range: "
    MENU("D",
         "Assign legend for fourth data range",
         NULL, NULL, 0),
    //"Enter legend for fourth data range: "
    MENU("E",
         "Assign legend for fifth data range",
         NULL, NULL, 0),
    //"Enter legend for fifth data range: "
    MENU("F",
         "Assign legend for sixth data range",
         NULL, NULL, 0),
    //"Enter legend for sixth data range: "
    MENU("Range",
         "Specify the range that contains legends for all data ranges",
         NULL, NULL, 0),
    //"Enter legend range: "
    MEND()
};

static struct menu_item const gr_menu[] = {
    MENU("Graph",
         "Clear all current graph settings",
         NULL, NULL, 0),
    MENU("X",
         "Clear X data range",
         NULL, NULL, 0),
    MENU("A",
         "Clear first data range",
         NULL, NULL, 0),
    MENU("B",
         "Clear second data range",
         NULL, NULL, 0),
    MENU("C",
         "Clear third data range",
         NULL, NULL, 0),
    MENU("D",
         "Clear fourth data range",
         NULL, NULL, 0),
    MENU("E",
         "Clear fifth data range",
         NULL, NULL, 0),
    MENU("F",
         "Clear sixth data range",
         NULL, NULL, 0),
    MENU("Ranges",
         "Clear A-F, X, and group ranges",
         NULL, NULL, 0),
    MENU("Options",
         "Cancel all /Graph Options settings",
         NULL, NULL, 0),
    MENU("Quit",
         "Return to previous menu",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const gof_menu[] = {
    MENU("Lines",
         "Connect data points with lines",
         NULL, NULL, 0),
    MENU("Symbols",
         "Display a symbol at each data point",
         NULL, NULL, 0),
    MENU("Both",
         "Display a symbol at each data point and connect data points with lines",
         NULL, NULL, 0),
    MENU("Neither",
         "Display neither symbols nor lines",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const got_menu[] = {
    MENU("First",
         "Assign first line of graph title",
         NULL, NULL, 0),
    //"Enter first line of graph title: "
    MENU("Second",
         "Assign second line of graph title",
         NULL, NULL, 0),
    //"Enter second line of graph title: "
    MENU("X-Axis",
         "Assign x-axis title",
         NULL, NULL, 0),
    //"Enter x-axis title: "
    MENU("Y-Axis",
         "Assign y-axis title",
         NULL, NULL, 0),
    //"Enter y-axis title: "
    MEND()
};

static struct menu_item const gog_menu[] = {
    MENU("Horizontal",
         "Draw grid lines across the graph",
         NULL, NULL, 0),
    MENU("Vertical",
         "Draw grid lines up the graph",
         NULL, NULL, 0),
    MENU("Both",
         "Draw grid lines both across and up the graph",
         NULL, NULL, 0),
    MENU("Clear",
         "Clear all grid lines",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const gosi_menu[] = {
    MENU("Yes",
         "Display scale indicator",
         NULL, NULL, 0),
    MENU("No",
         "Hide scale indicator",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const gos_menu[] = {
    MENU("Y-Scale",
         "Set y-axis scaling",
         NULL, NULL, 0),
    MENU("X-Scale",
         "Set x-axis scaling",
         NULL, NULL, 0),
    MENU("Skip",
         "Display every nth cell in X range",
         NULL, NULL, 0),
    //"Enter skip factor (1..8192): "
    //"Skip factor not within limits"
    MENU("Automatic",
         "Scale automatically based on data ranges",
         NULL, NULL, 0),
    MENU("Manual",
         "Scale according to specified lower and upper limits",
         NULL, NULL, 0),
    MENU("Lower",
         "Specify lower scale limit",
         NULL, NULL, 0),
    //"Enter lower limit: "
    MENU("Upper",
         "Specify upper scale limit",
         NULL, NULL, 0),
    //"Enter upper limit: "
    //"Select cell format for scale numbers"
    MENU("Indicator",
         "Display or hide scale indicator",
         run_menu, gosi_menu, 0),
    MEND()
};

static struct menu_item const god_menu[] = {
    MENU("A",
         "Assign first data-range data labels",
         NULL, NULL, 0),
    //"Enter data-label range for first data range: "
    MENU("B",
         "Assign second data-range data labels",
         NULL, NULL, 0),
    //"Enter data-label range for second data range: "
    MENU("C",
         "Assign third data-range data labels",
         NULL, NULL, 0),
    //"Enter data-label range for third data range: "
    MENU("D",
         "Assign fourth data-range data labels",
         NULL, NULL, 0),
    //"Enter data-label range for fourth data range: "
    MENU("E",
         "Assign fifth data-range data labels",
         NULL, NULL, 0),
    //"Enter data-label range for fifth data range: "
    MENU("F",
         "Assign sixth data-range data labels",
         NULL, NULL, 0),
    //"Enter data-label range for sixth data range: "
    MENU("X",
         "Assign all data labels",
         NULL, NULL, 0),
    //"Enter data-label range for all data ranges: "
    MEND()
};

static struct menu_item const go_menu[] = {
    MENU("Legend",
         "Create legends for data ranges",
         run_menu, gol_menu, 0),
    MENU("Format",
         "Draw lines or symbols in line or XY graphs",
         run_menu, gof_menu, 0),
    MENU("Titles",
         "Add graph titles or axis titles to graph",
         run_menu, got_menu, 0),
    MENU("Grid",
         "Set horizontal and/or vertical grid lines",
         run_menu, gog_menu, 0),
    MENU("Scale",
         "Select scaling options",
         run_menu, gos_menu, 0),
    MENU("Color",
         "Display graph in color",
         NULL, NULL, 0),
    MENU("B&W",
         "Display graph in black and white",
         NULL, NULL, 0),
    MENU("Data-Labels",
         "Label data points in a data range",
         run_menu, god_menu, 0),
    MEND()
};

static struct menu_item const gn_menu[] = {
    MENU("Use",
         "Make a named graph current",
         NULL, NULL, 0),
    //"Enter name of graph to make current: "
    MENU("Create",
         "Name the current graph",
         NULL, NULL, 0),
    //"Enter graph name: "
    MENU("Delete",
         "Delete a named graph",
         NULL, NULL, 0),
    //"Enter name of graph to delete: "
    MENU("Delete all named graphs",
         "Table",
         NULL, NULL, 0),
    MENU("Table",
         "Create a table of named graphs",
         NULL, NULL, 0),
    //"Enter range for table: "
    MEND()
};

static struct menu_item const gg_menu[] = {
    MENU("Columnwise",
         "Use columns as data ranges",
         NULL, NULL, 0),
    MENU("Rowwise",
         "Use rows as data ranges",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const graph_menu[] = {
    MENU("Type",
         "Line  Bar  XY  Stack-Bar  Pie",
         run_menu, gt_menu, 0),
    MENU("X",
         "Set X data range",
         NULL, NULL, 0),
    //"Enter x-axis range: "
    MENU("A",
         "Set first data range",
         NULL, NULL, 0),
    //"Enter first data range: "
    MENU("B",
         "Set second data range",
         NULL, NULL, 0),
    //"Enter second data range: "
    MENU("C",
         "Set third data range",
         NULL, NULL, 0),
    //"Enter third data range: "
    MENU("D",
         "Set fourth data range",
         NULL, NULL, 0),
    //"Enter fourth data range: "
    MENU("E",
         "Set fifth data range",
         NULL, NULL, 0),
    //"Enter fifth data range: "
    MENU("F",
         "Set sixth data range",
         NULL, NULL, 0),
    //"Enter sixth data range: "
    MENU("Reset",
         "Graph  X  A  B  C  D  E  F  Ranges  Options  Quit",
         run_menu, gr_menu, 0),
    MENU("View",
         "View the current graph",
         NULL, NULL, 0),
    MENU("Save",
         "Save the current graph in a file for printing",
         NULL, NULL, 0),
    //"Enter graph file name: "
    MENU("Options",
         "Legend  Format  Titles  Grid  Scale  Color  B&W  Data-Labels  Quit",
         run_menu, go_menu, 0),
    MENU("Name",
         "Use  Create  Delete  Reset  Table",
         run_menu, gn_menu, 0),
    MENU("Group",
         "Set all data ranges at once",
         run_menu, gg_menu, 0),
    //"Enter group range: "
    MENU("Quit",
         "Return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const dt_menu[] = {
    MENU("1",
         "One input cell, one or more dependent formulas",
         NULL, NULL, 0),
    //"Enter table range: "
    //"Enter input cell 1: "
    MENU("2",
         "Two input cells, one dependent formula",
         NULL, NULL, 0),
    //"Enter input cell 2: "
    MENU("Reset",
         "Clear table ranges and input cells for all data tables",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const ds_menu[] = {
    MENU("Data-Range",
         "Select records to be sorted",
         NULL, NULL, 0),
    //"Enter data range: "
    MENU("Primary-Key",
         "Specify primary order for records",
         NULL, NULL, 0),
    //"Primary sort key: "
    //"Sort order (A or D): "
    //"A"
    //"D"
    MENU("Secondary-Key",
         "Specify order for records with same primary key",
         NULL, NULL, 0),
    //"Secondary sort key: "
    MENU("Clear",
         "Clear data range and sort keys",
         NULL, NULL, 0),
    MENU("Go",
         "Sort data and return to READY mode",
         NULL, NULL, 0),
    MENU("Quit",
         "Return to READY mode",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const dq_menu[] = {
    MENU("Input",
         "Specify range that contains records to search",
         NULL, NULL, 0),
    //"Enter input range: "
    MENU("Criteria",
         "Specify the range that contains criteria",
         NULL, NULL, 0),
    //"Enter criteria range: "
    MENU("Output",
         "Specify the range to which extracted records are copied",
         NULL, NULL, 0),
    MENU("Find",
         "Highlight each record that matches criteria",
         NULL, NULL, 0),
    MENU("Extract",
         "Copy all records that match criteria to output range",
         NULL, NULL, 0),
    MENU("Unique",
         "Copy records that match criteria to output range, eliminating duplicates",
         NULL, NULL, 0),
    MENU("Delete",
         "Delete all records that match criteria",
         NULL, NULL, 0),
    //"Clear input, criteria, and output ranges"
    //"Cancel"
    //"Do not delete any records; return to previous menu"
    //"Delete records and close up spaces in input range"
    MEND()
};

static struct menu_item const dm_menu[] = {
    MENU("Invert",
         "Create the inverse of a matrix",
         NULL, NULL, 0),
    //"Enter range to invert: "
    MENU("Multiply",
         "Multiply two ranges as matrices",
         NULL, NULL, 0),
    //"Enter first range to multiply: "
    //"Enter second range to multiply: "
    MEND()
};

static struct menu_item const dri_menu[] = {
    MENU("Compute",
         "Calculate the y-axis intercept automatically",
         NULL, NULL, 0),
    MENU("Zero",
         "Use zero as the y-axis intercept",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const dr_menu[] = {
    MENU("X-Range",
         "Specify independent variables (X range)",
         NULL, NULL, 0),
    //"Enter independent variables, or X range: "
    MENU("Y-Range",
         "Specify dependent variable (Y range)",
         NULL, NULL, 0),
    //"Enter dependent variable, or Y range: "
    MENU("Output-Range",
         "Specify the output range",
         NULL, NULL, 0),
    MENU("Intercept",
         "Compute  Zero",
         run_menu, dri_menu, 0),
    MENU("Clear",
         "Clear the X range, Y range, output range, and reset Intercept to Compute",
         NULL, NULL, 0),
    MENU("Go",
         "Calculate a data regression on specified ranges",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const dpf_menu[] = {
    MENU("Create",
         "Create a format line at the current cell",
         NULL, NULL, 0),
    MENU("Edit",
         "Edit a format line at the current cell",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const dp_menu[] = {
    MENU("Format-Line",
         "Create or edit a format line at the current cell",
         run_menu, dpf_menu, 0),
    MENU("Input-Column",
         "Specify a column of labels to parse",
         NULL, NULL, 0),
    //"Enter column of labels to parse: "
    MENU("Range",
         "Specify the range in which parsed data is placed",
         NULL, NULL, 0),
    MENU("Clear",
         "Clear input column and output range",
         NULL, NULL, 0),
    MENU("Go",
         "Parse labels in the input column and place them in the output range",
         NULL, NULL, 0),
    MEND()
};

static struct menu_item const data_menu[] = {
    MENU("Fill",
         "Fill a range with a sequence of values",
         NULL, NULL, 0),
    //"Enter fill range: "
    //"Start: "
    //"Step: "
    //"Stop: "
    MENU("Table",
         "Create a table of values",
         run_menu, dt_menu, 0),
    MENU("Sort",
         "Sort database records",
         run_menu, ds_menu, 0),
    MENU("Query",
         "Find all records that satisfy given criteria",
         run_menu, dq_menu, 0),
    MENU("Distribution",
         "Calculate frequency distribution of the values in a range",
         NULL, NULL, 0),
    //"Enter values range: "
    //"Enter bin range: "
    MENU("Matrix",
         "Multiply and invert matrices",
         run_menu, dm_menu, 0),
    //"Enter output range: "
    MENU("Regression",
         "Calculate linear regression",
         run_menu, dr_menu, 0),
    MENU("Parse",
         "Convert a column of long labels into a range of labels or numbers",
         run_menu, dp_menu, 0),
    MEND()
};

static struct menu_item const addin_menu[] = {
    MENU("Attach",
         "Load an add-in program into memory",
         NULL, NULL, 0),
    MENU("Detach",
         "Remove an attached add-in program from memory",
         NULL, NULL, 0),
    MENU("Invoke",
         "Activate an attached add-in program",
         NULL, NULL, 0),
    MENU("Clear",
         "Remove all attached add-in programs from memory",
         NULL, NULL, 0),
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
    MENU("No",
         "Do not end 1-2-3 session; return to READY mode",
         run_abort, NULL, 0),
    MENU("Yes",
         "End 1-2-3 session (Remember to save your worksheet first)",
         run_quit, NULL, 0),
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
        move(1, 0);
        clrtoeol();
        addstr(menu[option].desc);
        hidecursor();
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
            move(0, 0); clrtoeol();
            move(1, 0); clrtoeol();
            return ESC;
        case ctl('g'):
        case EOF:
            move(0, 0); clrtoeol();
            move(1, 0); clrtoeol();
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
        move(0, 0); clrtoeol();
        move(1, 0); clrtoeol();
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
