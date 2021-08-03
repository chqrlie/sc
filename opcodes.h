/*      SC      A Spreadsheet Calculator
 *              Expression node definitions.
 *
 *              original by Charlie Gordon: August, 2021
 *              $Revision: 8.1 $
 */

#ifndef __
#define __(op,str,min,max,efun,arg)
#endif
#ifndef XX
#define XX(op,str,min,max,efun,arg)  OP(op,str,min,max,efun,arg)
#endif

/* dummy nodes for ancillary use (@EXT) */
OP( OP_DUMMY,       NULL, -2, 2, NULL, NULL)

/* constants and references */
OP( OP_CONST,       NULL, -2, 2, eval_const, NULL)
OP( OP_SCONST,      NULL, -2, 2, eval_sconst, NULL)
OP( OP_VAR,         NULL, -2, 2, eval_var, NULL)
OP( OP_RANGEARG,    NULL, -2, 1, NULL, NULL)

/* unary operators */
XX( OP_FIXED,       "@fixed ", -3, 2, NULL, NULL)
XX( OP_PFIXED,      "(@fixed)", -3, 2, NULL, NULL)
OP( OP_UMINUS,      "-", -3, 2, eval_neg, NULL)
OP( OP_UPLUS,       "+", -3, 2, NULL, NULL)
XX( OP_BANG,        "!", -3, 2, eval_not, NULL)

/* binary operators */
OP( OP_PLUS,        "+", -3, 2, eval_add, NULL)
OP( OP_MINUS,       "-", -3, 2, eval_sub, NULL)
OP( OP_STAR,        "*", -3, 2, eval_mul, NULL)
OP( OP_SLASH,       "/", -3, 2, eval_div, NULL)
XX( OP_PERCENT,     "%", -3, 2, eval_mod, NULL)// XXX: should be postfix %
OP( OP_CARET,       "^", -3, 2, eval_fn2, pow)
XX( OP_QMARK,       "?", -3, 2, eval_if, NULL)
OP( OP_COLON,       ":", -3, 2, NULL, NULL)
XX( OP_SEMI,        ";", -3, 2, eval_fl2, makecolor)
OP( OP_EQ,          "=", -3, 2, eval_cmp, NULL)
OP( OP_LG,          "<>", -3, 2, eval_cmp, NULL)
XX( OP_NE,          "!=", -3, 2, eval_cmp, NULL)
OP( OP_LT,          "<", -3, 2, eval_cmp, NULL)
OP( OP_LE,          "<=", -3, 2, eval_cmp, NULL)
OP( OP_GE,          ">=", -3, 2, eval_cmp, NULL)
OP( OP_GT,          ">", -3, 2, eval_cmp, NULL)
    /* should have : ! ~ */
XX( OP_AMPERSAND,   "&", -3, 2, eval_and, NULL)
XX( OP_VBAR,        "|", -3, 2, eval_or, NULL)
XX( OP_SHARP,       "#", -3, 2, eval_concat, NULL)
OP( OP_COMMA,       ",", -3, 2, NULL, NULL)

/* 6.5 Matrix functions */
/* 6.6 Bit operand functions */
OP( OP_BITAND,      "@bitand", 2, 2, eval_fl2, bitand) // BITAND(value1, value2)    Bitwise boolean AND of two numbers.
OP( OP_BITLSHIFT,   "@bitlshift", 2, 2, eval_fl2, bitlshift) // BITLSHIFT(value, shift_amount)    Shifts the bits of the input a certain number of places to the left.
OP( OP_BITOR,       "@bitor", 2, 2, eval_fl2, bitor) // BITOR(value1, value2)    Bitwise boolean OR of 2 numbers.
OP( OP_BITRSHIFT,   "@bitrshift", 2, 2, eval_fl2, bitrshift) // BITRSHIFT(value, shift_amount)    Shifts the bits of the input a certain number of places to the right.
OP( OP_BITXOR,      "@bitxor", 2, 2, eval_fl2, bitxor) // BITXOR(value1, value2)    Bitwise XOR (exclusive OR) of 2 numbers.

/* 6.7 Byte-position text functions */
/* 6.8 Complex Number Functions */
/* 6.9 Database Functions */

/* 6.10 Date and Time Functions */
OP( OP_DATE,        "@date", 1, 2, eval_date, NULL)
__( OP_DATEDIF,     "@datedif", 3, 3, NULL, NULL)
__( OP_DATEVALUE,   "@datevalue", 1, 1, NULL, NULL)
OP( OP_DAY,         "@day", 1, 1, eval_time, NULL)
__( OP_DAYS,        "@days", 2, 2, NULL, NULL)
__( OP_DAYS360,     "@days360", 2, 3, NULL, NULL)
__( OP_EDATE,       "@odate", 2, 2, NULL, NULL)
__( OP_EOMONTH,     "@oemonth", 2, 2, NULL, NULL)
OP( OP_HOUR,        "@hour", 1, 1, eval_time, NULL)
__( OP_ISOWEEKNUM,  "@isoweeknum", 1, 1, NULL, NULL)
OP( OP_MINUTE,      "@minute", 1, 1, eval_time, NULL)
OP( OP_MONTH,       "@month", 1, 1, eval_time, NULL)
__( OP_NETWORKDAYS, "@networkdays", 2, 4, NULL, NULL)
OP( OP_NOW,         "@now", -1, -1, eval_now, NULL)
OP( OP_SECOND,      "@second", 1, 1, eval_time, NULL)
__( OP_TIME,        "@time", 3, 3, NULL, NULL)
__( OP_TIMEVALUE,   "@timevalue", 1, 1, NULL, NULL)
__( OP_TODAY,       "@today", 0, 0, NULL, NULL)
__( OP_WEEKDAY,     "@weekday", 1, 2, NULL, NULL)
__( OP_WEEKNUM,     "@weeknum", 1, 2, NULL, NULL)
__( OP_WORKDAY,     "@workday", 2, 4, NULL, NULL)
OP( OP_YEAR,        "@year", 1, 1, eval_time, NULL)
__( OP_YEARFRAC,    "@yearfrac", 2, 3, NULL, NULL)
XX( OP_DTS,         "@dts", 3, 3, eval_dts, NULL)
XX( OP_TTS,         "@tts", 3, 3, eval_fn3, dotts)

/* 6.11 External Access Functions */
__( OP_DDE,         "@dde", 3, 4, NULL, NULL)
XX( OP_EXT,         "@ext", 2, 2, eval_ext, NULL)// XXX: should be 1,-1
__( OP_HYPERLINK,   "@hyperlink", 1, 2, NULL, NULL)

/* 6.12 Financial Functions */
OP( OP_FV,          "@fv", 3, 3, eval_fn3, fin_fv)
OP( OP_PMT,         "@pmt", 3, 3, eval_fn3, fin_pmt)
OP( OP_PV,          "@pv", 3, 3, eval_fn3, fin_pv)

/* 6.13 Information Functions */
__( OP_AREAS,       "@areas", 1, 1, NULL, NULL)
__( OP_CELL,        "@cell", 1, 2, NULL, NULL)
__( OP_COLUMN,      "@column", 1, 1, NULL, NULL)
__( OP_COLUMNS,     "@columns", 1, 1, NULL, NULL)
OP( OP_COUNT,       "@count", 1, -1, eval_rangeop, NULL)
__( OP_COUNTA,      "@counta", 1, -1, NULL, NULL)
__( OP_COUNTBLANK,  "@countblank", 1, -1, NULL, NULL)
__( OP_COUNTIF,     "@countif", 2, 2, NULL, NULL)
__( OP_COUNTIFS,    "@countifs", 2, -1, NULL, NULL)
__( OP_ERROR_TYPE,  "@error.type", 1, 1, NULL, NULL)
__( OP_FORMULA,     "@counta", 1, 1, NULL, NULL)
__( OP_ISBLANK,     "@isblank", 1, 1, NULL, NULL)
__( OP_ISERR,       "@isblank", 1, 1, NULL, NULL)
__( OP_ISERROR,     "@isblank", 1, 1, NULL, NULL)
__( OP_ISEVEN,      "@iseven", 1, 1, NULL, NULL)
__( OP_ISFORMULA,   "@isformula", 1, 1, NULL, NULL)
__( OP_ISLOGICAL,   "@islogical", 1, 1, NULL, NULL)
__( OP_ISNA,        "@isna", 1, 1, NULL, NULL)
__( OP_ISODD,       "@isodd", 1, 1, NULL, NULL)
__( OP_ISREF,       "@isref", 1, 1, NULL, NULL)
__( OP_ISTEXT,      "@istext", 1, 1, NULL, NULL)
__( OP_N,           "@n", 1, 1, NULL, NULL)
__( OP_NA,          "@na", 0, 0, NULL, NULL)
__( OP_NUMBERVALUE, "@numbervalue", 1, 3, NULL, NULL)
__( OP_ROW,         "@row", 1, 1, NULL, NULL)
__( OP_ROWS,        "@rows", 1, 1, eval_rangeop, NULL)
__( OP_SHEET,       "@sheet", 1, 1, NULL, NULL)
__( OP_SHEETS,      "@sheets", 1, 1, NULL, NULL)
__( OP_TYPE,        "@type", 1, 1, NULL, NULL)
__( OP_VALUE,       "@value", 1, 1, NULL, NULL)
XX( OP_ROWS,        "@rows", 1, 1, eval_rangeop, NULL)
XX( OP_COLS,        "@cols", 1, 1, eval_rangeop, NULL)
XX( OP_MYROW,       "@myrow", -1, -1, NULL, NULL)
XX( OP_MYCOL,       "@mycol", -1, -1, NULL, NULL)
XX( OP_LASTROW,     "@lastrow", -1, -1, NULL, NULL)
XX( OP_LASTCOL,     "@lastcol", -1, -1, NULL, NULL)
XX( OP_FILENAME,    "@filename", 1, 1, eval_filename, NULL)
XX( OP_COLTOA,      "@coltoa", 1, 1, eval_coltoa, NULL)
XX( OP_NVAL,        "@nval", 2, 2, eval_nval, NULL)
XX( OP_SVAL,        "@sval", 2, 2, eval_sval, NULL)
XX( OP_STON,        "@ston", 1, 1, eval_ston, NULL)
XX( OP_NUMITER,     "@numiter", -1, -1, NULL, NULL)
XX( OP_ERR,         "@err", -1, -1, NULL, NULL)

/* 6.14 Lookup Functions */
__( OP_ADDRESS,     "@address", 2, 5, NULL, NULL)
__( OP_CHOOSE,      "@choose", 1, 2, NULL, NULL)
__( OP_GETPIVOTDATA, "@getpivotdata", 2, 4, NULL, NULL)
OP( OP_HLOOKUP,     "@hlookup", 3, 3, eval_rangeop, NULL) /* 3, 4 */
OP( OP_INDEX,       "@index", 2, 3, eval_rangeop, NULL) /* 1, 4 */
__( OP_INDIRECT,    "@index", 1, 2, NULL, NULL)
OP( OP_LOOKUP,      "@lookup", 2, 2, eval_rangeop, NULL) /* 2, 3 */
__( OP_MATCH,       "@match", 2, 3, NULL, NULL)
__( OP_MULTIPLE_OPERATIONS, "@multiple.operations", 3, 5, NULL, NULL)
__( OP_OFFSET,      "@offset", 3, 5, NULL, NULL)
XX( OP_STINDEX,     "@stindex", 2, 3, eval_rangeop, NULL)
OP( OP_VLOOKUP,     "@vlookup", 3, 3, eval_rangeop, NULL) /* 3, 4 */

/* 6.15 Logical Functions */
__( OP_AND,         "@and", 1, -1, NULL, NULL)
__( OP_FALSE,       "@false", 0, 0, NULL, NULL)
OP( OP_IF,          "@if", 3, 3, eval_if, NULL) /* 1, 3 */
__( OP_IFERROR,     "@iferror", 2, 2, NULL, NULL)
__( OP_IFNA,        "@ifna", 2, 2, NULL, NULL)
__( OP_NOT,         "@not", 1, 1, NULL, NULL)
__( OP_OR,          "@or", 1, -1, NULL, NULL)
__( OP_TRUE,        "@true", 0, 0, NULL, NULL)
__( OP_XOR,         "@xor", 1, -1, NULL, NULL)

/* 6.16 Mathematical Functions */
OP( OP_ABS,         "@abs", 1, 1, eval_fn1, fabs)
OP( OP_ACOS,        "@acos", 1, 1, eval_fn1, acos)
__( OP_ACOSH,       "@acosh", 1, 1, NULL, NULL)
__( OP_ACOT,        "@acot", 1, 1, NULL, NULL)
__( OP_ACOTH,       "@acoth", 1, 1, NULL, NULL)
OP( OP_ASIN,        "@asin", 1, 1, eval_fn1, asin)
__( OP_ASINH,       "@asinh", 1, 1, NULL, NULL)
OP( OP_ATAN,        "@atan", 1, 1, eval_fn1, atan)
OP( OP_ATAN2,       "@atan2", 2, 2, eval_fn2, atan2)
__( OP_ATANH,       "@atanh", 1, 1, NULL, NULL)
__( OP_BESSELI,     "@besseli", 2, 2, NULL, NULL)
__( OP_BESSELJ,     "@besselj", 2, 2, NULL, NULL)
__( OP_BESSELK,     "@besselk", 2, 2, NULL, NULL)
__( OP_BESSELY,     "@bessely", 2, 2, NULL, NULL)
__( OP_COMBIN,      "@combin", 2, 2, NULL, NULL)
__( OP_COMBINA,     "@combina", 2, 2, NULL, NULL)
__( OP_CONVERT,     "@convert", 3, 3, NULL, NULL)
OP( OP_COS,         "@cos", 1, 1, eval_fn1, cos)
__( OP_COSH,        "@cosh", 1, 1, NULL, NULL)
__( OP_COT,         "@cot", 1, 1, NULL, NULL)
__( OP_COTH,        "@coth", 1, 1, NULL, NULL)
__( OP_CSC,         "@csc", 1, 1, NULL, NULL)
__( OP_CSCH,        "@csch", 1, 1, NULL, NULL)
__( OP_DEGREES,     "@degrees", 1, 1, NULL, NULL)
__( OP_DELTA,       "@delta", 1, 2, NULL, NULL)
__( OP_ERF,         "@erf", 1, 2, NULL, NULL)
__( OP_ERFC,        "@erfc", 1, 1, NULL, NULL)
__( OP_EUROCONVERT, "@euroconvert", 3, 5, NULL, NULL)
__( OP_EVEN,        "@even", 1, 1, NULL, NULL)
OP( OP_EXP,         "@exp", 1, 1, eval_fn1, exp)
__( OP_FACT,        "@fact", 1, 1, NULL, NULL)
__( OP_FACTDOUBLE,  "@factdouble", 1, 1, NULL, NULL)
__( OP_GAMMA,       "@gamma", 1, 1, NULL, NULL)
__( OP_GAMMALN,     "@gammaln", 1, 1, NULL, NULL)
__( OP_GCD,         "@gcd", 1, -1, NULL, NULL)
__( OP_GESTEP,      "@gestep", 2, 2, NULL, NULL)
__( OP_LCM,         "@lcm", 1, -1, NULL, NULL)
__( OP_LN,          "@ln", 1, 1, NULL, NULL)
OP( OP_LOG,         "@log", 1, 1, eval_fn1, log) /* 1, 2 */
OP( OP_LOG10,       "@log10", 1, 1, eval_fn1, log10)
__( OP_MOD,         "@mod", 2, 2, NULL, NULL)
__( OP_MULTINOMIAL, "@multinomial", 1, -1, NULL, NULL)
__( OP_ODD,         "@odd", 1, 1, NULL, NULL)
OP( OP_PI,          "@pi", -1, -1, eval_pi, NULL)
__( OP_POWER,       "@power", 2, 2, NULL, NULL)
__( OP_PRODUCT,     "@product", 1, -1, NULL, NULL)
__( OP_QUOTIENT,    "@quotient", 2, 2, NULL, NULL)
__( OP_RADIANS,     "@radians", 1, 1, NULL, NULL)
OP( OP_RAND,        "@rand", 0, 0, eval_rand, NULL)
OP( OP_RANDBETWEEN, "@randbetween", 2, 2, eval_fn2, rand_between)
__( OP_SEC,         "@sec", 1, 1, NULL, NULL)
__( OP_SECH,        "@sech", 1, 1, NULL, NULL)
__( OP_SERIESSUM,   "@seriessum", 4, 4, NULL, NULL)
__( OP_SIGN,        "@sign", 1, 1, NULL, NULL)
OP( OP_SIN,         "@sin", 1, 1, eval_fn1, sin)
__( OP_SINH,        "@sinh", 1, 1, NULL, NULL)
OP( OP_SQRT,        "@sqrt", 1, 1, eval_fn1, sqrt)
__( OP_SQRTPI,      "@sqrtpi", 1, 1, NULL, NULL)
__( OP_SUBTOTAL,    "@subtotal", 1, -1, NULL, NULL)
OP( OP_SUM,         "@sum", 1, -1, eval_rangeop, NULL)
__( OP_SUMIF,       "@sumif", 2, -1, NULL, NULL)
__( OP_SUMIFS,      "@sumifs", 3, -1, NULL, NULL)
__( OP_SUMPRODUCT,  "@sumproduct", 1, -1, NULL, NULL)
__( OP_SUMSQ,       "@sumsq", 1, -1, NULL, NULL)
__( OP_SUMX2MY2,    "@sumx2my2", 2, 2, NULL, NULL)
__( OP_SUMX2PY2,    "@sumx2py2", 2, 2, NULL, NULL)
__( OP_SUMXMY2,     "@sumxmy2", 2, 2, NULL, NULL)
OP( OP_TAN,         "@tan", 1, 1, eval_fn1, tan)
__( OP_TANH,        "@tanh", 1, 1, NULL, NULL)

XX( OP_FABS,        "@fabs", 1, 1, eval_fn1, fabs)
XX( OP_HYPOT,       "@hypot", 2, 2, eval_fn2, hypot)
XX( OP_DTR,         "@dtr", 1, 1, eval_fn1, dtr)
XX( OP_RTD,         "@rtd", 1, 1, eval_fn1, rtd)
XX( OP_POW,         "@pow", 2, 2, eval_fn2, pow)
XX( OP_PROD,        "@prod", 1, -1, eval_rangeop, NULL)

/* 6.17 Rounding Functions */
__( OP_CEILING,     "@ceiling", 1, 3, NULL, NULL)
__( OP_INT,         "@int", 1, 1, NULL, NULL)
XX( OP_FLOOR,       "@floor", 1, 1, eval_fn1, floor)
__( OP_MROUND,      "@mround", 2, 2, NULL, NULL)
OP( OP_ROUND,       "@round", 2, 2, eval_fn2, doround)
__( OP_ROUNDDOWN,   "@rounddown", 2, 2, NULL, NULL)
__( OP_ROUNDUP,     "@roundup", 2, 2, NULL, NULL)
__( OP_TRUNC,       "@trunc", 2, 2, NULL, NULL)

XX( OP_CEIL,        "@ceil", 1, 1, eval_fn1, ceil)
XX( OP_RND,         "@rnd", 1, 1, eval_fn1, dornd)

/* 6.18 Statistical Functions */
__( OP_AVEDEV,      "@avedev", 1, -1, NULL, NULL)
__( OP_AVERAGE,     "@average", 1, -1, NULL, NULL)
__( OP_AVERAGEA,    "@averagea", 1, -1, NULL, NULL)
__( OP_AVERAGEIF,   "@averageif", 2, 3, NULL, NULL)
__( OP_AVERAGEIFS,  "@averageifs", 3, -1, NULL, NULL)
__( OP_LARGE,       "@large", 1, -1, NULL, NULL)
OP( OP_MAX,         "@max", 1, -1, eval_rangeop, NULL)
__( OP_MAXA,        "@maxa", 1, -1, NULL, NULL)
__( OP_MEDIAN,      "@median", 1, -1, NULL, NULL)
OP( OP_MIN,         "@min", 1, -1, eval_rangeop, NULL)
__( OP_MINA,        "@mina", 1, -1, NULL, NULL)
__( OP_MODE,        "@mode", 1, -1, NULL, NULL)
__( OP_PERMUT,      "@permut", 2, 2, NULL, NULL)
__( OP_PERMUTATIONA,  "@permutationa", 2, 2, NULL, NULL)
__( OP_RANK,        "@rank", 1, -1, NULL, NULL)
__( OP_SMALL,       "@small", 1, -1, NULL, NULL)
__( OP_STDEV,       "@stdev", 1, -1, NULL, NULL)
__( OP_STDEVA,      "@stdeva", 1, -1, NULL, NULL)
__( OP_STDEVP,      "@stdevp", 1, -1, NULL, NULL)
__( OP_STDEVPA,     "@stdevpa", 1, -1, NULL, NULL)
__( OP_VAR,         "@var", 1, -1, NULL, NULL)
__( OP_VARA,        "@vara", 1, -1, NULL, NULL)
__( OP_VARP,        "@varp", 1, -1, NULL, NULL)
__( OP_VARPA,       "@varpa", 1, -1, NULL, NULL)

XX( OP_AVG,         "@avg", 1, -1, eval_rangeop, NULL)
XX( OP_LMAX,        "@max", 1, -1, eval_lmax, NULL)
XX( OP_LMIN,        "@min", 1, -1, eval_lmin, NULL)
XX( OP_STDDEV,      "@stddev", 1, -1, eval_rangeop, NULL)

/* 6.19 Number Representation Conversion Functions */
__( OP_ARABIC,      "@arabic", 1, 1, NULL, NULL)
__( OP_BASE,        "@base", 2, 3, NULL, NULL)
__( OP_BIN2DEC,     "@bin2dec", 1, 1, NULL, NULL)
__( OP_BIN2HEX,     "@bin2hex", 1, 2, NULL, NULL)
__( OP_BIN2OCT,     "@bin2oct", 1, 2, NULL, NULL)
__( OP_DEC2BIN,     "@dec2bin", 1, 2, NULL, NULL)
__( OP_DEC2HEX,     "@dec2hex", 1, 2, NULL, NULL)
__( OP_DEC2OCT,     "@dec2oct", 1, 2, NULL, NULL)
__( OP_DECIMAL,     "@decimal", 2, 2, NULL, NULL)
__( OP_HEX2BIN,     "@hex2bin", 1, 2, NULL, NULL)
__( OP_HEX2DEC,     "@hex2dec", 1, 1, NULL, NULL)
__( OP_HEX2OCT,     "@hex2oct", 1, 2, NULL, NULL)
__( OP_OCT2BIN,     "@oct2bin", 1, 2, NULL, NULL)
__( OP_OCT2DEC,     "@oct2dec", 1, 1, NULL, NULL)
__( OP_OCT2HEX,     "@oct2hex", 1, 2, NULL, NULL)
__( OP_ROMAN,       "@roman", 1, 2, NULL, NULL)
XX( OP_FMT,         "@fmt", 2, 2, eval_fmt, NULL)

/* 6.20 Text Functions */
__( OP_ASC,         "@asc", 1, 1, NULL, NULL)
__( OP_CHAR,        "@char", 1, 1, NULL, NULL)
__( OP_CLEAN,       "@clean", 1, 1, NULL, NULL)
__( OP_CODE,        "@code", 1, 1, NULL, NULL)
__( OP_CONCATENATE, "@CONCATENATE", 1, -1, NULL, NULL)
__( OP_DOLLAR,      "@dollar", 1, 2, NULL, NULL)
__( OP_EXACT,       "@exact", 2, 2, NULL, NULL)
__( OP_FIND,        "@find", 2, 3, NULL, NULL)
__( OP_FIXED,       "@fixed", 1, 3, NULL, NULL)
__( OP_JIS,         "@jis", 1, 1, NULL, NULL)
__( OP_LEFT,        "@left", 1, 2, NULL, NULL)
__( OP_LEN,         "@len", 1, 1, NULL, NULL)
OP( OP_LOWER,       "@lower", 1, 1, eval_case, NULL)
__( OP_MID,         "@mid", 3, 3, NULL, NULL)
__( OP_PROPER,      "@proper", 1, 1, NULL, NULL)
__( OP_REPLACE,     "@replace", 4, 4, NULL, NULL)
__( OP_REPT,        "@rept", 2, 2, NULL, NULL)
__( OP_RIGHT,       "@right", 1, 2, NULL, NULL)
__( OP_SEARCH,      "@search", 2, 3, NULL, NULL)
__( OP_SUBSTITUTE,  "@substitute", 3, 4, NULL, NULL)
__( OP_T,           "@t", 1, 1, NULL, NULL)
__( OP_TEXT,        "@text", 2, 2, NULL, NULL)
__( OP_TRIM,        "@trim", 1, 1, NULL, NULL)
__( OP_UNICHAR,     "@unichar", 1, 1, NULL, NULL)
__( OP_UNICODE,     "@unicode", 1, 1, NULL, NULL)
OP( OP_UPPER,       "@upper", 1, 1, eval_case, NULL)

XX( OP_SUBSTR,      "@substr", 3, 3, eval_substr, NULL)
XX( OP_CAPITAL,     "@capital", 1, 1, eval_case, NULL)
XX( OP_EQS,         "@eqs", 2, 2, eval_cmp, NULL)

/* SC specific functions */

XX( OP_BLACK,       "@black", -1, -1, NULL, NULL)
XX( OP_BLUE,        "@blue", -1, -1, NULL, NULL)
XX( OP_CYAN,        "@cyan", -1, -1, NULL, NULL)
XX( OP_GREEN,       "@green", -1, -1, NULL, NULL)
XX( OP_MAGENTA,     "@magenta", -1, -1, NULL, NULL)
XX( OP_RED,         "@red", -1, -1, NULL, NULL)
XX( OP_WHITE,       "@white", -1, -1, NULL, NULL)
XX( OP_YELLOW,      "@yellow", -1, -1, NULL, NULL)

#undef OP
#undef XX
#undef __
