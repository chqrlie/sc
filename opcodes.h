/* dummy nodes for ancillary use (@EXT) */
    OP( OP_DUMMY,      NULL, -2, 2)

/* constants and references */
    OP( OP_CONST,      NULL, -2, 2)
    OP( OP_SCONST,     NULL, -2, 2)
    OP( OP_VAR,        NULL, -2, 2)
    OP( OP_RANGEARG,   NULL, -2, 1)

/* unary operators */
    OP( OP_FIXED,      "@fixed ", -3, 2)    /* SC specific */
    OP( OP_PFIXED,     "(@fixed)", -3, 2)   /* SC specific */
    OP( OP_UMINUS,     "-", -3, 2)
    OP( OP_UPLUS,      "+", -3, 2)
    OP( OP_BANG,       "!", -3, 2)          /* SC specific */

/* binary operators */
    OP( OP_PLUS,       "+", -3, 2)
    OP( OP_MINUS,      "-", -3, 2)
    OP( OP_STAR,       "*", -3, 2)
    OP( OP_SLASH,      "/", -3, 2)
    OP( OP_PERCENT,    "%", -3, 2)          /* SC specific */ // XXX: should be postfix
    OP( OP_CARET,      "^", -3, 2)
    OP( OP_QMARK,      "?", -3, 2)          /* SC specific */
    OP( OP_COLON,      ":", -3, 2)
    OP( OP_SEMI,       ";", -3, 2)          /* SC specific */
    OP( OP_EQ,         "=", -3, 2)
    OP( OP_LG,         "<>", -3, 2)
    OP( OP_NE,         "!=", -3, 2)         /* SC specific */
    OP( OP_LT,         "<", -3, 2)
    OP( OP_LE,         "<=", -3, 2)
    OP( OP_GE,         ">=", -3, 2)
    OP( OP_GT,         ">", -3, 2)
    /* should have : ! ~ */
    OP( OP_AMPERSAND,  "&", -3, 2)          /* SC specific */
    OP( OP_VBAR,       "|", -3, 2)          /* SC specific */
    OP( OP_SHARP,      "#", -3, 2)          /* SC specific */
    OP( OP_COMMA,      ",", -3, 2)

/* 6.5 Matrix functions */
/* 6.6 Bit operand functions */
    OP( OP_BITAND, "@bitand", 2, 2) // BITAND(value1, value2)    Bitwise boolean AND of two numbers. Learn more.
    OP( OP_BITLSHIFT, "@bitlshift", 2, 2) // BITLSHIFT(value, shift_amount)    Shifts the bits of the input a certain number of places to the left. Learn more.
    OP( OP_BITOR, "@bitor", 2, 2) // BITOR(value1, value2)    Bitwise boolean OR of 2 numbers. Learn more.
    OP( OP_BITRSHIFT, "@bitrshift", 2, 2) // BITRSHIFT(value, shift_amount)    Shifts the bits of the input a certain number of places to the right. Learn more.
    OP( OP_BITXOR, "@bitxor", 2, 2) // BITXOR(value1, value2)    Bitwise XOR (exclusive OR) of 2 numbers. Learn more.

/* 6.7 Byte-position text functions */
/* 6.8 Complex Number Functions */
/* 6.9 Database Functions */

/* 6.10 Date and Time Functions */
    OP( OP_DATE,       "@date", 1, 2)
    //OP( OP_DATEDIF,    "@datedif", 3, 3)
    //OP( OP_DATEVALUE,  "@datevalue", 1, 1)
    OP( OP_DAY,        "@day", 1, 1)
    //OP( OP_DAYS,       "@days", 2, 2)
    //OP( OP_DAYS360,    "@days360", 2, 3)
    //OP( OP_EDATE,      "@odate", 2, 2)
    //OP( OP_EOMONTH,    "@oemonth", 2, 2)
    OP( OP_HOUR,       "@hour", 1, 1)
    //OP( OP_ISOWEEKNUM, "@isoweeknum", 1, 1)
    OP( OP_MINUTE,     "@minute", 1, 1)
    OP( OP_MONTH,      "@month", 1, 1)
    //OP( OP_NETWORKDAYS, "@networkdays", 2, 4)
    OP( OP_NOW,        "@now", -1, -1)
    OP( OP_SECOND,     "@second", 1, 1)
    //OP( OP_TIME,       "@time", 3, 3)
    //OP( OP_TIMEVALUE,  "@timevalue", 1, 1)
    //OP( OP_TODAY,      "@today", 0, 0)
    //OP( OP_WEEKDAY,    "@weekday", 1, 2)
    //OP( OP_WEEKNUM,    "@weeknum", 1, 2)
    //OP( OP_WORKDAY,    "@workday", 2, 4)
    OP( OP_YEAR,       "@year", 1, 1)
    OP( OP_YEARFRAC,   "@yearfrac", 2, 3)
    OP( OP_DTS,        "@dts", 3, 3)        /* SC specific */
    OP( OP_TTS,        "@tts", 3, 3)        /* SC specific */

/* 6.11 External Access Functions */
    //OP( OP_DDE,        "@dde", 3, 4)
    OP( OP_EXT,        "@ext", 2, 2)        /* SC specific */ // XXX: should be 1,-1
    //OP( OP_HYPERLINK,  "@hyperlink", 1, 2)

/* 6.12 Financial Functions */
    OP( OP_FV,         "@fv", 3, 3)
    OP( OP_PMT,        "@pmt", 3, 3)
    OP( OP_PV,         "@pv", 3, 3)

/* 6.13 Information Functions */
    //OP( OP_AREAS,      "@areas", 1, 1)
    //OP( OP_CELL,       "@cell", 1, 2)
    //OP( OP_COLUMN,     "@column", 1, 1)
    //OP( OP_COLUMNS,    "@columns", 1, 1)
    OP( OP_COUNT,      "@count", 1, -1)
    //OP( OP_COUNTA,     "@counta", 1, -1)
    //OP( OP_COUNTBLANK, "@countblank", 1, -1)
    //OP( OP_COUNTIF,    "@countif", 2, 2)
    //OP( OP_COUNTIFS,   "@countifs", 2, -1)
    //OP( OP_ERROR_TYPE, "@error.type", 1, 1)
    //OP( OP_FORMULA,    "@counta", 1, 1)
    //OP( OP_ISBLANK,    "@isblank", 1, 1)
    //OP( OP_ISERR,      "@isblank", 1, 1)
    //OP( OP_ISERROR,    "@isblank", 1, 1)
    //OP( OP_ISEVEN,     "@iseven", 1, 1)
    //OP( OP_ISFORMULA,  "@isformula", 1, 1)
    //OP( OP_ISLOGICAL,  "@islogical", 1, 1)
    //OP( OP_ISNA,       "@isna", 1, 1)
    //OP( OP_ISODD,      "@isodd", 1, 1)
    //OP( OP_ISREF,      "@isref", 1, 1)
    //OP( OP_ISTEXT,     "@istext", 1, 1)
    //OP( OP_N,          "@n", 1, 1)
    //OP( OP_NA,         "@na", 0, 0)
    //OP( OP_NUMBERVALUE, "@numbervalue", 1, 3)
    //OP( OP_ROW,        "@row", 1, 1)
    //OP( OP_ROWS,       "@rows", 1, 1)
    //OP( OP_SHEET,      "@sheet", 1, 1)
    //OP( OP_SHEETS,     "@sheets", 1, 1)
    //OP( OP_TYPE,       "@type", 1, 1)
    //OP( OP_VALUE,      "@value", 1, 1)
    OP( OP_ROWS,       "@rows", 1, 1)       /* SC specific */
    OP( OP_COLS,       "@cols", 1, 1)       /* SC specific */
    OP( OP_MYROW,      "@myrow", -1, -1)    /* SC specific */
    OP( OP_MYCOL,      "@mycol", -1, -1)    /* SC specific */
    OP( OP_LASTROW,    "@lastrow", -1, -1)  /* SC specific */
    OP( OP_LASTCOL,    "@lastcol", -1, -1)  /* SC specific */
    OP( OP_FILENAME,   "@filename", 1, 1)   /* SC specific */
    OP( OP_COLTOA,     "@coltoa", 1, 1)     /* SC specific */
    OP( OP_NVAL,       "@nval", 2, 2)       /* SC specific */
    OP( OP_SVAL,       "@sval", 2, 2)       /* SC specific */
    OP( OP_STON,       "@ston", 1, 1)       /* SC specific */
    OP( OP_NUMITER,    "@numiter", -1, -1)  /* SC specific */
    OP( OP_ERR,        "@err", -1, -1)      /* SC specific */

/* 6.14 Lookup Functions */
    //OP( OP_ADDRESS,    "@address", 2, 5)
    //OP( OP_CHOOSE,     "@choose", 1, 2)
    //OP( OP_GETPIVOTDATA, "@getpivotdata", 2, 4)
    OP( OP_HLOOKUP,    "@hlookup", 3, 3) /* 3, 4 */
    OP( OP_INDEX,      "@index", 2, 3) /* 1, 4 */
    //OP( OP_INDIRECT,   "@index", 1, 2)
    OP( OP_LOOKUP,     "@lookup", 2, 2) /* 2, 3 */
    //OP( OP_MATCH,      "@match", 2, 3)
    //OP( OP_MULTIPLE_OPERATIONS, "@multiple.operations", 3, 5)
    //OP( OP_OFFSET,     "@offset", 3, 5)
    OP( OP_STINDEX,    "@stindex", 2, 3)    /* SC specific */
    OP( OP_VLOOKUP,    "@vlookup", 3, 3) /* 3, 4 */

/* 6.15 Logical Functions */
    //OP( OP_AND,        "@and", 1, -1)
    //OP( OP_FALSE,      "@false", 0, 0)
    OP( OP_IF,         "@if", 3, 3) /* 1, 3 */
    //OP( OP_IFERROR,    "@iferror", 2, 2)
    //OP( OP_IFNA,       "@ifna", 2, 2)
    //OP( OP_NOT,        "@not", 1, 1)
    //OP( OP_OR,         "@or", 1, -1)
    //OP( OP_TRUE,       "@true", 0, 0)
    //OP( OP_XOR,        "@xor", 1, -1)

/* 6.16 Mathematical Functions */
    OP( OP_ABS,        "@abs", 1, 1)
    OP( OP_ACOS,       "@acos", 1, 1)
    //OP( OP_ACOSH,      "@acosh", 1, 1)
    //OP( OP_ACOT,       "@acot", 1, 1)
    //OP( OP_ACOTH,      "@acoth", 1, 1)
    OP( OP_ASIN,       "@asin", 1, 1)
    //OP( OP_ASINH,      "@asinh", 1, 1)
    OP( OP_ATAN,       "@atan", 1, 1)
    OP( OP_ATAN2,      "@atan2", 2, 2)
    //OP( OP_ATANH,      "@atanh", 1, 1)
    //OP( OP_BESSELI,    "@besseli", 2, 2)
    //OP( OP_BESSELJ,    "@besselj", 2, 2)
    //OP( OP_BESSELK,    "@besselk", 2, 2)
    //OP( OP_BESSELY,    "@bessely", 2, 2)
    //OP( OP_COMBIN,     "@combin", 2, 2)
    //OP( OP_COMBINA,    "@combina", 2, 2)
    //OP( OP_CONVERT,    "@convert", 3, 3)
    OP( OP_COS,        "@cos", 1, 1)
    //OP( OP_COSH,       "@cosh", 1, 1)
    //OP( OP_COT,        "@cot", 1, 1)
    //OP( OP_COTH,       "@coth", 1, 1)
    //OP( OP_CSC,        "@csc", 1, 1)
    //OP( OP_CSCH,       "@csch", 1, 1)
    //OP( OP_DEGREES,      "@degrees", 1, 1)
    //OP( OP_DELTA,        "@delta", 1, 2)
    //OP( OP_ERF,        "@erf", 1, 2)
    //OP( OP_ERFC,       "@erfc", 1, 1)
    //OP( OP_EUROCONVERT, "@euroconvert", 3, 5)
    //OP( OP_EVEN,       "@even", 1, 1)
    OP( OP_EXP,        "@exp", 1, 1)
    //OP( OP_FACT,       "@fact", 1, 1)
    //OP( OP_FACTDOUBLE, "@factdouble", 1, 1)
    //OP( OP_GAMMA,      "@gamma", 1, 1)
    //OP( OP_GAMMALN,    "@gammaln", 1, 1)
    //OP( OP_GCD,        "@gcd", 1, -1)
    //OP( OP_GESTEP,     "@gestep", 2, 2)
    //OP( OP_LCM,        "@lcm", 1, -1)
    //OP( OP_LN,         "@ln", 1, 1)
    OP( OP_LOG,        "@log", 1, 1) /* 1, 2 */
    OP( OP_LOG10,      "@log10", 1, 1)
    //OP( OP_MOD,        "@mod", 2, 2)
    //OP( OP_MULTINOMIAL, "@multinomial", 1, -1)
    //OP( OP_ODD,        "@odd", 1, 1)
    OP( OP_PI,         "@pi", -1, -1)
    //OP( OP_POWER,      "@power", 2, 2)
    //OP( OP_PRODUCT,    "@product", 1, -1)
    //OP( OP_QUOTIENT,   "@quotient", 2, 2)
    //OP( OP_RADIANS,    "@radians", 1, 1)
    OP( OP_RAND,       "@rand", 0, 0)
    OP( OP_RANDBETWEEN, "@randbetween", 2, 2)
    //OP( OP_SEC,        "@sec", 1, 1)
    //OP( OP_SECH,       "@sech", 1, 1)
    //OP( OP_SERIESSUM,  "@seriessum", 4, 4)
    //OP( OP_SIGN,       "@sign", 1, 1)
    OP( OP_SIN,        "@sin", 1, 1)
    //OP( OP_SINH,       "@sinh", 1, 1)
    OP( OP_SQRT,       "@sqrt", 1, 1)
    //OP( OP_SQRTPI,     "@sqrtpi", 1, 1)
    //OP( OP_SUBTOTAL,   "@subtotal", 1, -1)
    OP( OP_SUM,        "@sum", 1, -1)
    //OP( OP_SUMIF,      "@sumif", 2, -1)
    //OP( OP_SUMIFS,     "@sumifs", 3, -1)
    //OP( OP_SUMPRODUCT, "@sumproduct", 1, -1)
    //OP( OP_SUMSQ,      "@sumsq", 1, -1)
    //OP( OP_SUMX2MY2,   "@sumx2my2", 2, 2)
    //OP( OP_SUMX2PY2,   "@sumx2py2", 2, 2)
    //OP( OP_SUMXMY2,    "@sumxmy2", 2, 2)
    OP( OP_TAN,        "@tan", 1, 1)
    //OP( OP_TANH,       "@tanh", 1, 1)

    OP( OP_FABS,       "@fabs", 1, 1)       /* SC specific */
    OP( OP_HYPOT,      "@hypot", 2, 2)      /* SC specific */
    OP( OP_DTR,        "@dtr", 1, 1)        /* SC specific */
    OP( OP_RTD,        "@rtd", 1, 1)        /* SC specific */
    OP( OP_POW,        "@pow", 2, 2)        /* SC specific */
    OP( OP_PROD,       "@prod", 1, -1)      /* SC specific */

/* 6.17 Rounding Functions */
    //OP( OP_CEILING,    "@ceiling", 1, 3)
    //OP( OP_INT,        "@int", 1, 1)
    OP( OP_FLOOR,      "@floor", 1, 1) /* 1, 3 */
    //OP( OP_MROUND,     "@mround", 2, 2)
    OP( OP_ROUND,      "@round", 2, 2)
    //OP( OP_ROUNDDOWN,  "@rounddown", 2, 2)
    //OP( OP_ROUNDUP,    "@roundup", 2, 2)
    //OP( OP_TRUNC,      "@trunc", 2, 2)

    OP( OP_CEIL,       "@ceil", 1, 1)       /* SC specific */
    OP( OP_RND,        "@rnd", 1, 1)        /* SC specific */

/* 6.18 Statistical Functions */
    //OP( OP_AVEDEV,     "@avedev", 1, -1)
    //OP( OP_AVERAGE,    "@average", 1, -1)
    //OP( OP_AVERAGEA,   "@averagea", 1, -1)
    //OP( OP_AVERAGEIF,  "@averageif", 2, 3)
    //OP( OP_AVERAGEIFS, "@averageifs", 3, -1)
    //OP( OP_LARGE,      "@large", 1, -1)
    OP( OP_MAX,        "@max", 1, -1)
    //OP( OP_MAXA,       "@maxa", 1, -1)
    //OP( OP_MEDIAN,     "@median", 1, -1)
    OP( OP_MIN,        "@min", 1, -1)
    //OP( OP_MINA,       "@mina", 1, -1)
    //OP( OP_MODE,       "@mode", 1, -1)
    //OP( OP_PERMUT,     "@permut", 2, 2)
    //OP( OP_PERMUTATIONA, "@permutationa", 2, 2)
    //OP( OP_RANK,       "@rank", 1, -1)
    //OP( OP_SMALL,      "@small", 1, -1)
    //OP( OP_STDEV,      "@stdev", 1, -1)
    //OP( OP_STDEVA,     "@stdeva", 1, -1)
    //OP( OP_STDEVP,     "@stdevp", 1, -1)
    //OP( OP_STDEVPA,    "@stdevpa", 1, -1)
    //OP( OP_VAR,        "@var", 1, -1)
    //OP( OP_VARA,       "@vara", 1, -1)
    //OP( OP_VARP,       "@varp", 1, -1)
    //OP( OP_VARPA,      "@varpa", 1, -1)

    OP( OP_AVG,        "@avg", 1, -1)       /* SC specific */
    OP( OP_LMAX,       "@max", 1, -1)       /* SC specific */
    OP( OP_LMIN,       "@min", 1, -1)       /* SC specific */
    OP( OP_STDDEV,     "@stddev", 1, -1)    /* SC specific */

/* 6.19 Number Representation Conversion Functions */
    //OP( OP_ARABIC,     "@arabic", 1, 1)
    //OP( OP_BASE,       "@base", 2, 3)
    //OP( OP_BIN2DEC,    "@bin2dec", 1, 1)
    //OP( OP_BIN2HEX,    "@bin2hex", 1, 2)
    //OP( OP_BIN2OCT,    "@bin2oct", 1, 2)
    //OP( OP_DEC2BIN,    "@dec2bin", 1, 2)
    //OP( OP_DEC2HEX,    "@dec2hex", 1, 2)
    //OP( OP_DEC2OCT,    "@dec2oct", 1, 2)
    //OP( OP_DECIMAL,    "@decimal", 2, 2)
    //OP( OP_HEX2BIN,    "@hex2bin", 1, 2)
    //OP( OP_HEX2DEC,    "@hex2dec", 1, 1)
    //OP( OP_HEX2OCT,    "@hex2oct", 1, 2)
    //OP( OP_OCT2BIN,    "@oct2bin", 1, 2)
    //OP( OP_OCT2DEC,    "@oct2dec", 1, 1)
    //OP( OP_OCT2HEX,    "@oct2hex", 1, 2)
    //OP( OP_ROMAN,      "@roman", 1, 2)
    OP( OP_FMT,        "@fmt", 2, 2)        /* SC specific */

/* 6.20 Text Functions */
    //OP( OP_ASC,        "@asc", 1, 1)
    //OP( OP_CHAR,       "@char", 1, 1)
    //OP( OP_CLEAN,      "@clean", 1, 1)
    //OP( OP_CODE,       "@code", 1, 1)
    //OP( OP_CONCATENATE, "@CONCATENATE", 1, -1)
    //OP( OP_DOLLAR,        "@dollar", 1, 2)
    //OP( OP_EXACT,        "@exact", 2, 2)
    //OP( OP_FIND,      "@find", 2, 3)
    //OP( OP_FIXED,      "@fixed", 1, 3)
    //OP( OP_JIS,        "@jis", 1, 1)
    //OP( OP_LEFT,       "@left", 1, 2)
    //OP( OP_LEN,        "@len", 1, 1)
    OP( OP_LOWER,      "@lower", 1, 1)
    //OP( OP_MID,        "@mid", 3, 3)
    //OP( OP_PROPER,     "@proper", 1, 1)
    //OP( OP_REPLACE,    "@replace", 4, 4)
    //OP( OP_REPT,       "@rept", 2, 2)
    //OP( OP_RIGHT,      "@right", 1, 2)
    //OP( OP_SEARCH,     "@search", 2, 3)
    //OP( OP_SUBSTITUTE, "@substitute", 3, 4)
    //OP( OP_T,          "@t", 1, 1)
    //OP( OP_TEXT,       "@text", 2, 2)
    //OP( OP_TRIM,       "@trim", 1, 1)
    //OP( OP_UNICHAR,    "@unichar", 1, 1)
    //OP( OP_UNICODE,    "@unicode", 1, 1)
    OP( OP_UPPER,      "@upper", 1, 1)

    OP( OP_SUBSTR,     "@substr", 3, 3)     /* SC specific */
    OP( OP_CAPITAL,    "@capital", 1, 1)    /* SC specific */
    OP( OP_EQS,        "@eqs", 2, 2)        /* SC specific */

/* SC specific functions */

    OP( OP_BLACK,      "@black", -1, -1)    /* SC specific */
    OP( OP_BLUE,       "@blue", -1, -1)     /* SC specific */
    OP( OP_CYAN,       "@cyan", -1, -1)     /* SC specific */
    OP( OP_GREEN,      "@green", -1, -1)    /* SC specific */
    OP( OP_MAGENTA,    "@magenta", -1, -1)  /* SC specific */
    OP( OP_RED,        "@red", -1, -1)      /* SC specific */
    OP( OP_WHITE,      "@white", -1, -1)    /* SC specific */
    OP( OP_YELLOW,     "@yellow", -1, -1)   /* SC specific */
