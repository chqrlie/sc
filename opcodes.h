/*      SC      A Spreadsheet Calculator
 *              Expression node definitions.
 *
 *              original by Charlie Gordon: August, 2021
 *              $Revision: 8.1 $
 */

#ifndef __
#define __(op,str,min,max,efun,arg)
#endif
#ifndef LO  /* Libre Office specific */
#define LO(op,str,min,max,efun,arg)
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
__( OP_ADD,         "ADD(value1, value2)", 2, 2, NULL, NULL) // Returns the sum of two numbers. Equivalent to the `+` operator
__( OP_CONCAT,      "CONCAT(value1, value2)", 2, 2, NULL, NULL) // Returns the concatenation of two values. Equivalent to the `&` operator
__( OP_DIVIDE,      "DIVIDE(dividend, divisor)", 2, 2, NULL, NULL) // Returns one number divided by another. Equivalent to the `/` operator
__( OP_EQ,          "EQ(value1, value2)", 2, 2, NULL, NULL) // Returns `TRUE` if two specified values are equal and `FALSE` otherwise. Equivalent to the `=` operator
__( OP_GT,          "GT(value1, value2)", 2, 2, NULL, NULL) // Returns `TRUE` if the first argument is strictly greater than the second, and `FALSE` otherwise. Equivalent to the `>` operator
__( OP_GTE,         "GTE(value1, value2)", 2, 2, NULL, NULL) // Returns `TRUE` if the first argument is greater than or equal to the second, and `FALSE` otherwise. Equivalent to the `>=` operator
__( OP_ISBETWEEN,   "ISBETWEEN(value_to_compare, lower_value, upper_value, lower_value_is_inclusive, upper_value_is_inclusive)", 5, 5, NULL, NULL) // Checks whether a provided number is between two other numbers either inclusively or exclusively
__( OP_LT,          "LT(value1, value2)", 2, 2, NULL, NULL) // Returns `TRUE` if the first argument is strictly less than the second, and `FALSE` otherwise. Equivalent to the `<` operator
__( OP_LTE,         "LTE(value1, value2)", 2, 2, NULL, NULL) // Returns `TRUE` if the first argument is less than or equal to the second, and `FALSE` otherwise. Equivalent to the `<=` operator
__( OP_MINUS,       "MINUS(value1, value2)", 2, 2, NULL, NULL) // Returns the difference of two numbers. Equivalent to the `-` operator
__( OP_MULTIPLY,    "MULTIPLY(factor1, factor2)", 2, 2, NULL, NULL) // Returns the product of two numbers. Equivalent to the `*` operator
__( OP_NE,          "NE(value1, value2)", 2, 2, NULL, NULL) // Returns `TRUE` if two specified values are not equal and `FALSE` otherwise. Equivalent to the `<>` operator
__( OP_POW,         "POW(base, exponent)", 2, 2, NULL, NULL) // Returns a number raised to a power
__( OP_UMINUS,      "UMINUS(value)", 1, 1, NULL, NULL) // Returns a number with the sign reversed
__( OP_UNARY_PERCENT, "UNARY_PERCENT(percentage)", 1, 1, NULL, NULL) // Returns a value interpreted as a percentage; that is, `UNARY_PERCENT(100)` equals `1`
__( OP_UNIQUE,      "UNIQUE(range, by_column, exactly_once)", 3, 3, NULL, NULL) // Returns unique rows in the provided source range, discarding duplicates. Rows are returned in the order in which they first appear in the source range
__( OP_UPLUS,       "UPLUS(value)", 1, 1, NULL, NULL) // Returns a specified number, unchanged

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
__( OP_MDETERM,     "MDETERM(matrix)", 1, 1, NULL, NULL) // Calculates the determinant of a matrix.
__( OP_MINVERSE,    "MINVERSE(matrix)", 1, 1, NULL, NULL) // Returns the inverse of a matrix.
__( OP_MMULT,       "MMULT(matrix, matrix)", 2, 2, NULL, NULL) // Multiplies two matrices
__( OP_MUNIT,       "MUNIT(dimension)", 1, 1, NULL, NULL) // Returns a unit matrix of a specified dimension.
LO( OP_RANDARRAY,   "RANDARRAY(rows, columns)", 2, 2, NULL, NULL) // Generates an array of random numbers between 0 and 1.
LO( OP_SEQUENCE,    "SEQUENCE(rows, columns, start, step)", 4, 4, NULL, NULL) // Returns an array of sequential numbers, such as 1, 2, 3, 4.
__( OP_TRANSPOSE,   "TRANSPOSE(matrix)", 1, 1, NULL, NULL) // Returns the transpose of a matrix.

/* 6.6 Bit operand functions */
OP( OP_BITAND,      "BITAND(value1, value2)", 2, 2, eval_fl2, bitand) // Bitwise boolean AND of two numbers.
OP( OP_BITLSHIFT,   "BITLSHIFT(value, shift_amount)", 2, 2, eval_fl2, bitlshift) // Shifts the bits of the input a certain number of places to the left.
OP( OP_BITOR,       "BITOR(value1, value2)", 2, 2, eval_fl2, bitor) // Bitwise boolean OR of 2 numbers.
OP( OP_BITRSHIFT,   "BITRSHIFT(value, shift_amount)", 2, 2, eval_fl2, bitrshift) // Shifts the bits of the input a certain number of places to the right.
OP( OP_BITXOR,      "BITXOR(value1, value2)", 2, 2, eval_fl2, bitxor) // Bitwise XOR (exclusive OR) of 2 numbers.

/* 6.7 Byte-position text functions */
__( OP_FINDB,       "FINDB(search_for, text_to_search, [starting_at])", 2, 3, NULL, NULL) // Returns the position at which a string is first found within text counting each double-character as 2
__( OP_LEFTB,       "LEFTB(string, num_of_bytes)", 1, 1, NULL, NULL) // Returns the left portion of a string up to a certain number of bytes.
__( OP_LENB,        "LENB(string)", 1, 1, NULL, NULL) // Returns the length of a string in bytes.
__( OP_MIDB,        "MIDB(string)", 1, 1, NULL, NULL) // Returns a section of a string starting at a given character and up to a specified number of bytes.
__( OP_REPLACEB,    "REPLACEB(text, position, num_bytes, new_text)", 3, 3, NULL, NULL) // Replaces part of a text string, based on a number of bytes, with a different text string.
__( OP_RIGHTB,      "RIGHTB(string, num_of_bytes)", 2, 2, NULL, NULL) // Returns the right portion of a string up to a certain number of bytes.
__( OP_SEARCHB,     "SEARCHB(search_for, text_to_search, [starting_at])", 2, 3, NULL, NULL) // Returns the position at which a string is first found within text counting each double-character as 2

/* 6.8 Complex Number Functions */
__( OP_COMPLEX,     "COMPLEX(real_part, imaginary_part, [suffix])", 2, 3, NULL, NULL) // Creates a complex number given real and imaginary coefficients
__( OP_IMABS,       "IMABS(number)", 1, 1, NULL, NULL) // Returns absolute value of a complex number
__( OP_IMAGINARY,   "IMAGINARY(complex_number)", 1, 1, NULL, NULL) // Returns the imaginary coefficient of a complex number
__( OP_IMARGUMENT,  "IMARGUMENT(number)", 1, 1, NULL, NULL) // The IMARGUMENT function returns the angle (also known as the argument or \theta) of the given complex number in radians.
__( OP_IMCONJUGATE, "IMCONJUGATE(number)", 1, 1, NULL, NULL) // Returns the complex conjugate of a number
__( OP_IMCOS,       "IMCOS(number)", 1, 1, NULL, NULL) // The IMCOS function returns the cosine of the given complex number.
__( OP_IMCOSH,      "IMCOSH(number)", 1, 1, NULL, NULL) // Returns the hyperbolic cosine of the given complex number. For example, a given complex number 'x+yi' returns 'cosh(x+yi)'.
__( OP_IMCOT,       "IMCOT(number)", 1, 1, NULL, NULL) // Returns the cotangent of the given complex number. For example, a given complex number 'x+yi' returns 'cot(x+yi)'.
__( OP_IMCOTH,      "IMCOTH(number)", 1, 1, NULL, NULL) // Returns the hyperbolic cotangent of the given complex number. For example, a given complex number 'x+yi' returns 'coth(x+yi)'.
__( OP_IMCSC,       "IMCSC(number)", 1, 1, NULL, NULL) // Returns the cosecant of the given complex number.
__( OP_IMCSCH,      "IMCSCH(number)", 1, 1, NULL, NULL) // Returns the hyperbolic cosecant of the given complex number. For example, a given complex number 'x+yi' returns 'csch(x+yi)'.
__( OP_IMDIV,       "IMDIV(dividend, divisor)", 2, 2, NULL, NULL) // Returns one complex number divided by another
__( OP_IMEXP,       "IMEXP(exponent)", 1, 1, NULL, NULL) // Returns Euler's number, e (~2.718) raised to a complex power.
__( OP_IMLN,        "IMLN(complex_value)", 1, 1, NULL, NULL) // Returns the logarithm of a complex number, base e (Euler's number)
__( OP_IMLOG,       "IMLOG(value, base)", 2, 2, NULL, NULL) // Returns the logarithm of a complex number for a specified base.
__( OP_IMLOG10,     "IMLOG10(value)", 1, 1, NULL, NULL) // Returns the logarithm of a complex number with base 10.
__( OP_IMLOG2,      "IMLOG2(value)", 1, 1, NULL, NULL) // Returns the logarithm of a complex number with base 2.
__( OP_IMPOWER,     "IMPOWER(complex_base, exponent)", 2, 2, NULL, NULL) // Returns a complex number raised to a power
__( OP_IMPRODUCT,   "IMPRODUCT(factor1, [factor2, ...])", 1, -1, NULL, NULL) // Returns the result of multiplying a series of complex numbers together
__( OP_IMREAL,      "IMREAL(complex_number)", 1, 1, NULL, NULL) // Returns the real coefficient of a complex number
__( OP_IMSEC,       "IMSEC(number)", 1, 1, NULL, NULL) // Returns the secant of the given complex number. For example, a given complex number 'x+yi' returns 'sec(x+yi)'.
__( OP_IMSECH,      "IMSECH(number)", 1, 1, NULL, NULL) // Returns the hyperbolic secant of the given complex number. For example, a given complex number 'x+yi' returns 'sech(x+yi)'.
__( OP_IMSIN,       "IMSIN (number)", 1, 1, NULL, NULL) // Returns the sine of the given complex number.
__( OP_IMSINH,      "IMSINH(number)", 1, 1, NULL, NULL) // Returns the hyperbolic sine of the given complex number. For example, a given complex number 'x+yi' returns 'sinh(x+yi)'.
__( OP_IMSQRT,      "IMSQRT(complex_number)", 1, 1, NULL, NULL) // Computes the square root of a complex number
__( OP_IMSUB,       "IMSUB(first_number, second_number)", 2, 2, NULL, NULL) // Returns the difference between two complex numbers
__( OP_IMSUM,       "IMSUM(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns the sum of a series of complex numbers
__( OP_IMTAN,       "IMTAN(number)", 1, 1, NULL, NULL) // Returns the tangent of the given complex number.
__( OP_IMTANH,      "IMTANH(number)", 1, 1, NULL, NULL) // Returns the hyperbolic tangent of the given complex number. For example, a given complex number 'x+yi' returns 'tanh(x+yi)'.

/* 6.9 Database Functions */
__( OP_DAVERAGE,    "DAVERAGE(database, field, criteria)", 3, 3, NULL, NULL) // Returns the average of a set of values selected from a database table-like array or range using a SQL-like query
__( OP_DCOUNT,      "DCOUNT(database, field, criteria)", 3, 3, NULL, NULL) // Counts numeric values selected from a database table-like array or range using a SQL-like query
__( OP_DCOUNTA,     "DCOUNTA(database, field, criteria)", 3, 3, NULL, NULL) // Counts values, including text, selected from a database table-like array or range using a SQL-like query
__( OP_DGET,        "DGET(database, field, criteria)", 3, 3, NULL, NULL) // Returns a single value from a database table-like array or range using a SQL-like query
__( OP_DMAX,        "DMAX(database, field, criteria)", 3, 3, NULL, NULL) // Returns the maximum value selected from a database table-like array or range using a SQL-like query
__( OP_DMIN,        "DMIN(database, field, criteria)", 3, 3, NULL, NULL) // Returns the minimum value selected from a database table-like array or range using a SQL-like query
__( OP_DPRODUCT,    "DPRODUCT(database, field, criteria)", 3, 3, NULL, NULL) // Returns the product of values selected from a database table-like array or range using a SQL-like query
__( OP_DSTDEV,      "DSTDEV(database, field, criteria)", 3, 3, NULL, NULL) // Returns the standard deviation of a population sample selected from a database table-like array or range using a SQL-like query
__( OP_DSTDEVP,     "DSTDEVP(database, field, criteria)", 3, 3, NULL, NULL) // Returns the standard deviation of an entire population selected from a database table-like array or range using a SQL-like query
__( OP_DSUM,        "DSUM(database, field, criteria)", 3, 3, NULL, NULL) // Returns the sum of values selected from a database table-like array or range using a SQL-like query
__( OP_DVAR,        "DVAR(database, field, criteria)", 3, 3, NULL, NULL) // Returns the variance of a population sample selected from a database table-like array or range using a SQL-like query
__( OP_DVARP,       "DVARP(database, field, criteria)", 3, 3, NULL, NULL) // Returns the variance of an entire population selected from a database table-like array or range using a SQL-like query

/* 6.10 Date and Time Functions */
__( OP_DATE,        "DATE(year, month, day)", 3, 3, eval_dts, NULL) // Converts a year, month, and day triplet into a date
XX( OP_DATE,        "DATE(date, [format])", 1, 2, eval_date, NULL) // Converts a date into a formated string
__( OP_DATEDIF,     "DATEDIF(start_date, end_date, unit)", 3, 3, NULL, NULL) // Calculates the number of days, months, or years between two dates
__( OP_DATEVALUE,   "DATEVALUE(date_string)", 1, 1, NULL, NULL) // Converts a provided date string in a known format to a date value
OP( OP_DAY,         "DAY(date)", 1, 1, eval_time, NULL) // Returns the day of the month that a specific date falls on, in numeric format
__( OP_DAYS,        "DAYS(end_date, start_date)", 2, 2, NULL, NULL) // Returns the number of days between two dates.
__( OP_DAYS360,     "DAYS360(start_date, end_date, [method])", 2, 3, NULL, NULL) // Returns the difference between two days based on the 360 day year used in some financial interest calculations
__( OP_EDATE,       "EDATE(start_date, months)", 2, 2, NULL, NULL) // Returns a date a specified number of months before or after another date
__( OP_EOMONTH,     "EOMONTH(start_date, months)", 2, 2, NULL, NULL) // Returns a date representing the last day of a month which falls a specified number of months before or after another date
OP( OP_HOUR,        "HOUR(time)", 1, 1, eval_time, NULL) // Returns the hour component of a specific time, in numeric format
__( OP_ISOWEEKNUM,  "ISOWEEKNUM(date)", 1, 1, NULL, NULL) // Returns the number of the ISO week of the year where the provided date falls
OP( OP_MINUTE,      "MINUTE(time)", 1, 1, eval_time, NULL) // Returns the minute component of a specific time, in numeric format
OP( OP_MONTH,       "MONTH(date)", 1, 1, eval_time, NULL) // Returns the month of the year a specific date falls in, in numeric format
__( OP_NETWORKDAYS, "NETWORKDAYS(start_date, end_date, [holidays])", 2, 3, NULL, NULL) // Returns the number of net working days between two provided days
__( OP_NETWORKDAYS_INTL, "NETWORKDAYS.INTL(start_date, end_date, [weekend], [holidays])", 2, 4, NULL, NULL) //  Returns the number of net working days between two provided days excluding specified weekend days and holidays
OP( OP_NOW,         "NOW()", -1, 0, eval_now, NULL) // Returns the current date and time as a date value
OP( OP_SECOND,      "SECOND(time)", 1, 1, eval_time, NULL) // Returns the second component of a specific time, in numeric format
__( OP_TIME,        "TIME(hour, minute, second)", 3, 3, NULL, NULL) // Converts a provided hour, minute, and second into a time
__( OP_TIMEVALUE,   "TIMEVALUE(time_string)", 1, 1, NULL, NULL) // Returns the fraction of a 24-hour day the time represents
__( OP_TODAY,       "TODAY()", -1, 0, NULL, NULL) // Returns the current date as a date value
__( OP_WEEKDAY,     "WEEKDAY(date, [type])", 1, 2, NULL, NULL) // Returns a number representing the day of the week of the date provided
__( OP_WEEKNUM,     "WEEKNUM(date, [type])", 1, 2, NULL, NULL) // Returns a number representing the week of the year where the provided date falls
__( OP_WORKDAY,     "WORKDAY(start_date, num_days, [holidays])", 2, 3, NULL, NULL) // Calculates the end date after a specified number of working days
__( OP_WORKDAY_INTL, "WORKDAY.INTL(start_date, num_days, [weekend], [holidays])", 2, 4, NULL, NULL) // Calculates the date after a specified number of workdays excluding specified weekend days and holidays
OP( OP_YEAR,        "YEAR(date)", 1, 1, eval_time, NULL) // Returns the year specified by a given date
__( OP_YEARFRAC,    "YEARFRAC(start_date, end_date, [day_count_convention])", 2, 3, NULL, NULL) // Returns the number of years, including fractional years, between two dates using a specified day count convention

XX( OP_DTS,         "@dts", 3, 3, eval_dts, NULL)
XX( OP_TTS,         "@tts", 3, 3, eval_fn3, dotts)

/* 6.11 External Access Functions */
__( OP_DDE,         "DDE(server, topic, item, [mode=0])", 3, 4, NULL, NULL) // Returns data from a DDE request
LO( OP_ENCODEURL,   "ENCODEURL(text)", 1, 1, NULL, NULL) // Encodes a string of text for the purpose of using in a URL query.
XX( OP_EXT,         "EXT(command, arg)", 2, 2, eval_ext, NULL) // Run an external process, return first line of output
__( OP_HYPERLINK,   "HYPERLINK(url, [link_label])", 1, 2, NULL, NULL) // Creates a hyperlink inside a cell
LO( OP_IMPORTDATA,  "IMPORTDATA(url)", 1, 1, NULL, NULL) // Imports data at a given url in .csv (comma-separated value) or .tsv (tab-separated value) format
LO( OP_IMPORTFEED,  "IMPORTFEED(url, [query], [headers], [num_items])", 1, 4, NULL, NULL) // Imports a RSS or ATOM feed
LO( OP_IMPORTHTML,  "IMPORTHTML(url, query, index)", 3, 3, NULL, NULL) // Imports data from a table or list within an HTML page
LO( OP_IMPORTRANGE, "IMPORTRANGE(spreadsheet_url, range_string)", 2, 2, NULL, NULL) // Imports a range of cells from a specified spreadsheet
LO( OP_IMPORTXML,   "IMPORTXML(url, xpath_query)", 2, 2, NULL, NULL) // Imports data from any of various structured data types including XML, HTML, CSV, TSV, and RSS and ATOM XML feeds
LO( OP_ISURL,       "ISURL(value)", 1, 1, NULL, NULL) // Checks whether a value is a valid URL

/* 6.12 Financial Functions */
__( OP_ACCRINT,     "ACCRINT(issue, first_payment, settlement, rate, redemption, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the accrued interest of a security that has periodic payments
__( OP_ACCRINTM,    "ACCRINTM(issue, maturity, rate, [redemption], [day_count_convention])", 1, 1, NULL, NULL) // Calculates the accrued interest of a security that pays interest at maturity
__( OP_AMORLINC,    "AMORLINC(cost, purchase_date, first_period_end, salvage, period, rate, [basis])", 1, 1, NULL, NULL) // Returns the depreciation for an accounting period, or the prorated depreciation if the asset was purchased in the middle of a period.
__( OP_COUPDAYBS,   "COUPDAYBS(settlement, maturity, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the number of days from the first coupon, or interest payment, until settlement
__( OP_COUPDAYS,    "COUPDAYS(settlement, maturity, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the number of days in the coupon, or interest payment, period that contains the specified settlement date
__( OP_COUPDAYSNC,  "COUPDAYSNC(settlement, maturity, frequency, [day_count_convention])", 1, 1, NULL, NULL) //  Calculates the number of days from the settlement date until the next coupon, or interest payment
__( OP_COUPNCD,     "COUPNCD(settlement, maturity, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates next coupon, or interest payment, date after the settlement date
__( OP_COUPNUM,     "COUPNUM(settlement, maturity, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the number of coupons, or interest payments, between the settlement date and the maturity date of the investment
__( OP_COUPPCD,     "COUPPCD(settlement, maturity, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates last coupon, or interest payment, date before the settlement date
__( OP_CUMIPMT,     "CUMIPMT(rate, number_of_periods, present_value, first_period, last_period, end_or_beginning)", 1, 1, NULL, NULL) // Calculates the cumulative interest over a range of payment periods for an investment based on constant-amount periodic payments and a constant interest rate
__( OP_CUMPRINC,    "CUMPRINC(rate, number_of_periods, present_value, first_period, last_period, end_or_beginning)", 1, 1, NULL, NULL) // Calculates the cumulative principal paid over a range of payment periods for an investment based on constant-amount periodic payments and a constant interest rate
__( OP_DB,          "DB(cost, salvage, life, period, [month])", 1, 1, NULL, NULL) // Calculates the depreciation of an asset for a specified period using the arithmetic declining balance method
__( OP_DDB,         "DDB(cost, salvage, life, period, [factor])", 1, 1, NULL, NULL) // Calculates the depreciation of an asset for a specified period using the double-declining balance method
__( OP_DISC,        "DISC(settlement, maturity, price, redemption, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the discount rate of a security based on price
__( OP_DOLLARDE,    "DOLLARDE(fractional_price, unit)", 1, 1, NULL, NULL) // Converts a price quotation given as a decimal fraction into a decimal value
__( OP_DOLLARFR,    "DOLLARFR(decimal_price, unit)", 1, 1, NULL, NULL) // Converts a price quotation given as a decimal value into a decimal fraction
__( OP_DURATION,    "DURATION(settlement, maturity, rate, yield, frequency, [day_count_convention])", 1, 1, NULL, NULL) // ,"Calculates the number of compounding periods required for an investment of a specified present value appreciating at a given rate to reach a target value
__( OP_EFFECT,      "EFFECT(nominal_rate, periods_per_year)", 1, 1, NULL, NULL) // Calculates the annual effective interest rate given the nominal rate and number of compounding periods per year
OP( OP_FV,          "FV(rate, number_of_periods, payment_amount, [present_value], [end_or_beginning])", 3, 3, eval_fn3, fin_fv) // Calculates the future value of an annuity investment based on constant-amount periodic payments and a constant interest rate
__( OP_FVSCHEDULE,  "FVSCHEDULE(principal, rate_schedule)", 1, 1, NULL, NULL) // Calculates the future value of some principal based on a specified series of potentially varying interest rates
__( OP_INTRATE,     "INTRATE(buy_date, sell_date, buy_price, sell_price, [day_count_convention])", 1, 1, NULL, NULL) //  Calculates the effective interest rate generated when an investment is purchased at one price and sold at another with no interest or dividends generated by the investment itself
__( OP_IPMT,        "IPMT(rate, period, number_of_periods, present_value, [future_value], [end_or_beginning])", 1, 1, NULL, NULL) // Calculates the payment on interest for an investment based on constant-amount periodic payments and a constant interest rate
__( OP_IRR,         "IRR(cashflow_amounts, [rate_guess])", 1, 1, NULL, NULL) // Calculates the internal rate of return on an investment based on a series of periodic cash flows
__( OP_ISPMT,       "ISPMT(rate, period, number_of_periods, present_value)", 1, 1, NULL, NULL) // The ISPMT function calculates the interest paid during a particular period of an investment.
__( OP_MDURATION,   "MDURATION(settlement, maturity, rate, yield, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the modified Macaulay duration of a security paying periodic interest, such as a US Treasury Bond, based on expected yield
__( OP_MIRR,        "MIRR(cashflow_amounts, financing_rate, reinvestment_return_rate)", 1, 1, NULL, NULL) // Calculates the modified internal rate of return on an investment based on a series of periodic cash flows and the difference between the interest rate paid on financing versus the return received on reinvested income
__( OP_NOMINAL,     "NOMINAL(effective_rate, periods_per_year)", 1, 1, NULL, NULL) // Calculates the annual nominal interest rate given the effective rate and number of compounding periods per year
__( OP_NPER,        "NPER(rate, payment_amount, present_value, [future_value], [end_or_beginning])", 1, 1, NULL, NULL) //  Calculates the number of payment periods for an investment based on constant-amount periodic payments and a constant interest rate
__( OP_NPV,         "NPV(discount, cashflow1, [cashflow2, ...])", 1, 1, NULL, NULL) // Calculates the net present value of an investment based on a series of periodic cash flows and a discount rate
__( OP_PDURATION,   "PDURATION(rate, present_value, future_value)", 1, 1, NULL, NULL) // Returns the number of periods for an investment to reach a specific value at a given rate.
OP( OP_PMT,         "PMT(rate, number_of_periods, present_value, [future_value], [end_or_beginning])", 3, 3, eval_fn3, fin_pmt) // Calculates the periodic payment for an annuity investment based on constant-amount periodic payments and a constant interest rate
__( OP_PPMT,        "PPMT(rate, period, number_of_periods, present_value, [future_value], [end_or_beginning])", 1, 1, NULL, NULL) // Calculates the payment on the principal of an investment based on constant-amount periodic payments and a constant interest rate
__( OP_PRICE,       "PRICE(settlement, maturity, rate, yield, redemption, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the price of a security paying periodic interest, such as a US Treasury Bond, based on expected yield
__( OP_PRICEDISC,   "PRICEDISC(settlement, maturity, discount, redemption, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the price of a discount (non-interest-bearing) security, based on expected yield
__( OP_PRICEMAT,    "PRICEMAT(settlement, maturity, issue, rate, yield, [day_count_convention])", 1, 1, NULL, NULL) //  Calculates the price of a security paying interest at maturity, based on expected yield
OP( OP_PV,          "PV(rate, number_of_periods, payment_amount, [future_value], [end_or_beginning])", 3, 3, eval_fn3, fin_pv) // Calculates the present value of an annuity investment based on constant-amount periodic payments and a constant interest rate
__( OP_RATE,        "RATE(number_of_periods, payment_per_period, present_value, [future_value], [end_or_beginning], [rate_guess])", 1, 1, NULL, NULL) // Calculates the interest rate of an annuity investment based on constant-amount periodic payments and the assumption of a constant interest rate
__( OP_RECEIVED,    "RECEIVED(settlement, maturity, investment, discount, [day_count_convention])", 1, 1, NULL, NULL) //  Calculates the amount received at maturity for an investment in fixed-income securities purchased on a given date
__( OP_RRI,         "RRI(number_of_periods, present_value, future_value)", 1, 1, NULL, NULL) // Returns the interest rate needed for an investment to reach a specific value within a given number of periods.
__( OP_SLN,         "SLN(cost, salvage, life)", 1, 1, NULL, NULL) // Calculates the depreciation of an asset for one period using the straight-line method
__( OP_SYD,         "SYD(cost, salvage, life, period)", 1, 1, NULL, NULL) // Calculates the depreciation of an asset for a specified period using the sum of years digits method
__( OP_TBILLEQ,     "TBILLEQ(settlement, maturity, discount)", 1, 1, NULL, NULL) // Calculates the equivalent annualized rate of return of a US Treasury Bill based on discount rate
__( OP_TBILLPRICE,  "TBILLPRICE(settlement, maturity, discount)", 1, 1, NULL, NULL) // Calculates the price of a US Treasury Bill based on discount rate
__( OP_TBILLYIELD,  "TBILLYIELD(settlement, maturity, price)", 1, 1, NULL, NULL) // Calculates the yield of a US Treasury Bill based on price
__( OP_VDB,         "VDB(cost, salvage, life, start_period, end_period, [factor], [no_switch])", 1, 1, NULL, NULL) // Returns the depreciation of an asset for a particular period (or partial period).
__( OP_XIRR,        "XIRR(cashflow_amounts, cashflow_dates, [rate_guess])", 1, 1, NULL, NULL) // Calculates the internal rate of return of an investment based on a specified series of potentially irregularly spaced cash flows
__( OP_XNPV,        "XNPV(discount, cashflow_amounts, cashflow_dates)", 1, 1, NULL, NULL) // Calculates the net present value of an investment based on a specified series of potentially irregularly spaced cash flows and a discount rate
__( OP_YIELD,       "YIELD(settlement, maturity, rate, price, redemption, frequency, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the annual yield of a security paying periodic interest, such as a US Treasury Bond, based on price
__( OP_YIELDDISC,   "YIELDDISC(settlement, maturity, price, redemption, [day_count_convention])", 1, 1, NULL, NULL) //  Calculates the annual yield of a discount (non-interest-bearing) security, based on price
__( OP_YIELDMAT,    "YIELDMAT(settlement, maturity, issue, rate, price, [day_count_convention])", 1, 1, NULL, NULL) // Calculates the annual yield of a security paying interest at maturity, based on price

/* 6.13 Information Functions */
__( OP_AREAS,       "AREAS(reference list)", 1, 1, NULL, NULL) // Returns the number of areas in a given list of references.
__( OP_CELL,        "CELL(info_type, [reference])", 1, 2, NULL, NULL) // Returns the requested information about the specified cell
__( OP_COLUMN,      "COLUMN([cell_reference])", 0, 1, NULL, NULL) // Returns the column number of a specified cell, with `A=1`
__( OP_COLUMNS,     "COLUMNS(range)", 1, 1, NULL, NULL) // Returns the number of columns in a specified array or range
OP( OP_COUNT,       "COUNT(value1, [value2, ...])", 1, -1, eval_rangeop, NULL) // Returns a count of the number of numeric values in a dataset
__( OP_COUNTA,      "COUNTA(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns a count of the number of values in a dataset
__( OP_COUNTBLANK,  "COUNTBLANK(range)", 1, 1, NULL, NULL) // Returns the number of empty cells in a given range
__( OP_COUNTIF,     "COUNTIF(range, criterion)", 2, 2, NULL, NULL) // Returns a conditional count across a range
__( OP_COUNTIFS,    "COUNTIFS(criteria_range1, criterion1, [criteria_range2, criterion2, ...])", 2, -1, NULL, NULL) // Returns the count of a range depending on multiple criteria
LO( OP_COUNTUNIQUE, "COUNTUNIQUE(value1, [value2, ...])", 1, -1, NULL, NULL) // Counts the number of unique values in a list of specified values and ranges
__( OP_ERROR_TYPE,  "ERROR.TYPE(reference)", 1, 1, NULL, NULL) // Returns a number corresponding to the error value in a different cell
__( OP_FORMULA,     "FORMULA(cell)", 1, 1, NULL, NULL) // Checks cell formula as text
LO( OP_FORMULATEXT, "FORMULATEXT(cell)", 1, 1, NULL, NULL) // Returns the formula as a string.
__( OP_INFO,        "INFO(category)", 1, 1, NULL, NULL) // Returns system information
__( OP_ISBLANK,     "ISBLANK(value)", 1, 1, NULL, NULL) // Checks whether the referenced cell is empty
LO( OP_ISDATE,      "ISDATE(value)", 1, 1, NULL, NULL) // Returns whether a value is a date.
LO( OP_ISEMAIL,     "ISEMAIL(value)", 1, 1, NULL, NULL) // Checks whether a value is a valid email address
__( OP_ISERR,       "ISERR(value)", 1, 1, NULL, NULL) // Checks whether a value is an error other than `#N/A`
__( OP_ISERROR,     "ISERROR(value)", 1, 1, NULL, NULL) // Checks whether a value is an error
__( OP_ISEVEN,      "ISEVEN(value)", 1, 1, NULL, NULL) // Checks whether the provided value is even
__( OP_ISFORMULA,   "ISFORMULA(cell)", 1, 1, NULL, NULL) // Checks whether a formula is in the referenced cell
__( OP_ISLOGICAL,   "ISLOGICAL(value)", 1, 1, NULL, NULL) // Checks whether a value is `TRUE` or `FALSE`
__( OP_ISNA,        "ISNA(value)", 1, 1, NULL, NULL) // Checks whether a value is the error `#N/A`
__( OP_ISNONTEXT,   "ISNONTEXT(value)", 1, 1, NULL, NULL) // Checks whether a value is non-textual
__( OP_ISNUMBER,    "ISNUMBER(value)", 1, 1, NULL, NULL) // Checks whether a value is a number
__( OP_ISODD,       "ISODD(value)", 1, 1, NULL, NULL) // Checks whether the provided value is odd
__( OP_ISREF,       "ISREF(value)", 1, 1, NULL, NULL) // Checks whether a value is a valid cell reference
__( OP_ISTEXT,      "ISTEXT(value)", 1, 1, NULL, NULL) // Checks whether a value is text
__( OP_N,           "N(value)", 1, 1, NULL, NULL) // Returns the argument provided as a number
__( OP_NA,          "NA()", -1, 0, NULL, NULL) // Returns the 'value not available' error, `#N/A`
__( OP_NUMBERVALUE, "NUMBERVALUE(text, [decimalseparator, [groupseparator]])", 1, 3, NULL, NULL) // Convert text to number, in a locale-independent way.
__( OP_ROW,         "ROW([cell_reference])", 0, 1, NULL, NULL) // Returns the row number of a specified cell
OP( OP_ROWS,        "ROWS(range)", 1, 1, eval_rangeop, NULL) // Returns the number of rows in a specified array or range
__( OP_SHEET,       "SHEET([Text|Reference])", 0, 1, NULL, NULL) // Returns the sheet number of the reference or the string representing a sheet name.
__( OP_SHEETS,      "SHEETS([Reference])", 0, 1, NULL, NULL) // Returns the number of sheets in a reference or current document.
__( OP_TYPE,        "TYPE(value)", 1, 1, NULL, NULL) // Returns a number associated with the type of data passed into the function
__( OP_VALUE,       "VALUE(text)", 1, 1, NULL, NULL) // Converts a string in any of the date, time or number formats that Google Sheets understands into a number

XX( OP_COLS,        "@cols", 1, 1, eval_rangeop, NULL)
XX( OP_COLTOA,      "@coltoa", 1, 1, eval_coltoa, NULL)
XX( OP_ERR,         "@err", -1, -1, eval_other, NULL)
XX( OP_FILENAME,    "@filename", 1, 1, eval_filename, NULL)
XX( OP_LASTCOL,     "@lastcol", -1, -1, eval_other, NULL)
XX( OP_LASTROW,     "@lastrow", -1, -1, eval_other, NULL)
XX( OP_MYCOL,       "@mycol", -1, -1, eval_other, NULL)
XX( OP_MYROW,       "@myrow", -1, -1, eval_other, NULL)
XX( OP_NUMITER,     "@numiter", -1, -1, eval_other, NULL)
XX( OP_NVAL,        "@nval", 2, 2, eval_nval, NULL)
XX( OP_STON,        "@ston", 1, 1, eval_ston, NULL)
XX( OP_SVAL,        "@sval", 2, 2, eval_sval, NULL)

/* 6.14 Lookup Functions */
__( OP_ADDRESS,     "ADDRESS(row, column, [absolute_relative_mode], [use_a1_notation], [sheet])", 2, 5, NULL, NULL) // Returns a cell reference as a string
__( OP_CHOOSE,      "CHOOSE(index, choice1, [choice2, ...])", 2, -1, NULL, NULL) // Returns an element from a list of choices based on index
__( OP_GETPIVOTDATA, "GETPIVOTDATA(value_name, any_pivot_table_cell, [original_column, ...], [pivot_item, ...])", 2, -1, NULL, NULL) // Extracts an aggregated value from a pivot table that corresponds to the specified row and column headings
OP( OP_HLOOKUP,     "HLOOKUP(search_key, range, index, [is_sorted])", 3, 3, eval_rangeop, NULL) // Horizontal lookup. Searches across the first row of a range for a key and returns the value of a specified cell in the column found
OP( OP_INDEX,       "INDEX(reference, [row], [column])", 2, 3, eval_rangeop, NULL) // Returns the content of a cell, specified by row and column offset
__( OP_INDIRECT,    "INDIRECT(cell_reference_as_string, [is_A1_notation])", 1, 2, NULL, NULL) // Returns a cell reference specified by a string
OP( OP_LOOKUP,      "LOOKUP(search_key, search_range|search_result_array, [result_range])", 2, 3, eval_rangeop, NULL) // Looks through a row or column for a key and returns the value of the cell in a result range located in the same position as the search row or column
__( OP_MATCH,       "MATCH(search_key, range, [search_type])", 2, 3, NULL, NULL) // Returns the relative position of an item in a range that matches a specified value
__( OP_MULTIPLE_OPERATIONS, "MULTIPLE.OPERATIONS(formulacell, rowcell, rowreplacement, [columncell, columnreplacement])", 3, 5, NULL, NULL) // Executes a formula expression while substituting a row reference and a column reference.
__( OP_OFFSET,      "OFFSET(cell_reference, offset_rows, offset_columns, [height], [width])", 3, 5, NULL, NULL) // Returns a range reference shifted a specified number of rows and columns from a starting cell reference
OP( OP_VLOOKUP,     "VLOOKUP(search_key, range, index, [is_sorted])", 3, 3, eval_rangeop, NULL) // Vertical lookup. Searches down the first column of a range for a key and returns the value of a specified cell in the row found

XX( OP_STINDEX,     "@stindex", 2, 3, eval_rangeop, NULL)

/* 6.15 Logical Functions */
__( OP_AND,         "AND(logical_expression1, [logical_expression2, ...])", 1, -1, NULL, NULL) // Returns true if all of the provided arguments are logically true, and false if any of the provided arguments are logically false
__( OP_FALSE,       "FALSE()", -1, 0, NULL, NULL) // Returns the logical value `FALSE`
OP( OP_IF,          "IF(logical_expression, [value_if_true, [value_if_false]])", 3, 3, eval_if, NULL) // Returns one value if a logical expression is `TRUE` and another if it is `FALSE`
__( OP_IFERROR,     "IFERROR(value, [value_if_error])", 1, 2, NULL, NULL) // Returns the first argument if it is not an error value, otherwise returns the second argument if present, or a blank if the second argument is absent
__( OP_IFNA,        "IFNA(value, value_if_na)", 2, 2, NULL, NULL) // Evaluates a value. If the value is an #N/A error, returns the specified value.
LO( OP_IFS,         "IFS(condition1, value1, [condition2, value2], ...)", 2, -1, NULL, NULL) // Evaluates multiple conditions and returns a value that corresponds to the first true condition.
__( OP_NOT,         "NOT(logical_expression)", 1, 1, NULL, NULL) // Returns the opposite of a logical value - `NOT(TRUE)` returns `FALSE`; `NOT(FALSE)` returns `TRUE`
__( OP_OR,          "OR(logical_expression1, [logical_expression2, ...])", 1, -1, NULL, NULL) // Returns true if any of the provided arguments are logically true, and false if all of the provided arguments are logically false
LO( OP_SWITCH,      "SWITCH(expression, case1, value1, [default or case2, value2], ...)", 1, -1, NULL, NULL) // Tests an expression against a list of cases and returns the corresponding value of the first matching case, with an optional default value if nothing else is met
__( OP_TRUE,        "TRUE()", -1, 1, NULL, NULL) // Returns the logical value `TRUE`
__( OP_XOR,         "XOR(logical_expression1, [logical_expression2, ...])", 1, -1, NULL, NULL) // The XOR function performs an exclusive or of 2 numbers that returns a 1 if the numbers are different, and a 0 otherwise.

/* 6.16 Mathematical Functions */
OP( OP_ABS,         "ABS(value)", 1, 1, eval_fn1, fabs) // Returns the absolute value of a number
OP( OP_ACOS,        "ACOS(value)", 1, 1, eval_fn1, acos) // Returns the inverse cosine of a value, in radians
__( OP_ACOSH,       "ACOSH(value)", 1, 1, NULL, NULL) // Returns the inverse hyperbolic cosine of a number
__( OP_ACOT,        "ACOT(value)", 1, 1, NULL, NULL) // Returns the inverse cotangent of a value, in radians.
__( OP_ACOTH,       "ACOTH(value)", 1, 1, NULL, NULL) // Returns the inverse hyperbolic cotangent of a value, in radians. Must not be between -1 and 1, inclusive.
OP( OP_ASIN,        "ASIN(value)", 1, 1, eval_fn1, asin) // Returns the inverse sine of a value, in radians
__( OP_ASINH,       "ASINH(value)", 1, 1, NULL, NULL) // Returns the inverse hyperbolic sine of a number
OP( OP_ATAN,        "ATAN(value)", 1, 1, eval_fn1, atan) // Returns the inverse tangent of a value, in radians
OP( OP_ATAN2,       "ATAN2(x, y)", 2, 2, eval_fn2, atan2) // Returns the angle between the x-axis and a line segment from the origin (0,0) to specified coordinate pair (`x`,`y`), in radians
__( OP_ATANH,       "ATANH(value)", 1, 1, NULL, NULL) // Returns the inverse hyperbolic tangent of a number
__( OP_BESSELI,     "BESSELI(x, n)", 2, 2, NULL, NULL) // Returns the modified Bessel function of integer order In(X).
__( OP_BESSELJ,     "BESSELJ(x, n)", 2, 2, NULL, NULL) // Returns the Bessel function of integer order Jn(X) (cylinder function)
__( OP_BESSELK,     "BESSELK(x, n)", 2, 2, NULL, NULL) // Returns the modified Bessel function of integer order Kn(x).
__( OP_BESSELY,     "BESSELY(x, n)", 2, 2, NULL, NULL) // Returns the Bessel function of integer order Yn(X), also known as the Neumann function.
__( OP_COMBIN,      "COMBIN(n, k)", 2, 2, NULL, NULL) // Returns the number of ways to choose some number of objects from a pool of a given size of objects
__( OP_COMBINA,     "COMBINA(n, k)", 2, 2, NULL, NULL) // Returns the number of ways to choose some number of objects from a pool of a given size of objects, including ways that choose the same object multiple times.
__( OP_CONVERT,     "CONVERT(value, start_unit, end_unit)", 3, 3, NULL, NULL) // Converts a numeric value to a different unit of measure
OP( OP_COS,         "COS(angle)", 1, 1, eval_fn1, cos) // Returns the cosine of an angle provided in radians
__( OP_COSH,        "COSH(value)", 1, 1, NULL, NULL) // Returns the hyperbolic cosine of any real number
__( OP_COT,         "COT(angle)", 1, 1, NULL, NULL) // Cotangent of an angle provided in radians.
__( OP_COTH,        "COTH(value)", 1, 1, NULL, NULL) // Returns the hyperbolic cotangent of any real number.
__( OP_CSC,         "CSC(angle)", 1, 1, NULL, NULL) // Returns the cosecant of an angle provided in radians.
__( OP_CSCH,        "CSCH(value)", 1, 1, NULL, NULL) // The CSCH function returns the hyperbolic cosecant of any real number.
__( OP_DEGREES,     "DEGREES(angle)", 1, 1, NULL, NULL) // Converts an angle value in radians to degrees
__( OP_DELTA,       "DELTA(number1, [number2])", 1, 2, NULL, NULL) // Compare two numeric values, returning 1 if they're equal
__( OP_ERF,         "ERF(lower_bound, [upper_bound])", 1, 2, NULL, NULL) // The ERF function returns the integral of the Gauss error function over an interval of values.
__( OP_ERFC,        "ERFC(z)", 1, 1, NULL, NULL) // Returns the complementary Gauss error function of a value
LO( OP_ERFC_PRECISE, "ERFC.PRECISE(z)", 1, 1, NULL, NULL) // See ERFC


__( OP_EUROCONVERT, "EUROCONVERT(n, from, to, [fullprecision=FALSE, [triangulationprecision]])", 3, 5, NULL, NULL) // Converts a Number, representing a value in one European currency, to an equivalent value in another European currency, according to the fixed conversion rates defined by the Council of the European Union.
__( OP_EVEN,        "EVEN(value)", 1, 1, NULL, NULL) // Rounds a number up to the nearest even integer
OP( OP_EXP,         "EXP(exponent)", 1, 1, eval_fn1, exp) // Returns Euler's number, e (~2.718) raised to a power
__( OP_FACT,        "FACT(value)", 1, 1, NULL, NULL) // Returns the factorial of a number
__( OP_FACTDOUBLE,  "FACTDOUBLE(value)", 1, 1, NULL, NULL) // Returns the 'double factorial' of a number
__( OP_GAMMA,       "GAMMA(number)", 1, 1, NULL, NULL) // Returns the Gamma function evaluated at the specified value.
__( OP_GAMMALN,     "GAMMALN(value)", 1, 1, NULL, NULL) // Returns the the logarithm of a specified Gamma function, base e (Euler's number)
LO( OP_GAMMALN_PRECISE, "GAMMALN.PRECISE(value)", 1, 1, NULL, NULL) // See GAMMALN
__( OP_GCD,         "GCD(value1, [value2...])", 1, -1, NULL, NULL) // Returns the greatest common divisor of one or more integers
__( OP_GESTEP,      "GESTEP(value, [step])", 1, 2, NULL, NULL) // Returns 1 if the rate is strictly greater than or equal to the provided step value or 0 otherwise. If no step value is provided then the default value of 0 will be used.
__( OP_ISO_CEILING, "ISO.CEILING(number, [significance])", 1, 2, NULL, NULL) // See CEILING.PRECISE
__( OP_LCM,         "LCM(value1, [value2 ...])", 1, -1, NULL, NULL) // Returns the least common multiple of one or more integers
OP( OP_LN,          "LN(value)", 1, 1, eval_fn1, log) // Returns the the logarithm of a number, base e (Euler's number)
OP( OP_LOG,         "LOG(value, [base])", 1, 2, eval_fn1, log10) // Returns the the logarithm of a number given a base
OP( OP_LOG10,       "LOG10(value)", 1, 1, eval_fn1, log10) // Returns the the logarithm of a number, base 10
__( OP_MOD,         "MOD(dividend, divisor)", 2, 2, NULL, NULL) // Returns the result of the modulo operator, the remainder after a division operation
__( OP_MULTINOMIAL, "MULTINOMIAL(value1, [value2...])", 1, -1, NULL, NULL) // Returns the factorial of the sum of values divided by the product of the values' factorials
__( OP_ODD,         "ODD(value)", 1, 1, NULL, NULL) // Rounds a number up to the nearest odd integer
OP( OP_PI,          "PI()", -1, 0, eval_pi, NULL) // Returns the value of Pi to 14 decimal places
__( OP_POWER,       "POWER(base, exponent)", 2, 2, NULL, NULL) // Returns a number raised to a power
__( OP_PRODUCT,     "PRODUCT(factor1, [factor2, ...])", 1, -1, NULL, NULL) // Returns the result of multiplying a series of numbers together
__( OP_QUOTIENT,    "QUOTIENT(dividend, divisor)", 2, 2, NULL, NULL) // Returns one number divided by another
__( OP_RADIANS,     "RADIANS(angle)", 1, 1, NULL, NULL) // Converts an angle value in degrees to radians
OP( OP_RAND,        "RAND()", 0, 0, eval_rand, NULL) // Returns a random number between 0 inclusive and 1 exclusive
OP( OP_RANDBETWEEN, "RANDBETWEEN(low, high)", 2, 2, eval_fn2, rand_between) // Returns a uniformly random integer between two values, inclusive
__( OP_SEC,         "SEC(angle)", 1, 1, NULL, NULL) // The SEC function returns the secant of an angle, measured in radians.
__( OP_SECH,        "SECH(value)", 1, 1, NULL, NULL) // The SECH function returns the hyperbolic secant of an angle
__( OP_SERIESSUM,   "SERIESSUM(x, n, m, a)", 4, 4, NULL, NULL) // Given parameters x, n, m, and a, returns the power series sum a1xn + a2x(n+m) + ... + aix(n+(i-1)m), where i is the number of entries in range `a`
__( OP_SIGN,        "SIGN(value)", 1, 1, NULL, NULL) // Given an input number, returns `-1` if it is negative, `1` if positive, and `0` if it is zero
OP( OP_SIN,         "SIN(angle)", 1, 1, eval_fn1, sin) // Returns the sine of an angle provided in radians
__( OP_SINH,        "SINH(value)", 1, 1, NULL, NULL) // Returns the hyperbolic sine of any real number
OP( OP_SQRT,        "SQRT(value)", 1, 1, eval_fn1, sqrt) // Returns the positive square root of a positive number
__( OP_SQRTPI,      "SQRTPI(value)", 1, 1, NULL, NULL) // Returns the positive square root of the product of Pi and the given positive number
__( OP_SUBTOTAL,    "SUBTOTAL(function_code, range1, [range2, ...])", 2, -1, NULL, NULL) // Returns a subtotal for a vertical range of cells using a specified aggregation function
OP( OP_SUM,         "SUM(value1, [value2, ...])", 1, -1, eval_rangeop, NULL) // Returns the sum of a series of numbers and/or cells
__( OP_SUMIF,       "SUMIF(range, criterion, [sum_range])", 2, 3, NULL, NULL) // Returns a conditional sum across a range
__( OP_SUMIFS,      "SUMIFS(sum_range, criteria_range1, criterion1, [criteria_range2, criterion2, ...])", 3, -1, NULL, NULL) // Returns the sum of a range depending on multiple criteria
__( OP_SUMPRODUCT,  "SUMPRODUCT(arrays)", 1, -1, NULL, NULL) // Returns the sum of the products of the matrix elements.
__( OP_SUMSQ,       "SUMSQ(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns the sum of the squares of a series of numbers and/or cells
__( OP_SUMX2MY2,    "SUMX2MY2(array A, array B)", 2, 2, NULL, NULL) // Returns the sum of the difference between the squares of the matrices A and B.
__( OP_SUMX2PY2,    "SUMX2PY2(array A, array B)", 2, 2, NULL, NULL) // Returns the total sum of the squares of the matrices A and B
__( OP_SUMXMY2,     "SUMXMY2(array A, array B)", 2, 2, NULL, NULL) // Returns the sum of the squares of the differences between matrix A and B.
OP( OP_TAN,         "TAN(angle)", 1, 1, eval_fn1, tan) // Returns the tangent of an angle provided in radians
__( OP_TANH,        "TANH(value)", 1, 1, NULL, NULL) // Returns the hyperbolic tangent of any real number

XX( OP_FABS,        "@fabs", 1, 1, eval_fn1, fabs)
XX( OP_HYPOT,       "@hypot", 2, 2, eval_fn2, hypot)
XX( OP_DTR,         "@dtr", 1, 1, eval_fn1, dtr)
XX( OP_RTD,         "@rtd", 1, 1, eval_fn1, rtd)
XX( OP_POW,         "@pow", 2, 2, eval_fn2, pow)
XX( OP_PROD,        "@prod", 1, -1, eval_rangeop, NULL)

/* 6.17 Rounding Functions */
__( OP_CEILING,     "CEILING(value, [factor], [mode])", 1, 3, NULL, NULL) // Rounds a number up to the nearest integer multiple of specified significance
LO( OP_CEILING_MATH, "CEILING.MATH(number, [significance], [mode])", 1, 3, NULL, NULL) // Rounds a number up to the nearest integer multiple of specified significance, with negative numbers rounding toward or away from 0 depending on the mode.
LO( OP_CEILING_PRECISE, "CEILING.PRECISE(number, [significance])", 1, 2, NULL, NULL) // Rounds a number up to the nearest integer multiple of specified significance. If the number is positive or negative, it is rounded up.
__( OP_INT,         "INT(value)", 1, 1, NULL, NULL) // Rounds a number down to the nearest integer that is less than or equal to it
OP( OP_FLOOR,       "FLOOR(value, [factor])", 1, 2, eval_fn1, floor) // Rounds a number down to the nearest integer multiple of specified significance
LO( OP_FLOOR_MATH,  "FLOOR.MATH(number, [significance], [mode])", 1, 3, NULL, NULL) // Rounds a number down to the nearest integer multiple of specified significance, with negative numbers rounding toward or away from 0 depending on the mode.
LO( OP_FLOOR_PRECISE, "FLOOR.PRECISE(number, [significance])", 1, 2, NULL, NULL) // The FLOOR.PRECISE function rounds a number down to the nearest integer or multiple of specified significance.
__( OP_MROUND,      "MROUND(value, factor)", 2, 2, NULL, NULL) // Rounds one number to the nearest integer multiple of another
OP( OP_ROUND,       "ROUND(value, [places])", 1, 2, eval_fn2, doround) // Rounds a number to a certain number of decimal places according to standard rules
__( OP_ROUNDDOWN,   "ROUNDDOWN(value, [places])", 1, 2, NULL, NULL) // Rounds a number to a certain number of decimal places, always rounding down to the next valid increment
__( OP_ROUNDUP,     "ROUNDUP(value, [places])", 1, 2, NULL, NULL) // Rounds a number to a certain number of decimal places, always rounding up to the next valid increment
__( OP_TRUNC,       "TRUNC(value, [places])", 1, 2, NULL, NULL) // Truncates a number to a certain number of significant digits by omitting less significant digits

XX( OP_CEIL,        "@ceil", 1, 1, eval_fn1, ceil)
XX( OP_RND,         "@rnd", 1, 1, eval_fn1, dornd)

/* 6.18 Statistical Functions */
__( OP_AVEDEV,      "AVEDEV(value1, [value2, ...])", 1, -1, NULL, NULL) // Calculates the average of the magnitudes of deviations of data from a dataset's mean
__( OP_AVERAGE,     "AVERAGE(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns the numerical average value in a dataset, ignoring text
__( OP_AVERAGE_WEIGHTED, "AVERAGE.WEIGHTED(values, weights, [additional values], [additional weights])", 1, 1, NULL, NULL) // Finds the weighted average of a set of values, given the values and the corresponding weights.
__( OP_AVERAGEA,    "AVERAGEA(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns the numerical average value in a dataset
__( OP_AVERAGEIF,   "AVERAGEIF(criteria_range, criterion, [average_range])", 2, 3, NULL, NULL) // Returns the average of a range depending on criteria
__( OP_AVERAGEIFS,  "AVERAGEIFS(average_range, criteria_range1, criterion1, [criteria_range2, criterion2, ...])", 3, -1, NULL, NULL) // Returns the average of a range depending on multiple criteria
__( OP_BETA_DIST,   "BETA.DIST(value, alpha, beta, cumulative, lower_bound, upper_bound)", 1, 1, NULL, NULL) // Returns the probability of a given value as defined by the beta distribution function.
__( OP_BETA_INV,    "BETA.INV(probability, alpha, beta, lower_bound, upper_bound)", 1, 1, NULL, NULL) // Returns the value of the inverse beta distribution function for a given probability.
__( OP_BETADIST,    "BETADIST(value, alpha, beta, lower_bound, upper_bound)", 1, 1, NULL, NULL) // See BETA.DIST.
__( OP_BETAINV,     "BETAINV(probability, alpha, beta, lower_bound, upper_bound)", 1, 1, NULL, NULL) //  See BETA.INV
__( OP_BINOM_DIST,  "BINOM.DIST(num_successes, num_trials, prob_success, cumulative)", 1, 1, NULL, NULL) // See BINOMDIST
__( OP_BINOM_INV,   "BINOM.INV(num_trials, prob_success, target_prob)", 1, 1, NULL, NULL) // See CRITBINOM
__( OP_BINOMDIST,   "BINOMDIST(num_successes, num_trials, prob_success, cumulative)", 1, 1, NULL, NULL) // Calculates the probability of drawing a certain number of successes (or a maximum number of successes) in a certain number of tries given a population of a certain size containing a certain number of successes, with replacement of draws
__( OP_CHIDIST,     "CHIDIST(x, degrees_freedom)", 1, 1, NULL, NULL) // Calculates the right-tailed chi-squared distribution, often used in hypothesis testing
__( OP_CHIINV,      "CHIINV(probability, degrees_freedom)", 1, 1, NULL, NULL) // Calculates the inverse of the right-tailed chi-squared distribution
__( OP_CHISQ_DIST,  "CHISQ.DIST(x, degrees_freedom, cumulative)", 1, 1, NULL, NULL) // Calculates the left-tailed chi-squared distribution, often used in hypothesis testing
__( OP_CHISQ_DIST_RT, "CHISQ.DIST.RT(x, degrees_freedom)", 1, 1, NULL, NULL) // Calculates the right-tailed chi-squared distribution, which is commonly used in hypothesis testing
__( OP_CHISQ_INV,   "CHISQ.INV(probability, degrees_freedom)", 1, 1, NULL, NULL) // Calculates the inverse of the left-tailed chi-squared distribution
__( OP_CHISQ_INV_RT, "CHISQ.INV.RT(probability, degrees_freedom)", 1, 1, NULL, NULL) // Calculates the inverse of the right-tailed chi-squared distribution
__( OP_CHISQ_TEST,  "CHISQ.TEST(observed_range, expected_range)", 1, 1, NULL, NULL) // See CHITEST
__( OP_CHITEST,     "CHITEST(observed_range, expected_range)", 1, 1, NULL, NULL) // Returns the probability associated with a Pearsons chi-squared test on the two ranges of data. Determines the likelihood that the observed categorical data is drawn from an expected distribution
__( OP_CONFIDENCE,  "CONFIDENCE(alpha, standard_deviation, pop_size)", 1, 1, NULL, NULL) // See CONFIDENCE.NORM
__( OP_CONFIDENCE_NORM, "CONFIDENCE.NORM(alpha, standard_deviation, pop_size)", 1, 1, NULL, NULL) // Calculates the width of half the confidence interval for a normal distribution.
__( OP_CONFIDENCE_T, "CONFIDENCE.T(alpha, standard_deviation, size)", 1, 1, NULL, NULL) // Calculates the width of half the confidence interval for a Students t-distribution.
__( OP_CORREL,      "CORREL(data_y, data_x)", 1, 1, NULL, NULL) // Calculates r, the Pearson product-moment correlation coefficient of a dataset
__( OP_COUNT,       "COUNT(value1, [value2, ...])", 1, 1, NULL, NULL) // Returns a count of the number of numeric values in a dataset
__( OP_COUNTA,      "COUNTA(value1, [value2, ...])", 1, 1, NULL, NULL) // Returns a count of the number of values in a dataset
__( OP_COVAR,       "COVAR(data_y, data_x)", 1, 1, NULL, NULL) // Calculates the covariance of a dataset
__( OP_COVARIANCE_P, "COVARIANCE.P(data_y, data_x)", 1, 1, NULL, NULL) // See COVAR
__( OP_COVARIANCE_S, "COVARIANCE.S(data_y, data_x)", 1, 1, NULL, NULL) // Calculates the covariance of a dataset, where the dataset is a sample of the total population.
__( OP_CRITBINOM,   "CRITBINOM(num_trials, prob_success, target_prob)", 1, 1, NULL, NULL) // Calculates the smallest value for which the cumulative binomial distribution is greater than or equal to a specified criteria
__( OP_DEVSQ,       "DEVSQ(value1, value2)", 1, 1, NULL, NULL) // Calculates the sum of squares of deviations based on a sample
__( OP_EXPON_DIST,  "EXPON.DIST(x, lambda, cumulative)", 1, 1, NULL, NULL) // Returns the value of the exponential distribution function with a specified lambda at a specified value.
__( OP_EXPONDIST,   "EXPONDIST(x, lambda, cumulative)", 1, 1, NULL, NULL) // See EXPON.DIST
__( OP_F_DIST,      "F.DIST(x, degrees_freedom1, degrees_freedom2, cumulative)", 1, 1, NULL, NULL) // Calculates the left-tailed F probability distribution (degree of diversity) for two data sets with given input x. Alternately called Fisher-Snedecor distribution or Snedecor's F distribution
__( OP_F_DIST_RT,   "F.DIST.RT(x, degrees_freedom1, degrees_freedom2)", 1, 1, NULL, NULL) // Calculates the right-tailed F probability distribution (degree of diversity) for two data sets with given input x. Alternately called Fisher-Snedecor distribution or Snedecor's F distribution
__( OP_F_INV,       "F.INV(probability, degrees_freedom1, degrees_freedom2)", 1, 1, NULL, NULL) // Calculates the inverse of the left-tailed F probability distribution. Also called the Fisher-Snedecor distribution or Snedecors F distribution
__( OP_F_INV_RT,    "F.INV.RT(probability, degrees_freedom1, degrees_freedom2)", 1, 1, NULL, NULL) // Calculates the inverse of the right-tailed F probability distribution. Also called the Fisher-Snedecor distribution or Snedecors F distribution
__( OP_F_TEST,      "F.TEST(range1, range2)", 1, 1, NULL, NULL) // See FTEST.
__( OP_FDIST,       "FDIST(x, degrees_freedom1, degrees_freedom2)", 1, 1, NULL, NULL) // See F.DIST.RT.
__( OP_FINV,        "FINV(probability, degrees_freedom1, degrees_freedom2)", 1, 1, NULL, NULL) // See F.INV.RT
__( OP_FISHER,      "FISHER(value)", 1, 1, NULL, NULL) // Returns the Fisher transformation of a specified value
__( OP_FISHERINV,   "FISHERINV(value)", 1, 1, NULL, NULL) // Returns the inverse Fisher transformation of a specified value
__( OP_FORECAST,    "FORECAST(x, data_y, data_x)", 1, 1, NULL, NULL) // Calculates the expected y-value for a specified x based on a linear regression of a dataset
__( OP_FORECAST_LINEAR, "FORECAST.LINEAR(x, data_y, data_x)", 1, 1, NULL, NULL) // See FORECAST
__( OP_FTEST,       "FTEST(range1, range2)", 1, 1, NULL, NULL) // Returns the probability associated with an F-test for equality of variances. Determines whether two samples are likely to have come from populations with the same variance
__( OP_GAMMA_DIST,  "GAMMA.DIST(x, alpha, beta, cumulative)", 1, 1, NULL, NULL) // Calculates the gamma distribution, a two-parameter continuous probability distribution
__( OP_GAMMA_INV,   "GAMMA.INV(probability, alpha, beta)", 1, 1, NULL, NULL) // The GAMMA.INV function returns the value of the inverse gamma cumulative distribution function for the specified probability and alpha and beta parameters.
__( OP_GAMMADIST,   "GAMMADIST(x, alpha, beta, cumulative)", 1, 1, NULL, NULL) // See GAMMA.DIST
__( OP_GAMMAINV,    "GAMMAINV(probability, alpha, beta)", 1, 1, NULL, NULL) // See GAMMA.INV.
__( OP_GAUSS,       "GAUSS(z)", 1, 1, NULL, NULL) // The GAUSS function returns the probability that a random variable, drawn from a normal distribution, will be between the mean and z standard deviations above (or below) the mean.
__( OP_GEOMEAN,     "GEOMEAN(value1, value2)", 1, 1, NULL, NULL) // Calculates the geometric mean of a dataset
__( OP_HARMEAN,     "HARMEAN(value1, value2)", 1, 1, NULL, NULL) // Calculates the harmonic mean of a dataset
__( OP_HYPGEOM.DIST, "HYPGEOM.DIST(num_successes, num_draws, successes_in_pop, pop_size)", 1, 1, NULL, NULL) // See HYPGEOMDIST
__( OP_HYPGEOMDIST, "HYPGEOMDIST(num_successes, num_draws, successes_in_pop, pop_size)", 1, 1, NULL, NULL) //  Calculates the probability of drawing a certain number of successes in a certain number of tries given a population of a certain size containing a certain number of successes, without replacement of draws
__( OP_INTERCEPT,   "INTERCEPT(data_y, data_x)", 1, 1, NULL, NULL) // Calculates the y-value at which the line resulting from linear regression of a dataset will intersect the y-axis (x=0)
__( OP_KURT,        "KURT(value1, value2)", 1, 1, NULL, NULL) // Calculates the kurtosis of a dataset, which describes the shape, and in particular the 'peakedness' of that dataset
__( OP_LARGE,       "LARGE(data, n)", 2, 2, NULL, NULL) // Returns the nth largest element from a data set, where n is user-defined
__( OP_LOGINV,      "LOGINV(x, mean, standard_deviation)", 1, 1, NULL, NULL) // Returns the value of the inverse log-normal cumulative distribution with given mean and standard deviation at a specified value
__( OP_LOGNORM_DIST, "LOGNORM.DIST(x, mean, standard_deviation)", 1, 1, NULL, NULL) // See LOGNORMDIST
__( OP_LOGNORM_INV, "LOGNORM.INV(x, mean, standard_deviation)", 1, 1, NULL, NULL) // See LOGINV
__( OP_LOGNORMDIST, "LOGNORMDIST(x, mean, standard_deviation)", 1, 1, NULL, NULL) // Returns the value of the log-normal cumulative distribution with given mean and standard deviation at a specified value
OP( OP_MAX,         "MAX(value1, [value2, ...])", 1, -1, eval_rangeop, NULL) // Returns the maximum value in a numeric dataset
__( OP_MAXA,        "MAXA(value1, value2)", 2, 2, NULL, NULL) // Returns the maximum numeric value in a dataset
__( OP_MAXIFS,      "MAXIFS(range, criteria_range1, criterion1, [criteria_range2, criterion2], ¦)", 1, 1, NULL, NULL) // Returns the maximum value in a range of cells, filtered by a set of criteria.
__( OP_MEDIAN,      "MEDIAN(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns the median value in a numeric dataset
OP( OP_MIN,         "MIN(value1, [value2, ...])", 1, -1, eval_rangeop, NULL) // Returns the minimum value in a numeric dataset
__( OP_MINA,        "MINA(value1, value2)", 2, 2, NULL, NULL) // Returns the minimum numeric value in a dataset
__( OP_MINIFS,      "MINIFS(range, criteria_range1, criterion1, [criteria_range2, criterion2], ¦)", 1, 1, NULL, NULL) // Returns the minimum value in a range of cells, filtered by a set of criteria.
__( OP_MODE,        "MODE(value1, [value2, ...])", 1, -1, NULL, NULL) // Returns the most commonly occurring value in a dataset
__( OP_MODE_MULT,   "MODE.MULT(value1, value2)", 1, 1, NULL, NULL) // Returns the most commonly occurring values in a dataset.
__( OP_MODE_SNGL,   "MODE.SNGL(value1, [value2, ...])", 1, 1, NULL, NULL) // See MODE
__( OP_NEGBINOM_DIST, "NEGBINOM.DIST(num_failures, num_successes, prob_success)", 1, 1, NULL, NULL) // See NEGBINOMDIST
__( OP_NEGBINOMDIST, "NEGBINOMDIST(num_failures, num_successes, prob_success)", 1, 1, NULL, NULL) // Calculates the probability of drawing a certain number of failures before a certain number of successes given a probability of success in independent trials
__( OP_NORM_DIST,   "NORM.DIST(x, mean, standard_deviation, cumulative)", 1, 1, NULL, NULL) // See NORMDIST
__( OP_NORM_INV,    "NORM.INV(x, mean, standard_deviation)", 1, 1, NULL, NULL) // See NORMINV
__( OP_NORM_S_DIST, "NORM.S.DIST(x)", 1, 1, NULL, NULL) // See NORMSDIST
__( OP_NORM_S_INV,  "NORM.S.INV(x)", 1, 1, NULL, NULL) // See NORMSINV
__( OP_NORMDIST,    "NORMDIST(x, mean, standard_deviation, cumulative)", 1, 1, NULL, NULL) // Returns the value of the normal distribution function (or normal cumulative distribution function) for a specified value, mean, and standard deviation
__( OP_NORMINV,     "NORMINV(x, mean, standard_deviation)", 1, 1, NULL, NULL) // Returns the value of the inverse normal distribution function for a specified value, mean, and standard deviation
__( OP_NORMSDIST,   "NORMSDIST(x)", 1, 1, NULL, NULL) // Returns the value of the standard normal cumulative distribution function for a specified value
__( OP_NORMSINV,    "NORMSINV(x)", 1, 1, NULL, NULL) // Returns the value of the inverse standard normal distribution function for a specified value
__( OP_PEARSON,     "PEARSON(data_y, data_x)", 1, 1, NULL, NULL) // Calculates r, the Pearson product-moment correlation coefficient of a dataset
__( OP_PERCENTILE,  "PERCENTILE(data, percentile)", 1, 1, NULL, NULL) // Returns the value at a given percentile of a dataset
__( OP_PERCENTILE_EXC, "PERCENTILE.EXC(data, percentile)", 1, 1, NULL, NULL) // Returns the value at a given percentile of a dataset, exclusive of 0 and 1.
__( OP_PERCENTILE_INC, "PERCENTILE.INC(data, percentile)", 1, 1, NULL, NULL) // See PERCENTILE
__( OP_PERCENTRANK, "PERCENTRANK(data, value, [significant_digits])", 1, 1, NULL, NULL) // Returns the percentage rank (percentile) of a specified value in a dataset
__( OP_PERCENTRANK_EXC, "PERCENTRANK.EXC(data, value, [significant_digits])", 1, 1, NULL, NULL) // Returns the percentage rank (percentile) from 0 to 1 exclusive of a specified value in a dataset
__( OP_PERCENTRANK_INC, "PERCENTRANK.INC(data, value, [significant_digits])", 1, 1, NULL, NULL) // Returns the percentage rank (percentile) from 0 to 1 inclusive of a specified value in a dataset
__( OP_PERMUT,      "PERMUT(n, k)", 2, 2, NULL, NULL) // Returns the number of ways to choose some number of objects from a pool of a given size of objects, considering order
__( OP_PERMUTATIONA, "PERMUTATIONA(number, number_chosen)", 2, 2, NULL, NULL) // Returns the number of permutations for selecting a group of objects (with replacement) from a total number of objects.
__( OP_PHI,         "PHI(x)", 1, 1, NULL, NULL) // The PHI function returns the value of the normal distribution with mean 0 and standard deviation 1.
__( OP_POISSON,     "POISSON(x, mean, cumulative)", 1, 1, NULL, NULL) // See POISSON.DIST
__( OP_POISSON_DIST, "POISSON.DIST(x, mean, [cumulative])", 1, 1, NULL, NULL) // Returns the value of the Poisson distribution function (or Poisson cumulative distribution function) for a specified value and mean.
__( OP_PROB,        "PROB(data, probabilities, low_limit, [high_limit])", 1, 1, NULL, NULL) // Given a set of values and corresponding probabilities, calculates the probability that a value chosen at random falls between two limits
__( OP_QUARTILE,    "QUARTILE(data, quartile_number)", 1, 1, NULL, NULL) // Returns a value nearest to a specified quartile of a dataset
__( OP_QUARTILE_EXC, "QUARTILE.EXC(data, quartile_number)", 1, 1, NULL, NULL) // Returns value nearest to a given quartile of a dataset, exclusive of 0 and 4.
__( OP_QUARTILE_INC, "QUARTILE.INC(data, quartile_number)", 1, 1, NULL, NULL) // See QUARTILE
__( OP_RANK,        "RANK(value, data, [is_ascending])", 2, 3, NULL, NULL) // Returns the rank of a specified value in a dataset
__( OP_RANK_AVG,    "RANK.AVG(value, data, [is_ascending])", 1, 1, NULL, NULL) // Returns the rank of a specified value in a dataset. If there is more than one entry of the same value in the dataset, the average rank of the entries will be returned
__( OP_RANK_EQ,     "RANK.EQ(value, data, [is_ascending])", 1, 1, NULL, NULL) // Returns the rank of a specified value in a dataset. If there is more than one entry of the same value in the dataset, the top rank of the entries will be returned
__( OP_RSQ,         "RSQ(data_y, data_x)", 1, 1, NULL, NULL) // Calculates the square of r, the Pearson product-moment correlation coefficient of a dataset
__( OP_SKEW,        "SKEW(value1, value2)", 1, 1, NULL, NULL) // Calculates the skewness of a dataset, which describes the symmetry of that dataset about the mean
__( OP_SKEW_P,      "SKEW.P(value1, value2)", 1, 1, NULL, NULL) // Calculates the skewness of a dataset that represents the entire population.
__( OP_SLOPE,       "SLOPE(data_y, data_x)", 1, 1, NULL, NULL) // Calculates the slope of the line resulting from linear regression of a dataset
__( OP_SMALL,       "SMALL(data, n)", 2, 2, NULL, NULL) // Returns the nth smallest element from a data set, where n is user-defined
__( OP_STANDARDIZE, "STANDARDIZE(value, mean, standard_deviation)", 1, 1, NULL, NULL) // Calculates the normalized equivalent of a random variable given mean and standard deviation of the distribution
__( OP_STDEV,       "STDEV(value1, [value2, ...])", 1, -1, NULL, NULL) // Calculates the standard deviation based on a sample
__( OP_STDEV_P,     "STDEV.P(value1, [value2, ...])", 1, -1, NULL, NULL) // See STDEVP
__( OP_STDEV_S,     "STDEV.S(value1, [value2, ...])", 1, -1, NULL, NULL) // See STDEV
__( OP_STDEVA,      "STDEVA(value1, value2)", 2, 2, NULL, NULL) // Calculates the standard deviation based on a sample, setting text to the value `0`
__( OP_STDEVP,      "STDEVP(value1, value2)", 2, 2, NULL, NULL) // Calculates the standard deviation based on an entire population
__( OP_STDEVPA,     "STDEVPA(value1, value2)", 2, 2, NULL, NULL) // Calculates the standard deviation based on an entire population, setting text to the value `0`
__( OP_STEYX,       "STEYX(data_y, data_x)", 2, 2, NULL, NULL) // Calculates the standard error of the predicted y-value for each x in the regression of a dataset
__( OP_T_DIST,      "T.DIST(x, degrees_freedom, cumulative)", 3, 3, NULL, NULL) // Returns the right tailed Student distribution for a value x.
__( OP_T_DIST_2T,   "T.DIST.2T(x, degrees_freedom)", 2, 2, NULL, NULL) // Returns the two tailed Student distribution for a value x.
__( OP_T_DIST_RT,   "T.DIST.RT(x, degrees_freedom)", 2, 2, NULL, NULL) // Returns the right tailed Student distribution for a value x.
__( OP_T_INV,       "T.INV(probability, degrees_freedom)", 2, 2, NULL, NULL) // Calculates the negative inverse of the one-tailed TDIST function
__( OP_T_INV_2T,    "T.INV.2T(probability, degrees_freedom)", 2, 2, NULL, NULL) // Calculates the inverse of the two-tailed TDIST function
__( OP_T_TEST,      "T.TEST(range1, range2, tails, type)", 4, 4, NULL, NULL) // Returns the probability associated with Student's t-test. Determines whether two samples are likely to have come from the same two underlying populations that have the same mean.
__( OP_TDIST,       "TDIST(x, degrees_freedom, tails)", 3, 3, NULL, NULL) // Calculates the probability for Student's t-distribution with a given input (x)
__( OP_TINV,        "TINV(probability, degrees_freedom)", 2, 2, NULL, NULL) // See T.INV.2T
__( OP_TRIMMEAN,    "TRIMMEAN(data, exclude_proportion)", 2, 2, NULL, NULL) // Calculates the mean of a dataset excluding some proportion of data from the high and low ends of the dataset
__( OP_TTEST,       "TTEST(range1, range2, tails, type)", 4, 3, NULL, NULL) // See T.TEST.
__( OP_VAR,         "VAR(value1, [value2, ...])", 1, -1, NULL, NULL) // Calculates the variance based on a sample
__( OP_VAR_P,       "VAR.P(value1, [value2, ...])", 1, -1, NULL, NULL) // See VARP
__( OP_VAR_S,       "VAR.S(value1, [value2, ...])", 1, -1, NULL, NULL) // See VAR
__( OP_VARA,        "VARA(value1, value2)", 2, 2, NULL, NULL) // Calculates an estimate of variance based on a sample, setting text to the value `0`
__( OP_VARP,        "VARP(value1, value2)", 2, 2, NULL, NULL) // Calculates the variance based on an entire population
__( OP_VARPA,       "VARPA(value1, value2,...)", 2, -1, NULL, NULL) // Calculates the variance based on an entire population, setting text to the value `0`
__( OP_WEIBULL,     "WEIBULL(x, shape, scale, cumulative)", 4, 4, NULL, NULL) // Returns the value of the Weibull distribution function (or Weibull cumulative distribution function) for a specified shape and scale
__( OP_WEIBULL_DIST, "WEIBULL.DIST(x, shape, scale, cumulative)", 4, 4, NULL, NULL) // See WEIBULL
__( OP_Z_TEST,      "Z.TEST(data, value, [standard_deviation])", 2, 3, NULL, NULL) // Returns the one-tailed P-value of a Z-test with standard distribution.
__( OP_ZTEST,       "ZTEST(data, value, [standard_deviation])", 2, 3, NULL, NULL) // See Z.TEST.

XX( OP_AVG,         "@avg", 1, -1, eval_rangeop, NULL)
XX( OP_LMAX,        "@max", 1, -1, eval_lmax, NULL)
XX( OP_LMIN,        "@min", 1, -1, eval_lmin, NULL)
XX( OP_STDDEV,      "@stddev", 1, -1, eval_rangeop, NULL)

/* 6.19 Number Representation Conversion Functions */
__( OP_TO_DATE,     "TO_DATE(value)", 1, 1, NULL, NULL) // Converts a provided number to a date
__( OP_TO_DOLLARS,  "TO_DOLLARS(value)", 1, 1, NULL, NULL) // Converts a provided number to a dollar value
__( OP_TO_PERCENT,  "TO_PERCENT(value)", 1, 1, NULL, NULL) // Converts a provided number to a percentage
__( OP_TO_PURE_NUMBER, "TO_PURE_NUMBER(value)", 1, 1, NULL, NULL) // Converts a provided date/time, percentage, currency or other formatted numeric value to a pure number without formatting
__( OP_TO_TEXT,     "TO_TEXT(value)", 1, 1, NULL, NULL) // Converts a provided numeric value to a text value

__( OP_ARABIC,      "ARABIC(roman_numeral)", 1, 1, NULL, NULL) // Computes the value of a Roman numeral
__( OP_BASE,        "BASE(value, base, [min_length])", 2, 3, NULL, NULL) // Converts a number into a text representation in another base, for example, base 2 for binary.
__( OP_BIN2DEC,     "BIN2DEC(signed_binary_number)", 1, 1, NULL, NULL) // Converts a signed binary number to decimal format
__( OP_BIN2HEX,     "BIN2HEX(signed_binary_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a signed binary number to signed hexadecimal format
__( OP_BIN2OCT,     "BIN2OCT(signed_binary_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a signed binary number to signed octal format
__( OP_DEC2BIN,     "DEC2BIN(decimal_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a decimal number to signed binary format
__( OP_DEC2HEX,     "DEC2HEX(decimal_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a decimal number to signed hexadecimal format
__( OP_DEC2OCT,     "DEC2OCT(decimal_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a decimal number to signed octal format
__( OP_DECIMAL,     "DECIMAL(value, base)", 2, 2, NULL, NULL) // The DECIMAL function converts the text representation of a number in another base, to base 10 (decimal).
__( OP_HEX2BIN,     "HEX2BIN(signed_hexadecimal_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a signed hexadecimal number to signed binary format
__( OP_HEX2DEC,     "HEX2DEC(signed_hexadecimal_number)", 1, 1, NULL, NULL) // Converts a signed hexadecimal number to decimal format
__( OP_HEX2OCT,     "HEX2OCT(signed_hexadecimal_number, significant_digits)", 1, 2, NULL, NULL) // Converts a signed hexadecimal number to signed octal format
__( OP_OCT2BIN,     "OCT2BIN(signed_octal_number, [significant_digits])", 1, 1, NULL, NULL) // Converts a signed octal number to signed binary format
__( OP_OCT2DEC,     "OCT2DEC(signed_octal_number)", 1, 1, NULL, NULL) // Converts a signed octal number to decimal format
__( OP_OCT2HEX,     "OCT2HEX(signed_octal_number, [significant_digits])", 1, 2, NULL, NULL) // Converts a signed octal number to signed hexadecimal format
__( OP_ROMAN,       "ROMAN(number, [rule_relaxation])", 1, 2, NULL, NULL) // Formats a number in Roman numerals

XX( OP_FMT,         "@fmt", 2, 2, eval_fmt, NULL)

/* 6.20 Text Functions */
__( OP_ASC,         "ASC(text)", 1, 1, NULL, NULL) // Converts full-width ASCII and katakana characters to their half-width counterparts. All standard-width characters will remain unchanged.
__( OP_CHAR,        "CHAR(table_number)", 1, 1, NULL, NULL) // Convert a number into a character according to the current Unicode table
__( OP_CLEAN,       "CLEAN(text)", 1, 1, NULL, NULL) // Returns the text with the non-printable ASCII characters removed
__( OP_CODE,        "CODE(string)", 1, 1, NULL, NULL) // Returns the numeric Unicode map value of the first character in the string provided
__( OP_CONCATENATE, "CONCATENATE(string1, [string2, ...])", 1, -1, NULL, NULL) // Appends strings to one another
__( OP_DOLLAR,      "DOLLAR(number, [number_of_places])", 1, 2, NULL, NULL) // Formats a number into the locale-specific currency format
__( OP_EXACT,       "EXACT(string1, string2)", 2, 2, NULL, NULL) // Tests whether two strings are identical
__( OP_FIND,        "FIND(search_for, text_to_search, [starting_at])", 2, 3, NULL, NULL) // Returns the position at which a string is first found within text
__( OP_FIXED,       "FIXED(number, [number_of_places], [suppress_separator])", 1, 3, NULL, NULL) // Formats a number with a fixed number of decimal places
__( OP_JIS,         "JIS(text)", 1, 1, NULL, NULL) // Converts half-width to full-width ASCII and katakana characters.
LO( OP_JOIN,        "JOIN(delimiter, value_or_array1, [value_or_array2, ...])", 2, -1, NULL, NULL) // Concatenates the elements of one or more one-dimensional arrays using a specified delimiter
__( OP_LEFT,        "LEFT(string, [number_of_characters])", 1, 2, NULL, NULL) // Returns a substring from the beginning of a specified string
__( OP_LEN,         "LEN(text)", 1, 1, NULL, NULL) // Returns the length of a string
OP( OP_LOWER,       "LOWER(text)", 1, 1, eval_case, NULL) // Converts a specified string to lowercase
__( OP_MID,         "MID(string, starting_at, extract_length)", 3, 3, NULL, NULL) // Returns a segment of a string
__( OP_PROPER,      "PROPER(text_to_capitalize)", 1, 1, NULL, NULL) // Capitalizes each word in a specified string
LO( OP_REGEXEXTRACT, "REGEXEXTRACT(text, regular_expression)", 2, 2, NULL, NULL) // Extracts matching substrings according to a regular expression
LO( OP_REGEXMATCH,  "REGEXMATCH(text, regular_expression)", 1, 1, NULL, NULL) // Whether a piece of text matches a regular expression
LO( OP_REGEXREPLACE, "REGEXREPLACE(text, regular_expression, replacement)", 3, 3, NULL, NULL) // Replaces part of a text string with a different text string using regular expressions
__( OP_REPLACE,     "REPLACE(text, position, length, new_text)", 4, 4, NULL, NULL) // Replaces part of a text string with a different text string
__( OP_REPT,        "REPT(text_to_repeat, number_of_repetitions)", 2, 2, NULL, NULL) // Returns specified text repeated a number of times
__( OP_RIGHT,       "RIGHT(string, [number_of_characters])", 1, 2, NULL, NULL) // Returns a substring from the end of a specified string
__( OP_SEARCH,      "SEARCH(search_for, text_to_search, [starting_at])", 2, 3, NULL, NULL) // Returns the position at which a string is first found within text
LO( OP_SPLIT,       "SPLIT(text, delimiter, [split_by_each], [remove_empty_text])", 2, 4, NULL, NULL) // Divides text around a specified character or string, and puts each fragment into a separate cell in the row
__( OP_SUBSTITUTE,  "SUBSTITUTE(text_to_search, search_for, replace_with, [occurrence_number])", 3, 4, NULL, NULL) // Replaces existing text with new text in a string
__( OP_T,           "T(value)", 1, 1, NULL, NULL) // Returns string arguments as text
__( OP_TEXT,        "TEXT(number, format)", 2, 2, NULL, NULL) // Converts a number into text according to a specified format
LO( OP_TEXTJOIN,    "TEXTJOIN(delimiter, ignore_empty, text1, [text2], ...)", 3, -1, NULL, NULL) // Combines the text from multiple strings and/or arrays, with a specifiable delimiter separating the different texts.
__( OP_TRIM,        "TRIM(text)", 1, 1, NULL, NULL) // Removes leading and trailing spaces in a specified string
__( OP_UNICHAR,     "UNICHAR(number)", 1, 1, NULL, NULL) // Returns the Unicode character for a number.
__( OP_UNICODE,     "UNICODE(text)", 1, 1, NULL, NULL) // Returns the decimal Unicode value of the first character of the text.
OP( OP_UPPER,       "UPPER(text)", 1, 1, eval_case, NULL) // Converts a specified string to uppercase

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
