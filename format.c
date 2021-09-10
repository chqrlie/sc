/*****************************************************************************
 *
 * Mark Nagel <nagel@ics.uci.edu>
 * 20 July 1989
 *
 * $Revision: 8.1 $
 *
 * int format(buf, buflen, fmt, precision, num)
 *            char buf[];
 *            int buflen;
 *            const char *fmt;
 *            double num;
 *
 * The format function will produce a string representation of a number
 * given a _format_ (described below) and a double value.  The result is
 * written into the passed buffer -- if the resulting string is too
 * long to fit into the passed buffer, the function returns false.
 * Otherwise the function returns true.
 *
 * The fmt parameter contains the format to use to convert the number.
 *
 *  #   Digit placeholder.  If the number has fewer digits on either
 *      side of the decimal point than there are '#' characters in
 *      the format, the extra '#' characters are ignored.  The number
 *      is rounded to the number of digit placeholders as there are
 *      to the right of the decimal point.  If there are more digits
 *      in the number than there are digit placeholders on the left
 *      side of the decimal point, then those digits are displayed.
 *
 *  0   Digit placeholder.  Same as for '#' except that the number
 *      is padded with zeroes on either side of the decimal point.
 *      The number of zeroes used in padding is determined by the
 *      number of digit placeholders after the '0' for digits on
 *      the left side of the decimal point and by the number of
 *      digit placeholders before the '0' for digits on the right
 *      side of the decimal point.
 *
 *  .   Decimal point.  Determines how many digits are placed on
 *      the right and left sides of the decimal point in the number.
 *      Note that numbers smaller than 1 will begin with a decimal
 *      point if the left side of the decimal point contains only
 *      a '#' digit placeholder.  Use a '0' placeholder to get a
 *      leading zero in decimal formats.
 *
 *  %   Percentage.  For each '%' character in the format, the actual
 *      number gets multiplied by 100 (only for purposes of formatting
 *      -- the original number is left unmodified) and the '%' character
 *      is placed in the same position as it is in the format.
 *
 *  ,   Thousands separator.  The presence of a ',' in the format
 *      (multiple commas are treated as one) will cause the number
 *      to be formatted with a ',' separating each set of three digits
 *      in the integer part of the number with numbering beginning
 *      from the right end of the integer.
 *
 *  &   Precision.  When this character is present in the fractional
 *      part of the number, it is equivalent to a number of 0's equal
 *      to the precision specified in the column format command.  For
 *      example, if the precision is 3, "&" is equivalent to "000".
 *
 *  \   Quote.  This character causes the next character to be
 *      inserted into the formatted string directly with no
 *      special interpretation.
 *
 *  E- E+ e- e+
 *      Scientific format.  Causes the number to be formatted in scientific
 *      notation.  The case of the 'E' or 'e' given is preserved.  If
 *      the format uses a '+', then the sign is always given for the
 *      exponent value.  If the format uses a '-', then the sign is
 *      only given when the exponent value is negative.  Note that if
 *      there is no digit placeholder following the '+' or '-', then
 *      that part of the formatted number is left out.  In general,
 *      there should be one or more digit placeholders after the '+'
 *      or '-'.
 *
 *  ;   Format selector.  Use this character to separate the format
 *      into two distinct formats.  The format to the left of the
 *      ';' character will be used if the number given is zero or
 *      positive.  The format to the right of the ';' character is
 *      used if the number given is negative.
 *
 *  Any
 *      Self insert.  Any other character will be inserted directly
 *      into the formatted number with no change made to the actual
 *      number.
 *
 * Excel / GoogleSheet syntax:
 *
 * Up to 4 parts separated by `;`: positive;negative;zero;non-numeric
 *
 *  0    A digit in the number. An insignificant 0 will appear in the results.
 *  #    A digit in the number. An insignificant 0 will not appear in the results.
 *  ?    A digit in the number. An insignificant 0 will appear as a space in the results.
 *  $    Formats numbers as a dollar value.
 *  .    Formats numbers with a decimal point.
 *  ,    Formats numbers with a thousands separator.
 *  /    Formats numbers as a fraction.
 *  %    Formats numbers as a percent
 *  E    Formats numbers as an exponent.
 *  "text"    Adds text to the formula. Insert the desired text within quotations for it to appear.
 *  @     Displays text entered into a cell.
 *  *    Repeats the following character to fill in the remaining space in the cell.
 *  _    Adds a space equal in width to the following character.
 *
 *****************************************************************************/

#ifdef USELOCALE
#include <locale.h>
#endif
#include <math.h>
#include <time.h>
#include "sc.h"

/*****************************************************************************/

// XXX: should take buf_t destination
static int fmt_int(char *dest,
                   size_t size,
                   const char *val,  /* integer part of the value to be formatted */
                   const char *fmt,  /* integer part of the format */
                   int fmt_len,
                   sc_bool_t comma,       /* true if we should comma-ify the value */
                   sc_bool_t negative)    /* true if the value is actually negative */
{
    int i, j, k, vlen;
    int mindigits = 0;
    int digits = 0;

    if (thsep == '\0')
        comma = FALSE;

    /* count the digit placeholders (should be an argument?) */
    for (i = 0; i < fmt_len;) {
        char c;
        switch (c = fmt[i++]) {
        case '"':
            while (i < fmt_len) {
                c = fmt[i++];
                if (c == '"')
                    break;
                if (c == '\\' && i < fmt_len)
                    i++;
            }
            break;
        case '\\':
        case '_':
        case '*':
            if (i < fmt_len)
                i++;
            break;
        case '#':
        case '?':
            digits++;
            break;
        case '0':
            mindigits++;
            digits++;
            break;
        }
    }

    /* number of significant digits (should be an argument) */
    vlen = strlen(val);

    for (i = j = k = 0; i < fmt_len;) {
        char c;
        switch (c = fmt[i++]) {
        case '#':
        case '0':
        case '?':
            if (negative) {
                dest[k++] = '-';
                negative = FALSE;
            }
            while (vlen > digits) {
                dest[k++] = val[j++];
                vlen--;
                if (comma && vlen && !(vlen % 3))
                    dest[k++] = thsep;
            }
            if (vlen >= digits) {
                dest[k++] = val[j++];
                vlen--;
                if (comma && vlen && !(vlen % 3))
                    dest[k++] = thsep;
                mindigits--;
                digits--;
            } else {
                if (mindigits > 0) {
                    dest[k++] = (c == '?') ? ' ' : '0';
                    mindigits--;
                    if (comma && mindigits && !(mindigits % 3))
                        dest[k++] = thsep;
                }
                digits--;
            }
            break;
        case ',':
        case '.':
        case '%':
            break;
        case '"':
            while (i < fmt_len) {
                c = fmt[i++];
                if (c == '"')
                    break;
                if (c == '\\' && i < fmt_len)
                    c = fmt[i++];
                dest[k++] = c;
            }
            break;
        case '*':
            if (i < fmt_len) {
                c = fmt[i++];
                // XXX: should handle *: return fill char and position
            }
            break;
        case '_':
            if (i < fmt_len) {
                c = fmt[i++];
                dest[k++] = ' ';
            }
            break;
        case '\\':
            if (i < fmt_len)
                c = fmt[i++];
            FALLTHROUGH;
        default:
            dest[k++] = c;
            break;
        }
    }
    dest[k] = '\0';
    return k;
}

/*****************************************************************************/

// XXX: should take buf_t destination
static int fmt_frac(char *dest,
                    size_t size,
                    const char *val,    /* fractional part of the value to be formatted */
                    const char *fmt,    /* fractional portion of format */
                    int fmt_len,
                    int lprecision)     /* precision, for interpreting the "&" */
{
    char *bufptr = dest;
    // XXX: should protect against buffer overflow
    //char *endptr = dest + size - 1;
    const char *valptr = val;
    int i, j;
    int has_dec = 0;

    for (i = 0; i < fmt_len;) {
        char c;
        switch (c = fmt[i++]) {
        case '&':
            if (lprecision) {
                if (!has_dec++)
                    *bufptr++ = dpoint;
                for (j = 0; j < lprecision; j++)
                    *bufptr++ = (*valptr != '\0') ? *valptr++ : '0';
            }
            break;
        case '#':
            if (*valptr != '\0') {
                if (!has_dec++)
                    *bufptr++ = dpoint;
                *bufptr++ = *valptr++;
            }
            break;
        case '0':
        case '?':
            if (!has_dec++)
                *bufptr++ = dpoint;
            *bufptr++ = *valptr != '\0' ? *valptr++ : (c == '?' ? ' ' : '0');
            break;
        case ',':
        case '.':
        case '%':
            break;
        case '"':
            while (i < fmt_len) {
                c = fmt[i++];
                if (c == '"')
                    break;
                if (c == '\\' && i < fmt_len)
                    c = fmt[i++];
                *bufptr++ = c;
            }
            break;
        case '*':
            if (i < fmt_len) {
                c = fmt[i++];
                // XXX: should handle *: return fill char and position
            }
            break;
        case '_':
            if (i < fmt_len) {
                i++;
                *bufptr++ = ' ';
            }
            break;
        case '\\':
            if (i < fmt_len)
                c = fmt[i++];
            FALLTHROUGH;
        default:
            *bufptr++ = c;
            break;
        }
    }
    *bufptr = '\0';
    return bufptr - dest;
}

/*****************************************************************************/

// XXX: should take buf_t destination
static int fmt_exp(char *dest,      /* destination array */
                   size_t size,     /* size of destination array */
                   int val,         /* value of the exponent */
                   const char *fmt, /* exponent part of the format string */
                   int fmt_len)
{
    char valbuf[64];
    int i = 0;

    /* fmt points to [Ee][+-] */
    dest[i++] = fmt[0];
    if (val < 0) {
        val = -val;
        dest[i++] = '-';
    } else
    if (fmt[1] == '+') {
        dest[i++] = '+';
    }
    snprintf(valbuf, sizeof valbuf, "%u", (unsigned)val);
    return i + fmt_int(dest + i, size - i, valbuf, fmt + 2, fmt_len - 2, FALSE, FALSE);
}

/*****************************************************************************/

static int skip_fmt(const char *p) {
    int i = 0;
    char c;
    while ((c = p[i]) != '\0' && c != ';') {
        i++;
        switch (c) {
        case '"':
            while (p[i] != '\0') {
                c = p[i++];
                if (c == '"')
                    break;
                if (c == '\\' && p[i] != '\0')
                    i++;
            }
            break;
        case '*':
        case '\\':
        case '_':
            if (p[i] != '\0')
                i++;
            break;
        }
    }
    return i;
}

// XXX: should take buf_t destination
int format(char *buf, size_t buflen, const char *fmt, int lprecision, double val, int *alignp) {
    char mantissa[FBUFLEN];
    const char *integer;
    const char *fraction;
    const char *fmt2, *ep;
    const char *decfmt;
    const char *expfmt;
    char *cp,  *last_digit;
    int i, fmt_len, fmt2_len, decfmt_len, expfmt_len, len,  exp_val, prec;
    sc_bool_t comma = FALSE, negative = FALSE;

    *buf = '\0';
    if (fmt == NULL)
        return 0;

    if (*fmt == ctl('d')) {
        time_t v = (time_t)val;
        if (*alignp == ALIGN_DEFAULT)
            *alignp = ALIGN_LEFT;
        *alignp |= ALIGN_CLIP;
        // XXX: must check format string
        return ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
            (buf, buflen, fmt + 1, localtime(&v));
    }

    /*
     * select positive, negative or zero format if avalable
     */
    fmt_len = skip_fmt(fmt);
    ep = fmt + fmt_len;
    if (*ep == ';') {
        if (val <= 0.0) {
            fmt2_len = skip_fmt(fmt2 = ep + 1);
            ep = fmt2 + fmt2_len;
            if (val < 0.0) {
                val = -val;   /* format should provide sign if desired */
                fmt = fmt2;
                fmt_len = fmt2_len;
            } else {
                if (*ep == ';') {
                    fmt_len = skip_fmt(fmt = ep + 1);
                }
            }
        }
    }

    /* split the format into integer, fraction and exponent parts */
    decfmt = NULL;
    decfmt_len = 0;
    expfmt = NULL;
    expfmt_len = 0;
    for (i = 0; i < fmt_len;) {
        char c;
        switch (c = fmt[i++]) {
        case '"':
            while (i < fmt_len) {
                c = fmt[i++];
                if (c == '"')
                    break;
                if (c == '\\' && i < fmt_len)
                    i++;
            }
            break;
        case '\\':
        case '_':
        case '*':
            if (i < fmt_len)
                ++i;
            break;
        case ',':
            if (!decfmt && !expfmt)
                comma = TRUE;
            break;
        case '.':
            if (!decfmt && !expfmt)
                decfmt = fmt + i - 1;
            break;
        case '%':
            if (!expfmt)
                val *= 100.0;
            break;
        case 'e':
        case 'E':
            if (!expfmt && i + 1 < fmt_len
            &&  (fmt[i] == '+' || fmt[i] == '-')) {
                expfmt = fmt + i - 1;
            }
            break;
        }
    }

    /* The following line was necessary due to problems with the gcc
     * compiler and val being a negative zero.  Thanks to Mike Novack for
     * the suggestion. - CRM
     */
    // XXX: this hack may be catastrophic for very small values
    val = (val + 1.0) - 1.0;
    if (val < 0.0) {
        /* negative is set but will be cleared if the value rounds to zero */
        negative = TRUE;
        val = -val;
    }

    exp_val = 0;
    if (expfmt) {
        expfmt_len = fmt + fmt_len - expfmt;
        fmt_len = expfmt - fmt;
        if (val != 0.0) {
            // XXX: very crude base 10 exponent computation
            //      fails for many cases because of rounding
            while (val < 1.0) {
                val *= 10.0;
                exp_val--;
            }
            while (val >= 10.0) {
                val /= 10.0;
                exp_val++;
            }
        }
    }

    /*
     * determine maximum decimal places and use sprintf
     * to build initial character form of formatted value.
     */
    prec = 0;
    if (decfmt) {
        decfmt++;
        decfmt_len = fmt + fmt_len - decfmt;
        fmt_len = decfmt - fmt - 1;
        for (i = 0; i < decfmt_len;) {
            char c;
            switch (c = decfmt[i++]) {
            case '"':
                while (i < fmt_len) {
                    c = fmt[i++];
                    if (c == '"')
                        break;
                    if (c == '\\' && i < fmt_len)
                        i++;
                }
                break;
            case '\\':
            case '_':
            case '*':
                if (i < decfmt_len)
                    i++;
                break;
            case '?':
            case '#':
            case '0':
                prec++;
                break;
            case '&':
                prec += lprecision;
                break;
            }
        }
    }
    // XXX: should handle NaNs and infinities
    snprintf(mantissa, sizeof mantissa, "%.*lf", prec, val);
    fraction = "";
    last_digit = NULL;
    integer = mantissa;
    if (*integer == '0')
        integer++;
    for (cp = mantissa; *cp != '\0'; cp++) {
        if (*cp == dpoint) {
            *cp++ = '\0';
            fraction = cp;
        } else {
            if (*cp != '0')
                last_digit = cp;
        }
    }
    if (last_digit) {
        /* truncate insignificant zeroes */
        if (last_digit >= fraction)
            last_digit[1] = '\0';
    } else {
        negative = FALSE;
    }
    len = fmt_int(buf, buflen, integer, fmt, fmt_len, comma, negative);
    if (decfmt)
        len += fmt_frac(buf + len, buflen - len, fraction, decfmt, decfmt_len, lprecision);
    if (expfmt)
        len += fmt_exp(buf + len, buflen - len, exp_val, expfmt, expfmt_len);
    return len;
}

/*****************************************************************************/
/*
 * Tom Anderson    <toma@hpsad.hp.com>
 * 10/14/90
 *
 * This routine takes a value and formats it using fixed, scientific,
 * or engineering notation.  The format command 'f' determines which
 * format is used.  The formats are:         example
 *    0:   Fixed point (default)             0.00010
 *    1:   Scientific                        1.00E-04
 *    2:   Engineering                       100.00e-06
 *
 * The format command 'f' now uses three values.  The first two are the
 * width and precision, and the last one is the format value 0, 1, or 2 as
 * described above.  The format value is passed in the variable fmt.
 *
 * This formatted value is written into the passed buffer.  if the
 * resulting string is too long to fit into the passed buffer, the
 * function returns false.  Otherwise the function returns true.
 *
 * When a number is formatted as engineering and is outside of the range,
 * the format reverts to scientific.
 *
 * To preserve compatability with old spreadsheet files, the third value
 * may be missing, and the default will be fixed point (format 0).
 *
 * When an old style sheet is saved, the third value will be stored.
 *
 */

/* defined in sc.h */
#ifndef REFMTFIX
#define REFMTFIX        0
#define REFMTFLT        1
#define REFMTENG        2
#define REFMTDATE       3
#define REFMTLDATE      4
#endif

static const char * const engmult[] = {
    "-18", "-15", "-12", "-09", "-06", "-03",
    "+00",
    "+03", "+06", "+09", "+12", "+15", "+18"
};

// XXX: should take buf_t destination
int engformat(char *buf, size_t size, int fmt, int lprecision, double val, int *alignp) {
    int engind = 0;
    double engmant, engabs, engexp;
    time_t secs;

    // XXX: should ignore empty colformat?
    // XXX: fix this mess
    //if (fmt >= 0 && fmt < COLFORMATS && sp->colformat[fmt])
    //    return format(buf, size, s2c(sp->colformat[fmt]), lprecision, val, alignp);
    switch (fmt) {
    case REFMTFIX:
        return snprintf(buf, size, "%.*f", lprecision, val);
    case REFMTFLT:
        return snprintf(buf, size, "%.*E", lprecision, val);
    case REFMTENG:
        if (val == 0e0) {
            /* Hack to get zeroes to line up in engr fmt */
            // XXX: what hack?
            return snprintf(buf, size, "%.*f ", lprecision, val);
        } else {
            // XXX: this method is flawed because of rounding issues
            engabs = val;
            if ( engabs <  0e0)       engabs = -engabs;
            if ((engabs >= 1e-18) && (engabs <  1e-15)) engind = 0;
            if ((engabs >= 1e-15) && (engabs <  1e-12)) engind = 1;
            if ((engabs >= 1e-12) && (engabs <  1e-9 )) engind = 2;
            if ((engabs >= 1e-9)  && (engabs <  1e-6 )) engind = 3;
            if ((engabs >= 1e-6)  && (engabs <  1e-3 )) engind = 4;
            if ((engabs >= 1e-3)  && (engabs <  1    )) engind = 5;
            if ((engabs >= 1)     && (engabs <  1e3  )) engind = 6;
            if ((engabs >= 1e3)   && (engabs <  1e6  )) engind = 7;
            if ((engabs >= 1e6)   && (engabs <  1e9  )) engind = 8;
            if ((engabs >= 1e9)   && (engabs <  1e12 )) engind = 9;
            if ((engabs >= 1e12)  && (engabs <  1e15 )) engind = 10;
            if ((engabs >= 1e15)  && (engabs <  1e18 )) engind = 11;
            if ((engabs >= 1e18)  && (engabs <  1e21 )) engind = 12;
            if ((engabs < 1e-18)  || (engabs >= 1e21 )) {
                /* Revert to floating point */
                return snprintf(buf, size, "%.*E", lprecision, val);
            } else {
                engexp = (double)(engind - 6) * 3;
                engmant = val / pow(10.0e0, engexp);
                // XXX: why a lower case 'e'?
                return snprintf(buf, size, "%.*fe%s", lprecision,
                                engmant, engmult[engind]);
            }
        }
    case REFMTDATE:
        if (*alignp == ALIGN_DEFAULT)
            *alignp = ALIGN_LEFT;
        *alignp |= ALIGN_CLIP;
        secs = (time_t)val;
        return strftime(buf, size, "%e %b %y", localtime(&secs));
    case REFMTLDATE:
        if (*alignp == ALIGN_DEFAULT)
            *alignp = ALIGN_LEFT;
        *alignp |= ALIGN_CLIP;
        secs = (time_t)val;
        return strftime(buf, size, "%e %b %Y", localtime(&secs));
    }
    if (size > 0) {
        *buf = '\0';
    }
    return -1;
}

// XXX: should not use the C library for this
void sc_set_locale(int set) {
    dpoint = '.';
    thsep = ',';
    FullUpdate++;

    if (set) {
#ifdef USELOCALE
        char *loc = setlocale(LC_ALL, "");
        if (loc != NULL) {
            struct lconv *locstruct = localeconv();
            dpoint = locstruct->decimal_point[0];
            thsep = locstruct->thousands_sep[0];
        }
#else
        error("Locale support not available");
#endif
    } else {
        // XXX: should reset the locale
    }
}
