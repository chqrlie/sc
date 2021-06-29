/*****************************************************************************
 *
 * Mark Nagel <nagel@ics.uci.edu>
 * 20 July 1989
 *
 * $Revision: 8.1 $
 *
 * int format(fmt, precision, num, buf, buflen)
 *            const char *fmt;
 *            double num;
 *            char buf[];
 *            int buflen;
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
 *      side of the decimal point than  there are '#' characters in
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
 *      part of the number, it is equavalent to a number of 0's equal
 *      to the precision specified in the column format command.  For
 *      example, if the precision is 3, "&" is equivalent to "000".
 *
 *  \   Quote.  This character causes the next character to be
 *      inserted into the formatted string directly with no
 *      special interpretation.
 *
 *  E- E+ e- e+
 *      Scientific format.  Causes the number to formatted in scientific
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
 *****************************************************************************/

/*****************************************************************************/

#include <math.h>
#include <time.h>
#include "sc.h"

#define EOS     '\0'
#define MAXBUF  256

static char *fmt_int(const char *val, const char *fmt, bool comma, bool negative);
static char *fmt_frac(const char *val, const char *fmt, int lprecision);
static char *fmt_exp(int val, const char *fmt);

static char *reverse(char *buf);

SCXMEM char *colformat[COLFORMATS];

/*****************************************************************************/

int format(char *buf, size_t buflen, const char *fmt0, int lprecision, double val, int *alignp) {
    char *fmt, *cp, *tmp, *tp;
    bool comma = false, negative = false;
    char *integer = NULL, *decimal = NULL;
    char *exponent = NULL;
    int exp_val = 0;
    int width;
    static char *mantissa = NULL;
    static char *tmpfmt1 = NULL, *tmpfmt2 = NULL, *exptmp = NULL;
    static unsigned mantlen = 0, fmtsize = 0;
    const char *fraction = NULL;
    int zero_pad = 0;

    *buf = '\0';
    if (fmt0 == NULL)
        return 0;

    if (*fmt0 == ctl('d')) {
        time_t v = (time_t)val;
        if (*alignp == ALIGN_DEFAULT)
            *alignp = ALIGN_LEFT;
        *alignp |= ALIGN_CLIP;
        // XXX: must check format string
        return ((size_t (*)(char *, size_t, const char *, const struct tm *tm))strftime)
            (buf, buflen, fmt0 + 1, localtime(&v));
    }

    // XXX: what is this static mess?
    if (strlen(fmt0) >= fmtsize) {
        fmtsize = strlen(fmt0) + 40;
        tmpfmt1 = scxrealloc(tmpfmt1, fmtsize);
        tmpfmt2 = scxrealloc(tmpfmt2, fmtsize);
        exptmp = scxrealloc(exptmp, fmtsize);
    }
    strlcpy(tmpfmt1, fmt0, fmtsize);
    fmt = tmpfmt1;
    if (buflen + 1 > mantlen) {
        mantlen = buflen + 40;
        mantissa = scxrealloc(mantissa, mantlen);
    }

    /*
     * select positive or negative format if necessary
     */
    for (cp = fmt; *cp != ';' && *cp != EOS; cp++) {
        if (*cp == '\\' && cp[1] != EOS)
            cp++;
    }
    if (*cp == ';') {
        if (val < 0.0) {
            val = -val;     /* format should provide sign if desired */
            fmt = cp + 1;
        } else
            *cp = EOS;
    }

    /*
     * extract other information from format and produce a
     * format string stored in tmpfmt2 also scxmalloc()'d above
     */
    tmp = tmpfmt2;
    for (cp = fmt, tp = tmp; *cp != EOS; cp++) {
        switch (*cp) {
        case '\\':
            *tp++ = *cp;
            if (cp[1] != EOS)
                *tp++ = *++cp;
            break;

        case ',':
            comma = true;
            break;

        case '.':
            if (decimal == NULL)
                decimal = tp;
            *tp++ = *cp;
            break;

        case '%':
            val *= 100.0;
            *tp++ = *cp;
            break;

        default:
            *tp++ = *cp;
            break;
        }
    }
    *tp = EOS;
    fmt = tmpfmt2;

    /* The following line was necessary due to problems with the gcc
     * compiler and val being a negative zero.  Thanks to Mike Novack for
     * the suggestion. - CRM
     */
    val = (val + 1.0) - 1.0;
    if (val < 0.0) {
        negative = true;
        val = -val;
    }
    /*
     * extract the exponent from the format if present
     */
    for (cp = fmt; *cp != EOS; cp++) {
        if (*cp == '\\' && cp[1] != EOS)
            cp++;
        else if (*cp == 'e' || *cp == 'E') {
            if (cp[1] == '+' || cp[1] == '-') {
                strlcpy(exptmp, cp, fmtsize);
                exponent = exptmp;
                *cp = EOS;
                if (val != 0.0) {
                    while (val < 1.0) {
                        val *= 10.0;
                        exp_val--;
                    }
                    while (val >= 10.0) {
                        val /= 10.0;
                        exp_val++;
                    }
                }
                break;
            }
        }
    }

    /*
     * determine maximum decimal places and use sprintf
     * to build initial character form of formatted value.
     */
    width = 0;
    if (decimal) {
        *decimal++ = EOS;
        for (cp = decimal; *cp != EOS; cp++) {
            switch (*cp) {
            case '\\':
                if (cp[1]) cp++;
                break;

            case '#':
                width++;
                break;

            case '0':
                zero_pad = ++width;
                break;

            case '&':
                width += lprecision;
                zero_pad = width;
                break;
            }
        }
        zero_pad = strlen(decimal) - zero_pad;
    }
    snprintf(mantissa, mantlen, "%.*lf", width, val);
    for (cp = integer = mantissa; *cp != dpoint && *cp != EOS; cp++) {
        if (*integer == '0')
            integer++;
    }
    if (*cp == dpoint) {
        *cp++ = EOS;
        fraction = cp;
        cp += strlen(cp);
        for (; zero_pad > 0; zero_pad--, cp--) {
            if (cp[-1] == '0')
                cp[-1] = EOS;
            else
                break;
        }
    } else {
        fraction = "";
    }

    /*
     * format the puppy
     */
    {
        // XXX: so much static stuff?
        static char *citmp = NULL, *cftmp = NULL;
        static unsigned cilen = 0, cflen = 0;
        const char *ci, *cf, *ce;
        unsigned int len_ci, len_cf;

        ci = fmt_int(integer, fmt, comma, negative);
        len_ci = strlen(ci);
        if (len_ci >= cilen) {
            cilen = len_ci + 40;
            citmp = scxrealloc(citmp, cilen);
        }
        strlcpy(citmp, ci, cilen);
        ci = citmp;

        cf = decimal ? fmt_frac(fraction, decimal, lprecision) : "";
        len_cf = strlen(cf);
        if (len_cf >= cflen) {
            cflen = len_cf + 40;
            cftmp = scxrealloc(cftmp, cilen);
        }
        strlcpy(cftmp, cf, cilen);
        cf = cftmp;

        ce = (exponent) ? fmt_exp(exp_val, exponent) : "";
        return snprintf(buf, buflen, "%s%s%s", ci, cf, ce);
    }
}

/*****************************************************************************/

// XXX: should take destination array or buf_t

static char *fmt_int(const char *val,  /* integer part of the value to be formatted */
                     const char *fmt,  /* integer part of the format */
                     bool comma,       /* true if we should comma-ify the value */
                     bool negative)    /* true if the value is actually negative */
{
    int digit, f, v;
    int thousands = 0;
    const char *cp;
    static char buf[MAXBUF];
    char *bufptr = buf;

    /*
     * locate the leftmost digit placeholder
     */
    for (cp = fmt; *cp != EOS; cp++) {
        if (*cp == '\\' && cp[1])
            cp++;
        else if (*cp == '#' || *cp == '0')
            break;
    }
    digit = (*cp == EOS) ? -1 : cp - fmt;

    /*
     * format the value
     */
    f = strlen(fmt) - 1;
    v = (digit >= 0) ? (ssize_t)strlen(val) - 1 : -1;
    while (f >= 0 || v >= 0) {
        if (f > 0 && fmt[f-1] == '\\') {
            *bufptr++ = fmt[f--];
        } else if (f >= 0 && (fmt[f] == '#' || fmt[f] == '0')) {
            if (v >= 0 || fmt[f] == '0') {
                *bufptr++ = v < 0 ? '0' : val[v];
                if (comma && (thousands = (thousands + 1) % 3) == 0 &&
                        v > 0 && thsep != '\0')
                    *bufptr++ = thsep;
                v--;
            }
        } else if (f >= 0) {
            *bufptr++ = fmt[f];
        }
        if (v >= 0 && f == digit) {
            continue;
        }
        f--;
    }

    if (negative && digit >= 0)
        *bufptr++ = '-';
    *bufptr = EOS;
    return reverse(buf);
}

/*****************************************************************************/

// XXX: should take destination array or buf_t

static char *fmt_frac(const char *val,     /* fractional part of the value to be formatted */
                      const char *fmt,          /* fractional portion of format */
                      int lprecision)     /* precision, for interpreting the "&" */
{
    static char buf[MAXBUF];
    char *bufptr = buf;
    const char *fmtptr = fmt;
    const char *valptr = val;

    *bufptr++ = dpoint;
    while (*fmtptr != EOS) {
        if (*fmtptr == '&') {
            int i;
            for (i = 0; i < lprecision; i++)
                *bufptr++ = (*valptr != EOS) ? *valptr++ : '0';
        } else if (*fmtptr == '\\')
            *bufptr++ = *++fmtptr;
        else if (*fmtptr == '#' || *fmtptr == '0') {
            if (*valptr != EOS || *fmtptr == '0')
                *bufptr++ = (*valptr != EOS) ? *valptr++ : '0';
        } else
            *bufptr++ = *fmtptr;
        fmtptr++;
    }
    *bufptr = EOS;

    if (buf[1] < '0' || buf[1] > '9')
        return buf + 1;
    else
        return buf;
}

/*****************************************************************************/

// XXX: should take destination array or buf_t

static char *fmt_exp(int val,          /* value of the exponent */
                     const char *fmt)  /* exponent part of the format */
{
    static char buf[MAXBUF];
    char *bufptr = buf;
    char valbuf[64];
    bool negative = false;

    *bufptr++ = *fmt++;
    if (*fmt == '+')
        *bufptr++ = (val < 0) ? '-' : '+';
    else if (val < 0)
        *bufptr++ = '-';
    fmt++;
    *bufptr = EOS;

    if (val < 0) {
        val = -val;
        negative = false;
    }
    snprintf(valbuf, sizeof valbuf, "%d", val);

    strlcat(buf, fmt_int(valbuf, fmt, false, negative), sizeof buf);
    return buf;
}

/*****************************************************************************/

static char *reverse(char *buf) {
    if (*buf) {
        char *a = buf;
        char *b = buf + strlen(buf) - 1;
        while (a < b) {
            char tmp = *b;
            *b-- = *a;
            *a++ = tmp;
        }
    }
    return buf;
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

int engformat(char *buf, int buflen, int fmt, int lprecision, double val, int *alignp) {
    int engind = 0;
    double engmant, engabs, engexp;
    time_t secs;

    *buf = '\0';
    if (fmt >= 0 && fmt < COLFORMATS && colformat[fmt])
        return format(buf, buflen, colformat[fmt], lprecision, val, alignp);
    switch (fmt) {
    case REFMTFIX:
        return snprintf(buf, buflen, "%.*f", lprecision, val);
    case REFMTFLT:
        return snprintf(buf, buflen, "%.*E", lprecision, val);
    case REFMTENG:
        if (val == 0e0) {
            /* Hack to get zeroes to line up in engr fmt */
            return snprintf(buf, buflen, "%.*f ", lprecision, val);
        } else {
            // XXX: this method is flawed because of rounding issues
            engabs = (val);
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
                return snprintf(buf, buflen, "%.*E", lprecision, val);
            } else {
                engexp = (double)(engind - 6) * 3;
                engmant = val / pow(10.0e0, engexp);
                // XXX: why a lower case 'e'?
                return snprintf(buf, buflen, "%.*fe%s", lprecision,
                                engmant, engmult[engind]);
            }
        }
    case REFMTDATE:
        if (*alignp == ALIGN_DEFAULT)
            *alignp = ALIGN_LEFT;
        *alignp |= ALIGN_CLIP;
        secs = (time_t)val;
        return strftime(buf, buflen, "%e %b %y", localtime(&secs));
    case REFMTLDATE:
        if (*alignp == ALIGN_DEFAULT)
            *alignp = ALIGN_LEFT;
        *alignp |= ALIGN_CLIP;
        secs = (time_t)val;
        return strftime(buf, buflen, "%e %b %Y", localtime(&secs));
    }
    return -1;
}

void sc_set_locale(int set) {
    dpoint = '.';
    thsep = ',';
    FullUpdate++;

    if (set) {
#ifdef USELOCALE
        struct lconv *locstruct;
        char *loc = setlocale(LC_ALL, "");
        if (loc != NULL) {
            locstruct = localeconv();
            dpoint = locstruct->decimal_point[0];
            thsep = locstruct->thousands_sep[0];
        }
#else
        error("Locale support not available");
#endif
    }
}
