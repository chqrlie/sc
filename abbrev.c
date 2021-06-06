/*      SC      A Spreadsheet Calculator
 *              Abbreviations
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Originally created:  November, 2001
 *
 *              $Revision: 7.16 $
 */

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <limits.h>
#include "sc.h"

static struct abbrev *abbr_base;

static int are_abbrevs(void) {
    return abbr_base != NULL;
}

// add an abbreviation
void add_abbr(SCXMEM char *string) {
    struct abbrev *a;
    struct abbrev *prev;
    char *p;
    char *expansion;

    // null or empty string: list abbreviations to the PAGER
    if (!string || *string == '\0') {
        if (!are_abbrevs()) {
            error("No abbreviations defined");
        } else {
            FILE *f;
            int pid;
            char px[MAXCMD];
            const char *pager;

            if (!(pager = getenv("PAGER")))
                pager = DFLT_PAGER;
            snprintf(px, sizeof px, "| %s", pager);
            f = openfile(px, sizeof px, &pid, NULL);
            if (!f) {
                error("Can't open pipe to %s", pager);
            } else {
                fprintf(f, "\n%-15s %s\n", "Abbreviation", "Expanded");
                if (!brokenpipe)
                    fprintf(f, "%-15s %s\n", "------------", "--------");

                for (a = abbr_base; a && !brokenpipe; a = a->next) {
                    fprintf(f, "%-15s %s\n", a->abbr, a->exp);
                }
                closefile(f, pid, 0);
            }
        }
        scxfree(string);
        return;
    }

    // get expansion if any
    if ((expansion = strchr(string, ' ')) != NULL)
        *expansion++ = '\0';

    if (isalnumchar_(*string)) {
        for (p = string + 1; *p; p++) {
            if (!isalnumchar_(*p)) {
                error("Invalid abbreviation: %s", string);
                scxfree(string);
                return;
            }
        }
    } else {
        for (p = string + 1; *p; p++) {
            if (isalnumchar_(*p) && p[1]) {
                error("Invalid abbreviation: %s", string);
                scxfree(string);
                return;
            }
        }
    }

    a = find_abbr(string, -1, &prev);

    // no expansion: lookup abbreviation
    if (expansion == NULL) {
        if (a) {
            error("abbrev \"%s %s\"", a->abbr, a->exp);
        } else {
            error("abbreviation \"%s\" doesn't exist", string);
        }
        scxfree(string);
        return;
    }

    // string has space: separates name and expansion
    if (a) del_abbr(string);

    a = scxmalloc(sizeof(struct abbrev));
    a->abbr = string;
    a->exp = expansion;

    // link abbreviation in singly linked list
    if (prev) {
        a->next = prev->next;
        prev->next = a;
    } else {
        a->next = abbr_base;
        abbr_base = a;
    }
}

void del_abbr(const char *abbrev)
{
    struct abbrev *a;
    struct abbrev *prev;

    if ((a = find_abbr(abbrev, -1, &prev)) != NULL) {
        if (prev)
            prev->next = a->next;
        else
            abbr_base = a->next;
        scxfree(a->abbr);
        scxfree(a);
    }
}

struct abbrev *find_abbr(const char *abbrev, int len, struct abbrev **prev)
{
    struct abbrev *a;
    int cmp;
    int exact = FALSE;

    if (len < 0) {
        exact = TRUE;
        len = strlen(abbrev);
    }

    *prev = NULL;
    for (a = abbr_base; a; *prev = a, a = a->next) {
        if ((cmp = strncmp(abbrev, a->abbr, len)) < 0)
            return NULL;
        if (cmp == 0) {
            if (!exact || a->abbr[len] == '\0')
                return a;
        }
    }
    return NULL;
}

void write_abbrevs(FILE *f)
{
    struct abbrev *a;

    for (a = abbr_base; a; a = a->next) {
        fprintf(f, "abbrev \"%s %s\"\n", a->abbr, a->exp);
    }
}
