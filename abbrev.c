/*      SC      A Spreadsheet Calculator
 *              Abbreviations
 *
 *              Chuck Martin <nrocinu@myrealbox.com>
 *              Originally created:  November, 2001
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include "sc.h"

static SCXMEM struct abbrev *abbr_base;

static int are_abbrevs(void) {
    return abbr_base != NULL;
}

/* unlink and free an abbrev */
static void free_abbr(SCXMEM struct abbrev *a, struct abbrev *prev) {
    if (a) {
        if (prev)
            prev->next = a->next;
        else
            abbr_base = a->next;
        scxfree(a->name);
        scxfree(a->exp);
        scxfree(a);
    }
}

/* add an abbreviation */
void add_abbr(const char *string) {
    char name[32];
    struct abbrev *a;
    struct abbrev *prev;
    const char *expansion;
    int i, j;

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
                    fprintf(f, "%-15s %s\n", a->name, a->exp);
                }
                closefile(f, pid, 0);
            }
        }
        return;
    }

    /* extract the name */
    for (i = j = 0; string[i] && string[i] != ' '; i++) {
        if (i < (int)sizeof(name) - 1)
            name[j++] = string[i];
    }
    name[j] = '\0';

    expansion = NULL;
    if (string[i] == ' ')
        expansion = string + i + 1;

    if (isalnumchar_(*name)) {
        for (i = 1; name[i]; i++) {
            if (!isalnumchar_(name[i])) {
                error("Invalid abbreviation: %s", name);
                return;
            }
        }
    } else {
        for (i = 1; name[i]; i++) {
            if (isalnumchar_(name[i]) && name[i+1]) {
                error("Invalid abbreviation: %s", name);
                return;
            }
        }
    }

    a = find_abbr(name, -1, &prev);

    // no expansion: lookup abbreviation
    if (expansion == NULL) {
        if (a) {
            error("abbrev \"%s %s\"", a->name, a->exp);
        } else {
            error("abbreviation \"%s\" doesn't exist", name);
        }
        return;
    }

    // string has space: separates name and expansion
    free_abbr(a, prev);

    a = scxmalloc(sizeof(struct abbrev));
    a->name = scxdup(name);
    a->exp = scxdup(expansion);

    // link abbreviation in singly linked list
    if (prev) {
        a->next = prev->next;
        prev->next = a;
    } else {
        a->next = abbr_base;
        abbr_base = a;
    }
}

void del_abbr(const char *name) {
    struct abbrev *a;
    struct abbrev *prev;

    if ((a = find_abbr(name, -1, &prev)) != NULL)
        free_abbr(a, prev);
}

void clean_abbrevs(void) {
    while (abbr_base)
        free_abbr(abbr_base, NULL);
}

struct abbrev *find_abbr(const char *name, int len, struct abbrev **prev) {
    struct abbrev *a;
    int cmp;
    int exact = FALSE;

    if (len < 0) {
        exact = TRUE;
        len = strlen(name);
    }

    *prev = NULL;
    for (a = abbr_base; a; *prev = a, a = a->next) {
        if ((cmp = strncmp(name, a->name, len)) < 0)
            return NULL;
        if (cmp == 0) {
            if (!exact || a->name[len] == '\0')
                return a;
        }
    }
    return NULL;
}

void write_abbrevs(FILE *f) {
    struct abbrev *a;

    for (a = abbr_base; a; a = a->next) {
        fprintf(f, "abbrev \"%s %s\"\n", a->name, a->exp);
    }
}
