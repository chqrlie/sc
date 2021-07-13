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
        free_string(a->name);
        free_string(a->exp);
        scxfree(a);
    }
}

/* add an abbreviation */
void add_abbr(SCXMEM string_t *name, SCXMEM string_t *exp) {
    struct abbrev *a;
    struct abbrev *prev;
    const char *p;
    int i;

    // XXX: should have other commands to list abbrevs
    // null or empty string: list abbreviations to the PAGER
    if (sempty(name)) {
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
                    fprintf(f, "%-15s %s\n", s2c(a->name), s2c(a->exp));
                }
                closefile(f, pid, 0);
            }
        }
        free_string(name);
        free_string(exp);
        return;
    }

    /* extract the name */
    if (!exp) {
        for (p = s2c(name), i = 0; p[i]; i++) {
            if (p[i] == ' ') {
                exp = new_string(p + i + 1);
                set_string(&name, new_string_len(p, i));
                break;
            }
        }
    }
    p = s2c(name);
    if (isalphachar_(*p)) {
        for (p += 1; isalnumchar_(*p); p++)
            continue;
    }
    if (*p) {
        error("Invalid abbreviation: %s", s2c(name));
        free_string(name);
        free_string(exp);
        return;
    }

    a = find_abbr(s2c(name), -1, &prev);

    // no expansion: lookup abbreviation
    if (sempty(exp)) {
        if (a) {
            error("abbrev \"%s %s\"", s2c(a->name), s2c(a->exp));
        } else {
            error("abbreviation \"%s\" doesn't exist", s2c(name));
        }
        free_string(name);
        free_string(exp);
        return;
    }

    /* otherwise add an abbreviation */
    if (a) {
        set_string(&a->exp, exp);
        free_string(name);
        return;
    } else {
        a = scxmalloc(sizeof(struct abbrev));
        a->name = name;
        a->exp = exp;

        /* link abbreviation in singly linked list */
        if (prev) {
            a->next = prev->next;
            prev->next = a;
        } else {
            a->next = abbr_base;
            abbr_base = a;
        }
        return;
    }
}

void del_abbr(SCXMEM string_t *name) {
    struct abbrev *a;
    struct abbrev *prev;

    if (name && (a = find_abbr(s2c(name), -1, &prev)) != NULL)
        free_abbr(a, prev);
    free_string(name);
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
        const char *a_name = s2c(a->name);
        if ((cmp = strncmp(name, a_name, len)) < 0)
            return NULL;
        if (cmp == 0) {
            if (!exact || a_name[len] == '\0')
                return a;
        }
    }
    return NULL;
}

void write_abbrevs(FILE *f) {
    struct abbrev *a;

    for (a = abbr_base; a; a = a->next) {
        fprintf(f, "abbrev \"%s %s\"\n", s2c(a->name), s2c(a->exp));
    }
}
