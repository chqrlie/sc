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
static struct abbrev *abbr_tail;

static int are_abbrevs(void) {
    return abbr_base != NULL;
}

/* unlink and free an abbrev */
static void free_abbr(SCXMEM struct abbrev *a) {
    if (a) {
        if (a->next)
            a->next->prev = a->prev;
        else
            abbr_tail = a->prev;
        if (a->prev)
            a->prev->next = a->next;
        else
            abbr_base = a->next;
        string_free(a->name);
        string_free(a->exp);
        scxfree(a);
    }
}

/* add an abbreviation */
void add_abbr(SCXMEM string_t *name, SCXMEM string_t *exp) {
    struct abbrev *a, *prev;
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
                error("Cannot open pipe to %s", pager);
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
        string_free(name);
        string_free(exp);
        return;
    }

    /* extract the name */
    if (!exp) {
        for (p = s2c(name), i = 0; p[i]; i++) {
            if (p[i] == ' ') {
                exp = string_new(p + i + 1);
                string_set(&name, string_new_len(p, i));
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
        string_free(name);
        string_free(exp);
        return;
    }

    a = find_abbr(s2c(name), -1, &prev);

    // no expansion: lookup abbreviation
    if (sempty(exp)) {
        if (a) {
            error("abbrev \"%s %s\"", s2c(a->name), s2c(a->exp));
        } else {
            error("abbreviation \"%s\" does not exist", s2c(name));
        }
        string_free(name);
        string_free(exp);
        return;
    }

    /* otherwise add an abbreviation */
    if (a) {
        string_set(&a->exp, exp);
        string_free(name);
        return;
    } else {
        /* insert in lexicographical order */
        a = scxmalloc(sizeof(struct abbrev));
        a->name = name;
        a->exp = exp;

        /* link abbreviation in doubly linked list */
        if ((a->prev = prev) != NULL) {
            a->next = prev->next;
            prev->next = a;
        } else {
            a->next = abbr_base;
            abbr_base = a;
        }
        if (a->next)
            a->next->prev = a;
        else
            abbr_tail = a;

        modflg++;
        return;
    }
}

void del_abbr(SCXMEM string_t *name) {
    struct abbrev *a, *prev;

    if (name && (a = find_abbr(s2c(name), -1, &prev)) != NULL)
        free_abbr(a);
    string_free(name);
}

void clean_abbrevs(void) {
    while (abbr_base)
        free_abbr(abbr_base);
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
        // XXX: should quote expansion string
        fprintf(f, "abbrev \"%s %s\"\n", s2c(a->name), s2c(a->exp));
    }
}
