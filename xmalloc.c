/*
 * A safer saner malloc, for careless programmers
 * $Revision: 7.16 $
 */

#include <stdlib.h>
#include <limits.h>
#include "compat.h"
#include "sc.h"

static void fatal(const char *);

#define MAGIC   (double)1234567890.12344

void *scxmalloc(size_t n)
{
    void *ptr;

    if ((ptr = malloc(n + sizeof(double))) == NULL)
        fatal("scxmalloc: no memory");
    *((double *)ptr) = MAGIC;              /* magic number */
    return ptr + sizeof(double);
}

/* we make sure realloc will do a malloc if needed */
void *scxrealloc(void *ptr, size_t n)
{
    if (ptr == NULL)
        return scxmalloc(n);

    ptr -= sizeof(double);
    if (*((double *)ptr) != MAGIC)
        fatal("scxrealloc: storage not scxmalloc'ed");

    if ((ptr = realloc(ptr, n + sizeof(double))) == NULL)
        fatal("scxmalloc: no memory");
    *((double *)ptr) = MAGIC;              /* magic number */
    return ptr + sizeof(double);
}

char *scxdup(const char *s) {
    size_t size = strlen(s) + 1;
    return memcpy(scxmalloc(size), s, size);
}

void scxfree(void *p)
{
    if (p == NULL)
        fatal("scxfree: NULL");
    p -= sizeof(double);
    if (*((double *)p) != MAGIC)
        fatal("scxfree: storage not malloc'ed");
    free(p);
}

static void fatal(const char *str)
{
#ifndef PSC
    deraw(1);
#endif /* PSC */
    fprintf(stderr, "%s\n", str);
#ifndef PSC
    diesave();
#endif /* PSC */
    exit(1);
}
