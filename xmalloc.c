/*      SC      A Spreadsheet Calculator
 *              A safer saner malloc, for careless programmers
 *
 *              updated by Charlie Gordon: June, 2021
 *
 * $Revision: 8.1 $
 */

#include "sc.h"

#define MAGIC   (double)1234567890.12344

void *scxmalloc(size_t n)
{
    void *ptr;

    if ((ptr = malloc(n + sizeof(double))) == NULL)
        fatal("scxmalloc: no memory");
    *((double *)ptr) = MAGIC;              /* magic number */
    return (unsigned char *)ptr + sizeof(double);
}

/* we make sure realloc will do a malloc if needed */
void *scxrealloc(void *ptr, size_t n)
{
    if (ptr == NULL)
        return scxmalloc(n);

    ptr = (unsigned char *)ptr - sizeof(double);
    if (*((double *)ptr) != MAGIC)
        fatal("scxrealloc: storage not scxmalloc'ed");

    if ((ptr = realloc(ptr, n + sizeof(double))) == NULL)
        fatal("scxmalloc: no memory");
    *((double *)ptr) = MAGIC;              /* magic number */
    return (unsigned char*)ptr + sizeof(double);
}

char *scxdup(const char *s) {
    size_t size = strlen(s) + 1;
    return memcpy(scxmalloc(size), s, size);
}

void scxfree(void *p)
{
    if (p != NULL) {
        p = (unsigned char*)p - sizeof(double);
        if (*((double *)p) != MAGIC)
            fatal("scxfree: storage not malloc'ed");
        free(p);
    }
}
