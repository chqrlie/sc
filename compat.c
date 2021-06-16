/* Carsten Kunze, 2016 */
/* updated by Charlie Gordon: June, 2021 */

#include <string.h>
#include "config.h"
#include "util.h"

#ifndef HAVE_STRLCPY
size_t strlcpy(char *dst, const char *src, size_t dstsize) {
    size_t srclen;
    /* Not conform to strlcpy, but avoids to access illegal memory in case
     * of unterminated strings */
    for (srclen = 0; srclen < dstsize && src[srclen]; srclen++)
        continue;
    if (dstsize > srclen)
        dstsize = srclen;
    else if (dstsize)
        dstsize--;
    else
        return srclen;
    /* assumes non-overlapping buffers */
    memcpy(dst, src, dstsize);
    dst[dstsize] = '\0';
    return srclen;
}
#endif

#ifndef HAVE_STRLCAT
size_t strlcat(char *dst, const char *src, size_t dstsize) {
    size_t ld, ls;
    for (ld = 0; ld < dstsize && dst[ld]; ld++)
        continue;
    dst += ld;
    dstsize -= ld;
    for (ls = 0; ls < dstsize && src[ls]; ls++)
        continue;
    if (dstsize > ls)
        dstsize = ls;
    else if (dstsize)
        dstsize--;
    else
        return ld + ls;
    /* assumes non-overlapping buffers */
    memcpy(dst, src, dstsize);
    dst[dstsize] = '\0';
    return ld + ls;
}
#endif

size_t strsplice(char *dst, size_t size, size_t from, size_t len1,
                 const char *src, size_t len2)
{
    size_t len, len0, len3;
    len0 = strnlen(dst, size - 1);
    if (from > len0)
        from = len0;
    if (len1 > len0 - from)
        len1 = len0 - from;
    len3 = len0 - from - len1;
    len = from + len2 + len3;       /* theoretical length */
    if (len2 > size - from - 1)     /* truncate replacement */
        len2 = size - from - 1;
    if (len3 > size - from - len2 - 1) /* truncate remainder */
        len3 = size - from - len2 - 1;
    memmove(dst + from + len2, dst + from + len1, len3);
    memcpy(dst + from, src, len2);
    dst[from + len2 + len3] = '\0';
    return len;
}

/* return a pointer to the basename portion of the filename */
char *get_basename(const char *filename) {
    char *p = strchr(filename, *filename); // silent cast
    char *base = p;
    char c;
    while ((c = *p++)) {
#ifdef WINDOWS
        if (c == '/' || c == '\\')
            base = p;
#else
#ifdef VMS
        if (c == ']')
            base = p;
#else
        if (c == '/')
            base = p;
#endif
#endif
    }
    return base;
}

/* return a pointer to the extension portion of the filename */
char *get_extension(const char *filename) {
    char *p = get_basename(filename);
    char *ext = NULL;
    while (*p) {
        if (*p == '.')
            ext = p;
        p++;
    }
    if (!ext) {
        ext = p;
    }
    return ext;
}
