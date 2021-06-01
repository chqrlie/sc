/* Carsten Kunze, 2016 */

#include <string.h>
#include "config.h"

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
