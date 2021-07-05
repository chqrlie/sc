/*      SC      A Spreadsheet Calculator
 *              Utility functions:
 *                 malloc wrappers
 *                 string functions
 *                 buffered strings
 *
 *              updated by Charlie Gordon: June, 2021
 *
 * $Revision: 8.1 $
 */

#include "sc.h"

#define USE_MAGIC  1

#ifdef USE_MAGIC
#define MAGIC   (double)1234567890.12344
#define MAGIC_SIZE  sizeof(double)
#else
#define MAGIC_SIZE  0
#endif

void *scxmalloc(size_t n) {
    void *ptr;

    if ((ptr = malloc(n + MAGIC_SIZE)) == NULL)
        fatal("scxmalloc: no memory");

#ifdef USE_MAGIC
    *((double *)ptr) = MAGIC;  /* magic number */
    ptr = (unsigned char *)ptr + MAGIC_SIZE;
#endif
    return ptr;
}

/* we make sure realloc will do a malloc if needed */
void *scxrealloc(void *ptr, size_t n) {
    if (ptr == NULL)
        return scxmalloc(n);
    if (n == 0) {
        scxfree(ptr);
        return NULL;
    }
#ifdef USE_MAGIC
    ptr = (unsigned char *)ptr - MAGIC_SIZE;
    if (*((double *)ptr) != MAGIC)
        fatal("scxrealloc: storage not scxmalloc'ed");
#endif
    if ((ptr = realloc(ptr, n + MAGIC_SIZE)) == NULL)
        fatal("scxmalloc: no memory");
#ifdef USE_MAGIC
    *((double *)ptr) = MAGIC;  /* magic number */
    ptr = (unsigned char *)ptr + MAGIC_SIZE;
#endif
    return ptr;
}

char *scxdup(const char *s) {
    size_t size = strlen(s) + 1;
    char *p = scxmalloc(size);
    return p ? memcpy(p, s, size) : p;
}

void scxfree(void *p) {
    if (p != NULL) {
#ifdef USE_MAGIC
        p = (unsigned char*)p - MAGIC_SIZE;
        if (*((double *)p) != MAGIC)
            fatal("scxfree: storage not malloc'ed");
#endif
        free(p);
    }
}

char *set_string(SCXMEM char **pp, SCXMEM char *s) {
    scxfree(*pp);
    return *pp = s;
}

char *set_cstring(SCXMEM char **pp, const char *s) {
    scxfree(*pp);
    return *pp = s ? scxdup(s) : NULL;
}

/*---------------- string utilities ----------------*/

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

/*---------------- buffered strings ----------------*/

/* append a char to a buffer  */
int buf_putc(buf_t buf, int c) {
    if (buf->len < buf->size - 1) {
        buf->buf[buf->len++] = c;
        buf->buf[buf->len] = '\0';
        return (unsigned char)c;
    }
    return -1;
}

/* append repeated bytes to a buffer */
int buf_repc(buf_t buf, int c, int count) {
    if (buf->len + count >= buf->size)
        count = buf->size - buf->len - 1;
    while (count --> 0)
        buf->buf[buf->len++] = c;
    buf->buf[buf->len] = '\0';
    return count;
}

/* append a block of bytes to a buffer */
size_t buf_put(buf_t buf, const char *s, size_t len) {
    if (buf->len + len >= buf->size)
        len = buf->size - buf->len - 1;
    memcpy(buf->buf + buf->len, s, len);
    buf->buf[buf->len += len] = '\0';
    return len;
}

/* append a string to a buffer */
size_t buf_puts(buf_t buf, const char *s) {
    return buf_put(buf, s, strlen(s));
}

/* append a formated string to a buffer */
size_t buf_printf(buf_t buf, const char *fmt, ...) {
    size_t len;
    va_list ap;
    va_start(ap, fmt);
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    len = ((int (*)(char *, size_t, const char *, va_list))vsnprintf)
        (buf->buf + buf->len, buf->size - buf->len, fmt, ap);
    va_end(ap);
    if (len >= buf->size - buf->len)
        len = buf->size - buf->len - 1;
    buf->len += len;
    return len;
}

/* set buffer contents to block of bytes */
size_t buf_set(buf_t buf, const char *s, size_t len) {
    if (len >= buf->size)
        len = buf->size - 1;
    memcpy(buf->buf, s, len);
    buf->buf[len] = '\0';
    return buf->len = len;
}

/* set buffer contents to a string */
size_t buf_sets(buf_t buf, const char *s) {
    return buf_set(buf, s, strlen(s));
}

/* set buffer contents to a formated string */
size_t buf_setf(buf_t buf, const char *fmt, ...) {
    size_t len;
    va_list ap;
    va_start(ap, fmt);
    // Prevent warning: format string is not a string literal [-Werror,-Wformat-nonliteral]
    len = ((int (*)(char *, size_t, const char *, va_list))vsnprintf)
        (buf->buf, buf->size, fmt, ap);
    va_end(ap);
    if (len >= buf->size)
        len = buf->size - 1;
    return buf->len = len;
}

/* extend the buffer with scxmalloc or scxrealloc to a minimum size */
int buf_extend(buf_t buf, size_t size, size_t blocksize) {
    char *ptr;
    if (size <= buf->size)
        return 0;
    size = (size + blocksize - 1) / blocksize * blocksize;
    if (buf->flags & BUF_ALLOC) {
        ptr = scxrealloc(buf->buf, size);
        if (!ptr)
            return -1;
    } else {
        ptr = scxmalloc(size);
        if (!ptr)
            return -1;
        memcpy(ptr, buf->buf, buf->size);
    }
    buf->buf = ptr;
    buf->size = size;
    buf->flags |= BUF_ALLOC;
    return 0;
}

static int buf_quote(buf_t buf, int c) {
    // XXX: potentially incorrect for embedded `"` and other control characters
    buf_putc(buf, c);
    return 1;
}

int buf_quotechar(buf_t buf, int c1, int c, int c2) {
    int res = 0;
    if (c1 > 0)
        res += buf_putc(buf, c1);
    res += buf_quote(buf, c);
    if (c2 > 0)
        res += buf_putc(buf, c2);
    return res;
}

int buf_quotestr(buf_t buf, int c1, const char *s, int c2) {
    int res = 0;
    if (c1 > 0)
        res += buf_putc(buf, c1);
    while (*s) {
        if (*s == '"' || (*s == '\\' && (s[1] == '\\' || s[1] == '"'))) {
            res += buf_putc(buf, '\\');
            res += buf_putc(buf, *s++);
        } else {
            res += buf_quote(buf, *s++);
        }
    }
    if (c2 > 0)
        res += buf_putc(buf, c2);
    return res;
}
