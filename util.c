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

#define SCXMALLOC_USE_MAGIC  1
#define SCXMALLOC_TRACK_BLOCKS  1
#define SCXMALLOC_FAILURE_IS_FATAL  1

#ifdef SCXMALLOC_TRACK_BLOCKS
static struct dlink {
# ifdef SCXMALLOC_USE_MAGIC
    double magic;
# endif
    size_t size;
    struct dlink *prev, *next;
} mem_head = {
# ifdef SCXMALLOC_USE_MAGIC
    0.0,
# endif
    0, &mem_head, &mem_head
};

static void link_block(struct dlink *p, size_t size) {
    p->size = size;
    p->prev = mem_head.prev;
    p->next = &mem_head;
    p->prev->next = p->next->prev = p;
}

static void unlink_block(struct dlink *p) {
    p->prev->next = p->next;
    p->next->prev = p->prev;
}
#else
# define link_block(p,s)  (void)(p,s)
# define unlink_block(p)  (void)(p)
#endif

#ifdef SCXMALLOC_USE_MAGIC
# define MAGIC   (double)1234567890.12344
# ifdef SCXMALLOC_TRACK_BLOCKS
#  define MAGIC_SIZE  sizeof(struct dlink)
# else
#  define MAGIC_SIZE  sizeof(double)
# endif
#else
# define MAGIC_SIZE  0
#endif

SCXMEM void *scxmalloc(size_t n) {
    void *ptr = malloc(n + MAGIC_SIZE);

    if (ptr == NULL) {
#ifdef SCXMALLOC_FAILURE_IS_FATAL
        fatal("scxmalloc: no memory");
#endif
    } else {
        link_block(ptr, n);
#ifdef SCXMALLOC_USE_MAGIC
        *((double *)ptr) = MAGIC;  /* magic number */
#endif
        ptr = (unsigned char *)ptr + MAGIC_SIZE;
    }
    return ptr;
}

/* we make sure realloc will do a malloc if needed */
SCXMEM void *scxrealloc(SCXMEM void *ptr, size_t n) {
    if (ptr == NULL)
        return scxmalloc(n);

    if (n == 0) {
        scxfree(ptr);
        return NULL;
    }
    ptr = (unsigned char *)ptr - MAGIC_SIZE;
    unlink_block(ptr);
#ifdef SCXMALLOC_USE_MAGIC
    if (*((double *)ptr) != MAGIC)
        fatal("scxrealloc: storage not scxmalloc'ed");
#endif
    ptr = realloc(ptr, n + MAGIC_SIZE);
    if (ptr == NULL) {
#ifdef SCXMALLOC_FAILURE_IS_FATAL
        fatal("scxmalloc: no memory");
#endif
    } else {
        link_block(ptr, n);
#ifdef SCXMALLOC_USE_MAGIC
        *((double *)ptr) = MAGIC;  /* magic number */
#endif
        ptr = (unsigned char *)ptr + MAGIC_SIZE;
    }
    return ptr;
}

SCXMEM char *scxdup(const char *s) {
    size_t size = strlen(s) + 1;
    SCXMEM char *p = scxmalloc(size);
    return p ? memcpy(p, s, size) : p;
}

void scxfree(SCXMEM void *p) {
    if (p != NULL) {
        p = (unsigned char*)p - MAGIC_SIZE;
        unlink_block(p);
#ifdef SCXMALLOC_USE_MAGIC
        if (*((double *)p) != MAGIC)
            fatal("scxfree: storage not malloc'ed");
#endif
        free(p);
    }
}

void scxmemdump(void) {
#ifdef SCXMALLOC_TRACK_BLOCKS
    struct dlink *b = mem_head.next;
    const unsigned char *p;
    size_t i;
    int dup;
    if (b != &mem_head) {
        fprintf(stderr, "Memory blocks: {\n");
        while (b != &mem_head) {
            fprintf(stderr, "  %p: %zu bytes {", (void*)b, b->size);
            for (dup = -1, i = 0, p = (unsigned char *)(b + 1); i < b->size; i++) {
                int c = p[i];
                if (dup > 0 && p[i] == p[i - 1]) {
                    dup++;
                    continue;
                }
                if (dup > 1) {
                    fprintf(stderr, "*%d", dup);
                    dup = 1;
                }
                if (c >= 0x20 && c < 0x7F) {
                    if (dup) fprintf(stderr, " \"");
                    if (c == '\"' || c == '\\')
                        fputc('\\', stderr);
                    fputc(c, stderr);
                    dup = 0;
                } else {
                    if (!dup) fprintf(stderr, "\"");
                    fprintf(stderr, " %02X", c);
                    dup = 1;
                }
            }
            if (dup > 1) {
                fprintf(stderr, "*%d", dup);
                dup = 0;
            } else
            if (!dup) {
                fprintf(stderr, "\"");
            }
            fprintf(stderr, " }\n");
            b = b->next;
        }
        fprintf(stderr, "}\n");
    }
#endif
}

/*---------------- refcounted strings ----------------*/

string_t *new_string(const char *s) {
    size_t len = strlen(s);
    string_t *str = scxmalloc(offsetof(string_t, s) + len + 1);
    if (str) {
        str->refcount = 1;
        str->len = len;
        memcpy(str->s, s, len + 1);
    }
    return str;
}

string_t *new_string_len(const char *s, size_t len) {
    string_t *str = scxmalloc(offsetof(string_t, s) + len + 1);
    if (str) {
        str->refcount = 1;
        str->len = len;
        if (s) memcpy(str->s, s, len);
        str->s[len] = '\0';
    }
    return str;
}

SCXMEM string_t *cat_strings(SCXMEM string_t *s1, SCXMEM string_t *s2) {
    size_t len1, len2;
    SCXMEM string_t *s3;

    if (sempty(s1)) {
        free_string(s1);
        return s2;
    }
    if (sempty(s2)) {
        free_string(s2);
        return s1;
    }
    len1 = slen(s1);
    len2 = slen(s2);
    s3 = new_string_len(NULL, len1 + len2);
    memcpy(s3->s, s1->s, len1);
    memcpy(s3->s + len1, s2->s, len2 + 1);
    free_string(s1);
    free_string(s2);
    return s3;
}

// XXX: should handle UTF-8
/* v1 and v2 are zero based offsets, v1 is incuded, v2 is excluded */
SCXMEM string_t *sub_string(SCXMEM string_t *s, int v1, int v2) {
    SCXMEM string_t *p;
    int len;

    if (!s)
        return NULL;

    len = slen(s);
    if (v2 >= len) {                /* past end */
        v2 = len;                   /* to end */
        if (v1 == 0)
            return dup_string(s);
    }
    if (v1 < 0 || v1 >= v2) {       /* out of range, return empty string */
        p = new_string("");
    } else {
        p = new_string_len(&s->s[v1], v2 - v1);
    }
    free_string(s);
    return p;
}

/*---------------- string utilities ----------------*/

/* truncating version of strcpy, returns truncated length */
size_t pstrcpy(char *dst, size_t dstsize, const char *src) {
    size_t i = 0;
    /* assumes non-overlapping buffers */
    if (dstsize--) {
        while (i < dstsize && (dst[i] = src[i]) != '\0')
            i++;
        dst[i] = '\0';
    }
    return i;
}

/* truncating version of memcpy, returns truncated length */
size_t pstrncpy(char *dst, size_t dstsize, const char *src, size_t len) {
    size_t i = 0;
    /* assumes non-overlapping buffers */
    if (dstsize--) {
        while (i < dstsize && len --> 0 && (dst[i] = src[i]) != '\0')
            i++;
        dst[i] = '\0';
    }
    return i;
}

/* truncating version of strcat, returns truncated length */
size_t pstrcat(char *dst, size_t dstsize, const char *src) {
    size_t i = 0, j = 0;
    /* assumes non-overlapping buffers */
    if (dstsize--) {
        while (i < dstsize && dst[i] != '\0')
            i++;
        while (i < dstsize && (dst[i] = src[j]) != '\0') {
            i++;
            j++;
        }
        dst[i] = '\0';
    }
    return i;
}

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
