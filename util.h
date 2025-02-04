/*      SC      A Table Calculator
 *              Utility functions
 *
 *              Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
 */

#include <stddef.h>  // size_t

/*---------------- Utility macros ----------------*/

#if defined(__GNUC__) || defined(__clang__) || defined(__TINYC__)
/* make sure that the keyword is not disabled by glibc (TINYC case) */
#define sc__attr_printf(a, b)  __attribute__((format(printf, a, b)))
#else
#define sc__attr_printf(a, b)
#endif

#if defined(__clang__) && __clang_major__ >= 10
#define FALLTHROUGH  __attribute__((fallthrough))
#else
#define FALLTHROUGH
#endif

#ifndef FALSE
# define        FALSE   0
# define        TRUE    1
#endif /* !FALSE */

/* macro to swap int variables: fails for expressions */
#define SWAPINT(x, y) do { int xy##__LINE__ = x; x = y; y = xy##__LINE__; } while (0)

#define countof(a)  (sizeof(a) / sizeof(*(a)))

/*---------------- Memory allocation ----------------*/

#define SCXMEM  /* flag allocated pointers with this */

extern size_t scxmem_count, scxmem_requested, scxmem_allocated, scxmem_overhead;

extern SCXMEM void *scxmalloc(size_t n);
extern SCXMEM void *scxrealloc(SCXMEM void *ptr, size_t n);
extern SCXMEM char *scxdup(const char *s);
extern void scxfree(SCXMEM void *p);
extern void scxmemdump(void);

/*---------------- string utilities ----------------*/

/* truncating version of strcpy, returns truncated length */
extern size_t pstrcpy(char *dst, size_t dstsize, const char *src);
extern size_t pstrncpy(char *dst, size_t dstsize, const char *src, size_t len);
/* truncating version of strcat, returns truncated length */
extern size_t pstrcat(char *dst, size_t dstsize, const char *src);
extern size_t strsplice(char *dst, size_t size, size_t from, size_t len1,
                        const char *src, size_t len2);
extern char *strtrim(char *s);
extern char *get_basename(const char *filename);
extern char *get_extension(const char *filename);

/*---------------- simple case handling ----------------*/

/* character class macros to avoid undefined behavior on negative chars */
#define isspacechar(c)   isspace((unsigned char)(c))
#define isdigitchar(c)   isdigit((unsigned char)(c))
#define isxdigitchar(c)  isxdigit((unsigned char)(c))
#define isalphachar(c)   isalpha((unsigned char)(c))
#define isalnumchar(c)   isalnum((unsigned char)(c))
#define islowerchar(c)   islower((unsigned char)(c))
#define isupperchar(c)   isupper((unsigned char)(c))
#define tolowerchar(c)   tolower((unsigned char)(c))
#define toupperchar(c)   toupper((unsigned char)(c))

static inline int isalphachar_(char c) { return isalphachar(c) || c == '_'; }
static inline int isalnumchar_(char c) { return isalnumchar(c) || c == '_'; }

extern int sc_strcasecmp(const char *a, const char *b);
extern int sc_strncasecmp(const char *a, const char *b, size_t n);
extern char *sc_strcasestr(const char *s1, const char *s2);

extern int utf8_decode(const char *s, int *wp);
extern int utf8_encode(char *s, int code);
extern int utf8_bcount(const char *s, int n);
extern int utf8_ccount(const char *s, int n);

/*---------------- refcounted string_t ----------------*/

typedef struct string_t {
    int refcount;
    short len;
#define STRING_ASCII     1
#define STRING_UTF8      2
#define STRING_RAW       4
#define STRING_ENCODING  7
    unsigned char encoding;
    unsigned char flags; /* for alignment */
    char s[1];
}  string_t;

void string_init(void);
void string_exit(void);

SCXMEM string_t *string_new(const char *s);
SCXMEM string_t *string_new_len(const char *s, size_t len, int encoding);
SCXMEM string_t *string_clone(SCXMEM string_t *str);
SCXMEM string_t *string_empty(void);
int string_get_encoding(string_t *str);

static inline int string_is_ascii(string_t *str) {
    return (str->encoding & STRING_ASCII) || (string_get_encoding(str) & STRING_ASCII);
}

static inline int string_is_utf8(string_t *str) {
    return (str->encoding & STRING_UTF8) || (string_get_encoding(str) & STRING_UTF8);
}

static inline SCXMEM string_t *string_dup(string_t *str) {
    if (str) str->refcount++;
    return str;
}

static inline void string_free(SCXMEM string_t *str) {
    if (str && !--str->refcount)
        scxfree(str);
}

static inline const char *s2c(const string_t *str) { return str->s; }
static inline const char *s2str(const string_t *str) { return str ? str->s : ""; }

static inline int slen(const string_t *str) { return str->len; }
static inline int sempty(const string_t *str) { return !str || !str->len; }

static inline void string_set(SCXMEM string_t **sp, SCXMEM string_t *str) {
    string_free(*sp);
    *sp = str;
}

SCXMEM string_t *string_concat(SCXMEM string_t *str1, SCXMEM string_t *str2);
SCXMEM string_t *string_mid(SCXMEM string_t *str, int pos, int n);
SCXMEM string_t *string_trim(SCXMEM string_t *str);
SCXMEM string_t *string_clean(SCXMEM string_t *str);
SCXMEM string_t *string_lower(SCXMEM string_t *str);
SCXMEM string_t *string_upper(SCXMEM string_t *str);
SCXMEM string_t *string_proper(SCXMEM string_t *str);
#define SF_IGNORE_CASE      1
#define SF_USE_CODE_POINTS  2
#define SF_FREE_STRINGS     4
int string_find(SCXMEM string_t *search, SCXMEM string_t *t, int pos, int flags);

/*---------------- char buffer utilities ----------------*/

/* buf_t structure to collect bufferized output */
typedef struct buf_t {
    char *buf;
    size_t size, len;
    int flags;
#define BUF_ALLOC  1
} buf_t[1];

/* define a fixed size buf_t object */
#define buf_t(name, size) \
    char name##__buf[size]; \
    buf_t name = {{ name##__buf, sizeof(name##__buf), 0, 0 }}

/* initialize a buffer with explicit array and size */
static inline void buf_init(buf_t buf, char *p, size_t size) {
    buf->buf = p;
    buf->size = size;
    buf->len = 0;
    buf->flags = 0;
}

static inline void buf_init2(buf_t buf, char *p, size_t size, size_t len) {
    buf->buf = p;
    buf->size = size;
    buf->len = len;
    buf->flags = 0;
}

/* clear the contents of a buffer */
static inline void buf_reset(buf_t buf) {
    buf->buf[buf->len = 0] = '\0';
}

/* free the contents of a buffer */
static inline void buf_free(buf_t buf) {
    if (buf->flags & BUF_ALLOC)
        scxfree(buf->buf);
}

/* write the contents of a buffer to a system file handle */
static inline ssize_t buf_write(buf_t buf, int fd) {
    ssize_t res = write(fd, buf->buf, buf->len);
    buf->len = 0;
    return res;
}

/* append a char to a buffer */
int buf_putc(buf_t buf, int c);

/* append count copies of char to a buffer */
int buf_repc(buf_t buf, int c, int count);

/* append a block of bytes to a buffer */
size_t buf_put(buf_t buf, const char *s, size_t len);

/* append a string to a buffer */
size_t buf_puts(buf_t buf, const char *s);

/* append a formated string to a buffer */
size_t buf_printf(buf_t buf, const char *fmt, ...) sc__attr_printf(2,3);

/* set buffer contents to block of bytes */
size_t buf_set(buf_t buf, const char *s, size_t len);

/* set buffer contents to a string */
size_t buf_sets(buf_t buf, const char *s);

/* set buffer contents to a formated string */
size_t buf_setf(buf_t buf, const char *fmt, ...) sc__attr_printf(2,3);

/* extend the buffer with scxmalloc or scxrealloc */
int buf_extend(buf_t buf, size_t size, size_t blocksize);

int buf_quotechar(buf_t buf, int c1, int c, int c2);
int buf_quotestr(buf_t buf, int c1, const char *s, int c2);
