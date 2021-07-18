/*      SC      A Table Calculator
 *              Utility functions
 *
 *              Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
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
#define SWAPINT(x, y) do { int x##__##y = x; x = y; y = x##__##y; } while (0)

/*---------------- Memory allocation ----------------*/

#define SCXMEM  /* flag allocated pointers with this */

extern SCXMEM void *scxmalloc(size_t n);
extern SCXMEM void *scxrealloc(SCXMEM void *ptr, size_t n);
extern SCXMEM char *scxdup(const char *s);
extern void scxfree(SCXMEM void *p);

/*---------------- string utilities ----------------*/

/* truncating version of strcpy, returns truncated length */
extern size_t pstrcpy(char *dst, size_t dstsize, const char *src);
extern size_t pstrncpy(char *dst, size_t dstsize, const char *src, size_t len);
/* truncating version of strcat, returns truncated length */
extern size_t pstrcat(char *dst, size_t dstsize, const char *src);
extern size_t strsplice(char *dst, size_t size, size_t from, size_t len1,
                        const char *src, size_t len2);
extern char *get_basename(const char *filename);
extern char *get_extension(const char *filename);

/*---------------- refcounted string_t ----------------*/

typedef struct string_t {
    int refcount;
    int len;
    char s[1];
}  string_t;

string_t *new_string(const char *s);
string_t *new_string_len(const char *s, size_t len);

static inline string_t *dup_string(string_t *str) {
    if (str) str->refcount++;
    return str;
}

static inline void free_string(string_t *str) {
    if (str && !--str->refcount)
        scxfree(str);
}

static inline const char *s2c(const string_t *str) { return str->s; }
static inline const char *s2str(const string_t *str) { return str ? str->s : ""; }

static inline int slen(const string_t *str) { return str->len; }
static inline int sempty(const string_t *str) { return !str || !str->len; }

static inline void set_string(SCXMEM string_t **sp, SCXMEM string_t *str) {
    free_string(*sp);
    *sp = str;
}

SCXMEM string_t *cat_strings(SCXMEM string_t *s1, SCXMEM string_t *s2);
SCXMEM string_t *sub_string(SCXMEM string_t *, int v1_included, int v2_excluded);

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
