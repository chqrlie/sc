/*      SC      A Table Calculator
 *              Utility functions
 *
 *              Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <string.h>

#ifndef HAVE_STRLCPY
extern size_t strlcpy(char *dst, const char *src, size_t dstsize);
#endif
#ifndef HAVE_STRLCAT
extern size_t strlcat(char *dst, const char *src, size_t dstsize);
#endif
extern size_t strsplice(char *dst, size_t size, size_t from, size_t len1,
                        const char *src, size_t len2);
extern char *get_basename(const char *filename);
extern char *get_extension(const char *filename);
