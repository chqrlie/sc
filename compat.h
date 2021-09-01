/*      SC      A Table Calculator
 *              Compatibility definitions
 *
 *              by Charlie Gordon: August, 2021
 *
 *              $Revision: 8.1 $
 */

#ifndef _COMPAT_H
#define _COMPAT_H

#include <stddef.h>

#include "config.h"

#ifndef _XOPEN_SOURCE_EXTENDED
# define _XOPEN_SOURCE_EXTENDED
#endif

#ifdef SIGVOID
#define sigret_t void
#else
#define sigret_t int
#endif

#ifdef SYSV4
size_t strlen(const char *s);
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(strrchr)
#define strrchr rindex
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(strchr)
#define strchr index
#endif

#if (defined(BSD42) || defined(BSD43)) && !defined(ultrix)
#define memcpy(dest, source, len)   bcopy(source, dest, (unsigned int)(len))
#define memzero(dest, len)          bzero((dest), (unsigned int)(len))
#else
#include <memory.h>
#define memzero(dest, len)          memset(dest, 0, len)
#endif

#ifndef HAVE_ISFINITE
#define isfinite(v)  finite(v)
#endif

#endif /* _COMPAT_H */
