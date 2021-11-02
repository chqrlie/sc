/*      SC      A Table Calculator
 *              Curses support definitions
 *
 *              by Charlie Gordon: August, 2021
 *
 *              $Revision: 9.1 $
 */

#ifndef _COMPAT_H
#include "compat.h"
#endif

#if defined(HAVE_NCURSESW_CURSES_H)
# include <ncursesw/curses.h>
#elif defined(HAVE_NCURSES_CURSES_H)
# include <ncurses/curses.h>
#else
# include <curses.h>
#endif

#ifndef A_CHARTEXT      /* Should be defined in curses.h */
#define A_CHARTEXT 0xff
#endif

#if !defined(HAVE_ATTR_T)
typedef chtype attr_t;
#endif

#if !defined(HAVE_ATTR_GET) && !defined(NO_ATTR_GET)
#define attr_get(a, p, o)       ((void)((a) != 0 && (*(a) = stdscr->_attrs)), \
                                (void)((p) != 0 && \
                                (*(p) = PAIR_NUMBER(stdscr->_attrs))), OK)
#endif

#if defined BSD42 || defined SYSIII
# ifndef cbreak
# define cbreak      crmode
# define nocbreak    nocrmode
# endif
#endif

#ifdef BROKENCURSES    /* nl/nonl bug fix */
#undef nl
#undef nonl
#define nl()     (_tty.sg_flags |= CRMOD, _pfast = _rawmode, stty(_tty_ch, &_tty))
#define nonl()   (_tty.sg_flags &= ~CRMOD, _pfast = TRUE, stty(_tty_ch, &_tty))
#endif
