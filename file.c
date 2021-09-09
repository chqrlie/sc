/*      SC      A Spreadsheet Calculator
 *              Printing routines
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 8.1 $
 */

#include <sys/wait.h>
#include <time.h>
#include <utime.h>
#include <sys/file.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <signal.h>
#include "sc.h"

int macrofd;
static struct impexfilt *filt = NULL; /* root of list of impex filters */

static void write_options(FILE *f) {
    if (autocalc &&
        !autoinsert &&
        !autowrap &&
        !cslop &&
        !optimize &&
        !rndtoeven &&
        propagation == 10 &&
        calc_order == BYROWS &&
        !numeric &&
        prescale == 1.0 &&
        !extfunc &&
        showtop &&
        !tbl_style &&
        !craction &&
        !pagesize &&
        rowlimit < 0 &&
        collimit < 0 &&
        !color &&
        !colorneg &&
        !colorerr
       )
        return;         /* No reason to do this */

    fprintf(f, "set");
    if (!autocalc)  fprintf(f," !autocalc");
    if (autoinsert) fprintf(f," autoinsert");
    if (autowrap)   fprintf(f," autowrap");
    if (cslop)      fprintf(f," cslop");
    if (optimize)   fprintf(f," optimize");
    if (rndtoeven)  fprintf(f, " rndtoeven");
    if (propagation != 10)  fprintf(f, " iterations = %d", propagation);
    if (calc_order != BYROWS )  fprintf(f, " bycols");
    if (numeric)    fprintf(f, " numeric");
    if (prescale != 1.0)    fprintf(f, " prescale");
    if (extfunc)    fprintf(f, " extfun");
    if (!showtop)   fprintf(f, " !toprow");
    if (tbl_style) {
        fprintf(f, " tblstyle = %s",
                tbl_style == TBL ? "tbl" :
                tbl_style == LATEX ? "latex" :
                tbl_style == SLATEX ? "slatex" :
                tbl_style == TEX ? "tex" :
                tbl_style == FRAME ? "frame" : "0");
    }
    if (craction)   fprintf(f, " craction = %d", craction);
    if (pagesize)   fprintf(f, " pagesize = %d", pagesize);
    if (rowlimit >= 0)  fprintf(f, " rowlimit = %d", rowlimit);
    if (collimit >= 0)  fprintf(f, " collimit = %d", collimit);
    if (color)      fprintf(f," color");
    if (colorneg)   fprintf(f," colorneg");
    if (colorerr)   fprintf(f," colorerr");
    fprintf(f, "\n");
}

/* Open the input or output file, setting up a pipe if needed
   open for output rfd == NULL and input otherwise */
FILE *openfile(char *fname, size_t fnamesiz, int *rpid, int *rfd) {
    int pipefd[4];
    int pid;
    FILE *f;
    char *efname;

    while (*fname == ' ') { /* Skip leading blanks */
        fname++;
        fnamesiz--;
    }

    if (*fname != '|') {                /* Open file if not pipe */
        *rpid = 0;
        if (rfd != NULL)
            *rfd = 1;                   /* Set to stdout just in case */

        if (!(efname = findhome(fname, fnamesiz)))
            return NULL;
        if (dobackups && rfd == NULL && !backup_file(efname) &&
            (yn_ask("Could not create backup copy.  Save anyway?: (y,n)") != 1))
            return NULL;
        return fopen(efname, rfd == NULL ? "w" : "r");
    }

#ifdef NOPIPES
    error("Piping not available\n");
    return NULL;
#else
    fname++;                            /* Skip | */
    fnamesiz--;
    if (!(efname = findhome(fname, fnamesiz)))
        return NULL;
    if (pipe(pipefd) < 0 || (rfd != NULL && pipe(&pipefd[2]) < 0)) {
        error("Cannot make pipe to child");
        *rpid = 0;
        return NULL;
    }

    screen_deraw(rfd == NULL);
    if ((pid = fork()) == 0) {   /* if child */
        close(0);                /* close stdin */
        close(pipefd[1]);
        dup(pipefd[0]);          /* connect to first pipe */
        if (rfd != NULL) {       /* if opening for read */
            close(1);            /* close stdout */
            close(pipefd[2]);
            dup(pipefd[3]);      /* connect to second pipe */
        }
        signal(SIGINT, SIG_DFL); /* reset */
        execl("/bin/sh", "sh", "-c", efname, (char *)NULL);
        exit(-127);
    } else {                     /* else parent */
        *rpid = pid;
        if ((f = fdopen(pipefd[(rfd == NULL ? 1 : 2)], rfd == NULL ? "w" : "r")) == NULL) {
            kill(pid, 9);
            error("Cannot fdopen for %s", rfd == NULL ? "output" : "input");
            close(pipefd[1]);
            if (rfd != NULL)
                close(pipefd[3]);
            *rpid = 0;
            return NULL;
        }
    }
    close(pipefd[0]);
    if (rfd != NULL) {
        close(pipefd[3]);
        *rfd = pipefd[1];
    }
    return f;
#endif /* NOPIPES */
}

/* close a file opened by openfile(), if process wait for return */
void closefile(FILE *f, int pid, int rfd) {
    int temp;

    if (fclose(f) == EOF) {
        error("fclose(): %s", strerror(errno));
    }
#ifndef NOPIPES
    if (pid) {
        while (pid != wait(&temp))
            continue;
        if (rfd == 0) {
            screen_pause();
            screen_goraw();
            screen_erase();
        } else {
            close(rfd);
            screen_goraw();
        }
    }
#endif /* NOPIPES */
    if (brokenpipe) {
        error("Broken pipe");
        brokenpipe = FALSE;
    }
}

/* expand a ~ in a path to your home directory */
#ifndef NOGETPWNAM
#include <pwd.h>
#endif
char *findhome(char *path, size_t pathsiz) {
    // XXX: should get rid of static variable HomeDir
    static const char *HomeDir = NULL;

    if (*path == '~') {
        // XXX: should use strsplice()
        char tmppath[PATHLEN];
        char *pathptr = path + 1;
        if (*pathptr == '/' || *pathptr == '\0') {
            if (HomeDir == NULL) {
                HomeDir = getenv("HOME");
                if (HomeDir == NULL)
                    HomeDir = "/";
            }
            pstrcpy(tmppath, sizeof tmppath, HomeDir);
        } else {
#ifndef NOGETPWNAM
            struct passwd *pwent;
            char *namep;
            char name[50];

            namep = name;
            while (namep < name + sizeof(name) - 1 && (*pathptr != '\0') && (*pathptr != '/'))
                *namep++ = *pathptr++;
            *namep = '\0';
            if ((pwent = getpwnam(name)) == NULL) {
                error("Cannot find user %s", name);
                return NULL;
            }
            pstrcpy(tmppath, sizeof tmppath, pwent->pw_dir);
#else
            *tmppath = '\0';
#endif
        }
        pstrcat(tmppath, sizeof tmppath, pathptr);
        pstrcpy(path, pathsiz, tmppath);
    }
    return path;
}

/*
 * make a backup copy of a file, use the same mode and name in the format
 * [path/]file~
 * return 1 if we were successful, 0 otherwise
 */
int backup_file(const char *path) {
    struct stat statbuf;
    struct utimbuf timebuf;
    char tpath[PATHLEN];
    char sbuf[BUFSIZ];
    char *buf = sbuf;
    size_t buflen = sizeof buf;
    int infd, outfd, rc = 1;
    int count, wpos, wc;
    mode_t oldumask;

    /* tpath will be the [path/]file ---> [path/]file~ */
    if (snprintf(tpath, sizeof tpath, "%s~", path) >= (int)(sizeof tpath))
        return 0;

    if (stat(path, &statbuf))
        return (errno == ENOENT);

    if ((infd = open(path, O_RDONLY, 0)) < 0)
        return 0;

    // XXX: if path is read-only, open for writing might fail
    oldumask = umask(0);
    outfd = open(tpath, O_TRUNC | O_WRONLY | O_CREAT, statbuf.st_mode);
    umask(oldumask);
    if (outfd < 0) {
        close(infd);
        return 0;
    }
    /* if we know the optimum block size, use it */
    if ((int)buflen < (int)statbuf.st_blksize) {
        buflen = (int)statbuf.st_blksize;
        if ((buf = scxmalloc(buflen)) == NULL) {
            buf = sbuf;
            buflen = sizeof sbuf;
        }
    }
    chown(tpath, statbuf.st_uid, statbuf.st_gid);

    rc = 1;
    while (rc) {
        count = read(infd, buf, buflen);
        if (count <= 0) {
            if (count < 0) {
                if (errno == EINTR)
                    continue;
                rc = 0;
            }
            break;
        }
        for (wpos = 0; wpos < count; wpos += wc) {
            wc = write(outfd, buf + wpos, count - wpos);
            if (wc <= 0) {
                if (wc < 0) {
                    wc = 0;
                    if (errno == EINTR)
                        continue;
                }
                rc = 0;
                break;
            }
        }
    }
    if (buf != sbuf)
        scxfree(buf);
    close(infd);
    close(outfd);
    if (rc) {
        /* copy access and modification times from original file */
        timebuf.actime = statbuf.st_atime;
        timebuf.modtime = statbuf.st_mtime;
        utime(tpath, &timebuf);
    } else {
        unlink(tpath);
    }
    return rc;
}

int cmd_plugin(SCXMEM string_t *str) {
    char buf[PATHLEN];
    int res = -1;

    if (!sempty(str)) {
        snprintf(buf, sizeof buf, "|%s", s2c(str));
        res = readfile(buf, 0);
    }
    string_free(str);
    return res;
}

#ifndef NOPLUGINS
/* add a plugin/mapping pair to the end of the filter list. type is
 * r(ead) or w(rite)
 */

void add_plugin(SCXMEM string_t *ext, SCXMEM string_t *plugin, char type) {
    struct impexfilt *fp;
    char mesg[PATHLEN];

    if (!plugin_exists(s2c(plugin), -1, mesg, sizeof mesg)) {
        error("Cannot find plugin %s", s2c(plugin));
        string_free(ext);
        string_free(plugin);
        return;
    }
    if (filt == NULL) {
        filt = scxmalloc(sizeof(struct impexfilt));
        fp = filt;
    } else {
        fp = filt;
        while (fp->next != NULL)
            fp = fp->next;
        fp->next = scxmalloc(sizeof(struct impexfilt));
        fp = fp->next;
    }
    // XXX: should use string_t
    pstrcpy(fp->plugin, PATHLEN, s2c(plugin));
    pstrcpy(fp->ext, PATHLEN, s2c(ext));
    string_free(ext);
    string_free(plugin);
    fp->type = type;
    fp->next = NULL;
}

char *findplugin(const char *ext, char type) {
    struct impexfilt *fp;

    fp = filt;
    if (fp == NULL)
        return NULL;
    if ((!strcmp(fp->ext, ext)) && (fp->type == type))
        return fp->plugin;
    while (fp->next != NULL) {
        fp = fp->next;
        if ((!strcmp(fp->ext, ext)) && (fp->type == type))
            return fp->plugin;
    }

    return NULL;
}
#endif /* NOPLUGINS */

void write_fd(FILE *f, rangeref_t rr, int dcp_flags) {
    int r, c;

    fprintf(f, "# This data file was generated by the Spreadsheet ");
    fprintf(f, "Calculator.\n");
    fprintf(f, "# You almost certainly shouldn't edit it.\n\n");
    write_options(f);
    write_abbrevs(f);
    for (c = 0; c < COLFORMATS; c++) {
        // XXX: what if colformat is empty?
        if (colformat[c])
            fprintf(f, "format %d = \"%s\"\n", c, s2c(colformat[c]));
    }
    for (c = rr.left.col; c <= rr.right.col; c++) {
        if (fwidth[c] != DEFWIDTH || precision[c] != DEFPREC || realfmt[c] != DEFREFMT) {
            fprintf(f, "format %s %d %d %d\n",
                    coltoa(c), fwidth[c], precision[c], realfmt[c]);
        }
    }
    for (c = rr.left.col; c <= rr.right.col; c++) {
        if (col_hidden[c]) fprintf(f, "hide %s\n", coltoa(c));
    }
    for (r = rr.left.row; r <= rr.right.row; r++) {
        if (row_hidden[r]) fprintf(f, "hide %d\n", r);
    }
    write_nranges(f);
    write_franges(f);
    write_colors(f, 0);
    write_cranges(f);

    // XXX: should encode strings?
    if (!sempty(mdir)) fprintf(f, "mdir \"%s\"\n", s2c(mdir));
    if (!sempty(autorun)) fprintf(f, "autorun \"%s\"\n", s2c(autorun));
    for (c = 0; c < FKEYS; c++) {
        if (!sempty(fkey[c]))
            fprintf(f, "fkey %d = \"%s\"\n", c, s2c(fkey[c]));
    }
    write_cells(f, rr, rr.left, dcp_flags);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sht, r, c);
            if (p) {
                if (p->flags & IS_LOCKED) {
                    fprintf(f, "lock %s%d\n", coltoa(p->col), p->row);
                }
                if (p->flags & HAS_NOTE) {
                    fprintf(f, "addnote %s %s\n",
                            v_name(p->row, p->col),
                            r_name(p->nrr.left.row, p->nrr.left.col,
                                   p->nrr.right.row, p->nrr.right.col));
                }
            }
        }
    }
    fprintf(f, "goto %s %s\n", v_name(currow, curcol), v_name(strow, stcol));
}

void write_cells(FILE *f, rangeref_t rr, cellref_t cr, int dcp_flags) {
    buf_t(buf, FBUFLEN);
    int r, c;

    dcp_flags |= DCP_NO_LOCALE;
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sht, r, c);
            if (p) {
                int row = r + cr.row - rr.left.row;
                int col = c + cr.col - rr.left.col;
                if (p->type || p->expr) {
                    edit_cell(buf, row, col, p, dcp_flags, 0);
                    fprintf(f, "%s\n", buf->buf);
                } else
                if ((p->flags & ALIGN_MASK) != ALIGN_DEFAULT) {
                    const char *command = NULL;
                    switch (p->flags & ALIGN_MASK) {
                    default:
                    case ALIGN_LEFT:    command = "leftjustify";  break;
                    case ALIGN_RIGHT:   command = "rightjustify"; break;
                    case ALIGN_CENTER:  command = "center";       break;
                    }
                    fprintf(f, "%s %s\n", command, v_name(row, col));
                }
                if (p->format) {
                    buf_setf(buf, "fmt %s ", v_name(row, col));
                    buf_quotestr(buf, '"', s2c(p->format), '"');
                    fprintf(f, "%s\n", buf->buf);
                }
            }
        }
    }
}

int writefile(const char *fname, rangeref_t rr, int dcp_flags) {
    char save[PATHLEN];
    char tfname[PATHLEN];
    FILE *f;
    const char *p;
    char *ext;
    char *plugin;
    int pid;

#ifndef NOPLUGINS
    /* find the extension and mapped plugin if exists */
    p = get_extension(fname);
    if (*p) {
        if ((plugin = findplugin(p + 1, 'w')) != NULL) {
            size_t len;
            if (!plugin_exists(plugin, -1, save + 1, sizeof save - 1)) {
                error("plugin not found");
                return -1;
            }
            *save = '|';
            len = strlen(save);
            if (snprintf(save + len, sizeof(save) - len, " %s%d:%s%d \"%s\"",
                         coltoa(rr.left.col), rr.left.row,
                         coltoa(rr.right.col), rr.right.row, fname) >= PATHLEN) {
                error("Path too long");
                return -1;
            }
            /* pass it to readfile as an advanced macro */
            // XXX: does writefile pass to readfile?
            readfile(save, 0);
            return 0;
        }
    }
#endif

#ifndef NOCRYPT
    if (Crypt) {
        return cwritefile(fname, rr, dcp_flags);
    }
#endif /* NOCRYPT */

    if (*fname == '\0') {
        if (isatty(STDOUT_FILENO) || *curfile != '\0') {
            fname = curfile;
        } else {
            write_fd(stdout, rr, dcp_flags);
            return 0;
        }
    }

    pstrcpy(tfname, sizeof tfname, fname);
    // XXX: extension should determine file format: sc, xls, xlsx, csv
    if (scext != NULL) {
        ext = get_extension(tfname);
        if (!strcmp(ext, ".sc") || !strcmp(ext, s2c(scext)))
            *ext = '\0';
        pstrcat(tfname, sizeof tfname, ".");
        pstrcat(tfname, sizeof tfname, s2c(scext));
    }
    pstrcpy(save, sizeof save, tfname);

    // XXX: should pass Crypt flag
    if ((f = openfile(tfname, sizeof tfname, &pid, NULL)) == NULL) {
        error("Cannot create file \"%s\"", save);
        return -1;
    }

    if (usecurses) {
        error("Writing file \"%s\"...", save);
        screen_refresh();
    }
    write_fd(f, rr, dcp_flags);
    closefile(f, pid, 0);

    if (usecurses) {
        error("File \"%s\" written", save);
    }
    if (!pid) {
        pstrcpy(curfile, sizeof curfile, save);
        modflg = 0;
        FullUpdate++;
    }
    return 0;
}

int readfile(const char *fname, int eraseflg) {
    FILE *f;
    char save[PATHLEN];
    char buf[FBUFLEN];
    int tempautolabel;
    char *p;
    char *plugin;
    int pid = 0;
    int rfd = STDOUT_FILENO, savefd;

    tempautolabel = autolabel;          /* turn off auto label when */
    autolabel = 0;                      /* reading a file */

    if (*fname == '*' && !sempty(mdir)) {
        pstrcpy(save, sizeof save, s2c(mdir));
        pstrcat(save, sizeof save, fname);
    } else {
        if (*fname == '\0')
            fname = curfile;
        pstrcpy(save, sizeof save, fname);
    }

#ifndef NOPLUGINS
    if ((p = strrchr(fname, '.')) && (fname[0] != '|')) {  /* exclude macros */
        if ((plugin = findplugin(p+1, 'r')) != NULL) {
            size_t l;
            if (!plugin_exists(plugin, -1, save + 1, sizeof save - 1)) {
                error("plugin not found");
                return 0;
            }
            *save = '|';
            if ((strlen(save) + strlen(fname) + 2) > PATHLEN) {
                error("Path too long");
                return 0;
            }
            l = strlen(save);
            snprintf(save + l, sizeof(save) - l, " \"%s\"", fname);
            eraseflg = 0;
            /* get filename: could be preceded by params if this is a save */
            while (p > fname) {
                if (*p == ' ') {
                    p++;
                    break;
                }
                p--;
            }
            pstrcpy(curfile, sizeof curfile, p);
        }
    }
#endif

#ifndef NOCRYPT
    if (Crypt) {
        int ret = 0;
        if (*save == '-' && strlen(fname) == 1)
            error("Cannot use encryption in a pipeline.");
        else
        if (*save == '|')
            error("Cannot use encryption with advanced macros.");
        else
            ret = creadfile(save, eraseflg);
        autolabel = tempautolabel;
        return ret;
    }
#endif /* NOCRYPT */

    if (eraseflg && strcmp(fname, curfile) && modcheck(" first"))
        return 0;

    if (fname[0] == '-' && fname[1] == '\0') {
        f = stdin;
        *save = '\0';
    } else {
        // XXX: should pass a flag to invoke crypt
        if ((f = openfile(save, sizeof save, &pid, &rfd)) == NULL) {
            error("Cannot read file \"%s\"", save);
            autolabel = tempautolabel;
            return 0;
        }
    }
    if (*fname == '|')
        *save = '\0';

    if (eraseflg) {
        if (*save) {
            if (usecurses) {
                error("Reading file \"%s\"", save);
                screen_refresh();
            } else
                fprintf(stderr, "Reading file \"%s\"\n", save);
        }
        erasedb(TRUE);
    }

    remember(0);
    loading++;
    savefd = macrofd;
    macrofd = rfd;
    while (!brokenpipe && fgets(buf, sizeof buf, f)) {
        p = buf;
        if (*p == '|' && pid != 0) {
            *p = ' ';
        } else {
            while (*p == ' ') {
                /* skip initial blanks */
                p++;
            }
            if (*p == '#' || *p == '\0' || *p == '\n') {
                /* ignore comments and blank lines */
                continue;
            }
        }
        parse_line(buf);
    }
    macrofd = savefd;
    --loading;
    remember(1);

    closefile(f, pid, rfd);
    if (f == stdin) {
        freopen("/dev/tty", "r", stdin);
        screen_goraw();
    }
    if (eraseflg) {
        pstrcpy(curfile, sizeof curfile, save);
        modflg = 0;
        if (!sempty(autorun) && !skipautorun)
            readfile(s2c(autorun), 0);
        skipautorun = 0;
        EvalAll();
        if (*save) {
            if (usecurses) {
                error("File \"%s\" loaded.", save);
                screen_refresh();
            } else
                fprintf(stderr, "File \"%s\" loaded.\n", save);
        }
    }
    autolabel = tempautolabel;
    FullUpdate++;
    return 1;
}
