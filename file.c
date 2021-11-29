/*      SC      A Spreadsheet Calculator
 *              Printing routines
 *
 *              original by James Gosling, September 1982
 *              modifications by Mark Weiser and Bruce Israel,
 *                      University of Maryland
 *              More mods Robert Bond, 12/86
 *              updated by Charlie Gordon: June, 2021
 *
 *              $Revision: 9.1 $
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

sheet_t *sheet_init(sheet_t *sp) {
    memset(sp, 0, sizeof(*sp));
    sp->autocalc = 1;
    sp->propagation = 10;
    sp->calc_order = BYROWS;
    sp->prescale = 1.0;
    sp->showtop = 1;
    sp->rowlimit = -1;
    sp->collimit = -1;
    sp->color = 1;
    sp->colorneg = 1;
    sp->colorerr = 1;
    sp->maxrow = sp->maxcol = -1;
    return sp;
}

static void write_options(sheet_t *sp, FILE *f) {
    if (sp->autocalc &&
        !sp->autoinsert &&
        !sp->autowrap &&
        !sp->cslop &&
        !sp->optimize &&
        !sp->rndtoeven &&
        sp->propagation == 10 &&
        sp->calc_order == BYROWS &&
        !sp->protect &&
        !sp->numeric &&
        sp->prescale == 1.0 &&
        !sp->extfunc &&
        sp->showtop &&
        !sp->tbl_style &&
        !sp->craction &&
        !sp->pagesize &&
        sp->rowlimit < 0 &&
        sp->collimit < 0 &&
        !sp->color &&
        !sp->colorneg &&
        !sp->colorerr
       )
        return;         /* No reason to do this */

    fprintf(f, "set");
    if (!sp->autocalc)  fprintf(f," !autocalc");
    if (sp->autoinsert) fprintf(f," autoinsert");
    if (sp->autowrap)   fprintf(f," autowrap");
    if (sp->cslop)      fprintf(f," cslop");
    if (sp->optimize)   fprintf(f," optimize");
    if (sp->rndtoeven)  fprintf(f, " rndtoeven");
    if (sp->propagation != 10)  fprintf(f, " iterations = %d", sp->propagation);
    if (sp->calc_order != BYROWS )  fprintf(f, " bycols");
    if (sp->protect)    fprintf(f, " protect");
    if (sp->numeric)    fprintf(f, " numeric");
    if (sp->prescale != 1.0)    fprintf(f, " prescale");
    if (sp->extfunc)    fprintf(f, " extfun");
    if (!sp->showtop)   fprintf(f, " !toprow");
    if (sp->tbl_style) {
        fprintf(f, " tblstyle = %s",
                sp->tbl_style == TBL ? "tbl" :
                sp->tbl_style == LATEX ? "latex" :
                sp->tbl_style == SLATEX ? "slatex" :
                sp->tbl_style == TEX ? "tex" :
                sp->tbl_style == FRAME ? "frame" : "0");
    }
    if (sp->craction)   fprintf(f, " craction = %d", sp->craction);
    if (sp->pagesize)   fprintf(f, " pagesize = %d", sp->pagesize);
    if (sp->rowlimit >= 0)  fprintf(f, " rowlimit = %d", sp->rowlimit);
    if (sp->collimit >= 0)  fprintf(f, " collimit = %d", sp->collimit);
    if (sp->color)      fprintf(f," color");
    if (sp->colorneg)   fprintf(f," colorneg");
    if (sp->colorerr)   fprintf(f," colorerr");
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

int cmd_plugin(sheet_t *sp, SCXMEM string_t *str) {
    char buf[PATHLEN];
    int res = -1;

    if (!sempty(str)) {
        snprintf(buf, sizeof buf, "|%s", s2c(str));
        res = readfile(sp, buf, 0);
    }
    string_free(str);
    return res;
}

#ifndef NOPLUGINS
/* add a plugin/mapping pair to the end of the filter list. type is
 * r(ead) or w(rite)
 */

void plugin_add(SCXMEM string_t *ext, SCXMEM string_t *plugin, char type) {
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

char *plugin_find(const char *ext, char type) {
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

void write_fd(sheet_t *sp, FILE *f, rangeref_t rr, int dcp_flags) {
    int r, c, i;

    fprintf(f, "# This data file was generated by the Spreadsheet Calculator.\n");
    fprintf(f, "# You almost certainly shouldn't edit it.\n\n");
    write_options(sp, f);
    abbrev_write(sp, f);
    for (i = 0; i < COLFORMATS; i++) {
        // XXX: what if sp->colformat is an empty string?
        if (sp->colformat[i])
            fprintf(f, "format %d = \"%s\"\n", i, s2c(sp->colformat[i]));
    }
    // XXX: should output column ranges
    for (c = rr.left.col; c <= rr.right.col; c++) {
        if (sp->colfmt[c].fwidth != DEFWIDTH || sp->colfmt[c].precision != DEFPREC || sp->colfmt[c].realfmt != DEFREFMT) {
            fprintf(f, "format %s %d %d %d\n",
                    coltoa(c), sp->colfmt[c].fwidth, sp->colfmt[c].precision, sp->colfmt[c].realfmt);
        }
    }
    for (c = rr.left.col; c <= rr.right.col; c++) {
        if (col_hidden(sp, c)) fprintf(f, "hide %s\n", coltoa(c));
    }
    // XXX: should output row ranges
    for (r = rr.left.row; r <= rr.right.row; r++) {
        if (row_hidden(sp, r)) fprintf(f, "hide %d\n", r);
    }
    nrange_write(sp, f);
    frange_write(sp, f);
    colors_write(sp, f, 0);
    crange_write(sp, f);

    // XXX: should encode strings?
    if (!sempty(sp->mdir)) fprintf(f, "mdir \"%s\"\n", s2c(sp->mdir));
    if (!sempty(sp->autorun)) fprintf(f, "autorun \"%s\"\n", s2c(sp->autorun));
    for (c = 0; c < FKEYS; c++) {
        if (!sempty(sp->fkey[c]))
            fprintf(f, "fkey %d = \"%s\"\n", c, s2c(sp->fkey[c]));
    }
    write_cells(sp, f, rr, rr.left, dcp_flags);
    // XXX: should output ranges of locked cells
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p && (p->flags & IS_LOCKED)) {
                fprintf(f, "lock %s\n", cell_addr(sp, cellref(r, c)));
            }
        }
    }
    // XXX: should clip range and apply offset
    note_write(sp, f);

    fprintf(f, "goto %s %s\n",
            cell_addr(sp, cellref_current(sp)),
            cell_addr(sp, cellref(sp->strow, sp->stcol)));
}

void write_cells(sheet_t *sp, FILE *f, rangeref_t rr, cellref_t cr, int dcp_flags) {
    buf_t(buf, FBUFLEN);
    int r, c;
    int deltar = cr.row - rr.left.row;
    int deltac = cr.col - rr.left.col;

    dcp_flags |= DCP_NO_LOCALE;
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = getcell(sp, r, c);
            if (p) {
                int row = r + deltar;
                int col = c + deltac;
                if (p->type || p->expr) {
                    edit_cell(sp, buf, row, col, p, deltar, deltac, dcp_flags, 0);
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
                    fprintf(f, "%s %s\n", command, cell_addr(sp, cellref(row, col)));
                }
                if (p->format) {
                    buf_setf(buf, "fmt %s ", cell_addr(sp, cellref(row, col)));
                    buf_quotestr(buf, '"', s2c(p->format), '"');
                    fprintf(f, "%s\n", buf->buf);
                }
            }
        }
    }
}

int writefile(sheet_t *sp, const char *fname, rangeref_t rr, int dcp_flags) {
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
        if ((plugin = plugin_find(p + 1, 'w')) != NULL) {
            size_t len;
            if (!plugin_exists(plugin, -1, save + 1, sizeof save - 1)) {
                error("plugin not found");
                return -1;
            }
            *save = '|';
            len = strlen(save);
            // XXX: should encode fname string
            if (snprintf(save + len, sizeof(save) - len, " %s \"%s\"",
                         range_addr(sp, rr), fname) >= PATHLEN) {
                error("Path too long");
                return -1;
            }
            /* pass it to readfile as an advanced macro */
            // XXX: does writefile pass to readfile?
            readfile(sp, save, 0);
            return 0;
        }
    }
#endif

#ifndef NOCRYPT
    if (Crypt) {
        return cwritefile(sp, fname, rr, dcp_flags);
    }
#endif /* NOCRYPT */

    if (*fname == '\0') {
        if (isatty(STDOUT_FILENO) || sp->curfile[0] != '\0') {
            fname = sp->curfile;
        } else {
            write_fd(sp, stdout, rr, dcp_flags);
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
    write_fd(sp, f, rr, dcp_flags);
    closefile(f, pid, 0);

    if (usecurses) {
        error("File \"%s\" written", save);
    }
    if (!pid) {
        pstrcpy(sp->curfile, sizeof sp->curfile, save);
        sp->modflg = 0;
        FullUpdate++;
    }
    return 0;
}

int readfile(sheet_t *sp, const char *fname, int eraseflg) {
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

    if (*fname == '*' && !sempty(sp->mdir)) {
        pstrcpy(save, sizeof save, s2c(sp->mdir));
        pstrcat(save, sizeof save, fname);
    } else {
        if (*fname == '\0')
            fname = sp->curfile;
        pstrcpy(save, sizeof save, fname);
    }

#ifndef NOPLUGINS
    if ((p = strrchr(fname, '.')) && (fname[0] != '|')) {  /* exclude macros */
        if ((plugin = plugin_find(p+1, 'r')) != NULL) {
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
            pstrcpy(sp->curfile, sizeof sp->curfile, p);
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
            ret = creadfile(sp, save, eraseflg);
        autolabel = tempautolabel;
        return ret;
    }
#endif /* NOCRYPT */

    if (eraseflg && strcmp(fname, sp->curfile) && modcheck(sp, " first"))
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
            } else {
                fprintf(stderr, "Reading file \"%s\"\n", save);
            }
        }
        erasedb(sp);
        checkbounds(sp, MINROWS, MINCOLS);
        load_scrc(sp);
    }

    remember(sp, 0);
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
    remember(sp, 1);

    closefile(f, pid, rfd);
    if (f == stdin) {
        freopen("/dev/tty", "r", stdin);
        screen_goraw();
    }
    if (eraseflg) {
        pstrcpy(sp->curfile, sizeof sp->curfile, save);
        sp->modflg = 0;
        if (!sempty(sp->autorun) && !skipautorun)
            readfile(sp, s2c(sp->autorun), 0);
        skipautorun = 0;
        EvalAll(sp);
        if (*save) {
            if (usecurses) {
                error("File \"%s\" loaded.", save);
                screen_refresh();
            } else {
                fprintf(stderr, "File \"%s\" loaded.\n", save);
            }
        }
    }
    autolabel = tempautolabel;
    FullUpdate++;
    return 1;
}

int load_scrc(sheet_t *sp) {
    char path[PATHLEN];
    char *home;
    int res = 0, fd;

    /*
     * Load $HOME/.scrc if present.
     */
    pstrcpy(path, sizeof path, "~/.scrc");
    if (findhome(path, sizeof path) && (fd = open(path, O_RDONLY)) >= 0) {
        close(fd);
        res = readfile(sp, path, 0);
    }

    /*
     * Load ./.scrc if present and $HOME/.scrc contained `set scrc'.
     */
    if (scrc && strcmp(home, getcwd(path, PATHLEN))
    &&  (fd = open(".scrc", O_RDONLY)) >= 0) {
        close(fd);
        res += readfile(sp, ".scrc", 0);
    }
    return res;
}
