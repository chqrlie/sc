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

static void print_options(FILE *f) {
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
        tbl_style == 0 &&
        craction == 0 &&
        pagesize == 0 &&
        rowlimit == -1 &&
        collimit == -1 &&
        !color &&
        !colorneg &&
        !colorerr
       )
        return;         /* No reason to do this */

    fprintf(f, "set");
    if (!autocalc)
        fprintf(f," !autocalc");
    if (autoinsert)
        fprintf(f," autoinsert");
    if (autowrap)
        fprintf(f," autowrap");
    if (cslop)
        fprintf(f," cslop");
    if (optimize)
        fprintf(f," optimize");
    if (rndtoeven)
        fprintf(f, " rndtoeven");
    if (propagation != 10)
        fprintf(f, " iterations = %d", propagation);
    if (calc_order != BYROWS )
        fprintf(f, " bycols");
    if (numeric)
        fprintf(f, " numeric");
    if (prescale != 1.0)
        fprintf(f, " prescale");
    if (extfunc)
        fprintf(f, " extfun");
    if (!showtop)
        fprintf(f, " !toprow");
    if (tbl_style) {
        fprintf(f, " tblstyle = %s",
                tbl_style == TBL ? "tbl" :
                tbl_style == LATEX ? "latex" :
                tbl_style == SLATEX ? "slatex" :
                tbl_style == TEX ? "tex" :
                tbl_style == FRAME ? "frame" : "0" );
    }
    if (craction)
        fprintf(f, " craction = %d", craction);
    if (pagesize)
        fprintf(f, " pagesize = %d", pagesize);
    if (rowlimit >= 0)
        fprintf(f, " rowlimit = %d", rowlimit);
    if (collimit >= 0)
        fprintf(f, " collimit = %d", collimit);
    if (color)
        fprintf(f," color");
    if (colorneg)
        fprintf(f," colorneg");
    if (colorerr)
        fprintf(f," colorerr");
    fprintf(f, "\n");
}

/* Open the input or output file, setting up a pipe if needed */
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

        efname = findhome(fname, fnamesiz);
        if (dobackups && rfd == NULL && !backup_file(efname) &&
            (yn_ask("Could not create backup copy.  Save anyway?: (y,n)") != 1))
            return 0;
        return fopen(efname, rfd == NULL ? "w" : "r");
    }

#ifdef NOPIPES
    error("Piping not available\n");
    return 0;
#else
    fname++;                            /* Skip | */
    fnamesiz--;
    efname = findhome(fname, fnamesiz);
    if (pipe(pipefd) < 0 || (rfd != NULL && pipe(pipefd+2) < 0)) {
        error("Can't make pipe to child");
        *rpid = 0;
        return 0;
    }

    deraw(rfd == NULL);
#ifdef VMS
    fprintf(stderr, "No son tasks available yet under VMS--sorry\n");
    return f;
#else /* VMS */
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
        exit (-127);
    } else {                     /* else parent */
        *rpid = pid;
        if ((f = fdopen(pipefd[(rfd == NULL ? 1 : 2)], rfd == NULL ? "w" : "r")) == NULL) {
            kill(pid, 9);
            error("Cannot fdopen for %s", rfd == NULL ? "output" : "input");
            close(pipefd[1]);
            if (rfd != NULL)
                close(pipefd[3]);
            *rpid = 0;
            return 0;
        }
    }
    close(pipefd[0]);
    if (rfd != NULL) {
        close(pipefd[3]);
        *rfd = pipefd[1];
    }
    return f;
#endif /* VMS */
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
            printf("Press any key to continue ");
            fflush(stdout);
            cbreak();
            nmgetch(0);
            goraw();
            clear();
        } else {
            close(rfd);
            if (usecurses) {
# ifdef VMS
                VMS_read_raw = 1;
# else /* VMS */
#  ifdef HAVE_FIXTERM
                fixterm();
#  else
                cbreak();
                nonl();
                noecho();
#  endif
                kbd_again();
# endif /* VMS */
                if (color && has_colors())
                    bkgdset(COLOR_PAIR(1) | ' ');
            }
        }
    }
#endif /* NOPIPES */
    if (brokenpipe) {
        error("Broken pipe");
        brokenpipe = FALSE;
    }
}


/* expand a ~ in a path to your home directory */
#ifndef VMS
#include <pwd.h>
#endif
char *findhome(char *path, size_t pathsiz) {
    static const char *HomeDir = NULL;

    if (*path == '~') {
        char *pathptr;
        char tmppath[PATHLEN];

        if (HomeDir == NULL) {
            HomeDir = getenv("HOME");
            if (HomeDir == NULL)
                HomeDir = "/";
        }
        pathptr = path + 1;
        if ((*pathptr == '/') || (*pathptr == '\0'))
            strlcpy(tmppath, HomeDir, sizeof tmppath);
#ifndef VMS
        else {
            struct passwd *pwent;
            char *namep;
            char name[50];

            namep = name;
            while ((*pathptr != '\0') && (*pathptr != '/'))
                    *namep++ = *pathptr++;
            *namep = '\0';
            if ((pwent = getpwnam(name)) == NULL) {
                error("Can't find user %s", name);
                return NULL;
            }
            strlcpy(tmppath, pwent->pw_dir, sizeof tmppath);
        }
#endif
        strlcat(tmppath, pathptr, sizeof tmppath);
        strlcpy(path, tmppath, pathsiz);
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

int cmd_plugin(const char *str) {
    char buf[PATHLEN];
    snprintf(buf, sizeof buf, "|%s", str);
    return readfile(buf, 0);
}

#ifndef NOPLUGINS
/* add a plugin/mapping pair to the end of the filter list. type is
 * r(ead) or w(rite)
 */

void addplugin(const char *ext, const char *plugin, char type) {
    struct impexfilt *fp;
    char mesg[PATHLEN];

    if (!plugin_exists(plugin, -1, mesg, sizeof mesg)) {
        error("Cannot find plugin %s", plugin);
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
    strlcpy(fp->plugin, plugin, PATHLEN);
    strlcpy(fp->ext, ext, PATHLEN);
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

void write_fd(FILE *f, rangeref_t rr) {
    int r, c;

    fprintf(f, "# This data file was generated by the Spreadsheet ");
    fprintf(f, "Calculator.\n");
    fprintf(f, "# You almost certainly shouldn't edit it.\n\n");
    print_options(f);
    write_abbrevs(f);
    for (c = 0; c < COLFORMATS; c++) {
        if (colformat[c])
            fprintf(f, "format %d = \"%s\"\n", c, colformat[c]);
    }
    for (c = rr.left.col; c <= rr.right.col; c++) {
        if (fwidth[c] != DEFWIDTH || precision[c] != DEFPREC || realfmt[c] != DEFREFMT) {
            fprintf(f, "format %s %d %d %d\n",
                    coltoa(c), fwidth[c], precision[c], realfmt[c]);
        }
    }
    for (c = rr.left.col; c <= rr.right.col; c++) {
        if (col_hidden[c])
            fprintf(f, "hide %s\n", coltoa(c));
    }
    for (r = rr.left.row; r <= rr.right.row; r++) {
        if (row_hidden[r])
            fprintf(f, "hide %d\n", r);
    }
    write_nranges(f);
    write_franges(f);
    write_colors(f, 0);
    write_cranges(f);

    if (mdir)
        fprintf(f, "mdir \"%s\"\n", mdir);
    if (autorun)
        fprintf(f, "autorun \"%s\"\n", autorun);
    for (c = 0; c < FKEYS; c++) {
        if (fkey[c])
            fprintf(f, "fkey %d = \"%s\"\n", c + 1, fkey[c]);
    }
    write_cells(f, rr, rr.left);
    for (r = rr.left.row; r <= rr.right.row; r++) {
        for (c = rr.left.col; c <= rr.right.col; c++) {
            struct ent *p = *ATBL(tbl, r, c);
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

void write_cells(FILE *f, rangeref_t rr, cellref_t cr) {
    buf_t(buf, FBUFLEN);
    int r, c, mf;
    int rs = 0;
    int cs = 0;
    int dr = cr.row;
    int dc = cr.col;
    int r0 = rr.left.row;
    int c0 = rr.left.col;
    int rn = rr.right.row;
    int cn = rr.right.col;

    mf = modflg;
    if (dr != r0 || dc != c0) {
        yank_area(r0, c0, rn, cn);
        rn += dr - r0;
        cn += dc - c0;
        rs = currow;
        cs = curcol;
        currow = dr;
        curcol = dc;
        pullcells('x');
    }
    if (Vopt) valueize_area(rangeref(dr, dc, rn, cn));
    for (r = dr; r <= rn; r++) {
        for (c = dc; c <= cn; c++) {
            struct ent *p = *ATBL(tbl, r, c);
            if (p) {
                if (p->label || (p->flags & IS_STREXPR)) {
                    edits(buf, r, c, p);
                    fprintf(f, "%s\n", buf->buf);
                }
                if (p->flags & IS_VALID) {
                    editv(buf, r, c, p);
#if 0
                    // XXX: this ugly hack will patch the value
                    //      but only a single match in the formula
                    //      which may not even be a number!
                    //      should pass localisation context to
                    //      conversion function
                    if (dpoint != '.') {
                        char *dpointptr = strchr(buf->buf, dpoint);
                        if (dpointptr != NULL)
                            *dpointptr = '.';
                    }
#endif
                    fprintf(f, "%s\n", buf->buf);
                }
                if (p->format) {
                    buf_setf(buf, "fmt %s ", v_name(r, c));
                    buf_quotestr(buf, '"', p->format, '"');
                    fprintf(f, "%s\n", buf->buf);
                }
            }
        }
    }
    if (dr != r0 || dc != c0) {
        pullcells('x');
        currow = rs;
        curcol = cs;
        flush_saved();
    }
    modflg = mf;
}

int writefile(const char *fname, rangeref_t rr) {
    FILE *f;
    char save[PATHLEN];
    char tfname[PATHLEN];
    char *tpp;
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
        return cwritefile(fname, rr);
    }
#endif /* NOCRYPT */

    if (*fname == '\0') {
        if (isatty(STDOUT_FILENO) || *curfile != '\0') {
            fname = curfile;
        } else {
            write_fd(stdout, rr);
            return 0;
        }
    }

    /* copy the string, strip the \ in front of " */
    for (tpp = tfname, p = fname; *p; p++) {
        if (*p == '\\' && p[1] == '"')
            p++;
        *tpp++ = *p;
    }
    *tpp = '\0';
    ext = get_extension(tfname);
    if (scext != NULL) {
        if (!strcmp(ext, ".sc") || (scext && !strcmp(ext, scext)))
            *ext = '\0';
        strlcat(tfname, ".", sizeof tfname);
        strlcat(tfname, scext, sizeof tfname);
    }

    strlcpy(save, tfname, sizeof save);
    for (tpp = save; *tpp != '\0'; tpp++) {
        if (*tpp == '"') {
            strsplice(save, sizeof save, tpp - save, 0, "\\", 1);
            tpp++;
        }
    }
    if ((f = openfile(tfname, sizeof tfname, &pid, NULL)) == NULL) {
        error("Can't create file \"%s\"", save);
        return -1;
    }

    if (usecurses) {
        error("Writing file \"%s\"...", save);
        refresh();
    }
    write_fd(f, rr);

    closefile(f, pid, 0);

    if (!pid) {
        strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
        FullUpdate++;
        if (usecurses) {
            error("File \"%s\" written", curfile);
        } else
            fprintf(stderr, "\nFile \"%s\" written", curfile);
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

    if (*fname == '*' && mdir) {
        strlcpy(save, mdir, sizeof save);
        strlcat(save, fname, sizeof save);
    } else {
        if (*fname == '\0')
            fname = curfile;
        strlcpy(save, fname, sizeof save);
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
            strlcpy(curfile, p, sizeof curfile);
        }
    }
#endif

#ifndef NOCRYPT
    if (Crypt) {
        int ret = 0;
        if (*save == '-' && strlen(fname) == 1)
            error("Can't use encryption in a pipeline.");
        else
        if (*save == '|')
            error("Can't use encryption with advanced macros.");
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
        if ((f = openfile(save, sizeof save, &pid, &rfd)) == NULL) {
            error("Can't read file \"%s\"", save);
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
                refresh();
            } else
                fprintf(stderr, "Reading file \"%s\"\n", save);
        }
        erasedb();
    }

    remember(0);
    loading++;
    savefd = macrofd;
    macrofd = rfd;
    // XXX: should use a local buffer
    while (!brokenpipe && fgets(buf, sizeof(buf), f)) {
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
        goraw();
    }
    //linelim = -1;
    if (eraseflg) {
        strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
        cellassign = 0;
        if (autorun && !skipautorun) readfile(autorun, 0);
        skipautorun = 0;
        EvalAll();
        if (*save) {
            if (usecurses) {
                error("File \"%s\" loaded.", save);
                refresh();
            } else
                fprintf(stderr, "File \"%s\" loaded.\n", save);
        }
    }
    autolabel = tempautolabel;
    FullUpdate++;
    return 1;
}
