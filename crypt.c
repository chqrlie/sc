/*
 * Encryption utilites
 * Bradley Williams
 * {allegra,ihnp4,uiucdcs,ctvax}!convex!williams
 * $Revision: 8.1 $
 */

#include <sys/file.h>
#include <fcntl.h>
#include "sc.h"

#ifndef NOCRYPT

int Crypt = 0;
#define MAXKEYWORDSIZE 30
char KeyWord[MAXKEYWORDSIZE] = { "" };

int creadfile(const char *fname, int eraseflg) {
    char save[FBUFLEN];
    char buf[FBUFLEN];
    FILE *f;
    int pipefd[2];
    int fildes;
    int pid;
    char *p;

    pstrcpy(save, sizeof save, fname);

    if (eraseflg && strcmp(fname, curfile) && modcheck(" first"))
        return 0;

    if ((fildes = open(findhome(save, sizeof save), O_RDONLY, 0)) < 0) {
        error("Cannot read file \"%s\"", save);
        return 0;
    }

    if (eraseflg) erasedb(TRUE);

    if (pipe(pipefd) < 0) {
        error("Cannot make pipe to child");
        return 0;
    }

    screen_deraw(1);
    pstrcpy(KeyWord, sizeof KeyWord, getpass("Enter key:"));
    screen_goraw();

    if ((pid = fork()) == 0) {   /* if child              */
        close(0);                /* close stdin           */
        close(1);                /* close stdout          */
        close(pipefd[0]);        /* close pipe input      */
        dup(fildes);             /* standard in from file */
        dup(pipefd[1]);          /* connect to pipe       */
        fprintf(stderr, " ");
        execl(CRYPT_PATH, "crypt", KeyWord, 0);
        fprintf(stderr, "execl(%s, \"crypt\", %s, 0) in creadfile() failed",
                CRYPT_PATH, KeyWord);
        exit(-127);
    } else {                     /* else parent */
        close(fildes);
        close(pipefd[1]);        /* close pipe output */
        if ((f = fdopen(pipefd[0], "r")) == NULL) {
            kill(pid, 9);
            error("Cannot fdopen file \"%s\"", save);
            close(pipefd[0]);
            return 0;
        }
    }

    loading++;
    // XXX: should use a local buffer
    while (fgets(buf, sizeof buf, f)) {
        p = buf;
        while (*p == ' ') {
            /* skip initial blanks */
            p++;
        }
        if (*p == '#' || *p == '\0' || *p == '\n') {
            /* ignore comments and blank lines */
            continue;
        }
        parse_line(buf);
    }
    --loading;
    if (fclose(f) == EOF) {
        error("fclose(pipefd): %s", strerror(errno));
    }
    close(pipefd[0]);
    while (pid != wait(&fildes))
        continue;
    if (eraseflg) {
        pstrcpy(curfile, sizeof curfile, save);
        modflg = 0;
    }
    return 1;
}

int cwritefile(char *fname, rangeref_t rr, int dcp_flags) {
    char path[PATHLEN];
    FILE *f;
    int pipefd[2];
    int fildes;
    int pid;
    char *fn;

    if (*fname == '\0') fname = curfile;

    fn = fname;
    while (*fn && (*fn == ' ')) /* Skip leading blanks */
        fn++;

    if (*fn == '|') {
        error("Cannot have encrypted pipe");
        return -1;
    }

    pstrcpy(path, sizeof path, fname);
    findhome(path, sizeof path);

    if (dobackups && !backup_file(path) &&
            (yn_ask("Could not create backup copy, Save anyway?: (y,n)") != 1))
        return 0;
    if ((fildes = open(path, O_TRUNC|O_WRONLY|O_CREAT, 0600)) < 0) {
        error("Cannot create file \"%s\"", path);
        return -1;
    }

    if (pipe(pipefd) < 0) {
        error("Cannot make pipe to child\n");
        return -1;
    }

    if (KeyWord[0] == '\0') {
        screen_deraw(1);
        pstrcpy(KeyWord, sizeof KeyWord, getpass("Enter key:"));
        screen_goraw();
    }

    if ((pid = fork()) == 0) {           /* if child              */
        close(0);                        /* close stdin           */
        close(1);                        /* close stdout          */
        close(pipefd[1]);                /* close pipe output     */
        dup(pipefd[0]);                  /* connect to pipe input */
        dup(fildes);                     /* standard out to file  */
        fprintf(stderr, " ");
        execl(CRYPT_PATH, "crypt", KeyWord, 0);
        fprintf(stderr, "execl(%s, \"crypt\", %s, 0) in cwritefile() failed",
                CRYPT_PATH, KeyWord);
        exit(-127);
    } else {                             /* else parent */
        close(fildes);
        close(pipefd[0]);                /* close pipe input */
        f = fdopen(pipefd[1], "w");
        if (f == 0) {
            kill(pid, -9);
            error("Cannot fdopen file \"%s\"", path);
            close(pipefd[1]);
            return -1;
        }
    }

    write_fd(f, rr, dcp_flags);

    if (fclose(f) == EOF) {
        error("fclose(pipefd): %s", strerror(errno));
    }
    close(pipefd[1]);
    while (pid != wait(&fildes))
        continue;
    pstrcpy(curfile, sizeof curfile, path);

    modflg = 0;
    error("File \"%s\" written (encrypted).", curfile);
    return 0;
}

#endif /* NOCRYPT */
