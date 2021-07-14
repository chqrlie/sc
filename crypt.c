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

void creadfile(const char *save, int eraseflg) {
    char buf[FBUFLEN];
    FILE *f;
    int pipefd[2];
    int fildes;
    int pid;
    char *p;

    if (eraseflg && strcmp(save, curfile) && modcheck(" first"))
        return;

    if ((fildes = open(findhome(save), O_RDONLY, 0)) < 0) {
        error("Cannot read file \"%s\"", save);
        return;
    }

    if (eraseflg) erasedb();

    if (pipe(pipefd) < 0) {
        error("Cannot make pipe to child");
        return;
    }

    deraw(1);
    strlcpy(KeyWord, getpass("Enter key:"), sizeof KeyWord);
    goraw();

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
            return;
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
    //linelim = -1;
    if (eraseflg) {
        strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
    }
}

int cwritefile(char *fname, rangeref_t rr, int dcp_flags) {
    FILE *f;
    int pipefd[2];
    int fildes;
    int pid;
    char save[PATHLEN];
    char *fn;
    char *busave;

    if (*fname == '\0') fname = &curfile[0];

    fn = fname;
    while (*fn && (*fn == ' ')) /* Skip leading blanks */
        fn++;

    if (*fn == '|') {
        error("Cannot have encrypted pipe");
        return -1;
    }

    strlcpy(save, fname, sizeof save);

    busave = findhome(save);
    if (dobackups && !backup_file(busave) &&
            (yn_ask("Could not create backup copy, Save anyway?: (y,n)") != 1))
        return 0;
    if ((fildes = open(busave, O_TRUNC|O_WRONLY|O_CREAT, 0600)) < 0) {
        error("Cannot create file \"%s\"", save);
        return -1;
    }

    if (pipe(pipefd) < 0) {
        error("Cannot make pipe to child\n");
        return -1;
    }

    if (KeyWord[0] == '\0') {
        deraw(1);
        strlcpy(KeyWord, getpass("Enter key:"), sizeof KeyWord);
        goraw();
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
            error("Cannot fdopen file \"%s\"", save);
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
    strlcpy(curfile, save, sizeof curfile);

    modflg = 0;
    error("File \"%s\" written (encrypted).", curfile);
    return 0;
}

#endif /* NOCRYPT */
