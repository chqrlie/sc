/*
 * Encryption utilites
 * Bradley Williams
 * {allegra,ihnp4,uiucdcs,ctvax}!convex!williams
 * $Revision: 7.16 $
 */

#if !defined(VMS) && !defined(MSDOS) && defined(CRYPT_PATH)

#include <sys/types.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "sc.h"

int Crypt = 0;
#define MAXKEYWORDSIZE 30
char KeyWord[MAXKEYWORDSIZE] = { "" };

void creadfile(const char *save, int eraseflg)
{
    FILE *f;
    int pipefd[2];
    int fildes;
    int pid;

    if (eraseflg && strcmp(save, curfile) && modcheck(" first"))
        return;

    if ((fildes = open(findhome(save), O_RDONLY, 0)) < 0) {
        error ("Can't read file \"%s\"", save);
        return;
    }

    if (eraseflg) erasedb();

    if (pipe(pipefd) < 0) {
        error("Can't make pipe to child");
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
            error("Can't fdopen file \"%s\"", save);
            close(pipefd[0]);
            return;
        }
    }

    loading++;
    while (fgets(line, sizeof(line), f)) {
        linelim = 0;
        if (line[0] != '#') yyparse();
    }
    --loading;
    if (fclose(f) == EOF) {
        error("fclose(pipefd): %s", strerror(errno));
    }
    close(pipefd[0]);
    while (pid != wait(&fildes))
        continue;
    linelim = -1;
    if (eraseflg) {
        strlcpy(curfile, save, sizeof curfile);
        modflg = 0;
    }
}

int cwritefile(char *fname, int r0, int c0, int rn, int cn)
{
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
        error("Can't have encrypted pipe");
        return -1;
    }

    strlcpy(save, fname, sizeof save);

    busave = findhome(save);
    if (dobackups && !backup_file(busave) &&
            (yn_ask("Could not create backup copy, Save anyway?: (y,n)") != 1))
        return 0;
    if ((fildes = open (busave, O_TRUNC|O_WRONLY|O_CREAT, 0600)) < 0) {
        error("Can't create file \"%s\"", save);
        return -1;
    }

    if (pipe(pipefd) < 0) {
        error("Can't make pipe to child\n");
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
            error("Can't fdopen file \"%s\"", save);
            close(pipefd[1]);
            return -1;
        }
    }

    write_fd(f, r0, c0, rn, cn);

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

#endif /* CRYPT_PATH */
