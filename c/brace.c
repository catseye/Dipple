/*
 * Copyright (c) 2004 Chris Pressey, Cat's Eye Technologies.
 * Copyright (c) 1980, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * This file was derived from:
 *
 * @(#) Copyright (c) 1980, 1992, 1993 The Regents of the University of California.  All rights reserved.
 * @(#)script.c	8.1 (Berkeley) 6/6/93
 * $FreeBSD: src/usr.bin/script/script.c,v 1.11.2.1 2000/07/20 10:35:21 kris Exp $
 * $DragonFly: src/usr.bin/script/script.c,v 1.3 2003/10/04 20:36:50 hmp Exp $
 *
 * $Id: brace.c 11 2004-09-21 02:08:33Z catseye $
 */

/*
 * To build:
 *    cc brace.c -o brace -lutil
 * on NetBSD, add -DBSD
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <ctype.h>
#include <sysexits.h>

#ifdef BSD
  #include <util.h>
#else
  #include <pty.h>
#endif

/* constants */
#define BUFSIZE		4096

/* GLOBALS */

char		*this_prog;
char		*target_prog;
int		 master, slave;
int		 child;
struct termios	 tt;
int		 rand_in = 0;
int		 rand_out = 0;

/* prototypes */

static void	done(int);
static void	usage(void);
static void	process_input(int, char *, size_t);
static void	process_output(int, char *, size_t);
static void	strrandcase(char *, size_t);

int
main(int argc, char **argv)
{
	int cc;
	struct termios rtt, stt;
	struct winsize win;
	int ch, n;
	char obuf[BUFSIZ];
	char ibuf[BUFSIZ];
	fd_set rfd;
	int lastc = 1000;
	int die, e, pid;
	union wait status;

	this_prog = argv[0];

	/*
	 * Get command-line arguments.
	 */
	while ((ch = getopt(argc, argv, "r:")) != -1)
		switch(ch) {
		case 'r':
			rand_in = (strchr(optarg, 'i') != NULL);
			rand_out = (strchr(optarg, 'o') != NULL);
			if (!(rand_in || rand_out))
				usage();
			srandom(1);  /* XXX should be randomer, but we'll live */
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc == 0)
		usage();

	target_prog = argv[0];

	/*
	 * Create the pty.  Give it the same terminal attributes
	 * as the terminal we're currently using.
	 */
	tcgetattr(STDIN_FILENO, &tt);
	ioctl(STDIN_FILENO, TIOCGWINSZ, &win);
	if (openpty(&master, &slave, NULL, &tt, &win) == -1)
		err(EX_OSERR, "openpty");

	/*
	 * Make stdin 'raw' and stop it from echoing,
	 * otherwise we'll get doubled output.
	 */
	rtt = tt;
	cfmakeraw(&rtt);
	rtt.c_lflag &= ~ECHO;
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &rtt);

	/*
	 * Fork off a child process to handle the execution
	 * of the target program.
	 */
	child = fork();
	if (child < 0) {
		/*
		 * Oh no!  Something went wrong!
		 */
		warn("fork");
		done(EX_OSERR);
	}
	if (child == 0) {
		/*
		 * OK, looks like *we* are the child.
		 */
		close(master);
		login_tty(slave);
		execvp(argv[0], argv);
		/*
		 * If we got this far, we obviously failed to
		 * execute the program.
		 */
		warn("%s", argv[0]);
		kill(0, SIGTERM);
		done(EX_OSERR);
	}

	/*
	 * Otherwise, we're the parent.
	 * Loop, selecting on stdin and 'master' (the program.)
	 */
	FD_ZERO(&rfd);
	for (;;) {
		FD_SET(master, &rfd);
		FD_SET(STDIN_FILENO, &rfd);
		n = select(master + 1, &rfd, 0, 0, NULL);
		if (n < 0 && errno != EINTR)
			break;
		if (n > 0 && FD_ISSET(STDIN_FILENO, &rfd)) {
			cc = read(STDIN_FILENO, ibuf, sizeof(ibuf));
			if (cc <= 0)
				break;
			if (cc > 0) {
				/* Send keystrokes to program. */
				process_input(master, ibuf, cc);
			}
		}
		if (n > 0 && FD_ISSET(master, &rfd)) {
			cc = read(master, obuf, sizeof(obuf));
			if (cc <= 0)
				break;
			/* send program output to stdout */
			process_output(STDOUT_FILENO, obuf, cc);
		}
	}

	if (cc < 0)
		err(EX_IOERR, "read/write");

	/*
	 * Clean up and exit.
	 */
	die = e = 0;
	while ((pid = wait3((int *)&status, WNOHANG, 0)) > 0)
	        if (pid == child) {
			die = 1;
			if (WIFEXITED(status))
				e = WEXITSTATUS(status);
			else if (WIFSIGNALED(status))
				e = WTERMSIG(status);
			else /* can't happen */
				e = EX_OSERR;
		}

	if (die)
		done(e);

	done(0);
}

void
process_input(int fd, char *buf, size_t len)
{
	if (rand_in)
		strrandcase(buf, len);
	write(fd, buf, len);
}

void
process_output(int fd, char *buf, size_t len)
{
	size_t i = 0, j = 0, start = 0;

	while (i < len) {
		if (rand_out)
			strrandcase(buf, len);
		/*
		 * Add indentation.
		 * Inefficient, but again, we'll live.
		 */
		for (j = 0; j < len; j++) {
		    write(fd, buf + j, 1);
		    if (buf[j] == '\n') {
		        write(fd, "   ", 3);
		    }
		}
		/* write(fd, buf, len); */
		i = len;
		break;
	}
}

/*
 * cOnvERt a BUfFER tO raNdoM CAse.
 */
void
strrandcase(char *s, size_t len)
{
	size_t i;

	for(i = 0; i < len; i++)
		s[i] = random() & 0x01 ? toupper(s[i]) : tolower(s[i]);
}

static void
usage(void)
{
	fprintf(stderr,
	    "usage: %s [-r i|o|io] command\n", this_prog);
	exit(EX_USAGE);
}

void
done(int eno)
{
	/*
	 * Reset the terminal back to its original state.
	 */
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &tt);
	close(master);
	exit(eno);
}
