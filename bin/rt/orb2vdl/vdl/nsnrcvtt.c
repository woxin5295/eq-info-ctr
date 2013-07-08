/************************************************************
	This code connects to a serial line for a UNIX style system.
	ttpath=init_tt(char * path)		Initialize and open the line.
	err=read_modem(ttpath,&dtr,&rts,&cts,&car,&rng,&dsr)	! read modem line

	An example of how to set the modem lines is the the main debug routine.
	For most systems DTR and RTS are outputs from the UNIX system with the other
	lines input.

	D.C. Ketchum Mar 1996

*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/termio.h>
#include <sys/termios.h>
#include <sys/unistd.h>
#include <stdio.h>
#include <errno.h>

/*
	Main contains debugging code to check out the modem reads more easily.
	It should work standalone if the comments are removed.
*/

#ifdef MAIN
#include "rcv.h"					/* access our definitions */
FILE *logout;
main(argc,argv)
int argc;
char *argv[];
{
	int err,ttpath,i;
	int dtr,rts,cts,car,rng,dsr;
	char ttdev[30];
	logout=stdout;
	fprintf(logout,"start\n"); fflush(logout);
	for(i=0; i<argc; i++)
	{	if(strcmp(argv[i],"-o") == 0)
		{	strcpy(ttdev,argv[i+1]);
			fprintf(logout,"Device=%s\n",ttdev); fflush(logout);
		}
	}
	ttpath=init_ttout(ttdev);
	if(ttpath < 0) exit(1);
	printf("init_ttout returned %d\n",ttpath);
	dtr=TIOCM_DTR;
	err=ioctl(ttpath,TIOCMBIS,&dtr);
	printf("Set DTR on =%d\n",err);
	for(;;) {
		printf("Hit return\n");
		scanf("%c",&dtr);
		err=read_modem(ttpath,&dtr,&rts,&cts,&car,&rng,&dsr);
		printf("ttpath=%d dtr=%d rts=%d cts=%d car=%d rng=%d dsr=%d\n",
			ttpath,dtr,rts,cts,car,rng,dsr);
	}
}
#endif

#ifdef __STDC__
  int init_ttin( char *dev)
#else
  int init_ttin(dev)
char *dev;							/* device name to open */
#endif
/*
	Set up a terminal line that will be used for input primarily.  Allow input
	to XON/XOFF sender (such as a DPU).

	D.C. Ketchum Mar 1996
*/
{
	struct termios t;				/* make a termios data structure */
	int mode;
	char cmdchr;
	int ttpath;				/* The path number to the pipes and serial line*/
	int one=1;				
	int ierr;
	char ctrlq=17;
	extern FILE *logout;			/* connect to log file for output */
	memset( (char *) &t,0,sizeof(t));	/* clear the memory */
	mode=2;							/* indicate RW mode */
	ttpath=open(dev,(O_RDWR | O_NOCTTY | O_NONBLOCK ));		/* Open the path */
/*	fprintf(logout,"Open ttpath=%d\n",ttpath);*/
	if (ttpath < 0) {
		fprintf(logout,"Open port err=%d errno=%d %s\n",
			ttpath,errno,strerror(errno));
		perror("Open port");
		printf ("   INIT_TT :Error Opening Data port err=%x dev=%s\n",errno,dev);
		return -1;
	}
/*	ierr=write(ttpath,"a",1);		/* a write is needed for force it*/
	t.c_iflag= IGNBRK | IXOFF ;		/* ignore breaks, allow me to XON/XOFF */
	t.c_oflag=0;					/* no output processing to be done */
	t.c_cflag=B9600 | CS8 | CREAD | CLOCAL;/* 9600 baud, 8 bit, read enab, local*/
	t.c_lflag=0;					/* not canonical, no sigs, no echo etc */
/*	fprintf(logout,"i=%x o=%x c=%x l=%x\n",t.c_iflag,t.c_oflag,t.c_cflag,t.c_lflag);*/
	ierr=ioctl(ttpath,TCSETS,&t);			/* set mode now */
	if(ierr != 0) {
		fprintf(logout,"IOCTL err=%d errno=%x\n",ierr,errno);
		perror(" IOCTL error");
		return -1;
	}
	ierr=ioctl(ttpath,TIOCSSOFTCAR,&one);/* set soft carrier mode */
	if(ierr != 0) {
		fprintf(logout,"IOCTL err=%d errno=%x\n",ierr,errno);
		perror(" IOCTL error");
	}
	ierr=write(ttpath,&ctrlq,1);
/*	ierr=write(ttpath,"Hello testing\n",14);	/* a write is needed for force it*/
	return ttpath;
}
#ifdef __STDC__
  int init_ttout(char *dev)
#else
  int init_ttout(dev)
  char *dev;							/* device name to open */
#endif
/*
	Set up a terminal line that will be used for OUTPUT primarily.  Allow DPU to
	XON/XOFF us .

	D.C. Ketchum Mar 1996
*/
{
	struct termios t;				/* make a termios data structure */
	int mode;
	char cmdchr;
	int ttpath;						/* The path number to the pipes and serial line*/
	int one=1;				
	int ierr;
	extern FILE *logout;			/* connect to log output file descriptor */
	memset( (char *) &t,0,sizeof(t));/* clear the memory */
	mode=2;							/* indicate RW mode */
	fprintf(logout,"INIT_TTOUT: dev=%s!\n",dev); fflush(logout);
	ttpath=open(dev,(O_RDWR | O_NOCTTY /*| O_NONBLOCK*/ ));		/* Open the path */
	fprintf(logout,"Open ttpath=%d\n",ttpath); fflush(logout);
	if (ttpath < 0) {
		printf ("   INIT_OUT :Error Opening Data port err=%x dev=%s %s\n",
			errno,dev,strerror(errno));
		
		return -1;
	}
	t.c_iflag= IGNBRK | IXON ;		/* ignore breaks, allow me to XON/XOFF */
	t.c_oflag=0;					/* no output processing to be done */
	t.c_cflag=B9600 | CS8 | CREAD | CLOCAL;/* 9600 baud, 8 bit, read enab, local*/
	t.c_lflag=0;					/* not canonical, no sigs, no echo etc */
	memset(t.c_cc,0,NCCS);			/* no special control characters */
	t.c_cc[VSTART]=17;				/* Set control Q to start char */
	t.c_cc[VSTOP] =19;				/* Set control S to stop char */
/*	fprintf(logout,"i=%x o=%x c=%x l=%x\n",
		t.c_iflag,t.c_oflag,t.c_cflag,t.c_lflag);*/
	ierr=ioctl(ttpath,TCSETS,&t);			/* set mode now */
	if(ierr != 0) {
		fprintf(logout,"IOCTL err=%d errno=%x %s\n",ierr,errno,strerror(errno));
		return -1;
	}
	ierr=ioctl(ttpath,TIOCSSOFTCAR,&one);/* set soft carrier mode */
	if(ierr != 0) {
		fprintf(logout,"IOCTL err=%d errno=%x %s\n",ierr,errno,strerror(errno));
	}
/*	ierr=write(ttpath,"Hello testing\n",14);	/* a write is needed for force it*/
	return ttpath;
}
#ifdef __STDC__
   int read_modem(int ttpath, int *dtr, int *rts, int *cts, int *car, int *rng,
		 int *dsr)
#else
  int read_modem(ttpath,dtr,rts,cts,car,rng,dsr) 
  int ttpath;
  int *dtr,*rts,*cts,*car,*rng,*dsr;
#endif
/*
	Line mask should be one of the modem control masks
*/
{
	int err,mask;
	extern FILE *logout;				/* connect to logfile */
	err=ioctl(ttpath,TIOCMGET,&mask);
	if(err != 0) {
		fprintf(logout,"Error reading modem state %d %d %s\n",
			err,errno,strerror(errno));
		return err;
	}
	*dtr=(mask & TIOCM_DTR) ? 1 : 0;
	*rts=(mask & TIOCM_RTS) ? 1 : 0;
	*cts=(mask & TIOCM_CTS) ? 1 : 0;
	*car=(mask & TIOCM_CAR) ? 1 : 0;
	*rng=(mask & TIOCM_RNG) ? 1 : 0;
	*dsr=(mask & TIOCM_DSR) ? 1 : 0;
/*	fprintf(logout,"err=%d dtr=%d rts=%d cts=%d car=%d rng=%d dsr=%d mask=%x\n",
		err,*dtr,*rts,*cts,*car,*rng,*dsr,mask);*/
	return 0;
}
	
