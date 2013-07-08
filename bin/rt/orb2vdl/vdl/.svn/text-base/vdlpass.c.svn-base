/*	This program is a desendant of the PASS.C field code for the Quanterra
	adapted to serve a serial port on a Sun / Unix system.  It is very
	different from PASS in that it can handle multiple inputs from more
	than one VDL or FRONT via the UNIX socket system.  These can be local
	sockets (AF_UNIX) which are files in the file system, or internet like
	sockets where data comes to a well known "port".  
       
D.C. Ketchum Feb 1998 Initial functionality

command line switches
-passout ttt  ttt becomes the serial device name to use.
-passinport 	"Well Known" port this server will respond to 
-passtcp 		dotted.domain.address.  To use for output (-passout not allowed)
				Normally this would be NSN3 or NSN4.cr.usgs.gov
-passport nn 	well know port number to use (TCP output socket only). 
				If 0, no output TCP feeds.  2005 is the normal NSN port.
-passlog		Enable logout to file "pass.log?" instead of stdout.
-pass!			If on command line, turn on debug output.
-passdetail!	If on command line, very verbose logging on.
-passdetail!!	If on command line, very very verbose logging on.

Note that the serial port should monitor the DSR (pin 6) on a DPU for 
"call is up" flow control.  Since hookups vary the definition of 
"DCD" below should be set to the correct status line from the port
as returned by "read_modem".  This value is used in function dcdon()
to check this flow control.

*/

#ifndef	DCD					/* define a DCD if Compiler does not have one*/
#define DCD car				/* or dtr, rts, cts, car, rng or dsr to detect
								the DSR from pin 6 of the DPU */
#endif

/* Normal includes! */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <strings.h>
#include <stropts.h>
#include <math.h>
#include <poll.h>
#include "vdl.h"

/* Define command bytes from GOLDEN */
#define ASKTIME 42				/* SP request a time mark from master*/
#define ASKCONFIG 43			/* SP request configuation info from master*/
#define ASKTRIGGER 44			/* SP request trigger parameters */
#define TONETIME 40				/* SP reports a time mark */
#define ANSTIME 45
#define ANSCONFIG 46
#define ANSTRIGGER 47
#define ROLLBACK 4				/* Golden wants a retransmission of data */
#define NSNRESTART 41			/* USNSN has restarted.  Resend partials.*/
#define FRONTRESTART 48			/* USNSN in golden wants time/quanterra reset*/
#define GETABSCORRECT 49        /* We want to know our absolute right now*/
#define SETRATE 32				/* Golden wants to change the clock rate*/
#define ABSCORRECT 33

/* Other defs */
#define TICKS 100000           /* ticks to sleep*/
#define MAXROLLBACK 64          /* Number of rollback buffers to ring */
#define MAX_CONNECT 50			/* Maximum number of connections */

void cmdarg();					/* cmdarg does not return anything */
char *asctim();					/* asctim gives string pointer */
void msgout();					/* puts out msg depending on dbg flg */

FILE *logout;					/* log file name */
char compdate[20];
int rollcount=2;				/* used to control setting of ROLLBACK inhibit*/
int lastdcd=100;          	    /* tracks last value of DCD ON */
int nextin=0;					/* pointer to q to put next pipein packet*/
int nextout=0;					/* pointer to next block needing releif */
int naskroll=0;					/* counts consequtive ask rolls */
int npackout=0;					/* counts packets going out, limits rollbacks*/
struct gomberg *q;
time_t wtime;					/* internal time of last serial port write */
int nused;						/* number of input blocks in use */
int nsecwrite=1;				/* time to wait before next write */
int syncup;						/* 0= Not synced call is down - wait for DCD,
									1=DCD is up - send rqst for ROLLBACK,
									2=DCD up and everything is synced - happy*/
int gbaskroll=0;				/* tracks ask roll function */
int nused;						/* estimate of # of free packet slots */
static int dbg=0;				/* define debug output flag */
static int cmddbg=1;			/* turn on certain output commands */
int ttpath;						/* path to the serial port */

char ttdev[40]="";				/* string with serial port name */
int tcp_comm=0;					/* used to record whether output is TCP based*/
char outaddr[80]="";			/* string with output TCP host */

int port;						/* port # of output, if TCP sockets allowed */
int tcp_port=0;					/* port # for VDLPASS to serve */
char tcpaddr[80]="";			/* TCP dotted address if allowed */
char tag[6]="VDLPS";			/* for tcp routine tag output */
int totchar=0,totpack=0,totread=0;/* Statistics variables */
int lastseq=-1;					/* last sequence shipped */
char logfilename[80]="";		/* filename of log file, changes daily*/

#ifdef __STDC__
  void cc_handler(int arg)
#else
  void cc_handler(arg)
  int arg;
#endif
{
	char *asctim();
	int err;
	char false=0;
	fprintf(logout,"%s cc_handler=%d\n",asctim(),arg); fflush(logout);
#ifdef SUNOS
/*	err=ioctl(0,FIONBIO, &false);	/* set non blocking i/o */
#endif
	switch (arg) {
		case SIGINT :
 			fprintf(logout,"%s control C clean up\n",tag);
			break;
		case SIGHUP :
			fprintf(logout,"%s SIGHUP exit\n",tag);
			break;
		case SIGQUIT:
			fprintf(logout,"%s SIGQUIT exit\n",tag);
			break;
		case SIGILL:
			fprintf(logout,"%s SIGILL exit\n",tag);
			break;
		case SIGTRAP:
			fprintf(logout,"%s SIGTRAP exit\n",tag);
			break;
		case SIGABRT:
			fprintf(logout,"%s SIGABRT exit\n",tag);
			break;
		case SIGEMT:
			fprintf(logout,"%s SIGEMT exit\n",tag);
			break;
		case SIGFPE:
			fprintf(logout,"%s SIGFPE exit\n",tag);
			break;
		case SIGKILL:
			fprintf(logout,"%s SIGKILL exit\n",tag);
			break;
		case SIGBUS:
			fprintf(logout,"%s SIGBUS exit\n",tag);
			break;
		case SIGSEGV:
			fprintf(logout,"%s SIGSEGV exit\n",tag);
			break;
		case SIGSYS:
			fprintf(logout,"%s SIGSYS exit\n",tag);
			break;
		case SIGALRM:
			fprintf(logout,"%s SIGALRM exit\n",tag);
			break;
		case SIGTERM:
			fprintf(logout,"%s SIGTERM exit\n",tag);
			break;
		case SIGURG:
			fprintf(logout,"%s SIGURG exit\n",tag);
			break;
		case SIGSTOP:
			fprintf(logout,"%s SIGSTOP exit\n",tag);
			break;
		case SIGTSTP:
			fprintf(logout,"%s SIGTSTP exit\n",tag);
			break;
		case SIGCONT:
			fprintf(logout,"%s SIGCONT exit\n",tag);
			break;
		case SIGCHLD:
			fprintf(logout,"%s SIGCHLD exit\n",tag);
			break;
		case SIGTTIN:
			fprintf(logout,"%s SIGTTIN exit\n",tag);
			break;
		case SIGTTOU:
			fprintf(logout,"%s SIGTTOU exit\n",tag);
			break;
		case SIGIO:
			fprintf(logout,"%s SIGIO exit\n",tag);
			break;
		case SIGXCPU:
			fprintf(logout,"%s SIGXCPU exit\n",tag);
			break;
		case SIGXFSZ:
			fprintf(logout,"%s SIGXFSZ exit\n",tag);
			break;
		case SIGWINCH:
			fprintf(logout,"%s SIGWINCH RCVed/Ignored\n",tag);
			return ;
		default:
			fprintf(logout,"%s Unknown signal=%d \n",tag,arg);
	}
	fflush(logout);
	exit(arg);				/* this will go to ON_EXIT handler which will shutdown */
}

#define READ_MASTER		read_master.fds_bits[0]
#define WRITE_MASTER	write_master.fds_bits[0]
#define SPECIAL_MASTER	special_master.fds_bits[0]
#define READ_READY		read_ready.fds_bits[0]
#define WRITE_READY		write_ready.fds_bits[0]
#define SPECIAL_READY	special_ready.fds_bits[0]
main(argc,argv)
char **argv;
int argc;
{	
	fd_set read_ready,write_ready,special_ready;
	fd_set read_master,write_master,special_master;
	struct timeval timeout;
	int well_known,well_known_tcp;		/* the well known sockets to contact */
	struct sockaddr_un addr;			/* address structure Unix socket*/
	struct sockaddr_in server;			/* Internet servers structure */
	struct sockaddr addrun;				/* accept fills this with addr info*/
	struct sockaddr addrin;				/* accept fills this with info */
	int fd[MAX_CONNECT];	 			/* file descriptors for sockets */
	int something;						/* something happened, no sleep */
	int topsock;						/* track largest FD for select */
	int length;
	time_t now;
	int i,k,err,nb;
	int deleted;
	char * asctim();
	int lastyday;						/* last julian day opend for log files */
	struct tm *tmpnt;					/* pointer to time struct */
	struct pollfd fds[10];				/* structure for polling status of FD*/
	logfilename[0]=0;
	well_known=0;
	well_known_tcp=0;
	topsock=0;

	/* initialize fd array, memory for buffers, log file, and timeout */
	logout=stdout;						/* start with logout as stdout */
	for (i=0; i<MAX_CONNECT; i++) fd[i]=-1;
	i=(long)(q+1) - (long) q;			/* compute length of gomberg packets*/
	q = (struct gomberg *) malloc(MAXROLLBACK*i);/* memory for gomber buffers*/
	fprintf(logout,"gb=%d gb1=%d\n",q,q+1); fflush(logout);
	fprintf(logout,"diff=%d\n",((long)(q+1) - (long) q)); fflush(logout);
	if(q == NULL) 
	{	fprintf(logout,"Malloc failed for buffers! exit!\n");
		fflush(logout);
		exit(7);
	}
	timeout.tv_sec=2;					/* seconds of timeout */
	timeout.tv_usec=100000;				/* usec of timeout */
	time(&wtime);						/* set startup time */
	
	/* Set up the signal from VDLMOM */
	signal(SIGINT,cc_handler);
	signal(SIGHUP,cc_handler);
	signal(SIGQUIT,cc_handler);
	signal(SIGILL,cc_handler);
	signal(SIGTRAP,cc_handler);
	signal(SIGABRT,cc_handler);
	signal(SIGEMT,cc_handler);
	signal(SIGFPE,cc_handler);
	signal(SIGKILL,cc_handler);
	signal(SIGBUS,cc_handler);
	signal(SIGSEGV,cc_handler);
	signal(SIGSYS,cc_handler);
	signal(SIGTERM,cc_handler);
	signal(SIGSTOP,cc_handler);	
	signal(SIGCONT,cc_handler);
	signal(SIGTSTP,cc_handler);
	signal(SIGCHLD,cc_handler);
	signal(SIGXCPU,cc_handler);
	signal(SIGXFSZ,cc_handler);
	signal(SIGWINCH,cc_handler);

	/* Get arguments from invocation */
	cmdarg(argc,argv,ttdev);		/* parse commands into globals */
	if(logfilename[0] != 0)
	{	now=time(NULL);				/* get current time */
		tmpnt=gmtime(&now);			/* get pointer to parts */
		logfilename[strlen(logfilename)-1]=48+((tmpnt->tm_yday+1)%10);
		fprintf(logout,"Open initial Log file2=%s! day=%d\n",logfilename,
			tmpnt->tm_yday+1);
		fflush(logout);
		logout=fopen(logfilename,"a+");
		fprintf(logout,"Open initial Log file2=%s! day=%d\n",logfilename,
			tmpnt->tm_yday+1);
		fflush(logout);
		lastyday=tmpnt->tm_yday;	/* save last julian day */
	}
	if(ttdev[0] == 0)
	{	tcp_comm=1;
		ttpath=init_tcp();
	}
	else
	{	tcp_comm=0;
		ttpath=init_ttout(ttdev);		/* open the output serial port */
	}
	if(ttpath < 0)
	{	fprintf(logout,"Bad Serial port open %d errno=%x\n",
			ttpath,errno);
		exit(9);
	} 
	else 
	{	nb=0;
		while ( (k=read(ttpath,&i,1) > 0)) nb++;
		fprintf(logout,"Serial line %s is open on fd %d purged=%d\n",
			ttdev,ttpath,nb); fflush(logout);
	}
	
	/* create and bind well known socket */
	fprintf(logout,"Open sockets\n"); fflush(logout);
	if( (well_known=socket(AF_UNIX, SOCK_STREAM,0)) < 0)
	{	fprintf(logout,"Socket Unix failed-%s %d\n",strerror(errno),errno);
		exit(5);
	} else 
	{	fprintf(logout,"Unix socket opened well_known=%d\n",well_known);
		fflush(logout);
	}

	/* if tcp is allowed as input, set up the port for servicing*/
	if(tcp_port > 0)
	{	if( (well_known_tcp=socket(AF_INET, SOCK_STREAM,0)) < 0)
		{	fprintf(logout,"Socket TCP failed-%s %d\n",strerror(errno),errno);
			exit(6);
		}
		fprintf(logout,"Well_known_tcp=%d Tcp_port=%d Tcpaddr=%s for INET\n",
			well_known_tcp,tcp_port,tcpaddr); fflush(logout);
		server.sin_family=AF_INET;
		strcpy( (char *) &server.sin_addr, tcpaddr);
		server.sin_port=tcp_port;
		if(bind(well_known_tcp,(struct sockaddr *) &server, sizeof server))
		{	fprintf(logout,"binding socket return=%s %d\n",
				strerror(errno),errno);
			exit(8);
		}
		if(listen(well_known_tcp,5) < 0) 
		{	fprintf(logout,"listen INET failed-%s %d\n",strerror(errno),errno);
			exit(7);
		}
	}
	strcpy(addr.sun_path,"PASS_SOCKET");
	addr.sun_family=AF_UNIX;
	deleted=0;
REBIND:
	if(dbg) {fprintf(logout,"bind PASS_SOCKET\n"); fflush(logout);}
	if(bind(well_known,(struct sockaddr *) &addr,strlen(addr.sun_path)+
		sizeof(addr.sun_family)) < 0)
	{	if( errno == EADDRINUSE && deleted == 0)
		{	fprintf(logout,"ADDR in Use : Delete PASS_SOCKET\n");fflush(logout);
			system("rm -f PASS_SOCKET");
			deleted=1;
			goto REBIND;
		}
		fprintf(logout,"Bind pass_socket failed-%s %d\n",strerror(errno),errno);
		fflush(logout);
		exit(4);
	}
	if (fcntl(well_known, F_SETFL, O_NDELAY) <0 )
	{	fprintf(logout,"Failed to set main socket to non-blocking-%s %d",
			strerror(errno),errno);
		exit(1);
	}

	/* start listening to Unix socket */
	fprintf(logout,"start listening\n"); fflush(logout);
	if(listen(well_known,5) < 0) 
	{	fprintf(logout,"listen Unix failed-%s %d",strerror(errno),errno);
		exit(7);
	}

	/* initialize topsock */
	topsock=0;
	if(well_known > topsock) topsock=well_known;/* FDs needing to be checked */
	if(well_known_tcp > topsock) topsock=well_known_tcp;
	fprintf(logout,"topsock=%d wkwn=%d wkwn_tcp=%d\n",
		topsock,well_known,well_known_tcp);

	FD_ZERO(&read_master);				/* clear those ready to read */
	FD_ZERO(&write_master);				/* clear those ready to write */
	FD_ZERO(&special_master);			/* specials not implemented */
	fprintf(logout,"%s masks ZERO rd=%x wr=%x spec=%x\n",asctim(),
		READ_MASTER,WRITE_MASTER,SPECIAL_MASTER);		
	FD_SET(well_known,&read_master);	/* the listener is ready */
	if(tcp_port > 0) FD_SET(well_known_tcp,&read_master);
	fprintf(logout,"%s masks wk=%d rd=%x wr=%x spec=%x\n",asctim(),
					well_known,READ_MASTER,WRITE_MASTER,SPECIAL_MASTER);
	FD_SET(ttpath,&read_master);		/* set read bit for serial line */
	FD_SET(ttpath,&write_master);		/* set write bit for serial line */
	FD_SET(ttpath,&special_master);		/* set special bit for serial line */
	fprintf(logout,"%s masks tt=%d rd=%x wr=%x spec=%x\n",asctim(),
					ttpath,READ_MASTER,WRITE_MASTER,SPECIAL_MASTER);

	/* forever, loop looking for connections and data */
	for(;;) 
	{	something=0;					/* nothin has happened yet */
		read_ready=read_master;			/* set masks from master copies */
		write_ready=write_master;
		special_ready=special_master;
		if(dbg >=2) 
		{	fprintf(logout,"%s Top of loop topc=%d\n",asctim(),topsock);
			fflush(logout);
		}

		msgout("Bef select");
		if(dbg > 2)fprintf(logout,"%s read=%x write=%x special=%x,topsock=%d\n",
				asctim(),READ_READY,WRITE_READY,SPECIAL_READY,topsock+1);			
		nb=select(topsock+1, &read_ready, &write_ready, &special_ready, 
			&timeout);
		msgout("Aft select");
		if(nb < 0)
		{	msgout("err sel");
			fprintf(logout,"%s read=%x write=%x special=%x,topsock=%d\n",
				asctim(),READ_READY,WRITE_READY,SPECIAL_READY,topsock+1);
			fprintf(logout,"Select failed-%s %d\n",strerror(errno),errno);
			exit(9);
			continue;
		}

		msgout("after select");
		if(dbg >= 2 || FD_ISSET(well_known,&read_ready)) 
		{	fprintf(logout,"%s select nb=%d topc=%d well=%d %x IS_SET=%d\n",
				asctim(),nb,topsock,well_known,READ_READY,
				FD_ISSET(well_known,&read_ready)); fflush(logout);
		}

		/* check for new connection and add to fd and select lists */
		if( nb > 0 && FD_ISSET(well_known,&read_ready))
		{	for(i=0; i<MAX_CONNECT; i++) if(fd[i] == -1) break;
			if(i == MAX_CONNECT) 
			{	fprintf(logout,"Out of sockets connections\n");
				exit(2);
			}
			fprintf(logout,"%s Found new Unix connect %d wellknwn=%d\n",
				asctim(),i,well_known); fflush(logout);
			something=1;
			length=14;
			fd[i]=accept(well_known,(struct sockaddr *) &addrun, &length);
			if(fd[i] < 0) 
			{	fprintf(logout,"accept failed! errno=%d %s\n",
						errno,strerror(errno)); fflush(logout);
				exit(8);
			} else 
			{	fprintf(logout,"%s assign to fd=%d\n",asctim(),fd[i]); 
				fflush(logout);
				fprintf(logout,"%s len=%d family=%d str=%s\n",
						asctim(),length,addrun.sa_family,addrun.sa_data);
				FD_SET(fd[i],&read_master);			/* put in select set */
				FD_SET(fd[i],&write_master);
				FD_SET(fd[i],&special_master);
				fprintf(logout,"%s masks fd=%d rd=%x wr=%x spec=%x\n",asctim(),
					fd[i],READ_MASTER,WRITE_MASTER,SPECIAL_MASTER);
			}
			if(fcntl(fd[i], F_SETFL, O_NDELAY) < 0) 
			{	fprintf(logout,"Failed to set socket non-blocking %d %s\n",
					errno,strerror(errno)); fflush(logout);
				exit(3);
			} 
			if(fd[i] > topsock) topsock=fd[i];	/* keep track of biggest*/
		}


		/* check for new connection on tcp add to fd and select lists */
		msgout("chk new tcp");
		if( tcp_port > 0 && nb > 0 && FD_ISSET(well_known_tcp,&read_ready))
		{	for(i=0; i<MAX_CONNECT; i++) if(fd[i] == -1) break;
			if(i == MAX_CONNECT) 
			{	fprintf(logout,"Out of sockets connections\n");
				exit(2);
			}
			fprintf(logout,"%s Found new TCP connect %d\n",asctim(),i);
			something=1;
			length=14;
			fd[i]=accept(well_known_tcp,(struct sockaddr *) &addrin, &length);
			if(fd[i] <0) 
			{	fprintf(logout,"accept TCP failed!-%s %d\n",
					strerror(errno),errno);
				exit(8);
			} else 
			{	fprintf(logout,"%s assign to TCP fd=%d\n",asctim(),fd[i]);
				fprintf(logout,"%s len=%d family=%d str=%s\n",
						asctim(),length,addrin.sa_family,addrin.sa_data);
				FD_SET(fd[i],&read_master);			/* put in select set */
				FD_SET(fd[i],&write_master);
				FD_SET(fd[i],&special_master);
			}
			if(fcntl(fd[i], F_SETFL, O_NDELAY) < 0) 
			{	fprintf(logout,"Failed set TCP socket to non-blocking-%s %d\n",
					strerror(errno),errno);
				exit(3);
			}
			if(fd[i] > topsock) topsock=fd[i];	/* keep track of biggest*/
		}

 
		/* for all connection, look for input */
		msgout("read sockets");
		if(nb>0 && dbg && READ_READY != 0 ) /* select returned, and read o.k */
			fprintf(logout,"%s Select rtn=%d read=%x write=%x spec=%x\n",
				asctim(),nb,READ_READY,WRITE_READY,SPECIAL_READY);
		if(nb > 0) for(i=0; i<MAX_CONNECT; i++)
		{	if(fd[i] > 0) if(FD_ISSET(fd[i],&read_ready))
			{	/*fprintf(logout,"%s Input on fd=%d i=%d in,out=%d %d",
					asctim(),fd[i],i,nextin,nextout);*/
				if( (err=read_it(fd[i])) < 0)	/* read data from socket*/
				{	fprintf(logout,"%s Read socket error-%s %d\n",
						asctim(),strerror(errno),errno);
				} else if(err == 0)
				{	fprintf(logout,"%s Closing FD=%d i=%d errno=%x\n",
						asctim(),fd[i],i,errno);
					FD_CLR(fd[i],&read_master);
					FD_CLR(fd[i],&write_master);
					FD_CLR(fd[i],&special_master);
					close(fd[i]);
					fd[i]=-1;
				} else if(err > 1) something=1;/* note : err=1 means no room */
				else fprintf(logout,"%s No Read=%d read=%x write=%x spec=%x\n",
				asctim(),nb,READ_READY,WRITE_READY,SPECIAL_READY);
			}
		}

		/* Check output to serial line */
		msgout("chk serial");
		if(FD_ISSET(ttpath,&write_ready))		/* available to write?? */
		{	err=chk_serial(ttpath);
			if(err > 0) 
			{	something=1;
/*				if(dbg) fprintf(logout,"%s Chkserial wrote %d on %d\n",
					asctim(),err,ttpath);*/
			} else if(dbg >= 2) fprintf(logout,"%s Chkserial=%d\n",	
					asctim(),err);
			fflush(logout);
		} else 
		{	fds[0].fd=ttpath; 
			fds[0].events=255;
			poll(fds, 1, 100);
			time(&now);
/*			fprintf(logout,"Write Not rdy dcd=%d ev=%x rev=%x\n",
				dcdon(),fds[0].events,fds[0].revents);*/
			if(nused > MAXROLLBACK/4 && fabs(difftime(wtime,now)) > 120.)
			{	fprintf(logout,"%s No writes for two minutes. Reconnect...%d\n",
					asctim(),(int) difftime(wtime,now));
				close(ttpath);
				FD_CLR(ttpath,&read_master);
				FD_CLR(ttpath,&write_master);
				FD_CLR(ttpath,&special_master);
				if(ttdev[0] == 0) ttpath=init_tcp();
				else ttpath=init_ttout(ttdev);/* open the output serial port */
				if(ttpath < 0) 
					fprintf(logout,"%s Bad Serial port open %d errno=%x\n",
					asctim(),ttpath,errno);
				FD_SET(ttpath,&read_master);
				FD_SET(ttpath,&write_master);
				FD_SET(ttpath,&special_master);				
			}
			now=time(NULL);					/* get current time */
			tmpnt=gmtime(&now);				/* get pointer to parts */
			if(tmpnt->tm_yday != lastyday && logfilename[0] != 0)
			{	logfilename[strlen(logfilename)-1]=48+((tmpnt->tm_yday+1)%10);
				fclose(logout);				/* close old log file */
				logout=fopen(logfilename,"w");	/* open log file */
				fprintf(logout,"Open2 logfile %s day=%d\n",
					logfilename,tmpnt->tm_yday+1);
				lastyday=tmpnt->tm_yday;	/* save last time changed */
			}
		}

		/* Is there input data on output port */
		msgout("cmd_serial");
		if(FD_ISSET(ttpath,&read_ready))		/* Available command read?? */
		{	err=chk_serial_cmd(ttpath);
			if(err > 0) something = 1;
			else if(dbg) fprintf(logout,"%s no cmd %d\n",asctim(),err);
		}


		/* If nothing happened on that loop, then sleep awhile */
/*		fprintf(logout,"Sleep %d ticks\n",TICKS);*/
		msgout("tick");
		if( something == 0) usleep(TICKS);
	}				/* end of infinite FOR loop */
}
/*************************************************************************

    Check for data from PIPE to FRONT.  To read more data from pipe nextin+1
    cannot lap nextout. (NEXTIN=NEXTOUT means no buffers in use).

	return	Description
		1	No action, buffers too full
		0	Error, user should close FD and reestablish pipe
		>1	O.K. length of data read

***************************************************************************/
read_it(inpipe)
int inpipe;
{	
    int err;
    static unsigned char sequence=250; /* output GOMBERG sequence for rollback*/
    unsigned char lastgood;     /* Used by ROLLBACK logic to store last Good seq*/
    int cmd;                    /* The command code in input data*/
    unsigned char inbuf[256];   /* input and command assembly buffers*/
    unsigned char lastcmdchar;  /* character processed by command parser */
    int nc,nchar,ierr,nget,i,j,nextchar,nbad;/* character counters and counts */
    int lastrollback;			/* track for infinite rollback loops */
    struct gomberg *gb;
    extern int nused;			/* nextin-nextout. # not xmitted yet*/
    int tst=0;
	extern int npackout;		/* link to total packets out */
    int ncheck;                 /* Used in ROLLBACK logic */
    int idiff;
    char gbaskroll[10];
    char ttdev[6],temp[20];
    gbaskroll[0]=27; gbaskroll[1]=3; gbaskroll[2]=8; /* rqst rollback packet */
	nused=nextin-nextout;               /* how many unxmited packets */
	if(nused < 0) nused+=MAXROLLBACK;   /* if way behind, don't get more!*/
	if(dbg >= 2){fprintf(logout,"%s read_it : nused=%d dcd=%d nxin,out=%d %d\n",
		asctim(),nused,dcdon(),nextin,nextout);fflush(logout);}
	if( ((nextin+1) % MAXROLLBACK) ==nextout
          || nused > MAXROLLBACK*3/4){	/* if full or at least 1/2 full */
		fprintf(logout,"%s read_it : 3/4 FULL nused=%d dcd=%d nxin,out=%d %d\n",
		asctim(),nused,dcdon(),nextin,nextout);fflush(logout);
		usleep(1000000);
		return 1;						/* we do not want more data */
	}
	gb=q+nextin;				/* pointer to gomberg packet*/	
	errno=0;
	err=0;
	while ( (err=read(inpipe,gb,8)) <= 0)	/* get beginning of packet */
	{	if(dbg) 
			fprintf(logout,"%s  Read_it err inpipe=%d err=%d errno=%x d\n",
			asctim(),inpipe,err,errno);fflush(logout);
		if(err == 0 && errno == 0)
		{	fprintf(logout,"%s EOF on fd=%d Close...\n",asctim(),inpipe);
			 return 0;	/* nothing to read */
		}
		if(err < 0 && errno == EWOULDBLOCK) 
		{	usleep(TICKS);
		} else if(err < 0 && errno == ECONNRESET)
		{	fprintf(logout,"read_it ECONNRESET inpipe=%d\n",inpipe);
			fflush(logout);
			return 0;
		} else if(err < 0)
		{	fprintf(logout,"%s Read error err=%d errno=%x inpipe=%d ",
				asctim(),err,errno,inpipe);
			fprintf(logout,"%s errtxt=%s %d\n",asctim(),strerror(errno),errno);
			exit(3);
		}
	}

	/* calculate # of bytes in packet, check for rationality */
	nchar=gb->numbyt[0] + (gb->numbyt[1]*256 & 0xfff);
	if( nchar < 8 || nchar > 2040) 
	{	fprintf(logout,"%s  PASS : input nchar=d on fd=%d out of range.\n",
			asctim(),nchar,inpipe);
			return 0;					/* error to user, should close fd */
	}
	if(dbg) fprintf(logout,
		"%s    PASS : Read_it inpipe=%d in,out=%d %d sq=%d err=%d errno=%x nch=%d\n",
		asctim(),inpipe,nextin,nextout,sequence,err,errno,nchar);
	nget=read(inpipe,( (char *) gb)+8,nchar-8);/* read in rest of packet */
	if(nget != nchar-8)               /* check for unexpected error */
	{	 fprintf(logout,"%s GB pipe get read size wrong. Nchar=%d nget=%d\n",
			asctim(),nget,nchar);
		return 0;
	}
	totread+=nchar;
	nextin=(nextin+1)%MAXROLLBACK;  /* set next place to put data */
	gb->packseq=sequence;           /* set packet outbound sequence*/
	sequence++;
	npackout++;
	return nchar;
}   
int chk_serial(ttpath)
int ttpath;
{
	extern time_t wtime;				/* time of last write */
	static time_t now;					/* internal time strcuts */
	static time_t told;					/* static keeps time across calls */
	static time_t dcdlisten;			/* time when DCD came on */
	static time_t dcdtold;				/* track time of DCD across calls */
	int idiff;
	extern int lastdcd;					/* tracks DCD setting across calls */
	static int dcdup=0;					/* when set messages print? */
	extern int syncup;					/* global when true in "ask roll" wait*/
	int nget,nchar;
	extern int dbg;
	extern int lastseq;					/* last sequence # assigned */
	extern int rollback;				/* global with inhibit countdown */
	extern int syncup;					/* tracks progress of roll back syncs*/
	extern int nused;					/* number of unused packets */
	char *asctim();						/* time ascii routine */
	int i;
	unsigned char *tmppnt;
	char gbaskroll[10]={27,3,8,0,0,0,0,0,0,0};/* used in ask roll logic */
	struct gomberg *gb;					/* pointer to GB packet to use */
	time(&now);							/* get current time */
	idiff=difftime(now,told);
	if(idiff >= 311) 
	{	told=now;                           /* set old time for next time*/
		fprintf(logout,
			"%s    PASS :%d in=%4d out=%4d #pk=%3d sq=%3d us=%2d dcd=%d\n",
			asctim(),idiff,totread*8/idiff,totchar*8/idiff,totpack,lastseq,
			nused,dcdon());
		totread=0;
		totchar=0;
		totpack=0;
	}
/***********************************************************************

    Now if data is available send it out the terminal port
    
*********************************************************************/
	/*   Check for DCD ON (X.25 call connected.  If not, Wait for it */
	if(dcdon() != lastdcd)              /* is DCD down ???? */
	{	if(dcdon() == 0) 
		{	fprintf(logout,"%s   PASS : DCD Down Sleep %d %d\n",
			asctim(),dcdon(),lastdcd);
			syncup=0;                   /* Call down, sync from start*/
		}else 
		{	time(&dcdlisten);
			dcdup=1;                    /* set dcdup so message prints*/
			syncup=1;                   /* set rollback rqst mode */
		}
		lastdcd=dcdon();
	}

	if(syncup == 1)						/* are we in rqst rollback mode? */
	{	idiff=difftime(now,dcdtold);	/* get diff in seconds */
		if(abs(idiff) > 15)				/* only send one every 15 secs */
		{	naskroll++;
			fprintf(logout,"%s   PASS: ASKRL %d %5d lseq=%d",
				asctim(),naskroll,idiff,lastseq);fflush(logout);
			nget=write(ttpath,gbaskroll,8);/* send rqst for ROLLBACK */
			if(nget != 8) fprintf(logout,
				"%s    PASS : ASKROLL wrt tt=%d err=%d %x\n",
				asctim(),ttpath,nget,errno);
			time(&dcdtold);
		}
	}
	idiff=difftime(now,wtime);			/* how long has it been */
	if(dcdup) 
	{	idiff=difftime(now,dcdlisten);
		if(abs(idiff) > 6) 
		{	fprintf(logout,"%s    PASS : DCD Up Listening again...\n",asctim());
			dcdup=0;
		}
	}
  
	/* is data rdy, are we synced, is dcd up, has limit of rate time passed */
	if(dbg >= 2) fprintf(logout,
		"%s  chks in,out=%d %d dcd=%d sync=%d dcdup=%d idiff=%d nsecwr=%d\n",
		asctim(),nextin,nextout,dcdon(),syncup,dcdup,idiff,nsecwrite);
	if(nextin != nextout && dcdon() != 0 && syncup == 2 && dcdup == 0 &&
			abs(idiff) >= nsecwrite)	/* limit output rate to 3000 bps*/
	{	gb=q+nextout;					/* pointer to gomberg to send*/
/*		if( (totpack % 20) == 0) gb=q+ ( (nextout+1) % MAXROLLBACK);*/
		nchar=gb->numbyt[0]+(gb->numbyt[1]*256 & 0xfff);/* Nbyte in packet */
		if(dbg)fprintf(logout,"%s     PASS: to DPU nchar=%d nextout=%d sq=%d\n",
				asctim(),nchar,nextout,gb->packseq);
		if(rollcount > 0) 
		{	gb->numbyt[1] |= 0x80;
			fprintf(logout,
				"%s     PASS : Inh Set =%2d seq=%3d ch=%3d chsq=%3d n=%4d\n",
				asctim(),rollcount,gb->packseq,gb->chanid,gb->seq,nchar); 
			fflush(logout);
			rollcount--;
		} else 
		{	gb->numbyt[1]&= 0x7f;
/*			fprintf(logout,
				"%s     PASS : Inh Clr =%2d seq=%3d ch=%3d chsq=%3d n=%4d",
				asctim(),rollcount,gb->packseq,gb->chanid,gb->seq,nchar);*/
		}	
		fflush(logout);

/*		Dump Beginning of header */
/*		tmppnt=(unsigned char *) gb;
		for(i=0; i<10; i++) fprintf(logout,"%4o",*(tmppnt+i));
		fprintf(logout,"\n");*/


		if(dbg) fprintf(logout,
			"%s      PASS : wr %d seq=%d nchar=%d in,out=%d %d pk=%d %d %d\n",
			asctim(),idiff,gb->packseq,nchar,nextin,nextout,
			gb->routeid,gb->nodeid,gb->chanid);
		lastseq=gb->packseq;            /* save packet sequence */
		time(&wtime);              		/* get current time */
		nget=write(ttpath,gb,nchar);    /* write it out */
		if(nget != nchar) 
		{	fprintf(logout,"    PASS: inc wr %d %d\n",nchar,nget);
		}
		nsecwrite=nchar*8/10000;         /* how long to wait til next write*/
		if(nsecwrite <= 0) nsecwrite=1;	/* at lease one second */
		nsecwrite=0;					/* DEBUG : disable feature */
		time(&now);
		idiff=difftime(now,wtime);      /* how long did write take */
		if(abs(idiff) > 180)            /* did it take a 3 minutes*/
		{	syncup=1;                   /* call might have happened be sure*/
			fprintf(logout,"%s    PASS :LNG Wr resync %d seq=%d\n",
				asctim(),idiff,gb->packseq);
		}
		totchar+=nchar;
		totpack++;
		nextout=(nextout+1) % MAXROLLBACK;
		return nchar;
	}
	else
	{ if(nextin != nextout) 
		{	if(dcdon() == 0) 
				fprintf(logout,"%s CHKSER : NO DCD WRITE=%d\n",
					asctim(),dcdon());
			if( abs(idiff) < nsecwrite) 
				fprintf(logout,
					"%s CHKSER : NO WRITE BAUD LIMIT diff=%d nsec=%d\n",
					asctim(),idiff,nsecwrite);
			if( syncup != 2) 
				fprintf(logout,"%s CHKSER : NO SYNCUP =%d\n",asctim(),syncup);
		}
	}
	return 0;							/* wrote nothing */
}

int dcdon()
{
	int dtr,rts,cts,car,rng,dsr;
	read_modem(ttpath,&dtr,&rts,&cts,&car,&rng,&dsr);
	if(DCD) return 1;		/* note :DCD is #defined to be one of the signals*/
	else return 0;		
}

int chk_serial_cmd(ttpath)
int ttpath;
{
#define XON 17
#define XOFF 19	
	extern int dbg;
	int i,j,k;
	int nbad,lastcmdchar,lastgood,lastrollback,rb_deadman;
	int nextchar;
	extern int npackout;
	char inbuf[256],inlin[256];
	int ierr;
	int outpipe;
	int cmd;						/* the command code decoded */
	int ncheck;						/* used in rollback logic */
	int nc,nget;
	usleep(200000);					/* insure all packet is in */
	errno=0;						/* no funny output */
	nget=read(ttpath,inlin,256);	/* get command serial data */
	if(nget < 0) 
	{	fprintf(logout,"%s     PASS : err read nch=%d errno=%x-%s\n",
			asctim(),nget,errno,strerror(errno));
		return -1;
	}
	if(nget == 0)					/* no data read??? */
	{	if(errno == 0) return 0;	/* EOF - how did we get here??? */
		else
		{	fprintf(logout,"%s cmd read = 0 err=%d-%s\n",
				asctim(),errno,strerror(errno));
			return 0;
		}
	}


	/* purge any control chars XON or XOFF from command line */
	j=0;
	for (i=0; i<nget; i++) 
	{	if(inlin[i] == XON || inlin[i] == XOFF) 
/*			nc=0;					/* dummy work, nc is not important here */
			fprintf(logout,"%s      PASS : Purging XON/XOFF %d\n",
				asctim(),inlin[i]);
		else 
		{
			inlin[j]=inlin[i];
			j++;
		}
	}
	nextchar=0;						/* no chars found yet */
	nget=j;							/* set revised length in nget */
	if(nget <= 0) return 0;			/* all XONs and XOFFs, do nothing */

	/* If debug, dump packet*/
	if(dbg || cmddbg) 
	{	fprintf(logout,"CHKCMD nget=%d tt=%d %d %d %d %d %d %d\n",nget,
		ttpath,inlin[0],inlin[1],inlin[2],inlin[3],inlin[4],inlin[5]);
		for(i=0; i<nget; i++)
		{	fprintf(logout,"%4d",inlin[i]);
			if( (i%20) == 19) fprintf(logout,"\n");
		}
		fprintf(logout,"\n");
		fflush(logout);
	}

	/* read through characters and try to make command packet */
	nbad=0;
	i=0;
	while (i < nget)      /* Until we run out of characters*/
	{	if(nextchar ==0)   /* We are trying to sync up */
		{	if(inlin[i] ==3 && (lastcmdchar ==27 || i == 0))
			{	nextchar=2;
				inbuf[1]=27;
				nc=7;       /* we need 7 characters to compute length*/
				if(dbg) fprintf(logout,"%s     PASS : nc in cmd=%d\n",
					asctim(),nc);
			} else 
			{	if(inlin[i] != 27) 
				{	nbad++;
					if( (nbad % 15) == 1) 
						fprintf(logout,
							"\n%s    PASS purge cmd nget=%d i=%d %4d",
							asctim(),nget,i,inlin[i]); fflush(logout);
/*					else fprintf(logout,"%4d",inlin[i]);*/
					syncup=1;          /* set rollback rqst mode */
				}
			}
		}
		if(nextchar != 0) 
		{	inbuf[nextchar]=inlin[i];  /* Put character in cmd buf */
			if(nextchar == 6)      /* Do we have enough to know bytes*/
			{	nc=(inbuf[3]-32)*16+(inbuf[4]-32)+(inbuf[6]-32)*256;
				if(cmddbg) fprintf(logout,"%s     PASS : Cmd len=%d %x %x %x\n",
						asctim(),nc,inbuf[3],inbuf[4],inbuf[6]);
				if(nc < 14 || nc > 256) 
				{	fprintf(logout,
						"%s     PASS : Cmd len out of range=%d %x %x %x\n",
						asctim(),nc,inbuf[2],inbuf[3],inbuf[6]); fflush(logout);
					nextchar=-1;        /* go to unsynced state */
					syncup=1;           /* go to rqst rollback state */
					nc=1;
				}
			}
			nextchar++;
			if(nextchar >nc && nc > 14) 
			{	if(dbg || cmddbg) 
				{/*	fprintf(logout,"    PASS :Send CMD to Front : \n");
					for (j=1; j<= nc; j++) 
					{	fprintf(logout,"%4d",inbuf[j]);
						if(j % 20 == 19) fprintf(logout,"\n");
					}
					fprintf(logout,"\n");*/ fflush(logout);
				}
/***************************************************************************

Command buffer is complete.  Either handle it or send it to FRONT

**************************************************************************/
				cmd=0;
				cmd=(inbuf[27]-32)*16+(inbuf[28]-32);
				if(cmddbg) fprintf(logout,"%s     PASS : cmd=%d \n",
					asctim(),cmd);
				switch (cmd) 
				{	case ROLLBACK:
					syncup=2;               /* got rollback, state=2*/
					naskroll=0;
					if(rollcount > 0 ) 
					{	fprintf(logout,"%s Rollback in progress. RB ignored\n",
							asctim());
						nextchar=0;
					} else 
					{	lastgood=(inbuf[29]-32)*16+inbuf[30]-32;
						if(lastgood == lastrollback) rb_deadman++;
						else rb_deadman=0;
						lastrollback=lastgood;      /* track last rlbk*/
						ncheck=(npackout < MAXROLLBACK) ? /* limit if startup*/
							npackout : MAXROLLBACK;
						fprintf(logout,"    PASS : Roll to %d max=%d chk=%d",
							lastgood,MAXROLLBACK,ncheck); fflush(logout);
						rollcount=3;        /* insure inhibit bits */
						for (j=0; j< ncheck; j++) 
						{	if( (q+j)->packseq == lastgood) 
							{	fprintf(logout," -Fnd seq j=%d old nxtout=%d\n",
								j,nextout);
								nextout=j;  /* restart it */
								break;
							}
						}	
						if((j == ncheck && ncheck != MAXROLLBACK) || 
							rb_deadman > 5)
						{	if(rb_deadman > 5 ) 
								fprintf(logout,"- Deadman expire");
							fprintf(logout,"- Not found. Startup. Ignore.\n");
							rollcount=4;
						}
						if(j >= MAXROLLBACK) 
						{	nextout=(nextin+2) % MAXROLLBACK;
							rollcount=4;
							fprintf(logout,
								" - Not Found!! set to nextin+2=%d\n",nextout);
							for(k=0; k<MAXROLLBACK; k++) 
							{	if( (k % 20) == 19) 		
									fprintf(logout,"%d\n",(q+k)->packseq);
								else fprintf(logout,"%d ",(q+k)->packseq);
							}
							fprintf(logout,"\n"); fflush(logout);
						}
						nextchar=0;     
					}
					break;

					case FRONTRESTART:
					fprintf(logout,"%s NSN cmd FRONTRESTART. Ignore...\n",
						asctim());
					break;

					case GETABSCORRECT:
					fprintf(logout,
						"%s NSN cmd GETABSCORRECT. Ignore...\n",asctim());
					break;

					case SETRATE:
					fprintf(logout,"%s NSN cmd SETRATE. Ignore...\n",asctim());
					break;

					case ABSCORRECT:
					fprintf(logout,"%s NSN cmd ABSCORRECT Ignore...\n",
						asctim());
					break;

					case ASKCONFIG:
					fprintf(logout,"%s NSN cmd ASKCONFIG. Ignore...\n",
						asctim());
					break;

					case ASKTRIGGER:
					fprintf(logout,"%s NSN cmd ASKTRIGGER. Ignore...\n",
						asctim());
					break;

					case ASKTIME:
					fprintf(logout,"%s NSN cmd ASKTIME. Ignore...\n",asctim());
					break;


					case ANSTIME:
					fprintf(logout,"%s NSN cmd ANSTIME. Ignore...\n",asctim());
					break;

					case ANSCONFIG:
					fprintf(logout,"%s NSN cmd ANSCONFIG. Ignore...\n",
						asctim());
					break;

					case ANSTRIGGER:
					fprintf(logout,"%s NSN cmd ANSTRIGGER. Ignore...\n",
						asctim());
					break;

					case NSNRESTART:
					fprintf(logout,"%s NSN cmd RESTART. Ignore...\n",asctim());
					break;

					default:
					fprintf(logout,"Message for front?  Discard\n");
					fflush(logout);
/*					ierr=write(outpipe,&inbuf[1],nc);*/
					nextchar=0;
					break;
				}	 	/* end of case on command byte */
			}           /* end of if on enough characters */
			if(nbad > 0) fprintf(logout,"\n");
		}		/* end of if nexchar != 0 */
		lastcmdchar=inlin[i];
		i++;
	}			/* end of while more characters*/
	return nget;
}


#ifdef __STDC__
  void cmdarg(int argc, char *argv[],char *ttdev)
#else
  void cmdarg(argc,argv,ttdev)
  int argc;
  char *argv[];
	char *ttdev;
#endif
{	int i;
	extern FILE *logout;
	extern char tcpaddr[];
	extern int tcp_port;			/* port to advertise for input */
	extern int port;				/* port to hit with output */
	extern int dbg;
	extern char logfilename[];
	fprintf(logout,"#args=%d\n",argc);
	for (i=1; i<argc; i++)
	{	fprintf(logout,"%d #%s% \n",i,argv[i]);
		if(strcmp(argv[i], "-passtcp") == 0)
		{	strcpy(outaddr,argv[i+1]);
			fprintf(logout,"TCP Addr=%s\n",outaddr);
		}
		if(strcmp(argv[i],"-passlog") == 0)
		{ 	fprintf(logout,"Log file enabled \n");
			strcpy(logfilename,"vdlpass.loga");
		}
		if(strcmp(argv[i],"-passport") == 0)
		{	port=atoi(argv[i+1]);
			fprintf(logout,"TCP port=%d\n",port);
		}
		if(strcmp(argv[i],"-pass!") == 0)
		{	dbg=1;
			fprintf(logout,"Dbg on\n");
		}
		if(strcmp(argv[i],"-passdetail!") == 0)
		{	dbg=2;
			fprintf(logout,"Detail dbg on\n");
		}
		if(strcmp(argv[i],"-passdetail!!") == 0)
		{	dbg=3;
			fprintf(logout,"Detail %d dbg on\n",dbg);
		}
		if(strcmp(argv[i],"-passout") == 0)
		{	strcpy(ttdev,argv[i+1]);
			fprintf(logout,"TT device=%s\n",ttdev);
		}
		if(strcmp(argv[i],"-passinport") == 0)
		{	tcp_port=atoi(argv[i+1]);
			fprintf(logout,"PASS well known port=%d\n",tcp_port);
		}
	}
	return;
}
/* asctime returns a pointer to a string with the current GMT in it */
char *asctim()
{
	char *t;			/* pointer to string */
	struct tm *tm;
	time_t now;
	now=time(&now);		/* what time is it */
	tm=gmtime(&now);	/* convert to GMT */
	t=asctime(tm);		/* parse to a string */
	*(t+20)=0;			/* eliminate the new line and year */
	return (t+4);			/* hand to user */
}
void msgout(msg)
char *msg;
{
	extern int dbg;
	extern FILE *logout;
	if(dbg < 3) return;
	fprintf(logout,"%s\n",msg);
	fflush(logout);
	return;
}
