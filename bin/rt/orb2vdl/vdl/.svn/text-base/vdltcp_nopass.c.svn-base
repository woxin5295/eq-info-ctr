/************************************************************
	This code connects to a serial line for a UNIX style system.
*/
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <time.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h> 
#include <signal.h>
/* alex 12/27/96: added line below for memcpy and memset mods */
#include <string.h>
#include "vdl.h"

#ifdef _UUSS
#include "uuss_tcp.h"			/* include file for NSN project */
#endif
							/* define the signal parameters */
struct sockaddr_in outsock;					/* a socket data structure */
int out;									/* socket unit # */
int sigpipeoccurred;
void init_tcp();
void tcp_handler(arg)
int arg;
{
	extern FILE *logout;
	extern int ttpath;
	if(arg == SIGPIPE) {
		fprintf(logout,"%s SIGPIPE handler %x\n",asctim(),errno);
		sigpipeoccurred=1;
		return;
	} else {
		fprintf(logout,"TCP HANDlER UNknown arg=%x\n",arg);
	}
	exit(22);
}
/*
	Writeout GB deals with the fact that SUNS i*4 align ints.  Hence the
	time code structure is 8 bytes rather than 6.  The extra two bytes
	are before the int containing MS
*/
int writeoutgb(gb,len)
struct gomberg *gb;
int len;
{
	char buf[2048];
	memcpy(buf,gb,10);						/* move first 10 bytes */
	memcpy(buf+10,&(gb->tc.ms),len-10);		/* move the rest of the buffer */
	return (writeout(buf,len));		/* write it */
}


int writeout(buf,len)
int len;								/*Socket safe data write */
char *buf;
{
	int ierr;
	extern FILE *logout;
	extern int ttpath;/* user passes us a path but we know it may changes on reconn*/
	ierr=0;
	while (ierr <= 0) {						/* until we write it sucessfully */
		if(ierr == -1) fprintf(logout,"Wr error recover=%d \n",ttpath);
		sigpipeoccurred=0;					/* not SIGPIPE error is assumed */
		errno=0;
		ierr=write(ttpath,buf,len);			/* do I/O */
#ifdef DEBUG_PRINT
		fprintf(logout,"%s WR len=%d path=%d ierr=%d\n",
				asctim(),len,ttpath,ierr);
#endif
		if(ierr == len) return ierr;		/* if ok return to user */
		sleep(3);

		if(ierr <= 0) fprintf(logout,"Wr error=%d %x sp=%d\n",
				ierr,errno,sigpipeoccurred);
/*		if(errno == EINTR) continue;*/
		if( sigpipeoccurred || ierr == 0) {	/* if SIGPIPE or EOF occurred */
			fprintf(logout,"%s init_tcp recover path=%d\n",
				asctim(),ttpath);
			init_tcp();						/* Reconnect the socket or die trying */
			ierr=-1;						/* make sure we do the I/O again */
		} else {
			fprintf(logout,"Write error not handled ierr=%d errno=%x\n",ierr,errno);
			sleep(3);						/* make sure not fast loops blow logs */
			return ierr;					/* Don't know what kind of err*/
		}
	}
	return ierr;
}			
void init_tcp()
{
	extern FILE *logout;
	extern int ttpath;
	char text[100];
	extern char tag[];
	u_long addr;
#ifdef _UUSS
	short int port=uuss_tcp_port;			/* uuss port to get to GS */
#else
	short int port=2003;					/* port number to use if TCP*/
#endif
	int ierr;
	unsigned char *p;
	extern char outaddr[];
	struct hostent *hp,*gethostbyname();	/* Internet name resolutions routines*/
	strcpy(text,tag);
	ierr=-1;
	if(ttpath > 0) {
		fprintf(logout,"%s %s close tcp path=%d\n",tag,asctim(),ttpath);
		fflush(logout);
		close(ttpath);			/* close any prior socket */
	}
	ierr=-1;
	while (ierr < 0) {
		ttpath=socket(AF_INET, SOCK_STREAM,0);	/* create a Socket on unit ttpath */
		if(ttpath < 0) {
			fprintf(logout,"%s Socket creation failed=%d %x\n",tag,ttpath,errno);
			exit(202);
		}
		hp=gethostbyname(outaddr);	/* where is it */
		if( hp == 0) {
			fprintf(logout,"%s Could not translate %s\n",tag,outaddr);
			fflush(logout);
			if( (int) (addr = inet_addr(outaddr)) == -1)
			{
				fprintf(logout,"Bad Dot address also. %x %x\n",(int)addr,errno);
				fflush(logout);
				sleep(120);
				exit(203);
			} 
			else fprintf(logout,"DOT address translate o.k.=%x\n",addr);
			fflush(logout);
			hp = gethostbyaddr( (char *)&addr, sizeof(addr), AF_INET);
			if( hp == NULL) 
			{	fprintf(logout,"get NSN host by dot address failed!\n");
				exit(204);
			}
			fprintf(logout,"gethostbyaddr returned=%d\n",hp); fflush(logout);
		}
		p=(unsigned char *) hp->h_addr;
		fprintf(logout,"%s %s host=%s  port=%d adr=%d %d %d %d\n",tag,asctim(),
			(char *) hp->h_name,port,*p,*(p+1),*(p+2),*(p+3));fflush(logout);
		memset((char *)&outsock,0, sizeof(outsock));
		memcpy((char*) &outsock.sin_addr,(char*)hp->h_addr,hp->h_length);
											/* copy  stuff to struct*/
		outsock.sin_port=htons(port);			/* NSNTCP at NSN3 is port 2003*/
		outsock.sin_family=hp->h_addrtype;		/* set the address type*/
		if( (ierr=connect(ttpath, (struct sockaddr *)&outsock,
			 sizeof(outsock))) < 0) {
			fprintf(logout,
				"%s %s Cannot connect to port %d=%d path=%d errno=%d\n",
				tag,asctim(),port,ierr,ttpath,errno);	
			fflush(logout);					/* complain */
			close(ttpath);
			sleep(60);
			fprintf(logout,"%s %s Try to connect again \n",tag,asctim());
		}
	}
	fprintf(logout,"%s %s TCP/IP connection complete path=%d\n",
			tag,asctim(),ttpath); fflush(logout);
	signal(SIGPIPE,tcp_handler);
	return ;
}
void init_tt(dev)
char *dev;
{
	extern FILE *logout;
	fprintf(logout,"init_tt cannont be used.  VDLTCP is output \n");
	exit(201);
}
init_pass()
{	return;}
