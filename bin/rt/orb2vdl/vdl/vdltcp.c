/************************************************************
	This code connects to a socket for a UNIX style system.
	This code controls the output from a program to the next stage or
	communication method.  It has two modes :
	1) Output is via a TCP socket (dotted internet name in outaddr)
	2) Output is via a Unix socket (same machine undotted name)

	The method used depends on the address passed in global outaddr.  If it
	contains "." then it is assume an dotted internet address and TCP/IP sockets
	are used.  If it does not contain ".", it is a Unix socket and contains
	the file name to use the the filesystem for accessing the socket.

	global variable	purpose
	ttpath			Contains FD connected to output stream
	outaddr			Char string with address to contact (filename or IP dotted)
	port			If TCP/IP address, port to contact.
	tag				Character string include in output to help identify unit


*/
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>			/* Unix domain sockets */
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <time.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h> 
#include <signal.h>
#include <stropts.h>
#include <poll.h>
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
char * asctim();
void init_pass();


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

/*
	Writeout writes data on global FD ttpath from buf of len characters.  It 
	checks the FD for writability using poll.  If a FD is unwritable for 120 
	seconds then the socket is closed and remade.
*/
int writeout(buf,len)
int len;								/*Socket safe data write */
char *buf;
{
	int ierr,i;
	extern FILE *logout;
	extern int ttpath;			/* NOTE : reconnect may change this global */
	struct pollfd fds[2];
	static time_t wtime;
	time_t now;
	ierr=0;
/*	fprintf(logout,"write data len=%d ",len);
	for(i=0; i<20; i++) fprintf(logout,"%4d",buf[i]);
	fflush(logout);*/
	while (ierr <= 0) 						/* until we write it sucessfully */
	{	if(ierr == -1) fprintf(logout,"Wr error recover=%d\n",ttpath);
		fflush(logout);
		sigpipeoccurred=0;					/* not SIGPIPE error is assumed */
		errno=0;
		fds[0].fd=ttpath; 					/* set up poll structure */
		fds[0].events=255;
		poll(fds, 1, 100);					/* set poll bits */
		if( (fds[0].revents & POLLOUT) == 0)
		{	time(&now);						/* time now */
/*			fprintf(logout,"Write Not rdy ev=%x rev=%x %d\n",
				fds[0].events,fds[0].revents,(int) difftime(now,wtime));*/
			if( difftime(now,wtime) > 120.)
			{	fprintf(logout,"Write not ready for 120 secs.  Restart...\n");
				 init_tcp();				/* TCP start it */
				if(ttpath <= 0)				/* did it work */
				{	fprintf(logout,"VDL badout open %d errno=%x\n",
					ttpath,errno);
					sleep(30);				/* wait 30 and try again */
				}else
				{ fprintf(logout,"Reopen of vdlpass path successful.  %d\n",
					ttpath);
					sleep(2);
				}
			}
			continue;						/* go to bottom and wait on poll */
		}
		time(&wtime);
		ierr=write(ttpath,buf,len);			/* do I/O */
#ifdef DEBUG_PRINT
		fprintf(logout,"%s WR len=%d path=%d ierr=%d\n",
				asctim(),len,ttpath,ierr);
#endif
/*		fprintf(logout," done\n");fflush(logout);*/
		if(ierr == len) return ierr;		/* if ok return to user */
		sleep(3);
		if(ierr <= 0) fprintf(logout,"***Wr error=%d %x sp=%d\n",
				ierr,errno,sigpipeoccurred);
		if( sigpipeoccurred || ierr == 0) 	/* if SIGPIPE or EOF occurred */
		{	fprintf(logout,"%s init_tcp recover path=%d\n",
				asctim(),ttpath);
			init_tcp();						/* Reconnect the socket or die trying */
			ierr=-1;						/* make sure we do the I/O again */
		} else 
		{	fprintf(logout,"Write error not handled ierr=%d errno=%x\n",ierr,errno);
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
	extern char outaddr[];					/* usually nsn3.cr.usgs.gov */
	extern int port;					/* port number to use if TCP*/
	int ierr;
	unsigned char *p;
	struct hostent *hp,*gethostbyname();	/* Internet name resolutions routines*/
	strcpy(text,tag);
	ierr=-1;
	if(ttpath > 0) 
	{	fprintf(logout,"%s %s close tcp path=%d\n",tag,asctim(),ttpath);
		fflush(logout);
		close(ttpath);			/* close any prior socket */
	}
	if( !strchr(outaddr,'.') ) 
	{	init_pass();
		return ;
	}
	ierr=-1;
	while (ierr < 0) 
	{	ttpath=socket(AF_INET, SOCK_STREAM,0);	/* create a Socket on unit ttpath */
		if(ttpath < 0) 
		{	fprintf(logout,"%s Socket creation failed=%d %x\n",
				tag,ttpath,errno);
			exit(202);
		}
		hp = NULL;
		while (hp == 0)						/* loop until adr is found */
		{	hp=gethostbyname(outaddr);	/* where is it */
			if( hp == 0) 
			{	fprintf(logout,"%s Could not translate %s\n",tag,outaddr);
				fflush(logout);
				if( (int) (addr = inet_addr(outaddr)) == -1)
				{
					fprintf(logout,"Bad Dot address also. %x %x\n",
					(int)addr,errno);
					sleep(120);
				}
				hp = gethostbyaddr( (char *)&addr, sizeof(addr), AF_INET);
			}
		}
		p=(unsigned char *) hp->h_addr;
		fprintf(logout,"%s %s host=%s  adr=%d %d %d %d\n",tag,asctim(),
			(char *) hp->h_name,*p,*(p+1),*(p+2),*(p+3));fflush(logout);

		memset((char *)&outsock,0, sizeof(outsock));
		memcpy((char*) &outsock.sin_addr,(char*)hp->h_addr,hp->h_length);
											/* copy  stuff to struct*/
		outsock.sin_port=htons(port);		/* NSNTCP at NSN3 is port 2003*/
		outsock.sin_family=hp->h_addrtype;	/* set the address type*/
		if( (ierr=connect(ttpath, (struct sockaddr *)&outsock,
			 sizeof(outsock))) < 0) 
		{	fprintf(logout,
				"%s %s Cannot connect to port=%d err=%d path=%d errno=%d\n",
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
void init_pass()
{
	extern FILE *logout;
	extern int ttpath;
	extern char tag[];
	char text[100];
	u_long addr;
	extern char outaddr[];				/* usually nsn3.cr.usgs.gov */
	int ierr;
	struct sockaddr_un outsock;			/* a unix socket structure */
	if( strchr(outaddr,'.') ) 
	{	init_tcp();
		return ;
	}
	strcpy(text,tag);
	fprintf(logout,"Enter init_pass %d %s %s\n",ttpath,tag,outaddr);
	fflush(logout);
	ierr=-1;
	if(ttpath > 0) 
	{	fprintf(logout,"%s %s close tcp path=%d\n",tag,asctim(),ttpath);
		fflush(logout);
		close(ttpath);			/* close any prior socket */
	}
	ierr=-1;
	while (ierr < 0) 
	{	fprintf(logout,"socket call\n");	fflush(logout);
		ttpath=socket(AF_UNIX, SOCK_STREAM,0);	/* create a Socket on ttpath */
		if(ttpath < 0) 
		{	fprintf(logout,"%s Socket creation failed=%d %x\n",
				tag,ttpath,errno);
			exit(202);
		}
		fprintf(logout,"outaddr=%s!\n",outaddr); fflush(logout);
		strcpy(outsock.sun_path,outaddr);	/* set path to connect to */
		outsock.sun_family=AF_UNIX;			/* connection type */
											/* copy  stuff to struct*/
		fprintf(logout,"connect ttpath=%d fam=%d adr=%s len=%d\n",
			ttpath,outsock.sun_family,outsock.sun_path,
			strlen(outaddr)+sizeof(outsock.sun_family)); fflush(logout);
		if( (ierr=connect(ttpath, (struct sockaddr *)&outsock,
			 strlen(outaddr)+sizeof(outsock.sun_family))) < 0) 
		{	fprintf(logout,"%s in connect err %s\n",asctim(),tag); 
			fflush(logout);
			fprintf(logout,
				"%s %s Cannot connect to UNIX socket=%d path=%d errno=%d\n",
				tag,asctim(),ierr,ttpath,errno); fflush(logout);
			fflush(logout);					/* complain */
			close(ttpath);
			sleep(60);
			fprintf(logout,"%s %s Try to connect again \n",tag,asctim());
		} else 
		{	fprintf(logout,"%s Connection complete ierr=%d\n",tag,ierr);
			fflush(logout);
		}
	}
	fprintf(logout,"out of connect %s\n",tag);fflush(logout);
	fprintf(logout,"%s %s Unix Socket connection complete path=%d\n",
			tag,asctim(),ttpath); fflush(logout);
	signal(SIGPIPE,tcp_handler);
	return ;
}

