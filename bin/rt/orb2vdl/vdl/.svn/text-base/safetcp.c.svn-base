/************************************************************
	This code connects to a tcp socket for a UNIX style system.
	User must provide system name or dot address.  The read and write 
	routines will reopen the socket if it is detected to be down and
	hence are "safe" routines in that they should always give logical
	responses without makeing the user deal with socket vagarities.

	D.C. Ketchum Mar 1997

*/
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>
#include <stdio.h>
#include <errno.h> 
#include <signal.h>							/* define the signal parameters */

#include "safetcp.h"						/* define structs for this code */

/*
	Global variables 
*/
struct sockaddr_in outsock;					/* a socket data structure */
int sigpipeoccurred;						/* used to tcp_handler to let world know*/

/*
	TCP_HANDLER is called when bad things happen to a socket so we can
	log them!
*/
#ifdef __STDC__
  void safetcp_handler(int arg)
#else
  void safetcp_handler(arg)
  int arg;
#endif
{
	extern FILE *logout;
	if(arg == SIGPIPE) 
	{	fprintf(logout,"SIGPIPE handler %x\n",errno);
		sigpipeoccurred=1;
		return;
	} 
	else 
	{	fprintf(logout,"TCP HANDlER UNknown arg=%x\n",arg);
	}
	exit(22);
}


#ifdef __STDC__

	int readtcp(struct tcpsocket* tcp, void *buf, int len)

#else

	int readtcp(tcp, buf, len)
	struct tcpsocket* tcp;
	int len;
	void *buf;

#endif

/*
	Socket safe read - deals with socket errors transparently
	If return is zero it is an EOF and no data is available.  Negative reads
	are an error and cause the socket to automatically close and reopen.
	The user should never get a negative return.
	A positive number is the number of bytes returned by read.
*/
{
	int err;
	extern FILE *logout;					/* log error messages */
	sigpipeoccurred=0;						/* no sigpipe errors yet! */
	while ( (err=read(tcp->path, buf, len)) < 0) 
	{
		if(sigpipeoccurred) 
		{	fprintf(logout,"SIGPIPE occured on READ. Reestablish\n");
			if(err >= 0) 
			{
				fprintf(logout,"SIGPIPE but error >= 0 is %d\n",err);
				err=-1;
			}
		}
		if( err < 0) 
		{	fprintf(logout,"Connection lost on read - reconnect\n");
			open_tcp(tcp);						/* open the unit again */
		}
	}
	return err;								/* give user response */
}



#ifdef __STDC__

  int writetcp(struct tcpsocket *tcp, void *buf, int len)

#else

  int writetcp(tcp,buf,len)
  struct tcpsocket *tcp;				/* socket data from open */
  int len;								/*Socket safe data write */
  void *buf;

#endif

{
	int ierr;
	extern FILE *logout;
	ierr=0;
	while (ierr <= 0) {						/* until we write it sucessfully */
		sigpipeoccurred=0;					/* not SIGPIPE error is assumed */
		ierr=write(tcp->path,buf,len);			/* do I/O */
		if(ierr == len) return ierr;		/* if ok return to user */
		if(ierr <= 0) fprintf(logout,"Wr error=%d %x sp=%d\n",
				ierr,errno,sigpipeoccurred);
		if( sigpipeoccurred || ierr == 0) {/* if SIGPIPE or EOF occurred */
			open_tcp(tcp);						/* Reconnect the socket or die trying */
			ierr=-1;						/* make sure we do the I/O again *
		} else {
			fprintf(logout,"Write error not handled ierr=%d errno=%x\n",ierr,errno);
			return ierr;					/* Don;`'t know what kind of err*/
		}
	}
	return ierr;
}



int init_safetcp(tcpunit, hostname, dotadr, port)
struct tcpsocket *tcpunit;
char *hostname, *dotadr;					/* c string to name or dot address */
int port;									/* port number to open */

{	extern FILE *logout;					/* log file id */

	tcpunit->path=0;
	tcpunit->port=port;						/* save port number in structure */
	if(hostname != NULL) 					/* save host name if given */
	{  	tcpunit->hostname = (char *) malloc( strlen(hostname)+1);
		strcpy(tcpunit->hostname, hostname);
	} 
	else
	{	tcpunit->hostname = NULL;			/* not given, use NULL */
	}	
	if(dotadr != NULL) 						/* save dot adr if given */
	{  	tcpunit->dotadr = (char *) malloc( strlen(dotadr)+1);
		strcpy(tcpunit->dotadr, dotadr);
	}
	else
	{	tcpunit->dotadr = NULL;				/* not given, use NULL */
	}
	fprintf(logout,"tcp=%d name=%s dot=%d port=%d \n", tcpunit, 
		tcpunit->hostname, tcpunit->dotadr, tcpunit->port);
	tcpunit->path=open_tcp(tcpunit);		/* open the socket*/
	return tcpunit->path;					/* return path for abusers */
}


open_tcp(tcp)
struct tcpsocket *tcp;
{
	extern FILE *logout;					/* log file id */
	u_long addr;							/* IP address after translation */
	int ierr;
	unsigned char *p;
	extern char *sys_errlist[];
	struct hostent *hp,*gethostbyname();	/* Internet name resolutions */
	struct hostent *gethostbyaddr();		/* Get host by iternet addrs */

	if(tcp->path > 0)
	{	fprintf(logout,"Close Safetcp path=%d %s\n",tcp->path,tcp->hostname);
		fflush(logout);
		close(tcp->path);					/* close any prior socket */
		tcp->path=-1;						/* path not open */
	}
	ierr=-1;								/* set bad so while loop works*/
	while (ierr < 0) 						/* while no socket created */
	{	tcp->path=socket(AF_INET, SOCK_STREAM,0);/* create a Socket on path */
		if(tcp->path < 0) 
		{	fprintf(logout,"Safe Socket creation failed=%d %x\n",
			tcp->path,errno);
			exit(202);
		}
		if(tcp->hostname != NULL) 
		{ 	hp=gethostbyname(tcp->hostname);		/* where is it */
			if( hp == 0) 
			{	fprintf(logout,"Could not translate %s\n",tcp->hostname);
				exit(203);
			}
		} 
		else if (tcp->dotadr != NULL) 
		{	if( (int) (addr = inet_addr(tcp->dotadr)) == -1) 
			{	fprintf(logout,"Bad Dot address = %s \n",tcp->dotadr);
				exit(203);
			}
			hp = gethostbyaddr( (char *)&addr, sizeof(addr), AF_INET);
		} 
		else 
		{
			fprintf(logout,"TCP_INIT must have host name or dot address!");
			exit(203);
		}
			
		p=(unsigned char *) hp->h_addr;
		fprintf(logout,"host=%s  adr=%d %d %d %d\n",(char *) hp->h_name,
			*p,*(p+1),*(p+2),*(p+3));fflush(logout);
		memset((char *)&outsock, 0, sizeof(outsock));
		memcpy( (char*) &outsock.sin_addr, (char*)hp->h_addr, hp->h_length);
											/* copy stuff to struct*/
		outsock.sin_port=htons(tcp->port);	/* correct byte order */
		outsock.sin_family=hp->h_addrtype;	/* set the address type*/
		if( (ierr=connect(tcp->path, (struct sockaddr *) &outsock, 
			sizeof(outsock))) < 0) 
		{	fprintf(logout,
				"Cannot connect to port %d %s=%d path=%d errno=%x %s\n",
				tcp->port,tcp->hostname,ierr,tcp->path,errno,
				sys_errlist[errno]);	
			fflush(logout);					/* complain */
			fprintf(logout,"Wait 60 and retry\n");
			close(tcp->path);
			sleep(60);
			fprintf(logout,"Try to connect again %s %s\n",
				tcp->hostname,tcp->dotadr);
		}
	}
	fprintf(logout,"Safe TCP connection complete path=%d\n",tcp->path); 	
	fflush(logout);
	signal(SIGPIPE,safetcp_handler);
	return tcp->path;
}
