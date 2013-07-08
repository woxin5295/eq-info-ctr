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
{	extern FILE * logout;
	errno = 0;
	if(len < 0) fprintf(logout,"writeoutgb < 0 lengt=%d\n",len);
 	return len;
}
int writeout(buf,len)
int len;								/*Socket safe data write */
char *buf;
{	extern FILE *logout;
	if(len < 0) fprintf(logout,"writeout < 0 lengt=%d\n",len);
 	return len;
}			
void init_tcp()
{
	extern FILE *logout;
	extern int ttpath;
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
