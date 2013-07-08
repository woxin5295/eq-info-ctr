#include "vdl.h"
#include "vdlqsub.h"
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>


/* Main routine Remove comments to allow it to compile */
#ifdef MAIN

int feedpipe=-1;
FILE *logout;
int leapnext;
main(argc,argv)
int argc;
char **argv;
{
	int i;
	int ich;
	struct nsntime tc;
	long ia[10000];
	logout=fopen("frcv.log","a+");					/* open log file */
	fprintf(logout,"feedme_rcv argc=%d %s %s\n",
		argc,argv[argc-2],argv[argc-1]);			/* look at end of list*/
	for(i=0; i<argc; i++)							/* VDL normally does this*/
	{	if(strncmp(argv[i],"-z",2) == 0)			/* we need only pipe # */
		{	feedpipe=atoi(argv[i+1]);				/* get pipe number */
			fprintf(logout,"Input pipe # is %d\n",feedpipe);
		}
	}
	feedme_init(argc,argv);							/* initialize feedme */
	for(;;) 										/* call feedme */
	{	fprintf(logout,"feedme rtn=%d\n",feedme(&ich,ia,&tc));
		fflush(logout);
	}
	exit (1);
}

/*  end of main routine */

#else

extern FILE *logout;		/* connect to external logout */
extern int feedpipe;		/* connect to feed pipe variable */

#endif
/*
	This is designed to work with the user_proc_vdl.c to get data from a RCV 	
	type program.  The data comes through a pipe which was set up by the
	user_proc_vdl.c and its path number sent in the command string 

global variables :

*/
int rcvpath;					/* path # of pipe from RCV */
feedme_init(argc,argv)
int argc;
char **argv;
{
	extern int feedpipe;
	fprintf(logout,"Feedpipe=%d\n",rcvpath);
	rcvpath=feedpipe;
	fflush(logout);
	return 0;
}
feedme(ich,ia,tc)
int *ich;
long ia[];
struct nsntime *tc;
{
	int iy,id,ih,im,is,ms,nsamp;
	char name[6];
	char cname[5];
	extern char tag[];
	double rate;				/* digitizing rate */
	int seq,eof,ierr;				/* sequence # and EOF */
	struct stat buf;
	int ioff,nbytes;
	buf.st_size =0;
/*	fprintf(logout,"fdme path=%d\n",rcvpath); fflush(logout);*/
	for(;;)
	{	ierr=fstat(rcvpath,&buf);
		if(buf.st_size > 30) break;
/*		fprintf(logout,"Slp %d %d ",ierr,buf.st_size); fflush(logout);*/
		sleep(5);
	}
/*	fprintf(logout,"st_size=%d\n",buf.st_size); fflush(logout);*/
	ierr=read(rcvpath,name,5);
	if(ierr != 5)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,cname,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&rate,sizeof(double));
	if(ierr != sizeof(double) )
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&seq,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&eof,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&iy,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&id,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&ih,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&im,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&is,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&ms,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
	ierr=read(rcvpath,&nsamp,4);
	if(ierr != 4)
	{	fprintf(logout,"FDME rd err=%d errno=%d\n",ierr,errno);
	}
/*	fprintf(logout,
	" %s %s %d %d %2d:%2d:%2d.%3d rt=%f sq=%d eof=%d ns=%d\n",
	name,cname,iy,id,ih,im,is,ms,rate,seq,eof,nsamp); fflush(logout);*/
	nbytes=nsamp*4;
	ioff=0;
	while(nbytes > 0) 
	{	ierr=read(rcvpath,((char *) ia) + ioff,nbytes);
/*		fprintf(logout,"ioff=%d nb=%d ierr=%d %d\n",ioff,nbytes,ierr,errno);*/
		if(ierr > 0) 
		{	nbytes-=ierr;
			ioff+=ierr;
		}
	}
	*tc=maknsn(iy,id,ih,im,is,ms,0);
	*ich=-1;
	if(strcmp(cname,"BHZ") == 0) *ich=2;
	if(strcmp(cname,"BHN") == 0) *ich=0;
	if(strcmp(cname,"BHE") == 0) *ich=1;
	if(strcmp(cname,"LHZ") == 0) *ich=5;
	if(strcmp(cname,"LHN") == 0) *ich=3;
	if(strcmp(cname,"LHE") == 0) *ich=4;
	if(*ich == -1) 
	{	fprintf(logout,"Unknown component =%s %s\n",name,cname);
		return 0;
	}
	fprintf(logout,
	"%s %s feedme %d %d %2d:%2d:%2d.%3d rt=%f sq=%d eof=%d ns=%d ich=%d\n",
	name,cname,iy,id,ih,im,is,ms,rate,seq,eof,nsamp,*ich); fflush(logout);
	strcpy(tag,name);
	return nsamp;
}
