
#include <stdio.h>		/* Standard I/O header file	 */
#include <ctype.h>
#include <string.h>
#include <limits.h>
#include <signal.h>
#include <errno.h>
#include <memory.h>
#include <math.h>
#include <sys/types.h>
#include <unistd.h>
#include "vdl.h"
#include "vdlqsub.h"
#define BCD(A) ((A % 16) + (A / 16) *10)
struct dt_packet {
	unsigned char 	type[2];
	unsigned char	experiment;
	unsigned char 	year;
	unsigned char 	unit[2];
	unsigned char	bcdtime[6];
	unsigned char	nbytes[2];
	unsigned char	seq[2];
	unsigned char	event[2];
	unsigned char	stream;
	unsigned char	chan;
	unsigned char	nsamp[2];
	unsigned char	format[2];
	union {
		unsigned char data[1000];
		short 	idata[500];
		long	ldata[250];
	} d;
};
int lastseq=-1;
int emergency = 0;				/* loop to bail on many errors!!*/
FILE *logout;
FILE *fp;					/* connnect to raw input file */
int fd;						/* file descriptor for input */
/*main(argv,argc)
int             argc;
char           *argv[];
{
	FILE *fp;
	int fd,nb=0,err,i;
	struct nsntime tc;
	char buf[20480];
	int nsamp,iy,id,ih,im,is,ms,leap,ich,done;
	long ia[1024];
	logout=fopen("test.log","a");
	feedme_init(argv,argc);
	read(fd,buf,20480);
	for(;;) {
		nsamp=feedme(&ich,ia,&tc);
		nsnint(tc,&iy,&id,&ih,&im,&is,&ms,&leap);
		printf("Fd ich=%d ns=%d %d %d-%2d:%2d:%2d.%3d %8d %8d %8d %8d %8d\n",
			ich,nsamp,iy,id,ih,im,is,ms,ia[0],ia[1],ia[2],
			ia[nsamp-2],ia[nsamp-1]);
		sleep(1);
	}
}*/
char * strerror(errno)
int errno;
{	extern int sys_nerr;
	extern char *sys_errlist[];
	if(errno < 0  || errno > sys_nerr) return "Error Unknown";
	return sys_errlist[errno];
}
feedme_init(argv,argc) 
int             argc;
char           *argv[];
{
	int refnum,err;				/* refnum comes from the pasnum.ref file */
	FILE * reffile;				/* file pointer for same */
	char filename[50];			/* room to construct the file name */
	char num[10];				/* place to construct the name */
	printf("In feedme_init p\n");
	fprintf(logout,"In feedme_init\n"); fflush(logout);
top:
/*	while( (reffile=fopen("/export/home/ketchum/ref.pasnum","r")) == NULL) {*/
	while( (reffile=fopen("/dyna/data3/prog/lp/ref.pasnum","r")) == NULL) {
		fprintf(logout,"refnum file not availabile\n");
		fflush(logout);
		sleep(2);				/* wait for file available */
	}
	err=fscanf(reffile,"%d",&refnum);
	fclose(reffile);
	fprintf(logout,"%s File num cylc=%d\n",asctim(),refnum);
	fflush(logout);
	if(refnum < 0 || refnum > 100000) {
		fprintf(logout,"Bad ref num=%d\n",refnum);
		exit(101);
	}
	strcpy(filename,"/dyna/data3/raw/rawref.");
/*	strcpy(filename,"/export/home/ketchum/rawref.");*/
	sprintf(num,"%d",refnum);
	strcat(filename,num);
	fprintf(logout,"%s file=%s\n",asctim(),filename); fflush(logout);
	if(fp != NULL) fclose(fp);			/* if another file is open */
	fp=fopen(filename,"rb");
	if(fp == NULL) 
	{	fprintf(logout,"%s Open failed=%s errno=%d-%s\n", 
			asctim(),filename,errno,strerror(errno));
		fflush(logout);
		sleep(60);
		goto top;
	}
	fprintf(logout,"Open %s fd=%d fp->file=%d\n",filename,fp,fp->_file);
	fflush(logout);
	fd= fp->_file;
	err=lseek(fd,0,SEEK_END);
	fprintf(logout,"EOF set to %d\n",err); fflush(logout);
	exit(101);
	return 0;
}
feedme(ich,ia,tc)
int *ich;
long ia[];
struct nsntime *tc;
{
	static int sync;
	static long offset;
	int err;
	unsigned char buf[1024];
	struct dt_packet *dt;
	int iy,id,ih,im,is,ms;
	int waitcnt;
	int i,need,nchar;

	/* variables in DT */
	int experiment,seq,unit,nb,event,stream,chan,nsamp,format;
#ifdef DEBUG_PRINT
	int debug=1;
#else
	int debug=0;
#endif
	err=0;
	for(;;) 
	{	errno=0;
		nchar=0;
		waitcnt=0;
		while (nchar < 1024) 
		{	err=read(fd,&buf[nchar],1024-nchar);
			offset+=err;
			nchar +=err;
			if(debug) fprintf(logout,"Read err=%d sync=%d off=%d\n",
				err,sync,offset);
			if (err == 0) 
			{	sleep(2);
				waitcnt++;
				if(waitcnt > 20) 
				{	fprintf(logout,"No data for 20 sec.  Try reopen file...\n");
					feedme_init();	/* try to open a new file */
					waitcnt=0;
				}
			} 
		}
		err=nchar;
procit:	if(nchar == 1024)
		{	dt=(struct dt_packet *) buf;	/* set point to dt structure */
			iy=BCD(dt->year);
			experiment=BCD(dt->experiment);
			id=BCD(dt->bcdtime[0])*10+BCD(dt->bcdtime[1])/10;
			ih=(BCD(dt->bcdtime[1]) % 10)*10+BCD(dt->bcdtime[2])/10;
			im=(BCD(dt->bcdtime[2]) % 10)*10+BCD(dt->bcdtime[3])/10;
			is=(BCD(dt->bcdtime[3]) % 10)*10+BCD(dt->bcdtime[4])/10;
			ms=(BCD(dt->bcdtime[4]) % 10)*100+BCD(dt->bcdtime[5]);
			*tc=maknsn(iy,id,ih,im,is,ms,0);	/* make nsn time code */
			seq=BCD(dt->seq[0])*100+BCD(dt->seq[1]);
			nb=BCD(dt->nbytes[0])*100+BCD(dt->nbytes[1]);
			if(seq != lastseq+1 && lastseq !=-1) fprintf(logout,
				"Out of seq got=%d expecting=%d\n",seq,lastseq+1);
			lastseq=seq;
			if(debug)
				fprintf(logout,
				"err=%d tp=%c%c| sq=%d exp=%d %d %d-%2d:%2d:%2d.%3d nb=%d\n",
				err,buf[0],buf[1],seq,experiment,iy,id,ih,im,is,ms,nb);
				fflush(logout);
			if(buf[0] == 'D' && buf[1] == 'T') 	/* DT case */
			{	dt=(struct dt_packet *) buf;	/* set point to dt structure */
				event=BCD(dt->event[0])*100+BCD(dt->event[1]);
				stream=BCD(dt->stream);
				chan=BCD(dt->chan);
				nsamp=BCD(dt->nsamp[0])*100+BCD(dt->nsamp[1]);
				format=BCD(dt->format[0])*100+BCD(dt->format[1]);
				if(debug) {fprintf(logout,"ev=%d str=%d ch=%d ns=%d form=%d\n",
					event,stream,chan,nsamp,format); fflush(logout);}
				if(debug) {fprintf(logout,"data=%d %d %d %d %d\n",
					dt->d.ldata[0],dt->d.ldata[1],dt->d.ldata[2],
					dt->d.ldata[nsamp-2],dt->d.ldata[nsamp-1]);fflush(logout);}
				memcpy(ia,dt->d.ldata,1000);
				if(chan == 0) *ich=2;
				if(chan == 1) *ich=0;
				if(chan == 2) *ich=1;			/* set channels */
				return nsamp;	
			} 
			else if( buf[0] == 'C' && buf[1] == 'D') 
			{	if(debug) fprintf(logout,"Cal Def\n");
			}
			else if( buf[0] == 'D' && buf[1] == 'S') 
			{	if(debug) fprintf(logout,"Data Stream \n");
			}
			else if( buf[0] == 'E' && buf[1] == 'T') 
			{	if(debug) fprintf(logout,"Event Trailer\n");
				lastseq=-1;
			} 
			else if( buf[0] == 'O' && buf[1] == 'M') 
			{	if(debug) fprintf(logout,"Operating Mode\n");
			} 
			else if( buf[0] == 'S' && buf[1] == 'C') 
			{	if(debug) fprintf(logout,"Station Channel\n");
			} 
			else if( buf[0] == 'S' && buf[1] == 'H') 
			{	if(debug) fprintf(logout,"State of Health\n");
			}
			else if( buf[0] == 'E' && buf[1] == 'H') 
			{	if(debug) fprintf(logout,"Event Header\n");
			} 
			else if( buf[0] == 'D' && buf[1] == 'S') 
			{	if(debug) fprintf(logout,"Data Stream\n");
			} 
			else if( buf[0] == 'A' && buf[1] == 'D') 
			{	if(debug) fprintf(logout,"ADC report??\n");
			} 
			else 
			{	printf("Type unknown=%c%c offset=%d\n",buf[0],buf[1],offset);
				fprintf(logout,"Type unknown=%c%c offset=%d\n",
					buf[0],buf[1],offset);
				sync=0;
				goto resync;
			}
			sleep(1);		/* debug dont go to fast through non-aligned*/
		}				/* endif on err != 1024 */
resync:	if(sync == 0 && err > 0) 			/* try to sync up */
		{	for(i=0; i<err-1; i++) 
			{	if(buf[i] == 'D' && buf[i+1] == 'T') 
				{	sync=1;
					if(debug) fprintf(logout,"Sync up DT found i=%d\n",i);
					if(i != 0) 
					{	if(debug) fprintf(logout,"move data %d bytes.",
							err-i);
						memcpy(buf,&buf[i],err-i);	/* move data to beginning*/
						need=1024-(err-i);
						nchar=err-i;
						while(need > 0) 
						{	err=read(fd,&buf[nchar],need);
							offset += err;
							nchar += err;
							if(debug) fprintf(logout,
								"  got %d bytes need %d\n",err,need);
							if(err == need) 
							{	sync=1;			/* we are synced up */
								err=1024;		/* we have all the data */
								fprintf(logout,"Synced up! %d \n",nchar);
								goto procit;
							} 
							else 
							{	fprintf(logout,
								"Read remain short! need=%d err=%d errno=%x\n",
									need,err,errno);
								if(errno != EINTR || err != 0) 
								{	emergency++;
									perror("Short read");
									if(emergency > 1000)
									{	fprintf(logout,
											"Emergency short read exit=%d\n",
											emergency);
										fflush(logout);
										exit(201);
									}
									if(emergency > 1000) exit(202);
								}
								sleep(4);
								need=need-err;
							}
						}
						break;				/* get out of the for on "DT" */
					} else {sync=1; break;}
				}
			}
			if(err != 1024) continue;		/* no sense checking anything */
		}

	}					/* end of while err=0 */
	fprintf(logout,"FEEDME Impossible exit from for\n"); fflush(logout);
}
feedme_shutdown()
{
	if(fp != NULL) fclose(fp);
	fprintf(logout,"Feedme_shutdown executed\n"); fflush(logout);
	if(logout != NULL) fclose(logout);
	return;
}
