#include "sdr.h"
#include "vdl.h"
#include "vdlqsub.h"
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <termios.h>
#include <memory.h>
#include <string.h>
#include <unistd.h>
/*#include "steim.h"
/*#include "steimlib.h"*/
#include "safetcp.h"



#ifdef MAIN


/*#include "seed.h"*/
/*#include "../sunrcv/rcv.h"*/
/*
	Misc Defines
*/
/*
	Global variables 
*/	
FILE *logout;
int error;
main(argc,argv)
int argc;
char **argv;
{
	int iy,id,ih,im,is,ms,leap;
	int ich;
	long ia[1024];
	int ns;
	struct tcpsocket tcp;				/* a place to keed safe socket stuff*/
	struct nsntime tc;
/*
	Initialization
*/
	logout=stdout;						/* start with terminal output */
/*	logout=fopen("/export/home/ketchum/fseed.log","a");*/
	logout=fopen("fseed.log","a");
	if(feedme_init(argc,argv)) exit(4);	/*initialize link */
	for(;;)
	{	ns=feedme(&ich,ia,&tc);
		nsnint(tc,&iy,&id,&ih,&im,&is,&ms,&leap);
		fprintf(logout,
			"feedme ich=%d NS=%d %d %d-%2d:%2d:%2d.%3d %8d%8d%8d%8d\n",
			ich,ns,iy,id,ih,im,is,ms,ia[0],ia[1],ia[ns-2],ia[ns-1]);
		printf("feedme ich=%d NS=%d %d %d-%2d:%2d:%2d.%3d %8d%8d%8d%8d\n",
			ich,ns,iy,id,ih,im,is,ms,ia[0],ia[1],ia[ns-2],ia[ns-1]);
		fflush(logout);
	}
}
#endif


#define MAX_CHANNELS 10
#define MAX_DATA 1024
int dbg,tcpflg;
int nchannels;
char *statname[MAX_CHANNELS];
char *comps[MAX_CHANNELS];
int  channels[MAX_CHANNELS];
long icsteim[2][MAX_CHANNELS];
long ia0[MAX_CHANNELS];
int leapnext;
char seed_name[10];
char seed_location[10];
int first_warn;
struct tcpsocket tcp;				/* unit control for input socket */

int feedme_init(argc,argv)
int argc;
char **argv;
{
	int path;
	extern FILE *logout;
	int i,len;
	char *hostname, *dotadr;
	int port;
	extern int dbg;
	fprintf(logout,"feedme_init seed argc=%d\n",argc); fflush(logout);
	seed_location[0] = 0;
	first_warn = -2;
	fprintf(logout,"feedme_init seed fw=%d\n",first_warn); fflush(logout);
	for(i=0; i<MAX_CHANNELS; i++)
	{	icsteim[0][i] = 212374000;
		icsteim[1][i] = 212374000;
		ia0[i] = 212374000;
		statname[i]=NULL;
		comps[i]=NULL;
		channels[i]=0;
	}
	nchannels=0;
	fprintf(logout,"feedme_init seed maxc=%d\n",MAX_CHANNELS); fflush(logout);

	for (i=0; i<argc; i++) 
	{	/*fprintf(logout,"feedme_init i=%d argv=%s\n",
				i,argv[i]);fflush(logout);*/
		if(strcmp(argv[i],"-lhost") == 0) 
		{	if( isdigit( *(argv[i+1])) )
			{	dotadr=(char *) malloc(strlen(argv[i+1])+1);
				hostname=NULL;
				strcpy(dotadr,argv[i+1]);
				fprintf(logout,"dot adr=%s\n",dotadr);
			}
			else
			{	hostname=(char *) malloc(strlen(argv[i+1])+1 );
				strcpy(hostname, argv[i+1]);
				dotadr=NULL;
				fprintf(logout,"Host =%s\n",hostname);
			}
		}
		if(strcmp(argv[i],"-port") == 0) 
		{	port=atoi(argv[i+1]);
			fprintf(logout,"Port=%d\n",port);
		}
		if(strcmp(argv[i],"-S") == 0)
		{	strcpy(seed_name,argv[i+1]);
			fprintf(logout,"Seed name=%s\n",seed_name);
		}
		if(strcmp(argv[i],"-SL") == 0 ) 		/* limit location */
		{	/*len=strlen(argv[i+1]);
			fprintf(logout,"SL len=%d\n",len); fflush(logout);
			fprintf(logout,"SL str=%s\n",argv[i+1]); fflush(logout);*/
			strcpy(seed_location,argv[i+1]);
			fprintf(logout,"Seed location = %s\n",seed_location);
			fflush(logout);
		}
		if(strcmp(argv[i],"-%") == 0) 
		{	fprintf(logout,"Debug bef=%d ",dbg);
				dbg=(++dbg) %2;
			fprintf(logout,"Debug=%d\n",dbg);
		}
	}
 	path=init_safetcp(&tcp,hostname,dotadr,port);/* open path to TCP port */
	if(path <=0) 
	{	fprintf(logout,"RCV : Bad TERMINAL/TCP open %s %d\n",
				hostname,dotadr,port,path);
		return 1;
	}
	return 0;
}
int feedme(ich,idat,tc)
int *ich;
long idat[];
struct nsntime *tc;
{
extern FILE *logout;
int nbytes;
char bufin[1024];
static char buf[1024];
static int inpnt;
extern char station[];
extern FILE *logout;
static int sync;				/* are we synced up */
static int expect=512;					/* number of bytes expected */
struct _sdr_hdr * pnt;				/* point to seed header */
struct _blockette_1000 *blk1000;	/* point to data only seed blockette*/
int i,k,j,ncopy;
char name[6];
char comp[6];
char *p;							/* general pointer to byte stream array*/
int iy,id,ih,im,is,ms,leap;
int err;							/* error return from socket reads */
/*
		Decompression related variables 
*/
short sdcmp;						/* Steim Number of samples returned */
short final;						/* ptr to data[] with last integrations*/
short recursion=0;					/* always zero for Steim */
short flip=0;						/* indicates used native endedness*/
short ignore=0;						/* Number of IC to ignore in midframe*/
short statcode;						/* status code returned by compression*/
short level;						/* compression level (1,2, or 3)*/
int ipnt;							/* -> input data during decompression*/
int dpnt;							/* pointer to data output buf */
long unpacked[300];					/* temp buffer for unpacking data */
double rate;						/* place where rate is decoded */
static int lastseq;
int iseq;
char aseq[8];
static long zeroloop;
char line[100];
extern int first_warn;
char *asctim();
error:
p=bufin;							/* pointer to input buffer */
err=0;
nbytes=256;
dpnt=0;									/* samples decompressed */
while ( (err=readtcp(&tcp,bufin,nbytes)) <= 0)  /* try to read some data */
{	usleep(10000);					/* Let some time go by and try again */
	if(err < 0)
	{		fprintf(logout,"illegal return from readtcp=%d %x\n",err,errno);
	} else 
	{	zeroloop++;
		if( (zeroloop % 5000) == 0) 
		{	fprintf(logout,"%s zero loop = %d\n",asctim(),zeroloop);
			exit(555);
			zeroloop=0;
		}
	}
}
if(err == 0) fprintf(logout,"FEEDME : EOF on Terminal read\n");
if(err > 0) {
	if(dbg) 
	{	fprintf(logout,"          Got %d \n   0: ",err);		
		for(i=0; i< err ; i++) 
		(( i% 20) == 19) ? fprintf(logout,"%3d\n%4d: ",bufin[i],i) : 
						fprintf(logout,"%3d ",bufin[i]);
		fprintf(logout,"\n: ");
	}
}

while (nbytes > 0)
{
	if(dbg)fprintf(logout,"nb=%d inpnt=%d expect=%d used=%d sync=%d %s\n",
				nbytes,inpnt,expect,(int) (p-bufin), sync,seed_name);
top:
	if(sync)
	{	ncopy=(nbytes >= expect-inpnt) ? expect-inpnt : nbytes;
		memcpy(&buf[inpnt],p,ncopy);	/* move bytes to input buffer */
		inpnt+=ncopy;
		p+=ncopy;						/* move input buffer pointer */
		nbytes-=ncopy;					/* how manyleft in input */
 		if( inpnt >= expect)		/* buf has whole record */
		{	pnt=(struct _sdr_hdr *) &buf[0];/* point to hdr */
			if(pnt->station_id[0] == seed_name[0] && 
			   pnt->station_id[1] == seed_name[1] && 
			   pnt->station_id[2] == seed_name[2] )	
			{
				iy=pnt->time.year; id=pnt->time.day; ih=pnt->time.hour;
				im=pnt->time.minute; is=pnt->time.second;
				ms=pnt->time.ticks;
				ms=ms/10;
				*tc=maknsn(iy,id,ih,im,is,ms,leap);
				nsnint(*tc,&iy,&id,&ih,&im,&is,&ms,&leap);
				memcpy(aseq,pnt->seq_no,6);
				aseq[6]=0;
				iseq=atoi(aseq);
				if(iseq != lastseq+1) 
				{
					printf("Seq err is %d should be %d\n",iseq,lastseq+1);
					fprintf(logout,"%s   *** Seq error is %d should be %d\n",
						name,iseq,lastseq+1);
				}
				lastseq=iseq;
/* 				if(seed_name[0] == 'H' && seed_name[1] == 'I' &&
					seed_name[2] == 'A')*/
				if(dbg)
 					fprintf(logout,
"seq=%c%c%c%c%c%c D=%c stat=%c%c%c%c%c %c%c %c%c%c %c%c nsamp=%d rate=%d %d %d %d-%2d:%2d:%2d.%3d nblks=%d\n",
				pnt->seq_no[0],pnt->seq_no[1],pnt->seq_no[2],pnt->seq_no[3],
				pnt->seq_no[4],pnt->seq_no[5],
				pnt->data_hdr_ind, pnt->station_id[0], pnt->station_id[1],
				pnt->station_id[2],pnt->station_id[3], pnt->station_id[4],
				pnt->location_id[0],pnt->location_id[1],
				pnt->channel_id[0],pnt->channel_id[1],pnt->channel_id[2],
				pnt->network_id[0],pnt->network_id[1],
				pnt->num_samples,
				pnt->sample_rate_factor, pnt->sample_rate_mult,
				iy,id,ih,im,is,ms,pnt->num_blockettes);
				inpnt=0;
				memcpy(name,&(pnt->station_id[0]),5);			/* get name*/
				for(i=0; i<5; i++) if(name[i] == ' ') name[i]=0;/* char string*/
				memcpy(comp,&(pnt->channel_id[0]),3);	/* save station/name*/
				comp[3]=0;								/* make it a string */
				*ich=-10;
				
				if(comp[0] == 'L' && comp[1] == 'H') *ich=3;
				if(comp[0] == 'B' && comp[1] == 'H') *ich=0;
				if(*ich == -10) goto top;				/* unknown component*/
				if(seed_location[0] != 0 && pnt->location_id[0] != ' ')
				{	if( seed_location[0] != pnt->location_id[0] ||
						seed_location[1] != pnt->location_id[1]) goto top;
				}
/*
	In Oct 1998 ASL started using "location" code to differentiate two 
	seismometers at the same site.  When then did this they would normall 
	use a "00" and "10" set.  At other sites "location" is "  ".  This is
	here so when they do this, we get notified so we can update the vdlmom.setup
	to reflect the change 
*/
				if(seed_location[0] == 0 && pnt->location_id[0] != ' ' &&
					first_warn <= 0)
				{	first_warn++;
					strcpy(line,"Mail -s \"");
					strcat(line,name);
					strcat(line,
					" has location code!\" ketchum@gldfs.cr.usgs.gov ");
					strcat(line,"</dev/null");
					errno=0;
					err=system(line);
					fprintf(logout,"err=%d errno=%d %s\nLine=%s\n",err,errno,
						strerror(errno),line);
				}
					
				if(comp[2] != 'Z' && comp[2] != 'E' && comp[2] != 'N' &&
				   comp[2] != '1' && comp[2] != '2') goto top;
				if(comp[2] == 'E' || comp[2] == '2') *ich+=1;
				if(comp[2] == 'Z') *ich+=2;
/*				for(i=0; i<nchannels; i++) 
				if(strcmp(name,statnames[i]) == 0 && strcmp(comp,comps[i] == 0)
				{	*ich=channels[i];
					ch=i;
					break;
				}*/
				if(*ich < 0 || *ich >6) 
				{	if(dbg) fprintf(logout,"Comp not found %s %s\n",name,comp);
					goto error;
				}
				/* point at data blocket*/
				blk1000=(struct _blockette_1000 *)&buf[pnt->first_blockette];
				fprintf(logout,"form=%d order=%d rec_len=%d\n",blk1000->format,
					blk1000->word_order, blk1000->data_rec_len);
				expect=256;
				if(blk1000->data_rec_len == 8) expect=256;
				if(blk1000->data_rec_len == 9) expect=512;
				level=1;						/* assume steim I */
				for(i=0; i<pnt->num_blockettes; i++) 
				{		
					if(blk1000->hdr.type != 1000) 
					{	if(blk1000->hdr.type == 201) 
						{	fprintf(logout,"Murdock Hutt Detector\n");
							goto error;
						}
						fprintf(logout,"Not blk1000! %d\n",blk1000->hdr.type);
						blk1000=(struct _blockette_1000 *) 
								((char *)blk1000 + blk1000->hdr.next);
						goto error;
					}
					else
					{	level=0;
						if(blk1000->format == 10) level=1;/* Steim I */
						if(blk1000->format == 11) level=2;/* Steim II */
						if(level <= 0) 
						{	fprintf(logout,"Unsupported encoding=%d\n",
								blk1000->format);	
							goto error;
						}
					}
				}
				ipnt=pnt->first_data;				/* offset to first data*/
				dpnt=0;								/* offset to idat for data*/
				if(icsteim[1][*ich] == 212374000) {/* is it a beginning?*/
					sdcmp=0;
				} else {
					sdcmp=1;
					unpacked[1]=icsteim[0][*ich];	/*last words restored */
					unpacked[2]=icsteim[1][*ich];	/* set other word */
				}
				for(i=0; i<MAX_DATA; i++) idat[i]=0;/* zero buffer */
				if(dbg) fprintf(logout,
					"%s %s ich=%d ipnt=%d dpnt=%d expect=%d level=%d sdcmp=%d\n"
					,name,comp,*ich,ipnt,dpnt,expect,level,sdcmp);
				while (ipnt < expect) {
					sdcmp=decompress_frame(			/* decompress a single frm*/
					/*(compressed_frame *)&buf[ipnt],*/
					&buf[ipnt],
					unpacked,&final,sdcmp,level,	/* using unpacked for data*/
					recursion,flip,ignore,&statcode);
					memcpy((char *) &idat[dpnt],(char *) &unpacked[2],
							sdcmp*sizeof(long));
					if(dbg) {
						fprintf(logout,
						"Steim dcon=%d final=%d level=%d rec=%d flip=%d ign=%d stat=%d ipnt=%d dpnt=%d\n",							
							sdcmp,final,level,recursion,flip,ignore,statcode,
							ipnt,dpnt);
						fprintf(logout,"data(0,1)=%d %d  end=%d %d %d\n",
							unpacked[0],unpacked[1],unpacked[sdcmp-1],
							unpacked[sdcmp],unpacked[sdcmp+1]);
					}
					if(dferrorfatal(statcode,stdout)) {	/* process fatal errs */
						fprintf(logout,
							"%s %s *** Decompress_frame reports fatal error!\n",
							name,comp);
						break;
					}
					dpnt+=sdcmp;			/* where do we put next data */
					ipnt+=64;				/* increment to next frame to proc*/
				}
				icsteim[0][*ich]=unpacked[sdcmp];
				icsteim[1][*ich]=unpacked[sdcmp+1];	/* save IC for next time */
				if(dbg) fprintf(logout,
					"%s %s Steim dcmprs: ns=%d dpnt=%d ic=%d %d\n",
					name,comp,	pnt->num_samples,
					dpnt,icsteim[0][*ich],icsteim[1][*ich]);
				if(dpnt != pnt->num_samples) fprintf(logout,
					"%s %s *** # smp dcmprss err ns=%d dpnt=%d\n",
					name,comp,pnt->num_samples,dpnt);
				rate=pnt->sample_rate_factor;
			}
			else
			{	sync=0;
				fprintf(logout,"   ****** Lost sync name=%c%c%c%c%c %c%c%c seq=%c%c%c%c%c%c\n",
				pnt->station_id[0], pnt->station_id[1],
				pnt->station_id[2],pnt->station_id[3], pnt->station_id[4],
				pnt->channel_id[0],pnt->channel_id[1],pnt->channel_id[2],
				pnt->seq_no[0],pnt->seq_no[1],pnt->seq_no[2],pnt->seq_no[3],
				pnt->seq_no[4],pnt->seq_no[5]);

				nbytes+=ncopy;
				p-=ncopy;
				inpnt-=ncopy;			/* correct pointers to before error*/
				goto top;
			}
		} 
		else							/* not enough data to satisfy request*/
		{	/*fprintf(logout,"put partial at end inpnt=%d nb=%d\n",inpnt,nbytes);*/
			nbytes=0;
		}	
	}
	else
	{	fprintf(logout,"%s Look for sync nbytes=%d\n",name,nbytes);
		for(i=0; i<nbytes-3; i++) 	
		{	if(bufin[i] == seed_name[0] && bufin[i+1] == seed_name[1] && 
			 bufin[i+2] == seed_name[2] )
			{	k=i-8;						/* index to end of buffer */
				fprintf(logout,"%s sync up index=%d k=%d\n",name,i,k);
				if(k >= 0) 					/* is it available to us?? */
				{	inpnt=0;				/* how much data in buf */		
					p+=k; 					/* point in input buffer to next*/
					nbytes-=k;				/* how many points in inbuf*/
					sync=1;					/* found it */
				}
			}
			if(sync) break;					/* leave for loop */
		}
		if( !sync) break;					/* no more data left, set up exit*/
	}
}						/* end of while on input data */
if(dpnt > 0) return dpnt;
goto error;
}						/* end of subroutine */
