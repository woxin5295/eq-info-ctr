#include <stdio.h>
#include <errno.h>
#include "vdl.h"			/* Include file for the NSN project*/
#include "vdlqsub.h"			/* Declare QSUB prototypes and globals */

extern FILE *logout;

char *strerror(errnum)
int errnum;
{	extern int sys_nerr;
	extern char *sys_errlist[];
	if(errnum < 0  || errnum > sys_nerr) return "Error Unknown";
	return sys_errlist[errnum];
}
/*********************** COMPRESS *********************************
*
*	calling Sequence :	
*	argument 		description
*	ch				Pointer to channel structure to bring compression up to date
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
 *	Date Modified Description of Mod
*	20-apr-92	  Base Functionality
**********************************************************************/
#ifdef __STDC__
  int compressit(struct chan_desc *ch)
#else
  compressit(ch)
  struct chan_desc *ch;
#endif
{
/*
	COMPRESS will compress data on CH from the current CMPPNT for the channel
	to the latest sample in ring buffer.  It will cut off data if the channal
	if the TRIGDOWN goes negative and the channel is not marked continuous 
*/
	short last;
	short lastold;
	extern int gbldoy;						/* connect to DOY */
	extern int phmode;						/* is this a phoenix type? */
	extern struct chan_desc *cmpch;			/* Put buf uses this to know channel*/
	extern short detect_seq;				/* the detection sequence # */
	int nsamp,nsamp2;
	int idet,j;								/* scratch variables */
	extern FILE *lp;							/* connect to log output */
	if(ch->cmppnt == ch->ipnt) return 0;	/* no data to process */
	cmpch=ch;							/* save global channel in process*/
/*
	check if the trigger has reach shutdown or the absolute maximum shutdown
*/
	last=0;
	if(ch->continuous ==0) last=(ch->trigdown < 0) ? 1 :0;	/* set last based on trigdown */
	if(ch->continuous ==0 && ch->trigtotal > ch->trigabshutdown) {
		last=1;
		fprintf(lp,"%S Abs shutdown trgtot=%d abs=%d tdwn=%d\n",
			ch->txt,ch->trigtotal,ch->trigabshutdown,ch->trigdown);
	}
	lastold=last;
	if(ch->cmppnt < ch->ipnt) {
		nsamp2=0;
		nsamp=ch->ipnt-ch->cmppnt;			/* how many samples */
		cmprs(&ch->memex,nsamp,ch->ring+ch->cmppnt, &last,ch->cmp);
		ch->trigtotal+=nsamp;
		ch->cmppnt+=nsamp;					/* update where to compress from next*/
		ch->trigdown-=nsamp;				/* trig down */
	} else {
		nsamp=ch->max- ch->cmppnt;			/* number of samps to end */
		cmprs(&ch->memex,nsamp, ch->ring+ch->cmppnt, &last, ch->cmp);
		ch->trigtotal+=nsamp;				/* inc total compressed */
		ch->trigdown-=nsamp;				/* reduce # samples left to compress */
		if(last == lastold) {
			nsamp2=ch->ipnt;				/* set # of samples to compress */
			cmprs(&ch->memex,nsamp2,ch->ring, &last,ch->cmp);
			ch->trigtotal+=nsamp2;			/* up total compressed */
			ch->trigdown-=nsamp2;			/* reduce remaining trig length */
			ch->cmppnt=ch->ipnt;			/* update next place to compress */
		}
	}
	if(last !=lastold) {					/* is this the last one */
		if(last == 0) {						/* did we shutdown  in cmpress?*/
			if(ch->nsamp_qd > 5) {
				ch->triggered=0;/* trigger just turned off */
			}	
			ch->trigdown=-32000000;			/* indicate shut down */
		}
/*		if( ch->trig_chan > 0) */
			fprintf(lp,
			"%3s End of cmprs. Lst=%d old=%d chan=%d pnts=%d tdwn=%d \n",
			ch->txt,last,lastold,ch->stat_chan,ch->trigtotal,ch->trigdown);
		ch->trigtotal=0;
		ch->trigdead=100000000;
		ch->trigabshutdown=10000000;
	}
	return (int) (nsamp+nsamp2);		/* return number of points compressed*/
}
/*
/*********************** INITMEMORY *********************************
*
*	calling Sequence :	void initmemory()
*	argument 		description
*	NONE
*
*	Programmed by : D.C. Ketchum
*	Created :		June 1991
*	Date Modified Description of Mod
*	July 8, 1993	Added memory managment of events, LP Queue
**********************************************************************/
void initmemory()
{
	extern unsigned char *freelist;
	unsigned char *malloc();
	extern int meminit;
	extern int MAX_CH;
	extern unsigned long *cmpbufs;				/* Place for memory bufs for compres*/
	if(meminit ==0 ) {
		meminit=1;								/* Tell forground memory is alloc*/
		cmpbufs=(unsigned long *) malloc(2048*(MAX_CH+1));	/* Memory for compression */
		memset(cmpbufs,0,2048*(MAX_CH+1));		/* Clear cmprs buffer memory*/
	}
	return;

}
#ifdef __STDC__
  void dmpit(char * gb)
#else
  void dmpit(gb)
  char * gb;
#endif
{
	int i;
	for(i=0; i<20; i++) fprintf(logout,"%d ",*(gb+i));
	fprintf(logout,"\n");
}
#ifdef __STDC__
  int contrg(int detect_seq, struct nsntime tc, struct chan_desc *ch)
#else
  contrg(detect_seq,tc,ch)
  int detect_seq;
  struct nsntime tc;
  struct chan_desc *ch;
#endif
{
	struct gomberg gb;				/* get a gomberg packet */
	extern int net,node;			/* connect to netid and node id*/
	extern int gbldoy;				/* connect to day of year */
	extern int ttpath;				/* connect to path of output stream */
	extern int continuous_phoenix;
	int ierr;
	gb.lead1=27; gb.lead2=3;
	gb.routeid=net;					/* set network ID */
	gb.nodeid=node;					/* Set node # */
	if(continuous_phoenix) 
	{	gb.chanid=ch->stat_chan;
		gb.flags=NSN_FLAGS_CONTINUOUS | NSN_FLAGS_TRIGGER;
	} else
	{	gb.chanid=0;				/* control packet */
		gb.flags=NSN_FLAGS_CONTINUOUS;	/* initially set continuous flag */
	}
	gb.tc=tc;						/* set time per user */
	gb.format=255;					/* NSN trigger packet */
	gb.doy=gbldoy%256;				/* set the doy byte */
	gb.detseq[0]=detect_seq % 256;	/* set lower byte of detection seq */
	gb.detseq[1]=detect_seq / 256;	/* set upper byte of detection seq */
	gb.numbyt[0]=14+6;				/* low order packet length */
	gb.numbyt[1]=0;
	ierr=writeoutgb(&gb,20);
	if(ierr < 0) fprintf(logout,"Contrg write err=%d errno=%x\n",ierr,errno);
}

/*********************** PUTBUF *********************************
*
*	calling Sequence :	putbuf(npt,cmp,last,ipt,partial)
*	argument 	description
*	npt			Number of points compressed in buffer.
*	cmp			The compression buffer with data .
*	last		The LAST flag, If true this is the last compression buf for detect
*	ipt			Pointer to next byte in compression buffer to use.  We use it to
*				pack away the 4 bytes stored at the end of the packet, and to
*				know the length of the compression frame.	
*	partial		if non-zero this is a partial update call.  Just send bytes needed
*				to build record at other end and set partial flag.
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
*	Date Modified Description of Mod
	20-apr-92		Base Functionality
**********************************************************************/
#ifdef __STDC__
  void putbuf(short npt, unsigned char *cmp, short last, short ipt, int partial)
#else
  void putbuf(npt,cmp,last,ipt,partial)						
  short npt,ipt;
  unsigned char *cmp;
  short last;						/* set if last of compression*/
  int partial;					/* set non-zero if partial update */
#endif
{
/*
	COMPRESSION routine BUFOUT calls this user supplied routine.  The other 
	piece of information is in CMPCHAN a global variable which points to the
	channel structure for the channel being compressed.  In this way all 
	information needed to process the channel is known.
*/
	extern int ttpath;					/* path to serial output */
	extern int tcp_comm;				/* if true, tcp circuit */
	extern dbgmem,dbgmem2;				/* Memory debug flgs. */
	extern struct chan_desc *cmpch;		/* pointer to this channel */
	extern struct chan_desc *ch;
	extern int node,net;				/* hook to netid/routeid and node */
	extern int dbgpkt;					/* is debuging output of packets on */
	int msadd,i,err,j;					/* misc indexes, etc */
	extern FILE *lp;						/* connect to log path */
	long yr,day,hr,min,sec,ms;			/* Time translation vars */
	struct gomberg *gb,gbbuf;			/* pointer to output buffer */
	int ierr;							/* error return variable*/
	int pnt;							/* used to compute offset buf samp for time*/
	short itmp;
	char text[100];
	extern char tag[];
	strcpy(text,tag);
rexmit:
	gb=&gbbuf;							/* set pointer to buffer*/
	gb->lead1=27;gb->lead2=3;			/* set lead in characters */
	gb->routeid=net;					/* set routing/network ID */
	gb->nodeid=node;					/* set node id for this station */
	gb->chanid=cmpch->stat_chan;		/* set station channel byte */
	gb->tc=nsnadd(cmpch->cmptc,-(cmpch->delay));/* set time code corrected for delay*/
	if(dbgpkt) {
	fprintf(logout,"Put buf npt=%d ipt=%d last=%d ch=%d prt=%d %d",
		npt,ipt,last,cmpch->stat_chan,partial,cmpch->partial);
	fprintf(logout,"ic=%4x %4x npt=%4x\n",*cmp,*(cmp+1),*(cmp+2));
	nsnint(gb->tc,&yr,&day,&hr,&min,&sec,&ms);
	fprintf(logout," %4d %3d-%2d:%2d:%2d.%3d detseq=%2x %2x delay=%d lprt=%d\n",
		yr,day,hr,min,sec,ms,cmpch->detseq[0],cmpch->detseq[1],
		cmpch->delay,cmpch->lastpartial);
	fflush(logout);
	}

	gb->format=0;							/* NSN type packet */
	gb->flags=(last !=0) ? NSN_FLAGS_EOF : 0;/* initally set no flags */
	if(last !=0) fprintf(logout,"%s Putbuf : EOD set %d  tdwn=%d\n",
		cmpch->txt,cmpch->stat_chan,cmpch->trigdown);
	if(cmpch->trigflag !=0) {
		gb->flags |=NSN_FLAGS_TRIGGER;		/* mark trigger on */
		cmpch->trigflag--;
	}
	if(cmpch->continuous !=0) gb->flags|=NSN_FLAGS_CONTINUOUS;/* set continuouse flag */
	gb->doy=cmpch->doy;						/* set the DOY of detection*/
	gb->detseq[0]=cmpch->detseq[0];			/* Set detection sequence */
	gb->detseq[1]=cmpch->detseq[1];			/* Ditto */
	if(dbgpkt) fprintf(logout,"prt=%d\n",cmpch->partial);
	if(cmpch->partial == 0) {				/* if not the partial case */
		gb->seq=cmpch->seq;					/* set sequence numbber */
		cmpch->seq++;						/* bump packet sequence */
		memcpy(&gb->buf[0],cmp,NBK);		/* transfer block */
		if(last > 0) {
			for (i=NBK-4; i< NBK ;i++) {	/* copy last four bytes of record to */
				gb->buf[ipt]=gb->buf[i];	/* last open positions in buffer */
				ipt++;						/* This makes transmissions shorter*/
			}
		}
/*			Compression packet is ipt long.  (Actually ipt is next location in
			bytes to put data.  Since C counts arrays from zero it is also the
			length of the buffer.)  Gomberg header is 14.  The
			NSN data frame header is 6. 
*/
		gb->numbyt[0]=(ipt+14+6) % 256;		/* set low order of # of bytes */
		gb->numbyt[1]=(ipt+14+6) / 256;		/* High over of number of bytes */
		nsnint(gb->tc,&yr,&day,&hr,&min,&sec,&ms);
		cmpch->cmptotal+=npt;				/* total points since beginning */
		if(dbgpkt) {
			 fprintf(lp," %s %4d %3d-%2d:%2d:%2d.%3d detseq=%2x%2x ch seq=%d npt=%d lst=%d net=%d ch=%d tot=%d delay=%d\n",
			cmpch->txt,yr,day,hr,min,sec,ms,cmpch->detseq[0],cmpch->detseq[1],
			cmpch->seq,npt,last,gb->routeid,gb->chanid,cmpch->cmptotal,cmpch->delay);
			fprintf(lp," %s cmpch=%x ipt=%d\n",cmpch->txt,cmpch,ipt);
			fflush(logout);
		}

		pnt=(cmpch->cmpstart + cmpch->cmptotal) % cmpch->max;/* offset  beg of ring*/
		cmpch->lastpartial=0;				/* set full update done last */
		cmpch->cmptc=buftim(cmpch,pnt);		/* get time of this sample */
		ierr=writeoutgb(gb,ipt+14+6);
		if(ierr < 0) {
			fprintf(logout,"Write data port err=%d errno=%x-%s\n",
				ierr,errno,strerror(errno));
		}
	} else {								
 /*
	Partial Update
*/
		gb->flags |=NSN_FLAGS_PARTIAL;		/* mark parial update */
		gb->seq=cmpch->seq;					/* set sequence numbber */
		j=ipt+1-cmpch->lastpartial;			/* Number of new byte*/
		if(j == 1) {
/*			fprintf(logout,"No data on partial... partial=%d ipt=%d \n",
				partial,ipt);*/
			if(partial != 0) return;		/* no new data since last time!*/
		}
		if(j <= 0) {
			fprintf(logout,"%s J<0 j=%d ipt=%d last=%d\n",
				cmpch->txt,j,ipt,cmpch->lastpartial);
			j=1;
		}
		itmp=cmpch->lastpartial;			/* save lastpartial for later */
#ifdef _INTEL
		if(dbgpkt) {fprintf(logout,"INTEL=%d %d %d\n",
				_INTEL,j,npt);fflush(logout);}
		i2swap(npt,&gb->buf[0]);
		if(dbgpkt) {fprintf(logout,"INTEL Swap\n");fflush(logout);}
		memcpy(&gb->buf[2],&itmp,2);
#else
		if(dbgpkt) {fprintf(logout,"NO INTEL %d\n",j);fflush(logout);}
		memcpy(&gb->buf[0],&npt,2);			/* move data into xmit buf */
		i2swap(itmp,&gb->buf[2]);			/* Get pointer to where in buf*/
#endif
		if(dbgpkt) {fprintf(logout,"memcpy j=%d\n",j);fflush(logout);}
		memcpy(&gb->buf[4],cmp+cmpch->lastpartial,j);
		cmpch->lastpartial=ipt;				/* Offset in buf with data */
		if(j+24 > 2038) {					/* Is this too long? */
			cmpch->lastpartial=ipt-6;		/* take 6 bytes off length */
			gb->numbyt[0]=(j+4+14) % 256;	/* adjust bytes in packet */
			gb->numbyt[1]=(j+4+14)/256;		/* High order of bytes */
			fprintf(logout,"Part write too long j=%d lastp=%d\n",
				j,cmpch->lastpartial);
			ierr=writeoutgb(gb,j+4+14);/* write out all but 6 bytes*/
			if(ierr < 0) {
				fprintf(logout,"Partial write 10 err=%d errno=%x-%s\n",
					ierr,errno,strerror(errno));
			}
			goto rexmit;
		}
		if(partial == 0) {					/* is this the end of record */
			gb->flags|=NSN_FLAGS_EOR;		/* last update of partial record*/
			cmpch->lastpartial = 0;			/* indicate starting new record*/
			cmpch->cmptotal+=npt;			/* total points since beginning */
			nsnint(gb->tc,&yr,&day,&hr,&min,&sec,&ms);
			pnt=(cmpch->cmpstart + cmpch->cmptotal) % cmpch->max;/*offset beg of ring*/
			cmpch->cmptc=buftim(cmpch,pnt);		/* get time of this sample */
			cmpch->seq++;					/* bump packet sequence */
			if(dbgpkt) fprintf(logout,"%s partial done\n",cmpch->txt);
			j--;							/* one less byte for end */
			if(last > 0) {					/* if this is last buf of chain */
				j++;						/* assumed zero byte is not true*/
				for (i=NBK-4; i< NBK; i++) {/* Copy last IAN to last 4 bytes*/
					gb->buf[j+4]=cmp[i];	/* move the IAN */
					ipt++;					/* IPT does not accnt for IAN */
					j++;					/* next byte in buf */
				}
			}
		}
/* number of bytes is length of buffer+1, I*2 npt, I*2 Pointer, 14 char gomberg
		header, 6 byte data header
*/
		gb->numbyt[0]=(j+4+14+6) % 256;		/* set low order of number of bytes */
		gb->numbyt[1]=(j+4+14+6) / 256;		/* High over of number of bytes */
		nsnint(gb->tc,&yr,&day,&hr,&min,&sec,&ms);
		if(dbgpkt) {fprintf(lp,"P%s %4d %3d-%2d:%2d:%2d.%3d detseq=%2x%2x ch seq=%d npt=%d ipt=%d net=%d ch=%d\n",
			cmpch->txt,yr,day,hr,min,sec,ms,cmpch->detseq[0],
			cmpch->detseq[1],cmpch->seq,npt,ipt,gb->routeid,gb->chanid);
			fflush(logout);}
/*		fprintf(lp,"form=%d flags=%x byt=%d %d delay=%d\n",
			gb->format,gb->flags,gb->numbyt[0],gb->numbyt[1],cmpch->delay);*/
/*		dmpit(gb);*/
		ierr=writeoutgb(gb,j+4+14+6);	/* write out first 10 bytes*/
		if(ierr < 0) {
			fprintf(logout,
				"%s Part write 10 err=%d errno=%x j=%d ipt=%d lpart=%d-%s\n",
				cmpch->txt,ierr,errno,j,ipt,cmpch->lastpartial,strerror(errno));
		}
	}
}
/*********************** PUTPOW *********************************
*
*   calling Sequence :  putpow(exp,ir,tcpow)
*   argument        description
*   exp             Contains the exponent of the FFT pow 
*   ir              Contains the POW in raw form as array of longs
*   tcpow           Time code at beginning of this pow array
*
*   Programmed by : D.C. Ketchum
*   Created :       Apr 1992
*   Date Modified Description of Mod
*   20-apr-92     Base Functionality
***********************************************************************/
putpow(exp,ir,tcpow)
int exp;
struct nsntime tcpow;
long ir[8];
{
    static struct {
        char tc[6][6];
        short exp[6];
        long ir[6][8];
    } pow;
    extern int popout,pushout,outlist[];
    struct gomberg *gb,gbbuf;
    extern int node,net;                /* hook to netid/routeid and node */
    extern struct gomberg *qout;        /* hook to output area */
 	extern int ttpath;					/* Path to write data */
    static int powpnt=0;
	static struct nsntime tcpow0;		/*save 1st time code for putting ingb*/
	extern struct chan_desc *cmpch;			/* Put buf uses this to know channel*/
    extern int sequence;
    extern int gbldoy;
	short exp2;
	char text[100];
    int i,npage,err;
    int yr,day,hr,min,sec,ms,leap;
    nsnint(tcpow,&yr,&day,&hr,&min,&sec,&ms);
	memcpy(&pow.tc[powpnt][0],&tcpow,2);
	memcpy(&pow.tc[powpnt][2],&tcpow.ms,4);
	if(powpnt == 0) tcpow0=tcpow;			/* save first time code for later */
/*	fprintf(logout,"exp=%d ir=%d %d %d %d %d \n",exp,ir[0],ir[1],ir[2],ir[3],ir[4]);*/
	exp2=exp;
    i2swap(exp2,(unsigned char *) &pow.exp[powpnt]);
    for (i=0; i<8; i++) i4swap(ir[i],(unsigned char *) &pow.ir[powpnt][i]);
    powpnt++;
    if(powpnt <6) return 0;
    powpnt=0;                   /* reset counter */
    gb=&gbbuf;
    gb->lead1=27;gb->lead2=3;           /* set lead in characters */
    gb->routeid=NSN_STATUS;             /* set routing/network ID */
    gb->nodeid=node;                    /* set node id for this station */
    gb->chanid=127;                     /* set station channel byte */
    gb->tc=tcpow0;     		              /* set time code */
    gb->format=0;                       /* NSN type packet */
    gb->flags=NSN_FLAGS_POWER;          /* initally set POW flags */
    gb->doy=gbldoy%256;                 /* set the DOY of detection*/
    gb->seq=sequence % 256;sequence++;  /* set sequence numbber */
    gb->detseq[0]=net;                  /* Set detection sequence to netid*/
    gb->detseq[1]=0;                    /* Ditto */
    memcpy(&(gb->buf[0]),&(pow.tc[1]),240); /* transfer block */
    gb->numbyt[0]=254;                  /* set low order of number of bytes */
    gb->numbyt[1]=0;                    /* High over of number of bytes */
	err=writeoutgb(gb,254);				/* write first 10 bytes */
	if(err < 0) {
		fprintf(logout,"%s Part write10 err=%d errno=%x-%s\n",
				cmpch->txt,err,errno,strerror(errno));
	}
/*  printf("putpow rt=%d nd=%d dt=%d\n",gb->routeid,gb->nodeid,gb->detseq[0]);*/
    return 0;
}
 
