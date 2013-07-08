#include <ctype.h>
#include <string.h>			/* changed from strings.h for earthworm */
#include <stdio.h>	
#include <errno.h>
#include <math.h>			/* include math descriptor */
#include "vdl.h"			/* Include file for the NSN project*/
#include "vdlqsub.h"		/* Declare QSUB prototypes and globals */
#include "ifft.h"			/* Declare FFT and Power calc protos */
#include <time.h>			/* system time structure */
void newtrig();
void detect();
int procheli();
extern FILE *logout;
/*********************** PROC *********************************
*
*	calling Sequence :	proc(ia,ichan,nsamp,tc)
*
*	Arg		Description
*	ia		256 data point (should be called with 50 % overlap
*	ich		Channel number data is for (0-3=High freq, 3-5=LP)
*	nsamp	# of samples in IA from this channel
*	tc		Approximate time of beginning of this buffer
*
*	Programmed by : D.C. Ketchum
*	Created :	May 1991
*	Date Modified Description of Mod
*	20-apr-92	  Base Functionality
* 	1-dec-93	Modified for San Diego example
*	20-jan-95	Expand to use ring buffers for preevent and FFT memory
**********************************************************************/
#ifdef __STDC__
  void proc(long ia[], int ich, int nsamp, struct nsntime tc)
#else 
  void proc(ia,ich,nsamp,tc)
  struct nsntime tc;
  long ia[];				/* FFT buffer and Power spectra buffer*/
  int ich;				/* channel number of data in IA*/
  int nsamp;				/* number of samples to process  */
#endif
{
	double f;
	long ir[8];						/* power array */
	extern struct chan_desc *ch;/* Channel space */
	extern int gbldoy;				/* the current day of year */
	extern short detect_seq;		/* the current detect_seq */
	extern int SPFREQ;				/* digit rate for high frequency channels */
	extern int derive_lp;			/* connect to derive LP flag */
	extern char tag[];
	extern char logNam[];			/* filename of log file */
	extern FILE *lp;				/* path to log file */
	extern struct chan_desc *cmpch;	/* link to putbuf to know which channel is active*/
	long iacop[1040];				/* scratch buffer to perform FFT in */
	int err;
	int exp;						/* exponent for powers from IFFT */
	struct nsntime tc2;				/* scratch time code */
	extern int FFTLEN,FFTL2;		/* Attach to FFT length Globals */
	extern int phmode;
	int idiff;						/* temp variable for a difference */
	int iend;						/* temp variable for calc end of bufs*/
	int ns;							/* number of samples used in memcpy ops*/
	extern int fcalc_flag;			/* if true, print frequency calculation*/
	int i,j,msadd;
	int nchar;
	int lpch;						/* quick indexLP chan =ch[ich].derive_lp*/
	int iy,id,ih,im,is,ms,leap;		/* scratch time code broken apart */
	int iy2,id2,ih2,im2,is2,ms2;	/* another scratch time code */
	int iy3,id3,ih3,im3,is3,ms3;	/* another scratch time code */
	int ipntsav;					/* to cut off at midnight, save ipnt */
	int need_newtrig=0;				/* if need to declare newtrig for */
									/* triggered input data */
#ifdef DEBUG_PRINT
	fprintf(logout," %d %d",ch[ich].ipnt,ch[ich].freq);fflush(logout);
#endif

	/* look at this time code and compute expected time code (tc2) */
	nsnint(tc,&iy,&id,&ih,&im,&is,&ms,&leap);	/* get time code parts */
	tc2=ch[ich].tclastfeed;			/* tc2 is expected time */
	nsnint(tc2,&iy2,&id2,&ih2,&im2,&is2,&ms2,leap);/* expected*/
	ch[ich].tclastfeed=nsnadd(tc,(long) (nsamp*1000/ch[ich].freq_calc));
	/* expect next one */

	/* Is it a new day, If so, set new global DOY byte */
	if(id2 == gbldoy+1 || (id2 == 1 && gbldoy >=365) ) {
		gbldoy=id2;
		fprintf(logout,"Day rolling to %d ich=%d lpd=%d\n",
				id2,ich,derive_lp); fflush(logout);
		detect_seq=2;					/* a new day has dawned */
		fprintf(logout,"Closing the Log file %s \n",logNam);
		fflush(logout);
		fclose(logout);
		*(logNam+strlen(logNam)-1)=48+ (id2 % 10);
		logout=fopen(logNam,"w");		/* open the VDL log file */
		fprintf(logout,"Open New log file : %s %d\n",logNam,logout);
		print_status();					/* give status again */
	}
	
	/* If this is the first time, get a time and ignore continuity checks */
	if(ch[ich].tc.ms == -1) {
		ch[ich].tc=tc;					/* set time at beginning */
		ch[ich].cmptc=tc;				/* set time at beginning of trigger*/
		ch[ich].seq=1;					/* initialize channel sequence # */
		ch[ich].ipnt=0;					/* must be start of buffer */
		fprintf(logout,"%s Set init time %4d %d-%2d:%2d:%2d.%3d ch=%d\n",
			ch[ich].txt,iy,id,ih,im,is,ms,ich);
		fflush(logout);
		need_newtrig=1;					/* need a new trigger on this chan */

	/* Not first time, check data for continuity */
	} else {
		idiff=nsnsub(tc2,tc);					/* get difference in MS */
		if(abs(idiff) > 1000/ch[ich].freq) {	/* Discontinuity in the data! */
			if(lp != logout) {
				fprintf(lp,
				"%s Discont is  %d %d %d %d %d %d add=%d diff=%d frq=%d ipnt=%d ich=%d\n",
				ch[ich].txt,iy,id,ih,im,is,ms,ch[ich].ipnt*1000/ch[ich].freq,
				idiff,ch[ich].freq,ch[ich].ipnt,ich);
				fprintf(lp,"%s Discont exp %d %d %d %d %d %d\n",
				ch[ich].txt,iy2,id2,ih2,im2,is2,ms2);
			}
			fprintf(logout,"%s Discont is  %d %d %d:%d:%d.%3d add=%d diff=%d frq=%8.3f ipnt=%d ich=%d ns=%d\n",
				ch[ich].txt,iy,id,ih,im,is,ms,
				(long) (ch[ich].ipnt*1000/ch[ich].freq_calc),
				idiff,ch[ich].freq_calc,ch[ich].ipnt,ich,nsamp);
			fprintf(logout,"%s Discont exp %d %d %d:%d:%d.%3d\n",
				ch[ich].txt,iy2,id2,ih2,im2,is2,ms2);

			/* is there a trigger going on */
			if(ch[ich].trigdown > -30000000 || ch[ich].continuous != 0) {
				cmpch=&ch[ich];				/* mark channel being compressed*/
				if(ch[ich].seq == 1 && ch[ich].cmptotal < 2000)/* little data?*/
				{	 cmset(&ch[ich].memex,ch[ich].cmp);/* blow offthe channel */
					 fprintf(logout,"%s Blow off BB sq=%d ctot=%d\n",
						ch[ich].txt,ch[ich].seq, ch[ich].cmptotal);
				}
				else 
				{	cmfin(&ch[ich].memex,ch[ich].cmp);/* fin the channel */
					detect_seq++;			/* some data, new sequence */
				}
				if(abs(idiff) > 30000) 
					ch[ich].trigdown=-32000000;/* > 30 seconds, kill series*/
				ch[ich].triggered=0;
				ch[ich].seq=1;
			}
			if(ch[ich].derive_lp) {
				lpch=ch[ich].derive_lp;
				cmpch=&ch[lpch];			/* Mark channel being stopped */
				/* finish LP here also */
				if(ch[lpch].seq == 1 && ch[lpch].cmptotal < 600)/*too little?*/
				{	 cmset(&ch[lpch].memex,ch[lpch].cmp);/* blow it off */
					 fprintf(logout,"%s Blow off LP iseq=%d cmptot=%d\n",
					  ch[lpch].txt,ch[lpch].seq, ch[lpch].cmptotal);
				}
				else 
				{	cmfin(&ch[lpch].memex,ch[lpch].cmp);/* send it*/
					detect_seq++;			/* some data, new sequence */
				}
				fprintf(logout,"%s start new LP ich=%d lpch=%d\n",
					ch[lpch].txt,ich,lpch);
				ch[lpch].ipnt=0;				/* no LP data in ring now */
				ch[lpch].lppnt=0;			/* start deriving LP from new IPNT*/
				ch[lpch].tc=tc;				/* set time of first LP sample */
				ch[lpch].cmptc=tc;			/* new time to go */
				ch[lpch].cmpstart=0;
				ch[lpch].cmppnt=0;
				ch[lpch].cmptotal=1;
				ch[lpch].detseq[0]=detect_seq %256;/* low order of detect seq*/
				ch[lpch].detseq[1]=detect_seq/256;/* High order of detect seq*/
			}
			ch[ich].tc=tc;
			ch[ich].ipnt=0;					/* start at beginning of buffer */
			ch[ich].cmptc=tc;				/* set start time */
			ch[ich].cmpstart=0;				/* make pointers aggree */
			ch[ich].cmppnt=0;
			ch[ich].cmptotal=1;
			ch[ich].warmup=10;				/* force rewarming after discon*/
			if(phmode) ch[ich].fftpnt=FFTLEN+( (i*32) % FFTLEN);
			else ch[ich].fftpnt=FFTLEN;		/* next possible FFT time */
			need_newtrig=1;					/* if triggered input, need new */
			ch[ich].detseq[0]=detect_seq % 256;
			ch[ich].detseq[1]=detect_seq/256;
		}						/* data is continuous! drive on */
	}
/*
	Put data in ring buffers for later use.
*/
#ifdef DEBUG_PRINT
	fprintf(logout,"ich,ipnt,nsamp,max=%d %d %d %d\n",
		ich,ch[ich].ipnt,nsamp,ch[ich].max);fflush(logout);
#endif
	if( ch[ich].max < nsamp) {				/* too much data for one gulp? */
		fprintf(logout,
			"Ring buffer smaller than nsamp in proc nsamp=%d mx=%d ich=%d\n",
			nsamp,ch[ich].max,ich);			/* yes- bail! */
		exit(110);							/* give mother a chance to know why */
	}
	if( (ch[ich].ipnt+nsamp) <= ch[ich].max) { 	/* it will not lap ring */

		/* move data to ring buffer */
		memcpy(ch[ich].ring+ch[ich].ipnt,(char *) ia, nsamp*sizeof(long));
		ch[ich].ipnt+=nsamp;
/*		fprintf(logout,"cp %d ipnt=%d\n",nsamp*sizeof(long),ch[ich].ipnt);*/
	} else {
		iend=ch[ich].max-ch[ich].ipnt;			/* how many will fit */
		memcpy(ch[ich].ring+ch[ich].ipnt, (char *) ia, iend*sizeof(long));
		ch[ich].ipnt=0;							/* now at beginning */
		tc2=ch[ich].tc;						/* save old begin time */
		ch[ich].tc=nsnadd(tc,(long) (iend*1000/ch[ich].freq_calc));	/* time of point at start of buf */
		msadd=nsnsub(ch[ich].tc,tc2);			/* how man MS between wrap*/
		f=(double) ch[ich].max/(double) msadd*1000.;
		if(ch[ich].freq_calc == 0.){
			ch[ich].freq_calc=f;
			fprintf(logout,"%s initfreq calc=%f\n",
					ch[ich].txt,ch[ich].freq_calc);
		} else {
			ch[ich].freq_calc=(19.*ch[ich].freq_calc +f)/20.;/* weighted avg*/
			if(fcalc_flag) fprintf(logout,"%s frq calc2=%f %f max=%d add=%d\n",
					ch[ich].txt,f,ch[ich].freq_calc,ch[ich].max,msadd);
		}
		memcpy(ch[ich].ring, (char *) &ia[iend], (nsamp-iend)*sizeof(long));
		ch[ich].ipnt+=nsamp-iend;				/* putdate ipnt */
		nsnint(ch[ich].tc,&iy,&id,&ih,&im,&is,&ms,&leap);/* time code parts */
/*		fprintf(logout,"Time beg %s %d %d:%d:%d:%d.%d iend=%d\n",
			tag,iy,id,ih,im,is,ms,iend);
		fprintf(logout,"cp wrap iend=%d %d beg=%d %d ipnt=%d\n",			
			iend,iend*sizeof(long),nsamp-iend,(nsamp-iend)*sizeof(long),
			ch[ich].ipnt);
*/
	}
/*
		If a channel is triggered data on input, VDL want to forward it 
		along as is.  NSN data normally has the same "detection sequence" on
		all components for a given event.  We simulate this by making all
		of the components "cause" a trigger when they first come in and 
		any other components coming in later but within two minutes to use
		the same detection sequence
*/
	if(ch[ich].trig_chan < 0 && need_newtrig) 	/* is it triggered input data */
	{	for(j=0; j < 3; j++) if(ch[ich].forward[j] >= 0 && 
								ch[ich].forward[j] != ich)
		{	nsnint(ch[ch[ich].forward[j]].cmptc,
				&iy3,&id3,&ih3,&im3,&is3,&ms3,leap);/* expected*/
			fprintf(logout,"ck new detect=%d ich=%d %d %d %d %d:%2d:%2d\n",
			ich,ch[ich].forward[j],abs(nsnsub(tc,ch[ch[ich].forward[j]].cmptc)),
				iy3,id3,ih3,im3,is3);
			if( abs(nsnsub(tc,ch[ch[ich].forward[j]].cmptc)) < 120000) break;
		}
		fprintf(logout,"J=%d\n",j);
		if(j >= 3) 
		{	detect_seq++;				/* must be a new trigger */
			fprintf(logout,"New detection seq=%d\n",detect_seq);
		}
		newtrig(&ch[ich],0,detect_seq,0,100000);/* declare trigger */
		fprintf(logout,"%s New Triggered input : dt=%d %d %d %2d:%2d:%2d\n",
			ch[ich].txt,detect_seq,iy,id,ih,im,is);
	}

	/* If this is a continuous station, compress it and echk for day roll */
	if(ch[ich].continuous !=0) {			/* is this a continuous channel? */
		tc2=buftim(&ch[ich],ch[ich].ipnt-1);	/* What time is this sample ?*/
		nsnint(tc2,&iy2,&id2,&ih2,&im2,&is2,&ms2,&leap);	/* Break it apart */
#ifdef DEBUG_PRINT
		fprintf(logout,"cont ich=%d %d %d %2d:%2d:%2d.%3d doy=%d ipnt=%d\n",
				ich,iy2,id2,ih2,im2,is2,ms2,ch[ich].doy,ch[ich].ipnt);
		nsnint(ch[ich].tc,&iy3,&id3,&ih3,&im3,&is3,&ms3,&leap);
		fprintf(logout,"bef tc=%d %d %2d:%2d:%2d.%3d ipnt=%d\n ",
				iy3,id3,ih3,im3,is3,ms3,ch[ich].ipnt); fflush(logout);
#endif
/*
		If the time of the next sample is in the new day, cut off the data and
		start a new detection
*/
		if(ch[ich].doy != (id2 % 256)) {	/* day of year must change */
			ipntsav=ch[ich].ipnt;				/* save the pointer */
			ch[ich].ipnt=ch[ich].ipnt-(ih2*3600+im2*60+is2+1);/* adj to 00:00 */
			if(ch[ich].ipnt < 0) ch[ich].ipnt+=ch[ich].max;/* unwrap */
			fprintf(logout,
				"%s %d %d Roll Cont cmppnt=%d ipnt=%d adjust=%d to=%d %d %d -%2d:%2d:%2d\n",
				ch[ich].txt,ch[ich].doy,id2,ch[ich].cmppnt,
				ipntsav,(ih2*3600+im2*60+is2),ch[ich].ipnt,
				iy2,id2,ih2,im2,is2); fflush(logout);
			ns=compressit(&ch[ich]);				/* compress all data in ring buf*/
			fprintf(logout,"aft comp ns=%d cmppnt=%d ipnt=%d\n",
			ns,ch[ich].cmppnt,ch[ich].ipnt);
			cmpch=&ch[ich];									/* tell putbuf who is being cmprs*/
			cmfin(&ch[ich].memex,ch[ich].cmp);/* end old compression */
			ch[ich].cmppnt=ch[ich].ipnt;		/* reset compression ring pointer*/
			ch[ich].cmpstart=ch[ich].ipnt;	/* offset of first compression */
			ch[ich].ipnt=ipntsav;						/* restore ipnt */
			ch[ich].cmptc=buftim(&ch[ich],ch[ich].cmppnt);/* set compr time */
			nsnint(ch[ich].tc,&iy3,&id3,&ih3,&im3,&is3,&ms3,&leap);
			fprintf(logout,"bef tc=%d %d %2d:%2d:%2d.%3d ipnt=%d",
				iy3,id3,ih3,im3,is3,ms3,ch[ich].ipnt); fflush(logout);
			nsnint(ch[ich].cmptc,&iy3,&id3,&ih3,&im3,&is3,&ms3,&leap);
			fprintf(logout,"aft tc=%d %d %2d:%2d:%2d.%3d ipntsav=%d\n",
				iy3,id3,ih3,im3,is3,ms3,ipntsav); fflush(logout);
			ch[ich].doy=id2 % 256;				/* set day of year byte */
			ch[ich].detseq[0]=1;
			ch[ich].detseq[1]=0;
			ch[ich].detect_seq=3;
			detect_seq=2;									/* a new day has dawned */	
			ch[ich].seq=1;								/* start of new LP trigger */
			ch[ich].trigtotal=0;
			ch[ich].trigdead=100000000;		/* continuous do not have deads */
			ch[ich].cmptotal=1;				/* set total compress since change*/
			cmset(&ch[ich].memex,ch[ich].cmp);
		}
		ns=compressit(&ch[ich]);			/* compress all data in ring buf*/
	} else {
		if(ch[ich].trigdown > -3000000 && ch[ich].cmppnt != ch[ich].ipnt) {
			idiff=compressit(&ch[ich]);			/* compress triggered channel */
		}
			
	}

/*
	Helicorder processing
*/
	if(ch[ich].heli != 0) while (procheli(&ch[ich]) == 1) ;

/*
		FFT based  High gain
*/
	if(ch[ich].trig_chan <= 0) return;
	idiff=ch[ich].ipnt- ch[ich].fftpnt;
	if(idiff > ch[ich].max/2) {			/* fft is near beginning, ipnt nearend*/
		idiff-=ch[ich].max;
	}
	if(idiff < -ch[ich].max/2) {		/* ipnt is near beginning, fftpnt end*/
		idiff+=ch[ich].max;					/* How many unprocessed points*/
		if(idiff > ch[ich].max/2) idiff=0;	/* don't go too far */
	}
	while (idiff > 0) { 
		if(ch[ich].showdet) fprintf(logout,"Do fft %d ipnt=%d fftpnt=%d idiff=%d len=%d\n",
				ich,ch[ich].ipnt,ch[ich].fftpnt,idiff,FFTLEN); fflush(logout);
		idiff-=FFTL2;						/* count down this while loop */
		if(ch[ich].fftpnt < FFTLEN) {/* its split across the ring buf*/
			memcpy(iacop,ch[ich].ring+(ch[ich].max-(FFTLEN-ch[ich].fftpnt)),
				(FFTLEN-ch[ich].fftpnt)*sizeof(long));
			memcpy(&iacop[FFTLEN-ch[ich].fftpnt],ch[ich].ring,
				ch[ich].fftpnt*sizeof(long));
		} else {
			memcpy(iacop,ch[ich].ring+ch[ich].fftpnt-FFTLEN,
				FFTLEN*sizeof(long));
		}
/*		for (i=0; i<FFTLEN; i++) if( (i%10) == 0) {
			fprintf(logout,"%8d\n %d bef ",iacop[i],i);
		} else {
			fprintf(logout,"%8d ",iacop[i]);
		}
		fprintf(logout,"\n");*/
		exp=0;
		if( FFTLEN == 512) ifft512(&exp,iacop,ir);	/* compute FFT and powers*/
		if( FFTLEN == 256) ifft256(&exp,iacop,ir);	/* compute FFT and powers*/

		if( FFTLEN == 1024) ifft1024(&exp,iacop,ir);/* compute FFT and powers*/
		if(ch[ich].showdet) fprintf(logout,
			"Ret ipnt=%d fft=%d exp=%d pow=%d %d %d %d %d %d\n",
			ch[ich].ipnt,ch[ich].fftpnt,exp,ir[0],ir[1],ir[2],ir[3],ir[4]);
/*		for (i=0; i<30; i++) if( (i%10) == 0) {
			fprintf(logout,"%8d\n %d aft ",iacop[i],i);
			} else {
			fprintf(logout,"%8d ",iacop[i]);
		}
		fprintf(logout,"\n");*/
		tc2=buftim(&ch[ich],ch[ich].fftpnt-FFTLEN);
		detect(exp,ir,tc2,ich);				/* run detect algorithm on powers */
		ch[ich].fftpnt+=FFTL2;
		if(ch[ich].fftpnt > ch[ich].max) ch[ich].fftpnt-=ch[ich].max;
	}
	return;						/* this returns # of chans still triggered*/
}



/*********************** DETECT *********************************
*
*	calling Sequence :	detect(exp,ir,tc,ich)
*	argument 		description
*	exp				Power of two exponent from FFT to apply to IR powers
*	ir				Long words with POW as computed from FFT
*	tc				Time code of buffer
*	ich				Channel number (for access to histories )
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
*	Date Modified Description of Mod
*	20-apr-92	  Base Functionality
**********************************************************************/
#ifdef __STDC__
  void detect (int exp, long ir[8], struct nsntime tc,int ich)
#else
  void detect(exp,ir,tc,ich)
  int exp,ich;
  long ir[8];
  struct nsntime tc;
#endif
{
	extern short detect_seq;
	short last;
	extern struct chan_desc *ch;
	extern int posttime,leadtime;			/* post, pre event save times */
	unsigned char jtemp;
	int iss;
	int triggered;
	static int trig=0;
	extern FILE *lp;									/* connect to log path */
	extern int phmode;								/* single station or phoenix mode */
	int i,j;
	int iy,id,ih,im,is,ms,leap;			/* translated time variables*/
	if(ch[ich].sendpows) putpow(exp,ir,tc,ich);/* send power to GLD */
	trig=detctr(exp,ir,tc,&ch[ich]);	/* decide if triggered */
	if(ch[ich].triggered != 2) {				
		if(trig !=0) for (i=0; i<3 ; i++) if(ch[ich].forward[i] >= 0) 
				ch[ch[ich].forward[i]].trigflag=3;
/*
		For triggered channels (this entire ELSE ).  We calculate triggered
		to be zero if any of the triggering channels has shutdown. DCK feb 98
*/
		if(trig !=0) {					/* New Trigger */
			triggered=1;				/* assume all are triggered */
			for (j=0; j<3; j++) {
				if(ch[ich].forward[j] >= 0)	{/* for valid forwards */
					if(ch[ch[ich].forward[j]].triggered == 0) triggered=0;
				}
			}
			if( triggered == 0) {	/* new trigger set up variables */
				if(lp != logout) fprintf(lp,"New Trg fnd %s\n",ch[ich].txt);
/*
				Set up channel parameters for new trigger. Calc time difference
				between ring buffers so all triggers start at same time.
*/
/*				fprintf(logout,"%s New trg fnd %d\n",ch[ich].txt,
						ch[ch[ich].forward[0]].trigdown);*/
				if(phmode) detect_seq=ch[ich].detect_seq;
				for (j=0; j<3; j++) if(ch[ich].forward[j] >= 0 && 
					ch[ich].tc.ms != -1) {	/* must have a time! */
					i=ch[ich].forward[j];/* shorthand for channel to do */
					iss=nsnsub(ch[ich].tc,ch[i].tc)*ch[i].nsamp_qd/1000;
					newtrig(&ch[i],ch[ich].fftpnt+iss,detect_seq,
						leadtime,posttime);
					ch[ich].lastisn=trig;
					ch[i].triggered=1;	/* Mark  new */
				}
				if(phmode) ch[ich].detect_seq++;
				else detect_seq++;			/* increment sequence*/
			} else {
				if(ch[ich].continuous == 0) {/* if NOT a continuous chan*/
					nsnint(tc,&iy,&id,&ih,&im,&is,&ms,&leap);
#ifdef DEBUG_PRINT
					fprintf(logout,"%s Extend BB trig %d:%2d:%2d.%3d...\n",
						ch[ich].txt,ih,im,is,ms);
#endif
					for (j=0; j<3; j++) if(ch[ich].forward[j] >= 0) {
						i=ch[ich].forward[j];/* shorthand for forward chan */
						ch[i].trigdown=posttime*ch[i].nsamp_qd;	/*extend trig */
					}
					if(lp != logout) fprintf(lp,"Extend BB trig ...\n");
				}
			}
						
		}								/* end of  if */
	}									/* end of SMTRIGGER already on */
	if(ch[ich].continuous) ch[ich].triggered=0;	/* continuous stations do not trigger!*/
	return;
}



/*********************** DETCTR *********************************
*
*	calling Sequence :	detctr(exp,ir,tc,ch)
*	argument 		description
*	exp				Power of two to apply to IR powers from FFT
*	ir				Array of long words powers from FFT
*	tc				Time code at beginning of FFT series for this POW
*	ch				Pointer to channel descriptor
*
*	Programmed by : D.C. Ketchum from detctr in USNET and its sordid history
*	Created :		Apr 1992
*	Date Modified Description of Mod
*	20-apr-92	  Base Functionality
**********************************************************************/
#ifdef __STDC__
  int detctr(int exp, long ir[8], struct nsntime tc, struct chan_desc *ch)
#else
  int detctr(exp,ir,tc,ch)
  int exp;
  struct nsntime tc; 
  struct chan_desc *ch;
  long ir[8];
#endif
{
	long maxpow[NPOW];
	long curr[NPOW];
	extern int detnoisy;					/* flag cause detailed detection */
	extern char tag[];
	extern FILE *lp;
	int err,iy,id,ih,im,is,ms;
	int ncnt,k,kk,j,ishift,mm,isn,imag;
	long minpow,itmp;
	extern int trigfreq[16];
	extern int nfreq,expbias;
	ncnt=0;
	ishift=exp - ch->expbias;
	isn=0;
	if(ch->warmup >= 10) {
		if(ch->warmup == 10) fprintf(logout,"%s Clr Hist \n",ch->txt);
		for (j=0; j<NHIST;j++) {
			for (k=0; k<NPOW;k++) {
				ch->nold[k][j]=0;
				ch->hist[k][j]=0;
				ch->hist[k][NHIST]=0;
			}
		}
	}
	if(ch->showdet) fprintf(logout,"Exp=%d shft=%d Pow=%8d %8d\n",exp,ishift,ir[0],ir[1]);
	for (kk=0; kk < ch->nfreq; kk++) {
		k=ch->trigfreq[kk];				/* select a channel */
		if(ir[k] < 0) curr[k]=2100000000;
		else {
			imag=0;
			itmp=ir[k];
			while (itmp > 0) {imag++; itmp<<= 1;}/* count how many to overflow*/
			if(ishift < imag) {		/* if shift count small enough shift curr*/
				if(ishift ==0) curr[k]=ir[k];
				else curr[k]=(ishift > 0) ? ir[k] << ishift : ir[k] >> (-ishift);
			} else curr[k]=2100000000;	/* if shift count large, choose big */
		}
		maxpow[k]=1;					/* init maxpow */
 		for (j=0; j<NHIST; j++) if(maxpow[k] < ch->hist[k][j]) 
			maxpow[k]=ch->hist[k][j];
		if(curr[k] == 2100000000 || curr[k] >= maxpow[k]*ch->sntrig && 
			ch->warmup == 0) {			/* if detector on*/
			ncnt++;
			isn= ((curr[k]/maxpow[k]) > isn) ? curr[k]/maxpow[k] : isn;
		}
	}
	if(ch->showdet || (ncnt>= ch->ncoin && detnoisy)) {	/* Detection found */
		if(ch->showdet) {						/* Am I suppose to print it*/
			for (j=0; j<NHIST1;j++) {
				fprintf(logout,"Hist ");
				for (k=0; k< ch->nfreq; k++) {	/* list the histories*/
					kk=ch->trigfreq[k];
					fprintf(logout,"%10d",ch->hist[kk][j]);
				}
				fprintf(logout,"  Nold ");			/* list the old counts */
				if(j !=NHIST) for (k=0; k< ch->nfreq; k++) {
					kk=ch->trigfreq[k];
					fprintf(logout,"%4d",ch->nold[kk][j]);
				}
				fprintf(logout,"\n");
			}
		}
		if(lp != logout) fprintf(lp,"%s Max  ",tag);/* list the maxpow found*/
		fprintf(logout,"%s Max  ",ch->txt);			/* list the maxpow found*/
		for (k=0; k< ch->nfreq; k++) {
			kk=ch->trigfreq[k];
			if(lp != logout) fprintf(lp,"%10d",maxpow[kk]);
			fprintf(logout,"%10d",maxpow[kk]);
		}
		if(lp != logout) {
			fprintf(lp,"\n");		
			fprintf(lp,"%s Currb ",ch->txt);		/* list current data */
		}
		fprintf(logout,"\n");		
		fprintf(logout,"%s Currb ",tag);				/* list current data */	
		for (kk=0; kk< ch->nfreq; kk++) {
			k=ch->trigfreq[kk];
			if(lp != logout) {
				fprintf(lp,"%9d",curr[k]);
				fprintf(lp,"%s",(curr[k] > maxpow[k]*ch->sntrig) ? "*" : " ");
			}
			fprintf(logout,"%9d",curr[k]);
			fprintf(logout,"%s",(curr[k] > maxpow[k]*ch->sntrig) ? "*" : " ");
		}
		if(lp != logout) fprintf(lp,"\n%s Ncnt=%d  ISN=%d\n",tag,ncnt,isn);
		fprintf(logout,"\n");
	}
	if(ch->warmup == 0) {
		if(ncnt >= ch->ncoin && ch->ncoin > 0) {
			err=nsnint(tc,&iy,&id,&ih,&im,&is,&ms);
			if(ch->lastisn < isn) {
				fprintf(logout,
				"%s DETCTR: Trigger on %4d %3d-%2d:%2d:%2d.%3d Ncnt=%d ISN=%d ...\n",
				ch->txt,iy,id,ih,im,is,ms,ncnt,isn);
				ch->lastisn=isn;
			}
			if(lp != logout) 
				fprintf(lp,"%s Trigger on %4d %3d-%2d:%2d:%2d.%3d Ncnt=%d ISN=%d ...\n",
				ch->txt,iy,id,ih,im,is,ms,ncnt,isn);
			if(ch->trigdead > ch->trigtotal) return isn;
			fprintf(logout,"TRIGDEAD occuring dead=%d tot=%d...\n",
				ch->trigdead,ch->trigtotal);
		}
	}

	else {
		ch->warmup--;
		if(ch->warmup < 20) fprintf(logout,"%s Warmup=%d\n",ch->txt,ch->warmup);
	}
/*
	Trigger is not on. Update Histif the channels are no longer triggered
	 do not update if triggers going on.
*/
	if(ch->triggered !=0 && ch->trigdead > ch->trigtotal &&
		ch->warmup <=0) return 0;
/*
		Check on power history retiring do to being too old.
*/
	for (kk=0; kk< ch->nfreq; kk++) {
		k=ch->trigfreq[kk];
		for (j=0; j<NHIST; j++) {
			--ch->nold[k][j];
			if(ch->nold[k][j] <=0) {
				ch->hist[k][j]=ch->hist[k][NHIST];	/* expired replace it */
				ch->nold[k][j]=ch->nretired;	
			}
		}
	}
/*
		Look for minimum POW in each frequency and replace it with
		the POW from last time (not this time) if it is smaller.  Set 
		last time power to this time for use on next call
*/
	for (kk=0; kk< ch->nfreq; kk++) {
		k=ch->trigfreq[kk];
		minpow=2140000000;					/* set obsured maximum */
 		for (j=0; j<NHIST; j++) {
			if(ch->hist[k][j] < minpow) {
				minpow=ch->hist[k][j];
				mm=j;
			}
		}
		if(ch->hist[k][mm] < ch->hist[k][NHIST]) {
			ch->hist[k][mm]=ch->hist[k][NHIST];
			ch->nold[k][mm]=ch->nretired;
		}
		ch->hist[k][NHIST]=curr[k];			/* set NHIST1 to current */
	}
	return 0;
}	



int init_trig()
/*********************** init_trig *********************************
*
*	calling Sequence :	err=init_trig
*	argument 		description
*	NONE
*
*	Programmed by : D.C. Ketchum
*	Created :	
*	Date Modified Description of Mod
*	2-Oct-92		Initialize MS_DELAY field.
**********************************************************************/
{
/*			Initialize the pointers to the ring buffer channel generalized*/
	extern struct chan_desc *ch;
	extern int expbias;							/* default global bias */
	extern int trigfreq[];					/* default global freq bands in trig*/
	extern unsigned long *cmpbufs;	/* where the compression scratches */
	extern int gbldoy;							/* connect to the doy global */
	extern short detect_seq;				/* connect to the detection sequ*/
	extern int FFTLEN,FFTL2,SPFREQ;	/* Attach to FFT length Globals */
	extern int continuous;					/* Attach to continuous BB flag */
	extern int continuous_phoenix;	/* Attach to continuous BB PHMODE */
	extern int derive_lp;						/* Attach to derive LP flag */
	extern int lp_delay,bb_delay;		/* delay time for channels */
	extern char tag[];
	extern int short_period;				/* link to variable for short period */
	extern int decim_factor;				/* decimate factor (if any) */
	extern int phmode;							/* if non-zero, multichan mode */
	extern int MAX_CH;							/* number of channels in this VDL*/
	extern int net;									/* connect to net id */
	extern int node;								/* connect to node id */
	extern int digit_freq;					/* digitizer native frequency */
	extern char param_file[];				/* the file name of parameters */
	extern int showdet;							/* global show det */
	extern double freq_calc;				/* nominally given freq of digit */
	extern int triggered_input;			/* connect to trigger input flag */
	extern int heligain;						/* if set, heli possible */
	time_t time();									/* declare time function for GCC*/
	time_t clock;										/* place to stick the time unix*/
	struct tm *tm2;									/* pointer to parsed time for unix */
	int i,j,k;
	fprintf(logout,"init_trig phmode=%d cnt=%d cntph=%d dec=%d MAX_CH=%d\n",
	phmode,continuous,continuous_phoenix,decim_factor,MAX_CH); fflush(logout);

/*	If not a "phoenix mode", ie. most parameters on command line */

	time(&clock);										/* local time to set init detect_seq */
	tm2=gmtime(&clock);							/* broken down but in GMT */
	detect_seq=(tm2->tm_hour*3600+tm2->tm_min*60+tm2->tm_sec)/3+1;/*init it*/
	if(phmode == 0) {
		for(i=0; i<=MAX_CH; i++) 
		{	ch[i].stat_chan=i;
			ch[i].continuous=continuous;	/* set continuous flag */
			ch[i].decimate=decim_factor;
			if(ch[i].decimate > 1) 
			{	ch[i].decim_filt = setfilt(digit_freq, SPFREQ, 0);
				ch[i].delay=ch[i].decim_filt.delay;
				ch[i].sphist=(long *) malloc(ch[i].decimate * sizeof(long));
			}
			for(j=0; j<3; j++) ch[i].forward[j]=-1;
			ch[i].expbias=expbias;
			ch[i].ncoin=1;
			ch[i].nretired=50;
			ch[i].sntrig=3.5;
			ch[i].hfsntrig=10000.;
			ch[i].nfreq=4;
			ch[i].detect_seq=detect_seq;
			ch[i].sendpows=0;			/* send no powers */
			ch[i].ipnt=0;	/* stagger ring buffer offset */
			for(k=0; k<10; k++) ch[i].trigfreq[k]=trigfreq[k];
			if(i<3) 
			{	if(derive_lp) 
				{	ch[i].derive_lp=i+3;
					ch[ch[i].derive_lp].max=60;
					ch[i].lpfilt=setfilt(SPFREQ,1,0);/* set up filter */
					ch[ch[i].derive_lp].delay=ch[i].lpfilt.delay + ch[i].delay;
				}
				ch[i].freq=SPFREQ;
				if(freq_calc == 0.) ch[i].freq_calc=ch[i].freq;
				else ch[i].freq_calc=freq_calc;
			} else 
			{	ch[i].freq=1;
				ch[i].freq_calc=1.;
				if(derive_lp) ch[i].decimate=SPFREQ;
				else ch[i].decimate=1;
			}
		}
		if(triggered_input)
		{	for(i=0; i<3; i++) 
			{	ch[i].trig_chan=-1;
				ch[i].ncoin=-1;
				ch[1].forward[i]=i;
				ch[2].forward[i]=i;
				ch[0].forward[i]=i;
			}
		}
		else
		{	ch[2].trig_chan=1;				/* set trigger channel */
			ch[2].fftpnt=FFTLEN;
			ch[2].showdet=showdet;
			ch[2].sendpows=1;				/* send only regular VDL powers */
			ch[2].warmup=10;				/* set warmup */
			for(i=0; i<3; i++) ch[2].forward[i]=i;/* set 3 HF to go together */
			if(heligain)
			{	ch[2].helifilt=setfilt(SPFREQ,10,1);	/* set up filter */
				ch[2].helipnt=0;				/* pointer to ring buf */
				ch[2].heli=99;					/* set heli channel # */
				ch[2].helibuf=(short *) malloc(150*sizeof(short));
				ch[2].heliseq=1;				/* set heli sequence */
				ch[2].heligain=heligain;		/* set attenuation */
			}
		}
		strcpy(ch[CH_X20].txt,tag);
		strcpy(ch[CH_Y20].txt,tag);
		strcpy(ch[CH_Z20].txt,tag);
		strcpy(ch[CH_X1].txt,tag);
		strcpy(ch[CH_Y1].txt,tag);
		strcpy(ch[CH_Z1].txt,tag);		/* initialize channel names */
		strcat(ch[CH_X20].txt,"X20");
		strcat(ch[CH_Y20].txt,"Y20");
		strcat(ch[CH_Z20].txt,"Z20");
		strcat(ch[CH_X1].txt,"X1");
		strcat(ch[CH_Y1].txt,"Y1");
		strcat(ch[CH_Z1].txt,"Z1");

/* if "Phoenix mode" I.E. most parameters read in */

	} else 								/* phoenix mode */
	{	for(i=0; i<=MAX_CH; i++) 
		{	ch[i].ipnt=0;				/* start ring buffer at offset zero */
			fprintf(logout,"%d fr=%d dc=%d dlp=%d dlp[i]=%d %7.3f\n",
				i,ch[i].freq,ch[i].decimate,derive_lp,
				ch[i].derive_lp,ch[i].freq_calc);
			ch[i].detect_seq=detect_seq;
			if(ch[i].freq > 10) 	/* set up trigger stuff for each */
			{	ch[i].trig_chan=ch[i].ncoin;
				ch[i].fftpnt=FFTLEN+((32*i) % FFTLEN);
				ch[i].lastpartial=0;		/* init lastpartial */
				ch[i].warmup=10;
				ch[i].triggered=0;
				ch[i].showdet=showdet;
				if(continuous_phoenix) ch[i].continuous = 1;
				if(ch[i].forward[0] != i) 
				{	fprintf(logout,
					"WARNING : forward[0] on ch[%d] is not %d. Overriding.\n",
					i,i);
					ch[i].forward[0]=i;
				}
				if(ch[i].decimate <= 0) ch[i].decimate=decim_factor;
				if(ch[i].decimate <=0) fprintf(logout,"Decim <= 0 is %d\n",
					ch[i].decimate);
				if(ch[i].decimate > 1) 
				{	ch[i].decim_filt = setfilt(ch[i].freq*ch[i].decimate, ch[i].freq, 0);
					ch[i].delay=ch[i].decim_filt.delay;	/* set decim delay */
					ch[i].sphist=(long *) malloc(ch[i].decimate * sizeof(long));
				}
				if(derive_lp || ch[i].derive_lp) 	/* if derived LP called*/
				{	if(derive_lp) ch[i].derive_lp=i+3;
					ch[ch[i].derive_lp].max=60;
					ch[i].lpfilt=setfilt(ch[i].freq,1,0);/* get filter structure */
					ch[ch[i].derive_lp].delay=ch[i].lpfilt.delay + ch[i].delay;
 				}
				if(ch[i].heli > 0)					/* heli set up on this? */
				{	ch[i].helifilt=setfilt(ch[i].freq,10,1);/* set up filter */
					ch[i].helipnt=0;				/* pointer to ring buf */
					ch[i].helibuf=(short *) malloc(150*sizeof(short));
					ch[i].heliseq=1;				/* set heli sequence */
					if(ch[i].heligain <= 0) ch[i].heligain=100;
				}
			}
		}
	}
	cmpbufs=(unsigned long *)malloc((MAX_CH+1)*512*4);/* assign compress buf*/
	memset(cmpbufs,0,(MAX_CH+1)*512*4);
	fprintf(logout,"Cmpbufs=%x len=%d\n",cmpbufs,(MAX_CH+1)*512*4);
/*
	set other parameters.
*/
	for (i=0; i<=MAX_CH; i++) {
		ch[i].decim_off = 0;
		ch[i].trigdown=-32000000;		/* Start with no  counter */
		ch[i].trigflag=0;				/* not the trigger channel */
		ch[i].trigdead=100000000;		/* long dead time counter to start */
		ch[i].trigabshutdown=0;			/* just to be sure */
		ch[i].cmp=(void *) (cmpbufs+512*i);/* location for compression buffer */
		cmset(&ch[i].memex,ch[i].cmp);	/* Initialize Compression */
		ch[i].trigtotal=0;				/* Start with no trig on chan counter */
		ch[i].seq=0;					/* New trigger set sequence */
		ch[i].cmptotal=1;				/* No data, compressed 1 for extra point
										   in first compression buffer */
		ch[i].tc.ms=-1;					/* indicate no time yet */
		ch[i].lastpartial=0;			/* Set last time partial done */
		ch[i].lppnt=0;					/* where to start next LP conversion */
		ch[i].triggered=0;				/* starting up you cannot be triggered*/
		if(ch[i].freq > 10) {		/* is this 20-50 hz channel */
			ch[i].nsamp_qd=ch[i].freq;		/* set Hz data */
			ch[i].lpfilt=setfilt(ch[i].freq,1,0);/* set filter for 1 sps */

			if(ch[i].freq == 100) {
				if(phmode) ch[i].stat_chan=i;	/* NSN standard for 100 Hz */
				ch[i].max=12800;		/* ring buffer size */
				if(short_period) ch[i].stat_chan+=SH_CHAN;/* if not BB data*/

				/* time delay of data stream in MS */
				if(bb_delay > 0 && ch[i].delay == 0) ch[i].delay=bb_delay;
			}
			if(ch[i].freq == 80) {
				if(phmode == 0)ch[i].stat_chan=i;/* NSN standard chan for 80 HZ */
				if(short_period) ch[i].stat_chan+=SH_CHAN;	/* short period */
				ch[i].max=7680;			/* buffer size for ring */

				/* time delay of data stream in MS */
				if(bb_delay > 0 && ch[i].delay == 0) ch[i].delay=bb_delay;
			}				
			if(ch[i].freq == 50) {
				if(phmode == 0) ch[i].stat_chan=i+3;	/* NSN standard for 50 Hz */
				ch[i].max=6400;			/* ring buffer size */
				if(short_period) ch[i].stat_chan+=SH_CHAN;/* if its not BB data*/

				if(ch[i].continuous && phmode == 0) ch[i].stat_chan+=12;	/* continuous data */
				/* time delay of data stream in MS */
				if(bb_delay > 0 && ch[i].delay == 0) ch[i].delay=bb_delay;
			}
			if(ch[i].freq == 20) {
				if(phmode == 0)ch[i].stat_chan=i+6;/* NSN standard chan for 20 HZ */
				if(ch[i].continuous && phmode == 0) ch[i].stat_chan+=12;/* continuous data */
				if(short_period) ch[i].stat_chan+=SH_CHAN;	/* short period */
				ch[i].max=1920;			/* size of ring buffer */

				/* time delay of data stream in MS */
				if(bb_delay > 0 && ch[i].delay == 0) ch[i].delay=bb_delay;
			}	
			if(ch[i].freq == 40) {
				if(phmode == 0)ch[i].stat_chan=i+3;/* NSN standard chan for 40 HZ */
				if(short_period) ch[i].stat_chan+=SH_CHAN;	/* short period */
				if(ch[i].continuous && phmode == 0) ch[i].stat_chan+=12;	/* continuous data */
				ch[i].max=7680;			/* size of ring buffer 64 secs*/

				/* time delay of data stream in MS */
				if(bb_delay > 0 && ch[i].delay == 0) ch[i].delay=bb_delay;
			}
			ch[i].ring=(long *) malloc(ch[i].max*sizeof(long));/* ring buf */
			memset(ch[i].ring,0,ch[i].max*sizeof(long));/* zero the rings */
			fprintf(logout,"%d %s %s BB Delay=%d max=%d stch=%d\n",
					i,ch[i].txt,tag,ch[i].delay,ch[i].max,ch[i].stat_chan);
			ch[i].partial=0;
		} else {							/* This is an LP Channel */
			ch[i].nsamp_qd=1;				/* 1 Hz data */
/*			if(freq_calc == 0.) ch[i].freq_calc=ch[i].nsamp_qd;
			else ch[i].freq_calc=freq_calc;*/
			ch[i].continuous=1;				/* continuous data */
			if( !phmode) ch[i].stat_chan=i+9;/* NSN standard chan for 1 HZ */
			ch[i].partial=1;				/* partial updates enabled */
			ch[i].max=MAX_NSAMP_IN*1.2;		/* LP ring buffer size */
			if(ch[i].delay == 0) 
				ch[i].delay=lp_delay;		/* Delay of LP in MS per user */
			fprintf(logout,"%s %s LP delay=%d max=%d\n",
				ch[i].txt,tag,ch[i].delay,ch[i].max);
			ch[i].ring=(long *) malloc(ch[i].max*sizeof(long));
		}
		if(ch[i].continuous !=0) {
			ch[i].doy=gbldoy % 256;			/* Day of year detection started */
			ch[i].detseq[0]=detect_seq %256;/* low order of detect seq*/
			ch[i].detseq[1]=detect_seq/256;	/* High order of detect seq*/
		}
	}
	ch[2].trigflag=0;				/* this is the trigger channel */
	detect_seq++;					/* increment sequence*/
	if(FFTLEN == 512) ifftst512();	/* initialize FFT routine */
	if(FFTLEN == 256) ifftst256();	/* initialize FFT routine */
	if(FFTLEN == 1024) ifftst1024();	/* initialize FFT routine */
	print_status();					/* dump status information */
}


print_status()
{
	extern struct chan_desc *ch;
	extern char tag[];
	extern int short_period;			/* link to variable for short period */

	extern int expbias;					/* default global bias */
	extern int triggered_input;		/* connect to trigger input flag */
	extern int gbldoy;					/* connect to the doy global */
	extern int heligain;
	extern int lp_delay,bb_delay;		/* delay time for channels */
	extern int phmode;							/* if non-zero, multichan mode */
	extern int MAX_CH;							/* number of channels in this VDL*/
	extern int net;									/* connect to net id */
	extern int node;								/* connect to node id */
	extern int digit_freq;					/* digitizer native frequency */
	extern char param_file[];				/* the file name of parameters */
	extern int FFTLEN,FFTL2,SPFREQ;	/* Attach to FFT length Globals */
	extern int continuous;					/* Attach to continuous BB flag */
	extern int continuous_phoenix;	/* attach to flag for continuous PHs*/
	extern int derive_lp;						/* Attach to derive LP flag */
	extern int decim_factor;				/* decimate factor (if any) */
	extern double freq_calc;				/* nominally given freq of digit */
	int i;
	fprintf(logout,
		"decim_factor=%d freq_calc=%7.3f continuous=%d cnt_ph=%d derive_lp=%d expb=%d\n",
		decim_factor, freq_calc, continuous,continuous_phoenix,
		derive_lp,expbias);
	fprintf(logout,
		"heligain=%d triggered_input=%d lpdelay=%d bbdelay=%d gbldoy=%d\n",
		heligain,triggered_input, lp_delay, bb_delay, gbldoy);
	fprintf(logout,"MAX_CH=%d net=%d node=%d phmd=%d SPFREQ=%d dig_fq=%d FFTL=%d  phfile=%s\n",MAX_CH,net,node,phmode,SPFREQ,digit_freq,FFTLEN,param_file);
	fprintf(logout,
	"ch statn stch dc shw pw nsq  fq fcalc cn pr tg forwards dtseq    dlp   dly nc ex s/n nt trigfreq    Warm LPFILT\n");
	for(i=0; i<=MAX_CH; i++) {		/* for each station, print out chan stuff*/

		fprintf(logout,"%2d %6s %3d %2d %2d %2d %3d %3d %6.3f %2d %2d %2d %2d %2d %2d %2d%6d%6d %2d %2d %4.1f %1d %2d %2d %2d %2d %2d %3d%8d\n", 
		i,ch[i].txt,ch[i].stat_chan,ch[i].decimate,ch[i].showdet,
		ch[i].sendpows,ch[i].nsamp_qd,ch[i].freq,ch[i].freq_calc,
		ch[i].continuous,ch[i].partial,ch[i].trig_chan,ch[i].forward[0],
		ch[i].forward[1],ch[i].forward[2],ch[i].detect_seq,
		ch[i].derive_lp,ch[i].delay,
		ch[i].ncoin,ch[i].expbias,ch[i].sntrig,ch[i].nfreq,
		ch[i].trigfreq[0],ch[i].trigfreq[1],ch[i].trigfreq[2],
		ch[i].trigfreq[3],ch[i].trigfreq[4],ch[i].warmup,ch[i].lpfilt);
	}
	fprintf(logout,
	"ch chn    seq det1det2 cmptot cmpst cmppt lprtl   ring   max  ipnt lpnt warm fcalc cmpbuf heli gain\n");
	for(i=0; i<=MAX_CH; i++) fprintf(logout,
"%2d %6s%4d%4d%4d %6d %5d %5d %5d %8x %4d %4d %4d %3d %7.3f %8x %4d %4d\n",
	i,ch[i].txt,
	ch[i].seq,ch[i].detseq[0],ch[i].detseq[1],ch[i].cmptotal,ch[i].cmpstart,
	ch[i].cmppnt,ch[i].lastpartial,ch[i].ring,ch[i].max,ch[i].ipnt,
	ch[i].lppnt,ch[i].warmup,ch[i].freq_calc,ch[i].cmp,
	ch[i].heli,ch[i].heligain);
}



/*********************** NEWTRIG *********************************
*
*	calling Sequence :	newtrig(ch,ipnt,detect_seq,secbefore,secafter)
*	argument 		description
*	ch				Pointer to a channel structure
*	detect_seq		Detection sequence to assign to this detection
*	ipnt			Offset in Ring buffer to compute pre event memory from 
*	secbefore		Number of seconds before current data to save (pre-event)
*	secafter		Number of secnods of data to save after this point
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
*	Date Modified Description of Mod
*	20-apr-92	  Base Functionality
*	12-Jul-93		Maintain ev data structure (part of event based mem mgt).
**********************************************************************/
#ifdef __STDC__
  void newtrig(struct chan_desc *ch, int ipnt, int detect_seq,int secbefore, int secafter)
#else
  void newtrig(ch,ipnt,detect_seq,secbefore,secafter)
  struct chan_desc *ch; 
  int detect_seq,ipnt,secbefore,secafter;
#endif
/*		
	Since the first compression frame has one more sample than the number of
	differences returned by the compression frame, cmptotal is started at one.
	In this way the first buffers time offset will be the number of points 
	returned by the compression buffer+1 for the first buffer
*/
{
	extern struct eventdef *event;	/* pointer to array of event structures*/
	extern struct eventdef *ev,*hf_ev;/* pointer to current event and HF event */
	struct eventdef *evt;			/* temp pointer to event structure */
	extern int continuous20,continuous40;	/* flags for continuous SP */
	extern int continuous;			/* continuous SP flag */
	extern int continuous_phoenix;	/* continuous SP in phoenix mode */
	extern int gbldoy;				/* Cnnect to global doy */
	extern int phmode;				/* change reporting if PHMODE set */
	extern char tag[];
	extern FILE *lp;				/* connect to log path */
	struct nsntime tc;
	int i,iy,id,ih,im,iss,ms,leap,ns;/* temp variables */
	extern int leadtime,abstime,deadtime;/* Times of various detection parms*/
	extern int maxevent;			/* number of event structures */
	extern long contrgsecs;			/* seconds of last trigger packet */
	time_t time();
	time_t clock;
	struct tm *tm2;
	int is,nbefore;
	if(ipnt < 0) ipnt+=ch->max;		/* if negative ipnt, correct it */
	if(continuous || continuous_phoenix) {
		ch->trigdown=-32000000;		/* mark not trigger */
		time(&clock);				/* get Unix time */
		tm2=gmtime(&clock);			/* convert to be more conventional*/
		nbefore=tm2->tm_hour*3600+tm2->tm_min*60+tm2->tm_sec;
		if(abs(nbefore-contrgsecs) > 20) {
			tc=buftim(ch,ipnt);		/* get time of trigger */
			nsnint(tc,&iy,&id,&ih,&im,&iss,&ms,&leap);
			if(iy <= 1970) return;	/* no data for channel */
			fprintf(logout,"%s Contrg det=%d %d %d-%2d:%2d:%2d.%3d\n",
				ch->txt,detect_seq,iy,id,ih,im,iss,ms);
			contrg(detect_seq,tc,ch);	/* force trigger packet out */
			contrgsecs=nbefore;		/* set last time triggered */
		}
		return;
	}

	if(ch->trigdown > -30000000) {	/* is this channel still triggering?*/
		compressit(ch);				/* add any data we can to the trigger*/
		fprintf(logout,"%s NEWTRIG : Force shtdwn chn %d trgdwn=%d\n",
			ch->txt,ch->stat_chan,ch->trigdown);
		if(lp != logout) 
			fprintf(lp,"%s NEWTRIG : Force shtdwn chn %d \n",tag,ch->stat_chan);
		if(ch->trigdown > -30000000) cmfin(&ch->memex,ch->cmp);	/* force end of detect*/
		ch->trigdown=-32000000;		/* mark no longer triggered */
	}
	cmset(&ch->memex,ch->cmp);		/* Initialize Compression */
	nbefore=ch->nsamp_qd*secbefore;	/* how many samples before current to start*/
	if(ipnt < nbefore) is=ipnt+ch->max-nbefore;
	else is=ipnt-nbefore;			/* set where to start in samps in ring buf*/
	if(is < 0 || is > 20000) fprintf(logout,
			"Bad is=%d ipnt=%d ch->max=%d nbefore=%d nsq=%d secbef=%d\n",
			is,ipnt,ch->max,nbefore,ch->nsamp_qd,secbefore);
	ch->trigdown=(secafter+secbefore)*ch->nsamp_qd;/* # of samps to compress*/
	ch->cmppnt=is;					/* point compressor at start of data */
	ch->trigtotal=1;				/* Start with clear counter */
	ch->trigdead=(leadtime+deadtime)*ch->nsamp_qd; /* Sample to force history */
	ch->trigabshutdown=(leadtime+abstime)*ch->nsamp_qd;/* Longest posible trig*/
	ch->seq=1;						/* New trigger set sequence */
	ch->cmpstart=is;
	ch->cmptotal=1;					/* No data yet compressed - 1 extra 1st buf */
	ch->detseq[0]=detect_seq %256;	/* low order of detect seq*/
	ch->detseq[1]=detect_seq/256;	/* High order of detect seq*/
	ch->cmptc=buftim(ch,is);		/* Set the time of beginning of cmpression*/
	nsnint(ch->cmptc,&iy,&id,&ih,&im,&iss,&ms,&leap);
	ch->doy=id % 256;				/* Day of year detection started */
/*	if( ch->trig_chan > 0) */
		fprintf(logout,"%s NEWTRIG: declare trig det=%d is=%d %d %d-%d:%d:%d.%d\n",
			ch->txt,detect_seq,is,iy,id,ih,im,iss,ms);
	if(lp != logout  && ch->trig_chan > 0 ) 
		fprintf(lp,"%s declare trig is=%d %d %d-%d:%d:%d.%d\n",
			tag,is,iy,id,ih,im,iss,ms);
/*
	If this is the vertical component, assign and update the event record.
*/
	ns=compressit(ch);				/* compress all data in ring buf*/
	return;
}									/* end of new trigger if */




/*********************** LPFILT *********************************
*
*	calling Sequence :	lpfilt(ch)
*	argument 		description
*	ch				Pointer to a channel structure
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
*	Date Modified Description of Mod
***********************************************************************/
#ifdef __STDC__
  long lpfilt(struct chan_desc *ch)
#else
  long lpfilt(ch)
  struct chan_desc *ch;
#endif
/*
	create LP data from data in ring buffer.  Use the decimate system
	to filter and decimate the LP.
	
	The filtering routines FILT2 and FILT10 use the filters designed by 
		Quanterra for use in the Q680.

	D.C. Ketchum Apr 1995
	converted to vdlfilt dec 1997
*/
{
	long output[200];					/* buffer space for timeseries */
	int i,j;						/* counter variables */
	i=ch->nsamp_qd;					/* how many samples per second */
/*	fprintf(logout,"%s LPFILT max=%d i=%d ring=%x lppnt=%d\n",
		ch->txt,ch->max,i,ch->ring+ch->lppnt,ch->lppnt);*/	
	memcpy(output, ch->ring+ch->lppnt, ch->freq*sizeof(long));/* get sample form buffer */
	output[0]=decimate(ch->lpfilt, output);
	ch->lppnt += ch->freq;
	if(ch->lppnt >= ch->max) ch->lppnt -= ch->max;
	return output[0];
}



/*********************** SPFILT *********************************
*
*	calling Sequence :	spfilt(ch, ia, nsamp, tc)
*	argument 		description
*	ch				Pointer to a channel structure
*    ia				Array of Long data in 
*	*nsamp			pointer to # samp in, output # samples returned
*	tc				Time code of ia[0]
*
*	returned TC is time of ia[0] on return
*
*	Programmed by : D.C. Ketchum
*	Created :		Sep 1998
*	Date Modified Description of Mod
***********************************************************************/
#ifdef __STDC__
  struct nsntime spfilt(struct chan_desc *ch, long ia[], int *nsamp,  struct nsntime tc)
#else
  struct nsntime spfilt(ch, ia, nsamp, tc)
  struct chan_desc *ch;
   long ia[];
	int *nsamp;
	struct nsntime tc;
#endif
/*
	Decimate the data for channel ch.  New time series section is in ia
	and is nsamp long.  The tc passed in is of ia[0].  The timecode received
	is for the ia[0] after filtering and decimation
	
	The filtering routines FILT2 and FILT10 use the filters designed by 
		Quanterra for use in the Q680.

	D.C. Ketchum Apr 1995
	converted to vdlfilt dec 1997
*/
{
	int iy,id,ih,im,is,ms,leap;		/* Debug time vars */
	extern int SPFREQ;
	long output[20];				/* buffer space for timeseries */
	int i,j;						/* counter variables */
	i = 0;							/* next sample from ia to process */
	j = 0;							/* set next place in ia to put a decimated*/


/*	nsnint(tc,&iy, &id, &ih, &im, &is, &ms, &leap);
	fprintf(logout,"off=%d dc=%d fq=%3d ns=%4d %4d %3d %2d:%2d:%2d.%3d ",
		 ch->decim_off,ch->decimate, ch->freq, *nsamp, iy, id, ih, im, is, ms);	
	fflush(logout);*/

	/* compute time of first sample */
	tc = nsnadd(tc, (ch->decimate - ch->decim_off) * 1000 /
			(ch->freq * ch->decimate)); 

	/* As long as there are at least ch->decimate samples left in buffer */
	while ( i <= (*nsamp - ch->decimate) ) 
	{	memcpy(&ch->sphist[ch->decim_off], &ia[i], 	/* move to temp buf*/
		(ch->decimate - ch->decim_off) * sizeof(long));
		i += (ch->decimate - ch->decim_off);	/* point to next sample in */
		ch->decim_off = 0;						/* is always zero after 1st */
		ia[j++] = decimate(ch->decim_filt, ch->sphist);/* compute sample */
	}
	/* set left over data pointer and left over data */
	ch->decim_off = *nsamp - i;		/* is there data left over???? */
	if(ch->decim_off > 0) memcpy(ch->sphist, &ia[i], 
		ch->decim_off*sizeof(long));/* yes, copy it to leftover array */

	/* return number of samples returned */
	*nsamp = j;					

	/* debug output */
/*	nsnint(tc,&iy, &id, &ih, &im, &is, &ms, &leap);
	fprintf(logout,"SP %5s ns=%4d j=%4d off=%d tcout=%d %d %2d:%2d:%2d.%3d\n",
		ch->txt,*nsamp, j, ch->decim_off, iy, id, ih, im, is, ms);*/

	return tc;
}



int procheli(ch)
/*		Procheli examines the channel to see if enough data is present to
		compute a 15 second section of heli bound data.  If so, it processes it
		and outputs the section.

	D.C. Ketchum Dec 1997
*/
struct chan_desc *ch;
{	int pnt,i,ierr;
	int ns;
	long data;
	extern int ttpath;
	unsigned char *i1,*i2;				/* pointers for byte manipulations*/
	extern int net;
	extern int node;
	struct gomberg gb;					/* data packet to send */
	int iy,id,ih,im,is,ms,leap;			/* for debug time decode */
	if(ch->heli == 0) return 0;
	ns=ch->ipnt-ch->helipnt;
	if(ns < 0) ns += ch->max;
	if(ns < 15*ch->freq) return 0;		/* not enough data to process */
	gb.tc=buftim(ch,ch->helipnt);
/*	nsnint(gb.tc,&iy,&id,&ih,&im,&is,&ms,&leap);
	fprintf(logout,"%s Heli out %4d %3d %2d:%2d:%2d.%3d hp=%d ipnt=%d \n",
		ch->txt,iy,id,ih,im,is,ms,ch->helipnt,ch->ipnt); */
	gb.lead1=27; gb.lead2=3;
	gb.routeid=net; gb.nodeid=node;
	gb.chanid=ch->heli;				/* set station channel for this */
	gb.format=10;
	gb.doy=ch->doy;
	gb.flags=0;
	gb.seq=ch->heliseq;
	ch->heliseq++;
	gb.detseq[0]=1000%256;
	gb.detseq[1]=1000/256;
	gb.numbyt[0]=(150/2*3+14+6) % 256;
	gb.numbyt[1]=(150/2*3+14+6)/ 256;
	for (i=0; i<150; i++)
	{	data=decimate(ch->helifilt,ch->ring+ch->helipnt);/* process a point */
		data = data*100/ch->heligain;			/* apply attenuation */
/*		fprintf(logout," %8d %8d",data,*(ch->ring+ch->helipnt));
		if( (i%5) == 4) fprintf(logout,"\n");*/
		if(data > 2047) data=2047;		/* limit to 12 bits */
		if(data < -2048) data = -2048;
		ch->helibuf[i]=data;				/* store the point */
		ch->helipnt += ch->helifilt.decimation; /* pnt to next in time series */
		if(ch->helipnt >= ch->max) ch->helipnt -= ch->max;
	}
	pnt=0;								/* init index to gb.buf */
	for(i=0; i<150; i += 2)				/* every other sample */
	{	i1= (unsigned char *) &(ch->helibuf[i]);/* point to first sample */
		i2= (unsigned char *) &(ch->helibuf[i+1]);/* point to second sample */
		gb.buf[pnt]=*(i1+3); pnt++;		/* get low order of 1st sample*/
		gb.buf[pnt]=*(i2+3); pnt++;		/* get low order of 2nd sample */
		gb.buf[pnt]= (*(i1+2) & 0xf) | ((*(i2+2) & 0xf) << 4);
		pnt++;
	}
	ierr=writeoutgb(&gb, (150/2*3+14+6));
	if(ierr < 0) 
	{	fprintf(logout,"%s Write heli err=%d errno=%x\n",
				ch->txt,ierr,errno);
	}
 	return 1;
}
