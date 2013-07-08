#include <stdio.h>
#include <errno.h>
#include <math.h>
#include "vdl.h"
extern FILE *logout;
struct nsntime makedata();
double ran();
feedme_init(argc,argv)
int argc;
char **argv;
{
	extern int tstdata;
	fprintf(logout,"VDL init\n");
	tstdata=1;
	return 0;
}
feedme_shutdown()
{
	fprintf(logout,"VDL shutdown!\n");
	return 0;
}
feedme(ich,ia,tc)
int *ich;
long ia[];
struct nsntime *tc;
{
	fprintf(logout,"VDL feedme in feedme_test???\n");
	return (feedme_test(ich,ia,tc));
}
int feedme_test(ich,ia,tc)
int *ich;
long ia[];
struct nsntime *tc;
{
	static int ic=7;
	static long sp[3][MAX_NSAMP_IN],lp[3][MAX_NSAMP_IN/10];
	static struct nsntime tcsp,tclp;
	static int nsp,nlp;
	int i;
	int yr,id,ih,im,is,ms,leap;
	if(ic == 2) sleep(1);					
	if(ic >5) {
		tcsp=makedata(&sp[0][0],&nsp,&lp[0][0],&nlp,&tclp);
		nsnint(tcsp,&yr,&id,&ih,&im,&is,&ms,&leap);
		fprintf(logout,"%4d %3d:%2d:%2d:%2d.%3d Make data nsp=%d nlp=%d\n",
			yr,id,ih,im,is,ms,nsp,nlp);
		ic=0;
	}
	*ich=ic;
	if(ic <3) {
		for (i=0; i<nsp; i++) ia[i]=sp[ic][i];
		*tc=tcsp;
		ic++;
		return nsp;
	} else {
		for(i=0; i<nlp; i++) ia[i]=lp[ic-3][i];
		*tc=tclp;
		ic++;
		return nlp;
	}
}
struct nsntime makedata(ia,nsamp,lp,nlp,tclp)
long ia[3][256],lp[3][20];
int *nsamp,*nlp;
struct nsntime *tclp;
{
/***********************************************************
	Create  as sin wave time series to test program.  256 points
	of data in 3 channels is returned in IA, an appropriate number of
	LP channels is returned in lp.  The value returned is the time of
	the first sample in IA.
*/
	struct nsntime tc;				/* a time code to play with*/
	extern int trigforce;			/* set by console command to force trigger*/
	static long ifirst=1;			/* first time


 flag to perform setup stuff*/
	static double dt=0.,dtl=0.;		/* time of samples */
	static struct nsntime tcint;
	static struct nsntime tcl;
	extern int ranflg;				/* use random data flag */
	extern int SPFREQ;
	int i,iy,id,ih,im,is,ms,leap;	/* various scratch variables */
	*nsamp=1000;
/*	*nsamp=ran(0)*128.+128.;			/* how many samples to create this time */
	if(*nsamp < 1) *nsamp=1;
	if(ifirst) {					/* if first time */
		tcint.iyr=24*2;				/* year is 1994 2*(1994-1970) */
		tcint.doy=52;				/* Day 30 - fixed day for our test */
		tcint.ms=84000000*16;		/* time near the end of day */
		ifirst=0;					/* we do not need to do this again */
		nsnint(tcint,&iy,&id,&ih,&im,&is,&ms,&leap);	/* convert time for kicks*/
		fprintf(logout,"Start %4d %3d %2d:%2d:%2d.%3d\n",iy,id,ih,im,is,ms);
		tcl=tcint;
		for(i=0;i< 20; i++) *nsamp=ran(0)*256.;
	}
	tc=tcint;						/* set the time*/
	*tclp=tcl;						/* set start time of LP buffer */
	*nlp=0;
	for (i=0; i<*nsamp; i++) {		/* always return nsamp points */
		if(trigforce ) {			/* if triggered make it big */
			if(ranflg == 0) ia[0][i]=10000.*sin(dt*2.*3.1415926/20.)+1000.;
			else ia[0][i]=10*istuff();
		} else {
			if(ranflg == 0) ia[0][i]=1000.*sin(dt*2.*3.1415926/20.)+1000.;
			else ia[0][i]=istuff();
		}
		ia[1][i]=(ia[0][i])/4;			/* copy to other channels. */
		ia[2][i]=(ia[0][i])/10;
		dt=dt+.025;					/* time of next sample */
		if(fabs(dt-floor(dt)) <=.025) {
			lp[0][*nlp]=10000.*sin(dtl/20.*3.1415926);
			lp[1][*nlp]=lp[0][*nlp]/4;/* copy to other chans*/
			lp[2][*nlp]=lp[0][*nlp]/10;
			dtl=dtl+1.;					/* next time */
			tcl=nsnadd(tcl,1000);		/* time of next LP */
/*			fprintf(logout,"LP %d %d\n",*nlp,lp[0][*nlp]);*/
			(*nlp)++;
		}
	}
	trigforce=0;					/* reset trig force flag */
	tcint=nsnadd(tcint,*nsamp*1000/SPFREQ);	/* compute next time */
	return tc;						/* return time code */
}
int istuff()
{
	int i;
	extern double ranfreq,ranmag;
	static int irun,iw,ival=0;
	static double w=0.;
	if(irun <= 0) {
		irun=200.*ran(0);	/* get run length */
		iw=ran(0)*4194305.;	/* get offset */
	}
	ival=ival+iw;
	if(abs(ival) > 16777216) {
		ival=ival-2*iw;
		iw=-iw;
	}
	return ival;

	w+= 3.1415926/10.*ranfreq;
	i=sin(w)*ranmag;
	return i;
}
double ran(ix)
long ix;
{
	static double xx;
	double a;
	int i;
	a=ix;
	if(ix == 0) a=129.*xx+1.;
	i=a/34359738368.;
	xx=a-i*34359738368.;
	a=2.91038304567337036e-11*xx;
	return a;
}
