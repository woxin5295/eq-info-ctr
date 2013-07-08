/*
This collection of functions manipulate the Quanterra supplied QINIT/QTRAP
interface.  The QTRAPs routine was written for calling by FORTRAN and hence 
expects all arguments to be passed by reference.  For convenience from C these
routines will pass by value in and convert to reference calls to QTRAP.  
References to the functions in the Quanterra/FORTRAN interface have FORTRAN
style names like QTRAP_ and QINIT_ (caps mandatory).

D.C. Ketchum Mar 1991
*/
#include <stdio.h>
#include <errno.h>
#include "vdl.h"
#include "vdlqsub.h"		/* Include function prototypes and globals */

/**************************** I2swap ******************************/
#ifdef __STDC__
  void i2swap(short i, void *swp)
#else
  void i2swap(i,swp)
  short i;
  void *swp;
#endif
{	extern FILE *logout;
	unsigned char *swap;
	union {
 		short i2;
		unsigned char sw[2];
	} u;
	swap=(unsigned char *) swp;
	u.i2=i;
	*swap=u.sw[1];
	*(swap+1)=u.sw[0];
	return; 
} 
/************************************* i4swap ***************************/
#ifdef __STDC__
   void i4swap(long i, void *swp)
#else
  void i4swap(i,swp) 
  long i;
  void *swp; 
#endif
{ 
	char *swap;
 	union {
	long l;
	unsigned char b[4];
	} u;
	swap=(char *) swp;
	u.l=i;
	for (i=0; i< 4; i++) swap[3-i]=u.b[i];
	return;
}
/***********************************************************************/
#ifdef __STDC__
  int yrday(struct nsntime tc, long *iyear, long *doy)
#else
  int yrday(tc,iyear,doy)					/* From NSN time in TC get year and day*/
  struct nsntime tc;					/* a time code in */
  long *iyear,*doy;					/* return year and day of year */
#endif
{
	*iyear=tc.iyr/2+1970;			/* return year */
	*doy=tc.doy+((tc.iyr & 1) ? 256 : 0);
	return 0;
}
/*
	calculates time difference in MS between TC1-TC2.  It does not at this
	time correctly account for leap seconds though leap years are considered
	
	D.C. Ketchum Jan 1995
*/
#ifdef __STDC__
  int nsnsub(struct nsntime tc1, struct nsntime tc2)
#else
  int nsnsub(tc1,tc2)
  struct nsntime tc1,tc2;
#endif
{
	long iy1,id1,iy2,id2;			/* years and days of time code */
	int iday;						/* days separated counter */
#ifdef _INTEL
	i4swap( (long) tc1.ms,&tc1.ms);
	i4swap( (long) tc2.ms,&tc2.ms);
#endif
	yrday(tc1,&iy1,&id1);				/* get day and year */
	yrday(tc2,&iy2,&id2);				/* of second time */
	if(iy1 == iy2 && id1 == id2) {
		return (tc1.ms-tc2.ms)/16;
	}
	if(iy1 == iy2) {
		if(abs(id1-id2) > 24) {
			if(id1 > id2) return 2147483647;
			else return -2147483647;
		}
		return ((id1-id2)*86400000+(tc1.ms-tc2.ms)/16);
	}
	if(abs(iy1-iy2) > 1) {
		if(iy1 > iy2) return 2147483647;
		else return -2147483647;
	}
	if( (iy1-iy2) == 1) {			/* first day is in later year */
		iday=id1+365-id2;			/* days unless its leap year */
		if( (iy2 % 4) == 0 && (iy2 % 400) != 0) iday++;
		return (iday*86400000+(tc1.ms-tc2.ms)/16);
	} else {
		iday=id1-(id2+365);
		if( (iy2 % 4) == 0 && (iy2 % 400) != 0) iday;
		return (iday*86400000+(tc1.ms-tc2.ms)/16);
	}
	return 2147483647;
}  
#ifdef __STDC__
  struct nsntime maknsn(int iy, int id, int ih, int im,int is, int ms, int leap)
#else
  struct nsntime maknsn(iy,id,ih,im,is,ms,leap)
  int iy,id,ih,im,is,ms,leap;
#endif
{
	struct nsntime tc;
	if(iy > 1900) iy-=1900;
	tc.iyr=(iy-70)*2;
	if(id >= 256) {tc.iyr++;id-=256;}
	tc.doy=id;
	tc.ms=(ih*3600000+im*60000+is*1000+ms)*16;
	if(leap != 0) {
		if(leap == 1) ms= ms | 8;
		if(leap == -1) ms=ms | 4;
	}
#ifdef _INTEL
	i4swap( (long) tc.ms,&tc.ms);
#endif
	return tc;
}
	
/***********************************************************************/
#ifdef __STDC__
  int nsnint(struct nsntime tc, long *yr, long *day, long *hr, long *min, long *sec,long *ms)
#else
  int nsnint(tc,yr,day,hr,min,sec,ms)
  struct nsntime tc;
  long *yr,*day,*hr,*min,*sec,*ms;
#endif
/*  Convert a Time code to its correct time expressed as integers.  0 is 
normal return.  -1 returned means the time code had more the 24 hours in it
and was not a leap second.*/
{
	int msec;						/* error return */
	*yr=1970+tc.iyr/2;				/* get year */
	*day=tc.doy+((tc.iyr & 1) ? 256 : 0);/* compute day of year */
	msec=tc.ms;
#ifdef _INTEL
	i4swap(msec,&msec);
#endif
	msec=msec/16;					/* get number of MS since midnight */
/*	printf ("NSNINT msec=%d\n",msec); */
	*hr=msec/3600000;				/* compute hour of day */
	msec%=3600000;					/* remaining MS */
	*min=msec/60000;				/* compute minutes */
	msec%=60000;					/* remaining MS */
	*sec=msec/1000;					/* compute number of seconds */
	*ms=msec % 1000;
	msec=0;							/* use it as an error flag */
	if(*hr == 24) {
		if((tc.ms && 8) != 0) {
			(*hr)--;
			*min=59;
			*sec=60;
		}
		else
			msec=-1;
	}
	return msec;
}
/***********************************************************************/
/*
	nsnadd adds MSADD milleseconds to the USNSN time code in TC and returns
	a time code at that time. 
*/
#ifdef __STDC__
  struct nsntime nsnadd(struct nsntime tc, long msadd)
#else
  struct nsntime nsnadd(tc,msadd)
  struct nsntime tc; 
  long msadd;
#endif
{
	extern leapnext;				/* indicate leap status for tomorrow*/
	extern FILE *logout;
	long yr,day,hr,min,sec,ms,flags,nday,max,err,msorg,dayorg;
	if(msadd == 0) return tc;		/* trivial case */
	ms=tc.ms;
/*	if( abs(msadd) > 86400000) fprintf(logout,"NSNADD msadd big=%d\n",msadd);*/
#ifdef _INTEL
	i4swap(ms,&ms);
#endif
	flags=ms & 15;					/* save flags */
	ms=ms/16;					/* get number of ms */
	ms+=msadd;						/* add right number of ms to it */
	max=86400000;						/* assume a normal day */
	if((flags & 8) != 0) max=86401000;/* Its a positive leap day */
	if((flags & 4) != 0) max=86399000;/* Its a negative leap day */
 	if(ms >= 0 && ms < max) {		/* Easy case just add ms and return */
		tc.ms=ms*16+flags;			/* build new one */
#ifdef _INTEL
		i4swap(tc.ms,&tc.ms);
#endif
		return tc;
	}
	err=nsnint(tc,&yr,&day,&hr,&min,&sec,&msorg); /* break down time */
/*
If the number of MS is negative, add one days MS to it until its positive
and borrow from the days.  Correct the year if the day becomes < 0 
*/
	msorg=ms;						/* same MS count */
	dayorg=day;
	if(ms < 0) {					/* ms < 0  backwards in time */
		while (ms < 0) {
			ms+=86400000;			/* add a days worth */
			day--;					/* borrowd a day */
		}
		if(day <= 0) {				/* gone to prior year */
			yr--;					/* set year to prior */
			nday=(((yr % 4) == 0 && (yr % 400) != 0) ? 366 : 365); /* # of day */
			day+=nday;				/* correct day of year */
		}
	}
/*
	MS must be positive.  Adjust for days forward.  If tomorrow is a leap
	second day, take it into account.  If today is leap second, take it
	into account (MAX was set above to handle this case).  We must presume
	days further in the future that tommorrow are not leap second days.
*/
	while (ms >= max) {
		day++;						/* bump to tomorrow */
		ms-=max;					/* remove from today */
		max=86400000;				/* assume a normal day */
		if((day-dayorg) == 1) {		/* are we beyond tommorrow */
			if(leapnext != 1) max=86401000;/* Its a positive leap day */
			if(leapnext == -1) max=86399000;/* Its a negative leap day */
		}
		nday=(((yr % 4) == 0 && (yr % 400) != 0) ? 366 : 365);
		if(day > nday) {			/* Happy new year */
			yr++;
			day-=nday;				/* correct for number of days */
		}
	}
/*
The MS is now guaranteed to be positive.  Compute effect on days 
*/
/*	fprintf(logout,"Going to diff day %d %d %d\n",ms,day,yr);*/
	tc.iyr=(yr-1970)*2;			/* create year part */
	if(day < 256)
		tc.doy=day;				/* simple case no carry to yr part */
	else {
		tc.doy=day % 256;		/* set doy part */
		tc.iyr|=1;				/* set 256s of days in year byte */
	}
	tc.ms=ms*16;
	if(msorg > 0 && leapnext !=0 && (day-dayorg) == 1) {
		if(leapnext == 1) tc.ms|=8;		/* positive leap seconde */
		if(leapnext == -1) tc.ms|=4;	/* negative leap second */
	}
#ifdef _INTEL
	i4swap(tc.ms,&tc.ms);
#endif
	return tc;
}
/*********************** BUFTIM *********************************
*
*	calling Sequence :	buftim(ch,iss)
*	argument 		description
*	ch				Pointer to channel descriptor
*	iss				Offset in ring buffer to compute time for

*	Returns a time code per USNSN spec
*
*	Programmed by : D.C. Ketchum
*	Created :		Apr 1991
*	Date Modified Description of Mod
*	20-apr-92		Base functionality
*	2-Oct-92		Subtract MS_DELAY from all time code to compensate for
*						Filtering delay in Quanterra DSPs.
**********************************************************************/
#ifdef __STDC__
  struct nsntime buftim(struct chan_desc *ch,int iss)
#else
  struct nsntime buftim(ch,iss)
  int iss;
  struct chan_desc *ch;					/* channel control structure */
#endif
{
	struct nsntime tc;
	int msadd;
	int iy,id,ih,im,is,ms;
	if(iss < ch->ipnt || ch->ipnt == ch->max) 
		msadd=(double) iss/ch->freq_calc*1000.+.5;/* MS to add to initial time*/
	else msadd=(double)(iss-ch->max)/ch->freq_calc*1000.-.5;/* MS to subtract from init */
	tc=nsnadd(ch->tc,msadd);			/* convert time code */
/*	nsnint(ch->tc,&iy,&id,&ih,&im,&is,&ms);
	fprintf(logout,"buftim %s ch=%x is=%d ipnt=%d msadd=%d %2d:%2d:%2d.%3d ",
		ch->txt,ch,iss,ch->ipnt,msadd,ih,im,is,ms);
	nsnint(tc,&iy,&id,&ih,&im,&is,&ms);
	fprintf(logout,"%2d:%2d:%2d.%3d\n",ih,im,is,ms);*/
	return tc;
}
/* asctime returns a pointer to a string with the current GMT in it */
char *asctim()
{
	char *t;			/* pointer to string */
	struct tm *tm;
	time_t now;
	now=time(&now);		/* what time is it */
	tm=gmtime(&now);	/* convert to GMT */
	t=asctime(tm);		/* parse to a string */
	*(t+20)=0;			/* eliminate the new line and year */
	return (t);			/* hand to user */
}
