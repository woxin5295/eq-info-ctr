/*
	Routine to implement the filtering and decimation in VDL.
*/
#include <math.h> 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vdlfilt.h"
extern FILE * logout;

/*	Setfilt - returns a structure of form decimate which will decimate a
	time series by a factor of input_rate/output_rate and optionally apply
	the helicorder filter.  Note : the output rate must be 10 Hz for the 
	helicorder filter to be in the right place.   SETFILT figures the
	factors of 10, 5, and 2 needed to decimate between the input and output
	rates and create the chain of filters needed for the decimation factor.
	The user then can call the decimate routine with the output structure
	from SETFILT each time with the decimation factor of data.
*/

struct decimate setfilt(input_rate,output_rate,helifilt)
int input_rate,output_rate,helifilt;
{	int i,j,nhist;
	struct decimate filt;
	int dbg=0;
#ifdef DEBUG_PRINT
	dbg=1;
#endif
	i=input_rate/output_rate;
	filt.decimation=i;					/* set decimation factor */
	filt.nstage=0;						/* initialize stage counter */
	if(i > 100 || i < 2)
	{ 	fprintf(logout,"SETFILT : bad decimation=%d\n",i);
		exit(303);
	}
	if(dbg) {fprintf(logout,"Compute decimate=%d in=%d out=%d\n",
			filt.decimation,input_rate,output_rate); 	
		fflush(logout);}
	switch (i)
	{case 16:							
		filt.stage[filt.nstage]=2;		/* a stage of two */
		filt.nstage++;
	case 8:
		filt.stage[filt.nstage]=2;		/* a stage of two */
		filt.nstage++;
	case 4:
		filt.stage[filt.nstage]=2;		/* a stage of two */
		filt.nstage++;
	case 2 : 
		filt.stage[filt.nstage]=2;		/* a stage of two */
		filt.nstage++;
		break;
	case 50:
		filt.stage[filt.nstage]=10;		/* a stage of ten */
		filt.nstage++;
	case 5:
		filt.stage[filt.nstage]=5;		/* a stage of five */
		filt.nstage++;
		break;
	case 40:
		filt.stage[filt.nstage]=2;		/* a stage of two */
		filt.nstage++;
	case 20: 
		filt.stage[filt.nstage]=2;		/* a stage of two */
		filt.nstage++;
	case 10:
		filt.stage[filt.nstage]=10;		/* a stage of ten */
		filt.nstage++;
		break;
	default:
		fprintf(logout,"SET filt unknown decimation=%d\n",i);
		exit(303);
	}
	if(helifilt)						/* add heli shaping stage */
	{	filt.stage[filt.nstage]=-1;
		filt.nstage++;
	}
	if(filt.nstage > MAX_FILT_STAGE) 
	{	fprintf(logout,"setfilt : nstages to great!=%d\n",filt.nstage);
		exit(304);
	}
	nhist=0;
	if(dbg) {fprintf(logout,"Nstages=%d\n",filt.nstage); fflush(logout);}
	filt.delay=0;							/* initialize delay */
	j=input_rate;
	for(i=0; i<filt.nstage; i++)
	{	switch (filt.stage[i])
		{	case 2:
				filt.nhist[i]=66;			/* size of history for two stage*/
				nhist += filt.nhist[i];
				filt.delay += 30400/j;		/* delay amount in MS */
				j=j/2;
				break;
			case 10:
				filt.nhist[i]=264;			/* size of history for ten stage*/
				nhist += filt.nhist[i];
				filt.delay += 113720/j;
				j=j/10;
				break;
			case 5:
				filt.nhist[i]=208;			/* size of history for five stage*/
				nhist += filt.nhist[i];		/* iris says 20.00 sec delay from */
				filt.delay += 100000/j;	/* 5 hz to 1 add on delay in MS */
				j = j/5;
				break;
			case -1:
				filt.nhist[i]=24*sizeof(double)/sizeof(long);/* memory for history */
				nhist += filt.nhist[i];
				filt.delay += 1;
				break;
			default:
				fprintf(logout,"Bad stage found in set file=%d\n",
					filt.stage[i]);
				exit(304);
		}
	}
/*
	allocate and initialize history array 
*/
	if(dbg) {fprintf(logout,"History malloc for %d\n",nhist*sizeof(long)); 
			fflush(logout);}
	filt.hist= (long *) malloc( (nhist * sizeof(long)));
	if(dbg) {fprintf(logout,"malloc=%x nhist=%d\n",filt.hist,nhist); 	
			fflush(logout);}
	memset(filt.hist, 0, nhist*sizeof(long));
/*
	debug - dump out filter for inspection
*/
	if(dbg) 
	{	fprintf(logout,"In=%d out=%d Nstages=%d History space=%d\n",
			input_rate,output_rate,filt.nstage,nhist);
		for (i=0; i<filt.nstage; i++) 
			fprintf(logout,"%d decim=%d histsize=%d %x delay=%d\n",
			i,filt.stage[i],filt.nhist[i],filt.hist,filt.delay);
	}
	return filt;
}



/*	Decimate applies the filter set up in "struct decimate filt" to new data.
	This computes one point of the new series so the amount of data in 
	data must be the decimation factor. (if you are decimating between 100 and
	20 Hz then there must be 5 data points in data each invocation giving
	back the one point of decimated data.

*/
long decimate(filt,data)
struct decimate filt;
long data[];
{	int i,j,istage;
	int dbg=0;
	long output[100];				/* max # of points to process */
	long * pnt;						/* pointer to history array */
	i=filt.decimation;				/* I will count down the decimation */
	memcpy(output,data,i*sizeof(long));/* move timeseris to output */
	pnt=filt.hist;					/* sliding pointer for history stages */
	for (istage=0; istage<filt.nstage; istage++)/* for each filtering stage */
	{	switch (filt.stage[istage])
		{
			case 2:
				if(dbg) fprintf(logout,"%d Apply 2 decimation for i=%d\n",
						istage,i);
				for(j=0; j<i; j+=2) 
				{	output[j/2] = filt2(pnt,&output[j]);
				}
				pnt += filt.nhist[istage];		/* point to next history */
				i = i/2;						/* half as much data */
				break;
			case 10: 
				if(dbg) fprintf(logout,"%d Apply 10 decimation for i=%d\n",
					istage,i);
				for(j=0; j<i; j+=10)
				{	output[j/10] = filt10(pnt,&output[j]);
				}
				pnt += filt.nhist[istage];
				i = i/10;
				break;
			case 5:
				if(dbg) fprintf(logout,"%d Apply 5 decimation for i=%d\n",
					istage,i);
				for(j=0; j<i; j+=5)
				{	output[j/5] = filt5(pnt, &output[j]);
				}
				pnt += filt.nhist[istage];
				i = i/5;
				break;
			case -1:
				output[0]=filtheli(pnt,output[0]);
				pnt += filt.nhist[istage];
				break;
			default :
				fprintf(logout,"DECIMATE : bad filter stage=%d\n",
					filt.nhist[istage]);
				exit(305);
		}
	}							/* end of each stage */
	if(i != 1) fprintf(logout,"Filter stage divide down error ! %d\n",i);
	return output[0];
}
