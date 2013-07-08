#include <math.h>
#include <stdio.h>
#include <string.h>					/* changed from strings.h for earthworm*/
/*main()
{
	long ia[2000];
	long history[64],output[1000];
	int i,j;
	for (i=0; i<2000; i++) ia[i]=sin(3.1415926/20.*i)*10000.;
	for (i=0; i<64; i++) history[i]=0;
	j=0;
	for (i=0; i<2000; i+=2) {
		output[j]=filt2(history,&ia[i]);
		printf("%7d ",output[j++]);
		if( (i%20) == 0) printf("\n");
	}
}*/	
#ifdef __STDC__
  long filt2(long history[], long data[])
#else
  long filt2(history, data) 
  long history[],data[];
#endif
/*
	Filt2 takes two new data points in DATA and computes one OUTPUT point and
	maintains the history so the next call will have the correct history.  This 
	filter is based on the Q680N_3 filter in NETRSP: of the NSN source.  It is the
	filter Quanterra uses for decimation by 2.
	
	D.C. Ketchum Mar 1995,- Initial Functionality
*/
#define NCOEF	32
#define NDIV	2

{
	static double cd2[32]={
	  2.88049545e-04, 1.55313976e-03, 2.98230513e-03, 2.51714466e-03,-5.02926821e-04
	,-2.81205843e-03,-8.08708369e-04, 3.21542984e-03, 2.71266000e-03,-2.91550322e-03
	,-5.09429071e-03, 1.33933034e-03, 7.40034366e-03, 1.82796526e-03,-8.81958286e-03
	,-6.56719319e-03, 8.38608573e-03, 1.24268681e-02,-5.12978853e-03,-1.84868593e-02
	,-1.79236766e-03, 2.33604181e-02, 1.30477296e-02,-2.51709446e-02,-2.93134767e-02
	, 2.12669298e-02, 5.21898977e-02,-6.61517353e-03,-8.83535221e-02,-3.66062373e-02
	, 1.86273292e-01, 4.03764486e-01};
	int j,i;
	double sum,sum2;
	j=2*NCOEF-1;						/* last point in buffer */
	memcpy(&history[NCOEF*2-NDIV], &data[0], NDIV*sizeof(long));/* new data at end*/
	sum=0.;	
/*	sum2=0;							/* zero the summation variable */
	for (i=0; i<NCOEF; i++) {
		sum+=cd2[i]*history[i];			/* Convolve 1st half of series */
		sum+=cd2[i]*history[j];			/* Convolve 2nd half of series */
		j--	;
/*		sum2+=cd2[i];*/
	}
/*	for(i=0; i<NCOEF*2; i++) {
		printf("%7d ",history[i]);
		if( (i%10) == 9) printf("\n");
	}
	printf("\nSum=%f sum2=%f\n",sum,sum2);*/
	memcpy(&history[0],&history[NDIV], (NCOEF*2-NDIV)*sizeof(long));/* save history*/
	return (long) sum;					/* return answer as a long int */
}
