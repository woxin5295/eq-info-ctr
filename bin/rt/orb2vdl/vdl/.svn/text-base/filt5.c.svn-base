#include <math.h>
#include <stdio.h>
#include <string.h>				/* Changed from Strings.h for Earthworm */
/*main()
{
	long ia[2000];
	long history[270],output[1000];
	int i,j;
	for (i=0; i<2000; i++) ia[i]=sin(3.1415926/500.*i)*10000.;
	for (i=0; i<64; i++) history[i]=0;
	j=0;
	for (i=0; i<2000; i+=5) {
		output[j]=filt5(history,&ia[i]);
		printf("%7d ",output[j++]);
		if( (i%20) == 0) printf("\n");
	}
}*/
#ifdef __STDC__
  long filt5(long history[],long data[])
#else
  long filt5(history, data)
  long history[],data[];
#endif
/*
	Filt10 takes two new data points in DATA and computes one OUTPUT point and
	maintains the history so the next call will have the correct history.  This 
	filter is based on the Q680N_3 filter in NETRSP: of the NSN source.  It is the
	filter Quanterra uses for decimation by 2.
	
	D.C. Ketchum Mar 1995 - Initial Functionality
*/
#define NCOEF	100
#define NDIV	5
{
	static double cd2[NCOEF] = {
  3.26563000e-07,  6.33260000e-07,  5.80818000e-07, -1.01565000e-06, -6.33460000e-06,
 -1.87720000e-05, -4.28524000e-05, -8.36933000e-05, -1.45932000e-04, -2.32161000e-04,
 -3.41113000e-04, -4.66018000e-04, -5.93696000e-04, -7.04979000e-04, -7.76850000e-04,
 -7.86372000e-04, -7.15968000e-04, -5.59059000e-04, -3.24694000e-04, -3.96210000e-05,
  2.53443000e-04,  5.01929000e-04,  6.53407000e-04,  6.67806000e-04,  5.29163000e-04,
  2.53751000e-04, -1.08307000e-04, -4.79672000e-04, -7.70524000e-04, -8.99036000e-04,
 -8.13215000e-04, -5.08537000e-04, -3.59907000e-05,  5.03228000e-04,  9.77089000e-04,
  1.25319000e-03,  1.23375000e-03,  8.86415000e-04,  2.61813000e-04, -5.08843000e-04,
 -1.23828000e-03, -1.72626000e-03, -1.81207000e-03, -1.42318000e-03, -6.06267000e-04,
  4.70300000e-04,  1.54702000e-03,  2.33268000e-03,  2.57997000e-03,  2.15850000e-03,
  1.10463000e-03, -3.68453000e-04, -1.90808000e-03, -3.10241000e-03, -3.58647000e-03,
 -3.14790000e-03, -1.80284000e-03,  1.82236000e-04,  2.33534000e-03,  4.08545000e-03,
  4.90838000e-03,  4.47630000e-03,  2.76981000e-03,  1.17590000e-04, -2.85532000e-03,
 -5.36668000e-03, -6.67475000e-03, -6.28675000e-03, -4.12191000e-03, -5.79407000e-04,
  3.51786000e-03,  7.10179000e-03,  9.12450000e-03,  8.84788000e-03,  6.08102000e-03,
  1.29550000e-03, -4.42629000e-03, -9.61428000e-03, -1.27645000e-02, -1.27437000e-02,
 -9.14235000e-03, -2.47694000e-03,  5.82861000e-03,  1.37038000e-02,  1.88947000e-02,
  1.95345000e-02,  1.46899000e-02,  4.74422000e-03, -8.49351000e-03, -2.20100000e-02,
 -3.21194000e-02, -3.52107000e-02, -2.85584000e-02, -1.10178000e-02,  1.65598000e-02,
  5.13019000e-02,  8.87094000e-02,  1.23410000e-01,  1.50120000e-01,  1.64640000e-01
	};
	int j,i;
	double sum,sum2;
	j=2*NCOEF-1;						/* last point in buffer */
	memcpy(&history[NCOEF*2-NDIV], &data[0], NDIV*sizeof(long));/* new data at end*/
	sum=0.;								/* zero the summation variable */
/*	printf("\n");
	for(i=0; i<NCOEF*2; i++) {
		printf("%7d ",history[i]);
		if( (i%10) == 0) printf("\n%d data",i+1);
	}
	printf("\n");*/
/*	sum2=0.;*/
	for (i=0; i<NCOEF; i++) {
		sum+=cd2[i]*history[i];			/* Convolve 1st half of series */
		sum+=cd2[i]*history[j];			/* Convolve 2nd half of series */
		j--;
/*		sum2+=cd2[i];*/
	}
/*	for(i=0; i<NCOEF*2; i++) {
		printf("%7d ",history[i]);
		if( (i%10) == 9) printf("\n");
	}
	printf("\nSum=%f\n",sum);*/
	memcpy(&history[0],&history[NDIV], (NCOEF*2-NDIV)*sizeof(long));/* save history*/
	return (long) sum;					/* return answer as a long int */
}

