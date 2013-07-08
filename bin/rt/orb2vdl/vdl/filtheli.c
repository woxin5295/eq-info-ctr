#include <math.h>
#include <stdio.h>
#include <string.h>				/* Changed from Strings.h for Earthworm */
extern FILE *logout;
#define NSEC 2
#define MXCOEF 5
#define NALLCO NSEC*MXCOEF

/* 
	This routine takes one point "a" of a 10 HZ data stream and applies the
	helicorder shaping filter used in the Quanterra code on NSN systems.  It
	is primarily called by the VDLFILT.C routines as part of the decimate and
	filter chain for creating a data set suitable for a helicorder.  History
	is an array at least 24 doubles long to store history data between
	invocations on a channel
*/

long filtheli(history, a)
double *history;
long a;
{	int n,j;
	static int nc[NSEC]={5,4};
	static int mc[NSEC]={4,3};
	double xc[NSEC][MXCOEF];
	double yc[NSEC][MXCOEF];
	static double bc[NSEC][MXCOEF]=
		{{4.3284664e-1,-1.7313866e+0, 2.5970799e+0,-1.7313866e+0,
		4.3284664e-1},{ 9.8531161e-2, 2.9559348e-1, 2.9559348e-1,
		9.8531161e-2, 0.}};
	static double ac[NSEC][MXCOEF]={ 
		{2.3695130e+0,-2.3139884e+0, 1.0546654e+0,-1.8737949e-1,
        0.          },{ 5.7724052e-1,-4.2178705e-1, 5.6297236e-2,
        0.          , 0.}};
	double yn;
	memcpy(xc,history,sizeof(double)*NALLCO);
	memcpy(yc,history+NALLCO,sizeof(double)*NALLCO);
	yn=a;
/*	fprintf(logout,"a=%d %x %d\n",a,history,sizeof(double));
	for(n=0;n<NSEC;n++) for(j=0;j<MXCOEF;j++) fprintf(logout,"%10.3e ",xc[n][j]);
	fprintf(logout," xc\n");
	for(n=0;n<NSEC;n++) for(j=0;j<MXCOEF;j++) fprintf(logout,"%10.3e ",yc[n][j]);
	fprintf(logout," yc\n");*/
	for(n=0; n<NSEC; n++)
	{	xc[n][0]=yn;
  		yn=0.;

		for(j=0; j<nc[n]; j++) yn=yn+bc[n][j]*xc[n][j];

		for(j=nc[n]-1; j>0; j--) xc[n][j]=xc[n][j-1];

		for(j=0; j<mc[n]; j++) yn=yn+ac[n][j]*yc[n][j];

		for(j=mc[n]-1; j>0; j--) yc[n][j]=yc[n][j-1];

		yc[n][0]=yn;
	}
/*	for(n=0;n<NSEC;n++) for(j=0;j<MXCOEF;j++) fprintf(logout,"%10.3e ",xc[n][j]);
	fprintf(logout," xc\n");
	for(n=0;n<NSEC;n++) for(j=0;j<MXCOEF;j++) fprintf(logout,"%10.3e ",yc[n][j]);
	fprintf(logout," yc\n");*/
	memcpy(history,xc,sizeof(double)*NALLCO);
	memcpy(history+NALLCO,yc,sizeof(double)*NALLCO);
	return (long) yn;
}
dbghist(hist)
long *hist;
{	int j;
	for(j=220;j<230; j++) fprintf(logout,"%8d ",*(hist+j));
	fprintf(logout," hist\n");
}
