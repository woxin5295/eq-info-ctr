#include <math.h>				/* we use the math package here */
#include "ifft.h"				/* declare prototype functions */
/*#define			DEBUG	1*/
#define         N       256		/* SD uses shorter buffers */
#define         NR      5		/* Number of windows */
#define         MEX     7		/* pow of 2 that yields N/2 */
#define         NP1     (N+1)	/* FFT length + 1 */
#define         NP2     (N+2)	/* FFT length + 2 */
#define         NM1     (N-1)	/* FFT length -1 */
#define         NM2     (N-2)	/* FFT length -2*/
#define         N2      (N/2)	/* FFT length /2 */
#define         NF      (N2+1)	/* FFT length/2+1 */
#define         NL      (N2+2)	/* FFT length/2+2 */
#define         N4      (N2/2)	/* FFT length/4 */
#define         NT      (N4/2)	/* FFT length /8*/
#define         NH      (N4+1)	/* FFT length/4 +1 (# of cosines in table)*/
#define         MS      14		/* Shift amounts for normalization */
#define         NS      -14
#define         NC      16384	/* normalizaton constant for cosines */
#define         MX1     65535	/* maxiumu unsiged I*2 */
#define         MX2     32767	/* maximum signed I*2 */
#define         MX3     4194303	/* 2^22-1 				*/
#define         W0      (3.1415926536/N2)	/* PI over the Samples per 1/4 cosine */

#define         iabs(A)         ((A>=0)?A:-A)
static int ic[NH];					/* the cosines intitialized by IFFTST */
static int iw[NR][2] = {			/* .078125 Hz per coeff DC in zeroth */
3,6,								/* 0.235 - 0.4695 */
6,12,								/* 0.4695 - 0.939 Hz */
12,24,								/* .939 - 1.878 Hz */
24,48,								/* 1.878 - 3.756 */
48,96};								/* 3.756 - 7.512 */
/************************************** ifft *******************************/
/*
   Ifft transforms the N point integer time series stored in array ia in 
   place returning the N/2+1 points of the non-negative frequency portion of the 
   power spectrum.  Power averaged over 6 octave bands is returned in array 
   ir.  The powers are returned as integer values which must be multiplied by 
   2**ie to yield absolute power levels.  The series is tapered with a 12.5% 
   cosine, transformed (taking advantage of the Hermitian nature of the 
   series), and powers are formed and averaged renormalizing as needed.
   The 
*/
#ifdef __STDC__
  void ifft256(int *ie, long ia[], long ir[])
#else
  void ifft256(ie,ia,ir)
  int *ie;
  long ia[],ir[];
#endif
{
   int i,j,k,l,m,le,ke,le1,mr,mi,i1,m1;
   int me[NF],ne[NR];
   long iav,im,kr,ki,lr,li;
#ifdef DEBUG
float cn; 
#endif

   *ie = 0;
/* Compute the average of the time series.  Note that data should never be in 
 danger of overflowing as the maximum data value is 24-bits and the largest 
 mean should be on the order of 19-bits. */
   iav = 0;
   for(i = 0; i < N; i++) iav += ia[i];
   iav = iav/N;

   im = 0;                      /* De-mean the data. */
   for(i = 0; i < N; i++)
   {
      ia[i] -= iav;
      im = mxabs(im,ia[i]);
   }
   renorm(0,NM1,im,MX1,ie,ia);

#ifdef DEBUG
 cn = pow(2.,(double) *ie);
   printf("\nde-meaned:  %d %10.3e\n",*ie,cn);
   i = 0;
   for(k = 0; k < 3; k++)
   {
      for(j = 0; j < 6; j++) printf(" %10.3e",cn*ia[i++]);
      printf("\n");
   } 
#endif

/* Taper the data.  Using the same cosines available for the transform we can 
 do a 12.5% cosine taper handily.  Note that we don't renormalize the series to 

 account for the scaling of the cosine in order to avoid losing precision for 
 small data values. */
   ia[0] = 0;
   ia[NM1] = 0;
   im = 0;
   k = N4;
   for(i = 1; i < N2; i++)      /* Do both ends at the same time. */
   {
      m = NM1-i;
      if(i < NT)                /* Apply the taper. */
      {
         k -= 2;
         ia[i] = ic[k]*ia[i];
         ia[m] = ic[k]*ia[m];
      }
      else                      /* Scale the untapered portion of the data. */
      {
         ia[i] = kshft(ia[i],MS);
         ia[m] = kshft(ia[m],MS);
      }
      im = mxabs(mxabs(im,ia[i]),ia[m]);
   }	
   *ie += NS;                   /* Compensate for the cosine scaling. */
   renorm(0,NM1,im,MX1,ie,ia);

#ifdef DEBUG
 cn = pow(2.,(double) *ie);
   printf("\ntapered:  %d %10.3e\n",*ie,cn);
   i = 0;
   for(k = 0; k < 3; k++)
   {
      for(j = 0; j < 6; j++) printf(" %10.3e",cn*ia[i++]);
      printf("\n");
   } 
#endif

/* Permute the data in preparation for the FFT calculation. */
   j = 1;
   for(i = 1; i <= NM2; i += 2)
   {
      if(i < j)
/* Exchange two elements.  Note that the only the real part of the time series 
 is given.  As the imaginary part is zero, the odd terms of the time series 
 are treated as the real part and the even terms of the time series are 
 treated as the imaginary part of a complex series of half the length. */
      {
         kr = ia[j-1];
         ki = ia[j];
         ia[j-1] = ia[i-1];
         ia[j] = ia[i];
         ia[i-1] = kr;
         ia[i] = ki;
      }
      k = N2;                   /* Fiddle the indicies. */
      while(k < j)
      {
         j -= k;
         k = k/2;
      };
      j += k;
   }

#ifdef DEBUG
 cn = pow(2.,(double) *ie);
   printf("\npermuted:  %d %10.3e\n",*ie,cn);
   i = 0;
   for(k = 0; k < 3; k++)
   {
      for(j = 0; j < 6; j++) printf(" %10.3e",cn*ia[i++]);
      printf("\n");
   } 
#endif

/* Do the FFT calculation. */
   le = 2;
   ke = N;
   for(l = 1; l <= MEX; l++)    /* Loop over each power of two. */
   {
      le1 = le;
      le = 2*le;
      ke = ke/2;
      im = 0;
      k = 0;
      for(j = 0; j < le1; j += 2)       /* Loop over each exponential argument. 
*/
      {
         if(k < NH)             /* Get cosine and sine for 0 - pi/2. */
         {
            mr = ic[k];
            mi = ic[N4-k];
         }
         else                   /* Get cosine and sine for pi/2 - pi. */
         {
            mr = -ic[N2-k];
            mi = ic[k-N4];
         }
         for(i = j; i < N; i += le)     /* Loop over data values for each 
                                           exponential argument. */
         {
            i1 = i+1;
            m = i+le1;
            m1 = m+1;
            kr = kshft(ia[m]*mr-ia[m1]*mi,NS);  /* Compensate for the cosine */
            ki = kshft(ia[m1]*mr+ia[m]*mi,NS);  /* scaling. */
            ia[m] = ia[i]-kr;           /* Accumulate the complex integrals. */
            ia[m1] = ia[i1]-ki;
            ia[i] = ia[i]+kr;
            ia[i1] = ia[i1]+ki;
            im = mxabs(mxabs(mxabs(mxabs(im,ia[m]),ia[m1]),ia[i]),ia[i1]);
         }
         k += ke;
      }
      renorm(0,NM1,im,(l < MEX)?MX1:MX2,ie,ia);

#ifdef DEBUG
      cn = pow(2.,(double) *ie);
      printf("\nmex:  %d %d %10.3e\n",l,*ie,cn);
      i = 0;
      for(k = 0; k < 3; k++)
      {
         for(j = 0; j < 6; j++) printf(" %10.3e",cn*ia[i++]);
         printf("\n");
      } 
#endif
   }

/* Use the fact that the transform must be Hermitian to recover the positive 
 frequency half of the transform of the original time series from the complete 
 transform of the time series taken as half the number of real and imaginary 
 pairs.  Begin by faking the Nyquist spectral value. */
   ia[N] = ia[0];
   ia[NP1] = ia[1];
   k = 0;
   for(i = 0; i < NL; i += 2)           /* Loop over the spectrum. */
   {
      i1 = i+1;
      m = N-i;
      m1 = m+1;
      mr = ic[k];                       /* Get the cosine and sine. */
      mi = ic[N4-k];
      lr = ia[i]-ia[m];                 /* Combine pairs of spectral values. */
      li = ia[i1]+ia[m1];
      kr = kshft(mr*li+mi*lr,NS);       /* Compensate for the cosine scaling. */

      ki = kshft(mi*li-mr*lr,NS);
      lr = ia[i]+ia[m];                 /* Re-combine pairs of values. */
      li = ia[i1]-ia[m1];
      ia[m] = lr-kr;                    /* Reconstruct the series. */
      ia[m1] = ki-li;
      ia[i] = lr+kr;
      ia[i1] = ki+li;
      k++;
   }

#ifdef DEBUG
   cn = pow(2.,(double) *ie);
   printf("\nspectrum:  %d %10.3e\n",*ie,cn);
   i = 0;
   for(k = 0; k < 3; k++)
   {
      for(j = 0; j < 6; j++) printf(" %10.3e",cn*ia[i++]);
      printf("\n");
   } 
#endif

   for(i = 0; i < NF; i++) me[i] = 0;   /* Zero out the exponent flags. */

/* Form the power spectrum in place. */
   do
   {
      k = 0;
      ke = 0;
      for(i = 0; i < NP2; i += 2)
      {
         if(me[k] <= 0)         /* Skip if this point has been done. */
            if(iabs(ia[i]) <= MX2 && iabs(ia[i+1]) <= MX2)
                                /* Spectral value in range, compute power. */
            {
               ia[i] = ia[i]*ia[i]+ia[i+1]*ia[i+1];
               me[k] = 2*(*ie)+10000;
               ke++;
            }
            else                /* Spectral value not in range, shift down. */
            {
               ia[i] = kshft(ia[i],-4);
               ia[i+1] = kshft(ia[i+1],-4);
            }
         else
            ke++;
         k++;
      }
      *ie += 4;                 /* Compensate for the shifts. */
   } while(ke < NF);

/* Get the largest exponent, taking account of the squaring of the spectrum 
 which doubles the exponent. */
   *ie = 2*(*ie-4);
   k = 1;
   for(i = 2; i < NP2; i += 2)  /* Shift the powers down. */
      ia[k++] = ia[i];

#ifdef DEBUG
   printf("\nPower:  %d\n",*ie);
   i = 0;
   for(k = 0; k < 3; k++)
   {
      for(j = 0; j < 6; j++) printf(" %10.3e",
         pow(2.,(double) (me[i]-10000))*ia[i++]);
      printf("\n");
   } 
#endif

/* Compute average powers. */
   le = -10000;
   for(l = 0; l < NR; l++)      /* Loop over octave bands. */
   {
      mr=iw[l][0];
      mi=iw[l][1];				/* Get upper limit */
      if(mr > NF) mr=NF;
      if(mi > NF) mi=NF;
      
      ke = -10000;              /* Get the largest exponent. */
      for(i = mr; i <= mi; i++)
         ke = (ke >= me[i]-10000)?ke:me[i]-10000;
      le = (le >= ke)?le:ke;
      im = 0;                   /* Normalize powers to the same exponent. */
      for(i = mr; i <= mi; i++)
      {
         ia[i] = kshft(ia[i],me[i]-10000-ke);
         im = (im >= ia[i])?im:ia[i];
      }
      renorm(mr,mi,im,MX3,&ke,ia);
      im = 0;
      for(i = mr; i <= mi; i++) /* Sum the powers. */
      {
         me[i] = ke+10000;
         im += ia[i];
      }
      ne[l] = ke;
      ir[l] = im;
   }
	 
   *ie = (*ie >= le)?*ie:le;    /* Normalize powers to the same exponent. */
   for(i = 0; i < NF; i++)
      ia[i] = kshft(ia[i],me[i]-10000-(*ie));

#ifdef DEBUG
   cn = pow(2.,(double) *ie);
   printf("\npower:  %d %10.3e\n",*ie,cn);
   i = 0;
   for(k = 0; k < 42; k++)
   {
      for(j = 0; j < 6; j++) printf(" %10.3e",cn*ia[i++]);
      printf("\n");
   } 
   for(j = 0; j < 5; j++) printf(" %10.3e",cn*ia[i++]);
   printf("\n"); 
#endif

   for(i = 0; i < NR; i++)      /* Normalize average powers the same way. */
      ir[i] = kshft(ir[i],ne[i]-(*ie));

#ifdef DEBUG
   cn = pow(2.,(double) *ie);
   printf("\nRMS:  %d %10.3e\n",*ie,cn);
   for(i = 0; i < 6; i++) printf(" %10.3e",cn*ir[i]);
   printf("\n"); 
#endif

   return;
}
/***********************************************************************/
void ifftst256()
/*
   Ifftst must be called once before the first call to ifft to set up 
   constants.  Note that all sines and cosines are derived from one 
   quarter cycle of the cosine at the lowest frequency needed.
*/
{
   int i;
   ic[0] = NC;
   for(i = 1; i < N4; i++) ic[i] = NC*cos(i*W0);
   ic[N4] = 0;
   return;
}

