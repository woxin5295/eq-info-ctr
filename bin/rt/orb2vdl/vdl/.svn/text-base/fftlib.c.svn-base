#include <math.h>				/* we use the math package here */
#include "ifft.h"				/* declare prototype functions */
#define	iabs(A)	((A>=0)?A:-A)
/****************************** mxabs ********************************/
/*
   Mxabs returns the maximum of im and iabs(ia).
*/
#ifdef __STDC__
  int mxabs(long im, long ia)
#else
  int mxabs(im,ia)
long im,ia;
#endif
{
   long kabs;

   kabs = iabs(ia);
   return((im >= kabs)?im:kabs);
}
/*************************************** kshft ***********************/
/*
   Kshft implements an arithmetic shift of longword ia by ib bits to 
   the left (right is ib is negative) using the intrinsic logical shift 
   function ishft.
*/
#ifdef __STDC__
  int kshft(long ia, int ib)
#else
  int kshft(ia,ib)
  long ia;
 int ib;
#endif
{
   if(ib < 0)
      if(ia >= 0) 
         return(ia>>(-ib));             /* Right shift of a positive value. */
      else
         return(ia>>(-ib)|-1<<(32+ib)); /* Right shift of a negative value. */
   else if(ib > 0)
      return(ia<<ib);                   /* Left shift. */
   else
      return(ia);                       /* No shift. */
}
/************************************* renorm ******************************/
/*
   Renorm re-normalizes nn elements of array ia by arithmetic shifts to 
   the right such that the maximum value after the shift will be less 
   than or equal to mx.  Im is the maximum value before the shift.
*/
#ifdef __STDC__
  void renorm(int n1, int n2, long im, long mx, int *me, long ia[])
#else
  void renorm(n1,n2,im,mx,me,ia)
  int n1,n2,*me;
  long im,mx,ia[];
#endif
{
   long is;
   int ks,i;

   is = (im-1)/mx;      /* Ratio of the current to the desired maximum. */

   if(is > 0)
   {
      ks = 0;                           /* Count the shifts needed. */
      do
      {
         ks = ks-1;
         is = is/2;
      }
      while (is > 0);

      for(i = n1; i <= n2; i++)         /* Shift ia down. */
         ia[i] = kshft(ia[i],ks);
      *me = *me-ks;                     /* Remember the shift count. */
   }
   return;
}

