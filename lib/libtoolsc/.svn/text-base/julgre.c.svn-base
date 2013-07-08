/*************************************************************
 ***    A function that passes back the Gregorian month    ***
 *** and day given a year and Julian day.  If Julian date  ***
 *** given isn't legal, ONE is returned.  Otherwise, ZERO  ***
 ***   is returned.  Written by G. H. Cole Sonafrank 9/82. ***
 ***         Translated into "C" by GHCS, 3/15/88.         ***
 *************************************************************/
#include <stdio.h>
int julgre (year, julian, month, day)
  int year, julian;
  int *month, *day;
{
  
  int leap;
  static int prior[24] = {0,31,59,90,120,151,181,212,243,273,304,334,
			  0,31,60,91,121,152,182,213,244,274,305,335};

  if ((year % 4) == 0) {
    leap = 11;
    if (julian <= 0 | julian > 366) return (1);
    }
  else {
    leap = -1;
    if (julian <= 0 | julian > 365) return (1);
    }
  /* printf ("leap=%d",leap); */

  *month = 13;
  do {
    *month -= 1;
    /* printf (" month=%d", *month); */
    } while (julian <= prior[*month+leap]);

  *day = julian - prior[*month+leap];

  return (0);
} /* End of function julgre() */
/* main ()
 * {
 * int year, julian, month, day, status, julgre();
 * 
 * do { 
 *   printf ("Enter Julian date (yy,jjj): ");
 *   status = scanf ("%d,%d",&year,&julian);
 *   if (status == 2) {
 *     if (! julgre (year, julian, &month, &day))
 *       printf ("  Gregorian date is: month=%d day=%d year=%d\n",month,day,year);
 *     else  puts ("  Error in Julian date");
 *     }
 *   else puts ("");
 *   } while (status != EOF);
 * puts ("");
 * }
 */
