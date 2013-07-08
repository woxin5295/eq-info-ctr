/*********************************************************/
/* suds_time.c: time and date utility routines for suds. */
/*       Written by Bruce Julian and Peter Ward,         */
/*    U. S. Geological Survey, Menlo Park, Ca. 94025     */
/*********************************************************/
#include <sys/time.h>
#include <math.h>
#include <stdio.h>

#define	BASEJDN		2440588			/* 1 January 1970	*/

/* Calendar codes */
#define	GREGORIAN	1			/* Modern calendar	*/
#define	JULIAN		0			/* Old-style calendar	*/

/* Time intervals */
#define	SECOND		1
#define	MINUTE		(60*SECOND)
#define	HOUR		(60*MINUTE)
#define	DAY		(24*HOUR)

typedef int bool;

/* floor(x/y), where x, y>0 are integers, using integer arithmetic */
#define qfloor(x, y) (x>0 ? (x)/y : -((y-1-(x))/y))

static int	eom[2][15] = {
   { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365, 396, 424 },
   { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366, 397, 425 },
};

/* Recognize leap years */
bool suds_is_leap(yr, cal)
   int	yr;		/* Year			*/
   int	cal;		/* Calendar type	*/
{
   bool	l;

   if (yr < 0)
      yr++;
   l = (yr%4 == 0);
   if (cal == GREGORIAN)
      l = l && (yr%100 != 0 || yr%400 == 0);
   return l;
}

/* Compute day of year */
int suds_year_day(mo, day, lp)
   int	mo, day;	/* Month, day		*/
   bool	lp;		/* Leap year?		*/
{
   return eom[lp][mo-1] + day;
}

/* Calculate month, day from day of year */
void suds_month_day(d, lp, pm, pd)
   int	d;		/* Day of year		*/
   bool	lp;		/* Leap year?		*/
   int	*pm, *pd;	/* Month, day		*/
{
   int	i;
   int	*et = eom[lp];

   for (i=1; d>et[i]; i++)
		;
   *pm = i;
   *pd = d - et[i-1];
}

/* Compute Julian Day number from calendar date */
long suds_jdn(yr, mo, day, cal)
   int	yr, mo, day;	/* Year, month, day	*/
   int	cal;		/* Calendar type 	*/
{
   long ret;

   if (yr < 0)
      yr++;

   /* Move Jan. & Feb. to end of previous year */
   if (mo <= 2) {
      --yr;
      mo += 12;
   }

   ret=qfloor((long)(4*365+1)*(yr+4712), 4) + eom[0][mo-1] + day +
      (cal==GREGORIAN ? -qfloor(yr, 100) + qfloor(yr, 400) + 2 : 0);
   return(ret);
}

/* Compute calendar date from Julian Day Number */
void suds_date (n, py, pm, pd, cal)
   long	n;			/* Julian day number	*/
   int	*py, *pm, *pd;		/* Year, month, day	*/
   int	cal;			/* Calendar type	*/
{
   long	d, t;
   int	y;
   bool suds_is_leap();
   void suds_month_day();

/* Find position within cycles that are nd days long */
#define CYCLE(n, nd) { t=qfloor(d-1,nd); y+=t*n; d-=t*nd; }

/* The same, with bound on cycle number */
#define LCYCLE(n, nd, l) { t=qfloor(d-1,nd); if (t>l) t=l; y+=t*n; d-=t*nd; }

   y = -4799;
   if (cal == GREGORIAN) {
        d = n + 31739;		/* JD -31739 = 31 Dec 4801 B.C.	*/
      CYCLE(400, 146097)	/* Four-century cycle		*/
      LCYCLE(100, 36524, 3)	/* 100-year cycle		*/
   }
   else d = n + 31777;		/* JD -31777 = 31 Dec 4801 B.C.	*/

   CYCLE(4, 1461)		/* Four-year cycle		*/
   LCYCLE(1, 365, 3)		/* Yearly cycle			*/

   if (y <= 0) --y;
   *py = y;
   suds_month_day((int)d, suds_is_leap(y, cal), pm, pd);
}

extern int errno;

double get_suds_time()
{
   extern char *program_name;
   struct timeval tp;
   struct timezone tzp;

   if(gettimeofday(&tp,&tzp)== -1) {
      fprintf (stdout, "%s:\tget_suds_time: Error: Can't read system clock\n",
               program_name);
      return(0.0);
   }
   return((double)tp.tv_sec+((double)tp.tv_usec)/1000000.0);
}

double make_suds_time (year,month,day,hour,min,second)
   int year, month, day, hour, min;
   double second;
{
   extern char *program_name;
   int err=0;
   bool suds_is_leap();
   long suds_jdn();
   void suds_month_day();
   int	*et = eom[suds_is_leap(year,GREGORIAN)];

   if(year==0) {
      fprintf (stdout, "%s:\tmake_suds_time: Error: Year is %d\n",
               program_name, year);
      err=1;
   }
   if(month==0) suds_month_day(day,suds_is_leap(year,GREGORIAN),&month,&day);
   if(month<1 || month>12) {
      fprintf (stdout, "%s:\tmake_suds_time: Error: Month is %d\n",
               program_name, month);
      err=1;
   }
   if(day<1 || day>et[month]-et[month-1]) {
      fprintf (stdout, "%s:\tmake_suds_time: Error: Day is %d\n",
               program_name, day);
      err=1;
   }
   if(err) return(0.0);
   return((double) DAY*(suds_jdn(year,month,day,GREGORIAN)-BASEJDN) +
      HOUR*hour + MINUTE*min + SECOND*second);
}

int decode_suds_time (time,year,month,day,hour,min,second)
   double time;
   int *year,*month,*day,*hour,*min;
   double *second;
{
   long	d;

   d = floor(time/DAY);
   suds_date (d+BASEJDN,year,month,day,GREGORIAN);
   time -= d*DAY;
   *hour = time/HOUR;
   time -= (*hour)*HOUR;
   *min = time/MINUTE;
   *second = time - (*min)*MINUTE;
   return(1);
}
#ifdef DEBUG_SUDS
main (ac, av)
  int ac;  char *av[];
{
   double tim,tim1,sec, make_suds_time();
   int i,year,month,day,hour,min, suds_year_day();
   bool suds_is_leap();
   void decode_suds_time();

   tim=get_suds_time();
   printf("system time = %f\n",tim);
   decode_suds_time(tim,&year,&month,&day,&hour,&min,&sec);
   tim1=make_suds_time(year,month,day,hour,min,sec);
   printf("remade time is %f which differs by %f\n",tim1,tim1-tim);
   day=suds_year_day(month,day,suds_is_leap(year,GREGORIAN));
   printf("day of year is %d\n",day);
   tim1=make_suds_time(year,0,day,hour,min,sec);
   printf("remade time is %f which differs by %f\n",tim1,tim1-tim);
}
#endif
