/*************************************************************************/
/* Routines to do various conversions on dates and times.  Some of these */
/* routines use others that are in the file suds_time.c.  Also, programs */
/* must be linked with the -lm switch.  For example to compile gre_diff: */
/* cc -O -DGRE_DIFF -o gre_diff time_conv.c suds_time.o -lm              */
/*************************************************************************/

#include <stdio.h>

/*************************************************************
 ***    A function that passes back the Gregorian month    ***
 *** and day given a year and Julian day.  If Julian date  ***
 *** given isn't legal, ONE is returned.  Otherwise, ZERO  ***
 ***   is returned.  Written by G. H. Cole Sonafrank 9/82. ***
 ***         Translated into "C" by GHCS, 3/15/88.         ***
 *************************************************************/
int julgre (year, julian, month, day)
  int year, julian;
  int *month, *day;
{
  
  int leap;
  static int prior[24] = {0,31,59,90,120,151,181,212,243,273,304,334,
			  0,31,60,91,121,152,182,213,244,274,305,335};

  if (((year%4) == 0) && (((year%100) != 0) || ((year%400) == 0)) ) {
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

/*************************************************************************/

#ifdef JULGRE
main () {
int year, julian, month, day, status, julgre();
do { 
  printf ("Enter Julian date (yy,jjj): ");
  status = scanf ("%d,%d",&year,&julian);
  if (status == 2) {
    if (! julgre (year, julian, &month, &day))
      printf ("Gregorian date is: month=%d day=%d year=%d\n",month,day,year);
    else  puts ("  Error in Julian date");
    }
  else puts ("");
  } while (status != EOF);
puts ("");
}
#endif

/*************************************************************************/

/*************************************************************
 ***    A function that returns the Julian date given a    ***
 ***  Gregorian one.  If the date given isn't legal, zero  ***
 ***   is returned.  Written by G. H. Cole Sonafrank 9/82. ***
 ***         Translated into "C" by GHCS, 7/8/87.          ***
 *************************************************************/
int grejul (year, month, day)
  int year, month, day;
{
  static int check[24] = {31,28,31,30,31,30,31,31,30,31,30,31,
			  31,29,31,30,31,30,31,31,30,31,30,31};

  static int prior[24] = {0,31,59,90,120,151,181,212,243,273,304,334,
			  0,31,60,91,121,152,182,213,244,274,305,335};
  int index;

  if (year > 0 & month >= 1 & month <= 12) {
    index = month - 1;
    if (((year%4) == 0) && (((year%100) != 0) || ((year%400) == 0)) ) {
      index += 12;
      }
    if (day >= 1 & day <= check[index]) return (prior[index]+day);
    else return (0);
    }
  else return(0);
}

/*************************************************************************/

#ifdef GREJUL
main () {
int year, month, day, status;  int grejul();
do { 
  printf ("Enter Gregorian date (mm,dd,yy): ");
  status = scanf ("%d,%d,%d",&month,&day,&year);
  if (status == 3) printf ("  Julian date is: %d\n",grejul(year,month,day));
  else printf ("\n");
  } while (status != EOF);
printf ("\n");
}
#endif

/********************************************************************/
/****      julian_to_n converts the Julian datetime string       ****/
/****       into numeric datetime fields.  GHCS, 10/4/91.        ****/
/********************************************************************/
void julian_to_n (julian_datetime, year, julian, hour, minute, second)

  char  *julian_datetime;
  int   *year, *julian, *hour, *minute;
  double *second;

{

  char year_string[3], julian_string[4];
  char hour_string[3], minute_string[3], second_string[7];
  int  i=9, atoi();
  double atof();

  year_string[0]   = julian_datetime[0];
  year_string[1]   = julian_datetime[1];
  year_string[2]   = '\0';
  julian_string[0] = julian_datetime[2];
  julian_string[1] = julian_datetime[3];
  julian_string[2] = julian_datetime[4];
  julian_string[3] = '\0';
  hour_string[0]   = julian_datetime[5];
  hour_string[1]   = julian_datetime[6];
  hour_string[2]   = '\0';
  minute_string[0] = julian_datetime[7];
  minute_string[1] = julian_datetime[8];
  minute_string[2] = '\0';
  do {
    second_string[i-9] = julian_datetime[i];
    } while ((julian_datetime[i++] != '\0') &&
             (i < 16));
  second_string[6] = '\0';

  *year   = atoi (year_string);
  *julian = atoi (julian_string);
  *hour   = atoi (hour_string);
  *minute = atoi (minute_string);
  *second = atof (second_string);

#ifdef DEBUG
  fprintf (stderr, "\tyr=%d, jul=%d, hr=%d, min=%d, sec=%f\n",
           *year, *julian, *hour, *minute, *second);
#endif

} /* End julian_to_n() */

/********************************************************************/
/****   gregorian_to_n converts the Gregorian datetime string    ****/
/****       into numeric datetime fields.  GHCS, 10/4/91.        ****/
/********************************************************************/
void gregorian_to_n (gregorian_datetime,
                     year, month, day, hour, minute, second)

  char  *gregorian_datetime;
  int   *year, *month, *day, *hour, *minute;
  double *second;

{

  char year_string[3], month_string[3], day_string[3];
  char hour_string[3], minute_string[3], second_string[7];
  int  i=10, atoi();
  double atof();

  year_string[0]   = gregorian_datetime[0];
  year_string[1]   = gregorian_datetime[1];
  year_string[2]   = '\0';
  month_string[0]  = gregorian_datetime[2];
  month_string[1]  = gregorian_datetime[3];
  month_string[2]  = '\0';
  day_string[0]    = gregorian_datetime[4];
  day_string[1]    = gregorian_datetime[5];
  day_string[2]    = '\0';
  hour_string[0]   = gregorian_datetime[6];
  hour_string[1]   = gregorian_datetime[7];
  hour_string[2]   = '\0';
  minute_string[0] = gregorian_datetime[8];
  minute_string[1] = gregorian_datetime[9];
  minute_string[2] = '\0';
  do {
    second_string[i-10] = gregorian_datetime[i];
    } while ((gregorian_datetime[i++] != '\0') &&
             (i < 17));
  second_string[6] = '\0';

  *year   = atoi (year_string);
  *month  = atoi (month_string);
  *day    = atoi (day_string);
  *hour   = atoi (hour_string);
  *minute = atoi (minute_string);
  *second = atof (second_string);

#ifdef DEBUG
  fprintf (stderr, "\tyr=%d, mn=%d, day=%d, hr=%d, min=%d, sec=%f\n",
           *year, *month, *day, *hour, *minute, *second);
#endif

} /* End gregorian_to_n() */

/********************************************************************/
/****   n_to_gregorian converts the numeric datetime arguments   ****/
/****     into a Gregorian datetime string.  GHCS, 10/4/91.      ****/
/********************************************************************/
void n_to_gregorian (gregorian_datetime,
                     year, month, day, hour, minute, second)

  char *gregorian_datetime;
  int year, month, day, hour, minute;
  double second;

{
  if (year > 100) year -= 1900;
  (void) sprintf (gregorian_datetime, "%02d%02d%02d%02d%02d%06.3f",
                  year, month, day, hour, minute, second);
  if (gregorian_datetime[10] == ' ') gregorian_datetime[10] = '0';
#ifdef DEBUG
  printf ("Debug: %s\n", gregorian_datetime);
#endif

} /* End n_to_gregorian() */

/********************************************************************/
/**** n_to_julian converts the numeric datetime field arguments  ****/
/****       into a Julian datetime string.  GHCS, 10/4/91.       ****/
/********************************************************************/
void n_to_julian (julian_datetime, year, julian, hour, minute, second)

  char *julian_datetime;
  int year, julian, hour, minute;
  double second;

{
  if (year > 100) year -= 1900;
  (void) sprintf (julian_datetime, "%02d%03d%02d%02d%06.3f",
                  year, julian, hour, minute, second);
  if (julian_datetime[9] == ' ') julian_datetime[9] = '0';
#ifdef DEBUG
  printf ("Debug: %s\n", julian_datetime);
#endif

} /* End n_to_julian() */

#ifdef TEST_MAIN
main (ac, av) int ac; char *av[];
{int y,j,h,m; double s; char dt[15]; void julian_to_n(), n_to_julian();
 julian_to_n(av[1],&y,&j,&h,&m,&s); n_to_julian(dt,y,j,h,m,s);}
#endif

/*************************************************************************/

#ifdef GARBAGE
double julian_to_double (julian_datetime)

  char *julian_datetime;

{

  int year, julian, hour, minute, n_leap;
  double second;
  void julian_to_n();
  double datetime;

  julian_to_n (julian_datetime, &year, &julian, &hour, &minute, &second);

  n_leap = year / 4 - year / 100;
  datetime = year * 365 + n_leap + julian;
  datetime = datetime * 24 + hour;
  datetime = datetime * 60 + minute;
  datetime = datetime * 60 + second;

  return (datetime);

} /* End julian_to_double() */
#endif

/********************************************************************/
/****   This routine converts the Gregorian datetime argument    ****/
/****    into SUDS' double format which can be arithmatically    ****/
/****                manipulated.  GHCS, 10/4/91.                ****/
/********************************************************************/
double gregorian_to_double (gregorian_string)

  char *gregorian_string;

{

  int year, julian, month, day, hour, minute, length;
  int decode_suds_time(), julgre();
  double datetime, second, make_suds_time();
  void gregorian_to_n();
  size_t strlen();

  length = strlen (gregorian_string);

#ifdef DEBUG
  printf ("length = %d\n", length);
#endif

  if (length >= 12 && length <= 15) {
    gregorian_to_n (gregorian_string,
                    &year, &month, &day, &hour, &minute, &second);
    year += 1900;
#ifdef DEBUG
    printf ("yr=%d, mon=%d, day=%d, hr=%d, min=%d, sec=%f\n",
            year, month, day, hour, minute, second);
#endif
    }

  else {
    printf ("Error, unexpected argument length.\n");
    exit (1);
    }
  
  datetime = make_suds_time (year, month, day, hour, minute, second);
#ifdef DEBUG
  printf ("dt=%f\n", datetime);
#endif

  return (datetime);

} /* End gregorian_to_double() */

/********************************************************************/
/****  This routine converts the argument given in SUDS' double  ****/
/****  format into a Gregorian datetime string.  GHCS, 10/4/91.  ****/
/********************************************************************/
char *double_to_gregorian (datetime)

  double datetime;

{
  int year, month, day, hour, minute, decode_suds_time();
  double second;
  void n_to_gregorian();
  static char gregorian_datetime[17];

  (void) decode_suds_time (datetime,
                           &year,&month,&day,&hour,&minute,&second);

  (void) n_to_gregorian (gregorian_datetime,
                         year, month, day, hour, minute, second);

  return (gregorian_datetime);

} /* End double_to_gregorian() */

/********************************************************************/
/**** This routine converts the julian datetime string argument  ****/
/****    into SUDS' double format which can be arithmatically    ****/
/****                manipulated.  GHCS, 10/4/91.                ****/
/********************************************************************/
double julian_to_double (julian_string)

  char *julian_string;

{

  int year, julian, month, day, hour, minute, length;
  int decode_suds_time(), julgre();
  double datetime, second, make_suds_time();
  void julian_to_n();
  size_t strlen();

  length = strlen (julian_string);

#ifdef DEBUG
  printf ("length = %d\n", length);
#endif

  if (length == 11 || length == 9) {
    julian_to_n (julian_string, &year, &julian, &hour, &minute, &second);
    year += 1900;
#ifdef DEBUG
    printf ("yr=%d, jday=%d, hr=%d, min=%d, sec=%f\n",
            year, julian, hour, minute, second);
#endif
  
    (void) julgre (year, julian, &month, &day);
#ifdef DEBUG
    printf ("mon=%d, day=%d\n", month, day);
#endif
    }
  else {
    printf ("Error, unexpected argument length.\n");
    exit (1);
    }
  
  datetime = make_suds_time (year, month, day, hour, minute, second);
#ifdef DEBUG
  printf ("dt=%f\n", datetime);
#endif

  return (datetime);

} /* End julian_to_double() */

/********************************************************************/
/****  This routine converts the argument given in SUDS' double  ****/
/**** time format into a Julian datetime string.  GHCS, 10/4/91. ****/
/********************************************************************/
char *double_to_julian (datetime)

  double datetime;

{
  int year, julian, month, day, hour, minute, decode_suds_time(), grejul();
  double second;
  void n_to_julian();
  static char julian_datetime[16];

  (void) decode_suds_time (datetime,
                           &year,&month,&day,&hour,&minute,&second);

  julian = grejul (year, month, day);

  (void) n_to_julian (julian_datetime,
                         year, julian, hour, minute, second);

  return (julian_datetime);

} /* End double_to_julian() */

/********************************************************************/
/****     This is an example program which subtracts a given     ****/
/****  number of seconds from a gregorian date and returns the   ****/
/****         resulting string.  GHCS, October 4, 1991.          ****/
/********************************************************************/
#ifdef GRE_DIFF
#include <stdio.h>

char *program_name = {"gre_diff"};

main (ac, av)

  int ac;
  char *av[];

{
  double gregorian_to_double(), atof(), datetime_1, datetime_2, second;
  char *gregorian_datetime, *double_to_gregorian();

  if (ac >= 3) {

    datetime_1 = gregorian_to_double (av[1]);
    datetime_2 = datetime_1 - atof (av[2]);

    gregorian_datetime = double_to_gregorian (datetime_2);

    printf ("%s\n", gregorian_datetime);
    }

  else {
    fprintf (stderr, "Usage: gre_diff yymmddhhmmss [nsec]\n");
    }
}
#endif

/********************************************************************/
/****     This is an example program which subtracts a given     ****/
/****    number of seconds from a julian date and returns the    ****/
/****         resulting string.  GHCS, October 4, 1991.          ****/
/********************************************************************/
#ifdef JUL_DIFF
#include <stdio.h>

char *program_name = {"jul_diff"};

main (ac, av)

  int ac;
  char *av[];

{

  double julian_to_double(), atof(), datetime_1, datetime_2, second;
  char *julian_datetime, *double_to_julian();

  if (ac >= 3) {

    datetime_1 = julian_to_double (av[1]);
    datetime_2 = datetime_1 - atof (av[2]);

    julian_datetime = double_to_julian (datetime_2);

    printf ("%s\n", julian_datetime);
    }

  else {
    fprintf (stderr, "Usage: jul_diff yyjjjhhmmss [nsec]\n");
    }
}
#endif
