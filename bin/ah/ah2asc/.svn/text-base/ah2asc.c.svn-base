/* FILE: ah2asc.c                               (D. Tottingham  12/26/89)

Converts data from ah (Lamont) format to ASCII.
                                                                          */

/*************************************************************************
                            INCLUDE FILES

*************************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include "ak_ahheader.h"
#include "mconst.h"


/************************************************************************
                                DEFINES

************************************************************************/
#define BLOCK_SIZE      4096

#ifdef SUN3
#define SEEK_CUR        1
#define SEEK_END        2
#define SEEK_SET        0
#endif

#ifdef SUN4
#define SEEK_CUR        1
#define SEEK_END        2
#define SEEK_SET        0
#endif


/*************************************************************************
                                GLOBALS

*************************************************************************/
PRIVATE ak_ahhed lam;


/*=======================================================================*
 *                            get_datasize                               *
 *=======================================================================*/
/* Calculate the data size.                                              */

PRIVATE
long get_datasize ()
{
   switch (lam.record.type) {
      case ak_ahSHORT:
         return (2L);
      case ak_ahFLOAT:
      case ak_ahLONG:
         return (4L);
      case ak_ahDOUBLE:
         return (8L);
      default:
         printf ("ERROR: Unknown datatype of %d\n", lam.record.type);
         exit(0);
   }
}

/*=======================================================================*
 *                           display_header                              *
 *=======================================================================*/
/* Display an ah header.                                                 */

PRIVATE
void display_header (calflag)
FLAG calflag;
{
   int i;

   printf ("\n\nStation info for %s:\n", lam.station.code);
   printf ("   chan  = %s\n   stype = %s\n", lam.station.chan, lam.station.stype);
   printf ("   slat  = %f\n   slon  = %f\n", lam.station.slat, lam.station.slon);
   printf ("   elev  = %f\n   DS    = %f\n", lam.station.elev, lam.station.DS);
   if (calflag) {
     printf ("   A0    = %f\n   Calibration curve:\n", lam.station.A0);
     for (i = 0; i < NOCALPTS; i++)
      printf ("      pole:  r = %6.2f, i = %6.2f;  zero: r = %6.2f, i = %6.2f\n",
              lam.station.cal[i].pole.r, lam.station.cal[i].pole.i,
              lam.station.cal[i].zero.r, lam.station.cal[i].zero.i);
   }

   printf ("\nEvent info for %s:\n   lat  = %f\n", lam.station.code, lam.event.lat);
   printf ("   lon  = %f\n   dep  = %f\n", lam.event.lon, lam.event.dep);
   printf ("   Time = %d %d %d %d:%02d:%f\n", lam.event.ot.yr, lam.event.ot.mo,
           lam.event.ot.day, lam.event.ot.hr, lam.event.ot.mn, lam.event.ot.sec);
   printf ("   Comment = %s\n", lam.event.ecomment);

   printf ("\nRecord info for %s:\n   type    = %d\n", lam.station.code, lam.record.type);
   printf ("   ndata   = %ld\n   delta   = %.9f\n", lam.record.ndata, lam.record.delta);
   printf ("   maxamp  = %f\n", lam.record.maxamp);
   printf ("   Abstime = %d %d %d %d:%02d:%f\n", lam.record.abstime.yr,
           lam.record.abstime.mo, lam.record.abstime.day, lam.record.abstime.hr,
           lam.record.abstime.mn, lam.record.abstime.sec);
   printf ("   rmin    = %f\n   rcomment = %s\n", lam.record.rmin,
           lam.record.rcomment);
   printf ("   log     = %s\n", lam.record.log);
   printf ("\nExtras for %s:\n", lam.station.code);
   for (i = 0; i < NEXTRAS; i++) {
      printf ("   %2d = %10.4f", i, lam.extra[i]);
      if (! ((i+1) % 4)) printf ("\n");
   }
   printf ("\n");
}

/*=======================================================================*
 *                            display_data                               *
 *=======================================================================*/
/* Display some ah data.                                                 */

PRIVATE
void display_data (input, onedataflag)
FLAG onedataflag;
FILE *input;
{
   static char in_buffer[BLOCK_SIZE];
   long datasize, datalength, blocksize, readsize, bytes_remaining;
   int i, j;

   if (! onedataflag)
     printf ("\nData for %s:\n", lam.station.code);
   else
     printf ("\nFirst data value for each block for %s:\n", lam.station.code);
    
   datasize = get_datasize();

   j = 0;
   datalength = datasize * lam.record.ndata;
   blocksize = ((BLOCK_SIZE) / datasize) * datasize;
   for (bytes_remaining = datalength; bytes_remaining; bytes_remaining -= readsize) {
      readsize = (bytes_remaining >= blocksize) ? blocksize : bytes_remaining;
      readsize = fread (in_buffer, sizeof(char), readsize, input);
      if (! onedataflag)  
        for (i = 0; i < readsize; i += datasize) {
           switch (lam.record.type) {
              case ak_ahSHORT:
                 printf ("%13d", *((short *) &in_buffer[i]));
                 break;
              case ak_ahFLOAT:
                 printf ("%13.4f", *((float *) &in_buffer[i]));
                 break;
              case ak_ahLONG:
                 printf ("%13ld", *((long *) &in_buffer[i]));
                 break;
              case ak_ahDOUBLE:
                 printf ("%13.4lf", *((double *) &in_buffer[i]));
                 break;
           }
           j = (j == 5) ? 0 : j + 1;
           if (! j) printf ("\n");
        }
      else {
           switch (lam.record.type) {
              case ak_ahSHORT:
                 printf ("%13d", *((short *) &in_buffer[0]));
                 break;
              case ak_ahFLOAT:
                 printf ("%13.4f", *((float *) &in_buffer[0]));
                 break;
              case ak_ahLONG:
                 printf ("%13ld", *((long *) &in_buffer[0]));
                 break;
              case ak_ahDOUBLE:
                 printf ("%13.4lf", *((double *) &in_buffer[0]));
                 break;
           }
           j = (j == 5) ? 0 : j + 1;
           if (! j) printf ("\n");
      }  
    }
   printf ("\n");
}

/*=======================================================================*
 *                              skip_data                                *
 *=======================================================================*/
/* Skip the data.                                                        */

PRIVATE
void skip_data (input)
FILE *input;
{
   long datalength;

   datalength = get_datasize() * lam.record.ndata;
   fseek (input, datalength, SEEK_CUR);
}

main (argc, argv)
int argc;
char * argv[];
{
   int  fd;		/* file descriptor for regular open for get_ah() */
   FILE *input;
   FLAG dataflag;
   FLAG onedataflag;
   FLAG calflag;
   char *fname;

   /* Check command line and open files */
   dataflag = TRUE;
   calflag = TRUE;
   onedataflag = FALSE;
   if (argc < 2) {
      printf ("USAGE: ah2asc [-n, -s, or -t] [input filename] \n");
      printf ("-n skips trace data listing \n");
      printf ("-s also skips calibration listing \n");
      printf ("-t also skips cal but lists one data value per buffer \n");
      exit(0);
   }

   fname == NULL;
   while (--argc) {
      if (argv[argc][0] == '-')
         switch (argv[argc][1]) {
             case 'n':
                dataflag = FALSE;
		calflag = TRUE;
                break;
             case 's':
                dataflag = FALSE;
		calflag = FALSE;
                break;
             case 't':
                dataflag = TRUE;
		calflag = FALSE;
	        onedataflag = TRUE;
                break;
         }
      else fname = argv[argc];
   }

   /* open file using open() */
   if ((fd= open(fname, O_RDONLY)) == -1) {
      printf("ERROR: Unable to open %s for input.\n", argv[1]);
      exit(0);
   }

   /* associate stream with file */
   if ((input = fdopen (fd, "r")) == NULL) {
      printf ("ERROR: Unable to fd-open %s for input.\n", argv[1]);
      exit(0);
   }

   printf ("FILE: %s\n", fname);
   while (get_ah(fd, &lam)) {
   display_header (calflag);
      if (dataflag)
         display_data (input, onedataflag);
      else skip_data (input);
   }

   /*fclose (input);*/
   close (fd);
}


/* this code ripped from get_ah.c by Fred Williams, Nov. 1990
.
.
.
*/
/*******************************************************/
/*                      get_ah()                       */
/*******************************************************/
/* A function which reads the AH header.  This is done */
/*   in pieces to avoid problems from padding due to   */
/* forced longword alignment (e.g. on the Masscomps).  */
/*******************************************************/
/*   Written by G. H. Cole Sonafrank, March 13, 1988   */
/*******************************************************/

#define SUCCESS 1
#define ABORT 0


int get_ah (fd, ah)

  int fd;
  ak_ahhed *ah;

{

  if (read (fd, (char *)&ah->station, 520) != 520) {
    fprintf (stderr, "Error reading first section of AH header.\n");
    perror ("get_ah:read");
    return (ABORT);
    }

  if (read (fd, (char *)&ah->event, 22) != 22) {
    fprintf (stderr, "Error reading second section of AH header.\n");
    perror ("get_ah:read");
    return (ABORT);
    }

  if (read (fd, (char *)&ah->event.ot.sec, 86) != 86) {
    fprintf (stderr, "Error reading third section of AH header.\n");
    perror ("get_ah:read");
    return (ABORT);
    }

  if (read (fd, (char *)&ah->record.ndata, 22) != 22) {
    fprintf (stderr, "Error reading fourth section of AH header.\n");
    perror ("get_ah:read");
    return (ABORT);
    }

  if (read (fd, (char *)&ah->record.abstime.sec, 290) != 290) {
    fprintf (stderr, "Error reading fifth section of AH header.\n");
    perror ("get_ah:read");
    return (ABORT);
    }
  /* printf ("seconds in get_ah: %f\n", ah->record.abstime.sec); */

  if (read (fd, (char *)ah->extra, 84) != 84) {
    fprintf (stderr, "Error reading sixth section of AH header.\n");
    perror ("get_ah:read");
    return (ABORT);
    }

  return (SUCCESS);

} /* End get_ah() */
