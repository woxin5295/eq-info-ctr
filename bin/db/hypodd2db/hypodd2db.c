/*
  This program moves the hypocenter data from HYPODD reloc files to the 
  origin table of the CSS database.

Usage:  hypodd2db filename database

   where

        filename = HYPODD summary file (*.reloc) or file of identical format 
        database = database prefix 

Note: orid and evid numbers are taken verbatim from the HYPODD output file.
Note: This version supports the 174 byte hypocenter record in the HYPODD
      update which extended the magnitude field to a width of 5 places.

David von Seggern 10/10/2005
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "db.h"
#include "stock.h"
#include "coords.h"

int main(int argc, char *argv[])
{
  int		  leap,yr,mon,day,ihr,imn,ilat,ilon,jday;
  int             ndays[12] = { 1,32,60,91,121,152,182,213,244,274,305,335};
  int             flag,orid,evid;
  int             ne = 0;
  float           latm,lonm,rlat,rlon,sec,depth,mag,sdobs;
  double          time;
  char            *database, *filename;
  char            astring[176];
  char            a1[2];
  char            a2[3];
  char            a3[4];
  char            a4[5];
  char            a5[6];
  char            a6[7];
  char            linefeed,nshemi,ewhemi;
  FILE            *fptr,*errfptr;
  Dbptr           db;
/*Following should probably be queried as input.*/
  char		  auth[16] = "unknown";
  char            algorithm[16] = "HYPODD";

  if (argc < 3)
  {
	printf("Usage: hypodd2db hypofile database\n");
        return 1;
  }

/*printf("%d\n",argc);*/
  filename = argv[1];
  printf("%s\n",filename);
  fptr=fopen(filename,"r");
  database = argv[2];
  dbopen(database, "r+", &db);
  printf("%s\n",database);
  fflush(stdout);

/*Open a file for errors.*/
  errfptr=fopen("bad_records","w");

/*HYPODD record is 174 bytes plus 1 for EOL, so ask for 176 to ensure that
  the whole line is read and entered into astring.  fgets adds the null
  character after the EOL character to make 176 bytes*/
  while (fgets(astring,176,fptr) != NULL)
  {
/*  Scan out the required variables.*/
    sscanf(astring+  0,"%9d",&orid);
    sscanf(astring+103,"%4d",&yr);
    sscanf(astring+108,"%4d",&mon);
    sscanf(astring+111,"%2d",&day);
    sscanf(astring+114,"%2d",&ihr);
    sscanf(astring+117,"%2d",&imn);
    sscanf(astring+120,"%6f",&sec);
    sscanf(astring+ 10,"%10f",&rlat);
    sscanf(astring+ 21,"%11f",&rlon);
    sscanf(astring+ 35,"%7f",&depth);
    sscanf(astring+127,"%5f",&mag);

    jday = ndays[mon-1] - 1 + day;
/*  printf("%d\n",jday);*/
    leap = yr % 4;
    if (leap == 0 && jday >= 61)
      jday = jday + 1;
/*  Special case: March 1 on leap year.*/
    if (leap == 0 && mon == 3 && day == 1)
      jday = 61;
/*  printf("%d %d %d\n",yr,jday,leap);*/
    time = h2e(yr, jday, ihr, imn, (double) sec);
    evid = orid;

    printf("%8d %4d %03d %02d:%02d:%05.2f %7.4f %9.4f %5.2f %5.2f\n",
    orid,yr,jday,ihr,imn,sec,rlat,rlon,depth,mag);

/*  Now make the origin table entry.*/
               
    if (dbaddv(db, "origin", "lat", rlat, "lon", rlon, "depth", depth,
	       "time", time, "jdate", yearday(time), "ml", mag, 
               "algorithm", algorithm, "auth", auth, 
               "orid", orid, "evid", evid, 0) < 0)

   {
      printf("couldn't add %8d %4d %3d %2d %2d %5.2f to origin table\n", orid,yr,jday,ihr,imn,sec);
/*    Break to next event if origin table write fails -- write record to
      an error file first. */
      fputs(astring,errfptr);
      continue;
    }

    ne = ne + 1;
    if (ne % 100 == 0) printf("# events processed = %7d\n",ne);
  }
  fclose(fptr);
  fclose(errfptr);
  printf("total # events processed = %7d\n",ne);
  return 0;
}
