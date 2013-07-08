#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include "db.h"
#include "stock.h"
#include "coords.h"

void make_landscape_pf( char * );

void make_southcentral_pf( char * );

void distance_report( double, double, char * );

char *compass_from_azimuth( double );

main( int argc, char **argv )

{
	char	*dbname;
	char    distances[10000];
	char	dtype[3], etype[3], ak_time_str[50], time_str[50], date_str[50];
	char 	start_day[50], end_day[50], end_jday[10], today[50];
	char	depth_str[10], mag_str[10], qual[5];
	char	title_expr[100], center[100], expr[100], tempdb[50], auth[20];
	char	dbmapevents[STRSZ], free_dep[1];
	char	month[25], small_year[4], year[4], station[5];
	char	map_command[512], mv_command[512], rm_command[512];
	Dbptr 	dbsrt, db, dbout, dbtemp, dbo, dberr, dba, dbev;
	Dbptr	dbl, dbl1, dbl2, dbl3, dbl4;
	double  range, latctr, lonctr, min_delta, min_dist;
	double	prev_esaz, esaz, largest_gap, gap, first_gap, last_gap;
	double	lat, lon, depth, aktime, time, mb, ml, minmag;
	double	sdepth, sminax, smajax, sdobs, strike;
	double	seh=0.0, sez=0.0, chi_z_68=1, chi_h_68=0.6594, err_val;
	long	orid, ndef, grn, srn, lincnt=1, week;
	char	seisregion[50], georegion[50], size[15];
	long	n, nrows;
	FILE	*fp, *qp, *bp, *ep;
	Tbl	*sortkeys;
	int	rc;

	strcpy( dbmapevents, "ak_dbmapevents" );


/* Parse command line */
       	if (argc < 3) {
	 usage();
	 exit (1);
	 }
/* get mandatory command line arguments */
	dbname = argv[1];
	minmag = atof(argv[2]);

/* Open database */
 
	 if ((rc = dbopen(dbname, "r+", &db)) == dbINVALID) {
	   clear_register (1);
	   fprintf (stderr, "db2monthly: Unable to open database '%s'\n", dbname);
	   exit (1);
	 }

/* generate monthly event listing */

	dbo = dblookup(db, 0, "origin", 0, 0);
	dbtemp = dblookup( db, 0, "origerr", 0, 0 );
	dberr = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
	dbquery(dberr,dbRECORD_COUNT, &nrows);
	if (nrows < 1) {
	  fprintf (stderr, "db2monthly: No events to process\n");
	  exit (1);
	}

/* sort by time */
        sortkeys = newtbl(1);
        pushtbl(sortkeys,"time");
        dberr = dbsort(dberr,sortkeys,0,0);
	dbquery(dberr,dbRECORD_COUNT, &nrows);
        freetbl(sortkeys, 0);

/* subset view by current minmag */
        sprintf(expr, "(ml >= \"%f\" || mb >= \"%f\")", minmag, minmag);
        dberr = dbsubset (dberr, expr, 0);
	dbquery(dberr,dbRECORD_COUNT, &nrows);

/* open listing output files */
	fp = fopen("monthly_list", "w");
	ep = fopen("db2monthly.err", "w");

	printf("\n*** Generating earthquake listings for database: %s\n\n", dbname);

	for( dberr.record = 0; dberr.record < nrows; dberr.record++ ) {

        	dbgetv( dberr, 0, "origin.lat", &lat,
			"origin.lon", &lon,
			"depth", &depth,
			"time", &time,
			"orid", &orid,
			"etype", &etype,
			"ndef", &ndef,
			"mb", &mb,
			"sdepth", &sdepth,
			"sminax", &sminax,
			"smajax", &smajax,
			"sdobs", &sdobs,
			"strike", &strike,
			"dtype", &dtype,
			"auth", &auth,
			"ml", &ml, NULL  );

/* compute SEH and SEZ
		SEH = smajax*(chi square value = 1.24) - (Natasha: wrong)
		SEZ = sdepth*(chi square value = 1.87) - (Natasha: wrong because sdepth is already SEZ)
		chi square = 1.87 (see HYPOELLIPSE by J.C. Lahr for details)
		Natasha: correct way to do it for 68.3% confidence level:
		(see K.Lindquist White Paper on Error Ellipses for AEIC Earthquake catalogs)
		SEZ = sdepth
		SEH = smajax*0.6594
		For 90% confidence level:
		SEZ = sdepth*.6080
		SEH = smajaz*.4660
*/

		seh = smajax * chi_h_68;
		sez = sdepth * chi_z_68;

/* determine Quality */
		if (seh >= sez)
		   err_val = seh;
		if (sez > seh)
		   err_val = sez;

		if (err_val <= 1.34)
		   strcpy(qual, "A");
		if (err_val > 1.34 && err_val <= 2.67)
		   strcpy(qual, "B");
		if (err_val > 2.67 && err_val <= 5.35)
		   strcpy(qual, "C");
		if (err_val > 5.35)
		   strcpy(qual, "D");

/* check if depth is fixed or free - map db chars to AEIC listing chars*/
		strcpy(free_dep, "-");
		if (!strcmp(dtype, free_dep))
		  strcpy(dtype, " ");
		if (!strcmp(dtype, "f"))
		  strcpy(dtype, " ");

		/* if depth fixed by analyst  */
		if (!strcmp(dtype, "g")) {
		  if (depth == 33.0) {
		    strcpy(dtype, "N");
		  } else {
		    strcpy(dtype, "G");
		  }
		}

		/* if depth fixed by hypoe program  */
		if (!strcmp(dtype, "r"))
		  strcpy(dtype, "D");

                sprintf(depth_str, "%4.1f%-1s", depth, dtype);

/* create time string info */
		strcpy (time_str, epoch2str ( time, "%y %m %e (%j) %T"));
		strcpy (year, epoch2str ( time, "%Y"));

/* figure out the magnitudes */

		if (mb != -999.00) {
		  sprintf(mag_str, "%4.1f \t", mb);
		} else if (ml != -999.00) {
		  sprintf(mag_str, "\t %4.1f ", ml);
		} else {
		  sprintf(mag_str, "\t N/A");
		}

/* determine closest station */
/* subset view by current orid*/

        	dba = dblookup( db, 0, "assoc", 0, 0 );
		sprintf(expr, "(orid == \"%d\")", orid);
		dbev = dbsubset (dba, expr, 0);
		dbquery( dbev, dbRECORD_COUNT, &n );

/* sort by delta */
		sortkeys = newtbl(1);
        	pushtbl(sortkeys,"delta");
        	dbev = dbsort(dbev,sortkeys,0,0);
		freetbl(sortkeys, 0);
		dbev.record = 0;
		dbgetv(dbev, 0, "delta", &min_delta, 
			"sta", &station,
			NULL);

/* check for null values */
		if (min_delta == -1.0) {
	  	printf("db2monthly error: no delta entry for orid %d station %4s \n", orid, station);
	  	fprintf(ep , "db2monthly error: no delta entry for orid %d station %4s\n", orid, station);
		}

		min_dist = min_delta * 111.12;

/* determine seismic gap */

/* sort by azimuth - esaz */
        	sortkeys = newtbl(1);
        	pushtbl(sortkeys,"esaz");
        	dbev = dbsort(dbev,sortkeys,0,0);
		freetbl(sortkeys, 0);

		largest_gap = 0;
		for( dbev.record = 0; dbev.record < n; dbev.record++ ) {

/* if first record*/
	   	if (dbev.record == 0) {
	     		dbgetv(dbev, 0, "esaz", &prev_esaz, NULL);
	     		first_gap = prev_esaz;
	   	}
	     	dbgetv(dbo, 0, "esaz", &esaz, 
				"sta", &station, NULL);
/* check for null values */
	     	if (esaz == -999.0) {
	        printf("db2monthly error: no esaz entry for orid %d station %4s\n", orid, station);
	        fprintf(ep , "db2monthly error: no esaz entry for orid %d station %4s\n", orid, station);
	     	}

	   	gap = esaz - prev_esaz;

	   	if (gap > largest_gap)
	      	largest_gap = gap;

/* special case when crossing 0 */
	   	if (dbev.record == n-1) {
			last_gap = 360 - esaz;
	        	gap = last_gap + first_gap;
			if (gap > largest_gap)
                   	largest_gap = gap;
	   	}

	   	prev_esaz = esaz;

		} /*end for dbev*/

		dbfree(dbev);
		
/* determine geo and seismo region */
		grn = grnumber(lat, lon);
		srn = srnumber(grn);
		grname(grn, georegion); 
		srname(srn, seisregion); 
	
/* now print out the tables */

		if (lincnt == 6) {
/*		  printf("\n");*/
		  fprintf(fp, "\n");
		  lincnt=1;
		}
		lincnt=lincnt+1;

		if(!strcmp(etype, "R")) {
		  fprintf (fp, "%27s \t %8.3f \t %8.3f \t %7s \t %6s \t \t \t \t \t \t \t \t %1s \t %s\n", time_str, lat, lon, depth_str, mag_str, etype, georegion); 
/*		  printf ("%27s \t %8.3f \t %8.3f \t %7s \t %6s \t \t \t \t \t \t \t \t %1s \t %s\n", time_str, lat, lon, depth_str, mag_str, etype, georegion); */
		} else {
		  fprintf (fp, "%27s \t %8.3f \t %8.3f \t %7s \t %6s \t %5.2f \t %5.2f \t %5.2f \t %6.1f \t %3d \t %7.2f \t %s \t %1s \t %s \n", time_str, lat, lon, depth_str, mag_str, sdobs, seh, sez, largest_gap, ndef, min_dist, qual, etype, georegion); 
/*		  printf ("%27s \t %8.3f \t %8.3f \t %7s \t %6s \t %5.2f \t %5.2f \t %5.2f \t %6.1f \t %3d \t %7.2f \t %s \t %1s \t %s \n", time_str, lat, lon, depth_str, mag_str, sdobs, seh, sez, largest_gap, ndef, min_dist, qual, etype, georegion); */
		}


} /* end for loop */

	dbfree(dberr);

	fclose(fp);
	fclose(ep);


/* make figure 1 for the monthly report */

/* set up dbmapevents parameter file */

	dbl = dblookup(db, 0, "origin", 0, 0);
	sortkeys = newtbl(1);
        pushtbl(sortkeys,"time");
        dbl = dbsort(dbl,sortkeys,0,0);
        freetbl(sortkeys, 0);
        dbquery(dbl,dbRECORD_COUNT, &nrows);
        if (nrows < 1) {
          fprintf (stderr, "db2monthly: No events to process\n");
          exit (1);
        }
/* get starting day */
	dbl.record=0;
	dbgetv( dbl, 0, "time", &time, NULL  );
	strcpy (start_day, epoch2str ( time, "%B %d"));
/* get ending day */
	dbl.record=nrows-1;
	dbgetv( dbl, 0, "time", &time, NULL  );
	strcpy (end_day, epoch2str ( time, "%B %d"));
	strcpy (end_jday, epoch2str ( time, "%j"));

	strcpy (year, epoch2str ( time, "%Y"));

	sprintf(title_expr, "AEIC Monthly Seismicity Report for %s - %s, %s", start_day, end_day, year);

	make_landscape_pf(title_expr);

	printf("\n*** Generating monthly report Figure 1\n");

	range = 10.0;
	latctr=61.0;
	lonctr=-159.0;
	sprintf(center, "%3.1f:%4.1f", latctr, lonctr);

/* fire up dbmapevents */
	 sprintf( map_command, "/usr/local/aeic/5.2-64/bin/ak_dbmapevents %s %s %f 2>&1 | grep -v origin 2>&1 | grep -v events", dbname, center, range );
	 fprintf(stderr,"Running: %s \n", map_command);
	 system( map_command );

/* move dbmapevents.ps fig1.ps with unix system command */
	 sprintf(mv_command, "/bin/mv ak_dbmapevents.ps monthly_fig1.ps");
	 system( mv_command );
	 sprintf(mv_command, "/bin/mv ak_dbmapevents.pf fig1_mapset_macro.pf");
	 system( mv_command );

/*NEW INSERT HIGHLIGHTS*/

/* make event highlight listings */
/* now grab the largest events in the origin table for catalog highlights */

	fp = fopen ("monthly_highlights", "w");

/* determine todays date */

	strcpy(today, epoch2str(now(), "%B %d, %Y"));
	strcpy(small_year, epoch2str(now(), "%y"));

	strcpy(year, epoch2str(now(), "%Y"));
	fprintf(fp,"The following is a list of significant earthquakes during the time period: %s to %s, %4s:  \n\n", start_day, end_day, year);
	printf("\n*** Generating Monthly Highlights\n\n");

/* find  mb >= minmag */
	sortkeys = newtbl(1);
	pushtbl(sortkeys,"mb");
	dbo = dbsort(dbo,sortkeys,0,0);
	freetbl(sortkeys, 0);
	dbquery(dbo,dbRECORD_COUNT, &nrows);
/* subset by minmag */
	sprintf(expr,"(mb >= \"%3.1f\")",minmag);
	dbl = dbsubset (dbo, expr, 0);
	dbquery(dbl,dbRECORD_COUNT, &nrows);
	if (nrows < 1) {
	  fprintf (stderr, "db2weekly: No NEIC mb events to process\n");
	} /* no NEIC to process */

	printf("\n largest NEIC mb\n\n");
	for( dbl.record = 0; dbl.record < nrows; dbl.record++) {
		dbgetv( dbl, 0, "origin.lat", &lat,
			"origin.lon", &lon,
			"depth", &depth,
			"time", &time,
			"orid", &orid,
			"mb", &mb,
			"ml", &ml, NULL  );

/* create time string */
		strcpy (time_str, epoch2str ( time, "%B %e at %T UTC"));
                strcpy (year, epoch2str ( time, "%Y"));
                strcpy (month, epoch2str ( time, "%B"));

                strcpy (ak_time_str, zepoch2str ( time, "%B %e, %T %Z", "US/Alaska"));
		if (mb < 10 && mb > -2) {

/* get nearest place information */
		   distance_report(lat, lon, distances);
/* determine geo and seismo region */
	           grn = grnumber(lat, lon);
        	   srn = srnumber(grn);
        	   grname(grn, georegion);
        	   srname(srn, seisregion);
/* determine event size description */
   		   if (mb >= 8.0)
                    sprintf(size, "great");
                   if (mb >= 7.0 && mb <= 7.9)
                    sprintf(size, "major");
                   if (mb >= 6.0 && mb <= 6.9)
                    sprintf(size, "strong");
                   if (mb >= 5.0 && mb <= 5.9)
                    sprintf(size, "moderate");
                   if (mb >= 4.0 && mb <= 4.9)
                    sprintf(size, "light");
                   if (mb >= 3.0 && mb < 3.9)
                    sprintf(size, "minor");
                   if (mb < 3.0)
                    sprintf(size, "very minor");

		   printf(" %s (%s), mb %3.1f, %5.2fN %5.2fW, depth=%3.1f km:\n", time_str, ak_time_str, mb, lat, lon, depth);
		   fprintf(fp," %s (%s), mb %3.1f, %5.2fN %5.2fW, depth=%3.1f km:\n", time_str, ak_time_str, mb, lat, lon, depth);
		   fprintf(fp,"   A %s earthquake was located by NEIC in %s.\n", size, georegion);
		   fprintf(fp," %s\n", distances);
		   

		}

	}  /* for loop */

	dbfree(dbl);

/* find  ml  > = minmag */
	sortkeys = newtbl(1);
	pushtbl(sortkeys,"ml");
	dbo = dbsort(dbo,sortkeys,0,0);
	freetbl(sortkeys, 0);
        dbquery(dbo,dbRECORD_COUNT, &nrows);
/* subset by minmag */
        sprintf(expr,"(ml >= \"%3.1f\")",minmag);
        dbl = dbsubset (dbo, expr, 0);
	dbquery(dbl,dbRECORD_COUNT, &nrows);
	if (nrows < 1) {
	  fprintf (stderr, "db2weekly: No AEIC ml events to process\n");
	  exit (1);
	}

	printf("\n largest AEIC ml\n\n");
	for( dbl.record = nrows-1; dbl.record >= 0; dbl.record--) {
		dbgetv( dbl, 0, "origin.lat", &lat,
			"origin.lon", &lon,
			"depth", &depth,
			"time", &time,
			"orid", &orid,
			"mb", &mb,
			"ml", &ml, NULL  );

/* create time string */
                strcpy (time_str, epoch2str ( time, "%B %e at %T UTC"));
                strcpy (year, epoch2str ( time, "%Y"));
                strcpy (month, epoch2str ( time, "%B"));

                strcpy (ak_time_str, zepoch2str ( time, "%B %e, %T %Z", "US/Alaska"));
		if (ml < 10 && ml > -2) {

/* get nearest place information */
		   distance_report(lat, lon, distances);
/* determine geo and seismo region */
                   grn = grnumber(lat, lon);
                   srn = srnumber(grn);
                   grname(grn, georegion);
                   srname(srn, seisregion);
/* determine event size description */
                   if (ml >= 8.0)
                    sprintf(size, "great");
                   if (ml >= 7.0 && ml <= 7.9)
                    sprintf(size, "major");
                   if (ml >= 6.0 && ml <= 6.9)
                    sprintf(size, "strong");
                   if (ml >= 5.0 && ml <= 5.9)
                    sprintf(size, "moderate");
                   if (ml >= 4.0 && ml <= 4.9)
                    sprintf(size, "light");
                   if (ml >= 3.0 && ml < 3.9)
                    sprintf(size, "minor");
                   if (ml < 3.0)
                    sprintf(size, "very minor");

		   printf(" %s (%s), Ml %3.1f, %5.2fN %5.2fW, depth=%3.1f km:\n", time_str, ak_time_str, ml, lat, lon, depth);
		   fprintf(fp," %s (%s), Ml %3.1f, %5.2fN %5.2fW, depth=%3.1f km:\n", time_str, ak_time_str, ml, lat, lon, depth);
		   fprintf(fp,"   A %s earthquake was located in %s.\n", size, georegion);
		   fprintf(fp," %s\n", distances);

		}

	} /* end for dbl */

	dbfree(dbl);

	fclose(fp);



/* report outfile to user */

        printf("\n\n*** db2monthly generated the following output files for database: %s\n\n", dbname);
	printf("     monthly_highlights -- suggested earthquakes to note in report\n");
        printf("     monthly_list -- listing of earthquakes > minmag\n");
        printf("     monthly_fig1.ps -- map of Alaska seismicity\n");


}  /* end Main */



usage()
{
	fprintf (stderr,"\nUSAGE: db2monthly dbin minimim_mag \n\n");
	fprintf (stderr,"where:\ndbin = input database\n");
	fprintf (stderr,"minimum_mag =  Minimum magnitude threshold for listing\n");
}

void make_landscape_pf(title_expr)
	char	*title_expr;
{
	FILE	*fp;

	fp = fopen ("ak_dbmapevents.pf", "w");

	fprintf( fp, "center 61:-159\n" );
	fprintf( fp, "range 10\n" );
	fprintf( fp, "istaplt 0\n" );
	fprintf( fp, "istnam 0\n" );
	fprintf( fp, "ipdeplt 0\n" );
	fprintf( fp, "ipdepth 4\n" );
	fprintf( fp, "iporid 0\n" );
	fprintf( fp, "icities 1\n" );
	fprintf( fp, "ipipe 1\n" );
	fprintf( fp, "iblue 1\n" );
	fprintf( fp, "ipmag 1\n" );
	fprintf( fp, "idcirc 0\n" );
	fprintf( fp, "ititl 1\n" );
	fprintf( fp, "iflt 1\n" );
	fprintf( fp, "ipumps 0\n" );
	fprintf( fp, "icol 1\n" );
	fprintf( fp, "itran 1\n" );
	fprintf( fp, "ilegnd 1\n" );
	fprintf( fp, "title %s\n", title_expr);
	fprintf( fp, "label_files  &Tbl{\n" );
	fprintf( fp, "country\n" );
	fprintf( fp, "new_faults\n" );
	fprintf( fp, "oceans\n" );
	fprintf( fp, "pipeline\n" );
	fprintf( fp, "volcanoes\n" );
	fprintf( fp, "}\n" );
	fprintf( fp, "boilerplate_files  &Tbl{\n" );
	fprintf( fp, "aeic\n" );
	fprintf( fp, "aeic_vol\n" );
	fprintf( fp, "}\n" );
			 
	fclose(fp);
}

/* distance_report
 *
 * K. Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1998
 */

static char *Placedb;
char *compass_from_azimuth( double );

void
distance_report( lat, lon, report )
double  lat;
double  lon;
char    *report;
{
        Dbptr   db;
        char    expr[STRSZ];
        char    place[STRSZ];
        Tbl     *expr_tbl;
        Expression *dist_expr;
        Expression *az_expr;
        long     nrecs;
        double  dist_km, dist_mi;
        double  azimuth;
        char    *compass;
	
	Placedb="/Seis/databases/places/cities";
        dbopen( Placedb, "r", &db );
        db = dblookup( db, 0, "places", 0, 0 );

        dbquery( db, dbRECORD_COUNT, &nrecs );

        sprintf( expr, "azimuth(lat,lon,%f,%f)", lat, lon );
        dbex_compile( db, expr, &az_expr, 0 );

        sprintf( expr, "distance(lat,lon,%f,%f)*111.195", lat, lon );
        dbex_compile( db, expr, &dist_expr, 0 );
        expr_tbl = strtbl( expr, 0 );

        db = dbsort( db, expr_tbl, 0, 0 );

	strcpy( report, "   This earthquake was:\n" );
        for( db.record = 0; db.record < 3; db.record++ ) {
                dbex_eval( db, dist_expr, 0, &dist_km );
                dist_mi = dist_km / 1.609;

                dbex_eval( db, az_expr, 0, &azimuth );
                compass = compass_from_azimuth( azimuth );
                
                dbgetv( db, 0, "place", place, NULL );
                sprintf( report, "%s\t%6.0f miles (%3.0f km) %3s of %s\n",
                                 report, dist_mi, dist_km, compass, place );
        }
        
        dbclose( db );
}



/*
 * K. Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1998
 */

char *
compass_from_azimuth( azimuth )
double	azimuth;
{
	while( azimuth < 0. ) azimuth += 360.;
	while( azimuth > 360. ) azimuth -= 360.;

	if( azimuth >= 348.75 || azimuth < 11.25 ) {

		return "N";		/*  0.00 */

	} else if( azimuth >= 11.25 && azimuth < 33.75 ) {

		return "NNE";		/*  22.50 */

	} else if( azimuth >= 33.75 && azimuth < 56.25 ) {

		return "NE";		/*  45.00	 */

	} else if( azimuth >= 56.25 && azimuth < 78.75 ) {

		return "ENE";		/*  67.50	 */

	} else if( azimuth >= 78.75 && azimuth < 101.25 ) {

		return "E";		/*  90.00	 */

	} else if( azimuth >= 101.25 && azimuth < 123.75 ) {

		return "ESE";		/*  112.50	 */

	} else if( azimuth >= 123.75 && azimuth < 146.25 ) {

		return "SE";		/*  135.00	 */

	} else if( azimuth >= 146.25 && azimuth < 168.75 ) {

		return "SSE";		/*  157.50	 */

	} else if( azimuth >= 168.75 && azimuth < 191.25 ) {

		return "S";		/*  180.00	 */

	} else if( azimuth >= 191.25 && azimuth < 213.75 ) {

		return "SSW";		/*  202.50	 */

	} else if( azimuth >= 213.75 && azimuth < 236.25 ) {

		return "SW";		/*  225.00 	 */

	} else if( azimuth >= 236.25 && azimuth < 258.75 ) {

		return "WSW";		/*  247.50	 */

	} else if( azimuth >= 258.75 && azimuth < 281.25 ) {

		return "W";		/*  270.00	 */

	} else if( azimuth >= 281.25 && azimuth < 303.75 ) {

		return "WNW";		/*  292.50	 */

	} else if( azimuth >= 303.75 && azimuth < 326.25 ) {

		return "NW";		/*  315.00	 */

	} else if( azimuth >= 326.25 && azimuth < 348.75 ) {

		return "NNW";		/*  337.50	 */
	} 

	return "Compass error";
}

/*
 * function to calculate the great circle distance and back azimuth (azimuth
 * clockwise from north between two points on a sphere with a flattening of
 * 1/295)
 *
 * returns the azimuth from point a to point b, measured from north
 * 
 * all IO is in degrees
 * 
 */

#include <math.h>

double          flat = 1.0000000;
double          rad;

#define cl 0			/* subscripts for sine and cosine arrays */
#define sl 1
#define cn 2
#define sn 3

double          cdif;


delta_az(alati, aloni, blati, bloni, delta, baz)
	double          *alati, *aloni, *blati, *bloni, *delta, *baz;
{
	double          a[4], b[4];
	double          alat, alon, blat, blon;

	rad = 3.14159265358979323846 / 180;

	alat = (double) *alati;
	alon = (double) *aloni;
	blat = (double) *blati;
	blon = (double) *bloni;

	dbpt(alat, alon, a);
	dbpt(blat, blon, b);

	cdif = b[cn] * a[cn] + b[sn] * a[sn];
	*delta = acos(a[sl] * b[sl] * cdif + a[cl] * b[cl]) / rad;

	*baz = atan2((a[cn] * b[sn] - b[cn] * a[sn]) * b[sl], a[sl] * b[cl] - a[cl] * cdif * b[sl]);
	*baz /= rad;
	if (*baz < 0)
		*baz += 360;

}

double          maxDegree = 360;

dbpt(lat, lon, a)
	double          lat, lon, a[4];
{
	a[cl] = 1;
	if (abs(lat != 90))
		a[cl] = sin(atan(flat * tan(lat * rad)));
	a[sl] = sqrt(1 - a[cl] * a[cl]);
	a[sn] = sin(lon * rad);

	cdif = fmod(lon, maxDegree);
	if (cdif < 0)
		cdif *= -1;

	a[cn] = sqrt(1 - a[sn] * a[sn]);
	if (cdif > 90 && cdif < 270)
		a[cn] *= -1;

}
