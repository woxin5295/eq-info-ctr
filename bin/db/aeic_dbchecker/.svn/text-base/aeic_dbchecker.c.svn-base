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

double
get_distance( Dbptr db )
{
        Expression *ex;
        double  dist_km;

        dbex_compile( db,
                      "111.195*distance(site.lat,site.lon,origin.lat,origin.lon)",
                      &ex,
                      dbREAL );
        dbex_eval( db, ex, 0, &dist_km );
        dbex_free( ex );

        return dist_km;

}


main( int argc, char **argv )

{
	char	*dbname, phase[5];
	char    distances[10000];
	char    magtype[3];
	char	dtype[3], etype[3], ak_time_str[50], time_str[50], date_str[50];
	char 	start_day[50], end_day[50], today[50];
	char	depth_str[10], mag_str[10], qual[5];
	char	title_expr[100], center[20], expr[100], tempdb[50], auth[20];
	char	dbmapevents[STRSZ], free_dep[1], iphase[5];
	char	month[25], year[4], close_station[5], station[5];
	char	map_command[50], mv_command[50], rm_command[50];
	Dbptr 	dbsrt, dbs, dbsm, dbl, db, dbout, dbtemp, dbo, dba, dbt;
	Dbptr	dbp, dbz, dbx, dbparr, dbsarr;
	double  range, latctr, lonctr, min_delta, min_dist, dist, delta;
	double  slat, slon, magres, sum_sqr, var, stdev, min_mag, max_mag;
	double	sighi, siglow;
	double  ttres, sum_ttres, sum_ttsqr, ave_ttres;
	double	prev_esaz, esaz, largest_gap;
	double	newgap1, newgap2, newgap, gap, first_gap, last_gap;
	double	arc_dist, az;
	double  startgap, endgap, centergap;
	double	lat, lon, depth, aktime, time, mb, ml, stamag, mag;
	double  elat, elon, edep;
	double	sdepth, sminax, smajax, sdobs, strike;
	double	seh=0.0, sez=0.0, chi=0.6594, err_val;
	long	orid, ndef, grn, srn, lincnt=1, jday, week;
	char	seisregion[50], georegion[50], size[15];
	long	n, nsm, na, np, ns, nrows, nsta, nres, gap_over_zero=0;
	FILE	*fp, *qp, *bp, *ep, *mp, *tp;
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
	orid = atoi(argv[2]);

/* Open database */
 
	 if ((rc = dbopen(dbname, "r+", &db)) == dbINVALID) {
	   clear_register (1);
	   fprintf (stderr, "dbstats: Unable to open database '%s'\n", dbname);
	   exit (1);
	 }

/* open origin table and get event info */

	dbo = dblookup(db, 0, "origin", 0, 0);
        dbquery( dbo, dbRECORD_COUNT, &n );
	sprintf(expr, "(orid == \"%ld\")", orid);
	dbo = dbsubset (dbo, expr, 0);
        dbquery( dbo, dbRECORD_COUNT, &n );
	if (n < 1) {
                fprintf (stderr, "dbchecker: No orids to process.\n");
                exit (1);
        }
	dbtemp = dblookup( db, 0, "origerr", 0, 0 );
        dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
        dbquery( dbo, dbRECORD_COUNT, &n );
	dbo.record = 0;
	dbgetv(dbo, 0, "lat", &elat,
                        "lon", &elon,
                        "depth", &edep,
                        "time", &time,
                        "ndef", &ndef,
                        "ml", &ml,
                        "sdobs", &sdobs,
                        "sdepth", &sdepth,
                        "smajax", &smajax,
                        NULL);

/* create time string */
                strcpy (time_str, epoch2str ( time, "%Y %m %e (%j) %T"));

/* determine geo and seismo region */
        grn = grnumber(elat, elon);
        srn = srnumber(grn);
        grname(grn, georegion);
        srname(srn, seisregion);

/* report event information */

	printf("\n\n Database: %s     orid: %ld\n\n", dbname, orid);
	printf("Origin Time                   lat     lon      depth  ml\n");
	printf("----------------------------------------------------------\n");
	printf("%s %5.3f %5.3f %5.2f   %4.2f \n\n", time_str, elat, elon, edep, ml);
	printf("\nGeo Region: %s, %s\n", georegion, seisregion);


/* determine seismic gap */
        dbo = dblookup(db, 0, "assoc", 0, 0);
/* subset view by current orid*/
        sprintf(expr, "(orid == \"%ld\")", orid);
        dbo = dbsubset (dbo, expr, 0);
        dbquery( dbo, dbRECORD_COUNT, &n );
/* sort by azimuth - esaz */
        sortkeys = newtbl(1);
        pushtbl(sortkeys,"esaz");
        dbo = dbsort(dbo,sortkeys,0,0);
        freetbl(sortkeys, 0);

        largest_gap = 0;
        for( dbo.record = 0; dbo.record < n; dbo.record++ ) {

        /* if first record*/
           if (dbo.record == 0) {
             dbgetv(dbo, 0, "esaz", &prev_esaz, NULL);
             first_gap = prev_esaz;
           }
             dbgetv(dbo, 0, "esaz", &esaz,
                                "sta", &station, NULL);
/* check for null values */
             if (esaz == -999.0) {
                printf("dbchecker error: no esaz entry for orid %ld station %4s\n",
orid, station);
                fprintf(ep , "dbchecker error: no esaz entry for orid %ld station %4s\n", orid, station);
             }

           gap = esaz - prev_esaz;

           if (gap > largest_gap) {
              largest_gap = gap;
	      startgap = prev_esaz;
	      endgap = esaz;
	      centergap = startgap + gap/2.0;
	   }

/* special case when crossing 0 */
           if (dbo.record == n-1) {
                last_gap = 360 - esaz;
                gap = last_gap + first_gap;
                if (gap > largest_gap){
		   gap_over_zero = 1;
                   largest_gap = gap;
		   startgap = esaz;
		   endgap = first_gap;
		   centergap = startgap + gap/2.0;
		   if (centergap > 360)
		     centergap = centergap - 360;
		}
           }

           prev_esaz = esaz;

        }


	printf("\nLocation Statistics:");
	printf("\nSEH: %5.2fkm SEZ: %5.2fkm SDOBS: %5.2f GAP: %5.2f\n", smajax*chi, sdepth, sdobs, largest_gap);

/* check for deep events outside WBZ */
 
        if (edep > 40) {
          if (elat > 64) {
            printf ("\n    Deep event north of 64N latitude ");
	  }
          else {
          if (elat > 60 && elon < -154) {
            printf ("\n    Deep event west of 154W longitude ");
	  }
	  }
          if (elon > -146) {
            printf ("\n    Deep event east of 146W longitude ");
	  }
          else {
          if (elat < 60 && elon > -150) {
            printf ("\n    Deep event south of 60N latitude and east of 150W longitude");
	  }
          else {
          if (elat < 58 && elon > -152) {
            printf ("\n    Deep event south of 58N latitude and east of 152W longitude ");
	  }
	  }
	  }
        }

/* check for negative and below 220 km depths */
 
        if (edep > 220) {
            printf ("\n    Deep event : %5.3f km", depth);
	}
        if (edep < -3) {
            printf ("\n    Negative depth : %5.3f km", depth);
	}

/* check sdobs */
		if (sdobs > 1.0) {
		  printf("\n    Large SDOBS: %3.2f", sdobs);
		}

/* check smajax and sdepth */
		if (smajax >= 20.0 || sdepth >= 30) {
		  printf("\n    Large locaton error: SMAJAX = %5.1f, SDEPTH = %5.1f\n", smajax, sdepth);
		}

/* check for multiple station arrivals */

 
        dba = dblookup(db, 0, "origin", 0, 0);
/* subset origin for orid */
        sprintf(expr,"(orid == \"%ld\")",orid);
        dba = dbsubset (dba, expr, 0);
	dbtemp = dblookup( db, 0, "assoc", 0, 0 );
        dba = dbjoin( dba, dbtemp, 0,0,0,0,0 );
	dbtemp = dblookup( db, 0, "arrival", 0, 0 );
        dba = dbjoin( dba, dbtemp, 0,0,0,0,0 );
        dbquery (dba, dbRECORD_COUNT, &na);

        dbs = dblookup(db, 0, "site", 0, 0);
        dbquery (dbs, dbRECORD_COUNT, &nrows);

	for( dbs.record = 0; dbs.record < nrows; dbs.record++ ) {
	  dbgetv(dbs, 0, "sta", &station, NULL);

/* subset arrival for orid */
          dbquery (dba, dbRECORD_COUNT, &na);
          sprintf(expr,"(sta == \"%s\")",station);
          dbt = dbsubset (dba, expr, 0);
          dbquery (dbt, dbRECORD_COUNT, &n);

	  if (n > 1) {
/* subset for P arrivals */
	    sprintf(iphase,"P");
            sprintf(expr,"(iphase == \"%s\")",iphase);
            dbp = dbsubset (dbt, expr, 0);
            dbquery (dbp, dbRECORD_COUNT, &np);
	    if (np > 1)
	     printf("\nStation %s P arrival picked on multiple components", station);

/* subset for s arrivals */
	    sprintf(iphase,"S");
            sprintf(expr,"(iphase == \"%s\")",iphase);
            dbz = dbsubset (dbt, expr, 0);
            dbquery (dbz, dbRECORD_COUNT, &ns);
	    if (ns > 1)
	     printf("\nStation %s S arrival picked on multiple components", station);
	  }


	}


/* check number of P and S arrivals */
        printf ("\nChecking number of P and S picks and large travel time residuals...\n");
       dba = dblookup(db, 0, "origin", 0, 0);
/* subset origin for orid */
        sprintf(expr,"(orid == \"%ld\")",orid);
        dba = dbsubset (dba, expr, 0);
	dbtemp = dblookup( db, 0, "assoc", 0, 0 );
        dba = dbjoin( dba, dbtemp, 0,0,0,0,0 );
	dbtemp = dblookup( db, 0, "arrival", 0, 0 );
        dba = dbjoin( dba, dbtemp, 0,0,0,0,0 );
/* subset for P arrivals */
	sprintf(iphase,"P");
        sprintf(expr,"(iphase == \"%s\")",iphase);
        dbparr = dbsubset (dba, expr, 0);
        dbquery (dbparr, dbRECORD_COUNT, &np);
          if (np < 10) {
            printf ("\n    Number of P arrivals is less than 10: only %d picks", np);
          }
/* subset for S arrivals */
	sprintf(iphase,"S");
        sprintf(expr,"(iphase == \"%s\")",iphase);
        dbsarr = dbsubset (dba, expr, 0);
        dbquery (dbsarr, dbRECORD_COUNT, &ns);
          if (ns < 3) {
            printf ("\n    Number of S arrivals is less than 3: only %d picks", ns);
          }

/* Find large P residuals */
        dbquery (dbparr, dbRECORD_COUNT, &np);
	for( dbparr.record = 0; dbparr.record < np; dbparr.record++ ) {
          dbgetv(dbparr, 0, "sta", &station, "timeres", &ttres, "delta", &delta, NULL);
            if (delta < 1 && ttres > 0.75) {
              printf("\n        Large P-time residual: %5.3f at station %s, at distance %5.1f km", ttres, station, delta*111.195);
            } 
            if (delta < 2 && delta >= 1 && ttres > 1) {
              printf("\n        Large P-time residual: %5.3f at station %s, at distance %5.1f km", ttres, station, delta*111.195);
            }
            if (delta < 3 && delta >= 2 && ttres > 1.5) {
              printf("\n        Large P-time residual: %5.3f at station %s, at distance %5.1f km", ttres, station, delta*111.195);
            }
            if (delta >= 3 && ttres > 2) {
              printf("\n        Large P-time residual: %5.3f at station %s, at distance %5.1f km", ttres, station, delta*111.195);
            }
        }

/* Find large S residuals */
        dbquery (dbsarr, dbRECORD_COUNT, &ns);
	for( dbsarr.record = 0; dbsarr.record < ns; dbsarr.record++ ) {
          dbgetv(dbsarr, 0, "sta", &station, "timeres", &ttres, "delta", &delta, NULL);
            if (delta < 1 && ttres > 1) {
              printf("\n        Large S-time residual: %5.3f at station %s, at distance %5.1f km", ttres, station, delta*111.195);
            } 
            if (delta < 2 && delta >= 1 && ttres > 1.5) {
              printf("\n        Large S-time residual: %5.3f at station %s, at distance %5.1f km", ttres, station, delta*111.195);
            } 
            if (delta < 3 && delta >= 2 && ttres > 2) {
              printf("\n        Large S-time residual: %5.3f at station: %s, at distance %5.1f km", ttres, station, delta*111.195);
            } 
            if (delta >= 3 && ttres > 3) {
              printf("\n        Large S-time residual: %5.3f at station %s, at distance %5.1f km", ttres), station, delta*111.195;
            } 
        }

/* Look for large station mags */
	   printf("\n\nChecking station magnitude residuals:");
           dbsm = dblookup(db, 0, "stamag", 0, 0);
           dbquery (dbsm, dbRECORD_COUNT, &nsm);
/* subset stamag for orid */
           sprintf(expr,"(orid == \"%ld\")",orid);
           dbsm = dbsubset (dbsm, expr, 0);
           dbquery (dbsm, dbRECORD_COUNT, &nsm);
           if (nsm > 0) {
/* Find magnitude range */
/* sort by magnitude */
        sortkeys = newtbl(1);
        pushtbl(sortkeys,"magnitude");
        dbsm = dbsort(dbsm,sortkeys,0,0);
        freetbl(sortkeys, 0);
        dbsm.record = 0;
        dbgetv(dbsm, 0, "magnitude", &min_mag, NULL);
        dbsm.record = nsm-1;        
        dbgetv(dbsm, 0, "magnitude", &max_mag, NULL);
          if ((max_mag-min_mag) > 1.5) {
            printf("\n    Large magnitude range: %5.3f", (max_mag-min_mag));
          } 

              for (dbsm.record = 0; dbsm.record < nsm; dbsm.record++) {

                dbgetv (dbsm, 0, "magnitude", &stamag, "sta", &station, NULL);
                magres=ml-stamag;
                if (magres > 0.75 || magres < -0.75) {
                  printf("\n      Large magnitude residual at station: %s : %5.3f", station, magres);
                }
              }
           } else {
                fprintf (stderr, "dbchecker: No station magnitudes for orid %ld in '%s'\n", orid, dbname);
           } /* end if nsm */


/* determine closest station */
 
        dbo = dblookup(db, 0, "origin", 0, 0);
        dbtemp = dblookup( db, 0, "assoc", 0, 0 );
        dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
/* subset view by current orid*/
        sprintf(expr, "(orid == \"%ld\")", orid);
        dbo = dbsubset (dbo, expr, 0);
        dbquery( dbo, dbRECORD_COUNT, &n );
/* sort by delta */
        sortkeys = newtbl(1);
        pushtbl(sortkeys,"delta");
        dbo = dbsort(dbo,sortkeys,0,0);
        freetbl(sortkeys, 0);
        dbo.record = 0;
        dbgetv(dbo, 0, "delta", &min_delta,
                        "sta", &close_station,
                        NULL);
/* check for null values */
        if (min_delta == -1.0) {
          printf("dbchecker error: no delta entry for orid %ld station %4s \n", orid, station);
        }
        min_dist = min_delta * 111.195;

	printf("\n\nClosest Station: %s at %5.3f km", close_station, min_dist);

/* find closer stations */

	printf("\n checking for close stations (<50km) not picked.....");
	dbo = dblookup( db, 0, "arrival", 0, 0 );
        dbtemp = dblookup( db, 0, "assoc", 0, 0 );
        dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
/* subset view by current orid*/
        sprintf(expr, "(orid == \"%ld\")", orid);
        dbo = dbsubset (dbo, expr, 0);
        dbquery (dbo, dbRECORD_COUNT, &n);

	dbs = dbsubset( dbs, "offdate == NULL", 0 );
	dbs.record = 0;
        dbquery (dbs, dbRECORD_COUNT, &n);

        dbx = dbnojoin( dbs, dbo, 0,0,0 );
        dbquery (dbx, dbRECORD_COUNT, &n);

	for (dbx.record = 0; dbx.record < n; dbx.record++) {

	  dbgetv(dbx, 0, "lat", &slat,
                                "lon", &slon,
                                "sta", &station, NULL);
          delta_az(&elat, &elon, &slat, &slon, &arc_dist, &az);

	  dist = arc_dist * 111.195;

	  if (dist < 50 ){
	    printf("\n        Station: %s at %5.3f km not picked", station, dist);
	  }
	}

	printf("\n\nSeismic gap of %5.2f deg centered at azimuth %5.2f:", largest_gap, centergap);


/* loop through site table and suggest stations to fill gap */

	printf("\n checking for stations to fill gap.....");
        dbo = dblookup( db, 0, "arrival", 0, 0 );
        dbtemp = dblookup( db, 0, "assoc", 0, 0 );
        dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
/* subset view by current orid*/
        sprintf(expr, "(orid == \"%ld\")", orid);
        dbo = dbsubset (dbo, expr, 0);
        dbquery (dbo, dbRECORD_COUNT, &n);

	dbs = dbsubset( dbs, "offdate == NULL", 0 );
        dbs.record = 0;
        dbquery (dbs, dbRECORD_COUNT, &n);

        dbx = dbnojoin( dbs, dbo, 0,0,0 );
        dbquery (dbx, dbRECORD_COUNT, &n);

        for (dbx.record = 0; dbx.record < n; dbx.record++) {

          dbgetv(dbx, 0, "lat", &slat,
                                "lon", &slon,
                                "sta", &station, NULL);
          delta_az(&elat, &elon, &slat, &slon, &arc_dist, &az);

	      if (arc_dist < 4) {

/* scan wfdisc for station */
          dbtemp = dblookup( db, 0, "wfdisc", 0, 0 );
          sprintf(expr, "(sta == \"%s\")", station);
          dbtemp = dbsubset (dbtemp, expr, 0);
          dbquery (dbtemp, dbRECORD_COUNT, &nsta);

	  if (nsta > 0) {
/* check for null values */
	    if (gap_over_zero == 1) {
	      if (az > startgap || az < endgap) {
		newgap1=az-startgap;
		if (newgap1 < 0)
		   newgap1=360.0+newgap1;
		newgap2=endgap-az;
		if (newgap2 < 0)
		   newgap2=360.0+newgap2;
		if (newgap1 > newgap2) {
		   newgap = newgap1;
		} else {
		   newgap = newgap2;
		}

	        printf("\nStation %s at azimuth %5.1f and distance %5.1f km will reduce gap to %5.1f", station, az, arc_dist*111.195, newgap);
	      }

	    } else {

	      if (az > startgap && az < endgap) {
		newgap1=az-startgap;
		newgap2=endgap-az;
		if (newgap1 > newgap2) {
		   newgap = newgap1;
		} else {
		   newgap = newgap2;
		}
	        printf("\nStation %s at azimuth %5.1f and distance %5.1f km will reduce gap to %5.1f", station, az, arc_dist*111.195, newgap);
	      }
	    }
	   }
	  }

	}

	printf("\n\n  Leaving dbchecker\n\n");


} /* end main */

int usage()
{
        fprintf(stderr,"Usage:  \n dbchecker dbin orid\n\n");
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
 
#define cl 0                    /* subscripts for sine and cosine arrays */
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
 
        *baz = atan2((a[cn] * b[sn] - b[cn] * a[sn]) * b[sl], a[sl] * b[cl] - a[cl] * cdif
* b[sl]);
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


