#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include "db.h"
#include "stock.h"
#include "coords.h"

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

void usage()
{
	fprintf (stderr,"\nUSAGE: dberrchk dbin \n\n");
	fprintf (stderr,"where:\ndbin = input database\n");
}

main( int argc, char **argv )

{
	char	*dbname;
	char	dtype[3], etype[3], time_str[50];
	char	depth_str[10], mag_str[10], qual[5];
	char	expr[100], algo[20], auth[20];
	char	free_dep[1], iphase[5];
	char	year[4], assoc_station[5], station[5];
	Dbptr 	db, dbtemp, dbo, dba, dbarr, dbsm, dbsarr, dbparr, dberr, dbev;
	double  min_delta, min_dist, delta;
	double	prev_esaz, esaz, largest_gap, gap, first_gap, last_gap;
	double  slat, slon, arc_dist, az;
	double	lat, lon, depth, time, mb, ml, min_mag, max_mag;
	double	stamag, magres;
	double  prev_time, timegap, ttres;
	double	sdepth, sminax, smajax, sdobs, strike;
	double	seh=0.0, sez=0.0, chi=0.6594, err_val;
	long	assoc_orid, orid, ndef, grn, srn, prev_orid;
	char	seisregion[50], georegion[50];
	long	narr, np, ns, n, nass, nsm, table_writeable;
	long nrows;
	FILE	  *ep;
	Tbl	*sortkeys, *joinkeys, *joinkeys2;
	int	rc;


/* Parse command line */
       	if (argc < 2) {
	 usage();
	 exit (1);
	 }
/* get mandatory command line arguments */
	dbname = argv[1];

/* Open database */
 
	 if ((rc = dbopen(dbname, "r+", &db)) == dbINVALID) {
	   clear_register (1);
	   fprintf (stderr, "dberrchk: Unable to open database '%s'\n", dbname);
	   exit (1);
	 }

/* loop through origins */

	dbo = dblookup(db, 0, "origin", 0, 0);
	dbtemp = dblookup( db, 0, "origerr", 0, 0 );
	dberr = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
	dbquery(dberr,dbRECORD_COUNT, &nrows);
/* sort by time */
        sortkeys = newtbl(1);
        pushtbl(sortkeys,"time");
        dberr = dbsort(dberr,sortkeys,0,0);
        freetbl(sortkeys, 0);
	if ((long) nrows < (long) 1) {
	  fprintf (stderr, "dberrchk: No events to process\n");
	  exit (1);
	}

/* open output files */
	ep = fopen("dberrchk.err", "w");

	printf("\n*** Searching for event errors: %s\n\n", dbname);
	fprintf(ep, "\n*** event errors for database: %s\n\n", dbname);

	printf("\n------------------------------------------------------------------------------------------------");
	printf("\nOrigin Time                    lat.    lon.      depth(km)   mb   Ml   SDOBS  qual  georegion");
	printf("\n------------------------------------------------------------------------------------------------");
	fprintf(ep,"\n------------------------------------------------------------------------------------------------");
	fprintf(ep,"\nOrigin Time                    lat.    lon.      depth(km)   mb   Ml   SDOBS  qual  georegion");
	fprintf(ep,"\n------------------------------------------------------------------------------------------------");

	for( dberr.record = 0; (long) dberr.record < (long) nrows; dberr.record++ ) {
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
			"algorithm", &algo,
			"ml", &ml, NULL  );

 	if (orid > 0) {
 		printf ("\n"); 
		fprintf (ep,"\n"); 
		printf("\n*** Report for orid: %ld using %s by %s", orid, algo, auth);
		fprintf(ep,"\n*** Report for orid: %ld using %s by %s", orid, algo, auth);

/* compute SEH and SEZ
		SEH = smajax*(chi square value)
		SEZ = sdepth*(chi square value)
		chi square = 1.87
		see HYPOELLIPSE by J.C. Lahr for details
		Natasha: Wrong for CSS3.0 database. See K.Lindquist White Paper #2001-001.
		Correctway to do it for 68.3 confidence level:
		seh = smajax * 0.6594;
		sez = sdepth;
*/
		seh = smajax * chi;
		sez = sdepth;

/* determine Quality */
		if (seh >= sez)
		   err_val = seh;
		if (sez > seh)
		   err_val = sez;

		if (err_val <= 1.34)
		   strcpy(qual, "A");
		if (err_val > 1.34 && err_val <= 2.67)
		   strcpy(qual, "B");
		if (err_val > 2.67 && err_val <= 5.35){
		   strcpy(qual, "C");
/*		   printf("\n    Low Quality: %s, SMAJAX = %3.2f, SMINAX = %3.2f, SDEPTH = %3.2f", qual,smajax, sminax, sdepth);
		   fprintf(ep,"\n    Low Quality: %s, SMAJAX = %3.2f, SMINAX = %3.2f, SDEPTH = %3.2f", qual,smajax, sminax, sdepth);*/
		}
		if (err_val > 5.35) {
		   strcpy(qual, "D");
/*		   printf("\n    Low Quality: %s, SMAJAX = %3.2f, SMINAX = %3.2f, SDEPTH = %3.2f", qual,smajax, sminax, sdepth);
		   fprintf(ep,"\n    Low Quality: %s, SMAJAX = %3.2f, SMINAX = %3.2f, SDEPTH = %3.2f", qual,smajax, sminax, sdepth);*/
		}

/*		if (smajax >= 20)
		   printf("\n    ERROR: Low location quality: %s, SMAJAX = %3.2f, SDEPTH = %3.2f", qual, smajax, sdepth);
		   fprintf(ep,"\n    ERROR:     Low location quality: %s, SMAJAX = %3.2f, SDEPTH = %3.2f", qual, smajax, sdepth);
*/

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
		strcpy (time_str, epoch2str ( time, "%Y %m %e (%j) %T"));
		strcpy (year, epoch2str ( time, "%Y"));

/* determine geo and seismo region */
		grn = grnumber(lat, lon);
		srn = srnumber(grn);
		grname(grn, georegion); 
		srname(srn, seisregion); 
	
/* figure out the magnitudes */

		if (mb != -999.00) {
		  sprintf(mag_str, "%4.1f \t", mb);
		} else if (ml != -999.00) {
		  sprintf(mag_str, "\t %4.1f ", ml);
		} else {
		  sprintf(mag_str, "\t N/A");
		}

/* now print out the tables */

		if(!strcmp(etype, "R")) {
		  printf ("\n%29s  %8.3f  %8.3f  %7s  %6s         %1s  %s", time_str, lat, lon, depth_str, mag_str, etype, georegion); 
		  fprintf (ep,"\n%29s  %8.3f  %8.3f  %7s  %6s         %1s  %s", time_str, lat, lon, depth_str, mag_str, etype, georegion); 
		} else {
		  printf ("\n%29s  %8.3f  %8.3f  %7s  %6s  %5.2f  %1s  %1s  %s ", time_str, lat, lon, depth_str, mag_str, sdobs, qual, etype, georegion); 
		  fprintf (ep,"\n%29s  %8.3f  %8.3f  %7s  %6s  %5.2f  %1s  %1s  %s ", time_str, lat, lon, depth_str, mag_str, sdobs, qual, etype, georegion); 
		}

/* check for duplicate event */
		if (dberr.record == 0) {
			prev_time = time;
			prev_orid = orid;
		} else {
			timegap=time-prev_time;
			if (timegap < 30) {
			  printf("\n    WARNING: Possible duplicate event:  %ld", orid);
			  fprintf(ep,"\n    WARNING: Possible duplicate event: %ld", orid);
			
			}

			if (prev_orid == orid) {
			  printf("\n    ERROR: Multiple orids in origin table: %ld", orid);
			  fprintf(ep,"\n    ERROR: Multiple orids in origin table: %ld", orid);
			}
			prev_time = time;
			prev_orid = orid;
		}


/* check sdobs */
		if (sdobs > 2.0) {
		  printf("\n    ERROR: Large SDOBS: %3.2f", sdobs);
		  fprintf(ep,"\n    ERROR: Large SDOBS: %3.2f", sdobs);
		}
		
/* check smajax and sdepth */		
		if (smajax >= 20 || sdepth >= 30) {
		  printf("\n    ERROR: Low location quality: %s, SMAJAX = %3.2f, SDEPTH = %3.2f", qual, smajax, sdepth);
		  fprintf(ep,"\n    ERROR: Low location quality: %s, SMAJAX = %3.2f, SDEPTH = %3.2f", qual, smajax, sdepth);
		}

/* check for deep events outside WBZ */
 
        	if (depth > 40) {
          	if (lat > 64) {
            		printf ("\n    ERROR: Deep event north of 64N latitude ");
            		fprintf (ep,"\n    ERROR: Deep event north of 64N latitude ");
	  	}
          	else {
          	if (lat > 60 && lon < -154 && lon < 0) {
            		printf ("\n    ERROR: Deep event west of 154W longitude ");
            		fprintf (ep,"\n    ERROR: Deep event west of 154W longitude ");
	  	}
	  	}
          	if (lon > -146 && lon < 0) {
            		printf ("\n    ERROR: Deep event east of 146W longitude ");
            		fprintf (ep,"\n    ERROR: Deep event east of 146W longitude ");
	  	}
          	else {
          	if (lat < 60 && lon > -150 && lon < 0) {
            		printf ("\n    ERROR: Deep event south of 60N latitude and east of 150W longitude");
            		fprintf (ep,"\n    ERROR: Deep event south of 60N latitude and east of 150W longitude ");
	  	}
          	else {
          	if (lat < 58 && lon > -152 && lon < 0) {
            		printf ("\n    ERROR: Deep event south of 58N latitude and east of 152W longitude ");
            		fprintf (ep,"\n    ERROR: Deep event south of 58N latitude and east of 152W longitude ");
	  	}
	  	}
	  	}
        	}

/* check for negative and below 220 km depths */
 
        	if (depth > 220) {
            		printf ("\n    WARNING: Deep event : %5.3f km", depth);
            		fprintf (ep,"\n    WARNING: Deep event : %5.3f km", depth);
		}
        	if (depth < -3) {
            		printf ("\n    ERROR: Negative depth : %5.3f km", depth);
            		fprintf (ep,"\n    ERROR: Negative depth : %5.3f km", depth);
		}

/* determine closest station */

/* subset view by current orid*/
		dba = dblookup( db, 0, "assoc", 0, 0 );
		sprintf(expr, "(orid == \"%ld\")", orid);
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
		while (min_delta == -1.0) {
	  		printf("\ndberrchk error: no delta entry for orid %ld station %4s ", orid, station);
	  		fprintf(ep , "\ndberrchk error: no delta entry for orid %ld station %4s", orid, station);

/* fix delta entry in assoc table, need to join site */
	  		printf("\nComputing delta for orid: %ld station %s", orid, station);
	  		fprintf(ep,"\nComputing delta for orid: %ld station: %s", orid, station);
          		dbtemp = dblookup( db, 0, "site", 0, 0 );
	  		dbquery( dbtemp, dbRECORD_COUNT, &n );
	  		sprintf(expr, "(sta == \"%s\")", station);
	  		dbtemp = dbsubset (dbtemp, expr, 0);
	  		dbquery( dbtemp, dbRECORD_COUNT, &n );
	  		joinkeys = strtbl("sta", 0);
          		joinkeys2 = strtbl("sta", 0);
          		dbo = dbjoin( dbev, dbtemp, &joinkeys,&joinkeys2,0,0,0 );
          		dbfree(dbtemp);
	  		dbquery( dbo, dbRECORD_COUNT, &n );
           		dbtemp = dblookup( db, 0, "origin", 0, 0 );
           		dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
       	  		dbquery (dbo, dbRECORD_COUNT, &n);	  		
             		if (n < 1) {
                		fprintf (stderr, "\ndberrchk: No station to process.");
            		 } else {
				dbo.record = 0;
				min_dist = get_distance(dbo);
				delta = min_dist/111.195;
	     		}

/* write new delta assoc tables */
	     		printf("\nwriting delta to assoc table");
             		db = dblookup( db, 0, "assoc", 0, 0);
             		dbquery (db, dbRECORD_COUNT, &n);

/* loop through assoc table and look for missing delta */
             		for( db.record = 0; db.record < n; db.record++ ) {
                		dbgetv(db, 0, "orid", &assoc_orid, "sta", &assoc_station, NULL);
                		if (assoc_orid == orid && !strcmp(assoc_station, station)) {
                   		dbquery( db, dbTABLE_IS_WRITEABLE, &table_writeable );
                  		if(table_writeable) {
                          		dbputv (db,"assoc", "delta", delta, NULL);
                          		clear_register (1);
                  		} else {
                          		fprintf( stderr, "assoc table not writeable \n");
                  		}
                 		}
             		} /* end for n */
             		
/* resort on delta to find min_delta */
/* sort by delta */
			dba = dblookup( db, 0, "assoc", 0, 0 );			
			sprintf(expr, "(orid == \"%ld\")", orid);
			dbev = dbsubset (dba, expr, 0);			
             		sortkeys = newtbl(1);
             		pushtbl(sortkeys,"delta");
             		dbev = dbsort(dbev,sortkeys,0,0);
             		freetbl(sortkeys, 0);
             		dbev.record = 0;
             		dbgetv(dbev, 0, "delta", &min_delta,
                        	"sta", &station, NULL);
/*                		fprintf (stderr, "\nmin_delta after resorting %8.3f at station %s", min_delta, station);*/
			
/*		dbfree(dbtemp);*/
/*		dbfree(dbev);*/
/*		dbfree(dbo);*/
		} /* end while min_delta = -1 */

		min_dist = min_delta * 111.195;
/*		printf ("\n  Closest station %s is at %8.2f km", station, min_dist); 
		fprintf (ep,"\n  Closest station %s is at %8.2f km", station, min_dist); */
		

/* check assoc table for missing esaz values  for given orid */

		dba = dblookup( db, 0, "assoc", 0, 0 );			
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbev = dbsubset (dba, expr, 0);			
        	dbquery( dbev, dbRECORD_COUNT, &nass );
		for( dbev.record = 0; dbev.record < nass; dbev.record++ ) {
	   		dbgetv(dbev, 0, "esaz", &esaz, 
				"sta", &station, NULL);
/* check for null values */
	     		if (esaz == -999.0) {
	        		printf("\ndberrchk error: no esaz entry for orid %ld station %4s", orid, station);
	        		fprintf(ep , "\ndberrchk error: no esaz entry for orid %ld station %4s", orid, station);
/* find station lat and lon */
                		dbtemp = dblookup( db, 0, "site", 0, 0 );
                		sprintf(expr,"((sta == \"%s\"))",station);
                		dbtemp = dbsubset (dbtemp, expr, 0);
                		dbquery (dbtemp, dbRECORD_COUNT, &n);
                		dbtemp.record=0;
                		dbgetv (dbtemp, 0, "lat", &slat,
                        		"lon", &slon, NULL);
				dbfree(dbtemp);
 
				printf("\nComputing esaz for orid: %ld station: %s", orid, station);
				fprintf(ep,"\nComputing esaz for orid: %ld station: %s", orid, station);

                		delta_az(&lat, &lon, &slat, &slon, &arc_dist, &az);
                		
/* write new esaz assoc tables */
                		db = dblookup( db, 0, "assoc", 0, 0);
                		dbquery (db, dbRECORD_COUNT, &n);
                		
/* loop through assoc talbe and look for missing esaz */
                		for( db.record = 0; db.record < n; db.record++ ) {
                   			dbgetv(db, 0, "orid", &assoc_orid, "sta", &assoc_station, NULL);
                   			if (assoc_orid == orid && !strcmp(assoc_station, station)) {
                      				dbquery( db, dbTABLE_IS_WRITEABLE, &table_writeable );
                     				if(table_writeable) {
                             				dbputv (db,"assoc", "esaz", az, NULL);
                             				clear_register (1);
                     				} else {
                             				fprintf( stderr, "assoc table not writeable \n");
                     				}
                    			}
                 		} /* end for n */
	     		}  /* if esaz */
		} /* for dbo */
	
/*		dbfree(dbev);*/
/*		dbfree(dbtemp);*/

/* determine station gap */

		dba = dblookup( db, 0, "assoc", 0, 0 );			
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbev = dbsubset (dba, expr, 0);			
        	dbquery( dbev, dbRECORD_COUNT, &n );
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
	     		dbgetv(dbev, 0, "esaz", &esaz, 
				"sta", &station, NULL);

/* check for null values */
	     		if (esaz == -999.0) {
	        		printf("\ndberrchk error: no esaz entry for orid %ld station %4s", orid, station);
	        		fprintf(ep , "\ndberrchk error: no esaz entry for orid %ld station %4s", orid, station);
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

		} /* end for loop */

		if (largest_gap > 180 && lat >= 59 && lat <= 66 && lon <= -140 && lon >= -155) {
	  		printf("\n    ERROR: Large station gap: %5.2f", largest_gap);
	  		fprintf(ep,"\n    ERROR: Large station gap: %5.2f", largest_gap);
		}

	
/*removed search for stations to fill the station gap*/

/* check number of P and S arrivals */

		dba = dblookup( db, 0, "assoc", 0, 0 );			
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbev = dbsubset (dba, expr, 0);			
		dbtemp = dblookup( db, 0, "arrival", 0, 0 );
        	dbarr = dbjoin( dbev, dbtemp, 0,0,0,0,0 );
        	dbquery (dbarr, dbRECORD_COUNT, &narr);

/* subset for P arrivals */
		sprintf(iphase,"P");
        	sprintf(expr,"(iphase == \"%s\")",iphase);
        	dbparr = dbsubset (dbarr, expr, 0);
        	dbquery (dbparr, dbRECORD_COUNT, &np);
          	if (np < 5) {
            		printf ("\n    ERROR: Number of P arrivals is less than 5: only %ld picks", np);
            		fprintf (ep,"\n    ERROR: Number of P arrivals is less than 5: only %ld picks", np);
          	}

/* subset for S arrivals */
		sprintf(iphase,"S");
        	sprintf(expr,"(iphase == \"%s\")",iphase);
        	dbsarr = dbsubset (dbarr, expr, 0);
        	dbquery (dbsarr, dbRECORD_COUNT, &ns);
        	
          	if (ns < 3) {
            		printf ("\n    WARNING: Number of S arrivals is less than 3: only %ld picks", ns);
            		fprintf (ep,"\n    WARNING: Number of S arrivals is less than 3: only %ld picks", ns);
          	}
          	
/*         	dbfree(dbarr);*/

/* check for multiple station arrivals removed*/

/* figure out the magnitudes */

		if (mb != -999.00) {
		  sprintf(mag_str, "%4.1f \t", mb);
		} else if (ml != -999.00) {
		  sprintf(mag_str, "\t %4.1f ", ml);
		} else {
		  sprintf(mag_str, "\t N/A");
		}

/* Check magnitudes */
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
        		
          		if ((max_mag-min_mag) > 2.0) {
            			printf("\n    WARNING: Large magnitude range: %5.3f out of %ld magnitude readings", (max_mag-min_mag), nsm);
            			fprintf(ep,"\n    WARNING: Large magnitude range: %5.3f out of %ld magnitude readings", (max_mag-min_mag), nsm);
          		} 

/* Look for large station mag residuals */
              		for (dbsm.record = 0; dbsm.record < nsm; dbsm.record++) {

                		dbgetv (dbsm, 0, "magnitude", &stamag, "sta", &station, NULL);
                		magres=ml-stamag;
                		
                		if (magres > 1.0 || magres < -1.0) {
                  			printf("\n    ERROR: Large magnitude residual at station %s : %5.3f out of %ld magnitude readings", station, magres, nsm);
                  			fprintf(ep,"\n    ERROR: Large magnitude residual at station %s : %5.3f out of %ld magnitude readings", station, magres, nsm);
                		}
                        }

           	} else {
                	printf ("\n    WARNING: No station magnitudes for orid %ld in '%s'", orid, dbname);
                	fprintf (ep, "\n    WARNING: No station magnitudes for orid %ld in '%s'", orid, dbname);
           	} /* end if nsm */
           	
/*            	dbfree(dbsm);*/

/* Find large P residuals */
        	dbquery (dbparr, dbRECORD_COUNT, &np);
		for( dbparr.record = 0; dbparr.record < np; dbparr.record++ ) {
          		dbgetv(dbparr, 0, "sta", &station, "timeres", &ttres, "delta", &delta, NULL);
            		if (ttres > 3.0) {
              			printf("\n    ERROR: Large P-time residual: %5.3f at station %s, at distance %5.1f km out of %ld P-phases", ttres, station, delta*111.195, np);
              			fprintf(ep,"\n    ERROR: Large P-time residual: %5.3f at station %s, at distance %5.1f km out of %ld P-phases", ttres, station, delta*111.195, np);
            		} 
         	}

/*          	dbfree(dbparr);*/

/* Find large S residuals */
        	dbquery (dbsarr, dbRECORD_COUNT, &ns);
		for( dbsarr.record = 0; dbsarr.record < ns; dbsarr.record++ ) {
          		dbgetv(dbsarr, 0, "sta", &station, "timeres", &ttres, "delta", &delta, NULL);
            		if (ttres > 4.0) {
              			printf("\n    ERROR: Large S-time residual: %5.3f at station %s, at distance %5.1f km out of %ld S-phases", ttres, station, delta*111.195, ns);
              			fprintf(ep,"\n    ERROR: Large S-time residual: %5.3f at station %s, at distance %5.1f km out of %ld S-phases", ttres, station, delta*111.195, ns);
            		} 
         	}

         	dbfree(dbsarr);
         	dbfree(dbparr);
         	dbfree(dbarr);
          	dbfree(dbev);
         	dbfree(dbsm);
/*       	dbfree(dbtemp);*/
/*       	dbfree(dbo);*/

}
} /* end for origin loop */


	fclose(ep);

	printf("\n\nFinished checking database: %s\n\n", dbname);


}  /* end Main */





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
