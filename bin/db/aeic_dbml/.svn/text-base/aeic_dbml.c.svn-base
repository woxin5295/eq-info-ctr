#include <stdio.h>
#include <unistd.h>
#include <libgen.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "tttaup.h"
#include "tr.h"
#include "pf.h"
#include "new_local_mag_util.h"
 
#define STA_SIZE 7 
#define CHAN_SIZE 9 

#define SPLICE_TOLERANCE 0.5

#define DIVERGENT_MAG 100

#define STREQ(a, b) (strcmp((a), (b)) == 0)

char	ProgName[STRSZ];

char    WoodAndersonResponseFile[FILENAME_MAX];
                                /* full pathname of file with wood-anderson
                                   response curve (format as specified
                                   for Datascope Seismic Application
                                   Package, q.v.) */
int BandpassFilterOrder;        /* order of bandpass filter for local mag */
double BandpassUpperCutoff;     /* upper cutoff (Hz) of bandpass filter */
double BandpassLowerCutoff;     /* lower cutoff (Hz) of bandpass filter */

extern int mydbgetwf( Dbptr, char *, char *, double, double, float **, long, 
	       double *, double *, double *, long *, double, int (*fill)(), void * );

void
usage()
{
	fprintf(stderr, "Usage: %s [-r] [-v] [-t] [-p pfname] database [orid]\n", 
    		ProgName);
	exit(1);
}


int main(argc, argv)
int	argc;
char	**argv;
{
	extern char    *optarg;
	extern int      optind;
	static char *magtype = "ml";
	static char *auth = "aeic_dbml";
	static char *filter = "none";
	static char *meastype = "WA_amp";
	static char *units1 = "sec";
	static char *units2 = "nm";
	struct stat statbuf;
	char	pfname[STRSZ];
	int	c;
	int	left;
	Dbptr	db, dbo, dbt, dbresp, dborids, dbbase;
	Pf	*pf;
	char	*dbname;
	Arr	*magstas;
	Arr	*clipcheck_immune;
	Tbl	*immune;
	int	verbose = 0 ;
	int	tk_display = 0;
	int	read_only = 0;
	long 	nrecords ;
	long	norids;
	long	test_norids;
	int	nsta;
	long	evid = -1;
	long	orid = -1;
	long	magid = -1;
	long	arid = -1;
	char	expr[STRSZ];
	char	*mag_network_name;
	char	tk_message[5*STRSZ];
	char	cmd[5*STRSZ];
	char	message[STRSZ];
	char	stamag_message[STRSZ];
	double	pre_p_sec;
	double	maxval;
	double	clip_thresh;
	double	spreading;
	double	atten;
	double	refdist;
	double	refmag;
	double	calfreq;
	double	calib;
	double	delta;
	double	dist_km;
	double	esaz;
	double	depth;
	double	quake_lat, quake_lon, quake_time;
	double	site_lat, site_lon;
	double	arrtime;
	double	reqs, reqe;
	double	ts, te;
	double	samprate;
	char	sta[STA_SIZE], chan[CHAN_SIZE];
	char	respfile[FILENAME_MAX];
	double	ml, mlunc;
	double	netmag, netmag_unc;
	double	tmeas, amp_delta;
	double 	ZtoP_amp_nm;
	Smrpt	*stamag;
	Tbl	*stamags;
	float	*data = NULL;
	long	nmax = 0;
	long	npts;
	long	nresps;
	int	rc;
	int	i;
	Tbl	*st;
	Tbl	*ans;
	Hook 	*hp = NULL;
	int	index;
	long	origin_writeable;
	long	netmag_writeable;
	long	stamag_writeable;
	long	wfmeas_writeable;
	char	rsptype[8];
 
	/* ProgName = basename( argv[0] ); */
	strcpy(ProgName, "aeic_dbml" );
	sprintf( pfname, "%s", ProgName );

	while ((c = getopt(argc, argv, "vrtp:")) != -1)
	{
        switch (c)
		{
		case 'v' :
	    		verbose = 1 ;
	    		break ;
		case 't' :
			tk_display = 1;
			break;
		case 'r' :
			read_only = 1;
			break;
		case 'p':
			sprintf( pfname, "%s", optarg );
			break;
        	default:
            		clear_register( 1 );
            		usage();
          	}
      	}
 
	left = argc - optind;
	if ( left < 1 )
	{
		usage() ; 
	}
	else if ( left == 1 )
	{
		dbname = argv[optind++] ;
	}
	else if ( left == 2 )
	{
		dbname = argv[optind++] ;
		orid = (long) atoi( argv[optind++] );
	}
	else 
	{
		usage() ; 
	}

	pfread( ProgName, &pf );

	pre_p_sec = pfget_double( pf, "pre_p_sec" );
	maxval = pfget_double( pf, "maxval" );
	clip_thresh  = pfget_double( pf, "clip_thresh" );
	spreading = pfget_double( pf, "spreading" );
	atten = pfget_double( pf, "atten" );
	refdist = pfget_double( pf, "refdist" );
	refmag = pfget_double( pf, "refmag" );
	calfreq = pfget_double( pf, "calfreq" );
	strcpy( WoodAndersonResponseFile, pfget_string( pf, "WoodAndersonResponseFile" ));
	if( ( rc = stat( WoodAndersonResponseFile, &statbuf ) ) != 0 ) {
		perror( "aeic_dbml:" );
		if( rc == ENOENT ) {
			die( 1,
			     "aeic_dbml: Couldn't find response file %s\n",
			     WoodAndersonResponseFile );
		} else {
			die( 1,
			     "aeic_dbml: Couldn't stat response file %s\n",
			     WoodAndersonResponseFile );
		}
	}
	BandpassFilterOrder = pfget_int( pf, "BandpassFilterOrder" );
	BandpassUpperCutoff = pfget_double( pf, "BandpassUpperCutoff" );
	BandpassLowerCutoff = pfget_double( pf, "BandpassLowerCutoff" );
	mag_network_name = pfget_string( pf, "mag_network_name" );
	immune = pfget_tbl( pf, "clipcheck_immune" );
	clipcheck_immune = newarr( 0 );
	for( i=0; i < maxtbl( immune ); i++ ) {
		setarr( clipcheck_immune, gettbl( immune, i ), (void *) TRUE );
	}

	if( dbopen( dbname, "r+", &db ) < 0 ) {
		die( 1, "Failed to open database\n" );
	}

	db = dblookup( db, 0, "affiliation", 0, 0 );

	sprintf( expr, "net == \"%s\"", mag_network_name );
	db = dbsubset( db, expr, 0 );

	dbquery( db, dbRECORD_COUNT, &nrecords );

	if( verbose ) fprintf( stderr,
			"aeic_dbml: We have %ld stations in the network \"%s\" for computing Ml\n", 
			nrecords, mag_network_name );

	magstas = newarr( 0 );
	for( db.record = (long) 0; (long) db.record < (long) nrecords; db.record = (long)db.record + 1 ) {
		dbgetv( db, 0, "sta", sta, NULL );
		setarr( magstas, sta, (void *) TRUE );
	}

	db = dblookup( db, 0, "assoc", 0, 0 );

	dbt = dblookup( db, 0, "arrival", 0, 0 );
	db = dbjoin( db, dbt, 0, 0, 0, 0, 0 );

	dbt = dblookup( db, 0, "site", 0, 0 );
	db = dbjoin( db, dbt, 0, 0, 0, 0, 0 );

/*	sprintf( expr, "site.offdate == NULL" );
	db = dbsubset( db, expr, 0 );*/

	dbt = dblookup( db, 0, "calibration", 0, 0 );
	db = dbjoin( db, dbt, 0, 0, 0, 0, 0 );

/*	sprintf( expr, "calibration.endtime == NULL && chan == calibration.chan" );
	db = dbsubset( db, expr, 0 );*/
	sprintf( expr, "chan == calibration.chan" );
	db = dbsubset( db, expr, 0 );

	dbbase = db;

	dborids = dblookup( db, 0, "origin", 0, 0 );

	if( orid > 0 ) {

		sprintf( expr, "orid == %ld", orid );

		dborids = dbsubset( dborids, expr, 0 );

		dbquery( dborids, dbRECORD_COUNT, &norids );

		if( (long) norids != (long) 1 ) {

			die( 1, "%ld rows with orid %ld. Bye!\n", norids, orid );

		}

	} else {

		dbquery( dborids, dbRECORD_COUNT, &norids );
	}

	for( dborids.record = 0; (long) dborids.record < (long) norids; dborids.record++ ) {

		dbgetv( dborids, 0, "orid", &orid, NULL );

		fprintf( stderr, "aeic_dbml: Processing orid %ld\n", orid );
		if( tk_display ) {
			sprintf( tk_message, "%ld ", orid );
		}

		sprintf( expr, "orid == %ld", orid );

		dbo = dbsubset( dborids, expr, 0 );

		dbquery( dbo, dbRECORD_COUNT, &test_norids );

		if( (long) test_norids != (long) 1 ) {

			die( 1, "%ld rows with orid %ld. Bye!\n", norids, orid );

		}

		dbt = dbjoin( dbbase, dbo, 0, 0, 0, 0, 0 );
		dbgetv( dbt, 0, "origin.time", &quake_time, NULL);
		
		sprintf( expr, "site.ondate <= origin.time && site.offdate >= origin.time || site.ondate <= origin.time && site.offdate == NULL" );
	  	dbt = dbsubset( dbt, expr, 0 );

		sprintf( expr, "calibration.time <= origin.time && calibration.endtime >= origin.time || calibration.time <= origin.time && calibration.endtime == NULL" );
	 	dbt = dbsubset( dbt, expr, 0 );
				
		sprintf( expr, "iphase == \"P\" || phase == \"P\"" );
		db = dbsubset( dbt, expr, 0 );

		dbquery( db, dbRECORD_COUNT, &nrecords );

		if( verbose ) fprintf( stderr,
				"aeic_dbml: looking at %ld records for orid %ld\n", nrecords, orid );
		stamags = newtbl( 0 );

		for( db.record = (long) 0; (long) db.record < (long) nrecords; db.record = (long)db.record + 1 ) {
			dbgetv( db, 0, "delta", &delta, 
			       "depth",  &depth, 
			       "origin.lat",  &quake_lat,
			       "origin.lon", &quake_lon,
			       "origin.time", &quake_time,
			       "site.lat", &site_lat,
			       "site.lon", &site_lon,
			       "calibration.calib", &calib,
			       "arrival.time", &arrtime,  
			       "arrival.sta", sta,
			       "arrival.chan", chan, 
			       "arid", &arid,
			       NULL );
			/* if( ! getarr( magstas, sta ) ) { */
			if( getarr( magstas, sta ) == NULL ) {
				if( verbose ) fprintf( stderr,
					"aeic_dbml: %s %s: not in \"%s\" network\n",
					sta, chan, mag_network_name );
				continue;
			}
			if( calib == 0. ) {
				if( verbose ) fprintf( stderr,
					"aeic_dbml: %s %s: Null calib \n", sta, chan );
				continue;
			}


			if( delta >= 8.0 ) {
				if( verbose ) fprintf( stderr,
					"aeic_dbml: %s %s: Large distance %8.2f km, no mag \n", sta, chan, deg2km(delta) );
				continue;
			}

			dbresp = dblookup( db, 0, "instrument", 0, 0 );
			dbt = dblookup( db, 0, "sensor", 0, 0 );
			dbresp = dbjoin( dbresp, dbt, 0, 0, 0, 0, 0 );
			sprintf( expr, "sta == \"%s\" && chan == \"%s\"", sta, chan );
			dbresp = dbsubset( dbresp, expr, 0 );
			dbresp = dbjoin( dbresp, dbo, 0, 0, 0, 0, 0 );
			sprintf( expr,
				"sensor.time <= origin.time && sensor.endtime >= origin.time ||	sensor.time <= origin.time && sensor.endtime == NULL" );
			dbresp = dbsubset( dbresp, expr, 0 );
			dbquery( dbresp, dbRECORD_COUNT, &nresps );
			dbresp.record = 0;
			if( (long) nresps != (long) 1 ) {
				if( verbose ) fprintf( stderr,
					"aeic_dbml: %s %s: Can't find response file entry in database\n",
					sta, chan );
				continue;
			} else if( dbfilename( dbresp, respfile ) <= 0 ){
				if( verbose ) fprintf( stderr,
						"aeic_dbml: %s %s: Can't find response file\n",
						sta, chan );
				continue;
			}
			rsptype[0] = '\0';
			dbgetv( dbresp, 0, "rsptype", &rsptype, NULL);
			
			dist( rad( quake_lat ), rad( quake_lon ),
		      		rad( site_lat ), rad( site_lon ), 
		      		&delta, &esaz );
		
			delta = deg( delta );
			esaz = deg( esaz );

			dist_km = deg2km( delta );
			/* Use hypocentral distance: */
			dist_km = sqrt( dist_km * dist_km + depth * depth );

			reqs = arrtime - pre_p_sec;
			reqe = arrtime - ptime( delta, depth ) + 2 * stime( delta, depth );
			rc = mydbgetwf( db, sta, chan, reqs, reqe,
					&data, nmax, &ts, &te, &samprate, &npts,
					SPLICE_TOLERANCE, 0, 0);

			if( rc != 0 ) continue;

			if( (long) npts > (long) nmax ) nmax = npts;

			/* if( clip_check( data, npts, maxval, clip_thresh ) &&
		    		! getarr( clipcheck_immune, sta ) ) { */
			if( clip_check( data, npts, maxval, clip_thresh ) &&
		    		getarr( clipcheck_immune, sta ) == NULL ) {
				if( verbose ) fprintf( stderr, "aeic_dbml: %s %s: deemed clipped\n",
						sta, chan );
				free( data );
				data = NULL;
				nmax = 0;
				continue;
			}
	
                	rc = measure_local_mag( data, ts, npts, samprate,
                                	calib, calfreq, respfile,
                                	arrtime, dist_km, spreading, atten, refdist,
                                	refmag, &ml, &mlunc, &tmeas, &amp_delta,
                                	&ZtoP_amp_nm, rsptype );
			
			if( fabs( ml ) > DIVERGENT_MAG ) {
				if( verbose ) fprintf( stderr, "aeic_dbml: %s %s: Diverged\n",
						sta, chan );
				free( data );
				data = NULL;
				nmax = 0;
				continue;
			}

			fprintf( stderr, "aeic_dbml: %s %s: stamag Ml %4.2f %f %f %f\n",
					sta, chan, ml, tmeas, amp_delta, ZtoP_amp_nm );
			sprintf( message, "aeic_dbml: %s %s: stamag Ml %4.2f\\n",
					sta, chan, ml );
			sprintf( stamag_message, " %s_%s:%4.2f ", sta, chan, ml );


			if( tk_display ) {
				strcat( tk_message, stamag_message );
			}

			free( data );
			data = NULL;
			nmax = 0;

			stamag = (Smrpt *) malloc( sizeof( Smrpt ) );
 
			strcpy( stamag->sta, sta );
			strcpy( stamag->chan, chan );
			stamag->arid = arid;
			stamag->time = ts;
			stamag->endtime = ENDTIME( ts, samprate, npts );
			stamag->tmeas = tmeas;
			stamag->PtoP_timediff = amp_delta;
			stamag->ZtoP_amp_nm = ZtoP_amp_nm;
			stamag->stamag = ml;
			stamag->uncert = mlunc;
 
			pushtbl( stamags, stamag );
		}

		if( tk_display ) {
			Pf	*omit_pf;
			Tbl	*omit_tbl;
			int	itbl;
			char	*omit_sta;
			char	*omit_chan;
			Tbl	*omit_element;
			int	istamag; 

			sprintf( cmd, 
				"tkedit_ml '%s'",
				 tk_message );

			system( cmd );

			pfread( "tkedit_ml", &omit_pf );
			omit_tbl = pfget_tbl( omit_pf, "omit" );

			for( itbl = maxtbl( omit_tbl )-1; itbl >= 0; itbl-- ) {

				omit_element = split( gettbl( omit_tbl, itbl ), '_' );
				omit_sta = gettbl( omit_element, 0 );
				omit_chan = gettbl( omit_element, 1 );

				printf( "aeic_dbml: Omitting %s %s\n", omit_sta, omit_chan );

				for( istamag = 0; istamag < maxtbl( stamags ); istamag++ ) {
					stamag = gettbl( stamags, istamag );

					if( STREQ( stamag->sta, omit_sta ) && STREQ( stamag->chan, omit_chan ) ) {

						deltbl( stamags, istamag );
						break;
					}
				}
			}

			unlink( "tkedit_ml.pf" );
		}

		stamags_to_netmag( stamags, &netmag, &netmag_unc );

		if( netmag == -999. ) {
			fprintf( stderr,
				"\naeic_dbml: No network magnitude for orid %ld.\n\n", orid );
			sprintf( message, 
				"\\naeic_dbml: No network magnitude for orid %ld.\\n\\n",
				orid );
		} else {
			fprintf( stderr,
				"\naeic_dbml: Network magnitude %4.2f Ml for orid %ld.\n\n",
				netmag, orid );
			sprintf( message, 
				"\\naeic_dbml: Network magnitude %4.2f Ml for orid %ld.\\n\\n",
				netmag, orid );
			
		}

		db = dblookup( db, 0, "origin", 0, "dbALL" );
	
		dbquery( db, dbTABLE_IS_WRITEABLE, &origin_writeable );

		if( netmag == -999. ) {

			/* do nothing */

		} else if( ! (long) read_only && (long) origin_writeable ) {

			dbt = db;
			dbt.record = dbSCRATCH;

			dbputv( dbt, "origin", "orid", orid, NULL );

			st = strtbl( "orid", 0 );

			dbmatches( dbt, db, &st, &st, &hp, &ans );

			db.record = (long) gettbl( ans, 0 );

			freetbl( st, 0 );
			freetbl( ans, 0 );

			dbgetv( db, "origin", "evid", &evid, NULL );

			magid = dbnextid( db, "magid" );

			dbputv( db, "origin", "ml", netmag, "mlid", magid, NULL);

		} else if( ! read_only ) {

			complain( 1, "origin table not writeable!\n" );
		}

		db = dblookup( db, 0, "netmag", 0, "dbALL" );

		dbquery( db, dbTABLE_IS_WRITEABLE, &netmag_writeable );

		if( netmag == -999. ) {
		 
			 /* do nothing */
		  
		} else if( ! (long) read_only && (long) netmag_writeable ) {

			db.record = (long) dbaddnull( db );

			dbputv( db, 0, "magid", magid,
			       		"orid", orid,
			       		"evid", evid,
			       		"magtype", magtype,
			       		"nsta", maxtbl( stamags ),
			       		"magnitude", netmag,
			       		"uncertainty", netmag_unc,
			       		"auth", auth,
			       		NULL );

		} else if( ! read_only ) {
	
			complain( 1, "netmag table not writeable!\n" );

		}

		db = dblookup( db, 0, "stamag", 0, "dbALL" );

		dbquery( db, dbTABLE_IS_WRITEABLE, &stamag_writeable );

		if( netmag == -999. ) {
		 
			  /* do nothing */
			   
		} else if( ! (long) read_only && (long) stamag_writeable ) {

			nsta = maxtbl( stamags );

			for( index = 0; index < nsta; index++ ) {

				stamag = gettbl( stamags, index );

				db.record = (long) dbaddnull( db );

				dbputv( db, 0, "magid", magid,
				       		"sta", stamag->sta,
				       		"arid", stamag->arid,
				       		"orid", orid,
				       		"evid", evid,
				       		"magtype", magtype,
				       		"magnitude", stamag->stamag,
				       		"uncertainty", stamag->uncert,
				       		"auth", auth,
				       		NULL );
			}


		} else if( ! read_only ) {

			complain( 1, "stamag table not writeable!\n" );

		}
	
		db = dblookup( db, 0, "wfmeas", 0, "dbALL" );

		dbquery( db, dbTABLE_IS_WRITEABLE, &wfmeas_writeable );

		if( netmag == -999. ) {
	
			/* do nothing */

		} else if( ! (long) read_only && (long) wfmeas_writeable  ) {

			nsta = maxtbl( stamags );

			for( index = 0; index < nsta; index++ ) {

				stamag = gettbl( stamags, index );

				db.record = (long) dbaddnull( db );

				dbputv( db, 0, "sta", stamag->sta,
					       "chan", stamag->chan,
					       "arid", stamag->arid,
					       "meastype", meastype,
					       "filter", filter,
					       "time", stamag->time,
					       "endtime", stamag->endtime,
					       "tmeas", stamag->tmeas,
					       "val1", stamag->PtoP_timediff,
					       "val2", stamag->ZtoP_amp_nm,
					       "units1", units1,
					       "units2", units2,
					       "auth", auth,
					       "twin", (double) 0.00,
				       		NULL );
			}


		} else if( ! read_only ) {

			complain( 1, "wfmeas table not writeable!\n" );

		}
	}

	return 0;
}
