#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "win.h"
#include "tr.h"

#define FINAL 1
#define COMPRESS_INTERVAL 3000

int	Verbose = 0;
int	Fake_names = 0;
int	Timecorr = 0;

static void
usage ()
{
    char usagestr[STRSZ];
    
    sprintf( usagestr,
		"Usage: %s [-v] [-f] [-s site_dbname] winfile database \n",
		Program_Name);
    fprintf (stderr, usagestr);
    cbanner ( "$Revision: 1.2 $ $Date: 2002-02-07 01:45:01 $\n", usagestr, 
		"Kent Lindquist", "Geophysical Institute", "kent@giseis.alaska.edu" );
    exit (1);
}

static void 
handle_block( int length, unsigned char *block, Dbptr *tr, char *segtype )
{
	unsigned char *block_p = block;
	double 	starttime;
	double 	starttime_block;
	char	stacode[STRSZ];
	int	nsamp;
	double	samprate;
	int	*data;
	Trsample *trdata;
	int	i;
	Sinfo 	*sinfo;
	Sinfo	sinfo_fake;

	if( tr->database == dbINVALID ) {
		*tr = trnew( 0, 0 );
	} 

	strcpy( sinfo_fake.net, "XX" );
	strcpy( sinfo_fake.chan, "XXX" );
	sinfo_fake.calib = 0;
	sinfo_fake.calper = -1;
	sinfo_fake.commdelay = 0;

	block_p = strip_time( block_p, &starttime_block );

	if( Verbose ) fprintf( stderr, 
			"Processing packet at %s\n",
			strtime( starttime_block ) );

	do {
		block_p = strip_next_channel( block_p, stacode, &nsamp, &data ) ;
		allot( Trsample *, trdata, nsamp );
		for( i = 0; i < nsamp; i++ ) {
			trdata[i] = (Trsample) data[i];
		}
		free( data );

		if( nsamp == 0 ) {
			complain( 1, "zero samples \n" );
			return;
		}

		if( Fake_names ) {
			strcpy( sinfo_fake.sta, &stacode[2] );
			sinfo = &sinfo_fake;
		} else {
			sinfo = retrieve_station_info( stacode );
			if( sinfo == NULL ) {
				complain( 1, 
					"No site info for stacode %s\n",
					stacode );
				free( trdata );
				return;
			}	
		}

		if( Timecorr ) {
			starttime = starttime_block - sinfo->commdelay;
		} else {
			starttime = starttime_block;
		}
		dbaddv( *tr, "trace", 
			"net", sinfo->net, 
			"sta", sinfo->sta, 
			"chan", sinfo->chan,
			"time", starttime, 
			"nsamp", nsamp, 
			"samprate", (double) nsamp, 
			"endtime", ENDTIME( starttime, (double) nsamp, nsamp ),
			"segtype", segtype,
			"calib", sinfo->calib,
			"calper", sinfo->calper,
			"data", trdata, 
			NULL );
		clear_register( 1 );

	} while( block_p < block + length );
}

void
trrow_transfer( Dbptr trin, Dbptr trout )
{
	char	net[STRSZ];
	char	sta[STRSZ];
	char	chan[STRSZ];
	double	starttime;
	int	nsamp;
	double	samprate;
	double	endtime;
	char	segtype[10];
	double	calib;
	double	calper;
	Trsample *trdata;

	dbgetv( trin, 0,
		"net", net, 
		"sta", sta, 
		"chan", chan,
		"time", &starttime, 
		"nsamp", &nsamp, 
		"samprate", &samprate,
		"endtime", &endtime,
		"segtype", segtype,
		"calib", &calib,
		"calper", &calper,
		"data", &trdata, 
		NULL );

	dbaddv( trout, 0,
		"net", net, 
		"sta", sta, 
		"chan", chan,
		"time", starttime, 
		"nsamp", nsamp, 
		"samprate", samprate,
		"endtime", endtime,
		"segtype", segtype,
		"calib", calib,
		"calper", calper,
		"data", trdata, 
		NULL );
}

Dbptr
condense_traces( Dbptr tr, int final ) 
{
	static int last = 2 * COMPRESS_INTERVAL;
	long	nrecs;
	Dbptr	trreturn;
	Tbl	*keys;

	tr = dblookup( tr, 0, "trace", 0, 0 );
	dbquery( tr, dbRECORD_COUNT, &nrecs );

	if( ( ! final ) && ( (long)nrecs < (long)last ) ) {
		return tr;
	}

	if( Verbose ) fprintf( stderr, "Condensing traces...\n" );

	keys = strtbl( "sta", "chan", "time", 0 );
	tr = dbsort( tr, keys, 0, 0 );
	freetbl( keys, 0 );

	if( Verbose ) fprintf( stderr, "Sorting packets...\n" );

	trreturn = trnew( 0, 0 );
	trreturn = dblookup( trreturn, 0, "trace", 0, 0 );
	for( tr.record = 0; (long)tr.record < (long)nrecs; tr.record++ ) {
		trrow_transfer( tr, trreturn );
	}
	
	if( Verbose ) fprintf( stderr, "Splicing packets...\n" );

	trsplice( trreturn, trTOLERANCE, 0, 0 );
	clear_register( 1 );

	dbquery( trreturn, dbRECORD_COUNT, &nrecs );
	if( (long) nrecs >= (long) last ) {
		last += COMPRESS_INTERVAL;
	}

	return trreturn;
}

int
main (int argc, char **argv)
{
    	int	c;
	int	errflg = 0;
	char	*database;
	char	*winfile;
	char	*datatype;
	char	*segtype;
	char	*wfname;
	Pf 	*pf;
	FILE	*fp;
	int	onesec_block_size;
	unsigned char *onesec_block;
	unsigned char *onesec_block_p;
	char	*datatype_default = "s4";
	char	*segtype_default = "V";
	Dbptr	db;
	Dbptr	tr;
	long	nrecs;
	struct stat statbuf;
	double	fraction_complete = 0.1;
	double	size_done;
	double	total_size;
	char	site_dbname[FILENAME_MAX] = "";
	char	*str;

	elog_init ( argc, argv ) ; 

	if (pfread (Program_Name, &pf) != 0) {
		die (0, "Can't read parameter file\n");
   	} else {
		datatype = pfget_string( pf, "datatype" );
		if( datatype == NULL ) {
			datatype = datatype_default;
		}

		segtype = pfget_string( pf, "segtype" );
		if( segtype == NULL ) {
			segtype = segtype_default;
		}

		wfname = pfget_string( pf, "wfname" );

		Timecorr = pfget_boolean( pf, "timecorr" );
	}

	while ((c = getopt (argc, argv, "s:fvV")) != -1) {
		switch (c) {

		case 'f':
			Fake_names++;
			break;
		case 's':
			strcpy( site_dbname, optarg );
			break;
		case 'v':
	    		Verbose++;
	    		break;
		default:
	    		errflg++;
	    		break ;
		}
	}

	if ( errflg ) usage ();

	if( argc - optind != 2 ) usage();

	if( Fake_names ) {

		/* Do nothing with site database */

	} else if( strcmp( site_dbname, "" ) ) {

		if( Verbose ) fprintf( stderr, 
			"Using site database %s\n", site_dbname );

		load_station_info( site_dbname, segtype );

	} else {
        	if( ( str = getenv( "SITE_DB" ) ) == NULL ) {
                	die( 1, 
		"Environment variable SITE_DB must be defined, or use -f or -s\n" );
        	} else {
                	strcpy( site_dbname, str );
        	}

		if( Verbose ) fprintf( stderr, 
			"Using site database %s\n", site_dbname );

		load_station_info( site_dbname, segtype );
	}

	winfile = argv[optind++];
	database = argv[optind++];

	if( dbopen( database, "r+", &db ) < 0 ) {
		die( 1, "Failed to open database %s for writing\n", database );
	} else {
		db = dblookup( db, 0, "wfdisc", 0, 0 );
	}

	tr = dbinvalid();

	if( ( fp = fopen( winfile, "r" ) ) == NULL ) {

		die( 1, "Failed to open %s\n", winfile );

	} else if( Verbose ) {

		fprintf( stderr, "Opened %s\n", winfile );

		stat( winfile, &statbuf );
		total_size = (double) statbuf.st_size;
	}

	while( fread( &onesec_block_size, 1, 4, fp ) ) {

		onesec_block_p =
	  	onesec_block = (unsigned char *)
	        	malloc( onesec_block_size * sizeof( char ) );

		fread( &onesec_block[0], 1, onesec_block_size - 4, fp );

		handle_block( onesec_block_size - 4,
			      onesec_block,
			      &tr, segtype );

		tr = condense_traces( tr, ! FINAL );

		free( onesec_block );

		size_done = (double) ftello( fp );
		if( size_done / total_size >= fraction_complete ) {
			if( Verbose ) {
				fprintf( stderr, 
					"%02f%% Complete\n",
					fraction_complete * 100 );
			}
			fraction_complete += 0.1;
		}
	}

	tr = condense_traces( tr, FINAL );

	if( Verbose ) fprintf( stderr, "Saving to new database...\n" );

	trsave_wf( tr, db, datatype, wfname, trAPPEND );

	return 0;
}
