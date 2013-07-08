#include <stdlib.h>
#include <stdio.h>
#include "db.h"
#include "stock.h"
#include "win.h"

Arr *Site_info = NULL;

void 
load_station_info( char *site_dbname, char *segtype ) 
{
	Dbptr	db;
	Dbptr	dbt;
	Sinfo 	*sinfo;
	char	stacode[STRSZ];
	char	wincode[STRSZ];
	int	present;
	int	nrecs; 

	Site_info = newarr( 0 );

	if( dbopen( site_dbname, "r", &db ) < 0 ) {
		die( 1, "Failed to open %s. Bye.\n", site_dbname );
	}

	db = dblookup( db, 0, "win", 0, 0 );
	clear_register( 1 );
	dbquery( db, dbTABLE_PRESENT, &present );
	if( ! present ) {
		die( 1, "win table is not present in %s. Bye.\n", site_dbname );
	}

	dbt = dblookup( db, 0, "calibration", 0, 0 );
	clear_register( 1 );
	dbquery( dbt, dbTABLE_PRESENT, &present );
	if( ! present ) {
		die( 1, "calibration table is not present in %s. Bye.\n", site_dbname );
	} else {
		db = dbjoin( db, dbt, 0, 0, 0, 0, 0 );
		clear_register( 1 );
	}

	dbt = dblookup( db, 0, "timecorr", 0, 0 );
	clear_register( 1 );
	db = dbjoin( db, dbt, 0, 0, OUTER_JOIN, 0, 0 );
	clear_register( 1 );

	dbquery( db, dbRECORD_COUNT, &nrecs );
	for( db.record = 0; db.record < nrecs; db.record++ ) {

		allot( Sinfo *, sinfo, 1 );

		strcpy( sinfo->loc, "" );
		strcpy( sinfo->segtype, segtype );

		dbgetv( db, 0,
			"wincode", wincode, 
			"sta", sinfo->sta,
			"chan", sinfo->chan,
			"net", sinfo->net, 
			"calib", &sinfo->calib,
			"calper", &sinfo->calper,
			"commdelay", &sinfo->commdelay,
			0 );
		clear_register( 1 );

		sprintf( stacode, "0x%s", wincode );
			
		setarr( Site_info, stacode, sinfo );
	}

	dbclose( db );
}

Sinfo *
retrieve_station_info( char *stacode ) 
{
	return (Sinfo *) getarr( Site_info, stacode );
}

