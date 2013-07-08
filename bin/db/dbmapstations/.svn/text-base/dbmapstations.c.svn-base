#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include "db.h"
#include "stock.h"
#include "coords.h"

void make_stock_pf( void );

main( int argc, char **argv )
{
	char	*dbmapevents = "ak_dbmapevents";
	char	tempdir[FILENAME_MAX];
	char	*table;
	char	command[STRSZ];
	char	tempdb[STRSZ];
	char	center[STRSZ];
	char	expr[STRSZ];
	double	range;
	Dbptr	db;
	Dbptr	dbout;
	long	nrows;
	char	row[STRSZ];
	char	sta[7];
	long	ondate;
	long	offdate;
	int	select_row = -1;
	double	lat, lon;
	double	latctr, lonctr;
	double 	maxdist;
	double	delta;

	if( argc < 2 || argc > 3 ) {
		die( 1, "Usage: %s {dbname.site|-} [record]\n", argv[0] );
	} else {
		table = argv[1];
		if( argc == 3 ) sscanf( argv[2], "%d", &select_row );
	}

	sprintf( tempdir, "/tmp/dbmapstations_%s_%d",
			  getenv( "USER" ),
			  getpid() );
	mkdir( tempdir, 0755 );
	chdir( tempdir );

	if (dbopen_database(table, "r+", &db)< 0)
		die(0, "Can't open %s\n", table ) ;
	
	if( select_row != -1 ) {
		db.record = select_row;
		dbgetv( db, 0, "sta", sta, "ondate", &ondate, "offdate", &offdate, NULL );
		sprintf( expr, "sta == \"%s\" && ondate == %ld && offdate == %ld",
				sta, ondate, offdate );
		db = dbsubset( db, expr, 0 );
	}

	sprintf( tempdb, "moo%d", getpid() );
	sprintf( command, "/bin/rm %s.site %s.origin > /dev/null 2>&1 ", tempdb, tempdb );
	system( command );

	dbopen( tempdb, "r+", &dbout );
	dbout = dblookup( dbout, 0, "site", 0, 0 );

	dbtruncate( dbout, 0 );
	dbquery( db, dbRECORD_COUNT, &nrows );
	if( (long)nrows < (long)1 ) die( 1, "No records in database\n" );
	for( db.record = 0; (long) db.record < (long) nrows; db.record++ ) {

		dbgetv( db, 0, "sta", sta, "ondate", &ondate, 
			"site.lat", &lat, "site.lon", &lon, NULL  );
		dbout.record = dbaddnull( dbout );
		dbputv( dbout, "site", "sta", sta,
					"ondate", ondate,
					"lat", lat,
					"lon", lon, NULL );
	}

	latctr = lonctr = 0.;
	for( db.record = 0; (long) db.record < (long) nrows; db.record++ ) {
		dbgetv( db, 0, "site.lat", &lat, "site.lon", &lon, NULL );
		latctr += lat;
		lonctr += lon;
	}
	latctr /= nrows;
	lonctr /= nrows;

	sprintf( center, "%f:%f", latctr, lonctr );

	maxdist = 0.;
	for( db.record = 0; (long) db.record < (long) nrows; db.record++ ) {
		dbgetv( db, 0, "site.lat", &lat, "site.lon", &lon, NULL  );
		sprintf( command,
			"distance(%f,%f,%f,%f)",
			latctr, lonctr, lat, lon );
		dbex_evalstr( db, command, dbREAL, &delta );
		maxdist = MAX( maxdist, delta );
	}
	range = MAX( 5., ceil( maxdist ) + 1 );

	make_stock_pf();

	sprintf( command, "%s %s %s %f 2>&1 | grep -v origin 2>&1 | grep -v events",
		dbmapevents, tempdb, center, range );
	system( command );

	sprintf( command, "/bin/rm %s.site %s.origin > /dev/null 2>&1", tempdb, tempdb );
	system( command );

	return 0;
}

void make_stock_pf() 
{
	FILE *fp;

	fp = fopen( "ak_dbmapevents.pf", "w" );

        fprintf( fp, "center 62.4:-150.3\n" );
        fprintf( fp, "range 5\n" );
        fprintf( fp, "istaplt 1\n" );
        fprintf( fp, "istnam 1\n" );
        fprintf( fp, "ipdeplt 0\n" );
        fprintf( fp, "ipdepth 0\n" );
        fprintf( fp, "iporid 0\n" );
        fprintf( fp, "icities 1\n" );
        fprintf( fp, "ipipe 1\n" );
        fprintf( fp, "iblue 0\n" );
        fprintf( fp, "ipmag 0\n" );
        fprintf( fp, "idcirc 0\n" );
        fprintf( fp, "ititl 1\n" );
        fprintf( fp, "iflt 1\n" );
        fprintf( fp, "ipumps 0\n" );
        fprintf( fp, "icol 1\n" );
        fprintf( fp, "itran 0\n" );
        fprintf( fp, "title Seismic Stations\n" );
        fprintf( fp, "label_files  &Tbl{\n" );
        fprintf( fp, "country\n" );
        fprintf( fp, "faults\n" );
        fprintf( fp, "oceans\n" );
        fprintf( fp, "pipeline\n" );
        fprintf( fp, "volcanoes\n" );
        fprintf( fp, "}\n" );
        fprintf( fp, "boilerplate_files  &Tbl{\n" );
        fprintf( fp, "aeic\n" );
        fprintf( fp, "aeic_vol\n" );
        fprintf( fp, "}\n" );

	fclose( fp );
}
