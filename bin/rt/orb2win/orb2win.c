/*
 * orb2win
 *
 * Program to take data from orb and convert to Japanese WIN-format data 
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * June, 2000
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "orb.h"
#include "Pkt.h"
#include "win.h"

extern int OrbFd = 0;
extern int Verbose = 0;

#define STREQ(X,Y) (! strcmp(X,Y) )
#define MAXMESG 2048

static void
usage ()
{
	fprintf( stderr, 
	  "Usage: %s [-p pffile] [-m match] [-r reject] orbname\n",
		Program_Name );
	exit( 1 );
}

main( int argc, char **argv ) {
	int 	c = 0;
	int	errflag = 0;
	char	*str;
	Pf	*pf;

	char	pfname[STRSZ];
	char	orbname[STRSZ];
	char 	site_dbname[FILENAME_MAX];
	char	*match = 0;
	char	*reject = 0;
	int	nmatch;

	int	pktid;
	char	srcname[ORBSRCNAME_SIZE];
	Srcname	srcparts;
	char	*packet;
	double	time;
	int	nbytes;
	int	packetsz = 0;
	Packet	*pkt = 0;

	char 	key[STRSZ];
	Dbptr	db;
	char	*wincode;
	Arr	*wincodes;
	int	nrecs;

	elog_init( argc, argv );

	strcpy( pfname, Program_Name );
	strcpy( orbname, "" );

	while( ( c = getopt( argc, argv, "p:r:m:" ) ) != -1 ) {
		switch( c ) {
		case 'p':
			strcpy( pfname, optarg );
			break;
		case 'r':
			reject = optarg;
			break;
		case 'm':
			match = optarg;
			break;
		default:
			usage();
			break;
		}
	}

	if( errflag || ( argc - optind != 1 ) ) {
		usage(); 
	} else {
		strcpy( orbname, argv[optind++] );		
	}

	if( pfread( pfname, &pf ) != 0 ) {
		die( 1, "Can't read parameter file %s\n", pfname );
	}

	if( ( str = getenv( "SITE_DB" ) ) == NULL ) {
		die( 1, "Environment variable SITE_DB must be defined\n" );
	} else {
		strcpy( site_dbname, str );

		wincodes = newarr( 0 );

		if( dbopen( site_dbname, "r", &db ) < 0 ) {
			die( 1, "Couldn't open %s\n", site_dbname );
		}

		db = dblookup( db, 0, "win", 0, 0 );
		dbquery( db, dbRECORD_COUNT, &nrecs );
		if( nrecs <= 0 ) {
			die( 1, "No records in win table of %s\n",
				site_dbname );
		}
		for( db.record = 0; db.record < nrecs; db.record++ ) {
			allot( char *, wincode, STRSZ );

			dbgetv( db, 0, 
				"sta", srcparts.src_sta,
				"chan", srcparts.src_chan,
				"net", srcparts.src_net,
				"wincode", wincode,
				NULL );

			sprintf( key, "%s_%s_%s", 
			      srcparts.src_net,
			      srcparts.src_sta,
			      srcparts.src_chan );

			setarr( wincodes, key, wincode );
		}
	}

	if( ( OrbFd = orbopen( orbname, "w&" ) ) == -1 ) {
		die( 1, "Failed connecting to orb %s\n", orbname );
	}

	if( match ) {
		if( ( nmatch = orbselect( OrbFd, match ) ) < 0 ) {
			complain( 1, "select '%s' returned %d\n",
				  match, nmatch );
		} else {
			fprintf( stderr, "match: %d sources selected\n",
				nmatch );
		}
	}
	if( reject ) {
		if( ( nmatch = orbreject( OrbFd, reject ) ) < 0 ) {
			complain( 1, "reject '%s' returned %d\n",
				  match, nmatch );
		} else {
			fprintf( stderr, "reject: %d sources selected\n",
				nmatch );
		}
	}

	while( 1 ) {
		
		orbreap( OrbFd, &pktid, srcname, &time, 
			 &packet, &nbytes, &packetsz );

		/* SCAFFOLD */
		showPkt( pktid, srcname, time, packet, 
			 nbytes, stdout, PKT_TERSE );

		split_srcname( srcname, &srcparts );
	
		sprintf( key, "%s_%s_%s", 
			      srcparts.src_net,
			      srcparts.src_sta,
			      srcparts.src_chan );
		if( ( wincode = getarr( wincodes, key ) ) == NULL ) {
			complain( 1, "Couldn't find WIN code for %s\n", key );
			continue;
		} else {
			printf( "SCAFFOLD: code is %s i.e. 0x%x\n",
				wincode, ascii_to_hex( wincode ) );
		}

		switch( unstuffPkt( srcname, time, packet, nbytes, &pkt ) ) {
		case Pkt_wf:
			break;
		default:
			continue;
		}
	}
}
