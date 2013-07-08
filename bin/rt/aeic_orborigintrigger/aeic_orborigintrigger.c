#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include "db.h"
#include "stock.h"
#include "orb.h"
#include "xtra.h"
/* #include "pkt.h" */
#include "Pkt.h" 
#include "pf.h"

/* 
 * aeic_orborigintrigger
 *
 * K. Lindquist
 * Geophysical Institute
 * U. of Alaska
 * October, 1998
 * 
 */

#define PF_DEFAULT "aeic_orborigintrigger" 
#define USAGE "Usage: aeic_orborigintrigger [-v] [-p pffile] orbin orbout\n"

main( int argc, char **argv )
{
	char	*orbname_in;
	char	*orbname_out;
	char	tmpdb[FILENAME_MAX];
	char	pfname[STRSZ];
	char	pf_srcname[STRSZ];
	Pf	*config_pf;
	Pf	*output_pf;
	Tbl	*commands;
	char	*command;
	int	icommand; 
	int	ncommands;
	Dbptr	db;
	int	orbin;
	int	orbout;
	int	pktid;
	char	srcname[STRSZ];
	double	pkttime;
	char	*packet = NULL;
	int	nbytes = 0;
	int	bufsize = 0;
	int	ndef;
	double	ml;
	double	lat;
	double	lon;
	double	depth;
	double	origin_time;
	char	time_string[STRSZ];
	extern char *optarg;
	extern int optind;
	int	verbose = 0;
	char	*s;
	int	c;

	elog_init( argc, argv );

	strcpy( pfname, PF_DEFAULT ); 

	while( ( c = getopt( argc, argv, "vp:" ) ) != EOF ) {
		switch( c ) {
		case 'p':
			strcpy( pfname, optarg );
			break;
		case 'v':
			verbose = 1;
			break;
		default:
			die( 1, USAGE );
			break;
		}
	}

	if( argc - optind != 2 ) {
		die( 1, USAGE );
	} else {
		orbname_in = argv[argc-2];
		orbname_out = argv[argc-1];
	}

	pfread( pfname, &config_pf );
	s = pfget_string( config_pf, "pf_srcname" );
	if( s ) {
		strcpy( pf_srcname, s );
	} else {
		die( 1, "Can't find pf_srcname in parameter file %s\n", pfname );
	}
	pffree( config_pf );

	while( ( orbin = orbopen( orbname_in, "r&" ) ) < 0 ) {
		complain( 1, "aeic_orborigintrigger: failed to open %s; retry\n",
			orbname_in );
		sleep( 10 );
	}

	while( ( orbout = orbopen( orbname_out, "w&" ) ) < 0 ) {
		complain( 1, "aeic_orborigintrigger: failed to open %s; retry\n",
			orbname_out );
		sleep( 10 );
	}

	sprintf( tmpdb, "/tmp/db_%d_%d", getuid(), getpid() );

	dbopen( tmpdb, "r+", &db );
	db = dblookup( db, 0, "origin", 0, 0 );

	orbselect( orbin, "/db/origin" );

	for( ;; ) {

		orbreap( orbin, &pktid, srcname, &pkttime,
				&packet, &nbytes, &bufsize );

		db = orbpkt2db( packet, nbytes, db );

		if( db.table < 0 ) {
			clear_register( 1 );
			die( 1, 
				"aeic_orborigintrigger: Failed to interpret %s packet, pktid %d, timestamp %f. Giving up.\n",
				srcname,
				pktid,
				pkttime );
		}

		dbgetv( db, 0, "lat", &lat,
			       "lon", &lon,
			       "depth", &depth,
			       "time", &origin_time,
			       "ndef", &ndef,
			       "ml", &ml,
			       0 );

		pfread( pfname, &config_pf );

		commands = pfget_tbl( config_pf, "commands" );

		ncommands = maxtbl( commands );

		for( icommand = 0; icommand < ncommands; icommand++ ) {

			command = gettbl( commands, icommand );

			output_pf = pfnew( PFFILE );

			pfput_double( output_pf, "lat", lat );
			pfput_double( output_pf, "lon", lon );
			pfput_double( output_pf, "depth", depth );
			pfput_double( output_pf, "ml", ml );
			pfput_int( output_pf, "ndef", ndef );
	
			/* Force non-exponential notation: */
			sprintf( time_string, "%f", origin_time );
			pfput_string( output_pf, "time", time_string );

			pfput_string( output_pf, "cmdstring", command );

			if( verbose ) {
				fprintf( stderr, 
					"\n%s UTC Sending pf file:\n",
					 s = strtime( (double) time( 0 ) ) );
				free( s );

				pfout( stderr, output_pf ); 

				fprintf( stderr, "\n" );
			}

			pf2orbpkt( output_pf, pf_srcname, orbout );
			clear_register( 1 );

			pffree( output_pf );

		}

		free( commands );
		pffree( config_pf );
	}
}

