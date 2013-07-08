/*
 * win2orb
 *
 * Program to take Japanese WIN-format data and put it on an orb
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
#include "orb.h"
#include "Pkt.h"
#include "win.h"

#define STREQ(X,Y) (! strcmp(X,Y) )
#define MAXMESG 2048

extern int OrbFd = 0;
extern int Verbose = 0;
extern int ReallyVerbose = 0;
extern int Timecorr = 0;

static void
usage ()
{
	fprintf( stderr, 
	  "Usage: %s [-v] [-V] [-p pfname] [-u port] orbname\n",
		Program_Name );
	exit( 1 );
}

static unsigned char *
strip_next_second_block( unsigned char *block )
{
	int	length;
	unsigned char *block_p;
	char	stacode[STRSZ];
	double	starttime;
	double	secondblock_starttime;
	int	nsamp;
	int	*data;
	union {
		unsigned long i;
		char c[4];
	} un;

	Packet *pkt;
	PktChannel *pktchan;
	Sinfo	*sinfo;
	char	srcname[STRSZ];
	char	*packet = 0;
	int	nbytes;
	int	packetsz = 0;

	un.i = 0;
	un.c[2] = block[0];
	un.c[3] = block[1];

	length = un.i;

	block_p = strip_time( &block[2], &secondblock_starttime );

	while( block_p < block + length ) {

		block_p = strip_next_channel( block_p, stacode, &nsamp, &data );

		if( ( sinfo = retrieve_station_info( stacode ) ) == NULL ) {
			complain( 1, "Failed to retrieve station info for %s\n", 
				  stacode );
			continue;
		}

		if( Timecorr ) {
			starttime = secondblock_starttime - sinfo->commdelay;
		} else {
			starttime = secondblock_starttime;
		}

		pkt = newPkt();
		pktchan = newPktChannel();

		strcpy( pkt->parts.src_net, sinfo->net );
		strcpy( pkt->parts.src_sta, sinfo->sta ); 
		strcpy( pkt->parts.src_chan, sinfo->chan ); 
		pkt->time = starttime;
		pkt->pkttype = suffix2pkttype( "replay" );
		pkt->nchannels = 1;

		pktchan->data = data;
		pktchan->time = starttime;
		pktchan->samprate = (double) nsamp;
		pktchan->calib = sinfo->calib; 
		pktchan->calper = sinfo->calper; 
		pktchan->nsamp = nsamp;
		pktchan->datasz = nsamp; 
		strcpy( pktchan->net, sinfo->net ); 
		strcpy( pktchan->sta, sinfo->sta ); 
		strcpy( pktchan->chan, sinfo->chan ); 
		strcpy( pktchan->loc, sinfo->loc ); 
		strcpy( pktchan->segtype, sinfo->segtype ); 

		pushtbl( pkt->channels, pktchan );

		join_srcname( &pkt->parts, srcname );

		if( stuffPkt( pkt, srcname, &starttime, &packet, &nbytes, &packetsz )<0){
			complain( 0, "stuffPkt routine failed for %s\n", 	
				  pkt->pkttype->name );
		} else if( orbput( OrbFd, srcname, starttime, packet, nbytes ) ) {
			complain( 0, "orbput failes\n" );
		}

		freePkt( pkt );
	}

	free( packet );

	return block_p;
}

main( int argc, char **argv ) {
	int 	c = 0;
	int	errflag = 0;
	char	*str;
	Pf	*pf;
	FILE	*fp;

	char	pfname[STRSZ];
	char	orbname[STRSZ];
	char 	site_dbname[FILENAME_MAX];
	char 	*segtype;

	int	sock;
	int	rcv_buf_size = 32768;
	unsigned short 	port = 0;
	unsigned char buffer[MAXMESG];
	unsigned char *buffer_p;
	struct sockaddr_in to_addr;
	struct sockaddr_in from_addr;
	int	addr_len = sizeof( from_addr );
	int	nbytes;

	elog_init( argc, argv );

	strcpy( pfname, Program_Name );
	strcpy( orbname, "" );

	while( ( c = getopt( argc, argv, "svVp:f:u:" ) ) != -1 ) {
		switch( c ) {
		case 'p':
			strcpy( pfname, optarg );
			break;
		case 'u':
			port = (unsigned short) atoi( optarg );
			break;
		case 'v':
			Verbose = 1;
			break;
		case 'V':
			ReallyVerbose = 1;
			Verbose = 1;
			break;
		default:
			errflag++;
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
	} else {
		segtype = pfget_string( pf, "segtype" );
		Timecorr = pfget_boolean( pf, "timecorr" );
		if( Timecorr && Verbose ) {
			fprintf( stderr, 
				"Applying time corrections from timecorr table\n" );
		}
	}

	if( ( str = getenv( "SITE_DB" ) ) == NULL ) {
		die( 1, "Environment variable SITE_DB must be defined\n" );
	} else {
		strcpy( site_dbname, str );
		load_station_info( site_dbname, segtype );
	}

	if( ( OrbFd = orbopen( orbname, "w&" ) ) == -1 ) {
		die( 1, "Failed connecting to orb %s\n", orbname );
	}

	if( port != 0 ) {

		if( ( sock = socket( AF_INET, SOCK_DGRAM, 0 ) ) < 0 ) {

			die( 1, "Error creating unbound socket\n" );
		}

		if( setsockopt( sock,
		   		SOL_SOCKET,
				SO_RCVBUF,
				(char *)&rcv_buf_size,
				sizeof(rcv_buf_size) ) < 0 ) {

			die( 1, "Error setting socket receive-buffer size\n" );
		}

		memset( (char *) &to_addr, 0, sizeof(to_addr) );
		to_addr.sin_family = AF_INET;
		to_addr.sin_addr.s_addr = htonl( INADDR_ANY );
		to_addr.sin_port = htons( port );

		if( bind( sock, 
			  (struct sockaddr *) &to_addr,
			  sizeof( to_addr ) ) < 0 ) {

			die( 1, "Error binding socket to local address\n" );
		}

		while( 1 ) {

			nbytes = recvfrom( sock,
					   (void *) buffer, 
					   MAXMESG, 
					   0, 
					   (struct sockaddr *) &from_addr, 
					   &addr_len );

			buffer_p = &buffer[0];

			if( ReallyVerbose ) fprintf( stderr,
				 "Received %d bytes, packet %d(%d), id %X\n",
				 nbytes, buffer[0], buffer[1], buffer[2] );
			buffer_p += 3;

			do {

				buffer_p = strip_next_second_block( buffer_p );

			} while( buffer_p < &buffer[0] + nbytes );
		}

	} else {

		die( 1, "No input method specified\n" );
	}

}

