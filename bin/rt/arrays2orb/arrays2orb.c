#include <stdlib.h>
#include "stock.h"
#include "orb.h"
#include "Pkt.h"
#include <math.h>
#include <sys/types.h>
#include <time.h>


#define MAX_BLOCK_SIZE 1200
#define FRAME_A_LENGTH_BYTES 978
#define FRAME_B_LENGTH_BYTES 950

#define CRC_GOOD 0
 
#define STREQ(a, b) (strcmp((a), (b)) == 0)

unsigned short ComputeCRC( void *, int );

void packetlength_to_blockname( int, char * );
Pf *get_pf( char * );

main( int argc, char **argv )
{
        unsigned char block[MAX_BLOCK_SIZE];
	unsigned char *frame;
	int	length;
	char	block_name[STRSZ];
	char 	*net;
	Pf	*pf;
	Tbl	*pkts = 0;
	int	ipkt, ichan;
	char	*packet = 0;
	int	bufsize;
	int	nbytes;
	struct Packet *pkt;
	struct PktChannel *pktchan;
	int	orbfd;
	char	srcid[STRSZ];
	int	rc;
	double diff_time; 
	time_t computer_time;

	if( argc != 2) die( 1, "Usage: %s orbname\n", argv[0] );

	orbfd = orbopen( argv[1], "w&" );
	if( orbfd < 0 )
	{
		clear_register( 1 );
		exit( 1 );
	}

	for( ;; )
	{
		get_next_block( &block[0], &length );

		packetlength_to_blockname( length, &block_name[0] );

		if( STREQ( block_name, "" ) ) continue;

		if( ComputeCRC( (void *) &block[0], length-1 ) != CRC_GOOD ) {
			fprintf( stderr, 
				"%s: Rejecting packet with failed checksum, length %d\n",
				strtime( str2epoch( "now" ) ), length-1 );
			continue;
		}

		frame = &block[0] + 2;

		pf = get_pf( block_name );

		net = pfget_string( pf, "net" );

		CTFrame2orbpkts( frame, block_name, net, &pkts );

		for( ipkt = 0; ipkt < maxtbl( pkts ); ipkt++ )
		{
			pkt = gettbl( pkts, ipkt );
			for( ichan = 0; ichan < maxtbl( pkt->channels ); ichan++ )
			{
				pktchan = gettbl( pkt->channels, ichan );
				mystuff_iw_tracebuf( 0, 0, pktchan,
						     &packet, &nbytes, &bufsize );
				sprintf( srcid, "%s_%s_%s",
						net, pktchan->sta, pktchan->chan );
				/* make sure packet is within 20 */
				/* of computer time */
				diff_time = fabs(time(&computer_time) - 
					pktchan->time);
				if(diff_time < 1200.0) {
					rc = orbput( orbfd, srcid, pktchan->time, packet, nbytes );
					if( rc != 0 ) fprintf( stderr, "orbput %d\n", rc );
					orbflush( orbfd );
					clear_register( 1 );
				}
			}
		}
	}
}

void
packetlength_to_blockname( int length, char *blockname )
{
	switch( length )
	{
	case FRAME_A_LENGTH_BYTES:
		strcpy( blockname, "FrameA" );
		break;
	case FRAME_B_LENGTH_BYTES:
		strcpy( blockname, "FrameB" );
		break;
	default:
		fprintf( stderr, "%s: Unexpected block length %d\n",
				strtime( str2epoch( "now" ) ), length );
		strcpy( blockname, "" );
		break;
	}

}
