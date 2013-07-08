/* Routine to unstuff Science Horizons CTFrame packets into Datascope orb packets
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * May, 1997 
 */

#include "Pkt.h"

#include "frametbl.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

#define CTSTATUS_SIZE 4
#define RTSTATUS_SIZE 2

#define SPRT_SAMPRATE 20
#define LPRT_SAMPRATE 1
#define BBRT_SP_SAMPRATE 20
#define BBRT_LP_SAMPRATE 1

struct Packet *get_free_packet( int, Tbl ** );
double CTFrame_time_to_epoch( int, unsigned char a[5] );
void strip_RTsection2packets( char *, char *, double, char *,
			      int *, unsigned char **, Tbl ** );
void strip_and_check_status( char *, unsigned char ** );
double strip_starttime( unsigned char ** );
Pf *get_pf( char * );
Tbl *get_sections( char * );
void freepkt( void * );
void freepktchan( void * );
Tbl *malloced_split( char *, int );

void
CTFrame2orbpkts( frame, block_name, net, pkts )
unsigned char *frame;
char	*block_name;
char	*net;
Tbl	**pkts;
{
	struct Packet *Pkt;
	Tbl	*sections;
	unsigned char *frameptr;
	double	starttime;
	char	*RTtype;
	int	isect;
	int	npacked;

	frameptr = frame;
	frameptr += 3;		/* Skip address, control, and siteID */

	starttime = strip_starttime( &frameptr );

	strip_and_check_status( block_name, &frameptr );

	sections = get_sections( block_name );
	npacked = 0;
	for( isect = 0; isect < maxtbl( sections ); isect++ ) 
	{
		RTtype = gettbl( sections, isect );

		strip_RTsection2packets( block_name, RTtype, starttime, net,
				   &npacked, &frameptr, pkts );
	}
	freetbl( sections, free );

	while( maxtbl( *pkts ) > npacked ) 
	{
		Pkt = (struct Packet *) deltbl( *pkts, maxtbl( *pkts ) - 1 );
		freepkt( Pkt );
	}
}

double
strip_starttime( frameptr )
unsigned char **frameptr;
{
	double	starttime;
	unsigned char CTtime[5];
	char	*s;
	int	year;

	memcpy( CTtime, *frameptr, 5 );
	*frameptr += 5;
	year = atoi( s = epoch2str( time( 0 ), "%Y" ) );
	free( s );
	starttime = CTFrame_time_to_epoch( year, CTtime );

	return starttime;
}

void
strip_RTsection2packets( block_name, RTtype, starttime, net, npacked, frameptr, pkts )
char	*block_name;
char	*RTtype;
double	starttime;
char	*net;
int	*npacked;
unsigned char **frameptr;
Tbl	**pkts;
{
	struct Packet *Pkt;
	struct PktChannel *pktchan;
	siteinfo *sinfo;
	Tbl	*frames;
	double	samprate;
	int	nsamp;
	int	isamp;
	int	new_pktchannel;
	int	*data_out;
	int	nchan;
	int	ichan;
	int	irt;


	if( STREQ( RTtype, "SPRT" ) )
	{
		nchan = 1;
		nsamp = SPRT_SAMPRATE;
		samprate = SPRT_SAMPRATE;
	}
	else if( STREQ( RTtype, "LPRT" ) )
	{
		nchan = 3;
		nsamp = LPRT_SAMPRATE;
		samprate = LPRT_SAMPRATE;
	}
	else if( STREQ( RTtype, "BBRT" ) )
	{
		nchan = 6;
		nsamp = BBRT_SP_SAMPRATE;	/* begin here; switch to LP later */
		samprate = BBRT_SP_SAMPRATE;	/* begin here; switch to LP later */
	}
	else
	{
		fprintf( stderr, "RTtype %s unknown\n", RTtype );
		return;
	}

	frames = get_frametbl( block_name, RTtype );

	for( irt = 0; irt < maxtbl( frames ) / nchan; irt++ )
	{
		Pkt = get_free_packet( (*npacked)++, pkts );
		/* Pkt->pkttype = NULL;*/	/* SCAFFOLD */
		/* Pkt->hdrtype = NULL;*/   /* SCAFFOLD */
		if( Pkt->channels == 0 ) {
			Pkt->channels = newtbl( nchan );
		} else {
			while( maxtbl( Pkt->channels ) > nchan ) {
				pktchan = (PktChannel *)
					deltbl( Pkt->channels,
						maxtbl( Pkt->channels ) - 1 ); 
				if( pktchan != NULL ) freepktchan( pktchan );
			}
		}
		Pkt->nchannels = nchan;

		for( ichan = 0; ichan < nchan; ichan++ )
		{
			if( ichan >= 3 )
			{
				nsamp = BBRT_LP_SAMPRATE;
				samprate = BBRT_LP_SAMPRATE;
			}

			sinfo = gettbl( frames, irt * nchan + ichan );
			if( maxtbl( Pkt->channels ) <= ichan )
			{
				allot( PktChannel *, pktchan, 1 );
				settbl( Pkt->channels, ichan, (char *) pktchan );
				new_pktchannel = 1;
			}
			else
			{
				pktchan = (struct PktChannel *)
						gettbl( Pkt->channels, ichan );
				new_pktchannel = 0;
			}
			pktchan->time = starttime;
			pktchan->samprate = samprate;
			pktchan->calib = sinfo->calib;
			pktchan->nsamp = nsamp;
			/* pktchan->datatype = trINT; */
			strcpy( pktchan->net, net );
			strcpy( pktchan->sta, sinfo->sta );
			strcpy( pktchan->chan, sinfo->chan );

			/* if( new_pktchannel )
			/* {
			/* 	allot ( int *, data_out, pktchan->nsamp );
			/* 	pktchan->data = (void *) data_out;
			/* 	pktchan->nbytes = pktchan->nsamp * sizeof( int );
			/* }
			/* else if( pktchan->nbytes < pktchan->nsamp * sizeof( int ) )
			/* {
			/*	data_out = (int *) pktchan->data;
			/*	reallot( int *, data_out, pktchan->nsamp );
			/*	pktchan->data = (void *) data_out;
			/*	pktchan->nbytes = pktchan->nsamp * sizeof( int );
			/*}
			/*else
			/*{
			/*	data_out = (int *) pktchan->data;
			/*} */
			data_out = (int *) pktchan->data;	
			for( isamp = 0; isamp < nsamp; isamp++ )
			{
				data_out[isamp] = g2i( *frameptr );
				*frameptr += 2;
			}
		}
	}
}

void
strip_and_check_status( block_name, frameptr )
char	*block_name;
unsigned char **frameptr;
{
	Pf	*pf;
	Tbl	*sections;
	Tbl	*chantbl;
	int	nRTs = 0;
	char	*RTtype;
	unsigned char *CTstatus;
	unsigned char *RTstatus;
	int	nchan;
	int	istat;
	int	isect;

	pf = get_pf( block_name );

	sections = get_sections( block_name );

	for( isect = 0; isect < maxtbl( sections ); isect++ ) 
	{
		RTtype = gettbl( sections, isect );
		chantbl = pfget_tbl( pf, RTtype );
		nchan = maxtbl( chantbl );
		freetbl( chantbl, 0 );

		if( STREQ( RTtype, "SPRT" ) ) 
		{	
			nRTs += nchan;
		}
		else if( STREQ( RTtype, "LPRT" ) )
		{
			nRTs += nchan / 3;
		}
		else if( STREQ( RTtype, "BBRT" ) )
		{
			nRTs += nchan / 6;
		}
	}
	freetbl( sections, free );

	allot( unsigned char *, CTstatus, CTSTATUS_SIZE );
	memcpy( CTstatus, *frameptr, CTSTATUS_SIZE );
	*frameptr += CTSTATUS_SIZE;
	allot( unsigned char *, RTstatus, RTSTATUS_SIZE * nRTs );
	memcpy( RTstatus, *frameptr, RTSTATUS_SIZE * nRTs );
	*frameptr += RTSTATUS_SIZE * nRTs;
	if( CTstatus[0] != 0x0 || CTstatus[1] != 0x0 ||
	    CTstatus[2] != 0x20 || CTstatus[3] != 0x0 )
	{
		fprintf( stderr, "STATUS %08x for CT\n",
			 CTstatus[0] * 0x1000000 + 
			 CTstatus[1] * 0x10000 +
			 CTstatus[2] * 0x100 +
			 CTstatus[3] );
	}

	for( istat = 0; istat < nRTs; istat++ )
	{
		if( RTstatus[istat*2]*0x100+RTstatus[istat*2+1] != 0x4100 )
			fprintf( stderr, "STATUS %04x for RT %d\n", 
				RTstatus[istat*2]*0x100+RTstatus[istat*2+1], istat );
	}
	free( CTstatus );
	free( RTstatus );
}

Tbl *
get_sections( block_name )
char *block_name;
{
	Pf	*pf;
	Tbl	*sections;
	char	*section_list;
	char	section_list_copy[STRSZ];

	pf = get_pf( block_name );

	section_list = pfget_string( pf, "RT_order" );
	strcpy( section_list_copy, section_list );
	sections = malloced_split( section_list_copy, ' ' );

	return sections;
}

void
freepkt( void *pktp )
{
	struct Packet *pkt = (struct Packet *) pktp;

	freetbl( pkt->channels, freepktchan );
	free( pkt );
}

void
freepktchan( void *pktchanp )
{
	struct PktChannel *pktchan = (struct PktChannel *) pktchanp;

	if( pktchan->data != 0 ) free( pktchan->data );
	free( pktchan );
}

struct Packet *
get_free_packet( int index, Tbl **pkts )
{
	struct Packet *Pkt;
	static int nold_pkts = 0;

	if( *pkts == 0 )
	{
		*pkts = newtbl( 0 );
		nold_pkts = 0;
	}
	else if( index == 0 )
	{
		/* Assume we always start filling the table of packets 
		from the beginning */

		nold_pkts = maxtbl( *pkts );
	}

	if( index >= nold_pkts ) 
	{
		Pkt = newPkt();
		pushtbl( *pkts, Pkt );
	}
	else
	{
		Pkt = gettbl( *pkts, index );
	}

	return Pkt;
}


