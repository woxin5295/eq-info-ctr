#include <stdlib.h>
#include <stdio.h>
#include "win.h"

static int reformat_win_data( int, int, unsigned char *, int * );

unsigned char *
strip_next_channel( unsigned char *block, char *stacode, int *nsamp, int **data )
{
	unsigned int header;
	unsigned char *block_p;
	int	datasize_code, data_block_size;
	int	stacode_hex;

	block_p = block;
	memcpy( &header, block_p, 4 );
	block_p += 4;

	stacode_hex = header >> 16 & 0xFFFF;
	hex_to_ascii( stacode_hex, stacode );
	*nsamp = header & 0xFFF;

	datasize_code = ( header >> 12 ) & 0xF;
	if( datasize_code ) {
		data_block_size = datasize_code * ( *nsamp - 1 ) + 4;
	} else {
		data_block_size = ( *nsamp >> 1 ) + 4;
	}
	
	*data = (int *) malloc( *nsamp * sizeof( int ) );
	reformat_win_data( datasize_code, *nsamp, block_p, *data );

	return block + 4 + data_block_size;
}

/* reformat_win_data is modified by Lindquist from code in dewin.c by Urabe-san,
 *   ERI, Tokyo 
 */
static int 
reformat_win_data( int datasize_code, int nsamp, unsigned char *block, int *data )
{
	int	i;
	unsigned char *dp;	/* Pointer for walking through input data */
	unsigned char *pts;	/* Pointer for decompression calculations */
	short	shreg;		/* Temporary register */
	int	inreg;	 	/* Temporary register */

	dp = block;

	pts = (unsigned char *) &data[0];
	for( i = 0; i<4; i++ ) *pts++ = (*dp++);

	if( nsamp == 1 ) return 1;

	switch( datasize_code )
	{
	case 0:
		for( i = 1; i < nsamp-1; i += 2 )
		{
			data[i] = data[i-1] + ((*(char *)dp)>>4);
			data[i+1] = data[i] + (((char)(*(dp++)<<4))>>4);
		}
		data[i] = data[i-1] + ((*(char *)dp)>>4);
		break;
	case 1:
		for( i = 1; i < nsamp; i++ )
			data[i] = data[i-1] + (*(char *)(dp++));
		break;
	case 2:
		for( i = 1; i < nsamp; i++ )
		{
			pts = (unsigned char *) &shreg;
			*pts++ = (*dp++);
			*pts  = (*dp++);
			data[i] = data[i-1] + shreg;
		}
		break;
	case 3:
		for( i = 1; i < nsamp; i++ )
		{
			pts = (unsigned char *) &inreg;
			*pts++ = (*dp++);
			*pts++ = (*dp++);
			*pts  = (*dp++);
			data[i] = data[i-1] + (inreg>>8);
		}
		break;
	case 4:
		for( i = 1; i < nsamp; i++ )
		{
			pts = (unsigned char *) &inreg;
			*pts++ = (*dp++);
			*pts++ = (*dp++);
			*pts++ = (*dp++);
			*pts  = (*dp++);
			data[i] = data[i-1] + inreg;
		}
		break;
	default:
		return 0; /* bad header */
	}

	return nsamp;

}

