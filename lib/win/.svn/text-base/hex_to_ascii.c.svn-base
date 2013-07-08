/* hex_to_ascii.c
 * 
 * Convert an integer from hexidecimal to its uppercase ascii representation 
 *
 * Takes and returns a pointer to a user-supplied character buffer for the 
 * ascii output
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska
 * November, 1996
 */

#include <stdlib.h>
#include <stdio.h>
#include "win.h"

static char nybble_to_hexchar( int );

char *
hex_to_ascii( int hex, char *ascii )
{
	int	offset;
	int	nybble;

	if( hex == 0 ) {
		sprintf( ascii, "0x0" );
		return( ascii );
	}

	offset = 8 * sizeof( int );

	sprintf( ascii, "0x" );
	while( offset ) {
		offset -= 4;
		nybble = hex >> offset & 0xF;
		if( nybble > 0 ) {
			sprintf( ascii, "%s%c", ascii,
				nybble_to_hexchar( nybble ) );
			break;
		}
	}
	while( offset ) {
		offset -= 4;
		nybble = hex >> offset & 0xF;
		sprintf( ascii, "%s%c", ascii, nybble_to_hexchar( nybble ) );
	}

	return( ascii );
}

static char
nybble_to_hexchar( int nybble )
{
	if( nybble < 10 ) {
		return nybble + 0x30;
	} else {
		return nybble + 0x41 - 0xA;
	}
}

int	
ascii_to_hex( char *ascii )
{
	return strtol( ascii, (char **)NULL, 16 );
}
