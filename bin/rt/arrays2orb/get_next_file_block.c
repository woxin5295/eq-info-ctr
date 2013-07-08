#include <stdlib.h>
#include <stdio.h>

int match_end( unsigned char * );

void
get_next_file_block( char *datafile, unsigned char *block, int *length )
{
	static FILE *fp;
	static int first = 1;

	if( first )
	{
		fp = fopen( datafile, "r" );
		first = 0;
	}

	fread( block, 5, sizeof( char ), fp );
	while( ! match_end( block ) )
	{
		memmove( block, block + 1, 4 );
		fread( &block[4], 1, sizeof( char ), fp );
	}
	*length = 5;

	fread( &block[++(*length) - 1], 1, sizeof( char ), fp );
	while( ! match_end( block + *length - 4 - 1 ) )
	{
		fread( &block[++(*length) - 1], 1, sizeof( char ), fp );
	}
	fseek( fp, -5, SEEK_CUR );
	*length -= 4;
}

int 
match_end( unsigned char *buf )
{
	if( buf[0] != 0x01 && buf[0] != 0x02 ) return 0;
	if( buf[0] != buf[1] ) return 0;
	if( buf[2] != 0x01 ) return 0;
	if( buf[3] != 0x00 ) return 0;
	if( buf[4] != 0x01 ) return 0;
	return 1;
}
