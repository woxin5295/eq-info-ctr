/*
 * japan_timecode_to_epoch
 *
 * Convert a timecode of the form 
 *   YYMMDDhhmmss, Binary Coded Decimal, Japan Standard Time,
 * to UTC epoch time (seconds since midnight 1/1/1970)
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * November, 1996
 */

#include <stdlib.h>
#include <stdio.h>
#include "stock.h"
#include "coords.h"
#include "win.h"
 
double
japan_timecode_to_epoch( char *bcd_timecode )
{
	double	epoch;
	int	time_parts[6];
	char	timestr[STRSZ];
	int	cntr;

	for( cntr = 0; cntr < 6; cntr++ ) {
		time_parts[cntr] = bcd2dec( bcd_timecode[cntr] );
	}

	sprintf( timestr, "%02d/%02d/%02d %02d:%02d:%02d Japan",
			time_parts[1], time_parts[2], time_parts[0],
			time_parts[3], time_parts[4], time_parts[5] );
	epoch = str2epoch( timestr );
	
	return epoch;
}

void 
epoch_to_japan_timecode( double epoch, char *bcd_timecode, double *remainder_sec )
{
	char 	*s;
	int	year, month, day, hour, minute, second, millisecond;

	s = zepoch2str( epoch, "%y %m %e %H %M %S %s", "Japan" );

	sscanf( s, "%d%d%d%d%d%d%d",
		&year, &month, &day, &hour, &minute, &second, &millisecond );
	free( s );

	bcd_timecode[0] = dec2bcd( year % 100 );
	bcd_timecode[1] = dec2bcd( month  );
	bcd_timecode[2] = dec2bcd( day );
	bcd_timecode[3] = dec2bcd( hour  );
	bcd_timecode[4] = dec2bcd( minute );
	bcd_timecode[5] = dec2bcd( second );

	*remainder_sec = millisecond / 1000.;
}
