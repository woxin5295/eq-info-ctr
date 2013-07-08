#include <stdlib.h>
#include <stdio.h>
#include <coords.h>

#define STRSZ 1024

double CTFrame_time_to_epoch( int year, unsigned char buf[5] );

double
CTFrame_time_to_epoch( int year, unsigned char buf[5] )
{
	int	doy, hr, min, sec;
	double	epoch;
	char	s[STRSZ];

	doy = buf[0] * 0x100 + buf[1];
	hr = buf[2];
	min = buf[3];
	sec = buf[4];
	sprintf(s,"%d%03x %x:%02x:%02x\n",year,doy,hr,min,sec);
	epoch = str2epoch( s );
}
