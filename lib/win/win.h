
/* Include file for conversion of Japanese WIN-format data */

#ifndef _win_h_
#define _win_h_

#include "Pkt.h"
#include "stock.h"

#define OUTER_JOIN 1

typedef struct sinfo {
	char	sta[PKT_NAMESIZE];
	char	net[PKT_NAMESIZE];
	char	chan[PKT_NAMESIZE];
	char	loc[PKT_NAMESIZE];
	char	segtype[4];
	double	calib;
	double	calper;
	double	commdelay;
} Sinfo;
extern Arr *Site_info;

extern double japan_timecode_to_epoch( char * );
extern void epoch_to_japan_timecode( double, char *, double * );
extern unsigned char *strip_next_channel( unsigned char *, char *, int *, int ** );
extern int bcd2dec( char );
extern unsigned char dec2bcd( int dec );
extern char *hex_to_ascii( int, char * );
extern int ascii_to_hex( char * );
extern unsigned char *strip_time( unsigned char *, double * );
extern void load_station_info( char *, char * );
extern Sinfo *retrieve_station_info( char * );
extern int winform( long *, unsigned char *, int, unsigned short );

#endif
