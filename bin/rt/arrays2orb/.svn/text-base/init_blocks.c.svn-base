#include <stdlib.h>
#include <ctype.h>
#include "stock.h"
#include "pf.h"

#include "frametbl.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

typedef struct Frameid {
	char    band;
	int     num;
	char    comp[3];
} Frameid;

void strreplace( char *, char, char );
siteinfo *parse_siterow( char * );
Tbl *sitetbl2frametbl( Tbl * );
int cmp_sinfo_order( void *, void *, void * );
int cmp_vs_ordered_triplet( char order[3], char, char );
void expand_frameid( char frameid[5], Frameid * );
int printrow( void *, void * );
Pf *get_pf( char * );


Tbl *
get_frametbl( char *block_name, char *RTtype )
{
	static Arr *frametables = 0;
	Pf	*pf;
	Tbl	*frames;
	Tbl	*siterows;
	siteinfo *sinfo[6];
	char	key[STRSZ];
	int	i;

	sprintf( key, "%s:%s", block_name, RTtype );

	if( frametables == 0 )
	{
		frametables = newarr( 0 );
	}
	else if( ( frames = (Tbl *) getarr( frametables, key ) ) != 0 )
	{
		return frames;
	}

	pf = get_pf( block_name );

	siterows = pfget_tbl( pf, RTtype );
	frames = sitetbl2frametbl( siterows );
	freetbl( siterows, 0 );
	setarr( frametables, key, (void *) frames );

	sorttbl( frames, cmp_sinfo_order, 0 );

	if( STREQ( RTtype, "LPRT" ) )
	{
		if( maxtbl( frames ) % 3 != 0 )
		{
			fprintf( stderr, "Wrong # of components in LPRT table\n" );
			return 0;
		}
		for( i=0; i < maxtbl( frames ); i += 3 )
		{
			sinfo[0] = (siteinfo *) gettbl( frames, 0 );
			sinfo[1] = (siteinfo *) gettbl( frames, 1 );
			sinfo[2] = (siteinfo *) gettbl( frames, 2 );
			if( ! ( STREQ( sinfo[0]->sta, sinfo[1]->sta ) &&
			        STREQ( sinfo[1]->sta, sinfo[2]->sta ) ) )
			{
				fprintf( stderr, "Unexpected order in LPRT table\n" );
				return 0;
			}
		}
	}
	else if( STREQ( RTtype, "BBRT" ) )
	{
		if( maxtbl( frames ) % 6 != 0 )
		{
			fprintf( stderr, "Wrong # of components in BBRT table\n" );
			return 0;
		}
		for( i=0; i < maxtbl( frames ); i += 6 )
		{
			sinfo[0] = (siteinfo *) gettbl( frames, 0 );
			sinfo[1] = (siteinfo *) gettbl( frames, 1 );
			sinfo[2] = (siteinfo *) gettbl( frames, 2 );
			sinfo[3] = (siteinfo *) gettbl( frames, 3 );
			sinfo[4] = (siteinfo *) gettbl( frames, 4 );
			sinfo[5] = (siteinfo *) gettbl( frames, 5 );
			if( ! ( STREQ( sinfo[0]->sta, sinfo[1]->sta ) &&
			        STREQ( sinfo[1]->sta, sinfo[2]->sta ) &&
				STREQ( sinfo[2]->sta, sinfo[3]->sta ) &&
				STREQ( sinfo[3]->sta, sinfo[4]->sta ) &&
				STREQ( sinfo[4]->sta, sinfo[5]->sta ) ) )
			{
				fprintf( stderr, "Unexpected order in BBRT table\n" );
				return 0;
			}
		}
	}

	return frames;
}

int
printrow( void *sp, void *private )
{
        siteinfo *sinfo = (siteinfo *) sp;
 
        printf( "Data for frame:\n" );
        printf( "\tsta is %s\n", sinfo->sta );
        printf( "\tchan is %s\n", sinfo->chan );
        printf( "\tchanid is %d\n", sinfo->chid );
        printf( "\tsensid is %d\n", sinfo->sensid );
        printf( "\tsiteid is %d\n", sinfo->siteid );
        printf( "\tdnorth is %f\n", sinfo->dnorth );
        printf( "\tdeast is %f\n", sinfo->deast );
        printf( "\tcalib is %f\n", sinfo->calib );
        printf( "\tcalper is %f\n", sinfo->calper );
        printf( "\tinstype is %s\n", sinfo->instype );
        printf( "\tframeid is %s\n", sinfo->frameid );
        printf( "\torder is %d\n", sinfo->order );
 
        return 0;
}

int
cmp_sinfo_order( ap, bp, private )
void *ap;
void *bp;
void *private;
{
	siteinfo **a = (siteinfo **) ap;
	siteinfo **b = (siteinfo **) bp;
	Frameid	af, bf;
	int	cmp;
	static char band_order[] = { 's', 'l', 'b' };
	static char comp_order[] = { 'z', 'n', 'e' };

	expand_frameid( (*a)->frameid, &af );
	expand_frameid( (*b)->frameid, &bf );

	if( cmp = cmp_vs_ordered_triplet( band_order, af.band, bf.band ) )
	{
		return cmp;
	}
	else if( af.num != bf.num )
	{
		return af.num - bf.num;
	}
	else if( af.band == 'b' &&
		( cmp = cmp_vs_ordered_triplet( band_order, af.comp[0], bf.comp[0] ) ) )
	{
		return cmp;
	}
	else if( af.band == 'b' &&
		( cmp = cmp_vs_ordered_triplet( comp_order, af.comp[1], bf.comp[1] ) ) )
	{
		return cmp;
	}
	else if( cmp = cmp_vs_ordered_triplet( comp_order, af.comp[0], bf.comp[0] ) ) 
	{
		return cmp;
	}
	else
	{
		return 0;
	}
}

int
cmp_vs_ordered_triplet( char order[3], char a, char b )
{
	if( a == b ) return 0;
	else if( a == order[0] ) return -1;
	else if( b == order[0] ) return 1;
	else if( a == order[1] ) return -1;
	else if( b == order[1] ) return 1;
	fprintf( stderr, "Unexpected failure in cmp_vs_ordered_triplet()\n" );
	return 0;
}

void
expand_frameid( char frameid[5], Frameid *f )
{
	char	number[6], *np;
	int	i;

	f->band = frameid[0];
	np = &number[0];
	i = 1;
	while( isdigit( frameid[i] ) )
	{
		*(np++) = frameid[i++];	
	}
	*np = '\0';

	f->num = atoi( number );

	switch( f->band )
	{
	case 's':
		strcpy( f->comp, &frameid[ strlen( frameid )- 1 ] );
		break;
	case 'l':
		strcpy( f->comp, &frameid[ strlen( frameid )- 1 ] );
		break;
	case 'b':
		strcpy( f->comp, &frameid[ strlen( frameid )- 2 ] );
		break;
	default:
		strcpy( f->comp, "" );
		break;
	}	
}

Tbl *
sitetbl2frametbl( Tbl *sites )
{
	Tbl	*frames;
	siteinfo *sinfo;
	int	i;

	frames = newtbl( 0 );

	for( i = 0; i < maxtbl( sites ); i++ )
	{
		sinfo = parse_siterow( gettbl( sites, i ) );
		sinfo->order = i;
		pushtbl( frames, sinfo );
	}

	return frames;
}

siteinfo *
parse_siterow( char *siterow_in )
{
	siteinfo *sinfo;
	char	*siterow;
	Tbl	*fields;

	allot( char *, siterow, strlen( siterow_in ) + 1 );
	memcpy( siterow, siterow_in, strlen( siterow_in ) + 1 );

	strreplace( siterow, '\t', ' ' );
	fields = split( siterow, ' ' );

	allot( siteinfo *, sinfo, 1 );

	strcpy( sinfo->sta, shifttbl( fields ) );
	strcpy( sinfo->chan, shifttbl( fields ) );
	sinfo->chid = atoi( shifttbl( fields ) );
	sinfo->sensid = atoi( shifttbl( fields ) );
	sinfo->siteid = atoi( shifttbl( fields ) );
	sinfo->dnorth = atof( shifttbl( fields ) );
	sinfo->deast = atof( shifttbl( fields ) );
	sinfo->calib = atof( shifttbl( fields ) );
	sinfo->calper = atof( shifttbl( fields ) );
	strcpy( sinfo->instype, shifttbl( fields ) );
	strcpy( sinfo->frameid, shifttbl( fields ) );

	free( siterow );
	freetbl( fields, 0 );

	return sinfo;
}
