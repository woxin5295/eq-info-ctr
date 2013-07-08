#include "math.h" 
#include "tr.h"  
#include "db.h" 
#include "coords.h" 
#include "stock.h" 
#include "string.h"

#include "ak_ahheader.h" 

static void get_null_ak_ahhead( ak_ahhed * );
static void h2n_ak_ahhead( ak_ahhed * );

static int 
aah_datatype_to_size( short datatype )
{
    int datasize;

    switch( datatype ) {

    	case( ak_ahSHORT ):
		datasize = 2;
		break;
	
    	case( ak_ahFLOAT ):
    	case( ak_ahLONG ):
		datasize = 4;
		break;
	
    	case( ak_ahDOUBLE ):
    	case( ak_ahCOMPLEX ):
		datasize = 8;
		break;
    	default:
		datasize = 0;
		break;
    }

    return datasize;
}

int
wfinAAH ( char *buffer, int nbytes, int first, int nsamp, float *dest ) 
{
    ak_ahhed ah;
    char *bufptr = buffer;
    int i, retcode ;
    int datasize;
    short datatype;
    union { 
	int i;
	short s;
	float f;
	double d;
	complex c;
    } u;

    memcpy( &ah.station, bufptr, 520 );
    bufptr += 520;
    memcpy( &ah.event, bufptr, 22 );
    bufptr += 22;
    memcpy( &ah.event.ot.sec, bufptr, 86 );
    bufptr += 86;
    memcpy( &ah.record.ndata, bufptr, 22 );
    bufptr += 22;
    memcpy( &ah.record.abstime.sec, bufptr, 290 );
    bufptr += 290;
    memcpy( &ah.extra, bufptr, 84 );
    bufptr += 84;

    /* non-XDR AH is the same as Network (SPARC/Motorola) byte order: */

    N2H2( &datatype, &ah.record.type, 1 );

    datasize = aah_datatype_to_size( datatype );
    if( datasize <= 0 ) {
    	register_error (0, "Unrecognized type in non-XDR ah header" ) ;
	return -7;
    }


    if ( AK_AHHEADSIZE + (nsamp+first)*datasize <= nbytes ) { 

	bufptr += datasize * first ; 

	for ( i=0 ; i<nsamp ; i++ ) { 

	    memcpy( &u, bufptr, datasize );
	    bufptr += datasize;

	    switch( datatype ) {
    		case( ak_ahSHORT ):
	    		N2H2( &u.s, &u.s, 1 );
			dest[i] = u.s;
			break;

    		case( ak_ahFLOAT ):
	    		N2H4( &u.f, &u.f, 1 );
			dest[i] = u.f;
			break;

    		case( ak_ahLONG ):
	    		N2H4( &u.i, &u.i, 1 );
			dest[i] = u.i;
			break;

    		case( ak_ahDOUBLE ):
	    		N2H8( &u.d, &u.d, 1 );
			dest[i] = u.d;
			break;

    		case( ak_ahCOMPLEX ):
	    		N2H4( &u.c.r, &u.c.r, 1 );
			dest[i] = u.c.r;
			break;

    		default:
    			register_error (0,
				"Unrecognized type in non-XDR ah header" ) ;
			return -7;
	    }	
	}
	retcode = 0 ;
    } else { 
	retcode = -6 ;
    }
    return retcode ;
}

#define OUTSIDE(X,LOWER,UPPER) ((X) <= LOWER || (X) >= UPPER)

/* int
* wfoutAAH ( Wfdisc *wfdisc, Trsample *data, int nsamp,
*               char **outbuf, int *nbytes, int *bufsz )
*/
int
wfoutAAH ( Wfdisc *wfdisc, Trsample *data, int nsamp,
               char **outbuf, long *nbytes, long *bufsz )
{
    int i, needed ;
    char *out ;
    union { 
	float f;
	char c[4] ; 
    } u ;
    float fill ;
    int datasize = 4;
    int flags = 0 ;
    float trunc ;
    float MISSING, lower, upper ;
    MISSING = trwftype("t4")->fill ;
    lower = wfdisc->wftype->lower ;  
    upper = wfdisc->wftype->upper ;  

    needed = nsamp * datasize ;
    RESIZE_BUFFER( char *, *outbuf, *bufsz, needed ) ;
    out = *outbuf ;

    fill = wfdisc->wftype->fill ;

    for ( i=0 ; i<nsamp ; i++ ) { 

	if ( data[i] == MISSING ) {

	    u.f = fill ;
	    flags |= TR_GAPS ;

	} else {

	    if ( OUTSIDE(data[i], lower, upper) ) {
		flags |= TR_CLIPPED ;
	    }

	    u.f = data[i] ;
	    H2N4( &u.f, &u.f, 1 );
	    memcpy( out, &u.c, datasize );
	    out += datasize;
	}

    }
    *nbytes = nsamp * datasize ;
    return flags ;
}

/*int 
*wfhdrAAH (Wfdisc *wfdisc, void *data, int nsamp, char **header, int *nbytes, int *headersz )
*/ 
int 
wfhdrAAH (Wfdisc *wfdisc, void *data, int nsamp, char **header, long *nbytes, long *headersz ) 
{
    ak_ahhed	    ah;
    double          slat, slon, elev;
    int		    retcode ;
    char 	   *out ;
    char	   *s;
    int		    i;
    float	   *fdata;
    float	    val;
    float	    maxamp = 0;

    fdata = (float *) data;
    for( i=0; i<nsamp; i++ ) {
	val = fdata[i];
	N2H4( &val, &val, 1 );
	maxamp = maxamp < fabs( val ) ? fabs( val ) : maxamp;
    }

    /* Should probably put the header in a hook to make nsamp 
       available, therefore make AH files appendable. Make sure in 
       that case to handle byte ordering of nsamp correctly. Also
       reset maxamp if necessary. */

    get_null_ak_ahhead( &ah );

    retcode = dbgetv (wfdisc->dbhdr, 0, 
	    		"lat", &slat,
	    		"lon", &slon,
	    		"elev", &elev,
	    		0);
    if( slat == -999 ) slat = 0;
    if( slon == -999 ) slon = 0;
    if( elev == -999 ) elev = 0;

    /* Omitted: filling in ah.station.DS, ah.station.A0, ah.station.calib
       calibration information; filling in ah.event from dbhdr pointer */

    strcpy( ah.station.code, wfdisc->sta );
    strcpy( ah.station.chan, wfdisc->chan );
    ah.station.slat = slat;
    ah.station.slon = slon;
    ah.station.elev = elev;
    ah.record.type = ak_ahFLOAT;
    ah.record.ndata = wfdisc->nsamp;
    ah.record.delta = 1.0 / wfdisc->samprate;
    ah.record.maxamp = maxamp;
    ah.record.abstime.yr = (short) atoi( s=epoch2str( wfdisc->time, "%Y" ) );
    free( s );
    ah.record.abstime.mo = (short) atoi( s=epoch2str( wfdisc->time, "%m" ) );
    free( s );
    ah.record.abstime.day = (short) atoi( s=epoch2str( wfdisc->time, "%d" ) );
    free( s );
    ah.record.abstime.hr = (short) atoi( s=epoch2str( wfdisc->time, "%H" ) );
    free( s );
    ah.record.abstime.mn = (short) atoi( s=epoch2str( wfdisc->time, "%M" ) );
    free( s );
    ah.record.abstime.sec = atof( s=epoch2str( wfdisc->time, "%S.%s" ) );
    free( s );

    /* non-XDR AH is the same as Network (SPARC/Motorola) byte order: */

    h2n_ak_ahhead( &ah );

    SIZE_BUFFER ( char *, *header, *headersz, AK_AHHEADSIZE ) ;
    out = *header ;

    memcpy( out, &ah.station, 520 );
    out += 520;
    memcpy( out, &ah.event, 22 );
    out += 22;
    memcpy( out, &ah.event.ot.sec, 86 );
    out += 86;
    memcpy( out, &ah.record.ndata, 22 );
    out += 22;
    memcpy( out, &ah.record.abstime.sec, 290 );
    out += 290;
    memcpy( out, &ah.extra, 84 );
    out += 84;

    *nbytes = AK_AHHEADSIZE;

    return retcode ;
}

static void
get_null_ak_ahhead( hed )
ak_ahhed	*hed;
{
	int	i;

	strcpy(hed->station.code,"null");
	strcpy(hed->station.chan,"null");
	strcpy(hed->station.stype,"null");
	hed->station.slat= 0.0;
	hed->station.slon= 0.0;
	hed->station.elev= 0.0;
	hed->station.DS= 0.0;
	hed->station.A0= 0.0;
	for(i=0; i< NOCALPTS; ++i)
	{
		hed->station.cal[i].pole.r= 0.0;
		hed->station.cal[i].pole.i= 0.0;
		hed->station.cal[i].zero.r= 0.0;
		hed->station.cal[i].zero.i= 0.0;
	}

	hed->event.lat= 0.0;
	hed->event.lon= 0.0;
	hed->event.dep= 0.0;
	hed->event.ot.yr= (short)0;
	hed->event.ot.mo= (short)0;
	hed->event.ot.day= (short)0;
	hed->event.ot.hr= (short)0;
	hed->event.ot.mn= (short)0;
	hed->event.ot.sec= 0.0;
	strcpy(hed->event.ecomment,"null");

	hed->record.type= (short)0;
	hed->record.ndata= 0L;
	hed->record.delta= 0.0;
	hed->record.maxamp= 0.0;
	hed->record.abstime.yr= (short)0;
	hed->record.abstime.mo= (short)0;
	hed->record.abstime.day= (short)0;
	hed->record.abstime.hr= (short)0;
	hed->record.abstime.mn= (short)0;
	hed->record.abstime.sec= 0.0;
	hed->record.rmin= 0.0;
	strcpy(hed->record.rcomment,"null");
	strcpy(hed->record.log,"null");

	for(i=0; i< NEXTRAS; ++i)
		hed->extra[i]= 0.0;

	return;
}

static void
h2n_ak_ahhead( hed )
ak_ahhed	*hed;
{
	int	i;

	H2N4( &hed->station.slat, &hed->station.slat, 1 );
	H2N4( &hed->station.slon, &hed->station.slon, 1 );
	H2N4( &hed->station.elev, &hed->station.elev, 1 );
	H2N4( &hed->station.DS, &hed->station.DS, 1 );
	H2N4( &hed->station.A0, &hed->station.A0, 1 );

	for(i=0; i< NOCALPTS; ++i)
	{
		H2N4( &hed->station.cal[i].pole.r, 
		      &hed->station.cal[i].pole.r, 1 );
		H2N4( &hed->station.cal[i].pole.i, 
		      &hed->station.cal[i].pole.i, 1 );
		H2N4( &hed->station.cal[i].zero.r, 
		      &hed->station.cal[i].zero.r, 1 );
		H2N4( &hed->station.cal[i].zero.i, 
		      &hed->station.cal[i].zero.i, 1 );
	}

	H2N4( &hed->event.lat, &hed->event.lat, 1 );
	H2N4( &hed->event.lon, &hed->event.lon, 1 );
	H2N4( &hed->event.dep, &hed->event.dep, 1 );

	H2N2( &hed->event.ot.yr, &hed->event.ot.yr, 1 );
	H2N2( &hed->event.ot.mo, &hed->event.ot.mo, 1 );
	H2N2( &hed->event.ot.day, &hed->event.ot.day, 1 );
	H2N2( &hed->event.ot.hr, &hed->event.ot.hr, 1 );
	H2N2( &hed->event.ot.mn, &hed->event.ot.mn, 1 );

	H2N4( &hed->event.ot.sec, &hed->event.ot.sec, 1 );

	H2N2( &hed->record.type, &hed->record.type, 1 );

	H2N4( &hed->record.ndata, &hed->record.ndata, 1 );
	H2N4( &hed->record.delta, &hed->record.delta, 1 );
	H2N4( &hed->record.maxamp, &hed->record.maxamp, 1 );

	H2N2( &hed->record.abstime.yr, &hed->record.abstime.yr, 1 );
	H2N2( &hed->record.abstime.mo, &hed->record.abstime.mo, 1 );
	H2N2( &hed->record.abstime.day, &hed->record.abstime.day, 1 );
	H2N2( &hed->record.abstime.hr, &hed->record.abstime.hr, 1 );
	H2N2( &hed->record.abstime.mn, &hed->record.abstime.mn, 1 );

	H2N4( &hed->record.abstime.sec, &hed->record.abstime.sec, 1 );

	H2N4( &hed->record.rmin, &hed->record.rmin, 1 );

	H2N4( &hed->extra, &hed->extra, NEXTRAS );

	return;
}
