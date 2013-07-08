#include <stdlib.h>
#include <math.h>
/* #include <ieeefp.h> */
#include "stock.h"
#include "response.h"
#include "new_local_mag_util.h"

#define WOOD_ANDERSON_GAIN 2080.

#define TAPER_FRACT 0.05

#define FORWARD -1.0
#define INVERSE 1.0

#define TIME2SAMP(TIME,SAMPRATE,TIME1) (((TIME1)  -  (TIME))*(SAMPRATE) + 0.5)
#define SAMP2TIME(TIME,SAMPRATE,ISAMP) ((TIME) + ((ISAMP))  /(SAMPRATE))
#define STREQ(a, b) (strcmp((a), (b)) == 0)

extern char WoodAndersonResponseFile[];
extern int BandpassFilterOrder;
extern double BandpassUpperCutoff;
extern double BandpassLowerCutoff;

struct complex { float r, i; };

extern  cool_( int *, struct complex *, float * );

static struct complex mycmult( struct complex, struct complex );
static struct complex mycdiv( struct complex, struct complex );
static int bandpass_filter( struct complex *, int, double );
static int integrate( struct complex *, int, double );
static int convolve( char *, struct complex *, int, double );
static int deconvolve( char *, struct complex *, int, double );
static int stamag_OK( double );

int
pare_fir (Response *response)
{
    Response_group *gp ;
    int             i, j;

    for (i = 0; i < response->ngroups; i++) {
        gp = response->groups + i;
        if (gp->id == FIR ) {
	    for (j=i+1 ; j<response->ngroups ; j++) { 
		response->groups[i] = response->groups[j] ; 
	    }
	    response->ngroups-- ; 
	}
    }
    return 0;
}  

float
mean( float *data, int nsamp )
{
	int	index;
	float	sum;

	sum = 0;

	for( index = 0; index < nsamp; index++ )
	{
		sum += data[index];	
	}

	return( sum / nsamp );
}

int
demean( float *data, int nsamp )
{
	float 	avg;
	int	index;

	avg = mean( data, nsamp );
	for( index = 0; index < nsamp; index++ )
	{
		data[index] -= avg;
	}
	return( 0 );
}

int
extrema( float *data, int nsamp, float *min, float *max )
{
	int	index;

	*max = *min = 0.;

	for( index = 0; index < nsamp; index++ )
	{
		if( data[index] < *min ) *min = data[index];
		if( data[index] > *max ) *max = data[index];
	}

	return( 0 );
}
	
int
clip_check( float *data, int nsamp, float maxdef, double clip_thresh )
{
	float	min, max;
	float	clip_def;

	clip_def = clip_thresh * maxdef;

	extrema( data, nsamp, &min, &max );

	if( abs( max ) >= clip_def || abs( min ) >= clip_def )
	{
		return( 1 );
	}
	else 
	{
		return( 0 );
	}
}

int	
apply_Wood_Anderson_gain( float *data, int nsamp )
{
	int	index;

	for( index = 0; index < nsamp; index++ )
	{
		data[index] *= WOOD_ANDERSON_GAIN;
	}
	return( 0 );
}

int 
apply_flat_calib( float *data, int nsamp, char *flat_to, char *apply_as, 
		  double calib, double apply_at_cyclic_freq )
{
	int     index;
	double	angular_freq;
	double	rotated_calib;

	angular_freq = 2 * M_PI * apply_at_cyclic_freq;

	if( ! STREQ( flat_to, "D" ) &&
	    ! STREQ( flat_to, "V" ) &&
	    ! STREQ( flat_to, "A" ) ) return( -1 );

	if( ! STREQ( apply_as, "D" ) &&
	    ! STREQ( apply_as, "V" ) &&
	    ! STREQ( apply_as, "A" ) ) return( -1 );

	if( STREQ( flat_to, apply_as) )
	{
		rotated_calib = calib;
	}
	else if( STREQ( flat_to, "D" ) && STREQ( apply_as, "V" ) )
	{
		rotated_calib = calib * angular_freq;
	}
	else if( STREQ( flat_to, "V" ) && STREQ( apply_as, "A" ) )
	{
		rotated_calib = calib * angular_freq;
	}
	else if( STREQ( flat_to, "D" ) && STREQ( apply_as, "A" ) )
	{
		rotated_calib = calib * angular_freq * angular_freq;
	}
	else if( STREQ( flat_to, "V" ) && STREQ( apply_as, "D" ) )
	{
		rotated_calib = calib / angular_freq;
	}
	else if( STREQ( flat_to, "A" ) && STREQ( apply_as, "V" ) )
	{
		rotated_calib = calib / angular_freq;
	}
	else if( STREQ( flat_to, "A" ) && STREQ( apply_as, "D" ) )
	{
		rotated_calib = calib / ( angular_freq * angular_freq );
	}
	else
	{
		return( -1 );
	}

	for( index = 0; index < nsamp; index++ )
	{
		data[index] *= rotated_calib;
	}

	return( 0 );
}

int
stamags_to_netmag( Tbl *stamags, double *netmag, double *netmag_uncert )
{
	Smrpt	*stamag;
	int	nsta;
	int	nused;
	int	index;

	*netmag = -999.0;
	*netmag_uncert = -1.;

	nsta = maxtbl( stamags );

	if( nsta <= 0 ) 
	{
		return( 0 );
	}
	else
	{
		*netmag = 0.;
	}

	nused = 0;
	for( index = 0; index < nsta; index++ )
	{
		stamag = gettbl( stamags, index );
		if( stamag_OK( stamag->stamag ) )
		{
			*netmag += stamag->stamag;
			nused++;
		}
		else
		{
			deltbl( stamags, index );
		}
	}

	if( nused > 0 ) *netmag /= nused;

	return( 0 );
}

int
stamag_OK( double stamag )
{
	if( stamag == -99.99 ) return( 0 );
	else if( stamag == -999.0 ) return( 0 );
	if( ! finite( stamag ) ) return( 0 );
	else return( 1 );
}

int
next_higher_power_of_two( int i )
{
        return (int) pow( 2, ceil( log( (double) i ) / M_LN2 ) );
}

int	
deconvolve( char *respfile, struct complex *cdata, int nsamp, double samprate )
{
	Response *response;
	float	dfreq;
	float	omega;
	int	i, j;
	double	double_r, double_i;
	struct complex resp;
	FILE	*fp;

	if( respfile == NULL ) return( -1 );
	if( STREQ( respfile, "" ) ) return( -1 );
	if( ( fp = fopen( respfile, "r" ) ) == NULL )
	{
		return( -1 );
	}
	else
	{
		read_response( fp, &response );
	}

	dfreq = samprate / nsamp;

	/* Non-zero, non-Nyquist terms */
	for( i = 1, j = nsamp - 1; i < nsamp / 2; i++, j-- )
	{
		omega = 2 * M_PI * i * dfreq;

		pare_fir (response);
		eval_response( (double) omega, response,
				&double_r, &double_i );
		resp.r = double_r;
		resp.i = double_i;

		cdata[i] = mycdiv( cdata[i], resp );

		/* Wire in the conjugate for the negative frequencies */
		cdata[j].r = cdata[i].r;
		cdata[j].i = - cdata[i].i;

	}

	/* Nyquist term */
	i = nsamp / 2;
	omega = 2 * M_PI * samprate / 2;
	pare_fir (response);
	eval_response( (double) omega, response, &double_r, &double_i );
	resp.r = double_r;
	resp.i = double_i;
	
	cdata[i] = mycdiv( cdata[i], resp );

	free_response( response );
	fclose( fp );

	return( 0 );
}

int	
convolve( char *respfile, struct complex *cdata, int nsamp, double samprate )
{
	Response *response;
	float	dfreq;
	float	omega;
	int	i, j;
	double	double_r, double_i;
	struct complex resp;
	FILE	*fp;

	if( respfile == NULL ) return( -1 );
	if( STREQ( respfile, "" ) ) return( -1 );
	if( ( fp = fopen( respfile, "r" ) ) == NULL )
	{
		return( -1 );
	}
	else
	{
		read_response( fp, &response );
	}

	dfreq = samprate / nsamp;

	/* Non-zero, non-Nyquist terms */
	for( i = 1, j = nsamp - 1; i < nsamp / 2; i++, j-- )
	{
		omega = 2 * M_PI * i * dfreq;

		pare_fir (response);
		eval_response( (double) omega, response,
				&double_r, &double_i );
		resp.r = double_r;
		resp.i = double_i;

		cdata[i] = mycmult( cdata[i], resp );

		/* Wire in the conjugate for the negative frequencies */
		cdata[j].r = cdata[i].r;
		cdata[j].i = - cdata[i].i;

	}

	/* Nyquist term */
	i = nsamp / 2;
	omega = 2 * M_PI * samprate / 2;
	pare_fir (response);
	eval_response( (double) omega, response, &double_r, &double_i );
	resp.r = double_r;
	resp.i = double_i;

	cdata[i] = mycmult( cdata[i], resp );
	
	free_response( response );
	fclose( fp );

	return( 0 );
}

int	
integrate( struct complex *cdata, int nsamp, double samprate )
{
	float	dfreq;
	float	omega;
	int	i, j;
	struct complex iw;

	dfreq = samprate / nsamp;

	/* Don't divide the DC term by i-omega */
	for( i = 1, j = nsamp - 1; i < nsamp / 2; i++, j-- )
	{
		omega = 2 * M_PI * i * dfreq;
		iw.r = 0.;
		iw.i = omega;

		cdata[i] = mycdiv( cdata[i], iw );

		/* Wire in the conjugate for the negative frequencies */
		cdata[j].r = cdata[i].r;
		cdata[j].i = - cdata[i].i;

	}
	i = nsamp / 2;
	omega = 2 * M_PI * samprate / 2;
	iw.r = 0.;
	iw.i = omega;
	cdata[i] = mycdiv( cdata[i], iw );
	
	return( 0 );
}

int
bandpass_filter( struct complex *cdata, int nsamp, double samprate )
{
	int	i, j;
	int	filter_order;
	float	filter_freq;
	struct complex hs;
	float	freq;

	for( i = 1, j = nsamp - 1; i < nsamp / 2; i++, j-- )
	{
		freq = i * samprate / nsamp; 
		filter_freq = BandpassUpperCutoff;
		filter_order = BandpassFilterOrder; 

		bworth_( &freq, &filter_freq, &filter_order, &hs );

		cdata[i] = mycmult( cdata[i], hs );

		cdata[j].r = cdata[i].r;
		cdata[j].i = - cdata[i].i;

		filter_freq = BandpassLowerCutoff;
		filter_order = -1 * BandpassFilterOrder; 

		bworth_( &freq, &filter_freq, &filter_order, &hs );

		cdata[i] = mycmult( cdata[i], hs );

		cdata[j].r = cdata[i].r;
		cdata[j].i = - cdata[i].i;
	}

	i = nsamp / 2;			/* Nyquist */

	freq = i * samprate / nsamp;
	filter_freq = BandpassUpperCutoff;
	filter_order = BandpassFilterOrder;

	bworth_( &freq, &filter_freq, &filter_order, &hs );

	cdata[i] = mycmult( cdata[i], hs );

	filter_freq = BandpassLowerCutoff;
	filter_order = -1 * BandpassFilterOrder;

	bworth_( &freq, &filter_freq, &filter_order, &hs );

	cdata[i] = mycmult( cdata[i], hs );
}
 
int
cosine_taper( float *data, int nsamp, double fract )
{
	int	i, j;
	int	ntaper;
	float	taper;

	if( fract > 0.5 ) return( -1 );

	ntaper = nsamp * fract;

	for(i=0, j=nsamp - 1; i < ntaper; i++, j--)
	{
		taper = sin( M_PI * i / ( 2. * ntaper ) );
		data[i] *= taper;
		data[j] *= taper;
	}
	return( 0 );
}

int
max_amp_ptp( float *data, int nsamp, int parriv_index, 
	     int *i_amp, int *j_amp, float *maxamp )
{
	int	i;
	float	first_extremum, second_extremum;
	float	last_extremum;
	int	i_last_extremum;
	float	this_extremum;
	int	j_this_extremum;
	float	wkg_amp;
	int	big_i_amp, big_j_amp;
	float	big_amp;


	first_extremum = 0;
	second_extremum = 0;
	*maxamp = -1.;
	*i_amp = *j_amp = -1;
	big_amp = 0;
	big_i_amp = big_j_amp = 0;

	for( i = parriv_index; i < nsamp; i++ )
	{
		if( ( data[i] > data[i-1] && data[i] > data[i+1] ) ||
		    ( data[i] < data[i-1] && data[i] < data[i+1] ) )
		{
			first_extremum = data[i];
			big_i_amp = i;
			break;
		}
	}
	for( i = i + 1; i < nsamp; i++ )
	{
		if( ( data[i] > data[i-1] && data[i] > data[i+1] ) ||
		    ( data[i] < data[i-1] && data[i] < data[i+1] ) )
		{
			second_extremum = data[i];
			big_j_amp = i;
			break;
		}
	}

	big_amp = abs( second_extremum - first_extremum );

	last_extremum = second_extremum;
	i_last_extremum = big_j_amp;

	for( i = i + 1; i < nsamp; i++ )
	{
		if( ( data[i] > data[i-1] && data[i] > data[i+1] ) ||
		    ( data[i] < data[i-1] && data[i] < data[i+1] ) )
		{
			this_extremum = data[i];
			j_this_extremum = i;

			wkg_amp = abs( this_extremum - last_extremum );
			if( wkg_amp > big_amp )
			{
				big_amp = wkg_amp;
				big_i_amp = i_last_extremum;
				big_j_amp = j_this_extremum;
			}
			last_extremum = this_extremum;
			i_last_extremum = j_this_extremum;
		}

	}

	*i_amp = big_i_amp;
	*j_amp = big_j_amp;
	*maxamp = big_amp;

	return( 0 );
}

int
measure_local_mag( float *data_in, double ts, int nsamp, double samprate,
		     double calib, double calfreq, char *respfile,
		     double arrtime, double dist_km, double spreading,
		     double atten, double refdist, double refmag, 
		     double *ml, double *mlunc, double *tmeas, double *delta,
		     double *ZtoP_amp_nm, char *rsptype )
{
	float	*data;
	struct complex *cdata;
	int	nbytes_in;
	int	nbytes_padded;
	int	nsamp_padded;
	int	parriv_index;
	int	power;
	float	direction;
	int	i;
	float	maxamp;
	int	i_amp, j_amp;
	double	ZtoP_amp_mm;

	*ml = -99.99;
	*mlunc = -1.;
	*tmeas = *delta = 0.;
	*ZtoP_amp_nm = 0;
	i_amp = j_amp = -1;

	nbytes_in = nsamp * sizeof( float );
	nsamp_padded = next_higher_power_of_two( nsamp );
	nbytes_padded = nsamp_padded * sizeof( float );

	data = (float *) malloc( nbytes_padded );
	memset( data, '\0', nbytes_padded );
	memcpy( data, data_in, nbytes_in );

	apply_flat_calib( data, nsamp_padded, "V", "V", calib, calfreq );

	demean( data, nsamp );

	cosine_taper( data, nsamp_padded, TAPER_FRACT );

	cdata = (struct complex *)
			malloc( nsamp_padded * sizeof( struct complex ) );
	for( i = 0; i < nsamp_padded; i++ )
	{
		cdata[i].r = data[i];
		cdata[i].i = 0.;
	}

	power = log( nsamp_padded ) / M_LN2;
	direction = FORWARD;
	cool_( &power, cdata, &direction ); 

	deconvolve( respfile, cdata, nsamp_padded, samprate );
	if ( ! STREQ( rsptype, "D" ) ) {
	  if ( STREQ( rsptype, "A" ) ) {
	    integrate( cdata, nsamp_padded, samprate );
	  }
	  /* not D  */
	  integrate( cdata, nsamp_padded, samprate );
	}	

	bandpass_filter( cdata, nsamp_padded, samprate );

	convolve( WoodAndersonResponseFile, cdata, nsamp_padded, samprate );

	direction = INVERSE;
	cool_( &power, cdata, &direction );

	for( i = 0; i < nsamp; i++ )
	{
		data[i] = cdata[i].r / nsamp_padded;
	}

	parriv_index = TIME2SAMP( ts, samprate, arrtime );
	if( parriv_index < 0 || parriv_index >= nsamp - 1 )
	{
		free( cdata );
		free( data );
		return( -1 );
	}

	max_amp_ptp( data, nsamp, parriv_index, &i_amp, &j_amp, &maxamp );

	*tmeas = SAMP2TIME( ts, samprate, i_amp );
	*delta = 2 * ( SAMP2TIME( ts, samprate, j_amp ) - *tmeas );

	*ZtoP_amp_nm = maxamp / 2.;

	ZtoP_amp_mm = *ZtoP_amp_nm * 1e-6;

	*ml = log10( ZtoP_amp_mm ) + spreading * log10( dist_km / refdist ) +
		atten * ( dist_km - refdist ) + refmag;

	free( cdata );
	free( data );
	return( 0 );
}

/* cmult and cdiv modified from Dan Quinlan's original */

struct complex mycmult ( a, b )
struct complex a, b ;
{
    struct complex r ;
    r.r = a.r * b.r - a.i * b.i ;
    r.i = a.r * b.i + a.i * b.r ;
    return r ;
}
 
struct complex mycdiv ( a, b )
struct complex a, b ;
{
    struct complex r, conjugate ;
    float mag2 ;
 
    conjugate.r = b.r ;
    conjugate.i = -b.i ;
    r = mycmult ( a, conjugate ) ;
    mag2 = b.r * b.r + b.i * b.i ;
    r.r /= mag2 ;
    r.i /= mag2 ;
    return r ;
}

