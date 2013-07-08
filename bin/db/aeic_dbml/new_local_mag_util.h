#include "stock.h"

typedef struct {
	char    sta[7];
	char    chan[9];
	long	arid;
	double	time;
	double	endtime;
	double	tmeas;
	double	PtoP_timediff;
	double	ZtoP_amp_nm;
	double  stamag;
	double  uncert;
} Smrpt;		/* Stamag report */

float mean( float *, int );
int demean( float *, int );
int extrema( float *, int, float *, float * );
int clip_check( float *, int, float, double );
int max_amp_ptp( float *, int, int, int *, int *, float * );
int next_higher_power_of_two( int );
int taper( float *, int, double );
int apply_Wood_Anderson_gain( float *, int );
int apply_flat_calib( float *, int, char *, char *, double, double );
int stamags_to_netmag( Tbl *, double *, double * );
int measure_local_mag( float *, double, int, double, double, double, char *, double, double, double, double, double, double, double *, double *, double *, double *, double * , char *);
