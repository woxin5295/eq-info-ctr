#include <stdlib.h>
#include "db.h"
#include "tr.h"
#include "stock.h"

int mydbgetwf( Dbptr, char *, char *, double, double, float **, long,
	       double *, double *, double *, long *, double, int (*fill)(), void * );

int
mydbgetwf(
	Dbptr	db,
	char	*sta,
	char	*chan,
	double	reqt0,
	double	reqt1,
	float	**data,
	long	nmax,
	double	*t0,
	double	*t1,
	double	*samprate,
	long	*npts,
	double	splice_tolerance,
	int	(*fill)(),
	void	*fill_private )
{
	Dbptr	tr;
	char	expr[STRSZ];
	long	nrecords;
	double	endtime;
	double	segt0, segt1;
	float	*segdata;
	long	segnmax;
	long	segnpts;
	double	mysamprate;
	double	calib;
	db = dblookup( db, 0, "wfdisc", 0, "dbALL" );

	/* Identify all segments overlapping the request window */
	sprintf( expr, "sta == \"%s\" && chan == \"%s\" &&", sta, chan );
	sprintf( expr, "%s ( ( time <= %f && endtime >= %f ) ||",
			expr, reqt0, reqt0 );
	sprintf( expr, "%s ( time <= %f && endtime >= %f ) )",
			expr, reqt1, reqt1 );
	
	db = dbsubset( db, expr, 0 );
	
	dbquery( db, dbRECORD_COUNT, &nrecords );

	if( (long) nrecords < (long) 1 ) {

		fprintf( stderr, "aeic_dbml: no data \n" );
		return -9;

	} else if( (long) nrecords == (long) 1 ) {

		db.record = 0;

		dbgetv( db, 0, "samprate", samprate, NULL );

		trgetwf( db, 0, data, &nmax, 
			reqt0, reqt1, 
			t0, t1, npts, fill, fill_private );

		return 0;

	} else {
		
		tr = trnew( 0, 0 );

		tr = dblookup( tr, 0, "trace", 0, 0 );

		for( db.record = 0; (long) db.record < (long) nrecords; db.record++ ) {

			dbgetv( db, 0, "samprate", &mysamprate,
				       "calib", &calib,
				       NULL );

			segdata = NULL;
			segnmax = 0;
			segnpts = 0;

			if( trgetwf( db, 0, &segdata, &segnmax, reqt0, reqt1,
				  &segt0, &segt1, &segnpts, 0, 0 ) == 0 ) { 

				endtime = ENDTIME( segt0, mysamprate, segnpts );

				tr.record = dbaddnull( tr );

				dbputv( tr, 0, 
					"evid", (long) -1,
					"arid", (long) -1,
					"net", "FAKE",
				        "sta", sta,
				        "chan", chan,
				        "time", segt0, 
				        "endtime", endtime, 
					"processid", (long) -1,
				        "nsamp", segnpts,
				        "samprate", mysamprate,
					"instype", "-" 
				        "calib", calib, 
				        "calper",(double) -1.0, 
				        "response", (long) 0, 
				        "datatype", "t4",
				        "segtype", "-",
				        "procflag", (long) 0, 
				        "data", segdata, 
				        "lat", (double)-999.0, 
				        "lon", (double)-999.0, 
				        "elev", (double)-999.0, 
				        "refsta", "-",
				        "dnorth", (double)0.0,
				        "deast", (double)0.0,
				        "edepth", (double)0.0,
				        "hang", (double)0.0,
				        "vang", (double)0.0,
				        "comment", "-",
				       NULL );
			}
		}

		tr.record = dbALL;

		trsplice( tr, splice_tolerance, 0, 0 );

		tr = dblookup( tr, 0, "trace", 0, "dbALL" );

		dbquery( tr, dbRECORD_COUNT, &nrecords );

		if( (long) nrecords != (long) 1 ) {
			fprintf( stderr, "aeic_dbml: splice failed for %s:%s!\n",
				sta, chan );
			trdestroy( &tr ); 
			return -1;
		}
		
		tr.record = 0;

		dbgetv( tr, 0, "data", data,
			       "samprate", samprate,
			       "time", t0,
			       "endtime", t1,
			       "nsamp", npts,
			       NULL );

		dbputv( tr, 0, "data", 0, NULL );
		trdestroy( &tr );

		return 0;
	}
}
