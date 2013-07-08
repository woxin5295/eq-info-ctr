#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "dbap_defines.h"

char	*dbin;

void read_fkgrid( Dbptr, Apspec *, double *, double * );
void myplot_fkgrid( Dbptr, Apspec * );

main( int argc, char **argv )
{
	Dbptr	db;
	Apspec	*grid;
	char	*table;
	double 	time;
	double	endtime;
	char	dir[FILENAME_MAX];

        if( argc != 2 ) {
                die( 1, "Usage: %s dbname\n", argv[0] );
        } else {
		allot( char *, Program_Name, STRSZ );
		dirbase( argv[0], dir, Program_Name );
                table = argv[1];
        }
 
        if (dbopen_database(table, "r+", &db)< 0)
                die(0, "Can't open %s\n", table ) ;

	if( strcmp( table, "-" ) ) {
		db = dblookup( db, 0, "fkgrid", 0, 0 );
	}
	if( db.record < 0 ) {
		db.record = 0;
	}

	allot( Apspec *, grid, 1 );
	read_fkgrid( db, grid, &time, &endtime );
	myplot_fkgrid( db, grid );

	exit( 0 );
}

void myplot_fkgrid( Dbptr db, Apspec *grid )
{
	int	itrans = 0;
	double	size = 0.5;
	double	dim = 6.0;
	char	*plotfile = NULL;
	int	animate = 0;
	int	stack = 0;
	int	indx = 0;
	double	thresh = 0.9;
	double	xlow = 0.5;
	double	ylow = 2.0;
	double	pmax = -1.0;
	int	ititle = 1;
	int	iaxis = 1;

	allot( char *, dbin, FILENAME_MAX );
	dbquery( db, dbDATABASE_NAME, &dbin );

	clear_register( 1 );
	xygrid_plot( grid, itrans, size, plotfile, animate, stack, 
		     indx, thresh, dim, xlow, ylow, pmax, 1, 1);
}

void read_fkgrid( Dbptr db, Apspec *grid, double *time, double *endtime )
{
	Channelspec *gchan;
	Gridnode *gn;
	char	fkgridfile[FILENAME_MAX];
	char	datatype[STRSZ];
	double	dx, dy;
	int	ix, iy;
	int	size;
	int	nread;
	int	fd;

	allot( Channelspec *, gchan, 1 );
	grid->chans = newtbl( 0 );
	pushtbl( grid->chans, gchan );

	dbgetv( db, 0, 
		"sta", grid->array,
		"refsta", grid->refsta,
		"chan", grid->chan,
		"time", &time,
		"endtime", &endtime,
		"twin", &grid->twin,
		"filter", &gchan->filter,
		"dtime", &grid->dt,
		"nt", &grid->nt,
		"azimuth", &grid->summary_grid.peak_azimuth,
		"slo", &grid->summary_grid.peak_slow,
		"slowd", &grid->summary_grid.slow_width,
		"ppower", &grid->summary_grid.peak_power,
		"semin", &grid->xmin,
		"semax", &grid->xmax,
		"ne", &grid->nx,
		"snmin", &grid->ymin,
		"snmax", &grid->ymax,
		"nn", &grid->ny,
		"datatype", datatype,
		0 );
	
	grid->grid_nodes = newtbl( 0 );

	allot( float *, grid->x, grid->nx );
	allot( float *, grid->y, grid->ny );

	dx = ( grid->xmax - grid->xmin ) / ( grid->nx - 1 );
	dy = ( grid->ymax - grid->ymin ) / ( grid->ny - 1 );
	
	for( iy = 0; iy < grid->ny; iy++ ) {
		for( ix = 0; ix < grid->nx; ix++ ) {

			allot( Gridnode *, gn, 1 );

			gn->sx = grid->xmin + dx * ix;
			gn->sy = grid->ymin + dy * iy;
			gn->grid = 0;

			pushtbl( grid->grid_nodes, gn );

			grid->x[ix] = grid->xmin + dx * ix;
			grid->y[iy] = grid->ymin + dy * iy;
		}
	}

	if( strcmp( datatype, "t4" ) ) {
		die( 1, "%s handles datatype %s only.\n", 
			Program_Name, datatype );
	}
	
	dbfilename( db, fkgridfile );
	fd = open( fkgridfile, O_RDONLY );
	if( fd < 0 ) {
		die( 1, "Error opening fkgrid file %s\n", fkgridfile );
	}
	size = sizeof( float ) * grid->nx * grid->ny;

	allot( float *, grid->norm_summary_grid.power, grid->nx * grid->ny );
	nread = read( fd, grid->norm_summary_grid.power, size );
	if( nread < size ) {
		die( 1, "Failed to read fkgrid file %s\n", fkgridfile );
	}

	close( fd );
}
