#include "dbap_defines.h"

typedef struct region_ {
        int ixpeak;
        int iypeak;
        float peak;
        float xcentroid;
        float ycentroid;
        float size;
} Region;
 
#define RADIAL          1
#define TRANSVERSE      2
#define NORTH           3
#define EAST            4
 
#define INT(x)  ((x)<0.0?((x)-0.5):((x)+0.5))
#define VERBOSE (verbose > 1)
 
int
find4 (grid, z, thresh, ns, sx, sy, sr, pow)

Apspec *grid;
float *z;
double thresh;
int *ns;
double **sx;
double **sy;
double **sr;
double **pow;

{
	int i, n;
	int nr;
	double da, xscale, yscale;
	Region *regions;
	float gthresh;

	n = grid->nx*grid->ny;
	da = (grid->xmax-grid->xmin)*(grid->ymax-grid->ymin)/n;
	xscale = (grid->xmax-grid->xmin)/(grid->nx-1);
	yscale = (grid->ymax-grid->ymin)/(grid->ny-1);
	gthresh = thresh;
	find_region (grid->nx, grid->ny, z, gthresh, &nr, &regions);
	*ns = 0;
	*sx = NULL;
	*sy = NULL;
	*sr = NULL;
	*pow = NULL;
	if (nr == 0) {
		return;
	} else {
		*sx = (double *) my_malloc ("find4: *sx", nr*sizeof(double));
		if (*sx == NULL) return;
		*sy = (double *) my_malloc ("find4: *sy", nr*sizeof(double));
		if (*sy == NULL) return;
		*sr = (double *) my_malloc ("find4: *sr", nr*sizeof(double));
		if (*sr == NULL) return;
		*pow = (double *) my_malloc ("find4: *pow", nr*sizeof(double));
		if (*pow == NULL) return;
		*ns = nr;
		for (i=0; i<nr; i++) {
			(*sx)[i] = regions[i].xcentroid*xscale + grid->xmin;
			(*sy)[i] = regions[i].ycentroid*yscale + grid->ymin;
			(*sr)[i] = sqrt(da*regions[i].size)/M_PI;
			(*pow)[i] = regions[i].peak;
		}
		my_free (regions);
	}
}
