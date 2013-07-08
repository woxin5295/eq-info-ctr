#include <unistd.h>
 
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

#define UNUSED  0
#define IN      1
#define OUT     2
#define NEXT    3
 
int rgncmp();
  
int
find_region (nx, ny, grid, gthresh, nr, regions)

int          nx, ny;
float *              grid;
float                      gthresh;
int *                               nr;
Region **                               regions;

/* find_region will find contiguous regions on a gridded
   surface above a specified threshold value */

{
	int *state;
	int i;
	Region *regns;
	float total;
	int ix, iy, nproc;

	state = (int *) my_malloc ("find_region: state",
					nx*ny*sizeof(int));
	if (state == NULL) {
		fprintf (stderr, "find_region: malloc() error.\n");
		return (0);
	}
	for (i=0; i<nx*ny; i++) {
		state[i] = UNUSED;
		if (grid[i] < gthresh) state[i] = OUT;
	}

	*nr = 0;
	regns = NULL;
	while (1) {
		for (i=0; i<nx*ny; i++) if (state[i] == UNUSED) break;
		if (i == nx*ny) break;
		if (regns == NULL) {
			regns = (Region *) my_malloc ("find_region: regns",
						sizeof(Region));
			if (regns == NULL) {
				fprintf (stderr, "find_region: malloc() error.\n");
				my_free (state);
				return (0);
			}
		} else {
			regns = (Region *) my_realloc ("find_region: regns",
							regns, (*nr+1)*sizeof(Region));
			if (regns == NULL) {
				fprintf (stderr, "find_region: realloc() error.\n");
				*nr = 0;
				my_free (state);
				return (0);
			}
		}
		regns[*nr].peak = -1.e30;
		regns[*nr].xcentroid = 0.0;
		regns[*nr].ycentroid = 0.0;
		regns[*nr].size = 0.0;
		total = 0.0;
		state[i] = NEXT;
		while (1) {
			nproc = 0;
			for (iy=0,i=0; iy<ny; iy++) {
				for (ix=0; ix<nx; ix++,i++) {
					if (state[i] != NEXT) continue;
					if (ix > 0 && state[i-1] == UNUSED) {
						state[i-1] = NEXT;
					}
					if (ix < nx-1 && state[i+1] == UNUSED) {
						state[i+1] = NEXT;
					}
					if (iy > 0 && state[i-nx] == UNUSED) {
						state[i-nx] = NEXT;
					}
					if (iy < ny-1 && state[i+nx] == UNUSED) {
						state[i+nx] = NEXT;
					}
					state[i] = IN;
					nproc++;
					if (grid[i] > regns[*nr].peak) {
						regns[*nr].peak = grid[i];
						regns[*nr].ixpeak = ix;
						regns[*nr].iypeak = iy;
					}
					regns[*nr].size += 1.0;
					total += grid[i];
					regns[*nr].xcentroid += grid[i]*((float)ix);
					regns[*nr].ycentroid += grid[i]*((float)iy);
				}
			}
			if (nproc == 0) {
				regns[*nr].xcentroid /= total;
				regns[*nr].ycentroid /= total;
				break;
			}
		}
		(*nr)++;
	}
	*regions = regns;
	my_free (state);
	if (*nr == 0) return (1);
	qsort (regns, *nr, sizeof(Region), rgncmp);

	/* Normal exit. */

	return (1);
}

int
rgncmp (ptr1, ptr2)
 
Region *ptr1, *ptr2;
 
{
        if (ptr1->peak > ptr2->peak) return (-1);
        else if (ptr1->peak < ptr2->peak) return (1);
        else return (0);
}
