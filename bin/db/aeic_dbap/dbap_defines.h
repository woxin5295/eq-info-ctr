
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

#include "db.h"
#include "scv2.h"
#include "arrays.h"
#include "coords.h"

typedef struct grid_ {
	float *power;
	double peak_power;
	double peak_azimuth;
	double peak_slow;
	double slow_width;
	double tstart;
	double tend;
} Grid;

typedef struct spec_ {
	int nf;
	int orid;
	double azimuth;
	double slow;
	float df;
	float dt;
	float ts;
	float tshift;
	float acor;
	float *fftr;
	float *ffti;
	float *fftt;
	char phase[32];
} Spec;

typedef struct apspec_ {
	char array[16];
	char refsta[16];
	char chan[16];
	struct apspec_ *gather;
	double tpad;
	double samprate;
	double ts;
	double te;
	int type;
	int nsta;
	int savegrids;
	int summary;
	int nx;
	int ny;
	double xmin;
	double xmax;
	double ymin;
	double ymax;
	double fmin;
	double fmax;
	double azimuth;
	double slowi;
	float *x;
	float *y;
	int ns;
	double smin;
	double smax;
	int nt;
	double t0;
	double dt;
	double twin;
	float *t;
	float *s;
	float *norm_power_grid;
	float *power_grid;
	double az;
	double slow;
	double width;
	double sss2n;
	double bms2n;
	double gridratio;
	double az_grid;
	double slow_grid;
	double width_grid;
	double grid;
	double tshift_min;
	double tshift_max;
	Trace *grid_power;
	Trace *grid_raw_power;
	Trace *grid_raw_az;
	Trace *grid_raw_sl;
	Trace *tr;
	Tbl *grid_nodes;
	Tbl *chans;
	Tbl *grids;
	Grid norm_summary_grid;
	Grid summary_grid;
} Apspec;

typedef struct gridnode_ {
	double sx;
	double sy;
	double grid;
	double gridratio;
} Gridnode;

typedef struct chanspec_ {
	char sta[16];
	char chan[16];
	double wt;
	double tcor;
	double acor;
	double tshift;
	double dx;
	double dy;
	char *gap;
	char *filter;
	Trace *tr;
	Trace *power_tr;
	Spec *sp;
} Channelspec;

#define	XYGRID		1
#define	STGRID		2
#define	FTGRID		3
#define	BEAM		4
#define	CROSSCOR	5
#define	GATHER		6

#define	PROCESS	5
#define	GET	6

#define	STACK	7
#define	BEST	8

/* $Id: dbap_defines.h,v 1.1 2001-06-15 00:18:29 kent Exp $ */
