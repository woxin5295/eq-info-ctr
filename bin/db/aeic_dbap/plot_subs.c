
#include "dbap_defines.h"

char *dbin;
Tbl *sc;
Dbptr dbi;

int has_plotted=0;

char grid_sta[32] = "";
char grid_chan[32] = "";
double grid_tstart = -999.0;
double grid_tend;

int
antelope_init_plot (itran, size, fname, dbname)

int itran;
double size;
char *fname;
char *dbname;

{
	char progname[128];
	float ssize=0.95;
	float xwin=0.0;
	float ywin=0.0;
	char plotfile[256];
	char display[16];
	char program[1024];
	static int nplot=1;
        float xplt, yplt;
        float angle=0.0;
        int iclip=0;
        int iref=5;
        float height=0.06;
        float ratio=1.0;
        float slant=0.0;
        int jfont=114; 
	float xdim, ydim, xlow, ylow;
	float xmin, xmax, ymin, ymax;
	float thick=0.0;
	int ithick=0;
	float x1, y1, x2, y2;
	float hue, light, sat;
	float fac;
	long itime;
	int i;

	strcpy(progname, "dbap");
	if (has_plotted) return;
	has_plotted = 1;
 	if (fname) {
 		if (fname[0]) {
 			strcpy (plotfile, fname);
		} else {
			sprintf (plotfile, "%s.%s.%d.ps", progname, dbname, nplot++);
		}
	} else {
		sprintf (plotfile, "none");
	}
	strcpy (display, " ");
	strcpy (program, progname);
	if (size > 0.0) ssize = size;
	initt_ (&itran, plotfile, display, program, &ssize,
		&xwin, &ywin, strlen(plotfile), strlen(display),
		strlen(program));
	if (itran == 0) {
		ydim = 10.0;
		xdim = 7.5;
		xlow = 0.0;
		ylow = 0.0;
		xmin = 0.0;
		xmax = 1.0;
		ymin = 0.0;
		ymax = 1.0;
	} else {
		xdim = 10.0;
		ydim = 7.5;
		xlow = 0.0;
		ylow = 0.0;
		xmin = 0.0;
		xmax = 1.0;
		ymin = 0.0;
		ymax = 1.0;
	}
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	iclip = 1;
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	if (itran == 0) {
		ydim = 9.8;
		xdim = 7.3;
		xlow = 0.1;
		ylow = 0.1;
		ymin = 0.0;
		ymax = 9.8;
		xmin = 0.0;
		xmax = 7.3;
	} else {
		xdim = 9.8;
		ydim = 7.3;
		xlow = 0.1;
		ylow = 0.1;
		xmin = 0.0;
		xmax = 9.8;
		ymin = 0.0;
		ymax = 7.3;
	}
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
        xplt = 0.0;
        yplt = 0.0;
	jfont = 113;
	height = 0.10;
	iref = 0;
        cfont_ (&jfont);
        chrsiz_ (&height, &ratio, &slant);
        text_ (&xplt, &yplt, &angle, &iref, "BRTT", &iclip, strlen("BRTT"));
	jfont = 115;
        cfont_ (&jfont);
        itime = time(NULL);
	sprintf (program, "%s: %s %s %s %s", progname, dbname, plotfile, cuserid(NULL), ctime(&itime));
	program[strlen(program)-1] = '\0';
	xplt = 0.5;
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
}

int
antelope_finit ()

{
	has_plotted = 0;
	finitt_ ();
}

int
grid_plot (iplot, npl, grid, plotfile, t0, twin, t0b, twinb, plot)

int        iplot;
int        npl;
Apspec *        grid;
char *                  plotfile;
double                         t0;
double                             twin;
double                                   t0b;
double                                       twinb;
int                                                         plot;

{
	float xdim=2.1, ydim=2.1, ssize=0.9, xwin=0.0, ywin=0.0, xmarg=0.3, ymarg=0.4, xlow=0.3, ylow=2.7;
	float xmax, xmin, ymax, ymin, thick=0.0;
	float dysmal=0.02, dynumb=0.1;
	float dxsmal=0.02, dxnumb=0.1;
	float *z;
	float zmin, zmax, dz;
	Gridnode *node, *nodemax;
	Channelspec *gridchan;
        float fzero=0.0;
        float fmone= -1.0;
        float fone= 1.0;
        float fpt02=0.02;
        float fpt7=0.7;
        float fhalf=0.5;
        float x1, x2, y1, y2;
        float scale = 0.5;
        int izero=0;
	int itran=0, iclear=0, n, igraf=0, iclip=0, ithick=0;
	static char display[] = " ";
	static char program[] = "dbap:grid_plot";
	static char fmtx[] = "(none)";
	static char fmty[] = "(none)";
	static char labelx[] = " ";
	static char labely[] = " ";
	static char labely2[] = "Db";
	static char ctype[] = "colors";
	static char title[1024];
	static char string[1024];
	int jfont=130;
	int i;
	int iref;
	char v[32];
	char v2[32];
	double az;
	double az2;
	double r, rmax;
	double sx, sy, sr;
	double sss2n, bms2n;
	int qpl;
	int nchans;
	float ybot, ytop;
	float xtdim = 7.2;
	float xtlow = 0.2;
	float ytdim = 0.392;
	float ytlow = 5.1;
	float height = 0.1;
	float ratio = 1.0;
	float slant = 0.0;
	Trace *tr;

	n = grid->nx*grid->ny;
	z = (float *) my_malloc ("grid_plot: z", n*sizeof(float));
	if (z == NULL) {
		fprintf (stderr, "grid_plot: Malloc error.\n");
		return (0);
	}
	zmin = 0.0;
	zmax = 0.0;
	dz = 0.0;
	for (i=0; i<n; i++) {
		node = (Gridnode *) gettbl (grid->grid_nodes, i);
		z[i] = node->gridratio;
		if (z[i] > zmax) {
			zmax = z[i];
			nodemax = node;
		}
	}
	zmin = 1.0 / grid->nsta;
	zmin = 0.0;
	dz = 0.1;
	xmin = grid->xmin;
	xmax = grid->xmax;
	ymin = grid->ymin;
	ymax = grid->ymax;
	rmax = sqrt (xmax*xmax + ymax*ymax);
	strcpy (title, " ");
	if (iplot == 0) {
		antelope_init_plot (itran, ssize, plotfile, dbin);
	} else if (iplot > 5 || (iplot > 0 && plot == 2)) {
		my_free (z);
		find1 (grid, 0, 0.90*zmax, &sx, &sy, &sr);
		if (sx != 0.0 || sy != 0.0) {
			az2 = atan2 (sx, sy);
			az2 *= 180.0/M_PI;
			while (az2 < 0.0) az2 += 360.0;
			grid->az = az2;
			grid->slow = sqrt(sx*sx + sy*sy);
			grid->width = sr;
			grid->gridratio = zmax;
		} else {
			az2 = 0.0;
			grid->az = az2;
			grid->slow = 0.0;
			grid->width = sr;
			grid->gridratio = zmax;
		}
		return;
	} else {
		if (iplot < 3) {
			xlow += iplot*(xdim + 0.3);
		} else {
			xlow += (iplot-3)*(xdim + 0.3);
			ylow -= ydim+0.3;
		}
	}
	if (plot == 2) {
		xlow = 0.5;
		ylow = 0.5;
	}
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	sclsiz_ (&scale);
	set_ctable();
	ncontour_ (&grid->nx, &grid->nx, &grid->ny, grid->x, grid->y, z,  ctype,  &zmin,  &fone,
	                    &dz, strlen(ctype));
	my_free (z);
	find1 (grid, 0, 0.90*zmax, &sx, &sy, &sr);
	if (nodemax->sx != 0.0 || nodemax->sy != 0.0) {
		az = atan2 (nodemax->sx, nodemax->sy);
		az *= 180.0/M_PI;
		while (az < 0.0) az += 360.0;
		sprintf (v, "%.2f", 1.0/sqrt(nodemax->sx*nodemax->sx + nodemax->sy*nodemax->sy));
	} else {
		az = 0.0;
		strcpy (v, "inf");
	}
	if (sx != 0.0 || sy != 0.0) {
		az2 = atan2 (sx, sy);
		az2 *= 180.0/M_PI;
		while (az2 < 0.0) az2 += 360.0;
		grid->az = az2;
		grid->slow = sqrt(sx*sx + sy*sy);
		grid->width = sr;
		grid->gridratio = zmax;
		sprintf (v2, "%.2f", 1.0/sqrt(sx*sx + sy*sy));
	} else {
		az2 = 0.0;
		grid->az = az2;
		grid->slow = 0.0;
		grid->width = sr;
		grid->gridratio = zmax;
		strcpy (v2, "inf");
	}
/*	cfont_ (&jfont);*/
/*        ngrid_ (&fmone, &fzero, &fhalf, &fzero, &izero,
                &fpt02, &fzero, &fpt7, &fzero, &izero,
                &fmone, &fzero, &fhalf, &fzero, &izero,
                &fpt02, &fzero, &fpt7, &fzero, &izero);*/
	setfor_ (&fzero, &fhalf, &fzero);
	for (r=0.1; r<=rmax; r+=0.1) mycircle (0.0, 0.0, r);
	x1 = xmin;
	y1 = 0.0;
	x2 = xmax;
	y2 = 0.0;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	y1 = ymin;
	x1 = 0.0;
	y2 = ymax;
	x2 = 0.0;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	setfor_ (&fzero, &fzero, &fzero);
	gridchan = (Channelspec *) gettbl (grid->chans, 0);
	strcpy (grid_sta, grid->refsta);
	strcpy (grid_chan, gridchan->chan);
	grid_tstart = t0b;
	grid_tend = t0b+twinb;
	sprintf (title, "%s %s %s az = %.2f, v = %s", grid->array, grid->refsta, gridchan->chan, az2, v2);
	axis_ (&xdim, &ydim,  &xmarg,  &ymarg,  &xlow,  &ylow,  &xmax,
				&xmin,  &ymax,  &ymin, &dxsmal, &dxnumb, &dysmal, &dynumb, fmtx, fmty, labelx, labely, title,
				&iclear, strlen(fmtx), strlen(fmty), strlen(labelx), strlen(labely), strlen(title));
	x1 = 0.0;
	y1 = 0.0;
	x2 = nodemax->sx;
	y2 = nodemax->sy;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = nodemax->sx;
	x2 = nodemax->sx;
	y1 = nodemax->sy-0.02*(ymax-ymin);
	y2 = nodemax->sy+0.02*(ymax-ymin);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	y1 = nodemax->sy;
	y2 = nodemax->sy;
	x1 = nodemax->sx-0.02*(xmax-xmin);
	x2 = nodemax->sx+0.02*(xmax-xmin);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	mycircle (sx, sy, sr);
	x1 = 0.0;
	y1 = 0.0;
	x2 = sx;
	y2 = sy;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = sx;
	x2 = sx;
	y1 = sy-0.02*(ymax-ymin);
	y2 = sy+0.02*(ymax-ymin);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	y1 = sy;
	y2 = sy;
	x1 = sx-0.02*(xmax-xmin);
	x2 = sx+0.02*(xmax-xmin);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	if (plot == 2) {
		scale = 1.0;
		sclsiz_ (&scale);
		return;
	}
	nchans = maxtbl(grid->chans);
	for (i=0; i<nchans; i++) {
		gridchan = (Channelspec *) gettbl (grid->chans, i);
		if (!strcmp(grid->refsta, gridchan->sta)) break;
	}
	qpl = qpbin (gridchan->tr->tstart, gridchan->tr->dt, gridchan->tr->nsamps, gridchan->tr->data, t0, twin, 4000, 0);
	y1 = ytdim*(11-iplot);
	nqplotsegs (qpl, xtdim, ytdim, xtlow, ytlow+y1, &ybot, &ytop, 1, 1, 1.0);
	qpfree (qpl);
	xmin = 0.0;
	xmax = twin;
	setscl_ (&xmin, &xmax, &ybot, &ytop);
	y1 = ybot;
	y2 = ytop;
	x1 = t0b-t0;
	x2 = t0b-t0;
	setfor_ (&fzero, &fhalf, &fone);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = t0b-t0+twinb;
	x2 = t0b-t0+twinb;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	setfor_ (&fzero, &fzero, &fzero);
	grid->sss2n = s2n(gridchan->tr, t0b-twinb, twinb, t0b, twinb);
	sprintf (string, "%.2f", grid->sss2n);
	x1 = 0.02*twin;
	y1 = ybot + 0.6*(ytop-ybot);
	iref = 0;
	chrsiz_ (&height, &ratio, &slant);
	text_ (&x1, &y1, &fzero, &iref, string, &iclip, strlen(string));
	tr = (Trace *) make_beam (grid->chans, sx, sy, t0, t0+twin);
	if (tr) {
		qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, t0, twin, 4000, 0);
		y1 = ytdim*(5-iplot);
		nqplotsegs (qpl, xtdim, ytdim, xtlow, ytlow+y1, &ybot, &ytop, 0, 1, 1.0);
		qpfree (qpl);
		xmin = 0.0;
		xmax = twin;
		setscl_ (&xmin, &xmax, &ybot, &ytop);
		y1 = ybot;
		y2 = ytop;
		x1 = t0b-t0;
		x2 = t0b-t0;
		setfor_ (&fzero, &fhalf, &fone);
		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
		x1 = t0b-t0+twinb;
		x2 = t0b-t0+twinb;
		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
		setfor_ (&fzero, &fzero, &fzero);
		grid->bms2n = s2n(tr, t0b-twinb, twinb, t0b, twinb);
		sprintf (string, "%.2f %.2f", grid->bms2n, grid->bms2n/grid->sss2n);
		x1 = 0.02*twin;
		y1 = ybot + 0.6*(ytop-ybot);
		iref = 0;
		text_ (&x1, &y1, &fzero, &iref, string, &iclip, strlen(string));
		SCV_free_trace (tr);
	}
	if (iplot == npl-1 || iplot == 5) finitt_ ();
}

int
xygrid_plot (grid, itrans, size, plotfile, animate, stack, indx, thresh, dim, xlow, ylow, pmax, ititle, iaxis)

Apspec *   grid;
int                itrans;
double                     size;
char *                           plotfile;
int                                        animate;
int                                                 stack;
int                                                        indx;
double                                                           thresh;
double                                                                   dim;
double                                                                        xlow;
double                                                                              ylow;
double                                                                                    pmax;
int                                                                                             ititle, iaxis;

{
	float xdim, ydim, ssize=0.9, xwin=0.0, ywin=0.0, xmarg=0.3, ymarg=0.4, xxlow, yylow;
	float xmax, xmin, ymax, ymin, thick=0.0;
	float dysmal=0.02, dynumb=0.1;
	float dxsmal=0.02, dxnumb=0.1;
	float *z;
	float zmin, zmax, zzmax, dz;
	Gridnode *node, *nodemax;
	Channelspec *gridchan;
        float fzero=0.0;
        float fmone= -1.0;
        float fone= 1.0;
        float fpt02=0.02;
        float fpt7=0.7;
        float fhalf=0.5;
        float x1, x2, y1, y2;
        float scale = 1.0;
        int izero=0;
	int iclear=0, igraf=0, iclip=0, ithick=0;
	static char display[] = " ";
	static char program[] = "dbap:grid_plot";
	static char fmtx[] = "(none)";
	static char fmty[] = "(none)";
	static char labelx[] = " ";
	static char labely[] = " ";
	static char labely2[] = "Db";
	static char ctype[] = "colors";
	static char title[1024];
	static char string[1024];
	int jfont=130;
	int i, j, n, nn;
	int iref;
	char v[32];
	char v2[32];
	double az;
	double az2;
	double r, rmax;
	double *sx, *sy, *sr;
	double sss2n, bms2n, *pow;
	int qpl;
	int nchans;
	int ns;
	Grid *grd;
	float height,ratio=1.0,slant=0.0;

	if (animate) {
		if (grid->savegrids == 0 || grid->grids == NULL) {
			fprintf (stderr, "xygrid_plot: Cannot animate without saving grids.\n");
			return (0);
		}
		n = maxtbl(grid->grids);
		if (n < 1) {
			fprintf (stderr, "xygrid_plot: No grids to animate.\n");
			return (0);
		}
		antelope_init_plot (itrans, size, plotfile, dbin);
		zmin = 0.0;
		dz = 0.1;
		zmax = 1.0;
		xmin = grid->xmin;
		xmax = grid->xmax;
		ymin = grid->ymin;
		ymax = grid->ymax;
		rmax = sqrt (xmax*xmax + ymax*ymax);
		xdim = dim;
		ydim = ABS((ymax-ymin)/(xmax-xmin))*dim;
		xxlow = xlow;
		yylow = ylow;
    		setdim_ (&xdim, &ydim, &xxlow, &yylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		set_ctable();
		if (stack) {
			nn = grid->nx*grid->ny;
			z = (float *) my_malloc ("xygrid_plot: z", nn*sizeof(float));
			if (z == NULL) {
				fprintf (stderr, "xygrid_plot: Malloc error.\n");
				return (0);
			}
			for (i=0; i<nn; i++) {z[i] = 0.0;}
			for (j=0; j<n; j++) {
				grd = (Grid *) gettbl(grid->grids, j);
				zmax = 0.0;
				for (i=0; i<nn; i++) {
					z[i] += grd->power[i];
					if (z[i] > zmax) zmax = z[i];
				}
				if (pmax > 0.0) zmax = pmax;
				dz = zmax * 0.1;
				ncontour_ (&grid->nx, &grid->nx, &grid->ny, grid->x, grid->y, 
					z,  ctype,  &zmin,  &zmax, &dz, strlen(ctype));
			}
			my_free (z);
		} else {
			for (i=0; i<n; i++) {
				grd = (Grid *) gettbl(grid->grids, i);
				if (pmax > 0.0) zmax = pmax;
				else if (pmax == 0.0) {
					nn = grid->nx*grid->ny;
					zmax = 0.0;
					for (i=0; i<nn; i++) {
						if (z[i] > zmax) zmax = z[i];
					}
				}
				dz = zmax * 0.1;
				ncontour_ (&grid->nx, &grid->nx, &grid->ny, grid->x, grid->y, 
					grd->power,  ctype,  &zmin,  &zmax, &dz, strlen(ctype));
			}
		}
		return (1);
	}
	n = grid->nx*grid->ny;
	z = (float *) my_malloc ("xygrid_plot: z", n*sizeof(float));
	if (z == NULL) {
		fprintf (stderr, "xygrid_plot: Malloc error.\n");
		return (0);
	}
	zmin = 0.0;
	zmax = 0.0;
	dz = 0.1;
	if (stack && grid->savegrids && grid->chans) {
		for (i=0; i<n; i++) {z[i] = 0.0;}
		nn = maxtbl(grid->grids);
		for (j=0; j<nn; j++) {
			grd = (Grid *) gettbl(grid->grids, j);
			for (i=0; i<n; i++) {z[i] += grd->power[i];}
		}
		for (i=0; i<n; i++) {
			node = (Gridnode *) gettbl (grid->grid_nodes, i);
			if (z[i] > zmax) {
				zmax = z[i];
				nodemax = node;
			}
		}
		zzmax = zmax;
		if (pmax > 0.0) zzmax = pmax;
		dz = 0.1*zzmax;
	} else {
		for (i=0; i<n; i++) {
			node = (Gridnode *) gettbl (grid->grid_nodes, i);
			z[i] = grid->norm_summary_grid.power[i];
			if (z[i] > zmax) {
				zmax = z[i];
				nodemax = node;
			}
		}
		zzmax = 1.0;
		if (pmax > 0.0) zzmax = pmax;
		else if (pmax == 0.0) zzmax = zmax;
		dz = 0.1*zzmax;
	}
	xmin = grid->xmin;
	xmax = grid->xmax;
	ymin = grid->ymin;
	ymax = grid->ymax;
	rmax = sqrt (xmax*xmax + ymax*ymax);
	strcpy (title, " ");
	antelope_init_plot (itrans, size, plotfile, dbin);
	xdim = dim;
	ydim = ABS((ymax-ymin)/(xmax-xmin))*dim;
	xxlow = xlow;
	yylow = ylow;
    	setdim_ (&xdim, &ydim, &xxlow, &yylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	sclsiz_ (&scale);
	set_ctable();
	ncontour_ (&grid->nx, &grid->nx, &grid->ny, grid->x, grid->y, z,  ctype,  &zmin,  &zzmax,
	                    &dz, strlen(ctype));
	find4 (grid, z, thresh*zmax, &ns, &sx, &sy, &sr, &pow);
	my_free (z);
	if (nodemax->sx != 0.0 || nodemax->sy != 0.0) {
		az = atan2 (nodemax->sx, nodemax->sy);
		az *= 180.0/M_PI;
		while (az < 0.0) az += 360.0;
		sprintf (v, "%.2f", 1.0/sqrt(nodemax->sx*nodemax->sx + nodemax->sy*nodemax->sy));
	} else {
		az = 0.0;
		strcpy (v, "inf");
	}
	if (indx < 0) indx = 0;
	if (indx >= ns) indx = ns-1;
	if (ns > 0 && (sx[indx] != 0.0 || sy[indx] != 0.0)) {
		az2 = atan2 (sx[indx], sy[indx]);
		az2 *= 180.0/M_PI;
		while (az2 < 0.0) az2 += 360.0;
		grid->az = az2;
		grid->slow = sqrt(sx[indx]*sx[indx] + sy[indx]*sy[indx]);
		grid->width = sr[indx];
		grid->gridratio = pow[indx];
		sprintf (v2, "%.2f", 1.0/grid->slow);
	} else {
		az2 = 0.0;
		grid->az = az2;
		grid->slow = 0.0;
		grid->width = 0.0;
		grid->gridratio = 0.0;
		strcpy (v2, "inf");
	}
	if (!ititle && !iaxis) {
		if (ns > 0) {
			my_free (sx);
			my_free (sy);
			my_free (sr);
			my_free (pow);
		}
		return;
	}
	setfor_ (&fzero, &fhalf, &fzero);
	for (r=0.1; r<=rmax; r+=0.1) mycircle (0.0, 0.0, r);
	x1 = xmin;
	y1 = 0.0;
	x2 = xmax;
	y2 = 0.0;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	y1 = ymin;
	x1 = 0.0;
	y2 = ymax;
	x2 = 0.0;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	setfor_ (&fzero, &fzero, &fzero);
	gridchan = (Channelspec *) gettbl (grid->chans, 0);
	strcpy (grid_sta, grid->refsta);
	strcpy (grid_chan, gridchan->chan);
	sprintf (title, "%s %s az = %.2f, v = %s", grid->array, gridchan->chan, az2, v2);
	/*axis_ (&xdim, &ydim,  &xmarg,  &ymarg,  &xxlow,  &yylow,  &xmax,
				&xmin,  &ymax,  &ymin, &dxsmal, &dxnumb, &dysmal, &dynumb, fmtx, fmty, labelx, labely, title,
				&iclear, strlen(fmtx), strlen(fmty), strlen(labelx), strlen(labely), strlen(title));*/
	setdim_ (&xdim, &ydim, &xxlow,  &yylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	iref = 3;
	iclip = 1;
	x1 = 0.5*(xmin+xmax);
	y1 = ymax + 0.05*(ymax-ymin)/ydim;
	height = 0.07;
	cfont_ (&jfont);
	chrsiz_ (&height, &ratio, &slant);
	text_ (&x1, &y1, &fzero, &iref, title, &iclip, strlen(title));
	iclip = 0;
	x1 = 0.0;
	y1 = 0.0;
	x2 = nodemax->sx;
	y2 = nodemax->sy;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = nodemax->sx;
	x2 = nodemax->sx;
	y1 = nodemax->sy-0.02*(ymax-ymin);
	y2 = nodemax->sy+0.02*(ymax-ymin);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	y1 = nodemax->sy;
	y2 = nodemax->sy;
	x1 = nodemax->sx-0.02*(xmax-xmin);
	x2 = nodemax->sx+0.02*(xmax-xmin);
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	for (i=0; i<ns; i++) {
		mycircle (sx[i], sy[i], sr[i]);
		if (i == indx) {
			x1 = 0.0;
			y1 = 0.0;
			x2 = sx[i];
			y2 = sy[i];
			line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
		}
		x1 = sx[i];
		x2 = sx[i];
		y1 = sy[i]-0.02*(ymax-ymin);
		y2 = sy[i]+0.02*(ymax-ymin);
		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
		y1 = sy[i];
		y2 = sy[i];
		x1 = sx[i]-0.02*(xmax-xmin);
		x2 = sx[i]+0.02*(xmax-xmin);
		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	}
	scale = 1.0;
	sclsiz_ (&scale);
	if (ns > 0) {
		my_free (sx);
		my_free (sy);
		my_free (sr);
		my_free (pow);
	}
}

int
mycircle (x0, y0, r)

double    x0;
double        y0;
double            r;

{
	static float xplt[361];
	static float yplt[361];
	int igraf = 0;
	int iclip = 0;
	float thick = 0.0;
	int ithick = 0;
	static char asymb[] = " ";
	double ang;
	int i, n=361;

	for (i=0; i<n; i++) {
		ang = i*M_PI/180.0;
		xplt[i] = x0 + r*sin(ang);
		yplt[i] = y0 + r*cos(ang);
	}
	nplot_ (&n, xplt, yplt, &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));
}

int
time_plot (grids, time)

Tbl *      grids;
double            time;

{
	Apspec *grid;
	int ngrids, igr;
	int i, j;
	static int nplt=0;
	static double t0;
	static float tplt[400];
	static float powplt[6][400];
	static float powpltt[400];
	int itran = 0;
	static char plotfile[] = "none";
	static char display[] = " ";
	static char program[] = "dbap:time_plot";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float xdim = 6.9;
	float ydim = 3.0;
	float xlow = 0.5;
	float ylow = 0.5;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	int igraf = 0;
	int iclip = 0;
	float thick = 0.0;
	int ithick = 0;
	static char asymb[] = " ";

	if (nplt == 0) {
		t0 = time;
	}
	if (nplt >= 400) {
		for (i=0; i<399; i++) {
			tplt[i] = tplt[i+1];
			powpltt[i] = powpltt[i+1];
			for (j=0; j<6; j++) {
				powplt[j][i] = powplt[j][i+1];
			}
		}
		nplt--;
	}
	ngrids = maxtbl (grids);
	for (igr=0; igr<ngrids&&igr<6; igr++) {
		grid = (Apspec *) gettbl (grids, igr);
		tplt[nplt] = time - t0;
		powplt[igr][nplt] = grid->gridratio;
		if (igr == 0) {
			powpltt[nplt] = grid->gridratio;
		} else {
			powpltt[nplt] *= grid->gridratio;
		}
	}
	nplt++;
	if (nplt < 2) return;
	initt_ (&itran, plotfile, display, program, &ssize, &xwin, &ywin,
	                                strlen(plotfile), strlen(display), strlen(program));
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	xmax = tplt[nplt-1];
	xmin = xmax - 400*(tplt[1]-tplt[0]);
	ymin = 0.0;
	ymax = 1.0;
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	hue = 0.0;
	light = 0.5;
	sat = 1.0;
	for (i=0; i<igr; i++) {
		setfor_ (&hue, &light, &sat);
		hue += 90.0;
		nplot_ (&nplt, tplt, powplt[i], &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));
	}
	hue = 0.0;
	light = 0.0;
	sat = 0.0;
	setfor_ (&hue, &light, &sat);
	ylow += ydim;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	xmax = tplt[nplt-1];
	xmin = xmax - 400*(tplt[1]-tplt[0]);
	ymin = 0.0;
	ymax = 1.0;
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	nplot_ (&nplt, tplt, powpltt, &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));
	finitt_ ();
}

int
plot_sgrid (grids, plotfile)

Tbl *       grids;
char *             plotfile;

{
	Apspec *  grid;
	int ngrids, igr;
	int itran = 0;
	char plotf[256];
	static char display[] = " ";
	static char program[] = "dbap:sgrid";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float ydimt = 1.5;
	float ydimg = 4.8;
	float fzero = 0.0;
	float xdim, ydim, xlow, ylow;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	float zmin, zmax, dz;
	float *z;
	int igraf = 0;
	int iclip = 0;
	int iclear = 0;
	int ifill = 0;
	float thick = 0.0;
	int ithick = 0;
	float size;
	static char asymb[] = " ";
	static char ctype[] = "colors";
	static char stype[] = "cross";
	static int instance=1;
	static float xsthick=0.0, xshue=0.0, xslight=0.0, xssat=0.0;
	static float xnthick=0.0, xnhue=0.0, xnlight=0.0, xnsat=0.0;
	static float ysthick=0.0, yshue=0.0, yslight=0.0, yssat=0.0;
	static float ynthick=0.0, ynhue=0.0, ynlight=0.0, ynsat=0.0;
	static int ixsstyl=0, ixnstyl=0, iysstyl=0, iynstyl=0;
	float dxsmal, dxnumb, dysmal, dynumb, xmarg, ymarg;
	float x, y;
	int iref;
	float height, ratio=1.0, slant=0.0;
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	char title[80];
	char string[80];
	char filter[80];
	int i, j;
	float velocity, slow, x1, x2, x3, x4, len;
	Channelspec *gchan;
	int qpl;
	float ybot, ytop;
	int jfont = 130;

	if (!has_plotted) initt_ (&itran, plotfile, display, program, &ssize, &xwin, &ywin,
	                                strlen(plotfile), strlen(display), strlen(program));

	ngrids = maxtbl(grids);
	if (ngrids > 3) ngrids = 3;

	for (igr=0; igr<ngrids; igr++) {

	grid = (Apspec *) gettbl (grids, igr);

	gchan = (Channelspec *) gettbl (grid->chans, 0);
	qpl = qpbin (gchan->tr->tstart, gchan->tr->dt, gchan->tr->nsamps, gchan->tr->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
	if (itran == 0) {
		xdim = 6.5;
		ydim = ydimt/ngrids;
		xlow = 0.5;
		ylow = 10.0 - 0.5 - ((float)(igr+1))*ydim;
	} else {
		xdim = 8.0;
		ydim = ydimt/ngrids;
		xlow = 1.0;
		ylow = 7.5 - 0.5 - ((float)(igr+1))*ydim;
	}
	anno_times (xdim, ydim, xlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
	nqplotsegs (qpl, xdim, ydim, xlow, ylow, &ybot, &ytop, 1, 1, 1.0);
	xmin = 0.0;
	xmax = xdim;
	ymin = 0.0;
	ymax = ydim;
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	cfont_ (&jfont);
	height = 0.05;
	chrsiz_ (&height, &ratio, &slant);
	for (i=0,j=0; i<strlen(gchan->filter); i++) {
		if (!isprint(gchan->filter[i])) {
			filter[j++] = ' ';
			continue;
		}
		if (gchan->filter[i] == '\t') filter[j++] = ' ';
		else if (gchan->filter[i] == '\n') filter[j] = ' ';
		else filter[j++] = gchan->filter[i];
	}
	filter[i] = '\0';
	sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
	x = 0.05;
	y = ydim - 0.05;
	iref = 2;
	text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
	if (igr == 0) {
		sprintf (string, "%s %s %s Azimuth = %.2f", grid->array, grid->refsta,
							gchan->chan, grid->azimuth);
		height = 0.1;
		chrsiz_ (&height, &ratio, &slant);
		iclip = 1;
		x = 0.5*xdim;
		y = ydim + 0.05;
		iref = 3;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
	}
	if (grid_tstart != -999.0) {
		if (!strcmp(grid->refsta, grid_sta) && !strcmp(gchan->chan, grid_chan)) {
			xmin = 0.0;
			xmax = grid->dt*(grid->nt-1);
			setscl_ (&xmin, &xmax, &ymin, &ymax);
			x2 = 0.25*ydim;
			x3 = 0.75*ydim;
			hue = 0.0;
			light = 0.5;
			sat = 1.0;
			setfor_ (&hue, &light, &sat);
			x1 = grid_tstart - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
			x1 = grid_tend - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
		}
	}

	if (itran == 0) {
		xdim = 6.5;
		ydim = ydimg/ngrids;
		xlow = 0.5;
		ylow = 10.0 - ydimt - 0.5 - (igr+1)*ydim;
	} else {
		xdim = 8.0;
		ydim = ydimg/ngrids;
		xlow = 1.0;
		ylow = 7.5 - ydimt - 0.5 - (igr+1)*ydim;
	}
	xmin = grid->t[0];
	xmax = grid->t[grid->nt-1];
	ymin = grid->smin;
	ymax = grid->smax;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	zmin = 0.0;
	zmax = 1.0;
	dz = 0.1;
	z = (float *) my_malloc ("plot_sgrid: z", grid->nt*grid->ns*sizeof(float));
	if (z == NULL) {
		fprintf (stderr, "plot_sgrid: malloc error.\n");
		finitt_ ();
		return (0);
	}
	for (i=0; i<grid->nt; i++) {
		for (j=0; j<grid->ns; j++) {
			z[i+j*grid->nt] = grid->norm_power_grid[j+i*grid->ns];
		}
	}
	set_ctable();
	ncontour_ (&grid->nt, &grid->nt, &grid->ns, grid->t, grid->s, z,  ctype,  &zmin,  &zmax,
	                    &dz, strlen(ctype));
	my_free (z);
	if (igr == ngrids-1) {
		anno_times (xdim, ydim, xlow, ylow, grid->t0, grid->dt*(grid->nt-1), 1);
	} else {
		anno_times (xdim, ydim, xlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
	}
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	line_ (&xmin, &fzero, &xmax, &fzero, &thick, &ithick, &iclip);
	iclip = 1;
	iref = 7;
	height = 0.07;
	x = xmin-(xmax-xmin)*0.05/xdim;
	y = 0.0;
	strcpy (string, "Inf");
	chrsiz_ (&height, &ratio, &slant);
	text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));

	for (i=0; i<19; i++) {
		velocity = 2.0+i;
		slow = 1.0/velocity;
		if (slow > ymax) continue;
		if (ymin >= 0.0) if (slow < ymin) continue;
		if (velocity == 5.0 || velocity == 10.0 || velocity == 15.0 || velocity == 20.0) {
			len = 0.2;
			x = xmin-(xmax-xmin)*0.05/xdim;
			y = slow;
			sprintf (string, "%.1f", velocity);
			line_ (&xmin, &slow, &xmax, &slow, &thick, &ithick, &iclip);
			if (velocity != 15.0) {
				text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
			}
			continue;
		} else {
			len = 0.1;
		}
		x1 = xmin;
		x2 = x1+(xmax-xmin)*len/xdim;
		x3 = xmax;
		x4 = x3-(xmax-xmin)*len/xdim;
		line_ (&x1, &slow, &x2, &slow, &thick, &ithick, &iclip);
		line_ (&x3, &slow, &x4, &slow, &thick, &ithick, &iclip);
	}
	for (i=0; i<19; i++) {
		velocity = 2.0+i;
		slow = 1.0/velocity;
		if (-slow < ymin) continue;
		if (velocity == 5.0 || velocity == 10.0 || velocity == 15.0 || velocity == 20.0) {
			len = 0.2;
			x = xmin-(xmax-xmin)*0.05/xdim;
			y = slow;
			sprintf (string, "%.1f", velocity);
			slow = -slow;
			line_ (&xmin, &slow, &xmax, &slow, &thick, &ithick, &iclip);
			if (velocity != 15.0) {
				text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
			}
			continue;
		} else {
			len = 0.1;
		}
		x1 = xmin;
		x2 = x1+(xmax-xmin)*len/xdim;
		x3 = xmax;
		x4 = x3-(xmax-xmin)*len/xdim;
		slow = -slow;
		line_ (&x1, &slow, &x2, &slow, &thick, &ithick, &iclip);
		line_ (&x3, &slow, &x4, &slow, &thick, &ithick, &iclip);
	}
	for (i=0; i<grid->nt; i++) {
		x2 = 0.0;
		for (j=0; j<grid->ns; j++) {
			x1 = grid->norm_power_grid[j+i*grid->ns];
			if (x1 > x2) {
				x2 = x1;
				y = grid->s[j];
			}
		}
		if (x2 < 0.5) continue;
		x = grid->t[i];
		size = 0.05;
		nsymbol_ (stype, &x, &y, &size, &thick, &iclip, &ifill, strlen(stype));
	}
	xmin = 0.0;
	xmax = xdim;
	ymin = 0.0;
	ymax = ydim;
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
	x = 0.05;
	y = ydim - 0.05;
	iref = 2;
	text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
	}
	xlow = xlow + xdim - 3.0;
	xdim = 3.0;
	ydim = 0.3;
	ylow = 0.6;
	nlegend (xdim, ydim, xlow, ylow, zmin, zmax, dz);

	finitt_ ();
}

int
stgrid_plot (grid, itrans, size, plotfile, xdim, xlow, ydimgr, ylowgr, ydimtr, ylowtr, 
			ydimbm, ylowbm, ydimov, ylowov, 
			ydimdf, ylowdf, pred_phases, orid, norm, ititle, itimelabel, ntimes, times, dbcat,
			autotr, autogr)

Apspec *   grid;
int                itrans;
double                     size;
char *                           plotfile;
double                                     xdim, xlow, ydimgr, ylowgr, ydimtr, ylowtr;
double                  ydimbm, ylowbm;
double                  ydimov, ylowov;
double                  ydimdf, ylowdf;
char *pred_phases;
int orid;
int norm;
int                                     ititle;
int                                             itimelabel;
int                                                         ntimes;
double *                                                            times;
char *                                                                     dbcat;
int                     autotr, autogr;

{
	char plotf[256];
	static char display[] = " ";
	static char program[] = "dbap:sgrid";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float ydimt = 1.5;
	float ydimg = 4.8;
	float fzero = 0.0;
	float xxdim, ydim, xxlow, ylow;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	float zmin, zmax, dz;
	float *z;
	int igraf = 0;
	int iclip = 0;
	int iclear = 0;
	int ifill = 0;
	float thick = 0.0;
	int ithick = 0;
	float fsize;
	static char asymb[] = " ";
	static char ctype[] = "colors";
	static char stype[] = "cross";
	static int instance=1;
	static float xsthick=0.0, xshue=0.0, xslight=0.0, xssat=0.0;
	static float xnthick=0.0, xnhue=0.0, xnlight=0.0, xnsat=0.0;
	static float ysthick=0.0, yshue=0.0, yslight=0.0, yssat=0.0;
	static float ynthick=0.0, ynhue=0.0, ynlight=0.0, ynsat=0.0;
	static int ixsstyl=0, ixnstyl=0, iysstyl=0, iynstyl=0;
	float dxsmal, dxnumb, dysmal, dynumb, xmarg, ymarg;
	float x, y;
	int iref;
	float height, ratio=1.0, slant=0.0;
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	char title[80];
	char string[80];
	char filter[80];
	int i, j;
	float velocity, slow, x1, x2, x3, x4, len;
	Channelspec *gchan;
	int qpl;
	float ybot, ytop;
	int jfont = 130;
	Trace *tr, *trb, *trp;
	double sx, sy;
	Tbl *grids;
	int nph;
	double *timep, *ps, *dtdhs;
	char **phs;
	double time;
	float data;
	Dbptr dbc;

	/* Initialize plot */

	antelope_init_plot (itrans, size, plotfile, dbin);

	/* Plot trace */

	ybot = 0.0;
	ytop = 0.0;
	if (ydimtr > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		tr = gchan->tr;
		xxdim = xdim;
		ydim = ydimtr;
		xxlow = xlow;
		ylow = ylowtr;
		anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		if (tr) {
			qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
			qpfree (qpl);
		} else {
			setdim_ (&xxdim, &ydim, &xxlow, &ylow);
		}
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle) {
			sprintf (string, "%s %s %s Azimuth = %.2f", grid->array, grid->refsta,
								gchan->chan, grid->azimuth);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = ydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = grid->dt*(grid->nt-1);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

	/* Plot beam */

	if (ydimbm > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		sy = grid->slowi*cos(grid->azimuth*M_PI/180.0);
		sx = grid->slowi*sin(grid->azimuth*M_PI/180.0);
		tr = (Trace *) make_beam (grid->chans, sx, sy, grid->t0, grid->dt*(grid->nt-1)+grid->t0);
		if (tr == NULL) {
			fprintf (stderr, "stgrid_plot: make_beam() error.\n");
			return;
		}
		qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
		SCV_free_trace (tr);
		xxdim = xdim;
		ydim = ydimbm;
		xxlow = xlow;
		ylow = ylowbm;
		anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		if (autotr) {
			ytop = 0.0;
			ybot = 0.0;
		}
		if (ytop == 0.0 && ybot == 0.0) {
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
		} else {
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 0, 1, 1.0);
		}
		qpfree (qpl);
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		sprintf (string, "%s %s Az: %.2f Sl: %.3f", grid->array, gchan->chan, grid->azimuth, grid->slowi);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = grid->dt*(grid->nt-1);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

	/* Plot overlay traces */

	ybot = 0.0;
	ytop = 0.0;
	if (ydimov > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		tr = gchan->tr;
		xxdim = xdim;
		ydim = ydimov;
		xxlow = xlow;
		ylow = ylowov;
		anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		if (tr) {
			qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
			hue = 0.0;
			light = 0.7;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
			qpfree (qpl);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			sy = grid->slowi*cos(grid->azimuth*M_PI/180.0);
			sx = grid->slowi*sin(grid->azimuth*M_PI/180.0);
			trb = (Trace *) make_beam (grid->chans, sx, sy, grid->t0, grid->dt*(grid->nt-1)+grid->t0);
			if (trb == NULL) {
				fprintf (stderr, "stgrid_plot: make_beam() error.\n");
				return;
			}
			qpl = qpbin (trb->tstart, trb->dt, trb->nsamps, trb->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
			if (ytop == 0.0 && ybot == 0.0) {
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
			} else {
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 0, 1, 1.0);
			}
			qpfree (qpl);
			SCV_free_trace (trb);
		} else {
			setdim_ (&xxdim, &ydim, &xxlow, &ylow);
		}
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle && ydimtr == 0.0 && ydimbm == 0.0) {
			sprintf (string, "%s %s %s Azimuth = %.2f", grid->array, grid->refsta,
								gchan->chan, grid->azimuth);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = ydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = grid->dt*(grid->nt-1);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

	/* Plot difference trace */

	if (ydimdf > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		tr = gchan->tr;
		xxdim = xdim;
		ydim = ydimdf;
		xxlow = xlow;
		ylow = ylowdf;
		anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		if (tr) {
			sy = grid->slowi*cos(grid->azimuth*M_PI/180.0);
			sx = grid->slowi*sin(grid->azimuth*M_PI/180.0);
			trb = (Trace *) make_beam (grid->chans, sx, sy, grid->t0, grid->dt*(grid->nt-1)+grid->t0);
			if (trb == NULL) {
				fprintf (stderr, "stgrid_plot: make_beam() error.\n");
				return;
			}
			for (trp=trb; trp!=NULL; trp=trp->next) {
				for (i=0,time=trp->tstart; i<trp->nsamps; i++,time+=trp->dt) {
					if (!get_trace_float_data (tr, time, &data)) {
						trp->data[i] = 2.e30;
						continue;
					}
					if (data > 1.e30) {
						trp->data[i] = 2.e30;
						continue;
					}
					trp->data[i] = data - trp->data[i];
				}
			}
	                trb = (Trace *) convert_trace (trb, "t4");
			trb = (Trace *) SCV_trace_fixgaps (trb, "segment");
			trb = (Trace *) SCV_trace_tofloat (trb, 0);
			qpl = qpbin (trb->tstart, trb->dt, trb->nsamps, trb->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
			hue = 240.0;
			light = 0.5;
			sat = 1.0;
			setfor_ (&hue, &light, &sat);
			if (ytop == 0.0 && ybot == 0.0) {
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
			} else {
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 0, 1, 1.0);
			}
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			qpfree (qpl);
			SCV_free_trace (trb);
		} else {
			setdim_ (&xxdim, &ydim, &xxlow, &ylow);
		}
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle && ydimtr == 0.0 && ydimbm == 0.0 && ydimov == 0.0) {
			sprintf (string, "%s %s %s Azimuth = %.2f", grid->array, grid->refsta,
								gchan->chan, grid->azimuth);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = ydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = grid->dt*(grid->nt-1);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

	/* Plot grid */

	if (ydimgr > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		xxdim = xdim;
		xxlow = xlow;
		ydim = ydimgr;
		ylow = ylowgr;
		xmin = grid->t[0];
		xmax = grid->t[grid->nt-1];
		ymin = grid->smin;
		ymax = grid->smax;
		setdim_ (&xxdim, &ydim, &xxlow, &ylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		zmin = 0.0;
		zmax = 1.0;
		dz = 0.1;
		z = (float *) my_malloc ("stgrid_plot: z", grid->nt*grid->ns*sizeof(float));
		if (z == NULL) {
			fprintf (stderr, "plot_sgrid: malloc error.\n");
			finitt_ ();
			return (0);
		}
		if (norm) {
			if (autogr) zmax = 0.0;
			for (i=0; i<grid->nt; i++) {
				for (j=0; j<grid->ns; j++) {
					z[i+j*grid->nt] = grid->norm_power_grid[j+i*grid->ns];
					if (autogr && z[i+j*grid->nt] > zmax) zmax = z[i+j*grid->nt];
				}
			}
			dz = 0.1*(zmax-zmin);
		} else {
			zmax = 0.0;
			for (i=0; i<grid->nt; i++) {
				for (j=0; j<grid->ns; j++) {
					if (grid->power_grid[j+i*grid->ns] > 0.0) {
						z[i+j*grid->nt] = log10(grid->power_grid[j+i*grid->ns]);
				    	} else {
						z[i+j*grid->nt] = -1.e30;
					}
					if (z[i+j*grid->nt] > zmax) zmax = z[i+j*grid->nt];
				}
			}
			zmin = zmax - 2.0;
			dz = 0.1*(zmax-zmin);
		}
		set_ctable();
		ncontour_ (&grid->nt, &grid->nt, &grid->ns, grid->t, grid->s, z,  ctype,  &zmin,  &zmax,
	                    	&dz, strlen(ctype));
		my_free (z);
		if (itimelabel) {
			anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 1);
		} else {
			anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		}
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		line_ (&xmin, &fzero, &xmax, &fzero, &thick, &ithick, &iclip);
		iclip = 1;
		iref = 7;
		height = 0.07;
		x = xmin-(xmax-xmin)*0.05/xxdim;
		y = 0.0;
		chrsiz_ (&height, &ratio, &slant);
		if (y >= ymin && y <= ymax) {
			strcpy (string, "Inf");
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		}
	
		for (i=0; i<19; i++) {
			velocity = 2.0+i;
			slow = 1.0/velocity;
			if (slow > ymax) continue;
			if (slow < ymin) continue;
			if (velocity == 5.0 || velocity == 10.0 || velocity == 15.0 || velocity == 20.0) {
				len = 0.2;
				x = xmin-(xmax-xmin)*0.05/xxdim;
				y = slow;
				sprintf (string, "%.1f", velocity);
				line_ (&xmin, &slow, &xmax, &slow, &thick, &ithick, &iclip);
				if (velocity != 15.0) {
					text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
				}
				continue;
			} else {
				len = 0.1;
			}
			x1 = xmin;
			x2 = x1+(xmax-xmin)*len/xxdim;
			x3 = xmax;
			x4 = x3-(xmax-xmin)*len/xxdim;
			line_ (&x1, &slow, &x2, &slow, &thick, &ithick, &iclip);
			line_ (&x3, &slow, &x4, &slow, &thick, &ithick, &iclip);
		}
		for (i=0; i<19; i++) {
			velocity = 2.0+i;
			slow = 1.0/velocity;
			if (-slow < ymin) continue;
			if (velocity == 5.0 || velocity == 10.0 || velocity == 15.0 || velocity == 20.0) {
				len = 0.2;
				x = xmin-(xmax-xmin)*0.05/xxdim;
				y = slow;
				sprintf (string, "%.1f", velocity);
				slow = -slow;
				line_ (&xmin, &slow, &xmax, &slow, &thick, &ithick, &iclip);
				if (velocity != 15.0) {
					text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
				}
				continue;
			} else {
				len = 0.1;
			}
			x1 = xmin;
			x2 = x1+(xmax-xmin)*len/xxdim;
			x3 = xmax;
			x4 = x3-(xmax-xmin)*len/xxdim;
			slow = -slow;
			line_ (&x1, &slow, &x2, &slow, &thick, &ithick, &iclip);
			line_ (&x3, &slow, &x4, &slow, &thick, &ithick, &iclip);
		}
		if (pred_phases && orid > -1) {
			double etime, elat, elon, slat, slon, depth, elev;
			Dbptr dbe, dbs;

			sprintf (string, "%d", orid);
			dbopen (dbcat, "r+", &dbc);
			dbe = dblookup (dbc, 0, "origin", "orid", string);
			if (dbe.record != dbINVALID) {
				dbe.field = dbALL;
				dbgetv (dbe, 0, "lat", &elat, "lon", &elon, "depth", &depth, 
						"time", &etime, NULL);
				dbs = dblookup (dbi, 0, "site", "sta", grid->refsta);
				if (dbs.record != dbINVALID) {
					dbs.field = dbALL;
					dbgetv (dbs, 0, "lat", &slat, "lon", &slon, "elev", &elev, NULL);
					tt_taup_set_phases (pred_phases);
					tt_taup_set_event_depth (depth);
					tt_taup_p (elat, elon, slat, slon, elev, 0.0, 0.0,
						&nph, &timep, &ps, &dtdhs, &phs);
					hue = 240.0;
					light = 0.5;
					sat = 1.0;
					setfor_ (&hue, &light, &sat);
					for (i=0; i<nph; i++) {
						time = etime + timep[i];
						if (time < grid->t0) continue;
						if (time > grid->t0+grid->dt*(grid->nt-1)) continue;
						x = time - grid->t0;
						y = ps[i]/111.12;
						fsize = 0.1;
						nsymbol_ (stype, &x, &y, &fsize, &thick, &iclip, &ifill, strlen(stype));
						iref = 2;
						y -= 0.1*(ymax-ymin)/ydim;
						text_ (&x, &y, &fzero, &iref, phs[i], &iclip, strlen(phs[i]));
					}
					hue = 0.0;
					light = 0.0;
					sat = 0.0;
					setfor_ (&hue, &light, &sat);
				}
			}
		}
		for (i=0; i<grid->nt; i++) {
			x2 = 0.0;
			for (j=0; j<grid->ns; j++) {
				x1 = grid->norm_power_grid[j+i*grid->ns];
				if (x1 > x2) {
					x2 = x1;
					y = grid->s[j];
				}
			}
			if (x2 < 0.5) continue;
			x = grid->t[i];
			fsize = 0.05;
			nsymbol_ (stype, &x, &y, &fsize, &thick, &iclip, &ifill, strlen(stype));
		}
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
	}
}

int
ftgrid_plot (grid, itrans, size, plotfile, xdim, xlow, ydimgr, ylowgr, ydimtr, ylowtr, 
			ydimbm, ylowbm, xdimsp, xlowsp, ydimsp, ylowsp, 
			ititle, itimelabel, ntimes, times)

Apspec *   grid;
int                itrans;
double                     size;
char *                           plotfile;
double                                     xdim, xlow, ydimgr, ylowgr, ydimtr, ylowtr;
double                  ydimbm, ylowbm, xdimsp, xlowsp, ydimsp, ylowsp;
int                                                 ititle;
int                                                         itimelabel;
int                                                                     ntimes;
double *                                                                        times;

{
	char plotf[256];
	static char display[] = " ";
	static char program[] = "dbap:fgrid";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float ydimt = 1.5;
	float ydimg = 4.8;
	float fzero = 0.0;
	float xxdim, ydim, xxlow, ylow;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	float zmin, zmax, dz;
	float *yy, *z;
	int igraf = 0;
	int iclip = 0;
	int iclear = 0;
	int ifill = 0;
	float thick = 0.0;
	int ithick = 0;
	float fsize;
	static char asymb[] = " ";
	static char ctype[] = "colors";
	static char stype[] = "cross";
	static int instance=1;
	static float xsthick=0.0, xshue=0.0, xslight=0.0, xssat=0.0;
	static float xnthick=0.0, xnhue=0.0, xnlight=0.0, xnsat=0.0;
	static float ysthick=0.0, yshue=0.0, yslight=0.0, yssat=0.0;
	static float ynthick=0.0, ynhue=0.0, ynlight=0.0, ynsat=0.0;
	static int ixsstyl=0, ixnstyl=0, iysstyl=0, iynstyl=0;
	float dxsmal, dxnumb, dysmal, dynumb, xmarg, ymarg;
	float x, y;
	int iref;
	float height, ratio=1.0, slant=0.0;
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	char title[80];
	char string[80];
	char filter[80];
	int i, j;
	float velocity, slow, x1, x2, x3, x4, len;
	Channelspec *gchan;
	int qpl;
	float ybot, ytop;
	int jfont = 130;
	Trace *tr;
	double sx, sy;
	Tbl *grids;
	float zz, ffmin, ffmax;
	double wt;

	/* Initialize plot */

	antelope_init_plot (itrans, size, plotfile, dbin);

	/* Plot trace */

	ybot = 0.0;
	ytop = 0.0;
	if (ydimtr > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		qpl = qpbin (gchan->tr->tstart, gchan->tr->dt, gchan->tr->nsamps, gchan->tr->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
		xxdim = xdim;
		ydim = ydimtr;
		xxlow = xlow;
		ylow = ylowtr;
		anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle) {
			sprintf (string, "%s %s %s Azimuth = %.2f, Slowness = %.3f", grid->array, grid->refsta,
								gchan->chan, grid->azimuth, grid->slowi);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = ydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = grid->dt*(grid->nt-1);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

	/* Plot beam */

	if (ydimbm > 0.0) {
		gchan = (Channelspec *) gettbl (grid->chans, 0);
		sy = grid->slowi*cos(grid->azimuth*M_PI/180.0);
		sx = grid->slowi*sin(grid->azimuth*M_PI/180.0);
		tr = (Trace *) make_beam (grid->chans, sx, sy, grid->t0, grid->dt*(grid->nt-1)+grid->t0);
		if (tr == NULL) {
			fprintf (stderr, "ftgrid_plot: make_beam() error.\n");
			return;
		}
		qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, 
						grid->t0, grid->dt*(grid->nt-1), 4000, 0);
		SCV_free_trace (tr);
		xxdim = xdim;
		ydim = ydimbm;
		xxlow = xlow;
		ylow = ylowbm;
		anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		if (ytop == 0.0 && ybot == 0.0) {
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
		} else {
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 0, 1, 1.0);
		}
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		for (i=0,j=0; i<strlen(gchan->filter); i++) {
			if (!isprint(gchan->filter[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (gchan->filter[i] == '\t') filter[j++] = ' ';
			else if (gchan->filter[i] == '\n') filter[j] = ' ';
			else filter[j++] = gchan->filter[i];
		}
		filter[i] = '\0';
		sprintf (string, "%s %s Az: %.2f Sl: %.3f", grid->array, gchan->chan, grid->azimuth, grid->slowi);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = grid->dt*(grid->nt-1);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - grid->t0;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

/*
 *	Plot spectragram
 */
	if (ydimgr > 0.0) {
 		zmin = 0.0;
 		dz = 0.0;
 		zmax = -1.0e30;
 		z = (float *) my_malloc ("ftgrid_plot: z", grid->nt*grid->ns*sizeof(float));
 		if (z == NULL) {
 			fprintf (stderr, "ftgrid_plot: malloc() error.\n");
 			return;
 		}
 		yy = (float *) my_malloc ("ftgrid_plot: yy", grid->ns*sizeof(float));
 		if (yy == NULL) {
 			fprintf (stderr, "ftgrid_plot: malloc() error.\n");
 			return;
 		}
	 	for (i=0; i<grid->nt; i++) {
 			for (j=0; j<grid->ns; j++) {
 				z[i+j*grid->nt] = grid->norm_power_grid[i+j*grid->nt];
 				zz = z[i+j*grid->nt];
 				if (zz > zmax) zmax = zz;
 			}
		}
		ffmin = grid->s[0];
		ffmax = grid->s[grid->ns-1];
		for (j=0; j<grid->ns; j++) {
			yy[j] = log10(grid->s[j]);
		}
		zmin = 0.0;
		zmax = 1.0;
		dz = 0.1*zmax;
		ymin = yy[0];
		ymax = yy[grid->ns-1];
		xxdim = xdim;
		xxlow = xlow;
		ydim = ydimgr;
		ylow = ylowgr;
		xmin = grid->t[0];
		xmax = grid->t[grid->nt-1];
		setdim_ (&xxdim, &ydim, &xxlow, &ylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		set_ctable();
		ncontour_ (&grid->nt, &grid->nt, &grid->ns, grid->t, yy, z,  ctype,  &zmin,  &zmax,
	                    	&dz, strlen(ctype));
		my_free (z);
		my_free (yy);
		if (itimelabel) {
			anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 1);
		} else {
			anno_times (xxdim, ydim, xxlow, ylow, grid->t0, grid->dt*(grid->nt-1), 0);
		}
 		anno_freqs (xxdim, ydim, xxlow, ylow, ffmin, ffmax, 1, 0, "Frequency (hz)");
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		sprintf (string, "%s %s", gchan->sta, gchan->chan);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
	}

/*
 *	Plot stacked spectrum
 */
	if (ydimsp > 0.0) {
		z = (float *) my_malloc ("ftgrid_plot: z", grid->ns*sizeof(float));
		if (z == NULL) {
 			fprintf (stderr, "ftgrid_plot: malloc() error.\n");
 			return;
		}
 		yy = (float *) my_malloc ("ftgrid_plot: yy", grid->ns*sizeof(float));
 		if (yy == NULL) {
 			fprintf (stderr, "ftgrid_plot: malloc() error.\n");
 			return;
 		}
		for (i=0; i<grid->ns; i++) z[i] = 0.0;
		for (i=0; i<grid->nt; i++) {
			for (j=0; j<grid->ns; j++) {
				z[j] += grid->norm_power_grid[i+j*grid->nt];
			}
		}
		wt = 1.0/grid->nt;
		for (i=0; i<grid->ns; i++) z[i] *= wt;
		for (j=0; j<grid->ns; j++) {
			yy[j] = log10(grid->s[j]);
		}
		xxdim = xdimsp;
		xxlow = xlowsp;
		ydim = ydimsp;
		ylow = ylowsp;
		xmin = yy[0];
		xmax = yy[grid->ns-1];
		ymin = 0.0;
		ymax = 1.1;
		ffmin = grid->s[0];
		ffmax = grid->s[grid->ns-1];
		setdim_ (&xxdim, &ydim, &xxlow, &ylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
 		anno_freqs (xxdim, ydim, xxlow, ylow, ffmin, ffmax, 1, 1, "Frequency (hz)");
 		anno_amps (xxdim, ydim, xxlow, ylow, 0.0, 1.1, 0.0, 0.1, "%.1f", 1, 0, "Normalized Beam Amplitude");
		nplot_ (&grid->ns, yy, z, &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		my_free (z);
		my_free (yy);
	}
}

int
beam_plot (beam, itrans, size, plotfile, xdim, xlow, ydimbm, ylowbm, ydimtr, ylowtr, 
			ititle, itimelabel, ntimes, times, filteri)

Apspec *   beam;
int                itrans;
double                     size;
char *                         plotfile;
double                               xdim, xlow, ydimbm, ylowbm, ydimtr, ylowtr;
int                                     ititle;
int                                             itimelabel;
int                                                         ntimes;
double *                                                            times;
char *                                                      filteri;

{
	char plotf[256];
	static char display[] = " ";
	static char program[] = "dbap:beam";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float ydimt = 1.5;
	float ydimg = 4.8;
	float fzero = 0.0;
	float xxdim, ydim, xxlow, ylow;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	float zmin, zmax, dz;
	float *z;
	int igraf = 0;
	int iclip = 0;
	int iclear = 0;
	int ifill = 0;
	float thick = 0.0;
	int ithick = 0;
	float fsize;
	static char asymb[] = " ";
	static char ctype[] = "colors";
	static char stype[] = "cross";
	static int instance=1;
	static float xsthick=0.0, xshue=0.0, xslight=0.0, xssat=0.0;
	static float xnthick=0.0, xnhue=0.0, xnlight=0.0, xnsat=0.0;
	static float ysthick=0.0, yshue=0.0, yslight=0.0, yssat=0.0;
	static float ynthick=0.0, ynhue=0.0, ynlight=0.0, ynsat=0.0;
	static int ixsstyl=0, ixnstyl=0, iysstyl=0, iynstyl=0;
	float dxsmal, dxnumb, dysmal, dynumb, xmarg, ymarg;
	float x, y;
	int iref;
	float height, ratio=1.0, slant=0.0;
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	char title[80];
	char string[80];
	char filter[80];
	int i, j;
	float velocity, slow, x1, x2, x3, x4, len;
	Channelspec *bchan;
	int qpl;
	float ybot, ytop;
	int jfont = 130;
	Trace *tr, *trp;
	double range, yytop, yybot;

	/* Initialize plot */

	antelope_init_plot (itrans, size, plotfile, dbin);

	/* Plot trace */

	ybot = 0.0;
	ytop = 0.0;
	if (ydimtr > 0.0) {
		bchan = (Channelspec *) gettbl (beam->chans, 0);
		tr = (Trace *) get_trace_from_scs (sc, bchan->sta, bchan->chan, beam->ts, beam->te);
		if (tr == NULL) {
			fprintf (stderr, "beam_plot: get_trace_from_scs() error.\n");
			return (0);
		}
		if (filteri) {
			tr = (Trace *) filter_trace (tr, filteri, 0);
			if (tr == NULL) {
				fprintf (stderr, "beam_plot: filter_trace() error.\n");
				return (0);
			}
		} else {
			filteri = bchan->filter;
		}
		for (i=0,j=0; i<strlen(filteri); i++) {
			if (!isprint(filteri[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (filteri[i] == '\t') filter[j++] = ' ';
			else if (filteri[i] == '\n') filter[j] = ' ';
			else filter[j++] = filteri[i];
		}
		filter[i] = '\0';
		qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, 
						beam->ts, beam->te-beam->ts, 4000, 0);
		SCV_free_trace (tr);
		xxdim = xdim;
		ydim = ydimtr;
		xxlow = xlow;
		ylow = ylowtr;
		if (itimelabel && ydimbm == 0.0) {
			anno_times (xxdim, ydim, xxlow, ylow, beam->ts, beam->te-beam->ts, 1);
		} else {
			anno_times (xxdim, ydim, xxlow, ylow, beam->ts, beam->te-beam->ts, 0);
		}
		nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		sprintf (string, "%s %s Filter: %s", bchan->sta, bchan->chan, filter);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle) {
			sprintf (string, "%s %s %s Azimuth = %.2f Velocity = %.2f", beam->array, beam->refsta,
								bchan->chan, beam->azimuth, 1.0/beam->slowi);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = ydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
			ititle = 0;
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = beam->te-beam->ts;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - beam->ts;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

	/* Plot beam */

	if (ydimbm > 0.0) {
		bchan = (Channelspec *) gettbl (beam->chans, 0);
		tr = (Trace *) copy_trace (beam->tr, 1);
		if (tr == NULL) {
			fprintf (stderr, "beam_plot: No beam trace.\n");
			return;
		}
		if (filteri) {
			tr = (Trace *) filter_trace (tr, filteri, 0);
			if (tr == NULL) {
				fprintf (stderr, "beam_plot: filter_trace() error.\n");
				return (0);
			}
		} else {
			filteri = bchan->filter;
		}
		for (i=0,j=0; i<strlen(filteri); i++) {
			if (!isprint(filteri[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (filteri[i] == '\t') filter[j++] = ' ';
			else if (filteri[i] == '\n') filter[j] = ' ';
			else filter[j++] = filteri[i];
		}
		filter[i] = '\0';
		qpl = 0;
		for (trp=tr; trp!=NULL; trp=trp->next) {
			qpl = qpbin (trp->tstart, trp->dt, trp->nsamps, trp->data, 
						beam->ts, beam->te-beam->ts, 4000, qpl);
		}
		xxdim = xdim;
		ydim = ydimbm;
		xxlow = xlow;
		ylow = ylowbm;
		anno_times (xxdim, ydim, xxlow, ylow, beam->ts, beam->te-beam->ts, itimelabel);
		if (ytop == 0.0 && ybot == 0.0) {
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 1, 1, 1.0);
		} else {
			get_trace_maxmin (tr, beam->ts, beam->te, &yytop, &yybot);
			range = ytop - ybot;
			ytop = 0.5*(yytop+yybot) + 0.5*range;
			ybot = 0.5*(yytop+yybot) - 0.5*range;
			nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot, &ytop, 0, 1, 1.0);
		}
		qpfree (qpl);
		SCV_free_trace (tr);
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = ydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		sprintf (string, "%s %s Az: %.2f Sl: %.3f", beam->array, bchan->chan, beam->azimuth, beam->slowi);
		x = 0.05;
		y = ydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle) {
			sprintf (string, "%s %s %s Azimuth = %.2f Velocity = %.2f", beam->array, beam->refsta,
								bchan->chan, beam->azimuth, 1.0/beam->slowi);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = ydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = beam->te-beam->ts;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*ydim;
		x3 = 0.75*ydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - beam->ts;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

}

int
gather_plot (gather, itrans, size, plotfile, xdim, xlow, ydim, ylow,
			ititle, itimelabel, ntimes, times, filteri)

Apspec *     gather;
int                itrans;
double                     size;
char *                         plotfile;
double                               xdim, xlow, ydim, ylow;
int                                     ititle;
int                                             itimelabel;
int                                                         ntimes;
double *                                                            times;
char *                                                      filteri;

{
	char plotf[256];
	static char display[] = " ";
	static char program[] = "dbap:gather";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float ydimt = 1.5;
	float ydimg = 4.8;
	float fzero = 0.0;
	float xxdim, yydim, xxlow, yylow;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	float zmin, zmax, dz;
	float *z;
	int igraf = 0;
	int iclip = 0;
	int iclear = 0;
	int ifill = 0;
	float thick = 0.0;
	int ithick = 0;
	float fsize;
	static char asymb[] = " ";
	static char ctype[] = "colors";
	static char stype[] = "cross";
	static int instance=1;
	static float xsthick=0.0, xshue=0.0, xslight=0.0, xssat=0.0;
	static float xnthick=0.0, xnhue=0.0, xnlight=0.0, xnsat=0.0;
	static float ysthick=0.0, yshue=0.0, yslight=0.0, yssat=0.0;
	static float ynthick=0.0, ynhue=0.0, ynlight=0.0, ynsat=0.0;
	static int ixsstyl=0, ixnstyl=0, iysstyl=0, iynstyl=0;
	float dxsmal, dxnumb, dysmal, dynumb, xmarg, ymarg;
	float x, y;
	int iref;
	float height, ratio=1.0, slant=0.0;
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	char title[80];
	char string[80];
	char filter[80];
	int i, j;
	float velocity, slow, x1, x2, x3, x4, len;
	Channelspec *gchan;
	int qpl;
	float ybot, ytop;
	int jfont = 130;
	Trace *tr, *trh;
	double range, yytop, yybot;
	float ydimtr;
	int k, nch;
	char *ptr;

	/* Initialize plot */

	antelope_init_plot (itrans, size, plotfile, dbin);

	/* Plot traces */

	ybot = 0.0;
	ytop = 0.0;
	nch = maxtbl(gather->chans);
	ydimtr = ydim/nch;
	for (k=0; k<nch; k++) {
		gchan = (Channelspec *) gettbl (gather->chans, k);
		if (filteri) {
			ptr = filteri;
		} else {
			ptr = gchan->filter;
		}
		for (i=0,j=0; i<strlen(ptr); i++) {
			if (!isprint(ptr[i])) {
				filter[j++] = ' ';
				continue;
			}
			if (ptr[i] == '\t') filter[j++] = ' ';
			else if (ptr[i] == '\n') filter[j] = ' ';
			else filter[j++] = ptr[i];
		}
		filter[i] = '\0';
		xxdim = xdim;
		yydim = ydimtr;
		xxlow = xlow;
		yylow = ylow+ydim-ydimtr*(k+1);
		if (itimelabel && k == nch-1) {
			anno_times (xxdim, yydim, xxlow, yylow, gather->ts, gather->te-gather->ts, 1);
		} else {
			anno_times (xxdim, yydim, xxlow, yylow, gather->ts, gather->te-gather->ts, 0);
		}
		if (gchan->tr) {
			if (filteri) {
				trh = (Trace *) filter_trace (gchan->tr, filteri, 1);
				if (trh == NULL) {
					fprintf (stderr, "gather_plot: filter_trace() error.\n");
					return (0);
				}
			} else {
				trh = gchan->tr;
			}
			qpl = 0;
			for (tr=trh; tr!=NULL; tr=tr->next) {
				qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, 
							gather->ts, gather->te-gather->ts, 4000, qpl);
			}
			if (trh != gchan->tr) SCV_free_trace (trh);
			nqplotsegs (qpl, xxdim, yydim, xxlow, yylow, &ybot, &ytop, 1, 1, 1.0);
			qpfree (qpl);
		} else {
			setdim_ (&xxdim, &yydim, &xxlow, &yylow);
		}
		xmin = 0.0;
		xmax = xxdim;
		ymin = 0.0;
		ymax = yydim;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		cfont_ (&jfont);
		height = 0.05;
		chrsiz_ (&height, &ratio, &slant);
		sprintf (string, "%s %s Filter: %s", gchan->sta, gchan->chan, filter);
		x = 0.05;
		y = yydim - 0.05;
		iref = 2;
		text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
		if (ititle) {
			sprintf (string, "%s %s %s Azimuth = %.2f Velocity = %.2f", gather->array, gather->refsta,
								gchan->chan, gather->azimuth, 1.0/gather->slowi);
			height = 0.1;
			chrsiz_ (&height, &ratio, &slant);
			iclip = 1;
			x = 0.5*xdim;
			y = yydim + 0.05;
			iref = 3;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
			ititle = 0;
		}
		hue = 0.0;
		light = 0.5;
		sat = 1.0;
		setfor_ (&hue, &light, &sat);
		xmin = 0.0;
		xmax = gather->te-gather->ts;
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		x2 = 0.25*yydim;
		x3 = 0.75*yydim;
		for (i=0; i<ntimes; i++) {
			x1 = times[i] - gather->ts;
			line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
		}
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
	}

}

int
crosscor_plot (cc, itrans, size, plotfile, xdimtr, xlowtr, ydimtr, ylowtr, xdimcc, xlowcc, ydimcc, ylowcc, 
			xdimsp, xlowsp, ydimsp, ylowsp, fmin, fmax, ititle, itimelabel, ntimes, times)

Apspec *       cc;
int                itrans;
double                     size;
char *                         plotfile;
double                               xdimtr, xlowtr, ydimtr, ylowtr;
double                               xdimcc, xlowcc, ydimcc, ylowcc;
double                               xdimsp, xlowsp, ydimsp, ylowsp, fmin, fmax;
int                                     ititle;
int                                             itimelabel;
int                                                         ntimes;
double *                                                            times;

{
	char plotf[256];
	static char display[] = " ";
	static char program[] = "dbap:crosscor";
	float ssize = 0.9;
	float xwin = 0.0;
	float ywin = 0.0;
	float ydimt = 1.5;
	float ydimg = 4.8;
	float fzero = 0.0;
	float xxdim, ydim, xxlow, yylow, ylow;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	float zmin, zmax, dz;
	int igraf = 0;
	int iclip = 0;
	int iclear = 0;
	int ifill = 0;
	float thick = 0.0;
	int ithick = 0;
	float fsize;
	static char asymb[] = " ";
	static char ctype[] = "colors";
	static char stype[] = "cross";
	static int instance=1;
	static float xsthick=0.0, xshue=0.0, xslight=0.0, xssat=0.0;
	static float xnthick=0.0, xnhue=0.0, xnlight=0.0, xnsat=0.0;
	static float ysthick=0.0, yshue=0.0, yslight=0.0, yssat=0.0;
	static float ynthick=0.0, ynhue=0.0, ynlight=0.0, ynsat=0.0;
	static int ixsstyl=0, ixnstyl=0, iysstyl=0, iynstyl=0;
	float dxsmal, dxnumb, dysmal, dynumb, xmarg, ymarg;
	float x, y;
	float height, ratio=1.0, slant=0.0;
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	char title[80];
	char string[80];
	char filter[80];
	int i, j, k, n, iref, jref, ii;
	float velocity, slow, x1, x2, x3, x4, len;
	float y1, y2;
	Channelspec *bchan;
	int qpl;
	float ybot, ytop, ybot2, ytop2;
	float ybotw, ytopw;
	int jfont = 130;
	Trace *tr;
	double range, yytop, yybot;
	char *filteri;
	int iscl;
	double time, ts, te;
	double arg, amp;
	float *yy, *z;
	double arg1, arg2, dt;
	double tpad;

	/* Initialize plot */

	antelope_init_plot (itrans, size, plotfile, dbin);

	/* Plot original traces */

	ybot = 0.0;
	ytop = 0.0;
	if (ydimtr > 0.0) {
		n = maxtbl(cc->chans);
		for (k=-1; k<n+1; k++) {
			if (k < 0) {
				ybot = 1.e30;
				ytop = -1.e30;
				for (i=0; i<n; i++) {
					bchan = (Channelspec *) gettbl (cc->chans, i);
					ts = cc->ts;
					te = cc->te;
					if (bchan->tr == NULL) continue;
					for (j=0; j<bchan->tr->nsamps; j++) {
						time = bchan->tr->tstart + bchan->tshift + bchan->tcor + bchan->tr->dt*j;
						if (time < ts) continue;
						if (time > te) break;
						if (bchan->tr->data[j]*bchan->acor < ybot) ybot = bchan->tr->data[j]*bchan->acor;
						if (bchan->tr->data[j]*bchan->acor > ytop) ytop = bchan->tr->data[j]*bchan->acor;
					}
				}
				for (jref=0; jref<n; jref++) {
					bchan = (Channelspec *) gettbl (cc->chans, jref);
					if (!strcmp(bchan->sta, cc->refsta)) break;
				}
				ii = n;
				iscl = 0;
				tr = bchan->tr;
				filteri = bchan->filter;
			} else if (k < n) {
				if (k == jref) continue;
				bchan = (Channelspec *) gettbl (cc->chans, k);
				iscl = 0;
				tr = bchan->tr;
				filteri = bchan->filter;
			} else {
				tr = NULL;
			}
			ts = cc->ts;
			te = cc->te;
			for (i=0,j=0; i<strlen(filteri); i++) {
				if (!isprint(filteri[i])) {
					filter[j++] = ' ';
					continue;
				}
				if (filteri[i] == '\t') filter[j++] = ' ';
				else if (filteri[i] == '\n') filter[j] = ' ';
				else filter[j++] = filteri[i];
			}
			filter[i] = '\0';
			xxdim = xdimtr;
			ydim = ydimtr/(n+1);
			xxlow = xlowtr;
			yylow = ylowtr;
			ylow = ylowtr + ydim*ii;
			tpad = 1.0*(te-ts);
			ii--;
			if (itimelabel && k == n) {
				anno_times (xxdim, ydim, xxlow, ylow, ts-tpad, te-ts+2.0*tpad, 1);
			} else {
				anno_times (xxdim, ydim, xxlow, ylow, ts-tpad, te-ts+2.0*tpad, 0);
			}
			if (tr) {
				qpl = qpbin (tr->tstart+bchan->tshift+bchan->tcor, 
						tr->dt, tr->nsamps, tr->data, ts-tpad, te-ts+2.0*tpad, 4000, 0);
				ybotw = ybot/bchan->acor;
				ytopw = ytop/bchan->acor;
				nqplotsegs (qpl, xxdim, ydim, xxlow, yylow, &ybotw, &ytopw, iscl, 1, 1.0);
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybotw, &ytopw, iscl, 1, 1.0);
				qpfree (qpl);
			} else {
				setdim_ (&xxdim, &ydim, &xxlow, &ylow);
			}
			xmin = 0.0;
			xmax = xxdim;
			ymin = 0.0;
			ymax = ydim;
			setscl_ (&xmin, &xmax, &ymin, &ymax);
			box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
			cfont_ (&jfont);
			height = 0.05;
			chrsiz_ (&height, &ratio, &slant);
			if (k == n) {
				sprintf (string, "Overlay");
			} else {
				sprintf (string, "%s %s", bchan->sta, bchan->chan);
			}
			x = 0.05;
			y = ydim - 0.05;
			iref = 2;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
			if (k != n) {
				if (bchan->sp) {
					sprintf (string, "%6.3f %7.3f", bchan->tcor,
									bchan->acor);
					x = xxdim - 0.05;
					y = ydim - 0.05;
					iref = 8;
					text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
				}
			}
			if (ititle) {
				sprintf (string, "%s %s %s Azimuth = %.2f Velocity = %.2f", cc->refsta, bchan->sta,
									bchan->chan, cc->azimuth, 1.0/cc->slowi);
				height = 0.1;
				chrsiz_ (&height, &ratio, &slant);
				iclip = 1;
				x = 0.0;
				y = ydim + 0.05;
				iref = 0;
				text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
				ititle = 0;
			}
			hue = 0.0;
			light = 0.5;
			sat = 1.0;
			setfor_ (&hue, &light, &sat);
			xmin = 0.0;
			xmax = te-ts;
			setscl_ (&xmin, &xmax, &ymin, &ymax);
			x2 = 0.25*ydim;
			x3 = 0.75*ydim;
			for (i=0; i<ntimes; i++) {
				x1 = times[i] - ts;
				line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
			}
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
		}
	}

	/* Plot shifted cross correlation traces */

	ybot = 0.0;
	ytop = 0.0;
	if (ydimcc > 0.0) {
		n = maxtbl(cc->chans);
		for (k=-1; k<n+1; k++) {
			if (k < 0) {
				ybot = 1.e30;
				ytop = -1.e30;
				ybot2 = 1.e30;
				ytop2 = -1.e30;
				for (i=0; i<n; i++) {
					bchan = (Channelspec *) gettbl (cc->chans, i);
					ts = cc->ts;
					te = cc->te;
					if (bchan->tr == NULL) continue;
					for (j=0; j<bchan->tr->nsamps; j++) {
						time = bchan->tr->tstart+bchan->tshift+bchan->tcor+bchan->sp->tshift
											+ bchan->tr->dt*j;
						if (time < ts) continue;
						if (time > te) break;
						if (bchan->tr->data[j]*bchan->acor*bchan->sp->acor < ybot) 
								ybot = bchan->tr->data[j]*bchan->acor*bchan->sp->acor;
						if (bchan->tr->data[j]*bchan->acor*bchan->sp->acor > ytop) 
								ytop = bchan->tr->data[j]*bchan->acor*bchan->sp->acor;
					}
					for (j=0; j<bchan->sp->nf*2; j++) {
						time = ts + bchan->sp->dt*j;
						if (time > te) break;
						if (bchan->sp->fftt[j] < ybot2) ybot2 = bchan->sp->fftt[j];
						if (bchan->sp->fftt[j] > ytop2) ytop2 = bchan->sp->fftt[j];
					}
				}
				for (jref=0; jref<n; jref++) {
					bchan = (Channelspec *) gettbl (cc->chans, jref);
					if (!strcmp(bchan->sta, cc->refsta)) break;
				}
				ii = n;
				iscl = 0;
				tr = bchan->tr;
				filteri = bchan->filter;
			} else if (k < n) {
				if (k == jref) continue;
				bchan = (Channelspec *) gettbl (cc->chans, k);
				iscl = 0;
				tr = bchan->tr;
				filteri = bchan->filter;
			} else {
				tr = NULL;
			}
			ts = cc->ts;
			te = cc->te;
			for (i=0,j=0; i<strlen(filteri); i++) {
				if (!isprint(filteri[i])) {
					filter[j++] = ' ';
					continue;
				}
				if (filteri[i] == '\t') filter[j++] = ' ';
				else if (filteri[i] == '\n') filter[j] = ' ';
				else filter[j++] = filteri[i];
			}
			filter[i] = '\0';
			xxdim = xdimcc;
			ydim = ydimcc/(n+1);
			xxlow = xlowcc;
			yylow = ylowcc;
			ylow = ylowcc + ydim*ii;
			tpad = 1.0*(te-ts);
			ii--;
			if (itimelabel && k == n) {
				anno_times (xxdim, ydim, xxlow, ylow, ts-tpad, te-ts+2.0*tpad, 1);
			} else {
				anno_times (xxdim, ydim, xxlow, ylow, ts-tpad, te-ts+2.0*tpad, 0);
			}
			if (tr) {
				qpl = qpbin (tr->tstart+bchan->tshift+bchan->tcor+bchan->sp->tshift, 
						tr->dt, tr->nsamps, tr->data, ts-tpad, te-ts+2.0*tpad, 4000, 0);
				ybotw = ybot/(bchan->acor*bchan->sp->acor);
				ytopw = ytop/(bchan->acor*bchan->sp->acor);
				nqplotsegs (qpl, xxdim, ydim, xxlow, yylow, &ybotw, &ytopw, iscl, 1, 1.0);
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybotw, &ytopw, iscl, 1, 1.0);
				qpfree (qpl);
			} else {
				setdim_ (&xxdim, &ydim, &xxlow, &ylow);
			}
			if (bchan->sp) if (bchan->sp->fftt) if (ii == -500) {
				hue = 0.0;
				light = 0.5;
				sat = 1.0;
				setfor_ (&hue, &light, &sat);
				dt = bchan->sp->dt;
				qpl = qpbin (ts+bchan->sp->tshift, dt, bchan->sp->nf*2, bchan->sp->fftt,
							ts-tpad, te-ts+2.0*tpad, 4000, 0);
				nqplotsegs (qpl, xxdim, ydim, xxlow, yylow, &ybot2, &ytop2, iscl, 1, 1.0);
				nqplotsegs (qpl, xxdim, ydim, xxlow, ylow, &ybot2, &ytop2, iscl, 1, 1.0);
				qpfree (qpl);
				hue = 0.0;
				light = 0.0;
				sat = 0.0;
				setfor_ (&hue, &light, &sat);
			}
			xmin = 0.0;
			xmax = xxdim;
			ymin = 0.0;
			ymax = ydim;
			setscl_ (&xmin, &xmax, &ymin, &ymax);
			box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
			cfont_ (&jfont);
			height = 0.05;
			chrsiz_ (&height, &ratio, &slant);
			if (k == n) {
				sprintf (string, "Overlay");
			} else {
				sprintf (string, "%s %s", bchan->sta, bchan->chan);
			}
			x = 0.05;
			y = ydim - 0.05;
			iref = 2;
			text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
			if (k != n) {
				if (bchan->sp) {
					sprintf (string, "%6.3f %7.3f", bchan->tcor+bchan->sp->tshift,
									bchan->acor*bchan->sp->acor);
					x = xxdim - 0.05;
					y = ydim - 0.05;
					iref = 8;
					text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
				}
			}
			if (ititle) {
				sprintf (string, "%s %s %s Azimuth = %.2f Velocity = %.2f", cc->refsta, bchan->sta,
									bchan->chan, cc->azimuth, 1.0/cc->slowi);
				height = 0.1;
				chrsiz_ (&height, &ratio, &slant);
				iclip = 1;
				x = 0.0;
				y = ydim + 0.05;
				iref = 0;
				text_ (&x, &y, &fzero, &iref, string, &iclip, strlen(string));
				ititle = 0;
			}
			hue = 0.0;
			light = 0.5;
			sat = 1.0;
			setfor_ (&hue, &light, &sat);
			xmin = 0.0;
			xmax = te-ts;
			setscl_ (&xmin, &xmax, &ymin, &ymax);
			x2 = 0.25*ydim;
			x3 = 0.75*ydim;
			for (i=0; i<ntimes; i++) {
				x1 = times[i] - ts;
				line_ (&x1, &x2, &x1, &x3, &thick, &ithick, &iclip);
			}
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
		}
	}

	/* Plot cross correlation spectra */

	if (ydimsp > 0.0) {
		n = maxtbl(cc->chans);
		for (k=-1; k<n; k++) {
			if (k < 0) {
				ytop = -1.e30;
				for (i=0; i<n; i++) {
					bchan = (Channelspec *) gettbl (cc->chans, i);
					if (bchan->sp == NULL) continue;
					for (j=0; j<bchan->sp->nf; j++) {
						arg = bchan->sp->fftr[j]*bchan->sp->fftr[j]+bchan->sp->ffti[j]*bchan->sp->ffti[j];
						amp = sqrt(arg);
						if (amp > ytop) ytop = amp;
					}
				}
				for (jref=0; jref<n; jref++) {
					bchan = (Channelspec *) gettbl (cc->chans, jref);
					if (!strcmp(bchan->sta, cc->refsta)) break;
				}
				ii = n;
			} else if (k < n) {
				if (k == jref) continue;
				bchan = (Channelspec *) gettbl (cc->chans, k);
			}
			if (bchan->sp) {
				z = (float *) my_malloc ("crosscor_plot: z", bchan->sp->nf*sizeof(float));
				if (z == NULL) {
 					fprintf (stderr, "crosscor_plot: malloc() error.\n");
 					return;
				}
 				yy = (float *) my_malloc ("crosscor_plot: yy", bchan->sp->nf*sizeof(float));
 				if (yy == NULL) {
 					fprintf (stderr, "crosscor_plot: malloc() error.\n");
 					return;
 				}
				for (i=0; i<bchan->sp->nf; i++) {
					arg = bchan->sp->fftr[i]*bchan->sp->fftr[i]+bchan->sp->ffti[i]*bchan->sp->ffti[i];
					z[i] = sqrt(arg);
					yy[i] = bchan->sp->df*i;
				}
			}
			xxdim = xdimsp;
			ydim = ydimsp/(n+1);
			xxlow = xlowsp;
			yylow = ylowsp;
			ylow = ylowsp + ydim*ii;
			ii--;
			xmin = fmin;
			if (fmax > 0.0) {
				xmax = fmax*1.00001;
			} else {
				xmax = yy[bchan->sp->nf-1];
			}
			ymin = 0.0;
			ymax = 1.1*ytop;
			setdim_ (&xxdim, &ydim, &xxlow, &ylow);
			setscl_ (&xmin, &xmax, &ymin, &ymax);
 			/*anno_freqs (xxdim, ydim, xxlow, ylow, ffmin, ffmax, 1, 1, "Frequency (hz)");*/
 			/*anno_amps (xxdim, ydim, xxlow, ylow, ymin, ymax, 0.0, dy, "%.1f", 1, 0, "Amplitude");*/
 			i = 0;
 			if (k == n-1) i = 1;
 			anno_amps (xxdim, ydim, xxlow, ylow, xmin, xmax, 0.0, 0.5, "%.1f", i, 1, "Frequency");
			iclip = 0;
			if (bchan->sp) {
				nplot_ (&bchan->sp->nf, yy, z, &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));
				for (i=0; i<bchan->sp->nf; i++) {
					arg1 = bchan->sp->ffti[i];
					arg2 = bchan->sp->fftr[i];
					arg = atan2(arg1, arg2)*180.0/M_PI;
					z[i] = arg;
				}
				ymin = -180.0;
				ymax = 180.0;
				setscl_ (&xmin, &xmax, &ymin, &ymax);
				x1 = xmin;
				y1 = 0.0;
				x2 = xmax;
				y2 = 0.0;
				line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
				hue = 0.0;
				light = 0.5;
				sat = 1.0;
				setfor_ (&hue, &light, &sat);
				nplot_ (&bchan->sp->nf, yy, z, &igraf, &iclip, &thick, &ithick, asymb, strlen(asymb));
				hue = 0.0;
				light = 0.0;
				sat = 0.0;
				setfor_ (&hue, &light, &sat);
				my_free (z);
				my_free (yy);
			}
			box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
		}
	}
	
}

int nlegend (xdim, ydim, xlow, ylow, zmin, zmax, dz)

float        xdim, ydim, xlow, ylow, zmin, zmax, dz;

{
	nlegend2 (xdim, ydim, xlow, ylow, zmin, zmax, dz, "Normalized Beam Power");
}

int nlegend2 (xdim, ydim, xlow, ylow, zmin, zmax, dz, title)

float        xdim, ydim, xlow, ylow, zmin, zmax, dz;
char *                                                title;

{
	float xxdim, yydim, xxlow, yylow;
	float zzmin, zzmax, ddz;
	float xmin, xmax, ymin, ymax;
	static float y[2], z[200];
	float zz;
	int i, n;
	static char ctype[] = "colors";
	char fmtx[32];
	char fmty[32];
	char labelx[80];
	char labely[80];
	int iclear;
	float xmarg=0.5, ymarg=0.5;
	float dxsmal, dxnumb, dysmal, dynumb;
	float scale;

	xxdim = xdim;
	yydim = ydim;
	xxlow = xlow;
	yylow = ylow;
	zzmin = zmin;
	zzmax = zmax;
	ddz = dz;

	xmin = zmin;
	xmax = zmax;
	ymin = 0.0;
	ymax = 1.0;
	setdim_ (&xxdim, &yydim, &xxlow, &yylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	y[0] = 0.0;
	y[1] = 1.0;
	for (i=0; i<100; i++) {
		zz = zmin + i*dz;
		if (zz > zmax+0.01) break;
		z[i] = zz;
	}
	n = i;
	for (i=0; i<n; i++) {
		zz = zmin + i*dz;
		z[i+n] = zz;
	}
	i = 2;
	set_ctable();
	ncontour_ (&n, &n, &i, z, y, z,  ctype,  &zzmin,  &zzmax,
	                    &ddz, strlen(ctype));
	dxsmal = 0.1;
	dxnumb = 0.1;
	dysmal = 0.0;
	dynumb = 0.0;
	iclear = 0;
	strcpy (fmtx, "(f4.1)");
	strcpy (fmty, "(none)");
	strcpy (labelx, " ");
	strcpy (labely, " ");
	scale = 0.8;
	sclsiz_ (&scale);
	scale = 0.0;
	sclthk_ (&scale);
	axis_ (&xxdim, &yydim,  &xmarg,  &ymarg,  &xxlow,  &yylow,  &xmax,
				&xmin,  &ymax,  &ymin, &dxsmal, &dxnumb, &dysmal, &dynumb, fmtx, fmty, labelx, labely, title,
				&iclear, strlen(fmtx), strlen(fmty), strlen(labelx), strlen(labely), strlen(title));
	scale = 1.0;
	sclsiz_ (&scale);
	scale = 1.0;
	sclthk_ (&scale);
}

int
set_ctable ()

{
	float hx, lx, sx, hp, lp, sp;
	float x;
	int i, j, n;

	n = 1;
	for (i=0; i<11; i++) {
		switch (i) {
		case 0:
		case 1:
			hx = 240.0;
			sx = 1.0;
			lx = 0.975;
			hp = 240.0;
			sp = 1.0;
			lp = 0.99;
			break;
		case 2:
			x = i / 10.0;
			hx = 240.0 * (1.0 - x);
			sx = 1.0;
			lx = 0.90;
			hp = 240.0 * (1.0 - x);
			sp = 1.0;
			lp = 0.965;
			break;
		case 3:
			x = i / 10.0;
			hx = 240.0 * (1.0 - x);
			sx = 1.0;
			lx = 0.85;
			lx = 0.75;
			hp = 240.0 * (1.0 - x);
			sp = 1.0;
			lp = 0.94;
			break;
		default:
			x = i / 10.0;
			hx = 240.0 * (1.0 - x);
			sx = 1.0;
			hp = 240.0 * (1.0 - x);
			sp = 1.0;
			if (hx <= 60.0) {
				lx = 0.95 - 0.10*hx/60.0;
				lp = 0.95 - 0.05*hp/60.0;
			} else if (hx <= 120.0) {
				lx = 0.85;
				lp = 0.90;	
			} else {
				lx = 0.85 + 0.05*(hx-120.0)/120.0;
				lp = 0.90 + 0.05*(hp-120.0)/120.0;
			}
			break;
		}
		j = i + 1;
		lx = 1.0 - (1.0 - lp)*2.0;
		if (hx <= 60.0) {
			lx = 0.95 - 0.10*hx/60.0;
		} else if (hx <= 120.0) {
			lx = 0.85;
		} else {
			lx = 0.85 + 0.05*(hx-120.0)/120.0;
		}
		hdctablespec_ (&j, &n, &hx, &lx, &sx, &hp, &lp, &sp);
       }
       i = 999;
       i = 3;
       hdctableset_ (&i);
}

int
anno_times (xdim, ydim, xlow, ylow, tstart, twin, textflg)

float       xdim, ydim, xlow, ylow;
double                              tstart, twin;
int                                               textflg;

{
	float xxdim, yydim, xxlow, yylow;
	float xxdimo, yydimo, xxlowo, yylowo;
	float xmin, xmax, ymin, ymax;
	float xmino, xmaxo, ymino, ymaxo;
	double nstart, ninc, ttime, time;
	int maxincs, ntimes;
	float hue, light, sat;
	float x1, y1, x2, y2;
	float thick = 0.0;
	int ithick = 0;
	int iclip = 0;
	int jfont = 114;
	float height=0.06, ratio=1.0, slant=0.0, angle=0.0;
	char string[80];
	struct date_time dt;
	int i, iref;

	getdim_ (&xxdimo, &yydimo, &xxlowo, &yylowo);
	getscl_ (&xmino, &xmaxo, &ymino, &ymaxo);
	xxdim = xdim;
	yydim = ydim;
	xxlow = xlow;
	yylow = ylow;
	setdim_ (&xxdim, &yydim, &xxlow, &yylow);
	xmin = 0.0;
	xmax = twin;
	ymin = 0.0;
	ymax = ydim;
	setscl_ (&xmin, &xmax, &ymin, &ymax);

	maxincs = 2.0*xdim + 0.1;
	get_nice_times (tstart, tstart+twin, maxincs, &nstart, &ninc);
	ntimes = 0;
	ttime = nstart;
	if (nstart >= tstart && nstart <= tstart+twin) {
		ntimes++;
	}
	if (ninc > 0.0) {
		while (1) {
			nstart += ninc;
			if (nstart < tstart) continue;
			if (nstart > tstart+twin) break;
			ntimes++;
		}
	}
	cfont_ (&jfont);
	chrsiz_ (&height, &ratio, &slant);
	for (i=0,time=ttime; i<ntimes; i++,time+=ninc) {
		x1 = time - tstart;
		x2 = x1;
		y1 = 0.0;
		y2 = ydim;
		hue = 0.0;
		light = 0.5;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
		iclip = 0;
		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
		hue = 0.0;
		light = 0.0;
		sat = 0.0;
		setfor_ (&hue, &light, &sat);
		if (!textflg) continue;
		y1 = - 0.05;
		iref = 5;
		iclip = 1;
		if (time > 100000.0) {
			dt.epoch = time;
			etoh (&dt);
			sprintf (string, "%2.2d:%2.2d:%04.1f",
					dt.hour, dt.minute, dt.second);
		} else {
			sprintf (string, "%.1f", time);
		}
		text_ (&x1, &y1, &angle, &iref, string, &iclip, strlen(string));
		y1 = - 0.05 - 1.5*height;
		if (time > 100000.0) {
			sprintf (string, "%4.4d%3.3d", dt.year, dt.doy);
		} else {
			sprintf (string, " ");
		}
		text_ (&x1, &y1, &angle, &iref, string, &iclip, strlen(string));
	}
	setdim_ (&xxdimo, &yydimo, &xxlowo, &yylowo);
	setscl_ (&xmino, &xmaxo, &ymino, &ymaxo);
}


int
anno_freqs (xdim, ydim, xlow, ylow, fmin, fmax, textflg, horflg, label)

float       xdim, ydim, xlow, ylow;
double                              fmin, fmax;
int                                             textflg;
int                                                      horflg;
char *                                                           label;

{
	float xxdim, yydim, xxlow, yylow;
	float xxdimo, yydimo, xxlowo, yylowo;
	float xmin, xmax, ymin, ymax;
	float xmino, xmaxo, ymino, ymaxo;
	double y, yy;
	float hue, light, sat;
	float x1, y1, x2, y2;
	float thick = 0.0;
	int ithick = 0;
	int iclip = 0;
	int jfont = 114;
	float height=0.08, ratio=1.0, slant=0.0, angle=0.0;
	float height2=0.06;
	char string[80], format[80];
	int i, j, iref;
	int ipow;

	getdim_ (&xxdimo, &yydimo, &xxlowo, &yylowo);
	getscl_ (&xmino, &xmaxo, &ymino, &ymaxo);
	xxdim = xdim;
	yydim = ydim;
	xxlow = xlow;
	yylow = ylow;
	setdim_ (&xxdim, &yydim, &xxlow, &yylow);
	if (horflg) {
		ymin = 0.0;
		ymax = ydim;
		xmin = log10(fmin);
		xmax = log10(fmax);
	} else {
		xmin = 0.0;
		xmax = xdim;
		ymin = log10(fmin);
		ymax = log10(fmax);
	}
	setscl_ (&xmin, &xmax, &ymin, &ymax);

	y = 1.0e30;
	ipow = 30;
	while (y >= fmin) {
		y *= 0.1;
		ipow--;
	}
	iclip = 1;
	cfont_ (&jfont);
	while (1) {
		if (y >= fmin*0.9999 && y <= fmax*1.0001) {
			if (horflg) {
				x1 = log10(y);
				x2 = x1;
				y1 = 0.0;
				y2 = ydim;
			} else {
				y1 = log10(y);
				y2 = y1;
				x1 = 0.0;
				x2 = xdim;
			}
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			if (horflg) {
				y1 = 0.2;
				y2 = 0.0;
			} else {
				x1 = 0.2;
				x2 = 0.0;
			}
			thick = 0.01;
			line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			if (horflg) {
				y1 = ydim;
				y2 = ydim - 0.2;
			} else {
				x1 = xdim;
				x2 = xdim - 0.2;
			}
			line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			thick = 0.0;
			if (textflg) {
				if (ipow >= 0) {
					j = y + 0.5;
					sprintf (string, "%d", j);
				} else {
					sprintf (format, "%%.%df", -ipow);
					sprintf (string, format, y);
				}
				if (horflg) {
					iref = 5;
					y1 = -0.03;
				} else {
					iref = 7;
					x1 = -0.03;
				}
				chrsiz_ (&height, &ratio, &slant);
				text_ (&x1, &y1, &angle, &iref, string, &iclip, strlen(string));
			}
		}
		for (i=1; i<10; i++) {
			yy = y*i;
			if (yy >= fmin*0.9999 && yy <= fmax*1.0001) {
				if (horflg) {
					x1 = log10(yy);
					x2 = x1;
					y1 = 0.0;
					y2 = ydim;
				} else {
					y1 = log10(yy);
					y2 = y1;
					x1 = 0.0;
					x2 = xdim;
				}
				hue = 0.0;
				light = 0.5;
				sat = 0.0;
				setfor_ (&hue, &light, &sat);
				line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
				hue = 0.0;
				light = 0.0;
				sat = 0.0;
				setfor_ (&hue, &light, &sat);
				if (horflg) {
					y1 = 0.1;
					y2 = 0.0;
				} else {
					x1 = 0.1;
					x2 = 0.0;
				}
				line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
				if (horflg) {
					y1 = ydim;
					y2 = ydim - 0.1;
				} else {
					x1 = xdim;
					x2 = xdim - 0.1;
				}
				line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			}
		}
		y *= 10.0;
		ipow++;
		if (y > fmax) break;
	}
	if (textflg && label) {
		if (horflg) {
			angle = 0.0;
			iref = 4;
			y1 = -0.3;
			x1 = 0.5*(xmin+xmax);
		} else {
			angle = 90.0;
			iref = 4;
			x1 = -0.3;
			y1 = 0.5*(ymin+ymax);
		}
		chrsiz_ (&height, &ratio, &slant);
		text_ (&x1, &y1, &angle, &iref, label, &iclip, strlen(label));
	}
	setdim_ (&xxdimo, &yydimo, &xxlowo, &yylowo);
	setscl_ (&xmino, &xmaxo, &ymino, &ymaxo);
}

int
anno_amps (xdim, ydim, xlow, ylow, amin, amax, a0, da, fmt, textflg, horflg, label)

float      xdim, ydim, xlow, ylow;
double                             amin, amax, a0, da;
char *                                                 fmt;
int                                                         textflg;
int                                                                  horflg;
char *                                                                       label;

{
	float xxdim, yydim, xxlow, yylow;
	float xxdimo, yydimo, xxlowo, yylowo;
	float xmin, xmax, ymin, ymax;
	float xmino, xmaxo, ymino, ymaxo;
	double y, yy;
	float hue, light, sat;
	float x1, y1, x2, y2;
	float thick = 0.0;
	int ithick = 0;
	int iclip = 0;
	int jfont = 114;
	float height=0.08, ratio=1.0, slant=0.0, angle=0.0;
	float height2=0.06;
	char string[80], format[80];
	int i, j, iref;
	int ipow;

	getdim_ (&xxdimo, &yydimo, &xxlowo, &yylowo);
	getscl_ (&xmino, &xmaxo, &ymino, &ymaxo);
	xxdim = xdim;
	yydim = ydim;
	xxlow = xlow;
	yylow = ylow;
	setdim_ (&xxdim, &yydim, &xxlow, &yylow);
	if (horflg) {
		ymin = 0.0;
		ymax = ydim;
		xmin = amin;
		xmax = amax;
	} else {
		xmin = 0.0;
		xmax = xdim;
		ymin = amin;
		ymax = amax;
	}
	setscl_ (&xmin, &xmax, &ymin, &ymax);

	y = a0;
	iclip = 1;
	cfont_ (&jfont);
	while (1) {
		if (y >= amin*0.9999 && y <= amax*1.0001) {
			if (horflg) {
				x1 = y;
				x2 = x1;
				y1 = 0.0;
				y2 = ydim;
			} else {
				y1 = y;
				y2 = y1;
				x1 = 0.0;
				x2 = xdim;
			}
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_ (&hue, &light, &sat);
			if (horflg) {
				y1 = 0.2;
				y2 = 0.0;
			} else {
				x1 = 0.2;
				x2 = 0.0;
			}
			thick = 0.01;
			/*line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);*/
			if (horflg) {
				y1 = ydim;
				y2 = ydim - 0.2;
			} else {
				x1 = xdim;
				x2 = xdim - 0.2;
			}
			/*line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);*/
			thick = 0.0;
			if (textflg) {
				sprintf (string, fmt, y);
				if (horflg) {
					iref = 5;
					y1 = -0.03;
				} else {
					iref = 7;
					x1 = -0.03;
				}
				chrsiz_ (&height, &ratio, &slant);
				text_ (&x1, &y1, &angle, &iref, string, &iclip, strlen(string));
			}
		}
		y += da;
		if (y > amax) break;
	}
	if (textflg && label) {
		if (horflg) {
			angle = 0.0;
			iref = 4;
			y1 = -0.3;
			x1 = 0.5*(xmin+xmax);
		} else {
			angle = 90.0;
			iref = 4;
			x1 = -0.3;
			y1 = 0.5*(ymin+ymax);
		}
		chrsiz_ (&height, &ratio, &slant);
		text_ (&x1, &y1, &angle, &iref, label, &iclip, strlen(label));
	}
	setdim_ (&xxdimo, &yydimo, &xxlowo, &yylowo);
	setscl_ (&xmino, &xmaxo, &ymino, &ymaxo);
}

/* $Id: plot_subs.c,v 1.1 2001-06-15 00:18:31 kent Exp $ */
