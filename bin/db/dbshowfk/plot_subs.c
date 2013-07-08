#include "dbap_defines.h"
#include "stock.h"

char *Program_Name;

char *dbin;
Tbl *sc;
Dbptr dbi;

int has_plotted=0;

char grid_sta[32] = "";
char grid_chan[32] = "";
double grid_tstart = -999.0;
double grid_tend;

int
dsap_init_plot (itran, size, fname, progname, dbname)

int itran;
double size;
char *fname;
char *progname;
char *dbname;

{
	float ssize=0.95;
	float xwin=1.0;
	float ywin=0.25;
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
	char dbbase[FILENAME_MAX];
	char dbdir[FILENAME_MAX];

	height /= size;

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
	height /= size;
	iref = 0;
        cfont_ (&jfont);
        chrsiz_ (&height, &ratio, &slant);
        /*text_ (&xplt, &yplt, &angle, &iref, "DSAP", &iclip, strlen("DSAP"));*/
	jfont = 115;
        cfont_ (&jfont);
        itime = time(NULL);
	dirbase(dbname, dbdir, dbbase);
	sprintf (program, "%s: %s", progname, dbbase);
	    program[strlen(program)] = '\0';
	    xplt = 0.5;
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
}

int
dsap_finit ()

{
	has_plotted = 0;
	finitt_ ();
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
		dsap_init_plot (itrans, size, plotfile, Program_Name, dbin);
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
	dsap_init_plot (itrans, size, plotfile, Program_Name, dbin);
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
	sprintf (title, "%s %s az = %.2f, v = %s km/s", 
			grid->array, gridchan->chan, az2, v2);
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
	height = 0.12;
	height /= size;
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

/* $Id: plot_subs.c,v 1.1.1.1 2000-05-23 23:27:57 kent Exp $ */
