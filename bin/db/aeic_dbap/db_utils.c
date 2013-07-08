
#include "dbap_defines.h"

int verbose;

int
make_beam_arrival (dbi, dbo, arid, sta, chan, time, phase, azimuth, slow)

Dbptr              dbi, dbo;
int                          arid;
char *                             sta;
char *                                  chan;
double                                        time;
char *                                              phase;
double                                                     azimuth;
double                                                              slow;

{
	Dbptr dbarr;
	char expr[128];
	char line[2048];
	int arido;
	int n;

	if (arid > 0) {
		dbarr = dblookup (dbi, 0, "arrival", 0, 0);
		dbquery (dbarr, dbRECORD_COUNT, &n);
		for (dbarr.record=0; dbarr.record<n; dbarr.record++) {
			dbgetv (dbarr, 0, "arid", &arido, NULL);
			if (arid == arido) break;
		}
		if (dbarr.record == n) {
			clear_register (1);
			fprintf (stderr, "make_beam_arrival: Unable to find arid %d.\n", arid);
			return (0);
		}
		if (dbget(dbarr, line) == dbINVALID) {
			clear_register (1);
			fprintf (stderr, "make_beam_arrival: Unable to find arid %d.\n", arid);
			return (0);
		}
		dbo = dblookup (dbo, 0, "arrival", 0, 0);
		dbo.record = dbadd (dbo, line);
		arido = dbnextid (dbo, "arid");
		sprintf (expr, "dbap:%s", cuserid((char *) NULL));
		if (dbputv(dbo, 0, "arid", arido, "sta", sta, "chan", chan,
					"azimuth", azimuth, "slow", slow, "auth", expr, NULL) == dbINVALID) {
			clear_register (1);
			fprintf (stderr, "make_beam_arrival: dbputv() error for arrival.\n");
			return (0);
		}
	} else {
		sprintf (expr, "dbap:%s", cuserid((char *) NULL));
		if ((dbo.record=dbaddv(dbo, "arrival", "sta", sta, "chan", chan, "time", time, "iphase", phase,
					"azimuth", azimuth, "slow", slow, "auth", expr, NULL)) == dbINVALID) {
			clear_register (1);
			fprintf (stderr, "make_beam_arrival: dbaddv() error for arrival.\n");
			return (0);
		}
		dbgetv (dbo, "arrival", "arid", &arido, NULL);
	}
	return (arido);
}

int
write_beam (dbi, recipe, beam, tstart, tend, db)

Dbptr       dbi;
char *          recipe;
Apspec *                 beam;
double                         tstart;
double                                 tend;
Dbptr                                        db;

{
	int nbeams;
	int i, j, n;
	int overwrite=0;
	char dir[128], dfile[128];
	int year, day, hour, minute, isec;
	double sec, tend2, time1, time2;
	Dbptr dbwf;
	int wfid;
	Channelspec *beamchan;
	char sta[32], refsta[32], staname[64], statype[16];
	int ondate, offdate;
	double lat, lon, elev, edepth, dnorth, deast;
	char chan[32], instant[16];
	double time, endtime, calratio, calper, tshift;
	double ontime, offtime, hang, vang;
	char descrip[64], ctype[16];
	char insname[64], instype[32], band[16], digital[16], rsptype[16];
	double samprate, ncalib, ncalper;
	int inid, chanid, jdate;
	Tbl *match_tbl;
	Dbptr dbi2;
	int rec;

	static void *hook1=NULL;
	static void *hook2=NULL;
	static int first = 1;
	static Dbptr dbv;

	strcpy (dir, "./wfbeam");
	if (beam->tr == NULL) return (1);
	beam->tr = (Trace *) convert_trace (beam->tr, "t4");
	if (beam->tr == NULL) {
		fprintf (stderr, "write_beam: convert_trace() error.\n");
		return (0);
	}
	e2h (tstart, &year, &day, &hour, &minute, &sec);
	isec = sec + 0.5;
	sprintf (dfile, "%d:%2.2d:%2.2d:%2.2d.%s.%s.w", (year-1900)*1000+day,
						hour, minute, isec, beam->array, beam->chan);
	if (!write_trace (db, beam->array, beam->chan, dir, dfile, 
						beam->tr, tstart, tend, overwrite, &dbwf)) {
		fprintf (stderr, "write_beam: write_trace() error.\n");
		beam->tr = (Trace *) SCV_trace_tofloat (beam->tr, 0);
		return (0);
	}
	beam->tr = (Trace *) SCV_trace_tofloat (beam->tr, 0);
	dbgetv (dbwf, 0, "wfid", &wfid, NULL);
	sprintf (dir, "dbap:%s", cuserid((char *) NULL));
	n = maxtbl (beam->chans);
	for (i=0; i<n; i++) {
		beamchan = (Channelspec *) gettbl (beam->chans, i);
		if (!strcmp(beam->refsta, beamchan->sta)) break;
	}
	if (i == n) {
		fprintf (stderr, "write_beam: Unable to find refsta '%s'.\n", beam->refsta);
		return (0);
	}
	if (beamchan->filter && *(beamchan->filter)) {
		strcpy (dfile, beamchan->filter);
	} else {
		strcpy (dfile, "none");
	}
	if (dbaddv (db, "beam", "wfid", wfid,
				"azimuth", beam->azimuth,
				"slo", beam->slowi,
				"filter", dfile,
				"recipe", recipe,
				"algorithm", "slant-stack",
				"auth", dir, NULL) == dbINVALID) {
	    	fprintf (stderr, "write_beam: dbaddv() error.\n");
	    	clear_register (1);
	    	return (0);
	}
	rec = dbwf.record;
	dbwf.record = dbSCRATCH;
	dbputv (dbwf, 0, "sta", beam->refsta, NULL);
	dbi = dblookup (dbi, 0, "site", 0, 0);
	n = dbmatches (dbwf, dbi, 0, 0, &hook1, &match_tbl);
	if (n == dbINVALID) {
		fprintf (stderr, "write_beam: dbmatches() error.\n");
		clear_register (1);
		return (0);
	}
	if (n < 1) {
		fprintf (stderr, "write_beam: Unable to find site entry.\n");
		return (0);
	}
	dbi.record = (int) gettbl (match_tbl, 0);
	freetbl (match_tbl, 0);
	if (dbgetv (dbi, 0, "ondate", &ondate, "offdate", &offdate,
				"lat", &lat, "lon", &lon, "elev", &elev, NULL) == dbINVALID) {
		fprintf (stderr, "write_beam: dbgetv() error.\n");
		clear_register (1);
		return (0);
	}
	strcpy (staname, "-");
	strcpy (statype, "bm");
	strcpy (refsta, "-");
	dnorth = 0.0;
	deast = 0.0;
	ontime = epoch (ondate);
	if (offdate == -1) {
		offtime = 999999999999.999;
	} else {
		offtime = epoch (offdate);
	}
	if (!merge_site (db, beam->array, ontime, offtime, lat, lon, elev, staname, statype,
	                refsta, dnorth, deast)) {
		fprintf (stderr, "write_beam: merge_site() error.\n");
		return (0);
	}
	dbi = dblookup (dbi, 0, "sensor", 0, 0);
	n = dbmatches (dbwf, dbi, 0, 0, &hook2, &match_tbl);
	if (n == dbINVALID) {
		fprintf (stderr, "write_beam: dbmatches() error.\n");
		clear_register (1);
		return (0);
	}
	if (n < 1) {
		fprintf (stderr, "write_beam: Unable to find sensor entry.\n");
		return (0);
	}
	dbi.record = (int) gettbl (match_tbl, 0);
	freetbl (match_tbl, 0);
	if (dbgetv(dbi, 0, "chanid", &chanid, NULL) == dbINVALID) {
		fprintf (stderr, "write_beam: dbgetv() error.\n");
		clear_register (1);
		return (0);
	}
	if (dbputv(dbwf, 0, "chanid", chanid, NULL) == dbINVALID) {
		fprintf (stderr, "write_beam: dbputv() error.\n");
		clear_register (1);
		return (0);
	}
	if (first) {
		first = 0;
		dbi2 = dblookup (dbi, 0, "sitechan", 0, 0);
		dbv = dbjoin (dbi, dbi2, 0, 0, 1, 0, 0);
		dbi2 = dblookup (dbi, 0, "instrument", 0, 0);
		dbv = dbjoin (dbv, dbi2, 0, 0, 1, 0, 0);
		if (dbv.table == dbINVALID) {
			fprintf (stderr, "write_beam: Unable to join input sensor-sitechan-instrument.\n");
			clear_register (1);
			return (0);
		}
		dbquery (dbv, dbRECORD_COUNT, &n);
	}
	n = dbmatches (dbwf, dbv, 0, 0, &hook2, &match_tbl);
	if (n == dbINVALID) {
		fprintf (stderr, "write_beam: dbmatches() error.\n");
		clear_register (1);
		return (0);
	}
	if (n < 1) {
		fprintf (stderr, "write_beam: Unable to find sensor-sitechan-instrument entry.\n");
		return (0);
	}
	dbv.record = (int) gettbl (match_tbl, 0);
	freetbl (match_tbl, 0);
	if (dbgetv (dbv, 0, "sta", sta,
				"chan", chan,
				"time", &time,
				"endtime", &endtime,
				"inid", &inid,
				"chanid", &chanid,
				"jdate", &jdate,
				"calratio", &calratio,
				"calper", &calper,
				"tshift", &tshift,
				"instant", instant, 
				"ondate", &ondate,
				"offdate", &offdate,
				"ctype", ctype,
				"edepth", &edepth,
				"hang", &hang,
				"vang", &vang, 
				"descrip", descrip,
				"insname", insname,
				"instype", instype,
				"band", band,
				"digital", digital,
				"samprate", &samprate,
				"ncalib", &ncalib,
				"ncalper", &ncalper,
				"dir", dir,
				"dfile", dfile,
				"rsptype", rsptype, NULL) == dbINVALID) {
		fprintf (stderr, "write_beam: dbgetv() error.\n");
		clear_register (1);
		return (0);
	}
	ontime = epoch (ondate);
	if (offdate == -1) {
		offtime = 999999999999.999;
	} else {
		offtime = epoch (offdate);
	}
	strcpy (ctype, "b");
	if ((chanid=merge_sitechan (db, beam->array, beam->chan, ontime, offtime, ctype, edepth, hang, vang,
	                descrip)) == 0) {
		fprintf (stderr, "write_beam: merge_sitechan() error.\n");
		return (0);
	}
	dbwf.record = rec;
	if (dbputv(dbwf, 0, "chanid", chanid, NULL) == dbINVALID) {
		fprintf (stderr, "write_beam: dbputv() error.\n");
		clear_register (1);
		return (0);
	}
	if ((inid=merge_instrument (db, insname, instype, band, digital, samprate, ncalib, ncalper,
	                dir, dfile, rsptype)) == 0) {
		fprintf (stderr, "write_beam: merge_instrument() error.\n");
		return (0);
	}
	if (!merge_sensor (db, beam->array, beam->chan, time, endtime, inid, chanid, jdate,
	                calratio, calper, tshift, instant)) {
		fprintf (stderr, "write_beam: merge_sensor() error.\n");
		return (0);
	}

	/* Normal exit */

	return (1);
}

int
write_pows (grids, tstart, tend, db, raw, chan, az, sl)

Tbl *       grids;
double             tstart;
double                     tend;
Dbptr                            db;
int                                  raw;
char *                                    chan;
int                                             az;
int                                                 sl;

{
	int ngrids;
	int i, j, n;
	Apspec *grid;
	int overwrite=1;
	char dir[128], dfile[128];
	int year, day, hour, minute, isec;
	double sec, tst;
	Trace *tot=NULL;
	float *data, *datai;
	Dbptr dbwf;
	Trace *grid_power;

	if (db.database < 0) return (1);
	ngrids = maxtbl(grids);
	strcpy (dir, "./wfgrid");
	for (i=0; i<ngrids; i++) {
		grid = (Apspec *) gettbl (grids, i);
		if (raw) grid_power = grid->grid_raw_power;
		else if (az) grid_power = grid->grid_raw_az;
		else if (sl) grid_power = grid->grid_raw_sl;
		else grid_power = grid->grid_power;
		if (grid_power == NULL) continue;
		if (ngrids > 1) {
			if (tot == NULL) {
				tot = (Trace *) copy_trace (grid_power, 1);
				if (tot == NULL) {
					fprintf (stderr, "write_pows: copy_trace() error.\n");
					return (0);
				}
			} else {
				data = tot->raw_data;
				datai = grid_power->raw_data;
				for (j=0; j<tot->nsamps; j++) data[j] *= datai[j];
			}
		}
		tst = tstart;
		if (tst < grid_power->tstart) tst = grid_power->tstart;
		e2h (tst, &year, &day, &hour, &minute, &sec);
		isec = sec + 0.5;
		sprintf (dfile, "%d:%2.2d:%2.2d:%2.2d.%s.%s.w", (year-1900)*1000+day,
					hour, minute, isec, grid->refsta, chan);
		if (!write_trace (db, grid->refsta, chan, dir, dfile, 
						grid_power, tstart, tend, overwrite, &dbwf)) {
			fprintf (stderr, "write_pows: write_trace() error.\n");
			return (0);
		}
	}
	if (tot) {
		sprintf (dfile, "%d:%2.2d:%2.2d:%2.2d.%s.%s.w", (year-1900)*1000+day,
					hour, minute, isec, grid->refsta, "powt");
		if (!write_trace (db, grid->refsta, "powt", dir, dfile, 
						tot, tstart, tend, overwrite, &dbwf)) {
			fprintf (stderr, "write_pows: write_trace() error.\n");
			return (0);
		}
	}

	/* Normal exit */

	return (1);
}

int
write_stgrid (grid, dbo, igrwr)

Apspec *grid;
Dbptr dbo;
int igrwr;

{
	int rec;
	static int instance=0;
	int fd;
	char outdir[1024];
	char outbase[1024];
	char fname[1024];
	char dir[128], dfile[128];
	Dbvalue dbv;
	int overwrite = 1;
	int i, n;
	Channelspec *gchan;
	double time, endtime;
	int ret, size;

	if (dbo.database < 0) return (1);
	gchan = (Channelspec *) gettbl (grid->chans, 0);
	time = grid->t0;
	endtime = time + grid->dt*(grid->nt-1);
	if ((rec=dbaddv (dbo, "stgrid", 
			"sta", grid->array, 
			"refsta", grid->refsta, 
			"chan", gchan->chan,
			"time", time, 
			"endtime", endtime,
			"twin", grid->twin,
			"filter", gchan->filter,
			"azimuth", grid->azimuth,
			"smin", grid->smin,
			"smax", grid->smax,
			"ns", grid->ns,
			"dtime", grid->dt,
			"nt", grid->nt, NULL)) < 0) {
		clear_register (1);
		fprintf (stderr, "write_stgrid: dbaddv() error.\n");
		return (0);
	}
	if (!igrwr) return (1);
	instance++;
	sprintf (dfile, "%5.5d.%4.4d.stgr", getpid(), instance);
	strcpy (dir, "./stgrids");
	dbo = dblookup (dbo, 0, "stgrid", 0, 0);
	if (dir[0] == '/') {
		sprintf (fname, "%s/%s", dir, dfile);
	} else {
		dbquery (dbo, dbTABLE_DIRNAME, &dbv);
		strcpy (outbase, dbv.t);
		sprintf (fname, "%s/%s/%s", outbase, dir, dfile);
	}
	if (!overwrite) {
		if (zaccess(fname, F_OK) != -1) {
			fprintf (stderr, "write_stgrid: wf file conflict for '%s'.\n",
								fname);
			return (0);
		}
	}
	dirbase (fname, outdir, outbase);
	if (makedir(outdir) == -1) {
                fprintf (stderr, "write_stgrid: Unable to create %s\n", outdir);
                return (0);
        }
	fd = open (fname, (O_RDWR|O_CREAT|O_TRUNC), 0666);
	if (fd < 0) {
		fprintf (stderr, "write_stgrid: Open error on '%s'.\n", fname);
		return (0);
	}
	size = sizeof(float)*grid->ns*grid->nt;
	ret = write (fd, grid->norm_power_grid, size);
	close (fd);
	if (ret < size) {
		fprintf (stderr, "write_stgrid: Write error on '%s'.\n", fname);
		return (0);
	}
        dbo.record = rec;
        dbputv (dbo, "stgrid", "datatype", "t4", "dir", dir, "dfile", dfile, NULL);
	return (1);
}


int
write_fkgrid (grid, dbo, igrwr, igrids)

Apspec *grid;
Dbptr dbo;
int igrwr;

{
	int rec;
	static int instance=0;
	int fd;
	char outdir[1024];
	char outbase[1024];
	char fname[1024];
	char dir[128], dfile[128];
	Dbvalue dbv;
	int overwrite = 1;
	int i, n;
	Channelspec *gchan;
	double time, endtime;
	int ret, size;
	int ngrids;
	Grid *grd;

	if (dbo.database < 0) return (1);
	gchan = (Channelspec *) gettbl (grid->chans, 0);
	if (igrids) {
		time = grid->ts;
		endtime = time + grid->dt*(grid->nt-1);
	} else {
		time = grid->summary_grid.tstart;
		endtime = grid->summary_grid.tend;
	}
	if ((rec=dbaddv (dbo, "fkgrid", 
			"sta", grid->array, 
			"refsta", grid->refsta, 
			"chan", gchan->chan,
			"time", time, 
			"endtime", endtime,
			"twin", grid->twin,
			"filter", gchan->filter,
			"dtime", grid->dt,
			"nt", grid->nt,
			"azimuth", grid->summary_grid.peak_azimuth,
			"slo", grid->summary_grid.peak_slow,
			"slowd", grid->summary_grid.slow_width,
			"ppower", grid->summary_grid.peak_power,
			"semin", grid->xmin,
			"semax", grid->xmax,
			"ne", grid->nx,
			"snmin", grid->ymin,
			"snmax", grid->ymax,
			"nn", grid->ny, NULL)) < 0) {
		clear_register (1);
		fprintf (stderr, "write_fkgrid: dbaddv() error.\n");
		return (0);
	}
	if (!igrwr) return (1);
	instance++;
	sprintf (dfile, "%5.5d.%4.4d.fkgr", getpid(), instance);
	strcpy (dir, "./fkgrids");
	dbo = dblookup (dbo, 0, "fkgrid", 0, 0);
	if (dir[0] == '/') {
		sprintf (fname, "%s/%s", dir, dfile);
	} else {
		dbquery (dbo, dbTABLE_DIRNAME, &dbv);
		strcpy (outbase, dbv.t);
		sprintf (fname, "%s/%s/%s", outbase, dir, dfile);
	}
	if (!overwrite) {
		if (zaccess(fname, F_OK) != -1) {
			fprintf (stderr, "write_fkgrid: wf file conflict for '%s'.\n",
								fname);
			return (0);
		}
	}
	dirbase (fname, outdir, outbase);
	if (makedir(outdir) == -1) {
                fprintf (stderr, "write_fkgrid: Unable to create %s\n", outdir);
                return (0);
        }
	fd = open (fname, (O_RDWR|O_CREAT|O_TRUNC), 0666);
	if (fd < 0) {
		fprintf (stderr, "write_fkgrid: Open error on '%s'.\n", fname);
		return (0);
	}
	if (igrids) {
		size = sizeof(float)*grid->nx*grid->ny;
		ngrids = maxtbl(grid->grids);
		for (i=0; i<ngrids; i++) {
			grd = (Grid *) gettbl(grid->grids, i);
			ret = write (fd, grd->power, size);
			if (ret < size) {
				fprintf (stderr, "write_fkgrid: Write error on '%s'.\n", fname);
				return (0);
			}
		}
		close (fd);
	} else {
		size = sizeof(float)*grid->nx*grid->ny;
		ret = write (fd, grid->norm_summary_grid.power, size);
		close (fd);
		if (ret < size) {
			fprintf (stderr, "write_fkgrid: Write error on '%s'.\n", fname);
			return (0);
		}
	}
        dbo.record = rec;
        dbputv (dbo, "fkgrid", "datatype", "t4", "dir", dir, "dfile", dfile, NULL);
	return (1);
}

int
write_stacor (cc, db)

Apspec *      cc;
Dbptr             db;

{
	int rec;
	int i, n;
	Channelspec *cchan;
	char string[256];

	n = maxtbl(cc->chans);
	sprintf (string, "dbap:%s", cuserid((char *) NULL));
	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan->sp == NULL) continue;
		if (cchan->wt == 0.0) continue;
		if (verbose) {
			printf ("Writing stacor %s: %.3f %.3f %d\n", cchan->sta,cchan->tcor+cchan->sp->tshift,
						cchan->acor*cchan->sp->acor, cchan->sp->orid);
		}
		if ((rec=dbaddv (db, "stacor",	"sta", cchan->sta,
						"cortype", "c",
						"refsta", cc->refsta,
						"tcor", cchan->tcor+cchan->sp->tshift,
						"acor", cchan->acor*cchan->sp->acor,
						"filter", cchan->filter,
						"orid", cchan->sp->orid,
						"azimuth", cchan->sp->azimuth,
						"slow", cchan->sp->slow,
						"phase", cchan->sp->phase,
						"auth", string,
						NULL)) < 0) {
			clear_register (1);
			fprintf (stderr, "write_stacor: dbaddv() error.\n");
			return (0);
		}
	}
	return (1);
}

/* $Id: db_utils.c,v 1.1 2001-06-15 00:18:28 kent Exp $ */
