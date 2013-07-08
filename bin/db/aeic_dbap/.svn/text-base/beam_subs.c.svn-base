
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

#define	RADIAL		1
#define	TRANSVERSE	2
#define	NORTH		3
#define	EAST		4

#define ABS(x)  ((x)<0.0?-(x):(x))
#define INT(x)  ((x)<0.0?((x)-0.5):((x)+0.5))
#define VERBOSE	(verbose > 1)

Dbptr dbi;
char *dbin;

Trace *read_sc();
Trace *filter_trace();
Trace *make_beam();
double s2n();

int verbose;

int
read_gather (db, scsin, tstart, tend, recipes)

Dbptr        db;
Tbl *            scsin;
double                  tstart;
double                          tend;
Tbl *                                 recipes;

{
	int ngrids;
	Tbl *grids;
	int i, nn;
	char *name;
	int itype;
	void *ptr;
	Apspec *grid;
	double t1, t2, tpad;

	/* Set up time limits */

	ngrids = maxtbl(recipes);
	if (ngrids < 1) {
		fprintf (stderr, "read_gather: No gathers to read.\n");
		return (0);
	}
	grids = newtbl(1);
	if (grids == NULL) {
		fprintf (stderr, "read_gather: newtbl() error.\n");
		return (0);
	}
	if (VERBOSE) printf ("In form_ftgrids...\n");
	for (i=0,tpad=0.0,nn=0; i<ngrids; i++) {
		if (!get_process_recipe (recipes, i, &name, &itype, &ptr)) {
			fprintf (stderr, "form_ftgrids: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case GATHER:
			grid = ptr;
			nn++;
			break;
		default:
			grid = NULL;
			break;
		}
		if (grid == NULL) continue;
		if (tstart == 0.0 && tend == 0.0) {
			tstart = grid->ts;
			tend = grid->te;
		}
		grid->ts = tstart;
		grid->te = tend;
		if (grid->tpad > tpad) tpad = grid->tpad;
		if (settbl(grids, -1, grid) < 0) {
			fprintf (stderr, "form_ftgrids: settbl() error.\n");
			return (0);
		}
	}
	if (nn == 0) {
		return (1);
	}
	ngrids = nn;

	t1 = tstart - tpad;
	t2 = tend + tpad;
	if (!fill_grids (grids, scsin, t1, t2, 0.0)) {
		fprintf (stderr, "read_gather: fill_grids() error.\n");
		return (0);
	}

	/* Normal exit. */

	return (1);
}

int
remove_beam (db, gather, beams)

Dbptr        db;
Apspec *         gather;
Tbl *                    beams;

{
	int i, j, k, n, nb;
	Channelspec *chan, *bchan;
	Tbl *grids;
	double tshift, time, ts, te;
	Trace *tr, *trb;
	float cwt;
	int good;
	int ioff;
	int nbeams;
	Apspec *beam;

	if (VERBOSE) printf ("In remove_beam...\n");
	if (gather->type != GATHER) {
		fprintf (stderr, "remove_beam: Not the right object type.\n");
		return (0);
	}
	nbeams = maxtbl(beams);
	if (nbeams < 1) {
		fprintf (stderr, "remove_beam: No beams to process.\n");
		return (0);
	}
	for (i=0; i<nbeams; i++) {
		beam = (Apspec *) gettbl (beams, i);
		if (beam->type != BEAM) {
			fprintf (stderr, "remove_beam: Not the right object type.\n");
			return (0);
		}
		if (beam->tr == NULL) {
			fprintf (stderr, "remove_beam: No beam trace.\n");
			return (1);
		}
	}

	/* Copy in the gather data channels */

	grids = newtbl(1);
	if (grids == NULL) {
		fprintf (stderr, "remove_beam: newtbl() error.\n");
		return (0);
	}
	if (settbl(grids, -1, gather) < 0) {
		fprintf (stderr, "remove_beam: settbl() error.\n");
		return (0);
	}
	if (!fill_grids (grids, 0, gather->ts-gather->tpad, gather->te+gather->tpad, 0.0)) {
		fprintf (stderr, "read_gather: fill_grids() error.\n");
		return (0);
	}

	/* Loop through the gather channels */

	n = maxtbl(gather->chans);
	for (i=0; i<n; i++) {
		chan = (Channelspec *) gettbl (gather->chans, i);
		if (chan->tr == NULL) continue;

		/* Get the beam time shift and amplitude correction for this channel */

		for (k=0; k<nbeams; k++) {
			beam = (Apspec *) gettbl (beams, k);
			if (beam->type != BEAM) continue;
			if (beam->tr == NULL) continue;
			nb = maxtbl(beam->chans);
			for (j=0; j<nb; j++) {
				bchan = (Channelspec *) gettbl (beam->chans, j);
				if (strcmp(chan->sta, bchan->sta)) continue;
				if (strcmp(chan->chan, bchan->chan)) continue;
				break;
			}
			if (j < nb) break;
		}
		if (j == nb) {
			fprintf (stderr, "remove_beam: Cannot find beam channel %s %s.\n", chan->sta,chan->chan);
			SCV_free_trace (chan->tr);
			chan->tr = NULL;
			continue;
		}
		tshift = bchan->tshift+bchan->tcor;
		cwt = 1.0/bchan->acor;

		/* Remove the beam trace from the gather channel trace */

		for (tr=chan->tr; tr!=NULL; tr=tr->next) {
			for (j=0; j<tr->nsamps; j++) {
				time = tr->tstart + j*tr->dt;
				good = 0;
				for (trb=beam->tr; trb!=NULL; trb=trb->next) {
					ioff = INT((time+tshift-trb->tstart)/trb->dt);
					if (ioff < 0) continue;
					if (ioff >= trb->nsamps) continue;
					tr->data[j] -= cwt*trb->data[ioff];
					good = 1;
					break;
				}
				if (!good) {
					tr->data[j] = 1.e30;
				}
			}
		}

		/* Fix up the gather trace */

		chan->tr = (Trace *) convert_trace (chan->tr, "t4");
		chan->tr = (Trace *) SCV_trace_fixgaps (chan->tr, "segment");
		chan->tr = (Trace *) SCV_trace_tofloat (chan->tr, 0);
		if (chan->tr == NULL) {
			fprintf (stderr, "remove_beam: Problem fixing gather channel.\n");
			return (0);
		}
	}

	/* Normal exit. */

	return (1);
}

int
form_ftgrids (db, scsin, gather, tstart, tend, gwin, twin, recipes)

Dbptr         db;
Tbl *             scsin;
Apspec *                 gather;
double                           tstart;
double                                   tend;
double                                         gwin;
double                                               twin;
Tbl *                                                      recipes;

{
	int ngrids, nchans;
	Tbl *grids;
	int i, j, k, n, nn;
	double tpad, t1, t2, time;
	char *name;
	int itype;
	void *ptr;
	Apspec *grid;
	Tbl *scs;
	int igr;
	Channelspec *gchan;
	double tsfr, tefr;
	double dt;
	int nt, nf;
	float *t, *f, *amp, *phase;
	float *ampss;
	int first;
	double sx, sy;
	Trace *beam_tr;
	double tdel, wt;
	float *amps;
	int ia0, it0, nt0;

	/* Set up time limits */

	ngrids = maxtbl(recipes);
	if (ngrids < 1) {
		fprintf (stderr, "form_ftgrids: No grids to compute.\n");
		return (0);
	}
	grids = newtbl(1);
	if (grids == NULL) {
		fprintf (stderr, "form_ftgrids: newtbl() error.\n");
		return (0);
	}
	if (VERBOSE) printf ("In form_ftgrids...\n");
	for (i=0,tpad=0.0,nn=0; i<ngrids; i++) {
		if (!get_process_recipe (recipes, i, &name, &itype, &ptr)) {
			fprintf (stderr, "form_ftgrids: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case FTGRID:
			grid = ptr;
			nn++;
			break;
		default:
			grid = NULL;
			break;
		}
		if (grid == NULL) continue;
		grid->ts = tstart;
		grid->te = tend;
		grid->gather = gather;
		if (grid->tpad > tpad) tpad = grid->tpad;
		if (settbl(grids, -1, grid) < 0) {
			fprintf (stderr, "settbl() error.\n");
			return (0);
		}
	}
	if (nn == 0) {
		return (1);
	}
	ngrids = nn;

	/* Make the station-channel table */

	scs = scsin;
	n = maxtbl(scs);
	if (n < 1) {
		fprintf (stderr, "form_ftgrids: No data to process.\n");
		return (0);
	}

	/* Set up grid channel geometry */

	if (!setup_geom (db, grids)) {
		fprintf (stderr, "form_ftgrids: setup_geom() error.\n");
		return (0);
	}

	if (twin > tend-tstart || twin == 0.0) twin = tend - tstart + 1.0;

	/* Batch processing loop */

	time = tstart;
	igr = 0;
	if (verbose == 1) {
		printf ("Processing");
		fflush (stdout);
	}
	while (1) {
		if (VERBOSE) printf ("Frame #%d...\n", igr++);
		t1 = time - gwin - tpad;
		t2 = time + twin + gwin + tpad;
		tsfr = time;
		tefr = time + twin;
		if (tefr > tend) tefr = tend;
		if (!fill_grids (grids, scs, t1, t2, gwin)) {
			fprintf (stderr, "form_ftgrids: fill_grids() error.\n");
			return (0);
		}
		/* Grid loop */

		for (j=0; j<ngrids; j++) {
			grid = (Apspec *) gettbl (grids, j);
			grid->twin = twin;

			/* Make the beam and process */

			sy = grid->slowi*cos(grid->azimuth*M_PI/180.0);
			sx = grid->slowi*sin(grid->azimuth*M_PI/180.0);
			beam_tr = make_beam (grid->chans, sx, sy, t1, t2);
			if (beam_tr == NULL) {
				fprintf (stderr, "form_ftgrids: make_beam() error.\n");
				return (0);
			}
			if (!spgrm (beam_tr, tsfr, tefr, grid->fmin, grid->fmax, gwin, &nt, &t, &nf, &f,
							&amp, &phase)) {
				fprintf (stderr, "form_ftgrids: spgrm() error.\n");
				return (0);
			}
			if (grid->t == NULL) {
				grid->t = t;
				grid->nt = nt;
				grid->s = f;
				grid->ns = nf;
				grid->norm_power_grid = amp;
				my_free (phase);
				grid->dt = t[1] - t[0];
				grid->t0 = tsfr;
				it0 = 0;
				ia0 = 0;
			} else {
				grid->t = (float *) my_realloc ("form_ftgrids: grid->t",
								grid->t, (grid->nt + nt)*sizeof(float));
				if (grid->t == NULL) {
					fprintf (stderr, "form_ftgrids: malloc() error.\n");
					return (0);
				}
				for (k=0; k<nt; k++) grid->t[grid->nt+k] = t[k] + tsfr - grid->t0;
				it0 = grid->nt;
				ia0 = (grid->nt)*(grid->ns);
				grid->nt += nt;
				if (nf != grid->ns) {
					fprintf (stderr, "form_ftgrids: Frequencies dont match.\n");
					return (0);
				}
				for (k=0; k<nf; k++) {
					if (grid->s[k] != f[k]) {
						fprintf (stderr, "form_ftgrids: Frequencies dont match.\n");
						return (0);
					}
				}
				my_free (t);
				my_free (f);
				my_free (phase);
				grid->norm_power_grid = (float *) my_realloc ("form_ftgrids: grid->norm_power_grid",
							grid->norm_power_grid, (grid->nt)*(grid->ns)*sizeof(float));
				if (grid->norm_power_grid == NULL) {
					fprintf (stderr, "form_ftgrids: malloc() error.\n");
					return (0);
				}
				for (k=0; k<nt*nf; k++) {
					grid->norm_power_grid[ia0+k] = amp[k];
				}
				my_free (amp);
			}
			nt0 = nt;
			amps = (float *) my_malloc ("form_ftgrids: amps", nt*nf*sizeof(float));
			if (amps == NULL) {
				fprintf (stderr, "form_ftgrids: malloc() error.\n");
				return (0);	
			}
			for (k=0; k<nt*nf; k++) amps[k] = 0.0;
			SCV_free_trace (beam_tr);
			if (verbose == 1) {
				printf (".");
				fflush (stdout);
			}

			/* Loop through the channels */

			nchans = maxtbl(grid->chans);
			for (k=0; k<nchans; k++) {
				gchan = (Channelspec *) gettbl (grid->chans, k);
				if (!spgrm (gchan->tr, tsfr, tefr, grid->fmin, grid->fmax, gwin, &nt, &t, &nf, &f,
		                                &amp, &phase)) {
					fprintf (stderr, "form_ftgrids: spgrm() error.\n");
					return (0);
				}
				if (verbose == 1) {
					printf (".");
					fflush (stdout);
				}
				if (nf != grid->ns) {
					fprintf (stderr, "form_ftgrids: Frequencies dont match.\n");
					return (0);
				}
				for (i=0; i<nf; i++) {
					if (grid->s[i] != f[i]) {
						fprintf (stderr, "form_ftgrids: Frequencies dont match.\n");
						return (0);
					}
				}
				my_free (f);
				if (nt != nt0) {
					fprintf (stderr, "form_ftgrids: Times dont match.\n");
					return (0);
				}
				for (i=0; i<nt; i++) {
					tdel = t[i] + tsfr - grid->t0 - grid->t[i+it0];
					if (ABS(tdel) > 0.01) {
						fprintf (stderr, "form_ftgrids: Times dont match.\n");
						return (0);
					}
				}
				my_free (t);
				my_free (phase);
				for (i=0; i<nt*nf; i++) amps[i] += amp[i];
				my_free (amp);
			}
			wt = 1.0/nchans;
			for (k=0; k<nt*nf; k++) {
				if (amps[k] == 0.0 && grid->norm_power_grid[ia0+k] != 0.0) {
					grid->norm_power_grid[ia0+k] = 1.e30;
				} else if (amps[k] != 0.0) {
					grid->norm_power_grid[ia0+k] /= (wt*amps[k]);
				}
			}
			my_free (amps);
		}

		time = tefr + grid->dt;
		if (tefr == tend) break;
	}
	if (verbose == 1) {
		printf ("done\n");
	}

	/* Normal exit */

	return (1);
}

int
form_crosscor (db, scsin, gather, tstart, tend, recipes, orid, phase)

Dbptr          db;
Tbl *              scsin;
Apspec *                  gather;
double                            tstart;
double                                    tend;
Tbl *                                           recipes;
int                                                      orid;
char *                                                         phase;

{
	int nccs;
	int i, j, k, kref, n, nn;
	double tpad;
	Tbl *ccs;
	char *name;
	int itype;
	void *ptr;
	Tbl *scs;
	Apspec *cc;
	double t1, t2;
	Channelspec *cchan, *chan_ref;
	int nt, nmax, nfft, first, efirst;
	double time, tshift, omega;
	float tst4, dt4, shr, shi, fr, fi;
	float *ffti;
	Dbptr dbe, dbs;
	char string[64];
	double elat, elon, depth, slat, slon, elev, ref_tshift;
	int nph;
	double *times, *ps, *dtdhs;
	char **phs;
	float pmax;
	double delta, azimuth;
	double sumab, sumbb;

	/* Set up time limits */

	nccs = maxtbl(recipes);
	if (nccs < 1) {
		fprintf (stderr, "form_crosscor: No crosscors to compute.\n");
		return (0);
	}
	ccs = newtbl(1);
	if (ccs == NULL) {
		fprintf (stderr, "form_crosscor: newtbl() error.\n");
		return (0);
	}
	if (VERBOSE) printf ("In form_crosscor...\n");
	for (i=0,tpad=0.0,nn=0; i<nccs; i++) {
		if (!get_process_recipe (recipes, i, &name, &itype, &ptr)) {
			fprintf (stderr, "form_crosscor: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case CROSSCOR:
			cc = ptr;
			nn++;
			break;
		default:
			cc = NULL;
			break;
		}
		if (cc == NULL) continue;
		cc->ts = tstart;
		cc->te = tend;
		cc->gather = gather;
		if (cc->tpad > tpad) tpad = cc->tpad;
		if (settbl(ccs, -1, cc) < 0) {
			fprintf (stderr, "form_crosscor: settbl() error.\n");
			return (0);
		}
	}
	if (nn == 0) {
		return (1);
	}
	nccs = nn;

	/* Make the station-channel table */

	scs = scsin;
	n = maxtbl(scs);
	if (n < 1) {
		fprintf (stderr, "form_crosscor: No data to process.\n");
		return (0);
	}

	/* Set up grid channel geometry */

	if (!setup_geom (db, ccs)) {
		fprintf (stderr, "form_crosscor: setup_geom() error.\n");
		return (0);
	}

	/* Get input traces */

	t1 = tstart - tpad;
	t2 = tend + tpad;
	if (!fill_grids (ccs, scs, t1, t2, 0.0)) {
		fprintf (stderr, "form_crosscor: fill_grids() error.\n");
		return (0);
	}

	/* Form channel spectra */

	cc = (Apspec *) gettbl (ccs, 0);
	n = maxtbl(cc->chans);
	efirst = 1;
	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (!strcmp(cchan->sta, cc->refsta)) break;
	}
	if (i == n) {
		fprintf (stderr, "form_crosscor: No reference station.\n");
		return (1);
	}
	if (cchan->tr == NULL) {
		fprintf (stderr, "form_crosscor: No data for reference station.\n");
		return (1);
	}
	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan->tr == NULL) continue;
		cchan->sp = (Spec *) malloc (sizeof(Spec));
		if (cchan->sp == NULL) {
			fprintf (stderr, "form_crosscor: malloc() error.\n");
			return (0);
		}
		cchan->sp->acor = 1.0;
		cchan->sp->orid = orid;
		strcpy (cchan->sp->phase, "-");
		cchan->sp->azimuth = cc->azimuth;
		cchan->sp->slow = cc->slowi*111.12;
		nn = (cc->te - cc->ts)/cchan->tr->dt + 1.5;
		nmax = 10*nn;
		cchan->sp->fftr = (float *) malloc (nmax*sizeof(float));
		if (cchan->sp->fftr == NULL) {
			fprintf (stderr, "form_crosscor: malloc() error.\n");
			return (0);
		}
		cchan->sp->ffti = (float *) malloc (nmax*sizeof(float));
		if (cchan->sp->ffti == NULL) {
			fprintf (stderr, "form_crosscor: malloc() error.\n");
			return (0);
		}
		for (j=0; j<nmax; j++) {
			cchan->sp->fftr[j] = 0.0;
			cchan->sp->ffti[j] = 0.0;
		}
		if (orid >= 0) {
			if (efirst) {
				sprintf (string, "%d", orid);
				dbe = dblookup (dbi, 0, "origin", "orid", string);
				if (dbe.record == dbINVALID) {
					fprintf (stderr, "form_crosscor: Unable to find orid %d.\n", orid);
					return (0);
				}
				dbe.field = dbALL;
				dbgetv (dbe, 0, "lat", &elat, "lon", &elon, "depth", &depth, NULL);
				efirst = 0;
				tt_taup_set_phases ("basic");
				tt_taup_set_event_depth (depth);
				dbs = dblookup (dbi, 0, "site", "sta", cc->refsta);
				if (dbs.record == dbINVALID) {
					fprintf (stderr, "form_crosscor: Unable to find refsta %s.\n", cc->refsta);
					return (0);
				}
				dbs.field = dbALL;
				dbgetv (dbs, 0, "lat", &slat, "lon", &slon, "elev", &elev, NULL);
				tt_taup_p (elat, elon, slat, slon, elev, 0.0, 0.0,
	                                                &nph, &times, &ps, &dtdhs, &phs);
				for (j=0; j<nph; j++) if (!strcmp(phs[j], phase)) break;
				if (j == nph) {
					fprintf (stderr, "form_crosscor: Unable to find phase %s.\n", phase);
					return (0);
				}
				ref_tshift = times[j];
			}
			dbs = dblookup (dbi, 0, "site", "sta", cchan->sta);
			if (dbs.record == dbINVALID) {
				fprintf (stderr, "form_crosscor: Unable to find sta %s.\n", cchan->sta);
				return (0);
			}
			dbs.field = dbALL;
			dbgetv (dbs, 0, "lat", &slat, "lon", &slon, "elev", &elev, NULL);
			tt_taup_p (elat, elon, slat, slon, elev, 0.0, 0.0,
	                                                &nph, &times, &ps, &dtdhs, &phs);
			for (j=0; j<nph; j++) if (!strcmp(phs[j], phase)) break;
			if (j == nph) {
				fprintf (stderr, "form_crosscor: Unable to find phase %s.\n", phase);
				return (0);
			}
			cchan->tshift = -times[j]+ref_tshift;
			dist (slat*M_PI/180.0, slon*M_PI/180.0, elat*M_PI/180.0, elon*M_PI/180.0, &delta, &azimuth);
			delta *= 180.0/M_PI;
			azimuth *= 180.0/M_PI;
			cchan->sp->azimuth = azimuth;
			cchan->sp->slow = ps[j];
			strcpy (cchan->sp->phase, phase);
		}
		for (j=0,first=1; j<cchan->tr->nsamps; j++) {
			time = cchan->tr->tstart+cchan->tr->dt*j+cchan->tshift+cchan->tcor;
			if (time < cc->ts) continue;
			if (time > cc->te) break;
			nt = (time - cc->ts)/cchan->tr->dt + 0.5;
			if (first) {
				first=0;
				tshift = time - nt*cchan->tr->dt - cc->ts;
			}
			cchan->sp->fftr[nt++] = cchan->tr->data[j]*cchan->acor;
		}
		dt4 = cchan->tr->dt;
		nt = 2*nn;
		t2f_ (&nt,&nmax,&tst4,&dt4,cchan->sp->fftr,cchan->sp->ffti,&nfft,&cchan->sp->df);
		cchan->sp->nf = nt/2;
		cchan->sp->dt = 0.5/(cchan->sp->nf*cchan->sp->df);
		cchan->sp->ts = 0.5*(cc->te - cc->ts);
		for (j=0; j<cchan->sp->nf+1; j++) {
			omega = j*cchan->sp->df*2.0*M_PI;
			omega *= tshift;
			shr = cos(omega);
			shi = -sin(omega);
			fr = cchan->sp->fftr[j]*shr - cchan->sp->ffti[j]* shi;
			fi = cchan->sp->fftr[j]*shi + cchan->sp->ffti[j]* shr;
			cchan->sp->fftr[j] = fr;
			cchan->sp->ffti[j] = fi;
		}
		if (!strcmp(cchan->sta, cc->refsta)) chan_ref = cchan;
	}

	/* Compute the correlation spectra */

	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan == chan_ref) continue;
		if (cchan->tr == NULL) continue;
		if (cchan->sp == NULL) continue;
		if (cchan->sp->nf != chan_ref->sp->nf) {
			fprintf (stderr, "form_crosscor: Channel %s:%s nf doesn't match %s:%s\n",
						cchan->sta, cchan->chan, chan_ref->sta, chan_ref->chan);
			return (0);
		}
		if (cchan->sp->df != chan_ref->sp->df) {
			fprintf (stderr, "form_crosscor: Channel %s:%s df doesn't match %s:%s\n",
						cchan->sta, cchan->chan, chan_ref->sta, chan_ref->chan);
			return (0);
		}
		for (j=0; j<cchan->sp->nf+1; j++) {
			fr = cchan->sp->fftr[j]*chan_ref->sp->fftr[j] + cchan->sp->ffti[j]*chan_ref->sp->ffti[j];
			fi = -cchan->sp->fftr[j]*chan_ref->sp->ffti[j] + cchan->sp->ffti[j]*chan_ref->sp->fftr[j];
			cchan->sp->fftr[j] = fr;
			cchan->sp->ffti[j] = fi;
		}
	}
	for (j=0; j<chan_ref->sp->nf+1; j++) {
		fr = chan_ref->sp->fftr[j]*chan_ref->sp->fftr[j] + chan_ref->sp->ffti[j]*chan_ref->sp->ffti[j];
		chan_ref->sp->fftr[j] = fr;
		chan_ref->sp->ffti[j] = 0.0;
	}

	/* Compute time domain equivalents */

	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan->sp == NULL) continue;
		cchan->sp->fftt = (float *) malloc (nmax*sizeof(float));
		if (cchan->sp->fftt == NULL) {
			fprintf (stderr, "form_crosscor: malloc() error.\n");
			return (0);
		}
		ffti = (float *) malloc (nmax*sizeof(float));
		if (ffti == NULL) {
			fprintf (stderr, "form_crosscor: malloc() error.\n");
			return (0);
		}
		for (j=0; j<cchan->sp->nf+1; j++) {
			omega = j*cchan->sp->df*2.0*M_PI;
			omega *= cchan->sp->ts;
			shr = cos(omega);
			shi = -sin(omega);
			fr = cchan->sp->fftr[j]*shr - cchan->sp->ffti[j]* shi;
			fi = cchan->sp->fftr[j]*shi + cchan->sp->ffti[j]* shr;
			cchan->sp->fftt[j] = fr;
			ffti[j] = fi;
		}
		tst4 = 0.0;
		f2t_ (&nfft,cchan->sp->fftt,ffti,&tst4,&cchan->sp->dt);
		free (ffti);
	}

	/* Look for greatest positive peaks */

	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan->sp == NULL) continue;
		cchan->sp->tshift = 1.e30;
		pmax = 0.0;
		for (j=1; j<cchan->sp->nf*2-1; j++) {
			if (cchan->sp->fftt[j] < 0.0) continue;
			if (cchan->sp->fftt[j] < cchan->sp->fftt[j-1]) continue;
			if (cchan->sp->fftt[j] < cchan->sp->fftt[j+1]) continue;
			time = cchan->sp->ts - cchan->sp->dt*j;
/*			if (ABS(time) < ABS(cchan->sp->tshift)) cchan->sp->tshift = time;*/
			if (cchan->sp->fftt[j] > pmax) {
				pmax = cchan->sp->fftt[j];
				cchan->sp->tshift = time;
			}
		}
	}

	/* Compute the amplitude correction factor by fitting the time
	   corrected waveforms. */

	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan->sp == NULL) continue;
		sumab = 0.0;
		sumbb = 0.0;
		for (j=0; j<chan_ref->tr->nsamps; j++) {
			time = chan_ref->tr->tstart+chan_ref->tr->dt*j-chan_ref->tshift-chan_ref->tcor-chan_ref->sp->tshift;
			if (time < cc->ts) continue;
			if (time > cc->te) break;
			kref = INT((time-chan_ref->tr->tstart)/chan_ref->tr->dt);
			time = chan_ref->tr->tstart+chan_ref->tr->dt*j-cchan->tshift-cchan->tcor-cchan->sp->tshift;
			k = INT((time-cchan->tr->tstart)/cchan->tr->dt);
			if (k < 0) continue;
			if (k >= cchan->tr->nsamps) continue;
			sumab += chan_ref->tr->data[kref]*cchan->tr->data[k]*chan_ref->acor*cchan->acor;
			sumbb += cchan->tr->data[k]*cchan->tr->data[k]*cchan->acor*cchan->acor;
		}
		if (sumbb > 0.0) {
			cchan->sp->acor = sumab/sumbb;
		}
	}

	/* Shift the spectra according to the time shifts and scale according to the
	   amplitude corrections */

	for (i=0; i<n; i++) {
		cchan = (Channelspec *) gettbl (cc->chans, i);
		if (cchan->sp == NULL) continue;
		for (j=1; j<cchan->sp->nf+1; j++) {
			omega = j*cchan->sp->df*2.0*M_PI;
			omega *= cchan->sp->tshift;
			shr = cos(omega);
			shi = -sin(omega);
			fr = cchan->sp->fftr[j]*shr - cchan->sp->ffti[j]* shi;
			fi = cchan->sp->fftr[j]*shi + cchan->sp->ffti[j]* shr;
			cchan->sp->fftr[j] = fr*cchan->sp->acor*chan_ref->sp->acor;
			cchan->sp->ffti[j] = fi*cchan->sp->acor*chan_ref->sp->acor;
		}
	}

	/* Normal exit */

	return (1);
}

int
form_grids (db, scsin, gather, tstart, tend, gwin, twin, dgrid, tbreak, plot, recipes, indx, thresh, az, slow, dbo, igrwr, plotfile, snorm)

Dbptr       db;
Tbl *           scsin;
Apspec *               gather;
double                         tstart;
double                                 tend;
double                                       gwin;
double                                             twin;
double                                                   dgrid;
double						               tbreak;
int                                                                    plot;
Tbl *                                                                         recipes;
int                                                                                    indx;
double                                                                                       thresh;
double *                                                                                             az;
double *                                                                                                 slow;
Dbptr                                                                                                          dbo;
int                                                                                                                 igrwr;
char *                                                                                                                     plotfile;
int snorm;

{
	int ngrids;
	int i, j, k, l;
	double tpad;
	Apspec *grid;
	Channelspec *gchan;
	Gridnode *node;
	double t1, t2, time, tlast;
	int n, nn;
	Trace *trace, *tracef, *tr;
	char label[64];
	int igr, igr0, ngr, ngrid;
	Tbl *scs;
	int rotate;
	double hang, vang;
	float factor;
	char *name;
	int itype;
	void *ptr;
	Tbl *grids;
	Grid *grd;
	int nnodes;
	double t0gr, t1gr, ggwin;
	float *z;

	int interactive=0;

	/* Set up time limits */

	ngrids = maxtbl(recipes);
	if (ngrids < 1) {
		fprintf (stderr, "form_grids: No grids to compute.\n");
		return (0);
	}
	grids = newtbl(1);
	if (grids == NULL) {
		fprintf (stderr, "form_grids: newtbl() error.\n");
		return (0);
	}
	if (VERBOSE) printf ("In form_grids...\n");
	for (i=0,tpad=0.0,nn=0; i<ngrids; i++) {
		if (!get_process_recipe (recipes, i, &name, &itype, &ptr)) {
			fprintf (stderr, "form_grids: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case XYGRID:
			grid = ptr;
			grid->nt = 0;
			grid->dt = 0.0;
			nn++;
			break;
		case STGRID:
			grid = ptr;
			nn++;
			break;
		default:
			grid = NULL;
			break;
		}
		if (grid == NULL) continue;
		grid->ts = tstart;
		grid->te = tend;
		grid->gather = gather;
		if (grid->tpad > tpad) tpad = grid->tpad;
		if (settbl(grids, -1, grid) < 0) {
			fprintf (stderr, "settbl() error.\n");
			return (0);
		}
	}
	if (nn == 0) {
		return (1);
	}
	ngrids = nn;
	t1 = tstart - tpad;
	t2 = tend + tpad;

	/* Make the station-channel table */

	scs = scsin;
	n = maxtbl(scs);
	if (n < 1) {
		fprintf (stderr, "form_grids: No data to process.\n");
		return (0);
	}

	/* Set up grid channel geometry */

	if (!setup_geom (db, grids)) {
		fprintf (stderr, "form_grids: setup_geom() error.\n");
		return (0);
	}

	/* Input data channel loop */

	if (twin > 0.0 && t2-t1 > twin) {
		t2 = t1 + twin + tpad;
	}

	if (gwin > 0.0 && dgrid > 0.0) {
		ngrid = (tend - tstart - gwin + dgrid)/dgrid;
		ngr = tbreak / dgrid + 0.1;
		if (ngr > ngrid) ngr = ngrid;
		ggwin = gwin;
	} else {
		ngrid = 1;
		ggwin = tend - tstart;
		dgrid = 0.0;
		ngr = ngrid;
	}

	if (!fill_grids (grids, scs, t1, t2+ggwin, ggwin)) {
		fprintf (stderr, "form_grids: fill_grids() error.\n");
		return (0);
	}

	/* Grid processing loop */

	tlast = tstart;
	/*if (grid->type == XYGRID && dgrid > 0.0) {
		printf ("\ngrid   fmin    fmax  azimuth velocity  sss2n   bms2n   ratio  gridratio\n");
    	}*/
    	if (verbose == 1) {
    		printf ("Processing grids");
    	}
	for (igr=0,igr0=0; igr<ngrid; igr++) {
		time = tstart+igr*dgrid;
		if (VERBOSE) printf ("Grid #%d...\n", igr);
		if (time+gwin+tpad > t2) {
			t1 = time - gwin - tpad;
			t2 = time + twin + gwin + tpad;
			if (!fill_grids (grids, scs, t1, t2+ggwin, ggwin)) {
				fprintf (stderr, "form_grids: fill_grids() error.\n");
				return (0);
			}
		}
		if (verbose == 1) {
			printf (".");
			fflush (stdout);
		}
		for (i=0; i<ngrids; i++) {
			grid = (Apspec *) gettbl (grids, i);
			grid->twin = ggwin;
			gchan = (Channelspec *) gettbl (grid->chans, 0);
			if (dgrid == 0.0) {
				t0gr = time;
				t1gr = time+ggwin;
			} else {
				t0gr = time-ggwin;
				t1gr = time;
			}
			if (!process_grid (grid, t0gr, t1gr, indx, thresh, snorm)) {
				fprintf (stderr, "form_grids: process_grid() error.\n");
				return (0);
			}
			if (grid->type == XYGRID) {
				if (dgrid == 0.0) {
					double fmin, fmax, param3, param4;
					int iparam1, iparam2, iparam3, iparam4;
	
					grid->nt = 1;
					/*if (!grid_plot (i, ngrids, grid, plotfile, t1, t2-t1, tstart, tend-tstart, plot)) {
						fprintf (stderr, "form_grids: grid_plot() error.\n");
						return (0);
					}*/
					/*filter_get_params (gchan->filter, &fmin, &fmax, &param3, &param4,
							&iparam1, &iparam2, &iparam3, &iparam4);
					if (!write_grids (grid, gchan, dbo, time, gwin, fmin, fmax, igrwr)) {
						fprintf (stderr, "form_grids: write_grids() error.\n");
					}*/
					/*printf ("%4d %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %7.2f %.3f\n",
						i+1, fmin, fmax, grid->az, 1.0/grid->slow, grid->sss2n,
						grid->bms2n, grid->bms2n/grid->sss2n, grid->gridratio);*/
				} else {
					double fmin, fmax, param3, param4;
					int iparam1, iparam2, iparam3, iparam4;

					grid->dt = dgrid;
					(grid->nt)++;
					/*filter_get_params (gchan->filter, &fmin, &fmax, &param3, &param4,
							&iparam1, &iparam2, &iparam3, &iparam4);
					if (!write_grids (grid, gchan, dbo, time, gwin, fmin, fmax, igrwr)) {
						fprintf (stderr, "form_grids: write_grids() error.\n");
					}*/
					/*printf ("%4d %s %4d %7.2f %7.2f %7.3f\n", igr+1, 
							epoch2str (time, "%Y%j %H:%M:%S"), i+1,
							grid->az, 1.0/grid->slow, grid->gridratio);*/
					if (grid->grid_power == NULL) {
						grid->grid_power = (Trace *) make_trace (NULL, ngr);
						if (grid->grid_power == NULL) {
							fprintf (stderr, "form_grids: make_trace() error.\n");
							return (0);
						}
						grid->grid_power->tstart = time;
						grid->grid_power->dt = dgrid;
						grid->grid_power->nsamps = ngr;
					}
					grid->grid_power->data[igr-igr0] = grid->gridratio;
					if (grid->grid_raw_power == NULL) {
						grid->grid_raw_power = (Trace *) make_trace (NULL, ngr);
						if (grid->grid_raw_power == NULL) {
							fprintf (stderr, "form_grids: make_trace() error.\n");
							return (0);
						}
						grid->grid_raw_power->tstart = time;
						grid->grid_raw_power->dt = dgrid;
						grid->grid_raw_power->nsamps = ngr;
					}
					grid->grid_raw_power->data[igr-igr0] = grid->grid;
					if (grid->grid_raw_az == NULL) {
						grid->grid_raw_az = (Trace *) make_trace (NULL, ngr);
						if (grid->grid_raw_az == NULL) {
							fprintf (stderr, "form_grids: make_trace() error.\n");
							return (0);
						}
						grid->grid_raw_az->tstart = time;
						grid->grid_raw_az->dt = dgrid;
						grid->grid_raw_az->nsamps = ngr;
					}
					grid->grid_raw_az->data[igr-igr0] = grid->az_grid;
					if (grid->grid_raw_sl == NULL) {
						grid->grid_raw_sl = (Trace *) make_trace (NULL, ngr);
						if (grid->grid_raw_sl == NULL) {
							fprintf (stderr, "form_grids: make_trace() error.\n");
							return (0);
						}
						grid->grid_raw_sl->tstart = time;
						grid->grid_raw_sl->dt = dgrid;
						grid->grid_raw_sl->nsamps = ngr;
					}
					grid->grid_raw_sl->data[igr-igr0] = grid->slow_grid;
				}
				nnodes = maxtbl (grid->grid_nodes);
				if (grid->summary_grid.power == NULL) {
					grid->summary_grid.power = (float *) my_malloc ("form_grids: grid->summary_grid.power",
												nnodes*sizeof(float));
					if (grid->summary_grid.power == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					for (j=0; j<nnodes; j++) grid->summary_grid.power[j] = 0.0;
					grid->summary_grid.tstart = t0gr;
					grid->summary_grid.tend = t1gr;
				}
				z = (float *) my_malloc ("form_grids: z", nnodes*sizeof(float));
				if (z == NULL) {
					fprintf (stderr, "form_grids: malloc() error.\n");
					return (0);
				}
				for (j=0; j<nnodes; j++) {
					node = (Gridnode *) gettbl (grid->grid_nodes, j);
					z[j] = node->grid;
				}
				switch (grid->summary) {
				default:
					for (j=0; j<nnodes; j++) grid->summary_grid.power[j] = z[j];
					grid->summary_grid.peak_power = grid->grid;
					grid->summary_grid.peak_azimuth = grid->az_grid;
					grid->summary_grid.peak_slow = grid->slow_grid;
					grid->summary_grid.slow_width = grid->width_grid;
					grid->summary_grid.tstart = t0gr;
					grid->summary_grid.tend = t1gr;
					break;
				case STACK:
					for (j=0; j<nnodes; j++) grid->summary_grid.power[j] += z[j];
					find6 (grid, grid->summary_grid.power, indx, thresh,
						&grid->summary_grid.peak_azimuth, 
						&grid->summary_grid.peak_slow, 
						&grid->summary_grid.slow_width, 
						&grid->summary_grid.peak_power);
					grid->summary_grid.tend = t1gr;
					break;
				case BEST:
					if (grid->grid <= grid->summary_grid.peak_power) break;
					for (j=0; j<nnodes; j++) grid->summary_grid.power[j] = z[j];
					grid->summary_grid.peak_power = grid->grid;
					grid->summary_grid.peak_azimuth = grid->az_grid;
					grid->summary_grid.peak_slow = grid->slow_grid;
					grid->summary_grid.slow_width = grid->width_grid;
					grid->summary_grid.tstart = t0gr;
					grid->summary_grid.tend = t1gr;
					break;
				}
				my_free (z);
				if (grid->norm_summary_grid.power == NULL) {
					grid->norm_summary_grid.power = (float *) my_malloc ("form_grids: grid->norm_summary_grid.power",
												nnodes*sizeof(float));
					if (grid->norm_summary_grid.power == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					for (j=0; j<nnodes; j++) grid->norm_summary_grid.power[j] = 0.0;
					grid->norm_summary_grid.tstart = t0gr;
					grid->norm_summary_grid.tend = t1gr;
				}
				z = (float *) my_malloc ("form_grids: z", nnodes*sizeof(float));
				if (z == NULL) {
					fprintf (stderr, "form_grids: malloc() error.\n");
					return (0);
				}
				for (j=0; j<nnodes; j++) {
					node = (Gridnode *) gettbl (grid->grid_nodes, j);
					z[j] = node->gridratio;
				}
				switch (grid->summary) {
				default:
					for (j=0; j<nnodes; j++) grid->norm_summary_grid.power[j] = z[j];
					grid->norm_summary_grid.peak_power = grid->gridratio;
					grid->norm_summary_grid.peak_azimuth = grid->az;
					grid->norm_summary_grid.peak_slow = grid->slow;
					grid->norm_summary_grid.slow_width = grid->width;
					grid->norm_summary_grid.tstart = t0gr;
					grid->norm_summary_grid.tend = t1gr;
					break;
				case STACK:
					for (j=0; j<nnodes; j++) grid->norm_summary_grid.power[j] += z[j];
					find6 (grid, grid->norm_summary_grid.power, indx, thresh,
						&grid->norm_summary_grid.peak_azimuth, 
						&grid->norm_summary_grid.peak_slow, 
						&grid->norm_summary_grid.slow_width, 
						&grid->norm_summary_grid.peak_power);
					grid->norm_summary_grid.tend = t1gr;
					break;
				case BEST:
					if (grid->gridratio <= grid->norm_summary_grid.peak_power) break;
					for (j=0; j<nnodes; j++) grid->norm_summary_grid.power[j] = z[j];
					grid->norm_summary_grid.peak_power = grid->gridratio;
					grid->norm_summary_grid.peak_azimuth = grid->az;
					grid->norm_summary_grid.peak_slow = grid->slow;
					grid->norm_summary_grid.slow_width = grid->width;
					grid->norm_summary_grid.tstart = t0gr;
					grid->norm_summary_grid.tend = t1gr;
					break;
				}
				if (grid->savegrids) {
					grd = (Grid *) my_malloc ("form_grids: grd", sizeof(Grid));
					if (grd == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					grd->power = NULL;
					grd->tstart = t0gr;
					grd->tend = t1gr;
					grd->peak_power = 0.0;
					grd->peak_azimuth = 0.0;
					grd->peak_slow = 0.0;
					grd->slow_width = 0.0;
					if (settbl(grid->grids, -1, grd) < 0) {
						fprintf (stderr, "form_grids: settbl() error.\n");
						return (0);
					}
					grd->power = (float *) my_malloc ("form_grids: grd->power", 
										nnodes*sizeof(float));
					if (grd->power == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					for (j=0; j<nnodes; j++) {
						grd->power[j] = z[j];
					}
					grd->peak_power = grid->gridratio;
					grd->peak_azimuth = grid->az;
					grd->peak_slow = grid->slow;
					grd->slow_width = grid->width;
				}
				if (plot) {
					xygrid_plot (grid, 0, 0.8, "none", 0, 0, 2.0, 1.0, 1.0, 0.0, 0, 0);
				}
				my_free (z);
			} else {
				if (grid->nt < 1) {
					grid->t = (float *) my_malloc ("form_grids: grid->t", sizeof(float));
					if (grid->t == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					grid->nt = 1;
					grid->t0 = time;
					grid->t[0] = 0.0;
					grid->norm_power_grid = (float *) my_malloc ("form_grids: grid->norm_power_grid",
										grid->ns*sizeof(float));
					if (grid->norm_power_grid == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					grid->power_grid = (float *) my_malloc ("form_grids: grid->power_grid",
										grid->ns*sizeof(float));
					if (grid->power_grid == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					for (j=0; j<grid->ns; j++) {
						node = (Gridnode *) gettbl (grid->grid_nodes, j);
						grid->norm_power_grid[j] = node->gridratio;
						grid->power_grid[j] = node->grid;
					}
				} else {
					if (grid->nt == 1) grid->dt = time - grid->t0;
					grid->t = (float *) my_realloc ("form_grids: grid->t",
								grid->t, (grid->nt+1)*sizeof(float));
					grid->norm_power_grid = (float *) my_realloc ("form_grids: grid->norm_power_grid",
								grid->norm_power_grid, (grid->nt+1)*(grid->ns)*sizeof(float));
					grid->power_grid = (float *) my_realloc ("form_grids: grid->power_grid",
								grid->power_grid, (grid->nt+1)*(grid->ns)*sizeof(float));
					if (grid->t == NULL || grid->norm_power_grid == NULL || grid->power_grid == NULL) {
						fprintf (stderr, "form_grids: malloc() error.\n");
						return (0);
					}
					grid->t[grid->nt] = time - grid->t0;
					for (j=0; j<grid->ns; j++) {
						node = (Gridnode *) gettbl (grid->grid_nodes, j);
						grid->norm_power_grid[j+(grid->ns)*(grid->nt)] = node->gridratio;
						grid->power_grid[j+(grid->ns)*(grid->nt)] = node->grid;
					}
					(grid->nt)++;
				}
			}
		}
		if (grid->type == XYGRID) {
			/*if (plot == 1) time_plot (grids, tstart+igr*dgrid);*/
			if (interactive) {
				printf ("Enter grid #: ");
				scanf ("%d", &i);
				if (i > 0 && i <= ngrids) {
					grid = (Apspec *) gettbl (grids, i-1);
					*az = grid->az;
					*slow = grid->slow;
				} else {
					*az = -1.0;
					*slow = -1.0;
				}
			} else {
				grid = (Apspec *) gettbl (grids, 0);
				*az = grid->az;
				*slow = grid->slow;
			}
			if (tbreak > 0.0 && igr-igr0 >= ngr-1) {
				for (i=0; i<ngrids; i++) {
					grid = (Apspec *) gettbl (grids, i);
					if (grid->grid_power) 
						grid->grid_power = (Trace *) convert_trace (grid->grid_power, "t4");
					if (grid->grid_raw_power) 
						grid->grid_raw_power = (Trace *) convert_trace (grid->grid_raw_power, "t4");
					if (grid->grid_raw_az) 
						grid->grid_raw_az = (Trace *) convert_trace (grid->grid_raw_az, "t4");
					if (grid->grid_raw_sl) 
						grid->grid_raw_sl = (Trace *) convert_trace (grid->grid_raw_sl, "t4");
				}
				/*if (!write_pows (grids, tlast, time, dbo)) {
					fprintf (stderr, "form_grids: write_pows() error.\n");
					return (0);
				}
				for (i=0; i<ngrids; i++) {
					grid = (Apspec *) gettbl (grids, i);
					if (grid->grid_power) {
						SCV_free_trace (grid->grid_power);
						grid->grid_power = NULL;
					}
					if (grid->grid_raw_power) {
						SCV_free_trace (grid->grid_raw_power);
						grid->grid_raw_power = NULL;
					}
					if (grid->grid_raw_az) {
						SCV_free_trace (grid->grid_raw_az);
						grid->grid_raw_az = NULL;
					}
					if (grid->grid_raw_sl) {
						SCV_free_trace (grid->grid_raw_sl);
						grid->grid_raw_sl = NULL;
					}
				}*/
				tlast = time + dgrid;
				igr0 = igr+1;
				ngr = tbreak / dgrid + 0.1;
				if (ngr > ngrid-igr) ngr = ngrid-igr;
			}
		} else {
		}
	}
	if (verbose == 1) printf ("done\n");
	if (grid->type == XYGRID) {
		if (tbreak > 0.0 && igr-igr0) {
			for (i=0; i<ngrids; i++) {
				grid = (Apspec *) gettbl (grids, i);
				if (grid->grid_power) 
					grid->grid_power = (Trace *) convert_trace (grid->grid_power, "t4");
				if (grid->grid_raw_power) 
					grid->grid_raw_power = (Trace *) convert_trace (grid->grid_raw_power, "t4");
				if (grid->grid_raw_az) 
					grid->grid_raw_az = (Trace *) convert_trace (grid->grid_raw_az, "t4");
				if (grid->grid_raw_sl) 
					grid->grid_raw_sl = (Trace *) convert_trace (grid->grid_raw_sl, "t4");
			}
			/*if (!write_pows (grids, tlast, time, dbo)) {
				fprintf (stderr, "form_grids: write_pows() error.\n");
				return (0);
			}*/
		}
	} else {
		/*plot_sgrid (grids, plotfile);*/
	}

	/* Normal exit */

	return (1);
}

int
fill_grids (grids, scs, t1, t2, gwin)

Tbl *       grids;
Tbl *              scs;
double                  t1, t2;
double                          gwin;

{
	int i, j, k, l, n;
	Trace *trace, *tracef, *tr, *tro;
	int ngrids;
	double hang, vang, tstart;
	Apspec *grid, *gather;
	Channelspec *gchan, *gather_chan;
	int rotate;
	int nchans;
	float factor;
	int ioff;
	int first = 1;

	ngrids = maxtbl(grids);
	grid = (Apspec *) gettbl (grids, 0);
	gather = grid->gather;
	if (gather) {
		n = maxtbl(gather->chans);
	} else {
		n = maxtbl(scs);
	}

	/* first null out grid channel traces */

	if (VERBOSE) {
		printf ("Reading ");
		fflush (stdout);
	}
	for (j=0; j<ngrids; j++) {
		grid = (Apspec *) gettbl (grids, j);
		nchans = maxtbl(grid->chans);
		for (k=0; k<nchans; k++) {
			gchan = (Channelspec *) gettbl (grid->chans, k);
			SCV_free_trace (gchan->tr);
			SCV_free_trace (gchan->power_tr);
			gchan->tr = NULL;
			gchan->power_tr = NULL;
			if (gchan->sp) {
				if (gchan->sp->fftr) free (gchan->sp->fftr);
				if (gchan->sp->ffti) free (gchan->sp->ffti);
				if (gchan->sp->fftt) free (gchan->sp->fftt);
				free (gchan->sp);
				gchan->sp = NULL;
			}
		}
	}

	/* now fill the grid channels */

	for (i=0,trace=NULL; i<n; i++) {
		char sta[32], chan[32];

		/* Do we need this channel? */

		if (gather) {
			gather_chan = (Channelspec *) gettbl (gather->chans, i);
			strcpy (sta, gather_chan->sta);
			strcpy (chan, gather_chan->chan);
		} else {
			get_sc_stachan (gettbl(scs, i), sta, chan);
		}
		if (!need_channelg (grids, sta, chan)) continue;

		/* Read in trace */

		if (VERBOSE) {
			printf ("R");
			fflush (stdout);
		}
		if (gather) {
			if (gather_chan->tr == NULL) {
				trace = NULL;
			} else {
				trace = (Trace *) copy_trace (gather_chan->tr, 1);
			}
		} else {
			trace = read_sc (gettbl(scs, i), t1, t2, &hang, &vang);
		}
		if (trace == NULL) {
			fprintf (stderr, "fill_grids: Unable to read input trace for %s:%s.\n",
									sta, chan);
			continue;
		}
		if (first) {
			first = 0;
			tstart = trace->tstart;
		}

		/* Grid loop */

		for (j=0; j<ngrids; j++) {
			grid = (Apspec *) gettbl (grids, j);
			nchans = maxtbl(grid->chans);
			for (k=0; k<nchans; k++) {
				gchan = (Channelspec *) gettbl (grid->chans, k);
				if (!need_channelgchan (gchan, sta, chan, &rotate)) continue;
				if (!strcmp(gchan->gap, "drop")) {
					if (trace->next) continue;
				}
				if (gchan->wt == 0.0) continue;
				if (strcmp(gchan->gap, "segment")) {
					trace = (Trace *) SCV_trace_toraw (trace, 0);
					trace = (Trace *) SCV_trace_fillgaps (trace);
					trace = (Trace *) SCV_trace_fixgaps (trace, gchan->gap);
					trace = (Trace *) SCV_trace_tofloat (trace, 0);
				}
				if (VERBOSE) {
					printf ("f");
					fflush (stdout);
				}
				if (!strcmp(gchan->filter, "none")) {
					tracef = (Trace *) copy_trace (trace, 1);
					if (tracef == NULL) {
						fprintf (stderr, "fill_grids: copy_trace() error for %s:%s.\n",
								sta, chan);
						return (0);
					}
				} else {
					tracef = filter_trace (trace, gchan->filter, 1);
					if (tracef == NULL) {
						fprintf (stderr, "fill_grids: Unable to filter input trace for %s:%s.\n",
									sta, chan);
						return (0);
					}
				}
				if (VERBOSE) {
					printf ("s");
					fflush (stdout);
				}
				if (grid->samprate > 0.0) {
					if (!trace_resamp (tracef, grid->samprate, tstart)) {
						fprintf (stderr, "fill_grids: trace_resamp() error for %s:%s.\n",
								sta, chan);
						return (0);
					}
				} 
				if (VERBOSE) {
					printf ("r");
					fflush (stdout);
				}
				if (rotate) {
					if (rotate == RADIAL || rotate == TRANSVERSE) {
						if (grid->azimuth < 0.0 || grid->azimuth > 360.0) {
							fprintf (stderr, "fill_grids: Illegal azimuth value for rotation.\n");
							return (0);
						}
					} 
					switch (rotate) {
					case RADIAL:
						factor = cos((hang-grid->azimuth)*M_PI/180.0);
						break;
					case TRANSVERSE:
						factor = sin((hang-grid->azimuth)*M_PI/180.0);
						break;
					case NORTH:
						factor = cos(hang*M_PI/180.0);
						break;
					case EAST:
						factor = sin(hang*M_PI/180.0);
						break;
					}
					for (tr=tracef; tr!= NULL; tr=tr->next) {
						for (l=0; l<tr->nsamps; l++) {
							if (tr->data[l] >= 1.e30) continue;
							tr->data[l] *= factor;
						}
					}
					if (gchan->tr == NULL) {
						gchan->tr = tracef;
					} else {
						gchan->tr = (Trace *) trace_add (gchan->tr, tracef);
						SCV_free_trace (tracef);
					}
				} else {
					gchan->tr = tracef;
				}
			}
		}
		SCV_free_trace (trace);
	}

	/* Go back and compute the channel power traces */

	if (gwin == 0.0) {
		if (VERBOSE) printf ("done\n");
		return (1);
	}
	for (j=0; j<ngrids; j++) {
		grid = (Apspec *) gettbl (grids, j);
		if (grid->type == FTGRID) continue;
		nchans = maxtbl(grid->chans);
		for (k=0; k<nchans; k++) {
			gchan = (Channelspec *) gettbl (grid->chans, k);
			if (gchan->tr == NULL) continue;
			gchan->power_tr = (Trace *) copy_trace (gchan->tr, 1);
			if (gchan->power_tr == NULL) {
				fprintf (stderr, "fill_grids: copy_trace() error for %s:%s.\n",
								gchan->sta, gchan->chan);
				return (0);
			}
			for (tr=gchan->tr,tro=gchan->power_tr; tr!=NULL; tr=tr->next,tro=tro->next) {
				double dioff, point, lastpoint;
				double *sqpts;

				sqpts = (double *) malloc (tr->nsamps*sizeof(double));
				if (sqpts == NULL) {
					fprintf (stderr, "fill_grids: malloc() error.\n");
					return (0);
				}
				for (i=0; i<tr->nsamps; i++) {
					sqpts[i] = (tr->data[i]*gchan->acor)*(tr->data[i]*gchan->acor);
				}
				ioff = gwin/tr->dt + 1.5;
				dioff = 0.0;
				for (i=0; i<tr->nsamps; i++) {
					point = 0.0;
					dioff = 0.0;
					for (j=i; j<i+ioff && j < tr->nsamps; j++) {
						point += sqpts[j];
						dioff += 1.0;
					}
					tro->data[i] = point/dioff;
					if (tro->data[i] < 0.0) {
						fprintf (stderr, "problem\n");
					}
				}
				free (sqpts);
			}
		}
	}

	/* Normal exit */

	if (VERBOSE) printf ("done\n");
	return (1);
}


int
process_grid (grid, tstart, tend, indx, thresh, snorm)

Apspec *    grid;
double              tstart;
double                      tend;
int                               indx;
double                                  thresh;

{
	int inode, nnodes;
	int ichan, nchans;
	int i, j, n, ns;
	Gridnode *node;
	Channelspec *gchan, *ref_gchan;
	Trace *trace=NULL, *tr, *trd, *trp;
	double tshift, tendb, tendd, time, gmax, gmax_grid;
	double sx, sy, sr;
	float wt, swt, swt_ref, swt_refsq;
	int ishift, ioff, joff, x;
	double sssta, ss;
	float dat, wti;
	int good;
	float *bdata, *ddata;
	double spow, sspow;
	double cwt;

	nnodes = maxtbl (grid->grid_nodes);
	nchans = maxtbl (grid->chans);

	/* Make up a temp trace to hold the beam samples */

	if (VERBOSE) {
		printf ("Processing grid...\n");
		printf ("computing single station power...");
		fflush (stdout);
	}
	for (ichan=0,grid->nsta=0; ichan<nchans; ichan++) {
		int i0;

		gchan = (Channelspec *) gettbl (grid->chans, ichan);
		trd = gchan->tr;
		for (tr=trd; tr!=NULL; tr=tr->next) {
			tendd = tr->tstart+tr->dt*(tr->nsamps-1);
			if (tend > tr->tstart && tstart < tendd) break;
		}
		trd = tr;
		if (trd) break;
	}
	if (trd == NULL) {
		grid->az = -1.0;
		grid->slow = -1.0;
		grid->gridratio = 0.0;
		grid->grid = 0.0;
		for (inode=0; inode<nnodes; inode++) {
			node = (Gridnode *) gettbl (grid->grid_nodes, inode);
			node->grid = 0.0;
			node->gridratio = 0.0;	
		}
		return (1);
	}
	n = (tend-tstart)/trd->dt + 1.5;
	trace = (Trace *) make_trace (NULL, n);
	if (trace == NULL) {
		fprintf (stderr, "process_grid: make_trace() error.\n");
		return (0);
	}
	trace->nsamps = n;
	trace->dt = trd->dt;
	n = (tstart-trd->tstart)/trd->dt + 0.5;
	trace->tstart = trd->tstart + n*trace->dt;

	/* Find reference channel */

	for (ichan=0; ichan<nchans; ichan++) {
		gchan = (Channelspec *) gettbl (grid->chans, ichan);
		if (!strcmp(gchan->sta, grid->refsta)) break;
	}
	if (ichan < nchans) ref_gchan = gchan; else ref_gchan = NULL;

	/* Process grid nodes */

	if (VERBOSE) printf ("done\n");
	if (wti > 0.0) sssta /= wti;
	gmax = 0.0;
	gmax_grid = 0.0;
	if (trace) {
		tendb = trace->tstart + (trace->nsamps-1)*trace->dt;
		bdata = trace->data;
	}
	if (VERBOSE) {
		printf ("nodes");
		fflush (stdout);
	}
	for (inode=0,good=0; inode<nnodes; inode++) {
		if (VERBOSE && !(inode%10)) {
			printf (".");
			fflush (stdout);
		}
		node = (Gridnode *) gettbl (grid->grid_nodes, inode);
		node->grid = 0.0;
		node->gridratio = 0.0;
		if (trace == NULL) continue;
		if (snorm && ref_gchan) {
			tshift = node->sx*ref_gchan->dx + node->sy*ref_gchan->dy + ref_gchan->tcor;
			trd = ref_gchan->tr;
			for (tr=trd,trp=ref_gchan->power_tr; tr!=NULL; tr=tr->next,trp=trp->next) {
				tendd = tr->tstart+tr->dt*(tr->nsamps-1);
				if (tend > tr->tstart && tstart < tendd) break;
			}
			ioff = INT((trace->tstart - tshift - trd->tstart)/trd->dt);
			n = (tendb - tshift - trd->tstart)/trd->dt + 0.5;
			if (ioff >= 0 && n < trd->nsamps) {
				spow = trp->data[ioff];
				if (spow <= 0.0) swt_ref = 1.0; else swt_ref = sqrt(spow);
			} else {
				swt_ref = 1.0;
			}
		} else {
			swt_ref = 1.0;
		}
		swt_refsq = swt_ref*swt_ref;
		for (i=0; i<trace->nsamps; i++) bdata[i] = 0.0;
		for (ichan=0,wti=0.0,sspow=0.0; ichan<nchans; ichan++) {
			gchan = (Channelspec *) gettbl (grid->chans, ichan);
			trd = gchan->tr;
			for (tr=trd,trp=gchan->power_tr; tr!=NULL; tr=tr->next,trp=trp->next) {
				tendd = tr->tstart+tr->dt*(tr->nsamps-1);
				if (tend > tr->tstart && tstart < tendd) break;
			}
			trd = tr;
			if (trd == NULL) continue;
			ddata = trd->data;
			tshift = node->sx*gchan->dx + node->sy*gchan->dy + gchan->tcor;
			ioff = INT((trace->tstart - tshift - trd->tstart)/trd->dt);
			n = (tendb - tshift - trd->tstart)/trd->dt + 0.5;
			cwt = gchan->wt*gchan->acor;
			if (ioff >= 0 && n < trd->nsamps) {
				spow = 0.0;
				if (cwt == 1.0) {
					wt = 1.0;
					spow = trp->data[ioff];
					if (snorm) {
						if (spow <= 0.0) {
							swt = 0.0;
						} else {
							swt = swt_ref/sqrt(spow);
						}
						spow = swt_refsq;
						for (i=0; i<trace->nsamps; i++,ioff++) {
							if (ddata[ioff] > 0.9e30) continue;
							bdata[i] += swt*ddata[ioff];
						}
					} else {
						for (i=0; i<trace->nsamps; i++,ioff++) {
							if (ddata[ioff] > 0.9e30) continue;
							bdata[i] += ddata[ioff];
						}
					}
				} else if (gchan->wt == 0.0) {
					wt = 0.0;
				} else {
					wt = gchan->wt;
					spow = trp->data[ioff];
					if (snorm) {
						if (spow <= 0.0) {
							swt = 0.0;
						} else {
							swt = swt_ref*cwt/sqrt(spow);
						}
						spow = swt_refsq;
						for (i=0; i<trace->nsamps; i++,ioff++) {
							if (ddata[ioff] > 0.9e30) continue;
							bdata[i] += swt*ddata[ioff];
						}
					} else {
						for (i=0; i<trace->nsamps; i++,ioff++) {
							if (ddata[ioff] > 0.9e30) continue;
							bdata[i] += cwt*ddata[ioff];
						}
					}
				}
			} else {
				spow = 0.0;
				if (cwt == 1.0) {
					wt = 1.0;
					joff = (ioff<0)?0:ioff;
					joff = (joff>=trd->nsamps)?trd->nsamps-1:joff;
					spow = trp->data[joff];
					if (snorm) {
						if (spow <= 0.0) {
							swt = 0.0;
						} else {
							swt = swt_ref/sqrt(spow);
						}
						spow = swt_refsq;
						for (i=0; i<trace->nsamps; i++,ioff++) {
							joff = (ioff<0)?0:ioff;
							joff = (joff>=trd->nsamps)?trd->nsamps-1:joff;
							if (ddata[joff] > 0.9e30) continue;
							bdata[i] += swt*ddata[joff];
						}
					} else {
						for (i=0; i<trace->nsamps; i++,ioff++) {
							joff = (ioff<0)?0:ioff;
							joff = (joff>=trd->nsamps)?trd->nsamps-1:joff;
							if (ddata[joff] > 0.9e30) continue;
							bdata[i] += ddata[joff];
						}
					}
				} else if (gchan->wt == 0.0) {
					wt = 0.0;
				} else {
					wt = gchan->wt;
					joff = (ioff<0)?0:ioff;
					joff = (joff>=trd->nsamps)?trd->nsamps-1:joff;
					spow = trp->data[joff];
					if (snorm) {
						if (spow <= 0.0) {
							swt = 0.0;
						} else {
							swt = swt_ref*cwt/sqrt(spow);
						}
						spow = swt_refsq;
						for (i=0; i<trace->nsamps; i++,ioff++) {
							joff = (ioff<0)?0:ioff;
							joff = (joff>=trd->nsamps)?trd->nsamps-1:joff;
							if (ddata[joff] > 0.9e30) continue;
							bdata[i] += swt*ddata[joff];
						}
					} else {
						for (i=0; i<trace->nsamps; i++,ioff++) {
							joff = (ioff<0)?0:ioff;
							joff = (joff>=trd->nsamps)?trd->nsamps-1:joff;
							if (ddata[joff] > 0.9e30) continue;
							bdata[i] += cwt*ddata[joff];
						}
					}
				}
			}
			wti += ABS(wt);
			sspow += wt*wt*spow;
		}
		if (wti == 0.0) {
			continue;
		}
		wti = 1.0/wti;
		for (i=0,node->grid = 0.0; i<trace->nsamps; i++) {
			node->grid += bdata[i]*bdata[i];
		}
		node->grid /= trace->nsamps;
		node->grid *= wti*wti;
		if (node->grid > gmax_grid) {
			gmax_grid = node->grid;
		}
		sspow *= wti;
		node->gridratio = node->grid / sspow;
		if (node->gridratio > gmax) {
			gmax = node->gridratio;
		}
		good = 1;
	}
	if (VERBOSE) printf ("done\n");
	SCV_free_trace (trace);
	if (!good) {
		grid->az = -1.0;
		grid->slow = -1.0;
		grid->gridratio = 0.0;
		grid->grid = 0.0;
		return (1);
	}
	if (grid->type == XYGRID) {
		find1 (grid, indx, thresh*gmax, &sx, &sy, &sr);
		if (sx != 0.0 || sy != 0.0) {
			grid->az = atan2 (sx, sy);
			grid->az *= 180.0/M_PI;
			while (grid->az < 0.0) grid->az += 360.0;
			grid->slow = sqrt(sx*sx + sy*sy);
			grid->gridratio = gmax;
		} else {
			grid->az = 0.0;
			grid->slow = 0.0;
			grid->gridratio = gmax;
		}
		find1g (grid, indx, thresh*gmax_grid, &sx, &sy, &sr);
		if (sx != 0.0 || sy != 0.0) {
			grid->az_grid = atan2 (sx, sy);
			grid->az_grid *= 180.0/M_PI;
			while (grid->az_grid < 0.0) grid->az_grid += 360.0;
			grid->slow_grid = sqrt(sx*sx + sy*sy);
			grid->grid = gmax_grid;
		} else {
			grid->az_grid = 0.0;
			grid->slow_grid = 0.0;
			grid->grid = gmax_grid;
		}
	} else {
		grid->az = 0.0;
		grid->slow = 0.0;
		grid->gridratio = gmax;
		grid->az_grid = 0.0;
		grid->slow_grid = 0.0;
		grid->grid = gmax_grid;
	}

	/* Normal exit */

	return (1);
}

int
form_beams (db, scsin, gather, tstart, tend, recipes)

Dbptr       db;
Tbl *           scsin;
Apspec *               gather;
double                         tstart;
double                                 tend;
Tbl *                                        recipes;

{
	int nbeams;
	Tbl *beams;
	int i, j;
	double tpad;
	Apspec *beam;
	double t1, t2;
	int n, nn;
	Tbl *scs;
	Trace *trace;
	char label[64];
	double hang, vang;
	char *name;
	int itype;
	void *ptr;

	/* Set up time limits */

	nbeams = maxtbl(recipes);
	if (nbeams < 1) {
		fprintf (stderr, "form_beams: No beams to compute.\n");
		return (0);
	}
	beams = newtbl(1);
	if (beams == NULL) {
		fprintf (stderr, "form_beams: newtbl() error.\n");
		return (0);
	}
	for (i=0,tpad=0.0,nn=0; i<nbeams; i++) {
		if (!get_process_recipe (recipes, i, &name, &itype, &ptr)) {
			fprintf (stderr, "form_beams: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case BEAM:
			beam = ptr;
			nn++;
			break;
		default:
			beam = NULL;
			break;
		}
		if (beam == NULL) continue;
		beam->ts = tstart;
		beam->te = tend;
		beam->gather = gather;
		if (beam->tpad > tpad) tpad = beam->tpad;
		if (settbl(beams, -1, beam) < 0) {
			fprintf (stderr, "form_beams: settbl() error.\n");
			return (0);
		}
	}
	if (nn == 0) {
		return (1);
	}
	nbeams = nn;
	t1 = tstart - tpad;
	t2 = tend + tpad;

	/* Make the station-channel table */

	scs = scsin;
	n = maxtbl(scs);
	if (n < 1) {
		fprintf (stderr, "form_beams: No data to process.\n");
		return (0);
	}

	/* Set up beam channel time shifts */

	if (!setup_time_shifts (db, beams)) {
		fprintf (stderr, "form_beams: setup_time_shifts() error.\n");
		return (0);
	}

	/* Input data channel loop */

	for (i=0,nn=0,trace=NULL; i<n; i++) {
		char sta[32], chan[32];

		/* Do we need this channel? */

		get_sc_stachan (gettbl(scs, i), sta, chan);
		if (!need_channel (beams, sta, chan)) continue;

		/* Read in trace */

		SCV_free_trace (trace);
		trace = read_sc (gettbl(scs, i), t1, t2, &hang, &vang);
		if (trace == NULL) {
			fprintf (stderr, "form_beams: Unable to read input trace for %s:%s.\n",
									sta, chan);
			continue;
		}

		/* Process single channel trace */

/*		proc_trace ("strace", trace, tstart, tend, 2.0);*/
		nn++;

		/* Beam loop */

		for (j=0; j<nbeams; j++) {
			beam = (Apspec *) gettbl (beams, j);
			if (!process_channel (beam, &trace, sta, chan)) {
				fprintf (stderr, "form_beams: process_channel() error for %s:%s and beam # %d.\n",
									sta, chan, j+1);
				return (0);
			}
		}
	}

	sprintf (label, "Night_%d", nn);
/*	proc_plot ("strace", label, 0);*/

        /* Fix up beams */
 
        for (j=0; j<nbeams; j++) {
                beam = (Apspec *) gettbl (beams, j);
                if (!norm_beam (beam)) {
                        fprintf (stderr, "form_beams: norm_beam() error.\n");
                        return (0);
                }
		beam->tr = (Trace *) convert_trace (beam->tr, "t4");
		beam->tr = (Trace *) SCV_trace_fixgaps (beam->tr, "segment");
		beam->tr = (Trace *) SCV_trace_tofloat (beam->tr, 0);
		if (beam->tr == NULL) {
			fprintf (stderr, "form_beams: Problem fixing beam.\n");
			return (0);
		}
        }



	/* Normal exit */

	return (1);
}

int
process_channel (beam, tracei, sta, chan)

Apspec *       beam;
Trace **               tracei;
char *                        sta;
char *                             chan;

{
	int nchans;
	int i;
	Channelspec *beamchan;
	int ishift;
	Trace *tr, *trd, *itrace;
	double x, tshift, time;
	int ioff;
	float wt;
	float *data2;
	Trace *trace;
	double cwt;

	/* Find the right channel */

	nchans = maxtbl(beam->chans);
	for (i=0; i<nchans; i++) {
		beamchan = (Channelspec *) gettbl (beam->chans, i);
		if (!strcmp(sta,beamchan->sta) && !strcmp(chan,beamchan->chan)) break;
	}
	if (i == nchans) return (1);
	tshift = beamchan->tcor+beamchan->tshift;
	if (beamchan->wt == 0.0) return (1);

	/* Filter input data */

	trace = *tracei;
	if (strcmp(beamchan->gap, "segment")) {
		trace = (Trace *) SCV_trace_toraw (trace, 0);
		trace = (Trace *) SCV_trace_fillgaps (trace);
		trace = (Trace *) SCV_trace_fixgaps (trace, beamchan->gap);
		trace = (Trace *) SCV_trace_tofloat (trace, 0);
	}
	*tracei = trace;
	itrace = trace;
	trace = filter_trace (trace, beamchan->filter, 1);
	if (trace == NULL) {
		fprintf (stderr, "process_channel: filter_trace() error.\n");
		return (0);
	}

	/* Setup up beam trace structure */

	if (beam->tr == NULL) {
		beam->tr = (Trace *) copy_trace (trace, 1);
		if (beam->tr == NULL) {
			fprintf (stderr, "process_channel: copy_trace() error.\n");
			if (trace != itrace) SCV_free_trace (trace);
			return (0);
		}
		for (tr=beam->tr; tr!=NULL; tr=tr->next) {
			ishift = (x=tshift/tr->dt)>=0.0?x+0.5:x-0.5;
			tr->tstart += ishift*tr->dt;
			data2 = (float *) my_malloc ("process_channel: data2",
							tr->nsamps*sizeof(float));
			if (data2 == NULL) {
				fprintf (stderr, "process_channel: malloc() error.\n");
				if (trace != itrace) SCV_free_trace (trace);
				return (0);
			}
			for (i=0; i<tr->nsamps; i++) {
				data2[i] = 0.0;
				if (tr->data[i] > 0.9e30) continue;
				data2[i] = beamchan->wt;
			}
			tr->scv = (SCV *) data2;
		}
		if (beamchan->wt == 1.0) return (1);
		for (tr=beam->tr; tr!=NULL; tr=tr->next) {
			for (i=0; i<tr->nsamps; i++) tr->data[i] *= beamchan->wt;
		}
		if (trace != itrace) SCV_free_trace (trace);
		return (1);
	}

	/* Sum into beam trace */

	cwt = beamchan->wt*beamchan->acor;
	if (cwt == 1.0) {
		int good;

		for (tr=beam->tr; tr!=NULL; tr=tr->next) {
			data2 = (float *) tr->scv;
			for (i=0; i<tr->nsamps; i++) {
				if (tr->data[i] > 0.9e30) continue;
				time = tr->tstart + i*tr->dt;
				time -= tshift;
				good = 0;
				for (trd=trace; trd!=NULL; trd=trd->next) {
					ioff = (time - trd->tstart)/trd->dt + 0.5;
					if (ioff < 0) continue;
					if (ioff >= trd->nsamps) continue;
					tr->data[i] += trd->data[ioff];
					good = 1;
					break;
				}
				if (!good) {
					tr->data[i] = 1.e30;
				} else {
					data2[i] += 1.0;
				}
			}
		}
	} else {
		int good;

		wt = beamchan->wt;
		for (tr=beam->tr; tr!=NULL; tr=tr->next) {
			data2 = (float *) tr->scv;
			for (i=0; i<tr->nsamps; i++) {
				if (tr->data[i] > 0.9e30) continue;
				time = tr->tstart + i*tr->dt;
				time -= tshift;
				good = 0;
				for (trd=trace; trd!=NULL; trd=trd->next) {
					ioff = (time - trd->tstart)/trd->dt + 0.5;
					if (ioff < 0) continue;
					if (ioff >= trd->nsamps) continue;
					tr->data[i] += cwt*trd->data[ioff];
					good = 1;
					break;
				}
				if (!good) {
					tr->data[i] = 1.e30;
				} else {
					data2[i] += wt;
				}
			}
		}
	}
	if (trace != itrace) SCV_free_trace (trace);

	/* Normal exit */

	return (1);
}

int
norm_beam (beam)

Apspec *       beam;

{
	Trace *tr;
	int i;
	float *data2;

	for (tr=beam->tr; tr!=NULL; tr=tr->next) {
		data2 = (float *) tr->scv;
		for (i=0; i<tr->nsamps; i++) {
			if (tr->data[i] > 0.9e30) continue;
			tr->data[i] /= data2[i];
		}
		my_free (data2);
		tr->scv = NULL;
	}

	/* Normal exit */

	return (1);
}
int
need_channel (beams, sta, chan)

Tbl *         beams;
char *               sta;
char *                    chan;

{
	int nbeams;
	int i, j, n;
	Apspec *beam;
	Channelspec *beamchan;

	nbeams = maxtbl(beams);
	for (i=0; i<nbeams; i++) {
		beam = (Apspec *) gettbl (beams, i);
		n = maxtbl(beam->chans);
		for (j=0; j<n; j++) {
			beamchan = (Channelspec *) gettbl (beam->chans, j);
			if (!strcmp(sta,beamchan->sta) && !strcmp(chan,beamchan->chan)) return (1);
		}
	}
	return (0);
}

int
need_channelg (grids, sta, chan)

Tbl *         grids;
char *               sta;
char *                    chan;

{
	int ngrids;
	int i, j, n;
	Apspec *grid;
	Channelspec *gridchan;
	int rotate;

	ngrids = maxtbl(grids);
	for (i=0; i<ngrids; i++) {
		grid = (Apspec *) gettbl (grids, i);
		n = maxtbl(grid->chans);
		for (j=0; j<n; j++) {
			gridchan = (Channelspec *) gettbl (grid->chans, j);
			if (need_channelgchan (gridchan, sta, chan, &rotate)) return (1);
		}
	}
	return (0);
}

int
need_channelgchan (gridchan, sta, chan, rotate)

Channelspec *     gridchan;
char *                       sta;
char *                            chan;
int *                                   rotate;

{
	int l;

	*rotate = 0;
	if (strcmp(sta,gridchan->sta)) return (0);
	if (!strcmp(chan,gridchan->chan)) return (1);
	l = strlen(gridchan->chan);
	if (gridchan->chan[l-1] != 'R'
			&& gridchan->chan[l-1] != 'T'
			&& gridchan->chan[l-1] != 'N'
			&& gridchan->chan[l-1] != 'E') return (0);
	if (l != strlen(chan)) return (0);
	if (strncmp(chan, gridchan->chan, l-1)) return (0);
	if (chan[l-1] == 'N' || chan[l-1] == 'E') {
		if (gridchan->chan[l-1] == 'R') *rotate = RADIAL;
		if (gridchan->chan[l-1] == 'T') *rotate = TRANSVERSE;
		if (gridchan->chan[l-1] == 'N') *rotate = NORTH;
		if (gridchan->chan[l-1] == 'E') *rotate = EAST;
		return (1);
	}
	return (0);
}

int
setup_geom (db, grids)

Dbptr       db;
Tbl *           grids;

{
	Apspec *grid;
	int ngrids;
	int i;

	ngrids = maxtbl(grids);
	for (i=0; i<ngrids; i++) {
		grid = (Apspec *) gettbl (grids, i);
		if (!setup_geom_grid (db, grid)) {
			fprintf (stderr, "setup_geom: setup_geom_grid() error.\n");
			return (0);
		}
	}

	/* Normal exit */

	return (1);
}

int
setup_geom_grid (db, grid)

Dbptr       db;
Apspec *           grid;

{
	int j, n;
	Channelspec *gridchan;

	double rnorth, reast;
	double cnorth, ceast;
	double ns, es;


	/* Find ref station */

	if (!find_sta(db, grid->refsta, &rnorth, &reast)) {
		fprintf (stderr, "setup_geom_grid: Unable to find ref station '%s'.\n",
										grid->refsta);
		return (0);
	}

	/* grid channel loop */

	n = maxtbl(grid->chans);
	ns = grid->slowi*cos(grid->azimuth*M_PI/180.0);
	es = grid->slowi*sin(grid->azimuth*M_PI/180.0);
	for (j=0; j<n; j++) {
		gridchan = (Channelspec *) gettbl (grid->chans, j);
		if (gridchan == NULL) {
			fprintf (stderr, "setup_geom_grid: gettbl() error.\n");
			return (0);
		}

		/* Find channel station */

		if (!find_sta(db, gridchan->sta, &cnorth, &ceast)) {
			fprintf (stderr, "setup_geom_grid: Unable to find channel station '%s' for chan # %d.\n",
									gridchan->sta, j+1);
			return (0);
		}

		/* Compute offsets */

		gridchan->dx = (ceast-reast);
		gridchan->dy = (cnorth-rnorth);
		gridchan->tshift = (cnorth-rnorth)*ns + (ceast-reast)*es;
	}

	/* Normal exit */

	return (1);
}

int
setup_time_shifts (db, beams)

Dbptr              db;
Tbl *                  beams;

{
	int nbeams;
	int i, j, n;
	Apspec *beam;
	Channelspec *beamchan;

	nbeams = maxtbl(beams);
	for (i=0; i<nbeams; i++) {
		double rnorth, reast;
		double cnorth, ceast;
		double ns, es;

		beam = (Apspec *) gettbl (beams, i);

		/* Find ref station */

		if (!find_sta(db, beam->refsta, &rnorth, &reast)) {
			fprintf (stderr, "setup_time_shifts: Unable to find ref station '%s' for beam # %d.\n",
										beam->refsta, i+1);
			return (0);
		}
		ns = beam->slowi*cos(beam->azimuth*M_PI/180.0);
		es = beam->slowi*sin(beam->azimuth*M_PI/180.0);

		/* beam channel loop */

		n = maxtbl(beam->chans);
		for (j=0; j<n; j++) {
			beamchan = (Channelspec *) gettbl (beam->chans, j);
			if (beamchan == NULL) {
				fprintf (stderr, "setup_time_shifts: gettbl() error.\n");
				return (0);
			}

			/* Find channel station */

			if (!find_sta(db, beamchan->sta, &cnorth, &ceast)) {
				fprintf (stderr, "setup_time_shifts: Unable to find channel station '%s' for beam chan # %d %d.\n",
										beamchan->sta, i+1, j+1);
				return (0);
			}

			/* Compute time shift */

			beamchan->tshift = (cnorth-rnorth)*ns + (ceast-reast)*es;
		}
	}

	/* Normal exit */

	return (1);
}

int
find_sta (db, sta, dnorth, deast)

Dbptr     db;
char *        sta;
double *           dnorth;
double *                   deast;

{
	int n;
	char stai[32];
	double dnorthi, deasti;

	db = dblookup (db, 0, "site", 0, 0);
	dbquery (db, dbRECORD_COUNT, &n);
	for (db.record=0; db.record<n; db.record++) {
		dbgetv (db, 0, 	"sta", stai,
				"dnorth", &dnorthi,
				"deast", &deasti,
				NULL);
		if (!strcmp(sta, stai)) {
			*dnorth = dnorthi;
			*deast = deasti;
			return (1);
		}
	}
	return (0);
}

Trace *
make_beam (chans, sx, sy, tstart, tend)

Tbl *      chans;
double                sx;
double                    sy;
double                        tstart;
double                                tend;

{
	int ichan, nchans;
	int i, j, n;
	Channelspec *gchan;
	Trace *trace=NULL, *tr, *trd;
	float *data2=NULL;
	double tshift, time;
	float wt;
	int ishift, ioff, x;
	int nn;
	double cwt;

	nchans = maxtbl (chans);

	for (ichan=0; ichan<nchans; ichan++) {
		gchan = (Channelspec *) gettbl (chans, ichan);
		trd = gchan->tr;
		if (trd == NULL) continue;
		if (trd->next) continue;
		tshift = sx*gchan->dx + sy*gchan->dy + gchan->tcor;
		if (trace == NULL) {
			int i0;

			trace = (Trace *) copy_trace (trd, 1);
			if (trace == NULL) {
				fprintf (stderr, "make_beam: copy_trace() error.\n");
				SCV_free_trace (trace);
				return (0);
			}
			ishift = (x=tshift/trace->dt)>=0.0?x+0.5:x-0.5;
			trace->tstart += ishift*trace->dt;
			for (i=0,i0=-1; i<trace->nsamps; i++) {
				time = trace->tstart + i*trace->dt;
				if (time < tstart) continue;
				if (time > tend) break;
				if (i0 < 0) i0 = i;
			}
			if (i0 < 0) {
				SCV_free_trace (trace);
				trace = NULL;
				continue;
			}
			for (j=i0; j<i; j++) trace->data[j-i0] = trace->data[j];
			trace->tstart += i0*trace->dt;
			trace->nsamps = i-i0;
			if (trace->nsamps < 1) {
				SCV_free_trace (trace);
				trace = NULL;
				continue;
			}
			data2 = (float *) my_malloc ("make_beam: data2",
						trace->nsamps*sizeof(float));
			if (data2 == NULL) {
				fprintf (stderr, "make_beam: malloc() error.\n");
				SCV_free_trace (trace);
				return (0);
			}
			for (i=0; i<trace->nsamps; i++) {
				data2[i] = 0.0;
				if (trace->data[i] > 0.9e30) continue;
				data2[i] = gchan->wt;
			}
			if (gchan->wt == 1.0) continue;
			for (i=0; i<trace->nsamps; i++) trace->data[i] *= gchan->wt;
			continue;
		}
		cwt = gchan->wt*gchan->acor;
		if (cwt == 1.0) {
			for (i=0; i<trace->nsamps; i++) {
				if (trace->data[i] > 0.9e30) continue;
				time = trace->tstart + i*trace->dt;
				time -= tshift;
				ioff = (time - trd->tstart)/trd->dt + 0.5;
				if (ioff < 0) continue;
				if (ioff >= trd->nsamps) break;
				trace->data[i] += trd->data[ioff];
				data2[i] += 1.0;
			}
		} else {
			wt = gchan->wt;
			for (i=0; i<trace->nsamps; i++) {
				if (trace->data[i] > 0.9e30) continue;
				time = trace->tstart + i*trace->dt;
				time -= tshift;
				ioff = (time - trd->tstart)/trd->dt + 0.5;
				if (ioff < 0) continue;
				if (ioff >= trd->nsamps) break;
				trace->data[i] += cwt*trd->data[ioff];
				data2[i] += wt;
			}
		}
	}
	if (trace == NULL) return (trace);
	for (i=0; i<trace->nsamps; i++) {
		if (trace->data[i] > 0.9e30) continue;
		trace->data[i] /= data2[i];
	}
	my_free (data2);
	return (trace);
}

double
s2n (trace, t0n, twn, t0s, tws)

Trace *trace;
double      t0n, twn, t0s, tws;

{
	double nrms=0.0, srms=0.0;
	double time;
	int nn=0, ns=0;
	int i;

	for (i=0; i<trace->nsamps; i++) {
		time = trace->tstart + i*trace->dt;
		if (time >= t0n && time <= t0n+twn) {
			nrms += trace->data[i]*trace->data[i];
			nn++;
		}
		if (time >= t0s && time <= t0s+tws) {
			srms += trace->data[i]*trace->data[i];
			ns++;
		}
	}
	if (nn == 0 || ns == 0) return (0.0);
	nrms = sqrt(nrms/nn);
	srms = sqrt(srms/ns);
	return (srms/nrms);
}

float *
stack_grids (grid)

Apspec *     grid;

{
	float *z;
	int i, j, n, nn;
	Grid *grd;

	if (!grid->savegrids || !grid->grids) return (NULL);
	n = maxtbl(grid->grids);
	if (n < 1) return (NULL);
	nn = maxtbl(grid->grid_nodes);
	z = (float *) my_malloc ("stack_grids: z", nn*sizeof(float));
	if (z == NULL) return (NULL);
	for (j=0; j<nn; j++) {z[j] = 0.0;}
	for (i=0; i<n; i++) {
		grd = (Grid *) gettbl (grid->grids, i);
		for (j=0; j<nn; j++) {
			z[j] += grd->power[j];
		}
	}
	return (z);
}

#define	UNUSED	0
#define	IN	1
#define	OUT	2
#define	NEXT	3

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


int
find3 (grid, z, indx, rthresh, az, slo, pow)

Apspec *grid;
int indx;
float *z;
double rthresh;
double *az;
double *slo;
double *pow;

{
	double zmax, sx, sy, sr;
	int i, n;

	n = maxtbl(grid->grid_nodes);
	zmax = 0.0;
	for (i=0; i<n; i++) {
		if (z[i] > zmax) {
			zmax = z[i];
		}
	}
	find2 (grid, z, indx, rthresh*zmax, &sx, &sy, &sr, pow);
	if (sx != 0.0 || sy != 0.0) {
		*az = atan2 (sx, sy);
		*az *= 180.0/M_PI;
		while (*az < 0.0) *az += 360.0;
		*slo = sqrt(sx*sx + sy*sy);
	} else {
		*az = 0.0;
		*slo = 0.0;
	}
}

int
find2 (grid, z, indx, thresh, sx, sy, sr, pow)

Apspec *grid;
int indx;
float *z;
double thresh;
double *sx;
double *sy;
double *sr;
double *pow;

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
	if (nr == 0) {
		*sx = 0.0;
		*sy = 0.0;
		*sr = 0.0;
		*pow = 0.0;
	} else {
		*sx = regions[indx].xcentroid*xscale + grid->xmin;
		*sy = regions[indx].ycentroid*yscale + grid->ymin;
		*sr = sqrt(da*regions[indx].size)/M_PI;
		*pow = regions[indx].peak;
		my_free (regions);
	}
}

int
find1 (grid, indx, thresh, sx, sy, sr)

Apspec *grid;
int indx;
double thresh;
double *sx;
double *sy;
double *sr;

{
	int i, n;
	Gridnode *node;
	float *z;
	double pow;

	n = grid->nx*grid->ny;
	z = (float *) my_malloc ("find1: z", n*sizeof(float));
	if (z == NULL) return (0);
	for (i=0; i<n; i++) {
		node = (Gridnode *) gettbl (grid->grid_nodes, i);
		z[i] = node->gridratio;
	}
	find2 (grid, z, indx, thresh, sx, sy, sr, &pow);
	my_free (z);
}

int
find1g (grid, indx, thresh, sx, sy, sr)

Apspec *grid;
int indx;
double thresh;
double *sx;
double *sy;
double *sr;

{
	int i, n;
	Gridnode *node;
	float *z;
	double pow;

	n = grid->nx*grid->ny;
	z = (float *) my_malloc ("find1: z", n*sizeof(float));
	if (z == NULL) return (0);
	for (i=0; i<n; i++) {
		node = (Gridnode *) gettbl (grid->grid_nodes, i);
		z[i] = node->grid;
	}
	find2 (grid, z, indx, thresh, sx, sy, sr, &pow);
	my_free (z);
}

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


int
find6 (grid, z, indx, rthresh, az, slo, width, pow)

Apspec *grid;
int indx;
float *z;
double rthresh;
double *az;
double *slo;
double *width;
double *pow;

{
	double zmax, sx, sy;
	int i, n;

	n = maxtbl(grid->grid_nodes);
	zmax = 0.0;
	for (i=0; i<n; i++) {
		if (z[i] > zmax) {
			zmax = z[i];
		}
	}
	find2 (grid, z, indx, rthresh*zmax, &sx, &sy, width, pow);
	if (sx != 0.0 || sy != 0.0) {
		*az = atan2 (sx, sy);
		*az *= 180.0/M_PI;
		while (*az < 0.0) *az += 360.0;
		*slo = sqrt(sx*sx + sy*sy);
	} else {
		*az = 0.0;
		*slo = 0.0;
	}
}


/* $Id: beam_subs.c,v 1.1 2001-06-15 00:18:28 kent Exp $ */
