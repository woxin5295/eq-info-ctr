
#include <stdio.h>
#include <math.h>


#include "scv2.h"

#define	GWIDTH	4.0
#define	GDT	0.5

#define VERBOSE (verbose > 1)

int verbose;

int
spgrm (trace, tstart, tend, fmin, fmax, thalf, nt, t, nf, f,
				amp, phase)

Trace *trace;
double        tstart, tend, fmin, fmax, thalf;
int *                                              nt;
float **                                               t;
int *                                                     nf;
float **                                                      f;
float **                        amp;
float **                             phase;

/*
 *	spgram will compute a time-varying spectragram by repeatedly
 *	computing spectra for moving time windowed data. The windowing
 *	function used here is a Gaussian window.
 *
 *	Inputs -	trace	= Input data trace.
 *			tstart	= Desired start time.
 *			tend	= Desired end time.
 *			fmin	= Desired lower frequency in hz.
 *			fmax	= Desired upper frequency in hz.
 *			thalf	= Time window half width in seconds.
 *				  This is the 0.5 amplitude point for
 *				  the Gaussian windowing function.
 *
 *	Outputs -	nt	= Number of time samples.
 *			t	= Array of time values relative to tstart.
 *			nf	= Number of frequency samples.
 *			f	= Array of frequency samples in hz.
 *			amp	= 2-D array of amplitudes.
 *			phase	= 2-D array of phases.
 *			NOTE: amp and phase are ordered by time first.
 */

{
	double tfactor, dt, fhalf, fnyq, arg;
	double data_tstart, data_dt;
	int data_nsamps, data_free;
	float *data, *tmp, *winf, *rfft, *ifft;
	int ntmp, ntmax, ntime, nfft;
	float dtin, dtout, df;
	int i, j, k, l, j0, k0, nout, ndt, nthw, nwin;
	double time, t0, freq;
	int front_zero, back_zero;

	if (trace == NULL) return (0);
	if (trace->nsamps < 2) return (0);
	if (trace->next) {
		fprintf (stderr, "spgrm: Warning - Will only process first segment.\n");
	}

	/* Set up limits */

	tfactor = 0.832554611 / thalf;
	dt = thalf * GDT;
	fhalf = 1.0 / (M_PI * thalf);
	if (fmax <= 0.0) fmax = 0.9*0.5/trace->dt;
	fnyq = fmax + GWIDTH*fhalf;
	if (fmin <= 0.0) fmin = 0.5/thalf;

	/* Decimate data */

	if (VERBOSE) {
		printf ("\nIn spgrm\n");
		printf ("Decimating data...");
		fflush (stdout);
	}
	if (fnyq < 0.5/trace->dt) {
		dtin = trace->dt;
		data = (float *) my_malloc ("spgrm: data", trace->nsamps*sizeof(float));
		if (data == NULL) {
			fprintf (stderr, "spgrm: Malloc error.\n");
			return (0);
		}
		data_free = 1;
		for (i=0; i<trace->nsamps; i++) data[i] = trace->data[i];
		dtout = 0.5/fnyq;
		nout = (trace->nsamps*dtout)/dtin + 1.5;
		while (1) {
			time = (nout-1)*dtout;
			if (time <= (trace->nsamps-1)*dtin) break;
			nout--;
		}
		ntmp = 10*trace->nsamps;
		tmp = (float *) my_malloc ("spgrm: tmp", ntmp*sizeof(float));
		if (tmp == NULL) {
			fprintf (stderr, "spgrm: Malloc error.\n");
			return (0);
		}
		/*printf ("spgrm: Decimating from %d to %d samples...", trace->nsamps, nout);
		fflush (stdout);*/
		resamp_ (&trace->nsamps, &dtin, &nout, &dtout, data, tmp, &ntmp);
		/*printf ("done\n");*/
		my_free (tmp);
		data_nsamps = nout;
		data_dt = dtout;
		data_tstart = trace->tstart;
	} else {
		data = trace->data;
		data_nsamps = trace->nsamps;
		data_tstart = trace->tstart;
		data_dt = trace->dt;
		data_free = 0;
	}
	if (VERBOSE) printf ("done\n");
	if (VERBOSE) {
		printf ("Setting up arrays...");
		fflush (stdout);
	}

	/* figure out time array */

	ndt = dt/data_dt + 0.5;
	dt = data_dt*ndt;
	nthw = (GWIDTH*thalf)/data_dt + 1.5;
	nwin = 2*nthw + 1;
	for (i=0; i<data_nsamps; i++) {
		time = data_tstart + data_dt*i;
		if (time < data_tstart+GWIDTH*thalf) continue;
		if (time > tstart) break;
	}
	if (i == data_nsamps) {
		fprintf (stderr, "spgrm: No data to process.\n");
		if (data_free) my_free (data);
		return (0);
	}
	if (data_tstart > tstart - GWIDTH*thalf) {
		t0 = time - data_dt;
		*nt = 2;
		front_zero = 1;
		j0 = i - ndt;
	} else {
		t0 = time;
		*nt = 1;
		front_zero = 0;
		j0 = i;
	}
	j = i;
	while (1) {
		time = t0 + (*nt-1)*dt;
		if (time > (data_tstart+(data_nsamps-1)*data_dt)-GWIDTH*thalf) break;
		if (time > tend) break;
		if (j >= data_nsamps) break;
		(*nt)++;
		j += ndt;
	}
	if (data_tstart+(data_nsamps-1)*data_dt < tend + GWIDTH*thalf) {
		(*nt)++;
		back_zero = 1;
	} else {
		back_zero = 0;
	}
	*t = (float *) my_malloc ("spgrm: *t", (*nt)*sizeof(float));
	if (*t == NULL) {
		fprintf (stderr, "spgrm: Malloc error.\n");
		return (0);
	}

	/* set up Gaussian window function */

	winf = (float *) my_malloc ("spgrm: winf", nwin*sizeof(float));
	if (winf == NULL) {
		my_free (*t);
		*t = NULL;
		fprintf (stderr, "spgrm: Malloc error.\n");
		return (0);
	}
	ntmax = 4*nwin;
	rfft = (float *) my_malloc ("spgrm: rfft", ntmax*sizeof(float));
	if (rfft == NULL) {
		my_free (*t);
		*t = NULL;
		my_free (winf);
		fprintf (stderr, "spgrm: Malloc error.\n");
		return (0);
	}
	ifft = (float *) my_malloc ("spgrm: ifft", ntmax*sizeof(float));
	if (ifft == NULL) {
		my_free (*t);
		*t = NULL;
		my_free (winf);
		my_free (rfft);
		fprintf (stderr, "spgrm: Malloc error.\n");
		return (0);
	}
	winf[nthw] = 1.0;
	for (i=nthw+1,time=data_dt; i<nwin; i++,time+=data_dt) {
		arg = time*tfactor;
		winf[i] = exp(-arg*arg);
		winf[2*nthw-i] = winf[i];
	}
	if (VERBOSE) printf ("done\n");

	/* time loop */

	dtin = data_dt;
	*f = NULL;
	if (VERBOSE) {
		printf ("Time loop");
		fflush (stdout);
	}
	for (i=0,j=j0; i< (*nt); i++,j+=ndt) {
		if (VERBOSE) {
			printf (".");
			fflush (stdout);
		}
		(*t)[i] = (t0 - tstart) + i*dt;
		if (i == 0 && front_zero) continue;
		if (i == (*nt)-1 && back_zero) {
			for (k=0; k<*nf; k++) {
				(*amp)[i+(*nt)*k] = 0.0;
				(*phase)[i+(*nt)*k] = 0.0;
			}
			continue;
		}
		for (k=0; k<ntmax; k++) {
			rfft[k] = 0.0;
			if (k > nwin) continue;
			l = j - nthw + k;
			if (l < 0) continue;
			if (l >= data_nsamps) continue;
			rfft[k] = data[l]*winf[k];
		}
		ntime = nwin;
		t2f_ (&ntime, &ntmax, &((*t)[0]), &dtin, rfft, ifft, &nfft, &df);
		if (*f == NULL) {
			*nf = 0;
			k0 = -1;
			for (k=0; k<ntime/2+1; k++) {
				freq = k*df;
				if (freq >= fmin && freq <= fmax) {
					if (k0 < 0) k0 = k;
					(*nf)++;
				}
			}
			if (*nf < 1) {
				my_free (*t);
				*t = NULL;
				my_free (winf);
				my_free (rfft);
				my_free (ifft);
				fprintf (stderr, "spgrm: No frequencies to process.\n");
				return (0);
			}
			*f = (float *) my_malloc ("spgrm: *f", (*nf)*sizeof(float));
			if (*f == NULL) {
				my_free (*t);
				*t = NULL;
				my_free (winf);
				my_free (rfft);
				my_free (ifft);
				fprintf (stderr, "spgrm: Malloc error.\n");
				return (0);
			}
			*amp = (float *) my_malloc ("spgrm: *amp", (*nf)*(*nt)*sizeof(float));
			if (*amp == NULL) {
				my_free (*t);
				*t = NULL;
				my_free (*f);
				*f = NULL;
				my_free (winf);
				my_free (rfft);
				my_free (ifft);
				fprintf (stderr, "spgrm: Malloc error.\n");
				return (0);
			}
			*phase = (float *) my_malloc ("spgrm: *phase", (*nf)*(*nt)*sizeof(float));
			if (*phase == NULL) {
				my_free (*t);
				*t = NULL;
				my_free (*f);
				*f = NULL;
				my_free (*amp);
				*amp = NULL;
				my_free (winf);
				my_free (rfft);
				my_free (ifft);
				fprintf (stderr, "spgrm: Malloc error.\n");
				return (0);
			}
			for (k=0; k<*nf; k++) {
				(*f)[k] = (k+k0)*df;
			}
		}
		if (front_zero) {
			front_zero = 0;
			for (k=0; k<*nf; k++) {
				(*amp)[i+(*nt)*k] = 0.0;
				(*phase)[i+(*nt)*k] = 0.0;
			}
			continue;
		}
		for (k=0; k<*nf; k++) {
			arg = rfft[k+k0]*rfft[k+k0] + ifft[k+k0]*ifft[k+k0];
			(*amp)[i+(*nt)*k] = sqrt(arg);
			if (arg > 0.0) {
				(*phase)[i+(*nt)*k] = atan2(ifft[k+k0], rfft[k+k0])*180.0/M_PI;
			} else {
				(*phase)[i+(*nt)*k] = 0.0;
			}
		}
	}
	if (VERBOSE) printf ("done\n");

	/* Normal exit */

	my_free (rfft);
	my_free (ifft);
	my_free (winf);
	if (data_free) my_free (data);
	return (1);
}

/* $Id: spgrm.c,v 1.1 2001-06-15 00:18:31 kent Exp $ */
