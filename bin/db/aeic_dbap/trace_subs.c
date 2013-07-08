
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "tr.h"
#include "db.h"
#include "arrays.h"
#include "scv2.h"

Trace *read_trace();
Trace *convert_trace();
Trace *newtrace();

int
write_trace (db, sta, chan, dir, dfile, trace, tstart, tend, overwrite, dbwfdisc)

Dbptr        db;
char *           sta;
char *                chan;
char *                      dir;
char *                           dfile;
Trace *                                 trace;
double                                         tstart;
double                                                 tend;
int                                                          overwrite;
Dbptr *                                                                 dbwfdisc;

{
	int fd;
	char outdir[1024];
	char outbase[1024];
	char fname[1024];
	int chanid;
	double calib, calper;
	int i, n, sz, size, ret;
	int is, ie, ns;
	Dbvalue dbv;

	is = (tstart-trace->tstart)/trace->dt + 0.0001;
	ie = (tend-trace->tstart)/trace->dt - 0.0001;
	if (is > trace->nsamps-1) return (1);
	if (ie < 0) return (1);
	if (is < 0) is = 0;
	if (ie > trace->nsamps-1) ie = trace->nsamps-1;
	ns = ie - is + 1;
	if (ns < 1) return (1);
	tstart = trace->tstart+is*trace->dt;
	db = dblookup (db, 0, "wfdisc", 0, 0);
	if (dir[0] == '/') {
		sprintf (fname, "%s/%s", dir, dfile);
	} else {
		dbquery (db, dbTABLE_DIRNAME, &dbv);
		strcpy (outbase, dbv.t);
		sprintf (fname, "%s/%s/%s", outbase, dir, dfile);
	}
	if (!overwrite) {
		if (zaccess(fname, F_OK) != -1) {
			fprintf (stderr, "write_trace: wf file conflict for '%s'.\n",
								fname);
			return (0);
		}
	}
	dirbase (fname, outdir, outbase);
	if (makedir(outdir) == -1) {
                fprintf (stderr, "write_trace: Unable to create %s\n", outdir);
                return (0);
        }
	fd = open (fname, (O_RDWR|O_CREAT|O_TRUNC), 0666);
	if (fd < 0) {
		fprintf (stderr, "write_trace: Open error on '%s'.\n", fname);
		return (0);
	}
	sz = atoi(&trace->rawdata_format[strlen(trace->rawdata_format)-1]);
	size = sz*ns;
	ret = write (fd, &(((char *)trace->raw_data)[is*sz]), size);
	close (fd);
	if (ret < size) {
		fprintf (stderr, "write_trace: Write error on '%s'.\n", fname);
		return (0);
	}
        db.record = dbNULL;
        db.field = dbALL;
        dbget(db, 0);
        db.record = dbSCRATCH;
	dbputv (db, 0,	"sta", sta,
			"chan", chan,
			"time", tstart,
			"wfid", dbnextid(db, "wfid"),
/*			"chanid", chanid,*/
			"jdate", yearday(tstart),
			"endtime", tstart+trace->dt*(ns-1),
			"nsamp", ns,
			"samprate", 1.0/trace->dt,
			"calib", trace->calib,
			"calper", trace->calper,
			"instype", "BEAM",
			"segtype", trace->rawdata_type,
			"datatype", trace->rawdata_format,
/*			"clip", clip,*/
			"dir", dir,
			"dfile", dfile,
			"foff", 0,
			NULL);
	db.record = dbadd (db, 0);
	(*dbwfdisc) = db;
	return (1);
}

Trace *
convert_trace (trace, format)

Trace *trace;
char *format;

{
	Trace *tr;

	if (format) {
		for (tr=trace; tr!=NULL; tr=tr->next) {
			strcpy (tr->rawdata_format, format);
		}
	}
	trace = (Trace *) SCV_trace_toraw (trace, 0);
	trace = (Trace *) SCV_trace_fillgaps (trace);
	return (trace);
}

Trace *
copy_trace (trace, copydata)

Trace *     trace;
int                copydata;

{
	Trace *trhead, *tr;

	trhead = NULL;
	for (tr=trace; tr!=NULL; tr=tr->next) {
		trhead = (Trace *) copytr (trhead, tr, copydata);
		if (trhead == NULL) {
			fprintf (stderr, "copy_trace: copytr() error.\n");
			return (NULL);
		}
	}
	for (;trhead->prev!=NULL;trhead=trhead->prev);
	return (trhead);
}

Trace *
make_trace (trace, ndata)

Trace *     trace;
int                ndata;

{
	Trace *trhead, *tr;

	trhead = NULL;
	if (trace == NULL) {
		trhead = (Trace *) copytr (NULL, NULL, 0);
		if (trhead == NULL) {
			fprintf (stderr, "make_trace: copytr() error.\n");
			return (NULL);
		}
		trhead->data = (float *) my_malloc ("make_trace: trhead->data", ndata*sizeof(float));
		if (trhead->data == NULL) {
			fprintf (stderr, "make_trace: malloc() error.\n");
			return (NULL);
		}
		trhead->data_free = trhead->data;
		trhead->data_malloc = ndata*sizeof(float);
		trhead->raw_data = NULL;
		trhead->rawdata_free = NULL;
		trhead->rawdata_malloc = 0;
		return (trhead);
	}
	for (tr=trace; tr!=NULL; tr=tr->next) {
		trhead = (Trace *) copytr (trhead, tr, 0);
		if (trhead == NULL) {
			fprintf (stderr, "make_trace: copytr() error.\n");
			return (NULL);
		}
		trhead->data = (float *) my_malloc ("make_trace: trhead->data", ndata*sizeof(float));
		if (trhead->data == NULL) {
			fprintf (stderr, "make_trace: malloc() error.\n");
			return (NULL);
		}
		trhead->data_free = trhead->data;
		trhead->data_malloc = ndata*sizeof(float);
		trhead->raw_data = NULL;
		trhead->rawdata_free = NULL;
		trhead->rawdata_malloc = 0;
	}
	for (;trhead->prev!=NULL;trhead=trhead->prev);
	return (trhead);
}

int
decimate_trace (trace, ncoefs, coefs, dec_fac, tref)

Trace *trace;
Tbl *ncoefs;
Tbl *coefs;
Tbl *dec_fac;
double tref;

{
	Trace *tr;
	int i, j, nstages;
	float *cfs;
	int ncfs, decfac;
	int ns, nsout, ioff;
	float *buf=NULL;

	nstages = maxtbl(ncoefs);
	for (tr=trace; tr!=NULL; tr=tr->next) {
		ns = tr->nsamps;
		buf = (float *) my_malloc ("decimate_trace: buf", ns*sizeof(float));
		if (buf == NULL) {
			fprintf (stderr, "decimate_trace: Malloc error.\n");
			return (0);
		}
		for (i=0; i<nstages; i++) {
			cfs = (float *) gettbl (coefs, i);
			ncfs = *((int *) gettbl (ncoefs, i));
			decfac = *((int *) gettbl (dec_fac, i));
			ns = tr->nsamps;
			ioff = (tr->tstart-tref)/tr->dt + 0.5;
			ioff = decfac - (ioff%decfac);
			if (ioff == decfac) ioff = 0;
			convsym (tr->data, ns, cfs, ncfs, decfac, ioff, &nsout, buf);
			tr->tstart += ioff*tr->dt;
			tr->dt *= decfac;
			tr->nsamps = nsout;
			for (j=0; j<nsout; j++) tr->data[j] = buf[j];
		}
		my_free (buf);
	}
	return (1);
}

int
convsym (datain, nsin, coefs, ncoefs, decfac, ioff, nsout, dataout)

float *datain;
int nsin;
float *coefs;
int ncoefs;
int decfac;
int ioff;
int *nsout;
float *dataout;

{
	int i, j;
	float hold;
	int i1, i2, n1, n2;

	n1 = ncoefs-1;
	n2 = nsin-ncoefs;
	for (i=ioff,(*nsout)=0; i<nsin; i+=decfac,(*nsout)++) {
		if (i >= n1 && i <= n2) {
			hold = datain[i]*coefs[0];
			for (j=1; j<ncoefs; j++) hold += coefs[j]*(datain[i+j]+datain[i-j]);
		} else {
			i1 = i - n1;
			i2 = i + n1;
			hold = 0.0;
			for (j=i1; j<0; j++) hold += datain[0]*coefs[i-j];
			for (j=nsin; j<=i2; j++) hold += datain[nsin-1]*coefs[j-i];
			if (i1 < 0) i1 = 0;
			if (i2 > nsin-1) i2 = nsin-1;
			for (j=i1; j<i; j++) hold += datain[j]*coefs[i-j];
			for (j=i; j<=i2; j++) hold += datain[j]*coefs[j-i];
		}
		dataout[(*nsout)] = hold;
	}
}

int
add_trace (db, tstart, tend, trace, traceo)

Dbptr db;
double          tstart;
double                  tend;
Trace *                       trace;
Trace **                            traceo;

{
	Trace *tr, *trn;

	trn = read_trace (db, tstart, tend);
	if (trn == NULL) {
		fprintf (stderr, "add_trace: read_trace() error.\n");
		return (0);
	}
	if (trn->raw_data == NULL) {
		SCV_free_trace (trn);
		*traceo = trace;
		return (1);
	}
	if (trace == NULL) {
		*traceo = trn;
		return (1);
	}
	for (tr=trace; tr->next!=NULL; tr=tr->next);
	tr->next = trn;
	trn->prev = tr;
	*traceo = trace;
	return (1);
}

Trace *
read_trace (db, tstart, tend)

Dbptr db;
double          tstart;
double                  tend;

{
	char fname[1024];
	char dtype[8];
	char segtype[8];
	int foff, nsamp;
	void *data;
	Trace *trace;
	double time, dt, samprate;
	double calib, calper;
	double ts, te;
	int isamp, jsamp, size;
	int ret;

	if (dbextfile (db, "wfdisc", fname) < 1) {
		fprintf (stderr, "read_trace: Unable to find input file '%s'\n",
						fname);
		return (NULL);
	}
	dbgetv (db, 0, "time", &time, "samprate", &samprate,
				"nsamp", &nsamp, "datatype", dtype, 
				"segtype", segtype, "foff", &foff, 
				"calib", &calib, "calper", &calper, NULL);

	data = NULL;
	ret = trgetwf (db, NULL, (Trsample **) &data, 0, tstart, tend,
				&ts, &te, &nsamp, 0, 0);
	size = 4;

	switch (ret) {
	default:
	case 0:
		if (ret < 0) data = NULL;
		if (ts == 0.0 && te == 0.0) data = NULL;
		break;

	case -9: /* no data */
	    clear_register (0); 
	    data = NULL;
	    break; 

	case -1:
	case -2:
	case -3:
	case -5:
	case -6:
	case -7:
	case -8:
		complain (0, 
		"read_trace: trgetwf() error.\n");
		return (NULL);
	}

	if (data == (void *) -1) {
		data = NULL;
	}

	if (data == (void *) -2) {
		clear_register (0);
		data = NULL;
	}
	trace = newtrace();
	if (trace == NULL) {
		fprintf (stderr, "read_trace: newtrace() error.\n");
		my_free (data);
		return (NULL);
	}
	trace->scv = NULL;
	trace->tstart = ts;
	trace->dt = 1.0/samprate;
	trace->nsamps = nsamp;
	trace->calib = calib;
	trace->calper = calper;
	strcpy (trace->rawdata_format, "t4");
	strcpy (trace->rawdata_type, segtype);
	strcpy (trace->input_units, "");
	strcpy (trace->output_units, "");
	trace->data = NULL;
	trace->data_free = NULL;
	trace->data_malloc = 0;
	trace->raw_data = data;
	trace->rawdata_free = data;
	if (data) trace->rawdata_malloc = nsamp*size; else trace->rawdata_malloc = 0;
	trace->prev = NULL;
	trace->next = NULL;
	if (data) {
		trace = (Trace *) SCV_trace_fixgaps (trace, "segment");
	}
	return (trace);
}

int
read_file (fname, foff, datatype, nsamps, buf)

char *fname;
int foff;
char *datatype;
int *nsamps;
void **buf;

{
	FILE *file;
	int n, size;

	size = atoi(&datatype[strlen(datatype)-1]);
	file = zopen (fname);
	if (file == NULL) {
		fprintf (stderr, "read_file: Unable to open '%s'\n",
							fname);
		return (0);
	}
	if (fseek(file, foff, 0) < 0) {
		fprintf (stderr, "read_file: fseek() error on '%s'\n",
							fname);
		fclose (file);
		return (0);
	}
	*buf = (void *) my_malloc ("read_file: *buf", size*(*nsamps));
	if (*buf == NULL) {
		fprintf (stderr, "read_file: Malloc error.\n");
		fclose (file);
		return (0);
	}
	if ((n=fread (*buf, size, *nsamps, file)) < *nsamps) {
		if (n < 1) {
			fprintf (stderr, "read_file: fread() error on '%s'\n",
							fname);
			fclose (file);
			my_free (*buf);
			return (0);
		} else {
			fprintf (stderr, "read_file: Read %d samples but expected %d on '%s'\n",
							n, *nsamps, fname);
			*nsamps = n;
		}
	}
	fclose (file);
	return (1);
}

int
zaccess (path, mode)

char *path;
int mode;

{
	char fname[1024];

	if (access(path, mode) == 0) return (0);
	sprintf (fname, "%s.Z", path);
	if (access(fname, mode) == 0) return (1);
	sprintf (fname, "%s.gz", path);
	if (access(fname, mode) == 0) return (2);
	return (-1);
}

#define	INT(x,y)	((x)<0.0?((x)/(y)-0.5):((x)/(y)+0.5))

Trace *
trace_add (t1, t2)

Trace *    t1;
Trace *        t2;

{
	Trace *trout, *tr, *trd, *tr2;
	double time, tend;
	int i, j, good;

	if (!t2) return (NULL);
	if (!t1) {
		trout = (Trace *) copy_trace (t2, 1);
		return (trout);
	}
	trout = t1;
	for (tr=trout; tr!=NULL; tr=tr->next) {
		for (i=0,good=0; i<tr->nsamps; i++) {
			time = tr->tstart + i*tr->dt;
			for (trd=t2; trd!=NULL; trd=trd->next) {
				j = INT(time-trd->tstart,trd->dt);
				if (j >= 0 && j < trd->nsamps) break;
			}
			if (trd == NULL) {
				tr->data[i] = 2.e30;
				continue;
			}
			good++;
			tr->data[i] += trd->data[j];
		}
		if (!good) {
			if (tr == trout) {
				if (tr->next) {
					trout = tr->next;
					trout->prev = NULL;
					tr->next = NULL;
					SCV_free_trace (tr);
					tr = trout;
				} else {
					SCV_free_trace (tr);
					return (NULL);
				}
			} else {
				if (tr->next) tr->next->prev = tr->prev;
				tr->prev->next = tr->next;
				tr2 = tr->prev;
				tr->next = NULL;
				tr->prev = NULL;
				SCV_free_trace (tr);
				tr = tr2;
			}
			continue;
		}
	}
	return (trout);
}

get_trace_maxmin (trace, ts, te, ymax, ymin)

Trace *           trace;
double                   ts;
double                       te;
double *                         ymax;
double *                               ymin;

{
	Trace *tr;
	double time;
	int i;

	*ymax = -1.e30;
	*ymin = 1.e30;
	for (tr=trace; tr!=NULL; tr=tr->next) {
		if (tr->tstart > te) continue;
		time = tr->tstart+tr->dt*(tr->nsamps-1);
		if (time < ts) continue;
		for (i=0; i<tr->nsamps; i++) {
			time = tr->tstart+tr->dt*i;
			if (time < ts) continue;
			if (time > te) break;
			if (tr->data[i] > *ymax) *ymax = tr->data[i];
			if (tr->data[i] < *ymin) *ymin = tr->data[i];
		}
	}
	return (1);
}

int
get_trace_float_data (trace, time, data)

Trace *               trace;
double                       time;
float *                            data;

{
	Trace *tr;
	double te, smprate;
	int ioff;

	for (tr=trace; tr!=NULL; tr=tr->next) {
		te = tr->tstart+tr->dt*(tr->nsamps-1);
		if (time < tr->tstart) continue;
		if (time > te) continue;
		smprate = 1.0/tr->dt;
		ioff = (time-tr->tstart)*smprate+0.00000001;
		if (tr->data == NULL) return (0);
		if (tr->data[ioff] > 1.e30 || tr->data[ioff+1] > 1.e30) {
			*data = 2.e30;
			return (1);
		}
		te = tr->tstart+tr->dt*ioff;
		*data = tr->data[ioff] + (tr->data[ioff+1]-tr->data[ioff])*(time-te)*smprate;
		return (1);
	}
	*data = 2.e30;
	return (1);
}

/* $Id: trace_subs.c,v 1.2 2002-02-07 01:23:50 kent Exp $ */
