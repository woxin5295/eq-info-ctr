
#include <stdio.h>

#include "dbl2.h"
#include "csstime.h"

#define	MAXSTAS		100
#define	MAXCHANS	30
#define	MAXSEGS		2000
int nsta = 0;
struct sta_ {
	char sta[16];
	int nchan;
	struct chan_ {
		char chan[16];
		int nseg;
		struct seg_ {
			double tstart;
			double tend;
		} **segs;
	} chans[MAXCHANS];
} stas[MAXSTAS];

char table[32];

void
readcov_ (fname, sc, ts, te, iwftar, ierr, len_fname, len_sc)

char *fname;
char *sc;
double *ts;
double *te;
int *iwftar;
int *ierr;
int len_fname;
int len_sc;

{
	FILE *in=NULL;
	static char sta[512];
	static char loc[512];
	static char chan[512];
	static char stachan[512];
	static char line[512];
	int month_start, day_start, year_start, hour_start, minute_start;
	int month_end, day_end, year_end, hour_end, minute_end;
	double second_start, second_end;
	int i, j, k, n;
	double tstart, tend, duration, time;
	DBLink *dbl;
	struct date_time dt;
	struct seg_ *seg;

	double h2e();
	int compar();
	int compar2();
	int compar3();

	*ierr = 0;
	i = 0;
	while (i<len_fname && (fname[i] == ' ' || fname[i] == '\t')) i++;
	if (i == len_fname) {
		fprintf (stderr, "readcov: Blank file name\n");
		*ierr = 1; 
		return;
	}
	for (j=0; i<len_fname; i++,j++) {
		if (fname[i] == ' ' || fname[i] == '\t') break;
		sta[j] = fname[i];
	}
	sta[j] = '\0';
	i = 0;
	while (i<len_sc && (sc[i] == ' ' || sc[i] == '\t')) i++;
	if (i == len_sc) {
		fprintf (stderr, "readcov: Blank stachan\n");
		*ierr = 1; 
		return;
	}
	for (j=0; i<len_sc; i++,j++) {
		if (sc[i] == ' ' || sc[i] == '\t') break;
		stachan[j] = sc[i];
	}
	stachan[j] = '\0';
	in = fopen (sta, "r");
	dbl = NULL;
	if (in) {
		if (!fgets(line, 511, in)) {
			fprintf (stderr, "readcov: read error for %s.\n", sta);
			*ierr = 2; 
			return;
		}
		if (!strncmp(line, "css3.0", strlen("css3.0"))) 
					dbl = (DBLink *) db30_create (sta);
		fclose (in);
	} else {
		dbl = (DBLink *) db30_create (sta);
	}
	if (*iwftar) {
		strcpy (table, "wftar");
	} else {
		strcpy (table, "wfdisc");
	}
	if (dbl && (DBL_get_ntuples (dbl, table) > 0)) ; else {
		in = fopen (sta, "r");
		if (in == NULL) {
			fprintf (stderr, "readcov: fopen error for %s.\n", sta);
			*ierr = 2; 
			return;
		}
		dbl = NULL;
	}
	while (n=getstuff (in, dbl,
				sta, loc, chan, 
				&month_start, &day_start, &year_start,
				&hour_start, &minute_start, &second_start,
				&month_end, &day_end, &year_end,
				&hour_end, &minute_end, &second_end) > 0) {
		if (!match_stachans (stachan, sta, chan)) continue;
/*		printf ("%s %s %s %2.2d/%2.2d/%2.2d %2.2d:%2.2d:%06.3f\n",
				sta, loc, chan, 
				month_start, day_start, year_start,
				hour_start, minute_start, second_start);*/
		year_start += 1900;
		year_end += 1900;
		time = h2e(year_end,
				mday2doy(year_end, month_end, day_end), 
				hour_end, minute_end, second_end);
		if (time < *ts) continue;
		time = h2e(year_start,
				mday2doy(year_start, month_start, day_start), 
				hour_start, minute_start, second_start);
		if (time > *te) continue;
		for (i=0; i<nsta; i++) if (!strcmp(stas[i].sta, sta)) break;
		if (i == nsta) {
			if (nsta >= MAXSTAS) {
				fprintf (stderr, 
				"readcov: Attempt to exceed MAXSTAS\n");
				*ierr = 3;
				return;
			}
			strcpy (stas[nsta].sta, sta);
			stas[nsta].nchan = 1;
			strcpy (stas[nsta].chans[0].chan, chan);
			stas[nsta].chans[0].nseg = 1;
			stas[nsta].chans[0].segs = (struct seg_ **)
				malloc (sizeof(struct seg_ *));
			if (stas[nsta].chans[0].segs == NULL) {
				fprintf (stderr, 
					"readcov: malloc error.\n");
				*ierr = 5;
				return;
			}
			seg = (struct seg_ *) malloc (sizeof(struct seg_));
			if (seg == NULL) {
				fprintf (stderr, 
					"readcov: malloc error.\n");
				*ierr = 5;
				return;
			}
			stas[nsta].chans[0].segs[0] = seg;
			stas[nsta].chans[0].segs[0]->tstart = h2e(year_start, 
				mday2doy(year_start, month_start, day_start), 
				hour_start, minute_start, second_start);
			stas[nsta].chans[0].segs[0]->tend = h2e(year_end, 
				mday2doy(year_end, month_end, day_end), 
				hour_end, minute_end, second_end);
			nsta++;
			continue;
		}
		for (j=0; j<stas[i].nchan; j++) 
				if (!strcmp(stas[i].chans[j].chan, chan)) break;
		if (j == stas[i].nchan) {
			if (stas[i].nchan >= MAXCHANS) {
				fprintf (stderr, 
				"readcov: Attempt to exceed MAXCHANS\n");
				*ierr = 4;
				return;
			}
			strcpy (stas[i].chans[stas[i].nchan].chan, chan);
			stas[i].chans[stas[i].nchan].nseg = 1;
			stas[i].chans[stas[i].nchan].segs = (struct seg_ **)
				malloc (sizeof(struct seg_ *));
			if (stas[i].chans[stas[i].nchan].segs == NULL) {
				fprintf (stderr, 
					"readcov: malloc error.\n");
				*ierr = 5;
				return;
			}
			seg = (struct seg_ *) malloc (sizeof(struct seg_));
			if (seg == NULL) {
				fprintf (stderr, 
					"readcov: malloc error.\n");
				*ierr = 5;
				return;
			}
			stas[i].chans[stas[i].nchan].segs[0] = seg;
			stas[i].chans[stas[i].nchan].segs[0]->tstart 
			= h2e(year_start, 
				mday2doy(year_start, month_start, day_start), 
				hour_start, minute_start, second_start);
			stas[i].chans[stas[i].nchan].segs[0]->tend 
			= h2e(year_end, 
				mday2doy(year_end, month_end, day_end), 
				hour_end, minute_end, second_end);
			(stas[i].nchan)++;
			continue;
		}
		(stas[i].chans[j].nseg)++;
		stas[i].chans[j].segs = (struct seg_ **)
			realloc (stas[i].chans[j].segs,
				stas[i].chans[j].nseg*sizeof(struct seg_ *));
		if (stas[i].chans[j].segs == NULL) {
			fprintf (stderr, 
				"readcov: Realloc error.\n");
			*ierr = 5;
			return;
		}
		seg = (struct seg_ *) malloc (sizeof(struct seg_));
		if (seg == NULL) {
			fprintf (stderr, 
					"readcov: malloc error.\n");
			*ierr = 5;
			return;
		}
		stas[i].chans[j].segs[stas[i].chans[j].nseg-1] = seg;
		stas[i].chans[j].segs[stas[i].chans[j].nseg-1]->tstart
			= h2e(year_start, 
				mday2doy(year_start, month_start, day_start), 
				hour_start, minute_start, second_start);
		stas[i].chans[j].segs[stas[i].chans[j].nseg-1]->tend
			= h2e(year_end, 
				mday2doy(year_end, month_end, day_end), 
				hour_end, minute_end, second_end);
	}
	printf ("\n           IRIS/DMC JSP/GSN Station coverage for %d\n\n",
						year_start);
	printf (
" Sta Chan Nseg Start Time            End Time              Dur Prc  Av Prc\n");
	printf (
"---- ---- ---- --------------------- --------------------- ------- -------\n");
	qsort (stas, nsta, sizeof(struct sta_), compar2);
	for (i=0; i<nsta; i++) {
		qsort (stas[i].chans, 
			stas[i].nchan, sizeof(struct chan_), compar3);
		for (j=0; j<stas[i].nchan; j++) {
			qsort (stas[i].chans[j].segs, stas[i].chans[j].nseg,
				sizeof(struct seg_ *), compar);
			if (j == 0) printf ("%4.4s ", stas[i].sta);
			else printf ("     ");
			printf ("%s %5d ", stas[i].chans[j].chan,
					stas[i].chans[j].nseg);
			for (k=0; k<stas[i].chans[j].nseg; k++) {
				if (k == 0) {
					duration = 
					  stas[i].chans[j].segs[k]->tend 
					- stas[i].chans[j].segs[k]->tstart;
				} else if (stas[i].chans[j].segs[k]->tstart
					< stas[i].chans[j].segs[k-1]->tend) {
					duration += 
					  stas[i].chans[j].segs[k]->tend 
					- stas[i].chans[j].segs[k-1]->tend;
				} else {
					duration += 
					  stas[i].chans[j].segs[k]->tend 
					- stas[i].chans[j].segs[k]->tstart;
				}
			}
			tstart = stas[i].chans[j].segs[0]->tstart;
		tend = stas[i].chans[j].segs[stas[i].chans[j].nseg-1]->tend;
			e2h (tstart, &year_start, &day_start, &hour_start,
					&minute_start, &second_start);
			e2h (tend, &year_end, &day_end, &hour_end,
					&minute_end, &second_end);
			doy2mday (day_start, year_start, &month_start,
							&day_start);
			doy2mday (day_end, year_end, &month_end,
							&day_end);
			printf ("%2.2d/%2.2d/%2.2d %2.2d:%2.2d:%06.3f ",
				year_start-1900,month_start,
				day_start, hour_start,
				minute_start, second_start);
			printf ("%2.2d/%2.2d/%2.2d %2.2d:%2.2d:%06.3f ",
				year_end-1900,month_end,
				day_end, hour_end,
				minute_end, second_end);
			printf ("%6.2f%% ", 100.0*duration/(tend-tstart));
/*			if (year_start%4) 
				printf("%6.2f%%\n",100.0*duration/31536000.0);
			else
				printf("%6.2f%%\n",100.0*duration/31622400.0);*/
			printf("%6.2f%%\n",100.0*duration/(*te-*ts));
		}
	}
	if (in) fclose (in);
}

void
getnsta_ (nstas)

int *nstas;

{
	*nstas = nsta;
	return;
}

void
getsta_ (ista, sta, len_sta)

int *ista;
char *sta;
int len_sta;

{
	int i, j;

	i = *ista - 1;
	if (i < 0 || i > nsta-1) return;
	if (strlen(stas[i].sta) >= len_sta) {
		for (j=0; j<len_sta; j++) sta[j] = stas[i].sta[j];
	} else {
		strcpy (sta, stas[i].sta);
		for (j=strlen(sta); j<len_sta; j++) sta[j] = ' ';
	}
}

void
getnchan_ (ista, nchans)

int *ista;
int *nchans;

{
	int i;

	i = *ista - 1;
	if (i < 0 || i > nsta-1) return;
	*nchans = stas[i].nchan;
	return;
}

void
getchan_ (ista, ichan, chan, len_chan)

int *ista;
int *ichan;
char *chan;
int len_chan;

{
	int i, k, j;

	i = *ista - 1;
	if (i < 0 || i > nsta-1) return;
	k = *ichan - 1;
	if (k < 0 || k > stas[i].nchan-1) return;
	if (strlen(stas[i].chans[k].chan) >= len_chan) {
		for (j=0; j<len_chan; j++) chan[j] = stas[i].chans[k].chan[j];
	} else {
		strcpy (chan, stas[i].chans[k].chan);
		for (j=strlen(chan); j<len_chan; j++) chan[j] = ' ';
	}
}

void
getnseg_ (ista, ichan, nsegs)

int *ista;
int *ichan;
int *nsegs;

{
	int i, k;

	i = *ista - 1;
	if (i < 0 || i > nsta-1) return;
	k = *ichan - 1;
	if (k < 0 || k > stas[i].nchan-1) return;
	*nsegs = stas[i].chans[k].nseg;
	return;
}

void
getseg_ (ista, ichan, iseg, tstart, tend)

int *ista;
int *ichan;
int *iseg;
double *tstart;
double *tend;

{
	int i, k, j;

	i = *ista - 1;
	if (i < 0 || i > nsta-1) return;
	k = *ichan - 1;
	if (k < 0 || k > stas[i].nchan-1) return;
	j = *iseg - 1;
	if (j < 0 || j > stas[i].chans[k].nseg-1) return;
	*tstart = stas[i].chans[k].segs[j]->tstart;
	*tend = stas[i].chans[k].segs[j]->tend;
}

void
str2e_ (str, time, len_str)

char *str;
double *time;
int len_str;

{
	int i, j;
	static char string[512];

	i = 0;
	while (i<len_str && (str[i] == ' ' || str[i] == '\t')) i++;
	if (i == len_str) {
		fprintf (stderr, "str2e: Blank string\n");
		return;
	}
	for (j=0; i<len_str; i++,j++) {
		if (str[i] == ' ' || str[i] == '\t') break;
		string[j] = str[i];
	}
	string[j] = '\0';
	if (!time_string2epoch(string, time)) {
		fprintf (stderr, "str2e: time_string2epoch error.\n");
	}
}

void
fe2h_ (time, iyear, iday, ihour, iminute, sec)

double *time;
int *iyear;
int *iday;
int *ihour;
int *iminute;
float *sec;

{
	double second;

	e2h (*time, iyear, iday, ihour, iminute, &second);
	*sec = second;
}

void
fh2e_ (iyear, iday, ihour, iminute, sec, time)

int *iyear;
int *iday;
int *ihour;
int *iminute;
float *sec;
double *time;

{
	double second;
	double h2e();

	second = *sec;
	*time = h2e (*iyear, *iday, *ihour, *iminute, second);
}

void
fdoy2mday_ (doy, year, month, day)

int *doy;
int *year;
int *month;
int *day;

{
	doy2mday (*doy, *year, month, day);
}

int compar (seg1, seg2)

struct seg_ **seg1, **seg2;

{
	if ((*seg1)->tstart < (*seg2)->tstart) return (-1);
	else if ((*seg1)->tstart > (*seg2)->tstart) return (1);
	else return (0);
}


int compar2 (sta1, sta2)

struct sta_ *sta1, *sta2;

{
	return (strcmp(sta1->sta, sta2->sta));
}

int compar3 (chan1, chan2)

struct chan_ *chan1, *chan2;

{
	return (strcmp(chan1->chan, chan2->chan));
}


int
getstuff (in, dbl, sta, loc, chan, 
	month_start, day_start, year_start,
	hour_start, minute_start, second_start,
	month_end, day_end, year_end,
	hour_end, minute_end, second_end)

FILE *in;
DBLink *dbl;
char *sta;
char *loc;
char *chan;
int *month_start, *day_start, *year_start, *hour_start, *minute_start;
int *month_end, *day_end, *year_end, *hour_end, *minute_end;
double *second_start, *second_end;

{
	static int ituple=0;
	static int ntuple=0;
	double time, endtime;
	struct date_time dt;
	static char line[512];

	if (dbl) {
		if (ituple == 0) ntuple = DBL_get_ntuples (dbl, table);
		if (ituple >= ntuple) return (0);
		DBL_get_attrs (dbl, table, ituple,
				ATTRID_STA, sta,
				ATTRID_CHAN, chan,
				ATTRID_TIME, &time,
				ATTRID_ENDTIME, &endtime,
				NULL);
		dt.epoch = time;
		etoh (&dt);
		*month_start = dt.month;
		*day_start = dt.day;
		*year_start = dt.year - 1900;
		*hour_start = dt.hour;
		*minute_start = dt.minute;
		*second_start = dt.second;
		dt.epoch = endtime;
		etoh (&dt);
		*month_end = dt.month;
		*day_end = dt.day;
		*year_end = dt.year - 1900;
		*hour_end = dt.hour;
		*minute_end = dt.minute;
		*second_end = dt.second;
		if (!(ituple%1000)) {
			printf ("tuple %d\n", ituple);
			fflush (stdout);
		}
		ituple++;
		return (15);
	} else {
		int i;

AGAIN:		if (!fgets(line, 511, in)) return (0);
		for (i=0; i<strlen(line); i++) {if (line[i] == ' '
				|| line [i] == '\t' 
				|| line [i] == '\n') continue; else break;}
		if (i == strlen(line)) goto AGAIN;
		if (!strncmp(line, "        STAT LOC CHAN",
				strlen("        STAT LOC CHAN"))) goto AGAIN;
		return (sscanf (line, 
			" %s %s %s %d/%d/%d %d:%d:%lf %d/%d/%d %d:%d:%lf",
				sta, loc, chan, 
				month_start, day_start, year_start,
				hour_start, minute_start, second_start,
				month_end, day_end, year_end,
				hour_end, minute_end, second_end));
	}
}

/* $Id: readcov.c,v 1.1.1.1 2000-05-23 23:27:54 kent Exp $ */
