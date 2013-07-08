
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "db.h"
#include "coords.h"

struct arrival_ {
	char sta[8];
	char chan[8];
	char iphase[8];
	char phase[8];
	double time;
	double timeres;
	double predtime;
	double lat;
	double lon;
	double elev;
	double delta;
	double esaz;
	double seaz;
	int arid;
	int orid;
};

#define SHOW_MAGIC      (0xbcd1234)
int narrivals_show;
int show_arrivals;
struct arrivals_show_ {
        int magic;
        char sta[8];
        char chan[8];
        char phase[16];
        double time;
} *arrivals_show;


int
dbassoc (dbin, dbcat, timeres_P, timeres_S, irules, narids, arids,
	eorid, erec, dbout, nphases_out, phases_out)

char *dbin, *dbcat, *irules;
double timeres_P, timeres_S;
int narids;
int *arids;
int *eorid;
int *erec;
char *dbout;
int *nphases_out;
char ***phases_out;

{
	Dbptr dbi, dbc;
	int norigins;
	int pred=1;
	int i, j, n;
	struct arrival_ *arrivals;
	double tmin, tmax;
	double etime, elat, elon, edepth;
	int orid;
	int first=0;
	int best=1;
	char phases[64];
	char *rule;
	double meansq, meansqmin;
	int record_keep;
    	static char rules[256];
    	static char def_rules[64]="best,PS+";

/*
 *	Open input database.
 */
 	*eorid = -1;
 	*nphases_out = 0;
	if (dbopen (dbin, "r", &dbi) == dbINVALID) {
		clear_register (1);
		fprintf (stderr, "dbassoc: Unable to open input database %s.\n",
							dbin);
		return (0);
	}
/*
 *	Open catalog database.
 */
	if (dbopen (dbcat, "r", &dbc) == dbINVALID) {
		clear_register (1);
		fprintf (stderr, "dbassoc: Unable to open catalog database %s.\n",
							dbcat);
		return (0);
	}

/*
 *	Get the input arrival info.
 */
 	arrivals = (struct arrival_ *) malloc (narids*sizeof(struct arrival_));
 	if (arrivals == NULL) {
		fprintf (stderr, "dbassoc: Malloc error.\n");
		return (0);
 	}
 	if (!fill_arrivals (dbi, narids, arids, arrivals)) {
		fprintf (stderr, "dbassoc: Error in fill_arrivals().\n");
		return (0);
 	}

/*
 *	Set up the catalog database
 */
	dbc = dblookup (dbc, 0, "origin", 0, 0);
	dbquery (dbc, dbRECORD_COUNT, &norigins);
	if (norigins < 1) {
		fprintf (stderr, "dbassoc: No origin table rows in %s.\n",
							dbcat);
		return (0);
	}

/*
 *	Parse the rules.
 */
 	if (irules) strcpy (rules, irules); else strcpy (rules, def_rules);
 	for (rule=strtok(rules, ","); rule!=NULL; rule=strtok(NULL,",")) {
 		if (!rule[0]) continue;
 		if (!strcmp(rule, "first")) first=1;
 		else if (!strcmp(rule, "best")) best=1;
 		else if (!strcmp(rule, "P")) strcpy (phases, "P");
 		else if (!strcmp(rule, "S")) strcpy (phases, "S");
 		else if (!strcmp(rule, "PS")) strcpy (phases, "P,S");
 		else if (!strcmp(rule, "PS+")) strcpy (phases, "basic");
 	}

/*
 *	Loop through the catalog database.
 */
 	tmin = arrivals[0].time;
 	tmax = arrivals[0].time;
 	for (i=1; i<narids; i++) {
 		if (arrivals[i].time > tmax) tmax = arrivals[i].time;
 		if (arrivals[i].time < tmin) tmin = arrivals[i].time;
 	}
 	meansqmin = 1.e20;
 	record_keep = -1;
 	for (dbc.record=0; dbc.record<norigins; dbc.record++) {
 		dbgetv (dbc, 0,	"orid", &orid,
 				"time", &etime,
 				"lat", &elat,
 				"lon", &elon,
 				"depth", &edepth,
 				NULL);
		if (etime+3600.0 < tmin) continue;
		n = assoc_event (narids, arrivals, orid, etime, elat, elon, edepth,
					timeres_P, timeres_S, phases, first);
		if (n < 1) {
			if (etime > tmax) break;
			continue;
		}
		printf ("\nCandidate event %d with %d associations\n", orid, n);
		printf ("sta      iphase   phase        delta        seaz       esaz    timeres\n");
		meansq = 0.0;
		for (i=0; i<narids; i++) {
		    if (arrivals[i].orid < 0) {
		    	if (arrivals[i].phase[0]) {
				printf ("%-8.8s %-8.8s %-8.8s %10.3f %10.3f %10.3f (%9.3f)\n",
					arrivals[i].sta, arrivals[i].iphase, arrivals[i].phase,
					arrivals[i].delta, arrivals[i].seaz, arrivals[i].esaz,
					arrivals[i].timeres);
			} else {
				printf ("%-8.8s %-8.8s %-8.8s %10.3f %10.3f %10.3f\n",
					arrivals[i].sta, arrivals[i].iphase, arrivals[i].phase,
					arrivals[i].delta, arrivals[i].seaz, arrivals[i].esaz);
			}
		    } else {
			printf ("%-8.8s %-8.8s %-8.8s %10.3f %10.3f %10.3f %10.3f\n",
				arrivals[i].sta, arrivals[i].iphase, arrivals[i].phase,
				arrivals[i].delta, arrivals[i].seaz, arrivals[i].esaz,
				arrivals[i].timeres);
			meansq += arrivals[i].timeres*arrivals[i].timeres;
		    }
		}
		meansq /= n;
		printf ("Mean square residual = %.5f\n", meansq);
		if (meansq < meansqmin) {
			meansqmin = meansq;
			record_keep = dbc.record;
		}
		if (!best) {
			write_assoc (narids, arrivals, pred, dbout, dbc);
		}
 	}
 	if (!best) return (1);
 	if (record_keep < 0) return (1);
 	dbc.record = record_keep;
 	dbgetv (dbc, 0,	"orid", &orid,
 				"time", &etime,
 				"lat", &elat,
 				"lon", &elon,
 				"depth", &edepth,
 				NULL);
	*eorid = orid;
	*erec = dbc.record;
	n = assoc_event (narids, arrivals, orid, etime, elat, elon, edepth,
					timeres_P, timeres_S, phases, first);
	for (i=0,*nphases_out=0; i<narids; i++) {
		if (arrivals[i].orid < 0) continue;
		(*nphases_out)++;
	}
	*phases_out = (char **) malloc ((*nphases_out)*sizeof(char *));
	if (*phases_out == NULL) {
		fprintf (stderr, "dbassoc: malloc() error.\n");
		return (0);
	}
	for (i=0,j=0; i<narids; i++) {
		if (arrivals[i].orid < 0) continue;
		(*phases_out)[j] = strdup(arrivals[i].phase);
		if ((*phases_out)[j] == NULL) {
			fprintf (stderr, "dbassoc: strdup() error.\n");
			return (0);
		}
		j++;
	}
	write_assoc (narids, arrivals, pred, dbout, dbc);
/*
 *	Normal return.
 */
 	return (1);
}

int
write_assoc (narids, arrivals, pred, dbout, dbc)

int narids;
struct arrival_ *arrivals;
int pred;
char *dbout;
Dbptr dbc;

{
	static char phase[512];
	int i, n;
	Dbptr dbo;

/*
 *	write out assocs
 */
 	if (dbout) {
 		int orid;
 		double lat, lon, depth, time;

		if (dbopen (dbout, "r+", &dbo) == dbINVALID) {
			clear_register (1);
			fprintf (stderr, "write_assoc: Unable to open output database %s.\n",
								dbout);
			return (0);
		}
		dbgetv (dbc, "origin",	"lat", &lat,
					"lon", &lon,
					"time", &time,
					"depth", &depth,
					NULL);
		dbo.record = dbaddv (dbo, "origin", "lat", lat,
					"lon", lon,
					"time", time,
					"depth", depth,
					NULL);
		if (dbo.record < 0) {
			clear_register (1);
			fprintf (stderr, "write_assoc: Origin already in database.\n");
			return (0);
		}
		dbgetv (dbo, "origin",	"orid", &orid, NULL);
		dbo = dblookup (dbo, 0, "origin", 0, 0);
		dbget (dbc, phase);
		dbput (dbo, phase);
		dbputv (dbo, "origin", 	"orid", orid, NULL);
		for (i=0,n=0; i<narids; i++) {
			if (arrivals[i].orid < 0) continue;
			n++;
			dbaddv (dbo, "assoc",	"sta", arrivals[i].sta,
						"phase", arrivals[i].phase,
						"arid", arrivals[i].arid,
						"orid", orid,
						"timeres", arrivals[i].timeres,
						"delta", arrivals[i].delta,
						"seaz", arrivals[i].seaz,
						"esaz", arrivals[i].esaz,
						NULL);
		}
		dbputv (dbo, "origin",  "nass", n, NULL);
 	}
/*
 *	Do predicted arrivals.
 */
 	if (!pred) return;
	n = 0;
	for (i=0; i<narids; i++) {
		if (arrivals[i].orid < 0) continue;
		n++;
	}
        if (arrivals_show) free (arrivals_show);
        arrivals_show=NULL;
	narrivals_show=n;
	if (n < 1) return;
        arrivals_show = (struct arrivals_show_ *) malloc (narrivals_show*sizeof(struct arrivals_show_));
        if (arrivals_show == NULL) {
                narrivals_show=0;
                fprintf (stderr, "write_assoc: Malloc error.\n");
                return (0);
        }
	n = 0;
	for (i=0; i<narids; i++) {
		if (arrivals[i].orid < 0) continue;
        	sprintf (phase, "+%s", arrivals[i].phase);
		arrivals_show[n].magic = SHOW_MAGIC;
		strcpy (arrivals_show[n].sta, arrivals[i].sta);
		strcpy (arrivals_show[n].chan, arrivals[i].chan);
		strcpy (arrivals_show[n].phase, phase);
		arrivals_show[n].time = arrivals[i].predtime;
		n++;
	}
}

int 
assoc_event (nars, ars, orid, etime, elat, elon, edepth, timeres_P, timeres_S, phases, first)

int nars;
struct arrival_ *ars;
int orid;
double etime, elat, elon, edepth, timeres_P, timeres_S;
char *phases;
int first;

{
	int i, j, n;
	int nph;
	double *times;
	char **phs;
	double timeres, atimeres, atimeresmin;

	tt_taup_set_phases (phases);
	if (edepth < 1.0) edepth = 1.0;
	tt_taup_set_event_depth (edepth);
	for (i=0; i<nars; i++) {
		ars[i].orid = -1;
		strcpy (ars[i].phase, "");
		tt_taup (elat, elon, ars[i].lat, ars[i].lon, ars[i].elev,
				0.0, 0.0, &nph, &times, &phs);
		if (nph < 1) continue;
		switch (ars[i].iphase[0]) {
		case 'P':
		case 'p':
			atimeresmin = 1.e20;
			for (j=0; j<nph; j++) {
				if (phs[j][0] != 'P' && phs[j][0] != 'p') continue;
				timeres = ars[i].time - (etime + times[j]);
				atimeres = timeres>0.0?timeres:-timeres;
				if (first) {
					ars[i].predtime = etime + times[j];
					ars[i].timeres = timeres;
					strcpy (ars[i].phase, phs[j]);
					if (atimeres > timeres_P) break;
					ars[i].orid = orid;
					break;
				} else {
					if (atimeres > atimeresmin) continue;
					atimeresmin = atimeres;
					ars[i].predtime = etime + times[j];
					ars[i].timeres = timeres;
					strcpy (ars[i].phase, phs[j]);
					if (atimeres > timeres_P) continue;
					ars[i].orid = orid;
				}
			}
			break;
		case 'S':
		case 's':
			atimeresmin = 1.e20;
			for (j=0; j<nph; j++) {
				if (phs[j][0] != 'S' && phs[j][0] != 's') continue;
				timeres = ars[i].time - (etime + times[j]);
				atimeres = timeres>0.0?timeres:-timeres;
				if (first) {
					ars[i].predtime = etime + times[j];
					ars[i].timeres = timeres;
					strcpy (ars[i].phase, phs[j]);
					if (atimeres > timeres_S) break;
					ars[i].orid = orid;
					break;
				} else {
					if (atimeres > atimeresmin) continue;
					atimeresmin = atimeres;
					ars[i].predtime = etime + times[j];
					ars[i].timeres = timeres;
					strcpy (ars[i].phase, phs[j]);
					if (atimeres > timeres_S) continue;
					ars[i].orid = orid;
				}
			}
			break;
		default:
			continue;
		}
		dbpdist (elat, elon, ars[i].lat, ars[i].lon, &ars[i].delta, &ars[i].esaz);
		dbpdist (ars[i].lat, ars[i].lon, elat, elon, &ars[i].delta, &ars[i].seaz);
	}
	for (i=0,n=0; i<nars; i++) if (ars[i].orid > -1) n++;
	return (n);
}

int
dbpdist (lat1, lon1, lat2, lon2, del, az)

double lat1, lon1, lat2, lon2, *del, *az;

{
	double clat, slat, clong, slong, clat2, slat2, s;
	double x1, z1, x2, y2, z2, x, z, xpp, ypp, zpp;

	lat1 *= M_PI/180.0;
	lon1 *= M_PI/180.0;
	lat2 *= M_PI/180.0;
	lon2 *= M_PI/180.0;
	slat = sin(lat1);
	clat = cos(lat1);
	x1 = clat;
	z1 = slat;

	slat2 = sin(lat2);
	clat2 = cos(lat2);
	x2 = clat2*cos(lon2-lon1);
	y2 = clat2*sin(lon2-lon1);
	z2 = slat2;

	x = x2 - x1;
	z = z2 - z1;
	xpp = x*clat + z*slat;
	ypp = y2;
	zpp = -x*slat + z*clat;

	s = sqrt ( xpp*xpp + ypp*ypp + zpp*zpp );
	*del = 2.0 * asin ( 0.5*s );
	*az = atan2 ( ypp, zpp );
	if ((*az) < 0.0) (*az) += 2.0*M_PI;
	(*del) *= 180.0/M_PI;
	(*az) *= 180.0/M_PI;
}

int
fill_arrivals (dbi, narids, arids, arrivals)

Dbptr dbi;
int narids;
int *arids;
struct arrival_ *arrivals;

{
	int i, j, n, nn;
	int arid;
	int cont, stop;
	char sta[16];
	double lat, lon, elev;

	dbi = dblookup (dbi, 0, "arrival", 0, 0);
	dbquery (dbi, dbRECORD_COUNT, &n);
	if (n < 1) {
		fprintf (stderr, "fill_arrivals: No arrival table rows\n");
		return (0);
	}
	for (i=0; i<narids; i++) {
		arrivals[i].arid = -1;
		arrivals[i].lat = 1000.0;
	}
	nn=0;
	for (dbi.record=0; dbi.record<n; dbi.record++) {
		dbgetv (dbi, 0, "arid", &arid, NULL);
		stop=0;
		for (i=0; i<narids; i++) if (arid == arids[i]) {
			arrivals[i].arid = arid;
			dbgetv (dbi, 0,	"time", &arrivals[i].time,
					"iphase", arrivals[i].iphase,
					"sta", arrivals[i].sta,
					"chan", arrivals[i].chan,
					NULL);
			cont=0;
			for (j=0; j<narids; j++) if (arrivals[j].arid < 0) cont=1;
			if (!cont) stop=1;
			break;
		}
		if (stop) break;
	}
	stop = 0;
	for (i=0; i<narids; i++) {
		if (arrivals[i].arid < 0) {
			fprintf (stderr, "fill_arrivals: No arrival for arid = %d\n",
						arids[i]);
			stop = 1;
		}
	}
	if (stop) return (0);
	dbi = dblookup (dbi, 0, "site", 0, 0);
	dbquery (dbi, dbRECORD_COUNT, &n);
	if (n < 1) {
		fprintf (stderr, "fill_arrivals: No site table rows\n");
		return (0);
	}
	for (dbi.record=0; dbi.record<n; dbi.record++) {
		dbgetv (dbi, 0,		"sta", sta, 
					"lat", &lat,
					"lon", &lon,
					"elev", &elev,
					NULL);
		for (i=0; i<narids; i++) if (!strcmp(sta, arrivals[i].sta)) {
			arrivals[i].lat = lat;
			arrivals[i].lon = lon;
			arrivals[i].elev = elev;
		}
	}
	stop = 0;
	for (i=0; i<narids; i++) {
		if (arrivals[i].lat > 100.0) {
			fprintf (stderr, "fill_arrivals: No site for sta = %s\n",
						arrivals[i].sta);
			stop = 1;
		}
	}
	if (stop) return (0);
	return (1);
}

/* $Id: dbassoc.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
