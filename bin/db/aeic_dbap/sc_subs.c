
#include <stdio.h>

#include "db.h"
#include "scv2.h"
#include "arrays.h"

typedef struct stachan_ {
	Dbptr db;
	int recstart;
	int recend;
} Stachan;

Trace *add_trace();
Trace *read_sc();

int
make_scs (db, tstart, tend, scs)

Dbptr     db;
double        tstart;
double                tend;
Tbl **                      scs;

{
	char string[1024];
	Dbptr dbi, dbsc, dbsn;
	int n;
	Tbl *fields;
	Tbl *pattern1, *pattern2;
	char sta[32], chan[32];
	char staold[32], chanold[32];
	Stachan *sc;

	*scs = NULL;
	*scs = newtbl(0);
	pattern1 = newtbl(0);
	pattern2 = newtbl(0);
	if ((*scs) == NULL) {
		clear_register (1);
		fprintf (stderr, "make_scs: newtbl() error.\n");
		return (0);
	}

        /* Subset and sort input database */
 
        db = dblookup (db, 0, "wfdisc", 0, 0);
        dbsn = dblookup (db, 0, "sensor", 0, 0);
        dbsc = dblookup (db, 0, "sitechan", 0, 0);
        sprintf (string, "( endtime >= %.5f && time <= %.5f )", tstart, tend);
        dbi = dbsubset (db, string, 0);
        dbquery (dbi, dbRECORD_COUNT, &n);
        if (n < 1) {
                fprintf (stderr, "make_scs: No data within tstart and tend.\n");
                return (0);
        }
        dbquery (dbi, dbPRIMARY_KEY, &fields);
        dbi = dbjoin (dbi, dbsn, 0, 0, 0, 0, 0);
        dbquery (dbi, dbRECORD_COUNT, &n);
        if (n < 1) {
                fprintf (stderr, "make_scs: No data in wfdisc->sensor join.\n");
                return (0);
        }
        settbl (pattern1, -1, "sensor.chanid");
        settbl (pattern2, -1, "sitechan.chanid");
        dbi = dbjoin (dbi, dbsc, &pattern1, &pattern2, 0, 0, 0);
        dbquery (dbi, dbRECORD_COUNT, &n);
        if (n < 1) {
                fprintf (stderr, "make_scs: No data in sensor->sitechan join.\n");
                return (0);
        }
        dbi = dbsort (dbi, fields, 0, 0);

        /* Go through sorted table and form station-channel structures */

	strcpy (staold, "");
	strcpy (chanold, "");
        for (dbi.record=0; dbi.record<n; dbi.record++) {
        	dbgetv (dbi, 0,	"sta", sta,
        			"chan", chan,
        			0);
		if (strcmp(sta, staold) || strcmp(chan, chanold)) {
			sc = (Stachan *) my_malloc ("make_scs: sc", sizeof(Stachan));
			if (sc == NULL) {
				fprintf (stderr, "make_scs: Malloc error.\n");
				return (0);
			}
			sc->db = dbi;
			sc->recstart = dbi.record;
			if (settbl ((*scs), -1, sc) < 0) {
				clear_register (1);
				fprintf (stderr, "make_scs: settbl() error.\n");
				return (0);
			}
		}
		strcpy (staold, sta);
		strcpy (chanold, chan);
		sc->recend = dbi.record;
        }

	/* Normal exit */

	return (1);
}

int
get_sc_stachan (sc, sta, chan)

Stachan *       sc;
char *              sta;
char *                   chan;

{
	sc->db.record = sc->recstart;
	dbgetv (sc->db, 0, "sta", sta, "chan", chan, 0);

	/* Normal exit */

	return (1);
}


Trace *
read_sc (sc, tstart, tend, hang, vang)

Stachan *sc;
double       tstart;
double               tend;
double *                   hang;
double *                         vang;

{
	Trace *trace;
	int first = 1;
	double time, endtime;

	trace = NULL;
	for (sc->db.record=sc->recstart; sc->db.record<=sc->recend; (sc->db.record)++) {
		dbgetv (sc->db, 0, "time", &time, "endtime", &endtime, 0);
		if (tstart > endtime) continue;
		if (tend < time) break;
		if (!add_trace (sc->db, tstart, tend, trace, &trace)) {
			fprintf (stderr, "read_sc: add_trace() error.\n");
			return (NULL);
		}
		if (trace && first) {
			dbgetv (sc->db, 0, "hang", hang, "vang", vang, 0);
			first = 0;
		}
	}
	if (!trace) return (NULL);
	trace = (Trace *) SCV_trace_glue (trace);
	trace = (Trace *) SCV_trace_tofloat (trace, 0);
	return (trace);
}

Trace *
get_trace_from_scs (scs, sta, chan, tstart, tend)

Tbl *               scs;
char *                   sta;
char *                        chan;
double                              tstart;
double                                      tend;

{
	Trace *trace;
	Stachan *sc;
	int i, n;
	char stas[32], chans[32];
	double hang, vang;

	n = maxtbl(scs);
	for (i=0; i<n; i++) {
		sc = (Stachan *) gettbl (scs, i);
		get_sc_stachan (sc, stas, chans);
		if (!strcmp(sta, stas) && !strcmp(chan, chans)) break;
	}
	if (i == n) return (NULL);
	trace = read_sc (sc, tstart, tend, &hang, &vang);
	return (trace);
}

/* $Id: sc_subs.c,v 1.1 2001-06-15 00:18:31 kent Exp $ */
