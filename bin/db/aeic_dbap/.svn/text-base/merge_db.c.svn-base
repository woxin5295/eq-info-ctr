
/*
 *	These subroutine constitute a database merging facility
 *	that allow new candidate tuples to be merged into an
 *	existing database in an efficient manner.
 */

#include <stdio.h>
#include <string.h>

#include "db.h"
#include "arrays.h"

#define	ABS(x)	((x)<0.0?(-(x)):(x))

/*
 *	These are the "comparison" structures for each table in
 *	the database. Each structure contains the info necessary
 *	to determine if two tuples logically match within the
 *	database.
 */

struct affiliation_cmp_ {
	char sta[8];
	char net[16];
	Dbptr db;
	int ituple;
};

struct calibration_cmp_ {
	char sta[8];
	char chan[16];
	double time;
	double endtime;
	double calib;
	double calper;
	double fc;
	int stream;
	char units[16];
	Dbptr db;
	int ituple;
};

struct network_cmp_ {
	char net[16];
	char netname[96];
	char nettype[8];
	char auth[16];
	Dbptr db;
	int ituple;
};

struct site_cmp_ {
	char sta[8];
	double ontime;
	double offtime;
	double lat;
	double lon;
	double elev;
        double dnorth;
        double deast;
	Dbptr db;
	int ituple;
};

struct sitechan_cmp_ {
	char sta[8];
	char chan[16];
	double ontime;
	double offtime;
	double hang;
	double vang;
	double edepth;
	Dbptr db;
	int ituple;
	int chanid;
};

struct stage_cmp_ {
	char sta[8];
	char chan[16];
	double time;
	double endtime;
	int stageid;
	char ssident[32];
	double gnom;
	char iunits[32];
	char ounits[32];
	double gcalib;
	char gtype[32];
	int izero;
	int decifac;
	double samprate;
	double leadfac;
	char dir[128];
	char dfile[64];
	Dbptr db;
	int ituple;
};

struct instrument_cmp_ {
	char dir[128];
	char dfile[64];
	char rsptype[8];
	double samprate;
	double ncalib;
	double ncalper;
	Dbptr db;
	int ituple;
	int inid;
};

struct sensor_cmp_ {
	char sta[8];
	char chan[16];
	double time;
	double endtime;
	int inid;
	int chanid;
	double calratio;
	double calper;
	double tshift;
	Dbptr db;
	int ituple;
};

/*
 *	These are proceedures for creating the comparison
 *	structures.
 */

struct affiliation_cmp_ *create_acmp();
struct calibration_cmp_ *create_calcmp();
struct network_cmp_ *create_netcmp();
struct site_cmp_ *create_scmp();
struct sitechan_cmp_ *create_sccmp();
struct stage_cmp_ *create_stcmp();
struct instrument_cmp_ *create_icmp();
struct sensor_cmp_ *create_sncmp();

void *myaddstbl();

struct affiliation_cmp_ *
create_acmp (sta, net, db, ituple)

char *       sta;
char *            net;
Dbptr                  db;
int                        ituple;

{
	struct affiliation_cmp_ *acmp;

	acmp = (struct affiliation_cmp_ *) my_malloc ("create_acmp: acmp", sizeof(struct affiliation_cmp_));
	if (acmp == NULL) return (NULL);
	strcpy (acmp->sta, sta);
	strcpy (acmp->net, net);
	acmp->db = db;
	acmp->ituple = ituple;
	return (acmp);
}

struct calibration_cmp_ *
create_calcmp (sta, chan, time, endtime, calib, calper, fc, stream, units, db, ituple)

char *         sta;
char *              chan;
double                    time;
double                          endtime;
double                                   calib;
double                                          calper;
double                                                  fc;
int                                                         stream;
char *                                                              units;
Dbptr                                                                      db;
int                                                                            ituple;

{
	struct calibration_cmp_ *calcmp;

	calcmp = (struct calibration_cmp_ *) my_malloc ("create_calcmp: calcmp", sizeof(struct calibration_cmp_));
	if (calcmp == NULL) return (NULL);
	strcpy (calcmp->sta, sta);
	strcpy (calcmp->chan, chan);
	calcmp->time = time;
	calcmp->endtime = endtime;
	calcmp->calib = calib;
	calcmp->calper = calper;
	calcmp->fc = fc;
	calcmp->stream = stream;
	strcpy (calcmp->units, units);
	calcmp->db = db;
	calcmp->ituple = ituple;
	return (calcmp);
}

struct network_cmp_ *
create_netcmp (net, netname, nettype, auth, db, ituple)

char *         net;
char *              netname;
char *                       nettype;
char *                                auth;
Dbptr                                       db;
int                                             ituple;

{
	struct network_cmp_ *netcmp;

	netcmp = (struct network_cmp_ *) my_malloc ("create_netcmp: netcmp", sizeof(struct network_cmp_));
	if (netcmp == NULL) return (NULL);
	strcpy (netcmp->net, net);
	strcpy (netcmp->netname, netname);
	strcpy (netcmp->nettype, nettype);
	strcpy (netcmp->auth, auth);
	netcmp->db = db;
	netcmp->ituple = ituple;
	return (netcmp);
}

struct site_cmp_ *
create_scmp (sta, ontime, offtime, lat, lon, elev, dnorth, deast, db, ituple)

char *       sta;
double            ontime;
double                    offtime;
double                             lat;
double                                  lon;
double                                       elev;
double                             dnorth;
double                                  deast;
Dbptr                                              db;
int                                                    ituple;

{
	struct site_cmp_ *scmp;

	scmp = (struct site_cmp_ *) my_malloc ("create_scmp: scmp", sizeof(struct site_cmp_));
	if (scmp == NULL) return (NULL);
	strcpy (scmp->sta, sta);
	scmp->ontime = ontime;
	scmp->offtime = offtime;
	scmp->lat = lat;
	scmp->lon = lon;
	scmp->elev = elev;
	scmp->dnorth = dnorth;
	scmp->deast = deast;
	scmp->db = db;
	scmp->ituple = ituple;
	return (scmp);
}

struct sitechan_cmp_ *
create_sccmp (sta, chan, ontime, offtime, hang, vang, edepth, db, ituple, chanid)

char *       sta;
char *             chan;
double                   ontime;
double                           offtime;
double                                    hang;
double                                         vang;
double                                              edepth;
Dbptr                                                     db;
int                                                           ituple;
int                                                                      chanid;

{
	struct sitechan_cmp_ *sccmp;

	sccmp = (struct sitechan_cmp_ *) my_malloc ("create_sccmp: sccmp", sizeof(struct sitechan_cmp_));
	if (sccmp == NULL) return (NULL);
	strcpy (sccmp->sta, sta);
	strcpy (sccmp->chan, chan);
	sccmp->ontime = ontime;
	sccmp->offtime = offtime;
	sccmp->hang = hang;
	sccmp->vang = vang;
	sccmp->edepth = edepth;
	sccmp->db = db;
	sccmp->ituple = ituple;
	sccmp->chanid = chanid;
	return (sccmp);
}

struct stage_cmp_ *
create_stcmp (sta, chan, time, endtime, stageid, ssident, gnom, iunits, ounits,
			gcalib, gtype, izero, decifac, samprate, leadfac, dir, dfile, db, ituple)

char *         sta;
char *              chan;
double                    time;
double                          endtime;
int                                     stageid;
char *                                           ssident;
double                                                    gnom;
char *                                                          iunits;
char *                                                                  ounits;
double                 gcalib;
char *                         gtype;
int                                   izero;
int                                          decifac;
double                                                samprate;
double                                                          leadfac;
char *                                                                   dir;
char *                                                                        dfile;
Dbptr                                                                                db;
int                                                                                      ituple;

{
	struct stage_cmp_ *stcmp;

	stcmp = (struct stage_cmp_ *) my_malloc ("create_stcmp: stcmp", sizeof(struct stage_cmp_));
	if (stcmp == NULL) return (NULL);
	strcpy (stcmp->sta, sta);
	strcpy (stcmp->chan, chan);
	stcmp->time = time;
	stcmp->endtime = endtime;
	stcmp->stageid = stageid;
	strcpy (stcmp->ssident, ssident);
	stcmp->gnom = gnom;
	strcpy (stcmp->iunits, iunits);
	strcpy (stcmp->ounits, ounits);
	stcmp->gcalib = gcalib;
	strcpy (stcmp->gtype, gtype);
	stcmp->izero = izero;
	stcmp->decifac = decifac;
	stcmp->samprate = samprate;
	stcmp->leadfac = leadfac;
	strcpy (stcmp->dir, dir);
	strcpy (stcmp->dfile, dfile);
	stcmp->db = db;
	stcmp->ituple = ituple;
	return (stcmp);
}

struct instrument_cmp_ *
create_icmp (dir, dfile, rsptype, samprate, ncalib, ncalper, db, ituple, inid)

char *       dir;
char *            dfile;
char *                   rsptype;
double                            samprate;
double                                      ncalib;
double                                              ncalper;
Dbptr                                                        db;
int                                                              ituple;
int                                                                      inid;

{
	struct instrument_cmp_ *icmp;

	icmp = (struct instrument_cmp_ *) my_malloc ("create_icmp: icmp", sizeof(struct instrument_cmp_));
	if (icmp == NULL) return (NULL);
	strcpy (icmp->dir, dir);
	strcpy (icmp->dfile, dfile);
	strcpy (icmp->rsptype, rsptype);
	icmp->samprate = samprate;
	icmp->ncalib = ncalib;
	icmp->ncalper = ncalper;
	icmp->db = db;
	icmp->ituple = ituple;
	icmp->inid = inid;
	return (icmp);
}

struct sensor_cmp_ *
create_sncmp (sta, chan, time, endtime, inid, chanid, calratio, calper, tshift, db, ituple)

char *       sta;
char *             chan;
double                   time;
double                         endtime;
int                                     inid;
int                                          chanid;
double                                               calratio;
double                                                         calper;
double                                                                  tshift;
Dbptr                                                                          db;
int                                                                                ituple;

{
	struct sensor_cmp_ *sncmp;

	sncmp = (struct sensor_cmp_ *) my_malloc ("create_sncmp: sncmp", sizeof(struct sensor_cmp_));
	if (sncmp == NULL) return (NULL);
	strcpy (sncmp->sta, sta);
	strcpy (sncmp->chan, chan);
	sncmp->time = time;
	sncmp->endtime = endtime;
	sncmp->inid = inid;
	sncmp->chanid = chanid;
	sncmp->calratio = calratio;
	sncmp->calper = calper;
	sncmp->tshift = tshift;
	sncmp->db = db;
	sncmp->ituple = ituple;
	return (sncmp);
}

/*
 *	These are the comparison proceedures for each table.
 */

int
compare_affiliation (s1, s2)

struct affiliation_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->net, s2->net);
	if (i) return (i);
	return(strcmp(s1->sta, s2->sta));
}

int
compare_calibration (s1, s2)

struct calibration_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	i = strcmp(s1->chan, s2->chan);
	if (i) return (i);
	if (s1->endtime < s2->time-1.0) return (-1);
	if (s1->time > s2->endtime+1.0) return (1);
	if (ABS(s1->time-s2->endtime) < 1.0) 
		if (s1->stream != s2->stream 
			|| strcmp(s1->units, s2->units)
			|| !match(s1->calib,s2->calib,6) 
			|| !match(s1->calper,s2->calper,6)
			|| !match(s1->fc,s2->fc,6)) return (1);
	if (ABS(s2->time-s1->endtime) < 1.0) 
		if (s1->stream != s2->stream 
			|| strcmp(s1->units, s2->units)
			|| !match(s1->calib,s2->calib,6) 
			|| !match(s1->calper,s2->calper,6)
			|| !match(s1->fc,s2->fc,6)) return (-1);
	return (0);
}

int
compare_network (s1, s2)

struct network_cmp_ *s1, *s2;

{
	return(strcmp(s1->net, s2->net));
}

int
compare_site (s1, s2)

struct site_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	if (s1->offtime < s2->ontime-1.0) return (-1);
	if (s1->ontime > s2->offtime+1.0) return (1);
	if (ABS(s1->ontime-s2->offtime) < 1.0) 
		if (!match(s1->lat,s2->lat,4) || !match(s1->lon,s2->lon,4) || 
                    !match(s1->elev,s2->elev,4) || !match(s1->dnorth,s2->dnorth,4) || 
                    !match(s1->deast,s2->deast,4)) return (1);
	if (ABS(s2->ontime-s1->offtime) < 1.0) 
		if (!match(s1->lat,s2->lat,4) || !match(s1->lon,s2->lon,4) || 
                    !match(s1->elev,s2->elev,4) || !match(s1->dnorth,s2->dnorth,4) ||
                    !match(s1->deast,s2->deast,4)) return (-1);
	return (0);
}

int
compare_sitechan (s1, s2)

struct sitechan_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	i = strcmp(s1->chan, s2->chan);
	if (i) return (i);
	if (s1->offtime < s2->ontime-1.0) return (-1);
	if (s1->ontime > s2->offtime+1.0) return (1);
	if (ABS(s1->ontime-s2->offtime) < 1.0) 
		if (!match(s1->hang,s2->hang,1) || !match(s1->vang,s2->vang,1) 
				|| !match(s1->edepth,s2->edepth,4)) return (1);
	if (ABS(s2->ontime-s1->offtime) < 1.0) 
		if (!match(s1->hang,s2->hang,1) || !match(s1->vang,s2->vang,1) 
				|| !match(s1->edepth,s2->edepth,4)) return (-1);
	return (0);
}

int
compare_stage (s1, s2)

struct stage_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	i = strcmp(s1->chan, s2->chan);
	if (i) return (i);
	i = s1->stageid - s2->stageid;
	if (i) return (i);
	if (s1->endtime < s2->time-1.0) return (-1);
	if (s1->time > s2->endtime+1.0) return (1);
	if (ABS(s1->time-s2->endtime) < 1.0) 
		if (s1->izero != s2->izero
			|| s1->decifac != s2->decifac
			|| strcmp(s1->ssident, s2->ssident)
			|| strcmp(s1->iunits, s2->iunits)
			|| strcmp(s1->ounits, s2->ounits)
			|| strcmp(s1->gtype, s2->gtype)
			|| strcmp(s1->dir, s2->dir)
			|| strcmp(s1->dfile, s2->dfile)
			|| !match(s1->gnom,s2->gnom,6) 
			|| !match(s1->gcalib,s2->gcalib,6) 
			|| !match(s1->samprate,s2->samprate,7) 
			|| !match(s1->leadfac,s2->leadfac,7)) return (1);
	if (ABS(s2->time-s1->endtime) < 1.0) 
		if (s1->izero != s2->izero
			|| s1->decifac != s2->decifac
			|| strcmp(s1->ssident, s2->ssident)
			|| strcmp(s1->iunits, s2->iunits)
			|| strcmp(s1->ounits, s2->ounits)
			|| strcmp(s1->gtype, s2->gtype)
			|| strcmp(s1->dir, s2->dir)
			|| strcmp(s1->dfile, s2->dfile)
			|| !match(s1->gnom,s2->gnom,6) 
			|| !match(s1->gcalib,s2->gcalib,6) 
			|| !match(s1->samprate,s2->samprate,7) 
			|| !match(s1->leadfac,s2->leadfac,7)) return (-1);
	return (0);
}

int
compare_instrument (s1, s2)

struct instrument_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->dfile, s2->dfile);
	if (i) return (i);
	i = strcmp(s1->dir, s2->dir);
	if (i) return (i);
	i = strcmp(s1->rsptype, s2->rsptype);
	if (i) return (i);
	i = dbldif(s1->samprate,s2->samprate,7);
	if (i) return (i);
	i = dbldif(s1->ncalib,s2->ncalib,6);
	if (i) return (i);
	i = dbldif(s1->ncalper,s2->ncalper,6);
	if (i) return (i);
	return (0);
}

int
compare_sensor (s1, s2)

struct sensor_cmp_ *s1, *s2;

{
	int i;

	i = strcmp(s1->sta, s2->sta);
	if (i) return (i);
	i = strcmp(s1->chan, s2->chan);
	if (i) return (i);
	if (s1->endtime < s2->time-1.0) return (-1);
	if (s1->time > s2->endtime+1.0) return (1);
	if (ABS(s1->time-s2->endtime) < 1.0) 
		if (s1->inid != s2->inid || s1->chanid != s2->chanid
			|| !match(s1->calratio,s2->calratio,6) || !match(s1->calper,s2->calper,6) 
				|| !match(s1->tshift,s2->tshift,2)) return (1);
	if (ABS(s2->time-s1->endtime) < 1.0) 
		if (s1->inid != s2->inid || s1->chanid != s2->chanid
			|| !match(s1->calratio,s2->calratio,6) || !match(s1->calper,s2->calper,6) 
				|| !match(s1->tshift,s2->tshift,2)) return (-1);
	return (0);
}

int
match (dbl1, dbl2, n)
 
double dbl1, dbl2;
int n;
 
{
	double t;
	int i;
 
        for (i=0,t=0.5; i<n; i++) t *= 0.1;
        if (dbl1 > dbl2-t && dbl1 < dbl2+t) return (1);
        return (0);
}

int
dbldif (dbl1, dbl2, n)
 
double dbl1, dbl2;
int n;
 
{
        double t;
        int i;
 
        for (i=0,t=0.5; i<n; i++) t *= 0.1;
        if (dbl1 < dbl2-t) return (-1);
        if (dbl1 > dbl2+t) return (1);
        return (0);
}

/*
 *	These are the merging subroutines for each table.
 */

int
merge_affiliation (db, sta, net)

Dbptr           db;
char *              sta;
char *                    net;

{
	Dbvalue value;

	static Stbl *affiliation_stbl=NULL;
	static int ok_write;
	struct affiliation_cmp_ *acmp;
	struct affiliation_cmp_ *acmp2;
	char *ptr;

	int compare_affiliation();

/*
 *	Create affiliation sorted table
 */
	if (affiliation_stbl == NULL) {
		int n;

		affiliation_stbl = newstbl (compare_affiliation);
		if (affiliation_stbl == NULL) {
			fprintf (stderr, "merge_affiliation: Error creating affiliation sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "affiliation", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], neti[32];

			dbgetv (db, 0,	"sta", stai,
					"net", neti,
					NULL);
			if (!merge_affiliation (db, stai, neti)) {
				fprintf (stderr, "merge_affiliation: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing affiliation table.
 */
        db = dblookup (db, 0, "affiliation", 0, 0);
	acmp = create_acmp (sta, net, db, -1);
	if (acmp == NULL) {
		fprintf (stderr, "merge_affiliation: Malloc error.\n");
		return (0);
	}
/*
 *	Add affiliation compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (affiliation_stbl, acmp, compare_affiliation);
	if (ptr == NULL) {
		fprintf (stderr, "merge_affiliation: Error return from addstbl.\n");
		return (0);
	}
	acmp2 = (struct affiliation_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (acmp != acmp2) {
		/*if (strcmp(acmp->net, acmp2->net)) {
			fprintf (stderr, 
		"merge_affiliation: sta match with mismatch in net.\n");
			my_free (acmp);
			return (0);
		}*/
		my_free (acmp);
		return (1);
	}
/*
 *	No match in sorted table.
 *	Add new affiliation tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		acmp->ituple = value.i-1;
 		return (1);
	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"net", net,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	acmp->ituple = value.i-1;
	return (1);
}

int
merge_calibration (db, sta, chan, time, endtime, stream, calib, calper,
		   fc, units)

Dbptr              db;
char *                 sta;
char *                        chan;
double                              time;
double                                   endtime;
int                                              stream;
double                                                   calib;
double                                                          calper;
double             fc;
char *                 units;

{
	int i, n;
	Dbvalue value;

	static Stbl *calibration_stbl=NULL;
	static int ok_write;
	struct calibration_cmp_ *calcmp;
	struct calibration_cmp_ *calcmp2;
	char *ptr;

	int compare_calibration();

/*
 *	Create calibration sorted table
 */
	if (calibration_stbl == NULL) {
		calibration_stbl = newstbl (compare_calibration);
		if (calibration_stbl == NULL) {
			fprintf (stderr, "merge_calibration: Error creating calibration sorted table.\n");
			return (0);
		}
	}
/*
 *	Look for match in existing calibration table.
 */
        db = dblookup (db, 0, "calibration", 0, 0);
	calcmp = create_calcmp (sta, chan, time, endtime, calib, calper, fc, stream, units, db, -1, -1);
	if (calcmp == NULL) {
		fprintf (stderr, "merge_calibration: Malloc error.\n");
		return (0);
	}
/*
 *	Add calibration compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (calibration_stbl, calcmp, compare_calibration);
	if (ptr == NULL) {
		fprintf (stderr, "merge_calibration: Error return from addstbl.\n");
		return (0);
	}
	calcmp2 = (struct calibration_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (calcmp != calcmp2) {
		if (calcmp->time >= calcmp2->time-1.0 && calcmp->endtime <= calcmp2->endtime+1.0) {
			if (calcmp->stream != calcmp2->stream) {
				fprintf (stderr, 
		"merge_calibration: sta-chan-time match %s %s %f with mismatch in stream %d %d.\n",
					sta, chan, time, calcmp->stream, calcmp2->stream);
				my_free (calcmp);
				return (0);
			}
			if (!match(calcmp->calib,calcmp2->calib,6)) {
				fprintf (stderr, 
		"merge_calibration: sta-chan-time match %s %s %f with mismatch in calib %.6f %.6f.\n",
					sta, chan, time, calcmp->calib, calcmp2->calib);
				my_free (calcmp);
				return (0);
			}
			if (!match(calcmp->calper,calcmp2->calper,6)) {
				fprintf (stderr, 
		"merge_calibration: sta-chan-time match %s %s %f with mismatch in calper %.6f %.6f.\n",
					sta, chan, time, calcmp->calper, calcmp2->calper);
				my_free (calcmp);
				return (0);
			}
			if (!match(calcmp->fc,calcmp2->fc,6)) {
				fprintf (stderr, 
		"merge_calibration: sta-chan-time match %s %s %f with mismatch in fc %.6f %.6f.\n",
					sta, chan, time, calcmp->fc, calcmp2->fc);
				my_free (calcmp);
				return (0);
			}
			if (strcmp(calcmp->units, calcmp2->units)) {
				fprintf (stderr, 
		"merge_calibration: sta-chan-time match %s %s %f with mismatch in units %s %s.\n",
					sta, chan, time, calcmp->units, calcmp2->units);
				my_free (calcmp);
				return (0);
			}
			my_free (calcmp);
			return (1);
		}
		if (calcmp->time >= calcmp2->time && calcmp->time+1.0 < calcmp2->endtime) {
			fprintf (stderr, "merge_calibration: %.3f overlap in calibration times for %s %s.\n",
							calcmp2->endtime-calcmp->time, sta, chan);
			my_free (calcmp);
			return (0);
		}
		if (calcmp->endtime > calcmp2->time+1.0 && calcmp->endtime <= calcmp2->endtime) {
			fprintf (stderr, "merge_calibration: %.3f overlap in calibration times for %s %s.\n",
							calcmp->endtime-calcmp2->time, sta, chan);
			my_free (calcmp);
			return (0);
		}
		if (calcmp->time < calcmp2->time && calcmp->endtime > calcmp2->endtime) {
			fprintf (stderr, "merge_calibration: %.3f overlap in calibration times for %s %s.\n",
							calcmp2->endtime-calcmp2->time, sta, chan);
			my_free (calcmp);
			return (0);
		}
		if (ABS(calcmp->time-calcmp2->endtime) < 1.0) {
			if (match(calcmp->calib,calcmp2->calib,6) && match(calcmp->calper,calcmp2->calper,6) 
					&& match(calcmp->fc,calcmp2->fc,6)
					&& calcmp->stream == calcmp2->stream
					&& !strcmp(calcmp->units, calcmp2->units)) {
				calcmp2->endtime = endtime;
				calcmp2->db.record = calcmp2->ituple;
				dbputv (calcmp2->db, 0,
					"endtime", endtime,
					NULL);
				my_free (calcmp);
				return (1);
			}
		}
		if (ABS(calcmp->endtime-calcmp2->time) < 1.0) {
			if (match(calcmp->calib,calcmp2->calib,6) && match(calcmp->calper,calcmp2->calper,6) 
					&& match(calcmp->fc,calcmp2->fc,6)
					&& calcmp->stream == calcmp2->stream
					&& !strcmp(calcmp->units, calcmp2->units)) {
				calcmp2->time = time;
				calcmp2->db.record = calcmp2->ituple;
				dbputv (calcmp2->db, 0,
					"time", time,
					NULL);
				my_free (calcmp);
				return (1);
			}
		}
	}

/*
 *	No match in sorted table.
 *	Add new calibration tuple.
 */
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"chan", chan,
		"time", time,
		"endtime", endtime,
		"stream", stream,
		"calib", calib,
		"calper", calper,
		"fc", fc,
		"units", units,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	calcmp->ituple = value.i-1;
	return (1);
}

int
merge_network (db, net, netname, nettype, auth)

Dbptr          db;
char *             net;
char *                  netname;
char *                           nettype;
char *                                    auth;

{
	Dbvalue value;

	static Stbl *network_stbl=NULL;
	static int ok_write;
	struct network_cmp_ *ncmp;
	struct network_cmp_ *ncmp2;
	char *ptr;

	int compare_network();

/*
 *	Create network sorted table
 */
	if (network_stbl == NULL) {
		network_stbl = newstbl (compare_network);
		if (network_stbl == NULL) {
			fprintf (stderr, "merge_network: Error creating network sorted table.\n");
			return (0);
		}
	}
/*
 *	Look for match in existing network table.
 */
        db = dblookup (db, 0, "network", 0, 0);
	ncmp = create_netcmp (net, netname, nettype, auth, db, -1);
	if (ncmp == NULL) {
		fprintf (stderr, "merge_network: Malloc error.\n");
		return (0);
	}
/*
 *	Add network compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (network_stbl, ncmp, compare_network);
	if (ptr == NULL) {
		fprintf (stderr, "merge_network: Error return from addstbl.\n");
		return (0);
	}
	ncmp2 = (struct network_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (ncmp != ncmp2) {
		if (strcmp(ncmp->net, ncmp2->net)) {
			fprintf (stderr, 
		"merge_network: sta match with mismatch in net.\n");
			my_free (ncmp);
			return (0);
		}
		my_free (ncmp);
		return (1);
	}
/*
 *	No match in sorted table.
 *	Add new network tuple.
 */
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"net", net,
		"netname", netname,
		"nettype", nettype,
		"auth", auth,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	ncmp->ituple = value.i-1;
	return (1);
}

int
merge_site (db, sta, ontime, offtime, lat, lon, elev, staname, statype,
		refsta, dnorth, deast)

Dbptr     db;
char *        sta;
double             ontime;
double                     offtime;
double                              lat;
double                                   lon;
double                                        elev;
char *                                              staname;
char *                                                       statype;
char *         refsta;
double                  dnorth;
double                          deast;

{
	int i, n;
	Dbvalue value;
	double offtim;
	int ondate, offdate;

	static Stbl *site_stbl=NULL;
	static int ok_write;
	struct site_cmp_ *scmp;
	struct site_cmp_ *scmp2;
	char *ptr;

	int compare_site();

/*
 *	Create site sorted table
 */

	if (site_stbl == NULL) {
		int n;

		site_stbl = newstbl (compare_site);
		if (site_stbl == NULL) {
			fprintf (stderr, "merge_site: Error creating site sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "site", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], stanamei[64], statypei[8], refstai[32];
			double lati, loni, elevi, dnorthi, deasti;
			int ondatei, offdatei;
			double ontimei, offtimei;

			dbgetv (db, 0,	"sta", stai,
					"ondate", &ondatei,
					"offdate", &offdatei,
					"lat", &lati,
					"lon", &loni,
					"elev", &elevi,
					"staname", stanamei,
					"statype", statypei,
					"refsta", refstai,
					"dnorth", &dnorthi,
					"deast", &deasti,
					NULL);
			ontimei = epoch (ondatei);
			if (offdatei == -1) {
				offtimei = 999999999999.999;
			} else {
				offtimei = epoch (offdatei);
			}
			if (!merge_site (db, stai, ontimei, offtimei, lati, loni, elevi, stanamei, statypei,
										refstai, dnorthi, deasti)) {
				fprintf (stderr, "merge_site: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing site table.
 */
        db = dblookup (db, 0, "site", 0, 0);
	if (offtime < -9999999999.0 || offtime > 9999999999.0 ) {
		offdate = -1;
		offtim = 9999999999.999;
	} else {
		offdate = yearday(offtime - 1.0);
		offtim = offtime;
	}
	ondate = yearday(ontime + 1.0);
	scmp = create_scmp (sta, ontime, offtim, lat, lon, elev, dnorth, deast, db, -1);
	if (scmp == NULL) {
		fprintf (stderr, "merge_site: Malloc error.\n");
		return (0);
	}
/*
 *	Add site compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (site_stbl, scmp, compare_site);
	if (ptr == NULL) {
		fprintf (stderr, "merge_site: Error return from addstbl.\n");
		return (0);
	}
	scmp2 = (struct site_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (scmp != scmp2) {
		if (scmp->ontime >= scmp2->ontime-1.0 && scmp->offtime <= scmp2->offtime+1.0) {
			if (!match(scmp->lat,scmp2->lat,4)) {
				fprintf (stderr, 
		"merge_site: sta-ontime-offtime match with mismatch in lat.\n");
				my_free (scmp);
				return (0);
			}
			if (!match(scmp->lon,scmp2->lon,4)) {
				fprintf (stderr, 
		"merge_site: sta-ontime-offtime match with mismatch in lon.\n");
				my_free (scmp);
				return (0);
			}
			if (!match(scmp->elev,scmp2->elev,4)) {
				fprintf (stderr, 
		"merge_site: sta-ontime-offtime match with mismatch in elev.\n");
				my_free (scmp);
				return (0);
			}
			if (!match(scmp->dnorth,scmp2->dnorth,4)) {
				fprintf (stderr, 
		"merge_site: sta-ontime-offtime match with mismatch in dnorth.\n");
				my_free (scmp);
				return (0);
			}
			if (!match(scmp->deast,scmp2->deast,4)) {
				fprintf (stderr, 
		"merge_site: sta-ontime-offtime match with mismatch in deast.\n");
				my_free (scmp);
				return (0);
			}
			my_free (scmp);
			return (1);
		}
		if (scmp->ontime >= scmp2->ontime && scmp->ontime+1.0 < scmp2->offtime) {
			fprintf (stderr, "merge_site: overlap in site times.\n");
			my_free (scmp);
			return (0);
		}
		if (scmp->offtime > scmp2->ontime+1.0 && scmp->offtime <= scmp2->offtime) {
			fprintf (stderr, "merge_site: overlap in site times.\n");
			my_free (scmp);
			return (0);
		}
		if (scmp->ontime < scmp2->ontime && scmp->offtime > scmp2->offtime) {
			fprintf (stderr, "merge_site: overlap in site times.\n");
			my_free (scmp);
			return (0);
		}
		if (ABS(scmp->ontime-scmp2->offtime) < 1.0) {
		     if (match(scmp->lat,scmp2->lat,4) && match(scmp->lon,scmp2->lon,4) && 
                        match(scmp->elev,scmp2->elev,4) && match(scmp->dnorth,scmp2->dnorth,4) &&
                        match(scmp->deast,scmp2->deast,4)) {
				scmp2->offtime = offtim;
				scmp2->db.record = scmp2->ituple;
				dbputv (scmp2->db, 0,
					"offdate", offdate,
					NULL);
				my_free (scmp);
				return (1);
			}
		}
		if (ABS(scmp->offtime-scmp2->ontime) < 1.0) {
			if (match(scmp->lat,scmp2->lat,4) && match(scmp->lon,scmp2->lon,4) 
			&& match(scmp->elev,scmp2->elev,4) && match(scmp->dnorth,scmp2->dnorth,4)
                        && match(scmp->deast,scmp2->deast,4)) {
				scmp2->ontime = ontime;
				scmp2->db.record = scmp2->ituple;
				ondate = yearday(ontime + 1.0);
				dbputv (scmp2->db, 0,
					"ondate", ondate,
					NULL);
				my_free (scmp);
				return (1);
			}
		}
	}
/*
 *	No match in sorted table.
 *	Add new site tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		scmp->ituple = value.i-1;
 		return (1);
	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"ondate", ondate,
		"offdate", offdate,
		"lat", lat,
		"lon", lon,
		"elev", elev,
		"dnorth", dnorth,
		"deast", deast,
		"staname", staname,
		"statype", statype,
		"refsta", refsta,
		"dnorth", dnorth,
		"deast", deast,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	scmp->ituple = value.i-1;
	return (1);
}

int
merge_sitechan (db, sta, chan, ontime, offtime, ctype, edepth, hang, vang,
		descrip)

Dbptr     db;
char *        sta;
char *                 chan;
double                       ontime;
double                               offtime;
char *                                        ctype;
double                                               edepth;
double                                                       hang;
double                                                             vang;
char *          descrip;

{
	int i, n;
	Dbvalue value;
	static int ok_write;
	double offtim;
	int chanid;
	static int chanidi;
	int ondate, offdate;

	static Stbl *sitechan_stbl=NULL;
	struct sitechan_cmp_ *sccmp;
	struct sitechan_cmp_ *sccmp2;
	char *ptr;

	int compare_sitechan();

/*
 *	Create sitechan sorted table
 */
	if (sitechan_stbl == NULL) {
		int n;

		sitechan_stbl = newstbl (compare_sitechan);
		if (sitechan_stbl == NULL) {
			fprintf (stderr, "merge_sitechan: Error creating sitechan sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "sitechan", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], chani[32], ctypei[8], descripi[64];
			double edepthi, hangi, vangi;
			int ondatei, offdatei;
			double ontimei, offtimei;

			dbgetv (db, 0,	"sta", stai,
					"chan", chani,
					"ondate", &ondatei,
					"offdate", &offdatei,
					"ctype", ctypei,
					"edepth", &edepthi,
					"hang", &hangi,
					"vang", &vangi,
					"descrip", descripi,
					"chanid", &chanidi,
					NULL);
			ontimei = epoch (ondatei);
			if (offdatei == -1) {
				offtimei = 999999999999.999;
			} else {
				offtimei = epoch (offdatei);
			}
			if (!merge_sitechan (db, stai, chani, ontimei, offtimei, ctypei, edepthi, hangi, vangi,
			                descripi)) {
				fprintf (stderr, "merge_sitechan: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing sitechan table.
 */
        db = dblookup (db, 0, "sitechan", 0, 0);
	if (offtime < -9999999999.0 || offtime > 9999999999.0 ) {
		offdate = -1;
		offtim = 9999999999.999;
	} else {
		offdate = yearday(offtime - 1.0);
		offtim = offtime;
	}
	ondate = yearday(ontime + 1.0);
	sccmp = create_sccmp (sta, chan, ontime, offtim, hang, vang, edepth, db, -1, -1);
	if (sccmp == NULL) {
		fprintf (stderr, "merge_sitechan: Malloc error.\n");
		return (0);
	}
/*
 *	Add sitechan compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (sitechan_stbl, sccmp, compare_sitechan);
	if (ptr == NULL) {
		fprintf (stderr, "merge_sitechan: Error return from addstbl.\n");
		return (0);
	}
	sccmp2 = (struct sitechan_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (sccmp != sccmp2) {
		if (sccmp->ontime >= sccmp2->ontime-1.0 && sccmp->offtime <= sccmp2->offtime+1.0) {
			if (!match(sccmp->hang,sccmp2->hang,1)) {
				fprintf (stderr, 
		"merge_sitechan: sta-ontime-offtime match with mismatch in hang.\n");
				my_free (sccmp);
				return (0);
			}
			if (!match(sccmp->vang,sccmp2->vang,1)) {
				fprintf (stderr, 
		"merge_sitechan: sta-ontime-offtime match with mismatch in vang.\n");
				my_free (sccmp);
				return (0);
			}
			if (!match(sccmp->edepth,sccmp2->edepth,4)) {
				fprintf (stderr, 
		"merge_sitechan: sta-ontime-offtime match with mismatch in edepth.\n");
				my_free (sccmp);
				return (0);
			}
			my_free (sccmp);
			return (sccmp2->chanid);
		}
		if (sccmp->ontime >= sccmp2->ontime && sccmp->ontime+1.0 < sccmp2->offtime) {
			fprintf (stderr, "merge_sitechan: %d overlap in site dates for %s %s new: %d-%d old: %d-%d.\n",
							sccmp2->offtime-sccmp->ontime, sta, chan, 
							sccmp->ontime, sccmp->offtime, sccmp2->ontime, sccmp2->offtime);
			my_free (sccmp);
			return (0);
		}
		if (sccmp->offtime > sccmp2->ontime+1.0 && sccmp->offtime <= sccmp2->offtime) {
			fprintf (stderr, "merge_sitechan: %d overlap in site dates for %s %s new: %d-%d old: %d-%d.\n",
							sccmp->offtime-sccmp2->ontime, sta, chan, 
							sccmp->ontime, sccmp->offtime, sccmp2->ontime, sccmp2->offtime);
			my_free (sccmp);
			return (0);
		}
		if (sccmp->ontime < sccmp2->ontime && sccmp->offtime > sccmp2->offtime) {
			fprintf (stderr, "merge_sitechan: overlap in site times.\n");
			my_free (sccmp);
			return (0);
		}
		if (ABS(sccmp->ontime-sccmp2->offtime) < 1.0) {
			if (match(sccmp->hang,sccmp2->hang,1) && match(sccmp->vang,sccmp2->vang,1) 
							&& match(sccmp->edepth,sccmp2->edepth,4)) {
				sccmp2->offtime = offtim;
				sccmp2->db.record = sccmp2->ituple;
				dbputv (sccmp2->db, 0,
					"offdate", offdate,
					NULL);
				my_free (sccmp);
				return (sccmp2->chanid);
			}
		}
		if (ABS(sccmp->offtime-sccmp2->ontime) < 1.0) {
			if (match(sccmp->hang,sccmp2->hang,1) && match(sccmp->vang,sccmp2->vang,1) 
							&& match(sccmp->edepth,sccmp2->edepth,4)) {
				sccmp2->ontime = ontime;
				sccmp2->db.record = sccmp2->ituple;
				dbputv (sccmp2->db, 0,
					"ondate", ondate,
					NULL);
				my_free (sccmp);
				return (sccmp2->chanid);
			}
		}
	}
/*
 *	No match in sorted table.
 *	Add new sitechan tuple.
 */

	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		sccmp->ituple = value.i-1;
		sccmp->chanid = chanidi;
		return (chanidi);
	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	chanid = dbnextid(db, "chanid");
	dbputv (db, 0,
		"sta", sta,
		"chan", chan,
		"ondate", ondate,
		"chanid", chanid,
		"offdate", offdate,
		"ctype", ctype,
		"edepth", edepth,
		"hang", hang,
		"vang", vang,
		"descrip", descrip,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	sccmp->ituple = value.i-1;
	sccmp->chanid = chanid;
	return (chanid);
}

int
merge_sensor (db, sta, chan, time, endtime, inid, chanid, jdate,
		calratio, calper, tshift, instant)

Dbptr     db;
char *        sta;
char *               chan;
double                     time;
double                          endtime;
int                                      inid;
int                                            chanid;
int                                                    jdate;
double          calratio;
double                    calper;
double                             tshift;
char *                                   instant;

{
	int i, n;
	Dbvalue value;
	static int ok_write;

	static Stbl *sensor_stbl=NULL;
	struct sensor_cmp_ *sncmp;
	struct sensor_cmp_ *sncmp2;
	char *ptr;

	int compare_sensor();

/*
 *	Create sensor sorted table
 */
	if (sensor_stbl == NULL) {
		int n;

		sensor_stbl = newstbl (compare_sensor);
		if (sensor_stbl == NULL) {
			fprintf (stderr, "merge_sensor: Error creating sensor sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "sensor", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char stai[32], chani[32], instanti[8];
			double timei, endtimei, calratioi, calperi, tshifti;
			int inidi, chanidi, jdatei;

			dbgetv (db, 0,	"sta", stai,
					"chan", chani,
					"time", &timei,
					"endtime", &endtimei,
					"inid", &inidi,
					"chanid", &chanidi,
					"jdate", &jdatei,
					"calratio", &calratioi,
					"calper", &calperi,
					"tshift", &tshifti,
					"instant", instanti,
					NULL);
			if (!merge_sensor (db, stai, chani, timei, endtimei, inidi, chanidi, jdatei,
			                calratioi, calperi, tshifti, instanti)) {
				fprintf (stderr, "merge_sensor: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing sensor table.
 */
        db = dblookup (db, 0, "sensor", 0, 0);
	sncmp = create_sncmp (sta, chan, time, endtime, inid, chanid, calratio, calper, tshift, db, -1);
	if (sncmp == NULL) {
		fprintf (stderr, "merge_sensor: Malloc error.\n");
		return (0);
	}
/*
 *	Add sensor compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (sensor_stbl, sncmp, compare_sensor);
	if (ptr == NULL) {
		fprintf (stderr, "merge_sensor: Error return from addstbl.\n");
		return (0);
	}
	sncmp2 = (struct sensor_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (sncmp != sncmp2) {
		if (sncmp->time >= sncmp2->time-1.0 && sncmp->endtime <= sncmp2->endtime+1.0) {
			if (sncmp->inid != sncmp2->inid) {
				fprintf (stderr, 
		"merge_sensor: sta-ontime-offtime match with mismatch in inid.\n");
				my_free (sncmp);
				return (0);
			}
			if (sncmp->chanid != sncmp2->chanid) {
				fprintf (stderr, 
		"merge_sensor: sta-ontime-offtime match with mismatch in chanid.\n");
				my_free (sncmp);
				return (0);
			}
			if (!match(sncmp->calratio,sncmp2->calratio,6)) {
				fprintf (stderr, 
		"merge_sensor: sta-ontime-offtime match with mismatch in calratio.\n");
				my_free (sncmp);
				return (0);
			}
			if (!match(sncmp->tshift,sncmp2->tshift,2)) {
				fprintf (stderr, 
		"merge_sensor: sta-ontime-offtime match with mismatch in tshift.\n");
				my_free (sncmp);
				return (0);
			}
			if (!match(sncmp->calper,sncmp2->calper,6)) {
				fprintf (stderr, 
		"merge_sensor: sta-ontime-offtime match with mismatch in calper.\n");
				my_free (sncmp);
				return (0);
			}
			my_free (sncmp);
			return (1);
		}
		if (sncmp->time >= sncmp2->time && sncmp->time+1.0 < sncmp2->endtime) {
			fprintf (stderr, "merge_sensor: %.3f overlap in sensor times for %s %s.\n",
						sncmp2->endtime-sncmp->time, sta, chan);
			my_free (sncmp);
			return (0);
		}
		if (sncmp->endtime > sncmp2->time+1.0 && sncmp->endtime <= sncmp2->endtime) {
			fprintf (stderr, "merge_sensor: %.3f overlap in sensor times for %s %s.\n",
						sncmp->endtime-sncmp2->time, sta, chan);
			my_free (sncmp);
			return (0);
		}
		if (sncmp->time < sncmp2->time && sncmp->endtime > sncmp2->endtime) {
			fprintf (stderr, "merge_sensor: %.3f overlap in sensor times for %s %s.\n",
						sncmp2->endtime-sncmp2->time, sta, chan);
			my_free (sncmp);
			return (0);
		}
		if (ABS(sncmp->time-sncmp2->endtime) < 1.0) {
			if (match(sncmp->calratio,sncmp2->calratio,6) && match(sncmp->calper,sncmp2->calper,6) 
					&& match(sncmp->tshift,sncmp2->tshift,2) && sncmp->inid == sncmp2->inid
					&& sncmp->chanid == sncmp2->chanid) {
				sncmp2->endtime = endtime;
				sncmp2->db.record = sncmp2->ituple;
				dbputv (sncmp2->db, 0,
					"endtime", endtime,
					NULL);
				my_free (sncmp);
				return (1);
			}
		}
		if (ABS(sncmp->endtime-sncmp2->time) < 1.0) {
			if (match(sncmp->calratio,sncmp2->calratio,6) && match(sncmp->calper,sncmp2->calper,6) 
					&& match(sncmp->tshift,sncmp2->tshift,2) && sncmp->inid == sncmp2->inid
					&& sncmp->chanid == sncmp2->chanid) {
				sncmp2->time = time;
				sncmp2->db.record = sncmp2->ituple;
				jdate = yearday(time);
				dbputv (sncmp2->db, 0,
					"time", time,
					"jdate", jdate,
					NULL);
				my_free (sncmp);
				return (1);
			}
		}
	}

/*
 *	No match in sorted table.
 *	Add new sensor tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		sncmp->ituple = value.i-1;
		return (1);
 	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	jdate = yearday(time);
	dbputv (db, 0,
		"sta", sta,
		"chan", chan,
		"time", time,
		"endtime", endtime,
		"inid", inid,
		"chanid", chanid,
		"jdate", jdate,
		"calratio", calratio,
		"calper", calper,
		"tshift", tshift,
		"instant", instant,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	sncmp->ituple = value.i-1;
	return (1);
}

int
merge_instrument (db, insname, instype, band, digital, samprate, ncalib, ncalper,
		dir, dfile, rsptype)

Dbptr           db;
char *              insname;
char *                       instype;
char *                                band;
char *                                      digital;
double                                               samprate;
double                                                         ncalib;
double                                                                 ncalper;
char *          dir;
char *               dfile;
char *                      rsptype;

{
	Dbvalue value;
	int inid;
	static int ok_write;
	static int inidi;

	static Stbl *instrument_stbl=NULL;
	struct instrument_cmp_ *icmp;
	struct instrument_cmp_ *icmp2;
	char *ptr;

	int compare_instrument();

/*
 *	Create instrument sorted table
 */
	if (instrument_stbl == NULL) {
		int n;

		instrument_stbl = newstbl (compare_instrument);
		if (instrument_stbl == NULL) {
			fprintf (stderr, "merge_instrument: Error creating instrument sorted table.\n");
			return (0);
		}
		db = dblookup (db, 0, "instrument", 0, 0);
		dbquery (db, dbRECORD_COUNT, &n);
		ok_write = 0;
		for (db.record=0; db.record<n; db.record++) {
			char insnamei[64], instypei[32], bandi[8], digitali[8];
			char diri[256], dfilei[256], rsptypei[16];
			double sampratei, ncalibi, ncalperi;

			dbgetv (db, 0,	"insname", insnamei,
					"instype", instypei,
					"band", bandi,
					"digital", digitali,
					"samprate", &sampratei,
					"ncalib", &ncalibi,
					"ncalper", &ncalperi,
					"dir", diri,
					"dfile", dfilei,
					"rsptype", rsptypei,
					"inid", &inidi,
					NULL);
			if (!merge_instrument (db, insnamei, instypei, bandi, digitali, sampratei, ncalibi, ncalperi,
			                diri, dfilei, rsptypei)) {
				fprintf (stderr, "merge_instrument: Error loading sorted table.\n");
				return (0);
			}
		}
		ok_write = 1;
	}
/*
 *	Look for match in existing instrument table.
 */
        db = dblookup (db, 0, "instrument", 0, 0);
	icmp = create_icmp (dir, dfile, rsptype, samprate, ncalib, ncalper, db, -1, -1);
	if (icmp == NULL) {
		fprintf (stderr, "merge_instrument: Malloc error.\n");
		return (0);
	}
/*
 *	Add instrument compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (instrument_stbl, icmp, compare_instrument);
	if (ptr == NULL) {
		fprintf (stderr, "merge_instrument: Error return from addstbl.\n");
		return (0);
	}
	icmp2 = (struct instrument_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (icmp != icmp2) {
		my_free (icmp);
		return (icmp2->inid);
	}
/*
 *	No match in sorted table.
 *	Add new instrument tuple.
 */
 	if (!ok_write) {
		dbquery (db, dbRECORD_COUNT, &value);
		icmp->ituple = value.i-1;
		icmp->inid = inidi;
		return (icmp->inid);
 	}
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	inid = dbnextid(db, "inid");
	dbputv (db, 0,
		"inid", inid,
		"insname", insname,
		"instype", instype,
		"band", band,
		"digital", digital,
		"samprate", samprate,
		"ncalib", ncalib,
		"ncalper", ncalper,
		"dir", dir,
		"dfile", dfile,
		"rsptype", rsptype,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	icmp->ituple = value.i-1;
	icmp->inid = inid;
	return (icmp->inid);
}

int
merge_stage (db, sta, chan, time, endtime, stageid, ssident, gnom, iunits, ounits, gcalib, gtype, izero,
	     decifac, samprate, leadfac, dir, dfile)

Dbptr        db;
char *           sta;
char *                chan;
double                      time;
double                            endtime;
int                                        stageid;
char *                                              ssident;
double                                                       gnom;
char *                                                             iunits;
char *                                                                     ounits;
double                                                                             gcalib;
char *                                                                                     gtype;
int                                                                                               izero;
int          decifac;
double                samprate;
double                          leadfac;
char *                                   dir;
char *                                        dfile;

{
	int i, n;
	Dbvalue value;

	static Stbl *stage_stbl=NULL;
	struct stage_cmp_ *stcmp;
	struct stage_cmp_ *stcmp2;
	char *ptr;

	int compare_stage();

/*
 *	Create stage sorted table
 */
	if (stage_stbl == NULL) {
		stage_stbl = newstbl (compare_stage);
		if (stage_stbl == NULL) {
			fprintf (stderr, "merge_stage: Error creating stage sorted table.\n");
			return (0);
		}
	}
/*
 *	Look for match in existing stage table.
 */
        db = dblookup (db, 0, "stage", 0, 0);
	stcmp = create_stcmp (sta, chan, time, endtime, stageid, ssident, gnom, iunits, ounits,
	                        gcalib, gtype, izero, decifac, samprate, leadfac, dir, dfile, db, -1);
	if (stcmp == NULL) {
		fprintf (stderr, "merge_stage: Malloc error.\n");
		return (0);
	}
/*
 *	Add stage compare structure to sorted table.
 *
 *	addstbl() returns either the input pointer, indicating that there
 *	was no match and the new pointer was added to the table, or a 
 *	different pointer, indicating that there was a match with the
 *	returned pointer set to the matching table entry.
 */
	ptr = myaddstbl (stage_stbl, stcmp, compare_stage);
	if (ptr == NULL) {
		fprintf (stderr, "merge_stage: Error return from addstbl.\n");
		return (0);
	}
	stcmp2 = (struct stage_cmp_ *) ptr;
/*
 *	If return structure is different, then there is a match.
 */
	if (stcmp != stcmp2) {
		if (stcmp->time >= stcmp2->time-1.0 && stcmp->endtime <= stcmp2->endtime+1.0) {
			if (stcmp->izero != stcmp2->izero) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in izero %d %d.\n",
					sta, chan, time, stageid, stcmp->izero, stcmp2->izero);
				my_free (stcmp);
				return (0);
			}
			if (stcmp->decifac != stcmp2->decifac) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in decifac %d %d.\n",
					sta, chan, time, stageid, stcmp->decifac, stcmp2->decifac);
				my_free (stcmp);
				return (0);
			}
			if (strcmp(stcmp->ssident, stcmp2->ssident)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in ssident %s %s.\n",
					sta, chan, time, stageid, stcmp->ssident, stcmp2->ssident);
				my_free (stcmp);
				return (0);
			}
			if (strcmp(stcmp->iunits, stcmp2->iunits)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in iunits %s %s.\n",
					sta, chan, time, stageid, stcmp->iunits, stcmp2->iunits);
				my_free (stcmp);
				return (0);
			}
			if (strcmp(stcmp->ounits, stcmp2->ounits)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in ounits %s %s.\n",
					sta, chan, time, stageid, stcmp->ounits, stcmp2->ounits);
				my_free (stcmp);
				return (0);
			}
			if (strcmp(stcmp->gtype, stcmp2->gtype)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in gtype %s %s.\n",
					sta, chan, time, stageid, stcmp->gtype, stcmp2->gtype);
				my_free (stcmp);
				return (0);
			}
			if (strcmp(stcmp->dir, stcmp2->dir)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in dir %s %s.\n",
					sta, chan, time, stageid, stcmp->dir, stcmp2->dir);
				my_free (stcmp);
				return (0);
			}
			if (strcmp(stcmp->dfile, stcmp2->dfile)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in dfile %s %s.\n",
					sta, chan, time, stageid, stcmp->dfile, stcmp2->dfile);
				my_free (stcmp);
				return (0);
			}
			if (!match(stcmp->gnom,stcmp2->gnom,6)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in gnom %.6f %.6f.\n",
					sta, chan, time, stageid, stcmp->gnom, stcmp2->gnom);
				my_free (stcmp);
				return (0);
			}
			if (!match(stcmp->gcalib,stcmp2->gcalib,6)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in gcalib %.6f %.6f.\n",
					sta, chan, time, stageid, stcmp->gcalib, stcmp2->gcalib);
				my_free (stcmp);
				return (0);
			}
			if (!match(stcmp->samprate,stcmp2->samprate,7)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in samprate %.7f %.7f.\n",
					sta, chan, time, stageid, stcmp->samprate, stcmp2->samprate);
				my_free (stcmp);
				return (0);
			}
			if (!match(stcmp->leadfac,stcmp2->leadfac,7)) {
				fprintf (stderr, 
		"merge_stage: sta-chan-time,stageid match %s %s %f %d with mismatch in leadfac %.7f %.7f.\n",
					sta, chan, time, stageid, stcmp->leadfac, stcmp2->leadfac);
				my_free (stcmp);
				return (0);
			}
			my_free (stcmp);
			return (1);
		}
		if (stcmp->time >= stcmp2->time && stcmp->time+1.0 < stcmp2->endtime) {
			fprintf (stderr, "merge_stage: overlap in stage times.\n");
			my_free (stcmp);
			return (0);
		}
		if (stcmp->endtime > stcmp2->time+1.0 && stcmp->endtime <= stcmp2->endtime) {
			fprintf (stderr, "merge_stage: overlap in stage times.\n");
			my_free (stcmp);
			return (0);
		}
		if (stcmp->time < stcmp2->time && stcmp->endtime > stcmp2->endtime) {
			fprintf (stderr, "merge_stage: overlap in stage times.\n");
			my_free (stcmp);
			return (0);
		}
		if (ABS(stcmp->time-stcmp2->endtime) < 1.0) {
			if (match(stcmp->gnom,stcmp2->gnom,6) && match(stcmp->gcalib,stcmp2->gcalib,6) 
					&& match(stcmp->samprate,stcmp2->samprate,7) 
					&& match(stcmp->leadfac,stcmp2->leadfac,7) 
					&& stcmp->izero == stcmp2->izero
					&& stcmp->decifac == stcmp2->decifac
					&& !strcmp(stcmp->ssident, stcmp2->ssident)
					&& !strcmp(stcmp->iunits, stcmp2->iunits)
					&& !strcmp(stcmp->ounits, stcmp2->ounits)
					&& !strcmp(stcmp->gtype, stcmp2->gtype)
					&& !strcmp(stcmp->dir, stcmp2->dir)
					&& !strcmp(stcmp->dfile, stcmp2->dfile)) {
				stcmp2->endtime = endtime;
				stcmp2->db.record = stcmp2->ituple;
				dbputv (stcmp2->db, 0,
					"endtime", endtime,
					NULL);
				my_free (stcmp);
				return (1);
			}
		}
		if (ABS(stcmp->endtime-stcmp2->time) < 1.0) {
			if (match(stcmp->gnom,stcmp2->gnom,6) && match(stcmp->gcalib,stcmp2->gcalib,6) 
					&& match(stcmp->samprate,stcmp2->samprate,7) 
					&& match(stcmp->leadfac,stcmp2->leadfac,7) 
					&& stcmp->izero == stcmp2->izero
					&& stcmp->decifac == stcmp2->decifac
					&& !strcmp(stcmp->ssident, stcmp2->ssident)
					&& !strcmp(stcmp->iunits, stcmp2->iunits)
					&& !strcmp(stcmp->ounits, stcmp2->ounits)
					&& !strcmp(stcmp->gtype, stcmp2->gtype)
					&& !strcmp(stcmp->dir, stcmp2->dir)
					&& !strcmp(stcmp->dfile, stcmp2->dfile)) {
				stcmp2->time = time;
				stcmp2->db.record = stcmp2->ituple;
				dbputv (stcmp2->db, 0,
					"time", time,
					NULL);
				my_free (stcmp);
				return (1);
			}
		}
	}

/*
 *	No match in sorted table.
 *	Add new stage tuple.
 */
	db.record = dbNULL;
	db.field = dbALL;
	dbget(db, 0);
	db.record = dbSCRATCH;
	dbputv (db, 0,
		"sta", sta,
		"chan", chan,
		"time", time,
		"endtime", endtime,
		"stageid", stageid,
		"ssident", ssident,
		"gnom", gnom,
		"iunits", iunits,
		"ounits", ounits,
		"gcalib", gcalib,
		"gtype", gtype,
		"izero", izero,
		"decifac", decifac,
		"samprate", samprate,
		"leadfac", leadfac,
		"dir", dir,
		"dfile", dfile,
		NULL);
	dbadd (db, 0);
/*
 *	Fix up new compare structure in sorted table.
 */
	dbquery (db, dbRECORD_COUNT, &value);
	stcmp->ituple = value.i-1;
	return (1);
}

void *
myaddstbl (stbl, key, compare)

Stbl *     stbl;
void *           key;
int (*compare)();

{
	int i;
	void *nkey;

	for (i=0; i<maxstbl(stbl); i++) {
		nkey = getstbl (stbl, i);
		if (!(*compare)(key, nkey)) break;
	}
	if (i == maxstbl(stbl)) {
		nkey = addstbl (stbl, key);
	}
	return (nkey);
}

/* $Id: merge_db.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
