/* pelinedb2xml */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"

int debug = 0;
char *dbname = NULL;
char *dbdownname = NULL;
char *xmlfile = NULL;
char *ignore_subset_expr = NULL;

struct eventinfo {
	/* read in -> evid, prefor, time, lat, lon, ml, shakemap, ndef, review, lddate */
      int evid, prefor, ndef, shakemap;
      double time, lat, lon, ml, lddate;
      char review[4];
};

struct xmlinfo {
	/* 	-> dbxmlid, evid, prefor, time, lat, lon, ml, ndef, shakemap, review, orlddate */
	/*	-> lddate, evstatus, evused */
      int dbxmlid, evid, prefor, shakemap, event_use, ndef, evid_checked, update_row, evused;
      double time, lat, lon, ml, lddate, orlddate;
      char review[4], evstatus[16];
};

int usage ( int i, char *message ) {
	fprintf(stderr, "Usage: %s -db db  -dbdown downgradeddb -dbsubset ignore_subset_expr -xmlfile filename [-days num_days] [-debug] \n", Program_Name );
	switch (i) {
		case 1: fprintf(stderr,"%s is unknown option\n", message); break;
		case 2: fprintf(stderr,"%s database can not be openned \n", message); break;
		case 3: fprintf(stderr,"%s\n", message); break;
		case 4: fprintf(stderr,"-days %s, option is not valid \n", message); break;
		case 5: fprintf(stderr,"can not open xml file %s \n", message); break;
	}
	exit(0);
}

int main (int argc, char **argv)
{
	int i, x, xx, yy, nrecsdown, nrecsig, nrecs, nrecsp, evfound, evfound2;
	int used_later, xxx;
	int dbxmlid_lastid, crunchtable;	
	Program_Name = argv[0];
	Dbptr db, dbdown;
	Dbptr dbevent, dborigin, dbeventpipe, dbpipedb2xml, dbignorerow;
	Dbptr dbeventdown, dbeventdown1, dborigindown, dbeventpipedown, dbignorerowdown;
	char    prefjoinviewname[256],joinviewname[256], evstatus[2];
	char down_subsetstring[1024];
	char prefor_subsetstring[1024];

	extern void *calloc(size_t, size_t), *realloc(void *, size_t);
	extern char *epoch2str(double, char *);
	struct eventinfo *evtb, evdown;
	struct xmlinfo *xmltb;
	int totalnewrows, totalupdaterows;
	int startat, cur_evid, prev_evid_idx, days, minepoch, evidig;
	char whoig[25];
	extern FILE *fopen();
	FILE *xml_fp;

	totalnewrows = 0;

	(void) strcpy (prefjoinviewname, "preforjoin");
	(void) strcpy (joinviewname, "downjoin");

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	xx = 0;
	days = -1;
	for (x=1; x < argc; ++x) {
		if( strcmp("-debug", argv[x]) == 0 ) {
			debug = 1;
		} /* if */
		if( strcmp("-db", argv[x]) == 0 ) {
			if (x+1 < argc) {
				++x;
				dbname = argv[x];
				++xx; /* required option */
			} else { /* if */
				usage(3, "-db option has no dbname argument");
			} /* else */
		} /* if */
		if( strcmp("-dbdown", argv[x]) == 0 ) {
			if (x+1 < argc) {
				++x;
				dbdownname = argv[x];
				++xx; /* required option */
			} else { /* if */
				usage(3, "-dbdown option has no downgradeddb argument");
			} /* else */
		} /* if */
		if( strcmp("-dbsubset", argv[x]) == 0 ) {
			if (x+1 < argc) {
				++x;
				ignore_subset_expr = argv[x];
				++xx; /* required option */
			} else { /* if */
				usage(3, "-dbsubset option has no ignore_subset_expr argument");
			} /* else */
		} /* if */
		if( strcmp("-xmlfile", argv[x]) == 0 ) {
			if (x+1 < argc) {
				++x;
				xmlfile = argv[x];
				++xx; /* required option */
			} else { /* if */
				usage(3, "-xmlfile option has no xml file name argument");
			} /* else */
		} /* if */
		if( strcmp("-days", argv[x]) == 0 ) {
			if (x+1 < argc) {
				++x;
				days = atoi(argv[x]);
			} else { /* if */
				usage(3, "-days option has no argument");
			} /* else */
			if (days != -1) {
				/* days must be greater than 0 or not set -1 */
				if (days < 1 ) {
					usage(4, argv[x]); 
				}
			}
		} /* if */
	} /* for */
	if ( days > 0 ) {
		minepoch = days * 3600 * 24 ;
		minepoch = yearday(now()-minepoch);
	} else
	  {
		minepoch = 0;
	}
	if(xx != 4) { /* -db db -xmlfile filename, must be set */
		usage(3, "Required options not set, -db db -xmlfile filename");
	}
	
	/* open data base */
	if( debug == 1) {
		fprintf(stderr,"Open database %s \n", dbname);
	} 
	if(dbopen( dbname, "r+", &db) != 0) {
		usage(2, dbname);
	} /* if */
	
	/* join event, origin, eventpipe tables */
	dbevent = dblookup( db, 0, "event", 0, 0 );
	dborigin = dblookup( db, 0, "origin", 0, 0 );
	dbeventpipe = dblookup( db, 0, "eventpipe", 0, 0 );

	dbevent = dbjoin(dbevent, dborigin, 0, 0, 0, 0, 0);
	sprintf(prefor_subsetstring,"prefor == orid && evid != -1 && yearday(origin.time) > %d", minepoch );
	dbevent = dbsubset(dbevent, prefor_subsetstring, prefjoinviewname);
	dbevent = dbjoin(dbevent, dbeventpipe, 0, 0, 1/* outerjoin*/, 0, 0);

	/* read in  evid, prefor, time, lat, lon, ml, shakemap, ndef, review, lddate */
	dbquery( dbevent, dbRECORD_COUNT, &nrecs );
	if ( nrecs < 1 ) { i = 1; } else { i = nrecs; }
	if(!(evtb = (struct eventinfo *) calloc(i, sizeof(struct eventinfo)))) {
		fprintf(stderr, "Can not alocate memory for tables event, origin, eventpipe, in  db %s\n", dbname) ;
	}
	/* read in database dbjoin and dbsubset  event, origin, and eventpipe tables */
	if ( nrecs > 0 ) {
		if (debug == 1 ) {
			fprintf(stderr,"evtb: %d rows after join and subset of event, origin, and eventpipe tables\n", nrecs );
		}
		for(dbevent.record=0; dbevent.record < nrecs; ++dbevent.record) {
			i = dbevent.record;
			if(dbgetv(dbevent, 0,
		                "evid", &(evtb[i].evid),
		                "prefor", &(evtb[i].prefor),
				"time", &(evtb[i].time),
				"lat", &(evtb[i].lat), 
				"lon", &(evtb[i].lon),
				"ml", &(evtb[i].ml),
				"shakemap", &(evtb[i].shakemap),
              			"ndef", &(evtb[i].ndef), 
				"review", evtb[i].review,
              			"origin.lddate", &(evtb[i].lddate), 0) == dbINVALID) {
					complain(0,"dbgetv problem loading originevent index=%d \n", i);
				exit(0);
			}
			/* force shakemap to be zero even if outerjoin does not connect */
			if(evtb[i].shakemap < 0) {
				evtb[i].shakemap = 0;
			}
			/* if (debug == 1 ) {
			*	fprintf(stderr,"evtb: evid=%d, prefor=%d, ml=%f\n", evtb[i].evid, evtb[i].prefor, evtb[i].ml);
			*} */ 
		}
	}
		

	/* read in current pipedb2xml table */
	/* 	-> dbxmlid, evid, prefor, time, lat, lon, ml, shakemap, review, orlddate */
	/*	-> lddate, evstatus, evused */

	dbxmlid_lastid = -1;
	dbpipedb2xml = dblookup( db, 0, "pipedb2xml", 0, 0 );
	dbquery( dbpipedb2xml, dbRECORD_COUNT, &nrecsp );
	if ( nrecsp < 1 ) { i = 1; } else { i = nrecsp; }
	if(!(xmltb = (struct xmlinfo *) calloc(i, sizeof(struct xmlinfo)))) {
		fprintf(stderr, "Can not alocate memory for tables pipedb2xml, in  db %s\n", dbname) ;
	}
	if ( nrecsp > 0 ) {
		if (debug == 1 ) {
			fprintf(stderr,"xmltb: loading %d pipedb2xml rows  \n", nrecsp );
		}
		for(dbpipedb2xml.record=0; dbpipedb2xml.record < nrecsp; ++dbpipedb2xml.record) {
			i = dbpipedb2xml.record;
			if(dbgetv(dbpipedb2xml, 0,
		                "dbxmlid", &(xmltb[i].dbxmlid),
		                "evid", &(xmltb[i].evid),
		                "prefor", &(xmltb[i].prefor),
				"time", &(xmltb[i].time),
				"lat", &(xmltb[i].lat), 
				"lon", &(xmltb[i].lon),
				"ml", &(xmltb[i].ml),
				"ndef", &(xmltb[i].ndef),
				"shakemap", &(xmltb[i].shakemap),
				"review", xmltb[i].review,
				"evstatus", xmltb[i].evstatus,
				"evused", &(xmltb[i].evused),
				"orlddate", &(xmltb[i].orlddate),
              			"lddate", &(xmltb[i].lddate), 0) == dbINVALID) {
					complain(0,"dbgetv problem loading pipedb2xml index=%d \n", i);
				exit(0);
			} else
			  {
				/* init to zero, set event_use to one if row is used in events.xml */  
				xmltb[i].evid_checked = xmltb[i].update_row = xmltb[i].event_use = 0 ; 
				if(dbxmlid_lastid < xmltb[i].dbxmlid) {
					dbxmlid_lastid = xmltb[i].dbxmlid;
				}
			}
			/* if (debug == 1 ) {
			*	fprintf(stderr,"xmltb: dbxmlid=%d, evid=%d, prefor=%d, ml=%f, ndef=%d\n", xmltb[i].dbxmlid, xmltb[i].evid, xmltb[i].prefor, xmltb[i].ml, xmltb[i].ndef);
			* } */
		}
	}
	
	/* new prefor database for loop */
	for(x=0; x < nrecs; ++x) {
		evfound = 0;	
		/* pipedb2xml table loop */ 
		for(xx=0; xx < nrecsp; ++xx) {
			/* prefor event row is present */
			if (    (int) evtb[x].evid == (int) xmltb[xx].evid && 
				(int) evtb[x].prefor == (int) xmltb[xx].prefor &&
				(double) evtb[x].time == (double) xmltb[xx].time && 
				(double) evtb[x].lat == (double) xmltb[xx].lat &&
				(double) evtb[x].lon == (double) xmltb[xx].lon && 
				(double) evtb[x].ml == (double) xmltb[xx].ml &&
				(double) evtb[x].ndef == (double) xmltb[xx].ndef &&
				(int) evtb[x].shakemap == (int) xmltb[xx].shakemap && 
				strcmp(evtb[x].review, xmltb[xx].review) == 0 
				) { 
				/* check to see if event was downgraded, recreate event if no longer */
				/* Downgraded */
				used_later = 0;
				if ( (int) xmltb[xx].evused == (int) 0  ) {
					for(xxx=0; xxx < nrecsp; ++xxx) {
						if(xmltb[xxx].evused == 1 && 
							xmltb[xx].evid == xmltb[xxx].evid &&
							strcmp(xmltb[xxx].evstatus, "Downgraded") == 0 &&
							xx != xxx) {
							used_later = 1; 
							xx = xxx = nrecsp; /* exit loops */
						}
					}
				}
				if (used_later == 0) {
					/* event row found */
					evfound = 1;
					xx = nrecsp; /* exit out of loops */
				}
			}
		}
		/* add new prefor row to pipedb2xml table */
		if (evfound == 0 ) {
			/* new evid found add row only if ml is not NULL */
			dbpipedb2xml = dblookup( db, 0, "pipedb2xml", 0, 0 );
			dbpipedb2xml.record = dbaddnull (dbpipedb2xml);

			if( dbpipedb2xml.record > -1 ) {
				dbxmlid_lastid++;
			}
			(void) strcpy(evstatus, "-");
			if(dbpipedb2xml.record < 0 || dbputv(dbpipedb2xml, 0,
				"dbxmlid", dbxmlid_lastid,
		                "evid", evtb[x].evid, "prefor", evtb[x].prefor, "time", evtb[x].time,
				"lat", evtb[x].lat, "lon", evtb[x].lon, "ml", evtb[x].ml,
				"shakemap", evtb[x].shakemap, "review", evtb[x].review,
				"ndef", evtb[x].ndef,
				"evstatus", evstatus, /* set later */ 
				"evused", 0,  /* set later */
				 "orlddate", evtb[x].lddate, 
				0) == dbINVALID) {
					complain(0,"dbputv problem loading pipedb2xml row at index %d\n", x);
			} else
			  {
				++ totalnewrows;
				xx = nrecsp; 
				++nrecsp;
				if(!(xmltb = (struct xmlinfo *) realloc(
					(struct xmlinfo *) xmltb,  sizeof(struct xmlinfo ) * nrecsp ))) {
					fprintf(stderr, "Can not recalloc memory for tables pipedb2xml, in  db %s\n", dbname) ;
					exit(0);
				} else 
				  {
		                	xmltb[xx].dbxmlid = dbxmlid_lastid; 
		                	xmltb[xx].evid = evtb[x].evid;
		                	xmltb[xx].prefor = evtb[x].prefor;
					xmltb[xx].time = evtb[x].time;
					xmltb[xx].lat = evtb[x].lat; 
					xmltb[xx].lon = evtb[x].lon;
					xmltb[xx].ml = evtb[x].ml;
					xmltb[xx].ndef = evtb[x].ndef;
					xmltb[xx].shakemap = evtb[x].shakemap;
					(void) strcpy(xmltb[xx].review, evtb[x].review);
					(void) strcpy(xmltb[xx].evstatus, evstatus);
					xmltb[xx].evused  = 0;
					xmltb[xx].orlddate = evtb[x].lddate; 
					/* init to zero, set to one if row is used in events.xml */
					xmltb[xx].evid_checked = xmltb[xx].update_row = xmltb[xx].event_use = 0; 
              				xmltb[xx].lddate = (double) now ();

				}
			}
		}
	}
	/* update */
	if (debug == 1) {
		fprintf(stderr, "%d new rows added to table pipedb2xml\n", totalnewrows);
	}
	if( totalnewrows > 0 ) {
		/* find next event_id to process */
		startat = 0;
		while (startat < nrecsp ) {
			cur_evid = -2;
			for(xx=startat; xx < nrecsp; ++xx) {
				if(xmltb[xx].evid_checked == 0 ) {
					startat = xx; /* location to start next loop */
					cur_evid = xmltb[xx].evid;
					xx = nrecsp; /* exit loop */
				}
			}
			if(cur_evid == -2) startat = nrecsp; /* no new evids found exit loop */
			/* find best event row to use */
			prev_evid_idx = -1; 
			for(xx=startat; xx < nrecsp; ++xx) {
				if(xmltb[xx].evid_checked == 0  && xmltb[xx].evid == cur_evid) {	
					xmltb[xx].evid_checked = 1;
					if (prev_evid_idx == -1 ) {
						/* initialize values */
						if ( (double) xmltb[xx].ml > (double)-999.0 ) {
							xmltb[xx].event_use = 1;
							prev_evid_idx = xx;
							if( strcmp(xmltb[xx].evstatus, "-") == 0 ) {
								if( strcmp(xmltb[xx].review, "-" ) == 0 ) {
									(void) strcpy(xmltb[xx].evstatus, "Initial");
								/* event must be Revised if analyst reviewed */
								} else
								  {
									(void) strcpy(xmltb[xx].evstatus, "Revised");

								}
								xmltb[xx].update_row = 1;	
							} 
							(void) strcpy(evstatus, xmltb[xx].evstatus);
						}
					} else
					  {
						if ( (double) xmltb[xx].ml > (double)-999.0 ) {
							(void) strcpy(evstatus, 
								xmltb[prev_evid_idx].evstatus);
							if(xmltb[xx].dbxmlid > xmltb[prev_evid_idx].dbxmlid) {
								xmltb[prev_evid_idx].event_use = 0;
								xmltb[xx].event_use = 1;
								/*shakemap version number should only increase*/
								if(xmltb[prev_evid_idx].shakemap > 
									xmltb[xx].shakemap) {
									xmltb[xx].shakemap = xmltb[prev_evid_idx].shakemap;
									xmltb[xx].update_row = 1;	
								} 
								if(strcmp(evstatus, "Initial") == 0 ||
								   strcmp(evstatus, "Downgraded") == 0 ||
								   strcmp(evstatus, "Revised") == 0 ) {
									if(strcmp(xmltb[xx].evstatus, "-") 
										== 0) {
										(void)strcpy(xmltb[xx
										  ].evstatus, "Revised");
										xmltb[xx].update_row = 1;
									} /* if */
								} /* if */
							} /* if dbxmlid  */
							prev_evid_idx = xx;
						} /* if ml > -999.0 */
					} /* if, else prev_evid_idx */ 
				} /* if evid_cheched */
			} /* for xmltbl, xx=startat */ 
		} /* while still more rows not checked */ 

	} else /* totalnewrows > 0 */ 
	  { /* xmltb[].event_use = xmltb[xx].evused */ 
		for(xx=0; xx < nrecsp; ++xx) {
			xmltb[xx].event_use = xmltb[xx].evused;	
		}
	} 

	/* find Downgraded events */
	/* these are events that are no longer in the subset box */ 
	for(xx=0; xx < nrecsp; ++xx) {
		if(xmltb[xx].event_use == 1 && strcmp (xmltb[xx].evstatus, "Downgraded") != 0 ) {
			evfound = 0;
			for(x=0; x < nrecs; ++x) {
				if(evtb[x].evid == xmltb[xx].evid) {
					evfound = 1; x = nrecs;
				}
			}
	
			if (evfound == 0) { /* event xx was Downgraded */
				/* find prefor for evid in non subsetted database */  
				if(dbopen( dbdownname, "r", &dbdown) != 0) {
					usage(2, dbdownname);
				} /* if */
				/* join event, origin, eventpipe tables */
				dbeventdown = dblookup( dbdown, 0, "event", 0, 0 );
				dborigindown = dblookup( dbdown, 0, "origin", 0, 0 );
				dbeventpipedown = dblookup( dbdown, 0, "eventpipe", 0, 0 );
				dbignorerowdown = dblookup( dbdown, 0, "ignorerow", 0, 0 );
				dbeventdown = dbjoin(dbeventdown, dborigindown, 0, 0, 0, 0, 0);
				dbeventdown = dbjoin(dbeventdown, dbignorerowdown, 0, 0, 1/* outerjoin */ , 0, 0);
				sprintf(down_subsetstring,"(%s) && prefor == orid && evid == %d && yearday(origin.time) > %d", ignore_subset_expr, xmltb[xx].evid, minepoch);
				dbeventdown1 = dbsubset(dbeventdown, down_subsetstring, joinviewname);
				dbquery( dbeventdown1, dbRECORD_COUNT, &nrecsdown );
				/* no events inside subsetting box */
				if ( debug == 1 && nrecsdown > 0) { 
					fprintf(stderr, "dbsubset found %d rows from below subset-\n\t%s\n",nrecsdown, down_subsetstring);
				}
				if (nrecsdown == 0) { 
					/* sprintf(down_subsetstring,"(whoig!=\"all\"&&whoig==\"-\") && prefor == orid && evid == %d && yearday(origin.time) > %d", xmltb[xx].evid, minepoch); */
					/* for now only ignore events with whoig set to all */
					sprintf(down_subsetstring,"(whoig!=\"all\") && prefor == orid && evid == %d && yearday(origin.time) > %d", xmltb[xx].evid, minepoch);
					dbeventdown = dbsubset(dbeventdown, down_subsetstring, joinviewname);
					dbeventdown = dbjoin(dbeventdown, dbeventpipedown, 0, 0, 1/* outerjoin */ , 0, 0);
					dbquery( dbeventdown, dbRECORD_COUNT, &nrecsdown );
					if ( debug == 1 && nrecsdown > 0 ) {
						fprintf(stderr, "dbsubset found %d rows from below subset-\n\t%s\n",nrecsdown, down_subsetstring);
					}
				} else
				  {
					/* Do not Downgrade, leave pipedb2xml table alone */
					nrecsdown = 0;
				}
				/* events outside of box */
				if (nrecsdown > 0 ) {
					dbeventdown.record=0;
					if(dbgetv(dbeventdown, 0, "evid", &(evdown.evid),
			                  "prefor", &(evdown.prefor), "time", &(evdown.time),
					  "lat", &(evdown.lat), "lon", &(evdown.lon),
					  "ml", &(evdown.ml), "shakemap", &(evdown.shakemap),
       		       			  "ndef", &(evdown.ndef), "review", evdown.review,
       		       			  "origin.lddate", &(evdown.lddate), 0) == dbINVALID) {
						complain(0,"dbgetv problem loading dbeventdown index=%d \n", dbeventdown.record);
					  exit(0);
					}
					/* found prefor event with magnitude */
					if ((double)evdown.ml != (double) -999.0) {
						/* create new down graded row if it does not already exist */ 
						evfound2 = 0;	
						/* pipedb2xml table loop */ 
						for(yy=0; yy < nrecsp; ++yy) {
							/* prefor event row is present */
							if ( (int) evdown.evid == (int) xmltb[yy].evid && 
							  (int) evdown.prefor == (int) xmltb[yy].prefor &&
							  (double) evdown.time == (double) xmltb[yy].time && 
							  (double) evdown.lat == (double) xmltb[yy].lat &&
							  (double) evdown.lon == (double) xmltb[yy].lon && 
							  (double) evdown.ml == (double) xmltb[yy].ml &&
							  (double) evdown.ndef == (double) xmltb[yy].ndef &&
							  (int) evdown.shakemap == (int) xmltb[yy].shakemap && 
							  strcmp(evdown.review, xmltb[yy].review) == 0  
							) { 
								/* event row found */
								evfound2 = 1;
								yy = nrecsp; /* exit out of loops */
							}
						}
						if(evfound2 == 0) {
							/* new evid found add row only if ml is not NULL */
							dbpipedb2xml = dblookup( db, 0, "pipedb2xml", 0, 0 );
							dbpipedb2xml.record = dbaddnull (dbpipedb2xml);
							if( dbpipedb2xml.record > -1 ) {
								dbxmlid_lastid++;
							}
							(void) strcpy(evstatus, "Downgraded");
							if(dbpipedb2xml.record < 0 || dbputv(dbpipedb2xml, 0,
								"dbxmlid", dbxmlid_lastid,
		               				 	"evid", evdown.evid, 
								"prefor", evdown.prefor, "time", evdown.time,
								"lat", evdown.lat, "lon", evdown.lon, 
								"ml", evdown.ml, "shakemap", xmltb[xx].shakemap, 
								"review", evdown.review, "ndef", evdown.ndef,
								"evstatus", evstatus, 
								"evused", 1, 
				 				"orlddate", evdown.lddate, 
							0) == dbINVALID) {
								complain(0,"dbputv downgrade problem loading pipedb2xml row at index %d\n", x);
							} else
			  			  	  {
								++ totalnewrows;
								yy = nrecsp; 
								++nrecsp;
								if(!(xmltb = (struct xmlinfo *) realloc(
									(struct xmlinfo *) xmltb,  sizeof(struct xmlinfo ) * nrecsp ))) {
									fprintf(stderr, "Can not recalloc memory for tables pipedb2xml, in  db %s\n", dbname) ;
									exit(0);
								} else 
								  {
		                					xmltb[yy].dbxmlid = dbxmlid_lastid; 
		                					xmltb[yy].evid = evdown.evid;
		                					xmltb[yy].prefor = evdown.prefor;
									xmltb[yy].time = evdown.time;
									xmltb[yy].lat = evdown.lat; 
									xmltb[yy].lon = evdown.lon;
									xmltb[yy].ml = evdown.ml;
									xmltb[yy].ndef = evdown.ndef;
									/* use shakemap version from last */
									/* non Downgraded event */ 
									xmltb[yy].shakemap = xmltb[xx].shakemap;
									(void) strcpy(xmltb[yy].review, 
										evdown.review);
									(void) strcpy(xmltb[yy].evstatus, 
										evstatus);
									xmltb[yy].evused  = 1;
									xmltb[yy].orlddate = evdown.lddate; 
									/* init to zero, set to one if row is used in events.xml */
									xmltb[yy].evid_checked = 1;
									xmltb[yy].event_use = 1;
									xmltb[yy].update_row = 1; 
              								xmltb[yy].lddate = (double) now ();
									/* use new added row */ 
									xmltb[xx].event_use = 0;
									xmltb[xx].evused  = 0;
									xmltb[xx].update_row = 1;
									if (debug == 1 ) {
										fprintf(stderr,"xmltb down: dbxmlid=%d, evid=%d, prefor=%d, ml=%f, ndef=%d\n", xmltb[yy].dbxmlid, xmltb[yy].evid, xmltb[yy].prefor, xmltb[yy].ml, xmltb[yy].ndef);
									}
									/* exit loop */
									yy = nrecsp; 
								}  /* else if */
							} /* else if */
						} /* if evfound2 > 0 */
					} /* ml != -999.0 */ 
				} /* nrecs > 0 */
				dbclose(dbdown);
			} /* evfound > 0 */ 
		} /* event_use */
	} /* for */

	/* set event_use to 0 for all events found in ignore table */
	for(xx=0; xx < nrecsp; ++xx) {
		if(xmltb[xx].event_use == 1 ) {
			dbignorerow = dblookup( db, 0, "ignorerow", 0, 0 );
			dbquery( dbignorerow, dbRECORD_COUNT, &nrecsig );
			if( nrecsig > 0 ) {
				for(dbignorerow.record=0; dbignorerow.record < nrecsig; ++dbignorerow.record) {
					if(dbgetv(dbignorerow, 0, "evid", &evidig, 
						"whoig", whoig , 0) == dbINVALID) {
						complain(0,"dbgetv problem loading ignorerow index=%d \n", dbignorerow.record);
					  exit(0);
					} else
					  {
						/* do not use rows found in ignore database */ 
						if(evidig == xmltb[xx].evid && strncmp("all", whoig, 3) == 0) {
							xmltb[xx].event_use = 0;
							xmltb[xx].update_row = 1;
							if(debug == 1) {
								fprintf(stderr,"Ignore row detected for evid %d, whoig = %s\n",
									xmltb[xx].evid, whoig);
							}
						}
					}
				}
			}
		}
	} 
	/* set update_row and evused, current event_use */

	dbpipedb2xml = dblookup( db, 0, "pipedb2xml", 0, 0 );
	crunchtable = 0;
	for(xx=0; xx < nrecsp; ++xx) {
		if( xmltb[xx].event_use != xmltb[xx].evused ) {
			xmltb[xx].evused = xmltb[xx].event_use;
			xmltb[xx].update_row = 1;
		}
		/* null row in dbpiptdb2xml if older than -days option */
		if( (int) yearday(xmltb[xx].time) <= (int) minepoch ) {
			dbpipedb2xml.record = xx;
			dbmark(dbpipedb2xml);
			xmltb[xx].update_row = 0;
			crunchtable = 1;
			if(debug == 1) {
				fprintf(stderr,"Removing pipedb2xml row=%d, dbxmlid=%d, origin_yearday=%d\n",
					xx, xmltb[xx].dbxmlid, yearday(xmltb[xx].time));
			}
		}
	}

	totalupdaterows = 0;
	/* update all rows with xmltb[xx].update_row = 1 */
	for(xx=0; xx < nrecsp; ++xx) {
		if( xmltb[xx].update_row == 1 ) {
			if(debug == 1) {
				fprintf(stderr,"Updating record %d with evid %d\n", xx, xmltb[xx].evid);
			}
			++totalupdaterows;
			dbpipedb2xml.record = xx;
			xmltb[xx].lddate = (double) now ();
			if( dbputv(dbpipedb2xml, 0,
				"dbxmlid", xmltb[xx].dbxmlid,
		                "evid", xmltb[xx].evid, "prefor", xmltb[xx].prefor, 
				"time", xmltb[xx].time,
				"lat", xmltb[xx].lat, "lon", xmltb[xx].lon, 
				"ml", xmltb[xx].ml, "shakemap", xmltb[xx].shakemap, 
				"review", xmltb[xx].review,
				"ndef", xmltb[xx].ndef,
				"evstatus", xmltb[xx].evstatus, 
				"evused", xmltb[xx].evused, 
				"orlddate", xmltb[xx].orlddate,
				"lddate", xmltb[xx].lddate, 
				0) == dbINVALID) {
					complain(0,"dbputv update problem loading pipedb2xml row at index %d\n", x);
			} 
		}
	}

        if(crunchtable == 1) {
                dbcrunch(dbpipedb2xml);
        }

	/* create new events.xml file */
	/* only create new xml file if rows have been updated or added */
	if( totalnewrows > 0 || totalupdaterows > 0) {
		/* open xmlfile */
		if ((xml_fp = fopen(xmlfile, "w+")) == NULL) {
			usage(5, xmlfile);
		}
		fprintf(xml_fp, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n");
		fprintf(xml_fp, "  <response>\n");
		fprintf(xml_fp, "    <events>\n");
		for(xx=0; xx < nrecsp; ++xx) {
			/* print xml event information */
			if(xmltb[xx].evused == 1) {
				fprintf(xml_fp, "      <event status=\"%s\">\n", xmltb[xx].evstatus);
				fprintf(xml_fp, "        <event_id>%d</event_id>\n", xmltb[xx].evid);
				fprintf(xml_fp, "        <origin_time>%s</origin_time>\n", 
					epoch2str(xmltb[xx].time, "%y/%m/%d %T")); 
				fprintf(xml_fp, "        <latitude>%-.4f</latitude>\n", xmltb[xx].lat);
				fprintf(xml_fp, "        <longitude>%-.4f</longitude>\n", xmltb[xx].lon);
				fprintf(xml_fp, "        <magnitude>%-.2f</magnitude>\n", xmltb[xx].ml);
				fprintf(xml_fp, "        <shakemap>%d</shakemap>\n", xmltb[xx].shakemap);
				fprintf(xml_fp, "        <arrivals>%d</arrivals>\n", xmltb[xx].ndef);
				fprintf(xml_fp, "        <reviewed>%s</reviewed>\n", xmltb[xx].review);
				fprintf(xml_fp, "        <timestamp>%s</timestamp>\n", 
					epoch2str(xmltb[xx].lddate, "%y/%m/%d %T"));
				fprintf(xml_fp, "      </event>\n");
			}
		}
		fprintf(xml_fp, "    </events>\n");
		fprintf(xml_fp, "  </response>\n");
	}

	/* close database */
	dbclose(db);

	/* free evtb */
	free(evtb);
	free(xmltb);
} /* main */
