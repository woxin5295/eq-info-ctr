/* dbtmwfextractor  */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"

extern void *calloc(size_t, size_t), *realloc(void *, size_t);

struct tminfo {
	int found; /* 1 -> row already in output db, 0 -> new row */
	char sta[7], chan[9], filter[31], units[13]; 
	char segtype[2], auth[16];
	double alarm_peak, alarm_time, time, endtime, lddate; /* endtime remove from schema */
	char alarm_state[36];
};
struct tminfo *tm_info_raw, *tm_info;

struct wfinfo {
	char sta[7], chan[9]; 
	double time, endtime, samprate;
	long wfid, nsamp; 
};
struct wfinfo *inwf_info, *outwf_info;

struct stainfo {
	char sta[7];
	double in_time, in_endtime;
	double out_time, out_endtime;
};

/* hardwire station information */
#define MAXSTATIONS 2048 

/* global variables */
int debug;

int database_changed( Dbptr db )
{
	static int initialized = 0;
	static off_t tm_size = 0;
	static time_t tm_mtime = 0;
	int     present;
	long	numrows;
	static Dbptr   dbtm;
	static Dbvalue value;
	struct stat tm_statbuf;

	dbtm = dblookup( db, 0, "tm", 0, 0 );
	dbquery(dbtm, dbRECORD_COUNT, &numrows );
	if ( numrows > 0 ) { 
		dbquery( dbtm, dbTABLE_FILENAME, &value );
	
		stat( value.t, &tm_statbuf );

		if( ! initialized ) {
			tm_size = tm_statbuf.st_size;
			tm_mtime = tm_statbuf.st_mtime;
			initialized++;
			/* process on restart */
			return 1;
		}
	
		if( tm_size != tm_statbuf.st_size ||
			tm_mtime != tm_statbuf.st_mtime ) {
			tm_size = tm_statbuf.st_size;
			tm_mtime = tm_statbuf.st_mtime;
			return 1;
		}
	}

	return 0;
}

int usage ( int i, char *message ) {

		fprintf(stderr, "Usage: %s -dbtm dbtm -dbinwf dbinwf -dboutwf dboutwf\n\t [-datatype type] [-match match] [-pretime seconds] [-extracttime seconds] [-mintime seconds] [-posttime seconds] [ -sleeptime seconds ] [-onerun] [ -debug ] [-db2evt dir db2evt.pf] \n", Program_Name );
		switch (i) {
			case 1: fprintf(stderr,"%s is unknown option\n", message); break;
			case 2: fprintf(stderr,"%s \n", message); break;
			case 3: fprintf(stderr,"%s database can not be openned \n", message); break;
			case 4: fprintf(stderr,"%s database can not be re-openned \n", message); break;
		}

		fprintf(stderr,"\t -dbtm, db containing tm table \n");
		fprintf(stderr,"\t -dbinwf, db containg rawdata\n");
		fprintf(stderr,"\t -dboutwf, db containg extracted tm alarms \n");
		fprintf(stderr,"\t -datatype type, data type to write out data default is sd \n");
		fprintf(stderr,"\t\t May need to specified flag multiply_calib_in_sac_output \n");
		fprintf(stderr,"\t\t in trdefaults.pf, if using SAC datatype\n");
		fprintf(stderr,"\t -match match, regular expression of channels to match, default chan=~/([BH]N.*)/ \n"); 
		fprintf(stderr,"\t -sleeptime, interval in seconds \n\t\t between tm table modification, default 30 seconds \n");
		fprintf(stderr,"\t -pretime, pre-detection time at which th start extraction \n");
		fprintf(stderr,"\t\t start = thresholdtime - pretime, default 20 seconds \n");
		fprintf(stderr,"\t -mintime, minimum amount of time to extract in seconds \n");
		fprintf(stderr,"\t\t default 120 seconds \n");
		fprintf(stderr,"\t -extracttime, maximum period of data to extract in seconds \n");
		fprintf(stderr,"\t\t if no off alarm_state set \n");
		fprintf(stderr,"\t\t default 300 seconds \n");
		fprintf(stderr,"\t -posttime, length of time to include after threshold is no longer exceeded.");
		fprintf(stderr,"\t\t default 110 seconds \n");
		fprintf(stderr,"\t -onerun, only one tm table loop \n");
		fprintf(stderr,"\t -db2evt dir db2evt.pf, fork the db2evt program and create .EVT files dir directory, use db2evt.pf parameter file \n");
		fprintf(stderr,"\t -debug, turn on debugging \n");
		exit(0);
}

int main( int argc, char **argv )
{
	char    dir[FILENAME_MAX];
	static char base[FILENAME_MAX];
	Dbptr   dbtm,dbinwf,dboutwf,tr,dbsub;
	char    dbtm_name[STRSZ], dbinwf_name[STRSZ], dboutwf_name[STRSZ], match[STRSZ], datatype[STRSZ];
	char	stachanexpr[STRSZ], db2evtdir[STRSZ]; 
	char	db2evtdirname[STRSZ], db2evtparam[STRSZ];
	int x, sleeptime, pretime, posttime, extracttime, mintime, set, set2;
	int loop, onerun, i, ii, iii, found;
	long nrecsoutwf;
	long nrecstm, nrecsinwf;
	struct stainfo station[MAXSTATIONS];
	int numsta, xx, newsta, db2evtfork;
	double currenttime;
	char *forkprogargs[30]; /* fork a subprocess args*/
	char db2evt_name[128], db2evtstart[STRSZ], db2evtend[STRSZ];
	char db2evt_s_opt[3], db2evt_p_opt[3];
	int statusp, pid;

	
	/* hardwire db2evt_name */
	/* (void) strcpy(db2evt_name, "/home/mitch/CSS/NIKO_EVT_CODE/ins/db2evt"); */
	(void) strcpy(db2evt_name, "db2evt");
	forkprogargs[0] = &db2evt_name[0];
	forkprogargs[1] = &db2evt_name[0];
	(void) strcpy(db2evt_s_opt, "-s");
	forkprogargs[2] = &db2evt_s_opt[0];
	(void) strcpy(db2evt_p_opt, "-p");
	forkprogargs[4] = &db2evt_p_opt[0];

	numsta = 0;
	set2 = 0;
	debug = 0; /* 0-> debugging off, 1-> low level debugging on, > 1 more debug messages */

	dirbase( argv[0], dir, base );
	Program_Name = base;

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 );

	sleeptime = 30; /* defaults */
        pretime = 20;
        posttime = 110;
	mintime = 120;
        extracttime = 300;
	db2evtfork = 0;
	(void) strcpy(match,"chan=~/([BH]N.*)/");
	(void) strcpy(datatype,"sd");

	for (x=1; x < argc; ++x) {
		set =  0;
		if( strcmp("-dbtm", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++set2; ++x; (void) strcpy(dbtm_name, argv[x]);}
		}
		if( strcmp("-dbinwf", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;set2=set2+10; ++x; (void) strcpy(dbinwf_name, argv[x]);}
		}
		if( strcmp("-dboutwf", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;set2=set2+100; ++x; (void) strcpy(dboutwf_name, argv[x]);}
		}
		if( strcmp("-pretime", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				pretime = atoi(argv[x]);
			} 
		}
		if( strcmp("-extracttime", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				extracttime = atoi(argv[x]);
			} 
		}
		if( strcmp("-mintime", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				mintime = atoi(argv[x]);
			} 
		}
		if( strcmp("-posttime", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				posttime = atoi(argv[x]);
			} 
		}
		if( strcmp("-sleeptime", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				sleeptime = atoi(argv[x]);
			} 
		}
		if( strcmp("-match", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				(void) strcpy(match, argv[x]);
			} 
		}
		if( strcmp("-datatype", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++set; ++x; 
				(void) strcpy(datatype, argv[x]);
			} 
		}
		if( strcmp("-db2evt", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				db2evtfork = 1;
				++set; ++x; 
				(void) strcpy(db2evtdir, argv[x]);
				if ( (x + 1) != argc ) {
					++x;
					(void) strcpy(db2evtparam, argv[x]);
				}
			} 
		}
		if( strcmp("-onerun", argv[x]) == 0 ) {
			++set;
			onerun = 1;
		}
		if( strcmp("-debug", argv[x]) == 0 ) {
			++set;
			debug = 1;
		}
		if (set == 0) {
			usage (1, argv[x]); 
		}
	}
	if (set2 != 111) {
		usage ( 2, "Required options were not provided");
	}

	/* loop forever */
	loop = 1;
	while (loop) {
		if (onerun == 1) loop = 0;
		/* open tm database for memory loading */
		if(dbopen( dbtm_name, "r", &dbtm ) != 0 ) {
			usage(3, dbtm_name);
		}	
		/* open input wf db for reading */
		if(dbopen( dbinwf_name, "r", &dbinwf ) != 0 ) {
			usage(3, dbinwf_name);
		}
		/* open qddsdb database for memory loading */
		if(dbopen( dboutwf_name, "r+", &dboutwf ) != 0 ) {
			usage(3, dboutwf_name);
		}
		while (database_changed(dbtm) == 0) {
			sleep(sleeptime);
		}
		if (debug == 1) {
			fprintf(stderr,"tm table modification detected at %s \n",strtime(now()));
		} 
		/* load tm database tables into memory */
		dbtm = dblookup( dbtm, 0, "tm", 0, 0 );
		dbquery (dbtm, dbRECORD_COUNT, &nrecstm);
		/* allocate space for dbtm data */ 
		if ( (long) nrecstm < (long) 1 ) { i = 1; } else { i = nrecstm; }
		if(!(tm_info_raw = (struct tminfo *) calloc(i, sizeof(struct tminfo)))) {
			fprintf(stderr, "Can not alocate memory for tm table in %s\n", dbtm_name) ;
			exit(1);
		} 
		if(!(tm_info = (struct tminfo *) calloc(i, sizeof(struct tminfo)))) {
			fprintf(stderr, "Can not alocate memory for tm table in %s\n", dbtm_name) ;
			exit(1);
		} 
		 
		if ( (long) nrecstm > (long) 0 ) {
			/* load tm table into memory */
			if(debug == 1) fprintf(stderr,"Loading %d tm table records \n", nrecstm); 
			for(dbtm.record=0; (long) dbtm.record < (long) nrecstm; ++dbtm.record) {
				i = dbtm.record;
				/* endtime was removed from tm schema */ 
				if(dbgetv(dbtm, 0, 
					"alarm_peak", &(tm_info_raw[i].alarm_peak), 
					"alarm_time", &(tm_info_raw[i].alarm_time), 
					"time", &(tm_info_raw[i].time), 
					"lddate", &(tm_info_raw[i].lddate),
					"sta", tm_info_raw[i].sta, 
					"chan", tm_info_raw[i].chan, 
					"filter", tm_info_raw[i].filter, 
					"units", tm_info_raw[i].units, 
					"segtype", tm_info_raw[i].segtype, 
					"auth", tm_info_raw[i].auth,
					"alarm_state", tm_info_raw[i].alarm_state,
					0)== dbINVALID) {
					complain(0,"dbgetv problem loading tm record=%d\n", dbtm.record);
				}
				/* reset time and endtime within pretime, extracttime, and */
				/* posttime limitations */
				if ((double)tm_info_raw[i].time != 
					(double)-9999999999.99900 ) {
					tm_info_raw[i].time = tm_info_raw[i].time - 
						(double) pretime;
				}
			}
			/* group sta,chan,time */	
			ii = 0;
			for(i=0; (long) i < (long) nrecstm; ++i) {
				found = -1;
				for(iii = 0; iii < ii; ++iii) {
					if(strcmp(tm_info_raw[i].sta, 
						tm_info[iii].sta) == 0 &&
					   strcmp(tm_info_raw[i].chan, 
						tm_info[iii].chan) == 0 &&
					   (double) tm_info_raw[i].time == 
						(double) tm_info[iii].time){
						found = iii;
					}  
					   
				}
				/* not found */
				if (found == -1 ) {
					if( strncmp(tm_info_raw[i].alarm_state,"off",3) != 0) {
					  (void) strcpy(tm_info[ii].sta, tm_info_raw[i].sta);
					  (void) strcpy(tm_info[ii].chan, tm_info_raw[i].chan);
					  (void) strcpy(tm_info[ii].filter, 
						tm_info_raw[i].filter);
					  (void) strcpy(tm_info[ii].units, tm_info_raw[i].units);
					  (void) strcpy(tm_info[ii].segtype, 
						tm_info_raw[i].segtype);
					  (void) strcpy(tm_info[ii].alarm_state, 
						tm_info_raw[i].alarm_state);
					  (void) strcpy(tm_info[ii].chan, tm_info_raw[i].chan);
					  tm_info[ii].alarm_peak = 
						(double) tm_info_raw[i].alarm_peak; 
					  tm_info[ii].alarm_time = 
						(double) tm_info_raw[i].alarm_time; 
					  tm_info[ii].time = (double) tm_info_raw[i].time; 
					  tm_info[ii].lddate = tm_info_raw[i].lddate;
					  tm_info[ii].endtime = (double)9999999999.99900; 
					  ++ii;
					}
				/* found */
				} else
				  {
					if(strncmp(tm_info_raw[i].alarm_state,"off",3) == 0) {
						tm_info[found].endtime = 
							(double) tm_info_raw[i].alarm_time + 
							(double) posttime + (double) pretime;
						if((double)((double)tm_info[found].endtime - 
							(double)tm_info[found].time) < 
							(double) mintime ) {
							tm_info[found].endtime = 
								(double) tm_info_raw[i].time + 
								(double) mintime ;
						}
					}
				} 
			}
			/* set endtime for all rows that exceed */
			currenttime = now(); 
			nrecstm = ii;
			for(i=0; (long) i < (long) nrecstm; ++i) {
				if(tm_info[i].endtime == 9999999999.99900) {
					if( ((double)currenttime - (double)tm_info[i].time ) >
						(double) (1.0 * extracttime) ) {
						tm_info[i].endtime = tm_info[i].time + 
							(double) extracttime;
					} 
				} 
			}
			/* let wfdisc data flush, sleep 5 minutes to mintime */ 
			sleep(300 + mintime); 
			/* sleep(15 + mintime); */
			/* load input wfdisc table into memory */
			dbinwf = dblookup( dbinwf, 0, "wfdisc", 0, 0 );
			dbinwf = dbsubset ( dbinwf, match, 0 ) ;
			dbquery (dbinwf, dbRECORD_COUNT, &nrecsinwf);
			/* allocate space for dbinwf data */ 
			if ( (long) nrecsinwf < (long) 1 ) { i = 1; } else { i = nrecsinwf; }
			if(!(inwf_info = (struct wfinfo *) calloc(i, sizeof(struct wfinfo)))) {
				fprintf(stderr, "Can not alocate memory for input wfdisc table in %s\n", dbinwf_name) ;
				exit(1);
			} 
		 
			if ( (long) nrecsinwf > (long) 0 ) {
				if(debug == 1) fprintf(stderr,"Loading %d input wfdisc table records \n", nrecsinwf); 
				/* load input wfdisc table into memory */
				for(dbinwf.record=0; (long) dbinwf.record < (long) nrecsinwf; ++dbinwf.record) {
					i = dbinwf.record;
					if(dbgetv(dbinwf, 0,
						"sta", inwf_info[i].sta, 
						"chan", inwf_info[i].chan, 
						"time", &(inwf_info[i].time), 
						"endtime", &(inwf_info[i].endtime), 
						"samprate", &(inwf_info[i].samprate), 
						"wfid", &(inwf_info[i].wfid), 
						"nsamp", &(inwf_info[i].nsamp), 
						0)== dbINVALID) {
						complain(0,"dbgetv problem loading input wfdisc record=%d\n", dbinwf.record);
					}
				}
			}
			/* load output wfdisc table into memory */
			dboutwf = dblookup( dboutwf, 0, "wfdisc", 0, 0 );
			dboutwf = dbsubset ( dboutwf, match, 0 ) ;
			dbquery (dboutwf, dbRECORD_COUNT, &nrecsoutwf);
			/* allocate space for dboutwf data */ 
			if ( (long) nrecsoutwf < 1 ) { i = 1; } else { i = nrecsoutwf; }
			if(!(outwf_info = (struct wfinfo *) calloc(i, sizeof(struct wfinfo)))) {
				fprintf(stderr, "Can not alocate memory for output wfdisc table in %s\n", dboutwf_name) ;
				exit(1);
			} 
			if ( (long) nrecsoutwf > (long) 0 ) {
				/* load output wfdisc table into memory */
				if(debug == 1) fprintf(stderr,"Loading %d input wfdisc table records \n", nrecsinwf); 
				for(dboutwf.record=0; (long) dboutwf.record < (long) nrecsoutwf; ++dboutwf.record) {
					i = dboutwf.record;
					if(dbgetv(dboutwf, 0,
						"sta", outwf_info[i].sta, 
						"chan", outwf_info[i].chan, 
						"time", &(outwf_info[i].time), 
						"endtime", &(outwf_info[i].endtime), 
						"samprate", &(outwf_info[i].samprate), 
						"wfid", &(outwf_info[i].wfid), 
						"nsamp", &(outwf_info[i].nsamp), 
						0)== dbINVALID) {
						complain(0,"dbgetv problem loading output wfdisc record=%d\n", dboutwf.record);
					}
				}
			}
			/* setup station information time range values */
			for(x=0; (long) x < (long) nrecsinwf; ++x) {
				newsta = 0;
				for (xx=0; xx<numsta; ++xx) {
					if(strcmp(inwf_info[x].sta, station[xx].sta) == 0) {
						if(inwf_info[x].time < station[xx].in_time ||
							station[xx].in_time == -9999999999.99900 ) {
							station[xx].in_time = inwf_info[x].time;
						}
						if(inwf_info[x].endtime > station[xx].in_endtime ||
							station[xx].in_endtime == 9999999999.99900 ) {
							station[xx].in_endtime = inwf_info[x].endtime;
						}
						newsta = 1;
					} 
				}
				if(newsta == 0) {
					(void) strcpy( station[numsta].sta, inwf_info[x].sta);
					station[numsta].in_time = inwf_info[x].time;
					station[numsta].in_endtime = inwf_info[x].endtime;
					station[numsta].out_time = -9999999999.99900;
					station[numsta].out_endtime = 9999999999.99900;
					++numsta;
				}
			}
			for(x=0; (long) x < (long) nrecsoutwf; ++x) {
				newsta = 0;
				for (xx=0; xx<numsta; ++xx) {
					if(strcmp(outwf_info[x].sta, station[xx].sta) == 0 ) {
						if(outwf_info[x].time < station[xx].out_time ||
							station[xx].out_time == -9999999999.99900 ) {
							station[xx].out_time = outwf_info[x].time;
						}
						if(outwf_info[x].endtime > station[xx].out_endtime ||
							station[xx].out_endtime == 9999999999.99900 ) {
							station[xx].out_endtime = outwf_info[x].endtime;
						}
						newsta = 1;
					} 
				}
				if(newsta == 0) {
					(void) strcpy( station[numsta].sta, outwf_info[x].sta);
					station[numsta].out_time = outwf_info[x].time;
					station[numsta].out_endtime = outwf_info[x].endtime;
					station[numsta].in_time = -9999999999.99900;
					station[numsta].in_endtime = 9999999999.99900;
					++numsta;
				}
			}
			/* find tm table data that need to be segmented */
			for(x=0; (long) x < (long) nrecstm; ++x) {
				/* ignore null endtime rows */
				if (tm_info[x].endtime != 9999999999.99900) {
				  for (xx=0; xx<numsta; ++xx) {
					if( strcmp(tm_info[x].sta,station[xx].sta) == 0 && 
					    tm_info[x].time >= station[xx].in_time && 
					    tm_info[x].endtime <= station[xx].in_endtime &&
					    (tm_info[x].time >= station[xx].out_endtime ||
					    (station[xx].out_time == -9999999999.99900 &&
					     station[xx].out_endtime == 9999999999.99900))) {
						/* segment out this tm data */
						if(debug == 1) {
							fprintf(stderr,"sta=%s will segment out, \n\ttime=%s, endtime=%s\n\n", tm_info[x].sta, strtime(tm_info[x].time), strtime(tm_info[x].endtime));
						}
						(void) sprintf(stachanexpr,"sta=~/(%s)/ && %s", tm_info[x].sta, match);
						/* db2evt fork */
						if (db2evtfork == 1) {
							forkprogargs[3] = &stachanexpr[0];
							forkprogargs[5] = &db2evtparam[0];
							forkprogargs[6] = &dbinwf_name[0];
							char *tf;
							tf = epoch2str(tm_info[x].time, "%y%m%d%H%M%S");
							(void) sprintf(db2evtdirname,"%s/%s_%s.EVT",db2evtdir, tm_info[x].sta, tf);
							free(tf);
							(void) sprintf(db2evtstart,"%s", strtime(tm_info[x].time));
							(void) sprintf(db2evtend,"%s", strtime(tm_info[x].endtime));
							forkprogargs[7] = &db2evtdirname[0];
							forkprogargs[8] = &db2evtstart[0];
							forkprogargs[9] = &db2evtend[0];
							forkprogargs[10] = NULL;
							/* create child process  and start fork */
							if(debug == 1 ) {
								fprintf(stderr,"Forking db2evt\n"); 
								fprintf(stderr,"\t %s %s \"%s\" %s %s %s %s \"%s\" \"%s\" \n",
									forkprogargs[1], forkprogargs[2],
									forkprogargs[3], forkprogargs[4],
									forkprogargs[5], forkprogargs[6],
									forkprogargs[7], forkprogargs[8],
									forkprogargs[9]);
							}
							pid = fork();
							/* child process */
							if (pid == 0) {
								execvp(forkprogargs[0], &forkprogargs[1]);
							} else if (pid == -1) {
								if (debug == 1) {
									fprintf(stderr,"Can not fork %s \n", forkprogargs[0]);
								}
							/* parent */
							} else if (pid > 0) {
								(void) wait(&statusp);
							}
						}
						dbsub = dbsubset ( dbinwf, stachanexpr, 0);
						tr = dbinvalid() ;
						if ( trload_css ( dbsub, strtime(tm_info[x].time), strtime(tm_info[x].endtime), &tr, 0, 0 ) ) {
							die ( 1, "trload_css failed" ) ;
						}
						/* eliminate marked gaps from waveforms; these
						   are gaps where no data was recorded, but special missing values
						   were inserted instead of ending the wfdisc record */
						if ( trsplit(tr, 0, 0) ) {
							complain ( 0, "trsplit failed" ) ;
						}
						/* splice together any adjacent data segments which can be */
						if ( trsplice(tr, 0, 0, 0) ) {
							complain ( 0, "trsplice failed" ) ;
						}
						dboutwf = dblookup ( dboutwf, 0, "wfdisc", 0, 0 ) ;
						if ( trsave_wf ( tr, dboutwf, datatype, 0, 0 ) ) {
							die ( 0, "trsave_wf failed" ) ;
						}
						if(strcmp(tm_info[x].sta,station[xx].sta) == 0) {
							if( tm_info[x].time < station[xx].out_time  ||
								station[xx].out_time == -9999999999.99900) {
								station[xx].out_time = tm_info[x].time;
							}
							if( tm_info[x].endtime > station[xx].out_endtime ||
								station[xx].out_endtime == 9999999999.99900 ) {
								station[xx].out_endtime = tm_info[x].endtime;
							}
						}
						tr.table = dbALL ;
						trfree(tr) ;
					} 
				} /* for */
			    }/* if */
			} /* for */
		}
/*		if(debug == 1) {
*			for (xx=0; xx<numsta; ++xx) {
*				fprintf(stderr, "%d sta=%s,in_time=%f,in_endtime=%f,out_time=%f,out_endtime=%f\n",
*					xx, station[xx].sta, station[xx].in_time, station[xx].in_endtime,
*					station[xx].out_time, station[xx].out_endtime);
*			}
*		}
*/
		/* close database */
		dbclose(dbtm);
		dbclose(dbinwf);
		dbclose(dboutwf);
		/* free up dbtm structure */
		free(tm_info);
		free(tm_info_raw);
		free(inwf_info); 
		free(outwf_info); 
	}
}
