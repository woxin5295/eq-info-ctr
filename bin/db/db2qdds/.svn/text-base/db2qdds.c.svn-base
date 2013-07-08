/* db2qdds */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"

extern int MenloCheckChar( char * );
extern void *calloc(size_t, size_t), *realloc(void *, size_t);
struct qddsinfo {
	/* qdds table parts */
	long evid, orid, jdate, vernum;
	char auth[16], qtype[3], psent[2], qddscube[81];
	double lddate;
	/* parts used internally by program */
	long reset; /* keep track of which fields match rows in summary database */
};

#define QDDSBLOCKSIZE 512
#define CUBESIZE 80

/* global variables */
struct qddsinfo *qi;
long numqind, numignored;
Dbptr dbeventjn, dbeventjn2, dbevent, dbeventjn_ig, dbeventjn2_ig, dbevent_ig;
long ignore_db; 
long currentqddsmemsize=0;
/* dborigin_to_qdds global variables */
/* passing as pointers has problems on MAC OS X */
char aline[81];
long evid, orid, jdate, version;

void sendqddspolldir(polldir, dbqdds, debug)
	char *polldir;
	Dbptr dbqdds;
	long debug;
{
	char pollfile[STRSZ];
	char cubeinfo[81];
	FILE *qddsfp;
	long i, modrow; 
	long nrecsq;
	modrow = 0;

	long evid, orid, jdate, vernum;
	char auth[16], qtype[3], psent[2], qddscube[81];
	double lddate;

	lddate = now();
	for ( i = 0; (long) i < (long) numqind; ++ i) {
		/* if((long)debug == (long)1) {
		*	fprintf(stderr,"sendqddspolldir: evid=%ld,psent=%s\n", qi[i].evid, qi[i].psent);
		* }  */
		if (strcmp(qi[i].psent,"n") == 0) {
			strncpy(cubeinfo, &(qi[i].qddscube[2]), CUBESIZE - 2);
			cubeinfo[11] = '\0';
			sprintf(pollfile,"%s/event.%s%c", polldir, cubeinfo, '\0');	
			if ((qddsfp = (FILE *)fopen(pollfile, "w+")) != NULL) {
				if((long)debug == (long)1) {
					fprintf(stderr,"Writing %s: evid=%ld,psent=%s\n", pollfile, qi[i].evid, qi[i].psent);
				 } 
				/* added newline character Tue Mar 25 08:32:45 AKDT 2008 */
				/*fprintf( qddsfp, "%s", qi[i].qddscube); */
				fprintf( (FILE *) qddsfp, "%s\n", qi[i].qddscube);
				fflush((FILE *) qddsfp);
				strcpy(qi[i].psent, "y");
				modrow = 1;
				fclose(qddsfp);
			} else
			  {
				strcpy(qi[i].psent, "n");
			}
		}
	}
	/* if row has been created you need to chang psent value in qdds table */
	dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
	dbquery (dbqdds, dbRECORD_COUNT, &nrecsq);
	if ( (long)nrecsq > (long)0 ) {
		for(dbqdds.record=0; (long) dbqdds.record <  (long) nrecsq; ++dbqdds.record) {
			if(dbgetv(dbqdds, 0, "evid", &evid, "orid", &orid, 
				"jdate", &jdate, "vernum", &vernum, 
				"auth", auth, "qtype", qtype,
				"qddscube", qddscube, "psent", psent, "lddate", &lddate, NULL) == dbINVALID) {
					complain(0,"sendqddspolldir:dbgetv problem loadin 1 record=%ld\n", dbqdds.record);
			} else
			  { /* scan for match in memory map and update if psend is different */
				for (i = 0; (long) i < (long) numqind; ++i) {
					if((long)evid == (long)qi[i].evid && (long)orid == (long)qi[i].orid &&
					   strcmp(auth,qi[i].auth)==0 &&
					   (long)jdate == (long)qi[i].jdate && (long)vernum == (long)qi[i].vernum &&
					   strcmp(qtype, qi[i].qtype)==0 &&
					   strcmp(qddscube, qi[i].qddscube)==0 ) {
						if(strcmp(qi[i].psent, psent) != 0) {
							if (dbputv(dbqdds, 0, "evid", evid, "orid", orid,
								"auth", auth, "jdate", jdate,
								"vernum", vernum, "qtype", qtype,
								"qddscube", qddscube, "psent", qi[i].psent, 
								"lddate", lddate, NULL) == dbINVALID) {
								complain(0,"dbputv record=%ld, evid=%ld, ,orid=%ld, jdate=%ld, qtype=%s,vernum=%ld,auth=%s\n",
								i, evid, orid, jdate, qtype, vernum, auth);
							} 
						}
					}
				}
			}
		}
	}
}
void database_clean_qdds( minepoch, debug, dbqdds, polldir )
	long minepoch;
	long debug;
	Dbptr dbqdds;
	char *polldir;
{
	long i, ii;
	long nrecsq, nrecnew;
	long numqindorig;
	
	long evid, orid, jdate, vernum;
	char auth[16], qtype[3], psent[2], qddscube[81], deleterow[91];
	double lddate;

	long crunch, crunchall;
	size_t qddsmemsize;

	/* first clean qdds database */
	dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
	dbquery (dbqdds, dbRECORD_COUNT, &nrecsq);
	if ( (long)nrecsq > (long)0 ) {
		for(dbqdds.record=0; (long) dbqdds.record < (long) nrecsq; ++dbqdds.record) {
			if(dbgetv(dbqdds, 0, "jdate", &jdate, NULL) == dbINVALID) {
				complain(0,"dbgetv can not read record=%ld\n", dbqdds.record);
			} else
			  {
				if((long)jdate < (long)minepoch) {
					if(dbmark (dbqdds) != 0){
						complain(0,"dbmark has problems on record=%ld\n", dbqdds.record);	
					}
				}
			}
		}
		/* delete all null rows */
		if(dbcrunch(dbqdds) != 0) {
			complain(0,"Can not dbcrunch db\n");
		} else
		  {
			dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
			dbquery (dbqdds, dbRECORD_COUNT, &nrecnew);
			if((long)debug > (long)1) {
				fprintf(stderr,"%ld-%ld=%ld rows have been crunched\n",
				nrecsq, nrecnew, nrecsq - nrecnew);	
			}
		}
		numqindorig = numqind;
		/* compress qi */
		for (i = 0; ( (long) i > (long) -1 && (long) i < (long) numqind ); ++i) {
			/* move up */
			if( (long)qi[i].jdate < (long)minepoch ) {
				--numqind;
				for (ii = i; (long) ii < (long) numqind; ++ii) {
					qi[ii].evid = qi[ii+1].evid;	
					qi[ii].orid = qi[ii+1].orid;
					strcpy(qi[ii].auth, qi[ii+1].auth);
					qi[ii].jdate = qi[ii+1].jdate;	
					qi[ii].vernum = qi[ii+1].vernum;
					strcpy(qi[ii].qtype, qi[ii+1].qtype);
					strcpy(qi[ii].psent, qi[ii+1].psent);
					strncpy(qi[ii].qddscube, qi[ii+1].qddscube, CUBESIZE);
					qi[ii].lddate = qi[ii+1].lddate;
					qi[ii].reset = qi[ii+1].reset;
				}
				--i;
			}
		}
		/* relocate memory for array to fit smaller size */
		if((long)numqind > (long)0 && (long)numqind < (long)numqindorig) {
			qddsmemsize = sizeof(struct qddsinfo) * (numqind+1);
			if ( (long)qddsmemsize > (long)currentqddsmemsize ) {
				/* currentqddsmemsize = (sizeof(struct qddsinfo) * QDDSBLOCKSIZE) + currentqddsmemsize; */
				currentqddsmemsize = (sizeof(struct qddsinfo) * QDDSBLOCKSIZE) + qddsmemsize;
			   
				if(!(qi = (struct qddsinfo *) realloc((struct qddsinfo *) qi, currentqddsmemsize))) {
					fprintf(stderr, "Can not realloc memory for qdds qi array\n") ;
					exit(1);
				} else
				  {
					if((long)debug > (long)1) {
						fprintf(stderr,"%ld-%ld=%ld qdds memory map was reduced\n",
						numqindorig, numqind, numqindorig - numqind);	
					}
					if((long)debug == (long)1) {
						fprintf(stderr,"database_clean_qdds:%ld bytes of memory has been reallocate \n", currentqddsmemsize);		
					}
				}
			} 
		}
		/* reset is set to one if row from dbsum is found in dbqdds */
		/***********************************************************/
		if( 1 == 2) { /* stop doing this section of code */ 
		dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
		dbquery (dbqdds, dbRECORD_COUNT, &nrecsq);
		crunchall = 0; 
		/* find all dbqdds rows that do not match dbsum rows */ 
		for (i = 0; ((long) i > (long) -1 && (long) i < (long) numqind); ++i) {
			crunch = 0;
			if( (long)qi[i].reset == (long)0 ) {
				/* qtype dl row just remove, after qddsfile has been create */
				if(strcmp(qi[i].qtype,"dl") == 0 && strcmp(qi[i].psent, "y") == 0 ) {
					crunch = crunchall = 1 ;
				}
				/* not delete row, create delete row, remove row from */
				/* qddsdb and memory map */ 
				if(strcmp(qi[i].qtype,"dl") != 0 ) {
					crunch = crunchall = 1 ;
					/* create delete row */
					strcpy(deleterow, qi[i].qddscube);
					deleterow[0] = 'D';
					deleterow[1] = 'E';
					deleterow[79] = '\0';
					deleterow[79] = 'A';
					/* deleterow[79] = MenloCheckChar(deleterow); */
					deleterow[80] = '\0';
					dbqdds.record = dbaddnull (dbqdds);
					dbquery (dbqdds, dbRECORD_COUNT, &nrecsq);
					if((long)dbqdds.record < (long)0 || dbputv(dbqdds, 0,
						"evid", qi[i].evid, "orid", qi[i].orid,
						"auth", qi[i].auth, "jdate", qi[i].jdate,
						"vernum", qi[i].vernum, "qtype", "dl",
						"qddscube", deleterow, "lddate", qi[i].lddate, 
						"psent", "n", NULL)== dbINVALID) {
							complain(0,"dbputv delete row record=%ld \n", 
							dbqdds.record);
					} else
					  {
						/* update memory map */
						if((long)numqind < (long)0) numqind = 0;
						qddsmemsize = sizeof(struct qddsinfo) * (numqind+1);
						if ( (long)qddsmemsize > (long)currentqddsmemsize ) {
							/* currentqddsmemsize = 
							(sizeof(struct qddsinfo) 
							* QDDSBLOCKSIZE) + currentqddsmemsize; */
							currentqddsmemsize = 
							(sizeof(struct qddsinfo) 
							* QDDSBLOCKSIZE) + qddsmemsize;
							if(!(qi = (struct qddsinfo *) realloc((struct qddsinfo *) qi, 
								currentqddsmemsize))) {
								fprintf(stderr, "Can not realloc memory for qdds memory map\n");
								exit(1);
							} else
							  {
								if((long)debug == (long)1) {
									fprintf(stderr,"database_clean_qdds:%ld bytes of memory has been reallocate \n", currentqddsmemsize);		
								}
							}
						}
						qi[numqind].evid = qi[i].evid;
						qi[numqind].orid = qi[i].orid;
						strcpy(qi[numqind].auth, qi[i].auth);
						qi[numqind].jdate = qi[i].jdate;
						qi[numqind].vernum = qi[i].vernum;
						strcpy(qi[numqind].qtype,"dl");
						strcpy(qi[numqind].psent,"n");
						strncpy(qi[numqind].qddscube,deleterow, CUBESIZE);
						qi[numqind].lddate = qi[i].lddate;
						qi[numqind].reset = 1;
						++numqind;
					}
				} 
			}
			/* dbmark row in qddsdb and crunch memory map */ 
			if ((long)crunch == (long)1 ) {
				if ( (long)nrecsq >  (long)0 ) {
					for(dbqdds.record=0; (long) dbqdds.record < (long) nrecsq; ++dbqdds.record) {
						if(dbgetv(dbqdds, 0, "evid", &evid, "orid", &orid, 
							"jdate", &jdate,
							"vernum", &vernum, "auth", auth, "qtype", qtype,
							"qddscube", qddscube, "psent", psent, "lddate", &lddate, NULL) == dbINVALID) {
								complain(0,"sendqddspolldir:dbgetv problem loadin 2 record=%ld\n", dbqdds.record);
						} else
						  {
							if((long)evid == (long)qi[i].evid && (long)orid == (long)qi[i].orid &&
							   strcmp(auth,qi[i].auth)==0 &&
							   (long)jdate == (long)qi[i].jdate && (long)vernum == (long)qi[i].vernum &&
							   strcmp(qtype, qi[i].qtype)==0 &&
							   strcmp(qddscube, qi[i].qddscube)==0 ) {
								if(dbmark (dbqdds) != 0){
									complain(0,"dbmark has problems on record=%ld\n", dbqdds.record);	
								}
								/* if((long)debug == (long)1) {
								*	fprintf(stderr,"%d record removed, jdate=%ld, minepoch=%ld, rows removed from dbsum db\n", dbqdds.record, jdate, minepoch);
								* } */
							}
						}
					}
				}
				/* shift memory map */
				--numqind;
				for (ii = i; (long) ii < (long) numqind; ++ii) {
					qi[ii].evid = qi[ii+1].evid;	
					qi[ii].orid = qi[ii+1].orid;
					strcpy(qi[ii].auth, qi[ii+1].auth);
					qi[ii].jdate = qi[ii+1].jdate;	
					qi[ii].vernum = qi[ii+1].vernum;
					strcpy(qi[ii].qtype, qi[ii+1].qtype);
					strcpy(qi[ii].psent, qi[ii+1].psent);
					strncpy(qi[ii].qddscube, qi[ii+1].qddscube, CUBESIZE);
					qi[ii].lddate = qi[ii+1].lddate;
					qi[ii].reset = qi[ii+1].reset;
				}
				--i;	
			}
		}
		if ((long)crunchall == (long)1 ) {
			if(dbcrunch(dbqdds) != 0) {
				complain(0,"Can not dbcrunch dbqdds reset rows \n");
			} else
			  {
				dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
				dbquery (dbqdds, dbRECORD_COUNT, &nrecnew);
				/* if(debug == 1) {
				* 	fprintf(stderr,"%d-%d=%d reset rows have been crunched\n",
				*	nrecsq, nrecnew, nrecsq - nrecnew);	
				*} */
			}
			/* update delete rows */
			sendqddspolldir(polldir, dbqdds, debug); 
		}
		}
		/* End of doing this section of code */ 
		/***********************************************************/
	}
}
long database_changed_ignore_oldcrap( Dbptr db )
{
	static long initialized = 0;
	static off_t igevids_size = 0;
	static time_t igevids_mtime = 0;
	long numrows;
	static Dbptr   dbigevids;
	static Dbvalue value;
	struct stat igevids_statbuf;
  
	dbigevids = dblookup( db, 0, "igevids", 0, 0 );
	dbquery( dbigevids, dbRECORD_COUNT, &numrows );
	if ( (long)numrows > (long)0 ) { 
		dbquery( dbigevids, dbTABLE_FILENAME, &value );
		stat( value.t, &igevids_statbuf );

		if( (long) initialized == (long) 0 ) {
			igevids_size = igevids_statbuf.st_size;
			igevids_mtime = igevids_statbuf.st_mtime;
			initialized++;
			/* process on restart */
			return 0;
		}

		if( igevids_size != igevids_statbuf.st_size ||
			igevids_mtime != igevids_statbuf.st_mtime ) {
			igevids_size = igevids_statbuf.st_size;
			igevids_mtime = igevids_statbuf.st_mtime;
			return 1;
		}
	}

	return 0;
}

long database_changed_ignore( Dbptr db )
{
	static int initialized = 0;
	static off_t origin_size = 0;
	static time_t origin_mtime = 0;
	long numrows;
	static Dbptr   dborigin;
	static Dbvalue value;
	struct stat origin_statbuf;

	dborigin = dblookup( db, 0, "origin", 0, 0 );
	dbquery(dborigin, dbRECORD_COUNT, &numrows );
	if ( (long) numrows >  (long) 0 ) { 
		dbquery( dborigin, dbTABLE_FILENAME, &value );
	
		stat( value.t, &origin_statbuf );

		if( ! initialized ) {
			origin_size = origin_statbuf.st_size;
			origin_mtime = origin_statbuf.st_mtime;
			initialized++;
			/* process on restart */
			return 1;
		}
	
		if( origin_size != origin_statbuf.st_size ||
			origin_mtime != origin_statbuf.st_mtime ) {
			origin_size = origin_statbuf.st_size;
			origin_mtime = origin_statbuf.st_mtime;
			return 1;
		}
	}

	return 0;
}

long database_changed( Dbptr db )
{
	static int initialized = 0;
	static off_t origin_size = 0;
	static time_t origin_mtime = 0;
	long numrows;
	static Dbptr   dborigin;
	static Dbvalue value;
	struct stat origin_statbuf;

	dborigin = dblookup( db, 0, "origin", 0, 0 );
	dbquery(dborigin, dbRECORD_COUNT, &numrows );
	if (  (long) numrows > (long) 0 ) { 
		dbquery( dborigin, dbTABLE_FILENAME, &value );
	
		stat( value.t, &origin_statbuf );

		if( ! initialized ) {
			origin_size = origin_statbuf.st_size;
			origin_mtime = origin_statbuf.st_mtime;
			initialized++;
			/* process on restart */
			return 1;
		}
	
		if( origin_size != origin_statbuf.st_size ||
			origin_mtime != origin_statbuf.st_mtime ) {
			origin_size = origin_statbuf.st_size;
			origin_mtime = origin_statbuf.st_mtime;
			return 1;
		}
	}

	return 0;
}
int dborigin_to_qdds(char *, double, double, double, double, double, double, double , long, char *, char *, long , long, long ); 

long usage ( long i, char *message ) {

		fprintf(stderr, "Usage: %s -dbin dbin -dbignore dbignore -dbqdds dbqdds -polldir qdds_polldir_loc [-days days] [-sleeptime seconds ] [-debug] \n", Program_Name );
		switch (i) {
			case 1: fprintf(stderr,"%s is unknown option\n", message); break;
			case 2: fprintf(stderr,"%s \n", message); break;
			case 3: fprintf(stderr,"%s database can not be openned \n", message); break;
			case 4: fprintf(stderr,"%s database can not be re-openned \n", message); break;
		}

		fprintf(stderr,"\t -dbin, input database to monitor\n");
		fprintf(stderr,"\t -dbignore, database to monitor with events that need delete request sent\n");
		fprintf(stderr,"\t -dbqdds, database using the qdds table contained in the qddsaeic1.0 Schema\n");
		fprintf(stderr,"\t -polldir, location to place constructed CUBE files\n");
		fprintf(stderr,"\t -days, number of days to keep active default is 7\n");
		fprintf(stderr,"\t -sleeptime, time period between origin table checks \n");
		fprintf(stderr,"\t -debug, print out debugging information \n");
		exit(0);
}

int main( int argc, char **argv )
{
	char    dir[FILENAME_MAX];
	static char base[FILENAME_MAX];
	Dbptr   dbsum,dborigin,dborigin_ig,dbqdds,dbignore;
	char    db_name[STRSZ], dbqdds_name[STRSZ], dbignore_name[STRSZ];
	int rc;
	long nrecsq, nrecs;
	long sleeptime, i, x, set, set2, dbx;
	char    joinviewname[256];
	char	qtype[3];
	char    auth[16];
	char	polldir[STRSZ];
	double	lddate;
	long debug, maxdays;
	long minepoch; 
	size_t qddsmemsize;
	char psent_no[2];

	double  lat, lon, depth, time, ml, mb, ms;
	long ndef;
	char    review[5];

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	numqind = -1;
	debug = 0; /* 0-> debugging off, 1-> low level debugging on, > 1 more debug messages */

	strcpy (joinviewname, "qddsjoin");
	strcpy (psent_no, "n");

	dirbase( argv[0], dir, base );
	Program_Name = base;

	set2 =  0;
	maxdays = 7; /* default */
	sleeptime = 300; /* default */
	for (x=1; (long) x < (long) argc; ++x) {
		set =  0;
		if( strcmp("-dbin", argv[x]) == 0 ) {
			if ( (long) (x + 1) != (long) argc ) { ++set;++set2; ++x; strcpy(db_name, argv[x]);}
		}
		if( strcmp("-dbignore", argv[x]) == 0 ) {
			if ( (long) (x + 1) != (long) argc ) { ++set;set2=set2+10; ++x; strcpy(dbignore_name, argv[x]);}
		}
		if( strcmp("-dbqdds", argv[x]) == 0 ) {
			if ( (long) (x + 1) != (long) argc ) { ++set;set2=set2+100; ++x; strcpy(dbqdds_name, argv[x]);}
		}
		if( strcmp("-polldir", argv[x]) == 0 ) {
			if ( (long) (x + 1) != (long) argc ) { ++set;set2=set2+1000; ++x; strcpy(polldir, argv[x]);}
		}
		if( strcmp("-days", argv[x]) == 0 ) {
			if ( (long) (x + 1) != (long) argc ) { 
				++set; ++x; 
				maxdays = atoi(argv[x]);
			}
		}
		if( strcmp("-sleeptime", argv[x]) == 0 ) {
			if ( (long) (x + 1) != (long) argc ) { 
				++set; ++x; 
				sleeptime = atoi(argv[x]);
			} 
		}
		if( strcmp("-debug", argv[x]) == 0 ) {
			++set;
			debug = 1;
		}
		if ((long) set == (long) 0) {
			usage (1, argv[x]); 
		}
	}
	if ((long) set2 != (long) 1111) {
		usage ( 2, "Required options were not provided");
	}

	/* open summary database for memory loading */
	if(dbopen( db_name, "r", &dbsum ) != 0 ) {
		usage(3, db_name);
	}
	/* open qddsdb database for memory loading */
	if(dbopen( dbqdds_name, "r+", &dbqdds ) != 0 ) {
		usage(3, dbqdds_name);
	}
	/* open ignoredb database */
	if(dbopen( dbignore_name, "r", &dbignore ) != 0 ) {
		usage(3, dbignore_name);
	}

	/* min epoch */
	minepoch = maxdays * 3600 * 24 ;
	minepoch = yearday(now()-minepoch);
	if((long) debug == (long) 1) {
		fprintf(stderr,"Smallest epoch time allow in qdds database is %ld\n", minepoch);
	}


	/* load qdds database tables into memory */
	dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
	dbquery (dbqdds, dbRECORD_COUNT, &nrecsq);
	/* allocate space for qdds data, if no records must allocate 
		space for realloc used later */
	if ( (long) nrecsq <  (long) 1 ) { i = 1; } else { i = nrecsq; }
	if(!(qi = (struct qddsinfo *) calloc(i, sizeof(struct qddsinfo)))) {
		fprintf(stderr, "Can not alocate memory for qdds db %s\n", dbqdds_name) ;
		exit(1);
	} 
	 
	if ( (long) nrecsq >  (long) 0 ) {
		numqind = nrecsq;
		for(dbqdds.record=0; (long) dbqdds.record < (long) nrecsq; ++dbqdds.record) {
			i = dbqdds.record;
			if(dbgetv(dbqdds, 0, 
				"evid", &(qi[i].evid),
				"orid", &(qi[i].orid),
				"auth", qi[i].auth,
				"jdate", &(qi[i].jdate),
				"vernum", &(qi[i].vernum),
				"psent", qi[i].psent,
				"qtype", qi[i].qtype, 
				"qddscube", qi[i].qddscube,
				"lddate", &(qi[i].lddate), 
				NULL)== dbINVALID) {
				complain(0,"dbgetv problem loadin record=%ld\n", dbqdds.record);
				numqind--;
			} else {
				qi[i].reset = 0;
			}
		}
		if ((long) debug == (long) 1) {
			fprintf(stderr,"%ld records of %ld read in from qdds table into memory\n",
			numqind, nrecsq);
		}	
	}
	/* loop each time summary database is update */
	long dbsumchange, dbignorechanged;
	/* initialize dbsumchange and dbignorechanged */
	dbsumchange = dbignorechanged = -1; 
	while ( 1 ) {
		minepoch = maxdays * 3600 * 24 ;
		minepoch = yearday(now()-minepoch);
		if ((long) debug == (long) 1) {
			fprintf(stderr,"New loop started at %s\n", strtime(now()));
		}
		dbsumchange = database_changed(dbsum);
		dbignorechanged = database_changed_ignore(dbignore);
		if ( (long) dbsumchange == (long) 0 && (long) dbignorechanged == (long) 0 ) {
			/* no change, sleep x seconds and check again */
			sleep(sleeptime);
		} else
		  {
			/* change has occured wait 30 seconds for table update */ 
			sleep(10);
			/* close all data bases */
			if ((long) dbsumchange != (long) 0) {	
				dbclose(dbsum);
				/* reopen all data bases */
				if(dbopen( db_name, "r", &dbsum ) != 0 ) {
					usage(4, db_name);
				}
			}
			if ((long) dbignorechanged != (long) 0) {	
				dbclose(dbignore);
				if(dbopen( dbignore_name, "r", &dbignore ) != 0 ) {
					usage(4, dbignore_name);
				}
			}
			if ((long) debug == (long) 1) {
				/* both databases should change at the same time */
				/* process both*/
				if ((long) dbsumchange == (long) 1) {
					fprintf(stderr,"%s modification detected at %s\n", 
					db_name, strtime(now()));
				}
				if ((long) dbignorechanged == (long) 1) {
					fprintf(stderr,"%s modification detected at %s\n", 
					dbignore_name, strtime(now()));
				}
			}	

			/* process summary database rows */
			if ( (long) numqind > (long) 0 ) {
				for (nrecs = 0; (long) nrecs < (long) numqind; ++nrecs) {
					qi[nrecs].reset = 0;
				}
			}
			/* process dbsum and dbignore databsas */
			for(x = 0; (long) x < (long) 2; ++x ) {  /* x=0 is summarydb, x=1 is ignoredb */
				if( (long) x == (long) 0 ) { /* summary db */
					ignore_db = 0;
					dbeventjn = dblookup( dbsum, 0, "event", 0, 0 );
					dborigin = dblookup( dbsum, 0, "origin", 0, 0 );
				}
				if( (long) x == (long) 1 ) { /* ignoredb db */
					ignore_db = 1;
					dbeventjn_ig = dblookup( dbignore, 0, "event", 0, 0 );
					dborigin_ig = dblookup( dbignore, 0, "origin", 0, 0 );
				}
				if((long) ignore_db == (long) 0) {
					dbquery( dbeventjn, dbRECORD_COUNT, &nrecs );
				} else
				  {
					dbquery( dbeventjn_ig, dbRECORD_COUNT, &nrecs );
				}
				/* process prefor origin rows, if no event table process all origins */
				if ((long) nrecs > (long) 0) {
					if((long) ignore_db == (long) 0) {
						dbeventjn2 = dbjoin(dbeventjn, dborigin, 0, 0, 0, 0, 0);
						dbevent = dbsubset(dbeventjn2, "prefor == orid", joinviewname);
						dbquery( dbevent, dbRECORD_COUNT, &nrecs );
					} else
					  {
						dbeventjn2_ig = dbjoin(dbeventjn_ig, dborigin_ig, 0, 0, 0, 0, 0);
						dbevent_ig = dbsubset(dbeventjn2_ig, "prefor == orid", joinviewname);
						dbquery( dbevent_ig, dbRECORD_COUNT, &nrecs );

					}
					
					for(dbx=0; (long) dbx < (long) nrecs; ++dbx) {
						if((long) ignore_db == (long) 0) {
							dbevent.record = dbx;
						} else
						  {
							dbevent_ig.record = dbx;
						}
						aline[0] = '\0';
        					if((long) x == (long) 0 ) {
                					rc = dbgetv( dbevent, joinviewname, 
								"lat", &lat,
                       						"lon", &lon, "depth", &depth, 
								"time", &time, "ml", &ml,
                       						"mb", &mb, "ms", &ms, 
								"ndef", &ndef, "evid", &evid,
                       						"orid", &orid, "jdate", &jdate, 
								"review", review, "origin.auth", 
								auth, NULL );
								if( rc ) {
					                		fprintf(stderr,"Failed to get info from dbsum database pointer\n");
								}	
        					} else
        					  {
              					  	rc = dbgetv( dbevent_ig, joinviewname, 
								"lat", &lat,
                 					      	"lon", &lon, "depth", &depth, 
								"time", &time, "ml", &ml,
                      						"mb", &mb, "ms", &ms, 
								"ndef", &ndef, "evid", &evid,
                       						"orid", &orid, "jdate", &jdate, 
								"review", review,
                       						"origin.auth", auth, NULL );
								if( rc ) {
					                		fprintf(stderr,"Failed to get info from dbignore database pointer\n");
								}	
        					}
						if( rc ) {
					                rc = -1;
        					} else
						  {
							rc = dborigin_to_qdds(qtype, lat, lon, depth, time, ml, mb, ms, ndef, review, auth, minepoch, debug, x ); /* x=0 sumdb x=1 ignoredb*/
								/* rc == -5 is lat, lon not in alaska, just ignore */
						}
						if (rc == -1) {
							fprintf( stderr, "Skipping message %ld due to conversion failure\n", 
								dbx );
						} else if (rc == 0) { /* good return status */
							/* add new row  */
							lddate = now();
							dbqdds = dblookup( dbqdds, 0, "qdds", 0, 0 );
							dbqdds.record = dbaddnull (dbqdds);
							dbquery (dbqdds, dbRECORD_COUNT, &nrecsq);
							aline[CUBESIZE] = '\0';
							if((long)dbqdds.record < (long)0 || dbputv(dbqdds, 0, 
								"evid", evid, "orid", orid, "auth", auth,
								"jdate", jdate, "vernum", version, "qtype", qtype, 
								"qddscube", aline, "psent", "y", "lddate", lddate, 
								NULL) == dbINVALID) {
									if((long)debug == (long)1) {
										/* complain(0, "Problems adding record %ld evid=%ld, orid=%ld, auth=%s, jdate=%ld, vernum=%ld\n", nrecsq, evid, orid, auth, jdate, version); */
										die(0, "Problems adding record %ld evid=%ld, orid=%ld, auth=%s, jdate=%ld, vernum=%ld\n", nrecsq, evid, orid, auth, jdate, version);
									}
							} else
							  {
								/* realloc memory */
								if((long)numqind == (long)-1) numqind = 0;/* no pre-existind data */
								qddsmemsize = sizeof(struct qddsinfo) * (numqind+1);	
								if ( (long)qddsmemsize > (long)currentqddsmemsize ) {
									currentqddsmemsize = 
									(sizeof(struct qddsinfo) 
									* QDDSBLOCKSIZE) + qddsmemsize;
									if(!(qi = 
										(struct qddsinfo *) realloc((struct qddsinfo *) qi, 
										currentqddsmemsize))) {
										fprintf(stderr, "Can not realloc memory for qdds db %s\n", dbqdds_name) ;
										exit(1);
									} else
									  {
										if((long)debug == (long)1) {
											fprintf(stderr,"%ld bytes of memory has been reallocate \n", currentqddsmemsize);		
										}
									}
								}
								qi[numqind].evid = evid;
								qi[numqind].orid = orid;
								strcpy(qi[numqind].auth, auth);
								qi[numqind].jdate = jdate;
								qi[numqind].vernum = version;
								strcpy(qi[numqind].qtype,qtype); 
								strcpy(qi[numqind].psent,"n"); 
								strncpy(qi[numqind].qddscube,aline,CUBESIZE);
								qi[numqind].lddate = lddate; 
								qi[numqind].reset = 1;
								++numqind;
								if((long)debug > (long)1) { 
									fprintf(stderr,"Added row # %ld to %s\n\tevid=%ld,orid=%ld,jdate=%ld,qtype=%s,version=%ld,auth=%s,\n\taline=%s\n",
									numqind,dbqdds_name, evid, orid, 
									jdate, qtype, version, auth, aline);
								} 
							}
						}
					}
				} else /* if nrecs > 0 */
				  {
					if ((long)debug == (long)1 && (long)x == (long)0) {
						fprintf(stderr, "no event rows in table %s.event\n", dbqdds_name) ;
					}
					if ((long)debug == (long)1 && (long)x == (long)1) {
						fprintf(stderr, "no delete event rows in table %s.event\n", dbignore_name) ;
					}
				}
			} /* for x */
			/* send qdds polldirfiles and update psent table value in qdds table */  
			sendqddspolldir(polldir, dbqdds, debug);
			/* clean qdds database, sync with summary database */
			database_clean_qdds(minepoch, debug, dbqdds, polldir) ; 
		}
	}
}
