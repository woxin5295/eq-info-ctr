/* create_ignore_row */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"

int usage (int i, char *opt) {
	switch (i) {
		case 1: 
			fprintf(stderr,"%s is unknown option\n", opt); 
		break;
		case 2: 
			fprintf(stderr,"%s \n", opt); 
		break;
	}
	fprintf(stderr, "Usage: %s {-db db -evid evid -jdate jdate \n\t[-whoig whoig] [-add add] [-auth auth] [-minjdate jdate] | -db db -add add -minjdate minjdate }  \n", Program_Name );
	fprintf(stderr, "\t db- database name \n");	
	fprintf(stderr, "\t evid- event id \n");	
	fprintf(stderr, "\t whoig- who ignores this id, default all \n");	
	fprintf(stderr, "\t jdate- jdate of evid \n");	
	fprintf(stderr, "\t add- add=1 add new row, add=0 remove row and dbmark, default 1 \n");	
	fprintf(stderr, "\t\t add=3 do not add row but process minjdate option \n");	
	exit(1);
}
int main( int argc, char **argv )
{
	static char base[FILENAME_MAX];
	char    dir[FILENAME_MAX];
	char    db_name[STRSZ];
	int debug;
	int add, crunch; 
	long evid, get_evid, jdate, get_jdate, minjdate, nrecs;
	int x, set;
	int found;
	char whoig[21], get_whoig[21],  auth[16], get_auth[16], *user;
	double lddate;
	Dbptr db;

	db_name[0] = '\0';
	add = 1;
	(void) strcpy(whoig,"all");
	evid = -1;
	jdate = -1;
	debug = 1;
	crunch = 0;
	auth[0] = '\0';
	minjdate = 0;	

	dirbase( argv[0], dir, base );
	Program_Name = base;
	user = getenv("USER");

	for (x=1; x < argc; ++x) {
		set =  0;
		if( strcmp("-db", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set; ++x; (void) strcpy(db_name, argv[x]);}
		}
		if( strcmp("-evid", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++x; evid = (long) atoi(argv[x]);}
		}
		if( strcmp("-whoig", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++x; (void) strcpy(whoig, argv[x]);}
		}
		if( strcmp("-jdate", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++x; jdate = (long) atoi(argv[x]);}
		}
		if( strcmp("-add", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++x; add = atoi(argv[x]);}
		}
		if( strcmp("-auth", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++x; (void) strcpy(auth, argv[x]);}
		}
		if( strcmp("-minjdate", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set;++x; minjdate = (long) atoi(argv[x]);}
		}
		if(set == 0) {
			usage(1, argv[x]);
		}
	}
	if(db_name[0] == '\0' ) {
		usage(2, "No Database Provided");
	}
	if (add != 3) {
		if( (long) evid == (long) -1) usage(2, "evid Not Provided");
		if( (long) jdate == (long) -1) usage(2, "jdate Not Provided");
	} 
	if (add == 3) {
		if((long) minjdate == (long) 0) usage(2, "minjdate Not Provided");
	}
	if(auth[0] == '\0') { (void) strcpy(auth, user); }

	whoig[20]=get_whoig[20]=auth[15]=get_auth[15]='\0';
	

	/* open database */
	if(dbopen( db_name, "r+", &db ) != 0 ) {
		fprintf( stderr, "Error openning database %s\n", db_name) ;
		exit(1);
	}

	lddate = now ();
	db = dblookup( db, 0, "ignorerow", 0, 0 );
	dbquery (db, dbRECORD_COUNT, &nrecs);
	if ( (long) nrecs > (long) 0 && add != 3 ) {
		found = -1;
		for(db.record = 0; (long) db.record < (long) nrecs; ++db.record) {
			/* get row */
			if(dbgetv(db, 0,
				"evid", &get_evid,
				"whoig", get_whoig,
				"jdate", &get_jdate,
				"auth", get_auth,
				NULL)== dbINVALID) {
					fprintf(stderr,"can not dbgetv record %ld\n", db.record);
			}		
			if( (long) evid == (long) get_evid && strcmp(whoig, get_whoig) == 0 &&
			  (long) jdate == (long) get_jdate) {
				found = db.record;
				/* mark record if add == 0 */
				if( add == 0) {
					if(dbmark (db) != 0){
						fprintf(stderr, "can not dbmark %ld, %s, %ld, %s\n",
							get_evid, get_whoig, get_jdate, get_auth); 	
					}
					crunch = 1;
				}
				db.record = nrecs;
			} 
		}
		if (found == -1 && add == 1) {
			/* Add new row  */
			db.record = dbaddnull (db); 
		}
		if (found != -1 && add == 1) { 
			/* update row  */
			db.record = found;
		}
		if (add == 1 ) {
			if(db.record < 0 || dbputv(db, 0,
				"evid", evid,
				"whoig", whoig,
				"jdate", jdate,
				"auth", auth, NULL)== dbINVALID) {
				fprintf(stderr, "can not dbputv %ld, %s, %ld, %s\n",
					get_evid, get_whoig, get_jdate, get_auth); 	
			}
		}
	} /* add != 3 */
	/* first row */
	dbquery (db, dbRECORD_COUNT, &nrecs);
	if ( (long) nrecs == (long) 0 && add == 1 ) {
		db.record = dbaddnull (db);
		if(db.record < 0 || dbputv(db, 0,
			"evid", evid, "whoig", whoig, "jdate", jdate,
			"auth", auth, NULL)== dbINVALID) {
			fprintf(stderr, "can not dbputv first record %ld, %ld %s, %ld, %s\n",
				db.record, evid, whoig, jdate, auth); 	
		}
		
	}
	if( (long) minjdate > (long) 0 ) {
		dbquery (db, dbRECORD_COUNT, &nrecs);
		if ( (long) nrecs > (long) 0 ) {
			/* dbmark rows that are older the minjdate */
			for(db.record = 0; (long) db.record < (long) nrecs; ++db.record) {
				/* get row */
				if(dbgetv(db, 0, "jdate", &get_jdate, NULL) == dbINVALID) {
					fprintf(stderr,"can not dbgetv:2 record %ld\n", db.record);
				}
				if( (long) jdate < (long) minjdate) {
					if(dbmark (db) != 0){
						fprintf(stderr, "can not dbmark record %ld\n", db.record);
					}
					crunch = 1; 
				} 
			}
		}
	}
	/* dbcrunch rows if needed */
	if (crunch == 1) {
		dbcrunch(db);
	}
	dbclose(db);
}
