/* create_pipe_event  */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"
extern char *fgets();
extern pid_t wait();

extern char *strcpy();

int usage ( int i, char *message, double defmag ) {
	fprintf(stdout,"%f", defmag);
	fprintf(stderr, "Usage: %s -db db -ev evid -pr prefor [ -defmag mag -p parameter_file] \n", Program_Name );
	switch (i) {
		case 1:fprintf(stderr,"%s is unknown option or missing argument\n", message); break;
		case 2: fprintf(stderr,"%s \n", message); break;
		case 3: fprintf(stderr,"%s database can not be openned \n", message); break;
	}
	exit(1); 
}

struct NetmagTable {
	long magid, orid, evid, nsta, commid;
	double magnitude, uncertainty;
	char net[9], magtype[7], auth[16];
	double lddate;
};

int get_netmag(Dbptr dbin, int i, struct NetmagTable *tb) {
	if(dbgetv(dbin, 0,
		"magid", &(tb[i].magid), 
		"orid", &(tb[i].orid), 
		"evid", &(tb[i].evid), 
		"nsta", &(tb[i].nsta), 
		"commid", &(tb[i].commid), 
		"magnitude", &(tb[i].magnitude), 
		"uncertainty", &(tb[i].uncertainty),
		"net", tb[i].net, "magtype", tb[i].magtype, 
		"auth", tb[i].auth,
		"lddate", &(tb[i].lddate), 0) == dbINVALID) {
			fprintf(stderr,"dbgetv problem loading netmag record=%d\n", i);
		return (-1);
	} else
	  {
		tb[i].net[8]=tb[i].magtype[6]=tb[i].auth[15]='\0';
		return(0);
	}
}
void get_auth_magtype(char *s, char *auth, char *magtype) {
	/* antelope netmag.auth has max length of 15 characters */
	/* antelope netmag.magtype has max length of 6 characters */
	int x, y, z, w;
	/* get auth */
	y = strlen(s);
	for (x = 0; x < y; ++x ) {
		/* problem */
		if ( x == 16 && (s[x] != '\t' && s[x] != ' ' && s[x] != '\0')) {
			(void) strcpy(magtype, "XX"); auth[16] = '\0'; return;
		}
		if ( s[x] == ' ' || s[x] == '\t') {
			auth[x] = '\0';
			while((s[x] == '\t' || s[x] == ' ') && x < y ) {
				++x;
			}
			z = x; x = y;
		} else
		  {
			auth[x] = s[x];
			auth[x+1] = '\0'; 
		}
	}

	/* get magtype */
	w = 0;
	for (x = z; x < y; ++x ) {
		++w;
		/* problem */
		if ( w >= 7 && (s[x] != '\t' && s[x] != ' ' && s[x] != '\0')) {
			(void) strcpy(magtype, "XX"); auth[16] = '\0'; return;	
		}
		if ( s[x] == '\t' || s[x] == ' ' || s[x] == '\0' || x == y) {
			magtype[w-1] = '\0';
			return;
		} else
		  {
			magtype[w-1] = s[x];
			magtype[w] = '\0';
		}
	}
}
int main( int argc, char **argv )
{
	char    dir[FILENAME_MAX], *s, auth[16], magtype[7];
	static char base[FILENAME_MAX];
	char db_name[STRSZ],paramfile[STRSZ], expr[STRSZ];
	Dbptr db, dbnm;
	struct NetmagTable *nm;
	int set1, set2, i;
	long evid, prefor, premagid;
	long nrows_nm, x;
	int preforfound; 
	long curorid;
	Pf *pf = 0, *pf1 ;
	double defmag;

	defmag = -99.99;

	dirbase( argv[0], dir, base );
	Program_Name = base;

	(void) strcpy(paramfile, Program_Name);
	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	set1 = 0;
	for (x=1; x < argc; ++x) {
		set2 = 0;
		if( strcmp("-db", argv[x]) == 0 ) { 
			++x;
			if ((long) x < (long) argc) { 
				++set1;++set2;
				(void) strcpy(db_name,argv[x]);
			}
		}
		if( strcmp("-ev", argv[x]) == 0 ) { 
			++x;
			if ((long) x < (long) argc) { 
				++set1;++set2;
				evid = (long) atoi(argv[x]);
			}
		}
		if( strcmp("-pr", argv[x]) == 0 ) { 
			++x;
			if ((long) x < (long) argc) { 
				++set1;++set2;
				prefor = (long) atoi(argv[x]);
			}
		}
		if( strcmp("-defmag", argv[x]) == 0 ) { 
			++x;
			if ((long) x < (long) argc) { 
				++set2;
				defmag = (double) atof(argv[x]);
			}
		}
		if( strcmp("-p", argv[x]) == 0 ) { 
			++x;
			if ((long) x < (long) argc) { 
				++set2;
				(void) strcpy(paramfile, argv[x]);
			}
		}
		if (set2 == 0) {
			usage (1, argv[x], defmag);
		}
	}
	if (set1 != 3 ) {
		usage ( 2, "Required options not provided", defmag);
	}
	
	/* open  database */
	if(dbopen( db_name, "r+", &db ) != 0 ) {	
		usage ( 3, db_name, defmag);
	}
 
	/* load netmag table information */
	dbnm = dblookup(db, 0, "netmag", 0, 0 );
	dbquery (dbnm, dbRECORD_COUNT, &nrows_nm);
	(void) sprintf(expr, "(evid == \"%d\")", evid);
	dbnm = dbsubset(dbnm, expr, 0);
	dbquery (dbnm, dbRECORD_COUNT, &nrows_nm);
	if( (long) nrows_nm > (long) 0) {
		nm = (struct NetmagTable *) calloc(nrows_nm, sizeof(struct NetmagTable ));
		for(dbnm.record = 0; (long) dbnm.record < (long) nrows_nm; ++dbnm.record) {
			get_netmag(dbnm, dbnm.record, (struct NetmagTable *)nm);
		} 
	}
	if ( pfread ( paramfile, &pf ) != 0 )
		usage ( 2, "pfread finds no parameter file", defmag);

	switch ( pfget(pf, "authpref", (void **) &pf1 ) ) {
		case PFTBL:
			i = 0;
			preforfound = 0;
			while((s = pfget_string ( pf1, (char * ) i ) ) != 0) {
				++i;
				if(strlen(s) != 0) { 
					(void) get_auth_magtype(s, auth, magtype);
					/* find biggest orid or prefor for authpref */
					curorid = -1;
					for(x = 0; (long) x < (long) nrows_nm; ++x) {
						if ((long) nm[x].orid == (long) prefor && preforfound == 0 ) {
							premagid = nm[x].magid;
							x = nrows_nm;
							preforfound = 1;
						}
	 
						if (strcmp(auth,nm[x].auth) == 0 && 
						    strcmp(magtype,nm[x].magtype) == 0  
						    && preforfound == 1 ) {
							if ((long) nm[x].orid > (long) curorid ) {
								if(nm[x].magnitude != -99.99 ) {
									prefor = nm[x].orid;
									premagid = nm[x].magid;
								}	
							} 
						}
					}
				}
				free(s);
			}

			break ;
		default:
			break ;
	}
				
	/* int magid, orid, evid, nsta, commid; */
	/* double magnitude, uncertainty; */
	/* char net[9], magtype[7], auth[16]; */
	if (preforfound == 0 ) {
		fprintf(stdout,"%f",defmag);
	} else
	  {
		for(x = 0; (long) x <(long) nrows_nm; ++x) {
			if ((long) nm[x].orid == (long) prefor && (long) nm[x].magid == (long) premagid ) {
				fprintf(stdout,"%f",nm[x].magnitude);
			}
		}
	}
	if((long) nrows_nm > (long) 0) free(nm);	
	/* close db*/
	dbclose(db);
}
