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

int usage ( int i, char *message ) {
	fprintf(stderr, "Usage: %s -db db [-exec script] [-sleep second] [-onerun]\n", Program_Name );
	switch (i) {
		case 1:fprintf(stderr,"%s is unknown option\n", message); break;
		case 2: fprintf(stderr,"%s \n", message); break;
		case 3: fprintf(stderr,"%s database can not be openned \n", message); break;
	}
	exit(1); 
}

int monitor_database_changed( char *db_name )
{
	static int initialize = 1;
	Dbptr db;
	Dbptr dbevent, dbshakeversion;
	Dbvalue value_ev, value_sh;
	static struct stat tevent, tshake;
	struct stat table_statbuf_ev, table_statbuf_sh;
	int nrows, changed;

	
	if (initialize == 1) {
		initialize = 0;
		tevent.st_size = (off_t) 0;
		tevent.st_mtime = (time_t) 0;
		tshake.st_size = (off_t) 0;
		tshake.st_mtime = (time_t) 0;
	}

	changed = 0; 
	if(dbopen( db_name, "r", &db) != 0) {
		fprintf( stderr, "database_changed: Error opening input database %s\n", db_name);
		return(changed);
	} else
	  {
		/* Start event table  ***********************/
		dbevent = dblookup( db, 0, "event", 0, 0 );
		dbquery(dbevent, dbRECORD_COUNT, &nrows );
		if ( nrows > 0 ) {
			dbquery( dbevent,dbTABLE_FILENAME,&value_ev);
			stat( value_ev.t, &table_statbuf_ev);
			if((off_t)tevent.st_size !=  
			   (off_t)table_statbuf_ev.st_size || 
			   (time_t)tevent.st_mtime < 
			   (time_t) table_statbuf_ev.st_mtime) {
			   changed = 1;
			   	tevent.st_size = 
				  (off_t)table_statbuf_ev.st_size;
			   	tevent.st_mtime = 
				  (time_t) table_statbuf_ev.st_mtime;
			}
		}
		/* Start shakeversion table  ***********************/
		dbshakeversion = dblookup( db, 0, "shakeversion", 0, 0 );
		dbquery(dbshakeversion, dbRECORD_COUNT, &nrows );
		if ( nrows > 0 ) {
			dbquery(dbshakeversion,
				dbTABLE_FILENAME,&value_sh);
			stat( value_sh.t, &table_statbuf_sh);
			if((off_t)tshake.st_size !=  
			   (off_t)table_statbuf_sh.st_size || 
			   (time_t)tshake.st_mtime < 
			   (time_t) table_statbuf_sh.st_mtime) {
			   changed = 1;
			   	tshake.st_size = 
				  (off_t)table_statbuf_sh.st_size;
			   	tshake.st_mtime = 
				  (time_t) table_statbuf_sh.st_mtime;
			}
		}
		dbclose(db);
		return(changed);
	}
}
struct EventTable {
	int evid, prefor, commid;
	char evname[16], auth[16];
	double lddate;
};

int get_event(Dbptr dbin, int i, struct EventTable *tb) {
	if(dbgetv(dbin, 0,
		"evid", &(tb[i].evid), "prefor", &(tb[i].prefor),
		"commid", &(tb[i].commid), "evname", tb[i].evname,
		"auth", tb[i].auth,
		"lddate", &(tb[i].lddate), 0) == dbINVALID) {
			fprintf(stderr,"dbgetv problem loading event record=%d\n", i);
		return (-1);
	} else
	  {
		tb[i].evname[15]=tb[i].auth[15]='\0';
		return(0);
	}
}

struct ShakeversionTable {
	int evid, mysqlvernum;
	char mysqlcomment[256];
	double lddate;
};

int get_shakeversion(Dbptr dbin, int i, struct ShakeversionTable *tb) {
	if(dbgetv(dbin, 0,
		"evid", &(tb[i].evid),
		"mysqlvernum", &(tb[i].mysqlvernum),
		"mysqlcomment", tb[i].mysqlcomment,
		"lddate", &(tb[i].lddate), 0) == dbINVALID) {
			fprintf(stderr,"dbgetv problem loading shakeversion record=%d\n", i);
		return (-1);
	} else
	  {
		tb[i].mysqlcomment[255]='\0';
		return(0);
	}
}
struct EventpipeTable {
	int evid ;
	int shakemap;
	double lddate;

	int reset; /* 0-null row, 1-found or update */ 
};

int get_eventpipe(Dbptr dbin, int i, struct EventpipeTable *tb) {
	if(dbgetv(dbin, 0,
		"evid", &(tb[i].evid), 
		"shakemap", &(tb[i].shakemap),
		"lddate", &(tb[i].lddate), 0) == dbINVALID) {
			fprintf(stderr,"dbgetv problem loading eventpipe record=%d\n", i);
		return (-1);
	} else
	  {
		tb[i].reset = 0;
		return(0);
	}
}


int main( int argc, char **argv )
{
	int whileloop, onerun, inputchange, sleeptime;
	int set1, set2, x, xx, xxx;
	char    dir[FILENAME_MAX];
	static char base[FILENAME_MAX];
	char db_name[STRSZ];
	Dbptr db, dbev, dbsh, dbep;
	struct EventTable *ev;
	struct ShakeversionTable *sh;
	struct EventpipeTable *ep, new_ep;
	int nrows_ev, nrows_sh, nrows_ep, nrows_ep_add;
	int cur_mysqlvernum, found, changed, crunch, execscr, pid, statusp;
	char execpro[STRSZ];
	char *forkprogargs[3];

	execscr = 0;

	dirbase( argv[0], dir, base );
	Program_Name = base;

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	onerun = 0;
	sleeptime = 5; /* 5 seconds */
	/* argument list */
	set1 = 0;
	for (x=1; x < argc; ++x) {
		set2 = 0;
		if( strcmp("-db", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				++set1;++set2;
				(void) strcpy(db_name,argv[x]);
			}
		}
		if( strcmp("-sleep", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				++set2;
				sleeptime = atoi(argv[x]);
			}
		}
		if( strcmp("-onerun", argv[x]) == 0 ) { 
			onerun = 1;
			++set2;
		}
		if( strcmp("-exec", argv[x]) == 0 ) {
			++x;
			if (x < argc) {
				++set2;
				execscr = 1;
				(void) strcpy(execpro,argv[x]);
			}
		}
		if (set2 == 0) {
			usage (1, argv[x]);
		}
	}
	if (set1 != 1 ) {
		usage ( 2, "Required option -db was not provided");
	}
	
	/* open  database */
	if(dbopen( db_name, "r+", &db ) != 0 ) {	
		usage ( 3, db_name);
	} else
	  {
		dbclose(db);
	}

	whileloop = 1;
	while(whileloop == 1) {
		if(onerun == 1) whileloop = 0;
		inputchange = 0;
		while (inputchange == 0) {
			sleep(sleeptime);
			inputchange = monitor_database_changed(db_name);
			 if(inputchange == 1) {
				fprintf(stderr,"modification detected at %s \n", strtime(now()));
			}
		}
		/* open database */
		if(dbopen( db_name, "r+", &db ) == 0 ) {	
			/* load event table information */
			dbev = dblookup(db, 0, "event", 0, 0 );
			dbquery (dbev, dbRECORD_COUNT, &nrows_ev);
			if( nrows_ev > 0) {
				ev = (struct EventTable *) calloc(nrows_ev, sizeof(struct EventTable ));
				for(dbev.record = 0; dbev.record < nrows_ev; ++dbev.record) {
					get_event(dbev, dbev.record, (struct EventTable *)ev);
				} 
			}
			
			/* load shakeverion table infomation */
			dbsh = dblookup(db, 0, "shakeversion", 0, 0 );
			dbquery (dbsh, dbRECORD_COUNT, &nrows_sh);
			if( nrows_sh > 0) {
				sh = (struct ShakeversionTable *) calloc(nrows_sh, sizeof(struct ShakeversionTable )); 
				for(dbsh.record = 0; dbsh.record < nrows_sh; ++dbsh.record) {
					get_shakeversion(dbsh, dbsh.record, (struct ShakeversionTable *)sh);
				} 
			}
			/* load eventpipe table information */
			dbep = dblookup(db, 0, "eventpipe", 0, 0 );
			dbquery (dbep, dbRECORD_COUNT, &nrows_ep);
			if( nrows_ep > 0) {
				ep = (struct EventpipeTable *) calloc(nrows_ep, sizeof(struct EventpipeTable)); 
				for(dbep.record = 0; dbep.record < nrows_ep; ++dbep.record) {
					get_eventpipe(dbep, dbep.record, (struct EventpipeTable *)ep);
				} 
			}
			/* update eventpipe table information */
			for( x = 0; x < nrows_ev; ++x ) {
			    if(ev[x].evid != -1) {
				/* init eventpipe values */
				new_ep.evid = ev[x].evid;
				new_ep.shakemap = 0;
				cur_mysqlvernum = -1;

				/* scan shake version */
				for(xx = 0; xx < nrows_sh; ++xx) {
					if(ev[x].evid == sh[xx].evid && sh[xx].mysqlvernum > cur_mysqlvernum) {
						cur_mysqlvernum = sh[xx].mysqlvernum;
						if(strncmp(sh[xx].mysqlcomment,"Event cancelled.",16) == 0) {
							new_ep.shakemap = 0; /* cancelled */
						} else
						  {
							new_ep.shakemap = cur_mysqlvernum;
						}
						new_ep.evid = ev[x].evid;
						new_ep.lddate = (double) now ();

					}
				}
				/* scan event pipe */
				found = 0; 
				/* scan eventpipe version */
				for(xxx = 0; xxx < nrows_ep; ++xxx) {
					/* found */
					if(new_ep.evid == ep[xxx].evid) {
						found = 1;
						ep[xxx].reset = 1; 
						/* found update row */
						if( (int) new_ep.shakemap > (int) ep[xxx].shakemap ) {
							dbep.record = xxx;
							if(dbputv(dbep, 0, "evid", new_ep.evid, 
								"shakemap", new_ep.shakemap,
								"lddate", new_ep.lddate, 0) == dbINVALID) {
								fprintf(stderr,
								  "Can not update evid=%d\n",new_ep.evid);
							}

						}
						xxx = nrows_ep;
					}
				}
				/* add new row if row not found or updated */	
				if(found == 0) {
					dbep.record = dbaddnull (dbep);
					if(dbep.record < 0 || dbputv(dbep, 0, 
						"evid", new_ep.evid, 
						"shakemap", new_ep.shakemap,
						"lddate", new_ep.lddate, 0) == dbINVALID) {
							fprintf(stderr,
							  "Can not update evid=%d\n",new_ep.evid);
					}
				}
			    }    
			}
			/* remove deleted event rows */
			crunch = 0; 
			for(xxx = 0; xxx < nrows_ep; ++xxx) {
				if(ep[xxx].reset == 0) {
					crunch = 1; 
					dbep.record = xxx;
					if(dbmark (dbep) != 0){
						fprintf(stderr,"Can not dbmark eventpipe row %d, evid=%d\n",
							dbep.record, ep[xxx].evid);
					}
				} 
			}
			/* crunch marked rows */
			if(crunch == 1) {
				if(dbcrunch(dbep) != 0) {
					fprintf(stderr,"Can not dbcrunch null rows \n");
				}
			}
			/* dbclose */
			dbclose(db);
 
			/* free up memory */
			if(nrows_ev > 0) free(ev);
			if(nrows_sh > 0) free(sh);
			if(nrows_ep > 0) free(ep);
		} /* dbopen */
		dbclose(db);
		/* created xml file from script */
		if(execscr == 1) {
			forkprogargs[0] = execpro;
			forkprogargs[1] = execpro;
			forkprogargs[2] = NULL;
			pid = fork();
			if (pid == 0) {
				execvp(forkprogargs[0], &forkprogargs[1]);
			} else if (pid == -1) {
				fprintf(stderr,"Can not fork %s \n", forkprogargs[0]);
			/* parent */
			} else if (pid > 0) {
				(void) wait(&statusp);
			}
		} 
	}
}
