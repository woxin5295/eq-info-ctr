#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <mysql.h>

#include "Pkt.h"
#include "coords.h"
#include "db.h"
#include "stock.h"
#include "brttutil.h"

struct par_info {
	int debug;
	char *mysql_hosts, *mysql_user, *mysql_pass, *mysql_database;
	char *eventdb;
	int eventtimeout, watchdog_sec, origintime_cutoff;
};

struct par_info param_info;
int debug = 0;

int get_param_file_info(char *pfname, int debug) {
	static Pf *pf = 0;
	
	/* get parmeter file information */
	if ( pfread ( pfname, &pf ) != 0 ) {
		complain ( 0, "Error in parameter file: %s\n, exit", pfname ) ;
		exit(1);
	}

	if (parse_param (pf, "debug", P_LINT, 1, &param_info.debug) < 0) {
		param_info.debug = 0;
	}
	/* set in command line */
	if(debug == 1 ) param_info.debug = 1;
	if (param_info.debug > 1) param_info.debug = 1;

	param_info.mysql_hosts = pfget_string ( pf, "mysql_hosts" );
	if (param_info.mysql_hosts == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_hosts) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires mysql_hosts\n");
		exit(1);
	}
	param_info.mysql_user = pfget_string ( pf, "mysql_user" );
	if (param_info.mysql_user == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_user) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires mysql_user\n");
		exit(1);
	}
	param_info.mysql_pass = pfget_string ( pf, "mysql_pass" );
	if (param_info.mysql_pass == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_pass) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires mysql_pass\n");
		exit(1);
	}
	param_info.mysql_database = pfget_string ( pf, "mysql_database" );
	if (param_info.mysql_database == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_database) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires mysql_database\n");
		exit(1);
	}
	param_info.eventdb = pfget_string ( pf, "eventdb" );
	if (param_info.eventdb == NULL) {
		register_error (0, "parse_pf: pfget_string(eventdb) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires eventdb\n");
		exit(1);
	}
	if (parse_param (pf, "eventtimeout", P_LINT, 1, &param_info.eventtimeout) < 0) {
		param_info.eventtimeout = 300; /* default five minutes */
	} else
	  {
		if(param_info.eventtimeout < 1) {
			param_info.eventtimeout = 300;
		}
	}
	if (parse_param (pf, "watchdog_sec", P_LINT, 1, &param_info.watchdog_sec) < 0) {
		param_info.watchdog_sec = 5; /* five seconds */
	} else
	  {
		if(param_info.watchdog_sec < 1) {
			param_info.watchdog_sec = 5;
		}
	}
	if (parse_param (pf, "origintime_cutoff", P_LINT, 1, &param_info.origintime_cutoff) < 0) {
		param_info.origintime_cutoff = 3600; /* 1 hour */
	} else
	  {
		if(param_info.watchdog_sec < 1) {
			param_info.origintime_cutoff = 3600;
		}
	}
}		
int database_changed( char *dbname, int *passevid, int *prefor, double *lddate, double *origintime ) {
        int     tries, evid, present, numrows, nrecs;
	static int initialized = 0;
        static off_t event_size = 0;
        static time_t event_mtime = 0;
        static Dbptr   db, dbevent, dbeventj, dbeventjs, dborigin;
        static Dbvalue value;
        struct stat event_statbuf;
	char    joinviewname[256], *s1,*s2,*s3,*s4;
	double origin_cutoff;
	static Tbl  *sortkeys;

	/* open database */
	if(dbopen( dbname, "r", &db) != 0) {
		fprintf( stderr, "database_changed:Can not open database %s \n", dbname);
	}

	origin_cutoff = (double) now() - (double)(1.0 * param_info.origintime_cutoff); 

	(void) strcpy (joinviewname, "eventdbprefor");

        dbevent = dblookup( db, 0, "event", 0, 0 );
        dbquery(dbevent, dbRECORD_COUNT, &numrows );
        if ( numrows > 0 ) {
                dbquery( dbevent, dbTABLE_FILENAME, &value );

                stat( value.t, &event_statbuf );

		if( ! initialized ) {
                       	event_size = event_statbuf.st_size;
                       	event_mtime = event_statbuf.st_mtime;
                       	initialized++;
                       	/* process on restart */
			dbclose(db);
                       	return -1;
               	}

 		if( (off_t) event_size != (off_t) event_statbuf.st_size ||
                       	(time_t) event_mtime != (time_t) event_statbuf.st_mtime ) {
			if(debug == 1) {
				fprintf(stderr,"Event table modified at %s ",s1=strtime(now()));
				free(s1);
				if((off_t) event_size != (off_t) event_statbuf.st_size) {
					fprintf(stderr,"size changed, ");
				}
				if((time_t) event_mtime != (time_t) event_statbuf.st_mtime ) {
					fprintf(stderr,"mtime changed, ");
				}
				fprintf(stderr,"\n");
			}
                       	event_size = (off_t) event_statbuf.st_size;
                       	event_mtime = (time_t) event_statbuf.st_mtime;
				
			/* get last event id */
			evid = -2;
			nrecs = 0;
			tries = 0;
			/*  poll database at least five times with 3 second delays between polls */
			/*  event table might be written before origin table */
			while (nrecs < 1 && tries != 5) { 
				/* open new database to get information */
				dborigin = dblookup( db, 0, "origin", 0, 0 );
				dbeventj = dbjoin(dbevent, dborigin, 0, 0, 0, 0, 0);
				dbeventj = dbsubset(dbeventj, "evid != -1 && prefor == orid", joinviewname);
				sortkeys = strtbl("evid", 0 ) ;
				dbeventjs = dbsort(dbeventj,sortkeys,0,0);
				freetbl(sortkeys, 0);
				dbquery( dbeventjs, dbRECORD_COUNT, &nrecs );
				if(nrecs > 0 ) {
					dbeventjs.record = nrecs-1;
					if(dbgetv(dbeventjs, 0, "evid", passevid,
						"prefor", prefor, "lddate", lddate, 
						"time", origintime, 0)) {
						dbclose(db);
						fprintf(stderr,"can not get last event id at record = %d\n", numrows-1 );
					} else
					  {
						dbclose(db);
						if(debug == 1) {
							fprintf(stderr,"%s:ev=%d,pre=%d,ld=%s,\n\torigintime=%s,cutoff=%s\n", s1 = strtime(now()) , *passevid, *prefor, s2 = strtime(*lddate), s3 = strtime(*origintime), s4 = strtime(origin_cutoff));
							free(s1), free(s2), free(s3), free(s4);
						}
						if((double) *origintime > (double) origin_cutoff) {
							evid = *passevid;
						} else
						  {
							if(debug == 1) {
								fprintf(stderr,"%s:ev=%d,pre=%d,ld=%s,\n\t origintime=%s less than cutoff=%s\n", s1 = strtime(now()) , *passevid, *prefor, s2 = strtime(*lddate), s3 = strtime(*origintime), s4 = strtime(origin_cutoff));
								free(s1), free(s2), free(s3), free(s4);

							}
							return -1;
						}
					}
       		                	return evid;
				}
				dbclose(db);
				/* polling */
				if (nrecs < 1 ) {
					sleep(3);
					++tries;
					if(tries == 5) {
						fprintf(stderr,"%s: Event and Origin table can not be joined after 5 tries\n", s1 = strtime(now()));
						free(s1); 
					} else
					  {
						if(dbopen( dbname, "r", &db) != 0) {
							fprintf( stderr, "database_changed:Can not open database %s \n", dbname);
						}
					}
				}
			}
               	} else /* mtime and size have not changed */
		  {
			dbclose(db);
        		return -1;
		}
       	} else
	  {
		if(debug == 1) {
			fprintf(stderr,"No event rows present\n");
		}
		dbclose(db);
        	return -1;
	}
}

 
int main (int argc, char **argv)
{
        int     evid, prefor;
	double lddate, origintime;

	char dbname[STRSZ];
	/* open antelope event database */
	Dbptr db, dbevent;
	int dbeventchange;
	int new_evid, old_evid = -1, event_bit = 0;
	double eventdetectedat, eventtimeout;
	char *ss, *s1, *s2;
	int second;
	static char update_string[1024];
        static MYSQL_RES *result;
        static MYSQL_ROW row;
        static MYSQL *connection, mysql;
        static int state;
	char pfname[STRSZ];
	int x;

	Program_Name = argv[0];

	/* default parameter file name */
	(void) strcpy(pfname, "pipelinematrix.pf");

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	/* get command line arguments */
	for (x=1; x < argc; ++x) {
		if( strcmp("-pf", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) {
				++x;
				(void) strcpy(pfname, argv[x]);
			}
		}
		if( strcmp("-debug", argv[x]) == 0 ) {
			debug = 1;
		}
	}	
	int leave_loop = 0;

	/* get parameter file information */
	get_param_file_info(pfname, debug);

	(void) strcpy(dbname, param_info.eventdb);
	eventtimeout = (double) 1.0 * param_info.eventtimeout; /* default 5 minutes */
 
	/* loop monitoring event table */
	eventdetectedat = now() - (2.0 * eventtimeout);
	while(leave_loop == 0) {
		dbeventchange = database_changed(dbname, (int *) &evid, (int *) &prefor, 
			(double *) &lddate, (double *) &origintime);	
		sleep(param_info.watchdog_sec);
		if(dbeventchange > -1 ) {
			new_evid = dbeventchange;
			/* lets assume new event ids always have greater event id */
			/* than older events, this avoids delayed update from analyst  */
			if (new_evid > old_evid) {
				/* set mysql event bit */
				eventdetectedat = now();
				old_evid = new_evid;
				event_bit = 1;
				fprintf(stderr,"%s: event_bit = %d, second = %d, evid=%d, prefor=%d,\n\t lddate=%s, origintime=%s\n", ss = strtime(now()) ,event_bit, second, evid, prefor, s1 = strtime(lddate), s2 = strtime(origintime));
				free(s1); free(s2);
			}
		}
		/* unset event bit */
		if( (double) (now() - eventdetectedat) > (double) eventtimeout) {
			if( event_bit == 1 ) {
				fprintf(stderr,"%s: event_bit = 0, second = %d\n", 
					ss = strtime(now()) , second);
			}
			event_bit = 0;
		}
		/* get current seconds */
		ss = epoch2str(now(), "%S");
		second = atoi(ss);
		free(ss);
		/* update mysql events table */
		if(!mysql_init(&mysql)) {
			fprintf(stderr,"Error initializing MySQL client \n");
			exit(1);
		}
		/* connect to the mySQL database */
		connection = mysql_real_connect(&mysql, 
			param_info.mysql_hosts, param_info.mysql_user,
			param_info.mysql_pass, param_info.mysql_database,
			0, 0, 0);
		if( connection == NULL ) {
			fprintf(stderr,"Can not connect to mysql database at %s, exiting in 10 seconds \n", 
				ss = strtime(now()));
			free(ss);
			/* for mysql server to restart */
			exit(1); /* let rtexec restart */
		}
		(void) sprintf(update_string,"update events set event_detected = %d, event_watchdog = %d where event_detected < 2", event_bit, second);
		state = mysql_query(connection, update_string);
		if( state != 0 ) {
			fprintf(stderr,"Can not update:%s at %s, exiting in 10 seconds \n" , update_string, ss = strtime(now()));
			free(ss);
			exit(1); /* let rtexec restart */
		}
		result = mysql_store_result(connection);
		(void) mysql_free_result(result);
		if( connection != NULL ) {
			(void) mysql_close(connection);
		}
	}
}
