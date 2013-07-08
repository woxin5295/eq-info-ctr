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

int numdls = 0;

/* command line option used to force an off state of the test bit */
/* can be used during pipeline mini-shutdown */
int testbitoff = 0;

/* parameter file informatoin */
typedef struct sta_tag_params_ {
	char staname[STRSZ];
	int tagid;
} MyStaTagParams;

/* information retrieved from data logger status packets */
typedef struct data_logger_status_ {
	double dlt; /* data latency */	
	double clt; /* clock latency */ 
	double dv;  /* sensor voltage */ 
} MyDataLoggerStatus;

/* made these global */
MyDataLoggerStatus dlstat, dlstatcut;

struct par_info {
	int debug, orboldest, numsta, dlttime, clttime;
	double low_dv;
	int clean_time_period;
	int low_cutoff_time, medium_cutoff_time, high_cutoff_time, no_alarm_state_off;
	int cal_cutoff_time;
	char *orbname, *match; 
	char *outdbfile, *outpffile, *dbname;
	char *dlsrcname, *tmsrcname, *msgsrcname;
	MyStaTagParams Mytag[64];
	char *name[64];
	/* mysql info */
    	char *mysql_hosts, *mysql_user, *mysql_pass, *mysql_database;

};

struct par_info param_info;

struct Dl_Info  {
	char name[STRSZ];
	int tagid, skip;
	double time; /* most recent packet time */
	double time_tm, time_dl;
	/* data logger information */
	int test_c, status_c, test_o, status_o;
	int maint_c, maint_o;
	/* threshold information */
	int low_c, med_c, high_c;
	int low_o, med_o, high_o;
	double lcut, mcut, hcut, ccut;
	char p_mask_desc[17];
       /* currently defined p_mask_desc
       0000000000000001 - general test condition
       0000000000000010 - general status fault
       0000000000000100 - calibration test condition
       0000000000001000 - Data Latency exceeds a specified cutoff status fault
       0000000000010000 - GPS Clock Latency exceeds a specified cutoff status fault
       0000000000100000 - Sensor voltage is below a specified cutoff status fault
       before Nyman and H. pull head out of ass - 
       0000000000000001 - general system maintenance
       0000000000000010 - general system fault
       0000000000000100 - calibration maintenance
       0000000000001000 - Data Latency exceeds a specified cutoff
       */
};

int find_tag(char *name) {
	int j;
	for (j=0; j<param_info.numsta; j++) {
		if(strcmp(param_info.Mytag[j].staname, name) == 0) {
			return(param_info.Mytag[j].tagid); 
		}
	}
	/* no tag found */
	return(0);
}
struct Dl_Info dlinfo[128]; /* hardwired array, could malloc at a later date */
int breakup_message(char *message, char **parts) {
	int unknown, blanks, x,x2,i,l;
	blanks = 0;
	for(x=0; x < strlen(message); ++x) {
		if(message[x] == ' ') ++blanks;
	}
	unknown = 1;
	/* Maintenance type */
	if((strncmp(message,"Maintain ", 9) == 0 && blanks == 2) ||
	   (strncmp(message,"Test ", 5) == 0 && blanks == 2) ||
	   (strncmp(message,"Fault ", 6) == 0 && blanks == 2) ||
           (strncmp(message,"Heartbeat", 9) == 0 && blanks == 0) ||
	   (strncmp(message,"Cleanup", 7) == 0 && blanks == 0)
 		) {
		unknown = 0;
		if(strncmp(message,"Heartbeat", 9) == 0 && blanks == 0) {
			parts[0] = &message[0];
			return(0);
		}
		if(strncmp(message,"Cleanup", 7) == 0 && blanks == 0) {
			parts[0] = &message[0];
			return(0);
		}
	}
	if( unknown == 0) {
		i = 0;
		parts[i] = &message[0];
		l = strlen(message);
		for(x=1; x < l; ++x) {
			if(message[x] == ' ') {
				++i;
				message[x] = '\0';
				parts[i] = &message[x+1];
				if ( i == 1 ) {
					if(strncmp("ALL", &message[x+1], 3) == 0 || 
						(strncmp("PS", &message[x+1], 2) == 0 ||
						 strncmp("VM", &message[x+1], 2) == 0 )) {
						parts[i] = &message[x+1];
						x2 = x;
						
					} else
					  {
						message[x] = ' ';
						unknown = 1;
						x = l;
					}
				} else
				  {
					if(strncmp("on", &message[x+1], 2) == 0 || 
						strncmp("off", &message[x+1], 2) == 0 ) {
						parts[i] = &message[x+1];
					} else
					  {
						message[x] = ' ';
						message[x2] = ' ';
						unknown = 1;
						x = l;
					}
					
				} 
			}
		}
	}
	return(unknown); 
}

/* example orblog sent messages */
/* Send message to maintain station -
* orblog -s pipelinemsg moment:6514 "Maintain ALL on"
* orblog -s pipelinemsg moment:6514 "Maintain PS11 on"
* orblog -s pipelinemsg moment:6514 "Fault PS11 on"
* orblog -s pipelinemsg moment:6514 "Test PS11 on"
* Done maintaining station -
* orblog -s pipelinemsg moment:6514 "Maintain ALL off"
* orblog -s pipelinemsg moment:6514 "Maintain PS11 off"
* orblog -s pipelinemsg moment:6514 "Fault PS11 off"
* orblog -s pipelinemsg moment:6514 "Test PS11 off"
* Heartbeat
* orblog -s pipelinemsg moment:6514 "Heartbeat"
* orblog -s pipelinemsg moment:6514 "Cleanup"
*/

int new_dl_info(Dbptr db, int type, double pkttime, char *namesent, char *cal, double cuttime, char *alarm_s, int debug, char *message)
{
	/* type == 1 datalogger information, type == 2 threshold information, */
	/* type == 3 message placed on orb server with orblog, */
	/* type = 4 initialize station from first packet time received from orb*/
	/* cuttime - current cutoff time */
 
	int low_cutoff_time, medium_cutoff_time, high_cutoff_time, no_alarm_state_off;
	int cal_cutoff_time;
	char *name;
	int y,n, nnn, proceed, i, ci, newrow, newrow2,  nrec, nrec2, badmessage;
	Dbptr dbi, dbis, dbmax; /* db initialize */
	char name2[STRSZ], subset_expr[STRSZ];
	char *ss, alarm_state[STRSZ], *parts[25];
	int tagid, dbmarkset;
	double old_time;
	struct Dl_Info chdlinfo, dlmaxinfo;
	int updatematrix;
	static int initmatrix = 0;


	/* mysql param file information */	
        /* param_info.mysql_hosts, param_info.mysql_user, param_info.mysql_pass, param_info.mysql_database */
	/* mysql variables */
	static int mysql_connect_once;
	static char update_string[1024]; 
	static MYSQL_RES *result;
	static MYSQL_ROW row;
	static MYSQL *connection, mysql;
	static int state;

	newrow = 0; /* init newrow to zero */ 
	updatematrix = 0; /* set to one to update matrix */
	if(type != 3) {
		name = namesent;
	} else
	  {
		badmessage = breakup_message(message, parts);
		if(badmessage == 1) {
			fprintf(stderr,"%s: Unknown message: %s\n", ss=strtime(pkttime), message);
			free(ss); 
			return(0);
		} 
	}
	if(type == 3 ) {
		if(strncmp(parts[0],"Test", 4) == 0 ) {
			if( strncmp(parts[1], "ALL", 3) == 0 ) {
				for(n=0; n<numdls; ++n) {
					if( strncmp(parts[2], "on", 2) == 0) {
						dlinfo[n].test_c = 1;
       						/* 0000000000000001 - general test condition */
						dlinfo[n].p_mask_desc[15] = '1';
					} else
					  {
						dlinfo[n].p_mask_desc[15] = '0';
						/* 0000000000000100 - calibration test condition */
						if(dlinfo[n].p_mask_desc[13] == '0') {
							dlinfo[n].test_c = 0;
						}
					} 
				}
			} else /* one station */
			  {
				for(n=0; n<numdls; ++n) {
					if( strcmp(parts[1], dlinfo[n].name ) == 0) {
						if( strncmp(parts[2], "on", 2) == 0) {
							dlinfo[n].test_c = 1;
       							/* 0000000000000001 - general test condition */
							dlinfo[n].p_mask_desc[15] = '1';
						} else
						  {
							dlinfo[n].p_mask_desc[15] = '0';
							/* 0000000000000100 - calibration test condition */
							if(dlinfo[n].p_mask_desc[13] == '0') {
								dlinfo[n].test_c = 0;
							}
						}
					} 
				}
			}
		}
		/* note- station status is good when status bit is turned on */
		/*      station status has problem when status bit is turned off  */
		if(strncmp(parts[0],"Fault", 5) == 0 ) {
			if( strncmp(parts[1], "ALL", 3) == 0 ) {
				for(n=0; n<numdls; ++n) {
					if( strncmp(parts[2], "on", 2) == 0) {
						dlinfo[n].status_c = 0;
       						/* 0000000000000010 - general status fault */
						dlinfo[n].p_mask_desc[14] = '1';
					} else
					  {
						dlinfo[n].p_mask_desc[14] = '0';
						/* 0000000000001000 - Data Latency  */
       						/* 0000000000010000 - GPS Clock Latency */ 
       						/* 0000000000100000 - Sensor voltage  */
						if(dlinfo[n].p_mask_desc[12] == '0' &&  /* dlt */
						   dlinfo[n].p_mask_desc[11] == '0' &&  /* clt */
						   dlinfo[n].p_mask_desc[10] == '0'     /* dv */) {
							dlinfo[n].status_c = 1;
						}
					} 
				}
			} else /* one station */
			  {
				for(n=0; n<numdls; ++n) {
					if( strcmp(parts[1], dlinfo[n].name ) == 0) {
						if( strncmp(parts[2], "on", 2) == 0) {
							dlinfo[n].status_c = 0;
       							/* 0000000000000010 - general status fault */	
							dlinfo[n].p_mask_desc[14] = '1';
						} else
						  {
							dlinfo[n].p_mask_desc[14] = '0';
							/* 0000000000001000 - Data Latency  */
       							/* 0000000000010000 - GPS Clock Latency */ 
							/* 0000000000001000 - Data Latency exceeds a specified cutoff status fault */
							/* specified cutoff */
							if(dlinfo[n].p_mask_desc[12] == '0' &&  /* dlt */
							   dlinfo[n].p_mask_desc[11] == '0' &&  /* clt */
							   dlinfo[n].p_mask_desc[10] == '0'     /* dv */) {
								dlinfo[n].status_c = 1;
							}
						}
					} 
				}
			}
		}
		if(strncmp(parts[0],"Maintain", 8) == 0 ) {
			if( strncmp(parts[1], "ALL", 3) == 0 ) {
				for(n=0; n<numdls; ++n) {
					if( strncmp(parts[2], "on", 2) == 0) {
						dlinfo[n].maint_c = 1;
					} else
					  {
						dlinfo[n].maint_c = 0;
					} 
				}
			} else /* one station */
			  {
				for(n=0; n<numdls; ++n) {
					if( strcmp(parts[1], dlinfo[n].name ) == 0) {
						if( strncmp(parts[2], "on", 2) == 0) {
							dlinfo[n].maint_c = 1;
						} else
						  {
							dlinfo[n].maint_c = 0;
						}
					} 
				}
			}
		}
		if(strncmp(parts[0],"Heartbeat", 9) == 0 ) {
			if(debug == 1 ) {
				fprintf(stderr,"%s: Heartbeat received\n", ss=strtime(pkttime) ) ; 
				free(ss);
			}
			updatematrix = 1;
		}
		if(strncmp(parts[0],"Cleanup", 7) == 0 ) {
			
			if(debug == 1 ) {
				fprintf(stderr,"%s: Cleanup received\n", ss=strtime(pkttime) ) ; 
				free(ss);
			}
			
			dbmarkset = 0;
			db = dblookup( db, 0, "a_dlinfo", 0, 0 );
			dbquery (db, dbRECORD_COUNT, &nrec);
			for(n=0; n<numdls; ++n) {
				dlinfo[n].skip = 0; /* skip first occurance and process remainder */ 
			}
			/* check all records except last */
			if(nrec > 0) {
				for(ci = nrec-1; ci >= 0; --ci) {
					db.record = ci; 
					if( dbgetv(db, 0, 
						"time", &(chdlinfo.time),
						"p_TagName", chdlinfo.name,
						0) == dbINVALID) {
						fprintf(stderr,"dbgetv problem loading time field of a_dlinfo table row at index %d\n", dbis.record);
					}
					proceed = 0;
					for(nnn=0; nnn<numdls; ++nnn) {
						if(strcmp(chdlinfo.name, dlinfo[nnn].name) == 0) {
							if(dlinfo[nnn].skip == 0) {
								dlinfo[nnn].skip = 1;
								nnn = numdls;
							} else
							  {
								proceed = 1;
							}
						}
					}
					if(proceed == 1) { 
						dbis.record = ci;
						/* dbmark old packet times */
						old_time = now() - (double)chdlinfo.time ;
						if((double)old_time > 
							(double) (1.0 * param_info.clean_time_period))  {
							dbmarkset = 1;
							dbmark(db);
						}
					}
				}
				/* dbmark was set */
				if(dbmarkset == 1) dbcrunch(db);
			}
		}
	} else /* if type != 3 */
	  {	
		/* param files settings initialization */
		low_cutoff_time = param_info.low_cutoff_time;
		medium_cutoff_time = param_info.medium_cutoff_time;
		high_cutoff_time = param_info.high_cutoff_time;
		cal_cutoff_time = param_info.cal_cutoff_time;
		no_alarm_state_off = param_info.no_alarm_state_off;
	
		i = -1;
		newrow = 0; 
		(void) strcpy(name2, name);
		(void) strcpy(alarm_state, alarm_s);
		name2[25] = '\0';
		/* ignore bad orb packet time */
		if((double)pkttime < (double) 0.0) {
			/* if(debug == 1) {
			*	fprintf(stderr,"Bad packet time at %s\n", ss=strtime(pkttime) ) ; 
			*	free(ss);
			*} */	
			return(1);
		}
		/* ignore old packet times */
		old_time = now() - (double)pkttime ;
		if((double)old_time > (double) (1.0 * param_info.clean_time_period))  {
			 /* fprintf(stderr,"old_time %f seconds > %f seconds\n", 
			*	old_time, (1.0 * param_info.clean_time_period)) ;
			*/
			return(1);
		}
	 
		for(n=0; n<numdls; ++n) {
			if(strcmp(name2,  dlinfo[n].name) == 0) {
				i = n;
				n = numdls;
			}
		}
		/* new name init */
		if( i == -1 ) {
			i = numdls;
			(void) strcpy(dlinfo[i].name, name2);
			/* initialize tagid from parameter file information */
			dlinfo[i].tagid = find_tag(dlinfo[i].name);

			/* look for pre-existing database information */
			dbi = dblookup( db, 0, "a_dlinfo", 0, 0 );
			(void) sprintf(subset_expr,"p_TagName == \"%s\"", dlinfo[i].name);
			dbis = dbsubset ( dbi, subset_expr, 0 );
			dbquery (dbis, dbRECORD_COUNT, &nrec);
			if (nrec > 0) {
				/* get information from last record which has most recent information */
				dbis.record = nrec - 1;
				if( dbgetv(dbis, 0, 
					"time", &(dlinfo[i].time),
					"p_TagName", dlinfo[i].name,
					"p_tagid", &(dlinfo[i].tagid),
					"p_status", &(dlinfo[i].status_o),
					"p_maint", &(dlinfo[i].maint_o), 
					"p_low", &(dlinfo[i].low_o),
					"p_med", &(dlinfo[i].med_o),
					"p_high", &(dlinfo[i].high_o),
					"p_test", &(dlinfo[i].test_o),
					"p_mask_desc",dlinfo[i].p_mask_desc,
					0) == dbINVALID) {
					fprintf(stderr,"dbgetv problem loading a_dlinfo row at index %d\n", dbis.record);
					nrec = 0;
				} else
				  {
					dbfree(dbis);
					dlinfo[i].lcut = dlinfo[i].mcut = dlinfo[i].hcut = dlinfo[i].ccut = dlinfo[i].time;
					/* if type == 4 make _o (old) and _n equal */
					if(type == 4) {
						dlinfo[i].status_c = dlinfo[i].status_o;
						dlinfo[i].test_c = dlinfo[i].test_o;
						dlinfo[i].low_c = dlinfo[i].low_o;
						dlinfo[i].med_c = dlinfo[i].med_o;
						dlinfo[i].high_c = dlinfo[i].high_o;
						dlinfo[i].maint_c = dlinfo[i].maint_o;
						/* initialize dlinfo[i].time_tm and dlinfo[i].time_dl */
						dlinfo[i].time_tm = (double) dlinfo[i].time;  
						dlinfo[i].time_dl = (double) dlinfo[i].time;
					}
				}
			}	
			if(nrec < 1) { /* not already in database or dbgetv problem */
				newrow = 1;
				/* initialize p_mask_desc */ 
				(void) strcpy(dlinfo[i].p_mask_desc,"0000000000000000");
				if (type == 1 ) { /* datalogger info */		
					if(strcmp(cal,"-") == 0) { dlinfo[i].test_o = 0; } else { 
						dlinfo[i].ccut = pkttime;
						dlinfo[i].test_o = 1;
       						/* 0000000000000100 - calibration test condition */
						dlinfo[i].p_mask_desc[13] = '1'; 
					}
					if((double) dlstat.dlt < (double) dlstatcut.dlt &&
					   (double) dlstat.clt < (double) dlstatcut.clt &&
					   ((double) dlstat.dv >= (double) dlstatcut.dv
					   || (double) dlstat.dv == (double) 0.0)) { 
						dlinfo[i].status_o = 1; 
					} else { 
						dlinfo[i].status_o = 0;
					}
					/* 0000000000001000 - Data Latency  */
					if((double) dlstat.dlt < (double) dlstatcut.dlt) { 
						dlinfo[i].p_mask_desc[12] = '0';
					} else { 
						dlinfo[i].p_mask_desc[12] = '1';
					}
       					/* 0000000000010000 - GPS Clock Latency */ 
					if((double) dlstat.clt < (double) dlstatcut.clt) { 
						dlinfo[i].p_mask_desc[11] = '0';
					} else { 
						dlinfo[i].p_mask_desc[11] = '1';
					}
       					/* 0000000000100000 - Sensor voltage  */
					if((double) dlstat.dv >= (double) dlstatcut.dv || 
					   (double) dlstat.dv == (double) 0.0) { 
						dlinfo[i].p_mask_desc[10] = '0';
					} else { 
						dlinfo[i].p_mask_desc[10] = '1';
					}
					/* default datalogger values */
					dlinfo[i].low_o = dlinfo[i].med_o = dlinfo[i].high_o = 
						dlinfo[i].low_c = dlinfo[i].med_c = dlinfo[i].high_c = 0;
					/* default maintenance bit */
					dlinfo[i].maint_o = dlinfo[i].maint_c = 0;
				} else if (type == 2 ) /* tm infomation */
				  {
					dlinfo[i].low_o = dlinfo[i].med_o = dlinfo[i].high_o = 0;
					if( no_alarm_state_off == 0 ) {	
						if(strncmp(alarm_state,"low", 3) == 0) {
							dlinfo[i].low_o = 1; dlinfo[i].lcut = pkttime;
						}	
						if(strncmp(alarm_state,"medium", 6) == 0) {
							dlinfo[i].low_o = 1; dlinfo[i].lcut = pkttime;
							dlinfo[i].med_o = 1; dlinfo[i].mcut = pkttime; 
						}	
						if(strncmp(alarm_state,"high",4) == 0) {
							dlinfo[i].low_o = 1; dlinfo[i].lcut = pkttime;
							dlinfo[i].med_o = 1; dlinfo[i].mcut = pkttime; 
							dlinfo[i].high_o = 1; dlinfo[i].hcut = pkttime;
						} 
						if(strncmp(alarm_state,"off",3) == 0 ) {
							dlinfo[i].low_o = dlinfo[i].med_o = dlinfo[i].high_o = 0;
						}
					} else {
						if(dlinfo[i].low_o == 0) {
							if(strncmp(alarm_state,"low", 3) == 0) {
								dlinfo[i].low_o = 1; dlinfo[i].lcut = pkttime;
							}
						}	
						if(dlinfo[i].med_o == 0) {
							if(strncmp(alarm_state,"medium", 6) == 0) {
								dlinfo[i].low_o = 1; dlinfo[i].lcut = pkttime;
								dlinfo[i].med_o = 1; dlinfo[i].mcut = pkttime; 
							}
						}
						if(dlinfo[i].high_o == 0) {
							if(strncmp(alarm_state,"high",4) == 0) {
								dlinfo[i].low_o = 1; dlinfo[i].lcut = pkttime;
								dlinfo[i].med_o = 1; dlinfo[i].mcut = pkttime; 
								dlinfo[i].high_o = 1; dlinfo[i].hcut = pkttime;
							}
						} 
					}
					dlinfo[i].status_o = dlinfo[i].status_c = 1;
					dlinfo[i].test_o = dlinfo[i].test_c = 0;
					/* default maintenance bit */
					dlinfo[i].maint_o = dlinfo[i].maint_c = 0;
				} else if (type == 4 ) /* initialize station row in db */
				  {
					dlinfo[i].low_o = dlinfo[i].med_o = dlinfo[i].high_o = 0;
					dlinfo[i].low_c = dlinfo[i].med_c = dlinfo[i].high_c = 0;
					dlinfo[i].maint_o = dlinfo[i].maint_c = 0;
					dlinfo[i].test_o = dlinfo[i].test_c = 0 ;
					dlinfo[i].status_o = dlinfo[i].status_c = 1;
					dlinfo[i].time =  (double) pkttime; 
					/* initialize to first packet reveived from orb */
					/* This will only happen if no preexisting data base exist */
					dlinfo[i].time_tm =  (double) pkttime; 
					dlinfo[i].time_dl =  (double) pkttime;
				}
			} 
			++numdls;
		}
		if(     (double) dlinfo[i].time_tm < (double) pkttime || 
			(double) dlinfo[i].time_dl < (double) pkttime ||
			newrow == 1) {  
			if (    type == 1 &&
				((double) dlinfo[i].time_dl < (double) pkttime || newrow == 1) 
				) { /* datalogger info */		
				if(strcmp(cal,"-") == 0) {
					/* 0000000000000001 - general test condition */
					if(dlinfo[i].p_mask_desc[15] == '0') {
						cuttime = (double)pkttime - 
							(double)dlinfo[i].ccut;
						if(dlinfo[i].test_c == 1 &&
							((double) cuttime >
							(double) cal_cutoff_time)) { 
							dlinfo[i].test_c = 0;
						}
					} 
					/* 0000000000000100 - calibration test condition */
					if(dlinfo[i].p_mask_desc[13] == '1') newrow = 1;
					dlinfo[i].p_mask_desc[13] = '0';
				} else 
				  { 
					dlinfo[i].test_c = 1; 
					/* 0000000000000100 - calibration test condition */
					dlinfo[i].ccut = pkttime;
					if(dlinfo[i].p_mask_desc[13] == '0') newrow = 1;
					dlinfo[i].p_mask_desc[13] = '1';
	
				}
				if((double) dlstat.dlt < (double) (1.0*dlstatcut.dlt)) { 
					/* 0000000000000010 - general status fault */
					if(dlinfo[i].p_mask_desc[14] == '0' && /* general fault */
					   dlinfo[i].p_mask_desc[11] == '0' &&  /* clt */
					   dlinfo[i].p_mask_desc[10] == '0'     /* dv */
						) {
						dlinfo[i].status_c = 1;
					} 
					/* 0000000000001000 - Data Latency  */
					if(dlinfo[i].p_mask_desc[12] == '1') newrow = 1;
					dlinfo[i].p_mask_desc[12] = '0';
				} else 
				  { 
					dlinfo[i].status_c = 0;
					/* 0000000000001000 - Data Latency  */
					if(dlinfo[i].p_mask_desc[12] == '0') newrow = 1;
					dlinfo[i].p_mask_desc[12] = '1'; 
				}
				if((double) dlstat.clt < (double) (1.0*dlstatcut.clt)) { 
					/* 0000000000000010 - general status fault */
					if(dlinfo[i].p_mask_desc[14] == '0' && /* general fault */
					   dlinfo[i].p_mask_desc[12] == '0' &&  /* dlt */
					   dlinfo[i].p_mask_desc[10] == '0'     /* dv */
						) {
						dlinfo[i].status_c = 1;
					} 
       					/* 0000000000010000 - GPS Clock Latency */ 
					if(dlinfo[i].p_mask_desc[11] == '1') newrow = 1;
					dlinfo[i].p_mask_desc[11] = '0';
				} else 
				  { 
					dlinfo[i].status_c = 0;
       					/* 0000000000010000 - GPS Clock Latency */ 
					if(dlinfo[i].p_mask_desc[11] == '0') newrow = 1;
					dlinfo[i].p_mask_desc[11] = '1'; 
				}
				if((double) dlstat.dv >= (double) (1.0*dlstatcut.dv) ||
					(double) dlstat.dv == (double) 0.0) { 
					/* 0000000000000010 - general status fault */
					if(dlinfo[i].p_mask_desc[14] == '0' && /* general fault */
					   dlinfo[i].p_mask_desc[12] == '0' &&  /* dlt */
					   dlinfo[i].p_mask_desc[11] == '0'     /* clt */
						) {
						dlinfo[i].status_c = 1;
					} 
       					/* 0000000000100000 - Sensor voltage  */
					if(dlinfo[i].p_mask_desc[10] == '1') newrow = 1;
					dlinfo[i].p_mask_desc[10] = '0';
				} else 
				  { 
					dlinfo[i].status_c = 0;
       					/* 0000000000100000 - Sensor voltage  */
					if(dlinfo[i].p_mask_desc[10] == '0') newrow = 1;
					dlinfo[i].p_mask_desc[10] = '1';
				} 
				if(dlinfo[i].test_c != dlinfo[i].test_o || dlinfo[i].status_c != dlinfo[i].status_o) {
					newrow = 1;
					dlinfo[i].test_o = dlinfo[i].test_c;
					dlinfo[i].status_o = dlinfo[i].status_c;
				}
				dlinfo[i].time_dl = (double) pkttime;
			} else if (type == 2 && 
				((double) dlinfo[i].time_tm < (double) pkttime || newrow == 1) 
				) /* tm infomation */		
			  {
				if( no_alarm_state_off == 0 ) {	
					if(strncmp(alarm_state,"low", 3) == 0) {
						dlinfo[i].low_c = 1; dlinfo[i].lcut = pkttime;
					}	
					if(strncmp(alarm_state,"medium", 6) == 0) {
						dlinfo[i].low_c = 1; dlinfo[i].lcut = pkttime;
						dlinfo[i].med_c = 1; dlinfo[i].mcut = pkttime; 
					}	
					if(strncmp(alarm_state,"high",4) == 0) {
						dlinfo[i].low_c = 1; dlinfo[i].lcut = pkttime;
						dlinfo[i].med_c = 1; dlinfo[i].mcut = pkttime; 
						dlinfo[i].high_c = 1; dlinfo[i].hcut = pkttime;
					} 
					if(strncmp(alarm_state,"off",3) == 0 ) {
						dlinfo[i].low_c = dlinfo[i].med_c = dlinfo[i].high_c = 0;
					}
				} else {
					if(dlinfo[i].low_c == 0) {
						if(strncmp(alarm_state,"low", 3) == 0) {
							dlinfo[i].low_c = 1; dlinfo[i].lcut = pkttime;
						}
					}	
					if(dlinfo[i].med_c == 0) {
						if(strncmp(alarm_state,"medium", 6) == 0) {
							dlinfo[i].low_c = 1; dlinfo[i].lcut = pkttime;
							dlinfo[i].med_c = 1; dlinfo[i].mcut = pkttime; 
						}
					}
					if(dlinfo[i].high_c == 0) {
						if(strncmp(alarm_state,"high",4) == 0) {
							dlinfo[i].low_c = 1; dlinfo[i].lcut = pkttime;
							dlinfo[i].med_c = 1; dlinfo[i].mcut = pkttime; 
							dlinfo[i].high_c = 1; dlinfo[i].hcut = pkttime;
						}
					} 
				}
				if(dlinfo[i].low_c != dlinfo[i].low_o ||
				   dlinfo[i].med_c != dlinfo[i].med_o ||
				   dlinfo[i].high_c != dlinfo[i].high_o ) {
					newrow = 1;
					dlinfo[i].low_o = dlinfo[i].low_c;
					dlinfo[i].med_o = dlinfo[i].med_c;
					dlinfo[i].high_o = dlinfo[i].high_c;
				}
				dlinfo[i].time_tm = (double) pkttime;
			} 
		}
	} /* if type != 3 */
	/* check cutoff times for all stations */
	/* check to see if any manual setting have be set */
	for(n=0; n<numdls; ++n) {
		newrow2 = 0;
		if( low_cutoff_time > 0 ) {
			if( dlinfo[n].med_c == 0 && dlinfo[n].high_c == 0 ) { 
				cuttime = (double)pkttime - (double)dlinfo[n].lcut;
				if(dlinfo[n].low_c == 1 && ((double) cuttime > 
					(double) low_cutoff_time)) {
					newrow2 = 1; dlinfo[n].low_o = dlinfo[n].low_c = 0;
				}
			}
		} 
		if( medium_cutoff_time > 0 ) {
			if( dlinfo[n].high_c == 0 ) { 
				cuttime = (double)pkttime - (double)dlinfo[n].mcut;
				if(dlinfo[n].med_c == 1 && ((double) cuttime > 
					(double) medium_cutoff_time)) {
					newrow2 = 1; dlinfo[n].med_o = dlinfo[n].med_c = 0;
					dlinfo[n].low_o = dlinfo[n].low_c = 0;
				}
			}
		} 
		if( high_cutoff_time > 0 ) {
			cuttime = (double)pkttime - (double)dlinfo[n].hcut;
			if(dlinfo[n].high_c == 1 && ((double) cuttime > (double) high_cutoff_time)) {
				newrow2 = 1; dlinfo[n].high_o = dlinfo[n].high_c = 0;
				dlinfo[n].med_o = dlinfo[n].med_c = 0;
				dlinfo[n].low_o = dlinfo[n].low_c = 0;
			}
		} 
		/* maintenance setting */
		if(dlinfo[n].test_o != dlinfo[n].test_c) {
			newrow2 = 1;
			dlinfo[n].test_o = dlinfo[n].test_c;
		}
		/* status setting */
		if(dlinfo[n].status_o != dlinfo[n].status_c) {
			newrow2 = 1;
			dlinfo[n].status_o = dlinfo[n].status_c;
		}
		/* Test bit */
		if(dlinfo[n].maint_o != dlinfo[n].maint_c) {
			newrow2 = 1;
			dlinfo[n].maint_o = dlinfo[n].maint_c;
		}
		if(newrow2 == 1 && n != i) {
			db = dblookup( db, 0, "a_dlinfo", 0, 0 );
			db.record = dbaddnull (db);
			dbquery (db, dbRECORD_COUNT, &nrec);
			
			tagid = find_tag(dlinfo[n].name);	
			if(db.record < 0 || dbputv(db, 0, 
				"time", pkttime,
				"p_TagName", dlinfo[n].name,
				"p_tagid", tagid,
				"p_status", dlinfo[n].status_c,
				"p_maint", dlinfo[n].maint_c, 
				"p_low", dlinfo[n].low_c,
				"p_med", dlinfo[n].med_c,
				"p_high", dlinfo[n].high_c,
				"p_test", dlinfo[n].test_c,
				"p_mask_desc",dlinfo[n].p_mask_desc,
				0) == dbINVALID) {
				complain(0,"dbputv problem loading a_dlinfo row at index %d\n", nrec);
			} else
			  {
				dlinfo[n].time = (double) pkttime;
				updatematrix = 1;
			}
		}
		if(newrow2 == 1 && n == i) {
			if( n > -1 ) { /* probably do not need this if statement */
				newrow = 1;
			}
		}
	}
	if(newrow == 1 ) {
		db = dblookup( db, 0, "a_dlinfo", 0, 0 );
		db.record = dbaddnull (db);
		dbquery (db, dbRECORD_COUNT, &nrec);
		
		tagid = find_tag(dlinfo[i].name);	
		if(db.record < 0 || dbputv(db, 0, 
			"time", pkttime,
			"p_TagName", dlinfo[i].name,
			"p_tagid", tagid,
			"p_status", dlinfo[i].status_c,
			"p_maint", dlinfo[i].maint_c, 
			"p_low", dlinfo[i].low_c,
			"p_med", dlinfo[i].med_c,
			"p_high", dlinfo[i].high_c,
			"p_test", dlinfo[i].test_c,
			"p_mask_desc",dlinfo[i].p_mask_desc,
			0) == dbINVALID) {
			complain(0,"dbputv problem loading a_dlinfo row at index %d\n", nrec);
		} else
		  {
			dlinfo[i].time = (double) pkttime;
			updatematrix = 1;
		}
	}
	if(updatematrix == 1) {
		if (initmatrix == 0) {
			initmatrix = 1;
			dbmax = dblookup( db, 0, "a_matrix", 0, 0 );
			dbquery (dbmax, dbRECORD_COUNT, &nrec);
			/* remove existing matrix */
			if(nrec > 0 ) {
				for (dbmax.record=0; dbmax.record<nrec; dbmax.record++) {
					dbmark(dbmax);
				}
				dbcrunch(dbmax);
			}
			/* create default matrix table */
			for (n=0; n<param_info.numsta; n++) {
				dbmax.record = dbaddnull (dbmax);
				dbquery (dbmax, dbRECORD_COUNT, &nrec);
				if(dbmax.record < 0 || dbputv(dbmax, 0,
					"p_tagid", param_info.Mytag[n].tagid,
					"p_status", 1, "p_maint", 0, "p_low", 0,
					"p_med", 0, "p_high", 0, "p_test", 0, 0) == dbINVALID) {
					fprintf(stderr,"dbputv problem initializing a_matrix row at index %d\n", dbmax.record);
				}
			}
		} 
		dbmax = dblookup( db, 0, "a_matrix", 0, 0 );
		dbquery (dbmax, dbRECORD_COUNT, &nrec);
		if(nrec > 1) {
			/* update all rows in matrix */
			mysql_connect_once = 0;
			for (n=0; n<param_info.numsta; n++) {
				for(y=0; y<numdls; ++y) {
					if(param_info.Mytag[n].tagid == dlinfo[y].tagid) {
						dbmax.record = n;
						/* update antelope row */
						if(dbmax.record < 0 || dbputv(dbmax, 0,
							"p_tagid", dlinfo[y].tagid,
							"p_status", dlinfo[y].status_c, 
							"p_maint", dlinfo[y].maint_c, 
							"p_low", dlinfo[y].low_c,
							"p_med", dlinfo[y].med_c, 
							"p_high", dlinfo[y].high_c, 
							"p_test", dlinfo[y].test_c, 0) == dbINVALID) {
								fprintf(stderr,"dbputv problem updating a_matrix row at index %d\n", dbmax.record);
						} else
						  {
						}
						/* update mysql matrix rows */
						/* only initialize mysql one time */
						if (mysql_connect_once == 0) {
							/* this is freed up when mysql is called */
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
								fprintf(stderr,"Can not connect to mysql database at %s, exiting in 10 seconds \n", ss = strtime(now()));
								free(ss);
								sleep(10); /* sleep 10 seconds waiting */
									   /* for mysql server to restart */
								exit(1); /* let rtexec restart */
							}
							mysql_connect_once = 1;
						}
						if ( testbitoff == 0 ) {
							(void) sprintf(update_string,"update occ_display set trigger_low = %d, trigger_medium = %d, trigger_high = %d, system_status = %d, test = %d, maintenance = %d where station_id = %d",
								dlinfo[y].low_c, dlinfo[y].med_c, 
								dlinfo[y].high_c, dlinfo[y].status_c,
								dlinfo[y].test_c, dlinfo[y].maint_c,
								dlinfo[y].tagid );
						} else
						  {
							(void) sprintf(update_string,"update occ_display set trigger_low = %d, trigger_medium = %d, trigger_high = %d, system_status = %d, test = 0, maintenance = %d where station_id = %d",
								dlinfo[y].low_c, dlinfo[y].med_c, 
								dlinfo[y].high_c, dlinfo[y].status_c,
								dlinfo[y].maint_c,
								dlinfo[y].tagid );

						}
						state = mysql_query(connection, update_string);
						if( state != 0 ) {
							fprintf(stderr,"Can not update:%s at %s, exiting in 10 seconds \n", update_string, ss = strtime(now())); 
							free(ss);
							sleep(10); /* sleep 10 seconds waiting */
								   /* for mysql server to restart */
							exit(1); /* let rtexec restart */
						}
						result = mysql_store_result(connection);
						(void) mysql_free_result(result);
					}
				} /* for y */
			} /* for n */
			if( connection != NULL ) {
				(void) mysql_close(connection);
			}
		} /* nrec > 1 */
	} /* matrix */ 
}

/* tm info structure */
struct tm_info {
	char sta[7], chan[9], segtype[2];
	double alarm_peak, alarm_time;
	char alarm_state[36];
};


int get_param_file_info(char *pfname, int debug) {
	static Pf *pf = 0;
	char  *s, *tagname;
	int j;
	Arr *sta_list;
	Arr *dbsta_list_arr;
	Tbl *dbsta_list_tbl;
	static Pf *pfs;
	static Pf *pfsta_list_arr;


	/* get parmeter file information */
	if ( pfread ( pfname, &pf ) != 0 ) {
		complain ( 0, "Error in parameter file: %s\n, exit", pfname ) ;
		exit(1);
	}

	if (parse_param (pf, "orboldest", P_LINT, 1, &param_info.orboldest) < 0) {
		param_info.orboldest = 0;
	}
	if (param_info.orboldest > 1) param_info.orboldest = 1;

	if (parse_param (pf, "no_alarm_state_off", P_LINT, 1, &param_info.no_alarm_state_off) < 0) {
		param_info.no_alarm_state_off = 0;
	}

	if (param_info.no_alarm_state_off > 1) param_info.no_alarm_state_off = 1;

	if (parse_param (pf, "debug", P_LINT, 1, &param_info.debug) < 0) {
		param_info.debug = 0;
	}
	if (parse_param (pf, "dlttime", P_LINT, 1, &param_info.dlttime) < 0) {
		param_info.dlttime = 5; /* default five seconds */
	} else
	  {
		if(param_info.dlttime < 1) {
			param_info.dlttime = 5;
		}
	}
	if (parse_param (pf, "low_dv", P_DBL, 1, &param_info.low_dv) < 0) {
		param_info.low_dv = (double) 10.0; /* default 10.0 Volts */
	} else
	  {
		if((double)param_info.low_dv < (double) 0.0 ) {
			param_info.low_dv = (double) 10.0;
		}
	}
	if (parse_param (pf, "clttime", P_LINT, 1, &param_info.clttime) < 0) {
		param_info.clttime = 172800; /* default 2 days */
	} else
	  {
		if(param_info.clttime < 1) {
			param_info.clttime = 172888;
		}
	}
	if (parse_param (pf, "clean_time_period", P_LINT, 1, &param_info.clean_time_period) < 0) {
		param_info.clean_time_period = 0; /* default is zero, do not clean */
	} else
	  {
		if(param_info.clean_time_period < 1) {
			param_info.clean_time_period = 0;
		}
	}
	
	if (parse_param (pf, "cal_cutoff_time", P_LINT, 1, &param_info.cal_cutoff_time) < 0) {
		param_info.cal_cutoff_time = 120; /* default 120 seconds */
	} else
	  {
		if(param_info.cal_cutoff_time < 1) {
			param_info.cal_cutoff_time = 120;
		}
	}
	if (parse_param (pf, "low_cutoff_time", P_LINT, 1, &param_info.low_cutoff_time) < 0) {
		param_info.low_cutoff_time = 60; /* default 60 seconds */
	} else
	  {
		if(param_info.low_cutoff_time < 1) {
			param_info.low_cutoff_time = 60;
		}
	}
	if (parse_param (pf, "medium_cutoff_time", P_LINT, 1, &param_info.medium_cutoff_time) < 0) {
		param_info.medium_cutoff_time = 90; /* default 90 seconds */
	} else
	  {
		if(param_info.medium_cutoff_time < 1) {
			param_info.medium_cutoff_time = 90;
		}
	}
	if (parse_param (pf, "high_cutoff_time", P_LINT, 1, &param_info.high_cutoff_time) < 0) {
		param_info.high_cutoff_time = 120; /* default 120 seconds */
	} else
	  {
		if(param_info.high_cutoff_time < 1) {
			param_info.high_cutoff_time = 120;
		}
	}

	/* set in command line */
	if(debug == 1 ) param_info.debug = 1;	
	if (param_info.debug > 1) param_info.debug = 1;
	param_info.orbname = pfget_string ( pf, "orbname" );
	if (param_info.orbname == NULL) {
		register_error (0, "parse_pf: pfget_string(orbname) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires orbname\n");
		exit(1);
	} 
	param_info.match = pfget_string ( pf, "match" );
	if (param_info.orbname == NULL) {
		register_error (0, "parse_pf: pfget_string(match) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires match expresssion\n");
		exit(1);
	} 
	param_info.outdbfile = pfget_string ( pf, "outdbfile" );
	param_info.outpffile = pfget_string ( pf, "outpffile" );
	if (param_info.outpffile == NULL) {
		register_error (0, "parse_pf: pfget_string(outpffile) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires outpffile\n");
		exit(1);
	} 
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
	param_info.dbname = pfget_string ( pf, "dbname" );
	if (param_info.orbname == NULL) {
		register_error (0, "parse_pf: pfget_string(dbname) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires dbname\n");
		exit(1);
	} 
	param_info.dlsrcname = pfget_string ( pf, "dlsrcname" );
	if (param_info.dlsrcname == NULL) {
		register_error (0, "parse_pf: pfget_string(dlsrcname) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires dlsrcname\n");
		exit(1);
	} 
	param_info.tmsrcname = pfget_string ( pf, "tmsrcname" );
	if (param_info.tmsrcname == NULL) {
		register_error (0, "parse_pf: pfget_string(tmsrcname) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires tmsrcname\n");
		exit(1);
	}
	param_info.msgsrcname = pfget_string ( pf, "msgsrcname" );
	if (param_info.msgsrcname == NULL) {
		register_error (0, "parse_pf: pfget_string(msgsrcname) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires msgsrcname\n");
		exit(1);
	}

	/* Initialize arrays for db sta_list */
	sta_list = newarr (0);
	if (parse_param (pf, "sta_list", P_PFARR, 1, &dbsta_list_arr) < 0) {
		register_error (0, "parse_pf: parse_param(dbsta_list_arr) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires sta_list:dbsta_list_arr\n");
		exit(1);
	}
	if (parse_param (pf, "sta_list", P_ARRPF, 1, &(pfsta_list_arr)) < 0) {
		register_error (0, "parse_pf: parse_param(pfsta_list_arr) error\n");
		fprintf(stderr,"pipelinemaxtrix.pf requires sta_list:pfsta_list_arr\n");
		exit(1);
	}
	dbsta_list_tbl = keysarr( dbsta_list_arr );
	param_info.numsta = maxtbl(dbsta_list_tbl);
	for (j=0; j<param_info.numsta; j++) {
		param_info.name[j] = (char *) gettbl (dbsta_list_tbl, j);
		(void)strcpy(param_info.Mytag[j].staname, param_info.name[j]);
		switch (pfget(pfsta_list_arr, param_info.Mytag[j].staname, (void **) &pfs)) {
			case PFARR: break;
			case PFSTRING: break;
			default:
			fprintf(stderr,"pipelinemaxtrix.pf element %s has bad value, exit\n",
				param_info.Mytag[j].staname);
			exit(1);
		}

		param_info.Mytag[j].tagid = pfget_int(pfs,"tagid");
		if (parse_param (pfs, "tagid", P_LINT, 1, &(param_info.Mytag[j].tagid)) < 0) {
			fprintf(stderr,"pipelinemaxtrix.pf bad tagid, exit\n");
			exit(1);
		}
	}
	freearr(dbsta_list_arr, 0);
	freetbl(dbsta_list_tbl, 0);
	freearr(sta_list,0); 
}
int main (int argc, char **argv)
{
	Packet   *unstuffed=0 ;
	char *orbname, *match, *outpffile, *outdbfile, *dbname;
	char *dlsrcname, *tmsrcname, *msgsrcname;
	int orb, nmatch, rcode, x, type, found;
	int jj, ni, pktid, nbytes=0, bufsize=0;
	char srcname[ORBSRCNAME_SIZE];
	double pkttime, start_time, old_time, time;
	char *packet=0;
	char *s, *name;
	char *pfname, *statefile = NULL;
	int abort = 0;
	FILE *filep;
	Pf *pf = 0, *pf1 = 0, *pf2 = 0;
	Pf *pfdls_arr;
	Arr  *dbdls_arr;
	Tbl *dbdls_tbl;
	int j, numkey, i, debug = 0, orboldest = 0, no_alarm_state_off = 0;
	Dbptr db, dbpkt;
	struct tm_info tm;
	char message[128];
	int firstpacket;
	char *ss ;
	int typepfget, moff, tmoff;
	double new_now,old_now,future_check;
	double duration;
	
	Program_Name = argv[0];

	pfname = argv[0];

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	for (x=1; x < argc; ++x) {
		if( strcmp("-pf", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) {
				++x;
				pfname = argv[x];
			}
		}
		if( strcmp("-S", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) {
				++x;
				statefile = argv[x];
			}
		}
		if( strcmp("-debug", argv[x]) == 0 ) {
			debug = 1;
		}
		if( strcmp("-testbitoff", argv[x]) == 0 ) {
			testbitoff = 1;
		}
	}

	/* get parameter file information */
	get_param_file_info(pfname, debug);

	debug = param_info.debug;
	orbname = param_info.orbname;
	match = param_info.match;
	outpffile = param_info.outpffile;
	outdbfile = param_info.outdbfile;
	dbname = param_info.dbname;
	orboldest = param_info.orboldest;
	dlsrcname = param_info.dlsrcname;
	tmsrcname = param_info.tmsrcname;
	msgsrcname = param_info.msgsrcname;
	dlstatcut.dlt = (1.0) * param_info.dlttime;
	dlstatcut.clt = (1.0) * param_info.clttime;
	dlstatcut.dv = (1.0) * param_info.low_dv;
	no_alarm_state_off = (1.0) * param_info.no_alarm_state_off;

	if (debug == 1) {
		fprintf(stderr,"%s will collect %s packets from orb %s\n\t", Program_Name,
			match, orbname); 
		if( outpffile != NULL) {
			fprintf(stderr,"debugging paramter file is %s\n\t", outpffile);
		}
		if( outdbfile != NULL) {
			fprintf(stderr,"debugging database file is %s\n\t", outdbfile);
		}
		fprintf(stderr,"outputdb is %s db \n\t", dbname);
		fprintf(stderr,"orboldest=%d, no_alarm_state_off=%d, dlsrcname=%s, tmsrcname=%s\n\tmsgsrcname=%s \n\n ;", 
			orboldest, no_alarm_state_off, dlsrcname, tmsrcname, msgsrcname);
		fprintf(stderr,"Station List is ");
		for (j=0; j<param_info.numsta; j++) {
			fprintf(stderr,"(%s,%d),", param_info.Mytag[j].staname, param_info.Mytag[j].tagid);
		}
		fprintf(stderr,"\n\n");
	} 	
	/* open data base */	
	if(dbopen( dbname, "r+", &db) != 0) {
		fprintf( stderr, "Can not open database %s \n", dbname);
		exit(1);
	}
	
	/* open orbserver */
	if ( (orb = orbopen ( orbname, "r&" )) < 0 )
		die ( 0, "Can't open ring buffer '%s'\n", orbname ) ; 

	if ( (nmatch = orbselect ( orb, match )) < 0 )
		complain ( 1, "orbselect '%s' failed\n", match ) ;
	else
		fprintf (stderr,"%d sources selected after select\n", nmatch) ; 

	/* Initialize for statefile processing. 
	This involves 1) setting up signal handlers with exhume(), 
	2) parsing the input statefile, if it exists, with resurrect() and 
	3) positioning the input ORB read pointer. */
	if (statefile != NULL && orboldest == 0) {
		if ( exhume ( statefile, &abort, RT_MAX_DIE_SECS, 0 ) != 0 ) {
			elog_notify ( 0, "read old state file\n" ) ;
			if(debug == 1) fprintf(stderr,"read old state file\n"); 
		}
		if ( orbresurrect ( orb, &pktid, &time ) == 0 )  {
			elog_notify ( 0, "resurrection successful: repositioned to pktid #%d @ %s\n",
			pktid, ss=strtime(time) ) ;
			free(ss) ;
			if(debug == 1)fprintf(stderr,"resurrection successful: repositioned to pktid #%d\n",pktid ); 
			rcode = orbget ( orb, ORBCURRENT, &pktid, srcname, &time, &packet, &nbytes, &bufsize ) ;
		} else {
			complain ( 0, "resurrection unsuccessful get ORBNEWEST\n" ) ;
			if(debug == 1)fprintf(stderr,"resurrection unsuccessful get ORBNEWEST\n" ); 
			rcode = orbget ( orb, ORBNEWEST, &pktid, srcname, &time, &packet, &nbytes, &bufsize ) ;
		}
	} else
	  { 
		if( orboldest == 0) {
			rcode = orbget ( orb, ORBNEWEST, &pktid, srcname, &time, &packet, &nbytes, &bufsize ) ;
		} else
		  {
			rcode = orbget ( orb, ORBOLDEST, &pktid, srcname, &time, &packet, &nbytes, &bufsize ) ;
		}
	}
	/* printf ("starting at pktid #%d = %s at time %s\n", pktid, srcname, s=strtime(time) ) ; 
	free(s) ; */

	old_now = now(); /* used to time period between packet processing */
	start_time = now() ;
	firstpacket = 1;
	while (! abort) {
		dlstat.dlt = (double) 0.0;
		dlstat.clt = (double) 0.0;
		dlstat.dv = (double) 0.0;
		rcode = orbreap ( orb, &pktid, srcname, &time, &packet, &nbytes, &bufsize ) ;
		/* initialize all stations that are being monitored by the orb */
		/* after receiving orb first packet with good time */
		/* make sure packet is not more than 20 minutes into the future */
		future_check = (double)time - now();
		/* ignore negative time and future time */
		if ((double)time > (double) 0.0 && (double) future_check < 1200.0 ) {
			if(firstpacket == 1 ) {
				/* ignore bad orb packet time */
				
				old_time = now() - (double)time ;
				 if((double)old_time > (double) (1.0 * param_info.clean_time_period )) { 
					/* if(debug == 1) {
					*	fprintf(stderr,"Only initialize with good packet time, Bad time at %s\n",
					*		 ss=strtime(time) ) ; 
					*		free(ss);
					*}
					*/	
				} else
				  {
					for (j=0; j<param_info.numsta; j++) {
						new_dl_info(db, 4, (double)time, (char *)param_info.Mytag[j].staname, 
							(char *)"", (double)0.0, (char *)"", debug, ""); 
					}
					firstpacket = 0;
				}
			}
		}
		if ( rcode < 0  || 
		     (double)time < (double) 0.0 || 
		     (double) future_check > 1200.0  ) {
			if ( rcode < 0 ) {
				fprintf(stderr, "\norbreap fails\n" ) ; 
				break ;
			}
			if ((double)time < (double) 0.0) {
				fprintf(stderr,"Negative packet time\n");
			}
			if ((double) (double) future_check > (double) 1200.0) {
				fprintf(stderr,"Packet time %s is %f seconds in the future\n", ss=strtime(time),
					future_check);
				free(ss);
			}
		} else
		  {
		/* process Pkt */
			moff = strlen(srcname) - strlen(msgsrcname);
			if (moff < 0) moff = 0;
			tmoff = strlen(srcname) - strlen(tmsrcname);
			if (tmoff < 0) tmoff = 0;
			if(strcmp(srcname, dlsrcname) == 0 || 
				strcmp(&srcname[tmoff], tmsrcname) == 0 || 
				strcmp(&srcname[moff], msgsrcname) == 0) {
				/* unstuffPkt segmentation faults on dl status packets, use showPKt instead */
				if(strcmp(srcname, dlsrcname) == 0) {
					filep = fopen( outpffile , "w+");
					showPkt ( pktid, srcname, time, packet, nbytes, filep, PKT_UNSTUFF);
					fclose(filep);
				} else 
				  {
					type = unstuffPkt (srcname, time, packet, nbytes, &unstuffed) ;
				} 
			} else
			  {
				type = 0;
			}
	
			/* process message sent into orb for test bit or maintenance bit */
			/* Pkt_ch arbitrary character string */	
			if(strcmp(&srcname[moff], msgsrcname) == 0 && type == Pkt_ch ) {
				(void) strcpy(message, unstuffed->string);
				/* if(debug == 1) {
				*	fprintf(stderr," Recived Message: %s\n", message);
				*}
				*/
				new_dl_info(db, 3, (double)time, (char *)"", (char *)"", 
					(double)0.0, (char *)"", debug, message); 
			}
 
			if(strcmp(srcname, dlsrcname) == 0 || type == Pkt_db ) {
				if(strcmp(srcname, dlsrcname) == 0 ) {
					if(debug == 1) {
						new_now = now();
						duration = new_now - old_now;
						fprintf(stderr,"%s: %s %5.4f seconds\n", ss=strtime(time), srcname, duration );
							free(ss);
						old_now = new_now;
					}
					if ( pfread ( outpffile, &pf ) != 0 ) {
						fprintf(stderr, "Can not update %s for reading\n", outpffile ) ;
					} else
					  {
						if (parse_param (pf, "dls", P_PFARR, 1, &dbdls_arr) < 0) {
							fprintf(stderr,"%s:db2pipelinetag P_PFARR does not dls arrary\n", ss=strtime(time));
							free(ss);
						} else
						  {
							if (parse_param (pf, "dls", P_ARRPF, 1, &pfdls_arr) < 0) {
								fprintf(stderr,
								  "db2pipelinetag P_ARRPF does not dls array\n");
							} else
							  {
								dbdls_tbl = keysarr( dbdls_arr );
								numkey = maxtbl(dbdls_tbl);
								for (i=0; i<numkey; i++) {
									if((name = (char *) gettbl (dbdls_tbl, i)) != NULL) {

										found = 0;
										/* only process stations in list */
										for (j=0; j<param_info.numsta; j++) {
											/* disect network part */
											/* of station name */
											ni = 0; /* set name */
												/*index location */
											for (jj=0; jj < 
												strlen(name);++jj){
												if(name[jj]=='_') {
													if(jj+1 < strlen(name) ) {
													  ni=jj+1;
													}
													jj=strlen(name);
												}
											}
											if(strcmp(
											  (char *)&name[ni], 
											  param_info.Mytag[j].staname)
												== 0) {
												found = 1; 
												j = param_info.numsta;
											}
										}
										/* fold code found */
	if(found == 1) {
		typepfget = pfget((Pf *)pfdls_arr, (char *)name, (void **)&pf1);
		if(typepfget != PFARR ) {
			fprintf(stderr,"%s has Wrong Data type=%d \n", name, typepfget);
		} else
		  {
			s = pfget_string(pf1, "dlt");
			if(strcmp("-", s) == 0 || strlen(s) == 0 ) {
				dlstat.dlt = dlstatcut.dlt + 1.0;
			} else
			  {
				dlstat.dlt = (double) atof(s); 
			}
			free(s);
			s = pfget_string(pf1, "clt");
			if(strcmp("-", s) == 0 || strlen(s) == 0) {
				dlstat.clt = dlstatcut.clt + 1.0;
			} else
			  {
				dlstat.clt = (double) atof(s); 
			}
			free(s);
			s = pfget_string(pf1, "dv");
			if(strcmp("-", s) == 0 || strlen(s) == 0) {
				dlstat.dv = 0.0;
			} else
			  {
				dlstat.dv = (double) atof(s); 
			}
			free(s);
			/* dlstat.dlt = (double) pfget_double( pf1, "dlt"); */
			/* dlstat.clt = (double) pfget_double( pf1, "clt" ); */
			/* dlstat.dv = (double) pfget_double( pf1, "dv" ); */
			if((s = pfget_string ( pf1, "cals" )) != NULL) {
				if(strlen(s) > 0) {
					  new_dl_info(db, 1, (double)time, (char *)&name[ni], s, 
						(double)0.0, (char *)"", debug, ""); 
					  if( debug == 1) {
						if(!((double)dlstat.dlt < (double)dlstatcut.dlt &&
						   (double)dlstat.clt < (double)dlstatcut.clt &&
						   ((double)dlstat.dv >= (double)dlstatcut.dv 
						   || (double) dlstat.dv == (double) 0.0))) { 
					  		fprintf(stderr,"%s: %s %s Cals =%s, Dlt=%f,Clt=%f,Dv=%f\n", ss=strtime(time), srcname, name, s, dlstat.dlt, dlstat.clt, dlstat.dv);
							free(ss);
						}
					  }
					  free(s);
				} else
				  {
					  new_dl_info(db, 1, (double)time,(char *)&name[ni], "-", 
						(double)0.0, (char *)"", debug, "");
					  if( debug == 1 ) {	 
						if(!((double)dlstat.dlt < (double)dlstatcut.dlt &&
						   (double)dlstat.clt < (double)dlstatcut.clt &&
						   ((double)dlstat.dv >= (double)dlstatcut.dv 
						   || (double) dlstat.dv == (double) 0.0))) { 
					  		fprintf(stderr,"%s: %s %s Cals =, Dlt=%f,Clt=%f,Dv=%f\n", ss=strtime(time), srcname, name, dlstat.dlt, dlstat.clt, dlstat.dv);
							free(ss);
						}
					  }
				} /* if strlen */
			} /* if pfget_string */
		} /* if pfget */
/* unfold code found */
										} /* found == 1 */
									} /* if gettbl */
								} /* for */
								if(numkey > 0) {
									freetbl(dbdls_tbl, 0);
								} 
							} /* pfdls_arr */
						} /* dbdls_arr */
						(void) pffree((Pf *)pf); 
					} /* if pfread */
				} else 
				  {
					tmoff = strlen(srcname) - strlen(tmsrcname);
					if (tmoff < 0) tmoff = 0;
					if(strcmp(&srcname[tmoff], tmsrcname) == 0 && type == Pkt_db ) { /* else if srcname */
						if(outdbfile != NULL) {
							filep = fopen( outdbfile , "w+");
							showPkt ( pktid, srcname, time, packet, nbytes, filep, PKT_UNSTUFF ) ;
							fclose(filep);
						}
						dbpkt = unstuffed->db; 
						if(dbgetv(dbpkt, 0, "sta", tm.sta,
							"chan", tm.chan, "alarm_time",&(tm.alarm_time),
							"alarm_state",&(tm.alarm_state),
							"alarm_peak", &(tm.alarm_peak), "segtype",tm.segtype,
							0) == dbINVALID) {
								complain(0,"dbgetv orb db read problem\n");
						} else
						  {
							found = 0;
							/* only process stations in list */
							for (j=0; j<param_info.numsta; j++) {
								if(strcmp(tm.sta, param_info.Mytag[j].staname ) == 0) {
									found = 1; 
									j = param_info.numsta;
								}
							}
							if(found == 1) {
						  		new_dl_info(db, 2, (double)time, tm.sta, (char *)"", 
									(double)tm.alarm_time, 
									(char *)tm.alarm_state, debug, "");
								if(debug == 1) {
									fprintf(stderr,"%s: sta=%s, alarm_state=%s\n", 
										ss = strtime(tm.alarm_time), tm.sta, tm.alarm_state);
										free(ss);
								}
							}
						}
					} /* else if srcname */
				}
				/* (void) pffree ( (Pf *)pf ) ;  */
			} /* if fread */
		} /* if rcode */
    	} /* while(1) */
	if ( statefile != NULL ) bury() ;
} /* main */
