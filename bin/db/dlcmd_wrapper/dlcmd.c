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
#include <sys/wait.h>

struct par_info {
	char *orbfbks;
	char *mysql_hosts_fbks, *mysql_hosts_occ;
	char *mysql_user_fbks, *mysql_user_occ;
	char *mysql_pass_fbks, *mysql_pass_occ;
	char *mysql_database_fbks, *mysql_database_occ;
	int MinKeepTestOn; /* default 120 seconds minimun amount of time to keep test bit on */
	int TestOnCushion; /* default 60 seconds Amount of time to cushion test bit */
	int debug;
};

struct par_info param_info;

int num_sta_list = 11;
struct sta_list_struct {
	char sta[8];
	int station_id;
};
	
struct sta_list_struct sta_list[11];

int get_param_file_info(char *pfname) {
	static Pf *pf = 0;
	/* get parmeter file information */
	if ( pfread ( pfname, &pf ) != 0 ) {
		complain ( 0, "Can not read parameter file: %s\n, exit", pfname ) ;
		exit(1);
        }
	param_info.orbfbks = pfget_string ( pf, "orbfbks" );
	if (param_info.orbfbks == NULL) {
		register_error (0, "parse_pf: pfget_string(orbfbks) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires orbnfks\n");
		exit(1);
	}
	param_info.mysql_hosts_fbks = pfget_string ( pf, "mysql_hosts_fbks" );
	if (param_info.mysql_hosts_fbks == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_hosts_fbks) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_hosts_fbks\n");
		exit(1);
	}
	param_info.mysql_hosts_occ = pfget_string ( pf, "mysql_hosts_occ" );
	if (param_info.mysql_hosts_occ == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_hosts_occ) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_hosts_occ\n");
		exit(1);
	}
	param_info.mysql_user_fbks = pfget_string ( pf, "mysql_user_fbks" );
	if (param_info.mysql_user_fbks == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_user_fbks) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_user_fbks\n");
		exit(1);
	}
	param_info.mysql_user_occ = pfget_string ( pf, "mysql_user_occ" );
	if (param_info.mysql_user_occ == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_user_occ) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_user_occ\n");
		exit(1);
	}
	param_info.mysql_pass_fbks = pfget_string ( pf, "mysql_pass_fbks" );
	if (param_info.mysql_pass_fbks == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_pass_fbks) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_pass_fbks\n");
		exit(1);
	}
	param_info.mysql_pass_occ = pfget_string ( pf, "mysql_pass_occ" );
	if (param_info.mysql_pass_occ == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_pass_occ) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_pass_occ\n");
		exit(1);
	}
	param_info.mysql_database_fbks = pfget_string ( pf, "mysql_database_fbks" );
	if (param_info.mysql_database_fbks == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_database_fbks) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_database_fbks\n");
		exit(1);
	}
	param_info.mysql_database_occ = pfget_string ( pf, "mysql_database_occ" );
	if (param_info.mysql_database_occ == NULL) {
		register_error (0, "parse_pf: pfget_string(mysql_database_occ) error\n");
		fprintf(stderr,"dlcmd_wrapper.pf requires mysql_database_occ\n");
		exit(1);
	}
	if (parse_param (pf, "MinKeepTestOn", P_LINT, 1, &param_info.MinKeepTestOn) < 0) {
		param_info.MinKeepTestOn = 120; /* default 120 seconds */
	} 
	if (parse_param (pf, "TestOnCushion", P_LINT, 1, &param_info.TestOnCushion) < 0) {
		param_info.TestOnCushion = 60; /* default 60 seconds */
	} 
	if (parse_param (pf, "debug", P_LINT, 1, &param_info.debug) < 0) {
		param_info.debug = 0;
	} 
}

int main(int argc, char **argv) {
    int x,xx, debug;
    char sta_name[8];
    int station_id;
    int sta_found, calibrate;
    int cal_cmd_found;
    char *cmdargs[128], antelopedlcmd[512];
    int num_cmdargs, time_period;
    char *antelopedir, now_time[128];
    char pfname[256];
    int orblogpid, cmdpid, statusp;
    char *forkorblogargs[6];
    char orblogargs[6][64];
    int maxretries, max_retries, verified;
    double starttime, nowtime;

    /* mysql select results */
    MYSQL_RES *result_f, *result_a; /* _f fairbanks, _a anchorage */
    MYSQL_ROW row_f, row_a;
    MYSQL *connection_f, mysql_f, *connection_a, mysql_a;
    int myver_f, myver_a, value_f, value_a ; /* verified */
    int state_f, state_a; 
    char mysql_cmdstr[512];

    (void) strcpy (sta_list[0].sta, "PS_PS01"); sta_list[0].station_id = 1;
    (void) strcpy (sta_list[1].sta, "PS_PS04"); sta_list[1].station_id = 4;
    (void) strcpy (sta_list[2].sta, "PS_PS05"); sta_list[2].station_id = 5;
    (void) strcpy (sta_list[3].sta, "PS_PS06"); sta_list[3].station_id = 6;
    (void) strcpy (sta_list[4].sta, "PS_PS07"); sta_list[4].station_id = 7;
    (void) strcpy (sta_list[5].sta, "PS_PS08"); sta_list[5].station_id = 8;
    (void) strcpy (sta_list[6].sta, "PS_PS09"); sta_list[6].station_id = 9;
    (void) strcpy (sta_list[7].sta, "PS_PS10"); sta_list[7].station_id = 10;
    (void) strcpy (sta_list[8].sta, "PS_PS11"); sta_list[8].station_id = 11;
    (void) strcpy (sta_list[9].sta, "PS_PS12"); sta_list[9].station_id = 12;
    (void) strcpy (sta_list[10].sta, "PS_VMT"); sta_list[10].station_id = 13;

    max_retries = 12; /* retry setting test bit 10 times */  
    debug = 0; /* can be set to 1 from parameter file */

    /* hardware pfname into code */
    (void) strcpy(pfname,"/iwrun/acq/run/pf/dlcmd_wrapper.pf");

    nowtime = (double) now();
    if ((antelopedir = getenv("ANTELOPE")) == NULL) {
	/* hard wire if ANTELOPE is not set */  
	(void) strcpy(antelopedlcmd, "/opt/antelope/4.10/bin/dlcmd");
    } else
      {
	(void) sprintf(antelopedlcmd, "%s/bin/dlcmd", antelopedir);
    }
    cmdargs[0] = (char *) &antelopedlcmd[0];
    cmdargs[1] = (char *) &antelopedlcmd[0];
    num_cmdargs = 2; /* next command argument to be loaded */

    sta_found = cal_cmd_found = time_period = 0;
    /* parse dlcmd argument argument list, pass all arguments to antelope dlcmd */
    /* or intercept calibration commands */
    for (x = 1; x < argc; ++x) {
	cmdargs[num_cmdargs] = argv[x];
	num_cmdargs++;
	/* station name */
	for (xx = 0; xx < num_sta_list; ++xx) {
		if( strncmp(sta_list[xx].sta, argv[x], strlen(sta_list[xx].sta)) == 0) {
			(void) strcpy(sta_name, sta_list[xx].sta);
			station_id = sta_list[xx].station_id;
			sta_found = 1;
		}
	}
	/* calibrate */
	if( strncmp("calibrate", argv[x], 9) == 0) {
		calibrate = 1;
	}
    }
    /* set matrix test bit,  check bit, execute antelope dlcmd */
    verified = 1; 
    if (calibrate == 1 && sta_found == 1) { 
	/* get paramter file information */
	get_param_file_info(pfname);
	debug = param_info.debug;

    	/* orblog -s pipelinemsg aeicpipe:6510 "Test PS11 on" */
    	(void) strcpy(orblogargs[0], "orblog");
	forkorblogargs[0] = &orblogargs[0][0];
    	(void) strcpy(orblogargs[1], "orblog");
	forkorblogargs[1] = &orblogargs[1][0];
    	(void) strcpy(orblogargs[2], "-s");
	forkorblogargs[2] = &orblogargs[2][0];
    	(void) strcpy(orblogargs[3], "pipelinemsg");
	forkorblogargs[3] = &orblogargs[3][0];
    	(void) strcpy(orblogargs[4], param_info.orbfbks);
	forkorblogargs[4] = &orblogargs[4][0];
    	(void) sprintf(orblogargs[5],"Test %s on", &sta_name[3]); /* do not need PS_ part of name */  
	forkorblogargs[5] = &orblogargs[5][0];
	forkorblogargs[6] = NULL; 

	/* set mysql test bit on aeicpipe and aeicems */
	/* orblog message setup */
	/* create child process  and start fork */
	maxretries = verified = myver_f = myver_a = 0;
	while (maxretries < max_retries && verified == 0 ) { 
		orblogpid = fork();
		/* child process */
		if (orblogpid == 0) {
			if (debug == 1) {
				fprintf(stderr,"%s %s %s %s %s %s\n", forkorblogargs[0],
				forkorblogargs[1], forkorblogargs[2], forkorblogargs[3],
				forkorblogargs[4], forkorblogargs[5]);
			}
			execvp(forkorblogargs[0], &forkorblogargs[1]);
		} else if (orblogpid == -1) {
			if (debug == 1) {
				fprintf(stderr,"Can not fork %s \n", forkorblogargs[0]);
			}
		/* parent wait for child to finish */
		} else if (orblogpid > 0) {
			(void) wait(&statusp);
		}
		/* verify test bit has been set */
		sleep(maxretries+1);
		/* connect to the mySQL database in Fairbanks */
		if (myver_f == 0 ) {
			mysql_init(&mysql_f);
			connection_f = mysql_real_connect(&mysql_f, param_info.mysql_hosts_fbks,
				param_info.mysql_user_fbks, param_info.mysql_pass_fbks,
				param_info.mysql_database_fbks, 0, 0, 0);
			if( connection_f == NULL ) {
				/* print the error message */
				printf(mysql_error(&mysql_f));
			} else
			  {
				sprintf(mysql_cmdstr,"SELECT test * 1 FROM occ_display where station_id=%d", station_id);
				if(debug == 1) {
					fprintf(stderr, "FBKS: %s\n", mysql_cmdstr);
				}
				state_f = mysql_query(connection_f, mysql_cmdstr);
				if( state_f != 0 ) {
					printf(mysql_error(connection_f));
				} else
				  {
					result_f = mysql_store_result(connection_f);
					while( ( row_f = mysql_fetch_row(result_f)) != NULL ) {
						value_f = atoi(row_f[0]);
						if(value_f == 1) myver_f = 1;
					}
					/* free the result set */
					mysql_free_result(result_f); 
				}
				/* close the connection */
				mysql_close(connection_f); 
			}
		}
		/* connect to the mySQL database in Anchorage */
		if (myver_a == 0 ) {
			mysql_init(&mysql_a);
			connection_a = mysql_real_connect(&mysql_a, param_info.mysql_hosts_occ,
				param_info.mysql_user_occ, param_info.mysql_pass_occ,
				param_info.mysql_database_occ, 0, 0, 0);
			if( connection_a == NULL ) {
				/* print the error message */
				printf(mysql_error(&mysql_a));
			} else
			  {
				sprintf(mysql_cmdstr,"SELECT test * 1 FROM occ_display where station_id=%d", station_id);
				if(debug == 1) {
					fprintf(stderr,"OCC: %s\n", mysql_cmdstr);
				}
				state_a = mysql_query(connection_a, mysql_cmdstr);
				if( state_a != 0 ) {
					printf(mysql_error(connection_a));
				} else
				  {
					result_a = mysql_store_result(connection_a);
					while( ( row_a = mysql_fetch_row(result_a)) != NULL ) {
						value_a = atoi(row_a[0]);
						if(value_a == 1) myver_a = 1;
					}
					/* free the result set */
					mysql_free_result(result_a); 
				}
				/* close the connection */
				mysql_close(connection_a); 
			}
		}
		/* test bit is set on both mysql servers */
		if( myver_f == 1 && myver_a == 1) {
			verified = 1;
		}
		if( verified == 0 ) {
			maxretries++;
		}
	}
	if (maxretries >= max_retries) {
		if (debug == 1) {
			fprintf(stderr,"Can not send orlog message %s \n", forkorblogargs[5]);
		}
		/* cancel test bit if sent */   
		if( myver_f == 1 || myver_a == 1) {
    			(void) sprintf(orblogargs[5],"Test %s off", &sta_name[3]); /* do not need PS_ part of name */
			forkorblogargs[5] = &orblogargs[5][0];
			orblogpid = fork();
			/* child process */
			if (orblogpid == 0) {
				execvp(forkorblogargs[0], &forkorblogargs[1]);
			} else if (orblogpid == -1) {
				if (debug == 1) {
					fprintf(stderr,"Can not fork cancel bit %s \n", forkorblogargs[0]);
				}
			/* parent wait for child to finish*/
			} else if (orblogpid > 0) {
				(void) wait(&statusp);
			}
		}
		exit(1);
	}

	/* parse dlcmd argument argument list again */
	num_cmdargs = 2; /* next command argument to be loaded */
  	for (x = 1; x < argc; ++x) {
		cmdargs[num_cmdargs] = argv[x];
		num_cmdargs++;
		/* reset time to now */
		if( strncmp("-time", argv[x], 5) == 0) {
			if ((x + 1) < argc) {
				starttime = (double) str2epoch(argv[x+1]);
				if((double) starttime > (double) nowtime ) {
					/* starttime can not be more than MinKeepTestOn */
					/* in the future */
					if((double)(starttime - nowtime) > (double) param_info.MinKeepTestOn) {
						starttime = nowtime + (double) param_info.MinKeepTestOn;
					}
				} else
				  {
					starttime = (double) nowtime;
				}
				time_period = time_period + (starttime - nowtime);
			} else
			  {
				/* this should never happen */
				starttime = (double) nowtime;
			} 
			/* hard wire calibration start time to now */
    			(void) sprintf(now_time,"%f",starttime);
    			/* for(x = 0; x < strlen(now_time); ++x) {
			*	if(now_time[x] == '.') now_time[x] = '\0';
    			*}
			*/
			cmdargs[num_cmdargs] = (char *) &now_time[0];
			num_cmdargs++;
			++x; /* skip pass next argument */
		}
		if( strncmp("-duration", argv[x], 9) == 0 ||
		    strncmp("-settling_time", argv[x], 14) == 0 ||
		    strncmp("-trailer_time", argv[x], 13) == 0 ) {
			if ((x + 1) < argc) {
				time_period = time_period + atoi(argv[x+1]);
			} 
		}
    	}
    }
    cmdargs[num_cmdargs] = NULL;
    if( debug == 1) {
	if (calibrate == 1 && sta_found == 1) {
		/* parameter file information */
		fprintf(stderr,"\torbfbks = %s\n\tmysql_hosts_fbks = %s\n\tmysql_hosts_occ = %s\n\tmysql_user_fbks = %s\n\tmysql_user_occ = %s\n\tmysql_pass_fbks = %s\n\tmysql_pass_occ = %s\n\tmysql_database_fbks = %s\n\tmysql_database_occ = %s\n\tMinKeepTestOn = %d\n\tTestOnCushion = %d\n\tdebug = %d\n", 
		param_info.orbfbks, param_info.mysql_hosts_fbks,
		param_info.mysql_hosts_occ, param_info.mysql_user_fbks, 
		param_info.mysql_user_occ, param_info.mysql_pass_fbks, 
		param_info.mysql_pass_occ, param_info.mysql_database_fbks, 
		param_info.mysql_database_occ,  param_info.MinKeepTestOn, 
		param_info.TestOnCushion, param_info.debug);
		fprintf(stderr,"\n nowtime=%f,starttime=%f,time_period = %d \n", 
				nowtime, starttime,time_period);
	} 
	
	/* print out dlcmd */
	fprintf(stderr,"dlcmd_wrapper: ");
	for(x=0; x < num_cmdargs; ++x) {
		fprintf(stderr,"%s ", cmdargs[x]);
	}

    }
    /* submit dlcmd */
    if ( verified == 1 ) {
	cmdpid = fork();
	/* child process */
	if (cmdpid == 0) {
		execvp(cmdargs[0], &cmdargs[1]);
	} else if (cmdpid == -1) {
		if (debug == 1) {
			fprintf(stderr,"Can not fork %s \n", cmdargs[0]);
		}
	/* parent wait for child to finish */
	} else if (cmdpid > 0) {
		(void) wait(&statusp);
	}
    } 
    if (calibrate == 1 && sta_found == 1) { 
	
	/* sleep time_period */
	time_period = time_period + param_info.TestOnCushion;
	if ( time_period < param_info.MinKeepTestOn ) time_period = param_info.MinKeepTestOn;	
	sleep(time_period);
    	(void) sprintf(orblogargs[5],"Test %s off", &sta_name[3]); /* do not need PS_ part of name */
	forkorblogargs[5] = &orblogargs[5][0];
	orblogpid = fork();
	/* child process */
	if (orblogpid == 0) {
		execvp(forkorblogargs[0], &forkorblogargs[1]);
	} else if (orblogpid == -1) {
		if (debug == 1) {
			fprintf(stderr,"Can not fork %s \n", forkorblogargs[0]);
		}
	/* parent wait for child to finish*/
	} else if (orblogpid > 0) {
		(void) wait(&statusp);
	}
    }
}
