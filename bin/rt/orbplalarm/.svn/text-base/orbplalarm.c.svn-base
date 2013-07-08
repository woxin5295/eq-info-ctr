#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "brttpkt.h"
#include "Pkt.h"
#include "orb.h" 
#include "db.h"
#include "stock.h"
#include "bury.h"
#include "elog.h"

/* need to clean up unused include statements */
/*#include "response.h"
#include "coords.h"
#include "brttutil.h"
#include "arrays.h"
*/

static int verbose = 0;
static int debug = 0;

/* This is a structure containing program parameters */
typedef struct orbplalarm_ {
	int orbout;				/* output ORB for plalarm db packets */
	Dbptr dbout;			/* used to house the scratch record for the output ORB packets */
	int quit;				/* quit variable for exhume */
	double statetime;		/* state time for bury */
	double det_timeout;		/* timeout in seconds to quit looking at /db/wfmeas packets if
														 detection OFF isn't seen. */
	double report_interval;	/* Print current and previous alarm states for all stations
														 every report_time seconds */
	double null_endtime;	/* the default NULL value for endtime */

	Arr *Alarm_stations;	/* Array of stations to monitor. Keys are station names, 
								values are stationinfo structures. */

 	Arr *alarm_templates;	/* keys are template names and values are arrays of 
					   							 alarm criteria */
	Arr *alarm_levels;		/* table of strings that describe the alarm levels
												 	 low, medium, high, etc. */
	Arr *alarm_values;		/* actual cutoff values for alarm levels */
} Orbplalarm;

typedef struct stationinfo_ {
	char *station;
	int use_detection;
	char *detection_subset;
	char *wfmeas_subset;

	char *alarm_units;
	Tbl *alarm_levels;
	Tbl *alarm_values;

	double alarm_v;

	double detection_time;
	char *prev_plalarm;
	double prev_plalarm_time;
	char *curr_plalarm;
	double curr_plalarm_time;
} MyStationInfo;

static Orbplalarm orbplalarm_global;

/* local procedure declarations */
void usage();
void startup_banner();
int parse_pf (char *pfname, Orbplalarm *orbplalarm);
void processPkt ( int pktid, char *srcname, double time, char *packet, int nbytes, int mode, Orbplalarm *orbplalarm );
int send_alarm_db ( Dbptr db, Orbplalarm *orbplalarm, double time, double alarm_time, double endtime, char *alarm_state );
void report_status(Orbplalarm *orbplalarm);
MyStationInfo *new_stationinfo();

void
usage ()
{

	fprintf(stderr, "\n");
  fprintf(stderr, "usage: orbplalarm [-v] [-vv] [-start {pktid|time|OLDEST}] [-select select_expr]\n");
  fprintf(stderr, "[-reject reject_expr] [-pf pfname] orbin [orbout]\n\n");

}

int 
main (int argc, char **argv) 

{ 
	char *start = NULL;
	char *select_expr = "/db/(detection|wfmeas)";
	char *reject_expr = NULL;
	char *pfname = "orbplalarm" ;
	char *orbinname = NULL;
	char *orboutname = NULL;
	char *s;

	int orbin;
	int loop=1;
	int pktid, get;
	int mode = PKT_NOSAMPLES ;

	double time, endtime=0.0;
	double lastreporttime;

	Orbplalarm *orbplalarm ;

	/* initialize error and log reporting */
	elog_init (argc, argv);
  
	/* if not enough args, print usage line */
	if (argc < 2) {
		usage();
		exit (1);
	}

	startup_banner ();

	for (argc--,argv++; argc>0; argc--,argv++) {
		if (**argv != '-') break;
		if (!strcmp(*argv, "-start")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -start argument.\n");
				usage();
				exit (1);
			}
			start = *argv;
		} else if (!strcmp(*argv, "-end")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -end argument.\n");
				usage();
				exit (1);
			}
			if (zstr2epoch (*argv, &endtime) < 0) {
				complain (0, "Unable to parse -end time '%s'.\n", *argv);
				usage();
				exit (1);
			}
		} else if (!strcmp(*argv, "-select")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -select argument.\n");
				usage();
				exit (1);
			}
			select_expr = *argv;
		} else if (!strcmp(*argv, "-reject")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -reject argument.\n");
				usage();
				exit (1);
			}
			reject_expr = *argv;
		} else if (!strcmp(*argv, "-pf")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -pf argument.\n");
				usage();
				exit (1);
			}
			pfname = *argv;
		} else if (!strcmp(*argv, "-v")) {
			verbose = 1;
		} else if (!strcmp(*argv, "-vv")) {
			verbose = 2;
		} else if (!strcmp(*argv, "-vvv")) {
			verbose = 3;
		} else {
			complain (0, "Unrecognized option '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}
  
	/* parse command line for required args*/
	if (argc < 1) {
		complain (0, "Need orbinname argument.\n");
		usage();
		exit (1);
	}
	orbinname = *argv;
	argv++;
	argc--;

        if (argc > 0) {
                orboutname = *argv;
                argv++;
                argc--;
                if (argc > 0) {
                        complain (0, "Unrecognized argument '%s'.\n", *argv);
                        usage();
                        exit (1);
                }
        }

	
	/* Initialize orbplalarm structure */
	orbplalarm = &orbplalarm_global;
	memset (orbplalarm, 0, sizeof(Orbplalarm));

	/* parse the program parameter file */
	if (parse_pf (pfname, orbplalarm) < 0) {
		die (0, "parse_pf(%s) error.\n", pfname);
	}

	/* open up the input orb */
	if ( (orbin = orbopen ( orbinname, "r&" )) < 0 )
		die ( 0, "Can't open ring buffer '%s'\n", orbinname ) ; 

	/* apply the input select and reject keys to the input orb */
	if (orbselect (orbin, select_expr) < 0) {
		die (0, "orbselect(%s,%s) error.\n", orbinname, select_expr);
	}
	if (reject_expr) {
		if (orbreject (orbin, reject_expr) < 0) {
			die (0, "orbreject(%s,%s) error.\n", orbinname, reject_expr);
		}
	}

	/* set up the output orb and initialize dbout scratch record */
	orbplalarm->orbout = -1; 
	orbplalarm->orbout = orbopen (orboutname, "w&");
	if (orbplalarm->orbout < 0) {
		die (0, "orbopen(%s) error.\n", orboutname);
	} 

	orbplalarm->dbout = dbtmp("css3.0:plalarm");
	orbplalarm->dbout = dblookup( orbplalarm->dbout, 0, "plalarm", 0, "dbSCRATCH" );
	if (orbplalarm->dbout.table == dbINVALID) {
		die (0, "Table 'plalarm' does not appear to exist in css3.0:plalarm schema.\n");
	}
	
	/* position the input orb read pointer */
	pktid = ORBNEXT;
	get = 0;
	if (start) {
		if (!strcmp(start, "OLDEST")) {
			pktid = ORBOLDEST;
			get = 1;
		} else if (!strcmp(start, "NEWEST")) {
			pktid = ORBNEWEST;
			orbseek (orbin, pktid);
			orbseek (orbin, ORBPREV);
		} else {
			if (strlen(start) > 10 || strchr(start, ' ') || strchr(start, '.')
					|| strchr(start, ':') || strchr(start, '/')) {
				if (zstr2epoch (start, &time) < 0) {
					complain (0, "Unable to parse -start time '%s'.\n", start);
					usage();
					exit (1);
				}
				orbafter (orbin, time);
			} else {
				pktid = atoi(start);
				orbseek (orbin, pktid);
				orbseek (orbin, ORBPREV);
			}
		}
	}

	lastreporttime = now();

		/* read the orb packet - the little dance with get and orbget
       is necessary in order to properly implement the "-start OLDEST"
       startup so that the actual oldest packet is read instead
       of the next packet after the oldest packet , see orbwfrms.c (BRTT source code)*/
	while (loop) {
		double pkttime;
		char srcname[ORBSRCNAME_SIZE];
		static char *packet=NULL;
		static int bufsize=0;
		int nbytes;

		if (get) {
			orbget (orbin, pktid, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize);
			get = 0;
			orbseek (orbin, ORBOLDEST);
		} else orbreap (orbin, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize);
		if (verbose > 2) {
			char *s;
			s = strtime(pkttime);
			elog_debug (0, "read %s packet at %s.\n", srcname, s);
			free (s);
		}

		/* This does the real work */
		processPkt ( pktid, srcname, pkttime, packet, nbytes, mode, orbplalarm ) ;

		/* clear out the error register */
		clear_register (1);

		if (orbplalarm->report_interval > 0.0) {
			double nowtime;
		
			nowtime = now();
			if (nowtime-lastreporttime > orbplalarm->report_interval) {
				lastreporttime = nowtime;
				report_status (orbplalarm);
			}
		}

		/* normal exit */
	}
	exit (0);

}

/* parse the program parameter file */
int
parse_pf (char *pfname, Orbplalarm *orbplalarm)
     
{

	Pf *pf=NULL;
	Arr *alarmsta_arr;
	Tbl *alarmsta_tbl;

	Arr *alarmtemplates_arr;
	Tbl *alarmtemplates_tbl;

	Pf *pfalarmsta_arr;

	int i;

	/* Open and read pf file */
	if (pfread (pfname, &pf)) {
		register_error (0, "parse_pf: pfread('%s') error.\n", pfname);
		return (-1);
	}

	/* Initialize arrays for alarm templates and stations */
	orbplalarm->alarm_templates = newarr (0);
	if (orbplalarm->alarm_templates == NULL) {
		register_error (0, "parse_pf: newarr(alarm_templates) error.\n");
		return (-1);
	}

	orbplalarm->Alarm_stations = newarr (0);
	if (orbplalarm->Alarm_stations == NULL) {
		register_error (0, "parse_pf: newarr(Alarm_stations) error.\n");
		return (-1);
	}

	orbplalarm->alarm_levels = newarr (0);
	if (orbplalarm->alarm_levels == NULL) {
		register_error (0, "parse_pf: newarr(alarm_levels) error.\n");
		return (-1);
	}

	orbplalarm->alarm_values = newarr (0);
	if (orbplalarm->alarm_values == NULL) {
		register_error (0, "parse_pf: newarr(alarm_values) error.\n");
		return (-1);
	}

	/* ******** Start parsing the pf ************* */

	if (parse_param (pf, "report_interval", P_DBL, 1, &(orbplalarm->report_interval)) < 0) {
		register_error (0, "parse_pf: parse_param(report_interval) error.\n");
		return (-1);
	}

	/* Set the default NULL value for endtime field */
	orbplalarm->null_endtime = 9999999999.99900;

	/* Get the different alarm templates */
	if (parse_param (pf, "alarm_templates", P_PFARR, 1, &alarmtemplates_arr) < 0) {
		register_error (0, "parse_pf: parse_param(alarm_templates) error.\n");
		return (-1);
	}

	alarmtemplates_tbl = keysarr( alarmtemplates_arr );
	for (i=0; i<maxtbl(alarmtemplates_tbl); i++) {
		char *name;
		Pf *pfa;

		/* get the template name */
		name = (char *) gettbl (alarmtemplates_tbl, i);
		/* get the corresponding template pf pointer */
		pfa = (Pf *) getarr (alarmtemplates_arr, name);

		/* Place array in orbplalarm structure as a pf pointer */
		setarr (orbplalarm->alarm_templates, name, pfa); 
		if (verbose > 0) {
			elog_notify(0, "parse_pf: template %s \n", name);
		}

	}
	freetbl(alarmtemplates_tbl, 0);
	freearr(alarmtemplates_arr, 0);

	/* Now get the stations to be monitored. **************************
 *   This is a bit confusing, but will set up the orbplalarm structure
 *   to maximize conveniency in the processPkt subroutine, trying to 
 *   reduce the number of conditional loops. */
	if (parse_param (pf, "alarm_stations", P_PFARR, 1, &alarmsta_arr) < 0) {
		register_error (0, "parse_pf: parse_param(alarm_stations) error.\n");
		return (-1);
	}

	if (parse_param (pf, "alarm_stations", P_ARRPF, 1, &pfalarmsta_arr) < 0) {
		register_error (0, "parse_pf: parse_param(alarm_stations) error.\n");
		return (-1);
	}

	alarmsta_tbl = keysarr( alarmsta_arr );
	/*alarmsta_tbl = pfkeys( alarmsta_arr );*/
	for (i=0; i<maxtbl(alarmsta_tbl); i++) {
		char *name;
		Pf *pfs;
		int usedet;
		char *detsub;
		char *wfsub;
		char def[16];
		char *defdup;
		char *alarm_units;
		Tbl *thresh_tbl;
		int j;
		Tbl *level_tbl;
		Tbl *val_tbl;

		MyStationInfo *stationinfo;

		Pf *pfa;

		/* get the station name */
		name = gettbl (alarmsta_tbl, i);

		/* get the corresponding template pf pointer */
		/*pfs = (Pf *) getarr (alarmsta_arr, name);*/

		switch ( pfget(pfalarmsta_arr, name, (void **) &pfs) ) {
			case PFARR:
			/*elog_notify(0, "Found Arr for station %s in alarm_stations.\n", name);*/
			break;
			case PFSTRING:
			/*elog_notify(0, "Looking for template %s in alarm_templates.\n", pfget_string(pfalarmsta_arr, name));*/
			pfs = (Pf *) getarr (orbplalarm->alarm_templates, pfget_string(pfalarmsta_arr, name));
			if (pfs == NULL) {
				elog_notify(0, "Cannot find template %s in alarm_templates for station %s.\n", 
							pfget_string(pfalarmsta_arr, name), name);
				return(-1);
			}
			
			break;
		default:
			elog_complain(0, "Improper value in alarm_stations for station %s. Skipping.\n", name);
			continue;
			break;
		}
			
		if (parse_param (pfs, "use_detection", P_BOOL, 1, &usedet) < 0) {
			register_error (0, "parse_pf: parse_param(use_detection) error\n");
			return(-1);
		}	


		if (parse_param (pfs, "detection_subset", P_STR, 1, &detsub) < 0) {
			register_error (0, "parse_pf: parse_param(detection_subset) error\n");
			return(-1);
		}	


		if (parse_param (pfs, "wfmeas_subset", P_STR, 1, &wfsub) < 0) {
			register_error (0, "parse_pf: parse_param(wfmeas_subset) error\n");
			return(-1);
		}	


		/* Get the default units for the threshold levels in the pf */
		if (parse_param (pfs, "units", P_STR, 1, &alarm_units) < 0) {
			register_error (0, "parse_pf: parse_param(alarm_units) error\n");
			return(-1);
		}	

		/* ********************************************************* */
		/* Parse the thresholds and break them into two tables, one
 * 		 for the levels, one for the values. */
		if (parse_param (pfs, "thresholds", P_TBL, 1, &thresh_tbl) < 0) {
			register_error (0, "parse_pf: parse_param(thresholds) error\n");
			return(-1);
		}	

		level_tbl = newtbl(maxtbl(thresh_tbl));
		val_tbl = newtbl(maxtbl(thresh_tbl));
	
		for (j=0; j<maxtbl(thresh_tbl); j++) {
			char *line, lev[1024], valstr[1024];
			char *lev_dup, *val_dup;
			double val;
			int ret;

			/* get the entry from the thresholds table */
			line = (char *) gettbl (thresh_tbl, j);
	
			ret = sscanf (line, "%s %s", lev, valstr);
			if ( ret < 0 ) {
				register_error (1, "parse_pf: sscanf(%s) error for line number %d in thresholds table.\n",
												line, j);
				return(-1);
			}
			if (ret == 0) continue;
			if (ret == 1) {
				register_error (0, "parse_pf: need threshold level in %s for line number %d in thresholds table.\n", line, j);
				return(-1);
			}
			lev_dup = strdup(lev);
			if (lev_dup == NULL) {
				register_error (1, "parse_pf: strdup(lev) error at channels table line %d.\n", j);
				return(-1);
			}
			val_dup = strdup(valstr);
			if (val_dup == NULL) {
				register_error (1, "parse_pf: strdup(valstr) error at channels table line %d.\n", j);
				return(-1);
			}
			/*val = atof(valstr);*/
			settbl (level_tbl, j, lev_dup);
			settbl (val_tbl, j, val_dup);
			
		}

		stationinfo = new_stationinfo();
		/* may need to use duptbl for these? */
		stationinfo->alarm_levels = level_tbl;
		stationinfo->alarm_values = val_tbl;
		setarr (orbplalarm->alarm_levels, name, level_tbl);
		setarr (orbplalarm->alarm_values, name, val_tbl);
		/*freetbl(thresh_tbl);*/

		/* ********************************************************* */

		stationinfo->station = strdup(name);
		stationinfo->use_detection = usedet;
		stationinfo->detection_subset = strdup(detsub);
		stationinfo->wfmeas_subset = strdup(wfsub);
		stationinfo->alarm_units = strdup(alarm_units);
		/* This is where we might be able to read in an existing plalarm table
  		 to pre-set the current alarm status for stations in the parameter file.*/
		stationinfo->curr_plalarm = "OFF";
		stationinfo->prev_plalarm = "OFF";
		stationinfo->alarm_v = 0;

		if (verbose > 0) {
			elog_notify( 0, "parse_pf: station %s Status: %s\n", stationinfo->station, stationinfo->curr_plalarm );
		}
		setarr(orbplalarm->Alarm_stations, name, (void *) stationinfo);
	}
	freetbl(alarmsta_tbl, 0);
	freearr(alarmsta_arr, 0);

	return (0);
  
}

/* This is the main processing loop that decides what to do with
   reaped packets */

void
processPkt ( int pktid, char *srcname, double time, char *packet, int nbytes, int mode, Orbplalarm *orbplalarm ) 
{
	static Packet   *pkt=0 ;

	char sta[STRSZ];
	char det_state[STRSZ];
	double det_time;

	char meastype[STRSZ];
	char units1[STRSZ];
	double tmeas=0.0;
	double val1;
	int type;

	Dbptr db ;
	Dbptr dbs;

	int i;
	int nrecords;

	Arr *mystate;
	char *curr_alarm;
	char *prev_alarm;
	Pf *pfs;

	MyStationInfo *mystationinfo;


	if (verbose > 2) {
		showPkt ( pktid, srcname, time, packet, nbytes, stdout, mode ) ; 
	}

	type = unstuffPkt (srcname, time, packet, nbytes, &pkt) ;
	switch (type) {
	case Pkt_pf :
		/* Can add more cases for processing state of health */
	case Pkt_db :
		db = pkt->db ;
 
		/* Quick check right off the bat to see if we care about this station.
 * 		 If so, get the current and previous status for this station. */ 
		dbgetv( db, 0, "sta", &sta, NULL );
		mystationinfo = (MyStationInfo *) getarr ( orbplalarm->Alarm_stations, sta );

		if (mystationinfo == NULL) {
			if (verbose > 2) {
				elog_notify(0, "processPkt: Station %s not found in orbplalarm->Alarm_stations. Skipping packet.\n", sta);
			}
			break;
		}
		curr_alarm = mystationinfo->curr_plalarm;
		prev_alarm = mystationinfo->prev_plalarm;

		if (verbose > 2) {
			elog_notify( 0, "Station %s Current: %s Previous: %s\n", sta, curr_alarm, prev_alarm );
		}
		/* ************************************************ */	

		if ( strcmp(srcname, "/db/detection") == 0 && (mystationinfo->use_detection == 1)) {
			char *detsub;

			dbgetv( db, 0, "time", &det_time, "state", &det_state, NULL );

			detsub = mystationinfo->detection_subset;
			/* need provision for no detsub supplied in pf */
			dbs = dbsubset ( db, detsub, 0 );
			/* For some reason this subset returns nrecords of 0, 
*			 I think because db is a record instead of a view */
			dbquery ( dbs, dbRECORD_COUNT, &nrecords ) ;
			if (verbose > 1) {
				char *st;
				st = epoch2str(det_time, "%L/%d/%Y (%j) %k:%M:%S.%u");
				/*st = strtime(det_time);*/
				elog_debug( 0, "DETECTION %s time: %s %s nr %d \n", sta, st, det_state, nrecords );
				free(st);
			}
			if (strcmp(det_state, "D") == 0) {
				elog_notify(0, "Detection found for %s. Starting alarm sequence.\n", sta);
				elog_notify(0, "From mystationinfo: curr %s prev %s \n", curr_alarm, prev_alarm );
				mystationinfo->detection_time = det_time;
				/* This should set prev_alarm to OFF */
				mystationinfo->prev_plalarm = strdup(mystationinfo->curr_plalarm);
				mystationinfo->prev_plalarm_time = det_time;
				/* This should set curr_alarm to D */
				mystationinfo->curr_plalarm = strdup(det_state);
				mystationinfo->curr_plalarm_time = det_time;
				setarr( orbplalarm->Alarm_stations, sta, (void *) mystationinfo );
				send_alarm_db (db, orbplalarm, det_time, det_time, orbplalarm->null_endtime, "D" );
			} else if (strcmp(det_state, "OFF") == 0) {	

				elog_notify(0, "Detection turned OFF for %s. Stopping alarm sequence.\n", sta);
				elog_notify(0, "From mystationinfo: curr %s prev %s \n", curr_alarm, prev_alarm);
				/* Set the previous state to the previous current state */
				mystationinfo->prev_plalarm = strdup(mystationinfo->curr_plalarm);
				mystationinfo->prev_plalarm_time = mystationinfo->curr_plalarm_time;
				/* Set the current state */
				mystationinfo->curr_plalarm = strdup(det_state);
				mystationinfo->curr_plalarm_time = det_time;
				/* Reset the alarm value level */
				mystationinfo->alarm_v = 0;

				/* close out the original detection row */
				send_alarm_db (db, orbplalarm, mystationinfo->detection_time, mystationinfo->detection_time, 
								det_time, "D");

				/* Only close out previous alarm if it was NOT a D or OFF */
				if ((strcmp( prev_alarm, "D" ) != 0) || (strcmp( prev_alarm, "OFF" ) != 0) ) {
					elog_debug(0, "Detection turned OFF, closing open alarm %s.\n", curr_alarm);
					send_alarm_db (db, orbplalarm, mystationinfo->prev_plalarm_time, mystationinfo->prev_plalarm_time, 										det_time, mystationinfo->prev_plalarm);
				}

				setarr( orbplalarm->Alarm_stations, sta, (void *) mystationinfo );

			}
		} else if ( (strcmp(srcname, "/db/wfmeas") == 0)  && 
							( (strcmp(curr_alarm, "OFF") != 0) || (mystationinfo->use_detection == 0) ) ) {
			elog_notify(0, "DEBUG(/db/wfmeas): curr %s prev %s\n", curr_alarm, prev_alarm);
		/* Add timeout if it's been a while since detection in case the detection doesn't ever go OFF */

			char *meassub;
			int ret;
			char c_units[STRSZ];
			double c_val;
			char *myunits;
			Tbl *val_tbl;
			Tbl *level_tbl;
			int k;
			char *myalarm_level;

			dbgetv( db, 0, "meastype", &meastype, "tmeas", &tmeas, "val1", &val1, "units1", &units1, NULL );

			meassub = mystationinfo->wfmeas_subset;

			dbs = dbsubset ( db, meassub, 0 );
			/* For some reason this subset returns nrecords of 0 */
			dbquery ( dbs, dbRECORD_COUNT, &nrecords ) ;
			elog_debug( 0, "wfmeas %s meastype: %s Subset: %s nr %d \n", sta, meastype, meassub, nrecords );
			if (verbose > 1) {
				char *st;
				st = strtime(tmeas);
				elog_debug( 0, "tmeas %s %s %s %.2f\n", 
						st, sta, meastype, val1 );
				free(st);
			}
			/*if ( nrecords > 0 ) {
				elog_debug( 0, "wfmeas %s %s %s template %s\n", sta, s, meastype, meassub );
				break ; 
			} */

			myunits = mystationinfo->alarm_units;

			ret = units_convert( val1, units1, myunits, &c_val, c_units );
			if( ret == 1 ) {
				elog_complain( 0, "units_convert: '%s' did not match "
                           "request for '%s'\n", units1, myunits );
			} else if ( ret == -1 ) {
				elog_complain( 0, "units_convert: '%s' not recognized\n", units1 );
			}

			elog_notify(0, "Converted %.1f %s to %.8f %s\n", val1, units1, c_val, c_units);

			val_tbl = (Tbl *) getarr (orbplalarm->alarm_values, sta);
			level_tbl = (Tbl *) getarr (orbplalarm->alarm_levels, sta);
			for (k=0; k<maxtbl(val_tbl); k++) {
				char *v, *l;
				v = (char *) gettbl(val_tbl, k);
				l = (char *) gettbl(level_tbl, k);
				if (verbose > 2) {
					elog_debug( 0, "DEBUG(k=%d): level %s value %.8f c_val %.8f \n", k,l,atof(v),c_val);
				}
				if ( (fabs(c_val) < atof(v)) && k==0 ) {
					elog_notify(0, "DEBUG: ALARM: NONE (below lowest level)\n");
					if ( mystationinfo->use_detection == 0 && strcmp(curr_alarm, "OFF") != 0 ) {
						/* Close out last alarm if dropping below lowest threshold
  							and not using detections */
						elog_notify(0, "send it here 0\n");
						send_alarm_db ( db, orbplalarm, mystationinfo->curr_plalarm_time, 
										mystationinfo->curr_plalarm_time, tmeas, mystationinfo->curr_plalarm);
						mystationinfo->prev_plalarm = strdup(mystationinfo->curr_plalarm);
						mystationinfo->prev_plalarm_time = mystationinfo->curr_plalarm_time;
						mystationinfo->curr_plalarm = "OFF";
						mystationinfo->curr_plalarm_time = tmeas;
						mystationinfo->alarm_v = 0;
						setarr ( orbplalarm->Alarm_stations, sta, (void *) mystationinfo );
					}
					break;
				} else if (fabs(c_val) < atof(v)) {

					myalarm_level = (char *) gettbl(level_tbl, k-1);
					elog_notify(0, "ALARM: level %s v %.8f alarm_v %.8f prev %s\n", 
											myalarm_level, atof(v), mystationinfo->alarm_v, prev_alarm);

					if ( atof(v) > mystationinfo->alarm_v ) {

						if (strcmp(prev_alarm, "OFF") != 0 && strcmp(curr_alarm, "OFF") != 0) {
							/* This sends off an endtime for the current row because we've moved on to
								a higher level. */
							elog_notify(0, "send it here 1\n");
							send_alarm_db (db, orbplalarm, mystationinfo->curr_plalarm_time, mystationinfo->curr_plalarm_time,
											tmeas, mystationinfo->curr_plalarm );

						} else if (strcmp(prev_alarm, "OFF") == 0 && strcmp(curr_alarm, "OFF") != 0 
									&& mystationinfo->use_detection == 0) {
							elog_notify(0, "send it here 11\n");
							send_alarm_db (db, orbplalarm, mystationinfo->curr_plalarm_time, mystationinfo->curr_plalarm_time,
											tmeas, mystationinfo->curr_plalarm );
						}

						/* Now update status  and send off new row for higher level. */
						mystationinfo->prev_plalarm = strdup(mystationinfo->curr_plalarm);
						mystationinfo->prev_plalarm_time = mystationinfo->curr_plalarm_time;
						mystationinfo->curr_plalarm = strdup(myalarm_level);
						mystationinfo->curr_plalarm_time = tmeas;
						mystationinfo->alarm_v = atof(v);

						setarr ( orbplalarm->Alarm_stations, sta, (void *) mystationinfo );

						elog_notify(0, "send it here 2\n");
						send_alarm_db (db, orbplalarm, tmeas, tmeas, orbplalarm->null_endtime, myalarm_level);
					}
					break;
					/* Reached the end of threshold values */
				} else if (fabs(c_val) >= atof(v) && k == maxtbl(val_tbl)-1 ) {

					myalarm_level = (char *) gettbl(level_tbl, k);
					elog_notify(0, "ALARM: Highest alarm level %s\n", myalarm_level);

					if ( atof(v) >  mystationinfo->alarm_v ) {

						if (strcmp(prev_alarm, "OFF") != 0) {
							/* This sends off an endtime for the current row because we've moved on to
								a higher level. */

							elog_notify(0, "send it here 3\n");
							send_alarm_db ( db, orbplalarm, mystationinfo->curr_plalarm_time, mystationinfo->curr_plalarm_time,
											tmeas, mystationinfo->curr_plalarm );

						}
						/* Now update status and send off new row for higher level. */
						mystationinfo->prev_plalarm = strdup(mystationinfo->curr_plalarm);
						mystationinfo->prev_plalarm_time = mystationinfo->curr_plalarm_time;
						mystationinfo->curr_plalarm_time = tmeas;
						mystationinfo->curr_plalarm = strdup(myalarm_level);
						mystationinfo->alarm_v = atof(v);

						setarr( orbplalarm->Alarm_stations, sta, (void *) mystationinfo );

							elog_notify(0, "send it here 4\n");
						send_alarm_db (db, orbplalarm, tmeas, tmeas, orbplalarm->null_endtime, myalarm_level);
					}
					break;
				}	 /* End of conditional loop over values */
			} /* End of loop over val_tbl */
		} /* End of if /db/wfmeas */

		break ; 
		
	default:
		elog_notify ( 0, "unrecognized packet type %d: %s\n", type, srcname ) ; 
		break ; 
	} /* End of switch statement */

} /* End of processPkt */

int
send_alarm_db ( Dbptr db, Orbplalarm *orbplalarm, double time, double alarm_time, double endtime, char *alarm_state )
{
	
	char sta[16];
	char *table;
	
	static Packet *pkt=NULL;
	static char *packet=NULL;
	static int packetsz=0;
	int nbytes;
	double pkttime;
	char srcname[ORBSRCNAME_SIZE];

	dbquery( db, dbTABLE_NAME, &table );
	dbgetv( db, 0, "sta", &sta, NULL );

	if (verbose > 1) {
		elog_notify(0, "send_alarm_db: STA %s\n"
						"alarm_time:  %s\n"
						"endtime:     %s\n"
						"alarm_state: %s\n", sta, strtime(alarm_time), strtime(endtime), alarm_state);
	}

	if (orbplalarm->orbout < 0) return (0);

	/* We should come into this subroutine with the database
      pointer, orbplalarm->dbout, already set to point to table "plalarm" */

	/* null out the scratch record */
	orbplalarm->dbout.record = dbNULL;
	orbplalarm->dbout.field = dbALL;
	dbget (orbplalarm->dbout, 0);
	orbplalarm->dbout.record = dbSCRATCH;

	if ( dbputv( orbplalarm->dbout, 0,	"sta", sta, 
                                     	"time", time, 
										"endtime", endtime,
										"alarm_time", alarm_time,
       		                            "alarm_state", alarm_state, 
       	                             	"auth", "orbplalarm", 
                                      	NULL ) < 0 ) {
                register_error (0, "send_alarm_db: dbputv() error.\n");
				return(-1);
	}

        if (pkt == NULL) {
             pkt = newPkt();
             if (pkt == NULL) {
                     die (0, "send_alarm_db: newPkt() error.\n");
             }
             pkt->pkttype = suffix2pkttype ("db");
        }

        pkt->db = orbplalarm->dbout;

        /* stuff the packet into a raw ORB output packet */
        if (stuffPkt (pkt, srcname, &pkttime, &packet, &nbytes, &packetsz) < 0) {
                register_error (0, "send_alarm_db: stuffPkt() error.\n");
                return (-1);
        }

        /* output the ORB packet */
        while (orbput (orbplalarm->orbout, srcname, pkttime, packet, nbytes)) {
                char *s;
                s = strtime (time);
                complain (0, "send_alarm_db: orbput(%s,%s at %s) error.\n",
                                srcname, sta, s);
                free (s);
        }

	return(0);

}

void
startup_banner()

{

	elog_notify (0, "****************************************************\n");
	elog_notify (0, "orbplalarm startup\n");
	elog_notify (0, "****************************************************\n");
	elog_notify (0, "\n\n");

}

void 
report_status ( Orbplalarm *orbplalarm )
{
	int i;
	Tbl *sta_tbl;


	sta_tbl = (Tbl *) keysarr ( orbplalarm->Alarm_stations );
	printf("ALARM STATIONS STATUS:\n");
	for (i=0; i<maxtbl(sta_tbl); i++) {
		char *sta;
		char *c_alarm;
		char *p_alarm;
		Arr *state;

		MyStationInfo *stationinfo;

		sta = (char *) gettbl(sta_tbl, i);
		stationinfo = (MyStationInfo *) getarr(orbplalarm->Alarm_stations, sta);

		c_alarm = stationinfo->curr_plalarm;
		p_alarm = stationinfo->prev_plalarm;

		printf("%s: Current %s Previous %s\n", sta, c_alarm, p_alarm);	
	}
}

MyStationInfo *
new_stationinfo()
{
	MyStationInfo *stationinfo;

/*	stationinfo = (MyStationInfo *) malloc ( sizeof(MyStationInfo) );*/
	allot(MyStationInfo *, stationinfo, 1);

	stationinfo->station = 0;
	stationinfo->use_detection = -1;
	stationinfo->detection_subset = 0;
	stationinfo->wfmeas_subset = 0;
	stationinfo->alarm_units = 0;
	stationinfo->alarm_levels = newtbl(0);
	stationinfo->alarm_values = newtbl(0);
	stationinfo->alarm_v = 0;
	stationinfo->detection_time = 0;
	stationinfo->prev_plalarm = 0;
	stationinfo->prev_plalarm_time = 0;
	stationinfo->curr_plalarm = 0;
	stationinfo->curr_plalarm_time = 0;
	
	return stationinfo;
}
