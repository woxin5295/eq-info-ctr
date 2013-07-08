/* missing: 
   statefile does not work correctly
   single-station naming unclear
   units transformation: not implemented
   delay when closing: swith immediate to lower level or after waittime
   verbose flags more clear
   manpage
*/

/* Copyright (c) 2004 Boulder Real Time Technologies, Inc. */
/* Copyright (c) 2008 Nikolaus Horn / Vienna               */


/* All rights reserved */

/* This software may be used freely in any way as long as
   the copyright statements above are not removed. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "brttutil.h"
#include "brttpkt.h"
#include "Pkt.h"
#include "orb.h" 
#include "db.h"
#include "response.h"
#include "arrays.h"
#include "coords.h"
#include "stock.h"
#include "bury.h"
#include "orbtm_version.h"

static int verbose = 0;
static int debug = 3;

#define OFF 0
#define LOW 1
#define MEDIUM 2
#define HIGH 3

Xlat xlat_alarm_level[] = {
	"high",   HIGH,
	"low",    LOW,
	"medium", MEDIUM,
	"off", OFF
} ;

int nxlat_alarm_level = sizeof(xlat_alarm_level) / sizeof(Xlat) ;



/* Following are structure definitions */

/* This is a structure for containing processing parameters
   for a particular frequency-band */
typedef struct filter_params_ {
	char *filter_string;	/* Filter specification */
	double fpad;			/* Filter time pad for rmoving transients */
	double fc;				/* center frequency */
	double statetime;		/* time for last state (bury()) */
	double time;			/* time of first sample in current contiguous buffer */
	double nexttime;		/* time of next sample in current contiguous buffer */
	double procstarttime;	/* time of new samples, i.e. time to start processing */
	double dt;				/* sampling interval in current contiguous buffer */
	int nsamps;				/* number of samples in current contiguous buffer */
	int sizeof_data;		/* size in bytes of current contiguous data buffer */
	float *data;			/* current contiguous data buffer */
	Hook *filter_hook;		/* hook for trfilter_pkt(3) */
	double calib;			/* calib value */
	char segtype[4];		/* segtype code */
	double alarm_on_time;	/* alarm on */
	double minimum_ontime;
	int alarm_state;		/* current state */
	double alarm_time;
	double alarm_value;
	double low;
	double medium;
	double high;
	int output_individual_channels;
	double conversion_factor;
	char *tag;
	char *units;
} MyFilterParams;

typedef struct status_params_ {
	char *filter_string;
	char *segtype;
	char *tag;
	char *sta;
	char *sta_tag;
	double statetime;
	double alarm_on_time;	/* alarm on */
	int alarm_state;		/* current state */
	double alarm_time;
	double alarm_value;
	double low;
	double medium;
	double high;
	char *units;
	double conversions_factor;
	double h_value,h_state,h_time;
} AlarmStatus;

/* This is a structure for containing processing parameters
   for a particular station-channel */
typedef struct channel_params_ {
	int calib_from_db;		/* Get calib values from db? */
	int maxpkts;			/* Maximum size of time-ordered packet queue */
	int orbout;			/* output ORB for wfrms db packets */
	Dbptr dbout;			/* used to house the scratch record for the output ORB packets */
	PktChannelCalib *pcc;		/* pktchannelcalib(3) pointer for getting calib, etc. from database */
	Tbl *bands;			/* list of MyFilterParams structures for processing */
} MyChannelParams;

/* This is a structure for containing program parameters */
typedef struct Orbtm_ {
	PktChannelPipe *pcp;		/* pktchannelpipe(3) pointer for processing ORB packets */
	PktChannelCalib *pcc;		/* pktchannelcalib(3) pointer for getting calib, etc. from database */
	int maxpkts;			/* Default maximum size of time-ordered packet queue */
	int orbout;			/* output ORB for wfrms db packets */
	int flush;			/* time to flush out everything */
	int quit;			/* quit varibale set by exhume() signal handler */
	double statetime;		/* state time for bury() */
	Dbptr dbout;			/* used to house the scratch record for the output ORB packets */
	Arr *templates;			/* keys are template names and values
					   are pointers to MyChannelParams structures */
	Tbl *channels;			/* Table of channel expressions to process */
	Tbl *channels_for_pcp;		/* Table of channel expressions plus an extra maxpkts field
					   for input to pktchannelpipe_new(3). */
	Tbl *channels_template;		/* This is a list of the template names corresponding
					   to each of the expressions in the channels table. */
	Tbl *channels_reject;		/* Table of channel expressions to reject processing */
	Arr *channels_arr;		/* This is keyed to full SEED net_sta_chan[_loc] 
							   codes and its values are pointers to MyChannelParams
							   structure. This is filled dynamically as data is
							   processed */
	Arr *stations_arr; /* This is keyed to net_sta */
} Orbtm;

static Orbtm Orbtm_global;

Tbl *new_sta_tags;
static Arr *GlobalAlarmStatus;

/* Following are local prucedure declerations */

int mycallback (void *private, PktChannel *pktchan, int queue_code, double gaptime);
int myprocessbuffer (PktChannel *pktchan, MyChannelParams *cp, MyFilterParams *fp, 
			char *sta, char *chan, double calib, char *segtype);
int myflushbuffer (MyChannelParams *cp, MyFilterParams *fp, char *sta, char *chan, int force);
int myflush (Orbtm *Orbtm);
int findstate (double time, int nsamps, double dt, float *data,
				double tstart, double tend, int p2p, double low, double medium, double high, double *statevalue, double *statetime);
int outstate (Dbptr dbcp, int orbout, char *prefix, char *sta, char *chan, double time, double alarm_time, long state, double alarm_peak, 
		char *filter, char *segtype);
int statetime (Orbtm *Orbtm);
void mybury();
int parse_pf (char *pfname, Orbtm *Orbtm);
MyChannelParams *mychannelparams_new (MyChannelParams *def, Pf *pf);
void myfilterparams_free (void *ptr);
MyFilterParams *myfilterparams_new (MyFilterParams *def, Pf *pf);
void startup_banner();

double TIME_NULL;

/* helper function(s) */
void free_string (void *s) {
	free( s );
}
int cmp_string( char **a, char **b, void *private ) {
	return strcmp( *a, *b );
}
/* this is the main program */

int
main (int argc, char **argv)

{
	char *start = NULL;
	char *select_expr = NULL;
	char *reject_expr = NULL;
	char *pfname = "orbtm";
	char *statefile = NULL;
	char *dbname = NULL;
	char *orbinname = NULL;
	char *orboutname = NULL;

	Orbtm *Orbtm;
	int orbin;
	int pktid, get;
	double time, endtime=0.0;
	double lastburytime;
	int loop=1;
	long state1;
	
	/* initialize error and log reporting */
	elog_init (argc, argv);

	/* if not enough args, print usage line and banner */
	if (argc < 3) {
		cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email) ;
		exit (1);
	}

	/* or print startup banner if there are enough args */
	startup_banner ();

	/* parse command line for options */
	for (argc--,argv++; argc>0; argc--,argv++) {
		if (**argv != '-') break;
		if (!strcmp(*argv, "-start")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -start argument.\n");
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email) ;
				exit (1);
			}
			start = *argv;
		} else if (!strcmp(*argv, "-select")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -select argument.\n");
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email) ;
				exit (1);
			}
			select_expr = *argv;
		} else if (!strcmp(*argv, "-reject")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -reject argument.\n");
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email) ;
				exit (1);
			}
			reject_expr = *argv;
		} else if (!strcmp(*argv, "-end")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -end argument.\n");
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email) ;
				exit (1);
			}
			if (zstr2epoch (*argv, &endtime) < 0) {
				complain (0, "Unable to parse -end time '%s'.\n", *argv);
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
				exit (1);
			}
		} else if (!strcmp(*argv, "-pf")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -pf argument.\n");
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
				exit (1);
			}
			pfname = *argv;
		} else if (!strcmp(*argv, "-S")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -S argument.\n");
				cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
				exit (1);
			}
			statefile = *argv;
		} else if (!strcmp(*argv, "-v")) {
			verbose = 1;
			debug=0;
		} else if (!strcmp(*argv, "-vv")) {
			verbose = 2;
			debug=3;
		} else if (!strcmp(*argv, "-vvv")) {
			verbose = 3;
			debug=4;
		} else {
			complain (0, "Unrecognized option '%s'.\n", *argv);
			cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
			exit (1);
		}
	}

	/* parse command line for required arguments */
	if (argc < 1) {
		complain (0, "Need dbname argument.\n");
		cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
		exit (1);
	}
	dbname = *argv;
	argv++;
	argc--;

	if (argc < 1) {
		complain (0, "Need orbin argument.\n");
		cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
		exit (1);
	}
	orbinname = *argv;
	argv++;
	argc--;

	/* error exit if extra unrecognized arguments */
	if (argc > 0) {
		orboutname = *argv;
		argv++;
		argc--;
		if (argc > 0) {
			complain (0, "Unrecognized argument '%s'.\n", *argv);
			cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
			exit (1);
		}
	}

	GlobalAlarmStatus=newarr(0);
	/* setup the pktchannelcalib(3) pointer for getting CSS sta and chan
	   and also calib info from a database */
	Orbtm = &Orbtm_global;
	memset (Orbtm, 0, sizeof(Orbtm));

	Orbtm->pcc = pktchannelcalib_new (dbname, 1, 0, 0);
	if (Orbtm->pcc == NULL) {
		die (0, "pktchannelcalib_new(%s) error.\n", dbname);
	}
	Orbtm->channels_arr = newarr (0);
	if (Orbtm->channels_arr == NULL) {
		die (0, "newarr(Orbtm->channels_arr) error.\n");
	}
	Orbtm->stations_arr = newarr (0);
	if (Orbtm->stations_arr == NULL) {
		die (0, "newarr(Orbtm->stations_arr) error.\n");
	}

	/* Setup the output database scratch record and the output ORB handle.
	   Do it here so that these parameters can be passed down to the 
	   MyChannelParams structures in parse_pf() */
	Orbtm->orbout = -1;
	if (orboutname) {
		/* open the database */
		Orbtm->dbout = dbtmp ("css3.0" );
		/* make sure the wfrms table exists in this schema */
		Orbtm->dbout = dblookup (Orbtm->dbout, 0, "tm", 0, "dbNULL");
		if (Orbtm->dbout.table == dbINVALID) {
			die (0, "Table 'tm' does not appear to exist in %s database.\n",
							dbname);
		}
		if (dbgetv(Orbtm->dbout, 0, "time", &TIME_NULL, 0)<0) {
			die (0, "Problem obtaining NULL value for time or endtime from database %s\n",
							dbname);
		}
		Orbtm->dbout = dblookup (Orbtm->dbout, 0, "tm", 0, "dbSCRATCH");
		/* open the output ORB */
		Orbtm->orbout = orbopen (orboutname, "w&");
		if (Orbtm->orbout < 0) {
			die (0, "orbopen(%s) error.\n", orboutname);
		}
	}

	/* parse the program parameter file */
	if (parse_pf (pfname, Orbtm) < 0) {
		die (0, "parse_pf(%s) error.\n", pfname);
	}

	/* open the input ORB */
	orbin = orbopen (orbinname, "r&");
	if (orbin < 0) {
		die (0, "orbopen(%s) error.\n", orbinname);
	}

	/* apply the input select and reject keys to the input orb */
	if (select_expr) {
		if (orbselect (orbin, select_expr) < 0) {
			die (0, "orbselect(%s,%s) error.\n", orbinname, select_expr);
		}
	}
	if (reject_expr) {
		if (orbreject (orbin, reject_expr) < 0) {
			die (0, "orbreject(%s,%s) error.\n", orbinname, reject_expr);
		}
	}

	/* Initialize for statefile processing. This involves 1) setting
	   up signal handlers with exhume(), 2) parsing the input statefile,
	   if it exists, with resurrect() and 3) positioning the input ORB
	   read pointer. */
	if (statefile) {
		if ( exhume ( statefile, &(Orbtm->quit), RT_MAX_DIE_SECS, 0 ) != 0 ) {
			elog_notify ( 0, "read old state file\n" ) ;  
	    	}
		/* resurrect(3) is where we parse the state file. The "relic"
		   stuff is necessary for the resurrect(3) interface (i.e. read
		   the man page and look at bury.h). */
	    	if ( orbresurrect ( orbin, &pktid, &time ) == 0 )  {
				char *s;
				elog_notify ( 0, "resurrection successful: repositioned to time %s\n", 
								s=strtime(time)) ;
				free(s) ;
			} else {
				complain ( 0, "resurrection unsuccessful: time not set\n" ) ;  
			}
		lastburytime = now();
	}

	/* position the input orb read pointer according to the -start
	   command line argument. Note that -start will override the
	   ORB read pointer positioning by the state file. */
	pktid = ORBNEXT;
	get = 0;
	if (start) {
		if (statefile) {
			complain (0, "WARNING - -start arguments override statefile initialization of ORB read pointer.\n");
		}
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
					cbanner (Orbtm_Version,Orbtm_Usage,Orbtm_Author,Orbtm_Location,Orbtm_Email);
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

	/* create the PktChannelPipe structure by calling
	   pktchannelpipe_new (see pktchannelpipe(3)) */
	Orbtm->pcp = pktchannelpipe_new (Orbtm->channels_for_pcp, Orbtm->channels_reject,
			0, mycallback, Orbtm);
	if (Orbtm->pcp == NULL) {
		die (0, "pktchannelpipe_new() error.\n");
	}

	/* input orb packet read loop */
	while (loop) {
		double pkttime;
		char srcname[ORBSRCNAME_SIZE];
		Srcname parts;
		char *sta_tag;
		static char *packet=NULL;
		static int bufsize=0;
		int nbytes;
		int ret;
		AlarmStatus *as;
		char *ts;

		/* read the orb packet - the little dance with get and orbget
		   is necessary in order to properly implement the "-start OLDEST"
		   startup so that the actual oldest packet is read instead
		   of the next packet after the oldest packet */
		if (get) {
			orbget (orbin, pktid, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize);
			get = 0;
			orbseek (orbin, ORBOLDEST);
		} else orbreap (orbin, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize);
		if (debug > 3) {
			char *s;
			s = strtime(pkttime);
			elog_debug (0, "read %s packet at %s.\n", srcname, s);
			free (s);
		}

		new_sta_tags=newtbl(0);
		/* now process the packet with pktchannelpipe_push 
		   (see pktchannelpipe(3)) */
		ret = pktchannelpipe_push (Orbtm->pcp, srcname, pkttime, packet, nbytes);
		if (ret < 0) {
			die (0, "pktchannelpipe_push fatal error for %s at %s.\n",
								srcname, strtime(pkttime));
		}

		/* clear out any lingering error messages */
		clear_register (1);

		if (debug > 4) {
			char *s;
			s = strtime(pkttime);
			elog_debug (0, "processed %s packet at %s.\n", srcname, s);
			free (s);
		}


		while (sta_tag=(char *) poptbl(new_sta_tags)) {
			int out=0;
			if (cntarr(GlobalAlarmStatus) > 0) {
				// hierhin mit dem Kram...
				if ((as = (AlarmStatus *) getarr (GlobalAlarmStatus, sta_tag))!= NULL) {
					if ( (int) as->h_state != (int) as->alarm_state || 
						(double) as->h_value > (double) as->alarm_value) {
						if ((int) as->alarm_state== (int)OFF && 
							(int) as->h_state != (int)OFF) {
							/* new alarm */
							as->alarm_on_time=	(double) as->h_time;
							as->alarm_time=		(double) as->h_time;
							as->alarm_state=	(int) as->h_state;
							as->alarm_value=	(double) as->h_value;
							out=1;
						} else if ((int)as->h_state == (int) OFF && 
							(int) as->alarm_state != (int) OFF) {
							/* alarm off */
							as->alarm_time= (double) as->h_time;
							as->alarm_state= (int) as->h_state;
							out=1;
						} else if ( (int) as->h_state > (int)as->alarm_state)  {
							/* alarm escalation */
							as->alarm_state= (int) as->h_state;
							as->alarm_value= (double) as->h_value;
							as->alarm_time= (double) as->h_time;
							out=1;
						} else if ((int)as->h_state != (int)OFF && 
							(int) as->h_state == (int) as->alarm_state && 
							(double) as->h_value > (double) as->alarm_value) {
							/* same level but higher value */
							//as->alarm_state=as->h_state;
							as->alarm_value=(double) as->h_value;
							as->alarm_time=(double) as->h_time;
							out=1;
						}
					}
					if (out) {
						state1 = (long) as->alarm_state;
						if (outstate (Orbtm->dbout, Orbtm->orbout, as->tag , as->sta, "3C", 
								as->alarm_on_time, as->alarm_time,  state1, as->alarm_value, 
								as->filter_string, as->segtype ) < 0) {
							ts = strtime(as->statetime);
							register_error (0, "myflushbuffer: %s: %s %s:%s: outstate() error\n",
								ts, as->sta, "3C", as->filter_string); free (ts);
						}
					}
				} else {
					printf("should never happen...\n");
				}
			}

		}
			/*these were CERTAINLY memory leaks...*/
		freetbl(new_sta_tags,free_string);
		free(sta_tag);

		/* check for a shutdown signal */
		if (Orbtm->quit) {
			elog_notify (0, "Shutdown signal received - breaking out of read loop.\n");
			loop = 0;
		}

		/* skip the endtime processing if the packet just pushed was not used
		   in the pktchannelpipe buffers (pktchannelpipe_push() will return
		   0 if the packet is not used or 1 if it us used) */
		if (ret == 0) continue;	

		/* quit if endtime exceeded */
		if (endtime > 0.0 && pkttime > endtime) {
			loop = 0;
		}

		/* every so often (like every 10 minutes) bury the current state
		   time - we do this incase the program gets egregiously killed,
		   like with a kill -9 */
		if (statefile) {
			double nowtime;
			nowtime = now();
			if (nowtime-lastburytime > 60.0) {
				lastburytime = nowtime;
				bury();
			}
		}
	}

	/* flush out everything */

	/* normal exit */
	exit (0);
}

/* This is the main processing callback procedure that does most of
   the real work. This is where most of the customization would be done
   in a new ORB waveform processing program */
int
mycallback (void *private, PktChannel *pktchan, int queue_code, double gaptime)
{
	Orbtm *Orbtm = private;	
	char netstachanloc[64], *s;
	MyChannelParams *cp;
	double calib, calper;
	double dbcalib, dbcalper;
	char sta[16], chan[16], segtype[4];
	char dbsegtype[4];
	static double lastchecktime=0.0;
	static int check=0;
	double timenow;
	int i, ret,new_cp=0;
	char *tmp_str;
	char sta_tag[64];

	/* form up the SEED net_sta_chan[_loc] code */
	strcpy (netstachanloc, pktchan->net);
	strcat (netstachanloc, "_");
	strcat (netstachanloc, pktchan->sta);
	strcat (netstachanloc, "_");
	strcat (netstachanloc, pktchan->chan);
	if (pktchan->loc[0]) {
		strcat (netstachanloc, "_");
		strcat (netstachanloc, pktchan->loc);
	}

	s=strtime(pktchan->time);

	/* first thing - if this is a duplicate packet, toss it and return */
	if (queue_code == PKTCHANNELPIPE_DUP) { 
		if (verbose > 0) elog_notify (0, "%s: %s: duplicate packet\n", s, netstachanloc); 
		freePktChannel (pktchan);
		free (s);
		return (0);
	}

	/* get the channel params that go with this pktchan */
	cp = (MyChannelParams *) getarr (Orbtm->channels_arr, netstachanloc);

	/* The next bit of code gets executed once whenever a new station-channel
	   appears. This part figures out which template to use, makes a copy of
	   the MyChannelParams structure and sets the channels_arr. */
	if (cp == NULL) {
		Tbl *tmp_tbl;
		int i;
		char *name;
		MyChannelParams *cpc;

		/* find the entry in the channels table */
		tmp_tbl = tblgrep (Orbtm->channels, netstachanloc);
		if (maxtbl(tmp_tbl) < 1) {
			die (0, "mycallback: big problem with Orbtm->channels table.\n");
		}
		i = (int) gettbl (tmp_tbl, 0);
		freetbl (tmp_tbl, 0);

		/* now find the corresponding template name in the channels_template
		   table */
		name = (char *) gettbl (Orbtm->channels_template, i);

		/* find the MyChannelParams pointer from the templates array */
		cp = (MyChannelParams *) getarr (Orbtm->templates, name);
		if (cp == NULL) {
			die (0, "mycallback: big problem with Orbtm->templates(%s) table.\n", name);
		}

		/* We will first create a new copy of the MyChannelParams structure
		   before we set it in the channels_arr array. The reason for this
		   is so we can use this to hold channel-dependent state info that
		   will not conflict with other channels. Remember that the MyChannelParams
		   structure from the templates table can refer to multiple channels
		   since it is associated with a single template. */

		cpc = mychannelparams_new (cp, NULL);
		if (cpc == NULL) {
			die (0, "mycallback: mychannelparams_new(%s) error.\n", netstachanloc);
		}

		/* set the MyChannelParams pointer into the channels_arr array
		   so we don't have to do this again */
		setarr (Orbtm->channels_arr, netstachanloc, cpc);
		cp = cpc;

		if (debug > 3) elog_notify (0, "%s: %s: Adding channel with maxpkts = %d for processing.\n", 
							s, netstachanloc, cp->maxpkts);
		new_cp=1;

	}

	/* get CSS sta, chan, calib and segtype */
	calib = pktchan->calib;
	strcpy (segtype, pktchan->segtype);

	/* this code is put in to force a check on database file 
	   modification times every 10 seconds */
	timenow = now();
	check = 0;
	if (timenow > lastchecktime + 10.0) {
		lastchecktime = timenow;
		check = 1;
	}

	ret = pktchannelcalib_get (cp->pcc, pktchan->net, pktchan->sta,
			pktchan->chan, pktchan->loc, pktchan->time, check,
			sta, chan, 0, 0, 0, &dbcalib, &dbcalper, dbsegtype, 0);
	if (ret < 0) {
		die (0, "mycallback: pktchannelcalib_get(%s at %s) fatal error.\n", netstachanloc, s);
	}


	/* take calib and segtype from database values */
	if (cp->calib_from_db) {
		/* complain if the calib was not found in the database */
		switch (ret) {
		case 0:
			if (verbose > 0) complain (0, "mycallback: calib, calper, segtype not found in database for %s.\n", netstachanloc);
		break;
		case (PKTCHANNELCALIB_CALIB | PKTCHANNELCALIB_CALPER):
			if (verbose > 0) complain (0, "mycallback: segtype not found in database for %s.\n", netstachanloc);
			calib = dbcalib;
			break;
		default:
			calib = dbcalib;
			strcpy (segtype, dbsegtype);
			break;
		}
	}

	/* loop through the filter bands processing buffers */
	for (i=0; i<maxtbl(cp->bands); i++) {
		MyFilterParams *fp;
		Tbl *tlist;

		fp = (MyFilterParams *) gettbl (cp->bands, i);
		if (new_cp) {
			char *units,*desc;
			char nunits[1024];
			double factor;
			if ( trlookup_segtype(segtype,&units,&desc)!=0 ) {
				die(0,"mycallback: trlookup_segtype %s for %s not found.\n",units,netstachanloc);
			}
			if (units_convert(1.0,fp->units,units,&factor,nunits ) !=0 ) {
				elog_complain(0,"mycallback: units %s for %s not found. Setting conversion factor to 1! Please fix this!\n",units,netstachanloc);
				factor=1.0;
			}	
			fp->low *= factor;
			fp->medium *= factor;
			fp->high *= factor;
		}

		/* This is where we determine if this packet-channel
	           is time-continuous with the last one processed. If
		   it is, then go ahead and add it to the existing
		   processing buffer for this channel-band. If it is not,
		   or there is any other reason to flush/re-initialize
		   the processing buffer, such as changes to calib or samprate,
		   then flush out the buffer and re-initialize it. */

		/* First check the return code from the pktchannel pipe */
		switch (queue_code) {
		case PKTCHANNELPIPE_OVERLAP:
			if (verbose > 0) elog_notify (0, "%s: %s:%s - %.6f Overlap in data.\n", 
								s, netstachanloc, fp->filter_string, gaptime);
			myflushbuffer (cp, fp, sta, chan, 1);
			break;
		case PKTCHANNELPIPE_GAP:
			if (verbose > 0) elog_notify (0, "%s: %s:%s - %.6f Gap in data.\n", 
								s, netstachanloc, fp->filter_string, gaptime);
			myflushbuffer (cp, fp, sta, chan, 1);
			break;
		case PKTCHANNELPIPE_EARLY:
			if (verbose > 0) elog_notify (0, "%s: %s:%s - %.6f Early data.\n", 
								s, netstachanloc, fp->filter_string, gaptime);
			myflushbuffer (cp, fp, sta, chan, 1);
			break;
		default:
			break;
		}

		/* Now we check to see if samprate or calib has changed.
	   	   if so, then force a buffer flush  */
		if (fp->nsamps > 0) {
			if (!TRSAMERATE(1.0/fp->dt,pktchan->samprate)) {
				if (verbose > 0) elog_notify (0, "%s: %s %s:%s: Flush due to samprate change.\n",
									s, sta, chan, fp->filter_string);
				myflushbuffer (cp, fp, sta, chan, 1);
			} else if (calib != fp->calib) {
				if (verbose > 0) elog_notify (0, "%s: %s %s:%s: Flush due to calib change.\n",
									s, sta, chan, fp->filter_string);
				myflushbuffer (cp, fp, sta, chan, 1);
			} 
		}

		/* Now we process this packet */
		strcpy(sta_tag, pktchan->sta);
		strcat(sta_tag, "_");
		strcat(sta_tag, fp->tag);
		/* missing tag */

		tmp_str=strdup(sta_tag);
		tlist=tblgrep(new_sta_tags,tmp_str);
		if (maxtbl(tlist) <=0 ) {
			pushtbl(new_sta_tags,tmp_str);
		} 
		freetbl(tlist,free_string);
		myprocessbuffer (pktchan, cp, fp, sta, chan, calib, segtype);

		/* finally - if Orbtm->flush is set, then do a forced flush */
		if (Orbtm->flush) myflushbuffer (cp, fp, sta, chan, 1);
	}

	/* free up the PktChannel */
	freePktChannel (pktchan);
	free (s);
	return (0);
}

/* These are the routines that compute and output the peak values */

/* This routine accepts the PktChannel from the ORB, copies the data into
   an internal floating point buffer, processes the data (apply calib
   and filter), and then calls myflushbuffer to flush out the rms time
   windows. */
int
myprocessbuffer (PktChannel *pktchan, MyChannelParams *cp, MyFilterParams *fp, 
			char *sta, char *chan, double calib, char *segtype)

{
	int size;
	int ret;
	int statenow;
	int i, p2p=0;
	/*
	   double tstart, rms, min, max, tmin, tmax, v_alert, t_alert;
   */

	/* skip out if no input samples or bad samprate */
	if (pktchan->nsamp < 1) return (0);
	if (pktchan->samprate <= 0.0) return (0);
	if (calib == 0.0) calib = 1.0;

	/* allocate new data space if necessary */
	size = (fp->nsamps+pktchan->nsamp)*sizeof(float);
	if (size > fp->sizeof_data) {
		if (size < 10000) size = 10000;
		else size *= 2;
		if (fp->data == NULL) {
			fp->data = (float *) malloc (size);
			if (fp->data == NULL) {
				/* keep on or die? */
				die (1, "myprocessbuffer: malloc(%s,%s,%d) error.\n",
							sta, chan, size);
			}
		} else {
			fp->data = (float *) realloc (fp->data, size);
			if (fp->data == NULL) {
				/* keep on or die? */
				die (1, "myprocessbuffer: realloc(%s,%s,%d) error.\n",
							sta, chan, size);
			}
		}
		if (debug > 3) elog_debug (0, "allocating %d samples for %s %s:%s.\n",
							size/4, sta, chan, fp->filter_string);
		fp->sizeof_data = size;
	}

	/* set up the time, calib and segtype */
	if (fp->nsamps == 0) {
		fp->time = pktchan->time;
		fp->nexttime = pktchan->time;
		fp->calib = calib;
		strcpy (fp->segtype, segtype);
		/*
		fp->trms = fp->time + fp->fpad;
		fp->lastrms = 0.0;
		*/
		fp->statetime = fp->time - 100.0;
	}

	/* copy PktChannel samples into buffer applying calib */
	for (i=0; i<pktchan->nsamp; i++) fp->data[fp->nsamps+i] = fp->calib * pktchan->data[i];

	/* filter the data */
	fp->dt = 1.0/pktchan->samprate;
	fp->nexttime += pktchan->nsamp * fp->dt;
	fp->procstarttime=pktchan->time;
	ret = trfilter_pkt (pktchan->nsamp, fp->dt, &(fp->data[fp->nsamps]), 
							fp->filter_string, &(fp->filter_hook));
	if (ret == -1) {
		//better not die here...
		die (0, "myprocessbuffer: trfilter_pkt(%s,%s) fatal error.\n", sta, chan);
	}
	clear_register (1);
	fp->nsamps += pktchan->nsamp;

	/* process data */
	myflushbuffer (cp, fp, sta, chan, 0);

	return (0);
}

/* This routine is called whenever it is time to either 1) look
   for and dispatch time segments for rms averaging or 2) force
   a flushing of the internal floating point data buffer (by
   setting the force flag). Forced flushing should happen
   whenever there is some kind of discontinuity in the data
   timing, or certain data parameters, the sample rate and
   calib. */
int
myflushbuffer (MyChannelParams *cp, MyFilterParams *fp, char *sta, char *chan, int force)

{
	double endtime;
	int statenow;
	double statevalue,statetime;
	char *ts;
	char *sta_tag;
	AlarmStatus *as;
	int out=0,clear=0;
	long state1;

	/* free up the filter state info if this is a forced
	   flush (meaning that there is a time gap or other
	   discontinuity requiring a complete flush and
	   re-initialization) */
	if (force && fp->filter_hook) {
		if (debug > 2) {
			ts = strtime(fp->nexttime);
			elog_notify (0, "%s: %s %s:%s: re-initializing filter state\n",
						ts, sta, chan, fp->filter_string);
			free (ts);
		}
		free_hook (&(fp->filter_hook));
		fp->filter_hook = NULL;
	}

	/* eventually declare new alarm status */
	endtime = fp->nexttime - fp->dt;
	out=0;
	statenow = findstate (fp->time, fp->nsamps, fp->dt, fp->data, 
				fp->procstarttime, endtime, 0, fp->low, fp->medium, fp->high, &statevalue, &statetime);
	if ((int)statenow != (int)fp->alarm_state || (double)statevalue > (double)fp->alarm_value) {
		if ((int)fp->alarm_state==(int)OFF && (int)statenow != (int)OFF) {
			/* new alarm */
			fp->alarm_on_time=	(double) statetime;
			fp->alarm_time=		(double) statetime;
			fp->alarm_state=	(int) statenow;
			fp->alarm_value=	(double) statevalue;
			out=1;
		} else if ((int)statenow== (int)OFF && (int)fp->alarm_state != (int)OFF) {
			/* alarm off */
			//fp->alarm_on_time=	statetime;
			fp->alarm_time= (double) statetime;
			fp->alarm_state=	(int) statenow;
			fp->alarm_value=	(double) statevalue;
			out=1;
		} else if ((int)statenow > (int)fp->alarm_state)  {
			/* alarm escalation */
			//fp->alarm_on_time=	statetime;
			fp->alarm_time=(double) statetime;
			fp->alarm_state=(int) statenow;
			fp->alarm_value=(double)statevalue;
			out=1;
		} else if ((int)statenow != (int)OFF && (int)statenow == (int)fp->alarm_state 
			&& (double) statevalue > (double) fp->alarm_value) {
			/* same level but higher value */
			//fp->alarm_on_time=	statetime;
			fp->alarm_time=(double) statetime;
			fp->alarm_state=(int) statenow;
			fp->alarm_value=(double) statevalue;
			out=1;
		}
		if (out) {
			if (debug > 2) {
				ts= strtime(fp->statetime); 
				if (debug > 2) elog_debug(0, "changed! state:%s %s %s (%.0f) @ %s\n", sta, chan, xlatnum(statenow, xlat_alarm_level, nxlat_alarm_level), statevalue, ts); 
				free(ts);
			}
			if (fp->output_individual_channels == 0) {
				state1 = (long) fp->alarm_state;
				if (outstate (cp->dbout, cp->orbout, (char *) NULL, sta, chan, 
							fp->alarm_on_time, fp->alarm_time,  state1, fp->alarm_value, 
							fp->filter_string, fp->segtype ) < 0) {
					ts = strtime(fp->statetime);
					register_error (0, "myflushbuffer: %s: %s %s:%s: outstate() error\n",
						ts, sta, chan, fp->filter_string); free (ts);
				}
			}
		}
	}

	/* force out the remainder of the buffer and re-initialize */
	if (force) {
		
		/* this will re-initialize the buffer */
		if (fp->nsamps > 0) {
			fp->nsamps = 0;
			free (fp->data);
			fp->data = NULL;
			fp->sizeof_data = 0;
		}
	}

	/* we need to do some housekeeping - don't let the floating
	   point buffer grow unbounded */
	if (fp->nsamps > 5000) {
		int i;

		i = 1000;
		if (i > 0 && i < fp->nsamps-1) {
			int size;

			size = (fp->nsamps-i)*sizeof(float);
			memmove (fp->data, &(fp->data[i]), size);
			fp->time += fp->dt*i;
			fp->nsamps -= i;
			if (debug > 3) elog_debug (0, "removing %d samples from %s %s:%s.\n",
							i, sta, chan, fp->filter_string);
		}
	} 
	sta_tag=malloc(32);
	strcpy(sta_tag,sta);
	strcat(sta_tag,"_");
	strcat(sta_tag,fp->tag);

	if(cntarr(GlobalAlarmStatus) > 0) {
		if ((as = (AlarmStatus *) getarr (GlobalAlarmStatus, sta_tag))== NULL) {
			as=(AlarmStatus *) malloc(sizeof(AlarmStatus));
			as->sta=strdup(sta);
			as->tag=strdup(fp->tag);
			as->sta_tag=strdup(sta_tag);
			as->filter_string=strdup(fp->filter_string);
			as->segtype=strdup(fp->segtype);
			as->alarm_state=OFF;
			setarr(GlobalAlarmStatus,sta_tag,memdup(as,sizeof(AlarmStatus)));
			as->h_value=0;as->h_state=OFF;
		//} else {
		}
	} else {
		as=(AlarmStatus *) malloc(sizeof(AlarmStatus));
		as->sta=strdup(sta);
		as->tag=strdup(fp->tag);
		as->sta_tag=strdup(sta_tag);
		as->filter_string=strdup(fp->filter_string);
		as->segtype=strdup(fp->segtype);
		as->alarm_state=OFF;
		setarr(GlobalAlarmStatus,sta_tag,memdup(as,sizeof(AlarmStatus)));
		as->h_value=0;as->h_state=OFF;
	}

	if (statevalue> as->h_value) {
		as->h_value= statevalue;
		as->h_time=  statetime;
		as->h_state= statenow;
	}
	free(sta_tag);
	return (0);
}

/* Find the peak in some time window */
int
findstate (double time, int nsamps, double dt, float *data,
				double tstart, double tend, int p2p, double low, double medium, double high, double *statevalue,  double *statetime)

{
	int i, i0, i1;
	int n;
	int statenow;
	double min,max;
	double tmin,tmax;
	int imin,imax;

	i0 = TIME2SAMP (time, 1.0/dt, tstart);
	i1 = TIME2SAMP (time, 1.0/dt, tend);
	if (i0 < 0) i0 = 0;
	if (i1 > nsamps-1) i1 = nsamps-1;
	

	min=1.0e10;max=-1.0e10;.0;
	for (i=i0,n=0; i<=i1; i++,n++) {
		if (data[i] > max) {
			imax=i;
			max=data[i];
		}
		if (data[i] < min) {
			imin=i;
			min=data[i];
		}
		
	}
	tmin=SAMP2TIME(time,1.0/dt,imin);
	tmax=SAMP2TIME(time,1.0/dt,imax);
	
	if (p2p) {
		*statevalue=fabs(max) - fabs(min);
		*statetime=(tmax + tmin) / 2.0;
	} else {
		if (fabs(max) > fabs(min)) {
			*statevalue=fabs(max);
			*statetime=tmax;
		} else {
			*statevalue=fabs(min);
			*statetime=tmin;
		}
	}
	statenow=OFF;
	if (*statevalue > high) {
		statenow=HIGH;
	} else if (*statevalue > medium) {
		statenow=MEDIUM;
	} else if (*statevalue > low) {
		statenow=LOW;
	} else {
		statenow=OFF;
	}
	return (statenow);
}

/* output a new orbtm database row as a packet to the output ORB */
int
outstate (Dbptr dbout, int orbout, char *prefix, char *sta, char *chan, double time, double alarm_time, long state, double alarm_peak, 
		char *filter, char *segtype)

{
	static Packet *pkt=NULL;
	static char *packet=NULL;
	static int packetsz=0;
	int nbytes;
	char *alarm_state_ret;
	char alarm_state[36];
	double pkttime;
	char srcname[ORBSRCNAME_SIZE];
	if (orbout < 0) return (0);

	/* We (should) come into this subroutine with the database
	   pointer, cp->dbout, already set to point to table "tm" */

	/* null out the scratch record */
	dbout.record = dbNULL;
	dbout.field = dbALL;
	dbget (dbout, 0);
	dbout.record = dbSCRATCH;

	/* fill in the scratch record */
	if ((alarm_state_ret = xlatnum(state,xlat_alarm_level,nxlat_alarm_level)) != NULL) {
		(void) strcpy(alarm_state, alarm_state_ret);
	} else
	  {
		alarm_state[0] = '-';
		alarm_state[1] = '\0';
	}
	if (dbputv (dbout, 0, 	"sta", (char *)sta,
					"chan", (char *) chan,
					"time", (double) time,
					"alarm_time", (double) alarm_time,
					"alarm_state",(long) alarm_state,
					"alarm_peak", (double) alarm_peak,
					"filter", filter,
					"segtype", segtype,
					0) < 0) {
		register_error (0, "outrms: dbputv() error.\n");
		return (-1);
	}

	/* initialize the Packet structure used for output */
	if (pkt == NULL) {
		/* allocate */
		pkt = newPkt();
		if (pkt == NULL) {
			die (0, "outrms: newPkt() error.\n");
		}
		/* set the packet type to "db" */
		pkt->pkttype = suffix2pkttype ("db");
	}
	if (prefix) {
		strncpy(pkt->parts.src_net, prefix, PKT_TYPESIZE);
	} else {
		pkt->parts.src_net[0] = '\0';
	}

	/* set the packet db pointer */
	pkt->db = dbout;

	/* stuff the packet into a raw ORB output packet */
	if (stuffPkt (pkt, srcname, &pkttime, &packet, &nbytes, &packetsz) < 0) {
		register_error (0, "outrms: stuffPkt() error.\n");
		return (-1);
	}

	/* output the ORB packet */
	while (orbput (orbout, srcname, pkttime, packet, nbytes)) {
		char *s;
		s = strtime (time);
		complain (0, "outrms: orbput(%s,%s %s at %s) error.\n",
				srcname, sta, chan, s);
		free (s);
	}
	return (0);
}

/* This is called whenever the program is about to be 
   terminated and does a "master" flush of eveything */
int
myflush (Orbtm *Orbtm)

{
	Orbtm->flush = 1;
	if (pktchannelpipe_flush (Orbtm->pcp) < 0) {
		complain (0, "myflush: pktchannelpipe_flush() error.\n");
	}

	return (0);
}

/* This should be called right before bury() so that the
   global statetime can be computed. This is done by scanning
   through all of the channel-bands to find the minimum
   statetime. */
int
statetime (Orbtm *Orbtm)

{
	double statetime;
	/*
	while (sta_tag=(char *) poptbl(new_sta_tags)) {
	if (cntarr(GlobalAlarmStatus) > 0) {
		// hierhin mit dem Kram...
		if ((as = (AlarmStatus *) getarr (GlobalAlarmStatus, sta_tag))!= NULL) {
			if ((int)as->h_state != (int)as->alarm_state || 
					(double) as->h_value > (double) as->alarm_value) {
				if (as->alarm_state==OFF && as->h_state != OFF) {
					as->alarm_on_time=	(double) as->h_time;
					as->alarm_time=		(double) as->h_time;
					as->alarm_state=	(int) as->h_state;
					as->alarm_value=	(double) as->h_value;

					*/
	/* loop through the channels */
	statetime = 1.e30;
	/* set the global statetime */
	if (statetime < 1.e30) Orbtm->statetime = statetime;

	return (0);
}

/*
   statetime should be something like the first time a trigger is declared open
*/
/* parse the program parameter file */
int
parse_pf (char *pfname, Orbtm *Orbtm) {
	Pf *pf=NULL;
	MyChannelParams *sp_default;
	MyChannelParams *sp;
	Arr *templates_arr;
	Tbl *templates_tbl;
	Tbl *channels_tbl;
	int i;

	/* Open and read pf file */

	if (pfread (pfname, &pf)) {
		register_error (0, "parse_pf: pfread('%s') error.\n", pfname);
		return (-1);
	}

	/* initialize the templates array and allocate and parse the "default"
	   template */

	Orbtm->templates = newarr (0);
	if (Orbtm->templates == NULL) {
		register_error (0, "parse_pf: newarr(templates) error.\n");
		return (-1);
	}
	sp_default = mychannelparams_new (NULL, pf);
	if (sp_default == NULL) {
		register_error (0, "parse_pf: mychannelparams_new(default) error.\n");
		return (-1);
	}
	setarr (Orbtm->templates, "default", sp_default);

	/* set the main Orbtm program structure maxpkts from the default */
	Orbtm->maxpkts = sp_default->maxpkts;
	/* copy some stuff from the main Orbtm program structure */
	sp_default->pcc = Orbtm->pcc;
	sp_default->orbout = Orbtm->orbout;
	sp_default->dbout = Orbtm->dbout;
	if (sp_default->calib_from_db != 0 && sp_default->pcc == NULL) {
		register_error (0, "parse_pf: calib_from_db set in template %s, but no calib database.\n", "default");
		return (-1);
	}

	/* parse the templates array into a temporary local array */

	if (parse_param (pf, "templates", P_PFARR, 1, &templates_arr) < 0) {
		register_error (0, "parse_pf: parse_param(templates) error.\n");
		return (-1);
	}

	/* We work our way through the local templates array keys which are
	   the template names. The array values are parameter file objects
	   which contain the params corresponding to that template name. For
	   each template we need to 1) allocate a new channel_params_
	   structure, 2) initialized the new channel_params_ structure with 
	   the global default values, 3) parse the particular template
	   parameters to override the default values and 4) set the Orbtm
	   templates array value, for each template name as the key, to
	   the newly allocated/initialized/parsed channel_params_ structure. */
	
	templates_tbl = keysarr (templates_arr);
	for (i=0; i<maxtbl(templates_tbl); i++) {
		char *name;
		Pf *pft;

		/* get the template name */
		name = (char *) gettbl (templates_tbl, i);

		/* get the corresponding template pf pointer */
		pft = (Pf *) getarr (templates_arr, name);

		/* allocate, initialize and parse the MyChannelParams structure */
		sp = mychannelparams_new (sp_default, pft);
		if (sp == NULL) {
			freetbl (templates_tbl, 0);
			freearr (templates_arr, 0);
			register_error (0, "parse_pf: mychannelparams_new(%s) error.\n", name);
			return (-1);
		}

		/* set the Orbtm->templates array */
		setarr (Orbtm->templates, name, sp);
	}
	/* free the local templates array and table */
	freetbl (templates_tbl, 0);
	freearr (templates_arr, 0);

	/* parse the channels table into a temporary local table */

	if (parse_param (pf, "channels", P_TBL, 1, &channels_tbl) < 0) {
		register_error (0, "parse_pf: parse_param(channels) error.\n");
		return (-1);
	}

	/* We will reprocess the channels table to separate out the template
	   names from the channel expressions and making these into two
	   separate tables. We start by allocating the channels and 
	   channels_template tables in the Orbtm structure. We then work
	   our way through the channels_tbl local table, read and parse
	   each entry into channel expressions and template names and
	   fill in the Orbtm channels and channels_template tables. */

	Orbtm->channels = newtbl (1);
	if (Orbtm->channels == NULL) {
		register_error (0, "parse_pf: newtbl(channels) error.\n");
		return (-1);
	}
	Orbtm->channels_for_pcp = newtbl (1);
	if (Orbtm->channels_for_pcp == NULL) {
		register_error (0, "parse_pf: newtbl(channels_for_pcp) error.\n");
		return (-1);
	}
	Orbtm->channels_template = newtbl (1);
	if (Orbtm->channels_template == NULL) {
		register_error (0, "parse_pf: newtbl(channels_template) error.\n");
		return (-1);
	}

	for (i=0; i<maxtbl(channels_tbl); i++) {
		char *line, expr[1024], templ[1024];
		char *expr_dup, *templ_dup;
		int ret;

		/* get the entry from the table */
		line = (char *) gettbl (channels_tbl, i);

		/* parse the entry into an expression and a template name */
		ret = sscanf (line, "%s %s", expr, templ);
		if (ret < 0) {
			register_error (1, "parse_pf: sscanf(%s) error for line number %d in channels table.\n",
						line, i);
			return (-1);
		}
		if (ret == 0) continue;
		if (ret == 1) {
			register_error (0, "parse_pf: need template name in %s for line number %d in channels table.\n",
						line, i);
			return (-1);
		}

		/* fill in the channels and channels_template tables in Orbtm */
		expr_dup = strdup(expr);	/* cannot use expr and templ
						   since they are local automatic */
		if (expr_dup == NULL) {
			register_error (1, "parse_pf: strdup(expr) error at channels table line %d.\n", i);
			return (-1);
		}
		templ_dup = strdup(templ);
		if (templ_dup == NULL) {
			register_error (1, "parse_pf: strdup(templ) error at channels table line %d.\n", i);
			return (-1);
		}
		settbl (Orbtm->channels, -1, expr_dup);
		settbl (Orbtm->channels_template, -1, templ_dup);

		/* Find the template and compose the channels_for_pcp table.
		   This monkey business is necessary so that the input
		   channels table for pktchannel_new(3) contains also the maxpkts
		   field which can be different for each of the table entries. */
		sp = (MyChannelParams *) getarr (Orbtm->templates, templ_dup);
		if (sp == NULL) {
			register_error (0, "parse_pf: Template %s not defined but used in channels table entry #%d.\n",
								templ_dup, i);
			return (-1);
		}
		sprintf (expr, "%s %d", expr_dup, sp->maxpkts);
		expr_dup = strdup(expr);
		if (expr_dup == NULL) {
			register_error (1, "parse_pf: strdup(expr) error at channels table line %d.\n", i);
			return (-1);
		}
		settbl (Orbtm->channels_for_pcp, -1, expr_dup);
	}
	/* free the local channels table */
	freetbl (channels_tbl, 0);

	/* parse the channels_reject table */

	Orbtm->channels_reject = NULL;
	if (parse_param (pf, "channels_reject", P_TBL, 0, &(Orbtm->channels_reject)) < 0) {
		register_error (0, "parse_pf: parse_param(channels_reject) error.\n");
		return (-1);
	}

	return (0);
}

/* create a new MyChannelParams structure, copy values from a default and
   read in new values from a parameter file object */
MyChannelParams *
mychannelparams_new (MyChannelParams *def, Pf *pf) 

{
	MyChannelParams *cp;
	int required=1;
	int i;
	Pf *pfbands;

	/* allocate and initialize the new structure */

	cp = (MyChannelParams *) malloc (sizeof(MyChannelParams));
	if (cp == NULL) {
		register_error (1, "mychannelparams_new: malloc(channel params) error.\n");
		return (NULL);
	}
	memset (cp, 0, sizeof(MyChannelParams)); /* always a good idea to initialize
							   	   newly allocated memory */

	/* copy from the default */

	if (def) {
		required = 0;
		*cp = *def;
		if (def->bands) {
			cp->bands = newtbl(maxtbl(def->bands));
			if (cp->bands == NULL) {
				register_error (0, "mychannelparams_new: newtbl(%d) error.\n",
									maxtbl(def->bands));
				free (cp);
				return (NULL);
			}
			for (i=0; i<maxtbl(def->bands); i++) {
				MyFilterParams *band;
				MyFilterParams *band_copy;

				band = (MyFilterParams *) gettbl (def->bands, i);
				band_copy = myfilterparams_new (band, NULL);
				if (band_copy == NULL) {
					register_error (0, "mychannelparams_new: myfilterparams_new(def) error.\n");
					freetbl (cp->bands, myfilterparams_free);
					free (cp);
					return (NULL);
				}
				settbl (cp->bands, i, band_copy);
			}
		}
	}

	if (pf == NULL) return (cp);

	/* parse new parameters using the parse_param(3) subroutines */

	/*get rid of this, should always be 0 i.e all packets should be processed immediately*/
	if (parse_param (pf, "maxpkts", P_LINT, required, &(cp->maxpkts)) < 0) {
		register_error (0, "mychannelparams_new: parse_param(maxpkts) error.\n");
		return (NULL);
	}
	cp->maxpkts=0;
	if (parse_param (pf, "calib_from_db", P_BOOL, required, &(cp->calib_from_db)) < 0) {
		register_error (0, "mychannelparams_new: parse_param(calib_from_db) error.\n");
		return (NULL);
	}

	/* The "bands" table parsing is a little obtuse. First we need to
	   parse a pf-version of the table using parse_param(3). We will
	   then work through this pf-ized "bands" table using the lower level
	   pfget(3) routine. The reason for this approach is that the entries
	   in the "bands" table are associative arrays with no key values.
	   If we were to parse "bands" as a normal P_TBL, we would just get back
	   a list of strings instead of new parameter file objects, which is
	   what we will need for further parsing of each band. */

	pfbands = NULL;
	if (parse_param (pf, "bands", P_TBLPF, required, &pfbands) < 0) {
		register_error (0, "mychannelparams_new: parse_param(bands) error.\n");
		return (NULL);
	}
	if (pfbands) {
		if (cp->bands) {
			freetbl (cp->bands, myfilterparams_free);
			cp->bands = NULL;
		}
		cp->bands = newtbl(1);
		if (cp->bands == NULL) {
			register_error (0, "mychannelparams_new: newtbl() error.\n");
			free (cp);
			return (NULL);
		}
		for (i=0; i<pfmaxtbl(pfbands); i++) {
			int ret;
			Pf *pfb;
			MyFilterParams *band;

			/* This is where we parse each "bands" associative array.
			   Note that the returned pfb is another pf object.  */
			ret = pfget (pfbands, (char *) i, (void **) &pfb);
			if (ret != PFARR) continue; /* skip if not an associative array */

			/* and then we parse out the individual band */
			band = myfilterparams_new (NULL, pfb);
			if (band == NULL) {
				register_error (0, "mychannelparams_new: myfilterparams_new(band) error.\n");
				freetbl (cp->bands, myfilterparams_free);
				free (cp);
				return (NULL);
			}
			settbl (cp->bands, -1, band);
		}
	}

	return (cp);
}

/* create a new MyFilterParams structure, copy values from a default and
   read in new values from a parameter file object */
MyFilterParams *
myfilterparams_new (MyFilterParams *def, Pf *pf) 

{
	MyFilterParams *fp;
	int required=1;
	char *ptr;

	/* allocate and initialize the new structure */

	fp = (MyFilterParams *) malloc (sizeof(MyFilterParams));
	if (fp == NULL) {
		register_error (1, "myfilterparams_new: malloc(channel params) error.\n");
		return (NULL);
	}
	memset (fp, 0, sizeof(MyFilterParams)); /* always a good idea to initialize
							   	   newly allocated memory */

	/* copy from the default */

	if (def) {
		required = 0;
		*fp = *def;
		if (def->filter_string) {
			fp->filter_string = strdup(def->filter_string);
			if (fp->filter_string == NULL) {
				register_error (1, "myfilterparams_new: strdup(filter_string) error.\n");
				free (fp);
				return (NULL);
			}
		}
		if (def->tag) {
			fp->tag = strdup(def->tag);
			if (fp->tag == NULL) {
				register_error (1, "myfilterparams_new: strdup(tag) error.\n");
				free (fp);
				return (NULL);
			}
		}
		if (def->units) {
			fp->units = strdup(def->units);
			if (fp->tag == NULL) {
				register_error (1, "myfilterparams_new: strdup(units) error.\n");
				free (fp);
				return (NULL);
			}
		}
		fp->time = 0.0;
		fp->dt = 0.0;
		fp->nsamps = 0;
		fp->sizeof_data = 0;
		fp->data = NULL;
		fp->filter_hook = NULL;
		fp->alarm_time= (double) TIME_NULL;
		fp->alarm_on_time= (double) TIME_NULL;
		fp->alarm_state= (int) OFF;
		fp->output_individual_channels=0;
	}

	if (pf == NULL) return (fp);

	/* parse new parameters using the parse_param(3) subroutines */

	ptr = NULL;
	if (parse_param (pf, "filter", P_STR, required, &ptr) < 0) {
		register_error (0, "myfilterparams_new: parse_param(filter) error.\n");
		return (NULL);
	}
	if (ptr) {
		if (fp->filter_string) free (fp->filter_string);
		fp->filter_string = strdup(ptr);
	}
	if (parse_param (pf, "fpad", P_DBL, required, &(fp->fpad)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(fpad) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "fc", P_DBL, required, &(fp->fc)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(fc) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "low", P_DBL, required, &(fp->low)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(low) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "medium", P_DBL, required, &(fp->medium)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(medium) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "high", P_DBL, required, &(fp->high)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(high) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "output_individual_channels", P_BOOL, required, &(fp->output_individual_channels)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(output_individual_channelse) error.\n");
		return (NULL);
	} 
	if (parse_param (pf, "minimum_ontime", P_LINT, required, &(fp->minimum_ontime)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(minimum_ontime) error.\n");
		return (NULL);
	} 
	if (parse_param (pf, "tag", P_STR, required, &ptr) < 0) {
		register_error (0, "myfilterparams_new: parse_param(tag) error.\n");
		return (NULL);
	}
	if (ptr) {
		if (fp->tag) free (fp->tag);
		fp->tag =ptr;
	}
	if (parse_param (pf, "units", P_STR, required, &ptr) < 0) {
		register_error (0, "myfilterparams_new: parse_param(units) error.\n");
		return (NULL);
	}
	if (ptr) {
		if (fp->units) free (fp->units);
		fp->units = strdup(ptr);
	}
	return (fp);
}

void
myfilterparams_free (void *ptr)

{
	MyFilterParams *fp=ptr;

	if (fp == NULL) return;

	if (fp->filter_string) free (fp->filter_string);
	if (fp->data) free (fp->data);
	if (fp->filter_hook) free_hook (&(fp->filter_hook));
	free (fp);
}

/* startup banner printout */
void
startup_banner()

{
	int i, n;
	char *pre1 = "******";
	char *pre2 = "*     ";
	char *post1 = "******";
	char *post2 = "     *";
	char line1[256], line2[256];

	n = strlen(Orbtm_Version);
	strcpy (line1, "*");
	for (i=1; i<n; i++) {
		strcat (line1, "*");
	}

	elog_notify (0, "\n");
	elog_notify (0, "%s%s%s\n", pre1, line1, post1);
	strcpy (line2, " ");
	for (i=1; i<n; i++) {
		strcat (line2, " ");
	}
	memcpy (line2, "orbtm startup", strlen("orbtm startup"));
	elog_notify (0, "%s%s%s\n", pre2, line2, post2);
	elog_notify (0, "%s%s%s\n", pre2, Orbtm_Version, post2);
	elog_notify (0, "%s%s%s\n", pre1, line1, post1);

}
