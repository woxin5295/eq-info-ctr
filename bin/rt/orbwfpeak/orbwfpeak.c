/* Copyright (c) 2004 Boulder Real Time Technologies, Inc. */
/* All rights reserved */

/* This software may be used freely in any way as long as
   the copyright statement above is not removed. */

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
#include "tr.h"

static int verbose = 0;
static int debug = 0;

/* Following are structure definitions */

/* This is a structure for containing processing parameters
   for a particular frequency-band */
typedef struct filter_params_ {
	char *filter_string;		/* Filter specification */
	double fpad;			/* Filter time pad for rmoving transients */
	double fc;			/* center frequency */
	double twin;			/* averaging window */
	double dtpeak;			/* increment for moving averaging window */
	double tmin;			/* minimum contiguous data for averaging */
	double tpeak;			/* start time of current peak window */
	double lastpeak;			/* start time of last output peak window */
	double statetime;		/* time for last state (bury()) */
	double time;			/* time of first sample in current contiguous buffer */
	double nexttime;		/* time of next sample in current contiguous buffer */
	double dt;			/* sampling interval in current contiguous buffer */
	int nsamps;			/* number of samples in current contiguous buffer */
	int sizeof_data;		/* size in bytes of current contiguous data buffer */
	float *data;			/* current contiguous data buffer */
	Hook *filter_hook;		/* hook for trfilter_pkt(3) */
	double calib;			/* calib value */
	char segtype[4];		/* segtype code */
} MyFilterParams;

/* This is a structure for containing processing parameters
   for a particular station-channel */
typedef struct channel_params_ {
	int calib_from_db;		/* Get calib values from db? */
	int maxpkts;			/* Maximum size of time-ordered packet queue */
	int orbout;			/* output ORB for wfmeas db packets */
	Dbptr dbout;			/* used to house the scratch record for the output ORB packets */
	PktChannelCalib *pcc;		/* pktchannelcalib(3) pointer for getting calib, etc. from database */
	Tbl *bands;			/* list of MyFilterParams structures for processing */
} MyChannelParams;

/* This is a structure for containing program parameters */
typedef struct orbwfpeak_ {
	PktChannelPipe *pcp;		/* pktchannelpipe(3) pointer for processing ORB packets */
	PktChannelCalib *pcc;		/* pktchannelcalib(3) pointer for getting calib, etc. from database */
	int maxpkts;			/* Default maximum size of time-ordered packet queue */
	int orbout;			/* output ORB for wfmeas db packets */
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
} Orbwfpeak;

static Orbwfpeak orbwfpeak_global;

/* Following are local prucedure declerations */

int mycallback (void *private, PktChannel *pktchan, int queue_code, double gaptime);
int myprocessbuffer (PktChannel *pktchan, MyChannelParams *cp, MyFilterParams *fp, 
			char *sta, char *chan, double calib, char *segtype);
int myflushbuffer (MyChannelParams *cp, MyFilterParams *fp, char *sta, char *chan, int force);
int myflush (Orbwfpeak *orbwfpeak);
double computepeak (double time, int nsamps, double dt, float *data,
				double tstart, double tend, double *tmeas);
int outpeak (MyChannelParams *cp, char *sta, char *chan, double time,
		double twin, double fc, char *filter, char *segtype, double peak, double tmeas);
int statetime (Orbwfpeak *orbwfpeak);
void mybury();
int parse_pf (char *pfname, Orbwfpeak *orbwfpeak);
MyChannelParams *mychannelparams_new (MyChannelParams *def, Pf *pf);
void myfilterparams_free (void *ptr);
MyFilterParams *myfilterparams_new (MyFilterParams *def, Pf *pf);
void usage();
void startup_banner();

/* this is the main program */

int
main (int argc, char **argv)

{
	char *start = NULL;
	char *select_expr = NULL;
	char *reject_expr = NULL;
	char *pfname = "orbwfpeak";
	char *statefile = NULL;
	char *dbname = NULL;
	char *orbinname = NULL;
	char *orboutname = NULL;

	Orbwfpeak *orbwfpeak;
	int orbin;
	int pktid, get;
	double time, endtime=0.0;
	double lastburytime;
	int loop=1;
	
	/* initialize error and log reporting */
	elog_init (argc, argv);

	/* if not enough args, print usage line and banner */
	if (argc < 3) {
		usage();
		banner ("orbwfpeak", "$Revision: 1.1 $ $Date: 2005-10-18 23:45:29 $\n") ;
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
				usage();
				exit (1);
			}
			start = *argv;
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
		} else if (!strcmp(*argv, "-pf")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -pf argument.\n");
				usage();
				exit (1);
			}
			pfname = *argv;
		} else if (!strcmp(*argv, "-S")) {
			argv++;
			argc--;
			if (argc < 1) {
				complain (0, "Need -S argument.\n");
				usage();
				exit (1);
			}
			statefile = *argv;
		} else if (!strcmp(*argv, "-v")) {
			verbose = 1;
		} else if (!strcmp(*argv, "-vv")) {
			verbose = 2;
		} else {
			complain (0, "Unrecognized option '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}

	/* parse command line for required arguments */
	if (argc < 1) {
		complain (0, "Need dbname argument.\n");
		usage();
		exit (1);
	}
	dbname = *argv;
	argv++;
	argc--;

	if (argc < 1) {
		complain (0, "Need orbin argument.\n");
		usage();
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
			usage();
			exit (1);
		}
	}

	/* setup the pktchannelcalib(3) pointer for getting CSS sta and chan
	   and also calib info from a database */
	orbwfpeak = &orbwfpeak_global;
	memset (orbwfpeak, 0, sizeof(Orbwfpeak));
	orbwfpeak->pcc = pktchannelcalib_new (dbname, 1, 0, 0);
	if (orbwfpeak->pcc == NULL) {
		die (0, "pktchannelcalib_new(%s) error.\n", dbname);
	}
	orbwfpeak->channels_arr = newarr (0);
	if (orbwfpeak->channels_arr == NULL) {
		die (0, "newarr(orbwfpeak->channels_arr) error.\n");
	}

	/* Setup the output database scratch record and the output ORB handle.
	   Do it here so that these parameters can be passed down to the 
	   MyChannelParams structures in parse_pf() */
	orbwfpeak->orbout = -1;
	if (orboutname) {
		/* open the database */
		orbwfpeak->dbout = dbtmp ("css3.0" );
		/* make sure the wfpeak2 table exists in this schema */
		orbwfpeak->dbout = dblookup (orbwfpeak->dbout, 0, "wfmeas", 0, "dbSCRATCH");
		if (orbwfpeak->dbout.table == dbINVALID) {
			die (0, "Table 'wfmeas' does not appear to exist in %s database.\n",
							dbname);
		}
		/* open the output ORB */
		orbwfpeak->orbout = orbopen (orboutname, "w&");
		if (orbwfpeak->orbout < 0) {
			die (0, "orbopen(%s) error.\n", orboutname);
		}
	}

	/* parse the program parameter file */
	if (parse_pf (pfname, orbwfpeak) < 0) {
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
		Relic relic;

		/* Basically, exhume(3) sets up signal handlers for intercepting
		   stop/hangup signals, like SIGINT from CTRL-C on standard
		   input. It also looks for, reads, but doesn't parse (yet)
		   a state file. When the exhume singal handler sees one of
		   the shutdown signals, it sets orbwfpeak->quit immediately.
		   The application program should be continuously monitoring
		   orbwfpeak->quit and immediately initiate a gracefull shutdown
		   as soon as it is set. Generally, a graceful shutdown means
		   1) flush out all output buffers, 2) compute the global state
		   time with statetime() and 3) bury(3) the state time in the
		   statefile. In case the application is too busy to see that 
		   orbwfpeak->quit has been set, eg. it is blocked on something 
		   like orbreap(), then after 15 seconds the exhume signal handler 
		   will call bury() automatically. This gives the application 
		   program one last shot at a gracefull shutdown. mybury() is
		   an application program callback that is called by bury(3) 
		   before it writes to the statefile. Here it is just a wrapper
		   for statetime(), which computes the global state time. */
		if ( exhume ( statefile, &(orbwfpeak->quit), 15, mybury ) != 0 ) {
			elog_notify ( 0, "read old state file\n" ) ;  
	    	}
		/* resurrect(3) is where we parse the state file. The "relic"
		   stuff is necessary for the resurrect(3) interface (i.e. read
		   the man page and look at bury.h). */
		relic.dp = &(orbwfpeak->statetime);
	    	if ( resurrect ( "last_pkttime", relic, TIME_RELIC ) == 0 )  {
			char *s;

			/* position the input ORB read pointer */
			if (orbwfpeak->statetime > 0.0) {
				orbafter (orbin, orbwfpeak->statetime);
				elog_notify ( 0, "resurrection successful: repositioned to time %s\n", 
								s=strtime(orbwfpeak->statetime) ) ;
				free(s) ;
			} else {
				complain ( 0, "resurrection unsuccessful: time not set\n" ) ;  
			}
	    	} else {
			complain ( 0, "resurrection unsuccessful: no/incorrect state file\n" ) ;  
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

	/* create the PktChannelPipe structure by calling
	   pktchannelpipe_new (see pktchannelpipe(3)) */
	orbwfpeak->pcp = pktchannelpipe_new (orbwfpeak->channels_for_pcp, orbwfpeak->channels_reject,
			orbwfpeak->maxpkts, mycallback, orbwfpeak);
	if (orbwfpeak->pcp == NULL) {
		die (0, "pktchannelpipe_new() error.\n");
	}

	/* input orb packet read loop */
	while (loop) {
		double pkttime;
		char srcname[ORBSRCNAME_SIZE];
		static char *packet=NULL;
		static int bufsize=0;
		int nbytes;
		int ret;

		/* read the orb packet - the little dance with get and orbget
		   is necessary in order to properly implement the "-start OLDEST"
		   startup so that the actual oldest packet is read instead
		   of the next packet after the oldest packet */
		if (get) {
			orbget (orbin, pktid, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize);
			get = 0;
			orbseek (orbin, ORBOLDEST);
		} else orbreap (orbin, &pktid, srcname, &pkttime, &packet, &nbytes, &bufsize);
		if (debug > 2) {
			char *s;
			s = strtime(pkttime);
			elog_debug (0, "read %s packet at %s.\n", srcname, s);
			free (s);
		}

		/* now process the packet with pktchannelpipe_push 
		   (see pktchannelpipe(3)) */
		ret = pktchannelpipe_push (orbwfpeak->pcp, srcname, pkttime, packet, nbytes);
		if (ret < 0) {
			die (0, "pktchannelpipe_push fatal error for %s at %s.\n",
								srcname, strtime(pkttime));
		}

		/* clear out any lingering error messages */
		clear_register (1);

		/* check for a shutdown signal */
		if (orbwfpeak->quit) {
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

		/* every so often (like every 10 monutes) bury the current state
		   time - we do this incase the program gets egregiously killed,
		   like with a kill -9 */
		if (statefile) {
			double nowtime;

			nowtime = now();
			if (nowtime-lastburytime > 600.0) {
				lastburytime = nowtime;
				bury();
			}
		}
	}

	/* flush out everything */
	myflush (orbwfpeak);

	/* update state file */
	if (statefile) bury();

	/* normal exit */
	exit (0);
}

/* This is the main processing callback procedure that does most of
   the real work. This is where most of the customization would be done
   in a new ORB waveform processing program */
int
mycallback (void *private, PktChannel *pktchan, int queue_code, double gaptime)

{
	Orbwfpeak *orbwfpeak = private;	
	char netstachanloc[64], *s;
	MyChannelParams *cp;
	double calib, calper;
	double dbcalib, dbcalper;
	char sta[16], chan[16], segtype[4];
	char dbsegtype[4];
	static double lastchecktime=0.0;
	static int check=0;
	double timenow;
	int i, ret;

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
	s = strtime (pktchan->time);

	/* first thing - if this is a duplicate packet, toss it and return */
	if (queue_code == PKTCHANNELPIPE_DUP) { 
		if (verbose > 0) elog_notify (0, "%s: %s: duplicate packet\n", s, netstachanloc); 
		freePktChannel (pktchan);
		free (s);
		return (0);
	}

	/* get the channel params that go with this pktchan */
	cp = (MyChannelParams *) getarr (orbwfpeak->channels_arr, netstachanloc);

	/* The next bit of code gets executed once whenever a new station-channel
	   appears. This part figures out which template to use, makes a copy of
	   the MyChannelParams structure and sets the channels_arr. */
	if (cp == NULL) {
		Tbl *tmp_tbl;
		int i;
		char *name;
		MyChannelParams *cpc;

		/* find the entry in the channels table */
		tmp_tbl = tblgrep (orbwfpeak->channels, netstachanloc);
		if (maxtbl(tmp_tbl) < 1) {
			die (0, "mycallback: big problem with orbwfpeak->channels table.\n");
		}
		i = (int) gettbl (tmp_tbl, 0);
		freetbl (tmp_tbl, 0);

		/* now find the corresponding template name in the channels_template
		   table */
		name = (char *) gettbl (orbwfpeak->channels_template, i);

		/* find the MyChannelParams pointer from the templates array */
		cp = (MyChannelParams *) getarr (orbwfpeak->templates, name);
		if (cp == NULL) {
			die (0, "mycallback: big problem with orbwfpeak->templates(%s) table.\n", name);
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
		setarr (orbwfpeak->channels_arr, netstachanloc, cpc);
		cp = cpc;

		elog_notify (0, "%s: %s: Adding channel with maxpkts = %d for processing.\n", 
							s, netstachanloc, cp->maxpkts);
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

		fp = (MyFilterParams *) gettbl (cp->bands, i);

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
		myprocessbuffer (pktchan, cp, fp, sta, chan, calib, segtype);

		/* finally - if orbwfpeak->flush is set, then do a forced flush */
		if (orbwfpeak->flush) myflushbuffer (cp, fp, sta, chan, 1);
	}

	/* free up the PktChannel */
	freePktChannel (pktchan);
	free (s);

	return (0);
}

/* These are the routines that compute and output the peak values */

/* This routine accepts the PktChannel from the ORB, copies the data into
   an internal floating point buffer, processes the data (apply calib
   and filter), and then calls myflushbuffer to flush out the peak time
   windows. */
int
myprocessbuffer (PktChannel *pktchan, MyChannelParams *cp, MyFilterParams *fp, 
			char *sta, char *chan, double calib, char *segtype)

{
	int size;
	int ret;
	int i;
	char *s;

	/* skip out if no input samples or bad samprate */
	if (pktchan->nsamp < 1) return (0);
	if (pktchan->samprate <= 0.0) return (0);
	if (calib == 0.0) calib = 1.0;

	s = strtime (pktchan->time);

	/* allocate new data space if necessary */
	size = (fp->nsamps+pktchan->nsamp)*sizeof(float);
	if (size > fp->sizeof_data) {
		if (size < 10000) size = 10000;
		else size *= 2;
		if (fp->data == NULL) {
			fp->data = (float *) malloc (size);
			if (fp->data == NULL) {
				die (1, "myprocessbuffer: malloc(%s,%s,%d) error.\n",
							sta, chan, size);
			}
		} else {
			fp->data = (float *) realloc (fp->data, size);
			if (fp->data == NULL) {
				die (1, "myprocessbuffer: realloc(%s,%s,%d) error.\n",
							sta, chan, size);
			}
		}
		if (debug) elog_debug (0, "allocating %d samples for %s %s:%s.\n",
							size/4, sta, chan, fp->filter_string);
		fp->sizeof_data = size;
	}

	/* set up the time, calib and segtype */
	if (fp->nsamps == 0) {
		fp->time = pktchan->time;
		fp->nexttime = pktchan->time;
		fp->calib = calib;
		strcpy (fp->segtype, segtype);
		fp->tpeak = fp->time + fp->fpad;
		fp->lastpeak = 0.0;
		fp->statetime = fp->time - 100.0;
	}

	/* copy PktChannel samples into buffer applying calib */
	for (i=0; i<pktchan->nsamp; i++) fp->data[fp->nsamps+i] = fp->calib * pktchan->data[i];

	/* filter the data */
	fp->dt = 1.0/pktchan->samprate;
	fp->nexttime += pktchan->nsamp * fp->dt;
	ret = trfilter_pkt (pktchan->nsamp, fp->dt, &(fp->data[fp->nsamps]), 
							fp->filter_string, &(fp->filter_hook));
	if (ret == -1) {
		die (0, "myprocessbuffer: trfilter_pkt(%s,%s) fatal error.\n", sta, chan);
	}
	clear_register (1);
	fp->nsamps += pktchan->nsamp;

	/* flush out any completed peak segments */
	myflushbuffer (cp, fp, sta, chan, 0);

	free (s);
	return (0);
}

/* This routine is called whenever it is time to either 1) look
   for and dispatch time segments for peak averaging or 2) force
   a flushing of the internal floating point data buffer (by
   setting the force flag). Forced flushing should happen
   whenever there is some kind of discontinuity in the data
   timing, or certain data parameters, the sample rate and
   calib. */
int
myflushbuffer (MyChannelParams *cp, MyFilterParams *fp, char *sta, char *chan, int force)

{
	char *s;
	double endtime;

	/* free up the filter state info if this is a forced
	   flush (meaning that there is a time gap or other
	   discontinuity requiring a complete flush and
	   re-initialization) */
	if (force && fp->filter_hook) {
		if (verbose > 0) {
			s = strtime(fp->nexttime);
			elog_notify (0, "%s: %s %s:%s: re-initializing filter state\n",
						s, sta, chan, fp->filter_string);
			free (s);
		}
		free_hook (&(fp->filter_hook));
		fp->filter_hook = NULL;
	}

	/* output any peak windows that are ready */
	endtime = fp->nexttime - fp->dt;
	while (fp->tpeak+fp->twin <= fp->nexttime) {
		double tstart, peak, tmeas;

		tstart = fp->tpeak;
		if (tstart < fp->time) tstart = fp->time;
		if (fp->tpeak+fp->twin-tstart < fp->tmin) {
			if (verbose > 0) {
				/*s = strtime(fp->tpeak+fp->twin);*/
				s = strtime(fp->tpeak);
				elog_notify (0, "%s: %s %s:%s: skipping\n",
						s, sta, chan, fp->filter_string);
				free (s);
			}
			fp->lastpeak = fp->tpeak;
			fp->tpeak += fp->dtpeak;
			fp->statetime = fp->tpeak - fp->fpad - 100.0;
			continue;
		}
		peak = computepeak (fp->time, fp->nsamps, fp->dt, fp->data, 
						tstart, fp->tpeak+fp->twin, &tmeas);
		if (verbose > 1) {
			/*s = strtime(fp->tpeak+fp->twin);*/
			s = strtime(fp->tpeak);
			elog_notify (0, "%s: %s %s:%s: %.3f peak \n",
					s, sta, chan, fp->filter_string, peak);
			free (s);
		}
		/*if (outpeak (cp, sta, chan, fp->tpeak+fp->twin, fp->twin, fp->fc,
					fp->filter_string, fp->segtype, peak, tmeas) < 0) {*/
		if (outpeak (cp, sta, chan, fp->tpeak, fp->twin, fp->fc,
					fp->filter_string, fp->segtype, peak, tmeas) < 0) {
			/*s = strtime(fp->tpeak+fp->twin);*/
			s = strtime(fp->tpeak);
			register_error (0, "myflushbuffer: %s: %s %s:%s: outpeak() error\n",
					s, sta, chan, fp->filter_string);
			free (s);
		}
		fp->lastpeak = fp->tpeak;
		fp->tpeak += fp->dtpeak;
		fp->statetime = fp->tpeak - fp->fpad - 100.0;
	}

	/* force out the remainder of the buffer and re-initialize */
	if (force) {
		double tstart, peak, tmeas;

		tstart = fp->tpeak;
		if (tstart < fp->time) tstart = fp->time;
		if (fp->nexttime-tstart < fp->tmin) {
			if (verbose > 0) {
				/*s = strtime(fp->tpeak+fp->twin);*/
				s = strtime(fp->tpeak);
				elog_notify (0, "%s: %s %s:%s: skipping\n",
						s, sta, chan, fp->filter_string);
				free (s);
			}
		} else {
			peak = computepeak (fp->time, fp->nsamps, fp->dt, fp->data, 
						tstart, fp->nexttime, &tmeas);
			if (verbose > 1) {
				/*s = strtime(fp->tpeak+fp->twin);*/
				s = strtime(fp->tpeak);
				elog_notify (0, "%s: %s %s:%s: %.3f peak\n",
						s, sta, chan, fp->filter_string, peak);
				free (s);
			}
			/*if (outpeak (cp, sta, chan, fp->tpeak+fp->twin, fp->twin, fp->fc,
						fp->filter_string, fp->segtype, peak, tmeas) < 0) {*/
			if (outpeak (cp, sta, chan, fp->tpeak, fp->twin, fp->fc,
						fp->filter_string, fp->segtype, peak, tmeas) < 0) {
				/*s = strtime(fp->tpeak+fp->twin);*/
				s = strtime(fp->tpeak);
				register_error (0, "myflushbuffer: %s: %s %s:%s: outpeak() error\n",
						s, sta, chan, fp->filter_string);
				free (s);
			}
		}

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
	if (fp->nsamps > 0 && fp->time < fp->lastpeak-1000*fp->dt) {
		int i;

		i = TIME2SAMP(fp->time, 1.0/fp->dt, fp->lastpeak-100*fp->dt);
		if (i > 0 && i < fp->nsamps-1) {
			int size;

			size = (fp->nsamps-i)*sizeof(float);
			memmove (fp->data, &(fp->data[i]), size);
			fp->time += fp->dt*i;
			fp->nsamps -= i;
			if (debug > 1) elog_debug (0, "removing %d samples from %s %s:%s.\n",
							i, sta, chan, fp->filter_string);
		}
	} 

	return (0);
}

/* Compute a peak value over some time window */
double
computepeak (double time, int nsamps, double dt, float *data,
				double tstart, double tend, double *tmeas)

{
	double peak;
	int i, i0, i1;
	int n;
	int npeak;

	i0 = TIME2SAMP (time, 1.0/dt, tstart);
	i1 = TIME2SAMP (time, 1.0/dt, tend);
	if (i0 < 0) i0 = 0;
	if (i1 > nsamps-1) i1 = nsamps-1;

	for (i=i0,n=0,peak=0.0; i<=i1; i++,n++) {
		if (fabs(data[i]) > fabs(peak)) {
			peak = data[i];
			npeak = n;
		}
	}

	*tmeas = tstart + npeak*dt;
	if (verbose > 1) {
		printf("computepeak:time %s\n tstart %s tend %s tmeas %s\n", 
			strtime(time), strtime(tstart), strtime(tend), strtime(*tmeas));
	}

	return (peak);
}

/* output a new wfmeas database row as a packet to the output ORB */
int
outpeak (MyChannelParams *cp, char *sta, char *chan, double time,
		double twin, double fc, char *filter, char *segtype, double peak, double tmeas)

{
	static Packet *pkt=NULL;
	static char *packet=NULL;
	static int packetsz=0;
	int nbytes;
	double pkttime;
	char srcname[ORBSRCNAME_SIZE];
	char *units1;
	char *units1desc;

	double endtime;

	if (cp->orbout < 0) return (0);

	/* We should come into this subroutine with the database
	   pointer, cp->dbout, already set to point to table "wfmeas" */

	/* null out the scratch record */
	cp->dbout.record = dbNULL;
	cp->dbout.field = dbALL;
	dbget (cp->dbout, 0);
	cp->dbout.record = dbSCRATCH;

	trlookup_segtype( segtype, &units1, &units1desc );

	endtime = time + twin;

	/* fill in the scratch record */
	if (dbputv (cp->dbout, 0, 	"sta", sta,
					"chan", chan,
					"time", time,
					"endtime", endtime,
					"tmeas", tmeas,
					"filter", filter,
					"meastype", "peak",
		                        "val1", peak,
					"units1", units1,
					"auth", "orbwfpeak",
					NULL) < 0) {
		register_error (0, "outpeak: dbputv() error.\n");
		return (-1);
	}

	/* initialize the Packet structure used for output */
	if (pkt == NULL) {
		/* allocate */
		pkt = newPkt();
		if (pkt == NULL) {
			die (0, "outpeak: newPkt() error.\n");
		}
		/* set the packet type to "db" */
		pkt->pkttype = suffix2pkttype ("db");
	}

	/* set the packet db pointer */
	pkt->db = cp->dbout;

	/* stuff the packet into a raw ORB output packet */
	if (stuffPkt (pkt, srcname, &pkttime, &packet, &nbytes, &packetsz) < 0) {
		register_error (0, "outpeak: stuffPkt() error.\n");
		return (-1);
	}

	/* output the ORB packet */
	while (orbput (cp->orbout, srcname, pkttime, packet, nbytes)) {
		char *s;
		s = strtime (time);
		complain (0, "outpeak: orbput(%s,%s %s at %s) error.\n",
				srcname, sta, chan, s);
		free (s);
	}

	return (0);
}

/* This is called whenever the program is about to be 
   terminated and does a "master" flush of eveything */
int
myflush (Orbwfpeak *orbwfpeak)

{
	orbwfpeak->flush = 1;
	if (pktchannelpipe_flush (orbwfpeak->pcp) < 0) {
		complain (0, "myflush: pktchannelpipe_flush() error.\n");
	}

	return (0);
}

/* This should be called right before bury() so that the
   global statetime can be computed. This is done by scanning
   through all of the channel-bands to find the minimum
   statetime. */
int
statetime (Orbwfpeak *orbwfpeak)

{
	Tbl *keys;
	int i, j;
	double statetime;

	/* get the keys (netstachanlocs) from channels_arr */
	keys = keysarr (orbwfpeak->channels_arr);

	/* loop through the channels */
	statetime = 1.e30;
	for (i=0; i<maxtbl(keys); i++) {
		char *netstachanloc;
		MyChannelParams *cp;

		/* get netstachanloc for this channel */
		netstachanloc = (char *) gettbl (keys, i);
		/* get MyChannelParams for this channel */
		cp = (MyChannelParams *) getarr (orbwfpeak->channels_arr, netstachanloc);
		/* loop through the bands for this channel */
		for (j=0; j<maxtbl(cp->bands); j++) {
			MyFilterParams *fp;

			/* get MyFilterParams for this channel-band */
			fp = (MyFilterParams *) gettbl (cp->bands, j);
			/* look for minimum statetime */
			if (fp->statetime == 0.0) continue;
			if (fp->statetime < statetime) statetime = fp->statetime;
		}
	}

	/* set the global statetime */
	if (statetime < 1.e30) orbwfpeak->statetime = statetime;

	return (0);
}

/* bury() callback function - just need to compute global
   state time, in orbwfpeak->statetime, before writing to
   the statefile. */
void
mybury ()

{
	statetime (&orbwfpeak_global);
}

/* parse the program parameter file */
int
parse_pf (char *pfname, Orbwfpeak *orbwfpeak)

{
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

	orbwfpeak->templates = newarr (0);
	if (orbwfpeak->templates == NULL) {
		register_error (0, "parse_pf: newarr(templates) error.\n");
		return (-1);
	}
	sp_default = mychannelparams_new (NULL, pf);
	if (sp_default == NULL) {
		register_error (0, "parse_pf: mychannelparams_new(default) error.\n");
		return (-1);
	}
	setarr (orbwfpeak->templates, "default", sp_default);

	/* set the main Orbwfpeak program structure maxpkts from the default */
	orbwfpeak->maxpkts = sp_default->maxpkts;
	/* copy some stuff from the main Orbwfpeak program structure */
	sp_default->pcc = orbwfpeak->pcc;
	sp_default->orbout = orbwfpeak->orbout;
	sp_default->dbout = orbwfpeak->dbout;
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
	   parameters to override the default values and 4) set the orbwfpeak
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

		/* set the orbwfpeak->templates array */
		setarr (orbwfpeak->templates, name, sp);
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
	   channels_template tables in the orbwfpeak structure. We then work
	   our way through the channels_tbl local table, read and parse
	   each entry into channel expressions and template names and
	   fill in the orbwfpeak channels and channels_template tables. */

	orbwfpeak->channels = newtbl (1);
	if (orbwfpeak->channels == NULL) {
		register_error (0, "parse_pf: newtbl(channels) error.\n");
		return (-1);
	}
	orbwfpeak->channels_for_pcp = newtbl (1);
	if (orbwfpeak->channels_for_pcp == NULL) {
		register_error (0, "parse_pf: newtbl(channels_for_pcp) error.\n");
		return (-1);
	}
	orbwfpeak->channels_template = newtbl (1);
	if (orbwfpeak->channels_template == NULL) {
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

		/* fill in the channels and channels_template tables in orbwfpeak */
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
		settbl (orbwfpeak->channels, -1, expr_dup);
		settbl (orbwfpeak->channels_template, -1, templ_dup);

		/* Find the template and compose the channels_for_pcp table.
		   This monkey business is necessary so that the input
		   channels table for pktchannel_new(3) contains also the maxpkts
		   field which can be different for each of the table entries. */
		sp = (MyChannelParams *) getarr (orbwfpeak->templates, templ_dup);
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
		settbl (orbwfpeak->channels_for_pcp, -1, expr_dup);
	}
	/* free the local channels table */
	freetbl (channels_tbl, 0);

	/* parse the channels_reject table */

	orbwfpeak->channels_reject = NULL;
	if (parse_param (pf, "channels_reject", P_TBL, 0, &(orbwfpeak->channels_reject)) < 0) {
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
				register_error (0, "mychannelparams_new: newtbl(%s) error.\n",
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

	if (parse_param (pf, "maxpkts", P_LINT, required, &(cp->maxpkts)) < 0) {
		register_error (0, "mychannelparams_new: parse_param(maxpkts) error.\n");
		return (NULL);
	}
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
		fp->time = 0.0;
		fp->dt = 0.0;
		fp->nsamps = 0;
		fp->sizeof_data = 0;
		fp->data = NULL;
		fp->filter_hook = NULL;
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
		fp->filter_string = ptr;
	}
	if (parse_param (pf, "fpad", P_DBL, required, &(fp->fpad)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(fpad) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "fc", P_DBL, required, &(fp->fc)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(fc) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "twin", P_DBL, required, &(fp->twin)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(twin) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "dtpeak", P_DBL, required, &(fp->dtpeak)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(dtpeak) error.\n");
		return (NULL);
	}
	if (parse_param (pf, "tmin", P_DBL, required, &(fp->tmin)) < 0) {
		register_error (0, "myfilterparams_new: parse_param(tmin) error.\n");
		return (NULL);
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

/* usage printout */
void
usage()

{
	fprintf (stderr, "usage: orbwfpeak [-start {pktid|time|OLDEST}] [-select sexpr]\n");
	fprintf (stderr, "                [-reject rexpr] [-end tend_time] [-pf pfname]\n");
	fprintf (stderr, "                [-S statefile] [-v|-vv] dbname orbin [orbout]\n");
}

#include "orbwfpeak_version.h"

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

	n = strlen(Orbwfpeak_Version);
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
	memcpy (line2, "orbwfpeak startup", strlen("orbwfpeak startup"));
	elog_notify (0, "%s%s%s\n", pre2, line2, post2);
	elog_notify (0, "%s%s%s\n", pre2, Orbwfpeak_Version, post2);
	elog_notify (0, "%s%s%s\n", pre1, line1, post1);

}
