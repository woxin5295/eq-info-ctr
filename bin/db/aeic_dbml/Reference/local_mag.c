/*
 * local_mag.c
 *
 * Earthworm module to calculate a local magnitude upon request
 *
 * Kent Lindquist
 *   in consultation with Roger Hansen
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * June 1996
 */

#ifdef _OS2
FAIL--not coded for OS2
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <signal.h>
#include <time.h>
#include <earthworm.h>
#include <kom.h>
#include <transport.h>
#include <wave_client.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "tttaup.h"
#include <site_iw_ext.h>
#include "local_mag_util.h"

/*
 * Functions in this source file 
 */
void local_mag_config( char * );
void local_mag_lookup( void );
void local_mag_status( unsigned char, short, char * );
void local_mag_mlrequest( char * );
void terminate_if_requested( void );
void handle_getmsg_problem( int, MSG_LOGO *, long, char * );

static SHM_INFO Region1;	/* shared memory region to use for i/o    */
static SHM_INFO Region2;	/* private buffering region for local_mag */

#define STREQ(a, b) (strcmp((a), (b)) == 0)

#define  MAXLOGO   2
MSG_LOGO GetLogo[MAXLOGO];	/* array for requesting
					 * module,type,class */
short	nLogo;

#define MSG_SIZE 60000		/* define maximum size for an event msg   */
#define MAX_LOGMSG_SIZE 256	/* Maximum size of a log message */
#define REGION2SIZE (4*MSG_SIZE) /* size of local_mag's private buffering
					region */
#define PRE_P_SEC 3		/* Number of seconds before P wave to begin
				    looking for max amp */
#define NAPTIME_LIMIT 180       /* Maximum time to wait to make a waveform
				    request because of anticipated travel-time
				    of the seismic waves */
#define CALFREQ 5.		/* Frequency(Hz) at which to apply calibration */

#define ENDTIME(TIME,SAMPRATE,NSAMP)   ((TIME) + ((NSAMP)-1)/(SAMPRATE))

/*
 * Things to read or derive from configuration file 
 */
static char	RingName[20];	/* name of transport ring for i/o    */
static char	MyModName[20];	/* speak as this module name/id      */
static int 	LogSwitch;	/* 0 if no logfile should be written */
static int	Verbose;	/* 1 means print out lots of messages to log */
static long	HeartBeatInterval;	/* seconds between heartbeats */
char	WoodAndersonResponseFile[FILENAME_MAX];
				/* full pathname of file with wood-anderson
				   response curve (format as specified
				   for Datascope Seismic Application 
				   Package, q.v.) */
int BandpassFilterOrder; 	/* order of bandpass filter for local mag */
double BandpassUpperCutoff;	/* upper cutoff (Hz) of bandpass filter */
double BandpassLowerCutoff;	/* lower cutoff (Hz) of bandpass filter */
static char	MagNetwork[9];	/* Name of station network to use for 
					magnitude calculations */
static float	MaxVal;		/* Maximum value expected for all traces.
					Note that it treats all channels
					equally. */
static double	ClipThresh;	/* Fractional amount of MaxVal above which
					seismogram is declared clipped */
static double	Spreading;	/* Geometric spreading constant for local-mag */
static double	Atten;		/* Attenuation constant per km for local-mag */
static double	Refdist;	/* Reference distance (km) for local-mag */
static double	Refmag;		/* Magnitude at Reference distance */

/*
 * Things to look up in the earthworm.h tables with getutil.c functions 
 */
static long	RingKey;	/* key of transport ring for i/o     */
static long	BufferKey;	/* key to local_mag's buffering ring */
static unsigned char InstId;	/* local installation id             */
static unsigned char MyModId;	/* Module Id for this program        */
static unsigned char TypeHeartBeat;
static unsigned char TypeError;
static unsigned char TypeMlRequest;
static unsigned char TypeMlReport;

/*
 * Error messages used by local_mag 
 */
#define  ERR_MISSMSG       0	/* message missed in transport ring       */
#define  ERR_TOOBIG        1	/* retreived msg too large for buffer     */
#define  ERR_NOTRACK       2	/* msg retreived; tracking limit exceeded */
#define  ERR_MLREQREAD     3    /* error reading retrieved Ml request     */

main( int argc, char **argv )
{
	long	timeNow;/* current time                  */
	long	timeLastBeat;	/* time last heartbeat was sent  */
	char	msg[MSG_SIZE];	/* character string to hold event message */
	long	recsize; 	/* size of retrieved message     */
	MSG_LOGO reclogo;	/* logo of retrieved message     */
	int	res;
	char	*sitedb;

	if( argc != 2 ) 
	{
		fprintf( stderr, "Usage: local_mag <configfile>\n" );
		exit( 0 );
	}

	local_mag_config( argv[1] );

	local_mag_lookup();

	if( ( sitedb = getenv( "SITE_DB" ) ) == NULL )
	{
		fprintf(stderr, "Environment variable SITE_DB not defined; ");
		fprintf(stderr, "local_mag exiting.\n");
		exit( -1 );
   	} 

	read_site_db( sitedb );
	if( lookup_network( sitedb, MagNetwork ) <= 0 )
	{
		fprintf(stderr, "Found no stations in %s affiliated ", sitedb );		fprintf(stderr, "with network %s. Exiting.\n", MagNetwork );
		exit( -1 );
	}

	logit_init( "local_mag", (short) MyModId, MAX_LOGMSG_SIZE, LogSwitch );
	logit( "", "%s: Read command file <%s>\n", argv[0], argv[1] );

	tport_attach( &Region1, RingKey );
	logit( "", "local_mag: Attached to public memory region %s: %d\n",
	      RingName, RingKey );

	tport_create( &Region2, REGION2SIZE, BufferKey );
	logit( "", "local_mag: Attached to private memory region: %ld.\n",
		Region2.key );

	if ( tport_buffer( &Region1, &Region2, GetLogo, nLogo, sizeof(msg)-1,
		MyModId, InstId ) == -1 )
	{
		tport_destroy( &Region2 );
		logit( "et",
		 "local_mag: Error starting input buffer thread; exiting!\n" );
		exit( -1 );
	}
	else
	{
		logit( "t", "local_mag: Started input buffer thread.\n" );
	}

	/*
	 * Force a heartbeat to be issued in first pass thru main loop 
	 */
	timeLastBeat = time( &timeNow ) - HeartBeatInterval - 1;

	/*------------------ setup done; start main loop --------------------*/

	for( ;; ) 
	{
		if ( time( &timeNow ) - timeLastBeat >= HeartBeatInterval ) 
		{
			timeLastBeat = timeNow;
			local_mag_status( TypeHeartBeat, 0, "" );
		}
		/*
		 * Process all new messages 
		 */
		do 
		{
			terminate_if_requested();

			res = tport_getmsg( &Region2, GetLogo, nLogo,
			    &reclogo, &recsize, msg, sizeof( msg ) - 1 );

                        switch( res )
			{
                        case GET_OK:
                                break;
                        case GET_NONE:
                                continue;
                        case GET_TOOBIG:
                                handle_getmsg_problem(res,&reclogo,recsize,msg);
                                continue;
                        case GET_MISS:
                        case GET_NOTRACK:
                                handle_getmsg_problem(res,&reclogo,recsize,msg);
                                break;
                        default:
                                /* Shouldn't happen */
                                continue;
                        }
 
                        /* Fall through: Process the message */
 
                        msg[recsize] = '\0';
 
			if ( reclogo.type == TypeMlRequest )
			{
				local_mag_mlrequest( msg );
			}

		} while ( res != GET_NONE );	/* end of
						 * message-processing-loop */

		sleep_ew( 1000 );/* no more messages; wait for new ones to 
					arrive */

	}
}

/******************************************************************************
 *  local_mag_config() processes command file(s) using kom.c functions;        *
 *                    exits if any errors are encountered.                    *
 ******************************************************************************/
void 
local_mag_config( char *configfile )
{
	int	ncommand; /* # of required commands you expect to process */
	char	init[18]; /* init flags, one byte for each required command */
	int	nmiss;	/* number of required commands that were missed */
	char	*com;
	char	*str;
	int	nfiles;
	int	success;
	int	i;

	/*
	 * Set to zero one init flag for each required command 
	 */
	ncommand = 18;
	for ( i = 0; i < ncommand; i++ )
		init[i] = 0;
	nLogo = 0;

	/*
	 * Open the main configuration file 
	 */
	nfiles = k_open( configfile );
	if ( nfiles == 0 ) 
	{
		fprintf( stderr,
		   "local_mag: Error opening command file <%s>; exiting!\n",
			configfile );
		exit( -1 );
	}
	/*
	 * Process all command files 
	 */
	while ( nfiles > 0 ) 
	{	/* While there are command files open */
		while ( k_rd() ) 
		{/* Read next line from active file  */
			com = k_str();	/* Get the first token from line */

			/*
			 * Ignore blank lines & comments 
			 */
			if ( !com )
				continue;
			if ( com[0] == '#' )
				continue;

			/*
			 * Open a nested configuration file 
			 */
			if ( com[0] == '@' ) 
			{
				success = nfiles + 1;
				nfiles = k_open( &com[1] );
				if ( nfiles != success ) 
				{
					fprintf( stderr,
					"local_mag: Error opening command " );
					fprintf( stderr,
					"file <%s>; exiting!\n", &com[1] );
					exit( -1 );
				}
				continue;
			}
			/*
			 * Process anything else as a command 
			 */
			/* 0 */ if ( k_its( "LogFile" ) ) 
			{
				LogSwitch = k_int();
				init[0] = 1;
			}
			 /* 1 */ 
			else if ( k_its( "MyModuleId" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( MyModName, str );
				init[1] = 1;
			}
			 /* 2 */ 
			else if ( k_its( "RingName" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( RingName, str );
				init[2] = 1;
			}
			 /* 3 */ 
			else if ( k_its( "HeartBeatInterval" ) ) 
			{
				HeartBeatInterval = k_long();
				init[3] = 1;
			}
			/*
			 * Enter installation & module to get event messages
			 * from 
			 */
                         /* 4 */
                        else if ( k_its( "GetRequestsFrom" ) ) 
			{
                           if ( nLogo >= MAXLOGO ) 
			   {
                              fprintf( stderr,
                                "local_mag: Too many <GetRequestsFrom> " );
                              fprintf( stderr,"commands in <%s>",
                                        configfile );
                              fprintf( stderr, "; max=%d; exiting!\n",
                                        (int) MAXLOGO / 2 );
                              exit( -1 );
                           }
                           if ( ( str = k_str() ) ) 
			   {
                                if ( GetInst( str, &GetLogo[nLogo].class ) != 0 ) 
				{
                                   fprintf( stderr,
                                   "local_mag: Invalid installation name <%s>",
                                   str );
                                   fprintf( stderr,
                                   " in <GetRequestsFrom> cmd; exiting!\n" );
                                   exit( -1 );
                                }
                           }
                           if ( ( str = k_str() ) ) 
			   {
                                if ( GetModId( str, &GetLogo[nLogo].mod ) != 0 ) 
				{
                                   fprintf( stderr,
                                   "local_mag: Invalid module name <%s>", str );
                                   fprintf( stderr,
                                   " in <GetRequestsFrom> cmd; exiting!\n" );
                                   exit( -1 );
                                }
                           }
                           if ( ( str = k_str() ) ) 
			   {
                                if ( GetType( str, &GetLogo[nLogo].type ) != 0 ) 
				{
                                   fprintf( stderr,
                                      "local_mag: Invalid message type <%s>",
                                      str );
                                   fprintf( stderr, "; exiting!\n" );
                                   exit( -1 );
                                }
                           }
			   nLogo += 1;
			   init[4] = 1;
			}
			/* 5 */ 
			else if ( k_its( "Verbose" ) ) 
			{
				Verbose = k_int();
				init[5] = 1;
			}
			 /* 6 */ 
			else if ( k_its( "MagNetwork" ) ) 
			{
				str = k_str();
				if ( str ) strcpy( MagNetwork, str );
				init[6] = 1;
			}
			 /* 7 */
			else if ( k_its( "WaveServer" ) )
			{
				str = k_str();
				if ( str ) strcpy( WaveServer, str );
				init[7] = 1;
			}
			 /* 8 */
			else if ( k_its( "MaxVal" ) )
			{
				MaxVal = (float) k_val();
				init[8] = 1;
			}
			 /* 9 */
			else if ( k_its( "ClipThresh" ) )
			{
				ClipThresh = k_val();
				init[9] = 1;
			}
			 /* 10 */
			else if ( k_its( "Spreading" ) )
			{
				Spreading = k_val();
				init[10] = 1;
			}
			 /* 11 */
			else if ( k_its( "Atten" ) )
			{
				Atten = k_val();
				init[11] = 1;
			}
			 /* 12 */
			else if ( k_its( "Refdist" ) )
			{
				Refdist = k_val();
				init[12] = 1;
			}
			 /* 13 */
			else if ( k_its( "Refmag" ) )
			{
				Refmag = k_val();
				init[13] = 1;
			}
			 /* 14 */
			else if ( k_its( "WoodAndersonResponseFile" ) )
			{
				str = k_str();
				if ( str )
					strcpy( WoodAndersonResponseFile, str );
				init[14] = 1;
			}
			/* 15 */ 
			else if ( k_its( "BandpassFilterOrder" ) ) 
			{
				BandpassFilterOrder = k_int();
				init[15] = 1;
			}
			 /* 16 */
			else if ( k_its( "BandpassUpperCutoff" ) )
			{
				BandpassUpperCutoff = k_val();
				init[16] = 1;
			}
			 /* 17 */
			else if ( k_its( "BandpassLowerCutoff" ) )
			{
				BandpassLowerCutoff = k_val();
				init[17] = 1;
			}
			/*
			 * Unknown command 
			 */
			else 
			{
				fprintf( stderr,
				   "local_mag: <%s> Unknown command in <%s>.\n",
				   com, configfile );
				continue;
			}

			/*
			 * See if there were any errors processing the
			 * command 
			 */
			if ( k_err() ) 
			{
				fprintf( stderr,
				"local_mag: Bad <%s> command in <%s>; exiting!\n",
					com, configfile );
				exit( -1 );
			}
		}
		nfiles = k_close();
	}

	/*
	 * After all files are closed, check init flags for missed commands 
	 */
	nmiss = 0;
	for ( i = 0; i < ncommand; i++ )
		if ( !init[i] )
			nmiss++;
	if ( nmiss ) 
	{
		fprintf( stderr, "local_mag: ERROR, no " );
		if ( !init[0]) fprintf( stderr, "<LogFile> " );
		if ( !init[1] ) fprintf( stderr, "<MyModuleId> " );
		if ( !init[2] ) fprintf( stderr, "<RingName> " );
		if ( !init[3] ) fprintf( stderr, "<HeartBeatInterval> " );
		if ( !init[4] ) fprintf( stderr, "<GetRequestsFrom> " );
		if ( !init[5] ) fprintf( stderr, "<Verbose> " );
		if ( !init[6] ) fprintf( stderr, "<MagNetwork> " );
		if ( !init[7] ) fprintf( stderr, "<WaveServer> " );
		if ( !init[8] ) fprintf( stderr, "<MaxVal> " );
		if ( !init[9] ) fprintf( stderr, "<ClipThresh> " );
		if ( !init[10] ) fprintf( stderr, "<Spreading> " );
		if ( !init[11] ) fprintf( stderr, "<Atten> " );
		if ( !init[12] ) fprintf( stderr, "<Refdist> " );
		if ( !init[13] ) fprintf( stderr, "<Refmag> " );
		if ( !init[14] ) fprintf( stderr, "<WoodAndersonResponseFile> ");
		if ( !init[15] ) fprintf( stderr, "<BandpassFilterOrder> ");
		if ( !init[16] ) fprintf( stderr, "<BandpassUpperCutoff> ");
		if ( !init[17] ) fprintf( stderr, "<BandpassLowerCutoff> ");
		fprintf( stderr, "command(s) in <%s>; exiting!\n", configfile );
		exit( -1 );
	}
	return;
}

/******************************************************************************
 *  local_mag_lookup()   Look up important info from earthworm.h tables       *
 ******************************************************************************/
void 
local_mag_lookup( void )
{
	/*
	 * Look up keys to shared memory regions 
	 */
	if ( ( RingKey = GetKey( RingName ) ) == -1 ) 
	{
		fprintf( stderr,
		"local_mag:  Invalid ring name <%s>; exiting!\n", RingName );
		exit( -1 );
	}
	if( ( BufferKey = GetKey( "LOCAL_MAG_RING") ) == -1 )
	{
		fprintf( stderr,
		"local_mag:  Invalid ring name <LOCAL_MAG_RING>; exiting!\n" );
		exit( -1 );
	}
	/*
	 * Look up installations of interest 
	 */
	if ( GetLocalInst( &InstId ) != 0 ) 
	{
		fprintf( stderr,
		"local_mag: error getting local installation id; exiting!\n" );
		exit( -1 );
	}
	/*
	 * Look up modules of interest 
	 */
	if ( GetModId( MyModName, &MyModId ) != 0 ) 
	{
		fprintf( stderr,
		"local_mag: Invalid module name <%s>; exiting!\n", MyModName );
		exit( -1 );
	}
	/*
	 * Look up message types of interest 
	 */
	if ( GetType( "TYPE_HEARTBEAT", &TypeHeartBeat ) != 0 ) 
	{
		fprintf( stderr,
		"local_mag: Invalid message type <TYPE_HEARTBEAT>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_ERROR", &TypeError ) != 0 ) 
	{
		fprintf( stderr,
		"local_mag: Invalid message type <TYPE_ERROR>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_ML_REQUEST", &TypeMlRequest ) != 0 ) 
	{
		fprintf( stderr,
		"local_mag: Invalid message type <TYPE_ML_REQUEST>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_ML_REPORT", &TypeMlReport ) != 0 ) 
	{
		fprintf( stderr,
		"local_mag: Invalid message type <TYPE_ML_REPORT>; exiting!\n" );
		exit( -1 );
	}
	return;
}

/******************************************************************************
 * local_mag_status() builds a heartbeat or error message & puts it into       *
 *                   shared memory.  Writes errors to log file & screen.      *
 ******************************************************************************/
void 
local_mag_status( unsigned char type, short ierr, char *note )
{
	MSG_LOGO logo;
	char	msg[MAX_LOGMSG_SIZE];
	long	size;
	long	t;

	/*
	 * Build the message 
	 */
	logo.class = InstId;
	logo.mod = MyModId;
	logo.type = type;

	time( &t );

	if ( type == TypeHeartBeat ) 
	{
		sprintf( msg, "%ld\n\0", t );
	} 
	else if ( type == TypeError ) 
	{
		sprintf( msg, "%ld %hd %s\n\0", t, ierr, note );
		logit( "et", "local_mag: %s\n", note );
	}
	size = strlen( msg );	/* don't include the null byte in the message */

	/*
	 * Write the message to shared memory 
	 */
	if ( tport_putmsg( &Region1, &logo, size, msg ) != PUT_OK ) 
	{
		if ( type == TypeHeartBeat ) 
		{
			logit( "et", "local_mag:  Error sending heartbeat.\n" );
		} 
		else if ( type == TypeError ) 
		{
			logit( "et",
			 "local_mag:  Error sending error:%d.\n", ierr );
		}
	}
	return;
}

/********************************************************************/
/* terminate_if_requested() checks the ring for a TERMINATE request */
/* And shuts down local_mag if such a request is found             */
/********************************************************************/
void
terminate_if_requested( void  )
{
        if ( tport_getflag( &Region1 ) == TERMINATE )
        {
                tport_detach( &Region1 );
		tport_destroy( &Region2 );
                logit( "t", "local_mag: Termination requested; exiting\n" );
                fflush( stdout );
                exit( 0 );
        } 
	else 
	{
                return;
        }
}
/***********************************************************************/
/* handle_getmsg_problem() squeals about all difficulties that occurred */
/*  when reading messages off the shared-memory ring                    */
/***********************************************************************/
void
handle_getmsg_problem( int problem, MSG_LOGO *reclogo, long recsize, char *msg )
{
        char    Text[150];
 
        switch( problem )
        {
        case GET_TOOBIG:
                sprintf( Text,
                        "Retrieved msg[%ld] (c%u m%u t%u) too big for msg[%d]",
                        recsize,
                        reclogo->class,
                        reclogo->mod,
                        reclogo->type,
                        sizeof( msg ) - 1 );
                local_mag_status( TypeError, ERR_TOOBIG, Text );
                break;
        case GET_MISS:
                sprintf( Text, "Missed msg(s)  c%u m%u t%u  %s.",
                        reclogo->class,
                        reclogo->mod,
                        reclogo->type,
                        RingName );
                local_mag_status( TypeError, ERR_MISSMSG, Text );
                break;
        case GET_NOTRACK:
                sprintf( Text,
                "Msg received (c%u m%u t%u); transport.h NTRACK_GET exceeded",
                          reclogo->class, reclogo->mod, reclogo->type );
                local_mag_status( TypeError, ERR_NOTRACK, Text );
                break;
        default:
                /* Shouldn't happen */
                break;
        }
 
        return;
}

/******************************************************************************
 * local_mag_mlrequest( ) process a request for a local-magnitude calculation *
 ******************************************************************************/
void 
local_mag_mlrequest( char *msg )
{
	char	*msgptr;
	char	msgtemp[MAX_LOGMSG_SIZE];
	char	rptmsg[MSG_SIZE];
	char	stamsg[STRSZ];
	char	Text[150];
	long	quake_seq;
	double	quake_lat, quake_lon, quake_depth;
	int	nass;
	MSG_LOGO logo;
	int	length;
	int	narg;
	char	sta[7];
	char	chan[9];
	double	arrtime;
	double	delta, dist_km;
	double	esaz;
	double	reqs, reqe;
	int	index;
	STACHAN *stachan;
	int	rc;
	double	ts, samprate;
	int	nsamp;
	char	datatype[3];
	DataPtr data;
	double	calib;
	char	respfile[FILENAME_MAX];
	double	ml, mlunc;
	double	netmag, netmag_unc;
	double	tmeas, amp_delta;
	double	ZtoP_amp_nm;
	int	nsta;			/* Number of stamags for a netmag */
	Smrpt	*stamag;		
	Tbl	*stamags;
	WSEntry	*menu;
	int	n_menu_stachans;
	int	i_menu;
	int	found;
	int	sleeptime;
	int	slept;

	logo.class = InstId;
	logo.mod = MyModId;
	logo.type = TypeMlReport;

	if( Verbose )
	{
		msgptr = msg;
		logit( "t", "Received request:\n" );
		while( msgptr < msg + strlen(msg) )
		{
			strncpy( msgtemp, msgptr, MAX_LOGMSG_SIZE-1);
			msgtemp[MAX_LOGMSG_SIZE-1] = '\0';
			logit( "", "%s", msgtemp );
			msgptr += MAX_LOGMSG_SIZE-1;
		}
		logit( "", "\n" );
	}

	msgptr = msg;

	narg = sscanf( msgptr, "%ld %lf %lf %lf %d", 
			&quake_seq, &quake_lat,
			&quake_lon, &quake_depth, &nass );
	if( narg < 5 )
	{
		sprintf( Text, "local_mag_mlrequest: Error reading request" );
		local_mag_status( TypeError, ERR_MLREQREAD, Text );
		return;
	}

	rc = wave_client_get_menu( &n_menu_stachans, &menu );
	if( rc < 0 )
	{
		logit( "t",
			"Failed with %d to get menu from wave server\n", rc );
		return;

	}

	nsta = 0;
	stamags = newtbl( 0 );
	slept = 0;

	for( index=0; index<nass; index++ )
	{
		msgptr = (char *) strpbrk( msgptr, "\n" );
		msgptr++;
		narg = sscanf( msgptr, "%s %s %lf", sta, chan, &arrtime );
		if( narg < 3 )
		{
			sprintf( Text,
			"local_mag_mlrequest: Error reading request" );
			local_mag_status( TypeError, ERR_MLREQREAD, Text );
			freetbl( stamags, free );
			free( menu );
			return;
		}
		if( station_in_network( sta ) == 0 )
		{
			if( Verbose ) logit( "", 
				"Station %s not in %s network\n",
				sta, MagNetwork );
			continue;
		}
		if( ( stachan = lookup_stachan( sta, chan ) ) == NULL )
		{
			logit( "t", "Couldn't find %s %s in station info\n",
				sta, chan );
			continue;
		}

		if( stachan->calib == 0. )
		{
			logit( "t", "Null calib for %s %s\n", sta, chan );
			continue;
		}
		else
		{
			calib = stachan->calib;
		}

		if( STREQ( stachan->respfile, "" ) )
		{
			logit( "t", "No response file for %s %s\n", sta, chan );
			continue;
		}

		dist( rad( quake_lat ), rad( quake_lon ),
                      rad( stachan->lat ), rad( stachan->lon ),
                      &delta, &esaz );
 
                delta = deg( delta );
		dist_km = deg2km( delta );
		/* Use hypocentral distance: */
		dist_km = sqrt( dist_km * dist_km + quake_depth * quake_depth );

		reqs = arrtime - PRE_P_SEC;
		reqe = arrtime - ptime( delta, quake_depth ) +
				2 * stime( delta, quake_depth );

		found = 0;
		i_menu = 0;
		while( i_menu < n_menu_stachans && found == 0 )
		{
			if( STREQ( sta, menu[i_menu].sta ) &&
			    STREQ( chan, menu[i_menu].chan ) )
			{
				found = 1;
				break;
			}
			else
			{
				i_menu++;
			}
		}
		if( found )
		{
			sleeptime = (int) 
				ceil( reqe - menu[i_menu].tend ) - slept;
		}
		else
		{
			logit( "t", 
			   "Couldn't find wave-server menu entry for %s %s\n",
			   sta, chan );
			continue;
		}

		if( sleeptime > NAPTIME_LIMIT )
		{
			logit( "t", "Nap too long (%d sec) for %s:%s\n",
				sleeptime, sta, chan );
			continue;
		}
		if( sleeptime > 0 )
		{
			sleep_ew( sleeptime * 1000 );
			slept += sleeptime;
		}

		rc = wave_client_stachan_to_buf( sta, chan, NULL, reqs, reqe, 
					GAP_FAIL, &ts, &nsamp, &samprate, 
					datatype, &data );
		if( rc < 0 ) 
		{
			logit( "", "No data for %s %s, return code %d\n",
				sta, chan, rc );
			continue;
		}

		rc = wave_client_buf_to_float( datatype, nsamp, &data );
		if( rc < 0 )
		{
			logit( "",
				"Conversion to float failed for %s %s, rc %d\n",
				sta, chan, rc );
			free( data.c );
			continue;
		}

		if( clip_check( data.t4, nsamp, MaxVal, ClipThresh )
		    && ( ! STREQ( sta, "COLA" ) ) ) /* HACK */
		{
			logit( "",
				"%s %s clipped (exceeds %f * %f)\n",
				sta, chan, ClipThresh, MaxVal );
			free( data.c );
			continue;
		}

		rc = measure_local_mag( data.t4, ts, nsamp, samprate,
				calib, CALFREQ, stachan->respfile,
				arrtime, dist_km, Spreading, Atten, Refdist,
				Refmag, &ml, &mlunc, &tmeas, &amp_delta, 
				&ZtoP_amp_nm );
		if( rc < 0 )
		{
			logit( "",
				"Local Mag measurement failed for %s %s\n",
				sta, chan );
			free( data.c );
			continue;
		}
		else 
		{
			stamag = (Smrpt *) malloc( sizeof( Smrpt ) );

			strcpy( stamag->sta, sta );
			strcpy( stamag->chan, chan );
			stamag->time = ts;
			stamag->endtime = ENDTIME( ts, samprate, nsamp );
			stamag->tmeas = tmeas;
			stamag->PtoP_timediff = amp_delta;
			stamag->ZtoP_amp_nm = ZtoP_amp_nm;
			stamag->stamag = ml;
			stamag->uncert = mlunc;

			pushtbl( stamags, stamag );

			if( Verbose ) logit( "", 
				"stamag %s %s %6.3f %6.3f\t%f %f %f\n",
				sta, chan, ml, mlunc,
				tmeas, amp_delta, ZtoP_amp_nm );
		}

		free( data.c );
	}

	stamags_to_netmag( stamags, &netmag, &netmag_unc );
	nsta = maxtbl( stamags );

	if( Verbose ) logit( "", "qkseq %ld netmag %f +- %f with %d stations\n",
			quake_seq, netmag, netmag_unc, nsta );

	sprintf( rptmsg, "%ld %6.3f %6.3f %d\n",
		quake_seq, netmag, netmag_unc, nsta );
	for( index = 0; index < nsta; index++ )
	{
		stamag = poptbl( stamags );
		sprintf( stamsg, "%s %s %f %f %f %f %f %6.3f %6.3f\n",
			stamag->sta, stamag->chan,
			stamag->time, stamag->endtime,
			stamag->tmeas, stamag->PtoP_timediff,
			stamag->ZtoP_amp_nm,
			stamag->stamag, stamag->uncert );
		strcat( rptmsg, stamsg );
	}

	length = strlen( rptmsg );
	if( tport_putmsg( &Region1, &logo, length, rptmsg ) != PUT_OK )
	{
		logit( "et", "Error putting report on ring\n" );
	}
	freetbl( stamags, free );
	free( menu );
	return;
}
