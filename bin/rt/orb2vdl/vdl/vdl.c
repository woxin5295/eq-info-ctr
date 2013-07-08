/*
	This is a modification of FRONT to provide Virtual Data Logger (VDL) for
	any system capable of supplying multiple continuous data streams.  Data are 
	put into ring buffers which are time tagged each trip around with the closest
	time available.  Data can be 50, 40, or 20 HZ continuous.  LP data (1 HZ)
	can be submitted or derived from the high frequency streams.  The derived LP
	goes through the filtering and decimation used in the NSN systems internal
	to the Quanterra data logger.  The FFT based NSN detection algorithm is run
	on the high frequency vertical every 12.8 seconds (256 points of 20 hz or 512
	points of 40 Hz).  Detection, compression and output of NSN style data packets
	follows the NSN model.  Currently two output media are supported - TCP/IP sockets
	to the NSNTCP program in Golden, and satellite based serial communications using
	NSN style VSATS to NSNICP in Golden.  

	Multichannel VDLs :

	the -v filename command line argument is used to specify a file which
	contains the station configuration.  This may be temporary since a two
	way TCP/IP could allow dynamic configuration based on the protocol used
	for the PDP-11 based Phoenix eliminators.  The file contains the 
	following information :

var		type	Description
txt	 char[]	A non-white space text used to describe the channel (station name)

ch		int	The internal channel number to use for this component

stch 	int	The NSN station channel byte to use for data from this channel.

showdet	int	If true, detailed information about detector on this channel goes
				In the log file.

decim	int	Decimation factor needed to get data to correct rate.  This must
			be a power of 2 at this time (a multiple divide by 2 scheme).

freq	int	Frequency of the data (after decimation) for this channel.  In
				multichannel mode, if this matches the -f frequency the 
				channel will be triggered.

derivelpint	If positive, derive LP from this channel and put in derive_lp chan
				Of course, the corresponding channel should be set up as a 1sps
				continuous channel.

fwd[3]	int	Array of 3 channels to send if this channel triggers

delay	int	Delay in MS to Add to time codes to compensate for telemetry.  The
				delay introduced by the decimation is calculated in this 
				program and added to this delay.

expbias	int	The exponential bias to used when normalizing the power data

coin	int Number of bands which must be above the S/N threshold to cause
			a trigger.  If <0, then input data is triggered and should be passed on.

retire	int	The retirement age for history peaks in FFT buffer cycles.

sntrig	float	The Signal/noise (S/N) ratio which when exceeded causes the
				power windows to vote for a trigger.

nfsntrig float	Similar for HF data.  Not used in VDL at this time.

nfreq	int	Number of power windows to use in deciding if a trigger occured.

trigfreq	int[]  The power window numbers to check.  The first NFREQ of these
					get to vote.  If above the S/N they vote yes and if NCOIN
					windows vote yes, a detection is declared.

	initial functionality D.C. Ketchum May 1995 

	To implement a new stations requires the user to write three routines :

	feedme_init(argc,argv)		// called once at program start up 
	int argc;
	char **argv;

	int feedme(ich,ia,tc)		// returns data for channel ich, data in IA, & time 
	int *ich;					// channel returned 
	long ia[];					// 32 bit data buffer 
	struct nsn_time tc;			// uSNSN time code for first sample 

	feedme_shutdown()			// Called once at program shutdown or abort 

	Routine feedme_init() is called near startup to configure the system at
	the beginning.  It gets the command line arguments so they can be used to
	set configuration information.  VDL has its own set of arguments always of the
	form -t vvv where t is a single character and vvv is some value or string.

	-a gain	Send an analog "helicoder" channels at 10 sps attenuate by gain*100.
	-b v	Set the high frequency data group delay to v milleseconds.
	-c v	If v not zero, the output high frequency stream is to be continuous
	-cph v	If v not zero, the output BB from Phoenix eliminator is continuous.
	-d devnam	Set the serial style device name to devnam.
	-e		reserved for earthworm
	-f vvv	Sets the high frequency data rate.  Normally, 20 or 40.
	-F vvv	Decimal nominal digit rate (sets ch.freq_calc) (add 1000 to print)
	-h v	Set the LP group delay to v in ms (for non-derived LP data)
	-host adr	Host address or socket address for output socket.  If the host
				address is set to "local", then a local socket is used to
				communicate with VDLPASS.
	-i SSS	Set station tag to SSS (the station name in ASCII)
	-j vvv	Set the digit_freq variable to native digitizing rate
	-logpath Set the log file path to following argument, concatenate with
			-i tag and .log? to make filename,
	-k v	If v non=zero, allow command input.
	-l v	If v not zero, derive the LP from the high frequencey data
	-n v	The NSN network id number of the stations is v.
	-o dev	VDLPASS uses device following for serial output via VSAT.
	-p		Reserved for DATASOCK.
	-port n	Port number for output socket.  If VDLPASS this should be socket
			must be the same as the one used when VDLPASS starts.
	-q 1	Set SH Seed channels instead of BH
	-s v	The NSN node number of the station is v.
	-t v	If v  == 0, use serial VSAT communications, if t == 1, use TCP/IP
	-T vv	if v == 0, continuous BB input.  If v=1, triggered (segmented) input.
	
			This is the same as ncoin < 0 indicating all data should be passed as is.
	-v file	Set filename for configuring chans (used in Multiple station mode
	-x v	If set the exponent bias for the power to v (default=16)
	-z pipe	Sets the id number of the pipe that will be used to feed the program
	-! v	if non-zero, cause more detail of trigger to be recorded
	-+      If present, run with showdet on
	-]		If present, add packet level debug output
	-[ nn	Dump channel NN

In Feedme's these switches have been used :

earthworm
	-e filename	User parameter file (used by earthworm initially)

Datasock (UCB) :
	-H host.dots	Specify the host or numeric dot address to contact
	-P nnn			Port number on the host for this datasock.
	-S station		Set the Name of the Station to get
	-SL location	Limit data to one location
	-C chan,list	Comma delimited list of channels to get 
	-p password		Password for this port

ORB 
	-orbname name	orbname is an orbserver name on local system
	-orbchan regexp	The regexp is used to select channels from the orbserver

	Since the argc and argv are communicated to feedme_init() the user can define 
	other command line arguments as long as they do not conflict with the VDL
	specified ones.  

	Routine feedme() returns one channel of data.  Argument ICH should be set to
	CH_Z20, CH_X20 (north), or CH_Y20(east) (optionally CH_Z1, CH_X1, CH_Y1 for LP
	data.  These constants are defined in VDL.H. The data expressed as long integers
	is returned in array ia.  The NSN time code associated with the first returned
	point is in tc.  Gennerally feedme maps user channels to the CH_*, converts the
	data to long ints, and uses routine maknsn() to create an NSN style time code
	from the internal time code.  Feedme() returns the number of points in ia.  The
	routine should not return until data is available (i.e. always return a positive
	number of points).  ia is currently limited to 500 samples though this can be
	made larger by changing its dimension statement below.

	control of the program and restarting it are the responsibility of the user.  
	Feedme_init() often is used to fork a child which is then wait()ed for.  If the
	child dies, its is forked off again.  VDLTAP.C written for UCSD is an example of
	this kind of control.  The variety of ways of monitoring the VDLs is fairly 
	large and the style chosen seems best left to the owner of the system.


	
**********************************************************************/
/*#define DEBUG_PRINT 1 	/* if defined here or by cc (-D) log detail progress */

/*	
	COMPILER FLAGS

SUNOS _SOLARIS SOLARIS2 - select operating system.
SOLARIS2 is only used on datasock on solaris 

_INTEL		Intel type. Mostly used for byte swapping stuff on time codes etc
__STDC__	If set, use ANSI call declaration
DEBUG_PRINT Adds some output to see where stuff is bombing.
_EARTHWORM	On earthworm systems relative paths for logs are not good.  this
				give absolute paths for log files for earthworm systems
DEBUG		Many detailed routins like ifft, ifilt, will dump output with this

Earthworm code :
_OS2		Presumably yet another operating system
_SPARC		Seems to indicate SPARC byte ordering
_INTEL		Indicates PC/VAX ordering mostly.

*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>			/* error  number defs */
#include <math.h>				/* include Math handler */
#include "vdlfilt.h"		/* declare filtering struct and protos */
#include "vdl.h"				/* Include file for the NSN project*/
#include "vdlgbl.h"			/* Include global variable definitions */
#include "vdlqsub.h"		/* Declare QSUB prototypes and globals */
#include "ifft.h"				/* Declare FFT and Power calc protos */

#ifdef _UUSS
#include "uuss_tcp.h"
int uuss_tcp_port;
char uuss_tcp_host[MAXHOSTNAMELEN];
#endif

#include <sys/types.h>		/* Unix types */
#include <sys/ioctl.h>		/* declare io control routines stuff*/
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>					/* time stuff */
#ifdef _SOLARIS
#include <sys/filio.h>
#include <sys/termios.h>
#endif

/*		Declare function prototypes     */
void init_tcp();
void init_tt();
void initmemory();			/* uses malloc to allocate buffer space */
void init_tt();					/* initialize a serial style device for output */
void console();					/* reads input from STDIN and executes debugging commands*/
void newtrig();					/* declare a new trigger */
void detect();					/* run detector on vertical data stream */
void proc();						/* main call for processing time series */
void cmdarg();					/* processes command line args for VDL */
struct nsntime spfilt();			/* decimate/filter input data */
struct nsntime makedata();		/* debugging routine which manufactures a data stream */

/*	GLOBAL VARIABLES */

int iseed;									/* Random number seed used by makedat() */
char ttdev[]="/dev/ew_vdl";	/* string to open for terminal output */
char logPath[100] = "./";		/*have to be global */
#define LOG_FYL_EXT ".loga"
char logNam[sizeof(logPath) +1 + sizeof(tag)+5 ];
char* logPtr;
int capture;				/* if true, file with FEEDME blocks is made */
int capout;					/* file descriptor for close */
char date_compiled[50];

#ifdef SUNOS
/*
	E X I T _ H A N D L E R 
*/
#ifdef __STDC__ 
   void exit_handler(int status, int arg)
#else
  void exit_handler(status,arg)
  int status;
  int arg;
#endif
{
	int err;
	char false=0;
/*	err=ioctl(0,FIONBIO, &false);	/* set non blocking i/o */
	fprintf(logout,"On Exit handler status=%d arg=%d\n",status,arg);
	feedme_shutdown();
	return;
}
#endif

#ifdef _SOLARIS
#ifdef __STDC__ 
   void exit_handler()
#else
  void exit_handler()
#endif
{
	int err;
	char false=0;
	fprintf(logout,"On Exit handler. SOLARIS provide no info\n"); fflush(logout);
	if(capture > 0) close(capout);
	feedme_shutdown();
	return;
}
#endif
#ifdef __STDC__
  void cc_handler(int arg)
#else
  void cc_handler(arg)
  int arg;
#endif
{
	char *asctim();
	int err;
	char false=0;
	fprintf(logout,"%s cc_handler=%d\n",asctim(),arg); fflush(logout);
#ifdef SUNOS
/*	err=ioctl(0,FIONBIO, &false);	/* set non blocking i/o */
#endif
	switch (arg) {
		case SIGINT :
 			fprintf(logout,"%s control C (SIGINT) clean up\n",tag);
			break;
		case SIGHUP :
			fprintf(logout,"%s SIGHUP exit\n",tag);
			break;
		case SIGQUIT:
			fprintf(logout,"%s SIGQUIT exit\n",tag);
			break;
		case SIGILL:
			fprintf(logout,"%s SIGILL exit\n",tag);
			break;
		case SIGTRAP:
			fprintf(logout,"%s SIGTRAP exit\n",tag);
			break;
		case SIGABRT:
			fprintf(logout,"%s SIGABRT exit\n",tag);
			break;
		case SIGEMT:
			fprintf(logout,"%s SIGEMT exit\n",tag);
			break;
		case SIGFPE:
			fprintf(logout,"%s SIGFPE exit\n",tag);
			break;
		case SIGKILL:
			fprintf(logout,"%s SIGKILL exit\n",tag);
			break;
		case SIGBUS:
			fprintf(logout,"%s SIGBUS exit\n",tag);
			break;
		case SIGSEGV:
			fprintf(logout,"%s SIGSEGV exit\n",tag);
			break;
		case SIGSYS:
			fprintf(logout,"%s SIGSYS exit\n",tag);
			break;
		case SIGALRM:
			fprintf(logout,"%s SIGALRM exit\n",tag);
			break;
		case SIGTERM:
			fprintf(logout,"%s SIGTERM exit\n",tag);
			break;
		case SIGURG:
			fprintf(logout,"%s SIGURG exit\n",tag);
			break;
		case SIGSTOP:
			fprintf(logout,"%s SIGSTOP exit\n",tag);
			break;
		case SIGTSTP:
			fprintf(logout,"%s SIGTSTP exit\n",tag);
			break;
		case SIGCONT:
			fprintf(logout,"%s SIGCONT exit\n",tag);
			break;
		case SIGCHLD:
			fprintf(logout,"%s SIGCHLD exit\n",tag);
			break;
		case SIGTTIN:
			fprintf(logout,"%s SIGTTIN exit\n",tag);
			break;
		case SIGTTOU:
			fprintf(logout,"%s SIGTTOU exit\n",tag);
			break;
		case SIGIO:
			fprintf(logout,"%s SIGIO exit\n",tag);
			break;
		case SIGXCPU:
			fprintf(logout,"%s SIGXCPU exit\n",tag);
			break;
		case SIGXFSZ:
			fprintf(logout,"%s SIGXFSZ exit\n",tag);
			break;
		case SIGWINCH:
			fprintf(logout,"%s SIGWINCH RCVed/Ignored\n",tag);
			return ;
		default:
			fprintf(logout,"%s Unknown signal=%d \n",tag,arg);
	}
	feedme_shutdown();
	fflush(logout);
	exit(arg);				/* this will go to ON_EXIT handler which will shutdown */
}
/**************************************************************************
*
*	MAIN for FRONT
**************************************************************************/
#ifdef __STDC__
  main(int argc, char *argv[])
#else
  main(argc,argv)
  int argc;					/* user input command count */
  char *argv[];				/* user input command strings */
#endif
{
	static char true=1,false=0;
	int first;					/* flag for first time through main loop */
	char cmdchr;
	int ikill;					/* kill counter */
	extern int gbldoy;			/* the global doy, check for day roll overs*/
	extern int node;			/* This node number is assigned by the USGS*/
	extern int restart;			/* flag to determine restart state */
	long err,iyear,date,nchar,msadd;
	long iy,id,ih,im,is,ms,last,lastim,i,j,k,l,n,tonecount;
	long ia[MAX_NSAMP_IN];				/* buffers for made up data */
	struct nsntime tctmp,tc,tc2;/* local time code */
	char scratch[100];
	int trigflg;				/* set based on trigger value */
	int nsamp;					/* number of samples in data buffers */
	int ierr;					/* scratch variables */
	int ich,lpch;				/* the channel currently being processed*/
	int idiff;					/* used to decide if time to derive LP */
	long lpp;					/* a long period derived sample */
	int fd;						/* file descriptor for DEC's code */
	int dbgtrg;
	long output[80];			/* buffer space for timeseries decimation*/
	long *pnt;					/* internal pointer to history space - decimation*/
	int status_count=0;			/* determins when to dump status */
/*					End of debugging and temporary variables */
	double x;
	time_t time();				/* declare time function for GCC*/
	time_t clock;				/* place to stick the time unix*/
	struct tm *tm2;				/* pointer to parsed time for unix */
	ttpath=-1;
	datetime(date_compiled);	/* get compilation date */
	logout=stdout;
	lp=stdout;
	for(i=1; i<argc; i++)
	{	if( strcmp(argv[i], "-logpath") == 0)
		{	strcpy(logPath,argv[i+1]);
		}
		if( strcmp(argv[i], "-i") == 0)
		{	strcpy(tag,argv[i+1]);
		}
	}
#ifndef TERMINAL_LOG
#ifdef _EARTHWORM
       	/* Get path to log directory from environment variable EW_LOG
   	**********************************************************/
  	logPtr = getenv( "EW_LOG" );
   	if ( logPtr == NULL )
   	   {
      	   fprintf( stderr, 
			"Environment variable EW_LOG not defined; Exitting\n" );
           exit( -1 );
   	   }
	strcpy(logNam,logPtr);
	printf("logNam=%s\n",logNam);
#else
	strcpy(logNam,logPath);				/* set the path */
	i=strlen(logNam);
	if(logNam[i-1] != '/')				/* if path does not end in '/' */
	{	 logNam[i]='/';					/* then add one */
		logNam[i+1]=0;
	}
#endif
	strcat(logNam,tag);		/* set the name */
	strcat(logNam,LOG_FYL_EXT);			/* add extension */
	logout=fopen(logNam,"a+");		 
	if(logout == NULL) 
	   {
	   fprintf(stderr,"Unable to open %s\n",logNam);
	   exit(115); 
	   }
	fprintf(logout,"fopen file : %s\n",logNam);	 
 
#endif

/*	WARNING : no output to logout should be done before now! */
 	cmdarg(argc,argv,ttdev);	/* adjust for any command line switches */

	if(capture) 
	{	strcpy(scratch,logPath);
		strcat(scratch,"capture.dat");
		capout=open(scratch, (O_WRONLY | O_TRUNC | O_CREAT));
		if(capout == -1) fprintf(logout, "capture file open err=%d\n",errno);
		fprintf(logout,"Capture file name is : %s\n",scratch);
	}
#ifdef SUNOS
	on_exit(exit_handler,-1);
	fprintf(logout,"SUNOS on_exit set ...\n");
#endif
#ifdef _SOLARIS
	fprintf(logout,"SOLARIS atexit set...\n");
	atexit(exit_handler);
#endif
	fprintf(logout,"cmdflg=%d\n",cmd_flag);
	if(cmd_flag) {
#ifdef SUNOS
/*		err=ioctl(0,FIONBIO, &true);    /* set non blocking i/o */
		if(err < 0) fprintf(logout,"ioctl err=%d errno=%d\n",err,errno);
#endif
		fprintf(logout,"Set up for Terminal commands\n"); fflush(logout);
		signal(SIGTSTP,cc_handler);
		signal(SIGTTOU,cc_handler);
		signal(SIGTTIN,cc_handler);
	} else {        /* Ignore the terminal stop signals */
		signal(SIGTTOU, SIG_IGN);
		signal(SIGTTIN, SIG_IGN);
		signal(SIGTSTP, SIG_IGN);

#ifndef TERMINAL_LOG
		for (fd = 2; fd >= 0; --fd) {		/* close stdin stdout stderr */
			close(fd);
		}
		open("/dev/null", O_RDONLY);		/* open STDIN as rdonly to null */
		open("/dev/null", O_WRONLY);		/* open stdout as wronly */
#endif
/*		dup2(0, 1);*/
/*		dup2(logout, 2);					/* set stderr to logout */
#ifdef SUNOS
		if ((fd = open("/dev/tty", O_RDWR)) >= 0) {/* we need a controlling term*/
/*			ioctl(fd, TIOCNOTTY, 0); 		/* so we can disassociated it */ 
			close(fd);
		}
#endif
	}
	fprintf(logout,"Feedme init ttpath=%d\n",ttpath);
    fflush(logout);                     
	if(feedme_init(argc,argv)) exit(4);/*initialize host system */
	fprintf(logout,"tcpcomm=%d ttpath=%d\n",tcp_comm,ttpath);
	fflush(logout);
	meminit=0;						/* indicate we have not yet alloc memory*/
	if(tcp_comm) init_tcp();		/* if TCP is communications, start it */
	else init_pass(ttdev);			/* open tone task and console serial lines*/
/*
	TRAP every conceivable signal so that the feedme_shutdown 
	routine can be called
*/
	fprintf(logout,"call signal\n");
	fflush(logout);
	signal(SIGINT,cc_handler);
	signal(SIGHUP,cc_handler);
	signal(SIGQUIT,cc_handler);
	signal(SIGILL,cc_handler);
	signal(SIGTRAP,cc_handler);
	signal(SIGABRT,cc_handler);
	signal(SIGEMT,cc_handler);
	signal(SIGFPE,cc_handler);
	signal(SIGKILL,cc_handler);
	signal(SIGBUS,cc_handler);
	signal(SIGSEGV,cc_handler);
	signal(SIGSYS,cc_handler);
	signal(SIGTERM,cc_handler);
	signal(SIGSTOP,cc_handler);	
	signal(SIGCONT,cc_handler);
	signal(SIGTSTP,cc_handler);
	signal(SIGCHLD,cc_handler);
	signal(SIGXCPU,cc_handler);
	signal(SIGXFSZ,cc_handler);
	signal(SIGWINCH,cc_handler);
	fprintf(logout,"start top compiled=%s %s\n",
		date_compiled,logNam); 
	fflush(logout);

/**********************************************************************
	T o p   o f   R e s t a r t   L o o p
***********************************************************************	*/
  for (;;) {					/* if we exit inside infinite loop. restartFILE*/
	first=0;					/* indicate first trip through loop */
 	sequence=0;					/* initialize sequence of output buffers */
	rollcount=5;				/* after restart always force roll back inhib*/
	fprintf(logout,"initmemop V1.0\n"); fflush(logout);
	if(meminit==0) initmemory();	/* allocate buffer space */
	tonecount=0;				/* count time intervals for LP partial update*/
	for(j=0; j<MAX_CH; j++) {
		ch[j].warmup=10;
		ch[j].triggered=0;
	}

	/* An infinite loop starts here */
   for (;;) {					/* another infinite loop */
	if(cmd_flag) console();		/* yes, call command input handler */
	nsamp=0;
	while( nsamp <= 0) {			/* if zero returned go around again */
#ifdef DEBUG_PRINT
		fprintf(logout,"fdme "); fflush(logout);
#endif
		if(tstdata != 0) nsamp=feedme_test(&ich,ia,&tc);
		else nsamp=feedme(&ich,ia,&tc);
#ifdef DEBUG_PRINT
		fprintf(logout,"out "); fflush(logout);
#endif
		if(ich > MAX_CH) 
		{	i4swap(ich,&j);			/* is it a byte reversed problem EW! */
			if(j <= MAX_CH)
			{	if(j == 1) fprintf(logout,"***** Byte reversed chan %d\n",ich);
				ich=j;
			} else
			{	fprintf(logout,"FEEDME illegal channel # = %d max=%d\n",
					ich,MAX_CH);
				nsamp = 0;
			}
		}
		if(capture > 0 && nsamp > 1) {
			write(capout,&nsamp,4);
			write(capout,&ich,4);
			write(capout,&tc, 8);
			write(capout,ia,nsamp*4);
			capture--;
			fprintf(logout,"%d %d %d\n",capture,nsamp,ich);
			if(capture <= 0) {
				fprintf(logout,"capturing expired\n");
				close(capout);
				capout = -1;
			}
		}
	}
	if(nsamp > MAX_NSAMP_IN) {
		fprintf(logout,
			"Nsamp too large for version.Fix MAX_NSAMP_IN in vdl.h %d %d\n",
			nsamp,MAX_NSAMP_IN);
		exit(111);			/* tell mother its bad */
	}
	if(ich == dumpchan && nsamp > 0) {
		nsnint(tc,&iy,&id,&ih,&im,&is,&ms,&leap);/* convert time to ints*/
		fprintf(logout,"Log ns=%d %d %d:%2d:%2d:%2d.%3d\n",
				nsamp,iy,id,ih,im,is,ms);
		for(i=0; i<nsamp; i++) if( (i%10) == 9) fprintf(logout,"%8d\n",ia[i]);
			else fprintf(logout,"%8d",ia[i]);
		fprintf(logout,"\n");
	}


	/***********************
	Decimate to right rate 
	**************************/
	if(ch[ich].decimate > 1)
	{	if(first != 0) tc = spfilt(&ch[ich], ia, &nsamp, tc);/* no filt if 1st*/
	}
	if(ich == dumpchan && nsamp > 0) {
		fprintf(logout,"Log ns=%d %d %d:%2d:%2d:%2d.%3d\n",
			nsamp,iy,id,ih,im,is,ms);
		for(i=0; i<nsamp; i++) if( (i%10) == 9) fprintf(logout,"%8d\n",ia[i]);
			else fprintf(logout,"%8d",ia[i]);
		fprintf(logout,"\n");
	}

#ifdef DEBUG_PRINT
	fprintf(logout," int");
#endif
	nsnint(tc,&iy,&id,&ih,&im,&is,&ms,&leap);/* convert time to separte ints*/
	if( (im % 10) == 0 && lastim != im) {
		lastim=im;
		fprintf(logout,"%4d %3d-%2d:%2d:%2d\n",iy,id,ih,im,is);
	}
#ifdef DEBUG_PRINT
	fprintf(logout,"%4d %3d-%2d:%2d:%2d.%3d ich=%d nsamp=%d ia=%d %d %d",
		iy,id,ih,im,is,ms,ich,nsamp,ia[0],ia[1],ia[nsamp-1]);
#endif
	if( (ih == 23 && im > 58) || (ih == 0 && im < 2)) {
		dbglp=0;			/* to debug change this to 1 */
		if(dbglp) fprintf(logout,"dbglp=%d ih=%d im=%d der_lp=%d\n",
				dbglp,ih,im,derive_lp);
	} else dbglp=0;
/***********************************
	if first time, do this . 
	1) GBLDOY must be set to current DOY. This will end
	up in the header of the outbound packets.  It should change at midnight to
	the next day.  This will cause the continuous channels to close up and
	start a new detection on the new day( set detect_seq to 1 a midnight). 
	2) To prevent duplication of detection sequences when programs are restarted
	we always initialize detection sequence to the number of seconds since
	midnight/3. 
	3) Continuous channels are found and started.  This includes setting the
	time of the first sample, assigning a detect_seq, setting chan_seq=1, etc.
**************************************/
	if(first == 0 ){
		first=1;						/* one time only!!! */
		gbldoy=id;						/* must set before call to init_trig*/
		time(&clock);					/* local time to set init detect_seq */
		tm2=gmtime(&clock);
		detect_seq=(ih*3600+im*60+is)/3;/* correct way*/
		detect_seq=(tm2->tm_hour*3600+tm2->tm_min*60+tm2->tm_sec)/3;/*test way*/
		fprintf(lp,"LP =%d\n",lp->_file);
		if(logout == NULL) {
			fprintf(logout,"Log open failed lp=%d errno=%d\n",lp,errno);
			exit(136);
		}
  		if(lp == stdout) lp=logout;				/* if LP not opened in cmdarg */
		fprintf(logout,"Logout=%d ttpath=%d\n",logout->_file,ttpath);
		fprintf(logout,"########### START2 %s %s ###########\n",
			logNam,date_compiled);
		fprintf(logout,"%s First time init_trig detseq=%d doy=%d\n",
				tag,detect_seq,gbldoy);
		init_trig();					/* initialize trigger parameters */
		fprintf(logout,"%s End of INIT trg\n",tag);
	}									/* Init trig (after time set always)*/

/**************************************************
	1) ia contains nsamp samples. tc is the time of the first sample
	2) Triggered tells us the state of the trigger, 
			0=none, 1=New trigger started, 2=Trigger is in progress.
	3) Trigflg is true if current IA buffer is sufficient to cause a trigger
***********************************************/
	tc2=nsnadd(tc,nsamp*1000/ch[ich].nsamp_qd);/* time of last sample */
	if(nsamp > 3*ch[ich].max/4) fprintf(logout,
		" ***** big buffer in ns=%d max=%d\n",nsamp,ch[ich].max);
#ifdef DEBUG_PRINT
	fprintf(logout," Proc");
#endif
	if( (tc2.doy != tc.doy) && continuous) {	/* day change during sample? */
		fprintf(lp,"%s ich=%d Day spanning packet \n",tag,ich);
		for(i=0; i<nsamp; i++) {		/* process data one sample at a time */
			tc2=nsnadd(tc,i*1000/ch[ich].nsamp_qd);/* time of ith sample */
			proc(&ia[i],ich,1,tc2);		/* process one sample */
		}
	} else proc(ia,ich,nsamp,tc);		/* Process the data */
	if(ich == 0) tonecount+=nsamp;	/* count so we know when 2 minutes up*/
#ifdef DEBUG_PRINT
	fprintf(logout," Pdone");
#endif
	/* if needed derive LP data */
	if(ch[ich].derive_lp && ch[ich].tc.ms != -1) {/* is this a derive LP chan?*/
		idiff=ch[ich].ipnt-ch[ich].lppnt;/* how many BB samples are in */
		if(idiff < 0) idiff+=ch[ich].max;/* correct if lapping end of buffer */
#ifdef DEBUG_PRINT
		fprintf(logout," LPDEV");
#endif
		lpch=ch[ich].derive_lp;				/* LP channel number */
		if(dbglp) fprintf(logout,"LP derive idiff=%d ich=%d lch=%d ms=%d\n",
			idiff,ich,lpch, ch[lpch].tc.ms);
		while(idiff > ch[ich].freq) {			/* for each L can derive */
			if(ch[lpch].tc.ms == -1) tctmp = nsnadd(tc, 1000);/* 1st sample */
			else tctmp=buftim(&ch[ich],ch[ich].lppnt+ch[ich].freq);/* time of point */
			lpp=lpfilt(&ch[ich]);		/* Make an LP point */
			nsnint(tctmp,&iy,&id,&ih,&im,&is,&ms,&leap); 
			if(dbglp) 
			fprintf(logout,
		"Idiff=%d lppnt=%d cont=%d LP=%d  %4d %d-%2d:%2d:%2d.%3d %d lpch=%d\n",
				idiff,ch[ich].lppnt,ch[lpch].continuous,lpp,
				iy,id,ih,im,is,ms,ich,lpch);
			proc(&lpp,lpch,1,tctmp);	/* process the derived LP data */
			idiff-=ch[ich].freq;				/* reduce # of samp left to decimate */
		}
#ifdef DEBUG_PRINT
	fprintf(logout," LDONE");
#endif
	}
	if(tonecount > SPFREQ*150) {		/* time for partials on LP */
#ifdef DEBUG_PRINT
	fprintf(logout," PRTL ");
#endif
		for (i=0; i<= MAX_CH; i++) {	/* look at each channel */
			if(ch[i].continuous != 0 && ch[i].partial != 0) {/* is it a cont partial*/
				cmpch=&ch[i];			/* set cmpch so PUTBUF knows who called*/
#ifdef DEBUG_PRINT
				fprintf(logout," PRTL %d %s %d &d ",
					i,ch[i].txt,ch[i].cmp,&ch[i].memex);
#endif
				prtial(&ch[i].memex,(unsigned char *) ch[i].cmp);/* partial update */
#ifdef DEBUG_PRINT
				fprintf(logout," PRTLb");
#endif
				status_count++;
			}
		}
		if( (status_count % 100) == 5) {
			fprintf(logout,"status_count=%d\n",status_count);
			print_status();
		}
		tonecount=0;					/* Start another count */
	}
	if(restart == 1) break;				/* restart exits the infinite loop */
	dbgtrg++;
	if( (dbgtrg  % 1000) == 0) trigforce=1;
#ifdef DEBUG_PRINT
	fprintf(logout,"#\n");
#endif
	fflush(logout); fflush(lp);
   }			/* End of the infinite FOR loop */
   fprintf(logout,"FRONT restart request...\n");
  }			/* end of infinite for restarting a station*/
}			/* The ultimate End */
void console()		/* dummy console */
{
	return;
}
#ifdef __STDC__
  void cmdarg(int argc, char *argv[],char *ttdev)
#else
  void cmdarg(argc,argv,ttdev)
  int argc;
  char *argv[];
  char *ttdev;
#endif
/*********************** CMDARG *********************************
*
*	calling Sequence :	cmdarg(argc,argv,ttdev)	
*	argc	int with number of arguments parsed by shell
*	*argv[]	pointer to array of string pointers with text strings parsed by shell
*	ttdev	string to return device name if one is present
*
*	Programmed by : D.C. Ketchum
*	Created :		May 1995
*	Date Modified Description of Mod
*	22-May-95		Base Functionality
**********************************************************************/
{
	extern int FFTLEN,FFTL2;		/* the FFT parameters */
	extern int SPFREQ;				/* BB frequency (decimated from digit_freq)*/
	extern int digit_freq;			/* the native digitizing frequency */
	extern double freq_calc;		/* nominal digit value as a decimal */
	extern int decim_factor;			/* the decimation factor between the above */
	extern int net,node;			/* the network ID and node ID */
	extern int tcp_comm;			/* flag for communications type */
	extern int fcalc_flag;			/* if true, print calc of frequency */
	extern int derive_lp;			/* Flag controlling derivation of LP from BB*/
	extern int continuous;			/* flag on continuous BB stations */
	extern int continuous_phoenix;	/* flag on continuous BB phoenix elim */
	extern int feedpipe;			/* unit of input pipe */
	extern int cmd_flag;
	extern int detnoisy;			/* make trigger output more voluminous */
	extern int showdet;				/* if true, detailed trigger info */
	extern int dumpchan;			/* set if dump on channel needed */
	extern int heligain;
	extern int triggered_input;
	extern int port;				/* socket port number */
	extern char outaddr[];			/* socket name */
	int i,j,k,err,stop;
	float f;
	char *s;
	FILE *in;
	char line[120],scratch[120];
	strcpy(param_file,"None");
	for (i=1; i<argc; i++) 	{	/* for each command line argument */
		fprintf(logout,"%d #%s# ",i,argv[i]);
		if(strcmp(argv[i],"-a") == 0)
		{	heligain=atoi(argv[i+1]);
			fprintf(logout,"Heligain=%d\n",heligain);
		}
		if(strcmp(argv[i],"-b") == 0) {
			bb_delay=atoi(argv[i+1]);		/* Set the BB group delay */
			fprintf(logout,"BB Delay=%d ms. ",bb_delay);
		}
		if(strcmp(argv[i],"-cph") == 0) {
			continuous_phoenix = atoi(argv[i+1]);
			fprintf(logout,"Continuous_phoenix=%d\n",continuous_phoenix);
			continuous=continuous_phoenix;
		}
		if(strcmp(argv[i],"-c") == 0) {
			continuous=atoi(argv[i+1]);		/* Set the continuous flag */
			fprintf(logout,"Continuous=%d ",continuous);
		}
		if(strcmp(argv[i],"-capture") == 0) {
			capture = atoi(argv[i+1]);
			if(capture < 100) capture = 20000;
			fprintf(logout,"Capturing is on %d records ...\n",capture);
		}
		if(strcmp(argv[i],"-d") == 0) {
			strcpy(ttdev,argv[i+1]);		/* set path of serial line */

			fprintf(logout,"ttdev=%s ",ttdev);
		}
		if(strcmp(argv[i],"-f") == 0) {	/* Set digitizing rate */
			SPFREQ=atoi(argv[i+1]);		/* set digit rate */
			fprintf(logout," Freq=%d ",SPFREQ);
		}
		if(strcmp(argv[i],"-F") == 0) {	/* Set digitizing rate */
			f=atof(argv[i+1]);		/* set digit rate */
			freq_calc=f;
			if(f > 1000.) {
				fcalc_flag=1;
				freq_calc-=1000.;
			}
			fprintf(logout," Freq=%d ",freq_calc);
		}
		if(strcmp(argv[i],"-host") == 0) {
			strcpy(outaddr,argv[i+1]);
			fprintf(logout," Host addr=%s ",outaddr);
		}
		if(strcmp(argv[i],"-h") == 0) {
			lp_delay=atoi(argv[i+1]);		/* set the LP group delay in MS */
			fprintf(logout,"LP Delay=%d ms. ",lp_delay);
		}
		if(strcmp(argv[i],"-i") == 0) {		/* set the station tag */
			strcpy(tag,argv[i+1]);		
			fprintf(logout,"Set station name=%s ",tag);
		}
		if(strcmp(argv[i],"-j") == 0) {
			digit_freq=atoi(argv[i+1]);
			fprintf(logout,"Native Freq=%d\n",digit_freq);
		}
		if(strcmp(argv[i],"-k") == 0) {		/* set the pipe to get data from */
			cmd_flag=atoi(argv[i+1]);		
			fprintf(logout,"Cmds Enabled=%d ",cmd_flag);
		}
		if(strcmp(argv[i],"-l") == 0) {
			derive_lp=atoi(argv[i+1]);		/* set the derive LP flag */
			fprintf(logout,"Drv LP=%d ",derive_lp);	
		}
		if(strcmp(argv[i],"-n") == 0) {
			net=atoi(argv[i+1]);			/* set the network ID for station */
			fprintf(logout,"Net=%d ",net);
		}
		if(strcmp(argv[i],"-port") == 0) {
			port=atoi(argv[i+1]);
			fprintf(logout," port=%d ",port);
		}
		if(strcmp(argv[i],"-q") == 0) {
			short_period=atoi(argv[i+1]);	/* Set it as an SH rather than BH */
			fprintf(logout,"Short Period=%d ",short_period);
		}
		if(strcmp(argv[i],"-s") == 0) {
			node=atoi(argv[i+1]);			/* Set the node or station number */
			fprintf(logout,"Node=%d ",node);
		}
		if(strcmp(argv[i],"-t") == 0) {	
			tcp_comm=atoi(argv[i+1]);		/* set the TCP/ Serial comm flag */
			fprintf(logout,"TCP=%d ",tcp_comm);
		}
		if(strcmp(argv[i],"-T") == 0) {	
			triggered_input=atoi(argv[i+1]);		/* triggered input flag */
			fprintf(logout,"triggered_input=%d ",triggered_input);
		}
		if(strcmp(argv[i],"-v") == 0) {	

			strcpy(param_file,argv[i+1]);
			fprintf(logout,"MAX_CH=%d ",MAX_CH);
			phmode=1;
		}
		if(strcmp(argv[i],"-x") == 0) {		/* set the exponent bias */
			expbias=atoi(argv[i+1]);	
			fprintf(logout,"Set exp bias=%d ",expbias);
		}
		if(strcmp(argv[i],"-z") == 0) {		/* set the pipe to get data from */
			feedpipe=atoi(argv[i+1]);		
			fprintf(logout,"Feed Pipe=%d ",feedpipe);
		}
		if(strcmp(argv[i],"-!") == 0) {
			detnoisy=atoi(argv[i+1]);		/* Set Flag for voluminous trig output */
			fprintf(logout,"Noisy=%d ",detnoisy);
		}
		if(strcmp(argv[i],"-]") == 0 ) {	/* is the packet debug on */
			dbgpkt=1;
			fprintf(logout,"Pck dbg on ");
		}
		if(strcmp(argv[i],"-+") == 0 ) {	/* is the showdet debug on */
			showdet=1;
			fprintf(logout,"Showdet on");
		}
		if(strcmp(argv[i],"-[") == 0) {		/* dump channel debugging */
			dumpchan=atoi(argv[i+1]);
			fprintf(logout,"dumpchan=%d",dumpchan);
		}
		if(*argv[i] == '>') {
			lp=fopen(argv[i]+1,"w");
			if(lp == NULL) exit(118);
			fprintf(lp,"%s done ",argv[i]);
			fprintf(lp,"Standard out and err redirected to %s\n",argv[i]);
		}
	}
/*
	Validate the arguments.  Compute decimation factor if one is needed 
*/
	fprintf(logout,"\nValidate Parameters digit=%d SPFREQ=%d\n",
			digit_freq,SPFREQ);
	fflush(logout);
	decim_factor=1;
	if(digit_freq == 0) digit_freq=SPFREQ;	/* if not update, assume same */
	if(digit_freq != SPFREQ) {			/* need to decimate data, figure it */
		j=digit_freq;
		while (j > 50) {
			j=j/2;						/* have the rate each stage */
			decim_factor*=2;
		}
		SPFREQ=j;						/* the actual frequency */
		fprintf(logout,"digit_freq=%d SPFREQ=%d decim_factor=%d\n",
			digit_freq,SPFREQ,decim_factor); fflush(logout);
	}
	switch (SPFREQ) {
		case 20 :
			FFTLEN=256;
			break;
		case 40 :
		case 50 :
			FFTLEN=512;
			break;
		case 100 :
			FFTLEN=1024;
			break;
		default :
			fprintf(logout,"Illegal Digitizing Freq = %d digit_freq=%d decim=%d\n",
				SPFREQ,digit_freq,decim_factor);
			fflush(logout);
			exit(11);
	}
	FFTL2=FFTLEN/2;

	if(phmode) {			/* read in configuration file */
		stop=0;
		j=0;
		fprintf(logout,"Scan %s for # of chans.\n",param_file);
		fflush(logout);
		in=fopen(param_file,"r");		/* open param file for read */
		if(in == NULL)
			{
			fprintf(logout,"Cannot open %s. Exitting\n",param_file);
			exit(117);
			}
		MAX_CH=0;
		while( ! stop) {
			s=fgets(line,120,in);
			fprintf(logout,"%d line=%s",s,line); fflush(logout);
			if(s == NULL) break;
			sscanf(line,"%s%d",scratch,&j);
			fprintf(logout,"%s j=%d\n",scratch,j);
			if(j > MAX_CH && scratch[0] != '#') MAX_CH=j;
		}
		fclose(in);
		stop=0;
		fprintf(logout,"#chan found=%d\n",MAX_CH); fflush(logout);
		ch=(struct chan_desc *) malloc((MAX_CH+1)*sizeof(struct chan_desc));
		memset(ch,0,(MAX_CH+1)*sizeof(struct chan_desc));
		fprintf(logout,"Malloc for ch=%d MAX_CH=%d\n",ch,MAX_CH);
		in=fopen(param_file,"r");
		if(in == NULL) exit(118);
		while ( !stop) {				/* sure, a while would be better */
			err=fscanf(in,"%s",scratch);
			if(err == EOF) break;
			if(err != 1) break;
			if(scratch[0] == '#') 
				{fgets(line,120,in);fprintf(logout,"\n");continue;}
			fscanf(in,"%d",&j);
			strcpy(ch[j].txt,scratch);
			fprintf(logout,"%d %s ",j,ch[j].txt);
			fscanf(in,"%d",&err);
			ch[j].stat_chan=err;
			fprintf(logout,"stch=%d ",ch[j].stat_chan);
			fscanf(in,"%d",&ch[j].showdet);
			fprintf(logout,"shw=%d ",ch[j].showdet);
			fscanf(in,"%d",&ch[j].sendpows);
			fprintf(logout,"pows=%d \n",ch[j].sendpows);
			fscanf(in,"%d",&ch[j].decimate);
			fprintf(logout,"dc=%d ",ch[j].decimate);
			fscanf(in,"%lf",&ch[j].freq_calc);
			fprintf(logout,"fq=%7.3f ",ch[j].freq_calc);
			ch[j].freq=ch[j].freq_calc+.499999;
			fprintf(logout,"fq=%d ",ch[j].freq);
			fscanf(in,"%d",&ch[j].derive_lp);
			fprintf(logout,"dlp=%d ",ch[j].derive_lp);
			for(k=0; k<3; k++) fscanf(in,"%d",&ch[j].forward[k]);
			fprintf(logout,"fw=%d %d %d ",ch[j].forward[0],ch[j].forward[1],
				ch[j].forward[2]);
			fscanf(in,"%d",&ch[j].delay);
			fprintf(logout,"lpd=%d ",ch[j].delay);
			fscanf(in,"%d",&ch[j].expbias);
			fprintf(logout,"exp=%d ",ch[j].expbias);
			fscanf(in,"%d",&ch[j].ncoin);
			fprintf(logout,"coin=%d ",ch[j].ncoin);
			fscanf(in,"%d",&ch[j].nretired);

			fprintf(logout,"rtd=%d ",ch[j].nretired);
			fscanf(in,"%f",&f);
			ch[j].sntrig=f;
			fprintf(logout,"sn=%4.1f ",ch[j].sntrig);
			fscanf(in,"%f",&f);
			ch[j].hfsntrig=f;
			fprintf(logout,"hsn=%4.1f ",ch[j].hfsntrig);
			fscanf(in,"%d",&ch[j].nfreq);
			fprintf(logout,"nfq=%d ",ch[j].nfreq);
			for(k=0; k<10; k++) {fscanf(in,"%d",&err);ch[j].trigfreq[k]=err;}
			fprintf(logout," tfq=");
			for(k=0; k<10; k++) fprintf(logout,"%d ",ch[j].trigfreq[k]);
			fgets(line,120,in);
			fprintf(logout,"\n");
		}
	}  else {
		MAX_CH=5;
		ch=(struct chan_desc *) malloc((MAX_CH+1)*sizeof(struct chan_desc));
		memset(ch,0,(MAX_CH+1)*sizeof(struct chan_desc));
		fprintf(logout,"Malloc2 for ch=%d MAX_CH=%d\n",ch,MAX_CH);
	}
	if(MAX_CH <=0 || MAX_CH >100) {
		fprintf(logout,"MAX_CH out of range 0-100 =%d\n",MAX_CH);
		exit(13);
	}	
	fprintf(logout,"\n");
	fflush(logout);
	return;
}
