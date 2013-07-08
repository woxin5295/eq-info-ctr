/*	
Global variables used by VDL : 			
*/
FILE *lp;					/* Path to log file */
FILE *logout;
int icnt=0;
int ranflg=0;
double ranmag=100000.,ranfreq=2.;
int dbg=0;					/* Flag set to true if debug output wanted */
int dbgmem=0,dbgmem2=0;		/* flags for debuging memory management */
int dbglp=0;				/* flag to debug LP derived data */
int detnoisy=0;				/* cause detailed detection outout */
int tstdata=0;				/* if not zero, make up the data */
int dumpchan=-1;				/* if positive, dump out this channel */
int station;				/* the station ID from the satellite DPU */
int	node=4;					/* this nodes number */
int restart=0;				/* restart flag */
int net=15;					/* This nodes net id for data */
int meminit=0;				/* When zero, Memory has not been allocated*/
int short_period=0;			/* if SH channels, set to non-zero */
char tag[6]="Unkn";
int cmd_flag=0;				/* when zero, no keyboard commands allowed */
#ifdef _EARTHWORM
char outaddr[30]="nsn4.cr.usgs.gov";
#else
char outaddr[30]="PASS_SOCKET";
#endif
int port=2003;				/* default port # */
/*
	Command related variables
*/
int showdet;				/* Force Detector printouts */
int sequence;				/* Sequence number */
int rollcount=5;			/* insure first 4 packets get rollback inhibits*/
/*
		Time keeping related variables
*/
int leapnext=0;				/* indicate leap second status for tomorrow*/
int leap=0; 					/* I + or - 1 if a leap second day */
int gbldoy;					/* the global doy for secoff and msoff */
int gblyr;					/* the global year for secoff and msoff */
/*
		Terminal related variables 
*/
struct nsntime lasttime;	/* last time with that SOF */
int ttpath;					/* path to send data to */
int feedpipe;				/* path to input pipe */
/*
		Trigger related variables
*/
int leadtime=60;				/* length of time in sec before each trig*/
int posttime=180;				/* length of time after trigger to save*/
int deadtime=1200;				/* After dead time, histories are aged */
int abstime=1800;				/* absolute longest trigger */
struct chan_desc *cmpch;		/* Channel being compressed (set by COMPRESS)*/
short detect_seq=1;				/* detection sequence from this site */
short hfdetect_seq=1;
/*
		Trigger parameters.
*/
int SPFREQ=40;				/* digitizing rate in HZ of BB data from VDL */
int digit_freq=0;			/* the native digizer rate */
int fcalc_flag=0;			/* if true, print freq_calc info */
double freq_calc=0.;		/* nominal digit rate starting value */
int decim_factor=1;				/* decimation rate (digit_freq/SPFREQ) */
int FFTLEN=256;				/* number of point in fft buf default */
int FFTL2=128;				/* Half a FFT length */
int trigfreq[16]={0,1,2,3,4,0,0,0,0,0,0,0,0,0,0,0};	/* the NFREQ window to check in detector */
int continuous=0;			/* is this a continuous broadband */
int continuous_phoenix=0;	/* is this a continuous phoenix style vdl */
int triggered_input=0;		/* if triggered SP data is input, just forward it*/
int trigforce=0;			/* set by console to force triggers */
int expbias=12;				/* default bias */
int derive_lp=0;			/* if true, LP is derived by filtering BB */
int tcp_comm=1;				/* if true, USE TCP communications */
int bb_delay=0;				/* group delay to apply to BB channels */
int lp_delay=0;				/* group delay to apply to LP channels */
int contrgsecs=0;			/* seconds since last continuous trigger declared */
int dbgpkt=0;				/* if true, generate full packet debugging in log */
/*
		Output QUEUE related variables
*/
int MAX_CH=6;				/* maximum number of channels to dimension for */
int MAX_LCH=6;				/* Top of Lowgains, not yet supported */
struct chan_desc *ch;		/* Channel control structure */
/*unsigned */long *cmpbufs;		/* Pointer to compression memory bufs */
/*
	command line stuff
*/
int linepnt;				/* point to next char to get data in buf*/
int linemode;				/* if > 0, in line input mode */
unsigned char cmdchrsave;	/* command character*/
unsigned char linebuf[20];	/* buffer for input characters*/
char param_file[60];		/* place for parameter file if any */
int phmode=0;				/* mode, if != zero using phoenix mode */
int heligain=0;				/* set helicorder gain */
