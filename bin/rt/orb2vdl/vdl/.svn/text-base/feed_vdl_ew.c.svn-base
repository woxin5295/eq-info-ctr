/*
      	Modified 3/12/97 to feed two as well as four byte data. Alex
 *   feed_vdl_ew.c
	routines to hand trace data from an Earthworm ring to VDL.
	Includes the three required routines:
		feedme_init(), feedme(), and feedme_shutdown().
	Much code and inspiration taken from Doug Newhouser's version 
	of these routines.

	Expects standard format trace data messages. Selected pin's are
	picked up from the ring, buffered, and fed to VDL.
	Buffering between the transport ring and feedme() is via memory-
	based link list. It has a configurable maximum size, so as to not
	devour the universe.

	Startup sequence:
	Earthworm is brought up by 'startstop'. Startstop reads a 
	configuration file (startstop.d) containing command lines. 
	The command line for vdl is found there. That command line contains
	the usual vdl command line, plus an Earthworm "-e" switch specifying the 
	parameter file:
		vdl ... -e vdl_params.d ...
	where "vdl_params.d" will be read by feedme_init(), and contains all
	the Earthwormy things, like module id, ring to attach to, station to
	send, and what to call it.
	feedme_init() initiates logging, reads the parameter file, initializes the
	memory fifo buffer of trace messages, and starts the MsgGet thread. This 
	thread picks messages from the transport ring, sees if they're the pin numbers
	to be sent, and if so swaps as required, and pushes them into the memory fifo.

	Shutdown:
	When the message getting thread gets a termination message from the transport ring, it'll
	set a status variable to -1, and exit. feedme() will see the negative status the next
	time it's called, and send the signal SIGQUIT to its own pid. VDL gets the hint, and 
	performs a decent shutdown (we presume). This includes calling feedme_shutdown(),
	which shuts down the Earthwormy stuff.

	Operating System:
	Ath this time, VDL runs on Solaris only. This module is system dependent.
	An earlier attempt to make it system independent made it seem like it was
	more work than justified at this time.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <fcntl.h>
#include <math.h>
#include <time.h>

#include <errno.h>
#include <signal.h>
#include <earthworm.h>
#include <kom.h>
#include <transport.h>
#include <trace_buf.h>
#include <swap.h>
#include "queue_max_size.h" 
#include "vdl.h"
#include "vdlqsub.h"

/* Functions in this source file 
 *******************************/
int   vdl_ew_config  ( char * ); 			/* read the configuration file */
void  vdl_ew_lookup  ( void ); 				/* lookup numeric values for EW names */
void  vdl_ew_status  ( unsigned char, short, char * );	/* Issue status message into transport ring */ 
int   vdl_ew_filter  ( int ); 			/* Decide if a message is for us */
int   feedme_init (int argc, char **argv);		/* called by VDL at startup */
int   feedme (						/* called by VDL to supply data */
		int *ich,		/* channel index (returned).	*/
	 	long ia[],		/* data array (returned).	*/
	  	struct nsntime *tc);	/* USNSN sample time (returned).*/
int   feedme_shutdown ();				/* called by VDL on shutdown request */

/* Thready things
*****************/
#define THREAD_STACK 8192
static unsigned tidGetMsg;            /* message getter (transport to mem fifo) thread id */
int GetMsgStatus=0;	  	      /* 0=> GetMsg thread ok. <0 => dead: shut down VDL */

time_t LastCircularError;			/* at time of the last circular buf ovflow*/

QUEUE OutQueue; 		      /* from queue.h, queue.c; sets up linked list via malloc and free */
thr_ret GetMsg( void * );
                                      /* used to pass messages between main thread and SocketSender thread */


extern int  errno;

static  SHM_INFO  Region;       /* shared memory region to use for i/o    */

#define   MAXLOGO   10
MSG_LOGO  GetLogo[MAXLOGO];     /* array for requesting module,type,instid */
short 	  nLogo;
/* What to send to VDL and what to call it
******************************************/
#define MAXPIN 33
short	  nSend;		/* number of pins to send to VDL */
struct
	{
	int pin;	/* Pin number to be shipped to VDL */
	int vdl_name;   /* What we call it as we hand it to vdl */
	} SendPin[MAXPIN];
 
      
/* Things to read or derive from configuration file
 **************************************************/
static char    RingName[20];        /* name of transport ring for i/o    */
static char    MyModName[20];       /* speak as this module name/id      */
static int     LogSwitch;           /* 0 if no logfile should be written */
static int     HeartBeatInt;        /* seconds between heartbeats        */
static long    MaxMsgSize;          /* max size for input/output msgs    */
static int     MaxMessages;	    /* max messages in output circular buffer       */

/* Things to look up in the earthworm.h tables with getutil.c functions
 **********************************************************************/
static long          RingKey;       /* key of transport ring for i/o     */
static unsigned char InstId;        /* local installation id             */
static unsigned char MyModId;       /* Module Id for this program        */
static unsigned char TypeHeartBeat; 
static unsigned char TypeError;

/* Error messages used by vdl_ew 
 *******************************/
#define  ERR_MISSMSG       0   /* message missed in transport ring       */
#define  ERR_TOOBIG        1   /* retreived msg too large for buffer     */
#define  ERR_NOTRACK       2   /* msg retreived; tracking limit exceeded */
#define  ERR_QUEUE         3   /* error queueing message for sending     */
static char  Text[150];        /* string for log/error messages          */

/* Other variables: 
*******************/
   time_t MyLastBeat;  		/* time of last heartbeat into the local Earthworm ring */
   char         *inMsg;		/* message stuffed by thread             */    
   char         *outMsg;	/* Pulled out by feedme                  */    
   long          recsize;	/* size of retrieved message             */
   MSG_LOGO      reclogo;	/* logo of retrieved message             */
   int           res;
   long* dataPtrDebug;
   int itmp;

/****************** feedme_shutdown ***************************************
	Called by VDL when VDL has decided to quit operations.
	This could be because feedme() has sent it a quit signal.
***************************************************************************/
int feedme_shutdown()
{
   tport_detach( &Region ); /* let go of transport ring */
   (void) KillThread(tidGetMsg); /* try to shut down the message getting thread */
   logit("et","vdl_ew: Shutdown - detaching from Earthworm (feedme_shutdown)\n");
   return(0);
}

/************************* feedme_init *************************************
	Called by VDL at start of operations. We read our config
	file, connect to transport, start the memory queue, and
	start the message gathering thread.
****************************************************************************/
int feedme_init (int argc, char **argv)
{
    int i;
    int	 ret;
    int		c;
    char* param_file; 

   logit_init( "vdl_ew", (short) 23, 256, 1 ); /* debug */
   (void)BigOrLittle();
    /* Parse command line arguments.					*/
    /* Ignore arguments that we do not need.				*/
    /* Assume ALL cmdline arguments are options followed by single arg.	*/
    for (i=1; i<argc; i+=2) 
	{
	if (argv[i][0] != '-') 
		{
	 	logit ("", "Error - invalid option: %s\n", argv[i]);
	  	exit(1);
		}
	c = argv[i][1];
	switch (c) 
		{
	   case 'e':
	    	param_file = argv[i+1]; 
			logit("","VDL_EW:  paramfile1=%s\n",argv[i]);
	    	break;
	   default:
	    	break;
		}
	}

   /* Set initial GetMsg thread status to running
    *********************************************/
   GetMsgStatus= 0;

   /* Initialize heartbeat timer
    ****************************/
   MyLastBeat=0; /* assure a heartbeat on first execution of feedme() */
      
   /* Read the configuration file(s)
   ********************************/
   ret=vdl_ew_config( param_file );
   if(ret<0) return(-1); /* something went wrong. Prepare to abort */	

   /* Look up important info from earthworm.h tables
   ************************************************/
   vdl_ew_lookup();   
 
   /* Initialize name of log-file & open it 
   ***************************************/
   /* logit_init( "vdl_ew", (short) MyModId, 256, LogSwitch ); */
   logit( "" , "vdl_ew: This is VDL running as an Earthworm module. Read command file <%s>\n", param_file);

   /* Write reassuring  things to log file
    **************************************/
   logit("","Established fifo of %ld messages, max %ld bytes per message\n",MaxMessages,MaxMsgSize);
   logit("","Sending Pin as  VDL name \n");
   for(i=0;i<nSend;i++)
	logit("","    %d           %d\n",SendPin[i].pin,SendPin[i].vdl_name);

     /* Allocate space for  messages
   *********************************/
   if ( ( inMsg = (char *) malloc(MaxMsgSize) ) == (char *) NULL ) 
      {
      logit( "e", "vdl_ew: error allocating inMsg; exitting!\n" );
      return( -1 );
      }

   if ( ( outMsg = (char *) malloc(MaxMsgSize) ) == (char *) NULL ) 
      {
      logit( "e", "vdl_ew: error allocating outMsg; exitting!\n" );
      return( -1 );
      }

   /* Create a Mutex to control access to queue
   ********************************************/
   CreateMutex_ew();

   /* Initialize the message queue
   *******************************/
   initqueue ( &OutQueue, (unsigned long)MaxMessages,(unsigned long)MaxMsgSize);	   	

   /* Attach to Input/Output shared memory ring 
   *******************************************/
   tport_attach( &Region, RingKey );

   /* Start the GetMsg thread
   **************************/
   /* moves messages from transport to memory queue */
   if ( StartThread( GetMsg, THREAD_STACK, &tidGetMsg ) == -1 ) 
      {
      GetMsgStatus= -1; /* => not running */
      logit( "e", "vdl_ew: Error starting GetMsg thread. Exitting.\n" );
      tport_detach( &Region );
      return( -1 );
      }
    logit("","Message getting started. feedme_init() completed ok.\n");

   /* End of feedme_init
    ********************/
   return(0);
}
/****************************** end of feedme_init ****************************************************/

/***************************** feedme() *************************************
	Called by VDL to supply a chunck of trace data.
	Takes trace data messages from the memory queue.
	Messages are placed on this queue by the GetMsg
	tread.
	We also sense if that thread has quit, and send 
	a signal to VDL to quit.
***************************************************************************/

int feedme(int *ich,			/* channel index (returned).	*/
	    long ia[],			/* data array (returned).	*/
	    struct nsntime *tc)		/* USNSN sample time (returned).*/
{
	long *p;					/* point used in decimating data */
   int ret;
   int i,j;
	int ms;
   time_t now;
   static long msgSize;
   char timeBuf[50];		/* for creating ascii time for debug */
	extern FILE *logout;

      int temp;
      double dtemp;
      long lHdrSec;
      struct tm hdr; /* unix integer form time structure */
      /* recall that tc is a pointer to the usnsn time structure */ 

   /* Top of Working loop 
    *********************/
   topOfLoop:

   /* Beat the heart into the transport ring
    ****************************************/
   time(&now);
   if (difftime(now,MyLastBeat) > (double)HeartBeatInt)
	{	
        vdl_ew_status( TypeHeartBeat, 0, "" );
		time(&MyLastBeat);
    }

   /* Has Earthworm requested a shutdown?
   **************************************/
   if (GetMsgStatus <0 ) /* then the message getter has quit, and so should vdl */
	{
	/* send a quit signal to ourselves (VDL). VDL mainline code will
	 catch this, and perform a shutdown */
	ret=sigsend(P_PID,getpid(),SIGQUIT); /* warning: the P_MYID option did not seem to work */
	logit("et","vdl_ew: feedme() sent quit signal; ret: %d\n",ret);
	sleep_ew(500); /* problem was that we'd send MANY quit siganls before
			  vdl got the hint. */	
	return(-1);
	}

   /* Pull a message from the queue
    *******************************/
   RequestMutex();
   ret=dequeue( &OutQueue, outMsg, &msgSize, &reclogo);
   ReleaseMutex_ew();
   if(ret < 0 )
      { /* -1 means empty queue */
      sleep_ew(100); 		/*wait a bit DCK was 1000 */
      goto topOfLoop;
      }

   /* Decide on the channel designator
    **********************************/
   for (i=0; i<nSend; i++)
	{
	if ( ((TRACE_HEADER*)outMsg)->pinno == SendPin[i].pin )
	   {
	   *ich=SendPin[i].vdl_name;
	   goto got_name;
	   }
	}
	/* If we dropped out of loop, its a bad pin number */
	logit("et","vdl_ew: unkown pin number %d from feedme(). Requesting exit\n", ((TRACE_HEADER*)outMsg)->pinno);
	ret=sigsend(P_PID,getpid(),SIGQUIT); 
	sleep_ew(500); 
	return(-1);
	got_name: /* found a name for it */
/*
	Create NSN time code using MAKNSN from Unix time in seconds since 1970
*/
	lHdrSec=(long)floor( ((TRACE_HEADER*)outMsg)->starttime  );
	(void)gmtime_r( (time_t*) &lHdrSec, &hdr); 
	ms =  (int)( ( ((TRACE_HEADER*)outMsg)->starttime+.00001 - (double)lHdrSec ) * 1000.0 ) ; 
/*	fprintf(logout,"time= %4d %3d-%2d:%2d:%2d.%3d %d/%d/%d ich=%d tm=%d\n",
		hdr.tm_year,hdr.tm_yday,hdr.tm_hour,hdr.tm_min,hdr.tm_sec,ms,
		1hdr.tm_mday,hdr.tm_mon,hdr.tm_year,*ich,lHdrSec);*/
    *tc=maknsn(hdr.tm_year,hdr.tm_yday+1,hdr.tm_hour,hdr.tm_min,hdr.tm_sec,
		ms,0);
				
   /* Hand it the data 
    ******************/
      {
      /* story: here we unpack the EW message into 4-byte integerss suitable for digestion by vdl */
      int datLen=0;	/* bytes per data point */
      int j;		/* index over data points in message */
      char* next;	/* pointer to next data value (either 2 or 4 bytes long) */

      next = outMsg+sizeof(TRACE_HEADER);      /* point to start of data */
      if( strcmp( ((TRACE_HEADER*)outMsg)->datatype, "s4")==0 ) datLen=4;
      if( strcmp( ((TRACE_HEADER*)outMsg)->datatype, "s2")==0 ) datLen=2;
      if( strcmp( ((TRACE_HEADER*)outMsg)->datatype, "i4")==0 ) datLen=4;
      if( strcmp( ((TRACE_HEADER*)outMsg)->datatype, "i2")==0 ) datLen=2;
      if( strcmp( ((TRACE_HEADER*)outMsg)->datatype, "I2")==0 ) datLen=2;
      if(datLen ==0)
	{
	logit("","feedme dequeued illegal datatype: %s. Initiating shut-down\n",((TRACE_HEADER*)outMsg)->datatype);
	ret=sigsend(P_PID,getpid(),SIGQUIT);   /* send a quit signal to ourselves */
	sleep_ew(500);                         /* problem was that we'd send MANY quit siganls before vdl got the hint. */	
	return(-1);
	}
      for(j=0; j<(int)(((TRACE_HEADER*)outMsg)->nsamp); j++)
         {
         if (datLen == 2) ia[j] = (long) *((short*)next);
         if (datLen == 4) ia[j] = (long) *(( long*)next);
         next = next + datLen;
         }
      /*memcpy(ia, (long *)( outMsg+sizeof(TRACE_HEADER)), ((int)((TRACE_HEADER*)outMsg)->nsamp)*sizeof(long));*/
      /*logit("","feedme pin: %d; header time: %f\n", ((TRACE_HEADER*)outMsg)->pinno, ((TRACE_HEADER*)outMsg)->starttime ); */
      /*logit("","vdlName: %d; nsamp: %d Data: %ld %ld %ld %ld %ld %ld\n\n", *ich,(int)((TRACE_HEADER*)outMsg)->nsamp,
		 ia[0],ia[1],ia[2],ia[3],ia[4],ia[5]); */  
      }

   return( (int)((TRACE_HEADER*)outMsg)->nsamp);  
}



/***************************** GetMsg Thread *******************
	Pick up messages of our kind from the transport ring
	and push them into the memory fifo. feedme() will pull
	them out and hand them to VDL
 ******************************************************************/
thr_ret GetMsg( void *dummy )
{
   int ret;
	time_t tm;

   /* declare ourselves in operation 
    ********************************/
   GetMsgStatus =0; /* show optimism at initially */
  
   while( tport_getflag( &Region ) != TERMINATE )
      {
      /* Get a message from transport ring
      ***********************************/
      res = tport_getmsg( &Region, GetLogo, nLogo, &reclogo, &recsize, inMsg, MaxMsgSize-1 );
	/* debug if(res==GET_OK)logit("et","Got message from transport of %ld bytes, res=%d\n",recsize,res);*/

      if( res == GET_NONE ) {sleep_ew(40); continue;} /*DCK was 100 - wait if no messages for us */

      /* Check return code; report errors 
      **********************************/
      if( res != GET_OK )
         {
         if( res==GET_TOOBIG ) 
            {
            sprintf( Text, "msg[%ld] i%d m%d t%d too long for target",
                            recsize, (int) reclogo.instid,
			    (int) reclogo.mod, (int)reclogo.type );
            vdl_ew_status( TypeError, ERR_TOOBIG, Text );
            continue;
            }
         else if( res==GET_MISS ) 
            {
            sprintf( Text, "missed msg(s) i%d m%d t%d in %s",(int) reclogo.instid,
			    (int) reclogo.mod, (int)reclogo.type, RingName );
            vdl_ew_status( TypeError, ERR_MISSMSG, Text );
            }
         else if( res==GET_NOTRACK ) 
           {
            sprintf( Text, "no tracking for logo i%d m%d t%d in %s",
                          (int) reclogo.instid, (int) reclogo.mod,
                          (int)reclogo.type, RingName );
            vdl_ew_status( TypeError, ERR_NOTRACK, Text );
            }
         }

      /* Process retrieved msg (res==GET_OK,GET_MISS,GET_NOTRACK) 
      **********************************************************/
      /* First, extract a readable pin number
      **************************************/
      itmp=((TRACE_HEADER*)inMsg)->pinno;
      if( strcmp( ((TRACE_HEADER*)inMsg)->datatype, "i4")==0 || 
	  strcmp( ((TRACE_HEADER*)inMsg)->datatype, "i2")==0 )
	{
#ifdef _SPARC
	/* It's an Intel message, and we're a Sun machine */
     	SwapInt( &itmp );
#endif
	}
      else if ( strcmp( ((TRACE_HEADER*)inMsg)->datatype, "s4")==0 || 
		strcmp( ((TRACE_HEADER*)inMsg)->datatype, "s2")==0 )
	{
#ifdef _INTEL
	/* It's an Sun message, and we're an Intel ( DEC? Serbocroatian?) machine */
     	SwapInt( &itmp );
#endif
	}
      else
	{
	logit("et"," VDL_ew: unknown trace data type: %s. GetMsg thread requesting exit.\n",
							      ((TRACE_HEADER*)inMsg)->datatype);
   	GetMsgStatus= -1;  /* so that feedme() will signal to VDL to quit */
   	(void)KillSelfThread( );  /* this terminates us (the thread), without upsetting anyone else */
	}


     /* Next, pass it through the filter. If it passes, swap and place on queue. 
      *************************************************************************/
     /* See if we've been configured to pass on this pin number */
     if ( vdl_ew_filter( itmp ) == -1 ) continue; /* recall, we're in a while loop over messages */
     WaveMsgMakeLocal( (TRACE_HEADER*)inMsg ); /* convert the whole message to local byte order */
     RequestMutex();
     ret=enqueue( &OutQueue, inMsg, recsize, reclogo ); /* put it into the 'to be shipped' queue */
			/* feedme() is in the biz of de-queueng and handing to  */
     /* logit("","Stuffed message; ret: %d\n",ret); debug */
     ReleaseMutex_ew();
     if ( ret!= 0 )
         {       
	 if (ret==-2)  /* Serious: quit */
	    {
	    sprintf(Text,"internal queue error. Terminating/n");
            vdl_ew_status( TypeError, ERR_QUEUE, Text );
	    break;
	    }
 	 if (ret==-1) sprintf(Text,"queue cant allocate memory. Lost message\n");
	 if (ret==-3) {
		tm=time(&tm);						/* get time in seconds */
		if( abs( tm - LastCircularError) > 1) {
			sprintf(Text,"Circular queue lapped. Message lost %d %d\n",tm,
				LastCircularError);
         	vdl_ew_status( TypeError, ERR_QUEUE, Text );
		}
		LastCircularError=tm;
	 }
     continue;
         }
      } /*end of message getting loop */

   /* Shut it down
   **************/
   /* we do  this by setting our status variable to negative, which
      causes feedme() to send a signal to VDL to shut down
   */
   GetMsgStatus= -1;  /* so that feedme() will signal to VDL to quit */
   logit("t", "GetMsg thread: termination requested; exitting!\n" );
   (void)KillSelfThread( );  /* this terminates us (the thread), without upsetting anyone else */
}               



/*****************************************************************************
 *  vdl_ew_config() processes command file(s) using kom.c functions;         *
 *                    exits if any errors are encountered.	             *
 *****************************************************************************/
int vdl_ew_config( char *configfile )
{
   int      ncommand;     /* # of required commands you expect to process   */ 
   char     init[20];     /* init flags, one byte for each required command */
   int      nmiss;        /* number of required commands that were missed   */
   char    *com;
   int      nfiles;
   int      success;
   int      i;	
   char*    str;

/* Set to zero one init flag for each required command 
 *****************************************************/   
   ncommand = 8;
   for( i=0; i<ncommand; i++ )  init[i] = 0;
   nLogo = 0;

/* Open the main configuration file 
 **********************************/
   nfiles = k_open( configfile ); 
   if ( nfiles == 0 ) {
	fprintf( stderr,
         "vdl_ew: Error opening command file configfile=<%s>; exitting!\n", 
                 configfile );
	return( -1 );
   }

/* Process all command files
 ***************************/
   while(nfiles > 0)   /* While there are command files open */
   {
        while(k_rd())        /* Read next line from active file  */
        {  
	    com = k_str();         /* Get the first token from line */

        /* Ignore blank lines & comments
         *******************************/
            if( !com )           continue;
            if( com[0] == '#' )  continue;

        /* Open a nested configuration file 
         **********************************/
            if( com[0] == '@' ) {
               success = nfiles+1;
               nfiles  = k_open(&com[1]);
               if ( nfiles != success ) {
                  fprintf( stderr, 
                          "vdl_ew: Error opening command file <%s>; exitting!\n",
                           &com[1] );
                  return( -1 );
               }
               continue;
            }

        /* Process anything else as a command 
         ************************************/
  /*0*/     if( k_its("LogFile") ) {
                LogSwitch = k_int();
                init[0] = 1;
            }
  /*1*/     else if( k_its("MyModuleId") ) {
                str = k_str();
                if(str) strcpy( MyModName, str );
                init[1] = 1;
            }
  /*2*/     else if( k_its("RingName") ) {
                str = k_str();
                if(str) strcpy( RingName, str );
                init[2] = 1;
            }
  /*3*/     else if( k_its("HeartBeatInt") ) {
                HeartBeatInt = k_int();
                init[3] = 1;
            }

  /*4*/     else if( k_its("MaxMsgSize") ) {
                MaxMsgSize = k_int();
                init[4] = 1;
            }

  /*5*/     else if( k_its("MaxMessages") ) {
                MaxMessages = k_int();
                init[5] = 1;
            }


         /* Enter installation & module & message types to get
          ****************************************************/
  /*6*/     else if( k_its("GetMsgLogo") ) {
                if ( nLogo >= MAXLOGO ) {
                    fprintf( stderr, 
                            "vdl_ew: Too many <GetMsgLogo> commands in <%s>", 
                             configfile );
                    fprintf( stderr, "; max=%d; exitting!\n", (int) MAXLOGO );
                    return( -1 );
                }
                if( ( str=k_str() ) ) {
                   if( GetInst( str, &GetLogo[nLogo].instid ) != 0 ) {
                       fprintf( stderr, 
                               "vdl_ew: Invalid installation name <%s>", str ); 
                       fprintf( stderr, " in <GetMsgLogo> cmd; exitting!\n" );
                       return( -1 );
                   }
                }
                if( ( str=k_str() ) ) {
                   if( GetModId( str, &GetLogo[nLogo].mod ) != 0 ) {
                       fprintf( stderr, 
                               "vdl_ew: Invalid module name <%s>", str ); 
                       fprintf( stderr, " in <GetMsgLogo> cmd; exitting!\n" );
                       return( -1 );
                   }
                }
                if( ( str=k_str() ) ) {
                   if( GetType( str, &GetLogo[nLogo].type ) != 0 ) {
                       fprintf( stderr, 
                               "vdl_ew: Invalid msgtype <%s>", str ); 
                       fprintf( stderr, " in <GetMsgLogo> cmd; exitting!\n" );
                       return( -1 );
                   }
                }
                nLogo++;
                init[6] = 1;
            /*    printf("GetLogo[%d] inst:%d module:%d type:%d\n",
		        nLogo, (int) GetLogo[nLogo].instid,
                               (int) GetLogo[nLogo].mod,
                               (int) GetLogo[nLogo].type ); */  /*DEBUG*/
            }

	/* Pin number to send and vdl name to send it under
	 **************************************************/
  /*7*/	else if (k_its("SendPin") )
		{
		if (nSend>=MAXPIN)
     		    {
                    fprintf( stderr,"vdl_ew: Too many <SendPin> commands in <%s>",configfile );
                    fprintf( stderr, "; max=%d; exitting!\n", (int) MAXPIN );
                    return( -1 );
		    }
		SendPin[nSend].pin=k_int();
		SendPin[nSend].vdl_name=k_int();
		nSend++;
		init[7]=1;
		}


         /* Unknown command
          *****************/ 
	    else {
                fprintf( stderr, "vdl_ew: <%s> Unknown command in <%s>.\n", 
                         com, configfile );
                continue;
            }

        /* See if there were any errors processing the command 
         *****************************************************/
            if( k_err() ) {
               fprintf( stderr, 
                       "vdl_ew: Bad <%s> command in <%s>; exitting!\n",
                        com, configfile );
               return( -1 );
            }
	}
	nfiles = k_close();
   }

/* After all files are closed, check init flags for missed commands
 ******************************************************************/
   nmiss = 0;
   for ( i=0; i<ncommand; i++ )  if( !init[i] ) nmiss++;
   if ( nmiss ) {
       fprintf( stderr, "vdl_ew: ERROR, no " );
       if ( !init[0] )  fprintf( stderr, "<LogFile> "      );
       if ( !init[1] )  fprintf( stderr, "<MyModuleId> "   );
       if ( !init[2] )  fprintf( stderr, "<RingName> "     );
       if ( !init[3] )  fprintf( stderr, "<HeartBeatInt> " );
       if ( !init[4] )  fprintf( stderr, "<MaxMsgSize> " );
       if ( !init[5] )  fprintf( stderr, "<MaxMessages> " );
       if ( !init[6] )  fprintf( stderr, "<GetMsgLogo> "   );
       if ( !init[7] )  fprintf( stderr, "<SendPin> "  );
       fprintf( stderr, "command(s) in <%s>; exitting!\n", configfile );
       return( -1 );
   }

   return(0);
}

/****************************************************************************
 *  vdl_ew_lookup( )   Look up important info from earthworm.h tables       *
 ****************************************************************************/
void vdl_ew_lookup( void )
{
/* Look up keys to shared memory regions
   *************************************/
   if( ( RingKey = GetKey(RingName) ) == -1 ) {
	fprintf( stderr,
 	        "vdl_ew:  Invalid ring name <%s>; exitting!\n", 
                 RingName);
	exit( -1 );
   }

/* Look up installations of interest
   *********************************/
   if ( GetLocalInst( &InstId ) != 0 ) {
      fprintf( stderr, 
              "vdl_ew: error getting local installation id; exitting!\n" );
      exit( -1 );
   }

/* Look up modules of interest
   ***************************/
   if ( GetModId( MyModName, &MyModId ) != 0 ) {
      fprintf( stderr, 
              "vdl_ew: Invalid module name <%s>; exitting!\n", 
               MyModName );
      exit( -1 );
   }

/* Look up message types of interest
   *********************************/
   if ( GetType( "TYPE_HEARTBEAT", &TypeHeartBeat ) != 0 ) {
      fprintf( stderr, 
              "vdl_ew: Invalid message type <TYPE_HEARTBEAT>; exitting!\n" );
      exit( -1 );
   }
   if ( GetType( "TYPE_ERROR", &TypeError ) != 0 ) {
      fprintf( stderr, 
              "vdl_ew: Invalid message type <TYPE_ERROR>; exitting!\n" );
      exit( -1 );
   }
   return;
} 

/*******************************************************************************
 * vdl_ew_status() builds a heartbeat or error message & puts it into          *
 *                 shared memory.  Writes errors to log file & screen.         *
 *******************************************************************************/
void vdl_ew_status( unsigned char type, short ierr, char *note )
{
   MSG_LOGO    logo;
   char	       msg[256];
   long	       size;
   long        t;
 
/* Build the message
 *******************/ 
   logo.instid = InstId;
   logo.mod    = MyModId;
   logo.type   = type;

   time( &t );

   if( type == TypeHeartBeat )
   {
	sprintf( msg, "%ld\n\0", t);
   }
   else if( type == TypeError )
   {
	sprintf( msg, "%ld %hd %s\n\0", t, ierr, note);
	logit( "et", "vdl_ew: %s\n", note );
   }

   size = strlen( msg );   /* don't include the null byte in the message */ 	

/* Write the message to shared memory
 ************************************/
   if( tport_putmsg( &Region, &logo, size, msg ) != PUT_OK )
   {
        if( type == TypeHeartBeat ) {
           logit("et","vdl_ew:  Error sending heartbeat.\n" );
	}
	else if( type == TypeError ) {
           logit("et","vdl_ew:  Error sending error:%d.\n", ierr );
	}
   }

   return;
}



/************************** vdl_ew_filter *************************
 *           Decide if this message should be exported.           *
 *                 Change its format if necessary.                *
 *     Return    0 if the message should  be exported             *
 *              -1 if message should not be exported              *
 ******************************************************************/

int vdl_ew_filter( int pin)
{
   int i;
   for(i=0;i<nSend;i++)
	{
	if(  pin == SendPin[i].pin ) return(0);
	}
   return( -1 );
}

/********************** BigOrLittle ******************************
	to discover in which order this machine stores the
	bytes of a number
******************************************************************/

int BigOrLittle()
{
   char b[4];
   long l;
   union u {long l;char b[4];};
   union u overlay;

   l=1;
   logit("","byte order: %d %d %d %d\n",
		overlay.b[0],overlay.b[1],overlay.b[2],overlay.b[3]);
   return(1);
}
int feedme_test()
{
	return 0;
}
