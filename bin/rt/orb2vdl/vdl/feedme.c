/************************************************************************/
/*  VDL interface routines for Mini-SEED data from a TCP/IP socket	*/
/************************************************************************/
/*
Author:
	Doug Neuhauser, UC Berkeley Seismographic Station

Purpose:
	VDL interface routines to receive Mini-SEED data from a 
	TCP/IP socket process such as datalog.

Modification History:
Date     who	Version	Modifications.
------------------------------------------------------------------------
96/06/24 DSN	1.0	Initial coding.

*/
/************************************************************************/

/*
	DCK define optional params 
*/
/*#define DEBUG_PRINT*/
/*#define REQUEST_CHANNELS*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <time.h>

#include <qlib.h>

#include "vdl.h"
#include "datasock_codes.h"

#define	SUCCESS		0
#define	FAILURE		-1
#define	MIN_BLKSIZE	128
#define	TIMESTRLEN	40
#define	RETRY_SECONDS	60

/************************************************************************/
/*  External symbols required by functions in this file.		*/
/************************************************************************/
extern FILE *logout;			/* FILE ptr for log file.	*/

/************************************************************************/
/*  Externals required by functions in this file.			*/
/************************************************************************/
static char *hostname = NULL;		/* hostname providing data.	*/
static char *portname = NULL;		/* port/service providing data.	*/
static char *passwd = NULL;		/* password for data socket.	*/
static int port = 0;			/* port providing data.		*/
static int sd = -1;			/* file descriptor for socket.	*/
static char *station = NULL;		/* remote station to request.	*/
static char *channels = NULL;		/* remote channels to request.	*/

/************************************************************************/
/*  Externally visibile functions in this file.				*/
/************************************************************************/
int feedme_init (int argc, char **argv);
int feedme (int *ich,			/* channel index (returned).	*/
	    long ia[],			/* data array (returned).	*/
	    struct nsntime *tc);	/* USNSN sample time (returned).*/
int feedme_shutdown ();

/************************************************************************/
/*  Internally visibile functions in this file.				*/
/************************************************************************/
static
open_data_socket(char *server,	/* hostname providing data.	*/
		     char *service,	/* port/service providing data.	*/
		     char *passwd,	/* optional passwd for data.	*/
		     char *station,	/* optional station to request.	*/
		     char *channels);	/* optional channels to request.*/
static
DATA_HDR* get_packet (int sd,		/* input socket descriptor.	*/
		      char *indata);	/* input Mini-SEED buffer.	*/
static
char *gmt_clock_string(time_t clock);	/* time to convert to string.	*/

/************************************************************************/
/*  feedme_init:							*/
/*	Initialization routine for Mini-SEED socket interface.		*/
/************************************************************************/
int feedme_init (int argc, char **argv)
{
    int i;

    /* Variables needed for getopt. */
    extern char	*optarg;
    extern int	optind, opterr;
    int		c;

    /* Parse command line arguments.					*/
    /* Ignore arguments that we do not need.				*/
	fprintf(logout,"feedme_init top\n");
#ifdef	USE_GETOPT
    while ( (c = getopt(argc,argv,"f:p:c:l:t:n:s:d:g:h:i:x:k:q:H:P:S:C:p:")) != -1) {
#else
    /* Assume ALL cmdline arguments are options followed by single arg.	*/
    for (i=1; i<argc; i+=2) {
	if (argv[i][0] != '-') {
	    fprintf (logout, "Error - invalid option: %s %c %d\n", 
			argv[i],*argv[i],*argv[i]); fflush(logout);
	    exit(202);
	}
	c = strlen(argv[i]);			/* is it a 1 character switch */
	if(c <= 2) c = argv[i][1];
	else c='#';						/* its not, so do not process it */
	optarg = argv[i+1];
	fprintf(logout,"FEEDME_INIT : opt %s arg %s %c|\n",argv[i],argv[i+1],c);
#endif
	switch (c) {
	  case 'H':
	    /* Specify name of remote host.				*/
	    hostname = optarg; 
		fprintf(logout,"host=%s\n",hostname);
	    break;
	  case 'P':
	    portname = optarg; 
		fprintf(logout,"port=%s\n",portname);
	    break;
	  case 'S':
	    station = optarg;
		fprintf(logout,"station=%s\n",station);
	    break;
	  case 'C':
#ifdef	REQUEST_CHANNELS
	    channels = optarg;
		fprintf(logout,"Channels=%s\n",channels);

#endif
	    break;
	  case 'p':
	    passwd = optarg;
		fprintf(logout,"Password set \n");
	    break;
	  default:
	    break;
	}
    }
	fflush(logout);
    /* Verify that all required arguemnts are present.			*/
    if (hostname == NULL) {
	fprintf (logout, "Error - missing -H hostname option\n"); fflush(logout);
	exit(203);
    }
    if (portname == NULL) {
	fprintf (logout, "Error - missing -P portname option\n"); fflush(logout);
	exit(204);
    }
    port = atoi (portname);
    if (port <= 0) {
	char *proto = NULL;
	struct servent *servent;
	servent = getservbyname (portname, proto);
	if (servent) port = ntohs(servent->s_port);
    }
    if (port <= 0) {
	fprintf (logout, "Error - invalid or unknown portname %s\n", portname);
	fflush(logout);
	exit(205);
    }
    /* Password is optional, depending on remote socket configuration.	*/
    /* We do not insist on it being supplied.				*/
    
    if (station == NULL) {
	fprintf (logout, "Error - missing -S station option\n"); fflush(logout);
	exit(206);
    }
#ifdef	REQUEST_CHANNELS
    if (channels == NULL) {
	fprintf (logout, "Error - missing -C channels option\n");
	fflush(logout);
	exit(207);
    }
#endif
	fprintf(logout,"opendata sock %s %s \n",hostname,portname); fflush(logout);
    /* Open socket for remote data feed.				*/
    while ((sd = open_data_socket (hostname, portname, passwd, station, channels)) < 0) {
	fprintf (logout, "%s Error opening socket for data.\n",
		 gmt_clock_string(time(NULL)));
	sleep (RETRY_SECONDS);
    }
    return (SUCCESS);
}

/************************************************************************/
/*  feedme_shutdown:							*/
/*	Shutdown Mini-SEED socket connection.				*/
/************************************************************************/
int feedme_shutdown (){
    close (sd);
    sd = -1;
    return (SUCCESS);
}

#define	ONCE	while (0)
/************************************************************************/
/*  open_data_socket:							*/
/*	Open a socket on the specified machine and service name/number,	*/
/*	and perform any required configuration over the socket.		*/
/*	Returns:	open file descriptor				*/
/*			negative value on error.			*/
/************************************************************************/
static
int open_data_socket(char *server,	/* hostname providing data.	*/
		     char *service,	/* port/service providing data.	*/
		     char *passwd,	/* optional passwd for data.	*/
		     char *station,	/* optional station to request.	*/
		     char *channels)	/* optional channels to request.*/
{
    struct sockaddr_in server_sock;
	struct hostent hp2;
    struct hostent *hp;
    struct servent *sp;
    unsigned long  ipadd;
    char request_str[256];
    int status = FAILURE;
    int send_eot;
    int i, l, n;
    int s;
	fprintf(logout,"OPEN_DATA_SOCK : top \n"); fflush(logout);
    do {
	/* Create an socket on which to make the connection.		*/
	/* This function call says we want our communications to take	*/
	/* place in the internet domain (AF_INET).			*/
	/* (The other possible domain is the Unix domain).		*/
	s = socket (AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
	    fprintf (logout, "%s Error opening socket in client.\n",
		     gmt_clock_string(time(NULL)));
	    break;
	}
 	/* Get the IP address of the remote host in the "hp" structure.	*/
	/* It must be available to this machine or the call files.	*/
	/* Info can come from hosts file, NIS, DNS, etc.		*/
	for(i=0; i<=strlen(server); i++) fprintf(logout," %3o",server[i]);
	fprintf(logout,"\n");
	if (isalpha (server[0])) {
		fprintf(logout,"OPEN_DATA_SOCK : gethost %s \n",server); fflush(logout);
	    hp = gethostbyname (server);/* probe name server by name.	*/
	}
	else {
		fprintf(logout,"OPEN_DATA_SOCK : inetadr %s\n",server); fflush(logout);
	    ipadd = inet_addr (server);	/* convert IP to binary format.	*/
		fprintf(logout,"Converted Dot address %x \n",ipadd);
	    hp = gethostbyaddr((char*)&ipadd, sizeof(ipadd), AF_INET);	
		if(hp == NULL) {
			hp=&hp2;
			hp2.h_addrtype=2;
			hp2.h_addr=(char *) &ipadd;
			hp2.h_length=4;
		} else fprintf(logout,"addr=%x type=%d %d  %s\n",
		*hp->h_addr, hp->h_addrtype,hp->h_length,hp->h_name);
					/*probe name server by address.	*/
	}
	fprintf(logout,"OPEN_DATA_SOCK : chknull %d\n",hp); fflush(logout);

	if (hp == NULL) {
	    fprintf (logout, "%s Remote host is unknown to name server. %x\n",
		     gmt_clock_string(time(NULL)),h_errno); fflush(logout);
	    break;
	}

	/* If specified as a name the service/port must be defined by	*/
	/* name	on this system.						*/
	fprintf(logout,"OPEN_DATA_SOCK : getservbyname \n"); fflush(logout);
	if (isalpha (service[0])) {
	    sp = getservbyname (service, "tcp");/* probe service by name */
	    if (sp == NULL) {
		fprintf(logout, "%s Service not in local host table.\n",
			gmt_clock_string(time(NULL)));
		break;
	    }
	    else {
		server_sock.sin_port = sp->s_port;
	    }
	}
	else {
	    /* Convert service number from string to number.		*/
	    server_sock.sin_port = htons(atoi(service)); 
	    if (port <= 0) {
		fprintf (logout, "%s Invalid port number: %s\n", 
		     gmt_clock_string(time(NULL)), service);
		break;
	    }
	}

	/* Create 'sockaddr_in' internet address structure for connect.	*/
	server_sock.sin_family = hp->h_addrtype;
	strncpy ((char*)&server_sock.sin_addr, hp->h_addr, hp->h_length);

	/* Connect to the socket address.	*/
	fprintf(logout,"OPEN_DATA_SOCK : connect \n"); fflush(logout);
	if (connect (s, (struct sockaddr *) &server_sock, sizeof(server_sock)) < 0) {
	    fprintf (logout, "%s Attempt to connect to remote host failed.\n",
		     gmt_clock_string(time(NULL)));
	    break;
	}

	/* Send configuration information to remote system if necessary.	*/
	fprintf(logout,"OPEN_DATA_SOCK : passwd \n"); fflush(logout);
	send_eot = 0;
	if (passwd) {
	    sprintf (request_str, "%d %s %s\n", DATASOCK_PASSWD_CODE,
		     PASSWD_KEYWD, passwd);
	    l = strlen(request_str);
	    if ((n=write (s, request_str, l)) != l) {
		fprintf (logout, "%s Error writing request string\n",
			 gmt_clock_string(time(NULL))); fflush(logout);
		exit(208);
	    }
	    send_eot = 1;
	}
#ifdef	REQUEST_CHANNELS
	if (station && channels) {
	    sprintf (request_str, "%d %s %s\n", DATASOCK_CHANNEL_CODE,
		     station, channels);
	    l = strlen(request_str);
	    if ((n=write (s, request_str, l)) != l) {
		fprintf (logout, "%s Error writing request string\n",
			 gmt_clock_string(time(NULL)));
		break;
	    }
	    send_eot = 1;
	}
#endif
	if (send_eot) {
	    sprintf (request_str, "%d %s\n", DATASOCK_EOT_CODE,
		     EOT_KEYWD);
	    l = strlen(request_str);
	    if ((n=write (s, request_str, l)) != l) {
		fprintf (logout, "%s Error writing request string\n",
			 gmt_clock_string(time(NULL)));
		break;
	    }
	}
	status = SUCCESS;
    } ONCE;
	fprintf(logout,"OPEN_DATA_SOCK : Break DO %d %d %d\n",status,SUCCESS,FAILURE); fflush(logout);

    /* Return file descriptor of connected socket if successful.	*/
    if (status != SUCCESS) {
	if (s >= 0) close (s);
	s = FAILURE;
    }
	fprintf(logout,"OPEN_DATA_SOCK : Diagnostics \n"); fflush(logout);

    /* Diagnostics - print IP address in xx.xx.xx.xx format		*/
    if(hp != NULL) fprintf (logout, "%s Remote host: %s IP address: %s Port: %d\n", 
	     gmt_clock_string(time(NULL)), hp->h_name,
	     inet_ntoa(server_sock.sin_addr),
	     ntohs(server_sock.sin_port));
	else { fprintf(logout,"Open failed!!\n"); fflush(logout); exit(201);}
	fprintf(logout,"OPEN_DATA_SOCK : bottom \n"); fflush(logout);

    return(s);
}

/************************************************************************/
/*  feedme:								*/
/*	Return unpacked data from next Mini-SEED record.		*/
/*	Return number of samples as function value.			*/
/************************************************************************/
int feedme (int *ich,			/* channel index (returned).	*/
	    long ia[],			/* data array (returned).	*/
	    struct nsntime *tc)		/* USNSN sample time (returned).*/
{
    DATA_HDR *hdr = NULL;
    char ms[4096];
    EXT_TIME et;
    int nsamples = 0;
    int flags;
	extern FILE *logout;
    int msec;

    while (1) {
	if (hdr != NULL && hdr != (DATA_HDR *)FAILURE) {
	    free_data_hdr (hdr);
	    hdr = NULL;
	}

	while (sd < 0) {
#ifdef DEBUG_PRINT
		fprintf(logout," rtry"); fflush(logout);
#endif
	    sleep (RETRY_SECONDS);
	    sd = open_data_socket (hostname, portname, passwd, station, channels);
	    if (sd >= 0) break;
	    fprintf (logout, "%s Error opening socket for data.\n",
		 gmt_clock_string(time(NULL))); fflush(logout);
	}
#ifdef DEBUG_PRINT
	fprintf(logout," GET"); fflush(logout);
#endif
	hdr = get_packet (sd, ms);
#ifdef DEBUG_PRINT
	fprintf(logout," %x %d",hdr,hdr->num_samples); fflush(logout);
#endif
	/* Close socket if error reading from the socket.		*/
	if (hdr == (DATA_HDR *)FAILURE || hdr == NULL) {
	    fprintf (logout, "%s Error reading mini-SEED record\n",
		     gmt_clock_string(time(NULL)));fflush(logout);
	    close (sd);
	    sd = -1;
	    continue;
	}

	/* Skip empty packets -- seen at end of triggers.		*/
/*	fprintf (logout, "feedme:  station: %s channel: %s net: %s nsamples: %d rate=%d\n",
		hdr->station_id, hdr->channel_id, hdr->network_id,
		hdr->num_samples,hdr->sample_rate);*/
	if (hdr->num_samples == 0) continue;

	/* Unpack the Mini-SEED data.					*/
	/* Discard record if # of samples do not match header.		*/
	nsamples = unpack_ms_data (hdr, (int *)ia, hdr->num_samples, 
				   &ms[hdr->first_data]);
	if (nsamples != hdr->num_samples) {
		fprintf(logout,"nsamp != %d %d\n",nsamples,hdr->num_samples);
		fflush(logout);
		continue;
	}
	
#ifdef DEBUG_PRINT
	fprintf(logout," un %d ",nsamples);fflush(logout);
#endif

	/* Ensure that this packet is from the expected station.	*/
	if (strcmp(hdr->station_id, station) != 0) continue;
	    
	/* Assign NSN channel ID from Mini-SEED channel name.		*/
	/* This mapping may be STATION DEPENDENT.			*/
	if      (strcmp(hdr->channel_id,"BHZ")==0) *ich = CH_Z20;
	else if (strcmp(hdr->channel_id,"BHN")==0) *ich = CH_X20;
	else if (strcmp(hdr->channel_id,"BHE")==0) *ich = CH_Y20;
    else if (strcmp(hdr->channel_id,"HHZ")==0) *ich = CH_Z20;
	else if (strcmp(hdr->channel_id,"HHN")==0) *ich = CH_X20;
	else if (strcmp(hdr->channel_id,"HHE")==0) *ich = CH_Y20;
	else if (strcmp(hdr->channel_id,"LHZ")==0) *ich = CH_Z1;
	else if (strcmp(hdr->channel_id,"LHN")==0) *ich = CH_X1;
	else if (strcmp(hdr->channel_id,"LHE")==0) *ich = CH_Y1;
	else continue;		/* Skip unknown channel.		*/

	/* Compute flag portion of USNSN time code.			*/
	flags = 0;

	/* Compute USNSN time code.					*/
	/* Time of first sample is:					*/
	/*  et.year, et.doy, et.hour, et.minute, et.second, et.ticks	*/
	/* where a tick is .0001 second.				*/
#ifdef DEBUG_PRINT
	fprintf(logout," %s",hdr->channel_id); fflush(logout);
#endif
	et = int_to_ext(hdr->begtime);

	tc->iyr = (unsigned char)((et.year - 1970)*2 + (et.doy>>8));
	tc->doy = (unsigned char)(et.doy%256);

	msec = (((et.hour*60) + et.minute)*60 + et.second)*1000 + et.ticks/10;
	tc->ms = msec*16 + flags;

	/* Mini-SEED record handling is complete.			*/
	break;
    }
    if (hdr != NULL && hdr != (DATA_HDR *)FAILURE) {
#ifdef	DEBUG
	fprintf (logout, "feedme:  station: %s channel: %s net: %s nsamples: %d\n",
		hdr->station_id, hdr->channel_id, hdr->network_id,
		hdr->num_samples);
	fprintf (logout, "feedme:  Mini-Seed time: %s to ",		 
		time_to_str(hdr->begtime, JULIANC_FMT_1));
	fprintf (logout, "%s\n", time_to_str(hdr->endtime, JULIANC_FMT_1));
	fflush(logout);
#endif
#ifdef DEBUG_PRINT
	fprintf(logout," free %x",hdr); fflush(logout);
#endif
	free_data_hdr (hdr);
    }
#ifdef DEBUG_PRINT
	fprintf(logout," ret"); fflush(logout);
#endif
   return (nsamples);
}

/************************************************************************/
/*  get_packet:								*/
/*	Read a miniSEED packet from the socket, and return a DATA_HDR	*/
/*	structure for the packet.  Calling routine must free DATA_HDR.	*/
/************************************************************************/
static
DATA_HDR* get_packet (int sd,		/* input socket descriptor.	*/
		      char* indata)	/* input Mini-SEED buffer.	*/
{
    int            nr, blksize, i;
    DATA_HDR      *hdr;
    BS            *bs = NULL;
    BLOCKETTE_HDR *bh = NULL;
    int goodpacket;

    goodpacket = 0;
    if (sd < 0) return((DATA_HDR *)FAILURE);

    while(!goodpacket) {
	nr = xread(sd, (char *)indata, MIN_BLKSIZE);
	if (nr == 0) {
            fprintf (logout, "%s Found end of file on socket read: %d\n",
		     gmt_clock_string(time(NULL)), nr);
            return((DATA_HDR *)FAILURE);
	}
	if (nr < MIN_BLKSIZE) {
            fprintf (logout, "%s Error reading from socket: expected %d got %d\n",
		     gmt_clock_string(time(NULL)), MIN_BLKSIZE, nr);
			for(i = 0; i< nr; i++) {
				fprintf(logout,"%3d ",indata[i]);
				if( (i % 20) == 19) fprintf(logout,"\n");
			}
			for(i = 0; i< nr; i++) fprintf(logout,"%c ",indata[i]);
			fprintf(logout,"\n");
            continue;
	}
	if ((hdr = decode_hdr_sdr ((SDR_HDR *)indata, &blksize)) == NULL) {
	    fprintf (logout, "%s Error decoding SEED data hdr\n",
		     gmt_clock_string(time(NULL)));
	    continue;
	}
	if (blksize > MIN_BLKSIZE) {
	    nr = xread (sd, (char *)indata + MIN_BLKSIZE, blksize-MIN_BLKSIZE);
	    if (nr < blksize-MIN_BLKSIZE) {
                fprintf (logout, "%s Error reading SEED data\n",
			 gmt_clock_string(time(NULL)));
		free_data_hdr (hdr);
                continue;
	    }
	}
        
	goodpacket = 1;

	/* Fill in the number of data frames.   */
	if ((bs = find_blockette(hdr,1001)) &&
	    (bh = (BLOCKETTE_HDR *)(bs->pb))) {
	    hdr->num_data_frames = ((BLOCKETTE_1001 *)bh)->frame_count;
	    /* Explicitly set num_data_frames for SHEAR stations.   */
	    if (hdr->num_data_frames == 0 && hdr->sample_rate != 0)
	      hdr->num_data_frames = (blksize - hdr->first_data) / sizeof(FRAME);
	}
	else {
	    hdr->num_data_frames =
	      (hdr->sample_rate == 0) ? 0 :
	    (blksize - hdr->first_data) / sizeof(FRAME);
	}
    } /* End while !goodpacket */
    return (hdr);
}

/************************************************************************/
/*  gmt_clock_str:							*/
/*	Generate time string with GMT time.				*/
/************************************************************************/
static 
char *gmt_clock_string(time_t clock)	/* time to convert to string.	*/
{
    static char time_str[TIMESTRLEN];
    strftime(time_str, TIMESTRLEN, "%Y/%m/%d,%T GMT", gmtime(&clock));
    return(time_str);
}
