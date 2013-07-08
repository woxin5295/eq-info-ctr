/* @(#)tap.c	1.3 5/7/95 */
/*======================================================================
 *
 *  vdlidatap.c - adaptation of eztap.c by C. Chaves to be used as a feedme
 *		for IDA stations via UCSD. by D.C. Ketchum May 1995

		1) Command line args must be passed onto VDL
		2) Help is disabled because of 1)
 		3) times coverted to nsn
 *
 *
 *====================================================================*/
/*======================================================================
 *
 *
 *  The data are returned in an xfer_packet structure, defined in
 *  xfer.h  In this example we print a few of the fields, to verify
 *  that data are being acquired.  Below is a list of everything that
 *  is returned in the packet:
 *
 *  Type     Name     Descriptions
 *  char *   sname    station name
 *  float    lat      station latitutude, decimal degrees
 *  float    lon      station longitude, decimal degrees
 *  float    elev     station elevation, meters ASL
 *  char *   cname    channel name
 *  char *   instype  GSE2.0 instype
 *  float    depth    sensor depth of burial, meters 
 *  float    sint     nominal sample interval in seconds
 *  float    calib    CSS 3.0 calib
 *  float    calper   CSS 3.0 calper
 *  float    vang     vertical orientation of sensor
 *  float    hang     horizontal orientation of sensor
 *  double   beg      time of first sample in packet
 *  double   end      time of last  sample in packet
 *  int      tear     if set, there was a time tear
 *  long     nsamp    number of samples
 *  long *   data     the data, as properly ordered longs
 *
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "util.h"
#include "xfer.h"
#include "vdl.h"


/*
	locally global variables
*/
int time_store;					/* file descriptor for time file */
extern FILE *logout;
char **stations;				/* array of ptr to strings with codes inthem*/
int nstations;					/* number of stations in stations*/
int nrecs =0;					/* counts records processed to limit rates */
XFER *xp;						/* pointer to struct for using IDATAP*/
struct xfer_packet *packet;		/* buffer space for packets */
double idalasttime;				/* used to check on updates to last time*/

struct nsntime maknsn();

void help(myname)
char *myname;
{
    fprintf(logout,"\n");
    fprintf(logout,"usage: %s if=host sc=sta:chn,... [options]\n",
        myname
    );
    fprintf(logout,"\n");
    fprintf(logout,"Options:\n");
    fprintf(logout,"beg=string  => beg time (yyyy:ddd-hh:mm:ss)\n");
    fprintf(logout,"end=string  => end time (yyyy:ddd-hh:mm:ss)\n");
    fprintf(logout,"+/-keepup   => toggle timeout retry behavior\n");
    fprintf(logout,"+/-retry    => toggle timeout retry behavior\n");
    fprintf(logout,"\n");
    exit(1);
}
int feedme_init(argc, argv)
int argc;
char *argv[];
{
char *tmp;
char *host = NULL;			/* host of data source */
char *sc   = NULL;
int keepup = 1;
int retry  = 1;
double beg = XFER_YNGEST;
double end = XFER_YNGEST;
 
char timefile[30] = "";
char logname[]="./vdltap.log";
char *home     = NULL;
char *log      = logname;		/* if not logging, set to null, else logname*/
char *syscode  = NULL;
int debug      = 1;				/* use to be NRTS_DEFLOG */
size_t len;
int i,j,k,l,addon;
char tag[20];
char timename[30];
extern int MAX_CH;
extern struct chan_desc *ch;
extern int cmd_flag;

 
/*  Get command line arguments  */
  for (i = 1; i < argc; i++) 
	{	if (strncasecmp(argv[i], "if=", strlen("if=")) == 0) 
		{   host = argv[i] + strlen("if=");
			fprintf(logout,"IDATAP host=%s\n",host);
        }
		else if (strncasecmp(argv[i], "server=",strlen("server=")) == 0) 
		{	host = argv[i] + strlen("server=");
			fprintf(logout,"IDATAP host=%s\n",host);
        } 
		else if (strncmp(argv[i], "debug=", strlen("debug=")) == 0) 
		{	debug = atoi(argv[i]+strlen("debug="));
			fprintf(logout,"IDATAP debug=%d\n",debug);
    }
    else if (strcmp(argv[i],"-i") == 0)
    {	strcpy(timefile,argv[i+1]);
    	strcat(timefile,"_vdl.time");
    	fprintf(logout,"IDATAP Time file : %s\n",timefile);
    }
    
	}
	fflush(logout);
/*
	Create structures based on VDL channels.  The ch[i].txt is of the form
	stat_chn where chn is a 3 letter component name e.g. "BHZ".
*/
	nstations=0;
	fprintf(logout,"MAXCH=%d %d\n",MAX_CH,sizeof(char *)); fflush(logout);
	stations=(char **) malloc(MAX_CH * sizeof(char *));/* array of string ptrs*/
	for(i=0; i<MAX_CH; i++) stations[i]=NULL;		/* start with null array*/
	for(i=0; i<=MAX_CH; i++) 
	{	k=0;
		while (ch[i].txt[k] != '_') {	tag[k]=ch[i].txt[k]; k++;}
		tag[k]=0;							/* null terminate string*/
		/*fprintf(logout,"Tags=%s %d\n",tag,strlen(tag)); fflush(logout);*/
		for(j=0; j<MAX_CH; j++)				/* look thru stations for match*/
		{	if(stations[j] == NULL) 		/* end of stations? */
			{	stations[j]=(char *) malloc(50);	/*get space to store name*/
				nstations=j+1;						/* how many stations*/
				strcpy(stations[j],tag);			/* copy name onto list */
				strcat(stations[j],":");			/* add the colon */
				fprintf(logout,"New stat j=%d str=%s\n",j,stations[j]); fflush(logout);
				break;								/* done, break out */
			}
			/*fprintf(logout,"%d %s|%s|%d %d\n",j,stations[j],tag,strlen(tag),
				strncmp(stations[j],tag,strlen(tag))); fflush(logout);*/
			if(strncmp(stations[j],tag,strlen(tag)) == 0) break;
		}
		k++;								/* point to after the _ */
		addon=1;							/*flag add it to list */
		if(ch[i].txt[k] == 'l' && ch[i].txt[k+1] == 'h')
		{	for(l=0; l<MAX_CH; l++) if(ch[l].derive_lp == i) 
			{	addon=0;
				fprintf(logout,"Do not sc derived LPs %s\n",ch[i].txt);
				 break;
			}
		}
		if(addon) 
		{	strncat(stations[j], &ch[i].txt[k],3);	/* move component to end */
			strcat(stations[j],",");				/* add comma */
		}
	}
	fprintf(logout,"nstations=%d\n",nstations);
	for(i=0; i<nstations; i++) fprintf(logout,"%d %s\n",i,stations[i]);
	fflush(logout);
	len=0;
	for(i=0; i<nstations; i++) len+=strlen(stations[i])+1;/* how long */
	sc = (char *) malloc(len);					/* make it big enough */
	*sc=0;									/* zero length string */
	for(i=0; i<nstations; i++) 
	{	strncat(sc, stations[i], strlen(stations[i])-1);
		if(i != nstations-1) strcat(sc, "+");
	}
	fprintf(logout,"Sc=%s\n",sc);

/* Must specify host and sta/chan string */

    if (host == (char *) NULL || sc == (char *) NULL) help(argv[0]);

/*
	Open file which contains the last time gotten and hence time to start
*/
	if(strlen(timefile) == 0) strcpy(timefile,"vdl.time");
	time_store=open(timefile,O_RDWR);
	if(time_store != -1) 				/* if exists get time */
	{	timename[0]=0;
		i=read(time_store,timename,17);
		timename[17]=0;
		fprintf(logout,"Start time found = |%s| i=%d errno=%d fd=%d\n",
			timename,i,errno,time_store); fflush(logout);
		
        beg = util_attodt(timename);			/* Set in NRTS info struct */
		beg -=300.;							/* backup 5 minutes */
		fprintf(logout,"beg=%f\n",beg); fflush(logout);
	} 
	else 
	{	
		time_store=open(timefile,O_WRONLY | O_CREAT | O_TRUNC,0644);
		if(time_store == -1) 
		{	fprintf(logout,"Bad time store create! erno=%d\n",
				errno);fflush(logout);
			exit(100);
		}
	}
	fprintf(logout,"start at time %s\n",util_dttostr(beg, 0)); fflush(logout);

/* Open connection to server */
	if(debug) fprintf(logout,"host=%s beg=%f end=%f keepup=%d retry=%d\nsc=%s \n",
		host,beg,end,keepup,retry,sc); fflush(logout);
    xp = Xfer_Open(host, sc, beg, end, keepup, retry);
    if (xp == (XFER *) NULL) 
	{  fprintf(logout, "%s: can't connect with server: %s\n",
            argv[0], Xfer_ErrStr()
        );
		sleep(120);
        exit(401);
    } else
	{	fprintf(logout,"XP.host=%s port=%d keepup=%d retry=%d\n",
			xp->host, xp->port, xp->keepup, xp->retry);
	}
	packet= (struct xfer_packet *) malloc(sizeof(struct xfer_packet));
	return 0;
}


feedme_test(ich,ia,tc)
int *ich;
long ia[]; struct nsntime *tc;
{
	return 0;
}
feedme(ich,ia,tc)
int *ich;				/* channel number */
long ia[];				/* returned data */
struct nsntime *tc;		/* time of first sample */
{
int i;
int status;
unsigned int isleep=3;
static unsigned char lastchar,lastchar2;
char *line;
int iy,id,ih,im,is,ms;
int ifreq;
extern int SPFREQ;
extern int node;
char tag[20];
extern int cmd_flag;
extern int MAX_CH;
extern struct chan_desc *ch;
	
top:
#ifdef DEBUG_PRINT
		fprintf(logout,"Calling Xfer_Read..\n"); 
		fflush(logout);
#endif
	if((status = Xfer_Read(xp, packet)) == XFER_OK) 
	{
#ifdef DEBUG_PRINT
		fprintf(logout,"IDATAP ok.. %s %s ns=%d %s\n",
			packet->sname,packet->cname,
			packet->nsamp,util_dttostr(packet->beg,0));				
			fflush(logout);
#endif
		if(packet->nsamp < 0) 
		{	fprintf(logout,"*** Neg ns=%d wait and go %s\n",packet->nsamp,tag);
			fflush(logout);
			i=sleep(isleep);
			exit(20);
			goto top;
		}
			
		line=util_dttostr(packet->beg,0);
		line[20]=0;
/*		if( strcmp(packet->sname,"abkt") == 0) fprintf(logout,"%s %s sint=%f ns=%d %s\n",
			packet->sname,packet->cname,packet->sint,packet->nsamp,
			util_dttostr(packet->beg.time,0));
		if( strcmp(packet->sname,"abkt") == 0 && 
			(fabs( packet->sint - .05) > 0.01 || packet->nsamp != 240)) 
			goto top;	*/
		nrecs++;
		if( (nrecs % (nstations*2+1)) == 0) i=sleep(1);/* control max rate*/
		if( ((strcmp(packet->cname,"bhz") == 0) || (strcmp(packet->cname,"shz") == 0))
			 && (*(line+13) !=lastchar)) {
			lastchar=*(line+13);
			if(*(line+12) != lastchar2) {
				lastchar2=*(line+12);
				fprintf(logout,"%s %s %s ",
				packet->sname,packet->cname,util_dttostr(packet->beg,0));
/*				fprintf(logout," %6.3f ns=%3d,tear=%d ",
				packet->sint,packet->nsamp,packet->tear);*/
				fprintf(logout,"\n");
			}
			if(line[0] == '2' && line[1] == '0' && packet->beg >idalasttime) 
			{					/* is time code ok. year=9? */
				lseek(time_store,0,0);
				write(time_store,line,21);
				idalasttime=packet->beg;
			
			} 
			else if(line[0] != '2' || line[1] != '0') 
			{
				fprintf(logout,"bad time. Skip packet :");
				for(i=0; i<21; i++) fprintf(logout,"%c",line[i]);
				fprintf(logout,"\n");
				goto top;
			}
		}
		ifreq=1./packet->sint+.0001;
		if(SPFREQ != ifreq && ifreq > 2) {
			fprintf(logout,"%s Setting SPFREQ to %d\n",tag,ifreq);
			SPFREQ=ifreq;
		}
#ifdef DEBUG_PRINT
		for(i=0; i<20; i++) {
			fprintf(logout,"%8d",*(packet->data+i));
			if((i%10) == 9) fprintf(logout,"\n");
		}
#endif
		*ich=-1;
		strcpy(tag,packet->sname);			/* put station name */
		strcat(tag,"_");					/* add underscore */
		strcat(tag,packet->cname);			/* add component */
		for(i=0; i<=MAX_CH; i++)			/* look through chns to find match*/
		{	if(strcmp(tag,ch[i].txt) == 0) 
			{	*ich=i;						/* found it, break out */
				break;
			}
		}
		if(*ich == -1) fprintf(logout,"failed to translate station %s %s\n",
			packet->sname,packet->cname);


		/* put data in user data buffer */
		memcpy(ia,packet->data,sizeof(long)*packet->nsamp);
		if(*(line+20) == ' ' || *(line+20) == 0) *(line+20)='0';
		sscanf(line,"%4d:%3d-%2d:%2d:%2d.%3d",&iy,&id,&ih,&im,&is,&ms);
#ifdef DEBUG_PRINT
		fprintf(logout,"scan %d %d:%d:%d:%d.%d %d %s\n",iy,id,ih,im,is,ms,
			*(line+20),line);
#endif
		*tc=maknsn(iy,id,ih,im,is,ms,0);
		return (packet->nsamp);
    } 
	else 
	{	if(status == XFER_FINISHED )
		{	fprintf(logout,"End of data????\n");
			exit(105);
		}
		if(status == XFER_ERROR)
		{	fprintf(logout,"XFER_ERROR was returned by Xfer_Read. Exit...\n");
			exit(106);
		}
     	fprintf(logout,"exit??? Unexpected read status= %d\n",status);
		fflush(logout);
		return 0;   
	}   
}


feedme_shutdown()
{
	Xfer_Close(xp);					/* shutdown link to server */
	fprintf(logout,"Feedme_shutdown complete\n"); fflush(logout);
	return 0;
}
