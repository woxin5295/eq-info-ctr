#include <stdlib.h>
#include <errno.h>
#include "vdl.h"
#include "vdlqsub.h"
#include "db.h"
#include "stock.h"
/* Mitch Robinson  3-6-2002 */
/* #include "pkt.h" */ 
#include "Pkt.h" 
#include "orb.h"
int orbfd;
int dbgorb=0;
char orbchan[STRSZ]="";
char orbname[STRSZ]="gldketchum";
extern FILE *logout;
char *asctim();					/* asctim prototype */

/*
	feedme_init for ORB opens an ORBSERVER port and selects channels to be
	received.  These are based on the command line paramaters ofter 
	-orbname system.net:port and -orbchan "ORB regular expression".
	The normal return is zero.  All other returns indicate the ORBSERVER
	did not open or that the ORBSELECT failed.  feedme_init can be called
	later and will reopen the channel if ARGC is zero.

*/
feedme_init( int argc, char **argv )
{	int nsrc=-1;
	int i;
	fprintf(logout,"%s In feedme_init\n",asctim());  fflush(logout);

	/* Pick off the ORB related command line parameters */
	for(i=0; i<argc; i++)
	{	if( strcmp(argv[i],"-orbname") == 0 ) 
		{	strcpy(orbname, argv[i+1]);
			fprintf(logout,"Orb server name=%s\n",orbname);
		}
		if( strcmp(argv[i],"-orbchan") == 0)
		{	strcpy(orbchan, argv[i+1]);
			fprintf(logout,"Orb channels =%s\n",orbchan);
		}
		if( strcmp(argv[i],"-dbgorb") == 0)
		{	dbgorb=1;
			fprintf(logout,"Debug ORB is on \n");
		}
	}
	
	if( ( orbfd = orbopen( orbname, "r" ) ) < 0 ) 
	{	fprintf(logout,"%s ORBOPEN returned=%d errno=%d\n",
			asctim(),orbfd,errno);
		fflush(logout);
		sleep(120);			/* limit failure rate */
		return 1;
	}
	
	/* use ORBSELECT to choose channels to receive */
	nsrc=orbselect( orbfd, orbchan );
	if(nsrc < 0) 
	{	fprintf(logout,"%s ORBSELECT returned err=%d errno=%d\n",
			asctim(),nsrc,errno);
		sleep(120);				/* limit failure rate */
		return 1;
	}
	fprintf(logout,"orbselect returned = %d\n",nsrc);
	return 0;
}

/*
	feedme returns a channel number in ICH, the decoded data in array ia,
	and the time of the first sample in ia int tc.  It will block until some
	new data is available.  For ORB this routine basically calls orbreap()
	decompresses the data using unstuffpkt, and converts the time code to
	nsntime format.  Since orbreap might return a packet with more that one
	channel in it, this routine buffers the packet and returns each channel
	on successive calls.  ORBREAP is called only when no data remains to be
	transferred.

	D.C. Ketchum Apr 1999
	date	description
	8/99	found but in "phoenix mode" channel assignement, last channel not
			being used so never translated.
			Added code to handle multiple channels per ORBREAP. Alaska data
			did not require this, but ORB at UCSD did return data that way.
			Added code to call ORBCLOSE, reopen when orbreap returned a -1
			with no other error.  It would seem ORBREAP returns every minute
			if no data is available, it is not clear whether the close/reopen
			is needed, but it certainly did work.

*/
feedme(int *ich, long ia[], struct nsntime *tc)
{	int	pktid;
	static double	timestamp;
	static int	bufsize = 0;		/* ORBREAP will maintain this value */
	static char 	*rawpkt = NULL;	/* ORBREAP will maintain this value */
	static char	*srcname;
	static char srcn[100];
	char name[20];
	extern struct chan_desc *ch;	/* connect to channel descriptions */
	extern int phmode;				/* are we in phoenix mode? */
	extern int MAX_CH;				/* number of channels in phoenix mode*/
	int	nbytes;						/* number of bytes in packet */
	int	rc;
	static struct Packet *pkt = NULL;/* maintained by UNSTUFFPKT */
	struct PktChannel *pktchan;
	static	int nchan;
	int 	err;
	static int	ichan;
	char	*s;
	static nrec=0;
	int mon,day,yr,hr,min,sec,ms,yrday,i;
	if(dbgorb) fprintf(logout,"srcname=%d ichan=%d nchan=%d fd=%d\n",
		srcname,ichan,nchan,orbfd);
	if(srcname == NULL) srcname=&srcn[0];
  	for(;;)							/* loop until some data is found */
 	{	
	/* If ichan < nchan, previous data has been read in.  Use it, else
		call orbreap to get more data */
		if(ichan >= nchan)
		{
			if(dbgorb) {fprintf(logout," orbrp %d bs=%d rpk=%d src=%d ",
				orbfd,bufsize,rawpkt,srcname); fflush(logout);}

			/* ORBREAP will return data or block until some data is available */
			nbytes=-1;
			errno=-1;
			rc = orbreap ( orbfd, &pktid, srcname, &timestamp,
			       &rawpkt, &nbytes, &bufsize );
			nrec++;
			if(dbgorb) 
			{	fprintf(logout,"| rc=%d rpk=%d nby=%d src=%d tell=%d",
					rc,rawpkt,nbytes,srcname,orbtell(orbfd)); 
				if(srcname != NULL) fprintf(logout," rcn=%s ",srcname);
				fprintf(logout,"\n");
					fflush(logout);
			}

			/* if orbreap return error, exit and let mother restart us */
			if(rc < 0)
			{	 fprintf(logout,
					"\n%s +++ Error in orbrp rc=%d tell=%d errno=%d = %s\n",
					asctim(),rc, orbtell(orbfd), errno, strerror(errno));
				if(rc == -1 && errno == -1) 
				{	if(orbclose(orbfd) < 0) 
						fprintf(logout,"orbclose errno=%d\n",errno);
					feedme_init(0,(char **) rawpkt);/* reopen use feedme_init*/
					continue;
				}
				clear_register( 1 );
				exit(101);
			}

			/* if the # of bytes is <= 0 or the srcname is not right, then 
				the packet must be broken.  Get another packet via continue */
			if(nbytes <= 0 || srcname == NULL || !isalpha(*srcname)) {/*o.k.? */
				fprintf(logout,
					"\n +++ Skip packet nby=%d scrname=%d *srcname=%d\n",
					nbytes,srcname,*srcname);
				continue;			/* nothing in packet */
			}
			if(dbgorb) {fprintf(logout," unstf "); fflush(logout);}	

			/* unstuff the packet into struct pkt */
			/* Mitch Robinson, 3-6-2002 */
			/* err = unstuffpkt( timestamp, srcname, rawpkt, &pkt ); */
			err = unstuffPkt( srcname, timestamp, rawpkt, nbytes, &pkt );
			if(err <= 0)  			/* packet did not unstuff */
			{	fprintf(logout,"Error unstuffing ORB packet=%d\n",err);
				fflush(logout);
			}
			else if (err == 1)		/* packet unstuffed correctly */
			{	nchan = pkt->nchannels;
				ichan=0;			/* init channel to process next */
				if(dbgorb) {fprintf(logout,"nchan=%d ",nchan);	fflush(logout);}
			} else if(err == 2)
			{	fprintf(logout,"Status packet returned from ORB=%d\n",err);
				fflush(logout);
			} else
			{	fprintf(logout,"ORBREAP returned unexpected value=%d\n",err);
				fflush(logout);
			}
		}						/* end if no data in buffer, need orbreap */

		/* if data is available (ichan < nchan), return it to vdl */
		if(ichan < nchan)
	        { /* Mitch Robinson, 3-6-2002 */
			/* pktchan = gettbl( pkt->chan, ichan ); */
			pktchan = gettbl( pkt->channels, ichan );
			ichan++;				/* point to next channel */
			if(dbgorb ) {fprintf(logout,"%s:%s:%s rt=%f ns=%d buf=%x\n",
				pktchan->net, pktchan->sta, pktchan->chan, pktchan->samprate,
				pktchan->nsamp, (int) pktchan->data); fflush(logout);}

			/* debug printout */
/*			if(dbgorb) {
				fprintf(logout,"Network-Station-channel %s:%s:%s\n", 
					pktchan->net, pktchan->sta, pktchan->chan );
				fprintf(logout,"\tSamplerate %f\n", pktchan->samprate );
				fprintf(logout,"\tNsamp %d\n", pktchan->nsamp );

				fprintf(logout,"\tStarttime (epoch time) %f\n", timestamp );
				fprintf(logout,"\tStarttime (readable) %s\n", 
					( s = strtime( timestamp ) ) );
				free( s );
				fprintf(logout,"\tEndtime (epoch time) %f\n",
				ENDTIME( timestamp,  pktchan->samprate, pktchan->nsamp ) );

				fprintf(logout,"\tAddr of 4-byte, Motorola byte-order int data: %x\n",
				(int) pktchan->data );

			}	
*/

			/* convert time code using maknsn */
			yrday=yearday(timestamp);			/* get year day */
			s = strtime(timestamp);
			sscanf(s,"%d/%d/%d %d:%d:%d.%d",&mon,&day,&yr,&hr,&min,&sec,&ms);
			free( s );
			*tc=maknsn(yrday/1000, (yrday % 1000), hr,min,sec,ms,0);
			if(dbgorb) {fprintf(logout,"time=%d %2d:%2d:%2d.%3d\n",
				yrday,hr,min,sec,ms); fflush(logout);}
/*			fprintf(logout,"%s:%s:%s %d %2d:%2d:%2d.%3d id=%d %d ns=%d\n",
				pktchan->net, pktchan->sta, pktchan->chan, yrday,hr,min,sec,ms,
				nrec,ichan,pktchan->nsamp);*/

			/* select channel based, in PHMODE use the parameter file */
			*ich=-1;
			if(phmode)							/* this mode is used mostly*/
			{	sprintf(name,"%s_%s_%s",		/* make up the name of station*/
					pktchan->net,pktchan->sta,pktchan->chan);
				for(i=0; i<= MAX_CH; i++)
				{	/*fprintf(logout,"Match %d %s to %s\n",i,ch[i].txt,name);*/
					if( strcmp(name,ch[i].txt) == 0) 
					{	*ich=i;		/* set channel # */
						break;
					}
				}
				if(*ich == -1) 
				{	fprintf(logout,"Did not find match for %s\n",name);
					continue;		/* skip unknown channel */
				}
			}
			else							/* Single station, seldom used*/
			{
				if      (strcmp(pktchan->chan,"BHZ")==0) *ich = CH_Z20;
				else if (strcmp(pktchan->chan,"BHN")==0) *ich = CH_X20;
				else if (strcmp(pktchan->chan,"BHE")==0) *ich = CH_Y20;
   				else if (strcmp(pktchan->chan,"HHZ")==0) *ich = CH_Z20;
				else if (strcmp(pktchan->chan,"HHN")==0) *ich = CH_X20;
				else if (strcmp(pktchan->chan,"HHE")==0) *ich = CH_Y20;
				else if (strcmp(pktchan->chan,"LHZ")==0) *ich = CH_Z1;
				else if (strcmp(pktchan->chan,"LHN")==0) *ich = CH_X1;
				else if (strcmp(pktchan->chan,"LHE")==0) *ich = CH_Y1;
				else continue;		/* Skip unknown channel.		*/
			}
			if(dbgorb) {fprintf(logout,"copy ns=%d ich=%d\n",
				pktchan->nsamp,*ich); 		
				fflush(logout);}

			/* move the data to user array */
			memcpy(ia,pktchan->data, pktchan->nsamp*sizeof(long));
			return (pktchan->nsamp);
		}			/* end if  there is a channel in buffer */
	}				/* end of infinite FOR loop */
}

feedme_shutdown()
{	int err;
	if( (err=orbclose(orbfd)) < 0) fprintf(logout,"Orbclose error=%d\n",err);
	return;
}
