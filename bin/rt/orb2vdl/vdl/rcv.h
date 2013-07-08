/*
	Include files with NSN definitions 

	Update History
	2-Oct-92	Changed Channel def to include MS_DELAY for filtering delays.	
	5-Nov-93	Simplified for SD version
	15-mar-96	Create version for sample code RCV/STATION.
*/

/*

	Define Common Structures .  NSNTIME is badly aligned on Suns since it
	realligns the LONG MS to an i*4 boundary with two unused bytes after doy
*/
struct nsntime {
	unsigned char iyr;	/* year-1970)*2.  Low order bit is 256s of days*/
	unsigned char doy;	/* day of year modula 256 */
	long ms;			/* the milleseconds since midnight *16 + flags */
};
struct nsntime qnsntim(),nsnadd(),buftim();	/* prototype some functions */
/*
	NSNCOMM packets using Steim compression have this data structure 
*/
struct nsncomm {
	unsigned char revision;				/* Quanterra packet revision level */
	unsigned char offset;				/* the offset to data frames */
	char samprate;						/* sample rate - if positive as is,*/
										/* if negative rate=1/(-samprate) */
	unsigned char soh;					/* state of health byte */
	short nsamp;						/* number of samples compressed in packet*/
	unsigned char control;				/* control 0 if normal, 1 if station reset*/
	unsigned char extra;				/* space filler to allign */
	unsigned char data[2020];			/* steim compressed data */
};
/*
	Definitions for the FLAGS within a Gomberg NSN record and GOMBERG STRUCT
*/
#define NSN_FLAGS_EOF 1
#define NSN_FLAGS_PARTIAL 8
#define NSN_FLAGS_EOR 16
#define NSN_FLAGS_CONTINUOUS 32
#define NSN_FLAGS_TRIGGER 64
#define NSN_FLAGS_POWER 32
struct gomberg {
	unsigned char lead1;			/* always 27 (esc) */
	unsigned char lead2;			/* always 3 (STX) */
	unsigned char numbyt[2];		/* byte reversed number of bytes */
	unsigned char routeid;			/* USNSN route ID */
	unsigned char nodeid;			/* Node ID for this Guy */
	unsigned char chanid;			/* Channel/station ID */
	unsigned char packseq;			/* The packet sequence ID*/
	unsigned char tc[6];				/* a USNSN time code */
	unsigned char format;			/* a format code */
	unsigned char flags;			/* data flags */
	unsigned char doy;				/* doy detection started */
	unsigned char seq;				/* the sequence number */
	unsigned char detseq[2];		/* detection sequence */
	union {
		unsigned char data[2028];
		struct nsncomm sti;			/* steim structure */
	} u;
} ;

/*
	Trigger/Channel parameters and DEFS 
*/
#define LOW_GAIN 32					/* add to Low Gain versions of chans*/
#define SH_CHAN 64					/* Add to channels that are SP */
#define CH_X20	0
#define CH_Y20	1
#define CH_Z20	2
#define CH_X40	0
#define CH_Y40	1
#define CH_Z40	2
#define CH_X1	3
#define CH_Y1	4
#define	CH_Z1	5
#define MAX_CH 	CH_Z1				/* Last channel # */
#define MAX_LCH CH_Z1				/* Some dimensioning uses this from USNSN*/
/*#include "cmprs.h"
/*
	Format /compression codes - gomberg field format.
*/
#define FORM_NSN 0
#define FORM_STEIM 1
#define FORM_12BIT 2
#define FORM_16BIT 3
#define FORM_24BIT 4
#define FORM_32BIT 5
#define FORM_RSTN 6
#define FORM_SRO 7
#define FORM_GSS 8
#define FORM_14BIT 9
#define FORM_STEIM2A 11
#define FORM_STEIM2 10				/* Some stations mistakenly send 10 for steim2*/
/* #define FORM_12BIT_DAC 10		/* NSN dac data uses this form */
#define FORM_STEIM3 12
/*
	If ANSI C, set function prototypes
*/
#ifdef __STDC__
	void rollback(int seq);			/* routine that sends a rollback */
	void sendit(struct gomberg *gb);/* Routine that send gb packet to a pipe */
	void cvtcmd(unsigned char *in, unsigned char *out, int nchar);/* convert cmds*/
	void cmdarg(int argc, char *argv[]);
 	int writeout(int ttin, char *buf, int len);
	int init_tcp(char*);					/* open TCP socket prototype */
	void init_tt(char *dev);			/* Open up serial device */
	int init_ttout(char *dev);			/* output on serial device */
	int read_modem(int ttpath, int *dtr, int *rts, int *cts, int *car, int *rng,
		 int *dsr);						/* read modem prototype */
	void user_proc(int iy, int id, int ih, int im, int is, int ms, int leap, 
		long idat[],int nsamp, char *name, char *cname, char *network,
		int eof, double rate,int seq);
	int get_seed(unsigned char route, unsigned char node, unsigned char chan,
		char *name,char *comp, char *network);
	int status_pack(struct gomberg *gb);/* user status packet routine */
	int nsn_chan(unsigned char chanid, char *name, double *rate);/* nsn channel name*/
	int iris_chan(unsigned char chanid, char *name);
#else
	void sendit();						/* Send it has no return */
	void rollback();					/* rollback returns a void */
	void cvtcmd();						/* cvtcmd returns no value */
	void cmdarg();						/* argument processor returns no value */
	void init_tt();						/* initialize serial port */
	void dcon_steimi();					/* ditto */
	void user_proc();					/* user routine returns nothing */

#endif
/**********************************************************************
			E N D   O F   N S N . H
***********************************************************************/

