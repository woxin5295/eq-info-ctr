/*
	Start : VDL.H 
	Include files with NSN definitions 

	Update History
	2-Oct-92	Changed Channel def to include MS_DELAY for filtering delays.	
	5-Nov-93	Simplified for SD version
*/
#ifndef VDLFILT
#include "vdlfilt.h"
#endif

#define MAX_NSAMP_IN 4096/*

	Define Common Structures 
*/
struct nsntime {
	unsigned char iyr;	/* year-1970)*2.  Low order bit is 256s of days*/
	unsigned char doy;	/* day of year modula 256 */
	long ms;			/* the milleseconds since midnight *16 + flags */
};
/*
	Definitions for the FLAGS within a Gomberg NSN record and GOMBERG STRUCT
*/
#define NSN_STATUS 2
#define NSN_FLAGS_EOF 1
#define NSN_FLAGS_PARTIAL 8
#define NSN_FLAGS_EOR 16
#define NSN_FLAGS_CONTINUOUS 32
#define NSN_FLAGS_TRIGGER 64
#define NSN_FLAGS_POWER 32
struct nsntime qnsntim(),nsnadd(),buftim();	/* prototype some functions */
	struct gomberg {
		unsigned char lead1;			/* always 27 (esc) */
		unsigned char lead2;			/* always 3 (STX) */
		unsigned char numbyt[2];		/* byte reversed number of bytes */
		unsigned char routeid;			/* USNSN route ID */
		unsigned char nodeid;			/* Node ID for this Guy */
		unsigned char chanid;			/* Channel/station ID */
		unsigned char packseq;			/* The packet sequence ID*/
		struct nsntime tc;				/* a USNSN time code */
		unsigned char format;			/* a format code */
		unsigned char flags;			/* data flags */
		unsigned char doy;				/* doy detection started */
		unsigned char seq;				/* the sequence number */
		unsigned char detseq[2];		/* detection sequence */
		unsigned char buf[2028];

	} ;
/*
	Trigger/Channel parameters and DEFS 
*/
/*
	Trigger/Detector parameters
*/
#define NPOW 8			/* Number of pow frequency windows maximum */
#define NHIST 5			/* Number of Histories to maintaine */
#define NHIST1 NHIST+1	/* Ditto plus one */
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

#include "cmprs.h"
struct chan_desc {
	int nsamp_qd;					/* number of samples per Quanterra buffer*/
	int trigdown;					/* Counts down post memory in samples */
	int trigflag;					/* for continuous test counts down trigger*/
	int trigdead;					/* Set by NEWTRIG to point at which history*/
									/* updates begin again. */
	int trigabshutdown;				/* Number of samples forcing shutdown */
	struct cmprfr memex;			/* compression variable for this channel*/
/*	unsigned*/ void *cmp;				/* compression buffer space */
	struct nsntime cmptc;			/* time at beginning of current cmprs buf*/
	int trigtotal;					/* counter of total samples compressed */
	int continuous;					/* if true, channel is continuous send */
	int fftpnt;						/* if this is the trigger channel, next FFT pnt*/
	unsigned char stat_chan;		/* station/channel byte for this channel */
	unsigned char seq;				/* sequence number within this component */
	unsigned char doy;				/* day of year detection started */
	char txt[20];
	short detect_seq;						/* detection sequence last assigned */
	unsigned char detseq[2];		/* the two byte detection sequence */
	int cmptotal;					/* number of samples compressed in full buf
									this value is updated in putbuf by number
									of samples in each compression frame.  This
									less the starting ring buffer offset allows
									us to track the time of each compressed buf.*/
	int cmpstart;					/* ring buffer offset of 1st sample in 
									compression (needed for time computation)*/
	int cmppnt;						/* pointer to next point in ring to compress*/
	int lastpartial;				/* Last data byte updated via partial update
									Used to compute how much new data to send*/
	int partial;					/* Flag, if non-zero include in partial 
									update loop*/
	long *ring;						/* ring buffer */
	int ipnt;						/* ring buffer pointer */
	struct nsntime tclastfeed;		/* tc of last feed */
	int max;						/* number of samples in ring buffer */
	struct nsntime tc;				/* time code of beginning of buffer */
	long *spfilt;					/* filter memory for digit rate to internal*/
	int lppnt;						/* pointer in ring buffer for doing LP */
	int delay;						/* milleseconds to subtract from time for group
											delay of channel */
	int warmup;						/* warmup counter for history */
	int trig_chan;					/* if non-zero, this is a trigger chan */
	int forward[3];					/* additional channels to forward on trig*/
	int derive_lp;					/* if non-zero, derive LP to this chan*/
	struct decimate lpfilt;			/* filter for LP data */
	struct decimate decim_filt;		/* filter for main decimation data */
	long *sphist;					/* unused samples between feeds*/
	int decim_off;					/* number of samp in sphist between feeds*/
	int decimate;					/* if not one, decimate the channel */
	int freq;						/* frequency of this channel in Hz */
	long hist[NPOW][NHIST1];		/* history for trigger on this channel*/
	int nold[NPOW][NHIST];			/* aging count down for histories */
	int ncoin;						/* number of coincident power for trigger*/
	int triggered;					/* is this channel currently triggered? */
	int nretired;					/* aging time for history */
	int trigfreq[10];				/* trigger windows to trigger on */
	int nfreq;						/* number of freqs in trigfreq */
	int expbias;					/* bias to apply to powers to scale them */
	double sntrig,hfsntrig;			/* signal to noise triggering ratio */
	int showdet;					/* show detections on this channel */
	int sendpows;					/* if true, send powers from this channel*/
	double freq_calc;				/* calculated digit rate */
	int lastisn;					/* signal to noise max during trigger */
	struct decimate helifilt;		/* helicorder filter */
	int heli;						/* chan # to use for heli data derived from
										this channel*/
	int helipnt;					/* pointer to place in heli buffer to
										next start deriving the heli */
	short *helibuf;					/* buffer for storing heli data */
	unsigned char heliseq;			/* sequence for this channel of heli */
	int heligain;					/* attenuation to apply to heli */
} ;
/*
	FFT parameters
*/
#ifdef __STDC__
void cmdarg(int argc, char *argv[], char *ttdev);
/* vdlput.c */
  int compressit(struct chan_desc *ch);
  int contrg(int detect_seq, struct nsntime tc, struct chan_desc *);
/* vdltt.c */
  void init_tt(char *dev);

#endif
/***********************************************************************
			E N D   O F   V D L . H
***********************************************************************/

