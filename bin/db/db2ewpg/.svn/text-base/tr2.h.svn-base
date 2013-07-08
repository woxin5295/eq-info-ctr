
#ifndef __tr__
#define __tr__

#include "db.h"
#include "stock.h"
#include "msd.h"
#include "ant_steim.h"

#ifdef	__cplusplus
extern "C" {
#endif

typedef float Trsample ;

/* tr data codes */
#define trInternalInt   1
#define trInternalShort 2
#define trInternalFloat 3
#define trFLOAT  	4
#define trSHORT  	5
#define trINT	 	6
#define trSEED   	7
#define trGAIN_RANGE 	8
#define trIDA_2BYTE 	9
#define trIDA_4BYTE 	10
#define trASCII		11
#define trINTELFLOAT	12
#define trINTELSHORT	13
#define trINTELINT	14
#define trDISCARD	15
#define trGSE_CM6	16
#define trSAC		19
#define trAH		20
#define trAAH		21
#define trINTEL3BYTE    22
#define tr3BYTE		23
#define trREFTEK	24
#define trISAC		25
#define trCA		26
#define trFLATLINE	27
#define trSTEIM1   	28
#define trUNEVEN	29

#define trByCHAN 	1
#define trBySTA  	2
#define trByEVENT  	3
#define trByNETWORK	4

/* bits returned by i/tr2ext() */
#define TR_CLIPPED      2	/* indicates output data type doesn't have enough bits to represent internal data */
#define TR_TRUNCATED    1	/* indicates non-integer input */
#define TR_GAPS		8	/* indicates marked gap in data */

/* trsave(3) bits in flag */
#define trOVERWRITE 	 (1<<0)	/* overwrite existing waveform file */
#define trAPPEND 	 (1<<1)	/* append to any existing waveform file */
#define trAVOID_OVERLAP  (1<<2)	/* require that database row not conflict 
					with existing rows */
#define trUSE_LOCKS      (1<<3)   /* lock output files */
#define trFLUSH	         (1<<4)   /* flush output files */
#define trOLD_WFFILE	 (1<<5)	  /* use existing dir/dfile inside trwfnew */
#define trOLD_WFDISC	 (1<<6)	  /* overwrite existing wfdisc record inside trwfnew */
#define trNEW_WFFILE	 (1<<7)	  /* generate new wfname inside trwfnew if previous already exists */

/* structure to hold fill values, and representation limits for */
/* different datatypes */
typedef struct Wftype { 
    char 	datatype[4] ; 	/* datatype following wfdisc.datatype conventions */
    int	        code ; 		/* corresponding integer value for use in programs */
    double	fill ; 		/* default fill value to indicate missing data */
    double 	lower ; 	/* test for missing value is: */
    double 	upper ;    	/*   value <= lower || value >= upper */
    int	        bytesPerSamp ;  /* # bytes/sample if constant, otherwise zero */
    char *wfin ;		/* name of routine to read input data */
    char *wfwhdr ;		/* name of routine to write output header */
    char *wfout ;		/* name of routine to write output data */
    char *wftail ;		/* name of routine to write output tail (checksum for GSE_CM6) */
    int  appendable ; 		/* non-zero if wfout may be called repeatedly. */
    char *desc ;		/* short description of data type */
} Wftype ;

typedef struct Trsegment
{
    int             first,
                    last;
}               Trsegment;

typedef struct Trbuffer
{
    Trsample *data ; 
    double t0, t1 ; 
    int	nsamp; 
    Dbptr db ; 
} Trbuffer ;
	    
typedef struct Trseg {
    char sta[25], chan[25] ;
    double time, endtime, samprate, calib ; 
    int nsamp ;
    Trsample *data ;
    Dbptr tr ;
} Trseg ;

#define SAMP2TIME(TIME,SAMPRATE,ISAMP) ((TIME) + ((ISAMP))  /(SAMPRATE))
#define ENDTIME(TIME,SAMPRATE,NSAMP)   ((TIME) + ((NSAMP)-1)/(SAMPRATE))
#define TIME2SAMP(TIME,SAMPRATE,TIME1) floor(((TIME1)  -  (TIME))*(SAMPRATE) + 0.5)
#define NSAMP(TIME,SAMPRATE,ENDTIME)   floor(((ENDTIME) - (TIME))*(SAMPRATE) + 1.5)
#define SAMPRATE(TIME,NSAMP,ENDTIME)   (((NSAMP)-1)/((ENDTIME)-(TIME)))

#define TRCONTIGUOUS(T0,T1,R0,N0)       (ABS(((T1)-(T0))*(R0)-(N0))< trTOLERANCE)
#define TRSAMERATE(R0,R1)       (ABS(1.0-(R0)/(R1)) < trSAMPRATE_TOLERANCE)
#define TRSAMETICKS(T0,T1,R0)   (ABS( (((T1)-(T0))*(R0))-(floor(((T1)-(T0))*(R0) + 0.5)) ) < trTOLERANCE)
#define TRSAMETIME(T0,T1,R0) 	((ABS((T0)-(T1))*(R0)) < trTOLERANCE)

#define TRBADSEED_VALUE (268435456.)
#define TRGAP_VALUE     268435456
#define TRGAP_VALUE_S4  2147483647
#define CSS3_NSAMP_LIMIT 	99999999

#define DT_CSS          .000005	/* uncertainty in time from css tables */
#define DT_SEED         .00005  /* uncertainty in time from miniseed */
#define DT_TRUNC        (DT_CSS+DT_SEED)
#define MSDSAMETIME(T0,T1,R0) 	(((ABS((T0)-(T1))-DT_TRUNC)*(R0)) < trTOLERANCE)

typedef struct Wfdisc {
    char net[32] ;			/* network name : network and loc are used only in msd2wf */
    char sta[32] ;			/* station name */
    char chan[32] ;			/* channel name */
    char loc[32] ; 			/* loc code */
    double from, until ; 	        /* time range for output should be time <= t < until */
    double time ; 			/* starting time for segment */
    double samprate ; 			/* samprate in wfdisc */
    double calib ;			/* calib in wfdisc */
    double calper ;			/* calper in wfdisc */
    int nsamp ; 			/* current nsamp in output */
    int checksum ; 			/* cumulative checksum for GSE_CM6 output */
    char segtype[4] ; 			/* response type in wfdisc */
    char instype[8] ; 			/* instrument type in wfdisc */
    char *wfname ;			/* trwfname(3) pattern for output data files */
    Wftype *wftype ;			/* output data type */
    Msd *msd ;				/* miniseed initialization */
    Hook *hook ; 			/* hook for private info */
    off_t foff ; 				/* original offset for data in output file */
    char path[FILENAME_MAX] ;		/* pathname for output */
    char  dir[68] ; 
    char  dfile[36] ;
    int	  fd ; 				/* if > 0, open file descriptor */
    char *wf ; 				/* output buffer */
    int   wf_nbytes ;			/* # of bytes in output buffer */
    int   wf_nsamp ; 		        /* # of sample in wf output buffer */
    int   wf_size ;			/* size of output buffer when buffering data */
    Dbptr db ;				/* output database row */
    Dbptr dbhdr ;			/* db pointer from which to get lat, lon, etc for header */
    int flags ; 			/* flags for specifying behavior of writing routines */
} Wfdisc ;




#ifdef	__cplusplus
}
#endif

#endif

/* $Id: tr2.h,v 1.1.1.1 2008-01-29 22:34:42 glenn Exp $ */
