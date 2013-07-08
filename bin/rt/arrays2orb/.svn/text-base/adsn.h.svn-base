/*.h
 *
 * NAME: adsn.h
 *
 * PURPOSE: header for calibration and seismic extracted data as well as css 
 *          wfdisc formatting commands/structures.
 *
 * AUTHOR: Mark Richards 
 *
 * DATE: December 1990
 *
 */

#ifndef ADSN_H
#define ADSN_H

static int  scale[8] = {1, 4, 16, 64, 256, 1024, 4096, 16384};

typedef struct	/*used to read the recipe files for FrameA and FrameB*/
  {		/*matched with the s_wfdisc.h version of Wfdisc*/
    char sta[7];
    char chan[3];
    float samprate;
    float calib;
    float calper;
    char instype[7];
    char segtype[2];
    char datatype[3];
    long chanid;
  } Recipe_record;

/*newly added to this file, from the 890 version of network.h*/
typedef struct
  {
    unsigned char * data_loc;
    float smprat;
    char wfdisc_dattyp[3];
    int ct_dattyp;
    int comm_pos;
    FILE *w_fptr;
  } W_info;

#define GAIN_CODE_STRING "g2"
#define MAX_CHIDS       70              /* there are 69 in FrameB */

#define WFDISC_30_NF 20
#define WFDISC_28_NF 19



/** read control string for scanf on a wfdisc 3.0 record **/
#define WFDISC_30_RCS "%6c%*c%8c%lf%ld%ld%ld%lf%ld%f%f%f%*c%6c%*c%c%*c%2c%*c%c %s %s %ld%ld%*c%17c%*c"

/** read value list for scanf on a wfdisc 3.0 record */
#define WFDISC_30_RVL(SP) \
	(SP)->sta,(SP)->chan,&(SP)->time,&(SP)->wfid,&(SP)->chanid,\
	&(SP)->jdate,&(SP)->endtime,&(SP)->nsamp,&(SP)->samprate,&(SP)->calib,\
	&(SP)->calper,(SP)->instype,(SP)->segtype,(SP)->datatype,(SP)->clip,\
	(SP)->dir,(SP)->dfile,&(SP)->foff,&(SP)->commid,(SP)->lddate

/** write control string for printf on a wfdisc 3.0 record **/
#define WFDISC_30_WCS "%-6s %-8s %17.5f %8d %8d %8d %17.5f %8d %11.7f %16.6f %16.6f %-6s %1s %-2s %1s %-64s %-32s %10d %8d %-17s\n"

/** write value list for printf on a wfdisc 3.0 record */
#define WFDISC_30_WVL(SP) \
	(SP)->sta,(SP)->chan,(SP)->time,(SP)->wfid,(SP)->chanid,\
	(SP)->jdate,(SP)->endtime,(SP)->nsamp,(SP)->samprate,(SP)->calib,\
	(SP)->calper,(SP)->instype,(SP)->segtype,(SP)->datatype,(SP)->clip,\
	(SP)->dir,(SP)->dfile,(SP)->foff,(SP)->commid,(SP)->lddate


/** read control string for scanf on a wfdisc 2.8 record **/
#define WFDISC_28_RCS "%ld%lf%*c%6c%*c%2c%ld%f%f%f%*c%6c%*c%c%*c%2c%*c%c%ld%ld %s %s %ld%ld%*c%30c"

/** read value list for scanf on a wfdisc 2.8 record **/
#define WFDISC_28_RVL(SP) \
        &(SP)->jdate,&(SP)->time,(SP)->sta,(SP)->chan,&(SP)->nsamp,\
        &(SP)->smprat,&(SP)->calib,&(SP)->calper,(SP)->instyp,(SP)->segtyp,\
        (SP)->dattyp,(SP)->clip,&(SP)->chid,&(SP)->wfid,(SP)->dir,\
        (SP)->dfile,&(SP)->foff,&(SP)->adate,(SP)->remark


/** macro for adding null to end of strings read with the 3.0 RCS and RVL **/
#define WFDISC_30_NULL(SP) \
    (SP)->sta[6]='\0'; (SP)->chan[8]='\0'; (SP)->instype[6]='\0'; \
    (SP)->segtype[1]='\0'; (SP)->datatype[2]='\0'; (SP)->clip[1]='\0'; \
    (SP)->dir[64]='\0'; (SP)->dfile[32]='\0'; (SP)->lddate[17]='\0';



/** macro for adding null to end of strings read with the 2.8 RCS anf RVL **/
#define WFDISC_28_NULL(SP) \
    (SP)->sta[6]='\0'; (SP)->chan[2]='\0'; (SP)->instyp[6]='\0'; \
    (SP)->segtyp[1]='\0'; (SP)->dattyp[2]='\0'; (SP)->clip[1]='\0'; \
    (SP)->dir[30]='\0'; (SP)->dfile[20]='\0'; (SP)->remark[30]='\0';



/** macro to trim the wfdisc 3.0 records before inserting into oracle **/
#define WFDISC_30_TRIM(SP) \
	TRIM((SP)->sta,7); TRIM((SP)->chan,9); TRIM((SP)->instype,7); \
	TRIM((SP)->segtype,2); TRIM((SP)->datatype,3); TRIM((SP)->clip,2); \
	TRIM((SP)->dir,65); TRIM((SP)->dfile,33); TRIM((SP)->lddate,18)








/*this control string reads the science horizons wfdisc and does'nt pad 
  strings with blanks.  This will not work if there are blanks separating 
  pieces that make up a particular string in the data */
/*#define WFDISC_28_RCS "%ld %lf %s %s %ld %f %f %f %s %s %s %s %ld %ld %s %s %ld %ld %s"*/


#endif


 

