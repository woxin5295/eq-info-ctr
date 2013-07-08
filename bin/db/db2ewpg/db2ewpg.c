/*
 *    This program uses numerous routines from the program k2evt2ew.c
 *    revision 1.10 2007/08/03 by Jim Luetgert
 *
 *    k2evt2ew.c provides is used to calculate peak ground displacements,
 *    velocities, and accelerations, which are then used by the ShakeMap
 *    system, and possibly others.
 *
 *    Since AEIC does not use the Earthworm system, it needed to create its
 *    own interface to these subroutines
 *    The idea is that there should be a call to this program within
 *    db2sm_xml, which is called by retrieve whenever shake is called by shake_watch
 *
 *    This program needs to do the following things:
 *    (1) Load data for the input station/channel between tstart and tstart+twin from wfdb
 *        - to accomplish this I call trloadchan()
 *    (2) Apply instrument corrections (Roger wants calibration rather than deconvolution)
 *    (3) Call int_peak_ground(float *Data, int npts, int itype, float dt, SM_INFO *sm)
 *    (4) Output the data from sm structure into a wfmeas table
 * 
 *    db2sm_xml would then read the wfmeas table, and write XML 
 *
 *    Glenn Thompson (GTHO), AEIC, December 2007  
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>
#include "rw_strongmotionII.h"

/* Added from trload.c */
#include <unistd.h>
#include <math.h>
#include <tr.h>

/* GTHO - assuming we don't need the following */
/*
#include <earthworm.h>
#include <chron3.h>
#include <kom.h>
#include <k2evt2ew.h>
*/

#define GRAVITY 9.810      /* Gravity in cm/sec/sec */
/* GTHO */
#define MAXTRACELTH 120000


/* Functions in this source file
 *******************************/
int peak_ground(float *Data, int npts, int itype, float dt, SM_INFO *sm);
void demean(float *A, int N);
void locut(float *s, int nd, float fcut, float delt, int nroll, int icaus);
void rdrvaa(float *acc, int na, float omega, float damp, float dt,
            float *rd, float *rv, float *aa);
void amaxper(int npts, float dt, float *fc, float *amaxmm, 
	    float *aminmm, float *pmax, int *imin, int *imax);
void savetimeseries(int npts, float *data, char *filename); 

/* GTHO - Added main routine since we wish to call this program directly */
/* GTHO - based on trload.c */

static void usage ()
{
    cbanner ( "$Date: 2008-05-21 18:44:44 $", 
	     "database time endtime sta chan",
	     "BRTT example", 
	     "BRTT", 
	     "support@brtt.com" ) ; 
    exit (1);
}

int main (int argc, char **argv)
{
    Dbptr db, tr, dbout;
    FILE *fo;
//    char *database;
	char database[99], wfmeasdb[99], filter[30];
    double t0, t1, twin, timenow, spectral_acceleration ; /* epoch times */
    char t0str[25], t1str[25], meastype[6];
    char sta[6], chan[6], rsptype[6], expr[255], sta2[6], chan2[6], units[12];
    int	i;
    int samprate;
    Tbl *fields;
    int nrecords;
    int r;
    SM_INFO sm; // defined in rw_strongmotionII.h
    int npts;
    int itype;
    float dt;
    int pg; // return variable
    float calib, calper;
    int test;
// This step is crucial: must allocate memory for the array
    float *ptr2data, darray[MAXTRACELTH];
// Must create a pointer to the array to pass to other subroutines 
    ptr2data=darray;
// Doesn't seem that initialising to zero matters - but doesn't hurt
    for (i=0;i<MAXTRACELTH;i++) *(ptr2data+i)=0.0;

    /* output the usage if not correct number of input arguments */
    if (argc != 10) {
	printf("argc = %d\n", argc);
	usage ();
    }

    /* Read variables from the command line */
    sprintf(database,argv[1]);
    t0 = atof(argv[4]);
    t1 = atof(argv[5]);
    twin = t1 - t0;
    timenow = now();
    sprintf(sta,"%s",argv[2]);
    sprintf(chan,"%s",argv[3]);
    strcpy(t0str,epoch2str(t0, "%G %T"));
    strcpy(t1str,epoch2str(t1, "%G %T"));
    calib = atof(argv[6]);
    samprate = atoi(argv[7]);
    itype = atoi(argv[8]);
    sprintf(wfmeasdb,argv[9]);


    /* Display information from the command line */
    printf("Command line arguments:\n");
    printf("Database: %s\nStart Time: %s\nEnd Time: %s\nStation: %s\nChannel: %s\n", database, t0str, t1str, sta, chan);
    printf("Calib: %e\nsamprate: %d\nitype: %d\n", calib, samprate, itype);

    /* Open the waveform database */
    if ( dbopen(database, "r", &db) ) { 
        die ( 1, "Can't open database %s\n", database ) ;
    }

    /* Call trloadchan to create a trace structure and a pointer to it */
    tr = trloadchan ( db, t0, t1, sta, chan );

    /* Find out how many records are in this transient trace table */
    dbquery( tr, dbRECORD_COUNT, &nrecords ) ; 

    if ( nrecords == 1 ) {
	printf("\n1 data record loaded with trloadchan().\n\n");
	tr.record = 0;   
        dbgetv(tr, 0, "data", &ptr2data, 0 ) ;
        dbgetv(tr, 0, "nsamp", &npts);
        //savetimeseries(npts, ptr2data, "data0"); // save the raw data to a file if desired for checking with Matlab

	/* Apply calibration */
    	for (i=0;i<=npts;i++) *(ptr2data+i) *= calib;

	/* Create necessary variables for the SM_INFO structure */
	strcpy(sm.sta, sta);
	strcpy(sm.comp, chan);
	strcpy(sm.net, "AEIC");
	sm.t = t0;

        /* calculate the sampling frequency in Hz, and the samping period dt in s */
        samprate = round ((npts - 1) / (t1 - t0)) ;
        dt = (1 / (float) samprate);

        /* check values before passing */
        printf("Passing these values to peakground():\nnpts = %d, itype = %d, dt = %f\n\n", npts, itype, dt);

	/* Call peak_ground */
        peak_ground(ptr2data, npts, itype, dt, &sm);

	/* Display returned peak ground measurements for this data window */
	printf("\nPeak amplitudes of displacement, velocity and acceleration seismograms:\n");
        printf("PGD = %e, PGV = %e, PGA = %e\n\n", sm.pgd, sm.pgv, sm.pga);

	/* Display spectral ground accelerations */
	printf("Spectral acceleration:\n");
	for (i=0;i<sm.nrsa;i++)
        	printf("Period = %f, PGA = %e\n", sm.pdrsa[i], sm.rsa[i]);

	/* Save values to wfmeas table */
   	/* Open the waveform database */
    	if ( dbopen(wfmeasdb, "r+", &dbout) != 0) { 
        	die ( 1, "Can't open database %s\n", wfmeasdb ) ;
    	}
//	sprintf(expr, "touch %s.wfmeas", wfmeasdb);
//	system(expr);

	strcpy(filter, "BW 0.1 5 0 0");
	printf("itype = %d\n",itype);
	if (itype < 3) {
		sprintf(filter, "%s ; DIF",filter);
	}

	timenow = (double) now ();
	dbout = dblookup( dbout, 0, "wfmeas", 0, 0 );

	/* Turn measurements into mg & log them to a file */
	for (i=0;i<sm.nrsa;i++) {
		spectral_acceleration = sm.rsa[i] / GRAVITY * 1000;
		sprintf(meastype, "psa%02.0f", sm.pdrsa[i] * 10);
        	printf("meastype = %s, spectral_acceleration = %e\n", meastype, spectral_acceleration);

		dbout.record = dbaddnull (dbout);

		if(dbputv(dbout, 0,
			"sta", sta, 
			"chan", chan,
			"meastype", meastype,  
			"filter", filter,
			"time", t0, 
			"endtime", t1, 
		//	"tmeas", 0.0, 
			"twin", twin,
			"val1", spectral_acceleration,
		//	"val2", sm.pdrsa[2],
			"units1", "mg",
		//	"units2", "nm.s",
		//	"arid", 999,
			"auth", "db2ewpg",
//			"lddate", timenow,
			0) 
			== dbINVALID) {
			die( 1, "Could not write record to database %s\n", wfmeasdb);
		}

	}
	

    }
    else
    {
	printf("There are %d records for %s %s: needed exactly 1 to proceed\n",nrecords,sta,chan);
    }

    /* Free pointers */
//    trfree(tr);
    dbclose(db);

    /* return no error */
    return 0;
}



/******************************************************************************
 *   subroutine for estimation of ground motion                               *
 *                                                                            *
 *   input:                                                                   *
 *           Data   - data array                                              *
 *           npts   - number of points in timeseries                          *
 *           itype  - units for timeseries. 1=disp 2=vel 3=acc                *
 *           dt     - sample spacing in sec                                   *
 *   return:                                                                  *
 *           0      - All OK                                                  *
 *           1      - error                                                   *
 *                                                                            *
 ******************************************************************************/
int peak_ground(float *Data, int npts, int itype, float dt, SM_INFO *sm)
{

    //savetimeseries(npts, Data, "data1");

    int     imax_acc, imax_vel, imax_dsp, imax_raw;
    int     imin_acc, imin_vel, imin_dsp, imin_raw;
    int     ii, kk, kpts, id[4], npd[4][2], icaus;
    float  totint, a, tpi, omega, damp, rd, rv, aa;
    float  amax_acc, amin_acc, pmax_acc;
    float  amax_vel, amin_vel, pmax_vel;
    float  amax_dsp, amin_dsp, pmax_dsp;
    float  amax_raw, amin_raw, pmax_raw, gd[4], sd[4];

    float lowcutfreq;
    int numpoles;

    /* Made these float arrays static because Solaris was Segfaulting on an
     * allocation this big from the stack.  These currently are 3.2 MB each 
     * DK 20030108
     ************************************************************************/
	  static float    d1[MAXTRACELTH];
	  static float    d2[MAXTRACELTH];
	  static float    d3[MAXTRACELTH];

    gd[0] = 0.05;
    gd[1] = 0.10;
    gd[2] = 0.20;
    gd[3] = 0.50;
    icaus = 1; //# originally 1
    numpoles = 5; //# originally 2
    lowcutfreq = 0.1; //# originally 0.17

    tpi  = 8.0*atan(1.0);

/* GTHO - check passed values */
//    	printf("npts = %d, itype = %d, dt = %f\n", npts, itype, dt);

/* Find the raw maximum and its period
 *************************************/
    demean(Data, npts);
    amaxper(npts, dt, Data, &amin_raw, &amax_raw, &pmax_raw, &imin_raw, &imax_raw);
    //savetimeseries(npts, Data, "data2");

    if(itype == 1) {  /* input data is displacement  */
        for(kk=0;kk<npts;kk++) d1[kk] = Data[kk];
        locut(d1, npts, lowcutfreq, dt, numpoles, icaus);
        for(kk=1;kk<npts;kk++) {
            d2[kk] = (d1[kk] - d1[kk-1])/dt;
        }
        d2[0] = d2[1];
        demean(d2, npts);
        for(kk=1;kk<npts;kk++) {
            d3[kk] = (d2[kk] - d2[kk-1])/dt;
        }
        d3[0] = d3[1];
        demean(d3, npts);
    } else
    if(itype == 2) {  /* input data is velocity      */

        for(kk=0;kk<npts;kk++) d2[kk] = Data[kk];
        locut(d2, npts, lowcutfreq, dt, numpoles, icaus);
        //savetimeseries(npts, d2, "data3");

        for(kk=1;kk<npts;kk++) {
            d3[kk] = (d2[kk] - d2[kk-1])/dt;
        }
        d3[0] = d3[1];
        //savetimeseries(npts, d3, "data4");

        demean(d3, npts);
        //savetimeseries(npts, d3, "data5");


        totint = 0.0;
        for(kk=0;kk<npts-1;kk++) {
            totint = totint + (d2[kk] + d2[kk+1])*0.5*dt;
            d1[kk] = totint;
        }
        d1[npts-1] = d1[npts-2];
        demean(d1, npts);
        //savetimeseries(npts, d1, "data6");

    } else
    if(itype == 3) {  /* input data is acceleration  */
        for(kk=0;kk<npts;kk++) d3[kk] = Data[kk];
        locut(d3, npts, lowcutfreq, dt, numpoles, icaus);

        totint = 0.0;
        for(kk=0;kk<npts-1;kk++) {
            totint = totint + (d3[kk] + d3[kk+1])*0.5*dt;
            d2[kk] = totint;
        }
        d2[npts-1] = d2[npts-2];
        demean(d2, npts);

        totint = 0.0;
        for(kk=0;kk<npts-1;kk++) {
            totint = totint + (d2[kk] + d2[kk+1])*0.5*dt;
            d1[kk] = totint;
        }
        d1[npts-1] = d1[npts-2];
        demean(d1, npts);
    } else {
        return 1;
    }

/* Find the displacement(cm), velocity(cm/s), & acceleration(cm/s/s) maxima  and their periods
 *********************************************************************************************/

    amaxper(npts, dt, &d1[0], &amin_dsp, &amax_dsp, &pmax_dsp, &imin_dsp, &imax_dsp);
    amaxper(npts, dt, &d2[0], &amin_vel, &amax_vel, &pmax_vel, &imin_vel, &imax_vel);
    amaxper(npts, dt, &d3[0], &amin_acc, &amax_acc, &pmax_acc, &imin_acc, &imax_acc);


/* Find the spectral response
 ****************************/

    damp = 0.05;
    kk = 0;
    sm->pdrsa[kk] = 0.3;
    omega = tpi/sm->pdrsa[kk];
    rdrvaa(&d3[0], npts-1, omega, damp, dt, &rd, &rv, &aa);
    sm->rsa[kk] = aa;
    kk += 1;

    sm->pdrsa[kk] = 1.0;
    omega = tpi/sm->pdrsa[kk];
    rdrvaa(&d3[0], npts-1, omega, damp, dt, &rd, &rv, &aa);
    sm->rsa[kk] = aa;
    kk += 1;

    sm->pdrsa[kk] = 3.0;
    omega = tpi/sm->pdrsa[kk];
    rdrvaa(&d3[0], npts-1, omega, damp, dt, &rd, &rv, &aa);
    sm->rsa[kk] = aa;
    kk += 1;

    sm->nrsa = kk;



/* Since we are here, determine the duration of strong shaking
 *************************************************************/

    for(kk=0;kk<4;kk++) {
        id[kk] = npd[kk][1] = npd[kk][2] = 0;
        for(ii=1;ii<=npts-1;ii++) {
            a = fabs(d3[ii]/GRAVITY);
            if (a >= gd[kk]) {
                id[kk] = id[kk] + 1;
                if (id[kk] == 1) npd[kk][1] = ii;
                npd[kk][2] = ii;
            }
        }
        if (id[kk] != 0) {
            kpts = npd[kk][2] - npd[kk][1] + 1;
            sd[kk] = kpts*dt;
        } else {
            sd[kk] = 0.0;
        }
    }

    sm->pgd = fabs(amin_dsp)>fabs(amax_dsp)? fabs(amin_dsp):fabs(amax_dsp);
    sm->pgv = fabs(amin_vel)>fabs(amax_vel)? fabs(amin_vel):fabs(amax_vel);
    sm->pga = fabs(amin_acc)>fabs(amax_acc)? fabs(amin_acc):fabs(amax_acc);

    sm->tpgd = fabs(amin_dsp)>fabs(amax_dsp)? sm->t + dt*imin_dsp:sm->t + dt*imax_dsp;
    sm->tpgv = fabs(amin_vel)>fabs(amax_vel)? sm->t + dt*imin_vel:sm->t + dt*imax_vel;
    sm->tpga = fabs(amin_acc)>fabs(amax_acc)? sm->t + dt*imin_acc:sm->t + dt*imax_acc;

    return 0;
}


/******************************************************************************
 *  demean removes the mean from the n point series stored in array A.        *
 *                                                                            *
 ******************************************************************************/
void demean(float *A, int n)
{
    int       i;
    float    xm;

    xm = 0.0;
    for(i=0;i<n;i++) xm = xm + A[i];
    xm = xm/n;
    for(i=0;i<n;i++) A[i] = A[i] - xm;
}


/******************************************************************************
 *  Butterworth locut filter order 2*nroll (nroll<=8)                         *
 *   (see Kanasewich, Time Sequence Analysis in Geophysics,                   *
 *   Third Edition, University of Alberta Press, 1981)                        *
 *  written by W. B. Joyner 01/07/97                                          *
 *                                                                            *
 *  s[j] input = the time series to be filtered                               *
 *      output = the filtered series                                          *
 *      dimension of s[j] must be at least as large as                        *
 *        nd+3.0*float(nroll)/(fcut*delt)                                     *
 *  nd    = the number of points in the time series                           *
 *  fcut  = the cutoff frequency                                              *
 *  delt  = the timestep                                                      *
 *  nroll = filter order                                                      *
 *  causal if icaus.eq.1 - zero phase shift otherwise                         *
 *                                                                            *
 * The response is given by eq. 15.8-6 in Kanasewich:                         *
 *  Y = sqrt((f/fcut)**(2*n)/(1+(f/fcut)**(2*n))),                            *
 *                 where n = 2*nroll                                          *
 *                                                                            *
 * Dates: 01/07/97 - Written by Bill Joyner                                   *
 *        12/17/99 - D. Boore added check for fcut = 0.0, in which case       *
 *                   no filter is applied.  He also cleaned up the            *
 *                   appearance of the code (indented statements in           *
 *                   loops, etc.)                                             *
 *        02/04/00 - Changed "n" to "nroll" to eliminate confusion with       *
 *                   Kanesewich, who uses "n" as the order (=2*nroll)         *
 *        03/01/00 - Ported to C by Jim Luetgert                              *
 *                                                                            *
 ******************************************************************************/
void locut(float *s, int nd, float fcut, float delt, int nroll, int icaus)
{
    float    fact[8], b1[8], b2[8];
    float    pi, w0, w1, w2, w3, w4, w5, xp, yp, x1, x2, y1, y2;
    int       j, k, np2, npad;

    if (fcut == 0.0) return;       /* Added by DMB  */

    pi = 4.0*atan(1.0);
    w0 = 2.0*pi*fcut;
    w1 = 2.0*tan(w0*delt/2.0);
    w2 = (w1/2.0)*(w1/2.0);
    w3 = (w1*w1)/2.0 - 2.0;
    w4 = 0.25*pi/nroll;

    for(k=0;k<nroll;k++) {
        w5 = w4*(2.0*k + 1.0);
        fact[k] = 1.0/(1.0+sin(w5)*w1 + w2);
        b1[k] = w3*fact[k];
        b2[k] = (1.0-sin(w5)*w1 + w2)*fact[k];
    }

    np2 = nd;

    if(icaus != 1) {
        npad = 3.0*nroll/(fcut*delt);
        np2 = nd+npad;
        for(j=nd;j<np2;j++) s[j] = 0.0;
    }

    for(k=0;k<nroll;k++) {
        x1 = x2 = y1 = y2 = 0.0;
        for(j=0;j<np2;j++) {
            xp = s[j];
            yp = fact[k]*(xp-2.0*x1+x2) - b1[k]*y1 - b2[k]*y2;
            s[j] = yp;
            y2 = y1;
            y1 = yp;
            x2 = x1;
            x1 = xp;
        }
    }

    if(icaus != 1) {
        for(k=0;k<nroll;k++) {
            x1 = x2 = y1 = y2 = 0.0;

            for(j=0;j<np2;j++) {
                xp = s[np2-j-1];
                yp = fact[k]*(xp-2.0*x1+x2) - b1[k]*y1 - b2[k]*y2;
                s[np2-j-1] = yp;
                y2 = y1;
                y1 = yp;
                x2 = x1;
                x1 = xp;
            }
        }
    }

    return;
}


/******************************************************************************
 * rdrvaa                                                                     *
 *                                                                            *
 * This is a modified version of "Quake.For", originally                      *
 * written by J.M. Roesset in 1971 and modified by                            *
 * Stavros A. Anagnostopoulos, Oct. 1986.  The formulation is                 *
 * that of Nigam and Jennings (BSSA, v. 59, 909-922, 1969).                   *
 * Dates: 02/11/00 - Modified by David M. Boore, based on RD_CALC             *
 *        03/01/00 - Ported to C by Jim Luetgert                              *
 *                                                                            *
 *   acc = acceleration time series                                           *
 *    na = length of time series                                              *
 * omega = 2*pi/per                                                           *
 *  damp = fractional damping (e.g., 0.05)                                    *
 *    dt = time spacing of input                                              *
 *    rd = relative displacement of oscillator                                *
 *    rv = relative velocity of oscillator                                    *
 *    aa = absolute acceleration of oscillator                                *
 ******************************************************************************/
void rdrvaa(float *acc, int na, float omega, float damp, float dt,
            float *rd, float *rv, float *aa)
{
    float    omt, d2, bom, d3, omd, om2, omdt, c1, c2, c3, c4, cc, ee;
    float    s1, s2, s3, s4, s5, a11, a12, a21, a22, b11, b12, b21, b22;
    float    y, ydot, y1, z, z1, z2, ra;
    int       i;

    omt  = omega*dt;
    d2   = sqrt(1.0-damp*damp);
    bom  = damp*omega;
    d3   = 2.0*bom;
    omd  = omega*d2;
    om2  = omega*omega;
    omdt = omd*dt;
    c1 = 1.0/om2;
    c2 = 2.0*damp/(om2*omt);
    c3 = c1+c2;
    c4 = 1.0/(omega*omt);
    ee = exp(-damp*omt);
    cc = cos(omdt)*ee;
    s1 = sin(omdt)*ee/omd;
    s2 = s1*bom;
    s3 = s2 + cc;
    s4 = c4*(1.0-s3);
    s5 = s1*c4 + c2;

    a11 =  s3;          a12 = s1;
    a21 = -om2*s1;      a22 = cc - s2;

    b11 =  s3*c3 - s5;  b12 = -c2*s3 + s5 - c1;
    b21 = -s1+s4;       b22 = -s4;

    y = ydot = *rd = *rv = *aa = 0.0;
    for(i=0;i<na-1;i++) {
        y1   = a11*y + a12*ydot + b11*acc[i] + b12*acc[i+1];
        ydot = a21*y + a22*ydot + b21*acc[i] + b22*acc[i+1];
        y = y1;    /* y is the oscillator output at time corresponding to index i   */
        z = fabs(y);
        if (z > *rd) *rd = z;
        z1 = fabs(ydot);
        if (z1 > *rv) *rv = z1;
        ra = -d3*ydot -om2*y1;
        z2 = fabs(ra);
        if (z2 > *aa) *aa = z2;
    }
}

/******************************************************************************
 *   compute maximum amplitude and its associated period                      *
 *                                                                            *
 *   input:                                                                   *
 *           npts   - number of points in timeseries                          *
 *           dt     - sample spacing in sec                                   *
 *           fc     - input timeseries                                        *
 *   output:                                                                  *
 *           amaxmm - raw maximum                                             *
 *           pmax   - period of maximum                                       *
 *           imax   - index of maxmimum point                                 *
 *                                                                            *
 ******************************************************************************/
void amaxper(int npts, float dt, float *fc, float *aminmm, float *amaxmm,
                       float *pmax, int *imin, int *imax)
{
    float    amin, amax, pp, pm, mean, frac;
    int       i, j, jmin, jmax;

    *imax = jmax = *imin = jmin = 0;
    amax = amin = *amaxmm = *aminmm = fc[0];
    *aminmm = *pmax = mean = 0.0;
    for(i=0;i<npts;i++) {
        mean = mean + fc[i]/npts;
        if (fc[i] > amax) { jmax = i; amax = fc[i]; }
        if (fc[i] < amin) { jmin = i; amin = fc[i]; }
    }

/*     compute period of maximum    */

    pp = pm = 0.0;
    if (fc[jmax] > mean) {
        j = jmax+1;
        while(fc[j] > mean && j < npts) {
            pp += dt;
            j  += 1;
        }
        frac = dt*(mean-fc[j-1])/(fc[j]-fc[j-1]);
        frac = 0.0;
        pp = pp + frac;
        j = jmax-1;
        while(fc[j] > mean && j >= 0) {
            pm += dt;
            j  -= 1;
        }
        frac = dt*(mean-fc[j+1])/(fc[j]-fc[j+1]);
        frac = 0.0;
        pm = pm + frac;
    } else {
        j = jmax+1;
        if(fc[j] < mean && j < npts) {
            pp += dt;
            j  += 1;
        }
        frac = dt*(mean-fc[j-1])/(fc[j]-fc[j-1]);
        frac = 0.0;
        pp = pp + frac;
        j = jmax-1;
        if(fc[j] < mean && j >= 0) {
            pm += dt;
            j  -= 1;
        }
        frac = dt*(mean-fc[j+1])/(fc[j]-fc[j+1]);
        frac = 0.0;
        pm = pm + frac;
    }

    *imin = jmin;
    *imax = jmax;
    *pmax = 2.0*(pm+pp);
    *aminmm = amin;
    *amaxmm = amax;

    return;
}


void savetimeseries(int npts, float *data, char *filename) {
//	return;
	FILE *fp;
	int i;
if (strncmp(filename,"d",1)!=0) {
	strcpy(filename,"data1");
}
	fp = fopen(filename, "w");
	for (i=0; i<npts; i++) {
		fprintf(fp,"%e\n",data[i]);
	}
	fclose(fp);
}
