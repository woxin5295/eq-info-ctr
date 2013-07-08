/* @(#)raldpar.c	1.1 08/14/96 */
/*======================================================================
 *
 * Read Rex Allen picker parameters from a file
 *
 *====================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include "pick.h"

#define MAXTOKEN 512
#define BUFLEN 256

int pick_raldpar(complete, par)
int complete;
struct pick_rapar *par;
{
static char buffer[BUFLEN];
int i, j, ntoken, status, lineno;
char *token[MAXTOKEN];
static int pwin[23] = {
    1,  2,  3,  4,  5,  6,  7,  8, 10, 12, 16,
   20, 24, 28, 32, 36, 40, 44, 48, 56, 64, 70, 73
};

/* Following are from USGS Earthworm picker_params.d file */

if( complete ) {
    par->i5 =       3;  /* Parameter used to calculate itrm             */
    par->i6 =      40;  /* Minimum number of small zero crossings       */
    par->i7 =       3;  /* Minimum number of big zero crossings         */
    par->i8 =      60;  /* Minimum size of 1'st three peaks             */
    par->i9 =       3;  /* Minimum coda length in seconds               */
    par->c1 =   0.985;  /* Filter parameter for raw data                */
    par->c2 =     3.0;  /* Filter parameter for characteristic function */
    par->c3 =      .6;  /* Filter parameter for short-term average      */
    par->c4 =     .03;  /* Filter parameter for long-term average       */
    par->c5 =     5.0;  /* STA/LTA event threshold                      */
    par->c6 =  1200.0;  /* Dead station threshold                       */
    par->c7 =   49.14;  /* Coda termination threshold (60 mV, in counts)*/
    par->c8 =     0.8;  /* Frac of c7 at which alt coda term. used      */
    par->c9 =     1.5;  /* Frac of pre-event level for alt coda term.   */

/* Following are new to this implementation */

    par->n1 =     100;  /* number of samples to use to stablize filters */
}
/* Following were hard-coded into original Earthworm picker */

    par->h1  = 1.0e-10;  /* quiet station constant         (QUIET) */
    par->h2  =  0.9961;  /* used in calculating eabs          (C4) */
    par->h3  = 50000.0;  /* used in calculating crtinc     (EREFS) */
    par->h4  =     1.6;  /* used in calculating xfrz         (1.6) */
    par->h5  =     500;  /* Max int. between 0 crossings (MAXMINT) */
    par->h6  =     150;  /* used in computing itrm           (150) */
    par->h7  =      50;  /* used in computing itrm (max itrm) (50) */
    par->h8  =     100;  /* used in coda aav (lwindow)       (100) */
    par->h9  =     200;  /* used in coda aav (lwindow)       (200) */
    par->h10 =     4.0;  /* used in computing pick weight    (4.0) */
    par->h11 =     6.0;  /* used in computing pick weight    (6.0) */
    par->h12 =     0.5;  /* used in computing pick weight    (0.5) */
    par->h13 =   200.0;  /* used in computing pick weight  (200.0) */
    par->h14 =     3.0;  /* used in computing pick weight    (3.0) */
    par->h15 =     3.0;  /* used in computing pick weight    (3.0) */
    par->h16 =   100.0;  /* used in computing pick weight  (100.0) */
    par->h17 =     2.0;  /* used in computing pick weight    (2.0) */
    par->h18 =    25.0;  /* used in computing pick weight   (25.0) */

    par->npwin =   23;  /* number of windows for coda aav calculation */
    par->pwin  = pwin;  /* the windows                                */

}
