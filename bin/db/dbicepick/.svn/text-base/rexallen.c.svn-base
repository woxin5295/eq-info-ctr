/* @(#)rexallen.c	1.1 08/14/96 */
/*======================================================================
 *
 * Rex Allen P-wave picker.
 *
 * Returns number of picks found, or -1 if more picks than could be
 * stored were found (all picks found prior to that still OK).
 *
 *====================================================================*/
#include <math.h>
#include <stdio.h>
#include "pick.h"

#define  MAX(a,b) ((a) > (b) ? (a) : (b))
#define SIGN(a,b) (((a) > 0.0 && (b) > 0.0) || ((a) < 0.0 && (b) < 0.0))

int Pick_RexAllen(trace, info, par, pick, coda, npick, maxpick, restart)
struct pick_trace *trace; /* input data                       */
struct pick_rainfo *info; /* processing memory                */
struct pick_rapar *par;   /* picker tuning parameters         */
struct pick *pick;        /* array to store picks             */
struct coda *coda;        /* array to store codas             */
int *npick;               /* number of valid entries in above */
int maxpick;              /* dimension of pick and coda       */
int restart;              /* restart flag                     */
{
int i, j, lwindow, codalen, noise, itrm, status;
long sample;
double stime; /* Current sample time     */
double aav, adata, vt3, xpk, xon, xp0, xp1, xp2;


    if (info->first) {
        info->last_pwin  = par->pwin[par->npwin-1];
        info->maxcodalen = (2 * info->last_pwin) - 1;
        info->first = 0;
    }

    *npick = 0;

/* Initialize processing memory if (re)starting picker */

    if (restart) pick_rainit(info);

/* Loop over all samples in trace */

    for (i = 0; i < trace->nsamp; i++) {

        sample = trace->data[i];
        stime  = trace->tofs + ((double) i * (double) trace->sint);

        status = (info->pflag == PICK_IDLE && info->cflag == PICK_IDLE)
                ? PICK_IDLE : PICK_BUSY;

        if (status == PICK_IDLE) {
            info->prev_sample = info->dold;
            info->eold  = info->eref;
        }

    /* Process datum */

        info->rold  = info->rdat;
        info->rdat  = (info->rdat * par->c1) + 
                      (double) (sample - info->dold) + par->h1;
        info->rdif  = info->rdat - info->rold;
        info->dold  = sample;
        info->edat  = (info->rdat * info->rdat) + 
                      (par->c2 * info->rdif * info->rdif);
        info->esta += par->c3 * (info->edat - info->esta);
        info->elta += par->c4 * (info->edat - info->elta);
        info->eref  = info->elta * par->c5;
        info->eabs  = (par->h2 * info->eabs) + 
                      ((1.0 - par->h2) * fabs(info->rdat));

    /* Save intermediate results, if desired */

/* REIMPLEMENT    if (info->save) pick_rasave(info); */

    /* Let filters stabilize if necessary */

        if (++info->count < (u_long) par->n1) continue;

    /* If not currently processing a pick, look for a new arrival */

        if (status == PICK_IDLE) {

            if (info->eabs > par->c6) continue; /* ignore dead chans */

            if (info->esta > info->eref) {

        /* Declare an arrival and initialize */

                info->pflag = info->cflag = PICK_BUSY;

                info->pick.time = stime;

                info->xdot  = sample - info->prev_sample;
                info->rbig  = fabs(info->xdot) / 3.0;
                if (info->eabs > info->rbig) info->rbig = info->eabs;
                info->rlast = info->rdat;

                info->evlen = 0;
                info->nzero = 0;
                info->isml  = 0;
                info->m     = 1;
                info->mint  = 0;

                info->ecrit  = info->eold;
                info->crtinc = info->eref / par->h3;

                info->next   = 0;
                info->tmax   = fabs(info->rdat);

                info->ndrt   = 0;
                info->icount = 0;
                info->k      = 0;
                info->rsrdat = 0.0;

                for (j = 0; j < 6; j++) info->coda.aav[j] = 0;

                if (info->eabs > (par->c8 * par->c7)) {
                    info->cocrit = par->c9 * info->eabs;
                    info->coda.len = -1.0;
                } else {
                    info->cocrit = par->c7;
                    info->coda.len = 1.0;
                }

                info->fmdat[0] = sample;
                info->xfrz = par->h4 * info->eabs;
            }

    /* otherwise continue processing active pick or coda */

        } else {

            if (info->cflag == PICK_BUSY) {  /* begin coda processing */

                info->rsrdat += fabs(info->rdat);

                if (info->icount == info->last_pwin - 1) {
                    lwindow = par->h8;
                } else {
                    lwindow = par->h9;
                }

                /* Save windows specified in the pwin array */

                if (++info->ndrt >= lwindow) {
                    if (info->k >= par->npwin) {
                        fprintf(stderr, "ERROR: pwin overrun!\n");
                        exit(1);
                    }
                    if (info->icount++ == par->pwin[info->k]) {
                        for (j = 5; j > 0; j--) {
                            info->coda.aav[j] = info->coda.aav[j-1];
                        }
                        info->k++;
                    }

                    /* Compute and save average absolute value */

                    aav = info->rsrdat / (double) lwindow;
                    info->coda.aav[0] = (int) (aav + .5);

                    /* Initialize counter and coda amp sum */

                    info->ndrt   = 0;
                    info->rsrdat = 0.0;

                    /* See if the coda calculation is finished */

                    if ((info->icount == info->last_pwin)||(aav < info->cocrit)){
                        codalen = (2 * info->icount) - 1;
                        if (codalen == info->maxcodalen) codalen = info->maxcodalen-1;
                        if (codalen < par->i9 && info->pflag == PICK_BUSY) {
                            info->pflag = info->cflag = PICK_IDLE;
                            continue;
                        }
                        info->coda.len *= (float) codalen;
                        info->cflag = PICK_DONE;
                    }
                }
            } /* end of coda processing */

            if (info->pflag == PICK_BUSY) { /* begin pick processing */

            /* Save first 10 points after pick for FM determination */
    
                if (++info->evlen < 10) info->fmdat[info->evlen] = sample;

            /* Store current data if it is a new extreme value */

                if (info->next < 3) {
                    info->tmax = MAX(info->tmax, fabs(info->rdat));
                }

            /* Test for large zero crossing.  Large zero-crossing amplitude
             * must exceed ebig and must represent a crossing of opposite
             * polarity to previous crossing.
             */

                if (
                    (fabs( info->rdat ) >= info->rbig) &&
                    !SIGN( info->rdat, info->rlast )
                ) {
                    info->nzero++;
                    info->rlast = info->rdat;
                }

            /* Increment zero crossing interval counter.  Terminate pick
             * if no zero crossings have occurred recently.
             */

                if (++info->mint > par->h5) {
                    info->pflag = info->cflag = PICK_IDLE;
                    continue;
                }

            /* Test for small zero crossing */

                if (SIGN(info->rdat, info->rold)) continue;

            /* Small zero crossing found.  Reset interval counter */

                info->mint = 0;

            /* Update ecrit and determine whether at this crossing esta
             * is still above ecrit.  If not, increment isml, the number
             * of successive small crossings.  If esta is greater than
             * ecrit, reset isml to 0.
             */

                if (info->esta <= (info->ecrit += info->crtinc)) {
                    ++info->isml;
                } else {
                    info->isml = 0;
                }

            /* Store extrema of preceeding half cycle */

                if (info->next < 3) {
                    info->pick.xpk[info->next++] = info->tmax;

                    if (info->next == 1) {
                        vt3    = info->tmax / 3.;
                        info->rbig = (vt3 > info->rbig) ? vt3 : info->rbig;
                    }

                    info->tmax = 0.;
                }

            /* Compute itrm, the number of small zero crossings before
             * declaring the pick over.  This will start out quite
             * small and increase during the event to a maximum of par->h7.
             */

                itrm = (info->m > par->h6) ? par->h7 : 
                                             par->i5 + (info->m / par->i5);

            /*  See if the pick is over  */

                if (++info->m != par->i6 && info->isml < itrm) continue;
                info->pflag = PICK_DONE;

            /* See if the pick was a noise pick */

                for (noise = 1, j = 0; noise == 1 && j < 3; j++) {
                    if (
                        info->pick.xpk[j] >= par->i8 &&
                        info->m == par->i6 &&
                        info->nzero >= par->i7
                    ) noise = 0;
                }

                if (noise) {
                    info->pflag = info->cflag = PICK_IDLE;
                    continue;
                }

            /* A valid pick was found.  Determine the first motion. */

                if (info->xdot > 0 && info->fmdat[1] > info->fmdat[0]) {
                    info->pick.fm = 'U';
                } else if (info->xdot < 0 && info->fmdat[1] < info->fmdat[0]) {
                    info->pick.fm = 'D';
                } else {
                    info->pick.fm = ' ';
                }

            /* Determine pick weight */

                xpk = (info->pick.xpk[0] > fabs((double)info->fmdat[0])) ?
                       info->pick.xpk[0] : info->pick.xpk[1];
                xon = fabs(info->xdot   / info->xfrz);
                xp0 = info->pick.xpk[0] / info->xfrz;
                xp1 = info->pick.xpk[1] / info->xfrz;
                xp2 = info->pick.xpk[2] / info->xfrz;

                if (
                    xp0 > par->h10 &&
                    (xp1 > par->h11 || xp2 > par->h11) &&
                    xon > par->h12 &&
                    xpk > par->h13
                ) {
                    info->pick.wt = 0;
                } else if (
                    xp0 > par->h14 &&
                    (xp1 > par->h15 || xp2 > par->h15) &&
                    xon > par->h12 &&
                    xpk > par->h16
                ) {
                    info->pick.wt = 1;
                } else if (
                    xp0 > par->h17 &&
                    xon > par->h12 &&
                    xpk > par->h18
                ) {
                    info->pick.wt = 2;
                } else {
                    info->pick.wt = 3;
                }

                info->pflag = PICK_DONE;
            } /* end of pick processing */
        }

        if (info->pflag == PICK_DONE && info->cflag == PICK_DONE) {
            if (*npick == maxpick) return -1;
            pick[*npick] = info->pick;
            coda[*npick] = info->coda;
            info->pflag = PICK_IDLE;
            info->cflag = PICK_IDLE;
            *npick += 1;
        }
    }

    return *npick;
}
