/* @(#)rainit.c	1.1 08/14/96 */
/*======================================================================
 *
 * Init processing memory structure
 *
 *====================================================================*/
#include "pick.h"

void pick_rainit(info)
struct pick_rainfo *info;
{
int i;

    info->pflag  = PICK_IDLE;
    info->cflag  = PICK_IDLE;
    info->cocrit = 0.0;
    info->crtinc = 0.0;
    info->edat   = 0.0;
    info->rdif   = 0.0;
    info->eabs   = 0.0;
    info->ecrit  = 0.0;
    info->elta   = 0.0;
    info->eold   = 0.0;
    info->eref   = 0.0;
    info->esta   = 0.0;
    info->famp   = 0.0;
    info->rdat   = 0.0;
    info->rbig   = 0.0;
    info->rlast  = 0.0;
    info->rold   = 0.0;
    info->rsrdat = 0.0;
    info->tmax   = 0.0;
    info->xfrz   = 0.0;
    info->dold   = 0;
    info->evlen  = 0;
    info->iamp   = 0;
    info->icount = 0;
    info->isml   = 0;
    info->k      = 0;
    info->m      = 0;
    info->mint   = 0;
    info->ndrt   = 0;
    info->next   = 0;
    info->nzero  = 0;
    info->xdot   = 0;
    for (i = 0; i < 10; i++) info->fmdat[i] = 0;
    info->count  = 0;
}
