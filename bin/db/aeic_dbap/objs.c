
#include "dbap_defines.h"

typedef struct rcpobj_ {
	char type[32];
	int itype;
	void *ptr;
} Rcpobj;

Arr *recipe_objs=NULL;

typedef struct prcobj_ {
	char type[32];
	int itype;
	char *value;
	Tbl *recipes;
} Prcobj;

typedef struct prcrcp_ {
	char name[32];
	int itype;
	void *ptr;
} Prcrcp;

Arr *process_objs=NULL;

Arr *varlist=NULL;

Tbl *proclist=NULL;

Dbptr dbi;

int
recipe_obj_add (name, type, params, nchans)

char *          name;
char *                type;
char *                      params;
int                                 nchans;

{
	char *array, *refsta, *chan;
	char *value;
	int nx, ny, ns;
	double xmin, xmax, ymin, ymax, smin, smax;
	double tpad, samprate, azimuth, slow;
	double fmin, fmax;
	Apspec *gs, *cc;

	if (!strcmp(type, "beam")) {
		if (!parse_beam_params (params, &array, &refsta, &chan, &azimuth, &slow,
					&tpad, &samprate)) {
			fprintf (stderr, "recipe_obj_add: parse_beam_params() error for object '%s'.\n",
										name);
			return (0);
		}
		gs = (Apspec *) my_malloc ("recipe_obj_add: gs", sizeof(Apspec));
		if (gs == NULL) {
			fprintf (stderr, "recipe_obj_add: malloc error.\n");
			return (0);
		}
		strcpy (gs->array, array);
		strcpy (gs->refsta, refsta);
		strcpy (gs->chan, chan);
		gs->gather = NULL;
		gs->tpad = tpad;
		var_get ("samprate", &value);
		if (value) {
			gs->samprate = atof(value);
		} else {
			gs->samprate = samprate;
		}
		gs->type = BEAM;
		gs->nsta = 0;
		gs->savegrids = 0;
		gs->summary = 0;
		gs->nx = 0;
		gs->ny = 0;
		gs->xmin = 0.0;
		gs->xmax = 0.0;
		gs->ymin = 0.0;
		gs->ymax = 0.0;
		gs->fmin = 0.0;
		gs->fmax = 0.0;
		var_get ("azimuth", &value);
		if (value) {
			gs->azimuth = atof(value);
		} else {
			gs->azimuth = azimuth;
		}
		var_get ("slow", &value);
		if (value) {
			gs->slowi = atof(value);
		} else {
			gs->slowi = slow;
		}
		gs->x = NULL;
		gs->y = NULL;
		gs->ns = 0;
		gs->smin = 0.0;
		gs->smax = 0.0;
		gs->nt = 0;
		gs->t0 = 0.0;
		gs->dt = 0.0;
		gs->t = NULL;
		gs->s = NULL;
		gs->norm_power_grid = NULL;
		gs->power_grid = NULL;
		gs->tr = NULL;
		gs->az = 0.0;
		gs->slow = 0.0;
		gs->width = 0.0;
		gs->sss2n = 0.0;
		gs->bms2n = 0.0;
		gs->gridratio = 0.0;
		gs->grid_power = NULL;
		gs->grid_raw_power = NULL;
		gs->grid_raw_az = NULL;
		gs->grid_raw_sl = NULL;
		gs->grid_nodes = NULL;
		gs->grids = NULL;
		gs->chans = newtbl(nchans);
		if (gs->chans == NULL) {
			fprintf (stderr, "recipe_obj_add: newtbl() error for object '%s'.\n",
										name);
		}
		gs->norm_summary_grid.power = NULL;
		gs->norm_summary_grid.peak_power = 0.0;
		gs->norm_summary_grid.peak_azimuth = 0.0;
		gs->norm_summary_grid.peak_slow = 0.0;
		gs->norm_summary_grid.tstart = 0.0;
		gs->norm_summary_grid.tend = 0.0;
		gs->summary_grid.power = NULL;
		gs->summary_grid.peak_power = 0.0;
		gs->summary_grid.peak_azimuth = 0.0;
		gs->summary_grid.peak_slow = 0.0;
		gs->summary_grid.tstart = 0.0;
		gs->summary_grid.tend = 0.0;
		if (!recipe_obj_add_entry (name, type, BEAM, gs)) {
			fprintf (stderr, "recipe_obj_add: recipe_obj_add_entry() error for object '%s'.\n",
										name);
			return (0);
		}
	} else if (!strcmp(type, "gather")) {
		if (!parse_gather_params (params, &refsta, &tpad, &samprate)) {
			fprintf (stderr, "recipe_obj_add: parse_gather_params() error for object '%s'.\n",
										name);
			return (0);
		}
		gs = (Apspec *) my_malloc ("recipe_obj_add: gs", sizeof(Apspec));
		if (gs == NULL) {
			fprintf (stderr, "recipe_obj_add: malloc error.\n");
			return (0);
		}
		strcpy (gs->array, "-");
		strcpy (gs->refsta, refsta);
		strcpy (gs->chan, "-");
		gs->gather = NULL;
		gs->tpad = tpad;
		var_get ("samprate", &value);
		if (value) {
			gs->samprate = atof(value);
		} else {
			gs->samprate = samprate;
		}
		gs->type = GATHER;
		gs->nsta = 0;
		gs->savegrids = 0;
		gs->summary = 0;
		gs->nx = 0;
		gs->ny = 0;
		gs->xmin = 0.0;
		gs->xmax = 0.0;
		gs->ymin = 0.0;
		gs->ymax = 0.0;
		gs->fmin = 0.0;
		gs->fmax = 0.0;
		gs->azimuth = 0.0;
		gs->slowi = 0.0;
		gs->x = NULL;
		gs->y = NULL;
		gs->ns = 0;
		gs->smin = 0.0;
		gs->smax = 0.0;
		gs->nt = 0;
		gs->t0 = 0.0;
		gs->dt = 0.0;
		gs->t = NULL;
		gs->s = NULL;
		gs->norm_power_grid = NULL;
		gs->power_grid = NULL;
		gs->tr = NULL;
		gs->az = 0.0;
		gs->slow = 0.0;
		gs->width = 0.0;
		gs->sss2n = 0.0;
		gs->bms2n = 0.0;
		gs->gridratio = 0.0;
		gs->grid_power = NULL;
		gs->grid_raw_power = NULL;
		gs->grid_raw_az = NULL;
		gs->grid_raw_sl = NULL;
		gs->grid_nodes = NULL;
		gs->grids = NULL;
		gs->chans = newtbl(nchans);
		if (gs->chans == NULL) {
			fprintf (stderr, "recipe_obj_add: newtbl() error for object '%s'.\n",
										name);
		}
		gs->norm_summary_grid.power = NULL;
		gs->norm_summary_grid.peak_power = 0.0;
		gs->norm_summary_grid.peak_azimuth = 0.0;
		gs->norm_summary_grid.peak_slow = 0.0;
		gs->norm_summary_grid.tstart = 0.0;
		gs->norm_summary_grid.tend = 0.0;
		gs->summary_grid.power = NULL;
		gs->summary_grid.peak_power = 0.0;
		gs->summary_grid.peak_azimuth = 0.0;
		gs->summary_grid.peak_slow = 0.0;
		gs->summary_grid.tstart = 0.0;
		gs->summary_grid.tend = 0.0;
		if (!recipe_obj_add_entry (name, type, GATHER, gs)) {
			fprintf (stderr, "recipe_obj_add: recipe_obj_add_entry() error for object '%s'.\n",
										name);
			return (0);
		}
	} else if (!strcmp(type, "crosscor")) {
		if (!parse_crosscor_params (params, &refsta, &azimuth, &slow,
					&tpad, &samprate)) {
			fprintf (stderr, "recipe_obj_add: parse_crosscor_params() error for object '%s'.\n",
										name);
			return (0);
		}
		cc = (Apspec *) my_malloc ("recipe_obj_add: cc", sizeof(Apspec));
		if (cc == NULL) {
			fprintf (stderr, "recipe_obj_add: malloc error.\n");
			return (0);
		}
		strcpy (cc->array, "-");
		strcpy (cc->refsta, refsta);
		strcpy (cc->chan, "-");
		cc->gather = NULL;
		cc->tpad = tpad;
		var_get ("samprate", &value);
		if (value) {
			cc->samprate = atof(value);
		} else {
			cc->samprate = samprate;
		}
		cc->type = CROSSCOR;
		cc->nsta = 0;
		cc->savegrids = 0;
		cc->summary = 0;
		cc->nx = 0;
		cc->ny = 0;
		cc->xmin = 0.0;
		cc->xmax = 0.0;
		cc->ymin = 0.0;
		cc->ymax = 0.0;
		cc->fmin = 0.0;
		cc->fmax = 0.0;
		var_get ("azimuth", &value);
		if (value) {
			cc->azimuth = atof(value);
		} else {
			cc->azimuth = azimuth;
		}
		var_get ("slow", &value);
		if (value) {
			cc->slowi = atof(value);
		} else {
			cc->slowi = slow;
		}
		cc->x = NULL;
		cc->y = NULL;
		cc->ns = 0;
		cc->smin = 0.0;
		cc->smax = 0.0;
		cc->nt = 0;
		cc->t0 = 0.0;
		cc->dt = 0.0;
		cc->t = NULL;
		cc->s = NULL;
		cc->norm_power_grid = NULL;
		cc->power_grid = NULL;
		cc->tr = NULL;
		cc->az = 0.0;
		cc->slow = 0.0;
		cc->width = 0.0;
		cc->sss2n = 0.0;
		cc->bms2n = 0.0;
		cc->gridratio = 0.0;
		cc->grid_power = NULL;
		cc->grid_raw_power = NULL;
		cc->grid_raw_az = NULL;
		cc->grid_raw_sl = NULL;
		cc->grid_nodes = NULL;
		cc->grids = NULL;
		cc->chans = newtbl(nchans);
		if (cc->chans == NULL) {
			fprintf (stderr, "recipe_obj_add: newtbl() error for object '%s'.\n",
										name);
		}
		cc->norm_summary_grid.power = NULL;
		cc->norm_summary_grid.peak_power = 0.0;
		cc->norm_summary_grid.peak_azimuth = 0.0;
		cc->norm_summary_grid.peak_slow = 0.0;
		cc->norm_summary_grid.tstart = 0.0;
		cc->norm_summary_grid.tend = 0.0;
		cc->summary_grid.power = NULL;
		cc->summary_grid.peak_power = 0.0;
		cc->summary_grid.peak_azimuth = 0.0;
		cc->summary_grid.peak_slow = 0.0;
		cc->summary_grid.tstart = 0.0;
		cc->summary_grid.tend = 0.0;
		if (!recipe_obj_add_entry (name, type, CROSSCOR, cc)) {
			fprintf (stderr, "recipe_obj_add: recipe_obj_add_entry() error for object '%s'.\n",
										name);
			return (0);
		}
	} else if (!strcmp(type, "xygrid")) {
		if (!parse_xygrid_params (params, &array, &refsta, &nx, &xmin, &xmax,
					&ny, &ymin, &ymax, &tpad, &samprate)) {
			fprintf (stderr, "recipe_obj_add: parse_xygrid_params() error for object '%s'.\n",
										name);
			return (0);
		}
		gs = (Apspec *) my_malloc ("recipe_obj_add: gs", sizeof(Apspec));
		if (gs == NULL) {
			fprintf (stderr, "recipe_obj_add: malloc error.\n");
			return (0);
		}
		strcpy (gs->array, array);
		strcpy (gs->refsta, refsta);
		strcpy (gs->chan, "");
		gs->gather = NULL;
		gs->tpad = tpad;
		var_get ("samprate", &value);
		if (value) {
			gs->samprate = atof(value);
		} else {
			gs->samprate = samprate;
		}
		gs->type = XYGRID;
		gs->nsta = 0;
		gs->savegrids = 0;
		gs->summary = 0;
		gs->nx = nx;
		gs->ny = ny;
		gs->xmin = xmin;
		gs->xmax = xmax;
		gs->ymin = ymin;
		gs->ymax = ymax;
		gs->fmin = 0.0;
		gs->fmax = 0.0;
		gs->azimuth = 0.0;
		gs->slowi = 0.0;
		gs->x = NULL;
		gs->y = NULL;
		gs->ns = 0;
		gs->smin = 0.0;
		gs->smax = 0.0;
		gs->nt = 0;
		gs->t0 = 0.0;
		gs->dt = 0.0;
		gs->t = NULL;
		gs->s = NULL;
		gs->norm_power_grid = NULL;
		gs->power_grid = NULL;
		gs->tr = NULL;
		gs->az = 0.0;
		gs->slow = 0.0;
		gs->width = 0.0;
		gs->sss2n = 0.0;
		gs->bms2n = 0.0;
		gs->gridratio = 0.0;
		gs->grid_power = NULL;
		gs->grid_raw_power = NULL;
		gs->grid_raw_az = NULL;
		gs->grid_raw_sl = NULL;
		gs->grid_nodes = NULL;
		gs->grids = NULL;
		gs->chans = newtbl(nchans);
		if (gs->chans == NULL) {
			fprintf (stderr, "recipe_obj_add: newtbl() error for object '%s'.\n",
										name);
		}
		gs->norm_summary_grid.power = NULL;
		gs->norm_summary_grid.peak_power = 0.0;
		gs->norm_summary_grid.peak_azimuth = 0.0;
		gs->norm_summary_grid.peak_slow = 0.0;
		gs->norm_summary_grid.tstart = 0.0;
		gs->norm_summary_grid.tend = 0.0;
		gs->summary_grid.power = NULL;
		gs->summary_grid.peak_power = 0.0;
		gs->summary_grid.peak_azimuth = 0.0;
		gs->summary_grid.peak_slow = 0.0;
		gs->summary_grid.tstart = 0.0;
		gs->summary_grid.tend = 0.0;
		if (!recipe_obj_add_entry (name, type, XYGRID, gs)) {
			fprintf (stderr, "recipe_obj_add: recipe_obj_add_entry() error for object '%s'.\n",
										name);
			return (0);
		}
	} else if (!strcmp(type, "stgrid")) {
		if (!parse_stgrid_params (params, &array, &refsta, &azimuth,
					&ns, &smin, &smax, &tpad, &samprate)) {
			fprintf (stderr, "recipe_obj_add: parse_stgrid_params() error for object '%s'.\n",
										name);
			return (0);
		}
		gs = (Apspec *) my_malloc ("recipe_obj_add: gs", sizeof(Apspec));
		if (gs == NULL) {
			fprintf (stderr, "recipe_obj_add: malloc error.\n");
			return (0);
		}
		strcpy (gs->array, array);
		strcpy (gs->refsta, refsta);
		strcpy (gs->chan, "");
		gs->gather = NULL;
		gs->tpad = tpad;
		var_get ("samprate", &value);
		if (value) {
			gs->samprate = atof(value);
		} else {
			gs->samprate = samprate;
		}
		gs->type = STGRID;
		gs->nsta = 0;
		gs->savegrids = 0;
		gs->summary = 0;
		gs->nx = 0;
		gs->ny = 0;
		gs->xmin = 0.0;
		gs->xmax = 0.0;
		gs->ymin = 0.0;
		gs->ymax = 0.0;
		gs->fmin = 0.0;
		gs->fmax = 0.0;
		var_get ("azimuth", &value);
		if (value) {
			gs->azimuth = atof(value);
		} else {
			gs->azimuth = azimuth;
		}
		var_get ("slow", &value);
		if (value) {
			gs->slowi = atof(value);
		} else {
			gs->slowi = 0.0;
		}
		gs->x = NULL;
		gs->y = NULL;
		gs->ns = ns;
		gs->smin = smin;
		gs->smax = smax;
		gs->nt = 0;
		gs->t0 = 0.0;
		gs->dt = 0.0;
		gs->t = NULL;
		gs->s = NULL;
		gs->norm_power_grid = NULL;
		gs->power_grid = NULL;
		gs->tr = NULL;
		gs->az = 0.0;
		gs->slow = 0.0;
		gs->width = 0.0;
		gs->sss2n = 0.0;
		gs->bms2n = 0.0;
		gs->gridratio = 0.0;
		gs->grid_power = NULL;
		gs->grid_raw_power = NULL;
		gs->grid_raw_az = NULL;
		gs->grid_raw_sl = NULL;
		gs->grid_nodes = NULL;
		gs->grids = NULL;
		gs->chans = newtbl(nchans);
		if (gs->chans == NULL) {
			fprintf (stderr, "recipe_obj_add: newtbl() error for object '%s'.\n",
										name);
		}
		gs->norm_summary_grid.power = NULL;
		gs->norm_summary_grid.peak_power = 0.0;
		gs->norm_summary_grid.peak_azimuth = 0.0;
		gs->norm_summary_grid.peak_slow = 0.0;
		gs->norm_summary_grid.tstart = 0.0;
		gs->norm_summary_grid.tend = 0.0;
		gs->summary_grid.power = NULL;
		gs->summary_grid.peak_power = 0.0;
		gs->summary_grid.peak_azimuth = 0.0;
		gs->summary_grid.peak_slow = 0.0;
		gs->summary_grid.tstart = 0.0;
		gs->summary_grid.tend = 0.0;
		if (!recipe_obj_add_entry (name, type, STGRID, gs)) {
			fprintf (stderr, "recipe_obj_add: recipe_obj_add_entry() error for object '%s'.\n",
										name);
			return (0);
		}
	} else if (!strcmp(type, "ftgrid")) {
		if (!parse_ftgrid_params (params, &array, &refsta, &azimuth,
					&slow, &fmin, &fmax, &tpad, &samprate)) {
			fprintf (stderr, "recipe_obj_add: parse_ftgrid_params() error for object '%s'.\n",
										name);
			return (0);
		}
		gs = (Apspec *) my_malloc ("recipe_obj_add: gs", sizeof(Apspec));
		if (gs == NULL) {
			fprintf (stderr, "recipe_obj_add: malloc error.\n");
			return (0);
		}
		strcpy (gs->array, array);
		strcpy (gs->refsta, refsta);
		strcpy (gs->chan, "");
		gs->gather = NULL;
		gs->tpad = tpad;
		var_get ("samprate", &value);
		if (value) {
			gs->samprate = atof(value);
		} else {
			gs->samprate = samprate;
		}
		gs->type = FTGRID;
		gs->nsta = 0;
		gs->savegrids = 0;
		gs->summary = 0;
		gs->nx = 0;
		gs->ny = 0;
		gs->xmin = 0.0;
		gs->xmax = 0.0;
		gs->ymin = 0.0;
		gs->ymax = 0.0;
		gs->fmin = fmin;
		gs->fmax = fmax;
		var_get ("azimuth", &value);
		if (value) {
			gs->azimuth = atof(value);
		} else {
			gs->azimuth = azimuth;
		}
		var_get ("slow", &value);
		if (value) {
			gs->slowi = atof(value);
		} else {
			gs->slowi = slow;
		}
		gs->x = NULL;
		gs->y = NULL;
		gs->ns = 0;
		gs->smin = 0.0;
		gs->smax = 0.0;
		gs->nt = 0;
		gs->t0 = 0.0;
		gs->dt = 0.0;
		gs->t = NULL;
		gs->s = NULL;
		gs->norm_power_grid = NULL;
		gs->power_grid = NULL;
		gs->tr = NULL;
		gs->az = 0.0;
		gs->slow = 0.0;
		gs->width = 0.0;
		gs->sss2n = 0.0;
		gs->bms2n = 0.0;
		gs->gridratio = 0.0;
		gs->grid_power = NULL;
		gs->grid_raw_power = NULL;
		gs->grid_raw_az = NULL;
		gs->grid_raw_sl = NULL;
		gs->grid_nodes = NULL;
		gs->grids = NULL;
		gs->chans = newtbl(nchans);
		if (gs->chans == NULL) {
			fprintf (stderr, "recipe_obj_add: newtbl() error for object '%s'.\n",
										name);
		}
		gs->norm_summary_grid.power = NULL;
		gs->norm_summary_grid.peak_power = 0.0;
		gs->norm_summary_grid.peak_azimuth = 0.0;
		gs->norm_summary_grid.peak_slow = 0.0;
		gs->norm_summary_grid.tstart = 0.0;
		gs->norm_summary_grid.tend = 0.0;
		gs->summary_grid.power = NULL;
		gs->summary_grid.peak_power = 0.0;
		gs->summary_grid.peak_azimuth = 0.0;
		gs->summary_grid.peak_slow = 0.0;
		gs->summary_grid.tstart = 0.0;
		gs->summary_grid.tend = 0.0;
		if (!recipe_obj_add_entry (name, type, FTGRID, gs)) {
			fprintf (stderr, "recipe_obj_add: recipe_obj_add_entry() error for object '%s'.\n",
										name);
			return (0);
		}
	} else {
		fprintf (stderr, "recipe_obj_add: Unknown type '%s' for object '%s'.\n",
						type, name);
	    	return (0);
	}
	return (1);
}

int
recipe_obj_add_chan (name, ichan, str)

char *               name;
int                        ichan;
char *                            str;

{
	char *sta, *chan, *gaps, *filter, *type;
	double wt, tcor;
	void *ptr;
	Apspec *gs;
	Channelspec *cs;
	char *value;
	int i, itype;

	if (!parse_channel (str, &sta, &chan, &wt, &tcor, &gaps, &filter)) {
		fprintf (stderr, "recipe_obj_add_chan: parse_channel() error for object '%s'.\n",
										name);
		return (0);
	}
	if (!recipe_obj_get_entry (name, &type, &itype, &ptr)) {
		fprintf (stderr, "recipe_obj_add_chan: parse_channel() error for object '%s'.\n",
										name);
		return (0);
	}
	cs = (Channelspec *) my_malloc ("recipe_obj_add_chan: cs", sizeof(Channelspec));
	if (cs == NULL) {
		fprintf (stderr, "recipe_obj_add_chan: malloc() error for object '%s'.\n",
										name);
		return (0);
	}
	strcpy (cs->sta, sta);
	strcpy (cs->chan, chan);
	var_get ("chan", &value);
	if (value) {
		if (strlen(value) == strlen(cs->chan)) {
			for (i=0; i<strlen(value); i++) {
				if (value[i] == '.') continue;
				cs->chan[i] = value[i];
			}
		} else {
			strcpy (cs->chan, value);
		}
	}
	cs->wt = wt;
	cs->tcor = tcor;
	cs->acor = 1.0;
	cs->tshift = 0.0;
	cs->dx = 0.0;
	cs->dy = 0.0;
	cs->gap = strdup(gaps);
	var_get ("filter", &value);
	if (value) {
		cs->filter = strdup(value);
	} else {
		cs->filter = strdup(filter);
	}
	if (cs->gap == NULL || cs->filter == NULL) {
		fprintf (stderr, "recipe_obj_add_chan: malloc() error for object '%s'.\n",
										name);
		return (0);
	}
	cs->tr = NULL;
	cs->power_tr = NULL;
	cs->sp = NULL;
	switch (itype) {
	case XYGRID:
	case STGRID:
	case FTGRID:
	case BEAM:
	case CROSSCOR:
	case GATHER:
		gs = ptr;
		if (settbl (gs->chans, ichan, cs) < 0) {
			fprintf (stderr, "recipe_obj_add_chan: settbl() error for object '%s'.\n",
											name);
			return (0);
		}
		break;
	default:
		fprintf (stderr, "recipe_obj_add_chan: Unknown object type.\n");
		return (0);
	}
	return (1);
}

int
process_add (str)

char *       str;

{
	char string[2048];

	strcpy (string, str);
	if (!proc_put (string)) {
		fprintf (stderr, "process_add: proc_put() error for '%s'.\n", string);
		return (-1);
	}
	return (1);
}

int
copy_apspec (gs, args, gsout)

Apspec *     gs;
Arr *              args;
Apspec **              gsout;

{
	Apspec *gso;
	Gridnode *gn;
	Channelspec *cs;
	char *value;
	double dx, dy, ds;
	int ix, iy, is, i, j, nnodes, nchans;
	Tbl *chans;
	double tshift;

	gso = (Apspec *) my_malloc ("copy_apspec: gso", sizeof(Apspec));
	if (gso == NULL) {
		fprintf (stderr, "copy_apspec: malloc() error.\n");
		return (0);
	}
	*gsout = gso;
	*gso = *gs;
	switch (gs->type) {
	case BEAM:
		value = getarr (args, "bchan");
		if (value) {
			if (strlen(value) == strlen(gso->chan)) {
				for (j=0; j<strlen(value); j++) {
					if (value[j] == '.') continue;
					gso->chan[j] = value[j];
				}
			} else {
				strcpy (gso->chan, value);
			}
		}
		value = getarr (args, "samprate");
		if (value) {
			gso->samprate = atof(value);
		}
		value = getarr (args, "azimuth");
		if (value) {
			gso->azimuth = atof(value);
		}
		value = getarr (args, "slow");
		if (value) {
			gso->slowi = atof(value);
		}
		gso->grid_power = NULL;
		gso->grid_raw_power = NULL;
		gso->grid_raw_az = NULL;
		gso->grid_raw_sl = NULL;
		gso->tr = NULL;
		gso->grid_nodes = NULL;
		if (!copy_channels(gs->chans, args, &chans)) {
			fprintf (stderr, "copy_apspec: copy_channels() error.\n");
			return (0);
		}
		gso->chans = chans;
		break;
	case GATHER:
		value = getarr (args, "samprate");
		if (value) {
			gso->samprate = atof(value);
		}
		value = getarr (args, "azimuth");
		if (value) {
			gso->azimuth = atof(value);
		}
		gso->grid_power = NULL;
		gso->grid_raw_power = NULL;
		gso->grid_raw_az = NULL;
		gso->grid_raw_sl = NULL;
		gso->tr = NULL;
		gso->grid_nodes = NULL;
		if (!copy_channels(gs->chans, args, &chans)) {
			fprintf (stderr, "copy_apspec: copy_channels() error.\n");
			return (0);
		}
		gso->chans = chans;
		break;
	case CROSSCOR:
		value = getarr (args, "samprate");
		if (value) {
			gso->samprate = atof(value);
		}
		value = getarr (args, "azimuth");
		if (value) {
			gso->azimuth = atof(value);
		}
		value = getarr (args, "slow");
		if (value) {
			gso->slowi = atof(value);
		}
		gso->grid_power = NULL;
		gso->grid_raw_power = NULL;
		gso->grid_raw_az = NULL;
		gso->grid_raw_sl = NULL;
		gso->tr = NULL;
		gso->grid_nodes = NULL;
		if (!copy_channels(gs->chans, args, &chans)) {
			fprintf (stderr, "copy_apspec: copy_channels() error.\n");
			return (0);
		}
		gso->chans = chans;
		break;
	case XYGRID:
		value = getarr (args, "samprate");
		if (value) {
			gso->samprate = atof(value);
		}
		value = getarr (args, "azimuth");
		if (value) {
			gso->azimuth = atof(value);
		}
		value = getarr (args, "slow");
		if (value) {
			gso->slowi = atof(value);
		}
		value = getarr (args, "savegrids");
		if (value) {
			gso->savegrids = atoi(value);
		}
		value = getarr (args, "summary");
		if (value) {
			if (!strcmp(value, "stack")) {
				gso->summary = STACK;
			} else if (!strcmp(value, "best")) {
				gso->summary = BEST;
			}
		}
		gso->grid_power = NULL;
		gso->grid_raw_power = NULL;
		gso->grid_raw_az = NULL;
		gso->grid_raw_sl = NULL;
		gso->grid_nodes = NULL;
		gso->grids = newtbl(1);
		if (gso->grids == NULL) {
			fprintf (stderr, "copy_apspec: newtbl() error.\n");
			return (0);
		}
		if (!copy_channels(gs->chans, args, &chans)) {
			fprintf (stderr, "copy_apspec: copy_channels() error.\n");
			return (0);
		}
		gso->chans = chans;
		gso->x = (float *) my_malloc ("copy_apspec: gso->x", gso->nx*sizeof(float));
		gso->y = (float *) my_malloc ("copy_apspec: gso->y", gso->ny*sizeof(float));
		if (gso->x == NULL || gso->y == NULL) {
			fprintf (stderr, "copy_apspec: malloc() error.\n");
			return (0);
		}
		dx = (gso->xmax-gso->xmin)/(gso->nx-1);
		dy = (gso->ymax-gso->ymin)/(gso->ny-1);
		gso->grid_nodes = newtbl(gso->nx*gso->ny);
		if (gso->grid_nodes == NULL) {
			fprintf (stderr, "copy_apspec: newtbl() error.\n");
			return (0);
		}
		for (ix=0; ix<gso->nx; ix++) {
			gso->x[ix] = gso->xmin + dx*ix;
		}
		for (iy=0; iy<gso->ny; iy++) {
			gso->y[iy] = gso->ymin + dy*iy;
		}
		for (iy=0; iy<gso->ny; iy++) {
			for (ix=0; ix<gso->nx; ix++) {
				gn = (Gridnode *) my_malloc ("copy_apspec: gn", sizeof(Gridnode));
				if (gn == NULL) {
					fprintf (stderr, "copy_apspec: malloc() error.\n");
					return (0);
				}
				gn->sx = gso->xmin + dx*ix;
				gn->sy = gso->ymin + dy*iy;
				gn->grid = 0.0;
				if (settbl (gso->grid_nodes, -1, gn) < 0) {
					fprintf (stderr, "copy_apspec: settbl() error.\n");
					return (0);
				}
			}
		}
		if (!setup_geom_grid (dbi, gso)) {
			fprintf (stderr, "copy_apspec: setup_geom_grid() error.\n");
			return (0);
		}
		nnodes = maxtbl (gso->grid_nodes);
		nchans = maxtbl (gso->chans);
		gso->tshift_min = 1.e30;
		gso->tshift_max = -1.e30;
		for (i=0; i<nchans; i++) {
			cs = (Channelspec *) gettbl (gso->chans, i);
			for (j=0; j<nnodes; j++) {
				gn = (Gridnode *) gettbl (gso->grid_nodes, j);
				tshift = gn->sx*cs->dx + gn->sy*cs->dy + cs->tcor;
				if (tshift > gso->tshift_max) gso->tshift_max = tshift;
				if (tshift < gso->tshift_min) gso->tshift_min = tshift;
			}
		}
		break;
	case STGRID:
		value = getarr (args, "samprate");
		if (value) {
			gso->samprate = atof(value);
		}
		value = getarr (args, "azimuth");
		if (value) {
			gso->azimuth = atof(value);
		}
		value = getarr (args, "slow");
		if (value) {
			gso->slowi = atof(value);
		}
		gso->grid_power = NULL;
		gso->grid_raw_power = NULL;
		gso->grid_raw_az = NULL;
		gso->grid_raw_sl = NULL;
		gso->grid_nodes = NULL;
		if (!copy_channels(gs->chans, args, &chans)) {
			fprintf (stderr, "copy_apspec: copy_channels() error.\n");
			return (0);
		}
		gso->chans = chans;
		gso->s = (float *) my_malloc ("copy_apspec: gso->s", gso->ns*sizeof(float));
		if (gso->s == NULL) {
			fprintf (stderr, "copy_apspec: malloc() error.\n");
			return (0);
		}
		ds = (gso->smax-gso->smin)/(gso->ns-1);
		gso->grid_nodes = newtbl(gso->ns);
		if (gso->grid_nodes == NULL) {
			fprintf (stderr, "copy_apspec: newtbl() error.\n");
			return (0);
		}
		for (is=0; is<gso->ns; is++) {
			gso->s[is] = gso->smin + ds*is;
		}
		for (is=0; is<gso->ns; is++) {
			gn = (Gridnode *) my_malloc ("copy_apspec: gn", sizeof(Gridnode));
			if (gn == NULL) {
				fprintf (stderr, "copy_apspec: malloc() error.\n");
				return (0);
			}
			gn->sx = gso->s[is]*sin(gso->azimuth*M_PI/180.0);
			gn->sy = gso->s[is]*cos(gso->azimuth*M_PI/180.0);
			gn->grid = 0.0;
			if (settbl (gso->grid_nodes, -1, gn) < 0) {
				fprintf (stderr, "copy_apspec: settbl() error.\n");
				return (0);
			}
		}
		if (!setup_geom_grid (dbi, gso)) {
			fprintf (stderr, "copy_apspec: setup_geom_grid() error.\n");
			return (0);
		}
		nnodes = maxtbl (gso->grid_nodes);
		nchans = maxtbl (gso->chans);
		gso->tshift_min = 1.e30;
		gso->tshift_max = -1.e30;
		for (i=0; i<nchans; i++) {
			cs = (Channelspec *) gettbl (gso->chans, i);
			for (j=0; j<nnodes; j++) {
				gn = (Gridnode *) gettbl (gso->grid_nodes, j);
				tshift = gn->sx*cs->dx + gn->sy*cs->dy + cs->tcor;
				if (tshift > gso->tshift_max) gso->tshift_max = tshift;
				if (tshift < gso->tshift_min) gso->tshift_min = tshift;
			}
		}
		break;
	case FTGRID:
		value = getarr (args, "samprate");
		if (value) {
			gso->samprate = atof(value);
		}
		value = getarr (args, "azimuth");
		if (value) {
			gso->azimuth = atof(value);
		}
		value = getarr (args, "slow");
		if (value) {
			gso->slowi = atof(value);
		}
		gso->grid_power = NULL;
		gso->grid_raw_power = NULL;
		gso->grid_raw_az = NULL;
		gso->grid_raw_sl = NULL;
		gso->grid_nodes = NULL;
		if (!copy_channels(gs->chans, args, &chans)) {
			fprintf (stderr, "copy_apspec: copy_channels() error.\n");
			return (0);
		}
		gso->chans = chans;
		break;
	default:
		fprintf (stderr, "copy_apspec: Unknown recipe type.\n");
	    	return (0);
	}
	return (1);
}

int
copy_channels (chans, args, ochans)

Tbl *          chans;
Arr *                 args;
Tbl **                      ochans;

{
	int i, j, n;
	Channelspec *cs, *cso;
	char *value;

	n = maxtbl(chans);
	*ochans = newtbl(n);
	if (*ochans == NULL) {
		fprintf (stderr, "copy_channels: newtbl() error.\n");
		return (0);
	}
	for (i=0; i<n; i++) {
		cs = (Channelspec *) gettbl (chans, i);
		cso = (Channelspec *) my_malloc ("copy_channels: cso", sizeof(Channelspec));
		if (cso == NULL) {
			fprintf (stderr, "copy_channels: malloc() error.\n");
			return (0);
		}
		*cso = *cs;
		value = getarr (args, "chan");
		if (value) {
			if (strlen(value) == strlen(cso->chan)) {
				for (j=0; j<strlen(value); j++) {
					if (value[j] == '.') continue;
					cso->chan[j] = value[j];
				}
			} else {
				strcpy (cso->chan, value);
			}
		}
		cso->gap = strdup(cs->gap);
		value = getarr (args, "filter");
		if (value) {
			cso->filter = strdup(value);
		} else {
			cso->filter = strdup(cs->filter);
		}
		if (cso->gap == NULL || cso->filter == NULL) {
			fprintf (stderr, "copy_channels: malloc() error.\n");
			return (0);
		}
		if (settbl (*ochans, -1, cso) < 0) {
			fprintf (stderr, "copy_channels: settbl() error.\n");
			return (0);
		}
		cso->tr = NULL;
		cso->power_tr = NULL;
		cso->sp = NULL;
	}

	return (1);
}

int
free_apspec (ap)

Apspec *     ap;

{
	int i, n;
	Gridnode *gn;
	Channelspec *cs;
	Grid *grid;

	if (ap == NULL) return (1);
	if (ap->x) my_free (ap->x);
	if (ap->y) my_free (ap->y);
	if (ap->t) my_free (ap->t);
	if (ap->s) my_free (ap->s);
	if (ap->norm_power_grid) my_free (ap->norm_power_grid);
	if (ap->power_grid) my_free (ap->power_grid);
	if (ap->grid_power) SCV_free_trace (ap->grid_power);
	if (ap->grid_raw_power) SCV_free_trace (ap->grid_raw_power);
	if (ap->grid_raw_az) SCV_free_trace (ap->grid_raw_az);
	if (ap->grid_raw_sl) SCV_free_trace (ap->grid_raw_sl);
	if (ap->tr) SCV_free_trace (ap->tr);
	if (ap->grid_nodes) {
		n = maxtbl(ap->grid_nodes);
		for (i=0; i<n; i++) {
			gn = (Gridnode *) gettbl(ap->grid_nodes, i);
			if (gn) my_free (gn);
		}
		freetbl (ap->grid_nodes, NULL);
	}
	if (ap->chans) {
		n = maxtbl(ap->chans);
		for (i=0; i<n; i++) {
			cs = (Channelspec *) gettbl(ap->chans, i);
			if (cs) {
				if (cs->gap) my_free (cs->gap);
				if (cs->filter) my_free (cs->filter);
				if (cs->tr) SCV_free_trace (cs->tr);
				if (cs->power_tr) SCV_free_trace (cs->power_tr);
				if (cs->sp) {
					if (cs->sp->fftr) free (cs->sp->fftr);
					if (cs->sp->ffti) free (cs->sp->ffti);
					if (cs->sp->fftt) free (cs->sp->fftt);
					free (cs->sp);
				}
				my_free (cs);
			}
		}
		freetbl (ap->chans, NULL);
	}
	if (ap->grids) {
		n = maxtbl(ap->grids);
		for (i=0; i<n; i++) {
			grid = (Grid *) gettbl(ap->grids, i);
			if (grid) {
				if (grid->power) my_free (grid->power);
				my_free (grid);
			}
		}
		freetbl (ap->grids, NULL);
	}
	if (ap->norm_summary_grid.power) my_free (ap->norm_summary_grid.power);
	if (ap->summary_grid.power) my_free (ap->summary_grid.power);
	my_free (ap);
	return (1);
}

int
recipe_to_process (args, recipes, nrecipes, name)

Arr *              args;
char **                  recipes;
int                               nrecipes;
char **                                     name;

{
	int i;
	char *type;
	int itype;
	void *ptr;
	Tbl *rtbl;
	Prcrcp *prcrcp;
	Apspec *gs;
	char nam[32];
	int index;

	rtbl = newtbl(nrecipes);
	if (rtbl == NULL) {
		fprintf (stderr, "recipe_to_process: newtbl() error.\n");
		return (0);
	}
	for (i=0; i<nrecipes; i++) {
		if (!recipe_obj_get_entry (recipes[i], &type, &itype, &ptr)) {
			fprintf (stderr, "recipe_to_process: Undefined recipe object '%s'.\n", recipes[i]);
			return (0);
		}
		switch (itype) {
		case STGRID:
		case FTGRID:
		case XYGRID:
		case BEAM:
		case CROSSCOR:
		case GATHER:
			if (!copy_apspec (ptr, args, &gs)) {
				fprintf (stderr, "recipe_to_process: copy_apspec() error.\n");
				return (0);
			}
			ptr = gs;
			break;
		default:
			fprintf (stderr, "recipe_to_process: Unknown recipe type '%s' for %s.\n", type,
											recipes[i]);
			return (0);
		}
		prcrcp = (Prcrcp *) my_malloc ("recipe_to_process: prcrcp", sizeof(Prcrcp));
		if (prcrcp == NULL) {
			fprintf (stderr, "recipe_to_process: malloc() error.\n");
			return (0);
		}
		strcpy (prcrcp->name, recipes[i]);
		prcrcp->itype = itype;
		prcrcp->ptr = ptr;
		if (settbl (rtbl, -1, prcrcp) < 0) {
			fprintf (stderr, "recipe_to_process: settbl() error.\n");
			return (0);
		}
	}
	if (!process_obj_add_entry (nam, "process", PROCESS, NULL, rtbl)) {
		fprintf (stderr, "recipe_to_process: process_obj_add_entry() error.\n");
		return (0);
	}
	*name = strdup(nam);
	if (*name == NULL) {
		fprintf (stderr, "recipe_to_process: strdup() error.\n");
		return (0);
	}

	return (1);
}

int
process_to_process (args, process_name, name)

Arr *               args;
char *                    process_name;
char **                                 name;

{
	int i;
	char *type;
	int itype;
	char *value;
	void *ptr;
	Tbl *recipes;
	int nrecipes;
	Prcrcp *prcrcp;
	Apspec *gs;
	char *nam;
	char strnam[32];
	Tbl *rtbl;
	int index;

	if (!process_obj_get_entry (process_name, &type, &itype, &value, &recipes)) {
		fprintf (stderr, "process_to_process: process_obj_get_entry() error for '%s'.\n", process_name);
		return (0);
	}
	nrecipes = maxtbl(recipes);
	rtbl = newtbl(nrecipes);
	if (rtbl == NULL) {
		fprintf (stderr, "recipe_to_process: newtbl() error.\n");
		return (0);
	}
	for (i=0; i<nrecipes; i++) {
		if (!get_process_recipe (recipes, i, &nam, &itype, &ptr)) {
			fprintf (stderr, "process_to_process: get_process_recipe() error for '%s'.\n", process_name);
			return (0);
		}
		switch (itype) {
		case STGRID:
		case FTGRID:
		case XYGRID:
		case BEAM:
		case CROSSCOR:
		case GATHER:
			if (!copy_apspec (ptr, args, &gs)) {
				fprintf (stderr, "process_to_process: copy_apspec() error.\n");
				return (0);
			}
			ptr = gs;
			break;
		default:
			fprintf (stderr, "process_to_process: Unknown recipe type '%s' for %s.\n", type,
											recipes[i]);
			return (0);
		}
		prcrcp = (Prcrcp *) my_malloc ("process_to_process: prcrcp", sizeof(Prcrcp));
		if (prcrcp == NULL) {
			fprintf (stderr, "process_to_process: malloc() error.\n");
			return (0);
		}
		strcpy (prcrcp->name, nam);
		prcrcp->itype = itype;
		prcrcp->ptr = ptr;
		if (settbl (rtbl, -1, prcrcp) < 0) {
			fprintf (stderr, "process_to_process: settbl() error.\n");
			return (0);
		}
	}
	if (!process_obj_add_entry (strnam, "process", PROCESS, NULL, rtbl)) {
		fprintf (stderr, "process_to_process: process_obj_add_entry() error.\n");
		return (0);
	}
	*name = strdup(strnam);
	if (*name == NULL) {
		fprintf (stderr, "process_to_process: strdup() error.\n");
		return (0);
	}

	return (1);
}

int
get_process_recipe (recipes, index, name, itype, ptr)

Tbl *               recipes;
int                          index;
char **                             name;
int *                                     itype;
void **                                          ptr;

{
	int n;
	Prcrcp *prcrcp;

	n = maxtbl(recipes);
	if (index < 0 || index >= n) {
		fprintf (stderr, "get_process_recipe: Index out of range.\n");
		return (0);
	}
	prcrcp = (Prcrcp *) gettbl (recipes, index);
	if (prcrcp == NULL) {
		fprintf (stderr, "get_process_recipe: gettbl() error.\n");
		return (0);
	}
	*name = prcrcp->name;
	*itype = prcrcp->itype;
	*ptr = prcrcp->ptr;
	return (1);
}

int
recipe_obj_add_entry (name, type, itype, ptr)

char *                name;
char *                      type;
int                               itype;
void *                                   ptr;

{
	Rcpobj *obj;

	if (recipe_objs == NULL) {
		recipe_objs = newarr(NULL);
		if (recipe_objs == NULL) {
			fprintf (stderr, "recipe_obj_add_entry: newarr() error.\n");
			return (0);
		}
	}
	obj = (Rcpobj *) my_malloc ("recipe_obj_add_entry: obj", sizeof(Rcpobj));
	if (obj == NULL) {
		fprintf (stderr, "recipe_obj_add_entry: malloc() error.\n");
		return (0);
	}
	strcpy (obj->type, type);
	obj->itype = itype;
	obj->ptr = ptr;
	setarr (recipe_objs, name, obj);
	return (1);
}

int
process_obj_add_entry (name, type, itype, value, recipes)

char *                 name;
char *                       type;
int                                itype;
char *                                    value;
Tbl *                                            recipes;

{
	Prcobj *obj;
	static int index=0;

	if (process_objs == NULL) {
		process_objs = newarr(NULL);
		if (process_objs == NULL) {
			fprintf (stderr, "process_obj_add_entry: newarr() error.\n");
			return (0);
		}
	}
	obj = (Prcobj *) my_malloc ("process_obj_add_entry: obj", sizeof(Prcobj));
	if (obj == NULL) {
		fprintf (stderr, "process_obj_add_entry: malloc() error.\n");
		return (0);
	}
	strcpy (obj->type, type);
	obj->itype = itype;
	obj->value = NULL;
	if (value) {
		obj->value = strdup(value);
		if (obj->value == NULL) {
			fprintf (stderr, "process_obj_add_entry: strdup() error.\n");
			return (0);
		}
	}
	obj->recipes = recipes;
	sprintf (name, "%%%d", index++);
	setarr (process_objs, name, obj);
	return (1);
}

int
process_obj_free_entry (name)

char *                  name;

{
	Prcobj *obj;
	char *value2;

	if (process_objs == NULL) {
		return (1);
	}
	if (*name != '%') {
		var_get (name, &value2);
		if (value2 == NULL) {
			return (1);
		}
		name = value2;
	}
	obj = (Prcobj *) delarr (process_objs, name);
	if (obj == NULL) {
		return (1);
	}
	if (obj->recipes) freetbl (obj->recipes, NULL);
	my_free (obj);
	return (1);
}

int
recipe_obj_get_entry (name, type, itype, ptr)

char *                name;
char **                     type;
int *                             itype;
void **                                  ptr;

{
	Rcpobj *obj;

	if (recipe_objs == NULL) {
		fprintf (stderr, "recipe_obj_get_entry: No recipe object entries.\n");
		return (0);
	}
	obj = (Rcpobj *) getarr (recipe_objs, name);
	if (obj == NULL) {
		fprintf (stderr, "recipe_obj_get_entry: No entry for '%s'.\n", name);
		return (0);
	}
	*ptr = obj->ptr;
	*type = obj->type;
	*itype = obj->itype;
	return (1);
}

int
process_obj_get_entry (name, type, itype, value, recipes)

char *                 name;
char **                      type;
int *                              itype;
char **                                   value;
Tbl **                                           recipes;

{
	Prcobj *obj;
	char *value2;

	if (process_objs == NULL) {
		fprintf (stderr, "process_obj_get_entry: No process object entries.\n");
		return (0);
	}
	if (*name != '%') {
		var_get (name, &value2);
		if (value2 == NULL) {
			fprintf (stderr, "process_obj_get_entry: No entry for '%s'.\n", name);
			return (0);
		}
		name = value2;
	}
	obj = (Prcobj *) getarr (process_objs, name);
	if (obj == NULL) {
		fprintf (stderr, "process_obj_get_entry: No entry for '%s'.\n", name);
		return (0);
	}
	*recipes = obj->recipes;
	*type = obj->type;
	*itype = obj->itype;
	*value = obj->value;
	return (1);
}

int
recipe_obj_get_entry_names (names)

Tbl **                      names;

{
	Rcpobj *obj;

	if (recipe_objs == NULL) {
		fprintf (stderr, "recipe_obj_get_entry_names: No recipe object entries.\n");
		return (0);
	}
	*names = keysarr (recipe_objs);
	return (1);
}

int
var_put (name, value)

char *   name;
char *         value;

{
	if (varlist == NULL) {
		varlist = newarr(NULL);
		if (varlist == NULL) {
			fprintf (stderr, "var_put: newarr() error.\n");
			return (0);
		}
	}
	setarr (varlist, name, strdup(value));
	return (1);
}

int
var_get (name, value)

char *   name;
char **        value;

{
	*value = NULL;
	if (varlist == NULL) {
		return (1);
	}
	*value = (char *) getarr (varlist, name);
	return (1);
}

Tbl *
var_get_names ()

{
	if (varlist == NULL) {
		return (NULL);
	}
	return (keysarr (varlist));
}

int
proc_put (value)

char *    value;

{
	if (proclist == NULL) {
		proclist = newtbl(1);
		if (proclist == NULL) {
			fprintf (stderr, "proc_put: newtbl() error.\n");
			return (0);
		}
	}
	settbl (proclist, -1, strdup(value));
	return (1);
}

int
proc_get (index, value)

int       index;
char **          value;

{
	*value = NULL;
	if (proclist == NULL) {
		return (1);
	}
	*value = (char *) gettbl (proclist, index);
	return (1);
}

int
proc_get_n ()

{
	if (proclist == NULL) return (0);
	return (maxtbl(proclist));
}

/* $Id: objs.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
