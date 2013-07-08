
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "stock.h"
#include "pf.h"

int
parse_pf (pf_file)

char *pf_file;

{
	Pf *pf, *result;
	Arr *objs;
	Tbl *procs;

	/* Open and read pf file */

	if (pfread (pf_file, &pf)) {
		clear_register (1);
		fprintf (stderr, "parse_pf: pfread() error.\n");
		return (0);
	}

	/* Look for recipe objects */

	switch (pfget(pf, "recipe_objs", &result)) {
	case PFARR:
		objs = result->value.arr;
		break;
	default:
		fprintf (stderr, "parse_pf: Illegal recipe objects in file '%s.pf'\n", pf_file);
		return (0);
	}

	/* Parse recipe objects */

	if (!parse_recipes (objs)) {
		clear_register (1);
		fprintf (stderr, "parse_pf: parse_recipes() error for file '%s.pf'\n", pf_file);
		return (0);
	}

	/* Normal exit */

	return (1);
}

int
parse_recipes (objs)

Arr *          objs;

{
	Tbl *obj_names;
	Pf *pfo, *result;
	Tbl *chans;
	int i, j, k, n, m;
	char *str;
	char *type, *params;
	char *sta, *chan, *gaps, *filter;
	double wt, tcor;

	if (objs == NULL) return (1);
	obj_names = keysarr (objs);
	n = maxtbl (obj_names);
	for (i=0; i<n; i++) {
		pfo = (Pf *) getarr (objs, (char *) gettbl (obj_names, i));
		str = pfget_string (pfo, "type");
		if (str == NULL) {
			fprintf (stderr, "parse_recipes: No type for recipe object '%s'\n", 
								(char *) gettbl (obj_names, i));
			return (0);
		}
		type = str;
		str = pfget_string (pfo, "params");
		if (str == NULL) {
			fprintf (stderr, "parse_recipes: No params for recipe object '%s'\n", 
								(char *) gettbl (obj_names, i));
			return (0);
		}
		params = str;
		switch (pfget(pfo, "channels", &result)) {
		case PFTBL:
			chans = result->value.tbl;
			break;
		default:
			fprintf (stderr, "parse_recipes: Illegal channels for recipe object '%s'\n",
								(char *) gettbl (obj_names, i));
			return (0);
		}
		m = maxtbl(chans);
		for (j=0,k=0; j<m; j++) {
			str = pfget_string (result, j);
			if (*str == '\0') continue;
			if (!parse_channel (str, &sta, &chan, &wt, &tcor, &gaps, &filter)) {
				fprintf (stderr, "parse_recipes: parse_channel() error for recipe object '%s'\n",
								(char *) gettbl (obj_names, i));
				return (0);
			}
			k++;
		}
		if (k < 1) {
			fprintf (stderr, "parse_recipes: No channels for recipe object '%s'\n",
								(char *) gettbl (obj_names, i));
			return (0);
		}

		/* Everything looks OK for this recipe object */

		if (!recipe_obj_add ((char *) gettbl (obj_names, i), type, params, k)) {
			fprintf (stderr, "parse_recipes: recipe_obj_add() error for recipe object '%s'\n",
								(char *) gettbl (obj_names, i));
			return (0);
		}
		for (j=0,k=0; j<m; j++) {
			str = pfget_string (result, j);
			if (*str == '\0') continue;
			if (!recipe_obj_add_chan ((char *) gettbl (obj_names, i), k, str)) {
				fprintf (stderr, "parse_recipes: recipe_obj_add_chan() error for recipe object '%s'\n",
								(char *) gettbl (obj_names, i));
				return (0);
			}
			k++;
		}
	}
	return (1);
}

int
parse_proctbl (pf, procs)

Pf *           pf;
Tbl *              procs;

{
	int i, n;
	char *str;

	if (procs == NULL) return (1);
	n = maxtbl (procs);
	for (i=0; i<n; i++) {
		str = pfget_string (pf, i);
		if (*str == '\0') continue;
		if (process_add (str) < 0) {
			fprintf (stderr, "parse_proctbl: process_add() error for table entry #%d.\n", i+1);
			return (0);
		}
	}
	return (1);
}

int
parse_channel (str, sta, chan, wt, tcor, gaps, filter)

char *         str;
char **             sta;
char **                  chan;
double *                       wt;
double *                           tcor;
char **                                  gaps;
char **                                        filter;

{
	static char line[1024];
	static char fil[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*sta = ptr;
			break;
		case 2:
			*chan = ptr;
			break;
		case 3:
			*wt = atof(ptr);
			break;
		case 4:
			*tcor = atof(ptr);
			break;
		case 5:
			*gaps = ptr;
			break;
		case 6:
			strcpy (fil, ptr);
			break;
		default:
			strcat (fil, " ");
			strcat (fil, ptr);
			break;
		}
		i++;
	}
	i--;
	if (i < 6) {
		fprintf (stderr, "parse_channel: Too few fields in channel spec\n");
		fprintf (stderr, "               %s\n", str);
		fprintf (stderr, "               should be\n");
		fprintf (stderr, "               sta	chan	wt	tcor	gaps	filter\n");
		return (0);
	}
	*filter = fil;

	return (1);
}

int
parse_gather_params (str, refsta, tpad, samprate)

char *               str;
char **                   refsta;
double *                          tpad;
double *                                samprate;

{
	static char line[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	*samprate = 0.0;
	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*refsta = ptr;
			break;
		case 2:
			*tpad = atof(ptr);
			break;
		case 3:
			*samprate = atof(ptr);
			break;
		default:
			fprintf (stderr, "parse_gather_params: Too many fields in gather spec\n");
			fprintf (stderr, "                     %s\n", str);
			fprintf (stderr, "                     should be\n");
			fprintf (stderr, "                     refsta	tpad	samprate\n");
			return (0);
		}
		i++;
	}
	i--;
	if (i < 3) {
		fprintf (stderr, "parse_gather_params: Too few fields in gather spec\n");
		fprintf (stderr, "                     %s\n", str);
		fprintf (stderr, "                     should be\n");
		fprintf (stderr, "                     refsta	tpad	samprate\n");
		return (0);
	}
	return (1);
}

int
parse_beam_params (str, array, refsta, chan, azimuth, slow, tpad, samprate)

char *             str;
char **                 array;
char **                        refsta;
char **                                chan;
double *                                     azimuth;
double *                                              slow;
double *                                                    tpad;
double *                                                          samprate;

{
	static char line[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	*samprate = 0.0;
	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*array = ptr;
			break;
		case 2:
			*refsta = ptr;
			break;
		case 3:
			*chan = ptr;
			break;
		case 4:
			*azimuth = atof(ptr);
			break;
		case 5:
			*slow = atof(ptr);
			break;
		case 6:
			*tpad = atof(ptr);
			break;
		case 7:
			*samprate = atof(ptr);
			break;
		default:
			fprintf (stderr, "parse_beam_params: Too many fields in beam spec\n");
			fprintf (stderr, "                   %s\n", str);
			fprintf (stderr, "                   should be\n");
			fprintf (stderr, "                   array	refsta	chan	azimuth	slow	tpad	samprate\n");
			return (0);
		}
		i++;
	}
	i--;
	if (i < 7) {
		fprintf (stderr, "parse_beam_params: Too few fields in beam spec\n");
		fprintf (stderr, "                   %s\n", str);
		fprintf (stderr, "                   should be\n");
		fprintf (stderr, "                   array	refsta	chan	azimuth	slow	tpad	samprate\n");
		return (0);
	}
	return (1);
}

int
parse_crosscor_params (str, refsta, azimuth, slow, tpad, samprate)

char *                 str;
char **                     refsta;
double *                            azimuth;
double *                                     slow;
double *                                           tpad;
double *                                                 samprate;

{
	static char line[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	*samprate = 0.0;
	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*refsta = ptr;
			break;
		case 2:
			*azimuth = atof(ptr);
			break;
		case 3:
			*slow = atof(ptr);
			break;
		case 4:
			*tpad = atof(ptr);
			break;
		case 5:
			*samprate = atof(ptr);
			break;
		default:
			fprintf (stderr, "parse_crosscor_params: Too many fields in beam spec\n");
			fprintf (stderr, "                       %s\n", str);
			fprintf (stderr, "                       should be\n");
			fprintf (stderr, "                       refsta	azimuth	slow	tpad	samprate\n");
			return (0);
		}
		i++;
	}
	i--;
	if (i < 5) {
		fprintf (stderr, "parse_crosscor_params: Too few fields in beam spec\n");
		fprintf (stderr, "                       %s\n", str);
		fprintf (stderr, "                       should be\n");
		fprintf (stderr, "                       refsta	azimuth	slow	tpad	samprate\n");
		return (0);
	}
	return (1);
}

int
parse_xygrid_params (str, array, refsta, nx, xmin, xmax, ny, ymin, ymax, tpad, samprate)

char *               str;
char **                   array;
char **                          refsta;
int *                                    nx;
double *                                     xmin;
double *                                           xmax;
int *                                                    ny;
double *                                                     ymin;
double *                                                           ymax;
double *                                                                 tpad;
double *                                                                       samprate;

{
	static char line[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	*samprate = 0.0;
	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*array = ptr;
			break;
		case 2:
			*refsta = ptr;
			break;
		case 3:
			*nx = atoi(ptr);
			break;
		case 4:
			*xmin = atof(ptr);
			break;
		case 5:
			*xmax = atof(ptr);
			break;
		case 6:
			*ny = atoi(ptr);
			break;
		case 7:
			*ymin = atof(ptr);
			break;
		case 8:
			*ymax = atof(ptr);
			break;
		case 9:
			*tpad = atof(ptr);
			break;
		case 10:
			*samprate = atof(ptr);
			break;
		default:
			fprintf (stderr, "parse_xygrid_params: Too many fields in xygrid spec\n");
			fprintf (stderr, "                     %s\n", str);
			fprintf (stderr, "                     should be\n");
			fprintf (stderr, "                     array	refsta	nx	xmin	xmax	ny	ymin	ymax	tpad	samprate\n");
			return (0);
		}
		i++;
	}
	i--;
	if (i < 9) {
		fprintf (stderr, "parse_xygrid_params: Too few fields in xygrid spec\n");
		fprintf (stderr, "                     %s\n", str);
		fprintf (stderr, "                     should be\n");
		fprintf (stderr, "                     array	refsta	nx	xmin	xmax	ny	ymin	ymax	tpad	samprate\n");
		return (0);
	}
	return (1);
}

int
parse_stgrid_params (str, array, refsta, azimuth, ns, smin, smax, tpad, samprate)

char *               str;
char **                   array;
char **                          refsta;
double *                                 azimuth;
int *                                             ns;
double *                                              smin;
double *                                                    smax;
double *                                                          tpad;
double *                                                                samprate;

{
	static char line[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	*samprate = 0.0;
	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*array = ptr;
			break;
		case 2:
			*refsta = ptr;
			break;
		case 3:
			*azimuth = atof(ptr);
			break;
		case 4:
			*ns = atoi(ptr);
			break;
		case 5:
			*smin = atof(ptr);
			break;
		case 6:
			*smax = atof(ptr);
			break;
		case 7:
			*tpad = atof(ptr);
			break;
		case 8:
			*samprate = atof(ptr);
			break;
		default:
			fprintf (stderr, "parse_stgrid_params: Too many fields in stgrid spec\n");
			fprintf (stderr, "                     %s\n", str);
			fprintf (stderr, "                     should be\n");
			fprintf (stderr, "                     array	refsta	azimuth	ns	smin	smax	tpad	samprate\n");
			return (0);
		}
		i++;
	}
	i--;
	if (i < 7) {
		fprintf (stderr, "parse_stgrid_params: Too few fields in stgrid spec\n");
		fprintf (stderr, "                     %s\n", str);
		fprintf (stderr, "                     should be\n");
		fprintf (stderr, "                     array	refsta	azimuth	ns	smin	smax	tpad	samprate\n");
		return (0);
	}
	return (1);
}

int
parse_ftgrid_params (str, array, refsta, azimuth, slow, fmin, fmax, tpad, samprate)

char *               str;
char **                   array;
char **                          refsta;
double *                                 azimuth;
double *                                          slow;
double *                                                fmin;
double *                                                      fmax;
double *                                                            tpad;
double *                                                                  samprate;

{
	static char line[1024];
	char *ptr;
	int i;

	strcpy (line, str);

	*samprate = 0.0;
	for (ptr=strtok(line, " \t"),i=1; ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (i) {
		case 1:
			*array = ptr;
			break;
		case 2:
			*refsta = ptr;
			break;
		case 3:
			*azimuth = atof(ptr);
			break;
		case 4:
			*slow = atof(ptr);
			break;
		case 5:
			*fmin = atof(ptr);
			break;
		case 6:
			*fmax = atof(ptr);
			break;
		case 7:
			*tpad = atof(ptr);
			break;
		case 8:
			*samprate = atof(ptr);
			break;
		default:
			fprintf (stderr, "parse_ftgrid_params: Too many fields in ftgrid spec\n");
			fprintf (stderr, "                     %s\n", str);
			fprintf (stderr, "                     should be\n");
			fprintf (stderr, "                     array	refsta	azimuth	slow	fmin	fmax	tpad	samprate\n");
			return (0);
		}
		i++;
	}
	i--;
	if (i < 7) {
		fprintf (stderr, "parse_ftgrid_params: Too few fields in ftgrid spec\n");
		fprintf (stderr, "                     %s\n", str);
		fprintf (stderr, "                     should be\n");
		fprintf (stderr, "                     array	refsta	azimuth	slow	fmin	fmax	tpad	samprate\n");
		return (0);
	}
	return (1);
}

/* $Id: parse_pf.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
