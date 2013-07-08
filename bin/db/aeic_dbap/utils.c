
#include "dbap_defines.h"

Dbptr dbi;

int
get_overall_times (ts, te)

double *           ts;
double *               te;

{
	Tbl *names;
	char *name;
	void *ptr;
	Apspec *gs;
	int i, n;
	char *type;
	int itype;
	double tpad;
	char *value1, *value2;
	int arid;
	Dbptr dbarr;
	char expr[512];
	double time;

	/* Get maximum time pad */

	if (!recipe_obj_get_entry_names (&names)) {
		fprintf (stderr, "get_overall_times: recipe_obj_get_entry_names() error.\n");
		return (0);
	}
	n = maxtbl(names);
	tpad = 0.0;
	for (i=0; i<n; i++) {
		name = (char *) gettbl (names, i);
		if (!recipe_obj_get_entry (name, &type, &itype, &ptr)) {
			fprintf (stderr, "get_overall_times: recipe_obj_get_entry() error.\n");
			return (0);
		}
		switch (itype) {
		case XYGRID:
		case STGRID:
		case FTGRID:
		case BEAM:
		case CROSSCOR:
		case GATHER:
			gs = ptr;
			if (gs->tpad > tpad) tpad = gs->tpad;
			break;
		default:
			fprintf (stderr, "get_overall_times: Unknown recipe object type.\n");
			return (0);
		}
	}

	/* Look for -ts and -te values */

	var_get ("ts", &value1);
	var_get ("te", &value2);
	if (value1 && value2) {
		*ts = atof(value1) - tpad;
		*te = atof(value2) + tpad;
		return (1);
	}

	/* Look for -arid values */

	var_get ("arid", &value1);
	if (value1) {
		arid = atoi(value1);
		dbarr = dblookup (dbi, 0, "arrival", 0, 0);
		sprintf (expr, "( arid == %d )", arid);
		dbarr = dbsubset (dbarr, expr, 0);
		dbarr.record = 0;
		if (dbgetv(dbarr, 0, "time", &time, NULL) == dbINVALID) {
			clear_register (1);
			fprintf (stderr, "get_overall_times: Unable to find arid %d.\n", arid);
			return (0);
		}
		var_get ("arid_t0", &value1);
		if (value1) {
			*ts = time + atof(value1);
		} else {
			*ts = time;
		}
		var_get ("arid_tw", &value1);
		if (value1) {
			*te = *ts + atof(value1);
		} else {
			*te = *ts + 1.0;
		}
		*ts -= tpad;
		*te += tpad;
		return (1);
	}

	/* Unable to find time ranges - set to infinity */

	*ts = 0.0;
	*te = 9999999999.999;

	return (1);
}

int
get_times (args, ts, te)

Arr *      args;
double *         ts;
double *             te;

{
	char *value1, *value2;
	int arid;
	Dbptr dbarr;
	char expr[512];
	double time;


	/* Look for -ts and -te values */

	value1 = getarr (args, "ts");
	value2 = getarr (args, "te");
	if (value1 && value2) {
		*ts = atof(value1);
		*te = atof(value2);
		return (1);
	}

	/* Look for -arid values */

	value1 = getarr (args, "arid");
	if (value1) {
		arid = atoi(value1);
		dbarr = dblookup (dbi, 0, "arrival", 0, 0);
		sprintf (expr, "( arid == %d )", arid);
		dbarr = dbsubset (dbarr, expr, 0);
		dbarr.record = 0;
		if (dbgetv(dbarr, 0, "time", &time, NULL) == dbINVALID) {
			clear_register (1);
			fprintf (stderr, "get_times: Unable to find arid %d.\n", arid);
			return (0);
		}
		value1 = getarr (args, "arid_t0");
		if (value1) {
			*ts = time + atof(value1);
		} else {
			*ts = time;
		}
		value1 = getarr (args, "arid_tw");
		if (value1) {
			*te = *ts + atof(value1);
		} else {
			*te = *ts + 1.0;
		}
		return (1);
	}

	/* Unable to find time ranges */

	fprintf (stderr, "get_times: No time ranges specified.\n");
	return (0);
}

int
get_list (string, nitems, items)

char *    string;
int *             nitems;
char ***                  items;

{
	char str[2048];
	char *ptr;
	char *item;
	int i;

	strcpy (str, string);
	*nitems = 0;
	*items = (char **) my_malloc ("get_list: *items", sizeof(char *));
	if (*items == NULL) {
		fprintf (stderr, "get_list: malloc() error.\n");
		return (0);
	}
	for (ptr=strtok(str, " \t"); ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		item = strdup(ptr);
		if (item == NULL) {
			fprintf (stderr, "get_list: strdup() error.\n");
			for (i=0; i<*nitems; i++) free ((*items)[i]);
			my_free (*items);
			*nitems = 0;
			*items = NULL;
			return (0);
		}
		*items = (char **) my_realloc ("get_list_times: *items", *items, (*nitems+1)*sizeof(char *));
		if (*items == NULL) {
			fprintf (stderr, "get_list: realloc() error.\n");
			*nitems = 0;
			return (0);
		}
		(*items)[*nitems] = item;
		(*nitems)++;
	}
	return (1);
}

int
get_list_times (string, ntimes, times)

char *          string;
int *                   ntimes;
double **                       times;

{
	char str[2048];
	char *ptr;
	double time;

	strcpy (str, string);
	*ntimes = 0;
	*times = (double *) my_malloc ("get_list_times: *times", sizeof(double));
	if (*times == NULL) {
		fprintf (stderr, "get_list_times: malloc() error.\n");
		return (0);
	}
	for (ptr=strtok(str, " \t"); ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		if (!time_string2epoch(ptr, &time)) {
			fprintf (stderr, "get_list_times: Unable to decode '%s'.\n", ptr);
			my_free (*times);
			*ntimes = 0;
			*times = NULL;
			return (0);
		}
		*times = (double *) my_realloc ("get_list_times: *times", *times, (*ntimes+1)*sizeof(double));
		if (*times == NULL) {
			fprintf (stderr, "get_list_times: realloc() error.\n");
			*ntimes = 0;
			return (0);
		}
		(*times)[*ntimes] = time;
		(*ntimes)++;
	}
	return (1);
}

/* $Id: utils.c,v 1.1 2001-06-15 00:18:32 kent Exp $ */
