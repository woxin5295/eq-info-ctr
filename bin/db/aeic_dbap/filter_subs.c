
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "arrays.h"
#include "scv2.h"

#define	BUTWTH		1
#define	BUTWTHZP	2

typedef struct filter_ {
	int type;
	double param1;
	double param2;
	double param3;
	double param4;
	int iparam1;
	int iparam2;
	int iparam3;
	int iparam4;
} Filter;

Arr *filter_arr=NULL;

Trace *
filter_trace (trace, filter, save_oldtrace)

Trace *       trace;
char *               filter;
int                          save_oldtrace;

{
	Filter *fil;
	Trace *trn;

	if (!strcmp(filter, "none")) return (trace);
	if (filter_arr) {
		fil = (Filter *) getarr(filter_arr, filter);
	} else {
		filter_arr = newarr (NULL);
		if (filter_arr == NULL) {
			fprintf (stderr, "filter_trace: newarr() error.\n");
			return (NULL);
		}
		fil = NULL;
	}
	if (!fil) {
		if (!parse_filter (filter, &fil)) {
			fprintf (stderr, "filter_trace: parse_filter() error.\n");
			return (NULL);
		}
		setarr (filter_arr, filter, fil);
	}
	trn = (Trace *) copy_trace (trace, 1);
	if (trn == NULL) {
		fprintf (stderr, "filter_trace: copy_trace() error.\n");
		return (NULL);
	}
	if (!filter_tr (trace, trn, fil)) {
		fprintf (stderr, "filter_trace: filter_tr() error.\n");
		return (NULL);
	}
	if (!save_oldtrace) SCV_free_trace (trace);
	return (trn);
}

int
parse_filter (filstr, filter)

char *        filstr;
Filter **             filter;

{
	char *ptr, *copy;
	int type, i;
	double param1, param2, param3, param4;
	double iparam1, iparam2, iparam3, iparam4;

	copy = strdup(filstr);
	if (copy == NULL) {
		fprintf (stderr, "parse_filter: Malloc error.\n");
		return (0);
	}
	type = 0;
	param1 = 0.0;
	param2 = 0.0;
	param3 = 0.0;
	param4 = 0.0;
	iparam1 = 0.0;
	iparam2 = 0.0;
	iparam3 = 0.0;
	iparam4 = 0.0;
	for (ptr=strtok(copy, " \t"); ptr!=NULL; ptr=strtok(NULL, " \t")) {
		if (*ptr == '\0') continue;
		switch (type) {
		case 0:
			if (!strcmp(ptr, "BW")) {
				type = BUTWTH;
				i = 0;
			} else if (!strcmp(ptr, "BWZP")) {
				type = BUTWTHZP;
				i = 0;
			} else {
				fprintf (stderr, "parse_filter: Unknown filter type '%s'.\n",
										ptr);
				free (copy);
				return (0);
			}
			break;
		case BUTWTH:
		case BUTWTHZP:
			switch (i) {
			case 0:
				param1 = atof(ptr);
				break;
			case 1:
				iparam1 = atoi(ptr);
				break;
			case 2:
				param2 = atof(ptr);
				break;
			case 3:
				iparam2 = atoi(ptr);
				break;
			default:
				fprintf (stderr, "parse_filter: Too many filter parameters for '%s'.\n", copy);
				free (copy);
				return (0);
			}
			i++;
			break;
		}
	}
	switch (type) {
	case BUTWTH:
	case BUTWTHZP:
		if (i < 3) {
			fprintf (stderr, "parse_filter: Too few filter parameters for '%s'.\n", copy);
			free (copy);
			return (0);
		}
		break;
	}
	free (copy);
	(*filter) = (Filter *) malloc (sizeof(Filter));
	if ((*filter) == NULL) {
		fprintf (stderr, "parse_filter: Malloc error.\n");
		return (0);
	}
	(*filter)->type = type;
	(*filter)->param1 = param1;
	(*filter)->param2 = param2;
	(*filter)->param3 = param3;
	(*filter)->param4 = param4;
	(*filter)->iparam1 = iparam1;
	(*filter)->iparam2 = iparam2;
	(*filter)->iparam3 = iparam3;
	(*filter)->iparam4 = iparam4;
	return (1);
}

int
filter_get_params (filter, param1, param2, param3, param4,
			iparam1, iparam2, iparam3, iparam4)

char *filter;
double *param1;
double *param2;
double *param3;
double *param4;
int *iparam1;
int *iparam2;
int *iparam3;
int *iparam4;

{
	Filter *fil;

	*param1 = 0.0;
	*param2 = 0.0;
	*param3 = 0.0;
	*param4 = 0.0;
	*iparam1 = 0;
	*iparam2 = 0;
	*iparam3 = 0;
	*iparam4 = 0;
	if (!strcmp(filter, "none")) return (1);
	if (filter_arr) {
		fil = (Filter *) getarr(filter_arr, filter);
	} else {
		filter_arr = newarr (NULL);
		if (filter_arr == NULL) {
			fprintf (stderr, "filter_get_params: newarr() error.\n");
			return (0);
		}
		fil = NULL;
	}
	if (!fil) {
		if (!parse_filter (filter, &fil)) {
			fprintf (stderr, "filter_get_params: parse_filter() error.\n");
			return (0);
		}
		setarr (filter_arr, filter, fil);
	}
	*param1 = fil->param1;
	*param2 = fil->param2;
	*param3 = fil->param3;
	*param4 = fil->param4;
	*iparam1 = fil->iparam1;
	*iparam2 = fil->iparam2;
	*iparam3 = fil->iparam3;
	*iparam4 = fil->iparam4;
	return (1);
}

int
filter_tr (intrace, outtrace, filter)

Trace *    intrace;
Trace *             outtrace;
Filter *                      filter;

{
	Trace *itr, *otr, *intrace2;

	switch (filter->type) {
	case BUTWTH:
		for (itr=intrace,otr=outtrace; itr!=NULL; itr=itr->next,otr=otr->next) {
			float fl, fu, dt;
			int ol, ou, isave;

			fl = filter->param1;
			fu = filter->param2;
			dt = itr->dt;
			ol = filter->iparam1;
			ou = filter->iparam2;
			setbfl_ (&fl, &ol, &fu, &ou, &dt);
			inifil_ (itr->data);
			isave = 1;
			filrec_ (&itr->nsamps, itr->data, &isave, otr->data);
		}
		break;
	case BUTWTHZP:
		intrace2 = (Trace *) copy_trace (intrace, 1);
		for (itr=intrace,otr=intrace2; itr!=NULL; itr=itr->next,otr=otr->next) {
			float fl, fu, dt;
			int ol, ou, isave;
			int i;

			fl = filter->param1;
			fu = filter->param2;
			dt = itr->dt;
			ol = filter->iparam1;
			ou = filter->iparam2;
			setbfl_ (&fl, &ol, &fu, &ou, &dt);
			inifil_ (itr->data);
			isave = 1;
			filrec_ (&itr->nsamps, itr->data, &isave, otr->data);
			for (i=0; i<otr->nsamps/2; i++) {
				float hold;

				hold = otr->data[i];
				otr->data[i] = otr->data[otr->nsamps-1-i];
				otr->data[otr->nsamps-1-i] = hold;
			}
		}
		for (itr=intrace2,otr=outtrace; itr!=NULL; itr=itr->next,otr=otr->next) {
			float fl, fu, dt;
			int ol, ou, isave;
			int i;

			fl = filter->param1;
			fu = filter->param2;
			dt = itr->dt;
			ol = filter->iparam1;
			ou = filter->iparam2;
			setbfl_ (&fl, &ol, &fu, &ou, &dt);
			inifil_ (itr->data);
			isave = 1;
			filrec_ (&itr->nsamps, itr->data, &isave, otr->data);
			for (i=0; i<otr->nsamps/2; i++) {
				float hold;

				hold = otr->data[i];
				otr->data[i] = otr->data[otr->nsamps-1-i];
				otr->data[otr->nsamps-1-i] = hold;
			}
		}
		SCV_free_trace (intrace2);
		break;
	}
	return (1);
}

/* $Id: filter_subs.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
