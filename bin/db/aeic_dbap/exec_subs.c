
#include "dbap_defines.h"

#include <ctype.h>
#include <malloc.h>

Dbptr dbi;
Dbptr dbo;
Tbl *sc;
int verbose;
char *dbin;

Arr *get_process_args();
Arr *get_set_recipe_args();
Arr *get_get_recipe_args();
Arr *get_plot_args();
Arr *get_write_args();
Arr *get_get_args();
Arr *get_read_gather_args();
Arr *get_process_gather_args();
Arr *get_gen_args();

int
exec_process (argc, argv, name)

int argc;
char **argv;
char *name;

{
	Arr *args;
	double ts, te;
	char **recipes;
	Apspec *gather;
	char *gather_obj;
	int nrecipes;
	int i;
	char *nam;
	char *value;
	int itype;
	char *type;
	Tbl *recps;

	double dgrid=0.0;
	double gwin=0.0;
	double twin=7200.0;
	double tbreak=3600.0;
	double ovlp = 0.5;
	double thresh = 0.9;
	int plot=0;
	double az, slow;
	int igrwr=0;
	int indx = 0;
	int orid = -1;
	char phase[128];
	char *plotfile=NULL;
	int snorm=0;

	/* Parse the command line */

	args = get_process_args (argc, argv, &recipes, &nrecipes);
	if (args == NULL) {
		fprintf (stderr, "exec_process: get_process_args() error.\n");
		return (0);
	}
	if (verbose) {
		printf ("     process -recipe ");
		for (i=0; i<nrecipes; i++) printf ("%s ", recipes[i]);
		printargs (args);
	}

	/* Get time range */

	if (!get_times (args, &ts, &te)) {
		fprintf (stderr, "exec_process: get_times() error.\n");
		process_usage();
		return (0);
	}

	/* Look for a gather as input */

	gather_obj = getarr (args, "gather");
	gather = NULL;
	if (gather_obj) {
		if (!process_obj_get_entry (gather_obj, &type, &itype, &value, &recps)) {
			fprintf (stderr, "exec_process: process_obj_get_entry() error.\n");
			return (0);
		}
		if (itype != PROCESS) {
			fprintf (stderr, "exec_process: '%s' not a gather object.\n", gather_obj);
			return (0);
		}
		if (!get_process_recipe (recps, 0, &nam, &itype, &gather)) {
			fprintf (stderr, "exec_process: get_process_recipe() error.\n");
			return (0);
		}
		if (itype != GATHER) {
			fprintf (stderr, "exec_process: '%s' not a gather object.\n", gather_obj);
			return (0);
		}
	}

	/* Make the process object */

	if (!recipe_to_process (args, recipes, nrecipes, &nam)) {
		fprintf (stderr, "exec_process: recipe_to_process() error.\n");
		return (0);
	}
	strcpy (name, nam);
	if (!process_obj_get_entry (name, &type, &itype, &value, &recps)) {
		fprintf (stderr, "exec_process: process_obj_get_entry() error.\n");
		return (0);
	}

	/* Call the processing subroutine */

	value = getarr (args, "grid_ovlp");
	if (value) ovlp = atof(value);
	value = getarr (args, "grid_twin");
	if (value) gwin = atof(value);
	value = getarr (args, "grid_tbatch");
	if (value) twin = atof(value);
	value = getarr (args, "grid_tbreak");
	if (value) tbreak = atof(value);
	value = getarr (args, "index");
	if (value) indx = atoi(value);
	value = getarr (args, "thresh");
	if (value) thresh = atof(value);
	value = getarr (args, "plot");
	if (value) plot = atoi(value);
	plotfile = getarr (args, "psfile");
	value = getarr (args, "pred_times");
	if (value) {
		sscanf (value, "%d %s", &orid, phase);
	}
	value = getarr (args, "geq");
	if (value) snorm=1;
	if (ovlp < 0.0) ovlp = 0.0;
	if (ovlp > 1.0) ovlp = 1.0;
	dgrid = gwin*(1.0-ovlp);
	if (gwin == 0.0) dgrid = 0.0;
	if (!form_grids (dbi, sc, gather, ts, te, gwin, twin, dgrid, tbreak, plot, recps, indx,
								thresh, &az, &slow, dbo, igrwr, plotfile, snorm)) {
		fprintf (stderr, "exec_process: form_grids() error.\n");
		return (0);
	}
	if (!form_ftgrids (dbi, sc, gather, ts, te, gwin, twin, recps)) {
		fprintf (stderr, "exec_process: form_ftgrids() error.\n");
		return (0);
	}
	if (!form_beams (dbi, sc, gather, ts, te, recps)) {
		fprintf (stderr, "exec_process: form_beams() error.\n");
		return (0);
	}
	if (!form_crosscor (dbi, sc, gather, ts, te, recps, orid, phase)) {
		fprintf (stderr, "exec_process: form_crosscor() error.\n");
		return (0);
	}

	/* Normal exit */

	return (1);
}

int
exec_read_gather (argc, argv, name)

int argc;
char **argv;
char *name;

{
	Arr *args;
	double ts, te;
	char *recipe;
	int i;
	char *nam;
	char *value;
	int itype;
	char *type;
	Tbl *recps;

	/* Parse the command line */

	args = get_read_gather_args (argc, argv);
	if (args == NULL) {
		fprintf (stderr, "exec_read_gather: get_read_gather_args() error.\n");
		return (0);
	}
	if (verbose) {
		printf ("     read_gather ");
		printargs (args);
	}
	recipe = getarr (args, "recipe");
	if (recipe == NULL) {
		fprintf (stderr, "exec_read_gather: -recipe argument required.\n");
		read_gather_usage();
		return (0);
	}

	/* Get time range */

	if (!get_times (args, &ts, &te)) {
		fprintf (stderr, "exec_read_gather: get_times() error.\n");
		read_gather_usage();
		return (0);
	}

	/* Make the process object */

	if (!recipe_to_process (args, &recipe, 1, &nam)) {
		fprintf (stderr, "exec_process: recipe_to_process() error.\n");
		return (0);
	}
	strcpy (name, nam);
	if (!process_obj_get_entry (name, &type, &itype, &value, &recps)) {
		fprintf (stderr, "exec_process: process_obj_get_entry() error.\n");
		return (0);
	}

	/* Call the processing subroutine */

	if (!read_gather (dbi, sc, ts, te, recps)) {
		fprintf (stderr, "exec_read_gather: read_gather() error.\n");
		return (0);
	}

	/* Normal exit */

	return (1);
}

int
exec_process_gather (argc, argv, name)

int argc;
char **argv;
char *name;

{
	Arr *args;
	char *gather_obj;
	char *beam_obj;
	char *process_type;
	char *nam;
	char *type;
	int itype;
	char *value;
	Tbl *recps, *grecps;
	Apspec *gather, *new_gather, *beam;
	int i, n;
	int nobjs;
	char **beam_objs;
	Tbl *beams;

	/* Parse the command line */

	if (argc < 3) {
		process_gather_usage();
		return (0);
	}
	gather_obj = argv[1];
	process_type = argv[2];
	args = get_process_gather_args (argc-3, argv+3);
	if (args == NULL) {
		fprintf (stderr, "exec_process_gather: get_process_gather_args() error.\n");
		return (0);
	}
	if (verbose) {
		printf ("     process_gather %s %s ", gather_obj, process_type);
		printargs (args);
	}
	if (!strcmp (process_type, "copy")) {
	} else if (!strcmp (process_type, "remove_beam")) {
		beam_obj = getarr (args, "beam");
		if (beam_obj == NULL) {
			fprintf (stderr, "exec_process_gather: -beam argument required for remove_beam.\n");
			process_gather_usage();
			return (0);
		}
		if (!get_list (beam_obj, &nobjs, &beam_objs)) {
			fprintf (stderr, "exec_process_gather: get_list() error for -beams argument.\n");
			process_gather_usage();
			return (0);
		}
		beams = newtbl(1);
		if (beams == NULL) {
			fprintf (stderr, "exec_process_gather: newtbl() error for beams.\n");
			return (0);
		}
		for (i=0; i<nobjs; i++) {
			beam_obj = beam_objs[i];
			if (!process_obj_get_entry (beam_obj, &type, &itype, &value, &recps)) {
				fprintf (stderr, "exec_process_gather: process_obj_get_entry() error.\n");
				return (0);
			}
			if (!get_process_recipe (recps, 0, &nam, &itype, &beam)) {
				fprintf (stderr, "exec_process_gather: get_process_recipe() error.\n");
				return (0);
			}
			if (itype != BEAM) {
				fprintf (stderr, "exec_process_gather: -beam '%s' not a beam.\n", beam_obj);
				return (0);
			}
			if (settbl (beams, -1, beam) < 0) {
				fprintf (stderr, "exec_process_gather: settbl() error for beams.\n");
				return (0);
			}
		}
		my_free (beam_objs);
	} else {
		fprintf (stderr, "exec_process_gather: Unknown process type '%s'.\n", process_type);
		process_gather_usage();
		return (0);
	}

	/* Make the process object */

	if (!process_to_process (args, gather_obj, &nam)) {
		fprintf (stderr, "exec_process_gather: process_to_process() error.\n");
		return (0);
	}
	strcpy (name, nam);
	if (!process_obj_get_entry (name, &type, &itype, &value, &recps)) {
		fprintf (stderr, "exec_process_gather: process_obj_get_entry() error.\n");
		return (0);
	}
	if (!process_obj_get_entry (gather_obj, &type, &itype, &value, &grecps)) {
		fprintf (stderr, "exec_process_gather: process_obj_get_entry() error.\n");
		return (0);
	}

	/* Copy over the gather pointers */

	n = maxtbl(recps);
	for (i=0; i<n; i++) {
		if (!get_process_recipe (grecps, i, &nam, &itype, &gather)) {
			fprintf (stderr, "exec_process_gather: get_process_recipe() error.\n");
			return (0);
		}
		if (!get_process_recipe (recps, i, &nam, &itype, &new_gather)) {
			fprintf (stderr, "exec_process_gather: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case GATHER:
			new_gather->gather = gather;
			break;
		default:
			break;
		}
	}

	/* Call the processing subroutine */

	if (!strcmp (process_type, "copy")) {
		if (!read_gather (dbi, sc, 0.0, 0.0, recps)) {
			fprintf (stderr, "exec_read_gather: read_gather() error.\n");
			return (0);
		}
	} else if (!strcmp (process_type, "remove_beam")) {
		for (i=0; i<n; i++) {
			if (!get_process_recipe (recps, i, &nam, &itype, &new_gather)) {
				fprintf (stderr, "exec_process_gather: get_process_recipe() error.\n");
				return (0);
			}
			switch (itype) {
			case GATHER:
				if (!remove_beam (dbi, new_gather, beams)) {
					fprintf (stderr, "exec_process_gather: process_gather() error.\n");
					return (0);
				}
				break;
			default:
				break;
			}
		}
	}

	/* Normal exit */

	return (1);
}

int
exec_set_recipe (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *type;
	int itype;
	Apspec *ap;
	Channelspec *gchan;
	char *value;
	int i, j, n;
	int nchans;
	char **chans;
	char sta[64];
	char *ptr;

	/* Parse the command line */

	args = get_set_recipe_args (argc, argv);
	if (args == NULL) {
		fprintf (stderr, "exec_set_recipe: get_set_recipe_args() error.\n");
		return (0);
	}
	if (verbose) {
		printf ("     set_recipe %s ", argv[1]);
		printargs (args);
	}

	/* Get recipe object */

	if (!recipe_obj_get_entry (argv[1], &type, &itype, &ap)) {
		fprintf (stdout, "exec_set_recipe: recipe_obj_get_entry() error for '%s'.\n", argv[1]);
		return (0);
	}

	/* Set recipe parameters */

	value = getarr (args, "tcors");
	if (value) {
		if (ap->chans) {
			if (!get_list (value, &nchans, &chans)) {
				fprintf (stdout, "exec_set_recipe: get_list error for '%s'.\n", value);
				return (0);
			}
			n = maxtbl(ap->chans);
			for (i=0; i<n; i++) {
				gchan = (Channelspec *) gettbl (ap->chans, i);
				for (j=0; j<nchans; j++) {
					strcpy (sta, chans[j]);
					ptr = strchr(sta, '=');
					if (ptr == NULL) {
						if (verbose) {
							printf ("Setting sta '%s' tcor from %.3f to %.3f\n",
									gchan->sta, gchan->tcor, atof(chans[j]));
						}
						gchan->tcor = atof(chans[j]);
					} else {
						*ptr = '\0';
						ptr = strchr(chans[j], '=');
						ptr++;
						if (!strcmp(sta, gchan->sta)) {
							if (verbose) {
								printf ("Setting sta '%s' tcor from %.3f to %.3f\n",
										sta, gchan->tcor, atof(ptr));
							}
							gchan->tcor = atof(ptr);
						}
					}
				}
			}
			for (j=0; j<nchans; j++) free (chans[j]);
			my_free (chans);
		} 
	}
	value = getarr (args, "acors");
	if (value) {
		if (ap->chans) {
			if (!get_list (value, &nchans, &chans)) {
				fprintf (stdout, "exec_set_recipe: get_list error for '%s'.\n", value);
				return (0);
			}
			n = maxtbl(ap->chans);
			for (i=0; i<n; i++) {
				gchan = (Channelspec *) gettbl (ap->chans, i);
				for (j=0; j<nchans; j++) {
					strcpy (sta, chans[j]);
					ptr = strchr(sta, '=');
					if (ptr == NULL) {
						if (verbose) {
							printf ("Setting sta '%s' acor from %.4f to %.4f\n",
									gchan->sta, gchan->acor, atof(chans[j]));
						}
						gchan->acor = atof(chans[j]);
					} else {
						*ptr = '\0';
						ptr = strchr(chans[j], '=');
						ptr++;
						if (!strcmp(sta, gchan->sta)) {
							if (verbose) {
								printf ("Setting sta '%s' acor from %.4f to %.4f\n",
										sta, gchan->acor, atof(ptr));
							}
							gchan->acor = atof(ptr);
						}
					}
				}
			}
			for (j=0; j<nchans; j++) free (chans[j]);
			my_free (chans);
		} 
	}
	value = getarr (args, "wts");
	if (value) {
		if (ap->chans) {
			if (!get_list (value, &nchans, &chans)) {
				fprintf (stdout, "exec_set_recipe: get_list error for '%s'.\n", value);
				return (0);
			}
			n = maxtbl(ap->chans);
			for (i=0; i<n; i++) {
				gchan = (Channelspec *) gettbl (ap->chans, i);
				for (j=0; j<nchans; j++) {
					strcpy (sta, chans[j]);
					ptr = strchr(sta, '=');
					if (ptr == NULL) {
						if (verbose) {
							printf ("Setting sta '%s' wt from %.4f to %.4f\n",
									gchan->sta, gchan->wt, atof(chans[j]));
						}
						gchan->wt = atof(chans[j]);
					} else {
						*ptr = '\0';
						ptr = strchr(chans[j], '=');
						ptr++;
						if (!strcmp(sta, gchan->sta)) {
							if (verbose) {
								printf ("Setting sta '%s' wt from %.4f to %.4f\n",
										sta, gchan->wt, atof(ptr));
							}
							gchan->wt = atof(ptr);
						}
					}
				}
			}
			for (j=0; j<nchans; j++) free (chans[j]);
			my_free (chans);
		} 
	}
	value = getarr (args, "refsta");
	if (value) {
		if (verbose) {
			printf ("Setting refsta from '%s' to '%s'.\n", ap->refsta, value);
		}
		strcpy (ap->refsta, value);
	}
	value = getarr (args, "nx");
	if (value) {
		if (itype != XYGRID) {
			fprintf (stderr, "exec_set_recipe: No 'nx' parameter for '%s'.\n", argv[1]);
			return (0);
		}
		ap->nx = atoi(value);
	}
	value = getarr (args, "ny");
	if (value) {
		if (itype != XYGRID) {
			fprintf (stderr, "exec_set_recipe: No 'ny' parameter for '%s'.\n", argv[1]);
			return (0);
		}
		ap->ny = atoi(value);
	}
	value = getarr (args, "xmin");
	if (value) {
		if (itype != XYGRID) {
			fprintf (stderr, "exec_set_recipe: No 'xmin' parameter for '%s'.\n", argv[1]);
			return (0);
		}
		ap->xmin = atof(value);
	}
	value = getarr (args, "xmax");
	if (value) {
		if (itype != XYGRID) {
			fprintf (stderr, "exec_set_recipe: No 'xmax' parameter for '%s'.\n", argv[1]);
			return (0);
		}
		ap->xmax = atof(value);
	}
	value = getarr (args, "ymin");
	if (value) {
		if (itype != XYGRID) {
			fprintf (stderr, "exec_set_recipe: No 'ymin' parameter for '%s'.\n", argv[1]);
			return (0);
		}
		ap->ymin = atof(value);
	}
	value = getarr (args, "ymax");
	if (value) {
		if (itype != XYGRID) {
			fprintf (stderr, "exec_set_recipe: No 'ymax' parameter for '%s'.\n", argv[1]);
			return (0);
		}
		ap->ymax = atof(value);
	}

	/* Normal exit */

	return (1);
}

int
exec_get_recipe (argc, argv, val)

int argc;
char **argv;
char *                       val;

{
	Arr *args;
	char *type;
	int *itype;
	Apspec *ap;
	Channelspec *gchan;
	char *value;
	int i, j, n;

	/* Parse the command line */

	args = get_get_recipe_args (argc, argv);
	if (args == NULL) {
		fprintf (stderr, "exec_set_recipe: get_get_recipe_args() error.\n");
		return (0);
	}
	if (verbose) {
		printf ("     get_recipe %s ", argv[1]);
		printargs (args);
	}

	/* Get recipe object */

	if (!recipe_obj_get_entry (argv[1], &type, &itype, &ap)) {
		fprintf (stdout, "exec_set_recipe: recipe_obj_get_entry() error for '%s'.\n", argv[1]);
		return (0);
	}

	/* Get recipe parameters */

	strcpy (val, "");
	value = getarr (args, "stas");
	if (value) {
		if (ap->chans) {
			n = maxtbl(ap->chans);
			for (i=0; i<n; i++) {
				gchan = (Channelspec *) gettbl (ap->chans, i);
				if (i > 0) strcat (val, " ");
				strcat (val, gchan->sta);
			}
		}
	}
	value = getarr (args, "refsta");
	if (value) {
		if (val[0]) strcat (val, " ");
		strcat (val, ap->refsta);
	}

	/* Normal exit */

	return (1);
}

int
printargs (args)

Arr *      args;

{
	Tbl *names;
	int i, n;

	names = keysarr(args);
	n = maxtbl(names);
	for (i=0; i<n; i++) {
		printf ("-%s %s ", gettbl(names, i), getarr (args, gettbl(names, i)));
	}
	printf ("\n");
}

int
exec_get (argc, argv, name)

int argc;
char **argv;
char *name;

{
	char *value;
	int ret;
	int i;

#ifdef DEBUG
	if (argc == 2 && !strcmp(argv[1], "memory")) {
		struct mallinfo mi;

		mi = mallinfo();
		sprintf (name, "%d", mi.allocated);
		return (1);
	}
	if (argc == 2 && !strcmp(argv[1], "memory_tracker")) {
		ret = mem_tracker ("dbap", 0, 0, 0.0);
		sprintf (name, "%d", ret);
		return (1);
	}
	if (argc == 3 && !strcmp(argv[1], "memory_tracker")) {
		ret = mem_tracker ("dbap", atoi(argv[2]), 0, 0.0);
		sprintf (name, "%d", ret);
		return (1);
	}
	if (argc == 2 && !strcmp(argv[1], "set_allocs")) {
		set_malloc_state (1);
		strcpy (name, "0");
		return (1);
	}
	if ((argc == 2 || argc == 3) && !strcmp(argv[1], "allocs")) {
		if (argc == 3) {
			list_allocs (argv[2]);
		} else {
			list_allocs ("stdout");
		}
		strcpy (name, "0");
		return (1);
	}
#endif

	if (argc < 3) {
		fprintf (stderr, "usage: get name value [params]\n");
		return (0);
	}
	if (verbose) {
		printf ("     ");
		for (i=0; i<argc; i++) printf ("%s ", argv[i]);
		printf ("\n");
	}
	if (!process_get (argv[1], argc-2, &(argv[2]), &value)) {
		fprintf (stderr, "exec_get: process_get() error.\n");
		return (0);
	}
	strcpy (name, value);

	/* Normal exit */

	return (1);
}

int
exec_assoc (argc, argv, name)

int argc;
char **argv;
char *name;

{
	char *value;
	Arr *args;
	int ret;
	char *dbcat;
	double timeres_P=10.0;
	double timeres_S=10.0;
	char *irules=NULL;
	int narids=0;
	int *arids=NULL;
	int eorid, erec;
	char **items;
	int i;
	int nph;
	char **phases;

	strcpy (name, "-1");
	if (argc < 2) {
		fprintf (stderr, "usage: assoc arids [-dbcat dbcat_name] [-rules rules] [-P_thresh P_thresh]\n");
		fprintf (stderr, "                   [-S_thresh S_thresh]\n");
		return (0);
	}
	if (!get_list (argv[1], &narids, &items)) {
		fprintf (stderr, "exec_assoc: get_list() error for '%s'.\n", argv[1]);
		fprintf (stderr, "usage: assoc arids [-dbcat dbcat_name] [-rules rules] [-P_thresh P_thresh]\n");
		fprintf (stderr, "                   [-S_thresh S_thresh]\n");
		return (0);
	}
	if (narids < 1) {
		fprintf (stderr, "exec_assoc: No arids to process.\n");
		strcpy (name, "-1");
	}
	arids = (int *) malloc (narids*sizeof(int));
	if (arids == NULL) {
		fprintf (stderr, "exec_assoc: malloc() error.\n");
		return (0);
	}
	for (i=0; i<narids; i++) arids[i] = atoi(items[i]);
	args = get_gen_args (argc-2, argv+2);
	if (args == NULL) {
		fprintf (stderr, "exec_assoc: get_gen_args() error.\n");
		fprintf (stderr, "usage: assoc arids [-dbcat dbcat_name] [-rules rules] [-P_thresh P_thresh]\n");
		fprintf (stderr, "                   [-S_thresh S_thresh]\n");
		free (arids);
		return (0);
	}
	value = getarr (args, "dbcat");
	if (value) dbcat = value; else dbcat = dbin;
	irules = getarr (args, "rules");
	value = getarr (args, "P_thresh");
	if (value) timeres_P = atof(value);
	value = getarr (args, "S_thresh");
	if (value) timeres_S = atof(value);
	if (!dbassoc (dbin, dbcat, timeres_P, timeres_S, irules, narids, arids,
	        				&eorid, &erec, NULL, &nph, &phases)) {
		fprintf (stderr, "exec_exec_assoc: dbassoc() error.\n");
		free (arids);
		return (0);
	}
	free (arids);
	sprintf (name, "%d", eorid);
	for (i=0; i<nph; i++) {
		strcat (name, " ");
		strcat (name, phases[i]);
	}

	/* Normal exit */

	return (1);
}

int
exec_write (argc, argv)

int argc;
char **argv;

{
	char *value;
	char *value2;
	char *value3;
	char *value4;
	Arr *args;
	char *type;
	int itype;
	Tbl *recps;
	int i, n;
	char *name;
	void *ptr;
	Apspec *gs;
	int arid;
	double time;
	char phase[32];
	char chan[32];
	int igrwr = 0;
	int igrids = 0;
	Tbl *grids;

	/* Parse the command line */

	args = get_write_args (argc, argv);
	if (args == NULL) {
		fprintf (stderr, "exec_write: get_write_args() error.\n");
		return (0);
	}

	/* Get process object */

	if (!process_obj_get_entry (argv[1], &type, &itype, &value, &recps)) {
		fprintf (stderr, "exec_write: process_obj_get_entry() error.\n");
		return (0);
	}
	if (itype != PROCESS) {
		fprintf (stderr, "exec_write: Can only write process objects.\n");
		return (0);
	}
	if (dbo.database == -1) {
		fprintf (stderr, "exec_write: No output database.\n");
		return (0);
	}

	/* Write */

	n = maxtbl(recps);
	for (i=0; i<n; i++) {
		if (!get_process_recipe (recps, i, &name, &itype, &ptr)) {
			fprintf (stderr, "exec_plot: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case BEAM:
			gs = ptr;
			if (gs->tr == NULL) {
				fprintf (stderr, "exec_write: No data to write for '%s'.\n", name);
				break;
			}
			if (!write_beam (dbi, name, gs, gs->ts, gs->te, dbo)) {
				fprintf (stderr, "exec_write: write_beam() error for '%s'.\n", name);
				break;
			}
			value = getarr (args, "arrival");
			if (value) {
				arid = atoi(value);
				value = getarr (args, "time");
				if (value) {
					time = atof (value);
				} else {
					time = gs->ts+0.1*(gs->te-gs->ts);
				}
				value = getarr (args, "phase");
				if (value) {
					strcpy (phase, value);
				} else {
					strcpy (phase, "B");
				}
				if (!(arid=make_beam_arrival (dbi, dbo, arid, gs->array, gs->chan, 
								time, phase, gs->azimuth, 111.12*gs->slowi))) {
					fprintf (stderr, "exec_write: make_beam_arrival() error for '%s'.\n", name);
					break;
				}
			}
			break;
		case STGRID:
			gs = ptr;
			value = getarr (args, "write_grids");
			if (value) igrwr = 1;
			if (!write_stgrid (gs, dbo, igrwr)) {
				fprintf (stderr, "exec_write: write_stgrid() error for '%s'.\n", name);
				break;
			}
			break;
		case XYGRID:
			gs = ptr;
			value = getarr (args, "write_grids");
			if (value) igrwr = 1;
			value = getarr (args, "write_animate");
			if (value) igrids = 1;
			value = getarr (args, "write_power_trace");
			value2 = getarr (args, "write_raw_power_trace");
			value3 = getarr (args, "write_raw_azimuth_trace");
			value4 = getarr (args, "write_raw_slow_trace");
			if (value) {
				value = getarr (args, "chan");
				if (value) strcpy (chan, value);
				else strcpy (chan, "pow");
				grids = newtbl(1);
				if (grids == NULL) {
					fprintf (stderr, "exec_write: newtbl() error for '%s'.\n", name);
					break;
				}
				if (settbl (grids, -1, gs) < 0) {
					fprintf (stderr, "exec_write: settbl() error for '%s'.\n", name);
					break;
				}
				if (!write_pows (grids, 0.0, 9999999999.99, dbo, 0, chan, 0, 0)) {
					fprintf (stderr, "exec_write: write_pows() error for '%s'.\n", name);
					break;
				}
			} else if (value2) {
				value = getarr (args, "chan");
				if (value) strcpy (chan, value);
				else strcpy (chan, "rpow");
				grids = newtbl(1);
				if (grids == NULL) {
					fprintf (stderr, "exec_write: newtbl() error for '%s'.\n", name);
					break;
				}
				if (settbl (grids, -1, gs) < 0) {
					fprintf (stderr, "exec_write: settbl() error for '%s'.\n", name);
					break;
				}
				if (!write_pows (grids, 0.0, 9999999999.99, dbo, 1, chan, 0, 0)) {
					fprintf (stderr, "exec_write: write_pows() error for '%s'.\n", name);
					break;
				}
			} else if (value3) {
				value = getarr (args, "chan");
				if (value) strcpy (chan, value);
				else strcpy (chan, "raz");
				grids = newtbl(1);
				if (grids == NULL) {
					fprintf (stderr, "exec_write: newtbl() error for '%s'.\n", name);
					break;
				}
				if (settbl (grids, -1, gs) < 0) {
					fprintf (stderr, "exec_write: settbl() error for '%s'.\n", name);
					break;
				}
				if (!write_pows (grids, 0.0, 9999999999.99, dbo, 0, chan, 1, 0)) {
					fprintf (stderr, "exec_write: write_pows() error for '%s'.\n", name);
					break;
				}
			} else if (value4) {
				value = getarr (args, "chan");
				if (value) strcpy (chan, value);
				else strcpy (chan, "rsl");
				grids = newtbl(1);
				if (grids == NULL) {
					fprintf (stderr, "exec_write: newtbl() error for '%s'.\n", name);
					break;
				}
				if (settbl (grids, -1, gs) < 0) {
					fprintf (stderr, "exec_write: settbl() error for '%s'.\n", name);
					break;
				}
				if (!write_pows (grids, 0.0, 9999999999.99, dbo, 0, chan, 0, 1)) {
					fprintf (stderr, "exec_write: write_pows() error for '%s'.\n", name);
					break;
				}
			} else {
				if (!write_fkgrid (gs, dbo, igrwr, igrids)) {
					fprintf (stderr, "exec_write: write_fkgrid() error for '%s'.\n", name);
					break;
				}
			}
			break;
		case CROSSCOR:
			if (!write_stacor (ptr, dbo)) {
				fprintf (stderr, "exec_write: write_stacor() error for '%s'.\n", name);
				break;
			}
			break;
		default:
			fprintf (stderr, "exec_write: Don't know how to write '%s'.\n", name);
			return (0);
		}
	}

	/* Normal exit */

	return (1);
}

int
exec_free (argc, argv)

int argc;
char **argv;

{
	char *value;
	char *type;
	int itype;
	Tbl *recps;
	int i, n;
	char *name;
	void *ptr;
	Apspec *gs;
	int arid;
	double time;
	char phase[32];

	/* Parse the command line */

	if (argc != 2) {
		fprintf (stderr, "usage: free object\n");
		return (0);
	}

	/* Get process object */

	if (!process_obj_get_entry (argv[1], &type, &itype, &value, &recps)) {
		return (1);
	}
	if (itype != PROCESS) {
		return (1);
	}

	/* free */

	n = maxtbl(recps);
	for (i=0; i<n; i++) {
		if (!get_process_recipe (recps, i, &name, &itype, &ptr)) {
			continue;
		}
		switch (itype) {
		case XYGRID:
		case CROSSCOR:
		case STGRID:
		case FTGRID:
		case BEAM:
		case GATHER:
			if (!free_apspec (ptr)) {
				return (1);
			}
			break;
		default:
			fprintf (stderr, "exec_free: Don't know how to free '%s'.\n", name);
			continue;
		}
	}
	if (!process_obj_free_entry (argv[1])) {
		fprintf (stderr, "exec_free: process_obj_free_entry() error for '%s'.\n", argv[1]);
		return (0);
	}

	/* Normal exit */

	return (1);
}

int
exec_plot (argc, argv)

int argc;
char **argv;

{
	char *type, *value;
	int itype;
	Tbl *recps;
	Arr *args;
	int i, n;
	char *name;
	void *ptr;
	Apspec *gs, *cc;
	char *plotfile;
	char *filter;
	char *text;
	char *pred_phases;
	char string[64];
	int count;
	int ntimes;
	double *times;
	char *nonorm;

	double xdim=2.0;
	double ydim=2.0;
	double xlow = 0.5;
	double ylow = 0.5;
	double dx = 2.2;
	double dy = 2.2;
	static int itrans = 0;
	double xxlow, yylow;
	static double xdimt, ydimt;
	double ydimbm = 0.0, ylowbm = 0.0;
	double ydimov = 0.0, ylowov = 0.0;
	double ydimdf = 0.0, ylowdf = 0.0;
	double ydimtr = 0.0, ylowtr = 0.0;
	double ydimsp = 0.0, ylowsp = 0.0;
	double ydimcc = 0.0, ylowcc = 0.0;
	double xdimsp = 0.0, xlowsp = 0.0;
	double xdimcc = 0.0, xlowcc = 0.0;
	double xdimtr = 0.0, xlowtr = 0.0;
	double fmin = 0.0, fmax = 0.0;
	double dybm = 0.0;
	double dytr = 0.0;
	double size=0.0;
	double pmax=-1.0;
	double ampdecades=4.0;
	double thresh = 0.9;
	int first=1;
	int last=0;
	int title=1;
	int time_label=1;
	int animate = 0;
	int stack = 0;
	int indx = 0;
	float x4, y4, angle=0.0;
	float text_size = 0.1;
	float ratio=1.0;
	float slant=0.0;
	float xdim4, ydim4, xlow4, ylow4;
	float xmin4, xmax4, ymin4, ymax4;
	int iref=0;
	int iclip=1;
	int orid=-1;
	int norm=1;
	char *dbcat;
	int autotr=0;
	int autogr=0;

	/* Parse the command line */

	args = get_plot_args (argc, argv);
	if (args == NULL) {
		fprintf (stderr, "exec_plot: get_plot_args() error.\n");
		return (0);
	}
	if (!strcmp(argv[1], "close")) {
		antelope_finit ();
		return (1);
	}
	value = getarr (args, "orient");
	if (value) {
		if (!strcmp(value, "landscape")) {
			itrans = 1;
		} else {
			itrans = 0;
		}
	}
	if (itrans) {
		xdimt = 10.0;
		ydimt = 7.5;
	} else {
		xdimt = 7.5;
		ydimt = 10.0;
	}
	value = getarr (args, "title");
	if (value) {
		if (!strcmp(value, "yes")) {
			title = 1;
		} else {
			title = 0;
		}
	}
	value = getarr (args, "time_label");
	if (value) {
		if (!strcmp(value, "yes")) {
			time_label = 1;
		} else {
			time_label = 0;
		}
	}
	value = getarr (args, "xdim");
	if (value) xdim = atof(value);
	value = getarr (args, "ydim");
	if (value) ydim = atof(value);
	value = getarr (args, "ydimtr");
	if (value) ydimtr = atof(value);
	value = getarr (args, "ydimbm");
	if (value) ydimbm = atof(value);
	value = getarr (args, "ydimov");
	if (value) ydimov = atof(value);
	value = getarr (args, "ydimdf");
	if (value) ydimdf = atof(value);
	value = getarr (args, "ydimsp");
	if (value) ydimsp = atof(value);
	value = getarr (args, "ydimcc");
	if (value) ydimcc = atof(value);
	value = getarr (args, "xdimsp");
	if (value) xdimsp = atof(value);
	value = getarr (args, "xdimcc");
	if (value) xdimcc = atof(value);
	value = getarr (args, "xdimtr");
	if (value) xdimtr = atof(value);
	value = getarr (args, "xlow");
	if (value) xlow = atof(value);
	value = getarr (args, "ylow");
	if (value) ylow = atof(value);
	value = getarr (args, "ylowtr");
	if (value) ylowtr = atof(value);
	value = getarr (args, "ylowbm");
	if (value) ylowbm = atof(value);
	value = getarr (args, "ylowov");
	if (value) ylowov = atof(value);
	value = getarr (args, "ylowdf");
	if (value) ylowdf = atof(value);
	value = getarr (args, "ylowsp");
	if (value) ylowsp = atof(value);
	value = getarr (args, "ylowcc");
	if (value) ylowcc = atof(value);
	value = getarr (args, "xlowsp");
	if (value) xlowsp = atof(value);
	value = getarr (args, "xlowcc");
	if (value) xlowcc = atof(value);
	value = getarr (args, "xlowtr");
	if (value) xlowtr = atof(value);
	value = getarr (args, "dx");
	if (value) dx = atof(value);
	value = getarr (args, "dy");
	if (value) dy = atof(value);
	value = getarr (args, "dytr");
	if (value) dytr = atof(value);
	value = getarr (args, "dybm");
	if (value) dybm = atof(value);
	value = getarr (args, "fmin");
	if (value) fmin = atof(value);
	value = getarr (args, "fmax");
	if (value) fmax = atof(value);
	value = getarr (args, "size");
	if (value) size = atof(value);
	value = getarr (args, "pmax");
	if (value) pmax = atof(value);
	value = getarr (args, "animate");
	if (value) animate = atoi(value);
	value = getarr (args, "stack");
	if (value) stack = atoi(value);
	value = getarr (args, "index");
	if (value) indx = atoi(value);
	value = getarr (args, "thresh");
	if (value) thresh = atof(value);
	value = getarr (args, "time_marks");
	ntimes = 0;
	if (value) get_list_times (value, &ntimes, &times);
	plotfile = getarr (args, "psfile");
	filter = getarr (args, "filter");
	value = getarr (args, "autotr");
	if (value) autotr = 1;
	value = getarr (args, "autogr");
	if (value) autogr = 1;
	dbcat = getarr (args, "dbcat");
	if (!dbcat) dbcat = dbin;
	nonorm = getarr (args, "nonorm");
	if (nonorm) norm = 0;
	if (!strcmp(argv[1], "legend")) {
		value = getarr (args, "legend_title");
		if (value) {
			nlegend2 (xdim, ydim, xlow, ylow, 0.0, 1.0, 0.1, value);
		} else {
			nlegend (xdim, ydim, xlow, ylow, 0.0, 1.0, 0.1);
		}
		return (1);
	} 
	if (!strcmp(argv[1], "text")) {
		text = getarr (args, "text");
		if (!text) {
			fprintf (stderr, "exec_plot: plot text needs a -text argument.\n");
			fprintf (stderr, "           usage: plot text -text text_string -x x -y y\n");
			fprintf (stderr, "                           [-size size] [-angle angle]\n");
		}
		value = getarr (args, "x");
		if (!value) {
			fprintf (stderr, "exec_plot: plot text needs a -x argument.\n");
			fprintf (stderr, "           usage: plot text -text text_string -x x -y y\n");
			fprintf (stderr, "                           [-size size] [-angle angle]\n");
		}
		x4 = atof(value);
		value = getarr (args, "y");
		if (!value) {
			fprintf (stderr, "exec_plot: plot text needs a -y argument.\n");
			fprintf (stderr, "           usage: plot text -text text_string -x x -y y\n");
			fprintf (stderr, "                           [-size size] [-angle angle]\n");
		}
		y4 = atof(value);
		value = getarr (args, "angle");
		if (value) angle = atof(value);
		value = getarr (args, "ref");
		if (value) iref = atoi(value);
		if (size > 0.0) text_size = size;
		xdim4 = xdimt;
		ydim4 = ydimt;
		xlow4 = 0.0;
		ylow4 = 0.0;
		setdim_ (&xdim4, &ydim4, &xlow4, &ylow4);
		xmin4 = 0.0;
		xmax4 = xdimt;
		ymin4 = 0.0;
		ymax4 = ydimt;
		setscl_ (&xmin4, &xmax4, &ymin4, &ymax4);
		chrsiz_ (&text_size, &ratio, &slant);
		text_ (&x4, &y4, &angle, &iref, text, &iclip, strlen(text));
		return (1);
	}

	/* Get process object */

	if (!process_obj_get_entry (argv[1], &type, &itype, &value, &recps)) {
		fprintf (stderr, "exec_plot: process_obj_get_entry() error.\n");
		return (0);
	}
	if (itype != PROCESS) {
		fprintf (stderr, "exec_plot: Can only plot process objects.\n");
		return (0);
	}

	/* Plot */

	n = maxtbl(recps);
	xxlow = xlow;
	yylow = ylow;
	count = 1;
	for (i=0; i<n; i++) {
		if (!get_process_recipe (recps, i, &name, &itype, &ptr)) {
			fprintf (stderr, "exec_plot: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case BEAM:
			gs = ptr;
			if (xxlow+xdim > xdimt) {
				xxlow = xlow;
				yylow += dy;
			}
			if (i == n-1) last = 1;
			count++;
			beam_plot (gs, itrans, size, plotfile, xdim, xlow, ydim, ylow, ydimtr, ylowtr, 
						(first & title), (last & time_label), ntimes, times, filter);
			first = 0;
			if (dx != 0.0) {
				xxlow += dx;
			} else {
				yylow += dy;
			}
			break;
		case GATHER:
			gather_plot (ptr, itrans, size, plotfile, xdim, xlow, ydim, ylow,
						title, time_label, ntimes, times, filter);
			break;
		case CROSSCOR:
			cc = ptr;
			if (xxlow+xdim > xdimt) {
				xxlow = xlow;
				yylow += dy;
			}
			if (i == n-1) last = 1;
			count++;
			crosscor_plot (cc, itrans, size, plotfile, xdimtr, xlowtr, ydimtr, ylowtr,
						xdim, xlow, ydim, ylow, 
						xdimsp, xlowsp, ydimsp, ylowsp, fmin, fmax,
			                        (first & title), (last & time_label), ntimes, times);
			first = 0;
			if (dx != 0.0) {
				xxlow += dx;
			} else {
				yylow += dy;
			}
			break;
		case XYGRID:
			gs = ptr;
			if (xxlow+xdim > xdimt) {
				xxlow = xlow;
				yylow += dy;
			}
			xygrid_plot (gs, itrans, size, plotfile, animate, stack, indx, thresh, xdim, xxlow, yylow, pmax, 1, 1);
			if (dx != 0.0) {
				xxlow += dx;
			} else {
				yylow += dy;
			}
			break;
		case STGRID:
			gs = ptr;
			if (xxlow+xdim > xdimt) {
				xxlow = xlow;
				ylow += dy;
				ylowtr += dytr;
				ylowbm += dybm;
			}
			if (i == n-1) last = 1;
			count++;
			pred_phases = getarr (args, "pred_phases"); 
			value = getarr(args, "orid");
			if (value) orid = atoi(value);
			stgrid_plot (gs, itrans, size, plotfile, xdim, xxlow, ydim, ylow, ydimtr, ylowtr, 
						ydimbm, ylowbm, ydimov, ylowov, 
						ydimdf, ylowdf, pred_phases, orid, norm,
						(first & title), (last & time_label), ntimes, times, dbcat,
						autotr, autogr);
			first = 0;
			if (dx != 0.0) {
				xxlow += dx;
			} else {
				ylow += dy;
				ylowtr += dytr;
				ylowbm += dybm;
			}
			break;
		case FTGRID:
			gs = ptr;
			if (xxlow+xdim > xdimt) {
				xxlow = xlow;
				ylow += dy;
				ylowtr += dytr;
				ylowbm += dybm;
			}
			if (i == n-1) last = 1;
			count++;
			ftgrid_plot (gs, itrans, size, plotfile, xdim, xxlow, ydim, ylow, ydimtr, ylowtr, 
						ydimbm, ylowbm, xdimsp, xlowsp, ydimsp, ylowsp,
						(first & title), (last & time_label), ntimes, times);
			first = 0;
			if (dx != 0.0) {
				xxlow += dx;
			} else {
				ylow += dy;
				ylowtr += dytr;
				ylowbm += dybm;
			}
			break;
		default:
			fprintf (stderr, "exec_plot: Don't know how to plot '%s'.\n", name);
			return (0);
		}
	}

	/* Normal exit */

	return (1);
}

int
process_get (name, argc, argv, value)

char *       name;
int                argc;
char **                  argv;
char **                        value;

{
	char *val;
	char *type;
	int itype;
	Tbl *recipes;
	int i, n;
	char *name2;
	void *ptr;
	Apspec *gs, *cc;
	Channelspec *gchan;
	char num[64];
	char string[1024];
	double power;
	float *z;
	double az, slo, pow;
	int indx = 0;
	double thresh = 0.9;
	Arr *args;
	Dbptr dbarr, dborg, dbsta;
	int arid, orid;
	double time, elat, elon, slat, slon, delta, azimuth;
	double depth, elev;
	int nph;
	double *times, *ps, *dtdhs;
	char **phs;
	int nchans;
	char *sta, *chan, *nonorm;
	Dbptr db;

	if (!strcmp(name, "dbin") || !strcmp(name, "db")) {
		if (argc < 1) {
			fprintf (stderr, "process_get: No args for get db[in].\n");
			return (0);
		}
		if (!strcmp(name, "db")) {
			if (dbopen(argv[0], "r+", &db) == dbINVALID) {
				clear_register (1);
				fprintf (stderr, "process_get: dbopen() error for '%s'.\n", argv[0]);
				return (0);
			}
			argc--;
			argv++;
		} else {
			db = dbi;
		}
		if (argc < 1) {
			fprintf (stderr, "process_get: No args for get db[in].\n");
			return (0);
		}
		if (!strcmp(argv[0], "arrival")) {
			if (argc != 3) {
				fprintf (stderr, "process_get: usage is 'get dbin arrival arid value'.\n");
				return (0);
			}
			arid = atoi(argv[1]);
			val = argv[2];
			sprintf (string, "%d", arid);
			dbarr = dblookup (db, 0, "arrival", "arid", string);
			if (dbarr.record == dbINVALID) {
				clear_register (1);
				fprintf (stderr, "process_get: Unable to find arid %d.\n", arid);
				return (0);
			}
			dbarr.field = dbALL;
			if (!strcmp(val, "time")) {
				if (dbgetv(dbarr, 0, "time", &time, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get arrival time.\n");
					return (0);
				}
				sprintf (string, "%.5lf", time);
			} else {
				fprintf (stderr, "process_get: Don't know how to get %s from arrival.\n",
										val);
				return (0);
			}
		} else if (!strcmp(argv[0], "origin")) {
			if (argc < 3) {
				fprintf (stderr, "process_get: usage is 'get dbin origin orid value [args]'.\n");
				return (0);
			}
			orid = atoi(argv[1]);
			val = argv[2];
			sprintf (string, "%d", orid);
			dborg = dblookup (db, 0, "origin", "orid", string);
			if (dborg.record == dbINVALID) {
				clear_register (1);
				fprintf (stderr, "process_get: Unable to find orid %d.\n", orid);
				return (0);
			}
			dborg.field = dbALL;
			if (!strcmp(val, "time")) {
				if (dbgetv(dborg, 0, "time", &time, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin time.\n");
					return (0);
				}
				sprintf (string, "%.5lf", time);
			} else if (!strcmp(val, "mb")) {
				if (dbgetv(dborg, 0, "mb", &time, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin mb.\n");
					return (0);
				}
				sprintf (string, "%.2lf", time);
			} else if (!strcmp(val, "depth")) {
				if (dbgetv(dborg, 0, "depth", &depth, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin mb.\n");
					return (0);
				}
				sprintf (string, "%.3lf", depth);
			} else if (!strcmp(val, "seaz")) {
				if (argc != 4) {
					fprintf (stderr, "process_get: usage is 'get dbin origin orid seaz sta'.\n");
					return (0);
				}
				if (dbgetv(dborg, 0, "lat", &elat, "lon", &elon, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin lat lon.\n");
					return (0);
				}
				dbsta = dblookup (dbi, 0, "site", "sta", argv[3]);
				if (dbsta.record == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to find sta %s.\n", argv[3]);
					return (0);
				}
				dbsta.field = dbALL;
				if (dbgetv(dbsta, 0, "lat", &slat, "lon", &slon, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get site lat lon.\n");
					return (0);
				}
				elat *= M_PI/180.0;
				elon *= M_PI/180.0;
				slat *= M_PI/180.0;
				slon *= M_PI/180.0;
				dist (slat, slon, elat, elon, &delta, &azimuth);
				delta *= 180.0/M_PI;
				azimuth *= 180.0/M_PI;
				sprintf (string, "%.3lf", azimuth);
			} else if (!strcmp(val, "delta")) {
				if (argc != 4) {
					fprintf (stderr, "process_get: usage is 'get dbin origin orid delta sta'.\n");
					return (0);
				}
				if (dbgetv(dborg, 0, "lat", &elat, "lon", &elon, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin lat lon.\n");
					return (0);
				}
				dbsta = dblookup (dbi, 0, "site", "sta", argv[3]);
				if (dbsta.record == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to find sta %s.\n", argv[3]);
					return (0);
				}
				dbsta.field = dbALL;
				if (dbgetv(dbsta, 0, "lat", &slat, "lon", &slon, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get site lat lon.\n");
					return (0);
				}
				elat *= M_PI/180.0;
				elon *= M_PI/180.0;
				slat *= M_PI/180.0;
				slon *= M_PI/180.0;
				dist (slat, slon, elat, elon, &delta, &azimuth);
				delta *= 180.0/M_PI;
				azimuth *= 180.0/M_PI;
				sprintf (string, "%.3lf", delta);
			} else if (!strcmp(val, "phase_slow")) {
				if (argc != 5) {
					fprintf (stderr, "process_get: usage is 'get dbin origin orid phase_slow sta phase'.\n");
					return (0);
				}
				if (dbgetv(dborg, 0, "lat", &elat, "lon", &elon, "depth", &depth, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin lat lon.\n");
					return (0);
				}
				dbsta = dblookup (dbi, 0, "site", "sta", argv[3]);
				if (dbsta.record == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to find sta %s.\n", argv[3]);
					return (0);
				}
				dbsta.field = dbALL;
				if (dbgetv(dbsta, 0, "lat", &slat, "lon", &slon, "elev", &elev, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get site lat lon.\n");
					return (0);
				}
				tt_taup_set_phases ("basic");
				tt_taup_set_event_depth (depth);
				tt_taup_p (elat, elon, slat, slon, elev, 0.0, 0.0,
						&nph, &times, &ps, &dtdhs, &phs);
				for (i=0; i<nph; i++) {
					if (!strcmp(phs[i], argv[4])) break;
				}
				if (i == nph) {
					fprintf (stderr, "process_get: No phases found for '%s'.\n", argv[4]);
					return (0);
				}
				ps[i] /= 111.12;
				sprintf (string, "%.7lf", ps[i]);
			} else if (!strcmp(val, "ptime")) {
				if (argc != 5) {
					fprintf (stderr, "process_get: usage is 'get dbin origin orid ptime sta phase'.\n");
					return (0);
				}
				if (dbgetv(dborg, 0, "lat", &elat, "lon", &elon, "depth", &depth, "time", &time, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get origin lat lon.\n");
					return (0);
				}
				dbsta = dblookup (dbi, 0, "site", "sta", argv[3]);
				if (dbsta.record == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to find sta %s.\n", argv[3]);
					return (0);
				}
				dbsta.field = dbALL;
				if (dbgetv(dbsta, 0, "lat", &slat, "lon", &slon, "elev", &elev, NULL) == dbINVALID) {
					clear_register (1);
					fprintf (stderr, "process_get: Unable to get site lat lon.\n");
					return (0);
				}
				tt_taup_set_phases ("basic");
				tt_taup_set_event_depth (depth);
				tt_taup_p (elat, elon, slat, slon, elev, 0.0, 0.0,
						&nph, &times, &ps, &dtdhs, &phs);
				for (i=0; i<nph; i++) {
					if (!strcmp(phs[i], argv[4])) break;
				}
				if (i == nph) {
					fprintf (stderr, "process_get: No phases found for '%s'.\n", argv[4]);
					return (0);
				}
				sprintf (string, "%.5lf", times[i]+time);
			} else {
				fprintf (stderr, "process_get: Don't know how to get %s from arrival.\n",
										val);
				return (0);
			}
		} else {
			fprintf (stderr, "process_get: Don't know how to get %s from %s.\n",
										argv[0], name);
			return (0);
		}
		*value = strdup(string);
		if (*value == NULL) {
			fprintf (stderr, "process_get: strdup() error.\n");
			return (0);
		}

		return (1);
	}
	if (*name != '%') {
		var_get (name, &val);
		if (val == NULL) {
			fprintf (stderr, "process_get: Undefined name '%s'.\n", name);
			return (0);
		}
		name = val;
	}
	if (!process_obj_get_entry (name, &type, &itype, &val, &recipes)) {
		fprintf (stderr, "process_get: process_obj_get_entry() error for '%s'.\n", name);
		return (0);
	}
	if (itype != PROCESS) {
		fprintf (stderr, "process_get: Don't know how to get for %s object.\n", type);
		return (0);
	}
	args = get_get_args (argc, argv);
	if (args == NULL) {
		fprintf (stderr, "process_get: get_get_args() error.\n");
		return (0);
	}
	n = maxtbl(recipes);
	power = 0.0;
	strcpy (string, "");
	for (i=0; i<n; i++) {
		if (!get_process_recipe (recipes, i, &name2, &itype, &ptr)) {
			fprintf (stderr, "process_get: get_process_recipe() error.\n");
			return (0);
		}
		switch (itype) {
		case XYGRID:
			gs = ptr;
			val = getarr (args, "index");
			if (val) indx = atoi(val);
			val = getarr (args, "thresh");
			if (val) thresh = atof(val);
			sta = getarr (args, "sta");
			chan = getarr (args, "chan");
			if (val) thresh = atof(val);
			nonorm = getarr (args, "nonorm");
			val = getarr (args, "stack");
			if (val) {
				if (!gs->savegrids || !gs->grids) {
					fprintf (stderr, "process_get: Grids not saved - cannot stack.\n");
					return (0);
				}
				z = (float *) stack_grids (gs);
				if (z == NULL) {
					fprintf (stderr, "process_get: stack_grids() error.\n");
					return (0);
				}
				find3 (gs, z, indx, thresh, &az, &slo, &pow);
				my_free (z);
			} else {
				if (nonorm) {
					az = gs->summary_grid.peak_azimuth;
					slo = gs->summary_grid.peak_slow;
					pow = gs->summary_grid.peak_power;
				} else {
					az = gs->norm_summary_grid.peak_azimuth;
					slo = gs->norm_summary_grid.peak_slow;
					pow = gs->norm_summary_grid.peak_power;
				}
			}
			if (!strcmp(argv[0], "azimuth")) {
				sprintf (num, "%.3f", az);
				if (strlen(string) > 0) {
					strcat (string, " ");
				}
				strcat (string, num);
			} else if (!strcmp(argv[0], "best_azimuth")) {
				if (pow > power) {
					power = pow;
					sprintf (string, "%.3f", az);
				}
			} else if (!strcmp(argv[0], "slowness")) {
				sprintf (num, "%.4f", slo);
				if (strlen(string) > 0) {
					strcat (string, " ");
				}
				strcat (string, num);
			} else if (!strcmp(argv[0], "best_slowness")) {
				if (pow > power) {
					power = pow;
					sprintf (string, "%.4f", slo);
				}
			} else if (!strcmp(argv[0], "power")) {
				sprintf (num, "%.4f", pow);
				if (strlen(string) > 0) {
					strcat (string, " ");
				}
				strcat (string, num);
			} else if (!strcmp(argv[0], "ts")) {
				sprintf (num, "%.5lf", gs->ts);
				if (strlen(string) > 0) {
					strcat (string, " ");
				}
				strcat (string, num);
			} else if (!strcmp(argv[0], "te")) {
				sprintf (num, "%.5lf", gs->te);
				if (strlen(string) > 0) {
					strcat (string, " ");
				}
				strcat (string, num);
			} else if (!strcmp(argv[0], "filter")) {
				gchan = (Channelspec *) gettbl(gs->chans, 0);
				if (strlen(string) > 0) {
					strcat (string, " ");
				}
				strcat (string, gchan->filter);
			} else if (!strcmp(argv[0], "nchans")) {
				n = maxtbl(gs->chans);
				for (i=0,nchans=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(gs->chans, i);
					if (gchan->tr) nchans++;
				}
				sprintf (num, "%d", nchans);
				strcat (string, num);
			} else if (!strcmp(argv[0], "exist")) {
				n = maxtbl(gs->chans);
				for (i=0,nchans=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(gs->chans, i);
					if (sta && strcmp(gchan->sta, sta)) continue;
					if (chan && strcmp(gchan->chan, chan)) continue;
					if (gchan->tr) nchans=1;
				}
				sprintf (num, "%d", nchans);
				strcat (string, num);
			} else {
				fprintf (stderr, "process_get: Don't know how to get %s from %s.\n",
										argv[0], name2);
				return (0);
			}
			break;
		case CROSSCOR:
			cc = ptr;
			sta = getarr (args, "sta");
			chan = getarr (args, "chan");
			if (!strcmp(argv[0], "tcors")) {
				n = maxtbl(cc->chans);
				for (i=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(cc->chans, i);
					if (gchan->sp) {
						if (strcmp(string, "")) strcat (string, " ");
						sprintf (num, "%s=%.3f", gchan->sta, gchan->tcor+gchan->sp->tshift);
						strcat (string, num);
					}
				}
			} else if (!strcmp(argv[0], "acors")) {
				n = maxtbl(cc->chans);
				for (i=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(cc->chans, i);
					if (gchan->sp) {
						if (strcmp(string, "")) strcat (string, " ");
						sprintf (num, "%s=%.4f", gchan->sta, gchan->wt*gchan->sp->acor);
						strcat (string, num);
					}
				}
			} else if (!strcmp(argv[0], "nchans")) {
				n = maxtbl(cc->chans);
				for (i=0,nchans=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(cc->chans, i);
					if (gchan->sp) nchans++;
				}
				sprintf (num, "%d", nchans);
				strcat (string, num);
			} else if (!strcmp(argv[0], "stas")) {
				n = maxtbl(cc->chans);
				for (i=0,nchans=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(cc->chans, i);
					if (gchan->sp) {
						if (strlen(string) > 0) {
							strcat (string, " ");
						}
						strcat (string, gchan->sta);
					}
				}
			} else if (!strcmp(argv[0], "exist")) {
				n = maxtbl(cc->chans);
				for (i=0,nchans=0; i<n; i++) {
					gchan = (Channelspec *) gettbl(cc->chans, i);
					if (sta && strcmp(gchan->sta, sta)) continue;
					if (chan && strcmp(gchan->chan, chan)) continue;
					if (gchan->sp) nchans=1;
				}
				sprintf (num, "%d", nchans);
				strcat (string, num);
			} else {
				fprintf (stderr, "process_get: Don't know how to get %s from %s.\n",
										argv[0], name2);
				return (0);
			}
			break;
		default:
			fprintf (stderr, "process_get: Don't know how to get from '%s'.\n", name2);
			return (0);
		}
	}
	*value = strdup(string);
	if (*value == NULL) {
		fprintf (stderr, "process_get: strdup() error.\n");
		return (0);
	}

	return (1);
}

Arr *
get_process_args (argc, argv, recipes, nrecipes)

int argc;
char **argv;
char ***recipes;
int *nrecipes;

{
	Arr *args;
	double time;
	char string[512];
	int i, n;
	char *name, *value ;

	if (argc < 3) {
		process_usage();
		return (NULL);
	}
	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_process_args: newtbl() error.\n");
		return (NULL);
	}
	*recipes = NULL;
	for (argc--,argv++; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "-recipe")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_process_args: No argument for -recipe.\n");
				process_usage();
				return (NULL);
			}
			for (n=0; argc-n>0; n++) {
				if (*(argv[n]) == '-') break;
			}
			*recipes = (char **) my_malloc ("get_process_args: *recipes", n*sizeof(char *));
			if (*recipes == NULL) {
				fprintf (stderr, "get_process_args: malloc error.\n");
				usage();
				return (NULL);
			}
			*nrecipes = n;
			for (i=0; i<n; i++,argc--,argv++) {
				(*recipes)[i] = *argv;
			}
			argc++;
			argv--;
		} else if (!strcmp(*argv, "-ts")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_process_args: No argument for -ts.\n");
				process_usage();
				return (NULL);
			}
			if (**argv == '$' || **argv == '%') {
				setarr (args, "ts", *argv);
			} else {
				if (!time_string2epoch(*argv, &time)) {
					fprintf (stderr, "get_process_args: Unable to parse -ts time.\n");
					process_usage();
					return (NULL);
				}
				sprintf (string, "%.5lf", time);
				setarr (args, "ts", strdup(string));
			}
		} else if (!strcmp(*argv, "-te")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_process_args: No argument for -te.\n");
				process_usage();
				return (NULL);
			}
			if (**argv == '$' || **argv == '%') {
				setarr (args, "te", *argv);
			} else {
				if (!time_string2epoch(*argv, &time)) {
					fprintf (stderr, "get_process_args: Unable to parse -te time.\n");
					process_usage();
					return (NULL);
				}
				sprintf (string, "%.5lf", time);
				setarr (args, "te", strdup(string));
			}
		} else if (!strcmp(*argv, "-savegrids")) {
			setarr (args, "savegrids", strdup("1"));
		} else if (!strcmp(*argv, "-geq")) {
			setarr (args, "geq", strdup("1"));
		} else if (!strcmp(*argv, "-plot")) {
			setarr (args, "plot", strdup("1"));
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_process_args: No argument for %s.\n", *argv);
				process_usage();
				return (NULL);
			}
			name = *argv + 1;
			value = *++argv ;
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_process_args: Illegal argument '%s'.\n", *argv);
			process_usage();
			return (NULL);
		}
	}
	if (*recipes == NULL) {
		fprintf (stderr, "get_process_args: -recipe required.\n");
		process_usage();
		return (NULL);
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_process_args: sub_vars() error.\n");
		process_usage();
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

process_usage()

{
	fprintf (stderr, "usage: process -recipe recipe_obj [-gather gather_obj]\n");
	fprintf (stderr, "               [-ts tstart_time] [-te end_time]\n");
	fprintf (stderr, "               [-grid_ovlp grid_ovlp] [-grid_twin grid_twin]\n");
	fprintf (stderr, "               [-grid_tbatch grid_tbatch] [-grid_tbreak grid_tbreak]\n");
	fprintf (stderr, "               [-arid arid] [-arid_t0 arid_t0] [-arid_tw arid_tw]\n");
	fprintf (stderr, "               [-chan chan] [-filter filter] [-samprate samprate]\n");
	fprintf (stderr, "               [-azimuth azimuth] [-slow slow] [-psfile psfile]\n");
}

Arr *
get_set_recipe_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	if (argc < 2) {
		set_recipe_usage();
		return (NULL);
	}
	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_set_recipe_args: newtbl() error.\n");
		return (NULL);
	}
	for (argc -= 2,argv += 2; argc>0; argc--,argv++) {
		if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_set_recipe_args: No argument for %s.\n", *argv);
				set_recipe_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_set_recipe_args: Illegal argument '%s'.\n", *argv);
			set_recipe_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_set_recipe_args: sub_vars() error.\n");
		set_recipe_usage();
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

Arr *
get_get_recipe_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	if (argc < 2) {
		get_recipe_usage();
		return (NULL);
	}
	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_get_recipe_args: newtbl() error.\n");
		return (NULL);
	}
	for (argc -= 2,argv += 2; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "stas")) {
			setarr (args, "stas", "1");
		} else if (!strcmp(*argv, "nonorm")) {
			setarr (args, "nonorm", "1");
		} else if (!strcmp(*argv, "refsta")) {
			setarr (args, "refsta", "1");
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_get_recipe_args: No argument for %s.\n", *argv);
				get_recipe_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_get_recipe_args: Illegal argument '%s'.\n", *argv);
			get_recipe_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_get_recipe_args: sub_vars() error.\n");
		get_recipe_usage();
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

int
set_recipe_usage()

{
	fprintf (stdout, "usage: set_recipe recipe_name [-tcors tcor_list]\n");
}

int
get_recipe_usage()

{
	fprintf (stdout, "usage: get_recipe recipe_name {stas}\n");
}

Arr *
get_plot_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	if (argc < 2) {
		plot_usage();
		return (NULL);
	}
	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_plot_args: newtbl() error.\n");
		return (NULL);
	}
	for (argc -= 2,argv += 2; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "-orient")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_plot_args: No argument for -orient.\n");
				plot_usage();
				return (NULL);
			}
			if (!strcmp(*argv, "landscape")) {
			} else if (!strcmp(*argv, "portrait")) {
			} else {
				fprintf (stderr, "get_plot_args: Unrecognized argument for -orient.\n");
				plot_usage();
				return (NULL);
			}
			setarr (args, "orient", *argv);
		} else if (!strcmp(*argv, "-animate")) {
			setarr (args, "animate", strdup("1"));
		} else if (!strcmp(*argv, "-stack")) {
			setarr (args, "stack", strdup("1"));
		} else if (!strcmp(*argv, "-nonorm")) {
			setarr (args, "nonorm", strdup("1"));
		} else if (!strcmp(*argv, "-autotr")) {
			setarr (args, "autotr", strdup("1"));
		} else if (!strcmp(*argv, "-autogr")) {
			setarr (args, "autogr", strdup("1"));
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_plot_args: No argument for %s.\n", *argv);
				plot_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_plot_args: Illegal argument '%s'.\n", *argv);
			plot_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_plot_args: sub_vars() error.\n");
		plot_usage();
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

plot_usage()

{
	fprintf (stderr, "usage: plot {objname [-orient {landscale|portrait}]\n");
	fprintf (stderr, "                     [-xdim xdim] [-ydim ydim] [-xlow xlow] [-ylow ylow]\n");
	fprintf (stderr, "                     ...\n");
	fprintf (stderr, "            | legend -xdim xdim -ydim ydim -xlow xlow -ylow ylow\n");
	fprintf (stderr, "            | close}\n");
}

Arr *
get_write_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	if (argc < 2) {
		write_usage();
		return (NULL);
	}
	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_write_args: newtbl() error.\n");
		return (NULL);
	}
	for (argc -= 2,argv += 2; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "-write_grids")) {
			setarr (args, "write_grids", "1");
		} else if (!strcmp(*argv, "-write_animate")) {
			setarr (args, "write_animate", "1");
		} else if (!strcmp(*argv, "-write_power_trace")) {
			setarr (args, "write_power_trace", "1");
		} else if (!strcmp(*argv, "-write_raw_power_trace")) {
			setarr (args, "write_raw_power_trace", "1");
		} else if (!strcmp(*argv, "-write_raw_azimuth_trace")) {
			setarr (args, "write_raw_azimuth_trace", "1");
		} else if (!strcmp(*argv, "-write_raw_slow_trace")) {
			setarr (args, "write_raw_slow_trace", "1");
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_write_args: No argument for %s.\n", *argv);
				write_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_write_args: Illegal argument '%s'.\n", *argv);
			write_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_write_args: sub_vars() error.\n");
		write_usage();
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

write_usage()

{
	fprintf (stderr, "usage: write objname [-arrival arid] [-time time]\n");
	fprintf (stderr, "                     [-phase phase] [-write_grids]\n");
}

Arr *
get_get_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_get_args: newtbl() error.\n");
		return (NULL);
	}
	if (argc < 2) {
		return (args);
	}
	for (argc -= 1,argv += 1; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "-stack")) {
			setarr (args, "stack", strdup("1"));
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_get_args: No argument for %s.\n", *argv);
				write_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_get_args: Illegal argument '%s'.\n", *argv);
			write_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_get_args: sub_vars() error.\n");
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

Arr *
get_read_gather_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_read_gather_args: newtbl() error.\n");
		return (NULL);
	}
	if (argc < 2) {
		read_gather_usage();
		return (NULL);
	}
	for (argc--, argv++; argc>0; argc--,argv++) {
		if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_read_gather_args: No argument for %s.\n", *argv);
				read_gather_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_read_gather_args: Illegal argument '%s'.\n", *argv);
			read_gather_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_read_gather_args: sub_vars() error.\n");
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

int
read_gather_usage()

{
	fprintf (stderr, "usage: read_gather -recipe recipe -ts ts -te te [-filter filter]\n");
	fprintf (stderr, "                   [-chan chan] [-samprate samprate] [-azimuth azimuth]\n");
}

Arr *
get_process_gather_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char string[1024];
	char *name, *value ;

	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_process_gather_args: newtbl() error.\n");
		return (NULL);
	}
	for (; argc>0; argc--,argv++) {
		if (!strcmp(*argv, "-beam")) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_process_gather_args: No argument for %s.\n", *argv);
				process_gather_usage();
				return (NULL);
			}
			argv++;
			strcpy (string, "");
			for (; argc>0 && **argv!='-'; argc--,argv++) {
				if (string[0] != '\0') strcat (string, " ");
				strcat (string, *argv);
			}
			setarr (args, "beam", string);
			if (argc == 0) break;
			argc++;
			argv--;
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_process_gather_args: No argument for %s.\n", *argv);
				process_gather_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_process_gather_args: Illegal argument '%s'.\n", *argv);
			process_gather_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_process_gather_args: sub_vars() error.\n");
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

int
process_gather_usage()

{
	fprintf (stderr, "usage: process_gather gather_obj {copy\n");
	fprintf (stderr, "                                | remove_beam -beam beam_obj}\n");
}

Arr *
get_gen_args (argc, argv)

int argc;
char **argv;

{
	Arr *args;
	char *name, *value ;

	args = newarr (NULL);
	if (args == NULL) {
		fprintf (stderr, "get_gen_args: newtbl() error.\n");
		return (NULL);
	}
	if (argc < 1) {
		return (args);
	}
	for (; argc>0; argc--,argv++) {
		if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_gen_args: No argument for %s.\n", *argv);
				plot_usage();
				return (NULL);
			}
			name = *argv + 1 ; 
			value = *++argv ; 
			setarr (args, name, value ) ; 
		} else {
			fprintf (stderr, "get_gen_args: Illegal argument '%s'.\n", *argv);
			plot_usage();
			return (NULL);
		}
	}
	if (!sub_vars (args)) {
		fprintf (stderr, "get_gen_args: sub_vars() error.\n");
		plot_usage();
		return (NULL);
	}

	/* Normal exit */

	return (args);
}

int
sub_vars (arr)

Arr *     arr;

{
	Tbl *names;
	int i, n;
	char *name;
	char *value;
	char *newval;

	if (arr == NULL) return (1);
	names = keysarr (arr);
	n = maxtbl(names);
	for (i=0; i<n; i++) {
		name = gettbl (names, i);
		value = getarr (arr, name);
		if (*value != '$') continue;
		var_get (&value[1], &newval);
		if (newval == NULL) {
			fprintf (stderr, "sub_vars: Unable to find variable '%s'.\n", &value[1]);
			return (0);
		}
		value = strdup(newval);
		if (value == NULL) {
			fprintf (stderr, "sub_vars: strdup() error.\n");
			return (0);
		}
		setarr (arr, name, value);
	}
	return (1);
}

int
parse_line (line, argc, argv)

char *      line;
int *             argc;
char ***                argv;

{
	char *string;
	char **args=NULL;
	int i;
	int embedded;
	int first;

	string = strdup(line);
	if (string == NULL) {
		fprintf (stderr, "parse_line: strdup() error.\n");
		return (0);
	}
	*argc = 0;
	args = (char **) my_malloc ("parse_line: args", sizeof(char *));
	if (args == NULL) {
		fprintf (stderr, "parse_line: malloc() error.\n");
		return (0);
	}
	embedded = 0;
	first = 1;
	for (i=0; i<strlen(line); i++) {
		switch (embedded) {
		case 2:
			switch (string[i]) {
			case '"':
				string[i] = '\0';
				embedded = 0;
				break;
			default:
				break;
			}
			break;
		case 1:
			break;
		case 0:
			switch (string[i]) {
			case ' ':
			case '\t':
				string[i] = '\0';
				first = 1;
				break;
			case '"':
				string[i++] = '\0';
				args = (char **) my_realloc ("parse_line: args", args, ((*argc)+1)*sizeof(char *));
				if (args == NULL) {
					fprintf (stderr, "parse_line: realloc() error.\n");
					return (0);
				}
				args[*argc] = &string[i];
				(*argc)++;
				embedded = 2;
				break;
			default:
				if (!first) break;
				first = 0;
				args = (char **) my_realloc ("parse_line: args", args, ((*argc)+1)*sizeof(char *));
				if (args == NULL) {
					fprintf (stderr, "parse_line: realloc() error.\n");
					return (0);
				}
				args[*argc] = &string[i];
				(*argc)++;
				break;
			}
			break;
		}
	}
	*argv = args;
	return (1);
}

/* $Id: exec_subs.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
