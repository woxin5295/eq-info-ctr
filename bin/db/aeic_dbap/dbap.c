
#include <stdio.h>
#include <math.h>

#include "tcl.h"
#include "tk.h"

#include "db.h"
#include "arrays.h"
#include "tks.h"


char *dbin;
char *dbout;
int verbose=0;
int noexit=0;
int tk=0;
Tcl_DString command;
int tty;
char *fileName;

Dbptr dbi;
Dbptr dbo;
Tbl *sc;

main (argc, argv)

int argc;
char **argv;

{
	char **recipes;
	int i, n;
	double ts, te;
	double t1, t2;
	char *proc;
	Tcl_Interp *interp;
	char line[2048];
	char *name;
	TkSend *tkscom=NULL;
	XEvent event;
	char *msg;
	char *script;
	char **myargv;
	int replyrequest;

	/* Get arguments */

	Program_Name = argv[0] ; 

/*	 dbin and dbout are the filenames of the input and output databases (respectively).
	 Originally they simply pointed into argv, which worked because the databases had
	 to be specified on the command line (or not at all in the case of dbout), and could
	 not be changed at run time. Now that we're allowing runtime changing of databases,
	 that doesn't work, so we have to allocate memory for those two variables: -- Tobin 6/25/99 */

	dbin = malloc(500);
	dbout = malloc(500);
	dbo.database = -1;
	dbi.database = -1;
	dbout[0]=0;
	dbin[0]=0;

	get_args (argc, argv, dbin, dbout, &recipes, &n, &script);
        
#if 0
#ifdef __sun
	abrupt_underflow_ ();
#endif
#endif

	/* parse recipe .pf files */

	for (i=0; i<n; i++) {
		if (!parse_pf (recipes[i])) {
			fprintf (stderr, "dbap: parse_pf() error on '%s.pf'.\n", recipes[i]);
			exit (0);
		}
	}

/* opening of dbout, dbin have been moved to tcl_wrapper.c -- Tobin 6/25/99 */
 
	/* Tk processing loop */

	/* set up environment variables if required. */
	envfile ( "dbwish" ) ; 

	if (tk) {
		if (!script) {
			fprintf (stderr, "dbap: Need script file for tk processing.\n");
			exit (0);
		}
		myargv = (char **) malloc ((argc+2)*sizeof(char *));
		if (myargv == NULL) {
			fprintf (stderr, "dbap: malloc() error.\n");
			exit (0);
		}
		myargv[0] = argv[0];
		myargv[1] = script;
		for (i=1; i<argc; i++) myargv[i+1] = argv[i];
		argc += 1;
		if (verbose) printf ("Processing...\n");
		TkX_MainEx (argc, myargv, Tcl_AppInit, Tcl_CreateInterp() );
		if (verbose) printf ("Exiting\n");
		exit (0);
	}

	/* Connect to server for tkscom */

	var_get ("tkscom", &name);
	if (name) {
		tkscom = Tks_Create (0, 0, 0);
		if (tkscom == NULL) {
			fprintf (stderr, "dbap: Tks_Create() error.\n");
			exit (0);
		}
		Tks_ClearAppName (tkscom, name);
		if (!Tks_SetAppName (tkscom, name)) {
			fprintf (stderr, "dbap: Tks_SetAppName() error.\n");
			exit (0);
		}

	}

	/* Set up tcl stuff */

	interp = Tcl_CreateInterp();
	if (interp == NULL) {
		fprintf (stderr, "dbap: Tcl_CreateInterp() error.\n");
		if (tkscom) Tks_ClearAppName (tkscom->display, name);
		exit (0);
	}
	fileName = script;
	if (Tcl_AppInit(interp) == TCL_ERROR) {
		fprintf (stderr, "dbap: Tcl_AppInit() error.\n");
		if (tkscom) Tks_ClearAppName (tkscom->display, name);
		exit (0);
	}

	/* Process loop */

	if (verbose) printf ("Processing...\n");
	if (tkscom) {
		i=0;
		while (1) {
			XNextEvent (tkscom->display, &event);
			replyrequest = 0;
			if (!Tks_GetmsgEventProc (tkscom, &event, &proc, &replyrequest)) {
				fprintf (stderr, "dbap: Tks_GetmsgEventProc() error.\n");
				break;
			}
			if (!proc) continue;
			if (verbose) printf ("%3.3d: %s\n", i++, proc);
			strcpy (line, proc);
			if (!strcmp(line, "exit")) break;
			if (!strcmp(line, "quit")) break;
			if (strncmp(proc, "get ", 4)) {
				if (replyrequest) {
					Tks_Reply (tkscom, "");
				}
			}
			if (Tcl_Eval (interp, line) == TCL_ERROR) {
				fprintf (stderr, "dbap: Tcl_Eval() error.\n");
				fprintf (stderr, "      %s\n", interp->result);
			}
			if (!strncmp(proc, "get ", 4)) {
				if (replyrequest) {
					if (interp->result) Tks_Reply (tkscom, interp->result);
					else Tks_Reply (tkscom, "");
				}
			}
		}
		if (replyrequest) {
			Tks_Reply (tkscom, "exiting");
		}
	} else {
		if (script == NULL) {
			while (1) {
				printf ("dbap> ");
				fflush (stdout);
				if (!fgets (line, 2047, stdin)) {
					printf ("\n");
					break;
				}
				if (!strcmp(line, "exit")) break;
				if (!strcmp(line, "quit")) break;
				if (Tcl_Eval (interp, line) == TCL_ERROR) {
					fprintf (stderr, "dbap: Tcl_Eval() error.\n");
					fprintf (stderr, "      %s\n", interp->result);
				}
			}
		} else {
			if (Tcl_VarEval (interp, "source ", script, 0) == TCL_ERROR) {
					fprintf (stderr, "dbap: Tcl_VarEval() error.\n");
					fprintf (stderr, "      %s\n", interp->result);
					exit (0);
			}
		}
	}

	/* Normal exit */

	if (tkscom) Tks_ClearAppName (tkscom->display, name);
	if (verbose) printf ("Exiting\n");

	free(dbin);  /* Where else should I put this? -- Tobin 6/25/99 */
	free(dbout);

	exit (0);
}

int
get_args (argc, argv, dbin, dbout, recipes, nrecipes, script)

int argc;
char **argv;
char *dbin, *dbout, ***recipes;
int *nrecipes;
char **script;

{
	int i, n;
	double time;
	char string[512];
	char *name, *value ;

	if (argc < 3) {    /* -dbin is no longer required since it may be set at runtime -- Tobin 6/25/99 */
		usage();
		exit (0);
	} 

	*recipes = NULL;
	*script = NULL;
	if (!var_put ("dbo_name", "")) {
		fprintf (stderr, "get_args: var_put() error.\n");
		usage();	
		exit (0);
	}
	for (argc--,argv++; argc>0; argc--,argv++) {
		if (!strcmp(*argv,"-V")) {
		    /* Banner is for Antelope only; this modified version 
		       is now contrib code and needs to convert to cbanner
		    banner( Program_Name, "Version $Revision: 1.7 $ $Date: 2002-02-07 01:23:49 $\n" ) ; 
		    */
		    exit (0);
		}
		if (!strcmp(*argv, "-dbin")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for -dbin.\n");
				usage();
				exit (0);
			} else {
                        strcpy(dbin,argv[0]);
			if (!var_put ("dbi_name", dbin)) {
				fprintf (stderr, "get_args: var_put() error.\n");
				usage();
				exit (0); }
			}
		} else if (!strcmp(*argv, "-dbout")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for -dbout.\n");
				usage();
				exit (0);
			}
			if (!strcmp(*argv, "none")) continue;
 		        strcpy(dbout,argv[0]);	
			if (!var_put ("dbo_name", dbout)) {
				fprintf (stderr, "get_args: var_put() error.\n");
				usage();
				exit (0);
			}
		} else 
  		 if (!strcmp(*argv, "-v")) {
			verbose = 1;
		} else if (!strcmp(*argv, "-V")) {
			verbose = 2;
		} else if (!strcmp(*argv, "-noexit")) {
			noexit = 1;
		} else if (!strcmp(*argv, "-tk")) {
			tk = 1;
		} else if (!strcmp(*argv, "-recipe")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for -recipe.\n");
				usage();
				exit (0);
			}
			for (n=0; argc-n>0; n++) {
				if (*(argv[n]) == '-') break;
			}
			*recipes = (char **) my_malloc ("get_args: *recipes", n*sizeof(char *));
			if (*recipes == NULL) {
				fprintf (stderr, "get_args: malloc error.\n");
				usage();
				exit (0);
			}
			*nrecipes = n;
			for (i=0; i<n; i++,argc--,argv++) {
				(*recipes)[i] = *argv;
			}
			argc++;
			argv--;
		} else if (!strcmp(*argv, "-script")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for -script.\n");
				usage();
				exit (0);
			}
			*script = *argv;
		} else if (!strcmp(*argv, "-ts")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for -ts.\n");
				usage();
				exit (0);
			}
			if (!time_string2epoch(*argv, &time)) {
				fprintf (stderr, "get_args: Unable to parse -ts time.\n");
				usage();
				exit (0);
			}
			sprintf (string, "%.5lf", time);
			if (!var_put ("ts", string)) {
				fprintf (stderr, "get_args: var_put() error.\n");
				usage();
				exit (0);
			}
		} else if (!strcmp(*argv, "-te")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for -te.\n");
				usage();
				exit (0);
			}
			if (!time_string2epoch(*argv, &time)) {
				fprintf (stderr, "get_args: Unable to parse -te time.\n");
				usage();
				exit (0);
			}
			sprintf (string, "%.5lf", time);
			if (!var_put ("te", string)) {
				fprintf (stderr, "get_args: var_put() error.\n");
				usage();
				exit (0);
			}
		} else if (**argv == '-' && isalpha((*argv)[1])) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for %s.\n", *argv);
				process_usage();
				return (NULL);
			}
			name = *argv++ + 1; 
			value = *argv ;
			if (!var_put (name, value) ) {
				fprintf (stderr, "get_args: var_put() error.\n");
				usage();
				exit (0);
			}
		} else if (!strcmp(*argv, "-geometry")) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for %s.\n", *argv);
				process_usage();
				return (NULL);
			}
			argv++;
		} else if (!strcmp(*argv, "-display")) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for %s.\n", *argv);
				process_usage();
				return (NULL);
			}
			argv++;
		} else if (!strcmp(*argv, "-name")) {
			argc--;
			if (argc < 1) {
				fprintf (stderr, "get_args: No argument for %s.\n", *argv);
				process_usage();
				return (NULL);
			}
			argv++;
		} else if (!strcmp(*argv, "-sync")) {
		} else {
			fprintf (stderr, "get_args: Illegal argument '%s'.\n", *argv);
			usage();
			exit (0);
		}
	}
	if (*recipes == NULL) {
		fprintf (stderr, "get_args: -recipe required.\n");
		usage();
		exit (0);
	} 
}

int
usage()

{
	fprintf (stderr, "usage: dbap -dbin dbin -recipe recipe1 [recipe2 [...]]\n");
	fprintf (stderr, "            [-script script_file] [-dbout dbout] [-v] [-tk]\n");
	fprintf (stderr, "            [-tkscom appname]\n");
	fprintf (stderr, "            [-ts tstart_time] [-te end_time]\n");
	fprintf (stderr, "            [-arid arid] [-arid_t0 arid_t0] [-arid_tw arid_tw]\n");
	fprintf (stderr, "            [-chan chan] [-filter filter] [-samprate samprate]\n");
	fprintf (stderr, "            [-azimuth azimuth] [-slow slow] [-name1 value1]\n");
	fprintf (stderr, "            [-name2 value2] [...]\n");
}

/* $Id: dbap.c,v 1.7 2002-02-07 01:23:49 kent Exp $ */
