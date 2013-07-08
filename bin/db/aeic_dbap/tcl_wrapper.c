

/* 
 * tkXAppInit.c --
 *
 *      Provides a default version of the Tcl_AppInit procedure for use with
 *      applications built with Extended Tcl and Tk.  This is based on the
 *      the UCB Tk file tkAppInit.c
 *
 *-----------------------------------------------------------------------------
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tcl_wrapper.c,v 1.5 2011-01-14 00:55:59 cvsaeic Exp $
 *-----------------------------------------------------------------------------
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "tclExtend.h"
#include "tk.h"
#include <math.h>
#include "db.h"
#include <string.h>

/* Begin modification by Tobin, 6/25/99 */
extern Dbptr dbi;
extern Dbptr dbo;
extern char *dbout;
extern char *dbin;
extern Tbl *sc;
/* End */

/* Begin Mitch */
extern int exec_process(), exec_get (), exec_plot ();
extern int exec_write (), exec_free (), exec_set_recipe (); 
extern int exec_get_recipe (), exec_read_gather (); 
extern int exec_process_gather (), exec_assoc ();
extern int Itcl_Init(), Tkx_Init(), Itk_Init();
extern int tcl_setvar(), var_put(), get_overall_times(), make_scs();
/* End Mitch */

int tk;
char *fileName;

/*
 * The following variable is a special hack that allows applications
 * to be linked using the procedure "main" from the Tk library.  The
 * variable generates a reference to "main", which causes main to
 * be brought in from the library (and all of Tk and Tcl with it).
 */

EXTERN int main _ANSI_ARGS_((int     argc,
                             char  **argv));
/* int (*tclXDummyMainPtr)() = (int (*)()) main;*/

/*
 * The following variable is a special hack that insures the tcl
 * version of matherr() is used when linking against shared libraries
 * Only define if matherr is used on this system.
 */

#if defined(DOMAIN) && defined(SING)
EXTERN int matherr _ANSI_ARGS_((struct exception *));
int (*tclDummyMathPtr)() = (int (*)()) matherr;
#endif


/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
    Tk_Window main;
    int tty;
	int Tcl_process();
	int Tcl_set_recipe();
	int Tcl_get_recipe();
	int Tcl_get();
	int Tcl_plot();
	int Tcl_write();
	int Tcl_free();
	int Tcl_read_gather();
	int Tcl_process_gather();
	int Tcl_assoc();
	int Tcl_set_database_in();    /* Tobin 6/25/99 */
	int Tcl_set_database_out();   /* Tobin 6/25/99 */

    if (tk) main = Tk_MainWindow(interp);

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    if (Tcl_Init(interp) == TCL_ERROR) {
	fprintf (stderr, "Tcl_AppInit: Tcl_Init() error '%s'.\n", interp->result);
	return TCL_ERROR;
    }
    /* Extended tcl */
    if (!tk) {
    	tty = isatty(0);
    	Tcl_SetVar(interp, "tcl_interactive",
    	            ((fileName == NULL) && tty) ? "1" : "0", TCL_GLOBAL_ONLY);
    }
/* Mitch committed out below Lines */
/*    if (Tclx_Init(interp) == TCL_ERROR) {
*	fprintf (stderr, "Tcl_AppInit: Tclx_Init() error '%s'.\n", interp->result);
*	return TCL_ERROR;
*    }
*    Tcl_StaticPackage (interp, "Tclx", Tclx_Init, Tclx_SafeInit );
*/
    if (Itcl_Init(interp) == TCL_ERROR) {
	fprintf (stderr, "Tcl_AppInit: Itcl_Init() error '%s'.\n", interp->result);
	return TCL_ERROR;
    }

    if (tk) if (Tkx_Init(interp) == TCL_ERROR) {
	fprintf (stderr, "Tcl_AppInit: TkX_Init() error '%s'.\n", interp->result);
	return TCL_ERROR;
    }
    /* Mitch This may not work */
/*    Tcl_StaticPackage (interp, "Tkx", Tkx_Init, Tkx_SafeInit);
*
*    if (tk) if (Itk_Init(interp) == TCL_ERROR) {
*	fprintf (stderr, "Tcl_AppInit: Itk_Init() error '%s'.\n", interp->result);
*	return TCL_ERROR;
*    }
*/
    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

	Tcl_Eval ( interp, "package require Datascope" ) ;


	Tcl_CreateCommand (interp, "process", Tcl_process, NULL, NULL);
	Tcl_CreateCommand (interp, "get", Tcl_get, NULL, NULL);
	Tcl_CreateCommand (interp, "plot", Tcl_plot, NULL, NULL);
	Tcl_CreateCommand (interp, "write", Tcl_write, NULL, NULL);
	Tcl_CreateCommand (interp, "free", Tcl_free, NULL, NULL);
	Tcl_CreateCommand (interp, "set_recipe", Tcl_set_recipe, NULL, NULL);
	Tcl_CreateCommand (interp, "get_recipe", Tcl_get_recipe, NULL, NULL);
	Tcl_CreateCommand (interp, "read_gather", Tcl_read_gather, NULL, NULL);
	Tcl_CreateCommand (interp, "process_gather", Tcl_process_gather, NULL, NULL);
	Tcl_CreateCommand (interp, "assoc", Tcl_assoc, NULL, NULL);
	Tcl_CreateCommand (interp, "set_db_in",Tcl_set_database_in,NULL,NULL);
	Tcl_CreateCommand (interp, "set_db_out",Tcl_set_database_out,NULL,NULL);
	if (!tcl_setvars (interp)) {
		fprintf (stderr, "tcl_create_commands: tcl_setvars() error.\n");
		return (TCL_ERROR);
	}

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    Tcl_SetVar(interp, "tcl_rcFileName", "~/.dbtclrc", TCL_GLOBAL_ONLY);
    return TCL_OK;
}

#include <malloc.h>
#include <string.h>

#include "arrays.h"

int
Tcl_process(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute process */

	if (!exec_process (argc, argv, string)) {
		sprintf (string, "Tcl_process: exec_process() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	Tcl_SetResult(interp, string, TCL_VOLATILE);
	return TCL_OK;
}

int
Tcl_get(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute get */

	if (!exec_get (argc, argv, string)) {
		sprintf (string, "Tcl_process: exec_get() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	Tcl_SetResult(interp, string, TCL_VOLATILE);
	return TCL_OK;
}

int
Tcl_plot(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute plot */

	if (!exec_plot (argc, argv)) {
		sprintf (string, "Tcl_process: exec_plot() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	return TCL_OK;
}

int
Tcl_write(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute write */

	if (!exec_write (argc, argv)) {
		sprintf (string, "Tcl_process: exec_write() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	return TCL_OK;
}

/* ***TOBIN*** */

int Tcl_set_database_out(clientData,interp,argc,argv)
void *clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{ char line[2048]; 

  /* Open output database */

        if (argc<2) {
                dbo.database = -1;
		dbout[0]=0; /* nullify the string */
                printf("Output database disabled.\n");
        } else {
		strcpy(dbout,argv[1]);
                if (dbopen (dbout, "r+", &dbo) == dbINVALID) {
                        clear_register (1);
                        fprintf (stderr, "dbap: Unable to open database '%s'.\n", dbout);
                        exit (0);
                }
                printf("Output database is now %s.\n",dbout);
        }
        sprintf (line, "%d %d %d %d", dbo.database, dbo.table, dbo.field, dbo.record);
        if (!var_put ("dbo", line)) {
                fprintf (stderr, "dbap: var_put() error.\n");
                exit (0);
        }
        Tcl_SetVar(interp,"dbo",line,0);
	Tcl_SetVar(interp,"dbo_name",dbout,0);
 return (TCL_OK);

}


int Tcl_set_database_in(clientData,interp,argc,argv)
void 	*clientData;
Tcl_Interp	*interp;
int	argc;
char **argv;
{     double t1, t2;
char line[2048];	
int n;
        if (!(argc==2)) {
                fprintf (stderr, "dbap: db-input requires a database name as an argument.\n");
   		return 0; }
	strcpy(dbin,argv[1]);
        if (dbopen (dbin,"r+", &dbi) == dbINVALID) {
                clear_register (1);
                fprintf (stderr, "dbap: Unable to open database '%s'.\n", "mydatabase");
                exit (0);
        }
        printf("Input database is now %s.\n",dbin);
        sprintf (line, "%d %d %d %d", dbi.database, dbi.table, dbi.field, dbi.record);
        Tcl_SetVar(interp,"dbi",line,0);
 	Tcl_SetVar(interp,"dbi_name",dbin,0);
        /* Set up overall start and end times */

        if (!get_overall_times (&t1, &t2)) {
                clear_register (1);
                fprintf (stderr, "dbap: Unable to determine overall time range.\n");
                exit (0);
        }

        /* Make the station-channel table */

        if (!make_scs (dbi, t1, t2, &sc)) {
                fprintf (stderr, "dbap: make_scs() error.\n");
                exit (0);
        }
        n = maxtbl(sc);
        if (n < 1) {
                fprintf (stderr, "dbap: No data to process.\n");
                exit (0);
        }
 return (TCL_OK);
}


int
Tcl_free(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute free */

	if (!exec_free (argc, argv)) {
		sprintf (string, "Tcl_free: exec_free() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	return TCL_OK;
}

int
Tcl_set_recipe(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute free */

	if (!exec_set_recipe (argc, argv)) {
		sprintf (string, "Tcl_set_recipe: exec_set_recipe() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	return TCL_OK;
}

int
Tcl_get_recipe(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute free */

	if (!exec_get_recipe (argc, argv, string)) {
		sprintf (string, "Tcl_get_recipe: exec_get_recipe() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	Tcl_SetResult(interp, string, TCL_VOLATILE);
	return TCL_OK;
}

int
Tcl_read_gather(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute read_gather */

	if (!exec_read_gather (argc, argv, string)) {
		sprintf (string, "Tcl_read_gather: exec_read_gather() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	Tcl_SetResult(interp, string, TCL_VOLATILE);
	return TCL_OK;
}

int
Tcl_process_gather(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute process_gather */

	if (!exec_process_gather (argc, argv, string)) {
		sprintf (string, "Tcl_process_gather: exec_process_gather() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	Tcl_SetResult(interp, string, TCL_VOLATILE);
	return TCL_OK;
}

int
Tcl_assoc(clientData, interp, argc, argv)

void           *clientData;
Tcl_Interp     *interp;
int             argc;
char          **argv;

{
	char string[512];

	/* Execute assoc */

	if (!exec_assoc (argc, argv, string)) {
		sprintf (string, "Tcl_process: exec_assoc() error.\n");
		Tcl_SetResult(interp, string, TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Normal exit */

	Tcl_SetResult(interp, string, TCL_VOLATILE);
	return TCL_OK;
}

int
tcl_setvars (interp)

Tcl_Interp * interp;

{
	Tbl *names;
	int i, n;
	char *name;
	char *value;

	Tcl_SetVar (interp, "tcl_precision", "17", 0);
	names = (Tbl *) var_get_names();
	if (names) {
		n = maxtbl(names);
		for (i=0; i<n; i++) {
			name = gettbl(names, i);
			var_get (name, &value);
			if (value) {
				Tcl_SetVar (interp, name, value, 0);
			}
		}
	}

	/* Normal exit */

	return (1);
}

/* $Id: tcl_wrapper.c,v 1.5 2011-01-14 00:55:59 cvsaeic Exp $ */
