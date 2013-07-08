#include <stdio.h>
#include "db.h"
#include "pick.h"
#include "iceworm_extensions.h" /* current location /opt/antelope/4.2u/include/ */
#define MAX_STATIONS 1000
#define MAXPICK 500 /* Max number of picks per tracepacket */
int setup_pins( char *, char * );
static char     SiteDb[FILENAME_MAX];
int new_pin[MAX_PIN_NUM+1];/* Array tracks which pins have been seen */
int ignore_pin[MAX_PIN_NUM+1];  /* Array tracks which pins to ignore */
typedef struct {
	int     pinno;  /* Pin number for this structure's parameters */
	char    sta[7];
	char    chan[9];
	char    net[9];
	struct pick_rapar par;/* Parameters for picking this sta-chan */
} STARTUP;

typedef struct {
	char    sta[7];
	char    chan[9];
	struct pick_trace trace;
} RAW_DATA;

	
STARTUP startup_data[MAX_PIN_NUM+1], *startup;


main (argc, argv)
	int argc;
	char **argv;
{
	char *begin_st, *end_st, *db_name;
	int optind,nrecs1,x,x2,index,nsamp;
	float *ptr;
	double samprate, time;
	Dbptr db1, db2, trp1;
	int arid;
	RAW_DATA db1raw[MAX_STATIONS];
	int n, pinno;
	char    *str, *expr;
	struct pick_trace trace;
	struct pick_rainfo info;
	struct pick pick[MAXPICK];
	struct coda coda[MAXPICK];
	int     npick;
	int     restart, pick_status;
	if (!( argc == 7 || argc == 8 )) {
		usage();
		exit(0);
	}
	expr = NULL;
	if (argc == 8) {
		expr = argv[7];
		--argc;
	}
	/* get arguments */
	for (optind=1 ; optind < argc ; optind++ ) {
		if (strcmp(argv[optind], "-begin") == 0) {
			begin_st = argv[++optind];
		} else if (strcmp(argv[optind], "-end") == 0) {
			end_st = argv[++optind];
		} else if (strcmp(argv[optind], "-db") == 0) {
			db_name = argv[++optind] ;
		}
	}
	/* open data database and get data*/
	if ( dbopen(db_name, "r", &db1) != 0 ) {
		die ( 0, "Error opening database %s\n", db_name) ;
	}
	db1 = dblookup( db1, 0, "wfdisc", 0, 0 );
	if(expr != NULL) {
		db1 = dbsubset ( db1, expr, 0);
	}
	trp1 = dbinvalid();
	if( trload_css ( db1, begin_st, end_st, &trp1, 0, 0 ) != 0) {
		die ( 0, "Error reading trace data from %s\n",db_name);
	}
	if(trsplice(trp1,100/*trTOLERANCE*/,0,0) == -1) {
		die ( 0, "Error splicing trace data from %s\n",db_name);
	}
	dbquery( trp1, dbRECORD_COUNT, &nrecs1 );
	trp1.record = 0;
	for(x=0; x < nrecs1; ++x) {
		dbgetv( trp1, 0,"sta",
		db1raw[x].sta,"chan",db1raw[x].chan, 
		"time", &time, 
		"nsamp", &nsamp, 
		"samprate", &samprate,
		"data", &ptr, 0);
		db1raw[x].trace.nsamp = nsamp;
		db1raw[x].trace.tofs = time;
		db1raw[x].trace.sint = 1.0 / 
			samprate ;
		/* convert floats into longs */
		db1raw[x].trace.data = malloc (db1raw[x].trace.nsamp * 
			sizeof(long));
		for(x2=0; x2<db1raw[x].trace.nsamp; ++x2) {
			db1raw[x].trace.data[x2] = ptr[x2];
		}
		free(ptr);
		++trp1.record;
	}
	dbclose(db1);

	/* get picker information */
	strcpy( SiteDb, db_name );
	n = setup_pins( SiteDb, expr );
	/* process picks */
	n = 0;
	if ( dbopen(db_name, "r+", &db2) == dbINVALID ) {
		clear_register (1);
		die ( 0, "Error opening database %s for writing\n", db_name) ;
	}
	db2 = dblookup( db2, 0, "arrival", 0, 0 );
	for( pinno = 0; pinno <= MAX_PIN_NUM; pinno++ ) {
		startup = (STARTUP *) &startup_data[pinno];
		pick_raldpar( NULL, &startup->par);
		info.first = 1;
		restart = 1;
		if(ignore_pin[pinno] == 0) {
			++n;
			/* look for trace data */
			for(x=0; x < nrecs1; ++x) {
				if(strcmp(startup_data[pinno].sta,
				  db1raw[x].sta) == 0 &&
				  strcmp(startup_data[pinno].chan,
				  db1raw[x].chan) == 0) {
					pick_status = Pick_RexAllen( &(db1raw[x].trace), 
					&info, &startup->par,
					pick, coda, &npick, MAXPICK, restart );
					/* picks found */
					if(npick > 0) {
						for( index = 0; index < npick; index++ ) {
							/* open data database and */
							/* write arrivals */
							arid = dbnextid (db2, "arid");
						 	if( dbaddv( db2, "arrival", 
							    "sta",  startup_data[pinno].sta, 
							    "time", pick[index].time,
							    "arid", arid,
							    "jdate", yearday(pick[index].time),
							    "chan", startup_data[pinno].chan,
							    /* "iphase", "D",
							    "fm", pick[index].fm, */
							    0 ) == dbINVALID ) {
								clear_register (1);
							
					                }
						}
					} 
				}
			}
		}
	}
	dbclose(db2);
	exit(0);
}
int setup_pins( char *sitedb, char *expr )
{
	Dbptr	db, dbt;
	int	pin_index;
	int	pinno;
	struct pick_rapar *par;
	char	sta[7];
	char	chan[9];
	char	net[9];
	int	n;

	for( pin_index = 0; pin_index <= MAX_PIN_NUM; pin_index++ )
	{
		new_pin[pin_index] = 1;
		ignore_pin[pin_index] = 1;
	}

	if( dbopen( sitedb, "r", &db ) == dbINVALID ) 
	{
		clear_register( 1 );
		exit( 1 );
	}
	db = dblookup( db, 0, "picker", 0, 0 );
	if( expr != NULL) {
		db = dbsubset ( db, expr, 0 ) ;
	}
	dbt = dblookup( db, 0, "pins", 0, 0 );
	db = dbjoin( db, dbt, 0, 0, 0, 0, 0 );
	db = dbsubset( db, "savechan == \"y\" && pickit == \"y\"", 0 );
	dbquery( db, dbRECORD_COUNT, &n );

	for( db.record = 0; db.record < n; db.record++ )
	{
		dbgetv( db, 0, "pinno", &pinno, 0 );
		ignore_pin[pinno] = 0;
		startup_data[pinno].pinno = pinno;
		par = &startup_data[pinno].par;
		dbgetv( db, 0, "n1", &par->n1,
			       "i5", &par->i5,
			       "i6", &par->i6,
			       "i7", &par->i7,
			       "i8", &par->i8,
			       "i9", &par->i9,
			       "c1", &par->c1,
			       "c2", &par->c2,
			       "c3", &par->c3,
			       "c4", &par->c4,
			       "c5", &par->c5,
			       "c6", &par->c6,
			       "c7", &par->c7,
			       "c8", &par->c8,
			       "c9", &par->c9,
			       "sta", sta,
			       "chan", chan,
			       "net", net,
			       0 );
		strcpy( startup_data[pinno].sta, sta );
		strcpy( startup_data[pinno].chan, chan );
		strcpy( startup_data[pinno].net, net );
	}
	dbclose(db);
	return(n);
}
usage()
{
	fprintf(stderr, "Usage: dbicepick -begin time -end time -db db [expr]\n");
	fprintf(stderr, "Example: dbicepick -begin \"08/23/2000 13:30:00\" -end \"08/23/2000 13:33:00\" -db pickerdb/pickerdb 'sta =~ /W.*/'\n");
}
