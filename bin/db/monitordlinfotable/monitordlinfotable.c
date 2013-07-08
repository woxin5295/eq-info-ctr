#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "Pkt.h"
#include "coords.h"
#include "db.h"
#include "stock.h"
#include "brttutil.h"

int debug;
char    recipients[2048];
char	recipientsdutyphone[2048];

struct Dl_Info  {
	double time; /* most recent packet time */
	char name[STRSZ];
	int tagid, skip;
	int status, maint, low, med, high, test;
	char p_mask_desc[17];
       /* currently defined p_mask_desc
       0000000000000001 - general test condition
       0000000000000010 - general status fault
       0000000000000100 - calibration test condition
       0000000000001000 - Data Latency exceeds a specified cutoff status fault
       0000000000010000 - GPS Clock Latency exceeds a specified cutoff status fault
       0000000000100000 - Sensor voltage is below a specified cutoff status fault
       before Nyman and H. pull head out of ass - 
       0000000000000001 - general system maintenance
       0000000000000010 - general system fault
       0000000000000100 - calibration maintenance
       0000000000001000 - Data Latency exceeds a specified cutoff
       */
};

struct Dl_Info dlinfo;

int database_changed( Dbptr db )
{
	static int initialized = 0;
        static off_t a_dlinfo_size = 0;
        static time_t a_dlinfo_mtime = 0;
        int     present, numrows;
        static Dbptr   dba_dlinfo;
        static Dbvalue value;
        struct stat a_dlinfo_statbuf;

	dba_dlinfo = dblookup( db, 0, "a_dlinfo", 0, 0 );
	dbquery(dba_dlinfo, dbRECORD_COUNT, &numrows );
	if ( numrows > 0 ) {
		dbquery( dba_dlinfo, dbTABLE_FILENAME, &value );
		stat( value.t, &a_dlinfo_statbuf );
		if( ! initialized ) {
			a_dlinfo_size = a_dlinfo_statbuf.st_size;
			a_dlinfo_mtime = a_dlinfo_statbuf.st_mtime;
			initialized++;
			/* do not process on restart */
			return 0;
		}
		if( a_dlinfo_size != a_dlinfo_statbuf.st_size ||
			a_dlinfo_mtime != a_dlinfo_statbuf.st_mtime ) {
			a_dlinfo_size = a_dlinfo_statbuf.st_size;
			a_dlinfo_mtime = a_dlinfo_statbuf.st_mtime;
			if( debug == 1) {
				fprintf(stderr,"a_dlinfo table modification detected \n");
			}
			return 1;
		}
	}
	return 0;
}

int read_dlinfo (Dbptr db, char *db_name) {
	int nrecs;
	char infotext[1024];
	char mailmessage[2048];
	char mailmessagedutyphone[2048];
	db = dblookup( db, 0, "a_dlinfo", 0, 0 );
	dbquery (db, dbRECORD_COUNT, &nrecs);
	if ( nrecs > 0 ) {
		/* read last record */
		db.record = nrecs - 1; 
		if( dbgetv(db, 0,"time", &(dlinfo.time), 
			"p_TagName", dlinfo.name, "p_tagid", &(dlinfo.tagid), 
			"p_status", &(dlinfo.status), "p_maint", &(dlinfo.maint), 
			"p_low", &(dlinfo.low), "p_med", &(dlinfo.med), 
			"p_high", &(dlinfo.high), "p_test", &(dlinfo.test), 
			"p_mask_desc",dlinfo.p_mask_desc, 0) == dbINVALID) {
			fprintf(stderr,"dbgetv problem loading a_dlinfo row at index %d\n", db.record);
		}
		/* send email if required */
		if(dlinfo.status == 0) {
			sprintf(infotext,"%s: ", dlinfo.name);  
			/* general status fault */
			if(dlinfo.p_mask_desc[14] == '1') {
				sprintf(&infotext[strlen(infotext)],"general status fault: ");
			} 			
			/* Data Latency exceeds a specified cutoff status fault */
			if(dlinfo.p_mask_desc[12] == '1') {
				sprintf(&infotext[strlen(infotext)],"Data Latency fault: ");
			} 			
       			/* GPS Clock Latency exceeds a specified cutoff status fault */
			if(dlinfo.p_mask_desc[11] == '1') {
				sprintf(&infotext[strlen(infotext)],"Clock Latency fault: ");
			} 			
       			/* Sensor voltage is below a specified cutoff status fault */
			if(dlinfo.p_mask_desc[10] == '1') {
				sprintf(&infotext[strlen(infotext)],"Voltage fault: ");
			} 		
       			
			/* at time */
			sprintf(&infotext[strlen(infotext)],"at %s ", strtime(now()));

			sprintf(mailmessage,"dbmatrixtxt %s \"%s\" | rtmail -s \"%s Fault\" %s\n", db_name, infotext, dlinfo.name,  recipients);
			sprintf(mailmessagedutyphone,"/bin/echo %s Fault | rtmail -s \"%s Fault\" %s\n", dlinfo.name, dlinfo.name,  recipientsdutyphone);
			fprintf(stderr,"Below message sent at %s to email list \n", strtime(now()));
			fprintf(stderr,"\t%s\n", infotext);	
			system(mailmessage);	
			fprintf(stderr,"Fault message sent at %s to cell phone list \n", strtime(now()));
			system(mailmessagedutyphone);	
		}  
	}
}
int usage ( int i, char *message ) {
	fprintf(stderr, "Usage: %s -db db -list recipient[,...] -dutyphone recipient[,...] [-debug] \n", Program_Name );
	switch (i) {
		case 2: fprintf(stderr,"%s \n", message); break;
		case 3: fprintf(stderr,"%s database can not be openned \n", message); break;
	}
	fprintf(stderr,"\t -db, input database to monitor\n");
	fprintf(stderr,"\t -list, list of rtemail recipients \n");
	fprintf(stderr,"\t -dutyphone, list of rtemail dutyphone recipients \n");
	fprintf(stderr,"\t -debug, print out debugging information \n");
	exit(0);
}


int main( int argc, char **argv )
{
	char    dir[FILENAME_MAX];
	char    db_name[STRSZ];	
	static char base[FILENAME_MAX];
	int x, set, changed;
	Dbptr dbdlinfo;

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	dirbase( argv[0], dir, base );
	Program_Name = base;

	debug =  0;
	set =  0;
	for (x=1; x < argc; ++x) {
		if( strcmp("-db", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set; ++x; (void) strcpy(db_name, argv[x]);}
		}
		if( strcmp("-list", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set; ++x; (void) strcpy(recipients, argv[x]);}
		}
		if( strcmp("-dutyphone", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { ++set; ++x; (void) strcpy(recipientsdutyphone, argv[x]);}
		}
		if( strcmp("-debug", argv[x]) == 0 ) {
			debug = 1;
		}
	}
	if (set != 3) {
		usage ( 2, "Required options were not provided");
	}	
	/* open dlinfo database  */
	if(dbopen( db_name, "r", &dbdlinfo ) != 0 ) {
		usage(3, db_name);
	} 
	while ( 1 ) {
		changed = database_changed( dbdlinfo );
		/* database has changed */ 
		if(changed == 1 ) {
			dbclose(dbdlinfo);
			/* reopen  database */
			if(dbopen( db_name, "r", &dbdlinfo ) != 0 ) {
				usage(4, db_name);
			}
			/* check for faults */
			read_dlinfo (dbdlinfo, db_name);	
		} else
		  {
			/* check for database changes in 30 second intervals */
			sleep(30);
		}
	}
}
