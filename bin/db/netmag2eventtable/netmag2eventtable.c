#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
extern void *calloc(size_t, size_t);

int database_changed( char *databasename )
{
	static int changeinit;
	static off_t table_size;
	static time_t table_mtime;

        static int settest = 1;
        Dbptr db, dbtable;
        int changed;
        long numrows;
        static Dbvalue value;
        struct stat table_statbuf;

        if ( settest == 1 ) {
                settest = 0;
                changeinit = (int) 0;
                table_size = (off_t) 0;
                table_mtime = (time_t) 0;
        }

        changed = 0;
        if(dbopen( databasename, "r", &db) != 0) {
        	fprintf( stderr, "database_changed: Error opening input database %s\n", databasename);
        } else
          {
        	dbtable = dblookup( db, 0, "netmag", 0, 0 );
                dbquery(dbtable, dbRECORD_COUNT, &numrows );
                if ( (long) numrows > (long) 0 ) {
                	dbquery( dbtable, dbTABLE_FILENAME, &value );
			stat( value.t, &table_statbuf );

                        if( ! changeinit ) {
                        	table_size = (off_t) table_statbuf.st_size;
                        	table_mtime = (time_t) table_statbuf.st_mtime;
                        	changeinit++;
                        	changed = 1;
                         } else
                           {
                         	if( (off_t)table_size != (off_t) table_statbuf.st_size ||
                                	(time_t) table_mtime != (time_t) table_statbuf.st_mtime ) {
                               		table_size = (off_t) table_statbuf.st_size;
                                	table_mtime = (time_t) table_statbuf.st_mtime;
                                	changed = 1;
                                }
                        }
		}
		dbclose(db);
	}
	return(changed);
}

int main (int argc, char **argv)
{
    Dbptr           dbin, dbout;
    char           *database_in, *database_out, *igauth;
    long            nrecords_in, nrecords_out;
    char	    minepochs[100];
    int		    days, found;
    double	    min_lddate;
    
   struct TableInfo {
    	long orid, evid;
	char auth[16];
	double lddate;
   };
   
   struct TableInfo *netmag_info_a, *netmag_info_b, *event_info;
   struct TableInfo tempsort;
   int inputchange, newrows, x, y; 
   
    elog_init ( argc, argv ) ;
    if (argc!=5) {
        die (0, "Usage: netmag2eventtable dbin_netmag dbout_event ignore_auth days \n\t This program converts the evid and orid entries in the netmag table into\n\t evid and prefor rows in a event table\n\t example auth expression \'auth !~ /USGS.*/\'" ) ;
    }

database_in = argv[optind++];
database_out = argv[optind++];
igauth = argv[optind++];
days = atoi(argv[optind++]);
fprintf(stdout,"Input database table is %s.netmag,\n\t output database table is %s.event, \n\t ignore rows with these authors '%s',\n\t only keep input rows that are less than %d\n",
	database_in, database_out, igauth, days);
while (1) {
  inputchange = database_changed(database_in);
  sleep(5);
  if(inputchange == 1) {
   fprintf(stdout,"Netmag table modification detected at %s waiting\n",strtime(now()));
   if ( dbopen ( database_in, "r", &dbin ) ) {
       die (0, "Can't open dbin '%s'", database_in ) ;
   }
   if ( dbopen ( database_out, "r+", &dbout ) ) {
       die (0, "Can't open dbout '%s'", database_out ) ;
   }

  /* read information from netmat table */
  dbin = dblookup (dbin, 0, "netmag", 0, 0);
  dbin = dbsubset (dbin, igauth, 0);
  dbin = dbsubset (dbin, "magtype == \"ml\"", 0);
  min_lddate = (double) now() - (days * 3600.0 * 24.0);
  sprintf(minepochs, "lddate > %f", min_lddate);
  dbin = dbsubset (dbin, minepochs, 0); 
  dbquery (dbin, dbRECORD_COUNT, &nrecords_in);

  netmag_info_a = (struct TableInfo *) calloc(nrecords_in, sizeof(struct TableInfo));
  netmag_info_b = (struct TableInfo *) calloc(nrecords_in, sizeof(struct TableInfo));

  newrows = 0;
  for(dbin.record=0; dbin.record < nrecords_in; ++dbin.record) {
	if(dbgetv(dbin, 0,
                "orid", &(netmag_info_a[dbin.record].orid),
                "evid", &(netmag_info_a[dbin.record].evid), 
                "auth", netmag_info_a[dbin.record].auth,
                "lddate", &(netmag_info_a[dbin.record].lddate), NULL) == dbINVALID) {
                        complain(0,"dbgetv problem loading netmag record=%ld\n", dbin.record);
		}
  }
  /* sort evid */ 
  for(x=0; x < nrecords_in-1; ++x) {
  	for(y=x+1; y < nrecords_in; ++y) {
		/* swap */
		if ((long)netmag_info_a[x].evid > (long)netmag_info_a[y].evid ) {
			tempsort.evid = netmag_info_a[x].evid;
			tempsort.orid = netmag_info_a[x].orid;
			(void) strcpy (tempsort.auth, netmag_info_a[x].auth);
			tempsort.lddate = netmag_info_a[x].lddate;

			netmag_info_a[x].evid = netmag_info_a[y].evid;
			netmag_info_a[x].orid = netmag_info_a[y].orid;
			(void) strcpy (netmag_info_a[x].auth, netmag_info_a[y].auth);
			netmag_info_a[x].lddate = netmag_info_a[y].lddate;
			
			netmag_info_a[y].evid = tempsort.evid;
			netmag_info_a[y].orid = tempsort.orid;
			(void) strcpy (netmag_info_a[y].auth, tempsort.auth);
			netmag_info_a[y].lddate = tempsort.lddate;
		}
	}
  }
  x = 1;
  y = 0;
  netmag_info_b[y].evid = netmag_info_a[0].evid;
  netmag_info_b[y].orid = netmag_info_a[0].orid;
  (void) strcpy (netmag_info_b[y].auth, netmag_info_a[0].auth);
  netmag_info_b[y].lddate = netmag_info_a[0].lddate;
  
  while (x < nrecords_in) {
	if (((long)netmag_info_b[y].evid == (long)netmag_info_a[x].evid) && 
	    ((long)netmag_info_b[y].orid < (long)netmag_info_a[x].orid)) {
  		netmag_info_b[y].evid = netmag_info_a[x].evid;
  		netmag_info_b[y].orid = netmag_info_a[x].orid;
  		(void) strcpy (netmag_info_b[y].auth, netmag_info_a[x].auth);
  		netmag_info_b[y].lddate = netmag_info_a[x].lddate;
	}
	if ((long)netmag_info_b[y].evid != (long)netmag_info_a[x].evid) {
		++y; 
  		netmag_info_b[y].evid = netmag_info_a[x].evid;
  		netmag_info_b[y].orid = netmag_info_a[x].orid;
  		(void) strcpy (netmag_info_b[y].auth, netmag_info_a[x].auth);
  		netmag_info_b[y].lddate = netmag_info_a[x].lddate;
	}
	++x;
  }
  newrows = y+1;

  /* Load current event table */
  dbout = dblookup (dbout, 0, "event", 0, 0);
  dbquery (dbout, dbRECORD_COUNT, &nrecords_out);
  event_info = (struct TableInfo *) calloc(nrecords_out, sizeof(struct TableInfo));
  for(dbout.record=0; dbout.record < nrecords_out; ++dbout.record) {
	if(dbgetv(dbout, 0,
                "prefor", &(event_info[dbout.record].orid),
                "evid", &(event_info[dbout.record].evid), 
                "auth", event_info[dbout.record].auth,
                "lddate", &(event_info[dbout.record].lddate), NULL) == dbINVALID) {
                        complain(0,"dbgetv problem loading output event record=%ld\n", dbout.record);
		}
  }
  /* update existing rows */
  for(x=0; x < nrecords_out; ++x) {
	found = 0;
  	for(y=0; y < newrows; ++y) {
		if ((long)event_info[x].evid == (long)netmag_info_b[y].evid ) {
			found = 1;
			/* update event row with new orid */
			if ((long)event_info[x].orid != (long)netmag_info_b[y].orid ) {
				dbout.record = x;
				if (dbputv(dbout, 0, 
					"evid", netmag_info_b[y].evid, 
					"prefor", netmag_info_b[y].orid,
                			"auth", netmag_info_b[y].auth,
                			"lddate", netmag_info_b[y].lddate, NULL) == dbINVALID) {
                        		complain(0,"dbputv problem loading event row at index %d\n", x);
				} else
				  {
					fprintf(stdout,"Updated row at record %d\n\t evid=%ld, prefor=%ld, \n\t  auth=%s,lddate=%f\n",
					 	x, netmag_info_b[y].evid, netmag_info_b[y].orid, netmag_info_b[y].auth, netmag_info_b[y].lddate );
				}

			}
			netmag_info_b[y].evid = -2; /* this id has been processed and is done */
		}
	}
	/* row not found */
	if (found == 0 ) {
		dbout.record = x;
		dbmark(dbout);
	}
  }
  /* add all new rows that were not updated */
  dbout = dblookup( dbout, 0, "event", 0, 0 ); 
  for(y=0; y < newrows; ++y) {
	if ((long)netmag_info_b[y].evid != (long)-2 ) {
		dbout.record = dbaddnull (dbout);
		fprintf(stdout,"Adding row at record %ld\n\t evid=%ld, prefor=%ld, \n\t  auth=%s,lddate=%f\n",
		 	dbout.record, netmag_info_b[y].evid, netmag_info_b[y].orid, netmag_info_b[y].auth, netmag_info_b[y].lddate );
		if (dbputv(dbout, 0, 
			"evid", netmag_info_b[y].evid, 
			"prefor", netmag_info_b[y].orid,
               		"auth", netmag_info_b[y].auth,
               		"lddate", netmag_info_b[y].lddate, NULL) == dbINVALID) {
                       	complain(0,"dbputv problem loading event row at index %d\n", x);
		} 
	}
  }

  fflush(stdout);
  free(netmag_info_a);
  free(netmag_info_b);
  free(event_info);
  dbclose(dbin);
  dbcrunch(dbout);
  dbclose(dbout); 
 } /* inputchange if */
} /* while loop */ 
}  /* main */
