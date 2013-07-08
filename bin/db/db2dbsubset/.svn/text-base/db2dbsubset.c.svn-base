/* db2dbsubset  */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "Pkt.h"
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "brttutil.h"
#include "arrays.h"
#include "xtra.h"
#include <sys/wait.h>
long db_destroy;
int debug = 0;

int magfound = 0;
extern void *calloc(size_t, size_t), *realloc(void *, size_t);

struct output_tables {
	char *tb;
	/* type event=1,origin=2,arrival=3,emodel=4,netmag=5,origerr=6 */
	/*      predarr=7, stamag=8, wfmeas=9, mapassoc=10,  */
	/*      quakeregions=11, webmaps=12, assoc=13 */
	int type;
	long numrows; /* -1 can not open output database */
	int curalloc;
	struct ArrivalTable *artb;
	struct AssocTable *asstb;
	struct EmodelTable *emodtb;
	struct EventTable *evtb;
	struct NetmagTable *ntmgtb;
	struct OrigerrTable *orertb;
	struct OriginTable *ortb;
	struct PredarrTable *prartb;
	struct StamagTable *stmgtb;
	struct WfmeasTable *wfmstb;
	struct MapassocTable *mpastb;
	struct QuakeregionsTable *qkrgtb;
	struct WebmapsTable *wbmptb;
};

struct dbinfo {
	char *dbinname;
	char *dboutname;
	Dbptr dbin, dbout, dbproc, fin, dblook;
	Tbl *dbproctbl;
	long maxdays;
	long realloc;
	long crunchinterval;
	long oldcrunch;
	long crunchon;
	long dbinchanged; /* 1 if input database has change 0 if no change */ 
	long lddate; /* 1 is update, 0 is preserve */
	long numot; /* num output tables */
	struct output_tables *ot; 
	long changeinit;
	off_t table_size;
	time_t table_mtime;
	/* ignorerow table check */
	long changeinitig;
	off_t table_sizeig;
	time_t table_mtimeig;
	char *monitor;
};

long sleeptime; /* period between input database checks */
long waitandgetsleep; /* time period to pause before reading input db detected modifications */
long aftereventsleep; /* gap between event processing checks */ 
long onerun, whileloop; /* 0 multiple runs (default), 1 is one run */
char *forkprog; /* fork a subprocess */
char *forkprogargs[128]; /* fork a subprocess args*/
struct db_templates_struct {
	char name[STRSZ];
	struct dbinfo dbi; 
};

struct ArrivalTable {
  long arid, jdate, stassid, chanid, commid; 
  char sta[7], chan[9], iphase[9], stype[2]; 
  char clip[2], fm[3], qual[2], auth[16]; 
  double deltim, azimuth, delaz, slow; 
  double delslo, ema, rect, amp, per, logat, snr; 
  double time, lddate;

  int reset;
	/* dbsi[i].dbi.ot[j].evtb.reset = 0;  value before, inputdb scan */ 
	/* rows that remain 0 after scan get removed for outputdb */
	/* dbsi[i].dbi.ot[j].evtb.reset = 1;  leave alone */ 
	/* dbsi[i].dbi.ot[j].evtb.reset = 2;  update record */ 
	/* dbsi[i].dbi.ot[j].evtb.reset = 3;  new row */
};

int set_table_type(int i, int j, struct db_templates_struct *dbsi) {
	if (strcmp(dbsi[i].dbi.ot[j].tb,"event") == 0) {
		return(1);
	} else { 
	  if  (strcmp(dbsi[i].dbi.ot[j].tb,"origin") == 0) { 
		  return(2);
	  } else { 
	    if  (strcmp(dbsi[i].dbi.ot[j].tb,"arrival") == 0) {
		    return(3);
	    } else { 
	      if  (strcmp(dbsi[i].dbi.ot[j].tb,"emodel") == 0) {
		    return(4);
	      } else { 
	        if  (strcmp(dbsi[i].dbi.ot[j].tb,"netmag") == 0) {
		      return(5);
	        } else { 
		  if  (strcmp(dbsi[i].dbi.ot[j].tb,"origerr") == 0) {
		        return(6);
	          } else { 
	            if  (strcmp(dbsi[i].dbi.ot[j].tb,"predarr") == 0) {
		          return(7);
	            } else { 
		      if  (strcmp(dbsi[i].dbi.ot[j].tb,"stamag") == 0) {
		            return(8);
				      } else { 
			if  (strcmp(dbsi[i].dbi.ot[j].tb,"wfmeas") == 0) {
		              return(9);
			} else { 
			  if  (strcmp(dbsi[i].dbi.ot[j].tb,"mapassoc") == 0) {
			        return(10);
			  } else { 
			    if  (strcmp(dbsi[i].dbi.ot[j].tb,"quakeregions") == 0) {
		                  return(11);
			    } else { 
			      if  (strcmp(dbsi[i].dbi.ot[j].tb,"webmaps") == 0) {
		                    return(12);
			      } else { 
			        if  (strcmp(dbsi[i].dbi.ot[j].tb,"assoc") == 0) {
		                      return(13);
			        } else {  
				        fprintf(stderr,"db2dbsubset.pf Unsupportd table %s in output_tables informatioin %s, exit\n", 
				        dbsi[i].dbi.ot[j].tb, dbsi[i].name);
				        exit(1);
				} /* assoc */
			      } /* webmaps */
			    } /* quakeregions */
		          } /* mapassoc */
                        } /* wfmeas */	
                      } /* stamag */	
                    } /* predarr */	
                  } /* origerr */	
                } /* netmag */	
              } /* emodel */	
            } /*arrival */	
          } /*origin */	
        } /*event */
}

struct db_templates_struct *get_param_file_info( char *pfname, int *numdbtp)
{
	static Pf *pf = 0;
	static Pf *pftemplates_arr;
	Arr *db_templates;
	Arr *dbtemplates_arr;
	Tbl *dbtemplates_tbl;
	Tbl *thresh_tbl;
	static Pf *pfs;
	int i, j, len, x, noblank, current;
	struct db_templates_struct *dbsi;
	char *name, *s;

	/* get parmeter file information */
	if ( pfread ( pfname, &pf ) != 0 ) {
		complain ( 0, "Error in parameter file: %s\n, exit", pfname ) ;
		exit(1);
	}
	
	if (parse_param (pf, "sleeptime", P_LINT, 1, &sleeptime) < 0) {
		sleeptime = 10; /* default */
	}
	if (parse_param (pf, "waitandgetsleep", P_LINT, 1, &waitandgetsleep) < 0) {
		waitandgetsleep = 25; /* default */
	}
	if (parse_param (pf, "aftereventsleep", P_LINT, 1, &aftereventsleep) < 0) {
		aftereventsleep = 240; /* default */
	}
	if (parse_param (pf, "onerun", P_LINT, 1, &onerun) < 0) {
		onerun = 0; /* default */
	}
	if (onerun != 0) onerun = 1;
	if (parse_param (pf, "db_destroy", P_LINT, 1, &db_destroy) < 0) {
		db_destroy = 0; /* default */
	}
	if (db_destroy != 0) db_destroy = 1;

	forkprog = pfget_string ( pf, "forkprog" );
	if (forkprog != NULL) {
		s = forkprog;
		len = strlen(s);
		noblank = 0;
		current = 1;
		for(x=0; x < len; ++x) {
			if((s[x] != '\0' && s[x] != '\n') && noblank == 0) {
				forkprogargs[current] = &s[x];
				noblank = 1;
				++current;
			}
			if(s[x] == ' ' || s[x] == '\n') {
				s[x] = '\0';
				noblank = 0;
			}
		}
		if(current > 1) {
			forkprogargs[0] = forkprogargs[1];
		} else
		  {
			fprintf(stderr,"No progarm name to fork provided \n");
			forkprog = NULL;
		}
		forkprogargs[current] = NULL;
		if(debug == 1) {
			fprintf(stderr,"Will fork\n\t%s", forkprog);
			for(x=1; x < current; ++x) {
				fprintf(stderr," %s", forkprogargs[x]);
			}
			fprintf(stderr,"\n");
		}
	}

	/* Initialize arrays for db templates */
	db_templates = newarr (0);
	if (db_templates == NULL) {
		/* register_error (0, "parse_pf: newarr(db_templates) error.\n"); */
		fprintf(stderr,"db2dbsubset can not created array for db_templates\n");
		exit(1);
	}

	if (parse_param (pf, "db_templates", P_PFARR, 1, &dbtemplates_arr) < 0) {
		/* register_error (0, "parse_pf: parse_param(db_templates) error.\n"); */
		fprintf(stderr,"db2dbsubset requires db2dbsubset.pf to contain db_templates Arr\n");
		exit(1);
	}

	if (parse_param (pf, "db_templates", P_ARRPF, 1, &pftemplates_arr) < 0) {
		/* register_error (0, "parse_pf: parse_param(db_templates) error.\n"); */
		fprintf(stderr,"db2dbsubset requires db2dbsubset.pf to contain db_templates Arr\n");
		exit(1);
	}
 
	dbtemplates_tbl = keysarr( dbtemplates_arr );

	*numdbtp = maxtbl(dbtemplates_tbl);
	if ( *numdbtp > 0 ) {
		/* allocate space for dbsi struct */
		dbsi = (struct db_templates_struct *) 
			calloc(*numdbtp, sizeof(struct db_templates_struct));

	} else
	  {
		fprintf(stderr,"db2dbsubset.pf contains no template infomation, exit\n");
		exit(1);
	}


	for (i=0; i<*numdbtp; i++) { 
		/* get template name */
		name = (char *) gettbl (dbtemplates_tbl, i);
		(void) strcpy( dbsi[i].name, name);
		free(name);
		/* set input db change to true */
		/* this will force first processing of input db */
		dbsi[i].dbi.dbinchanged = 1;
		switch ( pfget(pftemplates_arr, dbsi[i].name, (void **) &pfs) ) {
			case PFARR:
				break;
			case PFSTRING:
				break;
			default:
				/*elog_complain(0, 
				*  "Improper value in db_templates for element %s. Skipping.\n", dbsi[i].name);
				*continue;
				*break;
				*/
				fprintf(stderr,"db2dbsubset.pf element %s has bad value, exit\n",dbsi[i].name);
				exit(1);
		}
		dbsi[i].dbi.dbinname = pfget_string ( pfs, "dbin" );
		if (dbsi[i].dbi.dbinname == NULL) {
			register_error (0, "parse_pf: pfget_string(dbin) error\n");
			fprintf(stderr,"db2dbsubset.pf %s requires dbin value, exit\n",dbsi[i].name);
			exit(1);
		}
		dbsi[i].dbi.monitor = pfget_string ( pfs, "monitor" );
		if (dbsi[i].dbi.monitor == NULL) {
			register_error (0, "parse_pf: pfget_string(monitor) error\n");
			fprintf(stderr,"db2dbsubset.pf %s requires input db table monitor value, exit\n",dbsi[i].name);
			exit(1);
		}

		/* init, table monitoring variables */
		dbsi[i].dbi.changeinit = (int) 0;
		dbsi[i].dbi.table_size = (off_t) 0;
		dbsi[i].dbi.table_mtime = (time_t) 0;
		/* init, ignorrow table monitoring variables */
		dbsi[i].dbi.changeinitig = (int) 0;
		dbsi[i].dbi.table_sizeig = (off_t) 0;
		dbsi[i].dbi.table_mtimeig = (time_t) 0;

		dbsi[i].dbi.dboutname = pfget_string ( pfs, "dbout" );
		if (dbsi[i].dbi.dboutname == NULL) {
			register_error (0, "parse_pf: pfget_string(dbout) error\n");
			fprintf(stderr,"db2dbsubset.pf %s requires dbout value, exit\n",dbsi[i].name);
			exit(1);
		}
		if (parse_param (pfs, "lddate", P_LINT, 1, &(dbsi[i].dbi.lddate)) < 0) {
			dbsi[i].dbi.lddate = 1; /* default */
		}
		if (parse_param (pfs, "maxdays", P_LINT, 1, &(dbsi[i].dbi.maxdays)) < 0) {
			dbsi[i].dbi.maxdays = 0; /* default */
		}
		if (parse_param (pfs, "realloc", P_LINT, 1, &(dbsi[i].dbi.realloc)) < 0) {
			dbsi[i].dbi.realloc = 512; /* default */
		}
		if (parse_param (pfs, "crunchinterval", P_LINT, 1, &(dbsi[i].dbi.crunchinterval)) < 0) {
			dbsi[i].dbi.crunchinterval = 86400; /* onday */
		}
		if (dbsi[i].dbi.crunchinterval < 0) dbsi[i].dbi.crunchinterval = 0;
		dbsi[i].dbi.oldcrunch = 0.0;
		dbsi[i].dbi.crunchon = 1;
		/* dbsi[i].dbi.dbproctbl = pfget_tbl (pfs, "subset_sequence" ) ; */
		if (parse_param (pfs, "subset_sequence", P_TBL, 1, &(dbsi[i].dbi.dbproctbl)) < 0) { 
			register_error (0, "parse_pf: parse_param(subset_sequence) error\n"); 
			fprintf(stderr,"db2dbsubset.pf %s requires subset_sequence Tbl, exit\n",dbsi[i].name); 
			exit(1); 
		} 

		if (parse_param (pfs, "output_tables", P_TBL, 1, &thresh_tbl) < 0) {
			register_error (0, "parse_pf: parse_param(output_tables) error\n");
			fprintf(stderr,"db2dbsubset.pf %s requires output_tables Tbl, exit\n",dbsi[i].name);
			exit(1);
		}
		dbsi[i].dbi.numot = maxtbl(thresh_tbl);
		if ( dbsi[i].dbi.numot > 0 ) {
			/* allocate space for dbsi[i].dbi.ot struct */
			dbsi[i].dbi.ot = (struct output_tables *) 
				calloc(dbsi[i].dbi.numot, sizeof(struct output_tables));
		} else
		  {
			fprintf(stderr,"db2dbsubset.pf no output_tables informatioin %s, exit\n", dbsi[i].name);
			exit(1);
		}
		for (j=0; j<dbsi[i].dbi.numot; j++) {
			/* get each table name for the output table list */
			dbsi[i].dbi.ot[j].tb = (char *) gettbl (thresh_tbl, j);
			/* type event=1,origin=2,arrival=3,emodel=4,netmag=5,origerr=6 */
			/*      predarr=7, stamag=8, wfmeas=9, mapassoc=10,  */
			/*      quakeregions=11, webmaps=12 */
			dbsi[i].dbi.ot[j].type = set_table_type(i, j, dbsi);
      		} /* for loop */
		freetbl(thresh_tbl, 0);	
	}
fprintf(stderr,"Freeing dbtemplates_tbl\n");
	freetbl(dbtemplates_tbl, 0);
fprintf(stderr,"Freeing dbtemplates_arr\n");
	/* freearr(dbtemplates_arr, 0); */
fprintf(stderr,"Freeing db_templates\n");
	freearr(db_templates,0);
	return(dbsi);
}

void cp_arrival (long i1, struct ArrivalTable *tb1, int i2, struct ArrivalTable *tb2) {
	tb1[i1].arid  = (long) tb2[i2].arid;
	tb1[i1].jdate  = (long) tb2[i2].jdate;
	tb1[i1].stassid = (long) tb2[i2].stassid;
	tb1[i1].chanid  = (long) tb2[i2].chanid;
	tb1[i1].commid = (long) tb2[i2].commid;
	tb1[i1].deltim  = (double) tb2[i2].deltim;
	tb1[i1].azimuth  = (double) tb2[i2].azimuth;
	tb1[i1].delaz = (double) tb2[i2].delaz;
	tb1[i1].slow  = (double) tb2[i2].slow;
	tb1[i1].delslo = (double) tb2[i2].delslo;
	tb1[i1].ema  = (double) tb2[i2].ema;
	tb1[i1].rect = (double) tb2[i2].rect;
	tb1[i1].amp  = (double) tb2[i2].amp;
	tb1[i1].per = (double) tb2[i2].per;
	tb1[i1].logat  = (double) tb2[i2].logat;
	tb1[i1].snr = (double) tb2[i2].snr;
	tb1[i1].time  = (double) tb2[i2].time;
	(void) strcpy(tb1[i1].sta, tb2[i2].sta);
	(void) strcpy(tb1[i1].chan, tb2[i2].chan);
	(void) strcpy(tb1[i1].iphase, tb2[i2].iphase);
	(void) strcpy(tb1[i1].stype, tb2[i2].stype);
	(void) strcpy(tb1[i1].clip, tb2[i2].clip);
	(void) strcpy(tb1[i1].fm, tb2[i2].fm);
	(void) strcpy(tb1[i1].qual, tb2[i2].qual);
	(void) strcpy(tb1[i1].auth, tb2[i2].auth);
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

/* get arrival data data */
int get_arrival(Dbptr dbin, int i, long rec, struct ArrivalTable *tb ) {
	char NaN[512];
	if(dbgetv(dbin, 0, 
		"arid", &(tb[i].arid), "jdate", &(tb[i].jdate), 
		"stassid", &(tb[i].stassid),  "chanid", &(tb[i].chanid), 
		"commid", &(tb[i].commid),
		"sta", tb[i].sta, "chan", tb[i].chan, "iphase", tb[i].iphase, 
		"stype", tb[i].stype, "clip", tb[i].clip, "fm", tb[i].fm, 
		"qual", tb[i].qual, "auth", tb[i].auth,
		"deltim", &(tb[i].deltim), "azimuth", &(tb[i].azimuth), 
		"delaz", &(tb[i].delaz), "slow", &(tb[i].slow), 
		"delslo", &(tb[i].delslo), "ema", &(tb[i].ema), 
		"rect", &(tb[i].rect), "amp", &(tb[i].amp), 
		"per", &(tb[i].per), "logat", &(tb[i].logat), 
		"snr", &(tb[i].snr), "time", &(tb[i].time), 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading arrival record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].sta[6]=tb[i].chan[8]=tb[i].iphase[8]=tb[i].stype[1]=
			tb[i].clip[1]=tb[i].fm[2]=tb[i].qual[1]=tb[i].auth[15]='\0';

		/* correct database bug */
		(void) sprintf(NaN,"%f", tb[i].snr);
		if(strcmp(NaN,"NaN") == 0) tb[i].snr = -1.0; 
		if(strcmp(NaN,"-NaN") == 0) tb[i].snr = -1.0; 
		return(0);		
	}
}

/* put data */
int put_arrival(Dbptr dbout, int i, struct ArrivalTable *tb, int new, int setlddate ) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "arrival", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, "arid", tb[i].arid, "jdate", tb[i].jdate, 
		"stassid", tb[i].stassid,  "chanid", tb[i].chanid, 
		"commid", tb[i].commid,
		"sta", tb[i].sta, "chan", tb[i].chan, "iphase", tb[i].iphase, 
		"stype", tb[i].stype, "clip", tb[i].clip, "fm", tb[i].fm, 
		"qual", tb[i].qual, "auth", tb[i].auth,
		"deltim", tb[i].deltim, "azimuth", tb[i].azimuth, 
		"delaz", tb[i].delaz, "slow", tb[i].slow, 
		"delslo", tb[i].delslo, "ema", tb[i].ema, 
		"rect", tb[i].rect, "amp", tb[i].amp, 
		"per", tb[i].per, "logat", tb[i].logat, 
		"snr", tb[i].snr, "time", tb[i].time, 
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading arrival row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
/* check data */ 
int check_arrival (long i1, struct ArrivalTable *tb1, int i2, struct ArrivalTable *tb2) {
	/* correct database bug */
	if(
		(long) tb1[i1].arid  == (long) tb2[i2].arid &&
		(long) tb1[i1].jdate  == (long) tb2[i2].jdate &&
		(long) tb1[i1].stassid == (long) tb2[i2].stassid  &&
		(long) tb1[i1].chanid  == (long) tb2[i2].chanid &&
		(long) tb1[i1].commid == (long) tb2[i2].commid &&
		(double) tb1[i1].deltim  == (double) tb2[i2].deltim &&
		(double) tb1[i1].azimuth  == (double) tb2[i2].azimuth &&
		(double) tb1[i1].delaz == (double) tb2[i2].delaz &&
		(double) tb1[i1].slow  == (double) tb2[i2].slow &&
		(double) tb1[i1].delslo == (double) tb2[i2].delslo &&
		(double) tb1[i1].ema  == (double) tb2[i2].ema &&
		(double) tb1[i1].rect == (double) tb2[i2].rect &&
		(double) tb1[i1].amp  == (double) tb2[i2].amp &&
		(double) tb1[i1].per == (double) tb2[i2].per &&
		(double) tb1[i1].logat  == (double) tb2[i2].logat &&
		(double) tb1[i1].snr == (double) tb2[i2].snr &&
		(double) tb1[i1].time  == (double) tb2[i2].time &&
		strcmp(tb1[i1].sta, tb2[i2].sta) == 0 &&
		strcmp(tb1[i1].chan, tb2[i2].chan) == 0 && 
		strcmp(tb1[i1].iphase, tb2[i2].iphase) == 0 &&
		strcmp(tb1[i1].stype, tb2[i2].stype) == 0 &&
		strcmp(tb1[i1].clip, tb2[i2].clip) == 0 &&
		strcmp(tb1[i1].fm, tb2[i2].fm) == 0 &&
		strcmp(tb1[i1].qual, tb2[i2].qual) == 0 &&
		strcmp(tb1[i1].auth, tb2[i2].auth) == 0
	) {
		return(1); /* exact match */
	} else
	  {
		if(
			strcmp(tb1[i1].sta, tb2[i2].sta) == 0 &&
			(double) tb1[i1].time  == (double) tb2[i2].time ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 

struct AssocTable {
  long arid, orid, commid; 
  char sta[7], phase[9], timedef[2]; 
  char azdef[2], slodef[2], vmodel[16]; 
  double belief, delta, seaz, esaz; 
  double timeres, azres, slores, emares, wgt;
  double lddate;

  int reset;
};

int get_assoc(Dbptr dbin, int i, long rec, struct AssocTable *tb) {
	char NaN[512];
	if(dbgetv(dbin, 0, 
		"arid", &(tb[i].arid), 
		"orid", &(tb[i].orid), "commid", &(tb[i].commid), 
		"sta", tb[i].sta, "phase", tb[i].phase, 
		"timedef", tb[i].timedef, "azdef", tb[i].azdef, 
		"slodef", tb[i].slodef, "vmodel", tb[i].vmodel, 
		"belief", &(tb[i].belief), "delta", &(tb[i].delta), 
		"seaz", &(tb[i].seaz), "esaz", &(tb[i].esaz), 
		"timeres", &(tb[i].timeres), "azres", &(tb[i].azres), 
		"slores", &(tb[i].slores), "emares", &(tb[i].emares), 
		"wgt", &(tb[i].wgt), 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading assoc record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].sta[6]=tb[i].phase[8]=tb[i].timedef[1]=tb[i].azdef[1]=
			tb[i].slodef[1]=tb[i].vmodel[15]='\0';
		/* correct database bug */
		sprintf(NaN,"%f", tb[i].wgt);
		if(strcmp(NaN,"NaN") == 0) tb[i].wgt = -1.0; 
		if(strcmp(NaN,"-NaN") == 0) tb[i].wgt = -1.0; 
		return(0);		
	}
}
int put_assoc(Dbptr dbout, int i, struct AssocTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "assoc", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, "arid", tb[i].arid, 
		"orid", tb[i].orid, "commid", tb[i].commid, 
		"sta", tb[i].sta, "phase", tb[i].phase, 
		"timedef", tb[i].timedef, "azdef", tb[i].azdef, 
		"slodef", tb[i].slodef, "vmodel", tb[i].vmodel, 
		"belief", tb[i].belief, "delta", tb[i].delta, 
		"seaz", tb[i].seaz, "esaz", tb[i].esaz, 
		"timeres", tb[i].timeres, "azres", tb[i].azres, 
		"slores", tb[i].slores, "emares", tb[i].emares, 
		"wgt", tb[i].wgt, "lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading assoc row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_assoc (long i1, struct AssocTable *tb1, int i2, struct AssocTable *tb2) {

	if(
		(long) tb1[i1].arid  == (long) tb2[i2].arid &&
		(long) tb1[i1].orid == (long) tb2[i2].orid &&
		(long) tb1[i1].commid == (long) tb2[i2].commid &&
		(double) tb1[i1].belief == (double) tb2[i2].belief &&
		(double) tb1[i1].delta  == (double) tb2[i2].delta &&
		(double) tb1[i1].seaz == (double) tb2[i2].seaz &&
		(double) tb1[i1].esaz  == (double) tb2[i2].esaz &&
		(double) tb1[i1].timeres == (double) tb2[i2].timeres &&
		(double) tb1[i1].azres  == (double) tb2[i2].azres &&
		(double) tb1[i1].slores == (double) tb2[i2].slores &&
		(double) tb1[i1].emares  == (double) tb2[i2].emares  &&
		(double) tb1[i1].wgt  == (double) tb2[i2].wgt && 
		strcmp(tb1[i1].sta,tb2[i2].sta) == 0  &&
		strcmp(tb1[i1].phase,tb2[i2].phase) ==  0 &&
		strcmp(tb1[i1].timedef,tb2[i2].timedef) == 0  &&
		strcmp(tb1[i1].azdef,tb2[i2].azdef) ==  0 &&
		strcmp(tb1[i1].slodef,tb2[i2].slodef) ==  0 &&
		strcmp(tb1[i1].vmodel,tb2[i2].vmodel) ==  0 
	) {
		return(1); /* exact match */
	} else
	  {
		if(
			(long) tb1[i1].arid  == (long) tb2[i2].arid &&
			(long) tb1[i1].orid == (long) tb2[i2].orid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_assoc (long i1, struct AssocTable *tb1, int i2, struct AssocTable *tb2) {
	tb1[i1].arid  = (long) tb2[i2].arid;
	tb1[i1].orid = (long) tb2[i2].orid;
	tb1[i1].commid = (long) tb2[i2].commid;
	tb1[i1].belief = (double) tb2[i2].belief;
	tb1[i1].delta  = (double) tb2[i2].delta;
	tb1[i1].seaz = (double) tb2[i2].seaz;
	tb1[i1].esaz  = (double) tb2[i2].esaz;
	tb1[i1].timeres = (double) tb2[i2].timeres;
	tb1[i1].azres  = (double) tb2[i2].azres;
	tb1[i1].slores = (double) tb2[i2].slores;
	tb1[i1].emares  = (double) tb2[i2].emares;
	tb1[i1].wgt  = (double) tb2[i2].wgt;
	(void) strcpy(tb1[i1].sta,tb2[i2].sta);
	(void) strcpy(tb1[i1].phase,tb2[i2].phase);
	(void) strcpy(tb1[i1].timedef,tb2[i2].timedef);
	(void) strcpy(tb1[i1].azdef,tb2[i2].azdef);
	(void) strcpy(tb1[i1].slodef,tb2[i2].slodef);
	(void) strcpy(tb1[i1].vmodel,tb2[i2].vmodel); 
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct EmodelTable {
  long orid; 
  double emodelx, emodely, emodelz, emodelt, lddate;

  int reset;
};

int get_emodel(Dbptr dbin, int i, long rec, struct EmodelTable *tb) {
	if(dbgetv(dbin, 0, 
		"orid", &(tb[i].orid), "emodelx", &(tb[i].emodelx), 
		"emodely", &(tb[i].emodely), "emodelz", &(tb[i].emodelz), 
		"emodelt", &(tb[i].emodelt), 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading emodel record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int put_emodel(Dbptr dbout, int i, struct EmodelTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "emodel", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"orid", tb[i].orid, "emodelx", tb[i].emodelx, 
		"emodely", tb[i].emodely, "emodelz", tb[i].emodelz, 
		"emodelt", tb[i].emodelt, 
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading emodel row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_emodel (long i1, struct EmodelTable *tb1, int i2, struct EmodelTable *tb2) {
	if(
		(long) tb1[i1].orid == (long) tb2[i2].orid && 
		(double) tb1[i1].emodelx == (double) tb2[i2].emodelx && 
		(double) tb1[i1].emodely == (double) tb2[i2].emodely &&
		(double) tb1[i1].emodelz == (double) tb2[i2].emodelz &&
		(double) tb1[i1].emodelt == (double) tb2[i2].emodelt 
	) {
		return(1); /* exact match */
	} else
	  {
		if(
			(long) tb1[i1].orid == (long) tb2[i2].orid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_emodel (long i1, struct EmodelTable *tb1, int i2, struct EmodelTable *tb2) {
	tb1[i1].orid = (long) tb2[i2].orid;
	tb1[i1].emodelx = (double) tb2[i2].emodelx; 
	tb1[i1].emodely = (double) tb2[i2].emodely;
	tb1[i1].emodelz = (double) tb2[i2].emodelz;
	tb1[i1].emodelt = (double) tb2[i2].emodelt;
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct EventTable {
  long evid, prefor, commid; 
  char evname[16], auth[16]; 
  double lddate;

  int reset;
};

int get_event(Dbptr dbin, int i, long rec, struct EventTable *tb) {
	if(dbgetv(dbin, 0,
		"evid", &(tb[i].evid), "prefor", &(tb[i].prefor), 
		"commid", &(tb[i].commid), "evname", tb[i].evname, 
		"auth", tb[i].auth, 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading event record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].evname[15]=tb[i].auth[15]='\0';
		return(0);		
	}
}
int put_event(Dbptr dbout, int i, struct EventTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "event", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"evid", tb[i].evid, "prefor", tb[i].prefor, 
		"commid", tb[i].commid, "evname", tb[i].evname, 
		"auth", tb[i].auth, 
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading event row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_event (long i1, struct EventTable *tb1, int i2, struct EventTable *tb2) {
	if(
		(long) tb1[i1].evid == (long) tb2[i2].evid &&
		(long) tb1[i1].prefor == (long) tb2[i2].prefor &&
		(long) tb1[i1].commid == (long) tb2[i2].commid &&
		strcmp(tb1[i1].evname,tb2[i2].evname) == 0 && 
		strcmp(tb1[i1].auth, tb2[i2].auth) == 0
	) {
		return(1); /* exact match */
	} else
	  {
		if( (long) tb1[i1].evid == (long) tb2[i2].evid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
}
void cp_event (long i1, struct EventTable *tb1, long i2, struct EventTable *tb2) {
fprintf (stderr,"debug cp_event 1\n");
	tb1[i1].evid = (long) tb2[i2].evid;
fprintf (stderr,"debug cp_event 2\n");
	tb1[i1].prefor = (long) tb2[i2].prefor;
fprintf (stderr,"debug cp_event 3\n");
	tb1[i1].commid = (long) tb2[i2].commid;
fprintf (stderr,"debug cp_event 4\n");
	(void) strcpy(tb1[i1].evname,tb2[i2].evname); 
fprintf (stderr,"debug cp_event 5\n");
	(void) strcpy(tb1[i1].auth, tb2[i2].auth);
fprintf (stderr,"debug cp_event 6\n");
	tb1[i1].lddate  = (double) tb2[i2].lddate;
fprintf (stderr,"debug cp_event 7\n");
}

struct NetmagTable {
  long magid, orid, evid, commid, nsta; 
  char net[9], magtype[7], auth[16]; 
  double magnitude, uncertainty; 
  double lddate;

  int reset;
};

int get_netmag(Dbptr dbin, int i, long rec, struct NetmagTable *tb) {
	if(dbgetv(dbin, 0, 
		"magid", &(tb[i].magid), "orid", &(tb[i].orid), 
		"evid", &(tb[i].evid), "commid", &(tb[i].commid), 
		"nsta", &(tb[i].nsta), "net", tb[i].net, 
		"magtype", tb[i].magtype, "auth", tb[i].auth, 
		"magnitude", &(tb[i].magnitude),
		"uncertainty", &(tb[i].uncertainty),
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading netmag record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].net[8]=tb[i].magtype[6]=tb[i].auth[15]='\0';
		return(0);		
	}
}
int put_netmag(Dbptr dbout, int i, struct NetmagTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "netmag", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"magid", tb[i].magid, "orid", tb[i].orid, 
		"evid", tb[i].evid, "commid", tb[i].commid, 
		"nsta", tb[i].nsta, "net", tb[i].net, 
		"magtype", tb[i].magtype, "auth", tb[i].auth, 
		"magnitude", tb[i].magnitude,
		"uncertainty", tb[i].uncertainty,
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading netmag row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_netmag (long i1, struct NetmagTable *tb1, int i2, struct NetmagTable *tb2) {
	if(
		(long) tb1[i1].magid == (long) tb2[i2].magid &&
		(long) tb1[i1].orid == (long) tb2[i2].orid &&
		(long) tb1[i1].evid == (long) tb2[i2].evid &&
		(long) tb1[i1].commid == (long) tb2[i2].commid &&
		(long) tb1[i1].nsta == (long) tb2[i2].nsta &&
		(double) tb1[i1].magnitude == (double) tb2[i2].magnitude &&
		(double) tb1[i1].uncertainty == (double) tb2[i2].uncertainty &&
		strcmp(tb1[i1].net,tb2[i2].net) == 0 &&
		strcmp(tb1[i1].magtype,tb2[i2].magtype) == 0 &&
		strcmp(tb1[i1].auth,tb2[i2].auth) == 0 
	) {
		return(1); /* exact match */
	} else
	  {
		if( (long) tb1[i1].magid == (long) tb2[i2].magid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_netmag (long i1, struct NetmagTable *tb1, long i2, struct NetmagTable *tb2) {
	tb1[i1].magid = (long) tb2[i2].magid;
	tb1[i1].orid = (long) tb2[i2].orid;
	tb1[i1].evid = (long) tb2[i2].evid;
	tb1[i1].commid = (long) tb2[i2].commid;
	tb1[i1].nsta = (long) tb2[i2].nsta;
	tb1[i1].magnitude = (double) tb2[i2].magnitude;
	tb1[i1].uncertainty = (double) tb2[i2].uncertainty;
	(void) strcpy(tb1[i1].net,tb2[i2].net);
	(void) strcpy(tb1[i1].magtype,tb2[i2].magtype);
	(void) strcpy(tb1[i1].auth,tb2[i2].auth); 
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct OrigerrTable {
  long orid, commid; 
  double sdobs, smajax, sminax; 
  double strike, sdepth, stime, conf; 
  double sxx, syy, szz, stt, sxy; 
  double sxz, syz, stx, sty, stz, lddate;

  int reset;
};
int get_origerr(Dbptr dbin, int i, long rec, struct OrigerrTable *tb) {
	if(dbgetv(dbin, 0, 
		"orid", &(tb[i].orid), "commid", &(tb[i].commid), 
		"sdobs", &(tb[i].sdobs), "smajax", &(tb[i].smajax), 
		"sminax", &(tb[i].sminax), "strike", &(tb[i].strike), 
		"sdepth", &(tb[i].sdepth), "stime", &(tb[i].stime), 
		"conf", &(tb[i].conf), "sxx", &(tb[i].sxx), 
		"syy", &(tb[i].syy), "szz", &(tb[i].szz), 
		"stt", &(tb[i].stt), "sxy", &(tb[i].sxy), 
		"sxz", &(tb[i].sxz), "syz", &(tb[i].syz), 
		"stx", &(tb[i].stx), "sty", &(tb[i].sty), 
		"stz", &(tb[i].stz),
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv origerr record=%ld, orid=%ld into index %d\n", rec, tb[i].orid, i);	
               if(tb[i].stime > 99999.99) {
                        tb[i].stime = (double) -1.0;
                }
                return(0); /* presume error is not real bad */
                /* return (-1); */
	} else
	  {
		/* correct value for input db that is out of range */
		if(tb[i].stime > 99999.99) {
			tb[i].stime = (double) -1.0;
		}
		return(0);		
	}
}
int put_origerr(Dbptr dbout, int i, struct OrigerrTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "origerr", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"orid", tb[i].orid, "commid", tb[i].commid, 
		"sdobs", tb[i].sdobs, "smajax", tb[i].smajax, 
		"sminax", tb[i].sminax, "strike", tb[i].strike, 
		"sdepth", tb[i].sdepth, "stime", tb[i].stime, 
		"conf", tb[i].conf, "sxx", tb[i].sxx, 
		"syy", tb[i].syy, "szz", tb[i].szz, 
		"stt", tb[i].stt, "sxy", tb[i].sxy, 
		"sxz", tb[i].sxz, "syz", tb[i].syz, 
		"stx", tb[i].stx, "sty", tb[i].sty, 
		"stz", tb[i].stz,
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			/* ignore error, for now */
			/* complain(0,"dbputv problem loading origerr row at index %d\n", i); */
		return(0); /* presume error is not real bad */
                /* return (-1); */
	} else
	  {
		return(0);		
	}
}
int check_origerr (long i1, struct OrigerrTable *tb1, int i2, struct OrigerrTable *tb2) {
	if(
		(long) tb1[i1].orid == (long) tb2[i2].orid &&
		(long) tb1[i1].commid == (long) tb2[i2].commid && 
		(double) tb1[i1].sdobs == (double) tb2[i2].sdobs &&
		(double) tb1[i1].smajax == (double) tb2[i2].smajax &&
		(double) tb1[i1].sminax == (double) tb2[i2].sminax &&
		(double) tb1[i1].strike == (double) tb2[i2].strike && 
		(double) tb1[i1].sdepth == (double) tb2[i2].sdepth &&
		(double) tb1[i1].stime == (double) tb2[i2].stime && 
		(double) tb1[i1].conf == (double) tb2[i2].conf &&
		(double) tb1[i1].sxx == (double) tb2[i2].sxx &&
		(double) tb1[i1].syy == (double) tb2[i2].syy &&
		(double) tb1[i1].szz == (double) tb2[i2].szz && 
		(double) tb1[i1].stt == (double) tb2[i2].stt &&
		(double) tb1[i1].sxy == (double) tb2[i2].sxy && 
		(double) tb1[i1].sxz == (double) tb2[i2].sxz &&
		(double) tb1[i1].syz == (double) tb2[i2].syz && 
		(double) tb1[i1].stx == (double) tb2[i2].stx &&
		(double) tb1[i1].sty == (double) tb2[i2].sty && 
		(double) tb1[i1].stz == (double) tb2[i2].stz
	) {
		return(1); /* exact match */
	} else
	  {
		if( tb1[i1].orid == tb2[i2].orid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_origerr (long i1, struct OrigerrTable *tb1,long  i2, struct OrigerrTable *tb2) {
	tb1[i1].orid = (long) tb2[i2].orid;
	tb1[i1].commid = (long) tb2[i2].commid; 
	tb1[i1].sdobs = (double) tb2[i2].sdobs;
	tb1[i1].smajax = (double) tb2[i2].smajax;
	tb1[i1].sminax = (double) tb2[i2].sminax;
	tb1[i1].strike = (double) tb2[i2].strike; 
	tb1[i1].sdepth = (double) tb2[i2].sdepth;
	tb1[i1].stime = (double) tb2[i2].stime;
	tb1[i1].conf = (double) tb2[i2].conf;
	tb1[i1].sxx = (double) tb2[i2].sxx;
	tb1[i1].syy = (double) tb2[i2].syy;
	tb1[i1].szz = (double) tb2[i2].szz; 
	tb1[i1].stt = (double) tb2[i2].stt;
	tb1[i1].sxy = (double) tb2[i2].sxy; 
	tb1[i1].sxz = (double) tb2[i2].sxz;
	tb1[i1].syz = (double) tb2[i2].syz; 
	tb1[i1].stx = (double) tb2[i2].stx;
	tb1[i1].sty = (double) tb2[i2].sty;
	tb1[i1].stz = (double) tb2[i2].stz;
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct OriginTable {
  double lat, lon, depth, depdp, mb, ms, ml; 
  long orid, evid, jdate; 
  long ndp, nass; 
  long ndef; 
  long grn, srn, mbid, msid, mlid, commid; 
  char etype[3], review[4], dtype[2], algorithm[16], auth[16]; 
  double time, lddate;

  int reset;
};

int get_origin(Dbptr dbin, int i, long rec, struct OriginTable *tb) {
	if(dbgetv(dbin, 0, 
		"lat", &(tb[i].lat), "lon", &(tb[i].lon), 
		"depth", &(tb[i].depth), "depdp", &(tb[i].depdp), 
		"mb", &(tb[i].mb), "ms", &(tb[i].ms), "ml", &(tb[i].ml), 
		"orid", &(tb[i].orid), "evid", &(tb[i].evid), 
		"jdate", &(tb[i].jdate), "nass", &(tb[i].nass), 
		"ndef", &(tb[i].ndef), "ndp", &(tb[i].ndp), 
		"grn", &(tb[i].grn), "srn", &(tb[i].srn), 
		"mbid", &(tb[i].mbid), "msid", &(tb[i].msid), 
		"mlid", &(tb[i].mlid), "commid", &(tb[i].commid), 
		"etype", tb[i].etype, "review", tb[i].review, 
		"dtype", tb[i].dtype, "algorithm", tb[i].algorithm, 
		"auth", tb[i].auth, "time", &(tb[i].time),
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading origin record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].etype[2]=tb[i].review[3]=tb[i].dtype[1]=tb[i].algorithm[15]=tb[i].auth[15]='\0';
		return(0);		
	}
}
int put_origin(Dbptr dbout, int i, struct OriginTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate =  (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "origin", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if((double)tb[i].mb > 0.0 || (double)tb[i].ml > 0.0 || (double)tb[i].ms > 0.0) {
		magfound = 1;
	}
/* fprintf(stderr,	"lat=%f\n", tb[i].lat); 
* fprintf(stderr,	"lon=%f\n", tb[i].lon); 
* fprintf(stderr,	"depth=%f\n", tb[i].depth); 
* fprintf(stderr,	"depdp=%f\n", tb[i].depdp); 
* fprintf(stderr,	"mb=%f\n", tb[i].mb); 
* fprintf(stderr,	"ms=%f\n", tb[i].ms); 
* fprintf(stderr,	"ml=%f\n", tb[i].ml); 
* fprintf(stderr,	"orid=%ld\n", tb[i].orid); 
* fprintf(stderr,	"evid=%ld\n", tb[i].evid); 
* fprintf(stderr,	"jdate=%ld\n", tb[i].jdate); 
* fprintf(stderr,	"nass=%ld\n", tb[i].nass); 
* fprintf(stderr,	"ndef=%ld\n", tb[i].ndef);
* fprintf(stderr,	"ndp=%ld\n", tb[i].ndp);
* fprintf(stderr,	"grn=%ld\n", tb[i].grn); 
* fprintf(stderr,	"srn=%ld\n", tb[i].srn); 
* fprintf(stderr,	"mbid=%ld\n", tb[i].mbid); 
* fprintf(stderr,	"msid=%ld\n", tb[i].msid); 
* fprintf(stderr,	"mlid=%ld\n", tb[i].mlid); 
* fprintf(stderr,	"commid=%ld\n", tb[i].commid); 
* fprintf(stderr,	"etype=%s\n", tb[i].etype); 
* fprintf(stderr,	"review=%s\n", tb[i].review); 
* fprintf(stderr,	"dtype=%s\n", tb[i].dtype); 
* fprintf(stderr,	"algorithm=%s\n", tb[i].algorithm); 
* fprintf(stderr,	"auth=%s\n", tb[i].auth); 
* fprintf(stderr,	"time=%f\n", tb[i].time);
* fprintf(stderr,	"lddate=%f\n", tb[i].lddate);
*/
 
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"lat",  tb[i].lat, 
		"lon",  tb[i].lon, 
		"depth",  tb[i].depth, 
		"depdp",  tb[i].depdp, 
		"mb",  tb[i].mb, 
		"ms",  tb[i].ms, 
		"ml",  tb[i].ml, 
		"orid",  tb[i].orid, 
		"evid",  tb[i].evid, 
		"jdate", tb[i].jdate, 
		"nass",  tb[i].nass, 
		"ndef",  tb[i].ndef,
		"ndp",  tb[i].ndp, 
		"grn",  tb[i].grn, 
		"srn",  tb[i].srn, 
		"mbid",  tb[i].mbid, 
		"msid",  tb[i].msid, 
		"mlid",  tb[i].mlid, 
		"commid",  tb[i].commid, 
		"etype",  tb[i].etype, 
		"review",  tb[i].review, 
		"dtype",  tb[i].dtype, 
		"algorithm", tb[i].algorithm, 
		"auth", tb[i].auth, 
		"time",  tb[i].time,
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
		/* "lddate", tb[i].lddate, 0) == dbINVALID) { */
			complain(0,"dbputv problem loading origin row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_origin (long i1, struct OriginTable *tb1, int i2, struct OriginTable *tb2) {
	if(
		(double) tb1[i1].lat == (double) tb2[i2].lat &&
		(double) tb1[i1].lon == (double) tb2[i2].lon && 
		(double) tb1[i1].depth == (double) tb2[i2].depth &&
		(double) tb1[i1].depdp == (double) tb2[i2].depdp &&
		(double) tb1[i1].mb == (double) tb2[i2].mb &&
		(double) tb1[i1].ms == (double) tb2[i2].ms && 
		(double) tb1[i1].ml == (double) tb2[i2].ml && 
		(long) tb1[i1].orid == (long) tb2[i2].orid &&
		(long) tb1[i1].evid == (long) tb2[i2].evid && 
		(long) tb1[i1].jdate == (long) tb2[i2].jdate &&
		(long) tb1[i1].nass == (long) tb2[i2].nass && 
		(long) tb1[i1].ndef == (long) tb2[i2].ndef &&
		(long) tb1[i1].ndp == (long) tb2[i2].ndp && 
		(long) tb1[i1].grn == (long) tb2[i2].grn &&
		(long) tb1[i1].srn == (long) tb2[i2].srn &&
		(long) tb1[i1].mbid == (long) tb2[i2].mbid &&
		(long) tb1[i1].msid == (long) tb2[i2].msid &&
		(long) tb1[i1].mlid == (long) tb2[i2].mlid &&
		(long) tb1[i1].commid == (long) tb2[i2].commid &&
		(double) tb1[i1].time == (double) tb2[i2].time &&
		strcmp(tb1[i1].etype, tb2[i2].etype) == 0 &&
		strcmp(tb1[i1].review, tb2[i2].review) == 0 && 
		strcmp(tb1[i1].dtype, tb2[i2].dtype) == 0 && 
		strcmp(tb1[i1].algorithm, tb2[i2].algorithm) == 0 && 
		strcmp(tb1[i1].auth, tb2[i2].auth) == 0 
	) {
		return(1); /* exact match */
	} else
	  {
		if(	/* primary keys */	
			(double) tb1[i1].lat == (double) tb2[i2].lat &&
			(double) tb1[i1].lon == (double) tb2[i2].lon &&
			(double) tb1[i1].depth == (double) tb2[i2].depth &&
			(double) tb1[i1].time == (double) tb2[i2].time &&
			(long) tb1[i1].nass == (long) tb2[i2].nass && 
			(long) tb1[i1].ndef == (long) tb2[i2].ndef &&
			/* additional keys to check */
			(double) tb1[i1].mb == (double) tb2[i2].mb &&
			(double) tb1[i1].ms == (double) tb2[i2].ms && 
			(double) tb1[i1].ml == (double) tb2[i2].ml 
			) { 
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_origin (long i1, struct OriginTable *tb1, long i2, struct OriginTable *tb2) {
	tb1[i1].lat = (double) tb2[i2].lat;
	tb1[i1].lon = (double) tb2[i2].lon; 
	tb1[i1].depth = (double) tb2[i2].depth;
	tb1[i1].depdp = (double) tb2[i2].depdp;
	tb1[i1].mb = (double) tb2[i2].mb;
	tb1[i1].ms = (double) tb2[i2].ms; 
	tb1[i1].ml = (double) tb2[i2].ml; 
	tb1[i1].orid = (long) tb2[i2].orid;
	tb1[i1].evid = (long) tb2[i2].evid; 
	tb1[i1].jdate = (long) tb2[i2].jdate;
	tb1[i1].nass = (long) tb2[i2].nass; 
	tb1[i1].ndef = (long) tb2[i2].ndef;
	tb1[i1].ndp = (long) tb2[i2].ndp; 
	tb1[i1].grn = (long) tb2[i2].grn;
	tb1[i1].srn = (long) tb2[i2].srn;
	tb1[i1].mbid = (long) tb2[i2].mbid;
	tb1[i1].msid = (long) tb2[i2].msid;
	tb1[i1].mlid = (long) tb2[i2].mlid;
	tb1[i1].commid = (long) tb2[i2].commid;
	tb1[i1].time = (double) tb2[i2].time;
	(void) strcpy(tb1[i1].etype, tb2[i2].etype);
	(void) strcpy(tb1[i1].review, tb2[i2].review); 
	(void) strcpy(tb1[i1].dtype, tb2[i2].dtype); 
	(void) strcpy(tb1[i1].algorithm, tb2[i2].algorithm); 
	(void) strcpy(tb1[i1].auth, tb2[i2].auth); 
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct PredarrTable {
  long arid, orid; 
  double slow, seaz, ema, esaz, dip; 
  double time, lddate;

  int reset;
};

int get_predarr(Dbptr dbin, int i, long rec, struct PredarrTable *tb) {
	char NaN[512];
	if(dbgetv(dbin, 0, 
		"arid", &(tb[i].arid), "orid", &(tb[i].orid), 
		"slow", &(tb[i].slow), "seaz", &(tb[i].seaz), 
		"ema", &(tb[i].ema), "esaz", &(tb[i].esaz), 
		"dip", &(tb[i].dip), "time", &(tb[i].time),
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading predarr record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		/* correct database bug */
		sprintf(NaN,"%f", tb[i].ema);
		if(strcmp(NaN,"NaN") == 0) tb[i].ema = -1.0; 
		if(strcmp(NaN,"-NaN") == 0) tb[i].ema = -1.0; 
		return(0);		
	}
}
int put_predarr(Dbptr dbout, int i, struct PredarrTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "predarr", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"arid", tb[i].arid, "orid", tb[i].orid, 
		"slow", tb[i].slow, "seaz", tb[i].seaz, 
		"ema", tb[i].ema, "esaz", tb[i].esaz, 
		"dip", tb[i].dip, "time", tb[i].time,
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading predarr row at index %d, orid=%ld\n", i, tb[i].orid);
			exit(1);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_predarr (long i1, struct PredarrTable *tb1, int i2, struct PredarrTable *tb2) {
	if(
		(long) tb1[i1].arid == (long) tb2[i2].arid &&
		(long) tb1[i1].orid == (long) tb2[i2].orid && 
		(double) tb1[i1].slow == (double) tb2[i2].slow &&
		(double) tb1[i1].seaz == (double) tb2[i2].seaz &&
		(double) tb1[i1].ema == (double) tb2[i2].ema &&
		(double) tb1[i1].esaz == (double) tb2[i2].esaz && 
		(double) tb1[i1].dip == (double) tb2[i2].dip &&
		(double) tb1[i1].time == (double) tb2[i2].time
	) {
		return(1); /* exact match */
	} else
	  {
		if( (long) tb1[i1].arid == (long) tb2[i2].arid &&
		    (long) tb1[i1].orid == (long) tb2[i2].orid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_predarr (long i1, struct PredarrTable *tb1, long i2, struct PredarrTable *tb2) {
	tb1[i1].arid = (long) tb2[i2].arid;
	tb1[i1].orid = (long) tb2[i2].orid; 
	tb1[i1].slow = (double) tb2[i2].slow;
	tb1[i1].seaz = (double) tb2[i2].seaz; 
	tb1[i1].ema = (double) tb2[i2].ema;
	tb1[i1].esaz = (double) tb2[i2].esaz; 
	tb1[i1].dip = (double) tb2[i2].dip;
	tb1[i1].time = (double) tb2[i2].time;
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct StamagTable {
  long magid, arid, orid, evid, commid; 
  char sta[7], phase[9], magtype[7], auth[16]; 
  double magnitude, uncertainty; 
  double lddate;

  int reset;
};
int get_stamag(Dbptr dbin, int i, long rec, struct StamagTable *tb) {
	if(dbgetv(dbin, 0, 
		"magid", &(tb[i].magid), "arid", &(tb[i].arid), 
		"orid", &(tb[i].orid), "evid", &(tb[i].evid), 
		"commid", &(tb[i].commid), "sta", tb[i].sta, 
		"phase", tb[i].phase, "magtype", tb[i].magtype, 
		"auth", tb[i].auth, "magnitude", &(tb[i].magnitude), 
		"uncertainty", &(tb[i].uncertainty),
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading stamag record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].sta[6]=tb[i].phase[8]=tb[i].magtype[6]=tb[i].auth[15]='\0';
		return(0);		
	}
}
int put_stamag(Dbptr dbout, int i, struct StamagTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "stamag", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"magid", tb[i].magid, "arid", tb[i].arid, 
		"orid", tb[i].orid, "evid", tb[i].evid, 
		"commid", tb[i].commid, "sta", tb[i].sta, 
		"phase", tb[i].phase, "magtype", tb[i].magtype, 
		"auth", tb[i].auth, "magnitude", tb[i].magnitude, 
		"uncertainty", tb[i].uncertainty,
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading stamag row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_stamag (long i1, struct StamagTable *tb1, int i2, struct StamagTable *tb2) {
	if(
		(long) tb1[i1].magid == (long) tb2[i2].magid &&
		(long) tb1[i1].arid == (long) tb2[i2].arid &&
		(long) tb1[i1].orid == (long) tb2[i2].orid &&
		(long) tb1[i1].evid == (long) tb2[i2].evid && 
		(long) tb1[i1].commid == (long) tb2[i2].commid &&
		(double) tb1[i1].magnitude == (double) tb2[i2].magnitude &&
		(double) tb1[i1].uncertainty == (double) tb2[i2].uncertainty &&
		strcmp(tb1[i1].sta, tb2[i2].sta) == 0 && 
		strcmp(tb1[i1].phase, tb2[i2].phase) == 0 &&
		strcmp(tb1[i1].magtype, tb2[i2].magtype) == 0 && 
		strcmp(tb1[i1].auth, tb2[i2].auth) == 0  
	) {
		return(1); /* exact match */
	} else
	  {
		if(
			(long) tb1[i1].magid == (long) tb2[i2].magid &&
			strcmp(tb1[i1].magtype, tb2[i2].magtype) == 0 &&
			strcmp(tb1[i1].sta, tb2[i2].sta) == 0 &&
			(long) tb1[i1].orid == (long) tb2[i2].orid ) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
} 
void cp_stamag (long i1, struct StamagTable *tb1, long i2, struct StamagTable *tb2) {
	tb1[i1].magid = (long) tb2[i2].magid;
	tb1[i1].arid = (long) tb2[i2].arid;
	tb1[i1].orid = (long) tb2[i2].orid;
	tb1[i1].evid = (long) tb2[i2].evid; 
	tb1[i1].commid = (long) tb2[i2].commid;
	tb1[i1].magnitude = (double) tb2[i2].magnitude;
	tb1[i1].uncertainty = (double) tb2[i2].uncertainty;
	(void) strcpy(tb1[i1].sta, tb2[i2].sta); 
	(void) strcpy(tb1[i1].phase, tb2[i2].phase);
	(void) strcpy(tb1[i1].magtype, tb2[i2].magtype);
	(void) strcpy(tb1[i1].auth, tb2[i2].auth);
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

struct WfmeasTable {
  char sta[7], chan[9], meastype[11], filter[31]; 
  char units1[13], units2[13], auth[16]; 
  double time, endtime, tmeas, lddate;
  double twin, val1, val2; 
  long arid; 

  int reset;
};
int get_wfmeas(Dbptr dbin, int i, long rec, struct WfmeasTable *tb) {
	if(dbgetv(dbin, 0, 
		"sta", tb[i].sta, "chan", tb[i].chan, 
		"meastype", tb[i].meastype, "filter", tb[i].filter, 
		"units1", tb[i].units1, "units2", tb[i].units2, 
		"auth", tb[i].auth, "time", &(tb[i].time), 
		"endtime", &(tb[i].endtime), "tmeas", &(tb[i].tmeas), 
		"twin", &(tb[i].twin), "val1", &(tb[i].val1), 
		"val2", &(tb[i].val2), "arid", &(tb[i].arid),		
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading wfmeas record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].sta[6]=tb[i].chan[8]=tb[i].meastype[10]=tb[i].filter[30]=
			tb[i].units1[12]=tb[i].units2[12]=tb[i].auth[15]='\0';
		return(0);		
	}
}
int put_wfmeas(Dbptr dbout, int i, struct WfmeasTable *tb, int new, int setlddate) {
	/* long nrec; */
	/* reset load date of new or update rows */
	if(setlddate == 1) { 
		tb[i].lddate = (double) now ();
	}
	if( new == 1) { /* new row , new == 0 means update current record */
		dbout = dblookup( dbout, 0, "wfmeas", 0, 0 );
		dbout.record = dbaddnull (dbout);
		/* dbquery (dbout, dbRECORD_COUNT, &nrec); */
	}
	if(dbout.record < 0 || dbputv(dbout, 0, 
		"sta", tb[i].sta, "chan", tb[i].chan, 
		"meastype", tb[i].meastype, "filter", tb[i].filter, 
		"units1", tb[i].units1, "units2", tb[i].units2, 
		"auth", tb[i].auth, "time", tb[i].time, 
		"endtime", tb[i].endtime, "tmeas", tb[i].tmeas, 
		"twin", tb[i].twin, "val1", tb[i].val1, 
		"val2", tb[i].val2, "arid", tb[i].arid,		
		"lddate", tb[i].lddate, NULL) == dbINVALID) {
			complain(0,"dbputv problem loading wfmeas row at index %d\n", i);
		return (-1);
	} else
	  {
		return(0);		
	}
}
int check_wfmeas (long i1, struct WfmeasTable *tb1, int i2, struct WfmeasTable *tb2) {
	if(
		(double) tb1[i1].time == (double) tb2[i2].time && 
		(double) tb1[i1].endtime == (double) tb2[i2].endtime &&
		(double) tb1[i1].tmeas == (double) tb2[i2].tmeas &&
		(double) tb1[i1].twin == (double) tb2[i2].twin &&
		(double) tb1[i1].val1 == (double) tb2[i2].val1 && 
		(double) tb1[i1].val2 == (double) tb2[i2].val2 &&
		(long) tb1[i1].arid == (long) tb2[i2].arid && 
		strcmp(tb1[i1].sta, tb2[i2].sta) == 0 &&
		strcmp(tb1[i1].chan, tb2[i2].chan) == 0 && 
		strcmp(tb1[i1].meastype, tb2[i2].meastype) == 0 &&
		strcmp(tb1[i1].filter, tb2[i2].filter) == 0 && 
		strcmp(tb1[i1].units1, tb2[i2].units1) == 0 &&
		strcmp(tb1[i1].units2, tb2[i2].units2) == 0 && 
		strcmp(tb1[i1].auth, tb2[i2].auth) == 0 
	) {
		return(1); /* exact match */
	} else
	  {
		if( 
			strcmp(tb1[i1].sta, tb2[i2].sta) == 0 &&
			strcmp(tb1[i1].chan, tb2[i2].chan) == 0 &&
			strcmp(tb1[i1].meastype, tb2[i2].meastype) == 0 &&
			strcmp(tb1[i1].filter, tb2[i2].filter) == 0 &&
			(double) tb1[i1].time == (double) tb2[i2].time &&
			(double) tb1[i1].endtime == (double) tb2[i2].endtime) {
			return(2); /* primary keys match update row */
		} else
		  {
			return(3); /* no match */
		}
	}
}

void cp_wfmeas (long i1, struct WfmeasTable *tb1, long i2, struct WfmeasTable *tb2) {
	tb1[i1].time = (double) tb2[i2].time;
	tb1[i1].endtime = (double) tb2[i2].endtime;
	tb1[i1].tmeas = (double) tb2[i2].tmeas;
	tb1[i1].twin = (double) tb2[i2].twin;
	tb1[i1].val1 = (double) tb2[i2].val1; 
	tb1[i1].val2 = (double) tb2[i2].val2;
	tb1[i1].arid = (long) tb2[i2].arid; 
	(void) strcpy(tb1[i1].sta, tb2[i2].sta);
	(void) strcpy(tb1[i1].chan, tb2[i2].chan); 
	(void) strcpy(tb1[i1].meastype, tb2[i2].meastype);
	(void) strcpy(tb1[i1].filter, tb2[i2].filter); 
	(void) strcpy(tb1[i1].units1, tb2[i2].units1);
	(void) strcpy(tb1[i1].units2, tb2[i2].units2); 
	(void) strcpy(tb1[i1].auth, tb2[i2].auth); 
	tb1[i1].lddate  = (double) tb2[i2].lddate;
}

/* dbrecenteqs1.1 additional tables mapassoc, quakeregions, webmaps */
/* These tables will only be read in a matched against the orid */
struct MapassocTable {
  char mapname[21], symshape[16], symcolor[16];
  long orid, x, y, symsize; 
  double lddate;

  int reset;
};

int get_mapassoc(Dbptr dbin, int i, long rec, struct MapassocTable *tb) {
	if(dbgetv(dbin, 0, 
		"mapname", tb[i].mapname,
		"symshape", tb[i].symshape,
		"symcolor", tb[i].symcolor,
		"orid", &(tb[i].orid), 
		"x", &(tb[i].x), 
		"y", &(tb[i].y), 
		"symsize", &(tb[i].symsize), 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading mapassoc record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].mapname[20]=tb[i].symshape[15]=tb[i].symcolor[15]='\0';
		return(0);		
	}
}

struct QuakeregionsTable {
  long orid; 
  char regname[81];
  double lddate;

  int reset; 
};

int get_quakeregions(Dbptr dbin, int i, long rec, struct QuakeregionsTable *tb) {
	if(dbgetv(dbin, 0, 
		"regname", tb[i].regname, 
		"orid", &(tb[i].orid), 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading quakeregions record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].regname[80]='\0';
		return(0);		
	}
}

struct WebmapsTable {
  char mapname[21], dir[65], dfile[33], url[101];
  long evid, orid;
  double lddate;

  int reset;
};

int get_webmaps(Dbptr dbin, int i, long rec, struct WebmapsTable *tb) {
	if(dbgetv(dbin, 0, 
		"mapname", tb[i].mapname, 
		"dir", tb[i].dir, 
		"dfile", tb[i].dfile, 
		"url", tb[i].url, 
		"evid", &(tb[i].evid), 
		"orid", &(tb[i].orid), 
		"lddate", &(tb[i].lddate), NULL) == dbINVALID) {
			complain(0,"dbgetv problem loading webmaps record=%ld into index %d\n", rec, i);
		return (-1);
	} else
	  {
		tb[i].mapname[20]=tb[i].dir[64]=tb[i].dfile[32]=tb[i].url[100]='\0';
		return(0);		
	}
}

int database_changed( int numdbtp, struct db_templates_struct *dbsi )
{
	struct testchange {
                int changeinit;
                off_t table_size;
                time_t table_mtime;
                /* ignorerow table check */
                int changeinitig;
                off_t table_sizeig;
                time_t table_mtimeig;
        };

	/* allow a maximum of 30 databased */
        static struct testchange testch[30];

	static int settest = 1;
	Dbptr db, dbtable, dbtableig;
	int changed, i;
	long igrows, numrows;
	static Dbvalue value, valueig;
	struct stat table_statbuf, table_statbufig;

        if ( settest == 1 ) {
                settest = 0;
                for(i = 0; i < numdbtp; ++i) {
                        testch[i].changeinit = (int) 0;
                        testch[i].table_size = (off_t) 0;
                        testch[i].table_mtime = (time_t) 0;
                        /* init, ignorrow table monitoring variables */
                        testch[i].changeinitig = (int) 0;
                        testch[i].table_sizeig = (off_t) 0;
                        testch[i].table_mtimeig = (time_t) 0;
                }
        }

	changed = 0;
	for(i = 0; i < numdbtp; ++i) {
		dbsi[i].dbi.dbinchanged = 0;
		if(dbopen( dbsi[i].dbi.dbinname, "r", &db) != 0) {
			fprintf( stderr, "database_changed: Error opening input database %s\n", dbsi[i].dbi.dbinname);
		} else
		  {
			/* Start ignorerow table  ***********************/
			dbtableig = dblookup( db, 0, "ignorerow", 0, 0 );
			dbquery(dbtableig, dbRECORD_COUNT, &igrows );
			if ( (long) igrows > (long) 0 ) { 
				dbquery( dbtableig, dbTABLE_FILENAME, &valueig );
	
				stat( valueig.t, &table_statbufig );

				if( ! testch[i].changeinitig ) {
					testch[i].table_sizeig = (off_t) table_statbufig.st_size;
					testch[i].table_mtimeig = (time_t) table_statbufig.st_mtime;
					testch[i].changeinitig++;
					/* process on restart */
					dbsi[i].dbi.dbinchanged = 1;
					changed = 1;
				} else
				  {
					if( (off_t)testch[i].table_sizeig != (off_t) table_statbufig.st_size ||
						(time_t) testch[i].table_mtimeig != 
						(time_t) table_statbufig.st_mtime ) {
						testch[i].table_sizeig = (off_t) table_statbufig.st_size;
						testch[i].table_mtimeig = (time_t) table_statbufig.st_mtime;
						dbsi[i].dbi.dbinchanged = 1;
						changed = 1;
					}
				}
			}
			/* End database ignorerow ***********************/

			/* Start database table to monitor ***********************/
			dbtable = dblookup( db, 0, dbsi[i].dbi.monitor, 0, 0 );
			dbquery(dbtable, dbRECORD_COUNT, &numrows );
			if ( (long) numrows > (long) 0 ) { 
				dbquery( dbtable, dbTABLE_FILENAME, &value );
	
				stat( value.t, &table_statbuf );

				if( ! testch[i].changeinit ) {
					testch[i].table_size = (off_t) table_statbuf.st_size;
					testch[i].table_mtime = (time_t) table_statbuf.st_mtime;
					testch[i].changeinit++;
					/* process on restart */
					dbsi[i].dbi.dbinchanged = 1;
					changed = 1;
				} else
				  {
					if( (off_t)testch[i].table_size != (off_t) table_statbuf.st_size ||
						(time_t) testch[i].table_mtime != 
						(time_t) table_statbuf.st_mtime ) {
						testch[i].table_size = (off_t) table_statbuf.st_size;
						testch[i].table_mtime = (time_t) table_statbuf.st_mtime;
						dbsi[i].dbi.dbinchanged = 1;
						changed = 1;
					}
				}
			}
			/* end database table to monitor ***********************/
		}
	}
	dbclose(db);
	return (changed);
}

void compress_memory_maps(int i, struct db_templates_struct *dbsi) {
	long j, k, l;

	/* for(i = 0; i < numdbtp; ++i) { */
	  if(dbsi[i].dbi.dbinchanged == 1) {
		/* start loading tables into memory */
		for (j=0; j<dbsi[i].dbi.numot; j++) {
			switch (dbsi[i].dbi.ot[j].type) {
				case 1: /* event */
					for(k=0;k>-1 && (long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].evtb[k].reset == 0 ) {
							--dbsi[i].dbi.ot[j].numrows;
							for(l=k;(long) l<(long) dbsi[i].dbi.ot[j].numrows;++l) {
fprintf(stderr,"debug 1 \n");
								cp_event (l, dbsi[i].dbi.ot[j].evtb, l+1, 
								  dbsi[i].dbi.ot[j].evtb);
								dbsi[i].dbi.ot[j].evtb[l].reset =
								  dbsi[i].dbi.ot[j].evtb[
								  l+1].reset;
							} /* for l */
							--k;
						} /* if reset == 0 */
					} /*  for k */
					/* set reset to zero for next input db read-in */
					for(k=0;k>-1 && (long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						dbsi[i].dbi.ot[j].evtb[k].reset = 0;
					}
				break;
			} /* switch */
		}/*for j */
	  }/* if changed */
	/*}*//* for i */

}

void update_output_db(int i, struct db_templates_struct *dbsi) {
	int j, l, crunch, jj, found;
	long nrecs, k, kk;
	struct ArrivalTable artb[1];
	struct AssocTable asstb[1];
	struct EmodelTable emodtb[1];
	struct EventTable evtb[1];
	struct NetmagTable ntmgtb[1];
	struct OrigerrTable orertb[1];
	struct OriginTable ortb[1];
	struct PredarrTable prartb[1];
	struct StamagTable stmgtb[1];
	struct WfmeasTable wfmstb[1];
	struct MapassocTable mpastb[1];
	struct QuakeregionsTable qkrgtb[1];
	struct WebmapsTable wbmptb[1];
	double crunchtest;

	

	/* for(i = 0; i < numdbtp; ++i) { */
	if(dbsi[i].dbi.dbinchanged == 1) {
		crunchtest = now() - (double) dbsi[i].dbi.oldcrunch;
		if( (double) crunchtest > (double) dbsi[i].dbi.crunchinterval) {
			dbsi[i].dbi.crunchon = 1;
			dbsi[i].dbi.oldcrunch = now();
		}
		/* start loading tables into memory */
		for (j=0; j<dbsi[i].dbi.numot; j++) {
			if( debug == 1) { 
				fprintf(stderr,"\tupdate_output_db %s table %s\n", 
					dbsi[i].name, dbsi[i].dbi.ot[j].tb);
			}
			crunch = 0;
			switch (dbsi[i].dbi.ot[j].type) {
				case 1: /* event */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "event", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_event(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, evtb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_event (k, dbsi[i].dbi.ot[j].evtb, 0, evtb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].evtb[k].reset == 0) {
if( debug == 1) {
fprintf(stderr,"\tDebug, Exact Match but reset is 0 dbmark row at record %ld\n\t evid=%ld, prefor=%ld, commid=%ld, \n\t evname=%s, auth=%s,lddate=%f\n",
		dbsi[i].dbi.dbout.record,
		evtb[0].evid, evtb[0].prefor,
		evtb[0].commid, 
		evtb[0].evname, evtb[0].auth, 
		evtb[0].lddate);
}
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									} 
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].evtb[k].reset == 0) {
if( debug == 1) {
fprintf(stderr,"\tDebug,Keys Match but Maxtime cutoffreset is 0 dbmark row at record %ld\n\t evid=%ld, prefor=%ld, commid=%ld, \n\t evname=%s, auth=%s,lddate=%f\n",
		dbsi[i].dbi.dbout.record,
		evtb[0].evid,evtb[0].prefor,
		evtb[0].commid, 
		evtb[0].evname, evtb[0].auth, 
		evtb[0].lddate);
}
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].evtb[k].reset != 0 ) {
if( debug == 1) {
fprintf(stderr,"\tDebug, Exact Match Update row at record %ld\n\t evid=%ld, prefor=%ld, commid=%ld, \n\t evname=%s, auth=%s,lddate=%f\n",
		dbsi[i].dbi.dbout.record,
		evtb[0].evid,evtb[0].prefor,
		evtb[0].commid, 
		evtb[0].evname, evtb[0].auth, 
		evtb[0].lddate);
}
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].evtb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].evtb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].evtb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].evtb[k].reset = 2;
									} 
									put_event(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].evtb, 
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && evtb[0].evid != -1 ) {
if( debug == 1) {
fprintf(stderr,"\tDebug, Row not found dbmark row at record %ld\n\t evid=%ld, prefor=%ld, commid=%ld, \n\t evname=%s, auth=%s,lddate=%f\n",
		dbsi[i].dbi.dbout.record,
		evtb[0].evid,evtb[0].prefor,
		evtb[0].commid, 
		evtb[0].evname, evtb[0].auth, 
		evtb[0].lddate);
}
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].evtb[k].reset == 3 ) {
if( debug == 1) {
fprintf(stderr,"\tDebug, Add new row at record %ld\n\t evid=%ld, prefor=%ld, commid=%ld, \n\t evname=%s, auth=%s,lddate=%f\n",
		dbsi[i].dbi.dbout.record,
		dbsi[i].dbi.ot[j].evtb[k].evid,dbsi[i].dbi.ot[j].evtb[k].prefor,
		dbsi[i].dbi.ot[j].evtb[k].commid, 
		dbsi[i].dbi.ot[j].evtb[k].evname, dbsi[i].dbi.ot[j].evtb[k].auth, 
		dbsi[i].dbi.ot[j].evtb[k].lddate);
}
							put_event(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].evtb, 1,
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 2: /* origin */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "origin", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_origin(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, ortb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_origin (k, dbsi[i].dbi.ot[j].ortb, 0, ortb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].ortb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].ortb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].ortb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].ortb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].ortb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].ortb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].ortb[k].reset = 2;
									} 
									put_origin(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].ortb, 
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && ortb[0].orid != -1) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add new rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].ortb[k].reset == 3 ) {
							put_origin(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].ortb, 1, 
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 3: /* arrival */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "arrival", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_arrival(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, artb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_arrival (k, dbsi[i].dbi.ot[j].artb, 0, artb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].artb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									} 
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].artb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].artb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].artb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].artb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].artb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].artb[k].reset = 2;
									} 
									put_arrival(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].artb, 
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && artb[0].arid != -1) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].artb[k].reset == 3 ) {
							put_arrival(dbsi[i].dbi.dbout, k, 
							dbsi[i].dbi.ot[j].artb, 1,
							dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 4: /* emodel */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "emodel", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_emodel(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, emodtb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_emodel (k, dbsi[i].dbi.ot[j].emodtb, 0, emodtb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].emodtb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									} 
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].emodtb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].emodtb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].emodtb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].emodtb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].emodtb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].emodtb[k].reset = 2;
									} 
									put_emodel(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].emodtb, 
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && emodtb[0].orid != -1) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;k<dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].emodtb[k].reset == 3 ) {
							put_emodel(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].emodtb, 1
								, dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 5: /* netmag */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "netmag", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_netmag(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, ntmgtb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_netmag (k, dbsi[i].dbi.ot[j].ntmgtb, 0, ntmgtb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].ntmgtb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].ntmgtb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].ntmgtb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].ntmgtb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].ntmgtb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].ntmgtb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].ntmgtb[k].reset = 2;
									} 
									put_netmag(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].ntmgtb
										, 0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && ntmgtb[0].magid != -1) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].ntmgtb[k].reset == 3 ) {
							put_netmag(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].ntmgtb, 1,
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 6: /* origerr */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "origerr", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_origerr(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, orertb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_origerr (k, dbsi[i].dbi.ot[j].orertb, 0, orertb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].orertb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].orertb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].orertb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].orertb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].orertb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].orertb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].orertb[k].reset = 2;
									} 
									put_origerr(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].orertb,
										 0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && orertb[0].orid != -1 ) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].orertb[k].reset == 3 ) {
							put_origerr(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].orertb, 
								1, dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 7: /* predarr */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "predarr", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_predarr(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, prartb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_predarr (k, dbsi[i].dbi.ot[j].prartb, 0, prartb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].prartb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].prartb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].prartb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].prartb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].prartb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].prartb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].prartb[k].reset = 2;
									} 
									put_predarr(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].prartb
										, 0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && (prartb[0].arid != -1 && prartb[0].orid != -1)) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].prartb[k].reset == 3 ) {
							put_predarr(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].prartb, 1,
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 8: /* stamag */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "stamag", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_stamag(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, stmgtb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_stamag (k, dbsi[i].dbi.ot[j].stmgtb, 0, stmgtb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].stmgtb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].stmgtb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].stmgtb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].stmgtb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].stmgtb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].stmgtb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].stmgtb[k].reset = 2;
									} 
									put_stamag(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].stmgtb,
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && stmgtb[0].magid != -1 ) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].stmgtb[k].reset == 3 ) {
							put_stamag(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].stmgtb, 1, 
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 9: /* wfmeas */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "wfmeas", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_wfmeas(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, wfmstb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_wfmeas (k, dbsi[i].dbi.ot[j].wfmstb, 0, wfmstb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].wfmstb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].wfmstb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].wfmstb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].wfmstb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].wfmstb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].wfmstb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].wfmstb[k].reset = 2;
									} 
									put_wfmeas(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].wfmstb,
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && wfmstb[0].arid != -1 ) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].wfmstb[k].reset == 3 ) {
							put_wfmeas(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].wfmstb, 1,
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
				case 10: /* mapassoc */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, 
						"mapassoc", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; 
						(long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_mapassoc(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, mpastb);
						found = 0;
						for (jj=0; jj<dbsi[i].dbi.numot; jj++) {
							switch (dbsi[i].dbi.ot[jj].type) {
							case 2: /* origin */
								for(kk=0;(long) kk<
								  (long) dbsi[i].dbi.ot[jj].numrows;++kk) {
									if ((long) mpastb[0].orid == 
									  (long) dbsi[i].dbi.ot[jj
									  ].ortb[kk].orid ) {
										found = 1;
										kk = dbsi[i].dbi.ot[
											jj].numrows;
									}

								}
								jj = dbsi[i].dbi.numot;
							break;
							default:
							break;
							}
						}
						if (found == 0 ) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							}			  
							crunch = 1;

						}
					}
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch mapassoc reset rows\n");
							} 
						}
					}
				break;
				case 11: /* quakeregions */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, 
						"quakeregions", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; 
						(long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_quakeregions(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, qkrgtb);
						found = 0;
						for (jj=0; jj<dbsi[i].dbi.numot; jj++) {
							switch (dbsi[i].dbi.ot[jj].type) {
							case 2: /* origin */
								for(kk=0;(long) kk<
								  (long) dbsi[i].dbi.ot[jj].numrows;++kk) {
									if ((long) qkrgtb[0].orid == 
									  (long) dbsi[i].dbi.ot[jj
									  ].ortb[kk].orid ) {
										found = 1;
										kk = dbsi[i].dbi.ot[
											jj].numrows;
									}

								}
								jj = dbsi[i].dbi.numot;
							break;
							default:
							break;
							}
						}
						if (found == 0 ) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							}			  
							crunch = 1;

						}
					}
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch quakeregions reset rows\n");
							} 
						}
					}
				break;
				case 12: /* webmaps */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, 
						"webmaps", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; 
						(long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_webmaps(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, wbmptb);
						found = 0;
						/* only look for map names that start with evid */
						if(strncmp(wbmptb[0].mapname, "evid" ,4) != 0 ) {
							found = 1;
						}
						if (found == 0 ) {
							for (jj=0; jj<dbsi[i].dbi.numot; jj++) {
								switch (dbsi[i].dbi.ot[jj].type) {
								case 1: /* event */
									for(kk=0;(long) kk<
									  (long) dbsi[i].dbi.ot[jj].numrows;++kk) {
										if ((long) wbmptb[0].evid == 
										  (long) dbsi[i].dbi.ot[jj
										  ].evtb[kk].evid ) {
											found = 1;
											kk = dbsi[i].dbi.ot[
												jj].numrows;
										}
									}
									jj = dbsi[i].dbi.numot;
								break;
								default:
								break;
								}
							}
						}
						if (found == 0 ) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							}			  
							crunch = 1;

						}
					}
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch webmap reset rows\n");
							} 
						}
					}
				break;
				case 13: /* assoc */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "assoc", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, &nrecs);
					for(dbsi[i].dbi.dbout.record=0; (long) dbsi[i].dbi.dbout.record < (long) nrecs;
						++dbsi[i].dbi.dbout.record) {
						get_assoc(dbsi[i].dbi.dbout, 0, dbsi[i].dbi.dbout.record, asstb);
						/* scan memory map */ 
						for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
							l = check_assoc (k, dbsi[i].dbi.ot[j].asstb, 0, asstb );
							if (l == 1) { /* exact match */
								/* skip if reset is 1 */
								/* if reset is 0 null row */
								if(dbsi[i].dbi.ot[j].asstb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									} 
									crunch = 1;
								}
								k=dbsi[i].dbi.ot[j].numrows;
							} /* l == 1 */	
							if (l == 2) { /* key match update */
								/* Maxtime cutoff removed row */ 
								if(dbsi[i].dbi.ot[j].asstb[k].reset == 0) {
									if(dbmark (dbsi[i].dbi.dbout) != 0){
										complain(0,"dbmark l=2 was not able to update %s record=%ld in db %s\n", 
										dbsi[i].dbi.ot[j].tb,	
										dbsi[i].dbi.dbout.record,
										dbsi[i].dbi.dboutname);
									}  
									crunch = 1;
								}
								/* update row */
								if(dbsi[i].dbi.ot[j].asstb[k].reset != 0 ) {
									/* row externally modified by user */
									if(dbsi[i].dbi.ot[j].asstb[k].reset == 1 ) {
										dbsi[i].dbi.ot[j].asstb[k].reset = 2;
									} 
									/* no need to add an updated row */
									if(dbsi[i].dbi.ot[j].asstb[k].reset == 3 ) {
										dbsi[i].dbi.ot[j].asstb[k].reset = 2;
									} 
									put_assoc(dbsi[i].dbi.dbout, 
										k, dbsi[i].dbi.ot[j].asstb, 
										0, dbsi[i].dbi.lddate);
								}
								k=dbsi[i].dbi.ot[j].numrows;
							}
						} /* scan memory map */ 
						/* no match in memory map dbmark row for removal */ 
						if (l == 3 && (asstb[0].arid != -1 && asstb[0].orid != -1)) {
							if(dbmark (dbsi[i].dbi.dbout) != 0){
								complain(0,"dbmark was not able to update %s record=%ld in db %s\n", 
									dbsi[i].dbi.ot[j].tb,	
									dbsi[i].dbi.dbout.record,
									dbsi[i].dbi.dboutname);
							} 
							crunch = 1;
						}
					} /* for record output db loop */
					/* add now rows */
					for(k=0;(long) k<(long) dbsi[i].dbi.ot[j].numrows;++k) {
						if(dbsi[i].dbi.ot[j].asstb[k].reset == 3 ) {
							put_assoc(dbsi[i].dbi.dbout, k, 
								dbsi[i].dbi.ot[j].asstb, 1,
								dbsi[i].dbi.lddate);
						}
					} /* end for k */
					/* crunch db tables */
					if(dbsi[i].dbi.crunchon == 1) {
						if (crunch == 1) {
							if(dbcrunch(dbsi[i].dbi.dbout) != 0) {
								complain(0,"Can not dbcrunch event reset rows\n");
							} 
						}
					}
				break;
			} /* switch */
		} /* for tables */		
		/* no more crunching */	
		dbsi[i].dbi.crunchon = 0;
	  } /* if changed */
/*	} *//* for sets */
}
void read_input_db(int i, struct db_templates_struct *dbsi) {
	int j, l;
	long nrecs, k;
	Dbptr dbsep;
	struct ArrivalTable artb[1];
	struct AssocTable asstb[1];
	struct EmodelTable emodtb[1];
	struct EventTable evtb[1];
	struct NetmagTable ntmgtb[1];
	struct OrigerrTable orertb[1];
	struct OriginTable ortb[1];
	struct PredarrTable prartb[1];
	struct StamagTable stmgtb[1];
	struct WfmeasTable wfmstb[1];
	/* struct MapassocTable mpastb[1]; */
	/* struct QuakeregionsTable qkrgtb[1]; */
	/* struct WebmapsTable wbmptb[1]; */
	/* for(i = 0; i < numdbtp; ++i) { */
		/* start comparing tables */   
		if(dbsi[i].dbi.dbinchanged == 1) {
		  for (j=0; j<dbsi[i].dbi.numot; j++) {
		  	if(debug == 1) {
				fprintf(stderr,"\tProcessing %s %s \n", dbsi[i].name, dbsi[i].dbi.ot[j].tb);
			}
			switch (dbsi[i].dbi.ot[j].type) {
			  case 1: /*event*/
				/* init all reset values to zero */
				for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].evtb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "event" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_event(dbsep, 0, dbsep.record, evtb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_event (k, dbsi[i].dbi.ot[j].evtb, 0, evtb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].evtb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].evtb[k].reset = 2; /* update record */ 
fprintf(stderr,"debug 2 \n");
							cp_event(k, dbsi[i].dbi.ot[j].evtb, 0, evtb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long) dbsi[i].dbi.ot[j].numrows > (long) dbsi[i].dbi.ot[j].curalloc)||(
						  (long) (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							(long) dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].evtb = (struct EventTable *)
								realloc((struct EventTable *) dbsi[i].dbi.ot[j].evtb,
								sizeof(struct EventTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal event table, exit\n");
									exit(1);
							}
						}
						if(dbsi[i].dbi.ot[j].numrows-1 > -1) {
fprintf(stderr,"debug 3 i=%d, j=%d", i, j);
fprintf(stderr,"debug 3 numrows-1=%ld \n", dbsi[i].dbi.ot[j].numrows-1);
							cp_event( dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].evtb, 0, evtb);
							dbsi[i].dbi.ot[j].evtb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 
			  break;
			  case 2: /* origin */
				/* init all reset values to zero */
				for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].ortb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "origin" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_origin(dbsep, 0, dbsep.record, ortb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_origin (k, dbsi[i].dbi.ot[j].ortb, 0, ortb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].ortb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].ortb[k].reset = 2; /* update record */ 
							cp_origin((long)k, dbsi[i].dbi.ot[j].ortb, (long)0, ortb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (long)(dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							(long)dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].ortb = (struct OriginTable *)
								realloc((struct OriginTable *) dbsi[i].dbi.ot[j].ortb,
								sizeof(struct OriginTable )*dbsi[i].dbi.ot[j].curalloc))){
								fprintf(stderr, "Can not realloc memory for internal origin table, exit\n");
									exit(1);
							}
						}
						if(dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_origin((long)dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].ortb, (long)0, ortb);
							dbsi[i].dbi.ot[j].ortb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  case 3: /* arrival */
				/* init all reset values to zero */
				for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].artb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "arrival" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_arrival(dbsep, 0, dbsep.record, artb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_arrival (k, dbsi[i].dbi.ot[j].artb, 0, artb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].artb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].artb[k].reset = 2; /* update record */ 
							cp_arrival(k, dbsi[i].dbi.ot[j].artb, 0, artb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long) dbsi[i].dbi.ot[j].numrows > (long) dbsi[i].dbi.ot[j].curalloc)||(
						  (long) (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							(long) dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].artb = (struct ArrivalTable *)
								realloc((struct ArrivalTable *) dbsi[i].dbi.ot[j].artb,
								sizeof(struct ArrivalTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal arrival table, exit\n");
									exit(1);
							}
						}
						if( dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_arrival(dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].artb, 0, artb);
							dbsi[i].dbi.ot[j].artb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
		 	  case 4: /* emodel */
				/* init all reset values to zero */
				for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].emodtb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "emodel" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_emodel(dbsep, 0, dbsep.record, emodtb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long) k<(long) dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_emodel (k, dbsi[i].dbi.ot[j].emodtb, 0, emodtb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].emodtb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].emodtb[k].reset = 2; /* update record */ 
							cp_emodel(k, dbsi[i].dbi.ot[j].emodtb, 0, emodtb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].emodtb = (struct EmodelTable *)
								realloc((struct EmodelTable *) dbsi[i].dbi.ot[j].emodtb,
								sizeof(struct EmodelTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal emodel table, exit\n");
									exit(1);
							}	
						}
						if( dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_emodel(dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].emodtb, 0, emodtb);
							dbsi[i].dbi.ot[j].emodtb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  case 5: /* netmag */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].ntmgtb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "netmag" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_netmag(dbsep, 0, dbsep.record, ntmgtb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_netmag (k, dbsi[i].dbi.ot[j].ntmgtb, 0, ntmgtb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].ntmgtb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].ntmgtb[k].reset = 2; /* update record */ 
							cp_netmag((long) k, dbsi[i].dbi.ot[j].ntmgtb, (long)0, ntmgtb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].ntmgtb = (struct NetmagTable *)
								realloc((struct NetmagTable *) dbsi[i].dbi.ot[j].ntmgtb,
								sizeof(struct NetmagTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal netmag table, exit\n");
									exit(1);
							}
						}
						if(dbsi[i].dbi.ot[j].numrows-1 > -1) {	
							cp_netmag((long)dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].ntmgtb, (long)0, ntmgtb);
							dbsi[i].dbi.ot[j].ntmgtb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  case 6: /* origerr */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].orertb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "origerr" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_origerr(dbsep, 0, dbsep.record, orertb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_origerr (k, dbsi[i].dbi.ot[j].orertb, 0, orertb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].orertb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].orertb[k].reset = 2; /* update record */ 
							cp_origerr((long)k, dbsi[i].dbi.ot[j].orertb, (long)0, orertb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].orertb = (struct OrigerrTable *)
								realloc((struct OrigerrTable *) dbsi[i].dbi.ot[j].orertb,
								sizeof(struct OrigerrTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal origerr table, exit\n");
									exit(1);
							}	
						}
						if(dbsi[i].dbi.ot[j].numrows > -1 ) {
							cp_origerr((long)dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].orertb, (long)0, orertb);
							dbsi[i].dbi.ot[j].orertb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  case 7: /* predarr */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].prartb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "predarr" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_predarr(dbsep, 0, dbsep.record, prartb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_predarr (k, dbsi[i].dbi.ot[j].prartb, 0, prartb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].prartb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].prartb[k].reset = 2; /* update record */ 
							cp_predarr((long)k, dbsi[i].dbi.ot[j].prartb, (long)0, prartb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].prartb = (struct PredarrTable *)
								realloc((struct PredarrTable *) dbsi[i].dbi.ot[j].prartb,
								sizeof(struct PredarrTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal predarr table, exit\n");
									exit(1);
							}
						}
						if( dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_predarr((long)dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].prartb, (long)0, prartb);
							dbsi[i].dbi.ot[j].prartb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  case 8: /* stamag */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].stmgtb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "stamag" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_stamag(dbsep, 0, dbsep.record, stmgtb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_stamag (k, dbsi[i].dbi.ot[j].stmgtb, 0, stmgtb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].stmgtb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].stmgtb[k].reset = 2; /* update record */ 
							cp_stamag((long)k, dbsi[i].dbi.ot[j].stmgtb, (long)0, stmgtb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].stmgtb = (struct StamagTable *)
								realloc((struct StamagTable *) dbsi[i].dbi.ot[j].stmgtb,
								sizeof(struct StamagTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal stamag table, exit\n");
									exit(1);
							}
						}
						if(dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_stamag((long)dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].stmgtb, (long)0, stmgtb);
							dbsi[i].dbi.ot[j].stmgtb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  case 9: /* wfmeas */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].wfmstb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "wfmeas" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_wfmeas(dbsep, 0, dbsep.record, wfmstb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_wfmeas (k, dbsi[i].dbi.ot[j].wfmstb, 0, wfmstb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].wfmstb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].wfmstb[k].reset = 2; /* update record */ 
							cp_wfmeas((long)k, dbsi[i].dbi.ot[j].wfmstb, (long)0, wfmstb);
							k = dbsi[i].dbi.ot[j].numrows;
						} 
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].wfmstb = (struct WfmeasTable *)
								realloc((struct WfmeasTable *) dbsi[i].dbi.ot[j].wfmstb,
								sizeof(struct WfmeasTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal wfmeas table, exit\n");
									exit(1);
							}	
						}
						if ( dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_wfmeas((long)dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].wfmstb, (long)0, wfmstb);
							dbsi[i].dbi.ot[j].wfmstb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
			  /* ignore */
			  case 10: /* mapassoc */
			  case 11: /* quakeregions */
			  case 12: /* webmaps */
			  break;
			  /* switch 10 to 110 to ignore */
			  case 110: /* mapassoc */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].mpastb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "mapassoc" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				fprintf(stderr,"%ld mapassoc records\n", nrecs);
			  break;
			  /* switch 11 to 111 to ignore */
			  case 111: /* quakeregions */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].qkrgtb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "quakeregions" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				fprintf(stderr,"%ld quakeregions records\n", nrecs);
			  break;
			  /* switch 12 to 112 to ignore */
			  case 112: /* webmaps */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].wbmptb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "webmaps" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				fprintf(stderr,"%ld  webmaps records\n", nrecs);
			  break;
			  case 13: /* assoc */
				/* init all reset values to zero */
				for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
					dbsi[i].dbi.ot[j].asstb[k].reset = 0; 
				}
				dbsep = dbseparate ( dbsi[i].dbi.fin, "assoc" ); 
				dbquery (dbsep, dbRECORD_COUNT, &nrecs);
				for(dbsep.record = 0; (long) dbsep.record < (long) nrecs; 
					++dbsep.record) {
					get_assoc(dbsep, 0, dbsep.record, asstb);
					/* scan memory map, decide to update row, null row, or add row */
					l = 0; /* incase nunrows is zero */ 
					for(k=0; (long)k<(long)dbsi[i].dbi.ot[j].numrows; ++k) {
						l = check_assoc (k, dbsi[i].dbi.ot[j].asstb, 0, asstb );
						if (l == 1) { /* exact match */
							dbsi[i].dbi.ot[j].asstb[k].reset = 1; /* leave alone */ 
							k = dbsi[i].dbi.ot[j].numrows;
						} 
						if (l == 2) { /* update */
							dbsi[i].dbi.ot[j].asstb[k].reset = 2; /* update record */ 
							cp_assoc(k, dbsi[i].dbi.ot[j].asstb, 0, asstb);
							k = dbsi[i].dbi.ot[j].numrows;
						}
					} /* for k, memory map rows */
					if (l == 3 || l == 0 ) { /* add row */
						/* reallocate memory */
						++dbsi[i].dbi.ot[j].numrows;
						if(((long)dbsi[i].dbi.ot[j].numrows > (long)dbsi[i].dbi.ot[j].curalloc)||(
						  (dbsi[i].dbi.ot[j].curalloc - dbsi[i].dbi.ot[j].numrows) >
							dbsi[i].dbi.realloc )) {
							dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.realloc + 
								dbsi[i].dbi.ot[j].numrows;
							if(!(dbsi[i].dbi.ot[j].asstb = (struct AssocTable *)
								realloc((struct AssocTable *) dbsi[i].dbi.ot[j].asstb,
								sizeof(struct AssocTable )*dbsi[i].dbi.ot[j].curalloc))){
									fprintf(stderr, "Can not realloc memory for internal assoc table, exit\n");
									exit(1);
							}	
						}
						if ( dbsi[i].dbi.ot[j].numrows-1 > -1 ) {
							cp_assoc(dbsi[i].dbi.ot[j].numrows-1, 
								dbsi[i].dbi.ot[j].asstb, 0, asstb);
							dbsi[i].dbi.ot[j].asstb[dbsi[i].dbi.ot[j].numrows-1].reset = 3; /* new row */
						}
					} /* end if */ 
				} /* for records */ 

			  break;
	/*       wfmeas=9, mapassoc=10,  */
	/*      quakeregions=11, webmaps=12, assoc=13 */
			} /* switch */
		  } /* for j */
		} /* if changed */
/*	} *//* for i */
}

void show_lookup_counts(Dbptr db, int i, struct db_templates_struct *dbsi, char *statement, int joined) {
	int  j;
	long nrecs;
	Dbptr dbsep;
	/* print table record count for tables that will be output tables */   
	for (j=0; j<dbsi[i].dbi.numot; j++) {
		/* joined table */
		if (joined == 1 ) {
			dbsep = dbseparate ( db, dbsi[i].dbi.ot[j].tb );
			/* lookup will force the intire table to show up */	
			/* dbsep = dblookup(dbsep, 0, dbsi[i].dbi.ot[j].tb, 0, 0 ); */
		} else
		  { 
			dbsep = dblookup(db, 0, dbsi[i].dbi.ot[j].tb, 0, 0 );
		}
		dbquery (dbsep, dbRECORD_COUNT, &nrecs);
		if ( debug == 1 ) {	
			fprintf(stderr,"%s %s db table %s view %s has %ld records \n", dbsi[i].name, dbsi[i].dbi.dbinname, dbsi[i].dbi.ot[j].tb, statement, nrecs);
		}
	}
}
void freex ( void *val )
{
	Stbl *stbl ;
	stbl = (Stbl *) val;
	freestbl(stbl, 0) ;
}

void show_records ( Arr *table_records ) {
	Stbl *stbl ;
	Tbl *keys ;
	int i, n ;
	long nrecords;
	char *atable ;

	keys = keysarr(table_records) ;
	n = maxtbl(keys) ;

	for ( i=0 ; i<n ; i++ ) {
		atable = gettbl(keys, i) ;
		printf ( "\nTable %s", atable ) ;
		stbl = getarr(table_records, atable) ;
		nrecords = maxstbl(stbl) ;
		printf ( "\nTable %s has %ld records", atable, nrecords ) ;
		/* for ( j=0 ; j<nrecords ; j++ ) {
		*	arecord = (int) getstbl(stbl, j) ;
		*	if ( j % 10 == 0 ) {
		*		printf ( "\n\t" ) ;
 		*	}
		*	printf ( " %5d", arecord ) ;
		*}
		*printf ( "\n" ) ;
		*/
	}
	freetbl (keys, 0) ;
}

void setup_input_db(int i, struct db_templates_struct *dbsi) {
	int minepoch;
	long nrecords;
	int minepoch1, minepoch2;
	char minepochs[100];
	/* Arr  *table_records=0 ; */
	double lasttime;
	Tbl	*by_time;

	if(dbsi[i].dbi.dbinchanged == 1) {
		if(dbopen( dbsi[i].dbi.dbinname, "r", &(dbsi[i].dbi.dbin) ) != 0) {
			fprintf( stderr, "Error opening input database %s\n", dbsi[i].dbi.dbinname);
		}
		if( debug == 1) {
			fprintf(stderr,"\tBefore dbprocess call \n");
		}
		/* show_lookup_counts(dbsi[i].dbi.dbin, i, dbsi,"dbi.dbin", 0); */
		dbsi[i].dbi.dbproc = dbprocess ( dbsi[i].dbi.dbin, dbsi[i].dbi.dbproctbl, 0 ) ;
		if( debug == 1) {
			fprintf(stderr,"\tAfter dbprocess call \n");
		}
		dbquery ( dbsi[i].dbi.dbproc, dbRECORD_COUNT, &nrecords ) ;
			/* if ( nrecords > 0 ) {
		*	dbuntangle ( dbsi[i].dbi.dbproc, &table_records ) ;
		*	show_records ( table_records ) ;
		*	freearr(table_records, freex ) ;
		*	table_records = 0 ;
		* 	else {
		*/
		if ( (long) nrecords < (long) 1 ) {
			if( debug == 1) {
				fprintf(stderr,"no records in dbproc %s view at %s, do nothing \n", dbsi[i].name,strtime(now()));
			}
			if( db_destroy == 0 ) {
				dbsi[i].dbi.dbinchanged = 0;
			}
			/* complain ( 0, "no records in dbproc view") ; */
		}	
		/* show_lookup_counts(dbsi[i].dbi.dbproc,i,dbsi,"dbdbproc", 1); */
		if(dbsi[i].dbi.maxdays > 0 ) {
			minepoch = dbsi[i].dbi.maxdays * 3600 * 24 ;
			by_time = strtbl("time", 0 ) ;
			dbsi[i].dbi.dbproc = dbsort ( dbsi[i].dbi.dbproc, by_time, 0, 0 ) ;
			freetbl(by_time, 0) ;
			dbquery ( dbsi[i].dbi.dbproc, dbRECORD_COUNT, &nrecords ) ;
			if ( (long) nrecords > (long) 0 ) {
				dbsi[i].dbi.dbproc.record = nrecords - 1;
				if(dbgetv(dbsi[i].dbi.dbproc, 0, "time", &lasttime, NULL) == dbINVALID) {
					 fprintf(stderr,"\tsetup_input_db:Can not find lasttime in %s after dbsort of dbproc view %s\n"
						,dbsi[i].name, strtime(now()));
				}

			} else
			  {
				/* problem no records */
				 fprintf(stderr,"\tsetup_input_db:No records in %s after dbsort of dbproc view %s\n"
					,dbsi[i].name, strtime(now()));
			}
			  
			minepoch1 = yearday(now()-minepoch);
			minepoch2 = yearday(lasttime-minepoch);
			if(minepoch2 > minepoch1) { /* event is in future, use computer time */
				minepoch = minepoch1;
			} else
			  {
				minepoch = minepoch2;
			} 
			sprintf(minepochs,"yearday(origin.time)>%d", minepoch);
			/* if( debug == 1) {
			*		fprintf(stderr,"Subsetting %s \n", minepochs);
			* }
			*/	
			dbsi[i].dbi.fin = dbsubset(dbsi[i].dbi.dbproc, minepochs, 0);
		} else
		  {
			dbsi[i].dbi.fin = dbsi[i].dbi.dbproc;
		}
		/* show_lookup_counts(dbsi[i].dbi.fin,i,dbsi,"dbi.fin", 1); */
	}
}
	
void free_input_db(int i, struct db_templates_struct *dbsi) {
	int j, set;
	long numrows;

	if(dbsi[i].dbi.dbinchanged == 1) {
		if(dbsi[i].dbi.maxdays > 0 ) {
			/* only free up if numrows is > 0 */
			dbquery (dbsi[i].dbi.fin, dbRECORD_COUNT, &numrows);
			if ( (long) numrows > (long) 0 ) {
				dbsi[i].dbi.fin.record = 0;
				if( dbfree(dbsi[i].dbi.fin) == dbINVALID) {
					fprintf(stderr,"\tfree_input_db:Can not dbfree dbi.fin %s view at %s\n"
						, dbsi[i].name,strtime(now()));
				}
			}
		}
		dbquery (dbsi[i].dbi.dbproc, dbRECORD_COUNT, &numrows);
		if ( (long) numrows > (long) 0 ) {
			dbsi[i].dbi.dbproc.record = 0;
			if( dbfree(dbsi[i].dbi.dbproc) == dbINVALID) {
				fprintf(stderr,"\tfree_input_db:Can not dbfree dbi.dbproc %s view at %s\n"
					, dbsi[i].name,strtime(now()));
			}
		}
		/* dbfree(dbsi[i].dbi.dbin); */
		/* dbsi[i].dbi.fin = dbsi[i].dbi.dbproc; */
		set = 0;
		for (j=0; j<dbsi[i].dbi.numot; j++) {
			/* fprintf(stderr,"\tfree_input_db:Trying to dbfree dbi.dbin %s %s table at %s\n"
					,dbsi[i].name, dbsi[i].dbi.ot[j].tb, strtime(now())); */
			dbsi[i].dbi.dblook = dblookup( dbsi[i].dbi.dbin, 
				0, (char *)dbsi[i].dbi.ot[j].tb, 0, 0 );
			dbquery (dbsi[i].dbi.dblook, dbRECORD_COUNT, &numrows);
			if ((long) numrows > (long) 0) {
				if( dbfree(dbsi[i].dbi.dblook) == dbINVALID) {
					 fprintf(stderr,"\tfree_input_db:Can not dbfree dbi.dbin %s %s table at %s\n"
					,dbsi[i].name, dbsi[i].dbi.ot[j].tb, strtime(now()));

				}
				/* if(debug == 1) {
					fprintf(stderr,"\tfree_input_db:dbfree dbi.dbin %s %s table at %s\n"
					,dbsi[i].name , dbsi[i].dbi.ot[j].tb, strtime(now()));
				} */
				set = 1;
			}
		}
		if ( set == 1 ) {
			if( dbclose(dbsi[i].dbi.dbin) == dbINVALID) {
				fprintf(stderr,"\tfree_input_db:Can not dbclose dbi.dbin %s db at %s\n"
					, dbsi[i].name,strtime(now()));
			}
		
		} else
		  {
			/* if(debug == 1) {
				fprintf(stderr,"\tfree_input_db:dbi.dbin %s db at %s contains no tables\n"
					, dbsi[i].name,strtime(now()));
			} */

		}
	} 
}

void load_output_db(int numdbtp, struct db_templates_struct *dbsi) {
	int i, j, x;
	for(i = 0; i < numdbtp; ++i) {
		/* open output db and load*/
		if(dbopen( dbsi[i].dbi.dboutname, "r+", &(dbsi[i].dbi.dbout) ) != 0 ) {
			fprintf( stderr, "Error opening output database %s\n", 
			dbsi[i].dbi.dboutname);
			for (j=0; j<dbsi[i].dbi.numot; j++) {
				dbsi[i].dbi.ot[j].numrows = -1;
			}
		} else
		  {
			/* start loading tables into memory */
			for (j=0; j<dbsi[i].dbi.numot; j++) {
				switch (dbsi[i].dbi.ot[j].type) {
				  case 1: /*event*/
					/* lookup table */
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "event", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].evtb = (struct EventTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct EventTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_event(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].evtb );
							dbsi[i].dbi.ot[j].evtb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 2: /*origin*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "origin", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].ortb = (struct OriginTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct OriginTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_origin(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].ortb );
							dbsi[i].dbi.ot[j].ortb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 3: /*arrival*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "arrival", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].artb = (struct ArrivalTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct ArrivalTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_arrival(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].artb );
							dbsi[i].dbi.ot[j].artb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 4: /*emodel*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "emodel", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].emodtb = (struct EmodelTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct EmodelTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_emodel(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].emodtb );
							dbsi[i].dbi.ot[j].emodtb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 5: /*netmag*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "netmag", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].ntmgtb = (struct NetmagTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct NetmagTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_netmag(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].ntmgtb );
							dbsi[i].dbi.ot[j].ntmgtb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 6: /*origerr*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "origerr", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].orertb = (struct OrigerrTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct OrigerrTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_origerr(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].orertb );
							dbsi[i].dbi.ot[j].orertb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 7: /*predarr*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "predarr", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].prartb = (struct PredarrTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct PredarrTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_predarr(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].prartb );
							dbsi[i].dbi.ot[j].prartb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 8: /*stamag*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "stamag", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].stmgtb = (struct StamagTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct StamagTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_stamag(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].stmgtb );
							dbsi[i].dbi.ot[j].stmgtb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 9: /*wfmeas*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "wfmeas", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].wfmstb = (struct WfmeasTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct WfmeasTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_wfmeas(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].wfmstb );
							dbsi[i].dbi.ot[j].wfmstb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  /* no reason to load these rows */
				  /* just dbmark rows created by dbrecenteqs by comparing orid */
				  case 10: /*mapassoc*/
				  case 11: /*quakeregions*/
				  case 12: /*webmaps*/
					/* dbsi[i].dbi.ot[j].numrows = 0; */
				  break;
				  /* switch 10 to 110 so switch statement ignores */
				  case 110: /*mapassoc*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "mapassoc", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].mpastb = (struct MapassocTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct MapassocTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_mapassoc(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].mpastb );
							dbsi[i].dbi.ot[j].mpastb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  /* switch 11 to 111 so switch statement ignores */
				  case 111: /*quakeregions*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "quakeregions", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].qkrgtb = (struct QuakeregionsTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct QuakeregionsTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_quakeregions(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].qkrgtb );
							dbsi[i].dbi.ot[j].qkrgtb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  /* switch 12 to 112 so switch statement ignores */
				  case 112: /*webmaps*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "webmaps", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].wbmptb = (struct WebmapsTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct WebmapsTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_webmaps(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].wbmptb );
							dbsi[i].dbi.ot[j].wbmptb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				  case 13: /*assoc*/
					dbsi[i].dbi.dbout = dblookup(dbsi[i].dbi.dbout, 0, "assoc", 0, 0 );
					dbquery (dbsi[i].dbi.dbout, dbRECORD_COUNT, 
						&(dbsi[i].dbi.ot[j].numrows));
					if( (long) dbsi[i].dbi.ot[j].numrows > (long) 0 ) {
						/* allocate memory for structure */
						dbsi[i].dbi.ot[j].asstb = (struct AssocTable *) 
							calloc(dbsi[i].dbi.ot[j].numrows, 
								sizeof(struct AssocTable ));
						dbsi[i].dbi.ot[j].curalloc = dbsi[i].dbi.ot[j].numrows;
						/* load rows into memory */
						for (dbsi[i].dbi.dbout.record = 0; 
							(long) dbsi[i].dbi.dbout.record < (long) dbsi[i].dbi.ot[j].numrows; 
							++dbsi[i].dbi.dbout.record) {
							x = dbsi[i].dbi.dbout.record;
							get_assoc(dbsi[i].dbi.dbout, x, x, dbsi[i].dbi.ot[j].asstb );
							dbsi[i].dbi.ot[j].asstb[x].reset = 0;
						}
					} else
					  {
						dbsi[i].dbi.ot[j].curalloc = 0;
					}
				  break;
				} /* switch */
			} /* for j */
			if (debug == 1 ) {
				for (j=0; j<dbsi[i].dbi.numot; j++) {
					fprintf(stderr, "%s %s loaded %ld records into memory \n", 
						dbsi[i].dbi.dboutname, dbsi[i].dbi.ot[j].tb, dbsi[i].dbi.ot[j].numrows); 
				}
			}
		} /* else dbopen */ 
	} /* for */ 
}

int main( int argc, char **argv )
{
	/* Dbptr dbarrival; */
	/* int narrivals; */

	int x, i;
	struct db_templates_struct *dbsi;
	int numdbtp, inputchange;
	char *pfname;
	int statusp, pid;

	/* table structure */
	/* struct ArrivalTable *artb; */
	/* struct AssocTable *asstb; */
	/* struct EmodelTable *emodtb; */
	/* struct EventTable *evtb; */
	/* struct NetmagTable *ntmgtb; */
	/* struct OrigerrTable *orertb; */
	/* struct OriginTable *ortb; */
	/* struct PredarrTable *prartb; */
	/* struct StamagTable *stmgtb; */
	/* struct WfmeasTable *wfmstb; */
	/* struct MapassocTable *mpastb; */
	/* struct QuakeregionsTable *qkrgtb; */
	/* struct WebmapsTable *wbmptb; */
	pfname = argv[0];

	/* Do not exit if more than 50 logs */
	elog_init (argc, argv);
	elog_set ( ELOG_MAXMSG, -1, 0 )  ;

	debug = 0;
	for (x=1; x < argc; ++x) {
		if( strcmp("-pf", argv[x]) == 0 ) {
			if ( (x + 1) != argc ) { 
				++x;
				pfname = argv[x];
			}
		}
		if( strcmp("-debug", argv[x]) == 0 ) {
			debug = 1;
		}
	}
	if (debug == 1) fprintf(stderr, "Reading parameter file %s \n", pfname); 
	dbsi = (struct db_templates_struct *) 
		get_param_file_info(pfname, &numdbtp);

	/* load output db contained in pf file into memory */
	if (debug == 1) fprintf(stderr, "loading output database \n"); 
	load_output_db(numdbtp, (struct db_templates_struct *)dbsi); 
	whileloop = 1;
	while(whileloop == 1) { 
		if(onerun == 1) whileloop = 0;
		/* setup input db */
		for(i = 0; i < numdbtp; ++i) {
			if(debug == 1) { fprintf(stderr,"Group %s \n", dbsi[i].name ); }
			if(debug == 1) { fprintf(stderr,"\tBefore setup_input_db \n"); }
			setup_input_db(i, (struct db_templates_struct *)dbsi);
			/* read input db and compare it against memory map */
			if(debug == 1) { fprintf(stderr,"\tBefore read_input_db \n"); }
 			read_input_db(i, (struct db_templates_struct *)dbsi);
			/* free input_db */
			if(debug == 1) { fprintf(stderr,"\tBefore free_input_db \n"); }
			free_input_db(i, (struct db_templates_struct *) dbsi);
			/* update output db tables */
			if(debug == 1) { fprintf(stderr,"\tBefore update_output_db \n"); }
			update_output_db(i, (struct db_templates_struct *)dbsi);
			/* compress memory maps */
			if(debug == 1) { fprintf(stderr,"\tBefore compress_memory_maps \n"); }
			compress_memory_maps(i, (struct db_templates_struct *)dbsi);
		}
		if (forkprog != NULL) {
			if(debug == 1) { fprintf(stderr,"\tBefore %s\n", forkprog); }
			/* create child process  and start fork */
			pid = fork();
			/* child process */
			if (pid == 0) {
				execvp(forkprogargs[0], &forkprogargs[1]);
			} else if (pid == -1) {
				if (debug == 1) {
					fprintf(stderr,"Can not fork %s \n", forkprog);
				}
			/* parent */
			} else if (pid > 0) {
				(void) wait(&statusp);
			} 
		}
		if (onerun == 0) {
			if(debug == 1) { fprintf(stderr,"Before waiting for input db change \n"); }
			/* wait for input db modificatons */
			inputchange = 0;
			for(i = 0; i < numdbtp; ++i) {
		  		dbsi[i].dbi.dbinchanged = 0;
			}
			if (magfound == 1) { 
				sleep(aftereventsleep);
			}
			while (inputchange == 0) {
				magfound = 0;
				sleep(sleeptime);
				inputchange = database_changed(numdbtp, (struct db_templates_struct *)dbsi);
				if(inputchange == 1) {
					fprintf(stderr,"modification detected at %s waiting %ld seconds before reading input db \n",strtime(now()), waitandgetsleep);
					sleep(waitandgetsleep);
				}
			}
		}
	}  /* end of while loop */
}
