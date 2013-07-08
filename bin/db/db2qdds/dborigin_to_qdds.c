#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "orb.h"
#include "xtra.h"

struct qddsinfo {
        /* qdds table parts */
       long evid, orid, jdate, vernum;
       char auth[16], qtype[3], psent[2], qddscube[81];
       double lddate;
      /* parts used internally by program */
      long reset; /* keep track of which fields match rows in summary database */
};

/* global variables */
extern struct qddsinfo *qi;
extern long numqind, numignored, ignore_db;
extern Dbptr dbevent, dbevent_ig;
extern char aline[81];
extern long evid, orid, jdate, version;

extern int MenloCheckChar( char *);
extern long closest_station ( char *, long);
extern long determine_seismic_gap( char *, long);
extern long determine_number_sta_used_sol( char *, long);
extern long determine_erho_erzz_rmss( char *, char *, char *, long);
extern long determine_nm_em( char *, char *, long);

/* Cube Format 
*TpEidnumbrSoVYearMoDyHrMnSecLatddddLongddddDeptMgNstNphDminRmssErhoErzzGpMNmEmLC
*     12345678901234567890123456789012345678901234567890123456789012345678901234567890
*              1         2         3         4         5         6         7         8
*
*     a2 * Tp   = Message type = "E " (seismic event)
*     a8 * Eid  = Event identification number  (any string)
*     a2 * So   = Data Source =  regional network designation
*     a1 * V    = Event Version     (ASCII char, except [,])
*     i4 * Year = Calendar year                (GMT) (-999-6070)
*     i2 * Mo   = Month of the year            (GMT) (1-12)
*     i2 * Dy   = Day of the month             (GMT) (1-31)
*     i2 * Hr   = Hours since midnight         (GMT) (0-23)
*     i2 * Mn   = Minutes past the hour        (GMT) (0-59)
*     i3 * Sec  = Seconds past the minute * 10 (GMT) (0-599)
*     i7 * Lat  = Latitude:  signed decimal degrees*10000 north>0
*     i8 * Long = Longitude: signed decimal degrees*10000 west <0
*     i4   Dept = Depth below sea level, kilometers * 10
*     i2   Mg   = Magnitude * 10
*     i3   Nst  = Number of stations used for location
*     i3   Nph  = Number of phases used for location
*     i4   Dmin = Distance to 1st station;   kilometers * 10
*     i4   Rmss = Rms time error; sec * 100
*     i4   Erho = Horizontal standard error; kilometers * 10
*     i4   Erzz = Vertical standard error;   kilometers * 10
*     i2   Gp   = Azimuthal gap, percent of circle; degrees/3.6
*     a1   M    = Magnitude type
*     i2   Nm   = Number of stations for magnitude determination
*     i2   Em   = Standard error of the magnitude * 10
*     a1   L    = Location method
*     a1 * C    = Menlo Park check character, defined below
*/

char getversionchar(long i) {
	char c;
	if (  (long) i <= (long) 9) c = '0' + i; 
	if (  (long) i >  (long) 9 &&  (long) i <=  (long) 36 ) c = 'A' + i - 10; 
	if (  (long) i >  (long) 36 &&  (long) i <= (long) 63 ) c = 'a' + i - 37;
	if (  (long) i >  (long) 63 ) c = 'z';
	return(c);
}

int dborigin_to_qdds(qtype, lat, lon, depth, time, ml, mb, ms, ndef, review, auth, minepoch, debug, dbtype )
char    *qtype;
double  lat, lon, depth, time, ml, mb, ms;
long     ndef;
char	*review;
char	*auth;
long 	minepoch;
long 	debug;
long	dbtype; /* 0 sumdb, 1 ignoredb */

{

	char	loc_method; /* Upper-case indicates an unconfirmed event */

	char	dbtype_str[30];
	char	dmin[6], gp[4], nst[5], erho[6], erzz[6], rmss[6], nm[4], em[4];
	/* atime variables */
	long year, month, day, hour, minute;
	long ilat, ilon, idep, imag, isec;
	float second;
	long i, currentver, ivernum,  createdelete;

	if ((long) dbtype == (long) 0) {
		strcpy(dbtype_str,"dbsum");
		createdelete = -1;
	} else
         {	strcpy(dbtype_str,"dbignore");
		createdelete = 1;
	}

	currentver = -1;

	auth[8] = '\0';

	/* return if jdate is too old */
	if( (long) jdate < (long) minepoch) {
		return(-5);
	}

	/* determine how to process new summary row */
	/* scan memory dumped qdds database */
	if ( (long) numqind > (long) 0 ) {
		for(i=0; (long) i < (long) numqind; ++i) {
			/* this row has been processed, return */
			if(( (long) qi[i].evid == (long) evid 
				&& (long) qi[i].orid == (long) orid && 
				(long) qi[i].jdate == (long) jdate && 
				(long) dbtype == (long) 0) ||
			   	( (long) qi[i].evid == (long) evid 
				&& (long) qi[i].jdate == (long) jdate &&
				strncmp((char *)qi[i].qtype,"dl", 2) == 0) ) {
				qi[i].reset = 1; /* row  matches row in qdds table, ignore */
				return (-5);
			}
			/* row has been updated, keep track of version number */
			if((long) qi[i].evid == (long) evid && 
				(long) qi[i].jdate == (long) jdate) {
				if((long) qi[i].vernum > (long) currentver ) {
					currentver = qi[i].vernum ;
					ivernum = i;
				} 
			} 
		}
	}

	if(strncmp(auth, "UAF:", 4) == 0 || strcmp(review,"-") != 0) {
		/* information release or processed */
		loc_method = 'a';
		if ( (long) currentver > (long) 1 ) {
			version = currentver + 1;
		} else
		  {  
			version = 2 ;
		}
		strcpy( qtype, "rv");
	} else
	  {
		/* only submit if no current version has been sub-mitted */
		if ( (long) currentver < (long) 2 ) {
			loc_method = 'A';
			version = 1 ;
			strcpy( qtype, "au");
		} else
		  {
			return (-5);
		}
	}
	/* compute closest station */
	closest_station (dmin, orid); 
	/* compute largest gap */
	determine_seismic_gap(gp, orid);
	/* determine number of stations used in solution */
	determine_number_sta_used_sol(nst,orid);
	/* determine determine_erho_erzz_rmss */
	determine_erho_erzz_rmss(erho, erzz, rmss, orid);
	/* determine determine_nm_em */
	determine_nm_em(nm, em, orid);

	sscanf(epoch2str(time,"%Y %m %d %H %M %S"),
	"%4ld%2ld%2ld%2ld%2ld%6f",&year,&month,&day,&hour,&minute,&second);
	isec = (second*10.0) + .5;
	ilat = (lat*10000.0) + .5;
	ilon = (lon*10000.0) + .5;
	idep = (depth*10.0) + .5;
       	if( ml == -999.0 ) {
		imag = 0;
	} else
	  {
		imag = (ml * 10.0) + .5;
	}
	/* Cannot handle negative magnitudes, so just set to zero.*/
	if ((long)imag < (long)0) imag = 0;
       	if( ml == -999.0  && mb == -999.0 && ms == -999.0 ) {
		sprintf(aline, "E %08ldAK%c%04ld%02ld%02ld%02ld%02ld%03ld%07ld%08ld%04ld%-2s%-3s%03ld%-4s%-4s%-4s%-4s%-2s%-1s%-2s%-2s%c ",
		evid,getversionchar(version),year,month,day,hour,minute,isec,
		ilat,ilon,idep,"  ",nst,ndef,dmin,
		rmss,erho,erzz,gp," ",nm,em,
		loc_method);
	} else
	  {
		if( ms != -999.0 ) {
			imag = (ms * 10.0) + .5;
			if ((long)imag < (long)0) imag = 0;
			sprintf(aline, "E %08ldAK%c%04ld%02ld%02ld%02ld%02ld%03ld%07ld%08ld%04ld%02ld%-3s%03ld%-4s%-4s%-4s%-4s%-2s%-1s%-2s%-2s%c ",
			evid,getversionchar(version),year,month,day,hour,minute,isec,
			ilat,ilon,idep,imag,nst,ndef,dmin,
			rmss,erho,erzz,gp,"S",nm,em,
			loc_method);
		} else if ( mb != -999.0 ) {
			imag = (mb * 10.0) + .5;
			if ((long)imag < (long)0) imag = 0;
			sprintf(aline, "E %08ldAK%c%04ld%02ld%02ld%02ld%02ld%03ld%07ld%08ld%04ld%02ld%-3s%03ld%-4s%-4s%-4s%-4s%-2s%-1s%-2s%-2s%c ",
			evid,getversionchar(version),year,month,day,hour,minute,isec,
			ilat,ilon,idep,imag,nst,ndef,dmin,
			rmss,erho,erzz,gp,"B",nm,em,
			loc_method);
		} else
		  {

			sprintf(aline, "E %08ldAK%c%04ld%02ld%02ld%02ld%02ld%03ld%07ld%08ld%04ld%02ld%-3s%03ld%-4s%-4s%-4s%-4s%-2s%-1s%-2s%-2s%c ",
			evid,getversionchar(version),year,month,day,hour,minute,isec,
			ilat,ilon,idep,imag,nst,ndef,dmin,
			rmss,erho,erzz,gp,"L",nm,em,
			loc_method);
		}
	}
	/* Deletion request for event  */	
	if ((long)dbtype == (long)1 )
	  {
		strcpy( qtype, "dl");
		aline[0] = 'D';
		aline[1] = 'E';
	}
	/* Compute checksum */
	aline[79] = '\0'; 
	aline[79] = MenloCheckChar( aline);
	aline[80] = '\0'; 
	/* set internal variables from global */
        return 0;
}

int MenloCheckChar( char* pch )
{
	unsigned short sum;

	for( sum=0; *pch; pch++ )
	sum = ((sum&01)?0x8000:0) + (sum>>1) + *pch;

	return (36+sum%91);
}

/* determine closest station */
long closest_station ( dmin, orid)
	char *dmin;
	long orid;
{ 
	static Dbptr   dbo, dbtemp, dboj, dbojs, dbojss;
	static Dbptr   dbo_ig, dbtemp_ig, dboj_ig, dbojs_ig, dbojss_ig;
	static Tbl     *sortkeys;
	char expr[100];
	long n;
	double  min_delta, min_dist;
	long int_min_dist;
	char station[5];
	
	strcpy(dmin,"    ");
	if((long)ignore_db == (long)0) {
		dbo = dblookup(dbevent, 0, "origin", 0, 0);
		dbtemp = dblookup( dbevent, 0, "assoc", 0, 0 );
		dboj = dbjoin( dbo, dbtemp, 0,0,0,0,0 );
		/* subset view by current orid*/
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbojs = dbsubset (dboj, expr, 0);
		dbquery( dbojs, dbRECORD_COUNT, &n );
	} else
	  {
		dbo_ig = dblookup(dbevent_ig, 0, "origin", 0, 0);
		dbtemp_ig = dblookup( dbevent_ig, 0, "assoc", 0, 0 );
		dboj_ig = dbjoin( dbo_ig, dbtemp_ig, 0,0,0,0,0 );
		/* subset view by current orid*/
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbojs_ig = dbsubset (dboj_ig, expr, 0);
		dbquery( dbojs_ig, dbRECORD_COUNT, &n );

	}
	/* find smallest delta */
	if ( (long)n > (long)0) { 
		sortkeys = strtbl("delta", 0 ) ;
		if((long)ignore_db == (long)0) {
			dbojss = dbsort(dbojs,sortkeys,0,0);
			freetbl(sortkeys, 0);
			dbojss.record = 0;
			dbgetv(dbojss, 0, "delta", &min_delta, "sta", &station, NULL);
		} else
		  {
			dbojss_ig = dbsort(dbojs_ig,sortkeys,0,0);
			freetbl(sortkeys, 0);
			dbojss_ig.record = 0;
			dbgetv(dbojss_ig, 0, "delta", &min_delta, "sta", &station, NULL);
		}

		if ((double) min_delta == (double) -1.0) {
			strcpy(dmin,"    ");	
		} else
	       	  {
			min_dist = (double) min_delta * (double) 111.12;
			int_min_dist = (long) (((double)min_dist * (double)10.0) + (double).5); /* kilometer * 10 */
			if ((long)int_min_dist > (long)9999) {
				strcpy(dmin,"    ");	
			} else
			  {	
				sprintf(dmin,"%04ld",int_min_dist);
			}
		}
	}
	if((long)ignore_db == (long)0) {
		dbfree(dbojs); dbfree(dboj); 
		if((long)n > (long)0 ) {
			dbfree(dbojss); 
		}
	} else
	  {
		dbfree(dbojs_ig); dbfree(dboj_ig); 
		if((long)n > (long)0 ) {
			dbfree(dbojss_ig); 
		}
	}
	/* do not free object views 20080331*/	
	return(0);
}

/* determine seismic gap */
long determine_seismic_gap(gp,orid)
	char *gp;
	long orid;
{	
	static Dbptr   dboa, dbos, dboss;
	static Dbptr   dboa_ig, dbos_ig, dboss_ig;
	static Tbl     *sortkeys;
	char expr[100];
	long dbx;
	long n;
	double prev_esaz, esaz, largest_gap, gap, first_gap, last_gap;
	char station[5];
	long int_largest_gap;

	strcpy(gp,"  ");
	if((long)ignore_db == (long)0) {
		dboa = dblookup(dbevent, 0, "assoc", 0, 0);
		/* dbtemp = dblookup( dbevent, 0, "assoc", 0, 0 ); */
		/* dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 ); */
		/* subset view by current orid*/
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbos = dbsubset (dboa, expr, 0);
		dbquery( dbos, dbRECORD_COUNT, &n );
	} else
	  {
		dboa_ig = dblookup(dbevent_ig, 0, "assoc", 0, 0);
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbos_ig = dbsubset (dboa_ig, expr, 0);
		dbquery( dbos_ig, dbRECORD_COUNT, &n );

	}
	if (  (long)n > (long)0) { 
		/* sort by azimuth - esaz */
		/* sortkeys = newtbl(1);
		* pushtbl(sortkeys,"esaz");
		*/
		sortkeys = strtbl("esaz", 0 ) ;
		if((long)ignore_db == (long)0) {
			dboss = dbsort(dbos,sortkeys,0,0);
		} else
		  {
			dboss_ig = dbsort(dbos_ig,sortkeys,0,0);
		}
		freetbl(sortkeys, 0);


		largest_gap = 0;
		for( dbx = 0; (long) dbx < (long) n; dbx++ ) {
			if((long)ignore_db == (long)0) {
				dboss.record = dbx;
			} else
			  {
				dboss_ig.record = dbx;
			}
	
			/* if first record*/
			if ((long)dbx == (long)0) {
				if((long)ignore_db == (long)0) {
					dbgetv(dboss, 0, "esaz", &prev_esaz, NULL);
				} else
				  {
					dbgetv(dboss_ig, 0, "esaz", &prev_esaz, NULL);
				}
				first_gap = prev_esaz;
			}
			if((long)ignore_db == (long)0) {
				dbgetv(dboss, 0, "esaz", &esaz, "sta", &station, NULL);
			} else
			  {
				dbgetv(dboss_ig, 0, "esaz", &esaz, "sta", &station, NULL);
			}
			/* check for null values */
			/* if (esaz == -999.0) {
     	  	         	complain( 2, "no esaz entry for orid %ld station %4s\n", orid, station);
   			} */ 
			gap = esaz - prev_esaz;
			if (gap > largest_gap) largest_gap = gap;
			/* special case when crossing 0 */
			if ((long)dbx == (long)n-1) {
				last_gap = 360 - esaz;
				gap = last_gap + first_gap;
				if (gap > largest_gap)
				largest_gap = gap;
			}
			prev_esaz = esaz;
		}
		if (largest_gap <= 0.0) {
			strcpy(gp,"  ");
		} else
		  {
			int_largest_gap = (largest_gap / 3.6) + .5 ;
			if ((long)int_largest_gap > (long)99) {
				strcpy(gp,"  ");
			} else
			  {
				sprintf(gp,"%02ld", int_largest_gap);
			}
		}
	}
	if((long)ignore_db == (long)0) {
		dbfree(dboss); dbfree(dbos);
	} else
	  {
		dbfree(dboss_ig); dbfree(dbos_ig);
	}
	return(0);
}

/* determine number of stations used in solution */
long determine_number_sta_used_sol(nst,orid)
	char *nst;
	long orid;
{	
	static Dbptr   dbo, dbos, dboss;
	static Dbptr   dbo_ig, dbos_ig, dboss_ig;
	static Tbl     *sortkeys;
	char expr[100];
	long nsta,dbx;
	long n;
	char station[5],prev_station[5];

	strcpy(nst,"   ");
	if((long)ignore_db == (long)0) {
		dbo = dblookup(dbevent, 0, "assoc", 0, 0);
		/* dbtemp = dblookup( dbevent, 0, "assoc", 0, 0 ); */
		/* dbo = dbjoin( dbo, dbtemp, 0,0,0,0,0 ); */
		/* subset view by current orid*/
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbos = dbsubset (dbo, expr, 0);
		/* subset view by timedef equal d */
		sprintf(expr, "(timedef == \"d\")");
		dbquery( dbos, dbRECORD_COUNT, &n );
	} else
	  {
		dbo_ig = dblookup(dbevent_ig, 0, "assoc", 0, 0);
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbos_ig = dbsubset (dbo_ig, expr, 0);
		sprintf(expr, "(timedef == \"d\")");
		dbquery( dbos_ig, dbRECORD_COUNT, &n );
	}
	if ( (long)n > (long)0) { 
		/* sort by sta */
		sortkeys = strtbl("sta", 0 ) ;
		if((long)ignore_db == (long)0) {
			dboss = dbsort(dbos,sortkeys,0,0);
		} else
		  {
			dboss_ig = dbsort(dbos_ig,sortkeys,0,0);
		}
		freetbl(sortkeys, 0);
		nsta=0;
		for( dbx = 0; (long) dbx < (long) n; dbx++ ) {
			if((long)ignore_db == (long)0) {
				dboss.record = dbx;
			} else
			  {
				dboss_ig.record = dbx;
			}
			/* if first record*/
			if((long)ignore_db == (long)0) {
				dbgetv(dboss, 0, "sta", &station, NULL);
			} else
		          {
				dbgetv(dboss_ig, 0, "sta", &station, NULL);
			}
			if ((long)dbx == (long)0) {
				++nsta;
				strcpy(prev_station, station);
			} else
			  {
				if (strcmp(prev_station, station) != 0) {
					++nsta;
					strcpy(prev_station, station);
				}
			}	
		}
		if ((long)nsta == (long)0 || (long)nsta > (long) 999) {
			strcpy(nst,"   ");
		} else
		  {
			sprintf(nst,"%03ld",nsta);
		}
	}
	if((long) ignore_db == (long) 0) {
		dbfree(dboss); dbfree(dbos);
	} else
	  {
		dbfree(dboss_ig); dbfree(dbos_ig);
	}
	return(0);
}

/* determine determine_erho_erzz */
long determine_erho_erzz_rmss(erho, erzz, rmss, orid)
	char *erho, *erzz, *rmss;
	long orid;
{
	static Dbptr   dbl, dbtemp, dblj, dbljs;
	static Dbptr   dbl_ig, dbtemp_ig, dblj_ig, dbljs_ig;
	double  seh, sez;
	long int_seh, int_sez, int_sdobs;
	long nrows;
	char expr[100];
	double smajax, sdepth, sdobs;

	seh=0.0; sez=0.0;
	if((long) ignore_db == (long) 0) {
		dbl = dblookup(dbevent, 0, "origin", 0, 0);
		dbtemp = dblookup( dbevent, 0, "origerr", 0, 0 );
		dblj = dbjoin( dbl, dbtemp, 0,0,0,0,0 );
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbljs = dbsubset (dblj, expr, 0);
		dbquery(dbljs,dbRECORD_COUNT, &nrows);
	} else
	  {
		dbl_ig = dblookup(dbevent_ig, 0, "origin", 0, 0);
		dbtemp_ig = dblookup( dbevent_ig, 0, "origerr", 0, 0 );
		dblj_ig = dbjoin( dbl, dbtemp_ig, 0,0,0,0,0 );
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbljs_ig = dbsubset (dblj_ig, expr, 0);
		dbquery(dbljs_ig,dbRECORD_COUNT, &nrows);
	}
	if ( (long) nrows <  (long) 1) {
		strcpy(erho, "    ");
		strcpy(erzz, "    ");
		strcpy(rmss, "    ");
	} else
	  {
		if((long) ignore_db == (long) 0) {
			dbljs.record = 0;
			dbgetv( dbljs, 0, "smajax", &smajax,
					"sdepth", &sdepth, 
					"sdobs", &sdobs, NULL); 
		} else
		  {
			dbljs_ig.record = 0;
			dbgetv( dbljs_ig, 0, "smajax", &smajax,
					"sdepth", &sdepth, 
					"sdobs", &sdobs, NULL ); 
		}
		seh = smajax * .6594;
		sez = sdepth;
		int_seh = (seh * 10.0) + .5;
		int_sez = (sez * 10.0) + .5;
		int_sdobs = (sdobs * 100.0) + .5;
		if (smajax == 0.0 || (long) int_seh > (long) 9999) {
			strcpy(erho,"    ");
		} else
		  {
			sprintf(erho,"%04ld",int_seh);
		}
		if (sdepth == -1.0 || (long) int_sez > (long) 9999 ) {
			strcpy(erzz,"    ");
		} else
		  {
			sprintf(erzz,"%04ld",int_sez);
		}
		if (sdobs == -1.0 || (long) int_sdobs > (long) 9999 ) {
			strcpy(rmss,"    ");
		} else
		  {
			sprintf(rmss,"%04ld",int_sdobs);
		}
	}
	if((long) ignore_db == (long) 0 ) {
		dbfree(dbljs); dbfree(dblj);
	} else
	  {
		dbfree(dbljs_ig); dbfree(dblj_ig);
	}
	return(0);
}
/* determine determine_nm_em */
long determine_nm_em(nm, em, orid)
	char *nm, *em;
	long orid;
{
	static Dbptr   dbl, dbtemp, dblj, dbljs;
	static Dbptr   dbl_ig, dbtemp_ig, dblj_ig, dbljs_ig;
	long int_em;
	long nrows, nsta;
	double uncertainty;
	char expr[100];
	if((long) ignore_db == (long) 0) {
		dbl = dblookup(dbevent, 0, "origin", 0, 0);
		dbtemp = dblookup( dbevent, 0, "netmag", 0, 0 );
		dblj = dbjoin( dbl, dbtemp, 0,0,0,0,0 );
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbljs = dbsubset (dblj, expr, 0);
		dbquery(dbljs,dbRECORD_COUNT, &nrows);
	} else
	  {
		dbl_ig = dblookup(dbevent_ig, 0, "origin", 0, 0);
		dbtemp_ig = dblookup( dbevent_ig, 0, "netmag", 0, 0 );
		dblj_ig = dbjoin( dbl_ig, dbtemp_ig, 0,0,0,0,0 );
		sprintf(expr, "(orid == \"%ld\")", orid);
		dbljs_ig = dbsubset (dblj_ig, expr, 0);
		dbquery(dbljs_ig,dbRECORD_COUNT, &nrows);
	}
	if ( (long) nrows <  (long) 1) {
		strcpy(nm, "  ");
		strcpy(em, "  ");
	} else
	  {
		if((long) ignore_db == (long) 0) {
			dbljs.record = 0;
			dbgetv( dbljs, 0, "nsta", &nsta,
				"uncertainty", &uncertainty, NULL );
		} else
		  {
			dbljs_ig.record = 0;
			dbgetv( dbljs_ig, 0, "nsta", &nsta,
				"uncertainty", &uncertainty, NULL );
		}
		if ( (long) nsta ==  (long) -1 || (long) nsta >  (long) 99) {
			strcpy(nm, "  ");
		} else
		  {
			sprintf(nm,"%02ld", nsta);
		}
		if (uncertainty == -1.0) {
			strcpy(em, "  ");
		} else
		  {	
			int_em = (uncertainty * 10.0) + .5;
                        if((long) int_em > (long) 99) {
				strcpy(em, "  ");
			} else
			  {
				sprintf(nm,"%02ld", int_em);
			}
		}
	}
	if((long) ignore_db == (long) 0) {
		dbfree(dbljs); dbfree(dblj);
	} else
	  {
		dbfree(dbljs_ig); dbfree(dblj_ig);
	}
	return(0);
}
