/* shakemysqlversionextract  */
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "xtra.h"
extern char *fgets();
extern pid_t wait();

int usage ( int i, char *message ) {
	fprintf(stderr, "Usage: %s -monitor mysqlrawdbversionfile -exec mysqlscript -filein shake_version_dumpfile -dbout outdb \n", Program_Name );
	switch (i) {
		case 1:fprintf(stderr,"%s is unknown option\n", message); break;
		case 2: fprintf(stderr,"%s \n", message); break;
		case 3: fprintf(stderr,"%s file can not be openned \n", message); break;
		case 4: fprintf(stderr,"%s database can not be openned \n", message); break;
	}
	exit(1); 
}


int monitor_file_changed(char *monitor)
{
	static int initialize = 1;
	static struct stat t;
	if (initialize == 1) {
		initialize = 0;	
		t.st_size = (off_t) 0;
		t.st_mtime = (time_t) 0;
	}
	struct stat table_statbuf;
	stat(monitor, &table_statbuf);
	if ( (off_t) table_statbuf.st_size != (off_t) t.st_size ||
		(time_t) table_statbuf.st_mtime != (time_t) t.st_mtime ) {
		t.st_mtime = (time_t) table_statbuf.st_mtime;
		t.st_size = (off_t) table_statbuf.st_size;
		return(1);
	} else
	  {
		return(0);
	} 
}

int main( int argc, char **argv )
{
	int len, first, start, cur, chansc, x, nrec;
	int set1, set2;
	char db_name[STRSZ], filein[STRSZ]; 
	char monitor[STRSZ], execpro[STRSZ];
	char    dir[FILENAME_MAX];
	static char base[FILENAME_MAX];
	FILE *fp;
	Dbptr dbout;
	char mysqlline[1024];
	char evid_my[81]; int evid_ant;
	char lddate_my[25]; double lddate_ant, currentlddate, biglddate; 
	char comment_my[256];  
	char ver_my[12]; int ver_ant, nrecs;
	char *forkprogargs[4];
	int whileloop, onerun, inputchange, sleeptime, statusp, pid;
	
	

	dirbase( argv[0], dir, base );
	Program_Name = base;
	onerun = 0;
	sleeptime = 5; /* 5 seconds */
	/* argument list */
	set1 = 0;
	for (x=1; x < argc; ++x) {
		set2 = 0;
		if( strcmp("-dbout", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				++set1;++set2;
				(void) strcpy(db_name,argv[x]);
			}
		}
		if( strcmp("-filein", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				set1 = set1 + 10; ++set2;
				(void) strcpy(filein,argv[x]);
			}
		}
		/* monitor this file it indicate changes in mysql file */
		if( strcmp("-monitor", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				set1 = set1 + 100; ++set2;
				(void) strcpy(monitor,argv[x]);
			}
		}
		if( strcmp("-exec", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				set1 = set1 + 1000; ++set2;
				(void) strcpy(execpro,argv[x]);
			}
		}
		if( strcmp("-sleep", argv[x]) == 0 ) { 
			++x;
			if (x < argc) { 
				++set2;
				sleeptime = atoi(argv[x]);
			}
		}
		if( strcmp("-onerun", argv[x]) == 0 ) { 
			onerun = 1;
			++set2;
		}
		if (set2 == 0) {
			usage (1, argv[x]);
		}
	}
	if (set1 != 1111 ) {
		usage ( 2, "Required options were not provided");
	}
	forkprogargs[0] = execpro;
	forkprogargs[1] = execpro;
	forkprogargs[2] = filein;
	forkprogargs[3] = NULL;
	/* open output database */
	if(dbopen( db_name, "r+", &dbout ) != 0 ) {	
		usage ( 3, db_name);
	}
	dbout = dblookup( dbout, 0, "shakeversion", 0, 0 );
	dbquery (dbout, dbRECORD_COUNT, &nrec);
	currentlddate = (double)0.0;
	if (nrec > 0 ) {
		for(dbout.record=0; dbout.record < nrecs; ++dbout.record) {
			if(dbgetv(dbout, 0, "lddate", &lddate_ant, 0) == dbINVALID) {
				fprintf(stderr,"dbgetv problem loading lddate record=%d \n", dbout.record);
			} else
			  {
				if((double)currentlddate < (double)lddate_ant ) {
					currentlddate = (double) lddate_ant;
				}
			}
		}
	}
	dbclose(dbout); 

	whileloop = 1;
	while(whileloop == 1) {
		if(onerun == 1) whileloop = 0;
		inputchange = 0;
		while (inputchange == 0) {
			sleep(sleeptime);
			inputchange = monitor_file_changed(monitor);
			/* if(inputchange == 1) {
			*	fprintf(stderr,"modification detected at %s \n", strtime(now()));
			*} */
		}
		/* create input file with */
		/* create child process  and start fork */
		pid = fork();
		/* child process */
		if (pid == 0) {
			execvp(forkprogargs[0], &forkprogargs[1]);
		 } else if (pid == -1) {
			fprintf(stderr,"Can not fork %s \n", forkprogargs[0]);
		/* parent */
		} else if (pid > 0) {
			(void) wait(&statusp);
		}

		/* load db */

		/* read filein */
		if ((fp = fopen(filein, "r")) == NULL) {
			fprintf(stderr,"Can not open file %s \n", filein);
		}
		biglddate = (double) 0.0;
		while ((fgets(mysqlline, 1024, fp)) != NULL) {
			len = strlen(mysqlline);
			evid_my[0]=ver_my[0]=lddate_my[0]=comment_my[0]='\0';
			first = 1;
			start = 0;
			cur = 1;
			while (cur < len) {
				if(mysqlline[cur] == '\t' || mysqlline[cur] == '\n') {
					mysqlline[cur] = '\0';
					switch (first) {
						case 1: /* evid */
							(void)strcpy(evid_my, &mysqlline[start]);
						break;
						case 2: /* ver */
							(void)strcpy(ver_my, &mysqlline[start]);
						break;
						case 3: /* lddate */
							(void)strcpy(lddate_my, &mysqlline[start]);
						break;
						case 4: /* comment */
							(void)strcpy(comment_my, &mysqlline[start]);
						break;
					}
					start = cur + 1;
					++first;
				}
				++cur;
			}
			/* only use numeric event ids */
			chansc = 0;
			for(x = 0; x < strlen(evid_my); ++x) {
				if(evid_my[x] < '0' || evid_my[x] > '9') {
					chansc = 1; x = strlen(evid_my);
				} 
			}
			if (chansc == 0 ) {
				evid_ant = (int) atoi(evid_my); 
				ver_ant = (int) atoi(ver_my);
				lddate_ant = (double) str2epoch(lddate_my);
				if((double) biglddate < (double) lddate_ant) {
					biglddate = (double) lddate_ant;
				}
				/* add row to database */
				if ( (double) currentlddate < (double) lddate_ant ) { 
					if(dbopen( db_name, "r+", &dbout ) == 0 ) {
						dbout = dblookup( dbout, 0, "shakeversion", 0, 0 );
						dbout.record = dbaddnull (dbout);
						dbquery (dbout, dbRECORD_COUNT, &nrec);
						/* evid mysqlvernum mysqlcomment lddate  */
						if(dbout.record < 0 || dbputv(dbout, 0,
							"evid", evid_ant,
							"mysqlvernum", ver_ant,
							"mysqlcomment", comment_my,
							"lddate", lddate_ant, 0) == dbINVALID) { 
							fprintf(stderr,"dbputv: did not load evid=%d, ver=%d \n", 
								evid_ant,ver_ant);
						}
					}
				}
				dbclose(dbout);
			}
		}
		fclose(fp);
		if((double) currentlddate < (double) biglddate) {
			currentlddate = (double) biglddate;
		}  
	}
}
