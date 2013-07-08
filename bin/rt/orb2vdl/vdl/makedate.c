#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
	char *month[]={"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",
		"Sep","Oct","Nov","Dec","BAD"};
main()
{
	FILE *fp;
	int format,day,tick;
	time_t time();
	time_t  clock;
	struct tm *tm2;
 	char line[100];
	int ierr,iyr,imon;
	fp=fopen("datetime.h","w");
	time(&clock);
	tm2=gmtime(&clock);
/*	printf("day=%d mon=%d yr=%d\n",tm2->tm_mday,tm2->tm_mon,tm2->tm_year);*/
	fprintf(fp,"\tstatic char compdate[]=%c%2d-%3s-%2d %02d:%02d:%02d%c;\n",
		'\042',tm2->tm_mday,month[tm2->tm_mon],tm2->tm_year,
		tm2->tm_hour,tm2->tm_min,tm2->tm_sec,'\042');
	fclose(fp);
	exit(0);
}
	
	

