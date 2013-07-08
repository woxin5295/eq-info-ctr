#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
FILE *logout;
int lunin;						/* file to read from */

int feedme_test()
{
	return 0;
}
int feedme_shutdown()
{	close(lunin);
}
int feedme_init(argc, argv)
int argc;
char **argv;
{	lunin = open("/export/home/ew/capture.dat", (O_RDONLY) );
	if( lunin < 0) fprintf(logout,"bad open on capture file...%d %d\n",lunin,errno);
	return 0;
}
int feedme(ich, ia, tc)
int * ich;
long *ia;
char *tc;
{	int nsamp, ierr;
	ierr = read(lunin, &nsamp,4);
	if(ierr <= 0) 
	{	fprintf(logout,"File read error=%d errno=%d\n",ierr,errno);
		exit(0);
	}
	read(lunin, ich, 4);
	read(lunin, tc, 8);
	read(lunin, ia, nsamp*4);
	*tc = *tc - 44;					/* convert to early year */
	if( nsamp % 2 != 0) 
	{	fprintf(logout,"***** odd # samples ich=%d naamp=%d\n",
		*ich,nsamp);
/*		nsamp = nsamp / 2 * 2;*/
	}
	return nsamp;
}
