#include <stdio.h>
#include <stdlib.h>
#include <errno.h>			/* error  number defs */
#include <signal.h>
#include "safetcp.h"

/*		Declare function prototypes     */
FILE *logout;
void init_tcp();
main()
{	struct tcpsocket tcpunit;
	int err;
	char buf[256];
	logout=stdout;
	err = init_safetcp(&tcpunit, "nsn4.cr.usgs.gov", "", 2009);
	if(err < 0)
	{	exit (1);
	}
	printf("Starting write...\n");
	err=writetcp(&tcpunit, "LKWY  ",6);
	printf("Write completed err=%d\n",err);
	
	for(;;)
	{	err=readtcp(&tcpunit, buf, 256);
		printf("Got %d bytes\n",err);
		if(err <= 0) exit(2);
	}
}
