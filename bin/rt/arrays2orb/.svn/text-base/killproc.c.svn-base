#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <string.h>

main( int argc, char **argv )
{
	prpsinfo_t p;
	int	retval;
	int	filedes;
	int	i;
	DIR	*dirp;
	struct dirent *direntp;
	char	procname[FILENAME_MAX];
	char	matchstring[PRARGSZ];
	int	length;
	int	matched = 0;
	pid_t	matched_pid = -1;	

	if( argc != 2 )
	{
		fprintf( stderr, "Usage(%d): %s 'command_line_to_match'\n",
				argc, argv[0] );
		exit( 1 );
	}
	else
	{
		length = strlen( argv[1] );
		length = length > PRARGSZ ? PRARGSZ : length;
		strncpy( matchstring, argv[1], length );
		matchstring[length] = '\0';
	}

	dirp = opendir( "/proc" );

	while( ( direntp = readdir( dirp ) ) != NULL )
	{
		if( strcmp( direntp->d_name, "." ) == 0 ) continue;
		if( strcmp( direntp->d_name, ".." ) == 0 ) continue;

		sprintf( procname, "/proc/%s", direntp->d_name );

		filedes = open( procname, O_RDONLY );
		if( filedes < 0 ) continue;

		retval = ioctl(filedes, PIOCPSINFO, (void *) &p);

		if( strcmp( p.pr_psargs, matchstring ) == 0 )
		{
			matched++;
			matched_pid = p.pr_pid;
			kill( matched_pid, SIGKILL );
		}
	}

	closedir( dirp );

	printf( "%d\n", matched );
}
