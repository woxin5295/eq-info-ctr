#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <sys/file.h>
#include <signal.h>

#include "stock.h"
#define MAX_BLOCK_SIZE 1200

/* static char killproc_command[] = "/usr/tools/bin/killproc"; */
static char killproc_command[] = "/usr/seis_apps/arrays2orb/killproc_ttya";
static char read_ttya_command[] = "/usr/tools/bin/read_ttya"; 

void get_next_block(block, length) 
	unsigned char *block;
	int *length;
{
	char	command[STRSZ];
        static FILE *fp;
        static FILE *pp;
        static int first = 1;
	static unsigned char block_head[5];
	pid_t	rsh_pid;
	int	status;
	int	pipedes[2];
	int	max_block_size;

	max_block_size = MAX_BLOCK_SIZE - 1;  
        if( first )
        {
		sprintf( command, "%s \\\"%s\\\"",
				killproc_command,
				read_ttya_command );
		system( command );
		/* sleep 120 seconds so kill has a chance to kill read_ttya */
		sleep(120); 

		pipe( pipedes );

		if( ( rsh_pid = fork() ) == 0 ) {
			close( 1 );
			dup( pipedes[1] );
			close( pipedes[0] );
			close( pipedes[1] );

			execlp( "/usr/bin/sh", "/usr/bin/sh", "-c",
				read_ttya_command, NULL );

		} else {
			close( pipedes[1] );
			fp = fdopen( pipedes[0], "r" );
		}

                first = 0;
        	fread( block, 5, sizeof( char ), fp );
        	while( ! match_end( block ) )
        	{
               		memmove( block, block + 1, 4 );
               		fread( &block[4], 1, sizeof( char ), fp );
        	}
        }
	else
	{
		memcpy( &block[0], &block_head[0], 5 );
	}

        *length = 5;

        fread( &block[++(*length) - 1], 1, sizeof( char ), fp );
        while( ! match_end( block + *length - 4 - 1 ) )
        {
                fread( &block[++(*length) - 1], 1, sizeof( char ), fp );
		if ( *length == max_block_size ) {
		  memmove( block + 5, block + 6, max_block_size - 5 );
		  --(*length);
		}
        }

        *length -= 4;

	memcpy( &block_head[0], &block[*length-1], 5 );
}

int match_end( char *buf )
{
	if( buf[0] != 0x01 && buf[0] != 0x02 ) return 0;
	if( buf[0] != buf[1] ) return 0;
	if( buf[2] != 0x01 ) return 0;
	if( buf[3] != 0x00 ) return 0;
	if( buf[4] != 0x01 ) return 0;
	return 1;
}
