#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "stock.h"
#include "elog.h"
#include "expect.h"
#include "pf.h"

#define LOGFILE "orb2aeic_beeper_expect_log"
#define DEFAULT_STRING "orb2aeic_beeper: unexpected Expect problem. Bye.\n"
#define TIMEOUT_STRING "orb2aeic_beeper: Kermit connection timed out. Bye.\n"
#define USAGE "Usage: orb2aeic_beeper [-d] [-v] [-p parameter_file] orbname\n"
#define PFNAME_DEFAULT "orb2aeic_beeper"
#define MAILCMD "rtmail"

#define SUCCESS 1
#define NOMODEM 2
#define INUSE 3
#define ATCONN 4
#define CONNECTED 5
#define IDEQUALS 6
#define NOCARRIER 7
#define PAGERID_REQ 8
#define MESSAGE_REQ 9
#define PAGE_ACCEPTED 10
#define ID_NOT_VALID 11
#define MODEM_BUSY 12

int myexp_send( int, char * );
int send_radio_fairbanks( Pf *, Pf *, char ** );
void clean_phone_number( char *, char * );

static int opt_v = 0;
static int opt_d = 0;

int 
myexp_send( int kermitfd, char *msg )
{
	char 	*mycopy;
	int	len;
	int	i;

	mycopy = strdup( msg );
	len = strlen( mycopy );

	for( i = 0; i < len - 1; i++ ) {
		if( *( mycopy + i ) == '\r' ) *( mycopy + i ) = '\n'; 
	}

	sleep( 1 );

	write( kermitfd, mycopy, len );

	sleep( 1 );

	free( mycopy );
}

void
close_kermit( int kermitfd ) 
{
	int	child_pid;
	int	retry_wait = 5;
	int	status;

	close( kermitfd );

	child_pid = waitpid( exp_pid, &status, WNOHANG );
	if( child_pid != exp_pid && retry_wait-- ) {
		sleep( 1 );	
		child_pid = waitpid( exp_pid, &status, WNOHANG );
	}
}
	
int /* < 0: failed. 0: complete success. >0: page accepted, cleanup failed */

send_radio_fairbanks( Pf *config_pf, Pf *message_pf, char **result ) 
{
	int	accepted = 0;
	int	kermitfd;
	int	expval;
	int	speed;
	char	*kermit_path;
	char	*line;
	char	*modem;
	char	*pager_co_number;
	char	modem_number_clean[STRSZ];
	char	*pager_number;
	char	pager_number_clean[STRSZ];
	char	*message;
	char	kermit_cmd[STRSZ];
	char	result_string[STRSZ];
	int	rc;

	*result = NULL;

	if( opt_d ) {
		char	debug_file[FILENAME_MAX];

		sprintf( debug_file, "orb2aeic_beeper_%d", getpid() );

		exp_is_debugging = 1;
		exp_debugfile = fopen( debug_file, "w" );
	}

	exp_timeout = 5;

	if( opt_v ) {
		exp_logfile_all = 1;
        	exp_logfile = fopen( LOGFILE, "a" ); 
		if( exp_logfile == NULL ) {
			sprintf( result_string,
			 	"orb2aeic_beeper: failed to open log %s. Bye\n",
				LOGFILE );
			*result = strdup( result_string );
			if( opt_d ) fclose( exp_debugfile );
			return -1;
		}
	}

	kermit_path = pfget_string( config_pf, "kermit_path" );

        kermitfd = exp_spawnl( kermit_path, kermit_path, 0 );

	expval = exp_expectl( kermitfd,
			      exp_exact, "C-Kermit>", SUCCESS,
			      exp_end );
	switch( expval ) {
	case SUCCESS: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	line = pfget_string( config_pf, "line" );
	sprintf( kermit_cmd, "set line %s\r", line );
	myexp_send( kermitfd, kermit_cmd );

	expval = exp_expectl( kermitfd,
			      exp_exact, "/var/spool/locks: Permission denied", NOMODEM,
			      exp_exact, "Locked by process", INUSE,
			      exp_exact, "Device busy", MODEM_BUSY,
			      exp_exact, "C-Kermit>", SUCCESS,
			      exp_end );
	switch( expval ) {
	case SUCCESS: 
		break;
	case MODEM_BUSY:
		*result = strdup( "orb2aeic_beeper: failed to connect to modem.\n" );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	case INUSE:
		*result = strdup( "orb2aeic_beeper: failed to connect to modem.\n" );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	case NOMODEM: 
		sprintf( result_string, 
			 "orb2aeic_beeper: no modem on %s. Bye\n",
			 getenv( "HOST" ) );

		*result = strdup( result_string );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	modem = pfget_string( config_pf, "modem" );
	sprintf( kermit_cmd, "set modem %s\r", modem );
	myexp_send( kermitfd, kermit_cmd );

	expval = exp_expectl( kermitfd,
			      exp_exact, "C-Kermit>", SUCCESS,
			      exp_end );
	switch( expval ) {
	case SUCCESS: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	speed = pfget_int( config_pf, "speed" );
	sprintf( kermit_cmd, "set speed %d\r", speed );
	myexp_send( kermitfd, kermit_cmd );

	expval = exp_expectl( kermitfd,
			      exp_exact, "C-Kermit>", SUCCESS,
			      exp_end );
	switch( expval ) {
	case SUCCESS: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	myexp_send( kermitfd, "connect\r" );

	expval = exp_expectl( kermitfd,
			      exp_exact, "to see other options.", ATCONN,
			      exp_end );
	switch( expval ) {
	case ATCONN: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	exp_timeout = 60;

	pager_co_number = pfget_string( config_pf, "pager_company_modem_number" );
	clean_phone_number( pager_co_number, modem_number_clean );
	sprintf( kermit_cmd, "atdt%s\r", modem_number_clean );
	myexp_send( kermitfd, kermit_cmd );

	expval = exp_expectl( kermitfd,
			      exp_exact, "CONNECT 1200\r\n", CONNECTED,
			      exp_end );
	switch( expval ) {
	case CONNECTED: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	myexp_send( kermitfd, "\r" );

	expval = exp_expectl( kermitfd,
			      exp_exact, "NO CARRIER", NOCARRIER,
			      exp_exact, "ID=", IDEQUALS,
			      exp_end );
	switch( expval ) {
	case IDEQUALS:
		break;
	case NOCARRIER: 
		*result = strdup( "orb2aeic_beeper: No carrier. Bye.\n" );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	myexp_send( kermitfd, "PAGE\r" );

	expval = exp_expectl( kermitfd,
			      exp_exact, "Pager ID?", PAGERID_REQ,
			      exp_end );
	switch( expval ) {
	case PAGERID_REQ:
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	pager_number = pfget_string( config_pf, "pager_number" );
	clean_phone_number( pager_number, pager_number_clean );
	sprintf( kermit_cmd, "%s\r", pager_number_clean );
	myexp_send( kermitfd, kermit_cmd );

	expval = exp_expectl( kermitfd,
			      exp_exact, "Message:", MESSAGE_REQ,
			      exp_exact, "ID not valid.", ID_NOT_VALID,
			      exp_end );
	switch( expval ) {
	case MESSAGE_REQ:
		break;
	case ID_NOT_VALID:
		*result = strdup( "orb2aeic_beeper: pager id not valid\n" );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	message = pfget_string( message_pf, "message" );
	sprintf( kermit_cmd, "%s\r", message );
	myexp_send( kermitfd, kermit_cmd );

	expval = exp_expectl( kermitfd,
			      exp_exact, "Page accepted.", PAGE_ACCEPTED,
			      exp_end );
	switch( expval ) {
	case PAGE_ACCEPTED:
		accepted = 1;
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return -1;
	}

	expval = exp_expectl( kermitfd,
			      exp_exact, "Pager ID?", PAGERID_REQ,
			      exp_end );
	switch( expval ) {
	case PAGERID_REQ:
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	}

	myexp_send( kermitfd, "\r" ); 

	expval = exp_expectl( kermitfd,
			      exp_exact, "Thank you.", 1, 
			      exp_end );
	switch( expval ) {
	case 1:
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	}

	expval = exp_expectl( kermitfd,
			      exp_exact, "NO CARRIER", SUCCESS,
			      exp_end );
	switch( expval ) {
	case EXP_TIMEOUT:
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	case SUCCESS:
		break;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	}

	myexp_send( kermitfd, "ath\r" ); 

	expval = exp_expectl( kermitfd,
			      exp_exact, "OK", SUCCESS, 
			      exp_end );
	switch( expval ) {
	case SUCCESS:
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	}

	myexp_send( kermitfd, "\034C" ); 

	expval = exp_expectl( kermitfd,
			      exp_exact, "C-Kermit>", SUCCESS,
			      exp_end );
	switch( expval ) {
	case SUCCESS: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	}

	myexp_send( kermitfd, "quit\r" );

	expval = exp_expectl( kermitfd,
			      exp_exact, "C-Kermit>", SUCCESS,
			      exp_end );
	switch( expval ) {
	case SUCCESS: 
		break;
	case EXP_EOF: 
		break;
	case EXP_TIMEOUT:
		*result = strdup( TIMEOUT_STRING ); 
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	default:
		*result = strdup( DEFAULT_STRING );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		close_kermit( kermitfd );
		return 1;
	}

	close_kermit( kermitfd );

	if( accepted ) {
		*result = strdup( "Page was accepted.\n" );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		return 0;
	} else {
		*result = strdup( "Unknown error in send_radio_fairbanks\n" );
		if( opt_d ) fclose( exp_debugfile );
		if( opt_v ) fclose( exp_logfile );
		return -1;
	}
}

void
clean_phone_number( char *in, char *out )
{
	int	len;
	int	i, j = 0;

	len = strlen( in );

	for( i = 0; i <= len; i++ ) {
		if( isdigit( in[i] ) ) out[j++] = in[i];
	}
	out[j] = '\0';
}

int
cmp_string( void *ap, void *bp, void *private )
{
	char	*a = *((char **) ap);
	char	*b = *((char **) bp);

	return strcmp( a, b );
}

void
handle_callback( Pf *message_pf, char *orbname, char *result )
{
	char	*recipients;
	char	*mailkey = "mail";
	char	*orbmsgkey = "orbmsg";
	char	cmd[STRSZ];
	char	tempfile[FILENAME_MAX];
	char	srcname[STRSZ];
	char	*msg_name;
	Tbl	*keys;
	int	ns, ne;
	int	nbytes;
	FILE	*fp;
	int	orbfd;
	int	rc;

	keys = pfkeys( message_pf );

	searchtbl( &mailkey, keys, cmp_string, 0, &ns, &ne );
	if( ns > ne ) {

		if( opt_v ) fprintf( stderr,
				"\nNo mail callback specified.\n\n" );
	
	} else {

		recipients = pfget_string( message_pf, "mail" );

		sprintf( tempfile, "/tmp/callback_%d", getpid() );
		fp = fopen( tempfile, "w" );
		fprintf( fp, "%s", result );
		fclose( fp );

		sprintf( cmd, "%s -s \"%s\" %s < %s", 
			MAILCMD,
			"Pager result",
			recipients,
			tempfile );
		system( cmd );

		unlink( tempfile );

	}

	searchtbl( &orbmsgkey, keys, cmp_string, 0, &ns, &ne );
	if( ns > ne ) {

		if( opt_v ) fprintf( stderr,
				"\nNo orbmsg callback specified.\n\n" );
	
	} else {

		msg_name = pfget_string( message_pf, "orbmsg" );
		sprintf( srcname, "/msg/%s", msg_name );

		orbfd = orbopen( orbname, "w" );
		if( orbfd < 0 ) {
			complain( 1,
			"orb2aeic_beeper: callback connection to orb failed\n" );
			return;
		}

		nbytes = strlen( result );
		rc = orbput( orbfd, srcname, (double) time( 0 ),
				result, nbytes );

		if( rc ) {
			complain( 1, 
			"orb2aeic_beeper: callback to orb failed\n" );
		} else if( opt_v ) {
			fprintf( stderr, "Put callback message on %s as %s\n",
					orbname, srcname );
		}

		orbclose( orbfd );
	}

	return;
}

int
main( int argc, char **argv )
{
	Pf	*config_pf;
	Pf	*message_pf = 0;
	char	pfname[STRSZ];
	char	*pf_source_name;
	char	orbname[STRSZ];
	int	orbfd;
	int     pktid;
	char    srcname[STRSZ];
	double  mytime;
	char    *rawpkt = NULL;
	int     bufsize = 0;
	int	pktsize = 0;
	char	*result;
	extern char *optarg;
	extern int optind, opterr, optopt;
	int	c;
	int	rc;

	strcpy( pfname, PFNAME_DEFAULT );

	while( ( c = getopt( argc, argv, "vdp:" ) ) != -1 ) {
		switch( c ) {
		case 'v':
			opt_v = 1;
			break;
		case 'd':
			opt_d = 1;
			break;
		case 'p':
			strcpy( pfname, optarg );
			break;
		default:
			die( 1, USAGE );
			break;
		}
	}

	if( argc - optind != 1 ) {
		die( 1, USAGE );
	} else {
		strcpy( orbname, argv[argc-1] );
	}

	rc = pfread( pfname, &config_pf );
	if( rc == -1 ) {
		die( 1, "orb2aeic_beeper: error finding parameter file %s.\n",
			pfname );
	} else if( rc < -1 ) {
		die( 1, "orb2aeic_beeper: error reading parameter file %s.\n",
			pfname );
	}

	pf_source_name = pfget_string( config_pf, "source_name" );

	if( ( orbfd = orbopen( orbname, "r&" ) ) <= 0 ) {
		complain( 1, 
		  "orb2aeic_beeper: failed to open orb connection to %s\n",
			  orbname );
		sleep( 10 );
	}

	orbselect( orbfd, pf_source_name );

	for( ;; ) {

		if( opt_v )
		  fprintf( stderr,
			"\norb2aeic_beeper: Waiting for pager message:\n\n" );

		orbreap( orbfd, &pktid, srcname, &mytime, &rawpkt, 
			 &pktsize, &bufsize );

		orbpkt2pf( rawpkt, pktsize, &message_pf );

		if( opt_v ) fprintf( stderr, "Message:\n%s\n[End of Message]\n",
			pfget_string( message_pf, "message" ) );

		send_radio_fairbanks( config_pf, message_pf, &result ); 

		if( opt_v ) fprintf( stderr, "Result:\n%s\n\n", result );

		handle_callback( message_pf, orbname, result );

		if( result != NULL ) free( result );

		pffree( message_pf );
		message_pf = NULL;
	}
}
