
# Quick shakemap-triggering script in rt system
# test script; not for operational use
#               ^^^^^^^^^^^^^^^^^^^^^
# Kent Lindquist
# Lindquist Consulting
# Feb. 2005

use lib "$ENV{ANTELOPE}/data/perl" ;
use Datascope ;
use orb;
require "getopts.pl" ;
 
$HARDWIRE_orb2dbt_sync_sleeptime = 5;

elog_init( $0, @ARGV );

if ( ! &Getopts('1ds:') || @ARGV != 2 ) { 

	my $pgm = $0 ; 
	$pgm =~ s".*/"" ;
	elog_die ( "Usage: $pgm [-1d] [-s seekposition] orbname shakemap_bindir\n" ) ; 

} else {
	
	$shakemap_bindir = pop( @ARGV );
	$orbname = pop( @ARGV );
}

if( ! -d "$shakemap_bindir" ) {
	
	elog_die( "Can't find directory '$shakemap_bindir'. Bye!\n" );

} else {
	
	chdir( $shakemap_bindir ) || 
	    elog_die( "Can't change to dir '$shakemap_bindir'. Bye!\n" );;
}

$orb = orbopen( $orbname, "r" );

if( $orb < 0 ) {

	elog_die( "Failed to open orbserver '$orbname'. Bye!\n" );
}

orbselect( $orb, "/db/origin" );

# $pktid = orbposition( $orb, "2/25/05 22:30" );	# SCAFFOLD 
$seekposition = $opt_s ? $opt_s : "ORBNEXT" ;
$pktid = orbseek( $orb, $seekposition );

for( ;; ) {

	( $pktid, $srcname, $time, $packet, $nbytes ) = orbreap( $orb );

	if ( ! defined $pktid ) {
		next;
	}
	&showPkt($pktid, $srcname, $time, $packet, $nbytes, 4) if $opt_d;

	sleep( $HARDWIRE_orb2dbt_sync_sleeptime );

	( $rc, $pkt ) = unstuffPkt( $srcname, $time, $packet, $nbytes );	

	if( $rc != "Pkt_db" ) {

		elog_complain( "Unstuffed packet is not a database row!! Skipping\n" );
		next;
	}

	@db = $pkt->db; 

	$evid = dbgetv( @db, "evid" ) . "\n" ;

	elog_notify( "$pgm: starting ShakeMap run for evid $evid\n" );

	system( "./shake -event $evid" );

	elog_notify( "$pgm: finished with ShakeMap run for evid $evid\n" );

	if( $opt_1 ) {
		# Only process one event
		exit(0);
	}
}

