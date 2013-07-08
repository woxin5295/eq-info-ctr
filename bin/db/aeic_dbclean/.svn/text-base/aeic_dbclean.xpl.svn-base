# aeic_dbclean
#
# Kent Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# February, 2000

sub announce {
	( $task ) = @_;

	printf STDERR "\n$task:\n";
	$underline = length( $task ) + 1;
	while( $underline-- ) {
		printf STDERR "-";
	}
	printf STDERR "\n";
}

use lib "$ENV{ANTELOPE}/data/perl" ;

require "getopts.pl" ;
 
if ( ! &Getopts('v') || @ARGV != 1 ) {
	die ( "Usage: $0 [-v] database\n" ) ; 
} else {
	$dbname = $ARGV[0];
	if( $opt_v ) {
		$v = "-v";
	} else {
		$v = "";
	}
}

use Datascope ;

announce( "Checking for appropriate record length" );
if( system( "dbcheck $v $dbname" ) ) {
	die( "aeic_dbclean: $dbname failed dbcheck\n" );
}

@db = dbopen( $dbname, "r" );

announce( "Deleting iphase == del from arrival table" );
system( "dbsubset $v $dbname.arrival \"iphase == 'del'\" | dbdelete $v -" );

announce( "Crunching null rows" );
system( "dbcrunch $v $dbname" );

announce( "Removing origins with null primary keys" );
system( "dbsubset $v $dbname.origin \" lat == NULL && lon == NULL && time == NULL\" | dbdelete $v - origin" );

announce( "Removing non-preferred origins" );
system( "dbjoin $v $dbname.event origin | dbsubset $v - \"orid != prefor\" |\
dbdelete $v - origin" );

announce( "Removing unnecessary origerr rows" );
system( "dbnojoin $v $dbname.origerr origin | dbdelete $v - origerr" );

announce( "Removing assoc rows with no hypocenters" );
system( "dbnojoin $v $dbname.assoc origin | dbdelete $v - assoc" );

announce( "Removing unassociated arrivals" );
system( "dbnojoin $v $dbname.arrival assoc | dbdelete $v - arrival" );

announce( "Removing predarr rows with no hypocenters" );
system( "dbnojoin $v $dbname.predarr origin | dbdelete $v - predarr" );

announce( "Removing unassociated predicted arrivals" );
system( "dbnojoin $v $dbname.predarr assoc | dbdelete $v - predarr" );

announce( "Removing duplicate predarr entries" );
system( "dbsort -ou $dbname.predarr arid time" );

announce( "Removing unassociated netmag entries" );
system( "dbnojoin $v $dbname.netmag origin | dbdelete $v - netmag" );

announce( "Removing unassociated stamag entries" );
system( "dbnojoin $v $dbname.stamag netmag | dbdelete $v - stamag" );

announce( "Removing wfmeas entries with no corresponding arrivalsl" );
system( "dbnojoin $v $dbname.wfmeas arrival | dbdelete $v - wfmeas" );

announce( "Removing duplicate wfmeas entries" );
system( "dbsort -ou $dbname.wfmeas sta chan meastype filter time tmeas arid" );

announce( "Sorting origin table by time" );
system( "dbsort -o $dbname.origin time" );

announce( "Fixing orids" );
system( "dbfixids $dbname orid" );

announce( "Sorting event table by prefor" );
system( "dbfixids $dbname evid" );

announce( "Fixing magids" );
system( "dbfixids $dbname magid" );

announce( "Sorting stamag table by magid" );
system( "dbsort -o $dbname.stamag magid" );

announce( "Sorting origerr table by orid" );
system( "dbsort -o $dbname.origerr orid" );

announce( "Sorting arrival table by time" );
system( "dbsort -o $dbname.arrival time" );

announce( "Fixing arids" );
system( "dbfixids $dbname arid" );

announce( "Sorting assoc table by orid and arid" );
system( "dbsort -o $dbname.assoc orid arid" );

announce( "Synching chanids where possible" );
if( $v ) { $q = ""; } else { $q = "-q"; }
system( "dbfixchanids $q $dbname" );

exit( 0 );
