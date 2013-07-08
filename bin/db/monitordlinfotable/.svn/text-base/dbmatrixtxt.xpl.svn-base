
require "getopts.pl" ;
 
if ( ! &Getopts('v') || @ARGV != 2 ) { 
    die ( "Usage: $0 [-v] database message \n" ) ; 
}

use Datascope ;

$database = shift ;
$message = shift ;
chomp($hostname = `uname -n`) ;

@db = dbopen ( $database, "r" ) ;
@db = dblookup ( @db, 0, "a_matrix", 0, 0 ) ; 

$n = dbquery ( @db, "dbRECORD_COUNT") ; 
print STDERR "starting with $n records\n" if $opt_v ; 

printf ( "%s current matrix: %s\n\n", $hostname, $message );

printf ( "sta\tstatus\tmaint\tlow\tmed\thigh\ttest\tlddate\n" );
 
for ( $db[3] = 0 ; $db[3] < $n ; $db[3]++ ) {
	($p_tagid, $p_status, $p_maint, $p_low, $p_med, $p_high, $p_test, $lddate ) = 
		dbgetv ( @db, qw( p_tagid p_status p_maint p_low p_med p_high p_test lddate )) ;
	if ( $p_tagid < 13 ) { 
		printf ( "PS%2.2d\t%d\t%d\t%d\t%d\t%d\t%d\t%s\n", 
		    $p_tagid, $p_status, $p_maint, $p_low, $p_med, $p_high, $p_test, &strtime($lddate) );
	} else
	  {
		printf ( "VMT \t%d\t%d\t%d\t%d\t%d\t%d\t%s \n", 
		     $p_status, $p_maint, $p_low, $p_med, $p_high, $p_test, &strtime($lddate) );
	} 
}
