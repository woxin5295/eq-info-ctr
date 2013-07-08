use lib "/opt/local/lib/perl5" ;
use Datascope;

require "css30.pl" ; # routines to handle CSS 3.0 files
require "epoch.pl" ; # routines to handle epoch times.
require "r3.pl" ; 
require "pf2.pl" ; 
require "getopts.pl" ; 

&Getopts("v"); 
die ( "Usage: dbscreen [ -v ] database\n" ) if ( @ARGV != 1 ) ;
$DB = $ARGV[0] ; 

$depth = 0.0 ; # depth for ttimes

&eval_pf ( "dbscreen.pf" ) ; 
if ( $Average_velocity ne "ttimes" ) 
    {
    $p_velocity = $Average_velocity / $Earth_radius ; 
    $ttimes = 0 ; 
    }
else 
    {
    $ttimes = 1 ; 
    }

$deltamax = 0.0 ; 

open ( SITE, "$DB.site" ) ; 
while ( <SITE> ) 
    {
    &unpack_site($_) ; 
    $lat{$sta} = $lat ;
    $lon{$sta} = $lon ; 
    ($x, $y, $z) =  &spher2r ( &d2r($lat), &d2r($lon) ) ; 
    ($x{$sta}, $y{$sta}, $z{$sta}) =  ($x, $y, $z) ;
    foreach $i ( @sta ) 
	{
	$distance = &distance ( $x, $y, $z, $x{$i}, $y{$i}, $z{$i} ) ;
	if ( $ttimes ) 
	    {
	    $time = &ttimes(&r2d($distance)); 
	    }
	else
	    {
	    $time = $distance / $p_velocity ; 
	    }
	$time = ($time + $epsilon_t ) * $fudge_factor ;
	if ( $opt_v ) { printf STDERR "%3s %3s %5.2f\n", $sta, $i, $time ;  }
	$deltamax{$sta, $i} = $deltamax{$i, $sta} = $time ; 
	if ( $deltamax < $time ) { $deltamax = $time ; }
	}
    @sta = ( @sta, $sta ) ;
    }
close SITE ;

#print STDERR "Sorting current $DB.arrival..\n" ; 
#system ( "sort +1 -n -o $DB.arrival $DB.arrival\n" ) ; 
if ( $opt_v ) { printf STDERR "Maximum time is %.1f\n", $deltamax ;  }

if ( $opt_v ) { print STDERR "Reading arrival table.\n" ;  }
open ( ARRIVAL, "$DB.arrival" ) ; 
@arrival = <ARRIVAL> ; 
close ARRIVAL ; 

if ( $opt_v ) { print STDERR "Looking through arrivals\n" ;  }
$narrivals = @arrival ; 
$i = 0 ; 
$keep[$narrivals] = 0 ; 
$last_time = 0 ; 

while ( $i < $narrivals - $Minimum_associated ) 
    {
    &unpack_arrival ( $arrival[$i] ) ; 
    $time0 = $time ; 
    if ( $time < $last_time ) 
	{ 
	print STDERR "Oops -- arrivals must be sorted by time. Start over.\n" ; 
	print STDERR "Sorting current $DB.arrival..\n" ;
	system ( "sort +1 -n -o $DB.arrival $DB.arrival\n" ) ;
	if ( $opt_v ) { print STDERR "Reading arrival table.\n" ;  }
	open ( ARRIVAL, "$DB.arrival" ) ; 
	@arrival = <ARRIVAL> ; 
	close ARRIVAL ; 

	$last_time = 0 ; 
	$i = 0 ; 
	next ; 
	}
    $last_time = $time ; 

    $sta0 = $sta ; 
    @ok = ( $i ) ; 
    undef %found ; 
    $found{$sta} = 1 ; 

    if ( $opt_v ) { print STDERR "Arrival $sta($arid) : " ;  }
    for ( $j = $i+1 ; $j < $narrivals ; $j++ ) 
	{
	&unpack_arrival ( $arrival[$j] ) ; 
	$delta = $time - $time0 ; 
	if ( $delta > $deltamax ) { last ;} 
	if ( defined $found{$sta} ) { next ; } 
	if ( $opt_v ) { printf STDERR "$sta($arid) = %.1f ", $delta ; }
	if ( $delta < $deltamax{$sta0, $sta} ) 
	    { 
	    if ( $opt_v ) { print STDERR " ok " ; }
	    @ok = ( @ok, $j ) ; 
	    $found{$sta} = 1 ; 
	    }
	}

    $ok = @ok ; 
    if ( $opt_v )  { print STDERR " $ok " ; } 
    if ( $ok >= $Minimum_associated ) 
	{
	if ( $opt_v ) { print STDERR "Kept" ; }
	foreach $j ( @ok ) 
	    {
	    $keep[$j] = 1 ; 
	    }
	}
    if ( $opt_v ) { print STDERR "\n" ; }
    $i++ ;
    }

for ( $i = 0 ; $i < $narrivals ; $i++ ) 
    {
    if ( $keep[$i] == 1) 
	{
	print $arrival[$i] ; 
	}
    }



sub ttimes { 
    local ( $distance ) = @_ ; 
    local ( $_, $tmp, $delta, $n, $phase, $time ) ; 

    $tmp = "/tmp/ttime$$" ; 
    open ( TMP, ">$tmp" ) ; 
    print TMP "P\n\n$depth\n$distance\n-1\n-1\n" ;
    close TMP ; 

    open ( TMP, "ttimes < $tmp |" ) ; 
    while ( <TMP> ) 
	{
	if ( /delta\s+# code\s+time/ ) { last ; }
	}
    $_ = <TMP> ; 
    $_ = <TMP> ; 
    ( $delta, $n, $phase, $time ) = split ( ' ', $_ ) ; 
    unlink $tmp ; 
    return $time ; 
    }
