##############################################################################
# Author: Glenn Thompson (GT) 2007
#         ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#       2007-11-09: Written
#
# Purpose:
#	Takes CUBE files as input, parses them, and spits out xml in 1 of 2
#       possible formats.
#       
##############################################################################

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use orb;
require "getopts.pl" ;

#use strict;
#use warnings;
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();  # PROG_NAME becomes $0 minus any path

# Usage - command line options and arguments
our $opt_m; 
if ( ! &Getopts('m:') || $#ARGV < 0 ) {
    print STDERR <<"EOU" ;

    Usage: $PROG_NAME [-m mode] cubefile_1 cubefile_2 ... cubefile_N > outputxmlfile
	   $PROG_NAME [-m mode] < file > outputxmlfile
                -m      	mode 1 or 2 (results in different xml formats)
                cubefile_i	path to a cubefile

    $PROG_NAME Takes CUBE files as input, parses them, and spits out xml in 1 of 2
               possible formats. 	

    Example:

	$PROG_NAME -m 2 /home/qdds/polldirfinish/event.* > allevents.xml 	

EOU
    exit 1 ;
}


# End of  GT Antelope Perl header
##############################################################################
#!/usr/bin/perl

$mode = 1;
if ($opt_m > 0) {
	$mode = $opt_m;
}

	

print "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n";	


if ($#ARGV > -1) {
	foreach $file (@ARGV) {
		open(FIN,"$file");
		$line = <FIN>;
		close(FIN);
		chomp($line);
		&parse_line($line);
	}
} 
else
{
	while ($line = <>) {
		chomp($line);
		&parse_line($line);
	}
}


sub parse_line {

	$line = $_[0];
	$i = 0;

	# Read
	($Tp,$i) 		= &get_substr($line,$i,2);
	($Eidnumbr,$i) 		= &get_substr($line,$i,8);
	($So,$i) 		= &get_substr($line,$i,2);
	($V,$i)			= &get_substr($line,$i,1);
	($Year,$i)		= &get_substr($line,$i,4);
	($Mo,$i) 		= &get_substr($line,$i,2);
	($Dy,$i) 		= &get_substr($line,$i,2);
	($Hr,$i)		= &get_substr($line,$i,2);
	($Mi,$i)		= &get_substr($line,$i,2);
	($Sec,$i)		= &get_substr($line,$i,3);
	$Sec /= 10;
	($Lat,$i)		= &get_substr($line,$i,7);
	($Long,$i) 		= &get_substr($line,$i,8);
	($Dept,$i)		= &get_substr($line,$i,4);
	$Dept /= 10;
	($Mg,$i) 		= &get_substr($line,$i,2);
	$Mg /= 10;
	($Nst,$i) 		= &get_substr($line,$i,3);
	($Nph,$i) 		= &get_substr($line,$i,3);
	($Dmin,$i) 		= &get_substr($line,$i,4);
	$Dmin /= 10;
	($Rmss,$i) 		= &get_substr($line,$i,4);
	$Rmss /= 100;
	($Erho,$i) 		= &get_substr($line,$i,4);
	$Erho /= 10;
	($Erzz,$i) 		= &get_substr($line,$i,4);
	$Erzz /= 10;
	($Gp,$i) 		= &get_substr($line,$i,2);
	$Gp *= 3.6;
	($M,$i) 		= &get_substr($line,$i,1);
	($Nm,$i) 		= &get_substr($line,$i,2);
	($Em,$i) 		= &get_substr($line,$i,2);
	$Em /= 10;
	($L,$i)			= &get_substr($line,$i,1);
	
	# Output
	print "\n<event>\n" if ($mode == 1);
	print "\n<event " if ($mode == 2);
	&data2xml("message_type",$Tp);
	&data2xml("eventid",$Eidnumbr);
	&data2xml("data_source",$So);
	&data2xml("event_version",$V);
	&data2xml("year",$Year);
	&data2xml("month",$Mo);
	&data2xml("day",$Dy);
	&data2xml("hour",$Hr);
	&data2xml("minute",$Mi);
	&data2xml("second",$Sec);
	&data2xml("latitude",$Lat);
	&data2xml("longitude",$Long);
	&data2xml("depth",$Dept);
	&data2xml("magnitude",$Mg);
	&data2xml("number_of_stations_location",$Nst);
	&data2xml("number_of_phases_solution",$Nph);
	&data2xml("distance_to_nearest_station",$Dmin);
	&data2xml("rms_time_error",$Rmss);
	&data2xml("horizontal_error",$Erho);
	&data2xml("vertical_error",$Erzz);
	&data2xml("azimuthal_gap",$Gp);
	&data2xml("magnitude_type",$M);
	&data2xml("number_of_stations_magnitude",$Nm);
	&data2xml("magnitude_error",$Em);
	&data2xml("location_method",$L);
	print "</event>\n" if ($mode == 1);
	print " />" if ($mode == 2);
	
}

sub get_substr {
	($str, $i, $len) = @_;
	$substr = substr($str, $i, $len);
	$i += $len;
	return ($substr, $i);
}

sub data2xml {
	($tag, $data) = @_;
	print "\t<$tag>\n\t\t$data\n\t</$tag>\n" if ($mode == 1);
	print " $tag=\"$data\"" if ($mode == 2);
}
