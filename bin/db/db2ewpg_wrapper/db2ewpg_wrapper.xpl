##############################################################################
# Author: Glenn Thompson (GT) 2007
#         ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#       2007-??-??: Written
#
# Purpose:
#
#       This program is 
##############################################################################

use Datascope;
use orb;
require "getopts.pl" ;

use strict;
use warnings;
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();  # PROG_NAME becomes $0 minus any path

# Usage - command line options and arguments
if ( $#ARGV != 4 ) {
    print STDERR <<"EOU" ;

    Usage: $PROG_NAME database sta chan start_time end_time

                database      	path to the database containing calibration & wfdisc table
                sta	   	station
		start_time	start date & time in epoch format
		end_time	end date & time in epoch format
		wfmeasdb	wfmeas database name (table will be wfmeasdb.wfmeas)

	
	$PROG_NAME attempts to retrieve calibration data for a particular station, looping over all BH*, BN* and HN* channels. For each channel found in the database, it calls db2ewpg which will load waveform data for that particular channel and compute peak ground motion and spectral acceleration values, and save them to a wfmeas table.  

EOU
    exit 1 ;
}

sub run {               # run system cmds safely
     	my ( $cmd, $ok ) = @_ ;

     	print "$cmd\n";
     	my $value = "";


        $value = `$cmd` if $ok;
        chomp($value);
        $value =~ s/\s*//g;

        if ($?) {
            print STDERR "$cmd error $? \n" ;
            exit(1);
        }

      	return $value;

}
# End of header
##############################################################################

my (@db, @db2, $expression, $calib, $rsptype, $sampfreq, $units, $numRecs, $itype, $chan, $table);
my ($database, $sta, $t0, $t1, $wfmeasdb) = @ARGV;

# Check if the database exists!
die("DB $database does not exist\n") unless (-e $database); 

die("table $database.wfdisc does not exist\n") unless (-e "$database.wfdisc"); 
system("touch $wfmeasdb.wfmeas") unless (-e "$wfmeasdb.wfmeas"); 
unless (-e $wfmeasdb) {
	print "$wfmeasdb does not exist: trying to create\n";
	open(F1,">$wfmeasdb") or die("Cannot create $wfmeasdb\n"); 
	print F1 "# Datascope Database Descriptor File\nschema rt1.0\ndbpath /iwrun/bak/run/dbmaster/{master_stations}\ndblocks local\n";
	close(F1);
}

# Open the database
@db = dbopen($database, "r");

# Open the origin table
@db = dblookup(@db, "", "calibration", "", "");

my @channels = qw(BHN BHE BNN BNE HNN HNE);
@channels = qw (BH1 BH2) if ($sta eq "COLA");


foreach $chan (@channels) {

	# Make expression
	$expression = "sta== \"$sta\" && chan== \"$chan\"  && time < $t0 && (endtime > $t1 || endtime == NULL)";

	# Subset
	@db2 = dbsubset(@db, $expression);
	$numRecs = dbquery(@db2, "dbRECORD_COUNT");

	if ($numRecs == 1) {
#		print "Subsetting $database.calibration with expression:\n\t$expression\n";


		$db2[3] = 0;
		($calib, $rsptype, $sampfreq, $units) = dbgetv(@db2, qw(calib segtype samprate units));
#		print "$calib $rsptype $sampfreq $units\n";

		$sampfreq = int($sampfreq + .5 * ($sampfreq <=> 0)); # makes this an integer, which the C program expects - wfdisc has values like 99.99999
	

		if (substr($units,0,2) eq "nm")
		{
			$calib *= 1.0e-9 ;
		}
		else
		{
			die("Units $units are unknown\n");
		}


		$itype = 2;
		$itype = 1 if($rsptype eq "D");
		$itype = 3 if($rsptype eq "A");

		&run("db2ewpg $database $sta $chan $t0 $t1 $calib $sampfreq $itype $wfmeasdb", 1);
	}
	else
	{
#		print "$numRecs records matching $sta $chan: \n";
	}
}
dbclose(@db);
#dbclose(@db2);

