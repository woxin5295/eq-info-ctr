##############################################################################
# Author: Glenn Thompson (GT) 2007
#         ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#       2007-11-08: Added header
#
# Purpose:
#	Briefly, the program monitors an origin table, and creates pf/orb2dbt packets for any new origin row satisfying specified criteria.
#	Those criteria can include a magnitude threshold and a minimum number of associated arrivals.
#	
#	By using the -i option, origerr, netmag and stamag tables can be included in the pf/orb2dbt packets also.
#       For more details, see man page.
#
# To do:
#	* Optionally include netmag, stamag and origerr tables in packets
#       * Optionally replay an old database at a specified rate (-r, where r=1 is real-time, r=100 is 100 times real-time, r=0 means ignore this option)
#	* Optionally add a parameter to restart from the beginning (DONE: 20080207 - just put -l 0)
#	* Optionally allow origins without magnitudes (DONE: 20080207 - just put -a)
##############################################################################

use Datascope;
use orb;
require "getopts.pl" ;

use strict;
use warnings;
use lib "/home/glenn/perlmod/lib/site_perl/5.8.0";
use Error qw(:try);
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();  # PROG_NAME becomes $0 minus any path

# Usage - command line options and arguments
our ($opt_a, $opt_d, $opt_i, $opt_l, $opt_m, $opt_n, $opt_r, $opt_s, $opt_t, $opt_v); 
if ( ! &Getopts('ad:il:m:n:r:s:t:v') || $#ARGV < 2 ) {
    print STDERR <<"EOU" ;

    Usage: $PROG_NAME [-a] [-d days] [-i] [-l last_lddate] [-m magnitude_threshold] [-n number_of_associations_threshold] [-r rate] [-s sleep] [-t time_to_wait_for_magnitude] [-v] database last_lddate_file orb [orb2 [orb3 ...]]

    Also see manpage.		

EOU
    exit 1 ;
}
$opt_d = -1  unless defined($opt_d);
$opt_l = -1 unless defined($opt_l);


# End of  GT Antelope Perl header
##############################################################################

#use File::Copy;
use File::stat;
use English;

# cannot handle full path names to orb2dbt.pf file
# so outfile MUST be orb2dbt.pf 

my $database = shift @ARGV;  # database name
my $ldfile  = shift @ARGV; # last_lddate file
my @orb = @ARGV;

# minimum magnitude needed
my $minMagnitude = $opt_m ? $opt_m : 0.0 ;

# number of associated arrivals needed
my $min_nass = $opt_n ? $opt_n : 4 ;

# rate - if non-zero it invokes replay mode which is designed to replay an old database at r times the real-time rate
my $replayRate = $opt_r ? $opt_r : 0;

# update time in seconds
my $sleep = $opt_s ? $opt_s : 10 ; # how long to wait between checks

# time to wait in seconds for a magnitude to arrive
my $timeToWaitForMagnitude = $opt_t ? $opt_t : 60;

our @dbtables = ("arrival", "assoc", "event", "origin") ;
push @dbtables, qw(netmag stamag origerr) if ($opt_i);

my $epochNow = str2epoch("now");
my $timestrNow = strydtime( $epochNow ); # epochtime now
print STDERR "\n###########  $PROG_NAME $timestrNow ##############\n\n";

foreach my $dbtable (@dbtables){
	if ( ! -e "$database.$dbtable" ) {
		die("No $dbtable table present for $database !\n");
	}
}

my @db      = dbopen ( $database, "r" ) ;

my @db_origin    = dblookup (@db, "", "origin", "", "" ) ;
my $origin_file = dbquery (@db_origin,"dbTABLE_FILENAME"); # filename of origin table
my $number_records_origin_at_startup  = dbquery ( @db_origin, "dbRECORD_COUNT") ; # number of records in origin table at program initiation

my @db_netmag = dblookup(@db, "", "netmag", "", "");
my $netmag_file = dbquery (@db_netmag,"dbTABLE_FILENAME"); # filename of origin table
my $number_records_netmag_start  = dbquery ( @db_netmag, "dbRECORD_COUNT") ; # number of records in netmag table at program initiation

my $this_lddate = -1;
my $last_lddate  = -1;

# process most recent modified row of origin table if $opt_d not specified 
if ($opt_l >= 0) {
	print STDERR "-d switch ignored as -l switch overrides this\n" if ($opt_d > -1);
	$last_lddate = $opt_l;
	&write_lastlddate($last_lddate,$ldfile);
	print STDERR "opt_l given, last_lddate = $last_lddate\n" if $opt_v;
}
elsif($opt_d >= 0) {
	my $secsperday = 60 * 60 * 24;
	$last_lddate = now() - ($opt_d * $secsperday);
	&write_lastlddate($last_lddate,$ldfile);
	print STDERR "no opt_l, opt_d given, last_lddate = $last_lddate\n" if $opt_v;

} else {
	print "no opt_l, if opt_d given it was set to 0\n" if $opt_v;

	if (-e $ldfile) {
		open(FIN,$ldfile);
		$last_lddate = <FIN>;
		close(FIN);
		print STDERR "- ldfile exists, last_lddate = $last_lddate\n" if $opt_v;

    	} 
	else 
	{	
		# Just go to end of database - same as -d 0
		$last_lddate = now();
		&write_lastlddate($last_lddate,$ldfile);
		print STDERR "- no ldfile, getting now last_lddate = $last_lddate\n" if $opt_v;

    	} 		
} 
dbclose(@db);

printf STDERR "\n\nStarting with last_lddate = %s ($last_lddate)\n",strtime($last_lddate) ; 
print STDERR "database $database starting with $number_records_origin_at_startup origin records and $number_records_netmag_start netmag records\n" ; 
#my $inode_start      = stat("$origin_file");

# force first run at startup after sleeping
my $mtime_when_db_last_read      = 0 ;
my $maxRecordsThatWillBeProcessed = 9999; # first time through only, this is a large number, thereafter it is 10
my $oldest_origin_in_minutes = 60;


my $first_origin = 1;

# Sit and wait for new origin db rows
for (;;) {
	sleep $sleep if ( $mtime_when_db_last_read != 0 ); # pause between loops

	my $inode_origin = stat("$origin_file"); # $number_records_origin_at_startup origin records
	my $mtime_origin = $inode_origin->mtime ; # when was origin table last modified?

	my $inode_netmag = stat("$netmag_file");
	my $mtime_netmag = $inode_netmag->mtime ; # when was netmag table last modified?

	if (($mtime_origin > $mtime_when_db_last_read)  || ($mtime_netmag > $mtime_when_db_last_read)   ) {
		print STDERR "\n\n *************************** \n$origin_file has changed\n" if ($mtime_origin > $mtime_when_db_last_read) ;
		print STDERR "\n\n *************************** \n$netmag_file has changed\n" if ($mtime_netmag > $mtime_when_db_last_read) ;

	    	@db = dbopen($database, "r");

	    	@db_origin = dblookup(@db, "", "origin", "", "");
	    	my $num_newrecords_origin = dbquery(@db_origin, "dbRECORD_COUNT");

	    	@db_netmag = dblookup(@db, "", "netmag", "", "");
	    	my $num_newrecords_netmag = dbquery(@db_netmag, "dbRECORD_COUNT");

		print STDERR "database $database now has $num_newrecords_origin origin records and $num_newrecords_netmag netmag records\n" ;


		# start a new log file every time the database gets cut down
		if ( ($num_newrecords_origin < $number_records_origin_at_startup) ||  ($num_newrecords_netmag < $number_records_netmag_start) ) {
			print STDERR "Looks like database has been tailed - starting a new log file\n";
			system("mv logs/$PROG_NAME logs/$PROG_NAME.old");
			$number_records_origin_at_startup = $num_newrecords_origin;
			$number_records_netmag_start = $num_newrecords_netmag;

			print STDERR "Database tailed so starting new log file\n";
			print STDERR "\n\nStarting last_lddate at $last_lddate, opt_l arg is $opt_d \n" ; 
			print STDERR "database $database starting with $number_records_origin_at_startup origin records and $number_records_netmag_start netmag records\n" ; 
		}	 

		my $recordNum_origin_start = ($num_newrecords_origin - $maxRecordsThatWillBeProcessed);
		$recordNum_origin_start = 0 if ($recordNum_origin_start < 0);
		printf STDERR "\n*** Starting at record %d ***\n", $recordNum_origin_start if $opt_v;

            	for ( $db_origin[3] = $recordNum_origin_start ; $db_origin[3] < $num_newrecords_origin ; $db_origin[3]++ ) { # Just check the last 10 records each time
			printf STDERR "\n\nThis is record %d of %d\n", $db_origin[3], $num_newrecords_origin if $opt_v;
 
			$db_origin[3] = 0 if $db_origin[3]<0; # make sure it doesn't go negative if less than 10 records in database
#			$maxRecordsThatWillBeProcessed = 10 if ($maxRecordsThatWillBeProcessed > 10);
            		my $this_lddate = dbgetv(@db_origin, "lddate") ;
	      		my $mb = dbgetv(@db_origin, "mb");
	      		my $ml = dbgetv(@db_origin, "ml");
	      		my $nass = dbgetv(@db_origin, "nass");
	      		my $orid = dbgetv(@db_origin, "orid");
	      		my $evid = dbgetv(@db_origin, "evid");
	      		my $auth = dbgetv(@db_origin, "auth");
			my $origin_time = dbgetv(@db_origin, "time");
			my $thisRecordNum = $db_origin[3] ;

			#print STDERR "new_lddate = $this_lddate, last_lddate = $last_lddate\n" if $opt_v;
              		if ( $this_lddate > $last_lddate )  {

				# replay mode
				if ($replayRate > 0) {
					my $time_to_sleep = ($this_lddate - $last_lddate) / $replayRate;
					printf STDERR "Replay: sleep for %.0f seconds\n", $time_to_sleep; 
					sleep( $time_to_sleep) if ($first_origin == 0);
				}
				$first_origin = 0;

				# Time criteria met
				my $timeNow = str2epoch("now");

	      			print STDERR "\nNew origin $orid detected for event $evid\n";
				printf STDERR "Origin time: %s\n",strtime($origin_time);
	      			print STDERR "Author: $auth, Nass: $nass, mb: $mb, ml: $ml\n";		 		
	        		printf STDERR "Time now: %s,  new_lddate = %s, last_lddate = %s  ...\n",strtime( $timeNow ), strtime($this_lddate), strtime($last_lddate);

	      			# GTHO: Check if origin has at least minimum number of arrivals
	      			if ($nass >= $min_nass) {
					print STDERR "Minimum nass criteria met\n";

					# GTHO: If no magnitude data yet, wait if event time is within last hour
					my $origin_age_in_minutes = ($timeNow - $origin_time) / 60;
					if ($origin_age_in_minutes < $oldest_origin_in_minutes) {

						my $seconds = 0;
						my $retry_secs = 5;
						while ($ml == -999.00 && $mb == -999.00 && $seconds < $timeToWaitForMagnitude) {
							printf STDERR "Origin has no magnitude data: will wait up to another %d seconds\n",$timeToWaitForMagnitude - $seconds;
							sleep($retry_secs); 
							$seconds +=$retry_secs; # wait X seconds between rechecks
	
							# then close, reopen and reread database (because the copy of the database each time is static - magnitude data never update as program sees a snapshot, not a dynamically changing database
							dbclose(@db);
							@db = dbopen($database, "r");
		    					@db_origin = dblookup(@db, "", "origin", "", "");
		    					my $num_records_origin_now = dbquery (@db_origin, "dbRECORD_COUNT");
							unless ($num_records_origin_now < $num_newrecords_origin) { # check if database tailed in the last sleep and skip this block if it was
										 # should just jump to checking next record from old database, which will skip again if there are no magnitudes
										 # but this is OK because last_lddate file wont be updated
										 # note that if $num_records_origin_now = $num_newrecords_origin, or $num_records_origin_now > $num_newrecords_origin, we just assume that at worst new records have been added, 
										 # and those will be picked up next time the modification time is checked.
	
								$db_origin[3] = $thisRecordNum;
								$mb = dbgetv(@db_origin, "mb");
			      					$ml = dbgetv(@db_origin, "ml");
								if ($ml > -999.00 || $mb > -999.00) {	
									print STDERR "Revised ml = $ml, mb = $mb after $seconds seconds\n" ;
								}
							}
						}
					}

					
					# Check the minimum magnitude has been met
					if (($opt_a) ||  ($ml >= $minMagnitude || $mb >= $minMagnitude)) {


						print STDERR "Magnitude criteria met\n";
						# Check that origin time criteria met
						if (($origin_age_in_minutes < $oldest_origin_in_minutes) || ($replayRate > 0) ) {
							printf STDERR "Origin age is %5.1f minutes\n",$origin_age_in_minutes;
							print STDERR "Sending record $thisRecordNum / orid $orid ...\n" ;

							# First send the origin, event, assoc, netmag and arrival tables
							# 20071214: added $pfout to return variables, so we can output packets
							my ($pfout,$packet) = run_event($thisRecordNum, @db_origin) ;
							my $t = now();

							# print out the packet if in verbose mode
							print STDERR "$packet\n" if $opt_v;
					
							# write to multiple orbs (at least one)
							my $orbname;
							foreach $orbname (@orb) {
								my $orbptr = orbopen("$orbname", "w");
								if ( $orbptr < 0 ) { 
								   die ( "Can't open $orbname\n" ) ; 
								}	
								my $nby = length($packet);
								my $pktid;
								$pktid = orbputx($orbptr, "/pf/orb2dbt", $t, $packet, $nby);
								print STDERR "packet $pktid written to $orbname with $nby bytes\n";
								orbclose($orbptr);
							}
		
							# Only want to update last_lddate file each time a new record is sent
							&write_lastlddate($this_lddate,$ldfile);
	   						$last_lddate = $this_lddate;
 	
						}
						else
						{
							print STDERR "Record $thisRecordNum / origin $orid - time was too old\n"; 
						} # end of if origin age

					}
					else
					{
						print STDERR "Record $thisRecordNum / origin $orid did not reach the magnitude threshold (or had none defined)\n"; 
					} # end of if ml
	    			}
				else
				{
					printf STDERR "Failed minimum nass criteria\n";	
	      			} # end of if nass > minnass


			} # end of new_ldate > last_lddate
				
	    	} # end of for $db_origin

		print STDERR "Closing Database db\n" if $opt_v;
	    	dbclose(@db);
	    	$mtime_when_db_last_read = $mtime_origin;
	    	print STDERR ".";

	}
	else
	{
		#printf STDERR "$origin_file has not changed\n" if $opt_v;
	}

}

sub run {
	my $cmd = $_[0];
	print STDERR "$cmd\n";
	system($cmd);
}

sub run_event {
    my ($myrec, @db2) = @_ ;
    my @dborigin = dblookup(@db2, "", "origin", "", "");
    $dborigin[3] = $myrec ;
    my $subor = dbgetv(@dborigin, "orid") ;
    my $orsub = qq(orid==$subor) ;
    print STDERR "$orsub\n" if $opt_v;
    my @dbproc;
    @dbproc = dbprocess(@db2, "dbopen origin",
	"dbsubset $orsub", 
	"dbjoin event", 
	"dbjoin assoc", 
	"dbjoin arrival") ;

    # Not all events will have an origerr, netmag or stamag row, plus these may not exist for some databases
    if ($opt_i) {
        my @dbproc2;
	# Add origerr if its there
   	@dbproc2 = dbprocess(@dbproc,	"dbjoin origerr");
	(@dbproc = @dbproc2) if (dbquery(@dbproc2, "dbRECORD_COUNT") > 0);

	# Add netmag if its there
  	@dbproc2 = dbprocess(@dbproc,	"dbjoin netmag");
	(@dbproc = @dbproc2) if (dbquery(@dbproc2, "dbRECORD_COUNT") > 0);

	# Add stamag if its there
   	@dbproc2 = dbprocess(@dbproc,	"dbjoin stamag");
	(@dbproc = @dbproc2) if (dbquery(@dbproc2, "dbRECORD_COUNT") > 0);
    } 

    my $numRecords_dbproc = dbquery(@dbproc, "dbRECORD_COUNT") ;
    print STDERR " $numRecords_dbproc records in dbproc\n" if $opt_v;

    my $pfout = "";
    foreach my $tbl (@dbtables) {
      
	try {
		my @dbsep = dbseparate(@dbproc, "$tbl");
      	
      	
      		if ($tbl eq "arrival") {
			$pfout .= "arrivals  &Literal{\n" ;
			@dbsep = dbsort(@dbsep, "sta", "iphase");
	      	} elsif ($tbl eq "assoc") {
			$pfout .= "assocs  &Literal{\n" ;
			@dbsep = dbsort(@dbsep, "sta", "phase");
	      	} elsif ($tbl eq "event" ) {
			$pfout .= "event  &Literal{\n";
	      	} elsif ($tbl eq "origin") {
			$pfout .= "origin  &Literal{\n";
	      	} elsif ($tbl eq "netmag") {
			$pfout .= "magnitude_update        yes\nnetmags  &Literal{\n";
	      	} elsif ($tbl eq "stamag") {
			$pfout .= "stamags  &Literal{\n";
	      	} elsif ($tbl eq "origerr") {
			$pfout .= "origerr  &Literal{\n";
	      	}
	      	my $num_sep = dbquery(@dbsep, "dbRECORD_COUNT");
	      	my @sep_fields = dbquery(@dbsep, "dbTABLE_FIELDS");
	      	my $nfields =  $#sep_fields;
	      	print STDERR "$tbl TABLE with $num_sep records and $nfields Fields\n" if $opt_v;
	      	for ($dbsep[3] = 0; $dbsep[3] < $num_sep; $dbsep[3]++) {
	        	my $row = "";
	        	foreach  my $fld (@sep_fields) {
	
	          		my @dbfld = dblookup(@dbsep, "", "", "$fld", "");
	          		my $fmt = dbquery(@dbfld, "dbFIELD_FORMAT");
	
	          		my $nul = dbquery(@dbfld, "dbNULL");
	          		my $fldval = dbgetv(@dbfld, "$fld");
				if ( ($fldval eq "") && ( (substr($fmt, -1) eq "f") || (substr($fmt, -1) eq "d") ) ) {
					$fldval = $nul;
				}	

	          		# must have space after fmt string for orb2dbt to read pf file
	          		try {
	          			$row .= sprintf("$fmt ", $fldval);
		  		}
		  		catch Error with {
					print STDERR "Error: fld = $fld, fmt = $fmt, fldval = $fldval, nul = $nul\n" if $opt_v;
		  		} 

	        	} # end foreach

			chop $row ;
			print STDERR "$row\n" if $opt_v;
	        	$pfout .= "$row\n";
	
	      	} # end for

      		$pfout .= "}\n";
      	
	} # end try
	catch Error with {
		print STDERR "dbseparate failed: No $tbl table for this origin\n";
      	}

    } # end foreach

    
    

    my $packet = s2pfpkt($pfout);

# 20071214: added $pfout to return variables, so we can output packets

    return ($pfout,$packet) ;
}


# This is the trick to get a string into packet format (see rtorbcmd)
sub s2pfpkt { 
    my ( $s ) = @_ ; 
    my $packet = chr(0) ;
    $packet .= chr(1) ;
    $packet .= $s ;
    $packet .= chr(0) ;
    return $packet ;
}

sub write_lastlddate {
	($last_lddate,$ldfile) = @_;
	print STDERR "write_lastlddate: writing $last_lddate to $ldfile\n" if $opt_v;
 	if (open(FOUT,">$ldfile")) { 
		print FOUT $last_lddate;
    		#print STDERR "Writing $last_lddate to $ldfile\n";
    		close FOUT;
    	}
    	else
 	{
		print STDERR "Could not write to $ldfile";
    	}

}

