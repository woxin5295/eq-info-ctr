##############################################################################
# orbsegment  
# Author: Josh Stachnik (JS) 2004, Glenn Thompson (GT) 2006-2007
#	  ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#	20061116 Comments added (GT) 
#	20061117 Added use strict, warnings, our and my
#
#
# Purpose:
#
# 	orbsegment watches an orb and sends segmented waveforms to another orb.
# 
# 	Looks at /pf/orb2dbt packets on an orb, subsets waveforms based on the stations 
#	in the 'arrivals' array in this pf packet, and sends the segmented waveforms to 
# 	another orb. 
#
# 	This passes segmented waveforms to an orb on inverse, which the orb at
#	the EOC pulls them from. Useful for the EOC, perhaps Murie Center or 
#	other museums, etc. This should be revamped or at least renamed. 
#	orbstaseg(1) is close to this, but only for one station.
#
# 	orbsegment watches for /pf/orb2dbt packets on an orb and segments data 
#	according to the arrivals array, sending the segmented data to another
#	orb. orbsegment is key to a FEMA system in that it sends segmented 
#	waveforms instead of having to stream continuous data for an unknown
# 	number of stations to an EOC. remember, orbstaseg is only useful for 
#	sending segmented data for ONE station. so if it is desired to send
#	segmented waveforms for multiple stations, multiple instances of 
#	orbstaseg will need to be running. And, orbstaseg monitors detections
# 	instead of arrivals. 
##############################################################################

use Datascope;
use orb;
require "getopts.pl" ;
 
use strict;
use warnings;
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();	# PROG_NAME becomes $0 minus any path


# End of  GT Antelope Perl header
##############################################################################

# Usage - command line options and arguments
if ( ! &Getopts('vn:l:p:') || $#ARGV != 1 ) { 
    print STDERR <<"EOF" ;
    
    Usage: $PROG_NAME [-v] [-l logfile] [-p pf] orbpf orbseg
		
		-v 	verbose mode on
                -l      logfile, default orbsegment.log
                -p      parameter file, default orbsegment.pf
                orbpf   originating orb containing /pf/orb2dbt 
			packets
                orbseg  output orb for segmented data

	Segments waveform data based on stations and arrival times 
	specified in arrivals array in /pf/orb2dbt packet on orbpf. 
	
	Waveform subsetting is relative to the arrival time of
	each station specified in arrivals array. An orb2orb
	connection is made for each station. This is more robust than
	copying multiple stachans at once, though slower. 

	Waveforms are segmented 'pretime' seconds before arrival time 
	and 'posttime' seconds after arrival time for 'chansift' channels.
	Mode, pretime, posttime, and channels are all specified in parameter file.

	An attempt is only made to run orb2orbcopy for a particular stachan if
	waveform data have not already been segmented for this stachan for a
	'similar' origin time. How similar is controlled by the parameter
	'max_origin_time_difference' and is currently hardwired as 2s.  This prevents
	multiple copies being made of essentially the same waveform data, which would
	be a significant problem if a large event occurred, since it would generate
	multiple origins, and there could be a large backlog of orb2orbcopy instances
	to run, slowing down waveform data for aftershocks or other events.
			

EOF
    exit 1 ;
}

##############################################################################

# Program specific variables

# Packet variables
my $Pktbytes;		# Used just once - cumulative count of #bytes
my $Pktcnt;		# #packets - compared to opt_n

our $opt_n;		# Gets tested for truth & against $Pktbytes
our $opt_p;		# Switch for alternative parameter file name
our $opt_v;		# Verbose flag
our $opt_l;		# Name of logfile to direct output to

our $orbpf = $ARGV[$#ARGV-1];	# Identifies the input  orbserver for /pf/orbdbt packets
our $orbseg = $ARGV[$#ARGV];	# Identifies the output orbserver for segmented data
die("orbpf not defined") if ($orbpf eq "");
die("orbseg not defined") if ($orbseg eq "");


our ($LOG, $logfile);
 
our $Pf;		# Parameter file name (without the .pf) 
my $mode; 		# 0 or 1 (from Pf)
my $pretime; 		# extra time to prepend to time window (from Pf)
my $posttime; 		# extra time to append to time window (from Pf)
my $chansift;		# channels to use (from Pf) - but doesn't get used
my $time_to_wait;	# number of seconds to wait for orb2orbcopy to run before killing it (from Pf)
my $orb2orbcopy;	# path to orb2orbcopy	
my @wforb;		# list of waveform orbservers to try for data

# packet variables
my ($pktid, $srcname, $time, $packet, $nbytes, $type, $pkt) ; 

# variables that get created after reading a packet
my ($pf, $ref, %pf, @arrvs, @org, $olat, $olon, $odep, $otime, $stas);
my ($arrv, @arr, $sta, $arrtime, @arrvar, $chan, $cnt, $stacha);
my ($subtime, $begtime, $endtime, $arrvar, $orbp);
my (@processes);
my ($wfdatagotfile) = "logs/wfdatagot";
my ($max_origin_time_difference) = 2; # seconds; if segmented data for a stacha already exists for
# a station within this interval of the current otime, the program will not attempt to run orb2orbcopy
# again
##############################################################################

# open logfile
$logfile = $opt_l ? $opt_l : "logs/$PROG_NAME";
$logfile =~ s/\.pl//;
print  "logfile is $logfile\n";
open LOGFILE, "> $logfile" or die();

select((select(LOGFILE), $|=1)[0]);
print LOGFILE "\n\n*********************************************\n";
print LOGFILE "\n**    $PROG_NAME startup    **\n" . 
		 "**   " . &epoch2timestr("now") . "   **\n";
print LOGFILE "*********************************************\n";
print LOGFILE "Reaping from $orbpf ...\n";
print LOGFILE "Writing to $orbseg ...\n";

system("touch $wfdatagotfile");

# obtain values from parameter file
$Pf = $opt_p ? $opt_p : "pf/$PROG_NAME.pf" ;

# time to wait
$time_to_wait = pfget($Pf, "time_to_wait");

$mode = pfget($Pf, "mode");
$pretime = pfget($Pf, "pretime");
$posttime = pfget($Pf, "posttime");
$chansift = pfget($Pf, "chansift");
$orb2orbcopy = pfget($Pf, "orb2orbcopy");
my $wforbref = pfget($Pf, "wforb");
@wforb = @$wforbref;

print LOGFILE "Parameter file read successfully ...\n";
print LOGFILE "mode = $mode\n";
if ($mode==0) {
 	print LOGFILE "segmenting channels $chansift \n$pretime secs before" .
 	"FIRST arrival time and $posttime secs after FIRST arrival time\n\n";
} 
else 
{
   	print LOGFILE "segmenting channels $chansift \n$pretime secs before" . 
	"EACH arrival time and $posttime secs after EACH arrival time\n\n";
}

# Orb initiation
$orbp = orbopen ( $orbpf, "r&" ) ; # opens the input orb in rw mode
print LOGFILE "$orbpf opened successfully ...\n"; 
orbselect($orbp, "/pf/orb2dbt") ; # select messages only of type "/pf/orb2bdt"
print LOGFILE "will select packets of type /pf/orb2dbt\n";
orbseek($orbp, "ORBNEWEST"); # sets $orbp to position of newest message
print LOGFILE "$orbpf at position of newest packet\n";

my $got_data = 0;
my $orb_num = 0;
my $cmd;

my $lastpktid = -1;

# LOOP OVER PACKETS
for (;;) { # an infinite loop - next to break or last to get out
	

	print LOGFILE "\n\n\n----------- Waiting for next packet at ". &epoch2timestr("now") . " -----------\n";
	print LOGFILE `date`;
	
	# read packet
  	($pktid, $srcname, $time, $packet, $nbytes) = orbreap($orbp) ;
	unless ($pktid == $lastpktid) {

		$lastpktid = $pktid;
	    	print LOGFILE "Processing packet $srcname $pktid ...";
    		$Pktcnt++ ; 
    		$Pktbytes += $nbytes ; 
	
		# show packet for debugging purposes		
	    	&showPkt($pktid, $srcname, $time, $packet, $nbytes, 2);
	
		# exit loop if packet isn't defined, or $opt_n < $Pktcnt
	    	last if (! defined $pktid || ($opt_n && ($opt_n < $Pktcnt))) ; 
	
		# separate orb packet into Packet structure    		
		($type, $pkt) = unstuffPkt($srcname, $time, $packet, $nbytes) ;
	
		# if type is "Pkt_pf"
	    	if ( $type eq "Pkt_pf" ) {
	
			# debugging message
			print LOGFILE "... GOOD\n"; 
	
			# This is OO code
			# Its procedural equivalent would be
			# $pf = pf($pkt);
			# Think of the -> as a .
			# So this is telling the data object $pkt to call the pf
			# method on itself
			# Apparently this retrieves the name of the parameter
			# file
	        	$pf = $pkt->pf ;
	
			# if the parameter file is defined, we can finally retrieve
			# the data we want
	        	if ( defined $pf ) {
	
				# Not sure what this $ref is - refers to blank
				# parameter name
	          		$ref = pfget($pf, "" ) ;
	
				# Now making %pf a hash of key-value pairs
				# (parameters?)
	            		%pf = %$ref ;
	
				# Debugging message - print arrivals array
		    		print LOGFILE "arrivals array is \n $pf{arrivals}\n" if ($opt_v);
	
				# Read arrivals array from %pf
	    			@arrvs = split(/\n/,$pf{arrivals});
	
				# Read origin parameters from %pf
	    			@org = split(/\s+/,$pf{origin});
	    			$olat = $org[1];
	    			$olon = $org[2];
	    			$odep = $org[3];
	    			$otime = $org[4];
	
				# Print origin array
		    		printf LOGFILE "origin: lat $olat lon $olon depth " .
					"$odep time  %s \n", strydtime($otime);
	
				# Send an alert
				my $orig_time = strydtime($otime);
				my $message ="New detected event:\ntime:\t$orig_time\nlatitude:\t$olat\nlongitude:\t$olon\ndepth:\t$odep\n";
				&mailadmin($message);			
	 
	
				# Blank the stations variable
		    		$stas = ""; # used only in mode 0


				# Blank out arrays
				my @begtimes;
				my @endtimes;
				my @stachas;
	
				# Loop over arrivals
		   		for ($cnt=0; $cnt<scalar(@arrvs); $cnt++) {
	
					# Get station this arrival relates to
					$arrv = $arrvs[$cnt];
					@arr = split(/\s+/,$arrv);
					$sta = $arr[0];
					#$stas .= "|$sta";
	
					# Get the arrival time this arrival
					# relates to
					$arrtime = $arr[1];
					#($sta, $arrtime) = @arr ;
	
					# Get the channel this arrival relates to
					$chan = $arr[6];
					#print "Station is $sta, channel is $chan\n"; # Info only - want to see what $chan is
								
					# This is for running orb2orb_copy later on single channels
				 	$begtime = $arrtime - $pretime;
				   	$endtime = $arrtime + $posttime;
					$stacha = "'.*$sta"."_$chan.*'";
	
					# Now push those onto arrays
					push @begtimes, $begtime;
					push @endtimes, $endtime;
					push @stachas, $stacha;
				
			   	} # end loop over arrivals

	   
				# Loop over orb and stacha, and run orb2orbcopy to segment out the wf data
				$got_data = 0;
				$orb_num = 0;
				# Initialise got_station array to zero & appropriate length
				my @got_station = ();
				for (my $cc=0;$cc<=$#stachas;$cc++) {
					$got_station[$cc]=0;
				}
				while (($got_data < $#stachas) && ($orb_num <= $#wforb)) {
					my @begintimes = @begtimes;
					my @endingtimes = @endtimes;
					my $thisorb = $wforb[$orb_num];
					print LOGFILE "\nTrying orbserver $thisorb\n";
					my $stanum = 0;

					foreach $stacha (@stachas) {
						
						# If this is the first orb, check the wf data got file first
						if ($got_station[$stanum]==0 && $orb_num==0) {

							# Let us see if we already have wf data for this stacha from a different origin
							open(WFDATA, "$wfdatagotfile");
							my @lines = <WFDATA>;
							close(WFDATA);
							@lines = grep($stacha, @lines);
							print "Got ".$#lines." matching lines for $stacha\n" if ($opt_v);
							my $line;
							foreach $line (@lines) {
								print "Testing line: $line\n" if ($opt_v);
								my @field = split / /, $line;
								print "Field0 = ".$field[0].", Field1 = ".$field[1]."\n" if ($opt_v);
								if (abs($otime - $field[1]) < $max_origin_time_difference) {
									$got_station[$stanum] = 1; # Wf data for a similar otime for this stacha already segmented
								}
							}
						}
							
						# Continue with this stacha, if no segmented data yet for this stacha for a similar otime 
						unless ($got_station[$stanum]) {


							$begtime = shift @begintimes;
							$endtime = shift @endingtimes;
							if (($begtime > 0) && ($endtime > 0)) {
								$cmd = "$orb2orbcopy -m $stacha $thisorb $orbseg $begtime $endtime";
								my $return_value = 0;
								my $success_code = "Failed";
								$return_value = &run_orb2orb($time_to_wait, $cmd, $thisorb);
								if ($return_value == 1) {
									$got_data++;
									$got_station[$stanum] = 1;
									$success_code = "Success";
								}
	
								# In Verbose mode, show which channels got
								if ($opt_v) {
									printf LOGFILE "%d/%d\t%d\t%s\t%d\t%d/%d",($orb_num+1),($#wforb+1),($stanum+1),$stacha,$return_value,$got_data,($#stachas+1);
									printf LOGFILE "\t%d",$got_station[$stanum] if ($return_value == 1);
									print LOGFILE "\n";
								}
								
								# Save station/channel and origin time to file
								if ($return_value == 1) {
									open WFDATA, "> $wfdatagotfile" or die();
									printf WFDATA "%s\t%f\n",$stacha, $otime;
									close(WFDATA);
								}
	
								if (`pgrep orb2orbcopy` ne "") {
									# kill cmd
									print LOGFILE "Killing orb2orbcopy\n";
									system("pkill orb2orbcopy");
								}
							}
						}

						$stanum++;
					}
					$orb_num++; 
				}

	
			} # end if parameter file defined
	
		} # end if type = "Pkt_pf"
		else
		{
			print LOGFILE "... not of type Pkt_pf\n";
		} 
		system("pkill orb2orbcopy"); # Just to be sure, shouldn't be needed
	} # end of unless pktid = lastpktid

} # end infinite loop
	
# end of program    
	
#############################################################################


sub run_orb2orb {

	my ($time_to_wait,$cmd,$thisorb) = @_;
	my $success = 0;
	my $tmpfile = "logs/orbsegment.tmp";
	print LOGFILE "$cmd > $tmpfile 2>&1 &\n";
	system("$cmd > $tmpfile 2>&1 &");				
	sleep($time_to_wait);
	my $return_value;
	if (-e $tmpfile) {
		my $wc = `wc -l $tmpfile`;
		$wc = substr($wc,0,8);
		my $numlines = int($wc);
		system("rm $tmpfile");
		my $recipient = "glenn\@giseis.alaska.edu";
		my $message;
	
		if ($numlines == 8) {
			# success
			$message = "\"success\"" if ($opt_v);
			$return_value=1;
		}
		elsif ($numlines == 4) {
			# no orbserver permission
			$message = "\"cannot connect to $thisorb\"" if ($opt_v);
			$return_value=2;
		}
		elsif ($numlines == 1) {
			# orb2orbcopy still running
			$message = "\"orb2orbcopy still running\"" if ($opt_v);
			$return_value=3;
		}
		elsif ($numlines == 31) {
			# wrong usage
			$message = "\"Wrong usage of orb2orb command\"" if ($opt_v);
			$return_value=4;
		}
		else
		{
			# something unexpected happened
			$message = "\"something weird\"";
			$return_value=5;
		}
		print LOGFILE "$message\n" if ($opt_v);
	}
	return $return_value;
}


sub mailadmin {
	my $message = $_[0];
	my $recipient = "glenn\@giseis.alaska.edu";
	system("echo $message | rtmail -s $PROG_NAME $recipient") if $opt_v;
}   

sub epoch2timestr {
	my $time = $_[0];
	my ($ss, $mm, $hh, $DD, $MM, $YYYY);
	if ( $time eq "now") {
		($ss, $mm, $hh, $DD, $MM, $YYYY) = localtime();
	}
	else
	{
		($ss, $mm, $hh, $DD, $MM, $YYYY) = localtime($time);
	}
	$YYYY = 1900 + $YYYY;
	$MM++;
	return "$YYYY-$MM-$DD $hh:$mm";
}

