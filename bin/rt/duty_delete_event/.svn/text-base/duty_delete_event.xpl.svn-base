: # use perl
eval 'exec perl -S $0 "$@"'
if 0;
##############################################################################
# Author: Glenn Thompson (GT) 2007
#         ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#       2007-10-01: Written
#
# Purpose:
#
#      A tool for the new summary database - a centralised way of deleting an event
#	from all systems. 
##############################################################################

use lib "$ENV{ANTELOPE}/data/perl" ;
use Datascope;
use orb;
require "getopts.pl" ;

use strict;
use warnings;
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();  # PROG_NAME becomes $0 minus any path

# End of  GT Antelope Perl header
##############################################################################

our $pf = "/home/glenn/pf/$PROG_NAME.pf";

# Usage - command line options and arguments
our $opt_t = 0;
our $opt_e = 0;
our $opt_q = 0;
our $opt_s = 0; 
if ( ! &Getopts('tqse') || $#ARGV < 1 ) {
    print STDERR <<"EOU" ;

    Usage: $PROG_NAME [-eqst] database evid [evtime]

                -t      	test mode (nothing is deleted)
		-q		remove evid from QDDS
		-s		remove from ShakeMap
		-e		remove from EOC
                database   	database from which to remove evid
                evid		the event id to remove from the database
                evtime  	if evid is not found, match this event time instead (in case databases have different ids)

        Removes an event evid from a database. If -q, -s or -e are specified, event is also deleted from QDDS, ShakeMap or EOC systems respectively. 
	evtime may also be specified in case evids do not match, e.g. for EOC system where different evid's are used

EOU
    exit 1 ;
}

# Define globals from command line variables
my $database = $ARGV[0];
my $evid = $ARGV[1];
my $evtime;

# if $evtime not provided on command line, try to find it
if ($#ARGV > 1) {
	$evtime = $ARGV[2];
	# check that the evid exists, and if not search for it from evtime
	unless ( &db_evid_exists($database, $evid) )  {
		die("event not found in database $database or no prefor set") if ( (my $newevid = &evtime2evid($database, $evtime)) == -1 ) ;
		if ($newevid ne $evid) {
			print "evid you provided is $evid, but based on the time you provided evid $newevid will be used\n";
			print "You have 3 seconds to kill this program if you do not want to delete event $newevid\n";
			sleep(3);
			$evid = $newevid;
		}		
	}

}
else
{	
	die("event not found in database $database") if ( ($evtime = &evid2evtime($database, $evid)) eq "");
}	

our @status = qw(no yes);

my ($rd) = 0;
my ($re) = 0;
my ($rq) = 0;
my ($rs) = 0;

$opt_t = 1 - $opt_t;

$re = &delete_from_eoc( $database, $evid, $evtime ) if $opt_e;
$rq = &delete_from_qdds( $database, $evid, $evtime) if $opt_q;
$rs = &delete_from_shakemap( $database, $evid, $evtime ) if $opt_s;
$rd = &delete_from_database( $database, $evid, $evtime );


&notify_dutystaff_of_deletes($database, $evid, $evtime, $rd, $re, $rq, $rs);
1;

	
	

################################################################################

sub evid2evtime {
	my ($database, $evid) = @_;

	print "\n** evid2evtime **\n";
	

	my $evtime = "";
	if (-e $database) {
		$evtime = run("dbsubset $database.origin 'evid==$evid' | dbjoin - $database.event | dbsubset - 'orid==prefor' | dbselect - time", 1);
	}
	else
	{
		die("$database not found\n");
	}

	return $evtime;
}

################################################################################

sub db_evid_exists {
	my ($database, $evid) = @_;

	print "\n** db_evid_exists **\n";
	


	my $evid_exists = 0;
	if (-e $database) {
		my $numrecs = run("dbsubset $database.origin 'evid==$evid' | dbquery - dbRECORD_COUNT", 1);
		$evid_exists = 1 if ($numrecs > 0);
	}
	else
	{
		die("$database not found\n");
	}
	return $evid_exists;
}

####################################################################################

sub evtime2evid {
	my ($database, $evtime) = @_;

	print "\n** evtime2evid **\n";
	


	my $evid = -1;
	if (-e $database) {
		$evid = run("dbsubset $database.origin 'time==$evtime' | dbselect - evid", 1);
	}
	else
	{
		die("$database not found\n");
	}
	return $evid;
}


################################################################################

sub run {               # run system cmds safely
     my ( $cmd, $ok ) = @_ ;
     
     print "$ok: $cmd\n";
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

################################################################################

sub delete_from_database {

	my ($database, $evid, $evtime) = @_;
	our $opt_t;


	my $deleted = 0;

	print "\n** delete_from_database **\n";

	if (-e $database) {

		# Build up the string
		my $execstring = "dbsubset $database.origin 'evid==$evid' | dbjoin - $database.event";
		
		# Add tables to execstring
		my $table;
		foreach $table qw( assoc arrival netmag stamag) {
			my $dt = $database . "." . $table;
			if (-e "$dt") {
				$execstring .= " $dt ";
			}
		}

		# Add delete command to execstring
		$execstring .= " | dbdelete -m -";

		# Execute execstring
		run("$execstring", $opt_t);
		$deleted = 1;
	}
	else
	{
		die("$database not found\n");
	}
	
	return $deleted;

};


######################

sub delete_from_eoc {

	my ($database, $evid, $evtime) = @_;
	our $opt_t;


	print "\n** delete_from_eoc **\n";

	
	# Delete from EOC server database
	my $eocdatabase = pfget($pf, "eoc_server_database");
	#print "Calling &delete_from_database for $eocdatabase\n";
	#&delete_from_database( $eocdatabase, $evid, $evtime );

	# Put a delete message into EOC orbxfer directory
	my $eoc_orbxfer = pfget($pf, "eoc_orbxfer_path");
	my $eoc_mode 	= pfget($pf, "eoc_delete_mode"); #time or evid


	if ($eoc_mode eq "time") {
		my $timefile = "$eoc_orbxfer/delete/$evtime.del";
		print "Sending delete message file $timefile to EOC System\n";
		run("touch $timefile", $opt_t);
	}
	else
	{
		my $evidfile = "$eoc_orbxfer/delete/$evid.del";
		print "Sending delete message file $evidfile to EOC System\n";
		run("touch $evidfile", $opt_t);
	}

	
	# At other end, orbxfer needs to pick these up, and run delete_from_database with a dummy id to delete these by evtime
	# from the EOC client database

	return 1;

}

######################

sub delete_from_shakemap {

	my ($database, $evid, $evtime) = @_;
	our $opt_t;

	print "\n** delete_from_shakemap **\n";

	my $shakemap_exists;
	my $removed = 0;
	my $sm_delete_script = pfget( $pf, "shakemap_delete_script" );

	# Cancel ShakeMap (following command returns "" if no such evid)
	$removed = run("/net/bedrock/export/bedrock1/ShakeMap/bin/cancel -event $evid", $opt_t);
	($removed eq "") ? $removed = 0 : $removed = 1; 

	return $removed;

}

######################

sub delete_from_qdds {

	my ($database, $evid, $evtime) = @_;
	our $opt_t;


	print "\n** delete_from_qdds **\n";


	my $deleted = 0;

	my $qdate = epoch2str($evtime, "%G");
	$qdate =~ s/-/\//g;

	my $qdds_bin = pfget( $pf, "bindir");
	my $qdds_delete_script = pfget( $pf, "qdds_delete_script" );

	if (-x "$qdds_bin/$qdds_delete_script") {
		run("$qdds_bin/$qdds_delete_script $qdate $evid", $opt_t);
		$deleted = 1;
	}
	else
	{
		printf "$qdds_delete_script not found\n";
	}

	return $deleted;

}

#######################

sub notify_dutystaff_of_deletes {

	print "\n** notify_dutystaff_of_deletes **\n";
	our $opt_t;

	use Env;
	my $home=$ENV{HOME};
	my $user=$ENV{USER};

	my $timenow = epoch2str(now(), "%G %H:%M");


	my ($database, $evid, $evtime, $fd, $fe, $fq, $fs) = @_;

	my $time = epoch2str($evtime, "%G %T");
	my $email_list;

	if ($opt_t) {
		$email_list = "glenn\@giseis.alaska.edu";
	} 
	else
	{
		$email_list = "duty\@giseis.alaska.edu";
	}

	my $subject = "\"Deleted event\"";
	$subject = "\"**** TEST MESSAGE ONLY (Deleted event) PLEASE IGNORE ****\"" if $opt_t;
	my $cmd = "echo rtmail -s $subject $email_list";	 
	my $msg="The program $PROG_NAME was called by $user at $timenow to delete the event:\n\tevid = $evid\n"; 
	$msg .=	"\tevent time = $time\n"; 
	$msg .=	"\nfrom the following:\n";
	$msg .=	"\tdatabase-\t$status[$fd]   ($database)\n"; 
	$msg .=	"\tEOC system-\t$status[$fe]\n" if $opt_e;
	$msg .=	"\tQDDS system-\t$status[$fq]\n" if $opt_q;
	$msg .= "\tShakeMap system-\t$status[$fs]\n" if $opt_s;

	$msg .= "\nThe \"yes\" or \"no\" responses indicate whether $PROG_NAME thinks it succeeded\n";

	$msg .= "\nNote that $PROG_NAME cannot delete events from the EOC, ShakeMap or QDDS systems directly. For the EOC system, a message file is created which is sent to remote EOCs via orbxfer/orb2orb, and processed at those remote sites to remove the event from the remote database. A ShakeMap script called \"cancel\" is invoked to delete a ShakeMap if it exists. A message file marking an event for deletion is sent to QDDS\n"
		. "\n* Please check to make sure these deletes worked *\n" if ($opt_s || $opt_q || $opt_e);

	$msg .= "\n**** THIS IS A TEST MESSAGE - NOTHING HAS BEEN DELETED - PLEASE IGNORE ****\n" if $opt_t;

	system("echo \"$msg\" | rtmail -s $subject $email_list");

	
	my $logfile = pfget($pf, "logfile");
	open(FLOG,">>$logfile") or die("Cannot open $logfile: $!\n");
	printf FLOG "$msg\n******************\n\n";
	close FLOG;
	
}
 

