##############################################################################
# Author: Glenn Thompson (GT) 2007
#         ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#       2007-10-15: Written
#
# Purpose:
#
#      	A tool for the new summary database. Delete messages are written to orbxfer,
#      	and appear in the output directory as defined in the orbxfer parameter file.
#      	The purpose of this program is to respond to those delete messages, by removing
#	the appropriate event from the database on the iMac.
##############################################################################

use Datascope;
use orb;
require "getopts.pl" ;

use strict;
use warnings;
our $PROG_NAME;
($PROG_NAME = $0) =~ s(.*/)();  # PROG_NAME becomes $0 minus any path

# Set default parameter file
our $pf = "pf/$PROG_NAME.pf";
$pf =~ s/\.pl//;

# End of  GT Antelope Perl header
##############################################################################


my $orbxferpf = pfget($pf,"orbxfer_pf");
my $orbxferdir = pfget($orbxferpf,"directory");
my $xferdir = "$orbxferdir/delete";
#print "orbxferpf = $orbxferpf\norbxferdir = $orbxferdir\nxferdir = $xferdir\n";

# Usage - command line options and arguments
our $opt_p;

if (  ! &Getopts('p:') | $#ARGV < 0 ) {
    print STDERR <<"EOU" ;

    Usage: $PROG_NAME [-p parameter_file] database [xferdir]

		-p		parameter file, default is $pf
                database   	database from which to remove evid
                xferdir		the directory to monitor for delete messages (if omitted, defaults to $xferdir)
                
        This program should be run as an rtexec cronjob on an iMac. It is designed to monitor xferdir for delete messages and then call delete_event to delete an event so it doesn't show up in dbevents_aeic. It should also delete any corresponding webmaps and ShakeMaps.
EOU
    exit 1 ;
}

use File::Basename;

print "$PROG_NAME\n";

# Define globals from command line variables
my $database = $ARGV[0];
$xferdir  = $ARGV[1] if ($#ARGV > 0);

# Look for all delete messages
my @all_msgs = glob("$xferdir/*.del");
printf("Found %d messages like *.del in $xferdir for processing\n", $#all_msgs + 1) unless( $#all_msgs == -1);;
my $msg;
my $msg2;
my $base;
my $l;
my $event;
my $delete_event = pfget($pf, "delete_event");

foreach $msg (@all_msgs) {

	print "\nProcessing $msg\n";
	$base = basename $msg;
	$l = length $base;
	$event = substr($base, 0, $l - 4);
	if ($l > 13) { # mode evtime 
		&run("$delete_event  -d $database 0 $event ",1); 
	}
	else
	{ # mode evid
		&run("$delete_event  -d $database $event ",1); 
	}
	
	($msg2 = $msg) =~ s/$event\.del/$event\.done/;
	&run("mv $msg $msg2", 1);
} 


################################################################################

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



