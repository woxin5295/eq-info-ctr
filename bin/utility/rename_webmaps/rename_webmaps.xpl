##############################################################################
# rename_webmaps
# Author: Glenn Thompson (GT) 2007
#	  ALASKA EARTHQUAKE INFORMATION CENTER
#
# Modifications:
#
#
# Purpose:
#
# 	dbrecenteqs produces gif files linked to recenteqs website, but these have evids that are otherwise meaningless
#	so the question is how to link to these using dbevents_aeic.
#	
#	So this program attempts to find those gifs, and copy them (to a directory watched by an orb) under a different name
#	The names given will depend on the time, lat, lon and depth rather than evid.
#
#	If two (or more) output directories are given, renamed gif files go into both (all), touch files only go into the first.
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
if ( $#ARGV != 2 ) { 
    print STDERR <<"EOF" ;
    
    Usage: rename_webmaps inputdir webmapdir xferdir

	inputdir is the directory under which dbrecenteqs makes it evid directories
	webmapdir is the directory (not necessarily on a web server) where renamed gifs and touch files will be stored (they are automatically deleted after a few days)
	xferdir is the directory to put another copy of the renamed webmap gifs for orb2orb transfer (using the orbxfer2 program). This is used to send gifs to the iMacs 
		for the EOC project. This way a user at an EOC can access the webmap gifs using dbevents_aeic without having to run dbrecenteqs locally. This is better 
		because dbrecenteqs requires the installation of many third-party softwares.

    Example:

	rename_webmaps /usr/local/mosaic/Seis/recenteqs_sub/quakes /usr/local/Mosaic/Input/glenn/aeic/webmaps /net/inverse/export/inverse/fema/run/xfer2mac/webmaps

EOF
    exit 1 ;
}



##############################################################################
use File::Basename qw(dirname basename);
my @clarray = @ARGV;
my $inputdir = $ARGV[0];
my $webmapdir = $ARGV[1];
my $xferdir = $ARGV[2];
my $maxgifs = 50;
my $gif;
my $html;
my $xfergif;
my $webmapsgif;

my ($otime, $lat, $lon);
my @lines;

# Chop down the gifs array as it likely to run into the thousands - just look at last $maxgifs events
my @allgifs = glob("$inputdir/*evid*/*.gif");
if ($#allgifs > $maxgifs) {
	@allgifs = @allgifs[$#allgifs-$maxgifs+1..$#allgifs];
}

# loop over gifs
my $count = 1;
foreach $gif (@allgifs)  {
		
	# Name of corresponding html file
	$html = $gif;
	$html =~ s/gif/html/;

	# Open HTML file and grep for prefor time
	open(HTML,"$html");
	@lines = grep /UTC/, <HTML>;
	$otime = $lines[0];
	$otime =~ s/<td>//;
	$otime =~ s/<\/td>//;
	$otime =~ s/UTC//;
	$otime =~ s/[\/\.:\s]/_/g;
	print "Preferred origin time: $otime\n";
		
	# What to call the new gif
	$webmapsgif = "$webmapdir/$otime.gif";

	# Copy gif to newgif
	unless (-e $webmapsgif) {
		print "Copying $gif to $webmapsgif\n";
		system("cp $gif $webmapsgif"); 

		# Also copy to xferdir
		$xfergif = "$xferdir/$otime.gif";
		system("cp $gif $xfergif"); 

	}


	$count++;
}


# Remove any webmaps older than 7 days, so they don't keep accumulating
my @allwebmaps = glob("$webmapdir/*.gif");
my $webmap;
foreach $webmap (@allwebmaps) {
	if (-M $webmap > 7) {
		print "Removing $webmap\n";
		system("rm $webmap");
	}
}
