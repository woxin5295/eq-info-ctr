#!/usr/bin/perl -w
# dbrename 
# Glenn Thompson, March 2007
# 

use strict;

###################
my $usage = <<"EOF";
NAME
	rename - rename a group of files (such as a Datascope database)

SYNOPSIS:
	rename matchstr replacestr

DESCRIPTION:

rename is a simple Perl script that searches for all files in the local directory whose filenames start with a particular string provided in the first command line argument, and substitutes another string provided in the second command line argument. Its particularly useful for renaming a Datascope database, without having to write an inline foreach loop or tediously issuing a "mv" command for each file. Think of it as the equivalent of the ed, vi or Perl regular expression "s/^\$matchstr/^\$replacestr/g" but on filenames rather than strings in a file. 


EXAMPLE:

If the following files exist in the current directory:

	short_period
	short_period.calibration
	short_period.instrument
	short_period.lastid
	short_period.sensor
	short_period.site
	short_period.sitechan

Then the command:
	rename short_period sp_with_dataless

Is equivalent to:
	mv short_period sp_with_dataless
	mv short_period.calibration sp_with_dataless.calibration
	mv short_period.instrument sp_with_dataless.instrument
	mv short_period.lastid sp_with_dataless.lastid
	mv short_period.sensor sp_with_dataless.sensor
	mv short_period.site sp_with_dataless.site
	mv short_period.sitechan sp_with_dataless.sitechan

Note that this utility is useful with any files, not just those belonging to a Datascope database. For example, if the following files exist in the local directory:

	image1.jpg
	image2.jpg
	image3.jpg

Then the command:

	rename image Hawaii

Will rename these to:

	Hawaii1.jpg
	Hawaii2.jpg
	Hawaii3.jpg

AUTHOR
	Glenn Thompson
	University of Alaska Fairbanks
	12 March 2007

EOF
#########################################	

die $usage unless ($#ARGV == 1);

my ($source,$target)=@ARGV;
my (@allfiles, $sourcefile, $targetfile, $count);
$count = 0;
@allfiles = glob("$source*");
die("There are no files matching $source\n") unless ($#allfiles > -1);
foreach $sourcefile (@allfiles) {
	$targetfile = $sourcefile;
	$targetfile =~ s/$source/$target/;
	print "Renaming $sourcefile to $targetfile\n";
	if (-e $targetfile) {
		print "Target $targetfile already exists. Replace (y/n)?\n";
		my $answer = <STDIN>;
		next unless (substr($answer,0,1) eq "y");
	}	
	die("Could not rename $sourcefile\n") unless(rename($sourcefile,$targetfile)); 
	$count++;
}
printf "Renamed %d files of %d matching %s",$count,($#allfiles+1),$source;

