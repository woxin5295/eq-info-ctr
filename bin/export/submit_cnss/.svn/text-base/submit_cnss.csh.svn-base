########################################################################
# Script to transfer monthly CNSS catalog file to the NCEDC.
# Doug Neuhauser, doug@seismo.berkeley.edu
#
# Revision History.
# Ver	Date		Modification
#----------------------------------------------------------------------
# 1.0	96/01/31	Initial coding
# 1.1   96/08/08	Mod by Steve Malone @ UofW for arguments
#----------------------------------------------------------------------
#
# USAGE:
#	sendcnss		(uses the current month)
# or
#       sendcnss YYYYMM      (Where YYYY and MM are year and month to be sent)
# or
#	sendcnss YYYYMM cat_file where cat_file is a pre-selected months data)
#
# N.B. Alaskan version at present requires the two-argument form
# Kent Lindquist 2/97
########################################################################
# Section 1:	Probably no changes needed in this section but check

set cnssuser = cnss
set cnsshost = quake.geo.berkeley.edu
set cnsstmpdir  = /home/ftp/dc/tmp.cnss
set cnsscatdir  = /home/ftp/dc/import.cnss

# Set to 1 if you want to be notified of successful transfers.
# Set to 0 of you do NOT want to be notified of successful tranfers.
set logxfer = 1

# Set for the location (or name) of your system utilities.
# set MAIL = "/usr/ucb/Mail"
set MAIL = "rtmail"
set RCP = "/usr/bin/rcp"
set RSH = "/usr/bin/rsh"
set DATE = "/usr/bin/date"
set RM = "/usr/bin/rm"
set CAT = "/usr/bin/cat"

########################################################################
# Section 2:  No change should be needed
# Set date and process dependent info.
set log = /tmp/cnss.export.$$
set cmdlog = /tmp/cnss.rsh.$$

# Pick up today's date (if necessary).
set long_date = `$DATE`
set cday = `$DATE +%d`
set cmonth = `$DATE +%m`
set ctime = `$DATE +%T`
set cyear = $long_date[6]
if($#argv == 0) then
#	echo Using current year-month: ${cyear}${cmonth}.  Kill now if not right.
#	sleep 5
	echo Usage: sendcnss YYYYMM cnss_file
	exit 1
else
	set cyear=`echo $1 |cut -c1-4`
	set cmonth=`echo $1 |cut -c5-6`
	echo Using year-month: ${cyear}$cmonth
	if(`echo $1 |wc -c` != 7) then
		echo 'USAGE: send.cnss.csh [YYYYMM]'
		exit 1
	endif
endif
set yr=`echo $cyear |cut -c3-4`

set mycat=""
if($#argv == 2) then
	set mycat=$2
	if(! -r $mycat)then
		echo Cannot open file, $mycat for reading. Quitting.
		exit 1
	endif
endif

# infile	- Pathname of catalog file to export. (probably OK as is)
set infile = /tmp/cnss.cat.${cyear}${cmonth}

########################################################################
# Section 3:	USER CUSTOMIZATION REQUIRED IN THIS SECTION
# User supplied code goes here.
#
# You MUST set your FDSN network identifier here.
set net = AK

# Set email address if you want transfer reports emailed to you.
# May be useful for error reporting from cron scripts but not necessary.
set mailto = "roger@giseis.alaska.edu,natasha@giseis.alaska.edu,mitch@giseis.alaska.edu"

# Put whatever code you need to construct your CNSS catalog file here.
# The following variables are available to help generate a data selection.
# cyear		- Year (4 digits)
# yr		- Year (last two digits only)
# cmonth	- Month (2 digits)
# mycat		- file_name for a pre-selected months data

if($mycat != "") then

# This section will be used if you specify a pre-selected months worth
# of data in the second argument.  Change the uwpk2cnss name to yours.
 cp $mycat $infile

else

# The following line should select a month's worth of data from your
# catalog of the size and type you want to contribute and pipe those
# data through the program to convert to CNSS format into the file, $infile
# NOTE: Any commands should probably include their whole path name.
# /u1/carl/sel -m 1.0 9.0 -p TSL ~seis/P/${yr}${cmonth}/loc | /u1/carl/uwpk2cnss >$infile

echo Please pre-generate the cnss catalog for the month in question, 
echo  and specify it on the command line: \"sendcnss YYYYMM cnss_catalog\". Thanks.
exit 1

endif
########################################################################
# Section 4:  nothing should be changed here
# Construct the remote catalog filename, and export the catalog file.

set cnsscat = ${net}.${cyear}.${cmonth}.cnss

if (-f "$infile") then	
	# The file is copied in 2 steps:
	# 1.  Copy the file into a temporary dir to the remote host.
	# 2.  Mv the file from the temporary dir to the import dir.
	# This ensures that the any file in the import dir is complete.

	# Copy the file to the tmp directory on $cnsshost.
        $RCP $infile $cnssuser@$cnsshost\:$cnsstmpdir/$cnsscat >&! $cmdlog
	set stat = $status
	if ($stat != 0 ||  ! -z $cmdlog) then
		echo ERROR $stat copying file $infile to $cnsshost >>! $log
		$CAT $cmdlog >>! $log
		goto report
	endif

	# Move the file to the import directory on $cnsshost.
        $RSH -l $cnssuser -n $cnsshost mv $cnsstmpdir/$cnsscat $cnsscatdir/$cnsscat >&! $cmdlog
	set stat = $status
	if ($stat != 0 || ! -z $cmdlog) then
		echo "ERROR $stat moving file to $cnsscatdir on $cnsshost" >>! $log
		$CAT $cmdlog >>! $log
		goto report
	endif
	if ($logxfer) then
		echo "Successful CNSS catalog transfer at $cyear/$cmonth/$cday,$ctime" >>! $log
		echo "	localfile:	$infile" >>! $log
		echo "	remotefile:	$cnsscat" >>! $log
	endif
endif

########################################################################
# Section 5:  Nothing should need to be changed here
# Perform final report distribution and file cleanup.

report:
if ( -e $log) then
	if ($mailto != "") then
		$MAIL -s "CNSS catalog export report" $mailto < $log
	else
		$CAT $log
	endif
	$RM $log
	if (-e $cmdlog) $RM $cmdlog
endif
