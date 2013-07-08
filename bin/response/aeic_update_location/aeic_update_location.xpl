# aeic_respond
#
# Driver script for information releases at AEIC
#
# K. Lindquist
# Geophysical Institute
# University of Alaska, Fairbanks
# June 1998

use Datascope;

require "getopts.pl";
require "winding.pl";

$Pf = "aeic_release.pf";

sub choose_database_row {
	my( $dbin_name ) = @_;
	my( @tables );

	if( $dbin_name eq "-" ) {

		@db = dbopen_table( $dbin_name, "r" );

		$Dbname = dbquery( @db, "dbDATABASE_NAME" );

		@tables = dbquery( @db, "dbVIEW_TABLES" );

		if( grep( /event/, @tables ) && grep( /origin/, @tables ) ) {
			
			@db = dbsubset( @db, "origin.orid == prefor" );

			@db = choose_unique_origin_row( @db );

		} elsif( grep( /event/, @tables ) ) {

			@dborigin = dblookup( @db, "", "origin", "", "" );

			@db = dbjoin( @db, @dborigin );

			@db = dbsubset( @db, "origin.orid == prefor" );

			@db = choose_unique_origin_row( @db );

		} elsif( grep( /origin/, @tables ) ) {

			@db = choose_unique_origin_row( @db );

		} else {
			MyDie( "No origin information passed to $Prog_name" );
		}
		
	} elsif( $Orid ) {

		$Dbname = `abspath $dbin_name`;
		chop( $Dbname );

		@db = dbopen( $Dbname, "r" );
		@db = dblookup( @db, "", "origin", "", "" );

		$yes = dbquery( @db, "dbTABLE_PRESENT" );
		if( ! $yes ) {
			MyDie( "Origin table not present in $dbin_name" );
		}

		@db = dbsubset( @db, "orid == $Orid" );
		$nrecs = dbquery( @db, "dbRECORD_COUNT" );

		if( $nrecs != 1 ) {
			MyDie( "Couldn't find orid $Orid in $dbin_name" );
		}

		$db[3] = 0;

	} elsif( $Evid ) {

		$Dbname = `abspath $dbin_name`;
		chop( $Dbname );

		@db = dbopen( $Dbname, "r" );
		@dbevent = dblookup( @db, "", "event", "", "" );
		
		$yes = dbquery( @dbevent, "dbTABLE_PRESENT" );
		if( ! $yes ) {
			MyDie( "Event table not present in $dbin_name" );
		}

		@dbscratch = dblookup( @dbevent, "", "event", "", "dbSCRATCH" );
		dbputv( @dbscratch, "evid", $Evid );

		@records = dbmatches( @dbscratch, @dbevent, "myhook", "evid" );

		if( $records[0] < 0 ) {
			MyDie( "Couldn't find evid $Evid in $dbin_name" );
		} else {
			$expr = "evid == $Evid";
			@dbevent = dbsubset( @dbevent, $expr );
		}

		@dborigin = dblookup( @db, "", "origin", "", "dbALL" );

		@db = dbjoin( @dbevent, @dborigin );

		@db = dbsubset( @db, "origin.orid == prefor" );

		@db = choose_unique_origin_row( @db );

	} else {

		$Dbname = `abspath $dbin_name`;
		chop( $Dbname );

		@db = dbopen( $Dbname, "r" );
		@db = dblookup( @db, "", "event", "", "" );
	
		$nevents = dbquery( @db, "dbRECORD_COUNT" );
	
		if( $nevents == 0 ) {
	
			@db = dblookup( @db, "", "origin", "", "" );
	
			@db = choose_unique_origin_row( @db );

		} elsif( $nevents > 1 ) {
	
			MyDie( "Multiple events in $Dbname" );
	
		} else {
	
			@dbtemp = dblookup( @db, "", "origin", "", "" );
			@db = dbjoin( @db, @dbtemp );
			@db = dbsubset( @db, "origin.orid == prefor" );

			@db = choose_unique_origin_row( @db );
		}
	}

	return @db;
}

sub choose_unique_origin_row {
	my( @db ) = @_;

	$norigins = dbquery( @db, "dbRECORD_COUNT" );

	if( $norigins <= 0 ) {

		MyDie( "origin row selection failed for $Dbname" );

	} elsif( $norigins == 1 ) {

		$db[3] = 0;

	} else {

		$db[3] = 0;
		my( $unique_orid ) = dbgetv( @db, "origin.orid" );

		for( $db[3] = 1; $db[3] < $norigins; $db[3]++ ) {

			if( dbgetv( @db, "origin.orid" ) != $unique_orid ) {
				MyDie( "multiple origins in $Dbname view" );
			}
		}

		$db[3] = 0;
	}

	return @db;
}

sub save_subset_database {
	my( @dborigin ) = @_;

	@dbass = dblookup( @dborigin, "", "assoc", "", "" );
	@dbarr = dblookup( @dborigin, "", "arrival", "", "" );
	@dborigerr = dblookup( @dborigin, "", "origerr", "", "" );
	@dbevent = dblookup( @dborigin, "", "event", "", "" );
	@dbnetmag = dblookup( @dborigin, "", "netmag", "", "" );

	@db = dbjoin( @dborigin, @dbevent );

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs <= 0 ) {
		dbunjoin( @dborigin, $subset_database );
		return;
	}

	@db = dbjoin( @db, @dborigerr );

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs <= 0 ) {
		dbunjoin( @dborigin, $subset_database );
		return;
	}

	@db = dbjoin( @db, @dbnetmag );

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs <= 0 ) {
		dbunjoin( @dborigin, $subset_database );
		return;
	}

	@db = dbjoin( @db, @dbass );

	my( $nrecs ) = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs <= 0 ) {
		dbunjoin( @dborigin, $subset_database );
		return;
	}

	@db = dbjoin( @db, @dbarr );

	$nrecs = dbquery( @db, "dbRECORD_COUNT" );
	if( $nrecs > 0 ) {
		dbunjoin( @db, $subset_database );
	} else {
		dbunjoin( @dborigin, $subset_database );
	}
}

sub epoch_to_yyyymmddhhmmss {
	local( $epoch ) = @_;

	$result = epoch2str( $epoch, "%Y%m%d%H%M%S" );
	return $result;
}

sub establish_correct_cwd {
	local( $event_time ) = @_;

	$base_release_dir =~ s@HOME@$ENV{'HOME'}@;

	if( -e $base_release_dir && ! -d $base_release_dir ) {

		$message = "Intended target directory $base_release_dir ";
		$message .= "but is not a directory";
		MyDie( $message );

	} elsif( ! -e $base_release_dir ) {

		mkdir( $base_release_dir, 0755 );
		if( ! -e $base_release_dir ) {
			MyDie( "Failed to make $base_release_dir" ); 
		}
		chdir( $base_release_dir );

	} elsif( ! -W $base_release_dir ) {

		MyDie( "Directory $base_release_dir is not writable" );

	} else {

		chdir( $base_release_dir );
	}

	$event_timestr = epoch_to_yyyymmddhhmmss( $event_time );
	$! = 0;
	mkdir( "$event_timestr", 0755 );
	if( $! ) {
		MyDie( "Failed to make $base_release_dir/$event_timestr: $!" ); 
	}
	chdir( $event_timestr );

	$release_dir = "$base_release_dir/$event_timestr";
}

sub set_vital_statistics {

	local( @db ) = @_;

	($lat, $lon, $depth_km, 
	$origin_time, $orid, $evid_info, $nass) = dbgetv( @db, "lat",
				"lon", "depth", "time", "orid", "evid", "nass" );
	
	if( $depth_km < 0 ) { $depth_km = 0.; }

	$min = $lat;
	$min =~ s/\d+\./0./;
	$min *= 60 * ( $lat > 0 ? 1. : -1. );
	$lat_string =  sprintf( "%02d %s %02d\'",
			$lat * ( $lat > 0 ? 1. : -1. ),
			$lat > 0 ? 'N' : 'S',
			$min + 0.5 );

	$min = $lon;
	$min =~ s/\d+\./0./;
	$min *= 60 * ( $lon > 0 ? 1. : -1. );
	$lon_string =  sprintf( "%02d %s %02d\'",
			$lon * ( $lon > 0 ? 1. : -1. ),
			$lon > 0 ? 'E' : 'W',
			$min + 0.5 );

	$local_time = &local_time( $origin_time );

	$depth_mi = $depth_km / 1.6;

	$depth_km = sprintf( "%1.0f", $depth_km );
	$depth_mi = sprintf( "%1.0f", $depth_mi );

	( $mag, $magtype ) = &choose_magnitude( @db );

	$mag = sprintf( "%3.1f", $mag );

	$gregion = dbex_eval( @db, "grname(lat,lon)" );

}

sub local_time {
	local( $mytime ) = @_;

	$result = epoch2str( $mytime, "%I:%M %p %Z", $ENV{'TZ'} );
	$result =~ s/^0//;
	$result =~ s/\s*$//;

	return $result;
}

sub choose_magnitude {
	local( @db ) = @_;

	( $ml, $mb, $ms ) = dbgetv( @db, "ml", "mb", "ms" );

	if(  $ms != -999.00 ) {

		return $ms, "Ms (NEIC)";

	} elsif( $mb != -999.00 ) {

		return $mb, "mb (NEIC)";

	} elsif( $ml != -999.00 ) {

		return $ml, "ML";

	} else {
		MyDie( "$Prog_name: no magnitude available" );
	}
}

sub summarize_update {

	$description_update = 
	"***** Event evid=$evid_info has been updated *****
	
	These are the new parameters:
	
	Time: 		$when_HMS
	Location:	$lat, $lon 
	Depth:		$depth_km km 
	Magnitude:	$magtype $mag
	Nass:		$nass"
	
}

sub describe_where {

	@dbregions = dbopen( $region_phrases_database, 'r' ); 
	@dbregions = dblookup( @dbregions, "", "regions", "", "" );
	@regions = get_containing_regions( @dbregions, $lat, $lon );

	if( ! defined( $where = shift( @regions ) ) ) {

		$where = "(" . grname( $lat, $lon ) . ")";
	}
}

sub describe_when {

	$when = epoch2str( $origin_time, "%m/%d/%Y %H:%M" );
	$when_HMS = epoch2str( $origin_time, "%m/%d/%Y %H:%M:%S" );
}

sub set_subject_update {

	$subject_update =
	  "Subject: AEIC EQ UPDATE: $when_HMS, $lat $lon, $magtype $mag $gregion\n\n";
}

sub write_email_update {

	open( M,">$email_update" );

	print M $subject_update;
	print M $description_update;
#	print M $felt;
#	print M $distances;
#	print M $vitals;
#	print M $trailer;
	
	close( M );
}


sub MyDie {
	my( $msg ) = @_;

	$msg = "$Prog_name: $msg. Bye!";

	if( $Graphic_death ) {
		$cmd = $Helpers{"tkshow_message"};
		$cmd .= " $msg";
		system( $cmd );
		exit( 1 );
	} else {
		$msg .= "\n";
		die( $msg );
	}
}

sub map_workfile_names {
	my( $origin_time ) = @_;

	@keys = ( "email_file",
		  "email_update",
		  "fmpdf_file",
		  "gif_release_file",
		  "map_epsi_file" );

	foreach $key ( @keys ) {
		$$key = epoch2str( $origin_time, "$$key" );
	}
}

sub initialize {

	@myhelpers = ( "aeic_partial_release",
		       "dbmaprelease",
		       "felt_report_tool",
		       "update_finger" );

	if( ! defined( pfget( $Pf, "Helpers" ) ) ) {
		die( "Helpers array undefined in parameter file.\n" );
	} else {
		my( $hashref ) = pfget( $Pf, "Helpers" );
		%Helpers = %$hashref;
	}

	$Graphic_death = 0 unless( defined( $Helpers{"tkshow_message"} ) );
	$Graphic_death = 0 unless( -x $Helpers{"tkshow_message"} );

	foreach $helper ( @myhelpers ) {

		$message = "Helper application \'$helper\' is not defined ";
		$message .= "in parameter file";
		MyDie( $message ) unless( defined( $Helpers{$helper} ) );

		MyDie( "Helper application $Helpers{$helper} is not available")
		 unless( -x "$Helpers{$helper}" );
	} 

#	if( ! defined( pfget( $Pf, "bad_framemaker_displays" ) ) ) {
#		die( "bad_framemaker_displays array undefined " .
#		     "in parameter file.\n" );
#	} else {
#		my( $hashref ) = pfget( $Pf, "bad_framemaker_displays" );
#		%bad_framemaker_displays = %$hashref;
#	}

	@myparameters = (
			 "email_release_recipient",
			 "email_update_recipient",
			 "web_directory",
			 "printer",
			 "place_database",
			 "common_place_database",
			 "region_phrases_database",
			 "contact_database",
			 "calldown_database",
			 "number_of_contacts",
			 "num_nearest",
			 "map_range",
			 "stock_felt_report",
			 "base_release_dir",
			 "email_file",
		  	 "email_update",
			 "voicemail_file", 
			 "fmpdf_file",
			 "gif_release_file",
			 "map_epsi_file",
			 "subset_database",
			 "voicemail_instructions",
			 "maintainer"
			);

	
	foreach $param ( @myparameters ) {
		if( ! defined( $$param = pfget( $Pf, "$param" ) ) ) {
			MyDie( "$param undefined in parameter file" );
		} elsif( $$param eq "" ) {
			MyDie( "$param value empty in parameter file" );
		} 
	}

	# Suppress warnings about single use of some variables
	use vars ('$num_nearest', '$map_range', '$email_update',
		  '$email_file', '$voicemail_file', '$common_place_database',
		  '$printer', '$place_database' );

	if( defined $ENV{'PFPATH'} ) {
		$ENV{'PFPATH'} .= ":.";
	} else {
		$ENV{'PFPATH'} = ".";
	}

}

$Prog_name = $0;
$Prog_name =~ s@.*/@@;
$Usage = "Usage: $Prog_name [-g] [-n] [-o orid] [-e evid] dbname\n";

$opt_n = "";
$opt_o = "";
$opt_e = "";
$opt_g = "";
if( ! &Getopts('gno:e:') || $#ARGV != 0 ) {
	die( "$Usage" );

} else {
	$dbin_name = $ARGV[0];

	if( $opt_g ) { $Graphic_death = 1; } else { $Graphic_death = 0; }
	if( $opt_o ) { $Orid = $opt_o; } else { $Orid = ""; }
	if( $opt_e ) { $Evid = $opt_e; } else { $Evid = ""; }

	if( ( $opt_o || $opt_e ) && $dbin_name eq "-" ) {
		$message = "$Prog_name: -o/-e options incompatible with ";
		$message .= "taking database from stdin.\n";
		die( $message );
	}
}

&initialize();

@db = &choose_database_row( $dbin_name );

set_vital_statistics( @db );

establish_correct_cwd( $origin_time );

save_subset_database( @db );

&describe_when();

&describe_where();

&set_subject_update();

&summarize_update();

&write_email_update();

print "\n$Prog_name:\tLaunching aeic_partial_release...\n\n";

system( "$Helpers{'aeic_partial_release'} $opt_n" );

print( "\n\n$Prog_name: Successfully finished earthquake response.\n\n" );

exit( 0 );
