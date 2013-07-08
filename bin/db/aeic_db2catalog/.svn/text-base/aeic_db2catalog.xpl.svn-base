use Datascope;
require "getopts.pl";

# aeic_db2catalog
# 
# Kent Lindquist (rewrite of original by Dan McNamara)
# Geophysical Institute
# University of Alaska
# 2002

sub largest_gap {
	my( @dbbundle ) = @_;
	my( @db );
	my( @esaz_list ) = ();
	my( @diff_list ) = ();
	my( $first, $last, $this, $largest_gap );

	$minrec = $dbbundle[3];
	$maxrec = $dbbundle[2];

 	@db = dblookup( @dbbundle, "", "", "dbALL", "" );

 	for( $db[3] = $minrec; $db[3] < $maxrec; $db[3]++ ) {
 		push( @esaz_list, dbgetv( @db, "esaz" ) );
 	}
 	if( $#esaz_list < 0 ) {
 		return -1;
 	}
 	@esaz_list = sort { $a <=> $b; } @esaz_list;
 
 	$first = $last = shift( @esaz_list );
 	while( $#esaz_list >= 0 ) {
 		$this = shift( @esaz_list );
 		push( @diff_list, $this - $last );
 		$last = $this;
 	}
 	push( @diff_list, ( $first + 360 - $last ) % 360 );
 	@diff_list = sort { $a <=> $b; } @diff_list;
 
 	$largest_gap = pop( @diff_list );
	
 	return $largest_gap;
}

sub get_qual {
	my( $err_val ) = @_;
	my( $qual );

	SWITCH: {
		$qual = "A", last SWITCH if $err_val <= 1.34;
		$qual = "B", last SWITCH if $err_val > 1.34 && $err_val <= 2.67;
		$qual = "C", last SWITCH if $err_val > 2.67 && $err_val <= 5.35;
		$qual = "D", last SWITCH if $err_val > 5.35;
		$quale = " ";
	}

	return $qual;
}

sub translate_dtype {
	my( $dtype, $depth ) = @_;
	my( $aeic_dtype );

	SWITCH: {
		$aeic_dtype = " ", last SWITCH if $dtype eq "-";
		$aeic_dtype = "f", last SWITCH if $dtype eq " ";
		$aeic_dtype = "N", last SWITCH if $dtype eq "g" && $depth == 33;
		$aeic_dtype = "G", last SWITCH if $dtype eq "g";
		$aeic_dtype = "D", last SWITCH if $dtype eq "r";
		$aeic_dtype = " ";
	}

	return $aeic_dtype;
}

sub get_magstr {
	my( $mb, $ml ) = @_;
	my( $mag_str );

	SWITCH: {
		$mb != -999 && do {
				$mag_str = sprintf( "%4.1f \t", $mb );
				last SWITCH;
			};
		$ml != -999 && do {
				$mag_str = sprintf( "\t %4.1f", $ml );
				last SWITCH;
			};
		$mb != -999 && $ml != -999 && do {
				$mag_str = sprintf( "%4.1f \t %4.1f", $mb, $ml );
				last SWITCH;
			};
		$mag_str = "\t  N/A";
	}

	return $mag_str;
}

sub make_figure {
	my( $h, $figure_time ) = @_;

	$filename = $h->{"filename"};
	$figure_name = $h->{"figure_name"};
	if( $opt_v ) {
		printf STDERR "Generating $figure_name\n";
	}

	if( defined( $h->{"subset"} ) ) {

		$expr = $h->{"subset"};
		$figure_dbname = "/tmp/db2catalog_$$_$<";

		system( "dbsubset $dbname.origin $expr | " .
			"dbselect - > $figure_dbname.origin" );

		@dbsubset = dbopen_table( "$figure_dbname.origin", "r" );
		if( dbquery( @dbsubset, "dbRECORD_COUNT" ) <= 0 ) {
			printf STDERR "No records with $expr; skipping $figure_name\n";
			dbclose( @dbsubset );
			next;
		}
		dbclose( @dbsubset );
		
	} else {
		$figure_dbname = $dbname;
	} 

	$title = epoch2str( $figure_time, $h->{"ak_dbmapevents"}->{"title"} );
	$center = $h->{"ak_dbmapevents"}->{"center"};
	$range = $h->{"ak_dbmapevents"}->{"range"};

	open( PF, ">ak_dbmapevents.pf" );
	print PF "title\t$title\n";
	print PF "center\t$center\n";
	print PF "range\t$range\n";
	print PF "istaplt\t", $h->{"ak_dbmapevents"}->{"istaplt"}, "\n";
	print PF "istnam\t", $h->{"ak_dbmapevents"}->{"istnam"}, "\n";
	print PF "ipdeplt\t", $h->{"ak_dbmapevents"}->{"ipdeplt"}, "\n";
	print PF "ipdepth\t", $h->{"ak_dbmapevents"}->{"ipdepth"}, "\n";
	print PF "iporid\t", $h->{"ak_dbmapevents"}->{"iporid"}, "\n";
	print PF "icities\t", $h->{"ak_dbmapevents"}->{"icities"}, "\n";
	print PF "ipipe\t", $h->{"ak_dbmapevents"}->{"ipipe"}, "\n";
	print PF "iblue\t", $h->{"ak_dbmapevents"}->{"iblue"}, "\n";
	print PF "ipmag\t", $h->{"ak_dbmapevents"}->{"ipmag"}, "\n";
	print PF "idcirc\t", $h->{"ak_dbmapevents"}->{"idcirc"}, "\n";
	print PF "ititl\t", $h->{"ak_dbmapevents"}->{"ititl"}, "\n";
	print PF "iflt\t", $h->{"ak_dbmapevents"}->{"iflt"}, "\n";
	print PF "ipumps\t", $h->{"ak_dbmapevents"}->{"ipumps"}, "\n";
	print PF "icol\t", $h->{"ak_dbmapevents"}->{"icol"}, "\n";
	print PF "itran\t", $h->{"ak_dbmapevents"}->{"itran"}, "\n";
	print PF "ilegnd\t", $h->{"ak_dbmapevents"}->{"ilegnd"}, "\n";

	if( defined( $h->{"ak_dbmapevents"}->{"label_files"} ) ) {
		$hash = $h->{"ak_dbmapevents"}->{"label_files"};
		print PF "label_files &Tbl{\n";
		print PF join( "\n", @{$hash} );
		print PF "\n}\n";
	}

	if( defined( $h->{"ak_dbmapevents"}->{"boilerplate_files"} ) ) {
		$hash = $h->{"ak_dbmapevents"}->{"boilerplate_files"};
		print PF "boilerplate_files &Tbl{\n";
		print PF join( "\n", @{$hash} );
		print PF "\n}\n";
	}

	close( PF );

	if( defined( $h->{"profile"} ) ) {
		$profile_title = 
			epoch2str( $map_time, $h->{"profile"}->{"title"} );

		open( PROFILE, ">profile.pf" );
		print PROFILE "title\t$profile_title\n";
		print PROFILE "width\t", $h->{"profile"}->{"width"}, "\n";
		print PROFILE "depth\t", $h->{"profile"}->{"depth"}, "\n";
		print PROFILE "angle\t", $h->{"profile"}->{"angle"}, "\n";
		print PROFILE "scale\t", $h->{"profile"}->{"scale"}, "\n";
		print PROFILE "vert_exag\t", $h->{"profile"}->{"vert_exag"}, "\n";
		print PROFILE "prof_coord &Tbl{\n";
		foreach $line ( @{$h->{"profile"}->{"prof_coord"}} ) {
			print PROFILE $line, "\n";
		}
		print PROFILE "}\n";
		close( PROFILE );

		$profile = "-c";
	} else {
		$profile = "";
	}

	system( "ak_dbmapevents $figure_dbname $center $range -o $filename $profile" );
	system( "alchemy -e $filename -Zm4 -Zc1 -Zo 7.5i -Z+ -o" );

	if( $figure_dbname != $dbname ) { unlink( $figure_dbname.origin ); }
	unlink( "ak_dbmapevents.pf" );

	if( defined( $h->{"profile"} ) ) {
		$prof_file = $h->{"profile"}->{"filename"};
		system( "/bin/mv profile.ps $prof_file" );
		system( "alchemy -e $prof_file -Zm4 -Zc1 -Zo 7.5i -Z+ -o" );
		unlink( "profile.pf" );
	}
}

sub make_highlights {

	open( H, ">monthly_highlights" );
	if( $opt_v ) { print STDOUT "\nMonthly Highlights\n\n"; }

	foreach $h ( @{pfget( "aeic_db2catalog", "highlights" )} ) {

		if( $opt_v ) { print STDOUT "\n", $h->{"title"}, "\n\n"; }
		print H "\n", $h->{"title"}, "\n\n";

		@db = dbprocess( @db, "dbopen origin", 
			      "dbsubset " . $h->{"subset"},
			      "dbsort -r " . $h->{"reverse_sort_field"} );

		$nrecs = dbquery( @db, "dbRECORD_COUNT" );
		if( $nrecs <= 0 ) {
			printf STDERR "No records for '" . $h->{"title"} . "'; skipping\n";
			next;
		}

		$nprint = $nrecs > $h->{"nmax"} ? $h->{"nmax"} : $nrecs;
	
		for( $db[3] = 0; $db[3] < $nprint; $db[3]++ ) {
			( $lat, $lon, $depth, $time, $orid, $mb, $ml ) = 
			dbgetv( @db, "lat", "lon", "depth", "time", "orid", 
			     	"mb", "ml" );
		
			$time_str = epoch2str( $time, $h->{"time_format"} );

			$line = sprintf( "%d %s %f %f %f %f %f",
				$orid, $time_str, $lat, $lon, $depth, $mb, $ml );

			if( $opt_v ) { print STDOUT "$line\n"; }
			print H "$line\n";

		}
	}

	close( H );

}

$Usage = "Usage: $0 dbname\n";

if( ! &Getopts('vtm') || $#ARGV != 0 ) {
	die( "$Usage" );
} else {
	$dbname = pop( @ARGV );
	if( $opt_t && $opt_m ) {
		printf STDERR 	
			"-t and -m incompatible. Defaulting to produce text and maps.\n";
		$opt_t = $opt_m = 0;
	}
}

@db = dbopen( $dbname, "r" );
@db = dbprocess( @db, "dbopen origin",
		      "dbjoin -o origerr",
		      "dbsort time" );

@dbassoc = dbprocess( @db, "dbopen assoc",
			   "dbsort orid",
			   "dbgroup orid" );

@db = dbjoin( @db, @dbassoc, "-outer" );

$nrecs = dbquery( @db, "dbRECORD_COUNT" );
if( $nrecs < 1 ) {
	die( "db2catalog: No events to process\n" );
}

@dbcheck = dbprocess( @db, "dbopen assoc", 
			   "dbsubset delta == NULL || esaz == NULL" );
if( dbquery( @dbcheck, "dbRECORD_COUNT" ) > 0 ) {
	die( "db2catalog: assoc table has null delta and/or esaz " .
	     "values. Please fix before continuing.\n" );
}

open( LIST, ">monthly_list" );
open( BIG, ">monthly_m4" );
open( QUARRY, ">monthly_quarry" );

if( $opt_v ) {
	print STDOUT "\n***\nGenerating earthquake listings\n***\n";
}
print LIST "\n";
$linecount = 5;

for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {

	( $lat, $lon, $depth, $time, $orid, $etype, 
	  $ndef, $mb, $sdepth, $sminax, $smajax, $sdobs,
	  $strike, $dtype, $auth, $ml, $dbbundle ) =
	dbgetv( @db, 
	  "lat", "lon", "depth", "time", "orid", "etype",
	  "ndef", "mb", "sdepth", "sminax", "smajax", "sdobs", 
	  "strike", "dtype", "auth", "ml", "bundle" );
	
	if( $db[3] == 0 ) { $map_time = $time; }
	last if $opt_m;

	@dbbundle = split( ' ', $dbbundle );

	if( dbex_eval( @db, "count()" ) <= 0 ) {
		$min_delta = -1;
		$min_dist = -1;
		$largest_gap = -1;
	} else {
		$min_delta = dbex_eval( @db, "min(delta)" );
		$min_dist = $min_delta * 111.19;
		$largest_gap = &largest_gap( @dbbundle );
	}

	$time_str = epoch2str( $time, "%y %m %e (%j) %T" );
	$year = epoch2str( $time, "%y" );
# for 68.3% confidence level
	$seh = $smajax * 0.6594;
	$sez = $sdepth;
# for 90% confidence level
#	$seh = $smajax * 0.4660;
#	$sez = $sdepth * 0.6080;
	$err_val = $seh >= $sez ? $seh : $sez;
	$qual = get_qual( $err_val );
	$aeic_dtype = translate_dtype( $dtype, $depth );
	$depth_str = sprintf( "%6.2f%1s", $depth, $aeic_dtype );
	$mag_str = get_magstr( $mb, $ml );
	$georegion = grname( $lat, $lon );
	$seisregion = srname( $lat, $lon );
	$etype = $etype eq "-" ? "E" : $etype;

	if( $etype eq "R" ) {
		if( $opt_v ) {printf STDOUT
			"%27s %8.3f %8.3f %7s %6s" .
			"                                        " . 
			"%1s %s\n",
			$time_str, $lat, $lon, $depth_str,
			$mag_str, $etype, $georegion;}
		printf LIST "%27s \t %8.3f \t %8.3f \t %7s \t %6s " .
			"\t \t \t \t \t \t \t \t %1s \t %s\n",
			$time_str, $lat, $lon, $depth_str,
			$mag_str, $etype, $georegion;
	} else {
		if( $opt_v ) {printf STDOUT "%27s %8.3f %8.3f %7s %6s " .
			"%5.2f %5.2f %5.2f %6.1f %3d %7.2f %s %1s %s \n",
			$time_str, $lat, $lon, $depth_str, $mag_str,
			$sdobs, $seh, $sez, $largest_gap,
			$ndef, $min_dist, $qual, $etype, $georegion;}
		printf LIST "%27s \t %8.3f \t %8.3f \t %7s \t %6s " .
			"\t %5.2f \t %5.2f \t %5.2f \t %6.1f \t %3d " .
			"\t %7.2f \t %s \t %1s \t %s \n",
			$time_str, $lat, $lon, $depth_str, $mag_str,
			$sdobs, $seh, $sez, $largest_gap,
			$ndef, $min_dist, $qual, $etype, $georegion;
	}

	if ( $ml >= 4 || $mb >= 4.0 ) {
		if( $etype eq "R" ) {
			printf BIG "%27s \t %8.3f \t %8.3f \t %7s \t " .
			  "%6s \t \t \t \t \t \t \t \t %1s \t %s\n",
			  $time_str, $lat, $lon, $depth_str, $mag_str, 
			  $etype, $georegion;
		} else {
			printf BIG "%27s \t %8.3f \t %8.3f \t %7s \t " .
			  "%6s \t %5.2f \t %5.2f \t %5.2f \t %6.1f " .
			  "\t %3d \t %7.2f \t %s \t %1s \t %s \n",
			  $time_str, $lat, $lon, $depth_str, $mag_str, 
			  $sdobs, $seh, $sez, $largest_gap, $ndef,
			  $min_dist, $qual, $etype, $georegion;
		}
	}

	if( $etype eq "Q" ) {
		printf QUARRY 
			"%27s \t %8.3f \t %8.3f \t %7s \t %6s \t " .
			"%5.2f \t %5.2f \t %5.2f \t %6.1f \t %3d " .
			"\t %7.2f \t %s \t %1s \t %s \n",
			$time_str, $lat, $lon, $depth_str, $mag_str, 
			$sdobs, $seh, $sez, $largest_gap, $ndef, 
			$min_dist, $qual, $etype, $georegion;
	}

	if( --$linecount < 1 ) {
		if( $opt_v ) {print STDOUT "\n";}
		print LIST "\n";
		$linecount = 5;
	}

}

close( LIST );
close( BIG );
close( Q );

&make_highlights();

if( $opt_t ) { exit 0;}

foreach $figurehash ( @{pfget( "aeic_db2catalog", "figures" )} ) {

	&make_figure( $figurehash, $map_time );

}
