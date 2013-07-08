
use Datascope ;

$Db_template = pfget( "aeic_rtsys", "analyzed_events_database" );
$Place_db = "/Seis/databases/places/cities";
$Target_file = "/sunhome/quake/.plan";

$tempdb = "/tmp/fingerdb_$<_$$";

$nquakes = 0; 
$day_epoch = str2epoch( "now" );
while ( $nquakes < 100 ) {
	$dbname = epoch2str( $day_epoch, $Db_template );	
	$day_epoch -= 86400;
	next if( ! -e "$dbname.origin" );
	
	system( "dbsubset $dbname.origin 'ml >= 2' | dbselect - >> $tempdb.origin" );
	$nquakes = dbquery( dbopen_table( "$tempdb.origin", "r+" ), "dbRECORD_COUNT" );
}

system( "dbsort -o -r $tempdb.origin time" );
@db = dbopen_table( "$tempdb.origin", "r+" );
dbtruncate( @db, 100 );
dbclose( @db );

$header = <<EOHEADER;

ALASKA EARTHQUAKE INFORMATION CENTER (AEIC) PRELIMINARY EPICENTER DETERMINATIONS

This listing includes the most recent 100 earthquakes with magnitudes of 2.0
or greater processed by the AEIC, a State of Alaska/USGS cooperative seismic 
observatory located at the Geophysical Institute of the University of Alaska 
Fairbanks. 

***************************************************************************
***************************************************************************
These earthquake locations are the result of routine Monday-Friday 9am-5pm 
daily processing. For timely (20-60 minutes after the event, 24hrs/7 days a 
week) information releases on large events or felt events, please see 

http://www.aeic.alaska.edu/cgi-bin/release_info.pl

***************************************************************************
***************************************************************************

Aleutian earthquakes located outside of the AEIC are based on data obtained from
the USGS National Earthquake Information Center (NEIC).  

QUAL is the location quality where A = good, D = poor, E = error or unknown, and
* = NEIC location and magnitude.  NEIC magnitudes can be mb, Ms, or Mw.

For more information see http://www.aeic.alaska.edu/ or send Email to 
aeic\@giseis.alaska.edu.  A listing that includes magnitudes less than 2.0 can be
obtained from "finger allquake\@giseis.alaska.edu".

The dates and times given below are in Universal Coordinated Time (UTC),
previously known as Greenwich Mean Time (GMT) for Greenwich, England.
Subtract 9 hours to obtain Alaska Standard Time in the winter and subtract
8 hours to obtain Alaska Daylight Time during the summer months.  Adjust this
correction for time zones that are nearer or farther from England.

UTC DATE   UTC TIME  LAT.   LON.   DEPTH MAG QUAL   COMMENTS     
year-mm-dd hh:mm:ss (deg.) (deg.)  (km)  (ml)
----  ---- -------- ------ ------- ----- --- ---- ---------------------------------
EOHEADER

open( T, "> $Target_file" );

print T $header;

@db = dbopen( "$tempdb", "r" );

@db = dblookup( @db, "", "origin", "", "" );
$nrecs = dbquery( @db, "dbRECORD_COUNT" );

for( $db[3] = 0; $db[3] < $nrecs; $db[3]++ ) {
	( $time, $lat, $lon, $depth, $ml ) =
		dbgetv( @db, "time", "lat", "lon", "depth", "ml" );

	@places = dbopen( "$Place_db", "r" );
	@places = dblookup( @places, 0, "places", 0, 0 );
	@p = dbsort( @places, "distance( lat, lon, $lat, $lon ) " );
	$p[3] = 0;
	$distance_km = dbex_eval( @p, "distance(lat,lon,$lat,$lon)*111.195" );
	$azimuth = dbex_eval( @p, "azimuth(lat,lon,$lat,$lon)" );
	$place = dbgetv( @p, "place" );
	dbclose( @places );

	if( $lon > 0 ) {
		$londir = "E";
	} else {
		$londir = "W";
		$lon *= -1;
	}
	if( $lat > 0 ) {
		$latdir = "N";
	} else {
		$latdir = "S";
		$lat *= -1;
	}

	printf T "%s %6.2f%s %6.2f%s %5.1f %3.1f     %6.1f km %s of %s\n",
		epoch2str( $time, "%Y/%m/%d %H:%M:%S" ),
			   $lat, $latdir, $lon, $londir, $depth, $ml,
			   $distance_km, compass_from_azimuth($azimuth), $place;
}

dbclose( @db );

close( T );

sub compass_from_azimuth {
	local( $azimuth ) = @_;

	while( $azimuth < 0. ) { $azimuth += 360.; };
	while( $azimuth > 360. ) { $azimuth -= 360.; };

	if( $azimuth >= 348.75 || $azimuth < 11.25 ) {

		return "N";		# 0.00

	} elsif( $azimuth >= 11.25 && $azimuth < 33.75 ) {

		return "NNE";		# 22.50

	} elsif( $azimuth >= 33.75 && $azimuth < 56.25 ) {

		return "NE";		# 45.00	

	} elsif( $azimuth >= 56.25 && $azimuth < 78.75 ) {

		return "ENE";		# 67.50	

	} elsif( $azimuth >= 78.75 && $azimuth < 101.25 ) {

		return "E";		# 90.00	

	} elsif( $azimuth >= 101.25 && $azimuth < 123.75 ) {

		return "ESE";		# 112.50	

	} elsif( $azimuth >= 123.75 && $azimuth < 146.25 ) {

		return "SE";		# 135.00	

	} elsif( $azimuth >= 146.25 && $azimuth < 168.75 ) {

		return "SSE";		# 157.50	

	} elsif( $azimuth >= 168.75 && $azimuth < 191.25 ) {

		return "S";		# 180.00	

	} elsif( $azimuth >= 191.25 && $azimuth < 213.75 ) {

		return "SSW";		# 202.50	

	} elsif( $azimuth >= 213.75 && $azimuth < 236.25 ) {

		return "SW";		# 225.00 	

	} elsif( $azimuth >= 236.25 && $azimuth < 258.75 ) {

		return "WSW";		# 247.50	

	} elsif( $azimuth >= 258.75 && $azimuth < 281.25 ) {

		return "W";		# 270.00	

	} elsif( $azimuth >= 281.25 && $azimuth < 303.75 ) {

		return "WNW";		# 292.50	

	} elsif( $azimuth >= 303.75 && $azimuth < 326.25 ) {

		return "NW";		# 315.00	

	} elsif( $azimuth >= 326.25 && $azimuth < 348.75 ) {

		return "NNW";		# 337.50	
	} 

	die( "Faulty logic in compass_from_azimuth subroutine\n" );
}
