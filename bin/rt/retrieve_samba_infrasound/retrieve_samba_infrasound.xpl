use Datascope ;

sub update_pffile {
	( $pfpid ) = @_;

	open( P, ">$pfname" );
	if( $epoch > $latest ) {
		print P "latest_time $epoch\n";
	} else {
		print P "latest_time $latest\n";
	}
	print P "user $user\n";
	print P "location $location\n";
	print P "pid $pfpid\n";
	close( P );
}

if( $#ARGV != 0 ) {
	die( "Usage: retrieve_samba_infrasound orbname\n" );
} else {
	$orbname = $ARGV[0];
} 

$latest = pfget( "retrieve_samba_infrasound", "latest_time" );
$user = pfget( "retrieve_samba_infrasound", "user" );
$location = pfget( "retrieve_samba_infrasound", "location" );
$pid = pfget( "retrieve_samba_infrasound", "pid" );

$pscount = `/usr/bin/ps -p $pid | grep -v PID | wc -l`;
chomp( $pscount );
if( $pscount >= 1 && $pid != 0 ) {
	die( "retrieve_samba_infrasound appears to be already running. Bye.\n" );
}

$pfname = `pfecho -w retrieve_samba_infrasound | tail -1`;
chomp( $pfname );
$pfname = `abspath $pfname`;
chomp( $pfname );

update_pffile( $$ );

open( L, "smbclient $location -U $user -c ls|" );
	@list = <L>;
close( L );

@list = grep( /\.dmx/, @list );
@list = grep( s/^.*\s+(\d+\.dmx).*$/$1/, @list );
@list = grep( s/\.dmx//, @list );

chdir( "/tmp" );

$epoch = $latest;

foreach $file ( @list ) {

	chomp( $file );
	$convert = $file;

	substr($convert,4,0,"-");
	substr($convert,7,0,"-");
	substr($convert,10,0," ");
	substr($convert,13,0,":");
	substr($convert,16,0,":");

	undef $epoch;
	eval( "\$epoch = str2epoch( \"$convert\" );" );
	if( ! defined( $epoch ) ) {
		print STDERR "Skipping $file: str2epoch problem\n";
		next;
	}

	next if( $epoch <= $latest );
	
	system( "smbclient $location -U $user -c 'get $file.dmx'" );
	system( "dmx2orb $file.dmx $orbname" );
	unlink( "$file.dmx" );

	update_pffile( $$ );
}

update_pffile( 0 );
