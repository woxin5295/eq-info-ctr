require "getopts.pl";

$opt_v = 0 ;
$opt_x = 0 ;
$opt_s = 0 ;
if ( ! &Getopts('xvs') || @ARGV < 3 )
     {
     die "Usage: aeic_dbconcat_event [-x] [-s] [-v] database database [ database ...] destination_database\n" ;
     }
    
@TABLES = ( "arrival", "event", "origin", "origerr", "assoc",
	    "netmag", "stamag", "wfmeas", "beam", "centryd", "fplane", 
	    "moment", "remark", "seedformat", "seedindex", 
	    "stassoc", "wfedit", "wfrms", ) ; 
@EXTREF_TABLES = ( "wfdisc", "fkgrid", "stgrid", "wftape", "wftar", "wftag" ) ;
@QUASISTATIC_TABLES = ( "site", "sitechan", "sensor", "calibration",
			"instrument", "affiliation", "network" );

@IDS = ( "arid", "evid", "orid", "magid", "commid", "stassid" ) ; 
@EXT_IDS = ( "wfid" ) ;
@QS_IDS = ( "chanid", "inid" ) ;
%IDDEFTBLS = ( "arid", "arrival", 
	       "evid", "event",
	       "orid", "origin", 
	       "magid", "netmag",
	       "commid", "remark", 
	       "stassid", "stassoc",
	       "chanid", "sitechan",
	       "inid", "instrument",
	       "wfid", "wfdisc");
$TMPDIR = "/tmp/concat$$" ; 

$NEW = pop(@ARGV) ; 
if ( -d "$NEW" ) { 
    die "must specify new database name (not just directory).\n" ; 
}

if( $opt_x )
    {
    @TABLES = ( @TABLES, @EXTREF_TABLES ) ;
    @IDS = ( @IDS, @EXT_IDS ) ;
    }

if( $opt_s )
    {
    @TABLES = ( @TABLES, @QUASISTATIC_TABLES ) ;
    @IDS = ( @IDS, @QS_IDS ) ;
    }

mkdir ($TMPDIR, 0775) ;
$OLD = $ENV{"PWD"} ; 

foreach $table ( @TABLES )
    {
    if ( -f "$NEW.$table" ) { $exist = 1 } 
    }
if ( $exist ) 
    {
    print STDERR "Will destroy database $NEW -- ok? " ; 
    $reply = <STDIN> ;
    if ( $reply !~ /y|yes|ok/ ) 
	{
	die "Please choose a new output database name." ;
	}
    }

foreach $db ( @ARGV )  
    {
    $dbname = $db ;
    $dbname =~ s".*/"" ; 
    foreach $table ( @TABLES )
	{
	if ( -f "$db.$table" )
	     {
	     print STDERR "copying $db.$table\n" ;
	     $cmd =  "cp $db.$table $TMPDIR; chmod +w $TMPDIR/$dbname.$table" ; 
	     if ( $opt_v ) { print STDERR "$cmd\n" ; } 
	     system ( $cmd ) ; 
	     }
	}

    print STDERR "Renumbering ids in copy of $db .." ;
    $dbt = "$TMPDIR/$dbname" ;

    foreach $id ( @IDS ) 
	{
	if ( -f "$db.$IDDEFTBLS{$id}" )
	     {
	     eval ( "\$next_id = `dbnextid $NEW $id`" ) ; 
	     print STDERR "\tstarting $id in $NEW with $next_id\n" ; 
	     $cmd = "dbfixids $dbt $id $next_id" ; 
	     if ( $opt_v ) { print STDERR "$cmd\n" ; } 
	     system ( $cmd ) ; 
	     }
	}

    foreach $table ( @TABLES )
	{
	print STDERR "$table .." ;
	if ( -f "$dbt.$table" )
	     {
	     $cmd = "cat $dbt.$table >> $NEW.$table" ; 
	     if ( $opt_v ) { print STDERR "$cmd\n" ; } 
	     system ( $cmd ) ; 
	     }
	}

    unlink "$NEW.lastid" ; 
    print STDERR "\n" ; 
    }

unlink "$OLD/$NEW.lastid" ; 
system ( "rm -rf $TMPDIR" ) ; 


# $Id: aeic_dbconcat_event.xpl,v 1.3 2002-04-26 18:41:04 mitch Exp $ 
