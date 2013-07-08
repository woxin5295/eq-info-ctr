# aeic_run_rtm
#
# Simple wrapper script to launch rtm in proper directory
#
# K. Lindquist
# Geophysical Institute
# U. of Alaska
#
# November, 1998

require "pf2.pl";

sub myeval_pf {
        my( $pffile_in ) = @_;
 
        $pffile = $pffile_in;
        $pffile =~ s@.*/([^/])@$1@;
 
        foreach $pfdir ( split( /:/, $ENV{"PFPATH"} ) ) {
 
                $pf_file_path = "$pfdir/$pffile";
 
                if( -r $pf_file_path ) { &eval_pf( $pf_file_path ); }
 
        }
 
        if( $pffile ne $pffile_in ) { &eval_pf( $pffile_in ); }
}

myeval_pf( "aeic_rtsys.pf" );

chdir( $rtexec_run_dirs{"$ENV{'HOST'}"} );

$ENV{'PWD'} = $rtexec_run_dirs{"$ENV{'HOST'}"};

system( "rtm -title $ENV{'HOST'} &" );
