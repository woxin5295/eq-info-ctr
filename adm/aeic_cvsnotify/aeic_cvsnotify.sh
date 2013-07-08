: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

while ( <STDIN> ) {
    if ( /Update of (.*)/ ) { 
	$dir = $1 ; 
    } elsif ( /In directory (.*)/ ) { 
	$from = $1 ;
    } elsif ( /^Log Message:/ ) {
	@explanation = <STDIN> ; 
	last ; 
    } else {
	push ( @the_rest, $_ ) ; 
    }
}

# open ( RECIPIENTS, "/opt/jspc/dev/data/monitors" ) ; 
# while ( <RECIPIENTS> ) {
#     chop ;
#     ($match, $who) = split ( ' ', $_, 2 ) ; 
#     if ( $dir =~ m"$match" ) {
# 	push ( @recipients, $who ) ; 
#    }
# }

push( @recipients, "natasha\@giseis.alaska.edu", "mitch\@giseis.alaska.edu","glenn\@giseis.alaska.edu" ); # HARD-WIRE

$maindir = $ARGV[0] ;
$maindir =~ s"antelope/src/"" ;
$subject = "CVS:$maindir/ " ;
$i = 1 ;
while ( length ($subject) < 60 ) {
    $subject .= " " . $ARGV[$i++] ;
}

open ( MAIL, "| /usr/bin/mailx -s '$subject' @recipients" ) ;
print MAIL <<"EOF";

@explanation

@the_rest

Files Modified:

    @ARGV

From:
    $from 

-- CVS
EOF
