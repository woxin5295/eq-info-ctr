if ( @ARGV != 1 )
    { die ( "Usage: $0 eps_file\n" ) ; }

use Datascope ;

system("alchemy $ARGV[0] -Zm4 -Zr270 -t -d17 ---K -o");
 

