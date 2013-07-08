if( @ARGV != 1 ) {
	die( "Usage: $0 package_name\n" );
} else {
	$new_package = $ARGV[0];
}

open( MAKEFILES, "find . -name Makefile -print|" );
@Makefiles = <MAKEFILES>;
chomp( @Makefiles );
close( MAKEFILES );

foreach $makefile ( @Makefiles ) {

	if( system( "egrep 'PACKAGE *=' $makefile > /dev/null 2>&1" ) ) {

		# egrep failed, need to add package

		$search = "^\\s*include\\s+\\\$\\(ANTELOPEMAKE\\)";
		$replace = "PACKAGE=$new_package\ninclude \$(ANTELOPEMAKE)";

	} else {

		# egrep succeeded, need to change package

		$search = '^\\s*PACKAGE\\s*=.*';
		$replace = "PACKAGE=$new_package";
	}

	$tmpfile = "/tmp/Makefile_$<_$$";
	system( "mv $makefile $tmpfile" );
	open( IN, "$tmpfile" );
	open( OUT, ">$makefile" );

	while( <IN> ) {
		s/$search/$replace/;
		print OUT;
	}

	close( OUT );
	close( IN );

	unlink( $tmpfile );
}
