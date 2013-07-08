proc new_quake { lat lon depth time ml ndef } {
        global Database
 
	set db [get_internal_database]

        set db [dblookup $db 0 origin 0 dbNULL]
        set nullrecord [dbget $db]
        set recno [dbadd $db $nullrecord]
        set db [lreplace $db 3 3 $recno]
 
	dbputv $db origin $recno lat $lat lon $lon depth $depth \
				time $time ml $ml ndef $ndef

        set orid [dbnextid $db orid]
        dbputv $db origin $recno orid $orid 

	set l [lookup_source_db $lat $lon $depth $time $ml $ndef]
		 
	set source_orid [lindex $l 0]
	set source_db [lindex $l 1]

	dbputv $db origin $recno source_orid $source_orid
	dbputv $db origin $recno source_db $source_db
 
        set_latest_quake
 
	fill_quakelist

        return $orid
}
