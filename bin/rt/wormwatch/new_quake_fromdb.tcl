proc new_quake_fromdb { dbin } {
        global Database State

	set db [get_internal_database]

        set db [dblookup $db 0 origin 0 dbNULL]
#        set nullrecord [dbget $db]
#        set recno [dbadd $db $nullrecord]
        set recno [dbaddnull $db]
        set db [lreplace $db 3 3 $recno]
 
	set css30fields [list lat lon depth time evid jdate \
		nass ndef ndp grn srn etype depdp dtype mb mbid \
		ms msid ml mlid algorithm auth commid lddate]

	foreach field $css30fields {
		set value [dbgetv $dbin origin [lindex $dbin 3] $field]
		dbputv $db origin $recno $field $value
	}

        set orid [dbnextid $db orid]
        dbputv $db origin $recno orid $orid 

	if { $State(hypo_sourcetype) == "orbserver" } {

		set lat [[dbgetv $dbin origin [lindex $dbin 3] lat]]
		set lon [[dbgetv $dbin origin [lindex $dbin 3] lon]]
		set depth [[dbgetv $dbin origin [lindex $dbin 3] depth]]
		set time [[dbgetv $dbin origin [lindex $dbin 3] time]]
		set ml [[dbgetv $dbin origin [lindex $dbin 3] ml]]
		set ndef [[dbgetv $dbin origin [lindex $dbin 3] ndef]]

		set l [lookup_source_db lat lon depth time ml ndef]

		set source_orid [lindex $l 0]
		set source_db [lindex $l 1]

	} else {

		set source_orid [dbgetv $dbin origin [lindex $db 3] orid]

		set source_db [dbquery $dbin dbDATABASE_NAME]
	}

	dbputv $db origin $recno source_orid $source_orid
	dbputv $db origin $recno source_db $source_db

        set_latest_quake
 
	fill_quakelist

        return $orid
}
