proc lookup_source_db { lat lon depth time ml ndef } {
	global State

	set source_orid -1
	set source_db "-"

	if { $State(get_iceworm_sourcedbs) } {
		set ymd [epoch_to_yymmdd $time]
		set dbname "$State(iceworm_dbbase)$ymd"
		set orid [orid_for_stats $dbname $lat $lon \
					$depth $time $ml $ndef]
		if { $orid > 0 } {
			set source_orid $orid
			set source_db $dbname
		}
	}

	return [list $source_orid $source_db]
}

proc orid_for_stats { dbname lat lon depth time ml ndef } {

	set orid -1

	set db [dbopen $dbname r]
	set db [dblookup $db 0 origin 0 0]
	set recno [dbfind $db -1 0 "time == $time"]

	if { $recno >= 0 } {
		set orid [dbgetv $db origin $recno orid]
	}

	return $orid
}
