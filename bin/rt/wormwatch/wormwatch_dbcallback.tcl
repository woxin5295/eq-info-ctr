proc wormwatch_dbcallback { db } {
        set table [dbquery $db dbTABLE_NAME]
        switch $table {
        origin {
		set i -504
		set lat [dbgetv $db 0 $i lat]
		set lon [dbgetv $db 0 $i lon]
		set depth [dbgetv $db 0 $i depth]
		set time [dbgetv $db 0 $i time]
		set ml [dbgetv $db 0 $i ml]
		set ndef [dbgetv $db 0 $i ndef]
                set orid [new_quake $lat $lon $depth $time $ml $ndef]
		map_quake $orid
		describe_quake $orid yellow
		audible_alert
        }
        default {}
        }
}
