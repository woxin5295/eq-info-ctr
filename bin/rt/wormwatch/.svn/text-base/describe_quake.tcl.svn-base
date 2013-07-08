proc describe_quake { orid color } {
 
        set db [dbptr_from_orid $orid]
 
        global State
	$State(info_frame) configure -bg $color
 
        set lat [dbgetv $db origin [lindex $db 3] lat]
        set lon [dbgetv $db origin [lindex $db 3] lon]
        set depth [dbgetv $db origin [lindex $db 3] depth]
        set time [dbgetv $db origin [lindex $db 3] time]
        set ndef [dbgetv $db origin [lindex $db 3] ndef]
        set ml [dbgetv $db origin [lindex $db 3] ml]
 
        if {$ml == -999} {set mldesc "No Ml"} else {set mldesc "Ml $ml"}

	set local_time [fmtclock [int $time] "%m/%d/%Y %I:%M:%S %p %Z"]
 
        $State(info_frame).lat configure -bg $color -text "Lat: $lat"
        $State(info_frame).lon configure -bg $color -text "Lon: $lon"
        $State(info_frame).depth configure -bg $color -text "Depth: $depth km"
        $State(info_frame).ztime configure -bg $color -text \
			"GMT Time: [strtime $time] GMT"
        $State(info_frame).ltime configure -bg $color -text \
			"Local Time: $local_time"
        $State(info_frame).ndef configure -bg $color -text "$ndef phases"
        $State(info_frame).ml configure -bg $color -text "$mldesc"
}
