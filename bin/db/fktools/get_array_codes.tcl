
proc get_array_codes {} {
	global dbi_name State

	set db [dbopen_database $dbi_name r]
	set db [dblookup $db "" "network" "" ""]
	set db [dbsubset $db "nettype == \"ar\""]
	set narrays [dbquery $db dbRECORD_COUNT]
	if {$narrays == 0} {
		echo No arrays defined in $dbi_name
		echo Bye
		exit 1
	} else {
		for {set i 0} {$i < $narrays} {incr i} {
			set net [dbgetv $db "" $i net]
			lappend arrays $net
			set State(arraynames($net)) "[join [dbgetv $db "" $i netname]]"
		}
	}
	return $arrays
}
