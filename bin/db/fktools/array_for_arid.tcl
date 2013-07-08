
proc array_for_arid { arid } {
	global dbi_name 

	set db [dbopen_database $dbi_name r]
	set db [dblookup $db "" "network" "" ""]
	set db [dbsubset $db "nettype == \"ar\""]
	set dbaff [dblookup $db "" "affiliation" "" ""]
	set db [dbjoin $db $dbaff]
	set dbarr [dblookup $db "" "arrival" "" ""]
	set db [dbjoin $db $dbarr]
	set db [dbsubset $db "arid == $arid"]
	set nrecs [dbquery $db dbRECORD_COUNT]
	if {$nrecs > 0} {
		set thisarray [dbgetv $db "" 0 net]
	} else {
		set thisarray "-"
	}

	return $thisarray
}
