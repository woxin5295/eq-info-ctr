proc spotlight_event { orid } {
	global plot

	set db [dbptr_from_orid $orid]

	set lat [dbgetv $db origin [lindex $db 3] lat]
	set lon [dbgetv $db origin [lindex $db 3] lon]
	set tag [dbgetv $db origin [lindex $db 3] quake_tag]

	latlonfocus $lat $lon

	display_summary $orid
}
