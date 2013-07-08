proc display_summary { orid } {
        global plot
 
	set db [dbptr_from_orid $orid]

        set tag [dbgetv $db origin [lindex $db 3] quake_tag]
        set label_tag [dbgetv $db origin [lindex $db 3] label_tag]
 
        $plot(mapcanvas) itemconfig $tag -fill red

	$plot(mapcanvas) raise $tag
	$plot(mapcanvas) raise $label_tag
 
        describe_quake $orid pink
}
