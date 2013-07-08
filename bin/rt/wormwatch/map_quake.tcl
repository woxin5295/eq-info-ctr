proc map_quake { orid } {
        global plot State
 
        set db [dbptr_from_orid $orid]
 
        set lat [dbgetv $db origin [lindex $db 3] lat]
        set lon [dbgetv $db origin [lindex $db 3] lon]
        set ml [dbgetv $db origin [lindex $db 3] ml]
        set ndef [dbgetv $db origin [lindex $db 3] ndef]
 
        set l [latlon2pix $lat $lon]
        set x [lindex $l 0]
        set y [lindex $l 1]
 
        set tag orid_$orid
        set label_tag orid_$orid\_label
 
	dbputv $db origin [lindex $db 3] quake_tag $tag
	dbputv $db origin [lindex $db 3] label_tag $label_tag

	set l [pixel_confine $x $y]
	set x [lindex $l 0]
	set y [lindex $l 1]
	set direction [lindex $l 2]

	if { $direction == "inside" } {
		quake_oval $x $y $ml $ndef $tag $label_tag 
	} else {
		quake_arrow $x $y $ml $ndef $tag $label_tag $direction
	}
 
        $plot(mapcanvas) bind $tag <Any-Enter> "display_summary $orid"
        $plot(mapcanvas) bind $label_tag <Any-Enter> "display_summary $orid"
        $plot(mapcanvas) bind $tag <ButtonPress-1> "respond $orid"
        $plot(mapcanvas) bind $label_tag <ButtonPress-1> "respond $orid"
        $plot(mapcanvas) bind $tag <Any-Leave> "revert_to_latest $orid"
        $plot(mapcanvas) bind $label_tag <Any-Leave> "revert_to_latest $orid"
 
        latlonfocus $lat $lon
 
        highlight_latest_quake
 
        describe_quake $State(latest_orid) yellow
}
