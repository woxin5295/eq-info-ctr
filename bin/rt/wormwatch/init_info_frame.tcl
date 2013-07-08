proc init_info_frame { w } {
        global State
        set State(info_frame) $w
 
        frame $w
 
        label $w.ztime -justify left -text "GMT Time: "
        pack $w.ztime -side top -anchor w 
 
        label $w.ltime -justify left -text "Local Time: "
        pack $w.ltime -side top -anchor w 
 
        label $w.lat -justify left -text "Lat: "
        pack $w.lat -side top -anchor w 
 
        label $w.lon -justify left -text "Lon: "
        pack $w.lon -side top -anchor w
 
        label $w.depth -justify left -text "Depth: "
        pack $w.depth -side top -anchor w
 
        label $w.ndef -justify left -text "0 phases"
        pack $w.ndef -side top -anchor w
 
        label $w.ml -justify left -text "No Ml"
        pack $w.ml -side top -anchor w
}
