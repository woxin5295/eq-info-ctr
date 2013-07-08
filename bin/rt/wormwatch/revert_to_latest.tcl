proc revert_to_latest { orid } {
        global plot State
        
	set db [dbptr_from_orid $orid]

        set tag [dbgetv $db origin [lindex $db 3] quake_tag]
 
        $plot(mapcanvas) itemconfig $tag -fill skyblue
 
	restack_quakes

        describe_quake $State(latest_orid) yellow
}
