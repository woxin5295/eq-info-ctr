proc init_windows {} {
        global State plot
 
        label .title -text Wormwatch
 
        if { $State(hypo_sourcetype) == "orbserver" } {
 
                frame .rtdframe
                pack .rtdframe -side bottom -anchor w -fill x
                set plot(rtdframe) .rtdframe
        } 
 
        frame .control 
 
        button .control.quit -bg red -command Quit -text "Quit"
        pack .control.quit -side top -fill x 
 
        set mysource \
           "Hypocenter source:\n$State(hypo_sourcetype) $State(hypo_source)"
        label .control.source -text $mysource -fg blue
        pack .control.source -side top -anchor w
 
        init_info_frame .control.info 
        pack .control.info -side top -fill x
	
	init_quakelist .control.qlist
	pack .control.qlist -side top -fill x
	
	button .control.change -text "Change Image" -command new_image
	pack .control.change -side top -fill x
	
        pack .title -side top -fill x
	tkwait visibility .title
	set plot(title_height) [winfo height .]

	if { $State(hypo_sourcetype) == "orbserver" } {
		set plot(rtd_height) 42
		set plot(rtd_title_height) $plot(title_height)
	} else {
		set plot(rtd_height) 0
		set plot(rtd_title_height) 0
	}

        pack .control -side left -fill y
	tkwait visibility .control
	set plot(control_height) [winfo height .]
	set plot(control_width) [winfo width .]
	set_minsize
	pack forget .control

	set plot(nonmap_width) [expr $plot(control_width)]
	set plot(nonmap_height) [expr $plot(title_height) + \
				      $plot(rtd_title_height) + \
				      $plot(rtd_height)]

	set_maxsize

        make_map .map

	place .map -x 0 -y $plot(title_height) \
		-relwidth 1.0 \
		-relheight 1.0 \
		-width [expr - $plot(nonmap_width)] \
		-height [expr - $plot(nonmap_height)]
	place .control -anchor ne -relx 1.0 -y $plot(title_height) 

	set_default_geometry

	tkwait visibility .map

	set plot(window_height) [winfo height .]
	set plot(window_width) [winfo width .]

	bind . <Configure> resize_wormwatch
}
