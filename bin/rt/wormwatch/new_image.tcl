proc new_image {} {
	global plot

	set plot(old_image) $plot(image_name)

	set plot(save_current_image) 0

	toplevel .change

	frame .change.f
	pack .change.f -side top -fill x

	frame .change.h
	pack .change.h -side top -fill x

	frame .change.g
	pack .change.g -side top -fill x

	scrollbar .change.f.sy -command ".change.f.l yview" \
			     -relief sunk 
	pack .change.f.sy -side right -fill y

	listbox .change.f.l -yscrollcommand ".change.f.sy set" 
	pack .change.f.l -side top -fill x

	foreach image [get_available_image_names] {
		.change.f.l insert end $image
	}

	checkbutton .change.h.save -text "Save current image" \
		-variable plot(save_current_image)
	pack .change.h.save -side top -fill x

	button .change.g.dismiss -text Dismiss -command "destroy .change"
	pack .change.g.dismiss -side left

	button .change.g.choose -text Choose -command {
		set chosen [.change.f.l get [.change.f.l curselection]]
		if { $plot(old_image) != $chosen } { 
			set plot(image_name) $chosen
			change_image $chosen
		}
		destroy .change
	}
	pack .change.g.choose -side left -fill x
}
