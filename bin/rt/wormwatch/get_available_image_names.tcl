proc get_available_image_names {} {
	global pfname

	pfgetarr Pf $pfname
	set pfelement_names [array names Pf]

	set nonimage_parameters [list canvas_width \
				      canvas_height \
				      waveform_sourcecode \
				      waveform_label \
				      waveform_database \
				      destination_database \
				      image_name \
				      alert_sound \
				      palette]
	
	foreach param $nonimage_parameters {
		set i [lsearch $pfelement_names $param]
		if { $i >= 0 } {
			set pfelement_names [lreplace $pfelement_names $i $i]
		}
	}

	return $pfelement_names
}
