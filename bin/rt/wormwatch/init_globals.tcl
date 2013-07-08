proc init_globals {} {
 
        global State env
        global argv0 argc argv 
        global plot
 
        global pfname
        set pfname [exec basename $argv0]
 
        if {$argc == 1} {
                set State(hypo_source) [lindex $argv 0]
 
                # should use dbopen_table
                if { [file exists $State(hypo_source).origin] } {
 
                        set State(hypo_sourcetype) database
                        set State(showcmd) show_database
 
                } else {
                        set State(hypo_sourcetype) orbserver 
                        set State(showcmd) init_orb
                        set plot(waveform_sourcecode) \
                                [pfget $pfname waveform_sourcecode]
                        set plot(waveform_label) [pfget $pfname waveform_label]
                }
        } else {
                usage
                exit 1
        }
 
	global tcl_precision
	set tcl_precision 12

	set plot(image_name) [pfget $pfname image_name]

	read_imagespecs $plot(image_name) 

        set plot(ovalsize) 10
	set plot(arrow_length) 30
        set plot(canvas_width) [pfget $pfname canvas_width]
        set plot(canvas_height) [pfget $pfname canvas_height]
        set plot(palette) [pfget $pfname palette]
	keylset plot(saved_images) none none
 
	if { $plot(canvas_width) > $plot(image_width_pixels) } {
		set plot(canvas_width) $plot(image_width_pixels)
	}
 
	if { $plot(canvas_height) > $plot(image_height_pixels) } {
		set plot(canvas_height) $plot(image_height_pixels)
	}

        set State(alert_sound) [pfget $pfname alert_sound]

        set State(dest_db) [pfget $pfname destination_database]
	set State(primary_system) [pfget aeic_rtsys primary_system]
	set State(waveforms_from) $State(primary_system) 

	if { $State(dest_db) == "" } {
		set State(dest_db) "$env(HOME)/alarm"
	}
	if { [info exists env(SITE_DB)] } {
        	set State(site_db) $env(SITE_DB)
	} else {
		tk_dialog .dialog "Sorry..." \
			"Environment variable SITE_DB not defined" \
			error 0 Dismiss
		Quit
	}

	set State(get_iceworm_sourcedbs) 0

	if { [info exists env(EW_PARAMS)] && [info exists env(EW_OUTPUT)] } {
		if { [file exists "$env(EW_PARAMS)/css_report.d"] } {
			exec cp $env(EW_PARAMS)/css_report.d /tmp/css_report.pf
			pushd /tmp
			set dbbase [pfget css_report Database]
			set dbbase [strip_quotes $dbbase]
			unlink css_report.pf
			popd
			set State(iceworm_dbbase) \
				"$env(EW_OUTPUT)/seg/$dbbase\_"
			set State(get_iceworm_sourcedbs) 1
		}
	}
}
