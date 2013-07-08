proc fkscan {} {
	global State dbi_name dbo_name

	if { ! [info exists dbi_name] || $dbi_name == "" } {
		tkdialog .e Error "No input database specified!" Ok
		return
	}

	if { ! [info exists dbo_name] || $dbo_name == "" } {
		tkdialog .e Error "No output database specified!" Ok
		return
	}

	set_db_in $dbi_name
	set_db_out $dbo_name

	if { [info exists gr] == 1} {
		free $gr
		unset gr
	}
	
	set stime [str2epoch $State(starttime)]
	set etime [str2epoch $State(endtime)]
	set tw [min $State(file_size) [expr $etime - $stime]]

	if { $State(array) == "all" } {
		set myarraylist $State(Arrays)
	} else {
		set myarraylist $State(array)
	}

	foreach Array $myarraylist {

		set tss $stime

		loop i 0 1000000 { 
			set tes [expr $tss + $tw] 
			set gr [process \
				-recipe myxy_$Array \
				-filter $State(filter) \
				-ts $tss \
				-te [expr $tes+$State(window)] \
				-grid_twin $State(window) \
				-samprate 0.0 \
				-thresh $State(threshold) \
				-geq \
				]
			write $gr -write_raw_power_trace 
			write $gr -write_raw_azimuth_trace
			write $gr -write_raw_slow_trace
			write $gr -write_power_trace
			free $gr 
			set tss $tes 
			if { $tss >= $etime } break 
		}
	}
}
