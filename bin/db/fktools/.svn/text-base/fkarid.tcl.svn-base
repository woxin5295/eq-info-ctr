proc fkarid {} {

	global State dbi_name

	if { ! [info exists dbi_name] || $dbi_name == "" } {
		tkdialog .e Error "No input database specified!" Ok
		return
	}

	if { $State(arid) < 0 } {
		tkdialog .e Error "Invalid arid!" Ok
		return
	}

	set_db_in $dbi_name 

	set array [array_for_arid $State(arid)]

	set gr [process -recipe myxy_$array -filter $State(filter) \
		-arid $State(arid) -arid_t0 $State(aridt0) \
		-arid_tw $State(aridtw) -tpad $State(tpad)]

	plot $gr -xlow 0.5 -xdim 2.5 -ylow 1.0 -orient portrait \
		-psfile $State(output_psname)
	
	set azimuth [get $gr azimuth]
	set slow [get $gr slowness]
	set power [get $gr power] 

	set ts [expr [get $gr ts] + $State(beam_pre)]
	set te [expr [get $gr ts] + $State(beam_post)]

	set bm [process -recipe mybeam_$array -ts $ts -te $te \
			-azimuth $azimuth -slow $slow -power $power]

	set marks "[get $gr ts] [get $gr te]"
	set filter [get $gr filter]

	plot $bm -xlow 0.5 -ylow 6.5 -ylowtr 7.5 -xdim 6.5 -ydim 1.0 \
		-ydimtr 1.0 -title yes -time_label no -time_marks $marks \
		-filter $filter

	plot $bm -xlow 0.5 -ylow 4.5 -ylowtr 5.5 -xdim 6.5 -ydim 1.0 \
		-ydimtr 1.0 -title no -time_label yes -time_marks $marks \
		-filter none


	plot legend -xdim 3.0 -ydim 0.3 -xlow 4.0 -ylow 1.0 

	plot close 
}
