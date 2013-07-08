#!/bin/sh
# This comment extends to the next line for tcl \
exec aeic_dbap -v -recipe myrecipes -script $0 -tk $*

lappend auto_path $env(ANTELOPE)/data/tcl/library
lappend auto_path $env(AEIC)/data/tcl/library/dbap_tools

proc init_globals {} {
	global filter arid State

	if { ! [info exists arid] } {
		set State(arid) -1
	} else {
		set State(arid) $arid
	}

	if { ! [info exists filter] } { 
		set State(filter) "BW 1 5 3 5"
	} else {
		set State(filter) $filter
	}

	set State(aridt0) -0.5
	set State(aridtw) 3
	set State(tpad) 10
	set State(beam_pre) -4
	set State(beam_post) 4
	set State(output_psname) dbap_out.ps

	set State(file_size) 900
	set State(grid_twin) 2
	set State(window) 1
	set State(threshold) 0.5 

	set State(Arrays) [get_array_codes]
}

proc fkscan_frame { w } { 
	global State

	frame $w

	LabelEntry $w.st -label "Start time:" \
			-textvariable State(starttime)
	pack $w.st -side top -fill x -expand yes

	LabelEntry $w.et -label "End time:  " \
			-textvariable State(endtime)
	pack $w.et -side top -fill x -expand yes

	LabelEntry $w.fs -label "File Size: " \
			-textvariable State(file_size)
	pack $w.fs -side top -fill x -expand yes

	LabelEntry $w.gt -label "Grid twin: " \
			-textvariable State(grid_twin)
	pack $w.gt -side top -fill x -expand yes

	LabelEntry $w.window -label "Window:    " \
			-textvariable State(window)
	pack $w.window -side top -fill x -expand yes

	LabelEntry $w.thresh -label "Threshold: " \
			-textvariable State(threshold)
	pack $w.thresh -side top -fill x -expand yes

	set options $State(Arrays)
	set options [linsert $options 0 all]
	SelectorButton $w.arr -options $options \
		-variable State(array)
	pack $w.arr -fill x

	button $w.b -text "FK scan" -command fkscan -bg chartreuse
	pack $w.b -side top -fill x -expand yes
}

proc fkarid_frame { w } {
	global State

	frame $w 

	LabelEntry $w.arid -label "Arid:          " \
			-textvariable State(arid)
	pack $w.arid -side top -fill x -expand yes

	LabelEntry $w.aridt0 -label "FK start:      " \
			-textvariable State(aridt0)
	pack $w.aridt0 -side top -fill x -expand yes

	LabelEntry $w.aridtw -label "FK time window:" \
			-textvariable State(aridtw)
	pack $w.aridtw -side top -fill x -expand yes

	LabelEntry $w.tpad -label "Time Padding:  " \
			-textvariable State(tpad)
	pack $w.tpad -side top -fill x -expand yes

	LabelEntry $w.beam_pre -label "Beam start:    " \
			-textvariable State(beam_pre)
	pack $w.beam_pre -side top -fill x -expand yes

	LabelEntry $w.beam_post -label "Beam end:      " \
			-textvariable State(beam_post)
	pack $w.beam_post -side top -fill x -expand yes

	button $w.b -text "FK arid" -command fkarid -bg chartreuse
	pack $w.b -side top -fill x -expand yes
}

proc init_windows {} {
	global dbi_name State

	frame .f 
	pack .f

	LabelEntry .f.dbin -label "Database In: " \
			-textvariable dbi_name -width 60
	pack .f.dbin -side top -fill x -expand yes

	LabelEntry .f.dbout -label "Database Out:" \
			-textvariable dbo_name -width 60
	pack .f.dbout -side top -fill x -expand yes

	LabelEntry .f.filter -label "Filter:      " \
			-textvariable State(filter) -width 60
	pack .f.filter -side top -fill x -expand yes

	LabelEntry .f.psfile -label "PS File:     " \
			-textvariable State(output_psname) -width 60
	pack .f.psfile -side top -fill x -expand yes

	frame .f.tools 
	pack .f.tools -side top -fill x -expand yes

	fkarid_frame .f.tools.fkarid
	pack .f.tools.fkarid -side left -fill y

	fkscan_frame .f.tools.fkscan
	pack .f.tools.fkscan -side left -fill y

	button .quit -text Quit -bg red -command "destroy ."
	pack .quit -side top -fill x -expand yes
}

init_globals 
init_windows
