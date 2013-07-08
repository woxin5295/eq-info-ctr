proc init_quakelist { w } {
	global plot

	frame $w

	scrollbar $w.s -command "$w.l yview" -relief sunk
	pack $w.s -side right -fill y

	listbox $w.l -width 35 -yscrollcommand "$w.s set"
	pack $w.l -side right -fill y
	
	set plot(quake_listbox) $w.l

	bind $w.l <ButtonPress-1> "init_quakelist_spotlight $w %y"
	bind $w.l <ButtonRelease-1> "init_quakelist_revert $w"
	bind $w.l <ButtonPress-3> "init_quakelist_respond $w %y"
}

proc orid_from_ycoord { w y } {
	global plot Database

	set index [$plot(quake_listbox) nearest $y]
	
	set db [dblookup $Database(db) 0 origin 0 dbALL]
	set recno [dbfind $db -1 0 "listbox_index == $index"]
	set db [lreplace $db 3 3 $recno]
	
	set orid [dbgetv $db origin [lindex $db 3] orid]

	return $orid
}

proc init_quakelist_revert { w } {
	global plot

	$w.l selection clear 0 end

	revert_to_latest $plot(spotlighted_orid)

	$plot(mapcanvas) xview moveto $plot(spotlight_lastx)
	$plot(mapcanvas) yview moveto $plot(spotlight_lasty)

	set plot(spotlighted_orid) -1
}

proc init_quakelist_spotlight { w y } {
	global plot 
	
	set plot(spotlight_lastx) [lindex [$plot(mapcanvas) xview] 0]
	set plot(spotlight_lasty) [lindex [$plot(mapcanvas) yview] 0]

	set orid [orid_from_ycoord $w $y]

	spotlight_event $orid

	set plot(spotlighted_orid) $orid 
}

proc init_quakelist_respond { w y } {

	set orid [orid_from_ycoord $w $y]

	respond $orid
}
