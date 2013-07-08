proc restack_quakes {} {
	global Database plot

	set db [dblookup $Database(db) 0 origin 0 dbALL]
	set db [dbsort $db time] 
	set nrecs [dbquery $db dbRECORD_COUNT]

	for { set i 0 } { $i < $nrecs } { incr i } {
		set tag [dbgetv $db origin $i quake_tag]
		set label_tag [dbgetv $db origin $i label_tag]

		$plot(mapcanvas) raise $tag
		$plot(mapcanvas) raise $label_tag
	}

	$plot(mapcanvas) raise highlight_circle
}
