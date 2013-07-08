proc fill_quakelist {} {
	global plot Database

	$plot(quake_listbox) delete 0 end

        set db [dblookup $Database(db) 0 origin 0 dbALL]
        set nrecs [dbquery $db dbRECORD_COUNT]
 
        set db [dbsort $db time]

	for { set i 0 } { $i < $nrecs } { incr i } {
        	set lat [dbgetv $db origin $i lat]
        	set lon [dbgetv $db origin $i lon]
        	set ml [dbgetv $db origin $i ml]
        	set time [dbgetv $db origin $i time]
		regexp {([0-9]+/[0-9]+)/[0-9]+ +([0-9]+:[0-9]+):[0-9]+} \
			[strtime $time] junk date time

		set date [format %5s $date]
		set time [format %5s $time]

		set mylat [format %4.1f $lat]
		set mylon [format %6.1f $lon] 
		if {$ml == -999} {
			set mldesc "No Ml"
		} else {
			set mldesc "Ml [format %3.1f $ml]"
		}
		set descript "$mylat  $mylon  $mldesc  $date $time  GMT"

		$plot(quake_listbox) insert end $descript
		set index [expr [$plot(quake_listbox) index end] - 1]

		dbputv $db origin $i listbox_index $index
	} 

}
