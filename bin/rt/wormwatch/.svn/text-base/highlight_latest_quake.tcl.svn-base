proc highlight_latest_quake {} {
        global plot Database State
 
        set orid $State(latest_orid)
 
        set db [dbptr_from_orid $orid]
 
        set lat [dbgetv $db origin [lindex $db 3] lat]
        set lon [dbgetv $db origin [lindex $db 3] lon]
 
        set l [latlon2pix $lat $lon]
        set x [lindex $l 0]
        set y [lindex $l 1]
 
	set l [pixel_confine $x $y]
	set x [lindex $l 0]
	set y [lindex $l 1]
	set direction [lindex $l 2]

	if { $direction == "inside" } {
		set ovalsize $plot(ovalsize)
	} else {
		set ovalsize 0
	}

        set xm [expr $x - 2 * $ovalsize]
        set ym [expr $y - $ovalsize]
        set xp [expr $x + 2 * $ovalsize]
        set yp [expr $y + $ovalsize]
        set lxm [expr $xm - 5]
        set lym [expr $ym - 5]
        set lxp [expr $xp + 5]
        set lyp [expr $yp + 5]
 
        $plot(mapcanvas) delete highlight_circle
 
        $plot(mapcanvas) create oval $lxm $lym $lxp $lyp \
                -width 5 -tags highlight_circle -outline yellow
}
