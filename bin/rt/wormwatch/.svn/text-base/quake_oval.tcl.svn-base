proc quake_oval { x y ml ndef tag label_tag } {
        global plot
 
        set xm [expr $x - 2 * $plot(ovalsize)]
        set ym [expr $y - $plot(ovalsize)]
        set xp [expr $x + 2 * $plot(ovalsize)]
        set yp [expr $y + $plot(ovalsize)]

	$plot(mapcanvas) create oval $xm $ym $xp $yp -fill skyblue -tags $tag

        if {$ml == -999.} {
                set text "N$ndef"
        } else {
                set mlround [format %3.1f $ml]
                set text "ML$mlround"
        }

        $plot(mapcanvas) create text $x $y -text $text \
		 -tags $label_tag 
}
