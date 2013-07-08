proc quake_arrow { x y ml ndef tag label_tag direction } {
        global plot
 
	set length $plot(arrow_length)
        switch $direction {
        up {
                up_arrow $plot(mapcanvas) $x $y $length skyblue $tag
                set anchor n
                set arrow_x $x
                set arrow_y [expr $y + $length]
        }
        down {
                down_arrow $plot(mapcanvas) $x $y $length skyblue $tag
                set anchor s
                set arrow_x $x
                set arrow_y [expr $y - $length]
        }
        right {
                right_arrow $plot(mapcanvas) $x $y $length skyblue $tag
                set anchor e
                set arrow_x [expr $x - $length]
                set arrow_y $y
        }
        left {
                left_arrow $plot(mapcanvas) $x $y $length skyblue $tag
		set anchor w
		set arrow_x [expr $x + $length]
		set arrow_y $y
	}
	default {
		echo direction $direction not implemented in quake_arrow
	}
	}

        if {$ml == -999.} {
                set text "N$ndef"
        } else {
                set mlround [format %3.1f $ml]
                set text "ML$mlround"
        }

        $plot(mapcanvas) create text $arrow_x $arrow_y -text $text \
		 -tags $label_tag -anchor $anchor
}
