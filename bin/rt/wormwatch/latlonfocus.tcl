proc latlonfocus { lat lon } {
        global plot
 
        set l [latlon2pix $lat $lon]
        set xfocus [lindex $l 0]
        set yfocus [lindex $l 1]
 
        set l [$plot(mapcanvas) xview] 
        set ctr_x [expr int( [lindex $l 0] * $plot(image_width_pixels) + \
                        $plot(canvas_width) / 2 )]
 
        set l [$plot(mapcanvas) yview] 
        set ctr_y [expr int( [lindex $l 0] * $plot(image_height_pixels) + \
                        $plot(canvas_height) / 2 )]
 
        $plot(mapcanvas) xview scroll [expr $xfocus - $ctr_x] units
        $plot(mapcanvas) yview scroll [expr $yfocus - $ctr_y] units
}
