proc make_map { w } {
        frame $w
 
        global plot
 
#        canvas $w.c -width $plot(canvas_width) -height $plot(canvas_height) 
        
        canvas $w.c \
          -background brown \
          -scrollregion \
	      "0 0 $plot(image_width_pixels) $plot(image_height_pixels)" \
          -yscrollcommand "$w.sy set" \
          -xscrollcommand "$w.sx set" \
          -yscrollincrement 1 \
          -xscrollincrement 1
 
        set myimage [image create photo \
                        -file $plot(image_file) \
                        -palette $plot(palette)]
 
	set plot(image_handle) $myimage
        set plot(imagetag) [$w.c create image 0 0 -image $myimage -anchor nw]
 
        scrollbar $w.sy -command "$w.c yview" -relief sunk
        scrollbar $w.sx -orient horiz -command "$w.c xview" -relief sunk
 
        pack $w.sx -side bottom -fill x
        pack $w.sy -side right -fill y
        place $w.c -x 0 -y 0 -relwidth 1.0 -width -27 -relheight 1.0 -height -27
 
        $w.c bind $plot(imagetag) <2> "plotDown $w.c %x %y"
        $w.c bind $plot(imagetag) <B2-Motion> "plotMove $w.c %x %y"
 
	set plot(mapframe) $w

        set plot(mapcanvas) $w.c
}
 
proc plotDown {w x y} {
        global plot
 
        set plot(lastX) $x
        set plot(lastY) $y
}
 
proc plotMove {w x y} {
        global plot
 
        set xdiff [expr $plot(lastX)-$x]
        set ydiff [expr $plot(lastY)-$y]
 
        $plot(mapcanvas) xview scroll $xdiff units
        $plot(mapcanvas) yview scroll $ydiff units
 
        set plot(lastX) $x
        set plot(lastY) $y
}
