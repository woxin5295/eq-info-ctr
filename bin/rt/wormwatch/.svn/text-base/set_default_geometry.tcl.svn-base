proc set_default_geometry {} {
	global plot

	set width [expr $plot(control_width) + 30 + $plot(canvas_width)]
	set height [expr $plot(canvas_height) + 27]
	set height [max $height $plot(min_height)]

	wm geometry . "$width\x$height"
}
