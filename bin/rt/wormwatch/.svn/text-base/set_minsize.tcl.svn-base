proc set_minsize {} {
	global plot

	set plot(min_height) [expr $plot(control_height) + \
				   $plot(rtd_title_height) + \
				   $plot(rtd_height)]

	#hard-wire scrollbar width and 3 pixels of image into minimum width
	set plot(min_width) [expr $plot(control_width) + 30]

        wm minsize . $plot(min_width) $plot(min_height)
}
