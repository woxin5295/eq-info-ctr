proc set_maxsize {} {
	global plot

	#hardwire scrollbar width of 27 pixels
	set sw 27

	set plot(max_width) [expr $plot(image_width_pixels) + \
					$sw + $plot(control_width)]
	set plot(max_height) [max \
			[expr $plot(image_height_pixels) + \
				$sw + $plot(nonmap_height)] \
			$plot(control_height) \
		 ] 

        wm maxsize . $plot(max_width) $plot(max_height)

	constrain_geometry
}
