proc pixel_confine { x y } {
	global plot

	if { $x < 0 } {
		set x_in 0
		set x_inside 0
	} elseif { $x > $plot(image_width_pixels) } {
		set x_in $plot(image_width_pixels)
		set x_inside 0
	} else {
		set x_in $x
		set x_inside 1
	}

	if { $y < 0 } {
		set y_in 0
		set y_inside 0
	} elseif { $y > $plot(image_height_pixels) } {
		set y_in $plot(image_height_pixels)
		set y_inside 0
	} else {
		set y_in $y
		set y_inside 1
	}
	
	if { $x_inside && $y_inside } {
		set direction inside
	} elseif { $x < 0 } {
		set direction left
	} elseif { $x > $plot(image_width_pixels) } {
		set direction right
	} elseif { $y < 0 } {
		set direction up
	} else {
		set direction down 
	}

	return [list $x_in $y_in $direction]
}
