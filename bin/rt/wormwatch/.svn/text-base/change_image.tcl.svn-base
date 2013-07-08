proc change_image { new_image } {

	global Database plot

	read_imagespecs $new_image

	if { $plot(save_current_image) } {

		keylset plot(saved_images) \
			$plot(old_image) $plot(image_handle)

	} else {

		image delete $plot(image_handle)

	}

	$plot(mapcanvas) delete $plot(imagetag)

	if { ! [keylget plot(saved_images) $new_image myimage] } {
        	set myimage [image create photo \
                        -file $plot(image_file) \
                        -palette $plot(palette)]
	}
 
        set plot(image_handle) $myimage
        set plot(imagetag) [$plot(mapcanvas) create image 0 0 \
					-image $myimage -anchor nw]

	$plot(mapcanvas) lower $plot(imagetag)

	$plot(mapcanvas) bind $plot(imagetag) <2> \
					"plotDown $plot(mapcanvas) %x %y"
	$plot(mapcanvas) bind $plot(imagetag) <B2-Motion> \
					"plotMove $plot(mapcanvas) %x %y"


	$plot(mapcanvas) configure -scrollregion \
		"0 0 $plot(image_width_pixels) $plot(image_height_pixels)"

	$plot(mapcanvas) delete highlight_circle

	set_maxsize

	set db [get_internal_database]
	set db [dblookup $db 0 origin 0 dbALL] 
	set nrecs [dbquery $db dbRECORD_COUNT]
	for { set i 0 } { $i < $nrecs } { incr i } {

		set tag [dbgetv $db origin $i quake_tag]
		set label_tag [dbgetv $db origin $i label_tag]
		set orid [dbgetv $db origin $i orid]

		$plot(mapcanvas) delete $tag
		$plot(mapcanvas) delete $label_tag

		map_quake $orid
	}

	if { $nrecs > 0 } { highlight_latest_quake }
}
