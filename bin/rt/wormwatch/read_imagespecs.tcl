proc read_imagespecs { image_name } {
	global plot pfname
	
	pfgetarr myimage %$pfname#$image_name

        set plot(image_file) $myimage(image_file)
        set plot(proj_center_lat) $myimage(proj_center_lat)
        set plot(proj_center_lon) $myimage(proj_center_lon)
        set plot(pix_per_deg) $myimage(pix_per_deg)
        set plot(image_width_pixels) $myimage(image_width_pixels)
        set plot(image_height_pixels) $myimage(image_height_pixels)
        set plot(proj_center_x) $myimage(proj_center_x)
        set plot(proj_center_y) $myimage(proj_center_y)
}
