proc resize_wormwatch {} {
	global plot

	#Adjust for width of scrollbars
	set sw 30

	set plot(canvas_width) [expr [winfo width .] - \
				     $plot(nonmap_width) - $sw]
	set plot(canvas_height) [expr [winfo height .] - \
				     $plot(nonmap_height) - $sw]
}
