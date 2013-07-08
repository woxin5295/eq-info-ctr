proc constrain_geometry {} {
	global plot

	regexp {([0-9]+)x([0-9]+)} [wm geometry .] junk currentx currenty

	set newx [min $currentx $plot(max_width)]
	set newy [min $currenty $plot(max_height)]

	wm geometry . "$newx\x$newy"
}
