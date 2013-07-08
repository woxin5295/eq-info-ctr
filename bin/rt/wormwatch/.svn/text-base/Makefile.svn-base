BIN= wormwatch
PF=wormwatch.pf

MAN1= wormwatch.1

TCL= \
	Quit.tcl \
	arrows.tcl \
	audible_alert.tcl \
	change_image.tcl \
	constrain_geometry.tcl \
	dbptr_from_orid.tcl \
	describe_quake.tcl \
	display_summary.tcl \
	epoch_to_yymmdd.tcl \
	extract.tcl \
	fill_quakelist.tcl \
	get_available_image_names.tcl \
	get_internal_database.tcl \
	highlight_latest_quake.tcl \
	init_globals.tcl \
	init_info_frame.tcl \
	init_orb.tcl \
	init_quakelist.tcl \
	init_windows.tcl \
	latlon2pix.tcl \
	latlonfocus.tcl \
	lookup_source_db.tcl \
	make_map.tcl \
	map_quake.tcl \
	new_image.tcl \
	new_quake.tcl \
	new_quake_fromdb.tcl \
	pixel_confine.tcl \
	quake_arrow.tcl \
	quake_oval.tcl \
	read_imagespecs.tcl \
	resize_wormwatch.tcl \
	respond.tcl \
	restack_quakes.tcl \
	revert_to_latest.tcl \
	set_default_geometry.tcl \
	set_latest_quake.tcl \
	set_maxsize.tcl \
	set_minsize.tcl \
	show_database.tcl \
	spotlight_event.tcl \
	startup.tcl \
	strip_quotes.tcl \
	usage.tcl \
	waveforms.tcl \
	wormwatch_dbcallback.tcl 
 
DATADIR= tcl/library/wormwatch
DATA= tclIndex $(TCL)

include $(AEICMAKE)

tclIndex : $(TCL)
	tclindex $(TCL) > $@
