BIN= fktools
MAN1= fktools.1

TCL= \
	array_for_arid.tcl \
	fkarid.tcl \
	fkscan.tcl \
	get_array_codes.tcl \
	LabelEntry.tcl

DATADIR= tcl/library/dbap_tools
DATA= tclIndex $(TCL)

include $(AEICMAKE)

tclIndex : $(TCL)
	tclindex $(TCL) > $@
