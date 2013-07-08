proc respond { orid } {
        global State 
 
	set db [dbptr_from_orid $orid]

        set source_db [dbgetv $db origin [lindex $db 3] source_db]
        set source_orid [dbgetv $db origin [lindex $db 3] source_orid]
 
        if { $source_orid < 0} {
                tk_dialog .dialog "Sorry..." \
                        "No source database for this event" \
                        error 0 Dismiss
                return
        }
 
        if {[catch {toplevel .r} message]} {
		wm deiconify .r
		raise .r
                tk_dialog .dialog "Sorry..." \
                        "Only one Response screen allowed at a time" \
                        error 0 Dismiss
                return
        }
 
        set db [dbopen $source_db r]
        set db [dblookup $db 0 origin 0 0]
        set recno [dbfind $db -1 0 "orid == $source_orid"]
        set lat [dbgetv $db origin $recno lat]
        set lon [dbgetv $db origin $recno lon]
        set depth [dbgetv $db origin $recno depth]
        set time [dbgetv $db origin $recno time]
        set ml [dbgetv $db origin $recno ml]
        set ndef [dbgetv $db origin $recno ndef]
        dbclose $db
 
        wm title .r "Response for orid $source_orid"
        wm minsize .r 300 196
        wm geometry .r +50+50
        listbox .r.l 
        button .r.dismiss -text "Dismiss" -command {
                destroy .r
        }

	frame .r.wf
	label .r.wf.l -text "Waveforms from:"
	pack .r.wf.l -side left
	pfgetarr processing_systems %aeic_rtsys#processing_systems
	set systems [array names processing_systems]
	foreach system $systems {
		pfgetarr myarr %aeic_rtsys#processing_systems#$system
		radiobutton .r.wf.$system -text $myarr(system_name) \
			-value $system -variable State(waveforms_from)
		pack .r.wf.$system -side left
	}

        frame .r.db
        button .r.db.make -text "Subset database" \
                          -command "extract $orid"
        entry .r.db.where -textvariable State(dest_db) -relief raised \
			  -width [max 24 [clength $State(dest_db)]]
        button .r.dbe -text "view database" \
                -command "exec dbe $State(dest_db) &" -state disabled
        button .r.dbpick -text "view waveforms" \
                         -state disabled \
                         -command "waveforms $orid"
        button .r.loc -text "relocate" -state disabled -command {
                set dir [exec dirname $State(dest_db)]
                cd $dir
                if {[file exists tmp]} {
                        set i [tk_dialog .dialog "Housecleaning" \
                                "something called tmp exists in $dir--delete?" \
                                question 0 Delete "Leave it"]
                        if {$i == 0} {
                                exec /bin/rm -r tmp
                        }
                }
                set Xterm "xterm -geometry 80x24+0-0"
                exec sh -c "$Xterm -e dbloc2 $State(dest_db)" &
        }
	button .r.respond -text "issue release" \
			  -state disabled \
	-command {
		set Xterm "xterm -geometry 80x24+0-0"
		exec sh -c "$Xterm -e aeic_respond -g $State(dest_db)" &
	}

        pack .r.l -side top -fill x
	pack .r.wf -side top -fill x
        pack .r.db -side top -fill x
        pack .r.db.make -side left 
        pack .r.db.where -side left -expand 1 -fill x
        pack .r.dbe -side top -fill x
        pack .r.dbpick -side top -fill x
        pack .r.loc -side top -fill x
	pack .r.respond -side top -fill x
        pack .r.dismiss -side top -fill x
 
        .r.l insert end "Time [strtime $time] GMT"
        .r.l insert end "Lat $lat"
        .r.l insert end "Lon $lon"
        .r.l insert end "Depth $depth km"
        .r.l insert end "Ml $ml"
        .r.l insert end "$ndef Phases"
        return
}
 
proc enable_response {} {
        .r.dbe configure -state normal
        .r.dbpick configure -state normal
        .r.loc configure -state normal
	.r.respond configure -state normal
        return
}
