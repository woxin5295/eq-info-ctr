proc extract { orid } {
        global State 
 
	set db [dbptr_from_orid $orid]

        set source_db [dbgetv $db origin [lindex $db 3] source_db]
        set source_orid [dbgetv $db origin [lindex $db 3] source_orid]
	set source_time [dbgetv $db origin [lindex $db 3] time]

	set key [join [list "processing_systems{" \
			   $State(waveforms_from) \
			   "{archive_database}}" ] ""]
        pfget aeic_rtsys $key waveform_db
	set waveform_db [epoch2str $source_time $waveform_db]
 
        set Subset "dbsubset $source_db.origin 'orid == $source_orid' "
#        set Join "dbjoin - assoc arrival origerr event"
        set Join "dbjoin - assoc arrival event"
        set Unjoin "dbunjoin -f -o $State(dest_db) -"
 
        if {[catch {exec sh -c "$Subset | $Join | $Unjoin"} message]} {
                tk_dialog .dialog "Sorry..." \
                        "Failed to subset $source_db for orid $source_orid." \
                        error 0 Dismiss
                return
        } else {
                if {[file exists $source_db.lastid]} {
			exec sh -c "cp $source_db.lastid $State(dest_db).lastid"
		} else {
			exec touch $State(dest_db).lastid
		}
 
                set fd [open "$State(dest_db)" w]
                puts $fd "css3.0"
                regsub {[^/][-a-zA-Z0-9_]*$} $State(dest_db) {{&}} DD
                regsub {[^/][-a-zA-Z0-9_]*$} $waveform_db {{&}} dw
#                regsub {[^/][-a-zA-Z0-9_]*$} $source_db {{&}} dn
                regsub {[^/][-a-zA-Z0-9_]*$} $State(site_db) {{&}} ds
 
#                puts $fd "$DD:$dw:$dn:$ds"
                puts $fd "$DD:$dw:$ds"
                close $fd
 
                enable_response
        }
        return
 
}
