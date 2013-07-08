proc waveforms { orid } {
        global State
 
	set db [dbptr_from_orid $orid]

        set source_orid [dbgetv $db origin [lindex $db 3] source_orid]
 
        set Xterm "xterm -geometry 80x24+0-0"
        set Dbpick "dbpick -nostarttalk -geom 966x503 -appname dbpick"
        set Smartpick "smartpick -nowait -appname dbpick"
 
        exec sh -c "$Xterm -e $Smartpick $State(dest_db) " &
 
        sleep 8
        send -async dbpick "sw off" 
        send -async dbpick "se $source_orid"
        send -async dbpick "tse"
        send -async dbpick "rec"
        send -async dbpick "cw 1 40"
        send -async dbpick "sw on"
        return
}
