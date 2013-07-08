proc show_database {} {
        global plot State
 
        set db [dbopen $State(hypo_source) r]
        set db [dblookup $db 0 origin 0 0]
        set nrecs [dbquery $db dbRECORD_COUNT]
 
       for { set i 0 } { $i < $nrecs } { incr i } {
                set db [lreplace $db 3 3 $i]
 
                set orid [new_quake_fromdb $db]
                map_quake $orid
                describe_quake $orid yellow
       }
}
