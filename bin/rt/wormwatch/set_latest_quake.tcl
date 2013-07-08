proc set_latest_quake {} {
        global Database State
 
        set Database(db) [dblookup $Database(db) 0 origin 0 0]
        set Database(db) [dbsort $Database(db) time]
        set nrecs [dbquery $Database(db) dbRECORD_COUNT]
        set last [expr $nrecs - 1]
        set Database(db) [lreplace $Database(db) 3 3 $last]
 
        set orid [dbgetv $Database(db) origin [lindex $Database(db) 3] orid]
 
        set State(latest_orid) $orid
}
