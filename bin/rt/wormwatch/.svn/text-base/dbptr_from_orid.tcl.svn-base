proc dbptr_from_orid { orid } {
        global Database
 
        set db $Database(db)
        set db [dblookup $db 0 origin 0 0]
        set recno [dbfind $db -1 0 "orid == $orid"]
        set db [lreplace $db 3 3 $recno]
 
        return $db
}
