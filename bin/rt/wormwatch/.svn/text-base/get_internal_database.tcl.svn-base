proc get_internal_database {} {
	global Database 

	if { ! [info exists Database(db)] } {
		set Database(name) "/tmp/wormwatch_[pid]"
		set Database(db) [dbopen $Database(name) "r+"]
		set Database(db) [dblookup $Database(db) 0 origin 0 dbALL]

		add_attribute $Database(db) Integer source_orid 8
		add_attribute $Database(db) String source_db 100
		add_attribute $Database(db) String quake_tag 20 
		add_attribute $Database(db) String label_tag 25
		add_attribute $Database(db) Integer listbox_index 8
		set new_fields \
		   "source_orid source_db quake_tag label_tag listbox_index"

		set fields [dbquery $Database(db) dbTABLE_FIELDS]
		set fields "Fields( $new_fields $fields )"
		set primary "Primary( [dbquery $Database(db) dbPRIMARY_KEY] )"
		set alternate "Alternate( [dbquery $Database(db) dbALTERNATE_KEY] )"
		set foreign "Foreign( [dbquery $Database(db) dbFOREIGN_KEYS] )"
		
		set new_relation \
			"Relation origin $fields $primary $alternate $foreign;"
		
		dbcompile $Database(db) $new_relation
	}
	
	return $Database(db)
}

proc add_attribute { db type name size } {

	switch $type {
	Integer {
		set mytype "Integer( $size )" 
		set format "Format( \"%$size\d\" )"
		set null "Null( \"-1\" )"
	}
	String {
		set mytype "String( $size )"
		set format "Format( \"%$size\s\" )"
		set null "Null( \"-\" )"
	}
	default {
		echo type $type no supported in add_attribute
	}
	}
	
	set new_attribute "Attribute $name $mytype $format $null;"

	dbcompile $db $new_attribute
}
