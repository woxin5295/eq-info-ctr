
proc LabelEntry {w args} { 
    frame $w
    label $w.l 
    entry $w.e 
    pack $w.l -side left 
    pack $w.e -side left -fill x
    while { ! [lempty $args] } {
	set option [lvarpop args]
	switch -- $option {
	    -label	{ set value [lvarpop args] ; $w.l config -text $value }
	    -textvariable	{ set value [lvarpop args] ; global $value; $w.e config -textvariable $value }
	    -width	{ set value [lvarpop args] ; $w.e config -width $value }
	}
    }
    return $w
}
