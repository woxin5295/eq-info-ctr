
proc down_arrow { w x y hgt color tag } {
	# half-width
	set hw 3 
	set aw 7
	set xphw [expr $x + $hw]
	set xmhw [expr $x - $hw]
	set xmlim [expr $xmhw - $aw]
	set xplim [expr $xphw + $aw]

	set ymh [expr $y - $hgt]
	set yma [expr $y - 2 * $aw]
	set ymaa [expr $y - 3 * $aw]

	set tag [$w create polygon $xmhw $ymh $xphw $ymh $xphw $yma $xplim $ymaa $xplim $yma $x $y $xmlim $yma $xmlim $ymaa $xmhw $yma $xmhw $ymh -fill $color -tags $tag]
}

proc left_arrow { w x y hgt color tag } {
	# half-width
	set hw 3 
	set aw 7
	set yphw [expr $y + $hw]
	set ymhw [expr $y - $hw]
	set ymlim [expr $ymhw - $aw]
	set yplim [expr $yphw + $aw]

	set xph [expr $x + $hgt]
	set xpa [expr $x + 2 * $aw]
	set xpaa [expr $x + 3 * $aw]

	set tag [$w create polygon $xph $ymhw $xph $yphw $xpa $yphw $xpaa $yplim $xpa $yplim $x $y $xpa $ymlim $xpaa $ymlim $xpa $ymhw $xph $ymhw  -fill $color -tags $tag]
}

proc right_arrow { w x y hgt color tag } {
	# half-width
	set hw 3 
	set aw 7
	set yphw [expr $y + $hw]
	set ymhw [expr $y - $hw]
	set ymlim [expr $ymhw - $aw]
	set yplim [expr $yphw + $aw]

	set xmh [expr $x - $hgt]
	set xma [expr $x - 2 * $aw]
	set xmaa [expr $x - 3 * $aw]

	set tag [$w create polygon $xmh $ymhw $xmh $yphw $xma $yphw $xmaa $yplim $xma $yplim $x $y $xma $ymlim $xmaa $ymlim $xma $ymhw $xmh $ymhw  -fill $color -tags $tag]
}

proc up_arrow { w x y hgt color tag } {
	# half-width
	set hw 3 
	set aw 7
	set xphw [expr $x + $hw]
	set xmhw [expr $x - $hw]
	set xmlim [expr $xmhw - $aw]
	set xplim [expr $xphw + $aw]

	set yph [expr $y + $hgt]
	set ypa [expr $y + 2 * $aw]
	set ypaa [expr $y + 3 * $aw]

	set tag [$w create polygon $xmhw $yph $xphw $yph $xphw $ypa $xplim $ypaa $xplim $ypa $x $y $xmlim $ypa $xmlim $ypaa $xmhw $ypa $xmhw $yph -fill $color -tags $tag]
}
