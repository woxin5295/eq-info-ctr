proc audible_alert {} {
	global State

	if { $State(alert_sound) == "beep" } {
		echo \007
	} elseif { $State(alert_sound) != "" } {
		catch "exec /usr/bin/audioplay $State(alert_sound) &"
	}
}
