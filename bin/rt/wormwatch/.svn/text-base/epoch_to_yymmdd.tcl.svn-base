proc epoch_to_yymmdd { epoch } {
	regexp {([0-9]+)/([0-9]+)/[0-9][0-9]([0-9]+)} [strtime $epoch] \
				junk mo dy yr
	
	return [format "%02d%02d%02d" $yr $mo $dy]
}
