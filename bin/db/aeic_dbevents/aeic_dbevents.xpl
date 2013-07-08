#!/bin/sh
# This comment extends to the next line for tcl \
# exec awish $0 -name aeic_dbevents$$ -- $*

#package require Datascope
#package require Pixmap
source $env(ANTELOPE)/data/tcl/rtm/tailfile.tcl

#   Copyright (c) 1997 Boulder Real Time Technologies, Inc.           
#                                                                     
#   This software module is wholly owned by Boulder Real Time         
#   Technologies, Inc. Any use of this software module without        
#   express written permission from Boulder Real Time Technologies,   
#   Inc. is prohibited.                                               

proc get_xhosts {} {
    set XHOST [open |xhost r]
    set xhosts {}
    while { [gets $XHOST line ] >= 0 } { 
	if { [regexp {access control disabled} $line] } { 
	    lappend xhosts ALL_HOSTS
	} elseif { ! [regexp {access control enabled} $line] } { 
	    regsub INET: $line {} line
	    lappend xhosts $line
	}
    }
    close $XHOST ;
    return $xhosts
}

proc turn_off_xhosts {xhosts} { 
    foreach host $xhosts { 
	regsub .*: $host {} host
	exec xhost -$host
    }
    set xhosts [get_xhosts]
    if { [llength $xhosts] > 0 } { 
	puts "\nfailed to eliminate the following hosts: $xhosts"
	puts "\tPlease disable xhost access for these by hand"
	set result 0
    } else {
	set result 1
    }
    return $result
}

proc xhosts_ok {msg} { 
    set result 0
    set xhosts [get_xhosts]
    if { [llength $xhosts] > 0 } {
	set answer [ tk_messageBox \
			-default cancel \
			-icon question \
			-message "$msg\n\nDisable xhost access for $xhosts?" \
			-title {Disable xhosts?} \
			-type okcancel ]

	switch -- $answer {
	    ok      { set result [turn_off_xhosts $xhosts] }
	}
    } else { 
	set result 1 
    }
    return $result
}

proc isthisme {} {
	global thisisme

	set thisisme 1
}

proc getappname {} {
	global thisisme

# added from 4.6 version JCSa
        tk appname "dbevents_[pid]"
# JCSb
	set thisisme 0
	set interps [winfo interps]
	foreach interp $interps {
		catch "send -async {$interp} isthisme"
		if {$thisisme == 1} {
			return $interp
		}
	}
	return -1
}

proc check_tc24 {} {
	set l [winfo visualsavailable .]
	foreach v $l {
		if {"$v" == "truecolor 24"} {return yes}
	}
	return no
}

proc usage {} {
	#NIKO
	#puts stderr "Usage: dbevents dbname"
    puts stderr "Usage: dbevents \[-maxage days \] \[-pf pffile\] dbname"
}

#NIKO
#if {$argc != 1} {
if {$argc < 1 || $argc >5} {
	usage
	exit
}

set tcl_precision 17
set update_ms	60000
set id -1
set stat	Startup
set lastupdate	-1
set lastevent	-1
set maxmapwidth	800
set maxmapheight	800
set dbpickgeom	600x800+500+0
set dbpicktypeingeom	100x20+518+601
set mtime_event	0
set mtime_origin 0
set wfdialogwin .wfd
set scsift *:BHZ
set channels 33
set twin 180
set filter 0
set automap	1
set doupdate	1
set textwidth	50
set legendon 1
set legendwidth	300
set legendheight 150
set last_evid -1
set myappname	[getappname]
set geometry	+0+0
set waittorestart	10000
set show_origins -1
#JCSa
set alerton 1
set repeaton 1
set qddson 1
#JCSb

if {$myappname == "-1"} {
	puts stderr "dbevents: Cannot get appname."
	exit
}
if {$tcl_version < 8.0} {
	set list_font fixed
} else {
	set list_font [list fixed 8]
}

set M_PI	3.14159265358979323846
set SQRT2	1.414213562
set dbpickon	0

set nstar 11
set xstar(0) 0.0 
set xstar(1) 0.88199
set xstar(2) 0.47553 
set xstar(3) 1.42659
set xstar(4) 0.29390 
set xstar(5) 0.0 
set xstar(6) -0.29390 
set xstar(7) -1.42659
set xstar(8) -0.47553 
set xstar(9) -0.88199 
set xstar(10) 0.0
set ystar(0) 0.5 
set ystar(1) 1.21353
set ystar(2) 0.15451 
set ystar(3) -0.46353 
set ystar(4) -0.40451 
set ystar(5) -1.5 
set ystar(6) -0.40451 
set ystar(7) -0.46353 
set ystar(8) 0.15451 
set ystar(9) 1.21353
set ystar(10) 0.5

set ntriangle 3
set xtriangle(0) 0.0 
set xtriangle(1) 0.866
set xtriangle(2) -0.866
set ytriangle(0) -1.0
set ytriangle(1) 0.5
set ytriangle(2) 0.5

tk_setPalette \#d9d9ee

proc mytk_popup {menu x y {entry {}}} {
    variable ::tk::Priv
    global tcl_platform
    if {($Priv(popup) != "") || ($Priv(postedMb) != "")} {
        tkMenuUnpost {}
    }
    tk::PostOverPoint $menu $x $y $entry
    if {$tcl_platform(platform) == "unix"} {
        tk::SaveGrabInfo $menu
        grab -global $menu
        set Priv(popup) $menu
    }
}

proc clock_seconds {} {
	global tcl_version

	if {$tcl_version < 8.0} {
		return [getclock]
	} else {
 		return [clock seconds]
	}
}

proc clock_format {time} {
	global tcl_version

	if {$tcl_version < 8.0} {
		return [fmtclock $time "%T" GMT]
	} else {
 		return [clock format $time -format "%T" -gmt 1]
	}
}

proc mercy {lat} {
	global M_PI

	if {$lat >= 90.0} {return 1.e30}
	if {$lat <= -90.0} {return -1.e30}
	set y [expr 180.0*log(tan((0.5*$M_PI*$lat/180.0)+0.25*$M_PI))/$M_PI]
	return $y
}

proc merclat {y} {
	global M_PI

	set lat [expr 180.0*(2.0 * (atan(exp(($y)*$M_PI/180.0) - 0.25*$M_PI)))/$M_PI]
	return $lat
}

proc dist {latc lonc lat lon} {
	global M_PI

	set lat1 [expr $latc*$M_PI/180.0]
	set lat2 [expr $lat*$M_PI/180.0]
	set lon1 [expr $lonc*$M_PI/180.0]
	set lon2 [expr $lon*$M_PI/180.0]
	set slat1 [expr sin($lat1)]
	set clat1 [expr cos($lat1)]
	set slat2 [expr sin($lat2)]
	set clat2 [expr cos($lat2)]
	set x2 [expr $clat2*cos($lon2-$lon1)]
	set y2 [expr $clat2*sin($lon2-$lon1)]
	set x [expr $x2-$clat1]
	set z [expr $slat2-$slat1]
	set xpp [expr $x*$clat1+$z*$slat1]
	set ypp $y2
	set zpp [expr -$x*$slat1+$z*$clat1]
	set s [expr sqrt($xpp*$xpp+$ypp*$ypp+$zpp*$zpp)]
	set del [expr 2.0*asin(0.5*$s)*180.0/$M_PI]
	if {$ypp == 0.0 && $zpp == 0.0} {
		set az 0.0
	} else {
		set az [expr atan2($ypp,$zpp)*180.0/$M_PI]
	}
	if {$az < 0.0} {set az [expr $az + 360.0]}
	return [list $del $az]
}

proc edpxy {latc lonc lat lon} {
	global M_PI

	if {$latc == $lat && $lonc == $lon} {return [list 0.0 0.0]}
	set delax [dist $latc $lonc $lat $lon]
	set del [lindex $delax 0]
	set az [expr $M_PI*[lindex $delax 1]/180.0]
	return [list [expr $del*sin($az)] [expr $del*cos($az)]]
}

proc set_mxscroll {w} {
        global mapon
        global mapxscroll
        global width
 
        set wtot $width($mapon)
        set first [lindex [$mapxscroll($mapon) get] 0]
        set last [expr $first+double($w)/double($wtot)]
        if {$last > 1.0} {
                set first [expr 1.0-($last-$first)]
                set last 1.0
        }
        $mapxscroll($mapon) set $first $last
}

proc set_myscroll {h} {
        global mapon
        global mapyscroll
        global height
 
        set htot $height($mapon)
        set first [lindex [$mapyscroll($mapon) get] 0]
        set last [expr $first+double($h)/double($htot)]
        if {$last > 1.0} {
                set first [expr 1.0-($last-$first)]
                set last 1.0
        }
        $mapyscroll($mapon) set $first $last
}

proc set_mxview args {
        global mapon
        global mapxscroll
        global mapcanvas
        global width
        global hmapframe
 
        set wtot $width($mapon)
	set i 0
        set arg [lindex $args $i]
        switch $arg {
        moveto {
                set l [$mapxscroll($mapon) get]
		set i [expr $i+1]
        	set first [lindex $args $i]
                if {$first < 0.0} {set first 0.0}
                set delta [expr $first-[lindex $l 0]]
                set last [expr [lindex $l 1]+$delta]
                if {$last > 1.0} {
                        set delta [expr 1.0 - [lindex $l 1]]
                        set first [expr [lindex $l 0] +$delta]
                        set last 1.0
                }
                set x [expr -int($first*$wtot+0.1)]
		$mapcanvas($mapon) xview moveto $first
                $mapxscroll($mapon) set $first $last
                set x [$mapcanvas($mapon) canvasx 5]
		set y [expr $hmapframe($mapon)-5]
                set y [$mapcanvas($mapon) canvasy $y]
		$mapcanvas($mapon) coords copyright $x $y
                }
        default
                {}
        }
}

proc set_mxview_map args {
        global mapxscroll
        global mapcanvas
        global width
        global hmapframe
 
	set i 0
        set mapon [lindex $args $i]
        set wtot $width($mapon)
	incr i
       	set arg [lindex $args $i]
        switch $arg {
        moveto {
                set l [$mapxscroll($mapon) get]
		incr i
        	set first [lindex $args $i]
                if {$first < 0.0} {set first 0.0}
                set delta [expr $first-[lindex $l 0]]
                set last [expr [lindex $l 1]+$delta]
                if {$last > 1.0} {
                        set delta [expr 1.0 - [lindex $l 1]]
                        set first [expr [lindex $l 0] +$delta]
                        set last 1.0
                }
                set x [expr -int($first*$wtot+0.1)]
		$mapcanvas($mapon) xview moveto $first
                $mapxscroll($mapon) set $first $last
                set x [$mapcanvas($mapon) canvasx 5]
		set y [expr $hmapframe($mapon)-5]
                set y [$mapcanvas($mapon) canvasy $y]
		$mapcanvas($mapon) coords copyright $x $y
                }
        default
                {}
        }
}

proc set_myview args {
        global mapon
        global mapyscroll
        global mapcanvas
        global height
        global hmapframe
 
        set htot $height($mapon)
	set i 0
        set arg [lindex $args $i]
        switch $arg {
        moveto {
                set l [$mapyscroll($mapon) get]
		set i [expr $i+1]
        	set first [lindex $args $i]
                if {$first < 0.0} {set first 0.0}
                set delta [expr $first-[lindex $l 0]]
                set last [expr [lindex $l 1]+$delta]
                if {$last > 1.0} {
                        set delta [expr 1.0 - [lindex $l 1]]
                        set first [expr [lindex $l 0] +$delta]
                        set last 1.0
                }
                set x [expr -int($first*$htot+0.1)]
		$mapcanvas($mapon) yview moveto $first
                $mapyscroll($mapon) set $first $last
                set x [$mapcanvas($mapon) canvasx 5]
		set y [expr $hmapframe($mapon)-5]
                set y [$mapcanvas($mapon) canvasy $y]
		$mapcanvas($mapon) coords copyright $x $y
                }
        default
                {}
        }
}

proc set_myview_map args {
        global mapyscroll
        global mapcanvas
        global height
        global hmapframe
 
	set i 0
        set mapon [lindex $args $i]
        set htot $height($mapon)
	incr i
       	set arg [lindex $args $i]
        switch $arg {
        moveto {
                set l [$mapyscroll($mapon) get]
		incr i
        	set first [lindex $args $i]
                if {$first < 0.0} {set first 0.0}
                set delta [expr $first-[lindex $l 0]]
                set last [expr [lindex $l 1]+$delta]
                if {$last > 1.0} {
                        set delta [expr 1.0 - [lindex $l 1]]
                        set first [expr [lindex $l 0] +$delta]
                        set last 1.0
                }
                set x [expr -int($first*$htot+0.1)]
		$mapcanvas($mapon) yview moveto $first
                $mapyscroll($mapon) set $first $last
                set x [$mapcanvas($mapon) canvasx 5]
		set y [expr $hmapframe($mapon)-5]
                set y [$mapcanvas($mapon) canvasy $y]
		$mapcanvas($mapon) coords copyright $x $y
                }
        default
                {}
        }
}

proc set_update {update_interval} {
	global db
	global id
	global update_ms
	global mtime_event
#JCSa change from 4.6 version
#	after cancel $id
#JCSb
	after cancel 4.6
	if {$update_interval == "now"} {
		set mtime_event -1
		plot_events $db 1 1
		return
	}
	set update_ms [expr [convert_time $update_interval]*1000]
	plot_events $db 1 1
}

proc update_latency {} {
	global lastupdate
	global lastevent
	global stat
	global doupdate

	if {$lastupdate < 0 || $doupdate == 0} {
		after 1000 update_latency
		return
	}
	set latency [expr [clock_seconds]-$lastupdate]
	if {$lastevent < 0} {
		set stat [format "Time: %s Gmt, %s seconds since last update, no events" \
			[mystrtime [clock seconds]] [clock_format $latency]]
		seticontime ""
	} else {
		set latency2 [expr int([clock_seconds]-$lastevent+0.5)]
		set stat [format "Time: %s Gmt, %s since last update, %s since last event" \
			[mystrtime [clock seconds]] [clock_format $latency] [clock_format $latency2]]
		seticontime $latency2
	}
	after 1000 update_latency
}

proc seticontime {time} {
	global lasticontime

	if {[info exists lasticontime] == 0} {
		set lasticontime foo
	}
	if {$time == $lasticontime} return
	set lasticontime $time
	if {$time == ""} {
		.icon.c itemconfigure icontime -text ""
		return
	} else {
		.icon.c itemconfigure icontime -text [clock_format $time]
	}
	set color \#ff0000
	if {$time >= 3600} {set color \#004000}
	if {$time >= 21600} {set color \#000040}
	if {$time >= 86400} {set color \#404040}
	.icon.c itemconfigure icontime -fill $color
}

proc mystrtime {epoch} {
	set str [strtime $epoch]
	set yd [yearday $epoch]
	set out [format "%s(%s) %s" [string range $yd 2 7] \
		[string range $str 0 4] [string range $str 12 19]]
	return "$out"
}

proc convert_time {time} {
	set l [string length $time]
	set lend [expr $l-1]
	set value [string range $time 0 [expr $l-2]]
	set units [string range $time $lend $lend]
	switch $units {
	s 	-
	S	{
		set out $value
		}
	m 	-
	M	{
		set out [expr $value*60]
		}
	h 	-
	H	{
		set out [expr $value*3600]
		}
	d 	-
	D	{
		set out [expr $value*24*3600]
		}
	default {
		set out [string range $time 0 end]
		}
	}
	return $out
}

proc quit_proc {} {
	send2dbpick quit
	exit
}

proc delete_event {} {
	global delete_evid
	global dbname

	set ans [tk_dialog .areyousure "dbevents: confirm" "Are You Sure?" "" 0 No Yes]
	if {$ans != 1} return
	catch "exec dbsubset $dbname.event evid==$delete_evid | dbjoin - origin assoc arrival | dbdelete -m -"
}
#JCSa
proc delete_event_qdds {} {
	global delete_evid
	global evtime
	global dbname
	global qddson
	global Pf

	set qdds_mail_list [pfget $Pf qdds_mail_list]
        # set correct date for script ex 2003_11_03     
	# stupid hack to make things work
	# sometimes dyy ends up as 1 character sometimes as 2
	# this drives me nuts.
	set qdat [split [strdate $evtime($delete_evid)] "/"]
	set mo [format "%2.2d" [lindex $qdat 0]]
	#set dyy [lindex $qdat 1]
	set dy2 [format "%s" [lindex $qdat 1]]
	set yy [format "%d" [lindex $qdat 2]]
	#set bs [string range $dyy 0 0]
	#if {$bs == 0} { 
	#   set dy [string range $dyy 1 1]
	#} else {
	#   set dy [string range $dyy 0 1]
	#}
#        set qdate [format "%d_%2.2d_%2.2d" [string range $qdat 6 9] [string range $qdat 0 1] $dy]
#        set qdate [format "%d_%2.2d_%2.2d" $yy $mo $dy]
        set qdate [format "%s_%2s_%s" $yy $mo $dy2]
	#puts "$evtime($delete_evid) [strdate $evtime($delete_evid)]" 
	#puts "$mo $dyy $yy $bs $dy $dy2"

#	toplevel .commandwin
#	label .commandwin.msg -text "Send query to QDDS?"
#	label .commandwin.label -text "Command to be executed:"
#	set cmd [format "/usr/local/bin/QDDS/delete_qdds_eventid %s /home/qdds/polldirdiff/aevent.%s /home/qdds/run/polldir" $qdate $delete_evid]
#	entry .commandwin.entry -width 100 -relief sunken -bd 2 -textvariable cmd
#	button .commandwin.apply -text Execute -command {catch "exec $cmd" delete_errmsg}
#	button .commandwin.dismiss -text Dismiss -command {destroy .commandwin}
	puts stderr " Command to be executed:"
#	puts stderr "/usr/local/bin/QDDS/delete_qdds_eventid $qdate /home/qdds/polldirdiff/aevent.$delete_evid /home/qdds/run/polldir"
	puts stderr "/usr/local/bin/QDDS/DELETE_QDDS_EVENTID $qdate $delete_evid"
	set ans [tk_dialog .areyousure "dbevents: confirm" "Send query to QDDS?" "" 0 No Yes]
	if {$ans != 1} {
		puts stderr "Event not deleted"
		return
	}
#	if {[file exists /home/qdds/polldirdiff/aevent.$delete_evid] == 0} {
#		set errmsg [format "/home/qdds/polldirdiff/aevent.%s does not exist:\n" $delete_evid]
#		append errmsg "There are 2 possibilities:\n"
#		append errmsg " 1) Event was not sent to ANSS web page\n"
#		append errmsg "   - see http://earthquake.usgs.gov/recenteqsUS/\n"
#		append errmsg " 2) QDDS file has not been built\n"
#		append errmsg "   - Please run /usr/local/bin/QDDS/createqddsdb"
#		return
#	}
#	if {[file exists /home/qdds/polldirdiff/AEVENT.$delete_evid] == 1} {
#		set errmsg [format "/home/qdds/polldirdiff/AEVENT.%s already exists\n" $delete_evid]
#		append errmsg "This event has already been deleted."
#		return
#	}
	if {$qddson == 1} {
	# catch "exec /usr/local/bin/QDDS/delete_qdds_eventid $qdate /home/qdds/polldirdiff/aevent.$delete_evid /home/qdds/run/polldir"
	catch "exec /usr/local/bin/QDDS/DELETE_QDDS_EVENTID $qdate $delete_evid"
		if {[file exists /home/qdds/polldirdelete/delete.$delete_evid] == 1} {
			set errmsg [format "Deleted evid %s from QDDS submission\n" $delete_evid]
			catch "send_qdds_email $qdds_mail_list $delete_evid" email_errmsg
			append errmsg $email_errmsg
		} else {
			set errmsg [format "AEVENT.%s does not exist, perhaps createqddsdb is still running:" $delete_evid]
			append errmsg "\n Event not deleted"
		}
	} else {
		set errmsg "Permission to delete from QDDS not allowed"
		append errmsg "\nSee Control pulldown menu to allow permission"
	}
	puts stderr "$errmsg"
}
proc send_qdds_email {email del_evid } {
	global evtime
        global evlat
        global evlon
        global evdepth
        global evauth
        global evreview
        global evnass
        global evndef
        global evmag
        global evmagtype
        global evorid
	
	set subject "\"QDDS event $del_evid\""
	set cmd "rtmail -s $subject $email"
	set mail [open |$cmd w+]
	puts $mail "
	*******  EVENT $del_evid DELETED FROM QDDS ********
	   [strtime $evtime($del_evid)]
	   lat: $evlat($del_evid)  lon: $evlon($del_evid)  depth: $evdepth($del_evid)
	   $evmag($del_evid) $evmagtype($del_evid) nass: $evnass($del_evid)

	"
	close $mail
	set errmsg "sent email to $email"
	return $errmsg
}

proc display_webmap {} {
	global Pf
	global delete_evid
	global dbname
	global currentselection
	global evlistelements
	global evtime
	global evmag
	global evmagtype

	set evtag [lindex $evlistelements $currentselection]
	set evid [string trimleft [string range $evtag 2 end] 0]
	set webmapsdb [pfget $Pf webmapsdb]
	set xvgeom [pfget $Pf xvgeometry]
	set db [dbopen $webmapsdb r]
	set dbmap [dbsubset [dblookup $db 0 webmaps 0 0] "evid==$evid"]
	set nrec [dbquery $dbmap dbRECORD_COUNT]
	if {[winfo exists .imagewin]} {
	  foreach wintype {imagedisplay dismiss time mag} {
		if {[winfo exists .imagewin.$wintype]} {
			destroy .imagewin.$wintype
		}
	  }
	} else {
	  toplevel .imagewin
	  wm geom .imagewin $xvgeom
	}
	if {$nrec == 1} {
	  set idir [dbgetv $dbmap 0 0 dir]
	  set idfile [dbgetv $dbmap 0 0 dfile]
	   if {[file exists $idir/$idfile] == 0} {
		label .imagewin.imagedisplay -text "$idir/$idfile does not exist"
	   } else { 
		set myimage [image create photo -file $idir/$idfile]
	        label .imagewin.imagedisplay -image $myimage 
	   }
	} else {
	  set msg [format "%d records\n for event:%d\n in %s:\n cannot display map"\
		  $nrec $evid $webmapsdb]
	  label .imagewin.imagedisplay -text "$msg"
	}
	wm title .imagewin "Event $evid"
	button .imagewin.dismiss -text Dismiss -command "destroy .imagewin"
	label .imagewin.time -text "[mystrtime $evtime($evid)]"
	label .imagewin.mag -text "Mag: $evmag($evid) $evmagtype($evid)"
	pack .imagewin.imagedisplay
	pack .imagewin.time .imagewin.mag .imagewin.dismiss -side left -padx 2 -expand 1
	#exec xv -ge $xvgeom -fi $idir/$idfile &
	#puts "Press q in image window to dismiss"
	dbfree $db
}

proc nearest_places {evid} {
	global Pf
	global evlistelements
	global currentselection
	global evlat
	global evlon
	global near_place_list

	set olat $evlat($evid)
	set olon $evlon($evid)
	set placedb [pfget $Pf placedb]
	set neardist [pfget $Pf neardist]
	set alwaysinc [pfget $Pf always_include]
	set nmax [pfget $Pf nplacemax]
	set ex [format "distance(lat,lon,%f,%f)*111.195 <= %d || place=~/%s/" $olat $olon $neardist $alwaysinc]
	set db [dbopen $placedb r]
	set dbp [dbsubset [dblookup $db 0 places 0 0] "$ex"]
	set ex1 [format "distance\(lat,lon,%f,%f\)*111.195" $olat $olon]
	set ex2 [format "azimuth\(lat,lon,%f,%f\)" $olat $olon]
	set dbp [dbsort $dbp $ex1]
	set nplaces [dbquery $dbp dbRECORD_COUNT]
	if {$nplaces < $nmax} {set nmax $nplaces}
	set near_place_str ""
	for {set i 0} {$i < $nmax} {incr i} {
		set place [dbgetv $dbp 0 $i place]
		set dbp [lreplace $dbp 3 3 $i]
		set dist_km [dbeval $dbp $ex1]
		set dist_mi [expr $dist_km/1.6]
		set azim [dbeval $dbp $ex2]
		set comp [compass_from_az $azim]
		set place_str [format "       %.0f km (%.0f mi) %s of %s\n" $dist_km $dist_mi $comp $place]
		append near_place_str $place_str
	}
	return $near_place_str
	dbfree $db
}

proc compass_from_az {azimuth} {
	while {$azimuth < 0.} {set azimuth [expr $az+360.]}
	while {$azimuth > 360.} {set azimuth [expr $az-360.]}
        if {$azimuth >= 348.75 || $azimuth < 11.25} {
                return "N";             # 0.00
        } elseif {$azimuth >= 11.25 && $azimuth < 33.75} {
                return "NNE";           # 22.50
        } elseif {$azimuth >= 33.75 && $azimuth < 56.25} {
                return "NE";            # 45.00 
        } elseif {$azimuth >= 56.25 && $azimuth < 78.75} {
                return "ENE";           # 67.50 
        } elseif {$azimuth >= 78.75 && $azimuth < 101.25} {
                return "E";             # 90.00 
        } elseif {$azimuth >= 101.25 && $azimuth < 123.75} {
                return "ESE";           # 112.50        
        } elseif {$azimuth >= 123.75 && $azimuth < 146.25} {
                return "SE";            # 135.00        
        } elseif {$azimuth >= 146.25 && $azimuth < 168.75} {
                return "SSE";           # 157.50        
        } elseif {$azimuth >= 168.75 && $azimuth < 191.25} {
                return "S";             # 180.00        
        } elseif {$azimuth >= 191.25 && $azimuth < 213.75} {
                return "SSW";           # 202.50        
        } elseif {$azimuth >= 213.75 && $azimuth < 236.25} {
                return "SW";            # 225.00        
        } elseif {$azimuth >= 236.25 && $azimuth < 258.75} {
                return "WSW";           # 247.50        
        } elseif {$azimuth >= 258.75 && $azimuth < 281.25} {
                return "W";             # 270.00        
        } elseif {$azimuth >= 281.25 && $azimuth < 303.75} {
                return "WNW";           # 292.50        
        } elseif {$azimuth >= 303.75 && $azimuth < 326.25} {
                return "NW";            # 315.00        
        } elseif {$azimuth >= 326.25 && $azimuth < 348.75} {
                return "NNW";           # 337.50        
        } else {
                return ""; # Faulty logic if we hit this
        }
}

#  JCSb
proc make_event {evid} {
	global db
	global seorid
	global sestas
	global sesta
	global seolat
	global seolon
	global seslat
	global seslon
	#JCSa
	global sesel
	global sesnam
	#JCSb
	global seevid
	global seprefor
	global seauth
	global sereview
	global senass
	global sendef
	global seotime
	global seodepth
	global semag
	global semagtype
	global seext
	global serev
	global seregion
	global selist
	global orlistelements
	global currentorselection
	global id
	global external_catalog_authors
	global seselected
	global sestachans
	global setimedef
	global stat
	global prefor

	set stat "Making event..."
	update
	if {[info exists seorid] != 0} {unset seorid}
	if {[info exists sestas] != 0} {unset sestas}
	if {[info exists sesta] != 0} {unset sesta}
	if {[info exists seolat] != 0} {unset seolat}
	if {[info exists seolon] != 0} {unset seolon}
	if {[info exists seslat] != 0} {unset seslat}
	if {[info exists seslon] != 0} {unset seslon}
	#JCS
	if {[info exists sesel] != 0} {unset sesel}
	if {[info exists sesnam] != 0} {unset sesnam}
	#JCS
	if {[info exists seauth]} {unset seauth}
	if {[info exists sereview]} {unset sereview}
	if {[info exists senass]} {unset senass}
	if {[info exists sendef]} {unset sendef}
	if {[info exists seotime]} {unset seotime}
	if {[info exists seodepth]} {unset seodepth}
	if {[info exists semag]} {unset semag}
	if {[info exists semagtype]} {unset semagtype}
	if {[info exists seext]} {unset seext}
	if {[info exists serev]} {unset serev}
	if {[info exists seregion]} {unset seregion}
	if {[info exists selist]} {unset selist}
	if {[info exists orlistelements]} {unset orlistelements}
	if {[info exists currentorselection]} {unset currentorselection}
	if {[info exists sestachans]} {unset sestachans}
	if {[info exists setimedef]} {unset setimedef}

	set dbe [dblookup $db 0 event 0 dbSCRATCH]
	set dbo [dblookup $db 0 origin 0 0]
	set dba [dblookup $db 0 assoc 0 0]
	set dbar [dblookup $db 0 arrival 0 dbSCRATCH]
	set dbd [dblookup $db 0 detection 0 dbSCRATCH]
	set dbs [dblookup $db 0 site 0 dbSCRATCH]
	if {[info exists prefor($evid)] == 0} {
		tk_messageBox -type ok -message "No event to show"
		return
	}
	set seprefor $prefor($evid)
	dbputv $dbe 0 dbSCRATCH evid $evid
	set origin_recs [dbmatches $dbe $dbo orhook evid\#evid]
	set n [llength $origin_recs]
	if {$n < 1} {
		tk_messageBox -type ok -message "No origins to show"
		return
	}

	set seevid $evid
	set seselected $seprefor
	foreach rec $origin_recs {
		set dbo [lreplace $dbo 3 3 $rec]
		set orid [dbgetv $dbo 0 $rec orid]
		set seorid($orid) $orid
		set seolat($orid) [dbgetv $dbo 0 $rec lat]
		set seolon($orid) [dbgetv $dbo 0 $rec lon]
		set seauth($orid) [dbgetv $dbo 0 $rec auth]
		set sereview($orid) [dbgetv $dbo 0 $rec review]
		set senass($orid) [dbgetv $dbo 0 $rec nass]
		set sendef($orid) [dbgetv $dbo 0 $rec ndef]
		set seotime($orid) [dbgetv $dbo 0 $rec time]
		set seodepth($orid) [dbgetv $dbo 0 $rec depth]
		set ondate [dbgetv $dbo 0 $rec jdate]

		set magtype mb
		set mag [dbgetv $dbo 0 $rec mb]
		if {$mag == "" || $mag == "{}"} {set mag -999.0}
		if {$mag == -999.0} {
			set magtype ms
			set mag [dbgetv $dbo 0 $rec ms]
			if {$mag == "" || $mag == "{}"} {set mag -999.0}
			if {$mag == -999.0} {
				set magtype ml
				set mag [dbgetv $dbo 0 $rec ml]
				if {$mag == "" || $mag == "{}"} {set mag -999.0}
			}
		}
		set semag($orid) $mag
		set semagtype($orid) $magtype

		if {[info exists external_catalog_authors($seauth($orid))] == 1} {
			set seext($orid) $external_catalog_authors($seauth($orid))
		} else {
			set seext($orid) 0
		}
		if {[info exists local_catalog_authors($seauth($orid))] == 1} {
			set serev($orid) $local_catalog_authors($seauth($orid))
		} else {
			if {$sereview($orid) == "y"} {
				set serev($orid) r
			} else {
				set serev($orid) 0
			}
		}
		set seregion($orid) [dbeval $db "grname($seolat($orid),$seolon($orid))"]
		if {$seext($orid) != "0"} {
			set rev $seext($orid)
		} else {
			if {$seext($orid) != "0"} {
				set rev $seext($orid)
			} else {
				set rev " "
			}
		}
		if {$mag == -999.0} {
			set selist($orid) [format "%s        %s %3d %s\n     lat = %s, lon = %s, depth = %s\n     orid = %d, nass = %d, evid = %d\n     auth = %s\n" \
				[mystrtime $seotime($orid)] $rev $sendef($orid) $seregion($orid) \
				$seolat($orid) $seolon($orid) $seodepth($orid) \
				$orid $senass($orid) $evid \
				$seauth($orid)] 
		} else {
			set selist($orid) [format "%s %4.1f%s %s %3d %s\n     lat = %s, lon = %s, depth = %s\n     orid = %d, nass = %d, evid = %d\n     auth = %s\n" \
				[mystrtime $seotime($orid)] $mag $magtype $rev $sendef($orid) $seregion($orid) \
				$seolat($orid) $seolon($orid) $seodepth($orid) \
				$orid $senass($orid) $evid \
				$seauth($orid)]
		}


		set arids [list]
		if {[info exists scs]} {unset scs}
                ############
                #On Wed Jun 15 09:48:58 AKDT 2011 Mitch added the below dbquery on
                # dbo to prevent the "dbmatches dbo dba ashook" from crashing dbevents
                set dbon [dbquery $dbo dbRECORD_COUNT]
                set dban [dbquery $dba dbRECORD_COUNT]
                ############

		set assoc_recs [dbmatches $dbo $dba ashook]
		foreach arec $assoc_recs {
			set arid [dbgetv $dba 0 $arec arid]
			set delta [dbgetv $dba 0 $arec delta]
			set timedef [dbgetv $dba 0 $arec timedef]
			if {"$timedef" == "-"} {set timedef "n"}
			if {"$timedef" == ""} {set timedef "n"}
			lappend arids $arid
			dbputv $dbar 0 dbSCRATCH arid $arid
			set arrival_recs [dbmatches $dbar $dbar ar1hook arid\#arid]
			if {[llength $arrival_recs] < 1} continue
			set arrec [lindex $arrival_recs 0]
			set sta [dbgetv $dbar 0 $arrec sta]
			set chan [dbgetv $dbar 0 $arrec chan]
			if {[info exists sesta($orid,$sta)] == 0} {
				dbputv $dbs 0 dbSCRATCH sta $sta ondate $ondate offdate $ondate
				set site_recs [dbmatches $dbs $dbs st1hook]
				if {[llength $site_recs] > 0} {
					set srec [lindex $site_recs 0]
					set lat [dbgetv $dbs 0 $srec lat]
					set lon [dbgetv $dbs 0 $srec lon]
					#JCSa
					set elev [dbgetv $dbs 0 $srec elev]
					set staname [dbgetv $dbs 0 $srec staname]
					#JCSb
				} else {
					set lat -1000.0
					set lon -1000.0
					#JCSa
					#set elev -1000.0
					#set staname ""
					#JCSb
				}
				set sesta($orid,$sta) $sta
				lappend sestas($orid) $sta
				set seslat($orid,$sta) $lat
				set seslon($orid,$sta) $lon
				#JCSa
				set sesel($orid,$sta) $elev
				set sesnam($orid,$sta) $staname
				#JCSb
				set setimedef($orid,$sta) $timedef
			}
			set stachan [format "%s:%s" $sta $chan]
			if {[info exists scs($stachan)] == 0} {
				set scs($stachan) $delta
				lappend sestachans($orid) [format "%s/%s/%s" $stachan $delta $timedef]
			} else {
			}
		}

		dbputv $dbs 0 dbSCRATCH ondate $ondate offdate $ondate
		set site_recs [dbmatches $dbs $dbs sthook ondate::offdate\#ondate::offdate]
		foreach srec $site_recs {
			set sta [dbgetv $dbs 0 $srec sta]
			set lat [dbgetv $dbs 0 $srec lat]
			set lon [dbgetv $dbs 0 $srec lon]
			#JCSa
			set elev [dbgetv $dbs 0 $srec elev]
			set staname [dbgetv $dbs 0 $srec staname]
			#JCSb
			set delta [dbeval $dba "distance\($lat,$lon,$seolat($orid),$seolon($orid)\)"]
			set pretime [dbeval $dba "ptime\($delta,$seodepth($orid)\)+$seotime($orid)"]
			dbputv $dbar 0 dbSCRATCH sta $sta time $pretime
			set arrival_recs [dbmatches $dbar $dbar arhook sta\#sta time\#\(time-10.0\)::\(time+30.0\)]
			foreach arec $arrival_recs {
				set arid [dbgetv $dbar 0 $arec arid]
				set chan [dbgetv $dbar 0 $arec chan]
				set j [lsearch -exact $arids $arid]
				if {$j < 0} {
					set timedef 0
				} else {
					continue
				}
				if {[info exists sesta($orid,$sta)] == 0} {
					set sesta($orid,$sta) $sta
					lappend sestas($orid) $sta
					set seslat($orid,$sta) $lat
					set seslon($orid,$sta) $lon
					#JCSa
					set sesel($orid,$sta) $elev
					set sesnam($orid,$sta) $staname
					#JCSb
					set setimedef($orid,$sta) $timedef
				}
				set stachan [format "%s:%s" $sta $chan]
				if {[info exists scs($stachan)] == 0} {
					set scs($stachan) $delta
					lappend sestachans($orid) [format "%s/%s/%s" $stachan $delta $timedef]
				} else {
				}
			}
			# dbputv $dbd 0 dbSCRATCH sta $sta time $pretime
			# set detection_recs [dbmatches $dbd $dbd dthook sta\#sta time\#\(time-10.0\)::\(time+30.0\)]
			# foreach drec $detection_recs {
			# 	set chan [dbgetv $dbd 0 $drec chan]
			# 	if {[info exists sesta($orid,$sta)] == 0} {
			# 		set sesta($orid,$sta) $sta
			# 		lappend sestas($orid) $sta
			# 		set seslat($orid,$sta) $lat
			# 		set seslon($orid,$sta) $lon
			#JCSa
			##set sesel($orid,$sta) $elev
			#set sesnam($orid,$sta) $staname
			#JCSb
			# 		set setimedef($orid,$sta) "1"
			# 	}
			# 	set stachan [format "%s:%s" $sta $chan]
			# 	if {[info exists scs($stachan)] == 0} {
			# 		set scs($stachan) $delta
			# 		lappend sestachans($orid) [format "%s/%s/1" $stachan $delta]
			# 	} else {
			# 	}
			# }
		}
		if {[info exists sestachans($orid)]} {
			if {[llength $sestachans($orid)] > 0} {
				set sestachans($orid) [lsort -command mycompare $sestachans($orid)]
			}
		}
	}

	plot_origins
}

proc mycompare {arg1 arg2} {
	set delta1 [lindex [split $arg1 "/"] 1]
	set delta2 [lindex [split $arg2 "/"] 1]
	if {$delta2 > $delta1} {
		return -1
	} elseif {$delta2 < $delta1} {
		return 1
	} else {
		return 0
	}
}

proc restartdbpick {} {
	send2dbpick quit
	after 5000 setwfevent
}

proc make_eventmenu {menu} {
	menu $menu
	$menu add command -label "Evid: " -command {}
	$menu add command -label "Orid: " -command {}
	$menu add command -label "Time: " -command {}
	$menu add command -label "Lat: " -command {}
	$menu add command -label "Lon: " -command {}
	$menu add command -label "Depth: " -command {}
	$menu add command -label "Mag: " -command {}
	$menu add command -label "Auth: " -command {}
	$menu add command -label "Reviewed: " -command {}
	$menu add command -label "Nassoc  : " -command {}
	$menu add command -label "Ndef    : " -command {}
	$menu add separator
	#JCSa
	$menu add command -label "Show Detail Map" -command display_webmap
	$menu add separator
	$menu add command -label "Delete event" -command delete_event
	$menu add command -label "Delete event QDDS" -command delete_event_qdds
	#JCSb
}

proc make_originmenu {menu} {
	menu $menu
	$menu add command -label "Evid: " -command {}
	$menu add command -label "Orid: " -command {}
	$menu add command -label "Time: " -command {}
	$menu add command -label "Lat: " -command {}
	$menu add command -label "Lon: " -command {}
	$menu add command -label "Depth: " -command {}
	$menu add command -label "Mag: " -command {}
	$menu add command -label "Auth: " -command {}
	$menu add command -label "Reviewed: " -command {}
	$menu add command -label "Nassoc  : " -command {}
	$menu add command -label "Ndef    : " -command {}
	#JCSa
	$menu add separator
	$menu add command -label "Show Detail Map" -command display_webmap
#	$menu add separator
#	$menu add command -label "Delete event QDDS" -command delete_event_qdds
	#JCSb
}

#JCSa
proc make_stamenu {menu} {
	menu $menu
	$menu add command -label "Sta: " -command {}
	$menu add command -label "Lat: " -command {}
	$menu add command -label "Lon: " -command {}
	$menu add command -label "Elev: " -command {}
	$menu add command -label "Name: " -command {}
}
#JCSb


proc incrementmap {} {
	global mapon
	global mapindex
	global mapnames

	set i $mapindex($mapon)
	incr i
	if {$i >= [array size mapindex]} {set i 0} 
	foreach map $mapnames {
		if {$mapindex($map) == $i} {
			set mapon $map
			set_map $map
			return
		}
	}
}

proc seteventmenu {menu evid} {
	global evtime
	global evlat
	global evlon
	global evdepth
	global evauth
	global evreview
	global evnass
	global evndef
	global evmag
	global evmagtype
	global evorid
	global delete_evid

	$menu entryconfigure 1 -label [format "Evid:     %s" $evid]
	$menu entryconfigure 2 -label [format "Orid:     %s" $evorid($evid)]
	$menu entryconfigure 3 -label [format "Time:     %s" [strtime $evtime($evid)]]
	$menu entryconfigure 4 -label [format "Lat:      %s" $evlat($evid)]
	$menu entryconfigure 5 -label [format "Lon:      %s" $evlon($evid)]
	$menu entryconfigure 6 -label [format "Depth:    %s" $evdepth($evid)]
	if {$evmag($evid) < 0.0} {
		$menu entryconfigure 7 -label [format "Mag:      "]
	} else {
		$menu entryconfigure 7 -label [format "Mag:      %s%s" $evmag($evid) $evmagtype($evid)]
	}
	$menu entryconfigure 8 -label [format "Auth:     %s" $evauth($evid)]
	$menu entryconfigure 9 -label [format "Reviewed: %s" $evreview($evid)]
	$menu entryconfigure 10 -label [format "Nassoc  : %s" $evnass($evid)]
	$menu entryconfigure 11 -label [format "Ndef    : %s" $evndef($evid)]
	set delete_evid $evid
}

proc setoriginmenu {menu orid} {
	global seevid
	global sestas
	global sesta
	global seolat
	global seolon
	global seslat
	global seslon
	#JCSa
	global sesel
	global sesnam
	#JCSb
	global seevid
	global seprefor
	global seauth
	global sereview
	global senass
	global sendef
	global seotime
	global seodepth
	global semag
	global semagtype
	global seext
	global serev
	global seregion
	global selist
	global orlistelements

	$menu entryconfigure 1 -label [format "Evid:     %s" $seevid]
	$menu entryconfigure 2 -label [format "Orid:     %s" $orid]
	$menu entryconfigure 3 -label [format "Time:     %s" [strtime $seotime($orid)]]
	$menu entryconfigure 4 -label [format "Lat:      %s" $seolat($orid)]
	$menu entryconfigure 5 -label [format "Lon:      %s" $seolon($orid)]
	$menu entryconfigure 6 -label [format "Depth:    %s" $seodepth($orid)]
	if {$semag($orid) < 0.0} {
		$menu entryconfigure 7 -label [format "Mag:      "]
	} else {
		$menu entryconfigure 7 -label [format "Mag:      %s%s" $semag($orid) $semagtype($orid)]
	}
	$menu entryconfigure 8 -label [format "Auth:     %s" $seauth($orid)]
	$menu entryconfigure 9 -label [format "Reviewed: %s" $sereview($orid)]
	$menu entryconfigure 10 -label [format "Nassoc  : %s" $senass($orid)]
	$menu entryconfigure 11 -label [format "Ndef    : %s" $sendef($orid)]
}

#JCSa
proc setstamenu {menu sta orid} {
	global sestas
	global sesta
	global seslat
	global seslon
	global sesel
	global sesnam
	$menu entryconfigure 1 -label [format "Sta:      %s" $sesta($orid,$sta)]
	$menu entryconfigure 2 -label [format "Lat:      %s" $seslat($orid,$sta)]
	$menu entryconfigure 3 -label [format "Lon:      %s" $seslon($orid,$sta)]
	$menu entryconfigure 4 -label [format "Elev(km): %s" $sesel($orid,$sta)]
	$menu entryconfigure 5 -label [format "Name:     %s" $sesnam($orid,$sta)]
}
proc setstamenu_val {menu sta lat lon stanam elev val} {
	$menu entryconfigure 1 -label [format "Sta:      %s" $sta]
	$menu entryconfigure 2 -label [format "Lat:      %s" $lat]
	$menu entryconfigure 3 -label [format "Lon:      %s" $lon]
	$menu entryconfigure 4 -label [format "Elev(km): %s" $sel]
	$menu entryconfigure 5 -label [format "Name:     %s" $nam]
	$menu entryconfigure 6 -label [format "Value:     %s" $val]

}

#JCSb

proc plot_star {canvas x y siz color tags selected} {
	global nstar
	global xstar
	global ystar

	set cmd "$canvas create polygon "
	if {$selected == 1} {
		set x1 [expr $x-1.5*$siz]
		set x2 [expr $x1+3.0*$siz]
		set y1 [expr $y-1.5*$siz]
		set y2 [expr $y1+3.0*$siz]
		$canvas create oval $x1 $y1 $x2 $y2 -fill #000000 -tags $tags
	}
	for {set i 0} {$i < $nstar} {incr i} {
		set xx [expr int($x+($xstar($i)*$siz)+0.5)]
		set yy [expr int($y+($ystar($i)*$siz)+0.5)]
		append cmd "$xx $yy "
	}
	if {$color != "none"} {
		append cmd "-fill $color "
	}
	append cmd "-outline #000000 -tags {"
	foreach tag $tags {
		append cmd "$tag "
	}
	append cmd "}"
	eval "$cmd"
}

proc plot_triangle {canvas x y siz color tags selected} {
	global ntriangle
	global xtriangle
	global ytriangle

	set cmd "$canvas create polygon "
	if {$selected == 1} {
		set x1 [expr $x-1.5*$siz]
		set x2 [expr $x1+3.0*$siz]
		set y1 [expr $y-1.5*$siz]
		set y2 [expr $y1+3.0*$siz]
		$canvas create oval $x1 $y1 $x2 $y2 -fill #000000 -tags $tags
	}
	for {set i 0} {$i < $ntriangle} {incr i} {
#puts "x y siz i: '$x' '$y' '$siz' '$i'"
#		puts "ntriangle xtriangle(i) ytriangle(i): '$ntriangle' '$xtriangle($i)'	'$ytriangle($i)'"
		set xx [expr int($x+($xtriangle($i)*$siz)+0.5)]
		set yy [expr int($y+($ytriangle($i)*$siz)+0.5)]
		append cmd "$xx $yy "
	}
	if {$color != "none"} {
		append cmd "-fill $color "
	}
	append cmd "-outline #000000 -tags {"
	foreach tag $tags {
#	puts "tag: $tag"
		append cmd "$tag "
	}
	append cmd "}"
	eval "$cmd"
}

proc drawlegend {} {
	global mapnames
	global maplegend
	global legendwidth
	global legendheight
	global symbol_color_number
	global symbol_color_unsel
	global symbol_color_text

	foreach map $mapnames {
	$maplegend($map) create rectangle 1 1 [expr $legendwidth-2] [expr $legendheight-2] -fill #ffffff -outline #000000
	$maplegend($map) create text [expr 0.5*$legendwidth] 5 -anchor n -text LEGEND -font {Times 12 bold} -fill black
	$maplegend($map) create text [expr 0.25*$legendwidth] 20 -anchor n -text STATUS -font {Times 10 bold}
	$maplegend($map) create text [expr 0.75*$legendwidth] 20 -anchor n -text AGE -font {Times 10 bold}
	plot_star $maplegend($map) 25 50 10 white [list legend] 0
	$maplegend($map) create text 40 50 -anchor w -text "- Unreviewed" -font {Times 10 bold}
	$maplegend($map) create rectangle 20 70 30 80 -fill #ffffff -outline #000000
	$maplegend($map) create text 40 75 -anchor w -text "- Reviewed" -font {Times 10 bold}
	$maplegend($map) create oval 20 95 30 105 -fill #ffffff -outline #000000
	$maplegend($map) create text 40 100 -anchor w -text "- Associated" -font {Times 10 bold}
	set x [expr 0.5*$legendwidth + 20]
	set y 50
	for {set i 0} {$i < $symbol_color_number} {incr i} {
		set x1 [expr $x-5]
		set x2 [expr $x+5]
		set y1 [expr $y-5]
		set y2 [expr $y+5]
		$maplegend($map) create rectangle $x1 $y1 $x2 $y2 -fill $symbol_color_unsel($i) -outline $symbol_color_unsel($i)
		set x1 [expr $x+15]
		$maplegend($map) create text $x1 $y -anchor w -text "$symbol_color_text($i)" -font {Times 10 bold}
		incr y 15
	}
	}
}

proc setlegend {} {
	global legendon
	global maplegend
	global mapframe
	global mapcanvas
	global mapon
	global legendwidth
	global legendheight
	global wmapframe
	global hmapframe

	if {$legendon == 1} {
		set x 8
		set y [expr $hmapframe($mapon)-$legendheight-20]
		place $maplegend($mapon) -x $x -y $y -width $legendwidth -height $legendheight
	} else {
		place forget $maplegend($mapon)
	}
}

proc make_events {db} {
	global events
	global evorid
	global evlat
	global evlon
	global evtime
	global evauth
	global evreview
	global evnass
	global evndef
	global evdepth
	global evext
	global evrev
	global evmag
	global evmagtype
	global lastevent
	global evlist
	global evlistevids
	global evevidtotime
	global external_catalog_authors
	global local_catalog_authors
	global stat
	global prefor
#NIKO
	global maxage
	global limit_age
	global magnitude_preference_order
#NIKO
#JCSa
	global evregion
	global evsrn
	global evgrn
	global evnearplaces
#JCSb

	set stat "Making events..."
	update
	set lastevent -1
	if {[info exists events] != 0} {unset events}
	if {[info exists evorid] != 0} {unset evorid}
	if {[info exists evlat] != 0} {unset evlat}
	if {[info exists evlon] != 0} {unset evlon}
	if {[info exists evtime] != 0} {unset evtime}
	if {[info exists evauth] != 0} {unset evauth}
	if {[info exists evreview] != 0} {unset evreview}
	if {[info exists evnass] != 0} {unset evnass}
	if {[info exists evndef] != 0} {unset evndef}
	if {[info exists evdepth] != 0} {unset evdepth}
	if {[info exists evext] != 0} {unset evext}
	if {[info exists evrev] != 0} {unset evrev}
	if {[info exists evmag] != 0} {unset evmag}
	if {[info exists evmagtype] != 0} {unset evmagtype}
	if {[info exists prefor] != 0} {unset prefor}
	if {[info exists evlist] != 0} {unset evlist}
	if {[info exists evlistevids] != 0} {unset evlistevids}
	if {[info exists evevidtotime] != 0} {unset evevidtotime}
	#JCSa
	if {[info exists evnearplaces] != 0} {unset evnearplaces}
	#JCSb

	set dbe [dblookup $db 0 event 0 0]
	set n [dbquery $dbe dbRECORD_COUNT]
	for {set i 0} {$i < $n} {set i [expr $i+1]} {
		set evid [dbgetv $dbe 0 $i evid]
		if {$evid < 0} continue
		set prefor($evid) [dbgetv $dbe 0 $i prefor]
	}
	set dbo [dblookup $db 0 origin 0 0]
#NIKO
#time subset if desired 
	if { $limit_age} {
		set my_timenow [clock seconds]
		set my_oldest [expr $my_timenow - $maxage]
		set dbo [dbsubset $dbo "time > $my_oldest"]
	}
#NIKO
		
	set n [dbquery $dbo dbRECORD_COUNT]
	for {set i 0} {$i < $n} {set i [expr $i+1]} {
		set evid [dbgetv $dbo 0 $i evid]
		if {$evid < 0} continue
		set orid [dbgetv $dbo 0 $i orid]
		if {$orid < 0} continue
		set lat [dbgetv $dbo 0 $i lat]
		set lon [dbgetv $dbo 0 $i lon]
		set auth [dbgetv $dbo 0 $i auth]
		set review [dbgetv $dbo 0 $i review]
		set nass [dbgetv $dbo 0 $i nass]
		set ndef [dbgetv $dbo 0 $i ndef]
		set time [dbgetv $dbo 0 $i time]
		set depth [dbgetv $dbo 0 $i depth]
#NIKO
#change magnitude display
		foreach magtype $magnitude_preference_order {
			set mag [dbgetv $dbo 0 $i $magtype]
			if {$mag == "" || $mag == "{}"} {set mag -999.0}
			if {$mag != -999.0} {
				break
			}
		}
#NIKO
#		set magtype mb
#		set mag [dbgetv $dbo 0 $i mb]
#		if {$mag == "" || $mag == "{}"} {set mag -999.0}
#		if {$mag == -999.0} {
#			set magtype ms
#			set mag [dbgetv $dbo 0 $i ms]
#			if {$mag == "" || $mag == "{}"} {set mag -999.0}
#			if {$mag == -999.0} {
#				set magtype ml
#				set mag [dbgetv $dbo 0 $i ml]
#				if {$mag == "" || $mag == "{}"} {set mag -999.0}
#			}
#		}
		if {[info exists events] == 0} {
			set event ev$evid
			set events($evid) $event
		} else {
			if {[info exists events($evid)] == 0} {
				set event ev$evid
				set events($evid) $event
			} else {
				set event $events($evid)
			}
		}
		if {[info exists prefor($evid)] == 0 && $evid > 0} {set prefor($evid) $orid}
		if {[info exists prefor($evid)] != 0 && $orid == $prefor($evid)} {
			set evorid($evid) $orid
			set evlat($evid) $lat
			set evlon($evid) $lon
			set evauth($evid) $auth
			set evreview($evid) $review
			set evnass($evid) $nass
			set evndef($evid) $ndef
			set evdepth($evid) $depth
			if {[info exists external_catalog_authors($auth)] == 1} {
				set evext($evid) $external_catalog_authors($auth)
			} else {
				set evext($evid) 0
			}
			if {[info exists local_catalog_authors($auth)] == 1} {
				set evrev($evid) $local_catalog_authors($auth)
			} else {
				if {$review == "y"} {
					set evrev($evid) r
				} else {
					set evrev($evid) 0
				}
			}
			set evtime($evid) $time
			set evmag($evid) $mag
			set evmagtype($evid) $magtype
			set evregion($evid) [dbeval $db "grname($lat,$lon)"]
#JCSa
			set evsrn($evid) [dbeval $db "srn($lat,$lon)"]
			set evgrn($evid) [dbeval $db "grn($lat,$lon)"]
#JCSb
			if {$time > $lastevent} {set lastevent $time}
			set ftime [mystrtime $time]
			if {$evext($evid) != "0"} {
				set rev $evext($evid)
			} else {
				if {$evrev($evid) != "0"} {
					set rev $evrev($evid)
				} else {
					set rev " "
				}
			}
			if {$mag == -999.0} {
				set evlist($ftime) [format "%s        %s %3d %s\n" $ftime $rev $evndef($evid) $evregion($evid)]
			} else {
				set evlist($ftime) [format "%s %4.1f%s %s %3d %s\n" $ftime $mag $magtype $rev $evndef($evid) $evregion($evid)]
			}
			set evlistevids($ftime) $evid
			set evevidtotime($evid) $ftime
			#JCSa set up nearest places stuff for prefor
			set evnearplaces($evid) [nearest_places $evid]
			#JCSb
		}
	}
}

proc set_map {mapn} {
	global mapnames
	global mapcanvas
	global mapframe
	global mapxscroll
	global mapyscroll
	global mapon
	global maplegend

	set mapon $mapn
	foreach map $mapnames {
		if {$map != $mapon} {
			pack forget $mapcanvas($map)
			pack forget $mapxscroll($map)
			pack forget $mapyscroll($map)
			pack forget $mapframe($map)
			place forget $maplegend($map)
		}
	}
	pack $mapcanvas($mapon) $mapxscroll($mapon) -fill x
	pack $mapyscroll($mapon) $mapframe($mapon) -side left -fill y
	setlegend
}

# Moved following lines from lines approx 2227-2244 to here to use Pf 
# to obtain soundfile JCSa
package require Datascope
package require Pixmap
set limit_age 0
set maxage -1

set Pf aeic_dbevents

for {set i 0} {$i<[expr $argc - 1]} {incr i} {
	switch -- [lindex $argv $i] {
		-maxage {
			set maxage [expr 86400 * [lindex $argv [expr $i + 1]]]
			set limit_age 1
			incr i
		}
		-pf {
			set Pf [lindex $argv [expr $i + 1]]
			incr i
		}
	}
}
# end of move JCSb

#JCSa
set alerton		[pfget $Pf alerton]
set repeaton		[pfget $Pf repeaton]
set qddson		[pfget $Pf qddson]
set autodetailmap	[pfget $Pf autodetailmap]
#JCSb
proc plot_events {db reread refork} {
	global events
	global mapnames
	global mapcanvas
	global update_ms
	global id
	global stat
	global lastupdate
	global evlist
	global evevidtotime
	global evlistevids
	global evlistelements
	global currentselection
	global mtime_event
	global mtime_origin
	global dbpickon
	global textwidth
	global hmapframe
	global last_evid
	global evidselection
#JCSa
	global Pf
	global evregion
	global evmag
	global evmagtype
	global evsrn
	global evgrn
	global alerton
	global repeaton
	global qddson
	global autodetailmap
	global mag_thresh
	global mysrn
	global usemysrn
#JCSb


	if {$reread > 0} {
		set fname [dbquery [dblookup $db 0 event 0 0] dbTABLE_FILENAME]
		if {[file exists $fname] == 0} {
			set id [after $update_ms {plot_events $db 1 1}]
			return
		}
		set mtimee [file mtime $fname]
		set fname [dbquery [dblookup $db 0 origin 0 0] dbTABLE_FILENAME]
		if {[file exists $fname] == 0} {
			set id [after $update_ms {plot_events $db 1 1}]
			return
		}
		set mtimeo [file mtime $fname]
		if {$mtimee == $mtime_event && $mtimeo == $mtime_origin} {
			if {$reread != 2} {
				set id [after $update_ms {plot_events $db 1 1}]
				return
			}
		} else {
			set mtime_event $mtimee
			set mtime_origin $mtimeo
			set lastupdate -1
			make_events $db
			set lastupdate [clock_seconds]
			set evidselection end
		}
	}
	set stat "Plotting events..."
	update
	.f.events.txt configure -state normal
	if {$reread == 2} {set reread 1}
	if {[info exists evlistelements] == 1} {unset evlistelements}
	if {[info exists currentselection] == 1} {unset currentselection}
	set tags [.f.events.txt tag names]
	foreach tg $tags {
		.f.events.txt tag delete $tg
	}
	.f.events.txt delete 0.0 end
	set times [lsort [array names evlist]]
	unset times
	set levids [lsort -integer [array names evevidtotime]]
	foreach evid $levids {
		lappend times $evevidtotime($evid)
		set l_evid $evid
	}
	set n [llength $times]
	set evlistelements [list]
	for {set i 0} {$i < $n} {incr i} {
		set time [lindex $times $i]
		set evtag [format "ev%d" $evlistevids($time)]
		if {$i == [expr $n-1]} {
			set evlist($time) [string trimright $evlist($time) "\n"]
			for {set j [string length $evlist($time)]} {$j < $textwidth} {incr j} {
				append evlist($time) " "
			}
		}
		.f.events.txt insert end $evlist($time) [list $evtag]
		.f.events.txt tag configure $evtag -background #ffffff
		.f.events.txt tag bind $evtag <ButtonPress-1> "setselection $evtag"
		#JCSa
#		.f.events.txt bind $evtag <ButtonPress-3> "originmenu $orid %x %y"
		#JCSb
		.f.events.txt tag bind $evtag <Up> "+decrementselection"
		.f.events.txt tag bind $evtag <Down> "+incrementselection"
		lappend evlistelements $evtag
	}
	set evids [array names events]
	foreach map $mapnames {
		$mapcanvas($map) delete events
		foreach evtag $evlistelements {
			plot_event $map $evtag 0
		}
	}

	#JCSa
	# moved audioplay inside if {$l_evid != $last_evid}
	# original has set dbpickon 3 in this if statement, this 
	# should clear up double audioplay execution when mag comes in late 
	# dbpickon 0 and l_evid -1 on startup - no matter what
#puts "dbpickon: $dbpickon   last_evid:$last_evid   l_evid:$l_evid"

	if {$dbpickon == 2} {
	  if {$l_evid != $last_evid} {set dbpickon 3}
	    if {$alerton == 1} {
		set soundpath [pfget $Pf soundpath]
		set audioplayer [pfget $Pf audioplayer]
		set audiovol [pfget $Pf audiovol]
		set pt $soundpath/point.au
		set eqin $soundpath/eqin.au
		set mg $soundpath/mag.au
		set snd $soundpath/[pfget $Pf soundfile]

		# GT: 20070606 modifying to use geographic region sound file if it exists
		# otherwise will use the more broad seismic region sound file
		set grnfile  $soundpath/grn$evgrn($l_evid).au
		set srnfile  $soundpath/srn$evsrn($l_evid).au

	 	if {[file exists $grnfile] == 1} {
	 	   set eqloc $grnfile
		} else {
	           set eqloc $srnfile
		}
		# end of GT:browse confirm saveas
		

		set mg1 [lindex [split [format "%.1f" $evmag($l_evid)] .] 0]
		set mg2 [lindex [split [format "%.1f" $evmag($l_evid)] .] 1]
		set mg1f $soundpath/$mg1.au
		set mg2f $soundpath/$mg2.au
	  	
		if {$l_evid != $last_evid} {
		  # small detail map not available for new events
		  # because dbrecenteqs likely has not built map
		  # so kill imagewin if it is running
		  if {[winfo exists .imagewin]} {
			destroy .imagewin
			puts stderr "Destroying imagewindow for new event"
		  }
	          # this is only place concerned if dbpick is running, restarts dbpick
	        #  if {$dbpickon == 2} {set dbpickon 3}
		  if {$mg1 >= $mag_thresh || $evmag($l_evid) == -999.0} {
		    if {($usemysrn == 1 && $evsrn($l_evid) == $mysrn) || $usemysrn == 0} {
		     if {$evmag($l_evid) == -999.0} {
		      set nomg $soundpath/nomag.au
		      #set sndstr {$snd $eqin $eqloc $nomg}
		      #exec $audioplayer -v $audiovol $snd $eqin $eqloc $nomg &
		      puts stderr "Magnitude $evmag($l_evid) $evmagtype($l_evid) in $evregion($l_evid) $l_evid"
system( "dbfixchanids $q $dbname" );
		      system( "$audioplayer -v $audiovol $snd" );
		      system( "$audioplayer -v $audiovol $eqin" );
		      system( "$audioplayer -v $audiovol $eqloc" );
		      system( "$audioplayer -v $audiovol $nomg" );
		     } else {
		      #set sndstr {$snd $eqin $eqloc $mg $mg1f $pt $mg2f}
		      #exec $audioplayer -v $audiovol $snd $eqin $eqloc $mg $mg1f $pt $mg2f & 
		      puts stderr "Magnitude $evmag($l_evid) $evmagtype($l_evid) in $evregion($l_evid) $l_evid"
		      system( "$audioplayer -v $audiovol $snd" ); 
		      system( "$audioplayer -v $audiovol $eqin" );
		      system( "$audioplayer -v $audiovol $eqloc" );
		      system( "$audioplayer -v $audiovol $mg" );
		      system( $audioplayer -v $audiovol $mg1f" );
		      system( "$audioplayer -v $audiovol $pt" );
		      system( "$audioplayer -v $audiovol $mg2f" );
		     }
		     # end usemysrn
		    }
		   }
		  if {$repeaton == 1} {
		   puts stderr "Sorry, repeat option not available"
		  } 
		 # New magnitude for previous event
		 # this part doesnt work properly, repeats info
		#} elseif {$l_evid == $last_evid && $dbpickon != 1} { 
		#  if {$mg1 >= $mag_thresh} {
		#    if {($usemysrn == 1 && $evsrn($l_evid) == $mysrn) || $usemysrn == 0} {
		#     set newmag $soundpath/newmag.au
		#     set prevev $soundpath/prevev.au
		#     exec $audioplayer -v $audiovol $newmag $prevev $mg $mg1f $pt $mg2f &
	 	#     puts stderr "Magnitude $evmag($l_evid) $evmagtype($l_evid) in $evregion($l_evid) $l_evid"
		#    }
		#  }
		 # end levid!=lastevid
		}
		# end alerton
	    }
	 # end dbpickon!=0
	} 
    #JCSb
	set last_evid $l_evid
	if {[info exists evidselection] == 0} {set evidselection end}
	if {"$evidselection" == "end"} {
		set tag [lindex $evlistelements end]
		set evidselection [string trimleft [string range $tag 2 end] 0]
	}
	setselection [format "ev%d" $evidselection]
	set stat Done
	if {$refork == 1} {
		set id [after $update_ms "plot_events [list $db] $reread 1"]
	} else {
		set id -1
	}
	.f.events.txt configure -state disabled
}

proc plot_origins {} {
	global seorid
	global sestas
	global sesta
	global seolat
	global seolon
	global seslat
	global seslon
	global seevid
	global seprefor
	global seauth
	global sereview
	global senass
	global sendef
	global seotime
	global seodepth
	global semag
	global semagtype
	global seext
	global serev
	global seregion
	global selist
	global orlistelements
	global citem
	global show_origins
	global stat
	global evnearplaces #JCS

	global mapnames
	global mapcanvas

	set stat "Plotting origins..."
	update
	.f.showevent.txt configure -state normal
	set orids [array names seorid]

	set tags [.f.showevent.txt tag names]
	foreach tg $tags {
		.f.showevent.txt tag delete $tg
	}
	.f.showevent.txt delete 0.0 end

	set orid $seprefor
#JCSa
#set evid $seevid($orid)
#JCSb
	set ortag [format "or%d" $orid]
	lappend orlistelements $ortag
	.f.showevent.txt insert end $selist($orid) [list $ortag]
	.f.showevent.txt insert end $evnearplaces($seevid) [list $ortag]
	.f.showevent.txt tag configure $ortag -background #ffffff -foreground red
	.f.showevent.txt tag bind $ortag <ButtonPress-1> "setorselection $ortag"
	.f.showevent.txt tag bind $ortag <Up> "+decrementorselection"
	.f.showevent.txt tag bind $ortag <Down> "+incrementorselection"
	set selection $ortag

	foreach orid $orids {
		if {$orid == $seprefor} continue
		set ortag [format "or%d" $orid]
		.f.showevent.txt insert end $selist($orid) [list $ortag]
		.f.showevent.txt tag configure $ortag -background #ffffff -foreground black
		.f.showevent.txt tag bind $ortag <ButtonPress-1> "setorselection $ortag"
		.f.showevent.txt tag bind $ortag <Up> "+decrementorselection"
		.f.showevent.txt tag bind $ortag <Down> "+incrementorselection"
		lappend orlistelements $ortag
	}

	foreach map $mapnames {
		if {$show_origins != 0} {
			$mapcanvas($map) lower events base
		}
		$mapcanvas($map) delete origins
		$mapcanvas($map) delete stations
		foreach orid $orids {
			set ortag [format "or%d" $orid]
			plot_origin $map $ortag 0
		}
		if {$show_origins == 0} {
			$mapcanvas($map) lower origins base
			$mapcanvas($map) lower stations base
		}
	}
	setorselection $selection
	set stat Done
	.f.showevent.txt configure -state disabled
}

#JCSa plot only alarm stations
proc plot_val_stations {db} {
	global mapnames
	global mapcanvas
	global mapcanvasstamenu
	
	set dbs [dblookup $db 0 site 0 dbSCRATCH]
	set site_recs [dbmatches $dbs $dbs st1hook]
	if {[llength $site_recs] > 0} {
		set srec [lindex $site_recs 0]
		set sta [dbgetv $dbs 0 $site_recs sta]
		set lat [dbgetv $dbs 0 $srec lat]
		set lon [dbgetv $dbs 0 $srec lon]
		set elev [dbgetv $dbs 0 $srec elev]
		set staname [dbgetv $dbs 0 $srec staname]
	} 
	plot_station_val $map $sta $lat $lon $val
	
}
#JCSb
proc setselection {tag} {
	global currentselection
	global evlistelements
	global evidselection

	if {[info exists currentselection] == 1} {
		.f.events.txt tag configure [lindex $evlistelements $currentselection] -background #ffffff
		unselect_event
	}
	set currentselection [lsearch -exact $evlistelements $tag]
	set evidselection [string trimleft [string range $tag 2 end] 0]
	.f.events.txt tag configure [lindex $evlistelements $currentselection] -background #aaffaa
	.f.events.txt see [lindex $evlistelements $currentselection].first
	select_event
}

proc setorselection {tag} {
	global currentorselection
	global orlistelements
	global oridselection

	if {[info exists currentorselection] == 1} {
		.f.showevent.txt tag configure [lindex $orlistelements $currentorselection] -background #ffffff
		unselect_origin
	}
	set currentorselection [lsearch -exact $orlistelements $tag]
	set oridselection [string trimleft [string range $tag 2 end] 0]
	.f.showevent.txt tag configure [lindex $orlistelements $currentorselection] -background #aaffaa
	.f.showevent.txt see [lindex $orlistelements $currentorselection].first
	select_origin
}

proc decrementselection {} {
	global currentselection
	global evlistelements
	global evidselection

	if {! [info exists evlistelements]} {
	    bell
	    return
	}
	.f.events.txt tag configure [lindex $evlistelements $currentselection] -background #ffffff
	unselect_event
	incr currentselection -1
	if {$currentselection < 0} {set currentselection 0}
	set evidselection [string trimleft [string range [lindex $evlistelements $currentselection] 2 end] 0]
	.f.events.txt tag configure [lindex $evlistelements $currentselection] -background #aaffaa
	select_event
}

proc decrementorselection {} {
	global currentorselection
	global orlistelements
	global oridselection

	.f.showevent.txt tag configure [lindex $orlistelements $currentorselection] -background #ffffff
	unselect_origin
	incr currentorselection -1
	if {$currentorselection < 0} {set currentorselection 0}
	set oridselection [string trimleft [string range $tag 2 end] 0]
	.f.showevent.txt tag configure [lindex $orlistelements $currentorselection] -background #aaffaa
	.f.showevent.txt see [lindex $orlistelements $currentorselection].first
	select_origin
}

proc incrementselection {} {
	global currentselection
	global evlistelements
	global evidselection

	if {! [info exists evlistelements]} {
	    bell
	    return
	}
	.f.events.txt tag configure [lindex $evlistelements $currentselection] -background #ffffff
	unselect_event
	incr currentselection 1
	if {$currentselection >= [llength $evlistelements]} {set currentselection [expr [llength $evlistelements]-1]}
	set evidselection [string trimleft [string range [lindex $evlistelements $currentselection] 2 end] 0]
	.f.events.txt tag configure [lindex $evlistelements $currentselection] -background #aaffaa
	select_event
}

proc decrementorselection {} {
	global currentorselection
	global orlistelements
	global oridselection

	.f.showevent.txt tag configure [lindex $orlistelements $currentorselection] -background #ffffff
	unselect_origin
	incr currentorselection 1
	if {$currentorselection >= [llength $orlistelements]} {set currentorselection [expr [llength $orlistelements]-1]}
	set oridselection [string trimleft [string range $tag 2 end] 0]
	.f.showevent.txt tag configure [lindex $orlistelements $currentorselection] -background #aaffaa
	.f.showevent.txt see [lindex $orlistelements $currentorselection].first
	select_origin
}

proc select_event {} {
	global mapnames
	global mapcanvas
	global symbol_color_number
	global symbol_color_time
	global symbol_color_unsel
	global symbol_color_sel
	global currentselection
	global evlistelements
	global evtime
	global evlat
	global evlon
	global dbpickon
	global startdbpick
	global automap
	global latminbb
	global latmaxbb
	global lonminbb
	global lonmaxbb
	global priority
	global mapon
	global waittorestart
	global show_origins

	set evtag [lindex $evlistelements $currentselection]
	set evid [string trimleft [string range $evtag 2 end] 0]
	set pr -1
	make_event $evid
	#JCSa
	if {[winfo exists .imagewin]} {
		display_webmap
	}
	#JCSb
	foreach map $mapnames {
		$mapcanvas($map) delete $evtag
		plot_event $map $evtag 1
		if {$show_origins != 0} {
			$mapcanvas($map) lower events base
		}
		if {$evlat($evid) <= $latmaxbb($map) && $evlat($evid) >= $latminbb($map)} {
			if {$evlon($evid) <= $lonmaxbb($map) && $evlon($evid) >= $lonminbb($map)} {
				if {$priority($map) > $pr} {
					set pr $priority($map)
					set newmap $map
				}
			}
		}
	}

# Uncommented the following lines RAH

	 if {$automap == 1} {
	 	if {[info exists newmap] == 1} {
	 		if {$newmap != $mapon} {
	 			set mapon $newmap
	 			set_map $mapon
	 		}
	 	}
	 }
	 if {$dbpickon == 2} {
	 	setwfevent
	 }
	 if {$dbpickon == 3} {
	 	set dbpickon 2
	 	after $waittorestart restartdbpick
	 }
	 if {$startdbpick == 1} {
	 	setwfevent
	 	set startdbpick 0
	 }

# Uncommented to here RAH

}

proc select_origin {} {
	global mapnames
	global mapcanvas
	global currentorselection
	global orlistelements
	global automap
	global newmap
	global mapon
	global seolat
	global seolon
	global seslat
	global seslon
	global setimedef
	global sestas
	global latmaxbb
	global latminbb
	global lonmaxbb
	global lonminbb
	global priority
	global show_origins
	global dbpickon
	global startdbpick
	global waittorestart

	set ortag [lindex $orlistelements $currentorselection]
	set orid [string trimleft [string range $ortag 2 end] 0]
	set pr -1
	foreach map $mapnames {
		$mapcanvas($map) delete $ortag
		$mapcanvas($map) delete stations
		plot_origin $map $ortag 1
		if { [info exists sestas($orid)] } {
		    foreach sta $sestas($orid) {
				set lat $seslat($orid,$sta)
				set lon $seslon($orid,$sta)
#NIKO
#plot stations only if possible
				if { $lat >= -90.0 && $lat <= 90.0 && $lon >= -180.0 && $lon <= 180 } {
#NIKO
					plot_station $map $seslat($orid,$sta) $seslon($orid,$sta) $orid $setimedef($orid,$sta) $sta
#NIKO	
				}
#NIKO
		    }
		}
		if {$show_origins == 0} {
			$mapcanvas($map) lower origins base
			$mapcanvas($map) lower stations base
		}
		$mapcanvas($map) lower origins stations
		if {$seolat($orid) <= $latmaxbb($map) && $seolat($orid) >= $latminbb($map)} {
			if {$seolon($orid) <= $lonmaxbb($map) && $seolon($orid) >= $lonminbb($map)} {
				if {$priority($map) > $pr} {
					set pr $priority($map)
					set newmap $map
				}
			}
		}
	}
	if {$automap == 1} {
		if {[info exists newmap] == 1} {
			if {$newmap != $mapon} {
				set mapon $newmap
				set_map $mapon
			}
		}
	}
	if {$dbpickon == 2} {
		setwfevent
	}
	if {$dbpickon == 3} {
		set dbpickon 2
		after $waittorestart restartdbpick
	}
	if {$startdbpick == 1} {
		setwfevent
		set startdbpick 0
	}
}

proc unselect_event {} {
	global mapnames
	global mapcanvas
	global symbol_color_number
	global symbol_color_time
	global symbol_color_unsel
	global symbol_color_sel
	global currentselection
	global evlistelements
	global evtime
	global show_origins

	set evtag [lindex $evlistelements $currentselection]
	foreach map $mapnames {
		$mapcanvas($map) delete $evtag
		plot_event $map $evtag 0
		if {$show_origins == 1} {
			$mapcanvas($map) lower events base
		}
	}
}

proc unselect_origin {} {
	global mapnames
	global mapcanvas
	global currentorselection
	global orlistelements
	global show_origins

	set ortag [lindex $orlistelements $currentorselection]
	foreach map $mapnames {
		$mapcanvas($map) delete $ortag
		plot_origin $map $ortag 0
		if {$show_origins == 0} {
			$mapcanvas($map) lower origins base
			$mapcanvas($map) lower stations base
		}
	}
}

proc plot_event {map evtag selected} {
	global proj
	global evtime
	global evlat
	global evlon
	global evext
	global evrev
	global xscale
	global yscale
	global xl
	global yt
	global latc
	global lonc
	global symsiz
	global symsizo2
	global mapcanvas
	global mapcanvasmenu
	#JCS
	global mapcanvasstamenu
	#JCS
	global symbol_color_number
	global symbol_color_time
	global symbol_color_unsel
	global symbol_color_sel
	global SQRT2
        global width
        global height
	global wmapframe
	global hmapframe

	set evid [string trimleft [string range $evtag 2 end] 0]
	set time [expr int([clock_seconds] - $evtime($evid) + 0.5)]
	for {set i 0} {$i < $symbol_color_number} {incr i} {
		if {$time <= $symbol_color_time($i)} break
	}
	if {$selected == 1} {
		set color $symbol_color_sel($i)
		set siz [expr $symsiz($map)*1.5]
		set sizo2 [expr $symsizo2($map)*1.5]
	} else {
		set color $symbol_color_unsel($i)
		set siz $symsiz($map)
		set sizo2 $symsizo2($map)
	}
	if {$proj($map) == "merc"} {
		set x [expr ($evlon($evid)-$xl($map))*$xscale($map)]
		set y [expr ([mercy $evlat($evid)]-$yt($map))*$yscale($map)]
	}
	if {$proj($map) == "edp"} {
		set xy [edpxy $latc($map) $lonc($map) $evlat($evid) $evlon($evid)]
		set x [expr ([lindex $xy 0]-$xl($map))*$xscale($map)]
		set y [expr ([lindex $xy 1]-$yt($map))*$yscale($map)]
	}
	if {$selected == 1} {
		set first [expr ($x-0.5*$wmapframe($map))/$width($map)]
		set_mxview_map $map moveto $first
		set first [expr ($y-0.5*$hmapframe($map))/$height($map)]
		set_myview_map $map moveto $first
	}
	if {$evext($evid) != "0" || $evrev($evid) != "0"} {
		set x1 [expr $x-$sizo2]
		set x2 [expr $x+$sizo2]
		set y1 [expr $y-$sizo2]
		set y2 [expr $y+$sizo2]
		if {$evext($evid) != "0"} {
			if {$selected == 1} {
				$mapcanvas($map) create rectangle $x1 $y1 $x2 $y2 -fill #000000 -tags [list events $evtag]
			}
			$mapcanvas($map) create oval $x1 $y1 $x2 $y2 -fill $color -outline #000000 -tags [list events $evtag]
		} else {
			if {$selected == 1} {
				set nsiz [expr $sizo2*$SQRT2]
				set xx1 [expr $x-$nsiz]
				set xx2 [expr $x+$nsiz]
				set yy1 [expr $y-$nsiz]
				set yy2 [expr $y+$nsiz]
				$mapcanvas($map) create oval $xx1 $yy1 $xx2 $yy2 -fill #000000 -tags [list events $evtag]
			}
			$mapcanvas($map) create rectangle $x1 $y1 $x2 $y2 -fill $color -outline #000000 -tags [list events $evtag]
		}
		$mapcanvas($map) bind $evtag <ButtonPress-1> "setselection $evtag"
		$mapcanvas($map) bind $evtag <ButtonPress-3> "eventmenu $mapcanvasmenu($map) $evid %x %y"
		return
	}
	plot_star $mapcanvas($map) $x $y $siz $color [list events $evtag] $selected
	$mapcanvas($map) bind $evtag <ButtonPress-1> "setselection $evtag"
	$mapcanvas($map) bind $evtag <ButtonPress-3> "eventmenu $mapcanvasmenu($map) $evid %x %y"
}

proc plot_origin {map ortag selected} {
	global proj
	global xscale
	global yscale
	global xl
	global yt
	global latc
	global lonc
	global symsiz
	global symsizo2
	global mapcanvas
	global mapcanvasoriginmenu
	global symbol_color_number
	global symbol_color_time
	global symbol_color_unsel
	global symbol_color_sel
	global SQRT2
        global width
        global height
	global wmapframe
	global hmapframe
	global seprefor
	global seolat
	global seolon

	set orid [string trimleft [string range $ortag 2 end] 0]
	if {$orid == $seprefor} {
		set color \#ff0000
		set siz [expr $symsiz($map)*3.0]
		set sizo2 [expr $symsizo2($map)*3.0]
	} else {
		set color \#ffff00
		set siz [expr $symsiz($map)*2.0]
		set sizo2 [expr $symsizo2($map)*2.0]
	}
	if {$proj($map) == "merc"} {
		set x [expr ($seolon($orid)-$xl($map))*$xscale($map)]
		set y [expr ([mercy $seolat($orid)]-$yt($map))*$yscale($map)]
	}
	if {$proj($map) == "edp"} {
		set xy [edpxy $latc($map) $lonc($map) $seolat($orid) $seolon($orid)]
		set x [expr ([lindex $xy 0]-$xl($map))*$xscale($map)]
		set y [expr ([lindex $xy 1]-$yt($map))*$yscale($map)]
	}
	if {$orid == $seprefor} {
		set first [expr ($x-0.5*$wmapframe($map))/$width($map)]
		set_mxview_map $map moveto $first
		set first [expr ($y-0.5*$hmapframe($map))/$height($map)]
		set_myview_map $map moveto $first
	}
	set x1 [expr $x-$sizo2]
	set x2 [expr $x+$sizo2]
	set y1 [expr $y-$sizo2]
	set y2 [expr $y+$sizo2]
	if {$selected} {
		$mapcanvas($map) create rectangle $x1 $y1 $x2 $y2 -fill #000000 -tags [list origins $ortag]
	}
	$mapcanvas($map) create oval $x1 $y1 $x2 $y2 -fill $color -outline #000000 -tags [list origins $ortag]
	$mapcanvas($map) bind $ortag <ButtonPress-1> "setorselection $ortag"
	$mapcanvas($map) bind $ortag <ButtonPress-3> "originmenu $mapcanvasoriginmenu($map) $orid %x %y"
}

proc plot_station {map lat lon orid timedef sta} {
	global proj
	global xscale
	global yscale
	global xl
	global yt
	global latc
	global lonc
	global symsiz
	global symsizo2
	global mapcanvas
	global mapcanvasoriginmenu
	global symbol_color_number
	global symbol_color_time
	global symbol_color_unsel
	global symbol_color_sel
	global SQRT2
        global width
        global height
	global wmapframe
	global hmapframe
	global seprefor
	#JCSa
	global mapcanvasstamenu
	#JCSb

	set statag [format "sta%s" $sta]
	switch $timedef {
	1	{
			set color \#ff0000
		}
	0	{
			set color \#ffff00
		}
	d	{
			set color \#0000ff
		}
	default	{
			set color \#00ff00
		}
	}
	set siz [expr $symsiz($map)*1.5]
	set sizo2 [expr $symsizo2($map)*1.5]
	if {$proj($map) == "merc"} {
		set x [expr ($lon-$xl($map))*$xscale($map)]
		set y [expr ([mercy $lat]-$yt($map))*$yscale($map)]
	}
	if {$proj($map) == "edp"} {
		set xy [edpxy $latc($map) $lonc($map) $lat $lon]
		set x [expr ([lindex $xy 0]-$xl($map))*$xscale($map)]
		set y [expr ([lindex $xy 1]-$yt($map))*$yscale($map)]
	}
#puts "lat: $lat lon: $lon x: $x y: $y sta: $sta"
#	plot_triangle $mapcanvas($map) $x $y $siz $color [list stations] 0
	plot_triangle $mapcanvas($map) $x $y $siz $color [list stations $statag] 0
	$mapcanvas($map) bind $statag <ButtonPress-2> "stamenu $mapcanvasstamenu($map) $sta $orid %x %y"
}
#JCSa this will plot stations based on some value
proc plot_station_val {map lat lon orid val sta} {
	global proj
	global xscale
	global yscale
	global xl
	global yt
	global latc
	global lonc
	global symsiz
	global symsizo2
	global mapcanvas
	global mapcanvasoriginmenu
	global station_color_number
	global station_color_val
	global station_color
	global SQRT2
        global width
        global height
	global wmapframe
	global hmapframe
	global seprefor
	#JCSa
	global mapcanvasstamenu

        for {set i 0} {$i < $station_color_number} {incr i} {
	  if {$val <= $station_color_val($i)} break
       	}
  	set color $station_color($i)
	set statag [format "sta%s" $sta]
	#JCSb
	#switch $timedef {
	#1	{
#			set color \#ff0000
#		}
#	0	{
#			set color \#ffff00
#		}
#	d	{
#			set color \#0000ff
#		}
#	default	{
#			set color \#00ff00
#		}
#	}
	set siz [expr $symsiz($map)*1.5]
	set sizo2 [expr $symsizo2($map)*1.5]
	if {$proj($map) == "merc"} {
		set x [expr ($lon-$xl($map))*$xscale($map)]
		set y [expr ([mercy $lat]-$yt($map))*$yscale($map)]
	}
	if {$proj($map) == "edp"} {
		set xy [edpxy $latc($map) $lonc($map) $lat $lon]
		set x [expr ([lindex $xy 0]-$xl($map))*$xscale($map)]
		set y [expr ([lindex $xy 1]-$yt($map))*$yscale($map)]
	}
#puts "lat: $lat lon: $lon x: $x y: $y sta: $sta"
#	plot_triangle $mapcanvas($map) $x $y $siz $color [list stations] 0
	plot_triangle $mapcanvas($map) $x $y $siz $color [list stations $statag] 0
	$mapcanvas($map) bind $statag <ButtonPress-2> "stamenu_val $mapcanvasstamenu($map) $sta $val %x %y"
}
#JCSb

proc eventmenu {menu evid x y} {
	seteventmenu $menu $evid
	mytk_popup $menu $x $y
}

proc originmenu {menu orid x y} {
	setoriginmenu $menu $orid
	mytk_popup $menu $x $y
}

#JCSa
proc stamenu {menu sta orid x y} {
	setstamenu $menu $sta $orid
	mytk_popup $menu $x $y
}
proc stamenu_val {menu sta val x y} {
	setstamenu_val $menu $sta $val
	mytk_popup $menu $x $y
}
#JCSb

proc setwfevent {} {
	global evidselection
	global oridselection
	global evorid
        global scsift
        global channels
        global twin
        global filter
        global sestachans

	set ts [expr -0.2*$twin]
	# set orid $evorid($evidselection)
	set orid $oridselection
	if {[info exists sestachans]} {
		set first 1
		foreach stachan $sestachans($orid) {
			set stachan [lindex [split $stachan "/"] 0]
			if {$first == 1} {
				set first 0
			} else {
				append pickchans ","
			}
			append pickchans $stachan
		}
	}
	set ok [xhosts_ok "Cannot show waveforms while xhost is enabled"]
	if { $ok } {
	    if {[info exists pickchans]} {
		    send2dbpick "sw 0 filter $filter se $orid tse pal 1 rec sc $scsift cw 1 $channels tw $twin ts $ts dw sc $pickchans cw 1 $channels sp P,S sw 1"
	    } else {
		    send2dbpick "sw 0 filter $filter se $orid tse pal 1 rec sc $scsift cw 1 $channels tw $twin ts $ts dw cw 1 $channels sp P,S sw 1"
	    }
	}
}

proc send2dbpick {msg} {
	global dbpickon
	global dbname
	global message
	global dbpickgeom
	global dbpicktypeingeom
	global stat
	global doupdate
	global dbpappname
	global myappname
	global iddbp

	if {$dbpickon != 1} {set message $msg}
	if {$message == "quit"} {
		set doupdate 0
		set stat "Quitting dbpick..."
		update
	}
	if {$dbpickon < 2} {
		if {$dbpickon < 1} {
			if {$message == "quit"} {set doupdate 1; return}
			set doupdate 0
			set stat "Starting dbpick..."
			update
			set dbpappname "dbevents_dbp[pid]" 
			exec xterm -iconic -sb -geom $dbpicktypeingeom -e dbpick -nostarttalk -noedit -appname $dbpappname -geometry $dbpickgeom $dbname &
			set dbpickon 1
		}
		if {[catch "send -async $dbpappname send $myappname respondhi"]} {
		}
		set cmd [format "\"send -async %s send $myappname respondhi\"" $dbpappname]
		set iddbp [after 5000 "send2dbpick $cmd"]
		return
	}
	if {[catch {send -async $dbpappname $message} errmsg]} {
		if {$message != "quit"} {
			set stat "Cannot talk to dbpick - disabling waveform display"
		}
		set dbpickon 0
		set doupdate 1
		return
	}
	if {$message == "quit"} {set doupdate 1; set dbpickon 0}
}

proc respondhi {} {
	global doupdate
	global dbpickon
	global message
	global iddbp

	after cancel $iddbp
	set dbpickon 2
	set doupdate 1
	send2dbpick $message
}

proc showbutton {} {
	global show_origins
	global evidselection
	global mapnames
	global mapcanvas
	global citem

	if {$show_origins == -1} {
		set show_origins 0
		.mbar.showevents configure -relief sunken -foreground red
		.mbar.showstations configure -relief raised -foreground black
		return
	}

	if {$show_origins == 0} {
		set show_origins 1
		.mbar.showevents configure -relief raised -foreground black
		.mbar.showstations configure -relief sunken -foreground red
		foreach map $mapnames {
			$mapcanvas($map) lower events base
			$mapcanvas($map) raise stations $citem($map)
			$mapcanvas($map) raise origins $citem($map)
		}
	} else {
		set show_origins 0
		.mbar.showevents configure -relief sunken -foreground red
		.mbar.showstations configure -relief raised -foreground black
		foreach map $mapnames {
			$mapcanvas($map) lower origins base
			$mapcanvas($map) lower stations base
			$mapcanvas($map) raise events $citem($map)
		}
	}
}

proc wf_dialog {} {
    variable ::tk::Priv
    global tcl_platform
    global wfdialogwin
    global scsift
    global channels
    global twin
    global filter

    set w $wfdialogwin
    set title "dbevents: wfopts"
    set text "edit waveform options"
    catch {destroy $wfdialogwin}
    toplevel $wfdialogwin -class Dialog
    wm title $wfdialogwin $title
    wm iconname $wfdialogwin Dialog
    wm protocol $wfdialogwin WM_DELETE_WINDOW { }
    wm overrideredirect $wfdialogwin 1

    wm transient $wfdialogwin [winfo toplevel [winfo parent $wfdialogwin]]
    if {$tcl_platform(platform) == "macintosh"} {
	unsupported1 style $w dBoxProc
    }

    frame $wfdialogwin.bot
    frame $wfdialogwin.mid
    frame $wfdialogwin.top
    if {$tcl_platform(platform) == "unix"} {
	$wfdialogwin.bot configure -relief raised -bd 1
	$wfdialogwin.mid configure -relief raised -bd 1
	$wfdialogwin.top configure -relief raised -bd 1
    }
    pack $wfdialogwin.bot -side bottom -fill both
    pack $wfdialogwin.top -side top -fill both
    pack $wfdialogwin.mid -side top -fill both -expand 1

    option add *Dialog.msg.wrapLength 3i widgetDefault
    label $wfdialogwin.msg -justify left -text $text
    if {$tcl_platform(platform) == "macintosh"} {
	$wfdialogwin.msg configure -font system
    } else {
	$wfdialogwin.msg configure -font {Times 18}
    }
    pack $wfdialogwin.msg -in $wfdialogwin.top -side right -expand 1 -fill both -padx 3m -pady 0

    button $w.done -text "DONE" -command "set Priv(button) 0" -font {Times 10}
    $w.done configure -default normal
    grid $w.done -in $w.bot -column 0 -row 0 -sticky ew -pady 0
    bind $w.bot <Return> "
    	$w.done configure -state active -relief sunken
    	update idletasks
    	after 100
    	set Priv(button) 0
    "
    bind $w <Destroy> {set Priv(button) -1}

    frame $w.mid.f1 -relief flat -bd 0
    label $w.mid.f1.label -text "Station-Channel Sifter: " -relief flat
    entry $w.mid.f1.entry -width 20 -textvariable scsift -relief flat
    pack $w.mid.f1.label $w.mid.f1.entry -side left

    frame $w.mid.f2 -relief flat -bd 0
    label $w.mid.f2.label -text "Channels To Display: " -relief flat
    entry $w.mid.f2.entry -width 5 -textvariable channels -relief flat
    pack $w.mid.f2.label $w.mid.f2.entry -side left

    frame $w.mid.f3 -relief flat -bd 0
    label $w.mid.f3.label -text "Time Window: " -relief flat
    entry $w.mid.f3.entry -width 8 -textvariable twin -relief flat
    pack $w.mid.f3.label $w.mid.f3.entry -side left

    frame $w.mid.f4 -relief flat -bd 0
    label $w.mid.f4.label -text "Filter Index: " -relief flat
    entry $w.mid.f4.entry -width 8 -textvariable filter -relief flat
    pack $w.mid.f4.label $w.mid.f4.entry -side left

    pack $w.mid.f1 $w.mid.f2 $w.mid.f3 $w.mid.f4 -side top -fill x

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab != ""} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    focus $w.done

    tkwait variable Priv(button)
    catch {focus $oldFocus}
    catch {
	bind $w <Destroy> {}
	destroy $w
    }
    if {$oldGrab != ""} {
	if {$grabStatus == "global"} {
	    grab -global $oldGrab
	} else {
	    grab $oldGrab
	}
    }
    return $Priv(button)
}

#package require Datascope
#package require Pixmap

# set check [exec xhost]
# set lcheck [split $check \n]
# if {[llength $lcheck] > 1} {
# 	puts stderr "dbevents: Cannot run with xhost authorization enabled."
# 	exit
# }

#NIKO
#prepare for new arguments
# moved higher up in script to use Pf earlier JCS
#set limit_age 0
#set maxage -1

#set Pf dbevents

#for {set i 0} {$i<[expr $argc - 1]} {incr i} {
#	switch -- [lindex $argv $i] {
#		-maxage {
#			set maxage [expr 86400 * [lindex $argv [expr $i + 1]]]
#			set limit_age 1
#			incr i
#		}
#		-pf {
#			set Pf [lindex $argv [expr $i + 1]]
#			incr i
#		}
#	}
#}
# end move JCS

#NIKO

#NIKO
#set dbname [lindex $argv 0]
set dbname [lindex $argv end]

#NIKO
# set before
#set Pf dbevents

set maxmapwidth		[pfget $Pf maxmapwidth]
set maxmapheight	[pfget $Pf maxmapheight]
set dbpickgeom		[pfget $Pf dbpickgeom]
set dbpicktypeingeom	[pfget $Pf dbpicktypeingeom]
set scsift		[pfget $Pf scsift]
set twin		[pfget $Pf twin]
set channels		[pfget $Pf channels]
set filter		[pfget $Pf filter]
set startdbpick		[pfget $Pf startdbpick]
set automap		[pfget $Pf automap]
set textwidth		[pfget $Pf textwidth]
set legendon		[pfget $Pf legendon]
set geometry		[pfget $Pf geometry]
set waittorestart	[expr 1000*[pfget $Pf waittorestart]]

if { [catch {set magnitude_preference_order [pfgetlist @$Pf#magnitude_preference_order]}] } {
	set magnitude_preference_order "mb ms ml"
}

pfgetarr maps @$Pf#maps

set update_intervals [pfgetlist @$Pf#update_intervals]

set update_interval [pfget $Pf update_interval]
set update_ms [expr [convert_time $update_interval]*1000]

set symbol_colors [pfgetlist @$Pf#symbol_colors]
#JCSa
set mag_thresh [pfget $Pf mag_thresh]
set mysrn [pfget $Pf mysrn]
set usemysrn [pfget $Pf usemysrn_mag]
set station_colors [pfgetlist @$Pf#station_colors]
set monitor_stas [pfget $Pf monitor_stations]
set select_stations [pfgetlist @$Pf#select_stations]
#JCSb

set i 0
foreach s $symbol_colors {
	if {$s == ""} continue
	set sl [split $s " \t"]
	if {$i == 0} {
		set symbol_color_text($i) [format "- Within [lindex $sl 0]"]
	} else {
		set symbol_color_text($i) [format "- %s to %s" $lasttime [lindex $sl 0]]
	}
	set symbol_color_time($i) [convert_time [lindex $sl 0]]
	set symbol_color_unsel($i) [lindex $sl 1]
	set symbol_color_sel($i) [lindex $sl 2]
	if {[info exists lasttime]} {set lasttime2 $lasttime}
	set lasttime [lindex $sl 0]
	incr i
}
set symbol_color_number $i
set symbol_color_time([expr $i-1]) 99999999
set symbol_color_text([expr $i-1]) [format "- Older than %s" $lasttime2]

#JCSa get station colors 
set i 0
foreach s $station_colors {
	if {$s == ""} continue
	set sl [split $s " \t"]
	if {$i == 0} {
		set station_color_text($i) [format "- Lessthan [lindex $sl 0]"]
	} else {
		set station_color_text($i) [format "- %s to %s" $lastval [lindex $sl 0]]
	}
	set station_color_val($i) [lindex $sl 0]
	set station_color_unsel($i) [lindex $sl 1]
	set station_color_sel($i) [lindex $sl 2]
	if {[info exists lastval]} {set lastval2 $lastval}
	set lastval [lindex $sl 0]
	incr i
}
set station_color_number $i
set station_color_val([expr $i-1]) 99999999
set station_color_text([expr $i-1]) [format "- Greater than %s" $lastval2]
#JCSb

pfgetarr external_catalog_authors @$Pf#external_catalog_authors
pfgetarr local_catalog_authors @$Pf#local_catalog_authors

set mapnames [array names maps]

proc waveforms_menu {m} {
    $m delete 0 end 
    set xhosts [get_xhosts]
    if { [llength $xhosts] == 0 } {
	$m add command -label Show -command setwfevent
	$m add command -label Dismiss -command {send2dbpick quit}
	$m add command -label Restart -command restartdbpick
	$m add command -label "Options..." -command wf_dialog
    } else { 
	$m add command -label "disabled because of xhost" -state disabled
	$m add command -label "   $xhosts" -state disabled
	$m add command -label "Disable xhosts" -command "turn_off_xhosts $xhosts"
    }
}

proc setupmain {} {
	global maps
	global mapnames
	global mapindex
	global mapon
	global imap
	global width
	global height
	global mapframe
	global maplegend
	global mapcanvas
	global mapcanvasmenu
	#JCS
	global mapcanvasstamenu
	#JCS
	global mapcanvasoriginmenu
	global mapxscroll
	global mapyscroll
	global proj
	global symsiz
	global symsizo2
	global latminbb
	global latmaxbb
	global lonminbb
	global lonmaxbb
	global latc
	global lonc
	global yb
	global yt
	global xl
	global xr
	global xscale
	global yscale
	global latmin
	global latmax
	global lonmin
	global lonmax
	global xdelmin
	global xdelmax
	global ydelmin
	global ydelmax
	global wmapframe
	global hmapframe
	global citem
	global fname
	global icon_pixmap
	global dbname
	global db
	global update_interval
	global update_intervals
	global maxmapwidth
	global maxmapheight
	global legendwidth
	global legendheight
	global automap
	global legendon
	global stat
	global textwidth
	global env
	global icon_pixmap
	global geometry
	global list_font
	global priority
	global tc24
#JCSa
	global alerton
	global repeaton
	global qddson
	global mag_thresh
	global mysrn
	global usemysrn
	global monitor_stas
#JCSb
	frame .mbar -relief raised -bd 2 -background gray
	menubutton .mbar.file -text File -underline 0 \
		-menu .mbar.file.menu -background gray \
		-activebackground darkgray
	menubutton .mbar.maps -text Maps -underline 0 \
		-menu .mbar.maps.menu -background gray \
		-activebackground darkgray
	menubutton .mbar.waveforms -text Waveforms -underline 0 \
		-menu .mbar.waveforms.menu -background gray \
		-activebackground darkgray
#JCSa
	menubutton .mbar.alert -text Alert -underline 0 \
		-menu .mbar.alert.menu -background gray \
		-activebackground darkgray
	menubutton .mbar.contrl -text Control -underline 0 \
		-menu .mbar.contrl.menu -background gray \
		-activebackground darkgray

	button .mbar.dbe -text Database -command {exec dbe $dbname &}
#JCSb
	button .mbar.nextmap -text NextMap -command incrementmap
	button .mbar.showevents -text ShowEvents -command showbutton
	button .mbar.showstations -text ShowStations -command showbutton
	button .mbar.nextevent -text NextEvent -command incrementselection
	button .mbar.prevevent -text PrevEvent -command decrementselection
	button .mbar.quit -text QUIT -command quit_proc
	pack .mbar.file .mbar.maps .mbar.waveforms .mbar.alert .mbar.contrl -side left
	pack .mbar.quit .mbar.nextmap .mbar.prevevent .mbar.nextevent .mbar.showstations .mbar.showevents .mbar.dbe -side right

	showbutton
	
	menu .mbar.file.menu
	.mbar.file.menu add command -label "Quit" -underline 0 \
		-accelerator "Ctrl+c" -command quit_proc \
		-background #80ff80 -activebackground red
	
	menu .mbar.maps.menu \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00
	.mbar.maps.menu add cascade -label "Map Display" \
		-menu .mbar.maps.menu.mapd 
	.mbar.maps.menu add cascade -label "Map Update" \
		-menu .mbar.maps.menu.mapu 
	.mbar.maps.menu add checkbutton -label "Auto Map" \
		-variable automap
	.mbar.maps.menu add checkbutton -label "Show Legend" \
		-variable legendon -command setlegend 
	
	menu .mbar.maps.menu.mapd \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00
	menu .mbar.maps.menu.mapu \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00
	.mbar.maps.menu.mapu add command -label "now" \
		-command {set_update now} \
		-background #a0a0ff

	foreach update $update_intervals {
		.mbar.maps.menu.mapu add radiobutton -label "$update" \
			-variable update_interval -value $update \
			-command {set_update $update_interval} \
			-background #a0a0ff 
	}
	
	menu .mbar.waveforms.menu \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00 \
		-disabledforeground #ff0000 \
		-postcommand "waveforms_menu .mbar.waveforms.menu"
#JCSa
	menu .mbar.alert.menu \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00 
	.mbar.alert.menu add checkbutton -label "Audible" \
		-variable alerton
	.mbar.alert.menu add checkbutton -label "Repeated Alert" \
		-variable repeaton
	.mbar.alert.menu add cascade -label "Mag Threshold" \
		-menu .mbar.alert.menu.magthresh
	menu .mbar.alert.menu.magthresh \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00
	foreach magt {0 1 2 3 4 5 6 7} {
		.mbar.alert.menu.magthresh add radiobutton -label "$magt" \
			-variable mag_thresh -value $magt \
			-background #a0a0ff 
	}
	.mbar.alert.menu add checkbutton -label "Use SRN" \
		-variable usemysrn

	menu .mbar.contrl.menu \
		-background #a0a0ff -activebackground #000060 \
		-activeforeground #ffff00 
	.mbar.contrl.menu add checkbutton -label "Allow QDDS Control" \
		-variable qddson
	.mbar.contrl.menu add command -label "Remove Event QDDS" \
		-command {puts stderr "Nothing happens yet"}
	.mbar.contrl.menu add command -label "Show Query" \
		-command {puts stderr "Nothing happens yet"}
	.mbar.contrl.menu add command -label "Send Query to QDDS" \
		-command {puts stderr "Nothing happens yet"}
	.mbar.contrl.menu add checkbutton -label "Auto Detail Map" \
		-variable autodetailmap
#JCSb

	if {"$tc24" == "yes"} {frame .f -visual "truecolor 24"} else {frame .f}
	frame .f.map -bd 0

	set first 1
	set i 0
	foreach map $mapnames {
		incr i
		set width($map) [pixmap $imap($map) width]
		set height($map) [pixmap $imap($map) height]
		set mapframe($map) .f.map.f$map
		set maplegend($map) .f.map.f$map.l$map
		set mapcanvas($map) .f.map.f$map.c$map
		set mapcanvasmenu($map) .f.map.f$map.c$map.m$map
		#JCS
		set mapcanvasstamenu($map) .f.map.f$map.c$map.ms$map
		#JCS
		set mapcanvasoriginmenu($map) .f.map.f$map.c$map.mo$map
		set mapxscroll($map) .f.map.f$map.sx$map
		set mapyscroll($map) .f.map.sy$map
		set proj($map) [pfget $maps($map) proj]
		set symsiz($map) [expr int([pfget $maps($map) symsize])]
		set symsizo2($map) [expr int(0.5*$symsiz($map)+0.5)]
		set latminbb($map) [pfget $maps($map) latminbb]
		set latmaxbb($map) [pfget $maps($map) latmaxbb]
		set lonminbb($map) [pfget $maps($map) lonminbb]
		set lonmaxbb($map) [pfget $maps($map) lonmaxbb]
		set priority($map) [pfget $maps($map) priority]
		if {$proj($map) == "merc"} {
			set latmin($map) [pfget $maps($map) latmin]
			set latmax($map) [pfget $maps($map) latmax]
			set lonmin($map) [pfget $maps($map) lonmin]
			set lonmax($map) [pfget $maps($map) lonmax]
			set yb($map) [mercy $latmin($map)]
			set yt($map) [mercy $latmax($map)]
			set xl($map) $lonmin($map)
			set xr($map) $lonmax($map)
			set xscale($map) [expr $width($map)/($xr($map)-$xl($map))]
			set yscale($map) [expr $height($map)/($yb($map)-$yt($map))]
		}
		if {$proj($map) == "edp"} {
			set latc($map) [pfget $maps($map) latc]
			set lonc($map) [pfget $maps($map) lonc]
			set xdelmin($map) [pfget $maps($map) xdelmin]
			set xdelmax($map) [pfget $maps($map) xdelmax]
			set ydelmin($map) [pfget $maps($map) ydelmin]
			set ydelmax($map) [pfget $maps($map) ydelmax]
			set yb($map) $ydelmin($map)
			set yt($map) $ydelmax($map)
			set xl($map) $xdelmin($map)
			set xr($map) $xdelmax($map)
			set xscale($map) [expr $width($map)/($xr($map)-$xl($map))]
			set yscale($map) [expr $height($map)/($yb($map)-$yt($map))]
		}
		if {$first == 1} {
			set mapon $map
		}
		set first 0
		set wmapframe($map) $width($mapon)
		if {$wmapframe($map) > $maxmapwidth} {set wmapframe($map) $maxmapwidth}
		set hmapframe($map) $height($mapon)
		if {$hmapframe($map) > $maxmapheight} {set hmapframe($map) $maxmapheight}
		frame $mapframe($map) -relief flat -bd 0
		canvas $mapcanvas($map) -width $wmapframe($map) -height $hmapframe($map) -confine 1 \
				-scrollregion [list 0 0 [expr $width($map)-1] [expr $height($map)-1]] \
				-xscrollincrement 10 -yscrollincrement 10
    		make_eventmenu $mapcanvasmenu($map)
    		make_originmenu $mapcanvasoriginmenu($map)
		#JCS
		make_stamenu $mapcanvasstamenu($map)
		#JCS	
		scrollbar $mapxscroll($map) -orient horiz -command set_mxview -width 10
		scrollbar $mapyscroll($map) -command set_myview  -width 10
		if {$width($map) < $maxmapwidth} {
			set w $maxmapwidth
		} else {
			set w $width($map)
		}
		if {$height($map) < $maxmapheight} {
			set h $maxmapheight
		} else {
			set h $height($map)
		}
		$mapcanvas($map) create rectangle 0 0 $w $h -fill white -outline white -tags [list base]
		# set citem($map) [$mapcanvas($map) create image 0 0 -image $imap($map) -anchor nw]
		set citem($map) [$mapcanvas($map) create pixmap $imap($map) 1 1 -anchor nw]
		bind $mapcanvas($map) <Configure> "+set_mxscroll %w; set_myscroll %h;"
		.mbar.maps.menu.mapd add radiobutton -label "$map" \
			-variable mapon -value $map \
			-command {set_map $mapon} \
			-background #a0a0ff 
		bind $mapcanvas($map) <Shift-ButtonPress-1> decrementselection
		bind $mapcanvas($map) <Shift-ButtonPress-3> incrementselection
		# bind $mapcanvas($map) <Shift-ButtonPress-2> incrementmap
		canvas $maplegend($map) -width $legendwidth -height $legendheight
		set y [expr $hmapframe($map)-5]
		$mapcanvas($map) create text 5 $y -anchor sw \
				-text "Copyright 2000 Boulder Real Time Technologies, Inc." \
				-font {Times 10 bold} -tags [list copyright]
	}

	drawlegend

	pack $mapcanvas($mapon) $mapxscroll($mapon) -fill x
	pack $mapyscroll($mapon) $mapframe($mapon) -side left -fill y

	grid .f.map -row 0 -column 0 -padx 0 -pady 0 -sticky nsew -rowspan 2
	
	setlegend
	
	focus .mbar
	
	frame .s -relief raised -bd 2 -background #ffe0e0 -relief flat
	label .s.slabel -text "Status:" -background #ffe0e0 -relief flat
	entry .s.sentry -width 80 -textvariable stat -background #ffe0e0 -relief flat
	pack .s.slabel .s.sentry -side left
	
	frame .f.events
	text .f.events.txt \
	    -width $textwidth \
	    -yscrollcommand ".f.events.scrolly set" \
	    -wrap none \
	    -font $list_font \
	    -cursor hand2

	scrollbar .f.events.scrolly -command ".f.events.txt yview" -width 10
	grid .f.events.scrolly -row 0 -column 0 -padx 0 -pady 0 -sticky nsw
	grid .f.events.txt -row 0 -column 1 -padx 0 -pady 0 -sticky nsew
	grid rowconfigure .f.events 0 -weight 1

	grid .f.events -row 0 -column 1 -padx 0 -pady 0 -sticky nsew

	frame .f.showevent -height [expr int($maxmapheight*0.25)]
	text .f.showevent.txt \
	    -width $textwidth \
	    -yscrollcommand ".f.showevent.scrolly set" \
	    -wrap none \
	    -font $list_font \
	    -cursor hand2

	scrollbar .f.showevent.scrolly -command ".f.showevent.txt yview" -width 10
	grid .f.showevent.scrolly -row 0 -column 0 -padx 0 -pady 0 -sticky nsw
	grid .f.showevent.txt -row 0 -column 1 -padx 0 -pady 0 -sticky nsew
	grid rowconfigure .f.showevent 0 -weight 1

	grid .f.showevent -row 1 -column 1 -padx 0 -pady 0 -sticky nsew

	grid rowconfigure .f 0 -weight 1
	grid rowconfigure .f 1 -weight 0 -minsize [expr int($maxmapheight*0.35)]
	
	pack .mbar -fill x
	pack .f -fill both
	pack .s -fill x
	
	set db [dbopen $dbname r]
	
	plot_events $db 1 1
	if {$monitor_stas == 1} {
	#	start_orb_read
	}
	
	update_latency

}

set tc24 [check_tc24]

set icon_pixmap [pixmap create $env(ANTELOPE)/data/icons/dbevents.gif]
if {"$tc24" == "yes"} {toplevel .icon -visual "truecolor 24"} else {toplevel .icon}
canvas .icon.c -width [pixmap $icon_pixmap width] -height [pixmap $icon_pixmap height]
.icon.c create pixmap $icon_pixmap 1 1 -anchor nw
.icon.c create text [expr int([pixmap $icon_pixmap width]*0.5)]  \
		[expr [pixmap $icon_pixmap height] - 1] -anchor s -text dbevents -font [list helvetica 10 bold]
.icon.c create text [expr int([pixmap $icon_pixmap width]*0.5)]  \
		1 -anchor n -text "" -font [list helvetica 10 bold] -tags [list icontime]
pack .icon.c

wm resizable . 0 0
#NIKO
#wm title . [format "dbevents: %s" $dbname]
#mention -maxage paramter in title
set titlestring [format "dbevents: %s" $dbname]
if {$limit_age} {set titlestring [format "%s - %ddays" $titlestring [expr int($maxage / 86400)]]}
wm title . $titlestring
#NIKO

wm geometry . $geometry
wm iconwindow . .icon
#NIKO
#shorten iconname for visibility
#wm iconname . $dbname
wm iconname . [string range $dbname [expr [string last / $dbname] +1 ] end]


if {"$tc24" == "yes"} {frame .f -visual "truecolor 24"} else {frame .f}
message .f.text -text "Reading map gif files..." -width 600
pack .f.text
pack .f

proc readmaps {} {
	global maps
	global mapnames
	global mapindex
	global fname
	global imap
	global env

	set i 0
	foreach map $mapnames {
		set mapindex($map) $i
		incr i
		set fname [pfget $maps($map) file]
		if {[string range $fname 0 7] == "ANTELOPE"} {
			set fname [format "%s%s" $env(ANTELOPE) [string range $fname 8 end]]
		}
		.f.text configure -text [format "Reading map %s from gif file\n%s" $map $fname]
		update
		if {[catch "pixmap create $fname" out]} {
			tk_messageBox -type ok -message "Cannot create map $map...$out"
			exit
		} else {
			set imap($map) $out
		}
	}

	destroy .f
	setupmain
}

after 1000 readmaps

